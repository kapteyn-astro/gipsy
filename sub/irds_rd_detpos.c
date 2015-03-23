/*
#>            irds_rd_detpos.dc2

Function:     irds_rd_detpos

Purpose:      To extract (projected) sky positions data from an IRDS

Category:     IR

File:         irds_dpos.c

Author:       Do Kester

Use:          IRDS_RD_DETPOS(  IRDS   ,    Input   character*(*)
                               SNIP   ,    Input   integer
                               SDET   ,    Input   integer
                               TICK   ,    Input   integer
			       COOR   ,    Input   integer
			       PROJ   ,    Input   integer
                               YLON   ,    Output  double( >=NDATA )
                               ZLAT   ,    Output  double( >=NDATA )
                               TWIST  ,    Output  double( >=NDATA )
                               NDATA  ,    In/Out  integer
                               STATUS )    Output  integer
                                       
              IRDS        Name of IRDS to read from.
              SNIP        Sequential snip number to read.
              SDET        Sequential detector number to read.
                          SDET = 0 corresponds to boresight,
                          SDET < 0 corresponds to the center of gravity
                          of band number SDET (see IRCC_BANDNR etc.)
              TICK        Sequential tick of first sample to read.
	      COOR	  coordinate system identification ( cf irco.dc2 )
			  0 is a valid number => no transformations	
              PROJ	  projection system number ( cf irco_prname.dc2 )
			  0 is a valid number => no projection
	      YLON        Array to receive LON coordinates (rad)
              ZLAT        Array to receive LAT coordinates (rad)
              TWIST       Array to receive TWIST angles (rad. ccw w.r.t. +ZLAT)
              NDATA       I - max number of samples to return
                          O - number of samples actually returned.
              STATUS      Error return code:
                           0  - no error.
                          -1  - IRDS does not exist
                          -2  - IRDS is not a legal irds
                          -3  - SNIP not in IRDS
                          -4  - SDET not in IRDS
                          -5  - TICK not in IRDS
                          -6  - gds read error
                          -7  - no coordinate info in header
                          -8  - bad SDET
			  -9  - not enough memory for boresight data
			  -10 - only one point bphf tick found
Description:
	The BPHF data are read for the satcal ticks in the scan. 
	The boresight positions are transformed to the proper detector
	positions, still in sunreferenced coordinates.
	Transformation and projection take place ( when requested 
	with coor and proj ) and the resulting positions are
	interpolated to the sample times. No conscious shortcuts or 
	approximations have been taken in these calculations. 

	The routine has been optimized for an inner loop over the 
	detectors and an outer loop over the snips.

	It is assumed that the ecliptic coordinate system has been precessed
	to 1983.5.
	The returned angles will be in radians

Bugs: 	Funny things will happen when the snip crosses an edge of the
	projection type.

Updates:      	30 Nov 1990: DK
		30 May 1991  DK, improved interpolation
		03 Jun 1991  DK, partial satcals allowed.
		07 Aug 1991  DK, free memory.
#<

@subroutine irds_rd_detpos( character, integer, integer, integer, integer,
@  integer, double precision, double precision, double precision, integer, 
@  integer )
*/

#include	"stdio.h"		/* temporary */
#include	"stdlib.h"
#include	"gipsyc.h"
#include	"math.h"
#include	"gdsd_rint.h"
#include	"gdsd_grint.h"
#include	"irco.h"		/* all coor.transf. routines */
#include	"irds_rd_bphf.h"
#include	"ircc_mask.h"
#include	"ircc_times.h"
#include	"error.h"
#include	"anyout.h"
#include 	"assert.h"

/* allocate n doubles to pointer x, either new memory or overwriting old */
#define		rmalloc( x, n ) \
			( double * )(x) ? realloc( (x),(n) ) : malloc( n )
#define		dsize( n )		( (n) * sizeof( double ) )
#define		allocmem( x, n )	( x = rmalloc( x, dsize( n ) ) )
#define		PRECESS		0.008167277
#define		D2R		( 3.1415926535897932384 / 180.0 )
#define		M2R		( D2R / 60.0 )
#define		SUNREF		5	/* standard sunref coordsys */
#define		MAXSAMPLES	32	/* max. nr. of samples: LRS */
#define		SERIOUS		3

/* predefine some internal functions */
static void sico( double*, double*, fint );
static fint det_xt( fint, double*, double*, double*, double*, double*, double*,
			  double*, double*, fint );
static void initpol( fint, fint, float*, float*, float* );
static void interpolate( double*, fint, double*, fint, float*, float*, float* );

void irds_rd_detpos_c(
	fchar	irds,
	fint	*snip,
	fint	*sdet,
	fint	*tick,
	fint	*coor,
	fint	*proj,
	double	*ylon,
	double	*zlat,
	double	*twist,
	fint	*ndata,
	fint 	*status )

{

/* static memory to preserve info in indentical SNIP calls in permanent pointers*/
    static double	*ppcl = 0, *ppcb = 0, *ppct = 0 ;
    static double   	*ppt = 0, *ppw = 0, *ppx = 0, *ppy = 0, *ppz = 0  ;
    static double	*ppsl = 0, *ppsb = 0, *ppst = 0, sunlong, sunrate ;
    static fint		prev_snip = 0, prev_tick = 0, prev_ndata = 0, rate, nb ;

    double		*pcl, *pcb, *pct, *pt, *pw, *px, *py, *pz ;
    double		*psl, *psb, *pst ;

    fint	nb3, nsats, level, err, axis, n, det, sunco = SUNREF,rdtick;
    fint	scancal, snipcal ;
    float	pf1[MAXSAMPLES], pf2[MAXSAMPLES], pf3[MAXSAMPLES] ;

/* first call: determine samplerate */
    if ( !prev_snip ) {
	level = 0; err = 0;
	gdsd_rint_c( irds, tofchar( "NAXIS1" ), &level, &rate, &err );
    }

/* first call for this snip: store bphf information */
    if ( *snip != prev_snip || *tick != prev_tick || *ndata != prev_ndata) {
	nsats = ( *ndata + rate - 1 ) / rate ;	/* partial satcals too */
	nb = nsats + 2 ;
	nb3 = 3 * nb ;
/* allocate memory to several double scratch variables */
	if ( !( allocmem( ppcl, nb ) && allocmem( ppsl, nb ) &&
	    allocmem( ppcb, nb )     && allocmem( ppsb, nb ) &&
	    allocmem( ppct, nb )     && allocmem( ppst, nb ) &&
	    allocmem( ppx, nb3 )     && allocmem( ppt, nb3 ) &&
	    allocmem( ppy, nb )      && allocmem( ppz, nb )  &&
	    allocmem( ppw, nb ) ) ) {
		err = SERIOUS ;
		error_c( &err, tofchar( "irds_rd_detpos: not enough memory" ) );
		*status = -9;
		return;
	}

/* assign workpointers to their equivalent counterparts */
	pcl = ppcl; psl = ppsl; pcb = ppcb; psb = ppsb;
	pct = ppct; pst = ppst; 
	px = ppx; py = ppy; pt = ppt; pw = ppw; pz = ppz;

/* read bphf; and extrapolate one tick upward and downward if necessary */
	if ( *tick == 1 ) {		/* shift one for extrapolation */
		pcl++ ; pcb++ ; pct++ ; nb-- ;
                rdtick = *tick ;
	} else {
                rdtick = *tick - 1 ;
        }
	irds_rd_bphf_c( irds, snip, &rdtick, pcl, psl, pcb, psb, pct,
		&sunlong, &sunrate, &nb, status );
	if ( nb <= 1 ) *status = -10 ;	/* not enough points */
	if ( *status < 0 ) return ;	/* error while reading bphf */
	if ( nb <= nsats ) {		/* extrapolate linearly upward */
		pcl[nb] = 2 * pcl[nb-1] - pcl[nb-2] ;
		pcb[nb] = 2 * pcb[nb-1] - pcb[nb-2] ;
		pct[nb] = 2 * pct[nb-1] - pct[nb-2] ;
		nb++ ;
	}
	if ( *tick == 1 ) {		/* extrapolate linearly downward */
		pcl-- ; pcb-- ; pct-- ; 
		*pcl = *(pcl + 1) * 2 - *(pcl + 2) ;
		*pcb = *(pcb + 1) * 2 - *(pcb + 2) ;
		*pct = *(pct + 1) * 2 - *(pct + 2) ;
		nb++ ;
	}

/* replace the angles and errors by cosine and sine of the angles resp. */
	sico( pcl, psl, nb ) ;
	sico( pcb, psb, nb ) ;
	sico( pct, pst, nb ) ;
	if ( *status == 1 ) {
/* intended positions read: find sunlong from satcal */
		axis = 4 ; n = 1 ;
		err = 0;
		gdsd_grint_c( irds, tofchar( "SCANCAL" ), &axis, snip, &n, 
			&scancal, &err ) ;
	        err = 0;
		gdsd_grint_c( irds, tofchar( "SNIPCAL" ), &axis, snip, &n, 
			&snipcal, &err ) ;
		scancal += snipcal + nsats / 2 ;
		sunlong = irco_sunlong_c( &scancal ) ;
	}
	else 	sunlong += 0.5 * nsats * sunrate + PRECESS ;
	irco_sunref_c( &sunlong );
	prev_snip = *snip;
	prev_tick = *tick;
	prev_ndata = *ndata;
    }
/* assign workpointers to their equivalent counterparts */
    pcl = ppcl; psl = ppsl; pcb = ppcb; psb = ppsb;
    pct = ppct; pst = ppst; 
    px = ppx; py = ppy; pt = ppt; pw = ppw; pz = ppz;

    if ( *sdet > 0 ) {
	axis = 3; n = 1; err = 0; 
	gdsd_grint_c( irds, tofchar( "DETNO" ), &axis, sdet, &n, &det, &err );
    } else	det = *sdet ;		/* boresight or band positions */

/* determine rect. coord. of detector DET for position PX and twist PT */
    if ( det_xt( det, pcl, psl, pcb, psb, pct, pst, px, pt, nb ) ) {
        *status = -8 ;
        return ;
    }

/* transform to the required coordinate system; if none do not transform */
    if ( *coor ) {
	irco_transform_c( px, &sunco, px, coor, &nb );
	irco_transform_c( pt, &sunco, pt, coor, &nb );
    }

/* project according to projection type; if none is given return
the sky positions. */
    if ( *proj ) 	irco_protwist_c( proj, px, pt, py, pz, pw, &nb );
    else 		irco_rectotwist_c( px, pt, py, pz, pw, &nb );

/* determine interpolating weights */
    initpol( rate, det, pf1, pf2, pf3 ) ;

/* interpolate YLON, ZLAT and TWIST to sample times */
    nsats = nb - 2 ;
    interpolate( py, nsats, ylon,  rate, pf1, pf2, pf3 ) ;
    interpolate( pz, nsats, zlat,  rate, pf1, pf2, pf3 ) ;
    interpolate( pw, nsats, twist, rate, pf1, pf2, pf3 ) ;

    *ndata = rate * nsats ;
    return ;

}


/* 
Lagrange three point interpolation from satcals to samples 
(Handbook of Mathematical Functions, Abramowitz & Stegun, p879)
The interpolation points are fixed wrt the satcals; 
pf1, pf2, pf3 give the interpolating weights for one tick. 
In INITPOL the interpolating weights for one tick are determined.
*/

static void initpol( 
	fint	rate,		/* IN:	sample rate */
	fint	det,		/* IN:	detector number */
	float	*pf1,		/* OUT: interpolating weight for prev. tick */
	float	*pf2,		/* OUT:	interpolating weight for this tick */
	float	*pf3 )		/* OUT: interpolating weight for next tick */

{
    float	dp, p, p2 ;

    dp = 2.0 / rate ;			/* increment in time between samples */
    p = 2 * ircc_times_c( &det ) - 1.0 ; 	/* sampletime - bphftime */
    while ( rate-- ) {			/* store pf's over one tick */
	p2 = p * p ;
	*pf1++ = 0.5 * ( p2 - p ) ;
	*pf2++ = 1.0 - p2 ;
	*pf3++ = 0.5 * ( p2 + p ) ;
	p += dp ;
    }
    return ;
}

/*
INTERPOLATE interpolates values PY which are known at time tick+0.5 (i.e. bphf)
to sample times into YLON. YLON runs from 1 to ns*rate, so that the ticks in
YLON are running from 1 to ns. In PY then, the ticks should run from 
0 to ns+1, i.e. one extra both at the beginning and at the end.
*/
static void interpolate( 
	double	*py,		/* IN:  values per tick, first tick is 0 */
	fint	ns,		/* IN:  nr of ticks in ylon */
	double	*ylon,		/* OUT: values per sample */
	fint	rate,		/* IN:  nr of samples per tick */
	float	*pf1,		/* IN:  interpolating weight for tick-1 */
	float	*pf2,		/* IN:	interpolating weight for tick   */
	float	*pf3 )		/* IN:  interpolating weight for tick+1 */

{
    double	y1, y2, y3, y12, y23 ;
    float	*f1, *f2, *f3 ;
    int		nr ;

    y2 = *py++ ; 
    y3 = *py++ ; 
    while ( ns-- ) {
	f1 = pf1 ;		/* set fn to begin of pfn */
	f2 = pf2 ;
	f3 = pf3 ;
	y1 = y2 ; 		/* shift y-values properly */
	y2 = y3 ; 
	y3 = *py++ ; 
	y12 = ( y1 + y2 ) / 2 ;
	y23 = ( y2 + y3 ) / 2 ;
	for ( nr = 0; nr < rate; nr++ ) {	/* interpolate over this tick */
		*ylon++ =  (*f1++) * y12 + (*f2++) * y2 + (*f3++) * y23 ;
	}
    } 

    return ;

}

/* 
replace first double by cosine of it 
and put the sine in the second double 
*/

static void sico( 
	double *pcl, 		/* IN:  angle in rad; OUT: cosine of it */
	double *psl, 		/* OUT: sine of it */
	fint ns )		/* IN:  number of angles */

{
    while ( ns-- ) {
	*psl = sin( *pcl ) ;
	*pcl = cos( *pcl ) ;
	pcl++ ; psl++ ;
    }
    return ;
}


/* 
Determine detector positions for the present detector.
IRCC_MASK yields tangent-plane-projected positions of the detector centers
dy and dz. Depending on the value of the twist angle they yield offsets (u,v)
in the telescope coordinate system:
	u = dy * cos( twist ) + dz * sin( twist )
	v =-dy * sin( twist ) + dz * cos( twist )
Unproject (u,v) into an xyz-vector in the telescope coordinate system: 
	x = r
	y = u * r
	z = v * r
	r = 1 / sqrt( 1 + dy**2 + dz**2 )
This xyz-vector must be transformed to a xyz-vector in the sun-referenced
system. The telescope system is a plate coordinate system: the values
for ZOLD, THETA and ZNEW are resp. 180 - l, b, -180 ( see irco_plate ).
l : SR longitude and b : SR latitude. 
The transfomation matrix boils down to ( see irco_transmat in irco.shl )
	cosl*cosb  -sinl  -cosl*sinb
	sinl*cosb   cosl  -sinl*sinb
	     sinb    0          cosb
The transformed xyz-vector becomes
	x' = x * cosl * cosb - y * sinl - z * cosl * sinb
	y' = x * sinl * cosb + y * cosl - z * sinl * cosb
	z' = x *        sinb +            z *        cosb
The corresponding twist-angle vector (tws) becomes
	t = ( x' * z' * cost + y' * sint ) / cosb'
	w = ( y' * z' * cost - x' * sint ) / cosb'
	s = cost * cosb'
with cosb' the cosine of the latitude in the SR system
	cosb' = sqrt( 1 - z'**2 )
*/

static fint det_xt( 
	fint	det,		/* IN:  detector number */
	double	*pcl,		/* IN:  cosine of longitude */
	double	*psl,		/* IN:  sine of longitude */
	double	*pcb,		/* IN:  cosine of latitude */
	double	*psb,		/* IN:  sine of latitude */
	double	*pct,		/* IN:  cosine of twist angle */
	double	*pst,		/* IN:  sine of twist angle */
	double	*px,		/* OUT: rect.coord. of position */
	double	*pt,		/* OUT: rect.coord. of twist angle */
	fint	ns )		/* IN:  number of angles */

{
    float	dy, dz, insize, crosize ;
    double	r, u, v, xx, yy, zz, cb ;

    if ( ircc_mask_c( &det, &dy, &dz, &insize, &crosize ) < 0 ) return( -1 ) ;

    dy *= M2R; dz *= M2R;
    r = 1.0 / sqrt( 1 + dy * dy + dz * dz ) ;

    while ( ns-- ) {
	u =  dy * *pct + dz * *pst ;
	v = -dy * *pst + dz * *pct ;
	xx = ( *pcl * *pcb - u * *psl - v * *pcl * *psb ) * r ;
	yy = ( *psl * *pcb + u * *pcl - v * *psl * *psb ) * r ;
	zz = (        *psb            + v        * *pcb ) * r ;
	cb = sqrt( 1.0 - zz * zz ) ;			/* cosb' */
	*pt++ = ( xx * zz * *pct + yy * *pst ) / cb ;	/* twv[i][0] */
	*pt++ = ( yy * zz * *pct - xx * *pst ) / cb ;	/* twv[i][1] */
	*pt++ = *pct * cb ;				/* twv[i][2] */
	*px++ = xx ;					/* xyz[i][0] */
	*px++ = yy ;					/* xyz[i][1] */
	*px++ = zz ;					/* xyz[i][2] */
	pcl++ ; psl++ ; pcb++ ; psb++ ; pct++ ; pst++ ;
    }
    return( 0 ) ;
}

