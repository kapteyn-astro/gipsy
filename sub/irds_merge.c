/*
		     Copyright (c) 1991
		   Space Research Groningen
		    All Rights Reserved.
	P.O. Box 800, 9700 AV GRONINGEN. The Netherlands


@ subroutine irds_merge( character, character, integer )

#>            irds_merge.dc2

Function:     IRDS_MERGE

Purpose:      merge IRDSX in IRDS

Category:     IRAS

File:         irds_merge.c

Author:       Do Kester

Use:          call irds_merge( 
                        irdsx,    I     character*(*)
                        irds,     I/O   character*(*)
                        status )  I/O   integer
        irdsx   name of irds to merge with IRDS
        irds    name of irds to merge in
        status   0 : OK. 
		-1 : both nonexistent
                -2 : the two IRDS's dont pertain to the same plate
		-3 : not the same instrument
		-4 : partly or non existing IRDS

description:    
	IRDSX and IRDS should pertain to the same plate i.e. all the
	output parameters obtained with irds_enquire should be the
	same. If not the subroutine returns with status = -2.
	The snips in the two IRDS's which have the same sop/att
	combination are glued into one; blank values are filled 
	in between if the two are not adjacent in time. 
	The final IRDS will have a minimal snip duration, necessary 
	to fit all the snips in IRDS.
	The snips remain in increasing order of sop and att.

externals:      

updates:        23/02/90 DK, document creation
		13/11/90 DK, code
		06/02/91 DK, make global variables and functions `static'
		08/03/91 DK, copy NOISE descriptors too
		12/02/92 FL, also copy BPHF data
		26/02/92 DK, copy SCANTYPE
		03/03/92 DRB,copy OBS
		18/03/92 FL, new enquire_snip and extend, so copying OBS,
		             SCANTYPE and PLATE obsolete.
		07/05/92 DK, flag error for cripple IRDS's
		11/03/93 DK, delete partial irds and use userfio
#< 
*/

#include "string.h"
#include "stdlib.h"
#include "stdio.h"
#include "math.h"
#include "gipsyc.h"
#include "irds_basic.h"
#include "ircc_rate.h"
#include "irds_rd_samples.h"
#include "irds_wr_samples.h"
#include "irds_rd_bphf.h"
#include "irds_wr_bphf.h"
#include "gds_rename.h"
#include "gds_delete.h"
#include "userfio.h"
#include "nelc.h"
#include "gdsd_rchar.h"
#include "gdsd_wchar.h"
#include "gdsd_rreal.h"
#include "gdsd_wreal.h"
#include "gdsd_rint.h"
#include "gdsd_wint.h"
#include "gdsc_word.h"
#include "gdsc_size.h"
#include "gdsc_range.h"
#include "gdsi_read.h"
#include "gdsi_write.h"

static void irds_fetch_snips( fchar, fchar, fint*, fint*, fint*, fint*, 
fint*, fint* );
static void irds_copy_snip( fchar, fchar, fint*, fint*, fint*, fint*, 
fint*, fint*, fint* );
static fint irdsd_noise( fchar, fchar, fchar, fint*, fint*, fint*, fint*,
fint*, fint* );

#define		FOREVER		for ( ; ; )
/* allocate n floats to pointer x, either new or overwriting old */
#define		fmalloc( x, n ) \
			( float * )(x) ? realloc( (x), (n) ) : malloc( n )
#define		fsize( n )	( (n) * sizeof( float ) )
#define		fallocmem( x, n )	( x = fmalloc( x, fsize( n ) ) )
/* allocate n doubles to pointer x, either new or overwriting old */
#define		dmalloc( x, n ) \
			( double * )(x) ? realloc( (x), (n) ) : malloc( n )
#define		dsize( n )	( (n) * sizeof( double ) )
#define		dallocmem( x, n )	( x = dmalloc( x, dsize( n ) ) )

#ifndef	NULL				/* stupid */
#define		NULL		0	/* null pointer */
#endif
#define	 	MAXAXIS		4
#define	 	MAXTAPES	10	/* number of tapes */
#define		LL		2	/* longitude and latitude */
#define		LP		21	/* length of character item in GDS */
#define		LI		20	/* length of character item in GDS */
#define		SNIPS		3	/* axis of the snips */
#define		DETS		2	/* axis of the detectors */
#define		TICKS		1	/* axis of the satcals */
#define		SAMPLES		0	/* axis of the smaples */
#define		OVERMAXSOPATT	999999  /* larger than any sop*1000+att */
#define		ERR_NONEXIST	0	/* irds's are non-existent */
/* error numbers 2 - 8 as in irds_enquire */
#define		ERR_INSTR	8	/* different instruments */
#define		ERR_CENTER	9	/* different centers */
#define		ERR_SIZE	10	/* different sizes */
#define		ERR_COOR	11	/* different coordinate systems*/
#define		ERR_IMPROPER	12	/* improper irds */
/* define error messages */
static char *errmes[] = {	
	"Both IRDS's are non-existent.",
	"it contains not enough (>=4) axes.",
	"it misses INSTRUME.",
	"it misses PRCLON and/or PRCLAT.",
	"it misses SIZELON and/or SIZELAT.",
	"it misses SKYSYS.",
	"it misses OBJECT.",
	"it misses EPOCHE.",    
	"The IRDS's have different instruments (INSTRUME).",
	"The IRDS's have different centers (PRCLON, PRCLAT).",
	"The IRDS's have different sizes (SIZELON, SIZELAT).",
	"The IRDS's have different coor. systems (SKYSYS,EPOCHE).",
	"is not a proper IRDS:", 
};
			

/* define global variables in this file */
static char	fc[21] = "\0" ;
static fint	sop1, obs1, att1, scancal1, scandur1, snipcal1, snipdur1;
static fchar	scantype1 = { fc, 20 } ;
static float	psi1, psirate1, theta1;
static fint	sop2, obs2, att2, scancal2, scandur2, snipcal2, snipdur2;
static fchar	scantype2 = { fc, 20 } ;
static float	psi2, psirate2, theta2;

/************************************************************************/

void irds_merge_c(
	fchar	irds1,
	fchar	irds2,
	fint 	*status )

{
	fint		naxis1, naxis2, axes1[MAXAXIS], axes2[MAXAXIS];
	fint		status1, status2;
	double		center1[LL], center2[LL], size1[LL], size2[LL];
	fchar		object1, object2, instru1, instru2, coor1, coor2;
	char		o1[LP], o2[LP], i1[LP], i2[LP], c1[LP], c2[LP];
	fint		sop, obs, att, scancal, scandur, snipcal, snipdur;
	fchar		scantype = { fc, 20 } ;
	float		psi, psirate, theta, epoch1, epoch2;
	fint		snip1, snip2, done1, done2, todo1, todo2;
	fint		snipdone1, snipdone2 ;
	fint		longsnip, axes[MAXAXIS], level, error;
	char		irm[15] = "MERGESCRATCH  " ;
	fchar		irmerge, observer;
	fint		startsnip, snip, rate;
	fint		noiser = 0;
	fint		no ;

	status1 = status2 = 0;
	object2.a = o2; object2.l = LI;
	instru2.a = i2; instru2.l = LI;
	coor2.a = c2; coor2.l = LI;

	irds_enquire_c( irds2, object2, instru2, &naxis2, axes2, center2, 
			size2, coor2, &epoch2, &status2 );

	object1.a = o1; object1.l = LI;
	instru1.a = i1; instru1.l = LI;
	coor1.a = c1; coor1.l = LI;
	irds_enquire_c( irds1, object1, instru1, &naxis1, axes1, center1, 
			size1, coor1, &epoch1, &status1 );

/* check for existence of at least one IRDS */
	if ( status1 == -1 && status2 == -1 ) {
		errorf( SERIOUS, errmes[ERR_NONEXIST] ) ;
		*status = -1;
		return;			/* both non-existent */
	}

/* check proper structure of IRDS's */
	if ( status1 < -2 || status2 < -2 ) {
		if ( status1 ) {
			errorf( SERIOUS, "%.*s %s", nelc_c( irds1 ),
					irds1.a, errmes[ERR_IMPROPER] ) ;
			errorf( SERIOUS, errmes[ 1 - status1 ] ) ;
		}
		if ( status2 )  {
			errorf( SERIOUS, "%.*s %s", nelc_c( irds1 ),
					irds2.a, errmes[ERR_IMPROPER] ) ;
			errorf( SERIOUS, errmes[ abs( status2 ) ] ) ;
		}
		*status = -2;
		return;			/* invalid IRDS('s) */
	}

/* check whether the IRDS's pertain to the same instrument and area */
	if ( !status2 && !status2 ) {
		if ( strncmp( instru1.a, instru2.a, LI ) ) {
			errorf( SERIOUS, errmes[ERR_INSTR] ) ;
			*status = -3;
			return;		/* different instruments */
		}

/* I do not think the following errors invalidate an irds completely; 
   if the user wants to mix data from different areas that is his/her choice.
   Give a warning message only. */
		if ( center1[1] != center2[1] || center1[0] != center2[0] )
			errorf( WARNING, errmes[ERR_CENTER] ) ;
		if ( size1[1] != size2[1] || size1[0] != size2[0] )
			errorf( WARNING, errmes[ERR_SIZE] ) ;
		if ( strncmp( coor1.a, coor2.a, LI ) || epoch1 != epoch2 )
			errorf( WARNING, errmes[ERR_COOR] ) ;
	}

/***
    use snipdone1/2 in stead of snip1/2 as the variables for the loop over
    the snip-axes. This to ensure that you do not lose the last snip of one
    of the two sets, which otherwise may happen once in a while.

    FL.
          If you do not understand, ask me.
***/
	snip1 = snip2 = snipdone1 = snipdone2 = 0;
	done1 = done2 = 1;
	todo1 = ( !status1 && naxis1 == 4 ) ? axes1[SNIPS] : 0;
	todo2 = ( !status2 && naxis2 == 4 ) ? axes2[SNIPS] : 0;
	longsnip = 0;
	while ( snipdone1 < todo1 || snipdone2 < todo2 ) {
		irds_fetch_snips( irds1, irds2, &snip1, &snip2, 
			&snipcal, &snipdur, &done1, &done2 );
		if (done1) snipdone1++;
		if (done2) snipdone2++;
		if ( snipdur > longsnip ) longsnip = snipdur;
	}

/* create a scratch IRDS of the proper length */
	axes[SAMPLES] = axes2[SAMPLES]; 
	axes[TICKS] = longsnip;
	axes[DETS] = axes2[DETS];
	irmerge.a = irm ; irmerge.l = 14 ;
	*status = 0; error = 0;
	level = 0;
	observer.a = o1; observer.l = LI;
	gdsd_rchar_c( irds2, tofchar( "OBSERVER" ), &level, observer, &error );
/* delete if an irds with this name already exists */
	irds_delete_c( irmerge, status );
	irds_create_c( irmerge, instru2, axes, center2, size2, coor2, &epoch2,
		object2, observer, status );
/* set BUNIT to the value in irds2 */
	error = 0;
	gdsd_rchar_c( irds2, tofchar( "BUNIT" ), &level, observer, &error );
	gdsd_wchar_c( irmerge, tofchar( "BUNIT" ), &level, observer, &error );

/***
    use snipdone1/2 in stead of snip1/2 as the variables for the loop over
    the snip-axes. This to ensure that you do not lose the last snip of one
    of the two sets, which otherwise may happen once in a while.

    FL.
          If you do not understand, ask me.
***/
	snip = snip1 = snip2 = snipdone1 = snipdone2 = 0;
	done1 = done2 = 1;
	rate = ircc_rate_c( instru2 );
	while ( snipdone1 < todo1 || snipdone2 < todo2 ) {
		snip++;
		irds_fetch_snips( irds1, irds2, &snip1, &snip2,
			&snipcal, &snipdur, &done1, &done2 );
		if ( done2 ) {		/* take remaining info from irds2 */
			sop = sop2; obs = obs2; att = att2;
			scancal = scancal2; scandur = scandur2;
			for( no = 0 ; no < scantype.l ; 
				scantype.a[no] = scantype2.a[no++] ) ;
			psi = psi2; psirate = psirate2; theta = theta2;
		} else {		/* take remaining info from irds1 */
			sop = sop1; obs = obs1 ; att = att1;
			scancal = scancal1; scandur = scandur1;
			for( no = 0 ; no < scantype.l ; 
				scantype.a[no] = scantype1.a[no++] ) ;
			psi = psi1; psirate = psirate1; theta = theta1;
		}

		irds_extend_c( irmerge, &sop, &obs, &att, scantype, &scancal, 
			&scandur, &snipcal, &snipdur, &psi, &psirate, &theta, 
			status );
		if ( done1 ) {
			snipdone1++ ;
			startsnip = snipcal1 - snipcal + 1;
			statusf( "%.*s snip %4d; "
				"copied to snip %4d at tick %d ", 
				nelc_c( irds1 ), irds1.a, 
				snip1, snip, startsnip );
			irds_copy_snip( irmerge, irds1, &snip, &snip1, 
				&startsnip, &snipdur1, &rate, &axes[DETS], 
				&error );
		}
		if ( done2 ) {
			snipdone2++ ;
			startsnip = snipcal2 - snipcal + 1;
			statusf( "%.*s snip %4d; "
				"copied to snip %4d at tick %d ", 
				nelc_c( irds2 ), irds2.a, 
				snip2, snip, startsnip );
			irds_copy_snip( irmerge, irds2, &snip, &snip2, 
				&startsnip, &snipdur2, &rate, &axes[DETS], 
				&error );
		}
		if ( ! noiser ) {
			noiser = irdsd_noise( irds1, irds2, irmerge,
				&snip1, &snip2, &snip, &done1, &done2, 
				&axes[DETS] );
		}
	}

	gds_delete_c( irds2, &error ) ;

	if ( gds_rename_c( irmerge, irds2 ) != 0 ) {
	  errorf( SERIOUS, "IRDS exists only partly" ) ;
	  *status = -4 ;
	}

	anyoutf( ANYOUT_DEF, "The new irds `%.*s' contains %d snips", 
		nelc_c( irds2 ), irds2.a, snip );
	anyoutf( ANYOUT_DEF, "and it has a sniplength of %d ticks.", longsnip );
	if ( snip == 0 ) {
	  anyoutf( ANYOUT_DEF, "No data found; the (partial) irds is deleted" );
	  error = 0 ;
	  gds_delete_c( irds2, &error ) ;
	}

	return;
}

/*********************************************************************/
static void irds_fetch_snips(
	fchar	irds1,
	fchar	irds2,
	fint	*snip1,
	fint	*snip2,
	fint	*snipcal,
	fint	*snipdur,
	fint	*done1,
	fint 	*done2 )

{
	fint		stat1, stat2, last, last1, last2;
	static int	sopatt1, sopatt2;

	stat1 = stat2 = 0;
	if ( *done1 ) {		/* fetch a new snip of irds1 */
		(*snip1)++;
		irds_enquire_snip_c( irds1, snip1, &sop1, &obs1, &att1, 
			scantype1, &scancal1,
 			&scandur1, &snipcal1, &snipdur1, &psi1, &psirate1,
			&theta1, &stat1 );
		if ( !stat1 ) sopatt1 = 1000 * sop1 + att1;
		else{
			anyoutf( ANYOUT_TST,
			"irds_fetch_snips: error in enquire_snip; set %.*s %d",
			nelc_c(irds2), irds2.a, stat1 ) ;
			sopatt1 = OVERMAXSOPATT;
		}
	}
	if ( *done2 ) {		/* fetch a new snip of irds2 */
		(*snip2)++;
		irds_enquire_snip_c( irds2, snip2, &sop2, &obs2, &att2, 
			scantype2, &scancal2,
			&scandur2, &snipcal2, &snipdur2, &psi2, &psirate2,
			&theta2, &stat2 );
		if ( !stat2 ) sopatt2 = 1000 * sop2 + att2;
		else{
			anyoutf( ANYOUT_TST,
			"irds_fetch_snips: error in enquire_snip; set %.*s %d",
			nelc_c(irds2), irds2.a, stat2 ) ;
			sopatt2 = OVERMAXSOPATT;
		}
	}
	anyoutf( ANYOUT_TST, "sopoatt1/soppatt2 %d %d", sopatt1, sopatt2 ) ;
	if ( sopatt1 == sopatt2 ){
		*snipcal = ( snipcal1 < snipcal2 ) ? snipcal1 : snipcal2;
		last1 = snipcal1 + snipdur1;
		last2 = snipcal2 + snipdur2;
		last = ( last1 > last2 ) ? last1 : last2 ;
		*snipdur = last - *snipcal;
		*done1 = 1; *done2 = 1;
	}
	else{
		if ( sopatt1 < sopatt2 ) {
			*snipdur = snipdur1;
			*snipcal = snipcal1;
			*done1 = 1; *done2 = 0;
		}
		else{
			*snipdur = snipdur2;
			*snipcal = snipcal2;
			*done1 = 0; *done2 = 1;
		}
	}
	return;
}

/**********************************************************************/

static void irds_copy_snip( 
	fchar	irmerge, 		/* to write in */
	fchar	irds, 			/* to read from */
	fint	*wsnip, 		/* snip number to write */
	fint	*rsnip, 		/* snip number to read */
	fint	*wstart, 		/* satcal to start writing */
	fint	*rlength,		/* ticks to read */
	fint	*rate,			/* samples per tick */
	fint	*maxdet,		/* number of detectors */		
	fint	*error )

{
	static	fint	prevticks = 0 ;
	fint		status = 0 ;
	fint		rtick, sdet, samps;
	static float	*reals = 0;
	static double	*srlon = NULL, *esrlon = NULL;
	static double	*srlat = NULL, *esrlat = NULL;
	static double	*twist = NULL;
	double		lngsun, sunrate ;

	if( *rlength > prevticks ){
		free( srlon  ) ; free( esrlon ) ; free( srlat  ) ;
		free( esrlat ) ; free( twist  ) ;
		if( (srlon = malloc(*rlength*sizeof(double))) == NULL ||
		    (esrlon = malloc(*rlength*sizeof(double))) == NULL ||
		    (srlat = malloc(*rlength*sizeof(double))) == NULL ||
		    (esrlat = malloc(*rlength*sizeof(double))) == NULL ||
		    (twist = malloc(*rlength*sizeof(double))) == NULL
		){
			*error = SERIOUS ;
			errorf( SERIOUS, "irds_merge: not enough memory" ) ;
			return ;
		}
		prevticks = *rlength ;
	}
	samps = *rate * *rlength ;
	if ( ! fallocmem( reals, samps ) ) {
		*error = SERIOUS ;
		errorf( SERIOUS, "irds_merge: not enough memory" ) ;
		return ;
	}

	rtick = 1;
	for ( sdet = 1; sdet <= *maxdet; sdet++ ) {
		irds_rd_samples_c( irds, rsnip, &sdet, &rtick, 
				reals, &samps, error );
		irds_wr_samples_c( irmerge, wsnip, &sdet, wstart,
				reals, &samps, error );
	}
	irds_rd_bphf_c( irds, rsnip, &rtick, 
				srlon, esrlon, srlat, esrlat, twist,
				&lngsun, &sunrate, rlength, &status ) ;
	if( status == 0 ) irds_wr_bphf_c( irmerge, wsnip, wstart,
				srlon, esrlon, srlat, esrlat, twist,
				&lngsun, &sunrate, rlength, &status ) ;
	return;
}

/************************************************************************/

/* combine the noise at snip-detector level in new irmerge */

static fint irdsd_noise( 
	fchar	irds1, 		/* In	irds to merge */
	fchar	irds2, 		/* In	irds to merge */
	fchar	irmerge, 	/* Out	irds to write */
	fint	*snip1, 	/* In	snip number in irds1 */
	fint	*snip2, 	/* In	snip number in irds2 */
	fint	*snip, 		/* In   snip number in irmerge */
	fint	*done1, 	/* In	if true do snip1 */
	fint	*done2,		/* In   if true do snip2 */
	fint	*maxdet )	/* In	number of detectors */

{
	fint		level1, level2, levelm, level, detaxis, snipaxis ;
	fint		err, sdet ;
	float		noise, noise1, noise2 ;
	
	err = level = level1 = level2 = levelm = 0 ;
	detaxis = DETS + 1 ;
	snipaxis = SNIPS + 1 ;
	if ( *done1 ) {
		level1 = gdsc_word_c( irds1, &snipaxis, snip1, &level1, &err ) ;
	}
	if ( *done2 ) {
		level2 = gdsc_word_c( irds2, &snipaxis, snip2, &level2, &err ) ;
	}
	levelm = gdsc_word_c( irmerge, &snipaxis, snip, &levelm, &err ) ;
	for ( sdet = 1; sdet <= *maxdet; sdet++ ) {
		noise = 0 ;
		if ( *done1 ) {
			err = 0 ;
			level = gdsc_word_c( irds1, &detaxis, &sdet,
						&level1, &err ) ;
			gdsd_rreal_c( irds1, tofchar( "NOISE" ), &level, 
						&noise1, &err ) ;
			if ( err != level ) return( err ) ;
			noise += noise1 * noise1 ;
		}
		if ( *done2 ) {
			err = 0 ;
			level = gdsc_word_c( irds2, &detaxis, &sdet,
						&level2, &err ) ;
			gdsd_rreal_c( irds2, tofchar( "NOISE" ), &level, 
						&noise2, &err ) ;
			if ( err != level ) return( err ) ;
			noise += noise2 * noise2 ;
		}
		noise = sqrt( noise / ( *done1 + *done2 ) ) ;
		err = 0 ;
		level = gdsc_word_c( irmerge, &detaxis, &sdet, &levelm, &err ) ;
		gdsd_wreal_c( irmerge, tofchar( "NOISE" ), &level, 
				&noise, &err ) ;
	}
	return( 0 ) ;
}
