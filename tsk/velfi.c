/* velfi.c

#>            velfi.dc1

Program:      VELFI

Purpose:      Calculates model velocity field from input parameters
              given by the user.

Category:     MODELS, ROTATION-CURVES, VELOCITY-FIELDS

File:         velfi.c

Author:       K.G. Begeman

Keywords:

   INSET=     Set (and subset) of original velocity field needed to
              create the output set.

   OUTSET=    Set (and subset) of the model velocity field.

   RADII=     Radii (in arcsec) at which the rotation curve is 
              measured. Up to 256 radii can be specified.

   PA=        Position angle of major axis (in degrees) for the radii
              entered above.

   INCL=      Inclination angle (in degrees) for the radii entered above.

   VROT=      Rotational velocities (in km/s) for the radii entered
              above.

   VRAD=      Expansion velocities (in km/s) for the radii entered
              above.

   VSYS=      Systemic velocity (in km/s) for the radii entered above.

   POS=       The rotation centre.

Notes:        1) The program calculates the model velocity field from
              the formula :
              V(X,Y)=VSYS+VROT(R)*COS(T)*SIN(INCL)+VRAD(R)*SIN(T)*SIN(INCL)
              where: V      is    resulting velocity field
                     VSYS         systemic velocity
                     VROT         rotational velocity (rotation curve)
                     T            azimuthal angle (in galaxy plane)
                     INCL         inclination
                     VRAD         expansion velocity
                     R            radius
                     X, Y         grid coordinates
              2) For the keywords PA=, INCL=, VROT=, VRAD= and VSYS=
              you can enter less values than the number of rings you have
              specified; in that case these quantities are taken to be
              constant for the remaining number of rings at a value
              equal to the last one given.
              The keywords RADII=, PA=, INCL=, VROT=, VRAD= and VSYS=
              are repeated until you answer with carriage return. This
              makes it possible to enter recall files. Note that the
              macro is only saved when you enter a recall file with last
              line empty. You can also enter more than one value to
              these keywords.

Updates:      May 21, 1982: KGB, original document created.
              Feb 26, 1983: KGB, program inplemented.
              May  7, 1986: KGB, migrated to VAX-VMS.
              Apr 12, 1988: KGB, new document.
              Dec 11, 1989: KGB, more than one systemic velocity.
              Jul 27, 1992: KGB, Converted to C and Unix.

#<

*/


/*
 * Includes:
 */

#include	"float.h"		/* <float.h> */
#include	"math.h"		/* <math.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"gipsyc.h"		/* GIPSY definitions */
#include	"cmain.h"		/* C programme */
#include	"anyout.h"		/* anyout_c */
#include	"axtype.h"		/* axtype_c */
#include	"cancel.h"		/* cancel_c */
#include	"error.h"		/* error_c */
#include	"factor.h"		/* factor_c */
#include	"finis.h"		/* finis_c */
#include	"gdsasn.h"		/* gdsasn_c */
#include	"gdsc_fill.h"		/* gdsc_fill_c */
#include	"gdsc_grid.h"		/* gdsc_grid_c */
#include	"gdsc_name.h"		/* gdsc_name_c */
#include	"gdsc_range.h"		/* gdsc_range_c */
#include	"gdsd_rchar.h"		/* gdsd_rchar_c */
#include	"gdsd_rdble.h"		/* gdsd_rdble_c */
#include	"gdsinp.h"		/* gdsinp_c */
#include	"gdsi_write.h"		/* gdsi_write_c */
#include	"gdsout.h"		/* gdsout_c */
#include	"gdspos.h"		/* gdspos_c */
#include	"init.h"		/* init_c */
#include	"minmax3.h"		/* minmax3_c */
#include	"nelc.h"		/* nelc_c */
#include	"setfblank.h"		/* setfblank_c */
#include	"sortra.h"		/* sortra_c */
#include	"status.h"		/* status_c */
#include	"userreal.h"		/* userreal_c */
#include	"wminmax.h"		/* wminmax_c */


/*
 * Defines:
 */

#define	CLASS		1		/* class of programme */
#define	CLASSDIM	2		/* dimension of subsets */
#define	MAXAXES		10		/* max. number of axes */
#define	MAXDATA		4096		/* max. size of data buffer */
#define	MAXFITSLEN	18		/* max. length fits char. */
#define	MAXMESLEN	80		/* max. length of message */
#define	MAXRADII	256		/* max. number of radii */
#define	MAXSETNAMLEN	80		/* max. length of set name */
#define	VERSION		"1.0"		/* programme version number */

#define	cosd( x )	cos( x * 0.017453293 )
#define	sind( x )	sin( x * 0.017453293 )
#define	fmake( f, c )	{f.a=c;f.l=sizeof(c);}


/*
 * Keywords and messsages:
 */

#define	KEY_INCL	tofchar("INCL=")
#define	KEY_INSET	tofchar("INSET=")
#define	KEY_OUTSET	tofchar("OUTSET=")
#define	KEY_PA		tofchar("PA=")
#define	KEY_POS		tofchar("POS=")
#define	KEY_RADII	tofchar("RADII=")
#define	KEY_VRAD	tofchar("VRAD=")
#define	KEY_VROT	tofchar("VROT=")
#define	KEY_VSYS	tofchar("VSYS=") 
#define	MES_INCL	tofchar("Enter Inclination Angles of rotation curve (degrees)")
#define	MES_INSET	tofchar("Set (and subset) observed velocity field")
#define	MES_OUTSET	tofchar("Set (and subset) model velocity field")
#define	MES_PA		tofchar("Enter Position Angles of rotation curve (degrees)")
#define	MES_POS		tofchar("Enter Dynamica Centre") 
#define	MES_RADII	tofchar("Enter radii of rotation curve (arcsec)")
#define	MES_VRAD	tofchar("Enter Expansion velocities (km/s)")
#define	MES_VROT	tofchar("Enter Rotation velocities (km/s)")
#define	MES_VSYS	tofchar("Enter Systemic velocities (km/s)")


/*
 * Variables for input set:
 */

static	char	isetb[MAXSETNAMLEN];		/* buffer for input set name */
static	fchar	iset = { isetb, MAXSETNAMLEN };	/* input set name */
static	fint	iaxperm[MAXAXES];		/* axes permutation */
static	fint	iaxsize[MAXAXES];		/* axes sizes */
static	fint	icwlo;				/* lower cw */
static	fint	icwup;				/* upper cw */
static	fint	isubset;			/* input subset */

/*
 * Variables for output set:
 */

static	char	osetb[MAXSETNAMLEN];		/* buffer for output set name */
static	fchar	oset = { osetb, MAXSETNAMLEN };	/* output set name */
static	fint	oaxperm[MAXAXES];		/* axes permutation */
static	fint	oaxsize[MAXAXES];		/* axes sizes */
static	fint	ocwlo;				/* lower cw */
static	fint	ocwup;				/* upper cw */
static	fint	osubset;			/* output subset */

/*
 * Other variables:
 */

static	char	mes[MAXMESLEN];			/* message buffer */
static	double	cdelt[CLASSDIM];		/* grid separations */
static	double	cpos[CLASSDIM];			/* rotation centre */
static	double	mapphi;				/* map rotation */
static	fint	blo[CLASSDIM];			/* lower frame corner */
static	fint	bup[CLASSDIM];			/* upper frame corner */
static	fint	gerror = 0;			/* GDS error return */
static	fint	nrad;				/* number of radii */
static	float	blank;				/* BLANK value */
static	float	data[MAXDATA];			/* data buffer */
static	float	rads[MAXRADII];			/* radii */
static	float	incl[MAXRADII];			/* inclination angles */
static	float	posa[MAXRADII];			/* position angles */
static	float	vrad[MAXRADII];			/* expansion velocities */
static	float	vrot[MAXRADII];			/* circular velocities */
static	float	vsys[MAXRADII];			/* systemic velocities */
static	float	xpos, ypos;			/* centre in grids */

/*
 * Function radius.
 */
 
static	float	radius( fint indx, float x, float y )
{
   float	xr, yr;

   xr = ( -x * sind( posa[indx] ) + y * cosd( posa[indx] ) );
   yr = ( -x * cosd( posa[indx] ) - y * sind( posa[indx] ) ) / cosd( incl[indx] );
   return( rads[indx] - sqrt( xr * xr + yr * yr ) );
}


/*
 * The main programme:
 */

MAIN_PROGRAM_ENTRY
{
   init_c( );					/* contact HERMES */
   setfblank_c( &blank );			/* get system BLANK */
   IDENTIFICATION( "VELFI", VERSION );		/* hailing frequencies */
   /*
    * Get input set and subset of observed velocity field.
    */
   {
      fint	class = CLASS;			/* class of gdsinp call */
      fint	classdim = CLASSDIM;		/* required subset dimension */
      fint	error_level = 4;		/* level of error */
      fint	input_level = 0;		/* level of input */
      fint	i;				/* loop counter */
      fint	items = 1;			/* number of items */
      fint	maxaxes = MAXAXES;		/* ax limit */
      fint	output_level = 11;		/* level of output */
      fint	tcount = 0;			/* axis id count */

      (void) gdsinp_c( iset, &isubset, &items, &input_level, KEY_INSET,
         MES_INSET, &output_level, iaxperm, iaxsize, &maxaxes, &class,
         &classdim );
      /*
       * check size of first subset axis.
       */
      if (iaxsize[0] > MAXDATA) {
         sprintf( mes, "Size of first subset axis > %d!", MAXDATA );
         error_c( &error_level, tofchar( mes ) );
      }
      /*
       * Get grid coordinates of set frame.
       */
      gdsc_range_c( iset, &isubset, &icwlo, &icwup, &gerror );
      for ( i = 0; i < CLASSDIM; i++ ) {
         blo[i] = gdsc_grid_c( iset, &iaxperm[i], &icwlo, &gerror );
         bup[i] = gdsc_grid_c( iset, &iaxperm[i], &icwup, &gerror );
      }
      /*
       * Get grid separations.
       */
      for ( i = 0; i < CLASSDIM; i++ ) {
         char	bctype[MAXFITSLEN+1];
         char	bcunit1[MAXFITSLEN+1];
         char	bcunit2[MAXFITSLEN+1];
         char	bdunit[MAXFITSLEN+1];
         char	descriptor[20];
         fchar	ctype, cunit, cunit1, cunit2, dunit;
         fint	axnum = iaxperm[i];
         fint	axtyp;
         fint	level = 0;
         fint	skysys, prosys, velsys;

         fmake( ctype, bctype );
         fmake( cunit1, bcunit1 );
         fmake( cunit2, bcunit2 );
         fmake( dunit, bdunit );
         gdsc_name_c( ctype, iset, &axnum, &gerror );
         axtyp = axtype_c( ctype, cunit1, dunit, &skysys, &prosys, &velsys );
         switch( axtyp ) {
            case 2: {				/* Latitude (get rotation) */
               sprintf( descriptor, "CROTA%d", axnum );
               gerror = 0;
               gdsd_rdble_c( iset, tofchar( descriptor ), &level, &mapphi,
                  &gerror );
               if (gerror) { mapphi = 0.0; }
            }
            case 1: {				/* get cdelt in arcsec */
               sprintf( descriptor, "CUNIT%d", axnum );
               gerror = 0;
               gdsd_rchar_c( iset, tofchar( descriptor ), &level, cunit2, &gerror );
               if (gerror) {
                  cunit = cunit1;
               } else {
                  cunit = cunit2;
               }
               sprintf( descriptor, "CDELT%d", axnum );
               gerror = 0;
               gdsd_rdble_c( iset, tofchar( descriptor ), &level, &cdelt[i],
                  &gerror );
               if (gerror) {
                  error_c( &error_level,
                     tofchar( "No gridseparation found!" ) );
               } else {
                  double	f;

                  (void) factor_c( cunit, tofchar( "ARCSEC" ), &f );
                  cdelt[i] *= f;
               }
               cdelt[i] = fabs( cdelt[i] );	/* make positive */
               break;
            }
            default: {
               axtyp = 0;
               break;
            }
         }
         tcount += axtyp;
      }
      if (tcount != 3) {
         error_c( &error_level,
            tofchar( "No Longitude & Latitude axes pair!" ) );
      }
   }
   /*
    * Get output set.
    */
   {
      fint	class = CLASS;			/* class of programme */
      fint	input_level = 0;		/* no default */
      fint	items = 1;			/* number of subsets */
      fint	maxaxes = MAXAXES;		/* max. number of axes */
      fint	output_level = 11;		/* level of output */

      gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
      (void) gdsout_c( oset, &osubset, &items, &input_level, KEY_OUTSET,
         MES_OUTSET, &output_level, oaxperm, oaxsize, &maxaxes );
      ocwlo = gdsc_fill_c( oset, &osubset, blo );
      ocwup = gdsc_fill_c( oset, &osubset, bup );
   }
   /*
    * Get parameters of velocity field.
    */
   {
      char	objectb[MAXFITSLEN];		/* buffer */
      fchar	object;				/* name of object */
      fint	count;			 	/* counter */
      fint	input_level;			/* level of input */
      fint	items;				/* max. number of items */
      fint	n;				/* loop counter */
      fint	ninp;				/* items entered by user */
      fint	output_level = 0;		/* level of output */

      /*
       * loop to get radii.
       */
      input_level = 0;				/* no default at first prompt */
      nrad = 0;					/* reset */
      do {
         items = MAXRADII - nrad;		/* number of items */
         if (nrad) cancel_c( KEY_RADII );	/* cancel keyword */ 
         count = userreal_c( &rads[nrad], &items, &input_level, KEY_RADII,
            MES_RADII );
         input_level = 1;			/* default allowed */
         nrad += count;				/* new number of rads */
      } while (count && nrad < MAXRADII);	/* loop control */
      /*
       * Check radii.
       */
      {
         fint	is = 0;

         sortra_c( rads, &nrad );		/* sort radii */
         while ( is < nrad && rads[is] < 0.0 ) is++;
         if ( is ) {
            fint	error_level = 4;	/* FATAL error */

            error_c( &error_level, tofchar( "Negative Radii not allowed!" ) );
         }
      }
      /*
       * Get position angles.
       */
      input_level = 0;				/* first time no default */
      ninp = 0;					/* reset */
      do {
         items = nrad - ninp;			/* max. number of items */
         if (ninp) cancel_c( KEY_PA );		/* cancel keyword */
         count = userreal_c( &posa[ninp], &items, &input_level, KEY_PA,
            MES_PA );
         input_level = 1;			/* default allowed */
         ninp += count;				/* number of entries */
      } while (count && ninp < nrad);		/* loop control */
      for ( n = ninp; n < nrad; n++ ) {		/* default values */
         posa[n] = posa[n-1];
      }
      /*
       * Get inclination angles.
       */
      input_level = 0;				/* first time no default */
      ninp = 0;					/* reset */
      do {
         items = nrad - ninp;			/* max. number of items */

         if (ninp) cancel_c( KEY_INCL );	/* cancel keyword */
         count = userreal_c( &incl[ninp], &items, &input_level, KEY_INCL,
            MES_INCL );
         input_level = 1;			/* default allowed */
         ninp += count;				/* number of entries */
      } while (count && ninp < nrad);		/* loop control */
      for ( n = ninp; n < nrad; n++ ) {		/* default values */
         incl[n] = incl[n-1];
      }
      /*
       * Get circular velocities.
       */
      input_level = 0;				/* first time no default */
      ninp = 0;					/* reset */
      do {
         items = nrad - ninp;			/* max. number of items */
         if (ninp) cancel_c( KEY_VROT );	/* cancel keyword */
         count = userreal_c( &vrot[ninp], &items, &input_level, KEY_VROT,
            MES_VROT );
         input_level = 1;			/* default allowed */
         ninp += count;				/* number of entries */
      } while (count && ninp < nrad);		/* loop control */
      for ( n = ninp; n < nrad; n++ ) {		/* default values */
         vrot[n] = vrot[n-1];
      }
      /*
       * Get expansion velocities.
       */
      input_level = 0;				/* first time no default */
      ninp = 0;					/* reset */
      do {
         items = nrad - ninp;			/* max. number of items */
         if (ninp) cancel_c( KEY_VRAD );	/* cancel keyword */
         count = userreal_c( &vrad[ninp], &items, &input_level, KEY_VRAD,
            MES_VRAD );
         input_level = 1;			/* default allowed */
         ninp += count;				/* number of entries */
      } while (count && ninp < nrad);		/* loop control */
      for ( n = ninp; n < nrad; n++ ) {		/* default values */
         vrad[n] = vrad[n-1];
      }
      /*
       * Get systemic velocities.
       */
      input_level = 0;				/* first time no default */
      ninp = 0;					/* reset */
      do {
         items = nrad - ninp;			/* max. number of items */
         if (ninp) cancel_c( KEY_VSYS );	/* cancel keyword */
         count = userreal_c( &vsys[ninp], &items, &input_level, KEY_VSYS,
            MES_VSYS );
         input_level = 1;			/* default allowed */
         ninp += count;				/* number of entries */
      } while (count && ninp < nrad);		/* loop control */
      for ( n = ninp; n < nrad; n++ ) {		/* default values */
         vsys[n] = vsys[n-1];
      }
      input_level = 0;				/* no default */
      items = 1;				/* max. number of items */
      (void) gdspos_c( cpos, &items, &input_level, KEY_POS, MES_POS,
         iset, &isubset );
      xpos = cpos[0];
      ypos = cpos[1];
      fmake( object, objectb );			/* assign fchar */
      gdsd_rchar_c( iset, tofchar( "OBJECT" ), &isubset, object, &gerror );
      if (gerror < 0) {
         gerror = 0;
         object = tofchar( "UNKNOWN" );
      }
      sprintf( mes, "Model velocity field from rotation curve for %.*s",
         (int) nelc_c( object ), object.a );
      anyout_c( &output_level, tofchar( mes ) );
      sprintf( mes, "Centre at %8.2f %8.2f", cpos[0], cpos[1] );
      anyout_c( &output_level, tofchar( mes ) );
      sprintf( mes, "  RADIUS      PA    INCL        VROT        VRAD        VSYS" );
      anyout_c( &output_level, tofchar( mes ) );
      for ( n = 0; n < nrad; n++ ) {
         sprintf( mes, "%8.1f%8.1f%8.1f%12.1f%12.1f%12.1f", (double) rads[n],
            (double) posa[n], (double) incl[n], (double) vrot[n],
            (double) vrad[n], (double) vsys[n] );
         posa[n] += mapphi;			/* add map rotation */
         anyout_c( &output_level, tofchar( mes ) );
      }
   }
   /*
    * Here we calculate the model velocity field.
    */
   {
      fint	count = 0;			/* counter for minmax3 */
      fint	l, lsize;			/* l counters */
      fint	m, mstep, mleft, mtotl, msize;	/* m counters */
      fint	nblanks;			/* number of blanks */
      fint	nwrite;				/* number of pixels written */
      fint	tid = 0;			/* transfer identifier */
      float	datamax, datamin;		/* running min. and max. */

      status_c( tofchar( "Calculating model velocity field" ) );
      lsize = bup[0] - blo[0] + 1;		/* l size */
      msize = bup[1] - blo[1] + 1;		/* m size */
      mstep = MAXDATA / lsize;			/* m step */
      mtotl = 0;				/* m total */
      mleft = msize;				/* m left */
      do {
         fint	j;
         fint	mb, me;				/* m begin and m end */

         if ( mstep > mleft ) mstep = mleft;	/* m left */
         mb = blo[1] + mtotl;			/* m begin */
         me = mb + mstep;			/* m end + 1 */
         for ( j = 0, m = mb; m < me; m++ ) {
            float	y;

            y = cdelt[1] * ( (float) m - ypos );
            for ( l = blo[0]; l <= bup[0]; l++, j++ ) {
               float	rp, x;

               x = cdelt[0] * ( (float) l - xpos );
               rp = sqrt( x * x + y * y );
               if ( rp > rads[nrad-1] ) {
                  data[j] = blank;
               } else {
                  float	d1, d2;

                  d1 = radius( 0, x, y );
                  d2 = radius( nrad - 1, x, y );
                  if ( d1 * d2 > 0.0 ) {
                     data[j] = blank;
                  } else {
                     fint	i;
                     float	incli;
                     float	posai;
                     float	radsi;
                     float	vradi;
                     float	vroti;
                     float	vsysi;
                     float	cost, sint;
                     float	w1, w2;

                     for ( i = 1; i < nrad; i++ ) {
                        d2 = radius( i, x, y );
                        if ( d1 * d2 > 0.0 ) {
                           d1 = d2;
                        } else {
                           break;
                        }
                     }
                     radsi = ( rads[i] * d1 - rads[i-1] * d2 ) / ( d1 - d2 );
                     w1 = ( rads[i-1] - radsi ) / ( rads[i-1] - rads[i] );
                     w2 = 1.0 - w1;
                     posai = w1 * posa[i] + w2 * posa[i-1];
                     incli = w1 * incl[i] + w2 * incl[i-1];
                     vroti = w1 * vrot[i] + w2 * vrot[i-1];
                     vradi = w1 * vrad[i] + w2 * vrad[i-1];
                     vsysi = w1 * vsys[i] + w2 * vsys[i-1];
                     if ( radsi > FLT_EPSILON ) {
                        cost = ( - x * sind( posai ) + y * cosd( posai ) ) / radsi;
                        sint = ( - x * cosd( posai ) - y * sind( posai ) ) / radsi / cosd( incli );
                     } else {
                        cost = 1.0;
                        sint = 0.0;
                     }
                     data[j] = vsysi + ( vroti * cost + vradi * sint ) * sind( incli );
                  }
               }
            }
         }
         gdsi_write_c( oset, &ocwlo, &ocwup, data, &j, &nwrite, &tid );
         minmax3_c( data, &j, &datamin, &datamax, &nblanks, &count );
         mtotl += mstep;			/* m total */
         mleft -= mstep;			/* m left */
      } while ( mleft );			/* loop control */
      /*
       * Update new datamin and datamax.
       */
      {
         fint	change = 1;
         fint	items = 1;
         
         wminmax_c( oset, &osubset, &datamin, &datamax, &nblanks, &items, &change );
      }
   }
   finis_c( );					/* end contact with HERMES */
   return( EXIT_SUCCESS );			/* return with status */
}
