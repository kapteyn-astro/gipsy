/* restore.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            restore.dc1

Program:      RESTORE

Purpose:      This program restores the residuals created by CLEAN.

Category:     CLEAN, RADIO, TABLES

File:         restore.c

Author:       K. Begeman

Description:  The restoration is done with a truncated gaussian beam
              {maximum 129*129 grid units}.  It uses existing components
              found by CLEAN.  The residual is overwritten by the
              restored image.

Keywords:

   INSET=     Set and subset(s) of residual(s).
              Usually the set is the same as the output set created by
              CLEAN. Maximum number of subsets is 2048. The subset
              dimension may be one or two.

   BEAM=      Half power beam width of clean beam in arcsecs.
              For a one-dimensional subset, enter the HPBW in arcsec.
              For a two-dimensional subset, first give HPBW along
              majo axis, then HPBW along minor axis. If the header
              items BMMIN and BMMAJ are present, their value will be
              the default.

   BEAMPA=    Position angle of major axis of clean beam in degrees.
              Only asked when subsets are two-dimensional.
              In general only needed for VLA maps. If the header item
              BMPA is present, its value will be the default.

** CLEANBOX=  The size of cleaned area in the residual map(s) [whole subset].
              The restoration takes place in this area.

** FACTOR=    Beam Multiplication Factor [1.0].
              With FACTOR=-1.0 you can undo a previous restoration.

Notes:        1. When you want to save the residual map, use COPY.
              2. There is no limit on the number of components in a file,
                 except the available memory in the machine.
              3. When you enter a zero for the half power beamwidth, the
                 gaussian  beam will be changed into a delta function
                 along that axis. This is used for creating component
                 maps from component tables.

Updates:      Jan 23, 1982: KGB Original program created.
              Mar 24, 1982: KGB Original document created.
              Dec 02, 1982: KGB Larger read buffer.
              Sep 12, 1983: KGB Allowed for zero beam size.
              Aug 22, 1986: KGB Larger buffer for clean bean.
              Mar 24, 1988: AXL Document standardized.
              May 13, 1989: KGB Allow negative grid separations.
              Sep  7, 1991: KGB Migrated to UNIX GIPSY.
              Dec 11, 1991: KGB Cancel replaced by reject.
              Apr  8, 1992: KGB One-dimensional restore implemented.
              Feb  1, 2000: JPT Increased number of subsets.

#<

*/



/*
 * The includes:
 */

#include	"math.h"			/* <math.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */
#include	"gipsyc.h"			/* GIPSY definitions */
#include	"cmain.h"			/* Main programme in C */
#include	"anyout.h"			/* define anyout_c */
#include	"axtype.h"			/* define axtype_c */
#include	"error.h"			/* define error_c */
#include	"factor.h"			/* define factor_c */
#include	"finis.h"			/* define finis_c */
#include	"gdsa_colinq.h"			/* define gdsa_colinq_c */
#include	"gdsa_rcint.h"			/* define gdsa_rcint_c */
#include	"gdsa_rcreal.h"			/* define gdsa_rcreal_c */
#include	"gdsbox.h"			/* define gdsbox_c */
#include	"gdsc_fill.h"			/* define gdsc_fill_c */
#include	"gdsc_grid.h"			/* define gdsc_grid_c */
#include	"gdsc_name.h"			/* define gdsc_name_c */
#include	"gdsc_ndims.h"			/* define gdsc_ndims_c */
#include	"gdsd_rchar.h"			/* define gdsd_rchar_c */
#include	"gdsd_rdble.h"			/* define gdsd_rdble_c */
#include	"gdsinp.h"			/* define gdsinp_c */
#include	"gdsi_read.h"			/* define gdsi_read_c */
#include	"gdsi_write.h"			/* define gdsi_write_c */
#include	"init.h"			/* define init_c */
#include	"nelc.h"			/* define nelc_c */
#include	"reject.h"			/* define reject_c */
#include	"status.h"			/* define status_c */
#include	"uminmax.h"			/* define uminmax_c */
#include	"userdble.h"			/* define userdble_c */
#include	"userreal.h"			/* define userreal_c */



/*
 * The defines:
 */

#define	BEAM(x,y)	beam[(MAXGRID+y)*(2*MAXGRID+1)+(MAXGRID+x)]
#define	CLASS		1		/* class of application */
#define	CLASSDIM	2		/* maximum dimension of subsets */
#define	MAXAXES		10		/* maximum number of axes */
#define	MAXCOLNAMLEN	8		/* maximum length of column name */
#define	MAXCOMP		50000		/* maximum number of components */
#define	MAXDATA		4096		/* maximum buffer length */
#define	MAXFITSCHARLEN	18		/* maximum length of FITS character */
#define	MAXGRID		64		/* maximum grid for clean beam */
#define	MAXMAPS		2048		/* maximum number of maps */
#define	MAXSETNAMLEN	80		/* maximum length of set names */
#define	MAXSTRINGLEN	80		/* maximum length of text strings */
#define	MAXVARLEN	150		/* maximum length for gdsd_wvar */
#define	VERSION		"1.1"		/* change version number here */

#define	MAX(x,y)	(x < y ? y : x)	/* returns max of x and y */
#define	MIN(x,y)	(x > y ? y : x)	/* returns min of x and y */


/*
 * The keywords:
 */

#define	KEY_INSET	tofchar("INSET=")
#define	KEY_CLEANBOX	tofchar("CLEANBOX=")
#define	KEY_BEAM	tofchar("BEAM=")
#define	KEY_BEAMPA	tofchar("BEAMPA=")
#define	KEY_FACTOR	tofchar("FACTOR=")

#define	MES_INSET	tofchar("Set and subsets(s) of CLEAN Residuals")
#define	MES_CLEANBOX	tofchar("Size of CLEANed area [whole subset]")
#define	MES_FACTOR	tofchar("Beam Multiplication Factor [1.0]")



/*
 * Tables:
 */

#define	TABLE_NAME	tofchar("CLEAN")
#define	TABLE_COLUMN_A	tofchar("AMP")
#define	TABLE_COLUMN_X	tofchar("XPOS")
#define	TABLE_COLUMN_Y	tofchar("YPOS")



/*
 * Variables:
 */

static	fint	classdim = 0;			/* dimension of subsets */



/*
 * readxy reads a part of a one or two-dimensional subset.
 */

static	void	readxy( fchar set,		/* name of set */
                        fint  subset,		/* subset level */
                        fint  xlo,		/* lower x grid */
                        fint  ylo,		/* lower y grid */
                        fint  xup,		/* upper x grid */
                        fint  yup,		/* upper y grid */
                        float *data )		/* the data to read */
{
   fint	cwlo;					/* lower coordinate word */
   fint	cwup;					/* upper coordinate word */
   fint	glo[CLASSDIM];				/* lower grid coordinates */
   fint	gup[CLASSDIM];				/* upper grid coordinates */
   fint	tid = 0;				/* transfer id */
   fint	ndone;					/* number of pixels read */
   fint	nread;					/* number of pixels to read */

   glo[0] = xlo; glo[1] = ylo;			/* lower grids */
   gup[0] = xup; gup[1] = yup;			/* upper grids */
   nread = (yup-ylo+1) * (xup-xlo+1);		/* number of pixels */
   cwlo = gdsc_fill_c( set, &subset, glo );	/* lower c.w. */
   cwup = gdsc_fill_c( set, &subset, gup );	/* upper c.w. */
   gdsi_read_c( set, &cwlo, &cwup, data, &nread, &ndone, &tid );
   if (tid) {					/* error ? */
      fint	error_level = 4;		/* FATAL error */

      error_c( &error_level , tofchar( "Error reading data!" ) );
   }
}



/*
 * writxy writes a part of a one or two-dimensional subset.
 */

static	void	writxy( fchar set,		/* name of set */
                        fint  subset,		/* subset level */
                        fint  xlo,		/* lower x grid */
                        fint  ylo,		/* lower y grid */
                        fint  xup,		/* upper x grid */
                        fint  yup,		/* upper y grid */
                        float *data )		/* the data to write */
{
   fint	cwlo;					/* lower coordinate word */
   fint	cwup;					/* upper coordinate word */
   fint	glo[CLASSDIM];				/* lower grid coordinates */
   fint	gup[CLASSDIM];				/* upper grid coordinates */
   fint	tid = 0;				/* transfer id */
   fint	ndone;					/* number of pixels written */
   fint	nwrit;					/* number of pixels to write */

   glo[0] = xlo; glo[1] = ylo;			/* lower grids */
   gup[0] = xup; gup[1] = yup;			/* upper grids */
   nwrit = ( yup - ylo + 1 ) * ( xup - xlo + 1 );	/* number of pixels */
   cwlo = gdsc_fill_c( set, &subset, glo );	/* lower c.w. */
   cwup = gdsc_fill_c( set, &subset, gup );	/* upper c.w. */
   gdsi_write_c( set, &cwlo, &cwup, data, &nwrit, &ndone, &tid );
   if (tid) {					/* error ? */
      fint	error_level = 4;		/* FATAL error */

      error_c( &error_level , tofchar( "Error writing data!" ) );
   }
}



/*
 * Variables for input (dirty) set:
 */

static	char	isetb[MAXSETNAMLEN];		/* buffer for set name */
static	fchar	iset = { isetb, MAXSETNAMLEN };	/* input set name */
static	fint	imaps[MAXMAPS];			/* input subsets */
static	fint	iperm[MAXAXES];			/* axes permutation */
static	fint	isize[MAXAXES];			/* size of axes */
static	fint	isetdim;			/* dimension of set */
static	fint	inmap;				/* number of input subsets */
static	fint	imin[CLASSDIM], imax[CLASSDIM];	/* edges of clean box */
static	fint	ilen[CLASSDIM];			/* sizes of clean box */

/*
 * Variables for the clean beam:
 */

static	fint	lblo, lbup, mblo, mbup;		/* edges of beam */
static	fint	lbx, lby;			/* beam size in grids */
static	float	beam[(2*MAXGRID+1)*(2*MAXGRID+1)];


MAIN_PROGRAM_ENTRY
{
   double	cdelt[CLASSDIM];		/* grid separations */
   double	mappa;				/* position angle of map */
   fint		maxdata = 0;			/* size of data buffer */
   fint		nmap;				/* map counter */
   float	*data = NULL;			/* data buffer */

   init_c( );					/* contact HERMES */
   IDENTIFICATION( "RESTORE", VERSION );	/* show user who we are */
   /*
    * First we ask the user for the set and subsets which he or she wants
    * to restore.
    */
   {
      char	descr[10];			/* buffer for FITS item */
      char	string[MAXSTRINGLEN];		/* text buffer */
      fint	class = CLASS;			/* class of application */
      fint	error_level = 4;		/* FATAL error */
      fint	gerror = 0;			/* GDS return code */
      fint	input_level = 0;		/* no default*/
      fint	level = 0;			/* top level */
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	maxmaps = MAXMAPS;		/* maximum number of maps */
      fint	output_level = 11;		/* output level */

      inmap = gdsinp_c( iset ,			/* input set name */
                        imaps ,			/* input subsets */
                        &maxmaps ,		/* maximum number of subsets */
                        &input_level ,		/* input level */
                        KEY_INSET ,		/* keyword */
                        MES_INSET ,		/* message */
                        &output_level ,		/* output level */
                        iperm ,			/* axes permutation */
                        isize ,			/* axes counter */
                        &maxaxes ,		/* maximum number of axis */
                        &class ,		/* class of program */
                        &classdim );		/* wanted dimension */
      if (classdim != 1 && classdim != 2) {     /* wrong dimension */
         fint   error_level = 4;                /* the error level */
               
         error_c( &error_level,
                  tofchar( "Subset dimension must be 1 or 2!" ) );
      }
      isetdim = gdsc_ndims_c( iset, &level );	/* dimension of set */
      {	
         fint	atsum = 0;			/* sum of axis types */
         fint	n;				/* loop counter */

         for (n = 0; n < classdim; n++) {	/* loop */
            char	ctypeb[MAXFITSCHARLEN];	/* text buffer */
            char	cunitb[MAXFITSCHARLEN];	/* primary units */
            char	dunitb[MAXFITSCHARLEN];	/* secondary units */
            double	f;			/* factor */
            fchar	ctype;			/* name of axis */
            fchar	cunit;			/* primary units of axis */
            fchar	dunit;			/* secondary units of axis */
            fint	atype;			/* type of axis */
            fint	prosys;			/* projection */
            fint	skysys;			/* sky system */
            fint	velsys;			/* velocity system */

            ctype.a = ctypeb; ctype.l = sizeof( ctypeb );
            cunit.a = cunitb; cunit.l = sizeof( cunitb );
            dunit.a = dunitb; dunit.l = sizeof( dunitb );
            gdsc_name_c( ctype, iset, &iperm[n], &gerror );
            atype = axtype_c( ctype ,		/* name of axis */
                              cunit ,		/* units of axis */
                              dunit ,		/* secondary units of axis */
                              &skysys ,		/* sky system */
                              &prosys ,		/* projection system */
                              &velsys );	/* velocity system */
            if (atype == 1 || atype == 2) {	/* Latitude or Longitude */
               char	aunitb[MAXFITSCHARLEN];	/* buffer */
               fchar	aunit;			/* units of axis */

               atsum += atype;			/* add axis type */
               sprintf( descr, "CDELT%d", iperm[n] );
               gdsd_rdble_c( iset ,		/* input set name */
                             tofchar( descr ) ,	/* the descriptor */
                             &level ,		/* top level */
                             &cdelt[n] ,	/* the grid separation */
                             &gerror );		/* GDS return code */
               if (gerror < 0) {		/* not found */
                  sprintf( string, "Descriptor CDELT%d not found!", iperm[n] );
                  error_c( &error_level, tofchar( string ) );
               }
               aunit.a = aunitb;		/* the address */
               aunit.l = sizeof( aunitb );	/* the length */
               sprintf( descr, "CUNIT%d", iperm[n] );
               gdsd_rchar_c( iset ,		/* input set name */
                             tofchar( descr ) ,	/* the descriptor */
                             &level ,		/* the level */
                             aunit ,		/* the value */
                             &gerror );		/* GDS return */
               if (gerror < 0) {		/* not found, use default */
                  aunit.a = cunit.a;		/* default is taken ... */
                  aunit.l = cunit.l;		/* ... from axtype */
                  gerror = 0;			/* reset */
               }
               factor_c( aunit, tofchar( "ARCSEC" ), &f );
               cdelt[n] = f * fabs( cdelt[n] );	/* grid size in arcsec */
               if (atype == 2) {		/* get rotation angle */
                  sprintf( descr, "CROTA%d", iperm[1] );
                  gdsd_rdble_c( iset ,		/* name of set */
                                tofchar( descr ) ,	/* name of FITS item */
                                &level ,	/* the subset level */
                                &mappa ,	/* the rotation */
                                &gerror );	/* GDS return code */
                  if (gerror) {			/* not found */
                     mappa = 0.0;		/* default */
                     gerror = 0;		/* reset */
                  }
               }
            } else {				/* wrong type */
               sprintf( string, "Cannot work with %.*s axis!", nelc_c( ctype ), ctype.a );
               error_c( &error_level, tofchar( string ) );
            }
         }
         switch( classdim ) {
            case 1: {
               if (atsum != 1 && atsum != 2) {
                  error_c( &error_level, tofchar( "Need a SKY axis!" ) );
               }
               break;
            }
            case 2: {
               if (atsum != 3) {		/* no sky plane */
                  error_c( &error_level, tofchar( "Need two different SKY axes!" ) );
               }
               break;
            }
            default: {
               break;
            }
         }
      }
   }
   /*
    * We have to get the beam sizes from the user or from the header.
    * The descriptors BMMIN, BMMAJ and BMPA hold the necessary information.
    */
   switch( classdim ) {
      case 1: {
         char	string[MAXSTRINGLEN];		/* text buffer */
         double	beamusr;			/* FWHP beam */
         fint	input_level;			/* input level */
         fint	items;				/* number of items */
         float	factor;				/* factor (+1.0 or -1.0) */

         do {					/* loop to get correct beam */
            input_level = 0;			/* no default */
            sprintf( string, "Half Power Beam Width (arcsec)" );
            items = 1;				/* two items wanted */
            (void) userdble_c( &beamusr ,	/* the beam */
                               &items ,		/* one */
                               &input_level ,	/* the input level */
                               KEY_BEAM ,	/* the keyword */
                               tofchar( string ) );/* the message */
            if (beamusr < 0.0) {		/* weird beam */
               sprintf( string, "Beam cannot be negative!" );
               reject_c( KEY_BEAM, tofchar( string ) );	/* reject keyword */
            } else {				/* nice beam */
               break;				/* leave loop */
            }
         } while (1);				/* until ... */
         /*
          * Get the multiplication factor.
          */
         {
            input_level = 2;			/* hidden keyword */
            items = 1;				/* only one item */
            factor = 1.0;			/* default */
            (void) userreal_c( &factor ,	/* the value */
                               &items ,		/* number of values */
                               &input_level ,	/* default level */
                               KEY_FACTOR ,	/* the keyword */
                               MES_FACTOR );	/* the message */
         }
         /*
          * Inform user about beam.
          */
         {
            char	string[MAXSTRINGLEN];	/* text buffer */
            fint	output_level = 3;	/* to screen and log file */

            sprintf( string, "Full With Half Max. : %8.2f arcsecs", beamusr );
            anyout_c( &output_level, tofchar( string ) );
            sprintf( string, "Beam Factor         : %8.3f", factor );
            anyout_c( &output_level, tofchar( string ) );
         }
         /*
          * Check for delta function.
          */
         if (beamusr == 0.0) {			/* delta function */
            beamusr = 0.10 * cdelt[0];
         }
         /*
          * Now we make the convolving beam.
          */
         {
            double	sig;
            double	x;			/* offset in arcsecs */
            fint	i;			/* counter */
            fint	l;			/* offset in grids */

            sig = beamusr / sqrt( log( 256.0 ) );
            lblo = MAXGRID; lbup = -MAXGRID;	/* initialize */
            mblo = 0; mbup = 0;			/* initialize */
            for (i = (MAXGRID * 2 + 1) * MAXGRID, l = -MAXGRID; l <= MAXGRID; l++, i++) {
               double	arg;			/* arg to exponent */

               x = ( cdelt[0] * l  ) / sig;
               arg = 0.5 * ( x * x );		/* the arg */
               if (arg > 25.0) {		/* essentially zero */
                  beam[i] = 0.0;
               } else {				/* get the gaussian */
                  beam[i] = factor * exp( -arg );
                  lblo = MIN( lblo, l );	/* new limits */
                  lbup = MAX( lbup, l );
               }
            }
            lbx = MAX( -lblo, lbup );		/* half beam in x */
            lby = MAX( -mblo, mbup );		/* half beam in y */
         }
         break;
      }
      case 2: {
         char	string[MAXSTRINGLEN];		/* text buffer */
         double	beamusr[2];			/* the beam */
         double	beampa;				/* the beam position angle */
         double	f;				/* factor from DEG to RAD */
         fint	derror = 0;			/* GDS error code */
         fint	input_level;			/* the level of the input */
         fint	items;				/* the number of items */
         float	factor;				/* factor (+1.0 or -1.0) */

         factor_c( tofchar( "DEGREE" ) ,	/* input type */
                   tofchar( "RADIAN" ) ,	/* output type*/
                   &f );			/* the factor*/
         gdsd_rdble_c( iset ,			/* name of input set */
                       tofchar( "BMMAJ" ) ,	/* the descriptor */
                       &imaps[0] ,		/* the first subset */
                       &beamusr[0] ,		/* the value */
                       &derror );		/* GDS return code */
         if (derror < 0) {			/* an error occurred */
            beamusr[0] = -1.0;			/* weird value */
            derror = 0;				/* reset */
         }
         gdsd_rdble_c( iset ,			/* name of input set */
                       tofchar( "BMMIN" ) ,	/* the descriptor */
                       &imaps[0] ,		/* the first subset */
                       &beamusr[1] ,		/* the value */
                       &derror );		/* GDS return code */
         if (derror < 0) {			/* an error occurred */
            beamusr[1] = -1.0;			/* weird value */
            derror = 0;				/* reset */
         }
         do {					/* loop to get correct beam */
            if (beamusr[0] > 0.0 && beamusr[1] > 0.0) {
               input_level = 5;			/* exact + default */
               sprintf( string, "Major and Minor axis of beam (arcsec) [%.2f,%.2f]", beamusr[0], beamusr[1] );
            } else {
               input_level = 4;			/* exact + no default */
               sprintf( string, "Major and Minor axis of beam (arcsec)" );
            }
            items = 2;				/* two items wanted */
            (void) userdble_c( beamusr ,	/* the beam */
                               &items ,		/* two */
                               &input_level ,	/* the input level */
                               KEY_BEAM ,	/* the keyword */
                               tofchar( string ) );/* the message */
            if (beamusr[0] < 0.0 || beamusr[1] < 0.0) {	/* weird beam */
               sprintf( string, "Beam cannot be negative!" );
               reject_c( KEY_BEAM, tofchar( string ) );	/* reject keyword */
            } else if (beamusr[0] < beamusr[1]) {	/* stupid */
               double	b;				/* extra */
               fint	output_level = 1;		/* to screen */

               anyout_c( &output_level, tofchar( "The Major axis is smaller than the Minor axis, you ...." ) );
               anyout_c( &output_level, tofchar( "Anyway, I will correct your mistake!" ) );
               b = beamusr[0]; beamusr[0] = beamusr[1]; beamusr[1] = b;
               break;
            } else {				/* nice beam */
               break;				/* leave loop */
            }
         } while (1);				/* until ... */
         gdsd_rdble_c( iset ,			/* name of input set */
                       tofchar( "BMPA" ) ,	/* the descriptor */
                       &imaps[0] ,		/* the first subset */
                       &beampa ,		/* the value */
                       &derror );		/* GDS return code */
         if (derror < 0) {			/* an error occurred */
            sprintf( string, "Position angle of Major axis (degrees)" );
            input_level = 0;			/* no default */
            derror = 0;				/* reset */
         } else {
            sprintf( string, "Position angle of Major axis (degrees) [%.2f]", beampa );
            input_level = 1;			/* default possible */
         }
         items = 1;				/* one item */
         (void) userdble_c( &beampa ,		/* the value */
                            &items ,		/* just one */
                            &input_level ,	/* the input level */
                            KEY_BEAMPA ,	/* the keyword */
                            tofchar( string ) );/* the message */
         /*
          * Get the multiplication factor.
          */
         {
            input_level = 2;			/* hidden keyword */
            items = 1;				/* only one item */
            factor = 1.0;			/* default */
            (void) userreal_c( &factor ,	/* the value */
                               &items ,		/* number of values */
                               &input_level ,	/* default level */
                               KEY_FACTOR ,	/* the keyword */
                               MES_FACTOR );	/* the message */
         }
         /*
          * Inform user about beam.
          */
         {
            char	string[MAXSTRINGLEN];	/* text buffer */
            fint	output_level = 3;	/* to screen and log file */

            sprintf( string, "Beam Major axis     : %8.2f arcsecs", beamusr[0] );
            anyout_c( &output_level, tofchar( string ) );
            sprintf( string, "Beam Minor axis     : %8.2f arcsecs", beamusr[1] );
            anyout_c( &output_level, tofchar( string ) );
            sprintf( string, "Beam Position Angle : %8.2f degrees", beampa );
            anyout_c( &output_level, tofchar( string ) );
            sprintf( string, "Beam Factor         : %8.3f", factor );
            anyout_c( &output_level, tofchar( string ) );
         }
         /*
          * Check for delta function.
          */
         {
            fint	i;

            for ( i = 0; i < 2; i++) {
               if (beamusr[i] == 0.0) {		/* delta function */
                  beamusr[i] = 0.10 * cdelt[i];
               }
            }
         }
         /*
          * Now we make the convolving beam.
          */
         {
            double	cosphi, sinphi;		/* cosine and sine */
            double	sigmaj, sigmin;
            double	x, y;			/* offset in arcsecs */
            double	phi;			/* rotation angle */
            fint	i;			/* counter */
            fint	l, m;			/* offset in grids */

            phi = f * ( beampa - mappa );	/* w.r.t. grid */
            cosphi = cos( phi );		/* cosine of rotation angle */
            sinphi = sin( phi );		/* sine of rotation angle */
            sigmaj = beamusr[0] / sqrt( log( 256.0 ) );
            sigmin = beamusr[1] / sqrt( log( 256.0 ) );
            lblo = MAXGRID; lbup = -MAXGRID;	/* initialize */
            mblo = MAXGRID; mbup = -MAXGRID;	/* initialize */
            for (i = 0, m = -MAXGRID; m <= MAXGRID; m++) {
               for (l = -MAXGRID; l <= MAXGRID; l++, i++) {
                  double	arg;		/* arg to exponent */

                  x = ( cdelt[0] * l * cosphi + cdelt[1] * m * sinphi ) / sigmin;
                  y = ( cdelt[1] * m * cosphi - cdelt[0] * l * sinphi ) / sigmaj;
                  arg = 0.5 * ( x * x + y * y );/* the arg */
                  if (arg > 25.0) {		/* essentially zero */
                     beam[i] = 0.0;
                  } else {			/* get the gaussian */
                     beam[i] = factor * exp( -arg );
                     lblo = MIN( lblo, l );	/* new limits */
                     lbup = MAX( lbup, l );
                     mblo = MIN( mblo, m );
                     mbup = MAX( mbup, m );
                  }
               }
            }
            lbx = MAX( -lblo, lbup );		/* half beam in x */
            lby = MAX( -mblo, mbup );		/* half beam in y */
         }
         break;
      }
      default: {
         break;
      }
   }
   /*
    * Here we get the CLEANBOX from the user. Only inside this region the
    * restoration is done.
    */
   {
      fint	blo[CLASSDIM];			/* lower grids */
      fint	bup[CLASSDIM];			/* upper grids */
      fint	i;				/* loop counter */
      fint	input_level = 2;		/* hidden */
      fint	output_level = 11;		/* output level */
      fint	option = 0;			/* default entire subset */

      gdsbox_c( blo ,				/* lower edge */
                bup ,				/* upper edge */
                iset ,				/* name input set */
                imaps ,				/* first input subset */
                &input_level ,			/* input level */
                KEY_CLEANBOX ,			/* the keyword */
                MES_CLEANBOX ,			/* the message */
                &output_level ,			/* output level */
                &option );			/* the box option */
      for ( i = 0; i < classdim; i++) {
         imin[i] = blo[i];			/* lower edge */
         imax[i] = bup[i];			/* upper edge */
         ilen[i] = bup[i] - blo[i] + 1;		/* length */
      }
      for ( i = classdim; i < CLASSDIM; i++) {
         imin[i] = imax[i] = 0;
         ilen[i] = 1;
      }
      /*
       * Now we allocate space for the data buffer. We only do this once.
       */
      maxdata = MAX( ilen[0], MAXDATA );		/* minimum required */
      data = calloc( maxdata, sizeof( float ) );
      if (data == NULL) {			/* error allocating memory */
         fint	error_level = 4;		/* FATAL error */

         error_c( &error_level ,		/* the error level */
                  tofchar( "Cannot allocate memory for read buffer" ) );
      }
   }
   for (nmap = 0; nmap < inmap; nmap++) {	/* loop over all subsets */
      fint	error_level;			/* error level */
      fint	nmax = 0;			/* number of components */
      fint	*xpos = NULL;			/* x positions */
      fint	*ypos = NULL;			/* y positions */
      float	*amps = NULL;			/* amplitudes */

      /*
       * First we obtain the clean components from the table.
       */
      {
         char	ccomms[MAXVARLEN];		/* column comment */
         char	ctypes[MAXVARLEN];		/* column types */
         char	cunits[MAXVARLEN];		/* column units */
         fchar	ccomm;				/* points to buffer above */
         fchar	ctype;				/* points to buffer above */
         fchar	cunit;				/* points to buffer above */
         fint	aerror = 0;			/* GDS return code */
         fint	nrows[3];			/* number of rows */

         ccomm.a = ccomms; ccomm.l = MAXVARLEN;
         ctype.a = ctypes; ctype.l = MAXVARLEN;
         cunit.a = cunits; cunit.l = MAXVARLEN;
         gdsa_colinq_c( iset ,			/* input set name */
                        &imaps[nmap] ,		/* input subset level */
                        TABLE_NAME ,		/* name of table */
                        TABLE_COLUMN_A ,	/* name of column */
                        ctype ,			/* column type */
                        ccomm ,			/* column comment */
                        cunit ,			/* column units */
                        &nrows[0] ,		/* number of rows */
                        &aerror );		/* GDS return code */
         if (aerror < 0) {			/* an error occurred */
            nrows[0] = -1;			/* error coding */
            aerror = 0;				/* reset */
         }
         gdsa_colinq_c( iset ,			/* input set name */
                        &imaps[nmap] ,		/* input subset level */
                        TABLE_NAME ,		/* name of table */
                        TABLE_COLUMN_X ,	/* name of column */
                        ctype ,			/* column type */
                        ccomm ,			/* column comment */
                        cunit ,			/* column units */
                        &nrows[1] ,		/* number of rows */
                        &aerror );		/* GDS return code */
         if (aerror < 0) {			/* an error occurred */
            nrows[1] = -1;			/* error coding */
            aerror = 0;				/* reset */
         }
         switch( classdim ) {
            case 1: {
               nrows[2] = nrows[1];
               break;
            }
            case 2: {
               gdsa_colinq_c( iset ,		/* input set name */
                              &imaps[nmap] ,	/* input subset level */
                              TABLE_NAME ,	/* name of table */
                              TABLE_COLUMN_Y ,	/* name of column */
                              ctype ,		/* column type */
                              ccomm ,		/* column comment */
                              cunit ,		/* column units */
                              &nrows[2] ,	/* number of rows */
                              &aerror );	/* GDS return code */
               if (aerror < 0) {		/* an error occurred */
                  nrows[2] = -1;		/* error coding */
                  aerror = 0;			/* reset */
               }
               break;
            }
            default: {
               break;
            }
         }
         if (nrows[0] != nrows[1] || nrows[1] != nrows[2]) {
            error_level = 1;			/* warning */
            error_c( &error_level ,
                     tofchar( "Error reading CLEAN component TABLE!" ) );
         } else if (nrows[0] == -1) {		/* no table present */
            error_level = 1;			/* error level */
            error_c( &error_level ,
                     tofchar( "No CLEAN components found!" ) );
         } else {				/* table present */
            nmax = nrows[0];			/* the number of components */
         }
         if (nmax) {				/* we have something */
            /*
             * Generate a message for the user.
             */
            {
               char	string[MAXSTRINGLEN];	/* text buffer */
               fint	n;			/* loop counter */

               sprintf( string, "Restoring %d components to set %.*s", nmax, nelc_c( iset ), iset.a );
               for (n = classdim; n < isetdim; n++) {
                  char	buffer[MAXSTRINGLEN];	/* buffer */
                  char	ctypeb[MAXFITSCHARLEN+1];
                  fchar	ctype;			/* axis name */
                  fint	gerror = 0;		/* GDS error return */
                  fint	grid;			/* the subset grid */
                  fint	l;			/* counter */

                  ctype.a = ctypeb; ctype.l = MAXFITSCHARLEN;
                  gdsc_name_c( ctype, iset, &iperm[n], &gerror );
                  for (l = 0; l < ctype.l && ctype.a[l] != '-' && ctype.a[l] != ' '; l++);
                  grid = gdsc_grid_c( iset, &iperm[n], &imaps[nmap], &gerror );
                  sprintf( buffer, ",%.*s=%d", l, ctype.a, grid );
                  strcat( string, buffer );	/* concatenate */
               }
               status_c( tofchar( string ) );	/* message for user */
            }
            amps = calloc( nmax, sizeof( float ) );
            xpos = calloc( nmax, sizeof( fint ) );
            ypos = calloc( nmax, sizeof( fint ) );
            if (amps == NULL || xpos == NULL || ypos == NULL) {
               error_level = 1;			/* warning */
               error_c( &error_level ,
                        tofchar( "Cannot allocate enough memory!" ) );
               nmax = 0;			/* no restoration */
               if (amps != NULL) free( amps );	/* release memory */
               if (xpos != NULL) free( xpos );	/* release memory */
               if (ypos != NULL) free( ypos );	/* release memory */
            } else {				/* okay, go on */
               fint	start = 1;		/* starting position */

               gdsa_rcreal_c( iset ,		/* input set name */
                              &imaps[nmap] ,	/* input subset level */
                              TABLE_NAME ,	/* table name */
                              TABLE_COLUMN_A ,	/* column name */
                              amps ,		/* the amplitudes */
                              &start ,		/* starting position */
                              &nmax ,		/* end position */
                              &aerror );	/* GDS error return */
               gdsa_rcint_c( iset ,		/* input set name */
                             &imaps[nmap] ,	/* input subset level */
                             TABLE_NAME ,	/* table name */
                             TABLE_COLUMN_X ,	/* column name */
                             xpos ,		/* the x positions */
                             &start ,		/* starting position */
                             &nmax ,		/* end position */
                             &aerror );		/* GDS error return */
               switch( classdim ) {
                  case 1: {
                     fint	i;

                     for ( i = 0; i < nmax; i++ ) {
                        ypos[i] = 0;
                     }
                     break;
                  }
                  case 2: {
                     gdsa_rcint_c( iset ,	/* input set name */
                                   &imaps[nmap] ,	/* input subset level */
                                   TABLE_NAME ,	/* table name */
                                   TABLE_COLUMN_Y ,	/* column name */
                                   ypos ,	/* the y positions */
                                   &start ,	/* starting position */
                                   &nmax ,	/* end position */
                                   &aerror );	/* GDS error return */
                     break;
                  }
                  default: {
                     break;
                  }
               }
            }
         }
      }
      if (nmax) {				/* do the restoration */
         fint	mdone = 0;			/* number of lines done */
         fint	mleft = ilen[1];		/* number of lines left */
         fint	mstep = maxdata / ilen[0];	/* number of lines */

         do {					/* loop */
            fint	m, m1, m2;		/* line numbers */

            mstep = MIN(mstep,mleft);		/* lines to do this time */
            m1 = imin[1] + mdone;		/* first line to do */
            m2 = m1 - 1 + mstep;		/* last line to do */
            readxy( iset ,			/* input set name */
                    imaps[nmap] ,		/* input subset level */
                    imin[0] ,			/* lower x */
                    m1 ,			/* lower y */
                    imax[0] ,			/* upper x */
                    m2 ,			/* upper y */
                    data );			/* the residual data */
            for (m = m1; m <= m2; m++) {	/* restore line by line */
               fint	n;			/* component counter */
               fint	k0 = (m-m1) * ilen[0];	/* line pointer */

               for (n = 0; n < nmax; n++) {	/* component loop */
                  fint	lc = xpos[n];		/* x component position */
                  fint	mc = ypos[n];		/* y component position */

                  if (labs(m-mc) <= lby) {	/* inside beam */
                     fint	l;		/* x counter */
                     fint	l1 = MAX(lc-lbx,imin[0]);	/* start x position */
                     fint	l2 = MIN(lc+lbx,imax[0]);	/* end x position */

                     for (l = l1; l <= l2; l++) {	/* convolve */
                        data[k0+l-imin[0]] += amps[n] * BEAM(l-lc,m-mc);
                     }
                  }
               }
            }
            writxy( iset ,			/* input set name */
                    imaps[nmap] ,		/* input subset level */
                    imin[0] ,			/* lower x */
                    m1 ,			/* lower y */
                    imax[0] ,			/* upper x */
                    m2 ,			/* upper y */
                    data );			/* the restored data */
            mdone += mstep;			/* update lines done */
            mleft -= mstep;			/* update lines left */
         } while (mleft);			/* until nothing left */
      }
      if (nmax) {				/* release memory */
         free( amps ); free( xpos ); free( ypos );
      }
   }
   if (data != NULL) free( data );		/* release meory */
   {
      fint	change = 1;			/* min and max have changed */

      status_c( tofchar( "Updating minimum and maximum" ) );
      uminmax_c( iset ,				/* input set name */
                 imaps ,			/* input subset levels */
                 &inmap ,			/* number of subsets */
                 &change );			/* change */
   }
   finis_c( );					/* bye HERMES */
   return( EXIT_SUCCESS );			/* return with status */
}
