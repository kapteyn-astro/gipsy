/* ellfit.c

	Copyright (c) Kapteyn Laboratorium Groningen 1992
	All Rights Reserved.
                
#>            ellfit.dc1

Programme:    ELLFIT

Purpose:      Programme fits an ellipse to an user defined bunch of points.

Category:     ANALYSIS, FITTING, TABLES

File:         ellfit.c

Author:       K.G. Begeman

Description:  ELLFIT will fit an ellipse to an user defined bunch of points.
              The data set currently displayed will be used. The user can
              define his points by choosing two contourlevels, in between
              lying points are taken as points belonging to an arbitrary
              ellipse (five parameters). The user can remove points with
              the cursor. Next a least squares fit to these points X(i), Y(i)
              is done, with an error calculation. If the fit was successfull,
              the fitted ellipse will be displayed.
              The results of the ellipse fit are stored in table ELLFIT.

Keywords:

** DISPLAY=   Display device [DEFAULT_DISPLAY].

** COLOUR1=   RGB intensities for graphics plane which defines the region
              of interest; only coordinates inside this region will be
              used for fitting the ellipse [1.0, 1.0, 1.0].

** COLOUR2=   RGB intensities for graphics plane which defines the
              coordinates which have data values in the range specified
              with the RANGE= keyword [1.0, 0.0, 0.0].

** COLOUR3=   RGB intensities for graphics plane which allows the user
              to undefine points selected with the RANGE= keyword
              [0.0, 1.0, 0.0].

** COLOUR4=   RGB intensities for graphics plane which displays the
              fitted ellipse [0.0, 0.0, 1.0].

** PLOT=      Plot fitted ellipse [Y]?

              ---------------------------------------------------------
              The user can now define the search area, that is the region
              of interest. Coordinates on the ellipse can only be taken
              from this region.

              Next the programme goes into a loop which allows the user
              to fit as many ellipses as he wants.
              ---------------------------------------------------------

   RANGE=     Two data values in between which the points on the ellipse
              are defined. After these have been supplied, the points
              inside this range are shown on the display.

   EDIT=      Edit points on ellipse? [Y]. The user can remove some of
              the unwanted points with the cursor. The user can draw
              polygons on the screen in a similar way as in defining the
              search area. Any point selected by RANGE= inside a polygon
              is unselected.

** AUTO=      Automatic initial estimates or user supplied [Y].
              If AUTO=N, ELLFIT prompts for the initial estimates with
              the following keyword:

   INIPAR=    Enter initial estimates for RADIUS, INCL, XPOS, YPOS and
              PA.

              Next the program will load the positions and do the least
              squares fitting. In case you want to keep some parameters
              fixed for your owm special purpose, you should type the
              appropriate keyword with the desired value (see keyword list
              below). When you want to free a parameter again, just type
              the corresponding keyword without any value.

** RADIUS=    Semi Major axis of ellipse (arcsec/pixels).

** INCL=      Inclination angle of ellipse (degrees).

** XPOS=      X position of centre of ellipse (grids).

** YPOS=      Y position of centre of ellipse (grids).

** PA=        Position of major axis (degrees).

              ---------------------------------------------------------
              After the fit the programme will display the results
              with the corresponding errors and prompt the user whether
              is should stop or continue.
              ---------------------------------------------------------

   CONTINUE=  Continue fitting ellipses? [Y].

   FILE=      Name of file to save fitted results [ellfit.dat].

Updates:      Jan 28, 1991: KGB Document created.
              Jun 23, 1993: KGB Results in GDS table.
              May 23, 2002: JPT Increased maximum set name length.
              Apr 15, 2009: VOG Replaced nint() by a function that uses floor()
                                to be compatible with other routines that
                                process coordinates.

#<

*/


/*
 * Includes:
 */

#include	"math.h"			/* <math.h> */
#include	"stddef.h"			/* <stddef.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */
#include	"gipsyc.h"			/* GIPSY definitions */
#include	"cmain.h"			/* main program in C */
#include	"anyout.h"			/* define anyout_c */
#include	"axtype.h"			/* define axtype_c */
#include	"cancel.h"			/* define cancel_c */
#include	"ellipse1.h"			/* define ellipse1_c */
#include	"ellipse2.h"			/* define ellipse2_c */
#include	"error.h"			/* define error_c */
#include	"factor.h"			/* define factor_c */
#include	"finis.h"			/* define finis_c */
#include	"gdi_close.h"			/* define gdi_close_c */
#include	"gdi_ginfo.h"			/* define gdi_ginfo_c */
#include	"gdi_grcol.h"			/* define gdi_grcol_c */
#include	"gdi_groff.h"			/* define gdi_groff_c */
#include	"gdi_gron.h"			/* define gdi_gron_c */
#include	"gdi_grread.h"			/* define gdi_grread_c */
#include	"gdi_grregion.h"		/* define gdi_grregion_c */
#include	"gdi_grwrite.h"			/* define gdi_grwrite_c */
#include	"gdi_iinfo.h"			/* define gds_iinfo_c */
#include	"gdi_open.h"			/* define gdi_open_c */
#include	"gdsparams.h"			/* define GDS_NAMLEN */
#include	"gds_exist.h"			/* define gds_exist_c */
#include	"gdsa_colinq.h"			/* define gdsa_colinq_c */
#include	"gdsa_crecol.h"			/* define gdsa_crecol_c */
#include	"gdsa_wcint.h"			/* define gdsa_wcint_c */
#include	"gdsa_wcreal.h"			/* define gdsa_wcreal_c */
#include	"gdsc_fill.h"			/* define gdsc_fill_c */
#include	"gdsc_grid.h"			/* define gdsc_grid_c */
#include	"gdsc_name.h"			/* define gdsc_name_c */
#include	"gdsc_ndims.h"			/* define gdc_ndims_c */
#include	"gdsd_rchar.h"			/* define gdsd_rchar_c */
#include	"gdsd_rdble.h"			/* define gdsd_rdble_c */
#include	"gdsi_read.h"			/* define gdsi_read_c */
#include	"init.h"			/* define init_c */
#include	"nelc.h"			/* define nelc_c */
#include	"rankra.h"			/* define rankra_c */
#include	"setfblank.h"			/* define setfblank_c */
#include	"statr.h"			/* define statr_c */
#include	"status.h"			/* define status_c */
#include	"userchar.h"			/* define userchar_c */
#include	"userlog.h"			/* define userlog_c */
#include	"userreal.h"			/* define userreal_c */
#include	"usertext.h"			/* define usertext_c */



/*
 *  Defines:
 */

#define	F		0.0174532925		/* from degrees to radians */
#define	MAXAXES		10			/* maximum number of axes */
#define	MAXELLIPSE	100			/* # of rows in table */
#define	MAXFITSCHARLEN	18			/* max. length FITS char */
#define	MAXKEYLEN	20			/* max. length of keyword */
#define	MAXMESLEN	80			/* max. length of message */
#define	MAXPARS		5			/* # of parameters */
#define	MAXPOINTS	10000			/* # of points on ellipse */
#define	MAXVARLEN	132			/* length of var records */
#define	PLANES		4			/* number of planes */
#define	PROGRAM		"ELLFIT"		/* name of program */
#define	TABLE_NAME	tofchar( PROGRAM )	/* name of table */
#define	TEXTLEN		80			/* length of text strings */
#define	VERSION		"1.1"			/* version number of program */

#define		finit( f, s )	{ \
   int	l; \
   for (l = 0; l < sizeof( s ); s[l++] = ' '); \
   f.a = s; f.l = l; \
}



/*
 * Keywords and messages:
 */

#define	KEY_AUTO	tofchar("AUTO=")
#define	KEY_CONTINUE	tofchar("CONTINUE=")
#define	KEY_DISPLAY	tofchar("DISPLAY=")
#define	KEY_EDIT	tofchar("EDIT=")
#define	KEY_FILE	tofchar("FILE=")
#define	KEY_INIPAR	tofchar("INIPAR=")
#define	KEY_PLOT	tofchar("PLOT=")
#define	KEY_RANGE	tofchar("RANGE=")
#define	MES_AUTO	tofchar("Automatic determination of initial estimates? [Y]")
#define	MES_CONTINUE	tofchar("Continue fitting ellipses? [Y]")
#define	MES_DISPLAY	tofchar("Display device [DEFAULT_DISPLAY]")
#define	MES_EDIT	tofchar("Edit selected points? [Y]")
#define	MES_FILE	tofchar("Name of file to store data [ellfit.dat]")
#define	MES_INIPAR	tofchar("Enter initial astimates")
#define	MES_PLOT	tofchar("Plot fitted ellipse [Y]?")
#define	MES_RANGE	tofchar("Range which defines points on ellipse")



/*
 * Variables:
 */

static float	colours[PLANES][3] = {		/* the default colours */
   { 1.0, 1.0, 1.0 },				/* for plane #1 */
   { 1.0, 0.0, 0.0 },				/* for plane #2 */
   { 0.0, 1.0, 0.0 },				/* for plane #3 */
   { 0.0, 0.0, 1.0 }				/* for plane #4 */
};

static	char	parkey[MAXPARS][MAXKEYLEN] = {	/* the keywords */
   "RADIUS=",					/* radius of ellipse */
   "INCL=",					/* inclination of ellipse */
   "XPOS=",					/* X centre of ellipse */
   "YPOS=",					/* Y centre of ellipse */
   "PA="					/* Position angle of ellipse */
};

static	char	parmes[MAXPARS][MAXMESLEN] = {
   "Enter radius of ellipse",
   "Enter Inclination of ellipse",
   "Enter X centre of ellipse",
   "Enter Y centre of ellipse",
   "Enter position angle of ellipse"
};

/*
static	fint	nint( double d )
{
   if ( d > 0.0 ) {
      return( (fint) ( d + 0.5 ) );
   } else {
      return( (fint) ( d - 0.5 ) );
   }
}
*/
static	fint	nint( double d )
{
   return ( (fint) floor(d + 0.5) );
}


/*
 * main program
 */

MAIN_PROGRAM_ENTRY
{
   bool		aut = TRUE;			/* automatic mode */
   bool		cont = TRUE;			/* continuation logical */
   bool		plot = TRUE;			/* plot fitted ellipse */
   char		bunitb[MAXFITSCHARLEN];		/* buffer for bunit */
   char		*gdata;				/* pointer to graphics data */
   char		setb[GDS_NAMLEN];		/* buffer for set name */
   char		sky[20];			/* arcsec or pixels */
   double	cdelt[2];			/* pixel separations */
   double	mapphi = 0.0;			/* rotation angle of map */
   fchar	bunit;				/* units of data */
   fchar	set;				/* points to setb */
   fint		blo[2], bup[2];			/* subset frame loaded image */
   fint		cwlo, cwup;			/* frame in c.w.'s */
   fint		display_id;			/* id of display */
   fint		display_stat;			/* display operation status */
   fint		pack = 4;			/* pack bytes */
   fint		perm[2];			/* axis permutation */
   fint		ndata;				/* number of data values */
   fint		nellipse = 0;			/* number of ellipse pars. */
   fint		np[MAXELLIPSE];			/* points in fit */
   fint		subset;				/* subset level loaded image */
   fint		type[2];			/* axis type */
   fint		ilen, jlen;			/* length of box */
   float	blank;				/* the BLANK */
   float	*idata;				/* pointer to image data */
   float	ie[MAXELLIPSE];			/* errors in inclination */
   float	iv[MAXELLIPSE];			/* inclination */
   float	pe[MAXELLIPSE];			/* error in position angle */
   float	pv[MAXELLIPSE];			/* position angle */
   float	re[MAXELLIPSE];			/* error in semi major axis */
   float	rv[MAXELLIPSE];			/* semi major axis */
   float	ve[MAXELLIPSE];			/* spread in map value */
   float	vv[MAXELLIPSE];			/* map value */
   float	xe[MAXELLIPSE];			/* error in x position */
   float	xv[MAXELLIPSE];			/* x position */
   float	ye[MAXELLIPSE];			/* error in y position */
   float	yv[MAXELLIPSE];			/* y position */

   init_c( );					/* get in touch with HERMES */
   IDENTIFICATION( PROGRAM, VERSION );		/* reveal thyself */
   finit( bunit, bunitb );			/* initialise unit string */
   finit( set, setb );				/* initialize set name string */
   setfblank_c( &blank );			/* get the BLANKs */
   /*
    * First we need the name of the display device. When we've got it,
    * we try to open it. If this is not successfull, just give an
    * error message and quit.
    */
   {
      char	display_nameb[TEXTLEN];		/* buffer for display name */
      fchar	display_name;			/* points to buffer */
      fint	error_level = 4;		/* fatal error */
      fint	input_level = 2;		/* hidden keyword */

      finit( display_name, display_nameb );	/* initialize string */
      (void) usertext_c( display_name ,		/* name of display */
                         &input_level ,		/* input level */
                         KEY_DISPLAY ,		/* keyword */
                         MES_DISPLAY );		/* message */
      display_id = gdi_open_c( display_name );	/* open display device */
      if (display_id < 0) {			/* error opening display */
         error_c( &error_level ,		/* level of error */
                  tofchar( "Could not open display!" ) );
      }
   }
   /*
    * Now we try to obtain the name, level and frame of the set which
    * is currently on the display. When we've got the information we need,
    * we check whether the set is still available on disk. If the set is
    * not present on disk, or if no image is loaded in the display device,
    * we simply quit with an error message.
    * Next the image data is read from the set.
    */
   {
      char	message[MAXMESLEN];		/* buffer for message */
      char	ctypeb[MAXFITSCHARLEN];
      char	cunitb[MAXFITSCHARLEN];
      char	dunitb[MAXFITSCHARLEN];
      fchar	ctype, cunit, dunit;
      fint	error_level = 4;		/* fatal error */
      fint	dummy;				/* just a dummy */
      fint	gerror = 0;			/* GDS status return */
      fint	m, n;				/* counters */
      fint	output_level = 3;		/* output to screen and log */
      fint	setdim;				/* dimension of set */
      fint	transfer_id = 0;		/* gds transfer identifier */
      fint	zero = 0;			/* zero */

      finit( ctype, ctypeb );
      finit( cunit, cunitb );
      finit( dunit, dunitb );
      display_stat = gdi_iinfo_c( &display_id ,	/* id of display */
                                  set ,		/* name of set */
                                  &subset ,	/* subset level */
                                  blo ,		/* lower left frame boundary */
                                  bup );	/* upper right frame boundary */
      if (display_stat < 0) {			/* error obtaining info */
         error_c( &error_level ,		/* level of error */
                  tofchar( "No image loaded! Use VIEW!" ) );
      }
      if (!tobool( gds_exist_c( set, &gerror ) ) ) {
         error_c( &error_level ,		/* level of error */
                  tofchar( "Set not present!" ) );
      }
      ilen = bup[0] - blo[0] + 1;
      jlen = bup[1] - blo[1] + 1;
      sprintf( message, "Set %.*s", (int) nelc_c( set ), set.a );
      anyout_c( &output_level, tofchar( message ) );
      if (gdsc_ndims_c( set, &subset ) != 2) {
         error_c( &error_level, tofchar( "Wrong dimension!" ) );
      }
      setdim = gdsc_ndims_c( set, &zero );
      for (m = n = 0; m < 2 && n < setdim; n++) {
         fint	axnum = n + 1;
         fint	gerror = 0;
         fint	grid;

         grid = gdsc_grid_c( set, &axnum, &subset, &gerror );
         if (gerror) {
            fint	skysys, prosys, velsys;

            gerror = 0;
            perm[m] = axnum;
            gdsc_name_c( ctype, set, &axnum, &gerror );
            type[m] = axtype_c( ctype, cunit, dunit, &skysys, &prosys, &velsys );
            sprintf( message, "%.*s from %5d to %5d", MAXFITSCHARLEN, ctypeb, blo[m], bup[m] );
            anyout_c( &output_level, tofchar( message ) );
            m += 1;
         }
      }
      if ((type[0] == 1 && type[1] == 2) || (type[0] == 2 && type[1] == 1)) {
         char	descr[10];

         strcpy( sky, "arcsec" );
         if (type[0] == 2) {
            sprintf( descr, "CROTA%d", perm[0] );
         } else {
            sprintf( descr, "CROTA%d", perm[1] );
         }
         gdsd_rdble_c( set, tofchar( descr ), &zero, &mapphi, &gerror );
         if (gerror) { gerror = 0; mapphi = 0.0; }
         for (m = 0; m < 2; m++) {
            sprintf( descr, "CDELT%d", perm[m] );
            gdsd_rdble_c( set, tofchar( descr ), &zero, &cdelt[m], &gerror );
            if (gerror) { gerror = 0; cdelt[m] = 1.0; }
            sprintf( descr, "CUNIT%d", perm[m] );
            gdsd_rchar_c( set, tofchar( descr ), &zero, cunit, &gerror );
            if (gerror) {
               gerror = 0;
               cdelt[m] *= 3600.0;
            } else {
               double	f;

               (void) factor_c( cunit, tofchar( "ARCSEC" ), &f );
               cdelt[m] *= f;
               cdelt[m] = fabs( cdelt[m] );
            }
         }
      } else {
         strcpy( sky, "pixels" );
         cdelt[0] = cdelt[1] = 1.0;
         mapphi = 0.0;
      }
      cwlo = gdsc_fill_c( set, &subset, blo );	/* lower c.w. */
      cwup = gdsc_fill_c( set, &subset, bup );	/* upper c.w. */
      ndata = ( bup[0] - blo[0] + 1 ) * ( bup[1] - blo[1] + 1 );
      idata = calloc( sizeof( float ), ndata );
      gdata = calloc( sizeof( char ), ndata );
      if (idata == NULL || gdata == NULL) {	/* error creating memory */
         error_c( &error_level ,		/* level of error */
                  tofchar( "Cannot allocate enough memory!" ) );
      }
      gdsi_read_c( set ,			/* name of set */
                   &cwlo ,			/* lower left cw */
                   &cwup ,			/* upper right cw */
                   idata ,			/* data buffer */
                   &ndata ,			/* number of data to read */
                   &dummy ,			/* number actually read */
                   &transfer_id );		/* transfer identifier */
      gerror = 0;
      gdsd_rchar_c( set, tofchar( "BUNIT" ), &subset, bunit, &gerror );
      if (gerror < 0) strcpy( bunit.a, "unknown" );
   }
   /*
    * Now we ask the user to enter the colours of the graphics planes
    * used by this program. ELLFIT needs four graphics planes:
    * 1 for defining the region of interest. Only coordinates inside this
    *   region will be used for fitting the ellipse.
    * 2 for defining the coordinates on the ellipse inside the first region
    *   on data values in between the specified range.
    * 3 for undefining coordinates on the ellipse.
    * 4 for displaying the fitted ellipse.
    *
    * First of all, we need to check whether the display has enough
    * graphics planes.
    */
   {
      fint	error_level = 4;		/* fatal error */
      fint	n;				/* counter */
      fint	nplanes;			/* number of graphics planes */
      fint	onmask;				/* planes which are on */

      display_stat = gdi_ginfo_c( &display_id ,	/* display identifier */
                                  &nplanes ,	/* the number of planes */
                                  &onmask );	/* these planes are on */
      if (display_stat < 0 || nplanes < PLANES) {
         error_c( &error_level ,		/* level of error */
                  tofchar( "Not enough graphics planes!" ) );
      }
      for (n = 0; n < PLANES; n++) {		/* loop to set the colours */
         char	keyword[TEXTLEN];		/* buffer for keyword */
         char	message[TEXTLEN];		/* message for user */
         fint	input_level = 6;		/* hidden and exact */
         fint	nitems = 3;			/* # of items */
         fint	plane = n + 1;			/* plane number */

         (void) sprintf( keyword ,		/* keyword for user */
                         "COLOUR%d=" ,		/* the keyword */
                         plane );		/* the plane number */
         (void) sprintf( message ,		/* message for user */
                         "RGB intensities for plane #%d [%4.2f,%4.2f,%4.2f]" ,
                         plane ,		/* the plane number */
                         colours[n][0] ,	/* red colour */
                         colours[n][1] ,	/* blue colour */
                         colours[n][2] );	/* green colour */
         (void) userreal_c( colours[n] ,	/* user defined colours */
                            &nitems ,		/* the number of intensities */
                            &input_level ,	/* the input level */
                            tofchar( keyword ) ,	/* the keyword */
                            tofchar( message ) );	/* the message */
         display_stat = gdi_grcol_c( &display_id ,
                                     &plane ,
                                     &colours[n][0] ,
                                     &colours[n][1] ,
                                     &colours[n][2] );
      }
   }
   /*
    * Now we ask the user whether he or she wants the fitted ellipse
    * plotted on the screen.
    */
   {
      fint	default_level = 2;
      fint	nitems = 1;

      (void) userlog_c( &plot, &nitems, &default_level, KEY_PLOT, MES_PLOT );
   }
   /*
    * We allow here the user to define the search area.
    * First we turn the graphics planes on, then let the user define the
    * search area, then turn the search area plane off. The graphics data
    * is then read from the display.
    */
   {
      fint	output_level = 9;		/* to screen, novice users */
      fint	mask1 = 1;			/* turn on first plane */
      fint	mask2 = 14;			/* only 2, 3 and 4 are on */
      fint	plane = 1;			/* plane number */

      display_stat = gdi_gron_c( &display_id, &mask1 );
      status_c( tofchar( "Define the ellipse search area" ) );
      anyout_c( &output_level , tofchar( "Press Ready when ready" ) );
      anyout_c( &output_level , tofchar( "Goto the Etcetera menu in GIDS and press Region!" ) );
      anyout_c( &output_level , tofchar( "Press Define to start region definition with mouse" ) );
      anyout_c( &output_level , tofchar( "Left   mouse button = Draw polygon" ) );
      anyout_c( &output_level , tofchar( "Middle mouse button = Remove polygon" ) );
      anyout_c( &output_level , tofchar( "Right  mouse button = Close polygon" ) );
      anyout_c( &output_level , tofchar( "Press Ready when ready" ) );
      display_stat = gdi_grregion_c( &display_id , &plane );
      display_stat = gdi_grread_c( &display_id, (fint *) gdata, &ndata, &pack );
      display_stat = gdi_gron_c( &display_id, &mask2 );
   }
   /*
    * Now we start the loop. The user is asked each run to enter a
    * range of values in between the coordinates of the ellipse are
    * defined. Next we show the selected coordinates.
    */
   do {
      fint	error_level;			/* level of error */
      fint	input_level;			/* level of input */
      fint	n;				/* counter */
      fint	nitems;				/* # of items */
      fint	npoints = 0;			/* # of points on ellipse */
      fint	mask[MAXPARS];			/* mask fixed/free */
      float	epar[MAXPARS];			/* errors in parameters */
      float	fpar[MAXPARS];			/* ellipse parameters */
      float	range[2];			/* data range defines ellipse */
      float	v[MAXPOINTS];			/* data values */
      float	x[MAXPOINTS];			/* x coordinates */
      float	y[MAXPOINTS];			/* y coordinates */

      do {
         input_level = 4;			/* exact number of values */
         nitems = 2;				/* number of values */
         (void) userreal_c( range ,		/* the values */
                            &nitems ,		/* number of values */
                            &input_level ,	/* leve of input */
                            KEY_RANGE ,		/* the keyword */
                            MES_RANGE );	/* the message */
         cancel_c( KEY_RANGE );			/* cancel keyword */
         if (range[0] > range[1]) {		/* wrong order */
            float	temp = range[0];	/* temporary */

            range[0] = range[1]; range[1] = temp;	/* correct range */
         }
         for (npoints = n = 0; n < ndata; n++) {/* loop defines ellipse */
            if (idata[n] != blank && idata[n] > range[0] && idata[n] < range[1] && gdata[n] & 1) {
               gdata[n] = 3;			/* selected */
               npoints++;			/* increase # of points */
            } else {
               gdata[n] &= 1;			/* cleared */
            }
         }
         if (!npoints) {
            fint	error_level = 1;

            error_c( &error_level, tofchar( "No points in specified range!" ) );
         }
      } while (!npoints);			/* until some points found */
      {
         char	message[MAXMESLEN];
         fint	output_level = 3;

         sprintf( message, "Found %d points on ellipse", npoints );
         anyout_c( &output_level, tofchar( message ) );
         if (npoints > MAXPOINTS) {		/* too many points */
            fint	error_level = 1;

            sprintf( message, "Too many points, maximum is %d", MAXPOINTS );
            error_c( &error_level, tofchar( message ) );
         }
      }
      display_stat = gdi_grwrite_c( &display_id, (fint *)gdata, &ndata, &pack );
      if (npoints > MAXPARS) {			/* enough points */
         bool	edit = TRUE;			/* edit selected points */
         int	mes = 0;			/* message bool */

         input_level = 1;			/* default allowed */
         nitems = 1;				/* number of items */
         (void) userlog_c( &edit ,		/* the value */
                           &nitems ,		/* # of items */
                           &input_level ,	/* level of input */
                           KEY_EDIT ,		/* the keyword */
                           MES_EDIT );		/* the message */
         cancel_c( KEY_EDIT );			/* cancel keyword */
         if (tobool(edit)) {			/* edit graphics plane #3 */
            fint	output_level = 9;	/* to screen, novice users */
            fint	plane = 4;		/* the plane number */
            static fint	first = 0;		/* first time */

            status_c( tofchar( "Edit the selected points" ) );
            if (!first) {
               anyout_c( &output_level , tofchar( "Goto the Etcetera menu in GIDS and press Region!" ) );
               anyout_c( &output_level , tofchar( "Press Define to start region definition with mouse" ) );
               anyout_c( &output_level , tofchar( "Left   mouse button = Draw polygon" ) );
               anyout_c( &output_level , tofchar( "Middle mouse button = Remove polygon" ) );
               anyout_c( &output_level , tofchar( "Right  mouse button = Close polygon" ) );
               anyout_c( &output_level , tofchar( "--> Points inside a region are NOT used for the fit <--" ) );
               first = 1;
            }
            display_stat = gdi_grregion_c( &display_id , &plane );
            display_stat = gdi_grread_c( &display_id, (fint *)gdata, &ndata, &pack );
         }
         for (npoints = n = 0; n < ndata; n++) {/* loop defines ellipse */
            if ((gdata[n] & 7) == 3) {		/* point on ellipse */
               if (npoints == MAXPOINTS) {	/* too many points */
                  gdata[n] &= 1;		/* mark */
                  if (!mes) {			/* put out message */
                     error_level = 1;		/* warning message */
                     error_c( &error_level ,	/* the error level */
                              tofchar( "Too many points on ellipse!" ) );
                     mes = 1;
                  }
               } else {
                  v[npoints] = idata[n];		/* get data value */
                  x[npoints] = ((n%ilen) + blo[0]) * cdelt[0];
                  y[npoints] = ((n/ilen) + blo[1]) * cdelt[1];
                  npoints += 1;			/* increase # of points */
               }
            } else {
               gdata[n] &= 1;
            }
         }
      }
      display_stat = gdi_grwrite_c( &display_id, (fint *)gdata, &ndata, &pack );
      {
         char	message[MAXMESLEN];
         fint	output_level = 3;

         sprintf( message, "Found %d points on ellipse", npoints );
         anyout_c( &output_level, tofchar( message ) );
      }
      if (npoints > MAXPARS) {			/* enough points */
         char	keyword[MAXKEYLEN];		/* buffer for keyword */
         char	message[MAXMESLEN];		/* buffer for message */
         fint	k;				/* counter */
         fint	r;				/* return value */

         /*
          * Automatic determination of initial estimates?
          */
         {
            fint	input_level = 2;
            fint	nitems = 1;

            (void) userlog_c( &aut, &nitems, &input_level, KEY_AUTO, MES_AUTO );
            cancel_c( KEY_AUTO );
         }
         if (tobool(aut)) {
            r = ellipse1_c( &npoints, x, y, fpar );
         } else {
            fint	input_level = 4;
            fint	nitems = 5;

            r = 0;
            (void) userreal_c( fpar, &nitems, &input_level, KEY_INIPAR, MES_INIPAR );
            cancel_c( KEY_INIPAR );
            fpar[2] *= cdelt[0];		/* to position */
            fpar[3] *= cdelt[1];		/* to position */
            fpar[4] += mapphi;			/* convert to map p.a. */
         }
         if (!r) {
            fint	output_level = 3;

            fpar[2] /= cdelt[0];		/* convert to pixel */
            fpar[3] /= cdelt[1];		/* convert to pixel */
            fpar[4] -= mapphi;			/* convert to sky p.a. */
            anyout_c( &output_level, tofchar( "Initial Estimates of Ellipse" ) );
            anyout_c( &output_level, tofchar( " " ) );
            (void) sprintf( message ,
                            "Semi Major axis: %10.4f %s" , fpar[0], sky );
            anyout_c( &output_level, tofchar( message ) );
            (void) sprintf( message ,
                            "Inclination    : %10.4f degrees" , fpar[1] );
            anyout_c( &output_level, tofchar( message ) );
            (void) sprintf( message ,
                            "X centre       : %10.4f pixels" , fpar[2] );
            anyout_c( &output_level, tofchar( message ) );
            (void) sprintf( message ,
                            "Y centre       : %10.4f pixels" , fpar[3] );
            anyout_c( &output_level, tofchar( message ) );
            (void) sprintf( message ,
                            "Position angle : %10.4f degrees" , fpar[4] );
            anyout_c( &output_level, tofchar( message ) );
         }
         for (k = 0; k < MAXPARS; k++) {
            input_level = 2;			/* default allowed */
            nitems = 1;				/* only one item */
            strcpy( keyword, parkey[k] );	/* get keyword */
            strcpy( message, parmes[k] );	/* get message */
            if (userreal_c( &fpar[k] ,		/* the value */
                            &nitems ,		/* # of values */
                            &input_level ,	/* level of input */
                            tofchar( keyword ) ,
                            tofchar( message ) )) {
               mask[k] = 0;			/* fixed parameter */
            } else {
               mask[k] = 1;			/* free parameter */
            }
         }
         fpar[2] *= cdelt[0];			/* convert to arcsec */
         fpar[3] *= cdelt[1];			/* convert to arcsec */
         fpar[4] += mapphi;			/* convert to map p.a. */
         r = ellipse2_c( &npoints, x, y, fpar, epar, mask );
         fpar[2] /= cdelt[0];			/* convert to pixels */
         fpar[3] /= cdelt[1];			/* convert to pixels */
         fpar[4] -= mapphi;			/* convert to sky p.a. */
         epar[2] /= cdelt[0];			/* convert to pixels */
         epar[3] /= cdelt[1];			/* convert to pixels */
         if (r > 0) {
            char	message[TEXTLEN];	/* message buffer */
            fint	nblank = 0;
            fint	ntot = 0;
            fint	output_level = 3;	/* to screen and logfile */
            float	amax;
            float	amin;
            float	mean;
            float	rms;

            statr_c( v, &npoints, &amin, &amax, &mean, &rms, &nblank, &ntot );
            anyout_c( &output_level, tofchar( " " ) );
            anyout_c( &output_level, tofchar( "Results from ELLipse FIT" ) );
            anyout_c( &output_level, tofchar( " " ) );
            (void) sprintf( message ,
                            "Semi Major axis: %10.4f (%10.4f) %s" ,
                            fpar[0], epar[0], sky );
            anyout_c( &output_level, tofchar( message ) );
            (void) sprintf( message ,
                            "Inclination    : %10.4f (%10.4f) degrees" ,
                            fpar[1], epar[1] );
            anyout_c( &output_level, tofchar( message ) );
            (void) sprintf( message ,
                            "X centre       : %10.4f (%10.4f) pixels" ,
                            fpar[2], epar[2] );
            anyout_c( &output_level, tofchar( message ) );
            (void) sprintf( message ,
                            "Y centre       : %10.4f (%10.4f) pixels" ,
                            fpar[3], epar[3] );
            anyout_c( &output_level, tofchar( message ) );
            (void) sprintf( message ,
                            "Position angle : %10.4f (%10.4f) degrees" ,
                            fpar[4], epar[4] );
            anyout_c( &output_level, tofchar( message ) );
            (void) sprintf( message ,
                            "Map level      : %10.4f (%10.4f) %.*s" ,
                            mean, rms, nelc_c( bunit ), bunit.a );
            anyout_c( &output_level, tofchar( message ) );
            /*
             * Next we plot the fitted ellipse. This is done by scanning the
             * whole map in x and y direction for crossing points of the
             * fitted ellipse.
             */
            if (tobool(plot)) {
               double	cosp, sinp, cosi, sini;
               double	a, b, c, d, e;
               fint	i, j;

               status_c( tofchar( "Drawing the fitted ellipse" ) );
               cosp = cos( F * ( fpar[4] + mapphi ) );
               sinp = sin( F * ( fpar[4] + mapphi ) );
               cosi = cos( F * fpar[1] );
               sini = sin( F * fpar[1] );
               a = cosi * cosi * sinp * sinp + cosp * cosp;
               b = sini * sini * sinp * cosp;
               c = cosi * cosi * cosp * cosp + sinp * sinp;
               d = cosi * cosi * fpar[0] * fpar[0];
               e = b * b - a * c;
               /* for each i, seek for two j's which cross the ellipse */
               for ( i = 0; i < ilen; i++ ) {
                  double	q, x, ym, yd;

                  x = ( blo[0] + i - fpar[2] ) * cdelt[0];
                  q = d * c + e * x * x;
                  if ( q >= 0.0 ) {
                     yd = sqrt( q ) / c;
                     ym = - b / c * x;
                     j = nint( ( ym - yd ) / cdelt[1] + fpar[3] ) - blo[1];
                     if ( j > -1 && j < jlen ) {
                        gdata[j * ilen + i] |= 8;
                     }
                     j = nint( ( ym + yd ) / cdelt[1] + fpar[3] ) - blo[1];
                     if ( j > -1 && j < jlen ) {
                        gdata[j * ilen + i] |= 8;
                     }
                  }
               }
               /* for each j, seek for two i's which cross the ellipse */
               for ( j = 0; j < jlen; j++ ) {
                  double	q, y, xm, xd;

                  y = ( blo[1] + j - fpar[3] ) * cdelt[1];
                  q = d * a + e * y * y;
                  if ( q >= 0.0 ) {
                     xd = sqrt( q ) / a;
                     xm = - b / a * y;
                     i = nint( ( xm - xd ) / cdelt[0] + fpar[2] ) - blo[0];
                     if ( i > -1 && i < ilen ) {
                        gdata[j * ilen + i] |= 8;
                     }
                     i = nint( ( xm + xd ) / cdelt[0] + fpar[2] ) - blo[0];
                     if ( i > -1 && i < ilen ) {
                        gdata[j * ilen + i] |= 8;
                     }
                  }
               }
               display_stat = gdi_grwrite_c( &display_id ,
                                             (fint *)gdata ,
                                             &ndata ,
                                             &pack );
            }
            if (nellipse == MAXELLIPSE) {
               char	message[MAXMESLEN];
               fint	error_level = 1;

               sprintf( message, "Table full, max table size is %d", MAXELLIPSE );
               error_c( &error_level, tofchar( message ) );
            } else {
               np[nellipse] = npoints;
               ie[nellipse] = epar[1];
               iv[nellipse] = fpar[1];
               pe[nellipse] = epar[4];
               pv[nellipse] = fpar[4];
               re[nellipse] = epar[0];
               rv[nellipse] = fpar[0];
               ve[nellipse] = rms;
               vv[nellipse] = mean;
               xe[nellipse] = epar[2];
               xv[nellipse] = fpar[2];
               ye[nellipse] = epar[3];
               yv[nellipse] = fpar[3];
               nellipse += 1;			/* add one */
            }
         }
      } else {					/* not enough points */
         error_level = 1;			/* warning message */
         error_c( &error_level ,		/* level of error */
                  tofchar( "Not enough points on ellipse!" ) ); 
      }
      input_level = 1;				/* default allowed */
      nitems = 1;				/* only one item */
      (void) userlog_c( &cont ,			/* the value */
                        &nitems ,		/* number of values */
                        &input_level ,		/* level of input */
                        KEY_CONTINUE ,		/* the keyword */
                        MES_CONTINUE );		/* the message */
      cancel_c( KEY_CONTINUE );			/* cancel keyword */
   } while (tobool(cont));			/* until user wants to quit */
   /*
    * Now we show the final results. Sorted in order of increasing semi major
    * axis radius.
    */
   if (nellipse) {
      FILE	*f = NULL;
      char	fileb[TEXTLEN];
      char	message[MAXMESLEN];
      fchar	file;
      fint	indx[MAXELLIPSE];
      fint	input_level = 1;
      fint	n, nc;
      fint	output_level = 3;

      /*
       * sort the parameters.
       */
      rankra_c( rv, indx, &nellipse );
      for (n = 0; n < nellipse; n++) {
         int	k = indx[n];

         if (k > n ) {
            float	fs;
            fint	ff;

            fs = ie[n]; ie[n] = ie[k]; ie[k] = fs;
            fs = iv[n]; iv[n] = iv[k]; iv[k] = fs;
            fs = pe[n]; pe[n] = pe[k]; pe[k] = fs;
            fs = pv[n]; pv[n] = pv[k]; pv[k] = fs;
            fs = re[n]; re[n] = re[k]; re[k] = fs;
            fs = rv[n]; rv[n] = rv[k]; rv[k] = fs;
            fs = ve[n]; ve[n] = ve[k]; ve[k] = fs;
            fs = vv[n]; vv[n] = vv[k]; vv[k] = fs;
            fs = xe[n]; xe[n] = xe[k]; xe[k] = fs;
            fs = xv[n]; xv[n] = xv[k]; xv[k] = fs;
            fs = ye[n]; ye[n] = ye[k]; ye[k] = fs;
            fs = yv[n]; yv[n] = yv[k]; yv[k] = fs;
            ff = np[n]; np[n] = np[k]; np[k] = ff;
         }
      }
      /*
       * Now save the parameters in a table.
       */
      {
         char	ctypeb[MAXVARLEN];
         char	ccommb[MAXVARLEN];
         char	cuntsb[MAXVARLEN];
         fchar	ctype;
         fchar	ccomm;
         fchar	cunts;
         fint	item = 0;
         fint	nitems = nellipse;
         fint	nrows;
         fint	terror = 0;				/* error return */
         int	create;					/* create table */

         finit( ctype, ctypeb );
         finit( ccomm, ccommb );
         finit( cunts, cuntsb );
         gdsa_colinq_c( set, &subset, TABLE_NAME, tofchar( "RADII" ),
            ctype, ccomm, cunts, &nrows, &terror );
         if (terror == -7) create = 1; else create = 0;
         terror = 0;
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "RADII" ),
               tofchar( "REAL" ), tofchar( "Semi Major axis of ellipses" ),
               tofchar( sky ), &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "RADII" ),
            rv, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "ERADII" ),
               tofchar( "REAL" ), tofchar( "Error in Semi Major axis" ),
               tofchar( sky ), &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "ERADII" ),
            re, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "INCL" ),
               tofchar( "REAL" ), tofchar( "Inclination of ellipses" ),
               tofchar( "DEGREES" ), &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "INCL" ),
            iv, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "EINCL" ),
               tofchar( "REAL" ), tofchar( "Error in Inclination" ),
               tofchar( "DEGREES" ), &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "EINCL" ),
            ie, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "XPOS" ),
               tofchar( "REAL" ), tofchar( "X-position centre" ),
               tofchar( "GRIDS" ), &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "XPOS" ),
            xv, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "EXPOS" ),
               tofchar( "REAL" ), tofchar( "Error in X-position" ),
               tofchar( "GRIDS" ), &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "EXPOS" ),
            xe, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "YPOS" ),
               tofchar( "REAL" ), tofchar( "Y-position centre" ),
               tofchar( "GRIDS" ), &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "YPOS" ),
            yv, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "EYPOS" ),
               tofchar( "REAL" ), tofchar( "Error in Y-position" ),
               tofchar( "GRIDS" ), &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "EYPOS" ),
            ye, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "PA" ),
               tofchar( "REAL" ), tofchar( "Position Angle" ),
               tofchar( "GRIDS" ), &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "PA" ),
            pv, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "EPA" ),
               tofchar( "REAL" ), tofchar( "Error in Position Angle" ),
               tofchar( "GRIDS" ), &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "EPA" ),
            pe, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "VALUE" ),
               tofchar( "REAL" ), tofchar( "Average Map Value" ),
               bunit, &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "VALUE" ),
            vv, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "EVALUE" ),
               tofchar( "REAL" ), tofchar( "Error in Map Value" ),
               bunit, &terror );
         }
         gdsa_wcreal_c( set, &subset, TABLE_NAME, tofchar( "EVALUE" ),
            ve, &item, &nitems, &terror );
         if (create) {
            gdsa_crecol_c( set, &subset, TABLE_NAME, tofchar( "NPTS" ),
               tofchar( "INT" ), tofchar( "Points on ellipse" ),
               tofchar( " " ), &terror );
         }
         gdsa_wcint_c( set, &subset, TABLE_NAME, tofchar( "NPTS" ),
            np, &item, &nitems, &terror );
      }
      {
         char	message[MAXMESLEN];
         fint	output_level = 0;

         sprintf( message, "Added %d rows to table %s", nellipse, PROGRAM );
         anyout_c( &output_level, tofchar( message ) );
      }
      finit( file, fileb );
      nc = usertext_c( file, &input_level, KEY_FILE, MES_FILE );
      if (!nc) {
         strcpy( fileb, "ellfit.dat" );
      } else {
         fileb[nc] = 0;
      }
      f = fopen( fileb, "a" );
      anyout_c( &output_level, tofchar( "      Radius  Inclination     X-centre     Y-centre           PA        Value" ) );
      sprintf( message, "(%10s) (   degrees) (    pixels) (    pixels) (   degrees) (%10.*s)", sky, (int) nelc_c( bunit ), bunit.a );
      anyout_c( &output_level, tofchar( message ) );
      if (f != NULL) {
         fprintf( f, "!      Radius        Error  Inclination        Error     X-centre        Error     Y-centre        Error           PA        Error        Value        Sigma    Points\n" );
         fprintf( f, "!(%10s) (%10s) (   degrees) (   degrees) (    pixels) (    pixels) (    pixels) (    pixels) (   degrees) (   degrees) (%10.*s) (%10.*s)\n", sky, sky, (int) nelc_c( bunit ), bunit.a, (int) nelc_c( bunit ), bunit.a );
      }
      for (n = 0; n < nellipse; n++) {
         sprintf( message, "%12.2f %12.2f %12.2f %12.2f %12.2f %12.2f", rv[n], iv[n], xv[n], yv[n], pv[n], vv[n] );
         anyout_c( &output_level, tofchar( message ) );
         sprintf( message, "(%10.2f) (%10.2f) (%10.2f) (%10.2f) (%10.2f) (%10g)", re[n], ie[n], xe[n], ye[n], pe[n], ve[n] );
         anyout_c( &output_level, tofchar( message ) );
         if (f != NULL) {
            fprintf( f, " %12.2f %12.2f %12.2f %12.2f %12.2f %12.2f %12.2f %12.2f %12.2f %12.2f %12.2f %12g %5d\n", rv[n], re[n], iv[n], ie[n], xv[n], xe[n], yv[n], ye[n], pv[n], pe[n], vv[n], ve[n], np[n] );
         }
      }
      if (f != NULL) fclose( f );		/* close file */
   }
   display_stat = gdi_close_c( &display_id );	/* close display */
   finis_c( );					/* goodbye HERMES, and thanks */
   return( EXIT_SUCCESS );			/* exit with status */
}
