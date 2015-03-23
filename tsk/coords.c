/* coords.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            coords.dc1

Program:      COORDS

Purpose:      Converts user supplied coordinates into physical coordinates.

Category:     COORDINATES

File:         coords.c

Author:       K.G. Begeman

Keywords:

   INSET=     Set and subset(s).
              Maximum number of subsets is 2048.

   POS=       Give position (any coordinates)       [programme quits]
              This keyword is repeated until default is used.

              To generate positions in different sky system/epoch:

** SKYSYS=    Give new sky coordinate system:            [old system]

** EPOCH=     Give epoch of new sky system:               [old epoch]
              Only asked if an equatorial/ecliptical system is wanted.
              The epoch is given in years e.g. EPOCH=1983.5

** FORMAT=    Format spatial positions (deg -> hms/dms)?          [Y]

              FORMAT


Description:  The output of physical coordinates can be extended with the
              output of coordinates in a different epoch or sky system.
              The sky system is specified with SKYSYS= and an integer
              number. The following sky systems are implemented:

              SKYSYS  CTYPEm  CTYPEn          Meaning
              ===================================================
                1      RA      DEC      Equatorial (EPOCH 1950.0)
                2      GLON    GLAT     Galactic
                3      ELON    ELAT     Ecliptic (EPOCH 1950.0)
                4      SLON    SLAT     Super galactic
                5      RA      DEC      Equatorial (EPOCH 2000.0)

              An epoch transformation is initiated with EPOCH= followed
              by an epoch in years. If, for example, the input set has
              an equatorial system and you want position 0,0 also in
              ecliptical coordinates with epoch 1983.5, use:
              POS=0 0  SKYSYS=3  EPOCH=1983.5



Example:      <USER> COORDS
              <USER> COORDS INSET=NGC4214 FREQ 29:34
              Set NGC4214 has 3 axes
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              FREQ-OHEL          from     1 to    63
              <USER> COORDS POS=0 0
                RA-NCP               DEC-NCP              FREQ-OHEL
               (DEGREE            ) (DEGREE            ) (KM/S              )
                 183.125 (    0.00)   36.6000 (    0.00)   279.153 (   29.00)
                 183.125 (    0.00)   36.6000 (    0.00)   287.414 (   30.00)
                 183.125 (    0.00)   36.6000 (    0.00)   295.675 (   31.00)
                 183.125 (    0.00)   36.6000 (    0.00)   303.936 (   32.00)
                 183.125 (    0.00)   36.6000 (    0.00)   312.198 (   33.00)
                 183.125 (    0.00)   36.6000 (    0.00)   320.461 (   34.00)
              <USER> COORDS POS=
              <STATUS>  COORDS   +++ FINISHED +++

Updates:      Sep 19, 1991: KGB, document created.
              May 14, 1992: VOG, Convert degrees to hms/dms
              Feb 24, 1993: VOG, Conversion to new sky system/epoch
              Jul 12, 1994: VOG, FORMAT= included

#<
*/

/*
 * Includes:
 */

#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY symbols & definitions */
#include	"cmain.h"		/* Main C programme */
#include        "math.h"
#include	"userfio.h"		/* defines anyout_c etc. */
#include	"axunit.h"		/* defines axunit_c */
#include        "axtype.h"              /* defines axtype_c */
#include	"cancel.h"		/* defines cancel_c */
#include	"cotrans.h"		/* defines cotrans_c */
#include	"finis.h"		/* defines finis_c */
#include	"gdsc_grid.h"		/* defines gdsc_grid_c */
#include	"gdsc_name.h"		/* defines gdsc_name_c */
#include	"gdsc_ndims.h"		/* defines gdsc_ndims_c */
#include	"gdsd_rdble.h"		/* defines gdsd_rdble_c */
#include	"gdsinp.h"		/* defines gdsinp_c */
#include	"gdspos.h"		/* defines gdspos_c */
#include	"init.h"		/* defines init_c */
#include        "nelc.h"                /* defines nelc_c */
#include        "setdblank.h"
#include        "epoco.h"
#include        "skyco.h"
#include        "eclipco.h"
#include        "userint.h"
#include        "userdble.h"
#include        "userlog.h"


/*
 * Defines:
 */

#define	CLASS		1		/* class of programme */
#define	CLASSDIM	0		/* class dimension */
#define	KEY_INSET	tofchar("INSET=")
#define	KEY_POS		tofchar("POS=")
#define	MAXAXES		10		/* max. number of axes */
#define	MAXFITSCHAR	24		/* max. length of FITS character */
#define	MAXSETNAMLEN	80		/* max. length of set name */
#define STRLEN          80
#define	MAXSTRINGLEN	1024		/* max. length of character strings */
#define	MAXSUBSET	2048		/* max. number of subsets */
#define	MES_INSET	tofchar("Input set and subset(s)")
#define	MES_POS		tofchar("Enter position [programme quits]")
#define	VERSION		"1.1"		/* change version number here */
#define EQUATORIAL      1
#define GALACTIC        2
#define ECLIPTIC        3
#define SUPERGALACTIC   4
#define HIDDEN          2

/* Initialize Fortran compatible string with macro 'fmake' */

#define fmake(fchr,size) { \
                           static char buff[size+1]; \
                           int i; \
                           for (i = 0; i < size; buff[i++] = ' '); \
                           buff[i] = 0; \
                           fchr.a = buff; \
                           fchr.l = size; \
                         }




/*
 * Variables for input set:
 */

static	char	ctypeb[MAXAXES][MAXFITSCHAR];	/* buffer for axis types */
static	char	cunitb[MAXAXES][MAXFITSCHAR];	/* buffer for axis units */
static  double  crval[2], cdelt[2];
static  double  crota;
static  char    nunitb[MAXFITSCHAR];
static  char    dunitb[MAXFITSCHAR];
static  char    isetb[MAXSETNAMLEN];            /* buffer for set name */
static	fchar	ctype[MAXAXES];			/* axis names */
static	fchar	cunit[MAXAXES];			/* axes units */
static  fchar   nunit;                          /* Natural units */
static  fchar   dunit;                          /* Secondary units */
static  fchar   iset = { isetb, MAXSETNAMLEN }; /* input set name */
static  fint    isubset[MAXSUBSET];             /* input subsets */
static  fint    iperm[MAXAXES];                 /* axes permutation */
static  fint    isize[MAXAXES];                 /* size of axes */
static  fint    insub;                          /* number of input subsets */
static	fint	isetdim;			/* dimension of set */
static	fint	isubdim;			/* dimension of subset */
static  fint    prosys, velsys;
static  fint    skysys[MAXAXES];                /* Sky system */
static  fint    axistype[MAXAXES];
static  fint    prec = 1;                       /* Precision in hms/dms functions */
static  char    txt[80];
static  double  dummyD[3];
static  fchar   key, mes;
static  fint    sky, newsky;
static  double  epoch, newepoch;
static  int     epotrans, skytrans;
static  fint    setlevel = 0;
static  double  posang = 0.0;
static  bool    format;


static void  axinfo( int typenum, int skynum, int pronum, int velnum,
                     char *typestr, char *skystr, char *prostr, char *velstr )
/*----------------------------------------------------------------------*/
/* What kind of axis is this? The 'typenum' corresponds to a text       */
/*----------------------------------------------------------------------*/
{
   typestr[0] = skystr[0] = prostr[0] = velstr[0] = '\0';
   switch ( (int) typenum ) {
      case 0:
         strcpy( typestr, "unknown type" );
         break;
      case 1:
         strcpy( typestr, "spatial axis longitude" );
         break;
      case 2:
         strcpy( typestr, "spatial axis latitude" );
         break;
      case 3:
         strcpy( typestr, "spectral axis frequency" );
         break;
      case 4:
         strcpy( typestr, "spectral axis velocity" );
         break;
      case 5:
         strcpy( typestr, "spectral axis wavelength" );
         break;
      case 6:
         strcpy( typestr, "spectral axis inverse wavelength" );
         break;
      case 7:
         strcpy( typestr, "spectral axis log(wavelength)" );
         break;
      case 8:
         strcpy( typestr, "time axis" );
         break;
      case 9:
         strcpy( typestr, "polarisation axis" );
         break;
      case 10:
         strcpy( typestr, "parameter axis" );
         break;
      case 11:
         strcpy( typestr, "sample axis of iras data" );
         break;
      case 12:
         strcpy( typestr, "tick axis of iras data" );
         break;
      case 13:
         strcpy( typestr, "detector axis of iras data" );
         break;
      case 14:
         strcpy( typestr, "snip axis of iras data" );
         break;
   }

   if ((typenum == 1) || (typenum == 2)) {
      /* Display sky system */
      switch( (int) skynum ) {
         case 1:
            strcpy( skystr, "equatorial" );
            break;
         case 2:
            strcpy( skystr, "galactic" );
            break;
         case 3:
            strcpy( skystr, "ecliptic" );
            break;
         case 4:
            strcpy( skystr, "supergalactic" );
            break;
      }

      switch( (int) pronum ) {
         /* Projection system */
         case 1:
            strcpy( prostr, "AITOFF equal area" );
            break;
         case 2:
            strcpy( prostr, "equivalent cylindrical" );
            break;
         case 3:
            strcpy( prostr, "flat" );
            break;
         case 4:
            strcpy( prostr, "gnomonic" );
            break;
         case 5:
            strcpy( prostr, "orthographic" );
            break;
         case 6:
            strcpy( prostr, "rectangular" );
            break;
         case 7:
            strcpy( prostr, "global sinusoidal" );
            break;
         case 8:
            strcpy( prostr, "north celestial pole (WSRT)" );
            break;
         case 9:
            strcpy( prostr, "stereographic" );
            break;
         case 10:
            strcpy( prostr, "mercator projection" );
            break;
      }
   }

   if (typenum == 3) {
      /* Display frequency system */
      switch( (int) skynum ) {
         case 1:
            strcpy( velstr, "optical" );
            break;
         case 2:
            strcpy( velstr, "radio" );
            break;
      }
   }
   if (typenum == 4)  strcpy( velstr, "radio" );
}



static void dms( double degrees, int prec, char *convstr, double *DMS )
/*-----------------------------------------------------------------------------
 * Convert degrees to deg/min/sec
 *-----------------------------------------------------------------------------
 */
{
   double    seconds;
   int       Idegs;
   double    min;
   int       Imin;
   int       negative;
   double    power;


   power = pow( 10.0, (double) prec );
   negative = 0;
   if (degrees < 0)
   {
      negative = 1;
      degrees = fabs(degrees);
   }
   Idegs   = (int) degrees;
   min     = degrees*60.0 - ((double)Idegs)*60.0;
   Imin    = (int) min;
   seconds = min*60.0 - ((double)Imin*60.0 );
   /* Avoid rounding by formatting */
   seconds = (double) ((int) (seconds * power) ) / power;

   if (negative)
   {
      /* Numbers between -90 and 90 */
      (void) sprintf( convstr, "-%2dd%2dm%5.*fs", Idegs, Imin, prec, seconds );
      Idegs *= -1;
   }
   else
      (void) sprintf( convstr, "%2dd%2dm%5.*fs", Idegs, Imin, prec, seconds );

   DMS[0] = (double) Idegs;
   DMS[1] = (double) Imin;
   DMS[2] = seconds;
}


static void hms( double degrees, int prec, char *convstr, double *HMS )
/*-----------------------------------------------------------------------------
 * Convert degrees to hours/min/sec
 *-----------------------------------------------------------------------------
 */
{
   double    seconds;
   double    hours;
   int       Ihours;
   double    min;
   int       Imin;
   int       negative;
   double    power;


   power = pow( 10.0, (double) prec );
   negative = 0;
   if (degrees < 0)
   {
      negative = 1;
      degrees = -1.0 * degrees;
   }
   hours   = degrees / 15.0;
   Ihours  = (int) hours;
   min     = hours*60.0 - ((double)Ihours)*60.0;
   Imin    = (int) ( min );
   seconds = min*60.0 - ((double)Imin)*60.0;

   /* Avoid format problems in sprintf */
   seconds = (double) ((int) (seconds * power) ) / power;
   if (negative)
   {
      (void) sprintf( convstr, "-%2dh%2dm%5.*fs", Ihours, Imin, prec, seconds );
      Ihours *= -1.0;
   }
   else
      (void) sprintf( convstr, "%2dh%2dm%5.*fs", Ihours, Imin, prec, seconds );

   HMS[0] = (double) Ihours;
   HMS[1] = (double) Imin;
   HMS[2] = seconds;
}






static void cotranserror( char *txt, fint Error )
/*-----------------------------------------------------------*/
/* Give more information about reason that 'cotrans' failed. */
/*-----------------------------------------------------------*/
{
   switch ( (int) Error ) {
      case 1:
         strcpy( txt, "unknown projection" );
         break;
      case 2:
         strcpy( txt, "unknown COTRANS mode" );
         break;
      case 3:
         strcpy( txt, "CROTA2 = 90.0 for COTRANS mode 1 and 2" );
         break;
      case 4:
         strcpy( txt, "CDELT1 and/or CDELT2 equal to zero" );
         break;
      case 5:
         strcpy( txt, "input sky system unknown" );
         break;
      case 6:
         strcpy( txt, "output sky system unknown" );
         break;
      case 7:
         strcpy( txt, "input and output sky system unknown" );
         break;
      case 8:
         strcpy( txt, "skypro error" );
         break;
      case 9:
         strcpy( txt, "unknown velocity system" );
         break;
      case 10:
         strcpy( txt, "rest frequency less than or equal to zero" );
         break;
      case 11:
         strcpy( txt, "crval equal to zero" );
         break;
      case 12:
         strcpy( txt, "cdelt equal to zero" );
         break;
      case 13:
         strcpy( txt, "no matching axis pair found" );
         break;
      case 14:
         strcpy( txt, "incompatible sky systems" );
         break;
      case 15:
         strcpy( txt, "cannot do epoch transformations" );
         break;
   }
}






MAIN_PROGRAM_ENTRY
{
   fint	goon;					/* loop control */
   fint	n;					/* loop counter */
   fint	output_level = 3;		        /* to screen and log file */
   fint      first = 1;
   fint      r1 = 0;
   fint      dfault;
   fint      nitems;
   char      message[120];


   for (n = 0; n < MAXAXES; n++) {		/* initializing loop */
      ctype[n].a = ctypeb[n]; ctype[n].l = MAXFITSCHAR;
      cunit[n].a = cunitb[n]; cunit[n].l = MAXFITSCHAR;
   }
   nunit.a = nunitb; nunit.l = MAXFITSCHAR;
   dunit.a = dunitb; dunit.l = MAXFITSCHAR;
   init_c( );					/* open link with HERMES */
   IDENTIFICATION( "COORDS", VERSION );		/* identify thyself */
   /*
    * Get input set and subsets.
    */
    fmake( key, 20 );
    fmake( mes, 80 );
   {
      fint	class = CLASS;			/* class of programme */
      fint	classdim =  CLASSDIM;		/* required subset dimension */
      fint	input_level = 0;		/* no default */
      fint	maxaxes = MAXAXES;		/* max. number of axes */
      fint	maxsubset = MAXSUBSET;		/* max. number of subsets */
      fint	output_level = 3;		/* output to screen and log */
      fint	zero = 0;			/* just zero */

      key = KEY_INSET;
      mes = MES_INSET;
      insub = gdsinp_c( iset ,			/* input set name */
                        isubset ,		/* input subset levels */
                        &maxsubset ,		/* max. number of subsets */
                        &input_level ,		/* input level */
                        key,      		/* keyword */
                        mes,    		/* message */
                        &output_level ,		/* output level */
                        iperm ,			/* axes permutation */
                        isize ,			/* axes counter */
                        &maxaxes ,		/* max. number of axes */
                        &class ,		/* class of programme */
                        &classdim );		/* subset dimension */
      isetdim = gdsc_ndims_c( iset, &zero );	/* dimension of set */
      isubdim = classdim;			/* dimension of subset */
   }
   /*
    * Get axis names and axis units.
    */
   for (n = 0; n < isetdim; n++) {		/* loop over all axes */
      fint	axnum = n + 1;			/* sequence number of axis */
      fint	gerror = 0;			/* GDS error return */

      gdsc_name_c( ctype[n] ,			/* axis name */
                   iset ,			/* input set name */
                   &axnum ,			/* axis number */
                   &gerror );			/* GDS error return */
      axunit_c( iset ,				/* input set name */
                &axnum ,			/* axis number */
                cunit[n] );			/* axis units */
      axistype[n] = axtype_c( ctype[n],
                              nunit,
                              dunit,
                              &skysys[n],
                              &prosys,
                              &velsys );
      if (axistype[n] == 1)
      {
         (void) sprintf( message, "CRVAL%d", axnum );
         r1 = 0;
         gdsd_rdble_c( iset, tofchar(message), &setlevel, &crval[0], &r1 );
         if (r1 < 0)
         {
            crval[0] = 0.0;
            anyoutf( output_level, "No CRVAL in header, 0.0 substituted" );
         }
         (void) sprintf( message, "CDELT%d", axnum );
         r1 = 0;
         gdsd_rdble_c( iset, tofchar(message), &setlevel, &cdelt[0], &r1 );
         if (r1 < 0)
         {
            cdelt[0] = 1.0;
            anyoutf( output_level, "No CDELT in header, 1.0 substituted" );
         }
      }
      if (axistype[n] == 2)
      {
         (void) sprintf( message, "CRVAL%d", axnum );
         r1 = 0;
         gdsd_rdble_c( iset, tofchar(message), &setlevel, &crval[1], &r1 );
         if (r1 < 0)
         {
            crval[1] = 0.0;
            anyoutf( output_level, "No CRVAL in header, 0.0 substituted" );
         }
         (void) sprintf( message, "CDELT%d", axnum );
         r1 = 0;
         gdsd_rdble_c( iset, tofchar(message), &setlevel, &cdelt[1], &r1 );
         if (r1 < 0)
         {
            cdelt[1] = 1.0;
            anyoutf( output_level, "No CDELT in header, 1.0 substituted" );
         }
      }
      (void) sprintf( message, "CROTA%d", axnum );
      r1 = 0;
      gdsd_rdble_c( iset, tofchar(message), &setlevel, &crota, &r1 );
      if ( (r1 >=0) && (crota != 0.0) )
         posang = crota;



      /* display axis info */
      {
         char   velstr[STRLEN];
         char   skystr[STRLEN];
         char   prostr[STRLEN];
         char   typestr[STRLEN];
         fint   outlev = 8;

         axinfo( axistype[n],
                 skysys[n],
                 prosys,
                 velsys,
                 typestr,
                 skystr,
                 prostr,
                 velstr );
         if (skysys[n] > 0 && skysys[n] <= 4)
            sky = skysys[n];

         (void) sprintf( message, "%.*s: Type: %s",
                         nelc_c(ctype[n]), ctype[n].a, typestr );
         if (strlen(skystr))
            (void) sprintf( message, "%.*s, Sky: %s", strlen(message), message, skystr );

         if ( (skysys[n] == 1 || skysys[n] == 3) && first )
         {
            r1 = 0;
            gdsd_rdble_c( iset, tofchar("EPOCH"), &setlevel, &epoch, &r1 );
            if (r1 >= 0)
            {
               (void) sprintf( message, "%.*s, Epoch: %.1f",
                               strlen(message), message, epoch );
            }
            else
            {
               anyoutf( outlev, "Cannot find epoch in header, 1950 assumed" );
               epoch = 1950.0;
            }
            first = 0;
         }
         if (strlen(prostr))
            (void) sprintf( message, "%.*s, Proj: %s",
                            strlen(message), message, prostr );
         if (strlen(velstr))
            (void) sprintf( message, "%.*s, Vel: %s",
                            strlen(message), message, velstr );
         anyoutf( outlev, message );
      }
   }


   {
      fint   r1;
      fint   dfault = HIDDEN;
      fint   one = 1;

      format = toflog( 1 );
      r1 = userlog_c( &format, &one, &dfault, tofchar("FORMAT="),
                      tofchar("Format spatial positions (deg -> hms/dms)?  [Y]"));
      format = tobool( format );
   }



   do {						/* main loop */

      double	coord1[MAXAXES];		/* grid coordinates */
      double	coord2[MAXAXES];		/* physical coordinates */
      fint	input_level = 1;		/* default allowed */
      fint	maxpos = 1;			/* only one position */

      key = KEY_POS;
      mes = MES_POS;
      goon = gdspos_c( coord1 ,			/* the grids */
                       &maxpos ,		/* max. number of positions */
                       &input_level ,		/* level of input */
                       key,     		/* keyword */
                       mes,	        	/* the message */
                       iset ,			/* input set name */
                       &isubset[0] );		/* first input subset */
      cancel_c( KEY_POS );			/* cancel keyword */

      if (goon) {
         /* Is a transformation wanted? */
         dfault   = HIDDEN;
         nitems   = 1;
         newsky   = sky;
         skytrans = userint_c( &newsky, &nitems, &dfault, tofchar("SKYSYS="),
                               tofchar("New sky system 1..4:       [old]") );
         if (skytrans) skytrans = (newsky != sky);
         newepoch = epoch;
         epotrans = userdble_c( &newepoch, &nitems, &dfault, tofchar("EPOCH="),
                                tofchar("New epoch (years):       [old]") );
         if (epotrans) {
            epotrans = ((newepoch != epoch) &&
                        ((newsky == EQUATORIAL) || (newsky == ECLIPTIC)));
         }
      }

      if (goon)
      {	                			/* continue */
         char	buf1[MAXSTRINGLEN];		/* buffer */
         char	buf2[MAXSTRINGLEN];		/* buffer */
         char	string1[MAXSTRINGLEN];		/* string */
         char	string2[MAXSTRINGLEN];		/* string */
         fint	output_level = 3;		/* to screen and log file */

         string1[0] = string2[0] = 0;		/* empty string */
         for (n = 0; n < isetdim; n++) {
            sprintf( buf1, "  %*.*s ", MAXFITSCHAR, MAXFITSCHAR, ctype[iperm[n]-1].a );
            sprintf( buf2, " (%*.*s)", MAXFITSCHAR, MAXFITSCHAR, cunit[iperm[n]-1].a );
            strcat( string1, buf1 );
            strcat( string2, buf2 );
         }
         anyoutf( output_level, string1 );	/* first string */
         anyoutf( output_level, string2 );	/* second string */
      }
      else
      {  					/* stop */
         break;					/* exit */
      }
      for (n = 0; goon && n < insub; n++) 	/* subset loop */
      {
         char	buf[MAXSTRINGLEN];		/* buffer */
         char	string[MAXSTRINGLEN];		/* string */
         fint	dir = 1;			/* -> physical coordinates */
         fint	m;				/* loop counter */
         fint	r;				/* error return from cotrans */
         double    longlat[2];
         double    blank;


         setdblank_c( &blank );
         longlat[1] = longlat[0] = blank;
         r = cotrans_c( iset ,			/* input set name */
                        &isubset[n] ,		/* input subset level */
                        coord1 ,		/* grid coordinates */
                        coord2 ,		/* physical coordinates */
                        &dir );			/* grids -> physical */
         if (r)
         {      				/* report error */
            cotranserror( string , r );
            anyoutf( output_level,  string );
         }
         for (m = isubdim; m < isetdim; m++) 	/* outside subset */
         {
            fint	gerror = 0;		/* GDS error return */

            coord1[m] = gdsc_grid_c( iset ,	/* input set name */
                                     &iperm[m] ,/* axis number */
                                     &isubset[n] ,
                                     &gerror );	/* GDS error return */
         }

         string[0] = 0;
         /* If the axes are spatial, convert to hms/dms */
         for (m = 0; m < isetdim; m++)
         {
            fint indx;

            indx = iperm[m] - 1;
            if (axistype[indx] == 1)
               longlat[0] = coord2[indx];
            if (axistype[indx] == 2)
               longlat[1] = coord2[indx];
            if ( axistype[indx] == 1 && skysys[indx] == 1 )
            {
               /* Spatial axis longitude --> Convert to hms if FORMAT=Y */
               if (!format)
                  (void) sprintf( buf,
                                 "%-11.5f (%+.2f)",
                                  coord2[indx],
                                  coord1[m] );
               else
               {
                  prec = 2;
                  hms( coord2[indx],  /* Use output numbers only */
                       prec,
                       txt,
                       dummyD );
                  (void) sprintf( buf, "%s (%+.2f)", txt, coord1[m] );
               }
            }
            if ( (axistype[indx] == 1 && skysys[indx] != 1) ||
                 axistype[indx] == 2 )
            {
               /* Second axis is spatial axis longitude or latitude */
               /* but sky system is not equatorial --> Convert to   */
               /* dms if FORMAT=Y */

               if (!format)
                  (void) sprintf( buf,
                                 "%-11.5f (%+.2f)",
                                 coord2[indx],
                                 coord1[m] );
               else
               {
                  prec = 1;
                  dms( coord2[indx],  /* Use output numbers only */
                       prec,
                       txt,
                       dummyD );
                  (void) sprintf( buf, "%s (%+.2f)", txt, coord1[m] );
               }
               longlat[1] = coord2[indx];
            }

            if ( (axistype[indx] != 1) && (axistype[indx] != 2) )
            {
               (void) sprintf( buf, "%-*g (%+.2f)",
                               (MAXFITSCHAR)/2,
                               coord2[indx],
                               coord1[m] );
            }
            (void) sprintf( txt, " %-*.*s  ", MAXFITSCHAR, MAXFITSCHAR, buf );
            strcat( string, txt );
         }
         anyoutf( output_level,  string );

         /* Also, if wanted, the transformed coordinates are displayed */
         if (((longlat[0] != blank) && (longlat[1] != blank)) &&
              (epotrans || skytrans) )
         {
            char    skystr[STRLEN];
            char    epostr[STRLEN];
            double  outX, outY;
            double  epochdef;
            char    str1[STRLEN], str2[STRLEN];

            if (newsky == 1)
               strcpy( skystr, "equatorial" );
            else if (newsky == 2)
               strcpy( skystr, "galactic" );
            else if (newsky == 3)
               strcpy( skystr, "ecliptic" );
            else if (newsky == 4)
               strcpy( skystr, "supergalactic" );

            outX = longlat[0];
            outY = longlat[1];
            if (skytrans) {
               if ((sky == EQUATORIAL) && (epoch != 1950.0))
               {
                  epochdef = 1950.0;
                  epoco_c( &longlat[0], &longlat[1], &epoch, &outX, &outY, &epochdef );
               }
               if ((sky == ECLIPTIC) && (epoch != 1950.0))
               {
                  epochdef = 1950.0;
                  eclipco_c( &longlat[0], &longlat[1], &epoch, &outX, &outY, &epochdef );
               }
               r = skyco_c( &outX, &outY, &sky, &outX, &outY, &newsky );
               if ((sky == EQUATORIAL) && (epoch != 1950.0))
               {
                  epochdef = 1950.0;
                  epoco_c( &outX, &outY, &epochdef, &outX, &outY, &epoch );
               }
               if ((sky == ECLIPTIC) && (epoch != 1950.0))
               {
                  epochdef = 1950.0;
                  eclipco_c( &outX, &outY, &epochdef, &outX, &outY, &epoch );
               }
            }
            if (epotrans)
            {
               (void) sprintf( epostr, "%.1f", newepoch );
               if (newsky == EQUATORIAL)
                  epoco_c( &outX, &outY, &epoch, &outX, &outY, &newepoch );
               if (newsky == ECLIPTIC)
                  eclipco_c( &outX, &outY, &epoch, &outX, &outY, &newepoch );
            }
            if (newsky == EQUATORIAL)
            {
               hms( outX, 2, str1, dummyD );
               dms( outY, 1, str2, dummyD );
            }
            else
            {
               dms(outX, 1, str1, dummyD );
               dms(outY, 1, str2, dummyD );
            }
            (void) sprintf( message, "LONG, LAT %s", skystr );
            if (epotrans)
            {
               (void) sprintf( message, "%.*s (%s)", strlen(message), message,
                               epostr );
            }
            (void) sprintf( message, "%.*s:  %s (=%g),  %s (=%g)", strlen(message), message,
                            str1, outX, str2, outY );
            anyoutf( output_level,  message );
         }
      }
   } while( goon );				/* for ever and ever */
   finis_c( );					/* close link with HERMES */
   return( EXIT_SUCCESS );			/* return with status */
}
