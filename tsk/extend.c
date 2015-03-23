/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             extend.dc1

Program:       EXTEND

Purpose:       Copy set and extend new set with one or more axes.
               Change size of set.

Category:      MANIPULATION, UTILITY, HEADER

File:          extend.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 8192.

   BOX=        Give box in ...., ...,                     [entire subset]
               This box that is given here will be the size of the
               output set.

   OUTSET=     Give output set (, subsets):
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets.

   CTYPE1=     Enter axis name of first axis            [stop axis input]
               For the n_th axis the keyword will be CTYPEn=, like
               all the following keywords

   NAXIS1=     Size of axis in grids.

   CRPIX1=     Reference pixel of the axis.

   CUNIT1=     Physical units of axis                  [depends on CTYPE]

   CRVAL1=     Value at reference pixel in CUNITs.

   CDELT1=     Grid separation in CUNITs.

   CROTA1=     Rotation angle of axis in degrees                    [0.0]
               This keyword is only asked in case of a DEC axis.

   DUNIT1=     Secondary units of axis                 [depends on CTYPE]
               This keyword is only asked in case of a FREQ axis.

   DRVAL1=     Secondary reference value in DUNITs.
               This keyword is only asked in case of a FREQ axis.

   FREQ0=      Give rest frequency in Hz.
               This keyword is only asked in case of a FREQ axis.



Description:   Sometimes it is convenient to extend a set with an
               extra axis. For instance, a couple of 2-dim. IRAS maps
               could be bundled into one set. With this program you can
               copy the first set to a new set and extend this set with an
               extra axis (e.g. a PARAM axis). With the program COPY, you
               can copy the other sets into your created new set.

               The size of the box is important for the size of the output
               set. If the box size is smaller than the frame of the original
               set, a smaller output set will be made. Also a bigger output
               set can be made, but then, only data within the frame
               of the input will be copied.

Notes:

Example:

Updates:       Jun 4,  1993: VOG, Document created.

#<
*/

/*  extend.c: include files     */

#include    "stdio.h"        /* Defines ANSI C input and output utilities */
#include    "stdlib.h"       /* Defines the ANSI C functions for number */
                             /* conversion, storage allocation, and similar tasks.*/
#include    "string.h"       /* Declares the ANSI C string functions*/
                             /* like:strcpy, strcat etc.*/
#include    "math.h"         /* Declares the mathematical functions and macros.*/
#include    "cmain.h"        /* Defines the main body of a C program with */
                             /* MAIN_PROGRAM_ENTRY and IDENTIFICATION */
#include    "gipsyc.h"       /* Defines the ANSI-F77 types for Fortran to C intface */
                             /* including def. of char2str,str2char,tofchar,zadd */
                             /* and macros tobool and toflog */
#include    "float.h"        /* Definition of FLT_MAX etc.*/
#include    "ctype.h"        /* Declares ANSI C functions for testing characters */
                             /* like: isalpha, isdigit etc. also tolower, toupper.*/

/* Common includes */

#include    "init.h"         /* Declare task running to HERMES and initialize.*/
#include    "finis.h"        /* Informs HERMES that servant quits and cleans up the mess.*/
#include    "anyout.h"       /* General character output routine for GIPSY programs.*/
#include    "setfblank.h"    /* Subroutine to set a data value to the universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/
#include    "status.h"


/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "usercharu.h"

/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "gdsd_rint.h"
#include    "gds_extend.h"   /* Create or extend an axis */
#include    "minmax3.h"      /* Find min, max and #blanks in subset. */
#include    "wminmax.h"      /* Writes (new) minimum and maximum and number */
                             /* of blanks of subsets in the descriptor file */
                             /* and optionally deletes the MINMAX descriptors */
                             /* at intersecting levels. */

/* Output set related includes */

#include    "gdsasn.h"       /* GDSASN copies the coordinate system of a */
                             /* previously opened input set obtained with */
                             /* GDSINP to the output set to be obtained */
                             /* with GDSOUT. */
#include    "gdscss.h"       /* Change axis sizes if output subsets */
#include    "gdsout.h"       /* GDSOUT prompts the user to enter the */
                             /* name of an output set and the subsets, */
                             /* and returns the number of subsets entered. */
#include    "gdsi_write.h"   /* Writes data to (part of) an set. */


/* Extension of axes: */

#include  "usercharu.h"
#include  "userdble.h"
#include  "axtype.h"
#include  "gdsd_wdble.h"
#include  "gdsd_wchar.h"


/* DEFINITIONS: */

/* Initialize Fortran compatible string with macro 'fmake' */

#define fmake(fchr,size) { \
                           static char buff[size+1]; \
                           int i; \
                           for (i = 0; i < size; buff[i++] = ' '); \
                           buff[i] = 0; \
                           fchr.a = buff; \
                           fchr.l = size; \
                         }

/* Malloc version of 'fmake'  */
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )
#define PI             3.141592653589793
#define RAD(a)         ( a * 0.017453292519943295769237 )
#define DEG(a)         ( a * 57.295779513082320876798155 )

#define RELEASE        "1.0"           /* Version number */
#define MAXAXES        10              /* Max. axes in a set */
#define MAXSUBSETS     8192            /* Max. allowed subsets */
#define MAXBUF         4096            /* Buffer size for I/O */
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set (subset(s)): ")

/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     nsubs;              /* Number of input subsets */
static fint     dfault;             /* Default option for input etc */
static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered ccording to the */
                                    /* specification by the user. */
static fint     showdev;            /* Device number (as in ANYOUT) for info */
static fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
                                    /* number of grids along an axes as */
                                    /* specified by the user. The first elements */
                                    /* (upto the dimension of the subset) contain */
                                    /* the length of the subset axes, the other */
                                    /* ones contain the the number of grids along */
                                    /* an axes outside the subset. */
static fint     maxsubs = MAXSUBSETS;
static fint     maxaxes = MAXAXES;  /* Max num. of axes the program can deal with.*/
static fint     class = 1;          /* Class 1 is for applications which repeat */
                                    /* the operation for each subset, Class 2 */
                                    /* is for applications for which the operation */
                                    /* requires an interaction between the different */
                                    /* subsets. */
static fint     subdim;             /* Dimensionality of the subsets for class 1 applications */
static fint     setdim;             /* Dimension of set. */

/* Box and frame related */

static fint     flo[MAXAXES];       /* Low  edge of frame in grids */
static fint     fhi[MAXAXES];       /* High edge of frame in grids */
static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */
static fint     blo2[MAXAXES];
static fint     bhi2[MAXAXES];
static fint     boxopt;             /* The different options are: */
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */

/* Reading data */

static fint     cwlo, cwhi;         /* Coordinate words. */
static fint     tid;                /* Transfer id for read function. */
static fint     maxIObuf = MAXBUF;  /* Maximum size of read buffer. */
static fint     pixelsread;         /* Number of pixels read by read routine. */
static fint     pixelswrite;        /* Number of pixels to write to output. */
static float    image[MAXBUF];      /* Buffer for read routine. */
static fint     subnr;              /* Counter for subset loop. */

/* OUTSET related variables */

static fchar    Setout;
static fint     subout[MAXSUBSETS];  /* Output subset coordinate words */
static fint     nsubsout;
static fint     axnumout[MAXAXES];
static fint     axcountout[MAXAXES];
static fint     cwloO, cwhiO;        /* Output Coordinate words. */
static fint     tidO;                /* Transfer id for write function. */


/* Extension of axes */

static fchar    ctype;
static fchar    cunit, dunit, sunit, nunit;
static char     keyword[20];
static fint     nitems;
static int      quit;
static fint     nax;

/* Miscellaneous */

static fchar    Key, Mes;
static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[120];       /* All purpose character buffer. */
static int      i;                  /* Various counters. */
static bool     agreed;             /* Loop guard. */
static float    minval[MAXSUBSETS]; /* Min. value of data for each subset. */
static float    maxval[MAXSUBSETS]; /* Max. value of data for each subset. */
static fint     nblanks[MAXSUBSETS];/* Number of blanks in each subset. */
static fint     mcount;             /* Initialize MINMAX3. */
static fint     change;             /* Used in WMINMAX. change!=0 means */
                                    /* minimum and maximum have changed and */
                                    /* that the MINMAX descriptors at */
                                    /* intersecting levels will be removed. */


void anyoutC( int dev, char *anyCstr )
/*------------------------------------------------------------------*/
/* The C version of 'anyout_c' needs two parameters:                */
/* an integer and a C-type string. The integer determines           */
/* the destination of the output which is:                          */
/*    0  use default [set by HERMES to 3 but can be changed by user]*/
/*    1  terminal                                                   */
/*    2  LOG file                                                   */
/*    8  terminal, suppressed in "experienced mode"                 */
/*   16  terminal, only when in "test mode"                         */
/*------------------------------------------------------------------*/
{
   fint ldev = (fint) dev;
   anyout_c( &ldev, tofchar( anyCstr ) );
}


MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      (void) myname_c( Task );             /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );
   fmake( Setin, STRLEN );
   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   dfault  = NONE;
   subdim  = 0;
   showdev = 3;
   Key     = KEY_INSET;
   Mes     = MES_INSET;
   nsubs   = gdsinp_c( Setin,      /* Name of input set. */
                       subin,      /* Array containing subsets coordinate words. */
                       &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                       &dfault,    /* Default code as is USERxxx. */
                       Key,        /* Keyword prompt. */
                       Mes,        /* Keyword message for the user. */
                       &showdev,   /* Device number (as in ANYOUT). */
                       axnum,      /* Array of size 'maxaxes' containing the axes numbers. */
                                   /* The first elements (upto the dimension of the subset) */
                                   /* contain the axes numbers of the subset, */
                                   /* the other ones contain the axes numbers */
                                   /* outside the subset ordered according to the */
                                   /* specification by the user. */
                       axcount,    /* Number of grids on axes in 'axnum' */
                       &maxaxes,   /* Max. number of axes. */
                                   /* the operation for each subset. */
                       &class,     /* Class 1 is for applications which repeat */
                       &subdim );  /* Dimensionality of the subsets for class 1 */
   setdim  = gdsc_ndims_c( Setin, &setlevel );

   /*-------------------------------*/
   /* Determine edges of this frame */
   /*-------------------------------*/
   {
      fint cwlo, cwhi;                          /* Local coordinate words */
      int  m;
      r1 = 0;
      gdsc_range_c( Setin, &setlevel, &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++) {
         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
   }

   /*-------------------------------*/
   /* Prepare a box for OUTSET       */
   /*-------------------------------*/

   do {
      agreed = YES;
      boxopt  = 1;
      showdev = 3;
      dfault  = REQUEST;
      Key     = KEY_BOX;
      Mes     = MES_BOX;
      gdsbox_c( blo, bhi, Setin, subin, &dfault,
                Key, Mes, &showdev, &boxopt );

      /*--------------------------------------------------------------*/
      /* Assign 'gdsinp' buffer to 'gdsout'. Output set will get same */
      /* coordinate system as input INSET=.  GDSOUT is a function     */
      /* which prompts the user to enter the name of a set and        */
      /* (optionally) subset(s) and returns the number of subsets     */
      /* entered.                                                     */
      /*--------------------------------------------------------------*/

      /* the box to be copied cannot be greater than the frame */
      for (i = 0; i < (int) subdim; i++) {
         if ((blo[i] > fhi[i]) || (bhi[i] < flo[i])) {
            agreed = NO;
         } else {
            blo2[i] = MYMAX( blo[i], flo[i] );
            bhi2[i] = MYMIN( bhi[i], fhi[i] );
         }
      }
      if (!agreed) reject_c( KEY_BOX, tofchar("No data in box!") );
   } while (!agreed);
   gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
   gdscss_c( KEY_OUTSET, blo, bhi );

   dfault  = NONE;
   showdev = 3;
   Key     = KEY_OUTSET;
   Mes     = MES_OUTSET;
   fmake( Setout, STRLEN );
   do {
      nsubsout = gdsout_c( Setout,        /* Name of the output set. */
                           subout,        /* Output array with subsets coordinate words.*/
                           &nsubs,        /* Maximum number of subsets in subout. */
                           &dfault,       /* Default code as in USERxxx. */
                           Key,           /* User keyword prompt. */
                           Mes,           /* Message for the user. */
                           &showdev,      /* Device number (as in ANYOUT). */
                           axnumout,      /* Array of size 'maxaxes' containing the axes numbers. */
                           axcountout,    /* Array with the axis sizes. */
                           &maxaxes );    /* Max axes the program can deal with. */
      agreed = (nsubsout == nsubs);
      if (!agreed) (void) reject_c( KEY_OUTSET, tofchar("#out != #in") );
   } while (!agreed);


   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/


   status_c( tofchar("Copying the data") );
   for(subnr = 0; subnr < nsubs; subnr++) {
      cwlo   = gdsc_fill_c( Setin, &subin[subnr], blo2 );
      cwhi   = gdsc_fill_c( Setin, &subin[subnr], bhi2 );
      /* Use input grid coordinates, but connect to output subsets */;
      cwloO  = gdsc_fill_c( Setout, &subout[subnr], blo2 );
      cwhiO  = gdsc_fill_c( Setout, &subout[subnr], bhi2 );
      mcount = 0;
      tidO   = 0;
      tid    = 0;
      do {
         /* Read 'maxIObuf' values in 'image'. */
         (void) gdsi_read_c( Setin,
                             &cwlo, &cwhi,
                             image,
                             &maxIObuf,
                             &pixelsread,
                             &tid );
         /* Calculate running min, max & blanks of output */
         minmax3_c( image,
                    &pixelsread,
                    &minval[subnr], &maxval[subnr],
                    &nblanks[subnr],
                    &mcount );
         pixelswrite = pixelsread;
         /* Write 'pixelswrite' values from 'image' to output. */
         (void) gdsi_write_c( Setout,
                              &cwloO, &cwhiO,
                              image,
                              &maxIObuf,
                              &pixelswrite,
                              &tidO );
      } while (tid != 0);
   }
   /* Update OUTSET= descriptor with new values */
   change = YES;
   (void) wminmax_c( Setout, subout,
                     minval, maxval, nblanks,
                     &nsubsout,
                     &change );


   /*----------------------------------------------------------------*/
   /* Extend the number of axes. All necessary axis items are asked. */
   /*----------------------------------------------------------------*/

   fmake( ctype, 20 );
   fmake( cunit, 20 );
   fmake( dunit, 20 );
   fmake( sunit, 20 );
   fmake( nunit, 20 );

   gdsd_rint_c( Setin, tofchar("NAXIS"), &setlevel, &nax, &r1 );
   nax += 1;
   do {
      fint      typ = 0;
      fint      skysys, prosys, velsys;
      fint      naxis;
      fint      gerror = 0;
      fint      derror = 0;
      double    crota, crpix, crval, cdelt, drval, freq0;

      if (nax >= MAXAXES) {
         anyoutC( 3, "Too many axes, cannot create more" );
         quit = YES;
      }

      /* Get next axis from user */
      (void) sprintf( keyword, "CTYPE%d=", nax );
      nitems = 1;
      dfault = REQUEST;
      do {
         r1 = usercharu_c( ctype, &nitems, &dfault, tofchar(keyword),
                           tofchar("Give axis name:  [stop axis input]") );
         if (r1 == 0) {
            quit = YES;
         } else {
            quit = NO;
            typ = axtype_c( ctype, nunit, sunit, &skysys, &prosys, &velsys );
            if (typ == 0) {
               anyoutC( 1, "Unknown type of axis" );
               cancel_c(tofchar(keyword));
            }
         }
      } while ((typ == 0) && (!quit));
      if (!quit) {
         /* Ask other axis characteristics */
         (void) sprintf( keyword, "NAXIS%d=", nax );
         nitems = 1;
         dfault = NONE;
         r1 = userint_c( &naxis, &nitems, &dfault, tofchar(keyword),
                         tofchar("length of axis") );
         axcount[nax] = naxis;

         (void) sprintf( keyword, "CRPIX%d=", nax );
         r1 = userdble_c( &crpix, &nitems, &dfault, tofchar(keyword),
                          tofchar("reference point of the axis") );

         (void) sprintf( keyword, "CUNIT%d=", nax );
         (void) sprintf( message, "give units of axis [%.*s]", nelc_c(nunit), nunit.a );
         (void) sprintf( cunit.a, "%.*s", nelc_c(nunit), nunit.a );
         dfault = REQUEST;
         r1 = usercharu_c( cunit, &nitems, &dfault, tofchar(keyword), tofchar( message ) );

         (void) sprintf( keyword, "CRVAL%d=", nax );
         (void) sprintf( message, "value at reference pixel in %.*s", nelc_c(cunit), cunit.a );
         dfault = NONE;
         r1 = userdble_c( &crval, &nitems, &dfault, tofchar(keyword), tofchar( message ) );

         (void) sprintf( keyword, "CDELT%d=", nax );
         (void) sprintf( message, "grid separation in %.*s", nelc_c(cunit), cunit.a );
         dfault = NONE;
         r1 = userdble_c( &cdelt, &nitems, &dfault, tofchar(keyword), tofchar( message ) );

         crota = 0.0;
         if (typ == 2) {
            (void) sprintf( keyword, "CROTA%d=", nax );
            dfault = REQUEST;
            r1 = userdble_c( &crota, &nitems, &dfault, tofchar(keyword),
                 tofchar("sky rotation angle [0.0 degrees]") );
         }

         if ((typ == 3) && (velsys != 0)) {
            (void) sprintf( keyword, "DUNIT%d=", nax );
            (void) sprintf( message, "Give units in velocity [%.*s]", nelc_c(sunit), sunit.a );
            (void) sprintf( dunit.a, "%.*s", nelc_c(sunit), sunit.a );
            dfault = REQUEST;
            r1 = usercharu_c( dunit,  &nitems, &dfault, tofchar(keyword), tofchar( message ) );

            (void) sprintf( keyword, "DRVAL%d=", nax );
            (void) sprintf( message, "Give reference value at reference pixel in %.*s", nelc_c(dunit), dunit.a );
            dfault = NONE;
            r1 = userdble_c( &drval, &nitems, &dfault, tofchar(keyword), tofchar( message ) );

            (void) sprintf( keyword, "FREQ0%d=", nax );
            dfault = NONE;
            r1 = userdble_c( &freq0, &nitems, &dfault, tofchar(keyword),
                             tofchar("Give rest frequency in Hz") );
         }

         /*  Now create the axis */
         gds_extend_c( Setout, ctype, &crpix, &naxis, &gerror );
         sprintf( message , "error=%d", gerror );
         if (gerror == -28) {
            fint lev = 1;
            error_c( &lev, tofchar("axis already in use") );
         } else {
            /* write CRVAL etc.*/
            (void) sprintf( keyword, "CRVAL%d", nax );
            gdsd_wdble_c( Setout, tofchar(keyword), &setlevel, &crval, &derror );
            (void) sprintf( keyword, "CDELT%d", nax );
            gdsd_wdble_c( Setout, tofchar(keyword), &setlevel, &cdelt, &derror );
            (void) sprintf( keyword, "CROTA%d", nax );
            gdsd_wdble_c( Setout, tofchar(keyword), &setlevel, &crota, &derror );
            (void) sprintf( keyword, "CUNIT%d", nax );
            gdsd_wchar_c( Setout, tofchar(keyword), &setlevel, cunit, &derror );

            if ((typ == 3) && (velsys != 0)) {
               (void) sprintf( keyword, "DRVAL%d", nax );
               gdsd_wdble_c( Setout, tofchar(keyword), &setlevel, &drval, &derror );
               (void) sprintf( keyword, "DUNIT%d", nax );
               gdsd_wchar_c( Setout, tofchar(keyword), &setlevel, dunit, &derror );
               (void) sprintf( keyword, "FREQ0%d", nax );
               gdsd_wdble_c( Setout, tofchar(keyword), &setlevel, &freq0, &derror );
            }
            nax++;
         }
      }
   } while (!quit);



   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
