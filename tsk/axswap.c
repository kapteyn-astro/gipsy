/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             axswap.dc1

Program:       AXSWAP

Purpose:       Swap first and second axis of 2-dim (sub) set
               and/or change the sign of the grid separation.

Category:      MANIPULATION, HEADER

File:          axswap.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set and (2-dim) subsets:
               Maximum number of subsets is 2048.


   OUTSET=     Give output set (, subsets):
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets.

   SWAPAXES=   Swap x and y axis?                               [Y]/N
               If you answer with No, then you can still change
               the sign of the pixel separations with keywords
               CHSEPX= and CHSEPY=

   CHSEPX=     Change sign of NEW pixel separation in x:         Y/[N]
               If Y then the grid ranges in x will be changed
               so that physical positions are unaltered.

   CHSEPY=     Change sign of NEW pixel separation in y:         Y/[N]
               If Y then the grid ranges in y will be changed
               so that physical positions are unaltered.


Description:   The order of data is changed according to the following
               table:
               SWAPAXES  CHSEPX  CHSEPY   xnew:   ynew:
                   Y       N       N        y       x
                   Y       Y       N       -y       x
                   Y       N       Y        y      -x
                   Y       Y       Y       -y      -x
                   N       N       N        x       y
                   N       Y       N       -x       y
                   N       N       Y        x      -y
                   N       Y       Y       -x      -y


Notes:         All header items on set level and subset level will be
               stored in the output, but items on levels that intersect
               these subsets will be lost in the transformation.

Example:

Updates:       Jan 12,  1993: VOG, Document created.
               Sep 19,  1995: VOG, Added keywords SWAPAXES=, CHSEPX= and
                                   CHSEPY=
               Nov 25,  2010: VOG, Removed bug with changing deltas while
                                   not swapping axes.

#<
*/

/*  axswap.c: include files     */

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

/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "usercharu.h"
#include    "userfio.h"
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/

/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsd_rchar.h"
#include    "gdsd_wchar.h"
#include    "gdsd_rdble.h"
#include    "gdsd_wdble.h"
#include    "gdsd_rint.h"
#include    "gdsd_wint.h"
#include    "gdsd_delete.h"

#include    "gdscpa.h"       /* Change primary axis */
#include    "gdscsa.h"       /* Change secondary axis */
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "minmax3.h"      /* Find min, max and #blanks in subset. */
#include    "showsub1.h"
#include    "wminmax.h"      /* Writes (new) minimum and maximum and number */
                             /* of blanks of subsets in the descriptor file */
                             /* and optionally deletes the MINMAX descriptors */
                             /* at intersecting levels. */

/* Output set related includes */

#include    "gdsasn.h"       /* GDSASN copies the coordinate system of a */
                             /* previously opened input set obtained with */
                             /* GDSINP to the output set to be obtained */
                             /* with GDSOUT. */
#include    "gdsout.h"       /* GDSOUT prompts the user to enter the */
                             /* name of an output set and the subsets, */
                             /* and returns the number of subsets entered. */
#include    "gdsi_write.h"   /* Writes data to (part of) an set. */


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
#define ISWAP(a,b)     { int temp=(a);(a)=(b);(b)=temp; } /* Swap 2 numbers */

#define RELEASE        "1.0"           /* Version number */
#define MAXAXES        10              /* Max. axes in a set */
#define MAXSUBSETS     2048            /* Max. allowed subsets */
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define FITSLEN        20
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
static fint     f2lo[MAXAXES];      /* Low  edge of frame in grids */
static fint     f2hi[MAXAXES];      /* High edge of frame in grids */

/* Reading data */

static fint     cwlo, cwhi;         /* Coordinate words. */
static fint     tid;                /* Transfer id for read function. */
static fint     maxIObuf;           /* Maximum size of read buffer. */
static fint     pixelsread;         /* Number of pixels read by read routine. */
static fint     pixelswrite;        /* Number of pixels to write to output. */
static fint     subnr;              /* Counter for subset loop. */
static float    *image = NULL;

/* OUTSET related variables */

static fchar    Setout;
static fint     subout[MAXSUBSETS];  /* Output subset coordinate words */
static fint     nsubsout;
static fint     axnumout[MAXAXES];
static fint     axcountout[MAXAXES];
static fint     cwloO, cwhiO;        /* Output Coordinate words. */
static fint     tidO;                /* Transfer id for write function. */

/* Miscellaneous */

static fchar    Key, Mes;
static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[120];       /* All purpose character buffer. */
static bool     agreed;             /* Loop guard. */



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   bool     chsepx, chsepy;
   bool     swapaxes;
   fint     r[6][2];            /* Array with error codes for subset items */
   fint     s[6][2];
   fint     nitems;
   fint     dfault;             /* Default option for input etc */
   int      lenXn;              /* Length of new x-axis */
   int      dxn, dyn;           /* Offsets in the output axes */

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
   subdim  = 2;
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
                       &class,     /* Class 1 is for applications which repeat operations*/
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


   dfault   = REQUEST;
   nitems   = 1;
   swapaxes = toflog( YES );
   r1 = userlog_c( &swapaxes, &nitems, &dfault, tofchar("SWAPAXES="),
                   tofchar("Swap x and y axis?                    [Y]/N") );
   swapaxes = tobool( swapaxes );

   dfault   = REQUEST;
   nitems   = 1;
   chsepx   = toflog( NO );
   r1 = userlog_c( &chsepx, &nitems, &dfault, tofchar("CHSEPX="),
                   tofchar("Change sign of NEW pixel separation in x:   Y/[N]") );
   chsepx = tobool( chsepx );

   dfault   = REQUEST;
   nitems   = 1;
   chsepy   = toflog( NO );
   r1 = userlog_c( &chsepy, &nitems, &dfault, tofchar("CHSEPY="),
                   tofchar("Change sign of NEW pixel separation in y:   Y/[N]") );
   chsepy = tobool( chsepy );


   /*--------------------------------------------------------------*/
   /* Assign 'gdsinp' buffer to 'gdsout'. Output set will get same */
   /* coordinate system as input INSET=.  GDSOUT is a function     */
   /* which prompts the user to enter the name of a set and        */
   /* (optionally) subset(s) and returns the number of subsets     */
   /* entered.                                                     */
   /*--------------------------------------------------------------*/

   gdsasn_c( KEY_INSET, KEY_OUTSET, &class );

   /*------------------------------------------------------------*/
   /* Now change all data on first subset axis to second and vv. */
   /* CDELT, CROTA, CRPIX, CRVAL, CTYPE, CUNIT, NAXIS and the    */
   /* 'D' versions. If an item was not found, it must be deleted */
   /* in the output set. For this, the global arrays r[] and s[] */
   /* are used.                                                  */
   /*------------------------------------------------------------*/

   {
      fint    ax[4];
      int     i,j;
      double  cdelt, crpix, crval, crota;
      fint    naxis;
      fchar   ctype, cunit;
      fint    pmask;
      double  ddelt, drpix, drval, drota;
      fchar   dtype, dunit;
      fint    smask;

      ax[0] = axnum[0];
      ax[1] = axnum[1];
      ax[2] = axnum[1];
      ax[3] = axnum[0];
      fmake( ctype, FITSLEN );
      fmake( cunit, FITSLEN );
      fmake( dtype, FITSLEN );
      fmake( dunit, FITSLEN );

      for (j = 0; j < 2; j++)
      {
         int   outnr;
         /* Initialize */
         cdelt = crpix = crval = crota = 0.0;
         ddelt = drpix = drval = drota = 0.0;
         outnr = j;
         if (swapaxes)
         {
            if (j == 0)
               outnr = 1;
            else
               outnr = 0;
         }
         (void) sprintf( message, "NAXIS%d", ax[j] );
         r1 = 0;
         gdsd_rint_c(  Setin, tofchar(message), &setlevel, &naxis, &r1 );

         for (i = 0; i <= 5; i++)
            r[i][j] = 0;

         (void) sprintf( message, "CDELT%d", ax[j] );
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &cdelt, &r[0][j] );
         (void) sprintf( message, "CROTA%d", ax[j] );
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crota, &r[1][j] );
         (void) sprintf( message, "CRPIX%d", ax[j] );
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crpix, &r[2][j] );
         (void) sprintf( message, "CRVAL%d", ax[j] );
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crval, &r[3][j] );
         (void) sprintf( message, "CTYPE%d", ax[j] );
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, ctype,  &r[4][j] );
         (void) sprintf( message, "CUNIT%d", ax[j] );
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, cunit,  &r[5][j] );

         if ( (swapaxes && ((j == 0 && chsepy) || (j == 1 && chsepx))) ||
              (!swapaxes && ((j == 0 && chsepx) || (j == 1 && chsepy))) )
         {
            /*----------------------------------------   */
            /* Change sign of grid separation. But       */
            /* relabel then grid 0 (at pixel CRPIX) also.*/
            /* This way we keep the physical coordi-     */
            /* nate system unaltered.                    */
            /*-------------------------------------------*/
            cdelt *= -1.0;
            crpix = naxis + 1 - crpix;
         }

         pmask = 0;
         if (r[0][j] >= 0) pmask += 32;
         if (r[1][j] >= 0) pmask += 16;
         if (r[2][j] >= 0) pmask +=  8;
         if (r[3][j] >= 0) pmask +=  4;
         if (r[4][j] >= 0) pmask +=  2;
         if (r[5][j] >= 0) pmask +=  1;
         gdscpa_c( KEY_OUTSET, &ax[outnr], &naxis,
                   &cdelt, &crota, &crpix, &crval,
                   ctype, cunit, &pmask );

         /* Secondary axes */
         for (i = 0; i <= 5; i++)
            s[i][j] = 0;

         (void) sprintf( message, "DDELT%d", ax[j] );;
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &ddelt, &s[0][j] );
         (void) sprintf( message, "DROTA%d", ax[j] );
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &drota, &s[1][j] );
         (void) sprintf( message, "DRPIX%d", ax[j] );
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &drpix, &s[2][j] );
         (void) sprintf( message, "DRVAL%d", ax[j] );
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &drval, &s[3][j] );
         (void) sprintf( message, "DTYPE%d", ax[j] );
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, dtype,  &s[4][j] );
         (void) sprintf( message, "DUNIT%d", ax[j] );
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, dunit,  &s[5][j] );

         if ( (swapaxes && ((j == 0 && chsepy) || (j == 1 && chsepx))) ||
              (!swapaxes && ((j == 0 && chsepx) || (j == 1 && chsepy))) )
         {
            ddelt *= -1.0;
            drpix = naxis + 1 - drpix;
         }
    
         smask = 0;
         if (s[0][j] >= 0) smask += 32;
         if (s[1][j] >= 0) smask += 16;
         if (s[2][j] >= 0) smask +=  8;
         if (s[3][j] >= 0) smask +=  4;
         if (s[4][j] >= 0) smask +=  2;
         if (s[5][j] >= 0) smask +=  1;
         gdscsa_c( KEY_OUTSET, &ax[outnr],
                   &ddelt, &drota, &drpix, &drval,
                   dtype, dunit, &smask );
      }
   }


   dfault  = NONE;
   showdev = 3;
   Key     = KEY_OUTSET;
   Mes     = MES_OUTSET;
   fmake( Setout, STRLEN );
   do
   {
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
      if (!agreed)
         reject_c( KEY_OUTSET, tofchar("#out != #in") );
   }
   while (!agreed);

   /*----------------------------------*/
   /* Determine edges of the new frame */
   /*----------------------------------*/
   {
      fint cwlo, cwhi;                          /* Local coordinate words */
      int  m;
      r1 = 0;
      gdsc_range_c( Setout, &setlevel, &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++) {
         f2lo[m] = gdsc_grid_c( Setout, &axnumout[m], &cwlo, &r1 );
         f2hi[m] = gdsc_grid_c( Setout, &axnumout[m], &cwhi, &r2 );
      }
   }

   /*-----------------------------------------------------------------*/
   /* Items have to be deleted if they could be found on one axis     */
   /* only. Example: s[0][0] <  0, DDELT1 was not found but,          */
   /*                s[0][1] >= 0, DDELT2 was found                   */
   /* DDELT1 gets the value of DDELT2, but DDELT2 is left unchanged   */
   /* because DDELT1 did not exist.                                   */
   /*-----------------------------------------------------------------*/
   {
      fint ax = 0;
      int  i;

      for (i = 0; i < 6; i++)
      {
         if (r[i][0] != r[i][1])
         {
            if (r[i][1] >= 0)
               ax = axnum[1];
            else
               ax = axnum[0];
            switch (i)
            {
               case 0 : sprintf( message, "CDELT%d", ax ); break;
               case 1 : sprintf( message, "CROTA%d", ax ); break;
               case 2 : sprintf( message, "CRPIX%d", ax ); break;
               case 3 : sprintf( message, "CRVAL%d", ax ); break;
               case 4 : sprintf( message, "CTYPE%d", ax ); break;
               case 5 : sprintf( message, "CUNIT%d", ax ); break;
            }
            if (swapaxes) {
               r1 = 0;
               gdsd_delete_c( Setout, tofchar(message), &setlevel, &r1 );
            }
         }
         if (s[i][0] != s[i][1])
         {
            if (s[i][1] >= 0)
               ax = axnum[1];
            else
               ax = axnum[0];
            switch (i)
            {
               case 0 : sprintf( message, "DDELT%d", ax ); break;
               case 1 : sprintf( message, "DROTA%d", ax ); break;
               case 2 : sprintf( message, "DRPIX%d", ax ); break;
               case 3 : sprintf( message, "DRVAL%d", ax ); break;
               case 4 : sprintf( message, "DTYPE%d", ax ); break;
               case 5 : sprintf( message, "DUNIT%d", ax ); break;
            }
            if (swapaxes) {
               r1 = 0;
               gdsd_delete_c( Setout, tofchar(message), &setlevel, &r1 );
            }
         }
      }
   }

   /* The values 'dxn' and 'dyn' are used to reposition the */
   /* image data if one of the cdelts changed its sign.     */
   
   dxn = f2hi[0]-f2lo[0];
   dyn = f2hi[1]-f2lo[1];

   maxIObuf = axcount[0] * axcount[1];
   image    = (float *) calloc( 2*maxIObuf, sizeof(float) );
   if (image == NULL)
   {
      anyoutf( 1, "Cannot allocate memory for 2*%d floats", maxIObuf );
      finis_c();
   }

   lenXn = axcount[0];
   if (swapaxes)
      lenXn = axcount[1];

   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/
   for(subnr = 0; subnr < nsubs; subnr++)
   {
      int   indxI, indxO;

      showsub1_c( Setin, &subin[subnr], axnum );
      cwlo   = gdsc_fill_c( Setin,  &subin[subnr],  flo );
      cwhi   = gdsc_fill_c( Setin,  &subin[subnr],  fhi );
      cwloO  = gdsc_fill_c( Setout, &subout[subnr], f2lo );
      cwhiO  = gdsc_fill_c( Setout, &subout[subnr], f2hi );
      tidO   = 0;
      tid    = 0;
      do
      {
         int    x, y;
         /* Read 'maxIObuf' values in 'image'. */
         gdsi_read_c( Setin,
                      &cwlo, &cwhi,
                      image,
                      &maxIObuf,
                      &pixelsread,
                      &tid );

         indxI = 0;
         /* Read per line in the input set */
         for (y = 0; y < axcount[1]; y++)
         {
            /* Process image value for each pixel in this line */
            for (x = 0; x < axcount[0]; x++)
            {
               int     xn, yn;
               float   value;
               xn = x; yn = y;
               if (swapaxes)
                  ISWAP( xn, yn );
               if (chsepx)
                  xn = dxn - xn;
               if (chsepy)
                  yn = dyn - yn;
               value = image[indxI++];
               indxO = yn * lenXn + xn;
               image[maxIObuf+indxO] = value;    /* The output image */
            }
         }
         pixelswrite = pixelsread;
         /* Write 'pixelswrite' values from 'image' to output. */
         gdsi_write_c( Setout,
                       &cwloO, &cwhiO,
                       &image[maxIObuf],
                       &maxIObuf,
                       &pixelswrite,
                       &tidO );
      } while (tid != 0);
   }

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   free( image );
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
