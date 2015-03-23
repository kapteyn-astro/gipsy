/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             mfilter.dc1

Program:       MFILTER

Purpose:       Apply a median (or average) filter to a (sub)set

Category:      CALCULATION, TRANSFER

File:          mfilter.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 2048.

   BOX=        Give box in .....                        [entire subset]

   OUTSET=     Give output set (, subsets):
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets.

   MEDBOX=     Give sizes of median box:                          [3,3]
               Sizes must be odd. A size equal to 1 means no
               median filter in that direction.

** AVERAGE=    Use average instead of median                      Y/[N]
               By repeatedly applying this boxcar average one can
               emulate cubic spline smoothing (use it 4 times).
               More applications rapidly emulates a 2d gaussian with
               standard deviation = N * MEDBOX / 12.

** INCLUDE=    Include central pixel in calculation?              [Y]/N

** ABSOLUTE=   Cutoff as absolute value?                          Y/[N]
               If ABSOLUTE=N (the default), the cutoff will be
               interpreted as a relative value.

   CUT=        Cutoff for replacing the centr.pix. by the med.val.[0.0]
               There are two modes for the cutoff. The modes are
               specified in ABSOLUTE=
               See description.

** REPLACE=    Replace blanks by local median?                    [Y]/N
               Default, a blank will be replaced by the median of
               the 'median' box. If REPLACE=N, a blank is NOT
               replaced.


Description:   An input set given with INSET= is filtered with a
               median filter and put in OUTSET=. For each pixel in
               BOX= the median is calculated in a l x m neighborhood
               where l and m are specified in MEDBOX=.
               There are some restrictions for the sizes in MEDBOX=
               1) the sizes must be odd.
               2) The size in Y direction must be smaller than the
                  number of lines in some buffer (calculated by the
                  program).
               3) The size in X-direction cannot exceed the width of
                  the subset.
               4) The total number of pixels in the 'median' box
                  cannot exceed the internal buffer size (calculated
                  by the program).

               The median is calculated by sorting the data in the
               'median' box. This can take a while for big boxes.
               An alternative (without sorting) is in development.
               You can exclude the value of the central pixel
               with INCLUDE=NO  (default, this pixel will be included).
               At the edges of the frame, not all neighbor pixels can
               contribute to the median.

               If the value of a pixel differs more than CUT= from
               the median, its value will be replaced by the median,
               otherwise, it will not be changed. The cutoff can be
               given as an relative or absolute value:

               1) ABSOLUTE=Y, the cutoff is absolute, i.e. if the
                  difference between an image value and the local
                  median is greater than CUT= , replace the image
                  value by the median, else keep the image value.
                  Expressed in statements:

                  diff = |image value - local median|
                  if diff > cutoff
                     return median
                  else
                     return image value

               2) ABSOLUTE=N (the default), the cutoff is a relative
                  value and is in the range 0..1. The statements are:

                  diff = | (image value - local median) / median |
                  if diff > cutoff
                     return median
                  else
                     return image value


               Blank pixels are replaced by the local median if
               REPLACE=Y (default).
               If REPLACE=N, blank pixels will not be replaced.


Notes:

Example:       <USER> mfilter
               MFILTER  Version 1.0  (May 13 1993)
               <USER> MFILTER INSET=HVC1
               Set HVC1 has 2 axes
               RA                 from  -207 to   207
               DEC                from  -236 to   236
               <USER> MFILTER BOX=
               BOX range for set HVC1 :
               RA                 from  -207 to   207
               DEC                from  -236 to   236
               <USER> MFILTER OUTSET=hvcmed
               Set hvcmed has 2 axes
               RA                 from  -207 to   207
               DEC                from  -236 to   236
               <USER> MFILTER MEDBOX=5 3
               <USER> MFILTER CUT=0.1

               ==================== MFILTER ====================
               Median filter in 5 x 3 neighborhood
               using cutoff value: 0.100000 in relative mode
               Name of input set:  HVC1
               Name of output set: hvcmed
               MFILTER processed data in 14.57 sec (7.64 cpu sec)
               <STATUS>  MFILTER   +++ FINISHED +++



Updates:       May 6,  1993: VOG, Document created.
               May 27, 1998: DoKester, averaging added
               Feb  1, 2000: JPT, Increased number of subsets.
#<
*/

/*  mfilter.c: include files     */

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
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/

/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */

/* miscellaneous */

#include    "stabar.h"
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "minmax3.h"      /* Find min, max and #blanks in subset. */
#include    "wminmax.h"      /* Writes (new) minimum and maximum and number */
                             /* of blanks of subsets in the descriptor file */
                             /* and optionally deletes the MINMAX descriptors */
                             /* at intersecting levels. */
#include    "sortra.h"       /* Sort real array in ascending order */
#include    "timer.h"        /* Returns the cpu time and real time */
#include    "status.h"


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

#define RELEASE        "1.0"           /* Version number */
#define MAXAXES        10              /* Max. axes in a set */
#define MAXSUBSETS     2048            /* Max. allowed subsets */
#define MAXBUF         8192            /* Buffer size for I/O */
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
#define KEY_MEDBOX     tofchar("MEDBOX=")
#define KEY_REPLACE    tofchar("REPLACE=")



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
static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */
static fint     boxopt;             /* The different options are: */
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */

/* Reading data */

static fint     maxIObuf = MAXBUF;  /* Maximum size of read buffer. */
static float    image[MAXBUF];      /* Buffer for write routine. */
static float    rbuffer[MAXBUF];    /* Buffer for read routine. */
static float    calcbuf[MAXBUF];    /* Calculation buffer */
static fint     subnr;              /* Counter for subset loop. */

/* OUTSET related variables */

static fchar    Setout;
static fint     subout[MAXSUBSETS];  /* Output subset coordinate words */
static fint     nsubsout;
static fint     axnumout[MAXAXES];
static fint     axcountout[MAXAXES];


/* Stabar related (progression bar) */

static float    stabarstart;
static float    stabarend;
static float    stabarcurrent;


/* Miscellaneous */

static fchar    Key, Mes;
static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[120];       /* All purpose character buffer. */
static bool     agreed;             /* Loop guard. */
static float    minval[MAXSUBSETS]; /* Min. value of data for each subset. */
static float    maxval[MAXSUBSETS]; /* Max. value of data for each subset. */
static fint     nblanks[MAXSUBSETS];/* Number of blanks in each subset. */
static fint     change;             /* Used in WMINMAX. change!=0 means */
                                    /* minimum and maximum have changed and */
                                    /* that the MINMAX descriptors at */
                                    /* intersecting levels will be removed. */
static fint     smallbox[2];
static bool     absolute;
static bool     include;
static bool     replace;            /* Replace blanks? */
static float    cutoff;
static fchar    Task;               /* Name of current task */
static double   realtime, cputime;
static fint     elapse;



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



#ifdef NUMREP

static float getmedian2( float *x, int n )
/*------------------------------------------------------------------*/
/* Function returns the median value of the array 'x' of length 'n'.*/
/* Numerical recipes, page 461                                      */
/*------------------------------------------------------------------*/
#define BIG 1.0e30
#define AFAC 1.5
#define AMP 1.5
{
   float   xmed;
   int     np,nm,j;
   float   xx,xp,xm,sumx,sum,eps,stemp,dum,ap,am,aa,a;


   if (n == 0) return( blank );
   if (n == 1) return( x[0] );

   a=0.5*(x[1]+x[n]);
   eps=fabs(x[n]-x[1]);
   am = -(ap=BIG);
   for (;;) {
      sum=sumx=0.0;
      np=nm=0;
      xm = -(xp=BIG);
      for (j=1;j<=n;j++) {
         xx=x[j];
         if (xx != a) {
            if (xx > a) {
               ++np;
               if (xx < xp) xp=xx;
            } else if (xx < a) {
               ++nm;
               if (xx > xm) xm=xx;
            }
            dum = 1.0/(eps+fabs(xx-a));
            sum += dum;
            sumx += (xx*dum);
         }
      }
      stemp=(sumx/sum)-a;
      if (np-nm >= 2) {
         am=a;
         aa =  stemp < 0.0 ? xp : xp+stemp*AMP;
         if (aa > ap) aa=0.5*(a+ap);
         eps=AFAC*fabs(aa-a);
         a=aa;
      } else if (nm-np >= 2) {
         ap=a;
         aa = stemp > 0.0 ? xm : xm+stemp*AMP;
         if (aa < am) aa=0.5*(a+am);
         eps=AFAC*fabs(aa-a);
         a=aa;
      } else {
         if (n % 2 == 0) {
            xmed = 0.5*(np == nm ? xp+xm : np > nm ? a+xp : xm+a);
         } else {
            xmed = np == nm ? a : np > nm ? xp : xm;
         }
         return( xmed );
      }
   }
}

#undef BIG
#undef AFAC
#undef AMP


#endif

static float getmedian( float *calcarray, fint len )
/*------------------------------------------------------------------*/
/* Determine the median of the 'calcarray' by sorting the array.    */
/* (this is not the fastest possible way).                          */
/*------------------------------------------------------------------*/
{
   int n;

   if (len == 0) return( blank );
   if (len == 1) return( calcarray[0] );

   sortra_c( calcarray, &len );
   n = len / 2;
   if (len%2) return( calcarray[n] );                           /* Odd length */
   else       return( 0.5*(calcarray[n-1]+calcarray[n]) );            /* Even */
}


static float getaverage( float *calcarray, fint len )
/*------------------------------------------------------------------*/
/* Determine the mean of the 'calcarray'.                           */
/*------------------------------------------------------------------*/
{
   int      n = len;
   float    sum = 0 ;

   if (len == 0) return( blank );

   while( n-- ) sum += *calcarray++ ;
   return sum / len ;
}




static void readbuf( fint Xstart, fint Xend, fint Ystart, fint Yend,
                     fint subset )
/*------------------------------------------------------------------*/
/* Read data between Xstart, Ystart, Xend and Yend. But extend this */
/* buffer first in both directions with the sizes of the 'median'.  */
/* box.                                                             */
/*------------------------------------------------------------------*/
{
   fint    cwlo, cwhi;
   fint    gridlo[2], gridhi[2];
   fint    tid = 0;
   fint    pixelsinimage = (Xend-Xstart+1)*(Yend-Ystart+1);
   fint    pixelsread;
   fint    extendY;


   /*----------------------------------------------------------------*/
   /* Read a buffer of Yend-Ystart+1 lines, but extend this buffer   */
   /* in + and - y direction with the 0.5*(y size-1) of the calcula- */
   /* tion box so that all pixels within Xstart,end, Ystart,end have */
   /* neighbours to calculate the median.                            */
   /*----------------------------------------------------------------*/
   extendY = (smallbox[1]-1)/2;
   gridlo[0] = Xstart;
   gridlo[1] = MYMAX( Ystart - extendY, flo[1] );
   gridhi[0] = Xend;
   gridhi[1] = MYMIN( Yend   + extendY, fhi[1] );
   pixelsinimage = (gridhi[0]-gridlo[0]+1)*(gridhi[1]-gridlo[1]+1);
   /* Get the coordinate words to read the buffer */
   cwlo = gdsc_fill_c( Setin, &subset, gridlo );
   cwhi = gdsc_fill_c( Setin, &subset, gridhi );
   gdsi_read_c( Setin, &cwlo, &cwhi, rbuffer,
               &pixelsinimage, &pixelsread, &tid );
   if (pixelsinimage != pixelsread) anyoutC( 3, "Not all pixels read" );
   if (tid != 0) anyoutC( 3, "Not all pixels transferred to buffer");
}



static void processbuf( fint Xstart, fint Xend,
                        fint Ystart, fint Yend )
/*------------------------------------------------------------------*/
/* Because a fixed buffer width was chosen, there is an offset in   */
/* the one dim. array index of the output array and the extended    */
/* input array. The offset depends on the extension in -y direction */
/* and the fixed buffer size in x direction. The variables flo,fhi  */
/* and blo,bhi are global arrays which indicate the frame size and  */
/* the box size. The 'smallbox' variable is also global and indica- */
/* tes the size of the box around each pixel in which the median is */
/* calculated.                                                      */
/*------------------------------------------------------------------*/
{
#define OUTSIDEBOX (int) ( (x < blo[0]) || (x > bhi[0]) || \
                           (y < blo[1]) || (y > bhi[1]) )

#define OUTSIDEFRAME (int) ( (xx < flo[0]) || (xx > fhi[0]) || \
                             (yy < flo[1]) || (yy > fhi[1]) )


   int    x, y;
   int    lenx = (Xend-Xstart+1);
   int    extendX = (smallbox[0]-1)/2;
   int    extendY = (smallbox[1]-1)/2;
   int    posim, posbuf;
   int    offset;



   offset = (Ystart - MYMAX( Ystart - extendY, flo[1] )) * lenx;
   for (y = Ystart; y <= Yend; y++) {
      for (x = Xstart; x <= Xend; x++) {
         posim = (y-Ystart)*lenx + (x-Xstart);
         posbuf = posim + offset;
         if (OUTSIDEBOX) {
            image[posim] = blank;
         } else {
            float  median;
            int    xx, yy;
            int    len = 0;
            int    indw, indr;
            float  centralpixel = blank;

            /* Loop over a box around the central pixel at x,y */
            for (yy = y - extendY; yy <= y + extendY; yy++) {
               for (xx = x - extendX; xx <= x + extendX; xx++) {
                  if (!OUTSIDEFRAME) {
                     float   val;
                     indw = (yy-Ystart)*lenx + (xx-Xstart);
                     indr = indw + offset;
                     val  = rbuffer[indr];

                     /*-----------------------------------------*/
                     /* 'calcbuf' IS AN ARRAY WITH VALUES OF    */
                     /* NEIGHBOURING PIXELS OF PIXEL (x,y) IN   */
                     /* A BOX WITH SIZES GIVEN IN MEDBOX=       */
                     /* ONLY NON-BLANK VALUES ARE INCLUDED.     */
                     /* THE VALUE OF THE CENTRAL PIXEL IS       */
                     /* STORED IN 'centralpixel'.               */
                     /*-----------------------------------------*/

                     if ((xx == x) && (yy == y)) {
                        centralpixel = val;
                        if (include && (val != blank)) {
                           calcbuf[len++] = val;
                        }
                     } else {
                        if (val != blank) {
                           calcbuf[len++] = val;
                        }
                     }
                  }
               }
            }
            /* Make a choice between median and average here */
            {
                fint    nitem, average, nitems=1, dfault=HIDDEN ;

                average = toflog( NO );
                nitem = userlog_c( &average, &nitems, &dfault,
                        tofchar("AVERAGE="),
                        tofchar("Use averaging instead of median?     Y/[N]") );

                if ( average ) median = getaverage( calcbuf, len );
                else           median = getmedian( calcbuf, len );
            }

            /*----------------------------------------------*/
            /* Decide whether to keep the original value or */
            /* to replace the original value by the median. */
            /*----------------------------------------------*/

            if (centralpixel == blank)
            {
               if (replace)
                  image[posim] = median;
               else
                  image[posim] = blank;
            }
            else
            {
               if (absolute)
               {
                  float diff = fabs( centralpixel - median );
                  if (diff > cutoff)
                     image[posim] = median;
                  else
                     image[posim] = centralpixel;
               }
               else
               {
                  /* The relative case */
                  if (median == 0.0)
                  {
                     image[posim] = median;
                  }
                  else
                  {
                     float diff = fabs( (centralpixel - median) / median );
                     if (diff > cutoff)
                        image[posim] = median;
                     else
                        image[posim] = centralpixel;
                  }
               }
            }
         }
      }
   }
}



static void writebuf( fint Xstart, fint Xend, fint Ystart, fint Yend,
                      fint subset )
/*------------------------------------------------------------------*/
/* Write data between Xstart, Ystart and Xend, Yend to (sub)set.    */
/*------------------------------------------------------------------*/
{
   fint    pixelsinimage = (Xend-Xstart+1)*(Yend-Ystart+1);
   fint    tid = 0;
   fint    cwlo, cwhi;
   fint    gridlo[2], gridhi[2];
   fint    pixelswrite;


   gridlo[0] = Xstart;  gridlo[1] = Ystart;
   gridhi[0] = Xend;    gridhi[1] = Yend;
   cwlo = gdsc_fill_c( Setout, &subset, gridlo );
   cwhi = gdsc_fill_c( Setout, &subset, gridhi );
   /* Write pixelsinimage' values from 'image' to output. */
   gdsi_write_c( Setout, &cwlo, &cwhi, image, &pixelsinimage, &pixelswrite, &tid );
   if (pixelsinimage != pixelswrite) anyoutC( 3, "Buffer not completely written" );
   if (tid != 0) anyoutC( 3, "Transfer not complete" );
}



static void writetolog( double realtime, double cputime )
/*------------------------------------------------------------*/
/* write some information to the log file.                    */
/*------------------------------------------------------------*/
{
   anyoutC( 3, " " );
   (void) sprintf( message, "==================== %.*s ====================",
                   nelc_c(Task), Task.a );
   anyoutC( 3, message );
   (void) sprintf( message,
      "Median filter in %d x %d neighborhood", smallbox[0], smallbox[1] );
   anyoutC( 3, message );
   if (absolute) {
      (void) sprintf( message, "using cutoff value: %f in absolute mode", cutoff );
   } else {
      (void) sprintf( message, "using cutoff value: %f in relative mode", cutoff );
   }
   anyoutC( 3, message );
   (void) sprintf( message, "Name of input set:  %.*s", nelc_c(Setin), Setin.a );
   anyoutC( 3, message );
   (void) sprintf( message, "Name of output set: %.*s", nelc_c(Setout), Setout.a );
   anyoutC( 3, message );
   (void) sprintf( message, "%.*s processed data in %.2f sec (%.2f cpu sec)",
                   nelc_c(Task), Task.a,
                   realtime, cputime );
   anyoutC( 3, message );
}


MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   int   maxYlines;
   fint  Xstart, Ystart;
   fint  Xend, Yend;
   fint  nitems;
   fint  dfault;


   init_c();                               /* contact Hermes */
   /* Task identification */
   {
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
   /* Prepare a box for INSET       */
   /*-------------------------------*/
   boxopt  = 0;
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

   gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
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

   /*------------------------------------------------------------------*/
   /* Ask for the size of a box around each pixel to calculate median  */
   /*------------------------------------------------------------------*/
   maxYlines  = maxIObuf / axcount[0];         /* Max. num of lines to read in buffer */
   nitems = 2;
   dfault = REQUEST;
   do {
      agreed = YES;
      smallbox[0] = 3; smallbox[1] = 3;
      r1 = userint_c( smallbox, &nitems, &dfault,
                      KEY_MEDBOX,
                      tofchar("Give sizes of median box:  [3,3]") );
      if ((smallbox[0] < 1) || (smallbox[1] < 1)) {
         agreed = NO;
         reject_c( KEY_MEDBOX, tofchar("size(s)<1 not allowed!") );
      }
      if (agreed && (smallbox[0]%2 == 0 || smallbox[1]%2 == 0)) {
         reject_c( KEY_MEDBOX, tofchar("Only odd values!") );
         agreed = NO;
      }
      if (agreed && ( (maxYlines-smallbox[1])<0)) {
         reject_c( KEY_MEDBOX, tofchar("Size in Y too big!") );
         agreed = NO;
      }
      if (agreed && (smallbox[0] > (fhi[0]-flo[0]+1))) {
         reject_c( KEY_MEDBOX, tofchar("Size in X too big!") );
         agreed = NO;
      }
      if (agreed && (smallbox[0] * smallbox[1] > MAXBUF)) {
         (void) sprintf( message, ">max(%d) pixels", MAXBUF);
         reject_c( KEY_MEDBOX, tofchar(message) );
         agreed = NO;
      }
   } while (!agreed);
   /*---------------------------------------------------------*/
   /* It must be possible to extend the buffer to get all the */
   /* neighbour pixels too, i.e. the max. number of lines to  */
   /* read is decreased.                                      */
   /*---------------------------------------------------------*/
   maxYlines += (1 - smallbox[1]);


   /* Include central pixel in calculations? */
   include = toflog( YES );
   nitems  = 1;
   dfault  = HIDDEN;
   r1 = userlog_c( &include, &nitems, &dfault,
                   tofchar("INCLUDE="),
                   tofchar("Include central pixel in calculation?   [Y]/N") );
   include = tobool( include );


   replace = toflog( YES );
   nitems  = 1;
   dfault  = HIDDEN;
   r1 = userlog_c( &replace, &nitems, &dfault,
                   KEY_REPLACE,
                   tofchar("Replace blanks by local median?    [Y]/N") );
   replace = tobool( replace );


   /*-------------------------------------------------------------*/
   /* To replace image values by a median or an average, a cutoff */
   /* is used. This cutoff can be used absolute or relative.      */
   /* The operation mode is specified in ABSOLUTE=                */
   /*-------------------------------------------------------------*/

   absolute = toflog( NO );
   nitems   = 1;
   dfault   = HIDDEN;
   r1 = userlog_c( &absolute, &nitems, &dfault,
                   tofchar("ABSOLUTE="),
                   tofchar("Cutoff as absolute value?      Y/[N]") );
   absolute = tobool( absolute );


   cutoff = 0.0;
   nitems = 1;
   dfault = REQUEST;
   if (absolute) {
      (void) sprintf( message, "Absolute cutoff > 0:            [0.0]");
   } else {
      (void) sprintf( message, "Relative cutoff 0..1:           [0.0]");
   }
   r1 = userreal_c( &cutoff, &nitems, &dfault,
                    tofchar("CUT="),
                    tofchar(message) );
   cutoff = fabs( cutoff );

   /*---------------------------------------------------------------*/
   /* The Y grid starts at lowest y. The highest y is 'maxYlines-1' */
   /* higher, so that the buffer is 'maxYlines' high. The maximum   */
   /* y value cannot exceed the maximum grid in y.                  */
   /*---------------------------------------------------------------*/
   /* The width of the buffer is always the subset width: */
   Xstart = flo[0];
   Xend   = fhi[0];

   /* Calculate total number of pixels to process for use in bar */
   stabarstart   = 0.0;
   stabarend     = (float) nsubs*(axcount[0]*axcount[1]);
   stabarcurrent = 0.0;
   stabar_c( &stabarstart, &stabarend, &stabarcurrent );

   /*------------------------------------------------------------*/
   /* Start a LOOP OVER ALL SUBSETS. Increase the y grids of the */
   /* buffer position until all lines are processed.             */
   /*------------------------------------------------------------*/
   elapse = 0;
   timer_c( &cputime, &realtime, &elapse );                    /* start timer */
   for(subnr = 0; subnr < nsubs; subnr++) {
      fint mcount = 0;
      Ystart = flo[1];
      Yend   = MYMIN( Ystart + maxYlines - 1, fhi[1] );
      do {
         fint pixelsinimage = (Xend-Xstart+1)*(Yend-Ystart+1);
         readbuf(    Xstart, Xend, Ystart, Yend, subin[subnr] );
         processbuf( Xstart, Xend, Ystart, Yend );
         minmax3_c(  image,            /* Calculate running min, max & blanks */
                     &pixelsinimage,
                     &minval[subnr], &maxval[subnr], &nblanks[subnr],
                     &mcount );
         writebuf(   Xstart, Xend, Ystart, Yend, subout[subnr] );
         stabarcurrent += (float) pixelsinimage;
         stabar_c( &stabarstart, &stabarend, &stabarcurrent );
         Ystart = Yend + 1;
         Yend   = MYMIN( Ystart + maxYlines - 1, fhi[1] );
      } while (Ystart <= Yend);
   }
   elapse = 1;
   timer_c( &cputime, &realtime, &elapse );       /* what's the elapsed time? */

   /*------------------------------------------------------------*/
   /* Update descriptor of OUTSET= with new values of minimum,   */
   /* maximum and the number of blanks. Minimum and maximum have */
   /* changed and the MINMAX descriptors at intersecting will be */
   /* removed by WMINMAX.                                        */
   /*------------------------------------------------------------*/
   change = YES;
   wminmax_c( Setout, subout,
              minval, maxval, nblanks,
              &nsubsout,
              &change );
   writetolog( realtime, cputime );
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
