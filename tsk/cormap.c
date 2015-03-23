/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             cormap.dc1

Program:       CORMAP

Purpose:       Check whether there is a correlation between two maps

Category:      ANALYSIS, CALCULATION

File:          cormap.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 2048.

   BOX=        Give box:                                [entire subset]

   RANGE=      Give range of levels to include in calculation:    [all]
               Input consists of two values. The first value may
               be greater than the second. See the description
               for the correct use of this keyword.

   INSET2=     Give set, subsets:
               Number of subsets and subset dimension must be equal
               to INSET= The size of INSET2= must be greater than
               or equal to the size of BOX= otherwise a new set will be
               asked.

   BOX2=       Give box in .....                  [defaults by program]
               The size of the second box must be equal to
               the size of the first box.
               If the edges of BOX= are contained within INSET2=, then
               the defaults are these edges, else the default are the
               maximum sizes of BOX=

   RANGE2=     Give range of levels to include in calculation:    [all]
               Range of levels for second (sub)set.

   GRDEVICE=   Plot device:                           [List of devices]
               Destination of plot, Screen or Hardcopy.

   PGMOSAIC=   View surface sub divisions in x,y:                 [1,1]
               View surface can contain a number of plots in
               in X and Y direction (mosaic). Default is 1 plot in
               both X- and Y direction.

   PGPAPER=    Give width(cm), aspect ratio:               [calc, calc]
               Aspect ratio is height/width.

   PGBOX=      Corners of box Xl,Yl,Xh,Yh:     [default by application]
               It is possible to overrule the calculated
               PGPLOT box size with PGBOX=. The coordinates (x,y) of
               the lower point are given first.

   PGCOLOR=    Give color 1..15:                                    [1]
               See description for the available colors.

   PGWIDTH=    Give line width 1..21:                               [1]

   PGHEIGHT=   Give character height:                             [1.0]

   PGFONT=     Give font 1..4:                                      [2]


Description:   CORMAP makes a diagram with pixel values of two
               (sub)sets plotted against each other. The pixels
               have data ranges as given in RANGE= and RANGE2=


               Furthermore, it
               calculates  the regression lines y=ax+b and x=cy+d, the
               deviations from these lines and the linear correlation
               coefficient r. If r = 1 or r = -1, there is perfect
               positive resp. negative correlation. If r = 0, there is
               total absence of correlation.
               The square root of the variance is calculated, where
               the variance is given by:

                          2     1                       2
                         S  = ----- SUM[ (Yi - a - b*Xi)  ]
                              n - 2

               If n is the number of valid data pairs in the regression,
               the significance of r is the probability Pc(r,n) that
               the data points could represent a sample from an
               uncorrelated parent population:
               Pc(r,n) < 5%   is significant.
               Pc(r,n) < 1%   is highly significant.
               Pc(r,n) < 0.1% is very highly significant.

               In most cases with large n, this probability is so
               small, that 0(%) is printed.
               The applied statistic t = r * sqrt( n-2 / 1-r**2 ) is
               distributed in the case of the null-hypothesis (of no
               correlation) like Student's t-distribution with
               v = n-2 degrees of freedom.
               The two sided significance level is given by Pc(r,n).

               References:

               -Kreyszig, E., Introductory Mathematical Statistics
                Chapter 17, 18.
               -Press, H. et al, Numerical Recipes, Chapter 14.
               -Bevington, P., Data Reduction and Error Analysis
                for the Physical Sciences, Chapter 7.
               -Ractliffe, J., Elements of Mathematical Statistics
                Chapter 16.


               The number of input subsets and the dimension of the
               subsets must be equal. The box sizes must be equal.
               Blanks in either one of the subsets will not be
               plotted and are not taken into calculation.


               Examples of the use of the RANGE= keyword:

               RANGE=2, 5
               (IF 2<value<5 THEN use this value ELSE value==>blank)

               RANGE=5, 2
               (IF 2<value<5 THEN value==>blank ELSE use this value)

               At the RANGE= keyword, the values -INF and INF can
               be input also. These values represent the minimum
               and maximum values of the current system.

               RANGE=5, INF
               (IF value>5 THEN use this value ELSE value==>blank)


               Color indices for PGCOLOR=

               0      Background
               1      Default (Black if background is white)
               2      Red
               3      Green
               4      Blue
               5      Cyan
               6      Magenta
               7      Yellow
               8      Orange
               7      Yellow
               8      Orange
               9      Green + Yellow
               10     Green + Cyan
               11     Blue + Cyan
               12     Blue + Magenta
               13     Red + Magenta
               14     Dark Gray
               15     Light Gray
               16-255 Undefined


               Available fonts for PGFONT=

               1  single stroke "normal" font
               2  roman font
               3  italic font
               4  script font

Notes:


Example:

   <USER> cormap
   <USER> CORMAP INSET=hi1277 1
   <USER> CORMAP BOX=
   <USER> CORMAP INSET2=hi1277 2
   <USER> CORMAP BOX2=
   <USER> CORMAP GRDEVICE=g

   Statistics
   ==========
   Data points diagram  : n = 100
   Regression Y on X    : Y = +0.9804 (+/-0.0357) X + -2.1644 (+/-0.3035)
   Regression X on Y    : X = +0.9026 (+/-0.0328) Y + +1.9823 (+/-0.2978)
   Linear corr. coeff.  : r = 0.940727
   Significance of r    : Pc(r,n) = 0.0(%)

   Pc(r,n) < 5%   is significant.
   Pc(r,n) < 1%   is highly significant.
   Pc(r,n) < 0.1% is very highly significant.
   <STATUS>  CORMAP   +++ FINISHED +++


Updates:       May 7,  1992: VOG, Document created.
               Jul 27, 1999: VOG, Increased length of message for 
                                  longer box text.

#<
*/

/*  cormap.c: include files     */

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
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "status.h"
#include    "getusernam.h"   /* Returns the user name of the current user.*/
#include    "getdate.h"      /* Returns the current time and date as a text string */
#include    "axunit.h"
#include    "clipper.h"      /* Subroutine to conditionally transfer data.*/


/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "usercharu.h"
#include    "getrange.h"
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/


#include    "minmax1.h"
#include    "error.h"
#include    "cotrans.h"

/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_name.h"    /* Return name of axis */
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/




/* PGPLOT includes */


#include    "pgplot.h"       /* Includes for pgplot routines */




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
#define MAXBUF         4096            /* Buffer size for I/O */
#define STRLEN         256             /* Max length of strings */
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
#define KEY_INSET2     tofchar("INSET2=")
#define MES_INSET2     tofchar("Give second input set (, subsets):")
#define KEY_BOX2       tofchar("BOX2=")
#define MES_BOX2       tofchar(" ")
#define MES_RANGE      tofchar("Give range of levels to Include:  [all]")

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

/* Repeat for second set: */

static fchar    Setin2;
static fint     subin2[MAXSUBSETS];
static fint     nsubs2;
static fint     axnum2[MAXAXES];
static fint     axcount2[MAXAXES];

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

/* Repeat for second set: */

static fint     blo2[MAXAXES];
static fint     bhi2[MAXAXES];
static fint     flo2[MAXAXES];
static fint     fhi2[MAXAXES];

/* Reading data */

static fint     cwlo, cwhi;         /* Coordinate words. */
static fint     tid;                /* Transfer id for read function. */
static fint     maxIObuf = MAXBUF;  /* Maximum size of read buffer. */
static fint     pixelsread;         /* Number of pixels read by read routine. */
static float    image[MAXBUF];      /* Buffer for read routine. */
static fint     subnr;              /* Counter for subset loop. */


/* Repeat for second set: */

static fint     cwlo2, cwhi2;
static fint     tid2;
static float    image2[MAXBUF];


/* PGPLOT variables */

const  fint  background  =  0;      /* Color definitions for PGPLOT. */
const  fint  foreground  =  1;      /* Black if background is white. */
const  fint  red         =  2;
const  fint  green       =  3;
const  fint  blue        =  4;
const  fint  cyan        =  5;
const  fint  magenta     =  6;
const  fint  yellow      =  7;
const  fint  orange      =  8;
const  fint  greenyellow =  9;
const  fint  greencyan   = 10;
const  fint  bluecyan    = 11;
const  fint  bluemagenta = 12;
const  fint  redmagenta  = 13;
const  fint  darkgray    = 14;
const  fint  lightgray   = 15;

static fint  symbol;

/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[120];       /* All purpose character buffer. */
static int      i;                  /* Various counters. */
static bool     agreed;
static bool     first;
static bool     inside;
static float    Xmin, Xmax, Ymin, Ymax;
static float    xmin, xmax, ymin, ymax;
static fint     nitems;

static float    sumX, sumY, sumXX, sumXY, sumYY;
static fint     ndata;
static float    M, B, sigM, sigB, Rlin, variance;
static float    x[MAXBUF], y[MAXBUF];
static fint     jj;
static float    xl, yl;
static double   prob;

static float    range[2], range2[2];


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



static double GAMMLN( double XX )
/*-------------------------------------------------------*/
/* Returns the value LN[gamma(xx)] for xx>0              */
/*-------------------------------------------------------*/
{
   double COF[6] = { 76.18009173,
                    -86.50532033,
                     24.01409822,
                    -1.231739516,
                     0.120858003e-2,
                    -0.536382e-5 };

   double STP = 2.50662827465;
   double HALF = 0.5, ONE = 1.0 , FPF = 5.5;
   double X, TMP, SER;
   int j;


   X = XX-ONE;
   TMP = X + FPF;
   TMP = ( X + HALF ) * log(TMP) - TMP;
   SER = ONE;
   for (j = 0; j < 6; j++) 
   {
      X = X + ONE;
      SER = SER + COF[j] / X;
   }
   return ( TMP + log( STP * SER ) );
}



static double probability( float Rlin, fint N )
/*----------------------------------------------------------------------*/
/* Calculate the probability Pc(r,N) of exceeding r in a random         */
/* sample of observations taken from an uncorrelated parent             */
/* population (rho=0).                                                  */
/* The statistic t = r * sqrt( n-2 / 1-r**2 ) is distributed in the     */
/* case of the null-hypothesis (of no correlation) like Student's t-    */
/* distribution with v = n-2 degrees of freedom. The two sided signi-   */
/* ficance level is given by 1 - A(t|v). For large n it is not          */
/* neccessary to assume a binormal distribution for the variables x, y  */
/* in the sample. r is the linear correlation coefficient for pairs of  */
/* quantities (Xi, Yi). The value of r lies between -1 and 1 inclusive. */
/* The function returns the significance level.                         */
/* If x and y are uncorrelated and we can assume that r is distributed  */
/* normally, this function returns the significance of the correlation, */
/* i.e. the probability that |r| should be larger than its observed     */
/* value in the null hypothesis. The significance level alpha is        */
/* classified as:                                                       */
/*                                                                      */
/* 5%   or less ( ttest < 0.05  )        ... significant                */
/* 1%   or less ( ttest < 0.01  )        ... highly significant         */
/* 0.1% or less ( ttest < 0.001 )        ... very highly significant    */
/*----------------------------------------------------------------------*/
{
   double freedom, free;
   double r, R2;
   int    i, imax;
   double term;
   double fi, fnum, sum, denom, pcorre;
   int    uneven;


   /*-------------------------------------------------------------------------*/
   /* Algorithm: Bevington, Philip R., 1969, Data Reduction and Error         */
   /*            Analysis for the Physical Sciences (New York: McGraw-Hill),  */
   /*            Chapter 6.                                                   */
   /*-------------------------------------------------------------------------*/

   r = (double) Rlin;
   R2 = r * r;
   freedom = (double) N - 2.0;
   if (freedom  < 0.0) 
      return( 0.0 );
   if ((1.0 - R2) <= 0.0) 
      return( 0.0 );
   uneven = fmod( freedom, 2.0 );
   if (!uneven) 
   {
      imax = (int) (freedom -2.0) / 2.0;
      free = freedom;
      term = fabs( r );
      sum  = term;
      if (imax < 0) return( 0.0 );
      if (imax == 0) return (1.0 - term);
      for (i = 1; i <= imax; i++) 
      {
         fi    = (double) i;
         fnum  = (double) (imax - i + 1.0);
         denom = 2.0 * i + 1;
         term  = -term * R2 * fnum/fi;
         sum   = sum + term/denom;
      }
      pcorre = 1.128379167 * exp( GAMMLN((free+1.0)/2.0)) /
                             exp( GAMMLN( free / 2.0 ));
      pcorre = 1.0 - pcorre * sum;
   } 
   else 
   {
      imax = (int) (freedom - 3) / 2;
      term = fabs( r ) * sqrt( 1.0 - R2 );
      sum = atan( R2/term );
      if (imax < 0) 
      {
         return (1.0 - 0.6366197724*sum);
      }
      if (imax == 0) 
      {
         sum += term;
         return (1.0 - 0.6366197724*sum);
      }
      sum  += term;
      for (i = 1; i <= imax; i++) 
      {
         fnum  = 2.0 * (double) i;
         denom = 2.0 * (double) i + 1.0;
         term  = term * (1.0-R2) * fnum/denom;
         sum   = sum + term;
      }
      pcorre = 1.0 - 0.6366197724*sum;
   }
/*   if (pcorre < 0.0) pcorre = 0.0;*/
   return (fabs(pcorre));
}



static fint sums( float *x, float *y, fint ndata,
                  float *fsumX, float *fsumY, float *fsumXX,
                  float *fsumXY, float *fsumYY, bool *first )
/*--------------------------------------------------------*/
/* Determine different sum, used in the linear regression */
/* Returned is the number of valid data pairs, i.e.       */
/* x nor y is a blank.                                    */
/*--------------------------------------------------------*/
{
   static double  sumX, sumY, sumXX, sumYY, sumXY;
   static fint    ntot;
   double         X, Y;
   int            i;


   if (*first) 
   {
      ntot = 0;
      sumX = sumY = 0.0;
      sumXX = sumYY = sumXY = 0.0;
   }
   for (i = 0; i < ndata; i++) 
   {
      if ( (x[i] != blank) && (y[i] != blank) ) 
      {
         ntot += 1;
         X = (double) x[i];
         Y = (double) y[i];
         sumX += X;
         sumY += Y;
         sumXX += X * X;
         sumXY += X * Y;
         sumYY += Y * Y;
      }
   }
   *fsumX  = (float) sumX;
   *fsumY  = (float) sumY;
   *fsumXX = (float) sumXX;
   *fsumXY = (float) sumXY;
   *fsumYY = (float) sumYY;
   *first  = 0;
   return ntot;
}




static fint linreg( fint ndata, float *fsumX, float *fsumY, float *fsumXX,
                    float *fsumXY, float *fsumYY, float *m, float *b,
                    float *corrcoeff, float *sigM, float *sigB,
                    float *varian )
/*-----------------------------------------------------------------------*/
/* INPUT:   fsumX etc, ndata                                             */
/* OUTPUT:  m, b, corrcoeff, sigM, sigB, corrcoeff, varian               */
/* PURPOSE: Input is sumX/Y/XX/XY/YY, and ndata, the number of data-     */
/*          points. In y = mx + b the parameters m and b are determined  */
/*          with a method called linear regression. Also a correlation-  */
/*          coefficient is determined. If there is not enough data, the  */
/*          values m = 0 and b = 0 are returned. The function itself     */
/*          then returns 0;                                              */
/*          The independent quantities are stored in x, the dependent    */
/*          data points are stored in y. sigM and sigB are the           */
/*          uncertainties in m and b (Bev. 6.21, 6.22).                  */
/*          In this routine there is no weighting ==>                    */
/*          chi2 = (ndata-2) * variance                                  */
/*          Bevington, Philip R., 1969, Data Reduction and Error         */
/*          Analysis for the Physical Sciences (New York: McGraw-Hill),  */
/*          Chapter 6.                                                   */
/*-----------------------------------------------------------------------*/
{
   double  sumX, sumY, sumXX, sumYY, sumXY;
   double  N;
   double  freedom;
   double  delta;
   double  variance;
   double  A, B, sigmA, sigmB;                              /* Y = A + BX !!! */
   double  Rlin;                            /* Linear correlation coefficient */


   N = (double) ndata;
   sumX  = (double) *fsumX;
   sumY  = (double) *fsumY;
   sumXX = (double) *fsumXX;
   sumXY = (double) *fsumXY;
   sumYY = (double) *fsumYY;

   if (ndata < 3) 
   {
      anyoutC( 3, "Not enough data pairs" );
      *m = *b = 0.0;
      *sigM = *sigB = 0.0;
      return(0);
   }

   delta      = N*sumXX - sumX*sumX;
   A          = (sumXX*sumY - sumX*sumXY) / delta;
   B          = (sumXY*N    - sumX*sumY ) / delta;
   freedom    = N - 2.0;
   variance   = (sumYY + A*A*ndata + B*B*sumXX -
                 2.0*(A*sumY+B*sumXY - A*B*sumX)) / freedom;
   sigmA      = sqrt( variance*sumXX / delta );
   sigmB      = sqrt( variance*N     / delta );
   Rlin       = (N*sumXY - sumX*sumY) / sqrt(delta*(N*sumYY - sumY*sumY));
   *m         = (float) B;
   *b         = (float) A;
   *sigM      = (float) sigmB;
   *sigB      = (float) sigmA;
   *corrcoeff = (float) Rlin;
   *varian    = (float) variance;
   return(1);
}





void initplot( void )
/*------------------------------------------------------------------*/
/* Initialize plot software. Set viewport and output dimensions.    */
/* If output device is a printer, ask user for line width.          */
/*------------------------------------------------------------------*/
{
   fint   unit;            /* Ignored by pgbeg, use unit=0. */
   fchar  Devspec;         /* Device specification. */
   fint   nxysub[2];       /* Number of subdivisions on 1 page. */
   float  width;           /* Width of output on paper */
   float  aspect;          /* Aspect ratio of output on paper */
   fint   nitems, dfault;
   fint   hidden = 2;
   fint   r1;
   fint   errlev = 4;      /* Set error level to fatal. */
   bool   pageoff;         /* Disable PGPLOT's NEXTPAGE keyword. */
   float  paper[2];
   float  xl, xr, yb, yt;  /* Edges of the viewport. */


   /* Begin PGPLOT, open output device. A return value of 1 indicates */
   /* successful completion. There are 4 arguments for PGBEG:         */
   /* UNIT, this argument is ignored by PGBEG (use zero).             */
   /* FILE, If this argument is a question mark PGBEG will prompt the */
   /*       user to supply a string.                                  */
   /* NXSUB,the number of subdivisions of the view surface in X.      */
   /* NYSUB,the number of subdivisions of the view surface in Y.      */

   nxysub[1] = nxysub[0] = 1;           /* Default no subdivisions in plot.*/
   nitems = 2;
   dfault = hidden;
   r1 = userint_c( nxysub,
                   &nitems,
                   &dfault,
                   tofchar("PGMOSAIC="),
                   tofchar("View surface sub divisions in x,y:   [1,1]") );

   unit = 0;
   Devspec = tofchar("?");
   r1 = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );
   if (r1 != 1) 
      error_c( &errlev, tofchar("Cannot open output device") );

   /* No PGPLOT's NEXTPAGE= keyword */
   pageoff = toflog( 0 );
   pgask_c( &pageoff );

   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */
   nitems = 2; dfault = hidden;
   paper[0] = 0.0; paper[1] = 1.0;
   r1 = userreal_c( paper,
                    &nitems,
                    &dfault,
                    tofchar("PGPAPER="),
                    tofchar("Give width(cm), aspect ratio: [calculated]") );
   if (r1 > 0) 
   {
      /* If width = 0.0 then the program will select the largest view surface */
      width  = paper[0] / 2.54;      /* Convert width to inches. */
      aspect = paper[1];
      pgpap_c( &width, &aspect );
   }

   /* Set viewport */
   xl = 0.2; xr = 0.9;
   yb = 0.3; yt = 0.9;
   pgsvp_c( &xl, &xr, &yb, &yt );
}


static int subsetstring( fchar Setin, fint subset, fint *axnum,
                         char *substr, double *phys )
/*----------------------------------------------------------------*/
/* Find physical coordinates of repeat axes. Return values in     */
/* string 'substr' and first value as double in 'phys'            */
/*----------------------------------------------------------------*/
{
   fint     setdim;
   fint     subdim;
   int      i;
   fint     r1, r2;
   double   coordin[MAXAXES];    /* Grids before transformation */
   double   coordout[MAXAXES];   /* Physical coordinates after transformation */
   fint     direction;           /* grid coord. -> physical coord. */
   fchar    Cunit;
   fchar    Axname;


   fmake( Axname, 20 );
   fmake( Cunit, 20 );
   setdim = gdsc_ndims_c( Setin, &setlevel );              /* dimension of set */
   subdim = gdsc_ndims_c( Setin, &subset );                /* dimension of subset */
   if (setdim == subdim) 
   {
      strcpy( substr, "Set level" );
      return( strlen(substr) );;
   }
   for (i = 0; i < subdim; i++) 
      coordin[i] = 0.0;
   for (i = subdim; i < setdim; i++) 
   {
      r1 = 0;
      coordin[axnum[i]-1] = (double) gdsc_grid_c( Setin, &axnum[i], &subset, &r1 );
   }
   direction = 1;                           /* grid coord. -> physical coord. */
   r1 = cotrans_c( Setin, &subset, coordin, coordout, &direction );

   for (i = subdim; i < setdim; i++) 
   {
      if (i == subdim) 
      {
         r2 = 0;
         gdsc_name_c( Axname, Setin, &axnum[i], &r2 );
         if (nelc_c(Axname) == 0) Axname.a[0] = '?';
         if (r1 == 0) 
         {
            (void) sprintf( substr, "%s %d=%.6g",
                            strtok( Axname.a, " -" ),
                            (int)coordin[axnum[i]-1], coordout[axnum[i]-1] );
         } 
         else 
         {
            (void) sprintf( substr, "%4d", (int) coordin[axnum[i]-1] );
         }
      } 
      else 
      {
         if (r1 == 0) 
            (void) sprintf( substr, "%.*s, %.6g", strlen(substr), substr, coordout[axnum[i]-1] );
         else 
            (void) sprintf( substr, "%.*s, %.4d", strlen(substr), substr, coordin[axnum[i]-1] );
      }
      r1 = axunit_c( Setin, &axnum[i], Cunit );
      if (r1 == 0) 
         (void) sprintf( substr, "%.*s %.*s", strlen(substr), substr,
                         nelc_c(Cunit), Cunit.a );
   }
   if (r1 == 0) 
      *phys = coordout[axnum[subdim]-1]; else *phys = coordin[axnum[subdim]-1];
   return( strlen(substr) );
}



void plotbox( float Xmin, float Ymin, float Xmax, float Ymax,
              fint subset, fint subset2, fint *axnum, fint *axnum2 )
/*------------------------------------------------------------------*/
/* Draw box and labels. Take special care for the y labels and      */
/* title. Colors are defined globally. Xmin etc are the corners of  */
/* the box in world coordinates.                                    */
/*------------------------------------------------------------------*/
{
   float  charsize = 1.0;
   float  delta;
   fint   lwidth;
   fint   r1;
   fint   nitems;
   float  drawbox[4];                         /* Corners of draw box. */
   fint   color;
   fint   font;
   fint   nxsub, nysub;
   float  xtick, ytick;
   fchar  Xtitle, Ytitle, Toptitle;
   fint   len1;
   fint   hidden = 2;
   char   message[STRLEN];
   double phys;


   pgpage_c();                                /* Advance to new page. */

   /* Increase the size of the box a little */
   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Xmin -= delta; Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Ymin -= delta; Ymax += delta;
   drawbox[0] = Xmin; drawbox[1] = Ymin;      /* Get size from user input */
   drawbox[2] = Xmax; drawbox[3] = Ymax;
   nitems = 4;
   (void) sprintf( message, "Corners of box Xl,Yl, Xh,Yh:  [%f,%f,%f,%f]", Xmin,Ymin,Xmax,Ymax );
   r1 = userreal_c( drawbox,
                    &nitems,
                    &hidden,
                    tofchar("PGBOX="),
                    tofchar( message ) );
   Xmin = drawbox[0]; Ymin = drawbox[1];
   Xmax = drawbox[2]; Ymax = drawbox[3];
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );    /* Set the window */

   nitems = 1; color = 1;
   r1 = userint_c( &color,
                   &nitems,
                   &hidden,
                   tofchar("PGCOLOR="),
                   tofchar("Give color 1..15:        [1]") );
   if (color > 15) color = 15;
   if (color < 1 ) color =  1;
   pgsci_c( &color );

   nitems = 1; lwidth = 1;
   r1 = userint_c( &lwidth,
                   &nitems,
                   &hidden,
                   tofchar("PGWIDTH="),
                   tofchar("Give line width 1..21:        [1]") );
   if (lwidth > 21) lwidth = 21;
   if (lwidth < 1 ) lwidth =  1;
   pgslw_c( &lwidth );                       /* Set line width. */

   nitems = 1; charsize = 1.0;
   r1 = userreal_c( &charsize,
                    &nitems,
                    &hidden,
                    tofchar("PGHEIGHT="),
                    tofchar("Give character height:     [1.0]") );
   pgsch_c( &charsize );                     /* Character height. */

   nitems = 1; font = 2;
   r1 = userint_c( &font,
                   &nitems,
                   &hidden,
                   tofchar("PGFONT="),
                   tofchar("Give font 1..4:        [2]") );
   if (font > 4) font = 4;
   if (font < 1) font = 1;
   pgscf_c( &font );                         /* Set font. */

   /* xtick is world coordinate interval between major tick marks    */
   /* on X axis. If xtick=0.0, the interval is chosen by PGBOX, so   */
   /* that there will be at least 3 major tick marks along the axis. */
   /* nxsub is the number of subintervals to divide the major        */
   /* coordinate interval into. If xtick=0.0 or nxsub=0, the number  */
   /* is chosen by PGBOX.                                            */
   /* BCNSTV :                                                       */
   /* B: draw bottom (X) or left (Y) edge of frame.                  */
   /* C: draw top (X) or right (Y) edge of frame.                    */
   /* N: write Numeric labels in the conventional location below     */
   /*    the viewport (X) or to the left of the viewport (Y).        */
   /* S: draw minor tick marks (Subticks).                           */
   /* T: draw major Tick marks at the major coordinate interval.     */
   /* V: orient numeric labels Vertically. This is only applicable   */
   /*    to Y.                                                       */
   xtick = ytick = 0.0;
   nxsub = nysub = 0;
   pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
            tofchar("BCNSTV"), &ytick, &nysub );

   /* Create titles */

   fmake( Xtitle, 80 ); fmake( Ytitle, 80 ); fmake( Toptitle, 80);


   len1 = subsetstring( Setin, subset, axnum, message, &phys );
   Xtitle.l = sprintf( Xtitle.a, "%.*s, %s", nelc_c(Setin), Setin.a,
                       message );
   len1 = subsetstring( Setin2, subset2, axnum2, message, &phys );
   Ytitle.l = sprintf( Ytitle.a, "%.*s, %s", nelc_c(Setin2), Setin2.a,
                       message );
   Toptitle = tofchar("Scatter diagram with regression lines");
   pglab_c( Xtitle, Ytitle, Toptitle );
}


static void putid( void )
/*------------------------------------------------------------*/
/* Create string with user name and date and plot it at the   */
/* left side of the (last) plot.                              */
/*------------------------------------------------------------*/
{
   fchar     Idstr;
   float     disp, coord, fjust;
   float     newheight, oldheight;


   pgqch_c( &oldheight );
   newheight = oldheight/1.4;
   pgsch_c( &newheight );
   fmake( Idstr, STRLEN );
   getusernam_c( Idstr );
   (void) sprintf( message, "%.*s", nelc_c( Idstr ), Idstr.a );
   getdate_c( Idstr );
   sprintf( message, "%.*s %.*s", strlen(message), message,
            nelc_c( Idstr ), Idstr.a );
   disp  = 3.0;
   coord = 0.5;
   fjust = 0.5;
   pgmtxt_c( tofchar("R"), &disp, &coord, &fjust, tofchar(message) );
   pgsch_c( &oldheight );
}




MAIN_PROGRAM_ENTRY
{
   float    fjust;
   float    angle;
   float    Xpl, Ypl;
   float    newheight, oldheight;
   float    deltaY;


   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    task;                /* Name of current task */
      fmake( task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( task );                    /* Get task name */
      task.a[nelc_c(task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( task.a, RELEASE );   /* Show task and version */
   }

   setfblank_c( &blank );
   fmake( Setin, STRLEN );
   dfault  = NONE;
   subdim  = 0;
   showdev = 3;
   nsubs   = gdsinp_c( Setin, subin, &maxsubs, &dfault, KEY_INSET, MES_INSET,
                       &showdev, axnum, axcount, &maxaxes,
                       &class, &subdim );
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
      for (m = 0; m < (int) setdim; m++) 
      {
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
   gdsbox_c( blo, bhi, Setin, subin, &dfault,
             KEY_BOX, MES_BOX, &showdev, &boxopt );



   range[0] = -1.0*FLT_MAX;            /* Defined in float.h */
   range[1] = FLT_MAX;
   dfault   = REQUEST;
   getrange_c( range, &dfault, tofchar("RANGE="), MES_RANGE );


   fmake( Setin2, STRLEN );
   dfault  = NONE;
   showdev = 3;
   do {
      nsubs2   = gdsinp_c( Setin2, subin2, &maxsubs, &dfault, KEY_INSET2, MES_INSET2,
                       &showdev, axnum2, axcount2, &maxaxes,
                       &class, &subdim );
      agreed = (nsubs2 == nsubs);
      if (!agreed) 
      {
         reject_c( KEY_INSET2, tofchar("Wrong # subsets!") );
      } 
      else 
      {
         /*-----------------------------------------------------*/
         /* Check if frame of this set is greater than or equal */
         /* to the box of the previous set.                     */
         /*-----------------------------------------------------*/
         fint cwlo, cwhi;                          /* Local coordinate words */
         int  m;
         r1 = 0;
         gdsc_range_c( Setin2, &setlevel, &cwlo, &cwhi, &r1 );
         r1 = r2 = 0;
         for (m = 0; m < (int) subdim; m++) 
         {
            flo2[m] = gdsc_grid_c( Setin2, &axnum2[m], &cwlo, &r1 );
            fhi2[m] = gdsc_grid_c( Setin2, &axnum2[m], &cwhi, &r2 );
            agreed = ( agreed && ( (fhi2[m] - flo2[m]) >= (bhi[m] - blo[m]) ) );
         }
         if (!agreed) 
            reject_c( KEY_INSET2, tofchar("Set too small for box!") );
      }
   } while (!agreed);


   /*--------------------------------------------------------*/
   /* Prepare a box for INSET2. if the values of blo and bhi */
   /* are contained in the second set, take these values as  */
   /* defaults.                                              */
   /*--------------------------------------------------------*/

   inside = YES;
   for (i = 0; i < (int) subdim; i++) 
   {
      inside = (inside && (flo2[i] >= blo[i]) && (fhi2[i] <= bhi[i]) );
   }
   if (inside) 
   {
      boxopt = 2 + 4;                     /* Default is the box of set 1 */
      for (i = 0; i < (int) subdim; i++) 
      {
         blo2[i] = blo[i]; bhi2[i] = bhi[i];
      }
   } 
   else 
   {
      boxopt  = 4;                        /* Default is the size in bhi */
      for (i = 0; i < (int) subdim; i++) 
         bhi2[i] = bhi[i] - blo[i] + 1;
   }
   showdev = 3;
   dfault  = REQUEST;
   do 
   {
      gdsbox_c( blo2, bhi2, Setin2, subin2, &dfault,
                KEY_BOX2, MES_BOX2, &showdev, &boxopt );
      agreed = YES;
      for (i = 0; i < (int) subdim; i++) 
         agreed = ( agreed && ( (bhi[i] - blo[i]) == (bhi2[i] - blo2[i]) ) );

      if (!agreed) 
         reject_c( KEY_BOX2, tofchar("Unequal box size!") );
   } 
   while (!agreed);


   range2[0] = -1.0*FLT_MAX;            /* Defined in float.h */
   range2[1] = FLT_MAX;
   dfault    = REQUEST;
   getrange_c( range2, &dfault, tofchar("RANGE2="), MES_RANGE );



   initplot();
   for(subnr = 0; subnr < nsubs; subnr++) 
   {
      cwlo = gdsc_fill_c( Setin, &subin[subnr], blo );
      cwhi = gdsc_fill_c( Setin, &subin[subnr], bhi );
      tid  = 0;
      cwlo2 = gdsc_fill_c( Setin2, &subin2[subnr], blo2 );
      cwhi2 = gdsc_fill_c( Setin2, &subin2[subnr], bhi2 );
      tid2  = 0;
      first = YES;
      Xmin = blank; Xmax = blank;
      Ymin = blank; Ymax = blank;
      status_c( tofchar("Plotting data...") );
      do 
      {
         /* This loop finds the min, max of the data you are using */
         gdsi_read_c( Setin, &cwlo, &cwhi, image,
                      &maxIObuf, &pixelsread, &tid );
         gdsi_read_c( Setin2, &cwlo2, &cwhi2, image2,
                      &maxIObuf, &pixelsread, &tid2 );
         clipper_c( &range[1], &range[0],
                    image, image, image, &pixelsread, &blank );
         clipper_c( &range2[1], &range2[0],
                    image2, image2, image2, &pixelsread, &blank );
         minmax1_c( image,  &pixelsread, &xmin, &xmax );
         minmax1_c( image2, &pixelsread, &ymin, &ymax );
         if (Xmin == blank) 
         {
            if (xmin != blank) 
               Xmin = xmin;
         } 
         else 
         {
            if (xmin != blank) 
               Xmin = MYMIN( xmin, Xmin );
         }
         if (Ymin == blank) 
         {
            if (ymin != blank) 
               Ymin = ymin;
         } 
         else 
         {
            if (ymin != blank) 
               Ymin = MYMIN( ymin, Ymin );
         }
         if (Xmax == blank) 
         {
            if (xmax != blank) 
               Xmax = xmax;
         } 
         else 
         {
            if (xmax != blank) 
               Xmax = MYMAX( xmax, Xmax );
         }
         if (Ymax == blank) 
         {
            if (ymax != blank) 
               Ymax = ymax;
         } 
         else 
         {
            if (ymax != blank) 
               Ymax = MYMAX( ymax, Ymax );
         }
      } 
      while (tid != 0);
      plotbox( Xmin, Ymin, Xmax, Ymax, subin[subnr], subin2[subnr],
               axnum, axnum2 );
      tid = tid2 = 0;
      symbol = -1;                                            /* dots in plot */
      first = YES;
      ndata = 0;
      do 
      {
         gdsi_read_c( Setin, &cwlo, &cwhi, image,
                      &maxIObuf, &pixelsread, &tid );
         gdsi_read_c( Setin2, &cwlo2, &cwhi2, image2,
                      &maxIObuf, &pixelsread, &tid2 );
         clipper_c( &range[1], &range[0],
                    image, image, image, &pixelsread, &blank );
         clipper_c( &range2[1], &range2[0],
                    image2, image2, image2, &pixelsread, &blank );
         r1 = sums( image, image2, pixelsread,
                    &sumX, &sumY, &sumXX, &sumXY,&sumYY, &first );
         nitems = 1;
         /* Intercept blanks */
         jj = 0;
         for (i = 0; i < (int) pixelsread; i++ ) 
         {
            if ( (image[i] != blank) && (image2[i] != blank) ) 
            {
               x[jj] = image[i]; y[jj] = image2[i];
               jj++;
            } 
            else 
            {
               pgpt_c( &jj, x, y, &symbol );
               jj = 0;
            }
            if (i == (pixelsread-1)) 
               pgpt_c( &jj, x, y, &symbol );
         }
      } 
      while (tid != 0);

      status_c( tofchar("Calculating statistics") );

      /* Display results */

      Xpl    = Xmin;
      deltaY = fabs(Ymax-Ymin) / 20.0;
      Ypl    = Ymin - 7.0 * deltaY;
      angle = 0.0;
      fjust = 0.0;
      pgqch_c( &oldheight );
      newheight = oldheight/1.6;
      pgsch_c( &newheight );

      anyoutC( 3," ");
      anyoutC( 3,"Statistics" );
      anyoutC( 3,"==========" );
      sprintf( message, "Data points diagram  : n = %d", r1 );
      anyoutC( 3, message );
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

      (void) linreg( r1, &sumX, &sumY, &sumXX, &sumXY,&sumYY,
                     &M, &B, &Rlin, &sigM, &sigB, &variance );
      (void) sprintf( message, "Regression Y on X (1): Y = %+f (+/-%f) X %+f (+/-%f)",
                      M, fabs(sigM), B, fabs(sigB) );

      anyoutC( 3, message );
      Ypl -= deltaY;
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

      (void) sprintf( message, "Stand. dev. residuals: %f", sqrt(fabs(variance)) );
      anyoutC( 3, message );
      Ypl -= deltaY;
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

      /* Draw this line */

      xl = MYMAX( xl, Xmin );

      xl = Xmin;
      yl = M*Xmin + B;
      if (yl > Ymax) 
      {
         yl = Ymax;
         xl = (yl - B) / M;
      }
      if (yl < Ymin) 
      {
         yl = Ymin;
         xl = (yl - B) / M;
      }
      pgmove_c( &xl, &yl );
      xl = Xmax;
      yl = M*Xmax + B;
      if (yl > Ymax) 
      {
         yl = Ymax;
         xl = (yl - B) / M;
      }
      if (yl < Ymin) 
      {
         yl = Ymin;
         xl = (yl - B) / M;
      }
      pgdraw_c( &xl, &yl );

      pgptxt_c( &xl, &yl, &angle, &fjust, tofchar(" (1)") );


      (void) linreg( r1, &sumY, &sumX, &sumYY, &sumXY, &sumXX,
                     &M, &B, &Rlin, &sigM, &sigB, &variance );
/*      {
         float err;
         err = sqrt( sigB*sigB/M/M + (B*sigM/M/M)*(B*sigM/M/M) );
         sprintf( message, "Regression X on Y    : Y = %+f (+/-%f) X %+f (+/-%f)",
                            1.0/M, fabs(sigM/(M*M)), -B/M, err );
      }
*/
      (void) sprintf( message, "Regression X on Y    : X = %+f (+/-%f) Y %+f (+/-%f)",
                      M, fabs(sigM), B, fabs(sigB) );
      anyoutC( 3, message );
      Ypl -= deltaY;
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

      (void) sprintf( message, "Stand. dev. residuals: %f", sqrt(fabs(variance)) );
      anyoutC( 3, message );
      Ypl -= deltaY;
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

      yl = Ymin;
      xl = M * Ymin + B;
      if (xl > Xmax) 
      {
        xl = Xmax;
        yl = (xl - B) / M;
      }
      if (xl < Xmin) 
      {
         xl = Xmin;
         yl = (xl - B) / M;
      }
      pgmove_c( &xl, &yl );
      yl = Ymax;
      xl = M * Ymax + B;
      if (xl > Xmax) 
      {
        xl = Xmax;
        yl = (xl - B) / M;
      }
      if (xl < Xmin) 
      {
         xl = Xmin;
         yl = (xl - B) / M;
      }
      pgdraw_c( &xl, &yl );

      (void) sprintf( message, "Linear corr. coeff.  : r = %f", Rlin );
      anyoutC( 3, message );

      Ypl -= deltaY;
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

      prob = 100.0 * probability( Rlin, r1);
      anyoutC( 3, "Probability that     ");
      anyoutC( 3, "Parent Distribution  ");
      sprintf( message, "is uncorrelated      : Pc(r,n) = %e(%)", prob );
      if (prob >= 5.0) 
         strcat( message, " ( Not significant )" );
      else if (prob <= 0.1) 
         strcat( message, " ( Very highly significant )" );
      else if (prob <= 1.0) 
         strcat( message, " ( Highly significant )" );
      else if (prob <= 5.0) 
         strcat( message, " ( Significant )" );
      anyoutC( 3, message );

      pgqch_c( &oldheight );
      putid();
   }                                             /* End loop over all subsets */
   
  
   pgend_c();
   finis_c();
   return(1);   /* Dummy return */
}
