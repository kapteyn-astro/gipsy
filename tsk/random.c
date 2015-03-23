/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             random.dc1

Program:       RANDOM

Purpose:       Generate random numbers from various distributions.
               Add noise to sets. Write random numbers to an (ascii)
               file on disk.

Category:      UTILITY, ANALYSIS, PLOTTING

File:          random.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give input set (, subsets):         [GENERATE R.N. ONLY]
               Maximum number of subsets is 2048.
               R.N. == Random Numbers. The default switches to a mode
               where no input/output sets are used. Then, only a plot
               of the selected distribution is created.

   BOX=        Give box in (.....)                      [entire subset]
               Select area to work on for both input-
               and output set.
               Only prompted if an input set is selected.

   OUTSET=     Give output set (, subsets):
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets.
               Only prompted if an input set is selected.

** FILENAME=   Name of ASCII file:                  [No output to file]
               Keyword is not asked if an input set is selected.
               If a name is specified, an ASCII file is created to
               store all the random numbers that you generate on disk
               until you end the program.. If you press carriage return,
               there will be no output to an ASCII file. If a given name
               already exists on disk, APPEND= must be specified.

   APPEND=     File exists, ok to append?                        [Y]/N
               The file specified in FILENAME= already exists.
               You can append to this file with APPEND=Y. If APPEND=N
               you will be prompted for another filename.

   GRDEVICE=   Plot device:                           [List of devices]
               Destination of plot, Screen or Hardcopy.


   OPTION=     Type of distribution 1..6:                           [1]
               Select a distribution for your random numbers.
               1 -- Gaussian:     exp(-0.5*x**2)
               2 -- Sech2:        1/cosh^2(x)
               3 -- Exponential:  exp(-|x|)
               4 -- Lorentz:      1/(1+x^2)
               5 -- Rectangular:  -1 < x < 1
               6 -- Gaussian:     Alternative method

               Options 1 to 5 all use the function 'randev'. Both
               algorithms for random number generator and
               Gaussian deviates are described in 'Numerical Recipes'.

               Option 6 is an numerical integration of white Gaussian
               noise, using the system random number generator 'rand()'


               The generated random numbers can be scaled with the
               formula: RNnew = RNold*sigma - mean.

   MEAN=       Give mean of distribution:                         [0.0]
               The peak of the distribution will be centered at
               this value.

   SIGMA=      Give characteristic width of distribution:         [1.0]

   ISEED=      For options 1..5:
               Seed (should be negative):                          [-1]

               Else:
               Seed:                                           [123456]

               Start generating a sequence of random numbers by
               giving a integer seed.

   ADDNUM=     Add numbers to original data:                      Y/[N]
               The input set is copied to the output set and the
               generated random numbers are added.
               The default is the creation of a set containing the
               random numbers.
               Only prompted if an input set is selected.

   MAKEPLOT=   Make histogram of generated values:                Y/[N]
               (if a set was given), or:

               Make histogram of generated values:                [Y]/N
               (without a set)

               Create a histogram of the generated values with
               NBINS= bins. For OPTION=1..5 also a calculated curve
               is plotted.
               Only prompted if an input set is selected. Otherwise
               a histogram is always plotted.

               Plots are made in a loop. The following keywords are
               asked in that loop:

   NBINS=      Give number of histogram bins:                     [101]
               In the next runs:
               Give number of histogram bins:               [STOP LOOP]

   RANGE=      If a set is used:
               Range in X values:                          [calculated]
               If no set is used:
               Range in X values:                                [-5 5]

   MAXRAN=     Give max. random numbers:                         [10^6]
               If no set was selected, give the number of
               random numbers.


** TIMER=      Do time test on RNG:                               Y/[N]
               Determine number of cpu seconds the RNG needs to
               generate the given amount of numbers.


               KEYWORDS RELATED TO PLOTTING:

** PGMOSAIC=   View surface sub divisions in x,y:                 [1,1]
               View surface can contain a number of plots in
               in X and Y direction (mosaic). Default is 1 plot in
               both X- and Y direction.

** PGPAPER=    Give width(cm), aspect ratio:               [calc, calc]
               Aspect ratio is height/width.

** PGBOX=      Corners of box Xl,Yl,Xh,Yh:     [default by application]
               It is possible to overrule the calculated
               PGPLOT box size with PGBOX=. The coordinates (x,y) of
               the lower point are given first.

** PGCOLOR=    Give color 1..15:                                    [1]
               See description for the available colors.

** PGWIDTH=    Give line width 1..21:                               [2]

** PGHEIGHT=   Give character height:                             [1.0]

** PGFONT=     Give font 1..4:                                      [2]


Description:   For Monte Carlo computer work or any sort of stochastic
               modeling, a reliable source of random uniform deviates
               is needed. Uniform deviates are random numbers that lie
               within a specified range. With these numbers one can
               generate random numbers that are drawn from a distribu-
               tion. This program provides the selection of 4 distribu-
               tions. It is possible to make an output set with noise
               or you can add noise to your input sets (INSET=,
               OUTSET=, ADDNUM=). The program can also operate without
               sets. You can write the generated numbers to an ASCII-
               file (FILENAME=). The program can make a plot of the
               distribution of the random numbers (MAKEPLOT=).
               Also a timer is build in. This timer determines the
               cpu time that is needed to generate a given number of
               random deviates (only). In this way, the program can
               be used as a test program for generating random deviates
               by including other algorithms. You can include the timer
               with the hidden keyword TIMER=Y. With the OPTION=
               keyword, you select your the distribution. The options
               1,2,3 and 4 generate random deviates drawn from the
               corresponding distribution. Option 5 generates uniform
               deviates. All options 1..5 are using the subroutine
               'randev' based on algorithms that can be found in
               Numerical Recipes. Option 6, like option 1, also draws
               numbers from a Gaussian distribution, but it uses
               an algorithm described in 'The Science of Fractal Images'
               by Peitgen and Saupe and the system RNG rand() which
               turns out to be slower than the RNG in 'randev'. So,
               option 6 is not for practical use, but it is more a
               programmers template for alternative routines.
               Option 7 is the RNG that COMBIN (and other applications
               that call 'fiedo') uses.

               The random numbers are all scaled using the values in
               MEAN= and SIGMA= according to:

                           RNnew = RNold*sigma - mean

               For a Gaussian distribution, this results in a Gauss
               with peak at MEAN= and sigma equal to SIGMA=
               For the uniform distribution, it results in uniform
               distributed random numbers between mean-sigma and
               mean+sigma.


               The generated random numbers are binned and a histogram
               is plotted. A curve, representing the selected
               distribution, is also plotted. The curve is scaled to
               have the same area under the curve as the histogram.
               This way, you are able to compare results and theory.
               The plots are made in a loop. In this loop you can
               vary the number of bins in the histograms that represents
               the selected distribution with NBINS=. The number of
               random numbers can be changed with MAXRAN= (unless you
               used a set, then the amount of numbers is fixed to the
               box size, BOX=). The program scales the plot height
               automatically, but the X-range can be modified with
               RANGE=. If no output set was used, the program cannot
               know exactly what the range in values will be and
               substitutes a reasonable default.

               To be able to generate the same sequence of random
               numbers twice, you can specify the same seed with
               ISEED=.


               PGPLOT: Color indices:

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

               Available fonts:

               1  single stroke "normal" font
               2  roman font
               3  italic font
               4  script font

Notes:         .......

Example:       .......

Updates:       Aug  5, 1994: VOG, Document created.
               Feb  1, 2000: JPT, Increased number of subsets.

#<
*/

/*  random.c: include files     */

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
#include    "randev.h"
#include    "iran.h"
#include    "presetr.h"
#include    "minmax1.h"
#include    "timer.h"
#include    "getdate.h"      /* Returns the current time and date as a text string */


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
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
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
#include    "gdsout.h"       /* GDSOUT prompts the user to enter the */
                             /* name of an output set and the subsets, */
                             /* and returns the number of subsets entered. */
#include    "gdsi_write.h"   /* Writes data to (part of) an set. */


/* PGPLOT includes */

#include    "pgplot.h"


/* Function evaluation routines */

#include    "fiepar.h"
#include    "fiedo.h"
#include    "fieini.h"


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
#define MAXBUF         4096            /* Buffer size for I/O */
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0
#define MAXRANDOMS     1000000
#define MAXOPTIONS     7
#define NUMBINS        101
#define GAUSSIAN       1
#define SECH2          2
#define EXPONENTIAL    3
#define LORENTZ        4
#define RECTANGULAR    5
#define GAUSSIAN2      6
#define GAUSSIAN3      7



/* Colors */

#define BACKGROUND    0
#define WHITE         1
#define RED           2
#define GREEN         3
#define BLUE          4
#define CYAN          5
#define MAGENTA       6
#define YELLOW        7
#define ORANGE        8
#define GREENYELLOW   9
#define GREENCYAN    10
#define BLUECYAN     11
#define BLUEMAGENTA  12
#define REDMAGENTA   13
#define DARKGRAY     14
#define LIGHTGRAY    15




/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):   [GENERATE RN. ONLY]")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set (subset(s)): ")
#define KEY_NBINS      tofchar("NBINS=")
#define KEY_RANGE      tofchar("RANGE=")
#define KEY_MAXRAN     tofchar("MAXRAN=")
#define KEY_TIMER      tofchar("TIMER=")


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

static fint     cwlo, cwhi;         /* Coordinate words. */
static fint     tid;                /* Transfer id for read function. */
static fint     maxIObuf = MAXBUF;  /* Maximum size of read buffer. */
static float    image[MAXBUF];      /* Buffer for read routine. */


/* OUTSET related variables */

static fchar    Setout;
static fint     subout[MAXSUBSETS];  /* Output subset coordinate words */
static fint     nsubsout;
static fint     axnumout[MAXAXES];
static fint     axcountout[MAXAXES];
static fint     cwloO, cwhiO;        /* Output Coordinate words. */
static fint     tidO;                /* Transfer id for write function. */


/* Random number generator: */

static int      Nrand;              /* Number of samples of ran0() to be taken in Gauss() */
static int      Arand;              /* ran0() returns values between 0 and Arand, */
                                    /* system dependent */
static double   GaussAdd;           /* real parameter for the linear transformation in Gauss() */
static double   GaussFac;           /* real parameter for the linear transformation in Gauss() */


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
static fint     mcount;             /* Initialize MINMAX3. */
FILE            *asciifile;         /* File pointer to ASCII file */
static int      plotopen;
static fint     iseed;
static fint     fieid;
static float    fiearray[MAXBUF];


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


static void movexy( float x, float y )
/*---------------------------------------*/
/* Alternative pgmove                    */
/*---------------------------------------*/
{
   pgmove_c( &x, &y );
}


static void drawxy( float x, float y )
/*---------------------------------------*/
/* Alternative pgdraw                    */
/*---------------------------------------*/
{
   pgdraw_c( &x, &y );
}



FILE *openfile( fchar Key, fchar Mes, fint dfault, char *filename,
                char mode )
/*-------------------------------------------------------------*/
/* Key       Keyword                                           */
/* Mes       Message                                           */
/* dfault    Default for user                                  */
/* filename  Default filename, if length equals 0 then the     */
/*           default means no file pointer returned            */
/* mode      character r or w for read/write in fopen C        */
/*           function                                          */
/*                                                             */
/* Open file for writing/reading. Ask filename in GIPSY way    */
/* Check file for existence. Return file pointer and the name  */
/* of the given file. The function introduces the keyword      */
/* APPEND= for 'write' files that already exist.               */
/* If APPEND=N the existing file will be overwritten.          */
/*-------------------------------------------------------------*/
{
   bool      append;
   fint      nitems = 1;
   fint      agreed;
   fint      n;
   FILE     *fp;
   bool      readmode, writemode;
   int       yes = 1, no = 0;
   char      filebuf[132];
   fchar     Filename;
   bool      nodeffile;


   Filename.a = filebuf;
   Filename.l = 132;
   readmode  = ('R' == toupper(mode) );
   writemode = ('W' == toupper(mode) );
   nodeffile = (strlen(filename) == 0);
   if (readmode) {
      do {
         n = usertext_c( Filename, &dfault, Key, Mes );
         if (n == 0) {
            if (nodeffile) return(NULL);
         } else {
            strcpy( filename, strtok(Filename.a, " ") );/* Delete after space */
         }
         fp = fopen(filename, "r");
         if (fp == NULL) {
            reject_c( Key, tofchar("Cannot read file") );
            if (dfault >= 2) dfault = 1;
            nodeffile = yes;
            Mes = tofchar( "Try another file name:" );
         }
      } while (fp == NULL);
      return( fp );
   }
   if (writemode) {
      do {
         n = usertext_c( Filename, &dfault, Key, Mes );
         if (n == 0) {
            if (nodeffile) return(NULL);
         } else {
            strcpy( filename, strtok(Filename.a, " ") );/* Delete after space */
         }
         fp = fopen(filename, "r");
         cancel_c( Key );
         if (fp != NULL) {       /* File exists */
            append = toflog( no );
            n   = userlog_c( &append,
                             &nitems,
                             &dfault,
                             tofchar("APPEND="),
                             tofchar("File exists, append?   Y=append/[N=overwrite]") );
            append = tobool( append );
            fclose( fp );
            cancel_c( tofchar("APPEND=") );
            if (append) {
               fp = fopen(filename, "a");
               agreed = (fp != NULL);
               if (!agreed) {
                  reject_c( Key,
                            tofchar("Cannot open for appending, try another!") );
               } else {
                  return( fp );
               }
            } else {
               fp = fopen(filename, "w");
               agreed = (fp != NULL);
               if (!agreed) {
                  reject_c( Key,
                            tofchar("Cannot open for writing, try another!") );
               } else {
                  return( fp );
               }
            }
         } else {
            /* File does not exist */
            fp = fopen(filename, "w");
            agreed = (fp != NULL);
            if (!agreed) {
               reject_c( Key,
                         tofchar("Cannot open for writing, try another!") );
            } else {
               return( fp );
            }
         }
      } while (!agreed);
   }
   return( NULL );                /* Return NULL if not write or read mode */
}


static int initplot( void )
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
   dfault = HIDDEN;
   r1 = userint_c( nxysub,
                   &nitems,
                   &dfault,
                   tofchar("PGMOSAIC="),
                   tofchar("View surface sub divisions in x,y:   [1,1]") );

   unit = 0;
   Devspec = tofchar("?");
   r1 = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );
   if (r1 != 1) {
      error_c( &errlev, tofchar("Cannot open output device") );
      return( 0 );
   }

   /* No PGPLOT's NEXTPAGE= keyword */
   pageoff = toflog( 0 );
   pgask_c( &pageoff );

   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */

   nitems   = 2;
   dfault   = HIDDEN;
   paper[0] = 0.0;
   paper[1] = 1.0;
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
   xl = 0.2; xr = 0.95;
   yb = 0.1; yt = 0.9;
   pgsvp_c( &xl, &xr, &yb, &yt );

   return( 1 );
}


void drawbox( float Xmin, float Ymin, float Xmax, float Ymax,
              char *xtitle, char *ytitle, char *toptitle )
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
   fint   dfault;
   float  pg_box[4];                          /* Corners of draw box. */
   fint   color;
   fint   font;
   fint   nxsub, nysub;
   float  xtick, ytick;
   fchar  Xtitle, Ytitle, Toptitle;
   char   message[80];


   pgpage_c();                                /* Advance to new page. */

   /* Increase the size of the box a little */
   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Xmin -= delta; Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Ymin -= delta; Ymax += delta;
   pg_box[0] = Xmin; pg_box[1] = Ymin;        /* Get size from user input */
   pg_box[2] = Xmax; pg_box[3] = Ymax;
   nitems = 4; dfault = HIDDEN;
   sprintf( message, "Corners of box Xl,Yl, Xh,Yh:  [%f,%f,%f,%f]", Xmin,Ymin,Xmax,Ymax );
   r1 = userreal_c( pg_box,
                    &nitems,
                    &dfault,
                    tofchar("PGBOX="),
                    tofchar( message ) );
   Xmin = pg_box[0]; Ymin = pg_box[1];
   Xmax = pg_box[2]; Ymax = pg_box[3];
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );   /* Set the window */

   color = 1; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &color,
                   &nitems,
                   &dfault,
                   tofchar("PGCOLOR="),
                   tofchar("Give color 1..15:        [1]") );
   if (color > 15) color = 15;
   if (color < 1 ) color =  1;
   pgsci_c( &color );

   lwidth = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &lwidth,
                   &nitems,
                   &dfault,
                   tofchar("PGWIDTH="),
                   tofchar("Give line width 1..21:        [2]") );
   if (lwidth > 21) lwidth = 21;
   if (lwidth < 1 ) lwidth =  1;
   pgslw_c( &lwidth );                  /* Set line width. */

   charsize = 1.0; nitems = 1; dfault = HIDDEN;
   r1 = userreal_c( &charsize,
                    &nitems,
                    &dfault,
                    tofchar("PGHEIGHT="),
                    tofchar("Give character height:     [1.0]") );
   pgsch_c( &charsize );               /* Character height. */

   font = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &font,
                   &nitems,
                   &dfault,
                   tofchar("PGFONT="),
                   tofchar("Give font 1..4:        [2]") );
   if (font > 4) font = 4;
   if (font < 1) font = 1;
   pgscf_c( &font );                   /* Set font. */

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
   Xtitle = tofchar("X title"); Ytitle = tofchar("Y title");
   Toptitle = tofchar("Top title");
   pglab_c( tofchar(xtitle), tofchar(ytitle), tofchar(toptitle) );
}



static void InitGauss( int seed, int Nrand, int Arand )
/*--------------------------------------------*/
/* Initialization of random number generators */
/*--------------------------------------------*/
{
   double n = (double) Nrand;
   GaussAdd = sqrt(3.0 * n);
   GaussFac = 2.0 * GaussAdd/(n * (double)Arand);
   srand(seed);
}


static double Gauss( void )
/*-------------------------------------------*/
/* Function returning Gaussian random number */
/*-------------------------------------------*/
{
   double    sum;
   register  int i;

   sum = 0.0;
   for (i = 0; i < Nrand; i++)
      sum += (double) rand();                      /* 'rand()' returns an int */
      /* sum += (double) iran_c( &iseed );*/
   return( GaussFac * sum - GaussAdd );
}


static double combinrand( void )
/*--------------------------------------------------------------*/
/* Fill a buffer with random numbers, using the RNG of 'fiedo'. */
/* The counter 'i' is used to check whether the buffer must be  */
/* filled again by calling 'fiedo'.                             */
/*--------------------------------------------------------------*/
{
   static int i = 0;
   fint   r;
   fint   maxnum = MAXBUF;
   double res;

   if (i == 0)
      r = fiedo_c( fiearray, &maxnum, fiearray, &fieid );
   res = fiearray[i];
   i = ++i % MAXBUF;
   return( (float) res );
}


static int getindex( fint maxbins, float number,
                     float minval, float maxval, int *indx )
/*--------------------------------------------------------------*/
/* Given a random number and a range, this routine calculates   */
/* an array index between 0 and < maxbins. Only 'maxval' will   */
/* result in the index 'maxbins'. This index is decreased by 1. */
/*--------------------------------------------------------------*/
{
   float res;
   if (number == blank) return( 0 );
   if (number > maxval) return( 0 );
   if (number < minval) return( 0 );
   res = (float)maxbins * (number-minval)/(maxval-minval);
   *indx = (int) res;
   if (number == maxval) (*indx)--;
   return( 1 );
}


static float getrandom( fint option, double mean, double sigma )
/*--------------------------------------------------------------*/
/* Get a random number. Distinguish options for different RNG's */
/*--------------------------------------------------------------*/
{
   if (option >= GAUSSIAN && option <= RECTANGULAR)
      return (float) ( randev_c( &option, &iseed ) * sigma + mean );
   else if (option == GAUSSIAN2)
      return (float) ( Gauss() * sigma + mean );
   else if (option == GAUSSIAN3)
      return (float) ( combinrand() * sigma + mean );
   else
      return( 0.0 );
}


static void distributioncurve( double area, double mean, double sigma,
                               fint maxbins, float binwidth,
                               float *X, fint option )
/*-----------------------------------------------------------------*/
/* Plot a distribution curve with the same sampling as the         */
/* histogram (X values). For example: the area of a Gaussian:      */
/*                 I[A*e^(-x*x/2*s*s)] = A*s*sqrt(2pi)             */
/* I = Integral, A = amplitude, s = sigma.                         */
/* This number is used to scale the amplitude of the curve. The    */
/* area is the total number of random numbers times the width of   */
/* a histogram bin. Now, the total area under the curve and the    */
/* total area of all bins are equal.                               */
/*-----------------------------------------------------------------*/
{
   double   amp = 1.0;
   double   integral = 1.0;
   fint     color;
   float    *funcval= NULL;
   register int i;


   funcval = (float *) calloc( maxbins, sizeof(float) );
   if (funcval == NULL)
   {
      anyoutC( 1, "Cannot allocate space for curve array" );
      return;
   }

   /* Calculate integral -inf, inf of the function met mean = 0 */
   /* and sigma = sigma and calculate amplitude to rescale.     */

   if (option == GAUSSIAN || option == GAUSSIAN2 || option == GAUSSIAN3)
      integral = sigma * sqrt( 2.0*PI );
   if (option == SECH2)
      integral = 2.0 * sigma;
   if (option == EXPONENTIAL)
      integral = 2.0 * sigma;
   if (option == LORENTZ)
      integral = PI * sigma;
   if (option == RECTANGULAR)
      /* Uniform */
      integral = 2.0 * sigma;

   amp = area / integral;

   for(i = 0; i < maxbins; i++)
   {
      double xx;

      X[i] +=  binwidth / 2.0;            /* Shift X position 1/2 bin */
      xx = (X[i]-mean)/sigma;
      if (option == GAUSSIAN || option == GAUSSIAN2 || option == GAUSSIAN3)
         funcval[i] = (float) (amp * exp(-1.0*xx*xx/2.0));
      else if (option == SECH2)
         funcval[i] = (float) (amp / (cosh(xx)*cosh(xx)));
      else if (option == EXPONENTIAL)
         funcval[i] = (float) (amp * exp(-1.0*fabs(xx)));
      else if (option == LORENTZ)
         funcval[i] = (float) (amp / (1.0 + xx*xx));
      else if (option == RECTANGULAR)
      {
         if (X[i] < (mean-sigma) || X[i] > (mean+sigma) )
            funcval[i] = 0.0;
         else
            funcval[i] = (float) (amp);
      }
      else
         funcval[i] = 0.0;
   }
   color = RED; pgsci_c( &color );
   movexy( mean, 0.0 );
   drawxy( mean, amp );
   if (option == GAUSSIAN || option == GAUSSIAN2 || option == GAUSSIAN3)
   {
      movexy( mean-sigma, amp*exp(-0.5) );
      drawxy( mean+sigma, amp*exp(-0.5) );
   }
   pgline_c( &maxbins, X, funcval );
   free( funcval );
}



static void histogram( fchar Setin, fint subnr,
                       fint  cwlo, fint cwhi,
                       float minval, float maxval,
                       fint  maxbins, fint option,
                       char *xtitle, char *ytitle, char *toptitle,
                       int usesets, long maxnum,
                       double mean, double sigma, bool timer,
                       fint fieid )
/*----------------------------------------------------------------------*/
/* Create a histogram of generated random numbers. Distinguish data     */
/* already written to SETOUT= and data that is generated locally.       */
/*----------------------------------------------------------------------*/
{
   fint     tid;
   fint     color;
   fint     pixelsread;
   float    maxcount, dummy;
   float    *binheights = NULL;
   float    *X = NULL;
   int      i;
   fint     center;
   int      j;
   float    binwidth;
   int      totpixels = 0;
   int      indx;
   double   area;


   /* Create space for the arrays */

   binheights = (float *) calloc( maxbins, sizeof(float) );
   if (binheights == NULL)
   {
      anyoutC( 1, "Cannot allocate space for histogram arrays" );
      return;
   }
   X = (float *) calloc( maxbins, sizeof(float) );
   if (X == NULL)
   {
      anyoutC( 1, "Cannot allocate space for histogram arrays" );
      free( binheights );
      return;
   }

   binwidth = (maxval-minval)/(float)maxbins;
   for (j = 0; j < maxbins; j++)
   {
      X[j] = minval + j * binwidth;
      binheights[j] = 0.0;
   }

   tid = 0;
   if (usesets)
   {
      do
      {
         gdsi_read_c( Setin,
                      &cwlo, &cwhi,
                      image,
                      &maxIObuf,
                      &pixelsread,
                      &tid );

         for (i = 0; i < pixelsread; i++)
         {
            if (getindex( maxbins, image[i], minval, maxval, &indx ))
            {
               binheights[indx] += 1.0;
               totpixels++;
            }
         }
      } while (tid != 0);
   }
   else
   {
      register long i;
      float    rannum;

      if (timer)
      {
         fint     elapse = 1;
         double   cputime, realtime;

         /* Time test: */
         anyoutC( 3, " " );
         anyoutC( 3, " ******* Timings for this generator ******" );
         elapse = 0;
         if (option >= GAUSSIAN && option <= RECTANGULAR)
         {
            timer_c( &cputime, &realtime, &elapse );     /* Set the cpu timer */
            for (i = 0; i < maxnum; i++)
               rannum = (float) randev_c( &option, &iseed );
         }
         if (option == GAUSSIAN2)
         {
            timer_c( &cputime, &realtime, &elapse );     /* Set the cpu timer */
            for (i = 0; i < maxnum; i++)
               rannum = (float) Gauss();
         }
         if (option == GAUSSIAN3)
         {
            timer_c( &cputime, &realtime, &elapse );     /* Set the cpu timer */
            for (i = 0; i < maxnum; i++)
               rannum = (float) combinrand();
         }
         elapse = 1;
         timer_c( &cputime, &realtime, &elapse );         /* Get the used cpu */
         (void) sprintf( message,
                        "Processed %ld ran. numbers in %.3f sec (%.3f cpu sec)",
                         maxnum, realtime, cputime );
         anyoutC( 1, message );
         anyoutC( 3, " " );
      }

      /* Create the data for histogram (and ascii file) */
      for (i = 0; i < maxnum; i++)
      {
         float  rannum;

         rannum = getrandom( option, mean, sigma );
         if (getindex( maxbins, rannum, minval, maxval, &indx ))
         {
            binheights[indx] += 1.0;
            if (asciifile)
               (void) fprintf( asciifile, "%f\n", rannum );
         }
      }
      totpixels = maxnum;
   }

   minmax1_c( binheights, &maxbins, &dummy, &maxcount );
   drawbox( minval, 0, maxval, maxcount, xtitle, ytitle, toptitle );
   center = toflog(0);
   color = YELLOW; pgsci_c( &color );
   pgbin_c( &maxbins, X, binheights, &center );


   /*---------------------------------------------*/
   /* Plot a scaled gaussian curve.               */
   /*---------------------------------------------*/

   /* What is the total area under the curve? */
   area = (double) totpixels * (double) binwidth;

   distributioncurve( area, mean, sigma, maxbins, binwidth, X, option );

   free( binheights );
   free( X );
}


MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint   nitems;
   fint   dfault;
   fint   option;                          /* Select distribution for R.N's */
   float  rannum;                          /* A random number */
   bool   addnumbers;                      /* Add random numbers to image */
   bool   makeplot;                        /* Always if no sets are used */
   bool   timer;                           /* Time the RNG (cpu sec) */
   fint   nbins;                           /* Number of histogram bins */
   char   toptitle[80];                    /* Title for top of histogram */
   int    usesets;                         /* Using sets, or just generating numbers */
   int    i;
   static long   maxnum = MAXRANDOMS;
   double mean, sigma;
   fint   subnr;                           /* Counter for subset loop. */

   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );
   fmake( Setin, STRLEN );
   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );


   /*-----------------------------------------------------------*/
   /* Ask user name of set, but allow option NOT using a set.   */
   /*-----------------------------------------------------------*/
   subdim     = 0;
   showdev    = 3;
   Key        = KEY_INSET;
   Mes        = MES_INSET;
   dfault     = REQUEST;
   Setin.a[0] = '\0';
   r1 = usertext_c( Setin, &dfault, Key, Mes );
   if (r1 == 0)
      usesets = NO;
   else
   {
      usesets = YES;
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
   }

   if (usesets)
   {
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
         if (!agreed)  reject_c( KEY_OUTSET, tofchar("#out != #in") );
      } while (!agreed);
   }


   /*------------------------------------------------------*/
   /* Writing random numbers to ASCII file opened with     */
   /* routine 'openfile'                                   */
   /*------------------------------------------------------*/
   if (!usesets)
   {
      char   writefile[256];

      writefile[0] = '\0';     /* <CR> in 'openfile' results in asciifile=NULL */
      dfault       = HIDDEN;
      asciifile = openfile( tofchar("FILENAME="),                  /* Keyword */
                            tofchar("File name for ASCII data:   [NO FILE]"),
                            dfault,                                /* default */
                            writefile,                        /* name of file */
                            'w' );                              /* write mode */
      if (asciifile != NULL)
      {
         fchar Idstr;
         fmake( Idstr, 256 );
         (void) sprintf( message, "ASCII data to disk in [%s]", writefile );
         anyoutC( 3, message );
         getdate_c( Idstr );
         (void) fprintf( asciifile,
                        "Random Numbers: %.*s\n",
                         nelc_c(Idstr), Idstr.a );
      }
   }

   /*-------------------------------------------------------*/
   /* Present a menu and ask user to select an distribution */
   /*-------------------------------------------------------*/

   anyoutC( 1, " " );
   anyoutC( 1, "   =======================================");
   anyoutC( 1, "     1 -- Gaussian:     exp(-0.5*x**2)");
   anyoutC( 1, "     2 -- Sech2:        1/cosh^2(x)");
   anyoutC( 1, "     3 -- Exponential:  exp(-|x|)");
   anyoutC( 1, "     4 -- Lorentz:      1/(1+x^2)");
   anyoutC( 1, "     5 -- Rectangular:  -1 < x < 1 ");
   anyoutC( 1, "     6 -- Gaussian:     Linear method");
   anyoutC( 1, "     7 -- Gaussian:     As used in COMBIN");
   anyoutC( 1, "   =======================================");
   anyoutC( 1, " " );

   nitems = 1;
   dfault = REQUEST;
   option = 1;
   (void) sprintf( message, "Type of distribution 1..%d:      [%d]",
                   MAXOPTIONS, option );
   r1 = userint_c( &option, &nitems, &dfault,
                   tofchar("OPTION="),
                   tofchar( message ) );
   if (option > MAXOPTIONS)
      option = MAXOPTIONS;
   if (option < 1)
      option = 1;

   toptitle[0] = '\0';
   (void) sprintf( message, "You selected: " );
   if (option == GAUSSIAN)
      strcpy( toptitle, "1 -- Gaussian:     exp(-0.5*x**2)");
   if (option == SECH2)
      strcpy( toptitle, "2 -- Sech2:        1/cosh^2(x)");
   if (option == EXPONENTIAL)
      strcpy( toptitle, "3 -- Exponential:  exp(-|x|)");
   if (option == LORENTZ)
      strcpy( toptitle, "4 -- Lorentz:      1/(1+x^2)");
   if (option == RECTANGULAR)
      strcpy( toptitle, "5 -- Rectangular:  -1 < x < 1 ");
   if (option == GAUSSIAN2)
      strcpy( toptitle, "6 -- Gaussian:     Alternative method");
   if (option == GAUSSIAN3)
      strcpy( toptitle, "7 -- Gaussian:     As used in COMBIN");

   strcat( message, toptitle );
   anyoutC( 3, message );

   nitems = 1;
   dfault = REQUEST;
   mean   = 0.0;
   (void) sprintf( message, "Give mean of distribution:     [%g]", mean );
   r1 = userdble_c( &mean, &nitems, &dfault,
                    tofchar("MEAN="),
                    tofchar( message ) );

   nitems = 1;
   dfault = REQUEST;
   sigma  = 1.0;
   (void) sprintf( message, "Give sigma of distribution:     [%g]", sigma );
   r1 = userdble_c( &sigma, &nitems, &dfault,
                    tofchar("SIGMA="),
                    tofchar( message ) );
   sigma = fabs( sigma );

   if (option == GAUSSIAN2)
   {
      iseed = 123456;
      (void) sprintf( message, "Seed:         [%d]", iseed );
   }
   else
   {
      iseed = -1;
      (void) sprintf( message, "Seed (should be negative):      [%d]", iseed );
   }

   if (option != GAUSSIAN3)
   {
      /* The RNG of 'fiedo' does not need a seed */
      r1 = userint_c( &iseed, &nitems, &dfault,
                      tofchar("ISEED="),
                      tofchar( message ) );
   }

   if (option == GAUSSIAN2)
   {
      Nrand = 3.0;
      Arand = RAND_MAX;
      /* If iran_c is involved, use: Arand = (int) pow( 2.0, 24.0 ) - 1;*/
      InitGauss( iseed, Nrand, Arand );
      anyoutC( 1, "The random number generator that is used, returns");
      (void) sprintf( message, "(on this machine) values between 0 and %d",
                      Arand );
      anyoutC( 1, message );
   }

   if (option == GAUSSIAN3)
   {
      fint err;
      r1 = fieini_c( tofchar("RANG(0.0,1.0)"), &fieid, &err);
      if (r1 < 0)
      {
         anyoutC( 1, "Could not initialize RNG" );
         finis_c();
      }
   }


   addnumbers = NO;
   if (usesets)
   {
      (void) sprintf( message, "Add numbers to original data:    Y/[N]" );
      r1 = userlog_c( &addnumbers, &nitems, &dfault,
                      tofchar("ADDNUM="),
                      tofchar( message ) );
      addnumbers = tobool( addnumbers );
   }

   plotopen = NO;

   if (usesets)
   {
      makeplot = NO;
      (void) sprintf( message,
                     "Make histogram of generated values:    Y/[N]" );
   }
   else
   {
      makeplot = YES;
      (void) sprintf( message,
                     "Make histogram of generated values:    [Y]/N" );
   }
   nitems = 1;
   dfault = REQUEST;

   makeplot = toflog( makeplot );
   r1 = userlog_c( &makeplot, &nitems, &dfault,
                   tofchar("MAKEPLOT="),
                   tofchar( message ) );
   makeplot = tobool( makeplot );

   if (!plotopen && makeplot)
      plotopen = initplot();
   else if (plotopen && !makeplot)
      pgend_c();


   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/

   if (!usesets)                               /* Number of subsets is 0 then */
      nsubs = 1;

   for(subnr = 0; subnr < nsubs; subnr++)
   {
      fint     pixelsread;         /* Number of pixels read by read routine. */
      fint     pixelswrite;        /* Number of pixels to write to output. */
      if (usesets)
      {
         cwlo   = gdsc_fill_c( Setin, &subin[subnr], blo );
         cwhi   = gdsc_fill_c( Setin, &subin[subnr], bhi );
         /* Use input grid coordinates, but connect to output subsets */
         cwloO  = gdsc_fill_c( Setout, &subout[subnr], blo );
         cwhiO  = gdsc_fill_c( Setout, &subout[subnr], bhi );
         mcount = 0;
         tidO   = 0;
         tid    = 0;
         do
         {
            /* Read 'maxIObuf' values in 'image'. */
            gdsi_read_c( Setin,
                         &cwlo, &cwhi,
                         image,
                         &maxIObuf,
                         &pixelsread,
                         &tid );

            for (i = 0; i < pixelsread; i++)
            {
               if (image[i] != blank)
               {
                  rannum = getrandom( option, mean, sigma );
                  if (addnumbers)
                     image[i] += rannum;
                  else
                     image[i] = rannum;
               }
            }
            /* Calculate running min, max & blanks of output */
            minmax3_c( image,
                       &pixelsread,
                       &minval[subnr], &maxval[subnr],
                       &nblanks[subnr],
                       &mcount );
            pixelswrite = pixelsread;
            /* Write 'pixelswrite' values from 'image' to output. */
            gdsi_write_c( Setout,
                          &cwloO, &cwhiO,
                          image,
                          &maxIObuf,
                          &pixelswrite,
                          &tidO );
         } while (tid != 0);
      }

      if (makeplot)
      {
         int first = YES;
         nbins = NUMBINS;
         do
         {
            /*---------------------------------------*/
            /* Ask user number of bins of histogram. */
            /*---------------------------------------*/
            dfault = REQUEST;
            nitems = 1;
            if (first)
            {
               nbins = NUMBINS;
               (void) sprintf( message,
                              "Give number of histogram bins:      [%d]",
                               nbins);
               first = NO;
            }
            else
            {
               nbins = -1;
               (void) sprintf( message,
                              "Give number of histogram bins:  [STOP LOOP]");
            }
            r1 = userint_c( &nbins, &nitems, &dfault, KEY_NBINS,
                            tofchar( message ) );
            cancel_c( KEY_NBINS );

            if (nbins > 0)
            {
               /*-------------------------------------------------------*/
               /* If a set was selected, the random numbers are already */
               /* generated so we know the minimum and maximum values   */
               /* i.e. the range of the x-axis of the histogram. If no  */
               /* set was selected, we use two fixed values.            */
               /*-------------------------------------------------------*/
               float minmax[2];
               if (usesets)
               {
                  minmax[0] = minval[subnr];
                  minmax[1] = maxval[subnr];
               }
               else
               {
                  float start = 1.0;
                  if (option == 5)
                     start = 1.0;
                  else
                     start = 5.0;
                  minmax[0] = -start * sigma + mean;
                  minmax[1] =  start * sigma + mean;
               }
               (void) sprintf( message, "Range in X values:  [%g %g]",
                               minmax[0], minmax[1] );
               nitems = 2;
               dfault = REQUEST;
               r1     = userreal_c( minmax, &nitems, &dfault,
                                    KEY_RANGE,
                                    tofchar( message ) );
               cancel_c( KEY_RANGE );

               /*-----------------------------------------------------*/
               /* Without a known box, we have to no how many numbers */
               /* have to be generated.                               */
               /*-----------------------------------------------------*/
               if (!usesets)
               {
                  nitems = 1;
                  dfault = REQUEST;
                  (void) sprintf( message,
                                 "Give max. random numbers:   [%ld]",
                                  maxnum );
                  r1 = userint_c( &maxnum, &nitems, &dfault,
                                  KEY_MAXRAN,
                                  tofchar( message ) );
                  cancel_c( KEY_MAXRAN );
               }

               nitems = 1;
               dfault = HIDDEN;
               (void) sprintf( message, "Do time test on RNG:           Y/[N]");
               timer  = toflog( NO );
               r1     = userlog_c( &timer, &nitems, &dfault,
                                   KEY_TIMER,
                                   tofchar( message ) );
               timer  = tobool( timer );

               histogram( Setout, subout[subnr], cwloO, cwhiO,
                          minmax[0], minmax[1], nbins, option,
                         "Random number intervals", "counts", toptitle,
                          usesets, maxnum, mean, sigma, timer, fieid );
            }
         } while (nbins > 0);
      }
   }

   if (usesets)
   {
      fint change;                        /* Used in WMINMAX. change!=0 means */
                                      /* minimum and maximum have changed and */
                                            /* that the MINMAX descriptors at */
                                      /* intersecting levels will be removed. */
      /* Update OUTSET= descriptor with new values */
      change = YES;
      wminmax_c( Setout, subout,
                 minval, maxval, nblanks,
                 &nsubsout,
                 &change );
   }
   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   if (asciifile)
      fclose( asciifile );
   if (plotopen)
      pgend_c();
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
