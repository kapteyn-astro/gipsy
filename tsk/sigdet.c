/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             sigdet.dc1

Program:       SIGDET (SIGnal DETection)

Purpose:       Separate signal from noise in image.

Category:      PROFILES, MANIPULATION,, TRANSFER

File:          sigdet.c

Author:        M. Vogelaar
               (written by: Foppe Leistra and Gert Cazemier)

Keywords:

   INSET=      Give input set. The set must have 3 axes.
               Maximum number of subsets is 2048. If some frequenties
               are not wanted, replace them with blanks with
               COMBIN or EDITSET.

   BOX=        Give box in .....                        [entire subset]
               Box must be given for all 3 dimensions.

   OUTSET=     Give output set (, subsets):
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets.

   OKAY=       Will overwrite data, okay ?                          [Y]
               When the outset already exists this keyword asks
               if it is ok to overwrite the outset.

   DIRECTIONS= Give directions(1, 2 and/or 3):                       []
               The directions that will be handled.
               Directions are 1,2 and 3.

   TIMES=      Give times:                                          [3]
               A pixel is indicated as signal, only when there are
               more then TIMES directions, where it was found as
               signal.

   WINDOW=     Give: minimum window length:                         [3]
               This keyword specifies the minimum window length.
               A profile contains signal only if this signal has a
               minimum length of WINDOW=.

   USELEVEL=   Use level:                                           [N]
               The program can calculate the signal-border. But a
               cut-off level can also be used. This keyword asks if
               a cut-off level must be used. The cut-off level can be
               given with the keyword WEIGHTx=, where x is the axis
               number.

   LEVEL=      Give level :                                          []
               When a cut-off level is used, the level can be given
               with this keyword.

   NSIGMA=     Give cut-off in number times sigma            [number=3]
               When no cut-off level is used, the signal-border is
               calculated as follows:
               mean + NSIGMA= * sigma.
               (horizontal yellow line in GIDS)

   SMOOTH=     Give smoothing length:                               [0]
               The size of the smoothing mask can be computed as:
               2 * SMOOTH= + 1. So when a mask of size 7 is used,
               you give SMOOTH= 3.
               See for more information at METHOD=, DIRECTIONS=
               and WEIGHTx=.

   REFINEMENT= Use refinement:                                      [Y]
               The signal can be refined at the beginning and the
               ending of the found signal. The values that are next
               to the signal are also indicated as signal, if they
               are between mean and signal-border.
               This keyword asks if refinement must be used.

   METHOD=     Give smoothing option:                               [0]
               The mask must be filled with weights.
               This can be done with:
               - 0: gauss distribution
               - 1: hanning distribution (0.25 0.5 0.25)
               - 2: weights given by the user (keyword WEIGHTx=).

   WEIGHTx=    Give weight:
               When the smoothing method is 2 (weights given by the
               user) then values can be given by this keyword.
               This is for axis x.

   SHOW=       Ask to show next profile?                            [Y]

   GRDEVICE=   Graphics device           [list of all graphics devices]
               Destination of plot: Screen, Hardcopy or Null.
               When the profile is send to GIDS, the colors have the
               following meaning:
               cyan:                   Original profile
               blue:                   Smoothed profile
               yellow:                 Noise peak in this axis
               red:                    The signal that is found in
                                       this axis
               horizontal yellow line: The signal-border or cut-off
                                       level
               horizontal green line:  The mean of this profile

** TOLERANCE=  Give the stop condition:                           [0.05]
               The loop, to determine what is signal and what not,
               stops when:  previous mean - mean < TOLERANCE=

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


EXAMPLES:
1: To determine the values of the keywords
2: When you have determined the 'correct' keywords

Example 1:
SIGDET  INSET=       M101
        BOX=         -10 -10 1 -7 -7 59     Analysis of a box.
        OUTSET=      M101.WINDOW
        OKAY=        Y                      Rewrite outset
        GRDEVICE=    GIDS                   Output to Gids
        DIRECTIONS=  1 2 3                  All axis
        TIMES=       2                      2 of the 3 axis must be found
                                            as signal
        REFINEMENT=  N Y Y                  No refinement in the first axis
        WINDOW=      3 3 3                  Minimum window must be 3
        SMOOTH=      5 5 5                  Size of smoothing mask is 2*5+1
        METHOD=      0 1 2                  Smooth is filled with:
                                            axis 1: Gauss distribution
                                            axis 2: Hanning distribution
                                            axis 3: Own weights(see WEIGHT3)
        WEIGHT3=     1 2 3 4 7 9 7 4 3 2 1  Weights for axis 3
        USELEVEL=    N Y N                  Use cut-off level for axis 2
        NSIGMA=      3 3                    Signal-border for axis 1 and 2
        LEVEL=       2.5                    Cut-off level for axis 2 is 2.5
        SHOW=        Y                      Now you have time to analyse a
                                            profile

Example 2:
SIGDET  INSET=       M101
        BOX=                                Complete set and subsets
        OUTSET=      M101.WINDOW
        OKAY=        Y                      Rewrite outset
        GRDEVICE=    NULL                   No output to gids
        SHOW=        N                      No waiting after every profile
        DIRECTIONS=  1 2 3                  All axis
        TIMES=       1                      In only 1 axis has to be signal
        REFINEMENT=  N Y Y                  No refinement in the first axis
        WINDOW=      3 3 3                  Minimum window must be 3
        SMOOTH=      5 5 5                  Size of smoothing mask is 2*5+1
        METHOD=      0 0 1                  Smooth is filled with:
                                            axis 1: Gauss distribution
                                            axis 2: Gauss distribution
                                            axis 3: Hanning distribution
        USELEVEL=    N N N                  Calculate signal-border for each
                                            profile
        NSIGMA=      3 3 3                  Signal-border for axis 1, 2 and 3


DESCRIPTION:
  The program can find signal in an image. The image must have 3 dimensions.
  To determine the signal the program handles the image, profile after
  profile. Profiles can be read from all 3 directions. The keyword
  TIMES= indicates in how many directions signal must be found at one position
  before it finally will be signal in the outset.

Window:
  The window-method is used to find the signal. This method will now be
  explained:
  We use a mask to indicate which channel of a profile is signal,
  noise or noise_peak. To make distinguish between signal and noise, we
  first calculate the mean and sigma of the profile. This calculation is
  done with profile values which have the status noise in the mask.
  The signal_border is computed. This is mean plus NSIGMA= times sigma.
  If WINDOW= channels successively have a greater intensity then the
  signal_border, then it is indicated as signal in the mask. When there
  are less than WINDOW= channels successively greater than the signal_border,
  and when the intensity is greater than two times the
  signal_border it is indicated as noise_peak. If a channel is neither signal
  or noise_peak, it's called noise. Previous steps are iterated until
  (old_mean-mean) < TOLERANCE=.

  All channels with the value blank are ignored.

Cut-off:
  It is also possible to use a Cut-off level. The parameter USELEVEL=
  asks for each axis if you want to use a cut-off level. When a cut-off level
  is used for a axis, the parameter LEVEL asks this value.

Smooth:
  The signal can also be smoothed. This can be done with the keyword SMOOTH=.
  This keyword asks for all axis the size of the smoothing. When you give 3
  with this keyword the smoothing size will be 2*3+1=7. You can smooth with
  the following distributions:
  0 - Gauss distribution:
                                               2
                                     ( z - mu )
                  1             -0.5 (--------)                  mu + 1
   f(z) = ------------------  e      (  sigma )      sigma = ----------------
           sigma sqrt( 2pi)                                   sqrt(12 ln 10)

      Sigma is calculated, so that the position just outside the smooth mask
      has the value 0.000001.

  1 - Hanning distribution
  2 - Own distribution (with the keyword WEIGHTx=, x is the axis number)

Updates:       Jun 1993: FLE & GRC, Document created.
               Aug 1993: Olaf Kolkman, Some changes and additions.
               Feb 1, 2000: JPT, Increased number of subsets.

#<
*/

/*  window.c: include files     */

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
#include    "ctype.h"        /* Declares ANSI C functions for testing char[6~acters */
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
                             /* value for each axis.*/
#include    "gdsc_name.h"    /* Name of axis. */
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


#include    "stabar.h"       /* Status bar */
#include    "timer.h"

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

#define MYMAX(a,b)	( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)	( (a) > (b) ? (b) : (a) )
#define NINT(a)		( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)		( (a) < 0 ? (-(a)) : (a) )
#define BETWEEN(a,b,c)	( (a) >= (b) && (a) <= (c) )
#define PI		3.141592653589793
#define E		2.71828182845
#define RAD(a)		( a * 0.017453292519943295769237 )
#define DEG(a)		( a * 57.295779513082320876798155 )

#define RELEASE        "1.0"           /* Version number */
#define MAXAXES        3               /* Max. axes in a set */
#define MAXSUBSETS     2048            /* Max. allowed subsets */
#define MAX_BUF        50
#define SMOOTH_MAX     5
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0

#define NOISE          0               /* Indecises noise and masker */
#define NOISE_PEAK     1
#define SIGNAL         2
#define BLANK          3

#define	MAX_ITERATIONS		10
#define SIGMA_FACTOR		1.0	/* otherwise the main loop ends    */
                                        /* immediate                       */
					/* origininal factor was 0.3	   */
					/* but that gave trouble 	   */
					/* increased to 1.0 by O.M.Kolkman */
#define	POS_NOISEPEAK_FACTOR	 2.0	/* when value is NEG_NOISEPEAK_FACTOR */
#define	NEG_NOISEPEAK_FACTOR	 2.0	/* above border and not signal we    */
					/* define a noise peak		     */
					/* added by O.M. Kolkman             */

#define MIN_TOTAL_SIGMA		 0.00001   /* a change by O.M. Kolkman      */

#define AXIS0			 0	/* The axes             */
#define AXIS1			 1
#define AXIS2			 2

#define LO			 0	/* Low, high and length  */
#define HI			 1
#define LEN			 2

#define INSET			 0
#define BOX			 1
#define BUF_RD			 2
#define BUF_WRT			 3
#define WEIGHT			 4

#define TOTAL_IMAGE_READ	 0
#define OK			 1

#define	SQRT_TWO_PI		 sqrt(2 * PI)

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set:")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set: ")

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
                                    /* ones contain the the number of grid[Bs along */
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

static fint     f[2][MAXAXES];      /* Edge of frame in grids */
static fint     boxopt;             /* The different options are: */
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */

/* Reading data */

static fint     cw[2];              /* Coordinate words. */
static fint     tid;                /* Transfer id for read function. */
static fint     maxIObuf;           /* Maximum size of read buffer. */
static fint     pixelsread;         /* Number of pixels read by read routine. */

static float    *buffer;            /* Buffer for read routine. */
static float	*buffer_write;
static fint	size[5][3][3];      /* Sizes of arrays:                        */
                                    /* First dimension:    INSET               */
                                    /*                     BOX                 */
                                    /*                     BUF_RD              */
                                    /*                     BUF_WRT             */
                                    /*                     WEIGHT              */
                                    /* Second dimension:   LO                  */
                                    /*                     HI                  */
                                    /*                     LEN                 */
                                    /* Third dimension:    AXIS0               */
                                    /*                     AXIS1               */
                                    /*                     AXIS2               */

static  int	X,Y,Z;              /* X,y,z directions in buffer              */

static  fint     subnr;              /* Counter for subset loop. */

static	int	mask[MAXSUBSETS];
static	float	profile[MAXSUBSETS];
static	float	original_profile[MAXSUBSETS];

static	fint	smooth[] = {1, 1, 1};	/* smooth sizes in AXIS0, AXIS1, AXIS2 */
static  float   weight[3][SMOOTH_MAX*2+1];

static	float	bar_max,
		bar_min,
		current;

static  char	axis[20+1] = {"                   "};
static  int     x,y,z;

static  int     ndirections;

/* OUTSET related variables */

static fchar    Setout;
static fint	nsubsout;
static fint	axnumout[MAXAXES];
static fint	axcountout[MAXAXES];
static fint     subout[MAXSUBSETS];  /* Output subset coordinate words */

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
static fint  symbol      =  2;      /* Take a plus as plot symbol, see PGPLOT MANUAL */
static fint  color;

/* Miscellaneous */

static fchar    Key, Mes;
static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[120];       /* All purpose character buffer. */
static float    minval[MAXSUBSETS]; /* Min. value of data for each subset. */
static float    maxval[MAXSUBSETS]; /* Max. value of data for each subset. */

double cpu_time;
double real_time;                 /* timers */
fint mode=0;

bool plot;

/*============================================================================*/
/*= anyoutC                                                                  =*/
/*============================================================================*/

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

/*============================================================================*/
/*= initplot                                                                 =*/
/*============================================================================*/

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
   if (r1 != 1) error_c( &errlev, tofchar("Cannot open output device") );

   /* No PGPLOT's NEXTPAGE= keyword */
   pageoff = toflog( 0 );
   (void) pgask_c( &pageoff );

   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */
   nitems = 2; dfault = HIDDEN;
   paper[0] = 0.0; paper[1] = 1.0;
   r1 = userreal_c( paper,
                    &nitems,
                    &dfault,
                    tofchar("PGPAPER="),
                    tofchar("Give width(cm), aspect ratio: [calculated]") );
   if (r1 > 0) {
      /* If width = 0.0 then the program will select the largest view surface */
      width  = paper[0] / 2.54;      /* Convert width to inches. */
      aspect = paper[1];
      (void) pgpap_c( &width, &aspect );
   }

   /* Set viewport */
   xl = 0.2; xr = 0.95;
   yb = 0.1; yt = 0.9;
   (void) pgsvp_c( &xl, &xr, &yb, &yt );
}

/*============================================================================*/
/*= axis_name                                                                =*/
/*============================================================================*/

void axis_name(fint axis_number, char *axis)
/*----------------------------------------------------------------------------*/
/*  Get the axis name from the header-file.                                   */
/*    Axis_number    - The axis number (0..2).                                */
/*    axis           - The string with the axis-name                          */
/*----------------------------------------------------------------------------*/
{
  fchar  Faxis;
  fint   err = 0;
  int    n;

  axis_number++;                            /* Axis num. always start with 1! */
  Faxis.a = axis; Faxis.l = 20; axis[20] = '\0';
  gdsc_name_c( Faxis, Setin, &axis_number, &err );
  n = 19;
  while ((axis[n] == ' ') && (n > 0))
    axis[n--] = '\0';
}

/*============================================================================*/
/*= drawbox                                                                  =*/
/*============================================================================*/

void drawbox( float Xmin, float Ymin, float Xmax, float Ymax )
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
   char   axis_x[20+1];
   char   axis_y[20+1];

   (void) pgpage_c();                         /* Advance to new page. */

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

   (void) pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );   /* Set the window */

   color = 1; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &color,
                   &nitems,
                   &dfault,
                   tofchar("PGCOLOR="),
                   tofchar("Give color 1..15:        [1]") );
   if (color > 15) color = 15;
   if (color < 1 ) color =  1;
   (void) pgsci_c( &color );

   lwidth = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &lwidth,
                   &nitems,
                   &dfault,
                   tofchar("PGWIDTH="),
                   tofchar("Give line width 1..21:        [2]") );
   if (lwidth > 21) lwidth = 21;
   if (lwidth < 1 ) lwidth =  1;
   (void) pgslw_c( &lwidth );                  /* Set line width. */

   charsize = 1.0; nitems = 1; dfault = HIDDEN;
   r1 = userreal_c( &charsize,
                    &nitems,
                    &dfault,
                    tofchar("PGHEIGHT="),
                    tofchar("Give character height:     [1.0]") );
   (void) pgsch_c( &charsize );               /* Character height. */

   font = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &font,
                   &nitems,
                   &dfault,
                   tofchar("PGFONT="),
                   tofchar("Give font 1..4:        [2]") );
   if (font > 4) font = 4;
   if (font < 1) font = 1;
   (void) pgscf_c( &font );                   /* Set font. */

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
   (void) pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
                   tofchar("BCNSTV"), &ytick, &nysub );

   /* Create titles */

   fmake( Xtitle, 80 ); fmake( Ytitle, 80 ); fmake( Toptitle, 80);
   Xtitle = tofchar(axis); Ytitle = tofchar("Intencity");

   axis_name(X, axis_x);
   axis_name(Y, axis_y);
   sprintf(message,"PROFILE: [%s: %d, %s: %d]",axis_x, size[BUF_WRT][LO][X]+x,
                                               axis_y, size[BUF_WRT][LO][Y]+y);
   Toptitle = tofchar(message);
   (void) pglab_c( Xtitle, Ytitle, Toptitle );
}

/*============================================================================*/
/*= start_timer                                                              =*/
/*============================================================================*/

void start_timer()
/*----------------------------------------------------------------------------*/
/*  This procedure starts the timer. The total duration is displayed at the   */
/*  end of the program.                                                       */
/*----------------------------------------------------------------------------*/
{
  mode = 0;
  timer_c( &cpu_time, &real_time, &mode );
}

/*============================================================================*/
/*= gettimer                                                                 =*/
/*============================================================================*/

double gettime()
/*----------------------------------------------------------------------------*/
/*  Get the time for total duration.                                          */
/*    return - time.                                                          */
/*----------------------------------------------------------------------------*/
{
  mode = 1;
  timer_c( &cpu_time, &real_time, &mode );
  return(real_time);
}

/*============================================================================*/
/*= gauss                                                                    =*/
/*============================================================================*/

float gauss(float z,int direction,float *sigma)
/*----------------------------------------------------------------------------*/
/*  Get the gauss-distribution at position z for filling the smoothing        */
/*  weights. Sigma is calculated, so that the value of the position just      */
/*  outside the smoothing 0.000001 is.                                        */
/*    z         - The position for calculating the gauss value.               */
/*    direction - The direction for which the gauss is calculated.            */
/*    sigma     - The sigma for this gauss-distribution.                      */
/*    return    - The calculated gauss value.                                 */
/*----------------------------------------------------------------------------*/
{
  float mu    = smooth[direction];

  *sigma = (mu+1) / sqrt(12*log(10));
  return(
          (1 / ( *sigma *  SQRT_TWO_PI ) ) *

          pow(E, -0.5 * pow ( (z - mu) / *sigma , 2 ))
        );
}

/*============================================================================*/
/*= hanning                                                                  =*/
/*============================================================================*/

float hanning(float z,int direction)
/*----------------------------------------------------------------------------*/
/*  Calculate the hanning distribution for the smoothing values.              */
/*  (0.25 0.5 0.25) The value is calculated for position z.                   */
/*    z         - The position for calculating the hanning value.             */
/*    direction - The direction for which the hanning is calculated.          */
/*    return    - The hanning value.                                          */
/*----------------------------------------------------------------------------*/
{
  if (z <= smooth[direction] )
    return(z+1);
  else  /* Spiegelen */
    return( weight[direction][smooth[direction]-( (int)z-smooth[direction] ) ] );
}

/*============================================================================*/
/*= read_buf                                                                 =*/
/*============================================================================*/

void read_buf(int axis_index)
/*----------------------------------------------------------------------------*/
/*  This procedure reads the first buffer for this direction form disk into   */
/*  memory. All the axes sizes gets there values.                             */
/*    axis_index   - axis counter.                                            */
/*----------------------------------------------------------------------------*/
{
   cw[LO]   = gdsc_fill_c( Setin, &subin[subnr], size[BUF_RD][LO]);
   cw[HI]   = gdsc_fill_c( Setin, &subin[subnr], size[BUF_RD][HI]);

   subnr  = 0;
   tid    = 0;

   /* Read 'maxIObuf' values in 'image'. */
   (void) gdsi_read_c( Setin,
                       &cw[LO], &cw[HI],
                       buffer,
                       &maxIObuf,
                       &pixelsread,
                       &tid );

   size[BUF_WRT][LO][X]   = size[BUF_RD][LO][X];
   size[BUF_WRT][HI][X]   = size[BUF_RD][HI][X];
   size[BUF_WRT][LEN][X]  = size[BUF_RD][LEN][X];

   size[BUF_WRT][LO][Y]   = size[BUF_RD][LO][Y];
   size[BUF_WRT][HI][Y]   = size[BUF_RD][HI][Y];
   size[BUF_WRT][LEN][Y]  = size[BUF_RD][LEN][Y];

   size[BUF_WRT][LO][Z]   = size[BUF_RD][LO][Z];
   size[BUF_WRT][HI][Z]   = size[BUF_RD][HI][Z];
   size[BUF_WRT][LEN][Z]  = size[BUF_RD][LEN][Z];

   if (axis_index != 0) {
     tid = 0;
     cw[LO]   = gdsc_fill_c( Setout, &subout[subnr], size[BUF_WRT][LO]);
     cw[HI]   = gdsc_fill_c( Setout, &subout[subnr], size[BUF_WRT][HI]);

     (void) gdsi_read_c( Setout,
                         &cw[LO],  &cw[HI],
                         buffer_write,
                         &maxIObuf,
                         &pixelsread,
                         &tid );
  }
}

/*============================================================================*/
/*= write_buf                                                                =*/
/*============================================================================*/

void write_buf()
/*----------------------------------------------------------------------------*/
/*  Write the buffer from memory to disk.                                     */
/*----------------------------------------------------------------------------*/
{
   fint    pixelswrite;

   cw[LO]   = gdsc_fill_c( Setout, &subout[subnr], size[BUF_WRT][LO]);
   cw[HI]   = gdsc_fill_c( Setout, &subout[subnr], size[BUF_WRT][HI]);

   pixelswrite = size[BUF_WRT][LEN][X] * size[BUF_WRT][LEN][Y] * size[BUF_WRT][LEN][Z];
   tid=0;
   (void) gdsi_write_c( Setout,
                        &cw[LO], &cw[HI],
                        buffer_write,
                        &maxIObuf,
                        &pixelswrite,
                        &tid );
}

/*============================================================================*/
/*= init_buf                                                                 =*/
/*============================================================================*/

void init_buf(int axis_index)
/*----------------------------------------------------------------------------*/
/*  Initialise the buffer parameters and reads the first buffer.              */
/*    axis_index - Axis counter.                                              */
/*----------------------------------------------------------------------------*/
{
  size[BUF_RD][LEN][X] = MYMIN(MAX_BUF, size[BOX][HI][X] - size[BOX][LO][X] + 1);
  size[BUF_RD][LEN][Y] = MYMIN(MAX_BUF, size[BOX][HI][Y] - size[BOX][LO][Y] + 1);
  size[BUF_RD][LEN][Z] = size[BOX][HI][Z] - size[BOX][LO][Z] + 1;

  size[BUF_RD][LO][X]  = size[BOX][LO][X];
  size[BUF_RD][LO][Y]  = size[BOX][LO][Y];
  size[BUF_RD][LO][Z]  = size[BOX][LO][Z];

  size[BUF_RD][HI][X]  = size[BOX][LO][X] + size[BUF_RD][LEN][X] - 1;
  size[BUF_RD][HI][Y]  = size[BOX][LO][Y] + size[BUF_RD][LEN][Y] - 1;
  size[BUF_RD][HI][Z]  = size[BOX][LO][Z] + size[BUF_RD][LEN][Z] - 1;

  /* Initialize buffers */
  maxIObuf     = MAX_BUF * MAX_BUF * size[BUF_RD][LEN][Z];

  buffer       = (float *) calloc( maxIObuf , sizeof(float *) );
  buffer_write = (float *) calloc( maxIObuf , sizeof(float *) );

  if(buffer == NULL || buffer_write == NULL) {
    anyoutC(3,"Buffers: Out of memory!");
    exit(1);
    }

  (void) read_buf(axis_index);
}

/*============================================================================*/
/*= finit_buf                                                                =*/
/*============================================================================*/

void finit_buf()
/*----------------------------------------------------------------------------*/
/*  Give the memory used for the buffers free.                                */
/*----------------------------------------------------------------------------*/
{
  free(buffer);
  free(buffer_write);
}

/*============================================================================*/
/*= position                                                                 =*/
/*============================================================================*/

long position(int buf_io,int x, int y, int z)
/*----------------------------------------------------------------------------*/
/*  Translate the 3-dimensional positions to an 1-dimensional position in     */
/*  the buffer array.                                                         */
/*    buf_io    - The buffer number (read or write buffer).                   */
/*    x, y, z   - The 3-dimensional positions.                                */
/*    return    - The 1-dimensional position in buffer.                       */
/*----------------------------------------------------------------------------*/
{
  switch (Z) {
    case AXIS0: return(z +
                       y * size[buf_io][LEN][AXIS0] +
                       x * size[buf_io][LEN][AXIS0] * size[buf_io][LEN][AXIS1]);

    case AXIS1: return(x +
                       z * size[buf_io][LEN][AXIS0] +
                       y * size[buf_io][LEN][AXIS0] * size[buf_io][LEN][AXIS1]);

    case AXIS2: return(x +
                       y * size[buf_io][LEN][AXIS0] +
                       z * size[buf_io][LEN][AXIS0] * size[buf_io][LEN][AXIS1]);
  }
  return( 0 );
}

/*============================================================================*/
/*= get_profile                                                              =*/
/*============================================================================*/

void get_profile(fint  x,
                 fint  y,
                 float *mean,
                 float *sigma)
/*----------------------------------------------------------------------------*/
/*  Get the profile from the buffer. If smoothing is used, the profile is     */
/*  smoothed. Mean and sigam for all points in this profile are calculated.   */
/*    x, y   - The profile position.                                          */
/*    mean   - The calculated mean.                                           */
/*    sigma  - Sigma.                                                         */
/*----------------------------------------------------------------------------*/
{
  static fint  z;
  int    z1;
  float  total_mean   = 0;
  float  total        = 0;
  float  total_weight = 0;
  float  total_sigma  = 0;
  int    nmean        = 0;			/* number of mean */
  int    min,max;

  minval[subnr]=0;
  maxval[subnr]=0;

  for(z = 0; z < size[BUF_WRT][LEN][Z]; z++) {
    total        = 0;
    total_weight = 0;
    min = MYMAX( 0, z-smooth[Z]);
    max = MYMIN( size[BUF_WRT][LEN][Z]-1, z+smooth[Z]);

    for(z1 = min; z1 <= max; z1++)
      if (buffer[position(BUF_RD,x,y,z1)] != blank) {
        total        += buffer[position(BUF_RD,x,y,z1)] * weight[Z][z1-(z-smooth[Z])];
        total_weight += weight[Z][z1-(z-smooth[Z])];
      }

    if (total_weight != 0)
       profile[z] = total / total_weight;
    else
      profile[z] = total;

    original_profile[z] = buffer[position(BUF_RD,x ,y ,z )];

    if(original_profile[z] != blank) {
      if (original_profile[z] > maxval[subnr]) maxval[subnr] = original_profile[z];
      if (original_profile[z] < minval[subnr]) minval[subnr] = original_profile[z];
       }

    if(profile[z] != blank) {
      if (profile[z] > maxval[subnr]) maxval[subnr] = profile[z];
      if (profile[z] < minval[subnr]) minval[subnr] = profile[z];
      }

    if (profile[z] != blank) {
      nmean++;
      total_mean  += profile[z];
      total_sigma += pow(profile[z],2);
      }
  }

  if(nmean <= 1) {
    *sigma = blank;
    *mean = blank;
  }
  else if (total_sigma < MIN_TOTAL_SIGMA) {
  	 /* Addition by O.M. Kolkman begin */
  	sprintf(message,"WARNING total_sigma < %f while %i points are designated as noise",MIN_TOTAL_SIGMA,nmean);
  	anyoutC(3, message);
        sprintf(message,"(in PROFILE: [ %d,  %d]) ",\
         size[BUF_WRT][LO][X]+x, size[BUF_WRT][LO][Y]+y);
        anyoutC(3, message);
        sprintf(message,"Check this profile");
        anyoutC(3, message);
       /* DEBUG PART end */

      *mean = (total_mean / (float) nmean);
      *sigma = 0;
      } else {
         *sigma =  sqrt((total_sigma / (float)(nmean-1)) - (pow(total_mean,2) / (float)(nmean*(nmean-1))));
         *mean = (total_mean / (float) nmean);
  }
}

/*============================================================================*/
/*= read_next_buffer                                                         =*/
/*============================================================================*/

int read_next_buffer(int axis_index)
/*----------------------------------------------------------------------------*/
/*  Reads the next buffer into memory. It determines the positions for        */
/*  the next buffer.                                                          */
/*    axis_index  - Axis counter.                                             */
/*    return      - OK               if buffer is read.                       */
/*                  TOTAL_IMAGE_READ If all the buffers are read.             */
/*----------------------------------------------------------------------------*/
{
  (void) write_buf(axis_index);

  if(size[BUF_RD][HI][X] < (size[BOX][HI][X])) {
    /* new buffer is to the right */
    size[BUF_RD][LO][X]  = size[BUF_RD][HI][X] + 1;
    size[BUF_RD][HI][X]  = MYMIN( size[BOX][HI][X], size[BUF_RD][HI][X] + MAX_BUF);
    size[BUF_RD][LEN][X] = size[BUF_RD][HI][X] - size[BUF_RD][LO][X] + 1;
    }
  else if(size[BUF_RD][HI][Y] < size[BOX][HI][Y]) {
    /* new buffer is up */
    size[BUF_RD][LO][X] = size[BOX][LO][X];
    size[BUF_RD][HI][X] = size[BOX][LO][X] + MAX_BUF - 1;
    size[BUF_RD][LO][Y] = size[BUF_RD][HI][Y] + 1;
    size[BUF_RD][HI][Y] = size[BUF_RD][LO][Y] + MAX_BUF - 1;

    if(size[BUF_RD][HI][X] > size[BOX][HI][X])
      size[BUF_RD][HI][X] = size[BOX][HI][X];
    if(size[BUF_RD][HI][Y] > size[BOX][HI][Y])
      size[BUF_RD][HI][Y] = size[BOX][HI][Y];

    size[BUF_RD][LEN][X] = size[BUF_RD][HI][X] - size[BUF_RD][LO][X] + 1;
    size[BUF_RD][LEN][Y] = size[BUF_RD][HI][Y] - size[BUF_RD][LO][Y] + 1;
    }
  else return(TOTAL_IMAGE_READ);

  (void) read_buf(axis_index);

  return(OK);
}

/*============================================================================*/
/*= save_profile                                                             =*/
/*============================================================================*/

void save_profile(int x,
                  int y,
                  fint times,
                  int axis_index)
/*----------------------------------------------------------------------------*/
/*  Write the profile to the write buffer. All points that are not signal     */
/*  get the value blank.                                                      */
/*    x, y       - Position of profile in buffer.                             */
/*    times      - A pixel is indicated as signal, only when there are        */
/*                 more then TIMES directions, where it was found as signal.  */
/*    axis_index - Axis counter.                                              */
/*----------------------------------------------------------------------------*/
{
  int z;

  if (axis_index == 0)   /* The first direction */
    for(z = 0; z < size[BUF_WRT][LEN][Z]; z++) {
      switch (mask[z]) {
        case SIGNAL     : buffer_write[position(BUF_WRT, x ,y ,z )] = 1; break;
        case BLANK      :
        case NOISE_PEAK :
        case NOISE      : buffer_write[position(BUF_WRT, x ,y ,z )] = 0; break;
      }
    }
  else
    for(z = 0; z < size[BUF_WRT][LEN][Z]; z++) {
      switch (mask[z]) {
        case SIGNAL     : buffer_write[position(BUF_WRT, x ,y ,z )]++; break;
        case BLANK      :
        case NOISE_PEAK :
        case NOISE      : break;
      }
    }

  if (axis_index == ndirections -1) {
    for(z = 0; z < size[BUF_WRT][LEN][Z]; z++)
      if (buffer_write[position(BUF_WRT, x ,y ,z )] >= times)
        buffer_write[position(BUF_WRT, x ,y ,z )] = original_profile[z];
      else
        buffer_write[position(BUF_WRT, x ,y ,z )] = blank;
  }
}


/*============================================================================*/
/*= plot_signal                                                              =*/
/*============================================================================*/

void plot_signal(float mean,
                 float border)
/*----------------------------------------------------------------------------*/
/*  Plot profile to the GRDEVICE.                                             */
/*    mean   - Mean for plotting (green line).                                */
/*    border - Signal border (yellow line).                                   */
/*----------------------------------------------------------------------------*/
{
  fint	z,
        number = 0;

  float	line[2],
	x[2],
	xa[MAXSUBSETS];

  for (z = 0; z < size[BUF_WRT][LEN][Z]; z++)
    xa[z]=z + size[BUF_WRT][LO][Z];

  drawbox(size[BUF_WRT][LO][Z],minval[0],size[BUF_WRT][HI][Z],maxval[0]);

  color = blue;
  (void) pgsci_c( &color );
  (void) pgline_c(&size[BUF_WRT][LEN][Z], xa, profile);
  pgpt_c( &size[BUF_WRT][LEN][Z], xa, profile, &symbol );

  color = cyan;
  (void) pgsci_c( &color );
  (void) pgline_c(&size[BUF_WRT][LEN][Z], xa, original_profile);
  pgpt_c( &size[BUF_WRT][LEN][Z], xa, original_profile, &symbol );

  line[0] = border;
  line[1] = border;
  x[0]    = size[BUF_WRT][LO][Z];
  x[1]    = size[BUF_WRT][HI][Z];
  number  = 2;
  color   = yellow;
  (void) pgsci_c( &color );
  (void) pgline_c(&number, x, line);

  line[0] = mean;
  line[1] = mean;
  color=green;
  (void) pgsci_c( &color );
  (void) pgline_c(&number, x, line);

  color=red;
  (void) pgsci_c( &color );
  number = 0;

  do {
    static fint begin,aantal;

    if(number < size[BUF_WRT][LEN][Z] && mask[number] == SIGNAL) {
      begin = number;
      while((++number<size[BUF_WRT][LEN][Z]) && (mask[number]==SIGNAL));
      aantal = number - begin;
      color = red;
      (void) pgsci_c(&color);
      (void) pgline_c(&aantal,&xa[begin],&original_profile[begin]);
      (void) pgpt_c(&aantal,&xa[begin],&original_profile[begin],&symbol);
      }
    if(number < size[BUF_WRT][LEN][Z] && mask[number] == NOISE_PEAK) {
      begin = number;
      while((++number<size[BUF_WRT][LEN][Z]) && (mask[number]==NOISE_PEAK));
      aantal = number - begin;
      color = yellow;
      (void) pgsci_c(&color);
      (void) pgline_c(&aantal,&xa[begin],&original_profile[begin]);
      (void) pgpt_c(&aantal,&xa[begin],&original_profile[begin],&symbol);
      }
    if (number < size[BUF_WRT][LEN][Z] && ((mask[number] == NOISE) || (mask[number] == BLANK)))
      number++;
  } while(number < size[BUF_WRT][LEN][Z]);
}

/*============================================================================*/
/*= determine_width                                                          =*/
/*============================================================================*/

int determine_width(int   begin,
                    float border)
/*----------------------------------------------------------------------------*/
/*  Determine signal_width                                                    */
/*    begin          - Begin position in profile.                             */
/*    border         - Signal border.                                         */
/*    return         - Width of signal.                                       */
/*----------------------------------------------------------------------------*/
{
  int  z;

  z = begin;
  do
    z++;
  while( (z < size[BUF_WRT][LEN][Z]) &&
         (mask[z] != BLANK) &&
         (profile[z] > border) );

  return(z - begin);                       /* Window width                */
}

/*============================================================================*/
/*= sieve_signal                                                             =*/
/*============================================================================*/

void sieve_signal(float border,
                  float border_min,
		  fint  min_win_length,
		  float *mean,
		  float *sigma)
/*----------------------------------------------------------------------------*/
/*  Determine where signal is in profile. Calculates mean and signal          */
/*  from noise.                                                               */
/*    - border         - Signal border.                                       */
/*    - border_min     - Negative signal border.                              */
/*    - min_win_length - The minimum window length.                           */
/*    - mean           - Mean of noise.                                       */
/*    - sigma          - Sigma of noise.                                      */
/*----------------------------------------------------------------------------*/
{
  int   z, t, width,
        nmean = 0;
  float total_mean  = 0,
        total_sigma = 0;
  float noise_peak_border,
        neg_noise_peak_border;

  noise_peak_border = *mean + NEG_NOISEPEAK_FACTOR * (border - *mean);
  neg_noise_peak_border =  *mean + NEG_NOISEPEAK_FACTOR  * (border_min - *mean);
	/* border_min - *mean is negative*/




  z=0;
  do
    if (mask[z] != BLANK) {
      if (profile[z] > border) {
        width = determine_width(z, border);
        if (width >= min_win_length) {
          for(t=z; t < z+width; t++)
            mask[t] = SIGNAL;
          z=z+width-1;
          continue;
          }
      }



      if ((profile[z] > noise_peak_border) || (profile[z] < neg_noise_peak_border))
                                          /* Noise_peak only when           */
        mask[z] = NOISE_PEAK;             /* greater then 2 x border or     */
                                          /* smaller then 2 x border_min    */
      else {
        mask[z] = NOISE;
        nmean++;                        /* compute mean/sigma             */
        total_mean  += profile[z];
        total_sigma += pow(profile[z],2);
      }
    }
  while (z++ < size[BUF_WRT][LEN][Z]);

  if(nmean <= 1) {
  	/* Addition by O.M. Kolkman begin */
  	sprintf(message,"WARNING: Only noise peaks and signal in this profile");
  	anyoutC(3, message);
        sprintf(message,"(PROFILE: [ %d,  %d]) ",\
         size[BUF_WRT][LO][X]+x, size[BUF_WRT][LO][Y]+y);
        anyoutC(3, message);
        sprintf(message," NSIGMA may be to small");
        anyoutC(3, message);
       /*  end */


    *sigma = blank;
    *mean = blank;
  }
  /*  larger range of total_sigma  used to be MIN_TOTAL_SIGMA */
  else if (total_sigma < MIN_TOTAL_SIGMA) {
  	/* Addition by O.M. Kolkman begin */
  	sprintf(message,"WARNING total_sigma < %f while %i points are designated as noise",MIN_TOTAL_SIGMA,nmean);
  	anyoutC(3, message);
        sprintf(message,"(in PROFILE: [ %d,  %d]) ",\
         size[BUF_WRT][LO][X]+x, size[BUF_WRT][LO][Y]+y);
        anyoutC(3, message);
        sprintf(message,"Check this profile");
        anyoutC(3, message);
       /* DEBUG PART end */

      *mean = (total_mean / (float) nmean);
      *sigma = 0;
      } else {
          *sigma =  sqrt((total_sigma / (float)(nmean-1)) - (pow(total_mean,2) / (float)(nmean*(nmean-1))));
          *mean = (total_mean / (float) nmean);
        }
}

/*============================================================================*/
/*= refinement                                                               =*/
/*============================================================================*/

void refinement(float *mean,
		float *sigma)
/*----------------------------------------------------------------------------*/
/*  All points on both side of the signal are signal too, if they are         */
/*  greater than the mean. Sigma and mean are also calculated.                */
/*    - mean   - The mean of the noise.                                       */
/*    - sigma  - The sigma of the noise.                                      */
/*----------------------------------------------------------------------------*/
{
  int   z, bt=0,
        nmean = 0;
  float total_mean = 0,
        total_sigma = 0;

  z = 0;
  do {
    if(mask[z] == SIGNAL) {
      bt = z-1;
      while((bt >= 0) && (original_profile[bt] >= *mean)) {
        if (mask[bt] == NOISE)
        {
          nmean--;
          total_mean  -= profile[bt];
          total_sigma -= pow(profile[bt],2);
        }
        mask[bt--] = SIGNAL;
      }
      while((++z < size[BUF_WRT][LEN][Z]) && (mask[z]==SIGNAL));
      while((z < size[BUF_WRT][LEN][Z]) && (original_profile[z] >= *mean))
        mask[z++] = SIGNAL;
      if((z < size[BUF_WRT][LEN][Z]) && (mask[z] == NOISE)) {
             nmean++;
             total_mean  += profile[z];
             total_sigma += pow(profile[z],2);
           }
    }
    else if (mask[z] == NOISE) {
           nmean++;
           total_mean  += profile[z];
           total_sigma += pow(profile[z],2);
         }
  } while(++z< size[BUF_WRT][LEN][Z]);

  if(nmean <= 1) {
    *sigma = blank;
    *mean = blank;
  }
  else if (total_sigma < MIN_TOTAL_SIGMA) {
  	/* Addition by O.M. Kolkman begin */
  	sprintf(message,"WARNING total_sigma < %f while %i points are designated as noise",MIN_TOTAL_SIGMA,nmean);
  	anyoutC(3, message);
        sprintf(message,"(in PROFILE: [ %d,  %d]) ",\
         size[BUF_WRT][LO][X]+x, size[BUF_WRT][LO][Y]+y);
        anyoutC(3, message);
        sprintf(message,"Check this profile");
        anyoutC(3, message);
       /* DEBUG PART end */

      *mean = (total_mean / (float) nmean);
      *sigma = 0;
      } else {
         *sigma =  sqrt((total_sigma / (float)(nmean-1)) - (pow(total_mean,2) / (float)(nmean*(nmean-1))));
         *mean = (total_mean / (float) nmean);
  }
}

/*============================================================================*/
/*= detect_signal                                                            =*/
/*============================================================================*/

void detect_signal(float tolerance,
                   float times_sigma,
                   fint  min_win_length,
                   bool  do_refinement,
                   bool  use_level,
                   float level,
                   float mean,
                   float sigma)
/*----------------------------------------------------------------------------*/
/*  Main loop to find signal.                                                 */
/*    - tolerance       - The tolerance level for stopping iteration.         */
/*    - times_sigma     - The times of sigma for calculating signal border    */
/*                        (keyword NSIGMA=).                                  */
/*    - min_win_length  - The minimum window length (keyword WINDOW=).        */
/*    - do_refinement   - Use refinement or not (keyword REFINEMENT=).        */
/*    - use_level       - Use cut-off level or not (keyword USELEVEL=).       */
/*    - level           - The value of the cut-off level (if cut-off level is */
/*                        used) (keyword LEVEL=).                             */
/*    - mean            - Mean of noise.                                      */
/*    - sigma           - Sigma of noise.                                     */
/*----------------------------------------------------------------------------*/
{
  fint	z,
	number = 0;

  float old_mean,
	old_sigma,
	border_min;

  float border = 0.0;

  for(z = 0; z < size[BUF_WRT][LEN][Z]; z++)
    if (profile[z] == blank)
      mask[z] = BLANK;
    else
      mask[z] = NOISE;

  if (use_level) {

    /* Use cut-off level */

    border     = level;
    border_min = -FLT_MAX;
    if((mean != blank) && (sigma != blank))
      sieve_signal(border,border_min,min_win_length,&mean,&sigma);

  } else {

    /* Use sigma and mean */

    if((mean != blank) && (sigma != blank))
      do {
        border = mean + SIGMA_FACTOR * times_sigma * sigma;

	/* O.M. Kolkman: changed from					*/
        /* border_min = mean - (2 * SIGMA_FACTOR * times_sigma * sigma);*/
	/* to								*/
        border_min = mean - (SIGMA_FACTOR * times_sigma * sigma);

        old_sigma = sigma;
        old_mean = mean;
        sieve_signal(border,border_min,min_win_length,&mean,&sigma);
      } while ((number++ < MAX_ITERATIONS) &&
               (ABS(mean-old_mean)>tolerance) &&
               (mean != blank) &&
               (sigma != blank));
    if((mean != blank) && (sigma != blank)) {
      border = mean + times_sigma * sigma;
      border_min = mean - (times_sigma * sigma);
      sieve_signal(border,border_min,min_win_length,&mean,&sigma);
    }
  }

  old_mean = mean;
  if((mean != blank) && (sigma != blank) && do_refinement)
    refinement(&mean,&sigma);

  if (plot) plot_signal(old_mean, border);
}

/*============================================================================*/
/*= MAIN_PROGRAM_ENTRY                                                       =*/
/*============================================================================*/

MAIN_PROGRAM_ENTRY
/*----------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the       */
/* main body of your GIPSY application. Variables defined as 'fchar' start    */
/* with a capital.                                                            */
/*----------------------------------------------------------------------------*/
{
   fint   dfault;
   fint   nitems,
          j,
          i;
   int    axis_index;

   int direction;
   int counter;
   fint agreed;

   float noise_tolerance;
   float times_sigma[] = {3,3,3};
   float times_sigma_temp[] = {3,3,3};
   float level[3],level_temp[3];
   fint  min_win_length[] = {3,3,3};
   fint  min_win_length_temp[] = {3,3,3};
   fint  times = 2;
   bool  asknext, next, do_refinement[3],do_refinement_temp[3];
   bool  use_level[3],use_level_temp[3];
   fint  smoothmethod[] = {0,0,0};
   fint  smoothmethod_temp[] = {0,0,0};
   fint  directions[3];
   float mean, sigma;
   fint  smooth_temp[] = {0,0,0};
   float  gauss_sigma=0;

   static fchar devtype;                /* Device specified in 'pgbeg' */
   static fint	len;

   bool ok;

   int total_duration, th, mi, se;

   start_timer();

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

   do {
     nsubs   = gdsinp_c( Setin,      /* Name of input set. */
                         subin,      /* Array containing subsets coordinate words. */
                         &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                         &dfault,    /* Default code as is USERxxx. */
                         Key,        /* Keyword prompt. */
                         Mes,        /* Keyword message for the user. */
                         &showdev,   /* Device number (as in ANYOUT). */
                         axnum,      /* Array of size 'maxaxess' containing the axes numbers. */
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
     if (subdim != 3) {
       (void) reject_c( KEY_INSET, tofchar("Inset must have 3 dimensions!") );
       subdim = 0;
       }
     }
   while(subdim != 3);


   /*-------------------------------*/
   /* Determine edges of this frame */
   /*-------------------------------*/
   {
      fint cw[2];                          /* Local coordinate words */
      int  m;
      r1 = 0;
      gdsc_range_c( Setin, &setlevel, &cw[LO], &cw[HI], &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++) {
         f[LO][m] = gdsc_grid_c( Setin, &axnum[m], &cw[LO], &r1 );
         f[HI][m] = gdsc_grid_c( Setin, &axnum[m], &cw[HI], &r2 );
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
   gdsbox_c( size[BOX][LO], size[BOX][HI], Setin, subin, &dfault,
             Key, Mes, &showdev, &boxopt );

   gdsasn_c(KEY_INSET, KEY_OUTSET, &class);
   dfault = REQUEST;
   showdev=3;
   Key = KEY_OUTSET;
   Mes = MES_OUTSET;
   fmake(Setout, STRLEN);
   do {
     nsubsout = gdsout_c(Setout, subout, &nsubs, &dfault, Key, Mes, &showdev
        , axnumout, axcountout, &maxaxes);
     agreed = (nsubsout == nsubs);
     if (!agreed)
       (void) reject_c( KEY_OUTSET, tofchar("Outset not same as Inset!!") );
   } while (!agreed);

   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/

   /* --- Tolerance --- */

   do {
     noise_tolerance = 0.07; nitems = 1; dfault = HIDDEN;
     r1 = userreal_c( &noise_tolerance,
                      &nitems,
                      &dfault,
                      tofchar("TOLERANCE="),
                      tofchar("Give noise tolerance:     [0.07]") );
     if (noise_tolerance <=0) {
       reject_c(KEY_OUTSET, tofchar("Must be greater than zero!"));
       cancel_c(tofchar("TOLERANCE="));
     }
   } while (noise_tolerance <= 0);

   /* --- Directions --- */

   do {
     directions[0] = 0; directions[1] = 0; directions[2] = 0;
     nitems = 3; dfault = NONE;
     r1 = userint_c( directions,
                     &nitems,
                     &dfault,
                     tofchar("DIRECTIONS="),
                     tofchar("Give directions(1, 2 and/or 3):        []") );

     ok = TRUE;
     ndirections = 0;
     for(i=0; i<3; i++) {
       if ( !BETWEEN(directions[i],0,3) ) ok = FALSE;
       if (  BETWEEN(directions[i],1,3) ) ndirections++;
       directions[i]--;
     }

     if (!ok || ndirections == 0) {
       reject_c( KEY_OUTSET, tofchar("Only 1, 2 and 3 are allowed directions!") );
       cancel_c( tofchar("DIRECTIONS=") );
     }
   } while (!ok || ndirections == 0);

   /* --- Times --- */

   if (ndirections == 1)
     times = 1;
   else
     do {
       times = ndirections; nitems = 1; dfault = REQUEST;
       sprintf(message,"Give times:  [%d]",ndirections);
       r1 = userint_c( &times,
                       &nitems,
                       &dfault,
                       tofchar("TIMES="),
                       tofchar(message) );
       if (!(ok = BETWEEN(times,1,ndirections))) {
         sprintf(message,"Only between 1 and %d are allowed!",ndirections);
         reject_c(KEY_OUTSET,tofchar(message));
         cancel_c(tofchar("TIMES="));
         }
     } while(!ok);

   /* --- Window --- */

   do {
     sprintf(message,"Give minimum window length:  [");
     for(i=0;i<ndirections-1;i++)
       strcat(message,"3,");
     strcat(message,"3]");
     nitems = ndirections; dfault = REQUEST;
     r1 = userint_c( &min_win_length_temp[0],
                     &nitems,
                     &dfault,
                     tofchar("WINDOW="),
                     tofchar(message) );
     ok = TRUE;
     for(i=0; i<ndirections; i++)
       if (min_win_length_temp[i] <= 0)
         ok = FALSE;
     if (!ok) {
       reject_c(KEY_OUTSET, tofchar("Must be greater than zero!"));
       cancel_c(tofchar("WINDOW="));
       }
   } while (!ok);

   /* --- Use level --- */

   dfault = REQUEST+EXACT; nitems=ndirections;
   use_level_temp[0] = toflog( NO );
   use_level_temp[1] = toflog( NO );
   use_level_temp[2] = toflog( NO );

   sprintf(message,"Use level:   [");
   for(i=0;i<ndirections-1;i++)
     strcat(message,"N,");
   strcat(message,"N]");

   r1  = userlog_c( &use_level_temp[0],
                    &nitems,
                    &dfault,
                    tofchar("USELEVEL="),
                    tofchar(message) );
   use_level_temp[0] = tobool( use_level_temp[0] );
   use_level_temp[1] = tobool( use_level_temp[1] );
   use_level_temp[2] = tobool( use_level_temp[2] );

   /* --- Level --- */

   counter = 0;
   for(i = 0; i<ndirections;i++)
     if (use_level_temp[i]) counter++;
   if (counter > 0) {
     nitems = counter; dfault = EXACT;
     r1 = userreal_c( &level_temp[0],
                      &nitems,
                      &dfault,
                      tofchar("LEVEL"),
                      tofchar("Give level :         []") );
   }

   /* --- Nsigma --- */

   if (ndirections - counter > 0)
     do {
       sprintf(message," Give cut-off in number times sigma [number=");
       for(i=0;i<ndirections-counter-1;i++)
         strcat(message,"3,");
       strcat(message,"3]");
       nitems = ndirections-counter; dfault = REQUEST;
       r1 = userreal_c( &times_sigma_temp[0],
                        &nitems,
                        &dfault,
                        tofchar("NSIGMA"),
                        tofchar(message) );
       ok = TRUE;
       for(i=0; i<ndirections-counter; i++)
         if (times_sigma_temp[i] <= 0)
           ok = FALSE;
       if (!ok) {
         reject_c(KEY_OUTSET, tofchar("Must be greater than zero!"));
         cancel_c(tofchar("NSIGMA="));
         }
     } while (!ok);

   for(i=0; i<ndirections; i++)
     if (use_level[i]) {
       /* Move nsigma to the right position */
       for(j=ndirections-1; j>i; j--)
         times_sigma_temp[j] = times_sigma_temp[j-1];
     } else {
       /* Move level to the right position */
       for(j=ndirections-1; j>i; j--)
         level_temp[j] = level_temp[j-1];
     }

   /* --- Smoothing --- */

   do {
     sprintf(message,"Give smoothing length:  [");
     for(i=0; i<ndirections-1; i++)
       strcat(message,"0,");
     strcat(message,"0]");

     smooth[0] = 0; nitems = ndirections; dfault = REQUEST+EXACT;
     r1 = userint_c( &smooth_temp[0],
                     &nitems,
                     &dfault,
                     tofchar("SMOOTH="),
                     tofchar(message) );

     ok = TRUE;
     for(i=0; i<ndirections; i++)
       if (!BETWEEN(smooth_temp[i],0,SMOOTH_MAX))
         ok = FALSE;
     if (!ok) {
       sprintf(message,"Smoothing must be between 0 and %d!",SMOOTH_MAX);
       reject_c(KEY_OUTSET,tofchar(message));
       cancel_c(tofchar("SMOOTH="));
       }
   } while (!ok);

   /* --- Refinement --- */

   dfault = REQUEST+EXACT; nitems=ndirections;
   do_refinement_temp[0] = toflog( YES );
   do_refinement_temp[1] = toflog( YES );
   do_refinement_temp[2] = toflog( YES );

   sprintf(message,"Use refinement:   [");
   for(i=0;i<ndirections-1;i++)
     strcat(message,"Y,");
   strcat(message,"Y]");

   r1  = userlog_c( &do_refinement_temp[0],
                    &nitems,
                    &dfault,
                    tofchar("REFINEMENT="),
                    tofchar(message) );
   do_refinement_temp[0] = tobool( do_refinement_temp[0] );
   do_refinement_temp[1] = tobool( do_refinement_temp[1] );
   do_refinement_temp[2] = tobool( do_refinement_temp[2] );

   /* --- Method --- */

   nitems = 0;
   for(i=0; i<ndirections; i++)
     if(smooth_temp[i] > 0) nitems++;

   if (nitems) {
     anyoutC(3, "           ====== SMOOTH METHOD ======");
     anyoutC(3,"0 Gauss distribution");
     anyoutC(3,"1 Hanning function");
     anyoutC(3,"2 With given weights");

     do {
       sprintf(message,"Give smoothing option:  [");
       for(i=0; i<ndirections-1; i++)
         strcat(message,"0,");
       strcat(message,"0]");
       dfault = REQUEST+EXACT;
       r1 = userint_c( &smoothmethod_temp[0],
                       &nitems,
                       &dfault,
                       tofchar("METHOD="),
                       tofchar(message) );
       ok = TRUE;
       for(i=0; i<nitems; i++)
         if ( !BETWEEN(smoothmethod_temp[i],0,2) ) ok = FALSE;
       if (!ok) {
         reject_c(KEY_OUTSET,tofchar("Must be 0,1 or 2 !"));
         cancel_c( tofchar("METHOD=") );
       }
     } while (!ok);
   }

   /* Get the right smooth_method for the right direction */

   for(i=AXIS0; i<=AXIS2; i++)
     smooth[i] = 0;

   for(axis_index=0; axis_index<ndirections; axis_index++) {
     direction = directions[axis_index];

     smooth[direction]         = smooth_temp[axis_index];
     smoothmethod[direction]   = smoothmethod_temp[axis_index];
     min_win_length[direction] = min_win_length_temp[axis_index];
     times_sigma[direction]    = times_sigma_temp[axis_index];
     do_refinement[direction]  = do_refinement_temp[axis_index];
     use_level[direction]      = use_level_temp[axis_index];
     level[direction]          = level_temp[axis_index];
     }

   /* --- Weight --- */

   for(Z=AXIS0; Z<=AXIS2; Z++)
     if(smooth[Z] && smoothmethod[Z]==2) {
       nitems = 2*smooth[Z]+1;dfault = NONE + EXACT;
       sprintf(message,"WEIGHT%d=",Z+1);
       r1 = userreal_c( weight[Z],
                        &nitems,
                        &dfault,
                        tofchar(message),
                        tofchar("Give weight:") );
       }
     else
       if(smooth[Z] && smoothmethod[Z]==0) {
         for(z=0; z <= smooth[Z]*2; z++)
           weight[Z][z] = gauss((float)z,Z,&gauss_sigma);
           axis_name(Z,axis);
           sprintf(message,"Standard deviation Gauss-distribution for axis %s: %f",
                 axis,gauss_sigma);
           anyoutC(3,message);
         }
       else
         if(smooth[Z] && smoothmethod[Z]==1)
           for(z=0; z <= smooth[Z]*2; z++)
             weight[Z][z] = hanning((float)z,Z);
         else
           weight[Z][0] = 1;

  /* --- Show --- */

   dfault = REQUEST; nitems=1; asknext = toflog( YES );
   r1  = userlog_c( &asknext,
                    &nitems,
                    &dfault,
                    tofchar("SHOW="),
                    tofchar("Ask to show next profile? [Y]") );
   asknext = tobool( asknext );

   initplot();
   len = 20;
   finit(devtype, len);
   (void) pgqinf_c( tofchar("TYPE"), devtype, &len );
   plot = (strncmp(devtype.a, "NULL", 4) != 0);

   if (plot) {
     color = (subnr % 12) + 2;
     (void) pgsci_c( &color );
   }

   size[WEIGHT][LO][AXIS0]  = 0;
   size[WEIGHT][HI][AXIS0]  = 2*smooth[AXIS0];
   size[WEIGHT][LEN][AXIS0] = 2*smooth[AXIS0] + 1;

   size[WEIGHT][LO][AXIS1]  = 0;
   size[WEIGHT][HI][AXIS1]  = 2*smooth[AXIS1];
   size[WEIGHT][LEN][AXIS1] = 2*smooth[AXIS1] + 1;

   size[WEIGHT][LO][AXIS2]  = 0;
   size[WEIGHT][HI][AXIS2]  = 2*smooth[AXIS2];
   size[WEIGHT][LEN][AXIS2] = 2*smooth[AXIS2] + 1;

   for(axis_index=0; axis_index<ndirections; axis_index++) {

     direction = directions[axis_index];

     switch (direction) {
       case AXIS0 : X = AXIS2; Y = AXIS1; Z = AXIS0; break;
       case AXIS1 : X = AXIS0; Y = AXIS2; Z = AXIS1; break;
       case AXIS2 : X = AXIS0; Y = AXIS1; Z = AXIS2; break;
     }

     axis_name(direction, axis);
     sprintf(message,"Axis %d: %s",direction+1, axis);
     anyoutC(3, message);

     (void) init_buf(axis_index);

     bar_min = 0.0;
     bar_max = (size[BOX][HI][X]-size[BOX][LO][X]+1) * (size[BOX][HI][Y]-size[BOX][LO][Y]+1);
     current = 0.0;
     dfault = REQUEST; nitems=1; next = toflog( YES );

     do {
       for(y = 0; y < size[BUF_WRT][LEN][Y] && (next == YES); y++) {
         stabar_c(&bar_min, &bar_max, &current);
         for(x = 0; x < size[BUF_WRT][LEN][X] && (next == YES); x++) {

           get_profile(x ,y , &mean, &sigma);
           detect_signal(noise_tolerance,times_sigma[Z],min_win_length[Z],
                         do_refinement[Z],use_level[Z],level[Z],mean, sigma);
           save_profile(x, y, times,axis_index);

           current++;

           if (asknext== YES) {
             r1  = userlog_c( &next,
                              &nitems,
                              &dfault,
                              tofchar("NEXT="),
                              tofchar("Show next profile? [Y]") );
             next = tobool( next );
             cancel_c( tofchar("NEXT="));
           }
         }
       }
     } while(next == YES && read_next_buffer(axis_index) != TOTAL_IMAGE_READ);
     finit_buf();
   }
   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   total_duration = gettime();
   th = total_duration/3600;
   mi = (total_duration/60) - (th*60);
   se = total_duration%60;
   sprintf(message,"Total duration: %02d:%02d:%02d    (%d sec)"
                    ,th,mi,se,total_duration);
   anyoutC(3, message);

   (void) pgend_c();
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
