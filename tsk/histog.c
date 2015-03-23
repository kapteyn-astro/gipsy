/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             histog.dc1

Program:       HISTOG

Purpose:       Program creates histogram of intensities in a set
               or part of a set.

Category:      ANALYSIS, PLOTTING

File:          histog.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give set (, subsets) for histogram:
               Maximum number of subsets is 2048.

   BOX=        Frame for input subsets.                 [entire subset]

   RANGE=      Give range in pixel values:                 [all values]
               To exclude pixels from the calculations, specify
               a range. Example:
               RANGE=1 5   : INCLUDE all pixels with value
                             equal to or between 1 and 5.
               RANGE=5 1   : EXCLUDE all pixels with value
                             between 1 and 5.

   GRDEVICE=   Give graphics device to      [list of available devices]
               plot on:

** PGMOSAIC=   View surface sub divisions in x,y:                 [1,1]
               View surface can contain a number of plots in
               in X and Y direction (mosaic). Default is 1 plot in
               both X- and Y direction.
                                             
** PAPER=      Give width (cm), aspect ratio:                 [0.0,1.0]
               Change the size of the (output) view surface.
               The aspect ratio is defined as height/width.
               The default is a calculated size and aspect ratio 1.

** LINEWIDTH=  Give line width (1-21):                              [2]
               Only required if a hardcopy device is selected.

   AUTOSCALE=  Automatic scaling of each plot:                    [Y]/N
               In the default mode the program calculates
               suitable plot boundaries. If no automatic
               scaling is selected, the boundaries of the first
               plot will be default for plots of data from other
               subsets.

   MINMAX=     Give min. and max. value of data:
               MINMAX= determines the length of the X-axis of the
               histogram.
               If the set header provides minimum and maximum, the
               keyword is asked hidden.

   FIXBIN=     Do you want to fix the bin width?                  Y/[N]
               If the user wants to select a constant bin width,
               FIXBIN=Y has to be specified and you are prompted
               with keyword BINWIDTH=

   BINS=       Give number of histogram bins:                     [100]
               If FIXBIN=N, specify the number of bins.

   BINWIDTH=   Give width of histogram bin:
               If FIXBIN=Y, specify the width of a bin.

** CUMULATIVE= Plot cumulative histogram?                         Y/[N]

** YLOG=       Y axis logarithmic?                                Y/[N]

** LOGFILE=    Print histogram values in Log-file                 Y/[N]

** GAUSS=      Plot a Gauss?                                      Y/[N]
               Include a Gaussian curve in the plot.

   GAUPAR=     Amplitude, Mean, Sigma & Zero level:        [calculated]
               The defaults for the Gauss parameters are the value
               of the mode, the mean and rms of the histogram
               data. The default for the offset is 0.

   COUNTS=     Interval of counts to plot:      [total range of counts]
               Select Y-axis interval of histogram.


Description:   The pixels in a set have a value in units given by the
               header of the set. HISTOG creates a histogram of these
               values. The bin width is in the same units as the pixel
               values and the bin height is the number of pixels in 
               your (sub)set that have a value in that bin.
               Along with a plot some elementary statistics are calculated 
               (mean, rms, etc). There is also an option to plot a 
               gaussian curve in the histogram.
               
               Set and subsets are given by INSET=
               
               Example: INSET=n1260s60 f ra 0 
               
               to get a histogram of data in set n1260s60 ( a RA-DEC-FREQ
               set ) along the DEC axis at RA = 0 and for all frequencies.
               
               If you want only a part of the DEC axis use keyword BOX=
               to define that part. If you are not satisfied with some
               data values and want to exclude these values use RANGE=
               Note that there is a special syntax for this keyword. 
               RANGE=3 5 INCLUDES all values between 3 and 5 (values 3 
                         and 5 included),
               RANGE=5 3 EXCLUDES all values between 3 and 5.
                              
               If the calculations extend over more than one subset then 
               there are two plot modes: first there is a possibility
               to scale the data for each plot automatically
               (AUTOSCALE=Y), second there is an option to fix the plot
               characteristics. With AUTOSCALE=N, histograms of
               different subsets can be compared (Use PGMOSAIC= to create
               'sub pages' on screen or paper). The choice has
               to be made before starting the loop over your subsets.
               
               The plot dimensions are controlled by two keywords:
               MINMAX= gives the lowest and the highest bin values
               (in header units) along the histogram's X-axis.
               COUNTS= gives the minimum and maximum heights of all
               histogram bins. The minimum value is 0 and the maximum
               value cannot be less than the minimum value + 1.
               The program automatically corrects for wrong input.
               Defaults are calculated for each subset individually.
               
               Lets discuss MINMAX= in more detail:              
               For each subset the values of the minimum and maximum on the 
               x-axis can be set with MINMAX=. With these numbers the 
               number of histogram bins (BINS=) or the bin width (BINWIDTH=)
               is calculated. The MINMAX= keyword can be asked hidden
               or with defaults. This depends on the program status: 
               1) If a valid range was given (RANGE=a b and a < b)
                  the keyword is hidden and the default for MINMAX=
                  are these range values. These values are used for all
                  subsets.
               2) If 1) is not valid, the program looks in the header of 
                  the input set if "DATAMIN" and "DATAMAX" are defined. 
                  If not, then the user is prompted for the keyword (no
                  defaults allowed) for each subset.
               3) If the header items are defined however, and the user
                  did not select automatic scaling (AUTOSCALE=N), the keyword 
                  is hidden. The defaults for all subsets are the values that 
                  were found in the header of the first subset.
                  If automatic scaling was selected, the defaults 
                  are the header values found for each subset.

               The bin width can be controlled in two different ways. 
               If FIXBIN=N, the user must give the number of bins to create 
               within the given data range. If FIXBIN=N
               you must specify a bin width in units of your data.
               
               Other plot options are:
                -Create a cumulative histogram ((CUMULATIVE=Y).
                -Label 'count' axis Logarithmically (YLOG=Y).
                -Include a gaussian curve (GAUSS=Y) with user given
                 (GAUPAR=) parameters. The defaults for amplitude, mean
                 and sigma, are calculated by the program. The default for
                 the offset in Y-direction (zero level) is 0.
                -Write bin values and bin heights in log file (LOGFILE=Y).
                -Adjust size of plot on output device with PAPER=.
                 The first parameter is the requested plot width in
                 cm. The second is the aspect ratio (height/width).
                 Define the number of histograms in one plot with
                 PGMOSAIC=
                -The width of the plotted lines can be changed on
                 the output device with LINEWIDTH=. The default
                 gives a reasonable plot. The keyword is asked only if
                 you specified a printer after GRDEVICE=.

Example:

               <USER >histog paper=14 gauss=y
               <USER >INSET=test2
               Set TEST2 has 3 axes
               RA-NCP             from    -9 to    10
               DEC-NCP            from    -9 to    10
               FREQ-OHEL          from     0 to     9
               <USER >BOX=
               BOX range for set TEST2 :
               RA-NCP             from    -9 to    10
               DEC-NCP            from    -9 to    10
               FREQ-OHEL          from     0 to     9
               <USER >RANGE=
               <USER >GRDEVICE=laserp
               <USER >FIXBIN=
               <USER >BINS=200
               ============================================================
               Set: TEST2  (RA,DEC,VELO)=(*,*,*)
               ============================================================
               Range in selected data:        All values
               Total number of valid pixels:  4000
               Number of blanks:              0
               Min. and max. in data:         (-4.87012, 14.6622)
               Min. and max. in plot:         (-4.87012, 14.8575)
               Sum :                          17987.4
               Mean:                          4.49684
               Rms:                           3.49833
               Mode:                          X=(4.69777, 4.7964),  (Y=54)
               ============================================================
               <USER >GAUPAR=
               <USER >COUNTS=
               <STATUS> histog -  +++ FINISHED +++


Updates:       Mar  1, 1991: VOG, Document created.
               Oct 10, 1991: WZ,  PGPLOT standard names implemented.
               Jun 20, 1994: VOG, Changed defaults for some keywords.
               Dec 14, 1994: VOG, Allocate array space dynamical. 
               Feb 22, 1996: VOG, Bug removed in length of dynamical
                                  array 'counts'.

#<

*/

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "math.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"
#include "gdsinp.h"
#include "gdsc_ndims.h"
#include "setfblank.h"
#include "myname.h"
#include "anyout.h"
#include "nelc.h"
#include "cancel.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsbox.h"
#include "gdsc_fill.h"
#include "gdsi_read.h"
#include "cotrans.h"
#include "axunit.h"
#include "userint.h"
#include "cancel.h"
#include "gdsd_rchar.h"
#include "error.h"
#include "gdsc_name.h"
#include "userreal.h"
#include "userlog.h"
#include "userint.h"
#include "reject.h"
#include "axcoord.h"
#include "statr.h"
#include "gdsd_rreal.h"
#include "time.h"


#include "pgplot.h"


#define AXESMAX    10               /* Max. allowed number of axes in a set */
#define SUBSMAX    2048             /* Max. number of substructures to be specified */
#define MAXBUF     4096             /* Buffer size for I/O */
#define BIGSTORE   180              /* Length of a string */
#define VERSION    "1.0"            /* Version number of this program */
#define NONE       0                /* Default values for use in userxxx routines */
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define NO         0
#define YES        1
#define PI         3.141592653589793


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

#define KEY_RANGE   tofchar( "RANGE=" )
#define KEY_MINMAX  tofchar( "MINMAX=" )

/* Initialize string with macro */
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


#define MYMAX(a,b) ((a) > (b) ? (a) : (b))
#define MYMIN(a,b) ((a) < (b) ? (a) : (b))
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )

/* Input of set, subsets: */

static fchar    Setin;                 /* Name of the set */
static fint     subin[SUBSMAX];        /* Array for the subset coordinate words */
static fint     nsubsI;                /* Number of input subsets */
static fint     dfault;                /* Default option for input etc */
static fchar    keyword, message;      /* Used in userxxx routines */
static fint     axnum[AXESMAX];        /* GDSINP axis numbers array */
static fint     axcount[AXESMAX];      /* GDSINP axis lengths array */
static fint     class = 1;             /* Repeat operation for each subset */
static fint     setdim;                /* Dimension of the set */
static fint     subdim;                /* Dimension of the subset */
static fint     scrnum = 0;            /* Destination of log output */
static fint     maxaxes  = AXESMAX;    /* Convert parameters to variables */
static fint     maxsubs  = SUBSMAX;
static fint     maxIObuf = MAXBUF;
static fint     subnr;                 /* Index of current subset */
static fint     i, m;                  /* Counters */
static fint     toplevel = 0;          /* Indicate set level */


/* Input of area etc.:*/

static fint     cwlo, cwhi;            /* Coordinate words */
static fint     FgridLO[AXESMAX];      /* Coordinate words for frame */
static fint     FgridHI[AXESMAX];
static fint     BgridLO[AXESMAX];      /* Coordinate words for box */
static fint     BgridHI[AXESMAX];
static fint     boxopt;                /* Input option for 'gdsbox' */
static fint     totpixels;             /* Total number of pixels in input */

/* Data transfer: */

static fint     pixelsdone;
static fint     pixelcount;
static fint     tid;                   /* Transfer id */
static float    image[MAXBUF];         /* Contains data to be binned */


/* Statistics: */

static fint  inrange;                  /* Number of pixels in one box with */
                                       /* values between min and max used  */
                                       /* for binning, and conforming to   */
                                       /* user given range                 */
static fint  outrange;                 /* All other pixels in that box except blanks */
static int   indx;                     /* Array index variable */
static float binwidth;                 /* Width of a histogram bin */
static float *Xarray = NULL;           /* Containing histogram interval values */
static float *counts = NULL;           /* Containing bin heights */
static float maxcounts;                /* Height of histogram mode */
static fint  numbins;                  /* Number of bins in a plot */
static float filter[MAXBUF];           /* Filtered array containing pixels */
                                       /* that are in range */
static float minval, maxval;           /* Min, max of examined, valid data */
static float sum, mean, rms;           /* Histogram characteristics */
static fint  nblanks;                  /* Number of blanks encountered */
static fint  Fdummy;
static fint  ntot;                     /* Total number of values checked in 'statr' */


/* Plot related: */

static fint   autoscale;               /* Fix plot parameters for each subset? */


/* Miscellaneous: */

static int      agreed;                /* Loop control */
static float    datamin, datamax;      /* Values used to determine bin intervals */
static fint     nitems;                /* Max. number of input items in userxxx routines */
static fint     r1, r2;                /* Results of userxxx routines */
static float    blank;                 /* Value of system blank */
static float    range[2];              /* User given data in/exclude range */
static float    value;                 /* Dummy value */
static int      inside;                /* Valid pixel? */
static char     messbuf[BIGSTORE];     /* Buffer for text message */

static fint     cumulat;               /* Cumulative histogram? */
static fint     Fgauss;                /* Is there a Gauss to plot? */
static float    gausspar[4];           /* Ampl, mean, rms, Y-offset of Gaussian */
static fint     logYax;                /* Logarithmically Y-axis? */
static int      Xmodeindx;             /* Array position of histogram mode */
static fint     tofile;                /* Write hist. values in log file? */
static fchar    Subsetstring;          /* String containing subset info */
static char     line1[BIGSTORE];       /* Dummy output string */
static char     line2[BIGSTORE];       /* Dummy output string */
static fint     subsetlevel;           /* Subset level */
static fint     setlevel = 0;
static char     border[BIGSTORE];      /* Dummy string */
static fint     filcount;              /* Length of filtered array */
static fint     Ferrlev;               /* Error level for use in 'error' routine */
static char     mapunits[20];
static bool     foundunits;



void anyoutC( int scrnum, char *anystr )
/*-------------------------------------------------------------------------*/
/* The C version of 'anyoutC' needs a C type string as argument only. and  */
/* and integer to determine the destination of the output:                 */
/*   0  use default [set by HERMES to 3 but can be changed by user]        */
/*   1  terminal                                                           */
/*   2  LOG file                                                           */
/*   8  terminal, suppressed in "experienced mode"                         */
/*  16  terminal, only when in "test mode                                  */
/*-------------------------------------------------------------------------*/
{
   fint Scrnum = (fint) scrnum;
   anyout_c( &Scrnum, tofchar( anystr ) );
}



void showsubset( fchar Setin, fint *subin, fint *axnum,
                 fint *subdim, fint *setdim, fchar Subsetstring )
/*-------------------------------------------------------------------------*/
/* Create the string 'Subsetstring' containing information about the axes. */
/* Example: Subsetstring = "(RA,DEC,FREQ) = (*,*,1)"                       */
/*-------------------------------------------------------------------------*/
{
   int    n;                   /* Counter */
   fchar  Fctype;              /* Type of axis, unmodified */
   fint   r1;                  /* Return level or function result */
   fint   Fgrid;               /* Current grid of running axis */
   char   dummystr[BIGSTORE];  /* Dummy string */
   char   leftbuf[BIGSTORE];   /* String for name part */
   char   rightbuf[BIGSTORE];  /* String for grid part */
   fchar  Funits;              /* Unused, but necessary */
   fint   Fcolev;              /* =1 if primary axis was used, etc */


   sprintf( leftbuf,  "%c", '(' );
   sprintf( rightbuf, "%c", '(' );
   for (n = 0; n < *setdim; n++ )
   {
      char *cptr;
      fmake( Fctype, 20 );
      fmake( Funits, BIGSTORE );
      /* Use 'axcoord' instead of the function gdsc_name because then always */
      /* the name of the primary axis is returned */
      r1 = axcoord_c( Setin,
                      &axnum[n],
                      Fctype,
                      Funits,
                      &Fcolev );
      cptr = strtok( Fctype.a, " -" );
      if (( n + 1 ) == *setdim)
      {
         /* Copy until space or hyphen! */
         if (cptr == NULL)
            (void) sprintf( dummystr, "%s", "?" );
         else
            (void) sprintf( dummystr, "%s", cptr );
      }
      else
      {
         /* Comma added in dummy string, because there is more to come */
         if (cptr == NULL)
            (void) sprintf( dummystr, "%s,", "?" );
         else
            (void) sprintf( dummystr, "%s,", cptr );
      }
      sprintf( leftbuf, "%.*s%s", strlen(leftbuf), leftbuf, dummystr );
      if (n >= *subdim)
      {
         /* Get current grid of this axis */
         r1 = 0;
         Fgrid = gdsc_grid_c( Setin, &axnum[n], subin, &r1 );
         sprintf( dummystr, "%d", Fgrid );
      }
      else
      {
         /* It was not one of the running axes */
         sprintf( dummystr, "%c", '*' );
      }
      if (( n + 1 ) == *setdim)
         sprintf( rightbuf, "%.*s%s", strlen(rightbuf), rightbuf, dummystr );
      else
         sprintf( rightbuf, "%.*s%s,", strlen(rightbuf), rightbuf, dummystr );
   }
   sprintf( leftbuf,  "%.*s%c", strlen(leftbuf),  leftbuf,  ')' );
   sprintf( rightbuf, "%.*s%c", strlen(rightbuf), rightbuf, ')' );
   /* Complete the string with subset indication */
   sprintf( Subsetstring.a, "%s=%s", leftbuf, rightbuf );
}



void showcoord( fchar Setin, fint *subin, fint *axnum,
                fint *subdim, fint *setdim, fchar Subsetstring )
/*-------------------------------------------------------------------------*/
/* Create the string 'Subsetstring' containing physical information about  */
/* the axes. Example: Subsetstring = "(*,29.5 DEGREE,200 KM/S)". This      */
/* string can be appended to the string obtained after a call to the       */
/* routine 'showsubset". The definitions for BIGSTORE and AXESMAX and the  */
/* macro 'fmake' must be available.                                        */
/*-------------------------------------------------------------------------*/
{
   int    n;                     /* Counter */
   fchar  Fcunit;                /* Units along this axis */
   fint   r1;                    /* Return level or function result */
   fint   Fgrid;                 /* Current grid used in coordinate transformation */
   char   dummystr[BIGSTORE];
   char   rightbuf[BIGSTORE];

   /* Coordinate transformation */

   fint     Fdirect;             /* grid coord. -> physical coord. */
   double   coordin[AXESMAX];    /* Grids before transformation */
   double   coordout[AXESMAX];   /* Physical coordinates after transformation */


   sprintf( rightbuf, "%c", '(' );
   for (n = 0; n < *setdim; n++ )
   {
      if (n >= *subdim)
      {
         r1 = 0;
         Fgrid = gdsc_grid_c( Setin, &axnum[n], subin, &r1 );
         /* Remember: lowest value of axnum is 1!
                      lowest array index is 0!  */
         coordin[ (int) axnum[n]-1 ] = (double) Fgrid;
      }
      else
         coordin[ (int) axnum[n]-1 ] = 0.0;
   }

   Fdirect = 1;                             /* grid coord. -> physical coord. */
   r1 = cotrans_c( Setin, subin, coordin, coordout, &Fdirect );
   if (r1 != 0)
   {
      strcpy( Subsetstring.a, "(*****)" );
      return;
   }
   for (n = 0; n < *setdim; n++ )
   {
      if (n >= *subdim)
      {
         fmake( Fcunit, 20 );
         r1 = axunit_c( Setin, &axnum[n], Fcunit );
         sprintf( dummystr, "%.6g %.*s", coordout[ (int) axnum[n]-1 ],
                  (int) nelc_c( Fcunit ), Fcunit.a );
      }
      else
         sprintf( dummystr, "%c", '*' );
      if (( n + 1 ) == *setdim)
         sprintf( rightbuf, "%.*s%s", strlen(rightbuf), rightbuf, dummystr );
      else /* Add comma */
         sprintf( rightbuf, "%.*s%s,", strlen(rightbuf), rightbuf, dummystr );
   }
   sprintf( Subsetstring.a, "%s)", rightbuf );
}




void getgauss( float *gausspar )
/*------------------------------------------------------------------------------
 * Description: If the user wants to include a Gaussian in the plot,
 *              ask for parameters amplitude, mean, rms, offset in y-direction.
 *              The array gausspar contains default values on input and the
 *              user given values on output.
 *------------------------------------------------------------------------------
 */
{
   fint    r1;
   fint    nitems;
   fint    dfault;


   nitems = 1;
   dfault = HIDDEN;
   /* Fgauss is global to the program */
   Fgauss = toflog(NO);
   r1 = userlog_c( &Fgauss, &nitems, &dfault,
                    tofchar("GAUSS="),
                    tofchar("Plot a Gauss?     Y/[N]" ) );
   if (Fgauss)
   {
      /* Ask for parameters */
      nitems = 4;
      dfault = REQUEST;
      r1 = userreal_c( gausspar, &nitems, &dfault,
                       tofchar("GAUPAR="),
                       tofchar("Amplitude, Mean, Sigma & Zero level:  [calculated]") );
   }
}



void getuserinp( fchar Setin, fint subset, int subnr,
                 float *datamin, float *datamax, float *range, 
                 fint *bins, float *binwidth )
/*--------------------------------------------------*/
/* Description: Input is set name, subset and index */
/* number of the subset. Return min, max value for  */
/* the binning, the number of bins and the width of */
/* a bin. Specify normal or cumulative mode (global */
/* cumulat), normal or Logarithmic Y-axis (global   */
/* logYax) and decide whether you want the histo-   */
/* gram results in the log file (global tofile).    */
/* Determination of range in X:                     */
/* For each subset the values of the min and max    */
/* on the x-axis can be set with MINMAX=. Whether   */
/* the keyword is hidden or not, depends on the     */
/* program status:                                  */
/* 1) If a valid range was given (r[0] < r[1]) then */
/*    the keyword is hidden and the default min,max */
/*    are these range values. The keyword is not    */
/*    cancelled.                                    */
/* 2) If 1) is not valid, the program looks in the  */
/*    header of the input set if "DATAMIN",         */
/*    "DATAMAX" are defined. If not: Ask keyword    */
/*    unhidden and do not allow defaults and cancel */
/*    keyword.                                      */
/* 3) If they are defined however, and the user     */
/*    did not select automatic scaling, the keyword */
/*    is asked hidden. The first time defaults are  */
/*    the values that were found in the header and  */
/*    the keyword is not cancelled. If automatic    */
/*    scaling was selected, the defaults are the    */
/*    header values found for each subset and the   */
/*    keyword is cancelled each time.               */
/*                                                  */
/*--------------------------------------------------*/
{
   fint    r1, r2;                 /* Return levels and function results */
   fint    agreed;                 /* Loop guard */
   fint    nitems;                 /* Number of items in userxxx routines */
   fint    dfault;                 /* Default in userxxx routines */
   bool    cancel = YES;
   
   static  float   minmax[2];      /* Array version of datamin/max */
   static  fint    numbins;        /* User given number of bins in histogram */
   static  fint    fixbin;         /* Input number of bins or binwidth? */

   do
   {
      nitems = 2;
      if (range[0] != blank && range[1] != blank)
      {
         if (range[0] < range[1])
         {
            /*----------------------------------*/
            /* A range was given with min < max */
            /*----------------------------------*/            
            dfault    = HIDDEN;
            minmax[0] = range[0];
            minmax[1] = range[1];
            sprintf( messbuf, "Give min, max for binning:  [%g %g] (%s)",
                     minmax[0], minmax[1], mapunits );
            cancel    = NO;
         }
         else
         {
            dfault = NONE;
            sprintf( messbuf, "Give min, max (in %s) for binning:", mapunits );
            cancel = NO;                                    
         }
      }
      else
      {
         /*----------------------------------------*/
         /* We still need two numbers to determine */
         /* the bin width.                         */
         /*----------------------------------------*/
         r1 = r2 = 0;
         gdsd_rreal_c( Setin, tofchar("DATAMIN"), &subset, datamin, &r1 );
         gdsd_rreal_c( Setin, tofchar("DATAMAX"), &subset, datamax, &r2 );
         if ( (r1 < 0) || (r2 < 0) ) 
         {
            /*-----------------------------------------*/
            /* Nothing in the header that is suitable. */
            /*-----------------------------------------*/            
            dfault = NONE;
            sprintf( messbuf, "Give min, max (%s) for binning:", mapunits );  
            cancel = NO;
         }
         else 
         {
            /*-----------------------------------------------------*/
            /* Min, max are in header.                             */
            /* The first time this keyword was asked and values    */
            /* were found in the header make sensible defaults.    */
            /* If it was not the first time, take previous values  */
            /* as defaults. This will work because minmax is       */
            /* defined as static                                   */
            /*-----------------------------------------------------*/
            if ((subnr == 0) || (autoscale))
            {
               minmax[0] = *datamin;  /* From header now */
               minmax[1] = *datamax;
            }
            dfault = HIDDEN; 
            cancel = NO;
            sprintf( messbuf, "Give min, max for binning:  [%g %g] (%s)",
                     minmax[0], minmax[1], mapunits );
            if (autoscale) 
               cancel = YES;
         }
      }
      r1 = userreal_c( minmax, &nitems, &dfault, 
                       KEY_MINMAX, tofchar( messbuf ) );

      agreed = (minmax[1] > minmax[0]);
      if (!agreed)
         reject_c( KEY_MINMAX, tofchar("Minimum >= Maximum")  );
      if (agreed)
      {
         agreed = (r1 == 0 || r1 == 2);
         if (!agreed)
            reject_c( KEY_MINMAX, tofchar("0 or 2 entries only!")  );
      }
      if (agreed) 
      {         
         if (cancel)
            cancel_c( KEY_MINMAX );
         if (r1 == 0) 
            minmax[1] += (minmax[1] - minmax[0]) / 100.0;
      }
   }
   while (!agreed);
   *datamin = minmax[0];
   *datamax = minmax[1];      


   fixbin = toflog(NO);
   nitems = 1;
   dfault = REQUEST;
   r1     = userlog_c( &fixbin,  &nitems, &dfault,
                       tofchar( "FIXBIN=" ),
                       tofchar("Do you want to fix the bin width?    Y/[N]") );

   if (fixbin)
   {
      nitems = 1;
      dfault = REQUEST;
      *binwidth = (*datamax - *datamin) / 100.0;  /* width eq. 100 bins */
      (void) sprintf( messbuf, "Give width of histogram bin:  [%f]",
                      *binwidth );
      r1 = userreal_c( binwidth,  &nitems, &dfault,
                       tofchar("BINWIDTH="),
                       tofchar(messbuf) );
      numbins = (int) ( (*datamax - *datamin) / *binwidth ) + 1;
   }
   else
   {
      if (subnr == 0)
      {
         numbins = 100;
         dfault = REQUEST;
      }
      else
         dfault = HIDDEN;
      nitems = 1;
      agreed = NO;
      while (!agreed)
      {
         r1 = userint_c( &numbins, &nitems, &dfault,
                          tofchar( "BINS=" ),
                          tofchar( "Give number of histogram bins:  [100]" ) );
         agreed = (numbins > 0);
         if (!agreed)
            reject_c( tofchar("BINS="), tofchar("# must be > 0") );
      }
      *binwidth = (*datamax - *datamin) / (float) numbins;
   }
   *bins = numbins;


   nitems = 1;
   dfault = HIDDEN;
   /* cumulat is global to the program */
   cumulat = toflog(NO);
   r1 = userlog_c( &cumulat, &nitems, &dfault,
                    tofchar("CUMULATIVE="),
                    tofchar("Plot cumulative histogram?    Y/[N]" ) );

   nitems = 1;
   dfault = HIDDEN;
   /* logYax is global to the program */
   logYax = toflog(NO);
   r1 = userlog_c( &logYax, &nitems, &dfault,
                    tofchar("YLOG="),
                    tofchar("Y axis logarithmic?    Y/[N]" ) );

   nitems = 1;
   dfault = HIDDEN;
   /* tofile is global to the program */
   tofile = toflog(NO);
   r1 = userlog_c( &tofile, &nitems, &dfault,
                    tofchar("LOGFILE="),
                    tofchar("Print histogram values in Log-file   Y/[N]" ) );
}


void logtable( float *Xarray, fint numbins, float *counts )
/*------------------------------------------------------------------------------
 * Description: Write all bin values in log file.
 *------------------------------------------------------------------------------
 */
{
   static char messbuf[BIGSTORE];
   static int  k;


   anyoutC( 0, "============================");
   sprintf( messbuf, "%6s  %10s  %8s", "index", "dataval", "counts" );
   anyoutC( 0, messbuf );
   anyoutC( 0, "============================");
   for (k = 0; k < numbins; k++)
   {
      sprintf( messbuf, "%6d  %10f  %8d", k, Xarray[k], (int) counts[k] );
      anyoutC( 0, messbuf );
   }
}


void makeplot( float *Xarray, fint numbins, float *counts,
               float datamin, float datamax, float maxcounts,
               fchar Fsetname, fint subset, float *gausspar,
               float binwidth, int subnr, char *toptitle )
/*------------------------------------------------------------------------------
 * Description: Create the histogram. The heights of the bins are known
 *              in this routine, so give user possibility to give a
 *              range in the bin heights to plot. If user wants gaussian
 *              in plot, calculate curve values and plot.
 *------------------------------------------------------------------------------
 */

{
   fint   Fcenter;                /* If false, the X values denote the
                                     lower edge (in X) of the bin. */
   float  Ymin, Ymax;             /* Min, max in plot */
   int    i, m;                   /* Counters */
   fchar  FXlabel, FYlabel;       /* X/Y axis labels */
   fchar  FTOPlabel;              /* Label on top of plot */
   char   TOPlabbuf[BIGSTORE+1];  /* Character storage for that label */
   fint   r1;                     /* Return value or level */
   fint   dfault;                 /* Default in userxxx routines */
   fint   nitems;                 /* Max. number of items in userxxx routines */
   float  Yminmax[2];             /* Array version of counts range */
   float  Xtick, Ytick;           /* Tick values for plot box */
   fint   Fnxsub, Fnysub;         /* Subintervals of major coordinate interval */
   float  charsize;               /* Size of plot characters */
   
   static float  Ymaxstore, Yminstore;   /* Store min, max of plot range */


   /* initialize strings */
   for (m = 0; m < BIGSTORE; m++) 
   {
      TOPlabbuf[m] = ' ';
   }
   TOPlabbuf[m]  = '\0';
   FTOPlabel.a = TOPlabbuf;
   FTOPlabel.l  = BIGSTORE;
   finit( FXlabel, BIGSTORE );
   finit( FYlabel, BIGSTORE );



   Ymin = 0.0;
   /* Determine maximum value in count array */
   Ymax = maxcounts;
   if (cumulat)
   {
      /* Create cumulative plot */
      for (i = 1; i < numbins; i++)
         counts[i] += counts[i-1];
      Ymax = counts[i-1];
   }

   if (logYax)
   {
      Ymin = 0.0;
      Ymax = (float) log10(Ymax);
      for (i = 0; i < numbins; i++)
      {
         if (counts[i] <= 1.0)
            counts[i] = 0.0;
         else
            counts[i] = (float) log10(counts[i]);
      }
   }

   /* The user can modify the min and max values in the y-direction */

   dfault = REQUEST;
   nitems = 2;
   if ((subnr == 0) || (autoscale))
   {
      Yminmax[0] = Ymin;
      Yminmax[1] = Ymax;
      (void) sprintf( messbuf, "Plot range in Y:    [%g %g]", Ymin, Ymax );
      r1 = userreal_c( Yminmax, &nitems, &dfault,
                        tofchar("COUNTS="),
                        tofchar(messbuf) );                 
      Ymin = MYMAX( Yminmax[0], 0.0 );
      Ymax = MYMAX( Ymin+1, Yminmax[1] );
   }

   /* For the first subset, store the minmax parameters.           */
   /* Store these parameters in case of autoscaling also           */
   /* The result is that if there is autoscaling, a new min and    */
   /* max will be used. If there is no autoscaling, the parameters */
   /* from the first plot will be used */

   if ((subnr == 0) || (autoscale))
   {
      Yminstore = Ymin;
      Ymaxstore = Ymax;
   }
   Ymin = Yminstore;
   Ymax = Ymaxstore;

   if (r1 == 0)
   {
      /* Little offset in Y-direction */
      Ymax = Ymax + Ymax / 10.0;
   }

   if ((r1 == 1) && (logYax))
   {
      if (Ymin < 1.0) Ymin = 1.0;
      Ymin = (float) log10(Ymin);
   }

   if ((r1 == 2) && (logYax))
   {
      if (Ymin < 1.0) Ymin = 1.0;
      Ymin = (float) log10(Ymin);
      Ymax = (float) log10(Ymax);
   }



   /* Plot the box... */

   charsize = 1.0;
   pgsch_c( &charsize );
   pgpage_c();
   pgswin_c( &datamin, &datamax, &Ymin, &Ymax );
   Xtick = 0.0; Ytick = 0.0;

   /* nx/nysub = the number of subintervals to divide the major
      coordinate interval into. If xtick=0.0 or nxsub=0,
      the number is chosen by PGBOX. */

   Fnxsub = 0; Fnysub = 0;

   if (logYax)
      pgbox_c( tofchar("BCNST"), &Xtick, &Fnxsub,
               tofchar("BCNSTLV"), &Ytick, &Fnysub );
   else
      pgbox_c( tofchar("BCNST"), &Xtick, &Fnxsub,
               tofchar("BCNSTV"), &Ytick, &Fnysub );


   /* The X values denote the lower edge (in X) of the bin. */
   Fcenter = toflog(NO);
   {
      fint    col = GREEN, oldcol;
      pgqci_c( &oldcol );
      pgsci_c( &col );   
      pgbin_c( &numbins, Xarray, counts, &Fcenter );
      pgsci_c( &oldcol );
   }


   /* Construct the labels */
   if (logYax)
      FYlabel = tofchar( "LOG(Counts)" );
   else
      FYlabel = tofchar( "Counts" );
   r1 = 0;
   gdsd_rchar_c( Fsetname, tofchar("BUNIT"), &subset, FXlabel, &r1 );
   if (r1 < 0)
     FXlabel = tofchar( "?" );
   FTOPlabel = tofchar( toptitle );

   /* Change character size in the plot */

   charsize = 1.0;
   pgsch_c( &charsize );
   pglab_c( FXlabel, FYlabel, FTOPlabel );


   /* Is there a Gaussian to be plotted ? */
   if (Fgauss)
   {
      static float expon;
      float amp       = gausspar[0];
      float mean      = gausspar[1];
      float rms       = gausspar[2];
      float offset    = gausspar[3];
      float *gaussval = NULL;

      gaussval = (float *) calloc( numbins, sizeof(float) );
      if (gaussval == NULL)
         anyoutC( 1, "Cannot allocate space for gauss array" );
      else
      {  
         fint    oldcol, col = YELLOW;
         
         pgqci_c( &oldcol );
         pgsci_c( &col );
         for(i = 0; i < numbins; i++) {
            Xarray[i] += binwidth / 2.0;
            /* value of the blank is global */
            if ((rms == 0.0) || (rms == blank))
               gaussval[i] = 0;
            else
            {
               expon = (Xarray[i]-mean)*(Xarray[i]-mean) / (2.0*rms*rms);
               gaussval[i] = amp * exp(-1.0*expon) + offset;
               if (logYax) 
                  gaussval[i] = log10(gaussval[i]);
            }
         }
         pgline_c( &numbins, Xarray, gaussval );
         pgsci_c( &oldcol );
      }
      if (gaussval)
         free( gaussval );
   }
}



void initplot( void )
/*------------------------------------------------------------------------------
 * Description: Initialize plot software. Set viewport and output dimensions.
 *              If output device is a printer, ask user for linewidth.
 *------------------------------------------------------------------------------
 */

{
   fint    Funit;                  /* Ignored by 'pgbeg', use 0 */
   fchar   Ffile;                  /* Device specification */
   fint    nxysub[2];              /* Number of subdivisions */
   float   width;                  /* Width of output on paper */
   float   aspect;                 /* Aspect ratio of output on paper */
   float   uservals[2];            /* Array version of above */
   fint    nitems;                 /* Use in userxxx routines */
   fint    dfault;                 /* Use in userxxx routines */
   fint    r1;                     /* Return value or level */
   fint    len;                    /* Length of a string */
   fint    Flinewidth;             /* Width of lines on output device */
   fchar   devtype;                /* Device specified in 'pgbeg' */
   fint    agreed;                 /* Loop guard */


   Funit = 0;                           /* Ignored by 'pgbeg' */
   fmake( Ffile, 10 );
   Ffile = tofchar( "?" );              /* 'pgbeg' will prompt the user
                                            to supply a string. */

   nxysub[1] = nxysub[0] = 1;           /* Default no subdivisions in plot.*/
   nitems = 2;
   dfault = HIDDEN;
   r1 = userint_c( nxysub,
                   &nitems,
                   &dfault,
                   tofchar("PGMOSAIC="),
                   tofchar("View surface sub divisions in x,y:   [1,1]") );


   /* Set window and viewport */
   r1 = pgbeg_c( &Funit, Ffile, &nxysub[0], &nxysub[1] );
   if (r1 != 1)
   {
      Ferrlev = 4;
      error_c( &Ferrlev, tofchar("Cannot open output device") );
   }


   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */

   nitems = 2;
   dfault = HIDDEN;
   uservals[0] = 0.0;
   uservals[1] = 1.0;
   r1 = userreal_c( uservals, &nitems, &dfault,
                     tofchar("PAPER="),
                     tofchar("Give width(cm), aspect ratio: [0.0,1.0]") );
   if (r1 > 0)
   {
      /* If width = 0.0 then the program will select the largest */
      /* view surface */
      width  = uservals[0];
      /* Convert from cm to inches */
      width /= 2.54;
      aspect = uservals[1];
      pgpap_c( &width, &aspect );
   }


   /* Get device-type code name of the current PGPLOT device */
   /* If the destination is a printer (=every destination  */
   /* except the Tektronix device), use thick lines in the plot */

   len = 20;
   finit(devtype, len);
   pgqinf_c( tofchar("TYPE"), devtype, &len );
   if (strncmp(devtype.a, "TEK4010", 6) == 0)
   {
      /* It is a Tektronix */
   }
   else
   {
      nitems = 1;
      dfault = HIDDEN;
      do
      {
         Flinewidth = 2;
         r1 = userint_c( &Flinewidth, &nitems, &dfault,
                          tofchar("LINEWIDTH="),
                          tofchar("Give line width (1-21):  [2]") );
         agreed = ((Flinewidth >= 1) && (Flinewidth <= 21));
         if (!agreed)
            reject_c( tofchar("LINEWIDTH="),
                      tofchar("Invalid number") );
      }
      while  (!agreed);
      pgslw_c( &Flinewidth );
   }
   { /* Set viewport */
     float Xl, Xr, Yb, Yt;

     Xl = 0.2;
     Xr = 0.95;
     Yb = 0.1;
     Yt = 0.9;
     pgsvp_c(&Xl, &Xr, &Yb, &Yt);
   }
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
      static fchar    Ftask;               /* Name of current task */
      fmake( Ftask, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Ftask );                   /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }

   setfblank_c( &blank );
   fmake(Setin, BIGSTORE);

   keyword = tofchar("INSET=");
   message = tofchar("Give set (, subsets) for histogram: " );
   dfault  = NONE;
   subdim  = 0;
   scrnum  = 3;
   nsubsI  = gdsinp_c( Setin, subin, &maxsubs, &dfault, keyword,
                       message, &scrnum, axnum, axcount, &maxaxes,
                       &class, &subdim );
   setdim = gdsc_ndims_c( Setin, &toplevel );
  /*
   *----------------------------------------------------------------------------
   * Determine the edges of this its frame (FgridLO/HI)
   *----------------------------------------------------------------------------
   */
   r1 = 0;
   gdsc_range_c( Setin, &toplevel, &cwlo, &cwhi, &r1 );
   r1 = r2 = 0;
   for (m = 0; m < (int) setdim; m++) 
   {
      FgridLO[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
      FgridHI[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
   }

  /*
   *----------------------------------------------------------------------------
   * Prepare a box for INSET. Default is entire subset
   *----------------------------------------------------------------------------
   */
   dfault  = REQUEST;
   keyword = tofchar("BOX=");
   message = tofchar(" ");
   boxopt  = 0;
   scrnum  = 3;
   gdsbox_c( BgridLO, BgridHI, Setin, subin,
             &dfault, keyword, message, &scrnum, &boxopt );

   /* Count number of pixels in this substructure */
   totpixels = 1;
   /* For one subset */
   for(m = 0; m < subdim; m++)
      totpixels *= (BgridHI[m] - BgridLO[m] + 1);
   
   totpixels *= nsubsI;


   /* Read units of data values from header */
   {
      fchar    Mapunits;      
      fmake( Mapunits, 20 );
      r1 = 0;
      gdsd_rchar_c( Setin, tofchar("BUNIT"), &setlevel, Mapunits, &r1 );
      if (r1 < 0) 
      {
         mapunits[0] = '\0';
         foundunits = NO;
      }
      else
      {
         sprintf( mapunits, "%.*s", nelc_c(Mapunits), Mapunits.a );
         foundunits = YES;      
      }                                   
   }

   setfblank_c( &blank );
   range[0] = blank;
   range[1] = blank;
   nitems   = 2;
   dfault   = REQUEST;
   do
   {
      if (foundunits)
         sprintf( messbuf, "Give range in pixel values:    [all] (in:%s)",
                  mapunits );
      else
         sprintf( messbuf, "Give range in pixel values:     [all]" );
         
      r1 = userreal_c( range, &nitems, &dfault,
                       KEY_RANGE, tofchar( messbuf ) );
      agreed = ((r1 == 0) || (r1 == 2));
      if (!agreed)
         reject_c( KEY_RANGE, tofchar("Enter 0 or 2 values!") );
      else
      {
         if ((range[0] == range[1]) && (range[1] != blank))
         {
            reject_c( KEY_RANGE, tofchar("No range!") );
            agreed = 0;
         }
      }
   } while (!agreed);

  /*
   *----------------------------------------------
   * Loop over all subsets and read data.
   *----------------------------------------------
   */
   pixelcount = 0;


   initplot();                      /* Initialize plot software */

   if (nsubsI > 1)
   /*-------------------------------------------------------------------*/
   /* It is possible to calculate the data min & max in both directions */
   /* (bins and heights) for all subsets. It is also possible to        */
   /* inherit the values of the 'previous' subset. Those are the values */
   /* calculated for the first subset or some user specified values     */
   /* (asked as hidden keywords). In the latter case it will be possible*/
   /* to create plots that can be compared in x & y.                    */
   /*-------------------------------------------------------------------*/
   {
      autoscale = toflog(YES);
      nitems = 1;
      dfault = REQUEST;
      (void) userlog_c( &autoscale, &nitems, &dfault,
                        tofchar("AUTOSCALE="),
                        tofchar("Automatic scaling of each plot:   [Y]/N" ) );
   }


   /* Some initializations */
   fmake( Subsetstring, 80 );
   /* Create a border for the table */
   {
      int l;
      for (l = 0; l < 70; border[l++] = '=');
      border[l] = '\0';                      /* Close array */
   }

   /* Now loop over all subsets... */

   for(subnr = 0; subnr < nsubsI; subnr++)
   {
      (void) getuserinp( Setin,              /* Get minmax of this subset */
                         subin[subnr],
                         subnr,
                         &datamin,
                         &datamax,
                         range, 
                         &numbins,           /* output: number of used bins */
                         &binwidth );        /* output: width of each bin */

      /* Create the X-array and initialize counts array */
      {         
         int j;
         Xarray = (float *) calloc( numbins, sizeof(float) );
         if (Xarray == NULL)
         {
            anyoutC( 1, "Cannot allocate space for histogram X array" );
            break;
         }
         counts  = (float *) calloc( numbins, sizeof(float) );
         if (counts == NULL)
         {
            anyoutC( 1, "Cannot allocate space for histogram Y array" );
            free( Xarray );
            break;
         }
                           
         for (j = 0; j < numbins; j++)
            Xarray[j] = datamin + j * binwidth;

      }

      /* Make coordinate words for current subset corners */
      cwlo      = gdsc_fill_c( Setin, &subin[subnr], BgridLO );
      cwhi      = gdsc_fill_c( Setin, &subin[subnr], BgridHI );
      tid       = 0;
      inrange   = 0;
      outrange  = 0;
      maxcounts = 0.0;
      Xmodeindx = 0;
      sum       = 0;
      ntot      = 0;                     /* Initialize 'statr' function */
      nblanks   = 0;
      do {
      	 gdsi_read_c( Setin,
      	              &cwlo, &cwhi,
      	              image,
                      &maxIObuf,
                      &pixelsdone,
                      &tid );
         filcount = 0;
         for (i = 0; i < pixelsdone; i++)
         {
            value = image[i];
            if (value == blank)
               nblanks++;
            else {
               if (range[0] != blank) {
                  /* A range was given */
                  if (range[1] > range[0])
                     inside = ( value >= range[0] && value <= range[1] &&
                                value >= datamin  && value < datamax );
                  else
                     inside = ( ( value > range[0] || value < range[1] ) &&
                                ( value >= datamin && value < datamax ) );

               }
               else
               {
                  /* No range was given */
                  if ((value >= datamin) && (value < datamax))
                     inside = YES;
                  else
                     inside = NO;
               }

               if (inside)
               {
                  filter[filcount++] = value;
                  inrange++;
                  indx = (int) ((value - datamin) / binwidth);
                  ++counts[indx];
                  if (counts[indx] > maxcounts)
                  {
                     maxcounts = counts[indx];
                     Xmodeindx = indx;
                  }
                  sum += value;
               }
               else {
                  outrange++;
               }
            }

         }
         pixelcount += pixelsdone;

         /* The filtered array does not contain any blanks.         */
         /* These blanks are already counted. Instead of 'nblanks', */
         /* the dummy variable 'Fdummy' is used in 'statr'          */

         statr_c( filter,
                  &filcount,
                  &minval,
                  &maxval,
                  &mean,
                  &rms,
                  &Fdummy,
                  &ntot );
      } while (tid != 0);


      /* Create table with statistics */

      /* create string with coordinate and grid vector */
      subsetlevel = subin[subnr];
      showsubset( Setin,
                  &subsetlevel,
                  axnum,
                  &subdim,
                  &setdim,
                  Subsetstring );

      sprintf( line1, "Set: %-.*s  %-.*s",
               (int) nelc_c(Setin), Setin.a,
               (int) nelc_c(Subsetstring), Subsetstring.a );
      if (subsetlevel > 0)
      {
         /* Create string with physical coordinates */
         showcoord( Setin,
                    &subsetlevel,
                    axnum,
                    &subdim,
                    &setdim,
                    Subsetstring );
         sprintf( line2, "=%.*s",
                  (int) nelc_c(Subsetstring), Subsetstring.a );
         strcat(line1, line2 );
      }

      anyoutC( 0, border );
      anyoutC( 0, line1 );
      anyoutC( 0, border );
      if (range[0] != blank)
         sprintf( messbuf , "Range in selected data:        (%.6G, %.6G)", range[0], range[1] );
      else
         sprintf( messbuf , "Range in selected data:        All values" );
      anyoutC( 0,  messbuf );
      if (range[0] != blank)
      {
         sprintf( messbuf,  "Pixels inside range:           %d ", inrange );
         anyoutC( 0, messbuf );
         sprintf( messbuf,  "Pixels outside range:          %d ", outrange );
         anyoutC( 0, messbuf );
      }
      else
      {
         sprintf( messbuf,  "Total number of valid pixels:  %d ", inrange );
         anyoutC( 0, messbuf );
      }
      sprintf( messbuf, "Number of blanks:              %d", (int) nblanks );
      anyoutC( 0, messbuf );
      sprintf( messbuf, "Min. and max. in data:         (%.6G, %.6G)", minval, maxval );
      anyoutC( 0, messbuf );
      sprintf( messbuf, "Min. and max. in plot:         (%.6G, %.6G)", datamin, datamax );
      anyoutC( 0, messbuf );
      sprintf( messbuf, "Sum :                          %.6G", sum );
      anyoutC( 0, messbuf );
      sprintf( messbuf, "Mean:                          %.6G", mean );
      anyoutC( 0, messbuf );
      if (rms == blank)
         sprintf( messbuf, "Rms:                            ***" );
      else
         sprintf( messbuf, "Rms:                           %.6G", rms );
      anyoutC( 0, messbuf );
      sprintf( messbuf, "Mode:                          X=(%.6G, %.6G),  (Y=%d)",
               Xarray[Xmodeindx], Xarray[Xmodeindx+1], (int) maxcounts );
      anyoutC( 0, messbuf );
      anyoutC( 0, border );


      /*------------------------------------------------------------*/
      /* Default parameters for gaussian. The gauss parameters are  */
      /* determined as follows: If A is the amplitude of a gaussian */
      /* curve, s is its sigma and I is the total area under this   */
      /* curve then: I[A*e^(-x*x/2*s*s)] = A*s*sqrt(2pi)            */
      /* We can plot a gauss with an amplitude A because we know    */
      /* the total area of the histogram (total number of valid     */
      /* pixels times the bin width. Its rms is the rms of all the  */
      /* data.                                                      */
      /*------------------------------------------------------------*/
      gausspar[0] = ((float)inrange*binwidth)/(sqrt(2*PI)*rms);
      gausspar[1] = mean;
      gausspar[2] = rms;
      gausspar[3] = 0.0;
      getgauss( gausspar );          /* Does user want to include a gaussian? */


      if (tofile)
         logtable( Xarray, numbins, counts );

      makeplot( Xarray,
                numbins,
                counts,
                datamin, datamax,
                maxcounts,
                Setin, subin[subnr],
                gausspar,
                binwidth,
                subnr,
                line1 );
      if (Xarray) 
         free( Xarray );
      if (counts)
         free( counts );                
   }
   pgend_c();   
   finis_c();                                                  /* Quit Hermes */
   return( EXIT_SUCCESS );        /* == 0, see stdlib.h */
}
