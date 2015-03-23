/*
                            COPYRIGHT (c) 2000
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             flux.dc1

Program:       FLUX

Purpose:       Program to calculate flux in user defined box.

Category:      ANALYSIS, CALCULATION, TABLES

File:          flux.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give input set, subsets:
               Input set (and subsets). Maximum number of subsets is 2048.

** POLYGON=    Define polygons instead of boxes?                     Y/[N]
               If you want the flux in a polygon select POLYGON=Y.
               The keyword VERTICES%= (%=1,2,3,...) is prompted instead
               of BOX%= The Polygon option can be used for two dimensional
               subsets only.

   VERTICES%=  Give vertices of polygon:
               Asked if POLYGON=Y
               The vertices are given in two dimensional coordinate pairs
               x1,y1, x2,y2, .....
               The minimum number of vertices is 3, the maximum is 128.
               The program closes the polygon. The maximum number of
               pixels in the box that encloses the polygon cannot exceed
               16384.

   BOX%=       Frame for input subsets.                    [entire subset]
               Asked if POLYGON=N
               It is possible to give more than one box (% indicates
               box number). The keyword is repeated until user pressed
               carriage return.

   CLEANBEAM=  Do you want to give values of a clean beam?           Y/[N]
               If data is radio data and there is no AP available,
               use CLEANBEAM=Y and BEAM= to calculate the sum in the AP.


   HEADER=     Antenna Pattern from header?                          [Y]/N
               If your data is radio data and there is a reference
               to an antenna pattern (AP) in the header, you can choose to
               select this antenna pattern by specifying HEADER=Y. If you
               want to get the APSET prompt to select another AP, specify
               HEADER=N

               if HEADER=Y:

** APSET=      Give set, subs. of ant.pat.:               [AP from header]

               if HEADER=N or no reference to AP:

   APSET=      Give set, subs. of ant.pat.:                       [NO AP]]
               Set and subset(s) of AP. This keyword is
               prompted if your data is radio data and there is no
               reference to an AP in the header, or there was a reference,
               but you selected HEADER=N. The program recognizes several
               radio telescopes.

   BEAM=       Maj. and min. axis of beam (arcsec)                  [none]
               Asked if CLEANBEAM=YES
               The sum in the AP is now the integral from -inf. to inf.
               of a gaussian with the specified FWHM.

   CDELT%=     Give grid spacing in 'axis type' in 'axis units':
               If a clean beam is wanted, the grid spacings are needed.
               If a spacing is not found in the header, you are prompted
               to give the spacing in units as found in the header.

   FREQUENCY=  Give frequency:                                 [1.415 GHZ]
               If FLUX cannot find a proper frequency for primary beam,
               correction, a frequency is asked with FREQUENCY=
               The keyword accepts a number and a unit. If no unit is given,
               the number is in Ghz. Otherwise, a conversion is done to Ghz.
               The possible units to convert from are for example MHz,
               Hz, but also cm (all upper- or lowercase). The keyword
               is asked unhidden only once.

   GRDEVICE=   Plot device:                              [List of devices]
               Destination of plot, Screen or Hardcopy.

   FILENAME=   Name of ASCII file:                     [No output to file]
               If a name is specified, an ASCII file is created
               where fit parameters are listed in a row. If you press
               carriage return, there will be no output to an ASCII file.

   APPEND=     File exists, ok to append?                            [Y]/N
               The file specified in FILENAME= already exists. You
               can append to this file with APPEND=Y. If APPEND=N
               you will be prompted for another name.


               PLOTTING:

   OPTION=     0)Exit  1)Sum  2)SumPBC  3)Flux  4)FluxPBC  5)GRdev    [3]
               Plot results. Default is a plot of the flux if the
               flux is calculated else the default is a plot of the
               sum. Option 5 offers the possibility to change the 
               plot device. You are prompted with the GRDEVICE= keyword
               again. Also the hidden 'PG" keywords (PGMOSAIC=, PGPAPER= etc.
               accept new values if you define them at this point. 

** PGMOSAIC=   View surface sub divisions in x,y:             [calculated]
               View surface can contain a number of plots in
               in X and Y direction (mosaic). Default the program tries
               to put the same number of columns as rows in 1 plot.

** PGPAPER=    Give width(cm), aspect ratio:                  [calc, calc]
               Aspect ratio is height/width.

** PGBOX=      Corners of box Xl,Yl,Xh,Yh:     [default by application]
               It is possible to overrule the calculated
               PGPLOT box size with PGBOX=. The coordinates (x,y) of
               the lower point are given first.

** PGTICKS=    Coord. int. between maj. tick marks in X,Y:    [calc, calc]
               World coordinate interval between major tick marks
               on X and Y axis. The default is 0 0 which results in 
               values calculated by PGPLOT.
               
** PGNSUB=     Number of subintervals between maj. ticks:     [calc, calc]
               The number of subintervals to divide the major
               coordinate interval into. The default is calculated 
               by PGPLOT.
               
** PGCOLOR=    Give color 1..15:                                       [1]
               See description for the available colors.

** PGWIDTH=    Give line width 1..21:                                  [1]

** PGHEIGHT=   Give character height:                                [1.0]

** PGFONT=     Give font 1..4:                                         [2]

** TABNAME=    Give name of table to store results:                 [FLUX]
               Columns are created on set level.

** TABAPPEND=  Append to existing columns?                           Y/[N]
               If a table already exists, it is possible to append
               to this table with TABAPPEND=Y
               The default always creates a new table.


Description:   Inside the box in which one wants to know the flux, all
               non blank pixel values are summed and then, if possible,
               the sum is converted to a flux in appropriate units. This
               conversion is case dependent:

               a) RADIO DATA

               Data is considered radio data, if the instrument name
               in the header is equal to one of 'WSRT', 'VLA' ,'FST',
               or 'DRAO'. Primary Beam Correction is available for
               'WSRT', 'VLA' ,'FST'.

               What is needed for the conversion from sum to flux
               is the 'SUMAP' in an equivalent box, i.e. the sum
               over all pixels in a box of the corresponding antenna
               pattern (AP) that has a size equal to the size of the box
               in which the flux is wanted. A warning is generated if
               the AP cannot be summed symmetrically. The axes in such
               a box will be extended in length until they are all
               symmetrical. Be warned that your box is changed then!
               Also a Primary Beam Corrected (PBC) sum and flux is
               determined. This is a correction for the response of one
               antenna and it depends on distance of a position to the
               beam center and on frequency. The distance is corrected
               for projection effects. If it is greater than a certain
               distance (validity range of PBC), there is no contribution
               to the sum or flux.

               PBC possible if:

               1) An instrument name can be found in the set header.
               2) The instrument is one of WSRT, VLA or FST.
               3) The dimension of the selected subset must be 1 or 2
               4) The subset axes are spatial.
               5) The subset is 2-dimensional, and the spatial axes
                  are be different.
               6) The reference values CRVAL% & CDELT% can be found in
                  the header.
               7) There is a spectral axis available and the units can
                  be converted to GHZ (if no spectral axis is available,
                  a frequency is asked with FREQUENCY=).


               If the RA-DEC pointing center of the telescope can not be
               found in the header,  it is assumed they are equal to
               CRVAL1 and CRVAL2 (position of grid 0,0). If an image value
               is not a blank and its position is within the validity
               region of the primary beam correction, the value is
               corrected and added to 'sumPBC'.

               If a polygon is defined, the AP is centered at the
               position weighted center of the data inside the polygon
               and only the part of the AP inside the polygon is summed.

               b) NON-RADIO DATA

               The grid spacings from the header are read and
               converted to steradian/pixel. The sum in the
               requested box is multiplied by this number.



               'flux' has a different definitions. For radio data you
               need an antenna pattern because sum = sum / sumAP. For
               other data: sum = sum * steradians/pixel.
               The units of the flux are the units found in the header.

               'fluxPBC' is calculated if there is a 'sumPBC' and an
               antenna pattern. It is comparable to 'flux', but in this
               case the sum is the primary beam corrected sum. If the
               data was in Westerbork Units (WU), a conversion to mJy's
               is applied, where 1 W.U. ==> 5 mJy/Beam.


               Plotting:

               It is possible for each given box, to make a plot of
               sum against subset index. The subset index always runs
               from 0 to the number of subsets you specified minus 1.
               A plot can be created only if the number of subsets
               is greater than 1;


               Tables:

               Results are always stored in a table. The name of the
               table is FLUX by default. You can change this with
               TABNAME=. The name is appended by a number that corres-
               ponds to the number of the used box (FLUX1, FLUX2, ...).
               The names of the columns are:

               SUM            Sum of data
               SUMPBC         Primary beam corrected sum
               FLUX           Flux
               FLUXPBC        Primary beam corrected flux
               SUMAP          Sum in Antenna pattern
               NDATA          Number of non blank points in statistics
               NBLANKS        Number of blanks in statistics
               XLO            Lowest X value of box
               YLO            Lowest Y value of box
               XHI            Highest X value of box
               YHI            Highest Y value of box
               SUBGRIDn       Used subset in grid coordinates
                              n is a number of an axis outside
                              the subset. If there are no axes outside
                              the subset, this column is not created.
               Axisname       If there are n axes outside the subset,
                              there will be n extra columns with the
                              name equal to the name of the (secondary)
                              axis, containing physical units.

               Use the application TABLE to print, plot and process the
               column entries.




               Color indices:

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

Updates:       Mar 20,  1990: VOG, Document created.
               Apr 10,  1992: VOG, New call to PRIBEAM.
               May 26,  1992: VOG, Rewritten in C.
               Oct  5,  1992: VOG, Units bug with 'strtok'
                                   Tables implemented.
               Apr 16,  1996: VOG, Implemented code for unequal column 
                                   lengths when appending tables.

#<
*/

/*  flux.c: include files     */

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
#include    "setdblank.h"    /* Subroutine to set a data value to the universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "status.h"       /* Display additional in the "RUNNING" status display. */

/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "usercharu.h"
#include    "userchar.h"
#include    "userfio.h"
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/

/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_name.h"    /* Extract name of axis. */
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "gdsd_readc.h"   /* Read descriptor item */
#include    "gdsd_rdble.h"   /* Read double from descriptor */
#include    "gdsd_rchar.h"   /* Read string from descriptor */


/* PGPLOT includes */

#include    "pgplot.h"


/* Related to tables */

#include    "gdsa_crecol.h"
#include    "gdsa_delcol.h"
#include    "gdsa_colinq.h"
#include    "gdsa_wcreal.h"
#include    "gdsa_wcdble.h"
#include    "gdsa_wcint.h"


/* Miscellaneous includes; */

#include    "pribeam.h"
#include    "axunit.h"
#include    "axcoord.h"
#include    "showsub1.h"
#include    "cotrans.h"


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
#define MAXBOXES       256             /* Max. number of boxes in one run */
#define MAXBUF         16384           /* Buffer size for I/O */
#define MAXOPT         4
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Key length in input functions */
#define MESLEN         80              /* Message length in input functions */
#define FITSLEN        20              /* Length of fits item from header */
#define LONGSTR        180
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0
#define MINVERT        3
#define MAXVERT        128
#define VARLEN         132
#define ITEMLEN        10
#define MAXCOLNAM      8
#define FILENAMELEN    80

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX        tofchar("BOX1=")
#define MES_BOX        tofchar(" ")
#define KEY_POLYGON    tofchar("POLYGON=")
#define MES_POLYGON    tofchar("Define polygons instead of boxes?         Y/[N]")
#define MES_VERTICES   tofchar("Give vertices of polygon:          [abort loop]")
#define KEY_FILENAME   tofchar("FILENAME=")
#define MES_FILENAME   tofchar("Name of ASCII file:     [No output to file]")
#define KEY_APPEND     tofchar("APPEND=")
#define MES_APPEND     tofchar("File exists, ok to append?    [Y]/N")




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

/* Same for AP set */

static fchar    APSetin;
static fint     APsubin[MAXSUBSETS];
static fint     APnsubs;
static fint     APaxnum[MAXAXES];
static fint     APaxcount[MAXAXES];
static fint     APsubdim;

/* Box and frame related */

static fint     flo[MAXAXES];       /* Low  edge of frame in grids */
static fint     fhi[MAXAXES];       /* High edge of frame in grids */
static fint     apflo[MAXAXES];     /* Low  edge of frame in grids in AP */
static fint     apfhi[MAXAXES];     /* High edge of frame in grids in AP */
static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */
static fint     bloa[MAXBOXES][MAXAXES];       /* Array version */
static fint     bhia[MAXBOXES][MAXAXES];       /* Array version */
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
static float    image[MAXBUF];      /* Buffer for read routine. */
static fint     subnr;              /* Counter for subset loop. */

/* Statistics */

static float    sum[    MAXBOXES][MAXSUBSETS];
static float    sumPBC[ MAXBOXES][MAXSUBSETS];
static float    sumAP[  MAXBOXES][MAXSUBSETS];
static fint     nblanks[MAXBOXES][MAXSUBSETS];
static fint     npoints[MAXBOXES][MAXSUBSETS];
static float    flux[   MAXBOXES][MAXSUBSETS];
static float    fluxPBC[MAXBOXES][MAXSUBSETS];
static fint     box;

/* Defining regions */

static fint     vertXa[MAXBOXES][MAXVERT];
static fint     vertYa[MAXBOXES][MAXVERT];
static fint     vertX[MAXVERT+1];
static fint     vertY[MAXVERT+1];
static bool     polygon;
static int      nvert[MAXBOXES];
static int      mask[MAXBUF];
static fint     Xcent, Ycent;       /* Weighted center positions of polygon */

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
static float Xmin, Xmax;
static float Ymin[MAXOPT], Ymax[MAXOPT];
static char  Xtitle[STRLEN];
static char  Ytitle[STRLEN];
static char  Toptitle[STRLEN];


/* Units related */

static char     sumunit[FITSLEN];
static char     sumPBCunit[FITSLEN];
static char     fluxunit[FITSLEN];
static char     fluxPBCunit[FITSLEN];
static char     sumAPunit[FITSLEN];
static char     levstr[MESLEN];
static char     axnames[MESLEN];
static char     tabheader[LONGSTR];
static char     unitheader[LONGSTR];
static char     borderheader[LONGSTR];
static char     boxstr[STRLEN];
static fchar    subsetstr;


/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[LONGSTR];       /* All purpose character buffer. */
static int      i,j,m;              /* Various counters. */
static bool     agreed;             /* Loop guard. */
static fchar    key, mes;
static bool     correc;             /* Is primary beam corr. possible? */
static bool     radio;              /* Is the data radio data? */
static fint     PBCopt;
static bool     equaledges;
static fint     boxcount;
static fint     nitems;
static bool     AP;                 /* Use antenna pattern? */
static bool     clean;              /* Use clean beam? */
static double   cdeltx, cdelty;     /* Grid spacings for clean beam */
static fchar    errtxt;
static bool     sterads;
static double   srpix;
static int      len;
static bool     westerbork;
FILE            *fp = NULL;
static char     filename[LONGSTR];
static fint     option;
static int      counter[MAXOPT];
static char     diskmess[LONGSTR];
static float    dumx = 0.0;
static float    dumy = 0.0;





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



FILE *fopenC( char *filename )
/*---------------------------------------------*/
/* Open file to write data extern. The         */
/* macro 'fmake' must be available.            */
/*---------------------------------------------*/
{
   fchar    Filename;
   bool     append;
   fint     request = 1;
   fint     dfault;
   fint     n;
   fint     nitems;
   fint     agreed;
   FILE     *fp;

   dfault = request;
   fmake( Filename, FILENAMELEN );
   do {
      append = toflog(YES);                               /* Default APPEND=Y */
      n = usertext_c( Filename,
                      &dfault,
                      KEY_FILENAME,
                      MES_FILENAME );
      if (n == 0) return NULL;

      strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */

      fp = fopen(filename, "r");
      if (fp != NULL) {                                    /* The file exists */
         nitems = 1;
         n = userlog_c( &append,
                        &nitems,
                        &dfault,
                        KEY_APPEND,
                        MES_APPEND );
         append = tobool( append );
         fclose( fp );
         cancel_c( KEY_APPEND );
      }
      if (!append) {
          cancel_c( KEY_FILENAME );
          agreed = NO;
      }
      else {
         fp = fopen(filename, "a");
         agreed = (fp != NULL);
         if (!agreed) {
            reject_c( KEY_FILENAME,
                      tofchar("Cannot open, try another!") );
         }
      }
   } while (!agreed);
   return( fp );
}


void initplot( int nplots )
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


   /* Calculate a default layout for the plots */

   nxysub[0] = (int) sqrt( (float) nplots );
   nxysub[1] = nplots / nxysub[0];
   if ( (nplots % nxysub[0]) > 0) nxysub[1]++;
   nitems = 2;
   dfault = HIDDEN;
   r1 = userint_c( nxysub,
                   &nitems,
                   &dfault,
                   tofchar("PGMOSAIC="),
                   tofchar("View surface sub divisions in x,y:   [1,nplots]") );

   unit = 0;
   Devspec = tofchar("?");
   r1 = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );
   if (r1 != 1) error_c( &errlev, tofchar("Cannot open output device") );

   /* No PGPLOT's NEXTPAGE= keyword */
   pageoff = toflog( 0 );
   pgask_c( &pageoff );

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
      pgpap_c( &width, &aspect );
   }

   /* Set viewport */
   xl = 0.2; xr = 0.95;
   yb = 0.1; yt = 0.9;
   pgsvp_c( &xl, &xr, &yb, &yt );
}


void drawbox( float Xmin, float Ymin, float Xmax, float Ymax,
              char *Xtitle, char *Ytitle, char *Toptitle )
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
   float  plotbox[4];                         /* Corners of draw box. */
   fint   color;
   fint   font;
   fint   nxsub, nysub;
   float  xtick, ytick;
   char   message[180];


   pgpage_c();                                /* Advance to new page. */

   /* Increase the size of the box a little */
   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Xmin -= delta; Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Ymin -= delta; Ymax += delta;
   plotbox[0] = Xmin; plotbox[1] = Ymin;      /* Get size from user input */
   plotbox[2] = Xmax; plotbox[3] = Ymax;
   nitems = 4; dfault = HIDDEN;
   sprintf( message, "Corners of box Xl,Yl, Xh,Yh:  [%f,%f,%f,%f]", Xmin,Ymin,Xmax,Ymax );
   r1 = userreal_c( plotbox,
                    &nitems,
                    &dfault,
                    tofchar("PGBOX="),
                    tofchar( message ) );
   Xmin = plotbox[0]; Ymin = plotbox[1];
   Xmax = plotbox[2]; Ymax = plotbox[3];
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );    /* Set the window */

   color = 1; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &color,
                   &nitems,
                   &dfault,
                   tofchar("PGCOLOR="),
                   tofchar("Give color 1..15:        [1]") );
   if (color > 15) color = 15;
   if (color < 1 ) color =  1;
   pgsci_c( &color );

   lwidth = 1; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &lwidth,
                   &nitems,
                   &dfault,
                   tofchar("PGWIDTH="),
                   tofchar("Give line width 1..21:        [1]") );
   if (lwidth > 21) lwidth = 21;
   if (lwidth < 1 ) lwidth =  1;
   pgslw_c( &lwidth );                        /* Set line width. */

   charsize = 1.0; nitems = 1; dfault = HIDDEN;
   r1 = userreal_c( &charsize,
                    &nitems,
                    &dfault,
                    tofchar("PGHEIGHT="),
                    tofchar("Give character height:     [1.0]") );
   pgsch_c( &charsize );                     /* Character height. */

   font = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &font,
                   &nitems,
                   &dfault,
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
   
   {
      float   ticks[2];
      fint    nsub[2];
   
      ticks[1] = ticks[0] = 0.0;      
      nitems = 2; 
      dfault = HIDDEN;
      r1 = userreal_c( ticks, &nitems, &dfault,
                       tofchar("PGTICKS="),
                       tofchar("Coord. int. between maj. tick marks in X,Y: [calc, calc]" ) );
      xtick = ticks[0];
      ytick = ticks[1];
      
      nsub[1] = nsub[0] = 0.0;
      nitems = 2; 
      dfault = HIDDEN;
      r1 = userint_c( nsub, &nitems, &dfault,
                      tofchar("PGNSUB="),
                      tofchar("Number of subintervals between maj. ticks: [calc, calc]" ) );
      nxsub = nsub[0];
      nysub = nsub[1];
   }
   pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
            tofchar("BCNSTV"), &ytick, &nysub );
   pglab_c( tofchar(Xtitle), tofchar(Ytitle), tofchar(Toptitle) );
}



void getspacing( double *cdeltx, double *cdelty,
                 fchar Setin, fint *axnum )
/*-----------------------------------------------------------*/
/* Get gridspacing from header. If spacings can not be found */
/* prompt for values.                                        */
/*-----------------------------------------------------------*/
{
   int    m;
   char   mess1[80], mess2[80];
   fint   r1, r2;
   fint   setlevel = 0;
   double cdelt[2];
   fint   nitems;
   fint   dfault;
   fchar  ctype, cunit;


   fmake( ctype, FITSLEN );
   fmake( cunit, FITSLEN );
   for(m = 0; m < 2; m++) {
      sprintf( mess1, "CDELT%d", axnum[m] );
      r1 = 0;
      gdsd_rdble_c( Setin, tofchar(mess1), &setlevel, &cdelt[m], &r1 );
      if (r1 < 0) {
         r2 = 0;
         gdsc_name_c( ctype,               /* axis name */
                      Setin,               /* input set name */
                      &axnum[m],           /* axis number */
                      &r2 );               /* GDS error return */
         if (r2 < 0) ctype = tofchar("?");
         sprintf( mess2, "FLUX: No grid spacing for %.*s in header!",
                  nelc_c(ctype), ctype.a );
         anyoutC( 3, mess2 );
         axunit_c( Setin,                  /* input set name */
                   &axnum[m],              /* axis number */
                   cunit );                /* axis units */

         sprintf( mess2, "Give grid spacing in %.*s in %.*s:",
                  nelc_c(ctype), ctype.a,
                  nelc_c(cunit), cunit.a );
         sprintf( mess1, "CDELT%d=", axnum[m] );
         mes    = tofchar( mess2 );
         key    = tofchar( mess1 );
         dfault = NONE;
         nitems = 1;
         r2 = userdble_c( &cdelt[m], &nitems, &dfault, key, mes );
      }
   }
   *cdeltx = cdelt[0];
   *cdelty = cdelt[1];
}


float getcleanval( double cdeltx, double cdelty )
/*--------------------------------------------------*/
/* Ask for axes of clean beam and calculate area    */
/* under given two dim. gauss.                      */
/*--------------------------------------------------*/
{
   fint    r1;
   fint    nitems;
   fint    dfault;
   double  val;
   double  beam[2];

   nitems  = 2;
   dfault  = NONE;
   key     = tofchar("BEAM=");
   mes     = tofchar("Maj. and min. axis of beam (arcsec)" );
   r1      = userdble_c( beam, &nitems, &dfault, key, mes );
   cdeltx *= 3600.0;
   cdelty *= 3600.0;
   val     = ( PI/(4.0*log(2.0)) ) * (beam[0]/cdeltx) * (beam[1]/cdelty);
   return( (float) fabs(val) );
}


float getsumAP( fchar APSetin, fint *apflo, fint *apfhi,
                fint box, fint APsubset, fint APsubdim,
                bool polygon, fint Xcent, fint Ycent )
/*--------------------------------------------------------------*/
/* Edges of the current box are in the global arrays bloa, bhia */
/* The sizes of the APbox will always be symmetrically.         */
/* In the polygon case, the AP is centered at position X,Ycent  */
/* in the original box. Only the AP within the polygon is       */
/* summed.                                                      */
/*--------------------------------------------------------------*/
{
   int     i, j;
   fint    boxlen;
   fint    aplen;
   fint    aplo[MAXAXES], aphi[MAXAXES];
   fint    apcenter;
   fint    cwlo, cwhi;
   float   localsumAP;
   fint    numpixels;
   fint    numinreadbuf;
   fint    stilltoread;
   fint    tid;
   float   data[MAXBUF];
   bool    sym;
   int     first;


   if (!polygon) {
      /* Check on symmetry first */
      first = YES;
      for (i = 0; i < (int) APsubdim; i++) {
         sym = ( (bhia[box][i]-bloa[box][i]) % 2 == 0 );
         if (!sym) {
            int  middle, l1, l2;
            if (first) {
               sprintf( message, "FLUX: With BOX%d, the AP cannot be summed symmetrically!", box+1 );
               anyoutC( 3, message );
               anyoutC( 3, "      FLUX will increase this box size so that it can sum the AP symmetrically.");
               first = NO;
            }
            /* Increase axis */
            middle = (bhia[box][i]+bloa[box][i]) / 2;
            l1 = ABS( middle - bhia[box][i] );
            l2 = ABS( middle - bloa[box][i] );
            /* Axis was not symmetrical so l1 != l2. */
            if (l1 < l2) {
               bhia[box][i]++;
            } else {
               bloa[box][i]--;
            }
            if (bhia[box][i] > fhi[i]) {
               anyoutC( 3, "FLUX: Cannot extend axis beyond highest value!");
               bhia[box][i]--;
            }
            if (bloa[box][i] < flo[i]) {
               anyoutC( 3, "FLUX: Cannot extend axis below lowest value!");
               bloa[box][i]++;
            }
         }
      }

      for (j = 0; j < (int) APsubdim; j++) {
         /* Length of 1 axis of subset box */
         boxlen = (bhia[box][j] - bloa[box][j]) / 2;
         /* Length of 1 axis of AP box */
         aplen = (apfhi[j] - apflo[j]) / 2;
         if (boxlen > aplen) {
            anyoutC(3, "FLUX: AP too small for current box!" );
            return( blank );
         } else {
            apcenter = (apfhi[j] + apflo[j]) / 2;     /* Center of 1 axis */
            aplo[j] = apcenter - boxlen;              /* Construct symmetrical AP */
            aphi[j] = apcenter + boxlen;
         }
      }
      status_c( tofchar("Calculating sum AP in box") );
   } else {
      fint   cpos[2];
      cpos[0] = Xcent;   cpos[1] = Ycent;
      /* Create AP box with shifted center */
      for (j = 0; j < 2; j++) {
         apcenter = (apfhi[j] + apflo[j]) / 2;
         aplo[j] = apcenter - ABS(cpos[j] - bloa[box][j]);
         aphi[j] = apcenter + ABS(cpos[j] - bhia[box][j]);
         if ( (aplo[j] < apflo[j]) || (aphi[j] > apfhi[j]) ) {
            anyoutC(3, "FLUX: AP too small to center" );
            return( blank );
         }
      }
      status_c( tofchar("Calculating sum AP in polygon") );
   }


   /* Calculate sumAP for current ant. pat. */
   cwlo = gdsc_fill_c( APSetin, &APsubset, aplo );
   cwhi = gdsc_fill_c( APSetin, &APsubset, aphi );
   localsumAP  = 0.0;                              /* Reset statistics for subsets */
   numpixels   = 0;
   stilltoread = 1;
   for(i = 0; i < (int) APsubdim; i++) {           /* Calculate number of pixels in this (sub) AP */
      stilltoread *= (aphi[i] - aplo[i] + 1);
   }
   tid = 0;                                        /* Reset transfer id */
   do {
      numinreadbuf = MYMIN( MAXBUF, stilltoread ); /* # per iteration cannot exceed maxIObuf */
      numpixels = 0;
      gdsi_read_c( APSetin, &cwlo, &cwhi, data,
                   &numinreadbuf, &numpixels, &tid );
      for(j = 0; j < numpixels; j++) {
         if (data[j] != blank) {
            if (!polygon) {
               localsumAP += data[j];
            } else {
               if (mask[j]) localsumAP += data[j];
            }
         }
      }
      stilltoread -= numinreadbuf;
   } while( stilltoread != 0 );
   return( localsumAP );
}



static fint showcoord( fchar Setin, fint *subin, fint *axnum, fchar subsetstr )
/*-----------------------------------------------------------------------------*/
/* Create the string 'subsetstr' containing information about the axes.        */
/* Return 1 if there are subset axes and transformation is possible.           */
/* Return 0 if there are no subset axes.                                       */
/* Return cotrans result if transformation was not possible.                   */
/*-----------------------------------------------------------------------------*/
{
   int    n;
   fint   err;
   fint   res;
   fint   grid;
   char   dummystr[80];
   fint   setdim, subdim;
   fint   zero = 0;

   /* Coordinate transformation */

   fint     direction;           /* grid coord. -> physical coord. */
   double   coordin[MAXAXES];    /* Grids before transformation */
   double   coordout[MAXAXES];   /* Physical coordinates after transformation */


   setdim = gdsc_ndims_c( Setin, &zero );                   /* dimension of set */
   subdim = gdsc_ndims_c( Setin, subin );                /* dimension of subset */
   subsetstr.a[0] = '\0';
   if (setdim == subdim) return(0);                         /* No subset axes */
   for (n = 0; n < setdim; n++ ) {
      if (n >= subdim) {
         err = 0;
         grid = gdsc_grid_c( Setin, &axnum[n], subin, &err );
         coordin[ (int) axnum[n]-1 ] = (double) grid;
      }
      else {
         coordin[ (int) axnum[n]-1 ] = 0.0;
      }
   }
   direction = 1;                           /* grid coord. -> physical coord. */
   res = cotrans_c( Setin, subin, coordin, coordout, &direction );
   if (res < 0) return(res);
   for (n = subdim; n < setdim; n++ ) {
      sprintf( dummystr, "%.6g", coordout[ (int) axnum[n]-1 ] );
      if ( n == subdim ) {
         sprintf( subsetstr.a, "%s", dummystr );
      } else {
         sprintf( subsetstr.a, "%.*s, %s", nelc_c(subsetstr), subsetstr.a, dummystr );
      }
   }
   return(1);
}


static fint getsubsetunits( fchar Setin, fint *subin, fint *axnum,
                            char *levstr, fchar Units )
/*--------------------------------------------------------*/
/* Return the units of the subset axes as used in cotrans */
/*--------------------------------------------------------*/
{
   fint   setdim, subdim;
   fint   zero = 0;
   fint   res;
   fchar  cunit;
   int    l, n;
   fint   len;
   int    i;

   len = FITSLEN;
   fmake( cunit, FITSLEN );
   setdim = gdsc_ndims_c( Setin, &zero );                   /* dimension of set */
   subdim = gdsc_ndims_c( Setin, subin );                   /* dimension of subset */
   levstr[0] = '\0';
   if (setdim == subdim) return(0);                         /* No subset axes */
   for (n = subdim, i = 0; n < setdim; n++ ) {
      res = axunit_c( Setin, &axnum[n], cunit );
      if (res != 0) {
         return(-1);
      } else {
         l = (int) nelc_c(cunit);
         if (n == subdim) {
            sprintf( levstr, "%.*s", l, cunit.a );
         } else {
            sprintf( levstr, "%.*s, %.*s",
                     strlen(levstr), levstr,
                     l, cunit.a );
         }
         strncpy( &Units.a[i*len], &cunit.a[0], l );
         Units.a[i*len+l] = '\0';
         i++;
      }
   }
   return(1);
}


static fint getsubsetaxnames( fchar Setin, fint subdim,
                              char *axnames, fchar Axisnames )
/*---------------------------------------------------------------*/
/* Get name of all subset axes and put names in string 'axnames' */
/* If successful, return 1, else return 0.                       */
/*---------------------------------------------------------------*/
{
   fchar    ctype;
   fchar    cunit;
   char     message[FITSLEN];
   fint     setdim;
   fint     zero = 0;
   fint     r1;
   fint     colev;
   int      slen;
   int      i, j;   


   fmake( ctype, FITSLEN );
   fmake( cunit, FITSLEN );

   axnames[0] = '\0';
   setdim = gdsc_ndims_c( Setin, &zero );
   if (setdim == subdim) 
      return(0);                                            /* No subset axes */

   for (i = subdim, j = 0; i < setdim; i++) 
   {
      /* Get name (CTYPE or DTYPE) of current non-subset axis */
      r1 = axcoord_c( Setin, 
                      &axnum[i], 
                      ctype, 
                      cunit, 
                      &colev );      
      if (r1 != 0)                                  /* Nothing could be found */
         strcpy( ctype.a, "?" );

      slen = nelc_c(ctype);      
      if (slen == 0)               /* Something was found but string is empty */
      {
         strcpy( ctype.a, "PHYSCO" );
         slen = nelc_c(ctype);
         if (colev == 2)
         {
            anyoutf( 1, " " );
            anyoutf( 1, "Warning -- DTYPE is missing. Column with physical coordinates" );
            anyoutf( 1, "        -- is now called 'PHYSCO'." );
            anyoutf( 1, " " );            
         }
      }
      if (slen > MAXCOLNAM) 
         slen = MAXCOLNAM;
         
      strncpy( &Axisnames.a[j*FITSLEN], &ctype.a[0], slen );
      Axisnames.a[j*FITSLEN+slen] = '\0';
      j++;
      /* Copy string until a space or hyphen is encountered */
      sprintf( message, "%-s", strtok(ctype.a, " -") );
      if (i == subdim) 
         strcpy( axnames, message );
      else 
         sprintf( axnames, "%.*s, %s", strlen(axnames), axnames, message );
   }
   return(1);
}


void getbox( fchar Setin, fint subdim, fint box, char *boxstr,
             int maxstrlen, bool polygon )
/*------------------------------------------------------------*/
/* Create string with current box                             */
/*------------------------------------------------------------*/
{  int    i;
   char   appendstr[20];
   int    len1, len2;

   if (polygon) {
      sprintf( boxstr, "SET: %.*s  BOX ENCLOSING VERTICES %d: [", nelc_c(Setin), Setin.a, box+1 );
   } else {
      sprintf( boxstr, "SET: %.*s  BOX %d: [", nelc_c(Setin), Setin.a, box+1 );
   }
   for (i = 0; i < subdim; i++) {
      sprintf( appendstr, "%d", bloa[box][i] );
      len1 = strlen(boxstr);
      len2 = strlen(appendstr);
      len2 = MYMIN( len2, maxstrlen-1-len1);
      sprintf( boxstr, "%.*s %.*s", len1, boxstr, len2, appendstr );
   }
   for (i = 0; i < subdim; i++) {
      sprintf( appendstr, "%d", bhia[box][i] );
      len1 = strlen(boxstr);
      len2 = strlen(appendstr);
      len2 = MYMIN( len2, maxstrlen-1-len1);
      sprintf( boxstr, "%.*s %.*s", len1, boxstr, len2, appendstr );
   }
   sprintf( boxstr, "%.*s]", strlen(boxstr), boxstr );
}


void getheaderunits( fchar Setin, char *sumunit )
/*---------------------------------------------------*/
/* Get units of data from header, if units cannot    */
/* be found, substitute '?'. Put units in uppercase. */
/*---------------------------------------------------*/
{
   fint     r1 = 0;
   fchar    dumtxt;
   fint     setlevel = 0;

   fmake( dumtxt, FITSLEN );
   sumunit[0] = '\0';
   gdsd_rchar_c( Setin, tofchar("BUNIT"), &setlevel, dumtxt, &r1 );
   if (r1 < 0) {
      strcpy( sumunit, "?" );
   } else {
      sprintf( sumunit, "%.*s", nelc_c(dumtxt), dumtxt.a );
   }
   for (r1 = 0; r1 < strlen(sumunit); r1++) {
      sumunit[r1] = toupper(sumunit[r1]);
   }
}


static int getpolygon( fint *blo, fint *bhi, fint *vertX, fint *vertY,
                       int boxcount, int *nvert )
/*-------------------------------------------------------------------------*/
/* Generate VERTICES keyword and message. Return number of vertices >= 3   */
/* and < MAXVERT in nvert. If return is presses, return 0 else return 1.    */
/* Return the vertices in vertX, vertY, and the enclosed box in blo, bhi.  */
/*-------------------------------------------------------------------------*/
{
   fint    dummyXY[MAXVERT*2];
   fint    nitems = MAXVERT*2;
   fint    dfault = REQUEST;
   int     i;

   sprintf( message, "VERTICES%d=", boxcount+1 );
   key = tofchar( message );
   mes = MES_VERTICES;
   do {
      do {
         r1 = userint_c( dummyXY, &nitems, &dfault, key, mes );
         if (r1 == 0) return(0);
         agreed = (r1 >= 6);
         if (!agreed) {
            reject_c( key, tofchar("At least 3 pairs needed!") );
         } else {
            agreed = ( (r1%2) == 0 );
            if (!agreed) reject_c( key, tofchar("Missing Y value!") );
         }
      } while (!agreed);
      /* Copy data in vertX,Y */
      blo[0] = bhi[0] = dummyXY[0];
      blo[1] = bhi[1] = dummyXY[1];
      *nvert = r1/2;
      for (i = 0; i < *nvert; i++) {
         int x, y;
         x = i * 2; y = x + 1;
         vertX[i] = dummyXY[x]; vertY[i] = dummyXY[y];
         if (vertX[i] < blo[0]) blo[0] = vertX[i];
         if (vertX[i] > bhi[0]) bhi[0] = vertX[i];
         if (vertY[i] < blo[1]) blo[1] = vertY[i];
         if (vertY[i] > bhi[1]) bhi[1] = vertY[i];
      }
      agreed = ( (blo[0] >= flo[0]) && (blo[1] >= flo[1]) &&
                 (bhi[0] <= fhi[0]) && (bhi[1] <= fhi[1]) );
      if (!agreed) {
         reject_c( key, tofchar("Box outside subset frame!") );
      } else {
         agreed = ( ((bhi[0]-blo[0]+1) * (bhi[1]-blo[1]+1)) <= MAXBUF);
         if (!agreed) reject_c( key, tofchar("Polygon too big!") );
      }
   } while (!agreed);
   return(1);
}


void setupmask( fint *blo, fint *bhi,
                fint *vertX, fint *vertY, int nvert,
                fint *Xcent, fint *Ycent )
/*-----------------------------------------------------------------------*/
/* blo, bhi are the corners of the box that encloses the polygon defined */
/* in vertX, vertY. Determine for each pixel in the box whether it is    */
/* inside or outside the polygon. Transform the one dimensional index to */
/* a two dimensional position, check and update the global array mask.   */
/* The polygon case is always two dimensional.                           */
/*-----------------------------------------------------------------------*/
{
   int     x, y;
   int     xlen;
   int     i, j;
   int     Xi[MAXVERT];
   double  Y1, Y2, X1, X2;
   int     msk;
   int     nsect;
   int     posX, posY;
   int     npos;


   /* Copy the first data pair into the last one vertex */
   vertX[nvert] = vertX[0];
   vertY[nvert] = vertY[0];
   xlen = bhi[0] - blo[0] + 1;            /* Calculate for this box the length of x-axis */
   for( y = blo[1]; y <= bhi[1]; y++) {
      /* For each line determine intersections */
      for (i = 1, nsect = 0; i <= nvert; i++) {
         X1 = vertX[i-1]; X2 = vertX[i];
         Y1 = vertY[i-1]; Y2 = vertY[i];
         if (  (y >= vertY[i-1] && y <= vertY[i]) ||
               (y <= vertY[i-1] && y >= vertY[i])   ) {
            /* Calculate the intersection */
            if (vertY[i] == vertY[i-1]) {
               /* Horizontal line, there are two intersections */
               Xi[nsect] = vertX[i];   nsect++;
               Xi[nsect] = vertX[i-1]; nsect++;
            } else {
               if (vertX[i] == vertX[i-1]) {
                  /* Vertical line, one intersection */
                  Xi[nsect] = vertX[i];   nsect++;
               } else {
                  Xi[nsect] = (int) ( X1 + (X2-X1) * ((double)y-Y1) / (Y2-Y1) );
                  nsect++;
               }
            }
         }
      }
      /* Sort the intersections into increasing x order */
      for (i = 1; i < nsect; i++) {
         for (j = 0; j <= i; j++) {
            float   temp;
            if (Xi[j] > Xi[i]) {
               temp  = Xi[j];
               Xi[j] = Xi[i];
               Xi[i] = temp;
            }
         }
      }
      /* Do not allow equal intersections */
      for (i = 0, j = 0; i < nsect; i++) {
         if (i == 0) {
            Xi[j] = Xi[i]; j++;
         } else {
            if (Xi[i] != Xi[i-1]) {
               Xi[j] = Xi[i]; j++;
            }
         }
      }
      nsect = j;


      /* Update mask */
      strcpy( message, "" );
      for (i = 0, msk = 0, x = blo[0]; x <= bhi[0]; x++) {
         int   index;
         index = (y-blo[1])*xlen + (x-blo[0]);
         if ( (x == Xi[i]) && (i < nsect) ) {
            if (nsect > 1) {
               /* Always include borders */
               if (msk == 0) {
                  msk = 1;
                  mask[index] = msk;
               } else {
                  mask[index] = msk;
                  msk = 0;
               }
               i++;
            }
         } else {
            mask[index] = msk;
         }
         /* print object pattern 0 is outside, 1 is inside */
         /* sprintf(message, "%.*s%1d", strlen(message), message, mask[index]);*/
      }
   }
   posX = posY = 0;
   npos = 0;
   /* Determine (weighted) center of object */
   for( y = blo[1]; y <= bhi[1]; y++) {
       for (x = blo[0]; x <= bhi[0]; x++) {
         int    index;
         index = (y-blo[1])*xlen + (x-blo[0]);
         if (mask[index]) {
            posX += x; posY += y;
            npos++;
         }
      }
   }
   if (npos == 0) {
      /* No points inside */
      *Xcent = blo[0];       *Ycent = blo[1];
   } else {
      *Xcent = posX / npos;  *Ycent = posY / npos;
   }
}


static void checkcol( fchar   Tname, 
                      fint    subin, 
                      char   *colname,
                      char   *coltype, 
                      char   *colunit, 
                      char   *comments,
                      bool    append, 
                      fint   *collen )
/*------------------------------------------------------------*/
/* PURPOSE: Check whether a column already exists. If so, and */
/* the user does not want to append, delete the column.       */
/*------------------------------------------------------------*/
{
   fchar   Cname;
   fchar   Units;
   fchar   Comment;
   fchar   Type;
   fint    r1, r2;
   fint    nrows;
   bool    exist;


   fmake( Cname,   ITEMLEN );
   fmake( Units,   VARLEN );
   fmake( Comment, VARLEN );
   fmake( Type,    VARLEN );
   Cname.l   = str2char( colname, Cname );
   r1    = 0;
   nrows = 0;
   gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
   exist = (r1 >= 0);
   if (exist && !append) {
      r2 = 0;
      gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      nrows = 0;
      exist = NO;
   }
   if (!exist) {
      Type.l    = str2char( coltype,  Type );
      Comment.l = str2char( comments, Comment);
      Units.l   = str2char( colunit,  Units );
      r1 = 0;
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   *collen = nrows;
}


static int inittable( fchar Setin, 
                      fint  subin, 
                      fchar Tname, 
                      char *sumunits,
                      char *fluxunits, 
                      char *sumAPunits, 
                      fchar Physunits,
                      fchar Axisnames, 
                      fint  naxis, 
                      bool  append, 
                      fint *collen )
/*------------------------------------------------------------*/
/* PURPOSE: Create columns.                                   */
/* If column already exist, delete before (re)creation        */
/*------------------------------------------------------------*/
{
   int    i;
   int    success = YES;
   fint   nrows;
   

   if (strlen(sumunits) == 0)   strcpy( sumunits,   "?" );
   if (strlen(fluxunits) == 0)  strcpy( fluxunits,  "?" );
   if (strlen(sumAPunits) == 0) strcpy( sumAPunits, "?" );

   checkcol( Tname, subin, "SUM",     "REAL", sumunits,   "Sum", append, &nrows );
   checkcol( Tname, subin, "SUMPBC",  "REAL", sumunits,   "Primary beam corrected sum", append, &nrows );
   checkcol( Tname, subin, "FLUX",    "REAL", fluxunits,  "Flux", append, &nrows );
   checkcol( Tname, subin, "FLUXPBC", "REAL", fluxunits,  "Primary beam corrected flux", append, &nrows );
   checkcol( Tname, subin, "SUMAP",   "REAL", sumAPunits, "Sum in Antenna pattern", append, &nrows );
   checkcol( Tname, subin, "NDATA",   "INT", "#",         "Valid points in statistics", append, &nrows );
   checkcol( Tname, subin, "NBLANKS", "INT", "#",         "Number of blanks in statistics", append, &nrows );
   checkcol( Tname, subin, "XLO",     "INT", "GRIDS",     "Box: X low", append, &nrows );
   checkcol( Tname, subin, "YLO",     "INT", "GRIDS",     "Box: Y low", append, &nrows );
   checkcol( Tname, subin, "XHI",     "INT", "GRIDS",     "Box: X high",append, &nrows );
   checkcol( Tname, subin, "YHI",     "INT", "GRIDS",     "Box: Y high",append, &nrows );

   *collen = nrows;
   for (i = 0; i < naxis; i++) 
   {
      char   unitstr[VARLEN];
      char   axisstr[VARLEN];
      char   cnamebuf[ITEMLEN];


      sprintf( cnamebuf, "SUBGRID%d", i+1);
      checkcol( Tname, subin, cnamebuf, "INT", "GRIDS",
               "Subset in grid coordinates", append, &nrows );
 
      if (nrows != *collen)
      {
         anyoutC( 1, "WARNING -- Not all rows have equal length. Please enter another table name!" );
         success = NO;
      }

      len = Axisnames.l;
      strncpy( axisstr, &Axisnames.a[i*len], len );
      axisstr[len] = '\0';
      len = Physunits.l;
      strncpy( unitstr, &Physunits.a[i*len], len );
      unitstr[len] = '\0';
      checkcol( Tname, subin, axisstr, "DBLE", unitstr,
               "Subset in physical coordinates", append, &nrows );
      if (nrows != *collen)
      {
         anyoutC( 1, "WARNING -- Not all rows have equal length. Please enter another table name!" );
         success = NO;
      }               
   }
   return( success );
}



static int getsubsetgrid( fchar Setin, fint subin, fint *axnum,
                          fint *grids, double *coords )
{
   int    i, n;
   fint   err;
   fint   res;
   fint   grid;
   fint   setdim, subdim;
   fint   zero = 0;

   /* Coordinate transformation */

   fint     direction;           /* grid coord. -> physical coord. */
   double   coordin[MAXAXES];    /* Grids before transformation */
   double   coordout[MAXAXES];   /* Physical coordinates after transformation */


   setdim = gdsc_ndims_c( Setin, &zero );                   /* dimension of set */
   subdim = gdsc_ndims_c( Setin, &subin );                  /* dimension of subset */
   if (setdim == subdim) return(-1);                         /* No subset axes */
   for (n = 0, i = 0; n < setdim; n++ ) {
      if (n >= subdim) {
         err = 0;
         grid = gdsc_grid_c( Setin, &axnum[n], &subin, &err );
         coordin[ (int) axnum[n]-1 ] = (double) grid;
         grids[i++] = grid;
      }
      else {
         coordin[ (int) axnum[n]-1 ] = 0.0;
      }
   }
   direction = 1;                           /* grid coord. -> physical coord. */
   res = cotrans_c( Setin, &subin, coordin, coordout, &direction );
   if (res < 0) return(-2);
   for (n = subdim, i = 0; n < setdim; n++ ) {
      coords[i++] = coordout[ (int) axnum[n]-1 ];
   }
   return(1);
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fchar      Tname;
   fint       flen;
   fchar      Axisnames;
   fchar      Physunits;
   fint       naxis;
   fchar      Key, Mes;
   fint       count;
   int        clen;
   bool       append;
   fint       nrows;


   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      fchar    Task;                       /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   /* Some advertisements */
   anyoutC( 1, "========================= NEWS (07-10-'92) ============================" );
   anyoutC( 1, " ");
   anyoutC( 1, "FLUX -calculates flux" );
   anyoutC( 1, "     -plots the results");
   anyoutC( 1, "     -writes data to COLUMNS in a TABLE (TABNAME=)");
   anyoutC( 1, "     -can use polygons instead of boxes (hidden keyword POLYGON=)" );
   anyoutC( 1, " ");
   anyoutC( 1, "=======================================================================" );
   setfblank_c( &blank );
   fmake( Key, 20 );
   fmake( Mes, 80 );
   fmake( Setin, STRLEN );
   dfault  = NONE;
   subdim  = 0;
   showdev = 3;
   key     = KEY_INSET;
   mes     = MES_INSET;
   nsubs   = gdsinp_c( Setin,      /* Name of input set. */
                       subin,      /* Array containing subsets coordinate words. */
                       &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                       &dfault,    /* Default code as is USERxxx. */
                       key,        /* Keyword prompt. */
                       mes,        /* Keyword message for the user. */
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
      for (m = 0; m < (int) setdim; m++) 
      {
         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
   }

   /*-----------------------------------------------------------------*/
   /* If user wants to use polygons instead of boxes, he must select  */
   /* POLYGON=Y  This affects the input of edges in several ways.     */
   /* Give warning if subdim != 2                                     */
   /*-----------------------------------------------------------------*/

   polygon = toflog(NO);
   nitems  = 1;
   dfault  = HIDDEN;
   key     = KEY_POLYGON;
   mes     = MES_POLYGON;
   r1      = userlog_c( &polygon, &nitems, &dfault, key, mes );
   polygon = tobool( polygon );
   if ( (polygon) && (subdim != 2) ) 
   {
      anyoutC( 1, "Only polygons for 2 dim. subsets, continue with boxes." );
      polygon = NO;
   }

   /*------------------------------------------------------------------------*/
   /* Initialize for primary beam correction. This is achieved by calling    */
   /* the function PRIBEAM with option PBCopt = 0. If primary beam           */
   /* correction is possible, this function returns a value >= 0             */
   /*------------------------------------------------------------------------*/

   PBCopt = 0;
   fmake(errtxt, 120);
   (void) pribeam_c( Setin, 
                     &subin[0], 
                     axnum, 
                     &dumx, 
                     &dumy,
                     &PBCopt, 
                     errtxt );

   correc = (PBCopt >= 0);
   /* Is it radio data? <== WSRT, VLA, VLAP, FST, or DRAO in header */
   radio = ((PBCopt < -2) || correc);
   if (!correc) 
   {
     anyoutC( 3, "FLUX: No primary beam correction." );
     anyoutC( 3, errtxt.a );
   }
   if (radio) 
   {
      anyoutC( 3, "FLUX: Data is identified as radio data" );
   }

   /*--------------------------------------------------*/
   /* Set 'outside validity region' option for PBC to  */
   /* blank for this program:                          */
   /*--------------------------------------------------*/

   PBCopt = 4;

   /*-------------------------------*/
   /* Prepare a box for INSET       */
   /*-------------------------------*/

   mes      = tofchar( " " );
   key      = KEY_BOX;
   boxopt   = 0;
   showdev  = 16;
   dfault   = REQUEST;
   boxcount = 0;

   /*-----------------------------------------------------------*/
   /* If the subset dimension is 2, it must also be possible to */
   /* give a polygon instead of a box. If two pairs are given   */
   /* the polygon must be a box and the given values are copied */
   /* in blo and bhi. For three or more pairs, the enclosed box */
   /* is calculated and a mask is determined.                   */
   /*-----------------------------------------------------------*/
   if (polygon) 
   {
      if (getpolygon( blo, bhi, vertX, vertY, boxcount, &nvert[boxcount] )) 
      {
         /* Copy the box values in box array */
         for (m = 0; m < subdim; m++) 
         {
            bloa[boxcount][m] = blo[m];
            bhia[boxcount][m] = bhi[m];
         }
         for (m = 0; m < nvert[boxcount]; m++) 
         {
            vertXa[boxcount][m] = vertX[m];
            vertYa[boxcount][m] = vertY[m];
         }
      }
   } else 
   {
      gdsbox_c( blo, bhi, Setin, subin, &dfault,
                key, mes, &showdev, &boxopt );
      /* Copy the box values in box array */
      for (m = 0; m < subdim; m++) 
      {
         bloa[boxcount][m] = blo[m];
         bhia[boxcount][m] = bhi[m];
      }
   }


   /* If the first box is smaller than the entire frame, ask */
   /* the user to give more boxes */


   boxcount++;
   equaledges = NO;

   if (!polygon) 
   {
      /* ask BOXES */
      while ( (!equaledges) && (boxcount < MAXBOXES) ) 
      {
         char mess1[STRLEN], mess2[STRLEN];
         /* Ask for more boxes */
         cancel_c( key );
         sprintf( mess1, "BOX%d=", boxcount+1 );
         key = tofchar( mess1 );
         sprintf( mess2, "Coordinates box %d:      [No more boxes]", boxcount+1);
         mes = tofchar( mess2 );
         for (m = 0; m < subdim; m++) 
         {
            blo[m] = bloa[boxcount-1][m];
            bhi[m] = bhia[boxcount-1][m];
         }
         /* Defaults in blo, bhi */
         boxopt = 6;
         gdsbox_c( blo, bhi, Setin, subin, &dfault,
                   key, mes, &showdev, &boxopt );
         /* Is new box same as previous? */
         equaledges = YES;
         for (m = 0; m < subdim; m++) 
         {
            equaledges = (equaledges &&
                         (blo[m] == bloa[boxcount-1][m]) &&
                         (bhi[m] == bhia[boxcount-1][m]) );
         }
         if (!equaledges) 
         {
            for (m = 0; m < subdim; m++) 
            {
               bloa[boxcount][m] = blo[m];
               bhia[boxcount][m] = bhi[m];
            }
            boxcount++;
         }
      }
   } 
   else 
   {
      /* Ask POLYGONS */
      while (getpolygon( blo, bhi, vertX, vertY, boxcount, &nvert[boxcount] )) 
      {
         for (m = 0; m < subdim; m++) 
         {
            bloa[boxcount][m] = blo[m];
            bhia[boxcount][m] = bhi[m];
         }
         for (m = 0; m < nvert[boxcount]; m++) 
         {
            vertXa[boxcount][m] = vertX[m];
            vertYa[boxcount][m] = vertY[m];
         }
         boxcount++;
      }
   }


   /*----------------------------------------------------------------*/
   /* If the data is radio data, try to find the sum of the values   */
   /* in the corresponding box in the corresponding Antenna pattern  */
   /* If a clean beam is wanted, the values of that beam is used for */
   /* the sumAP, else the set with the Antenna Pattern is asked.     */
   /* The default is the set found in the header in 'APSET' else     */
   /* the default is to quit the AP specification. If there is a     */
   /* default AP set in the header, but you do not want to use it,   */
   /* specify HEADER=N                                               */
   /*----------------------------------------------------------------*/
   if (radio) 
   {
      fchar  APstring;
      fint   len = STRLEN;
      fint   one = 1;
      fint   bytesdone;
      bool   APisitem;

      fmake( APstring, STRLEN );
      r1 = 0;
      gdsd_readc_c( Setin,
                    tofchar("APSET"),
                    &subin[0],
                    APstring,
                    &len,
                    &one,
                    &bytesdone,
                    &r1 );
      APisitem = ( (r1 >= 0) && (nelc_c(APstring) > 0) );
      if (APisitem) 
      {
         sprintf( message, "FLUX: AP found in header is: %.*s",
                  nelc_c( APstring ), APstring.a );
         anyoutC( 3, message );
      } 
      else 
      {
         APstring = tofchar( " " );
      }


      APsubdim = subdim;    /* Dimension of AP always 2 */
      if (APsubdim <= 2) 
      {
         AP = YES;
      } 
      else 
      {
         AP = NO;
         sprintf( message, "FLUX: Cannot use %d dimensional AP's", APsubdim );
         anyoutC( 3, message );
      }
      if (AP) 
      {
         class   = 1;
         showdev = 16;
         do 
         {
            clean  = toflog( NO );
            dfault = REQUEST;
            nitems = 1;
            key    = tofchar("CLEANBEAM=");
            mes    = tofchar("Do you want to give values of a clean beam?  Y/[N]");
            r1     = userlog_c( &clean, &nitems, &dfault, key, mes );
            clean  = tobool( clean );
            if (clean) 
            {
               agreed = YES;
               AP = NO;
            }
            if (!clean) 
            {
               fmake(APSetin, STRLEN);
               if (APisitem) 
               {
                 dfault   = REQUEST;
                 nitems   = 1;
                 APisitem = toflog(APisitem);
                 key      = tofchar("HEADER=");
                 mes      = tofchar("Antenna Pattern from header?        [Y]/N" );
                 r1       = userlog_c( &APisitem, &nitems, &dfault, key, mes );
                 APisitem = tobool(APisitem);
               }
               key    = tofchar("APSET=");
               dfault = REQUEST;
               if (APisitem) 
               {
                  mes = tofchar("Give set, subs. of ant.pat.: [AP from header]");
                  /* Default is the AP from the descriptor now */
                  sprintf( APSetin.a, "%.*s", nelc_c(APstring), APstring.a );
                  dfault = HIDDEN;
               } 
               else 
               {
                  mes = tofchar("Give set, subs. of ant.pat.: [No AP]" );
                  r1  = usertext_c( APSetin, &dfault, key, mes );
                  dfault = REQUEST;
                  if (r1 == 0) 
                  {
                     AP = NO;
                     agreed = YES;
                  }
               }
               if (AP) 
               {
                  APnsubs = gdsinp_c( APSetin, APsubin,
                                      &nsubs,
                                      &dfault,
                                      key, mes,
                                      &showdev,
                                      APaxnum, APaxcount,
                                      &maxaxes,
                                      &class,
                                      &APsubdim );

                  agreed = ( (APnsubs == 1) || (APnsubs == nsubs) || (APnsubs == 0) );
                  if (!agreed) 
                  {
                     APisitem = NO;
                     cancel_c( tofchar("APSET=") );
                     cancel_c( tofchar("CLEANBEAM=") );
                     anyoutC( 3, "FLUX: Number of AP subsets has to be equal to 0 or 1, or" );
                     anyoutC( 3, "      the number of input subsets!" );
                  }
               }
            }
         } 
         while (!agreed);
      }

      if (AP) 
      {
         /*-------------------------------*/
         /* Determine edges of this frame */
         /*-------------------------------*/
         fint cwlo, cwhi;                           /* Local coordinate words */
         int  m;

         r1 = 0;
         gdsc_range_c( APSetin, &setlevel, &cwlo, &cwhi, &r1 );
         r1 = r2 = 0;
         for (m = 0; m < (int) subdim; m++) 
         {
            apflo[m] = gdsc_grid_c( APSetin, &APaxnum[m], &cwlo, &r1 );
            apfhi[m] = gdsc_grid_c( APSetin, &APaxnum[m], &cwhi, &r2 );
         }
      }
   } /* End of radio data */


   if (radio) 
   {
      float    cleanval = 0.0;
      /*---------------------------------------------------------*/
      /* There are for each given box, nsubs fluxes to calculate */
      /* The sumAP's are stored in an array sumAP[box][subnr]    */
      /*---------------------------------------------------------*/
      if (clean) 
      {
         getspacing( &cdeltx, &cdelty, Setin, axnum );
         cleanval = getcleanval( cdeltx, cdelty );
      }

      if (AP) 
      {
         if (APnsubs == 1)                 /* Make 'nsubs' same ant. patterns */
         {
           for(i = 1; i < nsubs; i++) 
           {
             APsubin[i] = APsubin[i-1];
           }
         }
      }

      for (box = 0; box < boxcount; box++) 
      {
         for(subnr = 0; subnr < nsubs; subnr++) 
         {
            bool   first;
            bool   copy;
            sumAP[box][subnr] = blank;                /* Initialize sumAP */
            if (clean) 
            {
               sumAP[box][subnr] = cleanval;
            }
            if (AP) 
            {
               first = (subnr == 0);
               if (!first) 
               {
                  /* Check whether a AP is same as previous one. */
                  copy = (APsubin[subnr] == APsubin[subnr-1]);
               } 
               else 
               {
                  copy = NO;
               }
               if (copy) 
               {
                  sumAP[box][subnr] = sumAP[box][subnr-1];
               } 
               else 
               {
                  Xcent= Ycent = 0;
                  if (polygon) 
                  {
                     /* Prepare a mask for the AP */
                     for (j = 0; j < nvert[box]; j++) 
                     {
                        vertX[j] = vertXa[box][j];
                        vertY[j] = vertYa[box][j];
                     }
                     for (j = 0; j < subdim; j++) /* Grids of this box in this subset */
                     {
                        blo[j] = bloa[box][j];
                        bhi[j] = bhia[box][j];
                     }
                     setupmask( blo, bhi, vertX, vertY, nvert[box], &Xcent, &Ycent );
                  }
                  sumAP[box][subnr] = getsumAP( APSetin, apflo, apfhi, box,
                                                APsubin[subnr], APsubdim, polygon, Xcent, Ycent );
               }
            }   /* End if AP */
         }      /* End all subsets */
      }         /* End all boxes */
   }            /* End radio data */


   /*-------------------------*/
   /* All other kind of data: */
   /*-------------------------*/

   if (!radio) 
   {
      if (subdim != 2) 
      {
         sterads = NO;
         anyoutC( 3, "FLUX: No conversion to steradians because subset dimension != 2" );
      } 
      else 
      {
         fchar  cunit;
         fmake( cunit, FITSLEN );
         sterads = YES;
         getspacing( &cdeltx, &cdelty, Setin, axnum );
         for (i = 0; i < 2; i++) 
         {
            r1 = 0;
            sprintf( message, "CUNIT%d", axnum[i] );
            gdsd_rchar_c( Setin, tofchar(message), &setlevel, cunit, &r1 );
            /* Steradians only possible if units in deg. */
            if (( r1 < 0) || (strstr( cunit.a, "DEGREE" ) == 0) ) 
            {
               sterads = NO;
               anyoutC( 3, "FLUX: No conversion to steradians because axis units not in degrees!");
            }
         }
      }
      if (sterads)                /* Calculate number of steradians per pixel */
      {
         srpix = fabs( cdeltx * PI/180.0 * cdelty * PI/180.0 );
      } 
      else 
      {
         setdblank_c( &srpix );
      }
   }

   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/

   (void) initplot( boxcount );

   for (box = 0; box < boxcount; box++) 
   {
      fint   linelen;
      fint   numpixels, totpixels;
      fint   stilltoread;
      fint   numinreadbuf;
      fint   nstat;
      fint   start;
      fint   ax;
      float  val;
      fint   X, Y;
      float  RX, RY;
      float  pbcfac;


      for (j = 0; j < subdim; j++)        /* Grids of this box in this subset */
      {
         blo[j] = bloa[box][j];
         bhi[j] = bhia[box][j];
      }
      if (polygon) 
      {
         for (j = 0; j < nvert[box]; j++) 
         {
             vertX[j] = vertXa[box][j];
             vertY[j] = vertYa[box][j];
         }
         setupmask( blo, bhi, vertX, vertY, nvert[box], &Xcent, &Ycent );
      }
      linelen = bhi[0] - blo[0] + 1;           /* Calculate for this box the length of x-axis */
      for (subnr = 0; subnr < nsubs; subnr++)  /* Calculate flux per subset for current box */
      {
         showsub1_c( Setin, &subin[subnr], axnum );        /* Shows current set etc. */
         cwlo = gdsc_fill_c( Setin, &subin[subnr], blo );  /* Coordinate words */
         cwhi = gdsc_fill_c( Setin, &subin[subnr], bhi );

         /* Reset variables for subsets */

         if (correc) 
         {
            sumPBC[box][subnr] = 0.0;
         } 
         else 
         {
            sumPBC[box][subnr] = blank;
         }
         npoints[box][subnr] = 0;
         nblanks[box][subnr] = 0;
         sum[box][subnr]     = 0.0;
         numpixels           = 0;
         nstat               = 0;
         start               = 0;
         tid                 = 0;
         totpixels           = 1;
         for (ax = 0; ax < subdim; ax++) 
         {
            /* Calculate number of pixels in this sub frame */
            totpixels *=  (bhi[ax] - blo[ax] + 1);
         }
         stilltoread = totpixels;
         do 
         {
            int    k;
            /* # per iteration cannot exceed MAXBUF */
            numinreadbuf = MYMIN( MAXBUF, stilltoread );
            numpixels    = 0;

            gdsi_read_c( Setin, &cwlo, &cwhi, image,
                         &numinreadbuf, &numpixels, &tid );

            for (k = 0; k < numpixels; k++ ) 
            {
               if ( (!polygon) || (polygon && mask[start]) ) 
               {
                  nstat++;                        /* Number of pixels in box or polygon */
                  val = image[k];
                  if (val == blank) 
                  {
                     nblanks[box][subnr]++;
                  } 
                  else 
                  {
                     sum[box][subnr] += val;
                     if (correc) 
                     {
                        /* Calculate a one or two dim. position */
                        if (subdim == 1) 
                        {
                           RX = (float) start;
                           RY = 0.0;
                        }
                        if (subdim == 2) 
                        {
                           Y =  start / linelen;
                           X = ( start - Y * linelen );
                           X += blo[0];    Y += blo[1];
                           RX = (float) X; RY = (float) Y;
                        }
                        /* Primary beam correction on this position */
                        pbcfac = pribeam_c( Setin, 
                                            &subin[subnr], 
                                            axnum,
                                            &RX, &RY, 
                                            &PBCopt, 
                                            errtxt );
                        if ( (pbcfac != blank) && (pbcfac != 0.0) ) 
                        {
                           /* Add to flux if pixel within validity region */
                           sumPBC[box][subnr] += (val / pbcfac);
                        }
                     } /* End if correc */
                  }    /* End if not blank */
               }
               start++;
            } /* End for all pixels in buffer */
            stilltoread -= numinreadbuf;
         } while (stilltoread != 0);
         /* Correct number of points for blanks */
         npoints[box][subnr] = nstat - nblanks[box][subnr];
      } /* End for all subsets */
   }    /* End for all boxes */

   /*----------------------------------------------------------------------*/
   /* Now all subsets and boxes are examined and we can display the values */
   /* Distinguish radio and non radio data. First prepare a header for the */
   /* table.                                                               */
   /*----------------------------------------------------------------------*/

   /* Table data to disk also? */
   fp = fopenC( filename );


/* Layout of log file table header:
subset       sum    sumPBC      flux   fluxPBC     sumAP  points  blanks  level
1234###123456789#123456789#123456789#123456789#123456789#1234567#1234567##12345.....
=====================================================================================
*/
   naxis = setdim - subdim;
   flen = (naxis + 1) * FITSLEN;
   finit( Axisnames, flen );
   Axisnames.l = flen;
   r1 = getsubsetaxnames( Setin, subdim, axnames, Axisnames );
   Axisnames.l = FITSLEN;
   if (r1 == 0) strcpy( axnames, "Top level" );


   sprintf( tabheader, "subset       sum    sumPBC      flux   fluxPBC     sumAP  points  blanks");
   sprintf( tabheader, "%.*s  %-.*s",
            strlen(tabheader), tabheader, strlen(axnames), axnames );

   /* Get the image units */

   getheaderunits( Setin, sumunit );
   sterads = (sterads && strstr(sumunit, "/SR") );  /* Units in uppercase */
   if ( (radio) && (strstr( sumunit, "W.U" ) || strstr( sumunit, "WU") ) ) 
   {
      westerbork = YES;
   } 
   else 
   {
      westerbork = NO;
   }
   strcpy( fluxunit, " " );
   if (sterads) 
   {
      char copyunit[FITSLEN];    /* strtok slices the string, so copy first */
      strcpy( copyunit, sumunit );
      sprintf( fluxunit, "%-s", strtok(copyunit, " /") );
   }
   if (radio) 
   {
      if (westerbork) 
      {
         strcpy( fluxunit, "mJy" );
      } 
      else 
      {
         char copyunit[FITSLEN];    /* strtok slices the string, so copy first */
         if ( strstr(sumunit, "/BEAM") ) 
         {
            strcpy( copyunit, sumunit );
            sprintf( fluxunit, "%-s", strtok(copyunit, " /") );
         } 
         else 
         {            
            anyoutC( 3, "FLUX: Flux units not W.U. or /BEAM" );
         }
      }
   }

   strcpy( sumPBCunit, sumunit );
   strcpy( fluxPBCunit, fluxunit );
   strcpy( sumAPunit, "*" );
   finit( Physunits, flen );
   Physunits.l = flen;
   r1 = getsubsetunits( Setin, &subin[0], axnum, levstr, Physunits );
   Physunits.l = FITSLEN;
   if (r1 == -1) 
      strcpy( levstr, "Cannot find units" );
   sprintf( unitheader, "(  # ) %9s %9s %9s %9s %9s     (#)     (#)  (%-s)",
            sumunit,
            sumPBCunit,
            fluxunit,
            fluxPBCunit,
            sumAPunit,
            levstr );
   len = strlen( unitheader );
   for (i = 0; i < MYMIN( len, LONGSTR ); i++) 
      borderheader[i] = '=';
   borderheader[i] = '\0';


   if(fp) {
      fprintf( fp, "Units sum:  %s\n", sumunit );
      fprintf( fp, "Units flux: %s\n", fluxunit );
      fprintf( fp, "Units subset axes %s: %s\n", axnames, levstr );
      fprintf( fp, "subs        sum     sumPBC       flux    fluxPBC     level\n");
      fprintf( fp, "==========================================================\n");
   }

   /* Create table */

   fmake( Tname, ITEMLEN );
   fmake( subsetstr, STRLEN );

   /* Ask name of table: */

   str2char("FLUX", Tname);
   dfault  = HIDDEN;
   nitems  = 1;
   Key     = tofchar("TABNAME=");
   Mes     = tofchar("Give generic name of table to store results: [FLUX]");
   r1      = userchar_c( Tname, &nitems, &dfault, Key, Mes );
   clen    = sprintf( message, "%d", boxcount );
   clen    = MYMIN( nelc_c(Tname), MAXCOLNAM-clen );

   /* Append to existing columns? */
   dfault  = HIDDEN;
   nitems  = 1;
   Key     = tofchar("TABAPPEND=");
   Mes     = tofchar("Append to existing columns?    Y/[N]");
   append  = toflog( NO );
   r1      = userlog_c( &append, &nitems, &dfault, Key, Mes );
   append  = tobool( append );

   /* Start loop over boxes */

   for (box = 0; box < boxcount; box++) 
   {
      float   val;
      int     ok;

      do
      {
         sprintf( message, "%.*s%d", clen, Tname.a, box+1 );
         str2char(message, Tname);         
         ok = inittable( Setin, 
                         setlevel, 
                         Tname,
                         sumunit,
                         fluxunit,
                         sumAPunit,
                         Physunits,
                         Axisnames,
                         naxis,
                         append,
                         &nrows );
                         
         /*--------------------------------------------------*/
         /* Next check whether all columns involved in       */
         /* appending have the same length. If not then ask  */
         /* another table name.                              */ 
         /*--------------------------------------------------*/
         if (!ok)   
         {
            cancel_c(tofchar("TABNAME="));
            str2char("FLUX", Tname);
            dfault  = NONE;
            nitems  = 1;
            Key     = tofchar("TABNAME=");
            Mes     = tofchar("Give another table name: ");
            r1      = userchar_c( Tname, &nitems, &dfault, Key, Mes );
            clen    = sprintf( message, "%d", boxcount );
            clen    = MYMIN( nelc_c(Tname), MAXCOLNAM-clen );
         }
      }
      while (!ok);

      anyoutC( 3, " " );
      (void) getbox( Setin, subdim, box, boxstr, STRLEN, polygon );
      anyoutC( 3, boxstr );
      if (fp) 
      {
         fprintf( fp, "%s\n", boxstr );
      }
      len = strlen( boxstr );
      for (i = 0; i < MYMIN(len, LONGSTR); i++) 
         message[i] = '=';
      message[i] = '\0';
      anyoutC( 3, message );
      anyoutC( 3, tabheader  );
      anyoutC( 3, unitheader );
      anyoutC( 3, borderheader );
      message[0] = '\0';
      if (append) 
      {
         count = nrows;
      } 
      else 
      {
         count = 0;
      }

      for (subnr = 0; subnr < nsubs; subnr++) 
      {
         fint    one = 1;
         count++;
         sprintf( message, "(%4d)", subnr );
         if (fp) 
            sprintf( diskmess, "%4d", subnr );
         val = sum[box][subnr];
         if (val != blank) 
         {
            sprintf( message, "%.*s %#9.4g", strlen(message), message, val );
            if (fp) 
               sprintf( diskmess, "%.*s %10f", strlen(diskmess), diskmess, val );
         } 
         else 
         {
            sprintf( message, "%.*s %9s", strlen(message), message,"*" );
            if (fp) 
               sprintf( diskmess, "%.*s %10s", strlen(diskmess), diskmess, "*" );
         }
         r1 = 0; 
         gdsa_wcreal_c( Setin, &setlevel, Tname,
                        tofchar("SUM"), &val, &count, &one, &r1 );

         val = sumPBC[box][subnr];
         if (val != blank) 
         {
            sprintf( message, "%.*s %#9.4g", strlen(message), message, val );
            if (fp) 
               sprintf( diskmess, "%.*s %10f", strlen(diskmess), diskmess, val );
         } 
         else 
         {
            sprintf( message, "%.*s %9s", strlen(message), message,"*" );
            if (fp) 
               sprintf( diskmess, "%.*s %10s", strlen(diskmess), diskmess, "*" );
         }
         r1 = 0; 
         gdsa_wcreal_c( Setin, &setlevel, Tname,
                        tofchar("SUMPBC"), &val, &count, &one, &r1 );

         if (radio) 
         {
            if ( (sumAP[box][subnr] == blank) || (sumAP[box][subnr] == 0.0) ||
                 (sum[box][subnr] == blank) ) 
            {
               flux[box][subnr] = blank;
            } 
            else 
            {
               flux[box][subnr] = sum[box][subnr] / sumAP[box][subnr];
               if (westerbork) 
               {
                  /* !!!!!!!! Convert WU to mJy/Beam !!!!!!!!*/
                  flux[box][subnr] *= 5.0;
               }
            }
         } 
         else 
         {
            /* Must be optical */
            if (sterads) 
            {
               flux[box][subnr] = (float) ((double) sum[box][subnr] *  srpix);
            } 
            else 
            {
               flux[box][subnr] = blank;
            }
         }
         val = flux[box][subnr];
         if (val != blank) 
         {
            sprintf( message, "%.*s %#9.4g", strlen(message), message, val );
            if (fp) 
               sprintf( diskmess, "%.*s %10f", strlen(diskmess), diskmess, val );
         } 
         else 
         {
            sprintf( message, "%.*s %9s", strlen(message), message,"*" );
            if (fp) 
               sprintf( diskmess, "%.*s %10s", strlen(diskmess), diskmess, "*" );
         }
         r1 = 0; 
         gdsa_wcreal_c( Setin, &setlevel, Tname,
                        tofchar("FLUX"), &val, &count, &one, &r1 );

         if ( (sumAP[box][subnr] == blank) || (sumAP[box][subnr] == 0.0) ||
              (sumPBC[box][subnr] == blank) ) 
         {
            fluxPBC[box][subnr] = blank;
         } 
         else 
         {
            fluxPBC[box][subnr] = sumPBC[box][subnr] / sumAP[box][subnr];
            if (westerbork) 
            {
               /* !!!!!!!! Convert WU to mJy/Beam !!!!!!!! */
               fluxPBC[box][subnr] *= 5.0;
            }
         }
         val = fluxPBC[box][subnr];
         if (val != blank) 
         {
            sprintf( message, "%.*s %#9.4g", strlen(message), message, val );
            if (fp) 
               sprintf( diskmess, "%.*s %10f", strlen(diskmess), diskmess, val );
         } 
         else 
         {
            sprintf( message, "%.*s %9s", strlen(message), message,"*" );
            if (fp) 
               sprintf( diskmess, "%.*s %10s", strlen(diskmess), diskmess, "*" );
         }
         r1 = 0; 
         gdsa_wcreal_c( Setin, &setlevel, Tname,
                        tofchar("FLUXPBC"), &val, &count, &one, &r1 );

         val = sumAP[box][subnr];
         if (val != blank) 
         {
            sprintf( message, "%.*s %#9.4g", strlen(message), message, val );
         } 
         else 
         {
            sprintf( message, "%.*s %9s", strlen(message), message,"*" );
         }
         r1 = 0; 
         gdsa_wcreal_c( Setin, &setlevel, Tname,
                        tofchar("SUMAP"), &val, &count, &one, &r1 );

         sprintf( message, "%.*s %7d", strlen(message), message, npoints[box][subnr] );
         r1 = 0; 
         gdsa_wcint_c( Setin, &setlevel, Tname,
                       tofchar("NDATA"), &npoints[box][subnr],
                       &count, &one, &r1 );

         sprintf( message, "%.*s %7d", strlen(message), message, nblanks[box][subnr] );
         r1 = 0; 
         gdsa_wcint_c( Setin, &setlevel, Tname,
                       tofchar("NBLANKS"), &nblanks[box][subnr],
                       &count, &one, &r1 );

         r1 = 0; 
         gdsa_wcint_c( Setin, &setlevel, Tname,
                       tofchar("XLO"), &bloa[box][0],
                       &count, &one, &r1 );
         r1 = 0; 
         gdsa_wcint_c( Setin, &setlevel, Tname,
                       tofchar("YLO"), &bloa[box][1],
                       &count, &one, &r1 );
         r1 = 0; 
         gdsa_wcint_c( Setin, &setlevel, Tname,
                       tofchar("XHI"), &bhia[box][0],
                       &count, &one, &r1 );
         r1 = 0; 
         gdsa_wcint_c( Setin, &setlevel, Tname,
                       tofchar("YHI"), &bhia[box][1],
                       &count, &one, &r1 );

         {
            int    h;
            fint   grids[MAXAXES];
            double coords[MAXAXES];
            char   cnamebuf[ITEMLEN];
            char   axisstr[VARLEN];
            int    len;

            r2 = getsubsetgrid( Setin, 
                                subin[subnr], 
                                axnum, 
                                grids, 
                                coords );
            for (h = 0; h < naxis; h++) 
            {
               if (r2 == -2) 
                  setdblank_c( &coords[h] );
               sprintf( cnamebuf, "SUBGRID%d", h+1);
               r1 = 0; 
               gdsa_wcint_c( Setin, &setlevel, Tname,
                             tofchar(cnamebuf), &grids[h],
                             &count, &one, &r1 );

               len = Axisnames.l;
               strncpy( axisstr, &Axisnames.a[h*len], len );
               axisstr[len] = '\0';
               r1 = 0; 
               gdsa_wcdble_c( Setin, &setlevel, Tname,
                              tofchar(axisstr), &coords[h],
                              &count, &one, &r1 );
            }
         }

         /* Get subset level */
         r1 = showcoord( Setin, &subin[subnr], axnum, subsetstr );
         if (r1 == 0) 
            strcpy( subsetstr.a, "Top level" );
         if (r1 < 0)  
            strcpy( subsetstr.a, "Cannot convert" );
         sprintf( message, "%.*s  (%-.*s)",
                  strlen(message), message,
                  nelc_c(subsetstr), subsetstr.a );
         if (fp) 
            sprintf( diskmess, "%.*s %-.*s",
                     strlen(diskmess), diskmess,
                     nelc_c(subsetstr), subsetstr.a );

         anyoutC( 3, message );
         if (fp) 
            fprintf( fp, "%s\n", diskmess );
      }
   }
   if (westerbork) 
      anyoutC( 3, "NOTE: 1 W.U. == 5 mJy/Beam.");
   if (polygon) 
   {
      /* Print the vertices */
      for (box = 0; box < boxcount; box++) 
      {
         char    longtxt[240];
         sprintf(longtxt, "VERTICES%d=", box+1 );
         for (i = 0; i < nvert[box]; i++) 
         {
            sprintf( longtxt, "%.*s (%d,%d)",
                     strlen(longtxt), longtxt,
                     vertXa[box][i], vertYa[box][i] );
         }
         anyoutC( 3, longtxt );
      }
   }

   /*-----------------------------------------------*/
   /* Determine min, max of all possible candidates */
   /*-----------------------------------------------*/

   for (i = 0; i < MAXOPT; i++) 
   {
      counter[i] = 0;
      Ymin[i] = Ymax[i] = blank;
   }
   for (box = 0; box < boxcount; box++) 
   {
      for (subnr = 0; subnr < nsubs; subnr++) 
      {
         float  val[MAXOPT];
         val[0] = sum[box][subnr];
         val[1] = sumPBC[box][subnr];
         val[2] = flux[box][subnr];
         val[3] = fluxPBC[box][subnr];
         for (i = 0; i < MAXOPT; i++) 
         {
            if (val[i] != blank) 
            {
               if (counter[i] == 0) 
               {
                  Ymax[i] = Ymin[i] = val[i];
               } 
               else 
               {
                  if (val[i] > Ymax[i]) Ymax[i] = val[i];
                  if (val[i] < Ymin[i]) Ymin[i] = val[i];
               }
            }
            counter[i]++;
         }
      }
   }

   /*-------------------------------------------------------------*/
   /* Do the actual plotting. First select (in a loop) the values */
   /* along the Y-axis. The selected option in OPTION= determines */
   /* min, max and labels in the plot. Min, max are already      */
   /* calculated.                                                 */
   /*-------------------------------------------------------------*/

   do 
   {
      if (flux[0][0] != blank) 
      {
         option = 3;
      } 
      else 
      {
         option = 1;
      }
      nitems = 1;
      dfault = REQUEST;
      key    = tofchar("OPTION=");
      sprintf( message, "0)Exit  1)Sum  2)SumPBC  3)Flux  4)FluxPBC  5)GRdev [%d]", option );
      mes    = tofchar( message );
      r1     = userint_c( &option, &nitems, &dfault, key, mes );
      if (option == 0) 
         break;
      if (option > 5) 
         option = 5;
      if (option < 1) 
         option = 1;
      cancel_c( key );
      
      if (option == 5)
      {           
         pgend_c();
         cancel_c( tofchar("GRDEVICE=") );
         (void) initplot( boxcount );         
      }
      else
      {
         for (box = 0; box < boxcount; box++) 
         {
            float   Xarray[MAXSUBSETS];
            float   Yarray[MAXSUBSETS];
            float   val = 0.0;
            fint    i;
   
            Xmin = 0; Xmax = nsubs;
            for (subnr = 0, i = 0; subnr < nsubs; subnr++) 
            {
               switch (option) 
               {
                  case 1 : 
                  {
                     val = sum[box][subnr];
                     strcpy( Toptitle, "Sum" );
                     sprintf( Ytitle, "%s", sumunit );
                  } 
                  break;
                  case 2 : 
                  {
                     val = sumPBC[box][subnr];
                     strcpy( Toptitle, "PBC sum" );
                     sprintf( Ytitle, "%s", sumPBCunit );
                  } 
                  break;
                  case 3 : 
                  {
                     val = flux[box][subnr];
                     strcpy( Toptitle, "Flux" );
                     sprintf( Ytitle, "%s", fluxunit );
                  } 
                  break;
                  case 4 : 
                  {
                     val = fluxPBC[box][subnr];
                     strcpy( Toptitle, "PBC flux" );
                     sprintf( Ytitle, "%s", fluxPBCunit );
                  } 
                  break;
               }
               if (val != blank)                                /* Filter blanks */
               {
                  Xarray[i] = subnr;
                  Yarray[i] = val;
                  i++;
               }
            }
            if ( (i > 0) && (Ymax[option-1] > Ymin[option-1]) ) 
            {
               strcpy( Xtitle, "Subset index" );
               (void) getbox( Setin, subdim, box, boxstr, STRLEN, polygon );
               sprintf( Toptitle, "%.*s in %-s",
                        strlen(Toptitle), Toptitle, boxstr );
               drawbox( Xmin, Ymin[option-1], Xmax, Ymax[option-1],
                        Xtitle, Ytitle, Toptitle );
               pgpt_c( &i, Xarray, Yarray, &symbol );
            } 
            else 
            {
               if (i == 0) 
               {
                  anyoutC( 3, "FLUX: No subsets to plot" );
               } 
               else 
               {
                  sprintf( message, "FLUX: (option %d)  Cannot scale", option );
                  anyoutC( 3, message );
               }
            }
         }
      }
   } 
   while(1);

   /*--------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is    */
   /* closed and HERMES is instructed to stop.               */
   /*--------------------------------------------------------*/

   pgend_c();
   if (fp != NULL) 
      fclose( fp );                                   /* Close the ASCII file */
   finis_c();
   return(EXIT_SUCCESS);
}
