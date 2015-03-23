/*
                            COPYRIGHT (c) 1999
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             stat.dc1

Program:       STAT

Purpose:       Program to calculate elementary statistics in user

Category:      CALCULATION, PLOTTING, TABLES

File:          stat.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 2048.


   BOX*=       Frame for input subsets.                [entire subset]
               It is possible to give more than one box for each
               subset (with a maximum of 64), which can be handy if
               user wants to compare different boxes in one subset.
               The keywords appear as BOX1=, BOX2= etc.
               The BOX*= prompt is repeated until carriage return is
               pressed.


   TOSCREEN=   Print results on screen?                             [Y]


** DISPTOT=    Display table of totals?  [default depends on TOSCREEN=]
               Overrule the TOSCREEN= keyword for table with
               statistics of totals.


   FILENAME=   Name of ASCII file:                  [No output to file]
               If a name is specified, an ASCII file is created
               where data is written without the subset information.
               If you press carriage return, there will be no output 
               to an ASCII file.


   APPEND=     File exists, ok to append?                         [Y]/N
               The file specified in FILENAME= already exists. You
               can append to this file with APPEND=Y. If APPEND=N
               you will be prompted for another name.


   FORMAT=     Give format for output:                     [Calculated]
               Print numbers on screen in user given output format.
               See description for possible formats. Keyword is only
               asked if TOSCREEN=Y


   GRDEVICE=   Plot device:                           [List of devices]
               Destination of plot, Screen or Hardcopy.


   PLOT=       [0]=quit,1=sum,2=mean,3=rms,4=min,5=max,6=pts,7=blks,8=DEV

               1: Plot sum (default)
               2: Plot mean
               3: Plot rms
               4: Plot minimum values
               5: Plot maximum values
               6: Plot number of points without blanks
               7: Plot number of blanks
               8: Select another graphics device (GRDEVICE=)


** TABNAME=    Give name of table to store results:              [STAT]
               Columns are created on set level. The table name
               for one box is STAT. If there are more boxes, the names
               are STAT1, STAT2 etc. The box number is appended by the
               program. TABNAME= specifies the name only.


** TABAPPEND=  Append to existing columns?                        Y/[N]
               If a table already exists, it is possible to append
               to this table with TABAPPEND=Y
               The default always creates a new table.


               GRAPHICS:

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


** PGWIDTH=    Give line width 1..21:                               [1]


** PGHEIGHT=   Give character height:                             [1.0]


** PGFONT=     Give font 1..4:                                      [2]


Description:   STAT determines mean, rms, minimum value, maximum
               value, number of blanks and number of non-blank values
               in one or more boxes in user given subsets. The boxes
               are asked with BOX1=, BOX2= etc. The default for
               BOX1= is the entire subset. For the next box keywords,
               pressing carriage return will end the 'box loop'.
               STAT has two loops. The outer loop is always the loop
               over all given boxes and the inner loop is a loop over
               all given subsets. Results are written to screen if
               TOSCREEN=Y (default) in a formatted way. The format
               applies for all double precision numbers: sum, mean,
               rms, minimum and maximum. The format images that are
               allowed are described further on. The default format
               is: FORMAT=fffff.fff
               If TOSCREEN=N but DISPTOT=Y, only the statistics on
               the totals will be displayed.

               The results are also stored in a GDS table in the
               descriptor of the set you are working on. For each
               box a table name is created (or you can give one
               yourself with the hidden keyword TABNAME= ). This
               name is appended by the box number if there is more
               than one box. The appending of a number is always done
               by the program. The default table name is STAT. If
               for example, there are two boxes, you will have two
               tables with names STAT1 and STAT2. Each table has
               columns SUM, MEAN, RMS, MINVAL, MAXVAL, VALIDS, NBLANKS.
               There is also a column with the box number and there
               are columns with physical coordinates. These columns
               have the names of the 'repeat' axes. For instance, if
               AURORA is a RA-DEC-FREQ cube and you specified
               INSET=AURORA FREQ than the frequency axis is the repeat
               axis and a column with frequencies will be created.
               If a transformation can be made to a secondary axis type
               (f.i. velocity) than the name of the column will be
               the name of the secondary axis type and the physical
               coordinates will also be converted. Further statistics
               on these columns, plotting, editing etc. can be done
               with the program TABLE. The columns are all written on
               set level. In TABLE you need to specify: INSET=AURORA to
               get a table directory with columns made by STAT.
               If you want to append to existing columns, use TABAPPEND=Y

               Plotting is done in a loop. Each loop you can chose
               which result you want to plot (PLOT=). The x-coordinate
               is always a physical coordinate (if a conversion of grid
               to physical coordinate could be made). The y-coordinate
               is one of sum, mean, rms, minval, maxval, valids (the
               number of non blank values in the statistics) and
               nblanks (the number of blanks). For each selection
               the results of each box is plotted. In this loop, you can
               change the destination of the output (GRDEVICE=). Plots
               are sent to the printer after STAT is finished.

               Note that if there is more than one repeat axis, the
               physical coordinates in the plot will be taken from the
               first repeat axis. Example: STAT INSET=AURORA F 1:10 RA 3:5
               will plot sum etc. in a plot where the frequencies
               are the x-coordinates. You will see three curves, one
               for RA 3,  one for RA 4 and one for RA 5.


               PLOTTING
               ========
               
               How do you change the layout of your plot? You have the
               possibility to overrule the default sizes of your plot
               with PGBOX= The number of plots on one page can be
               selected with PGMOSAIC=n,m where n is the number of plots
               in the x-direction and m the number of plots in the y-
               direction. Color is selected with PGCOLOR= The width
               of the lines with PGWIDTH= The height of the characters
               with PGHEIGHT= and the font with PGFONT= Hardcopy
               output can be controlled with PGPAPER= which defines
               size and aspect ratio of the plot on paper.



               FORMATS
               =======


               The specification in FORMAT= is called a 'format image'.
               A 'format image' is used to print numbers in a user given
               format consisting of characters representing the
               wanted output format. The syntax is:

 flag(s)       Zero or more flags, in any order, which modify the
               meaning of the conversion specification.  The flag
               characters and their meanings are:

      -        The result of the conversion is left-
               justified within the field.

      +        The result of a signed conversion always
               begins with a sign, "+" or "-".

 string        Characters, some with special meaning.
               If the string (f.i. FFFFF.FF or gggg.gg or wwwww)
               contains no dot, the number of characters specify
               a minimum field width.  For an output field, if the
               converted value has fewer characters than the field
               width, it is padded on the left (or right, if the
               left-adjustment flag, - has been given) to the field
               width.
               If the string contains a dot, the total number of
               characters including the dot is the minimum field width
               and the number of characters after the dot is the
               precision.

               The characters are used to determine the conversion
               type. If the string contains an:

               'e' or 'E'
                      The floating-point-number argument is
                      printed in the style [-]drddde+dd,
                      where there is one digit before the
                      radix character, and the number of
                      digits after it is equal to the
                      precision. The E conversion character
                      produces a number with E introducing
                      the exponent instead of e. The
                      exponent always contains at least two
                      digits.  However, if the value to be
                      printed requires an exponent greater
                      than two digits, additional exponent
                      digits are printed as necessary.

               'g' or 'G'

                      The floating-point-number argument is
                      printed in style f or e (or int style E
                      n the case of a G conversion
                      character), with the precision
                      specifying the number of significant
                      digits.  The style used depends on the
                      value converted; style e is used only
                      if the exponent resulting from the
                      conversion is less than -4 or greater
                      than or equal to the precision.

               others
                      Strings without 'e', 'E', 'g' and 'G'
                      indicate a floating point conversion.
                      The floating point number argument is
                      printed in decimal notation in the
                      style [-]dddrddd, where the number of
                      digits after the radix character, r, is
                      equal to the precision specification.

               If the result of a conversion is longer than the
               field width, an asterisk is returned. If the
               input number is a blank, a 'b' is returned.



Examples:      Format string: +eeeeee.eeee
                      Number: 43345.5436
                      Result:  +4.3346e+04
                      Remark: exponential format
                              signed conversion
                              field width: 12
                              precision:    4

               Format string: gggg.ggggg
                      Number: 34.43
                      Result:     34.430
                      Remark: Field width is 10
                              Number of significant digits is 5

               Format string: +ffff.ff
                      Number: 23.456
                      Result:   +23.46
                      Remark: signed conversion

               Format string: -ffff
                      Number: 345.87
                      Result: 346
                      Remark: left justified

               Format string: -+ffff.fff
                              Number: 34.43
                              Result: +34.430
                              Remark: left justified
                                      signed conversion

               Format string: eee.eeee
                      Number: 234.43
                      Result:        *
                      Remark: Field width too small
                              for conversion

               Format string: ffff.ff
                      Number: blank
                      Result:       b
                      Remark: input was a blank



               COLOR INDICES:
               =============

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


               AVAILABLE FONTS:
               ===============

               1  single stroke "normal" font
               2  roman font
               3  italic font
               4  script font


Notes:

Example:

Updates:       Nov 17,  1992: VOG, Document created.
               Oct 23,  1995: VOG, Option included to Write data to ASCII 
                                   file also (FILENAME=)..
               Feb  1,  2000: JPT, Increased number of subsets.
               Feb  9,  2001: VOG, Included subset coordinates in output file.
               Sep 10,  2013: VOG, Fixed sprintf problem (added nelc_c() for Fortran string)

#<
*/

/*  stat.c: include files     */

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
#include    "error.h"        /* User error handling routine.*/
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "axcoord.h"      /* Returns the coordinate type and units as returned by cotrans.*/
#include    "getusernam.h"   /* Returns the user name of the current user.*/
#include    "getdate.h"      /* Returns the current time and date as a text string.*/
#include    "printusing.h"   /* Print numbers using a format image.*/
#include    "cotrans.h"      /* Transformation from grid coords to physical coords. vv.*/
#include    "status.h"       /* Display additional information in the "RUNNING" status display.*/


/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "usercharu.h"
#include    "userchar.h"
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/

/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_name.h"    /* Return the name of an axis */
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "gdsd_rchar.h"   /* Read FITS data field.*/


/* Related to tables */

#include    "gdsa_crecol.h"  /* Creates a column in a GDS descriptor file.*/
#include    "gdsa_delcol.h"  /* Deletes a column in a GDS table.*/
#include    "gdsa_colinq.h"  /* Give information about columns in a GDS table.*/
#include    "gdsa_wcreal.h"  /* Write real items to a column in a GDS table.*/
#include    "gdsa_wcdble.h"  /* Double.*/
#include    "gdsa_wcint.h"   /* Integer. */


/* PGPLOT includes */

#include    "pgplot.h"


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
#define CBUFLEN        256             /* Buffer for display in log file */
#define MAXBOXES       64              /* Max. boxes in one run */
#define LONGLEN        512             /* A string length */
#define VARLEN         132             /* Char. size in tables */
#define STRLEN         180              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define ITEMLEN        10              /* Table, column name length */
#define FILENAMELEN    512
#define MAXOPTIONS     9
#define NUMDBLE        5               /* Number of double prec. parameters in stat.*/
#define NUMINT         2               /* Number of integer parameters in stat.*/
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0

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
static fint     showdev;            /* Device number (as in ) for info */
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
static fint     ablo[MAXBOXES][MAXAXES];
static fint     abhi[MAXBOXES][MAXAXES];
static char     boxstr[MAXBOXES][STRLEN];
static fint     boxopt;             /* The different options are: */
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */



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

/* Miscellaneous */

static fchar    Key, Mes;
static fint     setlevel = 0;       /* To get header items at set level. */
static float    Fblank;             /* Global value for BLANK. */
static double   Dblank;             /* Global value for BLANK. */
static char     message[3*LONGLEN];   /* All purpose character buffer. */
static int      boxcount;
static double   sum[MAXBOXES][MAXSUBSETS];
static double   mean[MAXBOXES][MAXSUBSETS];
static double   rms[MAXBOXES][MAXSUBSETS];
static double   minval[MAXBOXES][MAXSUBSETS];
static double   maxval[MAXBOXES][MAXSUBSETS];
static fint     nblanks[MAXBOXES][MAXSUBSETS];
static fint     valids[MAXBOXES][MAXSUBSETS];
static char     substr[MAXSUBSETS][STRLEN];
static float    physcoord[MAXSUBSETS];
static double   phys;
static float    Yvals[MAXSUBSETS];
static bool     displaytab;
static bool     displaytot;
static float    minmax[(NUMDBLE+NUMINT)*2];     /* For each stat item a min. and a max. */
static char     axname[MAXAXES][KEYLEN];
static char     axunit[MAXAXES][KEYLEN];
static fchar    Mapunits;
static char     mapunits[KEYLEN+1];
static fchar    Formstr;
static fchar    Tname;
static float    image[MAXBUF];                  /* Buffer for read routine. */
static char     filename[FILENAMELEN];
FILE            *fp = NULL;


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
/*--------------------------------------------------*/
/* Open file to write data extern. The              */
/* macro 'fmake' must be available.                 */
/*--------------------------------------------------*/
{
#define   KEY_FILENAME    tofchar("FILENAME=")
#define   MES_FILENAME    tofchar("Name of ASCII file:     [No output to file]")
#define   KEY_APPEND      tofchar("APPEND=")
#define   MES_APPEND      tofchar("File exists, ok to append?    [Y]/N")

   fchar    Filename;
   bool     append;
   fint     dfault;
   fint     n;
   fint     nitems;
   fint     agreed;
   FILE     *fp = NULL;


   dfault = REQUEST;
   fmake( Filename, FILENAMELEN );
   do 
   {
      append = toflog(YES);                               /* Default APPEND=Y */
      n = usertext_c( Filename,
                      &dfault,
                      KEY_FILENAME,
                      MES_FILENAME );
      if (n == 0) 
         return( NULL );

      strncpy( filename, Filename.a, nelc_c(Filename) );

      fp = fopen( filename, "r" );
      if (fp != NULL) 
      {                                                    /* The file exists */
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
      if (!append) 
      {
          cancel_c( KEY_FILENAME );
          agreed = NO;
      }
      else 
      {
         fp = fopen(filename, "a");
         agreed = (fp != NULL);
         if (!agreed) 
            reject_c( KEY_FILENAME, tofchar("Cannot open, try another!") );
      }
   } 
   while (!agreed);
   return( fp );
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
   xl = 0.15; xr = 0.9;
   yb = 0.1; yt = 0.9;
   pgsvp_c( &xl, &xr, &yb, &yt );
}


void drawbox( float *VXmin, float *VYmin, float *VXmax, float *VYmax,
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
   fint   nitems;
   fint   dfault;
   float  pg_box[4];                          /* Corners of draw box. */
   fint   color;
   fint   font;
   fint   nxsub, nysub;
   float  xtick, ytick;
   char   message[80];
   float  Xmin, Xmax, Ymin, Ymax;


   Xmin = (*VXmin);
   Xmax = (*VXmax);
   Ymin = (*VYmin);
   Ymax = (*VYmax);

   pgpage_c();                                /* Advance to new page. */

   /* Increase the size of the box a little */
   delta = fabs( Xmax - Xmin ) / 20.0;
   if (delta == 0.0) delta = 1.0;
   Xmin -= delta; Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 20.0;
   if (delta == 0.0) delta = 1.0;
   Ymin -= delta; Ymax += delta;
   pg_box[0] = Xmin; pg_box[1] = Ymin;        /* Get size from user input */
   pg_box[2] = Xmax; pg_box[3] = Ymax;
   nitems = 4; dfault = HIDDEN;
   sprintf( message, "Corners of box Xl,Yl, Xh,Yh:  [%g,%g,%g,%g]", Xmin,Ymin,Xmax,Ymax );
   (void) userreal_c( pg_box,
                    &nitems,
                    &dfault,
                    tofchar("PGBOX="),
                    tofchar( message ) );
   Xmin = pg_box[0]; Ymin = pg_box[1];
   Xmax = pg_box[2]; Ymax = pg_box[3];
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );    /* Set the window */

   color = 1; nitems = 1; dfault = HIDDEN;
   (void) userint_c( &color,
                   &nitems,
                   &dfault,
                   tofchar("PGCOLOR="),
                   tofchar("Give color 1..15:        [1]") );
   if (color > 15) color = 15;
   if (color < 1 ) color =  1;
   pgsci_c( &color );

   lwidth = 1; nitems = 1; dfault = HIDDEN;
   (void) userint_c( &lwidth,
                   &nitems,
                   &dfault,
                   tofchar("PGWIDTH="),
                   tofchar("Give line width 1..21:        [1]") );
   if (lwidth > 21) lwidth = 21;
   if (lwidth < 1 ) lwidth =  1;
   pgslw_c( &lwidth )      ;                  /* Set line width. */

   charsize = 1.0; nitems = 1; dfault = HIDDEN;
   (void) userreal_c( &charsize,
                    &nitems,
                    &dfault,
                    tofchar("PGHEIGHT="),
                    tofchar("Give character height:     [1.0]") );
   pgsch_c( &charsize );                      /* Character height. */

   font = 2; nitems = 1; dfault = HIDDEN;
   (void) userint_c( &font,
                   &nitems,
                   &dfault,
                   tofchar("PGFONT="),
                   tofchar("Give font 1..4:        [2]") );
   if (font > 4) font = 4;
   if (font < 1) font = 1;
   pgscf_c( &font );                          /* Set font. */

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

   pglab_c( tofchar(xtitle), tofchar(ytitle), tofchar(toptitle) );

   (*VXmin) = Xmin;
   (*VXmax) = Xmax;
   (*VYmin) = Ymin;
   (*VYmax) = Ymax;

}


static void boxstring( char *boxstr, fint *blo, fint *bhi )
/*----------------------------------------------------------*/
/*----------------------------------------------------------*/
{
   int     n;


   boxstr[0] = '\0';
   strcpy( boxstr, "Box: [" );
   for (n = 0; n < subdim; n++) {
      (void) sprintf( boxstr, "%.*s %d", (int) strlen(boxstr), boxstr, blo[n] );
   }
   strcat( boxstr, "," );
   for (n = 0; n < subdim; n++) {
      (void) sprintf( boxstr, "%.*s %d", (int) strlen(boxstr), boxstr, bhi[n] );
   }
   strcat( boxstr, " ]" );
}



static void showsub( fchar Setin, fint subset,
                     fint *axnum, char *boxstr )
/*--------------------------------------------------------------*/
/* Show subset and box on status line.                          */
/*--------------------------------------------------------------*/
{
   fint   setdim, subdim;
   int    n;
   fchar  Axisname;
   fint   r1, r2;
   fint   grid;
   char   showstr[LONGLEN];



   setdim = gdsc_ndims_c( Setin, &setlevel );                   /* dimension of set */
   subdim = gdsc_ndims_c( Setin, &subset );

   strcpy( showstr, boxstr );
   if (setdim == subdim) {
      strcat( showstr, " Set level" );
      return;
   }
   strcat( showstr, " Subset:" );
   fmake( Axisname, STRLEN );
   for (n = subdim; n < setdim; n++) {
      r2 = r1 = 0;
      gdsc_name_c( Axisname, Setin, &axnum[n], &r1 );
      grid = gdsc_grid_c( Setin, &axnum[n], &subset, &r2 );
      if ( (n + 1) == setdim) {
         (void) sprintf( showstr, "%.*s%s=%d ", (int) strlen(showstr), showstr,
                         strtok( Axisname.a, " -" ), grid );
      } else {
         (void) sprintf( showstr, "%.*s%s=%d,", (int) strlen(showstr), showstr,
                         strtok( Axisname.a, " -" ), grid );
      }
   }
   status_c( tofchar(showstr) );
}


static int plotmenu( void )
/*--------------------------------------------------*/
/* Present menu and select plot.                    */
/*--------------------------------------------------*/
{
   fint          nitems, dfault;
   fint          option = 0;
   int           scr = 1;
   bool          agreed;
   static fint   first = YES;


   if (first) {
      anyoutC( scr, " " );
      anyoutC( scr, "          ====== PLOT OPTIONS =====");
      anyoutC( scr, "          0: Quit this plot loop" );
      anyoutC( scr, "          1: Plot sum (default)" );
      anyoutC( scr, "          2: Plot mean" );
      anyoutC( scr, "          3: Plot rms" );
      anyoutC( scr, "          4: Plot minimum values" );
      anyoutC( scr, "          5: Plot maximum values" );
      anyoutC( scr, "          6: Plot number of points without blanks" );
      anyoutC( scr, "          7: Plot number of blanks" );
      anyoutC( scr, "          8: Select another graphics device" );
      anyoutC( scr, " " );
      first = NO;
   }
   nitems = 1;
   dfault = REQUEST;
   do {
      Mes = tofchar("[0]=quit,1=sum,2=mean,3=rms,4=min,5=max,6=pts,7=blks,8=DEV");
      Key = tofchar("PLOT=");
      (void) userint_c( &option, &nitems, &dfault, Key, Mes );
      agreed = ((option >= 0) && (option <= (MAXOPTIONS-1)));
      if (!agreed) reject_c( Key, tofchar("Wrong option!") );
   } while (!agreed);
   cancel_c( Key );
   return( option );
}


static void dbletofloat( double *ddat, float *fdat, fint nsubs )
/*--------------------------------------------------------------*/
/* Convert a double array to a float array. Convert blanks in a */
/* proper way.                                                  */
/*--------------------------------------------------------------*/
{
   int      i;


   for (i = 0; i < nsubs; i++) {
      if (ddat[i] == Dblank) {
         fdat[i] = Fblank;
      } else {
         fdat[i] = (float) ddat[i];
      }
   }
}



static void inttofloat( fint *idat, float *fdat, fint nsubs )
/*--------------------------------------------------------------*/
/* Convert a int array to a float array. Do not check on blanks */
/*--------------------------------------------------------------*/
{
   int      i;


   for (i = 0; i < nsubs; i++) {
      fdat[i] = (float) idat[i];
   }
}





static void statistics( fchar Setin, fint subset, fint *blo, fint *bhi,
                        double *vsum, double *vmean, double *vrms,
                        double *vmin, double *vmax, fint *vvalids,
                        fint *vnblanks )
/*------------------------------------------------------------------*/
/* Statistics on (part of) subset. Use all data in subset between   */
/* blo and bhi.                                                     */
/*------------------------------------------------------------------*/
{
   double      sum     = 0.0;
   double      sum2    = 0.0;
   double      mean    = 0.0;
   double      rms     = 0.0;
   double      minval  = 0.0;
   double      maxval  = 0.0;
   double      var     = 0.0;
   double      value   = 0.0;
   int         nblanks = 0;
   int         valids  = 0;
   int         i;

   /* Reading data */

   fint        cwlo, cwhi;         /* Coordinate words. */
   fint        tid;                /* Transfer id for read function. */
   fint        maxIObuf = MAXBUF;  /* Maximum size of read buffer. */
   fint        pixelsread;         /* Number of pixels read by read routine. */


   cwlo   = gdsc_fill_c( Setin, &subset, blo );
   cwhi   = gdsc_fill_c( Setin, &subset, bhi );
   tid    = 0;
   do {
      /* Read 'maxIObuf' values in 'image'. */
      gdsi_read_c( Setin,
                   &cwlo, &cwhi,
                   image,
                   &maxIObuf,
                   &pixelsread,
                   &tid );
      for (i = 0; i < pixelsread; i++) {
         if (image[i] != Fblank) {
            value = (double) image[i];
            valids++;
            sum  += value;
            sum2 += value * value;
            if (valids == 1) {
               minval = value; maxval = value;
            } else {
               if (value < minval) minval = value;
               if (value > maxval) maxval = value;
            }
         } else {
            nblanks++;
         }
      }
   } while (tid != 0);

   if (valids == 0) {
      sum = rms = mean = minval = maxval = Dblank;
   } else {
      mean = sum / valids;
      if (valids == 1) {
         rms = 0;
      } else {
         var  = (1.0 / ((double) valids - 1)) * (sum2 - valids * mean * mean);
         if (var >= 0.0) {
            rms = sqrt( var );
         } else {
            rms = Dblank;
         }
      }
   }
   *vsum  = sum;
   *vmean = mean;
   *vrms  = rms;
   *vmax  = maxval;
   *vmin  = minval;
   *vvalids  = valids;
   *vnblanks = nblanks;
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
   fint     r1;
   double   coordin[MAXAXES];    /* Grids before transformation */
   double   coordout[MAXAXES];   /* Physical coordinates after transformation */
   fint     direction;           /* grid coord. -> physical coord. */


   setdim = gdsc_ndims_c( Setin, &setlevel );              /* dimension of set */
   subdim = gdsc_ndims_c( Setin, &subset );                /* dimension of subset */
   if (setdim == subdim) {
      strcpy( substr, "0" );
      return( strlen(substr) );
   }
   for (i = 0; i < subdim; i++) coordin[i] = 0.0;
   for (i = subdim; i < setdim; i++) {
      r1 = 0;
      coordin[axnum[i]-1] = (double) gdsc_grid_c( Setin, &axnum[i], &subset, &r1 );
   }
   direction = 1;                           /* grid coord. -> physical coord. */
   r1 = cotrans_c( Setin, &subset, coordin, coordout, &direction );

   for (i = subdim; i < setdim; i++) {
      if (i == subdim) {
         if (r1 == 0) {
            (void) sprintf( substr, " %4d  %.6g", (int)coordin[i], coordout[axnum[i]-1] );
         } else {
            (void) sprintf( substr, " %4d", (int) coordin[i] );
         }
      } else {
         if (r1 == 0) {
            (void) sprintf( substr, "%.*s,  %4d  %.6g",
                            (int) strlen(substr), substr,
                            (int)coordin[i], coordout[axnum[i]-1] );
         } else {
            (void) sprintf( substr, "%.*s, %.4d", (int) strlen(substr), substr, (int)coordin[i] );
         }
      }
   }
   if (r1 == 0) *phys = coordout[axnum[subdim]-1]; else *phys = coordin[subdim];
   return( strlen(substr) );
}


static void setminmax( double sum, double mean, double rms,
                       double minval, double maxval,
                       fint valids, fint nblanks, float *minmax )
/*-----------------------------------------------------------------*/
/* Update min, max of each array with the array elements 'sum',    */
/* 'mean', etc. The 'minmax' array must be initialized with blanks */
/*-----------------------------------------------------------------*/
{
   double    val[NUMDBLE];
   fint      ival[NUMINT];
   int       i;


   val[0] = sum;
   val[1] = mean;
   val[2] = rms;
   val[3] = minval;
   val[4] = maxval;


   for (i = 0; i < NUMDBLE; i++) {
      if (val[i] != Dblank) {
         if (minmax[i*2] == Fblank) {
            minmax[i*2]   = (float) val[i];
            minmax[i*2+1] = (float) val[i];
         } else {
            if (val[i] < (double) minmax[i*2])   minmax[i*2]   = (float) val[i];
            if (val[i] > (double) minmax[i*2+1]) minmax[i*2+1] = (float) val[i];
         }
      }
   }
   ival[0] = valids;
   ival[1] = nblanks;
   for (i = NUMDBLE; i < (NUMDBLE+NUMINT); i++) {
      if (minmax[i*2] == Fblank) {
         minmax[i*2]   = (float) ival[i-NUMDBLE];
         minmax[i*2+1] = (float) ival[i-NUMDBLE];
      } else {
         if (ival[i-NUMDBLE] < (int) minmax[i*2])   minmax[i*2]   = (float) ival[i-NUMDBLE];
         if (ival[i-NUMDBLE] > (int) minmax[i*2+1]) minmax[i*2+1] = (float) ival[i-NUMDBLE];
      }
   }
}


static fint selectmarker( int box )
/*---------------------------------------------*/
/* For each box, there must be a unique plot   */
/* symbol (from PGPLOT manual)                 */
/*---------------------------------------------*/
{
   return( box + 2 );
}



static void border( int scr, int len )
/*---------------------------------------------*/
/* Draw line with '=' of length len.           */
/*---------------------------------------------*/
{
   char    cbuf[CBUFLEN+1];

   len = MYMIN( len, CBUFLEN );
   memset( cbuf, '=', len);
   cbuf[len] = '\0';
   anyoutC( scr, cbuf );
}


static void plotarray( int box, int len, float *X, float *Y )
/*------------------------------------------------------*/
/* Select a marker for the current box, plot the points */
/* and connect. Take care of blanks (do not plot).      */
/* If direction of values in x-array changes, do not    */
/* connect points (f.i. with more than 1 subset axes).  */
/*------------------------------------------------------*/
{
   int      i;
   fint     j;
   float    Xdum[MAXSUBSETS];
   float    Ydum[MAXSUBSETS];
   fint     symbol;
   int      s1, s2;




   symbol = selectmarker( box );
   if (len == 0) return;
   if (len == 1) {
      if (Y[0] != Fblank) {
         j = 1;
         pgpt_c( &j, X, Y, &symbol );
      } else {
         anyoutC( 1, "Array contains only one (blank) value!" );
      }
      return;
   }

   s1 = (X[1] >= X[0]);
   for (i = 0, j = 0; i < len; i++) {
      if (i == 0) s2 = s1; else s2 = (X[i] >= X[i-1]);
      if (Y[i] != Fblank) {
         if (s1 != s2) {
            pgpt_c( &j, Xdum, Ydum, &symbol );
            pgline_c( &j, Xdum, Ydum );
            j = 0;
         }
         Xdum[j] = X[i];
         Ydum[j] = Y[i];
         j++;
      } else {
         pgpt_c( &j, Xdum, Ydum, &symbol );
         pgline_c( &j, Xdum, Ydum );
         j = 0;
      }
   }
   if (j != 0) {
      pgpt_c( &j, Xdum, Ydum, &symbol );
      pgline_c( &j, Xdum, Ydum );
   }
}




static void plotboxnr( int box, float minX, float minY,
                       float maxX, float maxY )
/*------------------------------------------------------------*/
/* Plot used box in upper left corner of the plot.            */
/*------------------------------------------------------------*/
{
   float    deltaX, deltaY;
   fint     color;
   float    Xp, Yp, Ypp;
   fint     one = 1;
   fint     mark;
   float    newheight, oldheight;
   float    zero = 0.0;


   deltaY = (maxY-minY)/20.0;
   deltaX = (maxX-minX)/30.0;
   color = box + 1;
   pgsci_c( &color );
   /* Select a marker */
   mark = selectmarker( box );
   /* Plot current symbol+box in upper left corner */
   /* but first determine position. */
   Xp = minX + deltaX;
   Yp = maxY - (box+1.5)*deltaY;
   Ypp = Yp + 0.2*deltaY;
   pgpt_c( &one, &Xp, &Ypp, &mark );        /* Plot symbol */
   Xp += deltaX;
   pgqch_c( &oldheight );
   newheight = oldheight/1.2;
   pgsch_c( &newheight );
   pgptxt_c( &Xp, &Yp, &zero, &zero, tofchar(boxstr[box]) );
   pgsch_c( &oldheight );
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
   sprintf( message, "%.*s", nelc_c( Idstr ), Idstr.a );
   getdate_c( Idstr );
   sprintf( message, "%.*s %.*s", (int) strlen(message), message,
            nelc_c( Idstr ), Idstr.a );
   disp  = 3.0;
   coord = 0.5;
   fjust = 0.5;
   pgmtxt_c( tofchar("R"), &disp, &coord, &fjust, tofchar(message) );
   pgsch_c( &oldheight );
}



static int screenheader( fchar  Setin, 
                         fint   subset, 
                         int    box,
                         int    *firstcolumnwidth,
                         bool   toscreen,
                         FILE   *fp )
/*-------------------------------------------------------------*/
/* Create a header above the table in the log file. The header */
/* includes the name of the statistics parameter and the units.*/
/* The first column contains the physical coordinates. The     */
/* maximum width of the first column will be returned in the   */
/* variable 'firstcolumnwidth'.                                */
/* The widths of the double prec. columns is set by FORMAT=    */
/*-------------------------------------------------------------*/
{
   fint    setdim, subdim;
   int     l1, l2, l3;
   int     scr = 3;
   int     i, j;
   double  dummy = 0.0;
   fint    dfault, nitems;
   char    cbuf1[STRLEN];
   char    cbuf2[STRLEN];
   int     colw1;
   char    cbuf[CBUFLEN+1];


   colw1  = (*firstcolumnwidth);
   nitems = 1;
   dfault = REQUEST;
   strcpy( Formstr.a, "fffff.fff" );
   Key    = tofchar("FORMAT=");
   (void) sprintf( message, "Give format for output:     [%s]", Formstr.a );
   Mes    = tofchar(message);
   (void) userchar_c( Formstr, &nitems, &dfault, Key, Mes );
   l1     = printusing_c( Formstr, &dummy, Mes );
   anyoutC( scr, " ");
   anyoutC( scr, boxstr[box] );
   setdim = gdsc_ndims_c( Setin, &setlevel );              /* dimension of set */
   subdim = gdsc_ndims_c( Setin, &subset );
   if (subdim == setdim) {
      cbuf1[0] = cbuf2[0] = '\0';
   } else {
      (void) sprintf( cbuf1, "[" );
      (void) sprintf( cbuf2, "(" );
   }
   for (i = subdim, j = 0; i < setdim; i++, j++) {
      if ( (i+1) < setdim ) {
         (void) sprintf( cbuf1, "%.*s%s,",
                         (int) strlen(cbuf1), cbuf1,
                         axname[j] );
         (void) sprintf( cbuf2, "%.*s%s,",
                         (int) strlen(cbuf2), cbuf2,
                         axunit[j] );
      } else {
         (void) sprintf( cbuf1, "%.*s%s]",
                         (int) strlen(cbuf1), cbuf1,
                         axname[j] );
         (void) sprintf( cbuf2, "%.*s%s)",
                         (int) strlen(cbuf2), cbuf2,
                         axunit[j] );
      }
   }
   l2 = strlen( cbuf1 );
   if (l2 > colw1) colw1 = l2;
   l2 = strlen( cbuf2 );
   if (l2 > colw1) colw1 = l2;
   l2 = sprintf( cbuf, "%*.*s %*s %*s %*s %*s %*s %*s %*s",
                 colw1, colw1, cbuf1,
                 l1, "SUM", l1, "MEAN", l1, "RMS",
                 l1, "MIN", l1, "MAX", 7, "PTS", 6, "BLANKS" );

   if (toscreen)                 
      anyoutC( scr, cbuf );
   
   if (fp)
   {
      /* Write header without subset info to ascii file */
/*      fprintf( fp, "!%s\n", &cbuf[*firstcolumnwidth] );   */
      fprintf( fp, "!%s\n", cbuf );
   }

   /* Now the units */

   l3 = nelc_c(Mapunits);
   Mapunits.a[l3] = '\0';
   l3 = sprintf( cbuf, "%*.*s %*s %*s %*s %*s %*s %*s %*s",
                 colw1, colw1, cbuf2,
                 l1, Mapunits.a,
                 l1, Mapunits.a,
                 l1, Mapunits.a,
                 l1, Mapunits.a,
                 l1, Mapunits.a,
                 7, "#",
                 6, "#" );
                 
   if (toscreen)
      anyoutC( scr, cbuf );
   
   if (fp)
   {
      /* Write header without subset info to ascii file */
/*      fprintf( fp, "!%s\n", &cbuf[*firstcolumnwidth] );*/
      fprintf( fp, "!%s\n", cbuf );
   }

   border( scr, MYMAX(l2, l3) );
   (*firstcolumnwidth) = colw1;
   return( MYMAX(l2, l3) );
}



static void displaystr( int   maxstrlen, 
                        int   box, 
                        int   sub, 
                        fchar Formstr,
                        bool  toscreen,
                        FILE *fp )
/*----------------------------------------------------------------------*/
/* Create a string with the data using the routine 'printusing' for the */
/* format in FORMAT=  The string cannot exceed the length of CBUFLEN.   */
/* 'maxstrlen' is the width of the first column.                        */
/*----------------------------------------------------------------------*/
{
   fint     len;
   char     cbuf[CBUFLEN+1];
   fchar    Dumstr;


   fmake( Dumstr, STRLEN );
   len  = sprintf( cbuf, "%-*.*s", maxstrlen, maxstrlen, substr[sub] );
   len += printusing_c( Formstr, &sum[box][sub], Dumstr ) + 1;
   if (len < CBUFLEN)
      (void) sprintf( cbuf, "%.*s %.*s", (int) strlen(cbuf), cbuf, nelc_c(Dumstr), Dumstr.a );
   len += printusing_c( Formstr, &mean[box][sub], Dumstr ) + 1;
   if (len < CBUFLEN)
      (void) sprintf( cbuf, "%.*s %.*s", (int) strlen(cbuf), cbuf, nelc_c(Dumstr), Dumstr.a );
   len += printusing_c( Formstr, &rms[box][sub], Dumstr ) + 1;
   if (len < CBUFLEN)
      (void) sprintf( cbuf, "%.*s %.*s", (int) strlen(cbuf), cbuf, nelc_c(Dumstr), Dumstr.a );
   len += printusing_c( Formstr, &minval[box][sub], Dumstr ) + 1;
   if (len < CBUFLEN)
      (void) sprintf( cbuf, "%.*s %.*s", (int) strlen(cbuf), cbuf, nelc_c(Dumstr), Dumstr.a );
   len += printusing_c( Formstr, &maxval[box][sub], Dumstr ) + 1;
   if (len < CBUFLEN)
      (void) sprintf( cbuf, "%.*s %.*s", (int) strlen(cbuf), cbuf, nelc_c(Dumstr), Dumstr.a );
   len += 7;
   if (len < CBUFLEN)
      (void) sprintf( cbuf, "%.*s %7d", (int) strlen(cbuf), cbuf, valids[box][sub] );
   len += 7;
   if (len < CBUFLEN)
      (void) sprintf( cbuf, "%.*s %6d", (int) strlen(cbuf), cbuf, nblanks[box][sub] );

   if (toscreen)
      anyoutC( 3, cbuf );
   
   if (fp)
/*      fprintf( fp, " %s\n", &cbuf[strlen(substr[sub])] );*/
      fprintf( fp, "%s\n", cbuf );
}


static void checkcol( fchar Tname, fint subin, char *colname,
                      char *coltype, char *colunit, char *comments,
                      bool append, fint *collen )
/*-------------------------------------------------------------------*/
/* Does this column already exist? If so, delete it if you do not    */
/* want to append.                                                   */
/*-------------------------------------------------------------------*/
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
   r1 = 0;
   gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
   exist = (r1 >= 0);
   if (exist && !append) {
      r2 = 0;
      gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
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


static int getsubsetgrid( fchar Setin, fint subin, fint *axnum,
                          fint *grids, double *coords )
/*------------------------------------------------------------------*/
/* 'coords' will contain the physical coordinates after a call to   */
/* 'cotrans'. If the transformation was not successful, return the  */
/* grids.                                                           */
/*------------------------------------------------------------------*/
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

   if (res < 0) {
      for (n = subdim, i = 0; n < setdim; n++ ) {
         coords[i++] = coordin[(int) axnum[n]-1];
      }
      return(-2);
   }
   for (n = subdim, i = 0; n < setdim; n++ ) {
      coords[i] = coordout[ (int) axnum[n]-1 ];
      i++;
   }
   return(1);
}



static void inittable( fchar Setin, fint subset, fchar Tname, int box,
                       char *mapunits, int naxis, bool append, fint *collen )
/*----------------------------------------------------------------------*/
/* Create columns. If column already exist, delete before (re)creation  */
/*----------------------------------------------------------------------*/
{
   int    i;
   fint   nrows;
   char   comment[VARLEN];


   sprintf( comment, "%d == %s", box + 1, boxstr[box] );
   checkcol( Tname, subset, "SUM",    "DBLE", mapunits, "Sum", append, &nrows );
   checkcol( Tname, subset, "MEAN",   "DBLE", mapunits, "Mean", append, &nrows );
   checkcol( Tname, subset, "RMS",    "DBLE", mapunits, "Rms", append, &nrows );
   checkcol( Tname, subset, "MINVAL", "DBLE", mapunits, "Minimum of data", append, &nrows );
   checkcol( Tname, subset, "MAXVAL", "DBLE", mapunits, "Maximum of data", append, &nrows );
   checkcol( Tname, subset, "NDATA",  "INT", "#", "Valid points in statistics", append, &nrows );
   checkcol( Tname, subset, "NBLANKS","INT", "#", "Number of blanks in statistics", append, &nrows );
   checkcol( Tname, subset, "BOX",    "INT", "GRIDS", comment, append, &nrows );

   if (append) *collen = nrows;


   for (i = 0; i < naxis; i++) {
      char   cnamebuf[ITEMLEN];

      (void) sprintf( cnamebuf, "SUBGRID%d", i+1);
      checkcol( Tname, subset, cnamebuf, "INT", "GRIDS",
               "Subset in grid coordinates", append, &nrows );
      checkcol( Tname, subset, axname[i], "DBLE", axunit[i],
               "Subset in physical coordinates", append, &nrows );
   }
}



static void filltab(  fchar Setin, fint subset, fchar Tname, fint count, int box,
                      int naxis, double sum, double mean, double rms,
                      double minval, double maxval, fint valids, fint nblanks )
/*--------------------------------------------------------------------------------*/
/* Fill GDS table with these values.                                              */
/*--------------------------------------------------------------------------------*/
{
   fint    r1;
   fint    one = 1;
   fint    boxnr = (fint) (box + 1);
   fint    grids[MAXAXES];
   double  coords[MAXAXES];
   char    cnamebuf[ITEMLEN];
   int     h;


   r1 = 0; gdsa_wcdble_c( Setin, &setlevel, Tname,
                          tofchar("SUM"), &sum, &count, &one, &r1 );
   r1 = 0; gdsa_wcdble_c( Setin, &setlevel, Tname,
                          tofchar("MEAN"), &mean, &count, &one, &r1 );
   r1 = 0; gdsa_wcdble_c( Setin, &setlevel, Tname,
                          tofchar("RMS"), &rms, &count, &one, &r1 );
   r1 = 0; gdsa_wcdble_c( Setin, &setlevel, Tname,
                          tofchar("MINVAL"), &minval, &count, &one, &r1 );
   r1 = 0; gdsa_wcdble_c( Setin, &setlevel, Tname,
                          tofchar("MAXVAL"), &maxval, &count, &one, &r1 );

   r1 = 0; gdsa_wcint_c( Setin, &setlevel, Tname,
                         tofchar("NDATA"), &valids, &count, &one, &r1 );

   r1 = 0; gdsa_wcint_c( Setin, &setlevel, Tname,
                         tofchar("NBLANKS"), &nblanks, &count, &one, &r1 );

   r1 = 0; gdsa_wcint_c( Setin, &setlevel, Tname,
                         tofchar("BOX"), &boxnr, &count, &one, &r1 );


    /* Physical coordinates on subset axes */

   r1 = getsubsetgrid( Setin, subset, axnum, grids, coords );
   for (h = 0; h < naxis; h++) {
      if (r1 == -2) setdblank_c( &coords[h] );  /* Cotrans error */
      (void) sprintf( cnamebuf, "SUBGRID%d", h+1);
      r1 = 0; gdsa_wcint_c( Setin, &setlevel, Tname,
                            tofchar(cnamebuf), &grids[h],
                            &count, &one, &r1 );
      r1 = 0; gdsa_wcdble_c( Setin, &setlevel, Tname,
                             tofchar(axname[h]), &coords[h],
                             &count, &one, &r1 );
   }
}



static void statstat( int box, int nsubs, int firstcolwidth, fchar Formstr )
{
#define    MAXSTAT     5

   int      i, j;
   double   Ssum[MAXSTAT], Ssum2[MAXSTAT];
   double   Smean[MAXSTAT];
   double   Srms[MAXSTAT];
   double   Sminval[MAXSTAT];
   double   Smaxval[MAXSTAT];
   double   val[MAXSTAT];
   int      Svalids[MAXSTAT];
   fint     Snblanks[MAXSTAT];
   fchar    Dumstr;


   for (j = 0; j < MAXSTAT; j++) {
      Ssum[j] = Ssum2[j] = 0.0;
      Snblanks[j] = Svalids[j] = 0;
   }
   for (i = 0; i < nsubs; i++) {
      val[0] = sum[box][i];
      val[1] = mean[box][i];
      val[2] = rms[box][i];
      val[3] = minval[box][i];
      val[4] = maxval[box][i];
      for (j = 0; j < MAXSTAT; j++) {
         double     value = val[j];
         if (value != Dblank) {
            Svalids[j]++;
            Ssum[j]  += value;
            Ssum2[j] += value * value;
            if (Svalids[j] == 1) {
               Sminval[j] = value; Smaxval[j] = value;
            } else {
               if (value < Sminval[j]) Sminval[j] = value;
               if (value > Smaxval[j]) Smaxval[j] = value;
            }
         } else {
            Snblanks[j]++;
         }
      }
   }

   for (j = 0; j < MAXSTAT; j++) {
      double     var;
      if (Svalids[j] == 0) {
         Ssum[j] = Srms[j] = Smean[j] = Sminval[j] = Smaxval[j] = Dblank;
      } else {
         Smean[j] = Ssum[j] / Svalids[j];
         if (Svalids[j] == 1) {
            Srms[j] = 0;
         } else {
            var  = (1.0 / ((double) Svalids[j] - 1)) *
                   (Ssum2[j] - Svalids[j]*Smean[j]*Smean[j]);
            if (var >= 0.0) {
               Srms[j] = sqrt( var );
            } else {
               Srms[j] = Dblank;
            }
         }
      }
   }

   fmake( Dumstr, STRLEN );
   (void) sprintf( message, "column ->" );
   anyoutC( 3, message );
   (void) sprintf( message, "tot. stats.:");
   anyoutC( 3, message );
   for (j = 0; j < MAXSTAT; j++) {
      fint     len;
      char     cbuf[CBUFLEN+1];
      char     field[STRLEN];


      if (j == 0) strcpy( field, "Sum:" );
      if (j == 1) strcpy( field, "Mean:" );
      if (j == 2) strcpy( field, "Rms:" );
      if (j == 3) strcpy( field, "Min:" );
      if (j == 4) strcpy( field, "Max:" );
      len  = sprintf( cbuf, "%*.*s", firstcolwidth, firstcolwidth, field );
      for (i = 0; i < MAXSTAT; i++) {
         double   val;
         if (j == 0) val = Ssum[i];
         if (j == 1) val = Smean[i];
         if (j == 2) val = Srms[i];
         if (j == 3) val = Sminval[i];
         if (j == 4) val = Smaxval[i];

         len += printusing_c( Formstr, &val, Dumstr ) + 1;
         if (len < CBUFLEN) {
            (void) sprintf( cbuf, "%.*s %.*s", (int) strlen(cbuf), cbuf, nelc_c(Dumstr), Dumstr.a );
         }
      }
      anyoutC( 3, cbuf );
   }
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint      nitems, dfault;
   bool      entire = NO;
   int       sub, box;
   int       i, j;
   fint      r1, r2;
   int       firstcolwidth;
   double    minphys, maxphys;
   bool      first;
   int       option;
   fchar     Setstr;
   int       clen;
   bool      append;
   fint      rowcount;
   fint      nrows;
   int       naxis;
   int       tabwidth = 0;


   init_c();                               /* Contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }


   /* Some advertisements */
   anyoutC( 3, "=========================  STAT  ============================" );
   anyoutC( 3, " ");
   anyoutC( 3, "     -plots the results on screen in user supplied format (FORMAT=).");
   anyoutC( 3, "     -writes data to COLUMNS in a TABLE (TABNAME=, TABAPPEND=).");
   anyoutC( 3, " ");
   anyoutC( 3, "=======================================================================" );

   setfblank_c( &Fblank );
   setdblank_c( &Dblank );
   fmake( Setin, STRLEN );
   fmake( Formstr, KEYLEN );
   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   dfault  = NONE;
   subdim  = 0;
   showdev = 3;
   Key     = tofchar("INSET=");
   Mes     = tofchar("Give input set (, subsets):");
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
   naxis   = (int) (setdim - subdim);
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
   boxcount = 0;
   boxopt   = 0;
   showdev  = 16;
   dfault   = REQUEST;
   entire   = YES;                /* Essential if subdim == 0 */
   Mes      = tofchar(" ");
   do {
      (void) sprintf( message, "BOX%d=", boxcount+1 );
      Key = tofchar( message );
      gdsbox_c( blo, bhi, Setin, subin, &dfault,
                Key, Mes, &showdev, &boxopt );
      entire = YES;
      for (i = 0; i < subdim; i++) {
         entire = ( entire && (blo[i] == flo[i]) && (bhi[i] == fhi[i]) );
      }
      if (entire && (boxcount == 0)) {
         for (i = 0; i < subdim; i++) {
            ablo[boxcount][i] = blo[i];
            abhi[boxcount][i] = bhi[i];
         }
         boxstring( boxstr[boxcount], blo, bhi );
         boxcount++;
      }
      if (!entire) {
         for (i = 0; i < subdim; i++) {
            ablo[boxcount][i] = blo[i];
            abhi[boxcount][i] = bhi[i];
         }
         boxstring( boxstr[boxcount], blo, bhi );
         boxcount++;
      }
   } while (!entire);


   /*---------------------------------------------*/
   /* Is output to the screen of the result       */
   /* table wanted?                               */
   /*---------------------------------------------*/

   displaytab = toflog(YES);
   nitems = 1;
   dfault = REQUEST;
   Key    = tofchar("TOSCREEN=");
   Mes    = tofchar("Output to screen?       [Y]/N");
   r1     = userlog_c( &displaytab, &nitems, &dfault, Key, Mes );
   displaytab = tobool( displaytab );

   /*---------------------------------------------*/
   /* Is output of the totals to the screen       */
   /* wanted?                                     */
   /*---------------------------------------------*/

   if (displaytab) {
      displaytot = toflog(YES);
      Mes        = tofchar("Include table of totals?      [Y]/N");
   } else {
      displaytot = toflog(NO);
      Mes        = tofchar("Include table of totals?      Y/[N]");
   }
   nitems = 1;
   dfault = HIDDEN;
   Key    = tofchar("DISPTOT=");
   r1     = userlog_c( &displaytot, &nitems, &dfault, Key, Mes );
   displaytot = tobool( displaytot );

   fp = NULL;
   fp = fopenC( filename );

   /*--------------------------------------------------*/
   /* Create array of strings and floats with physical */
   /* coordinates for all subsets                      */
   /*--------------------------------------------------*/

   firstcolwidth = 0;
   first     = YES;
   minphys   = maxphys = Fblank;
   for (sub = 0; sub < nsubs; sub++) {
      int    len;
      float  fval;
      len = subsetstring( Setin, subin[sub], axnum, substr[sub], &phys );
      if (phys == Dblank) {
         fval = Fblank;
      } else {
         fval = (float) phys;
         if (first) {
            minphys = fval;
            maxphys = fval;
            first = NO;
         } else {
            if (fval > maxphys) maxphys = fval;
            if (fval < minphys) minphys = fval;
         }
      }
      physcoord[sub] = fval;
      if (len > firstcolwidth) firstcolwidth = len;
   }

   /*-------------------------------------------------*/
   /* Get names of repeat axis (if there is only one) */
   /* and obtain the units of the map values          */
   /*-------------------------------------------------*/

   for (i = subdim, j = 0; i < setdim; i++, j++) 
   {
      fint  colev;
      fchar Axname, Axunit;
      fmake( Axname, STRLEN );
      fmake( Axunit, STRLEN );
      strcpy( axname[j], "?" );
      strcpy( axunit[j], "?" );
      r1 = axcoord_c( Setin, &axnum[i], Axname, Axunit, &colev );
      if (r1 == 0) {
         char  *cptr;
         cptr = strtok( Axname.a, " -" );
         if (cptr != NULL) {
            (void) sprintf( axname[j], "%s", cptr );
         }
         (void) sprintf( axunit[j], "%.*s", nelc_c( Axunit ), Axunit.a );
      }
   }
   r1 = 0;
   fmake( Mapunits, KEYLEN );
   Mapunits.l = KEYLEN;
   Mapunits.a = mapunits;
   gdsd_rchar_c( Setin, tofchar("BUNIT"), &setlevel, Mapunits, &r1 );
   if (r1 < 0) {
      anyoutC(1, "Could not find units (BUNIT) in header" );
      strcpy( mapunits, "?" );
   } else {
      (void) sprintf( mapunits, "%.*s", nelc_c( Mapunits ), Mapunits.a );
   }

   /*--------------------------*/
   /* Create table (or append) */
   /*--------------------------*/

   fmake( Tname, ITEMLEN );

   /* Ask name of table: */

   str2char("STAT", Tname);
   dfault  = HIDDEN;
   nitems  = 1;
   Key     = tofchar("TABNAME=");
   Mes     = tofchar("Give name of table to store results:    [STAT]");
   r1      = userchar_c( Tname, &nitems, &dfault, Key, Mes );

   /* Append to existing columns? */
   dfault  = HIDDEN;
   nitems  = 1;
   Key     = tofchar("TABAPPEND=");
   Mes     = tofchar("Append to existing columns?    Y/[N]");
   append  = toflog( NO );
   r1      = userlog_c( &append, &nitems, &dfault, Key, Mes );
   append  = tobool( append );

   clen    = sprintf( message, "%d", boxcount );
   clen    = MYMIN( nelc_c(Tname), 8-clen );


   /*--------------------------------------------------*/
   /* Start statistics loop over all boxes and subsets */
   /*--------------------------------------------------*/

   for (i = 0; i < 2*(NUMDBLE+NUMINT); i++) {         /* Initialize minmax array (with blanks) */
      minmax[i] = Fblank;
   }


   for (box = 0; box < boxcount; box++) {
      if (boxcount > 1) {
         sprintf( message, "%.*s%d", clen, Tname.a, box+1 );
         str2char( message, Tname );
      }
      (void) inittable( Setin, setlevel, Tname, box,
                        mapunits, naxis, append, &nrows );
      if (append) {                          /* Start position in (GDS-table) column */
         rowcount = nrows + 1;
      } else {
         rowcount = 1;
      }

      if (displaytab || fp) {
         tabwidth = screenheader( Setin, subin[0], box, &firstcolwidth, displaytab, fp );
      }
      for (sub = 0; sub < nsubs; sub++) {
         showsub( Setin, subin[sub], axnum, boxstr[box] );
         statistics( Setin, subin[sub], ablo[box], abhi[box],
                     &sum[box][sub], &mean[box][sub], &rms[box][sub],
                     &minval[box][sub], &maxval[box][sub], &valids[box][sub],
                     &nblanks[box][sub] );

         setminmax( sum[box][sub], mean[box][sub], rms[box][sub], minval[box][sub],
                    maxval[box][sub], valids[box][sub], nblanks[box][sub], minmax );

         filltab( Setin, subin[sub], Tname, rowcount, box, naxis,
                  sum[box][sub], mean[box][sub], rms[box][sub], minval[box][sub],
                  maxval[box][sub], valids[box][sub], nblanks[box][sub] );
         rowcount++;
         if (displaytab || fp) {
            displaystr( firstcolwidth, box, sub, Formstr, displaytab, fp );
         }
      }
      if (displaytab || fp) border( 3, tabwidth );
      /* If there are more than 2 subsets, do statistics on results */
      if ( (nsubs > 2) && (displaytot) ) {
         anyoutC( 3, "Statistics on columns:" );
         if (!(displaytab)) {
            firstcolwidth = 12;
            tabwidth = screenheader( Setin, subin[0], box, &firstcolwidth, displaytab, fp );
         }
         statstat( box, nsubs, firstcolwidth, Formstr );
         anyoutC( 3, " " );
      }
   }



   /* Plot the arrays */

   fmake( Setstr, STRLEN );
   dfault = HIDDEN;
   r1     = usertext_c( Setstr, &dfault, tofchar("INSET="), tofchar(" ") );
   option = plotmenu();
   if (option == 0) {
      finis_c();
      return(EXIT_SUCCESS);   /* Dummy return */
   }
   initplot();
   do {
      char     toptitle[STRLEN+20];      /* Setstr plus some text */
      char     xtitle[STRLEN], ytitle[STRLEN];

      (void) sprintf( xtitle, "%s (%s)", axname[0], axunit[0] );
      for (box = 0; box < boxcount; box++) {
         switch (option) {
            case 1:
               dbletofloat( sum[box], Yvals, nsubs );
               (void) sprintf( toptitle, "Sum in %.*s", nelc_c(Setstr), Setstr.a );
               (void) sprintf( ytitle, "Sum (%s)", mapunits );
               break;
            case 2:
               dbletofloat( mean[box], Yvals, nsubs );
               (void) sprintf( toptitle, "Mean in %.*s", nelc_c(Setstr), Setstr.a );
               (void) sprintf( ytitle, "Mean (%s)", mapunits );
               break;
            case 3:
               dbletofloat( rms[box], Yvals, nsubs );
               (void) sprintf( toptitle, "Rms in %.*s", nelc_c(Setstr), Setstr.a );
               (void) sprintf( ytitle, "Rms (%s)", mapunits );
               break;
            case 4:
               dbletofloat( minval[box], Yvals, nsubs );
               (void) sprintf( toptitle, "Min. values in %.*s", nelc_c(Setstr), Setstr.a );
               (void) sprintf( ytitle, "Min. value (%s)", mapunits );
               break;
            case 5:
               dbletofloat( maxval[box], Yvals, nsubs );
               (void) sprintf( toptitle, "Max. values in %.*s", nelc_c(Setstr), Setstr.a );
               (void) sprintf( ytitle, "Max. value (%s)", mapunits );
               break;
            case 6:
               inttofloat( valids[box], Yvals, nsubs );
               (void) sprintf( toptitle, "Non blank values in %.*s", nelc_c(Setstr), Setstr.a );
               (void) sprintf( ytitle, "Number" );
               break;
            case 7:
               inttofloat( nblanks[box], Yvals, nsubs );
               (void) sprintf( toptitle, "Blank values in %.*s", nelc_c(Setstr), Setstr.a );
               (void) sprintf( ytitle, "Number" );
               break;
         }
         if ((option >=1) && (option <= (MAXOPTIONS-2))) {
            float   Xmax, Xmin, Ymax, Ymin;
            int     indx = (option-1) * 2;
            if (box == 0) {
               if ( (setdim-subdim) > 1) {
                  anyoutC( 1, "WARNING: Plot is repeated for one physical axis" );
               }
               if (setdim == subdim) strcpy( xtitle, "Set level" );
               Xmin = minphys; Ymin = minmax[indx];
               Xmax = maxphys; Ymax = minmax[indx+1];
               drawbox( &Xmin, &Ymin, &Xmax, &Ymax,
                        xtitle, ytitle, toptitle );
            }
            plotboxnr( box, Xmin, Ymin, Xmax, Ymax );
            plotarray( box, nsubs, physcoord, Yvals );
         }
      }
      option = plotmenu();
      if ((option == 8) || (option == 0)) {
         fint color = 1;
         pgsci_c( &color );
         putid();
      }
      if (option == 8) {
         pgend_c();
         cancel_c( tofchar("GRDEVICE=") );
         initplot();
      }
   } while (option != 0);

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   pgend_c();
   if (fp)
      fclose( fp );
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
