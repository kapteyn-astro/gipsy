/*
                            COPYRIGHT (c) 1995
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             profglob.dc1

Program:       PROFGLOB

Purpose:       Interactive estimation of width of global profile.
               (No fitting involed)

Category:      ANALYSIS, PLOTTING, PROFILES

File:          profglob.c

Author:        M.G.R. Vogelaar

Keywords:

   GRDEVICE=   Plot device:              [list of all graphics devices]
               Destination of plot, Screen or Hardcopy.


   XARRAY=     Give values along X:

               Input from file, table, image or recall file.
               The X values will be sorted by the program in
               increasing order.
               SEE EXAMPLES !


   YARRAY=     Give # values for Y:

               # is the number of values entered with XARRAY=
               SEE EXAMPLES !


   EYARRAY=    Give one or more values for errors in Y:     [no errors]

               Pressing carriage return will exclude the possibility
               to plot error bars and the calculation of errors in
               the estimated profile width. Otherwise you can read
               error data from file, table or image. If you enter
               less than the number in XARRAY= then the remaining
               values are copied from the last one entered.
               Negative values are converted to positive values.
               SEE EXAMPLES !


   WLEVEL=     Give W-level (0-100):                               [20]

               Max. numbers that you are allowed to enter is 2.
               This value sets the height at which the profile width
               is measured at positions using maxima entered by
               XPOS= or entered with the interactive cursor.
               The heights are with respect to ZEROLEVEL=


   ZEROLEVEL=  Give zero level:                                   [0.0]

               W-levels are taken with respect to this value.


   XUNITS=     Give units of X axis:                                [?]

               Input is a text that will be plotted along the X-axis.


   YUNITS=     Give units of Y axis:                                [?]

               Input is a text that will be plotted along the Y-axis.
               The program creates a table where, besides the estimated
               width, also the profile area is listed. The units of
               the area are XUNITS= * YUNITS=


   FILENAME=   Write results to file with name:          [do not store]

               The results of the program will be listed in a table
               in the log file. This table can also be written to
               an ASCII file on disk. The file name will be FILENAME=


   OVERWRITE=  File exists, overwrite?                            Y/[N]

               If the file in FILENAME= already exists, you are
               prompted with this keyword. If you press 'N', then
               you are re-prompted with FILENAME= to enter a new name.


   XPOS=       Give x position(s) of maximum:              [calculated]

               Maximum number of X values is 2.
               If you know the positions of a maximum (or the maxima)
               beforehand, it is possible to enter it here.


   Q=          Give smoothing parameter:                   [calculated]

               The smoothing parameter is used in calculating the
               second derivative of the profile when the program
               searches gaussian components in a profile. The
               estimates for the positions of maxima is used to
               calculate default values for XPOS= if the profile
               has more than one peak.


   NPTS=       Give number of points to calculate pos. of max:      [5]

               Allow program to find a better estimate of the
               maximum (or the maxima) by examining NPTS= neighbour
               points to the left and NPTS= points to the right.


   INTERACT=   Determine positions of maxima manually?            Y/[N]

               If you are not satisfied with the defaults of XPOS=
               as suggested by the program, enter new values with
               the interactive cursor. Fix a position with the
               LEFT mouse button. Redraw and reset with the RIGHT
               button and continue with keyboard key 'Q' or 'q'.



               PLOTTING:
               ========

** PGMOSAIC=   View surface sub divisions in x,y:                 [1,1]
               View surface can contain a number of plots in
               in X and Y direction (mosaic). Default is 1 plot in
               both X- and Y direction.


** PGPAPER=    Give width(cm), aspect ratio:               [calc, calc]
               Aspect ratio is height/width.


** PGBOX=      Corners of box Xl,Yl,Xh,Yh:                 [calculated]
               It is possible to overrule the calculated
               PGPLOT box size with PGBOX=. The coordinates (x,y) of
               the lower point are given first.


** PGCOLOR=    Give color 1..15:                                    [1]
               See description for the available colors.


** PGWIDTH=    Give line width 1..21:                               [2]


** PGHEIGHT=   Give character height:                             [1.0]
               Character height as a multiple of the default height.


** PGFONT=     Give font 1..4:                                      [2]
               1  single stroke 'normal' font
               2  roman font
               3  italic font
               4  script font


Examples:      INPUT OF ARRAY DATA
               ===================

               The input of the array data follows the rules of
               input of floating point numbers. Probably you want to
               use one of the database or file functions 'table',
               'image', 'file'.

               FILE
               ====
               Syntax for reading from file:

                       keyword=file(filename,column,rows)

               and the syntax for 'rows' is as for recall files.


               ex.1:  XARRAY=file(profile.txt,2,3:20)

               reads from ASCII file profile.txt the second column.
               It starts reading at row 1 and it wil read until
               line 20 is read.

               ex.2:  XARRAY=file(profile.txt,2,1:)

               reads from ASCII file profile.txt the second column.
               It starts reading at row 1 and it will read until
               the end of that column.


               IMAGE
               =====
               Syntax for reading image data:

                           keyword=image(set, box)

               'set' is the set/subset specification as known from
               the INSET= keywords. 'box' sets the limits as in the
               BOX= keywords. Suppose we have a 2-dim RA/DEC GIPSY set
               called 'profset', then:

               ex.3:  YARRAY=image(profset dec 0, -14 15)

               reads profile data in the RA direction at DEC=0.
               It starts reading at RA=-14 and it stops reading
               image data after RA=15


               TABLE
               =====
               Syntax for reading data from a table:

                      keyword=table(set, tab, col, rows)

               'set' is the set/subset specification as known from
               the INSET= keywords. 'tab' is the name og the GDS table,
               'col' is the name of a column in that table and rows
               indicate the rows in that column.
               Set 'profset' has a table called 'tab1'. This table
               has two columns named 'X' and 'Y':

               ex.4:  XARRAY=table(profset,tab1,X,1:39)

               reads row 1 to 39 from column 'X' from table 'tab1'
               in set 'profset'.


Description:   PROFGLOB is a program that can assist you in estimating the
               width of a global profile. The profile will be plotted
               and defaults for the position of a maximum (or the maxima)
               will be calculated and plotted. If you are not satisfied
               with these defaults then you can enter positions manually
               using the interactive cursor (INTERACT=Y). At the so
               called W-level, a horizontal line will intersect the XPOS=
               vertical line (that indicates the position of the maximum)
               and the profile. The intersections with the profile
               determine the estimated width of the profile.
               Also the estimated errors in the width will be plotted.
               Results are listed in a table in the GIPSY log file or in
               an ASCII file on disk (FILENAME=).

               Here is a recipe how the errors are calculated:

               The horizontal line through WLEVEL= and XPOS= crosses
               the profile at certain X. Find the nearest (in X) data
               point. Use its error to construct two new horizontal
               lines (through Y+/-Yerr) that crosses the profile at two
               new values of X. Determine for each position the interval
               to which it belongs and get the left (or) right end point
               of that interval. Repeat it for the other X value. These
               points have also a corresponding error. If you connect
               the upper and lower Y values of these two points, two
               lines will emerge. The intersection of these two lines
               with the WLEVEL= line gives two X values of which the
               difference is a measure of the wanted error. If these
               actions are applied to the left and right side of the
               profile, then you get two error widths, say ew1 and ew2.
               The estimated error in the width will be:
               0.5*SQRT(ew1^2+ew2^2).

               The table that is created looks like:

               W-level |      width +/-      error |   velocity |       area ..|
               ===============================================================
                    20 |     107.96 +/-     4.1269 |    717.977 |    4433.36 ..|

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

Example:

Updates:       May 30, 1995: VOG, Document created.

#<
*/

/*  profglob.c: include files     */

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
#include    "status.h"       /* Display additional information in the "RUNNING" status display */
#include    "gauest.h"       /* Searches for gaussian components in a profile */

/* User input routines */

#include    "userfio.h"      /* Easy-C companions for user interface routines.*/
#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "usercharu.h"
#include    "userchar.h"
#include    "dcdreal.h"
#include    "dcderrstr.h"    /* Obtain an error message, given a DECODExxx error code.*/
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
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/


/* PGPLOT includes */

#include    "pgplot.h"       /* All PGPLOT includes. */



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

/* Malloc version of 'fmake. Strings allocated with'  */
/* finit, must be freed with free( fc.a ) */
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

#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     1024       /* Max. allowed subsets */
#define STARTBUF       4096       /* Buffer size for I/O */
#define STRLEN         256        /* Max length of strings */
#define FILENAMELEN    256        /* Max length of file names */
#define FITSLEN        20         /* Max length of header items etc.*/
#define BUFFEROVERFLOW -23
#define FILLED_DOT     17
#define NONE           0          /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")


typedef struct     /* Struct used in 'qsort' */
{
   float X;
   float Y;
   float Z;
} allarrays;



/* PGPLOT variables */

const  fint     background  =  0;      /* Color definitions for PGPLOT. */
static fint     foreground  =  1;      /* Black if background is white. */
static fint     red         =  2;
static fint     green       =  3;
const  fint     blue        =  4;
static fint     cyan        =  5;
const  fint     magenta     =  6;
static fint     yellow      =  7;
static fint     orange      =  8;
const  fint     greenyellow =  9;
const  fint     greencyan   = 10;
const  fint     bluecyan    = 11;
const  fint     bluemagenta = 12;
const  fint     redmagenta  = 13;
const  fint     darkgray    = 14;
const  fint     lightgray   = 15;
static fint     symbol      = FILLED_DOT;


/* Miscellaneous global variables */

static float    blank;              /* Global value for BLANK. */
static char     message[STRLEN];    /* All purpose character buffer. */



static   void  plmove( float x, float y )
/*---------------------------------------*/
/* Alternative pgmove                    */
/*---------------------------------------*/
{
    pgmove_c( &x, &y );
}



static   void  pldraw( float x, float y )
/*---------------------------------------*/
/* Alternative pgdraw                    */
/*---------------------------------------*/
{
    pgdraw_c( &x, &y );
}



static void plpoint( float x, float y,
                     fint symbol )
/*---------------------------------------*/
/* Alternative pgpt                      */
/*---------------------------------------*/
{
   fint    ndata = 1;
   pgpt_c( &ndata, &x, &y, &symbol );
}



static void errorC( int  level,
                    char *str )
/*-----------------------------------------------------------*/
/* PURPOSE: User error handling routine.                     */
/* The C version of 'error'.                                 */
/* 1 = Warning, 2 = Minor error, 3 = Serious error,          */
/* 4 = Fatal error                                           */
/*-----------------------------------------------------------*/
{
   fint flev = (fint) level;
   error_c( &flev, tofchar( str ) );
}



static void swapf( float  *x1,
                   float  *x2 )
/*-----------------------------------------------------------*/
/* PURPOSE: Swap contents of two floats.                     */
/*-----------------------------------------------------------*/
{
   float swap = *x1;
   *x1 = *x2;
   *x2 = swap;
}



static void swapi( int  *x1,
                   int  *x2 )
/*-----------------------------------------------------------*/
/* PURPOSE: Swap contents of two integers.                   */
/*-----------------------------------------------------------*/
{
   int  swap = *x1;
   *x1 = *x2;
   *x2 = swap;
}



void initplot( void )
/*------------------------------------------------------------*/
/* PURPOSE: Initialze PGPLOT.                                 */
/* Initialize plot software. Set viewport and output dims.    */
/* If output device is a printer, ask user for line width.    */
/*------------------------------------------------------------*/
{
   fint   unit;            /* Ignored by pgbeg, use unit=0. */
   fchar  Devspec;         /* Device specification. */
   fint   nxysub[2];       /* Number of subdivisions on 1 page. */
   float  width;           /* Width of output on paper */
   float  aspect;          /* Aspect ratio of output on paper */
   fint   nitems, dfault;
   fint   r;
   bool   paging;          /* Disable PGPLOT's NEXTPAGE keyword. */
   float  paper[2];
   float  xl, xr, yb, yt;  /* Edges of the viewport. */


   /*--------------------------------------------------*/
   /* Initialize PGPLOT with a call to 'pgbeg'.        */
   /* There are 4 arguments for PGBEG:                 */
   /* UNIT, this argument is ignored by PGBEG (use     */
   /*       zero).                                     */
   /* FILE, If this argument is a question mark PGBEG  */
   /*       will prompt the user to supply a string.   */
   /* NXSUB, # sub divisions of the view surface in X. */
   /* NYSUB, # sub divisions of the view surface in Y. */
   /*--------------------------------------------------*/

   nxysub[1] = nxysub[0] = 1;           /* No sub divisions is default */
   nitems = 2;
   dfault = HIDDEN;
   r = userint_c( nxysub,
                  &nitems,
                  &dfault,
                  tofchar("PGMOSAIC="),
                  tofchar("View surface sub divisions in x,y:   [1,1]") );

   unit = 0;
   Devspec = tofchar("?");
   r = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );
   if (r != 1)                         /* Fatal error */
      errorC( 4, "Cannot open output device" );

   /* No PGPLOT's NEXTPAGE= keyword */
   paging = toflog( NO );
   pgask_c( &paging );

   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */
   nitems = 2;
   dfault = HIDDEN;
   paper[0] = 0.0;
   paper[1] = 1.0;
   r = userreal_c( paper,
                   &nitems,
                   &dfault,
                   tofchar("PGPAPER="),
                   tofchar("Give width(cm), aspect ratio: [calculated]") );
   if (r > 0)
   {
      /* If width = 0.0 then the program will select the largest view surface */
      width  = paper[0] / 2.54;         /* Convert width to inches. */
      aspect = paper[1];
      pgpap_c( &width, &aspect );
   }

   /* Set viewport */
   xl = 0.2; xr = 0.95;
   yb = 0.1; yt = 0.9;
   pgsvp_c( &xl, &xr, &yb, &yt );
}



void drawbox( float  Xmin,
              float  Ymin,
              float  Xmax,
              float  Ymax,
              char  *xtitle,
              char  *ytitle,
              char  *ttitle )
/*------------------------------------------------------------*/
/* PURPOSE: Draw frame with labels for input box.             */
/* Draw box and labels. Take special care for the y labels    */
/* and title. Colors are defined globally. Xmin etc are the   */
/* corners of the box in world coordinates.                   */
/*------------------------------------------------------------*/
{
   float  charsize = 1.0;
   float  delta;
   fint   lwidth;
   fint   r;
   fint   nitems;
   fint   dfault;
   float  pg_box[4];                 /* Corners of draw box. */
   fint   color;
   fint   font;
   fint   nxsub, nysub;
   float  xtick, ytick;
   char   message[80];


   pgpage_c();                       /* Advance to new page. */

   /* Increase the size of the box a little */
   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0)
      delta = 1.0;
   Xmin -= delta;
   Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0)
      delta = 1.0;
   Ymin -= delta;
   Ymax += delta;
   pg_box[0] = Xmin;                /* Get size from user input */
   pg_box[1] = Ymin;
   pg_box[2] = Xmax;
   pg_box[3] = Ymax;
   nitems = 4;
   dfault = HIDDEN;
   sprintf( message, "Corners of box Xl,Yl, Xh,Yh:  [%f,%f,%f,%f]", Xmin,Ymin,Xmax,Ymax );
   r = userreal_c( pg_box,
                    &nitems,
                    &dfault,
                    tofchar("PGBOX="),
                    tofchar( message ) );
   Xmin = pg_box[0];
   Ymin = pg_box[1];
   Xmax = pg_box[2];
   Ymax = pg_box[3];
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );    /* Set the window */

   color = 1;
   nitems = 1;
   dfault = HIDDEN;
   r = userint_c( &color,
                  &nitems,
                  &dfault,
                  tofchar("PGCOLOR="),
                  tofchar("Give color 1..15:        [1]") );
   if (color > 15)
      color = 15;
   if (color < 1 )
      color =  1;
   pgsci_c( &color );

   lwidth = 2;
   nitems = 1;
   dfault = HIDDEN;
   r = userint_c( &lwidth,
                  &nitems,
                  &dfault,
                  tofchar("PGWIDTH="),
                  tofchar("Give line width 1..21:        [2]") );
   if (lwidth > 21)
      lwidth = 21;
   if (lwidth < 1 )
      lwidth =  1;
   pgslw_c( &lwidth );                  /* Set line width. */

   charsize = 1.0;
   nitems = 1;
   dfault = HIDDEN;
   r = userreal_c( &charsize,
                   &nitems,
                   &dfault,
                   tofchar("PGHEIGHT="),
                   tofchar("Give character height:     [1.0]") );
   pgsch_c( &charsize );               /* Character height. */

   font   = 2;
   nitems = 1;
   dfault = HIDDEN;
   r = userint_c( &font,
                  &nitems,
                  &dfault,
                  tofchar("PGFONT="),
                  tofchar("Give font 1..4:        [2]") );
   if (font > 4)
      font = 4;
   if (font < 1)
      font = 1;
   pgscf_c( &font );                   /* Set font. */

   /*----------------------------------------------------------------*/
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
   /*----------------------------------------------------------------*/
   xtick = ytick = 0.0;
   nxsub = nysub = 0;
   pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
            tofchar("BCNSTV"), &ytick, &nysub );

   /* Plot the titles */
   pglab_c( tofchar(xtitle), tofchar(ytitle), tofchar(ttitle) );
}



static void makeplot( float  Xmin,
                      float  Ymin,
                      float  Xmax,
                      float  Ymax,
                      char   *Xtitle,
                      char   *Ytitle,
                      char   *Toptitle,
                      float  *Xarray,
                      float  *Yarray,
                      float  *EYarray,
                      float  zerolevel,
                      fint   ndata,
                      bool   errbar )
/*------------------------------------------------------------*/
/* PURPOSE: Plot frame, data points, error bars & zero level. */
/*------------------------------------------------------------*/
{
   pgsci_c( &foreground );
   drawbox( Xmin, Ymin, Xmax, Ymax,
            Xtitle, Ytitle, Toptitle );

   pgpt_c( &ndata, Xarray, Yarray, &symbol );
   if (errbar)
   {
      fint   dir;
      float  termlen = 3.0;

      dir = 2;
      pgerrb_c( &dir, &ndata, Xarray, Yarray, EYarray, &termlen );
      dir = 4;
      pgerrb_c( &dir, &ndata, Xarray, Yarray, EYarray, &termlen );
   }
   pgsci_c( &cyan );
   plmove( Xmin, zerolevel );                   /* The zero base line */
   pldraw( Xmax, zerolevel );
   pgsci_c( &green );
   pgline_c( &ndata, Xarray, Yarray );

}



static int hascursor( void )
/*------------------------------------------------------------*/
/* PURPOSE: Check whether device has interactive cursor.      */
/*------------------------------------------------------------*/
{
   fchar  Infostr;
   fint   flen = 20;

   fmake( Infostr, 20 );
   pgqinf_c( tofchar("CURSOR"), Infostr, &flen );
   if (toupper(Infostr.a[0]) == 'Y')
      return( 1 );
   return( 0 );
}



static void getminmax( float  *X,
                       int    len,
                       float  *Xmin,
                       float  *Xmax )
/*------------------------------------------------------------*/
/* PURPOSE: Return min/max of an array 'X'.                   */
/*------------------------------------------------------------*/
{
   int    i;

   *Xmin = *Xmax = blank;
   for (i = 0; i < len; i++)
   {
      if (X[i] != blank)
      {
         if (*Xmin == blank)
            *Xmin = X[i];
         else if (X[i] < *Xmin)
               *Xmin = X[i];
         if (*Xmax == blank)
            *Xmax = X[i];
         else if (X[i] > *Xmax)
               *Xmax = X[i];
      }
   }
}



static char interactivecursor( float *xx,
                               float *yy )
/*------------------------------------------------------------*/
/* PURPOSE: Go into interactive mode until 'Q' is pressed or  */
/*          the left or right mouse button. Return the key-   */
/*          board status.                                     */
/*------------------------------------------------------------*/
{
   fint    r;
   float   x = *xx, y = *yy;
   fchar   Kar;
   char    ch;
   int     exit;

   fmake( Kar, 1 );
   do
   {
      r = pgcurs_c( &x, &y, Kar );
      ch = Kar.a[0];
      exit = ( toupper(ch) == 'Q' || ch == '1' || ch == '3' );
   }
   while (!exit);
   *xx = x;
   *yy = y;
   return( toupper(ch) );
}



static int findindex( float   X,
                      float  *Xarray,
                      int     ndata )
/*------------------------------------------------------------*/
/* PURPOSE: */
/*------------------------------------------------------------*/
{
   int   i = 0, j = 0;
   int   found = NO;


   while (!found && i < ndata - 1)
   {
      found = ( X >= Xarray[i] && X < Xarray[i+1] );
      if (found)
         j = i;
      i++;
   }
   if (!found)
      return( -1 );
   return( j );
}



static float getY( float   X,
                   float  *Xarray,
                   float  *Yarray,
                   int     ndata,
                   int     interval )
/*------------------------------------------------------------*/
/* PURPOSE: Find the linear interpolated value y at given x   */
/*          in given 'interval'.                              */
/*------------------------------------------------------------*/
{
   float    deltaX;
   float    a, b;


   if (interval < 0)
      return( blank );

   deltaX = Xarray[interval] - Xarray[interval+1];
   if (deltaX == 0.0)
      return( blank );
   else
   {
      a = (Yarray[interval] - Yarray[interval+1]) / deltaX;
      b = Yarray[interval] - a * Xarray[interval];
   }
   return( a * X + b );
}



static float getX( char   mode,
                   int    indx,
                   float  y,
                   float  *Xarray,
                   float  *Yarray,
                   int    ndata )
/*------------------------------------------------------------*/
/* PURPOSE: For a given y, find the corresponding (interpola- */
/*          ted x.                                            */
/* For a given y it is possible to look for the corresponding */
/* value of x to left or to the right ('mode') of a certain   */
/* index ('indx'). First find an interval so that             */
/* Yi-1 <= y < Yi. In this interval find x so that y=ax+b     */
/* where a and b are calculated using the end points of that  */
/* interval.                                                  */
/*------------------------------------------------------------*/
{
   int     found = NO;
   int     i;
   float   a = 0.0, b;


   i = indx;
   if (mode == 'L')
   {
      while (!found && i > 1)
      {
         if (y <= Yarray[i] && y > Yarray[i-1])
         {
            a = (Yarray[i] - Yarray[i-1]) / (Xarray[i] - Xarray[i-1]);
            found = YES;
         }
         else
            i--;
      }
   }
   else
   {
      while (!found && i < ndata-1)
      {
         if (y <= Yarray[i] && y > Yarray[i+1])
         {
            found = YES;
            a = (Yarray[i] - Yarray[i+1]) / (Xarray[i] - Xarray[i+1]);
         }
         else
            i++;
      }
   }
   if (found)
   {
      b = Yarray[i] - a * Xarray[i];
      return( (y-b) / a );
   }

   return( blank );
}




static float getarea( float *Xarray,
                      float *Yarray,
                      float *EYarray,
                      int    ndata,
                      bool   errbar,
                      float *Aerr )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate area under profile.                     */
/* The width of a bin is calculated as half the distance to   */
/* the left neighbour plus half the distance to the right     */
/* neighbour. Width times the Y value of that bin is tha area */
/* of that bin. Repeat action for all bins.                   */
/*------------------------------------------------------------*/
{
   int   i;
   float sum = 0.0;
   float errs = 0.0;

   for (i = 0; i < ndata - 1; i++)
   {
      if (Yarray[i] != blank)
      {
         float   dx;
         if (i-1 > 0)
            dx = ABS(Xarray[i]-Xarray[i-1]) / 2.0;
         else
            dx = ABS(Xarray[i+1]-Xarray[i]) / 2.0;
         if (i+1 < ndata)
            dx += ABS(Xarray[i+1]-Xarray[i]) / 2.0;
         else
            dx += ABS(Xarray[i]-Xarray[i-1]) / 2.0;
         sum += Yarray[i] * dx;
         if (errbar)
            errs += dx * EYarray[i] * EYarray[i];
      }
      else
      {
         if (Yarray[i] == blank )
            anyoutf( 1, "Profile contains BLANK at position index %d", i );
      }
   }
   *Aerr = sqrt( errs );
   return( sum );
}



static void fill( float  *X,
                  float  *Yh,
                  float  *Yl,
                  float  *Xarray,
                  float  *Yarray,
                  float  *EYarray,
                  int     j )
/*------------------------------------------------------------*/
/* PURPOSE: Help routine for function 'geterror'.             */
/*------------------------------------------------------------*/
{
   *X  = Xarray[j];
   *Yh = Yarray[j] + EYarray[j];
   *Yl = Yarray[j] - EYarray[j];
}




static float geterror( char   mode,
                       float  X,
                       float  Wlevel,
                       float *Xarray,
                       float *Yarray,
                       float *EYarray,
                       int    ndata )
/*------------------------------------------------------------*/
/* PURPOSE: Estimate errors for the profile widths.           */
/* The horizontal line at Wn crosses the profile at certain X.*/
/* Find the nearest (in X) data point. Use its errors to      */
/* construct two new horizontal lines that crosses            */
/* the profile at two new values of X. Determine for each     */
/* position the interval to which it belongs and get the left */
/* right end point                                            */
/* of that interval. Repeat it for the other X value.         */
/* These points have also a corresponding error. If you       */
/* connect the upper and lower Y values of these two points,  */
/* two lines will emerge. The intersection of these two lines */
/* with the Wn line gives two X values of which the difference*/
/* is equal to the wanted error.                              */
/*------------------------------------------------------------*/
{
   int     i;
   int     i_store;
   int     found = NO;
   float   Ylow, Yhigh;
   float   X1, X2;
   float   Y1h, Y1l, Y2h, Y2l;


   i = findindex( X, Xarray, ndata );
   if (i < ndata - 1)
   {
      if ( ABS(Xarray[i+1]-X) < ABS(Xarray[i]-X) )
         i++;
   }
   i_store = i;
   Yhigh = Yarray[i] + EYarray[i];
   Ylow  = Yarray[i] - EYarray[i];


   if (mode == 'L')
   {
      i = i_store;
      found = NO;

      while (i > 1 && !found)
      {
         found = (Ylow < Yarray[i] && Ylow >= Yarray[i-1]);
         if (found)
            fill( &X1, &Y1h, &Y1l, Xarray, Yarray, EYarray, i-1 );
         i--;
      }
      if (found)
      {
         found = NO;
         i = i_store;
         while (i < ndata - 1 && !found)
         {
            found = (Yhigh > Yarray[i] && Yhigh <= Yarray[i+1]);
            if (found)
               fill( &X2, &Y2h, &Y2l, Xarray, Yarray, EYarray, i+1 );
            i++;
         }
      }
   }

   if (mode == 'R')
   {
      i = i_store;
      found = NO;

      while (i > 1 && !found)
      {
         found = (Yhigh > Yarray[i] && Yhigh <= Yarray[i-1]);
         if (found)
            fill( &X1, &Y1h, &Y1l, Xarray, Yarray, EYarray, i-1 );
         i--;
      }
      if (found)
      {
         found = NO;
         i = i_store;
         while (i < ndata - 1 && !found)
         {
            found = (Ylow < Yarray[i] && Ylow >= Yarray[i+1]);
            if (found)
               fill( &X2, &Y2h, &Y2l, Xarray, Yarray, EYarray, i+1 );
            i++;
         }
      }
   }

   if (found)
   {
      float Xs1, Xs2;
      float Xerr;
      pgsci_c( &orange );
      Xs1 = X1 + (Wlevel-Y1h)*(X1-X2) / (Y1h-Y2h);
      Xs2 = X1 + (Wlevel-Y1l)*(X1-X2) / (Y1l-Y2l);
      plmove( X1, Y1h );
      pldraw( X2, Y2h );
      plmove( X1, Y1l );
      pldraw( X2, Y2l );
      Xerr = ABS(Xs2 - Xs1);
      if (mode == 'L')
         anyoutf( 16, "Estimated error-width left side: %g", Xerr );
      else
         anyoutf( 16, "Estimated error-width right side: %g", Xerr );
      return( Xerr );
   }

   return( blank );
}



static float getlocalmax( int   ic,
                          float *Xarray,
                          float *Yarray,
                          int   ndata,
                          int   npts )
/*------------------------------------------------------------*/
/* PURPOSE: Find the local maximum in Yarray in a neighbour-  */
/*          hood of Xarray[ic].                               */
/* Allow to examine 'npts' points to the left and 'npts'      */
/* points to the right.                                       */
/*------------------------------------------------------------*/
{
   float    xpos, ymax;
   int      i;

   ymax = Yarray[ic];
   xpos = Xarray[ic];
   for (i = ic; i > MYMAX(0, ic - npts); i--)
   {
      if (Yarray[i] > ymax)
      {
         ymax = Yarray[i];
         xpos = Xarray[i];
      }
   }
   for (i = ic; i < MYMIN(ndata, ic + npts); i++)
   {
      if (Yarray[i] > ymax)
      {
         ymax = Yarray[i];
         xpos = Xarray[i];
      }
   }
   return( xpos );
}



static float getrms( float *amplitudes,
                     fint   ndata )
/*------------------------------------------------------------*/
/* PURPOSE: Get the rms of the data in 'amplitudes'.          */
/* Needed to get an initial estimate for the rms (of the      */
/* noise!!!) in function 'gauest'.                            */
/*------------------------------------------------------------*/
{
   int   i;
   float sum, sumdev;
   float average;

   if (ndata < 2)
      return( 0.0 );

   for (i = 0, sum = 0.0; i < ndata; i++)
      if (amplitudes[i] != blank)
         sum += amplitudes[i];

   average = sum / ndata;
   for (i = 0, sumdev = 0.0; i < ndata; i++)
   {
      if (amplitudes[i] != blank)
      {
         float  dev;
         dev = amplitudes[i] - average;
         dev *= dev;
         sumdev += dev;
      }
   }
   return( (float) sqrt( sumdev/(ndata-1) ) );
}



static void endprogram( float *X,
                        float *Y,
                        float *EY,
                        FILE  *fp )
/*------------------------------------------------------------*/
/* PURPOSE: Free memory, close files and exit program.        */
/*------------------------------------------------------------*/
{
   pgend_c();
   if (X)
      free( X );
   if (Y)
      free( Y );
   if (EY)
      free( EY );
   if (fp != NULL)
      fclose( fp );
   finis_c();
}


static int mycomp( allarrays *xyz1, 
                   allarrays *xyz2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for 'qsort' only!                */
/* If one of the two values is a blank, do nothing. Else,     */
/* sort in increasing order.                                  */
/*------------------------------------------------------------*/
{
   if (xyz1->X == blank || xyz2->X == blank)
      return( 0 );
   if (xyz1->X == xyz2->X)
      return( 0 );
   if (xyz1->X > xyz2->X)
       return( 1 );
   return( -1 );          
}


static void mysort( int ndata, 
                    float *X,
                    float *Y,
                    float *Z )
/*------------------------------------------------------------*/
/* PURPOSE: Sort array X and re-order the others.             */
/* Put corresponding array values in a struct and sort the    */
/* structs with 'qsort'.                                      */
/*------------------------------------------------------------*/
{
   allarrays    *XYZ = NULL;
   int          i;
   
   XYZ = (allarrays *) malloc( sizeof(allarrays) * ndata );
   if (XYZ == NULL)
   {
      errorC( 4, "Cannot allocate work space for sorting" );
      return;
   }
   /* Fill the structure */
   for (i = 0; i < ndata; i++)
   {
      XYZ[i].X = X[i];
      XYZ[i].Y = Y[i];
      XYZ[i].Z = Z[i];      
   }
   /* Sort the structures */
   qsort( XYZ, ndata, sizeof(allarrays), (int(*)())mycomp );
   /* Fill the arrays with sorted structure values */
   for (i = 0; i < ndata; i++)
   {
      X[i] = XYZ[i].X;
      Y[i] = XYZ[i].Y;
      Z[i] = XYZ[i].Z;
   }   
   free( XYZ );
}
                    


MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint     dfault;                      /* Default option for input etc */
   fint     maxnumbers = STARTBUF;
   fint     r;
   fint     nitems;
   fint     ndata;
   float    *Xarray = NULL;
   float    *Yarray = NULL;
   float    *EYarray = NULL;
   float    *workarray = NULL;
   float    Xmin, Xmax;
   float    Ymin, Ymax;
   float    x, y;
   float    zerolevel = 0.0;
   float    X[2], Y[2];
   float    XL[2], YL[2];
   float    wlevel[2];
   float    area = 0.0;
   float    areaerror = 0.0;
   bool     interactive;
   bool     errbar;
   bool     overwrite;
   bool     writetofile;
   bool     agreed;
   char     stat;
   fchar    Filename;
   fchar    Xunits, Yunits;
   fchar    Dummy;
   int      found = 0;
   int      indx[2];
   int      i;
   fint     nlevels;
   fint     npts;
   fint     Q;
   FILE     *fp = NULL;


   init_c();                             /* contact Hermes */
   /* Task identification */
   {
      fchar    Task;                     /* Name of current task */
      fmake( Task, 20 );                 /* Macro 'fmake' must be available */
      myname_c( Task );                  /* Get task name */
      Task.a[nelc_c(Task)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, RELEASE ); /* Show task and version */
   }
   setfblank_c( &blank );

#ifdef NEEDSET
   /*--------------------------------------------------*/
   /* Get the input set. Documentation can be found in */
   /* $gip_sub/gdsinp.dc2                              */
   /*--------------------------------------------------*/
   {
      fmake( Setin, STRLEN );
      dfault  = NONE;
      subdim  = 2;                  /* Allow only 2-dim structures */
      nsubs = gdsinp_c( Setin,      /* Name of input set. */
                        subin,      /* Array containing subsets coordinate words. */
                        &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                        &dfault,    /* Default code as is USERxxx. */
                        KEY_INSET,  /* Keyword prompt. */
                        MES_INSET,  /* Keyword message for the user. */
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
   /* Prepare grid ranges for INSET */
   /*-------------------------------*/
   {
      fint     boxopt = 0;         /* The different options are: */

      dfault = REQUEST;
      gdsbox_c( blo, bhi, Setin, subin, &dfault,
                KEY_BOX, MES_BOX, &showdev, &boxopt );
   }

#endif

   initplot();
   if (!hascursor())
      errorC( 4, "Device has no interactive cursor!" );

   Xarray    = (float *) calloc( maxnumbers, sizeof(float) );
   Yarray    = (float *) calloc( maxnumbers, sizeof(float) );
   EYarray   = (float *) calloc( maxnumbers, sizeof(float) );
   workarray = (float *) calloc( maxnumbers, sizeof(float) );
   if (Xarray    == NULL || Yarray    == NULL  ||
       EYarray   == NULL || workarray == NULL )
      errorC( 4, "Cannot allocate memory for arrays!" );


   /*--------------------------------------------------*/
   /* Repeat the input of data for the Xarray until a  */
   /* correct input is entered AND enough memory is    */
   /* (dynamically) allocated).                        */
   /*--------------------------------------------------*/
   dfault = NONE;
   do
   {
      fint   err = 0;
      agreed = YES;
      fmake( Dummy, 256 );
      (void) usertext_c( Dummy,
                         &dfault,
                         tofchar("XARRAY="),
                         tofchar("Give values along X: ") );
      r = dcdreal_c( Dummy, Xarray, &maxnumbers, &err );
      agreed = (err >= 0);
      if (err == BUFFEROVERFLOW)
      {
         dfault = REQUEST;
         maxnumbers *= 2;
         Xarray  = realloc( Xarray, maxnumbers*sizeof(float) );
         if (Xarray == NULL)
            errorC( 4, "Cannot allocate memory for arrays!" );
      }
      if (!agreed && err != BUFFEROVERFLOW)
      {
         fchar   Errstr;
         fmake( Errstr, 40 );
         dfault = NONE;
         dcderrstr_c( Errstr, &err );
         reject_c( tofchar("XARRAY="), Errstr );
      }
   }
   while ( !agreed );

   Yarray  = (float *) calloc( maxnumbers, sizeof(float) );
   EYarray = (float *) calloc( maxnumbers, sizeof(float) );
   if (Yarray == NULL || EYarray == NULL)
      errorC( 4, "Cannot allocate memory for arrays!" );

   nitems = r;
   dfault = EXACT;
   (void) sprintf( message, "Give %d values for Y: ", nitems );
   r = userreal_c( Yarray,
                   &nitems,
                   &dfault,
                   tofchar("YARRAY="),
                   tofchar(message) );

   ndata = r;


   /*--------------------------------------------------*/
   /* Missing data in EYarray will be copied from the  */
   /* last one.                                        */
   /*--------------------------------------------------*/
   nitems = ndata;
   dfault = REQUEST;
   (void) sprintf( message, "Give one or more values for errors in Y:  [no errors]" );
   r = userreal_c( EYarray,
                   &nitems,
                   &dfault,
                   tofchar("EYARRAY="),
                   tofchar(message) );
   errbar = (r != 0);
   if (errbar && r < ndata)
   {
      for (i = r; i < ndata; i++)
         EYarray[i] = EYarray[i-1];
   }
   for (i = 0; i < ndata; i++)
      EYarray[i] = ABS( EYarray[i] );

 
   /* The Xarray must be in increasing order */
   mysort( ndata, Xarray, Yarray, EYarray );


   dfault = REQUEST;
   nitems = 2;
   wlevel[0] = 20;
   wlevel[1] = 50;
   (void) sprintf( message, "Give W-level (0-100):    [%g]", wlevel[0] );
   nlevels = userreal_c( wlevel,
                         &nitems,
                         &dfault,
                         tofchar("WLEVEL="),
                         tofchar(message) );

   if (nlevels == 0)
   {
      wlevel[0] = 20;
      nlevels = 1;
   }


   dfault = REQUEST;
   nitems = 1;
   zerolevel = 0.0;
   (void) sprintf( message, "Give zero level:    [%g]", zerolevel );
   (void) userreal_c( &zerolevel,
                      &nitems,
                      &dfault,
                      tofchar("ZEROLEVEL="),
                      tofchar(message) );


   fmake( Xunits, 80 );
   dfault = REQUEST;
   r = usertext_c( Xunits,
                   &dfault,
                   tofchar("XUNITS="),
                   tofchar("Give units of X axis:     [?]") );
   if (!r)
      strcpy( Xunits.a, "?" );
   else
      Xunits.a[nelc_c(Xunits)] = '\0';


   fmake( Yunits, 80 );
   dfault = REQUEST;
   r = usertext_c( Yunits,
                   &dfault,
                   tofchar("YUNITS="),
                   tofchar("Give units of Y axis:     [?]") );
   if (!r)
      strcpy( Yunits.a, "?" );
   else
      Yunits.a[nelc_c(Yunits)] = '\0';


   overwrite = YES;
   do
   {
      dfault = REQUEST;
      nitems = 1;
      fmake( Filename, 256 );
      r = userchar_c( Filename,
                      &nitems,
                      &dfault,
                      tofchar("FILENAME="),
                      tofchar("Write results to file with name:     [do not store]" ) );
      writetofile = (r != 0);
      if (writetofile)
      {
         Filename.a[nelc_c(Filename)] = '\0';
         fp = fopen( Filename.a, "r" );
         if (fp != NULL)                        /* File exists */
         {
            overwrite = toflog( NO );
            dfault = REQUEST;
            nitems = 1;
            r = userlog_c( &overwrite,
                           &nitems,
                           &dfault,
                           tofchar("OVERWRITE="),
                           tofchar("File exists, overwrite?    Y/[N]") );
            overwrite = tobool( overwrite );
            if (!overwrite)
            {
               cancel_c( tofchar("OVERWRITE=") );
               reject_c( tofchar("FILENAME="), tofchar("Enter new file name") );
            }
            fclose( fp );
         }
         if (overwrite)
         {
            fp = fopen( Filename.a, "w" );
            if (fp == NULL)
            {
               anyoutf( 1, "Cannot open [%s] for writing!", Filename.a );
               writetofile = NO;
            }
         }
      }
   }
   while (!overwrite);


   getminmax( Xarray, ndata, &Xmin, &Xmax );
   getminmax( Yarray, ndata, &Ymin, &Ymax );

   area = getarea( Xarray, Yarray, EYarray, ndata, errbar, &areaerror );

   makeplot( Xmin, Ymin, Xmax, Ymax,
             Xunits.a,
             Yunits.a,
            "Width of global profile",
             Xarray,
             Yarray,
             EYarray,
             zerolevel,
             ndata,
             errbar );


   X[0] = X[1] = Y[0] = Y[1] = blank;

   dfault = REQUEST;
   nitems = 2;
   r = userreal_c( X,
                   &nitems,
                   &dfault,
                   tofchar("XPOS="),
                   tofchar("Give x position(s) of maximum:   [Calculated]") );


   if (!r)
   /*--------------------------------------------------*/
   /* There are no values entered at XPOS= so we have  */
   /* to calculate reasonable defaults. Let 'gauest'   */
   /* determine one or two positions of maxima. The    */
   /* routine 'gauest', is sensible to the value of Q, */
   /* the smoothing factor. The smoothing parameter is */
   /* used in calculating the second derivative of the */
   /* profile. Estimates are stored as Ampl., centre,  */
   /* disp. They are sorted in amplitude.              */
   /*--------------------------------------------------*/
   {
      float   critampl = 0.0;
      float   critdisp = 0.0;
      fint    maxpars  = 2 * 3;
      float   estimates[2*3];
      float   estrms;


      dfault = REQUEST;
      nitems = 1;
      if (ndata > 2*6+1)
         Q = 6;
      else
         Q = (ndata - 1) / 2;

      (void) sprintf( message,
                     "Give smoothing parameter:   [%d]",
                      Q );
      r = userint_c( &Q,
                     &nitems,
                     &dfault,
                     tofchar("Q="),
                     tofchar(message) );
      if (Q < 1)
         Q = 1;

      estrms = 0.3 * getrms( Yarray, ndata ); /* 0.3 is arbitrary factor */

      r = gauest_c( Yarray,       /* Data points on profile */
                    workarray,    /* Work array (dimension 'ndata') */
                    &ndata,       /* Number of points in profile */
                    estimates,    /* Contains the estimated gaussian parameters */
                    &maxpars,     /* Maximum number of gaussians*pars */
                    &estrms,      /* The  r.m.s. noise level of the profile */
                    &critampl,    /* Gaussians below this amplitude will be discarded. */
                    &critdisp,    /* Same for dispersions */
                    &Q );         /* Smoothing parameter used in calculating */
                                  /* the second derivative */

      anyoutf( 16, "Number of maxima found with 'gauest' is %d.", r );
      r = MYMIN( 2, r );

      for (i = 0; i < r; i++)
      {
         float   delta;
         delta = ABS(Xarray[ndata/2] - Xarray[ndata/2+1]);
         anyoutf( 16, "Estimated position nr. %d is %g",
                  i, Xarray[NINT(estimates[1+i*3])] );
         anyoutf( 16, "Estimated dispersion nr. %d is %g",
                  i, delta*estimates[1+i*3] );
      }

      if (r)
      {
         fint   r2;
         dfault = REQUEST;
         nitems = 1;
         npts   = (int) ( MYMAX(estimates[2], estimates[2+3]) + 1.0 );
         (void) sprintf( message,
                        "Give number of neighbour points to scan for max: [%d]",
                         npts );
         r2 = userint_c( &npts,
                         &nitems,
                         &dfault,
                         tofchar("NPTS="),
                         tofchar(message) );
         if (npts < 1)
            npts = 1;
         if (npts > ndata)
         {
            anyoutf( 1, "More points requested than available data. NPTS= set to %d",
                     ndata/2 );
            npts = ndata / 2;
         }
      }


      for (i = 0; i < r; i++)
      {
         int   ic = NINT(estimates[1+i*3]);
         X[i] = getlocalmax( ic, Xarray, Yarray, ndata, npts );
      }
   }
   else if (r == 1)
      X[1] = X[0];

   if (r)
   {
      plmove( X[0], zerolevel);
      i = findindex( X[0], Xarray, ndata );
      Y[0] = getY( X[0], Xarray, Yarray, ndata, i );
      if (Y[0] != blank)
      {
         pldraw( X[0], Y[0] );
         indx[0] = i;
      }
      if (r == 2)
      {
         plmove( X[1], zerolevel);
         i = findindex( X[1], Xarray, ndata );
         Y[1] = getY( X[1], Xarray, Yarray, ndata, i );
         if (Y[1] != blank)
         {
            pldraw( X[1], Y[1] );
            indx[1] = i;
         }
      }
   }


   if (!r)
   {
      anyoutf( 1, "No default positions found or entered. Switching to interactive mode!");
      interactive = YES;
   }
   else
   {
      nitems = 1;
      dfault = REQUEST;
      interactive = toflog( NO );
      (void) userlog_c( &interactive,
                        &nitems,
                        &dfault,
                        tofchar("INTERACT="),
                        tofchar("Determine positions of maxima manually?    Y/[N]") );
      interactive = tobool( interactive );
   }

   found = r;
   x = (Xmin + Xmax) / 2.0;
   y = (Ymin + Ymax) / 2.0;
   if (interactive)
   {
      X[0] = X[1] = Y[0] = Y[1] = blank;
      x = (Xmin + Xmax) / 2.0;
      y = (Ymin + Ymax) / 2.0;
      makeplot( Xmin, Ymin, Xmax, Ymax,
                Xunits.a,
                Yunits.a,
               "Width of global profile",
                Xarray,
                Yarray,
                EYarray,
                zerolevel,
                ndata,
                errbar );
      found = 0;
      strcpy( message, "Key Q=quit and accept, Mouse LEFT=start interaction" );
      do
      {
         status_c( tofchar(message) );
         stat = interactivecursor( &x, &y );
         if (stat == '1' && found == 2)
         {
            /* There are already 2 x-positions so reset and redraw */
            stat  ='3';
            found = 0;
         }
         if (stat == '3')
         {
            X[0] = X[1] = Y[0] = Y[1] = blank;
            x = (Xmin + Xmax) / 2.0;
            y = (Ymin + Ymax) / 2.0;
            makeplot( Xmin, Ymin, Xmax, Ymax,
                      Xunits.a,
                      Yunits.a,
                     "Width of global profile",
                      Xarray,
                      Yarray,
                      EYarray,
                      zerolevel,
                      ndata,
                      errbar );
            found = 0;
         }
         if (stat == '1')
         {
            float   yold = y;
            plmove( x, zerolevel);
            i = findindex( x, Xarray, ndata );
            y = getY( x, Xarray, Yarray, ndata, i );
            if (y != blank)
            {
               indx[found] = i;
               X[found] = x;
               Y[found] = y;
               found++;
               pldraw( x, y );
            }
            y = yold;
         }
         strcpy( message, "Mouse LEFT=mark line, RIGHT=redraw, Key Q=quit and accept");
      }
      while (stat != 'Q');
   }

   if (!found)
   {
      anyoutf( 1, "No positions identified");
      endprogram( Xarray, Yarray, EYarray, fp );
   }

   if (found == 1)
   {
      X[1] = X[0];
      Y[1] = Y[0];
      indx[1] = indx[0];
   }

   /* Sort the points */
   if (X[0] > X[1])
   {
      anyoutf( 1, "Swapping positions..." );
      swapf( &X[0], &X[1] );
      swapf( &Y[0], &Y[1] );
      swapi( &indx[0], &indx[1] );
   }

   anyoutf( 3, " " );
   i = sprintf( message, "%9.9s | %9.9s +/- %9.9s | %9.9s | %9.9s +/- %9.9s |",
                "W-level", "width", "error", "velocity", "area", "error" );
   anyoutf( 3, message );
   if (writetofile)
   {
      message[0] = '!';               /* Start of comment character */
      fprintf( fp, "%s\n", message );
   }
   memset( message, '=', i );
   message[i] = '\0';
   anyoutf( 3, message );
   if (writetofile)
   {
      message[0] = '!';
      fprintf( fp, "%s\n", message );
   }

   for (i = 0; i < nlevels; i++)
   {
      float    delta;
      float    Xerr;
      char     formstr[80];
      
      if (nlevels == 1)
         delta = 0.0;
      else
      {
         delta = 0.01 * fabs(Ymax - Ymin);
         if (i == 0 && wlevel[0] < wlevel[1])
            delta *= -1.0;
         if (i == 1 && wlevel[1] < wlevel[0])
            delta *= -1.0;
      }
      if (i == 1)
         pgsci_c( &red );
      else
         pgsci_c( &yellow );

      /* 'zerolevel' could be != 0 */
      YL[0] = zerolevel + wlevel[i] * (Y[0]-zerolevel) / 100.0;
      YL[1] = zerolevel + wlevel[i] * (Y[1]-zerolevel) / 100.0;
      XL[0] = getX( 'L', indx[0], YL[0], Xarray, Yarray, ndata );
      XL[1] = getX( 'R', indx[1], YL[1], Xarray, Yarray, ndata );

      plmove( X[0],  YL[0] );
      pldraw( XL[0], YL[0] );
      plmove( X[1],  YL[1] );
      pldraw( XL[1], YL[1] );

      plpoint( XL[0], zerolevel + delta, FILLED_DOT );
      plmove(  XL[0], zerolevel + delta );
      pldraw(  XL[1], zerolevel + delta );
      plpoint( XL[1], zerolevel + delta, FILLED_DOT );

      Xerr = 0.0;
      strcpy( formstr, "%9g   %9g     %9g   %9g   %9g     %9g" );      
      if (errbar)
      {
         float Xerr_l, Xerr_r;
         
         Xerr_l = geterror( 'L', XL[0], YL[0], Xarray, Yarray, EYarray, ndata );
         Xerr_r = geterror( 'R', XL[1], YL[1], Xarray, Yarray, EYarray, ndata );
         if (Xerr_l != blank && Xerr_r != blank)
            Xerr = sqrt( Xerr_l*Xerr_l + Xerr_r*Xerr_r );
         else if (Xerr_l == blank)
            Xerr = Xerr_r;
         else if (Xerr_r == blank)
            Xerr = Xerr_l;
                         
         if (Xerr != 0.0)
         {
            Xerr *= 0.5;    /* Both ends involved */
            plmove( XL[0] - Xerr, zerolevel + delta );
            pldraw( XL[0] + Xerr, zerolevel + delta );
            plmove( XL[1] - Xerr, zerolevel + delta );
            pldraw( XL[1] + Xerr, zerolevel + delta );
         }
      }

      (void) sprintf( message, 
                      formstr, 
                      wlevel[i],
                      ABS( XL[1] - XL[0] ),
                      Xerr,
                      (XL[1] + XL[0]) / 2.0,
                      area,
                      areaerror );
      anyoutf( 3, message );
      if (writetofile)
         fprintf( fp, "%s\n", message );
   }
   anyoutf( 3, " " );

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   endprogram( Xarray, Yarray, EYarray, fp );
   return( EXIT_SUCCESS );                        /* Dummy return */
}
