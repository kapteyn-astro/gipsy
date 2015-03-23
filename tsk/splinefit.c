/*                             COPYRIGHT (c) 2006
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             splinefit.dc1

Program:       SPLINEFIT

Purpose:       Fit spline to data in selected subses and interpolate data for other subsets

Category:      FITTING, MANIPULATION

File:          splinefit.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 2048.
               (e.g. INSET=AURORA FREQ)


   BOX=        Give box in .....                        [entire subset]


   SPLINESET=  Give (same) input set and subsets which set the spline data:
               Enter the same set as in INSET=, but specify now the
               subsets which contains the spline control points.
               The calculated spline always will go through these 
               control points.
               (e.g. SPLINESET=AURORA FREQ -10 0 10 15)
       
   SORDER=     Number of intervals to calculate average Y for control point: [0]
   
               If another value than the default is selected, then the 
               program will sub divide a profile in SORDER= intervals
               and in each interval an average Y is calculated which
               (with the central X) forms one spline control point.
               The effect will be a smoothing of data.


   OUTSET=     Give output set (, subsets):
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets. 

   INGRIDS=    X values of profile in grids:                      Y/[N]
               In most cases you want to transform the X values in 
               your profiles to physical values (e.g. grids along 
               frequency axis to km/s).
               Default the conversion is done if the header defines
               such a transformation. To override the default and 
               work in grids only, you should select 'Y'.

   PLOT=       Plot all the profiles in the box:                  Y/[N]
               Each profile is plotted together with the spline control
               points and the calculated spline.


   PLOTMINMAX= Enter min, max in Y for profile plots:         [min,max]
               If a plot is wanted, the values for DATAMIN and DATAMAX 
               are read from the header of the input set. These values 
               are the default min, max in de prompt.
               The values are used to set the range in Y for the plots.

   GRDEVICE=   Plot device:                           [List of devices]
               Destination of plot, Screen or Hardcopy.


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
               1  single stroke 'normal' font
               2  roman font
               3  italic font
               4  script font


Description:   This program can serve as a template for programs that process 
               profiles. It avoids the confusing syntax for so called class 2
               programs. It tries to read, as efficient as possible, profile 
               data, manipulates the data and writes the result to an output 
               set.
               This program uses a sample of profile points (given with 
               SPLINESET=) and calculates the cubic spline through these points.
               The other points in the profile are interpolated using this 
               spline and written to the output set.
               If you subtract the result (OUTSET=) from the input (INSET=)
               then you should see no residuals in those subsets that were 
               selected to define the spline control points.
               
               Note that you can only fit missing subsets that are between the
               first and last subset with spline controls.
               
               The program is straightforward and easy to expand.
               It can easily be extended with other functions that 
               process the profile data (e.g. polynomial fit).

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

Example:       .......

Updates:       May 22, 2006: VOG, Document created.

#<
*/

/*  splinefit.c: include files     */

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

#include    "userfio.h"      /* Easy-C companions for user interface routines.*/
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
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "minmax3.h"      /* Find min, max and #blanks in subset. */
#include    "wminmax.h"      /* Writes (new) minimum and maximum and number */
                             /* of blanks of subsets in the descriptor file */
                             /* and optionally deletes the MINMAX descriptors */
                             /* at intersecting levels. */


/* Output set related */

#include    "gdsasn.h"       /* GDSASN copies the coordinate system of a */
                             /* previously opened input set obtained with */
                             /* GDSINP to the output set to be obtained */
                             /* with GDSOUT. */
#include    "gdsout.h"       /* GDSOUT prompts the user to enter the */
                             /* name of an output set and the subsets, */
                             /* and returns the number of subsets entered. */
#include    "gdsi_write.h"   /* Writes data to (part of) an set. */


/* PGPLOT includes */

#include    "pgplot.h"       /* All PGPLOT includes. */


#include    "grtoph.h"
#include    "phtogr.h"
#include    "gdsc_word.h"
#include    "matrix.h"       /* M[ylo..yhi][xlo..xhi] */
#include    "deputy.h"
#include    "xeq.h"
#include    "wkey.h"
#include    "gds_close.h"
#include    "setdblank.h"
#include    "gdsd_rreal.h"

/* DEFINITIONS: */

/* Initialize Fortran compatible string with macro 'fmake' */

#define OK		   0		/* no problems */
#define	NOMEMORY	  -1		/* no memory available for buffers */
#define EQALXES		  -2		/* two x-coordinates are equal */

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
#define RAD(a)         ( (a) * 0.017453292519943295769237 )
#define DEG(a)         ( (a) * 57.295779513082320876798155 )

#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     2048       /* Max. allowed subsets */
#define MAXBUF         4096       /* Buffer size for I/O */
#define STRLEN         256        /* Max length of strings */
#define FILENAMELEN    256        /* Max length of file names */
#define FITSLEN        20         /* Max length of header items etc.*/
#define NONE           0          /* Default levels in userxxx routines */
#define REQUEST        1          
#define HIDDEN         2          
#define EXACT          4          
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0          

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set and subsets to interpolate:")
#define KEY_SPLINESET      tofchar("SPLINESET=")
#define MES_SPLINESET      tofchar("Give (same) input set and subsets which set the spline data:")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set (subset(s)): ")
#define KEY_SORDER     tofchar("SORDER=");
#define MES_SORDER     tofchar("Number of intervals to calculate average Y for control point: [0]");

/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered according to the */
                                    /* specification by the user. */
static fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
                                    /* number of grids along an axes as */
                                    /* specified by the user. The first elements */
                                    /* (upto the dimension of the subset) contain */
                                    /* the length of the subset axes, the other */
                                    /* ones contain the the number of grids along */
                                    /* an axes outside the subset. */
                                    /* the operation for each subset, Class 2 */
                                    /* is for applications for which the operation */
                                    /* requires an interaction between the different */
                                    /* subsets. */
static fint     subdim;             /* Dimensionality of the subsets for class 1 applications */
static fint     setdim;             /* Dimension of set. */


static fchar    Setin_S;              /* Name of input set */
static fint     subin_S[MAXSUBSETS];  /* Subset coordinate words */
static fint     axnum_S[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered according to the */
                                    /* specification by the user. */
static fint     axcount_S[MAXAXES];   /* Array of size MAXAXES containing the */
                                    /* number of grids along an axes as */
                                    /* specified by the user. The first elements */
                                    /* (upto the dimension of the subset) contain */
                                    /* the length of the subset axes, the other */
                                    /* ones contain the the number of grids along */
                                    /* an axes outside the subset. */
                                    /* the operation for each subset, Class 2 */
                                    /* is for applications for which the operation */
                                    /* requires an interaction between the different */
                                    /* subsets. */
static fint     subdim_S;             /* Dimensionality of the subsets for class 1 applications */


static double   Xsp[MAXSUBSETS];
static double   Ysp[MAXSUBSETS];
static double   XspO[MAXSUBSETS];
static double   YspO[MAXSUBSETS];
static int      indx[MAXSUBSETS];
static float    Xplot[MAXSUBSETS];
static float    Yplot[MAXSUBSETS];


static fint klo = -1 ;
static fint khi = -1 ;



/* Box and frame related */

static fint     flo[MAXAXES];       /* Low  edge of frame in grids */
static fint     fhi[MAXAXES];       /* High edge of frame in grids */
static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */


/* Reading data */

static fint     subnr;              /* Counter for subset loop. */


/* OUTSET related variables */

static fchar    Setout;
static fint     subout[MAXSUBSETS];  /* Output subset coordinate words */
static fint     nsubsout;
static fint     axnumout[MAXAXES];
static fint     axcountout[MAXAXES];


/* PGPLOT variables */

const  fint     background  =  0;      /* Color definitions for PGPLOT. */
const  fint     foreground  =  1;      /* Black if background is white. */
const  fint     red         =  2;
const  fint     green       =  3;
const  fint     blue        =  4;
const  fint     cyan        =  5;
const  fint     magenta     =  6;
const  fint     yellow      =  7;
const  fint     orange      =  8;
const  fint     greenyellow =  9;
const  fint     greencyan   = 10;
const  fint     bluecyan    = 11;
const  fint     bluemagenta = 12;
const  fint     redmagenta  = 13;
const  fint     darkgray    = 14;
const  fint     lightgray   = 15;
static fint     symbol      =  2;      /* Take a '+' as plot symbol, see PGPLOT MANUAL */ 
static fint     color;


/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static bool     agreed = NO;        /* Loop guard. */
static float    minval[MAXSUBSETS]; /* Min. value of data for each subset. */
static float    maxval[MAXSUBSETS]; /* Max. value of data for each subset. */
static fint     nblanks[MAXSUBSETS];/* Number of blanks in each subset. */
static fint     mcount[MAXSUBSETS]; /* Counter for minmax3 */
static fint     change;             /* Used in WMINMAX. change!=0 means */
                                    /* minimum and maximum have changed and */
                                    /* that the MINMAX descriptors at */
                                    /* intersecting levels will be removed. */



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



void initplot( void )
/*------------------------------------------------------------*/
/* PURPOSE: Initialize PGPLOT.                                */
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
   paging = toflog( YES );
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



fint getcword( fchar  Setin,
               fint   profaxnum )
/*------------------------------------------------------------*/
/* PURPOSE: Find the coordinate word that corresponds to the  */
/*          profile axis.                                     */
/*                                                            */
/* The grids of all other axes are supposed to be 0.          */
/* This coordinate words enables the transformation between   */
/* grids and physical coordinates for the profile axis        */
/* (usually the velocity axis).                               */
/*------------------------------------------------------------*/
{
   int   i;
   fint  cw = 0;
   fint  setdim;


   setdim  = gdsc_ndims_c( Setin, &setlevel );
   for (i = 0; i < setdim; i++)
   {
      fint  igrid = 0;
      fint  axnr = (fint) i + 1;

      if (axnr != profaxnum)
      {
         fint  r1 = 0;
         cw = gdsc_word_c( Setin, &axnr, &igrid, &cw, &r1 );
      }
   }
   return( cw );
}



static fint spline( double *x   , double *y   , fint  n   , 
                    double  yp1 , double  ypn , double *y2 )
/*------------------------------------------------------------*/
/* PURPOSE:  spline constructs a cubic spline given a set     */
/*           of x and y values, through these values.         */
/*------------------------------------------------------------*/
{
	fint  i,k;
	double p,qn,sig,un,*u;

	u=(double *)malloc((unsigned) (n-1)*sizeof(double));
        if (!u) return( NOMEMORY ) ;
	if (yp1 > 0.99e30)
		y2[0]=u[0]=0.0;
	else {
		y2[0] = -0.5;
		u[0]=(3.0/(x[1]-x[0]))*((y[1]-y[0])/(x[1]-x[0])-yp1);
	}
	for (i=1;i<=n-2;i++) {
		sig=(x[i]-x[i-1])/(x[i+1]-x[i-1]);
		p=sig*y2[i-1]+2.0;
		y2[i]=(sig-1.0)/p;
		u[i]=(y[i+1]-y[i])/(x[i+1]-x[i]) - (y[i]-y[i-1])/(x[i]-x[i-1]);
		u[i]=(6.0*u[i]/(x[i+1]-x[i-1])-sig*u[i-1])/p;
	}
	if (ypn > 0.99e30)
		qn=un=0.0;
	else {
		qn=0.5;
		un=(3.0/(x[n-1]-x[n-2]))*(ypn-(y[n-1]-y[n-2])/(x[n-1]-x[n-2]));
	}
	y2[n-1]=(un-qn*u[n-2])/(qn*y2[n-2]+1.0);
	for (k=n-2;k>=0;k--)
		y2[k]=y2[k]*y2[k+1]+u[k];
	free(u);
        return( OK ) ;
}


static fint splint( double *xa , double *ya , double *y2a , 
                    fint n    , double x   , double *y   )
/*------------------------------------------------------------*/
/* PURPOSE:  splint uses the cubic spline generated with      */
/*           spline to interpolate values in the XY  table.   */
/*------------------------------------------------------------*/
{
        fint  r   = 0 ;
	fint  k;
	double h,b,a;

        if ( klo < 0 ){
  	   klo=0;
 	   khi=n-1;
        } else {
           if ( x < xa[klo] ) klo=0;
           if ( x > xa[khi] ) khi=n-1;
        }
	while (khi-klo > 1) {
		k=(khi+klo) >> 1;
		if (xa[k] > x) khi=k;
		else klo=k;
	}
	h=xa[khi]-xa[klo];
	if (h == 0.0) {
           setdblank_c( y ) ;
           r = EQALXES ;
        } else {
	   a=(xa[khi]-x)/h;
	   b=(x-xa[klo])/h;
	   *y=a*ya[klo]+b*ya[khi]+( (a*a*a-a)*y2a[klo]+
                                    (b*b*b-b)*y2a[khi] ) * (h*h) / 6.0;
        }
	return( r ) ;
}



fint dspline( double *xi , double *yi , fint *nin  ,
              double *xo , double *yo , fint *nout )
/*------------------------------------------------------------*/
/* PURPOSE:  Base spline function. Version for double         */
/*           precision                                        */
/*------------------------------------------------------------*/              
{
   double yp1 , ypn , *y2 , *xii , *yii , blank , alpha ;
   fint  error , n , m , nblank , niin ;

   setdblank_c( &blank ) ;
   nblank = 0 ;
   for ( n = 0 ; n < *nin ; n++ )
      if ( yi[ n ] == blank ) nblank++ ;

   niin = *nin - nblank ;
   if ( nblank == 0 ) {
      xii = xi ;
      yii = yi ;
   } else if ( nblank == *nin ) {
      for ( n = 0 ; n < *nout ; n++ ) 
         yo[ n ] = blank ;
      return( nblank ) ;
   } else {
      xii = (double *)malloc( ( niin ) * sizeof( double ) ) ;
      yii = (double *)malloc( ( niin ) * sizeof( double ) ) ;
      if ( !xii || !yii ) return( NOMEMORY ) ;
      for ( n = 0 , m = 0 ; ( n < *nin ) && ( m < niin ) ; n++ ) {
         if ( yi[ n ] != blank ) {
            xii[ m ] = xi[ n ] ;
            yii[ m ] = yi[ n ] ;
            m += 1 ;
         }
      }
   }

   y2 = (double *)malloc( ( niin ) * sizeof( double ) ) ;
   if ( !y2 ) return( NOMEMORY ) ;

   alpha = 1.0 ;
   yp1 = 3e30 * alpha ;
   ypn = 3e30 * alpha ;

   error = spline( xii , yii , niin , yp1 , ypn , y2 ) ;
   if ( error < 0 ) return( error ) ;
   
   klo = -1; /* Reset 'klo' before table interpolation */
   for ( n = 0  ; n < *nout ; n++ ) {
      error = splint( xii , yii , y2 , niin , xo[ n ] , &yo[ n ] ) ;
      if ( error < 0 ) return( error ) ;
   }

   if ( nblank != 0 ) {
      free( xii ) ;
      free( yii ) ;
   }
   free( y2  ) ;

#if (0)
   for ( n = 0  ; n < *nout ; n++ )
      anyoutf( 1, "xin, yin, xout, yout=%g %g %g %g", xi[n], yi[n], xo[n],yo[n] );
#endif

   return( nblank ) ;
}




MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint     maxsubs = MAXSUBSETS;
   fint     maxaxes = MAXAXES;           /* Max num. of axes the program can deal with.*/
   fint     class   = 1;                 /* Class 1 is for applications which repeat */
   fint     showdev = 1;
   fint     nsubs;                       /* Number of input subsets */
   fint     nsubs_S;
   fint     dfault;                      /* Default option for input etc */
   fint     profcw;                      /* Coordinate word of profile */
   fint     profaxnum;                   /* Axis number of profile in set */
   int      i, j;
   float    **image = NULL;              /* Multiple buffer for all subsets */
   fint     imagesize;
   fint     xlen;
   int      x, y, z;   
   bool     plot = NO;
   float    plotminmax[2];
   bool     ingrids = NO;
   fint     sorder;                      /* Subdivide each profile in this number of intervals and */
                                         /* calculate for each interval an aveage Y as spline control point */
   
   init_c();                             /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;              /* Name of current task */
      fmake( Task, 20 );                 /* Macro 'fmake' must be available */
      myname_c( Task );                  /* Get task name */
      Task.a[nelc_c(Task)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, RELEASE ); /* Show task and version */
   }
   setfblank_c( &blank );

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
   
   /*--------------------------------------------------*/
   /* Get the input set. Documentation can be found in */
   /* $gip_sub/gdsinp.dc2                              */
   /*--------------------------------------------------*/
   {
      fmake( Setin_S, STRLEN );
      dfault  = NONE;
      subdim  = 2;                  /* Allow only 2-dim structures */
      nsubs_S = gdsinp_c( Setin_S,      /* Name of input set. */
                        subin_S,      /* Array containing subsets coordinate words. */
                        &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                        &dfault,    /* Default code as is USERxxx. */
                        KEY_SPLINESET,  /* Keyword prompt. */
                        MES_SPLINESET,  /* Keyword message for the user. */
                        &showdev,   /* Device number (as in ANYOUT). */
                        axnum_S,      /* Array of size 'maxaxes' containing the axes numbers. */
                                    /* The first elements (upto the dimension of the subset) */
                                    /* contain the axes numbers of the subset, */
                                    /* the other ones contain the axes numbers */
                                    /* outside the subset ordered according to the */
                                    /* specification by the user. */
                        axcount_S,    /* Number of grids on axes in 'axnum' */
                        &maxaxes,   /* Max. number of axes. */
                                    /* the operation for each subset. */
                        &class,     /* Class 1 is for applications which repeat */
                        &subdim_S );  /* Dimensionality of the subsets for class 1 */
   }  
 
   anyoutf( 1, "%d subsets to be interpolated", nsubs );
   anyoutf( 1, "%d subsets that sets the spline data", nsubs_S );

   /*--------------------------------------------------*/
   /* Smooth data by calculating 'sorder' sub interval */
   /* Y average values as spline control points.       */
   /*--------------------------------------------------*/   
   sorder = 0;
   (void) userfint( &sorder, 1, REQUEST, "SORDER=",
                    "Number of intervals to calculate average Y for control point: [0]" );   
                           

   profaxnum = axnum[setdim-1];
   profcw = getcword( Setin, profaxnum );

   /*--------------------------------------------------*/
   /* Preset the array with indices which indicate     */
   /* that a subset contains a spline control point    */
   /*--------------------------------------------------*/   
   for (j = 0; j < nsubs_S; j++)
   {
      indx[j] = 0;
   }

   (void) userflog( &ingrids, 1, REQUEST, "INGRIDS=", 
                    "X values of profile in grids:         Y/[N]" );
   ingrids = tobool( ingrids );

   /*--------------------------------------------------*/
   /* Convert each grid in the profile to a physical   */
   /* coordinate. If the grid is equal to a grid       */
   /* corresponding with a subset from the spline set  */
   /* then flag this subset, i.e. store its array      */
   /* index.                                           */
   /*--------------------------------------------------*/      
   {
      int i, j;

      for (i = 0; i < nsubs; i++)
      {
         int   r1 = 0;
         double grid = (double) gdsc_grid_c( Setin, &profaxnum, &subin[i], &r1 );
         if (ingrids)
         {
            XspO[i] = grid;
         }
         else
         {
            r1 = grtoph_c( Setin, &profcw, &grid, &XspO[i] );
         }
         /* Check which subsets contain the spline control points */
         {
            for (j = 0; j < nsubs_S; j++)
            {
               fint  r2 = 0;
               double grid_S = (double) gdsc_grid_c( Setin, &profaxnum, &subin_S[j], &r2 );
               /*anyoutf( 1, "subset =%d spline subset= %d", (int)grid, (int)grid_S );*/
               if (grid == grid_S)
                  indx[j] = i;
            }
         }
      }
   }

   /*--------------------------------------------------*/
   /* Give user some information about the physical    */
   /* coordinates and the array locations that stored  */
   /* a spline control point.                          */
   /*--------------------------------------------------*/    
   for (i = 0; i < nsubs; i++)
   {
      anyoutf( 1, "Phys %d = %e", i, XspO[i] );      
   }

   for (j = 0; j < nsubs_S; j++)
   {
      anyoutf( 1, "Array element %d contains spline data %f", indx[j], XspO[indx[j]] );
   }
         	        
   /*--------------------------------------------------------------*/
   /* Assign 'gdsinp' buffer to 'gdsout'. Output set will get same */
   /* coordinate system as input INSET=.  GDSOUT is a function     */
   /* which prompts the user to enter the name of a set and        */
   /* (optionally) subset(s) and returns the number of subsets     */
   /* entered.                                                     */
   /*--------------------------------------------------------------*/
   gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
   dfault  = NONE;
   fmake( Setout, STRLEN );
   do {
      nsubsout = gdsout_c( Setout,        /* Name of the output set. */
                           subout,        /* Output array with subsets coordinate words.*/
                           &nsubs,        /* Maximum number of subsets in subout. */
                           &dfault,       /* Default code as in USERxxx. */
                           KEY_OUTSET,    /* User keyword prompt. */
                           MES_OUTSET,    /* Message for the user. */
                           &showdev,      /* Device number (as in ANYOUT). */
                           axnumout,      /* Array of size 'maxaxes' containing the axes numbers. */
                           axcountout,    /* Array with the axis sizes. */
                           &maxaxes );    /* Max axes the program can deal with. */
      agreed = (nsubsout == nsubs);
      if (!agreed)
         reject_c( KEY_OUTSET, tofchar("#out != #in") );
   }
   while (!agreed);

   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/

   

   (void) userflog( &plot, 1, REQUEST, "PLOT=", 
                    "Plot all the profiles in the box:         Y/[N]" );
   plot = tobool( plot );
   
   if (plot)
   {
      fint     r = 0;
      char     mess[128];
      
      initplot();
      gdsd_rreal_c( Setin, tofchar( "DATAMIN" ), &setlevel, &plotminmax[0], &r );
      r = 0;
      gdsd_rreal_c( Setin, tofchar( "DATAMAX" ), &setlevel, &plotminmax[1], &r );      
      sprintf( mess, "Enter min, max in Y for profile plots:  [%g,%g]", plotminmax[0], plotminmax[1] );
      (void) userfreal( plotminmax, 2, REQUEST, "PLOTMINMAX=", mess );
   }
   
   /*--------------------------------------------------*/
   /* Before looping over all profiles in the given    */
   /* box, create a buffer to hold the data. In a XYZ  */
   /* cube, the buffer is a XZ buffer plane. It must   */
   /* be big enough to store all the profile data for  */
   /* all the X values at certain Y.                   */
   /* Note that we treat a class 2 program as class 1. */
   /*--------------------------------------------------*/      
   image = fmatrix( blo[0], 0, bhi[0], nsubs-1 );
   imagesize = (bhi[0] - blo[0] + 1) * nsubs;
   xlen = bhi[0] - blo[0] + 1;                         /* Length of a profile */
   if (!image)
        errorf( 4, "Cannot allocate %d bytes for profile data!", imagesize*sizeof(float) );
    
   
   /* Preset counters for minmax3 */
   for (subnr = 0; subnr < nsubs; subnr++)  
   {
      mcount[subnr] = 0;
   }
 
   /* Now loop over all the lines in Y and read the XZ buffer */
   for (y = blo[1]; y <= bhi[1]; y++)
   {
      for (subnr = 0; subnr < nsubs; subnr++)
      {
         fint  tid = 0;
         fint  cwlo, cwhi;            /* Coordinate words */              
         fint  pixelsread;
         fint  glo[2], ghi[2];
         
         glo[0] = blo[0];
         glo[1] = y;
         ghi[0] = bhi[0];
         ghi[1] = y;
         cwlo   = gdsc_fill_c( Setin, &subin[subnr], glo );
         cwhi   = gdsc_fill_c( Setin, &subin[subnr], ghi );

         gdsi_read_c( Setin,
                      &cwlo, &cwhi,
                      &image[subnr][blo[0]],
                      &xlen,
                      &pixelsread,
                      &tid );                 
      }

      for (x = blo[0]; x <= bhi[0]; x++)
      {
         fint   r;
         /*--------------------------------------------------*/
         /* At this value of x, we process the profile for   */
         /* all values of z.                                 */
         /*--------------------------------------------------*/
         if (plot)
         {         
            for (z = 0; z < nsubs; z++)
            {
               /*--------------------------------------------------*/
               /* Copy the relevant part of the buffer to an array */
               /* which stores the data of just one profile.       */
               /*--------------------------------------------------*/
               Xplot[z] = (float) XspO[z];
               Yplot[z] = image[z][x];               
            }
            drawbox( Xplot[0], plotminmax[0], Xplot[nsubs-1], plotminmax[1], 
                     "X grid", "Y grid", "Yellow=data, Cyan=controls, red=spline fit" );         
            symbol = 2;
            color = yellow;            
            pgsci_c( &color );
            pgpt_c( &nsubs, Xplot, Yplot, &symbol );
         }
         if (sorder == 0)
         /*--------------------------------------------------*/
         /* Fill arrays with the spline control points.      */
         /*--------------------------------------------------*/
         {
            int  j;
            for (j = 0; j < nsubs_S; j++)
            {
               int    n = indx[j];
               Xsp[j] = XspO[n];
               Ysp[j] = (double) image[n][x];
               if (plot)
               {
                  Xplot[j] = (float) Xsp[j];
                  Yplot[j] = (float) Ysp[j];
               }
            }
            if (plot)
            {
               symbol = 4;
               color = cyan;
               pgsci_c( &color );
               pgpt_c( &nsubs_S, Xplot, Yplot, &symbol );
            }
         }
         else
         /*--------------------------------------------------*/
         /* Fill arrays with the spline control points, but  */
         /* do not read them form splineset= but use sorder= */
         /* intervals to calculate an average Y per interval.*/
         /*--------------------------------------------------*/
         {
            int    k, si;
            int    indx, pindx;
            double sumY = 0.0;
            double sumX = 0.0;            
            
            nsubs_S = 0;
            indx = pindx = 0;
            k = 0;                        
            for (si = 0; si < nsubs; si++)
            {
               indx = (int) ((si * (double) sorder) /(double) nsubs);
               if (indx > pindx || (indx == pindx && si == nsubs-1))
               {
               	  if (k > 0)
               	  {
               	     Ysp[pindx] = sumY / (double) k;
               	     Xsp[pindx] = sumX / (double) k;
               	     anyoutf(1, "indx=%d k=%d sumX=%f, sumY=%f", pindx, k, sumX, sumY );
                     if (plot)
                     {
                        Xplot[pindx] = (float) Xsp[pindx];            
                        Yplot[pindx] = (float) Ysp[pindx];
                     }                                	  
               	     nsubs_S++;
               	  }
                  k = 0; sumY = 0.0; sumX = 0.0; pindx = indx;    /* Reset for next average */
               }
               else
               {
               	  k++;
               	  sumY += (double) image[si][x];
               	  sumX += XspO[si];
               }
            }
            if (plot)
            {
               symbol = 4;
               color = cyan;
               pgsci_c( &color );
               pgpt_c( &nsubs_S, Xplot, Yplot, &symbol );
            }            
         }
         r = dspline( Xsp, Ysp, &nsubs_S, XspO, YspO, &nsubs );
         /* Copy result into XZ buffer */
         for (z = 0; z < nsubs; z++)
         {
            image[z][x] = (float) YspO[z];
            if (plot)
            {
               Xplot[z] = (float) XspO[z];            
               Yplot[z] = (float) YspO[z];
            }
         }
         if (plot)  /* The spline interpolation */
         {
            symbol = 5;
            color = red;
            pgsci_c( &color );                     
            pgpt_c( &nsubs, Xplot, Yplot, &symbol );
         }
      }

      /*--------------------------------------------------*/
      /* We processed all the profiles for this Y. Now    */
      /* store the modified XZ buffer in the outset.      */
      /*--------------------------------------------------*/      
      for (subnr = 0; subnr < nsubs; subnr++)
      {
         fint  tidO = 0;                 
         fint  cwloO, cwhiO;            /* Coordinate words */              
         fint  pixelswrite;
         fint  glo[2], ghi[2];
         
         glo[0] = blo[0];
         glo[1] = y;
         ghi[0] = bhi[0];
         ghi[1] = y;
         /* Use input grid coordinates, but connect to output subsets */
         cwloO  = gdsc_fill_c( Setout, &subout[subnr], glo );
         cwhiO  = gdsc_fill_c( Setout, &subout[subnr], ghi );
         
         minmax3_c( &image[subnr][blo[0]],
                    &xlen,
                    &minval[subnr], &maxval[subnr],
                    &nblanks[subnr],
                    &mcount[subnr] );        

         /* anyoutf( 1, "subset=%d, min=%f, max=%f", subnr, minval[subnr], maxval[subnr] ); */
         gdsi_write_c( Setout,
                       &cwloO, &cwhiO,
                       &image[subnr][blo[0]], 
                       &xlen,
                       &pixelswrite,
                       &tidO );         
      }
   }
   /* Update OUTSET= descriptor with new values */
   change = YES;
   wminmax_c( Setout, subout,
              minval, maxval, nblanks, 
              &nsubsout,
              &change );




#if (0)
   /*--------------------------------------------------*/
   /* Start task to calculate min, max on top level    */
   /*--------------------------------------------------*/      
   {
        char   mess[1024];
        fint  r = 0;
                
        Setout.a[nelc_c(Setout)] = '\0';
        sprintf( mess, "MNMX INSET=%s BOX=;", Setout.a );
        gds_close_c( Setout, &r );              
        r = 0;
        xeq_c( tofchar(mess) , &r );
        anyoutf( 1, "This program called %s. The return status was:%d", mess, r );
   }
#endif

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   pgend_c();
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
