/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             slice.dc1

Program:       SLICE

Purpose:       Make a slice through a set

Category:      MANIPULATION, COORDINATES, HEADER, PLOTTING

File:          slice.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 4096.
               Dimension of the subsets must be 2.

   POSITION=   Give central position:                          [0,0]

   ANGLE=      Give angle (deg), w.r.t North towards the East:

   GRIDOUT=    New grid spacing of            [spacing in longitude]
               rotated axis:

   POINTS=     Give number of pixels on rotated axis:
                                [Number of pixels on longitude axis]

   SLICES=     Number of slices above and below slice          [0,0]
               through new projection centre.
               Total number of slices cannot exceed 256.

   SPACE=      Spacing between slices:    [grid spacing in latitude]

   OUTSET=     Give output set:

   PLOT=       Create plot of slice?                              Y/[N]
               If you want to now how the slices are orientated
               in your box, select PLOT=Y. A box is plotted and labeled
               and the slices in the first subset are displayed.
               The following keywords can be specified now:


               PGPLOT keywords:

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

** PGCOLOR=    Give color 1..15:                                    [1]
               See description for the available colors.

** PGWIDTH=    Give line width 1..21:                               [2]

** PGHEIGHT=   Give character height:                             [1.0]

** PGFONT=     Give font 1..4:                                      [2]

** PGSYMBOL=   Give symbol number 0..127:                           [1]



Description:   The program SLICE allows you to extract a plane from
               a data set (INSET=) under an arbitrary angle. The
               extracted plane is stored in an output set (OUTSET=)
               that has the same axis structure as the input set.
               The subsets you define in the input must be two
               dimensional and must have two spatial axes. The
               number of subsets determine the length of the third
               axis in the output (usually the spectral axis) and
               must be less than 4096.
               You can think of the output as a set where the longitude
               axis originates from a rotated and/or shifted axis in the
               input set. The number of pixels in the latitude direction
               depends on the number of parallel slices you want to
               extract. In order to view for example a LV plane in GIDS
               you specify something like:

                              VIEW INSET=M8320SLICE d 0


               The central position (POSITION=) is used to determine
               the position of the rotation centre. The positions can be
               prefixed (according to the documentation of dcdpos.dc3).
               For example, if you want the new central position in
               input pixel coordinates (-10.680130, -8.700096) and
               position (0,0) in your input corresponds to the physical
               coordinates 198.468100, 46.002610 deg. you specify:

               POSITION=-10.680130 -8.700096              (pixels)   or:
               POSITION= U 198.493667  U 45.983306        (deg, deg) or:
               POSITION= * 13 13 58.48  * 45 58 59.90     (hms, dms)

               The position angle of the major axis of a galaxy is
               defined as the angle taken in anti-clockwise direction
               between the north direction in the sky and the major axis
               of the receding half of that galaxy (Rots 1975) astron,
               astrophys 45, 43. If you want a slice through this galaxy
               specify ANGLE= <position angle of galaxy>
               The program takes care of the fact that your original
               latitude axis perhaps does not align with the north.

               POINTS= gives the number of pixels on the rotated axis
               and the value in GRIDOUT= determines which pixels of
               the input are taken into account. If your new spacing
               exceeds a certain value, it will be possible that pixels
               are examined outside the input frame. The program
               substitudes blanks for these positions.
               Parallel slices (in real space) can be constructed if
               you specify SLICES= The default is no extra slices.
               If SLICES=n, m you get slices n pixels below and m
               output pixels above the central slice. The grid spacing
               of these output pixels are given in SPACE=


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



Notes:         If Hermes is in TEST MODE, there is extra output to
               the screen about properties of all axes in the input set.

Example:

Updates:       Sep 12, 1991: VOG, Document created.
               Apr 12, 1995: VOG, removed extra 'PGLAB' code.
               Apr 09, 2009: VOG, Added offsets to grids from proco. Finally all
                                  plots show slices through the selected origin, even
                                  for CRPIX values that end on 0.5


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
#include "float.h"
#include "gdsinp.h"
#include "gdspos.h"
#include "gdsasn.h"
#include "gdscpa.h"
#include "gdscss.h"
#include "gdsout.h"
#include "setfblank.h"
#include "myname.h"
#include "anyout.h"
#include "nelc.h"
#include "cancel.h"
#include "gdsbox.h"
#include "gds_exist.h"
#include "gds_delete.h"
#include "gds_create.h"
#include "gds_extend.h"
#include "gds_errstr.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsc_fill.h"
#include "gdsc_name.h"
#include "gdsi_read.h"
#include "gdsi_write.h"
#include "gdsc_ndims.h"
#include "usertext.h"
#include "userchar.h"
#include "userreal.h"
#include "userdble.h"
#include "userlog.h"
#include "userint.h"
#include "userfio.h"
#include "error.h"
#include "axtype.h"
#include "reject.h"
#include "getrange.h"
#include "minmax3.h"
#include "wminmax.h"
#include "status.h"
#include "cotrans.h"
#include "proco.h"
#include "factor.h"
#include "hms.h"
#include "dms.h"
#include "gdsd_find.h"
#include "gdsd_rchar.h"
#include "gdsd_rreal.h"
#include "gdsd_rdble.h"
#include "gdsd_wreal.h"
#include "gdsd_rint.h"
#include "gdsd_wint.h"
#include "gdsd_wdble.h"
#include "gdsd_wchar.h"
#include "gdsd_readc.h"
#include "gdsd_writec.h"
#include "gdsd_length.h"
#include "gdsd_delete.h"
#include "gdsd_rvar.h"
#include "gdsd_wvar.h"
#include "gdsa_wcdble.h"       /* Write doubles to a column in a GDS table.*/
#include "gdsa_crecol.h"       /* Create a column in a GDS table.*/

/* PGPLOT includes */

#include    "pgplot.h"


#define AXESMAX    10               /* Max. allowed number of axes in a set */
#define SUBSMAX    4096             /* Max. number of substructures to be specified */
#define MAXBUF     4096             /* Buffer size for I/O */
#define MAXPOINTS  2048             /* Width of a slice */
#define MAXSLICES  256              /* Max. num. of slices */
#define BIGSTORE   80               /* Length of a string */
#define FITSLEN    20
#define ITEMLEN    8                /* Table, column name length */
#define VERSION    "1.0"            /* Version number of this program */
#define NONE       0                /* Default values for use in userxxx routines */
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define false      0
#define true       1
#define YES        1
#define NO         0
#define PI         3.141592653589793
#define TERM       1
#define LOG        2
#define LOGTERM    3
#define TEST       16               /* Anyout device */


/* Keywords and messages */

#define KEY_INSET         tofchar("INSET=")
#define MES_INSET         tofchar("Give set (, subsets): ")
#define KEY_OUTSET        tofchar("OUTSET=")
#define MES_OUTSET        tofchar("Output set (name only):")
#define KEY_POSITION      tofchar("POSITION=")
#define MES_POSITION      tofchar("Give central position: ")
#define KEY_ANGLE         tofchar("ANGLE=")
#define MES_ANGLE         tofchar("Give angle (deg), w.r.t North towards the East:")
#define KEY_GRIDOUT       tofchar("GRIDOUT=")
#define KEY_POINTS        tofchar("POINTS=")
#define KEY_SLICES        tofchar("SLICES=")
#define MES_SLICES        tofchar("Number of slices Above and Below [0,0]")
#define KEY_SPACE         tofchar("SPACE=")
#define KEY_PLOT          tofchar("PLOT=")
#define MES_PLOT          tofchar("Create plot of slice?        Y/[N]")

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
#define MYMIN(a,b) ((a) > (b) ? (b) : (a))

/* Input of set, subsets: */

static fchar    Setin;                 /* Name of the set */
static fint     Subin[SUBSMAX];        /* Array for the subset coordinate words */
static fint     Nsubsin;               /* Number of input subsets */
static fint     Dfault;                /* Default option for input etc */
static fint     Axnum[AXESMAX];        /* GDSINP axis numbers array */
static fint     Axcount[AXESMAX];      /* GDSINP axis lengths array */
static fint     Class = 1;             /* Axis is operation axis */
static fint     Setdim;                /* Dimension of the set */
static fint     Subdim;                /* Dimension of the subset */
static fint     Maxaxes  = AXESMAX;    /* Convert parameters to variables */
static fint     Maxsubs  = SUBSMAX;    /* Max. input subsets */
static int      subnrI;                /* Index of current input subset */
static int      subnrO;                /* Index of current output subset */
static int      i,j;                   /* Counters */
static fint     Setlevel = 0;          /* Indicate set level */


/* Output set related */

static fchar    Setout;                /* Name of the set */
static fint     Subout[SUBSMAX];       /* Array for the subset coordinate words */
static fint     Nsubsout;              /* Number of input subsets */
static fint     Axnumout[AXESMAX];     /* GDSOUT axis numbers array */
static fint     Axcountout[AXESMAX];   /* GDSOUT axis lengths array */


/* Input of area etc.:*/

static fint     CwloI;                 /* Coordinate words */
static fint     CwhiI;
static fint     CwloO;
static fint     CwhiO;
static fint     GridloI[AXESMAX];      /* Coordinates for input-frame */
static fint     GridhiI[AXESMAX];
static fint     GridloO[AXESMAX];      /* Coordinates for output-frame */
static fint     GridhiO[AXESMAX];


/* Data transfer: */

static fint     Writepixels;
static fint     TidI;                  /* Tranfer id for input */
static fint     TidO;                  /* Transfer id for output */
static float    *ImageI = NULL;        /* Multiple buffer for all subsets */
static float    ImageO[MAXPOINTS*MAXSLICES];     /* One buffer for output */
static fint     Buflen;
static int      Bufheight;             /* To determine max. buffer size */
static fint     Pixelsdone;
static int      LenX, LenY;


/* Related to update of header etc: */

static float    Datamin[SUBSMAX];
static float    Datamax[SUBSMAX];
static fint     Nblanks[SUBSMAX];


/* Rotation related */

static double   Angle;                 /* User wants this rotation angle */
static double   HeaderPA;              /* Position angle of map wrt N in dir of E */
static double   HeaderPAout;           /* New map position angle */
static double   Crota2out;             /* Crota2 in new descriptor */
static fint     Projection;            /* Type of projection according to axes */

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


/* Miscellaneous: */


static fint     nitems;               /* Max. number of input items in userxxx routines */
static fint     R1, R2;               /* Results of userxxx routines */
static float    Blank;                /* Value of system blank */
static char     messbuf[BIGSTORE];    /* Buffer for text message */
static fint     Mcount;               /* Counter for 'minmax3' routine */
static int      agreed;               /* Loop control */
static fint     Maxpos = 1;           /* Two coordinates == 1 position */
static double   Position[2];          /* New projection center */
static double   Cdelt[AXESMAX];       /* Grid spacings */
static fchar    Ctype[AXESMAX];       /* Name of the axis */
static fchar    Cunit[AXESMAX];       /* Units along axis */
static fchar    NCunit[AXESMAX];      /* Natural units prim. axis */
static double   Crpix[AXESMAX];       /* Reference pixel */
static double   Crval[AXESMAX];       /* Physical value at ref. pixel */
static double   Crota[AXESMAX];       /* Rotation angle of axis wrt north*/
static int      secondary[AXESMAX];   /* Are there secondary axes? */
static double   Ddelt[AXESMAX];       /* Secondary axis characteristics */
static fchar    Dtype[AXESMAX];
static fchar    Dunit[AXESMAX];
static double   Drval[AXESMAX];
static double   Drpix[AXESMAX];
static fchar    NDunit[AXESMAX];
static double   Convers1, Convers2;
static double   GridoutRA, GridoutDEC;          /* New output grid spacings */
static fint     numpoints;                      /* Max. width of profile */
static fint     numslices;                      /* Number of parallel slices */
static fint     NumslicesAB[2];                 /* Num. slices above and under first */
static fint     ax1ind, ax2ind, ax3ind;         /* Permutated indices */
static double   offsetX, offsetY;
static double   crpixX, crpixY;
static double   Xpos[MAXPOINTS][MAXSLICES];     /* Output positions */
static double   Ypos[MAXPOINTS][MAXSLICES];
static double   Xposrot[MAXPOINTS][MAXSLICES];  /* Rotated pos. in input map */
static double   Yposrot[MAXPOINTS][MAXSLICES];
static double   Xmax[MAXSLICES];
static double   Ymax[MAXSLICES];
static double   Xmin[MAXSLICES];
static double   Ymin[MAXSLICES];
static double   Physpos[2];                     /* Phys. coords of project center */
static bool     pgplot;
static fchar    key, mes;



static void fatalerror( char *str )
/*------------------------------------------------------------------*/
/* C version of 'error_c' with error level set to 4.                */
/*------------------------------------------------------------------*/
{
   fint  lev = 4;
   error_c( &lev, tofchar(str) );
}



static void displayGDSerror( int  level,
                             fint err )
/*------------------------------------------------------------*/
/* PURPOSE: Display a message string associated with a GDS    */
/* error code.                                                */
/*------------------------------------------------------------*/
{
   fchar   Errstr;
   fmake( Errstr, 180 );
   gds_errstr_c( Errstr, &err );
   anyoutf( level, "GDS error (%d): %.*s",
            (int) err,  
            nelc_c(Errstr), Errstr.a );
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
   if (r1 != 1) 
      fatalerror( "Cannot open output device" );

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
}



void drawbox( float Xmin, float Ymin, float Xmax, float Ymax,
              fchar Xtitle, fchar Ytitle, fchar Toptitle )
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
   char   message[80];


   pgpage_c();                                /* Advance to new page. */

   /* Increase the size of the box a little */
   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0) 
      delta = 1.0;
   Xmin -= delta; Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0) 
      delta = 1.0;
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

   lwidth = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &lwidth,
                   &nitems,
                   &dfault,
                   tofchar("PGWIDTH="),
                   tofchar("Give line width 1..21:        [2]") );
   if (lwidth > 21) lwidth = 21;
   if (lwidth < 1 ) lwidth =  1;
   pgslw_c( &lwidth );                        /* Set line width. */

   charsize = 1.0; nitems = 1; dfault = HIDDEN;
   r1 = userreal_c( &charsize,
                    &nitems,
                    &dfault,
                    tofchar("PGHEIGHT="),
                    tofchar("Give character height:     [1.0]") );
   pgsch_c( &charsize );                      /* Character height. */

   font = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &font,
                   &nitems,
                   &dfault,
                   tofchar("PGFONT="),
                   tofchar("Give font 1..4:        [2]") );
   if (font > 4) 
      font = 4;
   if (font < 1) 
      font = 1;
   pgscf_c( &font );                          /* Set font. */
   
   xtick = ytick = 0.0;
   nxsub = nysub = 0;
   pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
            tofchar("BCNSTV"), &ytick, &nysub );

   /* Plot the titles */
   (void) pglab_c( Xtitle, Ytitle, Toptitle );
}



static int validaxes( fchar Fsetin, fint Subdim, fint *Axnum,
                      fint *Projection, double *HeaderPA )
/*----------------------------------------------------------------------*/
/* To continue the program, the units of the axes must be the same. The */
/* axes must be spatial axes. 'finit', 'MYMAX' are macros, defined      */
/* outside this function. 'AXESMAX', 'false' are constants, defined     */
/* outside this function. Return the projection system and the rotation */
/* angle found in the header.                                           */
/*----------------------------------------------------------------------*/
{
   int      m;
   int      len;   
   int      ret;
   fint     R1;
   fint     Skysys, Velsys, Prosys;
   fint     Axistype;


   /* Check for units */

   for (m = 0; m < (int) Subdim; m++) 
   {
      finit( NCunit[m], FITSLEN );
      finit( Ctype[m] , FITSLEN );
      finit( NDunit[m], FITSLEN );
      R1 = 0;
      /* return name of this axis */
      gdsc_name_c( Ctype[m],
                   Setin,
                   &Axnum[m],
                   &R1 );
      if (R1 < 0) 
      {
         anyoutf( 1, "Cannot get name of axis" );
         return false;
      }
      /* Axis has a name, now get type etc. */
      Axistype = axtype_c( Ctype[m],
                           NCunit[m],
                           NDunit[m],
                           &Skysys,
                           &Prosys,
                           &Velsys );


      if (Axistype == 2) 
      {
         /* Spatial axis latitude */
         *Projection = Prosys;
         R1 = 0;
         (void) sprintf( messbuf, "CROTA%d", m+1 );
         gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, HeaderPA, &R1 );
         if (R1 < 0) 
         {
            anyoutf( 1, "Cannot find position angle in header, 0.0 assumed!");
            *HeaderPA = 0.0;
         }
      }

      if (Axistype == 0) 
      {
         anyoutf( 1, "Unknown type of axis!");
         return false;
      }
      if ((Axistype != 1) && (Axistype != 2)) 
      {
         /* No spatial axis */
         anyoutf( 1, "Non-spatial axis encountered!");
         return false;
      }
   }
   /* Test the units */
   len = MYMAX(nelc_c(NCunit[0]), nelc_c(NCunit[1]) );
   ret = true;
   if (strncmp( NCunit[0].a, NCunit[1].a, len ) != 0) 
   {
     anyoutf( 1, "Units of both axes are not the same!" );
     ret = false;
   }
   for (m = 0; m < (int) Subdim; m++)
   {
      free( NCunit[m].a );
      free( NDunit[m].a );
      free( Ctype[m].a );
   }
       
   /* Passed all the tests */
   return( ret );
}



void printinfo()
/*----------------------------------------------------------------------*/
/* Write input- & output characteristics to screen and log-file         */
/*----------------------------------------------------------------------*/
{
   fchar           Convstr;
   fint            mode = 0;
   fint            prec;
   double          dummyD[3];
   int             dev = LOGTERM;


   fmake( Convstr, BIGSTORE );
   strcpy( messbuf, "====================================SLICE====================================" );
   anyoutf( dev, messbuf );
   sprintf( messbuf,
           "Input set: %.*s",
            nelc_c( Setin ),
            Setin.a );
   anyoutf( dev, messbuf );

   { /* Give information about central position in input */
      anyoutf( dev, "     Central position: (0, 0)" );
      prec = 2;
      (void) hms_c( &Crval[ax1ind],
                    Convstr,
                    dummyD,
                    &prec,
                    &mode );
      anyoutf( dev, "     Longitude: %f %.*s == %.*s",
               Crval[ax1ind],
               nelc_c( Cunit[ax1ind] ),
               Cunit[ax1ind].a,
               nelc_c( Convstr ), Convstr.a );
      prec = 1;
      (void) dms_c( &Crval[ax2ind],
                    Convstr,
                    dummyD,
                    &prec,
                    &mode );
      anyoutf( dev, "     Latitude:  %f %.*s == %.*s",
               Crval[ax2ind],
               nelc_c( Cunit[ax2ind] ),
               Cunit[ax2ind].a,
               nelc_c( Convstr ), Convstr.a );
   }


   R1 = factor_c( Cunit[ax1ind], tofchar("ARCSEC"), &Convers1 );
   if (R1 == 0) 
   {
      /* Conversion was possible */
      anyoutf( dev, "     Grid spacing longitude: %f %.*s = %f arcsec",
               Cdelt[ax1ind],
               nelc_c( Cunit[ax1ind] ),
               Cunit[ax1ind].a,
               Cdelt[ax1ind] * Convers1 );
   }
   else 
   {
      anyoutf( dev, "     Grid spacing longitude: %f %.*s", Cdelt[ax1ind] );
   }

   R1 = factor_c( Cunit[ax2ind], tofchar("ARCSEC"), &Convers2 );
   if (R1 == 0) 
   {
      /* Conversion was possible */
      anyoutf( dev, "     Grid spacing latitude:  %f %.*s = %f arcsec",
               Cdelt[ax2ind],
               nelc_c( Cunit[ax2ind] ),
               Cunit[ax2ind].a,
               Cdelt[ax2ind] * Convers2 );
   }
   else 
   {
      anyoutf( dev, "     Grid spacing latitude:  %f %.*s", Cdelt[ax2ind] );
   }


   /* Display projection system */
   switch( (int) Projection ) {
      case 1:
         anyoutf( dev, "     Projection: AITOFF equal area projection");
         break;
      case 2:
         anyoutf( dev, "     Projection: Equivalent Cylindrical projection");
         break;
      case 3:
         anyoutf( dev, "     Projection: Flat projection");
         break;
      case 4:
         anyoutf( dev, "     Projection: Gnomonic projection");
         break;
      case 5:
         anyoutf( dev, "     Projection: Orthographic projection");
         break;
      case 6:
         anyoutf( dev, "     Projection: Rectangular projection");
         break;
      case 7:
         anyoutf( dev, "     Projection: Global Sinusoidal projection");
         break;
      case 8:
         anyoutf( dev, "     Projection: North Celestial Pole projection");
         break;
      case 9:
         anyoutf( dev, "     Projection: Stereographic projection");
         break;
      case 10:
         anyoutf( dev, "     Projection: Mercator projection");
         break;
   }
   anyoutf( dev, "     Rotation angle from header: %f (deg.)", HeaderPA );

   anyoutf( dev, "     Third axis: type %.*s, central value: %g (%.*s), spacing: %f (%.*s)",
            nelc_c( Ctype[ax3ind] ),
            Ctype[ax3ind].a,
            Crval[ax3ind],
            nelc_c(Cunit[ax3ind] ),
            Cunit[ax3ind].a,
            Cdelt[ax3ind],
            nelc_c(Cunit[ax3ind] ),
            Cunit[ax3ind].a );

   if (secondary[ax3ind]) 
   {
      anyoutf( dev, "     (sec. units): type %.*s, central value: %g (%.*s), spacing: %f (%.*s)",
               nelc_c( Dtype[ax3ind] ),
               Dtype[ax3ind].a,
               Drval[ax3ind],
               nelc_c(Dunit[ax3ind] ),
               Dunit[ax3ind].a,
               Ddelt[ax3ind],
               nelc_c(Dunit[ax3ind] ),
               Dunit[ax3ind].a );
   }

   anyoutf( dev, " ");

   /* Output characteristics */
   anyoutf( dev, "Output set: %.*s",
            nelc_c( Setout ),
            Setout.a );

   { /* Give information about central position in output */
      anyoutf( dev, "     Central position: (%g, %g) (in input pixel coordinates!)",
               Position[0], Position[1] );
      prec = 2;
      (void) hms_c( &Physpos[0],
                    Convstr,
                    dummyD,
                    &prec,
                    &mode );
      anyoutf( dev, "     Longitude: %f %.*s == %.*s",
               Physpos[0],
               nelc_c( Cunit[ax1ind] ),
               Cunit[ax1ind].a,
               nelc_c( Convstr ), Convstr.a );
      prec = 1;
      (void) dms_c( &Physpos[1],
                    Convstr,
                    dummyD,
                    &prec,
                    &mode );
      anyoutf( dev, "     Latitude:  %f %.*s == %.*s",
               Physpos[1],
               nelc_c( Cunit[ax2ind] ),
               Cunit[ax2ind].a,
               nelc_c( Convstr ), Convstr.a );
   }

   R1 = factor_c( Cunit[ax1ind], tofchar("ARCSEC"), &Convers1 );
   if (R1 == 0) 
   {
      /* Conversion was possible */
      anyoutf( dev, "     Grid spacing longitude: %f %.*s = %f arcsec",
               GridoutRA,
               nelc_c( Cunit[ax1ind] ),
               Cunit[ax1ind].a,
               GridoutRA * Convers1 );
   }
   else 
   {
      anyoutf( dev, "     Grid spacing longitude: %f %.*s", GridoutRA );
   }

   R1 = factor_c( Cunit[ax2ind], tofchar("ARCSEC"), &Convers2 );
   if (R1 == 0) 
   {
      /* Conversion was possible */
      anyoutf( dev, "     Grid spacing latitude:  %f %.*s = %f arcsec",
               GridoutDEC,
               nelc_c( Cunit[ax2ind] ),
               Cunit[ax2ind].a,
               GridoutDEC * Convers2 );
   }
   else 
   {
      anyoutf( dev, "     Grid spacing latitude:  %f %.*s", GridoutDEC );
   }

   anyoutf( dev, "     Output length in spectral direction: %d", (int) Nsubsin );
   anyoutf( dev, "     Rotation angle of output: %f (deg.)", HeaderPAout );


   strcpy( messbuf, "=============================================================================" );
   anyoutf( dev, messbuf );
}



void getaxisinfo( void )
/*----------------------------------------------------------------------*/
/* Get properties of primary and secondary axes: CDELT, CRVAL, CRPIX,   */
/* CUNIT, CTYPE etc. Display only when Hermes in test mode (dev=16).    */
/*----------------------------------------------------------------------*/
{
   int      m;
   fint     R1;
   fint     dev = TEST;
   

   anyoutf( dev, "====================== AXIS INFO =========================");
   /* Axis numbers in the original set are numbered from 1 to n */
   for (m = 0; m < (int) Setdim; m++) 
   {
      if (m < (int) Subdim) 
      {
         sprintf( messbuf, "Axis number %d has length: %d", m + 1, Axcount[m] );
         anyoutf( dev, messbuf );
      }
      else 
      {
         sprintf( messbuf, "Axis number %d has %d subsets", m + 1, Axcount[m] );
         anyoutf( dev, messbuf );
      }



      (void) sprintf( messbuf, "CDELT%d", m + 1 );
      R1 = 0;
      gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, &Cdelt[m], &R1 );
      anyoutf( dev, "Grid spacing: %f", Cdelt[m] );
      (void) sprintf( messbuf, "DDELT%d", m + 1 );
      secondary[m] = false;
      R1 = 0;
      gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, &Ddelt[m], &R1 );
      if (R1 >= 0) 
      {
        secondary[m] = true;
        anyoutf( dev, "Secondary Grid spacing: %f", Ddelt[m] );
      }

      (void) sprintf( messbuf, "CROTA%d", m + 1 );
      R1 = 0;
      gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, &Crota[m], &R1 );
      anyoutf( dev, "Rotation angle: %f", Crota[m] );

      (void) sprintf( messbuf, "CRVAL%d", m + 1 );
      R1 = 0;
      gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, &Crval[m], &R1 );
      anyoutf( dev, "Reference physical value: %f", Crval[m] );
      (void) sprintf( messbuf, "DRVAL%d", m + 1 );
      R1 = 0;
      gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, &Drval[m], &R1 );
      if (R1 >= 0) 
      {
         anyoutf( dev, "Secondary Reference physical value: %f", Drval[m] );
      }

      (void) sprintf( messbuf, "CRPIX%d", m + 1 );
      R1 = 0;
      gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, &Crpix[m], &R1 );
      anyoutf( dev, "Reference pixel: %f", Crpix[m] );
      (void) sprintf( messbuf, "DRPIX%d", m + 1 );
      R1 = 0;
      gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, &Drpix[m], &R1 );
      if (R1 >= 0) 
      {
         anyoutf( dev, "Secondary Reference pixel: %f", Drpix[m] );
      }

      (void) sprintf( messbuf, "CTYPE%d", m + 1 );
      R1 = 0;
      gdsd_rchar_c( Setin, tofchar(messbuf), &Setlevel, Ctype[m], &R1 );
      anyoutf( dev, "Type: %.*s", nelc_c( Ctype[m] ), Ctype[m].a );
      (void) sprintf( messbuf, "DTYPE%d", m + 1 );
      R1 = 0;
      gdsd_rchar_c( Setin, tofchar(messbuf), &Setlevel, Dtype[m], &R1 );
      if (R1 >= 0) 
      {
         anyoutf( dev, "Secondary Type: %.*s", nelc_c( Dtype[m] ), Dtype[m].a );
      }

      (void) sprintf( messbuf, "CUNIT%d", m + 1 );
      R1 = 0;
      gdsd_rchar_c( Setin, tofchar(messbuf), &Setlevel, Cunit[m], &R1 );
      anyoutf( dev, "Units in header: %.*s",
                         nelc_c( Cunit[m] ), Cunit[m].a );

      (void)  sprintf( messbuf, "DUNIT%d", m + 1 );
      R1 = 0;
      gdsd_rchar_c( Setin, tofchar(messbuf), &Setlevel, Dunit[m], &R1 );
      if (R1 >= 0) 
      {
         anyoutf( dev, "Secondary Units: %.*s", nelc_c( Dunit[m] ), Dunit[m].a );
      }

   }
   anyoutf( dev, "=========================================================");
}



float getY( fint Subset, int x, int y )
/*----------------------------------------------------------------------*/
/* position 0 in the one dim. array is equivalent to position           */
/* Gridlo[0], Gridlo[1] in the two dim. subset. 'LenX' is global.       */
/*----------------------------------------------------------------------*/
{
   x -= GridloI[0];
   y -= GridloI[1];
   return( ImageI[y * LenX + x] );
}



static bool insideinset( double xx, double yy )
/*------------------------------------------------------------*/
/*------------------------------------------------------------*/
{
   double  x, y;
   /* floor(1.2)=1.0,  floor(-1.2)=-2.0 */
   x = floor( xx ); 
   y = floor( yy );
   if ( (int) x > GridhiI[0] ||
        (int) x < GridloI[0] ||
        (int) y > GridhiI[1] ||
        (int) y < GridloI[1] )
      return( NO );
   return( YES );                                 
}



float interpol( fint Subset, double xx, double yy )
/*----------------------------------------------------------------------*/
/* (xx, yy) is a coordinate pair corresponding to a fractional pixel    */
/* position in the input subset. If this position is outside the input, */
/* return blank. If it is inside, try to interpolate over the           */
/* neighbours.                                                          */
/*----------------------------------------------------------------------*/
{
   double  x, y;
   float   t, u;
   float   y1, y2, y3, y4;
   float   nom, denom;
   float   w1, w2, w3, w4;


   /* floor(1.2)=1.0,  floor(-1.2)=-2.0 */
   x = floor( xx ); y = floor( yy );
   if ( (int) x > GridhiI[0] ||
        (int) x < GridloI[0] ||
        (int) y > GridhiI[1] ||
        (int) y < GridloI[1] ) 
      return Blank;


   t = (float) (xx - x);
   u = (float) (yy - y);

   y1 = getY( Subset, x, y );
   if (y1 == Blank) 
   { 
      y1 = 0.0; 
      w1 = 0.0; 
   } 
   else 
      w1 = 1.0;

   if   (x+1 <= GridhiI[0]) 
   {
      y2 = getY( Subset, x+1, y );
      if (y2 == Blank) 
      { 
         y2 = 0.0; 
         w2 = 0; 
      } 
      else 
         w2 = 1.0;
   } 
   else 
   {
      y2 = 0.0; 
      w2 = 0;                           /* Outside frame */
   }

   if ( (x+1 <= GridhiI[0]) && (y+1 <= GridhiI[1]) ) 
   {
      y3 = getY( Subset, x+1, y+1 );
      if (y3 == Blank) 
      { 
         y3 = 0.0; 
         w3 = 0.0; 
      } 
      else 
         w3 = 1.0;
   } 
   else 
   {
      y3 = 0.0; 
      w3 = 0;                           /* Outside frame */
   }

   if (y+1 <= GridhiI[1]) 
   {
      y4 = getY( Subset, x, y+1 );
      if (y4 == Blank) 
      { 
         y4 = 0.0; 
         w4 = 0.0; 
      } 
      else 
         w4 = 1.0;
   } 
   else 
   {
      y4 = 0.0; 
      w4 = 0;                           /* Outside frame */
   }

   denom = (1-t)*(1-u)*w1 + t*(1-u)*w2 + t*u*w3 + (1-t)*u*w4;
   if (denom == 0.0) 
      return( Blank );
   nom = (1-t)*(1-u)*y1 + t*(1-u)*y2 + t*u*y3 + (1-t)*u*y4;
   return( nom / denom );
}



void cotranserror( fint Err )
/*----------------------------------------------------------------------*/
/* Print the cotrans error message and abort the program.               */
/*----------------------------------------------------------------------*/
{
   char    errmess[45];


   switch( (int) Err ) 
   {
      case 1:  strcpy( errmess, "unknown projection" );                  break;
      case 2:  strcpy( errmess, "unknown mode" );                        break;
      case 3:  strcpy( errmess, "CROTA2 = 90.0 for mode 1 and 2" );      break;
      case 4:  strcpy( errmess, "CDELT1 and/or CDELT2 equal to zero" );  break;
      case 5:  strcpy( errmess, "input sky system unknown" );            break;
      case 6:  strcpy( errmess, "output sky system unknown" );           break;
      case 7:  strcpy( errmess, "input and output sky system unknown" ); break;
      case 8:  strcpy( errmess, "skypro error" );                        break;
      case 9:  strcpy( errmess, "unknown velocity system" );             break;
      case 10: strcpy( errmess, "rest frequency less than or equal to zero" ); break;
      case 11: strcpy( errmess, "crval equal to zero" );                 break;
      case 12: strcpy( errmess, "cdelt equal to zero" );                 break;
      case 13: strcpy( errmess, "no matching axis pair found" );         break;
      case 14: strcpy( errmess, "incompatible sky systems" );            break;
      case 15: strcpy( errmess, "cannot do epoch transformations" );     break;
   }
   fatalerror( errmess );
}



static void createtable( double *Xmin, double *Xmax, 
                         double *Ymin, double *Ymax,
                         fint numslices )
/*------------------------------------------------------------*/
/* PURPOSE: Create a table with 4 columns containing start    */
/* end end grids for each slice.                              */
/*------------------------------------------------------------*/
{
   fchar   Tname;
   fint    rowstart = 1;
   fint    r1;

 
   fmake( Tname, ITEMLEN );
   (void) str2char( "SLICE", Tname );
   r1 = 0;
   gdsa_crecol_c( Setout, &Setlevel, Tname, tofchar("SLXMIN"), 
                  tofchar("DBLE"), 
                  tofchar("Start position in X of slice in grids"),
                  tofchar("GRIDS"),
                  &r1 );
   if (r1 < 0)
      displayGDSerror( 1, r1 );               
   r1 = 0;                         
   gdsa_wcdble_c( Setout, &Setlevel, Tname, tofchar("SLXMIN"),
                  Xmin, &rowstart, &numslices, &r1 );
   if (r1 < 0)
      displayGDSerror( 1, r1 );


   r1 = 0;
   gdsa_crecol_c( Setout, &Setlevel, Tname, tofchar("SLXMAX"),
                  tofchar("DBLE"),
                  tofchar("End position in X of slice in grids"),
                  tofchar("GRIDS"),
                  &r1 );
   if (r1 < 0)
      displayGDSerror( 1, r1 );         
   r1 = 0;
   gdsa_wcdble_c( Setout, &Setlevel, Tname, tofchar("SLXMAX"),
                  Xmax, &rowstart, &numslices, &r1 );
   if (r1 < 0)
      displayGDSerror( 1, r1 );
         

   r1 = 0;
   gdsa_crecol_c( Setout, &Setlevel, Tname, tofchar("SLYMIN"),
                  tofchar("DBLE"),
                  tofchar("Start position in Y of slice in grids"),
                  tofchar("GRIDS"),
                  &r1 );
   if (r1 < 0)
      displayGDSerror( 1, r1 ); 
   r1 = 0;
   gdsa_wcdble_c( Setout, &Setlevel, Tname, tofchar("SLYMIN"),
                  Ymin, &rowstart, &numslices, &r1 );
   if (r1 < 0)
      displayGDSerror( 1, r1 );
         

   r1 = 0;
   gdsa_crecol_c( Setout, &Setlevel, Tname, tofchar("SLYMAX"),
                  tofchar("DBLE"),
                  tofchar("End position in Y of slice in grids"),
                  tofchar("GRIDS"),
                  &r1 );
   if (r1 < 0)
      displayGDSerror( 1, r1 );
   r1 = 0;
   gdsa_wcdble_c( Setout, &Setlevel, Tname, tofchar("SLYMAX"),
                  Ymax, &rowstart, &numslices, &r1 );
   if (r1 < 0)
      displayGDSerror( 1, r1 );      
}



MAIN_PROGRAM_ENTRY
/*----------------------------------------------------------------------*/
/* Because Fortran passes all arguments by reference, all C functions   */
/* with a Fortran equivalent must do this also (GIPSY programmers guide,*/
/* Chapter 9). Variables that can be interchanged between Fortran and C */
/* all start with a capital.                                            */
/*----------------------------------------------------------------------*/
{
   fint       Scrnum = TERM;
   fint       Direction = 1;
   double     Dummypos[AXESMAX];
   int        m;
   
   
   init_c();                              /* contact Hermes */
   /* Task identification */
   {
      fchar  Task;                        /* Name of current task */
      fmake( Task, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Task );                   /* Get task name */
      Task.a[nelc_c(Task)] = '\0';        /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, VERSION );  /* Show task and version */
   }      

   setfblank_c( &Blank );
   fmake(Setin, 80);
   Dfault  = NONE;
   Subdim  = 2;                             /* Dimension of subsets must be 2 */
   Class   = 1;
   fmake( key, 20 );
   fmake( mes, BIGSTORE );
   key = KEY_INSET;
   mes = MES_INSET;
   do 
   {
      Nsubsin = gdsinp_c( Setin,
                          Subin,
                          &Maxsubs,
                          &Dfault,
                          key,
                          mes,
                          &Scrnum,
                          Axnum,
                          Axcount,
                          &Maxaxes,
                          &Class,
                          &Subdim );
      /* Ask input until subset has two spatial axes with same units! */
      agreed = validaxes( Setin, Subdim, Axnum, &Projection, &HeaderPA );
      if (!agreed) 
         reject_c( key, tofchar("Try again") );
   } 
   while (!agreed);


   Setdim = gdsc_ndims_c( Setin, &Setlevel );
   for (m = 0; m < (int) Setdim; m++)
   {
      finit( Ctype[m], FITSLEN );
      finit( Dtype[m], FITSLEN );
      finit( Cunit[m], FITSLEN );
      finit( Dunit[m], FITSLEN );
   }


   getaxisinfo();                                      /* Get axis properties */


   /*-----------------------------------------------------*/
   /* Determine the edges of this its frame (GridloI/hiI) */
   /*-----------------------------------------------------*/
   R1 = 0;
   gdsc_range_c( Setin,
                 &Setlevel,
                 &CwloI,
                 &CwhiI,
                 &R1 );

   for (m = 0; m < (int) Setdim; m++) 
   {
      R1 = R2 = 0;
      GridloI[m] = gdsc_grid_c( Setin, &Axnum[m], &CwloI, &R1 );
      GridhiI[m] = gdsc_grid_c( Setin, &Axnum[m], &CwhiI, &R2 );
   }



   /*-------------------------------*/
   /* Get grid spacings from header */
   /*-------------------------------*/

   for (m = 0; m < (int) Subdim; m++) 
   {
      /* Grid increment of the output map */
      (void) sprintf( messbuf, "CDELT%d", Axnum[m] );       /* Note capitals in item */
      R1 = 0;
      gdsd_rdble_c( Setin,
                    tofchar(messbuf),
                    &Setlevel,
                    &Cdelt[m],
                    &R1 );
   }


   /*-------------------------------------------------------------*/
   /* Give central position in input subset and specify the angle */
   /* of the new plane.                                           */
   /* The number of the first output axis is given by the first   */
   /* element of the Axnum array. The first element of that array */
   /* number 0. But the axes numbers are counted 1, 2, ... etc.   */
   /* To get the correct index of a corresponding item, you have  */
   /* to subtract 1 to start with 0 again.                        */
   /*-------------------------------------------------------------*/

   ax1ind = (int) Axnum[0] - 1;
   ax2ind = (int) Axnum[1] - 1;
   ax3ind = (int) Axnum[2] - 1;
   
   crpixX = Crpix[ax1ind];
   offsetX = crpixX - floor(crpixX+0.5);
   crpixY = Crpix[ax2ind];
   offsetY = crpixY - floor(crpixY+0.5);


   Dfault = REQUEST;
   Maxpos = 1;
   do 
   {
      fint    Numpos;
      Position[0] = (double)GridloI[0] + Crpix[ax1ind] - 1.0;
      Position[1] = (double)GridloI[1] + Crpix[ax2ind] - 1.0;
      (void) sprintf( messbuf,
                     "Give central position:  [%d,%d]",
                     (int)Position[0],
                     (int)Position[1] );
      key = KEY_POSITION;
      mes = tofchar(messbuf);
      Numpos = gdspos_c( Position,          /* One position occupies 'Subdim' items. */
                         &Maxpos,           /* Maximum number of items to enter */
                         &Dfault,
                         key,
                         mes,
                         Setin,
                         &Subin[0] );

      agreed = ((Position[0] <= GridhiI[0]) && (Position[0] >= GridloI[0]) &&
                (Position[1] <= GridhiI[1]) && (Position[1] >= GridloI[1]) );
      if (!agreed) 
         reject_c( key, tofchar("Pos. outside input") );
   } 
   while(!agreed);
   
   R1 = cotrans_c( Setin,
                   &Subin[0],
                   Position,
                   Dummypos,
                   &Direction );

   if (R1 != 0)
      cotranserror( R1 );
   else 
   {
      Physpos[0] = Dummypos[ax1ind];
      Physpos[1] = Dummypos[ax2ind];
   }

   /*-----------------------------------------------------------*/
   /* The user is asked to give the angle between the North and */
   /* the object (f.i. galaxy) in the direction of the east.    */
   /* (If) CDELT in RA direction is negative, this means  rotate*/
   /* anti clockwise. The angle used for the projection trans-  */
   /* formation must be corrected by CROTA2.                    */
   /*-----------------------------------------------------------*/
   nitems = 1;
   Dfault = NONE;
   key = KEY_ANGLE;
   mes = MES_ANGLE;
   R1  = userdble_c( &HeaderPAout,
                     &nitems,
                     &Dfault,
                     key,
                     mes );


   Angle = (HeaderPAout - HeaderPA) - 90.0 ;
   Angle *= -1;
   Crota2out = Angle;

   /*-------------------------------------------------------------*/
   /* Ask user grid spacing and number of points for ROTATED axis */
   /*-------------------------------------------------------------*/
   GridoutRA = Cdelt[ax1ind];
   R1 = factor_c( Cunit[ax1ind], tofchar("ARCSEC"), &Convers1 );
   R2 = factor_c( tofchar("ARCSEC"), Cunit[ax1ind], &Convers2 );
   if ( (R1 == 0) && (R2 == 0) ) 
   {
      /* Conversion was possible */
      GridoutRA *= Convers1;
      (void) sprintf( messbuf,
               "New grid spacing of rotated axis:  [%g] arcsec",
                GridoutRA );                  /* New grid spacing spatial axis */
   }
   else 
   {
      /* No conversion to natural units */
      Convers1 = Convers2 = 1.0;
      (void) sprintf( messbuf,
               "New grid spacing of rotated axis:  [%g] %.*s",
                GridoutRA,                    /* New grid spacing spatial axis */
                nelc_c(Cunit[ax1ind]),
                Cunit[ax1ind].a );            /* in units of old spatial axis */
   }

   do 
   {
      Dfault    = REQUEST;
      GridoutRA = Cdelt[ax1ind] * Convers1;
      nitems    = 1;
      key       = KEY_GRIDOUT;
      mes       = tofchar(messbuf);
      R1        = userdble_c( &GridoutRA,
                              &nitems,
                              &Dfault,
                              key,
                              mes );
      agreed    = (GridoutRA != 0.0);
      if (!agreed) 
         reject_c( key, tofchar("Not allowed!") );
   } 
   while(!agreed);
   
   GridoutRA *= Convers2;                    /* Convert to original units */


   /*------------------------------------------------------*/
   /* Ask user for number of pixels he wants in the output */
   /* for one line                                         */
   /*------------------------------------------------------*/
   nitems    = 1;
   Dfault    = REQUEST;
   numpoints = Axcount[ax1ind];
   sprintf( messbuf,
           "Give number of pixels on rotated axis:  [%d]",
            Axcount[ax1ind] );
   do 
   {
      key       = KEY_POINTS;
      mes       = tofchar(messbuf);
      numpoints = Axcount[ax1ind];
      R1        = userint_c( &numpoints,
                             &nitems,
                             &Dfault,
                             key,
                             mes );
      agreed = ((numpoints > 0) && (numpoints < MAXPOINTS));
      if ( numpoints <= 0 ) 
      {
         (void) sprintf( messbuf, "Must be > 0" );
      }
      if ( numpoints > MAXPOINTS ) 
      {
         int maxp = MAXPOINTS;
         (void) sprintf( messbuf, "Must be < %d", maxp );
      }
     if (!agreed) 
        reject_c( key, tofchar( messbuf ) );
   } 
   while (!agreed);


   /*--------------------------------------------------------------*/
   /* Ask user for number of slices and grid spacing between them. */
   /* The default grid spacing is the grid spacing of the latitude */
   /* axis.                                                        */
   /*--------------------------------------------------------------*/
   nitems = 2;
   Dfault = REQUEST;
   do 
   {
      do 
      {
         key = KEY_SLICES;
         mes = MES_SLICES;
         NumslicesAB[0] = 0;
         NumslicesAB[1] = 0;
         R1 = userint_c( NumslicesAB,
                         &nitems,
                         &Dfault,
                         key,
                         mes );
         if (R1 == 1) 
         {
            NumslicesAB[1] = NumslicesAB[0];
         }
         agreed = ((NumslicesAB[0] >= 0) && (NumslicesAB[1] >= 0));
         if (!agreed) 
            reject_c( key, tofchar("Must be >= 0 !") );
      } 
      while (!agreed);
      agreed = ((NumslicesAB[0] + NumslicesAB[1] + 1) <= MAXSLICES);
      if (!agreed) 
      {
         int maxs = MAXSLICES;
         (void) sprintf( messbuf, "Must be < %d", maxs );
         reject_c( key, tofchar( messbuf ) );
      }
   } 
   while (!agreed);


   GridoutDEC = Cdelt[ax2ind];
   if ((NumslicesAB[0] > 0) || (NumslicesAB[1] > 0)) 
   /*---------------------------------------------------------------*/
   /* It is relevant now to ask for a grid spacing                  */
   /* The first element of 'NumslicesAB' contains the number        */
   /* of slices on a distance +1*'GridoutDEC', +2*'GridoutDEC' etc, */
   /* from the original slice. The second element is the number of  */
   /* slices on distance -1*'GridoutDEC' etc.                       */
   /*---------------------------------------------------------------*/
   {
      R1 = factor_c( Cunit[ax2ind], tofchar("ARCSEC"), &Convers1 );
      R2 = factor_c( tofchar("ARCSEC"), Cunit[ax2ind], &Convers2 );
      if ( (R1 == 0) && (R2 == 0) ) 
      {
         /* Conversion was possible */
         GridoutDEC *= Convers1;
         (void) sprintf( messbuf,
                 "Grid spacing between slices:  [%g] arcsec",
                  GridoutDEC );              /* New grid spacing spatial axis */
      }
      else 
      {
         /* No conversion could be made */
         Convers1 = Convers2  = 1.0;
         sprintf( messbuf,
              "Grid spacing between slices:  [%g] %.*s",
               GridoutDEC,                   /* New grid spacing spatial axis */
               nelc_c(Cunit[ax2ind]),
               Cunit[ax2ind].a );            /* in units of old spatial axis */
      }

      do 
      {
         Dfault = REQUEST;
         nitems = 1;
         GridoutDEC = Cdelt[ax2ind] * Convers1;
         key = KEY_SPACE;
         mes = tofchar(messbuf);
         R1 = userdble_c( &GridoutDEC,
                          &nitems,
                          &Dfault,
                          key,
                          mes );
         agreed = (GridoutDEC != 0.0);
         if (!agreed) 
            reject_c( key, tofchar("Not allowed!") );
      } 
      while(!agreed);
      GridoutDEC *= Convers2;                 /* Convert to original units */
   }


   /*----------------------------------------------------------*/
   /* There are enough parameters to create the output set now */
   /*----------------------------------------------------------*/
   /* Assign GDSINP buffer to GDSOUT buffer */
   Class = 1;
   gdsasn_c(KEY_INSET, KEY_OUTSET, &Class );

   /* Modify the size of the output set */
   GridhiO[0] = numpoints / 2;
   GridloO[0] = GridhiO[0] - numpoints + 1;
   GridloO[1] = -1 * NumslicesAB[0];
   GridhiO[1] = NumslicesAB[1];
   gdscss_c( KEY_OUTSET, GridloO, GridhiO );

   /* Change the coordinate system */
   /*  item        bit      add    */
   /*  CDELT        5        32    */
   /*  CROTA        4        16    */
   /*  CRPIX        3         8    */
   /*  CRVAL        2         4    */
   /*  CTYPE        1         2    */
   /*  CUNIT        0         1    */

   /* crpix is already changed by GDSCSS ! */
   { 
      double    dummydble;
      fchar     dummystr;
      fint      Axlen;
      fint      Pmask;
      fint      ax1, ax2;
      
      fmake( dummystr, 80 );
      Pmask = (32 + 4);
      ax1 = 1;
      gdscpa_c( KEY_OUTSET,
                &ax1,
                &numpoints,
                &GridoutRA,
                &dummydble,
                &dummydble, /*&Crpix1,*/
                &Physpos[0],
                dummystr,
                dummystr,
                &Pmask );

      Pmask = (32 + 16 + 4);

      ax2 = 2;

      Axlen = 1 + NumslicesAB[0] + NumslicesAB[1];
      gdscpa_c( KEY_OUTSET,
                &ax2, /* voorheen ax1ind */
                &Axlen,
                &GridoutDEC,
                &Crota2out,
                &dummydble, /*&Crpix2, */
                &Physpos[1],
                dummystr,
                dummystr,
                &Pmask );
   }


   fmake( Setout, BIGSTORE );

   Dfault = NONE;
   do 
   {
      Nsubsout = gdsout_c( Setout,     /* Name of set as output */
                           Subout,     /* Subsets as output */
                           &Nsubsin,   /* Max. number of allowed output subsets */
                           &Dfault,
                           KEY_OUTSET,
                           MES_OUTSET,
                           &Scrnum,
                           Axnumout,   /* Axis numbers for output */
                           Axcountout,
                           &Maxaxes );
      agreed = (Nsubsout == Nsubsin);
      if (!agreed) 
         reject_c( KEY_SPACE, tofchar("unequal num. of subsets!") );
   } 
   while (!agreed);

   printinfo();                         /* Print global input characteristics */


   /*-------------------------------------------------------------*/
   /* Now there are enough items to calculate the pixel positions */
   /* for the wanted rotated line in the original image:          */
   /* the rotation angle 'Angle', the grid spacing 'GridoutRA'    */
   /* and the central position 'Position[0]', 'Position[1]'       */
   /* Loop over de box in the output and store the positions in   */
   /* Xpos & Ypos.                                                */
   /*-------------------------------------------------------------*/
   {
      int       indx, indy;

      indy = 0;
      for (j = GridloO[1]; j <= GridhiO[1]; j++) 
      {
         indx = 0;
         for (i = GridloO[0]; i <= GridhiO[0]; i++ ) 
         {
#ifdef TESTEN            
            Xpos[indx][indy] = Physpos[0] + (double) i * GridoutRA;
            Ypos[indx][indy] = Physpos[1] + (double) j * GridoutDEC;
#endif
            Xpos[indx][indy] = (double) i;
            Ypos[indx][indy] = (double) j;
            indx++;
         }
         indy++;
      }
   }


   /*----------------------------------------------------------*/
   /* If a plot of the slices is wanted, initialize PGPLOT and */
   /* draw a box.                                              */
   /*----------------------------------------------------------*/
   pgplot = toflog( 0 );
   nitems = 1;
   Dfault = REQUEST;
   key    = KEY_PLOT;
   mes    = MES_PLOT;
   R1 = userlog_c( &pgplot,
                   &nitems,
                   &Dfault,
                   key,
                   mes );
   pgplot = tobool(pgplot);

   if (pgplot) 
   {
      fchar Xtitle, Ytitle, Toptitle;
      fint  len;
      float x, y;

      fmake( Xtitle,   BIGSTORE );
      fmake( Ytitle,   BIGSTORE );
      fmake( Toptitle, BIGSTORE);
      initplot();
      len = sprintf( Xtitle.a, "%-s", strtok( Ctype[ax1ind].a, " -" ) );
      Xtitle.l = len;
      len = sprintf( Ytitle.a, "%-s", strtok( Ctype[ax2ind].a, " -" ) );
      Ytitle.l = len;
      len = sprintf( Toptitle.a, "Angle of slice: %.2f deg.", HeaderPAout );
      Toptitle.l = len;
      drawbox( GridloI[0], GridloI[1], GridhiI[0], GridhiI[1],
               Xtitle, Ytitle, Toptitle );
      /* Draw axes through projection pixel */
      x = (float) GridloI[0];   y = (float) Position[1];
      pgmove_c( &x, &y );
      x = (float) GridhiI[0];   y = (float) Position[1];
      pgdraw_c( &x, &y );
      x = (float) Position[0];  y = (float) GridloI[1];
      pgmove_c( &x, &y );
      x = (float) Position[0];  y = (float) GridhiI[1];
      pgdraw_c( &x, &y );
   }


   /*---------------------------------------------------------*/
   /* Angle was the angle between the horizontal axis and     */
   /* the new rotated axis. 'proco' has 4 modes:              */
   /*                                                         */
   /*    MODE       XIN     YIN   XOUT   YOUT                 */
   /*    ====================================                 */
   /*      0        LON     LAT      X      Y                 */
   /*      1          X     LAT    LON      Y                 */
   /*      2        LON       Y      X    LAT                 */
   /*      3          X       Y    LON    LAT                 */
   /*                                                         */
   /*---------------------------------------------------------*/

   color = red;
   pgsci_c( &color );
   numslices = GridhiO[1] - GridloO[1] + 1;
   for (j = 0; j < numslices; j++) 
   {
      float    Xrot[MAXPOINTS];
      float    Yrot[MAXPOINTS];
      bool     init = NO;      

      Xmin[j] = Xmax[j] = Blank;
      for (i = 0; i < numpoints; i++) 
      {
         double    Zeroangle = 0.0;
         double    Xpr, Ypr;
         fint      Mode;
         /* Create rotated physical positions, at new spacing wrt. new map */
         double X;         
         double Y;

         /* The new horizontal axis is sampled. Usually for the central slice ranges */
         /* the sample positions in the rotated map are: */
         /* -N  0
             ....
            -2  0
            -1  0
             0  0
             1  0
             2  0
             ....
             N  0  */
         /* where N is half the number of samples. */
         /* If these positions enter proco() then they are grid distances to the */
         /* selected center (Physpos[0], Physpos[1]). So the result will be */
         /* a equidistant positions in world coordinates in the rotated map. */

         Mode = 3;
         R1 = proco_c( &Xpos[i][j],
                       &Ypos[i][j],
                       &Xposrot[i][j],
                       &Yposrot[i][j],
                       &Physpos[0],
                       &Physpos[1],
                       &GridoutRA,
                       &GridoutDEC,
                       &Angle,
                       &Projection,
                       &Mode );

         /* Back to pixels wrt. old map. Note that we got positions in world */
         /* coordinates in the un-rotated map, with respect to the rotation  */
         /* center given in CRVAL. These positions are relative to CRPIX.    */
         /* But we need the positions in the grid system that is centered on */
         /* the center of the grids. Then a correction for the offset needs  */
         /* to be applied. See also GETGRID() and SETGRID in cotrans.c       */

         Mode = 0;
         R1 = proco_c( &Xposrot[i][j],
                       &Yposrot[i][j],
                       &X,
                       &Y,
                       &Crval[ax1ind],
                       &Crval[ax2ind],
                       &Cdelt[ax1ind],
                       &Cdelt[ax2ind],
                       &Zeroangle,
                       &Projection,
                       &Mode );
                       
         Xposrot[i][j] = X + offsetX;
         Yposrot[i][j] = Y + offsetY;
         /* Keep track of positions of leftmost and rightmost pixel */
         Xpr = Xposrot[i][j];
         Ypr = Yposrot[i][j];
         if ( insideinset(Xpr, Ypr) )
         {
            if (!init)
            {
               init = YES;
               Xmax[j] = Xmin[j] = Xpr;
               Ymax[j] = Ymin[j] = Ypr;
            }
            else
            {
               if (Xpr < Xmin[j])
               {
                  Xmin[j] = Xpr;
                  Ymin[j] = Ypr;               
               }
               if (Xpr > Xmax[j])
               {
                  Xmax[j] = Xpr;
                  Ymax[j] = Ypr;
               }            
            }
         }

         if (pgplot) 
         {
            Xrot[i] = Xpr;
            Yrot[i] = Ypr;
         }
#ifdef TESTEN         
         anyoutf( 16, "%d %d: %f %f ==> %f %f", 
                  i,j,
                  Xpos[i][j], Ypos[i][j],
                  Xposrot[i][j], Yposrot[i][j] );
#endif
      }
      if (pgplot) 
      {
         if (j == NumslicesAB[0]) 
         {
            symbol++;
            color = green;
            pgsci_c( &color );
         }
         pgpt_c( &numpoints, Xrot, Yrot, &symbol );
         if (j == NumslicesAB[0]) 
         {
            symbol++;
            color = yellow;
            pgsci_c( &color );
         }
      }
      /* Print min/max pixel positions for each slice */
      anyoutf( 8, "Slice #%d starts at (%d,%d) and ends at (%d,%d)",
               j,
               (int)(Xmin[j]), 
               (int)(Ymin[j]),
               (int)(Xmax[j]), 
               (int)(Ymax[j]) );
   }


   /*-----------------------------------------------*/
   /* Create space for input data                   */
   /*-----------------------------------------------*/
   LenX = (GridhiI[0] - GridloI[0] + 1);
   LenY = (GridhiI[1] - GridloI[1] + 1);
   Bufheight = LenY;
   Buflen = LenX * Bufheight;
   do 
   {
      ImageI = (float *) calloc( (int) Buflen, sizeof(float) );
      if (ImageI == NULL) 
      {
         agreed = false;
         status_c(tofchar("Decreasing buffer size"));
         Bufheight /= 2;
         Buflen = LenX * Bufheight;
      }
      else 
         agreed = true;
   } 
   while( !agreed || (Bufheight < 2) );
   
   if (Bufheight < 3) 
   {
      anyoutf( 1, "Cannot allocate space for at least 3 lines of input!");
      fatalerror( "...Memory Allocation problems!..." );
   }

   if (Buflen < LenX*LenY ) 
   {
      /* This is not a final solution to the buffering */
      anyoutf( 1, "Cannot allocate enough memory");
      fatalerror( "...Memory Allocation problems!..." );
   }


   /*-----------------------------------------------*/
   /* Loop over all subsets specified in the input. */
   /*-----------------------------------------------*/
   for (subnrI = 0; subnrI < (int) Nsubsin; subnrI++) 
   {
      fint IndexO;                      /* Reset index of output array */
      CwloI = gdsc_fill_c( Setin, &Subin[subnrI], GridloI );
      CwhiI = gdsc_fill_c( Setin, &Subin[subnrI], GridhiI );
      TidI  = 0;
      gdsi_read_c( Setin,
                   &CwloI,
                   &CwhiI,
                   ImageI,
                   &Buflen,
                   &Pixelsdone,
                   &TidI );

      subnrO = subnrI;                              /* Indices are equivalent */
      TidO   = 0;                                       /* Reset transfer id. */
      IndexO = 0;
      for (j = 0; j < numslices; j++) 
      {
         for (i = 0; i < numpoints; i++) 
         {
            IndexO = j*numpoints + i;
            ImageO[IndexO] = interpol( Subin[subnrI],
                                       Xposrot[i][j],
                                       Yposrot[i][j] );
         }
      }
      /* Calculate (running) min, max & number of blanks of output */
      minmax3_c( ImageO,
                 &IndexO,
                 &Datamin[subnrI],
                 &Datamax[subnrI],
                 &Nblanks[subnrI],
                 &Mcount );

      Writepixels = numslices * numpoints;
      CwloO = gdsc_fill_c( Setout, &Subout[subnrO], GridloO );
      CwhiO = gdsc_fill_c( Setout, &Subout[subnrO], GridhiO );

      /* Write complete output image per subset */
      gdsi_write_c( Setout,
                    &CwloO,
                    &CwhiO,
                    ImageO,
                    &Writepixels,
                    &Writepixels,
                    &TidO );

      /* Give proceedings message */
      (void) sprintf( messbuf, "%d %% completed", 100*(subnrI+1)/Nsubsin );
      status_c( tofchar(messbuf) );
   }

   /*------------------------------------------------------------*/
   /* The start and end points in grids are written to a column  */
   /* in the output set. The column names are SLXMIN, SLXMAX,    */
   /* SLYMIN, SLYMAX.                                            */   
   /*------------------------------------------------------------*/
   createtable( Xmin, Xmax, Ymin, Ymax, numslices );
      

   /*------------------------------------------------------------*/
   /* Final update of output header. Minimum and maximum have    */
   /* changed and the MINMAX descriptors at intersecting levels  */
   /* will be removed by WMINMAX (Remove != 0).                  */
   /*------------------------------------------------------------*/
   {
      fint remove = 1;
      wminmax_c( Setout,
                 Subout,
                 Datamin,
                 Datamax,
                 Nblanks,
                 &Nsubsout,
                 &remove );
   }

   for (m = 0; m < (int) Setdim; m++)
   {
      free( Ctype[m].a );
      free( Dtype[m].a );
      free( Cunit[m].a );
      free( Dunit[m].a );
   }

   if (ImageI)
      free( ImageI );
   finis_c();                                                  /* Quit Hermes */
   return(EXIT_SUCCESS);
}
