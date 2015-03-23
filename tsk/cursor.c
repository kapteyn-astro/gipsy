/*
                            COPYRIGHT (c) 1995
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             cursor.dc1

Program:       CURSOR

Purpose:       Write positions (GRIDS and PHYSICAL coordinates), pointed 
               with an interactive graphics cursor in a GIDS overlay, to 
               screen or ASCII file. Write also (interpolated) image 
               values.

Category:      UTILITY

File:          cursor.c

Author:        M.G.R. Vogelaar

Keywords:


   INSET=      Give input set (, subsets):

               If there is not a set displayed on GIDS then you are 
               prompted with this keyword. The task VIEW will be 
               spawned to display the map.
               Maximum number of subsets is 2048.

   
   BOX=        Give box in .....                        [entire subset]
                               

 **COLOUR=     Marker colour (number or abbrv. name):             [red]   
   
               The positions pointed by the graphics cursor will
               be marked in this colour. The colour is entered as
               a number between 1 and 15 (see notes) or as a string
               which represent a colour. The colours are listed in 
               the description. Colour strings can be abbreviated.
               Example: Set marker colour to yellow:
               COLOUR=7
               COLOUR=Yellow
               COLOUR=yel
   
  
   FILENAME=   Give name of ASCII file:                       [No file]
   
               Write positions to a file on disk or screen. If a 
               transformation to physical coordinates is possible, 
               write these coordinates also.
               
              
   OVERWRITE=  File exists, ok to overwrite?                      [Y]/N
               
               Only prompted if FILENAME= is an existing file. 
              
             

Description:   Program CURSOR writes an ASCII table with positions 
               pointed with a graphics cursor in GIDS where a (sub)set 
               is loaded. If nothing is loaded or GIDS is not available
               you are prompted with the INSET= and the BOX= keywords.
               Then application VIEW is spawned and (after VIEWs'
               CLIP= keyword) a (sub)set is loaded. A simple frame
               is plotted and the graphics cursor appears. 

               With the cursor it is possible to select positions with
               the left button or any keyboard key except 'Q'/'q'. The 
               cursor loop is aborted if the right mouse button or the 
               keyboard key 'Q' (or 'q') is pressed.
               
               Default, the entered positions are written to screen, 
               but if you give a name at the FILENAME= prompt,
               the data is written to an ASCII file on disk. 
               
               The first column in the file (or on screen) is a grid 
               position in X direction, the second is a grid position 
               in Y direction. If a transformation to header units is 
               possible, then also these transformed physical coordinates 
               are written where both columns are prefixed with a 'U' to 
               indicate that the units are header units (in most cases: 
               DEGREE). Between grid- and physical coordinates is a column
               with the interpolated image value at the listed position.
               Image data is taken from the input set at the four integer 
               neighbouring positions of an input position. If all
               positions are within the input box (BOX=) and all image 
               data is not blank then a bilinear interpolation is applied. 
               For three valid neighbours, a plane is constructed to do 
               the interpolation and for two positions, the interpolation
               is linear. 
               Example of the output:
   
               User              : M.G.R. Vogelaar 
               Date of creation  : Thu Jan  4 16:51:31 1996
               X-grid     Y-grid      Image        X-physical     Y-physical
               =================================================================
               -18.44     -19.94      +0.76    U  +158.346481 U   -39.386364
               -12.19     -19.56      -0.00    U  +158.312785 U   -39.384825
                -5.31     -24.81      +0.36    U  +158.275732 U   -39.406715
                etc.
                
               The prefix 'U' indicates that the coordinates are expressed
               in units as found in the header of INSET=.
                      
                             
               The colours that can be used are:

               index  name
               =============================================
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
               
               
Notes:         If you started GIDS on another machine than the one on 
               which you run CURSOR, then cursor actions can be very slow!

Example:       .......

Updates:       Feb  8,  1995: VOG, Document created.
               Jul 27,  2000: VOG, Changed local 'getipval' to 
                                   library 'interpol'.

#<
*/

/*  cursor.c: include files     */

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
                             /* 0 use default [set by HERMES to 3] */
                             /* 1  terminal                        */
                             /* 2  LOG file                        */
                             /* 8  terminal, suppressed in "experienced mode" */
                             /* 16  terminal, only when in "test mode". */
#include    "status.h"       /* Display info in the "RUNNING" status display */ 
#include    "userfio.h"      /* Easy-C companions for GIPSY user interface routines */
#include    "setfblank.h"    /* Function to set a data value to the universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "matrix.h"       /* Read data in M[y1..yn][x1..xn] format */

/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"      
#include    "userreal.h"     
#include    "userdble.h"     
#include    "usertext.h"     
#include    "userchar.h"    
#include    "usercharu.h"
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/
#include    "dcdint.h"       /* Decodes a string of characters into integers. */


/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gds_exist.h"    /* Test whether set exists. */
#include    "gds_delete.h"   /* Delete set. */
#include    "gds_create.h"   /* Create a new set. */
#include    "gds_extend.h"   /* Create or extend an axis. */
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_axnum.h"   /* Return axis number of a specified axis */
#include    "gdsc_name.h"    /* Return the name of an axis. */
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid. */
                             /* value for each axis. */


/* Data IO */

#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "gdsi_write.h"   /* Writes data to (part of) an set. */

         
/* Gids related */

#include    "gdi_iinfo.h"    /* Obtains info about GDS image loaded in DISPLAY SERVER. */
#include    "gdi_open.h"     /* Opens a display. */
#include    "gdi_open2.h"    /* Opens a d. only if the d. server is already running. */
#include    "gdi_frame.h"    /* Obtains info about frame currently on display. */
#include    "gdi_close.h"    /* Closes an opened display device. */


/* PGPLOT includes */

#include    "pgplot.h"


/* Miscellaneous */

#include    "cotrans.h"      /* Transformation from grid coordinates to */
                             /* physical coordinates and vice versa. */
#include    "wkey.h"         /* Write keywords to task's own parameter list */
#include    "deputy.h"       /* Start a task which temporarily assumes the */
                             /* role of the calling task. */
#include    "getusernam.h"   /* Returns the user name of the current user. */
#include    "getdate.h"      /* Returns the current time and date as a text string. */
#include    "wmatch.h"       /* Matches a test string with mask which */
                             /* contains wildcards. */
#include    "interpol.h"

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

/* Malloc version of 'fmake', but now space must be freed */
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }  


/* Miscellaneous defines */

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )
#define PI             3.141592653589793
#define RAD(a)         ( (a) * 0.017453292519943295769237 )
#define DEG(a)         ( (a) * 57.295779513082320876798155 )

#define RELEASE        "1.0"           /* Version number */
#define MAXAXES        10              /* Max. axes in a set */
#define MAXSUBSETS     2048            /* Max. allowed subsets */
#define STRLEN         120             /* Max length of strings */
#define FITSLEN        20    
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0


/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_FILENAME   tofchar("FILENAME=")
#define KEY_OVERWRITE  tofchar("OVERWRITE=")


/* Define for deputy tasks */

#define TSK_VIEW       tofchar("view")


/* Colours: */         

#define   BACKGROUND    0      /* Color definitions for PGPLOT. */
#define   FOREGROUND    1      /* Black if background is white. */
#define   RED           2
#define   GREEN         3
#define   BLUE          4
#define   CYAN          5
#define   MAGENTA       6
#define   YELLOW        7
#define   ORANGE        8
#define   GREENYELLOW   9
#define   GREENCYAN    10
#define   BLUECYAN     11
#define   BLUEMAGENTA  12
#define   REDMAGENTA   13
#define   DARKGRAY     14
#define   LIGHTGRAY    15


/* Variables for input */

static fint     subin[MAXSUBSETS];  /* Subset coordinate words */

static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered ccording to the */
                                    /* specification by the user. */
static fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
                                    /* number of grids along an axes as */
                                    /* specified by the user. The first elements */
                                    /* (upto the dimension of the subset) contain */
                                    /* the length of the subset axes, the other */
                                    /* ones contain the the number of grids along */
                                    /* an axes outside the subset. */
static fint     maxsubs = MAXSUBSETS;
static fint     maxaxes = MAXAXES;  /* Max num. of axes the program can deal with.*/


/* Box and frame related */

static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */


/* Reading data */

static float    **image = NULL;     /* 2-dim matrix with floats */



/* PGPLOT variables */

static int   pgopen = NO;


/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static char     message[STRLEN];    /* All purpose character buffer. */



static void errorC( int level, 
                    char *str )
/*------------------------------------------------------------------*/
/* The C version of 'error'.                                        */
/*------------------------------------------------------------------*/
{
   fint   flev = (fint) level;
   error_c( &flev, tofchar( str ) );
}



static int wmatchC( char *test, char *mask )
/*------------------------------------------------------------*/
/* PURPOSE: Matches a test string with the mask string which  */
/* contains wildcards. wmatchC returns non-zero if matched,   */
/* zero if not. It is set to case insensitive and the wild-   */
/* card character is '*'.                                     */
/*------------------------------------------------------------*/
{
      fint  casesensitive = NO;
         
      return( wmatch_c(tofchar(test),    /* String to test agains MASK. */
                       tofchar(mask),    /* String which contains the massk */
                       tofchar("*"),     /* Character which represents wildcard */
                       &casesensitive) );/* If zero, matching is case insensitive */
}



static void setcol( fint color )
/*------------------------------------------------------------------*/
/* The C version of pgsci_c.                                        */
/*------------------------------------------------------------------*/
{
   pgsci_c( &color );
}



static fint askcolor( void )
/*------------------------------------------------------------------*/
/* PURPOSE: Get colour from user. COLOUR= can be either a number    */
/*          between 1 and 15 or a (abbreviated) string.             */
/*------------------------------------------------------------------*/
{
   char   *mess = NULL;
   int    len;
   int    i;
   fint   one = 1;
   fint   err;
   fint   dfault, nitems;
   fint   r1;
   fint   color;
   fchar  Color;
  
 

   fmake( Color, 16 );
   dfault = HIDDEN;
   nitems = 1;
   r1 = usercharu_c( Color, &nitems, &dfault, 
                     tofchar("COLOUR="),
                     tofchar("Marker colour (number or abbrv. name):    [red]") );
                     
   if (!r1)                              /* The default */
      return( RED );


   /* Try to convert input */
   
   r1 = dcdint_c( Color, &color, &one, &err );
   if (r1 == 1)
   {
      if (color > 15)
         color = 1;
      if (color < 0)
         color = 0;
      return( color );
   }


   /* No default, no number, perhaps a string? */
   
   len  = nelc_c( Color );
   mess = malloc( len + 2 );          
   for (i = 0; i < len; i++)           /* Copy */
     mess[i] = Color.a[i];
   mess[len] = '*';                    /* Add wildcard */
   mess[len+1] = '\0';

   color = 1;                          /* Initialize */
   if      (wmatchC("background",  mess) )
      color = 0;
   else if (wmatchC("black",       mess) )
      color = 0;      
   else if (wmatchC("default",     mess) )
      color = 1;
   else if (wmatchC("red",         mess) )
      color = 2;
   else if (wmatchC("green",       mess) )
      color = 3;
   else if (wmatchC("blue",        mess) )
      color = 4;
   else if (wmatchC("cyan",        mess) )
      color = 5;
   else if (wmatchC("magenta",     mess) )
      color = 6;
   else if (wmatchC("yellow",      mess) )
      color = 7;
   else if (wmatchC("orange",      mess) )
      color = 8;
   else if (wmatchC("greenyellow", mess) )
      color = 9;
   else if (wmatchC("greencyan",   mess) )
      color = 10;
   else if (wmatchC("bluecyan",    mess) )
      color = 11;
   else if (wmatchC("bluemagenta", mess) )
      color = 12;
   else if (wmatchC("redmagenta",  mess) )
      color = 13;
   else if (wmatchC("darkgray",    mess) )
      color = 14;
   else if (wmatchC("LightGray",   mess) )
      color = 15;
   
   free( mess );
   return( color );
}



static int initplot( fint  *GIDSblo, 
                     fint  *GIDSbhi,
                     float *GIDSflo, 
                     float *GIDSfhi )
/*------------------------------------------------------------------*/
/* PURPOSE: Initialize plot software. Set viewport and output       */
/*          dimensions.                                             */
/* INPUT:   GIDSblo, GIDSbhi, GIDSflo, GIDSfhi                      */
/* Start PGPLOT, open output device. A return value of 1 indicates  */
/* successful completion. There are 4 arguments for PGBEG:          */
/* UNIT, this argument is ignored by PGBEG (use zero).              */
/* FILE, If this argument is a question mark PGBEG will prompt the  */
/*       user to supply a string.                                   */
/* NXSUB,the number of subdivisions of the view surface in X.       */
/* NYSUB,the number of subdivisions of the view surface in Y.       */
/*------------------------------------------------------------------*/
{
   fint   unit;                  /* Ignored by pgbeg, use unit=0. */
   fchar  Devspec;               /* Device specification. */
   fint   nxysub[2];             /* Number of subdivisions on 1 page. */
   fint   r1;
   int    i;
   bool   pageoff;               /* Disable PGPLOT's NEXTPAGE keyword. */
   float  xl, xr, yb, yt;        /* Edges of the viewport. */
   float  Gblo[2], Gbhi[2];

   
   nxysub[1] = nxysub[0] = 1;    /* Default no subdivisions in plot.*/
   unit      = 0;
   Devspec   = tofchar("gids//append");
   r1        = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );
   if (r1 != 1) 
   {
      anyoutf( 1, "Cannot open output device" );
      return( NO );
   }

   /* No PGPLOT's NEXTPAGE= keyword */
   pageoff = toflog( NO );
   pgask_c( &pageoff );

   /*--------------------------------------------------------------*/
   /* If a displayed set is zoomed, the box will keep              */
   /* its original values and the frame is adjusted. To keep a box */
   /* within the frame, adjust the box values.                     */
   /*--------------------------------------------------------------*/         
   for (i = 0; i < 2; i++)   
   {
      Gblo[i] = (float) GIDSblo[i];
      Gbhi[i] = (float) GIDSbhi[i];
      
      
      if (Gblo[i] < GIDSflo[i])
         Gblo[i] = (float) ( (int) GIDSflo[i] );
      if (Gbhi[i] > GIDSfhi[i])
         Gbhi[i] = (float) ( (int) GIDSfhi[i] );      
   }
   
   xl = (Gblo[0] - GIDSflo[0]) / (GIDSfhi[0] - GIDSflo[0]);
   xr = (Gbhi[0] - GIDSflo[0]) / (GIDSfhi[0] - GIDSflo[0]);
   yb = (Gblo[1] - GIDSflo[1]) / (GIDSfhi[1] - GIDSflo[1]);
   yt = (Gbhi[1] - GIDSflo[1]) / (GIDSfhi[1] - GIDSflo[1]);   
   pgsvp_c( &xl, &xr, &yb, &yt );                       /* Set viewport */
   pgswin_c( &Gblo[0], &Gbhi[0], &Gblo[1], &Gbhi[1]);   /* Set the window */
   return( YES );
}



static int getGIDSset( fchar Setin,
                       fint  *subset,
                       fint  *GIDSblo, 
                       fint  *GIDSbhi,
                       float *GIDSflo, 
                       float *GIDSfhi )   
/*------------------------------------------------------------*/
/* INPUT:   Setin                                             */
/* OUTPUT:  GIDSblo, GIDSbhi, GIDSflo, GIDSfhi                */
/* PURPOSE: Check wheter an overlay in GIDS can be made. I.e. */
/*          1) Gids must be started, 2) An image must be load-*/
/*          ed and 3) The name of the displayed set must match*/
/*          'Setin'. If so, return 1 and the sizes of the     */
/*          displayed box and the GIDS frame, Otherwise return*/
/*          0 as result of the function.                      */
/*------------------------------------------------------------*/
{
   fint         display_stat;                   /* display operation status */
   fint         setdim;
   fint         iax;
   fint         r1, r2;
   fint         grid;
   fchar        Axisname;
   fint         GIDSdisplay_id = -1;            /* id of display */
   fchar        GIDSset;
   fint         GIDSsubset;
   int          i;


   /* If not available, do NOT start GIDS */
   GIDSdisplay_id = gdi_open2_c( tofchar(" ") );    

   if (GIDSdisplay_id < 0)                      /* error opening display */
   {
      anyoutf( 1, "<WARNING> GIDS not started!" );
      return( 0 );
   }
   fmake( GIDSset, 128 );
   display_stat = gdi_iinfo_c( &GIDSdisplay_id, /* id of display */
                               GIDSset,         /* name of set */
                               &GIDSsubset,     /* subset level */
                               GIDSblo,         /* lower left frame boundary */
                               GIDSbhi );       /* upper right frame boundary */

   if (display_stat < 0)                        /* error obtaining info */
   {
      anyoutf( 1,  "<WARNING> No image loaded in GIDS!");
      return( 0 );
   }

   if (gdsc_ndims_c( GIDSset, &GIDSsubset ) != 2)
   {
      anyoutf( 1,  "<WARNING> Wrong dimension of set in GIDS!");
      return( 0 );
   }

   r1 = gdi_frame_c( &GIDSdisplay_id ,          /* id of display */
                     GIDSflo,                   /* lower left frame boundary */
                     GIDSfhi );                 /* .. in (floating) grids */

   if (r1 != 0)
   {
      (void) sprintf( message,
                     "Cannot obtain info about frame currently on display! (err=%d)", r1 );
      anyoutf( 1,  message );
      return( 0 );
   }
   display_stat = gdi_close_c( &GIDSdisplay_id ); /* close display */
   

   /* Some information about displayed set: */

   anyoutf( 8, "============== GIDS info ==============" );
   (void) sprintf( message, "%.*s", nelc_c(GIDSset), GIDSset.a );
   setdim = gdsc_ndims_c( GIDSset, &setlevel );
   fmake( Axisname, FITSLEN );
   
   /* Append non subset axis names to set name */
   for (iax = 1; iax <= setdim; iax++)
   {
      r1 = 0;
      grid = gdsc_grid_c( GIDSset, &iax, &GIDSsubset, &r1 );
      if (r1 >= 0)
      {
         r2 = 0;
         gdsc_name_c( Axisname, GIDSset, &iax, &r2 );
         Axisname.a[nelc_c(Axisname)] = '\0';
         strcat( message, " " );
         strcat( message, Axisname.a );
      }
   }
   anyoutf( 8,  message );
   for (i = 0; i < strlen( message ); i++)
      Setin.a[i] = message[i]; 
   (void) sprintf( message,
                  "GIDS box:   [%d %d %d %d]",
                   GIDSblo[0], GIDSblo[1], GIDSbhi[0], GIDSbhi[1] );
   anyoutf( 8,  message );
   (void) sprintf( message,
                  "Gids frame: [%g %g %g %g]", 
                   GIDSflo[0], GIDSflo[1], GIDSfhi[0], GIDSfhi[1] );
   anyoutf( 16, message );
   anyoutf( 8,  " " );

   *subset = GIDSsubset;
   return( 1 );
}



static void drawframe( fint   *blo, 
                       fint   *bhi,
                       double originX, 
                       double originY )
/*------------------------------------------------------------------*/
/* PURPOSE: Draw a simple labeled box around viewport and draw      */
/*          coordinate axes. If necessary, plot the direction of    */
/*          the north with an arrow.                                */
/*------------------------------------------------------------------*/
{
   fint    nxsub, nysub;
   float   xtick, ytick;
   float   x, y;

   xtick = ytick = 0.0;
   setcol( FOREGROUND );
   nxsub = nysub = 0;
   pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
            tofchar("BCNSTV"), &ytick, &nysub );
   setcol( RED );
   x = (float) originX;
   y = (float) blo[1];
   pgmove_c( &x, &y );
   y = (float) bhi[1];
   pgdraw_c( &x, &y );
   x = (float) blo[0];
   y = (float) originY;
   pgmove_c( &x, &y );
   x = (float) bhi[0];
   pgdraw_c( &x, &y );
}   

         


static FILE *getfileptr( void )
/*------------------------------------------------------------*/
/* PURPOSE: Ask user name of an ASCII file.                   */
/*                                                            */
/* Check whether the file already exists. If so, Ask for      */
/* permission to overwrite. Return a file pointer. This file  */
/* pointer is NULL if no file is wanted.                      */
/*------------------------------------------------------------*/
{
   FILE   *fp = NULL;
   fint   dfault, nitems;
   fint   r1;
   int    agreed;
   bool   overwrite;
   fchar  Filename;

   
   fmake( Filename, 512 );
   do 
   {
      dfault    = REQUEST;
      overwrite = toflog(YES);
      nitems    = 1;
      r1 = userchar_c( Filename,
                       &nitems,      
                       &dfault, 
                       KEY_FILENAME, 
                       tofchar("Give name of ASCII file:     [No file]") );

      if (!r1)
      {
         fp = NULL;            
         agreed = YES;
      } 
      else 
      {
         Filename.a[nelc_c(Filename)] = '\0';
         fp = fopen( Filename.a, "r" );
         if (fp != NULL)                       /* The file exists */
         {
            nitems = 1;
            dfault = REQUEST;
            r1 = userlog_c( &overwrite, 
                            &nitems,
                            &dfault,
                            KEY_OVERWRITE,
                            tofchar("File exists, ok to overwrite?    [Y]/N") );

            fclose( fp );
            cancel_c( KEY_OVERWRITE );
         }
         if (!overwrite) 
         {
            cancel_c( KEY_FILENAME );
            agreed = NO;
         }
         else 
         {
            fp = fopen( Filename.a, "w" );
            agreed = (fp != NULL);
            if (!agreed) 
               reject_c( KEY_FILENAME, tofchar("Cannot open, try another!") );
         }
      }
   } 
   while (!agreed);
   return( fp );  
}



static bool tophys( fchar  Setin,
                    fint   subset,
                    fint   *axnum,
                    fint   *blo,
                    fint   *bhi,
                    float  x,
                    float  y,
                    float  *imval,
                    double *xphys,
                    double *yphys )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate physical coordinates and get interpolat-*/
/*          ed image value.                                   */
/*------------------------------------------------------------*/
{
   double   phys[MAXAXES];
   double   grid[2];
   fint     grid2phys = 1;
   bool     trans;
  
 
   /* *imval = getipval( x, y, blo, bhi ); */
   *imval = interpol( x, y, image, blo, bhi, blank );
    
  
   grid[0] = (double) x;
   grid[1] = (double) y;
   trans = cotrans_c( Setin, &subset, grid, phys, &grid2phys );
   *xphys = phys[axnum[0]-1];
   *yphys = phys[axnum[1]-1];
   return( (trans == 0) );
}                       



static void writevalues( FILE  *fp,
                         float  x, 
                         float  y,
                         float  imval,
                         bool   physical, 
                         double xphys, 
                         double yphys,
                         fint   dev )
/*------------------------------------------------------------*/
/* PURPOSE: Write grid- and physical positions to screen or   */
/*          file (fp != NULL)                                 */
/*------------------------------------------------------------*/
{
   static int first = YES;
   char   mess2[128];
   
   if (first)
   {
      int     len;
      fchar   Idstr, Datestr;
      
      fmake( Idstr, 132 );
      fmake( Datestr, 132 );
      getusernam_c( Idstr );
      getdate_c( Datestr );
      (void) sprintf( message, "%10.10s %10.10s %10.10s    ",
                      "X-grid", "Y-grid", "Image" );
      if (physical)
      {
         (void) sprintf( mess2, "%14.14s %14.14s", "X-physical", "Y-physical" );
         strcat( message, mess2 );
      }
      len = strlen( message );
      memset( mess2, '=', len );
      mess2[len] = '\0';
      if (fp != NULL)
      {
         (void) fprintf( fp,"User              : %.*s\n", nelc_c(Idstr), Idstr.a );
         (void) fprintf( fp,"Date of creation  : %.*s\n", nelc_c(Datestr), Datestr.a );
         (void) fprintf( fp, message ); 
         (void) fprintf( fp, "\n" );
         (void) fprintf( fp, mess2 );
         (void) fprintf( fp, "\n" );         
      }
      else
      {
         anyoutf( dev, " " );
         anyoutf( dev, "User              : %.*s", nelc_c(Idstr), Idstr.a );
         anyoutf( dev, "Date of creation  : %.*s", nelc_c(Datestr), Datestr.a );
         anyoutf( dev, message );
         anyoutf( dev, mess2 );
      }              
      first = NO;       
   } 
        
   (void) sprintf( message, "%+10.2f %+10.2f ", x, y );
   if (imval != blank)
   {
      (void) sprintf( mess2, "%+10.2f    ", imval );
      strcat( message, mess2 );
   }
   else
   {
      (void) sprintf( mess2, "%10.10s    ", "blank" );
      strcat( message, mess2 );
   }

   if (physical)                           /* Add physical coordinates */
   {
      (void) sprintf( mess2, "U %+12f U %+12f", xphys, yphys );
      strcat( message, mess2 );
   }
   if (fp != NULL)
      (void) fprintf( fp, "%s\n", message );
   else
      anyoutf( dev, message );
}





static void printstatus( fchar Tsk, fint status )
/*------------------------------------------------------------*/
/* PURPOSE: Display error for deputy call.                    */
/*------------------------------------------------------------*/
{
   if (status == -6) 
      anyoutf( 1, "Called task (%.*s) not present", nelc_c(Tsk), Tsk.a );
   if (status == -7) 
      anyoutf( 1, "Max. number of tasks already active" );            
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint      dfault;                       /* Used in userxxx_c functions */
   fint      dev;                          /* Anyout destination */
   float     imval;                        /* Image value at certain pos. */
   float     xy[2];                        /* Position from 'pgcurs' */
   float     GIDSflo[2], GIDSfhi[2];       /* GIDS frame */
   fint      GIDSblo[2], GIDSbhi[2];       /* GIDS box */   
   fint      samples;                      /* Number of sample positions */
   fint      imagesize;                    /* Size of input subsets */
   fint      r1;                           /* Function results */
   fint      subdim;                       /* Dimensionality of the subsets for class 1 applications */
   fint      setdim;                       /* Dimension of set */
   fint      subset;                       /* A subset coord. word */
   fint      markcolor;
   int       i;                            /* Counter */
   fchar     Setin, Setdum;                /* Input set and a dummy name */
   fchar     Ch;                           /* Character for 'pgcurs' */
   char      ch;
   FILE      *fp = NULL;                   /* File pointer to ASCII file */
   


   init_c();                               /* Contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );
   
   fmake( Setin,  STRLEN );
   fmake( Setdum, STRLEN );   
   /*----------------------------------------------------------------*/
   /* If GIDS loaded a (sub)set then this set is the input set for   */
   /* CURSOR. In any other case, CURSOR will prompt with the INSET=  */
   /* keyword to get a set name. In both cases the name will be      */
   /* input for 'gdsinp' so that the 'axnum' array can be filled.    */
   /*----------------------------------------------------------------*/   
   if ( getGIDSset( Setin, 
                    &subset, 
                    GIDSblo, 
                    GIDSbhi, 
                    GIDSflo, 
                    GIDSfhi) )
   {
      pgopen = initplot( GIDSblo, 
                         GIDSbhi, 
                         GIDSflo, 
                         GIDSfhi );
      if (!pgopen)                         /* Abort task */
         errorC( 4, "Cannot initialize pgplot!" );     
      /*---------------------------------------------------*/
      /* There was a set displayed. Let 'gdsinp' check     */
      /* set parameters. Create a keyword INSET= and write */
      /* the keyword in the parameter list of the task.    */
      /*---------------------------------------------------*/      
      (void) sprintf( message, "INSET=%.*s", nelc_c(Setin), Setin.a );
      wkey_c( tofchar(message) );
   }
   /*--------------------------*/   
   /* Get set from user.       */
   /*--------------------------*/   
   {
      fint     class  = 1;         
      fint     nsubs;

      dfault = NONE;
      subdim = 2;                    /* Subset dimension MUST be 2! */      
      if (pgopen)
         dev = 16;
      else
         dev = 3;
      
      nsubs  = gdsinp_c( Setin,      /* Name of input set. */
                         subin,      /* Array containing subsets coordinate words. */
                         &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                         &dfault,    /* Default code as is USERxxx. */
                         KEY_INSET,  /* Keyword prompt. */
                         MES_INSET,  /* Keyword message for the user. */
                         &dev,       /* Device number (as in ANYOUT). */
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
                         &subdim );  /* Dimensionality of subset(s). */
      setdim = gdsc_ndims_c( Setin, &setlevel );
   }
 

   if (!pgopen)
   {
      fint boxopt = 0;
      dev     = 1;
      dfault  = REQUEST;
      gdsbox_c( blo, 
                bhi, 
                Setin, 
                subin, 
                &dfault,
                tofchar("BOX="), 
                tofchar(" "), 
                &dev, 
                &boxopt );
   
      /*---------------------------------------------------*/
      /* Start VIEW. The keyword INSET= is not cancelled,  */
      /* so VIEW starts with displaying all subsets until  */
      /* NEXT=NO is entered.                               */
      /*---------------------------------------------------*/            
      {         
         fint status; 
         deputy_c( TSK_VIEW, &status );
         if (status != 1) 
            printstatus( TSK_VIEW, status );
         else
            if ( getGIDSset( Setdum, 
                             &subset, 
                             GIDSblo, 
                             GIDSbhi, 
                             GIDSflo, 
                             GIDSfhi) )   
               pgopen = initplot( GIDSblo, 
                                  GIDSbhi, 
                                  GIDSflo, 
                                  GIDSfhi );
               if (!pgopen)                                   /* Abort task */
                  errorC( 4, "Cannot initialize pgplot!" );
       }
   }

   for (i = 0; i < 2; i++)
   {
      blo[i] = GIDSblo[i];
      bhi[i] = GIDSbhi[i];
   }   


   markcolor = askcolor();
  
   image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );     
   if (!image)
      errorC( 4, "Cannot allocate memory for image" );
   imagesize = (bhi[0] - blo[0] + 1) * (bhi[1] - blo[1] + 1);

   /*-----------------------------*/
   /* Read subset data in memory. */
   /*-----------------------------*/   
   {
      fint   cwlo, cwhi;           /* Coordinate words. */      
      fint   tid = 0;              /* Transfer id for read function. */
      fint   pixelsread;           /* Number of pixels read by read routine. */

      cwlo = gdsc_fill_c( Setin, &subset, blo );
      cwhi = gdsc_fill_c( Setin, &subset, bhi ); 
      gdsi_read_c( Setin,
                   &cwlo, &cwhi,
                   &image[blo[1]][blo[0]],
                   &imagesize,
                   &pixelsread,
                   &tid );
   }   

   fp = getfileptr();                       /* Open a file on disk */
   
        
   fmake( Ch, 1 );                          /* Used in 'pgcurs' */
   drawframe( blo, bhi, 0.0, 0.0 );
   samples = 0;
      
   xy[0] = blo[0] + (bhi[0]-blo[0])/3.0;    /* Here is the graphics cursor */
   xy[1] = blo[1] + (bhi[1]-blo[1])/3.0;
   setcol( markcolor );
   do
   {            
      double  xphys, yphys;
      bool    physical;
      float   xlo = (float) blo[0];
      float   xhi = (float) bhi[0];
      float   ylo = (float) blo[1];
      float   yhi = (float) bhi[1];
            
      status_c( tofchar( "LEFT button or key to mark, RIGHT bt. or Q to Quit" ) );
      r1 = pgcurs_c( &xy[0], &xy[1], Ch );
      if (!r1)
         anyoutf( 1, "Device has no cursor, or other error" );
      ch = toupper(Ch.a[0]);
      if (ch == 'Q' || ch == '3')                  
          r1 = 0;
      if (r1)
      {  
         fint one = 1;
         fint symbol = 2;                          /* '+' character */
         pgpt_c( &one, &xy[0], &xy[1], &symbol );
         physical = tophys( Setin, 
                            subset, 
                            axnum, 
                            blo, 
                            bhi, 
                            xy[0], 
                            xy[1],
                            &imval, 
                            &xphys, 
                            &yphys );
          
         if (xy[0] >= xlo && xy[0] <= xhi && xy[1] >= ylo && xy[1] <= yhi)
         {
            writevalues( fp, 
                         xy[0], xy[1], 
                         imval, 
                         physical, 
                         xphys, yphys, 1 );
            samples++;
         }
      }
   }
   while( r1 );      
   
   (void) sprintf( message, 
                  "Number of entered valid positions is %d", samples );
   anyoutf( 8, message );

   freefmatrix( image, blo[0], blo[1] );
   pgend_c();
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}

