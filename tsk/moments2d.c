/*
                            COPYRIGHT (c) 2000
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             moments2d.dc1

Program:       MOMENTS2D

Purpose:       Calculate FWHM and position angle of an ellipse, that represents
               a 2-dim gaussian, using zero-th, first and second moments
               of a distribution. The program can find asymmetrical features 
               in a data set.

Category:      ANALYSIS, CALCULATION

File:          moments2d.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set, subset:
               
               Maximum number of subsets is 1.


   BOX=        Give box in .....                        [entire subset]


   CLIPLO=     Enter one or more lower clip values:
               
               To select a range of data which you want to include in
               the moments analysis, use keywords CLIPLO= and CLIPHI=
               The entries in both keywords correspond so e.g. 
               CLIPLO=10 15 CLIPHI=20 40 means that the first range 
               includes data with levels between 10 and 20 (in units of
               the input data) and the second range includes data with 
               levels between 15 and 40.
               
              
   CLIPHI=     Enter exactly ... values for the upper clip:
               
               See description at CLIPLO=
               You have to specify exactly the same amount of levels
               as in CLIPLO=
               
              
   ZEROLEVEL=  Enter a value for the zero level of the data:      [calc]
               
               For the Full Width Half Maximum values that are 
               calculated using moments analysis, you need to define
               the intensity offset of your data. The default is that
               the program estimates this level by averaging the piixels
               on the border of the box in BOX=. If it cannot find any
               non-blank pixels on the border, the zero level is set 
               to 0.0.
               
              
   MARKRANGE=  Enter the clip range to mark in the plot:          [none]
               
               If you enter two values here then all data displayed
               in gids with a value within this range, will be marked 
               so that one can see which data is include in a moments
               analysis. If you enter just one value, then all pixels 
               with values greater than this one will be marked.
               

   FILENAME=   Enter a name or press return for log file only: [no file]
               
               If a name is specified, an ASCII file is created to
               store data. If you press carriage return, there will 
               be no output to an ASCII file only the GIPSY screen and
               log file.





Description:   Using moments analysis, one can return an estimated center,
               the Full Width Half Maximum in x and y and the position 
               angle of 2dim gaussian represented by data in a box that 
               is set by BOX=
               Only data is used with levels in ranges defined by CLIPLO=
               and CLIPHI=
                                                                          
               The analysis method uses the zero-th, first and second
               moment of the data to estimate the parameters of the       
               gaussian. The function can be represented by:
                                                                          
               z(x,y) = z0.exp[-{a(x-xc)^2+b(y-yc)^2+2c(x-xc)(y-yc)}]+o 
                  
               where 'o' is an offset in the data entered in ZEROLEVEL=
               xc and yc are the estimated centers and a,b,c are parameters
               that are converted to FWHM and position angle.
                                                                       
               Definition of the moments:
                                                                          
               First moment M0: M0 = SjSi Tji   (S == Sigma character)    
               For the peak position in grid coordinates we have:          
               x_av = 1/M0.SjSi[xi.Tji]   and  y_av = 1/M0.SjSi[yi.Tji]   
                                                                          
               Second moments Mxx = 1/M0.SjSi[xi^2.Mji]  - x_av^2         
                              Myy = 1/M0.SjSi[yi^2.Mji]  - y_av^2         
                              Mxy = 1/M0.SjSi[xi.yi.Mji] - y_av.x_av      

               Note that calculations are done both in grids and arcsec. 
               the grid parameters are used to plot an ellipse. The
               other parameters are only written to log file or file on disk. 

               The conversions to FWHM and angle are given by the formulas:
               
                           theta = 0.5 * atan(2.0*c/(a-b)); 
               
               A dummy variable 'p' is defined as:
               
                           p = sqrt( (a-b)*(a-b)+4.0*c*c );
                           
               then:
               
                    FWHM in x = 2.0 * sqrt( 2.ln(2)/(a+b+p) );
                    FWHM in y = 2.0 * sqrt( 2.ln(2)/(a+b-p) );
                    
               or v.v. if b > a.

               The moments analysis can be used for instance to find 
               asymmetric distributions in galaxies. The method is 
               described in more detail in the documentation of GAUFIT2D.
               
               
               Overlays:
               ========
               
               Ellipses are plotted in GIDS. If set. subset loaded in GIDS
               are the same as set and subset for this program, then 
               ellipses are plotted over the map. Else, program VIEW
               is started to display the right set and subset.


Notes:         

Example:       

Updates:       Oct 09, 2000: VOG, Document created.

#<
*/

/*  moments2d.c: include files     */

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
#include    "matrix.h"
#include    "time.h"
#include    "float.h"

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
#include    "factor.h"
#include    "pgellipse.h"
#include    "deputy.h"

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
#include    "gdsd_rchar.h"
#include    "gdsd_rdble.h"

/* GIDS overlays */

#include    "gdi_iinfo.h"
#include    "gdi_open2.h"
#include    "gdi_close.h"
#include    "gdi_frame.h"


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
#define RAD(a)         ( (a) * 0.017453292519943295769237 )
#define DEG(a)         ( (a) * 57.295779513082320876798155 )

#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     1          /* Max. allowed subsets */
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

#define GAUSSCUTOFF    0.05        /* Gauss cutoff in moments method */
#define DEBUG          16 
#define MAXRANGES      1024

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")




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


/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */



static void clearstr( fchar Fstr )
/*------------------------------------------------------------*/
/* Purpose: Blank a Fortran string up to 'len' characters     */
/*------------------------------------------------------------*/
{
   int    i;
   fint   len = Fstr.l;


   for (i = 0; i < (int) len; i++)
   {
      Fstr.a[i] = '\0';
   }
}



static void setchsize( float ch )
/*-------------------------------------------------------------*/               
/* PURPOSE: Set character height.                              */
/*-------------------------------------------------------------*/
{
   pgsch_c( &ch );
} 



static void setcolor( int col )
/*-------------------------------------------------------------*/               
/* PURPOSE: Set color to 'col'.                                */
/* Alternative pgsci.                                          */
/*-------------------------------------------------------------*/
{
   fint   Col = (fint) col;
   pgsci_c( &Col );
} 



static void setwidth( int width )
/*-------------------------------------------------------------*/               
/* PURPOSE: Set line width to 'width'.                         */
/* Alternative pgslw.                                          */
/*-------------------------------------------------------------*/
{
   fint    Width = (fint) width;
   pgslw_c( &Width );
} 



static void plsymbol( float x, float y, int symbol,
               float charh, int linewidth, int colour )                         
/*-------------------------------------------------------------*/
/* PURPOSE: Alternative for pgpt.                              */               
/*-------------------------------------------------------------*/               
{
   fint  num = 1;
   fint  symnr = (fint) symbol;
   fint  col = (fint) colour;
   fint  lw  = (fint) linewidth;
   fint  oldcol, oldlw;
   float oldch;


   pgqch_c( &oldch );
   pgqlw_c( &oldlw );
   pgqci_c( &oldcol );
   setchsize( charh );
   setwidth( lw );
   setcolor( col );
   pgpt_c( &num, &x, &y, &symnr );
   setchsize( oldch );
   setwidth( oldlw );
   setcolor( oldcol );
}


static int equalset( fchar Set1,
                     fchar Set2 )
/*------------------------------------------------------------*/
/* PURPOSE: Check whether two set names are equal.            */
/* Include path names in comparison. But before including a   */
/* path, copy the set names so that input is not changed!     */
/*------------------------------------------------------------*/
{
   int    i;
   fint   l1, l2;
   fchar  S1, S2;


   fmake( S1, FILENAMELEN );
   fmake( S2, FILENAMELEN );

   for (i = 0; i < nelc_c(Set1) && i < S1.l; i++)  /* Copy all characters in src */
      S1.a[i] = Set1.a[i];
   while (i < S1.l)                                /* Pad with blanks */
      S1.a[i++] = ' ';

   for (i = 0; i < nelc_c(Set2) && i < S2.l; i++)
      S2.a[i] = Set2.a[i];
   while (i < S2.l)
      S2.a[i++] = ' ';

   l1 = getpath_c( S1 );
   l2 = getpath_c( S2 );
   if (l1 != 0 || l2 != 0)
   {
      anyoutf( 1, "Cannot get full pathname for set name" );
      if (l1 == -1 || l2 == -1)
         anyoutf( 1, "Cannot get entry from password file.");
      if (l1 == -2 || l2 == -2)
         anyoutf( 1, "Full pathname too long for PATH" );
      return( 0 );
   }

   l1 = nelc_c( S1 );
   l2 = nelc_c( S2 );

   if (l1 != l2)
      return( 0 );
   return( strncmp( S1.a, S2.a, l1 ) == 0 );
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
      anyoutf( 1, "Cannot open this output device" );
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
   {
      float xtick = 0.0, ytick = 0.0;
      fint  nxsub = 0.0, nysub = 0;
      pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
               tofchar("BCNSTV"), &ytick, &nysub );
   
      /* Plot the titles */
      pglab_c( tofchar("X"), tofchar("Y"), tofchar("2D-MOMENTS PARAMETER ELLIPSE(s)") );
   }
   return( YES );
}



static fint getGIDSinfo( fchar  Setin,
                         fint   subset,
                         fint  *GIDSblo,
                         fint  *GIDSbhi,
                         float *GIDSflo,
                         float *GIDSfhi )
/*------------------------------------------------------------*/
/* INPUT:   Setin                                             */
/* OUTPUT:  GIDSblo, GIDSbhi, GIDSflo, GIDSfhi                */
/* PURPOSE: Check whether an overlay in GIDS can be made. I.e.*/
/*          1) Gids must be started, 2) An image must be load-*/
/*          ed and 3) The name of the displayed set must match*/
/*          'Setin'. If so, return 1 and the sizes of the     */
/*          displayed box and the GIDS frame, Otherwise return*/
/*          0 as result of the function.                      */
/*------------------------------------------------------------*/
{
   fint         display_stat;                   /* display operation status */
   fint         r1;
   fint         GIDSdisplay_id = -1;            /* id of display */
   fchar        GIDSset;
   char         setbuf[STRLEN];
   fint         GIDSsubset;
   char         message[STRLEN];
   int          equal = NO; 
  

   /* If not available, do NOT start GIDS */
   GIDSdisplay_id = gdi_open2_c( tofchar(" ") );

   if (GIDSdisplay_id < 0)                      /* error opening display */
   {
      return( -1 );
   }
   GIDSset.a = setbuf; GIDSset.l = STRLEN-1; clearstr( GIDSset );
   display_stat = gdi_iinfo_c( &GIDSdisplay_id, /* id of display */
                               GIDSset,         /* name of set */
                               &GIDSsubset,     /* subset level */
                               GIDSblo,         /* lower left frame boundary */
                               GIDSbhi );       /* upper right frame boundary */

   if (display_stat < 0)                        /* error obtaining info */
   {
      anyoutf( DEBUG,  "No image loaded!");
      return( -2 );
   }

   equal = (equalset( Setin, GIDSset ) && (GIDSsubset == subset) );
   if (!equal)
   {
      anyoutf( 1,  "Input set and aet displayed in GIDS are not equal!" );
      return( -3 );
   }

   if (gdsc_ndims_c( GIDSset, &GIDSsubset ) != 2)
   {
      anyoutf( 1,  "Wrong dimension of set in GIDS!");
      return( -4 );
   }


   r1 = gdi_frame_c( &GIDSdisplay_id ,          /* id of display */
                     GIDSflo,                   /* lower left frame boundary */
                     GIDSfhi );                 /* .. in (floating) grids */

   if (r1 != 0)
   {
      (void) sprintf( message,
                     "Cannot obtain info about frame currently on display! (err=%d)", r1 );
      anyoutf( 1, message );
      return( -5 );
   }
   display_stat = gdi_close_c( &GIDSdisplay_id );   /* close display */

   (void) sprintf( message,
                  "Displayed set has box: [%d %d %d %d]",
                   GIDSblo[0], GIDSblo[1], GIDSbhi[0], GIDSbhi[1] );
   anyoutf( DEBUG,  message );
   (void) sprintf( message,
                  "Gids frame: [%g %g %g %g]",
                   GIDSflo[0], GIDSflo[1], GIDSfhi[0], GIDSfhi[1] );
   anyoutf( DEBUG,  message );
   return( 0 );
}



static int gauss2ellipse( double   a,
                          double   b,
                          double   c,
                          double   *X,
                          double   *Y,
                          double   *phi,
                          double   ellipseconst )
/*------------------------------------------------------------*/
/* PURPOSE: Convert gauss parameters a,b,c to ellipse         */
/*          parameters axis in X, axis in Y, position angle   */
/*          The axes are the Full Widths at Half Maximum.     */
/*                                                            */
/* The equation of the gaussian is:                           */
/*                                                            */
/* z(x,y) = z0.exp[-{a(x-xc)^2+b(y-yc)^2+2c(x-xc)(y-yc)}]     */
/*------------------------------------------------------------*/
{
   double    p;
   double    theta;

   if (a*b-c*c <= 0.0)
   {
      anyoutf( 1, "Estimates from moments are unusable" );
      anyoutf( 1, "a=%f, b=%f, c=%f do not make an ellipse!", a, b, c );
      return( 0 );
   }
   if (a == b)
      theta = 0.0;
   else
   {
      /*--------------------------------------------------*/
      /* Use atan (instead of atan2) because result must  */
      /* be between -PI/2 and PI/2. Then                  */
      /* 'theta' is in the range -PI/4 and PI/4           */ 
      /*--------------------------------------------------*/
      theta = DEG( 0.5 * atan(2.0*c/(a-b)) );
   }

   p = sqrt( (a-b)*(a-b)+4.0*c*c );
   {
      double axisX, axisY;
      if (a>b)
      {
         axisX = 2.0 * sqrt( ellipseconst/(a+b+p) );
         axisY = 2.0 * sqrt( ellipseconst/(a+b-p) );
      }
      else
      {
         axisX = 2.0 * sqrt( ellipseconst/(a+b-p) );
         axisY = 2.0 * sqrt( ellipseconst/(a+b+p) );
      }

      anyoutf( DEBUG, "A=%f b=%f 2C=%f converted to:", a , b, 2.0*c );
      anyoutf( DEBUG, "angle=%f (deg), axisX=%f axisY=%f",
               theta, axisX, axisY );
      *X = axisX;
      *Y = axisY;
   }
   *phi = theta;
   return( 1 );
}



static int gauestmom2dim( float  **image,
                          fint   *blo,
                          fint   *bhi,
                          double *pars,
                          double *M,
                          double *grid2arcsec,
                          double *range,
                          double *zerolevel )
/*------------------------------------------------------------*/
/* PURPOSE: Using the moments method, return BWHP and         */
/* position angle of 2dim gaussian represented by data in     */
/* array 'data' in a box that is set by 'blo', 'bhi'.         */
/* Include only data in 'range'.                              */
/*                                                            */
/* The method that  uses the first and second                 */
/* moment of the data to estimate the parameters of the       */
/* gaussian. The function has no offset wrt. the x axis, and  */
/* can be represented by:                                     */
/*                                                            */
/*   z(x,y) = z0.exp[-{a(x-xc)^2+b(y-yc)^2+2c(x-xc)(y-yc)}]+o */
/*                                                            */
/* Define the moments:                                        */
/*                                                            */
/* First moment M0: M0 = SjSi Tji   (S == Sigma character)    */
/* For the peak position in gridcoordinates we have:          */
/* x_av = 1/M0.SjSj[xi.Tji]   and  y_av = 1/M0.SjSj[yi.Tji]   */
/*                                                            */
/* Second moments Mxx = 1/M0.SjSi[xi^2.Mji]  - x_av^2         */
/*                Myy = 1/M0.SjSi[yi^2.Mji]  - y_av^2         */
/*                Mxy = 1/M0.SjSi[xi.yi.Mji] - y_av.x_av      */
/*                                                            */
/* Note that calculations are done in grids. Then the angle   */
/* found after converting the 2d-gauss parameters to ellipse  */
/* parameters, is defined in a system with SQUARE grids.      */
/* This angle must be converted then if the grids are not     */
/* square.                                                    */
/* Return values: 0   -- Failure                              */
/*                1   -- Return parameter for 1-dim gauss in  */
/*                       'a', centre in 'xc' and 'zo'.        */
/*                2   -- Parameters of 2 dim gauss            */
/*------------------------------------------------------------*/
{
   double  S   = 0.0;
   double  Sx  = 0.0;
   double  Sy  = 0.0;
   double  Sxx = 0.0;
   double  Syy = 0.0;
   double  Sxy = 0.0;
   double  x, y;
   double  Xav, Yav;
   double  M0, Mxx, Myy, Mxy;
   double  denom;
   double  z0, xc, yc, a, b, c, zero;
   int     i, j;
   int     numonborder = 0;
   int     indx = 0;
   int     k;



   if (*zerolevel == blank)
   {
      /* Try to find the offset in height for this gaussian. */
      /* Estimate a value by averaging all pixels on the borders */
      S = 0.0;
      k = 0;
      for (j = blo[1]; j <= bhi[1]; j++)
      {
         for (i = blo[0]; i <= bhi[0]; i++)
         {
            float val = image[j][i];
            if (val != blank)
            {
               if ( j == blo[1] || j == bhi[1] ||
                    i == blo[0] || i == bhi[0] )
               {
                  S += (double) val;
                  numonborder++;
               }
            }
         }
      }
      zero = 0.0;
      if (numonborder != 0)
      {
         zero = S / (double) numonborder;      
      }
      *zerolevel = zero;
   }
   else
   {
      zero = *zerolevel;
   }


   S = 0.0;
   for (j = blo[1]; j <= bhi[1]; j++)
   {
      y = ((double) j) * grid2arcsec[1];
      for (i = blo[0]; i <= bhi[0]; i++)
      {
         float valfloat = image[j][i];
         x = ((double) i) * grid2arcsec[0];

         if (valfloat != blank && (valfloat >= range[0] && valfloat <= range[1]) )
         {
            double val = ((double) valfloat) - zero;
            S   += val;
            Sx  += val * x;
            Sy  += val * y;
            Sxx += val * x * x;
            Syy += val * y * y;
            Sxy += val * x * y;
         }
         indx++;
      }
   }
   M0  = S;
   if (M0 == 0.0)
   {
      anyoutf( 1, "Cannot estimate moments because first moment is 0" );
      return( 0 );
   }
   Xav = Sx  / M0;
   Yav = Sy  / M0;
   Mxx = Sxx / M0 - Xav*Xav;
   Myy = Syy / M0 - Yav*Yav;
   Mxy = Sxy / M0 - Xav*Yav;

   M[0] = M0;
   M[1] = Xav;
   M[2] = Yav;
   M[3] = Mxx;
   M[4] = Myy;
   M[5] = Mxy;               
   if (Mxx != 0.0 && Myy == 0.0 && Mxy == 0.0)
   {
      /* One dimensional gauss in x */
      a = 1.0 / (2.0*Mxx);
      xc = Xav / grid2arcsec[0];
      b = c = yc = 0.0;      /* dummies */
      z0 = grid2arcsec[0] * M0 / (sqrt(2.0*PI*Mxx));
      pars[0] = z0;
      pars[1] = xc;
      pars[2] = yc;
      pars[3] = a;
      pars[4] = b;
      pars[5] = c;
      pars[6] = zero;
      return( 1 );
   }
   if (Mxx == 0.0 && Myy != 0.0 && Mxy == 0.0)
   {
      /* One dimensional gauss in y */
      a = 1.0 / (2.0*Myy);
      xc = Yav / grid2arcsec[1];
      b = c = yc = 0.0;      /* dummies */
      z0 = grid2arcsec[1] * M0 / (sqrt(2.0*PI*Myy));
      pars[0] = z0;
      pars[1] = xc;
      pars[2] = yc;
      pars[3] = a;
      pars[4] = b;
      pars[5] = c;
      pars[6] = zero;
      return( 1 );
   }

   denom = 2.0 * (Mxx*Myy-Mxy*Mxy);
   if (denom == 0.0)
   {
      anyoutf( DEBUG, "Cannot estimate moments because Mxx*Myy-Mxy*Mxy == 0");
      anyoutf( DEBUG, "Mxx=%f Myy=%f Mxy=%f", Mxx, Myy, Mxy );
      return( 0 );
   }
   a = Myy / denom;
   b = Mxx / denom;
   c = -1.0*Mxy/denom;
   xc = Xav / grid2arcsec[0];               /* This goes from arcsec to grids */
   yc = Yav / grid2arcsec[1];
   z0 = grid2arcsec[0]*grid2arcsec[1]*M0 / ( 2.0*PI*sqrt(Mxx*Myy-Mxy*Mxy) );
   pars[0] = z0;
   pars[1] = xc;
   pars[2] = yc;
   pars[3] = a;
   pars[4] = b;
   pars[5] = c;
   pars[6] = zero;

   return( 2 );
}



static void getdata( fchar   Set,
                     fint    subset,
                     float **image,                     
                     fint   *blo,
                     fint   *bhi,
                     int     plot,
                     float  *markrange,
                     fint    col )
/*------------------------------------------------------------*/
/* PURPOSE:  Read all values in between blo and bhi in'image' */
/*           Note that one subset is entered.                 */
/*------------------------------------------------------------*/
{
   fint  pixelsread;
   fint  cwlo, cwhi;
   fint  tid = 0;
   fint  imagesize;
   int   x = blo[0];
   int   y = blo[1];

   imagesize = (bhi[0] - blo[0] + 1) * (bhi[1] - blo[1] + 1);
   cwlo   = gdsc_fill_c( Set, &subset, blo );
   cwhi   = gdsc_fill_c( Set, &subset, bhi );
   gdsi_read_c( Set,
                &cwlo, &cwhi,
                &(image[y][x]),
                &imagesize,
                &pixelsread,
                &tid );

   if (tid != 0 || pixelsread != imagesize)
   {
      /* PROBLEMS!!! PROBLEMS!!! PROBLEMS!!! PROBLEMS!!! */
      errorf( 4, "Cannot read the data!" );
   }
   if (markrange[0] != blank && plot)
   {
      for (x = blo[0]; x <= bhi[0]; x++)
      {
         for (y = blo[1]; y <= bhi[1]; y++) 
         {
            float val = image[y][x];
            if (val != blank && (val >= markrange[0] && val <= markrange[1]) )
            {
               plsymbol( (float)x, (float)y, 2, 1.0, 2, col );  /* i.e. sym,size,width,col */ 
            }
         }
      }
   }   
}



static void getgridspacing( fchar    Set, 
                            fint    *axnum,
                            double  *grid2arcsec )
/*------------------------------------------------------------*/
/* PURPOSE: If possible get the conversion from grid to arc-  */
/*          seconds.                                          */
/*------------------------------------------------------------*/
{
   fchar     Cunit[2]; 
   char      cunit[2][FITSLEN];
   char      message[FITSLEN]; 
   double    cdelt, cfact;   
   int       i;
 
   for (i = 0; i < 2; i++)
   {
      fint   r1;
      Cunit[i].a = cunit[i]; 
      Cunit[i].l = FITSLEN; 
      clearstr( Cunit[i] );
      
      r1 = 0;
      sprintf( message, "CUNIT%d", axnum[i] );     
      gdsd_rchar_c( Set, tofchar(message), &setlevel, Cunit[i], &r1 );
      Cunit[i].a[nelc_c(Cunit[i])] = '\0';
      if (r1 < 0)
         strcpy( Cunit[i].a, "?" );   

      sprintf( message, "CDELT%d", axnum[i] );
      r1 = 0;
      /* Get the pixel separation of the axes */
      gdsd_rdble_c( Set, tofchar(message), &setlevel, &cdelt, &r1 );
      if (r1 < 0)
      {
         cdelt = 1.0;
         anyoutf( 1, "No grid spacings in header of this set!" );
      }
      r1 = factor_c( Cunit[i], tofchar("ARCSEC"), &cfact );
      if (r1 == 0)
      {
         grid2arcsec[i] = fabs(cdelt) * cfact;
      }
      else
      {
         /* Do not allow conversions to arcsec for ANY axis! */
         anyoutf( 1, "Cannot convert to physical units. Spacing == 1" );
         grid2arcsec[i] = 1.0;
      }
   }
}



static void doplot( fint   *blo, 
                    fint   *bhi, 
                    double *range,
                    double  Xc,
                    double  Yc,
                    double  fvwmX,
                    double  fvwmY,
                    double  pa )
/*------------------------------------------------------------*/
/* PURPOSE: Plot the ellipse with the parameters fwhm and pa  */
/*          in the grid system.                               */
/*------------------------------------------------------------*/
{
   float xc = (float) Xc;
   float yc = (float) Yc;
   float maj = (float) fvwmX;
   float min = (float) fvwmY;
   float angle = (float)pa;   
   float start = 0.0;
   float end = 360.0;
   float delta = 1.0;
   
     
   pgellipse_c( &xc, &yc, &maj, &min, &angle, &start, &end, &delta );
   plsymbol( xc, yc, 2, 2.0, 4, blue );  /* i.e. sym,size,width,col */
}



static void stamp( char *mes )
/*------------------------------------------------------------*/
/* PURPOSE: Put time and user name in string.                 */
/*------------------------------------------------------------*/                
{
   struct tm   *ptr; 
   time_t      lt;

   lt    = time( NULL );                       /* Get the coded calendar time */
   ptr   = localtime( &lt );  
   strftime( mes, 128, "%d-%b-%Y (%H:%M:%S)", ptr ); 
}



static void tologfile( fchar   Setin, 
                       fint   *blo, 
                       fint   *bhi,
                       double *range, 
                       FILE   *fp,
                       double *parsgrid,
                       double  fwhmXgrid,
                       double  fwhmYgrid,
                       double  pagrid,
                       double *parsarcs,
                       double  fwhmXarcs,
                       double  fwhmYarcs,
                       double  paarcs )
/*------------------------------------------------------------*/
/* PURPOSE: Write results to scree, log and, if specified, a  */
/* file on disk.                                              */
/*------------------------------------------------------------*/
{
   char    stampstr[80];
   char    mes[512];   
   static  int     first = YES;

   anyoutf( 3, " " );   
   if (first)
   {
      stamp( stampstr );   
      sprintf( mes, "Moments of a 2d distribution, converted to ellipse parameters" );   
      anyoutf( 3, mes ); if (fp)  fprintf( fp, "!  %s\n", mes );
      sprintf( mes, "Date: %s\n", stampstr );
      anyoutf( 3, mes ); if (fp)  fprintf( fp, "!  %s\n", mes );
      sprintf( mes, "%10.10s | %10.10s | %10.10s | %10.10s | %10.10s | %10.10s | %10.10s | %10.10s | %10.10s | %10.10s |", 
               "clip lo", "clip hi", "Xc", "Yc", "fwhmX arc", "fwhmX grd",
               "fwhmY arc", "fwhmY grid", "PA arc", "PA grid" );
      anyoutf( 3, mes ); if (fp)  fprintf( fp, "!%s\n", mes );      
      first = NO;
   }
   sprintf( mes, "%+10.4f   %+10.4f   %+10.4f   %+10.4f   %+10.4f   %+10.4f   %+10.4f   %+10.4f   %+10.4f   %+10.4f",
            range[0], range[1],
            parsgrid[1],
            parsgrid[2],
            fwhmXarcs, fwhmXgrid,
            fwhmYarcs, fwhmYgrid,
            paarcs, pagrid );
   anyoutf( 3, mes ); if (fp)  fprintf( fp, " %s\n", mes );  
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   /* Variables for set input */

   fchar    Setin;              /* Name of input set */
   fint     subin[MAXSUBSETS];  /* Subset coordinate words */
   fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                /* axes numbers.  The first elements (upto */
                                /* the dimension of the subset) contain the */
                                /* axes numbers of the subset, the other */
                                /* ones ontain the axes numbers outside the */
                                /* the subset ordered according to the */
                                /* specification by the user. */
   fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
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
   fint     subdim;             /* Dimensionality of the subsets for class 1 applications */
   fint     setdim;             /* Dimension of set. */


/* Box and frame related */

   fint     flo[MAXAXES];       /* Low  edge of frame in grids */
   fint     fhi[MAXAXES];       /* High edge of frame in grids */
   fint     blo[MAXAXES];       /* Low  edge of box in grids */
   fint     bhi[MAXAXES];       /* High edge of box in grids */
                                /*  1 box may exceed subset size */
                                /*  2 default is in BLO */
                                /*  4 default is in BHI */
                                /*  8 box restricted to size defined in BHI*/
                                /*  These codes work additive.*/
                                 /*  When boxopt is 0 or 1, the default is the */
                                 /*  is the entire subset. */

   
   fint     maxsubs = MAXSUBSETS;
   fint     maxaxes = MAXAXES;           /* Max num. of axes the program can deal with.*/
   fint     class   = 1;                 /* Class 1 is for applications which repeat */
   fint     showdev = 1;
   fint     nsubs;                       /* Number of input subsets */
   fint     dfault;                      /* Default option for input etc */
   fint     r1, r2;
   float    **image = NULL;
   double   parsarcs[7], parsgrid[7];
   double   grid2arcsec[2];
   double   dummyconv[2];   
   double   twoln2 = 2.0*log(2.0);
   double   fwhmXgrid, fwhmYgrid, pagrid;
   double   fwhmXarcs, fwhmYarcs, paarcs;   
   double   momentsarcs[6];
   double   momentsgrid[6];
   double   cliplo[MAXRANGES];
   double   cliphi[MAXRANGES];
   double   zerolevel;
   float    markrange[2];
   fint     nranges = 0;
   int      i;
   FILE     *fp;
   char     filename[FILENAMELEN];
   float    GIDSflo[2], GIDSfhi[2];      /* GIDS frame */
   fint     GIDSblo[2], GIDSbhi[2];      /* GIDS box */
   int      plot;   
   
  

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

   {
      fint     nitems = MAXRANGES;
      fint     dfault = REQUEST;
      char     mes[128];
      
      nranges = userdble_c( cliplo, &nitems, &dfault,
                            tofchar("CLIPLO="), 
                            tofchar("Enter one or more lower clip values:") );
      sprintf( mes, "Enter exactly %d values for the upper clip:", nranges );
      (void) userdble_c( cliphi, &nranges, &dfault,
                            tofchar("CLIPHI="), 
                            tofchar(mes) );
   }

   {
      fint     nitems = 1;
      fint     dfault = REQUEST;
      fint     r;
      
      zerolevel = blank;
      r = userdble_c( &zerolevel, &nitems, &dfault,
                      tofchar("ZEROLEVEL="), 
                      tofchar("Enter a value for the zero level of the data:  [calc]") );
   }


   {
      fint     nitems = 2;
      fint     dfault = REQUEST;
      fint     r;
      
      markrange[0] = blank;
      markrange[1] = FLT_MAX;
      r = userreal_c( markrange, &nitems, &dfault,
                      tofchar("MARKRANGE="), 
                      tofchar("Enter the clip range to mark in the plot:  [none]") );
   }
   

   {
      fint     n = 0;
      fint     dfault = REQUEST; 
      fchar    Filename;
      
      Filename.a = filename;
      Filename.l = FILENAMELEN - 1;
      n = usertext_c( Filename, &dfault,
                      tofchar("FILENAME="),
                      tofchar("Enter a name or press return for log file only") );
      filename[nelc_c(Filename)] = '\0';
      if (n == 0)
         fp = NULL;
      else
      {
         fp = fopen(filename, "w");
         if (fp == NULL)
         {
            anyoutf( 1, "Could not write to this file. Output will be send to log!" );
         }
      }         
   }

   plot = YES;
   r1 = getGIDSinfo( Setin, subin[0], GIDSblo, GIDSbhi, GIDSflo, GIDSfhi );
   if (r1 != 0)
   {
      /* Start VIEW if */
      /* 1) GIDS was not started */
      /* 2) No image loaded */
      /* 3) GIDS set not equal to input set */
      if (r1 == -1 || r1 == -2 || r1 == -3)  
      {
         fint    r = 0;

         anyoutf( 1, "GIDS not started! Program will start VIEW now." );
         deputy_c( tofchar("VIEW"), &r );
         r1 = getGIDSinfo( Setin, subin[0], GIDSblo, GIDSbhi, GIDSflo, GIDSfhi );
         if (r1 != 0) 
            plot = NO;
      }
      else
      {
         plot = NO;
      }
   }

   
   if (plot)
   {
      if ( !initplot(GIDSblo, GIDSbhi, GIDSflo, GIDSfhi) )
      {
         anyoutf( 1, "Cannot open (PG)PLOT device!" );
         plot = NO; 
      }
   }
  
   image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );   
   getdata( Setin, subin[0], image, blo, bhi, plot, markrange, red );
   dummyconv[0] = dummyconv[1] = 1.0;      /* Grids in fact */
   getgridspacing( Setin, axnum, grid2arcsec );  
   anyoutf( 1, "Conversion factors from grid to arcsec in x, y: %f, %f", 
            grid2arcsec[0], grid2arcsec[1] );
   
  
   for (i = 0; i < nranges; i++)
   {   
      double clips[2];
      clips[0] = cliplo[i];
      clips[1] = cliphi[i];
      /* Moments in grids */
      gauestmom2dim( image, blo, bhi, parsgrid, momentsgrid, dummyconv, clips, &zerolevel );
      r1 = gauss2ellipse( parsgrid[3], parsgrid[4], parsgrid[5], 
                          &fwhmXgrid, &fwhmYgrid, &pagrid, twoln2 );    
                          
      /* Moments in arcsec */
      gauestmom2dim( image, blo, bhi, parsarcs, momentsarcs, grid2arcsec, clips, &zerolevel );
      r2 = gauss2ellipse( parsarcs[3], parsarcs[4], parsarcs[5],
                          &fwhmXarcs, &fwhmYarcs, &paarcs, twoln2 );
      if (r1 == 0 || r2 == 0)
      {
         anyoutf( 1, "Could not convert to FWHM's en PA" );
      }      
      tologfile( Setin, blo, bhi, clips, fp, 
                 parsgrid, fwhmXgrid, fwhmYgrid, pagrid,
                 parsarcs, fwhmXarcs, fwhmYarcs, paarcs );
      if (plot)
      {
         doplot( blo, bhi, clips,
                 parsgrid[1], parsgrid[2], fwhmXgrid, fwhmYgrid, pagrid );
      }
   }

    
  

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   if (fp != NULL)
      fclose( fp );  
   freefmatrix( image, blo[0], blo[1] );
   pgend_c();
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
