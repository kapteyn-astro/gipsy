/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             3dplot.dc1

Program:       3DPLOT

Purpose:       View Set, subset in perspective

Category:      PLOTTING

File:          3dplot.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give set (, subsets) to plot:
               Maximum number of subsets is 2048.

   BOX=        Frame for input subsets.                 [entire subset]

   GRDEVICE=   Plot device:                           [List of devices]
               Destination of plot, Screen or Hardcopy.
               
   MOSAIC=     View surface subdivisions x,y:                     [1,1]
               View surface can contain a number of plot pages in
               in X and Y direction (mosaic). Default is 1 plot in
               both X- and Y direction.    

** PAPER=      Give width(cm), aspect ratio:                  [0.0,1.0]
               Aspect ratio is height/width.
               
** LINEWIDTH=  Give line width (1-21):                              [1]
               For a hardcopy, the default is 2.
                                                      
   DISTANCE=   Distance of the eye from the screen:        [calculated]
               Control amount of perspective with this 
               unitless number.
               
   RHO=        Distance between viewpoint and origin.      [calculated]

   THETA=      Angle between view vector and positive x-axis wrt.  
               the positive x-axis (degrees):                      [30]
               x-axis is equivalent to the first subset axis.

   PHI=        Angle between view vector and positive z-axis wrt.
               the positive z-axis (degrees):                      [60]
               PHI= ranges from 0 to 180 deg.
               z-axis is equivalent to the image value axis.

** SCALE=      Give scale for image data:                  [calculated]
               Scale image data so that the range in z-values are
               in range of x- and y-values.
               
   DECIM=      Give decimation factors x, y:               [calculated]
               Two integer values that decimates the number of 
               pixels in x and y. Decimation must be used if too many
               positions are required.
           
   PLOTOPT=    Plot option:                                         [0]
               0: Plot surface in both directions
               1: Plot in X-direction only
               2: Plot in Y-direction only

   OPTION=     Min, max from: 1)User 2)Calculation 3)Header         [3]
   
   MINMAX=     Give data min, max:                      [header values]
               
               or, if OPTION=2
               
               Give data min, max:                         [calculated]
               This keyword specifies the unscaled range in data
               values.

   CONTINUE=   Continue?                                          [Y}/N
               Replot same subset with new values for DISTANCE=
               RHO=, PHI=, THETA=, SCALE=, PLOTOPT=, OPTION=
               and MINMAX=

Description:   In order to draw data in perspective there are two
               transformations to perform. First a viewpoint has to be
               specified (RHO=, THETA=, PHI= & DISTANCE=). The 'viewpoint' 
               transformation converts the coordinates in world coordinates 
               (i.e. pixel positions and image values) into
               'eye' coordinates expressed in a coordinate system
               centered at the viewpoint. The perspective transfor-
               mation produces the actual 2-dim. 'screen' coordinates.
               The perspective transformation has a single vanishing
               point and the screen axes are parallel to the 'eye'
               coordinates. The keywords related to perspective are 
               DISTANCE= and RHO=
               
               The viewing transformation:
               Point in the world coordinate system: (xw,yw,zw,1)
               Point in the eye coordinate system: (xe,ye,ze,1)
               theta = th, phi = ph.
               
               (xe,ye,ze,1) = (xw,yw,zw,1)V
               
               where V is matrix:


               | v11 v12 v13 v14 |
               | v21 v22 v23 v24 |  =                                     
               | v31 v32 v33 v34 |
               | v41 v42 v43 v44 |
                            
               |-sin(th) -cos(th)*cos(ph) -cos(th)*sin(ph)   0.0 |
               | cos(th) -sin(th)*cos(ph) -sin(th)*sin(ph)   0.0 |
               | 0.0      sin(ph)         -cos(ph)           0.0 |
               | 0.0      0.0              rho               1.0 |
                           
               Then:
               
               xe = v11*xw + v21*yw
               ye = v12*xw + v22*yw + v32*zw
               ze = v13*xw + v23*yw + v33*zw + v43
              
               The perspective transformation:
               If d is the distance of the eye to the screen and xs, ys
               are screen coordinates, then
               
               xs = d * xe / ze
               ys = d * ye / ze
               
              
                                             |
                               |             | ye
                               | ys          |
                    <- d ->    |             |
               >------------------------------ 
               eye            <-  ze  ->
              
               

Example:       Hint: If you want a plot with the same orientation
               like 'VIEW' use THETA=-80. PHI=20
                              

Updates:       Jul 14, 1994: MV, Document created.


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
#include "status.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsbox.h"
#include "gdsc_fill.h"
#include "gdsi_read.h"
#include "gdsi_write.h"
#include "gdsd_rreal.h"
#include "userint.h"
#include "usertext.h"
#include "fieini.h"
#include "fiedo.h"
#include "fiepar.h"
#include "cancel.h"
#include "gdsd_rchar.h"
#include "error.h"
#include "stabar.h"
#include "gdsc_name.h"
#include "userreal.h"
#include "userdble.h"
#include "userlog.h"
#include "userint.h"
#include "reject.h"
#include "pgplot.h"                 /* Include all pgplot includes */
#include "minmax1.h"
#include "axcoord.h"
#include "getusernam.h"
#include "getdate.h"

#define AXESMAX    10               /* Max. allowed number of axes in a set */
#define SUBSMAX    2048              /* Max. number of substructures to be specified */
#define MAXBUF     4096             /* Buffer size for I/O */
#define BIGSTORE   160              /* Length of a string */
#define VERSION    "1.0"            /* Version number of this program */
#define NONE       0                /* Default values for use in userxxx routines */
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define FITSLEN    20               /* Length of fits item from header */
#define false      0
#define true       1
#define NO         0
#define YES        1
#define PI         3.141592653589793
#define SCALEFACT  0.4
#define MAXRESX    1280
#define MAXPOINTS  256              /* Max. number of positions to store in 1 dir. */
#define DELTA      10

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




/* Keywords and messages */

#define KEY_INSET         tofchar("INSET=")
#define MES_INSET         tofchar("Give set (, subsets) to plot: " )
#define KEY_BOX           tofchar("BOX=")
#define MES_BOX           tofchar(" ")
#define KEY_RHO           tofchar("RHO=")
#define KEY_THETA         tofchar("THETA=")
#define KEY_PHI           tofchar("PHI=")
#define KEY_SCALE         tofchar("SCALE=")
#define KEY_EYE           tofchar("DISTANCE=")
#define KEY_DECIM         tofchar("DECIM=")
#define KEY_PLOTOPT       tofchar("PLOTOPT=")
#define MES_PLOTOPT       tofchar("0: Both directions  1: X  2: Y   [0]")
#define KEY_WINDOW        tofchar("WINDOW=")
#define MES_WINDOW        tofchar("Xmin, Ymin, Xmax, Ymax:  [calculated]")

#define KEY_MOSAIC        tofchar("MOSAIC=")
#define MES_MOSAIC        tofchar("View surface subdivisions x,y:   [1,1]")

#define KEY_OPTION        tofchar("OPTION=")
#define MES_OPTION1       tofchar("Min, max from: 1)User 2)Calculation 3)Header [3]")
#define MES_OPTION2       tofchar("Min, max from: 1)User 2)Calculation  [2]")
#define KEY_MINMAX        tofchar("MINMAX=")
#define KEY_CONTINUE      tofchar("CONTINUE=")

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
#define NINT(a)    ( (a)<0 ? (int)((a)-.5) : (int)((a)+.5) )
#define PI         3.141592653589793
#define TORAD(a)   ( (a)*PI/180.0 )             /* Convert degrees to radians */
#define TODEG(a)   ( (a)*180.0/PI )             /* Convert radians to degrees */

/* Input of set, subsets: */

static fchar    Setin;                /* Name of the set */
static fint     subin[SUBSMAX];       /* Array for the subset coordinate words */
static fint     nsubs;                /* Number of input subsets */
static fint     dfault;               /* Default option for input etc */
static fint     axnum[AXESMAX];       /* GDSINP axis numbers array */
static fint     axcount[AXESMAX];     /* GDSINP axis lengths array */
static fint     class = 1;            /* Repeat operation for each subset */
static fint     setdim;               /* Dimension of the set */
static fint     subdim;               /* Dimension of the subset */
static fint     scrnum = 3;           /* Destination of log output */
static fint     maxaxes  = AXESMAX;   /* Convert parameters to variables */
static fint     maxsubs  = SUBSMAX;
static fint     maxiobuf = MAXBUF;
static int      subnr;                /* Index of current subset */
static int      i, m;                 /* Counters */
static fint     setlevel = 0;         /* Indicate set level */


/* Input of area etc.:*/

static fint     cwlo, cwhi;           /* Coordinate words */
static fint     frameLO[AXESMAX];     /* Coordinate words for frame */
static fint     frameHI[AXESMAX];
static fint     boxLO[AXESMAX];       /* Coordinate words for box */
static fint     boxHI[AXESMAX];
static fint     boxopt;               /* Input option for 'gdsbox' */


/* Data transfer: */

static fint     totpixels;            /* Total number of pixels in input */
static fint     pixelsdone;
static float    imageIN[MAXBUF];      /* Contains data to be plotted*/


/* Variables related to perspective transform */

static double    v11, v12, v13, v14;  /* Global matrix elements for view transform! */
static double    v21, v22, v23, v24;
static double    v31, v32, v33, v34;
static double    v41, v42, v43, v44;
static double    rho, theta, phi;     /* Specify the viewpoint */
static double    d_eye;               /* Distance of the eye from screen */


/* PGPLOT related */

static float     aspectratio;
static char      xtitle[FITSLEN];
static char      ytitle[FITSLEN];
static char      ztitle[FITSLEN];


/* Miscellaneous */


static fint      nitems;                /* Max. number of input items in userxxx routines */
static fint      r1, r2;                /* Results of userxxx routines */
static float     blank;                 /* Value of system blank */
static char      messbuf[BIGSTORE];     /* Buffer for text message */
 
static int       box_Xlen, box_Ylen;
static float     xs_min, ys_min, xs_max, ys_max;
static float     minZ, maxZ;
static float     xs, ys;
static float     xw, yw, zw;
static int       Xpos, Ypos;

static float     image3D_X[MAXPOINTS][MAXPOINTS];
static float     image3D_Y[MAXPOINTS][MAXPOINTS];
static int       agreed;
static fint      decim[2];
static double    scale;

static float     minscr[MAXRESX], maxscr[MAXRESX];
static float     Xarray[MAXRESX], Yarray[MAXRESX];




void anyoutC( int scrnum, char *anystr )
/*-----------------------------------------------*/
/* The C version of 'anyout'.                    */
/*-----------------------------------------------*/
{
   fint Fscrnum = (fint) scrnum;
   anyout_c( &Fscrnum, tofchar( anystr ) );
}




void boxminmax( fint Bcwlo, fint Bcwhi, float *minZ, float *maxZ,
                fchar Setin, fint subset )
/*----------------------------------------------------------------------*/
/* For the scaling of the data, we need the minimum and maximum data    */
/* value in the box. This can be                                        */
/* 1) The header values DATAMIN, DATAMAX of the current subset          */
/* 2) User given values                                                 */
/* 3) Calculated min, max for current box.                              */
/*----------------------------------------------------------------------*/
{
   fint   tidin;
   fint   fnuminreadbuf;
   float  blank;
   fint   dfault;
   fint   nitems;
   fint   err;
   float  minmaxZ[2];
   int    headval;
   int    first;
   fint   option;
   float  datamin, datamax;
   fint   r1;


   /* Are min, max in the header at this level? */
   headval = true;
   err = 0;
   gdsd_rreal_c( Setin, tofchar( "DATAMIN" ), &subset, &datamin, &err );
   if (err != subset)
   {
      headval = false;
   }
   err = 0;
   gdsd_rreal_c( Setin, tofchar( "DATAMAX" ), &subset, &datamax, &err );
   if (err != subset)
   {
      headval = false;
   }


   /* Ask user for option to input min, max */
   dfault = REQUEST;
   nitems = 1;
   if (headval)
   {
      do
      {
         option = 1;
         r1 = userint_c( &option, &nitems, &dfault,
                          KEY_OPTION, MES_OPTION1 );
         agreed = (option >= 1 && option <= 3);
         if (!agreed)
            reject_c( KEY_OPTION, tofchar("Not allowed!") );
      } while (!agreed);
   }
   else
   {
      do
      {
         option = 1;
         r1 = userint_c( &option, &nitems, &dfault,
                          KEY_OPTION, MES_OPTION2 );
         agreed = (option >= 1 && option <= 2);
         if (!agreed)
             reject_c( KEY_OPTION, tofchar("Not allowed!") );
      } while (!agreed);
   }


   if (option == 1)
   {
      dfault = REQUEST;
      nitems = 2;
      if (headval)
      {
         sprintf( messbuf, "Give data min, max:   [%f,%f]", datamin, datamax );
         do
         {
            minmaxZ[0] = datamin;
            minmaxZ[1] = datamax;
            r1 = userreal_c( minmaxZ, &nitems, &dfault,
                             KEY_MINMAX, tofchar(messbuf) );
            agreed = (minmaxZ[1] > minmaxZ[0]);
            if (!agreed)
               reject_c( KEY_MINMAX, tofchar("max <= min") );
         }
         while (!agreed);
         cancel_c( KEY_MINMAX );
         *minZ = minmaxZ[0];
         *maxZ = minmaxZ[1];
         return;
      }
      else
      {
         /* NO HEADER ITEMS WERE FOUND */
         sprintf( messbuf, "Give data min, max:     [calculation]" );
         do
         {
            r1 = userreal_c( minmaxZ, &nitems, &dfault,
                             KEY_MINMAX, tofchar(messbuf) );
            if (r1 == 0)
            {
               option = 2;
               agreed = true;
            }
            else {
               agreed = (minmaxZ[1] > minmaxZ[0]);
               if (!agreed)
                  reject_c( KEY_MINMAX, tofchar("max <= min") );
               else
               {
                  cancel_c( KEY_MINMAX );
                  *minZ = minmaxZ[0];
                  *maxZ = minmaxZ[1];
                  return;
               }
            }
         } while (!agreed);
      }
   } /* End of option 1 */


   if (option == 2) 
   {
      setfblank_c( &blank );
      tidin = 0;
      first = true;
      status_c( tofchar("Calculating min, max in box") );
      do 
      {
         gdsi_read_c( Setin,
                      &Bcwlo, &Bcwhi,
                      imageIN,
                      &maxiobuf,
                      &fnuminreadbuf,
                      &tidin );

         for (i = 0; i < (int) fnuminreadbuf; i++)
         {
            if (imageIN[i] != blank)
            {
               if (first)
               {
                  datamin = imageIN[i];
                  datamax = datamin;
                  first = false;
               }
               else
               {
                  datamin = MYMIN( imageIN[i], datamin );
                  datamax = MYMAX( imageIN[i], datamax );
               }
            }
         }
      } while ( tidin != 0 );
      status_c( tofchar("") );      

      cancel_c( KEY_MINMAX );
      *minZ = datamin;
      *maxZ = datamax;
   }

   if (option == 3)
   {
      cancel_c( KEY_MINMAX );
      *minZ = minmaxZ[0];
      *maxZ = minmaxZ[1];
   }
}




void convert1D_to_2D( int onedimpos, int box_X, float *xw, float *yw )
{
   *yw = (float) (onedimpos / box_X);
   *xw = (float) (onedimpos - (*yw) * box_X);
}



void initviewtransform( float rho, float theta, float phi )
/*----------------------------------------------------------------*/
/* For a given rho, theta and phi, this routine need to be called */
/* only once.                                                     */
/*----------------------------------------------------------------*/
{
   double  sinth, costh, sinph, cosph;

   theta *= PI/180.0;
   phi   *= PI/180.0;

   costh = cos(theta);
   sinth = sin(theta);
   cosph = cos(phi);
   sinph = sin(phi);

   v11 = -sinth;    v12 = -costh*cosph;    v13 = -costh*sinph;   v14 = 0.0;
   v21 =  costh;    v22 = -sinth*cosph;    v23 = -sinth*sinph;   v24 = 0.0;
   v31 =  0.0;      v32 =  sinph;          v33 = -cosph;         v34 = 0.0;
   v41 =  0.0;      v42 =  0.0;            v43 =  rho;           v44 = 1.0;     
}


void coord2screen( float x, float y, float z, float *px, float *py )
/*-----------------------------------------------------------------------*/
/* Transform the three dimensional point x,y,z to the screen coordinates */
/* px, py. The conversion consists of a viewing and a perspective trans- */
/* formation.                                                            */
/*-----------------------------------------------------------------------*/
{
   double    xe, ye, ze;
   double    xx = (double) x;
   double    yy = (double) y;
   double    zz = (double) z;      

   /* Eye coordinates */
   xe = v11*xx + v21*yy;
   ye = v12*xx + v22*yy + v32*zz;
   ze = v13*xx + v23*yy + v33*zz + v43;

   /* Screen coordinates */
   *px = (float) (d_eye * xe/ze);
   *py = (float) (d_eye * ye/ze);   
}


void findscreenminmax( float *xs_min, float *xs_max, 
                       float *ys_min, float *ys_max,
                       fint *boxLO, fint *boxHI, float minZ, float maxZ, 
                       double scale )
/*-------------------------------------------------------------------------*/
/* The axis lengths are known and the range of the data values is known.   */
/* Construct the volume cube and determine the minimum and maximum values  */
/* in screen coordinates of this volume.                                   */
/*-------------------------------------------------------------------------*/
{
   float xs[8], ys[8];
   float xlen = (float) (boxHI[0] - boxLO[0] + 1);
   float ylen = (float) (boxHI[1] - boxLO[1] + 1);
   float zmax = maxZ*(float)scale;
   float zmin = minZ*(float)scale;   
   fint  ndat = 8;
   
  
   coord2screen( 0.0,  0.0,  zmax, &xs[0], &ys[0] );
   coord2screen( xlen, 0.0,  zmax, &xs[1], &ys[1] );
   coord2screen( xlen, ylen, zmax, &xs[2], &ys[2] );   
   coord2screen( 0.0,  ylen, zmax, &xs[3], &ys[3] );   
   coord2screen( 0.0,  0.0,  zmin, &xs[4], &ys[4] );
   coord2screen( xlen, 0.0,  zmin, &xs[5], &ys[5] );
   coord2screen( xlen, ylen, zmin, &xs[6], &ys[6] );   
   coord2screen( 0.0,  ylen, zmin, &xs[7], &ys[7] );      
   
   minmax1_c( xs, &ndat, xs_min, xs_max );
   minmax1_c( ys, &ndat, ys_min, ys_max );
}


static void setupmm( float *x1mm, float *x2mm, float *y1mm, float *y2mm )
/*----------------------------------------------------------------------------*/
/* Set viewport to the dimensions of total available view surface.            */
/*----------------------------------------------------------------------------*/
{
   fint   mm = 2;
   float  nx1 = 0.0, nx2 = 1.0, ny1 = 0.0, ny2 = 1.0;
         
   pgsvp_c( &nx1, &nx2, &ny1, &ny2 );
   pgqvp_c( &mm, x1mm, x2mm, y1mm, y2mm );
   pgswin_c( x1mm, x2mm, y1mm, y2mm );
}
               

void initplot( void )
/*---------------------------------------------------------------------------*/
/* Description: Initialize plot software. Set viewport and output dimensions.*/
/*              If output device is a printer, ask user for linewidth.       */
/*---------------------------------------------------------------------------*/
{
   fint    Funit;                  /* Ignored by 'pgbeg', use 0 */
   fchar   Ffile;                  /* Device specification */
   fint    Fnxsub, Fnysub;         /* Number of subdivisions */
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
   fint    Ferrlev;
   fint    XYsubdiv[2];            /* Number of subdivisions */
   fint    Foff;
   float   x1mm, x2mm, y1mm, y2mm;
   

   Funit = 0;                      /* Ignored by 'pgbeg' */
   fmake( Ffile, 10 );
   Ffile = tofchar( "?" );         /* 'pgbeg' will prompt the user
                                       to supply a string. */
   Fnxsub = 1;                     /* No subdivisions in plot */
   Fnysub = 1;

   do 
   {
      XYsubdiv[0] = 1;             /* Default no subdivisions in plot */
      XYsubdiv[1] = 1;
      nitems  = 2;
      dfault    = REQUEST;
      r1 = userint_c(  XYsubdiv,
                       &nitems,
                       &dfault,
                       KEY_MOSAIC,
                       MES_MOSAIC );
      agreed = (XYsubdiv[0] >= 1 && XYsubdiv[1] >= 1);
      if (!agreed)
         reject_c( KEY_MOSAIC, tofchar("Must both be >= 1!") );
   } while (!agreed);

   /* Set window and viewport */
   r1 = pgbeg_c( &Funit, Ffile, &XYsubdiv[0], &XYsubdiv[1] );
   if (r1 != 1) {
      Ferrlev = 4;
      error_c( &Ferrlev, tofchar("Cannot open output device") );
   }

   /* No NEXTPAGE= keyword */
   Foff = tobool( 0 );
   pgask_c( &Foff );


   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */

   nitems = 2;
   dfault = HIDDEN;
   uservals[0] = 0.0;
   uservals[1] = 1.0;
   r1 = userreal_c( uservals, &nitems, &dfault,
                     tofchar("PAPER="),
                     tofchar("Give width(cm), aspect ratio: [0.0,1.0]") );
   if (r1 > 0) {
      /* If width = 0.0 then the program will select the largest */
      /* view surface */
      width  = uservals[0];
      /* Convert from cm to inches */
      width /= 2.54;
      aspect = uservals[1];
      (void) pgpap_c( &width, &aspect );
   }

   /* Get device-type code name of the current PGPLOT device */
   /* If the destination is a printer (=every destination  */
   /* except the Tektronix device), use thick lines in the plot */

   len = 20;
   fmake(devtype, 20);
   pgqinf_c( tofchar("HARDCOPY"), devtype, &len );
   do 
   {   
      if (len == 3)
      {
         /* A hardcopy */
         Flinewidth = 2;
      }
      else 
         Flinewidth = 1;

      (void) sprintf( messbuf, "Give line width (1-21):     [%d]",
                      Flinewidth );
      nitems = 1;
      dfault = HIDDEN;
      r1 = userint_c( &Flinewidth, &nitems, &dfault,
                       tofchar("LINEWIDTH="), tofchar(messbuf) );
      agreed = ((Flinewidth >= 1) && (Flinewidth <= 21));
      if (!agreed) {
         (void) reject_c( tofchar("LINEWIDTH="),
                          tofchar("Invalid number") );
      }
   } while  (!agreed);
   pgslw_c( &Flinewidth );


   setupmm( &x1mm, &x2mm, &y1mm, &y2mm );
   aspectratio = (y2mm - y1mm) / (x2mm - x1mm );
   
   /* If however the sub divisions in X and Y are not equal */
   /* we have to correct the aspect ratio again.            */
   
   aspectratio *= (float) XYsubdiv[1] / (float) XYsubdiv[0];
}



void inithiddenline( float Ymin, float Ymax )
/*----------------------------------------------------------------------*/
/* Keep track of the floating horizons of lower values and upper values */
/* of screen coordinates.                                               */
/*----------------------------------------------------------------------*/
{
   int cell;

   for (cell = 0; cell < MAXRESX; cell++)   
   {
      minscr[cell] = Ymax;
      maxscr[cell] = Ymin;
   }
}


static void putsetname( void )
/*-------------------------------------*/
/* Put name of set somewhere on screen */
/*-------------------------------------*/
{
   fchar   Setstr;
   fint    hidden = 2;
   float   disp, coord, fjust;
   fint    oldcolor, color;
   float   newheight, oldheight;      
   
   pgqci_c( &oldcolor  );  
   color = WHITE; 
   pgsci_c( &color );  
   pgqch_c( &oldheight );
   newheight = oldheight * 2.0;
   pgsch_c( &newheight );   
   fmake( Setstr, BIGSTORE );
   Setstr.l = usertext_c( Setstr, &hidden, KEY_INSET, MES_INSET );
   disp  = -2.0;
   coord = 0.5;
   fjust = 0.5;
   pgmtxt_c( tofchar("T"), &disp, &coord, &fjust, Setstr );   
   pgsch_c( &oldheight );   
   pgsci_c( &oldcolor );        
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
   char      message[1024];
   fint      oldcolor, color;


   pgqci_c( &oldcolor  );
   color = WHITE; pgsci_c( &color );  
   pgqch_c( &oldheight );
   newheight = oldheight/1.4;
   pgsch_c( &newheight );
   fmake( Idstr, 160 );
   getusernam_c( Idstr );
   sprintf( message, "%.*s", nelc_c( Idstr ), Idstr.a );
   getdate_c( Idstr );
   sprintf( message, "%.*s %.*s", strlen(message), message, 
            nelc_c( Idstr ), Idstr.a );
   disp  = -2.0;
   coord = 0.5;
   fjust = 0.5;
   pgmtxt_c( tofchar("B"), &disp, &coord, &fjust, tofchar(message) );
   pgsch_c( &oldheight );
   pgsci_c( &oldcolor );     
}



static float textangle( float x2, float y2, 
                        float x1, float y1, float *fjust )
/*---------------------------------------------------------*/
/* Adjust angles for the axis titles along the x- &y axis. */
/*---------------------------------------------------------*/
{
   double  angle;
   
   angle = TODEG(atan2( y2-y1, x2-x1 ));
   *fjust = 1.0;
   if (angle > 90.0)
   {
      angle -= 180.0;
      *fjust = 0.0;
   }
   else if (angle < -90.0)
   {
      angle += 180.0;
      *fjust = 0.0;
   }
   return( (float) angle );
}



static void redrawcube( double phi, fint *boxLO, fint *boxHI,
                        float minZ, float maxZ, double scale )
/*--------------------------------------------------------------*/
/* Redraw the visible parts of the enclosing cube, so that they */
/* appear as visible.                                           */
/*--------------------------------------------------------------*/
{
   float xs[8], ys[8];
   float xlen, ylen;
   float zmin = minZ * (float) scale;
   float zmax = maxZ * (float) scale;
   fint  linewidth;
   fint  color;
   float fjust;
   float angle;
   float xend, yend;
   float xor, yor;   
      
   
   /* First pixel at 0, last pixel at 'xlen', so xlen is */
   /* not the length of the axis!                        */
   xlen = (float) (boxHI[0] - boxLO[0] );
   ylen = (float) (boxHI[1] - boxLO[1] );
  
   coord2screen( 0.0,  0.0,  zmax, &xs[0], &ys[0] ); 
   coord2screen( xlen, 0.0,  zmax, &xs[1], &ys[1] ); 
   coord2screen( xlen, ylen, zmax, &xs[2], &ys[2] );    
   coord2screen( 0.0,  ylen, zmax, &xs[3], &ys[3] );
   
   coord2screen( 0.0,  0.0,  zmin, &xs[4], &ys[4] );
   coord2screen( xlen, 0.0,  zmin, &xs[5], &ys[5] );
   coord2screen( xlen, ylen, zmin, &xs[6], &ys[6] );
   coord2screen( 0.0,  ylen, zmin, &xs[7], &ys[7] );        

   color = RED; pgsci_c( &color );
   pgqlw_c( &linewidth );
   linewidth += 1;                  /* Make it bigger for the axes */
   pgslw_c( &linewidth );
   
   if (phi < 90.0)
   {
      /* The top */
      pgmove_c( &xs[0], &ys[0] );
      pgdraw_c( &xs[1], &ys[1] );
      pgdraw_c( &xs[2], &ys[2] );
      pgdraw_c( &xs[3], &ys[3] );
      pgdraw_c( &xs[0], &ys[0] );            
   }
   
   if (phi > 90.0)
   {
      /* The bottom */   
      pgmove_c( &xs[4], &ys[4] );
      pgdraw_c( &xs[5], &ys[5] );         
      pgdraw_c( &xs[6], &ys[6] );         
      pgdraw_c( &xs[7], &ys[7] );         
      pgdraw_c( &xs[4], &ys[4] );
   }

   if (theta < 0.0 && theta >= -90.0)
   {
      pgmove_c( &xs[1], &ys[1] );
      pgdraw_c( &xs[5], &ys[5] );         
   }      
   if (theta >= 0.0 && theta < 90.0)
   {
      pgmove_c( &xs[2], &ys[2] );
      pgdraw_c( &xs[6], &ys[6] );         
   }
   if (theta >= 90.0 && theta < 180.0)
   {
      pgmove_c( &xs[3], &ys[3] );
      pgdraw_c( &xs[7], &ys[7] );
   }
   
   /* Origin */
   coord2screen( 0.0, 0.0, 0.0, &xor, &yor );  
           
   color = BLUECYAN; pgsci_c( &color );   
   coord2screen( xlen, 0.0, 0.0, &xend, &yend );
   angle = textangle( xend, yend, xor, yor, &fjust );
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar(xtitle) );

   /* Y-axis */
   coord2screen( 0.0, ylen, 0.0, &xend, &yend );
   angle = textangle( xend, yend, xor, yor, &fjust );   
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar(ytitle) );   

   /* Z-axis */
   coord2screen( 0.0, 0.0, zmax, &xend, &yend );
   angle = 0.0;
   fjust = 0.5;
   yend = yend + (yend-yor)/DELTA;   
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar(ztitle) );   

   linewidth -= 1;                  /* Reset */
   pgslw_c( &linewidth );
}



static void drawaxes( float xmin, float xmax, float ymin, float ymax,
                      float minZ, float maxZ, fint *boxLO, fint *boxHI,
                      char *xtitle, char *ytitle, char *ztitle, double scale )
/*----------------------------------------------------------------------*/
/* Draw a coordinate frame.                                             */
/*----------------------------------------------------------------------*/
{

   float   xend, yend, xor, yor;
   float   xlen, ylen;
   fint    linewidth;
   fint    color, oldcolor;
   float   zmin = minZ * (float) scale;
   float   zmax = maxZ * (float) scale;   
   float   xs[8], ys[8];
   float   angle;
   float   fjust;
   float   window_minmax[4];   
   float   plotaspectratio;   /* height/width */
   float   delta;

   /*-----------------------------------------------*/
   /* Advance plotter to a new (sub-)page,          */
   /* clearing the screen if necessary.             */
   /*-----------------------------------------------*/
   pgpage_c();

   /*-----------------------------------------------*/
   /* Change the window in world coordinate space   */
   /* that is to be mapped on to the viewport.      */
   /*-----------------------------------------------*/

   plotaspectratio = (ymax-ymin)/(xmax-xmin);
   /* Restore aspect ratio and center plot: */   
   if (plotaspectratio < 1.0) /* width > height */
   {
      delta = (xmax - xmin) * ( 1.0/plotaspectratio - 1.0 );      
      xmin -= delta/2.0;
      xmin += delta/2.0;
   }
   else
   {      
      delta = (ymax - ymin) * ( 1.0/plotaspectratio - 1.0 );   
      ymin -= delta/2.0;
      ymax += delta/2.0;
   }

   /* Restore screen aspect ratio */
   if (aspectratio < 1.0) /* width > height */
   {
      xmin /= aspectratio;
      xmax /= aspectratio;
   }
   else
   {
      ymin *= aspectratio;
      ymax *= aspectratio;
   }
   /* Create some extra space around plot */
   {
      float dx = (xmax - xmin) / 10.0;
      float dy = (ymax - ymin) / 10.0;
      xmin -= dx; ymin -= dy;
      xmax += dx; ymax += dy;
   }   
   
   
   (void) sprintf( messbuf, "Window Xlo Ylo Xhi Yhi: [%.2f %.2f %.2f %.2f]",
                   xmin, ymin, xmax, ymax );
   
   window_minmax[0] = xmin;
   window_minmax[1] = ymin;
   window_minmax[2] = xmax;
   window_minmax[3] = ymax;
   nitems = 4;
   dfault = HIDDEN;
   r1     = userreal_c( window_minmax,
                        &nitems, &dfault,
                        KEY_WINDOW,
                        tofchar( messbuf ) );
   cancel_c( KEY_WINDOW );
   xmin = window_minmax[0];
   ymin = window_minmax[1];
   xmax = window_minmax[2];
   ymax = window_minmax[3];
   
    
   pgswin_c( &xmin, &xmax, &ymin, &ymax );   
 
   pgqci_c( &oldcolor );
   color = BLUE; pgsci_c( &color );

   /* First pixel at 0, last pixel at 'xlen', so xlen is */
   /* not the length of the axis!                        */
   xlen = (float) (boxHI[0] - boxLO[0]);
   ylen = (float) (boxHI[1] - boxLO[1]);

   /* Plot axes */
   pgqlw_c( &linewidth );           /* Get original linewidth */
   linewidth += 2;                  /* Make it bigger for the axes */
   pgslw_c( &linewidth );


   /* Origin */
   coord2screen( 0.0, 0.0, 0.0, &xor, &yor );

   /* X-axis */
   pgmove_c( &xor, &yor );
   coord2screen( xlen, 0.0, 0.0, &xend, &yend );
   pgdraw_c( &xend, &yend );
   angle = textangle( xend, yend, xor, yor, &fjust );
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar(xtitle) );

   /* Y-axis */
   pgmove_c( &xor, &yor );
   coord2screen( 0.0, ylen, 0.0, &xend, &yend );
   pgdraw_c( &xend, &yend );
   angle = textangle( xend, yend, xor, yor, &fjust );   
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar(ytitle) );   

   /* Z-axis */
   pgmove_c( &xor, &yor );
   coord2screen( 0.0, 0.0, zmax, &xend, &yend );
   pgdraw_c( &xend, &yend );
   angle = 0.0;
   fjust = 0.5;
   yend = yend + (yend-yor)/DELTA;
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar(ztitle) );   

   linewidth -= 2;                   /* Reset width to original value */
   pgslw_c( &linewidth );

   /* Draw the volume cube */   
   color = RED; pgsci_c( &color );
   coord2screen( 0.0,  0.0,  zmax, &xs[0], &ys[0] ); 
   coord2screen( xlen, 0.0,  zmax, &xs[1], &ys[1] ); 
   coord2screen( xlen, ylen, zmax, &xs[2], &ys[2] );    
   coord2screen( 0.0,  ylen, zmax, &xs[3], &ys[3] );
   
   coord2screen( 0.0,  0.0,  zmin, &xs[4], &ys[4] );
   coord2screen( xlen, 0.0,  zmin, &xs[5], &ys[5] );
   coord2screen( xlen, ylen, zmin, &xs[6], &ys[6] );
   coord2screen( 0.0,  ylen, zmin, &xs[7], &ys[7] );            

   pgmove_c( &xs[0], &ys[0] );
   pgdraw_c( &xs[1], &ys[1] );
   pgdraw_c( &xs[2], &ys[2] );
   pgdraw_c( &xs[3], &ys[3] );
   pgdraw_c( &xs[0], &ys[0] );            
   pgdraw_c( &xs[4], &ys[4] );               

   pgslw_c( &linewidth );
   pgdraw_c( &xs[5], &ys[5] );         
   pgdraw_c( &xs[6], &ys[6] );         
   pgdraw_c( &xs[7], &ys[7] );         
   pgdraw_c( &xs[4], &ys[4] );
   
     
   pgmove_c( &xs[1], &ys[1] );                     
   pgdraw_c( &xs[5], &ys[5] );
   
   pgmove_c( &xs[2], &ys[2] );                     
   pgdraw_c( &xs[6], &ys[6] );
   
   pgmove_c( &xs[3], &ys[3] );                     
   pgdraw_c( &xs[7], &ys[7] );     

   (void) sprintf( messbuf, "theta (x)=%.2f phi (z)=%.2f rho=%.2f",
                   theta, phi, rho );
   anyoutC( 1, messbuf );
   (void) sprintf( messbuf, "scale=%f  eyetoscreen=%.2f", scale, d_eye );
   anyoutC( 1, messbuf );
   
   pgsci_c( &oldcolor );
}


static void getscale( fint *boxLO, fint *boxHI, float minZ, float maxZ, 
                      double *scale )
/*----------------------------------------------------------------------*/
/* Get a default value for the scale. The default scale is a value      */
/* that scales the z data to be in order of the length of the x- or     */
/* y- axis.                                                             */ 
/*----------------------------------------------------------------------*/
{
   fint    nitems = 1;
   fint    dfault = HIDDEN;
   double  xlen = (double) ( boxHI[0] - boxLO[1] + 1 );
   double  ylen = (double) ( boxHI[1] - boxLO[1] + 1 );
   double  zlen = fabs( maxZ - minZ );
   
   xlen *= SCALEFACT;
   ylen *= SCALEFACT;   
   *scale = MYMAX( xlen, ylen ) / zlen;
   (void) sprintf( messbuf, "Give scale for image data:   [%f]", *scale );
   r1 = userdble_c( scale, &nitems, &dfault, KEY_SCALE, tofchar(messbuf) );
}


static int xs2cell( float xs, float Xmin, float Xmax )
/*---------------------------------------------------------*/
/* An x coordinate is transformed to an integer value      */
/* from 0..MAXRES-1                                        */
/*---------------------------------------------------------*/
{
   return( (MAXRESX-1.0) * (xs - Xmin)/(Xmax - Xmin) );
}


static void polygon( float xs, float ys, char mode )
/*---------------------------------------------------------*/
/* Store points that must be connected. If mode == m, move */
/* to xs, ys, but first check whether there are points left*/
/* in the data buffer. If so, draw the defined lines.      */
/*---------------------------------------------------------*/
{
   static fint ndat = 0; 
   
   if (mode == 'm' || mode == 'M')
   {
      if (ndat > 1)
      {
         pgline_c( &ndat, Xarray, Yarray );
         ndat = 0;
      }      
      pgmove_c( &xs, &ys );
      Xarray[0] = xs;
      Yarray[0] = ys;
      ndat = 1;
   }
   else 
   {
      Xarray[ndat] = xs;
      Yarray[ndat] = ys;
      if (ndat == MAXRESX-1)
      {
         pgline_c( &ndat, Xarray, Yarray );
         ndat = 0;
         Xarray[0] = xs;
         Yarray[0] = ys;
      }
      ndat++;      
   }
}


static void drawto( int cell, float ys, float Xmin, float Xmax )
/*---------------------------------------------------------*/
/* Check interpolation point against floating horizon and  */
/* update horizon(s).                                      */
/*---------------------------------------------------------*/
{   
   float xs;

   
   xs = Xmin + (float) cell * (Xmax - Xmin)/(MAXRESX-1.0);
/*   sprintf( messbuf, "xs draw=%f", xs ); anyoutC( 1, messbuf );*/
   
   if (cell < 0 || cell > (MAXRESX-1) )
   {
      polygon( xs, ys, 'M' );
      return;
   }
   if (ys < maxscr[cell] && ys > minscr[cell])
   {
      polygon( xs, ys, 'M' );  
      return;
   }
   if (ys < minscr[cell]) minscr[cell] = ys;
   if (ys > maxscr[cell]) maxscr[cell] = ys; 
   polygon( xs, ys, 'D' );
}


static int quadrant( float angle )
/*---------------------------------------------------------*/
/* Convert an angle to its quadrant.                       */
/*---------------------------------------------------------*/
{
   angle = fmod( angle, 360.0 );
   if (angle < 0.0) 
      angle += 360.0;
   if (angle <= 90.0)  return 1;
   if (angle <= 180.0) return 2;
   if (angle <= 270.0) return 3;
   if (angle <= 360.0) return 4;   
   return 1;  /* dummy */
}


static void checkplot( float xs, float ys, 
                       int *prevcell, float *prevys,
                       float Xmin, float Xmax )
/*---------------------------------------------------------*/
/* Interpolation of cells between two values of xs.        */
/*---------------------------------------------------------*/
{
   float    yinc;
   float    nextys;
   int      cell, c;
   
  
   cell = xs2cell( xs, Xmin, Xmax );
   if (cell == *prevcell) 
   {
      if (ys != *prevys)
         drawto( cell, ys, Xmin, Xmax );         
   }
   else 
   {
      int sign = 1;

      yinc = (ys - *prevys) / (float) ((cell - *prevcell));
      nextys = *prevys;
      if (*prevcell > cell) sign = -1;
      c = *prevcell;      
      do
      {
         c += sign;
         nextys += sign*yinc;
         drawto( c, nextys, Xmin, Xmax );
      } while (c != cell);
   }
   *prevys = ys;
   *prevcell = cell;
}
                     

void drawplot( fint plotopt, float xmin, float xmax, 
               float ymin, float ymax )
/*---------------------------------------------------------*/
/* Get the transformed data from the arrays and connect the*/
/* points using a simple hidden line algorithm.            */
/*---------------------------------------------------------*/
{
   int   x, y;
   int   Xlength, Ylength;
   float xs, ys;
   fint  color;
   bool  moved = NO;
   int   prevcell;
   float prevys;
   int   q;


   Xlength = boxHI[0] - boxLO[0] + 1;
   Ylength = boxHI[1] - boxLO[1] + 1;
   Xlength /= decim[0];
   Ylength /= decim[1];

   color = GREEN;
   pgsci_c( &color );
   /* The x-direction */
   

   inithiddenline( ymin, ymax );   
   if (plotopt == 0 || plotopt == 1)
   {
      int yt;      
      for (yt = 0; yt < Ylength; yt++)
      {   
         q = quadrant( theta );
         if (q == 3 || q == 4)
            y = yt;
         else 
            y = Ylength - yt - 1;                                       
         
         if (y != 0)
         {
            moved = NO;
            for (x = 0; x < Xlength; x++)
            {
               xs = image3D_X[x][y];
               ys = image3D_Y[x][y];
               if (xs == blank || ys == blank)
                  moved = NO;
               else
               {
                  if (!moved)
                  {
                     polygon( xs, ys, 'M' );
                     moved = YES;
                     prevcell = xs2cell( xs, xmin, xmax );
                     prevys = ys;
                  }
                  else
                     checkplot( xs, ys, &prevcell, &prevys,
                                xmin, xmax );
               }
            }
         }
      }
   }

   inithiddenline( ymin, ymax );
   pgsci_c( &color );   
   /* The y-direction i.e. profiles at constant x */
   if (plotopt == 0 || plotopt == 2)
   {
      int xt;
      for (xt = 0; xt < Xlength; xt++)
      {
         q = quadrant( theta );
         if (q == 2 || q == 3)
            x = xt;    
         else
            x = Xlength - xt - 1;
            
         if (x != 0)   /* x == 0 is the x axis itself, do not overwrite */
         {
            moved = NO;
            for (y = 0; y < Ylength; y++)
            {
               xs = image3D_X[x][y];
               ys = image3D_Y[x][y];
               if (xs == blank || ys == blank)
                  moved = NO;
               else
               {
                  if (!moved)
                  {
                     polygon( xs, ys, 'M' );
                     moved = YES;
                     prevcell = xs2cell( xs, xmin, xmax );
                     prevys = ys;                  
                  }
                  else
                     checkplot( xs, ys, &prevcell, &prevys,
                                xmin, xmax );               
               }
            }                     
         }
      }
      polygon( 0.0, 0.0, 'M' );   /* Close polygon with dummy call */
   }
}



void makeplots( float xmin, float xmax, float ymin, float ymax,
                float zmin, float zmax, double scale,
                char *xtitle, char *ytitle, char *ztitle,
                double phi )
/*---------------------------------------------------------*/
/*---------------------------------------------------------*/
{

   fint  nitems;
   fint  dfault;
   fint  r1;
   fint  plotopt;


/* (void) sprintf( messbuf, "X:[%.2f,%.2f]   Y:[%.2f,%.2f]  Z:[%.2f %.2f]",
            xmin, xmax, ymin, ymax, zmin, zmax );
   anyoutC( 3, messbuf );
*/
      

   nitems   = 1;
   plotopt  = 0;
   dfault   = REQUEST;
   r1       = userint_c(  &plotopt,
                          &nitems, &dfault,
                          KEY_PLOTOPT,
                          MES_PLOTOPT );
   inithiddenline( ymin, ymax );
   drawaxes( xmin, xmax, ymin, ymax, zmin, zmax, boxLO, boxHI, 
             xtitle, ytitle, ztitle, scale );
   drawplot( plotopt, xmin, xmax, ymin, ymax );
   redrawcube( phi, boxLO, boxHI, zmin, zmax, scale );
}


void getviewpoint( double *rho, double *theta, double *phi, 
                   double *d_eye )
/*---------------------------------------------------------------*/
/* rho is the distance from origin to viewpoint. theta is angle  */
/* of viewvector with respect to positive x-axis and Phi is      */
/* angle of viewvector wrt. positive y-axis                      */
/*---------------------------------------------------------------*/
{
   fint   nitems = 1;
   fint   dfault = REQUEST;
   fint   r1;
   double dummy;


   dfault = HIDDEN;
   (void) sprintf( messbuf, "Distance of the eye from a screen:  [%.2f]",
                   *d_eye );
   r1 = userdble_c( d_eye, &nitems, &dfault, KEY_EYE, tofchar(messbuf) );
   if (*d_eye <= 0.0) 
      *d_eye = 1.0;
   
   dfault = REQUEST;
   do 
   {
      (void) sprintf( messbuf, "Distance to viewpoint:   [%.2f]", *rho );
      dummy = *rho;
      r1 = userdble_c( &dummy, &nitems, &dfault, KEY_RHO, tofchar(messbuf) );
      agreed = (dummy > 0.0);
      if (!agreed)
         reject_c( KEY_RHO, tofchar("Must be > 0.0!") );
   } while (!agreed);
   *rho = dummy;
   
   sprintf( messbuf, "Angle viewvector wrt. pos. x-axis (deg):  [%.2f]", *theta );
   dummy = *theta;
   r1 = userdble_c( &dummy, &nitems, &dfault, KEY_THETA, tofchar(messbuf) );
   *theta = dummy;
      
   do
   {
      (void) sprintf( messbuf, "Angle view vector wrt. pos. z-axis (deg):   [%.2f]", *phi );
      dummy = *phi;
      r1 = userdble_c( &dummy, &nitems, &dfault, KEY_PHI, tofchar(messbuf) );
      agreed = (dummy >= 0.0 && dummy <= 180.0);
      if (!agreed)
         reject_c( KEY_PHI, tofchar("0.0<=phi<=180") );
   } while (!agreed);
   *phi = dummy;
   
   cancel_c( KEY_RHO );  
   cancel_c( KEY_THETA );
   cancel_c( KEY_PHI );
   return;
}


static void getdecim( fint *decim )
/*---------------------------------------------------*/
/* Calculate reasonable defaults for the decimation. */
/* Get values from user.                             */
/*---------------------------------------------------*/
{
   fint   xlen = boxHI[0] - boxLO[0] + 1;
   fint   ylen = boxHI[1] - boxLO[1] + 1;      

        
   nitems = 2;
   dfault = REQUEST;   
   do
   {
      decim[0] = MYMAX( 1, xlen / 50 );
      decim[1] = MYMAX( 1, ylen / 50 );               
      (void) sprintf( messbuf, "Give decimation factors x, y:   [%d %d]",
                      decim[0], decim[1] );
      r1 = userint_c( decim, &nitems, &dfault, KEY_DECIM, tofchar(messbuf) );
      agreed = ( (decim[0] > 0) && (decim[1] > 0) );
      if (!agreed)
         reject_c( KEY_DECIM, tofchar("decim > 0 !") );
      else
      {
         agreed = ( (xlen/decim[0]) < MAXPOINTS && (ylen/decim[1]) < MAXPOINTS );
         if (!agreed)
            reject_c( KEY_DECIM, tofchar("decim(s) too small!") );
      }
   } while (!agreed);   
}


MAIN_PROGRAM_ENTRY
{
   bool    cont;

   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      fchar    Task;                       /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, VERSION );   /* Show task and version */
   }

   setfblank_c( &blank );
   fmake(Setin, 80);

   /*-------------------------------------------------------------------------*/
   /* Because Fortran passes all arguments by reference, all C functions with */
   /* a Fortran equivalent must do this also (GIPSY programmers guide,        */
   /* Chapter 9).                                                             */
   /*-------------------------------------------------------------------------*/



   /*------------------------*/
   /* Get the (sub)set(s)    */
   /*------------------------*/
   dfault = NONE;
   subdim = 2;              /* Subset must be two dimensional */
   scrnum = 8;
   nsubs  = gdsinp_c( Setin,
                      subin,
                      &maxsubs,
                      &dfault,
                      KEY_INSET,
                      MES_INSET,
                      &scrnum,
                      axnum,
                      axcount,
                      &maxaxes,
                      &class,
                      &subdim );

   setdim = gdsc_ndims_c( Setin, &setlevel );

   /*-----------------------------------------------------*/
   /* Determine the edges of this its frame ( frameLO/HI) */
   /*-----------------------------------------------------*/

   r1 = 0;
   (void) gdsc_range_c( Setin,
                        &setlevel,
                        &cwlo,
                        &cwhi,
                        &r1 );
   r1 = r2 = 0;
   for (m = 0; m < (int) setdim; m++) {
      frameLO[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
      frameHI[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
   }

   /*----------------------------------------------------------------*/
   /* Prepare a box for INSET. Default is a box equal to the frame.  */
   /*----------------------------------------------------------------*/

   dfault = REQUEST;
   boxopt = 0;
   scrnum = 8;
   (void) gdsbox_c( boxLO,
                    boxHI,
                    Setin,
                    subin,
                    &dfault,
                    KEY_BOX,
                    MES_BOX,
                    &scrnum,
                    &boxopt );

   /* Count number of pixels in this substructure */
   totpixels = 1;
   /* For only one subset: */
   for(m = 0; m < (int) subdim; m++)
   {
      totpixels *= (boxHI[m] - boxLO[m] + 1);
   }
   /* And for all subsets: */
   totpixels *= nsubs;

   /* Axis lengths */
   box_Xlen = boxHI[0] - boxLO[0] + 1;
   box_Ylen = boxHI[1] - boxLO[1] + 1;


   /* Get axis names and units to create xyz axis titles */
   {
      fchar Ztitle;
      
      Ztitle.a = ztitle;
      Ztitle.l = FITSLEN;
      gdsd_rchar_c( Setin, tofchar("BUNIT"), &setlevel, Ztitle, &r1 );      
      if (r1 < 0)
         strcpy( ztitle, "Z" );
      else
         ztitle[nelc_c(Ztitle)] = '\0';
   }
      
   /* Get subset axis names */     
   
   for (m = 0; m < (int) setdim; m++)
   {
      fchar Ctype, Cunit;
      fint  colev;      
      fmake( Ctype, FITSLEN );
      fmake( Cunit, FITSLEN );

      r1 = axcoord_c( Setin, &axnum[m], Ctype, Cunit, &colev );
      if (m == 0)
      {
         if (r1 != 0)         
            strcpy( xtitle, "X" );
         else
            strcpy( xtitle, strtok( Ctype.a, " -" ) );
      }
      else
      {
         if (r1 != 0)         
            strcpy( ytitle, "Y" );
         else
            strcpy( ytitle, strtok( Ctype.a, " -" ) );
      }  
   }
             


   /*------------------------------------------*/
   /* Some user input for plotting purposes.   */
   /*------------------------------------------*/
   {
      fint   xlen = boxHI[0] - boxLO[0] + 1;
      fint   ylen = boxHI[1] - boxLO[1] + 1;
             
      theta = 30.0;
      phi   = 60.0;   
      d_eye = 8.0 * MYMAX( xlen, ylen );      
      rho   = d_eye / 2.0;      
   }


   initplot();
   do
   {   
      fint     tidIN;

      /*--------------------------------------*/
      /* Loop over all specified subsets.     */
      /*--------------------------------------*/
            
      for(subnr = 0; subnr < nsubs; subnr++)
      {
         int  line;

         getviewpoint( &rho, &theta, &phi, &d_eye );      
         getdecim( decim );
         initviewtransform( rho, theta, phi );       
         
         /* Make coordinate words for these corners */
         cwlo = gdsc_fill_c( Setin, &subin[subnr], boxLO );
         cwhi = gdsc_fill_c( Setin, &subin[subnr], boxHI );

         boxminmax( cwlo, cwhi, &minZ, &maxZ, Setin, subin[subnr] );
         getscale( boxLO, boxHI, minZ, maxZ, &scale );

         findscreenminmax( &xs_min, &xs_max, &ys_min, &ys_max,
                           boxLO, boxHI, minZ, maxZ, scale ); 
       
      
         /* Read in data line by line */
         for (line = boxLO[1]; line <= boxHI[1]; line++)
         {
            fint  newboxLO[2], newboxHI[2];
            newboxLO[0] = boxLO[0]; newboxLO[1] = line;
            newboxHI[0] = boxHI[0]; newboxHI[1] = line;
            cwlo  = gdsc_fill_c( Setin, &subin[subnr], newboxLO );
            cwhi  = gdsc_fill_c( Setin, &subin[subnr], newboxHI );
            tidIN = 0;
            gdsi_read_c( Setin, 
      	                 &cwlo, &cwhi, 
      	                 imageIN,
                         &maxiobuf, 
                         &pixelsdone, 
                         &tidIN );
            for (i = 0; i < (int) pixelsdone; i++)
            {
               xw = i;
               yw = line - boxLO[1];
                              
               if ( ( ((int) xw) % decim[0] == 0) &&
                    ( ((int) yw) % decim[1] == 0) )
               {
                  Xpos = (int) xw / decim[0];
                  Ypos = (int) yw / decim[1];
                  if (imageIN[i] != blank)
                  {                  
                     zw = imageIN[i] * scale;
                     coord2screen( xw, yw, zw, &xs, &ys );   
                  }
                  else 
                  {
                     xs = ys = blank;
                  }
                  image3D_X[Xpos][Ypos] = xs;
                  image3D_Y[Xpos][Ypos] = ys;                     
               }
            }                       
         } 
         makeplots( xs_min, xs_max, ys_min, ys_max, minZ, maxZ, scale,
                    xtitle, ytitle, ztitle, phi );
      }                                           /* All subsets done? */   
      cont = toflog( true );
      dfault = REQUEST;
      r1 = userlog_c( &cont, &nitems, &dfault, KEY_CONTINUE,
                      tofchar("Continue?         [Y}/N") );
      cont = tobool( cont );      
      cancel_c( KEY_CONTINUE );
   } while (cont);
   putid();                /* User id at bottom */   
   putsetname( );          /* Input set name at top */

   pgend_c();
   finis_c();                                     /* Quit Hermes */
   return( 0 );
}
