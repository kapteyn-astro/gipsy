/* beam.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            beam.dc2

Function:     BEAM

Purpose:      Draw elliptical shaded beam (PGPLOT)

Category:     PLOTTING

File:         beam.c

Author:       M. Vogelaar

Use:          INTEGER BEAM( 
                         SET,        Input    character*(*)
                         SUBSET,     Input    integer
                         SPATIAL,    Input    integer
                         DELTA,      Input    double precision array
                         CENTREXY,   Input    double precision array
                         FWHMMAJ,    Input    double precision
                         FWHMMIN,    Input    double precision
                         PA,         Input    double precision
                         LINES,      Input    integer
                         SLOPE       Input    double precision
                         SHAPE )     Input    integer


              BEAM       Returns:
                          0  successful
                         -1  No valid shape selected
                         -2  No sky coordinates found in set
                         -3  Cannot find the grids for this beam!
              SET        GDS set (as returned by GDSINP).
              SUBSET     A single subset (as returned by GDSINP).
              SPATIAL    = 1 if both subset axes are spatial 
                         = 0 other combinations 
              DELTA      Grid spacing in X and Y. In the calling
                         environment this parameter is set to 
                         header values of cdelt or ddelt or
                         the parameter is calculated some other way.
              CENTREXY   X and Y position of centre of ellipse in
                         world coordinates/grids.
              FWHMMAJOR  FWHM major axis of ellipse in degrees!
              FWHMMINOR  FWHM minor axis of ellipse in degrees!
              PA         Position angle of ellipse wrt. north in the 
                         direction of the east, in degrees.
              LINES      Approximation of number of shading lines
                         for ellipse.
              SLOPE      Slope of shading lines in degrees.
                         This slope is defined in the system
                         of the grids i.e. slope = 0 is draws 
                         shading line parallel to the X axis.
              SHAPE      1: Plot shaded ellipse
                         2: Plot rectangle (e.g.IRAS beam)
                         3: Plot cross


Description:  Draw an beam with an elliptical or rectangular shape or a 
              cross with specifications (origin, axes and projection 
              angle). The beam axes are specified in degrees and all 
              calculated ellipse coordinates are transformed to grids 
              using the transformation formulas for sky system and
              projection.  The position angle of the major
              axis is wrt. +m axis. This angle is corrected for the 
              rotation of the image (CROTA). 
              If user wants shading (parallel line pattern only) you can 
              give the slope of the lines (wrt +X axis) in 'SLOPE' and the 
              approximate number of shade lines in 'DELTA'.
              
             
Updates:      Dec  9, 1991: MV,  Document created
              Sep 19, 1994: MV,  Rectangular beam implemented
              Jul  7, 1997: MV,  Cross implemented
              Sep 30, 1997: MV,  Completely rewritten 'pgbeam'.

#<

Fortran to C interface:

@ integer function beam(  character, 
@                         integer, 
@                         integer, 
@                         double precision, 
@                         double precision, 
@                         double precision, 
@                         double precision, 
@                         double precision, 
@                         integer, 
@                         double precision, 
@                         integer )
*/

#include "gipsyc.h"
#include "math.h"
#include "pgmove.h"
#include "pgdraw.h"
#include "pgline.h"
#include "skyrot.h"
#include "pgpt.h"
#include "grtoph.h"
#include "phtogr.h"
#include "pgqwin.h"
#include "userfio.h"

#define DEG(a)           ( (a) * 57.295779513082320876798155 )
#define ABS(a)           ( (a) < 0 ? (-(a)) : (a) )
#define RAD(a)           ( (a) * 0.017453292519943295769237 )
#define MYMAX(a,b)       ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)       ( (a) > (b) ? (b) : (a) )
#define YES              1
#define NO               0



static int dispcoord( double    longitude,
                      double    latitude,
                      double    disp,
                      double    angle,
                      double    direction,
                      double   *longout,
                      double   *latout )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate new longitude, latitude after a dis-    */
/*          placement in the sky in a direction given by an   */
/*          angle.                                            */
/* INPUT:   longitude: enter in degrees.                      */
/*          latitude:  enter in degrees.                      */
/*          disp:      the displacement in the sky entered    */
/*                     in degrees. The value can also be      */
/*                     negative to indicate the opposite      */
/*                     direction.                             */
/*          angle:     the angle wrt. a great circle of       */
/*                     constant declination entered in        */
/*                     degrees.                               */
/*          direction: If the longitude increases in the -X   */
/*                     direction (e.q. RA-DEC) then direction */
/*                     is -1. else direction = +1             */
/*                                                            */
/* Assume a triangle on a sphere with side b(=disp) connec-   */
/* ting two positions along a great circle and sides 90-d1,   */
/* and 90-d2 (d1, d2 are the declinations of the input and    */
/* output positions) that connect the input and output        */
/* position to the pole P of the sphere. Then the distance    */
/* between the two points Q1=(a1,d1) and Q2=(a2,d2) is:       */
/* cos(b)=cos(90-d1)cos(90-d2)+sin(90-d1)sin(90-d2)cos(a2-a1) */
/* Q2 is siuated to the left of Q1.                           */
/* If the angle PQ1Q2 is alpha then we have anoher cosine     */
/* rule:                                                      */
/* cos(90-d2) = cos(b)cos(90-d1)+sin(b)sin(90-d1)cos(alpha)   */
/* or :                                                       */
/* sin(d2) = cos(b)sin(d1)+sin(b)cos(d1)cos(alpha)            */
/* which gives d2. Angle Q1PQ2 is equal to a2-a1. For this    */
/* angle we have the sine formula:                            */
/* sin(b)/sin(a2-a1) = sin(90-d2)/sin(alpha) so that:         */
/* sin(a2-a1) = sin(b)sin(alpha)/cos(d2).                     */
/* b,alpha and d2 are known -> a2.                            */
/*------------------------------------------------------------*/
{
   double   alpha;
   double   b  = ABS(RAD(disp));
   double   a1 = RAD(longitude), a2;
   double   d1 = RAD(latitude),  d2;
   double   dH;


   if (disp < 0.0)
      angle += 180.0;
   
   alpha = RAD(angle);     
   d2 = asin( cos(b)*sin(d1)+cos(d1)*sin(b)*cos(alpha) );   
   dH = direction * asin( sin(b)*sin(alpha)/cos(d2) );
   
   /* Note that a2 is to the left of a1 and direction = -1 */
   /* if cdelt[0] < 0                                      */
   
   a2 = a1 - dH;
  
   *longout = DEG(a2);
   *latout  = DEG(d2);
               
   return( 1 );
}


static int inside_ellipse( fchar   Setin,
                           fint   *subset,
                           double *centralposphys,
                           double *gridpos,
                           double  minor2,             /* minor axis^2 in deg */
                           double  major2,
                           double  PA )             /* Position angle in deg. */
/*------------------------------------------------------------*/
/* PURPOSE: Is this grid position inside the ellipse or not?  */
/*------------------------------------------------------------*/
{
   fint    r;
   double  posphys[2];
   double  Rgr;
   double  gamma;
   double  Rgamma;
   double  CosA, SinA;
   double  denom;
   
   /*------------------------------------------------------------*/
   /* Convert the arbitrary grid position into an alpha, delta   */
   /* and calculate the distance in degrees between this point   */
   /* and the central position (also in deg.).                   */
   /*------------------------------------------------------------*/
   r = grtoph_c( Setin, subset, gridpos, posphys );
   Rgr = DEG( acos( sin(RAD(centralposphys[1]))*sin(RAD(posphys[1]))+
               cos(RAD(centralposphys[1]))*cos(RAD(posphys[1]))*
               cos(RAD(posphys[0]-centralposphys[0])) 
             ) );
 
   /*------------------------------------------------------------*/
   /* To find out whether this Rgr is inside the ellipse, we     */
   /* have to calculate what the maximum radius is at an angle   */
   /* gamma. Angle gamma is the angle between made up by the     */
   /* arbitrary gridpoint, the central position of the ellipse   */
   /* and the pole.                                              */
   /*------------------------------------------------------------*/
   if (Rgr == 0.0)
      gamma = 0.0;
   else
      gamma = DEG( asin( cos(RAD(posphys[1]))*
                    sin(RAD(posphys[0]-centralposphys[0]))/
                    sin(RAD(Rgr)) 
                  ) );
   if (posphys[1] < centralposphys[1])
      gamma += PA;           /* Correct for the position angle of the ellipse */
   else
      gamma -= PA;
   
   /*------------------------------------------------------------*/
   /* Solve for R(gamma)_max with the method described in the    */
   /* beam_c function.                                           */
   /*------------------------------------------------------------*/
   CosA = cos(RAD(gamma));
   SinA = sin(RAD(gamma));
   denom = (minor2*CosA*CosA + major2*SinA*SinA);
   if (denom != 0.0)
      Rgamma = sqrt( minor2*major2 / denom );
   else
      Rgamma = 0;
   return (Rgr <= Rgamma);
}


fint beam_c( fchar   Setin,
             fint   *subset,
             fint   *spatial,
             double *cdelt,                 /* grid spacing in x and y */
             double *Centre_XY,             /* Centre in grids */
             double *fwhm_major,            /* Axes in physical coordinates */
             double *fwhm_minor,
             double *Posangle,              /* Angle between major and X */
             fint   *Lines,                 /* Approximate number of shade lines */
             double *Line_slope,            /* Slope of shade lines */
             fint   *Line_shape )           /* Ellipse, rectangle or cross */
/*------------------------------------------------------------*/
/* Draw ellipse with area fill. The beam can also be plotted  */
/* as a rectangle.                                            */
/*------------------------------------------------------------*/
{
   double    minor, major;                  /* FWHM's */
   double    minor2, major2;
   double    minor2major2;   
   double    CosA, SinA;                    /* Angles */
   double    Alpha;                         /* Used in Polar coordinates */
   double    R;                             /* Radius used in Polar coordinates */
   double    Denom;                         /* Help var. */
   double    physpos[2];
   double    gridpos[2];      
   double    direction;
   double    crota;
   double    PA;
   double    cpos[2];   
   float     Xpoints[368], Ypoints[368];    /* Outline ellipse in arrays */
   int       i;                             /* Array index */
   fint      Numpoints;                     /* Number of points in arrays */
   fint      r;


   /* Is a valid shape selected? */
   if (*Line_shape < 1 || *Line_shape > 3)
   {
      anyoutf( 1, "BEAM: No valid shape selected" );
      return( -1 );
   }
   
  
   if (*spatial == 0)
   /*------------------------------------------------------------*/
   /* A set exists, but the subset axes or not longitude and     */
   /* latitude. Then a postion angle has no meaning and is set   */
   /* to zero. The shape can only be a cross. The input 'cdelt'  */
   /* can be either a header CDELT or DDELT or a grid spacing    */
   /* calculated in the calling environment.                     */
   /*------------------------------------------------------------*/
   {
      double X = *fwhm_major/2.0/cdelt[0];
      double Y = *fwhm_minor/2.0/cdelt[1];
      float  x, y;      

      x = (float)  (Centre_XY[0] - X);
      y = (float)  Centre_XY[1];
      pgmove_c( &x, &y );
      x = (float)  (Centre_XY[0] + X);
      pgdraw_c( &x, &y );
      x = (float)  Centre_XY[0];
      y = (float)  (Centre_XY[1] - Y);
      pgmove_c( &x, &y );
      y = (float)  (Centre_XY[1] + Y);
      pgdraw_c( &x, &y );
      return( 0 );
   }
   

   minor = MYMIN( (*fwhm_minor/2.0),  (*fwhm_major/2.0) );
   major = MYMAX( (*fwhm_minor/2.0),  (*fwhm_major/2.0) );
   
   r = skyrot_c( Setin, &crota );
   if (r == -1)
   {
      anyoutf( 1, "BEAM: No sky coordinates found in set." );
      return( -2 );
   }
   
   /*----------------------------------------*/
   /* Angle in spatial map is an angle       */
   /* between the north and the the major    */
   /* axis in the direction of the +m axis.  */
   /* This angle is corrected for CROTA.     */
   /* CROTA is the angle in degrees between  */
   /* the +y axis and the +m axis (latitude  */
   /* axis) at the position of the PC. The   */
   /* angle is counter-clockwise if the grid */
   /* separation (CDELT) in longitude        */
   /* is < 0.0. However, the algorithm uses  */
   /* coordinate transformations to convert  */
   /* to grids. Then CROTA is already taken  */
   /* into account so a correction on PA is  */
   /* not necessary.                         */
   /*----------------------------------------*/
   PA = *Posangle;                                   /* - crota not used here */

   cpos[0] = Centre_XY[0];
   cpos[1] = Centre_XY[1];
   r = grtoph_c( Setin, subset, cpos, cpos );      


   if (cdelt[0] < 0.0) 
     direction = -1.0;
   else 
     direction = 1.0;


   /* Draw the ellipse */
   if (*Line_shape == 1)
   {
      minor2 = minor * minor;
      major2 = major * major;
      minor2major2 = minor2 * major2;
        
      i = 0;                         /* Reset number of points on the ellipse */
      /*-----------------------------------------------------------*/
      /*                                2       2                  */
      /* General equation ellipse: (x/a) + (y/b) = 1 is also       */
      /* written as b^2.x^2 + a^2.y^2 = a^2.b^2                    */
      /* Substitute: x = R.cos(alpha), y = R.sin(alpha) and solve  */
      /* for R. Then                                               */
      /*                          a^2.b^2                          */
      /* R = sqrt( ------------------------------------- )         */
      /*            (b.cos(alpha))^2 + (a.sin(alpha))^2            */
      /*                                                           */
      /* Repeat this calculation for angles between 0 and 360 deg. */
      /* For each displacement R on a sphere in the direction      */
      /* position angle + alpha, there is a new (a,d) in degrees   */
      /* that can be transformed to grids with the standard        */
      /* coordination transformation routines.                     */
      /*-----------------------------------------------------------*/   
      for (Alpha = 0.0; Alpha <= 360.0; Alpha += 1.0) 
      {
         CosA = cos(RAD(Alpha));
         SinA = sin(RAD(Alpha));
         Denom = (minor2*CosA*CosA + major2*SinA*SinA);
         if (Denom != 0.0)
            R = sqrt( minor2major2 / Denom );      
         else
            R = 0;
         
         /*--------------------------------------------------------------*/
         /* Here we have calculated an ellipse radius for a given angle. */
         /* With this radius and angle, determine a new (a,d) using      */
         /* two spherical trigionometry formulas. This physical          */
         /* coordinate is transformed to a grid with the standard        */
         /* coordinate transformations. Note that in this case axes and  */
         /* position angle are plotted correctly for images of all       */ 
         /* sizes and known projections.                                 */
         /*--------------------------------------------------------------*/
         dispcoord( cpos[0], cpos[1],       /* Centre in physical coordinates */
                    R,
                    PA + Alpha,
                    direction,
                    &physpos[0], &physpos[1] ); 
         r = phtogr_c( Setin, subset, physpos, gridpos );
         Xpoints[i] = (float) gridpos[0]; 
         Ypoints[i] = (float) gridpos[1];
         i++;
      }
      Numpoints = i;
      pgline_c( &Numpoints, Xpoints, Ypoints );/* Can be replaced by 'pgpoly' */
   }
   
  
   /* Draw the rectangle */
   if (*Line_shape == 2  || *Line_shape == 3)
   {
      double xb[4], yb[4];      
      int    k;

      for (i = 0; i < 4; i++)
      {          
         double   dist;      
         double   alpha;;
         if (i == 0 || i == 2)
            dist = major;
         else
            dist = minor;
         alpha = PA + ((double) i)*90.0;
         dispcoord( cpos[0], cpos[1], /* Center in ph. coords. from prev. calc. */
                    dist,
                    alpha,
                    direction,
                    &physpos[0], &physpos[1] ); 
         r = phtogr_c( Setin, subset, physpos, gridpos );
         xb[i] = gridpos[0];
         yb[i] = gridpos[1];            
         {
            fint   n = 1;
            fint   sym = 5;
            float  x = (float) xb[i];
            float  y = (float) yb[i];
            pgpt_c( &n,  &x, &y, &sym );
         }
      }
      if (*Line_shape == 2)
      {
         for (k = 0; k <= 4; k++)
         {
            double   d;
            double   x0, x1, x2, x3, y0, y1, y2, y3;
            double   mu;         
            float    x, y;
            
            x0 = xb[(0+k)%4];  x1 = xb[(1+k)%4];
            x2 = xb[(2+k)%4];  x3 = xb[(3+k)%4];
            y0 = yb[(0+k)%4];  y1 = yb[(1+k)%4];
            y2 = yb[(2+k)%4];  y3 = yb[(3+k)%4];            
            d = (x3-x1)*(y2-y0) - (y3-y1)*(x2-x0);
            if (d == 0.0)
            {
               anyoutf( 1, "BEAM: Cannot find the grids for this beam!" );
               return( -3 );
            }
            mu = ((x1-x0)*(y2-y0) - (y1-y0)*(x2-x0)) / d;
            x = (float)   (x0 + mu * (x3-x1) );
            y = (float)   (y0 + mu * (y3-y1) );
            if (k == 0)
               pgmove_c( &x, &y );
            else
               pgdraw_c( &x, &y );               
         }
      }
      else
      {
         float    x, y;
         x = (float) xb[0];
         y = (float) yb[0];
         pgmove_c( &x, &y );
         x = (float) xb[2];
         y = (float) yb[2];
         pgdraw_c( &x, &y );
         x = (float) xb[1];
         y = (float) yb[1];
         pgmove_c( &x, &y );
         x = (float) xb[3];
         y = (float) yb[3];
         pgdraw_c( &x, &y );                           
      }
   }
  
   

   /* If wanted, fill the area with straight lines */

   if (*Lines && *Line_shape == 1) 
   {
      double   Delta;                    /* Space in world coordinates between lines */
      double   Slope;                    /* Slope of area filling lines wrt major axis */
      int      inside;
      double   lambda, lambdastep;
      double   x1, x2, y1, y2;
      

      Delta = (double) *Lines;
      Slope = (double) *Line_slope;
      minor = MYMIN( (*fwhm_minor/2.0), (*fwhm_major/2.0) );
      major = MYMAX( (*fwhm_minor/2.0), (*fwhm_major/2.0) ); 
      minor2 = minor * minor;
      major2 = major * major;
      minor2major2 = minor2 * major2;
      
      /*------------------------------------------------------------*/
      /* Fill the ellipse with given PA and minor and major axes.   */
      /* Try to generate shade lines with starting points on the    */
      /* major axis of the ellipse. Extend the length of the major  */
      /* axis to get a better coverage of the shading lines.        */
      /*------------------------------------------------------------*/      
      if (cdelt[0] < 0.0) 
         direction = -1.0;                  /* Needed in function 'dispcoord' */
      else 
         direction = 1.0;      
              
      dispcoord( cpos[0], cpos[1],  /* Center in ph. coords. from prev. calc. */
                 major,
                 PA + 0.0,
                 direction,
                 &physpos[0], &physpos[1] ); 
     
      r = phtogr_c( Setin, subset, physpos, gridpos );
      x1 = gridpos[0];
      y1 = gridpos[1];
      dispcoord( cpos[0], cpos[1],
                 major,
                 PA + 180.0,
                 direction,
                 &physpos[0], &physpos[1] ); 
     
      r = phtogr_c( Setin, subset, physpos, gridpos );
      x2 = gridpos[0];
      y2 = gridpos[1];
      
      lambdastep = 1.0 / Delta;
      for (lambda = -0.5; lambda <= 1.5; lambda += lambdastep)
      {
         double  lineCxy[2];
         double  x, y;
         int     k, c;
         float   xshade, yshade;                     
         
         lineCxy[0] = x1 + lambda * ( x2 - x1 );
         lineCxy[1] = y1 + lambda * ( y2 - y1 );
         xshade = lineCxy[0];
         yshade = lineCxy[1];
                 
         /*------------------------------------------------------------*/
         /* Draw shading lines in the ellipse. Start at a sample point */
         /* on the major axis. Search for the first point inside the   */
         /* ellipse (Note that sample starting points are also selected*/
         /* outside the ellipse). Store this position. Search along a  */
         /* line with given slope wrt the xy grid coordinate system,   */
         /* for all points inside the ellipse, until a point was found */
         /* outside the ellipse. Then draw a line between the first    */
         /* point inside and the last point inside. Repeat this action */
         /* along the shade line in the other direction.               */
         /* To prevent searching forever for a point inside the ellipse*/
         /* a limit is built in ('maxsearch').                         */
         /*------------------------------------------------------------*/
         for (k = 0; k < 2; k++)
         {
            int    maxsearch = 300; /* Limit search of first point inside ellipse */            
            double delt;
            double s;            
            float  x1, x2, y1, y2;

            /*--------------------------------------------------*/
            /* Determine smallest size of window in world       */
            /* coordinates and divide that range by a fixed     */
            /* number to get a step size.                       */
            /*--------------------------------------------------*/
            pgqwin_c( &x1, &x2, &y1, &y2 );
            s = MYMIN( x2-x1, y2-y1 );
            delt = s / 500.0;

            c = 0;              
            x = 0.0;
            inside = NO;
            if (k == 1)
               delt *= -1.0;
            while (!inside && c < maxsearch)
            {
               y = tan(RAD(Slope))*x;
               gridpos[0] = x + lineCxy[0];
               gridpos[1] = y + lineCxy[1];
               inside = inside_ellipse( Setin, subset, 
                                        cpos, gridpos, 
                                        minor2, major2, PA );
               if (inside)
               {
                  xshade = (float) gridpos[0];
                  yshade = (float) gridpos[1];               
                  pgmove_c( &xshade, &yshade );
               }
               x += delt;
               c++;
            }
            while (inside)
            {
               y = tan(RAD(Slope))*x;
               gridpos[0] = x + lineCxy[0];
               gridpos[1] = y + lineCxy[1];
               inside = inside_ellipse( Setin, subset, 
                                        cpos, gridpos, 
                                        minor2, major2, PA );            
               if (inside)
               {
                  xshade = (float) gridpos[0];
                  yshade = (float) gridpos[1];
               }
               else
               {
                  pgdraw_c( &xshade, &yshade );
               }
               x += delt;
            }
         }
      }
   } /* End if user wants area fill */
   return( 0 );
} /* End of routine */

