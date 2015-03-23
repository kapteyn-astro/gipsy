/* pgbeam.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            pgbeam.dc2

Function:     PGBEAM

Purpose:      Draw elliptical shaded beam (PGPLOT)

Category:     PLOTTING

File:         pgbeam.c

Author:       M. Vogelaar

Use:          CALL PGBEAM(    XCENTRE,    Input    real
                              YCENTRE,    Input    real
                              MAJOR,      Input    real
                              MINOR,      Input    real
                              PA,         Input    real
                              DELTA,      Input    real
                              SLOPE       Input    real
                              SHAPE )     Input    integer

              XCENTRE    X position of centre of ellipse in
                         world coordinates.
              YCENTRE    Same for Y position
              MAJOR      Major axis of ellipse
              MINOR      Minor axis of ellipse
              PA         Position angle of ellipse wrt. pos x-axis
                         in degrees.
              DELTA      Separation shading lines in world
                         coordinates.
              SLOPE      Slope of shading lines in degrees.
              SHAPE      1: Plot shaded ellipse
                         2: Plot shaded rectangle (e.g.IRAS beam)
                         3: Plot cross


Description:  Draw an elliptical beam with specifications (origin and axes)
              in world coordinates. The position angle of the major
              axis is wrt. the pos. x-axis. The ellipse is rotated
              counter-clockwise. If user wants shading (line pattern
              only) you can give the slope of the lines in 'SLOPE'
              and the distance between the lines in world coordinates
              'DELTA' in the y-direction in an unrotated frame.
              However, lines are plotted in the rotated frame.

Updates:      Dec  9, 1991: MV,  Document created
              Sep 19, 1994: MV,  Rectangular beam implemented
              Jul  7, 1997: MV,  Cross implemented

#<

Fortran to C interface:

@subroutine pgbeam( real, real, real, real, real, real, real, integer )

*/

#include "gipsyc.h"
#include "math.h"
#include "pgmove.h"
#include "pgdraw.h"
#include "pgline.h"

void pgbeam_c(    float *Centre_X,          /* Centre in grids */
                  float *Centre_Y ,
                  float *Major_axis,        /* axes in grids */
                  float *Minor_axis,
                  float *Posangle,
                  float *Line_delta,        /* Distance between shade lines */
                  float *Line_slope,        /* Slope of shade lines */
                  fint  *shape )            /* Ellipse or rectangle */
/*------------------------------------------------------------*/
/* Draw ellipse with area fill. The beam can also be plotted  */
/* as a rectangle.                                            */
/*------------------------------------------------------------*/
{
#define RAD(a)  ( a * 0.017453292519943295769237 )

   double    Cpx, Cpy;                      /* Central position x, y */
   double    Major, Minor;                  /* Axis of ellipse */
   double    CosP, SinP;                    /* Angles */
   double    CosA, SinA;                    /* Angles */
   double    Pa;                            /* Position angle */
   double    Alpha;                         /* Used in Polar coordinates */
   double    R;                             /* Radius used in Polar coordinates */
   double    Denom;                         /* Help var. */
   double    Xell, Yell;                    /* Points of not rotated ellipse */
   double    Xrot, Yrot;                    /* X,Yell rotated over Pos.angle */
   float     Xpoints[368], Ypoints[368];    /* Outline ellipse in arrays */
   int       i;                             /* Array index */
   fint      Numpoints;                     /* Number of points in arrays */


   /* Convert for convenience */

   Cpx   = (double) *Centre_X;
   Cpy   = (double) *Centre_Y;
   Minor = fabs((double) *Minor_axis);
   Major = fabs((double) *Major_axis);
   Pa    = (double) *Posangle;
   CosP  = cos( RAD(Pa) );
   SinP  = sin( RAD(Pa) );
   Xell  = 0.0;
   Yell  = 0.0;
   
  
   /* Is a valid shape selected? */
   if (*shape < 1 || *shape > 3)
      return;
 
   /* Draw the ellipse */
   if (*shape == 1)
   {
      i = 0;
      for (Alpha = 0.0; Alpha <= 360.0; Alpha += 1.0) {
         /*-----------------------------------------------------------*/
         /* Ellipse: b^2.x^2 + a^2.y^2 = a^2.b^2                      */
         /* Substitute: x = r.cos(alpha), y = r.sin(alpha) and solve  */
         /* for r. Repeat this action for angles between 0 and 90 deg */
         /*-----------------------------------------------------------*/
         CosA = cos(RAD(Alpha));
         SinA = sin(RAD(Alpha));
         Denom = (Minor*CosA * Minor*CosA + Major*SinA * Major*SinA);
         if (Denom == 0.0) {
            R = 0;
         }
         else {
            R = sqrt( Minor*Major * Minor*Major / Denom );
         }
         Xell = R * CosA;
         Yell = R * SinA;
         /* We have a point on this ellipse, now rotate this point */
         /* and move to given origin */
         Xrot = Xell * CosP - Yell * SinP  + Cpx;
         Yrot = Xell * SinP + Yell * CosP  + Cpy;
         Xpoints[i] = (float) Xrot; Ypoints[i] = (float) Yrot;
         i++;
      }
      Numpoints = i;
      pgline_c( &Numpoints, Xpoints, Ypoints );   /* Can be replaced by 'pgpoly' */
   }


   /* Draw the rectangle */
   if (*shape == 2)
   {
      for (i = 0; i < 5; i++)
      {
         if (i == 0) {  Xell = -Major; Yell = -Minor; }
         if (i == 1) {  Xell =  Major; Yell = -Minor; }
         if (i == 2) {  Xell =  Major; Yell =  Minor; }
         if (i == 3) {  Xell = -Major; Yell =  Minor; }
         if (i == 4) {  Xell = -Major; Yell = -Minor; }
         Xrot = Xell * CosP - Yell * SinP  + Cpx;
         Yrot = Xell * SinP + Yell * CosP  + Cpy;
         Xpoints[i] = (float) Xrot; Ypoints[i] = (float) Yrot;
      }
      Numpoints = i;
      pgline_c( &Numpoints, Xpoints, Ypoints );   /* Can be replaced by 'pgpoly' */
   }

   if (*shape == 3)
   {
      float    x, y, x1, y1, x2, y2;
      float    rx, ry;
      
      /* Draw the cross itself */
      Xell = Major;  Yell = 0.0;                     /* One part of FWHM axis */
      Xrot = Xell * CosP - Yell * SinP  + Cpx;       /* Rotate end point */
      Yrot = Xell * SinP + Yell * CosP  + Cpy;
      x1 = (float) Xrot;   y1 = (float) Yrot;
      Xell = -Major;  Yell = 0.0;
      Xrot = Xell * CosP - Yell * SinP  + Cpx;
      Yrot = Xell * SinP + Yell * CosP  + Cpy;
      x2 = (float) Xrot;   y2 = (float) Yrot;      
      pgmove_c( &x1, &y1 );      
      pgdraw_c( &x2, &y2 );
            
      /* The minor axes */               
      Xell = 0.0;  Yell = Minor;
      Xrot = Xell * CosP - Yell * SinP  + Cpx;
      Yrot = Xell * SinP + Yell * CosP  + Cpy;
      x1 = (float) Xrot;   y1 = (float) Yrot;      
      Xell = 0.0;  Yell = -Minor;
      Xrot = Xell * CosP - Yell * SinP  + Cpx;
      Yrot = Xell * SinP + Yell * CosP  + Cpy;
      x2 = (float) Xrot;   y2 = (float) Yrot;      
      pgmove_c( &x1, &y1 );      
      pgdraw_c( &x2, &y2 );
   }

   /* If wanted, fill the area with straight lines */

   if (*Line_delta > 0.0 && *shape != 3) {
      double   Delta;                    /* Space in world coordinates between lines */
      double   Slope;                    /* Slope of area filling lines wrt major axis */
      double   CosS, SinS;               /* Sine, cosine of this slope */
      int      flag1, flag2;             /* Is part of line within ellipse */
      double   Xrot1, Xrot2;             /* Rotated line positions */
      double   Yrot1, Yrot2;
      double   Xsq1, Xsq2, Ysq;          /* Generate lines in square first */
      double   Lambda;                   /* Generate points on rotated line */
      double   Xline, Yline;             /* Points on that line */
      float    X1pa, X2pa, Y1pa, Y2pa;   /* Rotate over pos. angle */
      double   Maj2, Min2;               /* Major and minor squared */
      double   sqsize;                   /* Size of square around ellipse */
      double   fact;
      int      inside = 0;;


      Delta = (double) *Line_delta;
      Slope = (double) *Line_slope;
      CosS  = cos( RAD(Slope) );
      SinS  = sin( RAD(Slope) );
      Maj2  = Major * Major;
      Min2  = Minor * Minor;

      /*-------------------------------------------------------------*/
      /* Draw imaginary square around ellipse so that the ellipse    */
      /* is completely contained in the square. Generate horizontal  */
      /* lines separated 'Delta' world coordinates.                  */
      /* Rotate the end points of this line and generate a number    */
      /* of points on this rotated line until a point is found       */
      /* within the ellipse. Do the same again but start at the      */
      /* end point at the right and go left until you found again a  */
      /* point on or within the ellipse. These two points are con-   */
      /* nected to plot a line as part of the area filling.          */
      /* If a rectangle has to be filled, change the border          */
      /* conditions.                                                 */
      /*-------------------------------------------------------------*/

      sqsize = Major;
      if (Minor > Major) sqsize = Minor;
      if (Minor == 0.0)  sqsize = 0.0;
      fact = 1.5;

      Xsq1 = -fact*sqsize;  Xsq2 = fact*sqsize;
      for ( Ysq = Xsq1; Ysq <= Xsq2; Ysq += Delta ) {
         flag1 = 0; flag2 = 0;
         /* The points on the square: */
         /* The rotated line: */
         Xrot1 = Xsq1*CosS - Ysq*SinS;  Yrot1 = Xsq1*SinS + Ysq*CosS;
         Xrot2 = Xsq2*CosS - Ysq*SinS;  Yrot2 = Xsq2*SinS + Ysq*CosS;

         Lambda = 0.0;
         while (Lambda <= 1.0) {
            Xline = Xrot1 + Lambda*(Xrot2-Xrot1);
            Yline = Yrot1 + Lambda*(Yrot2-Yrot1);

            if (*shape == 1)
               inside = ( (Min2*Xline*Xline + Maj2*Yline*Yline) <= Maj2*Min2 );

            if (*shape == 2)
               inside = ( Xline >= -Major && Xline <= Major &&
                          Yline >= -Minor && Yline <= Minor );
            if ( inside )
            {
               /* Point of this line is inside (or on) ellipse/rectangle */
               flag1 = 1;
               X1pa = (float) (Xline*CosP - Yline*SinP + Cpx);
               Y1pa = (float) (Xline*SinP + Yline*CosP + Cpy);
               break;
            }
            Lambda += 0.01;
         }

         Lambda = 1.0;
         if (flag1) {
            while (Lambda >= 0.0) {
               Xline = Xrot1 + Lambda*(Xrot2-Xrot1);
               Yline = Yrot1 + Lambda*(Yrot2-Yrot1);

               if (*shape == 1)
                  inside = ( (Min2*Xline*Xline + Maj2*Yline*Yline) <= Maj2*Min2 );

               if (*shape == 2)
                  inside = ( Xline >= -Major && Xline <= Major &&
                             Yline >= -Minor && Yline <= Minor );

               if ( inside )
               {
                  /* Point of this line is inside (or on) ellipse/rectangle */
                  flag2 = 1;
                  X2pa = (float) (Xline*CosP - Yline*SinP + Cpx);
                  Y2pa = (float) (Xline*SinP + Yline*CosP + Cpy);
                  break;
               }
               Lambda -= 0.01;
            }

         }

         if (flag1 && flag2) {
            /* draw the line: */
            pgmove_c( &X1pa, &Y1pa );
            pgdraw_c( &X2pa, &Y2pa );
         }

      } /* End for loop */
   } /* End if user wants area fill */
} /* End of routine */


