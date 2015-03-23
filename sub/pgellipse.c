/* pgellipse.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            pgellipse.dc2

Function:     PGELLIPSE

Purpose:      Draw ellipse (PGPLOT)
              
Category:     PLOTTING
             
File:         pgellipse.c

Author:       M. Vogelaar

Use:          CALL PGELLIPSE( XCENTRE,    Input    real
                              YCENTRE,    Input    real
                              MAJOR,      Input    real
                              MINOR,      Input    real
                              PA,         Input    real
                              STARTANG,   Input    real
                              ENDANG,     Input    real
                              DELTA  )    Input    real

              XCENTRE    X position of centre of ellipse in 
                         world coordinates.
              YCENTRE    Same for Y position
              MAJOR      Major axis of ellipse.
              MINOR      Minor axis of ellipse.    
              PA         Position angle of ellipse wrt. pos x-axis
                         in degrees.
              STARTANG   Angle to start drawing ellipse (Degrees).
              ENDANG     Angle to end drawing the ellipse (Degrees).
              DELTA      Calculate plot points from STARTANG to ENDANG
                         in steps of DELTA degrees.

Note:         The length of MAJOR can be smaller than the length of MINOR!

Description:  Draw an ellipse with specifications (origin and axes) 
              in world coordinates. The position angle of the major 
              axis is wrt. the pos. x-axis. The ellipse is rotated 
              counter-clockwise. The plotting starts at 'STARTANG'
              degrees from the major axis and stops at 'ENDANG'
              degrees from this major axis.

Updates:      Aug 03, 1992: VOG,  Document created
              Jun 08, 2000: VOG,  Added DELTA

#<

Fortran to C interface:

@subroutine pgellipse( real, real, real, real, real, real, real, real )

*/
#include "stdlib.h"
#include "gipsyc.h"
#include "math.h"
#include "pgmove.h"
#include "pgdraw.h"
#include "pgline.h"


void pgellipse_c( float *Centre_X, 
                  float *Centre_Y , 
                  float *Major_axis, 
                  float *Minor_axis, 
                  float *Posangle,
                  float *Startangle,
                  float *Endangle,
                  float *Delta )
/*------------------------------------------------------------*/
/* Draw ellipse from 'Startangle' to 'Endangle' */
/*------------------------------------------------------------*/
{
#define RAD(a)  ( a * 0.017453292519943295769237 )
#define false   0
#define true    1

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
   float     *Xpoints, *Ypoints;            /* Outline ellipse in arrays */
   int       i;                             /* Array index */
   int       len;                           /* Array length */
   fint      Numpoints;                     /* Number of points in arrays */
   


   Cpx   = (double) *Centre_X; 
   Cpy   = (double) *Centre_Y;
   Minor = (double) *Minor_axis;
   Major = (double) *Major_axis;
   Pa    = (double) *Posangle;
   CosP  = cos( RAD(Pa) );
   SinP  = sin( RAD(Pa) );   




   len = (int) ( fabs((*Endangle)-(*Startangle))/(*Delta) );   
   len += 10;                                            /* to be (very) sure */
   Xpoints = (float *) calloc( len, sizeof(float) );
   Ypoints = (float *) calloc( len, sizeof(float) );   

   
   if (Xpoints == NULL || Ypoints == NULL)
   {
      return;
   }


   i = 0;
   for (Alpha  = (double) *Startangle; 
        Alpha <= (double) *Endangle; 
        Alpha += (double) *Delta) 
   {
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
      Xpoints[i] = (float) (Xell * CosP - Yell * SinP  + Cpx); 
      Ypoints[i] = (float) (Xell * SinP + Yell * CosP  + Cpy);
      i++;
   }
   
   Numpoints = i;
   pgline_c( &Numpoints, Xpoints, Ypoints );   /* Can be replaced by 'pgpoly' */         
   free( Ypoints );
   free( Xpoints );
      
} /* End of routine */


