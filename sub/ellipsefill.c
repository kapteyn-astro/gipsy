/* ellipsefill.c

     Copyright (c) Kapteyn Laboratorium Groningen 2004
     All Rights Reserved.
*/
                    
                   
/*
#>            ellipsefill.dc2

Function:     ellipsefill

Purpose:      Set a mask for each point in a box that is inside a polygon
                            
Category:     MATH
                           
File:         ellipsefill.c
                          
Author:       M.G.R. Vogelaar
                        
Description:  Set a mask of points in a box that are contained by an ellipse

Use:          void ellipsefill( int   *mask, 
                                int    blo[0], 
                                int    blo[1], 
                                int    bhi[0], 
                                int    bhi[1], 
                                float *ellpar );

              The equation (for a non-rotated ellipse) is:

              (X-Xc)^2/major^2 + (Y-Yc))^2/minor^2 = 1

              An ellipse is defined by a number of parameters set by 'ellpar':
              Xc = ellpar[0];
              Yc = ellpar[1];
              major = ellpar[2];
              minor = ellpar[3];
              Pa = ellpar[4];
              These are the centre of the ellipse in grids, the length in grids
              of the semi major axis, the length in grids of the semi minor axis,
              Pa is position angle of ellipse wrt. pos x-axis in degrees. 
              Note that the terms major and minor are just names and could 
              easily be replaced by e.g. a and b. The ellipse therefore is
              mathematical, not astronomical. 
              
              A mask is an integer array with as many elements as contained 
              in the box given by blo and bhi (usually from the BOX= keyword).
              This mask represents the data in a subset in a box with limits blo
              and bhi. Initially it should be set to zero for each element in the mask.
              The function loops over all grid positions in the box and if such a 
              grid is inside the ellipse, then the corresponding position in the
              mask is flagged (i.e. one is added to the array element).
            
Remarks:      
              
Updates:      Jul 26, 2004: VOG, Document created.


#<
*/



/*
#> ellipsefill.h
void ellipsefill( int *, int, int, int, int, float *);
#<
*/


#include  "stdlib.h"
#include  "gipsyc.h"
#include  "userfio.h"
#include  "math.h"

/* Pre Apr 2009 def.: #define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) ) */
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) ) 
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) ) 
#define RAD(a)         ( (a) * 0.017453292519943295769237 )

void ellipsefill( int   *mask, 
                  int    xblo, int yblo, int xbhi, int ybhi, 
                  float *ellpar )
/*------------------------------------------------------------*/
/* PURPOSE: Set masker for grids inside/outside ellipse.      */
/* Translate and then rotate a position and apply the         */
/* condition x^2/maj^2 + y^2/min^2 <= 1                       */
/*------------------------------------------------------------*/
{
   /* Copy ellipse parameters into more readable variables */
   float Xc   = ellpar[0];
   float Yc   = ellpar[1];
   float Emaj = ellpar[2];
   float Emin = ellpar[3];
   float Pa   = ellpar[4];
   
   int     x, y;
   float   xlo, ylo, xhi, yhi;   
   float   CosP, SinP;
   float   Emin2, Emaj2;
   float   ax_max;
   float   ecc;
   int     lx = (xbhi - xblo) + 1;

   CosP = (float)cos( RAD(Pa) );
   SinP = (float)sin( RAD(Pa) );
   
   Emin2 = Emin * Emin;
   Emaj2 = Emaj * Emaj;
   ecc = Emaj2 / Emin2;

   /* If major and minor switched */
   ax_max = MYMAX( Emaj, Emin );

   xlo = NINT(-ax_max + Xc );
   xlo = MYMIN( xlo, xbhi );
   xlo = MYMAX( xlo, xblo );

   xhi = NINT( ax_max + Xc );   
   xhi = MYMIN( xhi, xbhi );
   xhi = MYMAX( xhi, xblo );

   ylo = NINT(-ax_max + Yc );
   ylo = MYMIN( ylo, ybhi );
   ylo = MYMAX( ylo, yblo );

   yhi = NINT( ax_max + Yc );
   yhi = MYMIN( yhi, ybhi );
   yhi = MYMAX( yhi, yblo );

   for (y = ylo; y <= yhi; y++)
   {
      for (x = xlo; x <= xhi; x++)
      {
         float xs = ((float) x) - Xc;
         float ys = ((float) y) - Yc;         
         {
            float xr = xs * CosP + ys * SinP;
            float yr = xs * SinP - ys * CosP;
            if (yr <= ax_max)
            {
               int Xi = x - xblo;
               int Yi = y - yblo;
               int indx = Yi * lx + Xi;
               if (xr*xr <= Emaj2 - ecc*yr*yr )
                  mask[indx] = 1;
            }
         }
      }      
   }
}


