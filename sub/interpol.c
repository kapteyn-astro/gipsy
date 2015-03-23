/* interpol.c
                           COPYRIGHT (c) 2000
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             interpol.h
#if !defined(_interpol_h_)
#define _interpol_h_
float interpol( float, float, float **, fint *, fint *, float );
#endif
#<                          
*/

/*
#>             interpol.dc2

Function:      interpol

Purpose:       Get 2d-bilinear interpolated image value at given position
               in a 2-dim array.
               
Category:      MATH

File:          interpol.c

Author:        M. Vogelaar 


Use:           float interpol( float   x,
                               float   y,
                               float **image,
                               fint   *blo,
                               fint   *bhi,
                               float   blank );
                               
               interpol: Output of bilenear interpolated image value
               x, y    : Input of position in image (in grids)
               image   : Input of two dimensional array
               blo     : Input of lower array boundary in x and y in grids
               bhi     : Input of upper array boundary in x and y in grids
               blank   : Input of the value of a GIPSY blank (float)
 
               
               See also function comment in 'interpol.c'
                 
                
Example:       ........
               setfblank_c( &blank );
               image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );
               gdsi_read_c( Setin, &cwlo, &cwhi, &image[blo[1]][blo[1]],
                            &imagesize, &pixelsread, &tid );
                            
               --- Get the bilinear interpolated value at position -3, -4 ---
               
               bival = interpol( -3, -4, image, blo, bhi, blank );


Comment:       This routine is NOT callable from FORTRAN.


Notes:         The value of a blank is needed to speed up the routine
               by avoiding setting the blank value in the routine itself.

               
Updates:       27 Jul,  2000: VOG, Document created. 
#<
*/


/*----------------------------------------------------------------------*/

#include    "gipsyc.h"
#include    "interpol.h"

#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) ) 


static float getimval( int      xi,
                       int      yi,
                       float  **image,
                       fint    *blo,
                       fint    *bhi,
                       float    blank )
/*------------------------------------------------------------------*/
/* PURPOSE: Get a value from 2d 'image' at integer position x,y.    */
/*------------------------------------------------------------------*/ 
{
   /* Inside box? */
   if (xi >= blo[0] && xi <= bhi[0] &&
       yi >= blo[1] && yi <= bhi[1] ) 
   {
      return( image[yi][xi] );
   }   
   return( blank );     
}



float interpol( float   x,
                float   y,
                float **image,
                fint   *blo,
                fint   *bhi,
                float   blank )
/*------------------------------------------------------------*/
/* PURPOSE: Get 2d-bilinear interpolated image value.         */
/*                                                            */
/* Given 4 values z1,z2,z3,z4 at positions:                   */
/*                                                            */
/*       z3       z4                                          */
/* (0,1) o--------o (1,1)                                     */
/*       |        |                                           */
/*       ^        |                                           */
/*    dy |        |                                           */
/*       | dx     |                                           */
/* (0,0) o---->---o (1,0)                                     */
/*      z1       z2                                           */
/*                                                            */
/* If the nearest pixel is blank, return a blank, otherwise   */
/* do an interpolation.                                       */
/* The system is based on two linear interpolations in the    */
/* x direction followed by a linear interpolation in y using  */
/* these two values. If one writes this out then the weights  */
/* are: (1-dx)*(1-dy) * z1, (1-dy)*dx * z2, (1-dx)*dy * z3,   */
/* and dx*dy * z4                                             */
/* The sum of these weigths is 1.0. If one of the intensities */
/* is a blank, rescale the weights.                           */
/*                                                            */
/* Use of the image data:                                     */
/* The image must be a TWO dimensional image and its array    */
/* boundaries are given in 'blo' and 'bhi'. Then function     */
/* 'getimval' is used to get the image values.                */
/*------------------------------------------------------------*/
{
   float   z[4];              /* 4 image values */
   float   g[4];              /* Corresponding weights */   
   float   dx, dy, dxdy;
   int     x1, y1, x2, y2;
   int     i;
   float   sumw, zav;         /* Sum of weights, average image value */


   /* Important to start at the nearest grid */
   x1 = NINT(x);
   y1 = NINT(y);

   /*--------------------------------------------------*/
   /* Get value of nearest pixel. Return if this value */
   /* is a blank or if the position is outside the box.*/
   /* In the latter case also a blank is returned by   */
   /* function 'getimval'.                             */
   /*--------------------------------------------------*/   

   z[0] = getimval( x1, y1, image, blo, bhi, blank );
   if (z[0] == blank)
      return( blank );

   dx = x - (float)x1;
   if (dx >= 0.0)        /* Right of nearest pixel */
      x2 = x1 + 1;
   else                  /* Left of nearest pixel */
   {
      dx = ABS(dx);
      x2 = x1 - 1;
   }

   dy = y - (float)y1;
   if (dy >= 0.0)
      y2 = y1 + 1;
   else
   {
      dy = ABS(dy);
      y2 = y1 - 1;
   }

   /* Calculate the weights */
   dxdy = dx * dy;
   g[0] = 1.0 - dx - dy +dxdy;    /* (1-dx)*(1-dy) */
   g[1] = dx - dxdy;              /* (1-dy)*dx     */
   g[2] = dy - dxdy;
   g[3] = dxdy;
   
   /* The image values of the neighbours */
   z[1] = getimval( x2, y1, image, blo, bhi, blank ); 
   z[2] = getimval( x1, y2, image, blo, bhi, blank ); 
   z[3] = getimval( x2, y2, image, blo, bhi, blank );

   sumw = 1;
   zav = 0.0;
   for (i = 0; i < 4; i++)
   {
      if (z[i] == blank)
      {
         sumw -= g[i];
      }
      else
      {
         zav += g[i] * z[i];
      }
   } 
   if (sumw != 1.0)
   {
      if (sumw != 0.0)
      {
         zav /= sumw;
      }
      else
      {
         zav = blank;
      }
   }
   return( zav );   
}
