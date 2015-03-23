/* fillgauss2d.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
                All Rights Reserved.
                
#>            fillgauss2d.dc2

Function:     FILLGAUSS2D

Purpose:      Fill array with values of (rotated) 2-d Gauss

Category:     CONVOLUTION

File:         fillgauss2d.c

Author:       M. Vogelaar


Use:          INTEGER FILLGAUSS2D( beam    ,    Input   REAL ARRAY
                                   phi     ,    Input   REAL
                                   maxconv ,    Input   INTEGER
                                   gridspac,    Input   REAL ARRAY
                                   amplitude,   Input   REAL
                                   cutoffratio, Input   REAL 
                                   normalize,   Input   INTEGER
                                   NconX   ,    Output  INTEGER 
                                   NconY   ,    Output  INTEGER 
                                   confie  ,    Output  REAL ARRAY
                                 )

              FILLGAUSS2D Returns a negative value if it was not 
                          possible to create an array with function 
                          values needed for convolution. Else, 
                          the array 'confie' is filled with convolution
                          data in NconX*NconY box. If 
              beam        First element contains the major-, second
                          contains the minor axis, both FWHM and in 
                          physical units.
              phi         Position angle of the major axis (WRT. POSITIVE
                          X-AXIS) in degrees.
              maxconv     Max. length of array containing the convolut-
                          ion function data. 
              gridspac    First element contains the grid spacing in 
                          X-direction, second in Y-direction in the same
                          units as the beam.
              amplitude   Maximum value in gauss. If 'normalize' is
                          unequal to 0, the amplitude is set to 1.0.                          
              cutoffratio This value determines where to cutoff the 
                          gaussian convolution function.      
              normalize   If unequal to 0, the convolution function
                          will be normalized i.e. values are scaled so
                          that the total area will be 1.
              NconX       Length in X-direction of function.
              NconY       Length in Y-direction of function.
              confie      A one dimensional array with length
                          NconX * NconY ( smaller than 'maxconv' ), 
                          containing the calculated values of a two 
                          dimensional gaussian in a region given by the 
                          beam sizes.


Description:  This routine calculates the convolution function according 
              to the user defined axis sizes of a gaussian that can 
              be written as:

              f(x,y) = A exp[ -C{ (x/xh)^2 + (y/yh)^2 } ] where xh, yh 
              are beamwidths at half power. So f(0.5*xh, 0) = 0.5 * A ==>
              C = 4 * Ln(2).

              The cutoff ratio determines the boundaries of the 
              gaussian: For 1 dimension you can write:

              f(x) = A exp[ -4 Ln(2).(x/xh)^2 ]
              The value for which f(x) = cutoffratio . A,  can be 
              calculated with:

              x = xh. SQRT[ Ln(cutoffratio) / 4.Ln(2) ]
               
              Values smaller than cutoff are set to zero.



Updates:      Aug  30, 1992: MV,  Document created

#<

Fortran to C interface:

@integer function fillgauss2d( real, real, integer, 
@                              real, real, real, integer,
@                              integer, integer, real )

*/

#include  "gipsyc.h"
#include  "math.h"
#include  "anyout.h"


#define TORAD(a)  ( a * 0.017453292519943295769237 )
/* Pre April 2009 def.: #define NINT(a)    ( (a)<0 ? (int)((a)-.5) : (int)((a)+.5) ) */
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )


int fillgauss2d_c( float *varbeam, 
                   float *varphi,
                   fint  *maxconv,
                   float *vargridspac,
                   float *varampl,                   
                   float *varcutoff,
                   fint  *varnorm,
                   fint  *varNconX,
                   fint  *varNconY,
                   float *confie )
                         
                        
{
   double      phi;
   double      cs, sn;
   double      beam[2];
   double      xr, yr;
   double      extend;
   double      x, x1, x2, y, y1, y2;
   int         nconX, nconY;
   fint        scr = 3;
   int         Xmax, Ymax;
   double      gridspac[2];
   int         pos;
   double      arg, argX, argY, argfac;
   double      totalarea;
   double      cutoffratio;
   int         i, j;
   double      c;
   double      amplitude;
   bool        normalize;
   
  
   normalize = ( *varnorm  != 0 );
   if (normalize) amplitude = 1.0; else amplitude = (double) *varampl;   

   /*-------------------------------------------------------------*/
   /* Cos and sin of beam position angle. Phi at input is defined */
   /* as the angle between major axis and north in the direction  */
   /* of the east. The rotation routines however expect an angle  */
   /* wrt the positive x axis.                                    */
   /*-------------------------------------------------------------*/

   phi = (double) TORAD(*varphi);        
   cs = cos( phi );
   sn = sin( phi );  
   beam[0] = fabs((double)varbeam[0]);
   beam[1] = fabs((double)varbeam[1]);

   /*----------------------------------------------------------------*/
   /* Calculate beam array sizes in xy-plane. The corners of         */
   /* the convolution function in the lm-plane are (Bl,Bm),	     */
   /* (-Bl,Bm), (Bl,-Bm) and (-Bl,-Bm), where B is half the          */
   /* beamsize times a factor calculated by:                         */
   /*                                                                */
   /* x = xh. SQRT[ -Ln(cutoffratio) / 4.Ln(2) ]                     */
   /*                                                                */
   /* Because xh = FWHM we can write:                                */
   /*                                                                */
   /* x = 0.5 * beam * SQRT[ -Ln(cutoffratio) / Ln(2) ]              */
   /*                                                                */
   /* In what follows, the corresponding corners in the xy-          */
   /* plane are calculated.                                          */
   /*----------------------------------------------------------------*/

   xr = 0.5 * beam[0];
   yr = 0.5 * beam[1];
   cutoffratio = (double) *varcutoff;

   extend = sqrt( -1.0*log(cutoffratio) / log(2.0) );
   xr *= extend;
   yr *= extend;

   /* Transform (xr,0) and (0,yr) */
   
   x1 = fabs(xr*cs - 0.0*sn);
   y1 = fabs(xr*sn + 0.0*cs);
   x2 = fabs(0.0*cs - yr*sn);
   y2 = fabs(0.0*sn + yr*cs);
   x = ( x2 > x1 ? x2 : x1 );
   y = ( y2 > y1 ? y2 : y1 );

   gridspac[0] = fabs( (double) vargridspac[0] );
   gridspac[1] = fabs( (double) vargridspac[1] );

   /* Convert these physical lengths to grids and make */
   /* convolution function size odd */   
   
   Xmax = NINT(x/gridspac[0]); 
   Ymax = NINT(y/gridspac[1]); 

   nconX = 2 * Xmax + 1;    
   nconY = 2 * Ymax + 1; 
   /* Is this too big to fit in confie array? */
   if ( (nconX * nconY) > *maxconv ) {
      anyout_c( &scr, tofchar("Convolution function too big for buffer") );
      return(-1);
   } else {
      *varNconX = (fint) nconX;
      *varNconY = (fint) nconY;         
   }
      
   /* Constant in gaussian argument */
   argfac = -4.0 * log(2.0);
   /* Loop to write the values in the array */

   gridspac[0] = fabs( (double) vargridspac[0] );
   gridspac[1] = fabs( (double) vargridspac[1] );
   totalarea = 0.0;
   for (j = -Ymax; j <= Ymax; j++) {
      for (i = -Xmax; i <= Xmax; i++) {
         pos = (j + Ymax) * nconX + (i + Xmax);    /* Position in 'confie' array */
         x = ((double) i) * gridspac[0];
         y = ((double) j) * gridspac[1];
         xr =        x*cs + y*sn;
         yr =   -1.0*x*sn + y*cs;
         if (beam[0] == 0.0) argX = 0.0; else argX = xr / beam[0];
         if (beam[1] == 0.0) argY = 0.0; else argY = yr / beam[1];
         arg = argfac * (argX*argX + argY*argY);
         c = exp(arg);
         if (c >= cutoffratio) {
            c *= amplitude;
            confie[pos] = (float) c;
            totalarea += c;
         } else {
            confie[pos] = 0.0;
         }
      }
   }
   
   /* Rescale the contents of the convolution array */

   if (normalize) {
      for (i = 0; i < (nconX*nconY); i++) {
         confie[i] = confie[i]/(float) totalarea;
      }
   }
   return(1);   
}
   
