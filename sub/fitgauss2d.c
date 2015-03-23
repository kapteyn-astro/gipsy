/* fitgauss2d.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
                All Rights Reserved.
                
#>            fitgauss2d.dc2

Function:     FITGAUSS2D

Purpose:      Fit 2d-gaussian to data in a box

Category:     MATH

File:         fitgauss2d.c

Author:       M. Vogelaar

Notes:        The function FUNC and the subroutine DERV for a 2-dim 
              gauss must be defined outside this function.

Use:          INTEGER FITGAUSS2D( 
                                DATAI,      Input          REAL ARRAY
                                NDATX,      Input          INTEGER
                                NDATY,      Input          INTEGER
                                GRIDX,      Input          DOUBLE PRECISION
                                GRIDY,      Input          DOUBLE PRECISION
                                PARLIST,    Input/Output   REAL ARRAY
                                ERRLIST,    Output         REAL ARRAY 
                                MPAR,       Input          INTEGER ARRAY
                                TOL,        INPUT          REAL
                                ITS,        INPUT          INTEGER
                                LAB,        INPUT          REAL 
                                CLIP        INPUT          REAL ARRAY )
                                
                               
                              
             FITGAUSS2D 
                  >=0: Fit found, return fit parameters in PARLIST, errors
                       in ERRLIST. FITGAUSS2D is the number of iterations
                       returned by the least squares fit routine.
                   -1: Too many free parameters, maximum is 32.
                   -2: No free parameters.
                   -3: Not enough degrees of freedom.
                   -4: Maximum number of iterations too small to obtain a 
                       solution which satisfies TOL.
                   -5: Diagonal of matrix contains elements which are zero.
                   -6: Determinant of the coefficient matrix is zero.
                   -7: Square root of negative number.
                   -8: Fit aborted because array size exceeds maximum
                       (128*128).
                   -9: Fit aborted because all data are blank.
                  -10: Length box equals 1 in x or y direction.
                  -11: Position of (gauss) maximum not in this box.
                  -12: Sum of data values equals 0.
                  -13: Initial estimate amplitude and zero level are mot
                       different.
                  -14: FWHM in x or y direction smaller than 0

             NDATX     Length of DATAI in X direction
             NDATY     Length of DATAI in Y direction
             GRIDX     Grid spacing in X
             GRIDY     Grid spacing in Y
             PARLIST   On input, these can be initial estimates for the fit.
                       If however, one of the values is blank on input, the
                       function calculates initial estimates. The return 
                       values are the fit parameters. The array elements are:
                       1) Amplitude in map units
                       2) FWHMx, Full width at half maximum in physical
                          coordinates in x-direction.
                       3) FWHMy, FWHM in y-direction.
                       4) X0, centre x-coordinate as offset in physical 
                          coordinates wrt. lower left corner of box,
                          usually centre in pixels times grid spacing.
                       5) Y0, centre y-coordinate.
                       6) angle in RADIANS wrt. positive X-axis!
                       7) zero level in map units
             ERRLIST   List with error estimates, in the same order as 
                       FITLIST, as calculated in a least squares fit.
             MPAR      For each of the 7 parameters in the 2d-gauss fit, this
                       mask determines whether the parameter is free 
                       (MPAR(n) = 1) or fixed (MPAR(n) = 0) in the fit.
             TOL       Relative tolerance. LSQFIT stops when successive 
                       iterations fail to produce a decrement in reduced 
                       chi-squared less than TOL. If TOL is less than the 
                       minimum tolerance possible, TOL will be set to this 
                       value. This means that maximum accuracy can be 
                       obtained by setting TOL=0.0.
             ITS       Maximum number of iterations in fit routine.
             LAB       Mixing parameter, LAB determines the initial
                       weight of steepest descent method relative to
                       the Taylor method. LAB should be a small
                       value (i.e. 0.01). LAB can only be zero when
                       the partial derivatives are independent of
                       the parameters. In fact, in this case LAB
                       should be exactly equal to zero.
             CLIP      Two values. The first value is a lower threshold 
                       and the second an upper threshold to the 
                       data that will be used in the fit. Examples:
                       1) Use only values between 2 and 4 (2,4 included):
                          CLIP(1) = 2 and CLIP(2) = 4 
                       2) Use values lower than (and equal to) 2:
                          CLIP(1) = BLANK and CLIP(2) = 2
                       3) Use values greater than (and equal to) 2:
                          CLIP(1) = 2 and CLIP(2) = BLANK
                       4) Use all values in the box:
                          CLIP(1) = BLANK and CLIP(2) = BLANK
                       
                          
                       

Update:       Sep  14, 1992: VOG, Document created.

#<

Fortran to C interface:

@ integer function fitgauss2d( real,
@                              integer,
@                              integer,
@                              double precision,
@                              double precision,
@                              real,
@                              real,
@                              integer,
@                              real,
@                              integer,
@                              real,
@                              real )

*/

#include        "stdio.h"               /* <stdio.h> */
#include        "gipsyc.h"              /* GIPSY symbols and definitions */
#include        "setfblank.h"           /* defines setfblank_c */
#include        "lsqfit.h"
#include        "anyout.h"
#include        "math.h"
 
             
#define          MAXFITDAT   128*128
#define          MAXITERS    100
#define          PI          3.141592653589793
#define          TODEG(a)    ( (a)*180.0/PI )  /* Convert radians to degrees */


fint fitgauss2d_c( float   *dataI,
                   fint    *ndatX,
                   fint    *ndatY,
                   double  *vargridX,
                   double  *vargridY, 
                   float   *parlist,
                   float   *errlist,
                   integer *mpar,
                   float   *tol,
                   integer *its,
                   float   *lab,
                   float   *clip
                 )

{
   int      totpixels;
   float    blank;
   float    value, maxval;
   int      imax, jmax;
   int      fitX, fitY;
   float    xcoor, ycoor;
   float    tanpa;
   char     message[80];
   int      i, j;
   int      index;
   float    xydat[MAXFITDAT][2];
   float    zdat[MAXFITDAT];
   float    wdat[MAXFITDAT];
   float    fwhmx, fwhmy;
   float    paest;
   fint     scr;
   float    gridX, gridY;
   int      clipcase;
   
   /* Moment analysis */
   
   float    M, Mx, My, Mxx, Myy, Mxy;
   float    zerolev;
   fint     zeronum;
   float    X0, Y0;

   /* 'lsqfit' parameters */

   fint     ndat;
   fint     npar;
   fint     xdim;
   fint     fopt;   
   fint     iters;
  

   gridX = (float) fabs(*vargridX); 
   gridY = (float) fabs(*vargridY);
   fitX  = *ndatX;    fitY  = *ndatY;
   totpixels = fitX * fitY;                      /* Total number of pixels in fit box*/
   if (totpixels > MAXFITDAT) return(-8);        /* Exceeds maximum size, return. */
   if ((fitX == 1) || (fitY == 1)) return(-10);
   setfblank_c( &blank );      
   if (clip[0] != blank) {
      if (clip[1] != blank) clipcase = 1;
      else clipcase = 2;
   } else {
      if (clip[1] != blank) clipcase = 3;
      else clipcase = 4;
   }
   /*----------------------------------------------------------------*/
   /* Put all data in arrays with positions, values and weights      */
   /* suitable for the LSQFIT function. First check if array         */
   /* contains blanks only and determine the center of gravity.      */
   /*----------------------------------------------------------------*/

   zerolev = 0.0;
   zeronum = 0;
   M  = Mx = My = 0.0;
   maxval  = blank;
   for (index = 0, j = 0; j < fitY; j++)  {
      for (i = 0; i < fitX; i++, index++) {
         value = dataI[index];
         switch (clipcase) {
            case 1:
              /* value must be between cliplo and cliphi (included) */
              if ((value > clip[1]) || (value < clip[0])) value = blank;
            break;
            case 2:
              /* value must be greater(equal) than cliplo */
              if (value < clip[0]) value = blank;
            break;
            case 3:
              /* value must be smaller(equal) than cliphi */
              if (value > clip[1]) value = blank;
            break;
            /* Option 4 uses all data */
         }
         if (maxval == blank) {
            /* Initialize maxval with first non blank value */
            if (value != blank) maxval = value;
         } else {
            if (value > maxval) {
               maxval = value;
               imax = i;
               jmax = j;
            }
         }
         if (value != blank) {
            if ( (i == 0) || (i == fitX) || (j == 0) || (j == fitY) ) {
               /* Pixel is at box border */
               zerolev += value;
               zeronum++;
            }
            Mx += ((float)i) * gridX * value;
            My += ((float)j) * gridY * value;
            M  += value;
         }
         dataI[index] = value;
      }
   }
   if (maxval == blank) return(-9);              /* All data are blanks */
   if (M == 0.0) return(-12);
   if (zeronum == 0) {
      scr = 3;
      anyout_c( &scr, tofchar("Edge of fit box filled with blanks"));
      anyout_c( &scr, tofchar("0.0 substituded for zero level"));
      zerolev = 0.0;
   } else {
      zerolev /= zeronum;                        /* Mean of all border pixels */         
   }

   X0 = Mx / M;                                  /* Center of gravity */
   Y0 = My / M;
   /* If one of the values is outside the box, take the position */
   /* of the maximum as initial estimate. */
   if ((X0 > fitX*gridX) || (X0 < 0.0)) return(-11);
   if ((Y0 > fitY*gridY) || (Y0 < 0.0)) return(-11);

   /*-------------------------------------------------------------------*/
   /* Put the data in two arrays xydat, zdat and assemble the           */
   /* centre data in the box for use in the estimates determination.    */
   /* The initial estimate of the zero level is made by taking the      */
   /* mean of all pixels at the border of the (fit)area                 */
   /*-------------------------------------------------------------------*/   

   M = Mx = My = Mxx = Myy = Mxy = 0.0;      
   for (index = 0, j = 0; j < fitY; j++ )  {
      for (i = 0; i < fitX; i++, index++ ) {
         value = dataI[index];
         /* Calculations in arcsecs */
         xydat[index][0] = ((float)i) * gridX;
         xydat[index][1] = ((float)j) * gridY;
         zdat[index]     = value;
         if (value == blank) {
            wdat[index] = 0.0;
         } else {
            value -= zerolev;
            M     += value;
            xcoor  = (((float)i) - X0) * gridX;
            ycoor  = (((float)j) - Y0) * gridY;
            Mx    += xcoor * value;
            My    += ycoor * value;
            Mxx   += xcoor * xcoor * value;
            Myy   += ycoor * ycoor * value;
            Mxy   += xcoor * ycoor * value;
            wdat[index] = 1.0;
         }
      }
   }
   if (M == 0.0) return(-12);      

   /*----------------------------------------------------------------*/
   /* See: 'Digital Picture Processing' by Azriel Rosenfeld cs.      */
   /* The line y = x.tan(paest) is called the principal axis         */
   /* of inertia of the distribution 'image'. It can be proved       */
   /* that for an ellips with its centroid at the origin,            */
   /* the principal axis coincides with the major axis of the        */
   /* ellips.                                                        */
   /*----------------------------------------------------------------*/    

   if (fabs(Myy-Mxx) < 1E-7) {
      paest = 0.0;
   } else {   
     paest = 0.5 * atan2(2.0 * Mxy, Myy - Mxx);
   }
   while (TODEG(paest)> 45.0) paest -= PI / 2.0;
   while (TODEG(paest)<-45.0) paest += PI / 2.0;
   tanpa = (float) tan(paest);   
   if (M == 0.0) M = 0.1;
   Mx /= M;
   My /= M;
   Mxx = fabs( Mxx/M - Mx*Mx );                  /* Dispersion: <x*x>-(x><x> */ 
   Myy = fabs( Myy/M - My*My );
   Mxy = -( Mxy/M - Mx*My );
   
   fwhmy = Myy - tanpa*Mxy;              
   fwhmx = Mxx + tanpa*Mxy;   
   fwhmx = 2.35*sqrt(fabs(fwhmx));               /* Convert sigma to FWHM */
   fwhmy = 2.35*sqrt(fabs(fwhmy));
     
   /* In the lsqfit routine, the angle must be wrt. the pos. x-axis */ 

   if (fabs((maxval - zerolev)) < 1e-12) return(-13);
   
   if (parlist[0] == blank) {           
       parlist[0]  = maxval - zerolev;           /* Amplitude corrected for base */
   }
   if (parlist[1] == blank) {
       parlist[1]  = fwhmx;                      /* FWHM in x direction */
   }
   if (parlist[2] == blank) {
       parlist[2]  = fwhmy;                      /* FWHM in y direction */
   }
   if (parlist[3] == blank) {
       parlist[3]  = X0;                         /* Central X-position of gaussian */
   }
   if (parlist[4] == blank) {
       parlist[4]  = Y0;                         /* Central Y-position of gaussian */
   }   
   if (parlist[5] == blank) {
       parlist[5]  = paest;                      /* Angle in radians wrt pos. x-axis */
   }
   if (parlist[6] == blank) {
       parlist[6]  = zerolev;                    /* Initial estimate for zero level */
   }

   scr = 16;
   anyout_c( &scr, tofchar(" ") );
   sprintf( message, "Initial estimates:" );
   anyout_c( &scr, tofchar(message) );
   sprintf( message, "Amplitude:    %f", parlist[0] );
   anyout_c( &scr, tofchar(message) );
   sprintf( message, "FWHM x, y:    %f %f", parlist[1], parlist[2] );
   anyout_c( &scr, tofchar(message) );
   sprintf( message, "X0, Y0:       %f %f", parlist[3], parlist[4]);    
   anyout_c( &scr, tofchar(message) );
   sprintf( message, "(X0,Y0 grids: %f %f)", parlist[3]/gridX, parlist[4]/gridY ); /* Back to pixels */
   anyout_c( &scr, tofchar(message) );
   sprintf( message, "Angle:        %f (rad) wrt pos. x-axis:", parlist[5] );
   anyout_c( &scr, tofchar(message) );      
   sprintf( message, "(Angle in deg:%f (deg) wrt pos. x-axis:)", TODEG(parlist[5]) );
   anyout_c( &scr, tofchar(message) );         
   sprintf( message, "Zero level:   %f", parlist[6] );
   anyout_c( &scr, tofchar(message) );

   /* 'lsqfit' parameters: mpar (input) determines which parameters */
   /* in the fit are free or fixed. The tolerance, 'tol' and mixing */
   /* parameter 'lab' are also input. */     

   ndat   = fitX * fitY;                         /* Total number of data points */
   npar   = 7;                                   /* Number of parameters in the fit */
   xdim   = 2;                                   /* Dimension of 'xydat' array */
   fopt   = 0;                                   /* Option for 'func' & 'derv' */
   iters  = lsqfit_c( &xydat[0][0], 
                      &xdim, 
                      zdat, 
                      wdat, 
                      &ndat, 
                      parlist, 
                      errlist, 
                      mpar, 
                      &npar, 
                      tol, 
                      its, 
                      lab, 
                      &fopt );                      
   /* Suspicious FWHMs ? */
   if ((parlist[1] < 0) || (parlist[2] < 0)) return(-14);
   return(iters);
}

