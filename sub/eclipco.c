/* eclipco.c

                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.
        
#>            eclipco.dc2       

Subroutine:   ECLIPCO

Purpose:      Transforms ecliptical coordinates from one epoch
              to another epoch.

Category:     COORDINATES

File:         eclipco.c

Author:       M.G.R. Vogelaar

Use:          CALL ECLIPCO( LAMBDA1    ,   Input    double precision
                            BETA1      ,   Input    double precision
                            EPOCH1     ,   Input    double precision
                            LAMBDA2    ,   Output   double precision
                            BETA2      ,   Output   double precision
                            EPOCH2     ,   Input    double precision
                          )

              LAMBDA1     Input ecliptical longitude in degrees at EPOCH1.
              BETA1       Input ecliptical latitude in degrees at EPOCH1.
              EPOCH1      EPOCH1 in years for LAMBDA1 and BETA1.
              LAMBDA2     Output ecliptical longitude in degrees at EPOCH2.
              BETA2       Output ecliptical latitude in degrees at EPOCH2.
              EPOCH2      EPOCH2 in years for LAMBDA2 and BETA2.


Example:      ......
                            
             
Notes:        

Updates:      Nov 6, 1992; VOG, Document created.

#<

Fortran to C interface:

@ subroutine eclipco( double precision ,
@                     double precision ,
@                     double precision ,
@                     double precision ,
@                     double precision ,
@                     double precision )


*/

#include    "math.h"
#include    "julianday.h"
#include    "epoco.h"

#define RAD(a)         ( a * 0.017453292519943295769237 )
#define DEG(a)         ( a * 57.295779513082320876798155 )


void eclipco_c( double *lambdaI, double *betaI, double *epochIN, 
                double *lambdaO, double *betaO, double *epochOUT )
/*-------------------------------------------------------------------*/
/* Transforms ecliptical coordinates from one epoch to another epoch.*/
/* First the obliquity of the ecliptic is calculated for the input   */
/* epoch. With this value (epsIN) a transformation is made to        */
/* equatorial coordinates. These coordinates are transformed to the  */
/* new epoch. Together with 'epsOUT' (=obliquity at 'epochOUT'), new */
/* ecliptical coordinates are calculated.                            */
/*-------------------------------------------------------------------*/
{
   double     day, month, year;
   double     JD;
   double     T;
   double     epsIN, epsOUT;
   double     sinL, cosL, tanL;
   double     sinB, cosB, tanB;
   double     sinA, cosA, tanA;
   double     sinE, cosE;
   double     sinD, cosD, tanD;
   double     A, D, L, B;
   

   if (*epochIN == *epochOUT) {
      *lambdaO = *lambdaI;
      *betaO   = *betaI;
      return;
   }  
   sinL = sin(RAD(*lambdaI));
   cosL = cos(RAD(*lambdaI));      
   tanB = tan(RAD(*betaI));
   sinB = sin(RAD(*betaI));
   cosB = cos(RAD(*betaI));

   day      = 0.0; 
   month    = 0.0;
   year     = *epochIN;
   JD       = julianday_c( &day, &month, &year );   /* Calculate Julian day for 0h UT */
   T        = ( JD - 2415020.0 ) / 36525.0;         /* Calculate sidereal time at Greenwich */
   epsIN    = 23.452294 - 0.0130125*T - 
            0.00000164*T*T + 0.000000503*T*T*T;     /* Obliquity of the ecliptic, epsilon: */
   cosE     = cos(RAD(epsIN));
   sinE     = sin(RAD(epsIN));
   tanA     = (sinL*cosE - tanB*sinE) / cosL;       /* Convert to equatorial */
   A        = atan2( sinL*cosE - tanB*sinE , cosL );
   sinD     = sinB*cosE + cosB*sinE*sinL;
   D        = asin(sinD);
   A        = DEG(A);
   D        = DEG(D);
   epoco_c( &A, &D, epochIN, &A, &D, epochOUT );
   A        = RAD(A);
   D        = RAD(D);
   cosA     = cos(A);
   sinA     = sin(A);   
   tanD     = tan(D);
   sinD     = sin(D);
   cosD     = cos(D);
   year     = *epochOUT;
   JD       = julianday_c( &day, &month, &year );      
   T        = ( JD - 2415020.0 ) / 36525.0;
   epsOUT   = 23.452294 - 0.0130125*T - 0.00000164*T*T + 0.000000503*T*T*T;     
   cosE     = cos(RAD(epsOUT));
   sinE     = sin(RAD(epsOUT));
   tanL     = (sinA*cosE + tanD*sinE) / cosA;
   L        = atan2( sinA*cosE + tanD*sinE, cosA );
   B        = asin( sinD*cosE - cosD*sinE*sinA );
   *lambdaO = DEG(L);
   *betaO   = DEG(B);
   /* Put lambda in range 0..360 deg without mod functions */
   while (*lambdaO < 0.0)
      *lambdaO += 360.0;
   while (*lambdaO >= 360.0)
      *lambdaO -= 360.0;
}

