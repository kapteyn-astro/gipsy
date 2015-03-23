/* velpro.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            velpro.dc2

Function:     VELPRO

Purpose:      Transformation from velocity to grid coordinates and vv.

File:         velpro.c

Author:       K.G. Begeman

Use:          INTEGER VELPRO( COORD1,    Input    DOUBLE PRECISION
                              COORD2,    Output   DOUBLE PRECISION
                              CRVAL ,    Input    DOUBLE PRECISION
                              CDELT ,    Input    DOUBLE PRECISION
                              DRVAL ,    Input    DOUBLE PRECISION
                              FREQ0 ,    Input    DOUBLE PRECISION
                              VELSYS,    Input    INTEGER
                              DIR   )    Input    INTEGER

              VELPRO    Returns:
                         0: transformation successful
                         9: unknown velocity system
                        10: rest frequency less than  zero
                        11: crval equal to zero
                        12: cdelt equal to zero
              COORD1    Input velocity in m/s or grid.
              COORD2    Output grid coordinate or velocity in m/s.
              CRVAL     Observed frequency in Hz at channel reference pixel.
              CDELT     Grid separation in Hz.
              DRVAL     Velocity at reference frequency in m/s.
              FREQ0     Rest frequency in Hz.
                        If FREQ0 is 0.0, the new (correct)
                        definition is used, if FREQ0 greater than 0.0,
                        the old (approximate) definition is used.
              VELSYS    Velocity system of input coordinates:
                        1 = optical definition
                        2 = radio definition
              DIR       Direction of transform:
                        DIR == 0:        velocity -> grid
                        DIR != 0:        grid -> velocity

Updates:      Dec  8, 1989: KGB, Document created.

#<

Fortran to C interface:

@ integer function velpro( double precision ,
@                          double precision ,
@                          double precision ,
@                          double precision ,
@                          double precision ,
@                          double precision ,
@                          integer          ,
@                          integer          )

*/

#include	"stdio.h"
#include	"gipsyc.h"

#define C         299792480.0             /* speed of light in vacuum (m/sec) */

fint velpro_c( double *coord1,
               double *coord2,
               double *crval ,
               double *cdelt ,
               double *drval ,
               double *freq0 ,
               fint   *velsys,
               fint   *dir   )
{
   fint   r = 0;

   if ((*freq0) < 0.0) return( 10 );
   if ((*crval) <= 0.0) return( 11 );
   if ((*cdelt) == 0.0) return( 12 );
   if ((*dir)) {					/* grid -> velocity */
      switch((*velsys)) {
         case 1: {                                      /* optical definition */
            if ((*freq0 > 0.0)) {
               (*coord2) = (*drval) - C * (*freq0) * (*coord1) * (*cdelt) / (*crval) /
                  ((*crval) + (*coord1) * (*cdelt));
            } else {
               (*coord2) = ((*drval) * (*crval) - C * (*coord1) * (*cdelt)) /
                  ((*crval) + (*coord1) * (*cdelt));
            }
            break;
         }
         case 2: {
            if ((*freq0) > 0.0) {
               (*coord2) = (*drval) - C / (*freq0) * (*coord1) * (*cdelt);
            } else {
               (*coord2) = ((*drval) * ((*crval) + (*coord1) * (*cdelt)) - C * (*coord1) * (*cdelt)) / (*crval);
            }
            break;
         }
         default: {
            r = 9;
            break;
         }
      }
   } else {                                               /* velocity -> grid */
      switch((*velsys)) {
         case 1: {                                      /* optical definition */
            double d;

            if ((*freq0) > 0.0) {
               d = ((*drval) - (*coord1)) * (*crval);
               (*coord2) = d * (*crval) / (C * (*freq0) - d) / (*cdelt);
            } else {
               d = ((*drval) - (*coord1)) * (*crval);
               (*coord2) = d / (C+(*coord1)) / (*cdelt);
            }
            break;
         }
         case 2: {                                        /* radio definition */
            if ((*freq0) > 0.0) {
               (*coord2) = ((*drval) - (*coord1)) / C * (*freq0) / (*cdelt);
            } else {
               (*coord2) = ((*coord1) - (*drval)) / ((*drval) - C) * (*crval) / (*cdelt);
            }
            break;
         }
         default: {
            r = 9;
            break;
         }
      }
   }
   return( r );
}


#if defined(TESTBED)
int main()
{
   double f0 = 1420405752.0;
   double cdelt = -78125.00;
   double crval = 1417248404.838;
   double drval = 650000.0;
   double v = 401777.7;
   double g = 32.0;
   double c;
   fint   velsys;
   fint   r;
   fint   dir;

   f0 = 0.0;
   velsys = 1;
   dir = 0;
   r = velpro_c( &v, &c, &crval, &cdelt, &drval, &f0, &velsys, &dir );
   printf( "velpro (%2ld): (%f) -> (%f)\n", r, v, c );
   g = c;
   dir = 1;
   r = velpro_c( &g, &c, &crval, &cdelt, &drval, &f0, &velsys, &dir );
   printf( "velpro (%2ld): (%f) -> (%f)\n", r, g, c );
   velsys = 2; drval = v + C * g * cdelt / f0;
   dir = 0;
   r = velpro_c( &v, &c, &crval, &cdelt, &drval, &f0, &velsys, &dir );
   printf( "velpro (%2ld): (%f) -> (%f)\n", r, v, c );
   g = c;
   dir = 1;
   r = velpro_c( &g, &c, &crval, &cdelt, &drval, &f0, &velsys, &dir );
   printf( "velpro (%2ld): (%f) -> (%f)\n", r, g, c );
}
#endif
