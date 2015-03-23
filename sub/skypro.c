/* skypro.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            skypro.dc2

Function:     SKYPRO

Purpose:      Transformation between sky and grid coordinates.

File:         skypro.c

Author:       K.G. Begeman

Use:          INTEGER SKYPRO ( XIN    ,    Input    double precision
                               YIN    ,    Input    double precision
                               XOUT   ,    Output   double precision
                               YOUT   ,    Output   double precision
                               CRVAL1 ,    Input    double precision
                               CRVAL2 ,    Input    double precision
                               CDELT1 ,    Input    double precision
                               CDELT2 ,    Input    double precision
                               CROTA2 ,    Input    double precision
                               SKYSYS ,    Input    integer
                               SKYBASE,    Input    integer
                               PROBASE,    Input    integer
                               MODE   )    Input    integer
              
              SKYPRO   0: transformation successful
                       1: unknown projection
                       2: unknown mode
                       3: CROTA2 = 90.0 for mode 1 and 2
                       4: CDELT1 or CDELT2 equal to zero
                       5: input sky system unknown
                       6: output sky system unknown
                       7: input and output sky system unknown
              XIN      Input X coordinate in degrees or grids.
              YIN      Input Y coordinate in degrees or grids.
              XOUT     Output X coordinate in degrees or grids.
              YOUT     Output Y coordinate in degrees or grids.
              CRVAL1   Projection centre of X coordinate in degrees.
              CRVAL2   Projection centre of Y coordinate in degrees.
              CDELT1   Grid separation along X axis in degrees.
              CDELT2   Grid separation along Y axis in degrees.
              CROTA2   Rotation angle in degrees.
              SKYSYS   Sky system of sky coordinates.
                       SKYSYS          sky system
                         1             equatorial (1950.0)
                         2             galactic
                         3             ecliptic
                         4             supergalactic
              SKYBASE  Sky system of projection centre.
                       SKYBASE         sky system
                         1             equatorial (1950.0)
                         2             galactic
                         3             ecliptic
                         4             supergalactic
              PROBASE  Projection system onto which the sky coordinates
                       are projected:
                       PROBASE         projection type
                         1             AITOFF equal area
                         2             Equivalent Cylindrical
                         3             Flat
                         4             Gnomonic
                         5             Orthographic
                         6             Rectangular
                         7             Global Sinusoidal
                         8             North Celestial Pole
                         9             Stereographic
                        10             Mercator
              MODE     Mode determines what type of input/output
                       coordinates are given/wanted.
                       MODE       XIN     YIN   XOUT   YOUT
                         0        sky     sky   grid   grid
                         1       grid     sky    sky   grid
                         2        sky    grid   grid    sky
                         3       grid    grid    sky    sky

Updates:      Dec 15, 1989: KGB, Document created.

#<

Fortran to C interface:

@ integer function skypro( double precision,
@                          double precision,
@                          double precision,
@                          double precision,
@                          double precision,
@                          double precision,
@                          double precision,
@                          double precision,
@                          double precision,
@                          integer         ,
@                          integer         ,
@                          integer         ,
@                          integer         )

*/

/*
 * Here follow the necessary include files to make it all work.
 */

#include "stdio.h"       /* 'standard' ANSI C include file supported by GIPSY */
#include "math.h"        /* 'standard' ANSI C include file supported by GIPSY */
#include "gipsyc.h"       /* include file to define some GIPSY specific types */
#include "proco.h"
#include "skyco.h"

#define EPSILON (double) 0.0000000000001   /* epsilon, convergence controller */

fint skypro_c( double *x1     ,
               double *y1     ,
               double *x2     ,
               double *y2     ,
               double *crval1 ,
               double *crval2 ,
               double *cdelt1 ,
               double *cdelt2 ,
               double *crota2 ,
               fint   *skysys ,
               fint   *skybase,
               fint   *probase,
               fint   *mode   )
{
   fint r = 0;
   fint m = *mode;

   switch(m) {
      /*
       * The (sky,sky) -> (grid,grid) can be done directly.
       */
      case 0: {                                 /* (sky, sky) -> (grid, grid) */
         double lon, lat;

         r = skyco_c( x1, y1, skysys, &lon, &lat, skybase );    /* to skybase */
         if (r) break;
         r = proco_c( &lon, &lat, x2, y2, crval1, crval2,
                      cdelt1, cdelt2, crota2, probase, mode );
         break;
      }
      /*
       * The (grid,sky) -> (sky,grid) transformations are done in an
       * iterative way. The convergence is reached when the difference
       * between known coordinates and estimated coordinates are less
       * than EPSILON.
       */
      case 1: {                                 /* (grid, sky) -> (sky, grid) */
         if (*skysys == *skybase) {              /* This can be done directly */
            r = proco_c( x1, y1, x2, y2, crval1, crval2,
                         cdelt1, cdelt2, crota2, probase, mode );
         } else {                         /* This is somewhat more cumbersome */
            double lon, lat;
            double jl0, jb0, jl1, jb1; 
            double jy0, jy1;
            fint   tosky = 3;
            
            jy0 = 0.0; jy1 = 1.0;
            r = proco_c( x1, &jy0, &lon, &lat, crval1, crval2,
                         cdelt1, cdelt2, crota2, probase, &tosky );
            if (r) break;
            r = skyco_c( &lon, &lat, skybase, &jl0, &jb0, skysys );
            if (r) break;
            do {                                 /* start iterating procedure */
               r = proco_c( x1, &jy1, &lon, &lat, crval1, crval2,
                            cdelt1, cdelt2, crota2, probase, &tosky );
               if (r) break;
               r = skyco_c( &lon, &lat, skybase, &jl1, &jb1, skysys );
               if (r) break;
               if (jb1 == jb0) break;
               jy1 = jy0 + ( *y1 - jb0 ) * ( jy1 - jy0 ) / ( jb1 - jb0 );
            } while (fabs( *y1 - jb1 ) > EPSILON);
            *x2 = jl1;
            *y2 = jy1;
         }
         break;
      }
      /*
       * The (sky,grid) -> (grid,sky) transformations are done in an
       * iterative way. The convergence is reached when the difference
       * between known coordinates and estimated coordinates are less
       * than EPSILON.
       */
      case 2: {                                 /* (sky, grid) -> (grid, sky) */
         if (*skysys == *skybase) {              /* This can be done directly */
            r = proco_c( x1, y1, x2, y2, crval1, crval2,
                         cdelt1, cdelt2, crota2, probase, mode );
            if (r) break;
         } else {                                  /* This is more cumbersome */
            double lon, lat;
            double jl0, jb0, jl1, jb1; 
            double jx0, jx1;
            fint   tosky = 3;
            
            jx0 = 0.0; jx1 = 1.0;
            r = proco_c( &jx0, y1, &lon, &lat, crval1, crval2,
                         cdelt1, cdelt2, crota2, probase, &tosky );
            if (r) break;
            r = skyco_c( &lon, &lat, skybase, &jl0, &jb0, skysys );
            if (r) break;
            do {                                 /* start iterating procedure */
               r = proco_c( &jx1, y1, &lon, &lat, crval1, crval2,
                            cdelt1, cdelt2, crota2, probase, &tosky );
               if (r) break;
               r = skyco_c( &lon, &lat, skybase, &jl1, &jb1, skysys );
               if (r) break;
               if (jl1 == jl0) break;
               jx1 = jx0 + ( *x1 - jl0 ) * ( jx1 - jx0 ) / ( jl1 - jl0 );
            } while (fabs( *x1 - jl1 ) > EPSILON);
            *x2 = jx1;
            *y2 = jb1;
         }
         break;
      }
      /*
       * The (grid,grid) -> (sky,sky) can be done directly.
       */
      case 3: {                                 /* (grid, grid) -> (sky, sky) */
         double lon, lat;

         r = proco_c( x1, y1, &lon, &lat, crval1, crval2,
                      cdelt1, cdelt2, crota2, probase, mode );
         if (r) break;
         r = skyco_c( &lon, &lat, skybase, x2, y2, skysys );     /* to skysys */
         break;
      }
      default: {
         r = 2;
         break;
      }
   }
   return(r);
}

#if defined(TESTBED)
/*
 * This part is for testing purposes only. It will test whether the
 * above defined routines perform according to specifications.
 */
int main()
{
   double lat0, lon0;
   double ra0 = 45.0, dec0 = 45.0;
   double ra = 45.5, dc = 45.5;
   double x0, y0, x1, y1, x2, y2;
   double crota2 = 0.0;
   double cdelt1 = -0.01, cdelt2 = 0.02;
   double sx1, sy1, sx2, sy2;
   fint   l, m, n;
   fint   skysys, skybase, probase, equ = 1;
   fint   mode;

   /* test skypro */
   for (l = 0; l < 10; l++) {
      probase = l + 1;
      for (m = 0; m < 4; m++) {
         skybase = m + 1;
         skyco_c( &ra0, &dec0, &equ, &lon0, &lat0, &skybase );
         for (n = 0; n < 4; n++) {
            fint r;
            skysys = n + 1;
            printf("skysys = %ld, skybase = %ld, probase = %ld\n",skysys,skybase,probase);
            skyco_c( &ra, &dc, &equ, &x1, &y1, &skysys );
            sx1 = x1; sy1 = y1;
            mode = 0;
            r = skypro_c( &x1, &y1, &x2, &y2,
                          &lon0, &lat0, &cdelt1, &cdelt2, &crota2,
                          &skysys, &skybase, &probase, &mode );
            sx2 = x2; sy2 = y2;
            printf("skypro = %2ld: %15.10f, %15.10f, %15.10f, %15.10f\n",r,x1-sx1,y1-sy1,x2-sx2,y2-sy2);
            x2 = sx2; y1 = sy1; mode = 1;
            r = skypro_c( &x2, &y1, &x1, &y2,
                          &lon0, &lat0, &cdelt1, &cdelt2, &crota2,
                          &skysys, &skybase, &probase, &mode );
            printf("skypro = %2ld: %15.10f, %15.10f, %15.10f, %15.10f\n",r,x1-sx1,y1-sy1,x2-sx2,y2-sy2);
            x1 = sx1; y2 = sy2; mode = 2;
            r = skypro_c( &x1, &y2, &x2, &y1,
                          &lon0, &lat0, &cdelt1, &cdelt2, &crota2,
                          &skysys, &skybase, &probase, &mode );
            printf("skypro = %2ld: %15.10f, %15.10f, %15.10f, %15.10f\n",r,x1-sx1,y1-sy1,x2-sx2,y2-sy2);
            x2 = sx2; y2 = sy2; mode = 3;
            r = skypro_c( &x2, &y2, &x1, &y1,
                          &lon0, &lat0, &cdelt1, &cdelt2, &crota2,
                          &skysys, &skybase, &probase, &mode );
            printf("skypro = %2ld: %15.10f, %15.10f, %15.10f, %15.10f\n",r,x1-sx1,y1-sy1,x2-sx2,y2-sy2);
         }
      }
   }
}
#endif
