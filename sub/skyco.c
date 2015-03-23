/* skyco.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            skyco.dc2

Function:     skyco

Purpose:      Transformation between equatorial, galactic, ecliptic and
              supergalactic coordinates.

File:         skyco.c

Author:       K.G. Begeman

Use:          INTEGER SKYCO ( XIN     ,   Input    double precision
                              YIN     ,   Input    double precision
                              TYPEIN  ,   Input    integer
                              XOUT    ,   Output   double precision
                              YOUT    ,   Output   double precision
                              TYPEOUT )   Input    integer

              SKYCO      0: transformation successful
                         5: input sky system unknown
                         6: output sky system unknown
                         7: input and output sky system unknown
              XIN        Input X coordinate in degrees.
              YIN        Input Y coordinate in degrees.
              TYPEIN     Type of input coordinates:
                         1   equatorial (1950.0)
                         2   galactic
                         3   ecliptic
                         4   supergalactic
                         5   equatorial (2000.0)
              XOUT       Output X coordinate in degrees.
              YOUT       Output Y coordinate in degrees.
              TYPEOUT    Type of output coordinates:
                         1   equatorial (1950.0)
                         2   galactic
                         3   ecliptic
                         4   supergalactic
                         5   equatorial (2000.0)

Updates:      Jul  4, 1989: KGB, Document created.
              Jan 17, 1990: KGB, Epoche 2000.0 sky system.

#<

Fortran to C interface:

@ integer function skyco( double precision ,
@                         double precision ,
@                         integer          ,
@                         double precision ,
@                         double precision ,
@                         integer          )

*/

#include "stdio.h"
#include "math.h"
#include "gipsyc.h"

/* Some definitions */
#define PI          3.1415926535897932385                        /* number PI */
#define TWOPI       6.2831853071795864769                  /* twice number PI */
#define PIHALF      1.5707963267948966192           /* number PI divided by 2 */
#define degrad(x) (57.2957795130823208768*(x))          /* radians -> degrees */
#define raddeg(x) ( 0.0174532925199432958*(x))          /* degrees -> radians */

/* Define matrices */
static double M11[9] = {            /* Equatorial 1950.0 -> Equatorial 1950.0 */
      1.0000000000,   0.0000000000,   0.0000000000,
      0.0000000000,   1.0000000000,   0.0000000000,
      0.0000000000,   0.0000000000,   1.0000000000
};
static double M12[9] = {                     /* Equatorial 1950.0 -> Galactic */
     -0.0669887394,  -0.8727557659,  -0.4835389146,
      0.4927284660,  -0.4503469580,   0.7445846333,
     -0.8676008112,  -0.1883746012,   0.4601997848
};
static double M13[9] = {                     /* Equatorial 1950.0 -> Ecliptic */
      1.0000000000,   0.0000000000,   0.0000000000,
      0.0000000000,   0.9174369529,   0.3978811850,
      0.0000000000,  -0.3978811850,   0.9174369529
};
static double M14[9] = {               /*  Equatorial 1950.0 -> Supergalactic */
      0.3831583954,   0.3366379840,   0.8601537722,
     -0.8972185056,  -0.0856688522,   0.4331971849,
      0.2195190133,  -0.9377290203,   0.2692130889
};
static double M15[9] = {            /* Equatorial 1950.0 -> Equatorial 2000.0 */
      0.9999257453,  -0.0111761178,  -0.0048578157,
      0.0111761178,   0.9999375449,  -0.0000271491,
      0.0048578157,  -0.0000271444,   0.9999882004
};
static double M21[9] = {                     /* Galactic -> Equatorial 1950.0 */
     -0.0669887394,   0.4927284660,  -0.8676008112,
     -0.8727557659,  -0.4503469580,  -0.1883746012,
     -0.4835389146,   0.7445846333,   0.4601997848
};
static double M22[9] = {                              /* Galactic -> Galactic */
      1.0000000000,   0.0000000000,   0.0000000000,
      0.0000000000,   1.0000000000,   0.0000000000,
      0.0000000000,   0.0000000000,   1.0000000000
};
static double M23[9] = {                              /* Galactic -> Ecliptic */
     -0.0669887394,   0.4927284660,  -0.8676008112,
     -0.9930894268,  -0.1169087247,   0.0102830156,
     -0.0963633701,   0.8622940385,   0.4971549978
};
static double M24[9] = {                         /* Galactic -> Supergalactic */
     -0.7353878609,   0.6776464374,   0.0000000002,
     -0.0745961752,  -0.0809524239,   0.9939225904,
      0.6735281025,   0.7309186075,   0.1100812618
};
static double M25[9] = {                     /* Galactic -> Equatorial 2000.0 */
     -0.0548808010,   0.4941079543,  -0.8676666568,
     -0.8734368042,  -0.4448322550,  -0.1980717391,
     -0.4838349376,   0.7469816560,   0.4559848231
};
static double M31[9] = {                     /* Ecliptic -> Equatorial 1950.0 */
      1.0000000000,   0.0000000000,   0.0000000000,
      0.0000000000,   0.9174369529,  -0.3978811850,
      0.0000000000,   0.3978811850,   0.9174369529
};
static double M32[9] = {                              /* Ecliptic -> Galactic */
     -0.0669887394,  -0.9930894268,  -0.0963633701,
      0.4927284660,  -0.1169087247,   0.8622940385,
     -0.8676008112,   0.0102830156,   0.4971549978
};
static double M33[9] = {                              /* Ecliptic -> Ecliptic */
      1.0000000000,   0.0000000000,   0.0000000000,
      0.0000000000,   1.0000000000,   0.0000000000,
      0.0000000000,   0.0000000000,   1.0000000000
};
static double M34[9] = {                         /* Ecliptic -> Supergalactic */
      0.3831583954,   0.6510831284,   0.6551949358,
     -0.8972185056,   0.0937652385,   0.4315171298,
      0.2195190133,  -0.7531924322,   0.6200907698
};
static double M35[9] = {                     /* Ecliptic -> Equatorial 2000.0 */
      0.9999257453,  -0.0121862169,  -0.0000099726,
      0.0111761178,   0.9173688522,  -0.3978812429,
      0.0048578157,   0.3978515869,   0.9174369278
};
static double M41[9] = {                /* Supergalactic -> Equatorial 1950.0 */
      0.3831583954,  -0.8972185056,   0.2195190133,
      0.3366379840,  -0.0856688522,  -0.9377290203,
      0.8601537722,   0.4331971849,   0.2692130889
};
static double M42[9] = {                         /* Supergalactic -> Galactic */
     -0.7353878609,  -0.0745961752,   0.6735281025,
      0.6776464374,  -0.0809524239,   0.7309186075,
      0.0000000002,   0.9939225904,   0.1100812618
};
static double M43[9] = {                         /* Supergalactic -> Ecliptic */
      0.3831583954,  -0.8972185056,   0.2195190133,
      0.6510831284,   0.0937652385,  -0.7531924322,
      0.6551949358,   0.4315171298,   0.6200907698
};
static double M44[9] = {                    /* Supergalactic -> Supergalactic */
      1.0000000000,   0.0000000000,   0.0000000000,
      0.0000000000,   1.0000000000,   0.0000000000,
      0.0000000000,   0.0000000000,   1.0000000000
};
static double M45[9] = {                /* Supergalactic -> Equatorial 2000.0 */
      0.3751891698,  -0.8982988298,   0.2286750954,
      0.3408758302,  -0.0957026824,  -0.9352243929,
      0.8619957978,   0.4288358766,   0.2703017493
};
static double M51[9] = {            /* Equatorial 2000.0 -> Equatorial 1950.0 */
      0.9999257453,   0.0111761178,   0.0048578157,
     -0.0111761178,   0.9999375449,  -0.0000271444,
     -0.0048578157,  -0.0000271491,   0.9999882004
};
static double M52[9] = {                     /* Equatorial 2000.0 -> Galactic */
     -0.0548808010,  -0.8734368042,  -0.4838349376,
      0.4941079543,  -0.4448322550,   0.7469816560,
     -0.8676666568,  -0.1980717391,   0.4559848231
};
static double M53[9] = {                     /* Equatorial 2000.0 -> Ecliptic */
      0.9999257453,   0.0111761178,   0.0048578157,
     -0.0121862169,   0.9173688522,   0.3978515869,
     -0.0000099726,  -0.3978812429,   0.9174369278
};
static double M54[9] = {                /* Equatorial 2000.0 -> Supergalactic */
      0.3751891698,   0.3408758302,   0.8619957978,
     -0.8982988298,  -0.0957026824,   0.4288358766,
      0.2286750954,  -0.9352243929,   0.2703017493
};
static double M55[9] = {            /* Equatorial 2000.0 -> Equatorial 2000.0 */
      1.0000000000,   0.0000000000,   0.0000000000,
      0.0000000000,   1.0000000000,   0.0000000000,
      0.0000000000,   0.0000000000,   1.0000000000
};

fint skyco_c( double *xin,
              double *yin,
              fint   *min,
              double *xou,
              double *you,
              fint   *mou )
{
   fint    r = 0;
   double  v[3], v0[3];
   double *ptr;

   switch(*min) {
      case 1: {                     /* input in equatorial coordinates 1950.0 */
         switch(*mou) {
            case 1: ptr = M11; break;  /* output in equatorial coords. 1950.0 */
            case 2: ptr = M12; break;       /* output in galactic coordinates */
            case 3: ptr = M13; break;       /* output in ecliptic coordinates */
            case 4: ptr = M14; break;  /* output in supergalactic coordinates */
            case 5: ptr = M15; break;  /* output in equatorial coords. 2000.0 */
            default: r = 6; break;
         }
         break;
      }
      case 2: {                              /* input in galactic coordinates */
         switch(*mou) {
            case 1: ptr = M21; break;  /* output in equatorial coords. 1950.0 */
            case 2: ptr = M22; break;       /* outpur in galactic coordinates */
            case 3: ptr = M23; break;       /* output in ecliptic coordinates */
            case 4: ptr = M24; break;  /* output in supergalactic coordinates */
            case 5: ptr = M25; break;  /* output in equatorial coords. 2000.0 */
            default: r = 6; break;
         }
         break;
      }
      case 3: {                              /* input in ecliptic coordinates */
         switch(*mou) {
            case 1: ptr = M31; break;  /* output in equatorial coords. 1950.0 */
            case 2: ptr = M32; break;       /* output in galactic coordinates */
            case 3: ptr = M33; break;       /* output in ecliptic coordinates */
            case 4: ptr = M34; break;  /* output in supergalactic coordinates */
            case 5: ptr = M35; break;  /* output in equatorial coords. 2000.0 */
            default: r = 6; break;
         }
         break;
      }
      case 4: {                         /* input in supergalactic coordinates */
         switch(*mou) {
            case 1: ptr = M41; break;  /* output in equatorial coords. 1950.0 */
            case 2: ptr = M42; break;       /* output in galactic coordinates */
            case 3: ptr = M43; break;       /* output in ecliptic coordinates */
            case 4: ptr = M44; break;  /* output in supergalactic coordinates */
            case 5: ptr = M45; break;  /* output in equatorial coords. 2000.0 */
            default: r = 6; break;
         }
         break;
      }
      case 5: {                     /* input in equatorial coordinates 2000.0 */
         switch(*mou) {
            case 1: ptr = M51; break;  /* output in equatorial coords. 1950.0 */
            case 2: ptr = M52; break;       /* output in galactic coordinates */
            case 3: ptr = M53; break;       /* output in ecliptic coordinates */
            case 4: ptr = M54; break;  /* output in supergalactic coordinates */
            case 5: ptr = M55; break;  /* output in equatorial coords. 2000.0 */
            default: r = 6; break;
         }
         break;
      }
      default: {
         switch(*mou) {
            case 1: r = 5; break;
            case 2: r = 5; break;
            case 3: r = 5; break;
            case 4: r = 5; break;
            case 5: r = 5; break;
            default: r = 7; break;
         }
      }
   }
   if (!r) {
      if (*mou == *min) {                                  /* simple solution */
         *xou = *xin;
         *you = *yin;
      } else {                                               /* do the matrix */
         double lon = raddeg(*xin);
         double lat = raddeg(*yin);
         fint   i, j;

         v0[0] = cos( lon ) * cos( lat );
         v0[1] = sin( lon ) * cos( lat );
         v0[2] = sin( lat );
         for (j = 0; j < 3; j++) {
            v[j] = 0.0;
            for (i = 0; i < 3; v[j] = v[j] + v0[i++] * (*ptr++));
         }
         if ((v[0] == 0.0) && (v[1] == 0.0)) {
            lon = 0.0;
            if (v[2] > 0.0) lat = PIHALF; else lat = -PIHALF;
         } else {
            lon = atan2( v[1], v[0] );
            if (lon < 0.0) lon = lon + TWOPI;
            lat = atan2( v[2], sqrt( v[0] * v[0] + v[1] * v[1] ) );
         }
         *xou = degrad( lon );
         *you = degrad( lat );
      }
   }
   return( r );
}

/*
#>            epoco.dc2

Subroutine:   EPOCO

Purpose:      Transforms equatorial coordinates from one epoch
              to another epoch.

File:         skyco.c

Author:       K.G. Begeman

Use:          CALL EPOCO( RA1    ,   Input    double precision
                          DEC1   ,   Input    double precision
                          EPOCH1 ,   Input    double precision
                          RA2    ,   Output   double precision
                          DEC2   ,   Output   double precision
                          EPOCH2 )   Input    double precision

              RA1         Input R.A. in degrees at EPOCH1.
              DEC1        Input Dec. in degrees at EPOCH1.
              EPOCH1      EPOCH1 in years for RA1 and DEC1.
              RA2         Output R.A. in degrees at EPOCH2.
              DEC2        Output Dec. in degrees at EPOCH2
              EPOCH2      EPOCH2 in years for RA2 and DEC2.

Updates:      Feb  5, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine epoco( double precision ,
@                   double precision ,
@                   double precision ,
@                   double precision ,
@                   double precision ,
@                   double precision )

*/

static double EPOCH1 = 1950.0;                                      /* epoch1 */
static double EPOCH2 = 1950.0;                                      /* epoch2 */

static double E12[9] = {                       /* matrix for epoch1 -> epoch2 */
      1.0000000000,   0.0000000000,   0.0000000000,
      0.0000000000,   1.0000000000,   0.0000000000,
      0.0000000000,   0.0000000000,   1.0000000000
};
static double E21[9] = {                       /* matrix for epoch1 -> epoch2 */
      1.0000000000,   0.0000000000,   0.0000000000,
      0.0000000000,   1.0000000000,   0.0000000000,
      0.0000000000,   0.0000000000,   1.0000000000
};

void epoco_c( double *a1 ,
              double *d1 ,
              double *t1 ,
              double *a2 ,
              double *d2 ,
              double *t2 )
{
   if (*t1 == *t2) {                                        /* quick solution */
      *a2 = *a1;
      *d2 = *d1;
   } else {                                        /* we need to do something */
      double  a, d;
      double  cosa, cosd, sina, sind;
      double  lon, lat;
      double  v[3], v0[3];
      double *ptr;
      fint    i, j;

      if (EPOCH1 == *t1 && EPOCH2 == *t2) {
         ptr = E12;                             /* we already have the matrix */
      } else if (EPOCH2 == *t1 && EPOCH1 == *t2) {
         ptr = E21;                /* we also have the inverse transformation */
      } else {
         double tau1, tau2;               /* epoch in centuries w.r.t. 1900.0 */
         double p, t, z;
         double cosp, cost, cosz, sinp, sint, sinz;

         ptr = E12;                                 /* from epoch1 to epoch 2 */
         EPOCH1 = *t1; EPOCH2 = *t2;                            /* save epoch */
         tau1 = 0.01 * (EPOCH1 - 1900.0);            /* based on epoch 1900.0 */
         tau2 = 0.01 * (EPOCH2 - EPOCH1);                       /* difference */
         p = ( ( 2304.250 + 1.396 * tau1 ) + ( 0.302 + 0.018 * tau2 ) * tau2 ) * tau2;
         t = ( ( 2004.682 - 0.853 * tau1 ) - ( 0.426 + 0.042 * tau2 ) * tau2 ) * tau2;
         z = ( ( 2304.250 + 1.396 * tau1 ) + ( 1.093 + 0.019 * tau2 ) * tau2 ) * tau2;
         p = raddeg( p / 3600.0 ); cosp = cos( p ); sinp = sin( p );
         t = raddeg( t / 3600.0 ); cost = cos( t ); sint = sin( t );
         z = raddeg( z / 3600.0 ); cosz = cos( z ); sinz = sin( z );
         E21[0] = E12[0] = -sinz * sinp + cosz * cost * cosp;
         E21[3] = E12[1] = -sinz * cosp - cosz * cost * sinp;
         E21[6] = E12[2] = -cosz * sint;
         E21[1] = E12[3] =  cosz * sinp + sinz * cost * cosp;
         E21[4] = E12[4] =  cosz * cosp - sinz * cost * sinp;
         E21[7] = E12[5] = -sinz * sint;
         E21[2] = E12[6] =  cosp * sint;
         E21[5] = E12[7] = -sinp * sint;
         E21[8] = E12[8] =  cost;
      }
      a = raddeg( *a1 ); cosa = cos( a ); sina = sin( a );
      d = raddeg( *d1 ); cosd = cos( d ); sind = sin( d );
      v0[0] = cosa * cosd;
      v0[1] = sina * cosd;
      v0[2] = sind;
      for (j = 0; j < 3; j++) {                            /* Matrix * v0 = v */
         v[j] = 0.0;
         for (i = 0; i < 3; v[j] = v[j] + v0[i++] * (*ptr++));
      }
      if ((v[0] == 0.0) && (v[1] == 0.0)) {
         lon = 0.0;
         if (v[2] > 0.0) lat = PIHALF; else lat = -PIHALF;
      } else {
         lon = atan2( v[1], v[0] );
         if (lon < 0.0) lon = lon + TWOPI;
         lat = atan2( v[2], sqrt( v[0] * v[0] + v[1] * v[1] ) );
      }
      *a2 = degrad( lon );
      *d2 = degrad( lat );
   }
}

#if defined(TESTBED)
main()
{
   double lat = 45.0, lon = 45.0;
   double x1, y1, x2, y2;
   double p1, p2;
   double t1, t2;
   fint   m1, m2;
   fint   n;

   printf("Testing SKYCO\n");
   for (m1 = 0; m1++ < 5; ) {
      for (m2 = 0; m2++ < 5; ) {
         n = skyco_c( &lon, &lat, &m1, &x1, &y1, &m2 );
         n = skyco_c( &x1, &y1, &m2, &x2, &y2, &m1 );
         p1 = 2.0 * (lon - x2) / (lon + x2);
         p2 = 2.0 * (lat - y2) / (lat + y2);
         printf("skyco = %ld: %ld, %ld, %18.15f, %18.15f\n", n, m1, m2, p1, p2 );
      }
   }
   printf("Testing EPOCO\n");
   for (n = 0; n < 10; n++) {
      double x1, y1, x2, y2, d1, d2, t1 = 1950.0, t2 = 2000.0;
      fint   r, m1 = 1, m2 = 5;

      x1 = 360.0 * ((double) rand()) / 32767.0;
      y1 = 90.0 * ((double) (rand() - 16383)) / 16383.0;
      epoco_c( &x1, &y1, &t1, &d1, &d2, &t2 );
      r = skyco_c( &x1, &y1, &m1, &x2, &y2, &m2 );
      d1 -= x2; d2 -= y2;
      printf("Differences RA (2000.0) %13.10f DEC (2000.0) %13.10f\n", d1, d2 );
      epoco_c( &x2, &y2, &t2, &d1, &d2, &t1 );
      r = skyco_c( &x2, &y2, &m2, &x1, &y1, &m1 );
      d1 -= x1; d2 -= y1;
      printf("Differences RA (1950.0) %13.10f DEC (1950.0) %13.10f\n", d1, d2 );
   }
}
#endif
