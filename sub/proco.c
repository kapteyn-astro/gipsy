/* proco.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            proco.dc2

Function:     PROCO

Purpose:      Function which does the conversion of grid to sky
              coordinates and vice versa.

File:         proco.c

Author:       K.G. Begeman

Use:          INTEGER PROCO( XIN   ,     Input      double precision
                             YIN   ,     Input      double precision
                             XOUT  ,     Output     double precision
                             YOUT  ,     Output     double precision
                             CRVAL1,     Input      double precision
                             CRVAL2,     Input      double precision
                             CDELT1,     Input      double precision
                             CDELT2,     Input      double precision
                             CROTA2,     Input      double precision
                             PROSYS,     Input      integer
                             MODE  )     Input      integer

              PROCO    0: successful conversion
                       1: unknown projection
                       2: unknown mode
                       3: CROTA2 = 90.0 for mode 1 and 2
                       4: CDELT1 or CDELT2 equal to zero
                       5: Input or internal values outside range of 
                          this projection
              XIN      Input X coordinate (Longitude in degrees or
                       X grids).
              YIN      Input Y coordinate (Latitude in degrees or
                       Y grids).
              XOU      Output X coordinate (X grids or Longitude
                       in degrees).
              YOU      Output Y coordinates (Y grids or Latitude
                       in degrees).
              CRVAL1   Projection centre longitude (Longitude in
                       degrees).
              CRVAL2   Projection centre latitude (Latitude in
                       degrees).
              CDELT1   X grid separation in degrees.
              CDELT2   Y grid separation in degrees.
              CROTA2   Rotation angle in degrees.
              PROSYS   Type of projection:
                        1    AITOFF equal area projection
                        2    Equivalent Cylindrical projection
                        3    Flat projection
                        4    Gnomonic projection
                        5    Orthographic projection
                        6    Rectangular projection
                        7    Global Sinusoidal projection
                        8    North Celestial Pole projection
                        9    Stereographic projection
                       10    Mercator projection
              MODE     Mode determines what type of input/output
                       coordinates are given/wanted.
                       MODE       XIN     YIN   XOUT   YOUT
                         0        LON     LAT      X      Y
                         1          X     LAT    LON      Y
                         2        LON       Y      X    LAT
                         3          X       Y    LON    LAT

Updates:      Jan  5, 1990: KGB, Document created.
              Sep 09, 1998: VOG, Replaced -180, 180 degrees restriction 
                            for AITOFF and RECTANGULAR (a,y) -> (x,d)
                            projections by 0, 360 restriction.
              Jan 24, 2007: Changed post condition for aitoff

#<

Fortran to C interface:

@ integer function proco( double precision ,
@                         double precision ,
@                         double precision ,
@                         double precision ,
@                         double precision ,
@                         double precision ,
@                         double precision ,
@                         double precision ,
@                         double precision ,
@                         integer          ,
@                         integer          )

*/

#include "stdio.h"
#include "math.h"
#include "gipsyc.h"

#define MAXPRO      10                               /* number of projections */
#define PI          3.1415926535897932385                        /* number PI */
#define TWOPI       6.2831853071795864769                  /* twice number PI */
#define PIHALF      1.5707963267948966192           /* number PI divided by 2 */
#define degrad(x) (57.2957795130823208768*(x))          /* radians -> degrees */
#define raddeg(x) ( 0.0174532925199432958*(x))          /* degrees -> radians */
#define ACCURACY    0.0000000001
#define MAXITERS    200                /* > 2log(i0/i) (initial interval size */
                                               /* divided by wanted accuracy. */

static struct {
   double a0;                                  /* longitude projection centre */
   double sina0;                       /* sine of longitude projection centre */
   double cosa0;                     /* cosine of longitude projection centre */
   double d0;                                   /* latitude projection centre */
   double sind0;                        /* sine of latitude projection centre */
   double cosd0;                      /* cosine of latitude projection centre */
   double rho;                                              /* rotation angle */
   double sinrho;                                   /* sine of rotation angle */
   double cosrho;                                 /* cosine of rotation angle */
} pro[MAXPRO] = {
   { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 },
   { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 },
   { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 },
   { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 },
   { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 },
   { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 },
   { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 },
   { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 },
   { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 },
   { 0.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0 }
};

static struct {                     /* buffer for aitoff projection constants */
   double a0;                       /* longitude projection centre in degrees */
   double d0;                        /* latitude projection centre in degrees */
   double dx;                              /* grid separation in x in degrees */
   double dy;                              /* grid separation in y in degrees */
   double fa;                                  /* scaling factor in longitude */
   double fd;                                   /* scaling factor in latitude */
   double m0;                                                     /* constant */
   double rho;
} ait = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };

static struct {                   /* buffer for mercator projection constants */
   double a0;                       /* longitude projection centre in degrees */
   double d0;                        /* latitude projection centre in degrees */
   double dx;                              /* grid separation in x in degrees */
   double dy;                              /* grid separation in y in degrees */
   double fa;                                  /* scaling factor in longitude */
   double fd;                                   /* scaling factor in latitude */
   double m0;                                                     /* constant */
   double rho;
} mer = { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 };


static double fxXDtoAY( double x,
                        double f,
                        double b,
                        double k )
/*----------------------------------------------------------------------*/
/* PURPOSE: Evaluate function used in AITOFF projection with:           */
/* (x,d) -> (a,y)                                                       */
/*----------------------------------------------------------------------*/

{   
   double  fx;
   
  
   fx = f*sqrt(0.5+b*cos(x))-k-sin(x);
   return( fx );
}



static double fxAYtoXD( double x,
                        double f,
                        double b,
                        double k )
/*----------------------------------------------------------------------*/
/* PURPOSE: Evaluate function used in AITOFF projection with:           */
/* (a,y) -> (x,d)                                                       */
/*----------------------------------------------------------------------*/
{
   double  fx;
   
  
   fx = f*sqrt(0.5+b*cos(x))+k*cos(x)-sin(x);
   return( fx );
}



static int interval( double   (*fx)(double,double,double,double),
                     double   x1,
                     double   x2,
                     double   *xx1,
                     double   *xx2,
                     double   f, double b, double k )
/*----------------------------------------------------------------------*/
/* PURPOSE: Look in interval [x1,x2] whether the function fx has a root */
/* by dividing the interval in n sub-intervals. Return the interval     */
/* where the first root is found or return an error code if there are   */
/* no roots. The functions that are examined have periods in the order  */
/* 2*PI so only the first root is necessary.                            */
/*----------------------------------------------------------------------*/
{
   double   x, dx;
   double   fp, fc;
   int      n = 10;
   int      i;
   
  
   dx = (x2-x1)/(double) n;
   x = x1;
   fp = (*fx)(x,f,b,k);
   for (i = 0; i < n; i++)
   {
      fc = (*fx)(x+dx,f,b,k);
      if (fc*fp < 0.0)   /* Sign changed */
      {
         *xx1 = x;
         *xx2 = x + dx;
         return( 1 );
      }
      x += dx;
   }
   return( 0 );                                  /* No roots in this interval */
}


static int bisect( double  (*fx)(double,double,double,double),
                   double  xx1,
                   double  xx2,
                   double  f, double b, double k,
                   int     jmax,
                   double  xacc,
                   double  *xroot )
/*----------------------------------------------------------------------*/
/* PURPOSE: Find the root of fx in interval [xx1,xx2] using an ordinary */
/* bisection method. Before the method is applied, an (sub) interval is */
/* determined where the function must have a root.                      */
/*----------------------------------------------------------------------*/
{
   double  dx, fu, fmid, xmid, rtb;
   int     j;
   double  x1, x2;


   if (!interval((*fx),xx1,xx2,&x1,&x2, f, b, k))
   {
      /* No roots between xx1 and xx2 */
      return( 0 );      
   }
      
   fu = (*fx)(x1,f,b,k);
   fmid = (*fx)(x2,f,b,k);
   if (fu == 0.0)
   {
      *xroot = x1 ;
      return( 1 );   
   } 
   if (fmid == 0.0)
   {
      *xroot = x2 ;
      return( 1 );
   }
   if (fu*fmid >= 0.0)
   {
      return(0);
   }
         
   if (fu < 0.0)
   {
      dx = x2-x1;
      rtb = x1;      
   }
   else
   {
      dx = x1-x2;
      rtb = x2;
   }
   for (j = 0; j < jmax; j++)
   {
      dx *= 0.5;
      xmid = rtb + dx;
      fmid = (*fx)(xmid,f,b,k);
      if (fmid <= 0.0)
         rtb = xmid;
      if (fabs(dx) <= xacc || fmid == 0.0)
      {
         *xroot = rtb ;
         return( 1 );
      }
   }
   return( 0 );
}



fint proco_c( double *xin  ,
              double *yin  ,
              double *xou  ,
              double *you  ,
              double *a0   ,
              double *d0   ,
              double *dx   ,
              double *dy   ,
              double *rho  ,
              fint   *sys  ,
              fint   *mode )
{
   fint p = (*sys - 1);
   fint r = 0;

   *xou = *xin; *you = *yin;                                         /* reset */
   if ((p < MAXPRO) && (p > -1)) {                 /* legal projection system */
      if (*a0 != pro[p].a0) {                  /* put in buffer for later use */
         pro[p].a0 = *a0;                   /* longitude of projection centre */
         pro[p].sina0 = sin( raddeg( *a0 ) );            /* sine of longitude */
         pro[p].cosa0 = cos( raddeg( *a0 ) );          /* cosine of longitude */
      }
      if (*d0 != pro[p].d0) {                  /* put in buffer for later use */
         pro[p].d0 = *d0;                    /* latitude of projection centre */
         pro[p].sind0 = sin( raddeg( *d0 ) );             /* sine of latitude */
         pro[p].cosd0 = cos( raddeg( *d0 ) );           /* cosine of latitude */
      }
      if (*rho != pro[p].rho) {                /* put in buffer for later use */
         pro[p].rho = *rho;                                 /* rotation angle */
         pro[p].sinrho = sin( raddeg( *rho ) );     /* sine of rotation angle */
         pro[p].cosrho = cos( raddeg( *rho ) );   /* cosine of rotation angle */
      }
   }
   switch(*sys) {                                 /* select projection system */
      case 1: {                               /* Aitoff equal area projection */
         if ((*dx != ait.dx) || (*dy != ait.dy) || (*a0 != ait.a0) || (*d0 != ait.d0) || (*rho != ait.rho)) {
            double t1, t2, t3, dda, ddd;

            ait.dx = *dx;
            ait.dy = *dy;
            ait.a0 = *a0;
            ait.d0 = *d0;
            ait.rho = *rho;
            dda = raddeg( *dx * pro[p].cosrho - *dy * pro[p].sinrho );
            ddd = raddeg( *dy * pro[p].cosrho + *dx * pro[p].sinrho );
            t1 = sqrt( ( 1.0 + pro[p].cosd0 * cos( dda / 2.0 ) ) / 2.0 );
            t2 = 2.0 * pro[p].cosd0 * sin( dda / 2.0 );
            ait.fa = dda * t1 / t2;
            t1 = sin( pro[p].d0 + ddd );
            t2 = sqrt( ( 1.0 + cos( pro[p].d0 + ddd ) ) / 2.0 );
            t3 = sin( pro[p].d0 ) / sqrt( ( 1.0 + cos( pro[p].d0 ) ) / 2.0 );
            ait.fd = ddd / ( t1 / t2 - t3 );
            ait.m0 = ait.fd * pro[p].sind0 / sqrt( ( 1.0 + pro[p].cosd0 ) / 2.0 );
         }
         switch(*mode) {                                 /* which way to go ? */
            case 0: {                                       /* (a,d) -> (x,y) */
               double d, l, m, z, da, cosd, sind, cosa, sina;

               da = raddeg( ( *xin - pro[p].a0 ) / 2.0 );
               cosa = cos( da );
               sina = sin( da );
               d = raddeg( *yin );
               cosd = cos( d );
               sind = sin( d );
               z = sqrt( ( 1.0 + cosd * cosa ) / 2.0 );
               l = 2.0 * ait.fa * cosd * sina / z;
               m = ait.fd * sind / z - ait.m0;
               *xou = degrad( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = degrad( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 1: {                                       /* (x,d) -> (a,y) */
               double a, b, c, x, d, z, da, daold, cosd, sind;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               x = raddeg( *xin * *dx );               
               d = raddeg( *yin );
               if (d > PI/2.0 || d < -PI/2.0) { r = 5; break; }
               cosd = cos( d );
               sind = sin( d );
               if (cosd == 0.0) { r = 5; break; }               
               {
                  double b,c,d,e,h,k;                  
                  b = 0.5 * cosd;
                  c = ait.m0 * pro[p].sinrho;
                  d = pro[p].sinrho * ait.fd * sind;
                  e = 2.0 * ait.fa * pro[p].cosrho * cosd;                  
                  h = (fabs(x)+c) / e;
                  k = d / e;                                     
                  if (!bisect( (*fxXDtoAY),               /* Function pointer */
                                -PI+0.0001, PI-0.0001,               /* Range */
                                h, b, k,               /* Function parameters */
                                MAXITERS,
                                ACCURACY,
                                &da))                     /* Root of function */
                  {
                     /* Do not replace with 'break' */
                     return(5);
                  }
               }

#ifdef HHHHH              
               da = 0.0;
               do {
                  daold = da;
                  z = sqrt( ( 1.0 + cosd * cos( da ) ) / 2.0 );
                  a = x * z;
                  b = pro[p].sinrho * ( ait.m0 * z - ait.fd * sind );
                  c = 2.0 * ait.fa * cosd * pro[p].cosrho;
                  da = asin( ( a + b ) / c );
               } while (fabs( da - daold ) > 0.0000000001);
#endif               

               z = sqrt( ( 1.0 + cosd * cos( da ) ) / 2.0 );      
               a = ait.fd * sind / z;
               b = x * pro[p].sinrho;
               *xou = pro[p].a0 + degrad( 2.0 * da );               
               *you = degrad( ( a - b - ait.m0 ) / pro[p].cosrho ) / *dy;
               /*-------------------------------------------------------*/
               /* if (*xou > 180.0 || *xou < -180.0) { r = 5; break; }  */
               /* Removed by VOG (25-01-2007). Condition incompatible   */
               /* with previous restriction that angle is between 0 and */
               /* 360 deg.                                              */
               /*-------------------------------------------------------*/               
               break;
            }
            case 2: {                                       /* (a,y) -> (x,d) */
               double d, q, r, s, t, x, y, z, da, dold, cosd, cosa, sina;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               if (*xin > 360.0 || *xin < 0.0) { r = 5; break; }
               da = raddeg( ( *xin - pro[p].a0 ) / 2.0 );               
               if (da >= PI || da <= -PI) { r = 5; break; }               
               cosa = cos( da );
               sina = sin( da );
               y = raddeg( *yin * *dy );
               if (y >= PI/2.0 || y <= -PI/2.0) { r = 5; break; }
               d = raddeg( pro[p].d0 );

#ifdef HHHHH
               do {
                  double   arg;
                  dold = d;
                  cosd = cos( d );
                  z = sqrt( ( 1.0 + cosd * cosa ) / 2.0 );
                  q = y * z;
                  r = ait.m0 * z * pro[p].cosrho;
                  s = 2 * ait.fa * cosd * pro[p].sinrho * sina;
                  t = ait.fd * pro[p].cosrho;
                  arg = (q + r + s ) / t;
                  if (arg > 1.0 || arg < -1.0)
                     return(5);                                          
                  d = asin( ( q + r + s ) / t );
               } while (fabs( d - dold ) > 0.0000000001);               
#endif
               {
                  double b,c,f,e,h;                  
                  b = 2.0 * ait.fa * pro[p].sinrho * sina;
                  c = ait.fd * pro[p].cosrho;
                  e = (y + ait.m0*pro[p].cosrho) / c;
                  f = b / c;
                  h = 0.5 * cosa;
                  if ( !bisect( (*fxAYtoXD),
                                -PI/2.0+0.0001, 
                                PI/2.0-0.0001, 
                                e, h, f, 
                                MAXITERS,
                                ACCURACY,                                
                                &d) ) 
                  {
                     return(5);
                  }
               }               
               if (d <= -PI/2.0 || d > PI/2.0) { r = 5; break; }
               cosd = cos( d );                                          
               z = sqrt( ( 1.0 + cosd * cosa ) / 2.0 );
               x = ( 2.0 * ait.fa * sina * cosd / z + y * pro[p].sinrho );
               *xou = degrad( x / pro[p].cosrho ) / *dx;
               *you = degrad( d );
               if (*you > 90.0 || *you < -90.0) { r = 5; break; }
               if (*xou > 180.0 || *xou < -180.0) { r = 5; break; }
               break;
            }
            case 3: {                                       /* (x,y) -> (a,d) */
               double d, l, m, s, t, x, y, z, da;

               x = raddeg( *xin * *dx );
               y = raddeg( *yin * *dy );
               l = x * pro[p].cosrho - y * pro[p].sinrho;
               m = y * pro[p].cosrho + x * pro[p].sinrho;
               s = l / ait.fa / 2.0;
               t = ( m + ait.m0 ) / ait.fd;
               z = 0.5 * sqrt( 4.0 - s * s - t * t );
               d = asin( t * z );
               da = 2.0 * asin( s * z / cos( d ) );
               *xou = pro[p].a0 + degrad( da );
               *you = degrad( d );
               break;
            }
            default: {                                        /* unknown mode */
               r = 2;
               break;
            }
         }
         break;
      }
      case 2: {                                     /* Cylindrical projection */
         switch(*mode) {                                 /* which way to go ? */
            case 0: {                                       /* (a,d) -> (x,y) */
               double l, m;

               l = *xin - pro[p].a0;
               m = degrad( sin( raddeg( *yin - pro[p].d0 ) ) );
               *xou = ( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = ( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 1: {                                       /* (x,d) -> (a,y) */
               double l, sind;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               sind = degrad( sin( raddeg( *yin - pro[p].d0 ) ) );
               l = ( *xin * *dx - sind * pro[p].sinrho ) / pro[p].cosrho;
               *xou = pro[p].a0 + l;
               *you = ( sind * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 2: {                                       /* (a,y) -> (x,d) */
               double da, m;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               da = (*xin - pro[p].a0);
               m = ( *yin * *dy + da * pro[p].sinrho ) / pro[p].cosrho;
               *xou = ( da * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = pro[p].d0 + degrad( asin( raddeg( m ) ) );
               break;
            }
            case 3: {                                       /* (x,y) -> (a,d) */
               double l, m, x, y;

               x = *xin * *dx;
               y = *yin * *dy;
               l = x * pro[p].cosrho - y * pro[p].sinrho;
               m = y * pro[p].cosrho + x * pro[p].sinrho;
               *xou = pro[p].a0 + l;
               *you = pro[p].d0 + degrad( asin( raddeg( m ) ) );
               break;
            }
            default: {                                        /* unknown mode */
               r = 2;
               break;
            }
         }
         break;
      }
      case 3: {                                            /* Flat projection */
         switch(*mode) {                                 /* which way to go ? */
            case 0: {                                       /* (a,d) -> (x,y) */
               double l, m;

               l = *xin - pro[p].a0;
               m = *yin - pro[p].d0;
               *xou = ( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = ( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 1: {                                       /* (x,d) -> (a,y) */
               double dd, l;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               dd = *yin - pro[p].d0;
               l = ( *xin * *dx - dd * pro[p].sinrho ) / pro[p].cosrho;
               *xou = pro[p].a0 + l;
               *you = ( dd * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 2: {                                       /* (a,y) -> (x,d) */
               double da, m;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               da = *xin - pro[p].a0;
               m = ( *yin * *dy + da * pro[p].sinrho ) / pro[p].cosrho;
               *xou = ( da * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = pro[p].d0 + m;
               break;
            }
            case 3: {                                       /* (x,y) -> (a,d) */
               double l, m, x, y;

               x = *xin * *dx;
               y = *yin * *dy;
               l = x * pro[p].cosrho - y * pro[p].sinrho;
               m = y * pro[p].cosrho + x * pro[p].sinrho;
               *xou = pro[p].a0 + l;
               *you = pro[p].d0 + m;
               break;
            }
            default: {                                        /* unknown mode */
               r = 2;
               break;
            }
         }
         break;
      }
      case 4: {                                        /* Gnomonic projection */
         switch(*mode) {                                 /* which way to go ? */
            case 0: {                                       /* (a,d) -> (x,y) */
               double l, m, da, cosa, sina, cosd, sind, t;

               da = raddeg( *xin - pro[p].a0 );
               cosa = cos( da );
               sina = sin( da );
               cosd = cos( raddeg( *yin ) );
               sind = sin( raddeg( *yin ) );
               
               t = ( sind * pro[p].sind0 + cosd * pro[p].cosd0 * cosa );
               l = cosd * sina / t;               
               m = ( sind * pro[p].cosd0 - cosd * pro[p].sind0 * cosa ) / t;
               *xou = degrad( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = degrad( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 1: {                                       /* (x,d) -> (a,y) */
               double a, b, c, d, s, t, x, da, cosa, sina, cosd, sind, tand;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               x = raddeg( *xin * *dx);
               a = pro[p].cosrho;
               b = x * pro[p].cosd0 + pro[p].sind0 * pro[p].sinrho;
               c = x * pro[p].sind0 - pro[p].cosd0 * pro[p].sinrho;
               d = raddeg( *yin );
               cosd = cos( d );
               sind = sin( d );
               tand = sind / cosd;
               da = atan( b / a ) + asin( tand * c / sqrt( a * a + b * b ) );
               cosa = cos( da );
               sina = sin( da );
               a = sind * pro[p].cosd0 * pro[p].cosrho;
               b = cosd * pro[p].sind0 * cosa * pro[p].cosrho;
               c = cosd * sina * pro[p].sinrho;
               s = sind * pro[p].sind0;
               t = cosd * pro[p].cosd0 * cosa;
               *xou = pro[p].a0 + degrad( da );
               *you = degrad( ( a - b - c ) / ( s + t ) ) / *dy;
               break;
            }
            case 2: {                                       /* (a,y) -> (x,d) */
               double a, b, c, s, t, y, da, cosa, sina, tand;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               da = raddeg( *xin - pro[p].a0 );
               cosa = cos( da );
               sina = sin( da );
               y = raddeg( *yin * *dy);
               a = pro[p].sind0 * cosa * pro[p].cosrho;
               b = sina * pro[p].sinrho;
               c = y * pro[p].cosd0 * cosa;
               s = pro[p].cosd0 * pro[p].cosrho;
               t = y * pro[p].sind0;
               tand = ( a + b + c ) / ( s - t );
               a = sina * pro[p].cosrho;
               b = tand * pro[p].cosd0 * pro[p].sinrho;
               c = pro[p].sind0 * cosa * pro[p].sinrho;
               s = tand * pro[p].sind0;
               t = pro[p].cosd0 * cosa;
               *xou = degrad( ( a + b - c ) / ( s + t ) ) / *dx;
               *you = degrad( atan( tand ) );
               break;
            }
            case 3: {                                       /* (x,y) -> (a,d) */
               double l, m, s, t, x, y, da;

               x = *xin * *dx;
               y = *yin * *dy;
               l = raddeg( x * pro[p].cosrho - y * pro[p].sinrho );
               m = raddeg( y * pro[p].cosrho + x * pro[p].sinrho );
               s = ( m * pro[p].cosd0 + pro[p].sind0 );
               t = ( pro[p].cosd0 - m * pro[p].sind0 );
               da = atan( l / t );
               *xou = pro[p].a0 + degrad( da );
               *you = degrad( atan( cos( da ) * s / t ) );
               if (((*you) * pro[p].d0 ) < 0.0) {
                  if ((*xou)>180.0) (*xou) -= 180.0; else (*xou) += 180.0;
                  (*you) *= -1.0;
               }
             
               break;
            }
            default: {                                        /* unknown mode */
               r = 2;
               break;
            }
         }
         break;
      }
      case 5: {                                    /* Orthographic projection */
         switch(*mode) {                                 /* which way to go ? */
            case 0: {                                       /* (a,d) -> (x,y) */
               double l, m, d, da, cosa, sina, cosd, sind;

               da = raddeg( *xin - pro[p].a0 );
               cosa = cos( da );
               sina = sin( da );
               d = raddeg( *yin );
               cosd = cos( d );
               sind = sin( d );
               l = cosd * sina;
               m = sind * pro[p].cosd0 - cosd * pro[p].sind0 * cosa;
               *xou = degrad( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = degrad( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 1: {                                       /* (x,d) -> (a,y) */
               double a, b, c, d, s, t, x, da, cosd, sind, cosa, sina;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               a = pro[p].cosrho;
               b = pro[p].sind0 * pro[p].sinrho;
               d = raddeg( *yin );
               x = raddeg( *xin * *dx);
               cosd = cos( d );
               sind = sin( d );
               s = x - sind * pro[p].cosd0 * pro[p].sinrho;
               t = cosd * sqrt( a * a + b * b );
               da = atan( b / a ) + asin( s / t );
               cosa = cos( da );
               sina = sin( da );
               a = sind * pro[p].cosd0 * pro[p].cosrho;
               b = cosd * pro[p].sind0 * cosa * pro[p].cosrho;
               c = cosd * sina * pro[p].sinrho;
               *xou = pro[p].a0 + degrad( da );
               *you = degrad( a - b - c ) / *dy;
               break;
            }
            case 2: {                                       /* (a,y) -> (x,d) */
               double a, b, c, d, y, da, cosa, sina, cosd, sind;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               da = raddeg( *xin - pro[p].a0 );
               cosa = cos( da );
               sina = sin( da );
               y = raddeg( *yin * *dy );
               a = pro[p].cosd0 * pro[p].cosrho;
               b = pro[p].sind0 * cosa * pro[p].cosrho + sina * pro[p].sinrho;
               d = atan( b / a ) + asin( y / sqrt( a * a + b * b ) );
               cosd = cos( d );
               sind = sin( d );
               a = cosd * sina * pro[p].cosrho;
               b = sind * pro[p].cosd0 * pro[p].sinrho;
               c = cosd * pro[p].sind0 * cosa * pro[p].sinrho;
               *xou = degrad( a + b - c ) / *dx;
               *you = degrad( d );
               break;
            }
            case 3: {                                       /* (x,y) -> (a,d) */
               double l, m, t, x, y, da;

               x = *xin * *dx;
               y = *yin * *dy;
               l = raddeg( x * pro[p].cosrho - y * pro[p].sinrho );
               m = raddeg( y * pro[p].cosrho + x * pro[p].sinrho );
               t = sqrt( 1 - l * l - m * m );
               da = atan( l / ( pro[p].cosd0 * t - m * pro[p].sind0 ) );
               *xou = pro[p].a0 + degrad( da );
               *you = degrad( asin( m * pro[p].cosd0 + pro[p].sind0 * t ) );
               break;
            }
            default: {                                        /* unknown mode */
               r = 2;
               break;
            }
         }
         break;
      }
      case 6: {                                     /* Rectangular projection */
         switch(*mode) {                                 /* which way to go ? */
            case 0: {                                       /* (a,d) -> (x,y) */
               double a, d, l, m, s, t, da, cosa, sina, cosd, sind;

               da = raddeg( *xin - pro[p].a0 );
               cosa = cos( da );
               sina = sin( da );
               d = raddeg( *yin );
               cosd = cos( d );
               sind = sin( d );
               s = sind * pro[p].sind0 + cosd * pro[p].cosd0 * cosa;
               if (s > 1.0) s = 1.0; else if (s < -1.0) s = -1.0;
               t = acos( s );
               if (t == 0.0) a = 1.0; else a = t / sin( t );
               l = a * cosd * sina;
               m = a * ( sind * pro[p].cosd0 - cosd * pro[p].sind0 * cosa );
               *xou = degrad( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = degrad( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 1: {                                       /* (x,d) -> (a,y) */
               double a, b, d, f, x, t, y, da, yold, cosd, sind;
               int i = 0;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               x = raddeg( *xin * *dx );
               d = raddeg( *yin );
               if (d > PI/2.0 || d < -PI/2.0) { r = 5; break; }
               cosd = cos( d );
               sind = sin( d );
               a = x * pro[p].sinrho * pro[p].cosd0;
               b = pro[p].cosrho * pro[p].cosd0;
               y = 0.0;
               do {
                  yold = y;
                  t = sqrt( x * x + y * y );
                  if (t == 0.0) f = 1.0; else f = sin( t ) / t;
                  y = ( sind - pro[p].sind0 * cos( t ) - a * f ) / b / f;
                  i++;
               } while (fabs( y - yold ) > 0.0000000001 && i < 10000 );
               t = sqrt( x * x + y * y );
               if (t == 0.0) f = 1.0; else f = sin( t ) / t;
               da = asin( f * ( x * pro[p].cosrho - y * pro[p].sinrho ) / cosd );
               *xou = pro[p].a0 + degrad( da );
               *you = degrad( y ) / *dy;
               break;
            }
            case 2: {                                       /* (a,y) -> (x,d) */
               double a, b, c, d, s, t, x, y, da, xold, cosa, sina;
               int  i = 0;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               da = raddeg( *xin - pro[p].a0 );
               if (*xin > 360.0 || *xin < 0.0) { r = 5; break; }
               cosa = cos( da );
               sina = sin( da );
               y = raddeg( *yin * *dy );
               a = atan( pro[p].sind0 / pro[p].cosd0 / cosa );
               b = sqrt( 1.0 - pro[p].cosd0 * pro[p].cosd0 * sina * sina );
               x = 0.0;
               do {
                  xold = x;
                  t = sqrt( x * x + y * y );
                  s = cos( t ) / b;
                  if (s > 1.0) s = 1.0; else if (s < -1.0) s = 1.0;
                  c = acos( s );
                  if (y < 0.0) d = a - c; else d = a + c;
                  if (t == 0.0) {
                     x = ( y * pro[p].sinrho + cos( d ) * sina ) / pro[p].cosrho;                     
                  } else {
                     x = ( y * pro[p].sinrho + cos( d ) * sina * t / sin( t ) ) / pro[p].cosrho;
                  }
                  i++;
/* VOG: At this point there could arise some problems because convergence */
/* could not always be reached. The problem has still to be fixed . */
               } while (fabs( x - xold ) > 0.0000000001 && i < 10000);
               *xou = degrad( x ) / *dx;
               *you = degrad( d );
               break;
            }
            case 3: {                                       /* (x,y) -> (a,d) */
               double a, d, l, m, t, x, y;

               x = *xin * *dx;
               y = *yin * *dy;
               l = raddeg( x * pro[p].cosrho - y * pro[p].sinrho );
               m = raddeg( y * pro[p].cosrho + x * pro[p].sinrho );
               t = sqrt( l * l + m * m );
               if (t == 0.0) a = 1.0; else a = sin( t ) / t;
               d = asin( a * m * pro[p].cosd0 + pro[p].sind0 * cos( t ) );
               *xou = pro[p].a0 + degrad( asin( a * l / cos( d ) ) );
               *you = degrad( d );
               break;
            }
            default: {                                        /* unknown mode */
               r = 2;
               break;
            }
         }
         break;
      }
      case 7: {                               /* Global sinusoidal projection */
         switch(*mode) {                                 /* which way to go ? */
            case 0: {                                       /* (a,d) -> (x,y) */
               double l, m;

               l = ( *xin - pro[p].a0 ) * cos( raddeg( * yin ) );
               m = ( *yin - pro[p].d0 );
               *xou = ( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = ( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 1: {                                       /* (x,d) -> (a,y) */
               double x, y, da, cosd;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               x = *xin * *dx;
               y = ( *yin - pro[p].d0 - x * pro[p].sinrho ) / pro[p].cosrho;
               cosd = cos( raddeg( *yin ) );
               da = ( x * pro[p].cosrho - y * pro[p].sinrho ) / cosd;
               *xou = pro[p].a0 + da;
               *you = y / *dy;
               break;
            }
            case 2: {                                       /* (a,y) -> (x,d) */
               double d, dold, t, x, y, da, d0;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               d0 = raddeg( pro[p].d0 );
               da = raddeg( *xin - pro[p].a0 );
               y = raddeg( *yin * *dy );
               t = da * pro[p].sinrho;
               d = d0 + y / pro[p].cosrho;
               do {
                  dold = d;
                  d = d0 + ( y + t * cos( dold ) ) / pro[p].cosrho;
               } while (fabs( d - dold ) > 0.0000000001 );
               x = da * cos( d ) * pro[p].cosrho + (d - d0 ) * pro[p].sinrho;
               *xou = degrad( x ) / *dx;
               *you = degrad( d );
               break;
            }
            case 3: {                                       /* (x,y) -> (a,d) */
               double d, l, m, x, y, cosd;

               x = *xin * *dx;
               y = *yin * *dy;
               l = x * pro[p].cosrho - y * pro[p].sinrho;
               m = y * pro[p].cosrho + x * pro[p].sinrho;
               d = pro[p].d0 + m;
               cosd = cos( raddeg( d ) );
               *xou = pro[p].a0 + l / cosd;
               *you = d;
               break;
            }
            default: {                                        /* unknown mode */
               r = 2;
               break;
            }
         }
         break;
      }
      case 8: {                            /* North Celestial Pole projection */
         switch(*mode) {                                 /* which way to go ? */
            case 0: {                                       /* (a,d) -> (x,y) */
               double l, m, da, cosd;

               da = raddeg( *xin - pro[p].a0 );
               cosd = cos( raddeg( *yin ) );
               l = cosd * sin( da );
               m = ( pro[p].cosd0 - cosd * cos( da ) ) / pro[p].sind0;
               *xou = degrad( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = degrad( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 1: {                                       /* (x,d) -> (a,y) */
               double a, b, c, s, t, x, da, cosd;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               x = raddeg( *xin * *dx );
               cosd = cos( raddeg( *yin ) );
               a = pro[p].cosrho;
               b = pro[p].sinrho / pro[p].sind0;
               s = x * pro[p].sind0 - pro[p].cosd0 * pro[p].sinrho;
               t = cosd * pro[p].sind0 * sqrt( a * a + b * b );
               da = atan( b / a ) + asin( s / t );
               a = pro[p].cosrho * pro[p].cosd0 / pro[p].sind0;
               b = pro[p].cosrho * cosd * cos( da ) / pro[p].sind0;
               c = pro[p].sinrho * cosd * sin( da );
               *xou = pro[p].a0 + degrad( da );
               *you = degrad( a - b - c ) / *dy;
               break;
            }
            case 2: {                                       /* (a,y) -> (x,d) */
               double a, b, c, d, q, s, t, y, da, cosa, sina, cosd;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               y = raddeg( *yin * *dy );
               da = raddeg( *xin - pro[p].a0 );
               cosa = cos( da );
               sina = sin( da );
               s = pro[p].cosd0 * pro[p].cosrho - y * pro[p].sind0;
               t = cosa * pro[p].cosrho + sina * pro[p].sinrho * pro[p].sind0;
               q = s / t;
               if (q > 1.0) q = 1.0; else if (q < -1.0) q = -1.0;
               d = acos( q );
               if (pro[p].d0 > 0.0) d = fabs( d ); else d = -fabs( d );
               cosd = cos( d );
               a = cosd * sina * pro[p].cosrho;
               b = pro[p].cosd0 * pro[p].sinrho / pro[p].sind0;
               c = cosd * cosa * pro[p].sinrho / pro[p].sind0;
               *xou = degrad( a + b - c ) / *dx;
               *you = degrad( d );
               break;
            }
            case 3: {                                       /* (x,y) -> (a,d) */
               double d, l, m, x, y, da, cosa;

               x = *xin * *dx;
               y = *yin * *dy;
               l = raddeg( x * pro[p].cosrho - y * pro[p].sinrho );
               m = raddeg( y * pro[p].cosrho + x * pro[p].sinrho );
               da = atan( l / ( pro[p].cosd0 - m * pro[p].sind0 ) );
               cosa = cos( da );
               d = acos( ( pro[p].cosd0 - m * pro[p].sind0 ) / cosa );
               if (pro[p].d0 > 0.0) d = fabs( d ); else d = - fabs( d );
               *xou = pro[p].a0 + degrad( da );
               *you = degrad( d );
               break;
            }
            default: {                                        /* unknown mode */
               r = 2;
               break;
            }
         }
         break;
      }
      case 9: {                                   /* Stereographic projection */
         switch(*mode) {                                 /* which way to go ? */
            case 0: {                                       /* (a,d) -> (x,y) */
               double l, m, d, t, da, cosa, sina, cosd, sind;

               da = raddeg( *xin - pro[p].a0 );
               cosa = cos( da );
               sina = sin( da );
               d = raddeg( *yin );
               cosd = cos( d );
               sind = sin( d );
               t = 1.0 + sind * pro[p].sind0 + cosd * pro[p].cosd0 * cosa;
               l = 2.0 * ( cosd * sina ) / t;
               m = 2.0 * ( sind * pro[p].cosd0 - cosd * pro[p].sind0 * cosa ) / t;
               *xou = degrad( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = degrad( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 1: {                                       /* (x,d) -> (a,y) */
               double a, b, c, d, s, t, x, da, cosa, sina, cosd, sind;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               x = raddeg( *xin * *dx );
               d = raddeg( *yin );
               cosd = cos( d );
               sind = sin( d );
               a = 2.0 * cosd * pro[p].cosrho;
               b = 2.0 * cosd * pro[p].sind0 * pro[p].sinrho + x * cosd * pro[p].cosd0;
               c = x + x * sind * pro[p].sind0 - 2.0 * sind * pro[p].cosd0 * pro[p].sinrho;
               da = atan( b / a ) + asin( c / sqrt( a * a + b * b ) );
               cosa = cos( da );
               sina = sin( da );
               a = sind * pro[p].cosd0 * pro[p].cosrho;
               b = cosd * pro[p].sind0 * cosa * pro[p].cosrho;
               c = cosd * sina * pro[p].sinrho;
               s = sind * pro[p].sind0;
               t = cosd * pro[p].cosd0 * cosa;
               *xou = pro[p].a0 + degrad( da );
               *you = degrad( 2.0 * ( a - b - c ) / ( 1.0 + s + t ) ) / *dy;
               break;
            }
            case 2: {                                       /* (a,y) -> (x,d) */
               double a, b, c, d, s, t, y, da, cosa, sina, cosd, sind;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               da = raddeg( *xin - pro[p].a0 );
               cosa = cos( da );
               sina = sin( da );
               y = raddeg( * yin * *dy );
               a = 2.0 * pro[p].cosd0 * pro[p].cosrho - y * pro[p].sind0;
               b = cosa * ( y * pro[p].cosd0 + 2.0 * pro[p].sind0 * pro[p].cosrho ) + 2.0 * sina * pro[p].sinrho;
               c = y;
               d = atan( b / a ) + asin( c / sqrt( a * a + b * b ) );
               cosd = cos( d );
               sind = sin( d );
               a = sind * pro[p].cosd0 * pro[p].sinrho;
               b = cosd * pro[p].sind0 * cosa * pro[p].sinrho;
               c = cosd * sina * pro[p].cosrho;
               s = sind * pro[p].sind0;
               t = cosd * pro[p].cosd0 * cosa;
               *xou = degrad( 2.0 * ( a - b + c ) / ( 1.0 + s + t ) ) / *dx;
               *you = degrad( d );
               break;
            }
            case 3: {                                       /* (x,y) -> (a,d) */
               double d, l, m, x, y, da, cosd, cost;

               x = *xin * *dx;
               y = *yin * *dy;
               l = raddeg( x * pro[p].cosrho - y * pro[p].sinrho );
               m = raddeg( y * pro[p].cosrho + x * pro[p].sinrho );
               cost = ( 4.0 - l * l - m * m ) / ( 4.0 + l * l + m * m );
               d = asin( cost * pro[p].sind0 + 0.5 * m * pro[p].cosd0 * ( 1.0 + cost ) );
               cosd = cos( d );
               da = asin( 0.5 * l * ( 1.0 + cost ) / cosd );
               *xou = pro[p].a0 + degrad( da );
               *you = degrad( d );
               break;
            }
            default: {                                        /* unknown mode */
               r = 2;
               break;
            }
         }
         break;
      }
      case 10: {                                       /* Mercator projection */
         if ((*dx != mer.dx) || (*dy != mer.dy) || (*a0 != mer.a0) || (*d0 != mer.d0) || (*rho != mer.rho)) {
            double d, t1, t2, ddd;

            mer.dx = *dx;
            mer.dy = *dy;
            mer.a0 = *a0;
            mer.d0 = *d0;
            mer.rho = *rho;
            ddd = raddeg( *dy * pro[p].cosrho + *dy * pro[p].sinrho );
            d = raddeg( pro[p].d0 );
            t1 = log( tan( ( d + ddd ) / 2.0 + PI / 4.0 ) );
            t2 = log( tan( d / 2.0 + PI / 4.0 ) );
            mer.fa = pro[p].cosd0;
            mer.fd = ddd / ( t1 - t2 );
            mer.m0 = mer.fd * t2;
         }
         switch(*mode) {                                 /* which way to go ? */
            case 0: {                                       /* (a,d) -> (x,y) */
               double d, l, m, da;

               da = raddeg( *xin - pro[p].a0 );
               d = raddeg( *yin );
               l = mer.fa * da;
               m = mer.fd * log( tan( d / 2.0 + PI / 4.0 ) ) - mer.m0;
               *xou = degrad( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = degrad( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 1: {                                       /* (x,d) -> (a,y) */
               double d, m, l, x;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               x = raddeg( *xin * *dx );
               d = raddeg( *yin );
               m = mer.fd * log( tan( d / 2.0 + PI / 4.0 ) ) - mer.m0;
               l = ( x - m * pro[p].sinrho ) / pro[p].cosrho;
               *xou = pro[p].a0 + degrad( l / mer.fa );
               *you = degrad( m * pro[p].cosrho - l * pro[p].sinrho ) / *dy;
               break;
            }
            case 2: {                                       /* (a,y) -> (x,d) */
               double l, m, y;

               if (pro[p].cosrho == 0.0) { r = 3; break; }            /* exit */
               y = raddeg( *yin * *dy );
               l = mer.fa * raddeg( *xin - pro[p].a0 );
               m = ( y + l * pro[p].sinrho ) / pro[p].cosrho;
               *xou = degrad( l * pro[p].cosrho + m * pro[p].sinrho ) / *dx;
               *you = degrad( 2 * atan( exp( ( m + mer.m0 ) / mer.fd ) ) - PI / 2.0 );
               break;
            }
            case 3: {                                       /* (x,y) -> (a,d) */
               double l, m, x, y;

               x = *xin * *dx;
               y = *yin * *dy;
               l = raddeg( x * pro[p].cosrho - y * pro[p].sinrho );
               m = raddeg( y * pro[p].cosrho + x * pro[p].sinrho );
               *xou = pro[p].a0 + degrad( l / mer.fa );
               *you = degrad( 2.0 * atan( exp( ( m + mer.m0 ) / mer.fd ) ) - PI / 2.0 );
               break;
            }
            default: {                                        /* unknown mode */
               r = 2;
               break;
            }
         }
         break;
      }
      default: {                                        /* Unknown projection */
         r = 1;
         break;
      }
   }
   return( r );
}

#if defined(TESTBED)
main()
{
   double rho = 45.0;
   double a0  = 45.0;
   double d0  = 45.0;
   double a;
   double d;
   double x;
   double y;
   double dx = -0.01;
   double dy = 0.02;
   fint   mode;
   fint   p;
   fint   pro;
   fint   r;


   for (p = 0; p < MAXPRO; p++) {
      double as, ds, xs, ys;
      pro = p + 1;
      printf( "Projection # %ld\n", pro );
      mode = 0; a = 44.5; d = 45.5;
      r = proco_c( &a, &d, &x, &y, &a0, &d0, &dx, &dy, &rho, &pro, &mode );
      as = a; ds = d; xs = x; ys = y;
      printf( "proco = %ld, a =%12.8f, d =%12.8f, x =%12.8f, y =%12.8f\n", r, a, d, x, y );
      mode = 1; x = xs; d = ds;
      r = proco_c( &x, &d, &a, &y, &a0, &d0, &dx, &dy, &rho, &pro, &mode );
      printf( "proco = %ld, a =%12.8f, d =%12.8f, x =%12.8f, y =%12.8f\n", r, a, d, x, y );
      mode = 2; a = as; y = ys;
      r = proco_c( &a, &y, &x, &d, &a0, &d0, &dx, &dy, &rho, &pro, &mode );
      printf( "proco = %ld, a =%12.8f, d =%12.8f, x =%12.8f, y =%12.8f\n", r, a, d, x, y );
      mode = 3; x = xs; y = ys;
      r = proco_c( &x, &y, &a, &d, &a0, &d0, &dx, &dy, &rho, &pro, &mode );
      printf( "proco = %ld, a =%12.8f, d =%12.8f, x =%12.8f, y =%12.8f\n", r, a, d, x, y );
   }
}
#endif
