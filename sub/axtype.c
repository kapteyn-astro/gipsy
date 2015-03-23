/* axtype.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            axtype.dc2

Function:     AXTYPE

Purpose:      The function axtype returns the type of axis, the natural
              units, secondary units and the projection type.

Category:     PHYSICAL COORDINATES

File:         axtype.c

Author:       K.G. Begeman

Use:          INTEGER AXTYPE( CTYPE,      Input      CHARACTER*(*)
                              CUNIT,      Output     CHARACTER*(*)
                              DUNIT,      Output     CHARACTER*(*)
                              SKYSYS,     Output     INTEGER
                              PROSYS,     Output     INTEGER
                              VELSYS )    Output     INTEGER

              AXTYPE  Returns:
                       0: unknown type of axis.
                       1: spatial axis longitude.
                       2: spatial axis latitude.
                       3: spectral axis frequency.
                       4: spectral axis velocity.
                       5: spectral axis wavelength.
                       6: spectral axis inverse wavelength.
                       7: spectral axis log(wavelength).
                       8: time axis.
                       9: polarisation axis.
                      10: parameter axis.
                      11: sample axis of iras data
                      12: tick axis of iras data
                      13: detector axis of iras data
                      14: snip axis of iras data
              CUNIT   Natural axis units.
              DUNIT   Secondary axis units.
              SKYSYS  If AXTYPE equals 1 or 2, the sky system id:
                      1 = equatorial
                      2 = galactic
                      3 = ecliptic
                      4 = supergalactic
              PROSYS  if AXTYPE equals 1 or 2, the sky projection
                      system id:
                      1 = AITOFF equal area
                      2 = equivalent cylindrical
                      3 = flat
                      4 = gnomonic
                      5 = orthographic
                      6 = rectangular
                      7 = global sinusoidal
                      8 = north celestial pole (WSRT)
                      9 = stereographic
                      10 = mercator projection
              VELSYS  If AXTYPE equals 3 the velocity system id:
                      1 = optical
                      2 = radio
                      If AXTYPE equals 4 VELSYS = 2.

Updates:      Dec 11, 1989 : KGB, document created.
              Aug 28, 1991 : PRR, add IRDS axis types (SAMPLE etc.)
              Sep 24, 1999 : VOG, allow unknown axis names to be of 
                                  parameter type.

#<

Fortran to C interface:

@ integer function axtype( character ,
@                          character ,
@                          character ,
@                          integer   ,
@                          integer   ,
@                          integer   )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"ctype.h"		/* <ctype.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */

#define	MAXBUFLEN	80		/* maximum length of axis name */

typedef struct { char *ctype; char *cunit; char *dunit; } c_struct;

static c_struct NAMES[] = {
   { "RA"    , "DEGREE" , ""     },     /* Equatorial, right ascension */
   { "DEC"   , "DEGREE" , ""     },     /* Equatorial, declination */
   { "GLON"  , "DEGREE" , ""     },     /* Galactic longitude */
   { "GLAT"  , "DEGREE" , ""     },     /* Galactic latitude */
   { "ELON"  , "DEGREE" , ""     },     /* Ecliptic longitude */
   { "ELAT"  , "DEGREE" , ""     },     /* Ecliptic latitude */
   { "SLON"  , "DEGREE" , ""     },     /* Supergalactic longitude */
   { "SLAT"  , "DEGREE" , ""     },     /* Supergalactic latitude */
   { "FREQ"  , "HZ"     , "M/S"  },     /* Frequency */
   { "VELO"  , "M/S"    , ""     },     /* Velocity axis */
   { "LAMBDA", "METER"  , ""     },     /* Wavelength */
   { "INVLAM", "1/METER", ""     },     /* Inverse Wavelength */
   { "LOGLAM", "LOG(M)" , ""     },     /* log(wavelength) */
   { "TIME"  , "SECOND" , ""     },     /* Time */
   { "POLN"  , ""       , ""     },     /* polarisation */
   { "PARAM" , "PAR"    , ""     },     /* Parameter axis */
   { "SAMPLE", "TICK"   , ""     },     /* IRDS sample axis */
   { "TICK"  , "TICK"   , ""     },     /* IRDS tick axis */
   { "SDET"  , ""       , ""     },     /* IRDS detector axis */
   { "SNIP"  , ""       , ""     }      /* IRDS snip axis */
};

#define	MAXNAMES	(sizeof(NAMES)/sizeof(c_struct))	/* CTYPEs */

static char *PROSYS[] = {               /* known projections */
  "AIT",                                /* aitoff equal area projection */
  "CYL",                                /* equivalent cylindrical projection */
  "FLT",                                /* flat projection */
  "TAN",                                /* gnomonic projection */
  "SIN",                                /* orthographic projection */
  "ARC",                                /* rectangular projection */
  "GLS",                                /* transversal projection */
  "NCP",                                /* WSRT projection */
  "STG",                                /* stereographic projection */
  "MER"                                 /* Mercator projection */
};

#define	MAXPROSYS	(sizeof(PROSYS)/sizeof(char *))	/* projections */

static char *VELSYS[] = {               /* known velocity systems */
   "OLSR",                              /* optical definition, w.r.t. lsr */
   "RLSR",                              /* radio definition, w.r.t. lsr */
   "OHEL",                              /* optical definition, heliocentric */
   "RHEL"                               /* radio definition, heliocentric */
};

#define	MAXVELSYS	(sizeof(VELSYS)/sizeof(char *))	/* vel. systems */

fint axtype_c( fchar  ctype  ,
               fchar  cunit  ,
               fchar  dunit  ,
               fint  *skysys ,
               fint  *prosys ,
               fint  *velsys )
{
   fint   d;
   fint   r = 0;
   fint   k, m, n;
   fint   lc = 0;
   fint   lp = 0;
   char   b[MAXBUFLEN+1];
   char  *a;
   char  *c;
   char  *p;

   *skysys = *prosys = *velsys = 0;     	/* reset */
   for (k = 0; k < cunit.l; cunit.a[k++] = ' ');
   for (k = 0; k < dunit.l; dunit.a[k++] = ' ');
   m = 0;
   while ((m < MAXBUFLEN) && (m < ctype.l)) {
      b[m] = toupper( ctype.a[m] );
      m++;
   }
   b[m] = 0;                                    /* add zero byte */
   a = c = b;                                   /* start of CTYPE */
   while ((*a != ' ') && (*a != '-') && (*a)) a++;	/* 1st part of axis name */
   lc = (fint) (a - c);                         /* length of CTYPE */
   if (!lc) return( r );                        /* empty ctype */
   while (*a == '-') a++;                       /* skip over separators */
   p = a;                                       /* address of projection */
   while ((*a != ' ') && (*a != '-') && (*a)) a++;	/* 2nd part of axis name */
   lp = (fint) (a - p);                         /* length of projection */
   n = 0;                                       /* reset */
   do {                                		/* compare CTYPE with the ones we know */
      d = strncmp( c, NAMES[n++].ctype, lc );
   } while (d && (n < MAXNAMES));
   /*---------------------------------------*/
   /* Allow unknown axis names to be of     */
   /* parameter type.                       */
   /*---------------------------------------*/
   if (d) 
      n = 16;                                   /* unknown type of axis == par */
   k = strlen( NAMES[n-1].cunit );              /* copy natural units */
   if (k > cunit.l ) k = cunit.l;
   for (m = 0; m < k; m++) cunit.a[m] = NAMES[n-1].cunit[m];
   k = strlen( NAMES[n-1].dunit );              /* copy secondary units */
   if (k > dunit.l ) k = dunit.l;
   for (m = 0; m < k; m++) dunit.a[m] = NAMES[n-1].dunit[m];
   switch(n) {                                  /* find encoded axis type */
      case 1: r = 1; *skysys = 1; break;        /* these are all spatial axes */
      case 2: r = 2; *skysys = 1; break;
      case 3: r = 1; *skysys = 2; break;
      case 4: r = 2; *skysys = 2; break;
      case 5: r = 1; *skysys = 3; break;
      case 6: r = 2; *skysys = 3; break;
      case 7: r = 1; *skysys = 4; break;
      case 8: r = 2; *skysys = 4; break;
      default: r = n - 6; break;                /* other axis */
   }
   switch(r) {                                  /* find encode projection type */
      case 1:                                   /* spatial (sky) axis */
      case 2: {
         if (lp) {
            n = 0;
            do {                /* compare projections with the ones we know */
               d = strncmp( p, PROSYS[n++], lp );
            } while (d && (n < MAXPROSYS));
            if (d) {            		/* we don't know the projection */
               *prosys = 4;                     /* assume gnomonic projection */
            } else {                            /* we do know the projection */
               *prosys = n;
            }
         } else {                               /* default projection is gnomonic */
            *prosys = 4;
         }
         break;
      }
      case 3: {                                 /* Frequency axis */
         if (lp) {                              /* there is a velocity system tag */
            n = 0;
            do {                                /* compare with velocity systems we know */
               d = strncmp( p, VELSYS[n++], lp );
            } while (d && (n < MAXVELSYS));
            if (!d) switch(n) {
               case 1: *velsys = 1; break;
               case 2: *velsys = 2; break;
               case 3: *velsys = 1; break;
               case 4: *velsys = 2; break;
               default: break;
            }
         }
         break;
      }
      case 4: {                                 /* Velocity axis */
         *velsys = 2;
         break;
      }
      default: {
         break;
      }
   }
   return(r);
}

#if defined(TESTBED)
void main( int argc, char *argv[] )
{
   fchar  ctype;
   fchar  cunit;
   fchar  dunit;
   char   t[MAXBUFLEN+1];
   char   c[MAXBUFLEN+1];
   char   b[MAXBUFLEN+1];
   fint   r;
   fint   n;
   fint   skysys, prosys, velsys;

   printf("MAXNAMES: %d\n",MAXNAMES);
   for (n = 1; n < argc; n++) {
      strncpy( t, argv[n], MAXBUFLEN ); t[MAXBUFLEN] = 0;
      ctype.a = t; ctype.l = strlen( t );
      cunit.a = c; cunit.l = MAXBUFLEN; c[MAXBUFLEN] = 0;
      dunit.a = b; dunit.l = MAXBUFLEN; b[MAXBUFLEN] = 0;
      r = axtype_c( ctype, cunit, dunit, &skysys, &prosys, &velsys );
      printf( "axtype = %ld\n", r );
      printf( "cunit  = %.10s\n", cunit.a );
      printf( "dunit  = %.10s\n", dunit.a );
      printf( "skysys = %ld\n", skysys );
      printf( "prosys = %ld\n", prosys );
      printf( "velsys = %ld\n", velsys );
   }
}
#endif
