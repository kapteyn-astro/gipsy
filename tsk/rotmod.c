/* rotmod.c

	Copyright (c) Kapteyn Laboratorium Groningen 1992
	All Rights Reserved.

#>            rotmod.dc1

Program:      ROTMOD

Purpose:      This program calculates the rotation curve for a truncated
              exponential disk (Casertano, M.N.R.A.S., vol. 203,
              p735-p747, 1983), or for any other user supplied density
              law. It can also calculate the rotation curve for a
              spherical bulge.

Category:     DYNAMICS, MODELS, ROTATION CURVES

File:         rotmod.c

Author:       K.G. Begeman

Keywords:

   TYPE=      Please choose type of potential, [DISK] or spherical BULGE.

   ZLAW=      DISK Density distribution in z [NONE].
              There are three distribution laws possible; the Van der
              Kruit and Searle law (SECH-SQUARED): D(Z)=sech(Z/Z0)**2/Z0,
              an exponential density law (EXPONENTIAL):
              D(Z)=exp(-Z/Z0)/Z0 and a simple sech law (SIMPLE-SECH):
              D(Z)=2*sech(Z/Z0)/Z0/PI. If either of those three distributions
              is wanted, the next keyword will be asked.

   Z0=        Scale height of DISK in kpc [0.0].
              Note: The integration in Z is the most time consuming part
              of the calculation. For example it takes about 0.2 seconds
              to calculate a point on the rotation curve for a disk with zero
              thickness, and about 2 seconds for a disk with non-zero
              thickness.

** ACCURACY=  DISK Accuracy parameter [1].
              Can be in the range 0-10. The programme needs more time
              for higher accuracies. If no ZLAW is specified, the accuracy
              parameter is always 10.

   USER=      DISK radial surface density distribution supplied by the user [Y],
              or a truncated exponential surface density distribution.
              In case the user wants to supply his own density distribution,
              the following two keywords will be asked:

   UNITS=     Enter units of radius and surface density [KPC,MSUN/PC**2].
              It is possible to enter ARCSEC or ARCMIN for radius units,
              but then the keyword DISTANCE= will appear. It is also
              possible to enter MAG/ARCSEC**2 as surface density units.
              Then also the keyword DISTANCE= will appear and magnitudes
              will be converted to intensities and it is assumed that
              M/L = 1. Note that in the conversion from MAGnitudes to MSUN
              no corrections are made (i.e. M = 10**(-0.4*MAG)).

** PAIRS=     Enter radii and surface densities in pairs [Y]/N?
              If Y, the keyword RADDENS= will be prompted, if N the keywords
              RADIUS= and DENSITY=.

   RADDENS=   Enter radius and surface density in UNITS (if PAIRS=Y).
              Up to 20000 radius-density pairs can be handled.
              Note:  this keyword is  repeated  until  carriage
              return is given.  So you can use recall files!

   RADIUS=    Enter radii (if PAIRS=N). Up to 20000 radii can be handled.
              Note:  this keyword is repeated until carriage return
              is given.

   DENSITY=   Enter surface densities (if PAIRS=N). Up to 20000 densities
              can be handled.
              Note:  this keyword is repeated until carriage return
              is given.

   DENS0=     Central density of exponential DISK in solar
              masses per square parsec. Only asked if USER=N.

   H=         Scale length of DISK in kpc. Only asked if USER=N.

   RCUT=      Cutoff radius for DISK in kpc [no cutoff]. Only asked if USER=N.

   DELTA=     Softening parameter of DISK in kpc [0.0]. It may be wise
              to have a non-zero softening in case of a
              truncated disk, because then the sharp features
              in the calculated rotation curve will be smoothed
              out. A softening parameter of about 0.2 scale
              lengths would be sufficient. Only asked if USER=N.

   MASS=      Wanted disk mass (in 10^9 solar masses) [mass
              calculated from from surface densities].

   RADII=     Sampling radii (in kpc) for which the rotation
              velocities will be calculated (up to 20000 radii
              can be given).

** EXTRA=     Calculate DISK rotation velocities also for some interesting
              radii, i.e. the sampling radii for the density distribution
              or the radii near a truncation Y/[N].

   FILE=      Name of file in which the calculated rotation
              curve is saved [rotmod.dat]. This file can be the
              input to the program ROTMAS.

Notes:        -  The accuracy of the calculation is about 1 in 1000 for a
                 thick disk with default accuracy.
              -  The sign of the calculated rotation velocity is the sign
                 of rotation velocity squared. So if this sign is negative,
                 this means that there is a net force away from the centre
                 of the galaxy.
              -  For the  sampling radii you should use an increment of
                 about 0.2 times the disk scale length, and the maximum
                 sampling radius should at least be larger than the radius
                 of the last measured point on your observed rotation curve.
              -  If the user wants to enter a measured density profile, and
                 the central density is not given (density at R=0), the
                 program will use linear extrapolation to obtain the central
                 density.

Updates:      Sep 24, 1984: KGB document created.
              Mar 19, 1985: KGB revision of document and program.
              Jun 11, 1985: KGB minor change in program.
              Jan 14, 1986: KGB minor change, interpolate to zero.
              Jun 14, 1986: KGB migrated to VAX-VMS.
              Mar 22, 1988: AXL document somewhat standardized.
              Jul 22, 1988: KGB bug in estimating scalelength repaired.
              Sep 10, 1988: KGB USERCHAR implemented.
              Aug 13, 1991: AHB more use of repinp.
              Nov 12, 1992: KGB C source.
              Feb  2, 1995: KGB Keyword EXTRA= implemented.
              May 11, 1995: KGB Spherical bulge implemented.
              Jul 16, 1996: KGB Bug in selecting radii fixed.
              Dec  9, 1996: KGB Simple-Sech z-density law implemented.
              Jul 22, 1998: VOG Anyout level changed from 3 to 0
              Mar 02, 2001: VOG Changed max number of radii (20000)

#<

*/


/*
 * Include files:
 */

#include	"math.h"		/* <math.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY definitions */
#include	"cmain.h"		/* main programme in C */
#include	"anyout.h"		/* anyout_c */
#include	"cancel.h"		/* cancel_c */
#include	"error.h"		/* error_c */
#include	"factor.h"		/* factor_c */
#include	"finis.h"		/* finis_c */
#include	"init.h"		/* init_c */
#include 	"match.h"		/* match_c */
#include	"moved.h"		/* moved_c */
#include	"nelc.h"		/* nelc_c */
#include	"rankda.h"		/* rankda_c */
#include	"reject.h"		/* reject_c */
#include	"sortda.h"		/* sortda_c */
#include	"units.h"		/* units_c */
#include	"usercharu.h"		/* usercharu_c */
#include	"userdble.h"		/* userdble_c */
#include	"userint.h"		/* userint_c */
#include	"userlog.h"		/* userlog_c */
#include	"userreal.h"		/* userreal_c */
#include	"usertext.h"		/* usertext_c */


/*
 * Defines:
 */

#define	CONSTANT	(2.0 * PI * G / 3.0)
#define	EPS		(0.000001)
#define	ERR_DELTA	tofchar("Wrong softening parameter!")
#define	ERR_DENSITY	tofchar("Weird surface density!")
#define	ERR_DENS0	tofchar("Wrong central surface density!")
#define	ERR_DISTANCE	tofchar("Wrong distance!")
#define	ERR_FILE	tofchar("Cannot create file!")
#define	ERR_H		tofchar("Wrong scale length of disk!")
#define	ERR_MASS	tofchar("Wrong mass of disk!")
#define	ERR_RADDENS	tofchar("Wrong radius/density pair!")
#define	ERR_RADII	tofchar("Wrong radii (<0.0)!")
#define	ERR_RADIUS	tofchar("Weird radius!")
#define	ERR_RCUT	tofchar("Wrong cutoff radius!")
#define	ERR_TYPE	tofchar("Unknown type of potential!")
#define	ERR_UNITS	tofchar("Wrong units!")
#define	ERR_ZLAW	tofchar("Unknown density law!")
#define	ERR_Z0		tofchar("Illegal value for Z0!")
#define G		(0.00000431158)		/* g in (km/s)^2 kpc  Msun^-1) */
#define	KEY_ACCURACY	tofchar("ACCURACY=")
#define	KEY_DELTA	tofchar("DELTA=")
#define	KEY_DENSITY	tofchar("DENSITY=")
#define	KEY_DENS0	tofchar("DENS0=")
#define	KEY_DISTANCE	tofchar("DISTANCE=")
#define	KEY_EXTRA	tofchar("EXTRA=")
#define	KEY_FILE	tofchar("FILE=")
#define	KEY_H		tofchar("H=")
#define	KEY_MASS	tofchar("MASS=")
#define	KEY_PAIRS	tofchar("PAIRS=")
#define	KEY_RADDENS	tofchar("RADDENS=")
#define	KEY_RADII	tofchar("RADII=")
#define	KEY_RADIUS	tofchar("RADIUS=")
#define	KEY_RCUT	tofchar("RCUT=")
#define	KEY_TYPE	tofchar("TYPE=")
#define	KEY_UNITS	tofchar("UNITS=")
#define	KEY_USER	tofchar("USER=")
#define	KEY_ZLAW	tofchar("ZLAW=")
#define	KEY_Z0		tofchar("Z0=")
#define	LEN1		(20016)
#define	LEN2		(40001)
#define	MESLEN		(80)		/* length of messages */
#define	MES_ACCURACY	tofchar("Accuracy of calculation [1]")
#define	MES_DELTA	tofchar("Softening parameter in kpc [0.0]")
#define	MES_DENSITY	tofchar("Enter surface density")
#define	MES_DENS0	tofchar("Central surface density in MSUN/PC**2")
#define	MES_DISTANCE	tofchar("Distance in MPC")
#define	MES_EXTRA	tofchar("Add some interesting sampling radii ? Y/[N]")
#define	MES_FILE	tofchar("Name of file for results [rotmod.dat]")
#define	MES_H		tofchar("Scale length of disk in kpc")
#define	MES_PAIRS	tofchar("Enter radii and densities in pairs ? [Y]")
#define	MES_RADDENS	tofchar("Enter radius and surface density")
#define	MES_RADII	tofchar("Enter sampling radii in kpc")
#define	MES_RADIUS	tofchar("Enter radii")
#define	MES_RCUT	tofchar("Cutoff radius of disk in kpc [no cutoff]")
#define	MES_TYPE	tofchar("Please choose type of potential, [DISK] or BULGE")
#define	MES_UNITS	tofchar("Enter units [KPC,MSUN/PC**2]")
#define	MES_USER	tofchar("User supplied surface densities [Y]")
#define	MES_ZLAW	tofchar("Density law in Z [NONE]")
#define	MES_Z0		tofchar("Scale height of disk in kpc")
#define	PI		(3.1415926536)	/* pi */
#define	TXTLEN		(20)		/* length of text strings */
#define	TWOPI		(2*PI)		/* guess what */
#define	VERSION		"1.1"		/* version number */

#define	fcopy( f, i, t )	\
{\
   int	k, o;\
   o = f.l * i;\
   for (k=0;k<f.l&&t[k];k++) f.a[o+k]=t[k];\
   while (k<f.l) f.a[o+k++]=' ';\
}

static	double	xdinp[LEN2];		/* radii surface density */
static	double	ydinp[LEN2];		/* the surface densities */
static	fint	ndinp;			/* number of surface densities */

static	double	radius[LEN1+LEN2+1];	/* sampling radii */
static	double	densit[LEN1+LEN2+1];	/* surface densities */
static	double	velocs[LEN1+LEN2+1];	/* the calculated velocities */
static	fint	nradii;			/* number of sampling radii */


/*
 * sortradii sorts radii in ascending order.
 */

static	void	sortradii( double r[], fint *nr )
{
   fint	n, mr = 0;

   sortda_c( r, nr );				/* sort radii */
   for ( n = 1; n < (*nr); n++ ) {
      if ( r[n] != r[mr] ) {
         mr += 1;
         if ( ( n - mr ) ) r[mr] = r[n];
      }
   }
   (*nr) = ++mr;
}


/*
 * distance gets distance in Mpc from user.
 */

static	double	getfactor( fchar units )
{
   double	r;				/* return value */
   fint		error_level = 4;		/* fatal error */
   fint		input_level = 0;		/* no default */
   fint		nitems = 1;			/* number of items */

   switch( units_c( units ) ) {			/* type of units */
      case 1: {					/* angle units */
         double	d;				/* distance */
         double	f;				/* factor from unit -> " */

         (void) userdble_c( &d, &nitems, &input_level, KEY_DISTANCE,
            MES_DISTANCE );
         if (d <= 0.0) {			/* check input */
            error_c( &error_level, ERR_DISTANCE );	/* error message */
         }
         (void) factor_c( units, tofchar( "ARCSEC" ), &f );
         r = 4.8481E-3 * d * f;			/* the factor */
         break;
      }
      case 2: {					/* length units */
         (void) factor_c( units, tofchar( "KPC" ), &r );
         break;
      }
      default: {				/* weird units */
         error_c( &error_level, ERR_UNITS );
         break;
      }
   }
   return( r );
}


/*
 * Get radial surface density profile from the user.
 */

static	void	getrandd( void )
{
   bool		mag;				/* densities in magnitudes */
   bool		pairs;				/* r & d pairs ? */
   char		*p;				/* points to length part */
   char		unitsb[2*TXTLEN+1];		/* buffer for units */
   double	fact[2];			/* conversion factors */
   fchar	unit2;				/* points to second unit */
   fchar	units;				/* the units */
   fint		error_level = 4;		/* fatal error */
   fint		input_level = 1;		/* default allowed */
   fint		n;				/* counter */
   fint		nitems = 2;			/* number of items */

   units.a = unitsb; units.l = TXTLEN;		/* initialize */
   unitsb[2*TXTLEN] = 0;			/* end with zero byte */
   unit2.a = &unitsb[TXTLEN]; unit2.l = TXTLEN;
   fcopy( units, 0, "KPC" );
   fcopy( units, 1, "MSUN/PC**2" );
   (void) usercharu_c( units, &nitems, &input_level, KEY_UNITS, MES_UNITS );
   fact[0] = getfactor( units );		/* get factor */
   n = nelc_c( unit2 );				/* length of string */
   unit2.a[n] = 0; unit2.l = n;			/* shorten it */
   if (strcmp( &unit2.a[n-3], "**2" )) {	/* check input */
      error_c( &error_level, ERR_UNITS );	/* error message */
   }
   unit2.a[n-3] = 0; unit2.l -= 3;		/* shorten it once more */
   p = strchr( unit2.a, '/' );			/* find / */
   if (p == NULL) {				/* check input */
      error_c( &error_level, ERR_UNITS );	/* error message */
   }
   *p++ = 0;					/* put zero byte in place */
   if (!strcmp( unit2.a, "MAG" )) {
      mag = 1;
   } else {
      mag = 0;
   }
   fact[1] = getfactor( tofchar( p ) );		/* get factor */
   fact[1] = fact[1] * fact[1];
   pairs = toflog( 1 );
   nitems = 1;
   input_level = 2;
   (void) userlog_c( &pairs, &nitems, &input_level, KEY_PAIRS, MES_PAIRS );
   if ( tobool( pairs ) ) {
      ndinp = 0;				/* reset */
      do {
         double	rs[2];				/* input */

         nitems = 2;				/* number of items */
         if (!ndinp) {
            input_level = 4;			/* no default, exact */
         } else {
            input_level = 5;			/* default, exact */
         }
         if (userdble_c( rs, &nitems, &input_level, KEY_RADDENS, MES_RADDENS )) {
            if (mag) {				/* magnitudes */
               rs[1] = pow( 10.0, -0.4 * rs[1] );
            }
            if ( rs[0] < 0.0 || rs[1] < 0.0 ) {
               error_c( &error_level, ERR_RADDENS );
            }
            xdinp[ndinp] = rs[0] * fact[0];	/* scale */
            ydinp[ndinp] = rs[1] / fact[1];	/* scale */
            ndinp += 1;				/* one more */
            cancel_c( KEY_RADDENS );		/* cancel keyword */
         } else {
            break;
         }
      } while (ndinp < ( LEN2 - 1 ));		/* stop here */
   } else {
      fint	nd;

      ndinp = 0;				/* reset */
      do {
         fint	n, nel;

         if (ndinp) input_level = 1; else input_level = 0;
         nitems  = ( LEN2 - 1 ) - ndinp;
         nel = userdble_c( &xdinp[ndinp], &nitems, &input_level, KEY_RADIUS, MES_RADIUS );
         if ( nel ) {
            fint	okay = 1;
            for ( n = 0; n < nel && okay; n++ ) {
               if ( xdinp[ndinp+n] < 0.0 ) {
                  reject_c( KEY_RADIUS, ERR_RADIUS );
                  okay = 0;
               } else {
                  xdinp[ndinp+n] *= fact[0];
               }
            }
            if (okay) {
               ndinp += nel;
               cancel_c( KEY_RADIUS );
            }
         } else {
            break;
         }
      } while (ndinp < ( LEN2 - 1 ));
      nd = 0;
      input_level = 0;
      do {
         fint	n, nel;

         nitems  = ndinp - nd;
         nel = userdble_c( &ydinp[nd], &nitems, &input_level, KEY_DENSITY, MES_DENSITY );
         if ( nel ) {
            fint	okay = 1;

            for ( n = 0; n < nel && okay; n++ ) {
               if (mag) {
                  ydinp[nd+n] = pow( 10.0, -0.4 *  ydinp[nd+n] );
               }
               if ( ydinp[nd+n] < 0.0 ) {
                  reject_c( KEY_DENSITY, ERR_DENSITY );
                  okay = 0;
               } else {
                  ydinp[nd+n] /= fact[1];
               }
            }
            if (okay) {
               nd += nel;
               cancel_c( KEY_DENSITY );
            }
         } else {
            break;
         }
      } while (nd < ndinp);
   }
   /*
    * Now sort the radii and surface densities
    */
   {
      fint	indx[LEN2];			/* index */
      double	temp[LEN2];			/* temp */

      rankda_c( xdinp, indx, &ndinp );		/* sort index */
      moved_c( xdinp, temp, &ndinp );
      for ( n = 0; n < ndinp; n++ ) {
         xdinp[n] = temp[indx[n]];
      }
      moved_c( ydinp, temp, &ndinp );
      for ( n = 0; n < ndinp; n++ ) {
         ydinp[n] = temp[indx[n]];
      }
      while ( ydinp[ndinp-1] == 0.0 ) ndinp--;
      if ( xdinp[0] != 0.0 ) {
         for ( n = ndinp; n > 0; n-- ) {
            xdinp[n] = xdinp[n-1];
            ydinp[n] = ydinp[n-1];
         }
         ndinp += 1;
         xdinp[0] = 0.0;
         if ( ydinp[1] == 0.0 ) {
            ydinp[0] = 0.0;
         } else {
            ydinp[0] = ydinp[1] - ( ydinp[2] - ydinp[1] ) /
               ( xdinp[2] - xdinp[1] ) * xdinp[1];
            if ( ydinp[0] < 0.0 ) ydinp[0] = 0.0;
         }
      }
   }
}


/*
 * Get name of file where we want to store the results
 */

static	FILE	*getfile( void )
{
   FILE		*r;
   char		nameb[MESLEN+1];		/* buffer for name of file */
   fchar	name;				/* file name */
   fint		error_level = 4;		/* fatal error */
   fint		input_level = 1;		/* default allowed */

   name.a = nameb; name.l = MESLEN;		/* initialize */
   fcopy( name, 0, "rotmod.dat" );
   (void) usertext_c( name, &input_level, KEY_FILE, MES_FILE );
   nameb[nelc_c( name )] = 0;
   r = fopen( nameb, "w" );			/* open file */
   if ( r == NULL ) {
      error_c( &error_level, ERR_FILE );
   }
   return( r );
}


/******************************************************************************


                                   THE DISK


 *****************************************************************************/

/*
 * func does weird things with elliptical functions.
 */

static	double	func( double x, double y, double z )
{
   double	r = 0.0;
   double	xxx, ppp, fm, el1, el2;

   if ( x == y && z == 0.0 ) {
   } else if ( x == 0.0 || y == 0.0 ) {
   } else {
      xxx = ( x * x + y * y + z * z ) / ( 2.0 * x * y );
      if ( xxx > 1.0 ) {
         double	rrr;

         rrr = ( xxx * xxx - 1.0 );
         ppp = 1.0 / ( xxx + sqrt( rrr ) );
         fm = 1.0 - ppp * ppp;
         el1 = ( 1.3862944 + fm * ( 0.1119723 + fm * 0.0725296 ) ) -
            ( 0.5 + fm * ( 0.1213478 + fm * 0.0288729 ) ) * log( fm );
         el2 = ( 1.0 + fm * ( 0.4630151 + fm * 0.1077812 ) ) -
            ( fm * ( 0.2452727 + fm * 0.0412496 ) ) * log( fm );
         r = ( ( 1.0 - xxx * y / x ) * el2 / ( rrr ) +
            ( y / x - ppp ) * el1 / sqrt( rrr ) ) / PI;
         r *= sqrt( x / ( y * ppp ) );
      }
   }
   return( r );
}


/*
 * denzed contains the Z-profile of the disk. It can be freely changed
 * by the user, except that the function has to be normalized; that is,
 * the condition
 *
 *    INFINITY
 *    /
 *   |
 *   |      DENZED( Z, Z0 ) DZ = 1
 *   |
 *   /
 *  0
 *
 * has to be satisfied.
 */

static	double	denzed( double z, double z0, fint mode )
{
   double	r = 0;

   switch( mode ) {
      case 2: {
         r = cosh( z / z0 );
         r = r * r * z0;
         r = 1.0 / r;
         break;
      }
      case 3: {
         r = exp( -z / z0 ) / z0;
         break;
      }
      case 4: {
         r = 2 / z0 / PI / cosh( z / z0 );
         break;
      }
      default: {
         break;
      }
   }
   return( r );
}


/*
 * intzed computes the integral over Z that defines the kernel of the
 * integral formula for the rotation velocity. This is done for an
 * arbitrary vertical density profile, specified in the function denzed.
 * The interval of integration is divided into several subintervals, in
 * each of which Simpson's rule is used. It is thus possible to have
 * different steps in different regions of the interval of integration,
 * and to compute the integral more accurately. An attempt has been made
 * at optimizing the subdivision. It is still possible that the kernel is
 * not accurate, especially when R is very close to U. It is NOT
 * recommended that the programme is used with very small, but non-zero,
 * values of the thickness. On the other hand, the situation with an
 * infinitely thin disk is dealt with properly, the density profile in
 * Z being then treated as a Dirac delta function.
 */

static	double	intzed( double r, double u, double z0, fint naccur, fint mode )
{
   double		res;
   static double	zeds[5] = { 0.02, 0.50, 1.00, 3.00, 10.00 };
   double		z, zne, zol, step, w;
   fint	 		i, j, n;

   if ( z0 != 0.0 && naccur != 0 ) {
      n = 10;
      n = naccur * n;
      if ( fabs( r - u ) <= ( 0.2 * z0 ) && naccur > 1 ) {
         n = 200;
      }
      zne = 0.0;
      res = 0.0;
      for ( i = 0; i < 5; i++ ) {
         zol = zne;
         zne = zeds[i] * z0;
         step = ( zne - zol ) / n;
         z = zol;
         res += step * func( r, u, z ) * denzed( z, z0, mode ) / 3.0;
         w = 4.0 / 3.0;
         for ( j = 0; j < n; j++ ) {
            z += step;
            res += step * func( r, u, z ) * denzed( z, z0, mode ) * w;
            w = 2.0 - w;
         }
         res -= step * func( r, u, z ) * denzed( z, z0, mode ) / 3.0;
      }
   } else {
      res = func( r, u, z0 );
   }
   return( res );
}

/*
 * interp does a Lagrange interpolation (second order).
 */

static	void	interpd( double x1[], double y1[], fint n1, double x2[],
		   double y2[], fint n2 )
{
   double	x, y, f;
   fint		done;
   fint		i, j, k, l;

   for ( i = 0; i < n2; i++ ) {
      x = x2[i];
      y = 0.0;
      for ( done = 0, j = 1; !done && j < ( n1 - 1 ); j++ ) {
         if ( ( x1[j-1] <= x ) && ( x1[j+1] >= x ) ) {
            for ( k = j - 1; k <= j + 1; k++ ) {
               f = 1.0;
               for ( l = j - 1; l <= j + 1; l++ ) {
                  if ( l != k ) {
                     f *= ( x - x1[l] ) / ( x1[k] - x1[l] );
                  }
               }
               y += f * y1[k];
            }
            done = 1;
         }
      }
      y2[i] = ( y > 0.0 ? y : 0.0 );
   }
}


/*
 * interg does the integration
 */

static	double	interg( double xd[], double yd[], fint nd,
		   double r, double rstart, double z0,
		   double step, fint ndens, fint naccur, fint mode )
{
   double	vsq = 0.0;			/* return value */
   fint		i, k, k1, k2;			/* counters */

   for ( i = 1, k = 0; k < ndens; k++ ) {
      double	x, y;
      fint	iw;

      if ( k == 0 || k == ( ndens - 1 ) ) {
         iw = 1;
      } else {
         iw = 4 - 2 * ( ( k + 1 ) % 2 );
      }
      x = rstart + step * k;
      y = 0.0;
      if ( x < xd[nd-1] ) {
         fint	done = 0;

         while ( !done && i < ( nd - 1 ) ) {
            if ( xd[i-1] <= x && xd[i+1] >= x ) {
               for ( k1 = i - 1; k1 <= ( i + 1 ); k1++ ) {
                  double	f = 1.0;

                  for ( k2 = i - 1; k2 <= ( i + 1 ); k2++ ) {
                     if ( k1 != k2 ) {
                        f *= ( x - xd[k2] ) / ( xd[k1] - xd[k2] );
                     }
                  }
                  y += f * yd[k1];
                  done = 1;
               }
            } else {
               i += 1;
            }
         }
         if ( y > 0.0 ) {
            double	res;

            res = intzed( r, x, z0, naccur, mode );
            vsq += CONSTANT * res * y * step * iw;
         }
      }
   }
   return( vsq );
}


static	void	rotdisk( void )
{
   FILE		*f;			/* filename */
   bool		extra = FALSE;		/* extra sampling radii */
   bool		user = TRUE;		/* user supplied surface brightness */
   char		message[MESLEN];	/* message buffer */
   double	delta = 0.0;		/* softening parameter */
   double	dens0;			/* central surface density */
   double	dkmass;			/* disk mass */
   double	h;			/* scale length of disk */
   double	rcut;			/* cutoff radius */
   double	rings[LEN1-16];		/* sampling radii */
   double	width;			/* width */
   double	z0;			/* Z scale length */
   double	z1;
   fint		mode;			/* which Z density law */
   fint		naccur;			/* accuracy of calculations */
   fint		nring;			/* number of sampling radii */
   fint		ntimes = 10;		/* accuracy multiplication factor */
   fint		output_level = 0;	/* default to screen and logfile */
                                        /* can be changed by user */

   /*
    * get density law in Z
    */
   {
      char	listb[4*TXTLEN], testb[TXTLEN];		/* text buffers */
      fchar	list, test;				/* for match */
      fint	error_level = 4;			/* fatal error */
      fint	input_level = 1;			/* default allowed */
      fint	nitems = 1;				/* number of items */
      fint	nlist = 4;				/* size of list */

      list.a = listb; list.l = TXTLEN;		/* initialize */
      test.a = testb; test.l = TXTLEN;		/* initialize */
      fcopy( list, 0, "NONE" );
      fcopy( list, 1, "SECH-SQUARED" );
      fcopy( list, 2, "EXPONENTIAL" );
      fcopy( list, 3, "SIMPLE-SECH" );
      fcopy( test, 0, "NONE" );
      (void) usertext_c( test, &input_level, KEY_ZLAW, MES_ZLAW );
      mode = match_c( list, &nlist, test );	/* match with list */
      switch( mode ) {				/* which option */
         case 1: {				/* flat disk */
            z0 = 0.0;				/* very flat */
            naccur = 10;			/* default accuracy */
            break;
         }
         case 2:				/* sech-squared law */
         case 3:				/* exponential */
         case 4: {				/* simple-sech */
            input_level = 0;			/* no default */
            (void) userdble_c( &z0, &nitems, &input_level, KEY_Z0, MES_Z0 );
            if (z0 <= 0.0) {			/* check */
               error_c( &error_level, ERR_Z0 );	/* error message */
            }
            naccur = 1;				/* default accuracy */
            input_level = 2;			/* default, hidden */
            (void) userint_c( &naccur, &nitems, &input_level, KEY_ACCURACY,
               MES_ACCURACY );
            if (naccur <= 0) naccur = 0;	/* least accurate */
            if (naccur > 10) naccur = 10;	/* most accurate */
            break;
         }
         default: {				/* unknown Z density law */
            error_c( &error_level, ERR_ZLAW );	/* error message */
            break;
         }
      }
      if (naccur >= 1) ntimes *= naccur;	/* modify */
   }
   /*
    * Next we need to know what kind of density distribution the user
    * wants to supply. Choices are:
    * TRUNCATED-EXPONENTIAL-DISK             USER=N
    * USER-SUPPLIED R and S(R) pairs         USER=Y
    */
   {
      fint	input_level = 1;		/* default allowed */
      fint	nitems = 1;			/* number of items */

      (void) userlog_c( &user, &nitems, &input_level, KEY_USER, MES_USER );
      user = tobool( user );			/* to C boolean */
   }
   /*
    * Now get the surface densities from the user.
    */
   if (!user) {
      fint	error_level = 4;		/* fatal error */
      fint	input_level = 0;		/* no default */
      fint	nitems = 1;			/* number of items */

      (void) userdble_c( &dens0, &nitems, &input_level, KEY_DENS0, MES_DENS0 );
      if (dens0 <= 0.0) {			/* check input */
         error_c( &error_level, ERR_DENS0 );	/* error message */
      }
      dens0 *= 1.0E6;				/* from pc**-2 to kpc**-2 */
      (void) userdble_c( &h, &nitems, &input_level, KEY_H, MES_H );
      if (h <= 0.0) {				/* check input */
         error_c( &error_level, ERR_H );
      }
      input_level = 1;				/* default allowed */
      rcut = 10.0 * h;				/* default: no truncation */
      (void) userdble_c( &rcut, &nitems, &input_level, KEY_RCUT, MES_RCUT );
      if (rcut <= 0.0) {			/* check input */
         error_c( &error_level, ERR_RCUT );	/* error message */
      }
      if (rcut > ( 10.0 * h )) {		/* too large */
         rcut = 10.0 * h;			/* decrease */
      }
      (void) userdble_c( &delta, &nitems, &input_level, KEY_DELTA, MES_DELTA );
      if (delta < 0.0) {			/* check input */
         error_c( &error_level, ERR_DELTA );	/* error message */
      }
      /*
       * Now calculate the surface densities.
       */
      {
         double	rdinp;				/* maximum radius */
         double	xstep;				/* step in radius */
         fint	i;				/* counter */

         rdinp = rcut + delta;			/* here the galaxy ends */
         ndinp = LEN2-1;			/* number of surface densities */
         xstep = rdinp / ( ndinp - 1 );		/* step in r */
         for ( i = 0; i < ndinp; i++ ) {	/* loop */
            double	x;			/* current radius */
            double	y;			/* current density */

            xdinp[i] = x = xstep * i;		/* here we are */
            if ( (x >= rcut) && (delta > 0.0) ) {
               y = dens0 * exp( -rcut / h ) * ( rcut + delta - x ) / delta;
            } else if (x >= rdinp ) {
               y = 0.0;				/* end of disk */
            } else {
               y = dens0 * exp( -x / h );	/* exponential disk */
            }
            ydinp[i] = y;
         }
      }
   } else {					/* input from user */
      getrandd( );				/* get it */
      rcut = xdinp[ndinp-1];
      delta = rcut - xdinp[ndinp-2];
      /*
       * Now try to fit an exponential and fit H and DENS0.
       */
      {
         fint	n;
         double	det, s = 0.0, sx = 0.0, sy = 0.0, sxy = 0.0, sxx = 0.0;

         for ( n = 0; n < ndinp; n++ ) {
            if ( ydinp[n] > 0.0 ) {
               s += 1.0;
               sx += xdinp[n];
               sxx += xdinp[n] * xdinp[n];
               sy += log( ydinp[n] );
               sxy += xdinp[n] * log( ydinp[n] );
            }
         }
         det = s * sxx - sx * sx;
         h = det / ( sx * sy - s * sxy );
         if ( h > ( 0.5 * rcut ) ) h = 0.5 * rcut;
         if ( h < ( 0.1 * rcut ) ) h = 0.1 * rcut;
         dens0 = exp( ( sy * sxx - sx * sxy ) / det );
      }
   }
   /*
    * Now we determine the disk mass and allow the user to modify it.
    */
   {
      double	rmass;				/* mass scale factor */
      fint	error_level = 4;		/* fatal error */
      fint	input_level = 1;		/* default allowed */
      fint	n;				/* counter */
      fint	nitems = 1;			/* number of items */

      dkmass = 0.0;				/* reset */
      for ( n = 1; n < ndinp; n++ ) {
         double	step;
         double	x = xdinp[n];

         if ( n < ( ndinp - 1 ) ) {
            step = ( xdinp[n+1] - xdinp[n-1] ) / 2.0;
         } else {
            step = xdinp[n] - xdinp[n-1];
         }
         dkmass += ydinp[n] * step * x;
      }
      dkmass *= ( 2.0 * PI );
      rmass = dkmass / 1.0E9;
      sprintf( message, "Disk Mass in 10^9 MSUN [%8.3f]", rmass );
      (void) userdble_c( &rmass, &nitems, &input_level, KEY_MASS,
        tofchar( message ) );
      if ( rmass <= 0.0 ) {			/* check input */
         error_c( &error_level, ERR_MASS );
      }
      rmass = ( rmass * 1.0E9 ) / dkmass;
      dkmass = rmass * dkmass;
      for ( n = 0; n < ndinp; n++ ) ydinp[n] *= rmass;
      dens0 = dens0 * rmass;
      z1 = z0;
      if ( z1 < ( 0.1 * h ) ) z1 = 0.1 * h;
      if ( z1 > ( 0.3 * h ) ) z1 = 0.3 * h;
   }
   /*
    * now get radii for which we should calculate the rotation curve.
    */
   {
      fint	error_level = 4;		/* fatal error */
      fint	input_level = 0;		/* no default */
      fint	l, m, n;			/* counters */
      fint	nitems = LEN1 - 16;		/* number of items */

      nring = userdble_c( rings, &nitems, &input_level, KEY_RADII, MES_RADII );
      for ( n = 0; n < nring; n++ ) {
         if ( rings[n] < 0.0 ) {
            error_c( &error_level, ERR_RADII );
         }
      }
      nitems = 1;
      input_level = 2;
      (void) userlog_c( &extra, &nitems, &input_level, KEY_EXTRA, MES_EXTRA );
      sortradii( rings, &nring );
      width = ( z1 < delta ? z1 : delta );
      if ( !user && width != 0.0 ) {
         double	s1, s2;
         fint	indmin, indmax;

         s1 = rcut - width;
         s2 = rcut + 2.0 * width;
         indmin = indmax = nring - 1;
         for ( n = 0; n < nring; n++ ) {
            if ( rings[n] > s1 ) {
               indmin = ( indmin < n ? indmin : n );
            }
            if ( rings[n] > s2 ) {
               indmax = ( indmax < n ? indmax : n );
            }
         }
         for ( m = n = 0; n < nring; n++) {
            if ( n < indmin || n > indmax ) {
               radius[m++] =  rings[n];
            } else {
               if ( n == indmin ) {
                  double	stp = width / 5.0;

                  for ( l = 0; l < 15; l++ ) {
                     radius[m++] = rcut - width * l * stp;
                  }
               }
               radius[m++] = rings[n];
            }
         }
         nradii = m;
      } else if (user) {
         double	r1 = 0.0, r2 = 0.0;

         radius[0] = 0.0;
         l = m = n = 0;
         while ( ( l < nring || n < ndinp ) && m < ( LEN1 + LEN2 + 1 ) ) {

            while ( l < nring && radius[m] >= ( r1 = rings[l] ) ) l++;
            while ( n < ndinp && radius[m] >= ( r2 = xdinp[n] ) ) n++;
            if ( r1 > radius[m] && r2 > radius[m] ) {
               radius[++m] = ( r1 < r2 ? r1 : r2 );
            } else if ( r1 > radius[m] ) {
               radius[++m] = r1;
            } else if ( r2 > radius[m] ) {
               radius[++m] = r2;
            }
         }
         nradii = m + 1;
      } else {
         nradii = 0;
         for ( n = 0; n < nring; n++ ) {
            radius[nradii++] = rings[n];
         }
      }
   }
   sortradii( radius, &nradii );
   f = getfile( );
   if (!user) {
      sprintf( message, "Scalelength     : %12.4f KPC", h );
      fprintf( f, "!%s\n", message );
      anyout_c( &output_level, tofchar( message ) );
      sprintf( message, "Central Density : %12.4f MSUN/PC**2", dens0 / 1.0E6 );
      fprintf( f, "!%s\n", message );
      anyout_c( &output_level, tofchar( message ) );
      sprintf( message, "Cutoff Radius   : %12.4f KPC", rcut );
      fprintf( f, "!%s\n", message );
      anyout_c( &output_level, tofchar( message ) );
      sprintf( message, "Softening       : %12.4f KPC", delta );
      fprintf( f, "!%s\n", message );
      anyout_c( &output_level, tofchar( message ) );
   }
   switch( mode ) {
      case 2: {
         sprintf( message, "Z-Density law   : SECH-SQUARED" );
         fprintf( f, "!%s\n", message );
         anyout_c( &output_level, tofchar( message ) );
         sprintf( message, "Z Scaleheight   : %12.4f KPC", z0 );
         fprintf( f, "!%s\n", message );
         anyout_c( &output_level, tofchar( message ) );
         break;
      }
      case 3: {
         sprintf( message, "Z-Density law   : EXPONENTIAL" );
         fprintf( f, "!%s\n", message );
         anyout_c( &output_level, tofchar( message ) );
         sprintf( message, "Z Scaleheight   : %12.4f KPC", z0 );
         fprintf( f, "!%s\n", message );
         anyout_c( &output_level, tofchar( message ) );
         break;
      }
      case 4: {
         sprintf( message, "Z-Density law   : SIMPLE-SECH" );
         fprintf( f, "!%s\n", message );
         anyout_c( &output_level, tofchar( message ) );
         sprintf( message, "Z Scaleheight   : %12.4f KPC", z0 );
         fprintf( f, "!%s\n", message );
         anyout_c( &output_level, tofchar( message ) );
         break;
      }
      default: {
         break;
      }
   }
   sprintf( message, "Accuracy        : %12d", naccur );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   sprintf( message, "Mass of disk    : %12.4f 10**9 MSUN", dkmass / 1.0E9 );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   sprintf( message, " " );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   sprintf( message, "   Radius      Density      Velocity   " );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   sprintf( message, "    KPC       MSUN/PC**2     KM/SEC    " );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   sprintf( message, "---------------------------------------" );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   /*
    * Now we do the calculation.
    */
   {
      fint	i, j = 0;				/* counter */

      interpd( xdinp, ydinp, ndinp, radius, densit, nradii );
      for ( i = 0; i < nradii; i++ ) {
         double	r = radius[i];
         double	rstart;
         double	r1, r2;
         double	step;
         double	vsq = 0.0;
         fint	ndens;
         int	skip;

         while ( ( j < nring ) && ( rings[j] < r ) ) j++;
         skip = ( rings[j] != r );
         r1 = r - 3.0 * z1;
         if ( r1 < 0.0 ) r1 = 0.0;
         r2 = 0.0;
         if ( r1 < ( rcut + 2.0 * delta ) ) {
            r2 = r + ( r - r1 );
            ndens = 6 * ntimes + 1;
            step = ( r2 - r1 ) / ( ndens - 1 );
            rstart = r1;
            vsq += interg( xdinp, ydinp, ndinp, r, rstart, z0, step, ndens,
               naccur, mode );
            if ( r1 > 0.0 ) {
               ndens = r1 * ntimes / h;
               ndens = 2 * ( ndens / 2 ) + 3;
               step = r1 / ( ndens - 1 );
               rstart = 0.0;
               vsq += interg( xdinp, ydinp, ndinp, r, rstart, z0, step, ndens,
                  naccur, mode );
            }
         }
         if ( r2 < ( rcut + 2.0 * delta ) ) {
            ndens = ( rcut + 2.0 * delta - r2 ) * ntimes / h;
            ndens = 2 * ( ndens / 2 ) + 3;
            step = ( rcut + 2.0 * delta - r2 ) / ( ndens - 1 );
            rstart = r2;
            vsq += interg( xdinp, ydinp, ndinp, r, rstart, z0, step, ndens,
               naccur, mode );
         }
         if ( vsq < 0.0 ) {
            velocs[i] = -sqrt( -vsq );
         } else {
            velocs[i] =  sqrt( vsq );
         }
         if ( !skip || tobool( extra ) ) {
            sprintf( message, "%10.5f  %10.5f  %13.6f", radius[i],
               densit[i] / 1.0E6, velocs[i] );
            fprintf( f, " %s\n", message );
            anyout_c( &output_level, tofchar( message ) );
         }
      }
   }
   fclose( f );					/* close file */
}


/******************************************************************************


                                   THE BULGE


 *****************************************************************************/

static	double	interpb( double x )
{
   double	r = 0;
   fint		j;

   for ( j = 1; j < ndinp; j++ ) {
      if ( ( xdinp[j-1] <= x ) && ( xdinp[j] >= x ) ) {
         if ( ydinp[j-1] > ydinp[j] ) {
            double	xx4 = pow( x,0.25 );
            double	x14 = pow( xdinp[j-1], 0.25 );
            double	x24 = pow( xdinp[j], 0.25 );
            double	fp;

            fp = ( xx4 - x14 ) / ( x24 - x14 );
            r = ydinp[j-1] * pow( ydinp[j] / ydinp[j-1], fp );
         } else {
            r = ( ydinp[j] - ydinp[j-1] ) / ( xdinp[j] - xdinp[j-1] ) * ( x - xdinp[j-1] ) + ydinp[j-1];
         }
         break;
      }
   }
   return( r );
}

static	double	mu( double x, double xmax )
{
   if ( x <= xmax ) {
      return( interpb( x ) );
   } else {
      return( 0.0 );
   }
}

static	double	one( double x, double xpar )
{
   return( TWOPI * x * mu( x, xdinp[ndinp-1] ) );
}

/*
 * two: Used in inversion
 */

static	double	two( double x, double r )
{
   double	rmax = xdinp[ndinp-1];

   if ( ( x > r ) && ( r <= rmax ) ) {
      double	d = r /sqrt( x * x - r * r );

      return( ( atan( d ) - d ) * x * mu( x, rmax ) );
   } else {
      return( 0.0 );
   }
}


static	double	sign( double d )
{
   if ( d < 0.0 ) return( -1.0 ); else return( 1.0 );
}


/*
 * tintd: RK4-version   double precision
 *
 *     Parameters:
 *       A            starting point of integration
 *       B            end point of integration
 *       EPS          typical radius of curvature at strongest edge,
 *                    put it to a small number, like 10**(-4) or so
 *       FUN          function to be integrated (must be EXTERNAL in calling)
 *       ERR          wanted relative accuracy in the final value of integration
 *
 *     Hidden parameters:
 *       TMIN         Determines how fast too small steps are detected
 *       TMAX         Determines how fast too large steps are detected
 *
 *     Additional paprameter:
 *       POWER        behaviour at either one of the edges
 *                    should be like EPS*power, where EPS is a
 *                    typical distance from the end edge (B !!!)
 *                    Normally 1.0 for continuous finite function
 *                    it only speeds up things when <>0.0
 *                    Note that EPS is also a parameter in the
 *                    the function call
 *     Updates:
 *                    Aug 12, 1986:  P.J. Teuben, Original code.
 *                    Aug 16, 1986:  KGB, debugged !!!!
 *                    Mar 29, 1995:  KGB, converted to ANSI C.
 */

#define	TMAX	(4.0)
#define	TMIN	(0.001)

static	double	tintd( double a, double b, double eps, double (*fun)( double, double ),
   double err, double power, double xpar )
{
   double	pfac, xend, yold;
   double	test, ynew, x, y, dx, dd1 = 0.0, dd2, dd3, dd4, dd5 = 0.0;
   double	dy1, dy2, fac, err1, err2;
   fint		loop, icount, nsteps;

   icount = 0;
   nsteps = 0;
   pfac = 2.0 / ( power + 1.0 );
   fac = 1.0;
   if ( power != 0.0 ) fac = 1.0 - eps;
   fac *= sign( b - a );
   dx = ( b - a ) * 0.05;
   /* start of integration: */
   x = a;
   y = 0.0;
   ynew = 0.0;
   while ( 1 ) {
      xend = x + dx;
      /* Test outside int-interval, also valid for DX<0 */
      /* IF (XEND.GT.B .XOR. DX.LT.0.0D0) DX=DABS(B-X)*FAC */
      if ( ( xend > b ) != ( dx < 0.0 ) ) dx = fabs( b - x ) * fac;
      if ( dx == 0.0 ) break;
      /* RESET small loop counter */
      loop = 0;
      while ( 1 ) {
         ++icount;
         ++loop;
         if ( nsteps > 0 ) {
            if ( loop == 1 ) dd1 = dd5;
         } else {
            dd1 = (*fun)( x, xpar ) * 0.5;
         }
         dd2 = (*fun)( x + dx * 0.25, xpar ) * 2.0;
         dd3 = (*fun)( x + dx * 0.50, xpar );
         dd4 = (*fun)( x + dx * 0.75, xpar ) * 2.0;
         dd5 = (*fun)( x + dx, xpar ) * 0.5;
         dy2 = dx * ( dd1 + dd2 + dd3 + dd4 + dd5 ) / 6.0;
         dy1 = dx * ( dd1 * 2.0 + dd3 * 4.0 + dd5 * 2.0 ) / 6.0;
         test = pow( fabs( ( dy1 - dy2 ) / ( dy1 * err ) ), 0.2 );
         if ( test < TMAX ) break;
         dx = dx * 2.0 / test;
      }
      yold = y + dd1 * ( b - x )  * pfac;
      y += ( dy2 * 16.0 - dy1 ) / 15.0;
      x += dx;
      ynew = y + dd5 * ( b - x ) * pfac;
      ++nsteps;
      err1 = fabs( ( yold - ynew ) / ynew );
      err2 = fabs( ( y - ynew ) / y );
      /* check if stepsize was too big */
      if ( test < TMIN ) {
         /* take a much bigger step */
         dx *= 10.0;
      } else {
         /* normal preventative restep */
         dx = dx * 2.0 / test;
      }
      /* check if we're done */
      if ( ( err1 < err ) || ( err2 < err ) ) break;
      /* IF (X.GE.B .XOR. DX.LT.0.0D0) THEN */
      if ( ( x >= b ) != ( dx < 0.0 ) ) break;
   }
   return( ynew * sign( b - a ) );
}


static	void	rotbulge( void )
{
   FILE		*f;				/* file descriptor */
   char		message[MESLEN];		/* message buffer */
   double	bumass;				/* mass of bulge */
   fint		output_level = 3;		/* output device */

   getrandd( );					/* get it */
   /*
    * Now we determine the bulge mass and allow the user to modify it.
    */
   {
      double	rmass;				/* mass scale factor */
      fint	error_level = 4;		/* fatal error */
      fint	input_level = 1;		/* default allowed */
      fint	n;				/* counter */
      fint	nitems = 1;			/* number of items */

#if	0
      bumass = 0.0;				/* reset */
      for ( n = 1; n < ndinp; n++ ) {
         double	step;
         double	x = xdinp[n];

         if ( n < ( ndinp - 1 ) ) {
            step = ( xdinp[n+1] - xdinp[n-1] ) / 2.0;
         } else {
            step = xdinp[n] - xdinp[n-1];
         }
         bumass += ydinp[n] * step * x;
      }
      bumass *= ( 2.0 * PI );
#endif
      bumass = tintd( 0.0, xdinp[ndinp-1] - EPS, 0.00001, one, 0.00001, 0, xdinp[ndinp-1] - EPS );
      rmass = bumass / 1.0E9;
      sprintf( message, "Bulge Mass in 10^9 MSUN [%8.3f]", rmass );
      (void) userdble_c( &rmass, &nitems, &input_level, KEY_MASS,
        tofchar( message ) );
      if ( rmass <= 0.0 ) {			/* check input */
         error_c( &error_level, ERR_MASS );
      }
      rmass = ( rmass * 1.0E9 ) / bumass;
      bumass = rmass * bumass;
      for ( n = 0; n < ndinp; n++ ) ydinp[n] *= rmass;
   }
   /*
    * now get radii for which we should calculate the rotation curve.
    */
   {
      double	rings[LEN1-16];
      fint	error_level = 4;		/* fatal error */
      fint	input_level = 0;		/* no default */
      fint	n;				/* counters */
      fint	nitems = LEN1 - 16;		/* number of items */
      fint	nring;

      nring = userdble_c( rings, &nitems, &input_level, KEY_RADII, MES_RADII );
      for ( n = 0; n < nring; n++ ) {
         if ( rings[n] < 0.0 ) {
            error_c( &error_level, ERR_RADII );
         }
      }
      sortda_c( rings, &nring );		/* sort radii */
      nradii = 0;
      if ( rings[0] != 0.0 ) {
         radius[nradii++] = 0.0;
      }
      for ( n = 0; n < nring; n++ ) {
         radius[nradii++] = rings[n];
      }
   }
   f = getfile( );
   sprintf( message, "Mass of bulge   : %12.4f 10**9 MSUN", bumass / 1.0E9 );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   sprintf( message, " " );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   sprintf( message, "   Radius      Density      Velocity   " );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   sprintf( message, "    KPC       MSUN/PC**2     KM/SEC    " );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   sprintf( message, "---------------------------------------" );
   fprintf( f, "!%s\n", message );
   anyout_c( &output_level, tofchar( message ) );
   {
      fint	i;
      double	rmax = xdinp[ndinp-1];
      double	brmass;

      velocs[0] = 0.0;
      densit[0] = interpb( 0.0 );
      sprintf( message, "%10.5f  %10.5f  %13.6f", radius[0],
         densit[0] / 1.0E6, velocs[0] );
      fprintf( f, " %s\n", message );
      anyout_c( &output_level, tofchar( message ) );
      for ( i = 1; i < nradii; i++ ) {
         double	rmod = radius[i];

         if ( rmod < rmax ) {
            double	y1, y2, y3;
            double	rhal;

            y1 = tintd(  0.0, rmod, 0.00001, one, 0.00001,  0.0, rmod );
            rhal = 0.5 * ( rmod + rmax );
            y2 = tintd( rhal, rmod, 0.00001, two, 0.00001, -0.5, rmod );
            y3 = tintd( rhal, rmax, 0.00001, two, 0.00001,  0.5, rmod );
            brmass = y1 + 4.0 * ( y2 + y3 );
         } else {
            brmass = bumass;
         }
         if ( brmass < 0.0 ) {
            velocs[i] = -sqrt( G * -brmass / rmod );
         } else {
            velocs[i] = sqrt( G * brmass / rmod );
         }
         densit[i] = interpb( rmod );
         sprintf( message, "%10.5f  %10.5f  %13.6f", radius[i],
            densit[i] / 1.0E6, velocs[i] );
         fprintf( f, " %s\n", message );
         anyout_c( &output_level, tofchar( message ) );
      }
   }
   fclose( f );
}


/******************************************************************************


                                 MAIN PROGRAM


 *****************************************************************************/

MAIN_PROGRAM_ENTRY
{
   init_c( );
   IDENTIFICATION( "ROTMOD", VERSION );		/* identify */
   {
      char	listb[2*TXTLEN], testb[TXTLEN];	/* text buffers */
      fchar	list, test;			/* for match */
      fint	error_level = 4;		/* fatal error */
      fint	input_level = 1;		/* default allowed */
      fint	nlist = 2;			/* size of list */

      list.a = listb; list.l = TXTLEN;		/* initialize */
      test.a = testb; test.l = TXTLEN;		/* initialize */
      fcopy( list, 0, "DISK" );
      fcopy( list, 1, "BULGE" );
      fcopy( test, 0, "DISK" );
      (void) usertext_c( test, &input_level, KEY_TYPE, MES_TYPE );
      switch( match_c( list, &nlist, test ) ) {	/* match with list */
         case	1: rotdisk( ); break;
         case	2: rotbulge( ); break;
         default : error_c( &error_level, ERR_TYPE ); break;
      }
   }
   finis_c( );
   return( EXIT_SUCCESS );
}
