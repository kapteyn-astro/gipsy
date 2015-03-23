/* ellipse.c

        Copyright (c) Kapteyn Laboratorium Groningen 1991
        All Rights Reserved.

#>            ellipse.dc2

Document:     ELLIPSE

Purpose:      Describes the ellipse fitting routines.

Category:     MATH

File:         ellipse.c

Author:       K.G. Begeman

Description:  The ellipse fitting routines available are:

              ELLIPSE1        Which finds the initial estimates of
                              the ellipse parameters.
              ELLIPSE2        Which finds the actual ellipse parameters
                              by a least-squares fit.
                              ELLIPSE1 can be used to determine the
                              initial estimates which are needed by
                              ELLIPSE2

Updates:      Jan 25, 1991: KGB Document created.

#<

*/

#include	"math.h"			/* <math.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include        "time.h"                        /* <time.h> */
#include	"gipsyc.h"			/* GIPSY definitions */

#define	F	0.0174532925			/* conversion factor */
#define	FAC	10.0				/* mixing * factor */
#define	LAB	0.01				/* start mixing parameter */
#define	LABMAX	1.0E+10				/* max. mixing parameter */
#define	LABMIN	1.0E-10				/* min. mixing parameter */
#define	PARS	5			 	/* number of parameters */
#define	T	50				/* max. number of iterations */
#define	TOL	0.00001				/* tolerance */


static fint invmat( double matrix[PARS][PARS], fint nfree )
/*
 * invmat calculates the inverse of matrix. The algorithm used is the
 * Gauss-Jordan algorithm described in Stoer, Numerische matematik, 1 Teil.
 */
{
   double even;
   double hv[PARS];
   double mjk;
   double rowmax;
   fint   evin;
   fint   i;
   fint   j;
   fint   k;
   fint   per[PARS];
   fint   row;

   for (i = 0; i < nfree; i++) per[i] = i;	/* set permutation array */
   for (j = 0; j < nfree; j++) {		/* in j-th column, ... */
      rowmax = fabs( matrix[j][j] );		/* determine row with ... */
      row = j;					/* largest element. */
      for (i = j + 1; i < nfree; i++) {
         if (fabs( matrix[i][j] ) > rowmax) {
            rowmax = fabs( matrix[i][j] );
            row = i;
         }
      }
      if (matrix[row][j] == 0.0) return( -6 );	/* determinant is zero! */
      if (row > j) {				/* if largest element not ... */
         for (k = 0; k < nfree; k++) {		/* on diagonal, then ... */
            even = matrix[j][k];		/* permutate rows. */
            matrix[j][k] = matrix[row][k];
            matrix[row][k] = even;
         }
         evin = per[j];				/* keep track of permutation */
         per[j] = per[row];
         per[row] = evin;
      }
      even = 1.0 / matrix[j][j];		/* modify column */
      for (i = 0; i < nfree; i++) matrix[i][j] *= even;
      matrix[j][j] = even;
      for (k = 0; k < j; k++) {
         mjk = matrix[j][k];
         for (i = 0; i < j; i++) matrix[i][k] -= matrix[i][j] * mjk;
         for (i = j + 1; i < nfree; i++) matrix[i][k] -= matrix[i][j] * mjk;
         matrix[j][k] = -even * mjk;
      }
      for (k = j + 1; k < nfree; k++) {
         mjk = matrix[j][k];
         for (i = 0; i < j; i++) matrix[i][k] -= matrix[i][j] * mjk;
         for (i = j + 1; i < nfree; i++) matrix[i][k] -= matrix[i][j] * mjk;
         matrix[j][k] = -even * mjk;
      }
   }
   for (i = 0; i < nfree; i++) {		/* finally, repermute the ... */
      for (k = 0; k < nfree; k++) {		/* columns. */
         hv[per[k]] = matrix[i][k];
      }
      for (k = 0; k < nfree; k++) {
         matrix[i][k] = hv[k];
      }
   }
   return( 0 );					/* all is well */
}

/*
 * inimat sets up the matrix to be inverted in inivec. 
 * inimat returns the reduced chi-squared.
 */
static	double	inimat( double	s[PARS][PARS] ,
                        double  rl[PARS] ,
                        float	*x ,
                        float	*y ,
                        fint	n ,
                        float	*p ,
                        float	*e ,
                        fint	ip[PARS] ,
                        fint	nfree )
{
   double	chi = 0.0;			/* return value */
   double	cosi, cosp, sini, sinp;		/* sines and cosines */
   fint		i, j, k;			/* counters */

   /*
    * initialize the matrix and the vector
    */
   for (j = 0; j < nfree; j++) {   
      rl[j] = 0.0;
      for (k = 0; k <= j; k++) {
         s[j][k] = 0.0;
      }
   }
   cosp = cos( F * p[4] );			/* cosine of p.a. */
   sinp = sin( F * p[4] );			/* sine of p.a. */
   cosi = cos( F * p[1] );			/* cosine of inclination */
   sini = sin( F * p[1] );			/* sine of inclination */
   for (i = 0; i < n; i++) {
      double	cost, sint, u;

      /*
       * Calculate the rotated x and y coordinates
       */
      cost=( -( x[i] - p[2] ) * sinp + ( y[i] - p[3] ) * cosp ) / p[0];
      sint=(- ( x[i] - p[2] ) * cosp - ( y[i] - p[3] ) * sinp ) / p[0] / cosi;
      u = 1.0 - cost * cost - sint * sint;	/* difference with model */
      /*
       * Now calculate the partial derivatives
       */
      e[0] = -2.0 * ( cost * cost + sint * sint ) / p[0];
      e[1] =  2.0 * F * sint * sint * sini / cosi;
      e[2] =  2.0 * ( cost * sinp + sint * cosp / cosi ) / p[0];
      e[3] = -2.0 * ( cost * cosp - sint * sinp / cosi ) / p[0];
      e[4] = -2.0 * F * sint * cost * sini * sini / cosi;
      chi = chi + u * u;			/* add to reduced chi-squared */
      /*
       * Now we fill the matrix and the vector
       */
      for (j = 0; j < nfree; j++) {
         rl[j] += u * e[ip[j]];
         for (k = 0; k <= j; k++) {
            s[j][k] += e[ip[j]] * e[ip[k]];
         }
      }
   }
   return( chi );				/* return chi-squared */
}

/*
 * inivec calculates the correction vector.
 */
static	fint	inivec( double	s[PARS][PARS] ,
                        double	s1[PARS][PARS] ,
                        double	rl[PARS] ,
                        double	labda ,
                        double	*q ,
                        float	*p ,
                        float	*e ,
                        float	*x ,
                        float	*y ,
                        fint	n ,
                        fint	ip[PARS] ,
                        fint	nfree )
{
   double	cosi, cosp, sinp;		/* sines and cosines */
   fint		i, j, k;			/* counters */

   /*
    * First we modify and scale the matrix.
    */
   for (j = 0; j < nfree; j++) {
      double	sjj = s[j][j];			/* diagonal */

      if (sjj == 0.0) { return( -3 ); }		/* error */
      for (k = 0; k < j; k++) {
         double	sjk = s[j][k] / sqrt( sjj * s[k][k] );

         s1[j][k] = s1[k][j] = sjk;
      }
      s1[j][j] = 1.0 + labda;			/* new value on diagonal */
   }
   if (invmat( s1, nfree )) return( -4 );	/* error inverting matrix */
   for (i = 0; i < PARS; e[i++] = 0.0);		/* zero difference vector */
   for (j = 0; j < nfree; j++) {
      double	sjj = s[j][j];

      for (k = 0; k < nfree; k++) {
         e[ip[j]] = e[ip[j]] + rl[k] * s1[j][k] / sqrt( sjj * s[k][k] );
      }
   }
   for (i = 0; i < PARS; i++) {
      e[i] += p[i];
   }
   (*q) = 0.0;
   cosp = cos( F * e[4] );			/* cosine of p.a. */
   sinp = sin( F * e[4] );			/* sine of p.a. */
   cosi = cos( F * e[1] );			/* cosine of inclination */
   for (i = 0; i < n; i++) {
      double	cost, sint, u;

      /*
       * Calculate the rotated x and y coordinates
       */
      cost=( -( x[i] - e[2] ) * sinp + ( y[i] - e[3] ) * cosp ) / e[0];
      sint=( -( x[i] - e[2] ) * cosp - ( y[i] - e[3] ) * sinp ) / e[0] / cosi;
      u = 1.0 - cost * cost - sint * sint;	/* difference with model */
      (*q) += u * u;				/* add to chi-squared */
   }
   return( 0 );					/* return to caller */
}

/*

#>            ellipse1.dc2

Function:     ELLIPSE1

Purpose:      Routine which gives an initial estimate for an ellipse to N
              points in the array X,Y.

Category:     MATH

File:         ellipse1.c

Author:       K.G. Begeman

Use:          INTEGER ELLIPSE1( N ,         Input      INTEGER
                                X ,         Input      REAL ARRAY
                                Y ,         Input      REAL ARRAY
                                P )         Output     REAL ARRAY

              ELLIPSE1   Returns 0 when successfull, and 1 on error 
              N          Number of points in X and Y.
              X          Array with X-coordinates.
              Y          Array with Y-coordinates.
              P          Estimated ellipse parameters:
                         P(1) = radius
                         P(2) = inclination (0..90 degrees)
                         P(3) = X0 (centre)
                         P(4) = Y0 (centre)
                         P(5) = position angle of major axis
                                (0..180 degrees) w.r.t. Y axis.

Related Docs: ellipse2.dc2

Updates:      Jan 23, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function ellipse1( integer, real, real, real )

*/

fint	ellipse1_c( fint	*n ,		/* number of points */
                    float	*x ,		/* X coordinates */
                    float	*y ,		/* Y coordinates */
                    float	*p )		/* ellipse parameters */
{
   double	ellips[PARS];			/* ellipse parameters */
   double	matrix[PARS][PARS];		/* the matrix */
   double	vector[PARS];			/* the vector */
   double	xc, yc;				/* centre of ellipse */
   fint		i, j, k;			/* loop counters */

   /*
    * Due to stability problems with ellipses
    * where the origin is far away from the centre
    * of the ellipse we first have to find an approximate
    */
   for (xc = yc = 0.0, k = 0; k < (*n); k++) {	/* loop over all positions */
      xc += x[k];				/* sum x coordinates */
      yc += y[k];				/* sum y coordinates */
   }
   xc /= (double) (*n);				/* mean x position */
   yc /= (double) (*n);				/* mean y position */
   /*
    * Next: initialize the matrix to be inverted by invmat and
    * the vector
    */
   for (i = 0; i < PARS; i++) {
      for (j = 0; j < PARS; j++) {
         matrix[i][j] = 0.0;
      }
      vector[i] = 0.0;
   }
   /*
    * Then: fill matrix
    */
   for (k = 0; k < (*n); k++) {
      double	xs = x[k] - xc;			/* x coord. w.r.t. centre */
      double	ys = y[k] - yc;			/* y coord. w.r.t. centre */
      double	xx;				/* x * x */
      double	xy;				/* cross product */
      double	yy;				/* y * y */
      
      xx = xs * xs;				/* x * x */
      xy = xs * ys;				/* cross product */
      yy = ys * ys;				/* y * y */
      matrix[0][0] += xx * xx;
      matrix[0][1] += 2.0 * xx * xy;
      matrix[0][2] += xy * xy;
      matrix[0][3] += xx * xs;
      matrix[0][4] += xy * xs;
      matrix[1][1] += 4.0 * xy * xy;
      matrix[1][2] += 2.0 * xy * yy;
      matrix[1][3] += 2.0 * xy * xs;
      matrix[1][4] += 2.0 * xy * ys;
      matrix[2][2] += yy * yy;
      matrix[2][3] += xy * ys;
      matrix[2][4] += yy * ys;
      matrix[3][3] += xx;
      matrix[3][4] += xy;
      matrix[4][4] += yy;
      vector[0] += xx;
      vector[1] += 2.0 * xy;
      vector[2] += yy;
      vector[3] += xs;
      vector[4] += ys;
   }
   /*
    * make the matrix symmetric since this saves a lot of typing
    */
   for (i = 0; i < PARS; i++) {
      for (j = 0; j < i; j++) {
         matrix[i][j] = matrix[j][i];
      }
   }
   /*
    * invert the matrix
    */
   if (invmat( matrix, PARS )) {
      return( 1 );				/* cannot invert matrix */
   }
   /*
    * solve MATRIX * ELLIPS = VECTOR
    */
   for (i = 0; i < PARS; i++) {
      ellips[i] = 0.0;				/* reset this ellipse parm. */
      for (j = 0; j < PARS; j++) {
         ellips[i] += matrix[i][j] * vector[j];
      }
   }
   /*
    * NOTE: The ellipse equation taken is
    * AA.x^2 + 2.BB.x.y + CC.y^2 + DD.x + EE.y = 1
    * Where AA..EE were now solved for
    * from which now the ellipse-parameters are derived
    */
   {
      double	aa = ellips[0];
      double	bb = ellips[1];
      double	cc = ellips[2];
      double	dd = ellips[3];
      double	ee = ellips[4];
      double	pa, pp;
      double	cospa, sinpa, sinpp;
      double	s1, s2, s3, y1, y2, y3;
      double	x0, y0;
      double	ab, al, r;

      pp = atan( 2.0 * bb / ( aa - cc ) );	/* estimate of position angle */
      pa = 0.5 * pp;				/* p.a. of an (UNDETERMINED) axis */
      cospa = cos( pa );			/* cosine */
      sinpp = sin( pp );			/* sine of double angle */
      sinpa = sin( pa );			/* sine */
      al = 2.0 * bb / sinpp / ( aa + cc );	/* auxiliary */
      r = sqrt( ( 1.0 + al ) / ( 1.0 - al ) );	/* axial ratio (OR ITS RECIPROCAL) */
      s1 = bb * ee - cc * dd;			/* three other auxiliaries */
      s2 = bb * dd - aa * ee;
      s3 = aa * cc - bb * bb;
      x0 = s1 / s3;				/* X-centre of ellipse */
      y0 = s2 / s3;				/* Y-centre of ellipse */
      y1 = sinpa * sinpa + r * r * cospa * cospa;
      y2 = x0 * y0 * ( r * r - 1.0 ) * sinpp;
      y3 = y0 * y0 * ( cospa * cospa + r * r * sinpa * sinpa );
						/* length of (yet undetermined) axis (A or B) */
      ab = sqrt( y1 * ( x0 * x0 + 1.0 / aa ) + y2 + y3 );
      pa = pa * 45.0 / atan( 1.0 );		/* convert to degrees */
     /*
      * determination which axis is the long axis
      */
      if (r < 1.0) {				/* the other is */
         ab /= r;				/* this one is the major axis */
         pa -= 90.0;				/* so change position angle */
      } else {					/* the right one is */
         r = 1.0 / r;				/* so change axial ratio */
      }
      if (pa < 0.0) pa += 180.0;		/* position angle in range 0.....180 */
      p[0] = ab;				/* radius */
      p[1] = acos( r ) * 45.0 / atan( 1.0 );	/* inclination assuming projected circle */
      p[2] = x0 + xc;				/* new x-position of centre */
      p[3] = y0 + yc;				/* new y-position of centre */
      p[4] = pa;				/* position angle major axis */
   }
   return( 0 );					/* return to caller */
}

/*

#>            ellipse2.dc2

Function:     ELLIPSE2

Purpose:      Fits an ellipse to a set of X and Y positions.

Category:     MATH

File:         ellipse.c

Author:       K.G. Begeman

Use:          INTEGER ELLIPSE2( N ,            Input       INTEGER
                                X ,            Input       REAL ARRAY
                                Y ,            Input       REAL ARRAY
                                P ,        Input/Output    REAL ARRAY
                                E ,            Output      REAL ARRAY
                                M )            Input       INTEGER ARRAY

              ELLIPSE2     Returns number of iterations on success, else
                           -1: No free parameters
                           -2: Exceede iteration limit (50)
                           -3: Diagonal of matrix has zeroes
                           -4: Matrix could not be inverted
              N            Number of positions.
              X            Array with X coordinates (in units).
              Y            Array with Y coordinates (in units).
              P            On input contains the initial estimates
                           of the ellipse parameters. On output the
                           fitted parameters. P contains:
                           P(1) = radius (in units)
                           P(2) = inclination (degrees)
                           P(3) = X centre of ellipse (units)
                           P(4) = Y centre of ellipse (units)
                           P(5) = Position angle (degrees)
              E            Contains the errors in the fitted parameters.
              M            Mask for free (1) or fixed (0) parameters.

Related Docs: ellipse1.dc2
Updates:      Jan 25, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function ellipse2( integer, real, real, real, real, integer )

*/

fint	ellipse2_c( fint	*n ,		/* number of coordinates */
                    float	*x ,		/* X coordinates */
                    float	*y ,		/* Y coordinates */
                    float	*p ,		/* ellipse parameters */
                    float	*e ,		/* errors in ellipse parms. */
                    fint	*m )		/* fixed/free mask */
{
   double       chi;                            /* red. chi-squared */
   double	labda;				/* mixing parameter */
   double       q;                              /* red. chi-squared */
   double	rl[PARS];			/* vector */
   double	s[PARS][PARS], s1[PARS][PARS];	/* matrices */
   fint		h = 0;				/* iteration counter */
   fint         i;                              /* counter */
   fint		ip[PARS];			/* permutation array */
   fint		nfree = 0;			/* number of free parameters */
   fint		r = 0;				/* return value */

   labda = LAB * FAC;				/* start value */
   for (i = 0; i < PARS; i++) {
      if (m[i]) ip[nfree++] = i;		/* fit this parameter */
   }
   if (nfree == 0 || nfree >= (*n)) return( -1 );
   do {						/* iteration loop */
     if (++h > T) { r = -2; break; }		/* too many iterations */
     chi = inimat( s, rl, x, y, (*n), p, e, ip, nfree );
     if (labda > LABMIN) labda /= FAC;		/* new labda */
     r = inivec( s, s1, rl, labda, &q, p, e, x, y, (*n), ip, nfree );
     if (r) break;   				/* error from inivec */
     while (q >= chi) {				/* interpolation loop */
        if (labda > LABMAX) break;		/* leave loop */
        labda *= FAC;				/* new labda */
        r = inivec( s, s1, rl, labda, &q, p, e, x, y, (*n), ip, nfree );
        if (r) break;				/* error from inivec */
      }
      if (labda <= LABMAX) {
         for (i = 0; i < PARS; i++) p[i] = e[i];
      }
      if (fabs( chi - q ) / q < TOL || labda > LABMAX) {
         labda = 0.0;				/* for Taylor solution */
         chi = inimat( s, rl, x, y, (*n), p, e, ip, nfree );
         r = inivec( s, s1, rl, labda, &q, p, e, x, y, (*n), ip, nfree );
         if (!r) {
            r = h;				/* number of iterations */
            for (i = 0; i < PARS; i++) {
               p[i] = e[i];			/* the parameters */
               e[i] = 0.0;			/* reset errors */
            }
            q = sqrt( q / (double) ( (*n) - nfree ) );
            for (i = 0; i < nfree; i++) {
               e[ip[i]] = q * sqrt( s1[i][i] / s[i][i] );
            }
         }
      }
   } while (!r);				/* until error or finished */
   return( r );					/* return to caller */
}

#if	defined(TESTBED)

#define	N	42

int	main( )
{
   fint		k;
   fint		n = N;
   fint		m[PARS];
   float	p[PARS];
   float        e[PARS];
   float	x[N], y[N];

   for (k = 0; k < PARS; k++) {
      p[k] = 0.0;
      e[k] = 0.0;
      m[k] = 1;
   }
   for (k = -10; k < 11; k++) {
      y[k+10] = k;
      x[k+10] = 5.0 * sqrt( 1.0 - y[k+10] * y[k+10] / 10.0 / 10.0 );
      y[k+31] = k;
      x[k+31] = -x[k+10];
   }
   srand( time( (time_t *) NULL ) );
   for (k = 0; k < N; k++) {
      float dx, dy;

      dx = (float) (RAND_MAX / 2 - rand( ) ) / (float) RAND_MAX;
      x[k] -= 2.5 + 2.0 * dx;
      dy = (float) (RAND_MAX / 2 - rand( ) ) / (float) RAND_MAX;
      y[k] += 5.0 + 2.0 * dy;
   }
   k = ellipse1_c( &n, x, y, p );
   printf( "ellipse1       = %10d\n\n", k );
   printf( "Major axis     = %10.4f (   10.0000)\n", p[0] );
   printf( "Inclination    = %10.4f (   60.0000)\n", p[1] );
   printf( "X position     = %10.4f (   -2.5000)\n", p[2] );
   printf( "Y position     = %10.4f (    5.0000)\n", p[3] );
   printf( "Position Angle = %10.4f (    0.0000)\n", p[4] );
   k = ellipse2_c( &n, x, y, p, e, m );
   printf( "ellipse2       = %10d\n\n", k );
   printf( "Major axis     = %10.4f (%10.4f)\n", p[0], e[0] );
   printf( "Inclination    = %10.4f (%10.4f)\n", p[1], e[1] );
   printf( "X position     = %10.4f (%10.4f)\n", p[2], e[2] );
   printf( "Y position     = %10.4f (%10.4f)\n", p[3], e[3] );
   printf( "Position Angle = %10.4f (%10.4f)\n", p[4], e[4] );
   return( 0 );
}
#endif
