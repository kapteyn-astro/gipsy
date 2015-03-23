/* skyfit.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            skyfit.dc2

Function:     SKYFIT

Purpose:      SKYFIT is a routine for determining the sky-projection
              parameters. It takes a set of grid positions and a set
              of sky coordinates to fit the parameters.
              The method is a mixture of the steepest descent method and
              the Taylor method.

Category:     MATH

File:         skyfit.c

Author:       K.G. Begeman

Use:          INTEGER SKYFIT( X    ,    Input      DOUBLE PRECISION ARRAY
                              Y    ,    Input      DOUBLE PRECISION ARRAY
                              A    ,    Input      DOUBLE PRECISION ARRAY
                              D    ,    Input      DOUBLE PRECISION ARRAY
                              N    ,    Input      INTEGER
                              XF   ,    Output     DOUBLE PRECISION ARRAY
                              YF   ,    Output     DOUBLE PRECISION ARRAY
                              FPAR ,   In/Output   DOUBLE PRECISION ARRAY
                              EPAR ,    Output     DOUBLE PRECISION ARRAY
                              MPAR ,    Input      INTEGER ARRAY
                              TOL  ,    Input      REAL
                              ITS  ,    Input      INTEGER
                              LAB  ,    Input      REAL
                              PROJ )    Input      INTEGER

              SKYFIT   Returns number of iterations needed to achieve
                       convergence according to TOL. When this
                       number is negative, the fitting was not
                       continued because a fatal error occurred:
                       -1 Unknown projection.
                       -2 No free parameters.
                       -3 Not enough degrees of freedom.
                       -4 Maximum number of iterations too small to
                          obtain a solution which satisfies TOL.
                       -5 Diagonal of matrix contains elements which
                          are zero.
                       -6 Determinant of the coefficient matrix is zero.
                       -7 Square root of negative number.
              X        Contains x grid coordinates.
              Y        Contains y grid coordinates.
              XF       Contains fitted x grid coordinates.
              YF       Contains fitted x grid coordinates.
              A        Contains longitude coordinates.
              D        Contains latitude coordinates.
              N        Number of positions.
              FPAR     On input contains initial estimates of the
                       parameters, on output the fitted parameters.
                       There are seven parameters, so the dimension of
                       FPAR (and EPAR and MPAR) must be seven.
                       The order of the parameters is the following:
                       1: longitude projection centre
                       2: latitude projection centre
                       3: rotation angle
                       4: grid size x grid
                       5: grid size y grid
                       6: reference x grid position
                       7: reference y grid position
              EPAR     Contains estimates of errors in fitted
                       parameters.
              MPAR     Logical mask telling which parameters are
                       free (MPAR(J)=non-zero) and which parameters
                       are fixed (MPAR(J)=0).
              TOL      Relative tolerance. LSQFIT stops when
                       successive iterations fail to produce a
                       decrement in reduced chi-squared less than
                       TOL. If TOL is less than the minimum tolerance
                       possible, TOL will be set to this value. This
                       means that maximum accuracy can be obtained by
                       setting TOL=0.0.
              ITS      Maximum number of iterations.
              LAB      Mixing parameter, LAB determines the initial
                       weight of steepest descent method relative to
                       the Taylor method. LAB should be a small
                       value (i.e. 0.01).
              PROJ     Projection type.
                       2: Cylindrical projection.
                       3: Flat projection.
                       4: Gnomonic projection.
                       5: Orthographic projection.
                       6: Rectangular projection.
                       7: Global Sinusoidal projection.
                       8: North Celestial Pole projection.
                       9: Stereographic projection.

Updates:      Jul  6, 1991: KGB, Document created.
              Aug 23, 1993: VOG, Output of fitted grid coordinates.

#<

Fortran to C interface:

@ integer function skyfit( double precision,
@                          double precision,
@                          double precision,
@                          double precision,
@                          integer,
@                          double precision,
@                          double precision,
@                          double precision,
@                          double precision,
@                          integer,
@                          real,
@                          integer,
@                          real,
@                          integer )

*/

#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"math.h"			/* <math.h> */
#include	"float.h"			/* <float.h> */
#include	"gipsyc.h"			/* GIPSY definitions */

#define	LABFAC	10.0				/* labda step factor */
#define	LABMAX	1.0e+10				/* maximum value for labda */
#define	LABMIN	1.0e-10				/* minimum value for labda */
#define	MAXPAR	7				/* number of free parameters */

#define degrad(x)	(57.2957795130823208768*(x))	/* radians -> degrees */
#define raddeg(x)	( 0.0174532925199432958*(x))	/* degrees -> radians */

static	double	chi1;				/* old reduced chi-squared */
static	double	chi2;				/* new reduced chi-squared */
static	double	labda;				/* mixing parameter */
static	double	tolerance;			/* accuracy */
static	double	vector[MAXPAR];			/* correction vector */
static	double	matrix1[MAXPAR][MAXPAR];	/* original matrix */
static	double	matrix2[MAXPAR][MAXPAR];	/* inverse of matrix1 */
static	fint	itc;				/* fate of fit */
static	fint	found;				/* solution found ? */
static	fint	nfree;				/* number of free parameters */
static	fint	parptr[MAXPAR];			/* parameter pointer */

static	void	sky( double	*x    ,		/* x grid */
                     double	*y    ,		/* y grid */
                     double     a     ,		/* longitude */
                     double     d     ,		/* latitude */
                     double	*par  ,		/* parameters */
                     fint	*proj )		/* projection type */
/*
 * The function sky calculates the grids from the input longitude and latitude
 * and the projection parameters.
 */
{
   double	a0  = par[0];			/* longitude projection centre */
   double	d0  = par[1];			/* latitude projection centre */
   double	rho = par[2];			/* rotation angle */
   double	dx  = par[3];			/* x grid size */
   double	dy  = par[4];			/* y grid size */
   double	x0  = par[5];			/* x reference grid */
   double	y0  = par[6];			/* y reference grid */
   double	L = 0;
   double	M = 0;

   switch( *proj ) {				/* which projection */
      case 2: {					/* Cylindrical projection */
         L = a - a0;
         M = sin(d-d0);
         break;
      }
      case 3: {					/* Flat projection */
         L = a - a0;
         M = d - d0;
         break;
      }
      case 4: {					/* Gnomonic projection */
         double	t;

         t = sin(d) * sin(d0) + cos(d) * cos(d0) * cos(a-a0);
         L = cos(d) * sin(a-a0) / t;
         M = ( sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0) ) / t;
         break;
      }
      case 5: {					/* Orthographic projection */
         L = cos(d) * sin(a-a0);
         M = sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0);
         break;
      }
      case 6: {					/* Rectangular projection */
         double	s, t;

         s = sin(d) * sin(d0) + cos(d) * cos(d0) * cos(a-a0);
         if (s > 1.0) s = 1.0; else if (s < -1.0) s = -1.0;
         t = acos(s);
         L = t / sin(t) * cos(d) * sin(a-a0);
         M = t / sin(t) * ( sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0) );
         break;
      }
      case 7: {					/* Global Sinusoidal projection */
         L = ( a - a0 ) * cos( d );
         M = ( d - d0 );
         break;
      }
      case 8: {					/* North Celestial Pole projection */
         L = cos(d) * sin(a-a0);
         M = ( cos(d0) - cos(d) * cos(a-a0) ) / sin(d0);         
         break;
      }
      case 9: {					/* Stereographic projection */
         double	t;

         t = 1.0 + sin(d) * sin(d0) + cos(d) * cos(d0) * cos(a-a0);
         L = 2.0 * sin(d) * sin(a-a0) / t;
         M = 2.0 * ( sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0) ) / t;
         break;
      }
      default: {				/* we should never get here */
         break;
      }
   }
   *x = x0 + ( L * cos(rho) + M * sin(rho) ) / dx;
   *y = y0 + ( M * cos(rho) - L * sin(rho) ) / dy;
}

static	void	der( double	a     ,		/* longitude */
                     double	d     ,		/* latitude */
                     double	*par  ,		/* parameters */
                     double	*xder ,		/* partial x derivatives */
                     double	*yder ,		/* partial y derivatives */
                     fint	*proj )		/* projection */
/*
 * The function der calculates the partial derivatives to the parameters
 * for a given set of sky coordinates.
 */
{
   double	a0  = par[0];			/* longitude projection centre */
   double	d0  = par[1];			/* latitude projection centre */
   double	rho = par[2];			/* rotation angle */
   double	dx  = par[3];			/* x grid size */
   double	dy  = par[4];			/* y grid size */
   double	L = 0.0;
   double	M = 0.0;
   double	dLda0 = 0.0;
   double	dLdd0 = 0.0;
   double	dMda0 = 0.0;
   double	dMdd0 = 0.0;

   switch( *proj ) {				/* which projection */
      case 2: {					/* Cylindrical projection */
         L = a - a0;
         M = sin(d-d0);
         dLda0 = -1.0;
         dMda0 =  0.0;
         dLdd0 =  0.0;
         dMdd0 = -cos(d-d0);
         break;
      }
      case 3: {					/* Flat projection */
         L = a - a0;
         M = d - d0;
         dLda0 = -1.0;
         dMda0 =  0.0;
         dLdd0 =  0.0;
         dMdd0 = -1.0;
         break;
      }
      case 4: {					/* Gnomonic projection */
         double	t;

         t = sin(d) * sin(d0) + cos(d) * cos(d0) * cos(a-a0);
         L = cos(d) * sin(a-a0) / t;
         M = ( sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0) ) / t;
         dLda0 = -( cos(d) * sin(d) * sin(d0) * cos(a-a0) + cos(d) * cos(d) * cos(d0) ) / t / t;
         dMda0 = -( sin(d) * cos(d) * sin(a-a0) ) / t / t;
         dLdd0 = -( cos(d) * sin(d) * cos(d0) * sin(a-a0) - cos(d) * cos(d) * sin(d0) * sin(a-a0) * cos(a-a0) ) / t / t;
         dMdd0 = -( sin(d) * sin(d) + cos(d) * cos(d) * cos(a-a0) * cos(a-a0) ) / t / t;
         break;
      }
      case 5: {					/* Orthographic projection */
         L = cos(d) * sin(a-a0);
         M = sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0);
         dLda0 = -cos(d) * cos(a-a0);
         dMda0 = -cos(d) * sin(d0) * sin(a-a0);
         dLdd0 = 0.0;
         dMdd0 = -sin(d) * sin(d0) - cos(d) * cos(d0) * cos(a-a0);
         break;
      }
      case 6: {					/* Rectangular projection */
         double	cost, sint, t;
         double	dtda0, dtdd0;

         cost = sin(d) * sin(d0) + cos(d) * cos(d0) * cos(a-a0);
         if (cost > 1.0) cost = 1.0; else if (cost < -1.0) cost = -1.0;
         t = acos(cost);
         sint = sin(t);
         L = t / sint * cos(d) * sin(a-a0);
         M = t / sint * ( sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0) );
         dtda0 = -cos(d) * cos(d0) * sin(a-a0) / sint;
         dtdd0 = -( sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0) ) / sint;
         dLda0 = - t / sint * cos(d) * cos(a-a0) + cos(d) * sin(a-a0) * ( sint - t * cost ) / sint / sint * dtda0;
         dMda0 = - t / sint * cos(d) * sin(d0) * sin(a-a0) + ( sin(d0) * cos(d0) - cos(d) * sin(d0) * cos(a-a0) ) * ( sint - t * cost ) / sint / sint * dtda0;
         dLdd0 = cos(d) * sin(a-a0) * ( sint - t * cost ) / sint / sint * dtdd0;
         dMdd0 = - t / sint * ( sin(d) * sin(d0) + cos(d) * cos(d0) * cos(a-a0) ) + ( sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0) ) * ( sint - t * cost ) / sint / sint * dtdd0;
         break;
      }
      case 7: {					/* Global Sinusoidal projection */
         L = ( a - a0 ) * cos(d);
         M = ( d - d0 );
         dLda0 = -1.0 * cos(d);
         dMda0 =  0.0;
         dLdd0 =  0.0;
         dMdd0 = -1.0;
         break;
      }
      case 8: {					/* North Celestial Pole projection */
         L = cos(d) * sin(a-a0);
         M = ( cos(d0) - cos(d) * cos(a-a0) ) / sin(d0);
         dLda0 = -cos(d) * cos(a-a0);
         dMda0 = -cos(d) * sin(a-a0) / sin(d0);
         dLdd0 = 0.0;
         dMdd0 = ( cos(d) * cos(d0) * cos(a-a0) - 1.0 ) / sin(d0) / sin(d0);
         break;
      }
      case 9: {					/* Stereographic projection */
         double	t, dtda0, dtdd0;

         t = 1.0 + sin(d) * sin(d0) + cos(d) * cos(d0) * cos(a-a0);
         L = 2.0 * sin(d) * sin(a-a0) / t;
         M = 2.0 * ( sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0) ) / t;
         dtda0 =                    cos(d) * cos(d0) * sin(a-a0);
         dtdd0 = sin(d) * cos(d0) - cos(d) * sin(d0) * cos(a-a0);
         dLda0 = -2.0 * sin(d) * cos(a-a0) / t - L * dtda0 / t;
         dMda0 = - cos(d) * sin(d0) * sin(a-a0) / t - M * dtda0 / t;
         dLdd0 = - L * dtdd0 / t;
         dMdd0 =  2.0 * ( 1.0 - t ) / t - M * dtdd0 / t;
         break;
      }
      default: {				/* we should never get here */
         break;
      }
   }
   xder[0] = dLda0 * cos(rho) / dx + dMda0 * sin(rho) / dx;
   yder[0] = dMda0 * cos(rho) / dy - dLda0 * sin(rho) / dy;
   xder[1] = dLdd0 * cos(rho) / dx + dMdd0 * sin(rho) / dx;
   yder[1] = dMdd0 * cos(rho) / dy - dLdd0 * sin(rho) / dy;
   xder[2] =  ( M * cos(rho) - L * sin(rho) ) / dx;
   yder[2] = -( L * cos(rho) + M * sin(rho) ) / dy;
   xder[3] = -( L * cos(rho) + M * sin(rho) ) / dx / dx;
   yder[3] = 0.0;
   xder[4] = 0.0;
   yder[4] = -( M * cos(rho) - L * sin(rho) ) / dy / dy;
   xder[5] = 1.0;
   yder[5] = 0.0;
   xder[6] = 0.0;
   yder[6] = 1.0;
}

   
static fint invmat( void )
/*
 * invmat calculates the inverse of matrix2. The algorithm used is the
 * Gauss-Jordan algorithm described in Stoer, Numerische matematik, 1 Teil.
 */
{
   double even;
   double hv[MAXPAR];
   double mjk;
   double rowmax;
   fint   evin;
   fint   i;
   fint   j;
   fint   k;
   fint   per[MAXPAR];
   fint   row;

   for (i = 0; i < nfree; i++) per[i] = i;	/* set permutation array */
   for (j = 0; j < nfree; j++) {		/* in j-th column, ... */
      rowmax = fabs( matrix2[j][j] );		/* determine row with ... */
      row = j;					/* largest element. */
      for (i = j + 1; i < nfree; i++) {
         if (fabs( matrix2[i][j] ) > rowmax) {
            rowmax = fabs( matrix2[i][j] );
            row = i;
         }
      }
      if (matrix2[row][j] == 0.0) return( -6 );	/* determinant is zero! */
      if (row > j) {				/* if largest element not ... */
         for (k = 0; k < nfree; k++) {		/* on diagonal, then ... */
            even = matrix2[j][k];		/* permutate rows. */
            matrix2[j][k] = matrix2[row][k];
            matrix2[row][k] = even;
         }
         evin = per[j];				/* keep track of permutation */
         per[j] = per[row];
         per[row] = evin;
      }
      even = 1.0 / matrix2[j][j];		/* modify column */
      for (i = 0; i < nfree; i++) matrix2[i][j] *= even;
      matrix2[j][j] = even;
      for (k = 0; k < j; k++) {
         mjk = matrix2[j][k];
         for (i = 0; i < j; i++) matrix2[i][k] -= matrix2[i][j] * mjk;
         for (i = j + 1; i < nfree; i++) matrix2[i][k] -= matrix2[i][j] * mjk;
         matrix2[j][k] = -even * mjk;
      }
      for (k = j + 1; k < nfree; k++) {
         mjk = matrix2[j][k];
         for (i = 0; i < j; i++) matrix2[i][k] -= matrix2[i][j] * mjk;
         for (i = j + 1; i < nfree; i++) matrix2[i][k] -= matrix2[i][j] * mjk;
         matrix2[j][k] = -even * mjk;
      }
   }
   for (i = 0; i < nfree; i++) {		/* finally, repermute the ... */
      for (k = 0; k < nfree; k++) {		/* columns. */
         hv[per[k]] = matrix2[i][k];
      }
      for (k = 0; k < nfree; k++) {
         matrix2[i][k] = hv[k];
      }
   }
   return( 0 );					/* all is well */
}

static void getmat( double	*x    ,		/* x grids */
                    double	*y    ,		/* y grids */
                    double	*a    ,		/* longitudes */
                    double	*d    ,		/* latitudes */
                    fint	*npos ,		/* number of coordinates */
                    double	*fpar ,		/* the parameters */
                    fint	*proj )		/* the projection */
/*
 * getmat builds the matrix.
 */
{
   double	fx;
   double	fy;
   double xd;
   double	xder[MAXPAR];
   double yd;
   double	yder[MAXPAR];
   fint   i;
   fint   j;
   fint   n;

   for (j = 0; j < nfree; j++) {
      vector[j] = 0.0;				/* zero vector ... */
      for (i = 0; i <= j; i++) {		/* and matrix ... */
         matrix1[j][i] = 0.0;			/* only on and below diagonal */
      }
   }
   chi2 = 0.0;					/* reset reduced chi-squared */
   for (n = 0; n < (*npos); n++) {		/* loop trough data points */
      double	an = raddeg( a[n] );
      double	dn = raddeg( d[n] );
      double	xn, yn;

      sky( &xn, &yn, an, dn, fpar, proj );	/* calculate the grids */
      der( an, dn, fpar, xder, yder, proj );	/* and the derivatives */
      xd = x[n] - xn;				/* the x difference */
      yd = y[n] - yn;				/* the y difference */
      chi2 += ( xd * xd + yd * yd );		/* add to chi-squared */
      for (j = 0; j < nfree; j++) {
         fx = xder[parptr[j]];			/* x derivative */
         fy = yder[parptr[j]];			/* y derivative */
         vector[j] += ( xd * fx + yd * fy );	/* fill vector */
         for (i = 0; i <= j; i++) {		/* fill matrix */
            matrix1[j][i] += ( xder[parptr[i]] * fx + yder[parptr[i]] * fy );
         }
      }
   }
}

static fint getvec( double	*x    ,		/* x grids */
                    double	*y    ,		/* y grids */
                    double	*a    ,		/* longitudes */
                    double	*d    ,		/* latitudes */
                    fint	*npos ,		/* number of positions */
                    double	*fpar ,		/* input parameters */
                    double	*epar ,		/* output parameters */
                    fint	*proj )		/* projection */
/*
 * getvec calculates the correction vector. The matrix has been built by
 * getmat, we only have to rescale it for the current value for labda.
 * The matrix is rescaled so that the diagonal gets the value 1 + labda.
 * Next we calculate the inverse of the matrix and then the correction
 * vector.
 */
{
   double	dj;
   double	dx;
   double	dy;
   double	mii;
   double	mji;
   double	mjj;
   fint		i;
   fint		j;
   fint		n;
   fint		r;

   for (j = 0; j < nfree; j++) {		/* loop to modify and ... */
      mjj = matrix1[j][j];			/* scale the matrix */
      if (mjj <= 0.0) return( -5 );		/* diagonal element wrong! */
      mjj = sqrt( mjj );
      for (i = 0; i < j; i++) {			/* scale it */
         mji = matrix1[j][i] / mjj / sqrt( matrix1[i][i] );
         matrix2[i][j] = matrix2[j][i] = mji;
      }
      matrix2[j][j] = 1.0 + labda;		/* scaled value on diagonal */
   }
   if (r = invmat( )) return( r );		/* invert matrix inplace */
   for (i = 0; i < MAXPAR; i++) epar[i] = fpar[i];
   for (j = 0; j < nfree; j++) {		/* loop to calculate ... */
      dj = 0.0;					/* correction vector */
      mjj = matrix1[j][j];
      if (mjj <= 0.0) return( -7 );		/* not allowed! */
      mjj = sqrt( mjj );
      for (i = 0; i < nfree; i++) {
         mii = matrix1[i][i];
         if (mii <= 0.0) return( -7 );
         mii = sqrt( mii );
         dj += vector[i] * matrix2[j][i] / mjj / mii;
      }
      epar[parptr[j]] += dj;			/* new parameters */
   }
   chi1 = 0.0;					/* reset reduced chi-squared */
   for (n = 0; n < (*npos); n++) {		/* loop through data points */
      double	an = raddeg( a[n] );
      double	dn = raddeg( d[n] );
      double	xn, yn;

      sky( &xn, &yn, an, dn, epar, proj );	/* calculate the grids */
      dx = x[n] - xn;				/* the x difference */
      dy = y[n] - yn;				/* the y difference */
      chi1 += ( dx * dx + dy * dy );
   }
   return( 0 );
}

fint skyfit_c( double	*x    ,			/* x grids */
               double	*y    ,			/* y grids */
               double	*a    ,			/* longitudes */
               double	*d    ,			/* latitudes */
               fint	*npos ,			/* number of positions */
               double	*xf   ,			/* fitted x grids */
               double	*yf   ,			/* fitted y grids */               
               double	*fpar ,			/* the parameters */
               double	*epar ,			/* the errors */
               fint	*mpar ,			/* the mask */
               float	*tol  ,			/* the tolerance */
               fint	*its  ,			/* the number of iterations */
               float	*lab  ,			/* the mixing parameter */
               fint	*proj )			/* the projection */
/*
 * skyfit is exported, and callable from C as well as Fortran.
 */
{
   double	rpar[MAXPAR];		/* parameters in radians */
   fint		i;
   fint		r;

   itc = 0;				/* fate of fit */
   found = 0;				/* reset */
   nfree = 0;				/* number of free parameters */
   if (*tol < (FLT_EPSILON * 10.0)) {
      tolerance = FLT_EPSILON * 10.0;	/* default tolerance */
   } else {
      tolerance = *tol;			/* tolerance */
   }
   labda = fabs( *lab ) * LABFAC;	/* start value for mixing parameter */
   if (*proj < 1 || *proj > 9) {	/* illegal projection */
      return( -1 );
   }
   for (i = 0; i < MAXPAR; i++) {
      if (mpar[i]) {
         parptr[nfree++] = i;		/* a free parameter */
      }
   }
   if (nfree == 0) return( -2 );	/* no free parameters */
   if (nfree >= (2*(*npos))) return( -3 );	/* no degrees of freedom */
   for (i = 0; i < (MAXPAR - 2); i++) {
      rpar[i] = raddeg( fpar[i] );
   }
   for (; i < MAXPAR; i++) {
      rpar[i] = fpar[i];
   }
   /*
    * The non-linear fit uses the steepest descent method in combination
    * with the Taylor method. The mixing of these methods is controlled
    * by labda. In the outer loop (called the iteration loop) we build
    * the matrix and calculate the correction vector. In the inner loop
    * (called the interpolation loop) we check whether we have obtained
    * a better solution than the previous one. If so, we leave the
    * inner loop, else we increase labda (give more weight to the
    * steepest descent method), calculate the correction vector and check
    * again. After the inner loop we do a final check on the goodness of
    * the fit and if this satisfies the tolerance we calculate the
    * errors of the fitted parameters.
    */
   while (!found) {				/* iteration loop */
      if (itc++ == (*its)) return( -4 );	/* increase iteration counter */
      getmat( x, y, a, d, npos, rpar, proj );
      /*
       * here we decrease labda since we may assume that each iteration
       * brings us closer to the answer.
       */
      if (labda > LABMIN) labda /= LABFAC;	/* decrease labda */
      r = getvec( x, y, a, d, npos, rpar, epar, proj );
      if (r) return( r );			/* error */
      while (chi1 >= chi2) {			/* interpolation loop */
         /*
          * The next statement is based on experience, not on the
          * mathematics of the problem although I (KGB) think that it
          * is correct to assume that we have reached convergence
          * when the pure steepest descent method does not produce
          * a better solution. Think about this somewhat more, anyway,
          * as already stated, the next statement is based on experience.
          */
         if (labda > LABMAX) break;		/* assume solution found */
         labda *= LABFAC;			/* Increase mixing parameter */
         r = getvec( x, y, a, d, npos, rpar, epar, proj );
         if (r) return( r );			/* error */
      }
      if (labda <= LABMAX) {			/* save old parameters */
         for (i = 0; i < MAXPAR; i++) rpar[i] = epar[i];
      }
      if (fabs( chi2 - chi1 ) <= (tolerance * chi1) || (labda > LABMAX)) {
         /*
          * We have a satisfying solution, so now we need to calculate
          * the correct errors of the fitted parameters. This we do
          * by using the pure Taylor method because we are very close
          * to the real solution.
          */
         labda = 0.0;				/* for Taylor solution */
         getmat( x, y, a, d, npos, rpar, proj );
         r = getvec( x, y, a, d, npos, rpar, epar, proj );
         if (r) return( r );			/* error */
         for (i = 0; i < MAXPAR; i++) {
            epar[i] = 0.0;			/* and set error to zero */
         }
         chi2 = sqrt( chi2 / (double) (( 2 * (*npos) ) - nfree) );
         for (i = 0; i < nfree; i++) {
            if ((matrix1[i][i] <= 0.0) || (matrix2[i][i] <= 0.0)) return( -7);
            epar[parptr[i]] = chi2 * sqrt( matrix2[i][i] ) / sqrt( matrix1[i][i] );
         }
         found = 1;				/* we found a solution */
      }
   }
   for (i = 0; i < (MAXPAR - 2); i++) {
      fpar[i] = degrad( rpar[i] );
      epar[i] = degrad( epar[i] );
   }
   for (; i < MAXPAR; i++) {
      fpar[i] = rpar[i];
   }
   

   /* Calculate the fitted grids */
   {
      double    radpars[MAXPAR];
      double    af, df;     
      /* Copy projection parameters forr use in 'sky' */
      for ( i = 0; i < MAXPAR - 2; i++) radpars[i] = raddeg( fpar[i] );
      for (; i < MAXPAR; i++) radpars[i] = rpar[i];
      /* Update fitted grids */
      for (i = 0; i < (*npos); i++) {           
         af = raddeg( a[i] );
         df = raddeg( d[i] );
         sky( &xf[i], &yf[i], af, df, radpars, proj );
      }
   }
   
   
   return( itc );				/* return number of iterations */
}

#if	defined(TESTBED)

#include	"time.h"		/*  <time.h> */

#define	MAXPOS	100			/* the number of positions */

int	main( int argc, char **argv )
{
#if	1
   FILE		*f;
   double	a[MAXPOS];
   double	d[MAXPOS];
   double	x[MAXPOS];
   double	y[MAXPOS];
   double	xf[MAXPOS];
   double	yf[MAXPOS];   
   double	fpar[MAXPAR];
   double	epar[MAXPAR];
   fint		mpar[MAXPAR];
   fint		proj;
   float	tol = 0.00001;
   float	lab = 0.01;
   fint		its = 100;
   fint		r;
   fint		n;
   fint		npos = 0;

   f = fopen( argv[1], "r" );
   fscanf( f, "%d", &proj );
   for ( n = 0; n < MAXPAR; n++ ) {
      fscanf( f, "%lf %d", &fpar[n], &mpar[n] );
   }
   while (fscanf( f, "%lf %lf %lf %lf", &x[npos], &y[npos], &a[npos], &d[npos] ) == 4) {
      npos += 1;
   }
   r = skyfit_c( x, y, a, d, &npos, xf, yf, fpar, epar, mpar, &tol, &its, &lab, &proj );
   printf( "skyfit = %d\n", r );
   for (n = 0; n < MAXPAR; n++) {
      printf( "out = %18.10g  err = %18.11g\n", fpar[n], epar[n] );
   }
   for ( n = 0; n < MAXPAR - 2; n++) {
      fpar[n] = raddeg( fpar[n] );
   }
   for (n = 0; n < npos; n++ ) {
      double	xgf, ygf;
      double	af, df;

      af = raddeg( a[n] );
      df = raddeg( d[n] );      
      sky( &xgf, &ygf, af, df, fpar, &proj );
      printf( "diff-x = %18.10g diff-y = %18.10g\n", xgf - x[n], ygf - y[n] );
      printf( "diff-x = %18.10g diff-y = %18.10g\n", xf[n] - x[n], yf[n] - y[n] );      
   }
#else
   double	a[MAXPOS], d[MAXPOS], x[MAXPOS], y[MAXPOS];
   double	xf[MAXPOS];
   double	yf[MAXPOS];      
   double	fpar[MAXPAR], epar[MAXPAR], spar[MAXPAR], qpar[MAXPAR];
   fint		mpar[MAXPAR];
   fint		r;
   fint		n;
   fint		its = 100;
   fint		proj = atoi( argv[argc-1] );
   float	tol = 0.00001;
   float	lab = 0.01;

   for (n = 0; n < MAXPAR; mpar[n++] = 1);
   mpar[2] = 0; mpar[5] = 0; mpar[6] = 0;
   srand( time( NULL ) );
   spar[0] = ((double) (rand( )) / (double) RAND_MAX ) * 360.0;
   spar[1] = ((double) (rand( )) / (double) RAND_MAX ) * 90.0;
   spar[2] = ((double) (rand( )) / (double) RAND_MAX ) * 360.0;
   spar[3] = -0.01;
   spar[4] =  0.01;
   spar[5] =  0.0;
   spar[6] =  0.0;
   for (n = 0; n < MAXPOS; n++) {
      a[n] = spar[0] + ((double) (rand( ) - RAND_MAX / 2) / (double) RAND_MAX) * 2.0;
      d[n] = spar[1] + ((double) (rand( ) - RAND_MAX / 2) / (double) RAND_MAX) * 2.0;
   }
   for (n = 0; n < MAXPOS; n++) {
      a[n] = raddeg( a[n] );
      d[n] = raddeg( d[n] );
   }
   for (n = 0; n < (MAXPAR - 2); n++) {
      spar[n] = raddeg( spar[n] );
   }
   fpar[0] = 0.0; fpar[1] = 0.0; fpar[5] = 0.0; fpar[6] = 0.0;
   for (n = 0; n <  MAXPOS; n++) {
      sky( &x[n], &y[n], a[n], d[n], spar, &proj );
      fpar[0] += (a[n] / MAXPOS);
      fpar[1] += (d[n] / MAXPOS);
      fpar[5] += (x[n] / MAXPOS);
      fpar[6] += (y[n] / MAXPOS);
   }
   for (n = 0; n < MAXPOS; n++) {
      a[n] = degrad( a[n] );
      d[n] = degrad( d[n] );
   }
   for (n = 0; n < (MAXPAR - 2); n++) {
      spar[n] = degrad( spar[n] );
   }
   fpar[0] = degrad( fpar[0] );
   fpar[1] = degrad( fpar[1] );
   fpar[2] = /* 45.0 */ spar[2];
   fpar[3] = -0.04;
   fpar[4] =  0.04;
   fpar[5] =  0.0;
   fpar[6] =  0.0; 
   for (n = 0; n < MAXPAR; n++) {
      qpar[n] = fpar[n];
   }
   n =  MAXPOS;
   r = skyfit_c( x, y, a, d, &n, xf, yf, fpar, epar, mpar, &tol, &its, &lab, &proj );
   printf( "skyfit = %d\n", r );
   for (n = 0; n < MAXPAR; n++) {
      printf( "real = %12.5g  in = %12.5g  out = %12.5g  err = %12.5g\n", spar[n], qpar[n], fpar[n], epar[n] );
   }
#endif
   return( 0 );
}
#endif
