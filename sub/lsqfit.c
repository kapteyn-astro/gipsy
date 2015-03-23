/* lsqfit.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            lsqfit.dc2

Function:     LSQFIT

Purpose:      LSQFIT is a routine for making a least-squares fit of a
              function to a set of data points. The method used is
              described in: Marquardt, J.Soc.Ind.Appl.Math. 11, 431 (1963).
              This  method is a mixture of the steepest descent method and
              the Taylor method.

Category:     MATH

File:         lsqfit.c

Author:       K.G. Begeman

Use:          INTEGER LSQFIT( XDAT ,    Input      REAL ARRAY
                              XDIM ,    Input      INTEGER
                              YDAT ,    Input      REAL ARRAY
                              WDAT ,    Input      REAL ARRAY
                              NDAT ,    Input      INTEGER
                              FPAR ,   In/Output   REAL ARRAY
                              EPAR ,    Output     REAL ARRAY
                              MPAR ,    Input      INTEGER ARRAY
                              NPAR ,    Input      INTEGER
                              TOL  ,    Input      REAL
                              ITS  ,    Input      INTEGER
                              LAB  ,    Input      REAL
                              FOPT )    Input      INTEGER

              LSQFIT   Returns number of iterations needed to achieve
                       convergence according to TOL. When this
                       number is negative, the fitting was not
                       continued because a fatal error occurred:
                       -1 Too many free parameters, maximum is 32.
                       -2 No free parameters.
                       -3 Not enough degrees of freedom.
                       -4 Maximum number of iterations too small to
                          obtain a solution which satisfies TOL.
                       -5 Diagonal of matrix contains elements which
                          are zero.
                       -6 Determinant of the coefficient matrix is zero.
                       -7 Square root of negative number.
              XDAT     Contains coordinates of data points.
                       XDAT is two-dimensional: XDAT(XDIM,NDAT)
              XDIM     Dimension of fit.
              YDAT     Contains data points.
              WDAT     Contains weigths for data points.
              NDAT     Number of data points.
              FPAR     On input contains initial estimates of the
                       parameters for non-linear fits, on output the
                       fitted parameters.
              EPAR     Contains estimates of errors in fitted
                       parameters.
              MPAR     Logical mask telling which parameters are
                       free (MPAR(J)=non-zero) and which parameters
                       are fixed (MPAR(J)=0).
              NPAR     Number of parameters (free+fixed).
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
                       value (i.e. 0.01). LAB can only be zero when
                       the partial derivatives are independent of
                       the parameters. In fact in this case LAB
                       should be exactly equal to zero.
              FOPT     A value which is passed unmodified to FUNC
                       and DERV (see below).

Notes:        The following routines have to be defined by the user:
              
              REAL FUNC( XDAT ,   Input    REAL ARRAY
                         FPAR ,   Input    REAL ARRAY
                         NPAR ,   Input    INTEGER
                         FOPT )   Input    INTEGER

              FUNC    Returns the function value of the function to
                      be fitted.
              XDAT    Coordinate(s) of data point.
              FPAR    Parameter list.
              NPAR    Number of parameters.
              FOPT    A user defined option.
              
              CALL DERV( XDAT ,   Input    REAL ARRAY
                         FPAR ,   Input    REAL ARRAY
                         DPAR ,   Output   REAL ARRAY
                         NPAR ,   Input    INTEGER
                         FOPT )   Input    INTEGER

              XDAT    Coordinate(s) of data point.
              FPAR    Parameter list.
              EPAR    Partial derivatives to the parameters of the
                      function to be fitted.
              NPAR    Number of parameters.
              FOPT    A user defined option.

              In FORTRAN applications you need to specify the
              following f2cvv syntax for the C to Fortran
              interface somewhere in your source code:
              
              C@ real function func( real, real, integer, integer )
              C@ subroutine derv( real, real, real, integer, integer )
              
Example:      Fitting y(x) = a + b * x 

              REAL FUNCTION FUNC( XDAT, FPAR, NPAR, FOPT )
              ...
              FUNC = FPAR(1) + FPAR(2) * XDAT
              RETURN
              END
              
              SUBROUTINE DERV( XDAT, FPAR, DPAR, NPAR, FOPT )
              ...
              DPAR(1) = 1.0
              DPAR(2) = XDAT
              RETURN
              END

Updates:      May  7, 1990: KGB, Document created.
              May 14, 1990: MXV, Document refereed.

#<

Fortran to C interface:

@integer function lsqfit( real, integer, real, real, integer, real, real,
@                         integer, integer, real, integer, real, integer )

*/

#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"math.h"			/* <math.h> */
#include	"float.h"			/* <float.h> */
#include	"gipsyc.h"			/* GIPSY definitions */

#define	LABFAC	10.0				/* labda step factor */
#define	LABMAX	1.0e+10				/* maximum value for labda */
#define	LABMIN	1.0e-10				/* minimum value for labda */
#define	MAXPAR	32				/* number of free parameters */

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
static	fint	nuse;				/* number of useable data points */
static	fint	parptr[MAXPAR];			/* parameter pointer */

extern float func_c( float *xdat ,		/* returns function value */
                     float *fpar ,
                     fint  *npar ,
                     fint  *fopt );
extern void  derv_c( float *xdat ,		/* returns derivatives */
                     float *fpar ,
                     float *epar ,
                     fint  *npar ,
                     fint  *fopt );

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

static void getmat( float *xdat ,
                    fint  *xdim ,
                    float *ydat ,
                    float *wdat ,
                    fint  *ndat ,
                    float *fpar ,
                    float *epar ,
                    fint  *npar ,
                    fint  *fopt )
/*
 * getmat builds the matrix.
 */
{
   double wd;
   double wn;
   double yd;
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
   for (n = 0; n < (*ndat); n++) {		/* loop trough data points */
      wn = wdat[n];
      if (wn > 0.0) {				/* legal weight ? */
         yd = ydat[n] - func_c( &xdat[(*xdim) * n], fpar, npar, fopt );
         derv_c( &xdat[(*xdim) * n], fpar, epar, npar, fopt );
         chi2 += yd * yd * wn;			/* add to chi-squared */
         for (j = 0; j < nfree; j++) {
            wd = epar[parptr[j]] * wn;		/* weighted derivative */
            vector[j] += yd * wd;		/* fill vector */
            for (i = 0; i <= j; i++) {		/* fill matrix */
               matrix1[j][i] += epar[parptr[i]] * wd;
            }
         }
      }
   }
}

static fint getvec( float *xdat ,
                    fint  *xdim ,
                    float *ydat ,
                    float *wdat ,
                    fint  *ndat ,
                    float *fpar ,
                    float *epar ,
                    fint  *npar ,
                    fint  *fopt )
/*
 * getvec calculates the correction vector. The matrix has been built by
 * getmat, we only have to rescale it for the current value for labda.
 * The matrix is rescaled so that the diagonal gets the value 1 + labda.
 * Next we calculate the inverse of the matrix and then the correction
 * vector.
 */
{
   double dj;
   double dy;
   double mii;
   double mji;
   double mjj;
   double wn;
   fint   i;
   fint   j;
   fint   n;
   fint   r;

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
   if (r = invmat( )) return( r );		/* invert matrix inlace */
   for (i = 0; i < (*npar); i++) epar[i] = fpar[i];
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
   for (n = 0; n < (*ndat); n++) {		/* loop through data points */
      wn = wdat[n];				/* get weight */
      if (wn > 0.0) {				/* legal weight */
         dy = ydat[n] - func_c( &xdat[(*xdim) * n], epar, npar, fopt );
         chi1 += wdat[n] * dy * dy;
      }
   }
   return( 0 );
}

fint lsqfit_c( float *xdat ,
               fint  *xdim ,
               float *ydat ,
               float *wdat ,
               fint  *ndat ,
               float *fpar ,
               float *epar ,
               fint  *mpar ,
               fint  *npar ,
               float *tol  ,
               fint  *its  ,
               float *lab  ,
               fint  *fopt )
/*
 * lsqfit is exported, and callable from C as well as Fortran.
 */
{
   fint   i;
   fint   n;
   fint   r;

   itc = 0;				/* fate of fit */
   found = 0;				/* reset */
   nfree = 0;				/* number of free parameters */
   nuse = 0;				/* number of legal data points */
   if (*tol < (FLT_EPSILON * 10.0)) {
      tolerance = FLT_EPSILON * 10.0;	/* default tolerance */
   } else {
      tolerance = *tol;			/* tolerance */
   }
   labda = fabs( *lab ) * LABFAC;	/* start value for mixing parameter */
   for (i = 0; i < (*npar); i++) {
      if (mpar[i]) {
         if (nfree > MAXPAR) return( -1 );	/* too many free parameters */
         parptr[nfree++] = i;		/* a free parameter */
      }
   }
   if (nfree == 0) return( -2 );	/* no free parameters */
   for (n = 0; n < (*ndat); n++) {
      if (wdat[n] > 0.0) nuse++;	/* legal weight */
   }
   if (nfree >= nuse) return( -3 );	/* no degrees of freedom */
   if (labda == 0.0) {			/* linear fit */
      for (i = 0; i < nfree; fpar[parptr[i++]] = 0.0);
      getmat( xdat, xdim, ydat, wdat, ndat, fpar, epar, npar, fopt );
      r = getvec( xdat, xdim, ydat, wdat, ndat, fpar, epar, npar, fopt );
      if (r) return( r );		/* error */
      for (i = 0; i < (*npar); i++) {
         fpar[i] = epar[i];		/* save new parameters */
         epar[i] = 0.0;			/* and set errors to zero */
      }
      chi1 = sqrt( chi1 / (double) (nuse - nfree) );
      for (i = 0; i < nfree; i++) {
         if ((matrix1[i][i] <= 0.0) || (matrix2[i][i] <= 0.0)) return( -7 );
         epar[parptr[i]] = chi1 * sqrt( matrix2[i][i] ) / sqrt( matrix1[i][i] );
      }
   } else {				/* Non-linear fit */
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
         getmat( xdat, xdim, ydat, wdat, ndat, fpar, epar, npar, fopt );
         /*
          * here we decrease labda since we may assume that each iteration
          * brings us closer to the answer.
          */
         if (labda > LABMIN) labda /= LABFAC;	/* decrease labda */
         r = getvec( xdat, xdim, ydat, wdat, ndat, fpar, epar, npar, fopt );
         if (r) return( r );		/* error */
         while (chi1 >= chi2) {		/* interpolation loop */
            /*
             * The next statement is based on experience, not on the
             * mathematics of the problem although I (KGB) think that it
             * is correct to assume that we have reached convergence
             * when the pure steepest descent method does not produce
             * a better solution. Think about this somewhat more, anyway,
             * as already stated, the next statement is based on experience.
             */
            if (labda > LABMAX) break;	/* assume solution found */
            labda *= LABFAC;		/* Increase mixing parameter */
            r = getvec( xdat, xdim, ydat, wdat, ndat, fpar, epar, npar, fopt );
            if (r) return( r );		/* error */
         }
         if (labda <= LABMAX) {		/* save old parameters */
            for (i = 0; i < (*npar); i++) fpar[i] = epar[i];
         }
         if (fabs( chi2 - chi1 ) <= (tolerance * chi1) || (labda > LABMAX)) {
            /*
             * We have a satisfying solution, so now we need to calculate
             * the correct errors of the fitted parameters. This we do
             * by using the pure Taylor method because we are very close
             * to the real solution.
             */
            labda = 0.0;		/* for Taylor solution */
            getmat( xdat, xdim, ydat, wdat, ndat, fpar, epar, npar, fopt );
            r = getvec( xdat, xdim, ydat, wdat, ndat, fpar, epar, npar, fopt );
            if (r) return( r );		/* error */
            for (i = 0; i < (*npar); i++) {
               epar[i] = 0.0;		/* and set error to zero */
            }
            chi2 = sqrt( chi2 / (double) (nuse - nfree) );
            for (i = 0; i < nfree; i++) {
               if ((matrix1[i][i] <= 0.0) || (matrix2[i][i] <= 0.0)) return( -7);
               epar[parptr[i]] = chi2 * sqrt( matrix2[i][i] ) / sqrt( matrix1[i][i] );
            }
            found = 1;			/* we found a solution */
         }
      }
   }
   return( itc );			/* return number of iterations */
}

#if	defined(TESTBED)
/*
 * For testing purposes only. We try to fit a one-dimensional Gaussian
 * to data with a uniform noise distribution. Although the algorithm
 * is developped for Gaussian noise patterns, it should not cause to
 * much problems. For Poison noise the algorithm is not suitable!
 */

static float func_c( float *xdat, float *fpar, fint *npar, fint *fopt )
/*
 * if *fopt == 1:
 * f(x) = a + b * exp( -1/2 * (c - x)^2 / d^2 ) (one-dimension Gauss)
 * if *fopt == 2:
 * f(x) = a + b * x + c * x^2 + d * x^3         (polynomial)
 */
{
   if (*fopt == 1) {
      double a;
      double b;
      double arg;
   
      a = fpar[2] - xdat[0];
      b = fpar[3];
      arg = 0.5 * a / b * a / b;
      return( fpar[0] + fpar[1] * exp( -arg ) );
   } else if (*fopt == 2) {
      double x = *xdat;

      return( fpar[0] + fpar[1] * x + fpar[2] * x * x + fpar[3] * x * x * x );
   }
   return( 0.0 );
}

static void derv_c( float *xdat, float *fpar, float *epar, fint *npar, fint *fopt )
{
   if (*fopt == 1) {
      double a;
      double b;
      double arg;
   
      a = fpar[2] - xdat[0];
      b = fpar[3];
      arg = 0.5 * a / b * a / b;
      epar[0] = 1.0;
      epar[1] = exp( -arg );
      epar[2] = -fpar[1] * epar[1] * a / b / b;
      epar[3] = fpar[1] * epar[1] * a * a / b / b / b;
   } else if (*fopt == 2) {
      double x = *xdat;

      epar[0] = 1.0;
      epar[1] = x;
      epar[2] = x * x;
      epar[3] = x * x * x;      
   }
}

void main( )
{
   fint  m;
   fint  n;
   fint  r;
   fint  xdim = 1;
   fint  its = 50;
   fint  ndat = 30;
   fint  nfit;
#if	defined(LINEAR)
   fint  opt = 2;
#else
   fint  opt = 1;
#endif
   fint  npar = 4;
   fint  mpar[4];
   float tol = 0.0;
#if	defined(LINEAR)
   float lab = 0.0;
#else
   float lab = 0.01;
#endif
   float xdat[30];
   float ydat[30];
   float wdat[30];
   float fpar[4];
   float epar[4];


   mpar[0] = 1; mpar[1] = 0; mpar[2] = 1; mpar[3] = 1;
   for (nfit = 0; nfit < 10; nfit++) {
      fpar[0] = 0.0; fpar[1] = 10.0; fpar[2] = 15.0; fpar[3] = 2.0;
      for (n = 0; n < ndat; n++) {
         float rndm;
         xdat[n] = (float) n;
         wdat[n] = 1.0;
         ydat[n] = func_c( &xdat[n], fpar, &npar, &opt );
         rndm = ((float) ( rand( ) - RAND_MAX / 2 )) / (float) RAND_MAX * 1.0;
         ydat[n] += rndm;
      }
      for (m = 0; m < npar; m++) {
         float rndm;
         rndm = (float) ( rand( ) - RAND_MAX / 2 ) / (float) RAND_MAX * 3.0;
         fpar[n] += rndm;
      }
      printf( "lsqfit:" );
      r = lsqfit_c( xdat, &xdim, ydat, wdat, &ndat, fpar, epar, mpar, &npar, &tol, &its, &lab, &opt );
      printf( " %ld", r );
      if (r < 0) {
         printf( ", error!\n" );
      } else {
         printf( ", success!\n" );
         printf( "fpar: %10f %10f %10f %10f\n", fpar[0], fpar[1], fpar[2], fpar[3] );
         printf( "epar: %10f %10f %10f %10f\n", (double) epar[0], (double) epar[1], (double) epar[2], (double) epar[3] );
      }
   }
}
#endif
