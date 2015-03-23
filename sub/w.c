/* w.c

	Copyright (c) Kapteyn Laboratorium Groningen 1992
	All Right Reserved.


#>            w.dc2

Function:     W

Purpose:      Error function for complex arguments.
              W(Z) = EXP(-Z*Z)ERFC(-iZ)

Category:

Files:        w.c

Author:       Kor G. Begeman

Use:          COMPLEX W(Z)          Input      COMPLEX

              W         The function value
              Z         Complex argument

Method:       see Abromowitz and Stegun (chapter 7)
                                - 6
Precision:    |error|  <  2 * 10

Updates:      Dec  2, 1982 : KGB creation date
              Apr 28, 1986 : KGB migrated to VAX-VMS
              Nov 26, 1992 : KGB Migrated to UNIX-GIPSY

#<

Fortran to C interface:

@ complex function w( complex )

*/

#include	"math.h"
#include	"stdio.h"
#include	"gipsyc.h"

#define	EPS	( 0.0000001 )		/* precision */

complex	w_c( complex *z )
{
   complex	r;			/* return value */
   double	wdx = 0.0;		/* real part result */
   double	wdy = 0.0;		/* imag part result */
   double	x, y;			/* real and imaginary arg */

   x = fabs( (double) z->r );		/* real part */
   y = fabs( (double) z->i );		/* imaginairy part */
   if ( x > 6.0 || y > 6.0 ) {		/* approximation for large arguments */
      static double	at[] = { 0.5124242, 0.05176536 };
      static double	bt[] = { 0.2752551, 2.72474500 };
      int		i;

      for ( i = 0; i < 2; i++ ) {
         double	det, dtx, dty, sav;

         sav = ( x * x - y * y - bt[i] );
         det = ( sav * sav + 4.0 * x * x * y * y );
         dtx = ( 2.0 * x * x * y - y * sav ) * at[i];
         dty = ( 2.0 * x * y * y + x * sav ) * at[i];
         wdx += dtx / det;
         wdy += dty / det;
      }
   } else if ( x > 3.9 || y > 3.0 ) {	/* approximation for intermediate arguments */
      static double	at[] = { 0.4613135, 0.09999216, 0.002883894 };
      static double	bt[] = { 0.1901635, 1.78449270, 5.525343700 };
      int		i;

      for ( i = 0; i < 3; i++ ) {
         double	det, dtx, dty, sav;

         sav = ( x * x - y * y - bt[i] );
         det = ( sav * sav + 4.0 * x * x * y * y );
         dtx = ( 2.0 * x * x * y - y * sav ) * at[i];
         dty = ( 2.0 * x * y * y + x * sav ) * at[i];
         wdx += dtx / det;
         wdy += dty / det;
      }
   } else {				/* no approximation */
      double	r;

      wdx = 1.0;
      r = sqrt( x * x + y * y );
      if ( r > 0.0 ) {
         static double	tt[] = { 1.0000000000, 0.5641895835 };
         double		tn[2];
         double		del, csp, snp, tcn, tsn;
         int		n = 0;

         csp = -y / r;
         snp = x / r;
         tcn = 1.0; tsn = 0.0;
         tn[0] = tt[0]; tn[1] = tt[1];
         do {
            double	tc, ts;
            int		i;

            n += 1;				/* increment interation number */
            tc = tcn;				/* save */
            ts = tsn;				/* save */
            tcn = ( tc * csp - ts * snp );	/* next cosine term */
            tsn = ( ts * csp + tc * snp );	/* next sine term */
            i = n%2;				/* argument */
            tn[i] *= ( 2.0 / (double) n );
            tn[0] *= r;				/* multiply with radius */
            tn[1] *= r;				/* multiply with radius */
            del = tn[i];			/* increment */
            wdx += ( tcn * del );		/* add increment */
            wdy += ( tsn * del );		/* add increment */
         } while ( del > EPS );			/* precision is reached */
      }
   }
   if ( z->r >= 0.0 && z->i >= 0.0 ) {
      r.r = wdx; r.i = wdy;
   } else if ( z->r >= 0.0 && z->i < 0.0 ) {
      double	csp, snp, sav;

      csp = cos( 2.0 * x * y );
      snp = sin( 2.0 * x * y );
      sav = exp( y * y - x * x );
      r.r = sav * csp - wdx;
      r.i = sav * snp + wdy;
   } else if ( z->r < 0.0 && z->i >= 0.0 ) {
      r.r = wdx;
      r.i = -wdy;
   } else if ( z->r < 0.0 && z->i < 0.0 ) {
      double	csp, snp, sav;

      csp = cos( 2.0 * x * y );
      snp = sin( 2.0 * x * y );
      sav = exp( y * y - x * x );
      r.r = sav * csp - wdx;
      r.i = -sav * snp - wdy;
   }
   return( r );				/* return */
}

#if	defined(TESTBED)

#include	"cmain.h"

MAIN_PROGRAM_ENTRY
{
   complex	w, z;
   double	x, y;
   int		i, j, k;

   for ( k = 0; k < 16; k++ ) {
      printf( " Y ");
      for ( i = 0; i < 5; i++ ) {
         x = k * 0.5 + i * 0.1;
         printf( "       X = %3.1f      ", x );
      }
      printf( "\n" );
      for ( j = 0; j < 91; j++ ) {
         y = j * 0.1;
         printf( "%3.1f", y );
         for ( i = 0; i < 5; i++ ) {
            x = k * 0.5 + i * 0.1;
            z.r = x; z.i = -y;
            w = w_c( &z );
            printf( " %9.6f %9.6f", (double) w.r, (double) w.i );
         }
         printf( "\n" );
      }
      printf( " \n" );
   }
   return( 0 );
}
#endif
