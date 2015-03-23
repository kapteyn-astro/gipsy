/* moms.c

          Copyright (c) Kapteyn Laboratorium Groningen 1995
          All Rights Reserved.
*/


/*
#>            moms.dc2

Function:     MOMS

Purpose:      Given an data array, return (depending on mode)
              sum, mean, variance, standard deviation, absolute deviation,
              skewness, kurtosis or median.

Category:     MATH, STATISTICS

File:         moms.c

Author:       M.G.R. Vogelaar

Description:  The available routines are the following:
              MOMSF returns a moment value for a real array.
              MOMSD returns a moment value for a double precision array.

Updates:      Feb 20, 1995: VOG, Document created.

#<
*/


/*
#>            momsd.dc2


Function:     MOMSD

Purpose:      Given an array of double precision numbers, MOMSD returns
              (depending on mode) sum, mean, variance, standard deviation,
              absolute deviation, skewness, kurtosis or median.

File:         moms.c

Author:       M.G.R. Vogelaar

Use:          DOUBLE PRECISION MOMSD (
                                 OPTION,    INPUT  INTEGER
                                 X,         INPUT  DOUBLE PRECISION ARRAY
                                 WORK,      INPUT  DOUBLE PRECISION ARRAY
                                 NDAT,      INPUT  INTEGER
                                 NOUT,      OUTPUT INTEGER )


              MOMSD   Return value for one of the following options:
              OPTION   1) sum
                       2) mean
                       3) variance
                       4) standard deviation
                       5) absolute deviation
                       6) skewness
                       7) kurtosis
                       8) median
                       9) minimum
                      10) maximum
                      11) number of blanks
              X       Double precision array
              WORK    Double precision work array (same size as X !!)
              NDAT    Length of X array
              NOUT    Number of values (non blank) used in calculations


Description:  Calculate moments (mean, variance, skewness, and
              kurtosis) and other properties (sum, standard deviation,
              absolute deviation, median, minimum, maximum, and
              number of blanks) of a distribution. The numbers are
              stored in array X. An array WORK has the same size as X
              and is used as a work array. The data entered in X is
              unchanged after a call to the function MOMSD.
              X can contain (double precision) blanks, but for all
              options except the last, they will be skipped in the
              calculations.


Updates:      Feb 20, 1995: VOG, Document created.

#<



Fortran to C interface:

@ double precision function momsd( integer,
@                                  double precision,
@                                  double precision,
@                                  integer,
@                                  integer )

*/


#include    "gipsyc.h"       /* Defines the ANSI-F77 types for F to C intf. */
#include    "math.h"         /* Declares the mathematical functions and macros.*/
#include    "sortda.h"       /* Sort double array in ascending order */
#include    "setdblank.h"    /* Sets the double blank */
#include    "setfblank.h"    /* Sets the float blank */

#define   SUM         1
#define   MEAN        2
#define   VARIANCE    3
#define   STANDEV     4
#define   ABSDEV      5
#define   SKEWNESS    6
#define   KURTOSIS    7
#define   MEDIAN      8
#define   MAXI        9
#define   MINI       10
#define   NBLANKS    11

double momsd_c( fint   *option,
                double *x,
                double *work,
                fint   *ndat,
                fint   *nout )
/*--------------------------------------------------------------*/
/* Calculate moments and other characteristis of a distribution.*/
/*--------------------------------------------------------------*/
{
   double dblank;
   double sum = 0.0;
   int    i, j;
   int    opt = (*option);
   fint   n;

   /*-------------------------------------------*/
   /* Fill the work array with non blank values */
   /*-------------------------------------------*/
   setdblank_c( &dblank );
   n = (*ndat);

   for (j = 0, i = 0; i < n; i++)
   {
      if (x[i] != dblank)
         work[j++] = x[i];
   }
   (*nout) = n = j;

   if (opt == NBLANKS)
      return( (double) ((*ndat) - n) );

   if (n == 0)
      return( dblank );

   if (opt == MAXI)
   {
      double maxi = work[0];
      for (i = 1; i < n; i++)
         if (work[i] > maxi)
            maxi = work[i];
      return( maxi );
   }

   if (opt == MINI)
   {
      double mini = work[0];
      for (i = 1; i < n; i++)
         if (work[i] < mini)
            mini = work[i];
      return( mini );
   }

   if (opt == MEDIAN)
   {
      double  median = dblank;
      if (n < 2)
         median = dblank;
      else if (n == 2)
         median = (work[0] + work[1]) / 2.0;
      else if (n == 3)
      {
              if (work[0] >= work[1] && work[0] <= work[2]) median = work[0];
         else if (work[0] <= work[1] && work[0] >= work[2]) median = work[0];
         else if (work[1] >= work[0] && work[1] <= work[2]) median = work[1];
         else if (work[1] <= work[0] && work[1] >= work[2]) median = work[1];
         else if (work[2] >= work[0] && work[2] <= work[1]) median = work[2];
         else if (work[2] <= work[0] && work[2] >= work[1]) median = work[2];
      }
      else
      {
         sortda_c( work, &n );
         if (n%2)                                    /* 'n' odd */
            median = work[(n+1)/2-1];
         else                                        /* 'n' even */
            median = 0.5 * (work[n/2-1] + work[n/2]);
      }
      return( median );
   }

   for (i = 0; i < n; i++)
      sum += work[i];

   if (opt == SUM)
      return( sum );

   if (opt == MEAN)
      return( sum / (double) n );

   if (opt == VARIANCE)
   {
      double   var = 0.0;
      double   mean = sum / (double) n;            /* n > 0 always! */
      if (n < 2)
         var = dblank;
      else
      {
         for (i = 0; i < n; i++)
           var += (work[i] - mean) * (work[i] - mean);
         var /= (double) (n - 1);
      }
      return( var );
   }

   if (opt == STANDEV)
   {
      double   sdev = 0.0;
      double   mean = sum / (double) n;            /* n > 0 always! */
      if (n < 2)
         sdev = dblank;
      else
      {
         for (i = 0; i < n; i++)
           sdev += (work[i] - mean) * (work[i] - mean);
         sdev = sqrt( sdev / (double) (n - 1) );
      }
      return( sdev );
   }

   if (opt == ABSDEV)
   {
      double   adev = 0.0;
      double   mean = sum / (double) n;            /* n > 0 always! */
      for (i = 0; i < n; i++)
         adev += fabs(work[i] - mean);
      adev /= (double) n;
      return( adev );
   }

   if (opt == SKEWNESS)
   {
      double   skew = 0.0;
      double   sdev = 0.0;
      double   mean = sum / (double) n;            /* n > 0 always! */

      if (n < 2)
         skew = dblank;
      else
      {
         double   xx;
         for (i = 0; i < n; i++)
         {
            xx = (work[i] - mean) * (work[i] - mean);
            sdev += xx;
            skew += (xx * (work[i] - mean));
         }
         sdev = sqrt( sdev / (double) (n - 1) );
         if (sdev == 0.0)
            skew = dblank;
         else
            skew /= (sdev*sdev*sdev * (double) n);
         return( skew );
      }
   }

   if (opt == KURTOSIS)
   {
      double   kurt = 0.0;
      double   sdev = 0.0;
      double   mean = sum / (double) n;            /* n > 0 always! */

      if (n < 2)
         kurt = dblank;
      else
      {
         double   xx;
         for (i = 0; i < n; i++)
         {
            xx = (work[i] - mean) * (work[i] - mean);
            sdev += xx;
            kurt += xx * xx;
         }
         sdev = sqrt( sdev / (double) (n - 1) );
         if (sdev == 0.0)
            kurt = dblank;
         else
            kurt = (kurt / (sdev*sdev*sdev*sdev * (double) n)) - 3.0;
         return( kurt );
      }
   }

   return( dblank );                               /* Everything else failed */
}


/*
#>            momsf.dc2


Function:     MOMSF

Purpose:      Given an array of real numbers, MOMSF returns
              (depending on mode) sum, mean, variance, standard deviation,
              absolute deviation, skewness, kurtosis or median.

Files:        moms.c

Author:       M.G.R. Vogelaar

Use:          DOUBLE PRECISION MOMSF (
                                 OPTION,    INPUT  INTEGER
                                 X,         INPUT  REAL ARRAY
                                 WORK,      INPUT  DOUBLE PRECISION ARRAY
                                 NDAT,      INPUT  INTEGER
                                 NOUT,      OUTPUT INTEGER )


              MOMSF   Return value for one of the following options:
              OPTION   1) sum
                       2) mean
                       3) variance
                       4) standard deviation
                       5) absolute deviation
                       6) skewness
                       7) kurtosis
                       8) median
                       9) minimum
                      10) maximum
                      11) number of blanks
              X       REAL array
              WORK    DOUBLE PRECISION work array (same size as X !!)
              NDAT    Length of X array
              NOUT    Number of values (non blank) used in calculations


Description:  Calculate moments (mean, variance, skewness, and
              kurtosis) and other properties (sum, standard deviation,
              absolute deviation, median, minimum, maximum, and
              number of blanks) of a distribution. The numbers are
              stored in array X. A double precision array WORK has
              the same size as X and is used as a work array so that the
              calculations can be performed in double precision. The data
              entered in X is unchanged after a call to the function MOMSF.
              X can contain blanks, but for all options exept the last,
              they will be skipped in the calculations.


Updates:      Feb 20, 1995: VOG, Document created.

#<
*/


double momsf_c( fint   *option,
                float  *x,
                double *work,
                fint   *ndat,
                fint   *nout )
/*------------------------------------------------------------*/
/* Calculate moments etc of real array, i.e. convert all      */
/* elements to doubles (convert blanks in special way) and    */
/* call 'momsd_c'. Convert blank results.                     */
/*------------------------------------------------------------*/
{
   float  fblank;
   double dblank;
   int    i;
   double val;

   setdblank_c( &dblank );
   setfblank_c( &fblank );
   for (i = 0; i < (*ndat); i++)
   {
      if (x[i] != fblank)
         work[i] = (double) x[i];
      else
         work[i] = dblank;
   }
   val = momsd_c(option, work, work, ndat, nout);
   if (val == dblank)
      return( fblank );
   return( (float) val );
}



#if defined(TESTBED)

/* Common includes */

#include    "cmain.h"
#include    "stdlib.h"
#include    "init.h"         /* Declare task running to HERMES and initialize.*/
#include    "finis.h"        /* Informs HERMES that servant quits and cleans up */
#include    "userfio.h"      /* Easy-C companions for GIPSY user interface rout */
#include    "setfblank.h"    /* Function to set a data value to the universal B*/

#include    "userdble.h"
#include    "userint.h"

#define     MAXITEMS    1000

static double    dblank;
static float     fblank;


MAIN_PROGRAM_ENTRY
{
   float    x[MAXITEMS];
   double   work[MAXITEMS];
   float    val;
   fint     nitems, dfault;
   fint     option, nout;
   fint     r;


   init_c();
/*   setdblank_c( &dblank ); */
   setfblank_c( &fblank );
   nitems = MAXITEMS;
   dfault = 0;
/*   r = userdble_c( x, &nitems, &dfault, tofchar("X="), tofchar( "Give x:" ) );*/
   r = userreal_c( x, &nitems, &dfault, tofchar("X="), tofchar( "Give x:" ) );

   option = 1;
   dfault = 1;
   nitems = 1;
   (void) userint_c( &option, &nitems, &dfault, tofchar("OPTION="),
                     tofchar( "Give option 1..11:     [1]" ) );

/*   val = momsd_c( &option, x, work, &r, &nout );*/
   val = momsf_c( &option, x, work, &r, &nout );
/*   if (val == dblank) */
   if (val == fblank)
      anyoutf( 1, "BLANK" );
   else if (option == 1)
      anyoutf( 1, "sum = %f", val );
   else if (option == 2)
      anyoutf( 1, "mean = %f", val );
   else if (option == 3)
      anyoutf( 1, "var = %f", val );
   else if (option == 4)
      anyoutf( 1, "sdev = %f", val );
   else if (option == 5)
      anyoutf( 1, "adev = %f", val );
   else if (option == 6)
      anyoutf( 1, "skewness = %f", val );
   else if (option == 7)
      anyoutf( 1, "kurtosis = %f", val );
   else if (option == 8)
      anyoutf( 1, "median = %f", val );
   else if (option == 9)
      anyoutf( 1, "max = %f", val );
   else if (option == 10)
      anyoutf( 1, "min = %f", val );
   else if (option == 11)
      anyoutf( 1, "number of blanks = %d", (int) val );

   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}

#endif
