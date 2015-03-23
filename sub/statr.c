/* statr.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            statr.dc2

Function:     STATR

Purpose:      Determines the min, max, running mean and rms of an array
              of reals.

File:         statr.c

Author:       K.G. Begeman

Use:          CALL STATR( ARRAY  ,  input      REAL ARRAY
                          NITEM  ,  input      INTEGER
                          AMIN   ,  output     REAL
                          AMAX   ,  output     REAL
                          MEAN   ,  output     REAL
                          RMS    ,  output     REAL
                          NBLANK ,  output     INTEGER
                          NTOT   )  input      INTEGER

              ARRAY    Input array (part of subset).
              NITEM    Number of reals in ARRAY.
              AMIN     Running minimum in subset.
              AMAX     Running maximum in subset.
              MEAN     Running mean in subset.
              RMS      Running rms in subset.
              NBLANK   Number of blank values.
              NTOT     Number of values checked.

Description:  The routine is initialized when NTOT = 0. If not enough
              defined values to determine AMIN, AMAX, MEAN or RMS then
              the routine will return BLANK (can be checked with
              logical function FBLANK).

Example:      NTOT=0
              REPEAT
                CALL GDSI_READ( , , , A, , N, IER )
                CALL STATR(A,N,MIN,MAX,MEAN,RMS,NBLANK,NTOT)
              UNTIL (IER.EQ.0)

Updates:      Dec 16, 1988: KGB, original document.

#<

@ subroutine statr( real, integer, real, real, real, real, integer, integer )

*/

#include "stdio.h"
#include "math.h"
#include "gipsyc.h"
#include "setfblank.h"

void statr_c( float *array ,
              fint  *nitem ,
              float *amin  ,
              float *amax  ,
              float *mean  ,
              float *rms   ,
              fint  *nblank,
              fint  *ntot  )
{
   double  local_amax;
   double  local_amin;
   double  local_mean = 0.0;
   double  local_sum;
   double  local_sumsqr = 0.0;
   fint    i = 0;
   fint    local_nblank = 0;
   fint    local_ndef = 0;
   fint    ndef;
   float   blank;
   float   v;

   setfblank_c( &blank );                         /* get system defined BLANK */
   if (*ntot == 0) {                                 /* not yet initialized ? */
      *amin = *amax = *mean = *rms = blank;
      *nblank = 0;
   }
   ndef = (*ntot - *nblank);      /* number of defined values treated already */
   /*
    * First we try to find the first defined data value.
    */
   while (!local_ndef && i < *nitem) {      /* loop until defined value found */
      v = array[i++];                                       /* get next value */
      if (blank == v ) {
         local_nblank += 1;                                  /* another blank */
      } else {
         local_ndef = 1;                           /* the first defined value */
         local_amin = v;
         local_amax = v;
         local_sum = v;
      }
   }
   /*
    * Now we have found a defined data value, so we will now determine
    * the local average.
    */
   while (i < *nitem) {                         /* loop through rest of array */
      v = array[i++];                                       /* get next value */
      if (blank == v ) {
         local_nblank += 1;                                  /* another blank */
      } else {
         local_ndef++;                               /* another defined value */
         local_sum += v;
         if (v > local_amax) {
            local_amax = v;
         } else if (v < local_amin) {
            local_amin = v;
         }
      }
   }
   /*
    * If local average could be defined, then determine the sum of
    * the squared deviations from the mean.
    */
   if (local_ndef) {
      local_mean = local_sum / (double) local_ndef;
      for (i = 0; i < *nitem; i++) {
         v = array[i];
         if (v != blank) {
            double r = v - local_mean;
            
            local_sumsqr += r * r;
         }
      }
      if (ndef) {                    /* was already defined in previous calls */
         double old_sumsqr;
         double r = (*mean) - local_mean;
         double f = (double) ndef * (double) local_ndef / (double) (ndef + local_ndef); 
         
         if (ndef > 1) {
            old_sumsqr = (*rms) * (*rms) * (double) (ndef - 1);
         } else {
            old_sumsqr = 0.0;
         }
         local_sumsqr += old_sumsqr + r * r * f;
         *rms = sqrt( local_sumsqr / (double) (local_ndef + ndef - 1) );
         *mean *= (double) ndef;
         *mean += (double) local_ndef * local_mean;
         *mean /= (double) (ndef + local_ndef);
         if (*amax < local_amax) *amax = local_amax;
         if (*amin > local_amin) *amin = local_amin;
      } else {                                               /* define it now */
         if (local_ndef > 1) {
            *rms = sqrt( local_sumsqr / (double) (local_ndef - 1) );
         }
         *mean = local_mean;
         *amax = local_amax;
         *amin = local_amin;
      } 
   }
   *nblank += local_nblank;                           /* new number of blanks */
   *ntot += *nitem;                           /* new number of values treated */
}

#if defined(TESTBED)
main()
{
   float a[100], min = 4.0, max = -4.0, mean = 0.0, rms = 0.0;
   float ndef;
   fint  i, n = 5, nblank = 0, ntot = 0, ndat = 100;
   for (i = 0; i < ndat; i++) {
      if (i%10) {
         a[i] = 4.5 - (float) (i%10);
         if (min > a[i]) min = a[i]; else if (max < a[i]) max = a[i];
         mean += a[i];
         rms += a[i] * a[i];
      } else {
         setfblank_c(&a[i]);
         nblank++;
      }
   }
   ndef = (float) (100 - nblank);
   mean = mean / ndef;
   rms = sqrt( (rms - ndef * mean * mean) / (ndef - 1.0) );
   printf( "min: %8.5f,max: %8.5f,mean: %8.5f,rms: %8.5f,nblank: %2ld,ntot: %3ld\n",
   min, max, mean, rms, nblank, ndat );
   while (ntot < ndat) {
      statr_c( &a[ntot], &n, &min, &max, &mean, &rms, &nblank, &ntot );
      printf( "min: %8.5f,max: %8.5f,mean: %8.5f,rms: %8.5f,nblank: %2ld,ntot: %3ld\n",
      min, max, mean, rms, nblank, ntot );
   }
}
#endif
