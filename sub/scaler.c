/* scaler.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            scaler.dc2

Subroutine:   SCALER

Purpose:      This subroutine scales a real input array to an output
              array. It recognizes BLANKs. This means that BLANKS are
              transferred unscaled.

File:         scaler.c

Author:       K.G Begeman

Use:          CALL SCALER( FACTOR,       Input    real
                           OFFSET,       Input    real
                           ARRAY1,       Input    real array
                           ARRAY2,       Output   real array
                           NEL )         Input    integer

              FACTOR  factor with which to multiply ARRAY1.
              OFFSET  Offset to be added after multiplying.
              ARRAY1  Input array.
              ARRAY2  Output array; ARRAY2 = A * ARRAY1 + B.
              NITEMS  Number of elements in ARRAY1 and ARRAY2.

Updates:      Jul 30, 1989: KGB, original document.

#<

@ subroutine scaler( real, real, real, real, integer )

*/
 
#include "stdio.h"
#include "gipsyc.h"
#include "setfblank.h"
#include "setnfblank.h"

void scaler_c( float *arg1, float *arg2, float *arg3, float *arg4, fint *arg5 )
{
   fint   i;
   float  blank;
   float *in = arg3;
   float *out = arg4;
   float  v;

   setfblank_c( &blank );                         /* get system defined BLANK */
   if (blank == *arg1 || blank == *arg2 ) {
      setnfblank_c( arg4, arg5 );                          /* output is BLANK */
   } else {
      float f = *arg1;
      float o = *arg2;

      for (i = 0; i < *arg5; i++) {                           /* scaling loop */
         v = *in++;
         if (blank == v ) *out++ = blank; else *out++ = v * f + o;
      }
   }
}

#if defined(TESTBED)
main()
{
   float in[10], out[10], a = 2.0, b = 1.0;
   fint  n = 10;
   fint  i;
   for (i = 0; i < n; in[i++] = (float) i);
   setfblank_c(&in[5]);
   scaler_c( &a, &b, in, out, &n );
   for (i = 0; i < n; i++) {
      if (fblank_c(&out[i])) printf("   BLANK\n"); else printf("%f\n",out[i]);
   }
   setfblank_c(&a);
   scaler_c( &a, &b, in, out, &n );
   for (i = 0; i < n; i++) {
      if (fblank_c(&out[i])) printf("   BLANK\n"); else printf("%f\n",out[i]);
   }
}
#endif
