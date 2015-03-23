/* ran.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            ran.dc2

Function:     RAN

Purpose:      Returns a uniform random deviate between 0.0 and 1.0.

Category:     MATH

Files:        ran.c

Author:       K.G. Begeman

Use:          REAL RAN( SEED )         Input/Output       INTEGER

              RAN       Uniform random deviate between 0.0 and 1.0.
              SEED      A negative value for SEED reintializes the
                        sequence with seed -SEED and on exit SEED
                        will be one.
                        A positive value for SEED has no effect except
                        for the first call to RAN. Then it initializes
                        the sequence with seed SEED and on exit SEED
                        will be one.

Description:  The random numbers are obtained with the subtractive
              method described by Knuth (Knuth, D.E. 1981, Seminumerical
              Algorithms, 2nd ed., vol. 2 of The Art of Computer
              Programming).

Updates:      May  25, 1990: KGB, Document created.

#<

Fortran to C interface:

@ real function ran( integer )

*/

#include        "stdio.h"               /* <stdio.h> */
#include        "stdlib.h"              /* <stdlib.h> */
#include        "gipsyc.h"              /* GIPSY symbols and definitions */

#define MBIG    1000000000              /* upper limit, should be large */
#define MKNUTH  55                      /* special, should not be changed */
#define MSEED   161803398               /* may be smaller, but must be large */
#define MZ      0                       /* lower limit, must be zero */
#define FAC     1.0e-9                  /* must be 1 / MBIG */

static  fint    IFF = 0;                /* ensures correct initialization */
static  fint    INEXT;                  /* points into table */
static  fint    INEXTP;                 /* points into table */
static  fint    MA[MKNUTH];             /* table */

float   ran_c( fint *seed )
{
   fint i, ii;
   fint k;
   fint mj, mk;

   if (*seed < 0 || IFF == 0) {         /* initialization */
      IFF = 1;
      mj = MSEED - labs((long)*seed);   /* initialize MA[MKNUTH-1] using .. */
      mj = mj%MBIG;                     /* .. the seed seed and the large .. */
      MA[MKNUTH-1] = mj;                /* .. number MSEED */
      mk = 1;
      for (i = 1; i < MKNUTH; i++) {    /* intialize the rest of the table, */
         ii = (21 * i)%MKNUTH - 1;      /* in a slightly random order, */
         MA[ii] = mk;                   /* with numbers that are not .. */
         mk = mj - mk;                  /* .. expecially random */
         if (mk < MZ) mk += MBIG;
         mj = MA[ii];
      }
      for (k = 0; k < 4; k++) {         /* "warming up the generator" */
         for (i = 0; i < MKNUTH; i++) {
            MA[i] = MA[i] - MA[(i+31)%MKNUTH];
            if (MA[i] < MZ) MA[i] += MBIG;
         }
      }
      INEXT = -1;                       /* indices for our first number */
      INEXTP = 30;                      /* the constant 30 is special (Knuth) */
      *seed = 1;                        /* make seed inoperative */
   }
   if (++INEXT == MKNUTH) INEXT = 0;    /* next table entry, wrap */
   if (++INEXTP == MKNUTH) INEXTP = 0;  /* ditto */
   mj = MA[INEXT] - MA[INEXTP];         /* generate new random number */
   if (mj < MZ) mj += MBIG;             /* check range */
   MA[INEXT] = mj;                      /* store it, and output .. */
   return( (float) mj * FAC );          /* .. the derived uniform deviate */
}
