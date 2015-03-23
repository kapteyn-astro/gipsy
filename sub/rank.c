/* rank.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            rank.dc2

Document:     RANK

Purpose:      Describes the available ranking routines.

Category:     MATH

File:         rank.c

Author:       K.G. Begeman

Description:  The RANK.. routines index an array ARRIN of length N,
              i.e. outputs an integer array INDX such that
              ARRIN(INDX(J)+1) is in ascending or descending order
              for J = 1, 2, ..., N. The input quantities N and ARRIN
              are not changed. The heapsort algorithm is used.
              The folowing routines are available:

              SUBROUTINE RANKIA( ARRIN, INDX, N )
                   indexes an integer array in ascending order.
              SUBROUTINE RANKID( ARRIN, INDX, N )
                   indexes an integer array in descending order.
              SUBROUTINE RANKRA( ARRIN, INDX, N )
                   indexes a single precision array in ascending order.
              SUBROUTINE RANKRD( ARRIN, INDX, N )
                   indexes a single precision array in descending order.
              SUBROUTINE RANKDA( ARRIN, INDX, N )
                   indexes a double precision array in ascending order.
              SUBROUTINE RANKDD( ARRIN, INDX, N )
                   indexes a double precision array in descending order.

Updates:      May 26, 1990: KGB, Document created.

#<

*/

#include        "stdio.h"               /* <stdio.h> */
#include        "stdlib.h"              /* <stdlib.h> */
#include        "gipsyc.h"              /* GIPSY symbols and definitions */

/*
#>            rankia.dc2

Function:     RANKIA

Purpose:      Indexes an integer array in ascending order.

Category:     MATH

File:         rank.c

Author:       K.G. Begeman

Use:          CALL RANKIA( ARRIN ,       Input      INTEGER ARRAY
                           INDX  ,       Output     INTEGER ARRAY
                           N     )       Input      INTEGER

              ARRIN      Array to be indexed.
              INDX       Sorted index.
              N          Number of elements in ARRIN and INDX.

Description:  RANKIA indexes an array ARRIN of length N, i.e. outputs
              the array INDX such that ARRIN(INDX(J)+1) is in
              ascending order for J = 1, 2, ..., N. The input
              quantities N and ARRIN are not changed. The heapsort
              algorithm is used.

Updates:      May 15, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine rankia( integer, integer, integer )

*/

void rankia_c( fint arrin[], fint indx[], fint *n )
{
   fint ir;
   fint l;

   /*
    * initialize the index aray with consecutive integers.
    */
   for (l = 0; l < (*n); l++) indx[l] = l;
   if ((*n) < 2) return;
   /*
    * The index L will be decremented from its initial value down to 1
    * during the "hiring" (heap creation) phase. Once it reaches 1, the
    * index IR will be decremented from its initial value down to 1
    * during the "retirement-and-promotion" (heap selection) phase.
    */
   l = (*n) / 2;
   ir = (*n) - 1;
   while (1) {
      fint i, j;
      fint indxt;
      fint q;

      if (l > 0) {                      /* Still in hiring phase */
         indxt = indx[--l];
         q = arrin[indxt];
      } else {                          /* In retirement-and-promotion phase */
         indxt = indx[ir];              /* Clear a space at end of array */
         q = arrin[indxt];
         indx[ir--] = indx[0];          /* Retire the top of the heap into it */
         if (ir == 0) {                 /* Done with the last promotion */
            indx[0] = indxt;            /* The least competent worker of all */
            return;
         }
      }
      /*
       * Whether we are in the hiring phase or promotion phase, we here
       * set up to sift down element q to its proper level.
       */
      i = l;
      j = l + l + 1;
      while (j <= ir) {
         /*
          * Compare to the better underling
          */
         if (j < ir && arrin[indx[j]] < arrin[indx[j+1]]) j++;
         if (q < arrin[indx[j]]) {      /* Demote q */
            indx[i] = indx[j];
            i = j;
            j = j + j + 1;
         } else {                       /* This is q's level */
            j = ir + 1;                 /* Set j to terminate the sift-down */
         }
      }
      indx[i] = indxt;                  /* Put q into its slot */
   }
}

/*
#>            rankid.dc2

Function:     RANKID

Purpose:      Indexes an integer array in descending order.

Category:     MATH

File:         rank.c

Author:       K.G. Begeman

Use:          CALL RANKID( ARRIN ,       Input      INTEGER ARRAY
                           INDX  ,       Output     INTEGER ARRAY
                           N     )       Input      INTEGER

              ARRIN      Array to be indexed.
              INDX       Sorted index.
              N          Number of elements in ARRIN and INDX.

Description:  RANKID indexes an array ARRIN of length N, i.e. outputs
              the array INDX such that ARRIN(INDX(J)+1) is in
              descending order for J = 1, 2, ..., N. The input
              quantities N and ARRIN are not changed. The heapsort
              algorithm is used.

Updates:      May 15, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine rankid( integer, integer, integer )

*/

void rankid_c( fint arrin[], fint indx[], fint *n )
{
   fint ir;
   fint l;

   /*
    * initialize the index aray with consecutive integers.
    */
   for (l = 0; l < (*n); l++) indx[l] = l;
   if ((*n) < 2) return;
   /*
    * The index L will be decremented from its initial value down to 1
    * during the "hiring" (heap creation) phase. Once it reaches 1, the
    * index IR will be decremented from its initial value down to 1
    * during the "retirement-and-promotion" (heap selection) phase.
    */
   l = (*n) / 2;
   ir = (*n) - 1;
   while (1) {
      fint i, j;
      fint indxt;
      fint q;

      if (l > 0) {                      /* Still in hiring phase */
         indxt = indx[--l];
         q = arrin[indxt];
      } else {                          /* In retirement-and-promotion phase */
         indxt = indx[ir];              /* Clear a space at end of array */
         q = arrin[indxt];
         indx[ir--] = indx[0];          /* Retire the top of the heap into it */
         if (ir == 0) {                 /* Done with the last promotion */
            indx[0] = indxt;            /* The least competent worker of all */
            return;
         }
      }
      /*
       * Whether we are in the hiring phase or promotion phase, we here
       * set up to sift down element q to its proper level.
       */
      i = l;
      j = l + l + 1;
      while (j <= ir) {
         /*
          * Compare to the better underling
          */
         if (j < ir && arrin[indx[j]] > arrin[indx[j+1]]) j++;
         if (q > arrin[indx[j]]) {      /* Demote q */
            indx[i] = indx[j];
            i = j;
            j = j + j + 1;
         } else {                       /* This is q's level */
            j = ir + 1;                 /* Set j to terminate the sift-down */
         }
      }
      indx[i] = indxt;                  /* Put q into its slot */
   }
}

/*
#>            rankra.dc2

Function:     RANKRA

Purpose:      Indexes a real array in ascending order.

Category:     MATH

File:         rank.c

Author:       K.G. Begeman

Use:          CALL RANKRA( ARRIN ,       Input      REAL ARRAY
                           INDX  ,       Output     INTEGER ARRAY
                           N     )       Input      INTEGER

              ARRIN      Array to be indexed.
              INDX       Sorted index.
              N          Number of elements in ARRIN and INDX.

Description:  RANKRA indexes an array ARRIN of length N, i.e. outputs
              the array INDX such that ARRIN(INDX(J)+1) is in
              ascending order for J = 1, 2, ..., N. The input
              quantities N and ARRIN are not changed. The heapsort
              algorithm is used.

Updates:      May 15, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine rankra( real, integer, integer )

*/

void rankra_c( float arrin[], fint indx[], fint *n )
{
   fint ir;
   fint l;

   /*
    * initialize the index aray with consecutive integers.
    */
   for (l = 0; l < (*n); l++) indx[l] = l;
   if ((*n) < 2) return;
   /*
    * The index L will be decremented from its initial value down to 1
    * during the "hiring" (heap creation) phase. Once it reaches 1, the
    * index IR will be decremented from its initial value down to 1
    * during the "retirement-and-promotion" (heap selection) phase.
    */
   l = (*n) / 2;
   ir = (*n) - 1;
   while (1) {
      fint  i, j;
      fint  indxt;
      float q;

      if (l > 0) {                      /* Still in hiring phase */
         indxt = indx[--l];
         q = arrin[indxt];
      } else {                          /* In retirement-and-promotion phase */
         indxt = indx[ir];              /* Clear a space at end of array */
         q = arrin[indxt];
         indx[ir--] = indx[0];          /* Retire the top of the heap into it */
         if (ir == 0) {                 /* Done with the last promotion */
            indx[0] = indxt;            /* The least competent worker of all */
            return;
         }
      }
      /*
       * Whether we are in the hiring phase or promotion phase, we here
       * set up to sift down element q to its proper level.
       */
      i = l;
      j = l + l + 1;
      while (j <= ir) {
         /*
          * Compare to the better underling
          */
         if (j < ir && arrin[indx[j]] < arrin[indx[j+1]]) j++;
         if (q < arrin[indx[j]]) {      /* Demote q */
            indx[i] = indx[j];
            i = j;
            j = j + j + 1;
         } else {                       /* This is q's level */
            j = ir + 1;                 /* Set j to terminate the sift-down */
         }
      }
      indx[i] = indxt;                  /* Put q into its slot */
   }
}

/*
#>            rankrd.dc2

Function:     RANKRD

Purpose:      Indexes a real array in descending order.

Category:     MATH

File:         rank.c

Author:       K.G. Begeman

Use:          CALL RANKRD( ARRIN ,       Input      REAL ARRAY
                           INDX  ,       Output     INTEGER ARRAY
                           N     )       Input      INTEGER

              ARRIN      Array to be indexed.
              INDX       Sorted index.
              N          Number of elements in ARRIN and INDX.

Description:  RANKRD indexes an array ARRIN of length N, i.e. outputs
              the array INDX such that ARRIN(INDX(J)+1) is in
              descending order for J = 1, 2, ..., N. The input
              quantities N and ARRIN are not changed. The heapsort
              algorithm is used.

Updates:      May 15, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine rankrd( real, integer, integer )

*/

void rankrd_c( float arrin[], fint indx[], fint *n )
{
   fint ir;
   fint l;

   /*
    * initialize the index aray with consecutive integers.
    */
   for (l = 0; l < (*n); l++) indx[l] = l;
   if ((*n) < 2) return;
   /*
    * The index L will be decremented from its initial value down to 1
    * during the "hiring" (heap creation) phase. Once it reaches 1, the
    * index IR will be decremented from its initial value down to 1
    * during the "retirement-and-promotion" (heap selection) phase.
    */
   l = (*n) / 2;
   ir = (*n) - 1;
   while (1) {
      fint  i, j;
      fint  indxt;
      float q;

      if (l > 0) {                      /* Still in hiring phase */
         indxt = indx[--l];
         q = arrin[indxt];
      } else {                          /* In retirement-and-promotion phase */
         indxt = indx[ir];              /* Clear a space at end of array */
         q = arrin[indxt];
         indx[ir--] = indx[0];          /* Retire the top of the heap into it */
         if (ir == 0) {                 /* Done with the last promotion */
            indx[0] = indxt;            /* The least competent worker of all */
            return;
         }
      }
      /*
       * Whether we are in the hiring phase or promotion phase, we here
       * set up to sift down element q to its proper level.
       */
      i = l;
      j = l + l + 1;
      while (j <= ir) {
         /*
          * Compare to the better underling
          */
         if (j < ir && arrin[indx[j]] > arrin[indx[j+1]]) j++;
         if (q > arrin[indx[j]]) {      /* Demote q */
            indx[i] = indx[j];
            i = j;
            j = j + j + 1;
         } else {                       /* This is q's level */
            j = ir + 1;                 /* Set j to terminate the sift-down */
         }
      }
      indx[i] = indxt;                  /* Put q into its slot */
   }
}

/*
#>            rankda.dc2

Function:     RANKDA

Purpose:      Indexes a double precision array in ascending order.

Category:     MATH

File:         rank.c

Author:       K.G. Begeman

Use:          CALL RANKIA( ARRIN ,    Input   DOUBLE PRECISION ARRAY
                           INDX  ,    Output  INTEGER ARRAY
                           N     )    Input   INTEGER

              ARRIN      Array to be indexed.
              INDX       Sorted index.
              N          Number of elements in ARRIN and INDX.

Description:  RANKDA indexes an array ARRIN of length N, i.e. outputs
              the array INDX such that ARRIN(INDX(J)+1) is in
              ascending order for J = 1, 2, ..., N. The input
              quantities N and ARRIN are not changed. The heapsort
              algorithm is used.

Updates:      May 15, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine rankda( double precision, integer, integer )

*/

void rankda_c( double arrin[], fint indx[], fint *n )
{
   fint ir;
   fint l;

   /*
    * initialize the index aray with consecutive integers.
    */
   for (l = 0; l < (*n); l++) indx[l] = l;
   if ((*n) < 2) return;
   /*
    * The index L will be decremented from its initial value down to 1
    * during the "hiring" (heap creation) phase. Once it reaches 1, the
    * index IR will be decremented from its initial value down to 1
    * during the "retirement-and-promotion" (heap selection) phase.
    */
   l = (*n) / 2;
   ir = (*n) - 1;
   while (1) {
      fint   i, j;
      fint   indxt;
      double q;

      if (l > 0) {                      /* Still in hiring phase */
         indxt = indx[--l];
         q = arrin[indxt];
      } else {                          /* In retirement-and-promotion phase */
         indxt = indx[ir];              /* Clear a space at end of array */
         q = arrin[indxt];
         indx[ir--] = indx[0];          /* Retire the top of the heap into it */
         if (ir == 0) {                 /* Done with the last promotion */
            indx[0] = indxt;            /* The least competent worker of all */
            return;
         }
      }
      /*
       * Whether we are in the hiring phase or promotion phase, we here
       * set up to sift down element q to its proper level.
       */
      i = l;
      j = l + l + 1;
      while (j <= ir) {
         /*
          * Compare to the better underling
          */
         if (j < ir && arrin[indx[j]] < arrin[indx[j+1]]) j++;
         if (q < arrin[indx[j]]) {      /* Demote q */
            indx[i] = indx[j];
            i = j;
            j = j + j + 1;
         } else {                       /* This is q's level */
            j = ir + 1;                 /* Set j to terminate the sift-down */
         }
      }
      indx[i] = indxt;                  /* Put q into its slot */
   }
}

/*
#>            rankdd.dc2

Function:     RANKDD

Purpose:      Indexes a double precision array in descending order.

Category:     MATH

File:         rank.c

Author:       K.G. Begeman

Use:          CALL RANKDD( ARRIN ,    Input   DOUBLE PRECISION ARRAY
                           INDX  ,    Output  INTEGER ARRAY
                           N     )    Input   INTEGER

              ARRIN      Array to be indexed.
              INDX       Sorted index.
              N          Number of elements in ARRIN and INDX.

Description:  RANKDD indexes an array ARRIN of length N, i.e. outputs
              the array INDX such that ARRIN(INDX(J)+1) is in
              descending order for J = 1, 2, ..., N. The input
              quantities N and ARRIN are not changed. The heapsort
              algorithm is used.

Updates:      May 15, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine rankdd( double precision, integer, integer )

*/

void rankdd_c( double arrin[], fint indx[], fint *n )
{
   fint ir;
   fint l;

   /*
    * initialize the index aray with consecutive integers.
    */
   for (l = 0; l < (*n); l++) indx[l] = l;
   if ((*n) < 2) return;
   /*
    * The index L will be decremented from its initial value down to 1
    * during the "hiring" (heap creation) phase. Once it reaches 1, the
    * index IR will be decremented from its initial value down to 1
    * during the "retirement-and-promotion" (heap selection) phase.
    */
   l = (*n) / 2;
   ir = (*n) - 1;
   while (1) {
      fint   i, j;
      fint   indxt;
      double q;

      if (l > 0) {                      /* Still in hiring phase */
         indxt = indx[--l];
         q = arrin[indxt];
      } else {                          /* In retirement-and-promotion phase */
         indxt = indx[ir];              /* Clear a space at end of array */
         q = arrin[indxt];
         indx[ir--] = indx[0];          /* Retire the top of the heap into it */
         if (ir == 0) {                 /* Done with the last promotion */
            indx[0] = indxt;            /* The least competent worker of all */
            return;
         }
      }
      /*
       * Whether we are in the hiring phase or promotion phase, we here
       * set up to sift down element q to its proper level.
       */
      i = l;
      j = l + l + 1;
      while (j <= ir) {
         /*
          * Compare to the better underling
          */
         if (j < ir && arrin[indx[j]] > arrin[indx[j+1]]) j++;
         if (q > arrin[indx[j]]) {      /* Demote q */
            indx[i] = indx[j];
            i = j;
            j = j + j + 1;
         } else {                       /* This is q's level */
            j = ir + 1;                 /* Set j to terminate the sift-down */
         }
      }
      indx[i] = indxt;                  /* Put q into its slot */
   }
}

#if     defined(TESTBED)
/*
 * When compiled with TESTBED defined, the RANK.. routines are tested.
 */

#define NMAX    2000                    /* length of arrays */

void main()
{
   double da[NMAX];
   fint   n = NMAX;
   fint   rank[NMAX];
   fint   ia[NMAX];
   float  ra[NMAX];
   int    i, j;
   int    okay;

   for (i = 0; i < n; i++) {
      ia[i] = (fint) rand();
      ra[i] = (float) rand();
      da[i] = (double) rand();
   }
   rankia_c( ia, rank, &n );
   for (okay = 1, i = 1; okay && i < n; i++) {
      if (ia[rank[i-1]] > ia[rank[i]]) okay = 0;
   }
   if (okay) {
      printf( "rankia: okay\n" );
   } else {
      printf( "rankia: error\n" );
   }
   rankid_c( ia, rank, &n );
   for (okay = 1, i = 1; okay && i < n; i++) {
      if (ia[rank[i-1]] < ia[rank[i]]) okay = 0;
   }
   if (okay) {
      printf( "rankid: okay\n" );
   } else {
      printf( "rankid: error\n" );
   }
   rankra_c( ra, rank, &n );
   for (okay = 1, i = 1; okay && i < n; i++) {
      if (ra[rank[i-1]] > ra[rank[i]]) okay = 0;
   }
   if (okay) {
      printf( "rankra: okay\n" );
   } else {
      printf( "rankra: error\n" );
   }
   rankrd_c( ra, rank, &n );
   for (okay = 1, i = 1; okay && i < n; i++) {
      if (ra[rank[i-1]] < ra[rank[i]]) okay = 0;
   }
   if (okay) {
      printf( "rankrd: okay\n" );
   } else {
      printf( "rankrd: error\n" );
   }
   rankda_c( da, rank, &n );
   for (okay = 1, i = 1; okay && i < n; i++) {
      if (da[rank[i-1]] > da[rank[i]]) okay = 0;
   }
   if (okay) {
      printf( "rankda: okay\n" );
   } else {
      printf( "rankda: error\n" );
   }
   rankdd_c( da, rank, &n );
   for (okay = 1, i = 1; okay && i < n; i++) {
      if (da[rank[i-1]] < da[rank[i]]) okay = 0;
   }
   if (okay) {
      printf( "rankdd: okay\n" );
   } else {
      printf( "rankdd: error\n" );
   }
}
#endif
