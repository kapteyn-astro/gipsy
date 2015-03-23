/* sort.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            sort.dc2

Document:     SORT

Purpose:      Describes the available routines which sort arrays.

File:         sort.dc2

Author:       K.G. Begeman

Description:  The available routines are the following:
              SORTIA   SORTs Integer array in Ascending order
              SORTIAI  SORTs Integer array in Ascending order with Index
              SORTID   SORTs Integer array in Descending order
              SORTIDI  SORTs Integer array in Descending order with Index
              SORTRA   SORTs Real array in Ascending order
              SORTRAI  SORTs Real array in Ascending order with Index
              SORTRD   SORTs Real array in Descending order
              SORTRDI  SORTs Real array in Descending order with Index
              SORTDA   SORTs Double array in Ascending order
              SORTDAI  SORTs Double array in Ascending order with Index
              SORTDD   SORTs Double array in Descending order
              SORTDDI  SORTs Double array in Descending order with Index

Updates:      Nov  4, 1989: KGB Document created.

#<

*/

#include "stdio.h"
#include "stdlib.h"
#include "gipsyc.h"

/*
#>            sortia.dc2

Subroutine:   SORTIA

Purpose:      SORTIA sorts an Integer array in Ascending order.

Files:        sort.c

Author:       K.G. Begeman

Use:          CALL SORTIA( DATA,     In/Output     INTEGER ARRAY
                           NDAT )     Input        INTEGER

              DATA         Data array to sort.
              NDAT         Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortia( integer, integer )

*/

void sortia_c( fint *x, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] > x[j+1]) {
            fint xtemp = x[j];
            x[j] = x[j+1];
            x[j+1] = xtemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortiai.dc2

Subroutine:   SORTIAI

Purpose:      SORTIAI sorts an Integer array in Ascending order and
              maintains the original order in an Index array.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTIAI( DATA,     In/Output     INTEGER ARRAY
                            INDX,     In/Output     INTEGER ARRAY
                            NDAT )     Input        INTEGER

              DATA          Data array to sort.
              INDX          Index array which maintains the same
                            organization as the DATA array.
              NDAT          Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortiai( integer, integer, integer )

*/

void sortiai_c( fint *x, fint *i, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] > x[j+1]) {
            fint xtemp = x[j], itemp = i[j];
            x[j] = x[j+1]; i[j] = i[j+1];
            x[j+1] = xtemp; i[j+1] = itemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortid.dc2

Subroutine:   SORTID

Purpose:      SORTID sorts an Integer array in Descending order.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTID( DATA,     In/Output     INTEGER ARRAY
                           NDAT )     Input        INTEGER

              DATA         Data array to sort.
              NDAT         Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortid( integer, integer )

*/

void sortid_c( fint *x, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] < x[j+1]) {
            fint xtemp = x[j];
            x[j] = x[j+1];
            x[j+1] = xtemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortidi.dc2

Subroutine:   SORTIDI

Purpose:      SORTIDI sorts an Integer array in Descending order and
              maintains the original order in an Index array.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTIDI( DATA,     In/Output     INTEGER ARRAY
                            INDX,     In/Output     INTEGER ARRAY
                            NDAT )     Input        INTEGER

              DATA          Data array to sort.
              INDX          Index array which maintains the same
                            organization as the DATA array.
              NDAT          Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortidi( integer, integer, integer )

*/

void sortidi_c( fint *x, fint *i, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] < x[j+1]) {
            fint xtemp = x[j], itemp = i[j];
            x[j] = x[j+1]; i[j] = i[j+1];
            x[j+1] = xtemp; i[j+1] = itemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortra.dc2

Subroutine:   SORTRA

Purpose:      SORTRA sorts a Real array in Ascending order.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTRA( DATA,     In/Output     REAL ARRAY
                           NDAT )     Input        INTEGER

              DATA         Data array to sort.
              NDAT         Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortra( real, integer )

*/

void sortra_c( float *x, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] > x[j+1]) {
            float xtemp = x[j];
            x[j] = x[j+1];
            x[j+1] = xtemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortrai.dc2

Subroutine:   SORTRAI

Purpose:      SORTRAI sorts a Real array in Ascending order and
              maintains the original order in an Index array.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTRAI( DATA,     In/Output     REAL ARRAY
                            INDX,     In/Output     INTEGER ARRAY
                            NDAT )     Input        INTEGER

              DATA          Data array to sort.
              INDX          Index array which maintains the same
                            organization as the DATA array.
              NDAT          Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortrai( real, integer, integer )

*/

void sortrai_c( float *x, fint *i, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] > x[j+1]) {
            float xtemp = x[j]; fint itemp = i[j];
            x[j] = x[j+1]; i[j] = i[j+1];
            x[j+1] = xtemp; i[j+1] = itemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortrd.dc2

Subroutine:   SORTRD

Purpose:      SORTRD sorts a Real array in Descending order.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTRD( DATA,     In/Output     REAL ARRAY
                           NDAT )     Input        INTEGER

              DATA         Data array to sort.
              NDAT         Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortrd( real, integer )

*/

void sortrd_c( float *x, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] < x[j+1]) {
            float xtemp = x[j];
            x[j] = x[j+1];
            x[j+1] = xtemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortrdi.dc2

Subroutine:   SORTRDI

Purpose:      SORTRDI sorts a Real array in Descending order and
              maintains the original order in an Index array.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTRDI( DATA,     In/Output     REAL ARRAY
                            INDX,     In/Output     INTEGER ARRAY
                            NDAT )     Input        INTEGER

              DATA          Data array to sort.
              INDX          Index array which maintains the same
                            organization as the DATA array.
              NDAT          Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortrdi( real, integer, integer )

*/

void sortrdi_c( float *x, fint *i, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] < x[j+1]) {
            float xtemp = x[j]; fint itemp = i[j];
            x[j] = x[j+1]; i[j] = i[j+1];
            x[j+1] = xtemp; i[j+1] = itemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortda.dc2

Subroutine:   SORTDA

Purpose:      SORTDA sorts a Double precision array in Ascending order.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTDA( DATA,     In/Output     DOUBLE ARRAY
                           NDAT )     Input        INTEGER

              DATA         Data array to sort.
              NDAT         Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortda( double precision, integer )

*/

void sortda_c( double *x, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] > x[j+1]) {
            double xtemp = x[j];
            x[j] = x[j+1];
            x[j+1] = xtemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortdai.dc2

Subroutine:   SORTDAI

Purpose:      SORTDAI sorts a Double precision array in Ascending order
              and maintains the original order in an Index array.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTDAI( DATA,     In/Output     DOUBLE ARRAY
                            INDX,     In/Output     INTEGER ARRAY
                            NDAT )     Input        INTEGER

              DATA          Data array to sort.
              INDX          Index array which maintains the same
                            organization as the DATA array.
              NDAT          Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortdai( double precision, integer, integer )

*/

void sortdai_c( double *x, fint *i, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] > x[j+1]) {
            double xtemp = x[j]; fint itemp = i[j];
            x[j] = x[j+1]; i[j] = i[j+1];
            x[j+1] = xtemp; i[j+1] = itemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortdd.dc2

Subroutine:   SORTDD

Purpose:      SORTDD sorts a Double precision array in Descending order.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTDD( DATA,     In/Output     DOUBLE ARRAY
                           NDAT )     Input        INTEGER

              DATA         Data array to sort.
              NDAT         Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortdd( double precision, integer )

*/

void sortdd_c( double *x, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] < x[j+1]) {
            double xtemp = x[j];
            x[j] = x[j+1];
            x[j+1] = xtemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

/*
#>            sortddi.dc2

Subroutine:   SORTDDI

Purpose:      SORTDDI sorts a Double precision array in Descending order
              and maintains the original order in an Index array.

File:         sort.c

Author:       K.G. Begeman

Use:          CALL SORTDDI( DATA,     In/Output     DOUBLE ARRAY
                            INDX,     In/Output     INTEGER ARRAY
                            NDAT )     Input        INTEGER

              DATA          Data array to sort.
              INDX          Index array which maintains the same
                            organization as the DATA array.
              NDAT          Number of data points in DATA.

Updates:      Nov  4, 1989: KGB Document created.

#<

@ subroutine sortddi( double precision, integer, integer )

*/

void sortddi_c( double *x, fint *i, fint *n )
{
   fint ibnd;

   if ((*n) < 2) return;
   ibnd = (*n) - 1;
   do {
      fint ixch = -1;
      fint j;
      for (j = 0; j < ibnd; j++) {
         if (x[j] < x[j+1]) {
            double xtemp = x[j]; fint itemp = i[j];
            x[j] = x[j+1]; i[j] = i[j+1];
            x[j+1] = xtemp; i[j+1] = itemp;
            ixch = j;
         }
      }
      ibnd = ixch;
   } while (ibnd != -1);
}

#if defined(TESTBED)
main()
{
   double ad[10], dd[10];
   float  ar[10], dr[10];
   fint   ai[10], di[10];
   fint   iad[10], idd[10], iar[10], idr[10], iai[10], idi[10];
   fint   bb[10], ii[10];
   fint   n = 10, i;
   for (i = 0; i < n; i++) {
      ii[i] = i;
      ai[i] = bb[i] = rand();
      di[i] = ai[i];
      ar[i] = (float) ai[i];
      dr[i] = ar[i];
      ad[i] = (double) ai[i];
      dd[i] = ad[i];
   }
   sortia_c( ai, &n ); sortid_c( di, &n );
   sortra_c( ar, &n ); sortrd_c( dr, &n );
   sortda_c( ad, &n ); sortdd_c( dd, &n );
   printf("   SORTAI    SORTDI    SORTAR    SORTDR    SORTAD    SORTDD \n");
   for (i = 0; i < n; i++) {
      printf("%9ld %9ld %9.0f %9.0f %9.0f %9.0f \n",ai[i],di[i],ar[i],dr[i],ad[i],dd[i]);
   }
   printf("\n  SORTAII   SORTDII   SORTARI   SORTDRI   SORTADI   SORTDDI \n");
   for (i = 0; i < n; i++) {
      iad[i] = idd[i] = iar[i] = idr[i] = iai[i] = idi[i] = ii[i];
      ad[i] = dd[i] = (double) bb[i];
      ar[i] = dr[i] = (float) bb[i];
      ai[i] = di[i] = bb[i];
   }
   sortiai_c( ai, iai, &n ); sortidi_c( di, idi, &n );
   sortrai_c( ar, iar, &n ); sortrdi_c( dr, idr, &n );
   sortdai_c( ad, iad, &n ); sortddi_c( dd, idd, &n );
   for (i = 0; i < n; i++) {
      printf("%9ld %9ld %9.0f %9.0f %9.0f %9.0f \n",bb[iai[i]]-ai[i],bb[idi[i]]-di[i],((float)bb[iar[i]])-ar[i],((float)bb[idr[i]])-dr[i],((double)bb[iad[i]])-ad[i],((double)bb[idd[i]])-dd[i]);
   }
}
#endif
