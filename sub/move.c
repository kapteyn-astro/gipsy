/* move.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            move.dc2

Document:     MOVE

Purpose:      Describes the routines which copy data from one
              array into another.

Category:     ARRAY

File:         move.c

Author:       K.G. Begeman

Description:  The available routines which copy data are the following:
              MOVEI( SOURCE, DESTIN, NITEMS )     copies integers
              MOVEL( SOURCE, DESTIN, NITEMS )     copies logicals
              MOVER( SOURCE, DESTIN, NITEMS )     copies reals
              MOVED( SOURCE, DESTIN, NITEMS )     copies doubles

Notes:        The arrays to be transferred may overlap.

Updates:      Jul 29, 1989 : KGB, Document created.

#<

*/

#include "stdio.h"
#include "string.h"
#include "gipsyc.h"

/*
#>            movei.dc2

Subroutine:   MOVEI

Purpose:      Copies integers from one array to another.

Category:     ARRAY

File:         move.c

Author:       K.G. Begeman

Use:          CALL MOVEI( SOURCE,       Input       INTEGER ARRAY
                          DESTIN,       Output      INTEGER ARRAY
                          NITEMS )      Input       INTEGER

              SOURCE   input array to be copied to DESTIN.
              DESTIN   output array.
              NITEMS   Number of elements to be copied.

Notes:        The arrays to be transferred may overlap.

Updates:      Jul 29, 1989 : KGB, Document created.

#<

@ subroutine movei( integer, integer, integer )

*/

void movei_c( fint *source, fint *destin, fint *nitems )
{
   (void) memmove( destin, source, (*nitems) * sizeof(fint) );
}

/*
#>            movel.dc2

Subroutine:   MOVEL

Purpose:      Copies logicals from one array to another.

Category:     ARRAY

File:         move.c

Author:       K.G. Begeman

Use:          CALL MOVEL( SOURCE,       Input       LOGICAL ARRAY
                          DESTIN,       Output      LOGICAL ARRAY
                          NITEMS )      Input       INTEGER

              SOURCE   input array to be copied to DESTIN.
              DESTIN   output array.
              NITEMS   Number of elements to be copied.

Notes:        The arrays to be transferred may overlap.

Updates:      Jul 29, 1989 : KGB, Document created.

#<

@ subroutine movel( logical, logical, integer )

*/

void movel_c( bool *source, bool *destin, fint *nitems )
{
   (void) memmove( destin, source, (*nitems) * sizeof(bool) );
}

/*
#>            mover.dc2

Subroutine:   MOVER

Purpose:      Copies single precision reals from one array to another.

Category:     ARRAY

File:         move.c

Author:       K.G. Begeman

Use:          CALL MOVER( SOURCE,       Input       REAL ARRAY
                          DESTIN,       Output      REAL ARRAY
                          NITEMS )      Input       INTEGER

              SOURCE   input array to be copied to DESTIN.
              DESTIN   output array.
              NITEMS   Number of elements to be copied.

Notes:        The arrays to be transferred may overlap.

Updates:      Jul 29, 1989 : KGB, Document created.

#<

@ subroutine mover( real, real, integer )

*/

void mover_c( float *source, float *destin, fint *nitems )
{
   (void) memmove( destin, source, (*nitems) * sizeof(float) );
}

/*
#>            moved.dc2

Subroutine:   MOVED

Purpose:      Copies double precision reals from one array to another.

Category:     ARRAY

File:         move.c

Author:       K.G. Begeman

Use:          CALL MOVED( SOURCE,       Input       DOUBLE ARRAY
                          DESTIN,       Output      DOUBLE ARRAY
                          NITEMS )      Input       INTEGER

              SOURCE   input array to be copied to DESTIN.
              DESTIN   output array.
              NITEMS   Number of elements to be copied.

Notes:        The arrays to be transferred may overlap.

Updates:      Jul 29, 1989 : KGB, Document created.

#<

@ subroutine moved( double precision, double precision, integer )

*/

void moved_c( double *source, double *destin, fint *nitems )
{
   (void) memmove( destin, source, (*nitems) * sizeof(double) );
}

#if defined(TESTBED)
void main()
{
   fint   i1[10], i2[10];
   bool   l1[10], l2[10];
   float  r1[10], r2[10];
   double d1[10], d2[10];
   fint   i, n = 10;
   for (i = 0; i < n; i++) {
      i1[i] = i;
      l1[i] = (i > 4 ? 1 : 0);
      r1[i] = (float) i;
      d1[i] = (double) i;
   }
   movei_c( i1, i2, &n );
   movel_c( l1, l2, &n );
   mover_c( r1, r2, &n );
   moved_c( d1, d2, &n );
   for (i = 0; i < n; i++) {
      if (i1[i] == i2[i]) printf("%ld I",i); else printf("%ld  ",i);
      if (l1[i] == l2[i]) printf(" L"); else printf("  ");
      if (r1[i] == r2[i]) printf(" R"); else printf("  ");
      if (d1[i] == d2[i]) printf(" D\n"); else printf("  \n");
   }
}
#endif
