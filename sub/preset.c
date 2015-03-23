/* preset.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            preset.dc2

Document:     PRESET

Purpose:      Describes the available routines which fill an array with
              a value.

Category:     ARRAY

File:         preset.c

Author:       K.G. Begeman

Description:  The available routines are the following:
              PRESETI( SOURCE, DESTIN, NITEMS ) fills an integer array
              PRESETL( SOURCE, DESTIN, NITEMS ) fills a logical array
              PRESETR( SOURCE, DESTIN, NITEMS ) fills a real array
              PRESETD( SOURCE, DESTIN, NITEMS ) fills a double array

Updates:      Jul 29, 1989: KGB, Document created

#<

*/

#include "stdio.h"
#include "gipsyc.h"

static void fillbytes( char *source, char *destin, size_t size, fint nitems )
{
   char *d = destin;
   char *s;
   int   n;

   while (nitems--) for (n = 0, s = source; n++ < size; *d++ = *s++);
}

/*
#>            preseti.dc2

Subroutine:   PRESETI

Purpose:      Presets an integer array to a constant value.

Category:     ARRAY

File:         preset.c

Author:       K.G. Begeman

Use:          CALL PRESETI( SOURCE,     Input      INTEGER
                            DESTIN,     Output     INTEGER ARRAY
                            NITEMS )    Input      INTEGER

              SOURCE    Value with which to fill DESTIN.
              DESTIN    Array which receives SOURCE.
              NITEMS    Number of items to fill in DESTIN.

Updates:      Jul 29, 1989: KGB, Document created.

#<

@ subroutine preseti( integer, integer, integer )

*/

void preseti_c( fint *source, fint *destin, fint *nitems )
{
   fillbytes( (char *) source, (char *) destin, sizeof( fint ), *nitems );
}

/*
#>            presetl.dc2

Subroutine:   PRESETL

Purpose:      Presets a logical array to a constant value.

Category:     ARRAY

File:         preset.c

Author:       K.G. Begeman

Use:          CALL PRESETL( SOURCE,     Input      LOGICAL
                            DESTIN,     Output     LOGICAL ARRAY
                            NITEMS )    Input      INTEGER

              SOURCE    Value with which to fill DESTIN.
              DESTIN    Array which receives SOURCE.
              NITEMS    Number of items to fill in DESTIN.

Updates:      Jul 29, 1989: KGB, Document created.

#<

@ subroutine presetl( logical, logical, integer )

*/

void presetl_c( bool *source, bool *destin, fint *nitems )
{
   fillbytes( (char *) source, (char *) destin, sizeof( bool ), *nitems );
}

/*
#>            presetr.dc2

Subroutine:   PRESETR

Purpose:      Presets a single precision array to a constant value.

Category:     ARRAY

File:         preset.c

Author:       K.G. Begeman

Use:          CALL PRESETR( SOURCE,     Input      REAL
                            DESTIN,     Output     REAL ARRAY
                            NITEMS )    Input      INTEGER

              SOURCE    Value with which to fill DESTIN.
              DESTIN    Array which receives SOURCE.
              NITEMS    Number of items to fill in DESTIN.

Updates:      Jul 29, 1989: KGB, Document created.

#<

@ subroutine presetr( real, real, integer )

*/

void presetr_c( float *source, float *destin, fint *nitems )
{
   fillbytes( (char *) source, (char *) destin, sizeof( float ), *nitems );
}

/*
#>            presetd.dc2

Subroutine:   PRESETD

Purpose:      Presets a double precision array to a constant value.

Category:     ARRAY

File:         preset.c

Author:       K.G. Begeman

Use:          CALL PRESETD( SOURCE,     Input      DOUBLE PRECISION
                            DESTIN,     Output     DOUBLE ARRAY
                            NITEMS )    Input      INTEGER

              SOURCE    Value with which to fill DESTIN.
              DESTIN    Array which receives SOURCE.
              NITEMS    Number of items to fill in DESTIN.

Updates:      Jul 29, 1989: KGB, Document created.

#<

@ subroutine presetd( double precision, double precision, integer )

*/

void presetd_c( double *source, double *destin, fint *nitems )
{
   fillbytes( (char *) source, (char *) destin, sizeof( double ), *nitems );
}

#if defined(TESTBED)
void main()
{
   fint   i1, i2[10];
   bool   l1, l2[10];
   float  r1, r2[10];
   double d1, d2[10];
   fint    i, n = 10;
   i1 = 10;
   l1 = 1;
   r1 = 1.2;
   d1 = 3.4;
   preseti_c( &i1, i2, &n );
   presetl_c( &l1, l2, &n );
   presetr_c( &r1, r2, &n );
   presetd_c( &d1, d2, &n );
   for (i = 0; i < n; i++) {
      if (i1 == i2[i]) printf("%ld I",i); else printf("%ld  ",i);
      if (l1 == l2[i]) printf(" L"); else printf("  ");
      if (r1 == r2[i]) printf(" R"); else printf("  ");
      if (d1 == d2[i]) printf(" D\n"); else printf("\n");
   }
}
#endif
