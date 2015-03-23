/* clipper.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            clipper.dc2

Subroutine:   CLIPPER

Purpose:      Subroutine to conditionally transfer data.

Category:     ARRAY

Files:        clipper.c

Author:       K.G. Begeman

Use:          CALL CLIPPER( CLIPLO ,    Input   REAL
                            CLIPUP ,    Input   REAL
                            INDATA ,    Input   REAL ARRAY
                            TEDATA ,    Input   REAL ARRAY
                            OUDATA ,    Output  REAL ARRAY
                            NDATA  ,    Input   INTEGER
                            REPLACE )   Input   REAL

              CLIPLO        Lower clip level.
              CLIPUP        Upper clip level.
              INDATA        Data which will be conditionally transferred
                            to OUDATA.
              TEDATA        Data used for testing the conditions.
              OUDATA        Array which will receive data from INDATA
                            or contains values equal to REPLACE.
              NDATA         Number of elements in INDATA, TEDATA and
                            OUDATA.
              REPLACE       Data not transferred will be replaced by
                            this value.

Description:  Data will be transferred unchanged if testdatum has a
              value outside the CLIP range, (!) CLIPLO is greater than
              CLIPUP, then the data will transferred unchanged when
              testdatum from TEDATA has a value in the clip range.
              Changed data has the value of REPLACE.

Updates:      Feb 28, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine clipper( real, real, real, real, real, integer, real )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"setfblank.h"		/* define setfblank_c */

void clipper_c( float *cl ,
                float *cu ,
                float *id ,
                float *td ,
                float *od ,
                fint  *nd ,
                float *rp )
{
   float blank;

   setfblank_c( &blank );               /* get BLANK value */
   if (*cl < *cu) {
      fint n;

      for (n = 0; n < *nd; n++) {
         float i = *id++;
         float t = *td++;

         if (t == blank) {
            *od++ = blank;			/* stays blank */
         } else if (*cu < t || t < *cl) {
            *od++ = i;				/* is copied */
         } else {
            *od++ = *rp;                        /* becomes replace value */
         }
      }
   } else {
      fint n;

      for (n = 0; n < *nd; n++) {
         float i = *id++;
         float t = *td++;

         if (t == blank) {
            *od++ = blank;			/* stays blank */
         } else if (*cl < t || t < *cu) {
            *od++ = *rp;			/* becomes replace value */
         } else {
            *od++ = i;				/* is copied */
         }
      }
   }
}

#if defined(TESTBED)
void main()
{
   float blank;
   float id[10] = { 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0 };
   float td[10] = { 9.0, 8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0, 0.0 };
   float od1[10];
   float od2[10];
   float cl;
   float cu;
   fint  n;
   fint  nd = 10;

   setfblank_c( &blank );
   cl = 2.5; cu = 6.5; clipper_c( &cl, &cu, id, td, od1, &nd, &blank );
   cl = 6.5; cu = 2.5; clipper_c( &cl, &cu, id, td, od2, &nd, &blank );
   for (n = 0; n < nd; n++) {
      printf( "od[%d] ", n );
      if (blank == od1[n]) {
         printf( " BLANK" );
      } else {
         printf( " %5.3f", od1[n] );
      }
      if (blank == od2[n]) {
         printf( " BLANK\n" );
      } else {
         printf( " %5.3f\n", od2[n] );
      }
   }
}
#endif
