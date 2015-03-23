/* convolve.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            convolve.dc2

Function:     CONVOLVE

Purpose:      Convolves data with two dimensional convolution function.
              Takes care of BLANKs.

Category:     MATH

File:         convolve.c

Author:       K.G. Begeman

Use:          INTEGER CONVOLVE( CONFIE ,      Input    REAL ARRAY
                                NCONX  ,      Input    INTEGER
                                NCONY  ,      Input    INTEGER
                                DATIN  ,      Input    REAL ARRAY
                                DATOUT ,      Output   REAL ARRAY
                                NDATX  ,      Input    INTEGER
                                NDATY  )      Input    INTEGER

              CONVOLVE  Returns:
                        0 : succesfull convolution
                        1 : NDATX < NCONX
                        2 : NDATY < NCONY
                        3 : NCONX not an odd number
                        4 : NCONY not an odd number
              CONFIE    Array containing the convolution function.
                        For a convolution function c(x,y), the
                        function values are stored as follows:
                        CONFIE(K) = c(x,y) where
                        K = (y+(NCONY-1)/2)*NCONX+x+(NCONX-1)/2+1
                        So the centre of the convolution function is
                        at (NCONY-1)/2 * NCONX + (NCONX-1)/2 + 1, and
                        NCONX and NCONY must be odd. When ever a value
                        in CONFIE is equal to zero it will not be used
                        i.e. BLANKs will not propagate.
              NCONX     Number of pixels along the X axis of the
                        convolution function.
              NCONY     Number of pixels along the Y axis of the
                        convolution function.
              DATIN     Array containing the data to be convolved.
                        The size of DATIN is equalt to NDATX * NDATY.
              DATOUT    Array containing the convolved data. The
                        size of the array is equal to the size of
                        DATIN. DATOUT will contain BLANKs where the
                        convolution is not valid. This can be because
                        of BLANKs in DATIN, or because the distance
                        from the edge is less than (NCONX-1)/2 or
                        (NCONY-1)/2.
              NDATX     Number of pixels along the X axis of the
                        data to be convolved.
              NDATY     Number of pixels along the Y axis of the
                        data to be convolved.

Update:       Apr  8, 1990: KGB, Document created.

#<

Fortran to C interface:

@ integer function convolve( real    ,
@                            integer ,
@                            integer ,
@                            real    ,
@                            real    ,
@                            integer ,
@                            integer )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"qcnvl1.h"		/* defines qcnvl1_c */
#include	"qcnvl2.h"		/* defines qcnvl2_c */
#include	"setfblank.h"		/* defines setfblank_c */

static	float	BLANK;                  /* system defined BLANK value */

fint convolve_c( float *cfie,
                 fint  *ncx,
                 fint  *ncy,
                 float *dat1,
                 float *dat2,
                 fint  *ndx,
                 fint  *ndy )
{
   fint	bcx = *ncx / 2;
   fint	bcy = *ncy / 2;
   fint	icx = *ncx / 2 + 1;
   fint	icy = *ncy / 2 + 1;
   fint	nblank = 0;
   fint	ntx = *ndx - *ncx + 1;
   fint	nty = *ndy - *ncy + 1;
   fint	x, y;
   fint	ier = 0;
   fint	k;

   if (ntx < 1) ier = 1;                /* x size too small */
   if (nty < 1) ier = 2;                /* y size too small */
   if ((icx + bcx) != *ncx) ier = 4;    /* odd x size of convolution function */
   if ((icy + bcy) != *ncy) ier = 8;    /* odd y size of convolution function */
   if (ier) return( ier );              /* stop if error */
   setfblank_c( &BLANK );               /* get system defined BLANK value */
   for (k = 0, y = 0; y < *ndy; y++) {
      for (x = 0; x < *ndx; x++) {
         if (dat1[k] == BLANK) nblank++;	/* count blanks */
         if (x < bcx || x > (*ndx - icx)) {	/* outside valid x range */
            dat2[k++] = BLANK;
         } else if (y < bcy || y > (*ndy - icy)) {	/* outside valid y range */
            dat2[k++] = BLANK;
         } else {                       /* convolution result is valid */
            dat2[k++] = 0.0;
         }
      }
   }
   for (y = 0; y < *ncy; y++) {
      for (x = 0; x < *ncx; x++) {
         float cf = cfie[*ncx - x - 1 + (*ncy - y - 1) * (*ncx)];

         if (cf != 0.0) {
            fint y2;

            for (y2 = 0; y2 < nty; y2++) {
               fint y1 = y + y2;
               float	*v1;
               float	*v2;

               v1 = &dat1[y1 * (*ndx) + x];
               v2 = &dat2[(bcy + y2) * (*ndx) + bcx];
               if (!nblank) {
                  qcnvl1_c( v1, v2, &cf, &ntx );
               } else {
                  qcnvl2_c( v1, v2, &cf, &ntx );
               }
            }
         }
      }
   }
   return( ier );
}

#if	defined(TESTBED)

#include	"stdlib.h"
#include	"time.h"
#include	"cmain.h"

#define	NCX	9
#define	NCY	9
#define	NDX	512
#define	NDY	512

MAIN_PROGRAM_ENTRY
{
#if	defined(TIMER)
   fint  ncx = NCX;
   fint  ncy = NCY;
   fint  ndx = NDX;
   fint  ndy = NDY;
   fint x, y, n;
   float cfie[NCX*NCY];
   float dat1[NDX*NDY];
   float dat2[NDX*NDY];
   clock_t ct1, ct2;
   time_t  rt1, rt2;
   double  dct, drt;

   for (n = 0, y = 0; y < NDY; y++) {
      for (x = 0; x < NDX; x++, n++) {
         dat1[n] = 2.0 * (float) rand( ) / (float) RAND_MAX - 1.0;
      }
   }
   for (n = 0, y = 0; y < NCY; y++) {
      for (x = 0; x < NCX; x++, n++) {
         cfie[n] = 1.0 / (float) ( NCX * NCY );
      }
   }
#if	defined(DO_BLANK)
   setfblank_c( &BLANK );
   dat1[1000] = BLANK;
#endif
   ct1 = clock( );
   rt1 = time (NULL);
   convolve_c( cfie, &ncx, &ncy, dat1, dat2, &ndx, &ndy );
   ct2 = clock( );
   rt2 = time( NULL );
   dct = (ct2 - ct1) / CLK_TCK;
   drt = difftime( rt2, rt1 );
/*
   printf( "real time = %f seconds\n", drt );
   printf( "cpu  time = %f seconds\n", dct );
*/
#else
   static fint  ncx = 3;
   static fint  ncy = 3;
   static fint  ndx = 10;
   static fint  ndy = 10;
   fint x, y, n;
   static float cfie[9] = {
      1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0
   };
   static float dat1[100] = {
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
   };
   static float dat2[100] = {
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0
   };

   setfblank_c( &BLANK );
   dat1[0] = 1.0;
   dat1[35] = 1.0;
   dat1[69] = 1.0;
   dat1[75] = BLANK;
   printf( "convolve: %ld\n",  convolve_c( cfie, &ncx, &ncy, dat1, dat2, &ndx, &ndy ) );
   printf( "original data\n" );
   for (n = 0, y = 0; y < ndy; y++) {
      for (x = 0; x < ndx; x++, n++) {
         if (dat1[n] == BLANK) {
            printf( " BLANK " );
         } else {
            printf( "%6.2f ", dat1[n] );
         }
      }
      printf( "\n" );
   }
   printf( "convolved data\n" );
   for (n = 0, y = 0; y < ndy; y++) {
      for (x = 0; x < ndx; x++, n++) {
         if (dat2[n] == BLANK) {
            printf( " BLANK " );
         } else {
            printf( "%6.2f ", dat2[n] );
         }
      }
      printf( "\n" );
   }
#endif
}
#endif
