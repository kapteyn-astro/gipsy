/* cnvrt.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            cnvrt.dc2

Document:     CNVRT

Purpose:      Describes the data conversion routines.

Category:     SYSTEM

File:         cnvrt.c

Author:       K.G. Begeman

Description:  The following routines for data conversion are available:
              SUBROUTINE CNVRTC      one byte character conversion
              SUBROUTINE CNVRTH      two byte integer conversion
              SUBROUTINE CNVRTF      four byte integer conversion
              SUBROUTINE CNVRTE      single precision f.p. conversion
              SUBROUTINE CNVRTD      double precision f.p. conversion
              Each routine is described in more detail in the
              accompanying document.
              These routines convert foreign data to the format for
              the current machine. The routines are setup so that
              conversions of any type of foreign data from the same
              foreign machine can be specified with the same foreign
              type. Data from VMS machines are specified with
              type = 1, data from ALLIANT, SUN, CONVEX (UNIX machines)
              with type = 2, data from IBM machines with type = 3, data
              from MSDOS PC's and DECstations with type 4 and data from
              CONVEXs with type 5.

Warnings:     Machine dependent! Currently implemented for VMS, SUN,
              CONVEX, ALLIANT, HP 9000 and MSDOS PC's.

Updates:      Jul 14, 1989: KGB, document created.

#<

*/

#include	"stdio.h"
#include	"gipsyc.h"

#if	OS_FLOATING_TYPE == 0 | OS_FLOATING_TYPE == 5

#define	IEEE_H

#elif	OS_FLOATING_TYPE == 1 | OS_FLOATING_TYPE == 6

#define	IEEE_L

#elif	OS_FLOATING_TYPE == 2

#define	CONVEX_N

#elif	OS_FLOATING_TYPE == 3

#define	VMS_N

#else

NOT IMPLEMENTED

#endif

typedef unsigned char byte;                                  /* define a byte */

static byte ASCEBC[256] = {               /* Conversion table EBCDIC to ASCII */
     0,   1,   2,   3,   0,   9,   0, 127,
     0,   0,   0,  11,  12,  13,  14,  15,
    16,  17,  18,   0,   0,   0,   8,   0,
    24,  25,   0,   0,  28,  29,  30,  31,
     0,   0,  28,   0,   0,  10,  23,  27,
     0,   0,   0,   0,   0,   5,   6,   7,
     0,   0,  24,   0,   0,  30,   0,   4,
     0,   0,   0,  19,  20,  21,   0,  26,
    32,   0,   0,   0,   0,   0,   0,   0,
     0,   0,   0,  46,  60,  40,  43, 124,
    38,   0,   0,   0,   0,   0,   0,   0,
     0,   0,  33,  36,  42,  41,  59,  94,
    45,  47,   0,   0,   0,   0,   0,   0,
     0,   0, 124,  44,  37,  95,  62,  63,
     0,   0,   0,   0,   0,   0,   0,   0,
     0,  96,  58,  35,  64,  39,  61,  34,
     0,  97,  98,  99, 100, 101, 102, 103,
   104, 105,   0,   0,   0,   0,   0,   0,
     0, 106, 107, 108, 109, 110, 111, 112,
   113, 114,   0,   0,   0,   0,   0,   0,
     0, 126, 115, 116, 117, 118, 119, 120,
   121, 122,   0,   0,   0,   0,   0,   0,
     0,   0,   0,   0,   0,   0,   0,   0,
     0,   0,   0,   0,   0,   0,   0,   0,
   123,  65,  66,  67,  68,  69,  70,  71,
    72,  73,   0,   0,   0,   0,   0,   0,
   125,  74,  75,  76,  77,  78,  79,  80,
    81,  82,   0,   0,   0,   0,   0,   0,
    92,   0,  83,  84,  85,  86,  87,  88,
    89,  90,   0,   0,   0,   0,   0,   0,
    48,  49,  50,  51,  52,  53,  54,  55,
    56,  57, 124,   0,   0,   0,   0,   0
};

/*
#>            cnvrtc.dc2

Subroutine:   CNVRTC

Purpose:      Converts bytes from foreign machines to the format
              supported by the current machine.

Category:     SYSTEM

File:         cnvrt.c

Author:       K.G. Begeman

Use:          CALL CNVRTC( INTYPE,       Input     INTEGER
                           INBYTE,       Input     BYTE ARRAY
                           OSBYTE,       Output    BYTE ARRAY
                           NBYTES )      Input     INTEGER

              INTYPE     Type of input format. Can be 1 (VMS),
                         2 (UNIX), 3 (IBM), 4 (MSDOS,DECstations)
                         or 5 (CONVEX).
              INBYTE     Array of bytes which contains the foreign
                         data.
              OSBYTE     Array of bytes which contains the converted
                         results.
              NBYTES     Number of bytes to be converted.

Warnings:     Machine dependent! Currently implemented for VMS, SUN,
              CONVEX, ALLIANT, HP 9000  and MSDOS PC's.

Updates:      Jul 14, 1989: KGB, document created.

#<

@ subroutine cnvrtc( integer, integer, integer, integer )

*/


void cnvrtc_c( fint *arg1, byte *arg2, byte *arg3, fint *arg4 )
{
   fint ta = *arg1;
   byte *a1 = arg2, *a2 = arg3;
   fint na = *arg4;
   fint ja;

   switch(ta) {
      case 1:						/* VMS_N to OS */
      case 2:						/* IEEE_H to OS */
      case 4:						/* IEEE_L to OS */
      case 5: {						/* CONVEX_N to OS */
         if (a1 != a2) for (ja = 0; ja++ < na; *a2++ = *a1++);
         break;
      }
      case 3: {						/* IBM to OS */
         for (ja = 0; ja++ < na; *a2++ = ASCEBC[*a1++]);
         break;
      }
      default: {
         break;
      }
   }
}

/*
#>            cnvrth.dc2

Subroutine:   CNVRTH

Purpose:      Converts two byte integers from foreign machines to the
              format supported by the current machine.

Category:     SYSTEM

File:         cnvrt.c

Author:       K.G. Begeman

Use:          CALL CNVRTH( INTYPE,       Input     INTEGER
                           INBYTE,       Input     BYTE ARRAY
                           OSSHRT,       Output    SHORT ARRAY
                           NSHRTS )      Input     INTEGER

              INTYPE     Type of input format. Can be 1 (VMS),
                         2 (UNIX), 3 (IBM), 4 (MSDOS,DECstations)
                         or r (CONVEX).
              INBYTE     Array of bytes which contains the foreign
                         data.
              OSBYTE     Array of shorts which contains the converted
                         results.
              NBYTES     Number of shorts to be converted.

Warnings:     Machine dependent! Currently implemented for VMS, SUN,
              CONVEX, ALLIANT, HP 9000  and MSDOS PC's.

Notes:        On the machines currently supported a short contains
              two bytes.

Updates:      Jul 14, 1989: KGB, document created.

#<

@ subroutine cnvrth( integer, integer, integer, integer )

*/


void cnvrth_c( fint *arg1, byte *arg2, byte *arg3, fint *arg4 )
{
   fint th = *arg1;
   byte *h1 = arg2, *h2 = arg3;
   fint nh = *arg4;
   byte b;
   fint jh;

   switch(th) {
      case 1:						/* VMS_N to OS */
      case 4: {						/* IEEE_L to OS */
#if	defined(IEEE_L) | defined(VMS_N)
         if (h1 != h2) {
            nh *= 2;
            for (jh = 0; jh++ < nh; *h2++ = *h1++);
         }
#elif	defined(IEEE_H) | defined(CONVEX_N)
         for (jh = 0; jh < nh; jh++) {
            b = *h1++; *h2++ = *h1++; *h2++ = b;
         }
#endif
         break;
      }
      case 2:						/* IEEE_H to OS */
      case 3:						/* IBM to OS */
      case 5: {						/* CONVEX_N to OS */
#if	defined(IEEE_L) | defined(VMS_N)
         for (jh = 0; jh < nh; jh++) {
            b = *h1++; *h2++ = *h1++; *h2++ = b;
         }
#elif	defined(IEEE_H) | defined(CONVEX_N)
         if (h1 != h2) {
            nh *= 2;
            for (jh = 0; jh++ < nh; *h2++ = *h1++);
         }
#endif
         break;
      }
      default: {
         break;
      }
   }
}

/*
#>            cnvrtf.dc2

Subroutine:   CNVRTF

Purpose:      Converts integers from foreign machines to the format
              supported by the current machine.

Category:     SYSTEM

File:         cnvrt.c

Author:       K.G. Begeman

Use:          CALL CNVRTF( INTYPE,       Input     INTEGER
                           INBYTE,       Input     BYTE ARRAY
                           OSINT,        Output    INTEGER ARRAY
                           NINTS )       Input     INTEGER

              INTYPE     Type of input format. Can be 1 (VMS),
                         2 (UNIX), 3 (IBM), 4 (MSDOS,DECstations)
                         or 5 (CONVEX).
              INBYTE     Array of bytes which contains the foreign
                         data.
              OSBYTE     Array of integers which contains the converted
                         results.
              NBYTES     Number of integers to be converted.

Warnings:     Machine dependent! Currently implemented for VMS, SUN,
              CONVEX, ALLIANT, HP 9000  and MSDOS PC's.

Updates:      Jul 14, 1989: KGB, document created.

#<

@ subroutine cnvrtf( integer, integer, integer, integer )

*/


void cnvrtf_c( fint *arg1, byte *arg2, byte *arg3, fint *arg4 )
{
   fint tf = *arg1;
   byte *f1 = arg2, *f2 = arg3;
   fint nf = *arg4;
   byte b[4];
   fint jf;

   switch(tf) {
      case 1:						/* VMS_N to OS */
      case 4: {						/* IEEE_L to OS */
#if	defined(IEEE_L) | defined(VMS_N)
         if (f1 != f2) {
            nf *= 4;
            for (jf = 0; jf++ < nf; *f2++ = *f1++);
         }
#elif	defined(IEEE_H) | defined(CONVEX_N)
         for (jf = 0; jf < nf; jf++) {
            b[0] = *f1++; b[1] = *f1++; b[2] = *f1++; b[3] = *f1++;
            *f2++ = b[3]; *f2++ = b[2]; *f2++ = b[1]; *f2++ = b[0];
         }
#endif
         break;
      }
      case 2:						/* IEEE_H to OS */
      case 3:						/* IBM to OS */
      case 5: {						/* CONVEX_N to OS */
#if	defined(IEEE_L) | defined(VMS_N)
         for (jf = 0; jf < nf; jf++) {
            b[0] = *f1++; b[1] = *f1++; b[2] = *f1++; b[3] = *f1++;
            *f2++ = b[3]; *f2++ = b[2]; *f2++ = b[1]; *f2++ = b[0];
         }
#elif	defined(IEEE_H) | defined(CONVEX_N)
         if (f1 != f2) {
            nf *= 4;
            for (jf = 0; jf++ < nf; *f2++ = *f1++);
         }
#endif
         break;
      }
      default: {
         break;
      }
   }
}

/*
#>            cnvrte.dc2

Subroutine:   CNVRTE

Purpose:      Converts single precision floating point numbers from
              foreign machines the format supported by the current
              machine.

Category:     SYSTEM

File:         cnvrt.c

Author:       K.G. Begeman

Use:          CALL CNVRTE( INTYPE,       Input     INTEGER
                           INBYTE,       Input     BYTE ARRAY
                           OSREAL,       Output    REAL ARRAY
                           NREALS )      Input     INTEGER

              INTYPE     Type of input format. Can be 1 (VMS),
                         2 (UNIX), 3 (IBM), 4 (MSDOS,DECstations)
                         or 5 (CONVEX).
              INBYTE     Array of bytes which contains the foreign
                         data.
              OSBYTE     Array of reals which contains the converted
                         results.
              NBYTES     Number of reals to be converted.

Warnings:     Machine dependent! Currently implemented for VMS, SUN,
              CONVEX, ALLIANT, HP 9000 and MSDOS PC's.

Updates:      Jul 14, 1989: KGB, document created.

#<

@ subroutine cnvrte( integer, real, real, integer )

*/


void cnvrte_c( fint *arg1, byte *arg2, byte *arg3, fint *arg4 )
{
   fint te = *arg1;
   byte *e1 = arg2, *e2 = arg3;
   fint ne = *arg4;
   union {
      float f;
      fint  i;
      byte  b[4];
   } u;
   fint ja, jb, je;

   switch(te) {
      case 1: {						/* VMS_N to OS */
#if	defined(VMS_N)
         if (e1 != e2) {
            ne *= 4;
            for (je = 0; je++ < ne; *e2++ = *e1++);
         }
#elif	defined(CONVEX_N)
         for (je = 0; je < ne; je++) {
            u.b[1] = *e1++; u.b[0] = *e1++; u.b[3] = *e1++; u.b[2] = *e1++;
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
         }
#elif	defined(IEEE_L) | defined(IEEE_H)
         for (je = 0; je < ne; je++) {
#if	defined(IEEE_H)
            u.b[1] = *e1++; u.b[0] = *e1++; u.b[3] = *e1++; u.b[2] = *e1++;
#elif	defined(IEEE_L)
            u.b[2] = *e1++; u.b[3] = *e1++; u.b[0] = *e1++; u.b[1] = *e1++;
#endif
            u.f /= 4.0;
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
         }
#endif
         break;
      }
      case 2: {						/* IEEE_H to OS */
#if	defined(VMS_N)
         for (je = 0; je < ne; je++) {
            u.b[1] = *e1++; u.b[0] = *e1++; u.b[3] = *e1++; u.b[2] = *e1++;
            u.f *= 4.0;
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
         }
#elif	defined(IEEE_H)
         if (e1 != e2) {
            ne *= 4;
            for (je = 0; je++ < ne; *e2++ = *e1++);
         }
#elif	defined(IEEE_L)
         for (je = 0; je < ne; je++) {
            u.b[0] = *e1++; u.b[1] = *e1++; u.b[2] = *e1++; u.b[3] = *e1++;
            *e2++ = u.b[3]; *e2++ = u.b[2]; *e2++ = u.b[1]; *e2++ = u.b[0];
         }
#elif	defined(CONVEX_N)
         for (je = 0; je < ne; je++) {
            u.b[0] = *e1++; u.b[1] = *e1++; u.b[2] = *e1++; u.b[3] = *e1++;
            u.f *= 4.0;
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
         }
#endif
         break;
      }
      case 3: {						/* IBM to OS */
         for (je = 0; je < ne; je++) {
#if	defined(VMS_N)
            jb = u.b[3] = *e1++; u.b[2] = *e1++; u.b[1] = *e1++; u.b[0] = *e1++;
#elif	defined(IEEE_H) | defined(CONVEX_N)
            jb = u.b[0] = *e1++; u.b[1] = *e1++; u.b[2] = *e1++; u.b[3] = *e1++;
#elif	defined(IEEE_L)
            u.b[3] = *e1++; u.b[2] = *e1++; u.b[1] = *e1++; jb = u.b[0] = *e1++;
#endif
#if	defined(VMS_N) | defined(IEEE_L) | defined(IEEE_H) | defined(CONVEX_N)
            ja = 4 * ((u.i & 0x7f000000) >> 24 ) - 128;
            if (ja <= 0) {
               u.i = 0;
            } else {
               if ((u.i & 0xffffff) == 0) {
                  u.i = 0;
               } else {
                  while ((ja > 0) && ((u.i & 0x800000 ) == 0)) {
                     u.i = ((u.i & 0xffffff) << 1);
                     ja = ja - 1;
                  };
                  if (ja <= 0) {
                     u.i = 0;
                  } else {
                     if (ja >= 256) {
                        u.i = -1;
                        ja = 255;
                     }
                     u.i = ((jb << 24) & 0x80000000) + (ja << 23) +
                           (u.i & 0x7fffff);
                  }
               }
            }
#endif
#if	defined(VMS_N)
            *e2++ = u.b[2]; *e2++ = u.b[3]; *e2++ = u.b[0]; *e2++ = u.b[1];
#elif	defined(IEEE_L) | defined(IEEE_H)
            u.f /= 4.0;
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
#elif	defined(CONVEX_N)
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
#endif
         }
         break;
      }
      case 4: {						/* IEEE_L to OS */
#if	defined(VMS_N)
         for (je = 0; je < ne; je++) {
            u.b[2] = *e1++; u.b[3] = *e1++; u.b[0] = *e1++; u.b[1] = *e1++;
            u.f *= 4.0;
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
         }
#elif	defined(IEEE_L)
         if (e1 != e2) {
            ne *= 4;
            for (je = 0; je++ < ne; *e2++ = *e1++);
         }
#elif	defined(IEEE_H)
         for (je = 0; je < ne; je++) {
            u.b[0] = *e1++; u.b[1] = *e1++; u.b[2] = *e1++; u.b[3] = *e1++;
            *e2++ = u.b[3]; *e2++ = u.b[2]; *e2++ = u.b[1]; *e2++ = u.b[0];
         }
#elif	defined(CONVEX_N)
         for (je = 0; je < ne; je++) {
            u.b[3] = *e1++; u.b[2] = *e1++; u.b[1] = *e1++; u.b[0] = *e1++;
            u.f *= 4.0;
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
         }
#endif
         break;
      }
      case 5: {						/* CONVEX_N to OS */
#if	defined(VMS_N)
         for (je = 0; je < ne; je++) {
            u.b[1] = *e1++; u.b[0] = *e1++; u.b[3] = *e1++; u.b[2] = *e1++;
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
         }
#elif	defined(CONVEX_N)
         if (e1 != e2) {
            ne *= 4;
            for (je = 0; je++ < ne; *e2++ = *e1++);
         }
#elif	defined(IEEE_L)
         for (je = 0; je < ne; je++) {
            u.b[3] = *e1++; u.b[2] = *e1++; u.b[1] = *e1++; u.b[0] = *e1++;
            u.f /= 4.0;
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
         }
#elif	defined(IEEE_H)
         for (je = 0; je < ne; je++) {
            u.b[0] = *e1++; u.b[1] = *e1++; u.b[2] = *e1++; u.b[3] = *e1++;
            u.f /= 4.0;
            *e2++ = u.b[0]; *e2++ = u.b[1]; *e2++ = u.b[2]; *e2++ = u.b[3];
         }
#endif
         break;
      }
      default: {
         break;
      }
   }
}

/*
#>            cnvrtd.dc2

Subroutine:   CNVRTD

Purpose:      Converts double precision floating point numbers from
              foreign machines the format supported by the current
              machine.

Category:     SYSTEM

File:         cnvrt.c

Author:       K.G. Begeman

Use:          CALL CNVRTD( INTYPE,       Input     INTEGER
                           INBYTE,       Input     BYTE ARRAY
                           OSDBLE,       Output    DOUBLE ARRAY
                           NDBLES )      Input     INTEGER

              INTYPE     Type of input format. Can be 1 (VMS),
                         2 (UNIX), 3 (IBM), 4 (MSDOS,DECstations)
                         or 5 (CONVEX).
              INBYTE     Array of bytes which contains the foreign
                         data.
              OSBYTE     Array of doubles which contains the converted
                         results.
              NDBLES     Number of doubles to be converted.

Warnings:     Machine dependent! Currently implemented for VMS, SUN,
              CONVEX, ALLIANT, HP 9000  and MSDOS PC's.

Updates:      Jul 14, 1989: KGB, document created.

#<

@ subroutine cnvrtd( integer, double precision, double precision, integer )

*/


void cnvrtd_c( fint *arg1, byte *arg2, byte *arg3, fint *arg4 )
{
   fint td = *arg1;
   byte *d1 = arg2, *d2 = arg3;
   fint nd = *arg4;
   union {
      double d;
      fint   i[2];
      byte   b[8];
   } u;
   fint ja, jb, jd;

   switch(td) {
      case 1: {						/* VMS_N to OS */
#if	defined(VMS_N)
         if (d1 != d2) {
            nd *= 8;
            for (jd = 0; jd++ < nd; *d2++ = *d1++);
         }
#endif
#if	defined(IEEE_L) | defined(IEEE_H) | defined(CONVEX_N)
         for (jd = 0; jd < nd; jd++) {
#if	defined(IEEE_H) | defined(CONVEX_N)
            u.b[1] = *d1++; u.b[0] = *d1++; u.b[3] = *d1++; u.b[2] = *d1++;
            u.b[5] = *d1++; u.b[4] = *d1++; u.b[7] = *d1++; u.b[6] = *d1++;
#elif	defined(IEEE_L)
            u.b[2] = *d1++; u.b[3] = *d1++; u.b[0] = *d1++; u.b[1] = *d1++;
            u.b[6] = *d1++; u.b[7] = *d1++; u.b[4] = *d1++; u.b[5] = *d1++;
#endif
            u.i[1] = ((u.i[1] >> 3) & 0x1fffffff) + ((u.i[0] & 0x7) << 29);
            u.i[0] = (u.i[0] & 0x80000000) + ((u.i[0] & 0x7fffff) >> 3) +
                     ((((u.i[0] & 0x7f800000) >> 23) - 130 + 1024) << 20);
#if	defined(CONVEX_N)
            u.d *= 4.0;
#endif
#if	defined(IEEE_H) | defined(CONVEX_N)
            *d2++ = u.b[0]; *d2++ = u.b[1]; *d2++ = u.b[2]; *d2++ = u.b[3];
            *d2++ = u.b[4]; *d2++ = u.b[5]; *d2++ = u.b[6]; *d2++ = u.b[7];
#elif	defined(IEEE_L)
            *d2++ = u.b[4]; *d2++ = u.b[5]; *d2++ = u.b[6]; *d2++ = u.b[7];
            *d2++ = u.b[0]; *d2++ = u.b[1]; *d2++ = u.b[2]; *d2++ = u.b[3];
#endif
         }
#endif
         break;
      }
      case 2: {						/* IEEE_H to OS */
#if	defined(VMS_N)
         for (jd = 0; jd < nd; jd++) {
            u.b[3] = *d1++; u.b[2] = *d1++; u.b[1] = *d1++; u.b[0] = *d1++;
            u.b[7] = *d1++; u.b[6] = *d1++; u.b[5] = *d1++; u.b[4] = *d1++;
            if (u.i[0] == 0) {
               u.i[1] = 0;
            } else {
               u.i[0] = (u.i[0] & 0x80000000) + ((u.i[0] & 0xfffff) << 3) +
                        ((((u.i[0] & 0x7ff00000) >> 20) + 130 - 1024) << 23) +
                        ((u.i[1] >> 29) & 0x7);
               u.i[1] = (u.i[1] << 3);
            }
            *d2++ = u.b[2]; *d2++ = u.b[3]; *d2++ = u.b[0]; *d2++ = u.b[1];
            *d2++ = u.b[6]; *d2++ = u.b[7]; *d2++ = u.b[4]; *d2++ = u.b[5];
         }
#elif	defined(IEEE_H)
         if (d1 != d2) {
            nd *= 8;
            for (jd = 0; jd++ < nd; *d2++ = *d1++);
         }
#elif	defined(IEEE_L)
         for (jd = 0; jd < nd; jd++) {
            u.b[3] = *d1++; u.b[2] = *d1++; u.b[1] = *d1++; u.b[0] = *d1++;
            u.b[7] = *d1++; u.b[6] = *d1++; u.b[5] = *d1++; u.b[4] = *d1++;
            *d2++ = u.b[4]; *d2++ = u.b[5]; *d2++ = u.b[6]; *d2++ = u.b[7];
            *d2++ = u.b[0]; *d2++ = u.b[1]; *d2++ = u.b[2]; *d2++ = u.b[3];
         }
#elif	defined(CONVEX_N)
         for (jd = 0; jd < nd; jd++) {
            u.b[0] = *d1++; u.b[1] = *d1++; u.b[2] = *d1++; u.b[3] = *d1++;
            u.b[4] = *d1++; u.b[5] = *d1++; u.b[6] = *d1++; u.b[7] = *d1++;
            u.d *= 4.0;
            *d2++ = u.b[0]; *d2++ = u.b[1]; *d2++ = u.b[2]; *d2++ = u.b[3];
            *d2++ = u.b[4]; *d2++ = u.b[5]; *d2++ = u.b[6]; *d2++ = u.b[7];
         }
#endif
         break;
      }
      case 3: {						/* IBM to OS */
         for (jd = 0; jd < nd; jd++) {
#if	defined(VMS_N)
            jb = u.b[3] = *d1++; u.b[2] = *d1++; u.b[1] = *d1++; u.b[0] = *d1++;
            u.b[7] = *d1++; u.b[6] = *d1++; u.b[5] = *d1++; u.b[4] = *d1++;
#elif	defined(IEEE_H) | defined(CONVEX_N)
            jb = u.b[0] = *d1++; u.b[1] = *d1++; u.b[2] = *d1++; u.b[3] = *d1++;
            u.b[4] = *d1++; u.b[5] = *d1++; u.b[6] = *d1++; u.b[7] = *d1++;
#elif	defined(IEEE_L)
            u.b[3] = *d1++; u.b[2] = *d1++; u.b[1] = *d1++; jb = u.b[0] = *d1++;
            u.b[7] = *d1++; u.b[6] = *d1++; u.b[5] = *d1++; u.b[4] = *d1++;
#endif
#if	defined(VMS_N) | defined(IEEE_L) | defined(IEEE_H) | defined(CONVEX_N)
            ja = 4 * ((u.i[0] & 0x7f000000) >> 24) - 128;
            if (ja <= 0) {
               u.i[0] = u.i[1] = 0;
            } else {
               if ((u.i[0] & 0xffffff) == 0) {
                  u.i[0] = u.i[1] = 0;
               } else {
                  while ((ja > 0) && ((u.i[0] & 0x800000) == 0)) {
                     u.i[0] = ((u.i[0] & 0xffffff) << 1);
                     if (u.i[1] < 0) u.i[0]++;
                     u.i[1] = (u.i[1] << 1);
                     ja--;
                  };
                  if (ja <= 0) {
                     u.i[0] = u.i[1] = 0;
                  } else {
                     if (ja > 255) {
                        u.i[0] = u.i[1] = -1;
                        ja = 255;
                     }
                     u.i[0] = ((jb << 24) & 0x80000000) + (ja << 23) +
                              (u.i[0] & 0x7fffff);
                  }
               }
            }
#endif
#if	defined(VMS_N)
            *d2++ = u.b[2]; *d2++ = u.b[3]; *d2++ = u.b[0]; *d2++ = u.b[1];
            *d2++ = u.b[6]; *d2++ = u.b[7]; *d2++ = u.b[4]; *d2++ = u.b[5];
#elif	defined(IEEE_L) | defined(IEEE_H) | defined(CONVEX_N)
            u.i[1] = ((u.i[1] >> 3) & 0x1fffffff) + ((u.i[0] & 0x7) << 29);
            u.i[0] = (u.i[0] & 0x80000000) + ((u.i[0] & 0x7fffff) >> 3) +
                     ((((u.i[0] & 0x7f800000) >> 23) - 130 + 1024) << 20);
#endif
#if	defined(CONVEX_N)
            u.d *= 4.0;
#endif
#if	defined(IEEE_H) | defined(CONVEX_N)
            *d2++ = u.b[0]; *d2++ = u.b[1]; *d2++ = u.b[2]; *d2++ = u.b[3];
            *d2++ = u.b[4]; *d2++ = u.b[5]; *d2++ = u.b[6]; *d2++ = u.b[7];
#elif	defined(IEEE_L)
            *d2++ = u.b[4]; *d2++ = u.b[5]; *d2++ = u.b[6]; *d2++ = u.b[7];
            *d2++ = u.b[0]; *d2++ = u.b[1]; *d2++ = u.b[2]; *d2++ = u.b[3];
#endif
         }
         break;
      }
      case 4: {						/* IEEE_L to OS */
#if	defined(VMS_N)
         for (jd = 0; jd < nd; jd++) {
            u.b[4] = *d1++; u.b[5] = *d1++; u.b[6] = *d1++; u.b[7] = *d1++;
            u.b[0] = *d1++; u.b[1] = *d1++; u.b[2] = *d1++; u.b[3] = *d1++;
            if (u.i[0] == 0) {
               u.i[1] = 0;
            } else {
               u.i[0] = (u.i[0] & 0x80000000) + ((u.i[0] & 0xfffff) << 3) +
                        ((((u.i[0] & 0x7ff00000) >> 20) + 130 - 1024) << 23) +
                        ((u.i[1] >> 29) & 0x7);
               u.i[1] = (u.i[1] << 3);
            }
            *d2++ = u.b[2]; *d2++ = u.b[3]; *d2++ = u.b[0]; *d2++ = u.b[1];
            *d2++ = u.b[6]; *d2++ = u.b[7]; *d2++ = u.b[4]; *d2++ = u.b[5];
         }
#elif	defined(IEEE_L)
         if (d1 != d2) {
            nd *= 8;
            for (jd = 0; jd++ < nd; *d2++ = *d1++);
         }
#elif	defined(IEEE_H)
         for (jd = 0; jd < nd; jd++) {
            u.b[3] = *d1++; u.b[2] = *d1++; u.b[1] = *d1++; u.b[0] = *d1++;
            u.b[7] = *d1++; u.b[6] = *d1++; u.b[5] = *d1++; u.b[4] = *d1++;
            *d2++ = u.b[4]; *d2++ = u.b[5]; *d2++ = u.b[6]; *d2++ = u.b[7];
            *d2++ = u.b[0]; *d2++ = u.b[1]; *d2++ = u.b[2]; *d2++ = u.b[3];
         }
#elif	defined(CONVEX_N)
         for (jd = 0; jd < nd; jd++) {
            u.b[7] = *d1++; u.b[6] = *d1++; u.b[5] = *d1++; u.b[4] = *d1++;
            u.b[3] = *d1++; u.b[2] = *d1++; u.b[1] = *d1++; u.b[0] = *d1++;
            u.d *= 4.0;
            *d2++ = u.b[0]; *d2++ = u.b[1]; *d2++ = u.b[2]; *d2++ = u.b[3];
            *d2++ = u.b[4]; *d2++ = u.b[5]; *d2++ = u.b[6]; *d2++ = u.b[7];
         }
#endif
         break;
      }
      case 5: {						/* CONVEX_N to OS */
#if	defined(VMS_N)
         for (jd = 0; jd < nd; jd++) {
            union { double d; byte b[8]; } s;

            u.b[3] = *d1++; u.b[2] = *d1++; u.b[1] = *d1++; u.b[0] = *d1++;
            u.b[7] = *d1++; u.b[6] = *d1++; u.b[5] = *d1++; u.b[4] = *d1++;
            if (u.i[0] == 0) {
               u.i[1] = 0;
            } else {
               u.i[0] = (u.i[0] & 0x80000000) + ((u.i[0] & 0xfffff) << 3) +
                        ((((u.i[0] & 0x7ff00000) >> 20) + 130 - 1024) << 23) +
                        ((u.i[1] >> 29) & 0x7);
               u.i[1] = (u.i[1] << 3);
            }
            s.b[0] = u.b[2]; s.b[1] = u.b[3]; s.b[2] = u.b[0]; s.b[3] = u.b[1];
            s.b[4] = u.b[6]; s.b[5] = u.b[7]; s.b[6] = u.b[4]; s.b[7] = u.b[5];
            s.d /= 4.0;
            *d2++ = s.b[0]; *d2++ = s.b[1]; *d2++ = s.b[2]; *d2++ = s.b[3];
            *d2++ = s.b[4]; *d2++ = s.b[5]; *d2++ = s.b[6]; *d2++ = s.b[7];
         }
#elif	defined(CONVEX_N)
         if (d1 != d2) {
            nd *= 8;
            for (jd = 0; jd++ < nd; *d2++ = *d1++);
         }
#elif	defined(IEEE_L)
         for (jd = 0; jd < nd; jd++) {
            u.b[7] = *d1++; u.b[6] = *d1++; u.b[5] = *d1++; u.b[4] = *d1++;
            u.b[3] = *d1++; u.b[2] = *d1++; u.b[1] = *d1++; u.b[0] = *d1++;
            u.d /= 4.0;
            *d2++ = u.b[0]; *d2++ = u.b[1]; *d2++ = u.b[2]; *d2++ = u.b[3];
            *d2++ = u.b[4]; *d2++ = u.b[5]; *d2++ = u.b[6]; *d2++ = u.b[7];
         }
#elif	defined(IEEE_H)
         for (jd = 0; jd < nd; jd++) {
            u.b[0] = *d1++; u.b[1] = *d1++; u.b[2] = *d1++; u.b[3] = *d1++;
            u.b[4] = *d1++; u.b[5] = *d1++; u.b[6] = *d1++; u.b[7] = *d1++;
            u.d /= 4.0;
            *d2++ = u.b[0]; *d2++ = u.b[1]; *d2++ = u.b[2]; *d2++ = u.b[3];
            *d2++ = u.b[4]; *d2++ = u.b[5]; *d2++ = u.b[6]; *d2++ = u.b[7];
         }
#endif
         break;
      }
      default: {
         break;
      }
   }
}

#if	defined(TESTBED)
int main()
{
#if	defined(VMS_N)
   static char *OS = { "VMS_N    " };
#elif	defined(IEEE_H)
   static char *OS = { "IEEE_H   " };
#elif	defined(IEEE_L)
   static char *OS = { "IEEE_L   " };
#elif	defined(CONVEX_N)
   static char *OS = { "CONVEX_N " };
#endif

   static byte vms_b[18] = {
       65,  97,  66,  98,  67,  99,  68, 100,  69,
      101,  48,  50,  52,  54,  56,  49,  48,   0
   };
   static byte unx_b[18] = {
       65,  97,  66,  98,  67,  99,  68, 100,  69,
      101,  48,  50,  52,  54,  56,  49,  48,   0
   };
   static byte ibm_b[18] = {
      193, 129, 194, 130, 195, 131, 196, 132, 197,
      133, 240, 242, 244, 246, 248, 241, 240,   0
   };
   static byte dos_b[18] = {
       65,  97,  66,  98,  67,  99,  68, 100,  69,
      101,  48,  50,  52,  54,  56,  49,  48,   0
   };
   static byte cvx_b[18] = {
       65,  97,  66,  98,  67,  99,  68, 100,  69,
      101,  48,  50,  52,  54,  56,  49,  48,   0
   };
   byte b[18];
   fint sten = 17;
   union {
      short s;
      byte  b[2];
   } vms_s, unx_s, ibm_s, dos_s, cvx_s;
   union {
      fint i;
      byte b[4];
   } vms_i, unx_i, ibm_i, dos_i, cvx_i;
   union {
      float f;
      byte  b[4];
   } vms_f, unx_f, ibm_f, dos_f, cvx_f;
   union {
      double d;
      byte   b[8];
   } vms_d, unx_d, ibm_d, dos_d, cvx_d;
   short  s;
   fint   i;
   float  f;
   double d;
   fint   type;
   fint   one = 1;
   vms_s.b[0] = 100; vms_s.b[1] =  50;
   unx_s.b[0] =  50; unx_s.b[1] = 100;
   ibm_s.b[1] = 100; ibm_s.b[0] =  50;
   dos_s.b[1] =  50; dos_s.b[0] = 100;
   cvx_s.b[0] =  50; cvx_s.b[1] = 100;
   vms_i.b[0] =   2; vms_i.b[1] =   4; vms_i.b[2] =   8; vms_i.b[3] =  16;
   unx_i.b[0] =  16; unx_i.b[1] =   8; unx_i.b[2] =   4; unx_i.b[3] =   2;
   ibm_i.b[0] =  16; ibm_i.b[1] =   8; ibm_i.b[2] =   4; ibm_i.b[3] =   2;
   dos_i.b[0] =   2; dos_i.b[1] =   4; dos_i.b[2] =   8; dos_i.b[3] =  16;
   cvx_i.b[0] =  16; cvx_i.b[1] =   8; cvx_i.b[2] =   4; cvx_i.b[3] =   2;
   vms_f.b[0] =  34; vms_f.b[1] =  64; vms_f.b[2] = 117; vms_f.b[3] = 104;
   unx_f.b[0] =  63; unx_f.b[1] =  34; unx_f.b[2] = 104; unx_f.b[3] = 117;
   ibm_f.b[0] =  64; ibm_f.b[1] = 162; ibm_f.b[2] = 104; ibm_f.b[3] = 117;
   dos_f.b[3] =  63; dos_f.b[2] =  34; dos_f.b[1] = 104; dos_f.b[0] = 117;
   cvx_f.b[0] =  64; cvx_f.b[1] =  34; cvx_f.b[2] = 104; cvx_f.b[3] = 117;
   vms_d.b[0] =  34; vms_d.b[1] =  64; vms_d.b[2] = 117; vms_d.b[3] = 104;
   vms_d.b[4] = 127; vms_d.b[5] =  32; vms_d.b[6] =  54; vms_d.b[7] = 100;
   unx_d.b[0] =  63; unx_d.b[1] = 228; unx_d.b[2] =  77; unx_d.b[3] =  14;
   unx_d.b[4] = 164; unx_d.b[5] =  15; unx_d.b[6] = 236; unx_d.b[7] = 134;
   ibm_d.b[0] =  64; ibm_d.b[1] = 162; ibm_d.b[2] = 104; ibm_d.b[3] = 117;
   ibm_d.b[4] =  32; ibm_d.b[5] = 127; ibm_d.b[6] = 100; ibm_d.b[7] =  54;
   dos_d.b[7] =  63; dos_d.b[6] = 228; dos_d.b[5] =  77; dos_d.b[4] =  14;
   dos_d.b[3] = 164; dos_d.b[2] =  15; dos_d.b[1] = 236; dos_d.b[0] = 134;
   cvx_d.b[0] =  64; cvx_d.b[1] =   4; cvx_d.b[2] =  77; cvx_d.b[3] =  14;
   cvx_d.b[4] = 164; cvx_d.b[5] =  15; cvx_d.b[6] = 236; cvx_d.b[7] = 134;
   type = 1; cnvrtc_c( &type, vms_b, b, &sten ); b[17] = 0;
   printf("VMS_N    -> %s: AaBbCcDdEe0246810 -> %s\n", OS, b );
   type = 2; cnvrtc_c( &type, unx_b, b, &sten ); b[17] = 0;
   printf("IEEE_H   -> %s: AaBbCcDdEe0246810 -> %s\n", OS, b );
   type = 3; cnvrtc_c( &type, ibm_b, b, &sten ); b[17] = 0;
   printf("IBM      -> %s: AaBbCcDdEe0246810 -> %s\n", OS, b );
   type = 4; cnvrtc_c( &type, dos_b, b, &sten ); b[17] = 0;
   printf("IEEE_L   -> %s: AaBbCcDdEe0246810 -> %s\n", OS, b );
   type = 5; cnvrtc_c( &type, cvx_b, b, &sten ); b[17] = 0;
   printf("CONVEX_N -> %s: AaBbCcDdEe0246810 -> %s\n", OS, b );
   type = 1; cnvrth_c( &type, (byte *)&vms_s.s, (byte *)&s, &one );
   printf("VMS_N    -> %s: 00000000000012900 -> %.17d\n", OS, s );
   type = 2; cnvrth_c( &type, (byte *)&unx_s.s, (byte *)&s, &one );
   printf("IEEE_H   -> %s: 00000000000012900 -> %.17d\n", OS, s );
   type = 3; cnvrth_c( &type, (byte *)&ibm_s.s, (byte *)&s, &one );
   printf("IBM      -> %s: 00000000000012900 -> %.17d\n", OS, s );
   type = 4; cnvrth_c( &type, (byte *)&dos_s.s, (byte *)&s, &one );
   printf("IEEE_L   -> %s: 00000000000012900 -> %.17d\n", OS, s );
   type = 5; cnvrth_c( &type, (byte *)&cvx_s.s, (byte *)&s, &one );
   printf("CONVEX_N -> %s: 00000000000012900 -> %.17d\n", OS, s );
   type = 1; cnvrtf_c( &type, (byte *)&vms_i.i, (byte *)&i, &one );
   printf("VMS_N    -> %s: 00000000268960770 -> %.17ld\n", OS, i );
   type = 2; cnvrtf_c( &type, (byte *)&unx_i.i, (byte *)&i, &one );
   printf("IEEE_H   -> %s: 00000000268960770 -> %.17ld\n", OS, i );
   type = 3; cnvrtf_c( &type, (byte *)&ibm_i.i, (byte *)&i, &one );
   printf("IBM      -> %s: 00000000268960770 -> %.17ld\n", OS, i );
   type = 4; cnvrtf_c( &type, (byte *)&dos_i.i, (byte *)&i, &one );
   printf("IEEE_L   -> %s: 00000000268960770 -> %.17ld\n", OS, i );
   type = 5; cnvrtf_c( &type, (byte *)&cvx_i.i, (byte *)&i, &one );
   printf("CONVEX_N -> %s: 00000000268960770 -> %.17ld\n", OS, i );
   type = 1; cnvrte_c( &type, (byte *)&vms_f.f, (byte *)&f, &one );
   printf("VMS_N    -> %s: 0.634406387805939 -> %.15f\n", OS, f );
   type = 2; cnvrte_c( &type, (byte *)&unx_f.f, (byte *)&f, &one );
   printf("IEEE_H   -> %s: 0.634406387805939 -> %.15f\n", OS, f );
   type = 3; cnvrte_c( &type, (byte *)&ibm_f.f, (byte *)&f, &one );
   printf("IBM      -> %s: 0.634406387805939 -> %.15f\n", OS, f );
   type = 4; cnvrte_c( &type, (byte *)&dos_f.f, (byte *)&f, &one );
   printf("IEEE_L   -> %s: 0.634406387805939 -> %.15f\n", OS, f );
   type = 5; cnvrte_c( &type, (byte *)&cvx_f.f, (byte *)&f, &one );
   printf("CONVEX_N -> %s: 0.634406387805939 -> %.15f\n", OS, f );
   type = 1; cnvrtd_c( &type, (byte *)&vms_d.d, (byte *)&d, &one );
   printf("VMS_N    -> %s: 0.634406395372381 -> %.15f\n", OS, d );
   type = 2; cnvrtd_c( &type, (byte *)&unx_d.d, (byte *)&d, &one );
   printf("IEEE_H   -> %s: 0.634406395372381 -> %.15f\n", OS, d );
   type = 3; cnvrtd_c( &type, (byte *)&ibm_d.d, (byte *)&d, &one );
   printf("IBM      -> %s: 0.634406395372381 -> %.15f\n", OS, d );
   type = 4; cnvrtd_c( &type, (byte *)&dos_d.d, (byte *)&d, &one );
   printf("IEEE_L   -> %s: 0.634406395372381 -> %.15f\n", OS, d );
   type = 5; cnvrtd_c( &type, (byte *)&cvx_d.d, (byte *)&d, &one );
   printf("CONVEX_N -> %s: 0.634406395372381 -> %.15f\n", OS, d );
   return( 0 );
}
#endif
