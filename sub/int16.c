/* int16.c

	Copyright (c) Kapteyn Laboratorium Groningen 1994
	All Rights Reserved.

#>            int16.dc3

Document:     int16

Purpose:      Describes the available routines to convert local short integers
              to/from 16 bit integers.

Category:     SYSTEM

File:         int16

Author:       K.G. Begeman

Description:  The following routines are available to do the conversion:

              void int_to_int16( short *, unsigned char *, int )
              void int16_to_int( unsigned char *, short *, int )
              void int_to_int16_n( short *, unsigned char *, int, int )
              void int16_to_int_n( unsigned char *, short *, int, int )

Updates:      Dec 12, 1994: KGB, Document created.

#<
#>            int_to_int16.dc3

Function:     int_to_int16

Purpose:      Converts a local short integer to a 16 bit twos-complement
              integer.

Category:     SYSTEM

File:         int16.c

Author:       K.G. Begeman

Call:         void int_to_int16( short *s, unsigned char *i16, int swap )

              s                 pointer to local short integer
              i16               pointer to 16 bit integer
              swap              need to swap bytes?

Notes:        Definition in "int16.h"

Updates:      Dec 12, 1994: KGB, Document created.

#<
#>            int16_to_int.dc3

Function:     int16_to_int

Purpose:      Converts a 16 bit twos-complement integer to a local short
              integer.

Category:     SYSTEM

File:         int16.c

Author:       K.G. Begeman

Call:         void int16_to_int( unsigned char *i16, short *s, int swap )

              i16               pointer to 16 bit integer
              s                 pointer to local short integer
              swap              need to swap bytes?

Notes:        Definition in "int16.h"

Updates:      Dec 12, 1994: KGB, Document created.

#<
#>            int_to_int16_n.dc3

Function:     int_to_int16_n

Purpose:      Converts a number of local short integers to 16 bit
              twos-complement integers.

Category:     SYSTEM

File:         int16.c

Author:       K.G. Begeman

Call:         void int_to_int16_n( short *s, unsigned char *i16, int n,
                                   int swap )

              s                 pointer to local short integer
              i16               pointer to 16 bit integer
              n                 number of integers to convert
              swap              need to swap bytes?

Notes:        Definition in "int16.h"

Updates:      Dec 12, 1994: KGB, Document created.

#<
#>            int16_to_int_n.dc3

Function:     int16_to_int_n

Purpose:      Converts a number of 16 bit twos-complement integers to local
              short integers.

Category:     SYSTEM

File:         int16.c

Author:       K.G. Begeman

Call:         void int16_to_int_n( unsigned char *i16, short *s, int n,
                                   int swap )

              i16               pointer to 16 bit integer
              s                 pointer to local short integer
              n                 number of integers to convert
              swap              need to swap bytes?

Notes:        Definition in "int16.h"

Updates:      Dec 12, 1994: KGB, Document created.

#<
#>            int16.h
#ifndef	_INT16_H
#define	_INT16_H
extern	void	int_to_int16( short *s, unsigned char *i16, int swap );
extern	void	int16_to_int( unsigned char *i16, short *s, int swap );
extern	void	int_to_int16_n( short *s, unsigned char *i16, int n, int swap );
extern	void	int16_to_int_n( unsigned char *i16, short *s, int n, int swap );
#endif
#<
*/

#include	"osdef.h"			/* system specific */
#include	"stdio.h"			/* <stdio.h> */
#include	"string.h"			/* <string.h> */
#include	"int16.h"			/* our definitions */

#ifdef	byte
#undef	byte
#endif
#define	byte	unsigned char

void	int_to_int16_n( short *s, byte *i16, int n, int swap )
{
   int	k;
   union {
      byte	b[sizeof(short)];
      short	s;
   }	u;

   if ( sizeof(short) == 2 ) {
      if ( swap ) {
         for ( k = 0; k < n ; k++ ) {
            u.s = *s++;
            *i16++ = u.b[1]; *i16++ = u.b[0];
         }
      } else {
         memcpy( i16, s, 2 * n );
      }
   } else {
      if ( swap ) {
         for ( k = 0; k < n; k++) {
            u.s = *s++;
#if	OS_INTEGER_TYPE == 0 || OS_INTEGER_TYPE == 2
            *i16++ = u.b[sizeof(short)-1];
            *i16++ = u.b[sizeof(short)-2];
#else
            *i16++ = u.b[1];
            *i16++ = u.b[0];
#endif
         }
      } else {
         for ( k = 0; k < n; k++ ) {
            u.s = *s++;
#if	OS_INTEGER_TYPE == 0 || OS_INTEGER_TYPE == 2
            *i16++ = u.b[sizeof(short)-2];
            *i16++ = u.b[sizeof(short)-1];
#else
            *i16++ = u.b[0];
            *i16++ = u.b[1];
#endif
         }
      }
   }
}

void	int_to_int16( short *s, byte *i16, int swap )
{
   int_to_int16_n( s, i16, 1, swap );
}

void	int16_to_int_n( byte *i16, short *s, int n, int swap )
{
   int	k;
   union {
      byte	b[sizeof(short)];
      short	s;
   }	u;

   if ( sizeof(short) == 2 ) {
      if ( swap ) {
         for ( k = 0; k < n; k++ ) {
            u.b[1] = *i16++; u.b[0] = *i16++;
         }
         *s++ = u.s;
      } else {
         memcpy( s, i16, 2 * n );
      }
   } else {
      for (  k = 0; k < n; k++ ) {
         if ( swap ) {
#if	OS_INTEGER_TYPE == 0 || OS_INTEGER_TYPE == 2
            u.b[sizeof(short)-1] = *i16++;
            u.b[sizeof(short)-2] = *i16++;
#else
            u.b[1] = *i16++;
            u.b[0] = *i16++;
#endif
         } else {
#if	OS_INTEGER_TYPE == 0 || OS_INTEGER_TYPE == 2
            u.b[sizeof(short)-2] = *i16++;
            u.b[sizeof(short)-1] = *i16++;
#else
            u.b[0] = *i16++;
            u.b[1] = *i16++;
#endif
         }
         if ( u.s & 0x8000 ) {
            int	l;
            for  ( l = 2; l < sizeof(short); l++ ) {
#if	OS_INTEGER_TYPE == 0 || OS_INTEGER_TYPE == 2
               u.b[l-2] = 0xff;
#else
               u.b[l] = 0xff;
#endif
            }
         } else {
            u.s &= 0xffff;
         }
         *s++ = u.s;
      }
   }
}

void	int16_to_int( byte *i16, short *s, int swap )
{
   int16_to_int_n( i16, s, 1, swap );
}

#ifdef	TESTBED

int	main( )
{
   byte		b[8];
   short	s[2], n;

   printf( "OS_INTEGER_TYPE = %d, size of short = %d\n", OS_INTEGER_TYPE, sizeof(short) ); 
   for ( n = -2; n < 3; n++ ) {
      int_to_int16( &n, b, 0 );
      printf( "%2.2x%2.2x <- %6d\n", b[0], b[1], (int) n );
      int16_to_int( b, s, 0 );
      printf( "%2.2x%2.2x -> %6d\n", b[0], b[1], (int)*s );
      int_to_int16( &n, b, 1 );
      printf( "%2.2x%2.2x <- %6d\n", b[0], b[1], (int)n );
      int16_to_int( b, s, 1 );
      printf( "%2.2x%2.2x -> %6d\n", b[0], b[1], (int)*s );
   }
   n = -32768;
   int_to_int16( &n, b, 0 );
   printf( "%2.2x%2.2x <- %6d\n", b[0], b[1], (int)n );
   int16_to_int( b, s, 0 );
   printf( "%2.2x%2.2x -> %6d\n", b[0], b[1], (int)*s );
   int_to_int16( &n, b, 1 );
   printf( "%2.2x%2.2x <- %6d\n", b[0], b[1], (int)n );
   int16_to_int( b, s, 1 );
   printf( "%2.2x%2.2x -> %6d\n", b[0], b[1], (int)*s );
   n = 32767;
   int_to_int16( &n, b, 0 );
   printf( "%2.2x%2.2x <- %6d\n", b[0], b[1], (int)n );
   int16_to_int( b, s, 0 );
   printf( "%2.2x%2.2x -> %6d\n", b[0], b[1], (int)*s );
   int_to_int16( &n, b, 1 );
   printf( "%2.2x%2.2x <- %6d\n", b[0], b[1], (int)n );
   int16_to_int( b, s, 1 );
   printf( "%2.2x%2.2x -> %6d\n", b[0], b[1], (int)*s );
   s[0] = -32768;
   s[1] =  32767;
   int_to_int16_n( s, b, 2, 0 );
   printf( "%2.2x%2.2x <- %6d  ", b[0], b[1], (int)s[0] );
   printf( "%2.2x%2.2x <- %6d\n", b[2], b[3], (int)s[1] );
   int16_to_int_n( b, s, 2, 0 );   
   printf( "%2.2x%2.2x -> %6d  ", b[0], b[1], (int)s[0] );
   printf( "%2.2x%2.2x -> %6d\n", b[2], b[3], (int)s[1] );
   int_to_int16_n( s, b, 2, 1 );
   printf( "%2.2x%2.2x <- %6d  ", b[0], b[1], (int)s[0] );
   printf( "%2.2x%2.2x <- %6d\n", b[2], b[3], (int)s[1] );
   int16_to_int_n( b, s, 2, 1 );   
   printf( "%2.2x%2.2x -> %6d  ", b[0], b[1], (int)s[0] );
   printf( "%2.2x%2.2x -> %6d\n", b[2], b[3], (int)s[1] );
   return( 0 );
}
#endif
