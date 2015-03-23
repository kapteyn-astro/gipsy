/* int32.c

	Copyright (c) Kapteyn Laboratorium Groningen 1994
	All Rights Reserved.

#>            int32.dc3

Document:     int32

Purpose:      Describes the available routines to convert local integers
              to/from 32 bit integers.

Category:     SYSTEM

File:         int32

Author:       K.G. Begeman

Description:  Programs which need to communicate with other programs
              at other hosts need a 'standard' integer, which is in our
              case a 32 bit twos-complement integer (can be either high or
              low endian). The following routines are available to do
              the conversion:

              void int_to_int32( int *, unsigned char *, int )
              void int32_to_int( unsigned char *, int *, int )
              void int_to_int32_n( int *, unsigned char *, int, int )
              void int32_to_int_n( unsigned char *, int *, int, int )

Updates:      Dec  7, 1994: KGB, Document created.

#<
#>            int_to_int32.dc3

Function:     int_to_int32

Purpose:      Converts a local integer to a 32 bit twos-complement integer.

Category:     SYSTEM

File:         int32.c

Author:       K.G. Begeman

Call:         void int_to_int32( int *i, unsigned char *i32, int swap )

              i                 pointer to local integer
              i32               pointer to 32 bit integer
              swap              need to swap bytes?

Notes:        Definition in "int32.h"

Updates:      Dec  7, 1994: KGB, Document created.

#<
#>            int32_to_int.dc3

Function:     int32_to_int

Purpose:      Converts a 32 bit twos-complement integer to a local integer.

Category:     SYSTEM

File:         int32.c

Author:       K.G. Begeman

Call:         void int32_to_int( unsigned char *i32, int *i, int swap )

              i32               pointer to 32 bit integer
              i                 pointer to local integer
              swap              need to swap bytes?

Notes:        Definition in "int32.h"

Updates:      Dec  7, 1994: KGB, Document created.

#<
#>            int_to_int32_n.dc3

Function:     int_to_int32_n

Purpose:      Converts a number of local integers to 32 bit twos-complement
              integers.

Category:     SYSTEM

File:         int32.c

Author:       K.G. Begeman

Call:         void int_to_int32_n( int *i, unsigned char *i32, int n, int swap )

              i                 pointer to local integer
              i32               pointer to 32 bit integer
              n                 number of integers to convert
              swap              need to swap bytes?

Notes:        Definition in "int32.h"

Updates:      Dec 12, 1994: KGB, Document created.

#<
#>            int32_to_int_n.dc3

Function:     int32_to_int_n

Purpose:      Converts a number of 32 bit twos-complement integers to local
              integers.

Category:     SYSTEM

File:         int32.c

Author:       K.G. Begeman

Call:         void int32_to_int_n( unsigned char *i32, int *i, int n, int swap )

              i32               pointer to 32 bit integer
              i                 pointer to local integer
              n                 number of integers to convert
              swap              need to swap bytes?

Notes:        Definition in "int32.h"

Updates:      Dec 12, 1994: KGB, Document created.

#<
#>            int32.h
#ifndef	_INT32_H
#define	_INT32_H
extern	void	int_to_int32( int *i, unsigned char *i32, int swap );
extern	void	int32_to_int( unsigned char *i32, int *i, int swap );
extern	void	int_to_int32_n( int *i, unsigned char *i32, int n, int swap );
extern	void	int32_to_int_n( unsigned char *i32, int *i, int n, int swap );
#endif
#<
*/

#include	"osdef.h"			/* system specific */
#include	"stdio.h"			/* <stdio.h> */
#include	"string.h"			/* <string.h> */
#include	"int32.h"			/* our definitions */

#ifdef	byte
#undef	byte
#endif
#define	byte	unsigned char

void	int_to_int32_n( int *i, byte *i32, int n, int swap )
{
   int	k;
   union {
      byte	b[sizeof(int)];
      int	i;
   }	u;

#if	OS_INTEGER_TYPE == 0 || OS_INTEGER_TYPE == 1
   if ( swap ) {
      for ( k = 0; k < n ; k++ ) {
         u.i = *i++;
         *i32++ = u.b[3]; *i32++ = u.b[2]; *i32++ = u.b[1]; *i32++ = u.b[0];
      }
   } else {
      memcpy( i32, i, 4 * n );
   }
#elif	OS_INTEGER_TYPE == 2 || OS_INTEGER_TYPE == 3
   if ( swap ) {
      for ( k = 0; k < n; k++) {
         u.i = *i++;
#if	OS_INTEGER_TYPE == 2
         *i32++ = u.b[7]; *i32++ = u.b[6]; *i32++ = u.b[5]; *i32++ = u.b[4];
#else
         *i32++ = u.b[3]; *i32++ = u.b[2]; *i32++ = u.b[1]; *i32++ = u.b[0];
#endif
      }
   } else {
      for ( k = 0; k < n; k++ ) {
         u.i = *i++;
#if	OS_INTEGER_TYPE == 2
         *i32++ = u.b[4]; *i32++ = u.b[5]; *i32++ = u.b[6]; *i32++ = u.b[7];
#else
         *i32++ = u.b[0]; *i32++ = u.b[1]; *i32++ = u.b[2]; *i32++ = u.b[3];
#endif
      }
   }
#endif
}

void	int_to_int32( int *i, byte *i32, int swap )
{
   int_to_int32_n( i, i32, 1, swap );
}

void	int32_to_int_n( byte *i32, int *i, int n, int swap )
{
   int	k;
   union {
   	byte	b[sizeof(int)];
   	int	i;
   }	u;

#if	OS_INTEGER_TYPE == 0 || OS_INTEGER_TYPE == 1
   if ( swap ) {
      for ( k = 0; k < n; k++ ) {
         u.b[3] = *i32++; u.b[2] = *i32++; u.b[1] = *i32++; u.b[0] = *i32++;
         *i++ = u.i;
      }
   } else {
      memcpy( i, i32, 4 * n );
   }
#elif	OS_INTEGER_TYPE == 2 || OS_INTEGER_TYPE == 3
   for (  k = 0; k < n; k++ ) {
      if ( swap ) {
#if	OS_INTEGER_TYPE == 2
         u.b[7] = *i32++; u.b[6] = *i32++; u.b[5] = *i32++; u.b[4] = *i32++;
#else
         u.b[3] = *i32++; u.b[2] = *i32++; u.b[1] = *i32++; u.b[0] = *i32++;
#endif
      } else {
#if	OS_INTEGER_TYPE == 2
         u.b[4] = *i32++; u.b[5] = *i32++; u.b[6] = *i32++; u.b[7] = *i32++;
#else
         u.b[0] = *i32++; u.b[1] = *i32++; u.b[2] = *i32++; u.b[3] = *i32++;
#endif
      }
      if ( u.i & 0x80000000 ) {
         u.i |= ( 0xffffffff << 32 );
      } else {
         u.i &= ( 0xffffffff );
      }
      *i++ = u.i;
   }
#endif
}

void	int32_to_int( byte *i32, int *i, int swap )
{
   int32_to_int_n( i32, i, 1, swap );
}

#ifdef	TESTBED

int	main( )
{
   byte	b[8];
   int	i[2], n;

   printf( "OS_INTEGER_TYPE = %d, sizeof(int) = %d\n", OS_INTEGER_TYPE, sizeof(int) );
   for ( n = -2; n < 3; n++ ) {
      int_to_int32( &n, b, 0 );
      printf( "%2.2x%2.2x%2.2x%2.2x <- %12d\n", b[0], b[1], b[2], b[3], n );
      int32_to_int( b, i, 0 );
      printf( "%2.2x%2.2x%2.2x%2.2x -> %12d\n", b[0], b[1], b[2], b[3], *i );
      int_to_int32( &n, b, 1 );
      printf( "%2.2x%2.2x%2.2x%2.2x <- %12d\n", b[0], b[1], b[2], b[3], n );
      int32_to_int( b, i, 1 );
      printf( "%2.2x%2.2x%2.2x%2.2x -> %12d\n", b[0], b[1], b[2], b[3], *i );
   }
   n = -2147483648;
   int_to_int32( &n, b, 0 );
   printf( "%2.2x%2.2x%2.2x%2.2x <- %12d\n", b[0], b[1], b[2], b[3], n );
   int32_to_int( b, i, 0 );
   printf( "%2.2x%2.2x%2.2x%2.2x -> %12d\n", b[0], b[1], b[2], b[3], *i );
   int_to_int32( &n, b, 1 );
   printf( "%2.2x%2.2x%2.2x%2.2x <- %12d\n", b[0], b[1], b[2], b[3], n );
   int32_to_int( b, i, 1 );
   printf( "%2.2x%2.2x%2.2x%2.2x -> %12d\n", b[0], b[1], b[2], b[3], *i );
   n = 2147483647;
   int_to_int32( &n, b, 0 );
   printf( "%2.2x%2.2x%2.2x%2.2x <- %12d\n", b[0], b[1], b[2], b[3], n );
   int32_to_int( b, i, 0 );
   printf( "%2.2x%2.2x%2.2x%2.2x -> %12d\n", b[0], b[1], b[2], b[3], *i );
   int_to_int32( &n, b, 1 );
   printf( "%2.2x%2.2x%2.2x%2.2x <- %12d\n", b[0], b[1], b[2], b[3], n );
   int32_to_int( b, i, 1 );
   printf( "%2.2x%2.2x%2.2x%2.2x -> %12d\n", b[0], b[1], b[2], b[3], *i );
   i[0] = -2147483648;
   i[1] =  2147483647;
   int_to_int32_n( i, b, 2, 0 );
   printf( "%2.2x%2.2x%2.2x%2.2x <- %12d  ", b[0], b[1], b[2], b[3], i[0] );
   printf( "%2.2x%2.2x%2.2x%2.2x <- %12d\n", b[4], b[5], b[6], b[7], i[1] );
   int32_to_int_n( b, i, 2, 0 );   
   printf( "%2.2x%2.2x%2.2x%2.2x -> %12d  ", b[0], b[1], b[2], b[3], i[0] );
   printf( "%2.2x%2.2x%2.2x%2.2x -> %12d\n", b[4], b[5], b[6], b[7], i[1] );
   int_to_int32_n( i, b, 2, 1 );
   printf( "%2.2x%2.2x%2.2x%2.2x <- %12d  ", b[0], b[1], b[2], b[3], i[0] );
   printf( "%2.2x%2.2x%2.2x%2.2x <- %12d\n", b[4], b[5], b[6], b[7], i[1] );
   int32_to_int_n( b, i, 2, 1 );   
   printf( "%2.2x%2.2x%2.2x%2.2x -> %12d  ", b[0], b[1], b[2], b[3], i[0] );
   printf( "%2.2x%2.2x%2.2x%2.2x -> %12d\n", b[4], b[5], b[6], b[7], i[1] );
   return( 0 );
}
#endif
