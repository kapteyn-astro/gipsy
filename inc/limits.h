/* limits.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            limits.dc3

Header:       limits.h

Purpose:      Defines some common ANSI C constants for the sizes of
              integral types.

File:         limits.h

Author:       K.G. Begeman

Use:          #include "limits.h"

Defines:      CHAR_BIT      bits in a char
              CHAR_MAX      maximum value of char
              CHAR_MIN      minimum value of char
              INT_MAX       maximum value of int
              INT_MIN       minimum value of int
              LONG_MAX      maximum value of long
              LONG_MIN      minimum value of long
              SCHAR_MAX     maximum value of signed char
              SCHAR_MIN     minimum value of signed char
              SHRT_MAX      maximum value of short
              SHRT_MIN      minimum value of short
              UCHAR_MAX     maximum value of unsigned char
              UINT_MAX      maximum value of unsigned int
              ULONG_MAX     maximum value of unsigned long
              USHRT_MAX     maximum value of unsigned short

Warning:      System dependent! At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP 9000, IBM/RS6000,
              SILICON GRAPHICS, SUN and VMS.

Updates:      May  1, 1990: KGB, Document created.
              Jun  8, 2009: JPT, Linux now includes from system

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_LIMITS_H)
#if	defined(__aix__)
#elif	defined(__alliant__)
#elif	defined(__alpha__)
#elif	defined(__convex__)
#elif	defined(__cray__)
#elif	defined(__hpux__)
#elif	defined(__mips__)
#elif	defined(__sgi__)
#elif	defined(__sun__)
#elif	defined(__vms__)
#else
#include	<limits.h>			/* from system */
#if	!defined(_LIMITS_H)
#define	_LIMITS_H
#endif
#endif
#endif

#if	!defined(_LIMITS_H)
#define	_LIMITS_H

#if	defined(__aix__)			/* AIX */

#define	CHAR_BIT		8		/* bits in char */
#define	CHAR_MAX		127		/* maximum char */
#define	CHAR_MIN		(-128)		/* minimum char */
#define	INT_MAX			2147483647	/* maximum int */
#define	INT_MIN			(-INT_MAX-1)	/* minimum int */
#define	LONG_MAX		2147483647	/* maximum long */
#define	LONG_MIN		(-LONG_MAX-1)	/* minimum long */
#define	SCHAR_MAX		127		/* maximum signed char */
#define	SCHAR_MIN		(-128)		/* minimum signed char */
#define	SHRT_MAX		32767		/* maximum short */
#define	SHRT_MIN		(-32768)	/* minimum short */
#define	UCHAR_MAX		255U		/* maximum unsigned char */
#define	UINT_MAX		4294967295U	/* maximum unsigned int */
#define	ULONG_MAX		4294967295U	/* maximum unsigned long */
#define	USHRT_MAX		65535U		/* maximum unsigned short */

#elif	defined(__alliant__)			/* ALLIANT */

#define	CHAR_BIT		8		/* bits in char */
#define	CHAR_MAX		127		/* maximum char */
#define	CHAR_MIN		(-128)		/* minimum char */
#define	INT_MAX			2147483647	/* maximum int */
#define	INT_MIN			(-INT_MAX-1)	/* minimum int */
#define	LONG_MAX		2147483647	/* maximum long */
#define	LONG_MIN		(-LONG_MAX-1)	/* minimum long */
#define	SCHAR_MAX		127		/* maximum signed char */
#define	SCHAR_MIN		(-128)		/* minimum signed char */
#define	SHRT_MAX		32767		/* maximum short */
#define	SHRT_MIN		(-32768)	/* minimum short */
#define	UCHAR_MAX		255U		/* maximum unsigned char */
#define	UINT_MAX		4294967295U	/* maximum unsigned int */
#define	ULONG_MAX		4294967295U	/* maximum unsigned long */
#define	USHRT_MAX		65535U		/* maximum unsigned short */

#elif	defined(__alpha__)			/* DEC ALPHA */

#define	CHAR_BIT		8		/* bits in char */
#define	CHAR_MAX		127		/* maximum char */
#define	CHAR_MIN		(-128)		/* minimum char */
#define	INT_MAX			2147483647	/* maximum int */
#define	INT_MIN			(-INT_MAX-1)	/* minimum int */
#define	LONG_MAX	9223372036854775807	/* maximum long */
#define	LONG_MIN		(-LONG_MAX-1)	/* minimum long */
#define	SCHAR_MAX		127		/* maximum signed char */
#define	SCHAR_MIN		(-128)		/* minimum signed char */
#define	SHRT_MAX		32767		/* maximum short */
#define	SHRT_MIN		(-32768)	/* minimum short */
#define	UCHAR_MAX		255U		/* maximum unsigned char */
#define	UINT_MAX		4294967295U	/* maximum unsigned int */
#define	ULONG_MAX	18446744073709551615U	/* maximum unsigned long */
#define	USHRT_MAX		65535U		/* maximum unsigned short */

#elif	defined(__convex__)			/* CONVEX */

#define	CHAR_BIT		8		/* bits in char */
#define	CHAR_MAX		127		/* maximum char */
#define	CHAR_MIN		(-128)		/* minimum char */
#define	INT_MAX			2147483647	/* maximum int */
#define	INT_MIN			(-INT_MAX-1)	/* minimum int */
#define	LONG_MAX		2147483647	/* maximum long */
#define	LONG_MIN		(-LONG_MAX-1)	/* minimum long */
#define	SCHAR_MAX		127		/* maximum signed char */
#define	SCHAR_MIN		(-128)		/* minimum signed char */
#define	SHRT_MAX		32767		/* maximum short */
#define	SHRT_MIN		(-32768)	/* minimum short */
#define	UCHAR_MAX		255U		/* maximum unsigned char */
#define	UINT_MAX		4294967295U	/* maximum unsigned int */
#define	ULONG_MAX		4294967295U	/* maximum unsigned long */
#define	USHRT_MAX		65535U		/* maximum unsigned short */

#elif	defined(__cray__)			/* CRAY */

#include	"/usr/include/limits.h"		/* use standard */

#elif	defined(__hpux__)			/* HP 9000 */

#define	CHAR_BIT		8		/* bits in char */
#define	CHAR_MAX		127		/* maximum char */
#define	CHAR_MIN		(-128)		/* minimum char */
#define	INT_MAX			2147483647	/* maximum int */
#define	INT_MIN			(-INT_MAX-1)	/* minimum int */
#define	LONG_MAX		2147483647	/* maximum long */
#define	LONG_MIN		(-LONG_MAX-1)	/* minimum long */
#define	SCHAR_MAX		127		/* maximum signed char */
#define	SCHAR_MIN		(-128)		/* minimum signed char */
#define	SHRT_MAX		32767		/* maximum short */
#define	SHRT_MIN		(-32768)	/* minimum short */
#define	UCHAR_MAX		255U		/* maximum unsigned char */
#define	UINT_MAX		4294967295U	/* maximum unsigned int */
#define	ULONG_MAX		4294967295U	/* maximum unsigned long */
#define	USHRT_MAX		65535U		/* maximum unsigned short */

#elif	defined(__linux__)			/* LINUX */

#define	CHAR_BIT		8		/* bits in char */
#define	CHAR_MAX		127		/* maximum char */
#define	CHAR_MIN		-128		/* minimum char */
#define	INT_MAX			2147483647	/* maximum int */
#define	INT_MIN			(-INT_MAX-1)	/* minimum int */
#define	LONG_MAX		2147483647	/* maximum long */
#define	LONG_MIN		(-LONG_MAX-1)	/* minimum long */
#define	SCHAR_MAX		127		/* maximum signed char */
#define	SCHAR_MIN		(-128)		/* minimum signed char */
#define	SHRT_MAX		32767		/* maximum short */
#define	SHRT_MIN		(-32768)	/* minimum short */
#define	UCHAR_MAX		255U		/* maximum unsigned char */
#define	UINT_MAX		4294967295U	/* maximum unsigned int */
#define	ULONG_MAX		4294967295U	/* maximum unsigned long */
#define	USHRT_MAX		65535U		/* maximum unsigned short */

#elif	defined(__mips__)			/* DEC ULTRIX */

#define	CHAR_BIT		8		/* bits in char */
#define	CHAR_MAX		127		/* maximum char */
#define	CHAR_MIN		-128		/* minimum char */
#define	INT_MAX			2147483647	/* maximum int */
#define	INT_MIN			(-INT_MAX-1)	/* minimum int */
#define	LONG_MAX		2147483647	/* maximum long */
#define	LONG_MIN		(-LONG_MAX-1)	/* minimum long */
#define	SCHAR_MAX		127		/* maximum signed char */
#define	SCHAR_MIN		(-128)		/* minimum signed char */
#define	SHRT_MAX		32767		/* maximum short */
#define	SHRT_MIN		(-32768)	/* minimum short */
#define	UCHAR_MAX		255U		/* maximum unsigned char */
#define	UINT_MAX		4294967295U	/* maximum unsigned int */
#define	ULONG_MAX		4294967295U	/* maximum unsigned long */
#define	USHRT_MAX		65535U		/* maximum unsigned short */

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

#define	CHAR_BIT		8		/* bits in char */
#define	CHAR_MAX		UCHAR_MAX	/* maximum char */
#define	CHAR_MIN		0		/* minimum char */
#define	INT_MAX			2147483647	/* maximum int */
#define	INT_MIN			(-INT_MAX-1)	/* minimum int */
#define	LONG_MAX		2147483647	/* maximum long */
#define	LONG_MIN		(-LONG_MAX-1)	/* minimum long */
#define	SCHAR_MAX		127		/* maximum signed char */
#define	SCHAR_MIN		(-128)		/* minimum signed char */
#define	SHRT_MAX		32767		/* maximum short */
#define	SHRT_MIN		(-32768)	/* minimum short */
#define	UCHAR_MAX		255U		/* maximum unsigned char */
#define	UINT_MAX		4294967295U	/* maximum unsigned int */
#define	ULONG_MAX		4294967295U	/* maximum unsigned long */
#define	USHRT_MAX		65535U		/* maximum unsigned short */

#elif	defined(__sun__)			/* SUN */

#define	CHAR_BIT		8		/* bits in char */
#define	CHAR_MAX		127		/* maximum char */
#define	CHAR_MIN		(-128)		/* minimum char */
#define	INT_MAX			2147483647	/* maximum int */
#define	INT_MIN			(-INT_MAX-1)	/* minimum int */
#define	LONG_MAX		2147483647	/* maximum long */
#define	LONG_MIN		(-LONG_MAX-1)	/* minimum long */
#define	SCHAR_MAX		127		/* maximum signed char */
#define	SCHAR_MIN		(-128)		/* minimum signed char */
#define	SHRT_MAX		32767		/* maximum short */
#define	SHRT_MIN		(-32768)	/* minimum short */
#define	UCHAR_MAX		255U		/* maximum unsigned char */
#define	UINT_MAX		4294967295U	/* maximum unsigned int */
#define	ULONG_MAX		4294967295U	/* maximum unsigned long */
#define	USHRT_MAX		65535U		/* maximum unsigned short */

#elif	defined(__vms__)			/* DEC VMS */

#define	CHAR_BIT		8		/* bits in char */
#define	CHAR_MAX		127		/* maximum char */
#define	CHAR_MIN		(-128)		/* minimum char */
#define	INT_MAX			2147483647	/* maximum int */
#define	INT_MIN			(-INT_MAX-1)	/* minimum int */
#define	LONG_MAX		2147483647	/* maximum long */
#define	LONG_MIN		(-LONG_MAX-1)	/* minimum long */
#define	SCHAR_MAX		127		/* maximum signed char */
#define	SCHAR_MIN		(-128)		/* minimum signed char */
#define	SHRT_MAX		32767		/* maximum short */
#define	SHRT_MIN		(-32768)	/* minimum short */
#define	UCHAR_MAX		255U		/* maximum unsigned char */
#define	UINT_MAX		4294967295U	/* maximum unsigned int */
#define	ULONG_MAX		4294967295U	/* maximum unsigned long */
#define	USHRT_MAX		65535U		/* maximum unsigned short */

#endif

#endif
