/* float.h

	Copyright (c) Kapteyn Laboratorium Groningen, 1990.
	All Rights Reserved.

#>            float.dc3

Header:       float.h

Purpose:      Defines constants related to floating-point arithmetic.

File:         float.h

Author:       K.G. Begeman

Use:          #include "float.h"

Declares:     FLT_RADIX
              FLT_ROUNDS
              FLT_DIG
              FLT_EPSILON
              FLT_MANT_DIG
              FLT_MAX
              FLT_MAX_EXP
              FLT_MIN
              FLT_MIN_EXP
              DBL_DIG
              DBL_EPSILON
              DBL_MANT_DIG
              DBL_MAX
              DBL_MAX_EXP
              DBL_MIN
              DBL_MIN_EXP

Warning:      System dependent! Currently implemented on ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP 9000, IBM/RS6000,
              SILICON GRAPHICS, SUN and VMS.

Updates:      May  3, 1990: KGB, Document created.
              Jun  8, 2009: JPT, Linux now includes from system

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_FLOAT_H)
#if	defined(__aix__)
#elif	defined(__alliant__)
#elif	defined(__alpha__)
#elif	defined(__convex__)
#elif	defined(__cray__)
#elif	defined(__freebsd__)
#elif	defined(__hpux__)
#elif	defined(__mips__)
#elif	defined(__sgi__)
#elif	defined(__sun__)
#elif	defined(__vms__)
#else
#include	<float.h>			/* from system */
#if	!defined(_FLOAT_H)
#define	_FLOAT_H
#endif
#endif
#endif

#if	!defined(_FLOAT_H)
#define	_FLOAT_H

#if	defined(__aix__)			/* AIX */

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX       2
#define FLT_ROUNDS      ( fp_read_rnd() )
#define FLT_DIG		6
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG    24
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	+128			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	-125			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		15			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	53			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	+1024			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	-1021			/* minimum n such that 10^n is a normalized number */

#elif	defined(__alliant__)			/* ALLIANT */

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX	2			/* radix of exponent representation */
#define FLT_ROUNDS	1			/* floating-point rounding mode for addition */
#define FLT_DIG		6			/* decimal digitsof precision */
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG	24			/* number of base FLT_RADIX digits in mantissa */
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	+128			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	-125			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		15			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	53			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	+1024			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	-1021			/* minimum n such that 10^n is a normalized number */

#elif	defined(__alpha__)			/* DEC ALPHA */

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX	2			/* radix of exponent representation */
#define FLT_ROUNDS	1			/* floating-point rounding mode for addition */
#define FLT_DIG		6			/* decimal digitsof precision */
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG	24			/* number of base FLT_RADIX digits in mantissa */
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	+128			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	-125			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		15			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	53			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	+1024			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	-1021			/* minimum n such that 10^n is a normalized number */

#elif	defined(__convex__)			/* CONVEX */

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX	2			/* radix of exponent representation */
#define FLT_ROUNDS	1			/* floating-point rounding mode for addition */
#define FLT_DIG		6			/* decimal digitsof precision */
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG	24			/* number of base FLT_RADIX digits in mantissa */
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	+128			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	-125			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		15			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	53			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	+1024			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	-1021			/* minimum n such that 10^n is a normalized number */

#elif	defined(__cray__)			/* CRAY */

#include	"/usr/include/float.h"		/* use strandard */

#elif	defined(__freebsd__)			/* FREEBSD*/

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX	2			/* radix of exponent representation */
#define FLT_ROUNDS	1			/* floating-point rounding mode for addition */
#define FLT_DIG		6			/* decimal digitsof precision */
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG	24			/* number of base FLT_RADIX digits in mantissa */
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	128			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	(-125)			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		15			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	53			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	1024			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	(-1021)			/* minimum n such that 10^n is a normalized number */

#elif	defined(__hpux__)			/* HP 9000 */

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX	2			/* radix of exponent representation */
#define FLT_ROUNDS	1			/* floating-point rounding mode for addition */
#define FLT_DIG		6			/* decimal digitsof precision */
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG	24			/* number of base FLT_RADIX digits in mantissa */
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	(+128)			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	(-125)			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		15			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	53			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	(+1024)			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	(-1021)			/* minimum n such that 10^n is a normalized number */

#elif	defined(__linux__)			/* LINUX */

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX	2			/* radix of exponent representation */
#define FLT_ROUNDS	1			/* floating-point rounding mode for addition */
#define FLT_DIG		6			/* decimal digitsof precision */
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG	24			/* number of base FLT_RADIX digits in mantissa */
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	+128			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	-125			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		15			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	53			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	+1024			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	-1021			/* minimum n such that 10^n is a normalized number */

#elif	defined(__mips__)			/* DEC ULTRIX */

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX	2			/* radix of exponent representation */
#define FLT_ROUNDS	1			/* floating-point rounding mode for addition */
#define FLT_DIG		6			/* decimal digitsof precision */
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG	24			/* number of base FLT_RADIX digits in mantissa */
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	+128			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	-125			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		15			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	53			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	+1024			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	-1021			/* minimum n such that 10^n is a normalized number */

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX       2
#define FLT_ROUNDS      1
#define FLT_DIG		6
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG    24
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	+128			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	-125			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		15			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	53			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	+1024			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	-1021			/* minimum n such that 10^n is a normalized number */

#elif	defined(__sun__)			/* SUN */

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX	2			/* radix of exponent representation */
#define FLT_ROUNDS	1			/* floating-point rounding mode for addition */
#define FLT_DIG		6			/* decimal digitsof precision */
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG	24			/* number of base FLT_RADIX digits in mantissa */
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	+128			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	-125			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		15			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	53			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	+1024			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	-1021			/* minimum n such that 10^n is a normalized number */

#elif	defined(__vms__)			/* DEC VMS */

extern	float		_flt_epsilon(void);	/* FLT_EPSILON */
extern	float		_flt_max(void);		/* FLT_MAX */
extern	float		_flt_min(void);		/* FLT_MIN */
extern	double		_dbl_epsilon(void);	/* DBL_EPSILON */
extern	double		_dbl_max(void);		/* DBL_MAX */
extern	double		_dbl_min(void);		/* DBL_MIN */

#define FLT_RADIX	2			/* radix of exponent representation */
#define FLT_ROUNDS	1			/* floating-point rounding mode for addition */
#define FLT_DIG		7			/* decimal digitsof precision */
#define	FLT_EPSILON	_flt_epsilon()		/* smallest number x such that 1.0+x != 1.0 */
#define FLT_MANT_DIG	23			/* number of base FLT_RADIX digits in mantissa */
#define FLT_MAX		_flt_max()		/* maximum floating-point number */
#define FLT_MAX_EXP	+127			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	FLT_MIN		_flt_min()		/* minimum normalized floating-point number */
#define FLT_MIN_EXP	-127			/* minimum n such that 10^n is a normalized number */
#define DBL_DIG		16			/* decimal digits of precision */
#define	DBL_EPSILON	_dbl_epsilon()		/* smallest number x such that 1.0+x!= 1.0 */
#define DBL_MANT_DIG	55			/* number of base FLT_RADIX digits in mantissa */
#define	DBL_MAX		_dbl_max()		/* maximum double floating-point number */
#define DBL_MAX_EXP	+127			/* maximum n such that FLT_RADIX^n-1 is representable */
#define	DBL_MIN		_dbl_min()		/* minimum normalized double floating-point number */
#define DBL_MIN_EXP	-127			/* minimum n such that 10^n is a normalized number */

#endif

#endif
