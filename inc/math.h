/* math.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            math.dc3

Header:       math

Purpose:      Declares the mathematical functions and macros.

File:         math.h

Author:       K.G. Begeman

Use:          #include "math.h"

Declares:     double acos(double x);
              double asin(double x);
              double atan(double x);
              double atan2(double x, double y);
              double ceil(double x);
              double cos(double x);
              double cosh(double x);
              double exp(double x);
              double fabs(double x);
              double floor(double x);
              double fmod(double x, double y);
              double frexp(double x, int *exp);
              double ldexp(double x, int n);
              double log(double x);
              double log10(double x);
              double modf(double x, double *ip);
              double pow(double x, double y);
              double sin(double x);
              double sinh(double x);
              double sqrt(double x);
              double tan(double x);
              double tanh(double x);

Warning:      System dependent. At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP 9000, IBM/RS6000,
              SILICON GRAPHICS, SUN and VMS.

Updates:      Apr 11, 1990: KGB, Document created.
              Jun  8, 2009: JPT, Linux now includes from system

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_MATH_H)
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
#include	<math.h>			/* from system */
#if	!defined(_MATH_H)
#define	_MATH_H
#endif
#endif
#endif

#if	!defined(_MATH_H)
#define	_MATH_H

#if	defined(__aix__)			/* AIX */

#elif	defined(__alliant__)			/* ALLIANT */

#define	fmod		fmod_x			/* from xclib */

#elif	defined(__alpha__)			/* DEC ALPHA */

#elif	defined(__convex__)			/* CONVEX */

#define	fmod		fmod_x			/* from xclib */

#elif	defined(__cray__)			/* CRAY */

#elif	defined(__hpux__)			/* HP 9000 */

#elif	defined(__linux__)			/* LINUX */

#elif	defined(__mips__)			/* DEC ULTRIX */

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

#elif	defined(__sun__)			/* SUN */

#define	fmod		fmod_x			/* from xclib */

#elif	defined(__vms__)			/* DEC ULTRIX */

#endif

extern	double	acos(double x);
extern	double	asin(double x);
extern	double	atan(double x);
extern	double	atan2(double y, double x);
extern	double	ceil(double x);
extern	double	cos(double x);
extern	double	cosh(double x);
extern	double	exp(double x);
extern	double	fabs(double x);
extern	double	floor(double x);
extern	double	fmod(double x, double y);
extern	double	frexp(double x, int *exp);
extern	double	ldexp(double x, int n);
extern	double	log(double x);
extern	double	log10(double x);
extern	double	modf(double x, double *ip);
extern	double	pow(double x, double y);
extern	double	sin(double x);
extern	double	sinh(double x);
extern	double	sqrt(double x);
extern	double	tan(double x);
extern  double  tanh(double x);

#endif
