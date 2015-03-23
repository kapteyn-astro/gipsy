/* assert.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            assert.dc3

Header:       assert.h

Purpose:      Defines ANSI C diagnostics for c programs.

File:         assert.h

Author:       K.G. Begeman

Use:          #include "assert.h"

Defines:      assert(expression)

Warning:      System dependent! At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, DEC VMS, HP 9000, IBM AIX
              SGI and SUN.

Updates:      May  9, 1990: KGB, Document created.

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	defined(__GNUC__)			/* GNU C-compiler */

#if	defined(assert)

#undef	assert					/* undefine */
#undef	__assert				/* undefine */

#endif

#if	defined(NDEBUG)

#define	assert(ignore)		((void)0)

#else

extern	void	__eprintf ();			/* Defined in gnulib */

#if	defined(__STDC__)			/* ANSI C */

#define	assert(expression)	\
	((expression) ? 0 : (__assert (#expression, __FILE__, __LINE__)))

#define	__assert(expression, file, line)	\
	(__eprintf ("Failed assertion `%s' at line %d of `%s'.\n",	\
			expression, line, file), 0 )

#else						/* traditional C */

#define	assert(expression)	\
	((void)((expression) ? 0 : __assert (expression, __FILE__, __LINE__)))

#define	__assert(expression, file, lineno)	\
	(__eprintf ("Failed assertion `%s' at line %d of `%s'.\n",	\
			"expression", lineno, file), 0)

#endif

#endif

#elif	defined(__aix__)			/* AIX */

#ifdef NDEBUG
#define assert(ignore) ((void)0)
#else
extern void _assert(char *assertion, char *filename, int line_num);
#define assert(EX) if (##EX) ; else _assert(# EX, __FILE__, __LINE__)
#endif

#elif	defined(__alliant__)			/* ALLIANT */

#if	defined(NDEBUG)

#define	_assert(ex)
#define	assert(ex)

#else

#define	_assert(ex)	\
	{if (!(ex)) {	\
		(void) fprintf(stderr,"Assertion failed: file \"%s\", line %d\n",	\
				 __FILE__, __LINE__);	\
		exit(1);	\
	}}
#define	assert(ex)	_assert(ex)

#endif

#elif	defined(__alpha__)			/* ALPHA */

#if	defined(NDEBUG)

#define	_assert(ex)	;

#else

#define	_assert(ex) \
	{if (!(ex)){	\
		(void) fprintf(stderr,"Assertion failed: file %s, line %d\n",	\
				__FILE__, __LINE__);	\
		exit(1);	\
	}}
#endif

#define	assert(ex)	_assert(ex)

#elif	defined(__convex__)			/* CONVEX */

#if	defined(NDEBUG)

#define	_assert(ex)	;
#define	assert(ex)	;

#else

#define	_assert(ex)	\
	{if (!(ex)) {	\
		(void) fprintf(stderr,"Assertion failed: file %s, line %d\n",	\
			 __FILE__, __LINE__);	\
		exit(1);	\
	}}
#define assert(ex)	\
	{if (!(ex)) {	\
		(void) fprintf(stderr,"Assertion failed: file %s, line %d\n",	\
			 __FILE__, __LINE__);	\
		exit(1);	\
	}}

#endif

#elif	defined(__hpux__)			/* HP 9000 */

#ifdef	assert
#undef	assert
#endif

#ifdef NDEBUG

#define assert(_EX)  ((void)0)

#else

#ifdef __STDC__

extern void	__assert(char *, char *, int);
#define assert(_EX) \
	((_EX) ? (void)0 : __assert(#_EX, __FILE__, __LINE__))

#else

extern void	__assert();
#define assert(_EX) \
	((_EX) ? (void)0 : __assert("_EX", __FILE__, __LINE__))

#endif

#endif

#elif	defined(__mips__)			/* DEC MIPS */

#if	defined(NDEBUG)

#define	_assert(ex)	;

#else

#define	_assert(ex) \
	{if (!(ex)){	\
		(void) fprintf(stderr,"Assertion failed: file %s, line %d\n",	\
				__FILE__, __LINE__);	\
		exit(1);	\
	}}
#endif

#define	assert(ex)	_assert(ex)

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

#ifndef __ASSERT_H__
#define __ASSERT_H__
#endif

#ifdef	NDEBUG
#define	assert(EX)	((void)0)
#else
extern	void	_assert(const char *, const char *, int);
#define	assert(EX)	((EX)?((void)0):_assert( # EX , __FILE__, __LINE__))
#endif

#elif	defined(__sun__)			/* SUN */

#if	defined(NDEBUG)

#define	_assert(ex)
#define	assert(ex)

#else

#define	_assert(ex)	\
	{if (!(ex)) {	\
		(void) fprintf(stderr,"Assertion failed: file \"%s\", line %d\n",	\
			 __FILE__, __LINE__);	\
		exit(1);	\
	}}
#define assert(ex)	_assert(ex)

#endif

#elif	defined(__vms__)			/* DEC VMS */

#if	defined(NDEBUG)

#define	assert(expr)

#else

extern	int	_assert( );

#define	assert(expr)	\
	if (expr) ; else _assert ("expr", __FILE__, __LINE__);

#endif

#else						/* generic */

#include	<assert.h>			/* assume present */

#endif
