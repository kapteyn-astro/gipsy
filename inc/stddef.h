/* stddef.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            stddef.dc3

Header:       stddef.h

Purpose:      Defines some common ANSI C types.

File:         stddef.h

Author:       K.G. Begeman

Use:          #include "stddef.h"

Defines:      size_t       Result of the sizeof operator
              ptrdiff_t    Result of subtracting two pointers
              NULL         Null pointer constant

Warning:      System dependent. At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP 9000, IBM/RS6000,
              SILICON GRAPHICS, SUN and VMS.

Updates:      Apr  8, 1990: KGB, Document created.
              Jan 29, 1999: JPT, Linux now includes from system

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_STDDEF_H)
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
#include	<stddef.h>			/* from system */
#if	!defined(_STDDEF_H)
#define	_STDDEF_H
#endif
#endif
#endif

#if	!defined(_STDDEF_H)
#define	_STDDEF_H

#if	defined(__aix__)			/* AIX */

#ifndef	_H_STDDEF
#define	_H_STDDEF
#endif

#include	<standards.h>

#ifndef _PTRDIFF_T
#define _PTRDIFF_T
typedef	int		ptrdiff_t;
#endif

#ifndef _SIZE_T
#define _SIZE_T
typedef	unsigned long	size_t;
#endif

#ifndef _WCHAR_T
#define _WCHAR_T
typedef unsigned short	wchar_t;
#endif

#ifndef NULL
#define	NULL	((void *)0)
#endif

#elif	defined(__alliant__)			/* ALLIANT */

#include	<sys/types.h>			/* alliant include */

typedef long		ptrdiff_t;		/* result of subtracting two pointers */

#ifndef	NULL
#define	NULL		0			/* traditional null pointer constant */
#endif

#elif	defined(__alpha__)			/* DEC ALPHA */

#include	<standards.h>

#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned long   size_t;
#endif

#ifndef _WCHAR_T
#define _WCHAR_T
typedef unsigned short  wchar_t;
#endif

#ifndef NULL
#define NULL    0L
#endif

#define offsetof(s_name, s_member) (size_t)&(((s_name *)0)->s_member)

#elif	defined(__convex__)			/* CONVEX */

#ifndef _OFF_T
#ifndef __OFF_T
	typedef long off_t;
#define __OFF_T
#if	defined(_CONVEX_SOURCE) && !defined(__OFF64_T)
#define __OFF64_T
	typedef long off64_t; 
#endif
#endif
#define _OFF_T
#endif

#include	"/usr/include/stddef.h"

#ifndef	NULL
#define	NULL		0			/* traditional null pointer constant */
#endif

#elif	defined(__cray__)			/* CRAY */

#ifdef	_STDDEF_H
#undef	_STDDEF_H
#endif
#include	"/usr/include/stddef.h"		/* use standard */
#ifndef _STDDEF_H
#define _STDDEF_H
#endif

#elif	defined(__hpux__)			/* HP 9000 */

#include	"/usr/include/stddef.h"

#ifndef	NULL
#define	NULL		0			/* traditional null pointer constant */
#endif

#elif	defined(__linux__)			/* LINUX */

#include	<sys/types.h>

#ifndef _LINUX_STDDEF_H
#define _LINUX_STDDEF_H
#endif

#ifndef _SIZE_T
#define _SIZE_T
typedef unsigned int size_t;
#endif

#ifndef NULL
#define NULL ((void *)0)
#endif

#ifndef	_WCHAR_T
#define	_WCHAR_T
typedef	unsigned long wchar_t;
#endif

#elif	defined(__mips__)			/* DEC ULTRIX */

#include	"/usr/include/stddef.h"

#ifndef	NULL
#define	NULL		0			/* traditional null pointer constant */
#endif

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

#include	"/usr/include/stddef.h"

#elif	defined(__sun__)			/* SUN */

#ifdef	__bsd__
#include	<sys/stdtypes.h>
#else
#undef	_STDDEF_H
#include	"/usr/include/stddef.h"
#endif

#ifndef	NULL
#define	NULL		0
#endif

#elif	defined(__vms__)			/* DEC VMS */

typedef	int		size_t;			/* result of the sizeof operator */
typedef	int		ptrdiff_t;		/* result of subtracting two pointers */

#define	NULL		(void *) 0		/* traditional null pointer constant */

#endif

#endif
