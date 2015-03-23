/* osdef.h

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            osdef.dc3

Header:       osdef.h

Purpose:      Defines standard architecture defines (i.e.__machine__).

File:         osdef.h

Author:       K.G. Begeman

Use:          #include "osdef.h"

Description:  Including osdef.h ensures that the different preprocessors
              on the same machine generate different defines. For each
              new system/preprocessor this file must be updated. This
              file is included in "gipsyc.h" and all the ANSI include files.
              Also the floating point type used on the current machine
              is defined in OS_FLOATING_TYPE. OS_FLOATING_TYPE can be:
              0 -> IEEE High_Endian with -Inf as BLANK
              1 -> IEEE Little_Endian with -Inf as BLANK
              2 -> CONVEX Native
              3 -> VMS D_Floating
              4 -> VMS G_Floating
              5 -> IEEE High_Endian with -FLT_MAX as BLANK
              6 -> IEEE Little_Endian with -FLT_MAX as BLANK
              Types 5 and 6 are enabled when BLANK2 is defined.
              The integer type used is defined in OS_INTEGER_TYPE.
              OS_INTEGER_TYPE can be:
              0 -> High_Endian
              1 -> Little_Endian
              Further more the architecture is defined in OS_ARCHITECTURE.
              OS_ARCHITECTURE can be:
              "aix"          -> IBM RS6000 systems
              "alliant"      -> alliant fx1/fx4/fx40/fx8/fx80 systems
              "alpha"        -> DEC OSF alpha systems
              "convex"       -> convex systems
              "hp9000s300"   -> hp9000 series 300
              "hp9000s700"   -> hp9000 series 700
              "hp9000s800"   -> hp9000 series 800
              "mips"         -> dec ultrix systems
              "sgi"          -> silicon graphics irix
              "sun4"         -> sun4 systems
              "vms"          -> dec vms systems

Warning:      System dependent! At the moment implemented for ALLIANT,
              CONVEX, DEC ULTRIX, HP9000, IBM/RS6000, SILICON GRAPHICS,
              SUN and DEC VMS.

Updates:      Apr  8, 1990: KGB, Document created.
              May  4, 2007: JPT, Included Apple Mac section.

#<

*/

/*
 * Here we make sure that the correct machine dependent name is
 * defined. Below we search all the possible machine defines which we
 * know of and make sure that __machinename__ is defined. We do NOT
 * undefine any of the other local defines.
 */

#if	!defined(_OSDEF_H)
#define	_OSDEF_H

#if	defined(__g77__) && !defined(__F2C__)
#define	__F2C__
#endif

#if	defined(_AIX) & defined(__STDC__)

#ifndef	__aix__
#define	__aix__
#endif
#ifndef	__unix__
#define	__unix__
#endif
#ifndef	_ANSI_C_SOURCE
#define	_ANSI_C_SOURCE
#endif
#ifndef	_POSIX_SOURCE
#define	_POSIX_SOURCE
#endif
#ifndef	_ALL_SOURCE
#define	_ALL_SOURCE
#endif
#define	OS_ARCHITECTURE		"aix"		/* aix */
#define	OS_FLOATING_TYPE	0		/* IEEE High_Endian */
#define	OS_INTEGER_TYPE		0		/* High_Endian */
#if	!defined(__sysv__)
#define	__sysv__				/* define __sysv__ */
#endif
#if	!defined(SYSV)
#define	SYSV					/* define SYSV */
#endif
#ifndef _H_LIMITS
#define _H_LIMITS 
#endif

#endif
						/* alliant ? */
#if	defined(alliant) | defined(__alliant__)

#if	!defined(__alliant__)
#define	__alliant__				/* define __alliant__ */
#endif
#if	!defined(__bsd__)
#define	__bsd__					/* define __bsd__ */
#endif
#define	OS_ARCHITECTURE		"alliant"	/* alliant */
#define	OS_FLOATING_TYPE	0		/* IEEE High_Endian */
#define	OS_INTEGER_TYPE		0		/* High_Endian */

#endif

						/* OSF/1 AXP alpha */
#if	defined(alpha) | defined(__alpha) | defined(__alpha__)

#ifndef	__alpha__
#define	__alpha__
#endif
#ifndef	__sysv__
#define	__sysv__
#endif
#ifndef	__LANGUAGE_C
#define	__LANGUAGE_C
#endif
#ifndef	LANGUAGE_C
#define	LANGUAGE_C
#endif
#define	OS_ARCHITECTURE		"alpha"		/* alpha */
#define	OS_FLOATING_TYPE	1		/* IEEE Little_Endian */
#define	OS_INTEGER_TYPE		1		/* Little_Endian */

#endif
						/* convex ? */
#if	defined(convex) | defined(__convex__) | defined(__convexc__)

#if	!defined(__convex__)
#define	__convex__				/* define __convex__ */
#endif
#if	!defined(__bsd__)
#define	__bsd__					/* define __bsd__ */
#endif
#define	OS_ARCHITECTURE		"convex"	/* convex */
#if	defined(_IEEE_FLOAT_)
#define	OS_FLOATING_TYPE	0		/* IEEE High_Endian */
#else
#define	OS_FLOATING_TYPE	2		/* CONVEX Native */
#endif
#define	OS_INTEGER_TYPE		0		/* High_Endian */
#if	!defined(_POSIX_SOURCE)
#define	_POSIX_SOURCE
#endif
#if	!defined(_CONVEX_SOURCE)
#define	_CONVEX_SOURCE
#endif

#endif
						/* Cray UNICOS system ? */
#if	defined(cray) | defined(CRAY1) | defined(CRAY2)

#if	!defined(__cray__)
#define	__cray__
#endif
#if	defined(CRAY1)
#if	!defined(__cray1__)
#define	__cray1__
#endif
#define	OS_ARCHITECTURE	"cray"
#endif
#if	defined(CRAY2)
#if	!defined(__cray2__)
#define	__cray2__
#endif
#define	OS_ARCHITECTURE	"CRAY2"
#endif
#ifndef	__sysv__
#define	__sysv__
#endif
#define	OS_FLOATING_TYPE	7		/* Cray internal format */
#define	OS_INTEGER_TYPE		2		/* High_Endian, 8 bytes */
#define	DATA_FLOATING_TYPE	0		/* IEEE High_Endian */
#define	DATA_INTEGER_TYPE	0		/* High_Endian */

#endif

						/* Free BSD */
#if	defined(__FreeBSD__)
#ifndef	__freebsd__
#define	__freebsd__
#endif
#if	!defined(__bsd__)
#define	__bsd__					/* define __bsd__ */
#endif
#define	OS_ARCHITECTURE		"freebsd"	/* free BSD */
#define	OS_FLOATING_TYPE	1		/* IEEE Little_Endian */
#define	OS_INTEGER_TYPE		1		/* Little_Endian */
#ifdef	__GNUC__
#ifndef	__F2C__
#define	__F2C__
#endif
#endif
#endif

						/* HP 9000  ? */
#if	defined(hpux) | defined(__hpux) | defined(__hpux__)

#if	!defined(__hpux__)
#define	__hpux__				/* define __hpux__ */
#endif
#if	defined(hp9000s300) | defined(__hp9000s300) | defined(__hp9000s300__)
#if	!defined(__hp9000s300__)
#define	__hp9000s300__				/* define __hp9000s300__ */
#endif
#if	!defined(__hp9000s300)
#define	__hp9000s300				/* define __hp9000s300 */
#endif
#define	OS_ARCHITECTURE		"hp9000s300"	/* hp9000s300 */
#ifndef	OS_MAJOR_VERSION
#define	OS_MAJOR_VERSION	9
#endif
#elif	defined(hp9000s700) | defined(__hp9000s700) | defined(__hp9000s700__) | defined(__hppa__)
#if	!defined(__hp9000s700__)
#define	__hp9000s700__				/* define __hp9000s700__ */
#endif
#if	!defined(__hp9000s700)
#define	__hp9000s700				/* define __hp9000s700 */
#endif
#if	!defined(__hp9000s800)
#define	__hp9000s800
#endif
#if	defined(__hp9000s800__)
#undef	__hp9000s800__
#endif
#define	OS_ARCHITECTURE		"hp9000s700"	/* hp9000s700 */
#elif	defined(hp9000s800) | defined(__hp9000s800) | defined(__hp9000s800__)
#if	!defined(__hp9000s700__)
#define	__hp9000s700__				/* define __hp9000s700__ */
#endif
#if	!defined(__hp9000s700)
#define	__hp9000s700				/* define __hp9000s700 */
#endif
#if	!defined(__hp9000s800__)
#define	__hp9000s800__				/* define __hp9000s800__ */
#endif
#if	!defined(__hp9000s800)
#define	__hp9000s800				/* define __hp9000s800 */
#endif
#define	OS_ARCHITECTURE		"hp9000s700"	/* hp9000s700 */
#endif						
#ifndef	_HPUX_SOURCE
#define	_HPUX_SOURCE				/* define _HPUX_SOURCE */
#endif
#if	!defined(__sysv__)
#define	__sysv__				/* define __sysv__ */
#endif
#if	!defined(SYSV)
#define	SYSV					/* define SYSV */
#endif
#define	OS_FLOATING_TYPE	0		/* IEEE High_Endian */
#define	OS_INTEGER_TYPE		0		/* High_Endian */

#endif

#if	defined(linux) | defined(__linux__)
#ifndef	__linux__
#define	__linux__
#endif
#if defined(__x86_64__)
#define	OS_ARCHITECTURE		"linux64"	/* Ix86_64 */
#else
#define	OS_ARCHITECTURE		"linux"		/* Ix86 */
#endif
#define	OS_FLOATING_TYPE	1		/* IEEE Little_Endian */
#define	OS_INTEGER_TYPE		1		/* Little_Endian */
#ifndef	__sysv__
#define	__sysv__
#endif
#ifdef	__GNUC__
#ifndef	__F2C__
#define	__F2C__
#endif
#endif
#endif

#if defined(APPLE) | defined(__APPLE__)
#ifndef __APPLE__
#define __APPLE__
#endif
#if     defined(i386) | defined(__i386__)
#define OS_ARCHITECTURE   "apple_i"   /* APPLE Intel */
#define OS_FLOATING_TYPE  1   /* IEEE Little_Endian */
#define OS_INTEGER_TYPE   1   /* Little_Endian */
#elif   defined(x86_64) | defined(__x86_64__)
#define OS_ARCHITECTURE   "apple_i64"   /* APPLE Intel 64-bit */
#define OS_FLOATING_TYPE  1   /* IEEE Little_Endian */
#define OS_INTEGER_TYPE   1   /* Little_Endian */
#else
#define OS_ARCHITECTURE   "apple_m"   /* APPLE Motorola */
#define OS_FLOATING_TYPE  0   /* IEEE Big_Endian */
#define OS_INTEGER_TYPE   0   /* Big_Endian */
#endif
#ifndef __bsd__
#define __bsd__
#endif
#ifndef __unix__
#define __unix__
#endif
#ifdef  __GNUC__
#ifndef __F2C__
#define __F2C__
#endif
#endif
#endif
						/* Silicon Graphics */
#if	defined(__sgi) | defined(__sgi__)
#ifndef	__sgi__
#define	__sgi__
#endif
#ifdef	__mips__
#undef	__mips__
#endif
#ifdef	__mips
#undef	__mips
#endif
#ifdef	mips
#undef	mips
#endif
#ifndef	_ABI_SOURCE
#define	_ABI_SOURCE
#endif
#ifdef	_POSIX_SOURCE
#undef	_POSIX_SOURCE
#endif
#ifndef	_BSD_TYPES
#define	_BSD_TYPES
#endif
#ifndef	_LANGUAGE_C
#define	_LANGUAGE_C
#endif
#ifndef	__EXTENSIONS__
#define	__EXTENSIONS__
#endif
#define	OS_ARCHITECTURE		"sgi"		/* mips */
#define	OS_FLOATING_TYPE	0		/* IEEE Little_Endian */
#define	OS_INTEGER_TYPE		0		/* Little_Endian */
#ifndef	__sysv__
#define	__sysv__
#endif
						/* DEC ULTRIX mips ? */
#elif	defined(mips) | defined(__mips) | defined(__mips__)
#ifndef	__decstation__
#define	__decstation__
#endif
#if	!defined(__mips__)
#define	__mips__				/* define __mips__ */
#endif
#if	!defined(__mips)			/* special trick for mips includes */
#define	__mips					/* define __mips */
#endif
#if     !defined(mips)				/* special trick for mips includes */
#define mips					/* define mips */
#endif
#if	!defined(__bsd__)
#define	__bsd__					/* define __bsd__ */
#endif
#if	!defined(__LANGUAGE_C)			/* special trick for mips includes */
#define	__LANGUAGE_C				/* define __LANGUAGE_C */
#endif
#if	!defined(LANGUAGE_C)			/* special trick for mips includes */
#define	LANGUAGE_C				/* define LANGUAGE_C */
#endif
#define	OS_ARCHITECTURE		"mips"		/* mips */
#define	OS_FLOATING_TYPE	1		/* IEEE Little_Endian */
#define	OS_INTEGER_TYPE		1		/* Little_Endian */
#endif
						/* sun ? */
#if	defined(sun) | defined(__sun__)
#if	!defined(__sun__)
#define	__sun__					/* define __sun__ */
#endif						/* Sparc architecture */
#if	defined(sparc) | defined(__sparc__)
#if	!defined(__sparc__)
#define	__sparc__				/* define __sparc__ */
#endif
#if	!defined(__svr4__)			/* Sunos < 5.0 */
#if	!defined(__bsd__)
#define	__bsd__					/* define __bsd__ */
#endif
#else						/* Solaris */
#if	!defined(__sysv__)
#define	__sysv__				/* define __sysv__ */
#endif
#endif
#if	defined(__bsd__)
#define	OS_ARCHITECTURE		"sun4"		/* sun4 */
#else
#define	OS_ARCHITECTURE		"sol4"		/* sol4 */
#endif
#define	OS_FLOATING_TYPE	0		/* IEEE High_Endian */
#define	OS_INTEGER_TYPE		0		/* High_Endian */
#else
#if	defined(i386) | defined(__i386__)
#if	!defined(__i386__)
#define	__i386___
#endif
#if	!defined(__svr4__)
#define	__svr4__
#endif
#if	!defined(__sysv__)
#define	__sysv__
#endif
#define	OS_ARCHITECTURE		"sun386i"	/* PC */
#define	OS_FLOATING_TYPE	1		/* IEEE Low_Endian */
#define	OS_INTEGER_TYPE		1		/* Low_Endian */
#endif
#endif
#endif
						/* UNIX ? */
#if	defined(unix) | defined(__unix) | defined(__unix__)
#if	!defined(__unix__)
#define	__unix__				/* define __unix__ */
#endif
#if	!defined(unix)
#define	unix					/* define unix */
#endif
#endif
						/* DEC VMS vax ? */
#if	defined(vms) | defined(__vms__) | defined(VMS) | defined(__VMS__)
#if	!defined(__vms__)
#define	__vms__					/* define __vms__ */
#endif
#define	OS_ARCHITECTURE		"vms"		/* vms */
#if	!defined(CC$gfloat)
#define	OS_FLOATING_TYPE	3		/* VMS D_Floating */
#elif	CC$gfloat
#define	OS_FLOATING_TYPE	4		/* VMS G_Floating */
#else
#define	OS_FLOATING_TYPE	3		/* VMS D_Floating */
#endif
#define	OS_INTEGER_TYPE		1		/* Little_Endian */
#endif

#if	!defined(OS_FLOATING_TYPE)		/* use default */
#define	OS_FLOATING_TYPE	0		/* IEEE High_Endian */
#define	OS_INTEGER_TYPE		0		/* High_Endian */
#endif

#if	defined(BLANK2)				/* special BLANK wanted */
#if	OS_FLOATING_TYPE == 0			/* IEEE High_Endian */
#undef	OS_FLOATING_TYPE
#define	OS_FLOATING_TYPE	5
#elif	OS_FLOATING_TYPE == 1			/* IEEE Low_Endian */
#undef	OS_FLOATING_TYPE
#define	OS_FLOATING_TYPE	6
#endif
#undef	BLANK2
#endif

#ifndef	DATA_FLOATING_TYPE
#define	DATA_FLOATING_TYPE	OS_FLOATING_TYPE
#endif

#ifndef	DATA_INTEGER_TYPE
#define	DATA_INTEGER_TYPE	OS_INTEGER_TYPE
#endif

#endif
