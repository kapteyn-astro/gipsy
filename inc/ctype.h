/* ctype.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            ctype.dc3

Header:       ctype.h

Purpose:      Declares ANSI C functions for testing characters.

File:         ctype.h

Author:       K.G. Begeman

Use:          #include "ctype.h"

Defines:      isalnum(int c)   isalpha(c) or isdigit(c) is true
              isalpha(int c)   isupper(c) or islower(c) is true
              iscntrl(int c)   control character
              isdigit(int c)   decimal digit
              isgraph(int c)   printing character except space
              islower(int c)   lower-case character
              isprint(int c)   printing character including space
              ispunct(int c)   printing character except space or letter
                               or digit
              isspace(int c)   space, formfeed, newline, carriage return,
                               tab, vertical tab
              isupper(int c)   upper-case letter
              isxdigit(int c)  hexadecimal digit
              
              int tolower(int c)   convert c to lower case
              int toupper(int c)   convert c to upper case

Warning:      System dependent. At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP 9000, IBM/RS6000,
              SILICON GRAPHICS, SUN and VMS.

Updates:      Apr  8, 1990: KGB, Document created.
              Jun  8, 2009: JPT, Linux now includes from system

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_CTYPE_H)
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
#include	<ctype.h>			/* from system */
#if	!defined(_CTYPE_H)
#define	_CTYPE_H
#endif
#endif
#endif

#if	!defined(_CTYPE_H)
#define	_CTYPE_H

#define	_IS_SP		1			/* is space */
#define	_IS_DIG		2			/* is digit indicator */
#define	_IS_UPP		4			/* is upper case */
#define	_IS_LOW		8			/* is lower case */
#define	_IS_HEX		16			/* [A-F] or [a-f] */
#define	_IS_CTL		32			/* Control */
#define	_IS_PUN		64			/* punctuation */

#if	defined(__aix__)			/* AIX */

extern	char		_X_CTYPE[];		/* Character type array */

#define	tolower		tolower_x		/* from xclib */
#define	toupper		toupper_x		/* from xclib */

#elif	defined(__alliant__)			/* ALLIANT */

extern	char		_X_CTYPE[];		/* Character type array */

#define	tolower		tolower_x		/* from xclib */
#define	toupper		toupper_x		/* from xclib */

#elif	defined(__alpha__)			/* DEC ALPHA */

extern	char		_X_CTYPE[];		/* Character type array */

#elif	defined(__convex__)			/* CONVEX */

extern	char		_X_CTYPE[];		/* Character type array */

#elif	defined(__cray__)			/* CRAY */

extern	char		_X_CTYPE[];		/* character type array */

#elif	defined(__freebsd__)			/* FREEBSD */

extern	char		_X_CTYPE[];		/* character type array */

#elif	defined(__hpux__)			/* HP 9000 */

extern	char		_X_CTYPE[];		/* Character type array */

#elif	defined(__linux__)			/* LINUX */

extern	char		_X_CTYPE[];		/* Character type array */

#elif	defined(__mips__)			/* DEC ULTRIX */

extern	char		_X_CTYPE[];		/* Character type array */

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

extern	char		_X_CTYPE[];		/* Character type array */

#elif	defined(__sun__)			/* SUN */

extern	char		_X_CTYPE[];		/* Character type array */

#define	tolower		tolower_x		/* from xclib */
#define	toupper		toupper_x		/* from xclib */

#elif	defined(__vms__)			/* DEC VMS */

globalref	char	_X_CTYPE[];		/* global characters */

#endif

#define	isalnum(c)	(_X_CTYPE[(c)+1] & (_IS_DIG | _IS_UPP | _IS_LOW))
#define	isalpha(c)	(_X_CTYPE[(c)+1] & (_IS_UPP | _IS_LOW))
#define	iscntrl(c)	(_X_CTYPE[(c)+1] & _IS_CTL)
#define	isdigit(c)	(_X_CTYPE[(c)+1] & _IS_DIG)
#define	isgraph(c)	((c) >= 0x21 && (c) <= 0x7e)
#define	islower(c)	(_X_CTYPE[(c)+1] & _IS_LOW)
#define	isprint(c)	((c) >= 0x20 && (c) <= 0x7e)
#define	ispunct(c)	(_X_CTYPE[(c)+1] & _IS_PUN)
#define	isspace(c)	(_X_CTYPE[(c)+1] & _IS_SP)
#define	isupper(c)	(_X_CTYPE[(c)+1] & _IS_UPP)
#define	isxdigit(c)	(_X_CTYPE[(c)+1] & (_IS_DIG | _IS_HEX))

extern	int	tolower(int c);			/* convert c to lower case */
extern	int	toupper(int c);			/* convert c to upper case */

#endif
