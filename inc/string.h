/* string.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            string.dc3

Header:       string.h

Purpose:      Declares the ANSI C string functions.

File:         string.h

Author:       K.G. Begeman

Use:          #include "string.h"

Declares:     char *strcpy(char *s, const char *ct);
              char *strncpy( char *s, const char *ct, size_t n);
              char *strcat(char *s, const char *ct);
              char *strncat(char *s, const char *ct, size_t n);
              char *strchr(const char *cs, int c);
              char *strrchr(const char *cs, int c);
              char *strpbrk(const char *cs, const char *ct);
              char *strstr(const char *cs, const char *ct);
              char *strerror(size_t n);
              char *strtok(char *s, const char *ct);
              int memcmp(const void *cs, const void *ct, size_t n);
              int strcmp(const char *cs, const char *ct);
              int strncmp(const char *cs, const char *ct, size_t n);
              size_t strspn(const char *cs, const char *ct);
              size_t strcspn(const char *cs, const char *ct);
              size_t strlen(const char *cs);
              void *memcpy(void *s, const void *ct, size_t n);
              void *memmove(void *s, const void *ct, size_t n);
              void *memchr(const coid *cs, int c, size_t n);
              void *memset(void *s, int c, size_t n);

Warning:      System dependent! At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP9000, SILICON GRAPHICS,
              SUN and DEC VMS.

Updates:      Apr  8, 1990: KGB, Document created.
              Jun  8, 2009: JPT, Linux now includes from system
#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_STRING_H)
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
#include	<string.h>			/* from system */
#if	!defined(_STRING_H)
#define	_STRING_H
#endif
#endif
#endif

#if	!defined(_STRING_H)
#define _STRING_H

#if	!defined(_STDDEF_H)
#include	"stddef.h"
#endif

#if	defined(__aix__)			/* AIX */

#elif	defined(__alliant__)			/* ALLIANT */

#define	memchr		memchr_x		/* from xclib */
#define	memcmp		memcmp_x		/* from xclib */
#define	memcpy		memcpy_x		/* from xclib */
#define	memmove 	memmove_x		/* from xclib */
#define	memset		memset_x		/* from xclib */
#define	strchr		strchr_x		/* from xclib */
#define	strcspn		strcspn_x		/* from xclib */
#define	strerror	strerror_x		/* from xclib */
#define	strpbrk		strpbrk_x		/* from xclib */
#define	strrchr		strrchr_x		/* from xclib */
#define	strspn		strspn_x		/* from xclib */
#define	strstr		strstr_x		/* from xclib */
#define	strtok		strtok_x		/* from xclib */

#elif	defined(__alpha__)			/* DEC ALPHA */

#elif	defined(__convex__)			/* CONVEX */

#define	memchr		memchr_x		/* from xclib */
#define	memcmp		memcmp_x		/* from xclib */
#define	memcpy		memcpy_x		/* from xclib */
#define	memmove 	memmove_x		/* from xclib */
#define	memset		memset_x		/* from xclib */
#define	strchr		strchr_x		/* from xclib */
#define	strcspn		strcspn_x		/* from xclib */
#define	strerror	strerror_x		/* from xclib */
#define	strpbrk		strpbrk_x		/* from xclib */
#define	strrchr		strrchr_x		/* from xclib */
#define	strspn		strspn_x		/* from xclib */
#define	strstr		strstr_x		/* from xclib */
#define	strtok		strtok_x		/* from xclib */

#elif	defined(__cray__)			/* CRAY */

#elif	defined(__freebsd__)			/* FREEBSD */

#elif	defined(__hpux__)			/* HP 9000 */

#elif	defined(__linux__)			/* LINUX */

#elif	defined(__mips__)			/* DEC ULTRIX */

#define	memmove		memmove_x		/* from xclib */
#define	strerror	strerror_x		/* from xclib */
#define	strstr		strstr_x		/* from xclib */

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

#ifndef __STRING_H__
#define __STRING_H__
#endif

#elif	defined(__sun__)			/* SUN */

#ifdef   __bsd__
#define	memmove		memmove_x		/* from xclib */
#define	strerror	strerror_x		/* from xclib */
#define	strstr		strstr_x		/* from xclib */
#endif

#elif	defined(__vms__)			/* DEC VMS */

#define	memchr		memchr_x		/* from xclib */
#define	memcmp		memcmp_x		/* from xclib */
#define	memcpy		memcpy_x		/* from xclib */
#define	memmove		memmove_x		/* from xclib */
#define	memset		memset_x		/* from xclib */
#define	strstr		strstr_x		/* from xclib */
#define	strtok		strtok_x		/* from xclib */

#endif

extern	char	*strcpy(char *s, const char *ct);
extern	char	*strncpy( char *s, const char *ct, size_t n);
extern	char	*strcat(char *s, const char *ct);
extern	char	*strncat(char *s, const char *ct, size_t n);
extern	char	*strchr(const char *cs, int c);
extern	char	*strrchr(const char *cs, int c);
extern	char	*strpbrk(const char *cs, const char *ct);
extern	char	*strstr(const char *cs, const char *ct);
extern	char	*strerror(size_t n);
extern	char	*strtok(char *s, const char *ct);
extern	int	memcmp(const void *cs, const void *ct, size_t n);
extern	int	strcmp(const char *cs, const char *ct);
extern	int	strncmp(const char *cs, const char *ct, size_t n);
extern	size_t	strspn(const char *cs, const char *ct);
extern	size_t	strcspn(const char *cs, const char *ct);
extern	size_t	strlen(const char *cs);
extern	void	*memcpy(void *s, const void *ct, size_t n);
extern	void	*memmove(void *s, const void *ct, size_t n);
extern	void	*memchr(const void *cs, int c, size_t n);
extern	void	*memset(void *s, int c, size_t n);

#endif
