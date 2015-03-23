/* stdlib.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            stdlib.dc3

Header:       stdlib

Purpose:      Defines the ANSI C functions for number conversion, storage
              allocation, and similar tasks.

File:         stdlib.h

Author:       K.G. Begeman

Use:          #include "stdlib.h"

Defines:      EXIT_FAILURE    Exit with error.
              EXIT_SUCCESS    Exit with no error.
              HUGE_VAL        Large double.
              RAND_MAX        Range of rand function.
              struct div_t    Result of div function.
              struct ldiv_t   Result of ldiv function.

Declares:     char *getenv(const char *name);
              div_t div(int num, int denom);
              double atof(const char *s);
              double strtod(const char *s, char **endp);
              int abs(int n);
              int atexit(void (*fcn)(void));
              int atoi(const char *s);
              int rand(void);
              int system(const char*s);
              ldiv_t ldiv(long num, long denom);
              long atol(const char *s);
              long labs(long n);
              long strtol(const char *s, char **endp, int base);
              unsigned long strtoul(const char *s, char **endp, int base);
              void abort(void);
              void exit(int status);
              void free(void *p);
              void qsort(void *base, size_t n,size_t size,
                         int (*cmp)(const void *, const void *));
              void srand(unsigned int seed);
              void *bsearch(const void *key, const void *base,
                            size_t n, size_t size,
                            int (*cmp)(const void *keyval, const void *datum));
              void *calloc(size_t nobj, size_t size);
              void *malloc(size_t size);
              void *realloc(void *p, size_t size);

Warning:      System dependent! At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP9000, SILICON GRAPHICS,
              SUN and DEC VMS.

Updates:      Apr 11, 1990: KGB, Document created.
              May 17, 1993: KGB, RAND_MAX for hpux corrected.
              Sep 22, 1998: KGB/VOG, RAND_MAX for svr4 included.
              Jun  8, 2009: JPT, Linux now includes from system

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_STDLIB_H)
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
#include	<stdlib.h>			/* from system */
#if	!defined(_STDLIB_H)
#define	_STDLIB_H
#endif
#endif
#endif

#if	!defined(_STDLIB_H)
#define	_STDLIB_H

#if	!defined(_STDDEF_H)
#include	"stddef.h"
#endif

#if	defined(__aix__)			/* AIX */

#ifndef	_H_STDLIB
#define	_H_STDLIB
#endif

#include <sys/localedef.h>

typedef struct div_t  {			/* struct returned from div	*/
	int quot;			/* quotient			*/
	int rem;			/* remainder			*/
 } div_t;

typedef struct ldiv_t  {		/* struct returned from ldiv	*/
	long int quot;			/* quotient			*/
	long int rem;			/* remainder			*/
 } ldiv_t;

#define RAND_MAX	32767		/* max value returned by rand	*/
#define	HUGE_VAL	_huge_val( )		/* Huge value */

#ifdef	OBJ_DATA
#define	MB_CUR_MAX		(OBJ_DATA(__lc_charmap)->cm_mb_cur_max)
#define	__max_disp_width	(OBJ_DATA(__lc_charmap)->cm_max_disp_width)
#else
#define	MB_CUR_MAX		(__lc_charmap->cm_mb_cur_max)
#define	__max_disp_width	(__lc_charmap->cm_max_disp_width)
#endif

#define	system		system_x		/* from xclib */

#elif	defined(__alliant__)			/* ALLIANT */

typedef	struct	{				/* structure returned by div */
	int	quot;				/* quotient */
	int	rem;				/* remainder */
} div_t;

typedef	struct	{				/* structure returned by ldiv */
	long	quot;				/* quotient */
	long	rem;				/* remainder */
} ldiv_t;

#define	RAND_MAX	0x7fffffff		/* Max. returned by "rand" function */
#define	HUGE_VAL	_huge_val( )		/* Huge value */

extern	double	_huge_val(void);		/* HUGE_VAL */

#define	abs		abs_x			/* from xclib */
#define	atexit		atexit_x		/* from xclib */
#define	bsearch		bsearch_x		/* from xclib */
#define	div		div_x			/* from xclib */
#define	labs		labs_x			/* from xclib */
#define	ldiv		ldiv_x			/* from xclib */
#define	strtod		strtod_x		/* from xclib */
#define	strtol		strtol_x		/* from xclib */
#define	strtoul		strtoul_x		/* from xclib */
#define	system		system_x		/* from xclib */


#elif	defined(__alpha__)			/* DEC ALPHA */

typedef struct div_t  {			/* struct returned from div	*/
	int quot;			/* quotient			*/
	int rem;			/* remainder			*/
} div_t;

typedef struct ldiv_t  {		/* struct returned from ldiv	*/
	long int quot;			/* quotient			*/
	long int rem;			/* remainder			*/
} ldiv_t;

#ifdef	_BSD
#define RAND_MAX        2147483647      /* max value returned by rand   */
#else
#define RAND_MAX        32767           /* max value returned by rand   */
#endif

#define	HUGE_VAL	_huge_val( )		/* Huge value */

extern	double	_huge_val(void);		/* HUGE_VAL */

#define	system		system_x		/* from xclib */

#elif	defined(__convex__)			/* CONVEX */

typedef	struct	{				/* structure returned by div */
	int	quot;				/* quotient */
	int	rem;				/* remainder */
} div_t;

typedef	struct	{				/* structure returned by ldiv */
	long	quot;				/* quotient */
	long	rem;				/* remainder */
} ldiv_t;

#define	RAND_MAX	0x7fffffff		/* Max. returned by "rand" function */
#define	HUGE_VAL	_huge_val( )		/* Huge value */

extern	double	_huge_val(void);		/* HUGE_VAL */

#define	abs		abs_x			/* from xclib */
#define	bsearch		bsearch_x		/* from xclib */
#define	div		div_x			/* from xclib */
#define	labs		labs_x			/* from xclib */
#define	ldiv		ldiv_x			/* from xclib */
#define	strtod		strtod_x		/* from xclib */
#define	strtol		strtol_x		/* from xclib */
#define	strtoul		strtoul_x		/* from xclib */
#define	system		system_x		/* from xclib */

#elif	defined(__cray__)			/* CRAY */

#include	"/usr/include/stdlib.h"		/* Use standard */

#elif	defined(__hpux__)			/* HP 9000 */

typedef	struct {
	int	quot;				/* quotient */
	int	rem;				/* remainder */
} div_t;

typedef	struct {
	long int	quot;			/* quotient */
	long int	rem;			/* remainder */
} ldiv_t;

#define	RAND_MAX	32767			/* Max. returned by "rand" function */
#define	HUGE_VAL	_huge_val( )		/* Huge value */

#define	system		system_x		/* from xclib */

#elif	defined(__linux__)			/* LINUX */

typedef struct {
    int quot;	/* Quotient.  */
    int rem;	/* Remainder.  */
} div_t;

typedef struct {
    long quot;	/* Quotient.  */
    long rem;	/* Remainder.  */
} ldiv_t;

#ifndef RAND_MAX
#define	RAND_MAX	2147483647
#endif
#define	MB_CUR_MAX	1
#define	HUGE_VAL	_huge_val( )		/* Huge value */

#elif	defined(__mips__)			/* DEC ULTRIX */

typedef	struct	{				/* structure returned by div */
	int	quot;				/* quotient */
	int	rem;				/* remainder */
} div_t;

typedef	struct	{				/* structure returned by ldiv */
	long	quot;				/* quotient */
	long	rem;				/* remainder */
} ldiv_t;

#define	RAND_MAX	0x7fffffff		/* Max. returned by "rand" function */
#define	HUGE_VAL	_huge_val( )		/* Huge value */

extern	double	_huge_val(void);		/* HUGE_VAL */

#define	abs		abs_x			/* from xclib */
#define	div		div_x			/* from xclib */
#define	labs		labs_x			/* from xclib */
#define	ldiv		ldiv_x			/* from xclib */
#define	strtod		strtod_x		/* from xclib */
#define	strtoul		strtoul_x		/* from xclib */
#define	system		system_x		/* from xclib */

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

#ifndef __STDLIB_H__
#define __STDLIB_H__
#endif

#ifndef NULL
#define NULL    0
#endif

#define RAND_MAX        32767			/* Maximum value returned by rand */
#define MB_CUR_MAX      1			/* Maximum # of bytes in a multibyte character. */

struct __div_s {
	int	quot;
	int	rem;
};
typedef	struct __div_s	div_t;

struct __ldiv_s {
	long int	quot;
	long int	rem;
};
typedef	struct __ldiv_s	ldiv_t;

#ifndef _SIZE_T_
#define _SIZE_T_
#ifndef	_SIZE_T
#define	_SIZE_T
typedef	unsigned int	size_t;
#endif
#endif

#ifndef _WCHAR_T_
#define _WCHAR_T_
#ifndef	_WCHAR_T
#define	_WCHAR_T
typedef	unsigned char	wchar_t;
#endif
#endif

#define	system		system_x		/* from xclib */

#elif	defined(__sun__)			/* SUN */

typedef	struct	{				/* structure returned by div */
	int	quot;				/* quotient */
	int	rem;				/* remainder */
} div_t;

typedef	struct	{				/* structure returned by ldiv */
	long	quot;				/* quotient */
	long	rem;				/* remainder */
} ldiv_t;

#if	defined(__mc86000__) | defined(mc86000)	/* SUN MC86000 */

#define	RAND_MAX	0x7fff			/* Max. returned by "rand" function */

#elif	defined(__svr4__)

#define	RAND_MAX	0x7fff			/* Max. returned by "rand" function */

#else

#define	RAND_MAX	0x7fffffff		/* Max. returned by "rand" function */

#endif

#define	HUGE_VAL	_huge_val( )		/* Huge value */

extern	double	_huge_val(void);		/* HUGE_VAL */

#define	abs		abs_x			/* from xclib */
#ifdef	__bsd__					/* SunOS < 5.0 */
#define	atexit		atexit_x		/* from xclib */
#endif
#define	div		div_x			/* from xclib */
#define	labs		labs_x			/* from xclib */
#define	ldiv		ldiv_x			/* from xclib */
#define	realloc		realloc_x		/* from xclib */
#define	strtoul		strtoul_x		/* from xclib */
#define	system		system_x		/* from xclib */

#elif	defined(__vms__)			/* DEC VMS */

typedef	struct	{				/* structure returned by div */
	int	quot;				/* quotient */
	int	rem;				/* remainder */
} div_t;

typedef	struct	{				/* structure returned by ldiv */
	long	quot;				/* quotient */
	long	rem;				/* remainder */
} ldiv_t;

#define	RAND_MAX	0x7fffffff		/* Max. returned by "rand" function */
#define	HUGE_VAL	_huge_val( )		/* Huge value */

extern	double	_huge_val(void);		/* HUGE_VAL */

#define	abs		abs_x			/* from xclib */
#define	div		div_x			/* from xclib */
#define	getenv		getenv_x		/* from xclib */
#define	labs		labs_x			/* from xclib */
#define	ldiv		ldiv_x			/* from xclib */
#define	realloc		realloc_x		/* from xclib */

#endif

#define	EXIT_FAILURE	1			/* Exit with error */
#define	EXIT_SUCCESS	0			/* Exit with no error */

extern	char	*getenv(const char *name);
extern	div_t	div(int num, int denom);
extern	double	atof(const char *s);
extern	double	strtod(const char *s, char **endp);
extern	int	abs(int n);
extern	int	atexit(void (*fcn)(void));
extern	int	atoi(const char *s);
extern	int	rand(void);
extern	int	system(const char*s);
extern	ldiv_t	ldiv(long num, long denom);
extern	long	atol(const char *s);
extern	long	labs(long n);
extern	long	strtol(const char *s, char **endp, int base);
extern	unsigned	long strtoul(const char *s, char **endp, int base);
extern	void	abort(void);
extern	void	exit(int status);
extern	void	free(void *p);
extern	void	qsort(void *base, size_t n,size_t size,
			int (*cmp)(const void *, const void *));
extern	void	srand(unsigned int seed);
extern	void	*bsearch(const void *key, const void *base,
			size_t n, size_t size,
			int (*cmp)(const void *keyval, const void *datum));
extern	void	*calloc(size_t nobj, size_t size);
extern	void	*malloc(size_t size);
extern	void	*realloc(void *p, size_t size);

#endif
