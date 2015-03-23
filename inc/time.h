/* time.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            time.dc3

Header:       stdio.h

Purpose:      Defines ANSI C date and time functions.

File:         time.h

Author:       K.G. Begeman

Use:          #include "time.h"

Defines:      CLOCKS_PER_SEC, CLK_TCK

Declares:     clock_t clock(void)
              time_t time(time_t *tp)
              double difftime(time_t time2, time_t time1)
              time_t mktime(struct tm *tp)
              char *asctime(const struct tm *tp)
              char *ctime(const time_t *tp)
              struct tm *gmtime(const time_t *tp)
              struct tm *localtime(const time_t *tp)
              size_t strftime(char *s, size_t smax, const char *fmt,
                     const struct tm *tp)

Warning:      System dependent! At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP 9000, IBM/RS6000,
              SILICON GRAPHICS, SUN and DEC VMS.

Updates:      Apr  8, 1990: KGB, Document created.
              Jul  7, 1999: JPT, Linux now includes from system.

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_TIME_H)
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
#include	<time.h>			/* from system */
#if	!defined(_TIME_H)
#define	_TIME_H
#endif
#endif
#endif

#if	!defined(_TIME_H)
#define	_TIME_H

#if	!defined(_STDDEF_H)
#include	"stddef.h"
#endif

#if	defined(__aix__)			/* AIX */

#ifndef _CLOCK_T
#define _CLOCK_T
typedef int		clock_t;
#endif

#ifndef _TIME_T
#define _TIME_T
typedef long		time_t;
#endif

#ifndef CLK_TCK
#define CLK_TCK   1000000
#endif

struct	tm {	/* see ctime(3) */
	int	tm_sec;
	int	tm_min;
	int	tm_hour;
	int	tm_mday;
	int	tm_mon;
	int	tm_year;
	int	tm_wday;
	int	tm_yday;
	int	tm_isdst;
};

#elif	defined(__alliant__)			/* ALLIANT */

#include <sys/time.h>

typedef	long		clock_t;		/* return type of clock */

#ifndef	CLK_TCK
#define	CLK_TCK		60			/* ticks/second */
#endif

#define	clock		clock_x			/* from xclib */
#define	difftime	difftime_x		/* from xclib */
#define	mktime		mktime_x		/* from xclib */
#define	strftime	strftime_x		/* from xclib */

#elif	defined(__alpha__)			/* DEC ALPHA */

#ifndef CLK_TCK
#define CLK_TCK   1000000
#endif

#ifndef _CLOCK_T
#define _CLOCK_T
typedef int             clock_t;
#endif

#ifndef _TIME_T
#define _TIME_T
typedef int           time_t;
#endif

struct	tm {			/* see ctime(3) */
        int     tm_sec;         /* seconds after the minute [0-60] */
        int     tm_min;         /* minutes after the hour [0-59] */
        int     tm_hour;        /* hours since midnight [0-23] */
        int     tm_mday;        /* day of the month [1-31] */
        int     tm_mon;         /* months since January [0-11] */
        int     tm_year;        /* years since 1900 */
        int     tm_wday;        /* days since Sunday [0-6] */
        int     tm_yday;        /* days since January 1 [0-365] */
        int     tm_isdst;       /* Daylight Savings Time flag */
	long    tm_gmtoff;
        char    *tm_zone;
};

#elif	defined(__convex__)			/* CONVEX */

#include	"/usr/include/time.h"

#ifdef	difftime				/* might be defined! */
#undef	difftime
#endif

#ifndef	CLK_TCK
#define	CLK_TCK		60			/* ticks/second */
#endif

#elif	defined(__cray__)			/* CRAY */

#include	"/usr/include/time.h"		/* Use standard */

#elif	defined(__hpux__)			/* HP 9000 */

#include	"/usr/include/time.h"

#ifdef	CLK_TCK
#undef	CLK_TCK
#endif
#if	defined(__hp9000s300__)
#define	CLK_TCK		50			/* ticks/second */
#elif	defined(__hp9000s700__)
#define	CLK_TCK		1000000			/* ticks/second */
#elif	defined(__hp9000s800__)
#define	CLK_TCK		100			/* ticks/second */
#endif

#elif	defined(__mips__)			/* DEC ULTRIX workstation */

#include	"/usr/include/time.h"

#ifndef	CLK_TCK
#define	CLK_TCK		60			/* ticks/second */
#endif

#define	clock		clock_x			/* from xclib */
#define	difftime	difftime_x		/* from xclib */
#define	mktime		mktime_x		/* from xclib */
#define	strftime	strftime_x		/* from xclib */

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

#include	"/usr/include/time.h"		/* Use Standard */

#ifdef	CLK_TCK
#undef	CLK_TCK
#endif
#define	CLK_TCK	CLOCKS_PER_SEC

#elif	defined(__sun__)			/* SUN */

#if	0
struct tm {
	int	tm_sec;				/* seconds after the minute */
	int	tm_min;				/* minutes after the hour */
	int	tm_hour;			/* hours since midnight */
	int	tm_mday;			/* day of the month */
	int	tm_mon;				/* months since January */
	int	tm_year;			/* years since 1900 */
	int	tm_wday;			/* days since Sunday */
	int	tm_yday;			/* days since January 1 */
	int	tm_isdst;			/* Daylight Saving Time flag */
	long	tm_gmtoff;			/* NOT ANSI C */
	char	*tm_zone;			/* NOT ANSI C */
};

typedef	long		clock_t;		/* return type of clock */
typedef	long		time_t;			/* return type of time */

#define	CLK_TCK		60			/* ticks/second */

#endif

#ifdef	__sysv__
#undef	_TIME_H
#endif

#include	"/usr/include/time.h"

#ifndef	CLK_TCK
	
#define	CLK_TCK		60			/* ticks/second */

#endif

#if 0
#define	clock		clock_x			/* from xclib */
#define	difftime	difftime_x		/* from xclib */
#define	mktime		mktime_x		/* from xclib */
#define	strftime	strftime_x		/* from xclib */
#endif

#elif	defined(__vms__)			/* DEC VMS */

struct tm {
	int	tm_sec;				/* seconds after the minute */
	int	tm_min;				/* minutes after the hour */
	int	tm_hour;			/* hours since midnight */
	int	tm_mday;			/* day of the month */
	int	tm_mon;				/* months since January */
	int	tm_year;			/* years since 1900 */
	int	tm_wday;			/* days since Sunday */
	int	tm_yday;			/* days since January 1 */
	int	tm_isdst;			/* Daylight Saving Time flag */
};

typedef	long		clock_t;
typedef	unsigned long	time_t;

#define	CLK_TCK		(100)			/* ticks second */

#define	mktime		mktime_x		/* from xclib */
#define	strftime	strftime_x		/* from xclib */

#endif

extern	clock_t		clock(void);
extern	double		difftime(time_t time2, time_t time1);
extern	time_t		time(time_t *tp);
extern	time_t		mktime(struct tm *tp);
extern	char		*asctime(const struct tm *tp);
extern	char		*ctime(const time_t *tp);
extern	struct tm	*gmtime(const time_t *tp);
extern	struct tm	*localtime(const time_t *tp);
extern	size_t		strftime(char *s, size_t smax, const char *fmt, const struct tm *tp);

#endif

#if !defined(CLOCKS_PER_SEC)
#define CLOCKS_PER_SEC CLK_TCK                /* last resort, probably wrong */
#endif
