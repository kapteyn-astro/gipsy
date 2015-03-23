/* stdarg.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            stdarg.dc3

Header:       stdarg.h

Purpose:      Defines ANSI C macros for the variable argument list.

File:         stdarg.h

Author:       K.G. Begeman

Use:          #include "stdarg.h"

Declares:     va_list
              va_start(va_list ap, lastarg)
              va_arg(va_list ap, type)
              va_end(va_list ap)

Warning:      System dependent. At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP 9000, IBM/RS6000,
              SILICON GRAPHICS, SUN and VMS.

Updates:      May  3, 1990: KGB, Document created.

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_STDARG_H)
#if	defined(__aix__)
#elif	defined(__alliant__)
#elif	defined(__alpha__)
#elif	defined(__convex__)
#elif	defined(__hpux__)
#elif	defined(__mips__)
#elif	defined(__sgi__)
#elif	defined(__sun__)
#elif	defined(__vms__)
#else
#include	<stdarg.h>			/* from system */
#if	!defined(_STDARG_H)
#define	_STDARG_H
#endif
#endif
#endif

#if	!defined(_STDARG_H)
#define	_STDARG_H

#if	defined(__aix__)			/* AIX */

#ifndef	_VA_LIST
#define _VA_LIST
typedef char *	va_list;
#endif

#define va_start(list,parmN) list = (char *) (&(parmN) + 1)
#define va_end(list)
#define va_arg(list, mode) ((mode *)((list) += sizeof(mode)))[-1]

#elif	defined(__alliant__)			/* ALLIANT */

typedef char *va_list;

void	va_end(va_list);				/* Defined in gnulib */

#define __va_rounded_size(TYPE)						\
	(((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))
#define va_start(AP,LASTARG)						\
	(AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)))
#define va_end(AP)
#define va_arg(AP,TYPE)							\
	(AP += __va_rounded_size (TYPE),				\
	*((TYPE *) (AP - __va_rounded_size (TYPE))))

#elif	defined(__alpha__)				/* DEC ALPHA */

#if	0

typedef struct {
	char	*a0;		/* pointer to first homed integer arg */
	int	offset;		/* byte offset of next param */
} va_list;

#define va_dcl long va_alist;
#define va_start(list, parmN) __builtin_va_start(list, parmN, 1)
#define va_end(list)
#define va_arg(list, mode) \
	(*(((list).offset += ((int)sizeof(mode) + 7) & -8), \
	(mode *)((list).a0 + (list).offset - \
	((__builtin_isfloat(mode) && (list).offset <= (6 * 8)) ? \
	(6 * 8) + 8 : ((int)sizeof(mode) + 7) & -8))))

#else

#ifndef	_GIPSY_STDARG_H
#define	_GIPSY_STDARG_H
#undef	_STDARG_H
#include	"/usr/include/stdarg.h"
#endif

#endif

#elif	defined(__convex__)				/* CONVEX */

typedef char *va_list;

void	va_end(va_list);				/* Defined in gnulib */

#define __va_rounded_size(TYPE)						\
	(((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))
#define va_start(AP,LASTARG)						\
	(AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)))
#define va_end(AP)
#define va_arg(AP,TYPE)							\
	(AP += __va_rounded_size (TYPE),				\
	*((TYPE *) (AP - __va_rounded_size (TYPE))))

#elif	defined(__hpux__)				/* HP 9000 */

#if	defined(__GNUC__)

#if	__GNUC__ > 1

#ifndef __GNUC_VA_LIST
#define __GNUC_VA_LIST

typedef double *__gnuc_va_list;

#endif

#define __va_ellipsis ...
#define __gnuc_va_start(AP) ((AP) = (va_list)__builtin_saveregs())
#define va_start(AP,LASTARG) __gnuc_va_start (AP)
#define va_arg(AP,TYPE)						\
  (sizeof(TYPE) > 8 ?						\
   ((AP = (__gnuc_va_list) ((char *)AP - sizeof (int))),	\
    (*((TYPE *) (void *) (*((int *) (AP))))))			\
   :((AP =							\
      (__gnuc_va_list) ((long)((char *)AP - sizeof (TYPE))	\
			& (sizeof(TYPE) > 4 ? ~0x7 : ~0x3))),	\
     (*((TYPE *) (void *) ((char *)AP + ((8 - sizeof(TYPE)) % 4))))))
#define va_end(AP)
typedef	__gnuc_va_list	va_list;

#else

typedef	char	*va_list;
#define	__va_rounded_size(TYPE)  \
	(((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))
#define	va_start(AP, LASTARG) \
	(AP = ((char *) __builtin_next_arg ()))
void	va_end (va_list);				/* Defined in gnulib */
#define	va_end(AP)
#define	va_arg(AP, TYPE) \
	(*((TYPE *) (AP += __va_rounded_size (TYPE), \
	AP - (sizeof (TYPE) < 4 ? sizeof (TYPE) : __va_rounded_size (TYPE)))))
#endif

#else

#if	defined(__hp9000s300__)

typedef	char	*va_list;
#define	va_start(__list,__parmN)	__list = (char *) ((char *)(&__parmN) \
					+ sizeof(__parmN))
#define	va_arg(__list,__mode)		((__mode *)(__list += sizeof(__mode)))[-1]
#define	va_end(__list)

#endif

#if	defined(__hp9000s700__) | defined(__hp9000s800__)

#define	__WORD_MASK	0xFFFFFFFC
#define	__DW_MASK	0xFFFFFFF8

typedef	double	*va_list;

#define	va_start(__list,__parmN)	__builtin_va_start (__list, &__parmN)
#define	va_arg(__list,__mode)		(sizeof(__mode) > 8 ? \
		((__list = (va_list) ((char *)__list - sizeof (int))), \
		(*((__mode *) (*((int *) (__list)))))) : \
		((__list = \
		(va_list) ((long)((char *)__list - sizeof (__mode)) \
		& (sizeof(__mode) > 4 ? __DW_MASK : __WORD_MASK))), \
		(*((__mode *) ((char *)__list + \
		((8 - sizeof(__mode)) % 4))))))
#define	va_end(__list)

#endif

#endif

#elif	defined(__mips__)				/* DEC ULTRIX */

#if	defined(__GNUC__)

typedef char	*va_list;

#define __va_rounded_size(TYPE) \
	(((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))
#define va_start(AP, LASTARG) \
	(AP = ((char *) __builtin_next_arg ()))
void va_end (va_list);					/* Defined in gnulib */
#define va_end(AP)
#define va_arg(AP, mode) \
	((mode *)(AP = (char *) (sizeof(mode) > 4 ? ((int)AP + 2*8 - 1) & -8 \
	: ((int)AP + 2*4 - 1) & -4)))[-1]

#else

typedef char	*va_list;
#define va_end(list)
#define va_start(list, parmN) (list = \
	(char *) (sizeof(parmN) > 4 ? ((int)&parmN + 2*8 - 1) & -8 \
				    : ((int)&parmN + 2*4 - 1) & -4))

#define va_arg(list, mode) ((mode *)(list = \
	(char *) (sizeof(mode) > 4 ? ((int)list + 2*8 - 1) & -8 \
				   : ((int)list + 2*4 - 1) & -4)))[-1]

#endif

#elif	defined(__sgi__)				/* SILICON GRAPHICS */

#if	defined(__GNUC__)
#else

#include	"/usr/include/stdarg.h"

#endif

#elif	defined(__sun__)				/* SUN */

#if	defined(__GNUC__)

typedef char *va_list;

void	va_end(va_list);				/* Defined in gnulib */

#define __va_rounded_size(TYPE)						\
	(((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#if	defined(__sparc__) | defined(sparc)		/* sparc */

#if	__GNUC__ == 2

#define va_start(AP, LASTARG) 						\
 (__builtin_saveregs (),						\
  AP = ((char *) __builtin_next_arg ()))

#else

#define va_start(AP,LASTARG)						\
	(__builtin_saveregs (),						\
	AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)))
#endif

#else

#define va_start(AP,LASTARG)						\
	(AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)))

#endif

#define va_end(AP)

#if	__GNUC__ == 1

#define va_arg(AP,TYPE)							\
	(AP += __va_rounded_size (TYPE),				\
	*((TYPE *) (AP - __va_rounded_size (TYPE))))

#else

#define va_arg(pvar,TYPE)                                       \
__extension__                                                   \
({ TYPE __va_temp;                                              \
   ((__builtin_classify_type (__va_temp) >= 12)                 \
    ? ((pvar) += __va_rounded_size (TYPE *),                    \
       **(TYPE **) ((pvar) - __va_rounded_size (TYPE *)))       \
    : __va_rounded_size (TYPE) == 8                             \
    ? ({ union {TYPE d; int i[2];} u;                           \
         u.i[0] = ((int *) (pvar))[0];                          \
         u.i[1] = ((int *) (pvar))[1];                          \
         (pvar) += 8;                                           \
         u.d; })                                                \
    : ((pvar) += __va_rounded_size (TYPE),                      \
       *((TYPE *) ((pvar) - __va_rounded_size (TYPE)))));})

#endif

#else

#if	defined(__SunOS_5_0) | defined(__SunOS_5_1) | defined(__SunOS_5_2) | defined(__SunOS_5_3) | defined(__SunOS_5_4)

typedef	void	*va_list;

#define	va_start(list, name)	(void) (list = (va_list) &__builtin_va_alist)

#if	defined(__sparc__) | defined(sparc)
#define	va_arg(list, mode)	((mode *)__builtin_va_arg_incr((mode *)list))[0]
#else
#define	va_arg(list, mode)	((mode *)(list += sizeof (mode)))[-1]
#endif

extern	void	va_end(va_list);
#define	va_end(list)	((void)0)

#else

#ifndef	_GIPSY_STDARG_H
#define	_GIPSY_STDARG_H
#undef	_STDARG_H
#include	"/usr/include/stdarg.h"
#endif

#endif

#endif

#elif	defined(__vms__)				/* DEC VMS */

typedef	char	*va_list;

#define	va_start(AP,LASTARG)						\
	(AP = (char *)&LASTARG + sizeof(LASTARG))
#define	va_arg(AP,TYPE)							\
	((AP=(va_list)(((int)AP+3)&~3)),((TYPE *)(AP += sizeof (TYPE)))[-1])
#define	va_end(AP)							\
	(AP = (char *)NULL)

#endif

#endif
