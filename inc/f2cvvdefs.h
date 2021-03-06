/* f2cvvdefs.h

	Copyright (c) Kapteyn Astronomical Institute Groningen 1993, 2011
	All Rights Reserved.

#>            f2cvvdefs.dc3

Header:       f2cvvdefs.h

Purpose:      Contains the f2cvv definitions generated by program f2cvv.

File:         f2cvvdefs.h

Author:       K.G. Begeman

Use:          #include "f2cvvdefs.h"

Defines:      fchar (fortran character), a struct with a pointer to
              a character string (a) and a length (l).
              complex (fortran complex), a struct with a real (r) and
              an imaginary part (i).
              fint (fortran integer).
              fint8 (64-bit fortran integer).
              bool (fortran logical).
              tobool and toflog, macros to convert from fortran logical
              to c logical and vv.
              TRUE and FALSE, fortran .TRUE. and .FALSE.

Updates:      Jul  7, 1993: KGB, Document created.
              Mar 11, 2011: JPT, 64-bit integer support.

#<

*/

/* f2cvv definitions generated by program f2cvv */
#ifndef _F2CVVDEFS_H_
#define _F2CVVDEFS_H_

#ifdef  TRUE
#undef  TRUE
#endif
#ifdef  FALSE
#undef  FALSE
#endif

#include <stdint.h>
#if     defined(__g77__) | defined(__F2C__) | defined(__linux__) | defined(__FreeBSD__) | defined(__APPLE__)
#if     defined(__APPLE__) && !defined(__x86_64__)
typedef long fint;
typedef long bool;
#else
typedef int fint;
typedef int bool;
#endif
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
typedef int64_t fint8;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l ? 1 : 0 )
#define toflog(l)       ( l ? 1 : 0 )
#else
#if     defined(_AIX) | defined(__aix__)
typedef long fint;
typedef long bool;
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l & 0x00000001 ? 1 : 0 )
#define toflog(l)       ( l ? 1 : 0 )
#endif
#if     defined(alliant) | defined(__alliant__)
typedef long fint;
typedef long bool;
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l & 0x00000001 ? 1 : 0 )
#define toflog(l)       ( l ? 1 : 0 )
#endif
#if     defined(__alpha) | defined(__alpha__)
typedef int fint;
typedef int bool;
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l )
#define toflog(l)       ( l ? 1 : 0 )
#endif
#if     defined(convex) | defined(__convex__) | defined(__convexc__)
typedef long fint;
typedef long bool;
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l )
#define toflog(l)       ( l ? 1 : 0 )
#endif
#if     defined(__hpux) | defined(__hpux__)
typedef long fint;
typedef long bool;
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
#if     defined(__hp9000s300) | defined(__hp9000s300__)
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l )
#define toflog(l)       ( l ? 1 : 0 )
#elif   defined(__hp9000s700) | defined(__hp9000s700__) | defined(__hppa__)
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l )
#define toflog(l)       ( l ? 1 : 0 )
#elif   defined(__hp9000s800) | defined(__hp9000s800__)
#define TRUE            ( 0x01000000 )
#define FALSE           ( 0x00000000 )
#define tobool(l)       ( l & 0x01000000 ? 0x01000000 : 0 )
#define toflog(l)       ( l ? 0x01000000 : 0x00000000 )
#endif
#endif
#if     defined(mips) | defined(__mips__)
typedef long fint;
typedef long bool;
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l )
#define toflog(l)       ( l ? 1 : 0 )
#endif
#if     defined(__sgi) | defined(__sgi__)
typedef long fint;
typedef long bool;
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l )
#define toflog(l)       ( l ? 1 : 0 )
#endif
#if     defined(sony) | defined(__sony__)
typedef long fint;
typedef long bool;
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l )
#define toflog(l)       ( l ? 1 : 0 )
#endif
#if     defined(sun) | defined(__sun__)
typedef long fint;
typedef long bool;
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l )
#define toflog(l)       ( l ? 1 : 0 )
#endif
#if     defined(vms) | defined(__vms__)
typedef long fint;
typedef long bool;
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l )
#define toflog(l)       ( l ? 1 : 0 )
#endif
#endif

#endif
