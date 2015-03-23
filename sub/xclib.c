/* xclib.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            xclib.dc3

Document:     XCLIB

Purpose:      XCLIB is the extended C library. It contains some of the
              ANSI library functions which are not available on some
              operating systems.

File:         xclib.dc3

Author:       K.G. Begeman

Description:  On some operating systems the ANSI C library routines are
              not available or have a non standard name. Some of the
              missing routines are supplied however in XCLIB. This
              document describes which routines are available. This means
              that routines NOT mentioned in this document might NOT
              be used by level 1 and level 2 programmers. This also
              means that the standard ANSI C include files can not be
              used. Therefore the GIPSY programming group supplies its
              own version of the include files. They contain only routines
              which YOU as a programmer are allowed to use. These include
              files are:

              assert.h            math.h           stdlib.h
              ctype.h             signal.h         string.h
              errno.h             stdarg.h         time.h
              float.h             stddef.h
              limits.h            stdio.h

              This implicates that the include files provided by the
              operating system cannot be used. These include files are
              stored under UPDATE, and you must refer to them in your
              source with #include "stdio.h" instead of #include <stdio.h>.

Warning:      System dependent! At the moment implemented for ALLIANT,
              CONVEX, DEC ULTRIX, HP 9000, SUN and DEC VMS.

Updates:      Feb 13, 1990: KGB, Document created.
              May 14, 1990: JPT, Refereed.
              Jun  4, 1991: KGB, strftime implemented, mktime corrected.
              Jul 20, 1991: KGB, signal and raise implemented.
              Oct  6, 1994: KGB, bug in signal_x removed.
              Feb  9, 1999: JPT, workaround for Linux ecvt() bug.
              May  4, 2007: JPT, define stub when there is nothing to compile.

#<

How xclib is setup:

In the include files all macros are defined and all functions are
declared. If a function is not present in the libraries of the host
system or when a function does not operate according to the ANSI C
specifications, the include file defines the name of the function
to be the same name with '_x' appended prior to the declaration of
the function. The function with '_x' should then be present in xclib.
In xclib.c the same header files are included, so also here the
substitute name is defined. In xclib.c it is checked whether the
name of the function is defined, and if so, the substitute function
is compiled. This assures that no unnecessary code is compiled.

What we have not working yet:

<stdio.h>

- setvbuf is not present on DEC VMS systems, although it is sometimes
advertised. There is no way to get this to work, but who wants to use
unix i/o on vms anyway. When setvbuf is called on VMS, it will always
return a non-zero value, meaning an error has occurred.

- the ..scanf functions are present on all supported systems, but like
the ..printf functions they do not operate according to the ANSI C
specifications. Since I have no use for these functions (yet), someone
else may have the priviledge to write them.

<setjmp.h>

- nothing has been implemented yet.

<time.h>

On VMS gmtime returns NULL!.

By the way, standard ANSI C function definitions are taken from:
Kernighan and Ritchie, second edition of 'the C programming language'.

*/

#include	"signal.h"
#include	"stdarg.h"
#include	"stddef.h"
#include        "stdio.h"
#include	"ctype.h"
#include	"string.h"
#include	"math.h"
#include	"stdlib.h"
#include	"limits.h"
#include	"float.h"
#include	"errno.h"
#include	"time.h"

/*
 * Here we test on which operating system the compiler is running.
 * If the operation system is not known to us, we assume that it
 * supports the ANSI C defined functions and nothing is compiled.
 * This is checked with __xclib__.
 */

#if	defined(__xclib__)				/* strange */
#undef	__xclib__
#endif
#if	defined(__aix__)				/* AIX */
#define	__xclib__
#elif	defined(__alliant__)				/* ALLIANT */
#define	__xclib__
#elif	defined(__alpha__)				/* ALPHA */
#define	__xclib__
#elif	defined(__convex__)				/* CONVEX */
#define	__xclib__
#elif	defined(__cray__)				/* CRAY */
#define	__xclib__
#elif	defined(__freebsd__)				/* FREEBSD */
#define	__xclib__
#elif	defined(__hpux__)				/* HP 9000 */
#define	__xclib__
#elif	defined(__linux__)				/* LINUX */
#define	__xclib__
#elif	defined(__mips__)				/* DEC ULTRIX */
#define	__xclib__
#elif	defined(__sgi__)				/* SILICON GRAPHICS */
#define	__xclib__
#elif	defined(__sun__)				/* SUN */
#define	__xclib__
#elif	defined(__vms__)				/* DEC VMS */
#define	__xclib__
#endif

/*
 * Here we check whether we have anything to compile.
 */

#if	defined(__xclib__)				/* COMPILE */

#if     defined(__vms__)
#include	<descrip.h>
#include	<lnmdef.h>
#include	<ssdef.h>
#endif

#if	defined(__vms__)
globaldef	char	_X_CTYPE[] = {  /* make it a global value */
#else
extern		char	_X_CTYPE[];     /* Character type array */

char	_X_CTYPE[] = {                  /* Character type array */
#endif
     0,                                 /* EOF */
    32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 33, 33, 33, 33, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
     1, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
     2,  2,  2,  2,  2,  2,  2,  2,  2,  2, 64, 64, 64, 64, 64, 64,
    64, 20, 20, 20, 20, 20, 20,  4,  4,  4,  4,  4,  4,  4,  4,  4,
     4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4, 64, 64, 64, 64, 64,
    64, 24, 24, 24, 24, 24, 24,  8,  8,  8,  8,  8,  8,  8,  8,  8,
     8,  8,  8,  8,  8,  8,  8,  8,  8,  8,  8, 64, 64, 64, 64, 32,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0
};

/*
 * Here come some of the missing stdio.h routines.
 */

/*
 * For the ..printf functions we need some procedures for
 * converting binary numbers to text strings. The functions ecvt, fcvt
 * and gcvt are supplied by the UNIX environment, the functions scvt,
 * ucvt and xcvt are made available here. The function doprint handles
 * the ANSI defined formatted output.
 */

#define	MAX_CVT	100				/* length of static string */

#if	defined(__decstation__) || defined(__freebsd__)

static	char	*cvt( double arg, int ndig, int *dec, int *sign, int eflg )
/*
 * This one dows all the work.
 */
{
   register int		r2;
   double		fi, fj;
   register char	*p, *p1;
   static char		buf[MAX_CVT];
   double 		modf( );

   if (ndig < 0) ndig = 0;
   if (ndig >= (MAX_CVT-1)) ndig = MAX_CVT-2;
   r2 = 0;
   *sign = 0;
   p = &buf[0];
   if (arg < 0) {
      *sign = 1;
      arg = -arg;
   }
   arg = modf( arg, &fi );
   p1 = &buf[MAX_CVT];
   /*
    * Do integer part
    */
   if (fi != 0) {
      p1 = &buf[MAX_CVT];
      while (fi != 0) {
         fj = modf( fi/10, &fi );
         *--p1 = (int)((fj+.03)*10) + '0';
         r2++;
      }
      while (p1 < &buf[MAX_CVT]) *p++ = *p1++;
   } else if (arg > 0) {
      while ((fj = arg*10) < 1) {
         arg = fj;
         r2--;
      }
   }
   p1 = &buf[ndig];
   if (eflg == 0) p1 += r2;
   *dec = r2;
   if (p1 < &buf[0]) {
      buf[0] = '\0';
      return( buf );
   }
   while (p <= p1 && p < &buf[MAX_CVT]) {
      arg *= 10;
      arg = modf( arg, &fj );
      *p++ = (int)fj + '0';
   }
   if (p1 >= &buf[MAX_CVT]) {
      buf[MAX_CVT-1] = '\0';
      return( buf );
   }
   p = p1;
   *p1 += 5;
   while (*p1 > '9') {
      *p1 = '0';
      if (p1 > buf) {
         ++*--p1;
      } else {
         *p1 = '1';
         (*dec)++;
         if (eflg == 0) {
            if (p > buf) *p = '0';
            p++;
         }
      }
   }
   *p = '\0';
   return( buf );
}

static   char   *ecvt( double arg, int ndig, int *dec, int *sign )
/*
 * ecvt converts to decimal. The number of digits is specified by ndig,
 * dec is set to the position of the decimal point, sign is set to 0 for
 * positive, 1 for negative.
 */
{
   return( cvt( arg, ndig, dec, sign, 1) );
}

static   char   *fcvt( double arg, int ndig, int *dec, int *sign )
/*
 * fcvt works much like ecvt, except that ndig specifies the number of
 * digits after the decimal point.
 */
{
   return( cvt( arg, ndig, dec, sign, 0 ) );
}

static	char	*gcvt( double number, int ndigit, char *buf )
/*
 * gcvt does the floating output conversion to minimal length string.
 */
{
   int			sign, dec;
   register char	*p1, *p2;
   register int		i;

   p1 = ecvt( number+0.0, ndigit, &dec, &sign );   /* avoid -0.0 for Linux */
   p2 = buf;
   if (sign) *p2++ = '-';
   for ( i = ndigit - 1; i > 0 && p1[i] == '0'; i-- ) ndigit--;
   if (dec >= 0 && (dec - ndigit) > 4 || dec < 0 && dec < -3) {	/* E-style */
      dec--;
      *p2++ = *p1++;
      *p2++ = '.';
      for ( i = 1; i < ndigit; i++ ) *p2++ = *p1++;
      *p2++ = 'e';
      if (dec < 0) {
         dec = -dec;
         *p2++ = '-';
      } else {
         *p2++ = '+';
      }
      *p2++ = dec/10 + '0';
      *p2++ = dec%10 + '0';
   } else {							/* F-style */
      if (dec <= 0) {
         if (*p1 != '0') *p2++ = '.';
         while (dec < 0) {
            dec++;
            *p2++ = '0';
         }
      }
      for ( i = 1; i <= ndigit; i++ ) {
         *p2++ = *p1++;
         if (i == dec) *p2++ = '.';
      }
      if (ndigit < dec) {
         while (ndigit++ < dec) *p2++ = '0';
         *p2++ = '.';
      }
   }
   if (p2[-1] == '.') p2--;
   *p2 = '\0';
   return( buf );
}
#else

extern	char	*ecvt( double value, int ndig, int *dec, int *sign );
extern	char	*fcvt( double value, int ndig, int *dec, int *sign );
extern	char	*gcvt( double value, int ndec, char *buf );

#endif

static	char	scvt_b[MAX_CVT];
static	char	scvt_n[] = "0123456789abcdefghijklmnopqrstuvwxyz";

static	char	*scvt( long value, int base, int *dec, int *sign )
/*
 * scvt does a conversion of a signed long to a character string, of
 * which the pointer is returned. The base can be from 2 - 36. In dec scvt
 * returns the number of characters in the converted string, in sign
 * whether value carries a sign.
 */
{
   double	dval;
   int   	ndig;

   if (value < 0) {				/* determine sign of value */
      *sign = 1;
      dval = -1.0 * (double) value;
   } else {
      *sign = 0;
      dval = (double) value;
   }
   if (value) {					/* determine number of digits */
      ndig = (int) (log10( dval ) / log10( (double) base )) + 1;
   } else {
      ndig = 1;
   }
   *dec = ndig;
   scvt_b[ndig] = 0;
   while (ndig--) {				/* do the conversion */
      long q;
      long r;

      q = value / base;				/* integer division */
      r = value - q * base;			/* get most significant digit */
      value = q;
      if (r < 0) r *= -1;
      scvt_b[ndig] = scvt_n[r];
   }
   return( scvt_b );				/* return pointer */
}

static	char	ucvt_b[MAX_CVT];
static	char	ucvt_n[] = "0123456789abcdefghijklmnopqrstuvwxyz";

static	char	*ucvt( unsigned long value, int base, int *dec )
/*
 * ucvt does a conversion of an unsigned long to a character string, of
 * which the pointer is returned. The base can from 2 - 36. In dec scvt
 * returns the number of characters in the converted string.
 */
{
   int   ndig;

   if (value) {					/* determine number of digits */
      ndig = (int) (log10( (double) value ) / log10( (double) base )) + 1;
   } else {
      ndig = 1;
   }
   *dec = ndig;
   ucvt_b[ndig] = 0;
   while (ndig--) {				/* do the conversion */
      unsigned long q;
      unsigned long r;

      q = value / base;				/* integer division */
      r = value - q * base;			/* get most significant digit */
      value = q;
      ucvt_b[ndig] = ucvt_n[r];
   }
   return( ucvt_b );				/* return pointer */
}

static	char	xcvt_b[MAX_CVT];

static	char	*xcvt( double val )
/*
 * xcvt check whether the argument is a special floating point
 * number. If so, is puts the associated text in static memory and
 * returns a pointer to that memeory; if not, it puts a zero in
 * static memory.
 */
{
   union { int l[2]; double d; } u;

   u.d = val;
#if	OS_FLOATING_TYPE == 0 | OS_FLOATING_TYPE == 5
   if ((u.l[0] & 0x7ff00000) == 0x7ff00000) {
      if (u.l[0] & 0x000fffff || u.l[1] & 0xffffffff) {
         strcpy( xcvt_b, "NaN" );
      } else if (u.l[0] & 0x80000000) {
         strcpy( xcvt_b, "-Inf" );
      } else {
         strcpy( xcvt_b, "+Inf" );
      }
   } else {
      xcvt_b[0] = 0;
   }
#elif	OS_FLOATING_TYPE == 1 | OS_FLOATING_TYPE == 6
   if ((u.l[1] & 0x7ff00000) == 0x7ff00000) {
      if (u.l[1] & 0x000fffff || u.l[0] & 0xffffffff) {
         strcpy( xcvt_b, "NaN" );
      } else if (u.l[1] & 0x80000000) {
         strcpy( xcvt_b, "-Inf" );
      } else {
         strcpy( xcvt_b, "+Inf" );
      }
   } else {
      xcvt_b[0] = 0;
   }
#elif	OS_FLOATING_TYPE == 2
   xcvt_b[0] = 0;
#elif	OS_FLOATING_TYPE == 3
   if (u.l[0] == 0x70000000 && u.l[1] == 0x00000000) {
      strcpy( xcvt_b, "-zero" );
   } else {
      xcvt_b[0] = 0;
   }
#elif	OS_FLOATING_TYPE == 4
   if (u.l[0] == 0x70000000 && u.l[1] == 0x00000000) {
      strcpy( xcvt_b, "-zero" );
   } else {
      xcvt_b[0] = 0;
   }
#else
   xcvt_b[0] = 0;
#endif
   return( xcvt_b );
}

static	char	*print_p = NULL;
static	int	print_n = 0;

#define SIZE_INIT 1024
#define	SHIP(out)	{ \
                           if (count == print_n) { \
                              print_n += SIZE_INIT; \
                              print_p = realloc( print_p, sizeof(char) * print_n ); \
                           } \
                           print_p[count++] = (out); \
                        }
#define	RESET		{ \
                           if (print_n != SIZE_INIT) { \
                              print_n = SIZE_INIT; \
                              print_p = realloc( print_p, sizeof(char) * print_n ); \
                           } \
                        }

static	char	*doprint( const char *format, va_list ap )
/*
 * doprint is an local procedure which takes care of the ANSI C
 * definitions of output formatting. It returns a pointer to an area
 * of dynamically allocated memory, where the formatted text is stored.
 * doprint does not report errors. They can only appear when writting
 * to a file.
 */
{
   char *p;
   int   count = 0;
   int   flags;
   int   prec;
   int   size;
   int   width;

   for (p = (char *) format; *p; p++) {
      char *savep;
      int   n;
      int   nflags;

      if (*p != '%') { SHIP( *p ) continue; }
      flags = 0;				/* flags */
      savep = p++;				/* save this location */
      /*
       * First we look for flags.
       */
      for (n = 0, nflags = strspn( p, "-+ 0#" ); n < nflags; n++) {
         switch(*p++) {
            case '-': flags |= 1; break;	/* left adjust */
            case '+': flags |= 2; break;	/* print sign */
            case ' ': flags |= 4; break;	/* blank padding */
            case '0': flags |= 8; break;	/* zero padding */
            case '#': flags |= 16; break;	/* special format */
            default : flags = 0; break;		/* no flag present */
         }
      }
      /*
       * Next we search for the width.
       */
      width = 0;				/* minimum field width */
      if (*p == '*') {
         width = va_arg( ap, int ); p++;	/* get it from argument list */
      } else {
         while (isdigit( *p )) width = 10 * width + *p++ - '0';	/* decode it */
      }
      /*
       * Third we search for the precision.
       */
      prec = -1;				/* precision */
      if (*p == '.') {
         prec = 0;
         if (*++p == '*') {
            prec = va_arg( ap, int ); p++;	/* get it from argument list */
         } else {
            while (isdigit( *p )) prec = 10 * prec + *p++ - '0'; /* decode it */
         }
      }
      /*
       * Fourth we search for the length modifier.
       */
      switch(*p) {				/* length modifier */
         case 'h': size = 1; p++; break;	/* short int */
         case 'l': size = 2; p++; break;	/* long int */
         case 'L': size = 3; p++; break;	/* long double */
         default : size = 0; break;		/* no modifier present */
      }
      /*
       * Finally we deal with the conversion character.
       */
      switch(*p) {				/* conversion characters */
         case 'd':				/* signed decimal */
         case 'i': {				/* signed decimal */
            char *cvt;
            int   base = 10;
            int   dec;
            int   numw;
            int   sign;
            long  lval;

            /*
             * Get argument from argument list.
             */
            switch(size) {			/* size of argument */
               case 0: {			/* int */
                  int ival = va_arg( ap, int );

                  lval = (long) ival;
                  break;
               }
               case 1: {			/* short */
                  short sval = va_arg( ap, int );

                  lval = (long) sval;
                  break;
               }
               case 2: {			/* long */
                  lval = va_arg( ap, long );
                  break;
               }
               default: {			/* just in case */
                  lval = 0;
                  break;
               }
            }
            cvt = scvt( lval, base, &dec, &sign );	/* binary to text */
            if (prec == 0 && lval == 0) dec = 0;
            if (prec == -1) prec = 1;		/* default precision */
            if (dec < prec) numw = prec; else numw = dec;
            if (sign || (flags & 6)) numw += 1;
            if (width < numw) width = numw;	/* minimum width */
            if (!(flags & 1) && !(flags & 8)) {
               while (width > numw) { width--; SHIP( ' ' ) }
            }
            if (sign) {
               width--; numw--; SHIP( '-' )
            } else if (flags & 2) {
               width--; numw--; SHIP( '+' )
            } else if (flags & 4) {
               width--; numw--; SHIP( ' ' )
            }
            if (!(flags & 1) && flags & 8) {
               while (width > numw) { width--; SHIP( '0' ) }
            }
            while (prec > dec) { width--; prec--; SHIP( '0' ) }
            while (dec) { width--; dec--; SHIP( *cvt++ ) }
            if (flags & 1) {
               while (width) { width--; SHIP( ' ' ) }
            }
            break;
         }
         case 'o':				/* unsigned octal int */
         case 'p':				/* pointer */
         case 'u':				/* unsigned decimal int */
         case 'x':				/* unsigned hexadecimal int */
         case 'X': {				/* unsigned hexadecimal int */
            char          *cvt;
            int            base;
            int            dec;
            int            numw;
            unsigned long  lval;

            switch(*p) {
               case 'o': base = 8; break;
               case 'p': base = 16; size = -1; break;
               case 'x':
               case 'X': base = 16; break;
               default : base = 10; break;
            }
            switch(size) {
               case -1: {			/* pointer to void */
                  void *vval = va_arg( ap, void * );

                  lval = (unsigned long) vval;
                  break;
               }
               case 0: {			/* unsigned int */
                  unsigned int ival = va_arg( ap, unsigned int );

                  lval = (unsigned long) ival;
                  break;
               }
               case 1: {			/* unsigned short */
                  unsigned short sval = va_arg( ap, int );

                  lval = (unsigned long) sval;
                  break;
               }
               case 2: {			/* unsigned long */
                  lval = va_arg( ap, unsigned long );
                  break;
               }
               default: {
                  lval = 0;
                  break;
               }
            }
            cvt = ucvt( lval, base, &dec );	/* binary to text */
            if (*p == 'x') {
               for (n = 0; cvt[n]; n++) cvt[n] = tolower( cvt[n] );
            } else if (*p == 'X') {
               for (n = 0; cvt[n]; n++) cvt[n] = toupper( cvt[n] );
            }
            if (prec == 0 && lval == 0) {
               dec = 0;
            } else if (prec == 0 || prec == -1) {
               prec = 1;
            }
            if (dec < prec) numw = prec; else numw = dec;
            if (flags & 16) {			/* alternate form */
               if (*p == 'o') numw += 1;
               if (*p == 'x' || *p == 'X') numw += 2;
            }
            if (!(flags & 1) && !(flags & 8)) {
               while (width > numw) { width--; SHIP( ' ' ) }
            }
            if (flags & 16) {		/* alternate form */
               if (*p == 'o') {
                  width--; numw--; SHIP( '0' )
               }
               if (*p == 'x') {
                  width--; numw--; SHIP( '0' )
                  width--; numw--; SHIP( 'x' )
               }
               if (*p == 'X') {
                  width--; numw--; SHIP( '0' )
                  width--; numw--; SHIP( 'X' )
               }
            }
            if (!(flags & 1) && flags & 8) {
               while (width > numw) { width--; SHIP( '0' ) }
            }
            while (prec > dec) { width--; prec--; SHIP( '0' ) }
            while (dec) { width--; dec--; SHIP( *cvt++ ) }
            if (flags & 1) {
               while (width) { width--; SHIP( ' ' ) }
            }
            break;
         }
         case '%':				/* % */
         case 'c': {				/* single character */
            int cval;
            int dec = 1;

            if (*p == '%') cval = '%'; else cval = va_arg( ap, int );
            if (!width) width = dec;
            if (flags & 1) {			/* left justified */
               int n;

               SHIP( cval )
               for (n = 1; n < width; n++) SHIP( ' ' )
            } else {				/* right justified */
               char pad;
               int  n;

               if (flags & 8) pad = '0'; else pad = ' ';
               for (n = width; n > dec; n--) SHIP( pad )
               SHIP ( cval )
            }
            break;
         }
         case 's': {				/* string pointer */
            char *sval;
            int   dec;

            sval = va_arg( ap, char * );
            if (prec == -1) {
               dec = strlen( sval );
            } else {
               int	n = 0;

               while (n < prec && sval[n]) n++;
               dec = n;
            }
            if (!width) width = dec;
            if (flags & 1) {			/* left justified */
               int n;

               for (n = 0; n < dec; n++) SHIP( sval[n] )
               for (; n < width; n++) SHIP( ' ' )
            } else {				/* right justified */
               char pad;
               int  n;

               if (flags & 8) pad = '0'; else pad = ' ';
               for (n = width; n > dec; n--) SHIP( pad )
               for (n = 0; n < dec; n++) SHIP( sval[n] )
            }
            break;
         }
         case 'f': {				/* signed double */
            char   *cvt;
            double  fval;
            int     dec;
            int     numw;
            int     sign;

            fval = va_arg( ap, double );
            if (prec == -1) prec = 6;		/* default */
            cvt = xcvt( fval );			/* binary to text */
            if (*cvt) {				/* special float */
               numw = strlen( cvt );
               if (width < numw) width = numw;
               if (!(flags & 1)) {
                  while (width < numw) { width--; SHIP( ' ') }
               }
               while (*cvt) { width--; SHIP( *cvt++ ) }
               if (flags & 1) {
                  while (width) { width--; SHIP( ' ' ) }
               }
               break;
            }
            cvt = fcvt( fval, prec, &dec, &sign );	/* binary to text */
            if (dec > 0) numw = dec; else numw = 1;
            if (prec || (flags & 16)) numw += prec + 1;
            if (sign || (flags & 6)) numw += 1;
            if (width < numw) width = numw;
            if (!(flags & 1) && !(flags & 8)) {
               while (width > numw) { width--; SHIP( ' ' ) }
            }
            if (sign) {
               width--; numw--; SHIP( '-' )
            } else if (flags & 2) {
               width--; numw--; SHIP( '+' )
            } else if (flags & 4) {
               width--; numw--; SHIP( ' ' )
            }
            if (!(flags & 1) && flags & 8) {
               while (width > numw) { width--; SHIP( '0' ) }
            }
            if (dec <= 0) {
               width--; SHIP( '0' )
            } else {
               while (dec) { dec--; width--; SHIP( *cvt++ ) }
            }
            if (prec || (flags & 16)) { width--; SHIP( '.' ) }
            while (prec--) {
               if (++dec > 0) SHIP( *cvt++ ) else SHIP( '0' )
               width--;
            }
            if (flags & 1) {
               while (width--) SHIP( ' ' )
            }
            break;
         }
         case 'e':
         case 'E': {				/* signed double */
            char   *cvt;
            char   *exp;
            double  fval;
            int     dec;
            int     edec;
            int     esign;
            int     numw;
            int     sign;
            long    expn;

            fval = va_arg( ap, double );
            if (prec == -1) prec = 6;		/* default */
            cvt = xcvt( fval );			/* binary to text */
            if (*cvt) {				/* special float */
               numw = strlen( cvt );
               if (width < numw) width = numw;
               if (!(flags & 1)) {
                  while (width < numw) { width--; SHIP( ' ') }
               }
               while (*cvt) { width--; SHIP( *cvt++ ) }
               if (flags & 1) {
                  while (width) { width--; SHIP( ' ' ) }
               }
               break;
            }
            cvt = ecvt( fval+0.0, prec + 1, &dec, &sign ); /* binary to text */
                                          /* add 0.0 to avoid -0.0 for Linux */
            expn = (long) dec - 1;
            exp = scvt( expn, 10, &edec, &esign );
            if (edec == 1) {
               numw = edec + 4;
            } else {
               numw = edec + 3;
            }
            if (prec || (flags & 16)) numw += prec + 1;
            if (sign || (flags & 6)) numw += 1;
            if (width < numw) width = numw;
            if (!(flags & 1) && !(flags & 8)) {
               while (width > numw) { width--; SHIP( ' ' ) }
            }
            if (sign) {
               width--; numw--; SHIP( '-' )
            } else if (flags & 2) {
               width--; numw--; SHIP( '+' )
            } else if (flags & 4) {
               width--; numw--; SHIP( ' ' )
            }
            if (!(flags & 1) && flags & 8) {
               while (width > numw) { width--; SHIP( '0' ) }
            }
            width--; SHIP( *cvt++ )
            if (prec || (flags & 16)) {
               width--; SHIP( '.' )
            }
            while (prec) { width--; prec--; SHIP( *cvt++ ) }
            if (*p == 'e') {
               width--; SHIP( 'e' )
            } else {
               width--; SHIP( 'E' )
            }
            if (esign) {
               width--; SHIP( '-')
            } else {
               width--; SHIP( '+' )
            }
            if (edec == 1) { width--; SHIP( '0' ) }
            while (edec) { width--; edec--; SHIP( *exp++ ) }
            if (flags & 1) {
               while (width) { width--; SHIP( ' ') }
            }
            break;
         }
         case 'g':
         case 'G': {				/* signed double */
            char   *cvt;
            char    gcvt_b[MAX_CVT];
            double  fval;
            int     numw;
            int     sign;

            fval = va_arg( ap, double );
            if (prec == -1) prec = 6;		/* default */
            cvt = xcvt( fval );			/* binary to text */
            if (*cvt) {				/* special float */
               numw = strlen( cvt );
               if (width < numw) width = numw;
               if (!(flags & 1)) {
                  while (width < numw) { width--; SHIP( ' ') }
               }
               while (*cvt) { width--; SHIP( *cvt++ ) }
               if (flags & 1) {
                  while (width) { width--; SHIP( ' ' ) }
               }
               break;
            }
            cvt = gcvt( fval, prec, gcvt_b );	/* binary to text */
            if (*cvt == '-') sign = 1; else sign = 0;
            {
               int n, ndec, ndig, nnz;

               for (n = 0, ndig = 0, ndec = 0, nnz = 0; gcvt_b[n]; n++) {
                  if (isdigit( gcvt_b[n] )) {
                     if ((gcvt_b[n] != '0') || nnz) { ndig++; nnz = 1; }
                  } else if (gcvt_b[n] == '.') {
                     ndec = 1;
                  } else if (toupper( gcvt_b[n] ) == 'E') {
                     if (*p == 'g') {
                        gcvt_b[n] = 'e';
                     } else {
                        gcvt_b[n] = 'E';
                     }
                     break;
                  }
               }
               if (flags & 16) {
                  int i;
                  int nins = 0;
                  int nmov = strlen( gcvt_b ) + 1 - n;

                  if (!ndec) nins = 1;
                  if (prec > ndig) nins += (prec - ndig);
                  for (i = strlen( gcvt_b ); nmov--; i--) {
                     gcvt_b[i+nins] = gcvt_b[i];
                  }
                  if (!ndec) gcvt_b[n++] = '.';
                  while (ndig++ < prec) gcvt_b[n++] = '0';
               }
            }
            numw = strlen( cvt );
            if (!sign && (flags & 6)) numw += 1;
            if (width < numw) width = numw;
            if (!(flags & 1) && !(flags & 8)) {
               while (width > numw) { width--; SHIP( ' ' ) }
            }
            if (sign) {
               width--; numw--; SHIP( *cvt++ )
            } else if (flags & 2) {
               width--; numw--; SHIP( '+' )
            } else if (flags & 4) {
               width--; numw--; SHIP( ' ' )
            }
            if (!(flags & 1) && flags & 8) {
               while (width > numw) { width--; SHIP( '0' ) }
            }
            while (*cvt) { width--; SHIP( *cvt++ ) }
            if (flags & 1) {
               while (width--) SHIP( ' ' )
            }
            break;
         }
         case 'n': {				/* return count */
            int *ival;

            ival = va_arg( ap, int * );
            *ival = count;
            break;
         }
         default: {				/* ship it all out */
            while (savep != p) SHIP( *savep++ )
            SHIP( *p )
            break;
         }
      }
   }
   SHIP( 0 )					/* ship trailing zero */
   return( print_p );
}

#undef	SHIP					/* undefine */

#if	defined(printf)				/* printf from xclib */

int	printf_x( const char *format, ... )
/*
 * printf converts and writes output to stdout under the control of
 * format. The return value is the number of characters written, or
 * negative if an error occurred.
 */
{
   char    *p;
   int      n;
   int      r;
   va_list  ap;

   va_start( ap, format );
   p = doprint( format, ap );
   va_end( ap );
   n = r = strlen( p );
   while (n && EOF != fputc( *p++, stdout ) ) n--;
   RESET					/* reset doprint buffer */
   if (n) return( -1 ); else return( r );
}
#endif						/* printf */

#if	defined(vprintf)			/* vprintf from xclib */

int	vprintf( const char *format, va_list ap )
/*
 * vprintf converts and writes output to stdout under the control of
 * format. The return value is the number of characters written, or
 * negative if an error occurred.
 */
{
   char    *p;
   int      n;
   int      r;

   p = doprint( format, ap );
   n = r = strlen( p );
   while (n && EOF != fputc( *p++, stdout ) ) n--;
   RESET					/* reset doprint buffer */
   if (n) return( -1 ); else return( r );
}
#endif						/* vprintf */

#if	defined(fprintf)			/* fprintf from xclib */

int	fprintf_x( FILE *stream, const char *format, ... )
/*
 * fprintf converts and writes output to stream under the control of
 * format. The return value is the number of characters written, or
 * negative if an error occurred.
 */
{
   char    *p;
   int      n;
   int      r;
   va_list  ap;

   va_start( ap, format );
   p = doprint( format, ap );
   va_end( ap );
   n = r = strlen( p );
   while (n && EOF != fputc( *p++, stream ) ) n--;
   RESET					/* reset doprint buffer */
   if (n) return( -1 ); else return( r );
}
#endif						/* fprintf */

#if	defined(vfprintf)			/* vfprintf from xclib */

int	vfprintf_x( FILE *stream, const char *format, va_list ap )
/*
 * vfprintf converts and writes output to stream under the control of
 * format. The return value is the number of characters written, or
 * negative if an error occurred.
 */
{
   char    *p;
   int      n;
   int      r;

   p = doprint( format, ap );
   n = r = strlen( p );
   while (n && EOF != fputc( *p++, stream ) ) n--;
   RESET					/* reset doprint buffer */
   if (n) return( -1 ); else return( r );
}
#endif						/* vfprintf */

#if	defined(sprintf)			/* sprintf from xclib */

int	sprintf_x( char *s, const char *format, ... )
/*
 * sprintf converts and writes output to s under the control of
 * format. The return value is the number of characters written, or
 * negative if an error occurred.
 */
{
   char    *p;
   va_list  ap;

   va_start( ap, format );
   p = doprint( format, ap );
   va_end( ap );
   strcpy( s, p );
   RESET					/* reset doprint buffer */
   return( strlen( p ) );
}
#endif						/* sprintf */

#if	defined(vsprintf)			/* vsprintf from xclib */

int	vsprintf_x( char *s, const char *format, va_list ap )
/*
 * vsprintf converts and writes output to s under the control of
 * format. The return value is the number of characters written, or
 * negative if an error occurred.
 */
{
   char    *p;

   p = doprint( format, ap );
   strcpy( s, p );
   RESET					/* reset doprint buffer */
   return( strlen( p ) );
}
#endif						/* vsprintf */

#undef	RESET					/* undefine */

#if	defined(remove)				/* remove from xclib */

int	remove_x( const char *filename )
/*
 * remove removes the named file, so that a subsequent attempt to open it
 * will fail. It returns EOF if any errors occurred, and zero otherwise.
 */
{
#if	defined(__alliant__) | defined(__convex__) | defined(__mips__) | defined(__sun__)
   int unlink( );

   return( unlink( filename ) );
#elif	defined(__vms__)
   int delete( );

   return( delete( filename ) );
#else
   NOT IMPLEMENTED
#endif
}
#endif						/* remove */

#if	defined(rename)				/* rename from xclib */

int	rename_x( const char *oldname, const char *newname )
/*
 * rename changes the name of a file; it returns non-zero if the attempt
 * fails.
 */
{
#if	defined(__vms__)				/* DEC VMS */
   struct dsc$descriptor old, new;
   int                   lib$rename_file( );
   int                   lo = 0, ln = 0;

   while (oldname[lo]) lo++;
   while (newname[ln]) ln++;
   old.dsc$a_pointer = oldname;
   old.dsc$w_length  = lo;
   old.dsc$b_class   = DSC$K_CLASS_S;
   old.dsc$b_dtype   = DSC$K_DTYPE_T;
   new.dsc$a_pointer = newname;
   new.dsc$w_length  = ln;
   new.dsc$b_class   = DSC$K_CLASS_S;
   new.dsc$b_dtype   = DSC$K_DTYPE_T;
   if (lib$rename_file( &old, &new ) == SS$_NORMAL) {
      return( 0 );
   } else {
      return( 1 );
   }
#else
    NOT IMPLEMENTED
#endif
}
#endif						/* rename */

#if	defined(setvbuf)			/* setvbuf from xclib */

int	setvbuf_x( FILE *stream, char *buf, int mode, size_t size )
/*
 * setvbuf controls buffering for the stream; it must be called before
 * reading or writing. A mode of _IOFBF causes full buffering, _IOLBF line
 * buffering of text files, and _IONBF no buffering. If buf is not NULL, it
 * will be used as the buffer; otherwise a buffer will be allocated. size
 * determines the buffer size. setvbuf returns non-zero for any error.
 */
{
#if	defined(__vms__)				/* DEC VMS */
   return( 1 );
#elif	defined(__alliant__) | defined(__convex__)	/* not available */
   char *b = buf;
   void  setbuffer( );
   int   setlinebuf( );
   void  free( );

   if (b == NULL && mode != _IONBF) {
      b = malloc( size );
      if (b == NULL) return( 1 );
   }
   switch(mode) {
      case _IOFBF: {				/* full buffering */
         setbuffer( stream, b, size );
         break;
      }
      case _IOLBF: {				/* line buffering */
         setlinebuf( stream );
         free( stream->_ptr );
         stream->_ptr = b;
         stream->_base = b;
         stream->_bufsiz = size;
         break;
      }
      case _IONBF: {				/* no buffering */
         setbuf( stream, NULL );
         break;
      }
      default: return( 2 );
   }
   return( 0 );
#else
   NOT IMPLEMENTED
#endif
}
#endif						/* setvbuf */

#if	defined(tmpnam)				/* tmpnam from xclib */

static char         tmpnam_b[L_tmpnam+1];
static unsigned int tmpnam_c = 0;

char	*tmpnam_x( char *s )
/*
 * tmpnam(NULL) creates a string that is not the name of an existing file,
 * and returns a pointer to an internal static array. tmpnam(s) stores the
 * string in s as well as returning it as the function value; s must have
 * room for at least L_tmpnam characters. tmpnam generates a different name
 * each time it is called; at most TMP_MAX different names are guaranteed
 * during execution of the program. Note that tmpnam creates a name, not a
 * file.
 */
{
#if	defined(__alliant__) | defined(__convex__)
   FILE         *tmp;
   char         *b = s;
   unsigned int  c = 0;

   if (b == NULL) b = tmpnam_b;
   (void) sprintf( b, "TMP%5.5u.TMP", tmpnam_c++ );
   if (tmpnam_c == TMP_MAX) tmpnam_c = 0;
   tmp = fopen( b, "r" );
   while (tmp != NULL && ++c < TMP_MAX) {
      fclose( tmp );
      sprintf( b, "TMP%5.5u.TMP", tmpnam_c++ );
      if (tmpnam_c == TMP_MAX) tmpnam_c = 0;
      tmp = fopen( b, "r" );
   }
   if (tmp != NULL) {
      fclose( tmp );
      b = NULL;
   }
   return( b );
#else
   NOT IMPLEMENTED
#endif
}
#endif						/* tmpnam */

#if	defined(tmpfile)			/* tmpfile from xclib */

FILE	*tmpfile_x( void )
/*
 * tmpfile creates a temporary file of mode "wb+" that will be automatically
 * removed when closed or when the program terminates normally. tmpfile
 * returns a stream, or NULL if it could not create the file.
 */
{
#if	defined(__alliant__) | defined(__convex__)
   char        *mktemp();
   static char  tname[13];
   static char *ptr;

   strcpy( tname, "/tmp/TXXXXXX" );
   ptr = mktemp( tname );
   if (ptr == NULL) return(NULL); else return( fopen( tname, "w+b" ) );
#else
   NOT IMPLEMENTED
#endif
}
#endif						/* tmpfile */

#if	defined(fgetpos)			/* fgetpos from xclib */

int	fgetpos_x( FILE *stream, fpos_t *ptr )
/*
 * fgetpos records the current position in stream in *ptr, for subsequent
 * use by fsetpos. The type fpos_t is suitable for recording such values.
 * fgetpos returns non-zero on error.
 */
{
#if	defined(__alliant__) | defined(__convex__) | defined(__mips__) | defined(__sun__)
   int  ret = 0;
   long pos;

   if ((pos = ftell( stream )) == -1L) {
      ret = -1;
   } else {
      *ptr = (fpos_t) pos;
   }
   return( ret );
#else
   NOT IMPLEMENTED
#endif
}						/* fgetpos */
#endif

#if	defined(fsetpos)			/* fsetpos from xclib */

int	fsetpos_x( FILE *stream, const fpos_t *ptr )
/*
 * fsetpos positions stream at the position recorded by fgetpos in *ptr.
 * fsetpos returns non-zero on error.
 */
{
#if	defined(__alliant__) | defined(__convex__) | defined(__mips__) | defined(__sun__)
   long pos;

   pos = (long) *ptr;
   return( fseek( stream, pos, SEEK_SET ) );
#else
   NOT IMPLEMENTED
#endif
}
#endif						/* fsetpos */

#if	defined(fopen)				/* fopen from xclib */

static	char	fopen_mode[20];			/* buffer for modified mode */

#undef	fopen

FILE	*fopen_x( const char *filename, const char *mode )
/*
 * fopen opens the names file, and returns a stream, or NULL if the attempt
 * fails.
 */
{
   FILE	*fopen( );				/* define it */
   int i = 0, j = 0;				/* counters */

   do {						/* loop */
      if (mode[i] != 'b') fopen_mode[j++] = mode[i];	/* strip 'b' */
   } while (mode[i++]);				/* until end of string */
   return( fopen( filename, fopen_mode ) );	/* return to caller */
}

#define	fopen	fopen_x				/* fopen from xclib */

#endif						/* fopen */
/*
 * Here come the ctype.h routines.
 */

#if	defined(tolower)			/* tolower from xclib */

int	tolower_x( int ch )
/*
 * tolower is a function that converts an integer ch (in the range EOF
 * to 255) to its lowercase value (if it was uppercase): all others are
 * left unchanged.
 */
{
   if ((ch < 'A') || (ch > 'Z')) return( ch ); else return( ch - 'A' + 'a');
}
#endif						/* tolower */

#if	defined(toupper)			/* toupper from xclib */

int	toupper_x( int ch )
/*
 * toupper is a function that converts an integer ch (in the range EOF
 * to 255) to its uppercase value (if it was lowercase): all others are
 * left unchanged.
 */
{
   if ((ch < 'a') || (ch > 'z')) return( ch ); else return( ch - 'a' + 'A');
}
#endif						/* toupper */

/*
 * Here come the missing string.h routines.
 */

#if	defined(memchr)				/* memchr from xclib */

void	*memchr_x( const void *cs, int c, size_t n )
/*
 * return pointer to first occurrence of character c in cs, or NULL if not
 * present among the first n characters.
 */
{
   char *cp = (char *) cs;

   while (*(cp++) != c) if (!--n) return( NULL );
   return( (void *) --cp );
}
#endif						/* memchr */

#if	defined(memcmp)				/* memcmp from xclib */

int	memcmp_x( const void *cs, const void *ct, size_t n )
/*
 * compare the first n characters of cs with ct; return as with strcmp.
 */
{
   char *ps = (char *) cs;
   char *pt = (char *) ct;
   int   d;

   while (!(d = ( *(ps++) - *(pt++) ))) if (!--n) return( 0 );
   return( d );
}
#endif						/* memcmp */

#if	defined(memcpy)				/* memcpy from xclib */

void	*memcpy_x( void *s, const void *ct, size_t n )
/*
 * copy n characters from ct to s, and return s.
 */
{
   char *ps = (char *) s;
   char *pt = (char *) ct;
   void *r = s;					/* save for later */

   while (n--) ( *(ps++) = *(pt++) );
   return( r );
}
#endif						/* memcpy */

#if	defined(memmove)			/* memmove from xclib */

void	*memmove_x( void *s, const void *ct, size_t n )
/*
 * same as memcpy except that it works even if the objects overlap.
 */
{
   char *ps = (char *) s;
   char *pt = (char *) ct;
   void *r = s;					/* save for later */

   if (ps < pt) {
      while (n--) ( *(ps++) = *(pt++) );
   } else if (ps > pt) {
      ps += n;
      pt += n;
      while (n--) ( *(--ps) = *(--pt) );
   }
   return( r );
}
#endif						/* memmove */

#if	defined(memset)				/* memset from xclib */

void	*memset_x( void *s, int c, size_t n )
/*
 * place character c into first n characters of s, return s.
 */
{
   char *ps = (char *) s;
   void *r = s;					/* save for later */

   while (n--) (*ps++) = c;
   return( r );
}
#endif						/* memset */

#if	defined(strcpy)				/* strcpy from xclib */

char	*strcpy_x( char *s, const char *ct )
/*
 * copy string ct to string s, includeing '\0'; return s.
 */
{
   char *r = s;					/* save for later */

   while (*s++ = *ct++);
   return( r );
}
#endif						/* strcpy */

#if	defined(strncpy)			/* strncpy from xclib */

char	*strncpy_x( char *s, const char *ct, size_t n )
/*
 * copy at most n characters of string ct to s; return s. Pad with '\0's
 * if t has fewer than n characters.
 */
{
   char *r = s;					/* save for later */

   while ((n-- > 0) && (*s++ = *ct++));
   while (n-- > 0) *s++ = 0;
   return( r );
}
#endif						/* strncpy */

#if	defined(strcat)				/* strcat from xclib */

char	*strcat_x( char *s, const char *ct )
/*
 * concatenate string ct to end of string s; return s.
 */
{
   char *r = s;					/* save for later */

   while (*s) s++;
   while (*ct) *s++ = *ct++;
   *s = '\0';
   return( r );
}
#endif						/* strcat */

#if	defined(strncat)			/* strncat from xclib */

char	*strncat_x( char *s, const char *ct, size_t n )
/*
 * concatenate at most n characters of string ct to string s, terminate
 * s with '\0'; return s.
 */
{
   char *r = s;					/* save for later */

   while (*s) s++;
   while ((n--) && (*ct)) *s++ = *ct++;
   *s = '\0';
   return( r );
}
#endif						/* strncat */

#if	defined(strcmp)				/* strcmp from xclib */

int	strcmp_x( const char *cs, const char *ct )
/*
 * compare string cs to string ct; return < 0 if cs < ct, 0 if cs == ct,
 * or > 0 if cs > ct.
 */
{
   int   d = 0;

   while (!(d = (*cs - *ct))) if ((!*cs++) || (!*ct++)) break;
   return( d );
}
#endif						/* strcmp */

#if	defined(strncmp)			/* strncmp from xclib */

int	strncmp_x( const char *cs, const char *ct, size_t n )
/*
 * compare at most n characters of string cs to string ct; return < 0
 * if cs < ct, 0 if cs == ct, or > 0 if cs > ct.
 */
{
   int   d = 0;

   while ((n--) && !(d = (*cs - *ct))) if ((!*cs++) || (!*ct++)) break;
   return( d );
}
#endif						/* strncmp */

#if	defined(strchr)				/* strchr from xclib */

char	*strchr_x( const char *cs, int c )
/*
 * return pointer to first occurrence of c in cs or NULL if not present.
 */
{
#if	defined(__alliant__) | defined(__convex__)
   char *index( );

   return( index( cs, c ) );
#else
   if (c) {					/* special case */
      while ((*cs) && (*cs != c)) cs++;
      if (!*cs) cs = NULL;
   } else {
      while (*cs) cs++;
   }
   return( cs );
#endif
}
#endif						/* strchr */

#if	defined(strrchr)			/* strrchr from xclib */

char	*strrchr_x( const char *cs, int c )
/*
 * return pointer to last occurence of c in cs or NULL if not present.
 */
{
#if	defined(__alliant__) | defined(__convex__)
   char *rindex( );

   return( rindex( cs, c ) );
#else
   char *r = s;

   while (*cs) cs++;				/* go to end of string */
   while ((cs >= r) && (*cs != c)) cs--;
   if (cs < r) return( NULL ); else return( cs );
#endif
}
#endif						/* strrchr */

#if	defined(strspn)				/* strspn from xclib */

size_t	strspn_x( const char *cs, const char *ct )
/*
 * return length of prefix of cs consisting of characters in ct.
 */
{
   const char *set;
   size_t      l = 0;

   if (!*cs) return( l );
   do {
      set = ct;
      while ((*cs != *set) && (*set)) set++;
      if (*set) l++;
   } while ((*(++cs)) && (*set));
   return( l );
}
#endif						/* strspn */

#if	defined(strcspn)			/* strcspn from xclib */

size_t	strcspn_x( const char *cs, const char *ct )
/*
 * return length of prefix of cs consisting of characters not in ct.
 */
{
   const char *set;
   size_t      l = 0;

   if (!*cs) return( l );
   do {
      set = ct;
      while ((*cs != *set) && (*set)) set++;
      if (!*set) l++;
   } while ((*(++cs)) && (!*set));
   return( l );
}
#endif						/* strcspn */

#if	defined(strpbrk)			/* strpbrk from xclib */

char	*strpbrk_x( const char *cs, const char *ct )
/*
 * return pointer to first occurrence in string cs of any character of
 * string ct, or NULL if none are present.
 */
{
   const char *set;

   if (!*cs) return( NULL );
   do {
      set = ct;
      while ((*cs != *set) && (*set)) set++;
      if (*set) return( (char *) cs );
   } while ((*(++cs)) && (!*set));
   return( NULL );
}
#endif						/* strpbrk */

#if	defined(strstr)				/* strstr from xclib */

char	*strstr_x( const char *cs, const char *ct )
/*
 * return pointer to first occurrence of string ct in cs, or NULL if
 * not present.
 */
{
   size_t l1 = strlen( cs );
   size_t l2 = strlen( ct );
   size_t n;
   size_t nc;

   if (l1 < l2) return( NULL );
   nc = l1 - l2 + 1;
   for (n = 0; n < nc; n++) {
      if (*cs++ == *ct) {
         const char *ss1 = cs;
         const char *ss2 = ct;
         int         d = 0;
         int         l = l2;

         while ((!d) && (--l)) d = (*(ss1++) - *(++ss2));
         if (!d) return( (char *) --cs );
      }
   }
   return( NULL );
}
#endif						/* strstr */

#if	defined(strlen)				/* strlen from xclib */

size_t	strlen_x( char *cs )
/*
 * return length of cs.
 */
{
   size_t r = 0;

   while (*cs++) r++;
   return( r );
}
#endif						/* strlen */

#if	defined(strtok)				/* strtok from xclib */

static	char	*strtok_p = NULL;

char	*strtok_x( char *s, const char *ct )
/*
 * strtok searches s for tokens delimited by characters from ct. A
 * sequence of calls of strtok(s,ct) splits s into tokens, each delimited
 * by a character from ct. The first call in a sequence has a non-NULL
 * s. It finds the first token in s consisting of characters not in ct;
 * it terminates that by overwriting the next character of s with '\0'
 * and returns a pointer to the token. Each subsequent call, indicated by
 * a NULL value of s, returns the next such token, searching from just past
 * the end of the previous one. strtok return NULL when no further token
 * is found. The string ct may be different on each call.
 */
{
   const char *set;
   char       *rs = NULL;
   int         def = 0;

   if ((s == NULL) && (strtok_p == NULL)) return( NULL );
   if (s != NULL) strtok_p = s;
   while (*strtok_p) {				/* skip the leading spans */
      set = ct;
      while ((*set) && (*strtok_p != *set)) set++;
      if (!*set) {				/* leading spans skipped */
         if (!def) { rs = strtok_p++; def = 1; } else { strtok_p++; }
      } else {
         if (!def) { strtok_p++; } else { *strtok_p++ = 0; return( rs ); }
      }
   }
   if (!def) return( NULL ); else return( rs );
}
#endif						/* strtok */

#if	defined(strerror)			/* strerror from xclib */

static char *errmes_x[] = {
   "No such error",					/*  no such error */
   "Not owner",						/*  1 */
   "No such file or directory",				/*  2 */
   "No such process",					/*  3 */
   "Interrupted system call",				/*  4 */
   "I/O error",						/*  5 */
   "No such device or address",				/*  6 */
   "Arg list too long",					/*  7 */
   "Exec format error",					/*  8 */
   "Bad file number",					/*  9 */
   "No children",					/* 10 */
   "No more processes",					/* 11 */
   "Not enough core",					/* 12 */
   "Permission denied",					/* 13 */
   "Bad address",					/* 14 */
   "Block device required",				/* 15 */
   "Mount device busy",					/* 16 */
   "File exists",					/* 17 */
   "Cross-device link",					/* 18 */
   "No such device",					/* 19 */
   "Not a director",					/* 20 */
   "Is a directory",					/* 21 */
   "Invalid argument",					/* 22 */
   "File table overflow",				/* 23 */
   "Too many open files",				/* 24 */
   "Not a typewriter",					/* 25 */
   "Text file busy",					/* 26 */
   "File too large",					/* 27 */
   "No space left on device",				/* 28 */
   "Illegal seek",					/* 29 */
   "Read-only file system",				/* 30 */
   "Too many links",					/* 31 */
   "Broken pipe",					/* 32 */
   "Argument too large",				/* 33 */
   "Result too large",					/* 34 */
   "Operation would block",				/* 35 */
#if	!defined(__vms__)					/* DEC VMS stops here */
   "Operation now in progress",				/* 36 */
   "Operation already in progress",			/* 37 */
   "Socket operation on non-socket",			/* 38 */
   "Destination address required",			/* 39 */
   "Message too long",					/* 40 */
   "Protocol wrong type for socket",			/* 41 */
   "Protocol not available",				/* 42 */
   "Protocol not supported",				/* 43 */
   "Socket type not supported",				/* 44 */
   "Operation not supported on socket",			/* 45 */
   "Protocol family not supported",			/* 46 */
   "Address family not supported by protocol family",	/* 47 */
   "Address already in use",				/* 48 */
   "Can't assign requested address",			/* 49 */
   "Network is down",					/* 50 */
   "Network is unreachable",				/* 51 */
   "Network dropped connection on reset",		/* 52 */
   "Software caused connection abort",			/* 53 */
   "Connection reset by peer",				/* 54 */
   "No buffer space available",				/* 55 */
   "Socket is already connected",			/* 56 */
   "Socket is not connected",				/* 57 */
   "Can't send after socket shutdown",			/* 58 */
   "Too many references: can't splice",			/* 59 */
   "Connection timed out",				/* 60 */
   "Connection refused",				/* 61 */
   "Too many levels of symbolic links",			/* 62 */
   "File name too long",				/* 63 */
   "Host is down",					/* 64 */
   "No route to host",					/* 65 */
   "Directory not empty",				/* 66 */
   "Too many processes",				/* 67 */
   "Too many users",					/* 68 */
   "Disc quota exceeded",				/* 69 */
   "Stale NFS file handle",				/* 70 */
   "Too many levels of remote in path",			/* 71 */
#if	defined(__alliant__)				/* ALLIANT */
   "No such error",					/* 72 */
   "No such error",					/* 73 */
   "No such error",					/* 74 */
   "No such error",					/* 75 */
   "No such error",					/* 76 */
   "No such error",					/* 77 */
   "Deadlock condition",				/* 78 */
   "No record locks available",				/* 79 */
#elif	defined(__convex__)				/* CONVEX */
   "Deadlock condition",				/* 72 */
   "No record locks available",				/* 73 */
#elif   defined(__mips__)				/* DEC ULTRIX */
   "No message of desired type",			/* 72 */
   "Identifier removed",				/* 73 */
   "Alignment error",					/* 74 */
   "LOCK_MAX exceeded",					/* 75 */
#elif	defined(__sun__)				/* SUN */
   "Device is not a stream",				/* 72 */
   "Timer expired",					/* 73 */
   "Out of streams recources",				/* 74 */
   "No message of desired type",			/* 75 */
   "Trying to read unreadable message",			/* 76 */
   "Identifier removed",				/* 77 */
   "Deadlock condition",				/* 78 */
   "No record locks available",				/* 79 */
   "Machine is not on the network",			/* 80 */
   "Object is remote",					/* 81 */
   "The link has been severed",				/* 82 */
   "Advertise error",					/* 83 */
   "srmount error",					/* 84 */
   "Communication error on send",			/* 85 */
   "Protocol error",					/* 86 */
   "multihop attempted",				/* 87 */
   "Cross mount point (not an error)",			/* 88 */
   "Remote address changed",				/* 89 */
#endif
#endif
   "Unknown error"					/* unknown error */
};

#define	MAX_ERRNUM_X	( sizeof( errmes_x ) / sizeof( char *) - 1 )

char	*strerror_x( size_t n )
/*
 * return pointer to implementation defined string corresponding to error n.
 */
{
   if (n < 0) {
      n = 0;
   } else if (n > MAX_ERRNUM_X) {
      n = MAX_ERRNUM_X;
   }
   return( errmes_x[n] );
}
#endif						/* strerror */

/*
 * Now come the missing math.h routines.
 */

#if	defined(fmod)				/* fmod from xclib */

double	fmod_x( double x, double y )
/*
 * floating-point remainder of x/y, with the same sign as x. If y is
 * zero, the result is implementation defined (here x is returned).
 */
{
   if (y == 0.0) {
      return( x );
   } else {
      if (y < 0.0) y = -y;
      if (x > 0.0) {
         return( x - y * floor( x / y ) );
      } else {
         return( x + y * floor( -x / y ) );
      }
   }
}
#endif						/* fmod */

/*
 * Now come some missing stdlib.h routines.
 */

#if	defined(abs)				/* abs from xclib */

int	abs_x( int n )
/*
 * abs returns the absolute value of its int argument.
 */
{
   if (n > 0) return( n ); else if (n < 0) return( -n ); else return( 0 );
}
#endif						/* abs */

#if	defined(atexit)				/* atexit from xclib */

#if	defined(__alliant__)		/* only for alliant */

#define	ATEXIT_SIZE	32		/* at least 32 */

struct atexit {				/* the struct */
   struct atexit *next;			/* next in list */
   int ind;				/* next index in this table */
   void (*fns[ATEXIT_SIZE])();		/* the table itself */
};

static	struct atexit	*__atexit = NULL;	/* points to head stack */

int	atexit_x( void (*fn)(void) )
/*
 * atexit registers the function fcn to be called when the program
 * terminates normally; it returns non-zero if the registration cannot
 * be made.
 */
{
   static struct atexit		__atexit0;	/* always one */
   register struct atexit	*p;

   if ((p = __atexit) == NULL) {		/* first call */
      __atexit = p = &__atexit0;		/* point to ours */
   } else if (p->ind >= ATEXIT_SIZE) {
      if ((p = malloc(sizeof(*p))) == NULL) return( -1 );
      p->ind = 0;
      p->next = __atexit;
      __atexit = p;
   }
   p->fns[p->ind++] = fn;
   return( 0 );
}

extern	void	_cleanup( void );
extern	void	_exit( int );

void	exit( int status )
/*
 * DANGEROUS!!!
 * We need to overrule the exit which is in the system library.
 */
{
   register struct	atexit *p;
   register int		n;

   for ( p = __atexit; p; p = p->next ) {
      for ( n = p->ind; --n >= 0; ) (*p->fns[n])( );
   }
   _cleanup( );
   _exit( status );
}
#elif	defined(__sun__)

int	atexit_x( void (*fcn)(void) )
{
   int	on_exit( void( *fcn)(void) );
   return( on_exit( fcn ) );
}
#endif
#endif						/* atexit */

#if	defined(bsearch)			/* bsearch from xclib */

void	*bsearch_x( const void *key, const void *base, size_t n, size_t size,
		    int (*cmp)(const void *keyval, const void *datum) )
/*
 * bsearch searches base[0]...base[n-1] for an item that matches *key. The
 * function cmp must return negative if its firts argument (the search key)
 * is less than its second (a table entry), zero if equal, and positive if
 * greater. Items in the array base must be in ascending order. bsearch
 * returns a pointer to a matching item, or NULL if none exists.
 */
{
   int   r = 0;
   char *p = (char *) base;

   while (n-- && (r = cmp( key, (void *) p )) < 0) p += size;	/* next item */
   if ( r ) return( NULL ); else return( (void *) p );
}
#endif						/* bsearch */

#if	defined(div)				/* div from xclib */

div_t	div_x( int num, int denom )
/*
 * div computes the quotient and remainder of num/denom. The results are
 * stored in the int members quot and rem of a structure of type div_t
 * (defined in stdlib.h).
 */
{
   div_t r;

   r.quot = num / denom;
   r.rem = num - denom * r.quot;
   return( r );
}
#endif						/* div */

#if	defined(getenv)				/* getenv from xclib */

static  char *getenv_buf = NULL;        /* dynamic memory which holds synomym */

char	*getenv_x( char *symbol )
/*
 * getenv takes as its single argument a pointer to a string which is
 * interpreted as a ''name'' understood by the execution environment. The
 * function returns a pointer to another string which is the ''value'' of
 * the argument name. If the indicated name has no value, a null pointer
 * is returned.
 */
{
#if	defined(__vms__)
   char   *lnm_table[3] = { "LNM$PROCESS", "LNM$JOB", "LNM$SYSTEM" };
   int     getenv_len = 256;
   int     len = 0;
   int     lnm_tab = 0;
   int     ret;
   int     sys$trnlnm();                 /* Does the logical name translation */
   long    attr = LNM$M_CASE_BLIND;             /* no difference between case */
   struct  dsc$descriptor tabnam, lognam;
   struct {                                       /* Item list for sys$trnlnm */
      short l;
      short c;
      int  *a;
      int  *r;
      int   e;
   } itmlst;

   getenv_buf = realloc( getenv_buf, getenv_len );     /* resize buffer space */
   lognam.dsc$a_pointer = symbol;
   lognam.dsc$w_length  = strlen( symbol );
   lognam.dsc$b_class   = DSC$K_CLASS_S;
   lognam.dsc$b_dtype   = DSC$K_DTYPE_T;
   itmlst.a = getenv_buf;
   itmlst.c = LNM$_STRING;
   itmlst.r = &len;
   itmlst.l = getenv_len;
   itmlst.e = 0;
   do {
      tabnam.dsc$a_pointer = lnm_table[lnm_tab];
      tabnam.dsc$w_length  = strlen( lnm_table[lnm_tab] );
      tabnam.dsc$b_class   = DSC$K_CLASS_S;
      tabnam.dsc$b_dtype   = DSC$K_DTYPE_T;
      ret = sys$trnlnm( &attr, &tabnam, &lognam, NULL, &itmlst );
   } while (++lnm_tab < 3 && ret == SS$_NOLOGNAM);
   if (ret == SS$_NORMAL) {                                       /* no error */
      getenv_buf = realloc( getenv_buf, len + 1 );         /* resize buffer */
      getenv_buf[len] = 0;                          /* add trailing zero byte */
   } else {                                                          /* error */
      free( getenv_buf );
      getenv_buf = NULL;
   }
   return( getenv_buf );
#else
   NOT IMPLEMENTED
#endif
}
#endif						/* getenv */

#if	defined(labs)				/* labs from xclib */

long	labs_x( long n )
/*
 * labs returns the absolute value of its long argument.
 */
{
   if (n > 0) return( n ); else if (n < 0) return( -n ); else return( 0 );
}
#endif						/* labs */

#if	defined(ldiv)				/* ldiv from xclib */

ldiv_t	ldiv_x( long num, long denom )
/*
 * ldiv computes the quotient and remainder of num/denom. The results are
 * stored in the long members quot and rem of a structure of type ldiv_t
 * (defined in stdlib.h).
 */
{
   ldiv_t r;

   r.quot = num / denom;
   r.rem = num - denom * r.quot;
   return( r );
}
#endif						/* ldiv */

#if	defined(realloc)			/* realloc from xclib */

#undef	realloc					/* undefined */

void	*realloc_x( void *p, size_t size )
{
   void *realloc( );

   if (p == NULL) {
      return( malloc( size ) );
   } else {
      return( realloc( p, size ) );
   }
}

#define	realloc	realloc_x			/* redefine */

#endif						/* realloc */

double	_huge_val( void )			/* HUGE_VAL from xclib */
{
   union { unsigned int l[2]; double d; } float_du;

#if	defined(__vms__)			/* DEC VMS */
   float_du.l[0] = 0xffff7fff;
   float_du.l[1] = 0xffffffff;
#elif	defined(__convex__)			/* CONVEX native format */
   float_du.l[0] = 0x7fffffff;
   float_du.l[1] = 0xffffffff;
#elif	defined(__mips__)			/* infinity */
   float_du.l[0] = 0x00000000;
   float_du.l[1] = 0x7ff00000;
#else						/* infinity */
   float_du.l[0] = 0x7ff00000;
   float_du.l[1] = 0x00000000;
#endif
   return( float_du.d );
}						/* HUGE_VAL */

#if	defined(strtod)				/* strtod from xclib */

double	strtod_x( const char *s, char **endp )
/*
 * strtod converts the prefix of s to double, ignoring leading white
 * space; it stores a pointer to any unconverted suffix in *endp unless
 * endp is NULL. If the answer would overflow, HUGE_VAL is returned
 * with the proper sign; if the answer would underflow, zero is returned.
 * In either case errno is set to ERANGE.
 */
{
   char   *start = (char *) s;
   double  r = 0.0;
   int     mtherr = 0;
   int     ndig = 0;
   int     sign;

   while (isspace( *s )) s++;			/* skip white space */
   if (*s == '-') {				/* determine sign */
      sign = -1;
      s++;
   } else if (*s == '+') {
      sign = 1;
      s++;
   } else {
      sign = 1;
   }
   while (isdigit( *s )) {			/* part before decimal point */
      if (ndig < DBL_DIG) {			/* past #digits of precision */
         r = r * 10.0 + (double) (*(s++) - '0' );
         if (r != 0.0) ndig++;
      }
   }
   if (*s == '.') {				/* decimal point present */
      double f = 0.1;

      s++;
      while (isdigit( *s )) {			/* part after decimal point */
         if (ndig < DBL_DIG) {			/* past #digits of precision */
            r += ( (double) (*(s++) - '0' ) ) * f;
            f *= 0.1;
            if (r != 0.0) ndig++;
         }
      }
   }
   if (*s == 'e' || *s == 'E')  {		/* exponent present */
      double p = 0.0;
      int    ndige = 0;
      int    t;

      s++;
      if (*s == '-') {
         s++;
         t = -1;
      } else if (*s == '+') {
         s++;
         t = 1;
      } else {
         t = 1;
      }
      while (isdigit( *s )) {			/* exponent */
         if (ndige < DBL_DIG) {
            p = p * 10.0 + (double) (*(s++) - '0');
            if (p != 0.0) ndige++;
         } else {
            ndige++;
         }
      }
      if (ndige > DBL_DIG) {
         mtherr = 1;
      } else if (r != 0.0) {
         double logten;

         logten = log10( r ) + p * t;
         if (logten > log10( DBL_MAX )) {
            mtherr = 1;				/* overflow */
         } else if (logten < log10( DBL_MIN )) {
            mtherr = -1;			/* underflow */
         } else {
            r = pow( 10.0, logten );
         }
      }
   }
   if (mtherr == 0) {
      r *= sign;
      if (endp != NULL) *endp = (char *) s;	/* return current position */
   } else if (mtherr == 1) {
      r = HUGE_VAL * ((double) sign);
      if (endp != NULL) *endp = start;		/* return current position */
      errno = ERANGE;
   } else if (mtherr == -1) {
      r = 0.0;
      if (endp != NULL) *endp = start;		/* return current position */
      errno = ERANGE;
   }
   return( r );
}
#endif						/* strtod */

#if	defined(strtol)				/* strtol from xclib */

static	char	*strtol_d = { "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" };

long	strtol_x( const char *s, char **endp, int base )
/*
 * strtol converts the prefix of s to long, ignoring leading white space;
 * it stores a pointer to any unconverted suffix in *endp unless endp is
 * NULL. If base is between 2 and 36, conversion is done assuming that the
 * input is written in that base. If base is zero, the base is 8, 10 or 16;
 * leading 0 implies octal and leading 0x or 0X hexadecimal. Letters in either
 * case represent digits from 10 to base-1; a leading 0x or 0X is permitted
 * in base 16. If the answer would overflow, LONG_MAX or LONG_MIN is
 * returned, depending on the sign of the result, and errno is set to
 * ERANGE.
 */
{
   char   *p;
   char   *start = (char *) s;
   ldiv_t  huge;
   long    r = 0;
   long    sign;
   long    val;

   while (isspace( *s )) s++;			/* skip white space */
   if (*s == '-') {				/* determine sign */
      sign = -1;
      s++;
   } else if (*s == '+') {
      sign = 1;
      s++;
   } else {
      sign = 1;
   }
   if (base == 0) {				/* determine base */
      if (s[0] == '0') {
         base = 8;				/* octal */
         if (s[1] == 'X' || s[1] == 'x') {
            base = 16;				/* hexadecimal */
         }
      } else {
         base = 10;				/* decimal */
      }
   }
   if (base == 16 && s[0] == '0' && (s[1] == 'X' || s[1] == 'x')) s += 2;
   if (base > 1 && base < 37) {
      if (sign > 0) {
         huge = ldiv( LONG_MAX, base );
      } else {
         huge = ldiv( LONG_MIN, base );
      }
      if ((p = strchr( strtol_d, toupper( *s ) )) != NULL) val = p - strtol_d; else val = base;
      while (val < base) {
         val *= sign;
         if (sign > 0) {
            if (r < huge.quot || (r == huge.quot && val <= huge.rem)) {
               r = base * r + val;
            } else {
               r = LONG_MAX;
               errno = ERANGE;
            }
         } else {
            if (r > huge.quot || (r == huge.quot && val >= huge.rem)) {
               r = base * r + val;
            } else {
               r = LONG_MIN;
               errno = ERANGE;
            }
         }
         if ((p = strchr( strtol_d, toupper( *(++s) ) )) != NULL) val = p - strtol_d; else val = base;
      }
      if (endp != NULL) *endp = (char *) s;	/* return current position */
      return( r );
   } else {
      if (endp != NULL) *endp = start;		/* return initial position */
      return( 0 );
   }
}
#endif						/* strtol */

#if	defined(strtoul)			/* strtoul from xclib */

static	char	*strtoul_d = { "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ" };

unsigned long	strtoul_x( const char *s, char **endp, int base )
/*
 * strtoul is the same as strtol except that the result is unsigned long
 * and the error value is ULONG_MAX.
 */
{
   char          *p;
   char          *start = (char *) s;
   unsigned long  quot;
   unsigned long  rem;
   unsigned long  r = 0;
   unsigned long  val;

   while (isspace( *s )) s++;			/* skip white space */
   if (*s == '-') {				/* sign present */
      if (endp != NULL) *endp = start;		/* return initial position */
      return( 0 );				/* error */
   } else if (*s == '+') {
      s++;
   }
   if (base == 0) {				/* determine base */
      if (s[0] == '0') {
         base = 8;				/* octal */
         if (s[1] == 'X' || s[1] == 'x') {
            base = 16;				/* hexadecimal */
         }
      } else {
         base = 10;				/* decimal */
      }
   }
   if (base == 16 && s[0] == '0' && (s[1] == 'X' || s[1] == 'x')) s += 2;
   if (base > 1 && base < 37) {
      quot = ULONG_MAX / base;
      rem  = ULONG_MAX - quot * base;
      if ((p = strchr( strtoul_d, toupper( *s ) )) != NULL) val = p - strtoul_d; else val = base;
      while (val < base) {
         if (r < quot || (r == quot && val <= rem)) {
            r = base * r + val;
         } else {
            r = ULONG_MAX;
            errno = ERANGE;
         }
         if ((p = strchr( strtoul_d, toupper( *(++s) ) )) != NULL) val = p - strtoul_d; else val = base;
      }
      if (endp != NULL) *endp = (char *) s;	/* return current position */
      return( r );
   } else {
      if (endp != NULL) *endp = start;		/* return initial position */
      return( 0 );				/* error */
   }
}
#endif						/* strtoul */

#if	defined(system)				/* system from xclib */

#undef	system					/* remove definition */

int	system_x( const char *s )
/*
 * system passes the string s to the environment for execution. If s is NULL,
 * system returns non-zero if there is a command processor. If s is not NULL,
 * the return value is implementation-dependent.
 * Here the return value is the return value of the command (0<=return<=255).
 */
{
   extern int	system( const char * );
   int		r;

   r = system( s );
#if	defined(__convex__)
   return( ( r & 0x000000ff ) );
#else
   return( ( r & 0x0000ff00 ) >> 8 );
#endif
}

#define	system	system_x			/* put back definition */

#endif						/* system */
/*
 * Here come some float.h defines (implemented as functions).
 */

float	_flt_epsilon( void )			/* FLT_EPSILON from xclib */
{
   union { int l; float f; } float_fu;

#if	OS_FLOATING_TYPE == 0 | OS_FLOATING_TYPE == 5
   float_fu.l = 0x34000000;
#elif	OS_FLOATING_TYPE == 1 | OS_FLOATING_TYPE == 6
   float_fu.l = 0x34000000;
#elif	OS_FLOATING_TYPE == 2
   float_fu.l = 0x35000000;
#elif	OS_FLOATING_TYPE == 3
   float_fu.l = 0x00003480;
#elif	OS_FLOATING_TYPE == 4
   float_fu.l = 0x00003480;
#else
   float_fu.l = 0x00000000;
#endif
   return( float_fu.f );
}						/* FLT_EPSILON */

float	_flt_max( void )			/* FLT_MAX from xclib */
{
   union { int l; float f; } float_fu;

#if	OS_FLOATING_TYPE == 0 | OS_FLOATING_TYPE == 5
   float_fu.l = 0x7f7fffff;
#elif	OS_FLOATING_TYPE == 1 | OS_FLOATING_TYPE == 6
   float_fu.l = 0x7f7fffff;
#elif	OS_FLOATING_TYPE == 2
   float_fu.l = 0x7fffffff;
#elif	OS_FLOATING_TYPE == 3
   float_fu.l = 0xffff7fff;
#elif	OS_FLOATING_TYPE == 4
   float_fu.l = 0xffff7fff;
#else
   float_fu.l = 0x00000000;
#endif
   return( float_fu.f );
}						/* FLT_MAX */

float	_flt_min( void )			/* FLT_MIN from xclib */
{
   union { int l; float f; } float_fu;

#if	OS_FLOATING_TYPE == 0 | OS_FLOATING_TYPE == 5
   float_fu.l = 0x00800000;
#elif	OS_FLOATING_TYPE == 1 | OS_FLOATING_TYPE == 6
   float_fu.l = 0x00800000;
#elif	OS_FLOATING_TYPE == 2
   float_fu.l = 0x00800000;
#elif	OS_FLOATING_TYPE == 3
   float_fu.l = 0x0000017f;
#elif	OS_FLOATING_TYPE == 4
   float_fu.l = 0x0000017f;
#else
   float_fu.l = 0x00000000;
#endif
   return( float_fu.f );
}						/* FLT_MIN */

double	_dbl_epsilon( void )			/* DBL_EPSILON from xclib */
{
   union { int l[2]; double d; } float_du;

#if	OS_FLOATING_TYPE == 0 | OS_FLOATING_TYPE == 5
   float_du.l[0] = 0x3cb00000;
   float_du.l[1] = 0x00000000;
#elif	OS_FLOATING_TYPE == 1 | OS_FLOATING_TYPE == 6
   float_du.l[0] = 0x00000000;
   float_du.l[1] = 0x3cb00000;
#elif	OS_FLOATING_TYPE == 2
   float_du.l[0] = 0x3cd00000;
   float_du.l[1] = 0x00000000;
#elif	OS_FLOATING_TYPE == 3
   float_du.l[0] = 0x00002480;
   float_du.l[1] = 0x00000000;
#elif	OS_FLOATING_TYPE == 4
   float_du.l[0] = 0x00003cc0;
   float_du.l[1] = 0x00000000;
#else
   float_du.l[0] = 0x00000000;
   float_du.l[1] = 0x00000000;
#endif
   return( float_du.d );
}						/* DBL_EPSILON */

double	_dbl_max( void )			/* DBL_MAX from xclib */
{
   union { int l[2]; double d; } float_du;

#if	OS_FLOATING_TYPE == 0 | OS_FLOATING_TYPE == 5
   float_du.l[0] = 0x7fefffff;
   float_du.l[1] = 0xffffffff;
#elif	OS_FLOATING_TYPE == 1 | OS_FLOATING_TYPE == 6
   float_du.l[0] = 0xffffffff;
   float_du.l[1] = 0x7fefffff;
#elif	OS_FLOATING_TYPE == 2
   float_du.l[0] = 0x7fffffff;
   float_du.l[1] = 0xffffffff;
#elif	OS_FLOATING_TYPE == 3
   float_du.l[0] = 0xffff7fff;
   float_du.l[1] = 0xffffffff;
#elif	OS_FLOATING_TYPE == 4
   float_du.l[0] = 0xffff7fff;
   float_du.l[1] = 0xffffffff;
#else
   float_du.l[0] = 0x00000000;
   float_du.l[1] = 0x00000000;
#endif
   return( float_du.d );
}						/* DBL_MAX */

double	_dbl_min( void )			/* DBL_MIN from xclib */
{
   union { int l[2]; double d; } float_du;

#if	OS_FLOATING_TYPE == 0 | OS_FLOATING_TYPE == 5
   float_du.l[0] = 0x00100000;
   float_du.l[1] = 0x00000000;
#elif	OS_FLOATING_TYPE == 1 | OS_FLOATING_TYPE == 6
   float_du.l[0] = 0x00000000;
   float_du.l[1] = 0x00100000;
#elif	OS_FLOATING_TYPE == 2
   float_du.l[0] = 0x00100000;
   float_du.l[1] = 0x00000000;
#elif	OS_FLOATING_TYPE == 3
   float_du.l[0] = 0x0000017f;
   float_du.l[1] = 0x00000000;
#elif	OS_FLOATING_TYPE == 4
   float_du.l[0] = 0x00000010;
   float_du.l[1] = 0x00000000;
#else
   float_du.l[0] = 0x00000000;
   float_du.l[1] = 0x00000000;
#endif
   return( float_du.d );
}						/* DBL_MIN */


/*
 * Here come the missing <time.h> functions.
 */

#if	defined(clock)				/* clock from xclib */

clock_t	clock_x( void )
/*
 * clock returns the processor time used by the program since the
 * beginning of execution, or -1 if unavailable, clock()/CLK_TCK is
 * a time in seconds.
 */
{
   clock_t r;						/* return value */
#if	defined(__unix__) | defined(unix)
   struct tms {
      time_t tms_utime;					/* user time */
      time_t tms_stime;					/* system time */
      time_t tms_cutime;				/* user time, children */
      time_t tms_cstime;				/* system time, children */
   } buffer;
   void times( struct tms *buffer );			/* returns tms struct */

   times( &buffer );					/* get total cpu time */
   r = buffer.tms_utime + buffer.tms_stime + buffer.tms_cutime + buffer.tms_cstime;
#else
   r = -1;						/* not implemented */
#endif
   return( r );
}
#endif						/* clock */

#if	defined(difftime)			/* difftime from xclib */

double	difftime_x( time_t time2, time_t time1 )
/*
 * difftime returns time2-time1 expressed in seconds.
 */
{
   return( (double) (time2 - time1) );
}
#endif						/* difftime */

#if	defined(mktime)				/* mktime from xclib */

time_t	mktime_x( struct tm *tp )
/*
 * mktime converts the local time in the structure *tp into calendar
 * time in the same representation used by time.  The components will
 * have values in the ranges shown. mktime returns the calendar time
 * or -1 if it cannot be represented.
 */
{
   int    days = 0;					/* day counter */
   int    yday = 0;					/* day of year */
   int    year = 70;					/* year counter */
   time_t seconds = 0;					/* calendar time */

   for (; year < tp->tm_year; year++) {
      if (year%4) days += 365; else days += 366;	/* days */
   }
   switch(tp->tm_mon) {					/* which month */
      case 0: {						/* January */
         if (tp->tm_mday < 1 || tp->tm_mday > 31) return( -1 );
         break;
      }
      case 1: {						/* february */
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            if (tp->tm_mday < 1 || tp->tm_mday > 28) return( -1 );
         } else {
            if (tp->tm_mday < 1 || tp->tm_mday > 29) return( -1 );
         }
         yday += 31;
         break;
      }
      case 2: {						/* march */
         if (tp->tm_mday < 1 || tp->tm_mday > 31) return( -1 );
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            yday += 59;
         } else {
            yday += 60;
         }
         break;
      }
      case 3: {						/* april */
         if (tp->tm_mday < 1 || tp->tm_mday > 30) return( -1 );
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            yday += 90;
         } else {
            yday += 91;
         }
         break;
      }
      case 4: {						/* may */
         if (tp->tm_mday < 1 || tp->tm_mday > 31) return( -1 );
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            yday += 120;
         } else {
            yday += 121;
         }
         break;
      }
      case 5: {						/* june */
         if (tp->tm_mday < 1 || tp->tm_mday > 30) return( -1 );
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            yday += 151;
         } else {
            yday += 152;
         }
         break;
      }
      case 6: {						/* july */
         if (tp->tm_mday < 1 || tp->tm_mday > 31) return( -1 );
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            yday += 181;
         } else {
            yday += 182;
         }
         break;
      }
      case 7: {						/* august */
         if (tp->tm_mday < 1 || tp->tm_mday > 31) return( -1 );
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            yday += 212;
         } else {
            yday += 213;
         }
         break;
      }
      case 8: {						/* september */
         if (tp->tm_mday < 1 || tp->tm_mday > 30) return( -1 );
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            yday += 243;
         } else {
            yday += 244;
         }
         break;
      }
      case 9: {						/* october */
         if (tp->tm_mday < 1 || tp->tm_mday > 31) return( -1 );
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            yday += 273;
         } else {
            yday += 274;
         }
         break;
      }
      case 10: {					/* november */
         if (tp->tm_mday < 1 || tp->tm_mday > 30) return( -1 );
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            yday += 304;
         } else {
            yday += 305;
         }
         break;
      }
      case 11: {					/* december */
         if (tp->tm_mday < 1 || tp->tm_mday > 31) return( -1 );
         if (tp->tm_year%4 || !(tp->tm_year%100)) {
            yday += 334;
         } else {
            yday += 335;
         }
         break;
      }
      default: {
         return( -1 );
      }
   }
   yday += tp->tm_mday - 1;				/* day of the year */
   tp->tm_yday = yday;					/* return day of year */
   days += yday;					/* total number of days */
   tp->tm_wday = (days + 4)%7;				/* return day of the week */
   seconds += (days * 86400);				/* days to seconds */
   if (tp->tm_hour < 0 || tp->tm_hour > 23) return( -1 );
   seconds += (tp->tm_hour * 3600);			/* hours to seconds */
   if (tp->tm_min < 0 || tp->tm_min > 59) return( -1 );
   seconds += (tp->tm_min * 60);			/* minutes to seconds */
   if (tp->tm_sec < 0 || tp->tm_sec > 59) return( -1 );
   seconds += (tp->tm_sec);				/* seconds to seconds */
   if (tp->tm_isdst > 0) seconds -= 3600;		/* correction */
#if	defined(__sysv__)				/* SYSTEM 5 */
   {
      extern long	timezone;

      seconds += timezone;
   }
#else							/* BSD and VMS */
   {
      extern void	ftime( );
      struct timeb {
         time_t		time;
         unsigned short	millitm;
         short		timezone;
         short		dstflag;
      } bsd_time;

      ftime( &bsd_time );
      seconds += 60 * bsd_time.timezone;
   }
#endif
   return( seconds );					/* return value */
}

#endif						/* mktime */

#if	defined(__vms__)			/* timezone for vms */

static	char	timezone_b[80];				/* static area */

static	char	*timezone( int zone, int dst )
{
   switch( zone ) {					/* which zone */
      case -720: {
         if (dst) {
            strcpy( timezone_b, "NZDT" );
         } else {
            strcpy( timezone_b, "NZST" );
         }
         break;
      }
      case -600: strcpy( timezone_b, "EST" ); break;
      case -540: {
         if (dst) {
            strcpy( timezone_b, "GMT+9:00" );
         } else {
            strcpy( timezone_b, "JST" );
         }
         break;
      }
      case -480: {
         if (dst) {
            strcpy( timezone_b, "GMT+8:00" );
         } else {
            strcpy( timezone_b, "WST" );
         }
         break;
      }
      case -120: {
         if (dst) {
            strcpy( timezone_b, "EET DST" );
         } else {
            strcpy( timezone_b, "EET" );
         }
         break;
      }
      case  -60: {
         if (dst) {
            strcpy( timezone_b, "MET DST" );
         } else {
            strcpy( timezone_b, "MET" );
         }
         break;
      }
      case    0: {
         if (dst) {
            strcpy( timezone_b, "BST" );
         } else {
            strcpy( timezone_b, "GMT" );
         }
         break;
      }
      case  240: {
         if (dst) {
            strcpy( timezone_b, "ADT" );
         } else {
            strcpy( timezone_b, "AST" );
         }
         break;
      }
      case  300: {
         if (dst) {
            strcpy( timezone_b, "EDT" );
         } else {
            strcpy( timezone_b, "EST" );
         }
         break;
      }
      case  360: {
         if (dst) {
            strcpy( timezone_b, "CDT" );
         } else {
            strcpy( timezone_b, "CST" );
         }
         break;
      }
      case  420: {
         if (dst) {
            strcpy( timezone_b, "MDT" );
         } else {
            strcpy( timezone_b, "MST" );
         }
         break;
      }
      case  480: {
         if (dst) {
            strcpy( timezone_b, "PDT" );
         } else {
            strcpy( timezone_b, "PST" );
         }
         break;
      }
      case  540: {
         if (dst) {
            strcpy( timezone_b, "YDT" );
         } else {
            strcpy( timezone_b, "YST" );
         }
         break;
      }
      case  600: {
         if (dst) {
            strcpy( timezone_b, "HDT" );
         } else {
            strcpy( timezone_b, "HST" );
         }
         break;
      }
      default: {
         int	h, m, s;

         if (zone < 0) {
            s = 0; zone = -zone;
         } else s = 1;
         h = zone / 60;
         m = zone % 60;
         if (s) {
            sprintf( timezone_b, "GMT-%d:%02.2d", h, m );
         } else {
            sprintf( timezone_b, "GMT+%d:%02.2d", h, m );
         }
         break;
      }
   }
   return( timezone_b );
}

#endif

#if	defined(strftime)			/* strftime from xclib */

size_t	strftime_x( char *s, size_t smax, const char *fmt, const struct tm *tp )
/*
 * strftime formats date and time information from *tp into s according
 * to fmt, which is analogous to a printf format. Ordinary charactrers
 * (including the terminating '\0') are copied into s. Each %c is replaced
 * as described below, using values appropriate for the local environment.
 * No more than smax characters are places into s. strftime returns the
 * number of characrters, excluding the '\0', or zero if more than smax
 * characters were produced.
 *
 *        %a    abbreviated weekday name.
 *        %A    full weekday name.
 *        %b    abbreviated month name.
 *        %B    full month name.
 *        %c    local date and time representation.
 *        %d    day of the month (01-31).
 *        %H    hour (24-hour clock) (00-23).
 *        %I    hour (12-hour clock) (01-12).
 *        %j    day of the year (001-366).
 *        %m    month (01-12).
 *        %M    minute (00-59).
 *        %p    local equivalent of AM or PM.
 *        %S    second (00-59).
 *        %U    week number of the year (Sunday as 1st day of week) (00-53).
 *        %w    weekday (0-6, Sunday is 0).
 *        %W    week number of the year (Monday as 1st day of week) (00-53).
 *        %x    local date representation.
 *        %X    local time representation.
 *        %y    year without century (00-99).
 *        %Y    year with century.
 *        %Z    time zone name, if any.
 *        %%    %.
 */
{
   char		*p;
   char		text[32];
   size_t	q = 0, r = 0;

   while (fmt[q]) {					/* until end of fmt */
      while (fmt[q] && fmt[q] != '%') {
         if (r < smax) s[r++] = fmt[q];
         q += 1;
      }
      if (!fmt[q++]) break;				/* we're done */
      switch( fmt[q++] ) {
         case 'a': {					/* abbreviated weekday */
            switch( tp->tm_wday ) {
               case 0: p = "Sun"; break;
               case 1: p = "Mon"; break;
               case 2: p = "Tue"; break;
               case 3: p = "Wed"; break;
               case 4: p = "Thu"; break;
               case 5: p = "Fri"; break;
               case 6: p = "Sat"; break;
               default: p = NULL; break;
            }
            break;
         }
         case 'A': {					/* full weekday */
            switch( tp->tm_wday ) {
               case 0: p = "Sunday"; break;
               case 1: p = "Monday"; break;
               case 2: p = "Tuesday"; break;
               case 3: p = "Wedneday"; break;
               case 4: p = "Thursday"; break;
               case 5: p = "Friday"; break;
               case 6: p = "Saturday"; break;
               default: p = NULL; break;
            }
            break;
         }
         case 'b': {					/* abbreviated month */
            switch( tp->tm_mon ) {
               case 0: p = "Jan"; break;
               case 1: p = "Feb"; break;
               case 2: p = "Mar"; break;
               case 3: p = "Apr"; break;
               case 4: p = "May"; break;
               case 5: p = "Jun"; break;
               case 6: p = "Jul"; break;
               case 7: p = "Aug"; break;
               case 8: p = "Sep"; break;
               case 9: p = "Oct"; break;
               case 10: p = "Nov"; break;
               case 11: p = "Dec"; break;
               default: p = NULL;
            }
            break;
         }
         case 'B': {					/* full month */
            switch( tp->tm_mon ) {
               case 0: p = "January"; break;
               case 1: p = "February"; break;
               case 2: p = "March"; break;
               case 3: p = "April"; break;
               case 4: p = "May"; break;
               case 5: p = "June"; break;
               case 6: p = "July"; break;
               case 7: p = "August"; break;
               case 8: p = "September"; break;
               case 9: p = "October"; break;
               case 10: p = "November"; break;
               case 11: p = "December"; break;
               default: p = NULL;
            }
            break;
         }
         case 'c': {					/* local date and time */
            p = asctime( tp );
            p[strlen(p)-1] = 0;
            break;
         }
         case 'd': {					/* day of the month */
            sprintf( text, "%02.2d", tp->tm_mday );
            p = text;
            break;
         }
         case 'H': {					/* 24-hour clock */
            sprintf( text, "%02.2d", tp->tm_hour );
            p = text;
            break;
         }
         case 'I': {					/* 12-hour clock */
            int	hour = tp->tm_hour;

            if (!hour) hour = 12; else if (hour > 12) hour -= 12;
            sprintf( text, "%02.2d", hour );
            p = text;
            break;
         }
         case 'j': {					/* day of the year */
            sprintf( text, "%03.3d", tp->tm_yday + 1);
            p = text;
            break;
         }
         case 'm': {					/* month */
            sprintf( text, "%02.2d", tp->tm_mon + 1 );
            p = text;
            break;
         }
         case 'M': {					/* minute */
            sprintf( text, "%02.2d", tp->tm_min );
            p = text;
            break;
         }
         case 'p': {					/* AM or PM */
            if (tp->tm_hour < 12) p = "AM"; else p = "PM";
            break;
         }
         case 'S': {					/* second */
            sprintf( text, "%02.2d", tp->tm_sec );
            p = text;
            break;
         }
         case 'U': {					/* week number */
            int	wday0, week;

            wday0 = ( tp->tm_wday + 14 - ( tp->tm_yday % 7 ) ) % 7;
            week = ( tp->tm_yday + wday0 ) / 7;
            sprintf( text, "%02.2d", week );
            p = text;
            break;
         }
         case 'w': {					/* week day */
            sprintf( text, "%d", tp->tm_wday );
            p = text;
            break;
         }
         case 'W': {					/* week number */
            int	wday0, week;

            wday0 = ( tp->tm_wday + 13 - ( tp->tm_yday % 7 ) ) % 7;
            week = ( tp->tm_yday + wday0 ) / 7;
            sprintf( text, "%02.2d", week );
            p = text;
            break;
         }
         case 'x': {					/* local date */
            sprintf( text, "%02.2d/%02.2d/%02.2d", tp->tm_mon + 1, tp->tm_mday, tp->tm_year );
            p = text;
            break;
         }
         case 'X': {					/* local time */
            sprintf( text, "%02.2d:%02.2d:%02.2d", tp->tm_hour, tp->tm_min, tp->tm_sec );
            p = text;
            break;
         }
         case 'y': {					/* year without century */
            sprintf( text, "%02.2d", tp->tm_year );
            p = text;
            break;
         }
         case 'Y': {					/* year with century */
            sprintf( text, "%04.4d", tp->tm_year + 1900 );
            p = text;
            break;
         }
         case 'Z': {					/* time zone name */
#if	defined(__sysv__)				/* SYSTEM 5 */
            extern char	*tzname[2];

            if (tp->tm_isdst > 0) {
               p = strcpy( text, tzname[1] );
            } else {
               p = strcpy( text, tzname[0] );
            }
#else							/* BSD & VMS */
            char	*timezone(int, int);
            void	ftime( );
            struct timeb {
               time_t		time;
               unsigned short	millitm;
               short		timezone;
               short		dstflag;
            } bsd_time;

            ftime( &bsd_time );
            p = timezone( bsd_time.timezone, tp->tm_isdst );
#endif
            break;
         }
         case '%': {					/* % */
            p = "%";
            break;
         }
         default: {
            p = NULL;
            break;
         }
      }
      if (p != NULL) {
         while (*p) if (r < smax) s[r++] = *p++; else p++;
      }
   }
   if (r < smax) s[r] = 0; else r = 0;
   return( r );
}

#endif						/* strftime */

/*
 * Here come the missing <signal.h> functions.
 */

#if	defined(raise)				/* raise from xclib */

int	raise_x( int sig )
/*
 * raise sends the signal to the program; it returns non-zero if unsuccessful.
 */
{
   extern	int	getpid( );
   extern	int	kill( );

   return( kill( getpid( ), sig ) );
}

#endif						/* raise */

#if	defined(signal)				/* signal from xclib */

#undef	signal

#define	MAXSIG	128				/* maximum number of signals */

static	void	(*sighandlers[MAXSIG])(int);	/* list of handlers */

static	void	sighandler( int sig )
{
   extern void	(*signal(int, void (*handler)(int)))(int);

   signal( sig, SIG_DFL );			/* restore default action */
   sighandlers[sig]( sig );			/* call the handler */
}

void	(*signal_x( int sig, void (*handler)(int)))(int)
/*
 * signal determines how subsequent signals will be handled. If handler
 * is SIG_DFL, the implementation-defined default behaviour is used; if
 * it is SIG_IGN, the signal is ignored; otherwise, the function pointed
 * to by handler will be called, with the argument of the type of signal.
 */
{
   void		(*r)(int);			/* return value */
   extern void	(*signal(int, void (*handler)(int)))(int);

   if ( ( handler == SIG_DFL ) || ( handler == SIG_IGN ) ) {
      r = signal( sig, handler );		/* no real handler */
   } else {
      r = signal( sig, sighandler );		/* install our handler */
      sighandlers[sig] = handler;		/* pointer in list */
   }
   return( r );					/* return to caller */
}

#define	signal	signal_x
#endif						/* signal */


#else
void xclib_stub(void)
{
}
#endif						/* STOP COMPILING */
