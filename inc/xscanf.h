/* xscanf.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            xscanf.dc3

Header:       xscanf.h

Purpose:      Includes code for reading entries from GIPSY SETUP files.

Category:     MANAGEMENT

File:         xscanf.h

Author:       K.G. Begeman

Use:          int xscanf( FILE *setup_file, const char *format, ... )

              xscanf         Returns the number of record entries or
                             EOF if end of file is reached.
              setup_file     File descriptor returned by fopen.
              format         Format which describes the decoding of
                             record entries. Decoding types are:
                             strings     They are denoted by %s or %Ns,
                                         where N is an optional maximum
                                         length. Instead of N an asterix
                                         may be used, which means that the
                                         maximum string length is read from
                                         the argument list.
                                         If a maximum length is specified,
                                         it does not include the trailing
                                         zero byte.
                             integers    They are denoted by %d.
              ...            Arguments can be of type character pointer for
                             text fields, integer pointer for integer fields
                             or integer in case of length specifier.

Updates:      May  3, 1991: KGB Document created.

#<

*/

#include        "ctype.h"			/* <ctype.h> */
#include        "stdarg.h"			/* <stdarg.h> */
#include	"stdio.h"			/* <stdio.h> */

static	int	xscanf( FILE *f, const char *format, ... )
{
   int	r = 0;					/* return value */

   if (feof( f )) return( EOF );		/* we've reached the end */
   while (!r) {					/* until something found */
      int	ch = fgetc( f );		/* get character */

      switch( ch ) {				/* which action */
         case EOF: {				/* end of file */
            r = EOF;
            break;
         }
         case '#': {				/* comment line, skip it */
            while ((ch = fgetc( f )) != '\n' && ch != EOF);
            if (ch == EOF) r = EOF;
            break;
         }
         case '\n': {				/* skip empty lines */
            break;
         }
         default: {				/* here is some info */
            const char	*p = format;		/* scans the format */
            int		pch = ':';		/* previous character */
            va_list	ap;			/* argument pointer */

            va_start( ap, format );		/* initialize argument pointer */
            while ( pch == ':' ) {
               int	width = 0;		/* width */

               while (*p && *p != '%') p++;	/* go to first % */
               if (!*p) {			/* skip rest of record */
                  while (ch != ':' && ch != '\n' && ch != EOF) {
                     int	doch = 1;	/* do next character */

                     while (ch == '\\') {	/* escape character */
                        ch = fgetc( f );	/* get escaped character */
                        if (ch == '\n') {	/* continuation on next line */
                           ch = fgetc( f );	/* next character */
                           if (ch == ':' || ch == '\n' || ch == EOF) {
                              doch = 0;
                              break;
                           }
                        } else {
                           break;
                        }
                     }
                     if (doch) ch = fgetc( f );	/* get next character */
                  }
                  r += 1;			/* skipped a field */
                  pch = ch;
                  if (ch == ':') ch = fgetc( f );	/* point to beginning of next field */
                  continue;			/* skip rest */
               }
               p++;				/* advance one character */
               if (*p == '*') {			/* read from argument list */
                  width = va_arg( ap, int );	/* get width */
                  p++;				/* advance one character */
               } else while (isdigit(*p)) {	/* decode integer */
                  width = 10 * width + (*p++) - '0';
               }
               switch( *p++ ) {			/* which type of info */
                  case 's': {			/* text info */
                     char	*sp;		/* pointer from argument list */
                     int	count = 0;	/* counter */

                     sp = va_arg( ap, char * );	/* get string pointer */
                     (*sp) = 0;			/* set to empty string */
                     while (ch != ':' && ch != '\n' && ch != EOF) {
                        int	doch = 1;	/* do next character */

                        while (ch == '\\') {	/* escape character */
                           ch = fgetc( f );	/* get escaped character */
                           if (ch == '\n') {	/* continuation on next line */
                              ch = fgetc( f );	/* next character */
                              if (ch == ':' || ch == '\n' || ch == EOF) {
                                 doch = 0;
                                 break;
                              }
                           } else {
                              break;
                           }
                        }
                        if (doch) {		/* do this character */
                           if (!width || count++ < width) *sp++ = ch;
                           ch = fgetc( f );	/* get next character */
                        }
                     }
                     *sp = '\0';		/* add zero byte */
                     r += 1;			/* update number of fields */
                     break;
                  }
                  case 'd': {			/* integer info */
                     char	*sp;
                     char	value[20];	/* string for number */
                     int	count = 0;	/* counter */
                     int	*ip;		/* argument pointer */
                     int	sign;		/* sign of integer */
                     int	width;		/* length */

                     width = sizeof( value ) - 1;
                     ip = va_arg( ap, int * );	/* get pointer */
                     (*ip) = 0;			/* set to zero */
                     sp = value;
                     while (ch != ':' && ch != '\n' && ch != EOF) {
                        int	doch = 1;	/* do next character */

                        while (ch == '\\') {	/* escape character */
                           ch = fgetc( f );	/* get escaped character */
                           if (ch == '\n') {	/* continuation on next line */
                              ch = fgetc( f );	/* next character */
                              if (ch == ':' || ch == '\n' || ch == EOF) {
                                 doch = 0;
                                 break;
                              }
                           } else {
                              break;
                           }
                        }
                        if (doch) {		/* do this character */
                           if (count++ < width) *sp++ = ch;
                           ch = fgetc( f );	/* get next character */
                        }
                     }
                     *sp = '\0';		/* add zero byte */
                     r += 1;			/* update number of fields */
                     sp = value;
                     if ((*sp) == '-') {	/* minus sign */
                        sign = -1;
                        sp++;
                     } else if ((*sp) == '+') {	/* plus sign */
                        sign = 1;
                        sp++;
                     } else {			/* default sign (+) */
                        sign = 1;
                     }
                     while (isdigit(*sp)) {	/* decode integer */
                        (*ip) = 10 * (*ip) + (*sp) - '0';
                        sp++;
                     }
                     (*ip) *= sign;		/* 'add' sign */
                     break;
                  }
                  default: {			/* nothing to be done */
                     break;
                  }
               }
               pch = ch;
               if (ch == ':') ch = fgetc( f );	/* point to beginning of next field */
            }
            va_end( ap );
            break;
         }
      }
   }
   return( r );
}

