/* fname.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            fname.dc2

Function:     FNAME

Purpose:      Translates GIPSY filenames to legal filenames
              (if necessary).

Category:     FILES

File:         fname.c

Author:       K.G. Begeman

Use:          INTEGER FNAME( INFILE  ,      Input    CHARACTER*(*)
                             OUTFILE )      Output   CHARACTER*(*)

              FNAME      Returns -1 on error, otherwise 0.
              INFILE     File name to be translated.
              OUTFILE    Translated file name. If not translatable,
                         contents of INFILE are copied to OUTFILE.

Example:      INTEGER       FNAME
              CHARACTER*80  FILENAME

              ...
              IF (FNAME( 'DB1:ROTCUR.DAT', FILENAME ) .EQ. 0)
              THEN
                 OPEN(UNIT=LUN,FILE=FILENAME,...)
                 ...
                 CLOSE(UNIT=LUN)
              ENDIF

Action:       FNAME converts the input Gipsy filename to a legal
              filename which is understood by the Fortran open
              or C fopen statement. A Gipsy filename has the
              following syntax:
              filename   : device + separator + name
                           name
              device     : string of legal chars
              separator  : {':'}
              name       : string of legal chars
              legal chars: one of {'0' '1' '2' '3' '4' '5' '6' '7'
                                   '8' '9' 'A' 'B' 'C' 'D' 'E' 'F'
                                   'G' 'H' 'I' 'J' 'K' 'L' 'M' 'N'
                                   'O' 'P' 'Q' 'R' 'S' 'T' 'U' 'V'
                                   'W' 'X' 'Y' 'Z' 'a' 'b' 'c' 'd'
                                   'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l'
                                   'm' 'n' 'o' 'p' 'q' 'r' 's' 't'
                                   'u' 'v' 'w' 'x' 'y' 'z' '_' '.'}

Warning:      System dependent!

Updates:      Oct 17, 1988: KGB Document created.
              Aug 17, 1990: KGB Migrated to portable GIPSY.

#<

Fortran to C interface:

@ integer function fname( character, character )

*/
 
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */ 
#include	"string.h"		/* <string.h> */
#include	"ctype.h"		/* <ctype.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */

fint	fname_c( fchar iname, fchar oname )
{
   char *inamea = iname.a;			/* input char pointer */
   char *onamea = oname.a;			/* output char pointer */
   fint  inamel = iname.l;			/* length of input string */
   fint  onamel = oname.l;			/* length of output string */
   fint  d = 0;					/* separator position */
   fint  m = 0;					/* loop counter output */
   fint  n = 0;					/* loop counter input */
   fint  r = 0;					/* return value */

   while (!r && inamea[n] && (inamea[n] != ' ') && (n < inamel)) {
      int ch = inamea[n++];			/* current character */

      if (m < onamel && !d && (ch == ':')) {
#if	defined(__vms__)			/* VMS */
         onamea[m++] = ':';			/* translation not needed */
         d = m;					/* save this position */
#else						/* all UNIX */
         char *s;				/* character pointer */

         onamea[m] = '\0';			/* add zero byte */
         s = getenv( onamea );			/* translate logical name */
         if (s != NULL) {			/* translation successful */
            m = 0;				/* reset counter */
            while (!r && s[d]) {		/* copy loop */
               if (m == onamel) {		/* error ? */
                  r = -1;			/* output buffer too small */
               } else {				/* no error */
                  onamea[m++] = s[d++];	/* copy */
               }
            }
            if (!r && (onamea[m-1] != '/')) {	/* add a slash */
               if (m < onamel) {
                  onamea[m++] = '/';		/* slash added */
               } else {				/* error */
                  r = -1;			/* output buffer too small */
               }
            }
         } else {
            r = -1;				/* no translation possible */
         }
#endif
      } else if (m < onamel && (isalnum(ch) || (ch == '.') || (ch == '_'))) {
         onamea[m++] = ch;			/* copy character */
      } else {					/* error ? */
         r = -1;				/* always an error */
      }
   }
   if (r) m = 0;				/* reset */
   while (m < onamel) onamea[m++] = ' ';	/* fill (rest) with blanks */
   return( r );					/* return to caller */
}
 
#if	defined(TESTBED)
int main()
{
   char  inameb[80];
   char  onameb[20];
   fchar iname;
   fchar oname;
   fint  r;

   strcpy( inameb, "device:file.dat     " );
   iname.a = inameb; iname.l = strlen( inameb );
   oname.a = onameb; oname.l = sizeof( onameb );
   printf( "iname = %.*s|\n", (int) iname.l, iname.a );
   r = fname_c( iname, oname );
   if (!r) {
      printf( "oname = %.*s|\n", (int) oname.l, oname.a );
   } else {
      printf( "error translating iname\n" );
   }
   return( r );
}
#endif
