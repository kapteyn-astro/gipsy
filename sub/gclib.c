/* gclib.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            gclib.dc3

Document:     gclib

Purpose:      Describes the available routines in the Gipsy C LIBrary.

Category:     UTILITY

File:         gclib.c

Author:       K.G. Begeman

Description:  The following useful C routines are available:

              int char2str( fchar c, char *s, int ls )
              int str2char( char *s, fchar c )
              fchar tofchar( char *s )
              char *zadd( fchar )
              
              Each routine has its own document!

Updates:      Apr 22, 1990: KGB, Document created.

#<

*/

#include	"stddef.h"			/* <stddef.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */
#include	"f2cvvdefs.h"			/* f2cvv definitions */

/*
#>            tofchar.dc3

Function:     tofchar

Purpose:      tofchar converts a null-terminated string to the fortran
              equivalent of a character (fchar).

Category:     UTILITY

File:         gclib.c

Author:       K.G. Begeman

Use:          fchar tofchar( char *s )

              tofchar   Returns a Fortran character descriptor (fchar)
                        from s.
              s         Input null-terminated string.

Notes:        tofchar is defined in "gipsyc.h".

Updates:      Feb 27, 1990: KGB, Document created.

#<
*/

fchar tofchar( char *s )
{
   fchar r;					/* return value */

   r.a = s;					/* copy pointer */
   r.l = strlen( s );				/* put length */
   return( r );					/* return to caller */
}

/*
#>            char2str.dc3

Function:     char2str

Purpose:      Copy a fchar value to a char[].

Category:     UTILITY

File:         gclib.c

Author:       J.P. Terlouw

Use:          int char2str( fchar c, char *s, int ls )

              char2str   Returns the number of characters transferred.
              c          Input fchar object. The transfer stops at the
                         end of c.
              s          Output string. If the length of c exceeds the
                         length of s, only the first ls-1 elements are
                         transferred.
              ls         length of s.

Notes:        char2str is defined in "gipsyc.h".

Updates:      Feb 27, 1990: KGB, Document created.

#<
*/

int char2str( fchar c, char *s, int ls )
{
   int r;					/* return value */

   for (r = 0; r < c.l && r < (ls - 1) ; r++) *(s++) = *(c.a++);
   *s = 0;					/* append nul */
   return( r );					/* return to caller */
}

/*
#>            str2char.dc3

Function:     str2char

Purpose:      Copy a zero-terminated character string to a fchar.

Category:     UTILITY

File:         gclib.c

Author:       J.P. Terlouw

Use:          int str2char( char *s, fchar c )

              str2char   Returns the actual number of characters
                         transferred from s to c.
              s          Input character string.
              c          Output fchar. If c is longer than s, then
                         the result is padded with blanks; if it is
                         shorter, then the result is truncated.

Notes:        str2char is defined in "gipsyc.h".

Updates:      Feb 27, 1990: KGB, Document created.

#<
*/

int str2char( char *s, fchar c )
{
   int r;					/* return value */

   for (r = 0; *s != 0 && r < c.l; r++) *(c.a++) = *(s++);	/* copy */
   while (c.l-- > r) *(c.a++) = ' ';		/* blank extend */
   return( r );					/* return to caller */
}

/*
#>            zadd.dc3

Function:     zadd

Purpose:      Converts fchar to ASCIIZ string.

Category:     UTILITY

File:         gclib.c

Author:       J.P. Terlouw

Use:          char *zadd( fchar fc )

              zadd       Returns a pointer to the newly created
                         ASCIIZ string.
              fc         Input fchar. All charcters (discarding
                         trailing blanks and zeroes) are copied to
                         the ASCIIZ string.

Notes:        - zadd is defined in "gipsyc.h".
              - zadd allocates space for the ASCIIZ string with
              malloc, which can be freed with free.
              Therefore the pointer returned by zadd may not be lost, i.e.
              statements like
                 printf( "%s\n", zadd( fc ) );
              must be avoided. One should use instead an intermediate
              variable:
                 char *ptr = zadd( fc );
                 printf( "%s\n", ptr );
                 free( ptr );

Updates:      Feb 27, 1990: KGB, Document created.

#<
*/

char *zadd( fchar string )
{
   char *r;					/* return value */
   fint  l = string.l;				/* length of input string */
   
   while (l-- && (string.a[l] == ' ' || string.a[l] == '\0'));
   l += 1;					/* number of character in r */
   r = calloc( sizeof( char ), l + 1 );
   if (r != NULL) {
      (void) strncpy( r, string.a, l );		/* copy characters to r */
      r[l] = 0;					/* add terminating nul */
   }
   return( r );					/* return to caller */
}
   
#if defined(TESTBED)
int main()
{
   char   b[5];
   char  *p;
   char  *s = "ABCDEFGKIJ";
   fchar  f;
   int    l;
   
   f = tofchar( s );
   printf( "length: %2d,  value: %s|\n", f.l, f.a );
   l = char2str( f, b, 5 );
   printf( "length: %2d,  value: %s      |\n", l, b );
   f.a = b; f.l = 5;
   l = str2char( s, f );
   printf( "length: %2d,  value: %.*s     |\n", l, l, f.a );
   p = zadd( f );
   printf( "length: %2d,  value: %s     |\n", strlen( p ), p );
   free( p );
   return( 0 );
}
#endif
