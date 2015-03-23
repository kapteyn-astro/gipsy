/* nelc.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            nelc.dc2

Function:     NELC

Purpose:      The integer function NELC finds the number of characters
              in a fortran character string discarding trailing blanks.

Category:     TEXT

File:         nelc.c

Author:       K.G Begeman

Use:          INTEGER NELC( STRING )     Input   character*(*)

              NELC        Returns the number of characters in STRING
                          discarding trailing blanks.
              STRING      Character string for which to determine the
                          number of characters.

Updates:      May 23, 1989: KGB, Creation date.

#<

Fortran to C interface:

@ integer function nelc( character )

*/

#include "stdio.h"
#include "string.h"
#include "gipsyc.h"

fint nelc_c( fchar arg )
{
   fint l = arg.l;

   while (l && (arg.a[l-1] == ' ' || arg.a[l-1] == 0)) l--;
   return( l );
}

#if defined(TESTBED)
void main()
{
   char  adr[20];
   fint  len = 20;
   fchar string;

   string.a = adr;
   string.l = len;
   strncpy( adr, "          ", 20) ;
   printf( "adr:%s, nelc_c:%5ld\n", adr, nelc_c(string) );
   strncpy( adr, "0123456789", 20 );
   printf( "adr:%s, nelc_c:%5ld\n", adr, nelc_c(string) );
   strncpy( adr, "012345    ", 20 );
   printf( "adr:%s, nelc_c:%5ld\n", adr, nelc_c(string) );
}
#endif
