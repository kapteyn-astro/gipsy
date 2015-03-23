/* chrint.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */

#define	MAXCHR	(3*sizeof(fint)+1)	/* this is large enough */

/*
#>            chrint.dc2

Function:     CHRINT

Purpose:      Converts an integer to a character string. The integer is
              left-adjusted encoded.

Category:     UTILITY

File:         chrint.c

Author:       K.G. Begeman

Use:          CHARACTER*(*) CHRINT( NUMBER )    Input   INTEGER

              CHRINT      Returns NUMBER as left-adjusted character
                          string.
              NUMBER      Integer which will be leftajusted encoded.
                          If NUMBER is too large to fit in the declared
                          size of CHRINT, the output will be filled
                          with '*'s.

Updates:      May 23, 1989, KGB, Document created.

#<

@ character function chrint( integer )

*/

void chrint_c( fchar chr, fint *val )
{
   char buffer[MAXCHR+1];
   fint l, i = 0;

   l = sprintf( buffer, "%d", *val );
   if (l > chr.l) {
      for (; i < chr.l; chr.a[i++] = '*');	/* fill with asterixes */
   } else {
      for (; i < l; chr.a[i] = buffer[i], i++);	/* copy encode string */
      for (; i < chr.l; chr.a[i++] = ' ');	/* trailing blanks */
   }
}

#if defined(TESTBED)
void main()
{
   fchar chr;
   char  b[3];
   fint  k;

   chr.a = b; chr.l = 3;
   k = 1; chrint_c(chr,&k); printf("Must be:1  ,is:%.3s\n", b);
   k = 12; chrint_c(chr,&k); printf("Must be:12 ,is:%.3s\n", b);
   k = 123; chrint_c(chr,&k); printf("Must be:123,is:%.3s\n", b);
   k = 1234; chrint_c(chr,&k); printf("Must be:***,is:%.3s\n", b);
}
#endif
