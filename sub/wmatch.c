/* wmatch.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            wmatch.dc2

Function:     WMATCH

Purpose:      Matches a test string with the mask string which contains
              wildcards.

Category:     UTILITY

File:         wmatch.c

Author:       K.G. Begeman

Use:          INTEGER WMATCH( TEST ,         Input       CHARACTER*(*)
                              MASK ,         Input       CHARACTER*(*)
                              WILDCARD ,     Input       CHARACTER*1
                              CASE )         Input       INTEGER

              WMATCH       Returns non-zero if matched, zero if not.
              TEST         String to test agains MASK.
              MASK         String which contains the mask with wildcards.
              WILDCARD     Character which represents the wildcard.
              CASE         If non-zero matching is case sensitive,
                           if zero matching is case insensitive.

Updates:      Mar  8, 1991: KGB Document created
              Nov 12, 1992: KGB Bug repaired
              Jan 18, 1993: VOG printf removed
              Aug 18, 1993: VOG { r = 1; break; } replaced by return(1);
                            in inner loop.

#<

Fortran to C interface:

@ integer function wmatch( character, character, character, integer )

*/

#include	"ctype.h"			/* <ctype.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */
#include	"gipsyc.h"			/* GIPSY definitions */


fint	wmatch_c( fchar	TEST ,			/* test string */
                  fchar	MASK ,			/* mask string */
                  fchar	WILD ,			/* wilcard character */
                  fint	*CASE )			/* case switch */
{
   char	*test, *mask, wild;			/* local variables */
   int	m = 0, t = 0, r = 0;			/* counters and return value */

   test = zadd( TEST ); mask = zadd( MASK );	/* make ASCIIZ strings */
   if (!(*CASE)) {				/* not case sensitive */
      int	i;				/* loop counter */

      for (i = 0; test[i]; test[i] = toupper( test[i] ), i++ );
      for (i = 0; mask[i]; mask[i] = toupper( mask[i] ), i++ );
      wild = toupper(WILD.a[0]);		/* get wildcard */
   } else {
      wild = WILD.a[0];				/* get wildcard */
   }
   while (mask[m]) {				/* main loop */
      int	mi = m, ti = t, om = 0;		/* old counters */

      r = 0;					/* reset */
      while (mask[m+om] == wild) om++;		/* skip wildcards */
      if (om && !mask[m+om]) { r = 1; break; }	/* matched, so we quit */
      m += om;					/* update counter */
      if (!m && !t) {				/* nothing processed yet */
         while (mask[m] == test[t]) {		/* compare */
            t += 1; m += 1;			/* update counters */
            if (!mask[m-1]) {           	/* matched, and both chars. must */
                                                /* be \0, so we quit */
               free( test ); free( mask );      /* free ASCIIZ strings */
               return(1);
            }
         }
         if (mask[m] == wild) r = 1;		/* prepare for next run */
      } else {					/* already started */
         int	c = 1;				/* inner loop control */

         while (c) {				/* inner loop */
            while (mask[m] != test[t++]) {	/* find next match */
               if (!test[t]) { c = 0; break; }	/* end of test string */
            }
            if (!c) break;			/* leave inner loop */
            mi = m; m += 1; ti = t;		/* update counters */
            while (mask[m] == test[t]) {	/* compare loop */
               if (!mask[m]) { c = 0; break; }	/* end of mask */
               m += 1; t += 1;			/* update counters */
            }
            if (mask[m] == wild) { c = 0; }	/* set leave flag */
            if (!c) { r = 1; break; }		/* prepare for next run */
            m = mi; t = ti;			/* update counters */
         }
      }
      if (!r) break;				/* leave outer loop */
   }
   free( test ); free( mask );			/* free ASCIIZ strings */
   return( r );					/* return to caller */
}

#if	defined(TESTBED)

int main( int argc, char *argv[] )
{
   fint	cs;

   cs = 1;
   printf( "wmask = %d\n", (int) wmatch_c( tofchar(argv[1]), tofchar(argv[2]), tofchar(argv[3]), &cs ) );
   cs = 0;
   printf( "wmask = %d\n", (int) wmatch_c( tofchar(argv[1]), tofchar(argv[2]), tofchar(argv[3]), &cs ) );
   return( 0 );
}

#endif
