/*  match.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            match.dc2

Function:     MATCH

Purpose:      Does a minimal match on strings (not case sensitive).

Category:     STRINGS

File:         match.c

Author:       K.G. Begeman

Use:          INTEGER MATCH( LIST  ,    Input     CHARACTER*(*) ARRAY
                             NLIST ,    Input     INTEGER
                             TEST  )    Input     CHARACTER*(*)

              MATCH      Returns:
                         >0: Item number in LIST which is matched by TEST
                          0: No match
                         -1: Multiple matching items in LIST
              LIST       Character array containing strings which will
                         be compared with TEST
              NLIST      Number of items in LIST
              TEST       Character string  to be compared to all items
                         in LIST

Updates:      Sep 20, 1990: KGB, Document created.

#<

Fortran to C interface:

@ integer function match( character, integer, character )

*/

#include        "stdio.h"               /* <stdio.h> */
#include        "string.h"              /* <string.h> */
#include        "ctype.h"               /* <ctype.h> */
#include        "gipsyc.h"              /* GIPSY symbols and definitions */

fint    match_c( fchar  list  ,		/* list of items to match with */
                 fint  *nlist ,		/* number of items in list */
                 fchar  test  )		/* item to compare with list */
{
   char *lptr;                                  /* pointer to list */
   char *tptr;                                  /* pointer to test */
   fint  r = 0;                                 /* return value */
   int   l;                                     /* list counter */
   int   m;                                     /* match counter */
   int   mstart;                                /* position to start matching */

   lptr = list.a;                               /* initialize list pointer */
   tptr = test.a;                               /* initialize test pointer */
   mstart = test.l;                             /* initialize mstart */
   while (mstart && (tptr[mstart-1] == ' ')) mstart--;
   if (!mstart) mstart++;                       /* increase */
   if (mstart > list.l) mstart = list.l;        /* decrease */
   for (l = 0; l < (*nlist); l++) {		/* loop */
      m = mstart;                               /* initialize match counter */
      while (m && (tolower(lptr[m-1]) == tolower(tptr[m-1]))) m--;
      if (!m) {                                 /* matches */
         if (!r) r = l + 1; else r = -1;        /* first match or ... */
      }
      lptr += (list.l);                         /* increase list pointer */
   }
   return( r );                                 /* return to caller */
}

#if     defined(TESTBED)

int main( int argc, char *argv[] )
{
   char  listb[1024];
   fchar list;
   fchar test;
   fint  m;
   fint  nlist;
   int   l;

   strcpy( listb, "bool      " );
   strcat( listb, "char      " );
   strcat( listb, "double    " );
   strcat( listb, "float     " );
   strcat( listb, "int       " );
   strcat( listb, "long      " );
   strcat( listb, "short     " );
   strcat( listb, "lang      " );
   nlist = 8;
   list.a = listb; list.l = 10;
   for (l = 1; l < argc; l++) {
      test.a = argv[l];
      test.l = strlen( argv[l] );
      m = match_c( list, &nlist, test );
      printf( "match = %ld ", m );
      if (m > 0) {
         printf( " (%.10s with %s) ", &listb[(m-1)*10], test.a );
      }
      printf( "\n" );
   }
   return( 0 );
}

#endif
