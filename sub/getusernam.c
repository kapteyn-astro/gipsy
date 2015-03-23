/* getusernam.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            getusernam.dc2

Function:     GETUSERNAM

Purpose:      Returns the user name of the current user.

Category:     UTILITY

File:         getusernam.c

Author:       M. Vogelaar

Use:          CALL GETUSERNAM( USERNAM )     Output      CHARACTER*(*)

              USERNAM        Name of user currently logged in.

Warning:      System dependent! Currently implemented for UNIX systems.

Updates:      Oct 9, 1992: VOG Document created.

#<

Fortran to C interface:

@ subroutine getusernam( character )

*/

#include	"stddef.h"		/* <stddef.h> */
#include	"stdio.h"		/* <stdio.h> */

#if	defined(__GNUC__) && defined(_CONVEX_SOURCE)
#define	fgetpwent(x)	fgetpwent(FILE *)
#endif

#define	getpwuid	GETPWUID

#include	<pwd.h>			/* pwd definitions */

#undef	getpwuid

#include	"gipsyc.h"		/* GIPSY definitions */

void	getusernam_c( fchar usernam )
{
   int	n = 0;
#if	defined(__unix__)
   char	                *ptr;
   int   		getuid( );
   struct passwd	*pw, *getpwuid( );

   pw = getpwuid( getuid( ) );
   ptr = pw->pw_gecos;
   while ( ( n < usernam.l ) && ( *ptr ) && (*ptr != ',') ) usernam.a[n++] = *ptr++;
#else
   ???
#endif
   while ( n < usernam.l ) usernam.a[n++] = ' ';
}

#if	defined(TESTBED) 
int	main( )
{
   char		buf[60];
   fchar	usernam;

   usernam.a = buf; usernam.l = sizeof( buf);
   getusernam_c( usernam );
   printf( "Logged user name: %.*s\n", usernam.l, usernam.a );
   return( 0 );
}
#endif 
