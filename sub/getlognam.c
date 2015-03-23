/* getlognam.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            getlognam.dc2

Function:     GETLOGNAM

Purpose:      Returns the log name of the current user.

Category:     UTILITY

File:         getlognam.c

Author:       K.G. Begeman

Use:          CALL GETLOGNAM( LOGNAM )     Output      CHARACTER*(*)

              LOGNAM        Name of user currently logged in.

Warning:      System dependent! Currently implemented for UNIX systems.

Updates:      Apr 27, 1991: KGB Document created.

#<

Fortran to C interface:

@ subroutine getlognam( character )

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

void	getlognam_c( fchar lognam )
{
   int	n = 0;
#if	defined(__unix__)
   char	*getlogin( );
   char	*ptr;

   ptr = getlogin( );
   if ( ptr == NULL ) {
      int		getuid( );
      struct passwd	*pw, *getpwuid( );

      pw = getpwuid( getuid( ) );
      ptr = pw->pw_name;
   }
   while ( ( n < lognam.l ) && ( *ptr ) ) lognam.a[n++] = *ptr++;
#else
   ???
#endif
   while ( n < lognam.l ) lognam.a[n++] = ' ';
}

#if	defined(TESTBED)
int	main( )
{
   char		buf[60];
   fchar	lognam;

   lognam.a = buf; lognam.l = sizeof( buf);
   getlognam_c( lognam );
   printf( "Logged user: %.*s\n", lognam.l, lognam.a );
   return( 0 );
}
#endif
