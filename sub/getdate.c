/* getdate.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            getdate.dc2

Function:     GETDATE

Purpose:      Returns the current time and date as a text string.

Category:     UTILITY

File:         getdate.c

Author:       K.G. Begeman

Use:          CALL GETDATE( STRING )         Input     CHARACTER*24

              STRING           Date and time in the form:
                               WDY MON dd hh:mm:ss YEAR

Updates:      Apr 27, 1991: KGB Document created

#<

Fortran to C interface:

@ subroutine getdate( character )

*/
#include	"gipsyc.h"		/* GIPSY definitions */
#include	"stdio.h"		/* <stdio.h> */
#include	"time.h"		/* <time.h> */

void	getdate_c( fchar date )
{
   char	        *ptr;
   int	        n = 0;
   time_t       now;

   now = time( NULL );
   ptr = ctime( &now );
   while ( ( n < date.l ) && ( n < 24 ) ) date.a[n++] = *ptr++;
   while ( n < date.l ) date.a[n++] = ' ';
}

#if	defined(TESTBED)
int	main( )
{
   char		buf[30];
   fchar	date;

   date.a = buf; date.l = sizeof( buf );
   getdate_c( date );
   printf( "Current date: %.*s\n", date.l, date.a );
   return( 0 );
}
#endif
