/* anyoutf.c

           Copyright (c) 1991
  Laboratory for Space Research Groningen 
       Kapteyn Laboratory Groningen 
           All Rights Reserved.

#>            anyoutf.dc2

Function:     anyoutf

Purpose:      anyout using variable arguments

Category:     UTILITY, USER-INTERFACE

Author:       Do Kester

Use:          void anyoutf( int         device, IN : device as in ANYOUT
                            char        *fmt,   IN : fmt as in printf
                            ... )               IN : variable arguments as in printf

Description:  The declaration of this function is in "userfio.h",
              where similar functions for anyoutf, errorf and userfxxx
              are declared too.
              This function uses an internal buffer of 80 characters long.

Comment:      This routine is NOT callable in FORTRAN.

Updates:      06 Aug 1991: DK, Creation date
              04 Jun 1992: DK, into the system.
              Nov 11, 1994: KGB, enlarged text buffer, modified document.
	      25 Oct 1995: DK, DEBUG mode added ANYOUT_DEBUG
#<
*/

#include "stdarg.h"
#include "gipsyc.h"
#include "stdio.h"
#include "anyout.h"
#include "userfio.h"

void anyoutf( 
	int	device,		/* IN : device as in ANYOUT */
	char	*fmt,		/* IN : fmt as in printf */
	        ... )		/* IN : variable arguments as in printf */
{
    fint	dev ;
    char	mess[512] ;
    va_list	args ;

    if ( device == ANYOUT_DEBUG ) return ;

    va_start( args, fmt ) ;
    vsprintf( mess, fmt, args ) ;
    dev = device ;
    anyout_c( &dev, tofchar( mess ) ) ;
    va_end( args ) ;
}

