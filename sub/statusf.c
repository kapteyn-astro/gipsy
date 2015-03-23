/* statusf.c

           Copyright (c) 1991
  Laboratory for Space Research Groningen 
       Kapteyn Laboratory Groningen 
           All Rights Reserved.

#>            statusf.dc2
Function:     statusf

Purpose:      status, using variable arguments

Category:     General C-programs

Author:       Do Kester		do@guspace.rug.nl

Use:  
void statusf( 
	char	*fmt,		IN : fmt as in printf
	        ... )		IN : variable arguments as in printf

Description:
        The declaration of this function is in "userfio.h",
        where similar functions for anyoutf, errorf and userfxxx
        are declared too.
	This function uses an internal buffer of 80 characters long.

Comment:      This routine is NOT callable in FORTRAN.

Updates:      06 Aug 1991: DK, Creation date
	      04 Jun 1992: DK, into the system.
#<
*/

#include "stdarg.h"
#include "gipsyc.h"
#include "stdio.h"
#include "status.h"

void statusf( 
	char	*fmt,		/* IN : fmt as in printf */
	        ... )		/* IN : variable arguments as in printf */
{
    char	mess[81] ;
    va_list	args ;

    va_start( args, fmt ) ;
    vsprintf( mess, fmt, args ) ;
    status_c( tofchar( mess ) ) ;
    va_end( args ) ;
}

