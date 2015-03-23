/* errorf.c

           Copyright (c) 1991
  Laboratory for Space Research Groningen 
       Kapteyn Laboratory Groningen 
           All Rights Reserved.

#>            errorf.dc2

Function:     errorf

Purpose:      error, using variable arguments

Category:     UTILITY, USER-INTERFACE

Author:       Do Kester

Use:  
void errorf( 
	int	error_level,	IN : errlev as in ERROR
	char	*fmt,		IN : fmt as in printf
	        ... )		IN : variable arguments as in printf

Description:
	The declaration of this function is in "userfio.h",
	where similar functions for anyoutf, statusf and userfxxx
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
#include "error.h"

void errorf( 
	int	error_level,	/* IN : as in error */
	char	*fmt,		/* IN : fmt as in printf */
	        ... )		/* IN : variable arguments as in printf */
{
    fint	err ;
    char	mess[81] ;
    va_list	args ;

    va_start( args, fmt ) ;
    vsprintf( mess, fmt, args ) ;
    err = error_level ;
    error_c( &err, tofchar( mess ) ) ;
    va_end( args ) ;
}

