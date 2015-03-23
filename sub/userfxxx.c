/* userfxxx.c

           Copyright (c) 1991
  Laboratory for Space Research Groningen 
       Kapteyn Laboratory Groningen 
           All Rights Reserved.

#>            userfxxx.dc2
Function:     userfxxx

Purpose:      Easy-C companions of userxxx_c

Category:     General C; Userinterface

Author:       Do Kester

Use:  
int userfxxx( 
	xxx	*items,		OUT ( see description )
	int     maxitems,	IN  ( is missing in userftext )
	int	default,	IN
	char	*keyword,	IN
	char	*format,	IN format as in printf
		... )		IN variable arguments as in printf

Description:
	Completely analogue to userxxx_c except for the fact that inputs are
	now indeed input only and the string `keyword' is a normal character
	string. The format and the variable arguments are combined into
	a message is passed to userxxx_c. 
        For userfchar, userfcharl userfcharu en userftext the type of
        items is fchar, otherwise it is the type fint*, float*, double*,
        or bool*.
	Note that there need not to be extra arguments, in which case format 
	is passed as message; only beware of the behaviour of `%'s.
        The declaration of this function is in "userfio.h",
        where similar functions for anyoutf, errorf and statusf
        are declared too.
        This function uses an internal buffer of 80 characters long.

Comment:      This routine is NOT callable in FORTRAN.

Updates:      06 Aug 1991: DK, Creation date
	      21 Aug 1991: DK, vararg list added
              04 Jun 1992: DK, into the system.
              25 Oct 1995: DK, userfio.h included as consistency check
#<
*/

#include "gipsyc.h"
#include "userchar.h"
#include "usercharl.h"
#include "usercharu.h"
#include "usertext.h"
#include "userint.h"
#include "userreal.h"
#include "userdble.h"
#include "userangle.h"
#include "userlog.h"
#include "stdarg.h"
#include "stdio.h"
#include "userfio.h"

/*
#>            userfchar.dc2
Function:     userfchar

Purpose:      Easy-C companion of userchar_c

Author:       Do Kester

Use:  
int userfchar( 
	fchar	items,		OUT
	int     maxitems,	IN 
	int	default,	IN
	char	*keyword,	IN
	char	*format,	IN format as in printf
		... )		IN variable arguments as in printf

Description:  See userfxxx.dc2

Comment:      This routine is NOT callable in FORTRAN.
#<
*/
int userfchar( fchar a, int b, int c, char *d, char *fmt, ... )
{
	fint	bb = b, cc = c;
	char	mess[81] ;
	va_list	args ;

	va_start( args, fmt ) ;
	vsprintf( mess, fmt, args ) ;
	va_end( args ) ;
	return( userchar_c( a, &bb, &cc, tofchar( d ), tofchar( mess ) ) ) ;
}

/*
#>            userfcharl.dc2
Function:     userfcharl

Purpose:      Easy-C companion of usercharl_c

Author:       Do Kester

Use:  
int userfcharl( 
	fchar	items,		OUT
	int     maxitems,	IN 
	int	default,	IN
	char	*keyword,	IN
	char	*format,	IN format as in printf
		... )		IN variable arguments as in printf

Description:  See userfxxx.dc2

Comment:      This routine is NOT callable in FORTRAN.
#<
*/
int userfcharl( fchar a, int b, int c, char *d, char *fmt, ... )
{
	fint	bb = b, cc = c;
	char	mess[81] ;
	va_list	args ;

	va_start( args, fmt ) ;
	vsprintf( mess, fmt, args ) ;
	va_end( args ) ;
	return( usercharl_c( a, &bb, &cc, tofchar( d ), tofchar( mess ) ) ) ;
}

/*
#>            userfcharu.dc2
Function:     userfcharu

Purpose:      Easy-C companion of usercharu_c

Author:       Do Kester

Use:  
int userfcharu( 
	fchar	items,		OUT
	int     maxitems,	IN 
	int	default,	IN
	char	*keyword,	IN
	char	*format,	IN format as in printf
		... )		IN variable arguments as in printf

Description:  See userfxxx.dc2

Comment:      This routine is NOT callable in FORTRAN.
#<
*/
int userfcharu( fchar a, int b, int c, char *d, char *fmt, ... )
{
	fint	bb = b, cc = c;
	char	mess[81] ;
	va_list	args ;

	va_start( args, fmt ) ;
	vsprintf( mess, fmt, args ) ;
	va_end( args ) ;
	return( usercharu_c( a, &bb, &cc, tofchar( d ), tofchar( mess ) ) ) ;
}

/*
#>            userftext.dc2
Function:     userftext

Purpose:      Easy-C companion of usertext_c

Author:       Do Kester

Use:  
int userftext( 
	fchar	items,		OUT
	int	default,	IN
	char	*keyword,	IN
	char	*format,	IN format as in printf
		... )		IN variable arguments as in printf

Description:  See userfxxx.dc2

Comment:      This routine is NOT callable in FORTRAN.
#<
*/
int userftext( fchar a, int c, char *d, char *fmt, ... )
{
	fint	cc = c;
	char	mess[81] ;
	va_list	args ;

	va_start( args, fmt ) ;
	vsprintf( mess, fmt, args ) ;
	va_end( args ) ;
	return( usertext_c( a, &cc, tofchar( d ), tofchar( mess ) ) ) ;
}

/*
#>            userfint.dc2
Function:     userfint

Purpose:      Easy-C companion of userint_c

Author:       Do Kester

Use:  
int userfint( 
	fint	*items,		OUT
	int     maxitems,	IN 
	int	default,	IN
	char	*keyword,	IN
	char	*format,	IN format as in printf
		... )		IN variable arguments as in printf

Description:  See userfxxx.dc2

Comment:      This routine is NOT callable in FORTRAN.
#<
*/
int userfint( fint *a, int b, int c, char *d, char *fmt, ... )
{
	fint	bb = b, cc = c;
	char	mess[81] ;
	va_list	args ;

	va_start( args, fmt ) ;
	vsprintf( mess, fmt, args ) ;
	va_end( args ) ;
	return( userint_c( a, &bb, &cc, tofchar( d ), tofchar( mess ) ) ) ;
}

/*
#>            userfreal.dc2
Function:     userfreal

Purpose:      Easy-C companion of userreal_c

Author:       Do Kester

Use:  
int userfreal( 
	real	*items,		OUT
	int     maxitems,	IN 
	int	default,	IN
	char	*keyword,	IN
	char	*format,	IN format as in printf
		... )		IN variable arguments as in printf

Description:  See userfxxx.dc2

Comment:      This routine is NOT callable in FORTRAN.
#<
*/
int userfreal( float *a, int b, int c, char *d, char *fmt, ... )
{
	fint	bb = b, cc = c;
	char	mess[81] ;
	va_list	args ;

	va_start( args, fmt ) ;
	vsprintf( mess, fmt, args ) ;
	va_end( args ) ;
	return( (int)userreal_c( a, &bb, &cc, tofchar( d ), tofchar( mess ) ) ) ;
}

/*
#>            userfdble.dc2
Function:     userfdble

Purpose:      Easy-C companion of userdble_c

Author:       Do Kester

Use:  
int userfdble( 
	double	*items,		OUT
	int     maxitems,	IN 
	int	default,	IN
	char	*keyword,	IN
	char	*format,	IN format as in printf
		... )		IN variable arguments as in printf

Description:  See userfxxx.dc2

Comment:      This routine is NOT callable in FORTRAN.
#<
*/
int userfdble( double *a, int b, int c, char *d, char *fmt, ... )
{
	fint	bb = b, cc = c;
	char	mess[81] ;
	va_list	args ;

	va_start( args, fmt ) ;
	vsprintf( mess, fmt, args ) ;
	va_end( args ) ;
	return( userdble_c( a, &bb, &cc, tofchar( d ), tofchar( mess ) ) ) ;
}

/*
#>            userfangle.dc2
Function:     userfangle

Purpose:      Easy-C companion of userangle_c

Author:       Do Kester

Use:  
int userfangle( 
	double	*items,		OUT
	int     maxitems,	IN 
	int	default,	IN
	char	*keyword,	IN
	char	*format,	IN format as in printf
		... )		IN variable arguments as in printf

Description:  See userfxxx.dc2

Comment:      This routine is NOT callable in FORTRAN.
#<
*/
int userfangle( double *a, int b, int c, char *d, char *fmt, ... )
{
	fint	bb = b, cc = c;
	char	mess[81] ;
	va_list	args ;

	va_start( args, fmt ) ;
	vsprintf( mess, fmt, args ) ;
	va_end( args ) ;
	return( userangle_c( a, &bb, &cc, tofchar( d ), tofchar( mess ) ) ) ;
}

/*
#>            userflog.dc2
Function:     userflog

Purpose:      Easy-C companion of userlog_c

Author:       Do Kester

Use:  
int userflog( 
	bool	*items,		OUT
	int     maxitems,	IN 
	int	default,	IN
	char	*keyword,	IN
	char	*format,	IN format as in printf
		... )		IN variable arguments as in printf

Description:  See userfxxx.dc2

Comment:      This routine is NOT callable in FORTRAN.
#<
*/
int userflog( bool *a, int b, int c, char *d, char *fmt, ... )
{
	fint	bb = b, cc = c;
	char	mess[81] ;
	va_list	args ;

	va_start( args, fmt ) ;
	vsprintf( mess, fmt, args ) ;
	va_end( args ) ;
	return( userlog_c( a, &bb, &cc, tofchar( d ), tofchar( mess ) ) ) ;
}

