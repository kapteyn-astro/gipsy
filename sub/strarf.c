/* strarf.c

           Copyright (c) 1995
  Laboratory for Space Research Groningen 
       Kapteyn Laboratory Groningen 
           All Rights Reserved.

#>            strarf.dc2

Function:     strarf

Purpose:      print array of int|long|char|float|double|pointer into a string

Category:     general C-programs

Author:       Do Kester

Use:          char *strarf( char   *mess,  IN : large enough to contain the 
						formatted array
                            char   *fmt,   IN : fmt as in printf for ONE element
                            			of the array 
                            void   *pX,    IN : the array to be formatted
                            long   nr,	   IN : nr of elements to be formatted
                            long   step )  IN : stepsize through the array

Returns:      pointer to mess 

Description:  It is decided what kind of array pX is by looking at the fmt.
	      This fmt string is very similar to the printf-format, with 
	      the exception that it has to discriminate between int and long
	      and between float and double, when those are different.
	        fmt	array
	        %i	int
	        %u	unsigned int
	        %d	long
	        %c	char
	        %efg	float
	        %EG	double
	        %p	pointer 
	      All standard flags and field-width specifiers of C are allowed 
	      between the % and the conversion character. 
	      
	      The declaration of this function is in "userfio.h"

Example:      float	*array = { 1.0, 2.0, 3.0, 4.0 } ;
	      anyoutf( 0, "array = %s", strarf( mess, "%5.1f", array, 2, 2 ) );
	      prints
	      array =   1.0  3.0

Comment:      This routine is NOT callable in FORTRAN.

Updates:      25 Oct 1995: DK, Creation date

#<
*/

#include "string.h"
#include "ctype.h"
#include "gipsyc.h"
#include "stdio.h"
#include "userfio.h"

static int decode_fmt( char* ) ;

char *strarf( 
	char	*mess,
	char	*fmt,
	void	*pX,
	long	nr,
	long	step )
{
    long	*pL, k = 0 ;
    int		*pI, code ;
    char	*pC ;
    float	*pF ;
    double	*pD ;
    void	**pP ;

    code = decode_fmt( fmt ) ;
    if ( code == 1 ) {
      pI = (int*)pX ;
      while ( nr-- ) {
    	sprintf( &mess[k], fmt, *pI ) ;
    	pI += step ;
        k  = strlen( mess ) ;
      }
    } else if ( code == 2 ) {
      pC = (char*)pX ;
      while ( nr-- ) {
    	sprintf( &mess[k], fmt, *pC ) ;
    	pC += step ;
        k  = strlen( mess ) ;
      }
    } else if ( code == 3 ) {
      pL = (long*)pX ;
      while ( nr-- ) {
    	sprintf( &mess[k], fmt, *pL ) ;
    	pL += step ;
        k  = strlen( mess ) ;
      }
    } else if ( code == 4 ) {
      pF = (float*)pX ;
      while ( nr-- ) {
    	sprintf( &mess[k], fmt, *pF ) ;
    	pF += step ;
        k  = strlen( mess ) ;
      }
    } else if ( code == 5 ) {
      pD = (double*)pX ;
      while ( nr-- ) {
    	sprintf( &mess[k], fmt, *pD ) ;
    	pD += step ;
        k  = strlen( mess ) ;
      }
    } else if ( code == 6 ) {
      pP = (void**)pX ;
      while ( nr-- ) {
    	sprintf( &mess[k], fmt, *pP ) ;
    	pP += step ;
        k  = strlen( mess ) ;
      }
    } else sprintf( mess, "Can not interpret format: %s", fmt ) ;
    return mess ;
}

static int decode_fmt(
	char	*fmt )
{
	while ( *fmt && *fmt != '%' ) fmt++ ;
	while ( *fmt && ! isalpha( *fmt ) ) fmt++ ;
	if ( *fmt == 'i' || *fmt == 'u' ) return 1 ;
	if ( *fmt == 'c' ) return 2 ;
	if ( *fmt == 'd' ) return 3 ;
	if ( *fmt == 'e' || *fmt == 'f' || *fmt == 'g' ) return 4 ;
	if ( *fmt == 'E' || *fmt == 'G' ) return 5 ;
	if ( *fmt == 'p' ) return 6 ;
	return 0 ;
}
