/* rejectf.c

                              COPYRIGHT (c) 1998
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>            rejectf.dc2
Function:     rejectf

Purpose:      reject, using variable arguments

Category:     USER-INTERFACE

Author:       J.P. Terlouw

Use:          void rejectf(
                            char *key,  IN : user input keyword
                            char *fmt,  IN : fmt as in printf
                            ...         IN : variable arguments as in printf
                           )

Description:
              The declaration of this function is in "userfio.h",
              where similar functions for statusf, anyoutf, errorf and userfxxx
              are declared too.
              This routine is NOT callable in FORTRAN.
              
Example:
              rejectf("FREQUENCY=", "Illegal frequency");

Related doc.: reject.dc2

Updates:      Apr 21, 1998: JPT, Document created.
#<
*/


#include "stdarg.h"
#include "stdio.h"
#include "gipsyc.h"
#include "taskcom.h"
#include "reject.h"
#include "userfio.h"

void rejectf(char *key, char *fmt, ... )
{
    char	buffer[REJLEN];
    va_list	args ;

    va_start( args, fmt ) ;
    vsprintf( buffer, fmt, args ) ;
    reject_c( tofchar(key), tofchar( buffer ) ) ;
    va_end( args ) ;
}

