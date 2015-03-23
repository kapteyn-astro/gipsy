/* wkeyf.c

                              COPYRIGHT (c) 1998
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>            wkeyf.dc2
Function:     wkeyf

Purpose:      wkey, using variable arguments

Category:     USER-INTERFACE

Author:       J.P. Terlouw

Use:          void wkeyf(
                          char *fmt,  IN : fmt as in printf
                          ...         IN : variable arguments as in printf
                        )

Description:
              The declaration of this function is in "userfio.h",
              where similar functions for statusf, anyoutf, errorf and userfxxx
              are declared too.
              This routine is NOT callable in FORTRAN.
              
Example:
              wkeyf("FREQUENCY=%f BEAM=%f %f", freq, major, minor);
                   //  This call sends two keyword-value pairs to Hermes.

Related doc.: wkey.dc2

Updates:      Apr 21, 1998: JPT, Document created.
#<
*/


#include "stdarg.h"
#include "stdio.h"
#include "gipsyc.h"
#include "taskcom.h"
#include "wkey.h"
#include "userfio.h"

void wkeyf(char *fmt, ... )
{
    char	buffer[CMDLEN];
    va_list	args ;

    va_start( args, fmt ) ;
    vsprintf( buffer, fmt, args ) ;
    wkey_c( tofchar( buffer ) ) ;
    va_end( args ) ;
}

