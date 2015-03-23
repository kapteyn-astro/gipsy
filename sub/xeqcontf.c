/* xeqcontf.c
                              COPYRIGHT (c) 2001
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>            xeqcontf.dc2
Function:     xeqcontf

Purpose:      xeqcont, using variable arguments

Category:     USER-INTERFACE

Author:       J.P. Terlouw

Use:          int xeqcontf(
                          char *key,  IN : status return keyword
                          char *fmt,  IN : fmt encoding command as in printf
                          ...         IN : variable arguments as in printf
                        )

Description:
              The declaration of this function is in "userfio.h",
              where similar functions for statusf, anyoutf, errorf, wkeyf and
              userfxxx are declared also.
              This routine is NOT callable in FORTRAN.
              
Example:      ok = xeqcontf("FPSTATUS=", "FUNPLOT FUNCTION=A*sin(F*x) A=1 F=2");

Related doc.: xeqcont.dc2

Updates:      Jan 17, 2001: JPT, Document created.
#<
*/

#include "stdarg.h"
#include "stdio.h"
#include "gipsyc.h"
#include "taskcom.h"
#include "xeqcont.h"
#include "userfio.h"

int xeqcontf(char *key, char *fmt, ... )
{
    char	command[CMDLEN];
    va_list	args ;

    va_start( args, fmt ) ;
    vsprintf( command, fmt, args ) ;
    va_end( args ) ;
    return xeqcont_c(tofchar(key), tofchar(command));
}
