#include "signal.h"
#include "taskcom.h"
#include "error.h"

static int mask, critdep=0;   

/*
#>            enterc.dc2
Subroutine:   ENTERC

Purpose:      Postpone the effectuation of user aborts until a corresponding
              call to LEAVEC.
              
Category:     SYSTEM 
              
File:         enterc.c

Author:       J.P. Terlouw

Use:          CALL ENTERC

Related document:
              leavec.dc2

Updates:      21-May-86 original document.
              29-Nov-91 portable version, rewritten in C.
              20-Aug-93 error in depth administration corrected.
              31-Jan-94 protection against task suspension added
               7-Jul-94 protection against hangup signal added 
#<
@ subroutine enterc ( )
*/
void enterc_c( void )
{
   if (!critdep++) mask = sigblock(  1<<(SIGABORT-1) 
                                   | 1<<(SIGTSTP-1)
                                   | 1<<(SIGHUP-1));
}

/*
#>            leavec.dc2
Subroutine:   LEAVEC

Purpose:      Allow user aborts again after a call to ENTERC.

Category:     SYSTEM 
              
File:         enterc.c

Author:       J.P. Terlouw

Use:          CALL LEAVEC

Notice:       A fatal error is generated when there is no corresponding call to
              ENTERC. This means that calls to ENTERC and LEAVEC behave like
              brackets that must match.

Related document:
              enterc.dc2

Updates:      21-May-86 original document.
              29-Nov-91 portable version, rewritten in C.
#<
@ subroutine leavec ( )
*/
void leavec_c( void )
{
   fint fatal=4;
   
   if (critdep--) {
      if (!critdep) {
         (void)sigsetmask(mask);
      }
   } else {
      error_c(&fatal,tofchar("LEAVEC -- inconsistent critical section depth"));
   }
}
