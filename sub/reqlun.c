/* reqlun.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"error.h"		/* define error_c */

static	fint	luntable[] = {		/* table for logical unit numbers */
   0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

#define	MAXLUN	( sizeof(luntable) / sizeof(fint) )	/* maximum number */



/*
#>            reqlun.dc3

Function:     REQLUN

Purpose:      This routine centralizes the use of FORTRAN logical
              unit numbers in a task. It reserves a logical unit number.

Category:     FILES

File:         reqlun.c

Author:       K.G. Begeman

Use:          CALL REQLUN( LUN )         Input/Output    INTEGER

              LUN           If zero, REQLUN returns a free logical
                            unit number, if positive REQLUN tries
                            to reserve that logical unit number if
                            possible. All other values will cause
                            a fatal error.

Related Docs: rellun.dc3

Updates:      Aug 30, 1990: KGB, document created.

#<

Fortran to C interface:

@ subroutine reqlun( integer )

*/


              
void	reqlun_c( fint *lun )
{
   fint elev = 4;				/* error code (FATAL) */

   if (!(*lun)) {				/* search for free lun */
      while ((*lun) < MAXLUN && luntable[*lun]) (*lun)++;
      if (*lun == MAXLUN) {			/* no lun free */
         error_c( &elev, tofchar( "No free logical unit!" ) );
      } else {
         luntable[*lun] = (*lun) + 1;		/* reserve */
         (*lun)++;				/* increase */
      }
   } else if ((*lun) < 1 || (*lun) > MAXLUN) {	/* lun out of range */
      error_c( &elev, tofchar( "Requested logical unit out of range!" ) ); 
   } else if (luntable[(*lun)-1]) {		/* lun not free */
      error_c( &elev, tofchar( "Requested logical unit not free!" ) );
   } else {					/* reserve lun */
      luntable[(*lun)-1] = *lun;
   }
}



/*
#>            rellun.dc3

Function:     RELLUN

Purpose:      This routine centralizes the use of FORTRAN logical
              unit numbers in a task. It releases a logical unit number.

Category:     FILES

File:         reqlun.c

Author:       K.G. Begeman

Use:          CALL RELLUN( LUN )         Input/Output    INTEGER

              LUN           If positive RELLUN releases the logical
                            unit number, even if it was not reserved.
                            All other values will cause a fatal error.

Related Docs: reqlun.dc3

Updates:      Aug 30, 1990: KGB, document created.

#<

Fortran to C interface:

@ subroutine rellun( integer )

*/



void	rellun_c( fint *lun )
{
   fint elev = 4;				/* error code (FATAL) */

   if ((*lun) < 1 || (*lun) > MAXLUN) {		/* not in range */
      error_c( &elev, tofchar( "Logical unit number outside range!" ) );
   } else {					/* okay */
      luntable[(*lun)-1] = 0;			/* free logical unit number */
   }
}



#if	defined(TESTBED)		/* for testing purposes */

#include	"cmain.h"		/* for C main program */
#include	"init.h"		/* define init_c */
#include	"finis.h"		/* define finis_c */
#include	"userint.h"		/* define userint_c */
#include	"stdlib.h"		/* <stdlib.h> */

MAIN_PROGRAM_ENTRY			/* instead of main() */
{
   fint dlev = 0;
   fint luns[MAXLUN];
   fint n;
   fint nmax = MAXLUN;
   fint nret;

   init_c();
   nret = userint_c( luns, &nmax, &dlev, tofchar("LUNS="), tofchar("Enter luns") );
   for (n = 0; n < nret; n++) {
      reqlun_c( &luns[n] );
      printf( "reqlun = %d\n", luns[n] );
   }
   finis_c();
}
#endif
