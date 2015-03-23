/* present.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            present.dc2

Document:     present

Purpose:      Describes the use of undefined arguments in subroutine
              and function calls.

Category:     UTILITY

File:         present.c

Author:       K.G. Begeman

Description:  To avoid the so called comma-comma construct which is
              accepted by some (not ALL) fortran compilers, the
              PRESENTN and PRESENTC routines were developped. These
              routines check the address of the argument against
              the address of a variable in a certain common block in
              the calling routine. If the addresses are equal, i.e.
              the value in the common block is passed as an argument
              to the procedure, the argument is called undefined
              (the same thing would be achieved by leaving out the
              argument when the compiler accepts the comma-comma
              construct). The common blocks reserved for this
              solution are common /NOARGN/ for numeric variables and
              common /NOARGC/ for character variables.
              C programmers can use the 'standard' method of putting
              the NULL pointer in the argument list for numeric
              arguments or a (fchar) character string with address NULL
              for character arguments.
              The routines PRESENTN and PRESENTC return false if the
              numeric or character argument is the first item in
              resp. common /NOARGN/ or common /NOARGC/, otherwise
              true is returned. Each of these routines is described
              in more detail in their DC2 documents.

Warning:      System dependent! At the moment it works on VMS, SUN,
              CONVEX, HP 9000, SONY, DECstations, ALLIANT, ALPHA, CRAY
              systems.

Updates:      Jul 30, 1989: KGB, original document.
              Apr 23, 1991: KGB, implemented for HP.
              Sep 22, 1994: KGB, implemented for CRAY.

#<

*/

#include	"stddef.h"			/* <stddef.h> */
#include 	"stdio.h"			/* <stdio.h> */
#include 	"gipsyc.h"			/* GIPSY defines */

/* Here comes the system dependent part */
#if	defined(__F2C__)			/* standard (?) method */
#define		NO_ARG_N noargn_		/* C name of common NOARGN */
#define		NO_ARG_C noargc_		/* C name of common NOARGC */
#elif	defined(__vms__)			/* DEC VMS */
#define		NO_ARG_N noargn			/* C name of common NOARGN */
#define		NO_ARG_C noargc			/* C name of common NOARGC */
#elif	defined(__convex__)			/* CONVEX */
#define		NO_ARG_N _noargn_		/* C name of common NOARGN */
#define		NO_ARG_C _noargc_		/* C name of common NOARGC */
#elif	defined(__cray__)			/* CRAY */
#define		NO_ARG_N NOARGN			/* C name of common NOARGN */
#define		NO_ARG_C NOARGC			/* C name of common NOARGC */
#elif	defined(__hpux__)			/* HP 9000 */
#define		NO_ARG_N noargn			/* C name of common NOARGN */
#define		NO_ARG_C noargc			/* C name of common NOARGC */
#else						/* EVERYTHING ELSE */
#define		NO_ARG_N noargn_		/* C name of common NOARGN */
#define		NO_ARG_C noargc_		/* C name of common NOARGC */
#endif

extern		fint NO_ARG_N;			/* link with common NOARGN */
extern		char NO_ARG_C;			/* link with common NOARGC */

fint NO_ARG_N;				/* declare the variable in NOARGN */
char NO_ARG_C;				/* declare the variable in NOARGC */

/*

#>            presentn.dc2

Function:     presentn

Purpose:      Logical function checking the 'presence' of a numeric
              argument in a subroutine/function call.

Category:     UTILITY

File:         present.c

Author:       K.G. Begeman

Use:          LOGICAL PRESENTN( NUMARG )   Input   any type except character

              PRESENTN  Returns true if argument is present, false if not.
              NUMARG    Numeric argument in argument list which is
                        checked for its presence.

Description:  PRESENTN is intended to be used in routines which perform
              a different action depending on the 'presence' or
              'absence' of certain numeric arguments.
              In C procedures a numeric argument is considered not
              present if the calling routine/program puts NULL instead
              of a pointer to a variable in the argument list, in
              FORTRAN or SHELTRAN one has to put the first element of
              the common block NOARGN in the argument list.

Example:      C     SHELTRAN example to demonstrate use of presentn

                    PROGRAM MAINF
                    INTEGER NULLN
                    COMMON /NOARGN/ NULLN
                    ...
                    CALL SUB( NULLN )
                    ...
                    STOP
                    END
                    SUBROUTINE SUB( ARG )
                    REAL ARG
                    LOGICAL PRESENTN
                    IF (PRESENTN( ARG ))
                    THEN
                       ...
                    CIF
                    RETURN
                    END

              C example to demonstrate use of presentn
              #include "gipsyc.h"
              #include "presentn.h"

              void sub( float *arg )
              {
                 if (presentn_c( arg )) {
                    ...
                 }
              }

              main()
              {
                 ...
                 sub( NULL );
                 ...
              }

Warnings:     System dependent!

Notes:        For character arguments use PRESENTC.

Updates:      Jul 21, 1988: JPT, Original document.
              Jul 12, 1989: KGB, Converted to ANSI C.
              Apr 23, 1991: KGB, implemented for HP.

#<

@ logical function presentn( integer )

*/

bool presentn_c( void *argn )
{
   if ((argn == NULL) || (argn == &NO_ARG_N)) return( FALSE ); else return( TRUE );
}

/*

#>            presentc.dc2

Function:     presentc

Purpose:      Logical function checking the 'presence' of a character
              argument in a subroutine/function call.

Category:     UTILITY

File:         present.c

Author:       K.G. Begeman

Use:          LOGICAL PRESENTC( CHARARG )   Input    CHARACTER*(*)

              PRESENTC  Returns true is argument is present, false if not.
              CHARARG   Character argument in argument list which is
                        checked for its presence.

Description:  PRESENTC is intended to be used in routines which perform
              a different action depending on the 'presence' or
              'absence' of certain character arguments.
              In C procedures a character argument is considered not
              present if the calling routine/program assigns NULL to
              the character pointer of the fchar struct, in
              FORTRAN or SHELTRAN one has to put the first element of
              the common block NOARGC in the argument list.

Example:      C     SHELTRAN example to demonstrate use of presentc

                    PROGRAM MAINF
                    CHARACTER NULLC
                    COMMON /NOARGC/ NULLC
                    ...
                    CALL SUB( NULLC )
                    ...
                    STOP
                    END
                    SUBROUTINE SUB( ARG )
                    CHARACTER*(*) ARG
                    LOGICAL PRESENTC
                    IF (PRESENTC( ARG ))
                    THEN
                       ...
                    CIF
                    RETURN
                    END

              C example to demonstrate use of presentc
              #include "gipsyc.h"
              #include "presentc.h"

              void sub( fchar arg )
              {
                 if (presentc_c( arg )) {
                    ...
                 }
              }

              main()
              {
                 static fchar nullc = { NULL, 0 };
                 ...
                 sub( nullc );
                 ...
              }

Warnings:     System dependent!

Notes:        Use PRESENTN for numeric arguments.

Updates:      Jul 21, 1988: JPT, Original document.
              Jul 12, 1989: KGB, Converted to ANSI C.
              Apr 23, 1991: KGB, implemented for HP.

#<

@ logical function presentc( character )

*/


bool presentc_c( fchar carg )
{
   if ((carg.a == NULL) || (carg.a == &NO_ARG_C)) return( FALSE ); else return( TRUE );
}
