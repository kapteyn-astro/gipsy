/* xflib.c

	Copyright (c) Kapteyn Laboratorium Groningen 1994
	All Rights Reserved.

#>            xflib.dc3

Document:     XFLIB

Purpose:      XFLIB is the extended Fortran library. It contains some of the
              non-ANSI Fortran functions which are not available on some
              operating systems.

File:         xflib.c

Author:       K.G. Begeman

Description:  The Fortran functions EXITF, IANDF and IORF are provided to
              replace the non-standard EXIT, IAND and IOR.

Updates:      Apr 21, 1994: KGB, Document created.

#<

*/

#include	"osdef.h"
#include	"stdlib.h"
#include	"f2cvvdefs.h"

/*

#>            exitf.dc3

Function:     EXITF

Purpose:      Terminate process with status. Replaces the non-standard EXIT.

Category:     SYSTEM

File:         xflib.c

Author:       K.G. Begeman

Use:          CALL EXITF( STATUS )        Input     INTEGER

              STATUS      Passed to the system for inspection.

Notes:        EXITF should replace the non-standard EXIT.

Updates:      Apr 21, 1994: KGB, Document created.

#<

Fortran to C interface:

@ subroutine exitf( integer )

*/

void	exitf_c( fint *status )
{
   exit( (int) *status );
}

/*

#>            iandf.dc3

Function:     IANDF

Purpose:      Returns the bitwise AND of two operands. Replaces the
              non-standard IAND.

Category:     SYSTEM

File:         xflib.c

Author:       K.G. Begeman

Use:          INTEGER IANDF( WORD1 ,        Input     INTEGER
                             WORD2 )        Input     INTEGER

              IANDF       Returns the bitwise AND of WORD1 and WORD2.
              WORD1       First operand.
              WORD2       Second operand.

Notes:        IANDF should replace the non-standard IAND.

Updates:      Apr 21, 1994: KGB, Document created.

#<

Fortran to C interface:

@ integer function iandf( integer, integer )

*/

fint	iandf_c( fint *word1, fint *word2 )
{
   return( (*word1) & (*word2) );
}

/*

#>            iorf.dc3

Function:     IORF

Purpose:      Returns the bitwise OR of two operands. Replaces the
              non-standard IOR.

Category:     SYSTEM

File:         xflib.c

Author:       K.G. Begeman

Use:          INTEGER IORF( WORD1 ,        Input     INTEGER
                            WORD2 )        Input     INTEGER

              IORF        Returns the bitwise OR of WORD1 and WORD2.
              WORD1       First operand.
              WORD2       Second operand.

Notes:        IORF should replace the non-standard IOR.

Updates:      Apr 21, 1994: KGB, Document created.

#<

Fortran to C interface:

@ integer function iorf( integer, integer )

*/

fint	iorf_c( fint *word1, fint *word2 )
{
   return( (*word1) | (*word2) );
}
