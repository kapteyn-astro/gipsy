/* gdsd_history.c

	Copyright (c) Kapteyn Laboratorium Groningen 1994
	All Rights Reserved.


#>            gdsd_history.dc2

Function:     GDSD_HISTORY

Purpose:      Writes current date and time and the current task parameters
              to history in the descriptor of a set at top level.

Category:     UTILITY

File:         history.c

Author:       K.G. Begeman

Use:          GDSD_HISTORY( SET,          INPUT        CHARACTER*(*)
                            ERROR )    INPUT/OUTPUT    INTEGER

              SET      Name of GDS set to which the HISTORY information
                       is written.
              ERROR    Error return code.

Updates:      Feb 18, 1994: KGB Document created.

#<

Fortran to C interface:

@ subroutine gdsd_history( character, integer )

*/

#include	"stddef.h"		/* <stddef.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"time.h"		/* <time.h> */
#include	"gipsyc.h"		/* GIPSY definitions */
#include	"allpar.h"		/* defines allpar_c */
#include	"gdsd_wvar.h"		/* defines gdsd_wvar_c */
#include	"myname.h"		/* defines myname_c */
#include	"nelc.h"		/* defines nelc_c */

#define	MAXTEXT	150			/* max. length of text */

void	gdsd_history_c( fchar	set,		/* name of set */
                        fint	*gerror )	/* GDS return code */
{
   char		textb[MAXTEXT+1];		/* buffer for text */
   fchar	text;				/* points to textb */
   fint		l;				/* number of characters */
   fint		mode = 0;			/* mode for allpar_c */
   fint		top = 0;			/* level of subset (top) */
   time_t	now;				/* current time */

   now = time( NULL );				/* get current time */
   text.a = textb; text.l = MAXTEXT;		/* initialize f-character */
   myname_c( text );				/* get name of application */
   l = nelc_c( text );				/* length of name */
   l = sprintf( &textb[l], " PARAMETERS %s", ctime( &now ) );
   gdsd_wvar_c( set, tofchar( "HISTORY" ), &top, tofchar( textb ), gerror );
   if ( (*gerror) < 0 ) return;			/* quit in case of error */
   while ( ( l = allpar_c( &mode, text ) ) > 0 ) {
      textb[l] = 0;				/* add zero byte */
      gdsd_wvar_c( set, tofchar( "HISTORY" ), &top, tofchar( textb ), gerror );
      if ( (*gerror) < 0 ) return;		/* quit in case of error */
   }
}
