/* rewind.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            rewind.dc1

Program:      REWIND

Purpose:      Rewinds a tape.

Category:     TAPES, UTILITY

File:         rewind.c

Author:       K.G. Begeman

Keywords:

    TAPE=     Tape device to rewind [list of all tape devices].

Updates:      Oct 21, 1991: KGB, Document created.

#<

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"cmain.h"		/* main programme in C */
#include	"error.h"		/* defines error_c */
#include	"finis.h"		/* defines finis_c */
#include	"init.h"		/* defines init_c */
#include	"mtclose.h"		/* defines mtclose_c */
#include	"mtname.h"		/* defines mtname_c */
#include	"mtopen.h"		/* defines mtopen_c */
#include	"mtrew.h"		/* defines mtrew_c */
#include	"nelc.h"		/* defines nelc_c */
#include	"status.h"		/* defines status_c */

MAIN_PROGRAM_ENTRY
{
   char		message[80];
   char		tapeb[30];
   fchar	tape;
   fint		mtid;

   tape.a = tapeb; tape.l = sizeof( tapeb );
   init_c( );
   mtid = mtopen_c( tofchar( "?TAPE=Tape device to rewind? [list of all tape devices]" ) );
   if (mtid < 0) {
      fint	error_level = 4;

      error_c( &error_level, tofchar( "Error opening tape device!" ) );
   }
   if (mtid == mtname_c( &mtid, tape )) {
      sprintf( message, "Rewinding %.*s", (int) nelc_c( tape ), tape.a );
      status_c( tofchar( message ) );
   }
   if (mtrew_c( &mtid )) {
      fint	error_level = 4;

      error_c( &error_level, tofchar( "Error rewinding tape!" ) );
   }
   if (mtclose_c( &mtid )) {
      fint	error_level = 4;

      error_c( &error_level, tofchar( "Error closing tape device!" ) );
   }
   finis_c( );
   return( EXIT_SUCCESS );
}
