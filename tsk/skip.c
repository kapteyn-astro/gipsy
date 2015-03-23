/* skip.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            skip.dc1

Program:      SKIP

Purpose:      Skips files forward or backward on tape.

Category:     TAPES, UTILITY

File:         skip.c

Author:       K.G. Begeman

Keywords:

    TAPE=     Tape device where to skip files [list of all tape devices].

    FILES=    Number of files to skip. A positive number means skip forward,
              a negative number means skip backward.

Updates:      Oct 29, 1991: KGB, Document created.

#<

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"cmain.h"		/* main programme in C */
#include	"anyout.h"		/* defines anyout_c */
#include	"error.h"		/* defines error_c */
#include	"finis.h"		/* defines finis_c */
#include	"init.h"		/* defines init_c */
#include	"mtbsf.h"		/* defines mtbsf_c */
#include	"mtclose.h"		/* defines mtclose_c */
#include	"mtfsf.h"		/* defines mtfsf_c */
#include	"mtname.h"		/* defines mtname_c */
#include	"mtopen.h"		/* defines mtopen_c */
#include	"nelc.h"		/* defines nelc_c */
#include	"status.h"		/* defines status_c */
#include	"userint.h"		/* defines userint_c */

MAIN_PROGRAM_ENTRY
{
   fint		mtid;
   fint		files;

   init_c( );
   mtid = mtopen_c( tofchar( "?TAPE=Tape device where to skip files? [list of all tape devices]" ) );
   if (mtid < 0) {
      fint	error_level = 4;

      error_c( &error_level, tofchar( "Error opening tape device!" ) );
   }
   {
      fint	input_level = 0;
      fint	items = 1;

      (void) userint_c( &files ,
                        &items ,
                        &input_level ,
                        tofchar( "FILES=" ),
                        tofchar( "Number of files to skip" ) );
   }
   if (files) {
      char	message[80];
      char	tapeb[30];
      fchar	tape;
      int	r;

      tape.a = tapeb; tape.l = sizeof( tapeb );
      mtname_c( &mtid, tape );
      if (files > 0) {
         sprintf( message, "Skipping %d files forward on %.*s", (int) files, (int) nelc_c( tape ), tape.a );
         status_c( tofchar( message ) );
         r = mtfsf_c( &mtid, &files );
      } else {
         files = -files;
         sprintf( message, "Skipping %d files backward on %.*s", (int) files, (int) nelc_c( tape ), tape.a );
         status_c( tofchar( message ) );
         r = mtbsf_c( &mtid, &files );
      }
      if (r < 0) {
         fint	error_level = 1;

         error_c( &error_level, tofchar( "Error skipping files!" ) );
      } else if (r != files) {
         fint	output_level = 3;

         sprintf( message, "Skipped %d files", r );
         anyout_c( &output_level, tofchar( message ) );
      }
   }
   if (mtclose_c( &mtid )) {
      fint	error_level = 4;

      error_c( &error_level, tofchar( "Error closing tape device!" ) );
   }
   finis_c( );
   return( EXIT_SUCCESS );
}
