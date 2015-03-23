/* rename.c

	Copyright (c) Kapteyn Laboratorium Groningen 1994
	All Rights Reserved.

#>            rename.dc1

Program:      RENAME

Purpose:      Renames a GDS set.

Category:     UTILITY

File:         rename.c

Author:       K.G. Begeman

Keywords:

   INSET=     Name of set to be renamed.

   OUTSET=    New name of set.

Updates:      Feb 22, 1994: KGB Document created.
              Oct  3, 1994: JPT Call gds_rename

#<

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"string.h"		/* <string.h> */

#include	"gipsyc.h"		/* GIPSY definitions */
#include	"cmain.h"		/* program in C */
#include	"error.h"		/* defines error_c */
#include	"finis.h"		/* defines finis_c */
#include	"gds_exist.h"		/* defines gds_exist_c */
#include        "gds_rename.h"          /* defines gds_rename_c */
#include	"init.h"		/* defines init_c */
#include	"reject.h"		/* defines reject_c */
#include	"usertext.h"		/* defines usertext_c */

#define	ERR_INSET1	tofchar( "Set present but cannot be opened" )
#define	ERR_INSET2	tofchar( "Set does not exist" )
#define	ERR_OUTSET1	tofchar( "Set present but cannot be opened" )
#define	ERR_OUTSET2	tofchar( "Set already present" )
#define	KEY_INSET	tofchar( "INSET=" )
#define	KEY_OUTSET	tofchar( "OUTSET=" )
#define	MES_INSET	tofchar( "Enter name of set to rename" )
#define	MES_OUTSET	tofchar( "Enter new name of set" )

MAIN_PROGRAM_ENTRY
{
   char		isetb[FILENAME_MAX+1];	/* buffer for input set name */
   char 	osetb[FILENAME_MAX+1];	/* buffer for output set name */
   fchar	iset;			/* input set */
   fchar	oset;			/* output set */
   fint		inc;			/* length of input set name */
   fint		onc;			/* length of output set name */
   fint         gerror;                 /* gds error return */

   iset.a = isetb; iset.l = sizeof( isetb ) - 1;
   oset.a = osetb; oset.l = sizeof( osetb ) - 1;
   init_c( );				/* contact HERMES */
   do {					/* loop to get input set name */
      bool	exist;			/* does set exist ? */
      fint	input_level = 0;	/* no default possible */

      gerror = 0;			/* gds error return */
      inc = usertext_c( iset, &input_level, KEY_INSET, MES_INSET );
      exist = tobool( gds_exist_c( iset, &gerror ) );
      if ( gerror < 0 ) {		/* something wrong with set */
         inc = 0;			/* reset */
         reject_c( KEY_INSET, ERR_INSET1 );
      } else if ( !exist ) {		/* set not present */
         inc = 0;			/* reset */
         reject_c( KEY_INSET, ERR_INSET2 );
      }
   } while ( !inc );			/* end of loop */
   do {					/* loop to get output set */
      bool	exist;			/* does set exist ? */
      fint	input_level = 0;	/* no default possible */

      gerror = 0;			/* gds error return */
      onc = usertext_c( oset, &input_level, KEY_OUTSET, MES_OUTSET );
      exist = tobool( gds_exist_c( oset, &gerror ) );
      if ( exist && gerror < 0 ) {	/* something wrong with set */
         onc = 0;			/* reset */
         reject_c( KEY_OUTSET, ERR_OUTSET1 );
      } else if ( exist ) {		/* set already exists */
         onc = 0;			/* reset */
         reject_c( KEY_OUTSET, ERR_OUTSET2 );
      }
   } while ( !onc );			/* end of loop */
   if ( gds_rename_c( iset, oset ) ) {	/* rename */
      char	msg[FILENAME_MAX+80];	/* message buffer */
      fint	error_level = 4;	/* fatal error */

      isetb[inc] = 0;			/* append zero byte */
      sprintf( msg, "Error renaming set %s", isetb );
      error_c( &error_level, tofchar( msg ) );
   }
   finis_c( );				/* end job */
   return( 0 );				/* THE END */
}
