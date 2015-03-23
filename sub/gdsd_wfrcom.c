/* gdsd_wfrcom.c

        Copyright (c) Kapteyn Laboratorium Groningen 1995
        All Rights Reserved.

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */

#include	"gipsyc.h"		/* GIPSY definitions */

#include	"gdsd_rfits.h"		/* defines gdsd_rfits_c */
#include	"ftsd_type.h"		/* defines ftsd_type_c */
#include	"gdsd_wfits.h"		/* defines gdsd_wfits_c */

#define	FRLEN	80			/* length of fits record */
#define	FRNCS	30			/* start of comment for non text */
#define	FRTCS	20			/* start of comment for text */
#define	FRDFS	10			/* start of data field */
#define	TPLEN	4			/* Max. length of FITS types */

static	char	buffer1[FRLEN];
static	char	buffer2[TPLEN];

static	fchar	record = { buffer1, FRLEN };
static	fchar	type =   { buffer2, TPLEN };

static	void	doit( fchar set,
                      fchar name,
                      fint  *subset,
                      fchar comment,
                      fint  *error,
                      int   W )
{
   fint	i = 0, s;
   fint	level;

   gdsd_rfits_c( set, name, subset, record, error );
   if ( (*error) < 0 ) return;
   if ( ( W ) && ( (*error) != (*subset) ) ) return;
   level = *error;
   ftsd_type_c( type, record );
   switch ( type.a[0] ) {
      case 'D':
      case 'I':
      case 'L':
      case 'R': {
         s = FRNCS;
         break;
      }
      case 'C': {
         s = FRDFS + 1;
         while ( ( s < FRLEN ) && ( record.a[s++] != '\'' ) );
         if ( s < FRTCS ) s = FRTCS;
         break;
      }
      default: {
         (*error) = -18;
         return;
      }
   }
   if ( W ) {
      record.a[s++] = ' ';
      record.a[s++] = '/';
      while ( ( i < comment.l ) && ( s < FRLEN ) ) record.a[s++] = comment.a[i++];
      while ( s < FRLEN ) record.a[s++] = ' ';
      gdsd_wfits_c( set, name, &level, record, error );
   } else {
      while ( ( s < FRLEN ) && ( record.a[s] == ' ' ) ) s++;
      if ( record.a[s] == '/' ) s++;
      while ( ( i < comment.l ) && ( s < FRLEN ) ) comment.a[i++] = record.a[s++];
      while ( i < comment.l ) comment.a[i++] = ' ';
   }
}

/*

#>            gdsd_wfrcom.dc2

Subroutine:   GDSD_WFRCOM

Purpose:      Writes comment section of a FITS record.

File:         gdsd_wfrcom.c

Author:       K.G. Begeman

Use:          CALL GDSD_WFRCOM( SET ,        Input       CHARACTER*(*)
                                NAME ,       Input       CHARACTER*(*)
                                SUBSET ,     Input       INTEGER
                                COMMENT ,    Input       CHARACTER*(*)
                                ERROR )    In/Output     INTEGER

              SET             Name of GDS set.
              NAME            Name of FITS descriptor.
              SUBSET          Subset coordinate word.
              COMMENT         Comment to be written to FITS descriptor.
                              Normally not more than 48 characters can be
                              stored in a FITS record.
              ERROR           Error return code.

Rel. Docs.:   gdsd_rfrcom.dc2

Update:       Dec 21, 1995: KGB, Document created.
              
#<

Fortran to C interface:

@ subroutine gdsd_wfrcom( character, character, integer, character, integer )

*/

void	gdsd_wfrcom_c( fchar set,
                       fchar name,
                       fint  *subset,
                       fchar comment,
                       fint  *error )
{
   doit( set, name, subset, comment, error, 1 );
}

/*

#>            gdsd_rfrcom.dc2

Subroutine:   GDSD_RFRCOM

Purpose:      Reads comment section of a FITS record.

File:         gdsd_wfrcom.c

Author:       K.G. Begeman

Use:          CALL GDSD_RFRCOM( SET ,        Input       CHARACTER*(*)
                                NAME ,       Input       CHARACTER*(*)
                                SUBSET ,     Input       INTEGER
                                COMMENT ,    Output      CHARACTER*(*)
                                ERROR )    In/Output     INTEGER

              SET             Name of GDS set.
              NAME            Name of FITS descriptor.
              SUBSET          Subset coordinate word.
              COMMENT         On return contains comment stored in FITS
                              descriptor. A maximum of 60 characters maybe
                              returned.
              ERROR           Error return code.

Rel. Docs.:   gdsd_wfrcom.dc2

Update:       Dec 21, 1995: KGB, Document created.
              
#<

Fortran to C interface:

@ subroutine gdsd_rfrcom( character, character, integer, character, integer )

*/

void	gdsd_rfrcom_c( fchar set,
                       fchar name,
                       fint  *subset,
                       fchar comment,
                       fint  *error )
{
   doit( set, name, subset, comment, error, 0 );
}

#ifdef	TESTBED

#include	"cmain.h"

#include	"anyout.h"
#include	"finis.h"
#include	"gdsinp.h"
#include	"init.h"
#include	"nelc.h"
#include	"usercharu.h"
#include	"userint.h"
#include	"usertext.h"

MAIN_PROGRAM_ENTRY
{
   char		db[20], cb[FRLEN], sb[80], tb[200];
   fchar	descr, comment, set, text;
   fint		mode = 0;
   fint		subset;
   
   comment.a = cb; comment.l = sizeof( cb );
   descr.a = db; descr.l = sizeof( db );
   set.a = sb; set.l = sizeof( sb );
   text.a = tb; text.l = sizeof( tb );
   init_c( );
   {
      fint	axcount[10];
      fint	axperm[sizeof(axcount)];
      fint	class = 1;
      fint	classdim = 0;
      fint	default_level = 0;
      fint	maxaxes = sizeof( axperm );
      fint	maxsub = 1;
      fint	output_level = 1;
      
      (void) gdsinp_c( set, &subset, &maxsub, &default_level, 
         tofchar( "INSET=" ), tofchar( " " ), &output_level, axperm,
         axcount, &maxaxes, &class, &classdim );
   }
   {
      fint	default_level = 0;
      fint	maxdescr = 1;
      fint	maxmode = 1;

      (void) usercharu_c( descr, &maxdescr, &default_level,
         tofchar( "NAME=" ), tofchar( "Name of descriptor" ) );
      (void) userint_c( &mode, &maxmode, &default_level,
         tofchar( "MODE=" ),
         tofchar( "Test Mode (1:Read,2:Write,3:Write&Read)" ) );
   }
   switch( mode ) {
      case 1: {
         fint	gerror = 0;
         fint	output_level = 1;

         anyout_c( &output_level, tofchar( "Testing gdsd_rfrcom" ) );
         gdsd_rfrcom_c( set, descr, &subset, comment, &gerror );
         sprintf( tb, "gdsd_rfrcom = %d", (int) gerror );
         anyout_c( &output_level, tofchar( tb ) );
         if ( gerror >= 0 ) {
            sprintf( tb, "Comment = %.*s", (int) nelc_c( comment ),
            comment.a );
            anyout_c( &output_level, tofchar( tb ) );
         }
         break;
      }
      case 2: {
         fint	gerror = 0;
         fint	input_level = 0;
         fint	output_level = 1;

         anyout_c( &output_level, tofchar( "Testing gdsd_wfrcom" ) );
         (void) usertext_c( comment, &input_level, tofchar( "COMMENT=" ),
            tofchar( "Enter comment to be added to descriptor" ) );
         gdsd_wfrcom_c( set, descr, &subset, comment, &gerror );
         break;
      }
      case 3: {
         fint	gerror = 0;
         fint	input_level = 0;
         fint	output_level = 1;

         anyout_c( &output_level, tofchar( "Testing gdsd_wfrcom" ) );
         (void) usertext_c( comment, &input_level, tofchar( "COMMENT=" ),
            tofchar( "Enter comment to be added to descriptor" ) );
         gdsd_wfrcom_c( set, descr, &subset, comment, &gerror );
         anyout_c( &output_level, tofchar( "Testing gdsd_rfrcom" ) );
         gdsd_rfrcom_c( set, descr, &subset, comment, &gerror );
         sprintf( tb, "gdsd_rfrcom = %d", (int) gerror );
         anyout_c( &output_level, tofchar( tb ) );
         if ( gerror >= 0 ) {
            sprintf( tb, "Comment = %.*s", (int) nelc_c( comment ),
               comment.a );
            anyout_c( &output_level, tofchar( tb ) );
         }
         break;
      }
   }
   finis_c( );
   return( 0 );
}
#endif
