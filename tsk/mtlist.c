/* mtlist.c

	Copyright (c) Kapteyn Laboratorium Groningen 1993
	All Rights Reserved.

#>            mtlist.dc1

Program:      MTLIST

Purpose:      Lists files on a tape device.

Category:     TAPES, UTILITY

File:         mtlist.c

Author:       K.G. Begeman

Description:  MTLIST knows about some file types. If it encounters a tape
              label, it will display the first record of the label. If it
              encounters a FITS file, if will list the OBJECT name, the
              INSTRUME or TELESCOP name, the BITPIX item, the sizes of
              the FITS structure and the first COMMENT line.
              If it encounters a WSRT file it will list the FIELD name,
              PROJECT number, OBSERVATION TYPE, SEQUENCE number,
              TOTAL BANDWIDTH, Number of Frequency Points, Number
              of Polarizations, Velocity at centre of band, Frequency at
              centre of band, Begin and End Hour Angle of observation,
              the minimum baseline in Meters and the frequency taper. For
              any other files, MTLIST will display the first 80 printable
              characters.

Keywords:

   TAPE=      Tape device to list.

   FILE=      Name of file for listing [tape.lis].

   REWIND=    Rewind tape after listing it [Y]?

Updates:      Dec 13, 1993: KGB, Document created.

#<

*/

#include	"ctype.h"		/* <ctype.h> */
#include	"stddef.h"		/* <stddef.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY definitions */
#include	"anyout.h"		/* anyout_c */
#include	"cmain.h"		/* Main of C program */
#include	"error.h"		/* error_c */
#include	"finis.h"		/* finis_c */
#include	"ftsd_find.h"		/* ftsd_find_c */
#include	"ftsd_rchar.h"		/* ftsd_rchar_c */
#include	"ftsd_rint.h"		/* ftsd_rint_c */
#include	"ftsd_rlog.h"		/* ftsd_rlog_c */
#include	"init.h"		/* init_c */
#include	"mtclose.h"		/* mtclose_c */
#include	"mtfsf.h"		/* mtfsf_c */
#include	"mtopen.h"		/* mtopen_c */
#include	"mtread.h"		/* mtread_c */
#include	"mtrew.h"		/* mtrew_c */
#include	"nelc.h"		/* nelc_c */
#include	"userlog.h"		/* userlog_c */
#include	"usertext.h"		/* usertext_c */

extern	void	cnvrtc_c( fint *, char *, char *, fint * );
extern	void	cnvrth_c( fint *, char *, short *, fint * );
extern	void	cnvrtf_c( fint *, char *, int *, fint * );
extern	void	cnvrte_c( fint *, char *, float *, fint * );
extern	void	cnvrtd_c( fint *, char *, double *, fint * );

MAIN_PROGRAM_ENTRY
{
   FILE		*o;			/* output file */
   bool		rewind = TRUE;		/* rewind tape at end */
   char		data[28800];		/* data buffer (10 FITS BLOCKS) */
   char		text[800];		/* text buffer */
   fchar	fhead;			/* fits header */
   fint		fatal = 4;		/* fatal error */
   fint		mtstat = 0;		/* tape return code */
   fint		mtid;			/* tape id */
   fint		ndata = sizeof( data );	/* size of data buffer */
   fint		filenr = 0;		/* tape file counter */
   fint		nread;			/* bytes read from tape */
   fint		one = 1;		/* 1 */
   fint		screen = 1;		/* output to screen */
   fint		two = 2;		/* 2 */

   init_c( );
   fhead.a = data;
   /*
    * Get and Open the Tape Device.
    */
   if ( ( mtid = mtopen_c( tofchar( "?TAPE=" ) ) ) < 0 ) {
      sprintf( text, "Error (%d) opening Tape Device", mtid );
      error_c( &fatal, tofchar( text ) );
   }
   /*
    * Get and Open the file for the tape listing.
    */
   {
      fchar	name;

      name.a = text; name.l = sizeof( text ) - 1;
      sprintf( text, "tape.lis" );
      usertext_c( name, &one, tofchar( "FILE=" ),
         tofchar( "Name of file for listing [tape.lis]" ) );
      text[nelc_c( name )] = 0;
      o = fopen( text, "w" );
      if ( o == NULL ) {
         error_c( &fatal, tofchar( "Cannot create file for tape listing!" ) );
      }
   }
   userlog_c( &rewind, &one, &one, tofchar( "REWIND=" ),
      tofchar( "Rewind tape ? [Y]" ) );
   sprintf( text, "FILE TYPE  INFO" );
   anyout_c( &screen, tofchar( text ) );
   fprintf( o, "%s\n", text );
   while ( ( nread = mtread_c( &mtid, (fint *) &data, &ndata ) ) ) {
      fint	i, m, n;

      filenr++;
      if ( nread < 0 ) {
         sprintf( text, "Tape Read Error (%d) on file #%d", nread, filenr );
         fclose( o );
         error_c( &fatal, tofchar( text ) );
      }
      if ( nread == 80 ) {
         sprintf( text, "%4d LABEL %.80s", filenr, data );
         anyout_c( &screen, tofchar( text ) );
         fprintf( o, "%s\n", text );
      } else if ( nread%2880 == 0 ) {
         bool	lval;

         sprintf( text, "%4d FITS  ", filenr );
         fhead.l = nread;
         if ( ftsd_rlog_c( fhead, tofchar( "SIMPLE" ), &lval ) < 0 ) {
            m = 0;
            n = sprintf( text, "%4d ????  ", filenr );
            while ( n < 80 && isprint( data[m] ) ) {
               text[n++] = data[m++];
            }
            text[n] = 0;
         } else if ( !tobool( lval ) ) {
            sprintf( text, "%4d FITS  Not SIMPLE", filenr );
         } else {
            char	string[81];
            fchar	sval;
            fint	ival;
            fint	naxis;

            sval.a = string; sval.l = sizeof( string ) - 1;
            while ( nread < ndata && ftsd_find_c( fhead, tofchar( "END" ), sval ) == -1 ) {
               fint	size = ndata - nread;

               mtstat = mtread_c( &mtid, (fint *) &data[nread], &size );
               if ( mtstat <= 0 ) break;
               nread += mtstat;
               fhead.l = nread;
            }
            n = sprintf( text, "%4d FITS  ", filenr );
            if ( ftsd_rchar_c( fhead, tofchar( "OBJECT" ), sval ) > 0 ) {
               n += sprintf( &text[n], "OBJECT=%.*s ", nelc_c( sval ), sval.a );
            }
            if ( ftsd_rchar_c( fhead, tofchar( "INSTRUME" ), sval ) > 0 ) {
               n += sprintf( &text[n], "INSTRUME=%.*s ", nelc_c( sval ), sval.a );
            } else if ( ftsd_rchar_c( fhead, tofchar( "TELESCOP" ), sval ) > 0 ) {
               n += sprintf( &text[n], "TELESCOP=%.*s ", nelc_c( sval ), sval.a );
            }
            if ( ftsd_rint_c( fhead, tofchar( "BITPIX" ), &ival ) > 0 ) {
               n += sprintf( &text[n], "BITPIX=%d ", ival );
            }
            if ( ftsd_rint_c( fhead, tofchar( "NAXIS" ), &naxis ) < 0 ) {
               n += sprintf( &text[n], "(?" );
            } else for ( i = 0; i < naxis; i++ ) {
               char	num[10];
               sprintf( num, "NAXIS%d", i + 1 );
               if ( ftsd_rint_c( fhead, tofchar( num ), &ival ) < 0 ) {
                  strcpy( num, "?" );
               } else {
                  sprintf( num, "%d", ival );
               }
               if ( i == 0 ) {
                  strcat( text, "(" );
               } else {
                  strcat( text, "," );
               }
               strcat( text, num );
            }
            strcat( text, ") " );
            if ( ftsd_find_c( fhead, tofchar( "COMMENT" ), sval ) > 0 ) {
               int	l = nelc_c( sval );
               int	n = 10;

               string[l] = 0;
               if ( n < l ) {
                  strcat( text, "COMMENT=" );
                  strcat( text, &string[n] );
               }
            }
         }
         anyout_c( &screen, tofchar( text ) );
         fprintf( o, "%s\n", text );
      } else if ( nread == 3840 ) {
         fint	tform;

         if ( !strncmp( &data[2], "FD", 2 ) ) {
            tform = 1;
         } else {
            tform = 3;
            cnvrtc_c( &tform, &data[2], &data[2], &two );
            if ( strncmp( &data[2], "FD", 2 ) ) {
               tform = -1;
            }
         }
         if ( tform > 0 ) {
            double	dval;
            fint	nb;
            float	eval;
            int		fval, posa, pos9;
            short	hval;

            n = sprintf( text, "%4d WSRT  ", filenr );
            nread = mtread_c( &mtid, (fint *) data, &ndata );
            nb = 12;
            cnvrtc_c( &tform, &data[28], &data[28], &nb );
            n += sprintf( &text[n], "%12.12s ", &data[28] );
            cnvrth_c( &tform, &data[26], &hval, &one );
            n += sprintf( &text[n], "%5d ", hval );
            cnvrtc_c( &tform, &data[46], &data[46], &two );
            n += sprintf( &text[n], "%2.2s ", &data[46] );
            cnvrtf_c( &tform, &data[40], &fval, &one );
            n += sprintf( &text[n], "%8d ", fval );
            cnvrte_c( &tform, &data[148], &eval, &one );
            n += sprintf( &text[n], "%6.3f ", (double) eval );
            cnvrth_c( &tform, &data[154], &hval, &one );
            n += sprintf( &text[n], "%4d ", hval );
            cnvrth_c( &tform, &data[158], &hval, &one );
            n += sprintf( &text[n], "%1d ", hval );
            cnvrte_c( &tform, &data[244], &eval, &one );
            n += sprintf( &text[n], "%7.2f ", (double) eval );
            cnvrtd_c( &tform, &data[192], &dval, &one );
            n += sprintf( &text[n], "%11.5f ", dval );
            cnvrtd_c( &tform, &data[200], &dval, &one );
            n += sprintf( &text[n], "%6.2f ", dval * 360.0 );
            cnvrtd_c( &tform, &data[208], &dval, &one );
            n += sprintf( &text[n], "%6.2f ", dval * 360.0 );
            cnvrtf_c( &tform, &data[336], &pos9, &one );
            cnvrtf_c( &tform, &data[340], &posa, &one );
            n += sprintf( &text[n], "%7.2f ", ( posa - pos9 ) / 65536.0 );
            cnvrth_c( &tform, &data[356], &hval, &one );
            n += sprintf( &text[n], "%2d ", (int) hval );

         } else {
            m = 0;
            n = sprintf( text, "%4d ????  ", filenr );
            while ( n < 80 && isprint( data[m] ) ) {
               text[n++] = data[m++];
            }
            text[n] = 0;
         }
         anyout_c( &screen, tofchar( text ) );
         fprintf( o, "%s\n", text );
      } else if ( ( nread % 512 ) == 0 ) {
         m = 0;
         n = sprintf( text, "%4d TAR   ", filenr );
         while ( n < ( sizeof( text ) - 1 ) && data[m] ) {
            text[n++] = data[m++];
         }
         text[n] = 0;
         anyout_c( &screen, tofchar( text ) );
         fprintf( o, "%s\n", text );
      } else {
         m = 0;
         n = sprintf( text, "%4d ????  ", filenr );
         while ( n < 80 && isprint( data[m] ) ) {
            text[n++] = data[m++];
         }
         text[n] = 0;
         anyout_c( &screen, tofchar( text ) );
         fprintf( o, "%s\n", text );
      }
      if ( ( mtstat = mtfsf_c( &mtid, &one ) ) != one ) {
         sprintf( text, "Error (%d) skipping file #%d!", mtstat, filenr );
         fclose( o );
         error_c( &fatal, tofchar( text ) );
      }
   }
   fclose( o );
   if ( tobool( rewind ) ) mtrew_c( &mtid );
   mtclose_c( &mtid );
   finis_c( );
   return( 0 );
}
