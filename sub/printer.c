/* printer.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            printer.dc3

Document:     PRINTER

Purpose:      Describes the routines which get information on the
              available text printers.

Files:        printer.c

Category:     PRINTING

Author:       K.G. Begeman

Description:  The following routines are available:
              PRNTRNUM     Returns the number of printers available.
              PRNTRNAM     Returns the name of a printer.
              PRNTRCOM     Returns comments about a printer.
              PRNTRDIM     Returns number of columns and lines of
                           selected printer.
              PRNTRACT     Prints a file.

              The information about printers is obtained from
              the file lpdevices which is located in the gip_loc
              directory.

Updates:      Aug 18, 1990: KGB, Document created.

#<

*/

#include	"ctype.h"		/* <ctype.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"fname.h"		/* define fname_c */
#include	"hostname.h"		/* define hostname_c */
#include	"nelc.h"		/* define nelc_c */
#include	"xscanf.h"		/* reads setup files */

#define	MAXNAMLEN	16		/* maximum length of printer name */
#define MAXCMDLEN	80		/* maximum length of command string */
#define	MAXCOMLEN	80		/* maximum length of printer comment */
#define MAXFILNAMLEN	128		/* maximum length of file name */
#define	MAXHOSTS	32		/* maximum number of hosts */
#define	MAXTEXTLEN	512		/* maximum length of text buffer */
#define	MAXHOSTNAMLEN   80		/* maximum length of hostname */

typedef struct {
   char  nam[MAXNAMLEN+1];		/* printer name */
   char  cmd[MAXCMDLEN+1];		/* format for printer command */
   char  com[MAXCOMLEN+1];		/* printer commentary text */
   int   col;				/* number of columns */
   int   row;				/* number of rows */
} print_struct;

static	print_struct	*prntrs = NULL;	/* buffer for printer info */
static	int		 nprntrs = 0;	/* number of printers */

static	fint	iniprinter( void )
/*
 * iniprinter reads the file lpdevices in the gip_loc directory and
 * searches for the different printers for the current host. This
 * information is stored in the print_struct.
 */
{
   char		nam[MAXNAMLEN+1];		/* maximum length of printer name */
   char		cmd[MAXCMDLEN+1];		/* maximum length of command string */
   char		com[MAXCOMLEN+1];		/* maximum length of printer comment */
   char		fnameb[MAXFILNAMLEN+1];		/* buffer for file name */
   char		hnameb[MAXHOSTNAMLEN+1];	/* buffer for hostname */
   char		host[MAXHOSTS*MAXHOSTNAMLEN+1];	/* buffer for hostnames */
   fchar	fname;				/* points to fnameb */
   fchar	hname;				/* points to hnameb */
   FILE		*f;				/* C file descriptor */
   fint		r;				/* return value */
   int		col, row;			/* columns and rows */
   int		hlen;				/* length of hostname */

   if (nprntrs) return( nprntrs );		/* already done */
   hname.a = hnameb; hname.l = MAXHOSTNAMLEN;	/* initialize f character */
   r = hostname_c( hname );			/* get hostname */
   if (r) return( -1 );				/* cannot obtain hostname */
   hlen = nelc_c( hname );			/* length of hostname */
   hnameb[hlen] = 0;				/* add zero byte */
   fname.a = fnameb; fname.l = MAXFILNAMLEN;	/* initialize f character */
   r = fname_c( tofchar( "gip_loc:lpdevices" ), fname );
   if (r) return( -2 );				/* fnameb too small */
   fnameb[nelc_c( fname )] = 0;			/* add zero byte */
   f = fopen( fnameb, "r" );			/* open file */
   if (f == NULL) return( -3 );			/* cannot open file */
   while (xscanf( f, "%s %s %s %s %d %d", host, nam, com, cmd, &col, &row ) == 6) {
      char	*hptr;				/* pointer */
      int	found = 0;			/* reset */

      if (!strcmp( host, "*" )) {		/* wild card */
         found = 1;				/* bingo */
      } else if ( (hptr = strstr( host, hnameb )) != NULL ) {
         found = 1;				/* bingo */
         if (hptr != host) {			/* not first in list */
            if (hptr[-1] != ',') found = 0;	/* no match */
         }
         if (found && hptr[hlen] != 0 && hptr[hlen] != '.' && hptr[hlen] != ',') {
            found = 0;				/* no match */
         }
      }
      if (found) {				/* we found one */
         int	nprn = 0;			/* reset */

         while (nprn < nprntrs && strcmp( nam, prntrs[nprn].nam ) ) nprn++;
         if ( nprn == nprntrs ) {		/* not in list */
            prntrs = realloc( prntrs, sizeof( print_struct ) * (++nprntrs) );
            if (prntrs == NULL) {		/* error */
               r = -4;				/* cannot allocate space */
               break;				/* quit searching */
            }
         }
         strcpy( prntrs[nprn].nam, nam );	/* name of printer */
         strcpy( prntrs[nprn].com, com );	/* comments */
         strcpy( prntrs[nprn].cmd, cmd );	/* print command */
         prntrs[nprn].col = col;		/* number of columns */
         prntrs[nprn].row = row;		/* number of rows */
      }      
   }
   fclose( f );
   if (!r) r = nprntrs; else nprntrs = r;	/* save status */
   return( r );					/* return to caller */
}
/*

#>            prntrnum.dc3

Function:     PRNTRNUM

Purpose:      Returns the number of printers available on current host

Category:     PRINTING

File:         printer.c

Author:       K.G. Begeman

Use:          INTEGER PRNTRNUM( )

              PRNTRNUM       Returns:
                             >0: number of printers available.
                             -1: cannot obtain hostname.
                             -2: cannot obtain translation of
                                 printer description file.
                             -3: cannot open printer description file.
                             -4: cannot allocate enough space.
                             -5: printer name exceeds buffer length.
                             -6: printer comment exceeds buffer length.
                             -7: printer command exceeds buffer length.
                             -8: cannot obtain number of columns.
                             -9: cannot obtain number of rows.

Updates:      Aug 18, 1990: KGB, Document created.

#<

Fortran to C interface:

@ integer function prntrnum( )

*/

fint	prntrnum_c( void )
{
   return( iniprinter( ) );			/* return status of iniprinter */
}

/*

#>            prntrnam.dc3

Function:     PRNTRNAM

Purpose:      Returns the name of a printer.

Category:     PRINTING

File:         printer.c

Author:       K.G. Begeman

Use:          INTEGER PRNTRNAM( PNUMBER ,    Input     INTEGER
                                PNAME   )    Output    CHARACTER*(*)

              PRNTRNAM       Returns:
                               0: No error.
                              -1: cannot obtain hostname.
                              -2: cannot obtain translation of
                                  printer description file.
                              -3: cannot open printer description file.
                              -4: cannot allocate enough space.
                             -10: no such printer.
                             -11: printer name truncated.
              PNUMBER        Printer number.
              PNAME          Name of printer.

Updates:      Aug 18, 1990: KGB, Document created.

#<

Fortran to C interface:

@ integer function prntrnam( integer, character )

*/

fint	prntrnam_c( fint *pnum, fchar pnam )
{
   fint	r;					/* status */

   r = iniprinter( );				/* initialize printer buffer */
   if (r > -1) {				/* no errors so far */
      if ( *pnum > 0 && *pnum <= r ) {		/* legal printer number */
         fint  i;				/* counter */
         fint  p = (*pnum - 1);			/* pointer in buffer */

         for (i = 0; prntrs[p].nam[i] && i < pnam.l; i++) {
            pnam.a[i] = prntrs[p].nam[i];	/* copy printer name */
         }
         if (prntrs[p].nam[i]) {
            r = -11;				/* truncation */
         } else {
            r = 0;				/* no error */
         }
         while (i < pnam.l) pnam.a[i++] = ' ';	/* fill rest with blanks */
      } else {					/* no such printer */
         r = -10;				/* set status */
      }
   }
   return( r );					/* return status */
}

/*

#>            prntrcom.dc3

Function:     PRNTRCOM

Purpose:      Returns commentary text about a printer.

Category:     PRINTING

File:         printer.c

Author:       K.G. Begeman

Use:          INTEGER PRNTRCOM( PNUMBER ,    Input     INTEGER
                                PCOMM   )    Output    CHARACTER*(*)

              PRNTRCOM       Returns:
                               0: No error.
                              -1: cannot obtain hostname.
                              -2: cannot obtain translation of
                                  printer description file.
                              -3: cannot open printer description file.
                              -4: cannot allocate enough space.
                             -10: no such printer.
                             -11: comment truncated.
              PNUMBER        Printer number.
              PCOMM          Commentary text about a printer.

Updates:      Aug 18, 1990: KGB, Document created.

#<

Fortran to C interface:

@ integer function prntrcom( integer, character )

*/

fint	prntrcom_c( fint *pnum, fchar pcom )
{
   fint r;					/* status */

   r = iniprinter( );				/* initialize printer buffer */
   if (r > -1) {				/* no errors so far */
      if ( *pnum > 0 && *pnum <= r ) {		/* legal printer number */
         fint  i;				/* counter */
         fint  p = (*pnum - 1);			/* pointer in buffer */

         for (i = 0; prntrs[p].com[i] && i < pcom.l; i++) {
            pcom.a[i] = prntrs[p].com[i];	/* copy printer comment */
         }
         if (prntrs[p].com[i]) {
            r = -11;				/* truncation */
         } else {
            r = 0;				/* no error */
         }
         while (i < pcom.l) pcom.a[i++] = ' ';	/* fill rest with blanks */
      } else {					/* no such printer */
         r = -10;				/* set status */
      }
   }
   return( r );					/* return status */
}

/*

#>            prntrdim.dc3

Function:     PRNTRDIM

Purpose:      Returns the number of columns and  number of rows
              of a print device.

Category:     PRINTING

File:         printer.c

Author:       K.G. Begeman

Use:          INTEGER PRNTRDIM ( PNUMBER,     Input    INTEGER
                                 PCOL   ,     Output   INTEGER
                                 PROW   )     Output   INTEGER

              PRNTRDIM       Returns:
                               0: No error.
                              -1: cannot obtain hostname.
                              -2: cannot obtain translation of
                                  printer description file.
                              -3: cannot open printer description file.
                              -4: cannot allocate enough space.
                             -10: no such printer.
              PNUMBER        Printer number.
              PCOL           Number of columns (width).
              PROW           Number of rows (lines).

Updates:      Aug 18, 1990: KGB, Document created.

#<

Fortran to C interface:

@ integer function  prntrdim( integer, integer, integer )

*/

fint	prntrdim_c( fint *pnum, fint *pcol, fint *prow )
{
   fint r;					/* status */

   r = iniprinter( );				/* initialize printer buffer */
   if (r > -1) {				/* no errors so far */
      if ( *pnum > 0 && *pnum <= r ) {		/* legal printer number */
         fint p = (*pnum - 1);			/* pointer in buffer */

         *pcol = prntrs[p].col;			/* return number of columns */
         *prow = prntrs[p].row;			/* return number of rows */
         r = 0;					/* no error */
      } else {					/* no such printer */
         r = -10;				/* set status */
      }
   }
   return( r );					/* return status */
}

/*

#>            prntract.dc3

Function:     PRNTRACT

Purpose:      Prints a file and (optionally) deletes it.

Category:     PRINTING

File:         printer.c

Author:       K.G. Begeman

Use:          INTEGER PRNTRACT( PNUMBER,     Input    INTEGER
                                FILENAM,     Input    CHARACTER*(*)
                                REMOVE )     Input    INTEGER

              PRNTRDIM       Returns:
                               0: No error.
                              -1: cannot obtain hostname.
                              -2: cannot obtain translation of
                                  printer description file.
                              -3: cannot open printer description file.
                              -4: cannot allocate enough space.
                             -10: no such printer.
              PNUMBER        Printer number.
              FILNAM         Name of file to print.
              REMOVE         If zero FILNAM will not be removed,
                             otherwise it will be deleted after
                             printing.

Updates:      Aug 18, 1990: KGB, Document created.

#<

Fortran to C interface:

@ integer function prntract( integer, character, integer )

*/

fint	prntract_c( fint *pnum, fchar fnam, fint *rem )
{
   fint r;					/* status */

   r = iniprinter( );				/* initialize printer buffer */
   if (r > -1) {				/* no errors so far */
      if ( *pnum > 0 && *pnum <= r ) {		/* legal printer number */
         char  cmd[MAXTEXTLEN];			/* buffer for print command */
         char *file;				/* pointer to file name */
         fint  p = (*pnum - 1);			/* pointer in buffer */

         file = zadd( fnam );			/* zero terminates string */
         (void) sprintf( cmd, prntrs[p].cmd, file );
         (void) system( cmd );			/* do the print */
         if (*rem) {				/* remove file ? */
            remove( file );			/* do it */
         }
         free( file );				/* free memory */
         r = 0;					/* no error */
      } else {					/* no such printer */
         r = -10;				/* set status */
      }
   }
   return( r );					/* return status */
}

#if	defined(TESTBED)

#include	"cmain.h"

int cmain( int argc, char *argv[] )
{
   char  pcommb[50];
   char  pnameb[16];
   fchar pcomm;
   fchar pname;
   fint  i, j;
   fint  m, n;
   fint  ncol, nrow;
   fint  np;
   fint  r;
   fint  pnum[1000];
   
   pcomm.a = pcommb; pcomm.l = sizeof( pcommb );
   pname.a = pnameb; pname.l = sizeof( pnameb );
   np = prntrnum_c( );
   if (np < 0) {
      printf( "prntrnum error %d\n", np );
   } else {
      if (argc == 1) {
         for (n = 0; n < np && n < 1000; n++) {
            pnum[n] = n + 1;
         }
      } else {
         n = 0;
         for (m = 1; m < argc; m++) {
            pnum[n] = atoi( argv[m] );
            if (pnum[n] > 0 && pnum[n] <= np) n++;
         }
         np = n;
      }
      for (n = 0; n < np; n++) {
         fint   rem = 1;

         r = prntrnam_c( &pnum[n], pname );
         if (!r) {
            printf( "Printer name       : %.*s\n", (int) nelc_c( pname ), pname.a );
         } else {
            printf( "prntrnam error     : %d\n", r );
         }
         r = prntrcom_c( &pnum[n], pcomm );
         if (!r) {
            printf( "Printer comment    : %.*s\n", (int) nelc_c( pcomm ), pcomm.a );
         } else {
            printf( "prntrcom error     : %d\n", r );
         }
         r = prntrdim_c( &pnum[n], &ncol, &nrow );
         if (!r) {
            printf( "Printer dimensions : %dx%d\n", ncol, nrow );
         } else {
            printf( "prntrdim error     : %d\n", r );
         }
      }
   }
   return( np );
}
#endif
