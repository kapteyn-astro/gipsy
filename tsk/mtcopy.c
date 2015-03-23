/* mtcopy.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            mtcopy.dc1

Program:      MTCOPY

Purpose:      Copies selected files from one tape device to another
              tape device. Tape device can be a real tape unit or a
              directory on disk.

Category:     TAPES, UTILITY

File:         mtcopy.c

Author:       K.G. Begeman

Keywords:

   INTAPE=    Input tape device                      [list of all devices]
   
              Name of tape device where to copy files from.
              The list is extracted from the local file 
              $gip_loc/mtdevices. If your tape is not in the list, 
              you can enter the full device specification, e.g.
              INTAPE=/dev/rmt/0mn


   OUTTAPE=   Output tape device                      [list of all devices]
   
              Name of tape device where to copy files to.
              See also at INTAPE=


   INFILES=   Files on input tape device which should be copied [all files] 
   
              When the default is used, files are copied
              from the input tape until an empty file is found (two
              subsequent tape marks). Note that filenumber zero
              indicates the current file, file number 1 the next file
              and file number -1 the previous file.


   SKIP=      Number of files to skip on OUTTAPE      [SKIP to End of Info]

              The first input file will be stored at this location, the
              second at the next location etc.
              Note that for new (empty) tapes you should give SKIP=0.


   INREWIND=  Rewind INTAPE after copying                             [YES]


   OUTREWIND= Rewind OUTTAPE after copying                             [NO]
   
              If NO, the outtape will
              be positioned between the last two tapemarks, so that
              you can easily add other files.


** REBLOCK=   New blocking factor on output tape device. 

              If the last
              block of a file is not completely filled, it will be
              padded with zeroes [blocking as on input tape device].



Updates:      Apr 24, 1990: KGB, Document created.
              Feb  8, 1993: KGB, Skip till EOI implemented.
              Oct 26, 1993: KGB, Keyword INREWIND= added. 
              Feb 23, 2001: VOG, Small changes in documentation.

#<

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"string.h"		/* <string.h> */
#include	"ctype.h"		/* <ctype.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"gipsyc.h"		/* GIPSY definitions and symbols */
#include	"cmain.h"		/* C main program */
#include	"init.h"		/* define init_c */
#include	"finis.h"		/* define finis_c */
#include	"anyout.h"		/* define anyout_c */
#include	"error.h"		/* define error_c */
#include	"nelc.h"		/* define nelc_c */
#include	"status.h"		/* define status_c */
#include	"userint.h"		/* define userint_c */
#include	"userlog.h"		/* define userlog_c */
#include	"usertext.h"		/* define usertext_c */

/* Now declare the tape io routines: */
extern	fint	mtopen_c( fchar );
extern	fint	mtclose_c( fint * );
extern	fint	mtrew_c( fint * );
extern	fint	mtread_c( fint *, char *, fint * );
extern	fint	mtwrite_c( fint *, char *, fint * );
extern	fint	mtweof_c( fint *, fint * );
extern	fint	mtfsf_c( fint *, fint * );
extern	fint	mtbsf_c( fint *, fint * );
extern	fint	mtfsr_c( fint *, fint * );
extern	fint	mtbsr_c( fint *, fint * );
extern	fint	mtname_c( fint *, fchar );

#define	finit(d,s)	(d.a=s,d.l=sizeof(s))

#define	INREWIND_KEY	tofchar("INREWIND=")
#define	INREWIND_MES	tofchar("Rewind INTAPE after copying? [YES]" )
#define	INTAPE_DEV	tofchar("?INTAPE=Input tape device [list of all devices]")
#define	INTAPE_ERR	tofchar("No such device or device not available")
#define	OUTREWIND_KEY	tofchar("OUTREWIND=")
#define	OUTREWIND_MES	tofchar("Rewind OUTTAPE after copying? [NO]")
#define	OUTTAPE_DEV	tofchar("?OUTTAPE=Output tape device [list of all devices]")
#define	OUTTAPE_ERR	tofchar("No such device or device not available")
#define	INFILES_KEY	tofchar("INFILES=")
#define	INFILES_MES	tofchar("File numbers to copy [all files]")
#define	SKIP_KEY	tofchar("SKIP=")
#define	SKIP_MES	tofchar("Number of files to skip on OUTTAPE [Skip to EOI]")
#define	REBLOCK_KEY	tofchar("REBLOCK=")
#define	REBLOCK_MES	tofchar("Blocking factor on output tape [no reblock]")
#define	REBLOCK_ERR	tofchar("Illegal blocking factor")
#define	VERSION		"0.3"

#define	MAXTXTLEN	80			/* max length of text strings */
#define	MAXFILES	1000			/* max number of files we allow */
#define	MINBLOCKSIZE	28800			/* initial size tape blocking */

MAIN_PROGRAM_ENTRY				/* here is where we start */
{
   bool   rewind1;				/* rewind INTAPE */
   bool   rewind2;				/* rewind OUTTAPE */
   char   buf1[MAXTXTLEN];			/* buffer for name of INTAPE */
   char   buf2[MAXTXTLEN];			/* buffer for name of OUTTAPE */
   char  *dat1 = NULL;				/* pointer to input data */
   char  *dat2 = NULL;				/* pointer to output data */
   char   text[MAXTXTLEN]; 			/* buffer for text strings */
   fchar  tape1;				/* points to buf1 */
   fchar  tape2;				/* points to buf2 */
   fint   nbyt1;				/* number of bytes read */
   fint   nbyt2;				/* number of bytes written */
   fint   ndat1;				/* number of bytes in dat1 */
   fint   ndat2;				/* number of bytes in dat2 */
   fint   mtid1;				/* device id INTAPE */
   fint   mtid2;				/* device id OUTTAPE */
   fint   fatal = 4;				/* fatal error */
   fint   files[MAXFILES];			/* buffer for input file numbers */
   fint   nfil1;				/* number of files to copy */
   fint   nfil2;				/* number of first output file */
   fint   one = 1;				/* just one */
   fint   deflev;				/* default level for userxxx */
   fint   nitems;				/* item counter */
   fint   reblock;				/* reblocking factor */
   fint   ndone = 0;				/* bytes read */
   fint   ncur1;				/* current input file number */
   fint   ncur2;				/* current output file number */
   fint   mterr1 = 0;				/* error return from INTAPE */
   fint   mterr2 = 0;				/* error return from OUTTAPE */
   fint   screen = 1;				/* output to screen */

   /*
    * set up the character strings for the device names and create
    * the buffers for input and output data.
    */
   finit(tape1,buf1);
   finit(tape2,buf2);
   dat1 = realloc( dat1, sizeof( char ) * MINBLOCKSIZE ); ndat1 = MINBLOCKSIZE;
   dat2 = realloc( dat2, sizeof( char ) * MINBLOCKSIZE ); ndat2 = MINBLOCKSIZE;

   init_c( );					/* get in touch with HERMES */
   IDENTIFICATION("MTCOPY",VERSION);		/* this we are */
   if (dat1 == NULL || dat2 == NULL) {		/* error */
      error_c( &fatal, tofchar( "Memory allocation problems!" ) );
   }
   if ((mtid1 = mtopen_c( INTAPE_DEV )) < 0) {	/* open INTAPE */
      error_c( &fatal, INTAPE_ERR );		/* display error message */
      finis_c( );				/* quit */
   }
   mtname_c( &mtid1, tape1 );			/* get device name INTAPE */
   if ((mtid2 = mtopen_c( OUTTAPE_DEV )) < 0) {	/* open OUTTAPE */
      error_c( &fatal, OUTTAPE_ERR );		/* display error message */
      finis_c( );				/* quit */
   }
   mtname_c( &mtid2, tape2 );			/* get device name OUTTAPE */
   deflev = 1;					/* default level */
   nitems = MAXFILES;				/* max number of files */
   nfil1 = userint_c( files, &nitems, &deflev, INFILES_KEY, INFILES_MES );
   deflev = 1;					/* default level */
   nitems = 1;					/* max number of items */
   ncur1 = 0;					/* current input file number */
   ncur2 = 0;					/* current output file number */
   if (!userint_c( &nfil2, &nitems, &deflev, SKIP_KEY, SKIP_MES )) {
      /*
       * First we try to skip a TM backwards. We might already be at EOI
       * so we don't want to destroy anything.
       */
      mterr2 = mtbsf_c( &mtid2, &one );		/* skip TM backwards */
      if ( mterr2 == 1 ) {			/* done */
         ncur2--;				/* decrease counter */
      } else if ( mterr2 == 0 ) {		/* at BOT */
         anyout_c( &screen, tofchar( "Tape at BOT" ) );
      } else {					/* an error */
         sprintf( text, "MTBSF error %d", mterr2 );
         error_c( &fatal, tofchar( text ) );	/* show user */
      }
      /*
       * Next we try to find EOI by skipping a TM and reading the next
       * record. If the read returns 0, we have found EOI.
       */
      while ( ( ( mterr2 = mtfsf_c( &mtid2, &one ) ) == 1 ) && ( ( ndone = mtread_c( &mtid2, dat2, &one ) ) > 0 ) ) {
         ncur2++;				/* increase */
      }
      if ( ndone == -2 ) ndone = 0;		/* EOD/EOT not fatal! */
      if (( mterr2 == 1 ) && ( ndone == 0 )) {	/* okay */
         sprintf( text, "Skipped %d files forward", ++ncur2 );
         anyout_c( &screen, tofchar( text ) );	/* show user */
         mterr2 = mtbsf_c( &mtid2, &one );	/* one TM back */
      }
      if (( mterr2 != 1 ) || ( ndone != 0 )) {	/* an error */
         sprintf( text, "MTFSF error %d, MTREAD error %d", mterr2, ndone );
         error_c( &fatal, tofchar( text ) );	/* show user */
      }
   } else if (nfil2 > 0) {			/* position output tape */
      mterr2 = mtfsf_c( &mtid2, &nfil2 );	/* skip forward */
      if (mterr2 >= 0) {			/* okay */
         ncur2 = mterr2;			/* file counter */
         sprintf( text, "Skipped %d files forward", ncur2 );
         anyout_c( &screen, tofchar( text ) );	/* show user */
      } else {					/* error */
         sprintf( text, "MTFSF error %d", mterr2 );
         error_c( &fatal, tofchar( text ) );	/* show user */
      }
   } else if (nfil2 < 0) {			/* skip backward */
      nfil2 = 1 - nfil2;			/* new count */
      if ((mterr2 = mtbsf_c( &mtid2, &nfil2 )) == nfil2) {
         ncur2 = 1 - mterr2;			/* current position */
         mterr2 = mtfsf_c( &mtid2, &one );	/* forward skip one tape mark */
         if (mterr2 != 1) {			/* error */
            sprintf( text, "MTFSF error %d", mterr2 );
            error_c( &fatal, tofchar( text ) );	/* show user */
         }
         sprintf( text, "Skipped %d files backward", ncur2 );
         anyout_c( &screen, tofchar( text ) );	/* show user */
      } else if (mterr2 >= 0 ) {		/* backwards into BOT */
         ncur2 = 0;				/* BOT */
         anyout_c( &screen, tofchar( "Tape at BOT" ) );
         sprintf( text, "Skipped %d files backward", mterr2 );
         anyout_c( &screen, tofchar( text ) );	/* show user */
      } else {					/* an error */
         sprintf( text, "MTBSF error %d", mterr2 );
         error_c( &fatal, tofchar( text ) );	/* show user */
      }
   }
   deflev = 1;					/* default allowed */
   nitems = 1;					/* number of items */
   rewind1 = TRUE;				/* the default */
   userlog_c( &rewind1, &nitems, &deflev, INREWIND_KEY, INREWIND_MES );
   deflev = 1;					/* default allowed */
   nitems = 1;					/* number of items */
   rewind2 = FALSE;				/* the default */
   userlog_c( &rewind2, &nitems, &deflev, OUTREWIND_KEY, OUTREWIND_MES );
   deflev = 2;					/* default and hidden */
   nitems = 1;					/* max number of items */
   if (!userint_c( &reblock, &nitems, &deflev, REBLOCK_KEY, REBLOCK_MES )) {
      reblock = 0;				/* default */
   }
   if (reblock < 0) {
      error_c( &fatal, REBLOCK_ERR );		/* display error message */
      finis_c( );				/* quit */
   }
   if (reblock) {				/* create space */
      dat2 = realloc( dat2, sizeof( char ) * reblock );
      if (dat2 == NULL) {			/* error */
         error_c( &fatal, tofchar( "Memory allocation problems!" ) );
      }
   }
   ndone = 0;					/* reset number of files copied */
   do {						/* loop to copy files */
      fint nout = 0;
      fint nread = 0;
      fint nrec1 = 0;
      fint nrec2 = 0;

      if (nfil1) {				/* position tape if read not sequential */
         fint nfsf = files[ndone] - ncur1;	/* number of files to skip forward */

         if (nfsf > 0) {			/* skip forward */
            mterr1 = mtfsf_c( &mtid1, &nfsf );
         } else if (nfsf < 0) {			/* skip backward */
            nfsf = 1 - nfsf;
            if ((mterr1 = mtbsf_c( &mtid1, &nfsf )) == nfsf) {
               nfsf = 1;
               mterr1 = mtfsf_c( &mtid1, &nfsf );
            }
         }
         ncur1 = files[ndone];			/* current file number */
      }
      nbyt1 = nbyt2 = 0;			/* reset */
      (void) sprintf( text, "Copy from %.*s (%d) to %.*s (%d)", (int) nelc_c( tape1 ), tape1.a, ncur1, (int) nelc_c( tape2 ), tape2.a, ncur2 );
      status_c( tofchar( text ) );
      while ((mterr1 = mtread_c( &mtid1, dat1, &ndat1 )) > 0) {	/* read */
         if (mterr1 > ndat1) {			/* record too large */
            ndat1 = mterr1;
            dat1 = realloc( dat1, sizeof( char ) * ndat1 );	/* larger buffer */
            if (dat1 == NULL) {			/* error */
               error_c( &fatal, tofchar( "Memory allocation problems!" ) );
            }
            if (!reblock) {			/* increase out buffer accordingly */
               ndat2 = ndat1;
               dat2 = realloc( dat2, sizeof( char ) * ndat2 );
               if (dat2 == NULL) {		/* error */
                  error_c( &fatal, tofchar( "Memory allocation problems!" ) );
               }
            }
            nitems = 1;				/* try to reread last record */
            if ((mterr1 = mtbsr_c( &mtid1, &nitems )) != nitems) break;
            if ((mterr1 = mtread_c( &mtid1, dat1, &ndat1 )) < 0) break;
         }
         nbyt1 += mterr1;			/* number of bytes read */
         nrec1 += 1;				/* next input record */
         nread = mterr1;			/* number of bytes read */
         if (reblock) {				/* loop for reblocking */
            fint nmove;
            fint noff1 = 0;

            while ((nmove = ((nout + mterr1 - noff1) > reblock ? (reblock - nout) : (mterr1 - noff1)))) {
               (void) memmove( &dat2[nout], &dat1[noff1], nmove );	/* move */
               noff1 += nmove;
               nout += nmove;
               if (nout == reblock) {		/* write to output tape device */
                  if ((mterr2 = mtwrite_c( &mtid2, dat2, &nout )) != nout) break;
                  nbyt2 += mterr2;		/* number of bytes written */
                  nrec2 += 1;
                  nout = 0;
               }
            }
            if (mterr2 < 0) break;
         } else {
            if ((mterr2 = mtwrite_c( &mtid2, dat1, &mterr1 )) != mterr1) break;
            nbyt2 += mterr2;			/* number of bytes written */
            nrec2 += 1;
         }
      }
      if (mterr1 >= 0 && mterr2 >= 0) {		/* end-of-file enncur2ered */
         if (reblock && nout) {
            (void) memset( &dat2[nout], 0, reblock - nout );
            if ((mterr2 = mtwrite_c( &mtid2, dat2, &reblock )) != reblock) break;
            nbyt2 += mterr2;			/* number of bytes written */
            nrec2 += 1;
         }
         if (!nfil1 && !nrec1) break;		/* end-of-information */
         (void) mtweof_c( &mtid2, &nitems );
         (void) sprintf( text, "Input  FILE number : %8d (%8d records, %8d bytes)", ncur1, nrec1, nbyt1 );
         anyout_c( &screen, tofchar( text ) );
         (void) sprintf( text, "Output FILE number : %8d (%8d records, %8d bytes)", ncur2, nrec2, nbyt2 );
         anyout_c( &screen, tofchar( text ) );
         ncur1 += 1;				/* next input file */
         ncur2 += 1;				/* next output file */
      } else {
         break;
      }
   } while (++ndone != nfil1);			/* loop until all files copied */
   if (mterr1 < 0) {				/* error on tape1 */
      (void) sprintf( text, "Error (%d) on input tape device", mterr1 );
      anyout_c( &screen, tofchar( text ) );
   }
   if (mterr2 < 0) {
      (void) sprintf( text, "Error (%d) on output tape device", mterr2 );
      anyout_c( &screen, tofchar( text ) );
   }
   nitems = 1;
   (void) mtweof_c( &mtid2, &nitems );		/* write final tape mark */
   if (tobool(rewind1)) {			/* rewind tape 1 ? */
      (void) mtrew_c( &mtid1 );			/* rewind tape 1 */
   }
   if (tobool(rewind2)) {			/* rewind tape 2 ? */
      (void) mtrew_c( &mtid2 );			/* rewind tape 2 */
   } else {					/* no rewind */
      (void) mtbsf_c( &mtid2, &one );		/* one tapemake back */
   }
   (void) mtclose_c( &mtid1 );			/* close tape 1 */
   (void) mtclose_c( &mtid2 );			/* close tape 2 */
   finis_c( );					/* we quit */
   return( EXIT_SUCCESS );
}
