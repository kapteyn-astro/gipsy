/* flist.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            flist.dc2

Function:     FLIST

Purpose:      Lists all files in a directory

Category:     FILES

File:         flist.c

Author:       K.G. Begeman

Use:          INTEGER FLIST( DIRNAME  ,   Input     CHARACTER*(*)
                             DIRENTRY )   Output    CHARACTER*(*)

              FLIST       Returns:
                           0: Next directory entry returned in DIRENTRY.
                          -1: Directory entry trunctated because DIRENTRY
                              is not large enough. 
                          -2: No more entries in directory.
                          -3: Error reading directory.
                          -4: Cannot obtain current directory.
              DIRNAME     Name of directory to list. If blank, the
                          current directory will be listed.
              DIRENTRY    Directory entry found in DIRNAME.

Notes:        The directory name may be a logical name. If it is
              translatable, it will be translated.

Example:      CHARACTER*80 NAME

              CALL ANYOUT( 0, 'List of files in Directory DB1:' )
              WHILE (FLIST( 'DB1:', NAME ) .EQ. 0)
                 CALL ANYOUT( 0, NAME )
              CWHILE

Warning:      System dependent! Currently implemented for UNIX and VMS
              systems.

Updates:      Sep 25, 1990: KGB, Document created.
              Oct 16, 1995: KGB, Bug removed (input file name lengths).
              Mar 24, 2011: JPT, Changed format for file size in testbed.

#<

Fortran to C interface:

@ integer function flist( character, character )

*/

#include	"stddef.h"			/* <stddef.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */

#if	defined(__unix__)			/* for unix systems only */

#if	defined(__bsd__)

#include	<sys/types.h>
#include	<sys/dir.h>

#else

#include	<sys/types.h>
#include	<dirent.h>

#endif

static	DIR	*dirp = NULL;			/* unix directory pointer */

#elif	defined(__vms__)			/* for vms systems only */

#include	<descrip.h>			/* descriptor definitions */
#include	<rmsdef.h>			/* rms definitions */

extern	long	lib$find_file( );		/* finds next directory entry */
extern	long	lib$find_file_end( );		/* quits search loop */

static	long	dirp = 0;			/* context address */
static	char	vmsdir[FILENAME_MAX+1];		/* vms directory name */
static	char	vmsnam[FILENAME_MAX+1];		/* vms filename */

#endif

#include 	"gipsyc.h"			/* GIPSY symbols and definitions */
#include	"fname.h"			/* define fname_c */
#include	"fsize.h"			/* define fsize_c */
#include	"nelc.h"			/* define nelc_c */

static  char    olddir[FILENAME_MAX+1];		/* buffer for directory name */

fint flist_c( fchar dirname, fchar direntry )
{
#if	defined(__unix__)
   fint           len = nelc_c( dirname );	/* length of filename */
#if     defined(__sysv__)
   struct dirent *dp;				/* directory entry pointer */
#else
   struct direct *dp;				/* directory entry pointer */
#endif
   if (dirp != NULL && len != strlen( olddir ) && strncmp( dirname.a, olddir, len )) {
      closedir( dirp );
      dirp = NULL;
   }
   if (dirp == NULL) {
      char  synb[FILENAME_MAX+1];		/* buffer for translated dir */
      fchar syn;				/* points to synb */

      syn.a = synb; syn.l = FILENAME_MAX;	/* initialize f character */
      if (len > FILENAME_MAX) {			/* directory name too long */
         len = FILENAME_MAX;			/* truncate without error */
      }
      if (len) {
         strncpy( olddir, dirname.a, len );	/* save directory name */
      }
      olddir[len] = '\0';			/* add trailing zero byte */
      if (len) {
         if (fname_c( dirname, syn )) {		/* cannot translate name */
            int	n;				/* loop counter */

            for (n = 0; n < len; n++) synb[n] = dirname.a[n];
            while (n < FILENAME_MAX) synb[n++] = ' ';
         }
         synb[nelc_c( syn )] = '\0';		/* add trailing zero byte */
      } else {
#if	defined(__sysv__)
         char	*getcwd( );			/* returns current dir */

         if (getcwd( synb, FILENAME_MAX + 1 ) == NULL) {
            return( -4 );
         }     
#else
         char	*getwd( );			/* returns current dir */

         if (getwd( synb ) == NULL) {
            return( -4 );
         }
#endif
      }
      dirp = opendir( synb );			/* open directory */
      if (dirp == NULL) {			/* error opening directory */
         return( -3 );				/* return code */
      }
   }
   dp = readdir( dirp );			/* next directory entry */
   if (dp == NULL) {				/* no more entries */
      closedir( dirp );				/* close directory */
      dirp = NULL;				/* reset directory pointer */
      return( -2 );				/* return code */
   } else {					/* another entry found */
      int l;					/* counter */
      int ll = strlen( dp->d_name );

      for (l = 0; l < ll && l < direntry.l; l++) {
         direntry.a[l] = dp->d_name[l];		/* copy name */
      }
      while (l < direntry.l) {			/* loop to add trailing blanks */
         direntry.a[l++] = ' ';			/* trailing blanks */
      }
      if ( direntry.l < ll ) {			/* output buffer too small */
         return( -1 );				/* return code */
      } else {					/* no error */
         return( 0 );				/* return code */
      }
   }
#elif	defined(__vms__)
   fint len = nelc_c( dirname );		/* length of filename */
   long status;					/* status return */
   $DESCRIPTOR( dir, vmsdir );			/* character string */
   $DESCRIPTOR( nam, vmsnam );			/* character string */

   if (dirp != 0 && len != strlen( olddir ) && strncmp( dirname.a, olddir, len )) {
      status = lib$find_file_end( &dirp );	/* close search loop */
      dirp = 0;					/* reset */
   }
   if (dirp == 0) {				/* new search loop */
      int l;					/* loop counter */

      if ((len+5) > FILENAME_MAX) {		/* directory name too long */
         len = FILENAME_MAX - 5;		/* truncate without error */
      }
      strncpy( olddir, dirname.a, len );
      olddir[len] = '\0';			/* add trailing zero byte */
      strcpy( vmsdir, olddir );			/* copy directory name */
      strcat( vmsdir, "*.*;*" );		/* add wildcards */
      l = strlen( vmsdir );			/* length of string */
      while (l < FILENAME_MAX) {		/* fill with blanks */
         vmsdir[l++] = ' ';			/* blank */
      }
      vmsdir[l] = '\0';				/* add trailing zero byte */
   }
   status = lib$find_file( &dir, &nam, &dirp );	/* find next directory entry */
   if (status == RMS$_NMF) {			/* end of directory list */
      status = lib$find_file_end( &dirp );	/* set end */
      dirp = 0;					/* reset */
      return( -2 );				/* return code */
   }
   if (status != RMS$_NORMAL) {			/* error */
      return( -3 );				/* return code */
   } else {					/* found a file */
      int l, l1, l2 = FILENAME_MAX;		/* counters */
      
      while (l2 && (vmsnam[l2-1] == ' ' || vmsnam[l2-1] == '\0')) l2--;
      l1 = l2;
      while (l1 && (vmsnam[l1-1] != ']' && vmsnam[l1-1] != ':')) l1--;
      for (l = 0; l < (l2 - l1) && l < direntry.l; l++) {
         direntry.a[l] = vmsnam[l1+l];		/* ship out directory entry */
      }
      while (l < direntry.l) direntry.a[l++] = ' ';
      if ((l2 - l1) > direntry.l) {		/* name too long */
         return( -1 );				/* return code */
      }
      return( 0 );				/* return to caller */
   }
#else
   NOT IMPLEMENTED
#endif
}

#if	defined(TESTBED)
#include "cmain.h"


int	cmain( int argc, char **argv )
{
   char  direntryb[FILENAME_MAX+1];
   char  filenameb[FILENAME_MAX+1];
   fchar dirname;
   fchar direntry;
   fchar filename;
   fint  f;

      
   direntry.a = direntryb; direntry.l = FILENAME_MAX;
   filename.a = filenameb; filename.l = FILENAME_MAX;
   dirname = tofchar( argv[argc-1] );
   while ( !(f = flist_c( dirname, direntry )) ) {
      direntryb[nelc_c( direntry )] = 0;
      (void) fname_c( dirname, filename );
      filenameb[nelc_c( filename )] = 0;
      strcat( filenameb, direntryb );
      printf( "%-40s %ld\n", filenameb, fsize_c( filename ) );
   }
   if (f) printf( "flist = %d\n", f );
   return( 0 );
}

#endif
