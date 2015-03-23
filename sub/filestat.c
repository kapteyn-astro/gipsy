/* filestat.c

	Copyright (c) Kapteyn Laboratorium Groningen 1992
	All Rights Reserved.

#>            filestat.dc2

Function:     filestat

Purpose:      Returns the status of a file.

Category:     UTILITY

File:         filestat.c

Author:       K.G. Begeman

Use:          INTEGER FILESTAT( FILENAME ,     Input      CHARACTER*(*)
                                ATIME ,        Output     INTEGER
                                MTIME ,        Output     INTEGER
                                FTYPE )        Output     INTEGER

              FILESTAT      Return 0 on success, non-zero on error.
              FILENAME      Name of file to check.
              ATIME         Last access time in seconds since 1 Jan 1970.
              MTIME         Last modification time in secs.
              FTYPE         1 regular file
                            2 directory
                            0 other

Warning:      System dependent! Currently implemented for UNIX systems.

Updates:      May 30, 1992: KGB Document created.

#<

Fortran to C interface:

@ integer function filestat( character, integer, integer, integer )

*/

#include	"stddef.h"
#include	"stdio.h"
#include	"stdlib.h"

#include	<sys/file.h>
#include	<sys/stat.h>

#if	defined(_S_IFMT) && !defined(S_IFMT)
#define	S_IFMT	_S_IFMT
#endif
#if	defined(_S_IFREG) && !defined(S_IFREG)
#define	S_IFREG	_S_IFREG
#endif
#if	defined(_S_IFDIR) && !defined(S_IFDIR)
#define	S_IFDIR	_S_IFDIR
#endif

#include	"gipsyc.h"

fint	filestat_c( fchar	filename ,
                    fint	*atime ,
                    fint	*mtime ,
                    fint	*ftype )
{
   char		*path;
   fint		r = 0;
   struct stat	buf;

   path = zadd( filename );
   r = stat( path, &buf );
   if (!r) {
      (*atime) = (fint) buf.st_atime;
      (*mtime) = (fint) buf.st_mtime;
      if ((buf.st_mode & S_IFMT) == S_IFREG) {
         (*ftype) = 1;
      } else if ((buf.st_mode & S_IFMT) == S_IFDIR) {
         (*ftype) = 2;
      } else {
         (*ftype) = 0;
      }
   free( path );
   }
   return( r );
}

#if	defined(TESTBED)
int	main( int argc, char *argv[] )
{
   int	argi = 1;

   while (argi < argc) {
      fint	r;
      fint	atime, mtime, ftype;

      r = filestat_c( tofchar( argv[argi] ), &atime, &mtime, &ftype );
      if (!r) {
         printf( "file  = %s\n", argv[argi] );
         printf( "atime = %10d\n", atime );
         printf( "mtime = %10d\n", mtime );
         printf( "ftype = %10d\n", ftype );
      }
      argi++;
   }
   return( 0 );
}
#endif
