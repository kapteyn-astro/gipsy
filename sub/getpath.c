/* getpath.c

        Copyright (c) Kapteyn Laboratorium Groningen 1992
        All Rights Reserved.
                
#>            getpath.dc2

Function:     GETPATH

Purpose:      Gets full pathname of a file. It tries to resolve links.

Category:     FILES

File:         getpath.c

Author:       K.G. Begeman

Use:          INTEGER GETPATH( PATH )    Input/Output   CHARACTER*(*)

              GETPATH      0: success.
                          -1: cannot get entry from password file.
                          -2: full pathname too long for PATH.
              PATH        On input, the filename for which the full
                          path is wanted, on output, the full pathname.

Warnings:     Only works on UNIX systems.

Updates:      Jun 10, 1992: KGB Document created.

#<

Fortran to C interface:

@ integer function getpath( character )

*/

#include	"stddef.h"
#include	"stdio.h"
#include	"stdlib.h"
#include	"string.h"

#if	defined(__GNUC__) && defined(_CONVEX_SOURCE)
#define	fgetpwent(x)	fgetpwent(FILE *)
#endif

#define	getpwnam	GETPWNAM
#define	getpwuid	GETPWUID

#include	<pwd.h>

#undef	getpwnam
#undef	getpwuid

#include	"gipsyc.h"
#include	"nelc.h"

#if	defined(__sysv__)
extern	char	*getcwd( char *path, int size );
#else
extern	char	*getwd( char *path );
#define	getcwd(path,size)	getwd(path)
#endif

fint	getpath_c( fchar path )
{
   char	file1[FILENAME_MAX+1];			/* buffer for input path */
   char	file2[FILENAME_MAX+1];			/* buffer for output path */
   char	file3[FILENAME_MAX+1];			/* path with links resolved */
   char	*o, *p;					/* pointers */
   int	l = nelc_c( path );			/* length of path */
   int	readlink( );				/* reads links */

   strncpy( file1, path.a, l );			/* copy input path */
   file1[l] = '\0';				/* terminate with zero */
   /*
    * remove //
    */
   while ((p = strstr( file1, "//")) != NULL) {
      if (p[2]) {
         strcpy( p, &p[1] );
      } else {
         p[1] = '\0';
      }
   }
   if (file1[0] == '~') {			/* a tilde operation */
      for (p = file2, o = &file1[1]; *o && *o != '/'; *p++ = *o++);
      *p = '\0';				/* terminate */
      if (file2[0] == '\0') {			/* no username attached */
         char	*home = getenv( "home" );	/* translate 'home' */

         if (home != NULL) {			/* translation ok */
            strcpy( file2, home );		/* path = home */
         } else {				/* get if from password file */
            int			getuid( );	/* .. by uid */
            struct passwd	*pw;		/* passwd struct */
            struct passwd	*getpwuid( );	/* .. */

            pw = getpwuid( getuid( ) );		/* get entry */
            if (pw == NULL) return( -1 );	/* error */
            strcpy( file2, pw->pw_dir );	/* path = home */
         }
      } else {					/* username found */
         struct passwd 	*pw;			/* passwd struct */
         struct passwd	*getpwnam( );		/* .. */

         pw = getpwnam( file2 );		/* get entry */
         if (pw == NULL) return( -1 );		/* error */
         strcpy( file2, pw->pw_dir );		/* path from password file */
      }
      strcat( file2, o );			/* append rest */
   } else if (file1[0] != '/') {		/* relative path */

      getcwd( file2, FILENAME_MAX + 1 );	/* get current directory */
      if (file1[0] == '.' && file1[1] == '/') {	/* replace */
         strcat( file2, &file1[1] );		/* append rest */
      } else {					/* put cwd in front */
         strcat( file2, "/" );			/* with a slash */
         strcat( file2, file1 );		/* append rest */
      }
   } else {					/* starts at root */
      strcpy( file2, file1 );			/* copy */
   }
   /*
    * Now we need to check for /..
    */
   while ((p = strstr( file2, "/.." )) != NULL) {
      for ( o = p; --o > file2 && *o != '/';);
      if (p[3]) {
         strcpy( o, &p[3] );
      } else {
         o[0] = '\0';
      }
   }
   /*
    * Next try to find links, and resolve them.
    */
   p = strtok( file2, "/" );
   file3[0] = '\0';
   while (p != NULL) {
      strcat( file3, "/" );
      strcat( file3, p );
      l = readlink( file3, file1, FILENAME_MAX );
      if (l != -1) {
         file1[l] = '\0';
         strcpy( file3, file1 );
      }
      p = strtok( NULL, "/" );
   }
   if (!file3[0]) strcpy( file3, "/" );		/* add a slash */
   l = strlen( file3 );				/* length of output path */   
   if (l > path.l) {				/* does not fit in */
      return( -2 );				/* error */
   } else {					/* copy */
      int	i;

      for (i = 0; i < l; i++) {
         path.a[i] = file3[i];
      }
      while (i < path.l) path.a[i++] = ' ';	/* append blanks */
      return( 0 );
   }
}

#if	defined(TESTBED)

#include	"cmain.h"

int	cmain( int argc, char *argv[] )
{
   char		buf[FILENAME_MAX+1];
   fchar	path;
   int		argi;

   
   for (argi = 1; argi < argc; argi++) {
      fint	r;
      int	i;

      strcpy( buf, argv[argi] );
      for (i = strlen( buf ); i < (sizeof(buf) - 1); buf[i++] = ' ');
      path.a = buf;
      path.l = FILENAME_MAX;
      if (!(r = getpath_c( path ))) {
         printf( "%35s %35.*s\n", argv[argi], nelc_c( path ), path.a );
      } else {
         printf( "getpath error = \n", r );
      }
   }
   return( 0 );
}
#endif
