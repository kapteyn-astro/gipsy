/* fsize.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            fsize.dc2

Function:     fsize

Purpose:      Determines the size of a file in bytes.

Category:     FILES

File:         fsize.c

Author:       K.G. Begeman

Use:          INTEGER(KIND=8) FSIZE( NAME )    Input     CHARACTER*(*)

              FSIZE       Returns:
                          >= 0  size of file in bytes
                          -1  error in opening file
                          -2  error in reporting current file size
                          -3  filename too long
              NAME        Name of file for which to determine the
                          size.

Updates:      Jul  4, 1990: KGB, Document created.
              Mar 24, 2011: JPT, Large file support. Return INTEGER(KIND=8).
              Jun  7, 2011: JPT, Changes to avoid update problems.

#<

Explicit header overriding f2cvv-generated header:

#> fsize.h
extern fint8 fsize_c(fchar);
#<

Fortran to C interface:

@ integer function fsize( character )

This should be integer*8 but sometimes this cannot be used yet because
the required new f2cvv is not yet available in this phase of the update.

*/

#include	"stddef.h"		/* <stddef.h> */
#include        "stdio.h"               /* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include        "gipsyc.h"              /* GIPSY symbols and definitions */
#include	"nelc.h"		/* define nelc_c */

#if	defined(__unix__)		/* only for unix systems */

#include	<sys/types.h>		/* define some unix types */
#include	<sys/stat.h>		/* define stat structure */

extern	int	lstat();		/* returns file status on unix */

#elif	defined(__vms__)

#include	<stat.h>		/* use stat instead of ftell */

#endif

fint8 fsize_c( fchar file_name )
{
   char  fn[FILENAME_MAX+1];		/* internal buffer for filename */
   fint  len = nelc_c( file_name );	/* length of filename */
   fint8 r = 0;				/* return value */
   
   if (len > FILENAME_MAX) {		/* filename too long */
      r = -3;				/* error code */
   } else {				/* filename okay */
#if	defined(__unix__)		/* unix systems */
      struct stat stat_buf;		/* file status buffer */

      strncpy( fn, file_name.a, len );	/* copy filename */
      fn[len] = '\0';			/* trailing zero byte */
      r = lstat( fn, &stat_buf );	/* get file status */
      if (!r) {				/* obtained file status */
         r = (fint8) stat_buf.st_size;	/* size of file */
      } else {				/* error obtaining file status */
         r = -2;			/* error code */
      }
#elif	defined(__vms__)
      FILE *f;				/* file descriptor */
      stat_t buffer;			/* file status buffer */
      
      strncpy( fn, file_name.a, len );	/* copy filename */
      fn[len] = '\0';			/* trailing zero byte */
      f = fopen( fn, "r" );		/* file pointer at end */
      if (f == NULL) {			/* error opening file */
         r = -1;			/* error code */
      } else {
         fclose( f );			/* close file */
         r = stat( fn, &buffer );	/* get file status */
         if (r) {			/* error obtaining file status */
            r = -2;			/* error code */
         } else {			/* no error */
            r = buffer.st_size;		/* size of file in bytes */
         }
      }
#else					/* other systems */
      FILE *f;				/* file descriptor */

      strncpy( fn, file_name.a, len );	/* copy filename */
      fn[len] = '\0';			/* trailing zero byte */
      f = fopen( fn, "r" );		/* file pointer at end */
      if (f == NULL) {			/* error opening file */
         r = -1;			/* error code */
      } else {				/* file open */
         if (fseek( f, 0, SEEK_END )) {	/* error positioning */
            r = -2;			/* error code */
         } else {			/* position set */
            r = ftell( f );		/* report old size of file */
            if (r < 0) r = -2;		/* error reporting position */
         }
         fclose( f );			/* close the file */
      }
#endif
   }
   return( r );				/* return to caller */
}

#if     defined(TESTBED)   
void main( int argc, char *argv[] )
{
   char name[FILENAME_MAX+1];
   int  l;
   
   for (l = 1; l < argc; l++) {
      int len = strlen( argv[l] );
      
      if (len > FILENAME_MAX) len = FILENAME_MAX;
      strncpy( name, argv[l], len );
      name[len] = 0;
      printf( "%40.*s (%ld)\n", len, name, fsize_c( tofchar( name ) ) );
   }
}
#endif
