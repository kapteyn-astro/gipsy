/* fextend.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            fextend.dc2

Function:     fextend

Purpose:      Extends files to a certain number of bytes.

Category:     FILES

File:         fextend.c

Author:       K.G. Begeman

Use:          INTEGER FEXTEND( NAME ,    Input     CHARACTER*(*)
                               SIZE )    Input     INTEGER

              FEXTEND     Returns:
                          0  file extended to SIZE bytes
                          1  error in opening file
                          2  error in reporting current file size
                          3  error in extending file
              NAME        Name of file to extend. If the file
                          does not exist, it will be created.
              SIZE        Extend the file NAME to SIZE bytes. If
                          SIZE is less than the actual size of the
                          file, nothing happens, otherwise zeroes are
                          written to extend the file to SIZE bytes.

Updates:      Jul  4, 1990: KGB, Document created.

#<

Fortran to C interface:

@ integer function fextend( character, integer )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */

static	char	zero[BUFSIZ];		/* static buffer with zeroes */

fint fextend_c( fchar file_name, fint *new_size )
{
   char *fn;				/* pointer to filename */
   fint  old_size = 0;			/* old size of file */
   fint  r = 0; 			/* return value */
   FILE *f;				/* file descriptor */
   int	 count;				/* number of bytes to write */

   fn = zadd( file_name );		/* pointer to zero terminated string */
   f = fopen( fn, "a+" );		/* file pointer at end */
   if (f == NULL) r = 1;		/* error opening file */
   if (!r) {
      old_size = ftell( f );		/* report old size of file */
      if (old_size == -1) r = 2;	/* error reporting position */
   }
   while (!r && (old_size < *new_size)) {	/* loop to extend file */
      count = (BUFSIZ - old_size%BUFSIZ);	/* extend size */
      old_size += count;			/* new size */
      if (old_size > *new_size) count -= (old_size - *new_size);
      if (fwrite( zero, sizeof( char ), count, f) != count ) r = 3;
   }
   fclose( f ); 			/* close file */
   free( fn );				/* free allocated memory */
   return( r ); 			/* return to caller */
}

#if     defined(TESTBED)
void main( int argc, char *argv[] )
{
   char name[80];
   fint size;

   strncpy( name, argv[1], 80 );
   size = atol( argv[2] );
   printf( "fextend = %d\n", fextend_c( tofchar( name ), &size ) );
}
#endif
