/* ftrunc.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            ftrunc.dc2

Function:     FTRUNC

Purpose:      Truncates a file to a specified number of bytes.

Category:     FILES, UTILITY

File:         ftrunc.c

Author:       K.G. Begeman

Use:          INTEGER FTRUNC( FILENAME,     Input     CHARACTER*(*)
                              NEWSIZE )     Input     INTEGER

              FTRUNC      Returns 0 when file was truncated succesfully,
                          otherwise a non zero value is returned.
              FILENAME    Name of file to truncate. This file must be
                          present!
              NEWSIZE     New file size in bytes.

Warning:      System dependent!

Updates:      Apr 11, 1990: KGB, Document created.

#<

Fortran to C interface:

@ integer function ftrunc( character, integer )

*/

#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "gipsyc.h"

fint ftrunc_c( fchar name, fint *length )
{
#if	defined(__unix__)

   char *onam;
   int   truncate( char *, int );
   fint  r;
   
   onam = zadd( name );
   r = truncate( onam, (int) *length );
   free( onam );
   return( r );

#else

   char  buf[BUFSIZ];
   char  tnam[L_tmpnam];
   char *onam;
   FILE *in;
   FILE *tm;
   fint  r = 0;

   onam = zadd( name );
   if (onam == NULL) return( 1 );
   if ((in = fopen( onam, "rb" )) && tmpnam( tnam ) && (tm = fopen( tnam, "w" ))) {
      long i = 0;

      while (i < *length && !feof( in ) && !ferror( in ) && !ferror( tm )) {
         size_t nr, nw;
         
         nr = fread( buf, sizeof( char ), BUFSIZ, in );
         if ((i + nr) > *length) nr = *length - i;
         nw = fwrite( buf, sizeof( char ), nr, tm );
         i += nr;
      }
      if (ferror( in ) || ferror( tm )) {
         r = 1;
      } else {
         fclose( in );
         fclose( tm );
         remove( onam );
         rename( tnam, onam );
      }
   } else {
      r = 1;
   }
   free( onam );
   return( r );
   
#endif
}

#if	defined(TESTBED)
int main( )
{
   char   cnam[80];
   fchar  name;
   fint   l = 0;
   fint   length = 8000;
   FILE  *tst;
   
   strcpy( cnam, "ftrunc.tst" ); name.a = cnam; name.l =strlen( cnam );
   tst = fopen( cnam, "wb" );
   while (l++ < length) fputc( ' ', tst );
   l = 0;
   while (l++ < length) fputc( ' ', tst );
   fclose( tst );
   printf( "ftrunc : %d ", ftrunc_c( name, &length ) );   
   tst = fopen( cnam, "r" );
   fseek( tst, 0, SEEK_END );
   if (ftell( tst ) == length) {
      printf( "okay\n" );
   } else {
      printf( "wrong\n" );
   }
   fclose( tst );
   return( 0 );
}
#endif
