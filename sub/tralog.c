/* tralog.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

*/

#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "gipsyc.h"

static int nchar( fchar a )
{
   int l = a.l;
   while ((l > 0) && ((a.a[l-1] == ' ') || (a.a[l-1] == '\0'))) l--;
   return(l);
}

/*
#>            tralog.dc2

Function:     TRALOG

Purpose:      Translates logical name to equivalence name.

File:         tralog.c

Author:       K.G. Begeman

Use:          INTEGER TRALOG( LOGNAME,     Input      CHARACTER*(*)
                              EQUNAME )    Output     CHARACTER*(*)

              TRALOG        Returns:
                            >0     success, length of equivalence string
                            -1     logical name does not translate
                            -2     output buffer to small to contain
                                   equivalence name
                            other  unidentified error
              LOGNAME       Logical name to be translated.
              EQUNAME       Equivalence name after successful
                            translation.

Notes:        TRALOG translates on VMS systems first logical names
              in the LNM$JOB table (i.e. those created with
              DEFINE/JOB), and second in the LNM$PROCESS table (i.e.
              those created with DEFINE/PROCESS). On UNIX systems it
              translates the names created with setenv.

Updates:      Jul 18, 1989: KGB, Document created.

#<

@ integer function tralog( character, character )

*/

fint tralog_c( fchar lnam, fchar tnam )
{
   char *lognam;
   char *tranam;
   int  ret;
   int  len = nchar(lnam);
   lognam = calloc(len+1,sizeof(char));      /* Create space for logical name */
   strncpy(lognam,lnam.a,len);                           /* Copy logical name */
   lognam[len] = 0;                     /* Terminating character put in place */
   if ((tranam = getenv(lognam)) == NULL) {
      free( lognam );
      ret = -1;              /* Logical name not present in environment table */
   } else {
      free( lognam );
      len = strlen(tranam);                     /* Length of equivalence name */
      if (len > tnam.l) {
         ret = -2;                               /* Equivalence name too long */
      } else {
         ret = len;                      /* Return length of equivalence name */
         strncpy(tnam.a,tranam,len);              /* Copy it to output string */
      }
   }
   return(ret);                  /* return error or length of equivalence name */
}

#if defined(TESTBED)
main( int argc, char *argv[] )
{
   static fchar  log = { "LOGNAME", 7 };
   fchar  dev;
   char   devbuf[256];
   int    ret;
   int    n = 0;
   dev.a = devbuf; dev.l = 255;
   if ((ret = tralog_c( log, dev)) > 0) {
      devbuf[ret] = 0;
      printf("%s -> %s (status: %d)\n", log.a, dev.a, ret );
   } else {
      printf("No translation of %s (error %d)\n", log.a, ret );
   }
}
#endif
