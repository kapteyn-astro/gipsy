/* diskinfo.c
                           COPYRIGHT (c) 2000
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved

#>             diskinfo.h
#if !defined(_diskinfo_h_)
#define _diskinfo_h_
char *diskinfo( char *, int, int, int );
#endif
#<
*/


/*

#>             diskinfo.dc2

Function:      diskinfo

Purpose:       Get a formatted string with axis names and sizes for a
               set on disk.

Category:      FILES, UTILITY

File:          diskinfo.c

Author:        M. Vogelaar

Use:           char *diskinfo( char *setname,     IN: Name of set
                               int   w1,          IN: Max length of name field
                               int   w2,          IN: Max length of axis info
                               int   w3 )         IN: Max length of size field


Example:       anyoutf( 1, diskinfo( "AURORA", 20, 35, 12) );

               results in:

               AURORA              RA:64 DEC:64 PARAM-SUM:1            12+16 kb

               First size is the descriptor size. The second number is the
               size of the image.

Comment:       This routine is NOT callable from FORTRAN.

Notes:         The total string length of the output cannot exceed 1024
               characters. The return value of this routine is a pointer
               to a static character array defined in this routine.

Updates:       20 Jul,  2000: VOG, Document created.
               24 Mar,  2011: JPT, Support for large files.
#<

*/

/* Include files */

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "ctype.h"
#include "gipsyc.h"
#include "gdsc_ndims.h"
#include "nelc.h"
#include "fsize.h"
#include "gdsc_size.h"
#include "gds_exist.h"
#include "gds_close.h"
#include "gds_errstr.h"
#include "getaxname.h"
#include "userfio.h"
#include "gds_handle.h"
#include "diskinfo.h"


static int tokilobyte( fint8 bytes )
/*------------------------------------------------------------*/
/* PURPOSE: Convert bytes kb.                                 */
/*------------------------------------------------------------*/
{
   return( (bytes+1023) / 1024 );
}


static void getranges( fchar Sethandle,
                       char  *axisstr )
/*------------------------------------------------------------*/
/* PURPOSE: Axis information (name and size).                 */
/*------------------------------------------------------------*/
{
   int    m, setdim;
   fint   setlevel = 0;


   setdim  = (int) gdsc_ndims_c( Sethandle, &setlevel );
   strcpy( axisstr, "" );
   for (m = 0; m < setdim; m++)
   {
      fint   axnum = m + 1;
      fint   chop = 1;          /* Allow to chop projection info */
      fchar  Axname;
      char   axname[20+1];
      char   buf[80];
      fint   r = 0;
      fint   sizeofax = 0;

      Axname.a = axname; Axname.l = 20;
      getaxname_c( Sethandle, &axnum, &chop, Axname );
      axname[nelc_c(Axname)] = '\0';
      sizeofax= gdsc_size_c( Sethandle, &axnum, &r );
      sprintf( buf, "%s:%d", axname, sizeofax );
      strcat( axisstr, buf );
      if (m < setdim-1)
         strcat( axisstr, " " );
   }
}



char *diskinfo( char *setname,
                int  l1, int l2, int l3 )
/*------------------------------------------------------------*/
/* PURPOSE: Generate info line for this set.                  */
/*------------------------------------------------------------*/
{
   static char infostr[1024];

   fint8  descrsize;
   fint8  imagesize;
   fint   r;
   bool   exist;
   char   sizestr[80];
   char   axisstr[512];
   char   newname[FILENAME_MAX];
   fchar  Sethandle;
   char   sethandle[21];                           /* Max length for a handle */


   sprintf( newname, "%s.descr", setname );
   descrsize = fsize_c( tofchar(newname) );
   sprintf( newname, "%s.image", setname );
   imagesize = fsize_c( tofchar(newname) );
   if (descrsize < 0)
      descrsize = 0;
   if (imagesize < 0)
      imagesize = 0;

   sprintf( sizestr, "%d+%d kb", tokilobyte(descrsize), tokilobyte(imagesize) );

   Sethandle.a = sethandle;
   Sethandle.l = 20;
   sethandle[nelc_c(Sethandle)] = '\0';

   r = 0;
   exist = tobool( gds_exist_c( tofchar(setname), &r) );

   /* Does not exist or the descriptor is corrupt */
   if (!exist)
   {
      fchar  Errstr;
      char   errstr[256];
      Errstr.a = errstr; Errstr.l = 256-1;
      gds_errstr_c( Errstr, &r );
      errstr[nelc_c(Errstr)] = '\0';

      if (r == -56)
      {
         strcpy( errstr, "Disk space problem in home dir.?" );
      }
      sprintf( infostr, "%-*.*s %-*.*s %-*.*s",
               l1, l1, setname,
               l2, l2, errstr,
               l3, l3, sizestr );
      r = 0;
      gds_close_c( tofchar(setname), &r );          /* Close the file */
      return( infostr );
   }
   r = 0;
   gds_handle_c( Sethandle, tofchar(setname), &r );
   getranges( Sethandle, axisstr );
   /* Set exists and descriptor is not corrupt */
   sprintf( infostr, "%-*.*s %-*.*s %-*.*s",
            l1, l1, setname,
            l2, l2, axisstr,
            l3, l3, sizestr );

   r = 0;
   gds_close_c( Sethandle, &r );             /* Close the file */
   return( infostr );
}


#if defined(TESTBED)
#include "cmain.h"
#include "init.h"
#include "finis.h"
#include "userchar.h"


#define  REQUEST          1

MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{

   fchar   keyword, message;               /* For userxxx functions */
   fchar   Setname;
   fint    res;                            /* Fortran int. res. of funct. call */
   fint    dfault;                         /* Default option for input etc */
   fint    numitems;                       /* Max. num. to enter in userxxx */
   char    setbuf[FILENAME_MAX+1];


   init_c();                               /* Contact Hermes */

   do {
   Setname.a = setbuf; Setname.l = FILENAME_MAX;
   keyword = tofchar("SETNAME=");
   message = tofchar("Enter name of set:" );
   numitems  = 1;
   dfault    = REQUEST;
   res = userchar_c( Setname, &numitems, &dfault, keyword, message );
   cancel_c( keyword );
   setbuf[nelc_c(Setname)] = '\0';
   anyoutf( 1, diskinfo( Setname.a, 20, 35, 17 ) );
   } while(1);

   finis_c();                                                  /* Quit Hermes */
   return(EXIT_SUCCESS);
}
#endif
