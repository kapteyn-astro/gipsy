/* hostname.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            hostname.dc3

Function:     HOSTNAME

Purpose:      Returns the current name of the host computer.

Category:     SYSTEM ROUTINES

Files:        hostname.c

Author:       K.G. Begeman

Use:          INTEGER HOSTNAME( NAME )    Output    CHARACTER*(*)

              HOSTNAME        Returns -1 on error, otherwise 0.
              NAME            Name of current host.

Warning:      System dependent!

Updates:      Aug 17, 1990: KGB, document created.

#<

Fortran to C interface:

@ integer function hostname( character )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */

#if	defined(__vms__)		/* VMS machines */

#include	<ssdef.h>
#include	<syidef.h>

#define	VMSHOSTNAMELEN	15		/* max length for VMS */

extern	long	sys$getsyiw( );

#elif	defined(__bsd__)		/* bsd systems */

extern	int	gethostname( char *name, int namelen );

#elif	defined(__sysv__)		/* sysv systems */

#include	<sys/utsname.h>

#endif

fint	hostname_c( fchar name )
{
#if	defined(__vms__)			/* VMS */
   char  hnam[VMSHOSTNAMELEN];			/* buffer for hostname */
   long  status;				/* status return */
   short len;					/* length */
   struct {					/* Item list for sys$getsyiw */
      short l;					/* length */
      short c;					/* command */
      int  *a;					/* address for output */
      int  *r;					/* address for output length */
      int   e;					/* stop word */
   } itmlst;
   itmlst.a = hnam;				/* hostname goes here */
   itmlst.c = SYI$_NODENAME;			/* this is the command */
   itmlst.r = &len;				/* output size goes here */
   itmlst.l = VMSHOSTNAMELEN;			/* input size */
   itmlst.e = 0;				/* stop word */
   status = sys$getsyiw( NULL, NULL, NULL, &itmlst, NULL, NULL, NULL );
   if (status != SS$_NORMAL) {
      return( -1 );				/* error code */
   } else {
      int i;					/* counter */
      
      for (i = 0; i < len && i < name.l; i++) name.a[i] = hnam[i];
      while (i < name.l) name.a[i++] = ' ';	/* fill with blanks */
      return( 0 );
   }
#elif	defined(__bsd__)			/* BSD systems */
   int i, r;

   r =  gethostname( name.a,  (int) name.l );
   if (!r) {
      for (i = 0; i < name.l && name.a[i]; i++);
      for (     ; i < name.l; name.a[i++] = ' ');
   }
   return( r );
#elif	defined(__sysv__)			/* SYSV systems */
   int i, r = -1;
   struct utsname n;

   if (uname( &n ) != -1) {
      r = 0;
      for (i = 0; i < name.l && n.nodename[i]; i++) {
         name.a[i] = n.nodename[i];
      }
      for (     ; i < name.l; name.a[i++] = ' ');
   }
   return( r );
#endif
}

#if	defined( TESTBED )
int main( )
{
   char  nameb[20];
   fchar name;
   fint  r;

   name.a = nameb; name.l = sizeof( nameb );
   r = hostname_c( name );
   if (!r) {
      printf( "hostname = %.*s\n", (int) name.l, name.a );
   } else {
      printf( "error obtaining hostname\n" );
   }
   return( r );
}
#endif
