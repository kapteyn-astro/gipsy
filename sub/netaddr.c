/* netaddr.c
                              COPYRIGHT (c) 1994
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands


Author: J.P. Terlouw  -- derived from code in K. Begeman's GIDS.

#>netaddr.dc3
Function:   netaddr

Purpose:    Obtain the ASCII dot-separated Internet address of the local host.

Category:   SYSTEM

File:       netaddr.c

Author:     J.P. Terlouw

Use:        char *netaddr()
            
Updates:    Apr  7, 1994: JPT, Document created
#<
#>netaddr.h
#if !defined(_netaddr_h_)
#define _netaddr_h_
char *netaddr(void);
#endif
#<
*/
#include "stddef.h"
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include "string.h"
#include "netaddr.h"

#define STRLEN 40

static char outstring[STRLEN];

/* ========================================================================== */
/*                                 netaddr                                    */
/* -------------------------------------------------------------------------- */
/*  netaddr() returns a pointer to the Internet address string of 
 *  the current host.
 */
char *netaddr(void)
{
   char	hostname[STRLEN];			/* for hostname */
   int	gethostname( );				/* obtains hostname */

   if (gethostname( hostname, STRLEN ) == -1) return NULL;
   /*
    * Now get inet address of host.
    */
   {
      char		*addr;
      char		*inet_ntoa( );
      struct hostent	*hp;
      struct hostent	*gethostbyname( );
      struct in_addr	ha;

      hp = gethostbyname( hostname );
      if ((hp == NULL) && (strchr( hostname, '.' ) == NULL)) {
         char	dname[STRLEN];
         int	getdomainname( );

         if (!getdomainname( dname, sizeof( dname ))) {
            strcat( hostname, "." );
            strcat( hostname, dname );
            hp = gethostbyname( hostname );
         }
      }
      if (hp != NULL) {
         memmove( (void *) &ha, (void *) hp->h_addr, hp->h_length );
#if	defined(__sun__) & defined(__GNUC__) & !defined(__i386__)
         addr = inet_ntoa( &ha );
#else
         addr = inet_ntoa( ha );
#endif
         strcpy( outstring, addr );
      } else {
         strcpy( outstring, hostname );
      }
   }
   return outstring;
}
