/* irim_userlist.c

	   Copyright (c) 1991
  Laboratory for Space Research Groningen
       Kapteyn Laboratory Groningen
	   All Rights Reserved.

#> irim_userlist.dc2
Function:     irim_userlist

Purpose:      allocate memory to list and read snip.det list from the user

Category:     IRAS, private to program: IMAGE

Author:       Do Kester

Use:
int irim_userlist(
	list_type	*ls,
	int		deflev,
	char		*key,
	char		*mess )

returns: -1	memory allocation error
	 other	number of items returned

Comment:      This routine is not callable in FORTRAN

Updates:      06 Aug 1991: DK, Creation date
	      04 Jun 1992: DK, prepared for GIPSY library
#<
*/

#include "gipsyc.h"
#include "irlist.h"
#include "userfio.h"
#include "stdlib.h"

#define	MAXLIST		500	/* maximum number of items in list */

int irim_userlist(		/* allocate memory to list and read userinfo */
	list_type	*ls,
	int		deflev,
	char		*key,
	char		*mess )
{

  free( ls->snip_det ) ;
  if ( !( ls->snip_det = (float*)malloc( MAXLIST*sizeof(float) ) ) ) {
/* allocate memory */
    errorf( SERIOUS, "irim_userlist: out of memory" ) ;
    return( -1 ) ;
  }
/* query the user */
  return( ls->nl = userfreal( ls->snip_det, MAXLIST, deflev, key, mess ) ) ;
}


