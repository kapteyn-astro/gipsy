/* irim_cutlist.c

	   Copyright (c) 1991
  Laboratory for Space Research Groningen
       Kapteyn Laboratory Groningen
	   All Rights Reserved.

#> irim_cutlist.dc2
Function:     irim_cutlist

Purpose:      cuts a list to the requested length

Category:     IRAS, private to program: IMAGE

Author:       Do Kester

Use:
void irim_cutlist(
	list_type	*ls,
	int		length )

returns: -1	out of memory
	 other	length

Comment:      This routine is not callable in FORTRAN

Updates:      06 Aug 1991: DK, Creation date
	      04 Jun 1992: DK, prepared for GIPSY library
	      22 Aug 1995: DK, realloc returns a void* which has to be filled
#<
*/

#include "gipsyc.h"
#include "irlist.h"
#include "userfio.h"
#include "stdlib.h"
#include "stddef.h"

void irim_cutlist(				/* cut list to size length */
	list_type	*ls,
	int		length )
{
  if ( length ) {
    if ( ! ( ls->snip_det = (float*)realloc( (float*)ls->snip_det, 
    			     length * sizeof( float ) ) ) ) {
      errorf( SERIOUS, "irim_cutlist: out of memory" ) ;
    }
  } else {					/* list is empty */
    free( ls->snip_det ) ;			/* free memory */
    ls->snip_det = NULL ;			/* nullify list pointer */
    ls->nl = 0 ;				/* number of item = zero */
  }
}

