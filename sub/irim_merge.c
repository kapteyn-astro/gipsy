/* irim_merge.c

	   Copyright (c) 1991
  Laboratory for Space Research Groningen
       Kapteyn Laboratory Groningen
	   All Rights Reserved.

#> irim_merge.dc2
Function:     irim_merge

Purpose:      merge list1 into list2

Category:     IRAS, private to program: IMAGE

Author:       Do Kester		do@guspace.rug.nl

Use:
void irim_merge(
	list_type	*ls1,
	list_type	*ls2 )

The items in ls1 are merged into ls2. The caller has to
      make sure that there is enough room in ls2 to contain the
      merged result. Items which differ less than 0.001 are considered
      identical.

Comment:      This routine is not callable in FORTRAN

Updates:      07 Aug 1991: DK, Creation date
	      04 Jun 1992: DK, prepared for GIPSY library

Original:     IMERGE
	Structured and translated by ASSISTANT II Plus, v1.2
#<
*/

#include "gipsyc.h"
#include "irlist.h"
#include "userfio.h"
#include "math.h"

void irim_merge(
	list_type	*ls1,
	list_type	*ls2 )
{
  int k1, k2, i, k ;


/* merge list1 with list2  */
  k2 = ls2->nl - 1 ;
  k1 = ls1->nl - 1 ;
  ls2->nl += ls1->nl ;
  k = ls2->nl - 1 ;
  while ( k >= 0 ) {
    if ( fabs( ls1->snip_det[k1]  - ls2->snip_det[k2]  ) < EPSIL ) {
/* identical: shift ls2 one position down  */
      for ( i = k + 1 ; i < ls2->nl ; i++ )
		ls2->snip_det[i - 1] = ls2->snip_det[i] ;
      ( ls2->nl )-- ;
      k2-- ;
    } else if ( ls2->snip_det[k2]  > ls1->snip_det[k1]	) {
      ls2->snip_det[k] = ls2->snip_det[k2] ;
      k2-- ;
    } else {
      ls2->snip_det[k] = ls1->snip_det[k1] ;
      k1-- ;
    }
    k-- ;
  }

}

