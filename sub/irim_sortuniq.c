/* irim_sortuniq.c

	   Copyright (c) 1991
  Laboratory for Space Research Groningen
       Kapteyn Laboratory Groningen
	   All Rights Reserved.

#> irim_sortuniq.dc2
Function:     irim_sortuniq

Purpose:      sort and unique list

Category:     IRAS, private to program: IMAGE

Author:       Do Kester		do@guspace.rug.nl

Use:
void irim_sortuniq(
	list_type	*ls )

The items in LIST are sorted ;	Doublets are silently kicked out.
      Two items are deemed the same if they differ by less than
      epsil (  = 0.001 ).

Comment:      This routine is not callable in FORTRAN

Updates:      07 Aug 1991: DK, Creation date
	      04 Jun 1992: DK, prepared for GIPSY library

Original:     IMSORT
	Structured and translated by ASSISTANT II Plus, v1.2
#<
*/

#include "gipsyc.h"
#include "irlist.h"
#include "userfio.h"
#include "math.h"

void irim_sortuniq(
	list_type	*ls )
{
  float		tmp ;
  int		i, j, k ;

/*  sort list and kick doublets out */

  k = 0 ;
  while ( k < ls->nl ) {
    i = k++ ;
/*  move an item downward as long as it is less than the previous one  */
    while ( i ) {
      if ( ls->snip_det[i-1] > ls->snip_det[i]	) {
	tmp = ls->snip_det[i-1] ;
	ls->snip_det[i-1] = ls->snip_det[i] ;
	ls->snip_det[i] = tmp ;
	i-- ;
      } else {
	if ( fabs( ls->snip_det[i] - ls->snip_det[i-1] ) < EPSIL ) {
/*  equal items are overwritten by the next ;  all others are shifted  */
	  for ( j = i ; j < ls->nl ; j++ )
		ls->snip_det[j-1] = ls->snip_det[j] ;
	  ( ls->nl )-- ;
	}
	break ;
      }
    }
  }
}

