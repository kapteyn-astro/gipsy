/* irim_disjunct.c

	   Copyright (c) 1991
  Laboratory for Space Research Groningen
       Kapteyn Laboratory Groningen
	   All Rights Reserved.

#> irim_disjunct.dc2
Function:     irim_disjunct

Purpose:      make two lists disjunct

Category:     IRAS, private to program: IMAGE

Author:       Do Kester		do@guspace.rug.nl

Use:
void irim_disjunct(
	list_type	*list1,
	list_type	*list2 )

The lists should be ordered before entering this routine.
They are made disjunct by kicking doublets out of list2.
Items which differ less than epsil = 0.001 are regarded
as identical. List1 remains unchanged.


Comment:      This routine is not callable in FORTRAN

Updates:      08 Aug 1991: DK, Creation date
	      04 Jun 1992: DK, prepared for GIPSY library

Original:     IMDISJ
	Structured and translated by ASSISTANT II Plus, v1.2
#<
*/

#include "gipsyc.h"
#include "irlist.h"
#include "math.h"
#include "userfio.h"

void irim_disjunct(
	list_type	*list1,
	list_type	*list2 )
{
    int 		k1, k2, i ;

/*  make the lists 1 & 2 disjunct by kicking doublets out of list 2 */

    k1 = k2 = 0 ;
    while ( k1 < list1->nl && k2 < list2->nl ) {
	if ( fabs( list1->snip_det[k1] - list2->snip_det[k2] ) < EPSIL ) {
	    for ( i = k2 + 1 ; i < list2->nl ; i++ )
		list2->snip_det[i-1] = list2->snip_det[i] ;
	    ( list2->nl )-- ;
	} else if ( list1->snip_det[k1] < list2->snip_det[k2] ) {
	    k1++ ;
	} else {
	    k2++ ;
	}
    }
}

