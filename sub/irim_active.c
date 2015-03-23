/* irim_active.c

	   Copyright (c) 1991
  Laboratory for Space Research Groningen
       Kapteyn Laboratory Groningen
	   All Rights Reserved.

#>  irim_active.dc2
Function:     irim_active

Purpose:      determine active detectors from a snip_det list

Category:     IRAS, private to program: IMAGE

Author:       Do Kester		do@guspace.rug.nl

Use:
int irim_active(
	int		scannr,
	list_type	*list,
	int		*lnr,
	int		*dlist,
	int		*nd )

returns: whether or not (some detectors of) this snip must be processed.

      LIST is an ordered list of reals,  where the integer part denotes
      a scan number and the fractional part are detector numbers as
      in IRAS-ES p.II-12&13.
      irim_active checks the LIST from position LNR onward for an integral
      part equal SCANNR. The fractional part is added / subtracted to
      DLIST in the right position. If complement is false and snip_det
      has no fractional part then DLIST is reset to all zero's,
      i.e. no detectors remain in DLIST.
      Example:
      LIST = ( 0.16, 0.17, 0.18, 0.19, 0.20, 0.21, 0.22 ) & SCANNR = 0
      if ( complement ): DLIST will be the first row of detectors of band 2
      else	       : DLIST will be the last row of detectors of band 2
      NOTE: dead detectors can not be included in DLIST, as they
	    cannot be in LIST; small detectors, however, can.

Comment:      This routine is not callable in FORTRAN

Updates:      15 Aug 1991: DK, Creation date
	      04 Jun 1992: DK, prepared for GIPSY library

Original:     IMDETS
	Structured and translated by ASSISTANT II Plus, v1.2
#<
*/

#include "gipsyc.h"
#include "irlist.h"
#include "userfio.h"
#include "math.h"

int irim_active(
	int		scannr,
	list_type	*list,
	int		*lnr,
	int		*dlist,
	int		*nd )
{
	int		snip_in_list, det, i, j, snr, doscan ;

	if ( ! list->nl ) return( list->complement ) ;

/*  skip list numbers smaller than the scan number  */
	while ( *lnr < list->nl && list->snip_det[*lnr] < scannr ) {
	    (*lnr)++ ;					/* next in list */
	}
	snr = (int)list->snip_det[*lnr] ;
	snip_in_list = *lnr < list->nl && snr == scannr ;
	if ( snip_in_list && fabs( scannr - list->snip_det[*lnr]  ) > EPSIL &&
		! list->complement ) {
/*  no detectors specified: delete all detectors from dlist  */
	    for ( j = 0 ; j < *nd ; j++ ) dlist[j] = 0 ;
	    *nd = 0 ;
	}

	doscan = ( snip_in_list != list->complement ) ;
	while ( *lnr < list->nl && snr == scannr ) {
	    det = (int)( 100 * ( list->snip_det[*lnr] - snr ) + 0.5 ) ;
/*  search position of detector  */
	    if ( det > 0 ) {
	      doscan = TRUE ;
	      j	= 0 ;
	      while ( j < *nd && dlist[j]  < det ) j++ ;
	      if ( dlist[j]  != det ) {
/*  add the detector to the list  */
		if ( ! list->complement ) {
		  for ( i = *nd - 1 ; i >= j ; i-- ) dlist[i+1] = dlist[i] ;
		  dlist[j] = det ;
		  (*nd)++ ;
		}
/*  delete the detector from dlist  */
	      } else if ( list->complement ) {
		for ( i = j + 1 ; i < *nd ; i++ ) dlist[i-1] = dlist[i] ;
		(*nd)-- ;
		dlist[*nd] = 0 ;	/* set last one to 0 */
	      }
	    }
/* next in list */
	    (*lnr)++ ;
	    snr = list->snip_det[*lnr] ;
	}
	return( doscan ) ;
}

