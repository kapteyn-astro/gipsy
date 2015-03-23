/* irim_seldets.c

	   Copyright (c) 1991
  Laboratory for Space Research Groningen
       Kapteyn Laboratory Groningen
	   All Rights Reserved.

#> irim_seldets.dc2
Function:     irim_seldets

Purpose:      enquire about the use of detectors

Category:     IRAS, private to program: IMAGE

Author:       Do Kester

Use:
void irim_seldets(
	list_type	*exc	I/O
	fint		band )		IN

Description:
	The user is queried with keyword DETS= to select which detectors
	are to be used. The answers can be
	    all 		all detectors are used
	    small		only the small (quarter size) detectors
	    large		the small detectors are NOT used.
  <list of detector numbers>	the list is used
	Detectors which are not in the band are silently kicked out.


Comment:      This routine is not callable in FORTRAN

Updates:      16 Aug 1991: DK, Creation date
	      04 Jun 1992: DK, prepared for GIPSY library
#<
*/

#include "gipsyc.h"
#include "irlist.h"
#include "userfio.h"
#include "stdlib.h"
#include "assert.h"
#include "math.h"
#include "stdio.h"

#define	NL1		100
#define	NL2		75
#define DETS_KEY	"DETS="
#define DETS_MES	"Give detectors to be used [large]"
#define MAXBAND 	5

void irim_seldets(
	list_type	*exclude,
	fint		band )
{
	fint		dets[NL1] ;
	bool		alldet, small, large ;
	float		*pl, *pd, *pe ;
	int		i, nitem ;
	static char	dummy[81] ;
	static fchar	text = { dummy, 80 } ;
	list_type	*list1, *list2 ;

	static float smalldets[8] =
		{ 0.26, 0.47, 0.39, 0.46, 0.11, 0.31, 0.55, 0.62 } ;
	static int	sdnr[MAXBAND+1] = { 0, 2, 2, 2, 2, 0 } ;

	static float bigdets[56] =
		{ 0.23, 0.24, 0.25, 0.27, 0.28, 0.29, 0.30,	/*  12 mu */
		  0.48, 0.49, 0.50, 0.51, 0.52, 0.53, 0.54,
		  0.16, 0.18, 0.19, 0.21, 0.22, 		/*  25 mu */
		  0.40, 0.41, 0.42, 0.43, 0.44, 0.45,
		  0.08, 0.09, 0.10, 0.12, 0.13, 0.14, 0.15,	/*  60 mu */
		  0.32, 0.33, 0.34, 0.35, 0.37, 0.38,
		  0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07,	/* 100 mu */
		  0.56, 0.57, 0.58, 0.59, 0.60, 0.61,
		  0.71, 0.72, 0.73, 0.74, 0.75 } ;		/* LRS */
	static int bdnr[MAXBAND+1] = { 0, 14, 11, 13, 13, 5 } ;

	small = alldet = FALSE ;
	nitem = userftext( text, DFLT_HIDD, DETS_KEY, DETS_MES ) ;
	small = dummy[0] == 's' || dummy[0] == 'S' ;
	alldet= dummy[0] == 'a' || dummy[0] == 'A' ;
	large = dummy[0] == 'l' || dummy[0] == 'L' ;

	pe = exclude->snip_det + exclude->nl ;
	if ( small ) {			/* exclude the large detectors */
	    pd = bigdets ;
	    for ( i = 1 ; i < band ; i++ ) pd += bdnr[i] ;
	    for ( i = 0 ; i < bdnr[band] ; i++ ) *pe++ = *pd++ ;
	    exclude->nl += bdnr[band] ;
	} else if ( alldet ) {		/* do nothing */
	} else if ( nitem && ! large ) {  /* exclude the dets NOT mentioned */
	    nitem = userfint( dets, NL1, DFLT_HIDD, DETS_KEY, DETS_MES ) ;
	    LIST_INIT( list1 ) ;
	    if ( !( list1->snip_det = (float*)calloc( NL1, sizeof( float )))) {
		errorf( SERIOUS, "irim_seldets: out of memory" ) ;
		return ;
	    }
	    pl = list1->snip_det ;
	    for ( i = 0 ; i < nitem ; i++ ) *pl++ = dets[i] / 100.0 ;
	    list1->nl = nitem ;
	    irim_sortuniq( list1 ) ;	/* sort list1 and make unique */
	    LIST_INIT( list2 ) ;
	    if ( !( list2->snip_det = (float*)calloc( NL2, sizeof( float )))) {
		errorf( SERIOUS, "irim_seldets: out of memory" ) ;
		return ;
	    }
	    pl = list2->snip_det ;
	    for ( i = 1 ; i <= NL2 ; i++ ) *pl++ = i / 100.0 ;
	    list2->nl = NL2 ;
	    irim_detinband( list2, band ) ;	/* list2 is all dets in band */
	    irim_disjunct( list1, list2 ) ;	/* list2 = all minus list1 */
	    pl = list2->snip_det ;
	    for ( i = 0 ; i < list2->nl ; i++ ) *pe++ = *pl++ ;
	    exclude->nl += list2->nl ;
	    free( list1->snip_det ) ; free( list1 ) ;
	    free( list2->snip_det ) ; free( list2 ) ;
	} else {			/* standard: exclude small dets */
	    pd = smalldets ;
	    for ( i = 1 ; i < band ; i++ ) pd += sdnr[i] ;
	    for ( i = 0 ; i < sdnr[band] ; i++ ) *pe++ = *pd++ ;
	    exclude->nl += sdnr[band] ;
	}

	return ;
}
#undef	NL1
#undef	NL2
#undef 	DETS_KEY
#undef 	DETS_MES
#undef 	MAXBAND

