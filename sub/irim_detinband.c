/* irim_detinband.c

	   Copyright (c) 1991
  Laboratory for Space Research Groningen
       Kapteyn Laboratory Groningen
	   All Rights Reserved.

#> irim_detinband.dc2
Function:     irim_detinband

Purpose:      check whether detectors are in band

Category:     IRAS, private to program: IMAGE

Author:       Do Kester		do@guspace.rug.nl

Use:
void irim_detinband(
	list_type	*ls,
			band )

  The detector extensions in LIST are checked against the range
      of detectors in BAND.
      Detectors that do not belong to BAND or dead detectors are
      kicked out of the list.


Comment:      This routine is not callable in FORTRAN

Updates:      07 Aug 1991: DK, Creation date
	      04 Jun 1992: DK, prepared for GIPSY library

Original:     IMBAND
	Structured and translated by ASSISTANT II Plus, v1.2
#<
*/

#include "gipsyc.h"
#include "irlist.h"
#include "userfio.h"
#include "math.h"

#define DEAD1	17
#define DEAD2	20
#define DEAD3	36

void irim_detinband(
	list_type	*ls,
	int		band )
{
  int det, j, k ;

/*   kick out detectors which are not in the band
		       dum  1	2   3	4  lrs	 */
  static int lolo[] = { 0, 23, 16,  8,	1,  0 } ;
  static int hilo[] = { 0, 30, 22, 15,	7,  0 } ;
  static int lohi[] = { 0, 47, 39, 31, 55, 71 } ;
  static int hihi[] = { 0, 54, 46, 38, 62, 75 } ;

    k = 0 ;
    while ( k < ls->nl ) {
	det = (int)( ls->snip_det[k] ) ;
	det = (int)( 100 * ( ls->snip_det[k] - det ) + 0.5 ) ;
	if ( !( ( lolo[band]  <= det && det <= hilo[band] ) ||
		( lohi[band]  <= det && det <= hihi[band] ) || det == 0 ) ||
		det == DEAD1 || det == DEAD2 || det == DEAD3 ) {
	    for ( j = k + 1 ; j < ls->nl ; j++ )
		ls->snip_det[j-1] = ls->snip_det[j] ;
	    (ls->nl)-- ;			/* one less in list */
	} else {
	    k++ ;
	}
    }
}

