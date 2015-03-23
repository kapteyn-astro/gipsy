/* irim_wrlist.c

	   Copyright (c) 1991
  Laboratory for Space Research Groningen
       Kapteyn Laboratory Groningen
	   All Rights Reserved.

#> irim_wrlist.dc2
Function:     irim_wrlist

Purpose:      write scan.detector lists to logfile

Category:     IRAS, private to program: IMAGE

Author:       Do Kester		do@guspace.rug.nl

Use:
void irim_wrlist(
	int		meslev,
	ch		*mess,
	list_type	*ls )

The message is written first,  whereafter all the items in
  list are written to the logfile. mess is overwritten.

Comment:      This routine is not callable in FORTRAN

Updates:      07 Aug 1991: DK, Creation date
	      04 Jun 1992: DK, prepared for GIPSY library

Original:     IMWRIT
	Structured and translated by ASSISTANT II Plus, v1.2
#<
*/

#include "gipsyc.h"
#include "irlist.h"
#include "userfio.h"
#include "math.h"
#include "stdio.h"

void irim_wrlist(
	int		meslev,
	char		*mess,
	list_type	*ls )
{
  int		k, km1=1, km2=1, snip = -1, det = -1, prsnip, prdet ;
  char		line1[41], line2[41] ;

    if ( ! ls->nl ) return ;

    anyoutf( meslev, mess ) ;
    sprintf( line1, "	   snips " ) ;
    sprintf( line2, "|	    dets " ) ;
    for ( k = 0 ; k < ls->nl ; k++ ) {
      prsnip = snip ; prdet = det ;
      snip = floor( ls->snip_det[k] ) ;
      det = floor( 100 * ( ls->snip_det[k] - snip ) + 0.1 ) ;
      if ( ( snip != prsnip && det != prdet ) || km1 > 35 || km2 > 35 ) {
	anyoutf( meslev, "%-40s%s", line1, line2 ) ;
	sprintf( line1, " " ) ;
	sprintf( line2, "|" ) ;
	km1 = km2 = 1 ;
      }
      if ( snip != prsnip ) {
	if ( snip ) sprintf( &line1[km1], "%4d", snip ) ;
	else	    sprintf( &line1[km1], " all" ) ;
	km1 += 4 ;
      }
      if ( det != prdet ) {
	if ( det )  sprintf( &line2[km2], "%4d", det ) ;
	else	    sprintf( &line2[km2], " all" ) ;
	km2 += 4 ;
      }
    }
    if ( km1 > 2 || km2 > 2 )
	anyoutf( meslev, "%-40s%s", line1, line2 ) ;
}

