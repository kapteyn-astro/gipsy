/*============================================================================
                                  sortc
------------------------------------------------------------------------------

#> sortc.dc2
Subroutine:    sortc

Purpose:       sort an array with character strings

File:          sortc.c

Author:        W. Zwitser

Use:           SORTC( STR,                    In/Out        character
                      NUM,                    In/Out        integer 
                      NEL )                   Input         integer

               STR    array with strings which is sorted in an alphabetic
                      order.

               NUM    array with numbers which correspond with a STR element.
                      Before the call to SORTC they are initialised from
                      1...NEL and after the call they give the original
                      position of each element in the STR array.

               NEL    number of elements in STR.

Description:   SORTC is a so called shellsort and was copied from the second
               edition of the C manual of Harbinson & Steele, page 211.

Updates:       Apr 27, 1990: WZ, installed

@ subroutine sortc( character, 
@                   integer, 
@                   integer )

#<
----------------------------------------------------------------------------*/
#include "stdlib.h"
#include "string.h"
#include "gipsyc.h"
#define  lc    21

static char *extr( char *cc, int lcc, char *ss, int iss )
{
	cc[lcc] = '\0';
	return( strncpy( cc, &ss[iss*lcc], lcc ) );
}

void sortc_c( fchar  str,   /* array with strings to be sorted              */
              fint  *num,   /* array with numbers which correspond with STR */
              fint  *nel )  /* number of elements in STR                    */
{
    char c[lc], ctemp[lc], *chr;
    int  gap, i, j, lchr;
    fint ntemp;

    chr  = str.a;
    lchr = str.l;
    gap = 1;
    do ( gap = 3 * gap + 1 ); while ( gap <= *nel );
    for ( gap /= 3; gap > 0; gap /= 3 )
        for ( i = gap; i < *nel; i++ )
        {
            ntemp = num[i];
            strncpy( ctemp, &chr[i*lchr], lchr );
            for ( j = i - gap; 
                   ( j >= 0 && 
                   strncmp( extr( c, lchr, chr, j ), ctemp, lchr ) > 0 );
                j -= gap )
            {
                strncpy( &chr[(j+gap)*lchr], c, lchr );
                num[j+gap] = num[j];
            }
            strncpy( &chr[(j+gap)*lchr], ctemp, lchr );
            num[j+gap] = ntemp;
        }
}	
