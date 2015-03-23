/* irco_praxname.c

           Copyright (c) 1991
  Laboratory for Space Research Groningen 
       Kapteyn Laboratory Groningen 
           All Rights Reserved.

#> irco_praxname.dc2

Function:     irco_praxname

Purpose:      provide names for the axes in all coor/proj systems

Category:     IRAS, private to program: IMAGE

Author:       Do Kester		do@guspace.rug.nl

Use:  
void irco_praxname_c( 
	fint	*coor, 		I	coordinate system number
	fint	*prid, 		I	projection type number
	fchar 	lname,		O	name of the 'longitude' axis
	fchar 	bname )		O	name of the 'latitude' axis

USE   For the combination of coordinate system and projection type
      identification names are returned which are valid in FITS.
      The names which are returned,  represent the coordinate axis
      (l/bname: 4 char) padded with a projection type (pname: 4 char).
         coor.name         lname    bname   prid      pname
         EQUatorial        RA--     DEC-     1       -STG
         ECLiptic          ELON     ELAT     2       -TAN
         GALactic          GLON     GLAT     3       -AZD
         SUPergalactic     SLON     SLAT     4       -AZA
         SUNreferenced     SUNL     SUNB     5       -SIN
         unknown           L---     B---     6       -CYL
                                             7       -MER
                                             8       -GLS
                                             9       -AIT
                                            10       -FLT
					     0       ----

      See irco_name for a full description of the coordinate systems
      and irco_prname for the projection types.

Updates:      05 Sep 1991: DK, Creation date

Original:     KOPRAX
	Structured and translated by ASSISTANT II Plus, v1.2
#<
@subroutine irco_praxname( integer, integer, character, character )
*/

#include "gipsyc.h"
#include "string.h"
#include "stdio.h"
#include "ctype.h"
#include "irco_namepoch.h"

void irco_praxname_c( 
	fint	*coor, 
	fint	*prid, 
	fchar 	lname,
	fchar 	bname )
{
  static char *lnames[] = {"RA--", "GLON", "ELON", "SLON", "SUNL", "L---"} ; 
  static char *bnames[] = {"DEC-", "GLAT", "ELAT", "SLAT", "SUNB", "B---"} ; 
  static char *pnames[] = {"----", "-STG", "-TAN", "-AZD", "-AZA", 
			   "-SIN", "-CYL", "-MER", "-GLS", "-AIT", "-FLT"} ; 
    fchar       coname ;
    char        cotmp[41], co3[4] ;
    float       epoch ;
    int		cindex ;

    coname.a = cotmp ; coname.l = 40 ;
    irco_namepoch_c( coor, coname, &epoch ) ;
/* check first three letters only */
    co3[0] = toupper( cotmp[0] ) ;    
    co3[1] = toupper( cotmp[1] ) ;    
    co3[2] = toupper( cotmp[2] ) ;    
    co3[3] = '\0' ;
    if ( strcmp( co3, "EQU" ) == 0 )      cindex = 0 ;
    else if ( strcmp( co3, "GAL" ) == 0 ) cindex = 1 ;
    else if ( strcmp( co3, "ECL" ) == 0 ) cindex = 2 ;
    else if ( strcmp( co3, "SUP" ) == 0 ) cindex = 3 ;
    else if ( strcmp( co3, "SUN" ) == 0 ) cindex = 4 ;
    else                                  cindex = 5 ;

    sprintf( lname.a, "%s%s", lnames[cindex], pnames[*prid] ) ; 
    lname.l = 8 ;
    sprintf( bname.a, "%s%s", bnames[cindex], pnames[*prid] ) ; 
    bname.l = 8 ;

}
