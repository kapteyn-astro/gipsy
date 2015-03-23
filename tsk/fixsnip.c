/* fixsnip.c
                           COPYRIGHT (c) 1992
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>            fixsnip.dc1

Program:      FIXSNIP

Purpose:      Adds the keywords OBS and SCANTYPE to the header
              if these are not present. This is neccessairy since
              these keywords have lately been added to the 
              standard keyword-list at snip level and IRDS_EXTEND
              and IRDS_ENQUIRE_SNIP have been adapted to this.
              
Category:     IRAS

File:         fixsnip.c             

Author:       Fred Lahuis

Keywords:

    IRSET=    Name of the IRDS                              [quit]
              A maximum number of 50 IRDS can be entered at
              once and IRSET is repeated until return is entered.

Description:  FIXSNIP adds the keywords OBS and SCANTYPE at sniplevel
              if these are not present. For OBS it adds the value 0
              and for SCANTYPE it adds an empty string.

Updates:      Mar 20, 1992: FL, Document created.
              Dec  1, 1992: VOG, Document updated.

#<
*/
#include "gipsyc.h"
#include "cmain.h"
#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "srvreq.h"
#include "gdsd_rint.h"
#include "gdsd_wint.h"
#include "gdsd_rchar.h"
#include "gdsd_wchar.h"
#include "irds_exist.h"
#include "gdsc_word.h"
#include "nelc.h"

#define  PROGRAM	"FIXSNIP"
#define  VERSION	"1.0"
#define  IRKEY		tofchar("IRSET=")
#define  IRMESS		tofchar("Give IRDS (max. 50) [quit]")
#define  OBSKEY		tofchar("OBS")
#define  SCANKEY	tofchar("SCANTYPE") 
#define  EMPTYSTRING	tofchar(" ")
#define  MAXTXTLEN	150
#define  MAXIRDS	50
#define  fmake(fchr,size)       { \
                                        static char buff[size+1] ; \
                                        int i ; \
                                        for( i = 0 ; i < size ; buff[i++] = ' ') ; \
                                        buff[i] = 0 ; \
                                        fchr.a = buff ; \
                                        fchr.l = size ; \
                                }


void get_inset( fchar irds, int *no_irds )
{
fint	maxirds = MAXIRDS ;
fint	one = 1 ;

	*no_irds = userchar_c( irds, &maxirds, &one, IRKEY, IRMESS ) ;
	if( *no_irds == 0 ){
		cancel_c( IRKEY ) ;
		finis_c() ;
	}
	return ;
}
void fixsnip( fchar irds, int no_irds )
{
fint	nul = 0, four = 4 ;
fint	n, no, no_snip, obs, top = 0, cw, status ;
fchar	scantype, Irds ;
char	Cstring[MAXTXTLEN] ;

fmake( scantype, 20 ) ;

Irds.a = irds.a ;
Irds.l = irds.l ;
for( no = 0 ; no < no_irds ; no++ ){
	if( !(irds_exist_c( Irds, &status )) ){
		status = 0 ;
		gdsd_rint_c( Irds, tofchar("NAXIS4"), &top, &no_snip, &status ) ;
		for( n = 1 ; n <= no_snip ; n++ ){
			sprintf( Cstring, "Fixing set %.*s, snip %d" , 
				nelc_c(Irds), Irds.a, n ) ;
			status_c( tofchar(Cstring) ) ;
			cw = status = 0 ;
			cw = gdsc_word_c( Irds, &four, &n, &cw, &status ) ;
			if( !status ){
				gdsd_rint_c( Irds, OBSKEY, &cw, &obs, &status ) ;
				if( status != cw ){
					status = 0 ;
					gdsd_wint_c( Irds, OBSKEY, &cw, &nul, &status ) ;
				} 
				gdsd_rchar_c( Irds, SCANKEY, &cw, scantype, &status ) ;
				if( status != cw ){
					status = 0 ;
					gdsd_wchar_c( Irds, SCANKEY, &cw, EMPTYSTRING, &status ) ;
				} 
			}
		}
	Irds.a += Irds.l ;
	}
}
return ;
}
MAIN_PROGRAM_ENTRY
{
fchar	irds ;
int	no_irds ;

	init_c() ;
	IDENTIFICATION( PROGRAM, VERSION ) ;
	
	fmake( irds, MAXIRDS*MAXTXTLEN ) ;
	irds.l = MAXTXTLEN ;
	do{
		get_inset( irds, &no_irds ) ;
		fixsnip( irds, no_irds ) ;
		cancel_c( IRKEY ) ;
	}while(1) ;
	finis_c() ;
	return(0) ;
}

	

