/*
                           COPYRIGHT (c) 1991
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>            merge.dc1

Program:      MERGE

Purpose:      Merges a number if IRDS's.

Category:     IRAS

File:         merge.c

Author:       Fred Lahuis

Keywords:

    MERGESET=  Set to write the result to.                  [mergeset]

    IRSET=     IRDS input sets. A maximum number of             [stop]
               fifty sets can be entered at once.
               It will be repeated until no sets are entered.

    OVERWRITE= Asked if the set to write to                      N/[Y]
               already exists. If yes the set will be
               rewritten, else the input sets will be 
               appended to it.
               
Description:   MERGE will merge the given input sets with 
               MERGESET in a loop using irds_merge.
               For details on the last see $gip_sub/irds_merge.dc2.
               
Comments:      The IRDS's to be merged should pertain to the same 
               plate i.e. all the output parameters obtained with
               irds_enquire should be the same.

Updates:       Feb 11 1992: FL, document created.

#<

*/

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"
#include "assert.h"
#include "nelc.h"
#include "anyout.h"
#include "status.h"
#include "userlog.h"
#include "usertext.h"
#include "userchar.h"
#include "cancel.h"   
#include "error.h"
#include "gds_delete.h"
#include "gdsd_rchar.h"
#include "gdsd_wchar.h"
#include "gds_close.h"
#include "gds_exist.h"
#include "irds_basic.h"
#include "irds_merge.h"

#define  VERSION     "1.0"
#define  PROGRAM     "MERGE"

#define  IRKEY       tofchar("IRSET=")
#define  IRMESS      tofchar("IRDS to be copied to outset [stop]")
#define  OUTKEY      tofchar("MERGESET=")
#define  OUTMESS     tofchar("Output IRDS. [mergeset]")
#define  OVERKEY     tofchar("OVERWRITE=")
#define  OVERMESS    tofchar("Overwrite output IRDS. [y]")
#define  MOREKEY     tofchar("MORE=")
#define  MOREMESS    tofchar("You want to copy more IRDS's Y/[N]")
#define  MAXIRDS     50
	static fint	maxirds = MAXIRDS ;
#define  MAXTXTLEN   80
	static fint	testmode = 16 ;
#define  TEST        (&testmode)		/* test output for anyout */
#define  max(a,b)    ( (a)>(b)?(a):(b) )
#define  fmake(fchr,size)       { \
			static char buff[size+1] ; \
			int i ; \
			for( i = 0 ; i < size ; buff[i++] = ' ') ; \
			buff[i] = 0 ; \
			fchr.a = buff ; \
			fchr.l = size ; \
		}
	static fint	rquest = 1 ; 
#define  RQUEST     (&rquest)
                        
static fint	device = 11 ;
static fint	nul = 0, one = 1 ;
static char	Cstring[250] ;


void get_outset( fchar outirds )
{
int	no ;
fint	overwrite ;
fint	status, error ;

do{
	no = usertext_c( outirds, RQUEST, OUTKEY, OUTMESS ) ;
	if( no == 0 ) strncpy( outirds.a, "mergeset", 8 ) ;
	status = error = 0 ;
	if( gds_exist_c( outirds, &error ) ){
		overwrite = 1 ;
		no = userlog_c( &overwrite, &one, RQUEST, OVERKEY, OVERMESS ) ;
/*
		cancel_c( OVERKEY ) ;
*/
		if( overwrite == 1 ){
			status = 0 ;
			gds_close_c( outirds, &status ) ;
			status = 0 ;
			gds_delete_c( outirds, &status ) ;
		}
	}
	break ;
}while( 1 ) ;
return ;
}

void type_irds( fchar irds, int *no_irds )
{
int	no ;
fchar	Irdsname ;

	Irdsname.a = irds.a ;
	Irdsname.l = MAXTXTLEN ;
	for( no = 0 ; no < *no_irds ; no++ ){
		anyout_c( TEST, Irdsname ) ;
		Irdsname.a += Irdsname.l ;
	}
	return ;
}	
void get_insets( fchar irds, int *no_irds )
{
	*no_irds = userchar_c( irds, &maxirds, RQUEST, IRKEY, IRMESS ) ;
	type_irds( irds, no_irds ) ;
	if( *no_irds == 0 ) finis_c() ;
	return ;
}

void create_outset( fchar irds, int *no_irds, fchar outirds )
{
int	no ;
fint	status, level ;
double	centre[2], size[2] ;
fint	naxis, axes[4] ;
float	epoch ;
fchar	object, instrument, coor, observer, units ;
fchar	Irdsname ;

fmake( units, 30 ) ;
fmake( object, 30 ) ;
fmake( instrument, 30 ) ;
fmake( coor, 30 ) ;
fmake( observer, 30 ) ;

Irdsname.a = irds.a ;
Irdsname.l = MAXTXTLEN ;

status = 0 ;
if( gds_exist_c( outirds, &status ) ) return ;
for( no = 0 ; no < *no_irds ; no++ ){
	anyout_c( TEST, Irdsname ) ;
	status = 0 ;
	irds_enquire_c( Irdsname, object, instrument, &naxis, axes,
			centre, size, coor, &epoch, &status ) ;
	if( status == 0 || status == -7 ){
		status = 0 ;
		gdsd_rchar_c( Irdsname, tofchar("OBSERVER"), &nul,
				observer, &status ) ;
		status_c( tofchar("creating output set") ) ;
		status = 0 ;
		irds_create_c( outirds, instrument, axes, centre, size,
					coor, &epoch, object, observer, &status ) ;
		level = status = 0 ;
		gdsd_rchar_c( Irdsname, tofchar("BUNIT"), &level, units, &status ) ;
		gdsd_wchar_c( outirds, tofchar("BUNIT"), &level, units, &status ) ;
		anyout_c( &device, tofchar("Output set created") ) ;
		break ;
	}
	Irdsname.a += Irdsname.l ;
}
free( object.a ) ; object.l = 0 ;
free( observer.a ) ; observer.l = 0 ;
free( coor.a ) ; coor.l = 0 ;
free( instrument.a ) ; instrument.l = 0 ;
free( Irdsname.a ) ; Irdsname.a = 0 ;
return ;
}

void copy_sets( fchar irds, int *no_irds, fchar outirds )
{
int	no ;
fint	status, error ;
fchar	Irdsname ;

Irdsname.a = irds.a ;
Irdsname.l = MAXTXTLEN ;

create_outset( irds, no_irds, outirds ) ;
for( no = 0 ; no < *no_irds ; no++ ){
	if( 
	    strncmp( Irdsname.a, outirds.a, max(nelc_c(Irdsname),nelc_c(outirds)) ) 
	){
		sprintf( Cstring, "Merging %.*s", nelc_c(Irdsname), Irdsname.a ) ;
		anyout_c( TEST, tofchar(Cstring) ) ;
		status_c( tofchar(Cstring) ) ;
		status = 0 ;
		irds_merge_c( Irdsname, outirds, &status ) ;
		sprintf( Cstring, "irds_merge: status %d", status ) ;
		anyout_c( TEST, tofchar(Cstring) ) ;
		switch( status ){
		case -2:sprintf( Cstring, "%.*s does not pertain to the same plate as %.*s",
				nelc_c(Irdsname), Irdsname.a, nelc_c(outirds), outirds.a ) ;
			status_c( tofchar(Cstring) ) ;
			anyout_c( &device, tofchar(Cstring) ) ;
			break ;
		case -3:sprintf( Cstring, "%.*s: not the same instrument as %.*s",
				nelc_c(Irdsname), Irdsname.a, nelc_c(outirds), outirds.a ) ;
			status_c( tofchar(Cstring) ) ;
			anyout_c( &device, tofchar(Cstring) ) ;
			break ;
		default:break ;
		}
	}
	else{
		sprintf( Cstring, "%.*s is not a proper IRDS",
				nelc_c(Irdsname), Irdsname.a ) ; 
		anyout_c( &device, tofchar(Cstring) ) ;
	}
	error = 0 ;
	gds_close_c( Irdsname, &error ) ;
	Irdsname.a += Irdsname.l ;
}				/* end of for loop */
return ;
}

MAIN_PROGRAM_ENTRY   
{
int	no_irds ;
fchar	irds, outirds ;

	init_c() ;
	IDENTIFICATION( PROGRAM , VERSION ) ;

	fmake( irds, MAXIRDS*MAXTXTLEN ) ;
	irds.l = MAXTXTLEN ;
	fmake( outirds, MAXTXTLEN ) ;

	get_outset( outirds ) ;
	do{
		get_insets( irds, &no_irds ) ;
		copy_sets( irds, &no_irds, outirds ) ;
		cancel_c( IRKEY ) ;
	}while(1) ;

	finis_c() ;
	return( 0 ) ;
}
