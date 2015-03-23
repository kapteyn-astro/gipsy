/*
                           COPYRIGHT (c) 1991
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>            irdspos.dc1

Program:      irdspos

Purpose:      Extract projected sky positions from positions
              in an IRDS set.

Category:     IRAS

File:         irdspos.c

Author:       Fred Lahuis

Keywords:

   IRSET=     IRDS data set.

   POS=       Pixel position for which to return the                   [1,1,0,1]
              sky position. The pixel position should be given as 
              an array containing SAMPLE , SDET, SNIP and TICK in 
              subsequent order.
              SDET = 0 corresponds to boresight,
              SDET < 0 corresponds to the center of gravity of 
              band number SDET (see IRCC_BANDNR etc.).
              Use of one of the hidden keywords SAMPLE, TICK, SDET 
              and SNIP overwrites the corresponding value given here.

   COOR=      Coordinate system.                      [as defined in the header]
              Standard coordinate systems:
                  Equatorial
                  Galactic
                  Ecliptic
                  Supergalactic
                  Sunreferenced
   
 **PROJ=      Type of projection.                                         [NONE]
              Entering H(elp) or ? returns the projections possible.
                               
 **SAMPLE=    Array containing sequential SAMPLE points.                [pos[0]]

 **SDET=      Array containing sequential SDET points.                  [pos[2]]
              SDET = 0 corresponds to boresight,
              SDET < 0 corresponds to the center of gravity of
              band number SDET (see IRCC_BANDNR etc.)

 **SNIP=      Array containing sequential SNIP points.                  [pos[3]]

 **TICK=      Array containing sequential TICK points.                  [pos[1]]


Description: 

Comments:                  

Updates:      Juli 2 1991:     FL Document created.
              December 4 1991: FL input to COOR and PROJ changed.

#<

*/


#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"
#include "nelc.h"
#include "anyout.h"
#include "cancel.h"
#include "error.h"
#include "usercharu.h"
#include "userint.h"
#include "userreal.h"
#include "irds_rd_detpos.h"
#include "gdsinp.h"
#include "irco_number.h"
#include "irco_precess.h"
#include "irco_prname.h"
#include "irco_prnumber.h"
#include "irco_namepoch.h"
#include "irds_enquire.h"
#include "irus_coor.h"
#include "dms.h"
#include "hms.h"
#include "math.h"

#define  VERSION     "1.0"
#define  PROGRAM     "IRDSPOS"

#define  inkey       tofchar("IRSET=")
#define  inmess      tofchar("Set and subset to work on:")
#define  poskey      tofchar("POS=")
#define  posmess     tofchar("Give pixel position in (SAMPLE,TICK,SDET,SNIP): [1,1,0,1]")
#define  samplekey   tofchar("SAMPLE=")
#define  samplemess  tofchar("Give SAMPLE pixel positions: [1]")
#define  tickkey     tofchar("TICK=")
#define  tickmess    tofchar("Give pixel positions in TICK : [1]")
#define  sdetkey     tofchar("SDET=")
#define  sdetmess    tofchar("Give pixel positions in SDET : [0]")
#define  snipkey     tofchar("SNIP=")
#define  snipmess    tofchar("Give pixel positions in SNIP: [1]")
#define  projkey     tofchar("PROJ=")
#define  projmess    tofchar("Give type of projection : [NONE]")
#define  coorkey     tofchar("COOR=")
#define  ECL         3
#define  RAD         ( 180.0 / 3.1415926535897932384 )
#define  MAXAX       4
#define  MAXSUB      1000
#define  EXACT       4				/* default for messages */
#define  HIDDEN      2				/* id. */
#define  REQUEST     1				/* id. */
#define  NONE        0				/* id. */
#define  BUFSIZE     250
#define  anyoutC     anyout_c( &device , tofchar(Cstring) )
#define  anyout(a)   anyout_c( &device , tofchar(a) )
#define  mess(a)     (message = tofchar((a)))
#define  fmake(fchr,size)	{ \
					static char buff[size+1] ; \
					int i ; \
					for( i = 0 ; i < size ; buff[i++] = ' ') ; \
					buff[i] = 0 ; \
					fchr.a = buff ; \
					fchr.l = size ; \
				}

static fint	irds_ss[MAXSUB] ;				/** set related stuff **/
static fint	axnum[MAXAX] ;
static fint	maxaxes = MAXAX, maxsub = MAXSUB ; 
static fint	class = 1 ;
static fint	nsub, subdim ;
static fint	naxis, axis[MAXAX] ;

static fint	bufsize = BUFSIZE ;				/** items for storing position **/
static fint	pos[] = {1,1,0,1} ;
static fint	sample[32], tick[BUFSIZE], sdet[16], snip[BUFSIZE] ;
static fint	no_sample, no_tick, no_sdet, no_snip ;

static fint	error ;						/** varia **/
static fint	status ;
static fint	device = 11 ;
static fint	dfault ;
static fint	ndata ;
static fint	j, k, l, n, no ;
static fint	one = 1, four = 4 ;
static fint	true = 1, false = 0 ;
static fint	proj = 0 ;

static fint	ecl = ECL ;
static fint	coor, coor_out ;
static fint	proceed ;

static fint	len[] = { 15 , 20 , 20 , 15 } ;		/** length of the output colums , **/

static double	lon[32], lat[32], twist[32] ;
static double	size[2], centre[2] ;

static float	epoch ;
static float	mission = 1983.5 ;

char  Cstring[80], Cstring2[20];

fchar message, object, instrument ;
fchar irds, coor_name, proj_name, Fstring ;

MAIN_PROGRAM_ENTRY
{
fint	nul = 0 ;

	init_c() ;
	IDENTIFICATION( PROGRAM, VERSION ) ;
	
	dfault = NONE ;
	fmake( irds, 80 ) ;
	fmake( coor_name, 30 ) ;
	fmake( proj_name, 30 ) ;
	fmake( Fstring, 30 ) ;
	subdim = 0 ;
	nsub = gdsinp_c( irds, irds_ss, &maxsub, &dfault, inkey, inmess, 
			&device, axnum, axis, &maxaxes, &class, &subdim ) ;
	irds_enquire_c( irds, object, instrument, &naxis, axis, centre,
			size, coor_name, &epoch, &status ) ;

	dfault = REQUEST ;			/** position as SAMPLE,TICK,SDET,SNIP **/
	no = userint_c( pos, &four, &dfault, poskey, posmess ) ;

	dfault = HIDDEN ;
	do{
		no_sample = userint_c( sample, &bufsize, &dfault, samplekey, samplemess ) ;
		if( !no_sample ){ no_sample = 1 ; sample[0] = pos[0] ; }
		if( no_sample > axis[0] ){
			proceed =  false ;
			cancel_c( samplekey ) ;
			dfault = REQUEST ;
			sprintf( Cstring, "number of samples is %d", axis[0] ) ; 
			anyoutC ;
		}
		else proceed = true ;
	}while( !proceed ) ;
	dfault = HIDDEN ;
	do{
		no_tick = userint_c( tick, &bufsize, &dfault, tickkey, tickmess ) ;
		if( !no_tick ){ no_tick = 1 ; tick[0] = pos[1] ; }
		if( no_tick > axis[1] ){
			proceed =  false ;
			cancel_c( tickkey ) ;
			dfault = REQUEST ;
			sprintf( Cstring, "maximum number of ticks is %d", axis[1] ) ; 
			anyoutC ;
		}
		else proceed = true ;
	}while( !proceed ) ;
	dfault = HIDDEN ;
	do{
		no_sdet = userint_c( sdet, &bufsize, &dfault, sdetkey, sdetmess ) ;
		if( !no_sdet ){ no_sdet = 1 ; sdet[0] = pos[2] ; }
		if( no_sdet > 2*axis[2]+1 ){
			proceed =  false ;
			cancel_c( sdetkey ) ;
			dfault = REQUEST ;
			sprintf( Cstring, "number of detectors is %d", axis[2] ) ; 
			anyoutC ; 
		}
		else proceed = true ;
	}while( !proceed ) ;
	dfault = HIDDEN ;
	do{
		no_snip = userint_c( snip, &bufsize, &dfault, snipkey, snipmess ) ;
		if( !no_snip ){ no_snip = 1 ; snip[0] = pos[3] ; }
		if( no_snip > axis[3] ){
			proceed =  false ;
			cancel_c( sdetkey ) ;
			dfault = REQUEST ;
			sprintf( Cstring, "maximum number of snips is %d", axis[3] ) ; 
			anyoutC ;
		}
		else proceed = true ;
	}while( !proceed ) ;
	
	dfault = HIDDEN ;
	
	while ( TRUE ) {
		dfault = REQUEST ;
		if ( !usercharu_c( proj_name, &one, &dfault, projkey, projmess ) )
					strcpy( proj_name.a, "NONE" ) ;
		proj = irco_prnumber_c( proj_name ) ;
		if ( !proj && strncmp( proj_name.a, "NONE", 4) ){
			if ( strncmp( proj_name.a, "H", 1) && strncmp( proj_name.a, "?", 1) )
				sprintf( Cstring, "Unknown projection type: %s", proj_name.a ) ;
			else sprintf( Cstring, "" ) ;
			anyoutC ;
			anyout( "Options: NONE   STEREO GNOMON AZEQD  AZEQA  ORTHO " );
			anyout( "         CYLIND MERCAT SINUS  AITOFF CYLEQD" );
			cancel_c( projkey );
		}else break ;
	}

	irco_precess_c( &ecl, &mission, &ecl ) ;

	coor = irco_number_c( coor_name, &epoch ) ;
	if( coor < 0 ){
		k = abs(coor) ;
		coor = 0 ;
		irco_precess_c( &k, &epoch, &coor ) ;
	}
	dfault = REQUEST ;
	coor_out = irus_coor_c( &coor, &dfault, coorkey ) ;
	irco_namepoch_c( &coor_out, coor_name, &epoch ) ;

	anyout("") ;
	sprintf( Cstring, "Coordinate system : %.*s", nelc_c(coor_name), coor_name.a ) ;
	if( !strncmp( coor_name.a, "EQU", 3 ) || !strncmp( coor_name.a, "ECL", 3 ) )
			sprintf( Cstring, "%s %.1f", Cstring, epoch ) ;
	anyoutC ;
	if( proj ){
		irco_prname_c( proj_name, &proj ) ;
		sprintf( Cstring, "%.*s projection", nelc_c(proj_name), proj_name.a ) ;
		anyoutC ;
	}
	anyout("") ;
	if( !strncmp(coor_name.a, "EQU", 3) )
		sprintf( Cstring, "%-*s%*s%-*s%*s%-*s%*s%-s", 
			len[0], "(SA,TI,SD,SN)",
			(len[1]-3)/2, "" , len[1]-(len[1]-3)/2 , "RA" , 
			(len[2]-3)/2 , "" , len[2]-(len[2]-3)/2 , "DEC" ,
			(len[3]-11)/2 , "" , "TWIST angle" ) ;
	else	sprintf( Cstring , "%-*s%*s%-*s%*s%-*s%*s%-s" , 
			len[0] , "(SA,TI,SD,SN)" ,
			(len[1]-3)/2 , "" , len[1]-(len[1]-3)/2 , "lon" , 
			(len[2]-3)/2 , "" , len[2]-(len[2]-3)/2 , "lat" ,
			(len[3]-11)/2 , "" , "TWIST angle" ) ;
	anyoutC ;
	sprintf( Cstring , "%*s%-s" ,
			len[0]+len[1]+len[2]+(len[3]-5)/2 , " " , "(deg)" ) ;
	anyoutC ;
	for( j = 0 ; j < no_tick ; j++ ){
		for( k = 0 ; k < no_sdet ; k++ ){
  			for( l = 0 ; l < no_snip ; l++ ){
				error = 0 ;
				ndata = axis[0] ;	/** only one tick each time, so axis[0] sample points **/
				for( n = 0 ; n < axis[0] ; n++ )lon[n] = lat[n] = twist[n] = 0.0 ;
				irds_rd_detpos_c( irds, &snip[l], &sdet[k], &tick[j], &coor_out, 
						&proj, lon, lat, twist, &ndata, &error ) ;
				if( !error ){
					for( n = 0 ; n < no_sample ; n++ ){
				  	 	no = sample[n] - 1 ;
				  		lon[no] = fmod( 360 + lon[no]*RAD , 360 ) ;
				  		lat[no] *= RAD ;
						twist[no] *= RAD ;
						sprintf( Cstring , " %2d %2d %2d %2d " , 
							sample[n] , tick[j] , sdet[k] , snip[l] ) ;
						if( coor_out == 1 )	/** RA-DEC only if coor. sys. is equatorial **/
							hms_c( &lon[no], Fstring, NULL, &one, &nul ) ;
						else dms_c( &lon[no], Fstring, NULL, &one, &nul ) ;
						sprintf( Cstring , "%-*.*s%*s%-s" ,
							len[0] , len[0] , Cstring ,
							(int)( (len[1]-nelc_c(Fstring))/2 ) , " " , 
							Fstring.a ) ;
						dms_c( &lat[no], Fstring, NULL, &one, &nul ) ;
						sprintf( Cstring , "%-*.*s%*s%-s" ,
							len[0]+len[1] , len[0]+len[1] , Cstring  ,
							(int)( (len[2]-nelc_c(Fstring))/2 ) , " " , 
							Fstring.a ) ;
						sprintf( Cstring2 , "%-.3f" , twist[no] ) ;
						sprintf( Cstring , "%-*.*s%*s%-s" ,
								len[0]+len[1]+len[2] , len[0]+len[1]+len[2] , Cstring , 
								(len[3]-strlen(Cstring2))/2 , " " , Cstring2 ) ;
						anyoutC ;
					}
				}
				else{
					sprintf( Cstring , " %2d %2d %2d %2d " , 
						sample[n] , tick[j] , sdet[k] , snip[l] ) ;
					switch( error ){
					case( -1 ) :
						anyout( "IRDS does not exist" ) ;
						finis_c() ; 
						break ;
					case( -2 ) :
						anyout( "IRDS is not a legal irds" ) ; 
						finis_c() ;
						break ;
					case( -3 ) :
						sprintf( Cstring , "%.*s   SNIP not in IRDS" , len[0] , Cstring ) ; 
						anyoutC ;
						break ;
					case( -4 ) :
						sprintf( Cstring , "%.*s   SDET not in IRDS" , len[0] , Cstring ) ; 
						anyoutC ;
						break ;
					case( -5 ) :
						sprintf( Cstring , "%.*s   TICK not in IRDS" , len[0] , Cstring ) ; 
						anyoutC ;
						break ;
					case( -6 ) :
						anyout( "   gds read error" ) ; break ;
					case( -7 ) :
						anyout( "   no coordinate info in header" ) ; break ;
					case( -8 ) :
						anyout( "   bad SDET" ) ; break ;
					default    :
						anyout( "   beats me" ) ; break ;
					}
				}
			}
		}
	}
	anyout("") ;
	finis_c() ;
	return(0) ;
}
