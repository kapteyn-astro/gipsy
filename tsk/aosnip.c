/*aosnip.c

                            COPYRIGHT (c) 1990
            Kapteyn Astronomical Institute - University of Groningen
                P.O. Box 800, 9700 AV Groningen, The Netherlands

#>              aosnip.dc1

Program:        aosnip

Purpose:        Snip AO-scan into seperate legs

Category:       IRAS

File:           aosnip.c

Author:         Fred Lahuis

Keywords:

    IRSET=      Name of input IR data set                   [quit]
    
    IRDS=       idem

    OUTSET=     Name of output IR data set             [input set]

**OVERWRITE=    Will overwrite the output set                  [Y]
                if it already exists.

Description:    AOSNIP reads the IRDS specified with INSET=. 
		The input IRDS must include bphf data, if a snip
		does not have pbhf data it is not snipped.
		For each satcal tick for all snips in INSET it will check 
		wether abs(PSIRATE) == abs(intended psirate). Only those 
		will be written to OUTSET. A change of sign in PSIRATE
		will result in the start of a new snip.

Updates:        Dec 12, 1990: PA, Document created.
                                  Original program by P. Arendz.
                Jan 21, 1992: FL, Program revised and installed.
		April 27, 1992: FL, First find longest leg, before snipping.	
		May 15, 1992: FL, I/O to OUTSET changed and bug removed.
		May 21, 1992: FL, BUNIT added to outset.
		Dec  1, 1992: VOG, Category added in document
#<
*/

#include "gipsyc.h"
#include "cmain.h"
#include "stdio.h"
#include "string.h"
#include "ctype.h"
#include "stdlib.h"
#include "math.h"
#include "nelc.h"
#include "init.h"
#include "finis.h"
#include "deputy.h"
#include "wkey.h"
#include "anyout.h"
#include "status.h"
#include "cancel.h"
#include "error.h"
#include "gds_delete.h"
#include "gds_exist.h"
#include "gds_rename.h"
#include "ircc_rate.h"
#include "irds_create.h"
#include "irds_delete.h"
#include "irds_exist.h"
#include "irds_extend.h"
#include "irds_enquire.h"
#include "irds_enquire_snip.h"
#include "irds_merge.h"
#include "irds_rd_bphf.h"
#include "irds_wr_bphf.h"
#include "irds_rd_samples.h"
#include "irds_wr_samples.h"
#include "gdsd_rchar.h"
#include "userint.h"
#include "userlog.h"
#include "userreal.h"
#include "usertext.h"
#include "irtp_sa2bphf.h"
#include "ftsd_geth.h"
#include "ftsi_geti.h"
#include "ftsd_rreal.h"
#include "ftsd_rdble.h"
#include "mtopen.h"
#include "mtfsf.h"
#include "mtclose.h"
#include "gdsc_word.h"
#include "gdsd_rreal.h"
#include "gdsd_wreal.h"
#include "gdsd_rchar.h"
#include "gdsd_wchar.h"

void irds_snip_copy( fchar, fchar, fint*, fint*, fint*, fint*, fint*, fint*, fint* ) ;

/* definitions for error levels */

/*static  fint            error_level_fatal      =   4;
#define FATAL_ERROR     ( &error_level_fatal   )
static  fint            error_level_serious    =   3;
#define SERIOUS_ERROR   ( &error_level_serious )
static  fint            error_level_minor      =   2;
#define MINOR_ERROR     ( &error_level_minor   )*/
static  fint            error_level_warning    =   1;
#define WARNING         ( &error_level_warning )

/* definitions for anyout levels */

static  fint            anyout_level_default   =  0 ;
#define ANYOUT_DEF      ( &anyout_level_default   )
/*static  fint            anyout_level_terminal  =  1 ;
#define ANYOUT_TERM     ( &anyout_level_terminal  )
static  fint            anyout_level_logfile   =  2 ;
#define ANYOUT_LOG      ( &anyout_level_logfile   )
static  fint            anyout_level_dumb_user =  8 ;
#define ANYOUT_NOEXP    ( &anyout_level_dumb_user )*/
static  fint            anyout_level_test      = 3;
/*static  fint            anyout_level_test      = 16 ;*/
#define ANYOUT_TST      ( &anyout_level_test      )

/* definitions for default levels */

/*
static  fint            default_no_default     =  0 ;
#define DFLT_NONE       ( &default_no_default  )
*/
static  fint            default_has_default    =  1 ;
#define DFLT_DEF	( &default_has_default )
/*static  fint            default_exact_number   =  4 ;
#define DFLT_EXACT      ( &default_exact_number)*/

#define max(a,b)	((a)>(b)?(a):(b))
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.l = len ; }

/* identification */
#define VERSION           "1.2"			/* version number */
#define PROGRAM		"aosnip"		/* program name */

/* keywords and USER*** message strings */
#define INSET_KEY	tofchar("IRSET=")
#define IRDS_KEY	tofchar("IRDS=")
#define INSET_MES	tofchar("Give input IR data set [quit]")
#define OUT_KEY		tofchar("OUTSET=")
#define OUT_MES		tofchar("Give output IR data set [input set]")
#define OVER_KEY	tofchar("OVERWRITE=")

/* miscellaneous definitions */
#define	FOREVER		for ( ; ; )
#define true               1
#define false              0
#define MAXTXTLEN        250			/* length of textlines */
#define HEDLEN		5000			/* length of BPHF tape header */
#define BUFWIDTH	   4			/* read 4 data items per satcal */
#define SCANLEN		5000			/* max length of scan in satcals */
#define	RDBLOCK		SCANLEN * BUFWIDTH	/* size of a block to read */
#define NPARAMS		  13			/* nr of params for BPHF conversion */
#define	PI		3.14159265358979	/* Value of pi	*/

/*
GetSet asks the user for an IRDS to process. The routine checks whether
the IRDS exists. If so control returns to 
the caller, if not the user is prompted for a new IRSD name.
If <CR> is given to the keyword the routine returns NULL indicating that
the user wants to terminate the program.
*/
fint GetSet( fchar setname )
{
char      line[MAXTXTLEN]   ;

fint      ierr   = 0 ;
fint      nitems = 0 ;

int       found  = 0 ;
int       done   = 0 ;

	anyout_c( ANYOUT_TST , tofchar(" - GetSet") ) ;
	while ( !done && !found ){
		nitems = usertext_c( setname, DFLT_DEF, INSET_KEY, INSET_MES ) ;/*get set*/
		if( nitems == 0 ) nitems = usertext_c( setname, DFLT_DEF, IRDS_KEY, INSET_MES ) ;/*get set*/
		done = ( nitems == 0 ) ; 					/* did user type CR? */
		if( !done ){
			found = irds_exist_c( setname , &ierr ) ;		/* does INSET exist? */
			if( found != 0 ){					/* INSET is not Irds */
				sprintf( line, "IRSET %.*s is not a good IR data set (%d)" , 
						nelc_c( setname ), setname.a, found ) ;
				error_c( WARNING , tofchar( line ) ) ;		/* tell user */
				cancel_c( INSET_KEY ) ;				/* give another chance */
				found = false ;
			}
			else found = true ;
		}
	}
	return( done ) ;
}

fint NewSet( fchar newname, fchar setname, fint *newset )
{					/* NewSet asks the user for the name of the output IRDS. */
char	line[MAXTXTLEN]   ;		/* If <CR> is given to the keyword the routine returns NULL */
					/* indicating that the user wants to terminate the program. */
fint	one = 1 ;
fint	ierr   = 0 ;
fint	nitems = 0 ;
fint	no, status, overwrite ;

int	done   = 0 ;
int	exist  = 1 ;

	anyout_c( ANYOUT_TST , tofchar(" - NewSet") ) ;
   
	while ( !done && exist ){
		nitems = usertext_c( newname, DFLT_DEF, OUT_KEY, OUT_MES ) ;
		done = ( nitems == 0 ) ;
		if( !done ){
			exist = gds_exist_c( newname, &ierr ) ;			/* does INSET exist? */
			if( exist ){
				overwrite = 1 ;
				if( !strncmp( setname.a, newname.a, max(nelc_c(setname),nelc_c(newname)) ) ){
					*newset = 0 ;
					sprintf( line, "output set equal to input set, will overwrite n/[y]" ) ;
					no = userlog_c( &overwrite, &one, DFLT_DEF, OVER_KEY, tofchar(line) ) ;
				}
				else{
					*newset = 1 ;
					sprintf( line , "OUTSET %.*s already exists will overwrite? [yes]" , 
						nelc_c( newname ), newname.a ) ;
					no = userlog_c( &overwrite, &one, DFLT_DEF, OVER_KEY, tofchar(line) ) ;
				}
				if( !overwrite ){
					cancel_c( OUT_KEY ) ;
					cancel_c( OVER_KEY ) ;
				}
				else{
					cancel_c( OVER_KEY ) ;
					if( *newset ) gds_delete_c( newname, &status ) ;
					else strncpy( newname.a, "aosnipSCR", 9 ) ;
					exist = 0 ;
				}
			}
			else *newset = 1 ;
		}
		else{
			strncpy( newname.a, "aosnipSCR", 9 ) ;
			*newset = done = exist = 0 ;
		}
	}
	return( done ) ;
}

void find_longest_leg(	fchar	setname,
			fint	*axlen,
			double	*srlon,
			double	*srlat,
			double  *esrlon,
			double	*esrlat,
			double	*twist )
{
fint	status ;
fint	snipnr, first_tick, tick ;
fint	sop, obs, att, scancal, scandur, snipcal, snipdur ;
fint	leg_start, leg_length, rate_sign ;
float	ipsi, itheta, ipsirate ;
double	lngsun , psirate, sunrate ;
fchar	scantype ;

	finit( scantype   , 20) ;
	axlen[1] = 0 ;
	for( snipnr = 1 ; snipnr <= axlen[3] ; snipnr++ ){
		irds_enquire_snip_c( setname, &snipnr, &sop, &obs, &att, scantype, &scancal, &scandur,
				&snipcal, &snipdur, &ipsi, &ipsirate, &itheta, &status ) ;
		first_tick = 0 ;
		tick = 1 ;
		irds_rd_bphf_c( setname, &snipnr, &tick, srlon, esrlon, srlat, esrlat,
				twist, &lngsun, &sunrate, &snipdur, &status) ;
		if( status == 1 ){
			return ;
		}
		psirate = srlon[1] - srlon[0] ;
		rate_sign = (psirate > 0 ) ;

		for( tick = first_tick = 1 ; tick < snipdur ; tick++ ){
			psirate = srlon[tick] - srlon[tick-1] ;		/* direction of tick */
			if( (rate_sign && psirate < 0) || (!rate_sign && psirate >= 0) ){
						/* next tick has other sign :  new leg */
				leg_length = tick - first_tick + 1 ;
				if( leg_length > axlen[1] ) axlen[1] = leg_length ;
				first_tick = tick + 1 ;
				rate_sign = !rate_sign ;
				leg_start += leg_length ;		/* snipcal of the next snip */
			}						/* change of sign :  new leg */
		}							/* for all ticks in input (sub)scan */
		if( first_tick <= snipdur ){				/* last leg */
			leg_length = snipdur - first_tick + 1 ;
			if( leg_length > axlen[1] ) axlen[1] = leg_length ;
		}
	}
	return ;
}
MAIN_PROGRAM_ENTRY
{
char      line[MAXTXTLEN];

fchar     setname, newname, scantype, instrument ;
fchar     units, cosys , object, observer;

fint	one = 1 ;
fint	done  , status, level ;
fint	naxis , axlen[4] , snipnr ;
fint	sop , obs , att , scancal , scandur , snipcal , snipdur, tick ;
fint	rate_sign, first_tick, legnr = 0 ;
fint	leg_start, leg_length, rate ;
fint	newset ;

double	center[2] , size [2] ;
float	ipsi , itheta , ipsirate , epoche ;
	
double	*srlon, *esrlon ;
double	*srlat, *esrlat ;
double	*twist ;
double	lngsun , psirate, sunrate ;

init_c() ;
IDENTIFICATION( PROGRAM , VERSION ) ;
  
finit( setname    , MAXTXTLEN ) ;			/* initialise fchars */
finit( newname    , MAXTXTLEN ) ;
finit( instrument , MAXTXTLEN ) ;
finit( cosys      , MAXTXTLEN ) ;
finit( object     , MAXTXTLEN ) ;
finit( observer   , MAXTXTLEN ) ;
finit( scantype   , 20 ) ;
finit( units      , 20 ) ;

done = GetSet( setname ) ;				/* get a setname */
if( !done ) done = NewSet ( newname, setname, &newset ) ;	/* get output irds name */

if( !done ){
	irds_enquire_c( setname, object, instrument, &naxis, axlen,
			center, size, cosys, &epoche, &status ) ;
	sprintf( line, "I'll snip IR-AO set %.*s (%.*s)", 
			nelc_c( setname ), setname.a, nelc_c( object ) , object.a ) ;
	anyout_c( ANYOUT_DEF, tofchar( line ) ) ;
	gdsd_rchar_c( setname, tofchar( "OBSERVER" ), &done, observer, &status) ;
	if( irds_exist_c( newname, &status ) ) irds_delete_c(newname, &status ) ;

	srlon = malloc( axlen[1]*sizeof(double) ) ;
	srlat = malloc( axlen[1]*sizeof(double) ) ;
	esrlon = malloc( axlen[1]*sizeof(double) ) ;
	esrlat = malloc( axlen[1]*sizeof(double) ) ;
	twist = malloc( axlen[1]*sizeof(double) ) ;

	find_longest_leg( setname, axlen, srlon, srlat, esrlon, esrlat, twist ) ;
	irds_create_c( newname, instrument, axlen , center , size,
			cosys, &epoche, object, observer, &status ) ;
	level = status = 0 ;
	gdsd_rchar_c( setname, tofchar("BUNIT"), &level, units, &status ) ;
	gdsd_wchar_c( newname, tofchar("BUNIT"), &level, units, &status ) ;
	sprintf( line, "I'll create IR-AO set %.*s (%.*s)", 
			nelc_c( newname), newname.a, nelc_c( object ) , object.a ) ;
	anyout_c( ANYOUT_DEF, tofchar( line ) ) ;

	for( snipnr = 1 ; snipnr <= axlen[3] ; snipnr++ ){
		irds_enquire_snip_c( setname, &snipnr, &sop, &obs, &att, scantype, &scancal, &scandur,
				&snipcal, &snipdur, &ipsi, &ipsirate, &itheta, &status ) ;
 		sprintf( line, " Scan %4d, sop/att %3d/%-3d, satcal %8d length %4d",
        	                 snipnr, sop, att, scancal + snipcal, snipdur ) ;
		anyout_c( ANYOUT_DEF , tofchar( line ) ) ;

		first_tick = 0 ;
		rate = ircc_rate_c( instrument) ;
		 
		tick = 1 ;
		irds_rd_bphf_c( setname, &snipnr, &tick, srlon, esrlon, srlat, esrlat,
				twist, &lngsun, &sunrate, &snipdur, &status) ;
		if( status == 1 ){
			sprintf( line, "<AOSNIP> No BPHF data for snip %d", snipnr ) ;
			anyout_c( ANYOUT_DEF, tofchar(line) ) ;
			status_c( tofchar(line) ) ;
		}
		else{
			psirate = srlon[1] - srlon[0] ;
			rate_sign = (psirate > 0 ) ;
	
			leg_start = snipcal ;
			for( tick = first_tick = 1 ; tick < snipdur ; tick++ ){
				psirate = srlon[tick] - srlon[tick-1] ;		/* direction of tick */
				if( (rate_sign && psirate < 0) || (!rate_sign && psirate >= 0) ){
							/* next tick has other sign :  new leg */
					legnr ++ ;
					leg_length = tick - first_tick + 1 ;
					irds_extend_c( newname, &sop, &obs, &att, scantype, &scancal, &scandur,
							&leg_start, &leg_length, &ipsi, &ipsirate,
							&itheta, &status ) ;
					status = 0 ;
					irds_wr_bphf_c( newname, &legnr, &one,
						srlon+(first_tick-1), esrlon+(first_tick-1),
						srlat+(first_tick-1), esrlat+(first_tick-1),
						twist+(first_tick-1), &lngsun, &sunrate,
						&leg_length, &status ) ;
					irds_snip_copy( newname, setname, &legnr, &snipnr,
						&first_tick, &leg_length, &rate, &axlen[2], &status) ;
					if( status < 0 || status == 1 ) printf (" status = %d\n", status ) ;
					sprintf( line, " Leg %5d, sop/att %3d/%-3d, satcal %8d length %4d",
							legnr, sop, att, leg_start+scancal, leg_length ) ;
					anyout_c( ANYOUT_DEF , tofchar( line ) ) ;
					first_tick = tick + 1 ;
					rate_sign = !rate_sign ;
					leg_start += leg_length ;		/* snipcal of the next snip */
				}						/* change of sign :  new leg */
			}							/* for all ticks in input (sub)scan */
			if( first_tick <= snipdur ){				/* last leg */
				legnr ++ ;
				leg_length = snipdur - first_tick + 1 ;
				irds_extend_c( newname, &sop, &obs, &att, scantype, &scancal, &scandur, 
						&leg_start, &leg_length, &ipsi, 
						&ipsirate, &itheta, &status) ;
				irds_wr_bphf_c( newname, &legnr, &one,
						srlon+first_tick-1, esrlon+first_tick-1,
						srlat+first_tick-1, esrlat+first_tick-1,
						twist+first_tick-1, &lngsun, &sunrate,
						&leg_length, &status ) ;
				irds_snip_copy ( newname, setname, &legnr, &snipnr,
						&first_tick, &leg_length, &rate,
						&axlen[2], &status) ;
				if( status ) printf (" status = %d\n", status ) ; status = 0;
       		 		sprintf( line, " Leg %5d, sop/att %3d/%-3d, satcal %8d length %4d",
       		 	                      legnr, sop, att, scancal+leg_start, leg_length ) ;
				anyout_c( ANYOUT_DEF , tofchar( line ) ) ;
			}
		}
	}
	if( !newset ){
		status = 0 ;
		if( legnr != 0 ) status = gds_rename_c( newname, setname ) ;
		else gds_delete_c( newname, &status ) ;
	}
}
finis_c() ;
return(0) ;
}

void irds_snip_copy(
	fchar	irmerge, 		/* to write in */
	fchar	irds, 			/* to read from */
	fint	*wsnip, 		/* snip number to write */
	fint	*rsnip, 		/* snip number to read */
	fint	*rstart, 		/* tick to start reading */
	fint	*rlength,		/* ticks to read/write */
	fint	*rate,			/* samples per tick */
	fint	*maxdet,		/* number of detectors */		
	fint	*status ){

fint	three = 3, four = 4 ;
fint	level ;
fint	wtick, dlev, samps;
float	noise ;
float	*reals ;
char	line[MAXTXTLEN] ;

	reals = malloc( (*rate) * (*rlength) * sizeof(float) ) ;

	for( dlev = 1 ; dlev <= *maxdet ; dlev++ ){
		level = 0 ;
		*status = 0 ;				/* copy NOISE */
		level = gdsc_word_c( irds, &four, rsnip, &level, status ) ;
		level = gdsc_word_c( irds, &three, &dlev, &level, status ) ;
		*status = 0 ;
		gdsd_rreal_c( irds, tofchar( "NOISE" ), &level, &noise, status ) ;
		*status = 0 ;
		level = 0 ;
		level = gdsc_word_c( irmerge, &four, wsnip, &level, status ) ;
		level = gdsc_word_c( irmerge, &three, &dlev, &level, status ) ;
		*status = 0 ;
		gdsd_wreal_c( irmerge, tofchar( "NOISE" ), &level, &noise, status ) ;

		wtick = 1 ;
		samps = (*rate) * (*rlength) ;
		*status = 0 ;
		irds_rd_samples_c( irds, rsnip, &dlev, rstart, reals, &samps, status ) ;
		if( *status < 0 ){
				switch( *status ){
			case -4:sprintf( line, "SDET %d not in irds", dlev ) ;
				anyout_c( ANYOUT_DEF , tofchar( line ) ) ;
				break ;
			default:sprintf( line, "Error reading samples, seq. det %d", dlev ) ;
				anyout_c( ANYOUT_DEF , tofchar( line ) ) ; 
				break ;
			}
		}
		else irds_wr_samples_c( irmerge, wsnip, &dlev, &wtick, reals, &samps, status ) ;
	}
	free( reals ) ;
	return ;
}
