/* irserver.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>	      irserver.dc2

Document:     IRSERVER

Purpose:      Describes the IRAS data server interface.

Category:     IRAS, SYSTEM

File:	      irserver.c

Author:       A.R.W. de Jonge

Description:  The irserver routines are an interface between files on a IRAS
	      data server and the GIPSY environment.

	      The IRAS data are stored on the server as files with a FITS
	      structure. The IRSERVER_OPEN or IRSERVER_SCAN routines
	      can be used to collect one or more of these data files into a
	      scratch directory, making it (read-only) accessable for the
	      FITS routines (see FTS_IO.DC2).
	      The routine IRSERVER_CLOSE subsequently destroys this
	      directory, reclaiming the space.

	      An example of use would be:

		mtid = IRSERVER_OPEN ( ... )		| select data
		REPEAT
			FTSD_GETH ( mtid, ... ) 	| read header
			...
			FTSI_GETR ( mtid, ... ) 	| read data
			...
			skip = FTS_SKIPFIL ( mtid, 1 )	| next file
		UNTIL ( skip .eq. -13 ) 		| (if present)
		IRSERVER_CLOSE ( mtid ) 		| clean up scratch space

	      Additional utility routines present in the interface are:

	      IRSERVER_SEARCH	Searches the IRAS index file

	      Each routine is described in more detail in the
	      appropriate document.

	Related Docs: mtiodev.dc2, fts_io.dc2

Updates:      Jul 13, 1993: AdJ Added irserver_scan
	      Nov 28, 1991: AdJ Check on existence of retrieved files
	      Aug 23, 1991: AdJ Document created.
	      May  4, 2007: JPT Renamed index to indexf.
	      Aug 11, 2009: JPT Renamed getline to GetLine

#<
*/

/* Imports from the outside world */

#include "stdio.h"
#include "math.h"
#include "string.h"
#include "stdlib.h"

#include "gipsyc.h"
#include "nelc.h"
#include "mtopen.h"
#include "mtclose.h"
#include "ircc_obsmode.h"
#include "ircc_bandnr.h"
#include "iras_root.h"

#include "userfio.h"
#include "taskcom.h"
/*
	#include "atfinis.h"	does not work again
 */
	int atfinis ( CleanProc, void *);

#include "irserver_open.h"
#include "irserver_close.h"
#include "irserver_search.h"
#include "irserver_scan.h"

/* General definitions */

#define IRS_LINESIZE	65
#define IRS_INDTYP_LEN	10
#define IRS_LINESTR	( IRS_LINESIZE + 1 )
#define IRS_ERROR_TYPE	-1
#define IRS_ERROR_FILE	-2
#define IRS_ERROR_ALL	-2
#define IRS_KEY_OFFSET	45

static char * index_of_set ( fchar );
static char * alloc_space ( void );
static void destroy_space ( char **  );
static void destroy_all ( void * );
static char * indexfilename ( char * );
static int movetotarget ( char *, char *, int );
static char * map_index_to_filename ( fchar , fchar );
static char * map_sopatt_to_filename ( fint , fint , fchar );

#include "stdarg.h"


/*
#>	      irserver_open.dc2

Function:     IRSERVER_OPEN

Purpose:      Accesses IRAS server data of the specified type for the specified
	      area of sky.

Category:     IRAS, SYSTEM

File:	      irserver.c

Author:       A.R.W. de Jonge

Use:	INTEGER IRSERVER_OPEN (
		ECL_LON,		Input	real
		ECL_LAT,		Input	real
		RADIUS, 		Input	real
		INSTRUMENT )		Input	character*(*)

	IRSERVER_OPEN
		A non-negative number indicates that the
		call was successfull. This number is equivalent
		to the MTID returned by a succesfull call to
		MTOPEN, see MTOPEN.DC2. It must be used in
		successive calls to the fits or mtio routines.
		A negative number denotes one of
		the following errors:
			-1	Illegal INSTRUMENT
			-2	Cannot allocate and use disk space
	ECL_LON
		Ecliptic longitude of the center of the search area,
		epoch 1983.5, radians.
	ECL_LAT
		Ecliptic latitude of the center of the search area,
		epoch 1983.5, radians.
	RADIUS
		Search radius in radians. Any scan which passes witin
		RADIUS from the specified center, will be included
		in the resulting dataset.
	INSTRUMENT
		One of the possible instrument names for IRAS data,
		as described in IRCC_INSTRNAME.DC2,
		eg. 'AO b1', 'survey b2', etc.


	A call to IRSERVER_OPEN replaces an MTOPEN when accessing IRAS
	data on an IRAS data server.
	The resulting MTID identifies a disk-space containing
	(references to or copies of) the requested IRAS data,
	in a read-only form compatible with the MTIODEV interface.

	The user should call IRSERVER_CLOSE to destroy the scratch disk space
	claimed by these utilities.

	Related Docs:	irserver.dc2, irserver_close.dc2
			mtiodev.dc2, mtopen.dc2, fts_io.dc2
			ircc_const.dc2, ircc_instrname.dc2

Updates:      Feb 25, 1992: AdJ Bug (skips first file) repaired.
	      Aug 23, 1991: AdJ Document created.

#<

Fortran to C interface:

@ integer function irserver_open( real, real, real, character )

*/

/* hidden interface between irserver_open and irserver_close */
typedef struct {	fint	mtid ;
			char	* name ;
			} irastape ;
static irastape * tapelist ;
static int	tapecount = 0 ;

static irastape * newtape ( void )
{	/* search unused entry in tapelist, if not, extend tapelist.
	   if allocation of new tapelist or diskspace fails,
		make slot unused and return NULL.
	   return slot with name filled but mtid undefined.
	 */

	irastape * this = & tapelist[0] ;
	while ( this < & tapelist[tapecount] && this->mtid >= 0 ) this++ ;
	if ( this == & tapelist[tapecount] ) {
		void * newlist = tapecount ?
			realloc ( tapelist, (tapecount+1) * sizeof (irastape) ):
			malloc ( sizeof(irastape) );
		if ( newlist == NULL ) {
			errorf ( 3, " Out of memory in irserver_open" );
			return NULL ;
			}
		if ( tapecount == 0 ) {
			/* should register destroy_all with abort as well */
			atfinis ( destroy_all, NULL );
			}
		tapelist = (irastape *) newlist ;
		this = & tapelist[tapecount++] ;
		}
	this->name = alloc_space ( );
	if ( this->name == NULL ) { this->mtid = -1 ; return NULL ; }
	return this ;
	}

fint irserver_open_c ( float * lon, float * lat, float * rad, fchar dataset )
{
	fchar		indexline, indextype ;
	irastape	* thistape ;
	char		line[IRS_LINESTR], * file ;
	fint		found ;
	int		count_index, count_target ;

	indexline.a = line ; indexline.l = IRS_LINESIZE ;
	indextype.a = index_of_set ( dataset );
	indextype.l = strlen ( indextype.a );
	thistape = newtape() ;
	if ( thistape == NULL ) {
		errorf( 3," No space for new IRSERVER tape" );
		return IRS_ERROR_ALL ;
		}
	anyoutf ( 16, "irserver_open using name %s", thistape->name );
	count_index = count_target = 0 ;
	while ( 1 == ( found = irserver_search_c
			( lon, lat, rad, indextype, indexline )
		) ) {
		file = map_index_to_filename ( indexline, dataset );
		if ( file ) {
			anyoutf ( 16, "selected file %s", file );
			if ( movetotarget ( file, thistape->name,
							count_target+1 ) )
				count_target ++ ;
			}
		else errorf ( 3, "cannot get filename for %.*s",
			nelc_c(indexline), indexline.a );
		count_index ++ ;
		}
	if ( found < 0 ) {
		errorf ( 3, " IRSERVER_SEARCH error code %d", found );
		destroy_space ( & thistape->name );
		return IRS_ERROR_ALL ;
		}
	anyoutf ( 16, "irserver_open: %d scans in index, %d on disk",
		count_index, count_target );
	thistape->mtid = mtopen_c ( tofchar ( thistape->name ) );
	if ( thistape->mtid >= 0 ) return thistape->mtid ;
	errorf ( 3, " irserver_open detected mtopen error %d", thistape->mtid );
	destroy_space ( & thistape->name );
	return IRS_ERROR_ALL ;
	}

/*
#>	      irserver_scan.dc2

Function:     IRSERVER_SCAN

Purpose:      Accesses IRAS server data of the specified type for the specified
	      scan(s)

Category:     IRAS, SYSTEM

File:	      irserver.c

Author:       A.R.W. de Jonge

Use:	INTEGER IRSERVER_SCAN (
		SOP,			Input	integer(*)
		ATT,			Input	integer(*)
		COUNT,			Input	integer
		INSTRUMENT )		Input	character*(*)

	IRSERVER_SCAN
		A non-negative number indicates that the
		call was successfull. This number is equivalent
		to the MTID returned by a succesfull call to
		MTOPEN, see MTOPEN.DC2. It must be used in
		successive calls to the fits or mtio routines.
		A negative number denotes one of
		the following errors:
			-1	Illegal INSTRUMENT
			-2	Cannot allocate and use disk space
	SOP
		Array with SOP numbers of requested scans.
	ATT
		Array with ATT numbers of requested scans.
	COUNT
		Number of scans requested. For each scan I, the
		array elements SOP(I) and ATT(I) uniquely identify
		a scan. The data set for that scan, as identified further
		by INSTRUMENT, are put into the dataset.
	INSTRUMENT
		One of the possible instrument names for IRAS data,
		as described in IRCC_INSTRNAME.DC2,
		eg. 'AO b1', 'survey b2', etc.


	A call to IRSERVER_OPEN replaces an MTOPEN when accessing IRAS
	data on an IRAS data server.
	The resulting MTID identifies a disk-space containing
	(references to or copies of) the requested IRAS data,
	in a read-only form compatible with the MTIODEV interface.

	The user should call IRSERVER_CLOSE to destroy the scratch disk space
	claimed by these utilities.

	Related Docs:	irserver.dc2, irserver_close.dc2
			mtiodev.dc2, mtopen.dc2, fts_io.dc2
			ircc_const.dc2, ircc_instrname.dc2

Updates:      Jul 13, 1993: AdJ Added irserver_scan

#<

Fortran to C interface:

@ integer function irserver_scan( integer, integer, integer, character )

*/


fint irserver_scan_c ( fint * sop, fint * att, fint * count, fchar dataset )
{
	irastape	* thistape ;
	char		* file ;
	int		count_request, count_target ;

	thistape = newtape() ;

	if ( thistape == NULL ) {
		errorf( 3," No space for new IRSERVER tape" );
		return IRS_ERROR_ALL ;
		}
	anyoutf ( 16, "irserver_scan using name %s", thistape->name );
	count_target = 0 ;
	for ( count_request = 0 ; count_request < * count ; count_request++ ){
		file = map_sopatt_to_filename ( sop[count_request],
			att[count_request], dataset );
		if ( file ) {
			anyoutf ( 16, "selected file %s", file  );
			if ( movetotarget ( file, thistape->name,
							count_target+1 ) )
				count_target ++ ;
			}
		else errorf ( 3,
			"No file name for sop %d att %d dataset '%.*s'",
			sop[count_request], att[count_request],
			nelc_c(dataset), dataset.a );
		}
	anyoutf ( 16, "irserver_scan: %d scans asked, %d on disk",
		* count, count_target );
	thistape->mtid = mtopen_c ( tofchar ( thistape->name ) );
	if ( thistape->mtid >= 0 ) return thistape->mtid ;
	errorf ( 3, " irserver_scan detected mtopen error %d", thistape->mtid );
	destroy_space ( & thistape->name );
	return IRS_ERROR_ALL ;
	}

/*
#>	      irserver_close.dc2

Function:     IRSERVER_CLOSE

Purpose:      Reclaims disk space used by IRSERVER_OPEN or IRSERVER_SCAN

Category:     IRAS, SYSTEM

File:	      irserver.c

Author:       A.R.W. de Jonge

Use:	IRSERVER_CLOSE (
		MTID	)		Input	integer

	MTID	The reference to a subset of the IRAS data as
		returned by a succesfull call to IRSERVER_OPEN

	A call to IRSERVER_OPEN or IRSERVER_SCAN replaces an MTOPEN when
	accessing IRAS data on an IRAS data server.
	The resulting MTID identifies a disk-space containing
	(references to or copies of) the requested IRAS data,
	in a read-only form compatible with the MTIODEV interface.

	The user MUST call IRSERVER_CLOSE to destroy the scratch disk space
	claimed by these utilities.

	Related Docs:	irserver.dc2, irserver_open.dc2, irserver_scan.dc2

Updates:      Jul 14, 1993: AdJ Added irserver_scan
	      Aug 23, 1991: AdJ Document created.

#<

Fortran to C interface:

@ subroutine irserver_close( integer )

*/

void irserver_close_c ( fint * mtid )
{	irastape * tape = & tapelist [ 0 ] ;
	while ( tape < & tapelist[tapecount] ){
		if ( tape->mtid == * mtid ) {
			mtclose_c ( & tape->mtid );
			tape->mtid = -1 ;
			destroy_space ( & tape->name );
			}
		tape++ ;
		}
	return ;
	}

/*
#>	      irserver_search.dc2

Function:     IRSERVER_SEARCH

Purpose:      Searches the index file on a IRAS data server for scans
	      near a given position on the sky.

Category:     IRAS, SYSTEM

File:	      irserver.c

Author:       A.R.W. de Jonge

Use:	INTEGER IRSERVER_SEARCH (
		ECL_LON,		Input	real
		ECL_LAT,		Input	real
		RADIUS, 		Input	real
		INDEXTYPE,		Input	character
		INDEXLINES )		Output	character

	IRSERVER_SEARCH
		The number of lines found in the index,
		matching the search criterium, and fitting into the
		string INDEXLINES.
		If the return value is negative, it indicates an error:
			-1:	Illegal INDEXTYPE
			-2:	Index files could not be found.
	ECL_LON
		Ecliptic longitude of the center of the search area,
		epoch 1983.5, radians.
	ECL_LAT
		Ecliptic latitude of the center of the search area,
		epoch 1983.5, radians.
	RADIUS
		Search radius in radians. Any scan which passes witin
		RADIUS from the specified center to within one degree
		accuracy, is considered to be found in this search.
	INDEXTYPE
		One of 'AO', 'survey', or 'all'
	INDEXLINES
		A character string to receive the line(s) found from the
		index file. The amount of data found, as indicated
		by the function return value, will be truncated
		or blank-filled to match the length of the string
		INDEXLINES. Each line is 65 characters, formatted as
			??????

	Consecutive calls to this function with the same search
	parameters will retrieve different subsets of the index, until
	a zero return value indicates that no more data remain.

	A call with different search parameters is necessary to
	re-initialize the search.


	Example of use:
		parameter ( linesize = 65 )

		repeat
			count = IRSERVER_SEARCH ( 0.0, 89.0, 1.0, 'AO', text )
			line = 0
			while ( line .lt. count )
				first = 1 + line * linesize
				line = line + 1
				last = min ( len(text), line * linesize )
				print *, text ( first:last )
			endwhile
		until count .le. 0
		if ( count .lt. 0 ) print *, 'error code', count





	Related Docs:	irserver.dc2

Updates:      Aug 26, 1991: AdJ Document created.

#<

Fortran to C interface:

@ integer function irserver_search( real, real, real, character, character )

*/

#define DEGR		(3.141592653589793238462643383279502884 /180)


/* global variables used in the irserver_search context */

static FILE	* indexf = NULL ;
static float	x_ecl_plate = 0.6 , y_ecl_plate = 0.8, z_ecl_plate = 0.0 ;
static float	margin_plate = 10 ;
static float	sintab[541], * costab = & sintab[90] ;

static char * GetLine ( void );
static int indexguess ( char * );
static int psi_check ( char * );

fint irserver_search_c ( float * par_lng, float * par_lat, float * par_rad,
		fchar par_indextype, fchar indexlines )
{
	static float	lng = 0, lat = 0 , radius = -1 ;
	static char	indextype[IRS_INDTYP_LEN] = " " ;


	fint		count ;
	char		* nextline ;

	if ( ! (	lng == *par_lng
		&&	lat == *par_lat
		&&	radius == *par_rad
	&&	strncmp ( par_indextype.a, indextype,
				strlen(indextype) ) == 0
		)) {
			lng = * par_lng ;
			lat = * par_lat ;
			radius = * par_rad ;
			indextype[0] = '\0' ;
			count = nelc_c ( par_indextype );
			if ( count >= IRS_INDTYP_LEN ) return IRS_ERROR_TYPE ;
			char2str ( par_indextype, indextype, count+1 );
			anyoutf ( 16,
				"irserver_search '%s': lng %g lat %g rad %g",
				indextype, lng /DEGR, lat /DEGR, radius/DEGR );
			x_ecl_plate = cos(lng) * cos(lat) ;
			y_ecl_plate = sin(lng) * cos(lat) ;
			z_ecl_plate = sin(lat) ;
			margin_plate = (int)(radius/DEGR + 0.5 );
			if ( margin_plate < 1 ) margin_plate = 1 ;
			if ( strcmp ( indextype, "AO" ) != 0 &&
			     strcmp ( indextype, "survey" ) != 0 &&
			     strcmp ( indextype, "unknown" ) != 0 &&
			     strcmp ( indextype, "all" ) != 0 ) {
				errorf ( 3,
				"irserver_search illegal indextype '%s'",
					indextype );
				return IRS_ERROR_TYPE ;
				}
			indexf = fopen ( indexfilename(indextype), "r" );
			if ( indexf == NULL ) {
				errorf ( 3,
				"irserver_search cannot open file '%s'",
				indexfilename(indextype) );
				indextype[0] = '\0' ;
				return IRS_ERROR_FILE ;
				}
			}
	count = 0 ;
	while ( count < indexlines.l / IRS_LINESIZE && indexf != NULL ) {
		nextline = GetLine ();
		if ( ! nextline ) {
			fclose ( indexf );
			indexf = NULL ;
			indextype[0] = '\0' ;
			}
		else if ( indexguess ( nextline ) ){
			strncpy ( & indexlines.a[count*IRS_LINESIZE],
				nextline, IRS_LINESIZE );
			count++ ;
			}
		}
	return count ;
	}

static char * GetLine ( void )
{	static char result [IRS_LINESTR] ;
	if ( indexf == NULL ) return NULL ;
	if ( fread ( result, IRS_LINESTR, 1, indexf ) != 1 ) return NULL ;
	return result ;
	}


static int indexguess ( char * indexline )
{	int long_sun, theta_scan, theta_low, theta_high ;
	float z_sun_plate ;
	static int first = 0 ;
	if ( first == 0 ) for ( first = 0 ; first <= 90 ; first ++ )
		sintab[180+first] = sintab[360-first] = - (
			sintab[first] = sintab[180-first] = sintab[360+first] =
			sin((float)first * DEGR )
			);
	long_sun =	10 * ( 10 * indexline[0] + indexline[1] ) + indexline[2]
			 - ( 100 * '0' + 10 * '0' + '0' ) ;
	theta_scan =	10 * indexline[3] + indexline[4]
			+	IRS_KEY_OFFSET - 10 * '0' - '0';
	z_sun_plate = x_ecl_plate * costab[long_sun]
			+ y_ecl_plate * sintab[long_sun] ;
	theta_low = theta_scan - margin_plate ;
	if ( theta_low < 0 ) theta_low = 0 ;
	theta_high = theta_scan + margin_plate ;
	if ( theta_high > 180 ) theta_high = 180 ;
	if ( costab[theta_low] >= z_sun_plate
				&& z_sun_plate >= costab[theta_high] )
		return psi_check ( indexline );
	else	return 0 ;
	}

static int psi_check ( char * line )
{	float psinul, psidot, dur, cosd ;
	float x_sun_plate, y_sun_plate ;
	float x_sun_midscan, y_sun_midscan ;
	int lsun, psi, allowsiz ;

/* method: get rectangular coordinates of midpoint of scan and plate.
	then cos(distance-in-psi) = ( x1 * x2 + y1 * y2 ) / norm
	allowed distance in psi is half the scan length plus plate margin.
*/
	sscanf ( line, "%3d%*2c%f%f%*f%*d%f",
		& lsun, & psinul, & psidot, & dur );
	if ( psidot * dur >= 360 || psidot * dur <= -360 ) return 1 ;
			/* full-circle scan ?? */

	x_sun_plate = z_ecl_plate ;
	y_sun_plate = - y_ecl_plate * costab[lsun] +
		x_ecl_plate * sintab[lsun] ;
	psi = (int)( psinul + 0.5 * psidot * dur + 720.5 ) % 360 ;
	allowsiz = (int)(
		0.5 * ( psidot > 0 ? psidot : -psidot ) * dur
		+ margin_plate + 0.5 );
	x_sun_midscan = costab[psi] ;
	y_sun_midscan = sintab[psi] ;
	cosd = ( x_sun_midscan * x_sun_plate + y_sun_midscan * y_sun_plate ) /
		sqrt ( y_sun_plate * y_sun_plate  + x_sun_plate * x_sun_plate );
	return cosd > costab[allowsiz] ;
	}


static char * map_index_to_filename ( fchar indexentry, fchar dataset )
{	int	sop, att ;

	if ( sscanf ( indexentry.a, "%*46c%d%d", & sop, & att ) != 2 ){
		errorf ( 3, " bad syntax on line from indexfile" );
		return NULL ;
		}
	return map_sopatt_to_filename ( (fint)sop, (fint)att, dataset ) ;
	}



/* Details of the specific server implementation should be restricted
	to the following routines */

#ifndef __unix__
#error	UNIX beyond here: filenames, symbolic links, directory access etc.
#endif

static char * indexfilename ( char * type )
{
	static char	namebuff[255] ;
	static fchar	fnamebuff = { namebuff, 254 } ;
	fint		rootlen ;

	rootlen = iras_root_c( fnamebuff ) ;
	sprintf ( &(namebuff[rootlen]), "/index/%s_scans.index", type );
	return namebuff ;
	}

static char *indexmap[] = { "all", "survey", "survey", "AO", "all", "unknown" };

static char * index_of_set ( fchar instrument )
{	return indexmap [ ircc_obsmode_c(instrument) ] ;
	}


static char * alloc_space ( void )
{	/* return, if possible, a pointer to an 'malloc'ated string,
	 the string naming a disk space suitable for 'movetotarget' )
	 */
	char * result ;
	static int count = 0 ;
	extern int getpid(), mkdir() ;
	result = (char*)malloc(100) ;
	if ( result == NULL ) { errorf ( 3, "Out of memory" ); return NULL ; }
	sprintf ( result, "/tmp/irserver.%d.%d", getpid(), ++count );
	if ( mkdir ( result, 0777 ) != 0 ) {
		errorf ( 3, "cannot create directory %s", result );
		free ( result );
		return NULL ;
		}
	return result ;
	}

/* #include <unistd.h> this fail because of a redefinition of types */
#define R_OK 4 /* proceed with fingers crossed */

static int movetotarget ( char * file, char * target, int count )
{	char targetname[255] ;
	extern int symlink (), access() ;
	if ( strlen ( target ) + 14 >=	255 ) return 0 ;
	sprintf ( targetname, "%s/file%06d.mt", target, count );
	return access ( file, R_OK ) == 0 && symlink ( file, targetname ) == 0 ;
	}


/* The next map should be replaced whenever the directory naming on
	the server is consistent with the string returned by IRCC_INSTRNAME */

static char *obsmodemap[] =
	{ "", "survey", "splines", "AO", "flashes", "unknown" } ;
static char *bandnrmap[] = { "", "b1", "b2", "b3", "b4", "lrs", "bphf" } ;


static char * map_sopatt_to_filename ( fint sop, fint att, fchar dataset )
{	static char	result[255] ;
	static fchar	fresult = { result, 254 } ;
	fint		obsmode = ircc_obsmode_c ( dataset );
	fint		bandnr	= ircc_bandnr_c ( dataset );
	fint		rootlen ;

	if ( obsmode == 0 || bandnr == 0 ){
		errorf ( 3, "illegal dataset '%.*s'",
				nelc_c(dataset), dataset.a );
		return NULL ;
		}
	rootlen = iras_root_c( fresult ) ;
	sprintf ( &(result[rootlen]), "/%s.%s/sop.%02d_/%03d%03d",
		obsmodemap[obsmode], bandnrmap[bandnr], sop/10, sop, att );
	return result ;
	}

static void destroy_space ( char ** space )
{	int system ();
	char command[255] ;
	(void) system( strcat ( strcpy( command, "rm -r " ), *space ) );
	free ( *space );
	*space = NULL ;
	return ;
	}

static void destroy_all ( void * ignored )
{	irastape * this =  & tapelist[0] ;
	while ( this < & tapelist[tapecount] ) {
		if ( this->name ) destroy_space ( & this->name );
		this ++ ;
		}
	}

#ifdef TESTBED
#include "cmain.h"
#include "init.h"
#include "finis.h"
#include "pause.h"
#include "userlog.h"
#include "userint.h"
#include "usertext.h"
#include "userreal.h"
#include "canall.h"
#include "sortia.h"

static fint Zero = 0, One = 1 , Two = 2 , Three = 3, Four = 4 ;

MAIN_PROGRAM_ENTRY
{	bool proceed = toflog ( TRUE ) ;
	float lng, lat, rad ;
	fchar instr ;
	char instr_str[255] ;
	fint result, sopatts[100], MaxSopAtts = 100, count;

	instr.a = instr_str ; instr.l = 255 ;

	init_c() ;
	while ( tobool(proceed) ){
		canall_c () ;
		(void) usertext_c ( instr, & Zero,
			tofchar("INSTRUMENT="),
			tofchar("IRAS instrument to select data from")
			);
		count = userint_c ( sopatts, & MaxSopAtts, & One,
			tofchar("SOPATT="),
			tofchar("SOP ATT combination(s) to select [none]" )
			);
		if ( count == 0 ) {
			(void) userreal_c ( & lng, & One, & Zero,
				tofchar("LONG="),
				tofchar("Ecl. longitude of center (degrees) ")
				);
			(void) userreal_c ( & lat, & One, & Zero,
				tofchar("LAT="),
				tofchar("Ecl. latitude of center (degrees) ")
				);
			(void) userreal_c ( & rad, & One, & Zero,
				tofchar("RADIUS="),
				tofchar("Search radius (degrees) ")
				);
			anyoutf ( 0, " testbed long %g lat %g radius %g",
				lng, lat, rad );
			lng *= DEGR ;
			lat *= DEGR ;
			rad *= DEGR ;
			result = irserver_open_c ( & lng, & lat, & rad, instr );
			anyoutf ( 0, "irserver_open returns %d", result );
			}
		else if ( count % 2 != 0 ) {
			anyoutf ( 0, "SOPATT= requires pairs of numbers" );
			continue ;
			}
		else {	int i ;
			for ( i = 0; i < count/2 ; i ++ )
				sopatts[2*i+1] += 100000 ;
			sortia_c ( sopatts, & count );
			count /= 2;
			for ( i = 0 ; i < count ; i ++ )
				sopatts[count+i] -= 100000 ;
			result = irserver_scan_c ( sopatts, sopatts+count,
				& count, instr );
			anyoutf ( 0, "irserver_scan returns %d", result );
			}
		pause_c( tofchar ( "Ready for processing/inspection" ));
		irserver_close_c ( & result );
		(void) userlog_c ( & proceed, & One, & One,
			tofchar("PROCEED="),
			tofchar(" Continue testing ? [Yes]")
			);
		}
	finis_c();
	return EXIT_SUCCESS ;
	}
#endif
