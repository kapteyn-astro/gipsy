/*
		     Copyright (c) 1991
		  Space Research Groningen
		    All Rights Reserved.
	P.O. Box 800, 9700 AV GRONINGEN, The Netherlands


#>            cfts_io.dc2

File:         cfts_oi.c

Purpose:      interface with a (compressed) FITS tape.

Category:     FITS

File:         cfts_io.c

Author:       Do Kester

Description:
          
The file contains the following functions:

	cftsd_geth	get header of (compressed) fits file 
				as ftsd_geth
	cftsi_getc	get compressed integers from a compressed FITS file
				no equivalent in ftsi_
	cftsi_geti	get integers form a (compressed) FITS file 
				as ftsi_geti
	cftsi_getr	get reals from a (compressed) FITS file
				as ftsi_getr
	cfts_skippix	skip pixels on a (compressd) FITS file
				as fts_skippix

Updates:	910911 DK.
		950622 DK, initialization of tp->cumul corrected	
#<
*/

#include	"ctype.h"
#include	"stdio.h"
#include	"stdlib.h"
#include	"string.h"
#include	"math.h"
/* #define 	NDEBUG 		1 */
#include	"assert.h"	/* toggle by (un)commenting the previous line */
#include	"gipsyc.h"

#include	"ftsd_geth.h"
#include	"ftsi_geti.h"
#include	"ftsi_getr.h"
#include	"fts_skippix.h"
#include	"ftsd_rint.h"
#include	"ftsd_rreal.h"
#include	"ftsd_wint.h"
#include	"ftsd_wreal.h"

#include	"nelc.h"
#include	"error.h"
#include	"anyout.h"
#include	"setfblank.h"
#include	"userfio.h"

#define	FOREVER		for ( ; ; )
#define	SERIOUS		3	/* serious error */
#define OUT_OF_MEMORY	-20	/* not enough memory to fill buffers */
#define CONVERSION	-22	/* conversion error */
#define	UNKNOWN_MTID	-30	/* unknown mtid */
#define BACKWARD	-31	/* negative read/skip request */
#define	MAXHEAD		46080	/* 16 blocks of 2880 characters */
#define	MAXTABLE	256	/* size of the difference table */
#ifndef	NULL			/* stupid */
#define	NULL		0	/* nullpointer */
#endif
#define CBLANK		0	/* blank  in the compressed fits file */
#define BITPIX		16	/* bitpix in the restored fits file */
#define BLANK		-32768	/* blank  in the restored fits file */
#define BSCALE		1.0 	/* bscale in the restored fits file */
#define BZERO		32768.0 /* bzero  in the restored fits file */
#define HEADLINE	80	/* length of one header line */
/* allocate memory to characters == char */
#define cmalloc( x, n )	( char* )(x) ? realloc( (x), (n) ) : malloc( n )
#define csize( n )	( (n) * sizeof( char ) )
#define callocmem( x, n )	( x = cmalloc( x, csize( n ) ) )
/* allocate memory to int integers == int */
#define lmalloc( x, n )	( int* )(x) ? realloc( (x), (n) ) : malloc( n )
#define lsize( n )	( (n) * sizeof( int ) )
#define lallocmem( x, n )	( x = lmalloc( x, lsize( n ) ) )
/* allocate memory to integers == fint */
#define imalloc( x, n )	( fint* )(x) ? realloc( (x), (n) ) : malloc( n )
#define isize( n )	( (n) * sizeof( fint ) )
#define newimem( x, n, o ) ( x = ( n > o ) ? imalloc( x, isize( n ) ) : x )
/* allocate memory to integer pointers == fint* */
#define pmalloc( x, n )	( fint** )(x) ? realloc( (x), (n) ) : malloc( n )
#define psize( n )	( (n) * sizeof( fint* ) )
#define newpmem( x, n, o ) ( x = ( n > o ) ? pmalloc( x, psize( n ) ) : x )

typedef struct	{
	fint	mt;		/* mtid of this "tape" */
	fint	initpos;	/* location of INITSTEP; 0 => not compressed */
	fint	initstep;	/* value of INITSTEP */
	fint	last_det;	/* last detector axis */
	fint	where;		/* byte location in the fits file */
	fint	cumul;		/* integrated differences */
	fint	naxis1;		/* length of the detector-snip */
	fint	naxis2;		/* number of detectors */
	fint	rate;		/* nr of samples per tick */
	fchar	chead;		/* header of compressed file */
} ttype ;
/* allocate memory to ttype  */
#define tmalloc( x, n )	( ttype* )(x) ? realloc( (x), (n) ) : malloc( n )
#define tsize( n )	( (n) * sizeof( ttype ) )
#define tallocmem( x, n )	( x = tmalloc( x, tsize( n ) ) )

static 	ttype	*tapes = NULL ;
static	fint	ntapes = 0 ;
static	int	*diftable = NULL	;
/*  static	char	mess[80]	;
    static	fint	meslev = 0 	;  */

static	fint	get_initval( fint, fint, ttype* )	;
static	ttype	*find_tape( fint ) ;
void	irshrinktab_c( fint* );

/*
@integer function cftsd_geth( integer, character, integer )
#>            cftsd_geth.dc2

Function:     CFTSD_GETH

Purpose:      Gets the header from a compressed FITS tape.

Category:     FITS

File:         cfts_io.c

Author:       Do Kester

Use:          INTEGER CFTSD_GETH( MTID,          Input          INTEGER
                                  HEADER,        Output         CHARACTER*(*)
                                  TID )          Input/Output   INTEGER

              CFTSD_GETH Returns:
                          >0 - number of characters transferred to HEADER
                               (i.e. ALL of the FITS header up to the
                               END record). The rest of HEADER is as
                               before the call.
                   -1 .. -10 - tape error (see MTIODEV.DC2)
                         -11 - not a FITS file.
                         -12 - found and skipped tape label
                         -13 - double tape mark found, tape skipped back to
                               before second tape mark.
                         -14 - keyword SIMPLE not present
                         -15 - SIMPLE=FALSE is not supported
                         -16 - keyword BITPIX not present
                         -17 - only BITPIX = 8, 16 and 32 supported.
                         -18 - keyword NAXIS not present
                         -19 - not enough NAXIS% keywords
                         -20 - could not get enough memory for internal ftsbuf.
                         -21 - the header array HEADER is too small for the
                               entire header.
                         -22 - conversion error
                         -23 - storage size of character type is not 1 byte
                         -24 - storage size of short type is not 2 bytes
                         -25 - storage size of int type is not 4 bytes
                         -29 - header contains illegal character 
              MTID      A device unit number as obtained from MTOPEN.
              HEADER    A string in which the header as read from unit
                        MTID will be stored as an array of strings.
              TID       Must be zero at input.
                        If output > zero, more bytes left to transfer
                        for FTSI_GETI or FTSI_GETR.

Description:
          
        CFTSD_GETH is functionally equivalent to FTSD_GETH, both for
        compressed as for normal fits files.

	CFTSD_GETH is used as an initialization routine for reading FITS data
	from a FITS tape.  The routine will try to read the file,
	interpreting the beginning of the file as a FITS header.  The data
	from tape will be stored in an (large) internal ftsbuf which will
	also be used in later calls of CFTSI_GETR and CFTSI_GETI. 

	 If the file consists of FITS blocks (i.e.  blocks of an integral
	number times 2880 bytes) CFTSD_GETH will try to read a number of FITS
	keywords nessecary for interpreting the FITS data: SIMPLE=, BITPIX=,
	NAXIS= and the NAXIS%= keywords.  If any of these is missing an error
	results.  Also CFTSD_GETH checks whether SIMPLE=T ( SIMPLE=F is not
	supported ), and whether the data can be read on the current machine
	given the value of BITPIX.    

	 Further error conditions are: the array HEADER provided by the
	caller is too small for the entire FITS header (-21), and not enough
	memory for internal databuffer (-20). 

	 CFTSD_GETH always assumes that the input 'tape' is positioned at the
	begining of a file, i.e.  immediately past an end of file mark.  If
	the first block read by CFTSD_GETH has length zero (i.e.  another end
	of file mark), the routine will skip back over that block and return
	with error -13; found double tape mark.  In FITS definition this
	means end of tape. 


Updates:
	07 Mar 1991 DK document creation
#<
*/

fint cftsd_geth_c( 
	fint	*mtid,          /* Input          */
        fchar	header,         /* Output         */
        fint	*tid )          /* Input/Output   */

{

	fint	chead_length, pos, ival	;
	int	i ;
	float	cdelt1, rval ;
	ttype	*tp	;

/* fill the difference table */
	if ( diftable == NULL ) {
		if ( ! lallocmem( diftable, MAXTABLE ) )
			return( OUT_OF_MEMORY ) ;
		irshrinktab_c( (fint *) diftable ) ;
	}

/* find the tape; if not create memory for it */
	if ( ! ( tp = find_tape( *mtid ) ) ) {
		if ( ! tallocmem( tapes, ntapes + 1 ) )
			return( OUT_OF_MEMORY ) ;
		tp = &tapes[ntapes] ;
		ntapes++ ;
		tp->mt = *mtid ;
		tp->chead.a = NULL ;
		if ( ! callocmem( tp->chead.a, MAXHEAD ) ) 
			return( OUT_OF_MEMORY ) ;
		tp->chead.l = MAXHEAD ;
	}

	chead_length = ftsd_geth_c( mtid, tp->chead, tid );
	if ( chead_length < 0 ) return( chead_length ) ;

/* copy up until INITSTEP to header; set the remainder to blanks */
	tp->initpos = ftsd_rint_c( tp->chead, tofchar( "INITSTEP" ), &tp->initstep );
	tp->initpos *= HEADLINE ;
	if ( tp->initpos < 0 ) {
		strncpy( header.a, tp->chead.a, chead_length ) ;
		tp->initpos = 0 ;
		return( chead_length ) ;
	}
	strncpy( header.a, tp->chead.a, tp->initpos ) ;
	for ( i = tp->initpos; i < header.l; i++ ) header.a[i] = ' ' ;

/* read the axes of the fits file */
	pos = 0 ;
	pos = ftsd_rint_c( tp->chead, tofchar( "NAXIS1" ), &tp->naxis1 ) ;
	assert( pos > 0 ) ;
	pos = 0 ;
	pos = ftsd_rint_c( tp->chead, tofchar( "NAXIS2" ), &tp->naxis2 ) ;
	assert( pos > 0 ) ;
	pos = 0 ;
	pos = ftsd_rreal_c( tp->chead, tofchar( "CDELT1" ), &cdelt1 ) ;
	tp->rate = floor( 0.1 + 1.0 / cdelt1 ) ;
	assert( pos > 0 ) ;

	tp->cumul = get_initval( 0, 0, tp ) ;

/* change some of the keywords back to the original */
	pos = 0 ; ival = BITPIX ;
	pos = ftsd_wint_c( header, tofchar( "BITPIX" ), &ival, 
		tofchar( "restored bitpix" ) ) ;
	assert( pos > 0 ) ;
	pos = 0 ; ival = BLANK ;
	pos = ftsd_wint_c( header, tofchar( "BLANK" ), &ival, 
		tofchar( "restored blank" ) ) ;
	assert( pos > 0 ) ;
	pos = 0 ; rval = BZERO ;
	pos = ftsd_wreal_c( header, tofchar( "BZERO" ), &rval,
		tofchar( "restored bzero" ) ) ;
	assert( pos > 0 ) ;
	pos = 0 ; rval = BSCALE;
	pos = ftsd_wreal_c( header, tofchar( "BSCALE" ), &rval,
		tofchar( "restored bscale" ) ) ;
	assert( pos > 0 ) ;

	tp->where = 0 ;
	tp->last_det = 0 ;
	return( tp->initpos ) ;
}

/*
@integer function cftsi_getc( integer, integer, integer, integer )
#>            cftsi_getc.dc2

Function:     CFTSI_GETC

Purpose:      Gets compressed integer data from a FITS tape.

Category:     FITS

File:         cfts_io.c

Author:       Do Kester

Use:          INTEGER CFTSI_GETC( MTID,          Input          INTEGER
                                  COMPS,         Output         INTEGER
                                  LENGTH,        Input          INTEGER
                                  TID )          Input/Output   INTEGER
              CFTSI_GETC Returns:
                          >0 - number of integers transferred to ARRAY.
                       =<-10 - tape error (see MTIODEV.DC2)
                         -11 - not a FITS file.
                         -12 - found and skipped tape label
                         -13 - double tape mark found, tape skipped back to
                               before second tape mark.
                         -20 - could not get enough memory for internal ftsbuf.
                         -22 - conversion error
                         -30 - Unknown mtid
                         -31 - Only forward skipping allowed.
              MTID      A device unit number as obtained from MTOPEN.
              COMPS     Target array for the data.
              LENGTH    Length of ARRAY
              TID       Tranfer ID from CFTSD_GETH.
                        If non zero on output more data is left on
                        tape, thus subsequent calls of CFTSI_GETI are
                        nessecary. TID=0 ends a series of calls.
                        TID=0 occurs at the end of file mark. After
                        TID=0 the tape is positioned befor end of file.

Description:
	The raw-raw values on the tape are returned i.e. the entries in
	the difference table. When the fits file is not compressed
	normal fits integers are returned.
Updates:
	07 Mar 1991 DK document creation
#<
*/

fint cftsi_getc_c( 
	fint	*mtid,
	fint	*comps,
	fint	*length,
	fint	*tid )

{
	fint	nread ;
	int	i, n ;
	ttype	*tp	;

	if ( ! *length ) return( 0 ) ;		/* 0 asked, 0 given */
	if ( *length < 0 ) return( BACKWARD ) ; /* no backward reading */
	if ( ! ( tp = find_tape( *mtid ) ) )
		return( UNKNOWN_MTID ) ;	/* cannot find tape */
	if ( ! tp->initpos ) 			/* not compressed */
		return( ftsi_geti_c( mtid, comps, length, tid ) ) ;

	nread = ftsi_geti_c( mtid, comps, length, tid ) ;
	if ( nread <= 0 ) return( nread ) ;

/* update the cumulative value; after finding initval */
	i = tp->where / tp->naxis1 ;
	n = ( tp->where + nread ) / tp->naxis1 ;
	if ( n > i ) {		/* new axis: integrate from INIT00nn */
		tp->cumul = get_initval( 0, n, tp ) ;
		n *= tp->naxis1 - tp->where ;
	} else n = 0 ;
	for ( i = n; i < nread; i++ ) tp->cumul += diftable[*comps++] ;
	tp->where += nread ;
	return( nread ) ;
}



/*
@integer function cftsi_geti( integer, integer, integer, integer )
#>            cftsi_geti.dc2

Function:     CFTSI_GETI

Purpose:      Gets integer data from a FITS tape.

Category:     FITS

File:         cfts_io.c

Author:       Do Kester

Use:          INTEGER CFTSI_GETI( MTID,          Input          INTEGER
                                  ARRAY,         Output         INTEGER
                                  ARRLEN,        Input          INTEGER
                                  TID )          Input/Output   INTEGER
              CFTSI_GETI Returns:
                          >0 - number of integers transferred to ARRAY.
                       =<-10 - tape error (see MTIODEV.DC2)
                         -11 - not a FITS file.
                         -12 - found and skipped tape label
                         -13 - double tape mark found, tape skipped back to
                               before second tape mark.
                         -20 - could not get enough memory for internal ftsbuf.
                         -22 - conversion error
                         -30 - Unknown mtid
                         -31 - Only forward skipping allowed.
              MTID      A device unit number as obtained from MTOPEN.
              ARRAY     Target array for the data.
              ARRLEN    Length of ARRAY
              TID       Tranfer ID from CFTSD_GETH.
                        If non zero on output more data is left on
                        tape, thus subsequent calls of CFTSI_GETI are
                        nessecary. TID=0 ends a series of calls.
                        TID=0 occurs at the end of file mark. After
                        TID=0 the tape is positioned befor end of file.

Description:

       CFTSI_GETI is functionally equivalent to FTSI_GETI, both for
       compressed as for normal fits files.


Updates:
	07 Mar 1991 DK document creation
#<
*/

fint cftsi_geti_c( 
	fint	*mtid,
	fint	*ints,
	fint	*length,
	fint	*tid )

{
	fint		nread, nrblank, npt, brate, lax, nrax, done, todo ;
	static fint	prevlen = 0, **pint = NULL, **ppint ;
	int		i ;
	ttype		*tp	;

	if ( ! *length ) return( 0 ) ;		/* 0 asked, 0 given */
	if ( *length < 0 ) return( BACKWARD ) ; /* no backward reading */
	if ( ! ( tp = find_tape( *mtid ) ) )
		return( UNKNOWN_MTID ) ;	/* cannot find tape */
	if ( ! tp->initpos ) 			/* not compressed */
		return( ftsi_geti_c( mtid, ints, length, tid ) ) ;

	done = tp->where - tp->last_det * tp->naxis1 ;
	todo = *length ;
	nread = 0 ;
	while ( todo ) {
		lax = done + todo ;	/* remainder on this axis */
		lax = ( lax > tp->naxis1 ) ? tp->naxis1 - done : todo ;
		npt = lax / tp->rate ;
		if ( ! newpmem( pint, npt, prevlen ) ) return( OUT_OF_MEMORY ) ;
		if ( npt > prevlen ) prevlen = npt ;
		ppint = pint ;
		nrax = ftsi_geti_c( mtid, ints, &lax, tid ) ;
		if ( nrax <= 0 ) return( nrax ) ;

		nrblank = brate = 0 ;
		for ( i = 0; i < nrax; i++ ) {
			tp->cumul += diftable[*ints] ;
			if ( *ints == CBLANK || brate ) {
				if ( ! brate ) {
					*ppint++ = ints ;
					nrblank++ ;
					brate = tp->rate - 1 ;
				} else  brate-- ;
			}
			*ints++ = tp->cumul ;
		}
		while ( nrblank-- ) { 
			ppint-- ;
			for ( i = 0 ; i < tp->rate ; i++ )
				*(*ppint+i) = BLANK ;
		}
		nread += nrax ;
		todo -= nrax ;
		if ( done + lax == tp->naxis1 ) {
			done = 0 ;  tp->last_det++ ;
			tp->cumul = get_initval( 0, tp->last_det, tp ) ;
		} else break ;
	}
	tp->where += nread ;

	return( nread ) ;
}




/*
@integer function cftsi_getr( integer, real, integer, integer )
#>            cftsi_getr.dc2

Function:     CFTSI_GETR

Purpose:      Gets float data from a FITS tape.

Category:     FITS

File:         cfts_io.c

Author:       Do Kester

Use:          INTEGER CFTSI_GETR( MTID,          Input          INTEGER
                                  ARRAY,         Output         REAL
                                  ARRLEN,        Input          INTEGER
                                  TID      )     Input/Output   INTEGER
                                 
              CFTSI_GETR Returns:
                          >0 - number of reals transferred to ARRAY.
                   -1 .. -10 - tape error (see MTIODEV.DC2)
                         -11 - not a FITS file.
                         -12 - found and skipped tape label
                         -13 - double tape mark found, tape skipped back to
                               before second tape mark.
                         -20 - could not get enough memory for internal ftsbuf.
                         -22 - conversion error.
                         -30 - Unknown mtid
                         -31 - Only forward skipping allowed.
              MTID      A device unit number as obtained from MTOPEN.
              ARRAY     Target array for the data.
              ARRLEN    Length of ARRAY
              TID       Tranfer ID from CFTSD_GETH.
                        If > zero on output more data is left on
                        tape, thus subsequent calls of CFTSI_GETR are
                        nessecary. TID=0 ends a series of calls.
                        TID=0 occurs at the end of file mark. After
                        TID=0 the tape is positioned before the end of file.
Description

       CFTSI_GETR is functionally equivalent to FTSI_GETR, both for
       compressed as for normal fits files.

Updates:
	07 Mar 1991 DK document creation
#<
*/

fint cftsi_getr_c( 
	fint	*mtid,
	float	*reals,
	fint	*length,
	fint	*tid )

{
	static fint	prevlen = 0, *ints = NULL, *pints	;
	fint		nread ;
	float	fblank, *preal	;
	int	i ;
	ttype	*tp	;

	if ( ! *length ) return( 0 ) ;		/* 0 asked, 0 given */
	if ( *length < 0 ) return( BACKWARD ) ; /* no backward reading */
	if ( ! ( tp = find_tape( *mtid ) ) )
		return( UNKNOWN_MTID ) ;		/* cannot find tape */
	if ( ! tp->initpos ) 			/* not compressed */
		return( ftsi_getr_c( mtid, reals, length, tid ) ) ;

	if ( ! newimem( ints, *length, prevlen ) ) return( OUT_OF_MEMORY ) ;
	if ( *length > prevlen ) prevlen = *length ;

	setfblank_c( &fblank ) ;
	nread = cftsi_geti_c( mtid, ints, length, tid ) ;
	if ( nread <= 0 ) return( nread ) ;

	pints = ints ; preal = reals ;
	for ( i = 0; i < nread ; i++ ) {
		if ( *pints == BLANK ) *preal = fblank ;
		else *preal = *pints * BSCALE + BZERO ;
		preal++ ; pints++ ;
	}

	return( nread ) ;
}




/*
@integer function cfts_skippix( integer, integer )
#>            cfts_skippix.dc2

Function:     CFTS_SKIPPIX

Purpose:      Skip pixels in a FITS file.

Category:     FITS

File:         cfts_io.c

Author:       Peter Roelfsema

Use:          integer CFTS_SKIPPIX ( MTID, 	input		INTEGER,
 				     NSKIP ) 	input/output	INTEGER.

              CFTS_SKIPPIX Returns:
                          >0 - number of pixels skipped on MTID.
                   -1 .. -10 - tape error (see MTIODEV.DC2)
                         -13 - end of tape found
			 -20 - out of memory
                         -30 - Unknown mtid
                         -31 - Only forward skipping allowed.
              MTID      A device unit number as obtained from MTOPEN.
              NSKIP     Number of pixels to skip.

Description:

       CFTS_SKIPPIX Skips nskip pixels in FITS files. This function is very
       usefull when only known small sections from FITS files have to be read.

       CFTS_SKIPPIX is functionally equivalent to FTS_SKIPPIX, both for
       compressed as for normal fits files.

Updates:
	07 Mar 1991 DK document creation
#<
*/

fint cfts_skippix_c(
	fint	*mtid,
	fint	*nskip )

{
	fint		tid, init, new, newdet, newinit, new1, skip ;
	static fint	prevlen = 0, *comps = NULL	;
	ttype		*tp	;

	if ( ! *nskip ) return( 0 ) ;		/* 0 asked, 0 given */
	if ( *nskip < 0 ) return( BACKWARD ) ; 	/* no backward reading */
	if ( ! ( tp = find_tape( *mtid ) ) )	/* cannot find tape */
		return( UNKNOWN_MTID ) ;
	if ( ! tp->initstep )			/* not compressed */
		return( fts_skippix_c( mtid, nskip ) ) ;

/* find positions after the requested skip */
	init = ( tp->where - tp->last_det * tp->naxis1 ) / tp->initstep ;
	new = tp->where + *nskip ;		/* new byte position */
	newdet = new / tp->naxis1 ;
	newinit = ( new - newdet * tp->naxis1 ) / tp->initstep ;

	if ( ( ( newdet == tp->last_det ) && ( newinit > init ) ) ||
	       ( newdet > tp->last_det ) ) {
/* find new fiducial point: goto INIT<newinit><newdet+1> */
		skip = newdet * tp->naxis1 + newinit * tp->initstep 
			- tp->where ;
		new = fts_skippix_c( mtid, &skip ) ;
		assert( new == skip ) ;
		tp->where += new ;
		tp->last_det = newdet ;
		tp->cumul = get_initval( newinit, newdet, tp );
		skip = *nskip - skip ;
	} else {
/* skip is inside the present initval */
		skip = *nskip ;
		new = 0 ;
	}

	if ( ! newimem( comps, skip, prevlen ) ) return( OUT_OF_MEMORY ) ;
	if ( skip > prevlen ) prevlen = skip ;
	tid = 1 ;
/* skip the remaining values accumulating after the initval */
	new1 = cftsi_getc_c( mtid, comps, &skip, &tid ) ;
	assert( new1 == skip ) ;

	return( new + new1 ) ;
}

/* get new initialization value */

static fint get_initval( fint newinit, fint newdet, ttype *tp )
{
	char	key[10] ;
	fint	pos, pos1, c ;

	if ( newdet >= tp->naxis2 ) return( 0 ) ;

	sprintf( key, "INIT%2.2d%2.2d", newinit, newdet + 1);
	pos = tp->initpos + HEADLINE * ( newdet * ( tp->naxis1 / 
		tp->initstep + 1 ) + newinit + 1 ) ;
	pos1 = ftsd_rint_c( tofchar( &tp->chead.a[pos] ), tofchar( key ), &c ) ;
	assert( pos1 == 0 ) ;		/* right at the pointer */

	return( c ) ;
}

static ttype*	find_tape( fint mtid )
{
	int	i;

	for ( i = 0 ; i < ntapes ; i++ )
		if ( mtid == tapes[i].mt ) return( &tapes[i] ) ;
	return( NULL ) ;
}


/*
@subroutine irshrinktab( integer )
#>            irshrinktab.dc2

Function:     IRSHRINKTAB

Purpose:      produce the difference table for compressed IRAS fits

Category:     IRAS, FITS

File:         cfts_io.c

Author:       Albrecht de Jonge

Use:          call IRSHRINKTAB( table )  output	INTEGER(256)

              table	the difference table

Description:
	The IRAS data on the HP/DOR are compressed in the same way as 
        when they were send down to the ground station. 
	The logarithmic difference table is returned by this routine.
	See IRAS Expl.Suppl. Page II-25.	       


Updates:
	15 Mar 1991 DK document creation
#<
*/

void irshrinktab_c ( fint *table )
{	
	
     	int 		hidden[16] = 	{ 8, 8, 8, 8, 8, 8, 8, 8,
						8, 8, 24, 16, 24, 16, 8, 0 },
			fact[16] =	{ 4096, 2048, 1024, 512, 256, 128, 64,
						32, 16, 8, 2, 2, 1, 1, 1, 1 };

	int		shiftcode, significand, k ;
	fint		diff ;

		
	k = 0 ;
	for ( shiftcode = 15 ; shiftcode >= 0 ; shiftcode -- ) 
		for ( significand = 0 ; significand < 8 ; significand++ ) {
			diff = hidden[shiftcode] + significand ;
			diff *= fact[shiftcode] ;
			table [ 128 + k ] = diff ;
			table [ 128 - k ] = - diff ;
			k = k + 1 ;
		}
	table[0] = 0 ;
	}

/*
	#include <stdio.h>
	void main(void)
	{	int tab[256] ;
		int i ;
		irshrinktab ( tab );
		for ( i = 0 ; i < 256 ; i ++ ) 
			printf ( "%4d %5d\n", i, tab[i] );
		}
*/

/*
bundle the .h files which are contained in this file
#>            cfts_io.h
#include "cftsd_geth.h"
#include "cftsi_getc.h"
#include "cftsi_geti.h"
#include "cftsi_getr.h"
#include "cfts_skippix.h"
#include "irshrinktab.h"
#<
*/

