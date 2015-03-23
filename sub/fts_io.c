/* fts_io.c

		  Copyright (c) Space Research Groningen 1990
			     All Rights Reserved.
		P.O. Box 800, 9700 AV Groningen, The Netherlands

#>	      fts_io.dc2

Document:     FTS_IO

Purpose:      Describes the FITS io routines

Category:     FITS

File:	      fts_io.c

Author:       P. Roelfsema

Description:  The fts_io routines read or write data from/to FITS tapes
	      and files.

	      The following functions are available:

	      FTSD_GETH 	Get a FITS header from tape.
	      FTSI_GETR 	Get real data from tape.
	      FTSI_GETI 	Get integer data from tape.

	      FTSD_PUTH 	Put a FITS header on tape.
	      FTSI_PUTR 	Put real data on tape.
	      FTSI_PUTI 	Put integer data on tape.

	      FTS_SKIPPIX	Skip pixels on a FITS tape.
	      FTS_SKIPFIL	Skips fits file(s).

	      Each routine is described in more detail in the
	      appropriate document.

Use:
	  To read a FITS tape first a tape-device must be opened using MTOPEN
	(see MTIODEV.DC2 and references therein).  From this device a FITS
	read operation is initialised by calling FTSD_GETH.  After FTSD_GETH
	the calling program has a character array containing the FITS header
	records.  Using the FTSD_FIND, FTSD_RCHAR, FTSD_RREAL etc.  routines
	the values of individual FITS keywords can be obtained from this
	header.  Subsequently a series of calls to FTSI_GETI or FTSI_GETR
	(combined with FTS_SKIPPIX calls) can be done to get the FITS data
	from the tape into an integer or real array respectively. A read
	operation can be stopped before the end of the tape-file by skipping
	to the next file with FTS_SKIPFIL.

	  After an error condition (i.e. a negative return value of one of the
	routines) the tape will always be positioned at the beginning of the
	next file.

	  A typical series of FTS calls for a FITS read operation would
	look as follows:

	  MTOPEN( )	      - open a tape-device
	  FTS_SKIPFIL( )      - skip some files

	  FTSD_GETH( )	      - get the header from tape
	  FTSD_RREAL( )     |
	  FTSD_RLOG( )	    |
	      . 	    | - decode the header
	      . 	    |
	  FTSD_RINT( )	    |

	  FTS_SKIPPIX( )      - skip some pixels

	  REPEAT	      - loop to
	     FTSI_GETR( )     -   read data
	  UNTIL( TID.EQ.0 )   -     until all is read in

	  MTCLOSE( )	      - close the tape-device

	  To write a FITS tape first a FITS header must be generated;
	such a header should be a long character type array which can be
	filled with FITS record using the FTSD_WCHAR etc. routines.
	Subsequently a tape-device must be opened using MTOPEN (see
	MTIODEV.DC2 and references therein). The FITS write operation is
	initialised by calling FTSD_PUTH which will store the above mentioned
	header on tape.  After FTSD_PUTH integer or real data can be written
	to the device mounted to mtid by calling the routines FTSI_PUTI or
	FTSI_PUTR. The writing of data ends when the FTSI_ routine is
	told to put 0 (zero) elements on tape, then two tapemarks
	(indicating EOF and EOT) will be written to tape.

	  After an error condition (negative return value) the file that
	was being written will be erased.  The tape will be positioned
	after the previous file on tape.

	  A typical series of FTS calls for a FITS write operation would
	look as follows:

	  MTOPEN( )	      - open a tape-device
	  REPEAT	      - skip some files until EOT
	  UNTIL( FTS_SKIPFIL( ) .EQ. -13 )

	  FTSD_WREAL( )     |
	  FTSD_WLOG( )	    |
	      . 	    | - fill the header
	      . 	    |
	  FTSD_WINT( )	    |
	  FTSD_PUTH( )	      - write the header to tape

	  REPEAT	      - loop to
	     FTSI_PUTR( )     -   write data
	  UNTIL( NPIX.EQ.0 )  -     until all is written
	  FTSI_PUTR( . 0 . )  - write 0 pixels => end of write

	  MTCLOSE( )	      - close the tape-device

Updates:
	Jun 27, 1990: PRR, Original document created
	Aug 22, 1990: SS,  Added write routines.
	Sep 12, 1990: SS,  Adjustments for new data structure.
	Oct  8, 1990: PRR, Major Rewrite.
	Nov 12, 1991: MV,  Casting of LONG_MIN, because of bug in
			   gcc compiler.
#<

*/


#include	"ctype.h"		/* <ctype.h> 			*/
#include	"limits.h"		/* <limits.h>			*/
#include	"stddef.h"		/* <stddef.h> 			*/
#include	"stdlib.h"		/* <stdlib.h> 			*/
#include	"stdio.h"		/* <stdio.h>  			*/
#include	"string.h"		/* <string.h> 			*/
#include	"gipsyc.h"		/* GIPSY definitions 		*/

#include	"mtread.h"		/* Mt functions used here:	*/
#include	"mtwrite.h"
#include	"mtweof.h"
#include	"mtbsr.h"
#include	"mtfsr.h"
#include	"mtbsf.h"
#include	"mtfsf.h"

#include	"ftsd_rlog.h"		/* Ftsd routines used here: 	*/
#include	"ftsd_rint.h"
#include	"ftsd_rreal.h"
#include	"ftsd_rdble.h"

#include	"setfblank.h"		/* HERMES routines used here:	*/
#include	"cnvrtf.h"
#include	"cnvrth.h"
#include	"nelc.h"
#include	"userfio.h"

#define NO_ERROR		  0	/* ERROR CODES: 		*/
#define MTIO_END_OF_TAPE	 -2
#define TAPE_IO_ERROR		-10
#define NO_FITS_FILE		-11
#define TAPE_LABEL		-12
#define END_OF_TAPE		-13
#define BEGIN_OF_TAPE		-13
#define NO_SIMPLE_KEY		-14
#define BAD_SIMPLE		-15
#define NO_BITPIX_KEY		-16
#define BAD_BITPIX		-17
#define NO_NAXIS_KEY		-18
#define NO_NAXISX_KEY		-19
#define NO_MEMORY		-20
#define BIG_HEADER		-21
#define CONVERSION_ERROR	-22
#define SIZEOF_CHAR_ERROR	-23
#define SIZEOF_SHORT_ERROR	-24
#define SIZEOF_FINT_ERROR	-25
#define BAD_BLOCKSIZE		-26
#define BAD_DATAMAX		-27
#define BAD_DATAMIN		-28
#define NO_END_FOUND		-29
#define UNKNOWN_MTID		-30
#define IMPOSSIBLE_SKIP 	-32
#define END_OF_FILE		-33

#define KEYLEN			 15	/* Length of value field FITS
					   character record.		*/
#define SYSTEM			  2	/* FITS is UNIX type byte order */
#define FTSRECLEN		 80	/* length of a FITS record	*/
#define FTSBLOCKLEN	       2880	/* length of a FITS block	*/
#define MINBUFFSIZE	      28800	/* Minimum size of data buffers */

#define SPACE			' '

#define DEFBZERO		0	/* Default BZERO		*/
#define DEFBSCALE		1	/* Default BSCALE		*/
#define DEFBLANK		0	/* Default BLANK		*/

#define IOT_REAL		1	/* For FTSI_ routines		*/
#define IOT_INT 		2


/*
   MtList is a global static array of Mt_Structs containing i/o information
   and buffers for every opened tape device.
   The Mt_Struct for a given tape device with id MTID is addressed as:

	Mt_Struct pointer : &MtList[ MTID ],

     MTID is the identifier as received from MTOPEN.  An Mt_Struct
   contains the data buffer and all information needed to read or write
   a file.  These structs are allocated when needed in the routines
   GETH or PUTH (every read or write operation has to begin with a
   call to one of these!) and will not be freed since they are rather
   small.  Nr_Of_Tapes contains the number of Mt_Structs currently in the
   array.

    The (big) data buffers are also allocated in the routines GETH or
   PUTH. These buffers are freed in at the end of a FITS transfer; i.e.
   when a device is skipped to another file (SKIPFIL) or when a PUTR or
   PUTI routine is told to write 0 elements to tape.  Thus, an Mt_Struct
   need not contain a data buffer at all times.
 */

#define MT_PUT			1	/* Tape status: write		*/
#define MT_GET			2	/* Tape status: read		*/
#define MT_EMPTY		3	/* Tape status: not used	*/

typedef struct {
	fint	id;		/* Tape id of associated tape.		*/
	int	status; 	/* To indicate input or output device.	*/
	int	nbytes; 	/* Number of bytes per word.		*/
	fint	bufsize;	/* Number of bytes in data buffer.	*/
	fint	nblocks;	/* Number of tape blocks in data buffer.*/
	fint	MtEOF;		/* End-of-file detector.		*/
	float	min, max;	/* Minimum and maximum values reals.	*/
	float	bzero, bscale;	/* BZERO and BSCALE for scaling 	*/
	fint	blank;		/* Value for undefined pixels.		*/
	unsigned char *buff;	/* Pointer to first elt. of data buffer.*/
	union {
	    unsigned char *cp;	/* Character pointer to data buffer.	*/
	    short	  *sp;	/* Short pointer to data buffer.	*/
	    fint	  *lp;	/* Long pointer to data buffer. 	*/
	} u;
} Mt_Struct;			/* Struct for tape unit buffer. 	*/

static	Mt_Struct	*MtList      = NULL;	/* List of tape units	*/
static	int		Nr_Of_Tapes  = 0;	/* Number of tape units */

#define MtBufLeft(tape) ( tape->buff + tape->bufsize - tape->u.cp )
#define MtBufUsed(tape) ( tape->u.cp - tape->buff )
#define MtBufFull(tape) ( tape->buff + tape->bufsize > tape->u.cp ? 0 : 1 )


/*
   Open_Tape( fint *mtid , int stat )

   Get pointer to element of MtList for the tape mounted to mtid.  If
   mtid =< current number of list elements (global variable Nr_Of_Tapes)
   one or more list elements are added to MtList so the new element can
   be adressed as: MtList[mtid].  That element is initialized (default
   values are set and data buffer pointer is set to NULL).
     If mtid < Nr_Of_Tapes, element MtList[mtid] will be reset, so an
   allocated data buffer might be lost here.
     Returned is the pointer to the element or NULL, when no memory could
   be allocated.
 */

static Mt_Struct *Open_Tape( fint *mtid , int stat )
{
    Mt_Struct	*tape = NULL;		/* Pointer to mt struct.	*/

    while ( !( *mtid < Nr_Of_Tapes ) ) {
	tape = realloc( MtList , sizeof( Mt_Struct ) * ++Nr_Of_Tapes ) ;
	if ( tape != NULL ) {
	    MtList	 = tape;
	    tape	 = &MtList[ Nr_Of_Tapes - 1 ] ;
	    tape->status = MT_EMPTY ;
	    tape->buff	 = NULL ;
	} else {
	    Nr_Of_Tapes--;			/* No new tape		*/
	    break;				/* No memory available	*/
	}
    }						/* End while.		*/

    if ( *mtid < Nr_Of_Tapes ) {		/* initialise mt_struct */
	tape	     = &MtList[ *mtid ] ;	/* Get pointer		*/
	if( tape->buff ) free( tape->buff ) ;	/* Free the buffer	*/
	tape->buff   = NULL ;			/* Buffer is empty	*/
	tape->id     = *mtid ;			/* Id is MTID from call */
	tape->status = stat ;			/* Tape status		*/
	tape->nbytes = 1 ;			/* Don't know this yet	*/
	tape->MtEOF  = 0 ;			/* Tape not at EOF	*/
    }
    return( tape ) ;				/* Return pointer.	*/
}						/* END Open_Tape	*/


/*
   Get_Tape( mtid , iotype )

   Try to find the tape unit associated with mtid in the MtList. If
   it is present return it, if not return NULL.
*/

static Mt_Struct *Get_Tape( fint *mtid , int iotype )
{
   Mt_Struct	*tape = NULL ;

   if ( *mtid < Nr_Of_Tapes ){			/* Is mtid known?	*/
      tape = &MtList[ *mtid ] ; 		/* Yes -> get it	*/
      if ( tape->status == iotype ){		/* Correct status?	*/
	 return( tape ) ;			/* Yes -> return tape	*/
      }
   }
   return( NULL ) ;				/* No -> return NULL	*/
}

/*
   Get_MtBufSize( Mt_Struct tape , int min_size )

   Determine the blocksize used on tape and set the data buffer size. The
   minimum data buffer size used will be parameter min_size.  If the
   blocksize used on tape is greater than min_size, the data buffer will
   contain exactly one tape or disk block.  Otherwise the data buffer
   size used will be:

     buffersize = ( ( min_size % blocksize ) == 0 ) ? min_size :
    (  ( ( min_size / blocksize ) + 1 ) * blocksize  ) ;

   The number of tape blocks used in one data buffer is set in tape->nblocks.
   Values returned:
	  0 : Routine ended successfully,
     -1 .. -10 : Some error detected by mt_read,
	-11 : File is not a FITS file,
	-12 : File is a tape label.
 */

static fint Get_MtBufSize( Mt_Struct *tape , fint min_size )
{
    fint	fdat ;			/* Memory for data .		*/
    fint	nore = 1;		/* Number of integers return.	*/
    fint	nbsf = 1;		/* Skip one record back.	*/
    fint	retv ;			/* Return value from mtread.	*/

    if ( ( retv = mtread_c( &tape->id , &fdat , &nore ) ) > 0 ) {
	if ( ( retv%FTSBLOCKLEN ) == 0 ){	/* Multiple FITS blocks */
	    if ( retv < min_size ) {
		tape->bufsize = ( ( min_size%retv ) == 0 ) ? min_size :
			( ( ( min_size / retv ) + 1 ) * retv ) ;
	    } else {
		tape->bufsize = retv;
	    }
	    tape->nblocks = tape->bufsize / retv;
						/* Skip record back	*/
	    (void) mtbsr_c( &tape->id , &nbsf ) ;
	} else {				/* NOT FITS blocks!	*/
	    if ( retv == 80 ) { 		/* Tape label.		*/
	       return( TAPE_LABEL ) ;
	    } else {
	       return( NO_FITS_FILE ) ; 	/* Unknown file size.	*/
	    }
	}
    } else {
	return( retv ) ;			/* Mtread error 	*/
    }
    return( NO_ERROR ) ;
}						/* END Get_MtBufSize	 */

/*
   Head2MtBuf( fchar header , Mt_Struct *tape )

   Copy header from fp to data buffer.
     All characters from fchar fp are copied to the databuffer of mt
   struct tape.  After the last record an END record is added.	Then the
   data buffer is filled with spaces till the next 2880 boundary.
     If the header fp contains more characters then the buffersize as
   defined in tape->bufsize, the full block(s) will be written to the
   tape mounted to mtid.
     Returned is the number of characters read from fp plus 80 characters
   (the END record) plus the number of spaces till the next 2880
   boundary or some error as detected by mtwrite_c.
*/
static fint Head2MtBuf( fchar header , Mt_Struct *tape )
{
    int 	cnt;			/* Nr of bytes read from header */
    int 	n;			/* Counter			*/
    int 	wrcnt = 0 ;		/* Nr of bytes written to tape	*/
    fint	nret;			/* Return value of mt* routines */
    int 	no_END_yet = 1 ;	/* END has not been written yet */
    char	*hedptr;		/* Used as pointer in array.	*/

    tape->u.cp = tape->buff;			/* Reset data buffer	*/
    hedptr     = header.a;			/* First element header */
    cnt        = ( nelc_c( header ) / FTSRECLEN + 1 ) * FTSRECLEN ;
    while ( cnt > 0 ) { 			/* Loop on header bytes */
       wrcnt	   = cnt < tape->bufsize ? cnt : tape->bufsize ;
       (void) memcpy( tape->u.cp , hedptr , cnt ) ;
       cnt	  -= wrcnt ;			/* What is left 	*/
       tape->u.cp += wrcnt ;			/* Increase pointer	*/
       if ( MtBufLeft(tape) > FTSRECLEN ) {	/* Space left for END ? */
	  memcpy( tape->u.cp , "END" , 3 ) ;	/* Yes => add it 	*/
	  tape->u.cp += 3 ;			/* Adjust buff pointer	*/
	  for ( n = 4 ; n <= FTSRECLEN ; n++ ) {
	     *tape->u.cp++ = SPACE ;		/* Add rec. with spaces */
	  }
	  wrcnt      += FTSRECLEN ;		/* One more rec. written*/
	  no_END_yet  = 0 ;			/* END is on tape now	*/
       }
       if ( MtBufFull(tape) ) { 		/* Buffer full ?	*/
	  if ( ( nret = mtwrite_c( &tape->id ,	/* Yes => flush to tape */
			 (fint *) tape->buff , &tape->bufsize ) ) > 0 ) {
	     tape->u.cp = tape->buff ;		/* Reset buffer 	*/
	  } else {				/* Tape write error	*/
	     return( nret ) ;			/* Return to caller	*/
	  }
       }
    }
    if ( no_END_yet ) { 			/* END not on tape yet	*/
       memcpy( tape->u.cp , "END" , 3 ) ;	/* Add it to buffer	*/
       tape->u.cp   += 3 ;			/* Adjust buff pointer	*/
       wrcnt	    += 3 ;
    }
    while ( MtBufUsed(tape) % FTSBLOCKLEN != 0 ){/* Pad with spaces	*/
       *tape->u.cp++  = SPACE ;
       wrcnt	    += 1 ;
    }
    return( wrcnt ) ;
}						/* END Head2MtBuf.	*/

/*
   Data2MtBuf( Mt_Struct *tape )

   Data2MtBuf reads the number of tape blocks as defined in
   tape->nblocks from tape to the data buffer of tape and sets the
   length of the buffer to the number of integers read.
     The integers are converted as defined by cnvrth_c for the current
   machine.  If EOF encountered, tape->MtEOF is set to 1.  Returned
   is the number of bytes read or some error as received from mtread_c.
     NOTE: The data buffer may contain some bytes when EOF is set!
 */
static fint Data2MtBuf( Mt_Struct *tape )
{
   fint 	nret	 = 0 ;		/* Value returned by mtread.	*/
   fint 	nitems	 = 0 ;		/* # of integers read.		*/
   fint 	system	 = SYSTEM ;
   fint 	nblocks  = 0 ;		/* Number of blocks read.	*/
   fint 	blksize  = 0 ;		/* Block size			*/
   fint 	one	 = 1 ;

    tape->u.cp = tape->buff;			/* Reset buffer.	*/
    blksize    = tape->bufsize / tape->nblocks; /* Set blocksize	*/
						/* Fill the data buffer */
    while ( ( nblocks++ < tape->nblocks ) && !( tape->MtEOF ) ) {
	nret = mtread_c( &tape->id , (fint *) ( tape->u.cp ) , &blksize ) ;
	if ( ( nret == MTIO_END_OF_TAPE ) || ( nret == 0 ) ) {
	   tape->MtEOF = 1 ;			/* EndOfFile detected	*/
	   (void) mtbsf_c( &tape->id , &one ) ; /* Skip to before EOF	*/
	   if ( MtBufUsed(tape) > 0 ) { 	/* Buffer NOT empty	*/
	      break ;				/* Exit read loop	*/
	   } else {				/* Buffer is empty	*/
	      return( END_OF_FILE ) ;		/* Tell caller EOF	*/
	   }
	}
	if ( nret < 0 ) {			/* Tape read error	*/
	   return( nret ) ;
	} else if ( ( ( nret % FTSBLOCKLEN ) != 0 ) ||
		      ( nret > blksize )  ) {	/* Wrong tape-block	*/
	    return( NO_FITS_FILE ) ;		/* Return to caller	*/
	}
	tape->u.cp += nret ;			/* Increment pointer	*/
    }

    tape->bufsize = MtBufUsed(tape) ;		/* Get # of bytes in buf*/
    tape->nblocks = nblocks - 1 ;		/* Get # of blocks	*/
    tape->u.cp	  = tape->buff ;		/* Reset pointer	*/

    nitems = tape->bufsize / tape->nbytes;	/* swap bytes		*/
    if ( tape->nbytes == 2 ) {			/* shorts => byteswap ? */
	 cnvrth_c( &system , tape->u.lp , tape->u.lp , &nitems ) ;
    } else if ( tape->nbytes == 4 ) {		/* ints => byteswap ?	*/
	 cnvrtf_c( &system , tape->u.lp , tape->u.lp , &nitems ) ;
    }

    return( tape->bufsize ) ;
}						/* END Data2MtBuf	*/

/*
   Check_Header( fchar header , Mt_Struct *tape )

   Initializes read/write proces. Checks the obligatory records,
   decodes the values and copies them to the mt struct tape.
    Values returned:
	 0 : routine ended successfully
	-14: NO_SIMPLE_KEY		-16: NO_BITPIX_KEY
	-17: BAD_BITPIX 		-18: NO_NAXIS_KEY
	-19: NO_NAXISX_KEY		-23: SIZEOF_CHAR_ERROR
	-24: SIZEOF_SHORT_ERROR 	-25: SIZEOF_FINT_ERROR
*/
static fint Check_Header( fchar header , Mt_Struct *tape )
{
      char	keybuf[KEYLEN + 1];	/* For NAXISx keys.		*/
      fint	bitpix; 		/* Only localy used.		*/
      bool	simple; 		/* Only localy used.		*/
      fint	cnt;			/* Counter.			*/
      fint	rdint, naxis;		/* Dummies for naxis% checking. */

      if ( ftsd_rlog_c( header , tofchar( "SIMPLE" ) , &simple ) < 0 ) {
	 return( NO_SIMPLE_KEY ) ;
      } else if ( simple == FALSE ) {		/* Not supported!		*/
	 return( BAD_SIMPLE ) ;
      }
						/* # of bytes per word	*/
      if ( (cnt=ftsd_rint_c( header , tofchar( "BITPIX" ) , &bitpix )) < 0 ) {
	 return( NO_BITPIX_KEY ) ;
      }
      tape->nbytes = bitpix / CHAR_BIT;
						/* check storage sizes	*/
      if ( (tape->nbytes!=1) && (tape->nbytes!=2) && (tape->nbytes!=4) ) {
	 return( BAD_BITPIX ) ;
      } else if ( ( tape->nbytes == 1 ) && sizeof( char )  != 1 ) {
	 return( SIZEOF_CHAR_ERROR ) ;
      } else if ( ( tape->nbytes == 2 ) && sizeof( short ) != 2 ) {
	 return( SIZEOF_SHORT_ERROR ) ;
      } else if ( ( tape->nbytes == 4 ) && sizeof( fint )  != 4 ) {
	 return( SIZEOF_FINT_ERROR ) ;
      }
						/* check naxis% keys	*/
      if ( ftsd_rint_c( header , tofchar( "NAXIS" )  , &naxis  ) < 0 ) {
	 return( NO_NAXIS_KEY ) ;
      }
      for ( cnt = 1 ; cnt <= naxis ; cnt++ ){
	 sprintf( keybuf , "NAXIS%d" , cnt ) ;
	 if ( ftsd_rint_c( header , tofchar( keybuf ) , &rdint ) < 0 ) {
	    return( NO_NAXISX_KEY ) ;
	 }
      }

      if ( ftsd_rint_c(  header , tofchar( "BLANK"  ) , &tape->blank) < 0)
	  tape->blank = DEFBLANK;


      /* The items BSCALE and BZERO can be stored as doubles.  */
      /* Try to read them first as doubles and if a conversion */
      /* error occurs, read them as floats.		       */

      {
	 static double	 Dval;
	 static fint	 err;

	 err = ftsd_rdble_c( header , tofchar( "BZERO"  ) , &Dval );
	 if (err >= 0) {
	    tape->bzero = (float) Dval;
	 }
	 else {
	    if (err == -3) {
	       /* Conversion error */
	       err = ftsd_rreal_c( header , tofchar( "BZERO"  ) , &tape->bzero);
	       if ( (err < 0) && (err != -3) ) tape->bzero = DEFBZERO;
	    }
	 }

	 err = ftsd_rdble_c( header , tofchar( "BSCALE"  ) , &Dval );
	 if (err >= 0) {
	    tape->bscale = (float) Dval;
	 }
	 else {
	    if (err == -3) {
	       /* Conversion error */
	       err = ftsd_rreal_c( header , tofchar( "BSCALE"  ), &tape->bscale);
	       if  ( (err < 0) && (err != -3) ) tape->bscale = DEFBSCALE;
	    }
	 }
      }
      return ( NO_ERROR ) ;
}						/* END Check_Header.	*/

/*
   Get_MinMax( Mt_Struct *tape , fchar hd)

   Get minimum and maximum data values in the file and store them in
   tape->min resp.  tape->max.
     At first the minimum and maximum integer values as defined in
   limits.h for the type as given by nbytes are scaled to the reals as
   defined by tape->BSCALE and tape->BZERO.  Then the FITS descriptors
   DATAMIN and DATAMAX are searched in the header hd.  If not found the
   values found as described above are used as resp.  tape->min and
   tape->max.
     In case the header contains a DATAMIN and/or a DATAMAX key(s),
   the value(s) as given in the header are compaired to those already
   found.  If the DATAMIN value is smaller than the datamin calculated
   or DATAMAX is greater than the datamax calculated, BAD_DATAMIN resp.
   BAD_DATAMAX is returned.  Otherwise DATAMIN and/or  DATAMAX are used.
     Returns:
       0 : routine defined minimum and maximum values,
     -27 : Bad DATAMAX value,
     -28 : Bad DATAMIN value.
*/
static fint Get_MinMax( Mt_Struct *tape , fchar header )
{					/* To indicate keys found:	*/
    float	hd_max, hd_min; 	/* Temp. var's for values read.	*/
    float	minval, maxval;

    minval = (float)(int)LONG_MIN;
    maxval = (float)(int)LONG_MAX;
    switch ( tape->nbytes ) {			/* Machine maxima:	*/
    case 1 :  tape->min = ( 0 * tape->bscale ) + tape->bzero;
	      tape->max = ( UCHAR_MAX * tape->bscale ) + tape->bzero;
	      break;
    case 2 :  tape->min = ( SHRT_MIN * tape->bscale ) + tape->bzero;
	      tape->max = ( SHRT_MAX * tape->bscale ) + tape->bzero;
	      break;
    case 4 :  tape->min = ( minval * tape->bscale ) + tape->bzero;
	      tape->max = ( maxval * tape->bscale ) + tape->bzero;
	      break;
    }
						/* Find keys in header	*/
    if ( ftsd_rreal_c( header , tofchar( "DATAMAX" ) , &hd_max ) > 0 ) {
	if ( hd_max > tape->max ) {
	    return( BAD_DATAMAX ) ;		/* Bad value in header	*/
	} else {
	    tape->max= hd_max;
	}
    }
    if ( ftsd_rreal_c( header , tofchar( "DATAMIN" ) , &hd_min ) > 0 ) {
	if ( hd_min < tape->min )  {
	    return( BAD_DATAMIN ) ;		/* Bad value in header	*/
	} else {
	    tape->min = hd_min;
	}
    }
    return ( NO_ERROR ) ;
}						/* END Get_MinMax.	*/


/*
   Handle_Error( Mt_Struct mp, fint err )

   Position tape, release data buffer and set status to MT_EMPTY.  The
   struct pointer mp should not be NULL, that will cause horrible things
   come true.
 */
static fint Handle_Error( Mt_Struct *tape , fint err )
{
    fint one = 1 ;

    if ( err == MTIO_END_OF_TAPE ) {		/* mtio detected EOT	*/
       err = END_OF_TAPE ;			/* convert to FTS EOT	*/
    }
    if ( tape->status == MT_PUT ) {
	(void) mtbsf_c(  &tape->id , &one ) ;	/* Skip back one file.	*/
	(void) mtweof_c( &tape->id , &one ) ;	/* Write one file mark. */
	(void) mtweof_c( &tape->id , &one ) ;	/* Write one file mark. */
	(void) mtbsf_c(  &tape->id , &one ) ;	/* Skip back file mark. */
    } else {
	(void) mtfsf_c(  &tape->id , &one ) ;	/* Skip forward file.	*/
    }
    if ( tape->buff != NULL )
	free( tape->buff ) ;			/* Free data buffer.	*/
    tape->status = MT_EMPTY;
    return( err ) ;
}						/* END Handle_Error	*/

/*
   Fts_Get( *mtid , *data , *ndata , *tid , iotype )

   Read integer or real data from tape mounted to *mtid.  Returned is one of
   the values as described in ftsi_geti and ftsi_getr.
 */
static fint Ftsi_Get( fint *mtid , void *vd_arr , fint *ndata , fint *morep ,
		      int iotype )
{
   Mt_Struct	*tape;			/* Pointer to mt struct.	*/
   fint 	*int_out = NULL;	/* Pointer to integer out array.*/
   float	*flt_out = NULL;	/* Pointer to float out array.	*/
   fint 	n	  = 0;		/* Number of floats in array.	*/
   fint 	ret	  = 0;		/* Return value.		*/
   fint 	value	  = 0;		/* Integer read from tape.	*/

    if ( !( tape = Get_Tape( mtid , MT_GET ) ) ){
	return( UNKNOWN_MTID ) ;		/* No tape -> exit	*/
    }

    if ( *ndata == 0 ) return( 0 ) ;		/* Want none -> do none */

    if ( *ndata < 0 )				/* read back impossible */
       return( Handle_Error( tape , IMPOSSIBLE_SKIP ) ) ;

    if ( iotype == IOT_REAL ) { 		/* Get pointer to array */
       flt_out = (float *) vd_arr;
    } else {
       int_out = (fint *) vd_arr;
    }
						/* fill user array	*/
    do {
	if ( MtBufFull(tape) ) {		/* Buffer full? Read	*/
	    if ( ( ret = Data2MtBuf( tape ) ) < 0 ) {
		*morep = 0;			/* Set output parameter */
		if ( ( ( ret == END_OF_TAPE ) ||
		       ( ret == END_OF_FILE )	 ) &&
			       ( n != 0 )	      ) {
		   break ;
		} else {
		   return( Handle_Error( tape , ret ) ) ;
		}
	    } else if ( ret == 0 ) {
		*morep = 0;			/* Set output parameter */
		break;				/* No new bytes read!	*/
	    }
	}
	switch ( tape->nbytes ) {
	    case 1  : value = (fint) *tape->u.cp++;  break;
	    case 2  : value = (fint) *tape->u.sp++;  break;
	    case 4  : value = (fint) *tape->u.lp++;  break;
	}
	if ( iotype == IOT_REAL ) {
	    if ( value != tape->blank ) {	/* Put scaled value	*/
		flt_out[n++] = ( (float) value ) * tape->bscale + tape->bzero;
	    } else {
		setfblank_c( &flt_out[n++] ) ;
	    }
	} else
	    int_out[n++] = value;
	*morep = !( tape->MtEOF && MtBufFull(tape) ) ;
    } while ( ( n < *ndata ) && ( *morep ) ) ;
    return( n  ) ;				/* Nr of floats read	*/
}						/* END Ftsi_Get.	*/

/*
   Ftsi_Put( *mtid , *array , *arrlen , io_type )

   Routine to write real or integer data (depending on io_type) to tape.
   Returned is one of the values as described in ftsi_putr and ftsi_puti.
 */
static fint Ftsi_Put( fint *mtid, void *array, fint *arrlen, int io_type )
{
    Mt_Struct	*tape = NULL ;		/* Pointer to mt struct.	*/
    fint	n     = 0 ;		/* Counter			*/
    float	fltval ;		/* Float var. for buffer values */
    float	*rp   = NULL ;		/* Pointer in float_array.	*/
    fint	*ip   = NULL ;		/* Pointer in integer_array.	*/
    fint	nret ;			/* Return value mtwrite.	*/
    fint	nbyt ;			/* Number of pixels to write	*/
    fint	sfli  = 0 ;		/* Scaled value of real 	*/
    fint	one   = 1 ;
    fint	two   = 2 ;
    float	sysblank ;		/* system blank value		*/

    if ( !( tape = Get_Tape( mtid , MT_PUT ) ) ){
	return( UNKNOWN_MTID ) ;		/* No tape -> exit	*/
    }

    if ( *arrlen == 0 ){			/* End of write 	*/
       if ( MtBufUsed(tape) > 0 ) {		/* Something is left	*/
	  while( MtBufUsed(tape) % FTSBLOCKLEN != 0 ) {
	     *tape->u.cp++ = 0 ;		/* Padd with zeros	*/
	  }
	  nbyt = MtBufUsed(tape) ;		/* Whats left to write? */
	  if ( ( nret = mtwrite_c( &tape->id , (fint *) tape->buff ,
				   &nbyt ) ) < 0 ) {	/* Write it	*/
	     return( Handle_Error( tape , nret ) ) ;	/* Write error	*/
	  }
	  if ( ( nret = mtweof_c( &tape->id , &two ) ) < 0 ) {
	     return( Handle_Error( tape , nret ) ) ;	/* Bad tapemark */
	  } else {
	     (void) mtbsf_c( &tape->id , &one ) ;	/* Tapemark OK	*/
	  }
	  free( tape->buff );			/* Empty tape buffers	*/
	  tape->buff = NULL ;
	  tape->bufsize = 0 ;
	  tape->status = MT_EMPTY ;		/* Tape is not in use	*/
       }
       return( *arrlen ) ;			/* Ready -> exit	*/
    }


    setfblank_c( &sysblank ) ;			/* determine blank	*/
    if ( io_type == IOT_REAL ) {		/* get begin of array	*/
	rp = (float *) array;
    } else {
	ip = (fint *) array;
    }

    while ( n++ < *arrlen ) {
	if ( MtBufFull(tape) ) {		/* Write buffer if full */
	    if ( ( nret = mtwrite_c( &tape->id , (fint *) tape->buff ,
					     &tape->bufsize ) ) < 0 ) {
		return ( Handle_Error( tape , nret ) ) ;
	    } else {
		tape->u.cp = tape->buff;	/* Reset Data Buffer	*/
	    }
	}
						/* get next value	*/
	if ( io_type == IOT_REAL ) {
	    fltval  = *rp++;			/* Real type value	*/
	    if ( ( fltval != sysblank ) &&
		 ( fltval  >= tape->min   ) && ( fltval <= tape->max ) ) {
		sfli = (fint) ( ( fltval - tape->bzero ) / tape->bscale ) ;
	    } else {
		sfli = tape->blank;		/* Value out of range	*/
	    }
	} else {				/* Integer type value	*/
	    sfli = *ip++;
	    fltval  = ( (float) sfli ) * tape->bscale + tape->bzero ;
	    if ( ( fltval  < tape->min	 ) || ( fltval > tape->max ) )
		       sfli = tape->blank ;		/* Value out of range	*/
	    }
	switch ( tape->nbytes ) {		/* Put value in buffer	*/
	    case 1 : *tape->u.cp++ = (unsigned char) sfli;	break;
	    case 2 : *tape->u.sp++ = (short) sfli;	break;
	    case 4 : *tape->u.lp++ = (fint) sfli;	break;
	}
    }						/* End while ( )	*/
    return( --n ) ;				/* Return # reals written */
}						/* END Ftsi_Put 	*/

/*
#>	      ftsd_geth.dc2

Function:     FTSD_GETH

Purpose:      Gets the header from a FITS tape.

Category:     FITS

File:	      fts_io.c

Author:       P. Roelfsema

Use:	      INTEGER FTSD_GETH( MTID,		Input	       INTEGER
				 HEADER,	Output	       CHARACTER*(*)
				 TID )		Input/Output   INTEGER

	      FTSD_GETH Returns:
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
			 -25 - storage size of long type is not 4 bytes
			 -29 - header contains illegal character
	      MTID	A device unit number as obtained from MTOPEN.
	      HEADER	A string in which the header as read from unit
			MTID will be stored as an array of strings.
	      TID	Must be zero at input.
			If output > zero, more bytes left to transfer
			for FTSI_GETI or FTSI_GETR.

Description:

	FTSD_GETH is used as an initialization routine for reading FITS data
	from a FITS tape.  The routine will try to read the file,
	interpreting the beginning of the file as a FITS header.  The data
	from tape will be stored in an (large) internal ftsbuf which will
	also be used in later calls of FTSI_GETR and FTSI_GETI.

	 If the file consists of FITS blocks (i.e.  blocks of an integral
	number times 2880 bytes) FTSD_GETH will try to read a number of FITS
	keywords nessecary for interpreting the FITS data: SIMPLE=, BITPIX=,
	NAXIS= and the NAXIS%= keywords.  If any of these is missing an error
	results.  Also FTSD_GETH checks whether SIMPLE=T ( SIMPLE=F is not
	supported ), and whether the data can be read on the current machine
	given the value of BITPIX.

	 Further error conditions are: the array HEADER provided by the
	caller is too small for the entire FITS header (-21), and not enough
	memory for internal databuffer (-20).

	 FTSD_GETH always assumes that the input 'tape' is positioned at the
	begining of a file, i.e.  immediately past an end of file mark.  If
	the first block read by FTSD_GETH has length zero (i.e.  another end
	of file mark), the routine will skip back over that block and return
	with error -13; found double tape mark.  In FITS definition this
	means end of tape.

Updates:
	Jun 27, 1990, PRR, Document created.
	Sep 12, 1990: SS,  Adjustments for new data structure.
	Dec 16, 1993: PRR, Allow non-printable characters in header
			   (are replaced by a blank space).
	Apr 28, 2003: DK,  Byteswap the proper amount when needed.
#<

Fortran to C interface:

@ integer function ftsd_geth(  integer , character , integer )

*/
fint ftsd_geth_c( fint *mtid , fchar header , fint *tid )
{
   Mt_Struct	*tape;			/* Pointer to mt struct.	*/
   fint 	system	  = SYSTEM;
   fint 	end_loc ;		/* Location of END		*/
   fint 	nitems	  = 0;
   fint 	nret;			/* Return value.		*/
   fchar	hedbuf ;
   int		cnt	  = 0;		/* Counter.			*/

    *tid = 0 ;					/* Should always be 0	*/

    if ( ( tape = Open_Tape( mtid , MT_GET ) ) == NULL ) {
	return( NO_MEMORY ) ;			/* No memory for mt	*/
    }

    if ( header.l > MINBUFFSIZE ) {
	nret = Get_MtBufSize( tape , header.l ) ;
    } else {
	nret = Get_MtBufSize( tape , (fint ) MINBUFFSIZE ) ;
    }
    if ( nret < 0 ) {				/* Something went wrong */
	return( Handle_Error( tape , nret ) ) ;
    }
						/* get memory for buffer*/
    if ( ( tape->buff = malloc( sizeof( char) * tape->bufsize ) ) == NULL ) {
	return( Handle_Error( tape , NO_MEMORY ) ) ;
    }
    if ( ( nret = Data2MtBuf(tape) ) < 0 ) {	/* Error filling buffer */
	if ( nret == END_OF_FILE ) {
	   nret = END_OF_TAPE ;
	}
	return( Handle_Error( tape , nret ) ) ;
    }
						/* look for 'END'  	*/
    end_loc = -1 ;
    for ( cnt = 0 ; cnt < header.l ; cnt += FTSRECLEN ) {
       if ( !strncmp( (char *)&tape->buff[ cnt ] , "END" , 3 ) ) {
	  end_loc = cnt ;			/* END found !		*/
	  break ;				/* exit search loop	*/
       }
    }
    if ( end_loc < 0 ) {			/* END was not found	*/
       return( Handle_Error( tape , BIG_HEADER ) ) ;
    }

    for ( cnt = 0 ; cnt < end_loc ; cnt++ ) {
       if ( !isprint( tape->buff[ cnt ] ) ) {	/* Printable character? */
	   tape->buff[ cnt ] = ' ' ;		/* if not, set to blank	*/
       }
    }

    hedbuf.a = (char *) tape->buff;		/* Copy header to buf	*/
    hedbuf.l = end_loc ;
    if ( ( nret = Check_Header( hedbuf , tape ) ) )  {	/* Check header */
	return( Handle_Error( tape , nret ) ) ; /* Header not OK	*/
    }
						/* Header OK		*/
    (void) memmove( header.a , tape->buff , end_loc ) ;

						/* Pad header		*/
    for ( cnt = end_loc ; cnt < header.l ; header.a[cnt++] = SPACE ) ;

						/* Position first datum */
    tape->u.cp = ( tape->buff + end_loc + FTSRECLEN ) ;

    while ( ( tape->u.cp - tape->buff ) % FTSBLOCKLEN  ) {
	tape->u.cp++;				/* Skip to FTS block	*/
    }

    if ( MtBufLeft(tape) > 0 ) {		/* Swap bytes?		*/
/*	 nitems = (int) tape->u.cp / tape->nbytes;	    replaced DK */
	 nitems = (int) MtBufLeft(tape) / tape->nbytes;
	 if ( tape->nbytes == 2 ) {		/* Shorts => byteswap	*/
	    cnvrth_c( &system , tape->u.lp , tape->u.lp , &nitems ) ;
	 } else if ( tape->nbytes == 4 ) {	/* Integers => byteswap */
	    cnvrtf_c( &system , tape->u.lp , tape->u.lp , &nitems ) ;
	 }
    }

    *tid = ( MtBufLeft(tape) > 0 )  ||
	    !tape->MtEOF ;			/* Set output parameter */
    return( end_loc ) ; 			/* Return header size	*/
}						/* END fint ftsd_geth_c */

/*
#>	      ftsi_getr.dc2

Function:     FTSI_GETR

Purpose:      Gets float data from a FITS tape.

Category:     FITS

File:	      fts_io.c

Author:       P. Roelfsema

Use:	      INTEGER FTSI_GETR( MTID,		Input	       INTEGER
				 ARRAY, 	Output	       REAL
				 ARRLEN,	Input	       INTEGER
				 TID	  )	Input/Output   INTEGER

	      FTSI_GETR Returns:
			  >0 - number of reals transferred to ARRAY.
		   -1 .. -10 - tape error (see MTIODEV.DC2)
			 -11 - not a FITS file.
			 -12 - found and skipped tape label
			 -13 - double tape mark found, tape skipped back to
			       before second tape mark.
			 -20 - could not get enough memory for internal ftsbuf.
			 -22 - conversion error.
			 -30 - tried to read from a tape (MTID) which is
			       unknown to the FTS* routines.
			       (occurs e.g. when FTSI_GETR is called
			       before FTSD_GETH)
			 -31 - cannot read backwards ( ARRLEN < 0 )
	      MTID	A device unit number as obtained from MTOPEN.
	      ARRAY	Target array for the data.
	      ARRLEN	Length of ARRAY
	      TID	Tranfer ID from FTSD_GETH.
			If > zero on output more data is left on
			tape, thus subsequent calls of FTSI_GETR are
			nessecary. TID=0 ends a series of calls.
			TID=0 occurs at the end of file mark. After
			TID=0 the tape is positioned before the end of file.

Updates:
	Jun 27, 1990, PRR, Document created.
	Sep 12, 1990: SS,  Adjustments for new data structure.
	Sep 17, 1990: SS,  Added parameter nskip.
#<

Fortran to C interface:

@ integer function ftsi_getr( integer , real , integer , integer )

*/
fint ftsi_getr_c( fint *mtid  , float *arr_out ,
		  fint *ndata , fint  *morep   )
{
    return( Ftsi_Get( mtid, arr_out , ndata , morep , IOT_REAL ) ) ;
}

/*
#>	      ftsi_geti.dc2

Function:     FTSI_GETI

Purpose:      Gets integer data from a FITS tape.

Category:     FITS

File:	      fts_io.c

Author:       P. Roelfsema

Use:	      INTEGER FTSI_GETI( MTID,		Input	       INTEGER
				 ARRAY, 	Output	       INTEGER
				 ARRLEN,	Input	       INTEGER
				 TID )		Input/Output   INTEGER
	      FTSI_GETI Returns:
			  >0 - number of integers transferred to ARRAY.
		       =<-10 - tape error (see MTIODEV.DC2)
			 -11 - not a FITS file.
			 -12 - found and skipped tape label
			 -13 - double tape mark found, tape skipped back to
			       before second tape mark.
			 -20 - could not get enough memory for internal ftsbuf.
			 -22 - conversion error
			 -30 - tried to read from a tape (MTID) which is
			       unknown to the FTS* routines.
			       (occurs e.g. when FTSI_GETI is called
			       before FTSD_GETH)
			 -31 - cannot read backwards ( ARRLEN < 0 )
	      MTID	A device unit number as obtained from MTOPEN.
	      ARRAY	Target array for the data.
	      ARRLEN	Length of ARRAY
	      TID	Tranfer ID from FTSD_GETH.
			If non zero on output more data is left on
			tape, thus subsequent calls of FTSI_GETI are
			nessecary. TID=0 ends a series of calls.
			TID=0 occurs at the end of file mark. After
			TID=0 the tape is positioned befor end of file.

Updates:
	Jun 27, 1990, PRR, Document created.
	Sep 12, 1990: SS,  Adjustments for new data structure.

#<

Fortran to C interface:

@ integer function ftsi_geti( integer ,integer ,integer, integer)

*/

fint ftsi_geti_c( fint *mtid  , fint *arr_out ,
			       fint *ndata , fint *morep   )
{
    return( Ftsi_Get( mtid , arr_out , ndata , morep , IOT_INT ) ) ;
}

/*
#>	      fts_skippix.dc2

Function:     fts_skippix

Purpose:      Skip pixels in a FITS file.

Category:     FITS

File:	      fts_io.c

Author:       Peter Roelfsema

Use:	      FTS_SKIPPIX ( MTID,	input		INTEGER,
			    NSKIP )	input/output	INTEGER.

	      FTS_SKIPPIX Returns:
			  >0 - number of pixels skipped on MTID.
		   -1 .. -10 - tape error (see MTIODEV.DC2)
			 -13 - end of tape found
			 -31 - Only forward skipping allowed.
	      MTID	A device unit number as obtained from MTOPEN.
	      NSKIP	Number of pixels to skip.

Description:

	  FTS_SKIPPIX Skips nskip pixels in FITS files. This function is very
       usefull when only known small sections from FITS files have to be read.


Updates:
	Oct 29, 1990: PRR, Original document.
#<
Declaration for FORTRAN to C interface:

@ integer function fts_skippix( integer, integer)

*/
fint fts_skippix_c( fint *mtid , fint *nskip )
{
   Mt_Struct	*tape ; 		/* Pointer to mt struct.	*/
   fint 	 sbyte = 0 ;		/* Nr. of bytes to skip 	*/
   fint 	 sblk = 0 ;		/* Nr. of tape buffers to skip	*/
   fint 	 ret = 0 ;		/* Return value from mt routines*/
   fint 	 blocksize = 0 ;	/* size of tapeblocks		*/

    if ( !( tape = Get_Tape( mtid , MT_GET ) ) ){
	return( UNKNOWN_MTID ) ;		/* No tape -> exit	*/
    }

    if ( *nskip ) {				/* Skip desired?	*/
       if ( *nskip < 0 ) {			/* Negative skip > error*/
	  return( Handle_Error( tape , IMPOSSIBLE_SKIP ) ) ;
       }
       sbyte = *nskip * tape->nbytes ;		/* Skip how many bytes? */
       if ( sbyte > MtBufLeft(tape) ) { 	/* More than in buffer? */
	  sbyte -= MtBufLeft(tape) ;		/* First flush buffer	*/
	  blocksize = tape->bufsize / tape->nblocks ;
	  sblk	 = sbyte / blocksize ;		/* Skip how many block? */
	  if ( sblk > 0 ) {			/* Skip blocks! 	*/
	     if ( ( ret = mtfsr_c( &tape->id , &sblk ) ) < 0 )	{
		return( Handle_Error( tape , ret ) ) ;
	     }
	     if ( ret < sblk ) {
		return( Handle_Error( tape , IMPOSSIBLE_SKIP ) ) ;
	     }
	     sbyte -= ( blocksize * ret ) ;	/* Decrease byteskip	*/
	  }
	  if ( ( ret = Data2MtBuf( tape ) ) < 0 ) {
	     return( Handle_Error( tape , ret ) ) ;
	  }
       }
       tape->u.cp += sbyte ;			/* Skip bytes in buffer */
   }

   return( *nskip ) ;				/* Return skip count	*/
}


/*
#>		ftsd_puth.dc2

Function:	ftsd_puth

Purpose:	Writes a FITS header on FITS tape.

Category:	FITS

File:		fts_io.c

Author: 	Peter Roelfsema

Use:		INTEGER FTSD_PUTH( MTID,		Input	INTEGER,
				   HEADER,		Input	CHARACTER*(*),
				   BLOCKSIZE )		Input	INTEGER.
	FTSD_PUTH returns:
		> 0 : number of characters written to tape (i.e. the
		      complete FITS header, including the END record).
	      <= -10: tape error (see MTIODEV.DC2):
		 -11: not a FITS file,
		 -14: SIMPLE not found,
		 -15: SIMPLE = FALSE not supported,
		 -16: BITPIX not found,
		 -17: only BITPIX 8, 16 and 32 are supported,
		 -18: NAXIS not found,
		 -19: not enough NAXIS descriptors found,
		 -20: I could not get enough memory for internal buffer,
		 -23: storage size of character type is not 1 byte,
		 -24: storage size of short type is not 2 bytes,
		 -25: storage size of long type is not 4 bytes,
		 -26: wrong blocksize.
	MTID	  A device unit number obtained from MTOPEN,
	HEADER	  A string in which the header is located,
	BLOCKSIZE The desired size for the FITS blocks on tape. Must be
		  a multiple of 2880 (FITS definition).

Description:

	  FTSD_PUTH is used as an initialization routine for writing data in
	FITS format to a tape.	It looks up some FITS descriptors necessary
	for encoding the data and validates them (see routines initio and
	Get_MinMax).  The header and the data will be written to tape in
	blocks of size blocksize.

	  Before writing to FTSD_PUTH will decode a number of FITS descriptors
	necessary for encoding the data: the descriptor SIMPLE should be set
	to True (False is not supported), the BITPIX setting should be in
	range of the current machine and the obligatory descriptors BLANK,
	NAXIS and NAXIS% should be defined in the header.  If any of these
	descriptors is missing or has an unknown setting, an error results.

	  FTSD_PUTH assumes that the input device is positioned at the
	beginning of a file, i.e.  immediately past an end of file mark.  The
	routine will overwrite whatever is on tape.

Updates:
	Aug 15, 1990: SS,  document created.
#<


Declaration for FORTRAN to C interface:

@ integer function ftsd_puth( integer, character, integer)

*/

fint ftsd_puth_c( fint *mtid , fchar header , fint *blocksize )
{
    fint	nret;			/* Return code. 		*/
    Mt_Struct	*tape;			/* Pointer to mt struct.	*/

    if ( ( tape = Open_Tape( mtid , MT_PUT ) ) == NULL ) {
	return( NO_MEMORY ) ;			/* No memory , exit	*/
    }

    if ( *blocksize % FTSBLOCKLEN ) {		/* Check buffer size	*/
	return( Handle_Error( tape , BAD_BLOCKSIZE ) ) ;
    }
    tape->bufsize = *blocksize; 		/* Set buffer size	*/

						/* Get buffer memory	*/
    if ( ( tape->buff = malloc( sizeof( char ) * tape->bufsize ) ) == NULL ) {
	return( Handle_Error( tape , NO_MEMORY ) ) ;
    }

    if ( ( nret = Check_Header( header , tape ) ) ) {
	return( Handle_Error( tape , nret ) ) ;
    } else if ( ( nret = Get_MinMax( tape , header ) ) ) {
	return( Handle_Error( tape , nret ) ) ;
    } else if ( ( nret = Head2MtBuf( header , tape ) ) < 0 ) {
	return( Handle_Error( tape , nret ) ) ;
    } else {
	return( nret ) ;
    }
}						/* END ftsd_puth_c	*/

/*
#>		ftsi_puti.dc2

Function:	ftsi_puti

Purpose:	Write integer data to FITS tape.

Category:	FITS

File:		fts_io.c

Author: 	Peter Roelfsema

Use:		INTEGER FTSI_PUTI ( MTID	input	INTEGER,
				    ARRAY	input	INTEGER ( >ARRLEN ),
				    ARRLEN )	input	INTEGER.
	FTSI_PUTI returns:
		>= 0 : number of integers accepted,
		-10 : tape io error,
		-20 : no memory for iobuf available,
	MTID	A device unit number as obtained from MTOPEN,
	ARRAY	Source array of the data,
	ARRLEN	Length of ARRAY, ARRLEN = 0 will result in the writing
		of a file mark indicating the end of the FITS file.


Description:
	Fills tape with integers.  The integers are checked to be in range
	as defined by bitpix.  If not a BLANK is put on tape instead of
	the integer.  Returned is the number of integers accepted for copy
	to tape or a negative number for an error indication.

Updates:
	Jul 26, 1991: AdJ, more consistent return values.
	Aug 15, 1990: SS,  document created.

#<
Declaration for FORTRAN to C interface:

@ integer function ftsi_puti( integer, integer, integer )

*/

fint ftsi_puti_c( fint *mtid , fint *array , fint *arrlen )
{
    return( Ftsi_Put( mtid , array , arrlen , IOT_INT ) ) ;

}

/*
#>		ftsi_putr.dc2

Function:	ftsi_putr

Purpose:	Write real data to FITS tape.

Category:	FITS

File:		fts_io.c

Author: 	Peter Roelfsema

Use:		INTEGER FTSI_PUTR ( MTID	input	INTEGER,
				    ARRAY	input	REAL ( >ARRLEN ),
				    ARRLEN )	input	INTEGER.
	FTSI_PUTR returns:
		>= 0 : number of values accepted,
		-10 : tape io error,
		-20 : no memory for iobuf available,
	MTID	A device unit number as obtained from MTOPEN,
	ARRAY	Source array of the data,
	ARRLEN	Length of ARRAY, ARRLEN = 0 will result in the writing
		of a file mark indicating the end of the FITS file.


Description:
	Fills a FITS tape with reals from ARRAY.  The reals are
	checked to be in range as defined by bitpix.  If not in range
	a BLANK is put on tape instead of the real.  Returned is the
	number of reals accepted for copy to tape,
	or a negative number as an error indication.

Updates:
	Jul 26, 1991: AdJ, more consistent return values.
	Aug 23, 1990: SS,  document created.
#<
Declaration for FORTRAN to C interface:

@ integer function ftsi_putr( integer , real , integer )

*/
fint ftsi_putr_c( fint *mtid , float *array , fint *arrlen )
{
    return( Ftsi_Put( mtid , array , arrlen , IOT_REAL ) ) ;
}


/*
#>		fts_skipfil.dc2

Function:	fts_skipfil

Purpose:	Skip FITS files forwards or backwards.

Category:	FITS

File:		fts_io.c

Author: 	Peter Roelfsema

Use:		FTS_SKIPFIL( MTID,	input		INTEGER,
			     NSKIP )	input/output	INTEGER.

		FTS_SKIPFIL Returns:
			   >0 - the absolute number of files skipped
		    -1 .. -10 - tape error (see MTIODEV.DC2)
			  -13 - Begin of tape (NSKIP < 0 )
			      - End of tape (NSKIP > 0 )
		MTID	 A device ID as obtained from MTOPEN
		NSKIP	 Number of files to skip forward (NSKIP>0) or
			 backward (NSKIP<0). NSKIP=0 will result in a
			 skip to begin of current file.

Description:

	FTS_SKIPFIL Skips nskip FITS files.
	 Returned is the number of files skipped or some error as detected by
	tape io routines.  If tape contains less then nskip files, the number
	returned is smaller than nskip!

Updates:
	Oct 29, 1990: PRR, Original document.
#<
Declaration for FORTRAN to C interface:

@ integer function fts_skipfil( integer, integer)

*/
fint fts_skipfil_c ( fint *mtid , fint *nskip )
{
    fint	one	= 1;		/* Number of bytes to read.	*/
    fint	n	= 0;		/* Counter files skipped.	*/
    fint	buff;			/* Buffer for read		*/
    fint	nret;			/* Return value mt routines.	*/

    if ( *nskip > 0 ) { 			/* Skip forward 	*/
       for ( n = 0 ; n < *nskip ; n++ ) {
	  if ( ( nret = mtfsf_c( mtid , &one ) ) < 0 ) {
	     if ( nret == MTIO_END_OF_TAPE ) {	/* Mt EOT detected	*/
		nret = END_OF_TAPE ;		/* Set FTS EOT code	*/
		(void) mtbsf_c( mtid , &one ) ; /* Skip file back	*/
	     }
	     return( nret ) ;			/* Skip error => return */
	  } else if ( nret == 0 ) {
	     return( END_OF_TAPE ) ;		/* Cannot skip anymore	*/
	  }
	  if ( ( nret = mtread_c( mtid , &buff , &one ) ) < 0 ) {
	     if ( nret == MTIO_END_OF_TAPE ) {	/* Mt EOT detected	*/
		nret = END_OF_TAPE ;		/* Set FTS EOT code	*/
		(void) mtbsf_c( mtid , &one ) ; /* Skip file back	*/
	     }
	     return( nret ) ;			/* Read error => return */
	  } else if ( nret == 0 ) {		/* Next is EOF => EOT	*/
	     (void) mtbsf_c( mtid , &one ) ;	/* Skip file back	*/
	     return( END_OF_TAPE ) ;		/* Done => return	*/
	  } else {				/* Next is new file	*/
	     (void) mtbsr_c( mtid , &one ) ;	/* Skip record back	*/
	  }
       }

    } else if ( *nskip == 0 ) { 		/* Skip to current BOF	*/
       if ( ( nret = mtbsf_c( mtid , &one ) ) < 0 ) {
	  return( nret ) ;			/* Skip error => return */
       } else if ( nret > 0 ) { 		/* Skipped back -> EOF	*/
	  (void) mtfsf_c( mtid , &one ) ;	/* Skip forward -> BOF	*/
       }
       if ( ( nret = mtread_c( mtid , &buff , &one ) ) < 0 ) {
	  if ( nret == MTIO_END_OF_TAPE ) {	/* Mt EOT detected	*/
	     nret = END_OF_TAPE ;		/* Set FTS EOT code	*/
		(void) mtbsr_c( mtid , &one ) ; /* Skip record back	*/
	  }
	  return( nret ) ;			/* Read error => return */
       } else if ( nret == 0 ) {		/* Next is EOF => EOT	*/
	  (void) mtbsr_c( mtid , &one ) ;	/* Skip record back	*/
	  return( END_OF_TAPE ) ;		/* Done => return	*/
       } else { 				/* Next is new file	*/
	  (void) mtbsr_c( mtid , &one ) ;	/* Skip record back	*/
       }
       return( *nskip ) ;
    } else {					/* Skip backward	*/
       for ( n = 0 ; n < -*nskip ; n++ ) {	/* Loop on nskip	*/
	  if ( ( nret = mtbsf_c( mtid , &one ) ) < 0 ) {
	     return( nret ) ;			/* Skip error => return */
	  } else if ( nret == 0 ) {		/* Zero skip => BOT	*/
	     return( n ) ;			/* Done => return	*/
	  }
       }
       if ( ( nret = mtbsf_c( mtid , &one ) ) < 0 ) {
	  return( nret ) ;			/* Skip error => return */
       } else if ( nret == 0 ) {		/* Zero skip => BOT	*/
	  return( n ) ; 			/* Done => return	*/
       }
       if ( ( nret = mtfsf_c( mtid , &one ) ) < 0 ) {
	  return( nret ) ;			/* Skip error => return */
       }
    }

    return( n ) ;
}						/* END fts_skipfil_c.	*/
