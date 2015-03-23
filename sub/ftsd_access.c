/* ftsd_access.c

                Copyright (c) Kapteyn Laboratorium Groningen 1997
                          All Rights Reserved.
                          

#>            ftsd_access.dc2


Purpose:      This file contains routines to access data in a FITS header.

Category:     FITS

File:	      ftsd_access.c

Author:	      Sjag Steensma (K.G. Begeman)


Description:

	The FITS (Flexible Image Transport System) format (see Wells) is
	designed for the interchange of astronomical images and other digital
	arrays on magnetic tape.  A FITS tape contains one or more files,
	each file containing the data for one image.  A file begins with one
	or more logical (tape) records containing "header" data in the form
	of 36 80-character (8-bits per character) records.  The last header
	record contains only the string END.  The data array starts at the
	first 2880 byte boundary after the END record.

	The header records describe the structure and coordinate systems of
	the data array and convey any auxiliary parameters and accompanying
	text.  The basis grammar of a FITS header record is:

                    descriptor = value / comment

	A descriptor is an 8-character ASCII string and unique in the header.
	The descriptors HISTORY and COMMENT are used for comment and may
	occur more than once in a header.  The = sign occurs in column 9 and
	column 10 is blank.  The parameter value is written in columns 11 -
	80 in a format depending on the type of the parameter.  The value
	field is optionally followed by a '/' and comment.
	The value field contains the value of the descriptor and can be of
	type character, logical, integer, real or double.  For every type
	separate read and write routines are used.

	FITS files reside on tape or are copied to disk for faster access.
	The header can be decoded using the routines described below.  To
	read a FITS file from tape or disk first get the complete header (one
	or more logical records) by calling FTSD_GETH(see fts_io.c).
	With the information given in the header the real or integer data can
	be obtained using the routines FTSI_GETR or FTSI_GETI.
	To write a FITS file to tape or disk first create a header.  With the
	routines FTSD_PUTH, FTSI_PUTR and FTSI_PUTI, the header and the real
	or integer data can be written to integer data blocks on disk.

	The routines in this file read or write records from or to a header.
	The header argument in the calling routine should be a variable of
	type fchar and a multiple of 80 characters in length.
	The FTSD_Rxxx routines search a FITS header for a descriptor and
	return the value if the descriptor is found in the header.  The
	header should conform the FITS standard, if not an error results.
	The FTSD_Wxxx routines fill a FITS header.  The header should be
	initialised with spaces.  With these routines descriptors (and their
	record) can be inserted, descriptor values altered or records removed.
	These routines return the offset of the record in the header, the
	first record having offset 0, the second 1 etc.

Related Docs:
	D.C. Wells et al. : 1981, 'FITS, A Flexible Image Transport System',
	Astronomy & Astrophysics Suppl. Ser. 44, 363-370.
	See for the GET and PUT routines file: fts_io.c.

Routines in this file:
	FTSD_FIND	Finds a FITS descriptor in a set of FITS records.
	FTSD_INSERT 	Inserts a record in a FITS header.
	FTSD_DELETE	Deletes a record from a FITS header.
	FTSD_RCHAR	Reads a FITS character descriptor from a FITS header.
	FTSD_RINT	Reads a FITS integer descriptor from a FITS header.
	FTSD_RREAL	Reads a FITS real descriptor from a FITS header.
	FTSD_RDBLE	Reads a FITS double descriptor from a FITS header.
	FTSD_RLOG	Reads a FITS logical descriptor from a FITS header.
	FTSD_WCHAR	Writes a FITS character descriptor from a FITS header.
	FTSD_WINT 	Writes a FITS integer descriptor in a FITS header.
	FTSD_WREAL 	Writes a FITS real descriptor in a FITS header.
	FTSD_WDBLE 	Writes a FITS double descriptor in a FITS header.
	FTSD_WLOG	Writes a FITS logical descriptor in a FITS header.
Updates:
	Aug 14, 1990: Sjag; document created.
#<
*/

#define ALLOW_FREE 1                    /* Allow free format records */

#include	"stdio.h"		/* <stdio.h> */
#include	"stddef.h"		/* <stddef.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"math.h"		/* <math.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */

/* size of FITS header: */
#define	FTSBLCKLEN	2880	/* length of tape block */
#define FTSRECLEN	80	/* length of FITS recorde */

/* partition of FITS header record: */
#define	FTSDSCNAMLEN	8	/* length of FITS descriptor */
#define	FTSDSPACE	2	/* '=' and space between descriptor and value */
#define MAXFIELDLEN     70	/* max. length of type character values */
#define FTSFIELDLEN     20	/* field length for other types */

/* Returned values: */
#define	NODESCRIPTOR	-1	/* Descriptor not found. */
#define	NOINFO		-2	/* No value field found */
#define	CONVERERROR	-3	/* Conversion error */
#define	HEADERFUL	-4	/* Header ful */
#define BADINDEX	-5	/* Used in ftsd_delete */

/* Descriptors used in these routines: */
#define	BLANKDSC 	"        "
#define	COMMENTDSC	"COMMENT "
#define	HISTORYDSC	"HISTORY "

#define SPACE		' '	/* Blank character */


/* min: return minimum of two values.
*/
#define min(a,b)	((a < b) ? a : b)


/* put_fts_value : move pfrec to '=' position in record and puts value 'v'
   in value field using format 's'. At exit 'p' points to character after last
   character written.
   NOTE: sprintf puts an ENDSTRING character in 'p', removed by last statement.
*/
#define put_fts_value(p, s, v) { \
p += FTSDSCNAMLEN; sprintf(p, s, v); \
p += strlen(p); \
*p = SPACE; } /* end macro put_fts_value(p, s, v). */


/* put_fts_comment(p, comstr, last) : Copy comstr to p[2..(last - 1)].
   'p' is pointer to first character available for comment in FITS
   record, comstr the comment string (type fchar) and  last the pointer to
   the first character of the next FITS record.  If comstr is empty or
   there are less then three characters left in the record for comment
   nothing happens. Else comstr is copied to p[2..(last - 1)] and
   p[0] and p[1] are filled with a backslash resp.  a space.  If
   comstr has more characters than available, only the characters that
   fit are copied to 'p'. If comstr has less characters, p is filled
   with spaces.
*/
static void put_fts_comment( char *p, fchar comstr, char *last)
{
    int  n; 			/* counter */

    if ( comstr.l > 0 && ((p + 3) < last) ) {	/* copy comment if */
    	*p++ = '/'; 				/*    comstr non-empty */
    	*p++ = SPACE; 				/*    AND there's room in p.*/
    	for (n = 0; p < last; n++) {
    	    if ( (n < comstr.l) && (comstr.a[n] != '\0') )
    	    	*p++ = comstr.a[n];
    	    else
    	        *p++ = SPACE;
    	}
    }
} /* void put_fts_comment( char *p, fchar comstr, char *last) */


/* ret_value : Returns the index of the record in the header.
*/
#define ret_value(p1, p2)	((p1 - p2) / FTSRECLEN)


/* index_to_offset: Returns offset of record(index) from start of header.
   NOTE: This function should be the inverse of ret_value without addition
   of p2, the address of the first record of the header.
*/
#define index_to_offset(i)       	(i * FTSRECLEN)


static char *find_descriptor( fchar fts_header, fchar fts_descriptor )
/*
 * find_descriptor returns a pointer to the FITS record in fts_header
 * which contains the FITS descriptor fts_descriptor. It returns NULL
 * if the FITS descriptor is not found.
 */
{
   char *r;					/* return value */
   char  dscbuf[FTSDSCNAMLEN];			/* internal descriptor buffer */
   fint  n, nrec;				/* counters */

   for (n = 0; n < FTSDSCNAMLEN && n < fts_descriptor.l; n++) {
      dscbuf[n] = fts_descriptor.a[n];		/* copy to internal buffer */
   }
   for (; n < FTSDSCNAMLEN; dscbuf[n++] = ' ');	/* add trailing blanks */
   nrec = fts_header.l / FTSRECLEN;		/* number of records */
   for (r = fts_header.a, n = 0; n < nrec && strncmp(r, dscbuf, FTSDSCNAMLEN);\
      n++) r += FTSRECLEN;
   if (n == nrec) r = NULL;			/* descriptor not found */
   return( r );					/* return to caller */
}

/*
#>            ftsd_find.dc2

Function:     FTSD_FIND

Purpose:      Finds a FITS descriptor in a set of FITS records.

Category:     FITS

File:         ftsd_access.c

Author:       K.G. Begeman

Use:          INTEGER FTSD_FIND( HEADER,      Input     CHARACTER*(*)
                                 DESCRIPTOR,  Input     CHARACTER*(*)
                                 RECORD )     Output    CHARACTER*(*)

              FTSD_FIND     Returns:
              		    >=0	: offset of record in header,
                            -1  : FITS descriptor not present
              HEADER        Contains FITS records where to look
                            for the descriptor.
              DESCRIPTOR    FITS descriptor to search for in HEADER.
              RECORD        If present, the complete FITS record
                            associated with DESCRIPTOR is returned
                            (including the descriptor).

Updates:      Jun 25, 1990: KGB, Document created.
	      Aug 14, 1990: Sjag; Offset of record is returned.

#<

Fortran to C interface:

@ integer function ftsd_find( character, character, character )

*/

fint ftsd_find_c( fchar fts_header, fchar fts_descriptor, fchar fts_record )
{
   char *ptr;					/* pointer to FITS record */
   fint  n;					/* loop counter */
   fint  r;				        /* return value */

   ptr = find_descriptor( fts_header, fts_descriptor );
   if (ptr == NULL) {
      r = NODESCRIPTOR;				/* descriptor not found */
   } else {
      r = ret_value(ptr, fts_header.a);		/* get offset record */
      for (n = 0; n < FTSRECLEN && n < fts_record.l; \
      	fts_record.a[n++] = *ptr++);
      while (n < fts_record.l) fts_record.a[n++] = ' ';
   }
   return( r );					/* return to caller */
}

/*
#>            ftsd_rchar.dc2

Function:     FTSD_RCHAR

Purpose:      Reads a FITS character descriptor from a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       K.G. Begeman

Use:          INTEGER FTSD_RCHAR( HEADER,      Input     CHARACTER*(*)
                                  DESCRIPTOR,  Input     CHARACTER*(*)
                                  VALUE )      Output    CHARACTER*(*)

              FTSD_RCHAR    Returns:
                             >=0 : Offset of record in header
                             -1	 : FITS descriptor not present
                             -2	 : descriptor does not carry info
                             -3	 : conversion error
              HEADER        Contains FITS records where to look
                            for the descriptor.
              DESCRIPTOR    FITS descriptor to search for in HEADER.
              VALUE         If present, the contents of the FITS record
                            associated with DESCRIPTOR is returned.

Updates:
	Jun 25, 1990: KGB, Document created.
	Aug  7, 1990: Sjag; Max. 68 char's in value field (see Wells).
        Aug 14, 1990: Sjag; Offset of record is returned.
        Dec 19, 1991: KGB, SS bug removed, work around gcc bug.
        Oct  8, 2001: JPT, Allow free format character strings.
#<

Fortran to C interface:

@ integer function ftsd_rchar( character, character, character )

*/

fint ftsd_rchar_c( fchar fts_header, fchar fts_descriptor, fchar value )
{
   char *ptr;					/* pointer to FITS record */
   fint  n;					/* loop counter */
   fint  len;					/* length of value */
   fint  r;					/* return value */
   int	 q; 					/* stop value for loop */

   len = value.l;				/* because of bug in gcc */
   for(n = 0; n < len; value.a[n++] = ' ');	/* reset return value */
   ptr = find_descriptor( fts_header, fts_descriptor );
   if (ptr == NULL) r = NODESCRIPTOR;		/* descriptor not found */
   else if (ptr[FTSDSCNAMLEN] != '=') {
      r = NOINFO;				/* item does not carry info */
   } else {
      int maxfieldlen=MAXFIELDLEN;
      for (n=FTSDSCNAMLEN+2; n<FTSRECLEN-1 && ptr[n] != '\''; n++) {
         maxfieldlen--;
      }
      if (ptr[n] != '\'') return CONVERERROR;
      r = ret_value(ptr, fts_header.a);		/* get offset record */
      ptr +=  n + 1;				/* position pointer */
      q = min(len, (maxfieldlen-2));
      for (n = 0; n < q && ptr[n] != '\''; value.a[n] = ptr[n], n++);
      if (ptr[n] != '\'') {			/* No closing quote found! */
      	 r = CONVERERROR;			/* conversion error */
      	 for (n = 0; n < len; value.a[n++] = ' ');/* reset return value */
      }
   }
   return( r );					/* return to caller */
} /* end fint ftsd_rchar_c(fchar fts_header,fchar fts_descriptor,fchar value) */

/*
#>            ftsd_rint.dc2

Function:     FTSD_RINT

Purpose:      Reads a FITS integer descriptor from a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       K.G. Begeman

Use:          INTEGER FTSD_RINT( HEADER,      Input     CHARACTER*(*)
                                 DESCRIPTOR,  Input     CHARACTER*(*)
                                 VALUE )      Output    INTEGER*(*)

              FTSD_RINT     Returns:
                             >=0 : Offset of record in header
                             -1	 : FITS descriptor not present
                             -2	 : descriptor does not carry info
                             -3	 : conversion error
              HEADER        Contains FITS records where to look
                            for the descriptor.
              DESCRIPTOR    FITS descriptor to search for in HEADER.
              VALUE         If present, the contents of the FITS record
                            associated with DESCRIPTOR is returned.

Updates:
	Jun 25, 1990: KGB, Document created.
	Aug 14, 1990: Sjag; Offset of record is returned.
	Aug 25, 2010: JPT, Allow free format numbers.

#<

Fortran to C interface:

@ integer function ftsd_rint( character, character, integer )

*/

fint ftsd_rint_c( fchar fts_header, fchar fts_descriptor, fint *value )
{
   char *ptr;					/* pointer to FITS record */
   fint  r;					/* return value */

   *value = 0;					/* reset return value */
   ptr = find_descriptor( fts_header, fts_descriptor );
   if (ptr == NULL) {
      r = NODESCRIPTOR;				/* descriptor not found */
   } else if (ptr[FTSDSCNAMLEN] != '=') {
      r = NOINFO;				/* item does not carry info */
   } else {
      char *endp[1];

      *endp = ptr;				/* set */
      r = ret_value(ptr, fts_header.a);		/* get offset record */
      *value = strtol( &ptr[FTSDSCNAMLEN+2], endp, 10 );
#if !ALLOW_FREE
      if (endp[0] != &ptr[FTSDSCNAMLEN+2+FTSFIELDLEN])
         r = CONVERERROR;			/* conversion error */
#endif
   }
   return( r );					/* return to caller */
}

/*
#>            ftsd_rreal.dc2

Function:     FTSD_RREAL

Purpose:      Reads a FITS real descriptor from a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       K.G. Begeman

Use:          INTEGER FTSD_RREAL( HEADER,      Input     CHARACTER*(*)
                                  DESCRIPTOR,  Input     CHARACTER*(*)
                                  VALUE )      Output    REAL*(*)

              FTSD_RREAL    Returns:
                             >=0 : Offset of record in header
                             -1	 : FITS descriptor not present
                             -2	 : descriptor does not carry info
                             -3	 : conversion error
              HEADER        Contains FITS records where to look
                            for the descriptor.
              DESCRIPTOR    FITS descriptor to search for in HEADER.
              VALUE         If present, the contents of the FITS record
                            associated with DESCRIPTOR is returned.

Updates:
	Jun 25, 1990: KGB, Document created.
	Aug 14, 1990: Sjag; Offset of record is returned.
	Feb 23, 1996: KGB, Fortran double format allowed.
	Aug 25, 2010: JPT, Allow free format numbers.

#<

Fortran to C interface:

@ integer function ftsd_rreal( character, character, real )

*/

fint ftsd_rreal_c( fchar fts_header, fchar fts_descriptor, float *value )
{
   char *ptr;					/* pointer to FITS record */
   fint  r;					/* return value */

   *value = 0.0;				/* reset return value */
   ptr = find_descriptor( fts_header, fts_descriptor );
   if (ptr == NULL) {
      r = NODESCRIPTOR;				/* descriptor not found */
   } else if (ptr[FTSDSCNAMLEN] != '=') {
      r = NOINFO;				/* item does not carry info */
   } else {
      char *endp[1];

      *endp = ptr;				/* set */
      r = ret_value(ptr, fts_header.a);		/* get offset record */
      *value = strtod( &ptr[FTSDSCNAMLEN+2], endp );
#if ALLOW_FREE
      if (1) {
#else
      if (endp[0] != &ptr[FTSDSCNAMLEN+2+FTSFIELDLEN]) {
#endif
         if ( endp[0][0] == 'D' || endp[0][0] == 'd' ) {
            long	e;

            endp[0]++;
            e = strtol( endp[0], endp, 10 );
#if ALLOW_FREE
            if (0) {
#else
            if (endp[0] != &ptr[FTSDSCNAMLEN+2+FTSFIELDLEN]) {
#endif
               r = CONVERERROR;			/* conversion error */
            } else {
               if ( e < 0 ) {
            	  (*value) /= pow( 10.0, -e );
               } else if ( e > 0 ) {
                  (*value) *= pow( 10.0, e );
               }
            }
         } else {
#if !ALLOW_FREE
            r = CONVERERROR;			/* conversion error */
#endif
         }
      }
   }
   return( r );					/* return to caller */
}

/*
#>            ftsd_rdble.dc2

Function:     FTSD_RDBLE

Purpose:      Reads a FITS double descriptor from a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       K.G. Begeman

Use:          INTEGER FTSD_RDBLE( HEADER,      Input     CHARACTER*(*)
                                  DESCRIPTOR,  Input     CHARACTER*(*)
                                  VALUE )      Output    DOUBLE*(*)

              FTSD_RDBLE    Returns:
                             >=0 : Offset of record in header
                             -1	 : FITS descriptor not present
                             -2	 : descriptor does not carry info
                             -3	 : conversion error
              HEADER        Contains FITS records where to look
                            for the descriptor.
              DESCRIPTOR    FITS descriptor to search for in HEADER.
              VALUE         If present, the contents of the FITS record
                            associated with DESCRIPTOR is returned.

Updates:
	Jun 25, 1990: KGB, Document created.
	Aug 14, 1990: Sjag; Offset of record is returned.
	Feb 23, 1996: KGB, Fortran double format allowed.
	Aug 25, 2010: JPT, Allow free format numbers.

#<

Fortran to C interface:

@ integer function ftsd_rdble( character, character, double precision )

*/

fint ftsd_rdble_c( fchar fts_header, fchar fts_descriptor, double *value )
{
   char *ptr;					/* pointer to FITS record */
   fint  r;					/* return value */

   *value = 0.0;				/* reset return value */
   ptr = find_descriptor( fts_header, fts_descriptor );
   if (ptr == NULL) {
      r = NODESCRIPTOR;				/* descriptor not found */
   } else if (ptr[FTSDSCNAMLEN] != '=') {
      r = NOINFO;				/* item does not carry info */
   } else {
      char *endp[1];

      *endp = ptr;				/* set */
      r = ret_value(ptr, fts_header.a);		/* get offset record */
      *value = strtod( &ptr[FTSDSCNAMLEN+2], endp );
#if ALLOW_FREE
      if (1) {
#else
      if (endp[0] != &ptr[FTSDSCNAMLEN+2+FTSFIELDLEN]) {
#endif
         if ( endp[0][0] == 'D' || endp[0][0] == 'd' ) {
            long	e;

            endp[0]++;
            e = strtol( endp[0], endp, 10 );
#if ALLOW_FREE
            if (0) {
#else
            if (endp[0] != &ptr[FTSDSCNAMLEN+2+FTSFIELDLEN]) {
#endif
               r = CONVERERROR;			/* conversion error */
            } else {
               if ( e < 0 ) {
            	  (*value) /= pow( 10.0, -e );
               } else if ( e > 0 ) {
                  (*value) *= pow( 10.0, e );
               }
            }
         } else {
#if !ALLOW_FREE
            r = CONVERERROR;			/* conversion error */
#endif
         }
      }
   }
   return( r );					/* return to caller */
}

/*
#>            ftsd_rlog.dc2

Function:     FTSD_RLOG

Purpose:      Reads a FITS logical descriptor from a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       K.G. Begeman

Use:          INTEGER FTSD_RLOG( HEADER,      Input     CHARACTER*(*)
                                 DESCRIPTOR,  Input     CHARACTER*(*)
                                 VALUE )      Output    LOGICAL*(*)

              FTSD_RLOG     Returns:
                             >=0 : Offset of record in header
                             -1	 : FITS descriptor not present
                             -2	 : descriptor does not carry info
                             -3	 : conversion error
              HEADER        Contains FITS records where to look
                            for the descriptor.
              DESCRIPTOR    FITS descriptor to search for in HEADER.
              VALUE         If present, the contents of the FITS record
                            associated with DESCRIPTOR is returned.

Updates:      Jun 25, 1990: KGB, Document created.
	      Aug 14, 1990: Sjag; Offset of record is returned.


#<

Fortran to C interface:

@ integer function ftsd_rlog( character, character, logical )

*/

fint ftsd_rlog_c( fchar fts_header, fchar fts_descriptor, bool *value )
{
   char *ptr;					/* pointer to FITS record */
   fint  r;					/* return value */

   *value = FALSE;				/* reset return value */
   ptr = find_descriptor( fts_header, fts_descriptor );
   if (ptr == NULL) {
      r = NODESCRIPTOR;				/* descriptor not found */
   } else if (ptr[FTSDSCNAMLEN] != '=') {
      r = NOINFO;				/* item does not carry info */
   } else if (ptr[FTSDSCNAMLEN+FTSFIELDLEN+1] == 'T') {
      *value = TRUE;				/* logical is true */
      r = ret_value(ptr, fts_header.a);		/* get offset record */
   } else if (ptr[FTSDSCNAMLEN+FTSFIELDLEN+1] == 'F') {
      *value = FALSE;				/* logical is false */
      r = ret_value(ptr, fts_header.a);		/* get offset record */
   } else {
      r = CONVERERROR;				/* conversion error */
   }
   return( r );					/* return to caller */
}


static char *insert_descriptor( fchar fts_header, fchar fts_descriptor )
/* insert_descriptor returns a pointer to the FITS record in fts_header
   which contains the FITS descriptor fts_descriptor. If fts_descriptor
   is not in fts_header, an empty line (i.e. a line without a descriptor)
   is searched to insert the descriptor. If found the descriptor is written
   in the descriptor field and the record is cleared. If descriptor is not
   present and no empty line is found, NULL is returned.
   NOTE: A comment without COMMENT or HISTORY descriptor might be deleted!
 */
{
    char *ptr;			/* pointer to FITS record. */
    int	 q, n;			/* stop value for loop, counter */

    ptr = find_descriptor(fts_header, fts_descriptor);
    if (ptr == NULL) 	   /* If not found look for line without descriptor: */
    	ptr = find_descriptor(fts_header, tofchar(BLANKDSC));
    if (ptr != NULL) {
    	q = min(fts_descriptor.l, FTSDSCNAMLEN);
    	for (n = 0; n < q; ptr[n++] = fts_descriptor.a[n]);
    }
    return( ptr );				/* return to caller */
} /* end *insert_descriptor( fchar fts_header, fchar fts_descriptor) */


/*
#>            ftsd_insert.dc2

Function:     FTSD_INSERT

Purpose:      Inserts record of DESCRIPTOR in HEADER.

Category:     FITS

File:         ftsd_access.c

Author:       Sjag Steensma (K.G. Begeman)

Use:          INTEGER FTSD_INSERT( HEADER,      Input	  CHARACTER*(*)
                                   DESCRIPTOR,  Input     CHARACTER*(*)
                                   RECORD )	input	  CHARACTER*(*)

              FTSD_INSERT   Returns:
                             >=0 : Offset of record used in header
			    -4	 : No room in header.
              HEADER        Contains FITS header to put record in
              DESCRIPTOR    FITS search descriptor
              RECORD	    Record to put in record of DESCRIPTOR in
              		    HEADER.
Description:
	FTSD_INSERT looks for a record with descriptor DESCRIPTOR. If found,
	RECORD is copied to the record found. If not found, a record with
	a blank descriptor is searched. If found, RECORD is copied there
	else -4 is returned.
	The descriptors COMMENT and HISTORY are always added to the header
	(i.e. a blank descriptor is searched) since these descriptors may
	occur more than once in a header.

Updates:
	Aug 14, 1990: Sjag; document created.

#<

Fortran to C interface:

@ integer function ftsd_insert( character, character, character )

*/

fint ftsd_insert_c( fchar fts_header, fchar fts_descriptor, fchar fts_record )
{
   char *pfrec;			/* pointer to FITS record */
   fint  r = HEADERFUL;		/* return value, default: no room in header */
   int   q, n;			/* # chars in descr, loop counter */

   q = min(fts_descriptor.l, FTSDSCNAMLEN);
   if ( (strncmp(fts_descriptor.a, COMMENTDSC, q) == 0) ||
      (strncmp(fts_descriptor.a, HISTORYDSC, q) == 0) )
      	pfrec = insert_descriptor(fts_header, tofchar(BLANKDSC));
   else
   	pfrec = insert_descriptor(fts_header, fts_descriptor);
   if (pfrec != NULL) {
        r = ret_value(pfrec, fts_header.a);	/* get offset record */
        q = min(FTSRECLEN, fts_record.l);
   	for (n = 0; n < q; *pfrec++ = fts_record.a[n++]);
   }
   return( r );					/* return to caller */
} /* end ftsd_insert_c( fchar fts_header, fchar fts_descriptor,
	fchar fts_record ) */


/*
#>            ftsd_delete.dc2

Function:     FTSD_DELETE

Purpose:      Deletes a record from a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       Sjag Steensma (K.G. Begeman)

Use:          INTEGER FTSD_DELETE( HEADER,      Input	  CHARACTER*(*)
                                   DESCRIPTOR   Input     CHARACTER*(*)
                                   INDEX )	Input	  INTEGER

              FTSD_DELETE   Returns:
                             =>0 : Index of record deleted from header,
			    -1   : Descriptor not found.
			    -5   : Bad index.
              HEADER        Contains FITS header to delete record from,
              DESCRIPTOR    FITS search descriptor,
              INDEX	    Index of HISTORY or COMMENT descriptor to delete.

Description:
	Ftsd_delete looks for the first occurence of a record with descriptor
	DESCRIPTOR.  If found, the record is deleted from the header (i.e.
	filled with spaces).
	For the descriptors COMMENT and HISTORY an index should be given.
	Starting from the index, the first record with a descriptor as
	given will be deleted. If (index < 0) -5 is returned and nothing
	happens.

Updates:
	Aug 14, 1990: Sjag; document created.

#<

Fortran to C interface:

@ integer function ftsd_delete( character, character, integer )

*/

fint ftsd_delete_c(fchar fts_header, fchar fts_descriptor, fint * index)
{
   char *pfrec, *toobig;	/* pointer to FITS record */
   fint  r = NODESCRIPTOR;	/* return value, default: desc. not found */
   int   q;			/* # chars in descr */
   fchar loc_head;		/* local header variable */

   q = min(fts_descriptor.l, FTSDSCNAMLEN);
   if ( (strncmp(fts_descriptor.a, COMMENTDSC, q) == 0) ||
      (strncmp(fts_descriptor.a, HISTORYDSC, q) == 0) ) {
      	if ( * index >= 0) {
      	    loc_head.a = fts_header.a + index_to_offset(*index);
      	    loc_head.l = (int) fts_header.a - index_to_offset(*index);
      	    pfrec = find_descriptor(loc_head, fts_descriptor);
      	} else {
      	    pfrec = NULL;		/* Unknown index given. */
      	    r = BADINDEX;
      	}
   } else
       pfrec = find_descriptor(fts_header, fts_descriptor);
   if (pfrec != NULL) {			/* Clear record. */
      r = ret_value(pfrec, fts_header.a);/* Get offset record */
      for (toobig = pfrec + FTSRECLEN; pfrec < toobig; *pfrec++ = SPACE);
   }
   return( r );				/* return to caller */
} /* end ftsd_delete_c( fchar fts_header, fchar fts_descriptor) */


/*
#>            ftsd_wchar.dc2

Function:     FTSD_WCHAR

Purpose:      Writes a FITS character descriptor in a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       Sjag Steensma (K.G. Begeman)

Use:          INTEGER FTSD_WCHAR( HEADER,      Input     CHARACTER*(*)
                                  DESCRIPTOR,  Input     CHARACTER*(*)
                                  VALUE,       Input     CHARACTER*(*)
				  COMMENT )    Input	 CHARACTER*(*)

	      FTSD_WCHAR    Returns:
                            >=0 : Index of record used in header,
                            -4	: No room in header.
              HEADER        Contains FITS header to write descriptor
                            to.
              DESCRIPTOR    FITS descriptor to write in HEADER.
              VALUE         Value for value field of DESCRIPTOR.
              COMMENT	    Comment for commentfield of record.

Updates:
	Aug 14, 1990: Sjag; document created.

#<

Fortran to C interface:

@ integer function ftsd_wchar( character, character, character, character )

*/

fint ftsd_wchar_c( fchar fts_header, fchar fts_descriptor, fchar value, \
	fchar fts_comment )
{
   char	*pfrec, s[MAXFIELDLEN - 1]; /* pointer to record, local buffer */
   char *nextrec; 		/* pointer to next FITS record */
   int	n, q;			/* counters */
   fint	r = HEADERFUL;		/* return value; default: no room in header */

   pfrec = insert_descriptor (fts_header, fts_descriptor);
   if (pfrec != NULL) {				/* if room insert value */
        r = ret_value(pfrec, fts_header.a);	/* get offset record */
        nextrec = pfrec + FTSRECLEN; 		/* pointer to next record */
        q = min(value.l, (MAXFIELDLEN - 2));
        for (n = 0; n < q; s[n] = value.a[n++]);/* Copy value */
	s[n] = '\0';				/* add endstring sign) */
        put_fts_value(pfrec, "= '%-8s' ", s);	/* format: minimal 8
				        	   characters, left justified. */
        put_fts_comment(pfrec, fts_comment, nextrec);
   }
   return ( r );
} /* end ftsd_wchar_c */

/*
#>            ftsd_wint.dc2

Function:     FTSD_WINT

Purpose:      Writes a FITS integer descriptor in a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       Sjag Steensma (K.G. Begeman)

Use:          INTEGER FTSD_WINT( HEADER,      Input     CHARACTER*(*)
                                 DESCRIPTOR,  Input     CHARACTER*(*)
                                 VALUE,       Input     INTEGER
                                 COMMENT )    Input     CHARACTER*(*)

              FTSD_WINT     Returns:
                            >=0 : Index of record in header,
                            -4	: No room in header.
              HEADER        Contains FITS header to write descriptor in.
              DESCRIPTOR    FITS descriptor to write in HEADER.
              VALUE         Value for value field of DESCRIPTOR.
              COMMENT	    Comment for commentfield of record.

Updates:
	Aug 14, 1990: Sjag; document created.

#<

Fortran to C interface:

@ integer function ftsd_wint( character, character, integer, character )

*/

fint ftsd_wint_c( fchar fts_header, fchar fts_descriptor, fint *value, \
	fchar fts_comment )
{
   char *pfrec;			/* pointer to FITS record */
   char *nextrec; 		/* pointer to next FITS record */
   fint	r = HEADERFUL;		/* return value; default: no room in header */

   pfrec = insert_descriptor (fts_header, fts_descriptor);
   if (pfrec != NULL) {				/* if room insert value */
        r = ret_value(pfrec, fts_header.a);	/* get offset record */
        nextrec = pfrec + FTSRECLEN; 		/* pointer to next record */
        put_fts_value(pfrec, "= %20d ", *value);
        put_fts_comment(pfrec, fts_comment, nextrec);
   }
   return ( r );
} /* end ftsd_wint_c */

/*
#>            ftsd_wreal.dc2

Function:     FTSD_WREAL

Purpose:      Writes a FITS real descriptor in a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       Sjag Steensma (K.G. Begeman)

Use:          INTEGER FTSD_WREAL( HEADER,      Input     CHARACTER*(*)
                                  DESCRIPTOR,  Input     CHARACTER*(*)
				  VALUE,       Input	 REAL
				  COMMENT )    Input     CHARACTER*(*)

              FTSD_WREAL    Returns:
                            >=0 : Index of record in header,
                            -4	: No room in header.
              HEADER        Contains FITS header to write descriptor in.
              DESCRIPTOR    FITS descriptor to write in HEADER.
              VALUE         Value for value field of DESCRIPTOR.
              COMMENT	    Comment for commentfield of record.

Updates:
	Aug 14, 1990: Sjag; document created.

#<

Fortran to C interface:

@ integer function ftsd_wreal( character, character, real, character )

*/

fint ftsd_wreal_c( fchar fts_header, fchar fts_descriptor, float *value, \
	fchar fts_comment )
{
   char *pfrec;			/* pointer to FITS record */
   char *nextrec; 		/* pointer to next FITS record */
   fint	r = HEADERFUL;		/* return value; default: no room in header */

   pfrec = insert_descriptor (fts_header, fts_descriptor);
   if (pfrec != NULL) {				/* if room insert value */
        r = ret_value(pfrec, fts_header.a);	/* get offset record */
        nextrec = pfrec + FTSRECLEN; 		/* pointer to next record */
    	put_fts_value(pfrec, "= %#20.6E ", *value);
    	put_fts_comment(pfrec, fts_comment, nextrec);
   }
   return ( r );
} /* end ftsd_wreal_c */

/*
#>            ftsd_wdble.dc2

Function:     ftsd_wdble

Purpose:      Writes a FITS double descriptor in a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       Sjag Steensma (K.G. Begeman)

Use:          INTEGER FTSD_WDBLE( HEADER,      Input     CHARACTER*(*)
                                  DESCRIPTOR,  Input     CHARACTER*(*)
                                  VALUE,       Input     DOUBLE
                                  COMMENT )    Input     CHARACTER*(*)

              FTSD_WDBLE    Returns:
                            >=0 : Index of record in header,
                            -4	: No room in header.
              HEADER        Contains FITS header to write descriptor in.
              DESCRIPTOR    FITS descriptor to write in HEADER.
              VALUE         Value for value field of DESCRIPTOR.
              COMMENT	    Comment for commentfield of record.

Updates:
	Aug 14, 1990: Sjag; document created.

#<

Fortran to C interface:

@ integer function ftsd_wdble( character, character, double precision, character )

*/

fint ftsd_wdble_c( fchar fts_header, fchar fts_descriptor, double *value, \
	fchar fts_comment )
{
   char *pfrec;			/* pointer to FITS record */
   char *nextrec; 		/* pointer to next FITS record */
   fint	r = HEADERFUL;		/* return value; default: no room in header */

    pfrec = insert_descriptor (fts_header, fts_descriptor);
   if (pfrec != NULL) {				/* if room insert value */
        r = ret_value(pfrec, fts_header.a);	/* get offset record */
        nextrec = pfrec + FTSRECLEN; 		/* pointer to next record */
    	put_fts_value(pfrec, "= %20.14E ", *value);
    	put_fts_comment(pfrec, fts_comment, nextrec);
   }
   return ( r );
} /* end ftsd_wdble_c */

/*
#>            ftsd_wlog.dc2

Function:     FTSD_WLOG

Purpose:      Writes a FITS logical descriptor in a FITS header.

Category:     FITS

File:         ftsd_access.c

Author:       Sjag Steensma (K.G. Begeman)

Use:          INTEGER FTSD_WLOG( HEADER,      Input     CHARACTER*(*)
                                 DESCRIPTOR,  Input     CHARACTER*(*)
                                 VALUE,       Input     LOGICAL
                                 COMMENT )    Input     CHARACTER*(*)

              FTSD_WLOG     Returns:
                            >=0 : Index of record in header,
                            -4	: No room in header.
              HEADER        Contains FITS header to write descriptor in,
              DESCRIPTOR    FITS descriptor to write in HEADER.
              VALUE         Value for value field of DESCRIPTOR.
              COMMENT	    Comment for commentfield of record.
Updates:
	Aug 14, 1990: Sjag; document created.

#<

Fortran to C interface:

@ integer function ftsd_wlog( character, character, logical, character )

*/

fint ftsd_wlog_c( fchar fts_header, fchar fts_descriptor, bool *value,\
	fchar fts_comment )
{
   char *pfrec;			/* pointer to FITS record */
   char *nextrec; 		/* pointer to next FITS record */
   fint	r = HEADERFUL;		/* return value; default: no room in header */

   pfrec = insert_descriptor (fts_header, fts_descriptor);
   if (pfrec != NULL) {				/* if room insert value */
        r = ret_value(pfrec, fts_header.a);	/* get offset record */
        nextrec = pfrec + FTSRECLEN; 		/* pointer to next record */
    	if ( tobool(*value) ) {
    	    put_fts_value(pfrec, "= %20.1s ", "T");
    	    put_fts_comment(pfrec, fts_comment, nextrec);
        } else {
    	    put_fts_value(pfrec, "= %20.1s ", "F");
    	    put_fts_comment(pfrec, fts_comment, nextrec);
    	}
   }
   return ( r );
} /* end ftsd_wlog_c */

/* end of file ftsd_access.c */
