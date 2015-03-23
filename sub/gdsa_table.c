/* gdsa_table.c

	Copyright (c) Kapteyn Laboratorium Groningen 1992
	All Rights Reserved.


#>            gdsa_table.dc2

Document:     GDSA_TABLE

Purpose:      Describes GDS Table System.

Category:     TABLES
 
File:         gdsa_table.c

Author:       K.G. Begeman

Description:  A table consists of a number of columns which are
              identified by names. Each column can have a number
              of items (called rows) which are all of the same
              data type and in the same (physical) units. The
              items (rows) in a column are stored sequentially in
              a record-like GDS descriptor. The following
              conventions are used:

              1) a table name has a maximum length of eight
              characters
              2) a column name has a maximum length of eight
              characters
              3) a descriptor name "T_tablename_columnname!" holds
              the column header. The column header has three
              variable length records which contain:
                 - data type stored in column (see notes)
                 - comments for the column only
                 - physical units of data in column
              4) a descriptor name "T_tablename_columnname?" holds
              the column data
              5) a descriptor name "T_tablename_????????#" holds
              comments for the whole table

Notes:        1) The column type can be one of the following
              types:
              CHARn   characters, n being the number of characters
                      per item. n cannot be greater then 132.
              INT     integers
              LOG     logicals
              REAL    single precision floating point numbers
              DBLE    double precision floating point numbers

              2) Error codes: Any non-negative number indicates a
              successful completion of the operation.
              Negative values indicate that an error has occured.

              error  routines        meaning
              -66      all       descriptor file not present
              -67   GDSA_CRECOL  illegal data type
                    GDSA_RCxxx   wrong data type in column
                    GDSA_WCxxx   wrong data type in column
              -68   GDSA_RCxxx   reading past end of information
              -69   GDSA_WCxxx   attempt to skip rows in writing
              -70   GDSA_RDCOM   end of information
              -71   GDSA_TABLIS  number of items in list too small
                    GDSA_TABINQ  number of items in list too small

              3) The following routines are now available:

              GDSA_TABINQ   gives information about a GDS table
              GDSA_TABLIS   list all tables present in a GDS
                            descriptor file
              GDSA_DELTAB   deletes a table
              GDSA_COLINQ   gives information about columns in a
                            table
              GDSA_WRCOM    write comments to a GDS table
              GDSA_RDCOM    reads comment from a GDS table
              GDSA_CRECOL   creates a column in a GDS table
              GDSA_DELCOL   deletes a column from a GDS table
              GDSA_WCCHAR   writes character*n items to a column
              GDSA_WCINT     "  "  integer       "    " "   "
              GDSA_WCLOG     "  "  logical       "    " "   "
              GDSA_WCREAL    "  "  real          "    " "   "
              GDSA_WCDBLE    "  "  double        "    " "   "
              GDSA_RCCHAR   reads  character*n items from a column
              GDSA_RCINT     "  "  integer       "     "  "    "
              GDSA_RCLOG     "  "  logical       "     "  "    "
              GDSA_RCREAL    "  "  real          "     "  "    "
              GDSA_RCDBLE    "  "  double        "     "  "    "
              GDSA_ISTABLE  determines whether a GDS descriptor
                            contains table info.

Updates:      Jul 28, 1987: KGB original document created.
              Feb 18, 1989: KGB Some major changes in column types.
              Nov 13, 1990: KGB Converted to C.
              Mar 23, 1994: JPT modified for use with GDS server.

#<
*/

#include	"stdio.h"		/* <stdio.h> */
#include	"ctype.h"		/* <ctype.h> */
#include	"string.h"		/* <string.h> */
#include	"stdlib.h"		/* <strlib.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */

#include	"spfpfl.h"
#include	"spfplf.h"
#include	"dpfpfl.h"
#include	"dpfplf.h"

#include	"gdsparams.h"
#include	"gdserrors.h" 		/* defines error codes */
#include	"gds_ftype.h"		/* defines gds_ftype_c */
#include	"gds_itype.h"		/* defines gds_itype_c */
#include	"gds_exist.h"		/* defines gds_exist_c */
#include	"gdsd_delete.h"		/* defines gdsd_delete_c */
#include	"gdsd_find.h"		/* defines gdsd_find_c */
#include	"gdsd_length.h"		/* defines gdsd_length_c */
#include	"gdsd_read.h"		/* defines gdsd_read_c */
#include	"gdsd_readc.h"		/* defines gdsd_readc_c */
#include	"gdsd_rewind.h"		/* defines gdsd_rewind_c */
#include	"gdsd_rvar.h"		/* defines gdsd_rvar_c */
#include	"gdsd_write.h"		/* defines gdsd_write_c */
#include	"gdsd_writec.h"		/* defines gdsd_writec_c */
#include	"gdsd_wvar.h"		/* defines gdsd_wvar_c */
#include	"gds_lock.h"		/* defines gds_lock_c */
#include	"gds_unlock.h"		/* defines gds_unlock_c */
#include	"nelc.h"		/* defines nelc_c */

#define	EMPTYSTRING	"EMPTY"				/* `empty' string */
#define	RECLEN		132				/* maximum record length */
#define	KEYLEN		(GDS_KEYLEN-1)			/* descriptor key length */
#define COLNAM_L	((KEYLEN-PREFIX_L-2)/2)		/* column name length */
#define	TABNAM_L	(KEYLEN-PREFIX_L-2-COLNAM_L)	/* table name length */
#define PREFIX		"T_"				/* descriptor prefix */
#define	PREFIX_L	2				/* length of prefix */
#define	SEPARATOR	'_'				/* table-column separator */
#define TH_POSTFIX	'#'				/* table-header postfix */
#define	CH_POSTFIX	'!'				/* column-header postfix */
#define CD_POSTFIX	'?'				/* column-data postfix */

#define UNLOCK(set) {fint zero=0; gds_unlock_c( set, &zero);}

#define	fmake(f,c)	{ f.l = sizeof(c); f.a = c; }	/* make fchar from char */

/*
 * fcopy copies a fortran character. It returns 0 when okay, a non-zero number
 * when there is not enough space in dest.
 */
static	int	fcopy( fchar dest, fchar source )
{
   int	i;
   int	l = nelc_c( source );\

   for ( i = 0; i < l && i < dest.l; i++ ) {
      dest.a[i] = source.a[i];
   }
   while ( i < dest.l ) dest.a[i++] = ' ';
   return( l > dest.l ? 1 : 0 ); 
}

static	void	swapfint( fint *in, fint *out, fint nf )
{
   union {
      fint	i;
      char	b[sizeof(fint)];
   } i, o;
   fint	n;

   for (n = 0; n < nf; n++) {
      int	l, m;

      i.i = in[n];
      for (l = sizeof( fint ), m = 0; m < sizeof(fint); o.b[m++] = i.b[--l] );
      out[n] = o.i;
   }
}

static	int	tmatch( fchar dscr, char *tname )
/*
 * tmatch matches a table descriptor with a given table name.
 */
{
   int n;					/* loop counter */
   int r = 1;					/* return value */

   for (n = 0; n < TABNAM_L && r; n++) {	/* compare loop */
      if (tname[n] != dscr.a[n+PREFIX_L]) r = 0;
   }
   return( r );					/* return to caller */
}

static	void	mkdsc( fchar r, fchar tname, fchar cname, char postfix )
/*
 * mkdsc creates a table descriptor out of table name tname and
 * column name cname and postfix character postfix.
 */
{
   char        *prefix = PREFIX;		/* the prefix */
   int         m = 0;
   int         n;				/* loop counter */

   for (n = 0; n < PREFIX_L && m < r.l; n++) {		/* set prefix */
      r.a[m++] = prefix[n];
   }
   for (n = 0; n < tname.l && n < TABNAM_L && m < r.l; n++) {
      r.a[m++] = tname.a[n];			/* copy table name */
   }
   while (n < TABNAM_L && m < r.l) {			/* blank out */
      r.a[m++] = ' ';
      n++;
   }
   if (m < r.l) r.a[m++] = SEPARATOR;		/* put in separator */
   for (n = 0; n < cname.l && n < COLNAM_L && m < r.l; n++) {
      r.a[m++] = cname.a[n];			/* copy column name */
   }
   while (n < COLNAM_L && m < r.l) {
      r.a[m++] = ' ';
      n++;
   }
   if (m < r.l) r.a[m++] = postfix;		/* add postfix character */
}

/*
 * collen determines column width (in bytes) from column type (coltype). It
 * returns 0 in case coltype is unknown.
 */
static fint	collen( fchar coltype )		/* data type of column */
{
   char *ptr;					/* buffer for column type */
   fint  n;					/* loop counter */
   fint  r = 0;					/* return value */

   ptr = zadd( coltype );			/* make it an ASCIIZ string */
   for (n = 0; ptr[n] && ptr[n] != ' '; n++) {	/* conversion loop */
      ptr[n] = toupper( ptr[n] );		/* convert to upper case */
   }
   ptr[n] = 0;					/* add zero byte */
   if (!strcmp( "INT", ptr )) {			/* INTEGER */ 
      r = sizeof( fint );			/* size of integer */
   } else if (!strcmp( "LOG", ptr )) {		/* LOGICAL */
      r = sizeof( bool );			/* size of logical */
   } else if (!strcmp( "REAL", ptr )) {		/* REAL */
      r = sizeof( float );			/* size of real */
   } else if (!strcmp( "DBLE", ptr )) {		/* DOUBLE PRECISION */
      r = sizeof( double );			/* size of double precision */
   } else if (!strncmp( "CHAR", ptr, 4 )) {	/* CHARACTER */
      if (!ptr[4]) {				/* default size */
         r = 1;					/* size of character*1 */
      } else {					/* no default */
         r = atoi( &ptr[4] );			/* size of character*(*) */
      }
      if (r > RECLEN) r = 0;			/* too large */
   }
   free( ptr );					/* free allocated memory */
   return( r );					/* return to caller */
}

/*
#>            gdsa_istable.dc2

Function:     GDSA_ISTABLE

Purpose:      Determines whether a GDS descriptor contains table info.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          INTEGER GDSA_ISTABLE( DESCR )   Input   CHARACTER*(*)

              GDSA_ISTABLE     Returns:
                               0: Does not contain table info.
                               1: Descriptor contains table header.
                               2: Descriptor contains column header.
                               3: Descriptor contains column data.
              DESCR            GDS descriptor to be examined.

Updates:      Sep  1, 1990: KGB Document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ integer function gdsa_istable( character )

*/

fint	gdsa_istable_c( fchar descriptor )	/* GDS descriptor */
{
   fint l;					/* length of descriptor */
   fint r = 0;					/* return value */

   l = nelc_c( descriptor );			/* effective length of descriptor */
   if (l != KEYLEN) {				/* wrong length */
      return( 0 );				/* no table descriptor */
   }
   if (strncmp( PREFIX, descriptor.a, PREFIX_L )) {
      return( 0 );				/* prefix not found */
   }
   if (descriptor.a[PREFIX_L+TABNAM_L] != SEPARATOR) {
      return( 0 );				/* no separator */
   }
   switch( descriptor.a[KEYLEN-1] ) {
      case TH_POSTFIX: r = 1; break;		/* table header */
      case CH_POSTFIX: r = 2; break;		/* column header */
      case CD_POSTFIX: r = 3; break;		/* column data */
      default        : r = 0; break;		/* no table descriptor */
   }
   return( r );					/* return to caller */
}

/*
#>            gdsa_tabinq.dc2

Function:     GDSA_TABINQ

Purpose:      Get information about a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_TABINQ( SET,      Input    CHARACTER*(*)
                                SUBSET,   Input    INTEGER
                                TNAME,    Input    CHARACTER*8
                                CNAMES,   Output   CHARACTER*8 ARRAY
                                NITEMS,   Input    INTEGER
                                NFOUND,   Output   INTEGER
                                ERROR )   Output   INTEGER

              SET         Name of GDS set.
              SUBSET      Subset coordinate word.
              TNAME       Name of GDS table.
              CNAMES      Names of GDS column found.
              NITEMS      Size of CNAMES.
              NFOUND      Number of columns found.
              ERROR       Error return code.

Updates:      Jul  1, 1987: KGB Original document.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@    subroutine gdsa_tabinq( character, integer, character, character,
@                            integer, integer, integer )

*/

void	gdsa_tabinq_c( fchar  set,		/* set name */
                       fint  *subset,		/* subset level */
                       fchar  tname,		/* table name */
                       fchar  cnames,		/* column names */
                       fint  *nitems,		/* maximum number of columns */
                       fint  *nfound,		/* number of colmns found */
                       fint  *error )		/* GDS error */
{
   char  dscb[KEYLEN];				/* descriptor buffer */
   char  name[TABNAM_L+1];			/* table name */
   fchar dsc;					/* points to dsc */
   fint  len = cnames.l;			/* length of column name */
   fint  n;					/* loop counter */
   fint  rcount = 0;				/* record counter */

   dsc.a = dscb; dsc.l = KEYLEN;		/* initialize f character */
   for (n = 0; n < TABNAM_L && n < tname.l; n++) {
      name[n] = tname.a[n];			/* copy table name */
   }
   while (n < TABNAM_L) name[n++] = ' ';	/* blank fill */
   name[TABNAM_L] = 0;				/* add zero byte */
   *nfound = 0;					/* reset number of columns found */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c(set, error);			/* exclusive access */
   do {
						/* find next descriptor */
      gdsd_find_c( dsc, set, subset, &rcount, error );
      if (rcount == 0) {			/* end of search */
         *error = 0;				/* reset error just in case */
         break;					/* leave loop */
      } else if (*error < 0) {			/* GDS error */
         break;					/* leave loop */
      } else if ( (*subset == *error ) &&	/* correct level */
            (gdsa_istable_c( dsc ) == 2) ) {	/* is a column header */
         if (tmatch( dsc, name )) {		/* table name matches */
            if (*nfound == *nitems) {		/* list is full */
               *error = GDS_TABTOOFEW;		/* set error code */
               break;				/* leave loop */
            }
						/* copy loop */
            for (n = 0; n < COLNAM_L && n < len; n++) {
               cnames.a[(*nfound)*len+n] = dsc.a[PREFIX_L+TABNAM_L+1+n];
            }
						/* blank fill */
            while (n < COLNAM_L) cnames.a[(*nfound)*len+n++] = ' ';
            *nfound += 1;			/* number of columns found */
         }
      }
   } while ( 1 );				/* infinite loop */
   UNLOCK( set )				/* allow others to  access */
}

/*
#>            gdsa_tablis.dc2

Function:     GDSA_TABLIS

Purpose:      List all tables present in a descriptor file

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_TABLIS( SET,       Input    CHARACTER*(*)
                                SUBSETS,   Output   INTEGER
                                TNAMES,    Output   CHARACTER*8
                                NITEMS,    Input    INTEGER
                                NFOUND,    Output   INTEGER
                                ERROR )    Output   INTEGER

              SET         Name of GDS set.
              SUBSETS     List of subsets where tables were found.
              TNAMES      List of GDS tables present.
              NITEMS      Size of TNAMES and SUBSETS.
              NFOUND      Number of tables found.
              ERROR       Error return code.

Updates:      Jul  1, 1987: KGB Document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@    subroutine gdsa_tablis( character, integer, character, integer,
@                            integer, integer )

*/

void	gdsa_tablis_c( fchar  set,		/* set name */
                       fint  *subsets,		/* subset list */
                       fchar  tnames,		/* table name list */
                       fint  *nitems,		/* maximum number of items */
                       fint  *nfound,		/* number of tables present */
                       fint  *error )		/* GDS error code */
{
   char  dscb[KEYLEN];				/* buffer for descriptor */
   char  name[TABNAM_L];			/* table name */
   fchar dsc;					/* descriptor */
   fint  ist;					/* returned from gdsa_istable */
   fint  len = tnames.l;			/* maximum length of table names */
   fint  rcount = 0;				/* record number */

   dsc.a = dscb; dsc.l = KEYLEN;		/* initialize descriptor */
   *nfound = 0;					/* reset number of tables */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set GDS error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c(set, error);			/* enter critical section */
   do {						/* loop to find tables */
      gdsd_find_c( dsc, set, NULL, &rcount, error );
      if (rcount == 0) {			/* end of search */
         *error = 0;				/* reset, just in case */
         break;					/* leave loop */
      }
      if (*error < 0) {				/* a GDS error occurred */
         break;					/* leave loop */
      }
      ist = gdsa_istable_c( dsc );		/* type of descriptor */
      if (ist == 1 || ist == 2) {		/* get table name */
         fint f = 1;				/* table name found */
         fint n;				/* loop counter */

         for (n = 0; n < TABNAM_L; n++) {	/* loop */
            name[n] = dsc.a[PREFIX_L+n];	/* copy table name */
         }
         for (n = 0; n < (*nfound) && f; n++) {	/* test whether already found */
            f = strncmp( name, &tnames.a[n*len], (len>TABNAM_L?TABNAM_L:len) );
            if (!f) f = subsets[n] - (*error);
         }
         if (f) {				/* not in list */
            if (*nfound == *nitems) {		/* list is full */
               *error = GDS_TABTOOFEW;		/* GDS error code */
               break;				/* leave loop */
            }
            for (n = 0; n < len && n < TABNAM_L; n++) {
               tnames.a[(*nfound)*len+n] = name[n];
            }
						/* blank fill */
            while (n < len) tnames.a[(*nfound)*len+n++] = ' ';
            subsets[*nfound] = *error;		/* copy subset level */
            *nfound += 1;			/* increase number of tables */
         }
      }
   }
   while ( 1 );					/* infinite loop */
   UNLOCK( set )				/* leave critical section */
}

/*
#>            gdsa_deltab.dc2

Function:     GDSA_DELTAB

Purpose:      Deletes a table (all associated columns).

Category:     TABLES

Files:        gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_DELTAB( SET,        Input     CHARACTER*(*)
                                SUBSET,     Input     INTEGER
                                TNAME,      Input     CHARACTER*8
                                ERROR )     Output    INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be found.
              TNAME       Name of GDS table.
              ERROR       Error return code.

Updates:      Jul  1, 1986: KGB document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_deltab( character, integer, character, integer )

*/

void	gdsa_deltab_c( fchar  set,		/* set name */
                       fint  *subset,		/* level of subset */
                       fchar  tname,		/* name of table */
                       fint  *error )		/* GDS error code */
{
   char   dscb[KEYLEN];				/* descriptor buffer */
   char   name[TABNAM_L];			/* table name */
   char  *tbuf = NULL;				/* dynamic table buffer */
   fchar  dsc;					/* descriptor */
   fint   rcount = 0;				/* record number */
   int    n;					/* loop counter */
   int    nbuf = 0;				/* number of descriptors */

   dsc.a = dscb; dsc.l = KEYLEN;		/* initialize f character */
   for (n = 0; n < TABNAM_L && n < tname.l; n++) {
      name[n] = tname.a[n];			/* copy table name */
   }
   while (n < TABNAM_L) name[n++] = ' ';	/* blank fill */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c(set, error);			/* exclusive access */
   do {
						/* find next descriptor */
      gdsd_find_c( dsc, set, subset, &rcount, error );
      if (rcount == 0) {			/* end of search */
         *error = 0;				/* reset error just in case */
         break;					/* leave loop */
      } else if (*error < 0) {			/* GDS error */
         break;					/* leave loop */
      } else if ( (*subset == *error) &&	/* correct level */
            (gdsa_istable_c( dsc )) ) {		/* is a table descriptor */
         if (tmatch( dsc, name )) {		/* table name matches */
            int n;				/* loop counter */
						/* increase internal buffer */
            tbuf = realloc( tbuf, KEYLEN * (nbuf + 1 ) );
            for (n = 0; n < KEYLEN; n++) {	/* copy to internal buffer */
               tbuf[(nbuf*KEYLEN)+n] = dsc.a[n];
            }
            nbuf += 1;				/* increase number of descriptors */
         }
      }
   } while ( 1 );				/* infinite loop */
   UNLOCK( set )				/* allow others to access */
   if (nbuf && (*error >= 0)) {
      int m, n;					/* loop counters */

      for (n = 0; n < nbuf; n++) {		/* delete loop */
         for (m = 0; m < KEYLEN; m++) {		/* copy loop */
            dsc.a[m] = tbuf[(n*KEYLEN)+m];
         }					/* delete the descriptor */
         gdsd_delete_c( set, dsc, subset, error );
         *error = 0;
      }
      free( tbuf );				/* free allocated memory */
   }
}

/*
#>            gdsa_colinq.dc2

Function:     GDSA_COLINQ

Purpose:      Give information about columns in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_COLINQ( SET,       Input      CHARACTER*(*)
                                SUBSET,    Input      INTEGER
                                TNAME,     Input      CHARACTER*8
                                CNAME,     Input      CHARACTER*8
                                CTYPE,     Output     CHARACTER*(*)
                                CCOMM,     Output     CHARACTER*(*)
                                CUNTS,     Output     CHARACTER*(*)
                                NROWS,     Output     INTEGER
                                ERROR )    Output     INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be found.
              TNAME       Name of GDS table.
              CNAME       Name of GDS column.
              CTYPE       Data type of column items.
              CCOMM       Comments for column.
              CUNTS       Units of data in column.
              NROWS       Number of items in column.
              ERROR       Error return code.

Updates:      Jul 23, 1987: KGB document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_colinq( character, integer, character, character,
@                         character, character, character, integer, integer )

*/

void	gdsa_colinq_c( fchar  set,		/* set name */
                       fint  *subset,		/* level of subset */
                       fchar  tname,		/* name of table */
                       fchar  cname,		/* name of column */
                       fchar  ctype,		/* data type of column */
                       fchar  ccomm,		/* comments for column */
                       fchar  cunts,		/* units of column */
                       fint  *nrows,		/* length of column */
                       fint  *error )		/* GDS error code */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   char  recb[RECLEN];
   fchar dat;					/* data descriptor */
   fchar hed;					/* header descriptor */
   fchar rec;					/* variable record */
   int   fs = 0;				/* fcopy status */

   fmake( dat, datb);
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data descriptor */
   fmake( hed, hedb);
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header descriptor */
   fmake( rec, recb);
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c(set, error);			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind descriptor */
   if (*error >= 0) {				/* no error, continue */
      gdsd_rvar_c( set, hed, subset, rec, error );
      if (*error >= 0) fs += fcopy( ctype, rec );
   }
   if (*error >= 0) {				/* no error, continue */
      gdsd_rvar_c( set, hed, subset, rec, error );
      if (*error >= 0) fs += fcopy( ccomm, rec );
      if (*error == -26) {			/* no comment */
         fint	n;

         *error = 0;
         n = strlen( EMPTYSTRING );
         strncpy( ccomm.a, EMPTYSTRING, n < ccomm.l ? n : ccomm.l );
         while ( n < ccomm.l ) ccomm.a[n++] = ' ';
      }
   }
   if (*error >= 0) {				/* no error, continue */
      gdsd_rvar_c( set, hed, subset, rec, error );
      if (*error >= 0) fs += fcopy( cunts, rec );
      if (*error == -26) {			/* no units */
         fint	n;

         *error = 0;
         n = strlen( EMPTYSTRING );
         strncpy( cunts.a, EMPTYSTRING, n < cunts.l ? n : cunts.l );
         while ( n < cunts.l ) cunts.a[n++] = ' ';
      }
   }
   if (*error >= 0) {				/* no error, continue */
      fint clength;				/* length of column */

      clength = gdsd_length_c( set, dat, subset, error );
      if (*error == -6) {			/* wrong level or error */
         *nrows = 0;				/* no data present */
         *error = 0;				/* reset error */
      } else if (*error >= 0) {			/* no error */
         *nrows = clength / collen( ctype );	/* number of items */
      }
   }
   UNLOCK( set )				/* leave critical section */
   if (*error >= 0 && fs ) {
      *error = -25;
   }
}

/*
#>            gdsa_wrcom.dc2

Function:     GDSA_WRCOM

Purpose:      Write comments to a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_WRCOM( SET,        Input    CHARACTER*(*)
                               SUBSET,     Input    INTEGER
                               TNAME,      Input    CHARACTER*8
                               TCOMM,      Input    CHARACTER*(*)
                               ERROR )     Output   INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be found.
              TNAME       Name of GDS table.
              TCOMM       Comments for GDS table.
              ERROR       Error return code.

Updates:      Jul  1, 1987: KGB Document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@    subroutine gdsa_wrcom( character, integer, character, character,
@                           integer )
*/

void	gdsa_wrcom_c( fchar  set,		/* name of set */
                      fint  *subset,		/* level of subset */
                      fchar  tname,		/* table name */
                      fchar  tabcom,		/* comments for table */
                      fint  *error )		/* GDS error code */
{
   char  cnameb[COLNAM_L];			/* buffer for column name */
   char  hedb[KEYLEN];
   fchar cname;					/* fake column name */
   fchar hed;					/* descriptor name */
   fchar string;				/* modified string */
   fint  n;					/* loop counter */

   for (n = 0; n < COLNAM_L; cnameb[n++] = '?');/* fake column name */
   cname.a = cnameb; cname.l = sizeof( cnameb );/* initialize f character */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, TH_POSTFIX );	/* table header descriptor */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   n = nelc_c( tabcom );
   if (n) {
      string.a = tabcom.a;
      if (n > RECLEN) string.l = RECLEN; else string.l = n;
      gdsd_wvar_c( set, hed, subset, string, error );
      if (*error >= 0 && n > RECLEN) *error = -27;
   }
}

/*
#>            gdsa_rdcom.dc2

Function:     GDSA_RDCOM

Purpose:      Reads comments from a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_RDCOM( SET,          Input    CHARACTER*(*)
                               SUBSET,       Input    INTEGER
                               TNAME,        Input    CHARACTER*8
                               TCOMM,        Output   CHARACTER*(*)
                               ERROR )       Output   INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be found.
              TNAME       Name of GDS table.
              TCOMM       Comments for GDS table.
              ERROR       Error return code.

Updates:      Jul  1, 1987: KGB Document created.
              Nov 13, 1990: KGB Converted to C.
              May  6, 1993: KGB Bug in detection of EOI repaired.

#<

Fortran to C interface:

@ subroutine gdsa_rdcom( character, integer, character, character, integer )

*/

void	gdsa_rdcom_c( fchar  set,		/* name of set */
                      fint  *subset,		/* level of subset */
                      fchar  tname,		/* table name */
                      fchar  tabcom,		/* comments for table */
                      fint  *error )		/* GDS error code */
{
   char  cnameb[COLNAM_L];			/* buffer for column name */
   char  hedb[KEYLEN];
   char  recb[RECLEN];
   fchar cname;					/* fake column name */
   fchar hed;					/* descriptor name */
   fchar rec;					/* variable record */
   fint  n;					/* loop counter */

   fmake( cname, cnameb );
   for (n = 0; n < COLNAM_L; cnameb[n++] = '?');/* make fake column name */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, TH_POSTFIX );	/* table header descriptor */
   fmake( rec, recb );
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gdsd_rvar_c( set, hed, subset, rec, error );
   if (*error >= 0 && fcopy( tabcom, rec )) *error = -25;
   if (*error < 0) {				/* error */
      if (*error == -7) {			/* descriptor not present ? */
         *error = GDS_TABEOI;			/* end of information */
      } else if (*error == -4) {		/* need to rewind descriptor */
         *error = 0;
         gdsd_rewind_c( set, hed, subset, error );
         *error = GDS_TABEOI;			/* end of information */
      } else if (*error == -25) {		/* record length exceeds buffer space */
      
      } else if (*error == -26) {		/* empty string ? */
         fint	n;

         *error = 0;
         n = strlen( EMPTYSTRING );
         strncpy( tabcom.a, EMPTYSTRING, n < tabcom.l ? n : tabcom.l );
         while ( n < tabcom.l ) tabcom.a[n++] = ' ';
      }
   }
}

/*
#>            gdsa_crecol.dc2

Function:     GDSA_CRECOL

Purpose:      Creates a column in a GDS descriptor file.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_CRECOL( SET,          Input     CHARACTER*(*)
                                SUBSET,       Input     INTEGER
                                TNAME,        Input     CHARACTER*8
                                CNAME,        Input     CHARACTER*8
                                CTYPE,        Input     CHARACTER*(*)
                                CCOMM,        Input     CHARACTER*(*)
                                CUNTS,        Input     CHARACTER*(*)
                                ERROR )       Output    INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be created.
              TNAME       Name of GDS table.
              CNAME       Name of GDS column.
              CTYPE       Data type.
              CCOMM       Comments for column.
              CUNTS       Units of data in column.
              ERROR       Error return code.

Updates:      Jul 23, 1987: KGB Document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_crecol( character, integer, character, character,
@                         character, character, character, integer )

*/

void	gdsa_crecol_c( fchar  set,		/* name of set */
                       fint  *subset,		/* level of subset */
                       fchar  tname,		/* table name */
                       fchar  cname,		/* name of column */
                       fchar  ctype,		/* data type of column */
                       fchar  ccomm,		/* comments for column */
                       fchar  cunts,		/* data units of column */
                       fint  *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   fchar dat;					/* data descriptor */
   fchar hed;					/* header descriptor */
   fchar string;				/* modified string */
   fint  cwidth;				/* width of column */
   fint  nel;					/* number of characters */
   int   fs = 0;				/* wvar status */

   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data descriptor */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header descriptor */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   cwidth = collen( ctype );			/* width of column */
   if (cwidth == 0) {				/* unknown column type */
      *error = GDS_TABBADTYPE;			/* set GDS error */
      return;					/* return to caller */
   }
   gds_lock_c(set, error);			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind descriptor */
   if (*error >= 0) {				/* no error, remove header */
      gdsd_delete_c( set, hed, subset, error );	/* remove header */
      gdsd_delete_c( set, dat, subset, error );	/* remove data */
   }
   *error = 0;					/* reset error */
   nel = nelc_c( ctype );
   string.a = ctype.a; string.l = nel;
						/* write CTYPE to column header */
   gdsd_wvar_c( set, hed, subset, string, error );
   if (*error < 0) {				/* GDS error */
     UNLOCK( set )				/* leave critical section */
     return;					/* return to caller */
   }
   nel = nelc_c( ccomm );			/* character count */
   if (nel) {
      string.a = ccomm.a;
      if (nel > RECLEN) {
         string.l = RECLEN;
         fs += 1;
      } else {
         string.l = nel;
      }
   } else {
      string = tofchar( EMPTYSTRING );
   }
						/* write CCOMM to column header */
   gdsd_wvar_c( set, hed, subset, string, error );
   if (*error < 0) {				/* GDS error */
     UNLOCK( set )				/* leave critical section */
     return;					/* return to caller */
   }
   nel = nelc_c( cunts );			/* character count */
   if (nel) {
      string.a = cunts.a;
      if (nel > RECLEN) {
         string.l = RECLEN;
         fs += 1;
      } else {
         string.l = nel;
      }
   } else {
      string = tofchar( EMPTYSTRING );
   }
						/* write CUNTS to column header */
   gdsd_wvar_c( set, hed, subset, string, error );
   if (*error >= 0 && fs) {			/* GDS error */
      *error = -27;
   }
   UNLOCK( set )				/* leave critical section */
}

/*
#>            gdsa_delcol.dc2

Function:     GDSA_DELCOL

Purpose:      Deletes a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_DELCOL( SET,        Input     CHARACTER*(*)
                                SUBSET,     Input     INTEGER
                                TNAME,      Input     CHARACTER*8
                                CNAME,      Input     CHARACTER*8
                                ERROR )     Output    INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be deleted.
              TNAME       Name of GDS table.
              CNAME       Name of GDS column.
              ERROR       Error return code.

Updates:      Jul 20, 1987: KGB Document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_delcol( character, integer, character, character, integer )

*/

void	gdsa_delcol_c( fchar  set,		/* name of set */
                       fint  *subset,		/* level of subset */
                       fchar  tname,		/* name of table */
                       fchar  cname,		/* name of column */
                       fint  *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */

   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* make column data dsc. */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* make column header dsc. */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gdsd_delete_c( set, hed, subset, error );	/* delete column header */
   *error = 0;
   gdsd_delete_c( set, dat, subset, error );	/* delete column data */
}

/*
#>            gdsa_rcchar.dc2

Function:     GDSA_RCCHAR

Purpose:      Reads character items from a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_RCCHAR( SET,      Input    CHARACTER*(*)
                                SUBSET,   Input    INTEGER
                                TNAME,    Input    CHARACTER*8
                                CNAME,    Input    CHARACTER*8
                                CDATA,    Output   CHARACTER ARRAY
                                ITEM,     Input    INTEGER
                                NITEMS,   Input    INTEGER
                                ERROR )   Output   INTEGER

              SET         Name of GDS set
              SUBSET      Subset where table is to be found
              TNAME       Name of table
              CNAME       Name of column
              CDATA       Array to recieve the data from column.
              ITEM        Row number where to start reading
              NITEMS      Number of rows to read
              ERROR       Error return code

Updates:      Feb 18, 1989: KGB  document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_rcchar( character, integer, character, character,
@                         character, integer, integer, integer )

*/

void	gdsa_rcchar_c( fchar  set,		/* name of set */
                       fint  *subset,		/* level of subset */
                       fchar  tname,		/* name of table */
                       fchar  cname,		/* name of column */
                       fchar  cdata,		/* column data */
                       fint  *first,		/* first row */
                       fint  *items,		/* number of rows */
                       fint  *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   char  recordb[RECLEN];			/* buffer for header info */
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */
   fchar record;				/* header info */
   fint  cbytes;				/* total number of bytes */
   fint  cdone;					/* bytes read */
   fint  cfirst;				/* first byte */
   fint  clast;					/* last byte */
   fint  clength;				/* length of column */
   fint  cwidth;				/* width of column */
   fint  n;					/* loop counter */

   fmake( record, recordb );
   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data dsc */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header dsc */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c(set, error);			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind table header */
   if (*error < 0) {
      UNLOCK( set )			/* leave critical section */
      return;					/* return to caller */
   }
						/* read first record of table header */
   gdsd_rvar_c( set, hed, subset, record, error );
   UNLOCK( set )				/* leave critical section */
   if (*error < 0) {				/* error */
      return;					/* return to caller */
   }
   for (n = 0; isalpha( record.a[n] ); n++) {	/* loop */
      record.a[n] = toupper( record.a[n] );	/* to uppercase */
   }
   if (strncmp( record.a, "CHAR", 4 )) {	/* wrong type */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
   cwidth = collen( record );			/* find width of column */
						/* find length of descriptor item */
   clength = gdsd_length_c( set, dat, subset, error );
   if (*error < 0) return;			/* GDS error */
   cfirst = (*first - 1) * cwidth + 1;		/* position of first byte to read */
   cbytes = (*items) * cwidth;			/* total number of bytes */
   clast  = cfirst - 1 + cbytes;		/* last byte to read */
   if (clast > clength) {			/* not enough bytes in column */
      *error = GDS_TABPASTEOI;			/* GDS error */
      return;					/* return to caller */
   }
   if (cwidth == cdata.l) {			/* in one go */
						/* read column data */
      gdsd_read_c( set, dat, subset, (fint *) cdata.a, &cbytes, &cfirst, &cdone, error );
   } else {					/* one by one */
      fint l = cdata.l;				/* length of character string */
      fint k = 0, m, n;				/* loop counters */

      for (n = cfirst; n <= clast; n += cwidth) {/* read loop */
						/* read column data */
         gdsd_read_c( set, dat, subset, (fint *) record.a, &cwidth, &n, &cdone, error );
         if (*error < 0) break;			/* GDS error */
         for (m = 0; m < l && m < cwidth; m++) {/* copy loop */
            cdata.a[k+m] = record.a[m];		/* copy */
         }
         while (m < l) cdata.a[k+m++] = ' ';	/* blank fill */
         k += l;				/* start of next item */
      }
   }
}

/*
#>            gdsa_rcint.dc2

Function:     GDSA_RCINT

Purpose:      Reads integer items from a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_RCINT( SET,      Input    CHARACTER*(*)
                               SUBSET,   Input    INTEGER
                               TNAME,    Input    CHARACTER*8
                               CNAME,    Input    CHARACTER*8
                               IDATA,    Output   INTEGER ARRAY
                               ITEM,     Input    INTEGER
                               NITEMS,   Input    INTEGER
                               ERROR )   Output   INTEGER

              SET         Name of GDS set
              SUBSET      Subset where table is to be found
              TNAME       Name of table
              CNAME       Name of column
              IDATA       Array to recieve the data from column.
              ITEM        Row number where to start reading
              NITEMS      Number of rows to read
              ERROR       Error return code

Updates:      Feb 18, 1989: KGB  document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_rcint( character, integer, character, character,
@                        integer, integer, integer, integer )

*/

void	gdsa_rcint_c( fchar  set,		/* name of set */
                      fint  *subset,		/* level of subset */
                      fchar  tname,		/* name of table */
                      fchar  cname,		/* name of column */
                      fint  *idata,		/* column data */
                      fint  *first,		/* first row */
                      fint  *items,		/* number of rows */
                      fint  *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN]; 
   char  recordb[RECLEN];			/* buffer for header info */
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */
   fchar record;				/* header info */
   fint  cbytes;				/* total number of bytes */
   fint  cdone;					/* bytes read */
   fint  cfirst;				/* first byte */
   fint  clast;					/* last byte */
   fint  clength;				/* length of column */
   fint  cwidth;				/* width of column */
   fint  n;					/* loop counter */

   fmake( record, recordb );
   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data dsc */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header dsc */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c( set, error );			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind table header */
   if (*error < 0) {
      UNLOCK( set )			/* leave critical section */
      return;					/* return to caller */
   }
						/* read first record of table header */
   gdsd_rvar_c( set, hed, subset, record, error );
   UNLOCK( set )				/* leave critical section */
   if (*error < 0) {				/* error */
      return;					/* return to caller */
   }
   for (n = 0; isalpha( record.a[n] ); n++) {	/* loop */
      record.a[n] = toupper( record.a[n] );	/* to uppercase */
   }
   if (strncmp( record.a, "INT", 3 )) {		/* wrong type */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
   cwidth = collen( record );			/* find width of column */
   if (cwidth != sizeof( fint )) {		/* error */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
						/* find length of descriptor item */
   clength = gdsd_length_c( set, dat, subset, error );
   if (*error < 0) return;			/* GDS error */
   cfirst = (*first - 1) * cwidth + 1;		/* position of first byte to read */
   cbytes = (*items) * cwidth;			/* total number of bytes */
   clast  = cfirst - 1 + cbytes;		/* last byte to read */
   if (clast > clength) {			/* not enough bytes in column */
      *error = GDS_TABPASTEOI;			/* GDS error */
      return;					/* return to caller */
   }
						/* read column data */
   gdsd_read_c( set, dat, subset, idata, &cbytes, &cfirst, &cdone, error );
   {
      fint	nmax = cdone / sizeof( fint );

      if (gds_itype_c(set,error)!=OS_INTEGER_TYPE) {
         swapfint( idata, idata, nmax );
      }
   }
}

/*
#>            gdsa_rclog.dc2

Function:     GDSA_RCLOG

Purpose:      Reads logical items from a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_RCLOG( SET,      Input    CHARACTER*(*)
                               SUBSET,   Input    INTEGER
                               TNAME,    Input    CHARACTER*8
                               CNAME,    Input    CHARACTER*8
                               LDATA,    Output   LOGICAL ARRAY
                               ITEM,     Input    INTEGER
                               NITEMS,   Input    INTEGER
                               ERROR )   Output   INTEGER

              SET         Name of GDS set
              SUBSET      Subset where table is to be found
              TNAME       Name of table
              CNAME       Name of column
              LDATA       Array to recieve the data from column.
              ITEM        Row number where to start reading
              NITEMS      Number of rows to read
              ERROR       Error return code

Updates:      Feb 18, 1989: KGB  document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_rclog( character, integer, character, character,
@                        logical, integer, integer, integer )

*/

void	gdsa_rclog_c( fchar  set,		/* name of set */
                      fint  *subset,		/* level of subset */
                      fchar  tname,		/* name of table */
                      fchar  cname,		/* name of column */
                      bool  *ldata,		/* column data */
                      fint  *first,		/* first row */
                      fint  *items,		/* number of rows */
                      fint  *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   char  recordb[RECLEN];			/* buffer for header info */
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */
   fchar record;				/* header info */
   fint  cbytes;				/* total number of bytes */
   fint  cdone;					/* bytes read */
   fint  cfirst;				/* first byte */
   fint  clast;					/* last byte */
   fint  clength;				/* length of column */
   fint  cwidth;				/* width of column */
   fint  n;					/* loop counter */

   fmake( record, recordb );
   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data dsc */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header dsc */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c( set, error );			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind table header */
   if (*error < 0) {
      UNLOCK( set )			/* leave critical section */
      return;					/* return to caller */
   }
						/* read first record of table header */
   gdsd_rvar_c( set, hed, subset, record, error );
   UNLOCK( set )				/* leave critical section */
   if (*error < 0) {				/* error */
      return;					/* return to caller */
   }
   for (n = 0; isalpha( record.a[n] ); n++) {	/* loop */
      record.a[n] = toupper( record.a[n] );	/* to uppercase */
   }
   if (strncmp( record.a, "LOG", 3 )) {		/* wrong type */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
   cwidth = collen( record );			/* find width of column */
   if (cwidth != sizeof( bool )) {		/* error */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
						/* find length of descriptor item */
   clength = gdsd_length_c( set, dat, subset, error );
   if (*error < 0) return;			/* GDS error */
   cfirst = (*first - 1) * cwidth + 1;		/* position of first byte to read */
   cbytes = (*items) * cwidth;			/* total number of bytes */
   clast  = cfirst - 1 + cbytes;		/* last byte to read */
   if (clast > clength) {			/* not enough bytes in column */
      *error = GDS_TABPASTEOI;			/* GDS error */
      return;					/* return to caller */
   }
						/* read column data */
   gdsd_read_c( set, dat, subset, (fint *) ldata, &cbytes, &cfirst, &cdone, error );
   {
      fint	n, nmax = cdone / sizeof( bool );

      for (n = 0; n < nmax; n++) {
         ldata[n] = toflog( ldata[n] );
      }
   }
}

/*
#>            gdsa_rcreal.dc2

Function:     GDSA_RCREAL

Purpose:      Reads real items from a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_RCREAL( SET,      Input    CHARACTER*(*)
                                SUBSET,   Input    INTEGER
                                TNAME,    Input    CHARACTER*8
                                CNAME,    Input    CHARACTER*8
                                RDATA,    Output   REAL ARRAY
                                ITEM,     Input    INTEGER
                                NITEMS,   Input    INTEGER
                                ERROR )   Output   INTEGER

              SET         Name of GDS set
              SUBSET      Subset where table is to be found
              TNAME       Name of table
              CNAME       Name of column
              RDATA       Array to recieve the data from column.
              ITEM        Row number where to start reading
              NITEMS      Number of rows to read
              ERROR       Error return code

Updates:      Feb 18, 1989: KGB  document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_rcreal( character, integer, character, character,
@                         real, integer, integer, integer )

*/

void	gdsa_rcreal_c( fchar  set,		/* name of set */
                       fint  *subset,		/* level of subset */
                       fchar  tname,		/* name of table */
                       fchar  cname,		/* name of column */
                       float *rdata,		/* column data */
                       fint  *first,		/* first row */
                       fint  *items,		/* number of rows */
                       fint  *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   char  recordb[RECLEN];			/* buffer for header info */
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */
   fchar record;				/* header info */
   fint  cbytes;				/* total number of bytes */
   fint  cdone;					/* bytes read */
   fint  cfirst;				/* first byte */
   fint  clast;					/* last byte */
   fint  clength;				/* length of column */
   fint  cwidth;				/* width of column */
   fint  n;					/* loop counter */

   fmake( record, recordb );
   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data dsc */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header dsc */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c( set, error );			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind table header */
   if (*error < 0) {
      UNLOCK( set )			/* leave critical section */
      return;					/* return to caller */
   }
						/* read first record of table header */
   gdsd_rvar_c( set, hed, subset, record, error );
   UNLOCK( set )				/* leave critical section */
   if (*error < 0) {				/* error */
      return;					/* return to caller */
   }
   for (n = 0; isalpha( record.a[n] ); n++) {	/* loop */
      record.a[n] = toupper( record.a[n] );	/* to uppercase */
   }
   if (strncmp( record.a, "REAL", 4 )) {	/* wrong type */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
   cwidth = collen( record );			/* find width of column */
   if (cwidth != sizeof( float )) {		/* error */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
						/* find length of descriptor item */
   clength = gdsd_length_c( set, dat, subset, error );
   if (*error < 0) return;			/* GDS error */
   cfirst = (*first - 1) * cwidth + 1;		/* position of first byte to read */
   cbytes = (*items) * cwidth;			/* total number of bytes */
   clast  = cfirst - 1 + cbytes;		/* last byte to read */
   if (clast > clength) {			/* not enough bytes in column */
      *error = GDS_TABPASTEOI;			/* GDS error */
      return;					/* return to caller */
   }
						/* read column data */
   gdsd_read_c( set, dat, subset, (fint *) rdata, &cbytes, &cfirst, &cdone, error );
   {
      fint	nmax = cdone / sizeof( float );
      fint	ftype;

      ftype = gds_ftype_c( set, error );
      (void) spfpfl_c( &ftype, rdata, rdata, &nmax );
   }
}

/*
#>            gdsa_rcdble.dc2

Function:     GDSA_RCDBLE

Purpose:      Reads double precision items from a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_RCDBLE( SET,      Input    CHARACTER*(*)
                                SUBSET,   Input    INTEGER
                                TNAME,    Input    CHARACTER*8
                                CNAME,    Input    CHARACTER*8
                                DDATA,    Output   DOUBLE PRECISION ARRAY
                                ITEM,     Input    INTEGER
                                NITEMS,   Input    INTEGER
                                ERROR )   Output   INTEGER

              SET         Name of GDS set
              SUBSET      Subset where table is to be found
              TNAME       Name of table
              CNAME       Name of column
              DDATA       Array to recieve the data from column.
              ITEM        Row number where to start reading
              NITEMS      Number of rows to read
              ERROR       Error return code

Updates:      Feb 18, 1989: KGB  document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_rcdble( character, integer, character, character,
@                         double precision, integer, integer, integer )

*/

void	gdsa_rcdble_c( fchar   set,		/* name of set */
                       fint   *subset,		/* level of subset */
                       fchar   tname,		/* name of table */
                       fchar   cname,		/* name of column */
                       double *ddata,		/* column data */
                       fint   *first,		/* first row */
                       fint   *items,		/* number of rows */
                       fint   *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   char  recordb[RECLEN];			/* buffer for header info */
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */
   fchar record;				/* header info */
   fint  cbytes;				/* total number of bytes */
   fint  cdone;					/* bytes read */
   fint  cfirst;				/* first byte */
   fint  clast;					/* last byte */
   fint  clength;				/* length of column */
   fint  cwidth;				/* width of column */
   fint  n;					/* loop counter */

   fmake( record, recordb );
   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data dsc */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header dsc */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c( set, error );			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind table header */
   if (*error < 0) {
      UNLOCK( set )			/* leave critical section */
      return;					/* return to caller */
   }
						/* read first record of table header */
   gdsd_rvar_c( set, hed, subset, record, error );
   UNLOCK( set )				/* leave critical section */
   if (*error < 0) {				/* error */
      return;					/* return to caller */
   }
   for (n = 0; isalpha( record.a[n] ); n++) {	/* loop */
      record.a[n] = toupper( record.a[n] );	/* to uppercase */
   }
   if (strncmp( record.a, "DBLE", 4 )) {	/* wrong type */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
   cwidth = collen( record );			/* find width of column */
   if (cwidth != sizeof( double )) {		/* error */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
						/* find length of descriptor item */
   clength = gdsd_length_c( set, dat, subset, error );
   if (*error < 0) return;			/* GDS error */
   cfirst = (*first - 1) * cwidth + 1;		/* position of first byte to read */
   cbytes = (*items) * cwidth;			/* total number of bytes */
   clast  = cfirst - 1 + cbytes;		/* last byte to read */
   if (clast > clength) {			/* not enough bytes in column */
      *error = GDS_TABPASTEOI;			/* GDS error */
      return;					/* return to caller */
   }
						/* read column data */
   gdsd_read_c( set, dat, subset, (fint *) ddata, &cbytes, &cfirst, &cdone, error );
   if (*error<0) return;
   {
      fint	nmax = cdone / sizeof( double );
      fint	ftype;

      ftype = gds_ftype_c(set, error);
      (void) dpfpfl_c( &ftype, ddata, ddata, &nmax );
   }
}

/*
#>            gdsa_wcchar.dc2

Function:     GDSA_WCCHAR

Purpose:      Write character items to a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_WCCHAR( SET,      Input    CHARACTER*(*)
                                SUBSET,   Input    INTEGER
                                TNAME,    Input    CHARACTER*8
                                CNAME,    Input    CHARACTER*8
                                CDATA,    Input    CHARACTER ARRAY
                                ITEM,     Input    INTEGER
                                NITEMS,   Input    INTEGER
                                ERROR )   Output   INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be created.
              TNAME       Name of GDS table.
              CNAME       Name of GDS column.
              CDATA       Array containing the data to be written
              ITEM        Row number where to start writing
                          IF zero data will be added at the end of
                          the column.
              NITEMS      Number of rows to write.
              ERROR       Error return code.

Updates:      Feb 18, 1989: KGB Document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_wcchar( character, integer, character, character,
@                         character, integer, integer, integer )

*/

void	gdsa_wcchar_c( fchar  set,		/* name of set */
                       fint  *subset,		/* level of subset */
                       fchar  tname,		/* name of table */
                       fchar  cname,		/* name of column */
                       fchar  cdata,		/* column data */
                       fint  *first,		/* first row */
                       fint  *items,		/* number of rows */
                       fint  *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   char  recordb[RECLEN];			/* buffer for header info */
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */
   fchar record;				/* header info */
   fint  cbytes;				/* total number of bytes */
   fint  cdone;					/* bytes read */
   fint  cfirst;				/* first byte */
   fint  clast;					/* last byte */
   fint  clength;				/* length of column */
   fint  cwidth;				/* width of column */
   fint  n;					/* loop counter */

   fmake( record, recordb );
   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data dsc */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header dsc */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c( set, error );			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind table header */
   if (*error < 0) {
      UNLOCK( set )			/* leave critical section */
      return;					/* return to caller */
   }
						/* read first record of table header */
   gdsd_rvar_c( set, hed, subset, record, error );
   UNLOCK( set )				/* leave critical section */
   if (*error < 0) {				/* error */
      return;					/* return to caller */
   }
   for (n = 0; isalpha( record.a[n] ); n++) {	/* loop */
      record.a[n] = toupper( record.a[n] );	/* to uppercase */
   }
   if (strncmp( record.a, "CHAR", 4 )) {	/* wrong type */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
   cwidth = collen( record );			/* find width of column */
						/* find length of descriptor item */
   clength = gdsd_length_c( set, dat, subset, error );
   if (*error < 0) {				/* GDS error */
      *error = 0;				/* reset error */
      clength = 0;
   }
   if (*first != 0) {				/* first to write */
      cfirst = (*first - 1) * cwidth + 1;	/* start here */
   } else {					/* append */
      cfirst = clength + 1;			/* start here */
   }
   cbytes = (*items) * cwidth;			/* total number of bytes */
   clast  = cfirst - 1 + cbytes;		/* last byte to read */
   if ((cfirst - 1) > clength) {		/* holes in column ? */
      *error = GDS_TABSKIPROW;			/* GDS error */
      return;					/* return to caller */
   }
   if (cwidth == cdata.l) {			/* in one go */
						/* write column data */
      gdsd_write_c( set, dat, subset, (fint *) cdata.a, &cbytes, &cfirst, &cdone, error );
   } else {					/* one by one */
      fint l = cdata.l;				/* length of character string */
      fint k = 0, m, n;				/* loop counters */

      for (n = cfirst; n <= clast; n += cwidth) {/* write loop */
         for (m = 0; m < l && m < cwidth; m++) {/* copy loop */
            record.a[m] = cdata.a[k+m];		/* copy */
         }
         k += l;				/* start of next item */
         while (m < cwidth) record.a[m++] = ' ';/* blank fill */
						/* write column data */
         gdsd_write_c( set, dat, subset, (fint *) record.a, &cwidth, &n, &cdone, error );
         if (*error < 0) break;			/* GDS error */
      }
   }
}

/*
#>            gdsa_wcint.dc2

Function:     GDSA_WCINT

Purpose:      Write integer items to a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_WCINT( SET,      Input    CHARACTER*(*)
                               SUBSET,   Input    INTEGER
                               TNAME,    Input    CHARACTER*8
                               CNAME,    Input    CHARACTER*8
                               IDATA,    Input    INTEGER ARRAY
                               ITEM,     Input    INTEGER
                               NITEMS,   Input    INTEGER
                               ERROR )   Output   INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be created.
              TNAME       Name of GDS table.
              CNAME       Name of GDS column.
              IDATA       Array containing the data to be written
              ITEM        Row number where to start writing
                          IF zero data will be added at the end of
                          the column.
              NITEMS      Number of rows to write.
              ERROR       Error return code.

Updates:      Feb 18, 1989: KGB Document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_wcint( character, integer, character, character,
@                        integer, integer, integer, integer )

*/

void	gdsa_wcint_c( fchar  set,		/* name of set */
                      fint  *subset,		/* level of subset */
                      fchar  tname,		/* name of table */
                      fchar  cname,		/* name of column */
                      fint  *idata,		/* column data */
                      fint  *first,		/* first row */
                      fint  *items,		/* number of rows */
                      fint  *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   char  recordb[RECLEN];			/* buffer for header info */
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */
   fchar record;				/* header info */
   fint  cbytes;				/* total number of bytes */
   fint  cdone;					/* bytes read */
   fint  cfirst;				/* first byte */
   fint  clast;					/* last byte */
   fint  clength;				/* length of column */
   fint  cwidth;				/* width of column */
   fint  n;					/* loop counter */

   fmake( record, recordb );
   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data dsc */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header dsc */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c( set, error );			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind table header */
   if (*error < 0) {
      UNLOCK( set )			/* leave critical section */
      return;					/* return to caller */
   }
						/* read first record of table header */
   gdsd_rvar_c( set, hed, subset, record, error );
   UNLOCK( set )				/* leave critical section */
   if (*error < 0) {				/* error */
      return;					/* return to caller */
   }
   for (n = 0; isalpha( record.a[n] ); n++) {	/* loop */
      record.a[n] = toupper( record.a[n] );	/* to uppercase */
   }
   if (strncmp( record.a, "INT", 3 )) {		/* wrong type */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
   cwidth = collen( record );			/* find width of column */
   if (cwidth != sizeof( fint )) {		/* error */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
						/* find length of descriptor item */
   clength = gdsd_length_c( set, dat, subset, error );
   if (*error < 0) {
      *error = 0;
      clength = 0;
   }
   if (*first != 0) {				/* first to write */
      cfirst = (*first - 1) * cwidth + 1;	/* start here */
   } else {					/* append */
      cfirst = clength + 1;			/* start here */
   }
   cbytes = (*items) * cwidth;			/* total number of bytes */
   clast  = cfirst - 1 + cbytes;		/* last byte to read */
   if ((cfirst - 1) > clength) {		/* holes in column ? */
      *error = GDS_TABSKIPROW;			/* GDS error */
      return;					/* return to caller */
   }
						/* write column data */
   {
      fint	nmax = cbytes / sizeof( fint );

      if (gds_itype_c(set, error) == OS_INTEGER_TYPE) {
         gdsd_write_c( set, dat, subset, idata, &cbytes, &cfirst, &cdone, error );
      } else {
         fint	*ibuff;

         ibuff = calloc( nmax, sizeof( fint ) );
         swapfint( idata, ibuff, nmax );
         gdsd_write_c( set, dat, subset, ibuff, &cbytes, &cfirst, &cdone, error );
         free( ibuff );
      }
   }
}

/*
#>            gdsa_wclog.dc2

Function:     GDSA_WCLOG

Purpose:      Write logical items to a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_WCLOG( SET,      Input    CHARACTER*(*)
                               SUBSET,   Input    INTEGER
                               TNAME,    Input    CHARACTER*8
                               CNAME,    Input    CHARACTER*8
                               LDATA,    Input    LOGICAL ARRAY
                               ITEM,     Input    INTEGER
                               NITEMS,   Input    INTEGER
                               ERROR )   Output   INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be created.
              TNAME       Name of GDS table.
              CNAME       Name of GDS column.
              LDATA       Array containing the data to be written
              ITEM        Row number where to start writing
                          IF zero data will be added at the end of
                          the column.
              NITEMS      Number of rows to write.
              ERROR       Error return code.

Updates:      Feb 18, 1989: KGB Document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_wclog( character, integer, character, character,
@                        logical, integer, integer, integer )

*/

void	gdsa_wclog_c( fchar  set,		/* name of set */
                      fint  *subset,		/* level of subset */
                      fchar  tname,		/* name of table */
                      fchar  cname,		/* name of column */
                      bool  *ldata,		/* column data */
                      fint  *first,		/* first row */
                      fint  *items,		/* number of rows */
                      fint  *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   char  recordb[RECLEN];			/* buffer for header info */
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */
   fchar record;				/* header info */
   fint  cbytes;				/* total number of bytes */
   fint  cdone;					/* bytes read */
   fint  cfirst;				/* first byte */
   fint  clast;					/* last byte */
   fint  clength;				/* length of column */
   fint  cwidth;				/* width of column */
   fint  n;					/* loop counter */

   fmake( record, recordb );
   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data dsc */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header dsc */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c( set, error );			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind table header */
   if (*error < 0) {
      UNLOCK( set )			/* leave critical section */
      return;					/* return to caller */
   }
						/* read first record of table header */
   gdsd_rvar_c( set, hed, subset, record, error );
   UNLOCK( set )				/* leave critical section */
   if (*error < 0) {				/* error */
      return;					/* return to caller */
   }
   for (n = 0; isalpha( record.a[n] ); n++) {	/* loop */
      record.a[n] = toupper( record.a[n] );	/* to uppercase */
   }
   if (strncmp( record.a, "LOG", 3 )) {		/* wrong type */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
   cwidth = collen( record );			/* find width of column */
   if (cwidth != sizeof( bool )) {		/* error */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
						/* find length of descriptor item */
   clength = gdsd_length_c( set, dat, subset, error );
   if (*error < 0) {
      *error = 0;
      clength = 0;
   }
   if (*first != 0) {				/* first to write */
      cfirst = (*first - 1) * cwidth + 1;	/* start here */
   } else {					/* append */
      cfirst = clength + 1;			/* start here */
   }
   cbytes = (*items) * cwidth;			/* total number of bytes */
   clast  = cfirst - 1 + cbytes;		/* last byte to read */
   if ((cfirst - 1) > clength) {		/* holes in column ? */
      *error = GDS_TABSKIPROW;			/* GDS error */
      return;					/* return to caller */
   }
						/* write column data */
   {
      fint	n, nmax = cbytes / sizeof( bool );

      for (n = 0; n < nmax; n++) {
         ldata[n] = tobool( ldata[n] );
      }
      gdsd_write_c( set, dat, subset, (fint *) ldata, &cbytes, &cfirst, &cdone, error );
      for (n = 0; n < nmax; n++) {
         ldata[n] = toflog( ldata[n] );
      }
   }
}

/*
#>            gdsa_wcreal.dc2

Function:     GDSA_WCREAL

Purpose:      Write real items to a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_WCREAL( SET,      Input    CHARACTER*(*)
                                SUBSET,   Input    INTEGER
                                TNAME,    Input    CHARACTER*8
                                CNAME,    Input    CHARACTER*8
                                RDATA,    Input    REAL ARRAY
                                ITEM,     Input    INTEGER
                                NITEMS,   Input    INTEGER
                                ERROR )   Output   INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be created.
              TNAME       Name of GDS table.
              CNAME       Name of GDS column.
              RDATA       Array containing the data to be written
              ITEM        Row number where to start writing
                          IF zero data will be added at the end of
                          the column.
              NITEMS      Number of rows to write.
              ERROR       Error return code.

Updates:      Feb 18, 1989: KGB Document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_wcreal( character, integer, character, character,
@                         real, integer, integer, integer )

*/

void	gdsa_wcreal_c( fchar  set,		/* name of set */
                       fint  *subset,		/* level of subset */
                       fchar  tname,		/* name of table */
                       fchar  cname,		/* name of column */
                       float *rdata,		/* column data */
                       fint  *first,		/* first row */
                       fint  *items,		/* number of rows */
                       fint  *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   char  recordb[RECLEN];			/* buffer for header info */
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */
   fchar record;				/* header info */
   fint  cbytes;				/* total number of bytes */
   fint  cdone;					/* bytes read */
   fint  cfirst;				/* first byte */
   fint  clast;					/* last byte */
   fint  clength;				/* length of column */
   fint  cwidth;				/* width of column */
   fint  n;					/* loop counter */

   fmake( record, recordb );
   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data dsc */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header dsc */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c( set, error );			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind table header */
   if (*error < 0) {
      UNLOCK( set )			/* leave critical section */
      return;					/* return to caller */
   }
						/* read first record of table header */
   gdsd_rvar_c( set, hed, subset, record, error );
   UNLOCK( set )				/* leave critical section */
   if (*error < 0) {				/* error */
      return;					/* return to caller */
   }
   for (n = 0; isalpha( record.a[n] ); n++) {	/* loop */
      record.a[n] = toupper( record.a[n] );	/* to uppercase */
   }
   if (strncmp( record.a, "REAL", 4 )) {	/* wrong type */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
   cwidth = collen( record );			/* find width of column */
   if (cwidth != sizeof( float )) {		/* error */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
						/* find length of descriptor item */
   clength = gdsd_length_c( set, dat, subset, error );
   if (*error < 0) {
      *error = 0;
      clength = 0;
   }
   if (*first != 0) {				/* first to write */
      cfirst = (*first - 1) * cwidth + 1;	/* start here */
   } else {					/* append */
      cfirst = clength + 1;			/* start here */
   }
   cbytes = (*items) * cwidth;			/* total number of bytes */
   clast  = cfirst - 1 + cbytes;		/* last byte to read */
   if ((cfirst - 1) > clength) {		/* holes in column ? */
      *error = GDS_TABSKIPROW;			/* GDS error */
      return;					/* return to caller */
   }
						/* write column data */
   {
      fint	ftype;
      fint	nmax = cbytes / sizeof( float );

      ftype = gds_ftype_c(set, error);
      if (ftype == OS_FLOATING_TYPE) {
         gdsd_write_c( set, dat, subset, (fint *) rdata, &cbytes, &cfirst, &cdone, error );
      } else {
         float	*rbuff;

         rbuff = calloc( nmax, sizeof( float ) );
         (void) spfplf_c( &ftype, rdata, rbuff, &nmax );
         gdsd_write_c( set, dat, subset, (fint *) rbuff, &cbytes, &cfirst, &cdone, error );
         free( rbuff );
      }
   }
}

/*
#>            gdsa_wcdble.dc2

Function:     GDSA_WCDBLE

Purpose:      Write double precision items to a column in a GDS table.

Category:     TABLES

File:         gdsa_table.c

Author:       K.G. Begeman

Use:          CALL GDSA_WCDBLE( SET,      Input    CHARACTER*(*)
                                SUBSET,   Input    INTEGER
                                TNAME,    Input    CHARACTER*8
                                CNAME,    Input    CHARACTER*8
                                DDATA,    Input    DOUBLE ARRAY
                                ITEM,     Input    INTEGER
                                NITEMS,   Input    INTEGER
                                ERROR )   Output   INTEGER

              SET         Name of GDS set.
              SUBSET      Subset where table is to be created.
              TNAME       Name of GDS table.
              CNAME       Name of GDS column.
              DDATA       Array containing the data to be written
              ITEM        Row number where to start writing
                          IF zero data will be added at the end of
                          the column.
              NITEMS      Number of rows to write.
              ERROR       Error return code.

Updates:      Feb 18, 1989: KGB Document created.
              Nov 13, 1990: KGB Converted to C.

#<

Fortran to C interface:

@ subroutine gdsa_wcdble( character, integer, character, character,
@                         double precision, integer, integer, integer )

*/

void	gdsa_wcdble_c( fchar   set,		/* name of set */
                       fint   *subset,		/* level of subset */
                       fchar   tname,		/* name of table */
                       fchar   cname,		/* name of column */
                       double *ddata,		/* column data */
                       fint   *first,		/* first row */
                       fint   *items,		/* number of rows */
                       fint   *error )		/* GDS error */
{
   char  datb[KEYLEN];
   char  hedb[KEYLEN];
   char  recordb[RECLEN];			/* buffer for header info */
   fchar dat;					/* column data descriptor */
   fchar hed;					/* column header descriptor */
   fchar record;				/* header info */
   fint  cbytes;				/* total number of bytes */
   fint  cdone;					/* bytes read */
   fint  cfirst;				/* first byte */
   fint  clast;					/* last byte */
   fint  clength;				/* length of column */
   fint  cwidth;				/* width of column */
   fint  n;					/* loop counter */

   fmake( record, recordb );
   fmake( dat, datb );
   mkdsc( dat, tname, cname, CD_POSTFIX );	/* column data dsc */
   fmake( hed, hedb );
   mkdsc( hed, tname, cname, CH_POSTFIX );	/* column header dsc */
   if (!tobool(gds_exist_c( set, error ))) {	/* set does not exist */
      *error = GDS_TABNOTFOUND;			/* set error code */
      return;					/* return to caller */
   }
   *error = 0;					/* reset GDS error */
   gds_lock_c( set, error );			/* enter critical section */
   gdsd_rewind_c( set, hed, subset, error );	/* rewind table header */
   if (*error < 0) {
      UNLOCK( set )			/* leave critical section */
      return;					/* return to caller */
   }
						/* read first record of table header */
   gdsd_rvar_c( set, hed, subset, record, error );
   UNLOCK( set )				/* leave critical section */
   if (*error < 0) {				/* error */
      return;					/* return to caller */
   }
   for (n = 0; isalpha( record.a[n] ); n++) {	/* loop */
      record.a[n] = toupper( record.a[n] );	/* to uppercase */
   }
   if (strncmp( record.a, "DBLE", 4 )) {	/* wrong type */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
   cwidth = collen( record );			/* find width of column */
   if (cwidth != sizeof( double )) {		/* error */
      *error = GDS_TABBADTYPE;			/* GDS error */
      return;					/* return to caller */
   }
						/* find length of descriptor item */
   clength = gdsd_length_c( set, dat, subset, error );
   if (*error < 0) {
      *error = 0;
      clength = 0;
   }
   if (*first != 0) {				/* first to write */
      cfirst = (*first - 1) * cwidth + 1;	/* start here */
   } else {					/* append */
      cfirst = clength + 1;			/* start here */
   }
   cbytes = (*items) * cwidth;			/* total number of bytes */
   clast  = cfirst - 1 + cbytes;		/* last byte to read */
   if ((cfirst - 1) > clength) {		/* holes in column ? */
      *error = GDS_TABSKIPROW;			/* GDS error */
      return;					/* return to caller */
   }
						/* write column data */
   {
      fint	ftype;
      fint	nmax = cbytes / sizeof( double );

      ftype = gds_ftype_c(set, error);
      if (ftype == OS_FLOATING_TYPE) {
         gdsd_write_c( set, dat, subset, (fint *) ddata, &cbytes, &cfirst, &cdone, error );
      } else {
         double	*dbuff;

         dbuff = calloc( nmax, sizeof( double ) );
         (void) dpfplf_c( &ftype, ddata, dbuff, &nmax );
         gdsd_write_c( set, dat, subset, (fint *) dbuff, &cbytes, &cfirst, &cdone, error );
         free( dbuff );
      }
   }
}
