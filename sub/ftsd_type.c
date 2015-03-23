/* ftsd_type.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            ftsd_type.dc2

Function:     FTSD_TYPE

Purpose:      Returns the type (i.e. CHAR, INT, LOG, REAL, DBLE) of the
              contents of a FITS header record.

Category:     FITS

File:         ftsd_type.c

Author:       K.G. Begeman

Use:          CHARACTER*(*) FTSD_TYPE( RECORD )   Input    CHARACTER*80

              FTSD_TYPE     Returns:
                            'CHAR' : record contains characters
                            'INT ' : record contains integer
                            'LOG ' : record contains logical
                            'DBLE' : record contains double prec. float
                            'REAL' : record contains single prec. float
                            '    ' : type could not be determined
              RECORD        Contains FITS header record of 80 bytes
                            long. Only the first 30 bytes are examined.

Updates:      Nov 23, 1990: KGB Document created.

#<

Fortran to C interface:

@ character function ftsd_type( character )

*/

#include	"ctype.h"	/* <ctype.h> */
#include	"float.h"	/* <float.h> */
#include	"stdio.h"	/* <stdio.h> */
#include	"string.h"	/* <string.h> */
#include	"gipsyc.h"	/* GIPSY symbols and definitions */

#define	FITSRECLEN	80	/* length of FITS header record */
#define	FITSDSCLEN	8	/* length of descriptor name field */
#define	FITSDATLEN	20	/* length of data field */
#define	FITSDATAFB	10	/* begin position of data field */

static	char	*types[] = { "CHAR", "DBLE", "INT ", "LOG ", "REAL" };

void	ftsd_type_c( fchar type,		/* output FITS type */
                     fchar record )		/* input FITS record */
{
   char field[FITSDATLEN+1];			/* copy of data field */
   fint n;					/* loop counter */

   for (n = 0; n < type.l; type.a[n++] = ' ');	/* initialize */
   if (record.l < (FITSDATLEN + FITSDATAFB)) {	/* not enough information */
      return;					/* return to caller */
   }
   if (record.a[FITSDSCLEN] != '=') {		/* not a data record */
      return;					/* return to caller */
   }
   for (n = 0; n < FITSDATLEN; n++) {		/* copy loop */
      field[n] = toupper( record.a[FITSDATAFB+n] );
   }
   field[FITSDATLEN] = '\0';			/* add zero byte */
   if (field[0] == '\'') {			/* must be character type */
      for (n = 0; n < 4 && n < type.l; n++) {	/* copy loop */
         type.a[n] = types[0][n];		/* copy data type */
      }
      return;					/* return to caller */
   }
   if ((strchr( field, 'T' ) != NULL) || (strchr( field, 'F' ) != NULL)) {
      for (n = 0; n < 4 && n < type.l; n++) {	/* copy loop */
         type.a[n] = types[3][n];		/* copy data type */
      }
   } else if (strchr( field, 'E' ) != NULL) {	/* single precision */
      int	count = 0;			/* count digits */
      int	t;				/* real or double */

      n = 0;					/* reset loop counter */
      while (field[n]) {			/* loop */
         if (field[n] == 'E') break;		/* stop counting */
         if (isdigit(field[n])) count++;	/* count */
         n++;					/* increment */
      }
      if (count > FLT_DIG) {			/* must be double */
         t = 1;
      } else {					/* must be single */
         t = 4;
      }
      for (n = 0; n < 4 && n < type.l; n++) {	/* copy loop */
         type.a[n] = types[t][n];		/* copy data type */
      }
   } else if (strchr( field, 'D' ) != NULL) {	/* double precision */
      for (n = 0; n < 4 && n < type.l; n++) {	/* copy loop */
         type.a[n] = types[1][n];		/* copy data type */
      }
   } else if (strchr( field, '.' ) != NULL) {	/* single precision */
      int	count = 0;			/* count digits */
      int	t;				/* real or double */

      n = 0;					/* reset loop counter */
      while (field[n]) {			/* loop */
         if (field[n] == 'E') break;		/* stop counting */
         if (isdigit(field[n])) count++;	/* count */
         n++;					/* increment */
      }
      if (count > FLT_DIG) {			/* must be double */
         t = 1;
      } else {					/* must be single */
         t = 4;
      }
      for (n = 0; n < 4 && n < type.l; n++) {	/* copy loop */
         type.a[n] = types[t][n];		/* copy data type */
      }
   } else {					/* make it an integer */
      for (n = 0; n < 4 && n < type.l; n++) {	/* copy loop */
         type.a[n] = types[2][n];		/* copy data type */
      }
   }
}
