/* gdspos.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            gdspos.dc2

Function:     GDSPOS

Purpose:      GDSPOS prompts the user to define a position in a subset.

Category:     USER IO, PHYSICAL COORDINATES

File:         gdspos.c

Author:       K.G. Begeman

Use:          INTEGER GDSPOS( POS     ,  Output  double precision array
                              MAXPOS  ,  Input   integer
                              DEFAULT ,  Input   integer
                              KEYWORD ,  Input   character*(*)
                              MESSAGE ,  Input   character*(*)
                              SET     ,  Input   character*(*)
                              SUBSET  )  Input   integer

              GDSPOS    Returns the number of positions entered.
              POS       Array which contains the positions entered
                        by the user. Each position occupies N items
                        in POS, where N is the dimension of the subset.
                        The positions are all converted to grid coordinates.
              MAXPOS    Maximum number of positions to enter. Note that
                        the size of POS must not be smaller than
                        MAXPOS * N, where N is the dimension of the
                        subset.
              DEFAULT   Default code as in USERxxx.
              KEY       Keyword prompts the user to enter positions.
                        If KEY is blank then GDSPOS will prompt with
                        keyword 'POS='.
              MESSAGE   Message for the user. If MESSAGE is blank then
                        GDSPOS will generate its own message.
              SET       Name of set for which a position is wanted.
              SUBSET    Coordinate word of the subset for which
                        positions should be defined.

Description:  GDSPOS is a subroutine which prompts the user to enter
              one or more positions. I always follows upon a call to
              GDSINP. The input syntax is described in dcdpos.dc3.

Related Docs: dcdpos.dc3

Updates:      Feb  4, 1990: KGB, Document created.
              Dec 10, 1991: KGB, cancel replaced by reject.

#<

Fortran to C interface:

@ integer function gdspos( double precision ,
@                          integer          ,
@                          integer          ,
@                          character        ,
@                          character        ,
@                          character        ,
@                          integer          )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"anyout.h"		/* defines anyout_c */
#include	"dcdpos.h"		/* defines dcdpos_c */
#include	"gdsc_grid.h"		/* defines gdsc_grid_c */
#include	"gdsc_name.h"		/* defines gdsc_name_c */
#include	"gdsc_ndims.h"		/* defines gdsc_ndims_c */
#include	"gdsc_range.h"		/* defines gdsc_range_c */
#include	"nelc.h"		/* defines nelc_c */
#include	"reject.h"		/* defines reject_c */
#include	"usertext.h"		/* defines usertext_c */


#define MAXNAMLEN  18                          /* maximum length of axis name */
#define MAXMESLEN 256                    /* maximum length of default message */
#define MAXBUFLEN 512                         /* maximum length of input text */

typedef struct {                                     /* struct for axis names */
   char ttype[MAXNAMLEN+1];                            /* truncated axis name */
   char ctype[MAXNAMLEN+1];                                 /* full axis name */
} ax;

static char buf[MAXBUFLEN];                          /* buffer for input text */
static char mes[MAXMESLEN];                     /* buffer for default message */

fint gdspos_c( double *pos     ,
               fint   *maxpos  ,
               fint   *defmode ,
               fchar   keyword ,
               fchar   message ,
               fchar   set     ,
               fint   *subset  )
{
   ax     *axs;              /* struct which contains names of axis in subset */
   char    errmes[MAXMESLEN];                     /* buffer for error message */
   fchar   key;                          /* keyword with which to prompt user */
   fint    axnum = 1;           /* ax number where to start subtracting grids */
   fint    poserr = 0;                          /* internal GDSPOS error code */
   fint    cerror = 0;                  /* error return for GDSC_xxx routines */
   fint    cwhi;                           /* upper coordinate word of subset */
   fint    cwlo;                           /* lower coordinate word of subset */
   fint    k;                                              /* general counter */
   fint    klen = nelc_c( keyword );                     /* length of keyword */
   fint    mlen = nelc_c( message );                     /* length of message */
   fint    r = 0;                                             /* return value */
   fint    subdim = gdsc_ndims_c( set, subset );      /* dimensions of subset */

   if (!subdim) return( 0 );                       /* quick and easy solution */
   if (klen) key = keyword; else key = tofchar( "POS=" );    /* default key ? */
   /*
    * Allocate memory for arrays which hold the axis names.
    */
   axs = (ax *) calloc( subdim, sizeof( ax ) );      /* memory for axes names */
   /*
    * Get here the axis names along the subset. These are truncated
    * (skipping the part starting with a hyphen, or at least skipping
    * the trailing blanks) for use in the default message.
    */
   gdsc_range_c( set, subset, &cwlo, &cwhi, &cerror ); /* upper and lower cws */
   k = 0;
   while (k < subdim) {                           /* loop to get subdim grids */
      (void) gdsc_grid_c( set, &axnum, subset, &cerror );
      if (cerror) {           /* grid not defined in subset cw, so we want it */
         char  *ptr;
         fchar name;

         cerror = 0;
         name.a = axs[k].ttype; name.l = MAXNAMLEN; /* make fortran character */
         gdsc_name_c( name, set, &axnum, &cerror );          /* get axis name */
         name.a[MAXNAMLEN] = 0;                         /* trailing zero byte */
         strcpy( axs[k].ctype, name.a );               /* save full axis name */
         ptr = strtok( name.a, "- " );                  /* truncate axis name */
         k += 1;                                                  /* increase */
      }
      axnum += 1;                                         /* next axis number */
   }
   /*
    * Now prompt the user until we are satisfied with the pos he/she entered.
    * In principal this loop will never stop when the user is a real dummy
    * and never looks in the documentation how to supply correct input for
    * GDSPOS.
    */
   do {
      fchar text;                         /* fortran character pointer to buf */
      fint  dmode = (*defmode & 3);                    /* strip the match bit */

      if (poserr) {
         reject_c( key, tofchar( errmes ) );            /* reject POS keyword */
         poserr = 0;                                                 /* reset */
      }
      for (k = 0; k < MAXBUFLEN; buf[k++] = ' ');             /* clear buffer */
      text.a = buf; text.l = MAXBUFLEN;             /* make fortran character */
      if (mlen) {                          /* message supplied by application */
         (void) usertext_c( text, &dmode, key, message );      /* prompt user */
      } else {                                    /* generate default message */
         if (*maxpos > 1) {
            strcpy( mes, "Give positions in " );   /* part of default message */
         } else {
            strcpy( mes, "Give position in " );    /* part of default message */
         }
         for (k = 0; k < subdim; k++) {                        /* subset loop */
            if (k) strcat( mes, "," );                           /* separator */
            strcat( mes, axs[k].ttype );   /* concatenate (part of) axis name */
         }
         (void) usertext_c( text, &dmode, key, tofchar( mes ) );    /* prompt */
      }
      poserr = dcdpos_c( set, subset, text, pos, maxpos );      /* decode pos */
      /*
       * Next we check on the dcdpos output code. Zero or positive codes
       * indicate correct input and denote the number of positions
       * entered. Negative code means an error has occurred.
       */
      switch(poserr) {                                    /* check error code */
         case -11: {					      /* DCDPOS error */
            strcpy( errmes, "BLANKS decoded!" );
            break;
         }
         case -10: {					      /* DCDPOS error */
            strcpy( errmes, "Decode error from dcddble!" );
            break;
         }
         case -9: {                                           /* DCDPOS error */
            strcpy( errmes, "Cannot handle mixed EPOCHs!" );
            break;
         }
         case -8: {                                           /* DCDPOS error */
            strcpy( errmes, "Cannot obtain header information!" );
            break;
         }
         case -7: {                                           /* DCDPOS error */
            strcpy( errmes, "Too many positions!" );
            break;
         }
         case -6: {                                           /* DCDPOS error */
            strcpy( errmes, "No grid separation defined!" );
            break;
         }
         case -5: {                                           /* DCDPOS error */
            strcpy( errmes, "'D' not allowed for positions!");
            break;
         }
         case -4: {                                           /* DCDPOS error */
            strcpy( errmes, "COTRANS error!" );
            break;
         }
         case -3: {                                           /* DCDPOS error */
            strcpy( errmes, "Incomplete input! " );
            break;
         }
         case -2: {                                           /* DCDPOS error */
            strcpy( errmes, "Prefix incompatible with axis!" );
            break;
         }
         case -1: {                                           /* DCDPOS error */
            strcpy( errmes, "Illegal use of 'PC', 'AC' or 'D'!" );
            break;
         }
         default: {                    /* no error, check number of positions */
            if ( poserr < 0 ) {
               strcpy( errmes, "Unregistered error!" );
               break;
            }
            r = poserr;                                /* number of positions */
            if (*defmode & 4) {                          /* need exact number */
               if (*defmode == 4 && r != *maxpos) {
                  sprintf( errmes, "Unequal number of positions. MUST be %d!", *maxpos );
               } else if (r != 0 || r != *maxpos) {
                  sprintf( errmes, "Unequal number of positions. MUST be %d!", *maxpos );
               } else {
                  poserr = 0;                                        /* reset */
               }
            } else {
               poserr = 0;                                           /* reset */
            }
            break;
	 }
      }
   } while (poserr);
   free( axs );                                                /* free memory */
   return( r );                 /* return number of positions entered by user */
}
