/* gdsbox.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            gdsbox.dc2

Function:     GDSBOX

Purpose:      GDSBOX prompts the user to define a box inside/around a
              subset.

Category:     USER IO, PHYSICAL COORDINATES

File:         gdsbox.c

Author:       K.G. Begeman

Use:          CALL GDSBOX( BLO     ,    In/Output  integer array
                           BHI     ,    In/Output  integer array
                           SET     ,      Input    character*(*)
                           SUBSET  ,      Input    integer
                           DEFAULT ,      Input    integer
                           KEYWORD ,    In/Output  character*(*)
                           MESSAGE ,      Input    character*(*)
                           SHOWDEV ,      Input    integer
                           OPTION  )      Input    integer

              BLO       On input this array may contain the default
                        centre or the default lower left edge of the
                        box (depends on OPTION).
                        On output it contains the user defined lower
                        left grid coordinates of the box.
              BHI       On input this array may contain the default
                        size or the default upper right edge of the
                        box (depends on OPTION).
                        On output it contains the user defined upper
                        right grid coordinates of the box.
              SET       Name of set for which a box should be defined.
              SUBSET    Coordinate word of the subset for which a box
                        should be defined.
              DEFAULT   Default code as in USERxxx.
              KEYWORD   Keyword prompts the user to enter a box. If
                        KEYWORD is blank the GDSBOX will prompt with 'BOX='.
              MESS      Message for the user. If MESS is blank then
                        GDSBOX will generate its own message.
              SHOWDEV   Device (as in ANYOUT) to which some info
                        about the chosen box is displayed.
              OPTION    The different options are:
                          1 box may exceed subset size
                          2 default is in BLO
                          4 default is in BHI
                          8 box restricted to size defined in BHI
                         16 return immediately in case of errors
                        These codes work additive. So when OPTION = 0
                        or 1 then the default is entire subset.
                        If option 16 is set, then in case of errors this
                        argument will be set to -1 and GDSBOX will return
                        immediately.

Description:  GDSBOX is a subroutine which prompts the user to enter a
              box around/inside a subset.
              It always follows upon a call to GDSINP. The input syntax
              is described in document dcdpos.dc3.

Related Docs: dcdpos.dc3

Updates:      Feb  1, 1990: KGB, Document created.
              Dec 11, 1991: KGB, cancel replaced by reject.
              Nov  7, 1997: JPT, Option 16 inplemented.
              Apr 09, 2009: VOG, Replaced NINT definition with one that
                                 uses floor(). Several other routines
                                 dealing with coordinates now use the
                                 same definition. The routines now can deal
                                 properly with CRPIX values that end on 0.5
                                 (tested for both negative and positive CRPIX)
#<

Fortran to C interface:

@ subroutine gdsbox( integer*8   ,
@                    integer*8   ,
@                    character ,
@                    integer*8   ,
@                    integer   ,
@                    character ,
@                    character ,
@                    integer   ,
@                    integer   )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"anyout.h"		/* defines anyout_c */
#include	"cancel.h"		/* defines cancel_c */
#include	"dcdpos.h"		/* defines dcdpos_c */
#include        "math.h"
#include	"gdsc_grid.h"		/* defines gdsc_grid_c */
#include	"gdsc_name.h"		/* defines gdsc_name_c */
#include	"gdsc_ndims.h"		/* defines gdsc_ndims_c */
#include	"gdsc_range.h"		/* defines gdsc_range_c */
#include	"nelc.h"		/* defines nelc_c */
#include	"reject.h"		/* defines reject_c */
#include	"usertext.h"		/* defines usertext_c */

/* Old definition: #define  NINT(x)     ( x > 0.0 ? (fint) ( x + 0.5 ) : (fint) ( x - 0.5 ) )*/
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )

#define MAXNAMLEN  18                          /* maximum length of axis name */
#define MAXKEYLEN  20                            /* maximum length of keyword */
#define MAXMESLEN 256                    /* maximum length of default message */
#define MAXBUFLEN 512                         /* maximum length of input text */

typedef struct {                                     /* struct for axis names */
   char ttype[MAXNAMLEN+1];                            /* truncated axis name */
   char ctype[MAXNAMLEN+1];                                 /* full axis name */
} ax;

static char buf[MAXBUFLEN];                          /* buffer for input text */
static char mes[MAXMESLEN];                     /* buffer for default message */

void gdsbox_c( fint8  *blo    ,
               fint8  *bhi    ,
               fchar  set    ,
               fint8 *subset ,
               fint  *defmode,
               fchar  keyword,
               fchar  message,
               fint  *showdev,
               fint  *option )
{
   ax     *axs;              /* struct which contains names of axis in subset */
   char   errmes[MAXMESLEN];                      /* buffer for error message */
   double *pos;                                           /* array for dcdpos */
   fchar   key;                                                    /* keyword */
   fint    axnum = 1;           /* ax number where to start subtracting grids */
   fint    boxerr = 0;                          /* internal GDSBOX error code */
   fint    cerror = 0;                  /* error return for GDSC_xxx routines */
   fint    cp = 0;                                /* keyword CPOS= was used ? */
   fint8    cwhi;                           /* upper coordinate word of subset */
   fint8    cwlo;                           /* lower coordinate word of subset */
   fint   *ghi;                     /* buffer for upper right grids of subset */
   fint   *glo;                      /* buffer for lower left grids of subset */
   fint    k;                                              /* general counter */
   fint    klen = nelc_c( keyword );                     /* length of keyword */
   fint    mlen = nelc_c( message );                     /* length of message */
   fint    subdim = gdsc_ndims_c( set, subset );      /* dimensions of subset */
   fint    dfault = *defmode;
   bool    exitflag;

   exitflag = (*option)&16;
   
   if (!subdim) return;                                      /* fast solution */
   if (klen) key = keyword; else key = tofchar( "BOX=" );    /* default key ? */
   /*
    * Allocate memory for arrays which hold the lower and upper edges of
    * subset, the axis names and the positions returned by dcdpos.
    */
   glo = (fint *) calloc( subdim, sizeof( fint ) );         /* memory for glo */
   ghi = (fint *) calloc( subdim, sizeof( fint ) );         /* memory for ghi */
   axs = (ax *) calloc( subdim, sizeof( ax ) );      /* memory for axes names */
   pos = (double *) calloc( 2 * subdim, sizeof( double ) ); /* memory for pos */
   /*
    * Get here the upper and lower grids of subset frame. We also
    * obtain the axis names along the subset. These are truncated
    * (skipping the part starting with a hyphen, or at least skipping
    * the trailing blanks) for use in the default message.
    */
   gdsc_range_c( set, subset, &cwlo, &cwhi, &cerror ); /* upper and lower cws */
   k = 0;
   while (k < subdim) {                           /* loop to get subdim grids */
      (void) gdsc_grid_c( set, &axnum, subset, &cerror );
      if (cerror) {           /* grid not defined in subset cw, so we want it */
         //char  *ptr;
         fchar name;

         cerror = 0;
         glo[k] = gdsc_grid_c( set, &axnum, &cwlo, &cerror ); /* extract grid */
         ghi[k] = gdsc_grid_c( set, &axnum, &cwhi, &cerror ); /* extract grid */
         name.a = axs[k].ttype; name.l = MAXNAMLEN; /* make fortran character */
         gdsc_name_c( name, set, &axnum, &cerror );          /* get axis name */
         name.a[MAXNAMLEN] = 0;                         /* trailing zero byte */
         strcpy( axs[k].ctype, name.a );               /* save full axis name */
         //ptr = strtok( name.a, "- " );                  /* truncate axis name */
         k += 1;                                                  /* increase */
      }
      axnum += 1;                                         /* next axis number */
   }
   /*
    * Now prompt the user until we are satisfied with the box he/she entered.
    * In principal this loop will never stop when the user is a real dummy
    * and never looks in the documentation how to supply correct input for
    * GDSBOX.
    */
   do {
      fchar text;                         /* fortran character pointer to buf */
      fint  dcdopt = 0;                           /* GDSBOX option for dcdpos */

      if (boxerr) {                                  /* an error was detected */
         reject_c( key, tofchar( errmes ) );            /* reject BOX keyword */
         if (exitflag) {
            *option = -1;
            return;
         }
         if (dfault & 2) dfault -= 2;
      }
      for (k = 0; k < MAXBUFLEN; buf[k++] = ' ');             /* clear buffer */
      text.a = buf; text.l = MAXBUFLEN;             /* make fortran character */
      if (mlen) {                          /* message supplied by application */
         (void) usertext_c( text, &dfault, key, message );     /* prompt user */
      } else {                                    /* generate default message */
         strcpy( mes, "Give BOX in " );      /* first part of default message */
         for (k = 0; k < subdim; k++) {                        /* subset loop */
            if (k) strcat( mes, "," );                           /* separator */
            strcat( mes, axs[k].ttype );   /* concatenate (part of) axis name */
         }
         if (*option == 1 || !*option ) {            /* default entire subset */
            strcat( mes, " [entire subset]" );              /* this is simple */
	 } else if ((*option & 6) == 6) {          /* blo and bhi are default */
            for (k = 0; k < subdim; k++) {                     /* subset loop */
               char num[20];               /* local buffer to generate number */

               if (k) {
                  sprintf( num, ",%4ld", blo[k] );        /* generate number */
               } else {
                  sprintf( num, " [%4ld", blo[k] );       /* generate number */
               }
               strcat( mes, num );                      /* concatenate number */
            }
            for (k = 0; k < subdim; k++) {                     /* subset loop */
               char num[20];               /* local buffer to generate number */

               sprintf( num, ",%4ld", bhi[k] );             /* generate number */
               strcat( mes, num );                      /* concatenate number */
            }
            strcat( mes, "]" );                    /* closing default bracket */
         } else if (*option & 2) {         /* blo is default central position */
            for (k = 0; k < subdim; k++) {                     /* subset loop */
               char num[20];               /* local buffer to generate number */

               if (k) {
                  sprintf( num, ",%4ld", blo[k] );          /* generate number */
               } else {
                  sprintf( num, " [CPOS=%4ld", blo[k] );    /* generate number */
               }
               strcat( mes, num );                      /* concatenate number */
            }
            strcat( mes, "]" );                    /* closing default bracket */
         } else if (*option & 4) {                     /* bhi is default size */
            for (k = 0; k < subdim; k++) {                     /* subset loop */
               char num[20];               /* local buffer to generate number */

               if (k) {
                  sprintf( num, ",%4ld", bhi[k] );          /* generate number */
               } else {
                  sprintf( num, " [D %4ld", bhi[k] );       /* generate number */
               }
               strcat( mes, num );                      /* concatenate number */
            }
            strcat( mes, "]" );                    /* closing default bracket */
         }
         (void) usertext_c( text, &dfault, key, tofchar( mes ) );   /* prompt */
      }
      boxerr = dcdpos_c( set, subset, text, pos, &dcdopt );     /* decode pos */
      if (!boxerr && (*option & 6) == 4) {             /* default size wanted */
         boxerr = 2;                                  /* we have got the size */
         for (k = 0; k < subdim; k++) {
            pos[k+subdim] = (double) bhi[k];                 /* size from bhi */
         }
      }
      /*
       * Next we check on the dcdpos output code. The following codes mean
       * correct input and have the following meaning:
       *  0  - no input available in text string.
       *  1  - one position found in input text string. This will be used by
       *       GDSBOX as the central position of the box.
       *  2 -  only the size is defined. GDSBOX will use this size and
       *       prompt the user for a central position of the box.
       *  3 -  central position and size were defined in input text string.
       *  4 -  two position were defined in input text string.
       */
      switch(boxerr) {                             /* what returned by dcdpos */
         case 0: {                                        /* nothing returned */
            if (*option == 1 || !*option) {  /* default is entire subset size */
               for (k = 0; k < subdim; k++) {
                  pos[k] = (double) glo[k];                /* lower left grid */
                  pos[k+subdim] = (double) ghi[k];        /* upper right grid */
               }
            } else if ((*option & 6) != 6) {        /* no default to be taken */
               boxerr = -10;     /* no default allowed by application program */
            } else {                                /* default in blo and bhi */
               for (k = 0; k < subdim; k++) {
                  pos[k] = (double) blo[k];                /* lower left grid */
                  pos[k+subdim] = (double) bhi[k];        /* upper right grid */
               }
            }
            break;
         }
         case 1: {                                 /* only one position given */
            boxerr = 0;                               /* reset internal error */
            if ((*option & 6) != 4) {          /* no default size or position */
               boxerr = -11;                                /* no default bhi */
            } else {
               for (k = 0; k < subdim; k++) {       /* loop along subset axes */
                  fint c = NINT( pos[k] );                /* central position */
                  fint s = bhi[k];                             /* size of box */

                  pos[k+subdim] = (double) ( c + s / 2 );      /* upper right */
                  pos[k] = pos[k+subdim] + (double) ( 1 - s );  /* lower left */
               }
            }
            break;
         }
         case 2: {                                       /* only size defined */
            boxerr = 0;                               /* reset internal error */
            if ((*option & 1) == 0) {            /* box must be inside subset */
               fint	okay = 1;

               for (k = 0; k < subdim; k++) {
                  if (NINT(pos[k+subdim]) > (ghi[k] - glo[k] + 1)) okay = 0;
               }
               if (!okay) boxerr = -13;
            }
            if (!boxerr && (*option & 8)) {          /* frame size must match */
               fint	okay = 1;

               for (k = 0; k < subdim; k++) {
                  fint s;

                  if ((*option & 6) == 4) {         /* required size in bhi ? */
                     s = bhi[k];
                  } else {                  /* required size in blo and bhi ? */
                     s = bhi[k] - blo[k] + 1;
                  }
                  if (s != NINT(pos[k+subdim])) okay = 0;    /* size not okay */
               }
               if (!okay) boxerr = -12;                /* size does not match */
            }
            if (!boxerr && (*option & 8)) {      /* we check size of box here */
               fint okay = 1;                                 /* okay is okay */

               for (k = 0; k < subdim; k++) {       /* loop along subset axes */
                  fint s;                                      /* size of box */

                  if ((*option & 6) == 4) {                    /* size in bhi */
                     s = bhi[k];
                  } else {                             /* size in bhi and blo */
                     s = bhi[k] - blo[k] + 1;
                  }
                  if (s != NINT( pos[k+subdim] )) okay = 0;   /* unequal size */
               }
               if (!okay) boxerr = -12;                                /* set */
            }
            for (k = 0; !boxerr && k < subdim; k++) {
               fint s = NINT( pos[k+subdim] );

               if (s <= 0) boxerr = -14;               /* illegal size of box */
            }
            if (!boxerr) {       /* check whether central position is defined */
               fint d;                             /* default level for CPOS= */
               fint dcdopt = 1;                   /* GDSPOS option for dcdpos */

               cp = 1;                   /* keyword CPOS= is going to be used */
               if ((*option & 6) == 2) d = 2; else d = 0;         /* hidden ? */
               strcpy( mes, "Give CPOS in " );       /* first part of message */
               for (k = 0; k < subdim; k++) {                  /* subset loop */
                  if (k) strcat( mes, "," );
                  strcat( mes, axs[k].ttype );       /* concatenate axis name */
               }
               if (d) {                         /* default position specified */
                  for (k = 0; k < subdim; k++) {               /* subset loop */
                     char num[20];         /* local buffer to generate number */

                     if (k) {
                        sprintf( num, ",%4ld", blo[k] );    /* generate number */
                     } else {
                        sprintf( num, " [%4ld", blo[k] );   /* generate number */
                     }
                     strcat( mes, num );                /* concatenate number */
                  }
                  strcat( mes, "]" );              /* closing default bracket */
               }
               do {
                  fchar cpos;

                  if (exitflag) {
                     reject_c(key, tofchar("central position missing") );
                     *option = -1;
                     return;
                  }
                  cpos = tofchar( "CPOS=" );                       /* keyword */
                  if (boxerr) {
                     reject_c( cpos, tofchar( errmes ) );
                     boxerr = 0;                                     /* reset */
                  }
                  for (k = 0; k < MAXBUFLEN; buf[k++] = ' ');        /* clear */
		  (void) usertext_c( text, &d, cpos, tofchar( mes ) );
                  boxerr = dcdpos_c( set, subset, text, pos, &dcdopt );
                  switch(boxerr) {           /* examine return code of dcdpos */
		     case -11: {			      /* DCDPOS error */
		        strcpy( errmes, "BLANKS decode!" );
			break;
		     }
		     case -10: {			      /* DCDPOS error */
		        strcpy( errmes, "Decode error from dcddble!" );
			break;
                     }
                     case -9: {                               /* DCDPOS error */
                        strcpy( errmes, "Cannot handle mixed EPOCHs!" );
                        boxerr = -1;
                        break;
                     }
                     case -8: {                               /* DCDPOS error */
                        strcpy( errmes, "Cannot obtain header information!" );
                        boxerr = -1;                       /* error, so retry */
                        break;
                     }
                     case -7: {                               /* DCDPOS error */
                        strcpy( errmes, "Too many positions!" );
                        boxerr = -1;                       /* error, so retry */
                        break;
                     }
                     case -6: {                               /* DCDPOS error */
                        strcpy( errmes, "No grid separation defined!" );
                        boxerr = -1;                       /* error, so retry */
                        break;
                     }
                     case -5: {                               /* DCDPOS error */
                        strcpy( errmes, "'D' not allowed for positions!" );
                        boxerr = -1;                       /* error, so retry */
                        break;
                     }
                     case -4: {                               /* DCDPOS error */
                        strcpy( errmes, "COTRANS error!" );
                        boxerr = -1;                       /* error, so retry */
                        break;
                     }
                     case -3: {                               /* DCDPOS error */
                        strcpy( errmes, "Incomplete input!" );
                        boxerr = -1;                       /* error, so retry */
                        break;
                     }
                     case -2: {                               /* DCDPOS error */
                        strcpy( errmes, "Prefix incompatible with axis!" );
                        boxerr = -1;                       /* error, so retry */
                        break;
                     }
                     case -1: {                               /* DCDPOS error */
                        strcpy( errmes, "Illegal use of 'PC', 'AC' or 'D'!" );
                        boxerr = -1;                       /* error, so retry */
                        break;
                     }
                     case 0:              /* no position, so default is taken */
                     case 1: {                       /* one position, so okay */
                        boxerr = 0;                                  /* reset */
                        for (k = 0; k < subdim; k++) {         /* subset loop */
                           fint c = NINT( pos[k] );          /* centre of box */
                           fint	l;                              /* lower grid */
			   fint s = NINT( pos[k+subdim] );     /* size of box */
			   fint	u;                              /* upper grid */

                           u = NINT( (double) ( c + s / 2 ) );         /* u-r */
                           l = NINT( (double) ( u + 1 - s ) );         /* l-l */
                           if ((*option & 1) == 0) {
                              if (l < glo[k] || u > ghi[k]) boxerr = -1;
                           }
                        }
                        if (boxerr) {
                           strcpy( errmes, "BOX outside frame of subset!" );
                        }
                        break;
                     }
                     default: {                           /* should not occur */
                        strcpy( errmes, "Unregistered error!" );
                        boxerr = -1;                       /* error, so retry */
                        break;
                     }
                  }
               } while (boxerr);                  /* stop when boxerr is zero */
               for (k = 0; k < subdim; k++) {
                  fint c = NINT( pos[k] );                   /* centre of box */
                  fint s = NINT( pos[k+subdim] );              /* size of box */

                  if (s <= 0) {
                     boxerr = -14;                     /* illegal size of BOX */
                  } else {
                     pos[k+subdim] = (double) ( c + s / 2 );           /* u-r */
                     pos[k] = pos[k+subdim] + (double) ( 1 - s );      /* l-l */
                  }
               }
            }
            break;
         }
         case 3: {                                   /* centre and size given */
            boxerr = 0;                                              /* reset */
            for (k = 0; !boxerr && k < subdim; k++) {          /* subset loop */
               fint c = NINT( pos[k] );                      /* centre of box */
	       fint s = NINT( pos[k+subdim] );                 /* size of box */

               if (s <= 0) {
                  boxerr = -14;                        /* illegal size of BOX */
               } else {
                  pos[k+subdim] = (double) ( c + s / 2 );      /* upper right */
                  pos[k] = pos[k+subdim] + (double) ( 1 - s );  /* lower left */
               }
            }
            break;
         }
         case 4: {                                     /* two positions given */
            boxerr = 0;                                              /* reset */
            for (k = 0; k < subdim; k++) {                     /* subset loop */
               fint l = NINT( pos[k] );                    /* lower left grid */
               fint u = NINT( pos[k+subdim] );            /* upper right grid */

               if (l > u) {                        /* lower was in fact upper */
                  pos[k] = (double) u;                          /* lower left */
                  pos[k+subdim] = (double) l;                  /* upper right */
               } else {                             /* lower was indeed lower */
                  pos[k] = (double) l;                          /* lower left */
                  pos[k+subdim] = (double) u;                  /* upper right */
               }
            }
            break;
         }
         default: {                                           /* dcdpos error */
            break;
         }
      }
      /*
       * Next we check whether the frame is allowed to be larger than
       * the subset. When not, it is checked whether it fits inside the
       * subset.
       */
      if (!boxerr && (*option & 1) == 0) {       /* box must be inside subset */
         fint okay = 1;

         for (k = 0; k < subdim; k++) {                        /* subset loop */
            fint l = NINT( pos[k] );                            /* lower left */
            fint u = NINT( pos[k+subdim] );                    /* upper right */

            if (l < glo[k] || u > ghi[k]) okay = 0;         /* outside subset */
         }
         if (!okay) boxerr = -13;                     /* outside subset frame */
      }
      /*
       * Next we check whether an exact size of the box is
       * necessary. If so, then the size of the box entered by the user
       * is checked against the required size.
       */
      if (!boxerr && *option & 8) {                  /* frame size must match */
         fint okay = 1;

         for (k = 0; k < subdim; k++) {                        /* subset loop */
            fint l = NINT( pos[k] );                            /* lower left */
            fint u = NINT( pos[k+subdim] );                    /* upper right */
            fint s;

            if ((*option & 6) == 4) {               /* required size in bhi ? */
               s = bhi[k];
            } else {                        /* required size in blo and bhi ? */
               s = bhi[k] - blo[k] + 1;
            }
            if (s != ( u - l + 1 )) okay = 0;                /* size not okay */
         }
         if (!okay) boxerr = -12;                      /* size does not match */
      }
      /*
       * Now we can check whether there were any errors detected by
       * GDSBOX or DCDPOS. If so then a message goes out to the user
       * and we will start all over again. If no error, the box defined
       * by the user is copied to blo and bhi.
       */
      switch(boxerr) {                                    /* check error code */
         case -14: {                                          /* illegal size */
            strcpy( errmes, "Zero or negative size entered" );
            break;
         }
         case -13: {                               /* outside frame of subset */
            strcpy( errmes, "BOX outside frame of subset!" );
            break;
         }
         case -12: {                                     /* wrong size of box */
            strcpy( errmes, "Wrong size of box!" );
            break;
         }
         case -11: {                /* no default size allowed by application */
            strcpy( errmes, "No default size allowed!" );
            break;
         }
         case -10: {                     /* no default allowed by application */
            strcpy( errmes, "Application allows no default!" );
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
            strcpy( errmes, "'D' not allowed for positions!" );
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
         case 0: {                /* no error, so copy results to blo and bhi */
            sprintf( mes, "BOX range for set %.*s :", (int) nelc_c( set ), set.a );
            anyout_c( showdev, tofchar( mes ) );                 /* show info */
            for (k = 0; k < subdim; k++) {
               blo[k] = NINT( pos[k] );
               bhi[k] = NINT( pos[k+subdim] );
               sprintf( mes, "%.*s from %5ld to %5ld", MAXNAMLEN, axs[k].ctype, blo[k], bhi[k] );
               anyout_c( showdev, tofchar( mes ) );
            }
            break;
         }
         default: {
            break;
	 }
      }
   } while (boxerr);
   free( axs ); free( pos ); free( glo ); free( ghi );         /* free memory */
   /*
    * At the end we check whether keyword CPOS= has been used in the
    * process of defining the BOX. If it has been used, don't remove it,
    * otherwize we will remove it to make sure it does not appear in the
    * macro.
    */
  if (!cp) cancel_c( tofchar( "CPOS=" ) );            /* cancel keyword CPOS= */
}
