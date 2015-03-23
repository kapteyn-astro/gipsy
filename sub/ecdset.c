/* ecdset.c

	Copyright (c) Kapteyn Laboratorium Groningen 1994
	All Rights Reserved.

#>            ecdset.dc2

Function:     ECDSET

Purpose:      Encodes set and subsets into a string. The set MUST exist!

Category:     USER IO

File:         ecdset.c

Author:       K.G. Begeman

Use:          INTEGER ECDSET( STRING,        Output    CHARACTER*(*)
                              SET,           Input     CHARACTER*(*)
                              SUBSETS,       Input     INTEGER ARRAY
                              NSUBS )        Input     INTEGER

              ECDSET        Returns 0 on success, else:
                            -1: Set does not exist.
                            -2: Cannot allocate sufficient memory.
                            -3: Encoded string longer than output buffer.
                            -4: Impossible to encode.
              STRING        Output encoded string.
              SET           Name of set.
              SUBSETS       The subset coordinate words.
              NSUBS         The number of subsets.

Related Docs: dcdset.dc2

Updates:      Jan 13, 1994: KGB, Document created.

#<

Fortran to C interface:

@ integer function ecdset( character, character, integer, integer )

*/

#include	"ctype.h"			/* <ctype.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */

#include	"gipsyc.h"			/* GIPSY definitions */
#include	"gds_close.h"			/* gds_close_c */
#include	"gds_exist.h"			/* gds_exist_c */
#include	"gdsc_grid.h"			/* gdsc_grid_c */
#include	"gdsc_name.h"			/* gdsc_name_c */
#include	"gdsc_ndims.h"			/* gdsc_ndims_c */
#include	"nelc.h"			/* nelc_c */

#define	MAXSTRINGLEN	1024			/* max. length of strings */
#define	MAXCTYPELEN	70			/* max. length of axis name */

#define	RETURN( r )	\
{ \
   if ( ax != NULL ) { \
      int	m; \
      for ( m = 0; m < ndef; m++ ) { \
         if ( ax[m].grid != NULL ) free( ax[m].grid ); \
         if ( ax[m].step != NULL ) free( ax[m].step ); \
      } \
      free( ax ); \
   } \
   return( r ); \
}

#define	SHIP( out )	\
{ \
   int	n = 0; \
   while ( out[n] && nc < string.l ) { \
      string.a[nc++] = out[n++]; \
   } \
   if ( out[n] ) RETURN( -3 ); \
}

typedef	struct {
   char	ctype[MAXCTYPELEN];			/* axis name */
   fint	axnum;					/* axis sequence number */
   fint	*grid;					/* the grids on axis */
   fint	*step;					/* the grid step */
   fint	*user;					/* user entered */
   fint	ngrid;					/* positions on axis */
   fint	nstep;					/* number of steps */
   fint	nsequ;					/* sequences */
} ax_struct;

/*
 * compar is called by qsort to sort the axis in the order as probably
 * defined in the command.
 */

static	int	compar( const void *a1, const void *a2 )
{
   const ax_struct	*b1 = a1;
   const ax_struct	*b2 = a2;

   return( b2->nsequ - b1->nsequ );
}

/*
 * ecdset encodes a set name and subsets into text (i.e. as would have been
 * entered by the user). The status indicates whether an error occurred:
 * value of status      meaning
 *       0              No error.
 *      -1              Set does not exist.
 *      -2              Cannot allocate sufficient memory.
 *      -3              Encoded string longer than output buffer.
 *      -4              Impossible to encode.
 */

fint	ecdset_c( fchar string, fchar set, fint subsets[], fint *nsubs )
{
   ax_struct	*ax = NULL;			/* the ax struct */
   fint		count;				/* counter */
   fint		gerror = 0;			/* gds error return */
   fint		nc = 0;				/* number of characters shipped */
   fint		m, n;				/* loop counters */
   fint		naxis;				/* number of axes */
   fint		ndef = 0;			/* number of defined axes */
   fint		subset;				/* subset number */
   fint		toplevel = 0;			/* whole set */

   for ( n = 0; n < string.l; string.a[n++] = ' ' );
   if ( !tobool( gds_exist_c( set, &gerror ) ) || gerror ) {
      return( -1 );
   }
   naxis = gdsc_ndims_c( set, &toplevel );	/* dimension of set */
   subset = subsets[0];				/* first subset */
   for ( n = 0; n < naxis; n++ ) {		/* loop to get defined axes */
      fint	axnum;				/* the axis number */
      fint	grid;				/* grid number */

      axnum = n + 1;				/* current axis number */
      gerror = 0;				/* reset */
      grid = gdsc_grid_c( set, &axnum, &subset, &gerror );
      if ( gerror == 0 ) {			/* axis defined */
         fchar	ctype;				/* points to buffer for axis name */

         ax = realloc( ax, sizeof( ax_struct ) * ( ndef + 1 ) );
         if ( ax == NULL ) RETURN( -2 );	/* error */
         ax[ndef].axnum = axnum;		/* axis sequence number */
         ax[ndef].nstep = 0;
         ax[ndef].ngrid = 0;
         ax[ndef].grid = NULL;
         ax[ndef].step = NULL;
         ax[ndef].user = NULL;
         ctype.a = ax[ndef].ctype;
         ctype.l = MAXCTYPELEN;
         gdsc_name_c( ctype, set, &axnum, &gerror );
         ctype.a[nelc_c( ctype )] = 0;
         ndef++;
      }
   }
   gerror = 0;
   nc = nelc_c( set );
   if ( nc > string.l ) RETURN( -3 );
   strncpy( string.a, set.a, nc );
   if ( ndef == 0 ) {
      if ( (*nsubs) != 1 ) RETURN( -4 );
      RETURN( 0 );
   }
   for ( m = 0; m < ndef; m++ ) {
      ax[m].grid = calloc( (*nsubs), sizeof( fint ) );
      if ( ax[m].grid == NULL ) RETURN( -2 );
      ax[m].step = calloc( (*nsubs), sizeof( fint ) );
      if ( ax[m].step == NULL ) RETURN( -2 );
      ax[m].user = calloc( (*nsubs), sizeof( fint ) );
      if ( ax[m].user == NULL ) RETURN( -2 );
   }
   for ( n = 0; n < (*nsubs); n++ ) {
      for ( m = 0; m < ndef; m++ ) {
         ax[m].grid[n] = gdsc_grid_c( set, &ax[m].axnum, &subsets[n], &gerror );
      }
   }
   for ( m = 0; m < ndef; m++ ) {
      fint	okay;
      fint	n1, n2;
      fint	ns = 1;
      fint	nt = (*nsubs);

      ax[m].nsequ = ns;
      while ( ns < (*nsubs) ) {
         ns++;
         if ( ( (*nsubs) % ns ) == 0 ) {
            nt = (*nsubs) / ns;
            for ( okay = 1, n1 = 0; n1 < nt && okay; n1++ ) {
               for ( n2 = 1; n2 < ns && okay; n2++ ) {
                  okay = ( ax[m].grid[n1] == ax[m].grid[n1+nt*n2] );
               }
            }
            if ( okay ) ax[m].nsequ = ns;
         }
      }
   }
   qsort( ax, ndef, sizeof( ax_struct ), compar );
   if ( ( ax[0].nsequ == (*nsubs) ) && ( ndef > 1 ) ) {
      ax_struct	save;

      ax[0].nsequ = 1;
      save = ax[0];
      for ( n = 1; n < ndef; n++ ) {
         ax[n-1] = ax[n];
      }
      ax[ndef-1] = save;
   }
   count = (*nsubs);
   ax[0].ngrid = count / ax[0].nsequ;
   count /= ax[0].ngrid;
   for ( n = 0; n < ax[0].ngrid; n++ ) {
      ax[0].user[n] = ax[0].grid[n];
   }
   for ( m = 1; m < ndef; m++ ) {
      ax[m].ngrid = ax[m-1].nsequ / ax[m].nsequ;
      count /= ax[m].ngrid;
      if ( m == ( ndef - 1 ) && count != 1 ) ax[m].ngrid *= count;
      for ( n = 0; n < ax[m].ngrid; n++ ) {
         ax[m].user[n] = ax[m].grid[n*((*nsubs)/ax[m-1].nsequ)];
      }
   }
   for ( m = 0; m < ndef; m++ ) {
      char	text[MAXSTRINGLEN];
      int	l = 0;
      int	ndone = 0;

      SHIP( " " );
      SHIP( ax[m].ctype );
      for ( n = 1; n < ax[m].ngrid; n++ ) {
         ax[m].step[n] = ax[m].user[n] - ax[m].user[n-1];
      }
      while ( ndone < ax[m].ngrid ) {
         l += sprintf( &text[l], " %d", ax[m].user[ndone++] );
         for ( n = ndone+1; n < ax[m].ngrid && ax[m].step[ndone] == ax[m].step[n]; n++ );
         if ( n > ( ndone + 1 ) ) {
            if ( ax[m].step[ndone] == 1 ) {
               l += sprintf( &text[l], ":%d", ax[m].user[n-1] );
            } else if ( ax[m].step[ndone] == 0 ) {
               l += sprintf( &text[l], "::%d", n - ndone + 1 );
            } else {
               l += sprintf( &text[l], ":%d:%d", ax[m].user[n-1], ax[m].step[ndone] );
            }
            ndone = n;
         }
      }
      SHIP( text );
   }
   RETURN( 0 );
}

#if	defined(TESTBED)

#include	"anyout.h"			/* anyout_c */
#include	"cancel.h"	 		/* cancel_c */
#include	"cmain.h"			/* C program */
#include	"finis.h"			/* finis_c */
#include	"init.h"			/* init_c */
#include	"dcdsetdef.h"			/* dscset */
#include	"usertext.h"			/* usertext_c */

MAIN_PROGRAM_ENTRY
{
   char			message[100];
   char			textb[81];
   dcdset_struct	*r;
   fchar		text;
   fint			nsubs;
   fint			output_level = 0;
   fint			rs;
   fint			*subsets = NULL;

   text.a = textb; text.l = sizeof( textb ) - 1;
   init_c( );
   while ( 1 ) {
      fint	input_level = 1;
      fint	ninp;

      ninp = usertext_c( text, &input_level, tofchar( "INSET=" ), tofchar( "Enter set" ) );
      cancel_c( tofchar( "INSET=" ) );
      if ( ninp == 0 ) break;
      textb[ninp] = 0;
      r = dcdset( textb );
      if ( r->status > 0 ) {
         nsubs = r->status;
         subsets = realloc( subsets, sizeof( fint ) * nsubs );
         memmove( subsets, r->subsets, sizeof( fint ) * nsubs );
         sprintf( message, "ORIGINAL: %.*s", nelc_c( text ), textb );
         anyout_c( &output_level, tofchar( message ) );
         rs = ecdset_c( text, tofchar( r->setname ), subsets, &nsubs );
         if ( rs == 0 ) {
            sprintf( message, "ENCODED : %.*s", nelc_c( text ), textb );
            anyout_c( &output_level, tofchar( message ) );
            r = dcdset( textb );
            if ( r->status == nsubs ) {
               fint	n;
               fint	okay;

               for ( okay =1, n = 0; n < nsubs; n++ ) {
                  okay = ( r->subsets[n] == subsets[n] );
               }
               if ( okay ) anyout_c( &output_level, tofchar( "OKAY" ) );
            }
         } else {
            sprintf( message, "ecdset = %d", rs );
            anyout_c( &output_level, tofchar( message ) );
         }
      }
   }
   finis_c( );
   return( 0 );
}

#endif
