/* dcdset.c

	Copyright (c) Kapteyn Laboratorium Groningen 1993
	All Rights Reserved.

#>            dcdset.dc2
Function:     dcdset

Purpose:      Decodes a string denoting a set and optional subsets into
              subset levels. The set MUST exist!

Category:     USER IO

File:         dcdset.c

Author:       K.G. Begeman

Call:         dcdset_struct *dcdset( char *string )

              dcdset returns a pointer to a dcdset_struct in static
              memory. The dcdset_struct is defined in dcdsetdef.h.

              typedef struct {
                 char setname[FILENAME_MAX+1]; // name of set
                 fint *subsets;                // decoded subset levels
                 fint status;                  // return status
              } dcdset_struct;

              The return status can be one of the following:

               >0  Number of subsets decoded.
               -1  Input string longer than internal buffer.
               -2  Empty input string.
               -3  Set does not exist.
               -4  Set name too long for buffer.
               -5  Could not allocate memory to contain axis info.
               -6  Axis name not unique.
               -7  Integer number expected.
               -8  Syntax error.
               -9  Could not allocate memory for storing grids positions.
              -10  Repeat argument <= 0.
              -11  Loop increment is 0.
              -12  Infinite loop.
              -13  Internal inconsistency.
              -14  Axis already defined.
              -15  Non existent subset.
              -16  Could not allocate memory for storing subset levels.

Warning:      dcdset closes the set before returning. This invalidates
              the internal set handle which then cannot be used anymore
              instead of the set name.
              For the FITS pseudo sets supported by GIPSY's Python binding,
              this behaviour has been suppressed, i.e sets which have the
              string "_tmp_" in their name are not closed.

Related Docs: dcdsetdef.dc3

Updates:      Nov 28, 1993: KGB, Document created.
              Feb  9, 2009: JPT, Added warning in document.
              Feb 18, 2009: JPT, Conditionally suppress set closing.

#<
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
#include	"gdsc_range.h"			/* gdsc_range_c */
#include	"gdsc_word.h"			/* gdsc_word_c */

#include	"dcdsetdef.h"			/* dcdset definitions */

#define	MAXSTRINGLEN	1024			/* max. length of string */
#define	MAXCTYPELEN	70			/* max. length of axis name */

typedef	struct {				/* the axis informations */
   char	ctype[MAXCTYPELEN];			/* axis name */
   fint	axnum;					/* axis sequence number */
   fint	count;					/* counter */
   fint	def;					/* axis defined ? */
   fint	glow;					/* lower grid along axis */
   fint	gupp;					/* upper grid along axis */
   fint	npos;					/* number of grids on axis */
   fint	*pos;					/* the grids on axis */
} ax_struct;					/* the struct */

#define	RETURN		{ \
   if ( ax != NULL ) { \
      int	n; \
      for ( n = 0; n < naxis; n++ ) { \
         if ( ax[n].pos != NULL ) free( ax[n].pos ); \
      } \
      free( ax ); \
   } \
   gerror = 0; \
   if ( open ) cclose( set, &gerror ); \
   return( &r ); \
}

/*
 * cclose conditionally closes a set if it does not have a name like "_tmp_...".
 * It is not clear why dcdset wants to close the set anyway, but
 * just to be sure we do not change this.
 */

static void cclose(fchar set, fint *error)
{
   char buffer[MAXSTRINGLEN];
   strncpy(buffer, set.a, set.l);
   buffer[set.l] = '\0';
   if (!strstr(buffer, "_tmp_")) gds_close_c(set, error);
}

/*
 * compar is called by qsort to sort the axis in the order as defined
 * in the command.
 */

static	int	compar( const void *a1, const void *a2 )
{
   const ax_struct	*b1 = a1;
   const ax_struct	*b2 = a2;

   return( b1->def - b2->def );
}

/*
 * parse is equivalent to strtok.
 */

static	char	*parse( char *s, const char *ct )
{
   static	char	*parse_p = NULL;
   const	char	*set;
   char			*rs = NULL;
   int			def = 0;

   if ((s == NULL) && (parse_p == NULL)) return( NULL );
   if (s != NULL) parse_p = s;
   while (*parse_p) {
      set = ct;
      while ((*set) && (*parse_p != *set)) set++;
      if (!(*set)) {
         if (!def) { rs = parse_p++; def = 1; } else { parse_p++; }
      } else {
         if (!def) { parse_p++; } else { *parse_p++ = 0; return( rs ); }
      }
   }
   if (!def) return( NULL ); else return( rs );
}

/*
 * dcdset parses the input string, which contains the setname and (optionally)
 * the defined subset grids. It returns a pointer to a dcdset_struct in static
 * memory which contains the decoded set name and subset level. The status
 * indicates whether an error occurred:
 * value of status      meaning
 *      >0              Number of subsets decoded.
 *      -1              Input string longer than internal buffer.
 *      -2              Empty input string.
 *      -3              Set does not exist.
 *      -4              Set name too long for buffer.
 *      -5              Could not allocate memory to contain axis info.
 *      -6		Axis name not unique.
 *      -7              Integer number expected.
 *      -8              Syntax error.
 *      -9              Could not allocate memory for storing grids positions.
 *     -10              Repeat argument <= 0.
 *     -11              Loop increment is 0.
 *     -12              Infinite loop.
 *     -13              Internal inconsistency.
 *     -14              Axis already defined.
 *     -15              Non existent subset.
 *     -16              Could not allocate memory for storing subset levels.
 *     -17              Unknown GDS error.
 */

dcdset_struct	*dcdset( char string[ ] )
{
   ax_struct		*ax = NULL;		/* the axis info */
   char			buffer[MAXSTRINGLEN];	/* local buffer */
   char			*separators = " ,";	/* separators */
   char			*sub;			/* pointer to parts in buffer */
   fchar		set;			/* pointer to set name */
   fint			axnum;			/* the current axis number */
   fint			clow, cupp;		/* coordinate words */
   fint			gerror = 0;		/* GDS error return code */
   fint			naxis = 0;		/* number of axis in set */
   fint			toplevel = 0;		/* top level in set */
   int			m;			/* counter */
   int			n;			/* counter */
   int			ndef = 0;		/* number of defined axes */
   int			open = 0;		/* set already open ? */
   int			subdim;			/* dimension of subset */
   static dcdset_struct	r = { "", NULL, 0 };	/* the static memory */

   if ( strlen( string ) >= sizeof( buffer ) ) {/* string too long */
      r.status = -1;				/* error code */
      RETURN;					/* we're done */
   }
   strcpy( buffer, string );			/* copy to own buffer */
   set.a = parse( buffer, separators );		/* get set name */
   if ( set.a == NULL ) {			/* empty string */
      r.status = -2;				/* error code */
      RETURN;					/* we're done */
   }
   set.l = strlen( set.a );			/* length of set name */
   if ( !tobool( gds_exist_c( set, &gerror ) ) || gerror ) {
      r.status = -3;				/* error code */
      RETURN;					/* we're done */
   }
   open = 1;					/* set is open */
   if ( set.l > FILENAME_MAX ) {		/* error */
      r.status = -4;				/* error code */
      RETURN;					/* we're done */
   }
   strcpy( r.setname, set.a  );			/* copy set name */
   /*
    * Now we get the axis names and dimensions of the set.
    */
   naxis = gdsc_ndims_c( set, &toplevel );	/* dimension of set */
   ax = calloc( naxis, sizeof( ax_struct ) );	/* allocate memory */
   if ( ax == NULL ) {				/* allocation error */
      r.status = -5;				/* error code */
      RETURN;					/* we're done */
   }
   /*
    * Now get the frame of the set and fill the ax_struct with the
    * essential information.
    */
   gdsc_range_c( set, &toplevel, &clow, &cupp, &gerror );
   if (gerror<0) {
      r.status = -17;
      RETURN;
   }
   for ( n = 0; n < naxis; n++ ) {		/* loop over all axes */
      fchar	ctype;				/* pointer to buffer */
      int	l;				/* counter */

      axnum = n + 1;				/* current axis number */
      ctype.a = ax[n].ctype; l = ctype.l = MAXCTYPELEN;
      gdsc_name_c( ctype, set, &axnum, &gerror );
      if (gerror<0) {
         r.status = -17;
         RETURN;
      }
      while ( --l && ( ctype.a[l] == ' ' )  ) ctype.a[l] = 0;
      ax[n].axnum = axnum;			/* store current axis number */
      ax[n].count = 0;				/* reset counter */
      ax[n].def = 0;				/* not defined */
      ax[n].glow = gdsc_grid_c( set, &axnum, &clow, &gerror );
      if (gerror<0) {
         r.status = -17;
         RETURN;
      }
      ax[n].gupp = gdsc_grid_c( set, &axnum, &cupp, &gerror );
      if (gerror<0) {
         r.status = -17;
         RETURN;
      }
      ax[n].npos = 0;				/* reset */
      ax[n].pos = NULL;				/* reset */
   }
   axnum = naxis;				/* default last axis */
   sub = parse( NULL, separators );		/* get next token */
   while ( sub != NULL ) {			/* parsing loop */
      int	len = strlen( sub );		/* length of mthis part */

      m = 0;					/* reset */
      for ( n = 0; n < len; n++ ) sub[n] = toupper( sub[n] );
      for ( n = 0; n < naxis; n++ ) {		/* loop to find axis name */
         if ( !strncmp( sub, ax[n].ctype, len ) ) {
            if ( m ) m = -1; else m = n + 1;	/* found an axis ? */
         }
      }
      switch( m ) {				/* what do we do now ? */
         case -1: {				/* axis name not unique */
            r.status = -6;			/* error code */
            RETURN;				/* we're done */
            break;				/* ? */
         }
         case 0: {				/* no axis name, so decode */
            char	*ptr = sub;		/* next part */
            char	*str = sub;		/* current part */
            long	n1 = 0, n2 = 0, n3 = 0;	/* the numbers */
            int		nn = 0;			/* the mode */

            n1 = strtol( str, &ptr, 10 );	/* first decode */
            if ( n1 == 0 && ptr == str ) {	/* decode error */
               r.status = -7;			/* error code */
               RETURN;				/* we're done */
            }
            nn = 1;				/* we have one number */
            if ( ptr[0] == ':' ) {		/* loop or repeat character */
               if ( ptr[1] == ':' ) {		/* repeat mode */
                  str = &ptr[2];		/* start of next decode */
                  n2 = strtol( str, &ptr, 10 );	/* second decode */
                  if ( n2 == 0 && ptr == str ) {/* decode error */
                     r.status = -7;		/* error code */
                     RETURN;			/* we're done */
                  }
                  nn = 2;			/* we have two numbers */
               } else {				/* loop mode */
                  str = &ptr[1];		/* start of next decode */
                  n2 = strtol( str, &ptr, 10 );	/* second decode */
                  if ( n2 == 0 && ptr == str ) {/* decode error */
                     r.status = -7;		/* error code */
                     RETURN;			/* we're done */
                  }
                  if ( ptr[0] == ':' ) {	/* third number */
                     str = &ptr[1];		/* start of next decode */
                     n3 = strtol( str, &ptr, 10 );
                     if ( n3 == 0 && ptr == str ) {
                        r.status = -7;		/* error code */
                        RETURN;			/* we're done */
                     }
                  } else {
                     n3 = 1;			/* default loop increment */
                  }
                  nn = 3;			/* we have three numbers */
               }
            }
            if ( ptr[0] != 0 ) {		/* the end of string ? */
               r.status = -8;			/* error code */
               RETURN;				/* we're done */
            }
            if ( !ndef ) ax[axnum-1].def = ++ndef;
            switch( nn ) {			/* what have we decoded */
               case 1: {			/* only one number */
                  ax[axnum-1].pos = realloc( ax[axnum-1].pos, ( ax[axnum-1].npos + 1 ) * sizeof( fint ) );
                  if ( ax[axnum-1].pos == NULL ) {
                     r.status = -9;		/* error code */
                     RETURN;			/* we're done */
                  }
                  ax[axnum-1].pos[ax[axnum-1].npos++] = n1;
                  break;
               }
               case 2: {			/* repeat loop */
                  if ( n2 < 1 ) {		/* error */
                     r.status = -10;		/* error code */
                     RETURN;			/* we're done */
                  }
                  ax[axnum-1].pos = realloc( ax[axnum-1].pos, ( ax[axnum-1].npos + n2 ) * sizeof( fint ) );
                  if ( ax[axnum-1].pos == NULL ) {
                     r.status = -9;		/* error code */
                     RETURN;			/* we're done */
                  }
                  while ( n2 > 0 ) {		/* loop to get grids */
                     ax[axnum-1].pos[ax[axnum-1].npos++] = n1;
                     n2--;			/* decrease */
                  }
                  break;
               }
               case 3: {			/* loop mode */
                  int	i;			/* counter */
                  int	nl;			/* number of positions */

                  if ( n3 == 0 ) {		/* illegal increment */
                     r.status = -11;		/* error code */
                     RETURN;			/* we're done */
                  }
                  if ( ( n2 > n1 && n3 < 0 ) || ( n2 < n1 && n3 > 0 ) ) {
                     r.status = -12;		/* error code */
                     RETURN;			/* we're done */
                  }
                  nl = ( n2 - n1 ) / n3 + 1;	/* number of positions */
                  ax[axnum-1].pos = realloc( ax[axnum-1].pos, ( ax[axnum-1].npos + nl ) * sizeof( fint ) );
                  if ( ax[axnum-1].pos == NULL ) {
                     r.status = -9;		/* error code */
                     RETURN;			/* we're done */
                  }
                  if ( n3 > 0 ) {		/* increasing */
                     for ( i = n1; i <= n2; i += n3 ) {
                        ax[axnum-1].pos[ax[axnum-1].npos++] = i;
                     }
                  } else {			/* decreasing */
                     for ( i = n1; i >= n2; i += n3 ) {
                        ax[axnum-1].pos[ax[axnum-1].npos++] = i;
                     }
                  }
                  break;
               }
               default: {			/* we should not get here */
                  r.status = -13;		/* error code */
                  RETURN;			/* we're done */
                  break;
               }
            }
            break;
         }
         default: {				/* matching axis name */
            if ( ax[m-1].def ) {		/* already defined */
               r.status = -14;			/* error code */
               RETURN;				/* we're done */
            } else {				/* o.k. */
               ax[m-1].def = ++ndef;		/* sequence */
               axnum = m;			/* current axis number */
            }
            break;
         }
      }
      sub = parse( NULL, separators );		/* get next part */
   }
   r.status = 1;				/* reset */
   subdim = naxis - ndef;			/* dimension of subset(s) */
   /*
    * Now we fill in the default positions and check the grids.
    */
   for ( m = 0; m < naxis; m++ ) {		/* loop over all axis */
      if ( ax[m].def ) {			/* defined */
         if ( !ax[m].npos ) {			/* use default */
            ax[m].npos = ax[m].gupp - ax[m].glow + 1;
            ax[m].pos = calloc( ax[m].npos, sizeof( fint ) );
            if ( ax[m].pos == NULL ) {		/* error */
               r.status = -9;			/* error code */
               RETURN;				/* we're done */
            }
            for ( n = 0; n < ax[m].npos; n++ ) {
               ax[m].pos[n] = ax[m].glow + n;	/* the default */
            }
         } else {				/* check positions */
            for ( n = 0; n < ax[m].npos; n++ ) {
               if ( ( ax[m].pos[n] < ax[m].glow ) || ( ax[m].pos[n] > ax[m].gupp ) ) {
                  r.status = -15;		/* error code */
                  RETURN;			/* we're done */
               }
            }
         }
         r.status *= ax[m].npos;		/* number of subsets */
      }
   }
   /*
    * Now sort the axes.
    */
   qsort( ax, naxis, sizeof( ax_struct ), compar );
   r.subsets = realloc( r.subsets, r.status * sizeof( fint ) );
   if ( r. subsets == NULL ) {			/* error */
      r.status = -16;				/* error code */
      RETURN;					/* we're done */
   }
   /*
    * Now calculate the subset coordinate words.
    */
   {
      fint	cerror = 0;			/* GDS error code return */
      fint	done = 0;			/* subset counter */

      while ( done < r.status ) {		/* loop over all subsets */
         fint	cw = 0;				/* initial subset level */

         for ( n = subdim; n < naxis; n++ ) {	/* loop over defined axes */
            fint	c = ax[n].count;	/* counter */

            if ( c == ax[n].npos ) {		/* reset counter ? */
               ax[n].count = c = 0;		/* reset */
               ax[n+1].count += 1;		/* increase next counter */
            }
            cw = gdsc_word_c( set, &ax[n].axnum, &ax[n].pos[c], &cw, &cerror );
         }
         if ( subdim < naxis ) {		/* in case subset is whole set */
            ax[subdim].count += 1;		/* increase first counter */
         }
         r.subsets[done++] = cw;		/* store subset level */
      }
   }
   RETURN;					/* we're done now */
}


#ifdef	TESTBED

#include	"cmain.h"
#include	"finis.h"
#include	"gdsc_grid.h"
#include	"init.h"
#include	"userfio.h"

MAIN_PROGRAM_ENTRY
{
   char		string[80];
   fchar	input;

   init_c( );
   input.a = string; input.l = sizeof( string ) - 1;
   string[input.l] = 0;
   while ( userftext( input, DFLT_DEF, "INSET=", "give set and subsets" ) ) {
      dcdset_struct	*p;

      cancel( "INSET=" );
      p = dcdset( string );
      anyoutf( ANYOUT_DEF, "status = %d", p->status );
      if ( p->status > 0 ) {
         int	n;
         fchar	set;
         fint	toplevel = 0;
         fint	naxis;

         anyoutf( ANYOUT_DEF, "set    = %s", p->setname );
         set.a = p->setname;
         set.l = strlen( p->setname );
         naxis = gdsc_ndims_c( set, &toplevel );
         for ( n = 0; n < p->status; n++ ) {
            char	text[80];
            fint	cw = p->subsets[n];
            int		m;

            sprintf( text, "Subset %3d = (", n + 1 );
            for ( m = 0; m < naxis; m++ ) {
               char	num[10];
               fint	axnum = m + 1;
               fint	cerror = 0;
               fint	grid;

               grid = gdsc_grid_c( set, &axnum, &cw, &cerror );
               if ( cerror >= 0 ) {
                  sprintf( num, "%d", grid );
               } else {
                  sprintf( num, "-" );
               }
               strcat( text, num );
               if ( m < ( naxis - 1 ) ) {
                  strcat( text, "," );
               }
            }
            strcat( text, ")" );
            anyoutf( ANYOUT_DEF, text );
         }
      }
   }
   finis_c( );
   return( EXIT_SUCCESS );
}
#endif
