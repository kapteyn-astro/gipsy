/* gmarker.c

	Copyright (c) Kapteyn Laboratorium Groningen 1995
	All Rights Reserved.

#>            gmarker.dc1

Program:      GMARKER

Purpose:      Mark user supplied positions in a graphics plane of GIDS.

Category:     DISPLAY

File:         gmarker.c

Author:       K.G. Begeman

Description:  GMARKER allows the user to mark some positions while using
              BLOT. GMARKER should be started when BLOT tells you to start
              defining the region.

Keywords:

** GRPLANE=   Graphics plane to draw in [2].

** GRCLEAR=   Clear graphics plane first ? [Y].

** GRCOLOR=   RGB color of graphics plane [0.667,0.833,0.500].

   POS=       Enter one position to mark in graphics plane [stop]. This
              keyword is repeated until default is used.

Updates:      Feb 14, 1995: KGB, Document created.
              Apr 15, 2009: VOG, Changed definition of nint macro to one
                                 that uses floor() to be consistent with 
                                 other routines that process coordinates.

#<

*/


#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"gipsyc.h"		/* GIPSY symbols */
#include	"cmain.h"		/* main program in C */
#include        "math.h"                /* needed for floor() */
#include	"finis.h"		/* defines finis_c */
#include	"gdi_close.h"		/* defines gdi_close_c */
#include	"gdi_error.h"		/* defines gdi_error_c */
#include	"gdi_ginfo.h"		/* defines gdi_ginfo_c */
#include	"gdi_grcol.h"		/* defines gdi_grcol_c */
#include	"gdi_gron.h"		/* defines gdi_gron_c */
#include	"gdi_grread.h"		/* defines gdi_grread_c */
#include	"gdi_grwrite.h"		/* defines gdi_grwrite_c */
#include	"gdi_iinfo.h"		/* defines gdi_iinfo_c */
#include	"gdi_open2.h"		/* defines gdi_open2_c */
#include	"gdspos.h"		/* defines gdspos_c */
#include	"gds_exist.h"		/* defines gds_exist_c */
#include	"gdsc_grid.h"		/* defines gdsc_grid_c */
#include	"gdsc_ndims.h"		/* defines gdsc_ndims_c */
#include	"gdsc_range.h"		/* defines gdsc_range_c */
#include	"init.h"		/* defines init_c */
#include	"userfio.h"		/* user input */

#define	check(e)	{if(e<0){gdi_error_c(&e,errmes);errorf(4,"%.*s",errmes.l,errmes.a);}}
#define	fmake(f,size)	{static char b[size+1];f.a=b;f.l=size;}
/* Pre Apr 2009 def.: #define	nint(f)		(f>0.0?(fint)(f+0.5):-1*((fint)(0.5-f))) */
#define nint(a) ( (int) floor( (double) (a) + 0.5 ) )


MAIN_PROGRAM_ENTRY
{
   char		*mask = NULL;
   double	pos[2];
   fchar	set;
   fchar	errmes;
   fint		blo[2], bup[2];
   fint		flo[2], fup[2];
   fint		gdi_id, gdi_stat;
   fint		gds_stat = 0;
   fint		nmem;
   fint		npos = 0;
   fint		one = 1;
   fint		subset;
   fint		packed = 0;
   fint		planes, planes_on;
   fint		pmask = 0;

   fmake( errmes, 80 );
   fmake( set, 80 );
   init_c( );
   gdi_id = gdi_open2_c( tofchar( " " ) );
   check( gdi_id );
   gdi_stat = gdi_iinfo_c( &gdi_id, set, &subset, blo, bup );
   check( gdi_stat );
   anyoutf( 16, "set = %.*s", set.l, set.a );
   anyoutf( 16, "frame = %d %d %d %d", blo[0], bup[0], blo[1], bup[1] );
   if ( !tobool( gds_exist_c( set, &gds_stat ) ) ) {
      errorf( 4, "Set %.*s does not exist", set.l, set.a );
   }
   if ( gdsc_ndims_c( set, &subset ) != 2 ) {
      errorf( 4, "Something wrong with subset" );
   } else {
      fint	cwlo, cwup;
      fint	m;
      fint	n, ndims, nul = 0;

      gdsc_range_c( set, &subset, &cwlo, &cwup, &gds_stat );
      ndims = gdsc_ndims_c( set, &nul );
      for ( m = n = 0; n < ndims && m < 2; n++ ) {
         fint	axnum = n + 1;
         fint	g;

         gds_stat = 0;
         g = gdsc_grid_c( set, &axnum, &cwlo, &gds_stat );
         if ( !gds_stat ) {
            flo[m] = g;
            fup[m] = gdsc_grid_c( set, &axnum, &cwup, &gds_stat );
            m += 1;
         }
      }
      anyoutf( 16, "Subset size: %d %d %d %d", flo[0], fup[0], flo[1], fup[1] );
      if ( ( blo[0] < flo[0] ) || ( blo[1] < flo[1] ) || 
         ( bup[0] > fup[0] ) || ( bup[1] > fup[1] ) ) {
         errorf( 4, "Image not within subset" );
      }
   }
   nmem = ( bup[0] - blo[0] + 1 ) * ( bup[1] - blo[1] + 1 );
   mask = malloc( nmem );
   if ( mask == NULL ) {
      errorf( 4, "Cannot allocate enough memory!" );
   }
   gdi_stat = gdi_grread_c( &gdi_id, (fint *)mask, &nmem, &packed );
   check( gdi_stat );
   gdi_stat = gdi_ginfo_c( &gdi_id, &planes, &planes_on );
   check( gdi_stat );
   /*
    * Get the user input.
    */
   {
      bool		clear = TRUE;
      fint		plane = 2;
      static float	rgb[3] = { 0.667, 0.833, 0.500 };

      (void) userfint( &plane, 1, 2, "GRPLANE=", "Graphics plane to draw in [%d]",
         plane );
      if ( plane < 1 || plane > planes ) {
         errorf( 4, "Wrong plane, must be in range from 1 to %d", planes );
      }
      pmask = ( 1 << ( plane - 1 ) );
      planes_on |= pmask;
      (void) userflog( &clear, 1, 2, "GRCLEAR=",
         "Clear graphics plane first ?[%s]", tobool( clear ) ? "Y" : "N" );
      if ( tobool( clear ) ) {
         int	n;

         for ( n = 0; n < nmem; n++ ) {
            if ( mask[n] & pmask ) mask[n] -= pmask;
         }
      }
      (void) userfreal( rgb, 3, 2, "GRCOLOR=",
         "RGB color of graphics plane [%4.2f,%4.2f,%4.2f]", rgb[0], rgb[1], rgb[2] );
      {
         int	n;

         for ( n = 0; n < sizeof( rgb ); n++ ) {
            if ( rgb[n] < 0.0 ) {
               rgb[n] = 0.0;
            } else if ( rgb[n] > 1.0 ) {
               rgb[n] = 1.0;
            }
         }
         gdi_stat = gdi_grcol_c( &gdi_id, &plane, &rgb[0], &rgb[1], &rgb[2] );
         check( gdi_stat );
      }
   }
   while ( gdspos_c( pos, &one, &one, tofchar( "POS=" ), tofchar( " " ), set, &subset ) ) {
      int	x0, x, y0, y;

      cancel_c( tofchar( "POS=" ) );
      x0 = nint( pos[0] );
      y0 = nint( pos[1] );
      if ( ( x0 < blo[0] ) || ( x0 > bup[0] ) ||
         ( y0 < blo[1] ) || ( y0 > bup[1] ) ) {
         errorf( 1, "Position outside image" );
      } else {
         int	xmin = x0 - 2;
         int	xmax = x0 + 2;
         int	ymin = y0 - 2;
         int	ymax = y0 + 2;

         if (!npos++) {
            gdi_stat = gdi_gron_c( &gdi_id, &planes_on );
            check( gdi_stat );
         }
         if ( xmin < blo[0] ) xmin = blo[0];
         if ( xmax > bup[0] ) xmax = bup[0];
         if ( ymin < blo[1] ) ymin = blo[1];
         if ( ymax > bup[1] ) ymax = bup[1];
         for ( x = xmin; x <= xmax; x++ ) {
            int	n = ( y0 - blo[1] ) * ( bup[0] - blo[0] + 1 ) + ( x - blo[0] );

            mask[n] |= pmask;
         }            
         for ( y = ymin; y <= ymax; y++ ) {
            int	n = ( y - blo[1] ) * ( bup[0] - blo[0] + 1 ) + ( x0 - blo[0] );

            mask[n] |= pmask;
         }
         gdi_stat = gdi_grwrite_c( &gdi_id, (fint *)mask, &nmem, &packed );
         check( gdi_stat );
      }
   }
   gdi_stat = gdi_close_c( &gdi_id );
   check( gdi_stat );
   finis_c( );
   return( EXIT_SUCCESS );
}
