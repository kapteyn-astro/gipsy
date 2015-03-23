/* colour.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            colour.dc1

Programme:    COLOUR

Purpose:      Loads a COLOUR Look Up Table in GIDS. This LUT can be
              activated with the USER button in the COLOR menu.

Category:     DISPLAY

File:         colour.c

Author:       K.G. Begeman

Description:  The programme scans first the directory $usr_dat/lut for
              LUT tables and second the direcotry $gip_dat/lut. A LUT
              table is a file with extension .lut which contains three
              columns and 256 rows. The first colomn contains the Red,
              the second the Green and the third the Blue intensities.
              Intensities run from 0.0 to 1.0. The programme will list
              all LUTs found before the user can enter with the keyword
              LUT= the wanted LUT.

Keywords:

** DISPLAY=   Name of display device [DEFAULT_DISPLAY].

   LUT=       Name of Look Up Table [programme quits].
              The programme keeps prompting for LUT until the default
              is used.

Updates:      Aug  1, 1991: KGB Document created.
              Dec 11, 1991: KGB Cancel replaced by reject.
              Apr 22, 1993: KGB Error in documentation repaired.

#<

*/

#include	"ctype.h"			/* <ctype.h> */
#include	"stddef.h"			/* <stddef.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"string.h"			/* <string.h> */
#include	"gipsyc.h"			/* GIPSY symbols */
#include	"cmain.h"			/* C main */
#include	"anyout.h"			/* define anyout_c */
#include	"cancel.h"			/* define cancel_c */
#include	"error.h"			/* define error_c */
#include	"finis.h"			/* define finis_c */
#include	"flist.h"			/* define flist_c */
#include	"gdi_close.h"			/* define gdi_close_c */
#include	"gdi_cinfo.h"			/* define gdi_cinfo_c */
#include	"gdi_colput.h"			/* define gdi_colput_c */
#include	"gdi_open.h"			/* define gdi_open_c */
#include	"init.h"			/* define init_c */
#include	"nelc.h"			/* define nelc_c */
#include	"reject.h"			/* define reject_c */
#include	"usertext.h"			/* define usertext_c */

#define	PROGRAM		"colour"		/* program name */
#define	VERSION		"1.0"			/* version of program */

#define	MAXCOLOUR	256			/* number of color entries */
#define	MAXLUTNAME	32			/* maximum length of lut name */
#define	MAXLUTS		200			/* maximum number of luts */
#define	NCOL		4			/* display 4 colouns on screen */
#define	WIDTH		20			/* width of column on screen */

static	char	*path[] = {			/* the search path */
   "usr_dat",					/* first USER files */
   "gip_dat",					/* second GIPSY files */
};

static struct color_struct {			/* contains the intensities */
   float	red;				/* red intensity */
   float	green;				/* green intensity */
   float	blue;				/* blue intensity */
} colour[MAXCOLOUR];

static struct lut_struct {			/* contains the LUTs */
   char	l_name[MAXLUTNAME+1];			/* name of lut */
   char	p_name[FILENAME_MAX+1];			/* name of directory */
} lut_table[MAXLUTS];


/*
 * fcmp is called by qsort.
 */

static	int	fcmp( const void *a, const void *b )
{
   const struct lut_struct	*la, *lb;	/* for correct types */

   la = a; lb = b;				/* assignments */
   return( strcmp( la->l_name, lb->l_name ) );	/* return to caller */
}


/*
 * getlut searches for all luts in all paths.
 */

static	int	getlut( void )
{
   int	n = 0;					/* loop counter */
   int	r = 0;					/* return value */

   while (n < (sizeof(path)/sizeof(char *))) {	/* through all paths */
      char	*p = getenv( path[n] );		/* translate next path */
      int	nlut = 0;			/* reset lut counter */

      if (p != NULL) {				/* it did translate */
         char	dirb[FILENAME_MAX+1];		/* the directory */
         char	lutb[MAXLUTNAME+1];		/* name of lut */
         fchar	dir;				/* points to dirb */
         fchar	lut;				/* points to lutb */

         sprintf( dirb, "%s/lut", p );		/* path to directory */
         dir = tofchar( dirb );			/* the directory */
         lut.a = lutb; lut.l = sizeof(lutb) - 1;/* the lut name */
         while (!flist_c( dir, lut )) {		/* scan directory */
            char	*ext;			/* points to extension */
            int		m = 0;			/* counter */

            lutb[nelc_c( lut )] = 0;		/* add trailing zero */
            ext = strstr( lutb, ".lut" );	/* find extension */
            if (ext != NULL) {			/* there is one */
               *ext = 0;			/* strip it of */
               while (m < r && strcmp( lutb, lut_table[m].l_name )) m++;
               if (m == r) {			/* add to table */
                  if ((r+nlut) < MAXLUTS) {
                     strcpy( lut_table[nlut+r].l_name, lutb );
                     strcpy( lut_table[nlut+r].p_name, dirb );
                     nlut += 1;
                  } else {
                     char	mes[80];		/* message */
                     fint	error_level = 1;	/* a warning */

                     sprintf( mes, "Not enough memory for lut %s!", lutb );
                     error_c( &error_level, tofchar( mes ) );
                  }
               }
            }
         }
      }
      n += 1;					/* next path */
      r += nlut;				/* increase counter */
   }
   return( r );					/* return to caller */
}


MAIN_PROGRAM_ENTRY				/* main program */
{
   fchar	gdi_name;			/* name of display */
   fint		gdi_id;				/* display id */
   fint		gdi_r;				/* return from gdi routines */
   fint		max;				/* max display value */
   fint		min;				/* min display value */
   fint		n;				/* loop counter */
   fint		nc;				/* number of display values */
   fint		res;				/* reserved display value */
   fint		values[MAXCOLOUR];		/* the display values */
   float	red[MAXCOLOUR];			/* the reds */
   float	blue[MAXCOLOUR];		/* the blues */
   float	green[MAXCOLOUR];		/* the greens */
   int		nluts;				/* the number of luts */

   init_c( );					/* contact HERMES */
   IDENTIFICATION( PROGRAM, VERSION );		/* reveal thyself */
   {
      fint		input_level = 2;	/* hidden keyword */
      static char	name[80];		/* for display name */

      gdi_name.a = name;   			/* assign to f character */
      gdi_name.l = sizeof( name );		/* the length */
      if (!usertext_c( gdi_name, &input_level, tofchar( "DISPLAY=" ),
         tofchar( "Enter name of display [DEFAULT_DISPLAY]" ) ) ) {
         strcpy( name, "DEFAULT_DISPLAY" );	/* the default */
         gdi_name.l = strlen( name );		/* the length */
      }
   }
   gdi_id = gdi_open_c( gdi_name );		/* open display */
   if (gdi_id < 0) {				/* error */
      fint	error_level = 4;		/* fatal error */

      error_c( &error_level, tofchar( "Cannot open display!" ) );
   }
   gdi_r = gdi_cinfo_c( &gdi_id, &min, &max, &nc, &res );
   if (gdi_r) {					/* error */
      fint	error_level = 4;		/* fatal error */ 

      error_c( &error_level, tofchar( "Cannot obtain colour info!" ) );
   }
   if (nc > MAXCOLOUR) {			/* error */
      fint	error_level = 4;		/* fatal error */

      error_c( &error_level, tofchar( "COLOUR buffer to small!" ) );
   }
   for (n = 0; n < nc; n++) {			/* loop to set the ... */
      values[n] = min + n;			/* ... display values */
   }
   nluts = getlut( );				/* get luts */
   if (nluts == 0) {				/* no luts */
      fint	error_level = 4;		/* fatal error */

      error_c( &error_level, tofchar( "No LUTS found!" ) );
   }
   qsort( lut_table, nluts, sizeof( struct lut_struct ), fcmp );
   for (n = 0; n < nluts; ) {			/* list LUTs */
      char	message[NCOL*WIDTH+1];		/* message buffer */
      fint	output_level = 1;		/* to screen */
      int	ncol;				/* column counter */
      int	w = 0;				/* witdh counter */

      for (ncol = 0; n < nluts && ncol < NCOL; ncol++) {
         int	l = 0;				/* counter */

         while (l < WIDTH && lut_table[n].l_name[l]) {
            message[w++] = lut_table[n].l_name[l++];
         }
         while (l++ < WIDTH) message[w++] = ' ';/* fillerup */
         n += 1;				/* next lut */
      }
      message[w] = 0;				/* add zero byte */
      anyout_c( &output_level, tofchar( message ) );
   }
   {
      char	lutb[MAXLUTNAME+1];		/* buffer for lut name */
      fchar	lut;				/* points to lutb */
      fint	input_level = 1;		/* default */
      fint	nel;				/* character counter */

      lut.a = lutb; lut.l = sizeof( lutb ) - 1;	/* assign f character */

      while ( ( nel = usertext_c( lut, &input_level, tofchar( "LUT=" ),
         tofchar( "Name of Look Up Table [programme quits]" ) ) ) ) {
         fint	nlut = 0;			/* reset */

         lutb[nel] = 0;				/* add trailing zero */
         while ((nlut < nluts) && strcmp( lutb, lut_table[nlut].l_name )) nlut++;
         if (nlut == nluts) {			/* error */
            reject_c( tofchar( "LUT=" ), tofchar( "LUT not found!" ) );
         } else {				/* o.k. */
            FILE	*f;			/* file descriptor */
            char	filename[FILENAME_MAX+1];

            sprintf( filename, "%s/%s.lut", lut_table[nlut].p_name, lutb );
            f = fopen( filename, "r" );		/* open file */
            if (f == NULL) {			/* error */
               reject_c( tofchar( "LUT=" ), tofchar( "LUT file not found!" ) );
            } else {				/* o.k. */
               for (n = 0; n < MAXCOLOUR; n++) {/* scan loop */
                  fscanf( f, "%e%e%e", &colour[n].red, &colour[n].green, &colour[n].blue );
               }
               fclose( f );			/* close file */
               for (n = 0; n < nc; n++) {	/* rebin loop */
                  int	m;			/* counter */

                  m = (float) (MAXCOLOUR - 1) / (float) (nc - 1) * (float) n + 0.5;
                  red[n]   = colour[m].red;	/* the red */
                  green[n] = colour[m].green;	/* the green */
                  blue[n]  = colour[m].blue;	/* the blue */
               }
               gdi_r = gdi_colput_c( &gdi_id, values, red, green, blue, &nc );
               if (gdi_r) {			/* error */
                  fint	error_level = 1;	/* warning */

                  error_c( &error_level, tofchar( "Cannot load LUT!" ) );
               }
               cancel_c( tofchar( "LUT=" ) );	/* cancel keyword */
            }
         }
      }
   }
   gdi_r = gdi_close_c( &gdi_id );		/* close link with display */
   finis_c( );					/* quit contact with HERMES */
   return( EXIT_SUCCESS );			/* return with status */
}
