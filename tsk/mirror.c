/* mirror.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991, 1993
	All Rights Reserved.

#>            mirror.dc1

Program:      MIRROR

Purpose:      Mirrors a two-dimensional image in x, y or both. It can
              also rotate the image over +90 or -90 degrees.

Category:     MANIPULATION

File:         mirror.c

Author:       K.G. Begeman

Keywords:

    INSET=    Set and subsets to mirror/rotate. The subsets must be
              two-dimensional. Maximum number of subsets is 2048.

*** BOX=      Box to mirror [whole subset].

    OPTION=   Enter mirror option. 1 Mirrors image around y axis, 2 around
              x axis and 3 around the centre op the box. Option 4 rotates
              the box +90 degrees and options 5 rotates the box -90 degrees.

    OUTSET=   Set and subsets for the mirror images. The output subsets will
              have the size as specified with BOX.

Updates:      Nov  7, 1991: KGB Document created.
              Apr 22, 1993: KGB Bug repaired, output set possible.
              Mar 17, 1994: KGB Rotate option implemented.
              May  3, 1994: KGB Commandline documentation corrected.
              Feb  1, 2000: JPT Increased number of subsets.
#<

*/

#include	"stdio.h"
#include	"stdlib.h"
#include	"gipsyc.h"
#include	"cmain.h"
#include	"error.h"
#include	"finis.h"
#include	"gdsasn.h"
#include	"gdsbox.h"
#include	"gdscss.h"
#include	"gdsc_fill.h"
#include	"gdsinp.h"
#include	"gdsi_read.h"
#include	"gdsi_write.h"
#include	"gdsout.h"
#include	"init.h"
#include	"status.h"
#include	"userint.h"

#define	CLASS		1
#define	CLASS_DIM	2
#define	KEY_BOX		tofchar("BOX=")
#define	KEY_INSET	tofchar("INSET=")
#define	KEY_OPTION	tofchar("OPTION=")
#define	KEY_OUTSET	tofchar("OUTSET=")
#define	MAXAXES		10
#define	MAXSETNAMELEN	80
#define	MAXSUBSETS	2048
#define	MES_BOX		tofchar(" ")
#define	MES_INSET	tofchar("Set and subsets to mirror/rotate")
#define	MES_OPTION	tofchar("Mirror? 1: y, 2: x 3: x+y, Rotate? 4: +90, 5: -90")
#define	MES_OUTSET	tofchar(" ")

static	char	setbi[MAXSETNAMELEN];
static	char	setbo[MAXSETNAMELEN];
static	fchar	seti = { setbi, MAXSETNAMELEN };
static	fchar	seto = { setbo, MAXSETNAMELEN };
static	fint	axpermi[MAXAXES];
static	fint	axpermo[MAXAXES];
static	fint	axsizei[MAXAXES];
static	fint	axsizeo[MAXAXES];
static	fint	bloi[CLASS_DIM];
static	fint	bloo[CLASS_DIM];
static	fint	bupi[CLASS_DIM];
static	fint	bupo[CLASS_DIM];
static	fint	subseti[MAXSUBSETS];
static	fint	subseto[MAXSUBSETS];

MAIN_PROGRAM_ENTRY
{
   fint		indata = 1;
   fint		ns;
   fint		nsub;
   fint		option;
   fint		xsize = 0;
   fint		ysize = 0;
   float	*in = NULL;
   float	*ou = NULL;

   init_c( );
   {
      fchar	key = KEY_INSET;
      fchar	mes = MES_INSET;
      fint	class = CLASS;
      fint	classdim = CLASS_DIM;
      fint	maxaxes = MAXAXES;
      fint	maxsubsets = MAXSUBSETS;
      fint	input_level = 0;
      fint	output_level = 3;

      nsub = gdsinp_c( seti ,
                       subseti ,
                       &maxsubsets ,
                       &input_level ,
                       key ,
                       mes ,
                       &output_level ,
                       axpermi ,
                       axsizei ,
                       &maxaxes ,
                       &class ,
                       &classdim );
   }
   {
      fchar	key = KEY_BOX;
      fchar	mes = MES_BOX;
      fint	input_level = 2;
      fint	mode = 0;
      fint	output_level = 3;

      gdsbox_c( bloi ,
                bupi ,
                seti ,
                subseti ,
                &input_level ,
                key ,
                mes ,
                &output_level ,
                &mode );
      for (ns = 0; ns < CLASS_DIM; ns++) {
         indata *= ( bupi[ns] - bloi[ns] + 1 );
         switch(ns) {
            case 0: {
               xsize = bupi[ns] - bloi[ns] + 1;
               break;
            }
            case 1: {
               ysize = bupi[ns] - bloi[ns] + 1;
               break;
            }
            default: {
               break;
            }
         }
      }
   }
   {
      fchar	key = KEY_OPTION;
      fchar	mes = MES_OPTION;
      fint	input_level = 0;
      fint	items = 1;

      userint_c( &option, &items, &input_level, key, mes );
      if (option > 5 || option < 1) {
         fint	error_level = 4;

         error_c( &error_level, tofchar( "Illegal Option!" ) );
      }
      in = calloc( indata, sizeof( float ) );
      if (in == NULL) {
         fint	error_level = 4;

         error_c( &error_level, tofchar( "Cannot create enough memory!" ) );
      }
      if (option > 3) {
         ou = calloc( indata, sizeof( float ) );
         if (ou == NULL) {
            fint	error_level = 4;

            error_c( &error_level, tofchar( "Cannot create enough memory!" ) );
         }
      } else {
         ou = in;
      }
   }
   {
      fchar	key = KEY_OUTSET;
      fchar	mes = MES_OUTSET;
      fint	class = CLASS;
      fint	maxaxes = MAXAXES;
      fint	maxsubsets = nsub;
      fint	input_level = 4;
      fint	output_level = 3;

      gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
      switch( option ) {
         case 1:
         case 2:
         case 3: {
            for ( ns = 0; ns < CLASS_DIM; ns++ ) {
               bloo[ns] = bloi[ns];
               bupo[ns] = bupi[ns];
            }
            break;
         }
         case 4:
         case 5: {
            for ( ns = 0; ns < CLASS_DIM; ns++ ) {
               bloo[ns] = bloi[(ns+1)%CLASS_DIM];
               bupo[ns] = bupi[(ns+1)%CLASS_DIM];
            }
            break;
         }
         default: {
            break;
         }
      }
      gdscss_c( KEY_OUTSET, bloo, bupo );
      nsub = gdsout_c( seto ,
                       subseto ,
                       &maxsubsets ,
                       &input_level ,
                       key ,
                       mes ,
                       &output_level ,
                       axpermo ,
                       axsizeo ,
                       &maxaxes );
   }
   for (ns = 0; ns < nsub; ns++) {
      fint	cwloi, cwupi;
      fint	cwloo, cwupo;
      fint	nread;
      fint	nwrite;
      fint	tid = 0;

      cwloi = gdsc_fill_c( seti, &subseti[ns], bloi );
      cwupi = gdsc_fill_c( seti, &subseti[ns], bupi );
      cwloo = gdsc_fill_c( seto, &subseto[ns], bloo );
      cwupo = gdsc_fill_c( seto, &subseto[ns], bupo );
      gdsi_read_c( seti, &cwloi, &cwupi, in, &indata, &nread, &tid );
      switch( option ) {
         case 1: {				/* flip around in x */
            fint	j;

            for (j = 0; j < ysize; j++) {
               fint	i1, i2, k;

               i1 = j * xsize;
               i2 = i1 + xsize - 1;
               for (k = 0; k < xsize / 2; k++) {
                  float	save = in[i1+k];

                  in[i1+k] = in[i2-k];
                  in[i2-k] = save;
               }
            }
            break;
         }
         case 2: {				/* flip around in y */
            fint	k;

            for (k = 0; k < ysize / 2; k++) {
               fint	i, j1, j2;

               j1 = k * xsize;
               j2 = ( ysize - k - 1 ) * xsize;
               for (i = 0; i < xsize; i++) {
                  float	save = in[j1+i];

                  in[j1+i] = in[j2+i];
                  in[j2+i] = save;
               }
            }
            break;
         }
         case 3: {				/* flip in x and y */
            {
               fint	j;

               for (j = 0; j < ysize; j++) {
                  fint	i1, i2, k;

                  i1 = j * xsize;
                  i2 = i1 + xsize - 1;
                  for (k = 0; k < xsize / 2; k++) {
                     float	save = in[i1+k];

                     in[i1+k] = in[i2-k];
                     in[i2-k] = save;
                  }
               }
            }
            {
               fint	k;

               for (k = 0; k < ysize / 2; k++) {
                  fint	i, j1, j2;

                  j1 = k * xsize;
                  j2 = ( ysize - k - 1 ) * xsize;
                  for (i = 0; i < xsize; i++) {
                     float	save = in[j1+i];

                     in[j1+i] = in[j2+i];
                     in[j2+i] = save;
                  }
               }
            }
            break;
         }
         case 4: {
            fint	i, j;

            for ( j = 0; j < ysize; j++ ) {
               for ( i = 0; i < xsize; i++ ) {
                  ou[(i*ysize)+ysize-j-1] = in[(j*xsize)+i];
               }
            }
            break;
         }
         case 5: {
            fint	i, j;

            for ( j = 0; j < ysize; j++ ) {
               for ( i = 0; i < xsize; i++ ) {
                  ou[((xsize-i-1)*ysize)+j] = in[(j*xsize)+i];
               }
            }
            break;
         }
         default: {
            break;
         }
      }
      gdsi_write_c( seto, &cwloo, &cwupo, ou, &indata, &nwrite, &tid );
   }
   finis_c( );
   return( EXIT_SUCCESS );
}
