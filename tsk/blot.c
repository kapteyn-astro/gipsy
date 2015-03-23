/* blot.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            blot.dc1

Program:      BLOT

Purpose:      Conditional transfer of maps using blotch region.

Category:     DISPLAY, MANIPULATION

File:         blot.c

Author:       K.G. Begeman

Description:  BLOT allows the user to define areas of interrest with the
              cursor of the display device. Data inside or outside the
              user defined region can be set to BLANK.

Keywords:

   INSET=     Set and subset(s) to be blotched.
              Maximum number of subsets is 500. Dimension of
              subsets must be 2.

** BOX=       Select area to be blotched [whole subset].

   CLIP=      Lower and upper clip levels for display
              [min. and max. from subsets].

   OUTSET=    Set and subset(s) for results.

   WITHIN=    Values withing blotch region transferred unchanged? [Y].
              If No, values outside blotch region are unchanged.

** DISPLAY=   Name of display device [DEFAULT_DISPLAY].

** GRPLANE=   Graphics plane to be used [1].

** GRCOLOR=   Color of graphics plane in RGB intensities [0.5,0.0,0.5].

** GRCLEAR=   Clear graphics plane before defining? [Y].

** GMARKER=   Deputize GMARKER to mark some positions in another graphics
              plane [N]. The keywords GRPLANE=, GRCOLOR= and GRCLEAR= for
              GMARKER will be changed into MPLANE=, MCOLOR= and MCLEAR=.

              ---------------------------------------------------------
              Next the programme will display the data and allow the
              user to define some regions. The user should go to the
              Etcetera menu in GIDS and press Region. After pressing
              Define the user can define several regions with the
              cursor. The left mouse button defines the polygon, the
              middle mouse button remove the last draw line of the polygon
              and the right mouse button closes the polygon.
              When all regions have been defined, the user should press
              Ready. Then the programme reads the regions defined by the
              user.
              ---------------------------------------------------------

Example:      <USER> BLOT
              BLOT  Version 1.0  (Nov 20 1991)
              <USER> BLOT INSET=ngc4214 1
              Set ngc4214 has 3 axes
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              FREQ-OHEL          from     1 to    63
              BOX range for set ngc4214 :
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              <USER> BLOT CLIP=
              <USER> BLOT OUTSET=mask
              Set mask has 3 axes
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              FREQ-OHEL          from     1 to     1
              <USER> BLOT WITHIN=
              <STATUS>  BLOT   +++ FINISHED +++

Updates:      Jan  4, 1991: KGB Document created.
              Dec 11, 1991: KGB Cancel replaced by reject.
              Feb 15, 1995: KGB Keyword GMARKER= implemented.

#<

*/


/*
 * include files:
 */

#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"cmain.h"		/* program written in C */
#include	"anyout.h"		/* define anyout_c */
#include	"deputy.h"		/* define deputy_c */
#include 	"error.h"		/* define error_c */
#include	"finis.h"		/* define finis_c */
#include	"gdi_open.h"		/* define gdi_open_c */
#include	"gdi_cinfo.h"		/* define gdi_cinfo_c */
#include	"gdi_close.h"		/* define gdi_close_c */
#include	"gdi_defimg.h"		/* define gdi_defimg_c */
#include	"gdi_ginfo.h"		/* define gdi_ginfo_c */
#include	"gdi_grclear.h"		/* define gdi_grclear_c */
#include	"gdi_grcol.h"		/* define gdi_grcol_c */
#include	"gdi_gron.h"		/* define gdi_gron_c */
#include	"gdi_grread.h"		/* define gdi_grread_c */
#include	"gdi_imwrite.h"		/* define gdi_imwrite_c */
#include	"gdi_mhead.h"		/* define gdi_mhead_c */
#include	"gdi_grregion.h"	/* define gdi_grreagion_c */
#include	"gdi_setid.h"		/* define gdi_setid_c */
#include	"gdsasn.h"		/* define gdsasn_c */
#include	"gdsbox.h"		/* define gdsbox_c */
#include	"gdsc_fill.h"		/* define gdsc_fill_c */
#include	"gdsd_rreal.h"		/* define gdsd_rreal_c */
#include	"gdsi_read.h"		/* define gdsi_read_c */
#include	"gdsi_write.h"		/* define gdsi_write_c */
#include	"gdsinp.h"		/* define gdsinp_c */
#include	"gdsout.h"		/* define gdsout_c */
#include	"init.h"		/* define init_c */
#include	"minmax3.h"		/* define minmax3_c */
#include	"reject.h"		/* define reject_c */
#include	"rminmax.h"		/* define rminmax_c */
#include	"setfblank.h"		/* define setfblank_c */
#include	"showsub1.h"		/* define showsub1_c */
#include	"status.h"		/* define status_c */
#include	"userint.h"		/* define userint_c */
#include	"userlog.h"		/* define userlog_c */
#include	"userreal.h"		/* define userreal_c */
#include	"usertext.h"		/* define usertext_c */
#include	"wminmax.h"		/* define wminmax_c */


/*
 * Defines:
 */

#define	CLASS		1		/* class of program */
#define	CLASSDIM	2		/* dimension of (sub)set(s) */
#define	FMT_CLIP	"Enter clipping levels [%g,%g]"
#define	KEY_BOX		tofchar(" ")
#define	KEY_CLIP	tofchar("CLIP=")
#define	KEY_DISPLAY	tofchar("DISPLAY=")
#define	KEY_GMARKER	tofchar("GMARKER=")
#define	KEY_GRCLEAR	tofchar("GRCLEAR=")
#define	KEY_GRCOLOR	tofchar("GRCOLOR=")
#define	KEY_GRPLANE	tofchar("GRPLANE=")
#define	KEY_INSET	tofchar("INSET=")
#define	KEY_OUTSET	tofchar("OUTSET=")
#define	KEY_WITHIN	tofchar("WITHIN=")
#define	MAXAXES		10		/* maximum number of axis in set */
#define	MAXDATA		10240		/* maximum number of pixels */
#define	MAXDEVNAMLEN	40		/* maximum length of device name */
#define	MAXMESSAGELEN	80		/* maximum length of message */
#define	MAXSETNAMLEN	80		/* maximum length of set name */
#define	MAXSUBSETS	500		/* maximum number of subsets */
#define	MES_BOX		tofchar(" ")
#define	MES_CLIP	tofchar("Enter clipping levels");
#define	MES_DISPLAY	tofchar("Name of display device [DEFAULT_DISPLAY]")
#define	MES_GMARKER	tofchar("Execute GMMARKER to mark some positions [N]")
#define	MES_GRCLEAR	tofchar("Clear graphics before defining blotch [Y]")
#define	MES_GRCOLOR	tofchar("RGB color of graphics plane [0.5,0.0,0.5]")
#define	MES_GRPLANE	tofchar("Graphics plane to use [1]")
#define	MES_INSET	tofchar("Set and subset(s) to be blotched")
#define	MES_OUTSET	tofchar("Set and subset(s) for results")
#define	MES_WITHIN	tofchar("Data inside blotch unchanged [Y]")
#define	PROGRAMME	"BLOT"		/* the name of this programme */
#define	STA_CLIP	tofchar("Determining default clip levels")
#define	VERSION		"1.0"		/* change version number here */

#define	finit(f,s)	{ \
				static	char	buf[s+1]; \
				int		i; \
				for (i = 0; i < s; buf[i++] = ' '); \
				buf[s] = '\0'; \
				f.a = buf; \
				f.l = s; \
			}


/*
 * The workhorse.
 */

MAIN_PROGRAM_ENTRY				/* main program */
{
   fchar		display;		/* name of display device */
   fchar		set1;			/* name of input set */
   fchar		set2;			/* name of output set */
   fint			axperm1[MAXAXES];	/* axis permutation array */
   fint			axperm2[MAXAXES];	/* axis permutation array */
   fint			axsize1[MAXAXES];	/* axis size array */
   fint			axsize2[MAXAXES];	/* axis size array */
   fint			blank_color;		/* reserved display value */
   fint			blo[CLASSDIM];		/* lower box coordinates */
   fint			bhi[CLASSDIM];		/* upper box coordinates */
   fint			clear;			/* clear graphics */
   fint			cwhi1;			/* upper coordinate word */
   fint			cwhi2;			/* upper coordinate word */
   fint			cwlo1;			/* lower coordinate word */
   fint			cwlo2;			/* lower coordinate word */
   fint			display_id;		/* id of display device */
   fint			display_stat;		/* status of gdi call */
   fint			gmarker;		/* start GMARKER ? */
   fint			max_color;		/* maximum display value */
   fint			min_color;		/* minimum display value */
   fint			nblanks[MAXSUBSETS];	/* the number of blanks */
   fint			ncolors;		/* number of display values */
   fint			nplanes;		/* number of graphics planes */
   fint			ns;			/* subset counter */
   fint			nsubs;			/* number of subsets */
   fint			onmask;			/* planes which are on */
   fint			plane;			/* graphics plane number */
   fint			pmask;			/* graphics plane mask */
   fint			subsets1[MAXSUBSETS];	/* buffer for subset levels */
   fint			subsets2[MAXSUBSETS];	/* buffer for subset levels */
   fint			within;			/* values within blotch unchanged */
   fint			wrong_input;		/* flag */
   float		blank;			/* blank value */
   float		bscale;			/* real scaling factor */
   float		cscale;			/* display scaling factor */
   float		bzero;			/* real offset */
   float		czero;			/* display offset */
   float		clip[CLASSDIM];		/* clip levels */
   float		bdata[MAXDATA];		/* real data */
   float		datamax[MAXSUBSETS];	/* the maxima */
   float		datamin[MAXSUBSETS];	/* the minima */
   unsigned char	cdata[MAXDATA];		/* display data */

   init_c( );					/* get in touch with HERMES */
   IDENTIFICATION( PROGRAMME, VERSION );	/* identify ! */
   setfblank_c( &blank );			/* get blank value */
   finit( set1, MAXSETNAMLEN );			/* initialize f character */
   /*
    * Here we get the input set and subsets from the user.
    */
   {						/* get set and subsets */
      fchar	key, mes;			/* keyword and message */
      fint	class = CLASS;			/* class two application */
      fint	classdim = CLASSDIM;		/* dimension of subsets */
      fint	input_level = 0;		/* input level (no default) */
      fint	maxaxes = MAXAXES;
      fint	maxsubsets = MAXSUBSETS;	/* maximum number of subsets */
      fint	output_level = 11;		/* output level */

      key = KEY_INSET;				/* the keyword */
      mes = MES_INSET;				/* the message */
      nsubs = gdsinp_c( set1 ,			/* name of set */
                        subsets1 ,		/* levels of subsets */
                        &maxsubsets ,		/* maximum number of subsets */
                        &input_level ,		/* default level */
                        key ,			/* keyword */
                        mes ,			/* message */
                        &output_level ,		/* output level */
                        axperm1 ,		/* axis permutation */
                        axsize1 ,		/* axis size */
                        &maxaxes ,		/* maximum number of axes */
                        &class ,		/* class of application */
                        &classdim );		/* dimension of class */
   }
   /*
    * Now we allow the user to select a part of the subsets to blotch.
    */
   {
      fchar	key, mes;			/* keyword and message */
      fint	box_mode = 0;			/* type of box */
      fint	input_level = 2;		/* hidden keyword */
      fint	output_level = 11;		/* output level */

      key = KEY_BOX;				/* the keyword */
      mes = MES_BOX;				/* the message */
      gdsbox_c( blo ,				/* the lower box grids */
                bhi ,				/* the upper box grids */
                set1 ,				/* the set name */
                subsets1 ,			/* the subset level */
                &input_level ,			/* the input level */
                key ,				/* the keyword */
                mes ,				/* the message */
                &output_level ,			/* the output level */
                &box_mode );			/* the box type */
   }
   /*
    * We need clip levels from the user for displaying the images.
    * Here is where we get them.
    */
   {						/* now get default clippings */
      fint	change = 0;			/* no change in minmax */

      status_c( STA_CLIP );			/* status message */
      rminmax_c( set1 ,				/* name of set */
                 subsets1 ,			/* the subsets */
                 datamin ,			/* the minimum */
                 datamax ,			/* the maximum */
                 nblanks ,			/* the blank count */
                 &nsubs ,			/* number of subsets */
                 &change );			/* change ? NO */
      clip[0] = clip[1] = blank;		/* set them to blank */
      for (ns = 0; ns < nsubs; ns++) {		/* loop over subsets */
         if (datamin[ns] != blank) {		/* defined data */
            if (clip[0] == blank) {		/* not yet defined */
               clip[0] = datamin[ns];		/* define minimum */
               clip[1] = datamax[ns];		/* define maximum */
            } else {				/* already defined */
               if (clip[0] > datamin[ns]) {	/* smaller than before */
                  clip[0] = datamin[ns];	/* new minimum */
               }
               if (clip[1] < datamax[ns]) {	/* larger than before */
                  clip[1] = datamax[ns];	/* new maximum */
               }
            }
         }
      }
   }
   do {						/* get the clippings */
      char	mes[MAXMESSAGELEN];		/* buffer for message */
      fchar 	key;				/* keyword */
      fchar	message;			/* message for user */
      fint	input_level;			/* default possible ?*/
      fint	n_items = CLASSDIM;		/* dimension is two */
      float	dummy[CLASSDIM];		/* dummy clippings */

      dummy[0] = clip[0];			/* just copy the ... */
      dummy[1] = clip[1];			/* ... default clippings */
      key = KEY_CLIP;				/* the keyword */
      if (clip[0] == clip[1]) {			/* no range left */
         input_level = 4;			/* exact, no default */
         message = MES_CLIP;			/* message for user */
      } else {
         input_level = 1;			/* default */
         sprintf( mes, FMT_CLIP, clip[0], clip[1] );
         message = tofchar( mes );		/* message for user */
      }
      (void) userreal_c( dummy ,		/* the clippings */
                         &n_items ,		/* how many clippings */
                         &input_level ,		/* default or not */
                         key ,			/* keyword */
                         message );		/* message */
      if (dummy[0] >= dummy[1]) {		/* cleary wrong input */
         reject_c( key, tofchar( "Wrong clip levels (max <= min)!" ) );
         wrong_input = 1;			/* set flag */
      } else {
         wrong_input = 0;			/* clear flag */
         clip[0] = dummy[0];			/* just copy the ... */
         clip[1] = dummy[1];			/* clippings back */
      }
   } while (wrong_input);			/* we've got the clippings */
   /*
    * Here we get the output set and subsets from the user.
    */
   finit( set2, MAXSETNAMLEN );			/* initialize f character */
   {						/* get set and subsets */
      fchar	key, mes;			/* keyword and message */
      fint	class = CLASS;			/* class of application */
      fint	input_level = 4;		/* input level (no default) */
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	maxsubsets = nsubs;		/* maximum number of subsets */
      fint	output_level = 11;		/* output level */

      gdsasn_c( KEY_INSET, KEY_OUTSET, &class );/* assign coordinate system */
      key = KEY_OUTSET;				/* the keyword */
      mes = MES_OUTSET;				/* the message */
      nsubs = gdsout_c( set2 ,			/* name of set */
                        subsets2 ,		/* levels of subsets */
                        &maxsubsets ,		/* maximum number of subsets */
                        &input_level ,		/* default level */
                        key ,			/* keyword */
                        mes ,			/* message */
                        &output_level ,		/* output level */
                        axperm2 ,		/* axis permutation */
                        axsize2 ,		/* axis size */
                        &maxaxes );
   }
   /*
    * Now we need to know whether we have to blank inside or outside
    * the region defined by the user.
    */
   {
      bool	v;				/* logical */
      fchar	key, mes;			/* keyword and message */
      fint	input_level = 1;		/* default allowed */
      fint	nitems = 1;			/* number of items */

      key = KEY_WITHIN;				/* the keyword */
      mes = MES_WITHIN;				/* the message */
      v = TRUE;					/* set default */
      (void) userlog_c( &v ,			/* the value */
                        &nitems ,		/* the number of items */
                        &input_level ,	  	/* the input level */
                        key ,			/* the keyword */
                        mes );			/* the message */
      within = tobool( v );			/* to c terminology */
   }
   /*
    * Next we obtain the name of the display device.
    */
   finit( display, MAXDEVNAMLEN );		/* initialize f character */
   {						/* get display device */
      fchar	key, mes;			/* keyword and message */
      fint	input_level = 2;		/* input level (hidden) */

      key = KEY_DISPLAY;			/* the keyword */
      mes = MES_DISPLAY;			/* the message */
      (void) usertext_c( display ,		/* name of display device */
                         &input_level ,		/* input  level */
                         key ,			/* keyword */
                         mes );			/* message */
   }
   display_id = gdi_open_c( display );		/* open display device */
   if (display_id < 0) {			/* cannot open display */
      fint	error_level = 4;		/* error level (fatal) */

      error_c( &error_level,			/* level of error */
               tofchar("Unable to open display!" ) );
   }
   /*
    * We try to get the display levels from the display and calculate
    * the scaling factors in the next part.
    */
   display_stat = gdi_cinfo_c( &display_id,	/* display id */
                               &min_color,	/* minimum display value */
                               &max_color,	/* maximum display value */
                               &ncolors,	/* number of display values */
                               &blank_color );	/* reserved display value */
   bscale = ( clip[1] - clip[0] ) / (float) ( max_color - min_color );
   bzero  = ( max_color * clip[0] - min_color * clip[1] ) /
      (float) (max_color - min_color );
   cscale = (float) ( max_color - min_color ) / ( clip[1] - clip[0] );
   czero  = ( clip[1] * min_color - clip[0] * max_color ) /
      ( clip[1] - clip[0] );
   /*
    * We get the graphics status below.
    */
   display_stat = gdi_ginfo_c( &display_id ,	/* display id */
                               &nplanes ,	/* number of planes */
                               &onmask );	/* which are on */
   /*
    * The next part gets the plane number and turns is on.
    */
   do {
      fchar	key, mes;			/* keyword and message */
      fint	input_level = 2;		/* hidden keyword */
      fint	nitems = 1;			/* only one item */

      plane = 1;				/* the default */
      key = KEY_GRPLANE;			/* the keyword */
      mes = MES_GRPLANE;			/* the message */
      (void) userint_c( &plane ,		/* plane number */
                        &nitems ,		/* the number of items */
                        &input_level ,		/* the input level */
                        key ,			/* the keyword */
                        mes );			/* the message */
      if (plane < 1 || plane > nplanes) {	/* wrong plane */
         reject_c( key, tofchar( "Illegal plane!" ) );
         wrong_input = 1;			/* set flag */
      } else {					/* plane number is correct */
         pmask = (1<<(plane-1));		/* create plane mask */
         if (!(pmask & onmask)) {		/* do something */
            fint	nmask = pmask | onmask;	/* leave old planes on */

            display_stat = gdi_gron_c( &display_id ,
                                       &nmask );
         }
         if (display_stat) {			/* error */
            char	error_message[80];	/* message */
            fint	error_level = 4;	/* fatal error */

            sprintf( error_message, "gdi_gron    = %d", display_stat );
            error_c( &error_level, tofchar( error_message ) );
         }
         wrong_input = 0;			/* input o.k. */
      }
   } while (wrong_input);			/* end of loop */
   /*
    * The next part gets the colour of the plane.
    */
   do {
      fchar 	key, mes;			/* keyword and message */
      fint	input_level = 6;		/* hidden keyword, exact */
      fint	n;				/* loop counter */
      fint	nitems = 3;			/* three items */
      fint	okay = 1;			/* input okay */
      float	rgb[3];				/* rgb color code */

      rgb[0] = 0.5; rgb[1] = 0.0; rgb[2] = 0.5;	/* default */
      key = KEY_GRCOLOR;			/* keyword */
      mes = MES_GRCOLOR;			/* message */
      (void) userreal_c( rgb ,			/* the colours */
                         &nitems ,		/* number of colours */
                         &input_level ,		/* the input level */
                         key ,			/* the keyword */
                         mes );			/* the message */
      for (n = 0; n < nitems; n++) {		/* check colours */
         if (rgb[n] < 0.0 || rgb[n] > 1.0) okay = 0;
      }
      if (okay) {				/* set colours */
         display_stat = gdi_grcol_c( &display_id ,
                                     &plane ,	/* the plane */
                                     &rgb[0] ,	/* red */
                                     &rgb[1] ,	/* green */
                                     &rgb[2] );	/* blue */
         if (display_stat) {			/* error */
            char	error_message[80];	/* message */
            fint	error_level = 4;	/* fatal error */

            sprintf( error_message, "gdi_grcol    = %d", display_stat );
            error_c( &error_level, tofchar( error_message ) );
         }
         wrong_input = 0;			/* o.k. leave loop */
      } else {					/* problems */
         reject_c( key, tofchar( "Colors outside range (0.0 - 1.0)!" ) );
         wrong_input = 1;			/* wrong */
      }
   } while (wrong_input);			/* end of loop */
   /*
    * We allow the user to save the defined regions.
    */
   {
      bool	v;				/* logical */
      fchar	key, mes;			/* keyword and message */
      fint	input_level = 2;		/* hidden keyword */
      fint	nitems = 1;			/* only one */

      v = TRUE;					/* default */
      key = KEY_GRCLEAR;			/* keyword */
      mes = MES_GRCLEAR;			/* message */
      (void) userlog_c( &v ,			/* the value */
                        &nitems ,		/* number of items */
                        &input_level ,		/* the input level */
                        key ,			/* the keyword */
                        mes );			/* the message */
      clear = tobool( v );			/* -> C */
   }
   /*
    * We allow the user to mark some positions in another graphics plane
    * by starting GMARKER.
    */
   {
      bool	v;				/* logical */
      fchar	key, mes;			/* keyword and message */
      fint	input_level = 2;		/* hidden keyword */
      fint	nitems = 1;			/* only one */

      v = FALSE;				/* default */
      key = KEY_GMARKER;			/* keyword */
      mes = MES_GMARKER;			/* message */
      (void) userlog_c( &v ,			/* the value */
                        &nitems ,		/* number of items */
                        &input_level ,		/* the input level */
                        key ,			/* the keyword */
                        mes );			/* the message */
      gmarker = tobool( v );			/* -> C */
   }
   /*
    * Now we define the image and start rolling.
    */
   display_stat = gdi_defimg_c( &display_id ,	/* image id */
                                blo ,		/* lower edge */
                                bhi ,		/* upper edge */
                                &bscale ,	/* scaling factor */
                                &bzero );	/* offset */
   for (ns = 0; ns < nsubs; ns++) {		/* start rolling */
      fint	count = 0;			/* counter for minmax3 */
      fint	maxdata = MAXDATA;		/* size of data buffers */
      fint	ndata;				/* counts them */
      fint	transfer_id1 = 0;		/* transfer id input set */
      fint	transfer_id2 = 0;		/* trassfer id output set */

      display_stat = gdi_setid_c( &display_id, set1, &subsets1[ns], axperm1 );
      cwlo1 = gdsc_fill_c( set1, &subsets1[ns], blo );
      cwhi1 = gdsc_fill_c( set1, &subsets1[ns], bhi );
      cwlo2 = gdsc_fill_c( set2, &subsets2[ns], blo );
      cwhi2 = gdsc_fill_c( set2, &subsets2[ns], bhi );
      showsub1_c( set1, &subsets1[ns], axperm1 );
      do {					/* the LOOP */
         fint	packed = 0;			/* packing code */
         int	n;				/* loop counter */

         gdsi_read_c( set1 ,			/* input set name */
                      &cwlo1 ,			/* lower c.w. */
                      &cwhi1 ,			/* upper c.w. */
                      bdata ,			/* the data */
                      &maxdata ,		/* maximum */
                      &ndata,			/* done */
                      &transfer_id1 );		/* transfer id */
         for (n = 0; n < ndata; n++) {		/* to display --> */
            if (bdata[n] == blank) {		/* blank ? */
               cdata[n] = blank_color;		/* set to display blank */
            } else if (bdata[n] > clip[1]) {	/* too large */
               cdata[n] = max_color;		/* set to max */
            } else if (bdata[n] < clip[0]) {	/* too small */
               cdata[n] = min_color;		/* set to min */
            } else {				/* scale it */
               cdata[n] = cscale * bdata[n] + czero + 0.5;
            }
         }
         display_stat = gdi_imwrite_c( &display_id ,
                                       (fint *) cdata ,
                                       &ndata ,
                                       &packed );
         if (display_stat != 0) {		/* error */
            char	string[80];		/* message */
            fint	error_level = 4;	/* fatal error */

            sprintf( string, "gdi_wdata  = %d\n", display_stat );
            error_c( &error_level, tofchar( string ) );
         }
      } while (transfer_id1);			/* until all displayed */
      if (clear) {				/* clear blotch plane */
         display_stat = gdi_grclear_c( &display_id, &pmask );
      }
      if (ns == 0) {				/* first time */
         fint	output_level = 8;		/* not for experienced users */

         if (gmarker) {
            fint	irc;

            subst_c( tofchar( "GRPLANE=MPLANE=GRCOLOR=MCOLOR=GRCLEAR=MCLEAR=" ), &irc );
            if ( irc < 0 ) {
               char	error_message[80];
               fint	error_level = 4;

               sprintf( error_message, "subst error %d", (int) irc );
               error_c( &error_level, tofchar( error_message ) );
            }
            deputy_c( tofchar( "GMARKER" ), &irc );
            if ( irc < 0 ) {
               char	error_message[80];
               fint	error_level = 4;

               sprintf( error_message, "deputy error %d", (int) irc );
               error_c( &error_level, tofchar( error_message ) );
            }
         }
         anyout_c( &output_level, tofchar( "Goto the Etcetera menu in GIDS and press Region!" ) );
         anyout_c( &output_level, tofchar( "Press Define to start region definition with mouse" ) );
         anyout_c( &output_level, tofchar( "Left   mouse button = Draw polygon" ) );
         anyout_c( &output_level, tofchar( "Middle mouse button = Remove polygon" ) );
         anyout_c( &output_level, tofchar( "Right  mouse button = Close polygon" ) );
         anyout_c( &output_level, tofchar( "Press Ready when ready" ) );
      }
      status_c( tofchar( "Press DEFINE to start, READY when ready!\a" ));
      display_stat = gdi_grregion_c( &display_id, &pmask );
      if (display_stat) {			/* error */
         char	error_message[80];		/* message */
         fint	error_level = 4;		/* fatal error */

         sprintf( error_message, "gdi_grregion = %d", display_stat );
         error_c( &error_level, tofchar( error_message ) );
      }
      showsub1_c( set2, &subsets2[ns], axperm2 );
      do {					/* read graphics data */
         fint	packed = 0;			/* packing code */
         int	n;				/* loop counter */

         gdsi_read_c( set1 ,			/* name of input set */
                      &cwlo1 ,			/* lower c.w. */
                      &cwhi1 ,			/* upper c.w. */
                      bdata ,			/* the data */
                      &maxdata ,		/* maximum */
                      &ndata ,			/* done */
                      &transfer_id1 );		/* transfer id */
         display_stat = gdi_grread_c( &display_id ,
                                      (fint *)cdata ,
                                      &ndata ,
                                      &packed );
         if (display_stat != 0) {		/* error */
            char	string[80];		/* message */
            fint	error_level = 4;	/* fatal error */

            sprintf( string, "gdi_grread   = %d\n", display_stat );
            error_c( &error_level, tofchar( string ) );
         }
         if (within) {				/* change outside blotch */
            for (n = 0; n < ndata; n++) { 	/* data loop */
               if (!(cdata[n] & pmask)) {	/* it's off */
                  bdata[n] = blank;		/* set to blank */
               }
            }
         } else {				/* change inside blotch */
            for (n = 0; n < ndata; n++) {	/* data loop */
               if ((cdata[n] & pmask)) {	/* it's on */
                  bdata[n] = blank;		/* set to blank */
               }
            }
         }
         gdsi_write_c( set2 ,			/* name of output set */
                       &cwlo2 ,			/* lower c.w. */
                       &cwhi2 ,			/* upper c.w. */
                       bdata ,			/* output data */
                       &ndata ,			/* the amount */
                       &ndata ,			/* done */
                       &transfer_id2 );		/* transfer id */
         minmax3_c( bdata ,			/* the data */
                    &ndata ,			/* the number */
                    &datamin[ns] ,		/* the running min */
                    &datamax[ns] ,		/* the running max */
                    &nblanks[ns] ,		/* the running blanks */
                    &count );			/* the counter */
      } while (transfer_id1);			/* until all done */
   }
   /*
    * Finally we update the minima and maxima and close the display.
    */
   {
      fint	change = 1;			/* delete intersecting levels */

      wminmax_c( set2 ,				/* the output set name */
                 subsets2 ,			/* the subsets */
                 datamin ,			/* the minima */
                 datamax , 			/* the maxima */
                 nblanks ,			/* the blanks */
                 &nsubs ,			/* number of subsets */
                 &change );			/* change flag */
   }
   if (!(pmask & onmask)) {			/* do something */
      display_stat = gdi_gron_c( &display_id ,
                                 &onmask );
   }
   display_stat = gdi_close_c( &display_id );	/* close display */
   if (display_stat != 0) {			/* error closing display */
      fint	error_level = 4;		/* error level (fatal) */

      error_c( &error_level,			/* level of error */
               tofchar("Unable to close display!" ) );
   }
   finis_c( );					/* quit working with HERMES */
   return( EXIT_SUCCESS );			/* return with status */
}
