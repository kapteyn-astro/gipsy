/* view.c -X

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            view.dc1
Program:      VIEW

Purpose:      Displays two dimensional images.

Category:     DISPLAY

File:         view.c

Author:       K.G. Begeman

Keywords:

   INSET=     Set and subset(s) to be displayed.
              Maximum number of subsets is 2048. Dimension of
              subsets must be 2.

** BOX=       Select area which to display [whole subset].

   CLIP=      Lower and upper clip levels [min. and max. from subsets].

** DISPLAY=   Name of display device [DEFAULT_DISPLAY].

** RECORD=    Record images for movie ? [N].

** FIRSTRECORD= If RECORD=Y, give here the record number for the first
              subset [1].

   NEXT=      Display next subset ? [Y].
              If more than one subset should be displayed and the images
              should not be recorded, this keyword is asked.
              If the images should be recorded, this keyword is asked only
              once to allow the user to position the image.

** ANYOUT=    Device to which output is directed:                  [8]
              Redirect output messages for INSET= and BOX= keywords.

Notes:        You first have to start GIDS (Gipsy Image Display System)
              with INIDISPLAY before any display programme can be run.

              ANYOUT= redirects output to:
              0  use default [set by HERMES to 3 but can be changed by user]
              1  terminal
              2  LOG file
              8  terminal, suppressed in "experienced mode"
              16 terminal, only when in "test mode"
              These devices can be combined by adding the codes.
              
              If GIDS cannot be started because the default visual is not
              8-bit PseudoColor, SLICEVIEW will be started instead.
              Of the keywords mentioned above, only INSET= will then be
              effective. For other keywords see SLICEVIEW's help menu or
              sliceview.dc1.

Updates:      Nov 26, 1990: KGB Document created.
              Dec 11, 1991: KGB Cancel replaced by reject.
              Nov  2, 1992: VOG ANYOUT= keyword added
              Jun 26, 1995: KGB Keyword FIRSTRECORD= implemented.
              May 17, 2001: JPT Redirect to SLICEVIEW if GIDS is unavailable.
              Nov 22, 2001: JPT Use DefaultScreen in pseudocolor test.
              Jun 27, 2003: VOG MAXSETNAMLEN = FILENAME_MAX
              Mar 29, 2005: JPT Max number of subsets increased to 2048.
              May 31, 2010: VOG Removed pseudocolor test. GIDS supports
                                DirectColor too.

#<

*/


/*
 * include files:
 */

#include        "stddef.h"
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"cmain.h"		/* program written in C */
#include	"anyout.h"		/* defines anyout_c */
#include	"axunit.h"		/* defines axunit_c */
#include	"cancel.h"		/* defines cancel_c */
#include	"cotrans.h"		/* defines cotrans_c */
#include 	"error.h"		/* defines error_c */
#include	"finis.h"		/* defines finis_c */
#include	"gdi_open.h"		/* defines gdi_open_c */
#include	"gdi_cinfo.h"		/* defines gdi_cinfo_c */
#include	"gdi_close.h"		/* defines gdi_close_c */
#include	"gdi_defimg.h"		/* defines gdi_defimg_c */
#include	"gdi_error.h"		/* defines gdi_error_c */
#include	"gdi_imwrite.h"		/* defines gdi_imwrite_c */
#include	"gdi_mhead.h"		/* defines gdi_mhead_c */
#include	"gdi_record.h"		/* defines gdi_record_c */
#include	"gdi_rinfo.h"		/* defines gdi_rinfo_c */
#include	"gdi_rmask.h"		/* defines gdi_rmask_c */
#include	"gdi_sequence.h"	/* defines gdi_sequence_c */
#include	"gdi_setid.h"		/* defines gdi_setid_c */
#include	"gdsbox.h"		/* defines gdsbox_c */
#include	"gdsc_fill.h"		/* defines gdsc_fill_c */
#include	"gdsi_read.h"		/* defines gdsi_read_c */
#include	"gdsinp.h"		/* defines gdsinp_c */
#include	"init.h"		/* defines init_c */
#include	"nelc.h"		/* defines nelc_c */
#include	"reject.h"		/* defines reject_c */
#include	"rminmax.h"		/* defines rminmax_c */
#include	"setfblank.h"		/* defines setfblank_c */
#include	"showsub1.h"		/* defines showsub1_c */
#include	"status.h"		/* defines status_c */
#include	"userlog.h"		/* defines userlog_c */
#include	"userreal.h"		/* defines userreal_c */
#include	"userint.h"		/* defines userint_c */
#include	"usertext.h"		/* defines usertext_c */
#include        "deputy.h"
#include        "userfio.h"



/*
 * Defines:
 */

#define	CLASSDIM	2		/* dimension of (sub)set(s) */
#define	FMT_CLIP	"Enter clipping levels [%g,%g]"
#define	KEY_BOX		tofchar(" ")
#define	KEY_CLIP	tofchar("CLIP=")
#define	KEY_DISPLAY	tofchar("DISPLAY=")
#define	KEY_FIRSTRECORD	tofchar("FIRSTRECORD=")
#define	KEY_INSET	tofchar("INSET=")
#define	KEY_NEXT	tofchar("NEXT=")
#define	KEY_RECORD	tofchar("RECORD=")
#define KEY_ANYOUT      tofchar("ANYOUT=")
#define	MAXAXES		10		/* maximum number of axis in set */
#define	MAXDATA		10240		/* maximum number of pixels */
#define	MAXDEVNAMLEN	40		/* maximum length of device name */
#define	MAXMESSAGELEN	80		/* maximum length of message */
#define	MAXSETNAMLEN	FILENAME_MAX	/* maximum length of set name */
#define	MAXSUBSETS	2048		/* maximum number of subsets */
#define	MES_BOX		tofchar(" ")
#define	MES_CLIP	tofchar("Enter clipping levels");
#define	MES_DISPLAY	tofchar("Name of display device [DEFAULT_DISPLAY]")
#define	MES_FIRSTRECORD	tofchar("Record number for first subset [1]")
#define	MES_INSET	tofchar("Set and subset(s) to be displayed")
#define	MES_NEXT	tofchar("Show next subset ? [Y]")
#define	MES_RECORD	tofchar("Record images for movie ? [N]")
#define MES_ANYOUT      tofchar("Device to which output is directed:            [8]")
#define	STA_CLIP	tofchar("Determining default clip levels")
#define	VERSION		"1.0"		/* change version number here */

#define	finit(f,s)	{\
				static	char	buf[s+1];\
				int		i;\
				for (i = 0; i < s; buf[i++] = ' ');\
				buf[s] = '\0';\
				f.a = buf;\
				f.l = s;\
			}

void eightbit(void)
/*
 *  redirect to SLICEVIEW if GIDS cannot be used.
 */
{
   Display *display;
   XVisualInfo *available, template;
   Visual      *defvis;
   VisualID    defvisid;
   int         nfound;
   long        mask;
    
   display = XOpenDisplay(NULL);
   if (!display) {
      errorf(1, "Cannot open display.");
   }
   defvis = XDefaultVisual(display, DefaultScreen(display));
   defvisid = XVisualIDFromVisual(defvis);
   mask = VisualIDMask;
   template.visualid = defvisid;
   available = XGetVisualInfo(display, mask, &template, &nfound);
   anyoutf(16, "%d-bit, class %d", available[0].depth, available[0].class);
   if (available[0].depth!=8 || available[0].class!=PseudoColor) {
      fint irc;
      XFree(available);
      errorf(1, "No 8-bit PseudoColor - starting SLICEVIEW ");
      deputy_c(tofchar("sliceview"), &irc);
      if (irc!=1) errorf(4, "SLICEVIEW failed ");
      finis_c();
   }
   XFree(available);
}
   

MAIN_PROGRAM_ENTRY				/* main program */
{
   fchar		display;		/* name of display device */
   fchar		set;			/* name of set */
   fchar		text;			/* text buffer */
   fint			axperm[MAXAXES];	/* axis permutation array */
   fint			axsize[MAXAXES];	/* axis size array */
   fint			blank_color;		/* reserved display value */
   fint			blo[CLASSDIM];		/* lower box coordinates */
   fint			bhi[CLASSDIM];		/* upper box coordinates */
   fint			cwhi;			/* upper coordinate word */
   fint			cwlo;			/* lower coordinate word */
   fint			display_id;		/* id of display device */
   fint			display_stat;		/* status of gdi call */
   fint			max_color;		/* maximum display value */
   fint			min_color;		/* minimum display value */
   fint			mrecord;		/* maximum recordings */
   fint			ncolors;		/* number of display values */
   fint			nrecord;		/* number of recordings */
   fint			nblanks[MAXSUBSETS];	/* number of blanks */
   fint			ns;			/* subset counter */
   fint			nsequence;		/* sequence counter */
   fint			nsubs;			/* number of subsets */
   fint			sequence[MAXSUBSETS];	/* recording sequence */
   fint			subsets[MAXSUBSETS];	/* buffer for subset levels */
   float		blank;			/* blank value */
   float		bscale;			/* scale factor to set data */
   float		bzero;			/* offset to set data */
   float		cscale;			/* scale factor to display */
   float		czero;			/* offset to display data */
   float		clip[CLASSDIM];		/* clip levels */
   float		datamax[MAXSUBSETS];	/* maximum in subsets */
   float		datamin[MAXSUBSETS];	/* minimum in subsets */
   float		rdata[MAXDATA];		/* array for real data */
   int			record = 0;		/* record images */
   int			wrong_input;		/* okay */
   unsigned char	cdata[MAXDATA];		/* display data */
   bool                 search;
   fint                 output_level = 8;       /* output level */

   init_c( );					/* get in touch with HERMES */
   /*eightbit(); */
   IDENTIFICATION( "VIEW", VERSION );		/* identify ! */
   setfblank_c( &blank );			/* get blank value */
   finit( set, MAXSETNAMLEN );			/* initialize f character */
   finit( text, 80 );				/*   "        "     "     */
   {						/* get set and subsets */
      fint	class = 1;			/* class two application */
      fint	classdim = CLASSDIM;		/* dimension of subsets */
      fint	input_level = 0;		/* input level (no default) */
      fint	maxaxes = MAXAXES;
      fint	maxsubsets = MAXSUBSETS;	/* maximum number of subsets */

      {
         fint  n_items = 1;
         fint  input_level = 2;                 /* Hidden */
         fint  r;

         r = userint_c( &output_level ,
                        &n_items ,
                        &input_level ,          /* default or not */
                        KEY_ANYOUT ,            /* keyword */
                        MES_ANYOUT );
      }


      nsubs = gdsinp_c( set ,			/* name of set */
                        subsets ,		/* levels of subsets */
                        &maxsubsets ,		/* maximum number of subsets */
                        &input_level ,		/* default level */
                        KEY_INSET ,		/* keyword */
                        MES_INSET ,		/* message */
                        &output_level ,		/* output level */
                        axperm ,		/* axis permutation */
                        axsize ,		/* axis size */
                        &maxaxes ,		/* maximum number of axes */
                        &class ,		/* class of application */
                        &classdim );		/* dimension of class */
   }						/* we've got the data */
   {						/* now get the area */
      fint	box_mode = 0;			/* mode for gdsbox */
      fint	input_level = 2;		/* hidden keyword */

      gdsbox_c( blo ,				/* lower box coordinates */
                bhi ,				/* upper box coordinates */
                set ,				/* name of set */
                subsets ,			/* first subset */
                &input_level ,			/* input level */
                KEY_BOX ,			/* keyword */
                MES_BOX ,			/* message */
                &output_level ,			/* output level */
                &box_mode );			/* box option */
   }						/* now we've got the box */

   {
      fint      r;
      fchar	message;			/* message for user */
      fint	input_level;			/* default possible ?*/
      fint	n_items = CLASSDIM;		/* dimension is two */

      input_level = 2;                          /* Hidden */
      message = MES_CLIP;
      r = userreal_c( clip ,	          	/* the clippings */
                      &n_items ,		/* how many clippings */
                      &input_level ,		/* default or not */
                      KEY_CLIP ,		/* keyword */
                      message );		/* message */
      search = (r == 0);
      if (r == 1) clip[1] = clip[0];
   }

   if (search) {                                /* now get default clippings */
      fint	change = 0;			/* no change in minmax */

      status_c( STA_CLIP );			/* status message */
      rminmax_c( set ,				/* name of set */
                 subsets ,			/* the subsets */
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
      fchar	message;			/* message for user */
      fint	input_level;			/* default possible ?*/
      fint	n_items = CLASSDIM;		/* dimension is two */
      float	dummy[CLASSDIM];		/* dummy clippings */

      dummy[0] = clip[0];			/* just copy the ... */
      dummy[1] = clip[1];			/* ... default clippings */
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
                         KEY_CLIP ,		/* keyword */
                         message );		/* message */
      if (dummy[0] >= dummy[1]) {		/* cleary wrong input */
         reject_c( KEY_CLIP, tofchar( "Wrong clip levels (max <= min)!" ) );
         wrong_input = 1;			/* set flag */
      } else {
         wrong_input = 0;			/* clear flag */
         clip[0] = dummy[0];			/* just copy the ... */
         clip[1] = dummy[1];			/* clippings back */
      }
   } while (wrong_input);			/* we've got the clippings */
   finit( display, MAXDEVNAMLEN );		/* initialize f character */
   do {						/* get display device */
      fint	input_level;			/* input level (hidden ?) */

      if (wrong_input) {			/* we've been here before */
         input_level = 1;			/* don't make it hidden */
      } else {					/* first time */
         input_level = 2;			/* make it hidden */
      }
      (void) usertext_c( display ,		/* name of display device */
                         &input_level ,		/* input  level */
                         KEY_DISPLAY ,		/* keyword */
                         MES_DISPLAY );		/* message */
      display_id = gdi_open_c( display );	/* open display device */
      if (display_id < 0) {			/* cannot open display */
         gdi_error_c( &display_id, text );	/* get error text */
         reject_c( KEY_DISPLAY , text );	/* reject keyword */
         wrong_input = 1;			/* set flag */
      } else {
         wrong_input = 0;			/* clear flag */
      }
   } while (wrong_input);			/* now we've got the device */
   {						/* should we record the images */
      bool	val;				/* dummy */
      fint	input_level = 2;		/* input level (hidden) */
      fint	nitems = 1;			/* number of items */

      if (userlog_c( &val ,			/* the dummy */
                     &nitems ,			/* only one */
                     &input_level ,		/* hidden */
                     KEY_RECORD ,		/* the keyword */
                     MES_RECORD )) {		/* the message */
         record = tobool( val );		/* do the recording ? */
      } else {					/* default */
         record = 0;				/* no recording */
      }
   }
   display_stat = gdi_cinfo_c( &display_id ,	/* display id */
                               &min_color ,	/* minimum display value */
                               &max_color ,	/* maximum display value */
                               &ncolors ,	/* number of display values */
                               &blank_color );	/* reserved display value */
   if (display_stat != 0) {			/* error */
      fint	error_level = 4;		/* error level (fatal) */

      gdi_error_c( &display_stat, text );	/* get error text */
      error_c( &error_level, text );		/* error message */
   }
   bscale = ( clip[1] - clip[0] ) / (float) ( max_color - min_color );
   bzero  = ( max_color * clip[0] - min_color * clip[1] ) /
      (float) (max_color - min_color );
   cscale = (float) ( max_color - min_color ) / ( clip[1] - clip[0] );
   czero  = ( clip[1] * min_color - clip[0] * max_color ) /
      ( clip[1] - clip[0] );
   if (record) {				/* the recording */
      fint	frec = 1;			/* dummy */
      fint	input_level = 2;		/* input level (hidden) */
      fint	nitems = 1;			/* number of items */
      
      (void) userint_c( &frec ,			/* the dummy */
                        &nitems ,		/* only one */
                        &input_level ,		/* hidden */
                        KEY_FIRSTRECORD ,	/* the keyword */
                        MES_FIRSTRECORD );	/* the message */
      display_stat = gdi_rinfo_c( &display_id ,	/* display id */
                                  &nrecord ,	/* number of recordings */
                                  &mrecord );	/* maximum number */
      if (frec > mrecord) {			/* too large !! */
         fint	error_level = 4;		/* fatal error */

         error_c( &error_level, tofchar( "First record > Total records" ) );
      }
      if (frec < 1) {				/* stupid user */
         fint	error_level = 4;		/* FATAL error */

         error_c( &error_level, tofchar( "FIRSTRECORD cannot be < 1!" ) );
      }
      if ((mrecord + frec - 1) < nsubs) {	/* too many images */
         fint	error_level = 1;		/* warning */

         error_c( &error_level, tofchar( "cannot record all images") );
      } else {
         mrecord = nsubs;			/* number of recordings */
      }
      for (ns = 0; ns < mrecord; ns++) {	/* loop */
         sequence[ns] = ns + frec;		/* set sequence */
      }
   }
   display_stat = gdi_defimg_c( &display_id ,	/* display id */
                                blo ,		/* lower box coordinates */
                                bhi ,		/* upper box coordinates */
                                &bscale ,	/* scaling factor */
                                &bzero );	/* offset */
   if (display_stat != 0) {			/* something went wrong */
      fint	error_level = 4;		/* error level (fatal) */

      gdi_error_c( &display_stat, text );	/* get error text */
      error_c( &error_level, text );		/* error message */
   }
   for (ns = nsequence = 0; ns < nsubs; ns++) {	/* loop over all subsets */
      fint	maxdata = MAXDATA;		/* number of data */
      fint	ndata;				/* data counter */
      fint	transfer_id = 0;		/* transfer id */

      display_stat = gdi_setid_c( &display_id, set, &subsets[ns], axperm );
      if (display_stat != 0) {			/* error */
         fint	error_level = 1;		/* error level (warning) */

         gdi_error_c( &display_stat, text );	/* get error text */
         error_c( &error_level, text );		/* error message */
      }
      cwlo = gdsc_fill_c( set, &subsets[ns], blo );
      cwhi = gdsc_fill_c( set, &subsets[ns], bhi );
#if 0
      display_stat = gdi_mhead_c( &display_id,
                                  set,
                                  &cwlo,
                                  &cwhi );
      if (display_stat != 0) {			/* error */
         fint	error_level = 1;		/* error level (warning) */

         gdi_error_c( &display_stat, text );	/* get error text */
         error_c( &error_level, text );		/* error message */
      }
#endif
      showsub1_c( set, &subsets[ns], axperm );	/* user info */
      do {					/* loop to write to display */
         fint	packed = 0;			/* packing code */
         int	n;				/* loop counter */

         gdsi_read_c( set ,			/* name of set */
                      &cwlo ,			/* lower c.w. */
                      &cwhi ,			/* upper c.w. */
                      rdata ,			/* array to receive data */
                      &maxdata ,		/* size of array */
                      &ndata ,			/* number read */
                      &transfer_id );		/* transfer id */
         for (n = 0; n < ndata; n++) {		/* loop through data */
            if (rdata[n] == blank) {		/* blank value */
               cdata[n] = blank_color;		/* make if look blank */
            } else if (rdata[n] > clip[1]) {	/* above clip level */
               cdata[n] = max_color;		/* maximum color value */
            } else if (rdata[n] < clip[0]) {	/* below clip level */
               cdata[n] = min_color;		/* minimum color value */
            } else {				/* data in range */
               cdata[n] = cscale * rdata[n] + czero + 0.5;
            }
         }
         display_stat = gdi_imwrite_c( &display_id ,
                                       (fint *)cdata ,
                                       &ndata ,
                                       &packed );
         if (display_stat != 0) {		/* something went wrong */
            fint	error_level = 4;	/* error level (fatal) */

            gdi_error_c( &display_stat, text );	/* get error text */
            error_c( &error_level, text );	/* error message */
         }
      } while (transfer_id);			/* end of copy loop */
      if ( ((ns+1) < nsubs && !record) || (!ns && record && nsubs > 1)) {
         bool	next = TRUE;			/* default */
         fint	n_items = 1;			/* only one item */
         fint	input_level = 1;		/* default allowed */


         (void) userlog_c( &next ,		/* the value */
                           &n_items ,		/* only one */
                           &input_level ,	/* default possible */
                           KEY_NEXT ,		/* keyword */
                           MES_NEXT );		/* message */
         cancel_c( KEY_NEXT );			/* always cancel this keyword */
         if (!tobool( next )) break;		/* stop now */
      }
      if (record && ns < mrecord) {		/* record it */
         display_stat = gdi_record_c( &display_id,
                                      &sequence[nsequence] );
         if (display_stat) {			/* something went wrong */
            fint	error_level = 1;	/* error level (warning) */

            gdi_error_c( &display_stat, text );	/* get error text */
            error_c( &error_level, text );	/* error message */
         } else {
            nsequence += 1;
         }
      }
   }
   if (record) {				/* now define the sequence */
      display_stat = gdi_sequence_c( &display_id,
                                     sequence,
                                     &nsequence );
      if (display_stat) {			/* something went wrong */
         fint	error_level = 1;		/* error level (warning) */

         gdi_error_c( &display_stat, text );	/* get error text */
         error_c( &error_level, text );		/* error message */
      }
   }
   display_stat = gdi_close_c( &display_id );	/* close display */
   if (display_stat != 0) {			/* error closing display */
      fint	error_level = 4;		/* error level (fatal) */

      gdi_error_c( &display_stat, text );	/* get error text */
      error_c( &error_level, text );		/* error message */
   }
   finis_c( );					/* quit working with HERMES */
   return( EXIT_SUCCESS );			/* exit with status */
}
