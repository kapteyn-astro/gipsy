/* storegids.c
                           COPYRIGHT (c) 1999
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.   

#>            storegids.dc1

Program:      STOREGIDS

Purpose:      Store a set displayed in GIDS in a user supplied
              record number

Category:     DISPLAY

File:         storegids.c

Author:       M.G.R. Vogelaar 
              (real author: R.A. Swaters)

Keywords:

RECORD=       Give here the record number to store the displayed set
              in.

Notes:        You first have to start GIDS (Gipsy Image Display System)
              with INIDISPLAY before any display programme can be run.

Updates:      Jul  23, 1999: RAS, Document created, based on view.c

#<

*/


/*
 * include files:
 */

#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
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


/*
 * Defines:
 */

#define	CLASSDIM	2		/* dimension of (sub)set(s) */
#define	KEY_DISPLAY	tofchar("DISPLAY=")
#define	KEY_RECORD	tofchar("RECORD=")
#define	MAXAXES		10		/* maximum number of axis in set */
#define	MAXDATA		10240		/* maximum number of pixels */
#define	MAXDEVNAMLEN	40		/* maximum length of device name */
#define	MAXMESSAGELEN	80		/* maximum length of message */
#define	MAXSETNAMLEN	80		/* maximum length of set name */
#define	MAXSUBSETS	500		/* maximum number of subsets */
#define	MES_DISPLAY	tofchar("Name of display device [DEFAULT_DISPLAY]")
#define	MES_RECORD	tofchar("GIDS Record number to store displayed set in:   [1]")
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


MAIN_PROGRAM_ENTRY				/* main program */
{
   fchar		display;		/* name of display device */
   fchar		text;			/* text buffer */
   fint			display_id;		/* id of display device */
   fint			display_stat;		/* status of gdi call */
   fint                 frec;                   /* record number */
   fint			mrecord;		/* maximum recordings */
   fint			nrecord;		/* number of recordings */
   fint			nsequence;		/* sequence counter */
   fint			sequence[MAXSUBSETS];	/* recording sequence */
   float		blank;			/* blank value */
   int			record = 0;		/* record images */
   int			wrong_input = 0;	/* okay */
   fint                 output_level = 8;       /* output level */

   init_c( );					/* get in touch with HERMES */
   IDENTIFICATION( "STOREGIDS", VERSION );	/* identify ! */
   setfblank_c( &blank );			/* get blank value */
   finit( text, 80 );				/*   "        "     "     */

   {
      fint nitems = 1;
      fint input_level = 1;
      frec = 1;
      (void) userint_c( &frec ,                 /* the dummy */
                        &nitems ,               /* only one */
                        &input_level ,          /* with default*/
                        KEY_RECORD ,            /* the keyword */
                        MES_RECORD );           /* the message */   
   }

   finit( display, MAXDEVNAMLEN );              /* initialize f character */ 
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

   display_stat = gdi_rinfo_c( &display_id , /* display id */
                               &nrecord ,    /* number of recordings */
                               &mrecord );   /* maximum number */
   if (frec > mrecord) {                     /* too large !! */
      fint   error_level = 4;                /* fatal error */
      error_c( &error_level, tofchar( "Record > Total records" ) );
   }
   if (frec < 1) {                           /* stupid user */
      fint   error_level = 4;                /* FATAL error */
      error_c( &error_level, tofchar( "RECORD cannot be < 1!" ) );
   }

   nsequence = 0;
   sequence[nsequence] = frec;
   
   display_stat = gdi_record_c( &display_id,
                                &sequence[nsequence] );

   if (display_stat) {			/* something went wrong */
      fint	error_level = 1;	/* error level (warning) */
       gdi_error_c( &display_stat, text );	/* get error text */
      error_c( &error_level, text );	/* error message */
   } else {
      nsequence += 1;
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

