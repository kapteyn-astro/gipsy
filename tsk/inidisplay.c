/* inidisplay.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            inidisplay.dc1

Program:      inidisplay

Purpose:      Initializes the display for GIPSY applications.

Category:     DISPLAY, UTILITY

Files:        inidisplay.c

Author:       K.G.  Begeman

Description:  The information about displays is obtained from
              the file displays which is located in the gip_sys
              directory and from the X11 environment variable
              DISPLAY.
              If the X11 server is a workstation and not the local
              machine, inidisplay will try to logon to that machine
              and start the display server on the workstation. The
              .rhosts file in your home directory must have a line
              stating the name of the remote host and your user name.
              It is also advisable to add the current host to the .rhosts
              file. Also the GIPSY environment must be setup
              via your .cshrc on the remote machine. On X terminals
              the display server will always run on the local machine.

Keywords:

** DISPLAY=   Name of display device [DEFAULT_DISPLAY]. The display
              name must be an environment variable (in uppercase) which
              contains the name of a file. At GIPSY startup this symbol
              will be defined automatically.

Updates:      Nov 27, 1990: KGB, Document created.
              Apr 10, 1991: KGB, Complete revision.

#<

*/

#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"gipsyc.h"			/* GIPSY symbols and definitions */
#include	"cmain.h"			/* main program in C */
#include	"error.h"			/* define error_c */
#include	"finis.h"			/* define finis_c */
#include	"gdi_close.h"			/* define gdi_close_c */
#include	"gdi_error.h"			/* define gdi_error_c */
#include	"gdi_open.h"			/* define gdi_open_c */
#include	"init.h"			/* define init_c */
#include	"usertext.h"			/* define usertext_c */

#define	VERSION		"1.1"			/* version number */


MAIN_PROGRAM_ENTRY				/* start of main program */
{
   char		deviceb[80];			/* buffer for device name */
   fchar	device;				/* name of display device */
   fint		id;				/* display id */

   init_c( );					/* contact master */
   IDENTIFICATION( "INIDISPLAY", VERSION );	/* identify ! */
   {
      fint	input_level = 2;  		/* hidden keyword */
      fint	nc;				/* number of characters */

      device.a = deviceb;			/* initialize pointer */
      device.l = sizeof( deviceb ) - 1;		/* initialize length */
						/* get name of display device */
      nc = usertext_c( device, &input_level, tofchar( "DISPLAY=" ), tofchar( "Name of display device [DEFAULT_DISPLAY]" ) );
      if (nc == 0) {				/* default */
         device = tofchar( "DEFAULT_DISPLAY" );
      } else {					/* no default */
         deviceb[nc] = 0;			/* end it with zero byte */
      }
   }
   id = gdi_open_c( device );			/* open device */
   if (id < 0) {				/* not opened */
      char	errmesb[80];			/* buffer for error message */
      fchar	errmes;				/* points to buffer */
      fint	error_level = 4;		/* FATAL error */

      errmes.a = errmesb; errmes.l = sizeof( errmesb );
      gdi_error_c( &id, errmes );
      error_c( &error_level, errmes );
   } else {					/* opened */
      id = gdi_close_c( &id );			/* close device */
   }
   finis_c( );					/* bye master */
   return( EXIT_SUCCESS );			/* exit with status */
}
