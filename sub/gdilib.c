    /* gdilib.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            gdilib.dc2

Document:     gdilib

Purpose:      Describes the available routines which handle the display.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Description:  The available routines are:

              GDI_BLANKCOL  Sets the colors for BLANKS values.
              GDI_CINFO     Gets info about the color tables of the display.
              GDI_CLOSE     Closes an opened display device.
              GDI_COLGET    Obtains color Look Up Table from the display.
              GDI_COLPUT    Sends a color Look Up Table to the display.
              GDI_DEFIMG    Defines the sizes and scaling of a display image.
              GDI_ERROR     Generates error messages for gdi error codes.
              GDI_FRAME     Obtains info about frame currently on display.
              GDI_GDSID     Sends GDS set name and subset level to SERVER.
              GDI_GETLUT    Obtains the current colors.
              GDI_GINFO     Obtains info about the graphics planes.
              GDI_GRCLEAR   Clears graphics planes.
              GDI_GRCOL     Sets the color for a graphics plane.
              GDI_GROFF     Turns graphics planes off.
              GDI_GRON      Turns graphics planes on.
              GDI_GRREAD    Gets the graphics data.
              GDI_GRREGION  Lets user define a region in graphics planes.
              GDI_GRWRITE   Puts the graphics data.
              GDI_IDLEN     Returns maximum length of id.
              GDI_IINFO     Obtains info about GDS image loaded in SERVER.
              GDI_IINFO2    Obtains info about GDS image loaded in SERVER.
              GDI_IMMID     Main identification of image.
              GDI_IMSID     Sub identification of image.
              GDI_IMWRITE   Sends display data to server.
              GDI_MHEAD     Creates an image header from a GDS set.
              GDI_OPEN      Opens a display.
              GDI_OPEN2     Opens a display only if the SERVER is running.
              GDI_PGPLOT    PGPLOT interface routines.
              GDI_RECORD    Records the images currently on the display.
              GDI_REMOVE    Removes a recorded image.
              GDI_RINFO     Obtains info about the number of recorded images.
              GDI_RMASK     Obtain mask of recorded images.
              GDI_SEQUENCE  Set playback sequence for recorded images.
              GDI_SETID     Creates main and sub id from gds subset.
              GDI_SETXGRID  Set text for displaying x grids instead of numbers.
              GDI_SETYGRID  Set text for displaying y grids instead of numbers.
              GDI_SAVE      Saves image currently displayed.
              GDI_RESTORE   Restores image previously saved.

Updates:      Jan 11, 1990: KGB Document created.
              Nov 19, 1997: KGB Routines GDI_SAVE/GDI_RESTORE.
              May 31, 2010: JPT Disabled display name mangling.

#<

*/

#include	"errno.h"			/* <errno.h> */
#include	"limits.h"			/* <limits.h> */
#include	"signal.h"			/* <signal.h> */
#include 	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */

#include	"xscanf.h"			/* scans setup files */

#define	STRINGLEN	80			/* length of strings */

#if	defined(__unix__)			/* for UNIX only */

#include	<sys/types.h>			/* define some weird types */
#include	<sys/socket.h>			/* the socket things */
#include	<sys/un.h>			/* unix things */
#include	<netinet/in.h>			/* inet things */
#include	<netdb.h>			/* network */
#if	defined(__sysv__)
#include	<sys/utsname.h>			/* SYSV only */
#endif
#if	!defined(htons) & !defined(__alpha__) & !defined(__linux__)
extern	u_short	htons( );
#endif

extern	int	read( );
extern	int	write( );

#endif						/* __unix__ */

#if	!defined(NO_GIPSY)
#include	"gipsyc.h"			/* GIPSY symbols and definitions */
#else
#include	"f2cvvdefs.h"			/* Fortran to C definitions */
#endif

#include	"gdisysdef.h"			/* GDI symbols and definitions */

#if	!defined(NO_GIPSY)

#include	"axunit.h"			/* define axunit_c */
#include	"cotrans.h"			/* define cotrans_c */
#include	"ftsd_mkhead.h"			/* define ftsd_mkhead_c */
#include	"gds_exist.h"			/* define gds_exist_c */
#include	"gdsc_grid.h"			/* define gdsc_grid_c */
#include	"gdsc_name.h"			/* define gdsc_name_c */
#include	"gdsc_ndims.h"			/* define gdsc_ndims_c */
#include	"gdsc_substruct.h"		/* define gdsc_substruct_c */
#include	"getpath.h"			/* define getpath_c */
#endif

static	Command_struct	srv;			/* command structure */

typedef struct {				/* define gdi info struct */
   char	name[STRINGLEN+1];			/* name of display device */
   int	sock;					/* display handle */
   int	open;					/* display open or closed */
} gdi_struct;					/* the type */

static	gdi_struct	*gdi_info = NULL;	/* initial NULL */
static  int		gdi_nums = 0;		/* initial zero */

/*
 * Some defines for gdi_start_server
 */

#define	COMMANDLEN	1024			/* length of command */

static	fint	lenc( fchar string )
{
   fint	r = string.l;

   while (r && (string.a[r-1] == ' ' || string.a[r-1] == 0)) r--;
   return( r );
}

static	void	broken_pipe( int sig )
{
   if (sig == SIGPIPE) signal( sig, broken_pipe );
}

static	int	name_of_host( char *hostname, int len )
{
#if	defined(__bsd__)
   int	gethostname( );

   return( gethostname( hostname, len ) );
#elif	defined(__sysv__)
   int			r = -1;
   struct utsname	name;

   if (uname( &name ) != -1) {
      r = 0;
      if (strlen( name.nodename ) > len) {
         strncpy( hostname, name.nodename, len );
      } else {
         strcpy( hostname, name.nodename );
      }
   }
   return( r );
#else
   return( -1 );
#endif
}

static	fint	srv_start( char *display_name )
{
   char	display_client[STRINGLEN+1];		/* name of current host */
   char	display_default[STRINGLEN+1];		/* default display device */
   char	*display_device;			/* points to X11 server name */
   char	display_host[STRINGLEN+1];		/* name of X display server */
   char	display_server[STRINGLEN+1];		/* name of GIDS */
   int	display_memory;				/* internal device memory */
   int	display_remote;				/* remote server */
   fint	r = GDIE_SUCCESS;			/* return code */

   /*
    * First we get the name of the host and constitute a
    * default X11 server address.
    */
   if (r == GDIE_SUCCESS) {			/* 1st step */
      fint	n;				/* counter */

      if (name_of_host( display_client, sizeof( display_client ) )) {
         return( r );
      }
      strcpy( display_default, display_client );/* copy */
      strcat( display_default, ":0.0" );	/* default X server */
      display_device = getenv( "DISPLAY" );	/* translate */
#if 0 /* disable display name mangling */
      if (display_device == NULL) {		/* not defined */
         display_device = display_default;	/* use default */
      } else if (!strncmp( display_device, "localhost:", 10 )) {
         display_device = display_default;	/* default wanted */
      } else if (!strncmp( display_device, "unix:", 5 )) {
         display_device = display_default;	/* default wanted */
      } else if (!strncmp( display_device, ":", 1 )) {
         display_device = display_default;	/* default wanted */
      } else if (!strncmp( display_device, "::", 2 )) {
         display_device = display_default;	/* default wanted */
      }
#endif /* (end display name mangling) */
      for (n = 0; n < STRINGLEN; n++) {		/* loop to get server name */
         if (display_device[n] != ':' && display_device[n]) {
            display_host[n] = display_device[n];
         } else {
            display_host[n] = 0;		/* we've got it */
            break;
         }
      }
   }
   /*
    * Second we have to read the setup file and determine whether
    * there is a X11 server available.
    */
   if (r == GDIE_SUCCESS) {			/* 2nd step */
      char	device[STRINGLEN+1];		/* first field */
      char	filename[FILENAME_MAX+1];	/* buffer for file name */
      char	*envp;				/* pointer to environment */

      envp = getenv( "gids_setup" );		/* translate */
      if ( envp == NULL ) {			/* no, do GIPSY thing */
         envp = getenv( "gip_loc" );		/* translate */
      }
      if ( envp == NULL ) {			/* error */
         r = GDIE_NO_SETUP_SYMBOLS;
      } else {					/* okay */
         FILE	*f;				/* file descriptor */

         sprintf( filename, "%s/tvdevices", envp );
         f = fopen( filename, "r" );		/* open for readonly */
         if (f == NULL) {			/* error opening file */
            r = GDIE_NO_SETUP_FILE;
         } else {				/* okay */
            fint	retc = 0;		/* return code */

            do {				/* scan fields */
               retc = xscanf( f, "%*s %d %*s %d", STRINGLEN, device, &display_remote, STRINGLEN, display_server, &display_memory );
            } while (retc == 4 && strcmp( device, display_device ) );
            fclose( f );			/* close file */
            if (retc != 4) {			/* error */
               r = GDIE_DEVICE_UNKNOWN;
            }
         }
         if (r != GDIE_SUCCESS) {               /* device not found */
            strcpy(device, display_device);
            display_remote = 0;                 /* assume local */
            strcpy(display_server, "${gip_exe}/gids"); /* assume gids */
            display_memory = 400000;            /* assume reasonable terminal */
            
            r = GDIE_SUCCESS;                   /* hope for the best... */
         }
      }
   }
   /*
    * Third we try to start the GIPSY DISPLAY SERVER. For remote runs
    * we use rsh/remsh.
    */
   if (r == GDIE_SUCCESS) {			/* 3rd step */
      char	command[COMMANDLEN];		/* command to execute */
      char	*windowname;			/* name of GIDS window */
      char	*keyboard;			/* alt-keyboard */
      int	retc;				/* return code */

      keyboard = getenv( "ALT_KEYBOARD" );
      if (keyboard == NULL) keyboard = "ALT_KEYBOARD";
      windowname = getenv( "gids_windowname" );
      if ( windowname == NULL ) windowname = "GIDS";
      if (display_remote && strcmp( display_client, display_host )) {
#if	defined(__hpux__)
         sprintf( command, "remsh %s -n '%s' -display %s -memory %d -file %s -keyboard %s -name %s",  display_host, display_server, display_device, display_memory, display_name, keyboard, windowname );
#else
         sprintf( command, "rsh %s -n '%s' -display %s -memory %d -file %s -keyboard %s -name %s",  display_host, display_server, display_device, display_memory, display_name, keyboard, windowname );
#endif
      } else {
         sprintf( command, "%s -display %s -memory %d -file %s -keyboard %s -name %s", display_server, display_device, display_memory, display_name, keyboard, windowname );
      }
      retc = system( command );			/* do it */
      if (retc) {				/* cannot execute */
         r = GDIE_SERVER_NOT_STARTED;		/* error code */
      }
   }
   /*
    * Fourth we check whether we can read the com file.
    */
   if (r == GDIE_SUCCESS) {			/* next step */
      FILE	*f = NULL;			/* file descriptor */

      f = fopen( display_name, "r" );		/* try to open com file */
      if (f == NULL) {				/* error */
         r = GDIE_NO_COM_FILE;			/* no communication file */
      } else {					/* okay */
         fclose( f );				/* close com file */
      }
   }
   return( r );					/* return to caller */
}

static	int	srv_send( int socket, void *data, int ndata )
{
   char	*p = (char *) data;			/* make character pointer */
   int	nd = 0;					/* current byte count */
   int	nl = ndata;				/* number of bytes left  */
   int	nt = 0;					/* number of bytes done */
   int	r = GDIE_SUCCESS;			/* return value */

   while (nl) {
      while ((nd = write( socket, &p[nt], nl )) == -1 && errno == EINTR);
      if (nd == -1) { r = GDIE_C_SEND; break; }
      nl -= nd;
      nt += nd;
   }
   return( r );					/* return to caller */
}

static	int	srv_receive( int socket, void *data, int ndata )
{
   char	*p = (char *) data;			/* make character pointer */
   int	nd = 0;					/* current byte count */
   int	nl = ndata;				/* number of bytes left  */
   int	nt = 0;					/* number of bytes done */
   int	r = GDIE_SUCCESS;			/* return value */

   while (nl) {
      while ((nd = read( socket, &p[nt], nl )) == -1 && errno == EINTR);
      if (nd == -1) { r = GDIE_C_RECEIVE; break; }
      nl -= nd;
      nt += nd;
   }
   return( r );					/* return to caller */
}

static	int	srv_flush( int socket, int nflush )
{
   char	c;					/* flush buffer */
   int	n;					/* loop counter */
   int	nd;					/* number of bytes done */
   int	r = GDIE_SUCCESS;			/* return value */

   for (n = 0; n < nflush; n++) {
      while  ((nd = read( socket, &c, 1 )) == -1 && errno == EINTR);
      if (nd == -1) { r = GDIE_C_FLUSH; break; }
   }
   return( r );					/* return to caller */
}


/*

#>            gdi_error.dc2

Function:     GDI_ERROR

Purpose:      Returns an error message associated with a gdi error code.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_ERROR( GDI_ERR,      Input     INTEGER
                                 ERR_STRING )  Output    CHARACTER*(*)

              GDI_ERROR      Returns 0 on success, -61 when error code
                             is not known.
              GDI_ERR        Error returned by gdi function (<0).
              ERR_STRING     String with error message.

Updates:      Feb 23, 1993: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_error( integer, character )

*/

fint	gdi_error_c( fint	*gdi_err ,	/* the error code */
                     fchar	err_string )	/* the error message */
{
   char	*string;
   fint	r = GDIE_SUCCESS;
   int	l;

   switch( *gdi_err ) {
      case GDIE_SUCCESS: {
         string = "No error";
         break;
      }
      case GDIE_NOT_IMPLEMENTED: {
         string = "Function not (yet) implemented";
         break;
      }
      case GDIE_ERROR_UNKNOWN: {
         string = "Error unknown";
         break;
      }
      case GDIE_ALREADY_OPEN: {
         string = "Device already open";
         break;
      }
      case GDIE_ILLEGAL_ID: {
         string = "Illegal display identfier";
         break;
      }
      case GDIE_DEVICE_UNKNOWN: {
         string = "Device unknown";
         break;
      }
      case GDIE_NOT_OPEN: {
         string = "Device not open";
         break;
      }
      case GDIE_NO_SUCH_SET: {
         string = "Set does not exist";
         break;
      }
      case GDIE_GDS_ERROR: {
         string = "GDS error";
         break;
      }
      case GDIE_MHEAD_DIMS: {
         string = "Wrong dimensions (MHEAD)";
         break;
      }
      case GDIE_COLOR_RANGE: {
         string = "Color outside range";
         break;
      }
      case GDIE_IMWRITE_OVERFLOW: {
         string = "Data overflow (IMWRITE)";
         break;
      }
      case GDIE_TOO_LONG: {
         string = "Text overflow";
         break;
      }
      case GDIE_RECORD_RANGE: {
         string = "Record out of range";
         break;
      }
      case GDIE_RECORD_NO_IMAGE: {
         string = "No image to record";
         break;
      }
      case GDIE_RECORD_ERROR: {
         string = "Error recording image";
         break;
      }
      case GDIE_RECORD_ILLEGAL: {
         string = "Illegal record";
         break;
      }
      case GDIE_GRAPHICS_RANGE: {
         string = "Illegal graphics planes";
         break;
      }
      case GDIE_OTHER_REQUEST: {
         string = "Already in wait state";
         break;
      }
      case GDIE_DATA_LEFT: {
         string = "Not enough data left";
         break;
      }
      case GDIE_NO_IMAGE: {
         string = "No image defined";
         break;
      }
      case GDIE_C_SEND: {
         string = "Client send error";
         break;
      }
      case GDIE_C_RECEIVE: {
         string = "Client receive error";
         break;
      }
      case GDIE_C_FLUSH: {
         string = "Client flush error";
         break;
      }
      case GDIE_C_ALLOC: {
         string = "Client allocation error";
         break;
      }
      case GDIE_S_SEND: {
         string = "Server send error";
         break;
      }
      case GDIE_S_RECEIVE: {
         string = "Server receive error";
         break;
      }
      case GDIE_S_FLUSH: {
         string = "Server flush error";
         break;
      }
      case GDIE_S_ALLOC: {
         string = "Server allocation error";
         break;
      }
      case GDIE_GRWRITE_OVERFLOW: {
         string = "Data overflow (GRWRITE)";
         break;
      }
      case GDIE_SERVER_NOT_STARTED: {
         string = "Server was not started";
         break;
      }
      case GDIE_NO_COM_FILE: {
         string = "Could not read sockets file";
         break;
      }
      case GDIE_NO_SETUP_SYMBOLS: {
         string = "No gids_setup or gip_loc defined";
         break;
      }
      case GDIE_NO_SETUP_FILE: {
         string = "Cannot open setup file tvdevices";
         break;
      }
      case GDIE_WRONG_COM_FILE: {
         string = "Wrong sockets file";
         break;
      }
      case GDIE_NO_SUCH_ERROR: {			/* haha */
         string = "No such error code";
         break;
      }
      case GDIE_S_NOOPEN: {
         string = "Cannot open display";
         break;
      }
      case GDIE_S_NOCFONT: {
         string = "Cannot get calibration font";
         break;
      }
      case GDIE_S_NOMFONT: {
         string = "Cannot get menu font";
         break;
      }
      case GDIE_S_NOCOLOR: {
         string = "Cannot initialize colors";
         break;
      }
      case GDIE_S_NOMENU: {
         string = "Cannot initialize menu";
         break;
      }
      case GDIE_S_NOSCALE: {
         string = "Cannot initialize scaling";
         break;
      }
      case GDIE_S_NOWEDGE: {
         string = "Cannot initialize wedge";
         break;
      }
      case GDIE_S_NODATA: {
         string = "Cannot initialize data";
         break;
      }
      case GDIE_S_NOCOORD: {
         string = "Cannot initialize coords";
         break;
      }
      case GDIE_S_NOCON: {
         string = "Cannot establish connections";
         break;
      }
      case GDIE_S_NOCOMFILE: {
         string = "Cannot create communications file";
         break;
      }
      default: {
         r = GDIE_NO_SUCH_ERROR;
         string = "Don't know what to make of this";
         break;
      }
   }
   for ( l = 0; l < err_string.l && string[l]; l++ ) {
      err_string.a[l] = string[l];
   }
   while ( l < err_string.l ) err_string.a[l++] = ' ';
   return( r );
}


/*

#>            gdi_open.dc2

Function:     GDI_OPEN

Purpose:      Opens a display.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_OPEN( DISPLAY )      Input       CHARACTER*(*)

              GDI_OPEN         Returns on success a non-negative display
                               identifier for further use. Negative
                               values indicate an error condition.
              DISPLAY          Name of display device. If DISPLAY
                               is empty, the default display (DEFAULT_DISPLAY)
                               will be used.

Updates:      Nov  2, 1990: KGB Document created.
              Aug  5, 1999: JPT Assume modest X server when device is unknown.

#<

Fortran to C interface:

@ integer function gdi_open( character )

*/


fint	gdi_open_c( fchar display )		/* name of display device */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   FILE	*file;					/* file descriptor */
   char	*device;				/* pointer real device */
   char	dsply[STRINGLEN+1];			/* buffer for display name */
   char	host[STRINGLEN+1];			/* host name */
   char	hostname[80];				/* buffer for host name */
   char	*name;					/* device name pointer */
   char	dnet_name[STRINGLEN+1];			/* dnet number */
   char	inet_name[STRINGLEN+1];			/* inet port number */
   char	unix_name[STRINGLEN+1];			/* name of unix socket */
   int	close( );				/* closes a descriptor */
   int	connect( );				/* connects to server */
   int	con_type = -1;				/* connection type */
   int	inet_addr( );				/* get inet address */
   int	l;
   int	n;					/* loop counter */
   int	nf;					/* counts fields */
   int	ntry = 2;				/* number of tries */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock = -1;				/* socket number */
   int	socket( );				/* creates a socket */
   int	ftype = OS_FLOATING_TYPE;		/* type of floating point */

   if ((l = lenc( display ))) {			/* not empty */
      if (l > STRINGLEN) l = STRINGLEN;		/* truncate */
      name = strncpy( dsply, display.a, l );
      dsply[l] = 0;
   } else {					/* default */
      name = strcpy( dsply, "DEFAULT_DISPLAY" );
   }
   for (n = 0; n < gdi_nums; n++) {		/* search for display name */
      if (gdi_info[n].open && !strcmp( name, gdi_info[n].name )) break;
   }
   if (n < gdi_nums) {				/* already open */
      gdi_info[n].open += 1;			/* increase open count */
      return( n );				/* return open id */
   }
   for (n = 0; n < gdi_nums; n++) {		/* search for free space */
      if (!gdi_info[n].open) break;		/* leave loop */
   }
   if (n == gdi_nums) {				/* increase size of gdi_info */
      gdi_info = realloc( gdi_info, sizeof( gdi_struct ) * ++gdi_nums );
      gdi_info[n].open = 0;			/* not yet open */
   }
   device = getenv( name );			/* translate device name */
   if (device == NULL) {			/* not translated */
      device = name;				/* copy name */
   }
   while (ntry) {				/* loop to connect to server */
      int	connected = 0;			/* not connected */

      file = fopen( device, "r" );		/* open file with device info */
      if (file == NULL) {			/* error opening file */
         fint	retc;				/* return code */

         ntry = 1;				/* one try less */
         retc = srv_start( device );		/* try to start server */
         if (retc != GDIE_SUCCESS) {		/* error starting server */
            return( retc );			/* error: unknown device */
         } else {				/* okay */
            file = fopen( device, "r" );	/* open file with device info */
            if (file == NULL) {			/* again error */
               return( GDIE_ERROR_UNKNOWN );
            }
         }
      }
      nf = xscanf( file, "%*s %*s %*s %*s", STRINGLEN, host, STRINGLEN, unix_name, STRINGLEN, inet_name, STRINGLEN, dnet_name );
      fclose( file );				/* close the file */
      if (nf < 1) {				/* no info obtained */
         return( GDIE_WRONG_COM_FILE );		/* error: reading line */
      }
      if (nf == 1) {				/* only one field */
         fint	gdi_error = atoi( host );

         if (gdi_error < 0) {
            if (ntry != 2) return( gdi_error );	/* error from file */
            remove( device );			/* error from previous run ? */
            continue;
         } else {
            return( GDIE_WRONG_COM_FILE );
         }
      }
      name_of_host( hostname, sizeof( hostname ) );	/* get host name */
      if (isdigit(host[0])) {			/* hostname is its address */
         struct hostent	*hp;
         int		ha;

         hp = gethostbyname( hostname );
         if (hp == NULL) {
            ha = inet_addr( hostname );
         } else {
            memmove( (void *) &ha, (void *) hp->h_addr,
               hp->h_length > sizeof( int ) ?  sizeof( int ) : hp->h_length );
         }
         if (ha == inet_addr( host )) {
            con_type = 0;			/* unix connection is best */
         } else {
            con_type = 1;			/* try inet connection */
         }
      } else if (!strcmp( host, hostname )) {
         con_type = 0;				/* unix connection is best */
      } else {
         con_type = 1;				/* inet connection */
      }
      if (con_type == 0 && unix_name[0] == 0) {
         con_type = 1;				/* try inet connection */
      }
      if (con_type == 1 && inet_name[0] == 0) {
         con_type = 2;				/* try dnet connection */
      }
      if (con_type == 2 && dnet_name[0] == 0) {
         con_type = -1;				/* unknown connection */
      }
      switch( con_type ) {
         case 0: {				/* unix connection */
            struct sockaddr_un	server;		/* unix socket address struct */

            sock = socket( AF_UNIX, SOCK_STREAM, 0 );	/* create socket */
            if (sock < 0) {			/* error creating socket */
               return( GDIE_ERROR_UNKNOWN );	/* code: unknown error */
            }
            server.sun_family = AF_UNIX;	/* unix socket */
            strcpy( server.sun_path, unix_name );	/* name of socket */
            if (connect( sock, (struct sockaddr *)&server, sizeof( server )) < 0) {
               close( sock );			/* close socket */
               remove( device );		/* remove device file */
	       if (!(--ntry)) {			/* no more tries */
                  return( GDIE_DEVICE_UNKNOWN );/* code: unknown display */
               }
            } else {
               connected = 1;			/* connected to socket */
               signal( SIGPIPE, broken_pipe );	/* catch broken pipes */
            }
            break;
         }
         case 1: {				/* inet connection */
            short		port;		/* inet port number */
            struct hostent	*hp;		/* host entry struct */
            struct hostent	*gethostbyname( );	/* get host info */
            struct sockaddr_in	server;		/* inet socket address struct */

            hp = gethostbyname( host );		/* get host by name */
            if (hp == NULL) {			/* unknown host */
               server.sin_addr.s_addr = inet_addr( host );
               if (server.sin_addr.s_addr == -1) {
                  return( GDIE_DEVICE_UNKNOWN );/* code: unknown display */
               }
            } else {
               memmove( (char *)&server.sin_addr, (char *)hp->h_addr,
                  hp->h_length );
            }
            sock = socket( AF_INET, SOCK_STREAM, 0 );	/* create socket */
            if (sock < 0) {			/* error creating socket */
               return( GDIE_ERROR_UNKNOWN );	/* code: unknown error */
            }
            port = atoi( inet_name );		/* port number */
            server.sin_family = AF_INET;	/* internet socket */
            server.sin_port = htons( port );	/* port number */
            if (connect( sock, (struct sockaddr *)&server, sizeof( server )) < 0) {
               remove( device );		/* remove device file */
               if (!(--ntry)) {			/* no more tries */
                  return( GDIE_DEVICE_UNKNOWN );/* code: unknown display */
               }
            } else {
               connected = 1;			/* connected to socket */
            }
            break;
         }
         case 2: { 				/* dnet connection */
            return( GDIE_NOT_IMPLEMENTED );	/* code: not implemented */
            break;
         }
         default: {
            return( GDIE_NOT_IMPLEMENTED );	/* code: not implemented */
            break;
         }
      }
      if (connected) {				/* connected to socket ? */
         srv.cmd    = GDI_OPEN;			/* command for server */
         srv.code   = GDI_OPEN_CLIENT;		/* open by client */
         srv.nbytes = sizeof( ftype );		/* next send */
         r = srv_send( sock, &srv, sizeof( srv ) );
         if (r != GDIE_SUCCESS) {		/* error in send */
            return( r );			/* send error */
         }
         r = srv_send( sock, &ftype, sizeof( ftype ) );
         if (r != GDIE_SUCCESS) {		/* error in send */
            return( r );			/* send error */
         }
         r = srv_receive( sock, &srv, sizeof( srv ) );
         if (r != GDIE_SUCCESS) {		/* error in receive */
            return( r );			/* code: unknown error */
         }
         if (srv.code != GDIE_SUCCESS) {	/* error from server */
            return( srv.code );			/* code: error from server */
         }
         ntry = 0;				/* stop trying */
      }
   }
   gdi_info[n].open += 1;			/* set open code */
   gdi_info[n].sock = sock;			/* save handle */
   strcpy( gdi_info[n].name, name );		/* attach name */
   return( n );					/* display id */
#else						/* other */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#endif
}

/*

#>            gdi_open2.dc2

Function:     GDI_OPEN2

Purpose:      Opens a display only if the display server is already running.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_OPEN( DISPLAY )      Input       CHARACTER*(*)

              GDI_OPEN         Returns on success a non-negative display
                               identifier for further use. Negative
                               values indicate an error condition.
              DISPLAY          Name of display device. If DISPLAY
                               is, the default display (DEFAULT_DISPLAY)
                               will be used.

Updates:      Feb 23, 1993: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_open2( character )

*/


fint	gdi_open2_c( fchar display )		/* name of display device */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   FILE	*file;					/* file descriptor */
   char	*device;				/* pointer real device */
   char	dsply[STRINGLEN+1];			/* buffer for display name */
   char	host[STRINGLEN+1];			/* host name */
   char	hostname[80];				/* buffer for host name */
   char	*name;					/* device name pointer */
   char	dnet_name[STRINGLEN+1];			/* dnet number */
   char	inet_name[STRINGLEN+1];			/* inet port number */
   char	unix_name[STRINGLEN+1];			/* name of unix socket */
   int	close( );				/* closes a descriptor */
   int	connect( );				/* connects to server */
   int	con_type = -1;				/* connection type */
   int	inet_addr( );				/* get inet address */
   int	l;
   int	n;					/* loop counter */
   int	nf;					/* counts fields */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock = -1;				/* socket number */
   int	socket( );				/* creates a socket */
   int	ftype = OS_FLOATING_TYPE;		/* type of floating point */

   if ((l = lenc( display ))) {			/* not empty */
      if (l > STRINGLEN) l = STRINGLEN;		/* truncate */
      name = strncpy( dsply, display.a, l );
      dsply[l] = 0;
   } else {					/* default */
      name = strcpy( dsply, "DEFAULT_DISPLAY" );
   }
   for (n = 0; n < gdi_nums; n++) {		/* search for display name */
      if (gdi_info[n].open && !strcmp( name, gdi_info[n].name )) break;
   }
   if (n < gdi_nums) {				/* already open */
      gdi_info[n].open += 1;			/* increase open count */
      return( n );				/* return open id */
   }
   device = getenv( name );			/* translate device name */
   if (device == NULL) {			/* not translated */
      device = name;				/* copy name */
   }
   file = fopen( device, "r" );			/* open file with device info */
   if (file == NULL) {				/* error opening file */
      return( GDIE_NO_COM_FILE );		/* error */
   }
   nf = xscanf( file, "%*s %*s %*s %*s", STRINGLEN, host, STRINGLEN, unix_name, STRINGLEN, inet_name, STRINGLEN, dnet_name );
   fclose( file );				/* close the file */
   if (nf < 1) {				/* no info obtained */
      return( GDIE_WRONG_COM_FILE );		/* error: reading line */
   }
   name_of_host( hostname, sizeof( hostname ) );/* get host name */
   if (isdigit(host[0])) {
      struct hostent	*hp;
      int		ha;

      hp = gethostbyname( hostname );
      if (hp == NULL) {
         ha = inet_addr( hostname );
      } else {
         memmove( (void *) &ha, (void *) hp->h_addr,
            hp->h_length > sizeof( int ) ?  sizeof( int ) : hp->h_length );
      }
      if (ha == inet_addr( host )) {
         con_type = 0;				/* unix connection is best */
      } else {
         con_type = 1;				/* try inet connection */
      }
   } else if (!strcmp( host, hostname )) {
      con_type = 0;				/* unix connection is best */
   } else {
      con_type = 1;				/* inet connection */
   }
   if (con_type == 0 && unix_name[0] == 0) {
      con_type = 1;				/* try inet connection */
   }
   if (con_type == 1 && inet_name[0] == 0) {
      con_type = 2;				/* try dnet connection */
   }
   if (con_type == 2 && dnet_name[0] == 0) {
      con_type = -1;				/* unknown connection */
   }
   switch( con_type ) {
      case 0: {					/* unix connection */
         struct sockaddr_un	server;		/* unix socket address struct */

         sock = socket( AF_UNIX, SOCK_STREAM, 0 );	/* create socket */
         if (sock < 0) {			/* error creating socket */
            return( GDIE_ERROR_UNKNOWN );	/* code: unknown error */
         }
         server.sun_family = AF_UNIX;		/* unix socket */
         strcpy( server.sun_path, unix_name );	/* name of socket */
         if (connect( sock, (struct sockaddr *)&server, sizeof( server )) < 0) {
            close( sock );			/* close socket */
            remove( device );			/* remove device file */
            return( GDIE_DEVICE_UNKNOWN );	/* code: unknown display */
         }
         break;
      }
      case 1: {					/* inet connection */
         short			port;		/* inet port number */
         struct hostent		*hp;		/* host entry struct */
         struct hostent		*gethostbyname( );	/* get host info */
         struct sockaddr_in	server;		/* inet socket address struct */

         hp = gethostbyname( host );		/* get host by name */
         if (hp == NULL) {			/* unknown host */
            server.sin_addr.s_addr = inet_addr( host );
            if (server.sin_addr.s_addr == -1) {
               return( GDIE_DEVICE_UNKNOWN );	/* code: unknown display */
            }
         } else {
            memmove( (char *)&server.sin_addr, (char *)hp->h_addr,
               hp->h_length );
         }
         sock = socket( AF_INET, SOCK_STREAM, 0 );	/* create socket */
         if (sock < 0) {			/* error creating socket */
            return( GDIE_ERROR_UNKNOWN );	/* code: unknown error */
         }
         port = atoi( inet_name );		/* port number */
         server.sin_family = AF_INET;		/* internet socket */
         server.sin_port = htons( port );	/* port number */
         if (connect( sock, (struct sockaddr *)&server, sizeof( server )) < 0) {
            remove( device );			/* remove device file */
            return( GDIE_DEVICE_UNKNOWN );	/* code: unknown display */
         }
         break;
      }
      case 2: { 				/* dnet connection */
         return( GDIE_NOT_IMPLEMENTED );	/* code: not implemented */
         break;
      }
      default: {
         return( GDIE_NOT_IMPLEMENTED );	/* code: not implemented */
         break;
      }
   }
   srv.cmd    = GDI_OPEN;			/* command for server */
   srv.code   = GDI_OPEN_CLIENT;		/* open by client */
   srv.nbytes = sizeof( ftype );		/* next send */
   r = srv_send( sock, &srv, sizeof( srv ) );
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* send error */
   }
   r = srv_send( sock, &ftype, sizeof( ftype ) );
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* send error */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );
   if (r != GDIE_SUCCESS) {			/* error in receive */
      return( r );				/* code: unknown error */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* code: error from server */
   }
   for (n = 0; n < gdi_nums; n++) {		/* search for free space */
      if (!gdi_info[n].open) break;		/* leave loop */
   }
   if (n == gdi_nums) {				/* increase size of gdi_info */
      gdi_info = realloc( gdi_info, sizeof( gdi_struct ) * ++gdi_nums );
      gdi_info[n].open = 0;			/* not yet open */
   }
   gdi_info[n].open += 1;			/* set open code */
   gdi_info[n].sock = sock;			/* save handle */
   strcpy( gdi_info[n].name, name );		/* attach name */
   return( n );					/* display id */
#else						/* other */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#endif
}


/*

#>            gdi_close.dc2

Function:     GDI_CLOSE

Purpose:      Closes an opened display device.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_CLOSE( GDI_ID )     Input      INTEGER

              GDI_CLOSE      Returns zero if successful, otherwize a
                             negative value is returned.
              GDI_ID         Display id as returned by GDI_OPEN.

Updates:      Nov  2, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_close( integer )

*/


fint	gdi_close_c( fint *gdi_id )		/* display id */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	close( );				/* close a descriptor */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return(GDIE_NOT_OPEN );			/* code: display not opened */
   }
   if (gdi_info[id].open != 1) {		/* one open call */
      gdi_info[id].open -= 1;			/* decrease open count */
      return( GDIE_SUCCESS );			/* success */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_CLOSE;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* error code */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* error code */
   }
   close( sock );				/* close socket */
   if (srv.code == GDIE_SUCCESS) {		/* no error */
      gdi_info[id].open = 0;			/* set close code */
      gdi_info[id].sock = -1;			/* reset */
      while (gdi_nums && !gdi_info[gdi_nums-1].open) {
         gdi_info = realloc( gdi_info, sizeof( gdi_struct ) * --gdi_nums );
      }
   }
   return( srv.code );				/* return to caller */
#else						/* other */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#endif
}


/*
#>            gdi_cinfo.dc2

Function:     GDI_CINFO

Purpose:      Obtains info about the color tables of the display.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_CINFO( GDI_ID,        Input     INTEGER
                                 MINCOL,        Output    INTEGER
                                 MAXCOL,        Output    INTEGER
                                 NCOLORS,       Output    INTEGER
                                 BLANK )        Output    INTEGER

              GDI_CINFO      Returns zero on success, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              MINCOL         Minimum display value for which a
                             color can be assigned.
              MAXCOL         Maximum display value for which a
                             color can be assigned.
              NCOLORS        Number of colors (MAXCOL - MINCOL + 1).
              BLANK          Display value reserved for undefined
                             data.

Updates:      Dec 11, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_cinfo( integer, integer, integer, integer, integer )

*/


fint	gdi_cinfo_c( fint *gdi_id ,		/* display id */
                     fint *mincol ,		/* minimum color value */
                     fint *maxcol ,		/* maximum color value */
                     fint *ncolors,		/* number of colors */
                     fint *blank  )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */
   int	color_info[4];				/* color information */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_CINFO;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   r = srv_receive( sock, color_info, srv.nbytes );
   if (r != GDIE_SUCCESS) {			/* error in receive */
      return( r );				/* return to caller */
   }
   *mincol  = color_info[0];			/* minimum color value */
   *maxcol  = color_info[1];			/* maximum color value */
   *ncolors = color_info[2];			/* number of values */
   *blank   = color_info[3];			/* blank value */
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_ginfo.dc2

Function:     GDI_GINFO

Purpose:      Obtains info about the graphics planes.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_GINFO( GDI_ID ,     Input      INTEGER
                                 NPLANES ,    Output     INTEGER
                                 PMASK )      Output     INTEGER

              GDI_GINFO      Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              NPLANES        Number of graphics planes available.
              PMASK          Mask which specifies which planes are on.

Updates:      Jan  3, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_ginfo( integer, integer, integer )

*/

fint	gdi_ginfo_c( fint	*gdi_id	 ,
                     fint	*nplanes ,
                     fint	*pmask )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */
   int	graphics_info[2];			/* record information */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_GINFO;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   r = srv_receive( sock, graphics_info, srv.nbytes );
   if (r != GDIE_SUCCESS) {			/* error in receive */
      return( r );				/* return to caller */
   }
   *nplanes = graphics_info[0];			/* number of recordings */
   *pmask   = graphics_info[1];			/* max. number of recordings */
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_rinfo.dc2

Function:     GDI_RINFO

Purpose:      Obtains info about the number of recorded images.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_RINFO( GDI_ID ,     Input      INTEGER
                                 NRECORD ,    Output     INTEGER
                                 MRECORD )    Output     INTEGER

              GDI_RINFO      Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              NRECORD        Number of recorded images.
              MRECORD        Maximum number of recorded images.

Updates:      Dec 26, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_rinfo( integer, integer, integer )

*/

fint	gdi_rinfo_c( fint	*gdi_id	 ,
                     fint	*nrecord ,
                     fint	*mrecord )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */
   int	record_info[2];				/* record information */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_RINFO;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   r = srv_receive( sock, record_info, srv.nbytes );
   if (r != GDIE_SUCCESS) {			/* error in receive */
      return( r );				/* return to caller */
   }
   *nrecord = record_info[0];			/* number of recordings */
   *mrecord = record_info[1];			/* max. number of recordings */
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_grcol.dc2

Function:     GDI_GRCOL

Purpose:      Sets the color for a graphics plane.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_GRCOL( GDI_ID ,     Input      INTEGER
                                 PLANE,       Input      INTEGER
                                 RED ,        Input      REAL
                                 GREEN ,      Input      REAL
                                 BLUE )       Input      REAL

              GDI_GRCOL      Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              PLANE          Plane number (1, 2, 4, etc. ).
              RED            Red color intensity (0.0 .. 1.0).
              GREEN          Green color intensity (0.0 .. 1.0).
              BLUE           Blue color intensity (0.0 .. 1.0 ).

Updates:      Jan  3, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_grcol( integer, integer, real, real, real )

*/

fint	gdi_grcol_c( fint	*gdi_id	 ,
                     fint	*plane ,
                     float	*red ,
                     float	*green ,
                     float	*blue )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int		id = *gdi_id;			/* display id */
   int		nw;				/* number of bytes to write */
   int		r = GDIE_SUCCESS;		/* return value */
   int		sock;				/* socket */
   float	color_info[3];			/* record information */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_GRCOL;			/* server command */
   srv.code   = *plane;				/* plane code */
   srv.nbytes = nw = sizeof( color_info );	/* no next send */
   color_info[0] = (*red);
   color_info[1] = (*green);
   color_info[2] = (*blue);
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, color_info, nw );
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_gron.dc2

Function:     GDI_GRON

Purpose:      Turns graphics planes on.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_GRON( GDI_ID ,     Input      INTEGER
                                PMASK )      Input      INTEGER

              GDI_GRON       Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              PMASK          Mask which specifies which planes should be
                             turned on.

Updates:      Jan  3, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_gron( integer, integer )

*/

fint	gdi_gron_c( fint	*gdi_id	 ,
                    fint	*pmask )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_GRON;			/* server command */
   srv.code   = (*pmask);			/* mask code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_groff.dc2

Function:     GDI_GROFF

Purpose:      Turns graphics planes off.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_GROFF( GDI_ID ,     Input      INTEGER
                                 PMASK )      Input      INTEGER

              GDI_GROFF      Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              PMASK          Mask which specifies which planes should be
                             turned off.

Updates:      Jan  3, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_groff( integer, integer )

*/

fint	gdi_groff_c( fint	*gdi_id	 ,
                     fint	*pmask )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_GROFF;			/* server command */
   srv.code   = (*pmask);			/* mask code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_grclear.dc2

Function:     GDI_GRCLEAR

Purpose:      Clears graphics planes.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_GRCLEAR( GDI_ID ,     Input      INTEGER
                                   PMASK )      Input      INTEGER

              GDI_GRCLEAR    Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              PMASK          Mask which specifies which planes should be
                             cleared.

Updates:      Jan  3, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_grclear( integer, integer )

*/

fint	gdi_grclear_c( fint	*gdi_id	 ,
                       fint	*pmask )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_GRCLEAR;			/* server command */
   srv.code   = (*pmask);			/* mask code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_grregion.dc2

Function:     GDI_GRREGION

Purpose:      Lets user define a region in graphics planes.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_GRREGION( GDI_ID ,     Input      INTEGER
                                    PLANE )      Input      INTEGER

              GDI_GRREGION   Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              PLANE          Graphics plane.

Updates:      Jan  3, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_grregion( integer, integer )

*/

fint	gdi_grregion_c( fint	*gdi_id	 ,
                        fint	*plane )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_GRREGION;			/* server command */
   srv.code   = *plane;				/* zero code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_record.dc2

Function:     GDI_RECORD

Purpose:      Records the images currently on the display.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_RECORD( GDI_ID ,     Input      INTEGER
                                  RECORD )     Input      INTEGER

              GDI_RECORD     Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              RECORD         record number (0 to .....).

Updates:      Dec 26, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_record( integer, integer )

*/

fint	gdi_record_c( fint	*gdi_id	,
                      fint	*record )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_RECORD;			/* server command */
   srv.code   = *record;			/* record code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_remove.dc2

Function:     GDI_REMOVE

Purpose:      Removes a recorded image.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_REMOVE( GDI_ID ,     Input      INTEGER
                                  RECORD )     Input      INTEGER

              GDI_REMOVE     Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              RECORD         Record number (0 to .....) of image to be
                             removed.

Updates:      Dec 26, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_remove( integer, integer )

*/

fint	gdi_remove_c( fint	*gdi_id	,
                      fint	*record )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_REMOVE;			/* server command */
   srv.code   = *record;			/* record code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_rmask.dc2

Function:     GDI_RMASK

Purpose:      Obtain mask of recorded images.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_RMASK( GDI_ID  ,     Input      INTEGER
                                 RECORDS ,     Output     INTEGER
                                 NRECORDS )    Input      INTEGER

              GDI_RMASK      Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              RECORDS        Contains one if recorded image is present,
                             zero if not.
              NRECORDS       Size of RECORDS.
Updates:      Dec 26, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_rmask( integer, integer, integer )

*/

fint	gdi_rmask_c( fint	*gdi_id	  ,
                     fint	*records  ,
                     fint	*nrecords )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	n;					/* loop counter */
   int	nr;					/* number of bytes to read */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */
   int	*mask;					/* mask for recorded images */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   mask = calloc( sizeof( int ), *nrecords );	/* allocate memory */
   if (mask == NULL) return( GDIE_C_ALLOC );	/* allocation error */
   nr = sizeof( int ) * (*nrecords);		/* number of bytes to read */
   srv.cmd    = GDI_RMASK;			/* server command */
   srv.code   = *nrecords;				/* code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      free( mask );				/* release memory */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      free( mask );				/* release memory */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      free( mask );				/* release memory */
      return( srv.code );			/* return to caller */
   }
   r = srv_receive( sock, mask, nr );		/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      free( mask );				/* release memory */
      return( r );				/* return to caller */
   }
   for (n = 0; n < *nrecords; n++) {
      records[n] = mask[n];
   }
   free( mask );				/* release memory */
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_sequence.dc2

Function:     GDI_SEQUENCE

Purpose:      Set playback sequence for recorded images.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_SEQUENCE( GDI_ID  ,     Input      INTEGER
                                    RECORDS ,     Output     INTEGER
                                    NRECORDS )    Input      INTEGER

              GDI_RMASK      Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.
              RECORDS        Containse sequence of recorded images.
              NRECORDS       Size of RECORDS.

Updates:      Dec 26, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_sequence( integer, integer, integer )

*/

fint	gdi_sequence_c( fint	*gdi_id	  ,
                       fint	*records  ,
                       fint	*nrecords )
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	n;					/* loop counter */
   int	nw;					/* number of bytes to write */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */
   int	*sequence;				/* mask for recorded images */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   sequence = calloc( sizeof( int ), *nrecords );
   if (sequence == NULL) return( GDIE_C_ALLOC );/* allocation error */
   nw = sizeof( int ) * (*nrecords);		/* number of bytes to read */
   for (n = 0; n < *nrecords; n++) {
      sequence[n] = records[n];
   }
   srv.cmd    = GDI_SEQUENCE;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = nw;				/* next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      free( sequence );				/* release memory */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, sequence, nw );		/* send sequence */
   free( sequence );				/* release memory */
   if (r != GDIE_SUCCESS) {
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_colput.dc2

Function:     GDI_COLPUT

Purpose:      Sends a color Look Up Table to the display.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_COLPUT( GDI_ID,     Input      INTEGER
                                  VALUES,     Input      INTEGER ARRAY
                                  RED,        Input      REAL ARRAY
                                  GREEN,      Input      REAL ARRAY
                                  BLUE,       Input      REAL ARRAY
                                  NCOLORS )   Input      INTEGER

              GDI_COLPUT     Returns zero on succes, negative on error.
              VALUES         Array containing the display values which
                             should have the new colors.
              RED            Red intensities in the range 0.0 to 1.0.
              GREEN          Green intensities in the range 0.0 to 1.0.
              BLUE           Blue intensities in the range 0.0 to 1.0.
              NCOLORS        Total number of colors to send to display.

Updates:      Dec 10, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_colput( integer, integer, real, real, real, integer )

*/


fint	gdi_colput_c( fint  *gdi_id  ,		/* display id */
                      fint  *values  ,		/* display values */
                      float *red     ,		/* red intensity */
                      float *green   ,		/* green intensity */
                      float *blue    ,		/* blue intensity */
                      fint  *ncolors )		/* number of colors */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int			id = *gdi_id;		/* display id */
   int			n;			/* loop counter */
   int			nc = *ncolors;		/* number of colors */
   int			nw;			/* number of bytes to write */
   int			m;			/* indexer */
   int			r = GDIE_SUCCESS;	/* return value */
   int			sock;			/* socket */
   unsigned short	*data;			/* data to send */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   data = calloc( sizeof( unsigned short ), 4 * nc );
   if (data == NULL) return( GDIE_C_ALLOC );	/* return to caller */
   nw = 4 * nc * sizeof( unsigned short );	/* number of bytes to send */
   srv.cmd    = GDI_COLPUT;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = nw;				/* number of bytes */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   for (m = 0, n = 0; n < nc; n++) {
      data[m++] = values[n];			/* copy value */
      if (red[n] < 0.0) {			/* out of range */
         data[m++] = 0;				/* minimum value */
      } else if (red[n] > 1.0) {		/* out of range */
         data[m++] = USHRT_MAX;			/* maximum value */
      } else {					/* scale it */
         data[m++] = red[n] * (float) USHRT_MAX;
      }
      if (green[n] < 0.0) {			/* out of range */
         data[m++] = 0;				/* minimum value */
      } else if (green[n] > 1.0) {		/* out of range */
         data[m++] = USHRT_MAX;			/* maximum value */
      } else {					/* scale it */
         data[m++] = green[n] * (float) USHRT_MAX;
      }
      if (blue[n] < 0.0) {			/* out of range */
         data[m++] = 0;				/* minimum value */
      } else if (blue[n] > 1.0) {		/* out of range */
         data[m++] = USHRT_MAX;			/* maximum value */
      } else {					/* scale it */
         data[m++] = blue[n] * (float) USHRT_MAX;
      }
   }
   r = srv_send( sock, data, nw );		/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      free( data );				/* release data */
      return( r );				/* return to caller */
   }
   free( data );				/* release data */
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* code: from server */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_blankcol.dc2

Function:     GDI_BLANKCOL

Purpose:      Sends the colors for BLANKS values.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_BLANKCOL( GDI_ID,     Input      INTEGER
                                    RED,        Input      REAL ARRAY
                                    GREEN,      Input      REAL ARRAY
                                    BLUE )      Input      REAL ARRAY

              GDI_COLPUT     Returns zero on succes, negative on error.
              RED            Red intensities in the range 0.0 to 1.0.
              GREEN          Green intensities in the range 0.0 to 1.0.
              BLUE           Blue intensities in the range 0.0 to 1.0.

Updates:      Dec 10, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_blankcol( integer, real, real, real )

*/


fint	gdi_blankcol_c( fint  *gdi_id  ,	/* display id */
                        float *red     ,	/* red intensity */
                        float *green   ,	/* green intensity */
                        float *blue    )	/* blue intensity */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int			id = *gdi_id;		/* display id */
   int			nw;			/* number of bytes to write */
   int			r = GDIE_SUCCESS;	/* return value */
   int			sock;			/* socket */
   unsigned short	data[3];		/* data to send */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   nw = sizeof( data );				/* number of bytes to send */
   srv.cmd    = GDI_BLANKCOL;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = nw;				/* number of bytes */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if ((*red) < 0.0) {				/* out of range */
      data[0] = 0;				/* minimum value */
   } else if ((*red) > 1.0) {			/* out of range */
      data[0] = USHRT_MAX;			/* maximum value */
   } else {					/* scale it */
      data[0] = (*red) * (float) USHRT_MAX;
   }
   if ((*green) < 0.0) {			/* out of range */
      data[1] = 0;				/* minimum value */
   } else if ((*green) > 1.0) {			/* out of range */
      data[1] = USHRT_MAX;			/* maximum value */
   } else {					/* scale it */
      data[1] = (*green) * (float) USHRT_MAX;
   }
   if ((*blue) < 0.0) {				/* out of range */
      data[2] = 0;				/* minimum value */
   } else if ((*blue) > 1.0) {			/* out of range */
      data[2] = USHRT_MAX;			/* maximum value */
   } else {					/* scale it */
      data[2] = (*blue) * (float) USHRT_MAX;
   }
   r = srv_send( sock, data, nw );		/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* code: from server */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_idlen.dc2

Function:     GDI_IDLEN

Purpose:      Returns max. length of id.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_IDLEN( GDI_ID,     Input      INTEGER
                                 IDLEN )     Output     INTEGER

              GDI_IMMID      Returns zero on succes, negative on error.
              IDLEN          Maximum number of characters in id.

Updates:      Jan  6, 1993: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_idlen( integer, integer )

*/


fint	gdi_idlen_c( fint	*gdi_id ,		/* display id */
                     fint	*idlen )		/* length */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */
   int	id_info[1];				/* buffer for results */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_IDLEN;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = 0;				/* number of bytes */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, id_info, srv.nbytes );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );
   }
   (*idlen) = id_info[0];			/* get result */
   return( r );					/* code: from server */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_immid.dc2

Function:     GDI_IMMID

Purpose:      Main identification of image.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_IMMID( GDI_ID,     Input      INTEGER
                                 TEXT )      Input      CHARACTER*(*)

              GDI_IMMID      Returns zero on succes, negative on error.
              TEXT           Text as main image identifier.

Updates:      Jan 14, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_immid( integer, character )

*/


fint	gdi_immid_c( fint	*gdi_id ,		/* display id */
                     fchar	text )			/* text */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	nw;					/* number of bytes to write */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   nw = lenc( text );				/* number of bytes to send */
   srv.cmd    = GDI_IMMID;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = nw;				/* number of bytes */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, text.a, nw );		/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* code: from server */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_imsid.dc2

Function:     GDI_IMSID

Purpose:      Sub identification of image.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_IMSID( GDI_ID,     Input      INTEGER
                                 TEXT )      Input      CHARACTER*(*)

              GDI_IMSID      Returns zero on succes, negative on error.
              TEXT           Text as main image identifier.

Updates:      Jan 14, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_imsid( integer, character )

*/


fint	gdi_imsid_c( fint	*gdi_id ,		/* display id */
                     fchar	text )			/* text */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	nw;					/* number of bytes to write */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   nw = lenc( text );				/* number of bytes to send */
   srv.cmd    = GDI_IMSID;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = nw;				/* number of bytes */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, text.a, nw );		/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* code: from server */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_colget.dc2

Function:     GDI_COLGET

Purpose:      Obtains color Look Up Table from the display.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_COLGET( GDI_ID,     Input      INTEGER
                                  VALUES,     Input      INTEGER ARRAY
                                  RED,        Output     REAL ARRAY
                                  GREEN,      Output     REAL ARRAY
                                  BLUE,       Output     REAL ARRAY
                                  NCOLORS )   Input      INTEGER

              GDI_COLGET     Returns zero on success, negative on error.
              VALUES         Array containing the display values for
                             which the colors should be obtained.
              RED            Red intensities in the range 0.0 to 1.0.
              GREEN          Green intensities in the range 0.0 to 1.0.
              BLUE           Blue intensities in the range 0.0 to 1.0.
              NCOLORS        Total number of colors to read from display.

Updates:      Dec 11, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_colget( integer, integer, real, real, real,
@                              integer, integer )

*/


fint	gdi_colget_c( fint  *gdi_id  ,		/* display id */
                      fint  *values  ,		/* display values */
                      float *red     ,		/* red intensity */
                      float *green   ,		/* green intensity */
                      float *blue    ,		/* blue intensity */
                      fint  *ncolors )		/* number of colors */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int			id = *gdi_id;		/* display id */
   int			n;			/* loop counter */
   int			nc = *ncolors;		/* number of colors */
   int			nr;			/* number of bytes to read */
   int			ns;			/* number of shorts */
   int			nw;			/* number of bytes to write */
   int			m;			/* indexer */
   int			r = GDIE_SUCCESS;	/* return value */
   int			sock;			/* socket */
   unsigned short	*data;			/* data to send */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   nw = nc * sizeof( unsigned short );		/* number of bytes to send */
   data = calloc( sizeof( char ), nw );		/* allocate data buffer */
   if (data == NULL) return( GDIE_C_ALLOC );	/* allocation error */
   srv.cmd    = GDI_COLGET;			/* server command */
   srv.code   = 0;				/* zero code */
   srv.nbytes = nw;				/* number of bytes */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, data, nw );		/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      free( data );				/* release data */
      return( r );				/* return to caller */
   }
   free( data );				/* release data */
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   nr = srv.nbytes;				/* number of bytes to read */
   data = calloc( sizeof( char ), nr );		/* allocate memory */
   if (data == NULL) {				/* allocation error */
      r = srv_flush( sock, nr );		/* flush it */
      return( GDIE_C_ALLOC );			/* return to caller */
   }
   r = srv_receive( sock, data, nr );		/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      free( data );				/* release memory */
      return( r );				/* return to caller */
   }
   ns = nr / sizeof( unsigned short );		/* number of shorts */
   for (m = 0, n = 0; n < nc; n++) {		/* scale loop */
      red[n]   = (float) data[m++] / (float) USHRT_MAX;
      green[n] = (float) data[m++] / (float) USHRT_MAX;
      blue[n]  = (float) data[m++] / (float) USHRT_MAX;
   }
   free( data );				/* deallocate memory */
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}


/*
#>            gdi_defimg.dc2

Function:     GDI_DEFIMG

Purpose:      Defines the sizes and scaling of a display image.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_DEFIMG( GDI_ID ,     Input     INTEGER
                                  GLO ,        Input     INTEGER ARRAY
                                  GHI ,        Input     INETEGR ARRAY
                                  BSCALE ,     Input     REAL
                                  BZERO )      Input     REAL

              GDI_DEFIMG     Returns zero on succes, negative on error.
              GDI_ID         Display identifier.
              GLO            Array containing the lower grid units
                             (first X, then Y) of image on display.
              GHI            Array containing the upper grid units
                             (fitst X, then Y) of image on display.
              BSCALE         Scaling factor from display data to
                             real data:
                             real = BSCALE * display + BZERO
              BZERO          See above.

Notes:        GDI_DEFIMG must be called prior to GDI_IMWRITE.

Updates:      Dec 18, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_defimg( integer, integer, integer, real, real )

*/


fint	gdi_defimg_c( fint	*gdi_id	,
                      fint	*glo	,
                      fint	*ghi	,
                      float	*bscale	,
                      float	*bzero	)
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   char	buffer[2*sizeof(float)+4*sizeof(int)];
   int	id = *gdi_id;				/* display identifier */
   int	nw;					/* number of bytes to write */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */
   int	l;
   float	f;

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   l = glo[0];					/* lower x */
   memmove( &buffer[0], &l, sizeof(int) );	/* copy */
   l = glo[1];					/* lower y */
   memmove( &buffer[sizeof(int)], &l, sizeof(int) );
   l = ghi[0];					/* upper x */
   memmove( &buffer[2*sizeof(int)], &l, sizeof(int) );
   l = ghi[1];					/* upper y */
   memmove( &buffer[3*sizeof(int)], &l, sizeof(int) );
   f = (*bscale);
   memmove( &buffer[4*sizeof(int)], &f, sizeof(float) );
   f = (*bzero);
   memmove( &buffer[4*sizeof(int)+sizeof(float)], &f, sizeof(float) );
   nw = sizeof( buffer );			/* number of bytes to send */
   srv.cmd    = GDI_DEFIMG;			/* server command */
   srv.code   = 0;				/* no special code */
   srv.nbytes = nw;				/* number of characters to send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, buffer, nw );		/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}


/*
#>            gdi_mhead.dc2

Function:     GDI_MHEAD

Purpose:      Creates an image header from a GDS set for the display.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_MHEAD( GDI_ID ,    Input      INTEGER
                                 SET ,       Input      CHARACTER*(*)
                                 CWLO ,      Input      INTEGER*8
                                 CWHI )      Input      INTEGER*8

              GDI_MHEAD      Returns zero on success, negative on error.
              GDI_ID         Id of display.
              SET            Name of GDS set.
              CWLO           Lower coordinate word of frame.
              CWHI           Upper coordinate word of frame.

Updates:      Dec 14, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_mhead( integer, character, integer*8, integer*8 )

*/


fint	gdi_mhead_c( fint	*gdi_id   ,	/* display id */
                     fchar	set       ,	/* set name */
                     fint8	*cwlo     ,	/* lower c.w. */
                     fint8  	*cwhi     )	/* upper c.w. */
{
#if	defined(NO_GIPSY)
   return( GDIE_NOT_IMPLEMENTED );
#elif	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   bool		blocked = FALSE;		/* not blocked */
   fchar	rec;				/* points to FITS record */
   char		*header;			/* header data for server */
   fint 	gerror = 0;			/* GDS error return */
   fint8		level = 0;			/* set level (top ) */
   fint		mrec;				/* records in buffer */
   fint		r = GDIE_SUCCESS;		/* return value */
   fint		setdim;				/* dimensions of set */
   fint		subdim;				/* dimension of subset */
   fint8		subset = 0;			/* subset coordinate word */
   int		id = *gdi_id;			/* display id */
   int		nrec = 0;			/* current record */
   int		nw;				/* number of bytes to write */
   int		sock;				/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   if (!tobool(gds_exist_c( set, &gerror ))) {
      return( GDIE_NO_SUCH_SET );		/* all GDS errors */
   }
   setdim = gdsc_ndims_c( set, &level );	/* dimension of set */
   subset = gdsc_substruct_c( set, cwlo, cwhi, &gerror );
   if (gerror < 0) return( GDIE_GDS_ERROR );	/* all GDS errors */
   subdim = gdsc_ndims_c( set, &subset );	/* dimension of subset */
   if (subdim > 2) return( GDIE_MHEAD_DIMS );	/* too large */
   mrec = setdim * 13 + 5;			/* number of FITS records */
   header = calloc( sizeof( char ), mrec * 80 );/* allocate the header buffer */
   if (header == NULL) return( GDIE_C_ALLOC );	/* allocation error */
   rec.a = header; rec.l = 80;			/* initialize */
   r = ftsd_mkhead_c( set, cwlo, cwhi, &blocked, rec, &mrec );
   if (r < 0) {					/* error */
      free( header );				/* not needed */
      return( GDIE_GDS_ERROR );			/* error code */
   }
   nrec += r;					/* number of records */
   nw = nrec * 80;				/* total number of bytes */
   srv.cmd    = GDI_MHEAD;			/* server command */
   srv.code   = 0;				/* no special code */
   srv.nbytes = nw;				/* number of characters to send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      free( header );				/* release memory */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, header, nw );		/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      free( header );				/* release header */
      return( r );				/* return to caller */
   }
   free( header );				/* not needed anymore */
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}


/*
#>            gdi_imwrite.dc2

Function:     GDI_IMWRITE

Purpose:      Sends display data to server.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_IMWRITE( GDI_ID ,     Input     INTEGER
                                   DATA ,       Input     INTEGER ARRAY
                                   NDATA ,      Input	  INTEGER
                                   PACKED )     Input     INTEGER

              GDI_IMWRITE    Returns zero on success, negative on error.
              GDI_ID         Id of display.
              DATA           Array containing display data packed
                             according to the PACKED code.
              NDATA          Number of display data packed into DATA.
              PACKED         Number of display data per integer.
                             The least significant part of the
                             integer contains the most left display
                             datum. A value of zero means that
                             DATA contains plain bytes.

Updates:      Dec 19, 1990: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_imwrite( integer, integer, integer, integer )

*/


fint	gdi_imwrite_c( fint	*gdi_id	,	/* display identifier */
                       fint	*cdata	,	/* display data */
                       fint	*ndata	,	/* number of display data */
                       fint	*packed )	/* packing code */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int			alloc = 0;		/* alocate memory */
   int			id = *gdi_id;		/* display id */
   int			nw = *ndata;		/* number of bytes to write */
   int			r = GDIE_SUCCESS;	/* return value */
   int			sock;			/* socket */
   unsigned char	*data = NULL;		/* display data */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   if (*packed == 0) {				/* plain bytes */
      data = (unsigned char *) cdata;		/* simple */
   } else if (*packed == 1) {			/* one byte / integer */
      int	n;				/* counter */

      data = calloc( sizeof( unsigned char ), nw );
      if (data == NULL) return( GDIE_C_ALLOC );	/* allocation error */
      alloc = 1;				/* memory allocated */
      for (n = 0; n < nw; n++) {		/* copy loop */
         data[n] = cdata[n];			/* copy */
      }
   } else if (*packed == sizeof( fint )) {
#if	defined(__mips__)
      int	l;				/* counter */
      int	m;				/* counter */
      int	n;				/* counter */
      int	n_fint = nw / sizeof( fint );
      union {
         fint		f;
         unsigned char	b[sizeof(fint)];
      } u;

      data = calloc( sizeof( unsigned char ), nw );
      if (data == NULL) return( GDIE_C_ALLOC );	/* allocation error */
      alloc = 1;				/* memory allocated */
      for (m = 0, n = 0; n < n_fint; n++) {
         u.f = cdata[n];
         for (l = sizeof( fint ); l-- > 0; data[m++] = u.b[l] );
      }
#else
      data = (unsigned char *) cdata;		/* simple */
#endif
   } else {
      return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
   }
   srv.cmd    = GDI_IMWRITE;			/* server command */
   srv.code   = 0;				/* no sub code */
   srv.nbytes = nw;				/* number of bytes to send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      if (alloc) free( data );			/* release memory */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, data, nw );		/* send to server */
   if (alloc) free( data );			/* release memory */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}


/*
#>            gdi_grread.dc2

Function:     GDI_GRREAD

Purpose:      Gets the graphics data.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_GRREAD( GDI_ID ,     Input     INTEGER
                                  DATA ,       Input     INTEGER ARRAY
                                  NDATA ,      Input	INTEGER
                                  PACKED )     Input     INTEGER

              GDI_GRREAD     Returns zero on success, negative on error.
              GDI_ID         Id of display.
              DATA           Array containing graphics data packed
                             according to the PACKED code.
              NDATA          Number of graphics data packed into DATA.
              PACKED         Number of graphics data per integer.
                             The least significant part of the
                             integer contains the most left graphics
                             datum. A value of zero means that
                             DATA contains plain bytes.

Updates:      Jan  3, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_grread( integer, integer, integer, integer )

*/


fint	gdi_grread_c( fint	*gdi_id	,	/* display identifier */
                      fint	*cdata	,	/* display data */
                      fint	*ndata	,	/* number of display data */
                      fint	*packed )	/* packing code */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int			alloc = 0;		/* alocate memory */
   int			id = *gdi_id;		/* display id */
   int			nr = *ndata;		/* number of bytes to read */
   int			r = GDIE_SUCCESS;	/* return value */
   int			sock;			/* socket */
   unsigned char	*data = NULL;		/* display data */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_GRREAD;			/* server command */
   srv.code   = nr;				/* next receive */
   srv.nbytes = 0;				/* number of bytes to send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   nr = srv.nbytes;
   if (*packed == 0) {				/* plain bytes */
      data = (unsigned char *) cdata;		/* simple */
   } else if (*packed == 1) {			/* one byte / integer */
      data = calloc( sizeof( unsigned char ), nr );
      if (data == NULL) return( GDIE_C_ALLOC );	/* allocation error */
      alloc = 1;				/* memory allocated */
   } else if (*packed == sizeof( fint )) {
#if	defined(__mips__)
      data = calloc( sizeof( unsigned char ), nr );
      if (data == NULL) return( GDIE_C_ALLOC );	/* allocation error */
      alloc = 1;				/* memory allocated */
#else
      data = (unsigned char *) cdata;		/* simple */
#endif
   } else {
      return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
   }
   r = srv_receive( sock, data, nr );		/* get from server */
   if (r != GDIE_SUCCESS) {			/* error */
      if (alloc) free( data );			/* release memory */
      return( r );				/* return to caller */
   }
   if (*packed == 0) {
   } else if (*packed == 1) {
      int	n;				/* counter */

      for (n = 0; n < nr; n++) {		/* copy loop */
         cdata[n] = data[n];			/* copy */
      }
      free( data );
   } else if (*packed == sizeof( fint )) {
#if	defined(__mips__)
      int	l;				/* counter */
      int	m;				/* counter */
      int	n;				/* counter */
      int	n_fint = nr / sizeof( fint );
      union {
         fint		f;
         unsigned char	b[sizeof(fint)];
      } u;

      for (m = 0, n = 0; n < n_fint; n++) {
         for (l = sizeof( fint ); l-- > 0; u.b[l] = data[m++] );
         cdata[n] = u.f;
      }
      free( data );
#else
#endif
   }
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}


/*
#>            gdi_grwrite.dc2

Function:     GDI_GRWRITE

Purpose:      Puts the graphics data.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_GRWRITE( GDI_ID ,     Input     INTEGER
                                   DATA ,       Output    INTEGER ARRAY
                                   NDATA ,      Input     INTEGER
                                   PACKED )     Input     INTEGER

              GDI_GRWRITE    Returns zero on success, negative on error.
              GDI_ID         Id of display.
              DATA           Array containing graphics data packed
                             according to the PACKED code.
              NDATA          Number of graphics data packed into DATA.
              PACKED         Number of graphics data per integer.
                             The least significant part of the
                             integer contains the most left graphics
                             datum. A value of zero means that
                             DATA contains plain bytes.

Updates:      Aug  6, 1991: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_grwrite( integer, integer, integer, integer )

*/


fint    gdi_grwrite_c( fint     *gdi_id ,       /* display identifier */
                       fint     *cdata  ,       /* display data */
                       fint     *ndata  ,       /* number of display data */
                       fint     *packed )       /* packing code */
{
#if     defined(__vms__)                        /* VMS */
   return( GDIE_NOT_IMPLEMENTED );              /* not implemented */
#elif   defined(__unix__)                       /* UNIX */
   int                  alloc = 0;              /* alocate memory */
   int                  id = *gdi_id;           /* display id */
   int                  nw = *ndata;            /* number of bytes to write */
   int                  r = GDIE_SUCCESS;       /* return value */
   int                  sock;                   /* socket */
   unsigned char        *data = NULL;           /* display data */

   if (id < 0 || id >= gdi_nums) {              /* illegal display id */
      return( GDIE_ILLEGAL_ID );                /* code: illegal display id */
   }
   if (!gdi_info[id].open) {                    /* display not open */
      return( GDIE_NOT_OPEN );                  /* code: display not opened */
   }
   sock = gdi_info[id].sock;                    /* get socket */
   if (*packed == 0) {                          /* plain bytes */
      data = (unsigned char *) cdata;           /* simple */
   } else if (*packed == 1) {                   /* one byte / integer */
      int	n;				/* counter */

      data = calloc( sizeof( unsigned char ), nw );
      if (data == NULL) return( GDIE_C_ALLOC ); /* allocation error */
      alloc = 1;                                /* memory allocated */
      for (n = 0; n < nw; n++) {		/* copy loop */
         data[n] = cdata[n];			/* copy */
      }
   } else if (*packed == sizeof( fint )) {
#if     defined(__mips__)
      int	l;				/* counter */
      int	m;				/* counter */
      int	n;				/* counter */
      int	n_fint = nw / sizeof( fint );
      union {
         fint		f;
         unsigned char	b[sizeof(fint)];
      } u;

      data = calloc( sizeof( unsigned char ), nw );
      if (data == NULL) return( GDIE_C_ALLOC ); /* allocation error */
      alloc = 1;                                /* memory allocated */
      for (m = 0, n = 0; n < n_fint; n++) {
         u.f = cdata[n];
         for (l = sizeof( fint ); l-- > 0; data[m++] = u.b[l] );
      }
#else
      data = (unsigned char *) cdata;           /* simple */
#endif
   } else {
      return( GDIE_NOT_IMPLEMENTED );           /* not implemented */
   }
   srv.cmd    = GDI_GRWRITE;                    /* server command */
   srv.code   = 0;                              /* no sub code */
   srv.nbytes = nw;                             /* number of bytes to send */
   r = srv_send( sock, &srv, sizeof( srv ) );   /* send to server */
   if (r != GDIE_SUCCESS) {                     /* error */
      return( r );                              /* return to caller */
   }
   r = srv_send( sock, data, nw );              /* send to server */
   if (alloc) free( data );                     /* release memory */
   if (r != GDIE_SUCCESS) {                     /* error */
      return( r );                              /* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );                          /* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );              /* not implemented */
#endif
}


/*

#>            gdi_gdsid.dc2

Function:     GDI_GDSID

Purpose:      Sends GDS database name and subset level to DISPLAY SERVER.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_GDSID( GDI_ID ,       Input     INTEGER
                                 SET ,          Input     CHARACTER*(*)
                                 SUBSET )       Input     INTEGER*8

              GDI_GDSID      Returns zero on success, negative on error.
              GDI_ID         Id of display.
              SET            Name of GDS database.
              SUBSET         Subset level of GDS database.

Updates:      Nov 27, 1991: KGB, Document created.

#<

Fortran to C interface:

@ integer function gdi_gdsid( integer, character, integer*8 )

*/

fint	gdi_gdsid_c( fint	*display_id ,		/* display id */
                     fchar	set ,			/* set name */
                     fint8	*subset )		/* subset level */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   char	setbuf[MAXSETNAMELEN];			/* buffer for set name */
   int	i;					/* loop counter */
   int	id = *display_id;			/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   for (i = 0; i < set.l && i < MAXSETNAMELEN; i++) {
      setbuf[i] = set.a[i];
   }
   while (i < MAXSETNAMELEN) setbuf[i++] = ' ';
#if	!defined(NO_GIPSY)
   {
      fchar	fs;

      fs.a = setbuf; fs.l = MAXSETNAMELEN;
      getpath_c( fs );
   }
#endif
   srv.cmd    = GDI_GDSID;			/* server command */
   srv.code   = *subset;			/* subset level */
   srv.nbytes = MAXSETNAMELEN;			/* number of bytes to send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, setbuf, MAXSETNAMELEN );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}


/*

#>            gdi_iinfo.dc2

Function:     GDI_IINFO

Purpose:      Obtains info about GDS image loaded in DISPLAY SERVER.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_IINFO( GDI_ID ,       Input     INTEGER
                                 SET ,          Output    CHARACTER*(*)
                                 SUBSET ,       Output    INTEGER
                                 BLO ,          Output    INTEGER ARRAY
                                 BHI )          Output    INTEGER ARRAY

              GDI_GDSID      Returns zero on success, negative on error.
              GDI_ID         Id of display.
              SET            Name of GDS database.
              SUBSET         Subset level of GDS database.
              BLO            Contains lower X and Y grids of loaded image.
              BHI            Contains upper X and Y grids of loaded image.

Updates:      Nov 27, 1991: KGB, Document created.

#<

Fortran to C interface:

@ integer function gdi_iinfo( integer, character, integer, integer, integer )

*/

fint	gdi_iinfo_c( fint	*display_id ,		/* display id */
                     fchar	set ,			/* set name */
                     fint	*subset ,		/* subset level */
                     fint	*blo ,			/* lower grids */
                     fint	*bhi )			/* upper grids */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   char	setbuf[MAXSETNAMELEN];			/* buffer for set name */
   int	i;					/* loop counter */
   int	id = *display_id;			/* display id */
   int	nr;
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */
   int	grids[4];				/* boundaries */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_IINFO;			/* server command */
   srv.code   = 0;				/* no code */
   srv.nbytes = 0;				/* number of bytes to send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   } else if (srv.code < 0) return( srv.code );
   *subset = srv.code;
   nr = srv.nbytes;
   r = srv_receive( sock, setbuf, MAXSETNAMELEN );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   for (i = 0; i < set.l && i < MAXSETNAMELEN; i++) {
      set.a[i] = setbuf[i];
   }
   while (i < set.l) set.a[i++] = ' ';
   r = srv_receive( sock, grids, sizeof( grids ) );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   blo[0] = grids[0];
   blo[1] = grids[1];
   bhi[0] = grids[2];
   bhi[1] = grids[3];
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}


/*

#>            gdi_iinfo2.dc2

Function:     GDI_IINFO2

Purpose:      Obtains info about GDS image loaded in DISPLAY SERVER.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_IINFO2( GDI_ID ,       Input     INTEGER
                                  SET ,          Output    CHARACTER*(*)
                                  SUBSET ,       Output    INTEGER
                                  BLO ,          Output    INTEGER ARRAY
                                  BHI ,          Output    INTEGER ARRAY
                                  BSCALE ,       Output    REAL
                                  BZERO )        Output    REAL

              GDI_GDSID      Returns zero on success, negative on error.
              GDI_ID         Id of display.
              SET            Name of GDS database.
              SUBSET         Subset level of GDS database.
              BLO            Contains lower X and Y grids of loaded image.
              BHI            Contains upper X and Y grids of loaded image.
              BSCALE         Scaling: data = BSCALE * index + BZERO
              BZERO          Offset:  data = BSCALE * index + BZERO

Updates:      Feb 23, 1993: KGB, Document created.

#<

Fortran to C interface:

@ integer function gdi_iinfo2( integer, character, integer, integer, integer,
@                              real, real )

*/

fint	gdi_iinfo2_c( fint	*display_id ,		/* display id */
                      fchar	set ,			/* set name */
                      fint	*subset ,		/* subset level */
                      fint	*blo ,			/* lower grids */
                      fint	*bhi ,			/* upper grids */
                      float	*bscale ,		/* scaling */
                      float	*bzero )		/* offset */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   char	setbuf[MAXSETNAMELEN];			/* buffer for set name */
   int	i;					/* loop counter */
   int	id = *display_id;			/* display id */
   int	nr;
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */
   int grids[4];				/* boundaries */
   float	ab[2];				/* scaling */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_IINFO2;			/* server command */
   srv.code   = 0;				/* no code */
   srv.nbytes = 0;				/* number of bytes to send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   } else if (srv.code < 0) return( srv.code );
   *subset = srv.code;
   nr = srv.nbytes;
   r = srv_receive( sock, setbuf, MAXSETNAMELEN );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   for (i = 0; i < set.l && i < MAXSETNAMELEN; i++) {
      set.a[i] = setbuf[i];
   }
   while (i < set.l) set.a[i++] = ' ';
   r = srv_receive( sock, grids, sizeof( grids ) );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   blo[0] = grids[0]; blo[1] = grids[1];
   bhi[0] = grids[2]; bhi[1] = grids[3];
   r = srv_receive( sock, ab, sizeof( ab ) );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   (*bscale) = ab[0]; (*bzero) = ab[1];
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*

#>            gdi_frame.dc2

Function:     GDI_FRAME

Purpose:      Obtains info about frame currently on display.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_FRAME( GDI_ID ,       Input     INTEGER
                                 FLO ,          Output    REAL ARRAY
                                 FHI )          Output    REAL ARRAY

              GDI_FRAME      Returns zero on success, negative on error.
              GDI_ID         Id of display.
              FLO            Contains lower X and Y grids of displayed image.
              FHI            Contains upper X and Y grids of displayed image.

Updates:      Nov 27, 1991: KGB, Document created.

#<

Fortran to C interface:

@ integer function gdi_frame( integer, real, real )

*/

fint	gdi_frame_c( fint	*display_id ,		/* display id */
                     float	*flo ,			/* lower grids */
                     float	*fhi )			/* upper grids */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *display_id;			/* display id */
   int	nr;
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */
   float	grids[4];			/* boundaries */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_FRAME;			/* server command */
   srv.code   = 0;				/* no code */
   srv.nbytes = 0;				/* number of bytes to send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   nr = srv.nbytes;
   r = srv_receive( sock, grids, nr );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   flo[0] = grids[0];
   flo[1] = grids[1];
   fhi[0] = grids[2];
   fhi[1] = grids[3];
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*

#>            gdi_setid.dc2

Function:     GDI_SETID

Purpose:      Constitutes a main and sub id from a subset for the
              display device.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_SETID( ID ,        Input      INTEGER
                                 SET ,       Input      CHARACTER*(*)
                                 SUBSET ,    Input      INTEGER
                                 AXPERM )    Input      INTEGER ARRAY

              GDI_SETID     Return 0 on success, negative on error.
              ID            Display id, i.e. returned from GDI_OPEN.
              SET           Name of set.
              SUBSET        Subset coordinate word.
              AXPERM        Axes premutation array as returned from GDSINP.

Updates:      Aug  1, 1991: KGB, Document created.

#<

Fortran to C interface:

@ integer function gdi_setid( integer, character, integer*8, integer )

*/

fint	gdi_setid_c( fint	*display_id ,		/* display id */
                     fchar	set ,			/* set name */
                     fint8	*subset ,		/* subset level */
                     fint	*axperm )		/* axis permutation */
{
#if	defined(NO_GIPSY)
   return( GDIE_NOT_IMPLEMENTED );
#else
   char		cbuf[18];				/* buffer for fchar */
   char		string[80];				/* text buffer */
   double	coords[3];				/* the coordinates */
   double	grids[2] = { 0.0, 0.0 };		/* the grids */
   fchar	buf;					/* points to cbuf */
   fchar	fstr;					/* Fortran string */
   fint		dir = 1;				/* direction of transform */
   fint		gerror = 0;				/* gds error return */
   fint		idlen;					/* length of id */
   fint8		level = 0;				/* set level */
   fint		ndims;					/* number of axes */
   fint		r = 0;					/* return value */

   (void) gdi_gdsid_c( display_id, set, subset );	/* make gds id */
   (void) gdi_idlen_c( display_id, &idlen );
   {
      int	l, len;					/* counters */

      len = lenc( set );				/* length of set name */
      for (l = len - 1; l && set.a[l] != '/'; l--);	/* to skip path */
      if (l) l += 1;					/* name part */
      sprintf( string, "%*.*s", idlen, (len - l) > idlen ? idlen : len - l, &set.a[l] );
      fstr.a = string; fstr.l = strlen( string );	/* Fortran string */
      r = gdi_immid_c( display_id, fstr );		/* main id */
      if (r) return( r );				/* error */
   }
   buf.a = cbuf; buf.l = sizeof( cbuf );		/* set fchar */
   ndims = gdsc_ndims_c( set, &level );			/* dimension of set */
   if (ndims == 3) {					/* possible */
      if (cotrans_c( set, subset, grids, coords, &dir )) {
         fint	grid;					/* the grid */
         fint	l = 0;					/* counter */

         grid = gdsc_grid_c( set ,			/* name of set */
                             &axperm[2] ,		/* last axis */
                             subset ,			/* subset */
                             &gerror );			/* gds status */
         gdsc_name_c( buf, set, &axperm[2], &gerror );	/* name of axis */
         while (l < sizeof(cbuf) && (buf.a[l] != ' ' || buf.a[l] != '-')) l++;
         (void) sprintf( string, "%.*s=%5d", l, buf.a, grid );
      } else {						/* we know the coordinate */
         int	l;					/* counter */

         (void) axunit_c( set, &axperm[2], buf );	/* get units */
         l = lenc( buf );				/* get length */
         (void) sprintf( string, "%#*G %.*s", idlen - l - 1, coords[2], l, buf.a );
      }
   } else {						/* ndim != 3 */
      fint	n;					/* counter */

      string[0] = 0;					/* initialize */
      for (n = 2; n < ndims; n++) {			/* loop */
         char	text[80];				/* text buffer */
         fint	grid;					/* the grid */
         fint	l = 0;					/* loop counter */

         if (n > 2) strcat( string, "," );		/* append separator */
         grid = gdsc_grid_c( set ,			/* name of set */
                             &axperm[n] ,		/* axis number */
                             subset ,			/* subset level */
                             &gerror );			/* gds status */
         gdsc_name_c( buf, set, &axperm[n], &gerror );	/* axis name */
         while (l < sizeof(cbuf) && (buf.a[l] != ' ' || buf.a[l] != '-')) l++;
         (void) sprintf( text, "%.*s=%d", l, buf.a, grid );
         strcat( string, text );			/* append text */
      }
   }
   fstr.a = string; fstr.l = strlen( string );		/* make Fortran string */
   return( gdi_imsid_c( display_id, fstr )) ;
#endif
}


/*

#>            gdi_getlut.dc2

Function:     GDI_GETLUT

Purpose:      Obtains the current colors.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_GETLUT( GDI_ID ,       Input     INTEGER
                                  RED ,          Output    REAL ARRAY
                                  GREEN ,        Output    REAL ARRAY
                                  BLUE ,         Output    REAL ARRAY
                                  NCOL )         Output    INTEGER

              GDI_GETLUT     Returns zero on success, negative on error.
              GDI_ID         Id of display.
              RED            Red intensities (0.0-1.0).
              GREEN          Green intensities (0.0-1.0).
              BLUE           Blue intensities (0.0-1.0).
              NCOL           Number of colors returned. The maximum
                             number of colors is 256, so be sure that
                             arrays RED, GREEN and BLUE have at least this
                             size.

Updates:      Feb 23, 1993: KGB, Document created.

#<

Fortran to C interface:

@ integer function gdi_getlut( integer, real, real, real, integer )

*/

fint	gdi_getlut_c( fint	*display_id ,		/* display id */
                      float	*red ,			/* red intensities */
                      float	*green ,		/* green intensities */
                      float	*blue ,			/* blue intensities */
                      fint	*ncol )			/* number of colors */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *display_id;			/* display id */
   int	nr;
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_GETLUT;			/* server command */
   srv.code   = 0;				/* no code */
   srv.nbytes = 0;				/* number of bytes to send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   nr = srv.nbytes;
   (*ncol) = nr / ( 3 * sizeof( float ) );	/* number of colors */
   r = srv_receive( sock, red, (*ncol) * sizeof( float ) );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, green, (*ncol) * sizeof( float ) );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, blue, (*ncol) * sizeof( float ) );
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/* gdi_setxgrid.c

#>            gdi_setxgrid.dc2

Function:     GDI_SETXGRID

Purpose:      Set text for X grid coordinates.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_SETXGRID( ID    ,   Input   INTEGER
                                    TEXT  ,   Input   CHARACTER*(*) ARRAY
                                    XSIZE )   Input   INTEGER

              GDI_SETXGRID  Returns 0 on success, negative on error.
              ID            Display id, i.e. returned from GDI_OPEN.
              TEXT          Text to be displayed on display instead of
                            grid coordinates. Usually only 7 characters
                            are used.
              XSIZE         Number of items in TEXT. Must be equal to
                            the size as defined with gdi_defimg.

Notes:        The call to gds_setxgrid should immediately follow the call to
              gdi_defimg.

Updates:      Aug 23, 1993: KGB, Document created.

#<

Fortran to C interface:

@ integer function gdi_setxgrid( integer, character, integer )

*/

fint	gdi_setxgrid_c( fint	*display_id ,		/* display id */
                        fchar	text ,			/* text */
                        fint	*len )			/* Items in text */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *display_id;			/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_SETXGRID;			/* server command */
   srv.code   = 0;				/* empty code */
   srv.nbytes = text.l * (*len);		/* number of bytes */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, text.a, srv.nbytes );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/* gdi_setygrid.c

#>            gdi_setygrid.dc2

Function:     GDI_SETYGRID

Purpose:      Set text for Y grid coordinates.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_SETYGRID( ID    ,   Input   INTEGER
                                    TEXT  ,   Input   CHARACTER*(*) ARRAY
                                    YSIZE )   Input   INTEGER

              GDI_SETYGRID  Return 0 on success, negative on error.
              ID            Display id, i.e. returned from GDI_OPEN.
              TEXT          Text to be displayed on display instead of
                            grid coordinates. Usually only 7 characters
                            are used.
              YSIZE         Number of items in TEXT. Must be equal to
                            the size as defined with gdi_defimg.

Notes:        The call to gds_setygrid should immediately follow the call to
              gdi_defimg.

Updates:      Aug 23, 1993: KGB, Document created.

#<

Fortran to C interface:

@ integer function gdi_setygrid( integer, character, integer )

*/

fint	gdi_setygrid_c( fint	*display_id ,		/* display id */
                        fchar	text ,			/* text */
                        fint	*len )			/* Items in text */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *display_id;			/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_SETYGRID;			/* server command */
   srv.code   = 0;				/* empty code */
   srv.nbytes = text.l * (*len);		/* number of bytes */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_send( sock, text.a, srv.nbytes );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   return( srv.code );				/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_save.dc2

Function:     GDI_SAVE

Purpose:      Saves image currently displayed.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_SAVE( GDI_ID )     Input      INTEGER

              GDI_SAVE       Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.

Updates:      Nov 19, 1997: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_save( integer, integer )

*/

fint	gdi_save_c( fint	*gdi_id	)
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_SAVE;			/* server command */
   srv.code   = 0;				/* dummy code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/*
#>            gdi_restore.dc2

Function:     GDI_RESTORE

Purpose:      Restores the image previously recorded.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_RESTORE( GDI_ID )     Input      INTEGER

              GDI_RESTORE    Returns zero on succes, negative on error.
              GDI_ID         Display id as returned by GDI_OPEN.

Updates:      Nov 19, 1997: KGB Document created.

#<

Fortran to C interface:

@ integer function gdi_restore( integer, integer )

*/

fint	gdi_restore_c( fint	*gdi_id	)
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* code: not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *gdi_id;				/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_RESTORE;			/* server command */
   srv.code   = 0;				/* dummy code */
   srv.nbytes = 0;				/* no next send */
   r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
   if (r != GDIE_SUCCESS) {			/* error in send */
      return( r );				/* return to caller */
   }
   r = srv_receive( sock, &srv, sizeof( srv ) );/* read from server */
   if (r != GDIE_SUCCESS) {			/* error */
      return( r );				/* return to caller */
   }
   if (srv.code != GDIE_SUCCESS) {		/* error from server */
      return( srv.code );			/* return to caller */
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}

/* gdi_pgplot.c

#>            gdi_pgplot.dc2

Function:     GDI_PGPLOT

Purpose:      The PGPLOT interface with GIDS.

Category:     DISPLAY

File:         gdilib.c

Author:       K.G. Begeman

Use:          INTEGER GDI_PGPLOT( ID ,        Input      INTEGER
                                  IFUNC ,     Input      INTEGER
                                  RBUF ,   Input/Output  REAL ARRAY
                                  NRBF ,   Input/Output  INTEGER
                                  CBUF ,   Input/Output  CHARACTER*(*)
                                  NCBF )   Input/Output  INTEGER

              GDI_PGPLOT    Return 0 on success, negative on error.
              ID            Display id, i.e. returned from GDI_OPEN.
              IFUNC         PGPLOT driver funtion.
              RBUF          Real Buffer.
              NRBF          Items in RBUF.
              CBUF          Character buffer.
              NCBF          Items in CBUF.

Updates:      Apr 29, 1992: KGB, Document created.

#<

Fortran to C interface:

@ integer function gdi_pgplot( integer, integer, real, integer, character, integer )

*/

fint	gdi_pgplot_c( fint	*display_id ,		/* display id */
                      fint	*ifunc ,		/* PGPLOT funtion */
                      float	*rbuf ,			/* float buffer */
                      fint	*nbuf ,			/* # of floats */
                      fchar	chr ,			/* character buffer */
                      fint	*lchr )			/* # of characters */
{
#if	defined(__vms__)			/* VMS */
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#elif	defined(__unix__)			/* UNIX */
   int	id = *display_id;			/* display id */
   int	r = GDIE_SUCCESS;			/* return value */
   int	sock;					/* socket */

   if (id < 0 || id >= gdi_nums) {		/* illegal display id */
      return( GDIE_ILLEGAL_ID );		/* code: illegal display id */
   }
   if (!gdi_info[id].open) {			/* display not open */
      return( GDIE_NOT_OPEN );			/* code: display not opened */
   }
   sock = gdi_info[id].sock;			/* get socket */
   srv.cmd    = GDI_PGPLOT;			/* server command */
   srv.code   = (*ifunc);			/* PGPLOT function */
   switch( *ifunc ) {
      /*
       * The following functions read floats!
       */
      case 2:
      case 3:
      case 6:
      case 7: {
         srv.nbytes = 0;			/* number of bytes to send */
         r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
         if (r != GDIE_SUCCESS) {		/* error */
            return( r );			/* return to caller */
         }
         r = srv_receive( sock, &srv, sizeof( srv ) );
         if (r != GDIE_SUCCESS) {		/* error */
            return( r );			/* return to caller */
         } else {
            *nbuf = srv.nbytes / sizeof( float );
            r = srv_receive( sock, rbuf, srv.nbytes );
         }
         break;
      }
      /*
       * The following functions read characters!
       */
      case 4: {
         srv.nbytes = 0;			/* number of bytes to send */
         r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
         if (r != GDIE_SUCCESS) {		/* error */
            return( r );			/* return to caller */
         }
         r = srv_receive( sock, &srv, sizeof( srv ) );
         if (r != GDIE_SUCCESS) {		/* error */
            return( r );			/* return to caller */
         } else {
            *lchr = srv.nbytes;
            r = srv_receive( sock, chr.a, srv.nbytes );
         }
         break;
      }
      /*
       * The following functions read/write nothing!
       */
      case 10:
      case 11:
      case 16: {
         srv.nbytes = 0;
         r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
         if (r != GDIE_SUCCESS) {		/* error */
            return( r );			/* return to caller */
         }
         break;
      }
      /*
       * The following functions send floats!
       */
      case 12:
      case 13:
      case 14:
      case 15:
      case 19:
      case 20:
      case 21:
      case 22:
      case 24: {
         srv.nbytes = (*nbuf) * sizeof( float );/* number of bytes to send */
         r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
         if (r != GDIE_SUCCESS) {		/* error */
            return( r );			/* return to caller */
         }
         r = srv_send( sock, rbuf, srv.nbytes );
         break;
      }
      /*
       * Open workstation.
       */
      case 9: {
         srv.nbytes = (*nbuf) * sizeof( float );
         r = srv_send( sock, &srv, sizeof( srv ) );
         if (r != GDIE_SUCCESS) return( r );
         r = srv_send( sock, rbuf, srv.nbytes );
         if (r != GDIE_SUCCESS) return( r );
         r = srv_receive( sock, &srv, sizeof( srv ) );
         if (r != GDIE_SUCCESS) return( r );
         (*nbuf) = srv.nbytes / sizeof( float );
         (*lchr) = 0;
         r = srv_receive( sock, rbuf, srv.nbytes );
         break;
      }
      /*
       * Read the cursor.
       */
      case 17: {
         int	nc, nf;

         srv.nbytes = (*nbuf) * sizeof( float );
         r = srv_send( sock, &srv, sizeof( srv ) );	/* send to server */
         if (r != GDIE_SUCCESS) return( r );
         r = srv_send( sock, rbuf, srv.nbytes );
         if (r != GDIE_SUCCESS) return( r );
         r = srv_receive( sock, &srv, sizeof( srv ) );
         if (r != GDIE_SUCCESS) return( r );
         nf = srv.nbytes / sizeof( float );
         nc = srv.nbytes - nf * sizeof( float );
         (*nbuf) = nf;
         (*lchr) = nc;
         r = srv_receive( sock, rbuf, srv.nbytes - nc );
         r = srv_receive( sock, chr.a, nc );
         break;
      }
      default: {
         break;
      }
   }
   return( r );					/* return to caller */
#else
   return( GDIE_NOT_IMPLEMENTED );		/* not implemented */
#endif
}
