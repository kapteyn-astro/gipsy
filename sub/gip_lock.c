/* gip_lock.c

        Copyright (c) Kapteyn Laboratorium Groningen 1993
        All Rights Reserved.

#>            gip_lock.dc3

Function:     gip_lock

Purpose:      Uses the GIPSY lockserver (see lckserver.doc) to put
              a lock on a file. The file does not have to exist.

Category:     SYSTEM

File:         gip_lock.c

Author:       K.G. Begeman

Call:         int gip_lock( char *path )

              gip_lock       returns 0 on success, or
                             -1  empty path
                             -2  cannot get current working directory
                             -3  unable to read .glock file
                             -4  cannot create socket
                             -5  cannot obtain hostbyname
                             -6  cannot connect to lskserver
                             -7  cannot obtain acknowledgement from lckserver
                             -8  wrong acknowledgement from lckserver
                             -9  cannot start lckserver
                             -10 sudden death of lckserver
                             -11 bad lskserver
                             -12 file was not locked (gip_unlock)
                             -13 file was already locked (gip_lock)
                             -14 lckserver out of sockets
                             -15 bad client
                             -16 unknown error
              path           name of the file

Updates:      Jan  5, 1993:  KGB Document created
              Dec  5, 2000:  JPT Fixed redundant close bug

#<

#>            gip_unlock.dc3

Function:     gip_unlock

Purpose:      Uses the GIPSY lockserver (see lckserver.doc) to remove
              a lock from a file. The file does not have to exist.

Category:     SYSTEM

File:         gip_lock.c

Author:       K.G. Begeman

Call:         int gip_unlock( char *path )

              gip_unlock     returns 0 on success, or
                             -1  empty path
                             -2  cannot get current working directory
                             -3  unable to read .glock file
                             -4  cannot create socket
                             -5  cannot obtain hostbyname
                             -6  cannot connect to lskserver
                             -7  cannot obtain acknowledgement from lckserver
                             -8  wrong acknowledgement from lckserver
                             -9  cannot start lckserver
                             -10 sudden death of lckserver
                             -11 bad lskserver
                             -12 file was not locked (gip_unlock)
                             -13 file was already locked (gip_lock)
                             -14 lckserver out of sockets
                             -15 bad client
                             -16 unknown error
              path           name of the file

Updates:      Jan  5, 1993:  KGB Document created

#<

*/

#include        "errno.h"                       /* <errno.h> */
#include        "limits.h"                      /* <limits.h> */
#include	"signal.h"			/* <signal.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */
#include	"xscanf.h"			/* read devces file */

#define	GLOCK_CONNECT			1	/* connection made */
#define	GLOCK_LOCK			2	/* give lock */
#define	GLOCK_NOLOCK			3	/* no lock on file */
#define	GLOCK_UNLOCK			4	/* file unlocked */
#define	GLOCK_NOSOCK			5	/* no more sockets */
#define	GLOCK_ILLEGAL			6	/* illegal request */
#define	GLOCK_DISCONNECT		7	/* disconnect from server */
#define	GLOCK_ONLOCK			8	/* already locked */

#define	GLOCK_ERROR_NOPATH		-1	/* empty path */
#define	GLOCK_ERROR_GETCWD		-2	/* cannot get cwd */
#define	GLOCK_ERROR_FILEREAD		-3	/* error reading glock file */
#define	GLOCK_ERROR_SOCKET		-4	/* error creating socket */
#define	GLOCK_ERROR_GETHOSTBYNAME	-5	/* error gethostbyname */
#define	GLOCK_ERROR_CONNECT		-6	/* error connect */
#define	GLOCK_ERROR_CONNECT1		-7	/* error obtaining ack */
#define	GLOCK_ERROR_CONNECT2		-8	/* error in ack */
#define	GLOCK_ERROR_SERVERSTART		-9	/* error starting server */
#define	GLOCK_ERROR_SERVERDEAD		-10	/* error server died */
#define	GLOCK_ERROR_SERVERBAD		-11	/* error from server */
#define	GLOCK_ERROR_NOTLOCKED		-12	/* file was not locked */
#define	GLOCK_ERROR_WASLOCKED		-13	/* file was already locked */
#define	GLOCK_ERROR_NOSOCKETS		-14	/* lskserver out of sockets */
#define	GLOCK_ERROR_CLIENTBAD		-15	/* bad client */
#define	GLOCK_ERROR_UNKNOWN		-16	/* ??? */

#define	MAXSOCKS			16	/* max. number of sockets */
#define	STRINGLEN			250	/* length of strings */

#include	<sys/time.h>			/* timeval definitions */
#include        <sys/types.h>                   /* define some weird types */
#include        <sys/socket.h>                  /* the socket things */
#if	defined(__aix__)
#include	<sys/select.h>			/* special for aix */
#endif
#include        <sys/un.h>                      /* unix things */
#include        <netinet/in.h>                  /* inet things */
#include        <netdb.h>                       /* network */

#if     !defined(htons) & !defined(__alpha__) & !defined(__linux__)
extern  u_short htons( );
#endif
#if	!defined(ntohs) & !defined(__alpha__) & !defined(__linux__)
extern	u_short	ntohs( );
#endif

typedef struct {				/* the lock struct */
   char	directory[FILENAME_MAX];		/* directory */
   int	locks;					/* number of locks */
   int	sock;					/* socket to lckserver */
} lock_struct;

static	int		nlocks = 0;		/* number of connections */
static	lock_struct	*mylock = NULL;		/* the memory bank */


/*
 * swapint( ) swaps int integers
 */

static	void	swapint( int *l, int nl )
{
   int		i, j;
   union {
      char	b[sizeof(int)];
      int	l;
   } u1, u2;

   for ( i = 0; i < nl; i++ ) {
      u1.l = l[i];
      for ( j = 0; j < sizeof( int ); j++ ) {
         u2.b[j] = u1.b[sizeof( int ) - j - 1];
      }
      l[i] = u2.l;
   }
}


/*
 * put( ) puts ndata bytes on the socket and returns 0 on success,
 * 1 on error.
 */

static	int	put( int socket,  void *data, int ndata )
{
   char	*p = (char *) data;		/* make char pointer */
   int	count = 0;			/* nul write counter */
   int	nd = 0;				/* number of bytes done */
   int	nl = ndata;			/* number of bytes left */
   int	nt = 0;				/* total number of bytes */
   int	r = 0;				/* return value */
   int	write( );			/* writes on socket */

   while (nl) {				/* while still some bytes left */
      while ((nd = write( socket, &p[nt], nl )) == -1 && errno == EINTR);
      if (nd < 0) { r = -1; break; }	/* an error */
      if (!nd && count++ > 10) { r = -1; break; }
      nl -= nd;				/* decrease */
      nt += nd;				/* increase */
   }
   return( r );				/* return to caller */
}


/*
 * get( ) gets ndata bytes from the socket. It returns 0 on success,
 * 1 on error.
 */

static	int	get( int socket, void *data, int ndata )
{
   char	*p = (char *) data;		/* make char pointer */
   int	count = 0;			/* nul read counter */
   int	nd = 0;				/* number of bytes done */
   int	nl = ndata;			/* number of bytes left to do */
   int	nt = 0;				/* total number of bytes */
   int	r = 0;				/* return value */
   int	read( );			/* read bytes from socket */

   while (nl) {				/* until nothing left to do */
      while ((nd = read( socket, &p[nt], nl )) == -1 && errno == EINTR);
      if (nd < 0) { r = -1; break; }	/* error */
      if (!nd && count++ > 10) { r = -1; break; }
      nl -= nd;				/* decrease */
      nt += nd;				/* increase */
   }
   return( r );				/* return to caller */
}


/*
 * split( ) decomposes a path name into the directory and the filename.
 */

static	int	split( char *path, char *dirc, char *name )
{
   char	*s;				/* pointer */
   int	l1, l2;				/* length counters */

   l1 = strlen( path );			/* length of path */
   if ( l1 == 0 ) return( GLOCK_ERROR_NOPATH );
   s = strrchr( path, '/' );		/* find last / */
   if ( s == NULL ) {			/* no directory, so use current */
#if	defined(__bsd__)		/* get current working directory */
      extern char	*getwd( );

      if ( getwd( dirc ) == NULL ) {
#else
      extern char	*getcwd( );

      if ( getcwd( dirc, FILENAME_MAX ) == NULL ) {
#endif
         return( GLOCK_ERROR_GETCWD );
      }
      strcpy( name, path );		/* we've got it */
   } else {				/* strip direcotry from path */
      s++;				/* after this, the filename */
      l2 = strlen( s );			/* length of filename */
      if ( l2 == 0 ) return( GLOCK_ERROR_NOPATH );
      strcpy( name, s );		/* save file name */
      strncpy( dirc, path, l1 - l2 );	/* now the directory */
      dirc[l1-l2] = 0;			/* add zero */
   }
   l1 = strlen( dirc );			/* length of directory */
   if ( dirc[l1-1] != '/' ) { dirc[l1] = '/'; dirc[l1+1] = 0; }
   return( 0 );				/* we're done */
}


/*
 * makeconnection( ) reads the .glock file and make the connection with
 * lckserver.
 */

static	int	makeconnection( FILE *file )
{
   char			hostname[STRINGLEN];	/* name of host */
   int			close( );		/* close */
   int			connect( );		/* connect */
   int			inet_addr( );		/* get inet address */
   int			port;			/* port number */
   int			r;			/* return value */
   int			socket( );		/* socket */
   int  		obuf[2];		/* data from lckserver */
   short		portnumber;		/* port number */
   struct hostent	*gethostbyname( );	/* gethostbyname */
   struct hostent	*hp;			/* host */
   struct sockaddr_in	server;			/* sock struct */

   if ( xscanf( file, "%s %d", hostname, &port ) != 2 ) {
      fclose( file );
      return( GLOCK_ERROR_FILEREAD );
   }
   fclose( file );				/* close .glock file */
   portnumber = port;				/* the port */
   r = socket( AF_INET, SOCK_STREAM, 0 );	/* create socket */
   if ( r == -1 ) return( GLOCK_ERROR_SOCKET );	/* error */
   hp = gethostbyname( hostname );		/* get host by name */
   if ( hp == NULL ) {				/* error */
      server.sin_addr.s_addr = inet_addr( hostname );
      if (server.sin_addr.s_addr == -1) {
         close( r );
         return( GLOCK_ERROR_GETHOSTBYNAME );
      }
   } else {
      server.sin_addr.s_addr = INADDR_ANY;	/* any address */
      memmove( (void *) &server.sin_addr, (void *) hp->h_addr, hp->h_length );
   }
   server.sin_family      = AF_INET;		/* inet */
   server.sin_port        = htons( portnumber );	/* the port */ 
   if ( connect( r, (void *) &server, sizeof( server ) ) == -1 ) {
      close( r );
      return( GLOCK_ERROR_CONNECT );
   } else {					/* connection okay */
      if ( get( r, obuf, sizeof( obuf ) ) ) {
         close( r );
         return( GLOCK_ERROR_CONNECT1 );	/* error */
      }
      if ( obuf[0] != 1 ) swapint( obuf, 2 );	/* swap bytes */
      if ( obuf[0] != 1 ) {			/* error */
         close( r );
         return( GLOCK_ERROR_CONNECT2 );
      } else if ( obuf[1] != GLOCK_CONNECT ) {	/* error */
         close( r );
         if ( obuf[1] == GLOCK_NOSOCK ) {
            return( GLOCK_ERROR_NOSOCKETS );
         } else {
            return( GLOCK_ERROR_SERVERBAD );
         }
      }
   }
   return( r );					/* we're done */
}


/*
 * serveropen( ) starts locker.
 */
static	FILE	*serveropen( char *dirc, int clear )
{
   FILE	*r;					/* points to .glock */
   char	filename[FILENAME_MAX];			/* name of .glock */

   sprintf( filename, "%s.glock", dirc );	/* make filename */
   if ( clear ) remove( filename );		/* remove it */
   r = fopen( filename, "r" );			/* open it */
   if ( r == NULL ) {				/* error */
      char	cmd[STRINGLEN];			/* command */

#ifndef	TESTBED
      sprintf( cmd, "$gip_exe/lckserver %s", dirc );
#else
      sprintf( cmd, "./lckserver %s", dirc );
#endif
      if ( system( cmd ) ) return( NULL );	/* error */
      sleep(2);                                 /* allow lckserver to start */
      r = fopen( filename, "r" );
   }
   return( r );					/* we're done */
}


/*
 * getsock( ) make the connection with the locker.
 */

static	int	getsock( char *dirc )
{
   FILE		*file;				/* points to .glock */
   int		r = 0;				/* return value */

   file = serveropen( dirc, 0 ); 		/* open server */
   if ( file == NULL ) return( GLOCK_ERROR_SERVERSTART );
   r = makeconnection( file );			/* make connection */
   if ( r < 0 ) {				/* error */
      file = serveropen( dirc, 1 );
      if ( file == NULL ) return( GLOCK_ERROR_SERVERSTART );
      r = makeconnection( file );
   }
   return( r );					/* we're done */
}


/*
 * contact( ) connects to the lock server.
 */

static	int	contact( char *dirc )
{
   int	s = 0;
   int	sock;

   while ( s < nlocks && strcmp( mylock[s].directory, dirc ) ) s++;
   if ( s == nlocks ) {				/* not in list */
      mylock = realloc( mylock, sizeof( lock_struct ) * ++nlocks );
      strcpy( mylock[s].directory, dirc );
      mylock[s].locks = 0;
   } else if ( mylock[s].locks ) {		/* still open */
      return( s );
   }
   if ( ( sock = getsock( dirc ) ) < 0 ) {	/* get socket */
      return( sock );				/* error */
   } else {					/* done */
      mylock[s].sock = sock;
      return( s );
   }
}


/*
 * dolock( ) does the (un)locking.
 */

static	int	dolock( char *path, int lockopt )
{
   char	dirc[FILENAME_MAX];			/* the directory */
   char	name[FILENAME_MAX];			/* the name */
   int	close( );
   int	s;
   int	r;
   int	ibuf[3];				/* to server */
   int	obuf[2];				/* from server */

   r = split( path, dirc, name );		/* split */
   if ( r ) return( r );			/* error */
   s = contact( dirc );				/* contact lckserver */
   if ( s < 0 ) return( s );			/* error */
   ibuf[0] = 1; ibuf[1] = lockopt; ibuf[2] = strlen( name );
   if ( put( mylock[s].sock, ibuf, sizeof( ibuf ) ) ) {
      close( mylock[s].sock );			/* error */
      mylock[s].locks = 0;
      return( GLOCK_ERROR_SERVERDEAD );
   }
   if ( put( mylock[s].sock, name, strlen( name ) ) ) {
      close( mylock[s].sock );			/* error */
      mylock[s].locks = 0;
      return( GLOCK_ERROR_SERVERDEAD );
   }
   if ( get( mylock[s].sock, obuf, sizeof( obuf ) ) ) {
      close( mylock[s].sock );			/* error */
      mylock[s].locks = 0;
      return( GLOCK_ERROR_SERVERDEAD );
   }
   if ( obuf[0] != 1 ) swapint( obuf, 2 );	/* swap bytes */
   if ( obuf[0] != 1 ) {
      close( mylock[s].sock );			/* error */
      mylock[s].locks = 0;
      return( GLOCK_ERROR_SERVERBAD );
   }
   switch( obuf[1] ) {				/* what happened ? */
      case GLOCK_LOCK: {			/* we got a lock */
         mylock[s].locks += 1;
         break;
      }
      case GLOCK_NOLOCK: {			/* there was no lock */
         r = GLOCK_ERROR_NOTLOCKED;
         break;
      }
      case GLOCK_ONLOCK: {			/* was already locked */
         r = GLOCK_ERROR_WASLOCKED;
         break;
      }
      case GLOCK_UNLOCK: {			/* removed lock */
         mylock[s].locks -= 1;
         if ( !mylock[s].locks ) {		/* disconnect from server */
            ibuf[0] = 1; ibuf[1] = GLOCK_DISCONNECT; ibuf[2] = 0;
            put( mylock[s].sock, ibuf, sizeof( ibuf ) );
            close( mylock[s].sock );
         }
         break;
      }
      case GLOCK_ILLEGAL: {			/* we did something wrong */
         r = GLOCK_ERROR_CLIENTBAD;
         break;
      }
      default: {				/* ??? */
         r = GLOCK_ERROR_UNKNOWN;
         break;
      }
   }
   return( r );
}


/*
 * gip_lock( ) puts a lock on a file.
 */

int	gip_lock( char *path )
{
   return( dolock( path, GLOCK_LOCK ) );
}


/*
 * gip_unlock( ) removes a lock.
 */

int	gip_unlock( char *path )
{
   return( dolock( path, GLOCK_UNLOCK ) );
}

#ifdef	TESTBED

int	main( int argc, char *argv[] )
{
   int	iarg;
   int	sleep( );

   for ( iarg = 1; iarg < argc; iarg += 2 ) {
      printf( "gip_lock   = %4d\n", gip_lock( argv[iarg] ) );
      sleep( atoi( argv[iarg+1] ) );
   }
   for ( iarg = 1; iarg < argc; iarg += 2 ) {
      printf( "gip_unlock = %4d\n", gip_unlock( argv[iarg] ) );
      sleep( atoi( argv[iarg+1] ) );
   }
   return( 0 );
}
#endif
