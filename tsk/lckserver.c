/* lckserver.c

        Copyright (c) Kapteyn Laboratorium Groningen 1993
        All Rights Reserved.

#>            lckserver.doc

Programme:    lckserver

Purpose:      Handles GIPSY (un)lock requests on files.

Category:     SYSTEM

File:         lckserver.c

Author:       K.G. Begeman

Use:          $gip_exe/lckserver directory

Description:  The GIPSY functions gip_lock and gip_unlock start the
              lckserver if it was not running yet. When started,
              it will create a inet socket on which it will be listening
              for connection requests from the gip_(un)lock functions.
              It will write a hidden file .glock in the directory where the
              locking is monitored by the server in which the hostname
              and the inet port number are stored (separated by a colon).
              If no requests are pending, the server will die after one hour
              and remove the .glock file. So the lckserver monitors the locking
              in a particular directory (the one entered on the command line).
              

Updates:      Jan  5, 1993: KGB Document created.

#<

*/

#include        "errno.h"                       /* <errno.h> */
#include        "limits.h"                      /* <limits.h> */
#include	"signal.h"			/* <signal.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */

#define	GLOCK_CONNECT		1		/* connection made */
#define	GLOCK_LOCK		2		/* give lock */
#define	GLOCK_NOLOCK		3		/* no lock on file */
#define	GLOCK_UNLOCK		4		/* file unlocked */
#define	GLOCK_NOSOCK		5		/* no more sockets */
#define	GLOCK_ILLEGAL		6		/* illegal request */
#define	GLOCK_DISCONNECT	7		/* disconnect from client */
#define	GLOCK_ONLOCK		8		/* already locked */

#define	MAXSOCKS		16		/* max. number of sockets */
#define	STRINGLEN		250		/* length of strings */

#define  open	OPEN

#include	<sys/time.h>			/* timeval definitions */
#include        <sys/types.h>                   /* define some weird types */
#include        <sys/socket.h>                  /* the socket things */
#ifdef	__aix__
#include	<sys/select.h>			/* special for aix */
#endif
#include        <sys/un.h>                      /* unix things */
#include        <netinet/in.h>                  /* inet things */
#include        <netdb.h>                       /* network */
#include	<fcntl.h>
#ifdef	__sysv__
#include	<sys/utsname.h>
#endif

#undef	open

#if     !defined(htons) & !defined(__alpha__) & !defined(__linux__)
extern  u_short htons( );
#endif
#if	!defined(ntohs) & !defined(__alpha__) & !defined(__linux__)
extern	u_short	ntohs( );
#endif

typedef	struct {				/* the lock struct */
   char	filename[FILENAME_MAX];			/* file name */
   int	lock;					/* has lock */
   int	request;				/* pending lock requests */
} lock_struct;

typedef struct {				/* the sock struct */
   int	sock;					/* the socket */
   int	stat;					/* status of socket */
} sock_struct;

static	int		nfiles = 0;		/* number of files monitored */
static	lock_struct	*mylock = NULL;		/* the lock struct */
static	sock_struct	mysock[MAXSOCKS];	/* the sock struct */


/*
 * swapint( ) swaps int integers
 */

static	void	swapint( int *l, int nl )
{
   int		i, j, k;
   union {
      char	b[sizeof(int)];
      int	l;
   } u1, u2;

   for ( i = 0; i < nl; i++ ) {
      u1.l = l[i];
      for ( j = 0, k = sizeof( int ); j < sizeof( int ); j++ ) {
         u2.b[j] = u1.b[--k];
      }
      l[i] = u2.l;
   }
}


/*
 * put( ) puts ndata bytes on the socket and returns 0 on success,
 * 1 on error.
 */

static	int	put( int socket, void *data, int ndata )
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
 * myput( ) puts bytes on the socket.
 */

static	int	myput( int sock, int code )
{
   int	buf[2];

   buf[0] = 1; buf[1] = code;		/* prepare data to send */
   return( put( sock, buf, sizeof( buf ) ) );
}


/*
 * cleanupsock( ) closes socket and removes locks and lock requests from
 * list.
 */

static	void	cleanupsock( int s )
{
   int	close( );
   int	i;

   if ( mysock[s].stat ) {		/* socket is alive */
      close( mysock[s].sock );		/* close it */
      mysock[s].stat = 0;		/* make it dead */
      for ( i = 0; i < nfiles; i++ ) {	/* remove requests */
         if ( mylock[i].request & ( 1 << s ) ) {
            mylock[i].request -= ( 1 << s );
         }
      }
      for ( i = 0; i < nfiles; i++ ) {	/* remove locks */
         if ( mylock[i].lock == ( 1 << s ) ) {
            mylock[i].lock = 0;
         }
      }
   }
}


/*
 * checkrequests( ) checks for lock requests
 */

static	void	checkrequests( void )
{
   int	i, s;

   for ( i = 0; i < nfiles; i++ ) {	/* lock for requests */
      if ( mylock[i].request && !mylock[i].lock ) {
         for ( s = 0; s < MAXSOCKS && !mylock[i].lock; s++ ) {
            if ( mylock[i].request & ( 1 << s ) ) {
               if ( myput( mysock[s].sock, GLOCK_LOCK ) ) {
                  cleanupsock( s );
               } else {
                  mylock[i].lock     = ( 1 << s );
                  mylock[i].request -= ( 1 << s );
               }
            }
         }
      }
   }
}


/*
 * addlock( ) tries to put lock on a file.
 */

static	void   addlock( int s, char *filename )
{
   int	i = 0;

   while ( i < nfiles && strcmp( mylock[i].filename, filename ) ) i++;
   if ( i == nfiles ) {			/* not in list */
      mylock = realloc( mylock, sizeof( lock_struct ) * ( ++nfiles ) );
      strcpy( mylock[i].filename, filename );
      mylock[i].lock    = 0;
      mylock[i].request = 0;
   }
   if ( !mylock[i].lock ) {		/* no lock on file */
      if ( myput( mysock[s].sock, GLOCK_LOCK ) ) {
         cleanupsock( s );
      } else {
         mylock[i].lock = ( 1 << s );
      }
   } else if ( ( mylock[i].lock & ( 1 << s ) ) ) {
      if ( myput( mysock[s].sock, GLOCK_ONLOCK ) ) {
         cleanupsock( s );
      }
   } else {				/* put in request list */
      mylock[i].request |= ( 1 << s );
   }
}


/*
 * dellock( ) removes lock from a file.
 */

static	void	dellock( int s, char *filename )
{
   int	i = 0;

   while ( i < nfiles && strcmp( mylock[i].filename, filename ) ) i++;
   if ( i == nfiles || !( mylock[i].lock & ( 1 << s ) ) ) {
      if ( myput( mysock[s].sock, GLOCK_NOLOCK ) ) {
         cleanupsock( s );
      }
   } else {				/* in list */
      if ( myput( mysock[s].sock, GLOCK_UNLOCK ) ) {
         cleanupsock( s );
      } else {
         mylock[i].lock = 0;		/* mark not locked */
      }
   }
}


/*
 * dosockopt( ) reads the instructions from the socket
 */

static	void	dosockopt( int s )
{
   char	filename[FILENAME_MAX];
   int	buf[3];

   if ( get( mysock[s].sock, buf, sizeof( buf ) ) ) {
      cleanupsock( s );
   } else {
      if ( buf[0] != 1 ) swapint( buf, 3 );
      if ( buf[0] != 1 ) {
         cleanupsock( s );
      } else {
         if ( get( mysock[s].sock, filename, buf[2] ) ) {
            cleanupsock( s );
         } else {
            filename[buf[2]] = 0;	/* add zero byte */
            switch( buf[1] ) {		/* what shold we do */
               case GLOCK_LOCK: {	/* lock it */
                  addlock( s, filename );
                  break;
               }
               case GLOCK_UNLOCK: {	/* unlock it */
                  dellock( s, filename );
                  break;
               }
               case GLOCK_DISCONNECT: {	/* disconnect from client */
                  cleanupsock( s );
                  break;
               }
               default: {		/* we don't know */
                  if ( myput( mysock[s].sock, GLOCK_ILLEGAL ) ) {
                     cleanupsock( s );
                  }
                  break;
               }
            }
         }
      }
   }
   checkrequests( );			/* check all requests */
#ifdef	TESTBED
/*
 * Here we show the internal lock status in TESTBED mode
 */
{
   FILE	*tf;
   int	n;
   tf = fopen( "lckserver.log", "a" );
   for ( n = 0; n < nfiles; n++ ) {
      fprintf( tf, "%30.30s %8x %8x\n", mylock[n].filename, mylock[n].lock, mylock[n].request );
   }
   fclose( tf );
}
#endif
}


/*
 * action( ) listens for connects and does the locking.
 */

static	int	action( int listensock )
{
   int	n;
   int	nsocks;
   int	msock;
   int	select( );

#ifdef	TESTBED
/*
 * Here we create the log file in TESTBED mode
 */
{
   FILE	*tf;
   tf = fopen( "lckserver.log", "w" );
   fprintf( tf, "                      FILENAME    LOCKS REQUESTS\n" );
   fclose( tf );
}
#endif
   for ( n = 0; n < MAXSOCKS; n++ ) {
      mysock[n].stat = 0;
   }
   do {					/* loop */
      fd_set		read_fds;
      int		nfd;
      struct timeval	to;

      FD_ZERO( &read_fds );
      FD_SET( listensock, &read_fds );
      for ( nsocks = 0, msock = listensock, n = 0; n < MAXSOCKS; n++ ) {
         if ( mysock[n].stat ) {
            nsocks += 1;
            FD_SET( mysock[n].sock, &read_fds );
            if ( mysock[n].sock > msock ) msock = mysock[n].sock;
         }
      }
      to.tv_sec  = 3600;
      to.tv_usec = 0;
      while ((nfd = select( msock + 1, (void *) &read_fds, NULL, NULL, &to )) == -1 && errno == EINTR);
      if ( nfd == 0 && nsocks == 0 ) break;	/* time elapsed */
      if ( nfd > 0 ) {
         if (FD_ISSET( listensock, &read_fds )) {
            int	accept( );
            int	close( );
            int	msgsock;

            nfd -= 1;
            msgsock = accept( listensock, NULL, NULL );
            if (msgsock != -1) {
               for ( n = 0; n < MAXSOCKS && mysock[n].stat; n++ );
               if ( n < MAXSOCKS ) {
                  if ( myput( msgsock, GLOCK_CONNECT ) ) {
                     close( msgsock );
                  } else {
                     mysock[n].sock = msgsock;
                     mysock[n].stat = 1;
                  }
               } else {
                  myput( msgsock, GLOCK_NOSOCK );
                  close( msgsock );
               }
            }
         }
      }
      if ( nfd > 0 ) {
         for ( n = 0; nfd && n < MAXSOCKS; n++ ) {
            if ( mysock[n].stat && FD_ISSET( mysock[n].sock, &read_fds ) ) {
               dosockopt( n );
               nfd -= 1;
            }
         }
      }
   } while (1);
   return( 0 );
}


/*
 * dolocker( ) starts the real remote tape server.
 */

static	int	dolocker( char *directory )
{
   char			filename[FILENAME_MAX+1];
   char			hostname[STRINGLEN];
   char			string[STRINGLEN];
   int			bind( );
   int			close( );
   int			fd;
   int			fork( );
   int			getsockname( );
   int			length;
   int			listen( );
   int			listensock;
   int			nw;
   int			open( );
   int			r;
   int			socket( );
   int			write( );
   short		port = 0;
   struct sockaddr_in	server;

#ifdef	__bsd__
   {
      int	gethostname( );

      if ( gethostname( hostname, STRINGLEN ) == -1 ) {
         fprintf( stderr, "lckserver: cannot obtain hostname\n" );
         return( EXIT_FAILURE );
      }
   }
#else
   {
      struct utsname	name;

      if ( uname( &name ) == -1 ) {
         fprintf( stderr, "lckserver:cannot obtain hostname\n" );
         return( EXIT_FAILURE );
      } else {
         strcpy( hostname, name.nodename );
      }
   }
#endif
   /*
    * Now get inet address of host.
    */
   {
      char		*addr;
      char		*inet_ntoa( );
      struct hostent	*hp;
      struct hostent	*gethostbyname( );
      struct in_addr	ha;

      hp = gethostbyname( hostname );
      if ( ( hp == NULL ) && ( strchr( hostname, '.' ) == NULL ) ) {
         char	dname[STRINGLEN];
         int	getdomainname( );

         if (!getdomainname( dname, sizeof( dname ))) {
            strcat( hostname, "." );
            strcat( hostname, dname );
            hp = gethostbyname( hostname );
         }
      }
      if (hp != NULL) {
         memmove( (void *) &ha, (void *) hp->h_addr, hp->h_length );
#if     defined(__sun__) & defined(__GNUC__) & !defined(__i386__)
         addr = inet_ntoa( &ha );
#else
         addr = inet_ntoa( ha );
#endif
         strcpy( hostname, addr );
      }
   }
   sprintf( filename, "%s.glock", directory );
   listensock = socket( AF_INET, SOCK_STREAM, 0 );
   if (listensock < 0) {
      fprintf( stderr, "lckserver: cannot create socket\n" );
      return( EXIT_FAILURE );
   }
   server.sin_family      = AF_INET;
   server.sin_addr.s_addr = INADDR_ANY;
   server.sin_port        = htons( port );
   length = sizeof( server );
   r = bind( listensock, (void *) &server, length );
   if (r == -1) {
      fprintf( stderr, "lckserver: cannot bind to socket\n" );
      return( EXIT_FAILURE );
   }
   r = getsockname( listensock, (void *) &server, &length );
   if (r == -1) {
      fprintf( stderr, "lckserver: getsockname error\n" );
      return( EXIT_FAILURE );
   }
   port = ntohs( server.sin_port );
   fd = open( filename, O_CREAT | O_EXCL | O_RDWR, 0644 );
   if (fd == -1) {
      fprintf( stderr, "lckserver: cannot create %s\n", filename );
      if (errno == EEXIST) {
         return( EXIT_SUCCESS );
      } else {
         return( EXIT_FAILURE );
      }
   }
   nw = sprintf( string, "%s:%d\n", hostname, (int) port );
   if ( write( fd, string, nw ) == -1 ) {
      fprintf( stderr, "lckserver: cannot write to %s\n", filename );
      close( fd );
      remove( filename );
      return( EXIT_FAILURE );
   }
   close( fd );
   fclose( stdin );
   fclose( stdout );
   fclose( stderr );
   if (!(r = fork( ))) {
      r = listen( listensock, 5 );
      if (r == -1) {
         remove( filename );
         return( EXIT_FAILURE );
      }
      r = action( listensock );
      remove( filename );
      return( EXIT_FAILURE );
   } else if (r == -1) {
      close( listensock );
      remove( filename );
      return( EXIT_FAILURE );
   } else {
      close( listensock );
      return( EXIT_SUCCESS );
   }
}


/*
 * Main programme. Needs arguments from initiator.
 */

int	main( int argc, char *argv[] )
{
   char directory[FILENAME_MAX];	/* directory */
   int	l;				/* length of directory */

   if ( argc != 2 ) {
      fprintf( stderr, "lckserver: wrong or missing arguments\n" );
      return( EXIT_FAILURE );
   }
   strcpy( directory, argv[1] );
   l = strlen( directory );
   if ( directory[l-1] != '/' ) {
      directory[l++] = '/';
      directory[l] = '\0';
   }
   return( dolocker( directory ) );
}
