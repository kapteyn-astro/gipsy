/* gftp.c

        Copyright (c) Kapteyn Laboratorium Groningen 1993, 2000
        All Rights Reserved.

#>            gftp.doc

Program:      gftp

Purpose:      Retrieves GIPSY sources from the GIPSY source server.

Category:     MANAGEMENT

File:         gftp.c

Author:       K.G. Begeman

Use:          gftp contacts the GIPSY source server and obtains GIPSY
              sources for remote updates. The programme is usually
              started from compile.
              The file $gip_loc/server is read to get the name of
              the GIPSY source server. If $gip_loc/server is not
              present, $gip_sys/server.mgr is tried.
              
              When compiled with -DSRC_SERVER, the resulting executable
              will be the GIPSY source server.

Updates:      Mar 16, 1993: KGB, document created.
              Nov 30, 2000: JPT, changed for server's new source path.
              Dec 13, 2011: JPT, retry gethostbyname() with "localhost"
                                 if it fails with "normal" hostname.
              
#<

*/

#define         EXT_ROOT "/tha3/users/gipsy"    /* pseudo gip_root */

#include	"ctype.h"			/* <ctype.h> */
#include        "errno.h"                       /* <errno.h> */
#include        "limits.h"                      /* <limits.h> */
#include	"signal.h"			/* <signal.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */

#ifndef	SRC_SERVER
#include	"xscanf.h"			/* define xscanf */
#endif

#include	<fcntl.h>			/* fcntl definitions */
#include	<sys/time.h>			/* timeval definitions */
#include        <sys/types.h>                   /* define some weird types */
#include        <sys/socket.h>                  /* the socket things */
#if	defined(__aix__)
#include	<sys/select.h>			/* special for aix */
#endif
#include        <sys/un.h>                      /* unix things */
#include        <netinet/in.h>                  /* inet things */
#include        <netdb.h>                       /* network */
#if	defined(__sysv__)
#include	<sys/utsname.h>
#endif

#ifdef	SRC_SERVER
#include	"gipsyc.h"
#include	"getpath.h"
#include	"nelc.h"
#endif

#if     !defined(htons) & !defined(__alpha__) & !defined(__linux__)
extern  u_short htons( );
#endif
#if	!defined(ntohs) & !defined(__alpha__) & !defined(__linux__)
extern	u_short	ntohs( );
#endif

#ifndef	BLOCKLEN
#define	BLOCKLEN	10240		/* bytes in block */
#endif
#define	MAXBLOCKLEN	102400		/* maximum size of block */
#ifndef	SRV_MAIL
#define	SRV_MAIL	"gipsy@[129.125.6.224]"
#endif
#ifndef	SRV_NAME
#define	SRV_NAME	"129.125.6.224"
#endif
#ifndef	SRV_PATH
#define	SRV_PATH	"/tha3/users/gipsy"
#endif
#define	STRINGLEN	250		/* length of strings */

#define	OP_CON		0		/* connect to server */
#define	OP_DIS		1		/* disconnect from server */
#define	OP_GET		2		/* get file from server */
#define	OP_PUT		3		/* put file on server */
#define	OP_NOP		4		/* no operation */

typedef struct {			/* the op code struct */
   int	counts;				/* counter */
   int	opcode;				/* function */
   int	status;				/* status */
} op_struct;				/* the struct */

static	char	*gip_root = NULL;	/* GIPSY root directory */
static	char	programme[STRINGLEN];	/* name of programme */
static	char	srvmessage[STRINGLEN];	/* message from server */
static	int	mode = 0;		/* mode */
static	int	regnum = 0;		/* client register number */

#ifdef	SRC_SERVER

static	int	slv_sock = -1;		/* socket to client */
static  int	swap = 0;		/* swap needed ? */

#else

static	int	srv_sock = -1;		/* socket to server */

#endif


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

#ifdef	SRC_SERVER


/*
 * flop( ) swabs the bytes of the op_struct.
 */

static	op_struct	flop( op_struct op )
{
   int		i;
   op_struct	r;
   union {
      unsigned char	b[sizeof(int)];
      int		l;
   } u1, u2;

   u1.l = op.counts;
   for (i = 0; i < sizeof(int); i++) {
      u2.b[sizeof(int)-i-1] = u1.b[i];
   }
   r.counts = u2.l;
   u1.l = op.opcode;
   for (i = 0; i < sizeof(int); i++) {
      u2.b[sizeof(int)-i-1] = u1.b[i];
   }
   r.opcode = u2.l;
   u1.l = op.status;
   for (i = 0; i < sizeof(int); i++) {
      u2.b[sizeof(int)-i-1] = u1.b[i];
   }
   r.status= u2.l;
   return( r );
}

#endif

#ifndef	SRC_SERVER


/*
 * name_of_host returns name of current host.
 */

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

#endif

/*
 * checksum calculates the checksum for a binary buffer.
 */

static	int	checksum( char *buf, int len )
{
   int	sum = 0;
   int	n;

   for ( n = 0; n < len; n++ ) {
      if ((buf[n] & 0x80)) {
         sum += (buf[n] & 0x7f) + 0x80;
      } else {
         sum += buf[n];
      }
   }
   return( sum );
}

#ifdef	SRC_SERVER


/*
 * slv_con( ) connects to client.
 */

static	int	slv_con(  op_struct op )
{
   FILE	*f;
   char	client[STRINGLEN];
   char	lckfil[FILENAME_MAX];
   char	line[STRINGLEN];
   char	regfil[FILENAME_MAX];
   int	close( );
   int	lid;
   int	meslen;

   if (slv_sock < 0) return( -1 );
   if (swap) op = flop( op );
   if (get( slv_sock, client, op.counts )) {
      return( -2 );
   }
   sprintf( regfil, "%s/adm/register", gip_root );
   sprintf( lckfil, "%s/adm/.srvlock", gip_root );
   while ((lid = open( lckfil, O_CREAT | O_EXCL | O_RDWR, 0644 )) == -1) {
      int	sleep( );

      sleep( 10 );				/* wait 10 seconds */
   }
   f = fopen( regfil, "r+" );
   if (f == NULL) {
      f = fopen( regfil, "w" );
      if ( f != NULL ) {
         fprintf( f, "%s\n", client );
         fclose( f );
         regnum = 1;
      }
   } else {
      int	fs;

      do {
         regnum++;
      } while (((fs = fscanf( f, "%s", line )) != EOF) && strcmp( line, client ));
      if (fs == EOF && client[0]) {
         fprintf( f, "%s\n", client );
      }
      fclose( f );
   }
   close( lid ); remove( lckfil );		/* remove lock file */
   meslen = strlen( srvmessage );
   op.opcode = OP_NOP;
   op.status = regnum;
   op.counts = meslen;
   if (swap) op = flop( op );
   if (put( slv_sock, &op, sizeof( op_struct ) )) {
      return( -3 );
   }
   if (meslen) {
      if (put( slv_sock, srvmessage, meslen + 1 )) {
         return( -4 );
      }
   }
   return( 0 );
}

#else


/*
 * srv_con( ) connects to the GIPSY Source Server.
 * Arguments: srv    name of host on which GSS is running
 */

static	int	srv_con( char *srv )
{
   char			*addr;			/* Internet address */
   char			slv[STRINGLEN];		/* client */
   char			*inet_ntoa( );		/* net to text */
   int			close( );		/* close */
   int			connect( );		/* connect */
   int			inet_addr( );		/* get net address */
   int			r;			/* return value */
   int			socket( );		/* socket */
   op_struct		op;			/* opcode */
   short		port;			/* port number */
   struct hostent	*gethostbyname( );	/* gethostbyname */
   struct hostent	*hp;			/* host */
   struct in_addr	ha;
   struct servent	*sp;
   struct sockaddr_in	server;			/* sock struct */

   name_of_host( slv, STRINGLEN );		/* name of client */
   sp = getservbyname( "GIPSYRSS", "tcp" );	/* get port number */
   if ( sp != NULL ) {				/* yes */
      port = htons( sp->s_port );		/* the port */
   } else {
#ifdef	GIPSYRSS
      port = GIPSYRSS;				/* the port */
#else
      port = 2841;				/* the port */
#endif
   }
   r = socket( AF_INET, SOCK_STREAM, 0 );	/* create socket */
   if ( r == -1 ) return( -1 );			/* error */
   hp = gethostbyname( srv );			/* get host by name */
   if ( hp == NULL ) {				/* error */
      server.sin_family      = AF_INET;		/* family */
      server.sin_addr.s_addr = inet_addr( srv );
      if (server.sin_addr.s_addr == -1) {
         close( r );
         return( -2 );
      }
   } else {
      server.sin_family      = hp->h_addrtype;	/* inet */
      server.sin_addr.s_addr = INADDR_ANY;	/* any address */
      memmove( (void *) &server.sin_addr, (void *) hp->h_addr, hp->h_length );
   }
   server.sin_port = htons( port );		/* the port */ 
   if ( connect( r, (void *) &server, sizeof( server ) ) == -1 ) {
      close( r );
      return( -3 );
   }
   hp = gethostbyname( slv );			/* host info */
   if (hp==NULL) hp = gethostbyname("localhost");
   memmove( &ha, (void *) hp->h_addr, hp->h_length );
#if	defined(__sun__) & defined(__GNUC__) & !defined(__i386__)
   addr = inet_ntoa( &ha );
#else
   addr = inet_ntoa( ha );
#endif
   op.opcode = OP_CON;
   op.counts = strlen( addr ) + 1;
   op.status = 1;
   if ( put( r, &op, sizeof( op_struct ) ) ) {
      close( r );
      return( -4 );	/* error */
   }
   if ( put( r, addr, op.counts ) ) {
      close( r );
      return( -5 );
   }
   if ( get( r, &op, sizeof( op_struct ) ) ) {
      close( r );
      return( -6 );
   }
   if ( op.counts != 0) {
      if (get( r, srvmessage, op.counts )) {
         close( r );
         return( -7 );
      }
   }
   regnum = op.status;				/* client register number */
   srv_sock = r;
   if (op.counts) return( -8 );
   return( 0 );					/* we're done */
}


/*
 * srv_dis( ) disconnects from GSS.
 */


static	int	srv_dis( void )
{
   int		close( );
   int		r;
   op_struct	op;

   if (srv_sock < 0) return( -1 );
   op.opcode = OP_DIS;
   op.counts = 0;
   op.status = 0;
   r = put( srv_sock, &op, sizeof( op_struct ) );
   close( srv_sock );
   srv_sock = -1;
   return( r );
}

#endif

#ifdef	SRC_SERVER

/*
 * subst_path() substitutes the old gipsy root /tha3/users/gipsy
 * by the current gip_root. Clients need not know of the actual
 * server gip_root anymore.
 */

static void subst_path(char *filename)
{
   int l_ext;
   if (strstr( filename, EXT_ROOT) == filename) {
      char tmpname[FILENAME_MAX+1];
      l_ext = strlen(EXT_ROOT);
      strcpy(tmpname, filename+l_ext);
      sprintf(filename, "%s%s", gip_root, tmpname);
   }
}


/*
 * slv_get( ) sends a file to the client.
 * Arguments: op       opcode
 *
 */

static	int	slv_get( op_struct op  )
{
   FILE		*f = NULL;
   char		filename[FILENAME_MAX+1];
   int		size = 0;

   fchar	name;
   name.a = filename; name.l = FILENAME_MAX;
   if (slv_sock < 0) return( -1 );
   if (get( slv_sock, filename, op.counts )) {
      return( -2 );
   }
   subst_path(filename);
   if (getpath_c( name )) {
      op.status = -2;
   } else {
      filename[nelc_c( name )] = 0;
      if (strstr( filename, gip_root ) != filename ) {
         op.status = -2;
      } else {
         f = fopen( filename, "rb" );
         if ( f != NULL ) {
            fseek( f, 0, SEEK_END );
            size = ftell( f );
            fseek( f, 0, SEEK_SET );
            op.status = 0;
         } else {
            op.status = -1;
         }
      }
   }
   op.opcode = OP_NOP;
   op.counts = size;
   if (swap) op = flop( op );
   if (put( slv_sock, &op, sizeof( op_struct ) )) {
      return( -2 );
   }
   if (f == NULL) return( 0 );
   if (get( slv_sock, &op, sizeof( op_struct ) )) {
      return( -3 );
   }
   if (swap) op = flop( op );
   if (op.status) {
      fclose( f );
      return( 0 );
   }
   while ( size ) {
      char	buf[MAXBLOCKLEN];
      int	n;
      int	count = 0;

      n = ( BLOCKLEN < size ? BLOCKLEN : size );
      if ( fread( buf, 1, n, f ) == n ) {
         op.opcode = OP_NOP;
         op.counts = n;
         op.status = checksum( buf, n );
      } else {
         op.opcode = OP_NOP;
         op.counts = 0;
         op.status = -1;
      }
      if (swap) op = flop( op );
      if (put( slv_sock, &op, sizeof( op_struct ) )) {
         fclose( f );
         return( -4 );
      }
      if (op.counts == 0) {
         fclose( f );
         return( 0 );
      }
      do {
         count += 1;
         if (put( slv_sock, buf, n )) {
            fclose( f );
            return( -5 );
         }
         if (get( slv_sock, &op, sizeof( op_struct ) )) {
            fclose( f );
            return( -6 );
         }
         if (swap) op = flop( op );
         if (op.counts == -1) {
            fclose( f );
            return( -7 );
         }
         if (count > 10) {
            fclose( f );
            return( -8 );
         }
      } while (op.status);
      size -= n;
   }
   fclose( f );
   return( 0 );
}

#else


/*
 * srv_get( ) retrieves a file from GSS.
 * Arguments: srvname   GIPSY source file remote
 *            slvname   GIPSY source file local
 *
 */

static	int	srv_get( char *srvname, char *slvname )
{
   FILE		*f;
   int		close( );
   int		size;
   op_struct	op;

   if (srv_sock < 0 ) return( -1 );
   op.opcode = OP_GET;
   op.counts = strlen( srvname ) + 1;
   op.status = 0;
   if (put( srv_sock, &op, sizeof( op_struct ) ) ) {
      close( srv_sock ); srv_sock = -1; return( -1 );
   }
   if (put( srv_sock, srvname, op.counts )) {
      close( srv_sock ); srv_sock = -1; return( -2 );
   }
   if (get( srv_sock, &op, sizeof( op_struct ) ) ) {
      close( srv_sock ); srv_sock = -1; return( -3 );
   }
   if (op.status) return( -4 );
   size = op.counts;
   if (!size) return( -5 );
   f = fopen( slvname, "wb" );
   op.opcode = OP_NOP;
   op.status = ( f == NULL );
   op.counts = 0;
   if (put( srv_sock, &op, sizeof( op_struct ) )) {
      close( srv_sock ); srv_sock = -1; return( -6 );
   }
   if (op.status) return( -7 );
   while (size) {
      char	buf[MAXBLOCKLEN];
      int	count = 0;
      int	n;
      int	sum;

      if (get( srv_sock, &op, sizeof( op_struct ) )) {
         fclose( f ); remove( slvname );
         close( srv_sock ); srv_sock = -1; return( -8 );
       }
       sum = op.status;
       n = op.counts;
       if (sum < 0) {
          fclose( f );
          remove( slvname );
          return( -9 );
       }
       do {
         count += 1;
         if (get( srv_sock, buf, n )) {
            fclose( f ); remove( slvname );
            close( srv_sock ); srv_sock = -1; return( -10 );
         }
         sum -= checksum( buf, n );
         op.opcode = OP_NOP;
         op.counts = 0;
         op.status = sum;
         if (sum == 0) {
            if (fwrite( buf, 1, n, f ) != n) {
               op.counts = -1;
               fclose( f ); remove( slvname );
            }
         }
         if (put( srv_sock, &op, sizeof( op_struct ) )) {
            fclose( f ); remove( slvname );
            close( srv_sock ); srv_sock = -1; return( -11 );
         }
         if (op.counts) return( -12 );
         if (count > 10) {
            fclose( f ); remove( slvname );
            close( srv_sock ); srv_sock = -1; return( -13 );
         }
      } while (sum);
      size -= n;
   }
   fclose( f );
   return( 0 );
}

#endif

#ifdef	SRC_SERVER


/*
 * slv_put( ) receives a file from the client.
 * Arguments: op       opcode
 *
 */

static	int	slv_put( op_struct op )
{
   FILE		*f = NULL;
   char		filename[FILENAME_MAX];
   int		size = 0;

   if (slv_sock < 0) return( -1 );
   sprintf( filename, "%s/adm/register.%5.5d", gip_root, regnum );
   f = fopen( filename, "wb" );
   if ( f != NULL ) {
      op.status = 0;
   } else {
      op.status = -1;
   }
   op.opcode = OP_NOP;
   op.counts = 0;
   if (swap) op = flop( op );
   if (put( slv_sock, &op, sizeof( op_struct ) )) {
      return( -2 );
   }
   if (f == NULL) return( -3 );
   if (get( slv_sock, &op, sizeof( op_struct ) )) {
      return( -4 );
   }
   if (swap) op = flop( op );
   if (op.status) {
      fclose( f );
      remove( filename );
      return( -5 );
   }
   size = op.counts;
   while ( size ) {
      char	buf[MAXBLOCKLEN];
      int	count = 0;
      int	n;
      int	sum;

      if (get( slv_sock, &op, sizeof( op_struct ) )) {
         fclose( f ); remove( filename ); return( -6 );
      }
      if (swap) op = flop( op );
      sum = op.status;
      n = op.counts;
      if (sum < 0) {
         fclose( f ); remove( filename ); return( -7 );
      }
      do {
         count += 1;
         if (get( slv_sock, buf, n )) {
            fclose( f ); remove( filename ); return( -8 );
         }
         sum -= checksum( buf, n );
         op.opcode = OP_NOP;
         op.counts = 0;
         op.status = sum;
         if (sum == 0) {
            if (fwrite( buf, 1, n, f ) != n) {
               op.counts = -1;
               fclose( f ); remove( filename );
            }
         }
         if (swap) op = flop( op );
         if (put( slv_sock, &op, sizeof( op_struct ) )) {
            fclose( f ); remove( filename ); return( -9 );
         }
         if (op.counts) return( -10 );
         if (count > 10) {
            fclose( f ); remove( filename ); return( -11 );
         }
      } while (sum);
      size -= n;
   }
   fclose( f );
   return( 0 );
}

#else


/*
 * srv_put( ) sends a file to GSS.
 * Arguments: filename   GIPSY source file local
 *
 */

static	int	srv_put( char *filename  )
{
   FILE		*f = NULL;
   int		close( );
   int		size = 0;
   op_struct	op;

   if (srv_sock < 0 ) return( -1 );
   op.opcode = OP_PUT;
   op.counts = 0;
   op.status = 0;
   if (put( srv_sock, &op, sizeof( op_struct ) ) ) {
      close( srv_sock ); srv_sock = -1; return( -2 );
   }
   if (get( srv_sock, &op, sizeof( op_struct ) ) ) {
      close( srv_sock ); srv_sock = -1; return( -3 );
   }
   if (op.status) return( -4 );
   f = fopen( filename, "rb" );
   if (f != NULL) {
      fseek( f, 0, SEEK_END );
      size = ftell( f );
      fseek( f, 0, SEEK_SET );
   }
   op.opcode = OP_NOP;
   op.status = ( f == NULL );
   op.counts = size;
   if (put( srv_sock, &op, sizeof( op_struct ) )) {
      close( srv_sock ); srv_sock = -1; return( -5 );
   }
   while (size) {
      char	buf[MAXBLOCKLEN];
      int	count = 0;
      int	n;

      n = ( BLOCKLEN < size ? BLOCKLEN : size );
      if ( fread( buf, 1, n, f ) == n ) {
         op.opcode = OP_NOP;
         op.counts = n;
         op.status = checksum( buf, n );
      } else {
         op.opcode = OP_NOP;
         op.counts = 0;
         op.status = -1;
      }
      if (put( srv_sock, &op, sizeof( op_struct ) )) {
         fclose( f );
         close( srv_sock ); srv_sock = -1; return( -6 );
      }
      if (op.counts == 0) {
         fclose( f );
         return( 0 );
      }
      do {
         count += 1;
         if (put( srv_sock, buf, n )) {
            fclose( f );
            close( srv_sock ); srv_sock = -1; return( -7 );
         }
         if (get( srv_sock, &op, sizeof( op_struct ) )) {
            fclose( f );
            close( srv_sock ); srv_sock = -1; return( -8 );
         }
         if (op.counts == -1) {
            fclose( f ); return( -9 );
         }
         if (count > 10) {
            fclose( f ); return( -10 );
         }
      } while (op.status);
      size -= n;
   }
   fclose( f );
   return( 0 );
}

#endif

#ifdef	SRC_SERVER

#define	WNOHANG		1		/* from <sys/wait.h> */


/*
 * reaper( ) uses the SIGCHLD signal to "reap" child processes wich have
 * exited because the daemon did not explicity wait for their termination.
 * This is to prevent zomby processes.
 */

static	void	reaper( int sig )
{
   int	wait3( );

   while ( wait3( NULL, WNOHANG, NULL ) > 0 );	/* wait */
   signal( SIGCHLD, reaper );		/* reinstate the handler */
}


/*
 * dosockop( ) does the socket communication and tape io.
 */

static	int	dosockop( void )
{
   fd_set	read_fds;		/* list of fd's */
   int		close( );
   int		cont = 1;		/* continue */
   op_struct	op;			/* mag tape op struct */

   do {					/* main action loop */
      int		nfd;		/* number of fd's */
      int		select( );	/* select */

      FD_ZERO( &read_fds );		/* clear */
      FD_SET( slv_sock, &read_fds );	/* set for this socket */
      while ((nfd = select( slv_sock + 1, (void *) &read_fds, NULL, NULL, NULL )) == -1 && errno == EINTR);
      if (nfd > 0 && FD_ISSET( slv_sock, &read_fds )) {	/* this socket */
         if (get( slv_sock, &op, sizeof( op_struct ) )) {
            cont = 0;			/* don't continue */
            break;			/* leave loop */
         }
         if (swap) op = flop( op );	/* swap bytes */
         switch( op.opcode ) {		/* which server function */
            case OP_CON: {		/* connect */
               if (op.status != 1) swap = 1;
               if (slv_con( op )) cont = 0;
               break;
            }
            case OP_DIS: {		/* disconnect */
               cont = 0;		/* always quit */
               break;
            }
            case OP_GET: {		/* get file from server */
               if (slv_get( op )) cont = 0;
               break;
            }
            case OP_PUT: {		/* send file to server */
               if (slv_put( op )) cont = 0;
               break;
            }
            default: {			/* unknown function */
               op.status = -1;
               if (swap) op = flop( op );
               if (put( slv_sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               }
               break;
            }
         }
      } else {
         if (nfd < 0) {
            cont = 0;			/* quit */
         }
      }
   } while (cont);			/* until we stop */
   if (slv_sock != -1) {
      close( slv_sock );		/* close socket */
      slv_sock = -1;
   }
   return( EXIT_SUCCESS );
}

/*
 * Main programme.
 */

int	main( int argc, char *argv[] )
{
   char			filename[FILENAME_MAX];	/* message file name */
   int			bind( );
   int			close( );
   int			fork( );
   int			getsockname( );
   int			length;
   int			listen( );
   int			listensock;
   int			r;
   int			select( );
   int			socket( );
   short		port = 0;
   struct servent	*sp = NULL;
   struct sockaddr_in	server;

   strcpy( programme, argv[0] );		/* get name of programme */
   gip_root = getenv( "gip_root" );
   if (gip_root == NULL) {
      fprintf( stderr, "srcserver: gip_root not defined\n" );
      return( EXIT_FAILURE );
   }
   sprintf( filename, "%s/adm/srvmessage", gip_root );
   sp = getservbyname( "GIPSYRSS", "tcp" );
   if (sp == NULL) {
      fprintf( stderr, "srcserver: tcp/GIPSYRSS unknown service\n" );
#ifdef	GIPSYRSS
      port = GIPSYRSS;				/* the port */
#else
      port = 2841;				/* the port */
#endif
   } else {
      port = htons( sp->s_port );
   }
   listensock = socket( AF_INET, SOCK_STREAM, 0 );
   if (listensock < 0) {
      fprintf( stderr, "srcserver: cannot create socket\n" );
      return( EXIT_FAILURE );
   }
   server.sin_family      = AF_INET;
   server.sin_addr.s_addr = INADDR_ANY;
   server.sin_port        = htons( port );
   length = sizeof( server );
   r = bind( listensock, (void *) &server, length );
   if (r == -1) {
      if (errno != EADDRINUSE) {
         fprintf( stderr, "srcserver: cannot bind to socket\n" );
      }
      return( EXIT_FAILURE );
   }
   r = getsockname( listensock, (void *) &server, &length );
   if (r == -1) {
      fprintf( stderr, "srcserver: getsockname error\n" );
      return( EXIT_FAILURE );
   }
   fclose( stdin );			/* close 0 */
   fclose( stdout );			/* close 1 */
   fclose( stderr );			/* close 2 */
   if (!(r = fork( ))) {
      signal( SIGCHLD, reaper );
      r = listen( listensock, 5 );
      if (r == -1) {
         return( EXIT_FAILURE );
      }
      do {
         FILE	*f;			/* for messages */
         fd_set	read_fds;
         int	nfd;
         struct timeval	to;		/* the timer struct */

         to.tv_sec = 300;		/* wait 5 minutes */
         to.tv_usec = 0;		/* and zero micro seconds */
         FD_ZERO( &read_fds );
         FD_SET( listensock, &read_fds );
         while ((nfd = select( listensock + 1, (void *) &read_fds, NULL, NULL, &to )) == -1 && errno == EINTR);
         if (nfd != -1) {
            f = fopen( filename, "r" );
            if (f == NULL) {
               srvmessage[0] = 0;	/* no message */
            } else {
               if (fgets( srvmessage, STRINGLEN, f ) == NULL) {
                  srvmessage[0] = 0;
               }
               fclose( f );
            }
            if (FD_ISSET( listensock, &read_fds )) {
               int	accept( );
               int	msgsock;

               msgsock = accept( listensock, NULL, NULL );
               if (msgsock != -1) {
                  int	pid;

                  if (!(pid = fork())) {
                     close( listensock );
                     slv_sock = msgsock;
                     return( dosockop( ) );
                  } else {
                     close( msgsock );
                  }
               }
            }
         }
      } while (1);
      return( EXIT_FAILURE );
   } else if (r == -1) {
      return( EXIT_FAILURE );
   } else {
      return( EXIT_SUCCESS );
   }
}

#else

#define	GFTP_GET		1
#define	GFTP_PUT		2
#define	GFTP_REP		3
#define	GFTP_REG		4
#define	GFTP_COM		5
#define	GFTP_TST		6

int	main( int argc, char *argv[] )
{
   FILE	*f;
   char	client[STRINGLEN];
   char	filename1[FILENAME_MAX];
   char	filename2[FILENAME_MAX];
   char	sfilename[FILENAME_MAX];
   char	srv_mail[STRINGLEN];
   char	srv_name[STRINGLEN];
   char	srv_path[STRINGLEN];
   int	info = 0;
   int	i;
   int	r;
   int	status = EXIT_SUCCESS;

   mode = GFTP_REG;				/* default mode */
   strcpy( programme, argv[0] );		/* get name of programme */
   gip_root = getenv( "gip_root" );
   if (gip_root == NULL) {
      fprintf( stdout, "%s -- gip_root not defined\n", argv[0] );
      return( EXIT_FAILURE );
   }
   name_of_host( client, STRINGLEN );
   for ( i = 1; i < argc; i++ ) {
      if (!strcmp( argv[i], "get" )) {
         mode = GFTP_GET;
         if (argc == 4) {
            strcpy( filename1, argv[++i] );
            strcpy( filename2, argv[++i] );
         } else {
            return( EXIT_FAILURE );
         }
      } else if (!strcmp( argv[i], "put" )) {
         mode = GFTP_PUT;
         if (argc == 3) {
            strcpy( filename1, argv[++i] );
         } else {
            return( EXIT_FAILURE );
         }
      } else if (!strcmp( argv[i], "tst" )) {
         mode = GFTP_TST;
         if (argc > 2) {
            return( EXIT_FAILURE );
         }
      } else {
         return( EXIT_FAILURE );
         mode = 0;
      }
   }
   info = 1;
   sprintf( sfilename, "%s/loc/server", gip_root );
   f = fopen( sfilename, "r" );
   if ( f == NULL ) {
      info = 2;
      sprintf( sfilename, "%s/sys/server.mgr", gip_root );
      f = fopen( sfilename, "r" );
   }
   if ( f != NULL ) {
      if (xscanf( f, "%s %s %s", srv_name, srv_mail, srv_path ) != 3) {
         fclose( f );
         f = NULL;
      }
   }
   if ( f == NULL ) {
      info = 3;
      strcpy( srv_name, SRV_NAME );
      strcpy( srv_mail, SRV_MAIL );
      strcpy( srv_path, SRV_PATH );
   }
   switch( mode ) {
      case GFTP_GET: {
         if (!( r = srv_con( srv_name ) )) {
            if ((r = srv_get( filename1, filename2 ))) {
               status = EXIT_FAILURE;
            }
            r = srv_dis( );
         }
         break;
      }
      case GFTP_PUT: {
         if (!( r = srv_con( srv_name ) )) {
            if ((r = srv_put( filename1 ))) {
               status = EXIT_FAILURE;
            }
            r = srv_dis( );
         }
         break;
      }
      case GFTP_REG: {
         switch( r = srv_con( srv_name ) ) {
            case  0: {
               fprintf( stdout, "gftp: %s registered as client #%d\n", client, regnum );
               break;
            }
            case -1: {
               fprintf( stdout, "gftp: could not create INET socket\n" );
               break;
            }
            case -2: {
               fprintf( stdout, "gftp: could not obtain INET address for %s\n", srv_name );
               break;
            }
            case -3: {
               fprintf( stdout, "gftp: could not connect to srcserver\n" );
               break;
            }
            case -4:
            case -5: {
               fprintf( stdout, "gftp: could not send to srcserver\n" );
               break;
            }
            case -6: {
               fprintf( stdout, "gftp: could not receive from srcserver\n" );
               break;
            }
            case -7: {
               fprintf( stdout, "gftp: could not receive message from srcserver\n" );
               break;
            }
            case -8: {
               fprintf( stdout, "gftp: %s", srvmessage );
               break;
            }
            default: {
               fprintf( stdout, "gftp: unknown error\n" );
               break;
            }
         }
         if (r) status = EXIT_FAILURE;
         r = srv_dis( );
         break;
      }
      case GFTP_TST: {
         r = srv_con( srv_name );
         if (r == -2) {
            r = srv_dis( );
            strcpy( srv_name, SRV_NAME );
            strcpy( srv_mail, SRV_MAIL );
            strcpy( srv_path, SRV_PATH );
            if ( !(r = srv_con( srv_name )) ) {
               f = fopen( "server.new", "w" );
               if ( f != NULL ) {
                  fprintf( f, "%s:%s:%s\n", srv_name, srv_mail, srv_path );
                  fclose( f );
                  fprintf( stdout, "gftp: server.new created\n" );
               }
            }
         }
         if (r) {
            fprintf( stdout, "gftp: cannot connect to srcserver\n" );
            status = EXIT_FAILURE;
         } else {
            fprintf( stdout, "gftp: %s registered as GIPSY client #%d\n", client, regnum );
         }
         r = srv_dis( );
         break;
      }
      default: {
         status = EXIT_FAILURE;
         break;
      }
   }
   return( status );  
}

#endif
