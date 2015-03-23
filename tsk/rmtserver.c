/* rmtserver.c

        Copyright (c) Kapteyn Laboratorium Groningen 1991
        All Rights Reserved.

#>            rmtserver.doc

Program:      rmtserver

Purpose:      Allows access to remote tape units.

Category:     TAPES

File:         rmtserver.c

Author:       K.G. Begeman

Use:          rmtserver can be used in two modes: the server mode
              and the local mode. In server mode rmtserver acts as
              a daemon and has to be started on the hosts which have
              tape devices attached. It has to be started only once
              with the following command line:

              rmtserver -daemon <directory>

              The directory is the directory where the file mtdevices
              resides (normaly $gip_loc). rmtserver will examine this
              file whether there are any tape devices associated with
              the current host (field #3), and if so it will run in
              daemon mode.
              rmtserver can best be started by crontab every hour. You
              can use the rmtserver.csh script (in $gip_sys) for this.
              rmtserver can also be started via the /etc/rc.local or
              /etc/localrc scripts. You have to ask the system mananger
              to add the following lines:

              # Start remote tape server
              if [ -f $gip_exe/rmtserver ]; then
                      $gip_exe/rmtserver -daemon $gip_loc
                      echo -n ' GIPSY Remote Tape Server'
              fi

              Note: the $gip_exe and the $gip_loc symbols will probably
              be undefined, so you will have to replace them with the
              real path names.

              The internet portnumber should be specified in field #7
              of the $gip_loc/mtdevices setup file, or the system
              manager must also edit the /etc/services file
              to add the port for the rmtserver in the following way:

              # GIPSY functions
              GIPSYRTS        3198/tcp        # GIPSY Remote Tape Service

              It does not matter which portnumber you specify (here 3198),
              as long as all hosts which run GIPSY and which have tape
              devices use the same portnumber.

              The other mode of operation of rmtserver is the local mode.
              In this mode rmtserver is started by the mtopen routine.
              It can only work properly if the user has access to
              the remote host. On the remote host the local host must be
              added to the .rhosts file in the users home directory. Also
              the .cshrc on the remote system must properly setup the
              GIPSY environment.

Updates:      Oct 11, 1991: KGB Document created.

#<

#>            rmtserver.csh
#!/bin/csh
#
# Script for starting the rmtserver via cron.
#
# crontab entry: 0 * * * * /bin/csh <gip_sys>/rmtserver.csh
#
if ( $0 != '' ) then					# command line argv[0]
	set tmp = $0					# command line
	if ( -d ${tmp:h} ) then 			# directory ?
		set mywd = `\pwd`			# cwd
		cd ${tmp:h}				# change wd
		set tmp = `\pwd`			# get wd
		cd ${mywd}				# back
		unset mywd				# unset
	else						# else
		set tmp = `\pwd`			# cwd
	endif						# }
	set tmp = ${tmp:h}				# path to gip_root
	if ( -e ${tmp}/sys/cshrc.csh ) then		# present
		setenv gip_root ${tmp}			# make gip_root
		source ${gip_root}/sys/cshrc.csh	# execute
	endif						# }
	unset tmp					# unset
endif							# }
if ( ${?gip_root} && -x ${gip_exe}/rmtserver && -r ${gip_loc}/mtdevices ) then
	\rm -f ${gip_loc}/rmtserver.`hostname`
	${gip_exe}/rmtserver -daemon ${gip_loc} >& ${gip_loc}/rmtserver.`hostname`
else
	mail `whoami` << EOF
The Remote Tape Server (rmtserver) was not started on `hostname`

                                rmtserver.csh
EOF
endif
#<

*/

#include        "errno.h"                       /* <errno.h> */
#include        "limits.h"                      /* <limits.h> */
#include	"signal.h"			/* <signal.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */

#define	STRINGLEN	250			/* length of strings */

#define	WNOHANG		1			/* from <sys/wait.h> */

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

#include	"gipsyc.h"			/* GIPSY symbols */
#include	"cmain.h"			/* C programme */
#include	"xscanf.h"			/* read devces file */

extern	fint	mtopen_c( fchar );			/* open tape device */
extern	fint	mtclose_c( fint * );			/* close tape device */
extern	fint	mtfsr_c( fint *, fint * );		/* forward skip record */
extern	fint	mtbsr_c( fint *, fint * );		/* backward skip record */
extern	fint	mtfsf_c( fint *, fint * );		/* forward skip file */
extern	fint	mtbsf_c( fint *, fint * );		/* backward skip file */
extern	fint	mtrew_c( fint * );			/* rewind tape */
extern	fint	mtstat_c( fint * );			/* get tape status */
extern	fint	mtweof_c( fint *, fint * );		/* write tape marks */
extern	fint	mtread_c( fint *, char *, fint * );	/* read a record */
extern	fint	mtwrite_c( fint *, char *, fint * );	/* write a record */

#define	OP_OPN		0		/* open tape device */
#define	OP_CLO		1		/* close tape device */
#define	OP_FSR		2		/* forward skip record */
#define	OP_BSR		3		/* backward skip record */
#define	OP_FSF		4		/* forward skip file */
#define	OP_BSF		5		/* backward skip file */
#define	OP_REW		6		/* rewind tape */
#define	OP_STA		7		/* get tape status */
#define	OP_WTM		8		/* write tape marks */
#define	OP_GET		9		/* read record */
#define	OP_PUT		10		/* write record */

typedef struct {			/* the op code struct */
   int	counts;				/* counter */
   int	opcode;				/* function */
   int	status;				/* status */
} op_struct;				/* the struct */


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
 * dosockop( ) does the socket communication and tape io.
 */

static	int	dosockop( int sock )
{
   fd_set	read_fds;		/* list of fd's */
   fint		mtid = -1;		/* tape id */
   int		close( );
   int		cont = 1;		/* continue */
   int		swap = 0;		/* to swap or not to swap */
   op_struct	op;			/* mag tape op struct */

   do {					/* main action loop */
      int		nfd;		/* number of fd's */
      int		select( );	/* select */
      struct timeval	to;		/* timer struct */

      to.tv_sec = 3600;			/* wait one hour */
      to.tv_usec = 0;			/* and zero micro seconds */
      FD_ZERO( &read_fds );		/* clear */
      FD_SET( sock, &read_fds );	/* set for this socket */
      while ((nfd = select( sock + 1, (void *) &read_fds, NULL, NULL, &to )) == -1 && errno == EINTR);
      if (nfd > 0 && FD_ISSET( sock, &read_fds )) {	/* this socket */
         if (get( sock, &op, sizeof( op_struct ) )) {
            cont = 0;			/* don't continue */
            break;			/* leave loop */
         }
         if (swap) op = flop( op );	/* swap bytes */
         switch( op.opcode ) {		/* which tape function */
            case OP_OPN: {		/* open tape unit */
               char	*tapeunit;	/* name of device */

               if (op.status != 1) swap = 1;
               if (swap) op = flop( op );
               tapeunit = malloc( op.counts );
               if (get( sock, tapeunit, op.counts )) {
                  op.status = -1;
                  cont = 0;
               } else if (mtid != -1) {
                  op.status = -1;
               } else {
                  op.status = mtid = mtopen_c( tofchar( tapeunit ) );
               }
               free( tapeunit );
               if (mtid < 0) cont = 0;
               if (swap) op = flop( op );
               if (put( sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               }
               break;
            }
            case OP_CLO: {		/* close tape unit */
               cont = 0;		/* always quit */
               op.status = mtclose_c( &mtid );
               mtid = -1;
               if (swap) op = flop( op );
               put( sock, &op, sizeof( op_struct ) );
               break;
            }
            case OP_FSR: {		/* forward skip record */
               fint	nfsr = op.counts;

               op.status = mtfsr_c( &mtid, &nfsr );
               if (swap) op = flop( op );
               if (put( sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               }
               break;
            }
            case OP_BSR: {		/* backward skip record */
               fint	nbsr = op.counts;

               op.status = mtbsr_c( &mtid, &nbsr );
               if (swap) op = flop( op );
               if (put( sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               }
               break;
            }
            case OP_FSF: {		/* forward skip file */
               fint	nfsf = op.counts;

               op.status = mtfsf_c( &mtid, &nfsf );
               if (swap) op = flop( op );
               if (put( sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               }
               break;
            }
            case OP_BSF: {		/* backward skip file */
               fint	nbsf = op.counts;

               op.status = mtbsf_c( &mtid, &nbsf );
               if (swap) op = flop( op );
               if (put( sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               }
               break;
            }
            case OP_REW: {		/* rewind tape */
               op.status = mtrew_c( &mtid );
               if (swap) op = flop( op );
               if (put( sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               }
               break;
            }
            case OP_STA: {		/* return status */
               op.status = mtstat_c( &mtid );
               if (swap) op = flop( op );
               if (put( sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               }
               break;
            }
            case OP_WTM: {		/* write tape marks */
               fint	neof = op.counts;

               op.status = mtweof_c( &mtid, &neof );
               if (swap) op = flop( op );
               if (put( sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               }
               break;
            }
            case OP_GET: {		/* read from tape */
               char	*buf;
               fint	nget = op.counts;
               fint	nput;

               buf = malloc( nget );
               nput = op.status = mtread_c( &mtid, buf, &nget );
               if (nput > nget) nput = nget;
               op.counts = nput;
               if (swap) op = flop( op );
               if (put( sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               } else if (nput > 0) {
                  if (put( sock, buf, nput )) {
                     cont = 0;
                  }
               }
               free( buf );
               break;
            }
            case OP_PUT: {		/* write to tape */
               char	*buf;
               fint	nput = op.counts;

               buf = malloc( nput );
               if (  get( sock, buf, nput )) {
                  cont = 0;
               } else {
                  op.status = mtwrite_c( &mtid, buf, &nput );
                  if (swap) op = flop( op );
                  if (put( sock, &op, sizeof( op_struct ) )) {
                     cont = 0;
                  }
               }
               free( buf );
               break;
            }
            default: {			/* unknown function */
               op.status = -1;
               if (swap) op = flop( op );
               if (put( sock, &op, sizeof( op_struct ) )) {
                  cont = 0;
               }
               break;
            }
         }
      } else {
         if (nfd < 0) cont = 0;		/* quit */
      }
   } while (cont);			/* until we stop */
   if (mtid != -1) {			/* if still open, close it */
      mtclose_c( &mtid );
   }
   if (sock > -1) close( sock );	/* close socket */
   return( EXIT_SUCCESS );
}


/*
 * doserver1( ) starts a local server for one user only. The server
 * executes on the remote host.
 */

static	int	doserver1( char *portnumber, char *parenthost )
{
   int	fork( );			/* fork a process */
   int	r;				/* return value */

   signal( SIGCHLD, SIG_IGN );		/* ignore signals from children */
   fclose( stdin );			/* close 0 */
   fclose( stdout );			/* close 1 */
   fclose( stderr );			/* close 2 */
   if (!(r = fork())) {			/* child goes on */
      int			connect( );
      int			sock = -1;
      int			socket( );
      short			port;
      struct hostent		*hp;
      struct hostent		*gethostbyname( );
      struct sockaddr_in	server;

      hp = gethostbyname( parenthost );
      sock = socket( AF_INET, SOCK_STREAM, 0 );
      memmove( (void *) &server.sin_addr, (void *) hp->h_addr, hp->h_length );
      port = atoi( portnumber );
      server.sin_family = AF_INET;
      server.sin_port = htons( port );
      r = connect( sock, (void *) &server, sizeof( server ));
      if (r != -1) return( dosockop( sock ) );
   } else if (r != -1) {
      r = 0;
   }
   if (r) {
      return( EXIT_FAILURE );
   } else {
      return( EXIT_SUCCESS );
   }
}


/*
 * doserver2( ) starts the real remote tape server.
 */

static	int	doserver2( char *gip_loc )
{
   FILE			*file;
   char			f[7][STRINGLEN];
   char			filename[FILENAME_MAX+1];
   char			hostname[64];
   int			bind( );
   int			close( );
   int			fork( );
   int			gethostname( );
   int			getsockname( );
   int			len;
   int			length;
   int			listen( );
   int			listensock;
   int			nf;
   int			nhosts = 0;
   int			r;
   int			select( );
   int			socket( );
   short		port = 0;
   struct servent	*sp = NULL;
   struct sockaddr_in	server;

   gethostname( hostname, 64 );
   len = strlen( hostname );
   sprintf( filename, "%s/mtdevices", gip_loc );
   file = fopen( filename, "r" );
   if (file == NULL) {
      fprintf( stderr, "rmtserver: %s not found\n", filename );
      return( EXIT_FAILURE );
   }
   while ( ( nf = xscanf( file, "%s %s %s %s %s %s %s", f[0], f[1], f[2], f[3], f[4], f[5], f[6] )) >= 6 ) {
      if ( strstr( f[2], hostname ) == f[2] ) {
         if ( ( f[2][len] == '.' ) || ( f[2][len] == '\0' ) ) {
            nhosts++;
            if ( nf == 7  && !port ) port = atoi( f[6] );
         }
      }
   }
   fclose( file );
   if (nhosts == 0) {
      fprintf( stderr, "rmtserver: no magnetic tape devices for host %s\n", hostname );
      return( EXIT_FAILURE );
   }
   if ( !port ) {
      sp = getservbyname( "GIPSYRTS", "tcp" );
      if (sp == NULL) {
         fprintf( stderr, "rmtserver: tcp/GIPSYRTS unknown service\n" );
         return( EXIT_FAILURE );
      }
      port = htons( sp->s_port );
   }
   listensock = socket( AF_INET, SOCK_STREAM, 0 );
   if (listensock < 0) {
      fprintf( stderr, "rmtserver: cannot create socket\n" );
      return( EXIT_FAILURE );
   }
   server.sin_family      = AF_INET;
   server.sin_addr.s_addr = INADDR_ANY;
   server.sin_port        = htons( port );
   length = sizeof( server );
   r = bind( listensock, (void *) &server, length );
   if (r == -1) {
      if (errno == EADDRINUSE) {
         fprintf( stderr, "rmtserver: already active at port %d\n", port );
      } else {
         fprintf( stderr, "rmtserver: cannot bind to socket\n" );
      }
      return( EXIT_FAILURE );
   }
   r = getsockname( listensock, (void *) &server, &length );
   if (r == -1) {
      fprintf( stderr, "rmtserver: getsockname error\n" );
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
         fd_set	read_fds;
         int	nfd;

         FD_ZERO( &read_fds );
         FD_SET( listensock, &read_fds );
         while ((nfd = select( listensock + 1, (void *) &read_fds, NULL, NULL, NULL )) == -1 && errno == EINTR);
         if (nfd != -1) {
            if (FD_ISSET( listensock, &read_fds )) {
               int	accept( );
               int	msgsock;

               msgsock = accept( listensock, NULL, NULL );
               if (msgsock != -1) {
                  int	pid;

                  if (!(pid = fork())) {
                     close( listensock );
                     return( dosockop( msgsock ) );
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


/*
 * Main programme. Needs arguments from initiator.
 */

int	cmain( int argc, char **argv )
{
   char *gip_loc = NULL;   		/* gip_loc directory */
   char	*parenthost = NULL;		/* host which initiales tape requests */
   char	*portnumber = NULL;		/* inet port number */
   int	iarg = 1;			/* argument counter */

   while (iarg < argc) {		/* check for commanline arguments */
      if (!strcmp( argv[iarg], "-inetportnumber" )) {
         portnumber = argv[++iarg];	/* the port number */
      } else if (!strcmp( argv[iarg], "-parenthost" )) {
         parenthost = argv[++iarg];	/* the initiator host */
      } else if (!strcmp( argv[iarg], "-daemon" )) {
         gip_loc = argv[++iarg];	/* the gip_loc directory */
      }
      iarg++;				/* increase */
   }
   if (portnumber && parenthost) {	/* we are a local server */
      return( doserver1( portnumber, parenthost ) );
   } else if (gip_loc) {		/* we are a real server */
      return( doserver2( gip_loc ) );
   } else {				/* ? */
      return( EXIT_FAILURE );		/* we have a problem */
   }
}
