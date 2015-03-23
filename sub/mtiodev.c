/* mtiodev.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            mtiodev.dc2

Document:     MTIODEV

Purpose:      Describes the tape manipulation routines and setup.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Description:  The mtiodev routines can read/write data from/to tapes
              and files. The structure of a 'disktape' is as follows:
              The name of a disktape is the name of a directory. This
              directory contains the data files "file<filenum>.mt" and
              the tape descriptor file "tape.descriptor". The descriptor
              file is a text file which contains a description of the
              structure on a real tape (i.e. information about records and
              file markers), the binary files contain the data on the tape.
              <filenum> indicates the sequence number of the file on the
              disktape. There is a one to one relation between the number
              of tape marks skipped so far and <filenum> (<filenum> =
              number of tape marks plus one). So the data structure on a
              real tape can be completely simulated on disk, as is shown
              below:

              Structure on tape       Structure on disk

                                      tape.descriptor
              BLOCK of fb11 bytes     "@file000001.mt"
                                      "fb11"
              BLOCK of fb12 bytes     "fb12"
              ..... .. .... .....     "...."
              BLOCK of fb1n bytes     "fb1n"
              TAPE MARK               "0"
              BLOCK of fb21 bytes     "@file000002.mt"
                                      "fb21"
              ..... .. .... .....     "...."
              BLOCK of fb2n bytes     "fb2n"
              TAPE MARK               "0"
              ...
              ...
              TAPE MARK               "0"
              TAPE MARK               "0"

              Importing foreign disktape files is very simple. One can
              either create the file "tape.descriptor" with an editor, or
              rename the data files to be imported "file000001.mt",
              "file000002.mt" etc. Whenever a disktape is opened with
              MTOPEN, it will check whether the tape descriptor file exists.
              When it does not exist, MTOPEN will check whether there is
              a file named "file000001.mt". If so, MTOPEN will assume that
              the blocking factor is 2880 bytes (FITS standard) and create
              the tape descriptor file. Then it will check whether a
              file with name "file000002.mt" exists etc.

              A tape device, be it a tapeunit or a disk file, is opened
              with function MTOPEN. MTOPEN has one argument, a character
              string denoting the name of the device. If the name corresponds
              with the name of a tape unit in $gip_loc/mtdevices (not
              case sensitive), this unit will be used, otherwize it is assumed
              to be a directory on disk. The argument to MTOPEN may also be
              a question mark (?), optionally followed by a keyword. This
              means that MTOPEN will prompt the user for a tape device
              (with keyword MTDEVICE= or the keyword specified after the
              question mark), and list the available tape units (if the
              user wants it). The syntax of $gip_loc/mtdevices is described
              in $gip_doc/mtdevices.doc.

              After a succesful call to MTOPEN, the returned TAPEID is
              used to denote this tape device in further calls to MTIODEV
              routines.

              The following tape manipulation functions are available:

              MTOPEN   Opens a tape device.
              MTCLOSE  Closes an opened tape device.
              MTREW    Rewinds an opened tape device.
              MTSTAT   Returns status of an opened tape device.
              MTREAD   Reads a block from an opened tape device.
              MTWRITE  Writes a block to an opened device.
              MTFSF    Forward spaces a tape mark on an opened device.
              MTBSF    Backward spaces a tape mark on an opened device.
              MTFSR    Forward spaces a block on an opened device
              MTBSR    Backward spaces a block on an opened device
              MTWEOF   Writes tape mark to an opened tape device.
              MTNAME   Returns name (synonym) of tape device.

              Each routine is described in more detail in the
              appropriate document.

Notes:        For disktapes, files containing the real data may have any
              legal name (excluding <cr> and space). The correct filename
              should then be inserted in the tape.descriptor file, preceded
              by a @.

Related Docs: rmtserver.dc1

Updates:      Mar 27, 1990: KGB Document created.
              Oct 31, 1990: KGB Remote tapeserver implemented.
              Jun 24, 1991: PRR Implemented use of read-only disk-tapes.
              Oct 15, 1991: KGB Better implementation of mtopen.
              Dec 11, 1991: KGB Call to cancel replaced by call to reject.
              Jun 13, 1997: KGB Allowed non BSD compatible devices on solaris.
              Jan 22, 1998: KGB Implemented DLT on hpux.
              Nov  5, 1999: JPT Support generic DAT for Solaris.
              Feb 23, 2001: KGB Support DLT for Solaris.
              Jun 22, 2010: JPT Fix to prevent compilation failure on Apple.

#<

*/

#include	"ctype.h"	/* <ctype.h> */
#include	"stdio.h"	/* <stdio.h> */
#include	"string.h"	/* <string.h> */
#include	"stdlib.h"	/* <stdlib.h> */
#include	"errno.h"	/* <errno.h> */
#include	"ctype.h"	/* <ctype.h> */
#include	"xscanf.h"	/* include xcanf code */

#define	MAXDEVNAMLEN	80	/* maximum length of device name */
#define MAXRECORDLEN	32768	/* maximum length of record */

#if	defined(__vms__)	/* VAX includes */

#include	<descrip.h>	/* VMS descriptor macro         */
#include	<iodef.h>	/* VMS QIO function codes       */
#include	<ssdef.h>	/* VMS status code definitions  */
#include	<dvidef.h>	/* VMS dvi defines */
#include	<devdef.h>	/* VMS device defines */
#include	<mtdef.h>	/* VMS mt defines */

#elif defined(__unix__)

#define	ioctl	IOCTL
#define	mkdir	MKDIR
#define	open	OPEN

#if	defined(__decstation__)	/* decstations do it differently */
#include	<sys/devio.h>
#endif
#if	defined(__convex__)
#define	_SYS_TERMIOS_H_
#endif
#include	<sys/file.h>	/* file access definitions */
#include	<sys/types.h>	/* define some types */
#include	<sys/socket.h>	/* the socket things */
#include	<sys/stat.h>	/* status definitions */
#include	<sys/time.h>	/* timeval definitions */
#include	<sys/ioctl.h>	/* io operations */

#if	defined(__aix__)

#include	<sys/select.h>
#include	<sys/tape.h>

#else

#if defined(__APPLE__)                  /* For Apple, who removed mtio.h */

/*
 * This code does not attempt to implement magtape functionality for Apple
 * but only allows this source to be compiled without errors.
 */

struct mtop {
	short	mt_op;		/* operations defined below */
	daddr_t	mt_count;	/* how many of them */
};

/* operations */
#define MTWEOF		0	/* write an end-of-file record */
#define MTFSF		1	/* forward space file */
#define MTBSF		2	/* backward space file */
#define MTFSR		3	/* forward space record */
#define MTBSR		4	/* backward space record */
#define MTREW		5	/* rewind */
#define MTOFFL		6	/* rewind and put the drive offline */
#define MTNOP		7	/* no operation, sets status only */
#define MTRETEN		8	/* retension */
#define MTERASE		9	/* erase entire tape */
#define MTEOM		10	/* forward to end of media */
#define MTNBSF		11	/* backward space to beginning of file */
#define MTCACHE		12	/* enable controller cache */
#define MTNOCACHE	13	/* disable controller cache */
#define MTSETBSIZ	14	/* set block size; 0 for variable */
#define MTSETDNSTY	15	/* set density code for current mode */

/* structure for MTIOCGET - mag tape get status command */
/* LP64todo - not 64-bit safe */
struct mtget {
	short	mt_type;	/* type of magtape device */
/* the following two registers are grossly device dependent */
	u_short	mt_dsreg;	/* ``drive status'' register. SCSI sense byte 0x02.  */
	u_short	mt_erreg;	/* ``error'' register. SCSI sense byte 0x0C. */
	u_short mt_ext_err0;	/* SCSI sense bytes 0x13..0x14 */
	u_short mt_ext_err1;	/* SCSI sense bytes 0x15..0x16 */
/* end device-dependent registers */
	short	mt_resid;	/* residual count */
/* the following two are not yet implemented */
	daddr_t	mt_fileno;	/* file number of current position */
	daddr_t	mt_blkno;	/* block number of current position */
/* end not yet implemented */
	daddr_t	mt_blksiz;	/* current block size */
	daddr_t	mt_density;	/* current density code */
	daddr_t	mt_mblksiz[4];	/* block size for different modes */
	daddr_t mt_mdensity[4];	/* density codes for different modes */
};

/*
 * Constants for mt_type byte.  These are the same
 * for controllers compatible with the types listed.
 */
#define	MT_ISTS		0x01		/* TS-11 */
#define	MT_ISHT		0x02		/* TM03 Massbus: TE16, TU45, TU77 */
#define	MT_ISTM		0x03		/* TM11/TE10 Unibus */
#define	MT_ISMT		0x04		/* TM78/TU78 Massbus */
#define	MT_ISUT		0x05		/* SI TU-45 emulation on Unibus */
#define	MT_ISCPC	0x06		/* SUN */
#define	MT_ISAR		0x07		/* SUN */
#define	MT_ISTMSCP	0x08		/* DEC TMSCP protocol (TU81, TK50) */
#define MT_ISCY		0x09		/* CCI Cipher */
#define MT_ISCT		0x0a		/* HP 1/4 tape */
#define MT_ISFHP	0x0b		/* HP 7980 1/2 tape */
#define MT_ISEXABYTE	0x0c		/* Exabyte */
#define MT_ISEXA8200	0x0c		/* Exabyte EXB-8200 */
#define MT_ISEXA8500	0x0d		/* Exabyte EXB-8500 */
#define MT_ISVIPER1	0x0e		/* Archive Viper-150 */
#define MT_ISPYTHON	0x0f		/* Archive Python (DAT) */
#define MT_ISHPDAT	0x10		/* HP 35450A DAT drive */
#define MT_ISWANGTEK	0x11		/* WANGTEK 5150ES */
#define MT_ISCALIPER	0x12		/* Caliper CP150 */
#define MT_ISWTEK5099	0x13		/* WANGTEK 5099ES */
#define MT_ISVIPER2525	0x14		/* Archive Viper 2525 */
#define MT_ISMFOUR	0x11		/* M4 Data 1/2 9track drive */
#define MT_ISTK50	0x12		/* DEC SCSI TK50 */
#define MT_ISMT02	0x13		/* Emulex MT02 SCSI tape controller */
#define MT_ISGS		0x14		/* Generic SCSI Tape */

/* mag tape io control commands */
#define	MTIOCTOP	_IOW('m', 1, struct mtop)	/* do a mag tape op */
#define	MTIOCGET	_IOR('m', 2, struct mtget)	/* get tape status */
#define MTIOCIEOT	_IO('m', 3)			/* ignore EOT error */
#define MTIOCEEOT	_IO('m', 4)			/* enable EOT error */

#define	DEFTAPE	"/dev/rst0"


#else                                              /* for normal systems */

#include	<sys/mtio.h>	/* tape operations */

#endif

#if !defined(MT_ISXY) /* Missing in SunOS 5.7 */
#define MT_ISXY         0x09            /* sun: Xylogics 472 */
#endif

#endif

#include	<sys/un.h>	/* the unix things */
#include	<netinet/in.h>	/* inet things */
#include	<netdb.h>	/* net operations */
#if	defined(__sysv__)
#include	<sys/utsname.h>
#endif
#if	defined(__sun__) & defined(__svr4__)
#include	<sys/mkdev.h>
#endif
#if	!defined(O_RDONLY)
#include	<fcntl.h>
#endif

#undef	ioctl
#undef	mkdir
#undef	open

#if	defined(__STDC__)
/*
 * The following redefines are necessary when compiling with an ANSI C
 * compiler. Note that this copies part of the defines in mtio.h and
 * ioctl.h.
 */

#if	defined(__bsd__)
#if	defined(__alliant__) | defined(__sun__)
#ifndef	IOC_OUT
#define	IOC_OUT	_IOC_OUT
#endif
#ifndef	IOC_IN
#define	IOC_IN	_IOC_IN
#endif
#ifndef	IOCPARM_MASK
#define	IOCPARM_MASK	_IOCPARM_MASK
#endif
#if	defined(_IOR)		/* needs to be redefined */
#undef	_IOR
#define	_IOR(x,y,t)	(IOC_OUT|((sizeof(t)&IOCPARM_MASK)<<16)|(x<<8)|y)
#endif
#if	defined(_IOW)		/* needs to be redefined */
#undef	_IOW
#define	_IOW(x,y,t)	(IOC_IN|((sizeof(t)&IOCPARM_MASK)<<16)|(x<<8)|y)
#endif
#if	defined(MTIOCTOP)	/* needs to be redefined */
#undef	MTIOCTOP
#define	MTIOCTOP	_IOW('m',1,struct mtop)
#endif
#if	defined(MTIOCGET)	/* needs to be redefined */
#undef	MTIOCGET
#define	MTIOCGET	_IOR('m',2,struct mtget)
#endif
#if	defined(MTIOCVSR)	/* needs to be redefined */
#undef	MTIOCVSR
#define	MTIOCVSR	_IOR('m',127,struct mtvsr)
#endif
#endif
#endif
#endif

#if	defined(__sun__)        /* ?? */

#define M_ST_MASK  077          /* Status code mask: ?? */
#define M_ST_SUCC  000          /* Command Successed: ?? */
#define M_ST_DATA  010          /* Data Error: ?? */
#define M_ST_TAPEM 022          /* Tape Mark Encountared: ?? */
#define M_ST_BOT   077          /* Beginning of Tape: ?? */
#define M_ST_SEX   077          /* Serious Exception: ?? */
#define M_ST_RDTRN 043          /* Record Data Truncated: ?? */

#endif

#endif

#define legal(x) ((x >= 0) && (x < mt_devs) ? 1 : 0)

#define MTIO_NO_ERROR          0	/* no error */
#define MTIO_OP_ERROR         -1	/* operation error */
#define MTIO_END_OF_TAPE      -2	/* end of tape detected */
#define MTIO_ALREADY_OPEN     -3	/* device already opened */
#define MTIO_NOT_OPENED       -4	/* device not opened */
#define MTIO_TAPE_MARK        -5	/* tape mark encountered */
#define MTIO_NOT_IMPLEMENTED  -6	/* not available */
#define MTIO_BEGIN_OF_TAPE    -7	/* tape at BOT */
#define MTIO_REC_TRUNCATE     -8	/* record too long */
#define MTIO_CALL_ERROR       -9	/* error in call */
#define	MTIO_WRITE_LOCK	     -10	/* write protected */

#if	defined(__unix__)		/* UNIX */

#if	!defined(ntohs) & !defined(__alpha__) & !defined(__linux__)
extern	u_short	ntohs( );
#endif
#if     !defined(htons) & !defined(__alpha__) & !defined(__linux__)
extern  u_short htons( );
#endif

typedef struct {			/* the remore options struct */
   int	counts;				/* counter */
   int	opcode;				/* the tape function */
   int	status;				/* the status */
} op_struct;				/* the struct */

#define	OP_OPN		0		/* open */
#define	OP_CLO		1		/* close */
#define	OP_FSR		2		/* forward skip a record */
#define	OP_BSR		3		/* backward skip record */
#define	OP_FSF		4		/* forward skip file */
#define	OP_BSF		5		/* backward skip file */
#define	OP_REW		6		/* rewind */
#define	OP_STA		7		/* status */
#define	OP_WTM		8		/* write tape mark */
#define	OP_GET		9		/* read */
#define	OP_PUT		10		/* write */

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

#include	"gipsyc.h"	/* GIPSY symbols and definitions */
#include	"error.h"	/* define error_c */
#include	"fsize.h"	/* define fsize_c */
#include	"ftrunc.h"	/* define ftrunc_c */
#include	"nelc.h"	/* define nelc_c */
#include	"userfio.h"

/*
 * For testing purposes we have here some alternatives for
 * HERMES. Note that a ? means no input!
 */

#ifdef	NOHERMES

static	void	anyout_c( fint *ol, fchar text )
{
   fprintf( stdout, "%.*s\n", (int) text.l, text.a );
}

static	void	reject_c( fchar keyword, fchar message )
{
   fprintf( stderr, "%.*s\a\n", (int) message.l, message.a );
}

static	fint	userchar_c( fchar text, fint *nt, fint *ol, fchar key, fchar mes )
{
   char	input[200];
   int	l, m;

   fprintf( stdout, "%.*s\n", (int) mes.l, mes.a );
   fprintf( stdout, "%.*s", (int) key.l, key.a );
   fscanf( stdin, "%s", input );
   l = strlen( input );
   for ( m = 0; m < text.l && m < l; m++ ) {
      text.a[m] = input[m];
   }
   while ( m < text.l ) text.a[m++] = ' ';
   if ( text.a[0] == '?' ) {
      text.a[0] = ' ';
      return( 0 );
   } else {
      return( 1 );
   }
}

#else

#include	"anyout.h"	/* define anyout_c */
#include	"reject.h"	/* define reject_c */
#include	"userchar.h"	/* define userchar_c */

#endif

#define	MAXCOMLEN	80		/* length of comment */
#define	MAXHOSTS	32		/* # of hosts */
#define	MAXHOSTNAMLEN	80		/* length of host name */
#define	MAXSYNNAMLEN	32		/* length of synonym */

#define	KEYWORD		tofchar("MTDEVICE=")
#define	MESSAGE		tofchar("Give Tape Device [list of all devices]")

typedef struct {			/* the device struct */
   char	synonym[MAXSYNNAMLEN+1];	/* synonym name */
   char	remotehost[MAXHOSTNAMLEN+1];	/* remote host name */
   char	devicename[MAXDEVNAMLEN+1];	/* device name */
   char	comment[MAXCOMLEN+1];		/* text */
   int	density;			/* tape density */
   int	remote;				/* remote tape unit */
   int	port;				/* portnumber of rmtserver */
} dev_struct;				/* the struct */

static	dev_struct	*devs = NULL;	/* holds tape device info */
static	int		ndevs = 0;	/* number of devices */

typedef struct {			/* record struct */
   char  *file;				/* name of data file */
   int    size;				/* record length */
} rc_struct;				/* the struct */

typedef struct {			/* tape device struct */
   char		*dev;			/* (translated) name/synonym of device */
   char		*device;		/* real device name */
   char		*devdir;		/* name of disktape */
   char		*host;			/* name of remote host */
   int		remote;			/* remote tape */
   int		dens;			/* tape density (0 denotes disk tape) */
   int		mtid;			/* device id */
   int		port;			/* port number rmtserver */
   int		sock;			/* secondary pipe to remote server */
   int		nfil;			/* current file number */
   int		nrec;			/* current block (record) number */
   int		open;			/* in use? */
   rc_struct	*fptr;			/* pointer to struct block */
   int		nptr;			/* number of records in tape descriptor file */
   int		recc;			/* record count in header file */
   int		bsd;			/* bsd compatible (solaris only) */
   int		readonly;		/* read only device */
   int		stat;			/* device status */
   int		type;			/* type of device */
   FILE		*data;			/* file descriptor data part */
} mt_struct;				/* the struct */

static mt_struct *mt_info = NULL;	/* struct holds tape info */

static int mt_devs = 0;			/* number of devices */


/*
 * inidev reads $gip_loc/mtdevices and stores the info in the device struct.
 */

static	void	inidev( void )
{
   FILE		*f;
   char		*gip_loc;
   char		comment[MAXCOMLEN+1];
   char		dname[MAXDEVNAMLEN+1];
   char		hname[MAXHOSTNAMLEN+1];
   char		sname[MAXSYNNAMLEN+1];
   char		fname[MAXHOSTNAMLEN+1];
   char		lname[MAXHOSTS*MAXHOSTNAMLEN+1];
   char		devf[FILENAME_MAX+1];
   int		dens;
   int		port;
   int		len;
   int		nf;

   if (ndevs) return;
   gip_loc = getenv( "gip_loc" );
   if (gip_loc == NULL) return;
   sprintf( devf, "%s/mtdevices", gip_loc );
   f = fopen( devf, "r" );
   if (f == NULL) return;
   name_of_host( hname, MAXHOSTNAMLEN );
   hname[MAXHOSTNAMLEN] = 0;
   len = strlen( hname );
   while ((nf = xscanf( f, "%s %s %s %s %d %s %d", lname, sname, fname, dname, &dens, comment, &port )) >= 6) {
      char	*hptr;
      int	found = 0;

      if (!strcmp( lname, "*" )) {		/* wild card */
         found = 1;				/* in list */
      } else if ( (hptr = strstr( lname, hname )) != NULL ) {
         found = 1;				/* in list */
         if (hptr != lname) {			/* not first */
            if (hptr[-1] != ',') found = 0;	/* wrong match */
         }
         if (found && hptr[len] != 0 && hptr[len] != '.' && hptr[len] != ',') {
            found = 0;				/* wrong match */
         }
      }
      if (found) {				/* got it */
         int	n;

         n = 0;
         while (sname[n]) { sname[n] = toupper(sname[n]); n++; }
         n = 0;
         while ( n < ndevs && strcmp( devs[n].synonym, sname ) ) n++;
         if ( n == ndevs ) {			/* new mt device */
            devs = realloc( devs, sizeof( dev_struct ) * ( ++ndevs ) );
            if (devs == NULL) {			/* FATAL */
               fint	fatal = 4;
               error_c( &fatal, tofchar( "Memory allocation problems" ) );
            }
         }
         strcpy( devs[n].synonym, sname );	/* synonym */
         strcpy( devs[n].remotehost, fname );	/* remote host */
         strcpy( devs[n].devicename, dname );	/* device name */
         strcpy( devs[n].comment, comment );	/* text */
         devs[n].density = dens;		/* density */
         devs[n].remote = 1;			/* remote */
         if (strstr( fname, hname ) == fname ) {
            if ((fname[len] == '.') || (fname[len] == '\0')) {
               devs[n].remote = 0;		/* local */
            }
         }
         if (nf < 7) port = 0;			/* no port */
         devs[n].port = port;			/* save port */
      }
   }
   fclose( f );
}


#if	defined(__unix__)

/*
 * put puts bytes on a socket. It return 0 on success, -1 on failure.
 */

static	int	put( int socket,  void *data, int ndata )
{
   char	*p = (char *) data;
   int	nd = 0;
   int	nl = ndata;
   int	nt = 0;
   int	r = 0;
   int	write( );

   while (nl) {
      while ((nd = write( socket, &p[nt], nl )) == -1 && errno == EINTR);
      if (nd == -1) { r = -1; break; }
      nl -= nd;
      nt += nd;
   }
   if (r) {
      anyoutf( 3, "put error" );
   }
   return( r );
}

/*
 * get gets bytes of a socket. It returns 0 on success, -1 on failure.
 */

static	int	get( int socket, void *data, int ndata )
{
   char	*p = (char *) data;
   int	nd = 0;
   int	nl = ndata;
   int	nt = 0;
   int	r = 0;
   int	read( );

   while (nl) {
      while ((nd = read( socket, &p[nt], nl )) == -1 && errno == EINTR);
      if (nd == -1) { r = -1; break; }
      nl -= nd;
      nt += nd;
   }
   if (r) {
      anyoutf( 3, "get error" );
   }
   return( r );
}

/*
 * start starts the remote tape server. It returns a connected inet socket
 * on success, -1 on failure.
 */

static	int	start( char *remote_host, int port )
{
   char			command[1024];
   char			local_host[80];
   fd_set		read_fds;
   int			accept( );
   int			bind( );
   int			close( );
   int			connect( );
   int			getsockname( );
   int			listen( );
   int			select( );
   int			socket( );
   int			length;
   int			r = -1;
   int			rstat;
   int			portnumber;
   int			sock1;
   int			sock2;
   struct hostent	*gethostbyname( );
   struct hostent	*hp;
   struct servent	*getservbyname( );
   struct servent	*sp;
   struct sockaddr_in	server1, server2;

   /*
    * first try to find whether there is a daemon present
    */
   sock1 = socket( AF_INET, SOCK_STREAM, 0 );
   if (sock1 == -1) return( r );
   if (port == 0) {
      sp = getservbyname( "GIPSYRTS", "tcp" );
      if (sp != NULL) {
         port = htons( sp->s_port );
      }
   }
   if (port) {
      hp = gethostbyname( remote_host );
      if (hp != NULL) {
         server1.sin_family = AF_INET;
         server1.sin_addr.s_addr = INADDR_ANY;
         server1.sin_port = htons( port );
         memmove( (void *) &server1.sin_addr, (void *) hp->h_addr, hp->h_length );
         if (connect( sock1, (void *) &server1, sizeof( server1 )) != -1) {
            return( sock1 );
         }
      }
   }
   name_of_host( local_host, 80 );
   sock2 = socket( AF_INET, SOCK_STREAM, 0 );
   server2.sin_family = AF_INET;
   server2.sin_addr.s_addr = INADDR_ANY;
   server2.sin_port = 0;
   if (bind( sock2, (void *)&server2, sizeof( server2 ) ) == -1) {
      return( r );
   }
   length = sizeof( server2 );
   if (getsockname( sock2, (void *) &server2, &length )) {
      return( r );
   }
   portnumber = ntohs( server2.sin_port );
   if (listen( sock2, 5 ) == -1) {
      return( r );
   }
#if	defined(__hpux__)
   sprintf( command, "remsh %s -n '$gip_exe/rmtserver -inetportnumber %d -parenthost %s'", remote_host, portnumber, local_host );
#else
   sprintf( command, "rsh   %s -n '$gip_exe/rmtserver -inetportnumber %d -parenthost %s'", remote_host, portnumber, local_host );
#endif
   rstat = system( command );
   if (rstat != EXIT_SUCCESS) return( -1 );
   FD_ZERO( &read_fds );
   FD_SET( sock2, &read_fds );
   do {
      int		nfd;
      struct timeval	to;

      to.tv_sec = 60;
      to.tv_usec = 0;
      while ((nfd = select( sock2 + 1, (void *) &read_fds, NULL, NULL, &to )) == -1 && errno == EINTR);
      if (!nfd) return( -1 );			/* it took to long */
   } while (!FD_ISSET( sock2, &read_fds ));
   r = accept( sock2, NULL, NULL );
   return( r );
}

#endif


#if	defined(__unix__)			/* UNIX */

static fint mtio_status( fint mtid )		/* Gets io status */
{
#if	defined(__aix__)
   return( MTIO_NO_ERROR );
#else
   fint          mt_error = MTIO_NO_ERROR;	/* tape status */
   int           ioctl( );			/* tape io operations */
#if	defined(__decstation__)
   struct devget	devget;

   if (ioctl( mt_info[mtid].mtid, DEVIOCGET, &devget ) == -1) {
      mt_error = MTIO_OP_ERROR;
#else
   struct mtget  mtget;				/* tape status struct */

   if (ioctl( mt_info[mtid].mtid, MTIOCGET, &mtget ) == -1) {
      mt_error = MTIO_OP_ERROR;			/* error */
#endif
   } else {
#if	defined(__alliant__)			/* alliant */
      if (mtget.mt_type == MT_ISXY) {		/* Xylogics 472 */
         switch(mtget.mt_erreg & 0x3f) {	/* select status code */
            case 0x00: {			/* command successed */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x1e: {			/* Tape mark enc. */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x30: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            case 0x1c: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x23: {			/* record too long */
               mt_error = MTIO_REC_TRUNCATE;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown alliant code: erreg = %x", mtget.mt_erreg & 0x3f );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
      } else if (mtget.mt_type == MT_ISXY772) {
         switch(mtget.mt_erreg & 0xff) {	/* select status */
            case 0x00: {			/* command successed */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0xa1: {			/* Tape mark enc. */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0xa0: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            case 0x40: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x47: {			/* record too long */
               mt_error = MTIO_REC_TRUNCATE;
               break;
            }
            case 0x48: {			/* error ? */
               mt_error = MTIO_OP_ERROR;
               break;
            }
            case 0x60: {			/* TAPE EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x90: {			/* write protected */
               mt_error = MTIO_WRITE_LOCK;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown alliant code: erreg = %x", mtget.mt_erreg & 0xff );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
      } else {					/* not implemented */
#if	defined(TESTBED)
         anyoutf( 0, "(MTIODEV) Unknown alliant device %x", mtget.mt_type );
#endif
         mt_error = MTIO_NOT_IMPLEMENTED;
      }
#elif	defined(__convex__)
      if (mtget.mt_type == MT_ISTA) {		/* 9-track tape */
         if (mtget.mt_dsreg & MT_TM) {		/* Tape mark */
            mt_error = MTIO_TAPE_MARK;
         } else if (mtget.mt_dsreg & MT_BOT) {	/* BOT */
            mt_error = MTIO_BEGIN_OF_TAPE;
         } else if (mtget.mt_dsreg & MT_EOT) {	/* EOT */
            mt_error = MTIO_END_OF_TAPE;
         } else if (mtget.mt_dsreg & MT_OVR) {	/* record too long */
            mt_error = MTIO_REC_TRUNCATE;
         } else {
            mt_error = MTIO_NO_ERROR;
         }
         if (!(mtget.mt_dsreg & MT_ONL) && !(mtget.mt_dsreg & MT_RDY)) {
            mt_error = MTIO_OP_ERROR;
         }
      } else {					/* not implemented */
#if	defined(TESTBED)
         anyoutf( 0, "(MTIODEV) Unknown convex device %x", mtget.mt_type );
#endif
         mt_error = MTIO_NOT_IMPLEMENTED;
      }
#elif	defined(__sun__)			/* sun */
#ifdef	TESTBED
      {
         static int  show = 1;
         if ( show ) {
            show = 0; anyoutf( 0, "Sun       mt_type    : %8x", mtget.mt_type );
         }
      }
#endif
      if (mtget.mt_type == MT_ISXY) {		/* Xylogics 472 */
         switch(mtget.mt_erreg & 0x3f) {	/* select status */
            case 0x00: {			/* command successed */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x1e:
            case 0x22: {			/* Tape mark enc. */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x30: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            case 0x1c: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x23: {			/* record too long */
               mt_error = MTIO_REC_TRUNCATE;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown sun code: type = %x erreg = %x", mtget.mt_type, mtget.mt_erreg & 0x3f );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#if	!defined(MT_ISEXABYTE) & defined(MT_ISCCS25)
#define	MT_ISEXABYTE	MT_ISCCS25
#endif
      } else if (mtget.mt_type == MT_ISEXABYTE) {
         switch( mtget.mt_erreg & 0x3f ) {
            case 0x00: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x06: {
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x08: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x12: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x13:				/* Backwards into BOT */
            case 0x15: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown sun code: type = %x erreg = %x", mtget.mt_type, mtget.mt_erreg & 0x3f );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#if	!defined(MT_ISHP) & defined(MT_ISCCS20)
#define	MT_ISHP	MT_ISCCS20
#endif
      } else if (mtget.mt_type == MT_ISHP) {
         switch( mtget.mt_erreg & 0x3f ) {
            case 0x00: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x03: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x06: {
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x08: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x12: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x13: {			/* cannot skip past last TM */
               mt_error = MTIO_OP_ERROR;
               break;
            }
            case 0x15: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown sun code: type = %x erreg = %x", mtget.mt_type, mtget.mt_erreg & 0x3f );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#if	!defined(MT_ISEXB8500) & defined(MT_ISCCS26)
#define	MT_ISEXB8500	MT_ISCCS26
#endif
#if	!defined(MT_ISEXB8500) & defined(MT_ISEXABYTE2)
#define	MT_ISEXB8500	MT_ISEXABYTE2
#endif
      } else if (mtget.mt_type == MT_ISEXB8500) {
#ifdef	TESTBED
         anyoutf( 0, "Sun       mt_erreg   : %8x", mtget.mt_erreg );
#endif
         switch( mtget.mt_erreg & 0x3f ) {
            case 0x00: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x03:				/* EOT */
            case 0x13: {			/* extra 980710 KGB */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x06: {
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x12: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
#if	0					/* 980710 KGB */
            case 0x13:				/* Backwards into BOT */
#endif
            case 0x15: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown sun code: type = %x erreg = %x", mtget.mt_type, mtget.mt_erreg & 0x3f );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#if	!defined(MT_ISWANGTHS) & defined(MT_ISCCS27)
#define	MT_ISWANGTHS	MT_ISCCS27
#endif
      } else if (mtget.mt_type == MT_ISWANGTHS) {
         switch( mtget.mt_erreg & 0x3f ) {
            case 0x00: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x03: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x06: {
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x12: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x13:				/* Backwards into BOT */
            case 0x15: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown sun code: type = %x erreg = %x", mtget.mt_type, mtget.mt_erreg & 0x3f );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#ifdef	MT_IS8MM
      } else if (mtget.mt_type == MT_IS8MM) {
         switch( mtget.mt_erreg & 0xffff ) {
            case 0x00: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
#if	0
            case 0x03: {			/* TM */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x03: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x06: {
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x12: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x13:				/* Backwards into BOT */
            case 0x15: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
#endif
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown sun code: type = %x erreg = %x", mtget.mt_type, mtget.mt_erreg & 0xffff );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#endif
#if	defined(__svr4__)
#if	defined(MT_ISCCS28) & !defined(MT_ISRDAT_A)
#define	MT_ISRDAT_A	MT_ISCCS28
#endif
#if	!defined(MT_ISRDAT_A) & defined(MT_ISDAT)
#define	MT_ISRDAT_A	MT_ISDAT
#endif
#else
#if	!defined(MT_ISRDAT_A) & defined(MT_ISCCS28)
#define	MT_ISRDAT_A	MT_ISCCS28
#endif
#endif
      } else if (mtget.mt_type == MT_ISRDAT_A
#if defined(MT_ISDAT)
                                              || mtget.mt_type == MT_ISDAT
#endif
                                                                           ) {
         switch( mtget.mt_erreg & 0xffff ) {
            case 0x00: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x13:
            case 0x08: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x12: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x15: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown sun code: type = %x erreg = %x", mtget.mt_type, mtget.mt_erreg & 0xffff );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#if	defined(MT_ISPYTHON)
      } else if (mtget.mt_type == MT_ISPYTHON) {
         switch( mtget.mt_erreg & 0xffff ) {
            case 0x00: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x08:
            case 0x13: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x12: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x15: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown sun code: type = %x erreg = %x", mtget.mt_type, mtget.mt_erreg & 0xffff );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#endif
#if	defined(MT_ISDEFAULT)
      } else if (mtget.mt_type == MT_ISDEFAULT) {
         switch( mtget.mt_erreg & 0xffff ) {
            case 0x00: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x08:
            case 0x13: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x12: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x15: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown sun code: type = %x erreg = %x", mtget.mt_type, mtget.mt_erreg & 0xffff );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#endif
#if	defined(MT_ISDLT)
      } else if (mtget.mt_type == MT_ISDLT) {
         switch( mtget.mt_erreg & 0xffff ) {
            case 0x00: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x08:
            case 0x13: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            case 0x12: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x15: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            default: {				/* always bad */
#if	defined(TESTBED)
               anyoutf( 0, "(MTIODEV) Unknown sun code: type = %x erreg = %x", mtget.mt_type, mtget.mt_erreg & 0xffff );
#endif
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#endif
      } else {					/* not implemented */
#if	defined(TESTBED)
         anyoutf( 0, "(MTIODEV) Unknown sun device %x", mtget.mt_type );
#endif
         mt_error = MTIO_NOT_IMPLEMENTED;
      }
#elif	defined(__decstation__)			/* decstation */
      if (devget.bus == DEV_SCSI) {
         mt_error = MTIO_NO_ERROR;
         if (devget.category_stat & DEV_TPMARK) {
            mt_error = MTIO_TAPE_MARK;
         }
      } else {					/* not implemented */
#if	defined(TESTBED)
         anyoutf( 0, "(MTIODEV) Unknown dec device %x", devget.bus );
#endif
         mt_error = MTIO_NOT_IMPLEMENTED;
      }
#elif	defined(__hpux__)
      if (mtget.mt_type == MT_ISSTREAM) {
         if (mtget.mt_dsreg1 & 0x80) {
            mt_error = MTIO_TAPE_MARK;
         } else if (mtget.mt_dsreg1 & 0x100) {
            mt_error = MTIO_TAPE_MARK;
         } else if (mtget.mt_dsreg1 & 0x400) {
            mt_error = MTIO_BEGIN_OF_TAPE;
         } else if (mtget.mt_dsreg1 & 0x40) {
            mt_error = MTIO_BEGIN_OF_TAPE;
         } else if (mtget.mt_resid) {
            mt_error = MTIO_REC_TRUNCATE;
         } else {
            mt_error = MTIO_NO_ERROR;
         }
#if	defined(MT_ISDDS2)
      } else if (mtget.mt_type == MT_ISDDS2) {
         switch(mtget.mt_dsreg1 & 0xffff) {
            case 0x000: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x100: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x400: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            case 0x500: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            default: {
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#endif
#if	defined(MT_ISDLT)
      } else if (mtget.mt_type == MT_ISDLT) {
         switch(mtget.mt_dsreg1 & 0xffff) {
            case 0x000: {			/* success */
               mt_error = MTIO_NO_ERROR;
               break;
            }
            case 0x100: {			/* tape mark encountered */
               mt_error = MTIO_TAPE_MARK;
               break;
            }
            case 0x400: {			/* BOT */
               mt_error = MTIO_BEGIN_OF_TAPE;
               break;
            }
            case 0x500: {			/* EOT */
               mt_error = MTIO_END_OF_TAPE;
               break;
            }
            default: {
               mt_error = MTIO_OP_ERROR;
               break;
            }
         }
#endif
      } else {					/* not implemented */
         mt_error = MTIO_NOT_IMPLEMENTED;
      }
#endif
   }
   return( mt_error );				/* return to caller */
#endif
}

static fint mtio_ctop( fint mtid, int op, fint count )	/* tape operations */
{
   fint        mt_error = MTIO_NO_ERROR;	/* tape status */
   int         ioctl( );			/* tape io operations */
#if	defined(__aix__)
   struct stop stop;				/* tape operation struct */

   stop.st_op = op;				/* type of operation */
   stop.st_count = count;			/* number of operations (usually one) */
   if (ioctl( mt_info[mtid].mtid, STIOCTOP, &stop) == -1) {
      switch( op ) {
         case STREW: {
            break;
         }
         case STWEOF: {
            break;
         }
         case STFSF: {
            mt_error = MTIO_END_OF_TAPE;
            break;
         }
         case STRSF: {
            mt_error = MTIO_BEGIN_OF_TAPE;
            break;
         }
         case STFSR: {
            mt_error = MTIO_TAPE_MARK;
            break;
         }
         case STRSR: {
            mt_error = MTIO_TAPE_MARK;
            break;
         }
         default: {
            mt_error = MTIO_OP_ERROR;
            break;
         }
      }
   } else {
      mt_error = mtio_status( mtid );		/* return status */
   }
#else
   struct mtop mtop;				/* tape operation struct */

   mtop.mt_op = op;				/* type of operation */
   mtop.mt_count = count;			/* number of operations (usually one) */
   if (ioctl( mt_info[mtid].mtid, MTIOCTOP, &mtop) == -1) {
      mt_error = mtio_status( mtid );		/* MTIO_OP_ERROR; */
   } else {
      mt_error = mtio_status( mtid );		/* return status */
   }
#endif
   return( mt_error );				/* return to caller */
}

static fint mtio_clear( fint mtid )		/* Clears status (we hope) */
{
   fint  mt_error = MTIO_NO_ERROR;		/* device status */
#if	defined(MTCSE)				/* decstation has it */
   mt_error = mtio_ctop( mtid, MTCSE, 0 );
#elif	defined(__aix__)
/* Removed  by KGB, Nov 7, 1994
#elif	defined(__hpux__)
*/
#else						/* other */
   char *device;				/* device name */
   int   fd;					/* file descriptor */
   int   close( );				/* closes device */
   int   ioctl( );				/* performs io operation on device */
   int   open( );				/* opens device */
#if	defined(__alliant__)
   void  flock( );				/* locks device */
#endif

#if	defined(__sun__)
   switch( mt_info[mtid].type ) {
      case MT_ISEXABYTE:
      case MT_ISHP:
      case MT_ISEXB8500: {
         mt_error = mtio_ctop( mtid, MTNOP, 1 );
         return( mt_error );
         break;
      }
      default: {
         break;
      }
   }
#endif
   fd = mt_info[mtid].mtid;			/* file descriptor */
   device = mt_info[mtid].device;		/* name of device */
#if	0/*defined(__alliant__)*/
   flock( fd, LOCK_UN );			/* unlock tape unit */
#endif
   if (close( fd ) == -1) {			/* close clears status (we hope) */
      mt_error = MTIO_OP_ERROR;			/* error */
   }
   if (mt_error == MTIO_NO_ERROR) {		/* now try to open it again */
      if (mt_info[mtid].readonly) {
         fd = open( device, O_RDONLY );
      } else {
         fd = open( device, O_RDWR );
      }
      if (fd == -1) mt_error = MTIO_OP_ERROR;
   }
   if (mt_error == MTIO_NO_ERROR) {		/* successful clear */
#if	0/*defined(__alliant__)*/
      flock( fd, LOCK_EX );			/* lock tape unit */
#endif
      mt_info[mtid].mtid = fd;			/* new (?) file descriptor */
   }
#endif
   return( mt_error );				/* return to caller */
}
#endif


static fint mtio_open( fint mtid )		/* Open a tape device */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      char   dname[MAXDEVNAMLEN];		/* device name */
      char  *name;
      $DESCRIPTOR(src,dname);
      int    sys$assign();
      int    sys$mount();
      int    status;
      int    tden,flag,siz;
      short  qio_chan;
      char  *c;
      struct MNTL {
         short length;
         short func;
         char  *buf;
         int   term;
      } mntl[6];

      name = mt_info[mtid].device; c = dname;
      while (*name) *c++ = *name++;		/* copy dev. name to buf. */
      *c = '\0';
      siz = 32000; tden = mt_info[mtid].dens;	/* setup MOUNT parameters */
      flag = MNT$M_FOREIGN | MNT$M_NOLABEL;
      mntl[0].length = strlen(dname);
      mntl[0].func = MNT$_DEVNAM;
      mntl[0].buf = dname;
      mntl[0].term = 0;
      mntl[1].length = 4;
      mntl[1].func = MNT$_FLAGS;
      mntl[1].buf = (char *) &flag;
      mntl[1].term = 0;
      mntl[2].length = 4;
      mntl[2].func = MNT$_DENSITY;
      mntl[2].buf = (char *) &tden;
      mntl[2].term = 0;
      mntl[3].length = 4;
      mntl[3].func = MNT$_BLOCKSIZE;
      mntl[3].buf = (char *) &siz;
      mntl[3].term = 0;
      mntl[4].length = 4;
      mntl[4].func = MNT$_RECORDSIZ;
      mntl[4].buf = (char *) &siz;
      mntl[4].term = 0;
      mntl[5].length = 0;
      mntl[5].func = 0;
      mntl[5].buf = (char *) 0;
      mntl[5].term = 0;
      status = sys$mount(mntl);
      status &= 0xffff;				/* blank out status level */
      if (status != SS$_NORMAL && status != SS$_DEVMOUNT) {
         mt_error = MTIO_OP_ERROR;
      }
      if (mt_error == MTIO_NO_ERROR /*&& status == SS$_DEVMOUNT*/) {
         status = sys$assign(&src,&qio_chan,0,0);	/* Device already mounted */
      }
      if (mt_error == MTIO_NO_ERROR && status != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      }
      if (mt_error == MTIO_NO_ERROR) {
         mt_info[mtid].mtid = qio_chan;
         mt_info[mtid].open = 1;
      }
#else						/* UNIX */
      if (!mt_info[mtid].remote) {		/* local */
         int         fd;
         int         open( );
#if	0/*defined(__alliant__)*/
         void        flock( );
#endif

#if	defined(__convex__)
         /*
          * Convexes need to mount a tape.
          */
         {
            char	cmd[80];

            sprintf( cmd, "tpmount -a %s > /dev/null", mt_info[mtid].device );
            system( cmd );
         }
#endif
         if ((fd = open( mt_info[mtid].device, O_RDWR )) == -1) {
            if ((fd = open( mt_info[mtid].device, O_RDONLY )) == -1) {
               mt_error = MTIO_OP_ERROR;
            } else {
               mt_info[mtid].readonly = 1;
            }
         } else {
            mt_info[mtid].readonly = 0;
         }
#if	defined(__aix__)
         mt_info[mtid].mtid = fd;		/* save file descriptor */
         mt_info[mtid].open = 1;		/* set open flag */
         if (mt_error == MTIO_NO_ERROR) {
            int			ioctl( );
            struct stchgp	chgp;

            chgp.st_ecc = ST_NOECC;
            chgp.st_blksize = 0;
            if (ioctl( fd, STIOCHGP, &chgp ) == -1) {
            }
         }
#else
         if (mt_error == MTIO_NO_ERROR) {
            struct mtget	mtget;		/* tape status struct */
            int			ioctl( );

#if	0/*defined(__alliant__)*/
            flock( fd, LOCK_EX );		/* lock it */
#endif
            mt_info[mtid].mtid = fd;		/* save file descriptor */
            if (ioctl( fd, MTIOCGET, &mtget ) == -1) {
               mt_error = MTIO_OP_ERROR;	/* error */
            } else {
               mt_info[mtid].type = mtget.mt_type;
               mt_info[mtid].open = 1;		/* set open flag */
#if	defined(__convex__)
               if (mt_info[mtid].type == MT_ISTA) {
               	  if (mtget.mt_dsreg & MT_FPT) {
               	     mt_info[mtid].readonly = 1;
               	  }
               }
#endif
#if	defined(__sun__) & defined(__svr4__)
               {
                  struct stat ptr;

                  if (fstat( fd, &ptr )) {
                     mt_error = MTIO_OP_ERROR;
                  } else {
                     if ( MT_BSD & minor( ptr.st_rdev ) ) {
                        mt_info[mtid].bsd = 1;
                     } else {
                        mt_info[mtid].bsd = 0;
                     }
                  }
               }
#endif
            }
         }
#endif
      } else {					/* remote */
         op_struct	op;			/* the op struct */

         mt_info[mtid].sock = start( mt_info[mtid].host, mt_info[mtid].port );
         if (mt_info[mtid].sock != -1) {
            op.opcode = OP_OPN;
            op.status = 1;
            op.counts = strlen( mt_info[mtid].device ) + 1;
            if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
               mt_error = MTIO_OP_ERROR;
            } else if (put( mt_info[mtid].sock, mt_info[mtid].device, op.counts )) {
               mt_error = MTIO_OP_ERROR;
            } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
               mt_error = MTIO_OP_ERROR;
            } else if (!op.status) {
               mt_error = MTIO_NO_ERROR;
               mt_info[mtid].open = 1;		/* set open flag */
            } else {
               mt_error = MTIO_OP_ERROR;
            }
         } else {
            mt_error = MTIO_OP_ERROR;
         }
      }
#endif
   } else {					/* disk tape */
      char       dir[MAXDEVNAMLEN];
      char       hfil[MAXDEVNAMLEN];
      char       dfil[MAXDEVNAMLEN];
      char       line[MAXDEVNAMLEN];
      FILE      *head;
      FILE      *data;
      int        bin;
      int        r;
      int        mkdir( );
      int        nptr;
      int	 dummy = 0;
      rc_struct *fptr;

#if	defined(__vms__)			/* VMS */
      {
         char  vmsnam[MAXDEVNAMLEN];
         FILE *vmsdir;

         strcpy( vmsnam, mt_info[mtid].device );
         strcat( vmsnam, ".dir" );
         if ((vmsdir = fopen( vmsnam, "r" )) == NULL) {
            r = mkdir( mt_info[mtid].device, 480 );	/* create directory */
         } else {
            r = 0;
            fclose( vmsdir );
         }
      }
#else						/* UNIX */
      r = mkdir( mt_info[mtid].device, 0755 );		/* create directory */
      if (r && ( errno == EEXIST || errno == EROFS ) ) r = 0;
#endif
      if (r) {
         mt_error = MTIO_OP_ERROR;
      } else {					/* make correct directory prefix */
#if	defined(__vms__)			/* VMS */
         strcpy( dir, "[." );
         strcat( dir, mt_info[mtid].device );
         strcat( dir, "]" );
#else						/* UNIX */
         strcpy( dir, mt_info[mtid].device );
         strcat( dir, "/" );
#endif
         r = strlen( dir );
         mt_info[mtid].devdir = calloc( r + 1, sizeof( char ) );
         strcpy( mt_info[mtid].devdir, dir );
         strcpy( hfil, dir );
         strcat( hfil, "tape.descriptor" );	/* tape descriptor file */
         if (fsize_c( tofchar( hfil ) ) > 0) {
            head = fopen( hfil, "r" );
         } else {
            head = NULL;
         }
         if (head == NULL) {			/* no descriptor, try to make one */
            int num = 0;
            do {
               sprintf( dfil, "%sfile%06.6d.mt", dir, num + 1 );	/* name file */
               data = fopen( dfil, "rb" );
               if (data != NULL) {
                  int r;
                  int size;
                  int nblocks;
                  int nover;

                  r = fseek( data, 0, SEEK_END );
                  size = ftell( data );
                  nblocks = size / 2880;
                  nover = size % 2880;
                  fclose(data);
                  head = fopen( hfil, "a" );
                  if ( head == NULL ) {
                     strcpy( hfil, "/tmp/dummy.descriptor" );
                     head = fopen( hfil, "a" );
                     if ( head == NULL ) {
                        mt_error = MTIO_OP_ERROR;
                        break ;
                     }
                     dummy = 1;
                  }
                  fprintf( head, "@file%06.6d.mt\n", ++num );
                  for (r = 0; r < nblocks; r++) {
                     fprintf( head, "2880,0\n" );
                  }
                  if (nover) {
                     fprintf( head, "%d,0\n", nover );
                  }
                  fprintf( head, "0,0\n" );
                  fclose( head );
               } else {
                  break;
               }
            } while (1);
            if (mt_error == MTIO_NO_ERROR) {
               head = fopen( hfil, "a" );
               if ( head == NULL ) {
                  mt_error = MTIO_OP_ERROR;
               } else {
                  if (num) fprintf( head, "0,0\n" );
                  fclose( head );
               }
            }
         } else {
            fclose( head );
         }
         /*
          * Now get the file pointers of the tape descriptor file.
          */
         fptr = NULL;
         nptr = 0;
         if (mt_error == MTIO_NO_ERROR) {
            head = fopen( hfil, "r" );
            bin = 0;
            while (!feof( head )) {
               if (fgets( line, MAXDEVNAMLEN, head )) {
                  fptr = realloc( fptr, ++nptr * sizeof( rc_struct ) );
                  if (!isdigit( line[0] )) {
                     char   *file, *lptr;
                     if (line[0] == '@') lptr = &line[1]; else lptr = &line[0];
                     file = lptr;
                     file[strcspn( lptr, " ,\n" )] = 0;
                     fptr[nptr-1].file = calloc( sizeof( char ), strlen( file ) + 1 );
                     strcpy( fptr[nptr-1].file, file );
                     if (fgets( line, MAXDEVNAMLEN, head )) {
                        char *size = line;
                        size[strcspn( line, " ,\n" )] = 0;
                        fptr[nptr-1].size = bin = atoi( size );
                        if (!bin) {
                           free( fptr[nptr-1].file );
                           fptr = realloc( fptr, --nptr * sizeof( rc_struct ) );
                        }
                     } else {
                        free( fptr[nptr-1].file );
                        fptr = realloc( fptr, --nptr * sizeof( rc_struct ) );
                        break;
                     }
                  } else {
                     char *size = line;
                     size[strcspn( line, " ,\n" )] = 0;
                     fptr[nptr-1].size = bin = atoi( size );
                     if (bin) {
                        fptr[nptr-1].file = fptr[nptr-2].file;
                     } else {
                        fptr[nptr-1].file = NULL;
                     }
                  }
               } else {
                  break;
               }
            };
            fclose( head );
            if (dummy) remove( hfil );
         }
         mt_info[mtid].data = NULL;
         mt_info[mtid].nptr = nptr;
         mt_info[mtid].fptr = fptr;
         mt_info[mtid].recc = 0;
         mt_info[mtid].nfil = 0;
         mt_info[mtid].nrec = 0;
         mt_info[mtid].mtid = -1;
         mt_info[mtid].open = 1;
      }
   }
   if (mt_error != MTIO_NO_ERROR) {
      mt_info[mtid].open = 0;
      free( mt_info[mtid].device );
      if ((mt_devs - mtid) == 1) {
         mt_info = realloc( mt_info, --mt_devs * sizeof( mt_struct ) );
      }
   } else {
      mt_error = mtid;
   }
   return( mt_error );				/* return to caller */
}


static fint mtio_close( fint mtid )		/* Closes a specific tape device */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      char  dname[MAXDEVNAMLEN];                /* device name */
      char *name;
      $DESCRIPTOR(src,dname);
      int   sys$dassgn();
      int   sys$dismou();
      int   status;
      short qio_chan;
      char *c;
      short iosb[4];

      name = mt_info[mtid].device; c = dname;
      while (*name) *c++ = *name++;		/* copy dev. name to buf. */
      *c = '\0';
      qio_chan = mt_info[mtid].mtid;
      status = sys$qiow(0,qio_chan,IO$_REWIND,iosb,0,0,0,0,0,0,0,0);
      status = sys$dassgn(qio_chan);
      if (status != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      } else {
         status = sys$dismou(&src,DMT$M_NOUNLOAD);
      }
#else						/* UNIX */
      if (!mt_info[mtid].remote) {		/* local */
         int  fd = mt_info[mtid].mtid;
         int  close( );
#if	0/*defined(__alliant__)*/
         void flock( );
#endif

#if	0/*defined(__alliant__)*/
         flock( fd, LOCK_UN );
#endif
         if (close( fd ) == -1) mt_error = MTIO_OP_ERROR;
#if	defined(__convex__)
         /*
          * Convexes need to dismount the tape.
          */
         {
            char	cmd[80];

            sprintf( cmd, "tpunmount -k -s %s > /dev/null", mt_info[mtid].device );
            system( cmd );
         }
#endif
      } else {					/* remote */
         op_struct 	op;			/* the op struct */

         op.opcode = OP_CLO;
         if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = op.status;
         }
      }
#endif
   } else {					/* disk tape */
      char  file[MAXDEVNAMLEN];
      FILE *head;
      int   bin;
      int   n;

      if (mt_info[mtid].data != NULL) {
         fclose( mt_info[mtid].data ); mt_info[mtid].data = NULL;
      }
      strcpy( file, mt_info[mtid].devdir );
      strcat( file, "tape.descriptor" );
      head = fopen( file, "w" );
      if( head ) {
         for (bin = 0, n = 0; n < mt_info[mtid].nptr; n++) {
            if (mt_info[mtid].fptr[n].file && !bin) {
               fprintf( head, "@%s\n", mt_info[mtid].fptr[n].file );
               free( mt_info[mtid].fptr[n].file );
               bin = 1;
            } else if (!mt_info[mtid].fptr[n].file) {
               bin = 0;
            }
            fprintf( head, "%d\n", mt_info[mtid].fptr[n].size );
         }
         fclose( head );
      }
      if (mt_info[mtid].fptr) free( mt_info[mtid].fptr );
      mt_info[mtid].nptr = 0;
   }
   if (mt_error == MTIO_NO_ERROR) {
      mt_info[mtid].open = 0;
      if (mt_info[mtid].dev != NULL) free( mt_info[mtid].dev );
      mt_info[mtid].dev = NULL;
      if (mt_info[mtid].devdir != NULL) free( mt_info[mtid].devdir );
      mt_info[mtid].devdir = NULL;
      if (mt_info[mtid].remote) {
         int	close( );

/*         if (mt_info[mtid].host != NULL) free( mt_info[mtid].host );*/
         mt_info[mtid].host = NULL;
         close( mt_info[mtid].sock );
         mt_info[mtid].sock = -1;
      }
      while (mt_devs && !mt_info[mt_devs-1].open) {
         mt_info = realloc( mt_info, --mt_devs * sizeof( mt_struct ) );
      }
   }
   return( mt_error );				/* return to caller */
}


static fint mtio_rew( fint mtid )		/* Rewind tape device */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      int   sys$qiow();
      int   status;
      short qio_chan;
      short iosb[4];

      qio_chan = mt_info[mtid].mtid;
      status = sys$qiow(0,qio_chan,IO$_REWIND,iosb,0,0,0,0,0,0,0,0);
      if (status != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      }
#else						/* UNIX */
      if (!mt_info[mtid].remote) {		/* local */
#if	defined(__aix__)
         mt_error = mtio_ctop( mtid, STREW, 1 );
#else
         mt_error = mtio_ctop( mtid, MTREW, 1 );
#endif
         if (mt_error == MTIO_NO_ERROR || mt_error == MTIO_BEGIN_OF_TAPE) {
            mt_error = mtio_clear( mtid );
         } else {
            mt_error = MTIO_OP_ERROR;
         }
      } else {					/* remote */
         op_struct	op;			/* the op struct */

         op.opcode = OP_REW;
         if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = op.status;
         }
      }
#endif
   } else {					/* disk tape */
      if (mt_info[mtid].data != NULL) {
         fclose( mt_info[mtid].data ); mt_info[mtid].data = NULL;
      }
      mt_info[mtid].recc = 0;
      mt_info[mtid].nfil = 0;
      mt_info[mtid].nrec = 0;
   }
   return( mt_error );				/* return to caller */
}


static fint mtio_read( fint mtid, char *buffer, fint size )	/* read from tape */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      char  *buf = NULL;
      int    fac = 10;
      int    nbf = 0;
      int    sys$qiow();
      int    status;
      short  qio_chan;
      short  iosb[4];

      qio_chan = mt_info[mtid].mtid;
      status = sys$qiow(0,qio_chan,IO$_READLBLK,iosb,0,0,buffer,size,0,0,0,0);
      if (status != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      } else if (iosb[0] == SS$_ENDOFFILE) {
         mt_error = 0;
      } else {
         while (iosb[0] == SS$_DATAOVERUN) {
            short  iosb2[4];

            nbf = size * fac;
            fac *= 2;
            buf = realloc( buf, sizeof( char ) * nbf );
            status = sys$qiow(0,qio_chan,IO$_SKIPRECORD,iosb2,0,0,-1,0,0,0,0,0);
            if (status != SS$_NORMAL) { mt_error = MTIO_OP_ERROR; break; }
            status = sys$qiow(0,qio_chan,IO$_READLBLK,iosb,0,0,buf,nbf,0,0,0,0);
            if (status != SS$_NORMAL) { mt_error = MTIO_OP_ERROR; break; }
         }
         if (mt_error == MTIO_NO_ERROR) mt_error = iosb[1];
         if (buf != NULL) free( buf );
      }
#else						/* UNIX */
      /* Here we read the record into local memory. We expect read
       * to return the number of bytes actually read. In order to
       * detect long/short records we try to read MAXRECORDLEN
       * bytes.
       */
      if (!mt_info[mtid].remote) {		/* local */
         char	mybuf[MAXRECORDLEN];
         int	read( );

         mt_error = read( mt_info[mtid].mtid, mybuf, MAXRECORDLEN );
         if (mt_error > 0) {
            memcpy( buffer, mybuf, mt_error > size ? size : mt_error );
         } else {
            mt_error = mtio_status( mtid );
            if (mt_error == MTIO_TAPE_MARK) {
#if	defined(__sun__) & defined(__svr4__)
               if ( !mt_info[mtid].bsd ) {
                  mt_error = mtio_ctop( mtid, MTFSF, 1 );
               } else {
                  mt_error = MTIO_NO_ERROR;
               }
#else
               mt_error = mtio_clear( mtid );
#endif
            }
            if (mt_error == MTIO_NO_ERROR) {
               mt_error = 0;
            }
         }
      } else {					/* remote */
         op_struct	op;			/* the op struct */

         op.opcode = OP_GET;
         op.counts = size;
         if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = op.status;
         }
         if (mt_error > 0) {
            if (get( mt_info[mtid].sock, buffer, op.counts )) {
               mt_error = MTIO_OP_ERROR;
            }
         }
      }
#endif
   } else {					/* disk tape */
      char file[MAXDEVNAMLEN];
      int  pos;
      int  r;
      int  recc;


      if (mt_info[mtid].recc >= mt_info[mtid].nptr) {
         mt_error = MTIO_END_OF_TAPE;
      } else {
         recc = mt_info[mtid].recc++;
         if (mt_info[mtid].fptr[recc].file && mt_info[mtid].data == NULL) {
            strcpy( file, mt_info[mtid].devdir );
            strcat( file, mt_info[mtid].fptr[recc].file );
            if ((mt_info[mtid].data = fopen( file, "r+b" )) == NULL) {
               if (errno == EACCES || errno == EROFS ) {/* try only reading */
                  if ((mt_info[mtid].data = fopen( file, "rb" )) == NULL) {
                     mt_error = MTIO_OP_ERROR;
                  } else {
                     fseek( mt_info[mtid].data, 0, SEEK_SET );
                  }
               } else {
                  mt_error = MTIO_OP_ERROR;
               }
            } else {
               fseek( mt_info[mtid].data, 0, SEEK_SET );
            }
         } else if (!mt_info[mtid].fptr[recc].file && mt_info[mtid].data != NULL) {
            if (fclose( mt_info[mtid].data )) {
               mt_error = MTIO_OP_ERROR;
            }
            mt_info[mtid].data = NULL;
         }
         if (mt_error == MTIO_NO_ERROR) {
            int length = mt_info[mtid].fptr[recc].size;

            if (length) {
               pos = ftell( mt_info[mtid].data ) + length;
               if (length < size) size = length;
               r = fread( buffer, sizeof( char ), size, mt_info[mtid].data );
               r = fseek( mt_info[mtid].data, pos, SEEK_SET );
               mt_error = length;
             } else {				/* tape mark */
               if (mt_info[mtid].data != NULL) {
                  fclose( mt_info[mtid].data ); mt_info[mtid].data = NULL;
               }
               mt_info[mtid].nfil += 1;
               mt_error = 0;
            }
         }
      }
   }
   return( mt_error );				/* return to caller */
}


static fint mtio_write( fint mtid, char *buffer, fint size )	/* Write a block */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      int   sys$qiow();
      int   status;
      short qio_chan;
      short iosb[4];

      qio_chan = mt_info[mtid].mtid;
      status = sys$qiow(0,qio_chan,IO$_WRITELBLK,iosb,0,0,buffer,size,0,0,0,0);
      if (status != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      } else if (iosb[0] == SS$_ENDOFTAPE) {
         mt_error = MTIO_END_OF_TAPE;
      } else if (iosb[0] == SS$_TIMEOUT) {
         mt_error = MTIO_OP_ERROR;
      } else {
         mt_error = iosb[1];
      }
#else						/* UNIX */
      if (!mt_info[mtid].remote) {		/* local */
         int          write( );

         if (mt_info[mtid].readonly) {
            mt_error = MTIO_WRITE_LOCK;
         } else if ((mt_error = write( mt_info[mtid].mtid, buffer, size )) == -1) {
            if (errno == EACCES) {
               mt_error = MTIO_WRITE_LOCK;
            } else {
               mt_error = mtio_status( mtid );
            }
         }
      } else {					/* remote */
         op_struct	op;			/* the op struct */

         op.opcode = OP_PUT;
         op.counts = size;
         if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else if (put( mt_info[mtid].sock, buffer, size )) {
            mt_error = MTIO_OP_ERROR;
         } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = op.status;
         }
      }
#endif
   } else {					/* disk tape */
      char       file[MAXDEVNAMLEN];
      FILE      *data = mt_info[mtid].data;
      int        nptr = mt_info[mtid].nptr;
      int        recc = mt_info[mtid].recc;
      int        r;
      rc_struct *fptr = mt_info[mtid].fptr;

      if (recc < nptr) {			/* remove files and records past current record */
         int bin;
         int n = recc;

         if (data != NULL) {
            fint pos = ftell( data );
            fclose( data );
            strcpy( file, mt_info[mtid].devdir );
            strcat( file, fptr[n].file );
            r = ftrunc_c( tofchar( file ), &pos );
            data = fopen( file, "r+" );
            fseek( data, pos, SEEK_SET );
            while (n < nptr && fptr[n].file) n++;	/* skip */
         }
         for (bin = 0; n < nptr; n++) {
            if (fptr[n].file && !bin) {
               strcpy( file, mt_info[mtid].devdir );
               strcat( file, fptr[n].file );
               remove( file );
               free( fptr[n].file );
               bin = 1;
            } else if (!fptr[n].file) {
               bin = 0;
            }
         }
         fptr = realloc( fptr, recc * sizeof( rc_struct ) );
         nptr = recc;
      }
      fptr = realloc( fptr, ++nptr * sizeof( rc_struct ) );
      if (data == NULL) {
         char line[MAXDEVNAMLEN];

         sprintf( line, "file%06.6d.mt", mt_info[mtid].nfil+1 );
         fptr[recc].file = calloc( sizeof( char ), strlen( line ) + 1 );
         strcpy( fptr[recc].file, line );
         strcpy( file, mt_info[mtid].devdir );
         strcat( file, line );
         data = fopen( file, "w+b" );
         if (data == NULL) {
            mt_error = MTIO_OP_ERROR;
         }
         mt_info[mtid].nrec = 0;
      } else {
         fptr[recc].file = fptr[recc-1].file;
      }
      if (mt_error == MTIO_NO_ERROR) {
         mt_error = fwrite( buffer, sizeof( char ), size, data );
         if (mt_error < size && errno == EACCES) {
            mt_error = MTIO_WRITE_LOCK;
         }
      }
      fptr[recc].size = size;
      mt_info[mtid].data = data;
      mt_info[mtid].recc = nptr;
      mt_info[mtid].nrec += 1;
      mt_info[mtid].nptr = nptr;
      mt_info[mtid].fptr = fptr;
   }
   return( mt_error );				/* return to caller */
}


static fint mtio_fsf( fint mtid, fint ntm )	/* Skip forward space file */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      int   sys$qiow();
      int   status;
      short qio_chan;
      short iosb[4];

      qio_chan = mt_info[mtid].mtid;
      status = sys$qiow(0,qio_chan,IO$_SKIPFILE,iosb,0,0,ntm,0,0,0,0,0);
      if (status != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      } else {
         mt_error = iosb[1];
      }
#else						/* UNIX */
      if (!mt_info[mtid].remote) {		/* local */
        int n = 0;

         while (n < ntm && mt_error == MTIO_NO_ERROR) {
#if	defined(__aix__)
            mt_error = mtio_ctop( mtid, STFSF, 1 );
#else
            mt_error = mtio_ctop( mtid, MTFSF, 1 );
#endif
            if (mt_error == MTIO_TAPE_MARK) mt_error = MTIO_NO_ERROR;
            if (mt_error == MTIO_NO_ERROR) n++;
         }
         if (n < ntm) {
            if (mt_error == MTIO_END_OF_TAPE) {
               mt_error = mtio_clear( mtid );
            } else {
               mt_error = MTIO_OP_ERROR;
            }
         }
         if (mt_error == MTIO_NO_ERROR) mt_error = n;
      } else {					/* remote */
         op_struct	op;			/* the op struct */

         op.opcode = OP_FSF;
         op.counts = ntm;
         if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = op.status;
         }
      }
#endif
   } else {					/* disk tape */
      int n = 0;

      if (mt_info[mtid].data != NULL) {
         fclose( mt_info[mtid].data );
         mt_info[mtid].data = NULL;
      }
      while (mt_info[mtid].recc < mt_info[mtid].nptr && n < ntm ) {
         if (!mt_info[mtid].fptr[mt_info[mtid].recc].file) n++;
         mt_info[mtid].recc += 1;
      }
      mt_info[mtid].nfil += n;
      mt_error = n;
   }
   return( mt_error );				/* return to caller */
}


static fint mtio_bsf( fint mtid, fint ntm )	/* Skip backward space file */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      int   sys$qiow();
      int   status;
      short qio_chan;
      short iosb[4];

      qio_chan = mt_info[mtid].mtid;
      status = sys$qiow(0,qio_chan,IO$_SKIPFILE,iosb,0,0,-ntm,0,0,0,0,0);
      if (status != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      } else {
         mt_error = iosb[1];
      }
#else						/* UNIX */
      if (!mt_info[mtid].remote) {		/* local */
         int n = 0;

         while (n < ntm && mt_error == MTIO_NO_ERROR) {
#if	defined(__aix__)
            mt_error = mtio_ctop( mtid, STRSF, 1 );
#else
            mt_error = mtio_ctop( mtid, MTBSF, 1 );
#endif
            if (mt_error == MTIO_TAPE_MARK) mt_error = MTIO_NO_ERROR;
            if (mt_error == MTIO_NO_ERROR) n++;
         }
         if (n < ntm ) {
            if (mt_error == MTIO_BEGIN_OF_TAPE) {
               mt_error = mtio_clear( mtid );
            } else {
               mt_error = MTIO_OP_ERROR;
            }
         }
         if (mt_error == MTIO_NO_ERROR) mt_error = n;
      } else {					/* remote */
         op_struct	op;			/* the op struct */

         op.opcode = OP_BSF;
         op.counts = ntm;
         if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = op.status;
         }
      }
#endif
   } else {					/* disk tape */
      int n = 0;

      if (mt_info[mtid].data != NULL) {
         fclose( mt_info[mtid].data );
         mt_info[mtid].data = NULL;
      }
      while (mt_info[mtid].recc && n < ntm) {
         if (!mt_info[mtid].fptr[--mt_info[mtid].recc].file) n++;
      }
      mt_info[mtid].nfil -= n;
      mt_error = n;
   }
   return( mt_error );				/* return to caller */
}


static fint mtio_fsr( fint mtid, fint count )	/* Forward space record */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      int   sys$qiow();
      int   status;
      short qio_chan;
      short iosb[4];

      qio_chan = mt_info[mtid].mtid;
      status = sys$qiow(0,qio_chan,IO$_SKIPRECORD,iosb,0,0,count,0,0,0,0,0);
      if (status != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      } else {
         mt_error = iosb[1];
         if (iosb[0] == SS$_ENDOFFILE) mt_error -= 1;
      }
#else						/* UNIX */
      if (!mt_info[mtid].remote) {		/* local */
         int n = 0;

#if	defined(__aix__)
         while (n < count && (mt_error = mtio_ctop( mtid, STFSR, 1 )) == MTIO_NO_ERROR) n++;
#else
         while (n < count && (mt_error = mtio_ctop( mtid, MTFSR, 1 )) == MTIO_NO_ERROR) n++;
#endif
         if (n < count) {
#if	defined(__aix__)
            if (mt_error == MTIO_TAPE_MARK) {
               mt_error = mtio_ctop( mtid, STFSF, 1 );
            }
#endif
#if	defined(__sun__)
            if (mt_error == MTIO_TAPE_MARK) {
               switch( mt_info[mtid].type ) {
                  case MT_ISEXABYTE:
                  case MT_ISHP:
                  case MT_ISEXB8500:
                  case MT_ISWANGTHS:
                  case MT_ISRDAT_A:
#ifdef MT_ISDAT
                  case MT_ISDAT:
#endif
                  {
                     
                     mt_error = mtio_ctop( mtid, MTFSF, 1 );
                     break;
                  }
                  default: {
                     break;
                  }
               }
            }
#endif
            if (mt_error == MTIO_END_OF_TAPE || mt_error == MTIO_TAPE_MARK) {
               mt_error = mtio_clear( mtid );
            } else if (mt_error != MTIO_NO_ERROR) {
               mt_error = MTIO_OP_ERROR;
            }
         }
         if (mt_error == MTIO_NO_ERROR) mt_error = n;
      } else {					/* remote */
         op_struct	op;			/* the op struct */

         op.opcode = OP_FSR;
         op.counts = count;
         if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = op.status;
         }
      }
#endif
   } else {					/* disk tape */
      char  file[MAXDEVNAMLEN];
      int   nptr;
      int   recc;
      int   n = 0;

      nptr = mt_info[mtid].nptr;
      recc = mt_info[mtid].recc;
      while (n < count && recc < nptr && mt_info[mtid].fptr[recc].file) {
         if (mt_info[mtid].data == NULL) {
            strcpy( file, mt_info[mtid].devdir );
            strcat( file, mt_info[mtid].fptr[recc].file );
            mt_info[mtid].data = fopen( file, "r+b" );
            if (mt_info[mtid].data == NULL &&
                       ( errno == EACCES || errno == EROFS ) ) {
               mt_info[mtid].data = fopen( file, "rb" );
            }
            if (mt_info[mtid].data == NULL) {
               mt_error = MTIO_OP_ERROR;
               break;
            } else {
               fseek( mt_info[mtid].data, 0, SEEK_SET );
            }
         }
         fseek( mt_info[mtid].data, mt_info[mtid].fptr[recc++].size, SEEK_CUR );
         n++;
      }
      if (mt_error == MTIO_NO_ERROR) {
         mt_error = n;
         if (n < count && recc < nptr) {
            mt_info[mtid].nfil += 1;
            if (!mt_info[mtid].fptr[recc].file) {
               if (mt_info[mtid].data != NULL) {
                  recc += 1;
                  fclose( mt_info[mtid].data );
                  mt_info[mtid].data = NULL;
               }
            }
         }
      }
      mt_info[mtid].recc = recc;
   }
   return( mt_error );				/* return to caller */
}


static fint mtio_bsr( fint mtid, fint count )	/* Backward space record */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      int   sys$qiow();
      int   status;
      short qio_chan;
      short iosb[4];

      qio_chan = mt_info[mtid].mtid;
      status = sys$qiow(0,qio_chan,IO$_SKIPRECORD,iosb,0,0,-count,0,0,0,0,0);
      if (status != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      } else {
         mt_error = iosb[1];
         if (iosb[0] == SS$_ENDOFFILE) mt_error -= 1;
      }
#else						/* UNIX */
      if (!mt_info[mtid].remote) {		/* local */
         int n = 0;

#if	defined(__aix__)
         while (n < count && (mt_error = mtio_ctop( mtid, STRSR, 1 )) == MTIO_NO_ERROR) n++;
#else
         while (n < count && (mt_error = mtio_ctop( mtid, MTBSR, 1 )) == MTIO_NO_ERROR) n++;
#endif
         if (n < count) {
#if	defined(__aix__)
            if (mt_error == MTIO_TAPE_MARK) {
               mt_error = mtio_ctop( mtid, STRSF, 1 );
               if (mt_error == MTIO_BEGIN_OF_TAPE) {
                  mt_error = MTIO_NO_ERROR;
               }
            }
#elif	defined(__sun__)
            if (mt_error == MTIO_TAPE_MARK) {
               switch( mt_info[mtid].type ) {
                  case MT_ISEXABYTE:
                  case MT_ISHP:                  
                  case MT_ISEXB8500:
                  case MT_ISWANGTHS:
                  case MT_ISRDAT_A:
#ifdef MT_ISDAT
                  case MT_ISDAT:
#endif
#ifdef	MT_ISDEFAULT
                  case MT_ISDEFAULT:
#endif
#ifdef	MT_ISDLT
                  case MT_ISDLT:
#endif
#ifdef	MT_ISPYTHON
                  case MT_ISPYTHON:
#endif
                  {
                     mt_error = mtio_ctop( mtid, MTBSF, 1 );
                     break;
                  }
                  default: {
                     mt_error = mtio_clear( mtid );
                     break;
                  }
               }
            } else if (mt_error == MTIO_BEGIN_OF_TAPE) {
               mt_error = mtio_clear( mtid );
            } else {
               mt_error = MTIO_OP_ERROR;
            }
#else
            if (mt_error == MTIO_TAPE_MARK || mt_error == MTIO_BEGIN_OF_TAPE) {
               mt_error = mtio_clear( mtid );
            } else {
               mt_error = MTIO_OP_ERROR;
            }
#endif
         }
         if (mt_error == MTIO_NO_ERROR) mt_error = n;
      } else {					/* remote */
         op_struct	op;			/* the op struct */

         op.opcode = OP_BSR;
         op.counts = count;
         if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = op.status;
         }
      }
#endif
   } else {					/* disk tape */
      char  file[MAXDEVNAMLEN];
      int   nptr;
      int   recc;
      int   n = 0;

      nptr = mt_info[mtid].nptr;
      recc = mt_info[mtid].recc;
      while (n < count && recc && mt_info[mtid].fptr[recc-1].file) {
         if (mt_info[mtid].data == NULL) {
            strcpy( file, mt_info[mtid].devdir );
            strcat( file, mt_info[mtid].fptr[recc-1].file );
            mt_info[mtid].data = fopen( file, "r+b" );
            if (mt_info[mtid].data == NULL &&
                     ( errno == EACCES || errno == EROFS ) ) {
               mt_info[mtid].data = fopen( file, "rb" );
            }
            if (!mt_info[mtid].data) {
               mt_error = MTIO_OP_ERROR;
               break;
            } else {
               fseek( mt_info[mtid].data, 0, SEEK_END );
            }
         }
         fseek( mt_info[mtid].data, -mt_info[mtid].fptr[--recc].size, SEEK_CUR );
         n++;
      }
      if (mt_error == MTIO_NO_ERROR) {
         if (n < count && recc) {
            if (!mt_info[mtid].fptr[recc-1].file)  {
               mt_info[mtid].nfil -= 1;
               if (mt_info[mtid].data != NULL) {
                  recc -= 1;
                  fclose( mt_info[mtid].data );
                  mt_info[mtid].data = NULL;
               }
            }
         }
         mt_error = n;
      }
      mt_info[mtid].recc = recc;
   }
   return( mt_error );				/* return to caller */
}


static fint mtio_weof( fint mtid, fint ntm )	/* Write end-of-file record */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      int   sys$qiow();
      int   i, status;
      short qio_chan;
      short iosb[4];

      qio_chan = mt_info[mtid].mtid;
      status = sys$qiow(0,qio_chan,IO$_WRITEOF,iosb,0,0,ntm,0,0,0,0,0);
      if (status != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      } else if (iosb[0] == SS$_ENDOFTAPE) {
         mt_error = MTIO_END_OF_TAPE;
      } else {
         mt_error = iosb[1];
      }
#else						/* UNIX */
      if (!mt_info[mtid].remote) {		/* local */
         if (mt_info[mtid].readonly) {
            mt_error = MTIO_WRITE_LOCK;
         } else {
            int n = 0;

            while (n < ntm && mt_error == MTIO_NO_ERROR) {
#if	defined(__aix__)
               mt_error = mtio_ctop( mtid, STWEOF, 1 );
#else
               mt_error = mtio_ctop( mtid, MTWEOF, 1 );
#endif
               if (mt_error == MTIO_TAPE_MARK) mt_error = MTIO_NO_ERROR;
               if (mt_error == MTIO_NO_ERROR) n++;
            }
            if (n < ntm) {
               if (mt_error == MTIO_END_OF_TAPE) {
                  mt_error = mtio_clear( mtid );
               } else {
                  mt_error = MTIO_OP_ERROR;
               }
            }
            if (mt_error == MTIO_NO_ERROR) mt_error = n;
         }
      } else {					/* remote */
         op_struct	op;			/* the op struct */

         op.opcode = OP_WTM;
         op.counts = ntm;
         if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = op.status;
         }
      }
#endif
   } else {					/* disk tape */
      char       file[MAXDEVNAMLEN];
      FILE      *data = mt_info[mtid].data;
      int        nptr = mt_info[mtid].nptr;
      int        recc = mt_info[mtid].recc;
      int        r;
      int        n;
      rc_struct *fptr = mt_info[mtid].fptr;

      if (recc < nptr) {			/* remove files and records past current record */
         int bin;
         int n = recc;

         if (data != NULL) {
            fint pos = ftell( data );
            fclose( data );
            strcpy( file, mt_info[mtid].devdir );
            strcat( file, fptr[n].file );
            r = ftrunc_c( tofchar( file ), &pos );
            fclose( data );
            data = NULL;
            while (n < nptr && fptr[n].file) n++;	/* skip */
         }
         for (bin = 0; n < nptr; n++) {
            if (fptr[n].file && !bin) {
               strcpy( file, mt_info[mtid].devdir );
               strcat( file, fptr[n].file );
               remove( file );
               free( fptr[n].file );
               bin = 1;
            } else if (!fptr[n].file) {
               bin = 0;
            }
         }
         fptr = realloc( fptr, recc * sizeof( rc_struct ) );
         nptr = recc;
      }
      fptr = realloc( fptr, ++nptr * sizeof( rc_struct ) );
      if (data != NULL) { fclose( data ); data = NULL; }
      for (n = 0; n < ntm; n++) {
         fptr = realloc( fptr, ++recc * sizeof( rc_struct ) );
         fptr[recc-1].file = NULL;
         fptr[recc-1].size = 0;
      }
      mt_error = ntm;
      mt_info[mtid].nfil += ntm;
      mt_info[mtid].data = data;
      mt_info[mtid].recc = recc;
      mt_info[mtid].nrec = 0;
      mt_info[mtid].nptr = recc;
      mt_info[mtid].fptr = fptr;
   }
   return( mt_error );				/* return to caller */
}


static fint mtio_stat( fint mtid )		/* Get status */
{
   fint mt_error = MTIO_NO_ERROR;		/* tape status */

   if (mt_info[mtid].dens) {			/* tape unit */
#if	defined(__vms__)			/* VMS */
      int sys$getdviw();
      int ret;
      int devchr, devdep;
      int ldevchr, ldevdep;
      int saerror;
      short qiochan;
      struct {
         short l;
         short c;
         int  *a;
         int  *r;
      } itmlst[3];

      qiochan = mt_info[mtid].mtid;
      itmlst[0].l = 4;
      itmlst[0].c = DVI$_DEVCHAR;
      itmlst[0].a = &devchr;
      itmlst[0].r = &ldevchr;
      itmlst[1].l = 4;
      itmlst[1].c = DVI$_DEVDEPEND;
      itmlst[1].a = &devdep;
      itmlst[1].r = &ldevdep;
      itmlst[2].l = 0;
      itmlst[2].c = 0;  			/* end */
      ret = sys$getdviw(NULL,qiochan,NULL,itmlst,NULL,NULL,NULL,NULL);
      if (ret != SS$_NORMAL) {
         mt_error = MTIO_OP_ERROR;
      } else {
         if (devdep & MT$M_BOT) mt_error += 1;
         if (devdep & MT$M_EOT) mt_error += 2;
         if (!(devdep & MT$M_HWL)) mt_error += 4;
         if (devchr & DEV$M_AVL) mt_error += 8;
         if (devdep & MT$M_EOF) mt_error += 16;
      }
#else						/* UNIX */
      if (!mt_info[mtid].remote) {		/* local */
#if	defined(__alliant__)			/* alliant */
         int ret = 0, stat;
         int ioctl();
         struct mtvsr mvs;

         ret = ioctl( mt_info[mtid].mtid, MTIOCVSR, &mvs);
         if (ret == -1) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = 0;
            stat = (mvs.mt_status & mvs.mt_valid);
            if (stat & MTVSR_BOT) mt_error += 1;
            if (stat & MTVSR_EOT) mt_error += 2;
            if (stat & MTVSR_WE) mt_error += 4;
            if (stat & MTVSR_ONLINE) mt_error += 8;
            if (stat & MTVSR_EOF) mt_error += 16;
            if (stat & MTVSR_ERROR) mt_error += 32;
         }
#else						/* other */
         mt_error = MTIO_NOT_IMPLEMENTED;
#endif
      } else {					/* remote */
         op_struct	op;			/* the op struct */

         op.opcode = OP_STA;
         if (put( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else if (get( mt_info[mtid].sock, &op, sizeof( op_struct ) )) {
            mt_error = MTIO_OP_ERROR;
         } else {
            mt_error = op.status;
         }
      }
#endif
   } else {					/* disk tape */
      mt_error = 12;
      if (mt_info[mtid].recc == 0) {
         mt_error += 1;
      } else if (mt_info[mtid].recc == mt_info[mtid].nptr) {
         mt_error += 2;
      }
   }
   return( mt_error );
}


/*
#>            mtopen.dc2

Function:     MTOPEN

Purpose:      Opens a specified tape unit at a specified density or
              a binary file.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTOPEN( DEVICE )   Input    CHARACTER*(*)

              MTOPEN    A non-negative number indicates that the
                        device/file was successfully opened. This
                        number must be used in successive calls to the
                        MTIO routines. A negative number denotes one of
                        the following errors:
                        -1  system error.
                        -3  tape unit already open.
                        -5  tape unit not present.
              DEVICE    Name of device to be opened. MTOPEN will compare
                        this name with devices listed in gip_loc/mtdevices.
                        If not matched (case insensitive match) it will
                        assume that the name is a directory or a
                        environment variable which contains the name of a
                        directory. If the device name is a question mark
                        (?), optionally followed by a keyword and optionally
                        a message, MTOPEN will prompt the user (with MTDEVICE=
                        or the specified keyword) to enter the tape device.
                        The user can then get a list of all available tape
                        units. This is the preferred way of prompting for a
                        tape device.

Warning:      Code is System dependent!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C interface:

@ integer function mtopen( character )

*/

fint mtopen_c( fchar tapedevice )
{
   char		*dev;
   char		*deviceb;
   fchar	device;
   fint		idev = -1;
   fint		nret;
   int		dens;
   int		len;
   int		n;
   int		nf;
   int		port;
   int		remote;

   deviceb = malloc( MAXDEVNAMLEN+1 );
   device.a = deviceb;
   device.l = MAXDEVNAMLEN;
   inidev( );
   len = nelc_c( tapedevice );
   if (tapedevice.a[0] == '?' || !len) {
      fchar	keyword;
      fchar	message;
      fint	nc;

      if (len > 1) {
         fint	l = 1;

         keyword.a = &tapedevice.a[l];
         while (l < len && tapedevice.a[l] != '=') l++;
         keyword.l = l;
         if (++l != len) {
            message.a = &tapedevice.a[l];
            message.l = len - l;
         } else {
            message = MESSAGE;
         }
      } else {
         keyword = KEYWORD;
         message = MESSAGE;
      }
      do {
         fint	input_level = 1;
         fint	one = 1;

         nc = userchar_c( device ,
                          &one ,
                          &input_level ,
                          keyword ,
                          message );
         if (!nc) {
            fint	output_level = 3;

            anyoutf( 1, "Tape Device          Description" );
            for (n = 0; n < ndevs; n++) {
               char	string[1024];

               sprintf( string, "%-20.20s %-50.50s", devs[n].synonym, devs[n].comment );
               anyout_c( &output_level, tofchar( string ) );
            }
            anyoutf( 1, "<directory name>     Directory containing Tape file" );
            reject_c( keyword, tofchar( "Here's your list!" ) );
         }
      } while (!nc);
   } else {
      for (n = 0; n < tapedevice.l && n < device.l; n++) {
         device.a[n] = tapedevice.a[n];
      }
      while (n < device.l) device.a[n++] = ' ';
   }
   len = nelc_c( device );
   deviceb[len] = 0;
   for (n = 0; n < ndevs && idev == -1; n++) {
      char	*p1 = deviceb;
      char	*p2 = devs[n].synonym;
      int	m = 0;

      while (toupper( p1[m] ) == p2[m] && p1[m]) m++;
      if (p1[m] == p2[m]) {
         idev = n;
         strcpy( deviceb, devs[n].synonym );
      }
   }
   if (idev == -1) {
      dens = 0;
      remote = 0;
      port = 0;
   } else {
      dens = devs[idev].density;
      remote = devs[idev].remote;
      port = devs[idev].port;
   }
   if (idev == -1) {
      dev = getenv( deviceb );
      if (dev == NULL) {
         dev = deviceb;
      }
#if	defined(__unix__)
      if (!strncmp( "/dev/", dev, 5 )) {
         dens = 1;
      }
#endif
   } else {
      dev = deviceb;
   }
   for (nf = 1, n = 0; nf && n < mt_devs; n++) {
      if (mt_info[n].open) nf = strcmp( mt_info[n].dev, dev );
   }
   if (nf) {                                   /* look for free space */
      for (n = 0; n < mt_devs && mt_info[n].open; n++);
      if (n == mt_devs) {
         nret = mt_devs++;
         mt_info = realloc( mt_info, sizeof( mt_struct ) * mt_devs );
      } else {
         nret = n;
      }
      mt_info[nret].dens = dens;
      mt_info[nret].open = 0;
      mt_info[nret].dev = calloc( strlen( dev ) + 1, sizeof( char ) );
      strcpy( mt_info[nret].dev, dev );
      if (idev == -1) {
         mt_info[nret].device = dev;
      } else {
         mt_info[nret].device = devs[idev].devicename;
      }
      mt_info[nret].devdir = NULL;
      mt_info[nret].remote = remote;
      mt_info[nret].port   = port;
      if (!remote) {
         mt_info[nret].host = NULL;
      } else {
         mt_info[nret].host = devs[idev].remotehost;
      }
      nret = mtio_open( nret );                 /* try to open device */
   } else {
      nret = MTIO_ALREADY_OPEN;
   }
   if (nret < 0) free( deviceb );
   return( nret );
}


/*
#>            mtclose.dc2

Function:     MTCLOSE

Purpose:      Closes a specified tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTCLOSE( MTID )      Input        INTEGER

              MTCLOSE   Returns:
                         0: Operation was completed successfully.
                        -1: System error.
                        -4: Device not open.
              MTID      Id returned by MTOPEN.

Warning:      Code is System dependent!

Updates:      Mar 30, 1990 : KGB Document created.

#<

Fortran to C interface:

@ integer function mtclose( integer )

*/

fint mtclose_c( fint *mtid )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      return( mtio_close( *mtid ) );
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtrew.dc2

Function:     MTREW

Purpose:      Rewinds a specified tape unit.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTREW( MTID )       Input       INTEGER

              MTREW    Returns:
                        0 : Operation was completed successfully.
                       -1 : System error.
                       -4 : Tape unit not open.
              MTID     Device id returned by MTOPEN.

Warning:      Code is System dependent!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C interface:

@ integer function mtrew( integer )

*/

fint mtrew_c( fint *mtid )                   /* rewinds a tape device */
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      return( mtio_rew( *mtid ) );
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtread.dc2

Function:     MTREAD

Purpose:      Reads data from a specified tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTREAD( MTID,       Input        INTEGER
                              DATA,       Output       INTEGER ARRAY
                              NREQ )      Input        INTEGER

              MTREAD    Returns:
                        >0 : Size of record just read in bytes. This
                             may be larger than NREQ. The DATA array is
                             filled with MIN(MTREAD,NREQ) bytes.
                        -1 : System error.
                        -2 : End of tape encountered.
                        -4 : Tape device not open.
                        -8 : Record longer than 32768 bytes.
                        -9 : Call error.
              MTID      Tape device id returned by MTOPEN.
              DATA      Integer array where data are returned.
              NREQ      Wanted number of bytes to read.

Warning:      Code is System dependant!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C inerface:

@ integer function mtread( integer, integer, integer )

*/


fint mtread_c( fint *mtid, char *buffer, fint *size )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      if (*size > 0) {
         return( mtio_read( *mtid, buffer, *size ) );
      } else {
         return( MTIO_CALL_ERROR );
      }
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtreadc.dc2

Function:     MTREADC

Purpose:      Reads data from a specified tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTREADC( MTID,       Input        INTEGER
                               DATA )      Output       CHARACTER*(*)

              MTREADC   Returns:
                        >0 : Size of record just read in bytes. This
                             may be larger than LEN(DATA). The DATA array
                             is filled with MIN(MTREAD,LEN(DATA)) bytes.
                        -1 : System error.
                        -4 : Tape device not open.
                        -9 : Call error.
              MTID      Tape device id returned by MTOPEN.
              DATA      Character variable. The number of bytes MTREADC
                        tries to read is equal to LEN(DATA).

Warning:      Code is System dependant!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C inerface:

@ integer function mtreadc( integer, character )

*/

fint mtreadc_c( fint *mtid, fchar buffer )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      if (buffer.l > 0) {
         return( mtio_read( *mtid, buffer.a, buffer.l ) );
      } else {
         return( MTIO_CALL_ERROR );
      }
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtwrite.dc2

Function:     MTWRITE

Purpose:      Writes data to a specified tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTWRITE( MTID,       Input      INTEGER
                               DATA,       Input      INTEGER ARRAY
                               NREQ )      Input      INTEGER

              MTWRITE   Returns:
                        >0 : Number of bytes written.
                        -1 : System error.
                        -4 : Device not open.
                        -9 : Call error.
                        -10: Write protected.
              MTID      Tape device id returned by MTOPEN.
              DATA      Integer array containing data to be written.
              NREQ      Wanted number of bytes to write.

Warning:      Code is System dependant!

Updates:      Jul 20, 1989: KGB, Document created.

#<

Fortran to C interface:

@ integer function mtwrite( integer, integer, integer )

*/


fint mtwrite_c( fint *mtid, char *buffer, fint *size )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      if (*size > 0) {
         return( mtio_write( *mtid, buffer, *size ) );
      } else {
         return( MTIO_CALL_ERROR );
      }
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtwritec.dc2

Function:     MTWRITEC

Purpose:      Writes data to a specified tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTWRITEC( MTID,       Input        INTEGER
                                DATA )      Input        CHARACTER*(*)

              MTWRITEC  Returns:
                        >0 : Number of bytes actually written.
                        -1 : System error.
                        -4 : Tape device not open.
                        -9 : Call error.
                        -10: Write protected.
              MTID      Tape device id returned by MTOPEN.
              DATA      Character variable. The number of bytes MTWRITEC
                        tries to write is equal to LEN(DATA).

Warning:      Code is System dependant!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C inerface:

@ integer function mtwritec( integer, character )

*/

fint mtwritec_c( fint *mtid, fchar buffer )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      if (buffer.l > 0) {
         return( mtio_write( *mtid, buffer.a, buffer.l ) );
      } else {
         return( MTIO_CALL_ERROR );
      }
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtfsf.dc2

Function:     MTFSF

Purpose:      Forward spaces tape marks on a specified tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTFSF( MTID,          Input         INTEGER
                             NFIL )         Input         INTEGER

              MTFSF     Returns:
                        >0 : Number of files skipped.
                        -1 : System error.
                        -4 : Tape device not open.
                        -9 : Call error.
              MTID      Device id returned by MTOPEN.
              NFIL      Number of files to skip forward.

Warning:      Code is System dependent!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C interface:

@ integer function mtfsf( integer, integer )

*/


fint mtfsf_c( fint *mtid, fint *count )
{
   if (legal( *mtid) && mt_info[*mtid].open) {
      if (*count > 0) {
         return( mtio_fsf( *mtid, *count ) );
      } else if (*count < 0) {
         return( MTIO_CALL_ERROR );
      } else {
         return( 0 );
      }
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtbsf.dc2

Function:     MTBSF

Purpose:      Backward spaces tape marks on a specified tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTBSF( MTID,          Input         INTEGER
                             NFIL )         Input         INTEGER

              MTBSF     Returns:
                        >0 : Number of files skipped backwards.
                        -1 : System error.
                        -4 : Tape device not open.
                        -9 : Call error.
              MTID      Tape device id returned by MTOPEN.
              NFIL      Number of files to skip backward.

Warning:      Code is System dependent!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C interface:

@ integer function mtbsf( integer, integer )

*/


fint mtbsf_c( fint *mtid, fint *count )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      if (*count > 0) {
         return( mtio_bsf( *mtid, *count ) );
      } else if (*count < 0) {
         return( MTIO_CALL_ERROR );
      } else {
         return( 0 );
      }
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtfsr.dc2

Function:     MTFSR

Purpose:      Forward space record on a specified tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTFSR( MTID,          Input         INTEGER
                             NREC )         Input         INTEGER

              MTFSR     Returns:
                        >0 : Number of records skipped forwards. If
                             MTFSR < NREC, a tape mark was encountered.
                             The tape is position after that tape mark.
                        -1 : System error.
                        -4 : Tape device not open.
                        -9 : Call error.
              MTID      Tape device id returned by MTOPEN.
              NREC      Number of records to skip forward.

Warning:      Code is System dependent!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C interface:

@ integer function mtfsr( integer, integer )

*/


fint mtfsr_c( fint *mtid, fint *count )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      if (*count > 0) {
         return( mtio_fsr( *mtid, *count ) );
      } else if (*count < 0) {
         return( MTIO_CALL_ERROR );
      } else {
         return( 0 );
      }
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtbsr.dc2

Function:     MTBSR

Purpose:      Backward space record on a specified tape unit.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTBSR( MTID,          Input         INTEGER
                             NREC )         Input         INTEGER

              MTBSR     Returns:
                        >0 : Number of records skipped backwards. If
                             MTBSR < NREC, a tape mark was encountered.
                             The tape is position before that tape mark.
                        -1 : System error.
                        -4 : Tape device not open.
                        -9 : call error.
              MTID      Tape device id returned by MTOPEN.
              NREC      Number of records to skip backward.

Warning:      Code is System dependent!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C interface:

@ integer function mtbsr( integer, integer )

*/


fint mtbsr_c( fint *mtid, fint *count )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      if (*count > 0) {
         return( mtio_bsr( *mtid, *count ) );
      } else if (*count < 0) {
         return( MTIO_CALL_ERROR );
      } else {
         return( 0 );
      }
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtweof.dc2

Function:     MTWEOF

Purpose:      Writes tape marks to a specified tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTWEOF( MTID ,         Input         INTEGER
                              NOEF )         Input         INTEGER

              MTWEOF    Returns:
                        >0 : Number of Tape Marks written.
                        -1 : System error.
                        -4 : Tape device not open.
                        -9 : Call error.
              MTID      Tape device id returned by MTOPEN.
              NEOF      Number of file marks to write.

Warning:      Code is System dependent!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C interface:

@ integer function mtweof( integer, integer )

*/


fint mtweof_c( fint *mtid, fint *count )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      if (*count > 0) {
         return( mtio_weof( *mtid, *count ) );
      } else if (*count < 0) {
         return( MTIO_CALL_ERROR );
      } else {
         return( 0 );
      }
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtstat.dc2

Function:     MTSTAT

Purpose:      Reports the status of a specified tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTSTAT( MTID )         Input         INTEGER

              MTSTAT    Returns:
                        >0 : Status of device (see below).
                        -1 : System error.
                        -4 : Device not open.
                        -6 : Not implemented.
              MTID      Tape device id returned by MTOPEN.

Note:         The returned status contains the following in formation:
              bit 0   At beginning of tape.
              bit 1   At end of tape mark.
              bit 2   Tape is write enabled.
              bit 3   Tape is online.
              bit 4   Last record read was a tape mark.
              bit 5   Error occurred in last tape operation.

Warning:      Code is System dependent!

Updates:      Jul 20, 1989, KGB, Document created.

#<

Fortran to C interface:

@ integer function mtstat( integer )

*/


fint mtstat_c( fint *mtid )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      return( mtio_stat( *mtid ) );
   } else {
      return( MTIO_NOT_OPENED );
   }
}


/*
#>            mtname.dc2

Function:     MTNAME

Purpose:      Returns name (synonym) of opened tape device.

Category:     TAPES

File:         mtiodev.c

Author:       K.G. Begeman

Use:          INTEGER MTNAME( MTID ,         Input         INTEGER
                              NAME )         Output        CHARACTER*(*)

              MTNAME    Returns:
                        >0 : MTID
                        -4 : Device not open.
              MTID      Tape device id returned by MTOPEN.
              NAME      Synonym of tape device.

Warning:      Code is System dependent!

Updates:      Oct 21, 1991; KGB, Document created.

#<

Fortran to C interface:

@ integer function mtname( integer, character )

*/


fint mtname_c( fint *mtid, fchar name )
{
   if (legal( *mtid ) && mt_info[*mtid].open) {
      fint	l;

      for (l = 0; l < name.l && mt_info[*mtid].dev[l]; l++) {
         name.a[l] = mt_info[*mtid].dev[l];
      }
      while (l < name.l) name.a[l++] = ' ';
      return( *mtid );
   } else {
      return( MTIO_NOT_OPENED );
   }
}

#if defined(TESTBED)

#include	"cmain.h"
#ifdef	NOHERMES
static	void	init_c( void ) { }
static	void	finis_c( void ) { exit( 0 ); }
#else
#include	"init.h"
#include	"finis.h"
#endif

MAIN_PROGRAM_ENTRY
{
   char line[MAXDEVNAMLEN];
   fint mtid;
   fint nel;
   fint nfil;
   fint nrec;
   fint r, r2 = 0;
   fint	count;
   fint	test;

   init_c( );
   mtid = mtopen_c( tofchar( "?INTAPE=Test Tape Device [list of all devices]" ) );
   anyoutf( 0, "mtopen  : %2d", mtid );
   if (mtid < 0) finis_c( );
   nel = userfint( &test, 1, 0, "TESTMODE=", "Which test mode?" );
   switch( test ) {
      case 1: {
         anyoutf( 0, "Standard Test" );
         r = mtrew_c( &mtid );
         anyoutf( 0, "mtrew   : %d", r );
         for (nfil = 0; nfil < 10; nfil++) {
            for (nrec = 0; nrec < 10; nrec++) {
               sprintf( line, "File number %02d Record number %02d!", nfil+1, nrec+1 );
               nel = strlen( line );
               r = mtwrite_c( &mtid, line, &nel );
               anyoutf( 0, "mtwrite : %2d", r );
               if ( r == -10) {
                  r = mtclose_c( &mtid );
                  anyoutf( 0, "mtclose : %2d", r );
                  finis_c( );
                  return( EXIT_FAILURE );
               }
            }
            nel = 1;
            r = mtweof_c( &mtid, &nel );
            anyoutf( 0, "mtweof  : %2d", r );
         }
         nel = 1;
         r = mtweof_c( &mtid, &nel );
         anyoutf( 0, "mtweof  : %2d", r );
         r = mtrew_c( &mtid );
         anyoutf( 0, "mtrew   : %2d", r );
         nel = 3;
         r = mtfsf_c( &mtid, &nel );
         anyoutf( 0, "mtfsf   : %2d", r );
         nel = 80;
         for (nrec = 0; nrec < 10; nrec++) {
            (void) memset( line, 0, nel );
            r = mtread_c( &mtid, line, &nel );
            if (r >= 0) line[r] = 0;
            anyoutf( 0, "mtread  : %2d %s", r, line );
         }
         nel = 5;
         r = mtbsr_c( &mtid, &nel );
         anyoutf( 0, "mtbsr   : %2d", r );
         nel = 31;
         for (nrec = 0; nrec < 10; nrec++) {
            (void) memset( line, 0, 80 );
            r = mtread_c( &mtid, line, &nel );
            line[nel] = 0;
            anyoutf( 0, "mtread  : %2d %s", r, line );
         }
         for (nrec = 0; nrec < 3; nrec++) {
            (void) memset( line, 0, 80 );
            nel = 80;
            r = mtread_c( &mtid, line, &nel );
            if (r >= 0) line[r] = 0;
            anyoutf( 0, "mtread  : %2d %s", r, line );
            nel = 1;
            r = mtbsr_c( &mtid, &nel );
            anyoutf( 0, "mtbsr   : %2d", r );
         }
         nel = 10;
         r = mtfsr_c( &mtid, &nel );
         anyoutf( 0, "mtfsr   : %2d", r );
         nel = 80;
         (void) memset( line, 0, 80 );
         r = mtread_c( &mtid, line, &nel );
         if (r >= 0) line[r] = 0;
         anyoutf( 0, "mtread  : %2d %s", r, line );
         nel = 1;
         r = mtbsf_c( &mtid, &nel );
         anyoutf( 0, "mtbsf   : %2d", r );
         nel = 10;
         r = mtbsr_c( &mtid, &nel );
         anyoutf( 0, "mtbsr   : %2d", r );
         nel = 80;
         (void) memset( line, 0, 80 );
         r = mtread_c( &mtid, line, &nel );
         if (r >= 0) line[r] = 0;
         anyoutf( 0, "mtread  : %2d %s", r, line );
         nel = 80;
         (void) memset( line, 0, 80 );
         r = mtread_c( &mtid, line, &nel );
         if (r >= 0) line[r] = 0;
         anyoutf( 0, "mtread  : %2d %s", r, line );
         nel = 100;
         r = mtbsr_c( &mtid, &nel );
         anyoutf( 0, "mtbsr   : %2d", r );
         nel = 80;
         (void) memset( line, 0, 80 );
         r = mtread_c( &mtid, line, &nel );
         if (r >= 0) line[r] = 0;
         anyoutf( 0, "mtread  : %2d %s", r, line );
         nel = 80;
         (void) memset( line, 0, 80 );
         r = mtread_c( &mtid, line, &nel );
         if (r >= 0) line[r] = 0;
         anyoutf( 0, "mtread  : %2d %s", r, line );
         nel = 1;
         r = mtfsf_c( &mtid, &nel );
         anyoutf( 0, "mtfsf   : %2d", r );
         nel = 1;
         r = mtbsf_c( &mtid, &nel );
         anyoutf( 0, "mtbsf   : %2d", r );
         nel = 1;
         r = mtbsr_c( &mtid, &nel );
         anyoutf( 0, "mtbsr   : %2d", r );
         nel = 80;
         (void) memset( line, 0, 80 );
         r = mtread_c( &mtid, line, &nel );
         if (r >= 0) line[r] = 0;
         anyoutf( 0, "mtread  : %2d %s", r, line );
         nel = 80;
         (void) memset( line, 0, 80 );
         r = mtread_c( &mtid, line, &nel );
         if (r >= 0) line[r] = 0;
         anyoutf( 0, "mtread  : %2d %s", r, line );
         break;
      }
      case 2: {
         anyoutf( 0, "Data Append Test!" );
         r = mtrew_c( &mtid );
         anyoutf( 0, "mtrew   : %d", r );
         nfil = 0;
         nel = 1;
         while (mtfsf_c( &mtid, &nel ) == 1 && mtread_c( &mtid, line, &nel ) > 0 ) {
            nfil++;
         }
         nfil++;
         r = mtbsf_c( &mtid, &nel );
         anyoutf( 0, "mtbsf   : %d", r );
         anyoutf( 0, "Skipped %d files", nfil );
         for (nrec = 0; nrec < 10; nrec++) {
            sprintf( line, "File number %02d Record number %02d!", nfil+1, nrec+1 );
            nel = strlen( line );
            r = mtwrite_c( &mtid, line, &nel );
            anyoutf( 0, "mtwrite : %2d", r );
         }
         nel = 2;
         r = mtweof_c( &mtid, &nel );
         anyoutf( 0, "mtweof  : %2d", r );
         break;
      }
      case 3: {
         anyoutf( 0, "Data Append/Overwrite Test!" );
         nrec = 0;
         for (nfil = 0; nfil < 10; nfil++) {
            sprintf( line, "File number %02d Record number %02d!", nfil+1, nrec+1 );
            nel = strlen( line );
            r = mtwrite_c( &mtid, line, &nel );
            anyoutf( 0, "mtwrite : %2d", r );
            nel = 1;
            r = mtweof_c( &mtid, &nel );
            anyoutf( 0, "mtweof  : %2d", r );
         }
         nel = 1;
         r = mtweof_c( &mtid, &nel );
         anyoutf( 0, "mtweof  : %2d", r );
         break;
      }
      case 4: {
         anyoutf( 0, "Backwards into BOT test!" );
         r = mtrew_c( &mtid );
         anyoutf( 0, "mtrew   : %2d", r );
         nel = 5;
         r = mtfsf_c( &mtid, &nel );
         anyoutf( 0, "mtfsf   : %2d", r );
         nel = 100;
         r = mtbsf_c( &mtid, &nel );
         anyoutf( 0, "mtbsf   : %2d", r );
         nel = 5;
         r = mtfsr_c( &mtid, &nel );
         anyoutf( 0, "mtfsr   : %2d", r );
         nel = 1000;
         r = mtbsr_c( &mtid, &nel );
         anyoutf( 0, "mtbsr   : %2d", r );
         break;
      }
      case 5: {
         anyoutf( 0, "Forwards into EOT/EOD test!" );
         r = mtrew_c( &mtid );
         anyoutf( 0, "mtrew   : %d", r );
         nel = 1000;
         r = mtfsf_c( &mtid, &nel );
         anyoutf( 0, "mtfsf   : %d", r );
         break;
      }
      case 6: {
         anyoutf( 0, "Append test!" );
         count = 0;
         nel = 1;
         r = mtbsf_c( &mtid, &nel );
         anyoutf( 0, "mtbsf   : %2d", r );
         if ( r == 0 ) {
            anyoutf( 0, "Tape at BOT" );
         } else {
            count -= 1;
         }
         nel = 1;
         while ( ( ( r = mtfsf_c( &mtid, &nel ) ) == 1 ) && ( ( r2 = mtread_c( &mtid, line, &nel ) ) > 0 ) ) {
            count++;
            anyoutf( 0, "mtfsf   : %2d", r );
            anyoutf( 0, "mtread  : %2d", r2 );
         }
         if ( ( r == 1 ) && ( r2 == 0 || r2 == MTIO_END_OF_TAPE ) ) {
            anyoutf( 0, "Skipped %d files forward", ++count );
            r = mtbsf_c( &mtid, &nel );
            anyoutf( 0, "mtbsf   : %2d", r );
            for (nrec = 0; nrec < 10; nrec++) {
               sprintf( line, "File number %02d Record number %02d!", count+1, nrec+1 );
               nel = strlen( line );
               r = mtwrite_c( &mtid, line, &nel );
               anyoutf( 0, "mtwrite : %2d", r );
            }
            nel = 1;
            r = mtweof_c( &mtid, &nel );
            anyoutf( 0, "mtweof  : %2d", r );
            nel = 1;
            r = mtweof_c( &mtid, &nel );
            anyoutf( 0, "mtweof  : %2d", r );
         } else {
            anyoutf( 0, "mtfsf   : %2d", r );
            anyoutf( 0, "mtread  : %2d", r2 );
         }
         break;
      }
      case 7: {
         anyoutf( 0, "Read test" );
         nel = sizeof( line ) - 1 ;
         do {
            r = mtread_c( &mtid, line, &nel );
            anyoutf( 0, "mtread  : %2d", r );
         } while ( r >= 0 );
         break;
      }
      default: {
         anyoutf( 0, "No test!" );
         break;
      }
   }
   r = mtrew_c( &mtid );
   anyoutf( 0, "mtrew   : %2d", r );
   r = mtclose_c( &mtid );
   anyoutf( 0, "mtclose : %2d", r );
   finis_c( );
   return( EXIT_SUCCESS );
}
#endif
