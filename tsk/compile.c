/* compile.c

        Copyright (c) Kapteyn Laboratorium Groningen 1991
        All Rights Reserved.

#>            compile.doc

Document:     compile

Purpose:      Describes how to compile/install a procedure/application
              program, how to install a document or include file,
              how to (un)reserve a source file with the program
              compile.

Category:     MANAGEMENT

File:         compile.c

Author:       K.G. Begeman

Use:          $gip_exe/compile [switches] [source file(s)]

              The following switches are recognized by the program:

              -#          discard default switch #.
              -c          compile sources.
              -cc #       # indicates the name of the c compiler to
                          be used instead of the default.
              -check      checks the GIPSY tree for strange and missing
                          files.
              -copts      defines the following switches to be compiler
                          switches.
              -delete     deletes a source file.
              -export #   compares the history files in $gip_sys and
                          #/sys, and copies new/modified sources to #.
              -fc #       # indicates the name of the fortran compiler
                          to be used instead of the default.
              -if #       # is the name of a file which contains
                          names of source files (one per line).
              -import #   compares the history files in # and $gip_sys,
                          and copies new/modified sources to $gip_sys.
              -install    install new version(s) of source file(s).
              -lopts      defines the following switches to be loader
                          switches.
              -o #        # denotes the name of the executable to
                          be produced.
              -purify     uses purify to build/link tasks.
              -rebuild    rebuilds procedures and applications.
              -reserve    reserves a source for the user.
              -retrieve   retrieves an older version of a source file
                          (-version switch must also be used).
              -sub        same as -c.
              -sysgen     used only when generating the system.
              -unreserve  unreserves a source file reserved by the user.
              -update     updates out of date procedures (and
                          applications if -c or -sub not used).
              -version #  # indicates the version number of a source file
                          for retrieval or reinstall (only with
                          -retrieve switch).
              -X          (private use) adds X11 compiler and/or linker
                          options to the compiler command.
                          If switch is found in the first line of a c-source
                          file, it had the same effect for privileged use.
              -XT         (private use) adds X-Tookit compiler and/or linker
                          options to the compiler command.
                          If switch is found in the first line of a c-source
                          file, it has the same effect for privileged use.

              The following file extensions are recognized:

              .c          c source code.
              .col        cola scripts.
              .csh        csh shell script.
              .dc0        level 0 documentation.
              .dc1        level 1 documentation.
              .dc2        level 2 documentation.
              .dc3        level 3 documentation.
              .h          c include file.
              .f          fortran source code.
              .make       make file.
              .mem        memo's.
              .mgr        management files.
              .nws        file contains GIPSY news.
              .o          object file.
              .py         Python script
              .rep        report files.
              .sh         sh shell scripts
              .shl        sheltran source code.
              .src        a set of other known source files.
              .tex        document in tex/latex format.

              While running make, the following variables are defined:
              ARCHITECTURE   type of machine (i.e. alliant, sun4)
              CC_COMP        name of c compiler
              CC_OPTS        default c compiler options
              CC_LIBS        default libraries for GIPSY c programmes
              FC_COMP        name of fortran compiler
              FC_OPTS        default fortran compiler options
              FC_LIBS        default libraries for GIPSY fortran programmes
              X11_OPTS       default X11 compiler options (includes)
              X11_LIBS       default X11 linker options (library)
              XT_LIBS        default X-Toolkit linker options (libraries)

              The sources will be directed to the following directories:

              Extension           Directory
              .c .f .shl .src     gip_sub (function), gip_tsk (application)
              .col                gip_tsk
              .csh .sh .mgr .py   gip_sys
              .dc0 .doc .tex      gip_doc
              .dc1                gip_tsk
              .dc2 .dc3           gip_sub
              .mem .nws .rep      gip_mis

              Most sources need documentation:

              Extension           Documentation (one of)
              .c .f .shl .src     .dc1 or .doc for applications
              .c .f .shl .src     .dc2 or .dc3 for functions
              .col                .dc1
              .csh .sh .mgr .py   .doc
              .h                  .dc2 .dc3
              .tex                .doc

              The files extracted while installing a (new) source will be
              directed to the following directories:

              Extension           Directory
              .dc0 .doc .tex      gip_doc
              .dc1                gip_tsk
              .dc2 .dc3           gip_sub
              .h                  gip_inc (if parent is a function)
              .csh .sh            gip_sys

Updates:      May 23, 1991: KGB Document created.
              Mar 22, 1993: KGB .rep and .sh extensions added.
              Jan  3, 1995: KGB -export and -import options added.
              Mar 22, 1996: KGB length of strings increased.
              Oct  9, 1996: KGB switch -X implemented.
              Nov 25, 1996: KGB switch -purify implemented.
              May 12, 1997: KGB check for new server implemented.
              Jun 22, 1997: KGB check for -X in first source line.
              Jul 29, 1997: KGB check for -XT in first source line.
              Nov  9, 1998: JPT doubled MAXCHILD and MAXSOURCE to 2048.
              Feb  1, 1999: JPT declared big array static and reduced MAXCHILD
                                and MAXSOURCE (due to Linux static limit)
              Dec 12, 2000: JPT for Linux: private version of system()
              Mar  9, 2001: JPT changed "thales" into "gipsy".
              May  4, 2007: JPT included conditional code for Apple Mac.
              Nov 24, 2008: JPT deactivated strip command.
              Oct  7, 2010: JPT allow for code generated in relation to -fPIC
              Jun  8, 2012: JPT .py extension added

#<

*/

#include	"ctype.h"			/* <ctype.h> */
#include	"errno.h"			/* <errno.h> */
#include	"signal.h"			/* <signal.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */
#include	"time.h"			/* <time.h> */
#include	"xscanf.h"			/* code for xscanf */

#if	defined(__GNUC__) && defined(_CONVEX_SOURCE)
#define	fgetpwent(x)	fgetpwent(FILE *)
#endif

/*
 * The following routines might be defined in the system include files.
 * So we redefine them.
 */

#define	access		ACCESS
#define	chdir		CHDIR
#define	chmod		CHMOD
#define	fcntl		FCNTL
#define	ftruncate	FTRUNCATE
#define	getcwd		GETCWD
#define	getpwuid	GETPWUID
#define	open		OPEN
#define	sleep		SLEEP
#define	umask		UMASK

#include	<pwd.h>				/* pwd definitions */
#include	<fcntl.h>			/* fcntl definitions */
#include	<sys/file.h>			/* file control */
#include	<sys/stat.h>
#include	<sys/types.h>			/* unix types */
#ifndef	F_OK
#include	<unistd.h>
#endif
#include	<sys/socket.h>
#include	<netinet/in.h>
#include	<netdb.h>
#if	defined(__bsd__)
#include        <sys/dir.h>
#else
#include        <dirent.h>
#endif

#if	!defined(ntohl) && !defined(__alpha__)
extern	u_long	ntohl( );
#endif

#if	defined(__alliant__)
#else
#include	<sys/utsname.h>			/* for hostinfo */
#endif

#undef	access
#undef	chdir
#undef	chmod
#undef	fcntl
#undef	ftruncate
#undef	getcwd
#undef	getpwuid
#undef	open
#undef	sleep
#undef	umask


/*
 * The following lines of code try to determine the machine architecture.
 * If OS_ARCHITECTURE is not defined, this code should not compile and
 * the system manager should modify the osdef.h include file.
 */

static	char	*architecture = OS_ARCHITECTURE;/* set the achitecture */
static	char	*gipsyrelease = "Release 3.6";	/* GIPSY release */

#define	LONG_AGE	172800.0		/* two days */
#define	SHORT_AGE	1800.0			/* 30 minutes */

#define	INSTALL_MODE	0x00000001		/* install a source */
#define	UPDATE_MODE	0x00000002		/* update (sources) */
#define	REBUILD_MODE	0x00000004		/* rebuild (sources) */
#define	RESERVE_MODE	0x00000008		/* reserve a source */
#define	UNRESERVE_MODE	0x00000010		/* unreserve a source */
#define	DELETE_MODE	0x00000020		/* delete a source */
#define	RETRIEVE_MODE	0x00000040		/* retrieve a source */
#define	SUBROUTINE_MODE	0x00000080		/* a function */
#define	OUTPUT_MODE	0x00000100		/* output name */
#define	VERSION_MODE	0x00000200		/* version */
#define	HISTORY_MODE	0x00000400		/* show history */
#define	COMPILE_MODE	0x00000800		/* compile something */
#define	PRIVATE_MODE	0x00001000		/* you! */
#define	MASTER_MODE	0x00002000		/* the manager */
#define	MAIL_MODE	0x00004000		/* from mail */
#define	NOSORT_MODE	0x00008000		/* don't sort */
#define	SYSGEN_MODE	0x00010000		/* generate system */
#define	CHECK_MODE	0x00020000		/* check system */
#define	RETRY_MODE	0x00040000		/* retry compiling */
#define	UNLOCK_MODE	0x00080000		/* unlock */
#define	EXPORT_MODE	0x00100000		/* export */
#define	IMPORT_MODE	0x00200000		/* import */
#define	PURIFY_MODE	0x00400000		/* purify */

#define	FLAG_SYM	0			/* gipsy symbols defined */
#define	FLAG_EXE	1			/* gipsy may run */
#define	FLAG_COM	2			/* gipsy user compile */
#define	FLAG_INS	4			/* gipsy user install */
#define	FLAG_SUI	8			/* compile may not run suid */
#define	FLAG_LCK	16			/* gipsy uses own locking */
#define	FLAG_OLD	32			/* gipsy does not save old executables */
#define	FLAG_SLV	64			/* compile runs as a slave */
#define	FLAG_FTP	128			/* gipsy obtains source from server */
#define	FLAG_LNK	256			/* has symbolic links */

#define	FATAL		0xffffffff		/* fatal error */

#define	UTYPE_COL	1			/* cola scripts */
#define	UTYPE_SYS	2			/* system scripts */
#define	UTYPE_MIS	3			/* miscellaneous */
#define	UTYPE_DOC	4			/* documentation */
#define	UTYPE_INC	5			/* include file */
#define	UTYPE_SUB	6			/* subroutine */
#define	UTYPE_TSK	7			/* application */

#define	FTYPE_BUG	0x00000001		/* bug report */
#define	FTYPE_C		0x00000002		/* c source file */
#define	FTYPE_COL	0x00000004		/* cola script */
#define	FTYPE_CSH	0x00000008		/* csh script */
#define	FTYPE_DC0	0x00000010		/* dc0 document */
#define	FTYPE_DC1	0x00000020		/* dc1 document */
#define	FTYPE_DC2	0x00000040		/* dc2 document */
#define	FTYPE_DC3	0x00000080		/* dc3 document */
#define	FTYPE_DOC	0x00000100		/* doc document */
#define	FTYPE_EXE       0x00000200		/* executable */
#define	FTYPE_F		0x00000400		/* fortran source file */
#define	FTYPE_FIX	0x00000800		/* fix report */
#define	FTYPE_FTOC	0x00001000		/* interface routine */
#define	FTYPE_H		0x00002000		/* c include file */
#define	FTYPE_L		0x00004000		/* lex files */
#define	FTYPE_LNK	0x00008000		/* is a symbolic link */
#define	FTYPE_MAKE	0x00010000		/* make file */
#define	FTYPE_MEM	0x00020000		/* memo's */
#define	FTYPE_MGR	0x00040000		/* Gipsy news file */
#define	FTYPE_NWS	0x00080000		/* news file */
#define	FTYPE_O		0x00100000		/* object file */
#define	FTYPE_OPT	0x00200000		/* options file */
#define	FTYPE_REP	0x00400000		/* report file */
#define	FTYPE_S		0x00800000		/* assembler source file */
#define	FTYPE_SH	0x01000000		/* Bourne shell script */
#define	FTYPE_SHL	0x02000000		/* sheltran source file */
#define	FTYPE_SRC	0x04000000		/* packed source file */
#define	FTYPE_SYN	0x08000000		/* synonym file */
#define FTYPE_TEX       0x10000000		/* tex document */
#define FTYPE_PY        0x20000000		/* Python script */
#define	FTYPE_UNKNOWN	0x40000000		/* ????? */

#define	MAXARG		64			/* max. # or arguments */
#define	MAXARCH		20			/* max. # of architectures */
#define	MAXCHAR		128			/* length of strings */
#define	MAXCHILD	1500			/* maximum number of children */
#define	MAXCMD		4096			/* length of command */
#define	MAXINC		64			/* number of .h files */
#define	MAXLINE		256			/* length of line */
#define	MAXPROG		100			/* maximum number of programmers */
#define	MAXSOURCE	1500			/* maximum number of sources */

#define	werr(f)	{ if (f < 0) { fprintf( stderr, "%s -- Write error!\a\n", program ); return( FATAL ); }}

#define	BOF_MARK	"Begin_Of_File"		/* denotes begin of file */
#define	EOF_MARK	"End_Of_File"		/* denotes end of file */

#define	CHMOD_SRC	00644			/* for sources */
#define	CHMOD_EXE	00755			/* for executables */
#define	CHMOD_SUI	06755			/* for compile */

static	char	*nochecklist[] = {		/* do NOT check for documents */
   "xclib.c"					/* documented in K&R */
};

#define	NOCHECKLIST	(sizeof(nochecklist)/sizeof(char *))

static	char	*nodocchecklist[] = {		/* do NOT check for author names */
   "thelp.dc1"					/* helpfile for thermes */
};

#define	NODOCCHECKLIST	(sizeof(nodocchecklist)/sizeof(char *))

static	char	*nosupportlist[] = {		/* these executables ar not supported */
   "xhermes"					/* xhermes */
};

#define	NOSUPPORTLIST	(sizeof(nosupportlist)/sizeof(char *))

typedef struct {
   char	user[MAXCHAR];				/* logname of programmer */
   char	addr[MAXCHAR];				/* mail address of programmer */
   char	name[MAXCHAR];			 	/* full name of programmer */
   char	last[MAXCHAR];				/* last name */
} prog_struct;

static	prog_struct	prog[MAXPROG];		/* list of known programmers */
static	int		nprog = 0;		/* number of known programmers */

static	char	*utypes[] = {
   "",						/* dummy */
   "col",					/* cola files */
   "sys",					/* system files */
   "mis",					/* miscelleneous */
   "doc",					/* documents */
   "inc",					/* include files */
   "sub",					/* routines */
   "tsk"					/* applications */
};

#define	MAXUTYPES	(sizeof(utypes)/sizeof(char *))

typedef struct {
   char		module[MAXCHAR+1];		/* module name */
} nm_struct;

typedef struct off_struct {
   char		fname[MAXCHAR];			/* name of file */
   char		lname[MAXCHAR];			/* name of symbolic link */
   int		ftype;				/* type of file */
   char		*path;				/* path to destination */
   int		status;				/* status */
} offspring;

typedef struct {
   char		fname[MAXCHAR+1];		/* points to argument */
   char		fpath[FILENAME_MAX+1];		/* real path */
   char		uname[MAXCHAR+1];		/* owner of source */
   char		uname_old[MAXCHAR+1];		/* previous owner */
   char		*upath;				/* here it goes */
   char		*rpath;				/* remote path */
   int		done;				/* already handled */
   int		ftype;				/* type of file */
   int		mode;				/* which mode */
   int		nchild;				/* number of children */
   int		size;				/* size of source */
   int		status;				/* status */
   int		utype;				/* this is the type */
   int		version;			/* version number of source */
   int		version_old;			/* previous version */
   offspring	*child;				/* any offspring */
} parent;

static	int	With_x11 = 0;			/* default */
static	char	ar_add[MAXCMD+1];		/* add to archive */
static	char	ar_del[MAXCMD+1];		/* remove from archive */
static	char	as_cmd[MAXCMD+1];		/* assembler */
static	char	cc_name[MAXCMD+1];		/* name default c compiler */
static	char	cc_opts1[MAXCMD+1];		/* install options c compiler */
static	char	cc_opts2[MAXCMD+1];		/* private c compiler options */
static	char	cc_libs[MAXCMD+1];		/* libs for c main */
static	char	chost[64];			/* name of current host */
static	char	cwd[FILENAME_MAX+1];		/* current working directory */
static	char	exp_dir[FILENAME_MAX+1];	/* export to this directory */
static	char	fc_name[MAXCMD+1];		/* name default fortran compiler */
static	char	fc_opts1[MAXCMD+1];		/* install options fortran compiler */
static	char	fc_opts2[MAXCMD+1];		/* private fortran compiler options */
static	char	fc_libs[MAXCMD+1];		/* libs for fortran main */
static	char	gfs_addr[MAXCHAR+1];		/* gipsy file server */
static	char	gfs_mail[MAXCHAR+1];		/* e-mail address */
static	char	gip_arch[MAXCHAR+1];		/* architecture of client */
static	char	gip_exe[FILENAME_MAX+1];	/* executables */
static	char	gip_doc[FILENAME_MAX+1];	/* document sources */
static	char	gip_inc[FILENAME_MAX+1];	/* include files */
static	char	gip_lib[FILENAME_MAX+1];	/* library */
static	char	gip_loc[FILENAME_MAX+1];	/* local setup files */
static	char	gip_mis[FILENAME_MAX+1];	/* miscellaneous files */
static	char	gip_old[FILENAME_MAX+1];	/* old versions */
static	char	gip_sub[FILENAME_MAX+1];	/* subroutine sources */
static	char	gip_sys[FILENAME_MAX+1];	/* system directory */
static	char	gip_tmp[FILENAME_MAX+1];	/* temporary directory */
static	char	gip_tsk[FILENAME_MAX+1];	/* application sources */
static	char	gip_root[FILENAME_MAX+1];	/* gipsy root directory */
static	char	imp_dir[FILENAME_MAX+1];	/* import from this directory */
static	char	nm_cmd[MAXCMD+1];		/* nm command */
static	char	x11_opts[MAXCMD+1];		/* x11 includes */
static	char	x11_libs[MAXCMD+1];		/* x11 libraries */
static	char	xt_libs[MAXCMD+1];		/* x11 Toolkit libraries */
static	char	rem_adm[FILENAME_MAX+1];	/* path to remote adm */
static	char	rem_doc[FILENAME_MAX+1];	/* path to remote doc */
static	char	rem_inc[FILENAME_MAX+1];	/* path to remote inc */
static	char	rem_mis[FILENAME_MAX+1];	/* path to remote mis */
static	char	rem_sub[FILENAME_MAX+1];	/* path to remte sub */
static	char	rem_sys[FILENAME_MAX+1];	/* path to remote sys */
static	char	rem_tsk[FILENAME_MAX+1];	/* path to remote tsk */
static	char	rem_root[FILENAME_MAX+1];	/* path to remote root */
static	char	*cc = NULL;			/* name of C compiler */
static	char	*fc = NULL;			/* name of Fortran compiler */
static	char	*output = NULL;			/* name of output file */
static	char	*program;			/* name of program */
static	char	uobjects[MAXCMD+1];		/* the objects */
static	char	ranlib[MAXCMD+1];		/* makes library out of archive */
static	char	usermail[MAXCHAR+1];		/* mail address of user */
static	char	username[MAXCHAR+1];		/* username */
static	char	mgrname[MAXCHAR+1];		/* full name of manager */
static	char	mgrmail[MAXCHAR+1];		/* mail address of manager */
static	char	mgrlogn[MAXCHAR+1];		/* log name of manager */
static	char	lmgrmail[MAXCHAR+1];		/* mail address local manager */
static	int	ar_len;				/* max. length of archived file */
static	int	intcount = 0;			/* interrupt counter */
static	int	sflag = 0;			/* what may we do */
static	int	no_update = 0;			/* do update */
static	int	version = -1;			/* version number */
static	nm_struct	dummy1[MAXSOURCE];	/* dummy 1 */
static	nm_struct	dummy2[MAXSOURCE];	/* dummy 2 */
static	parent		dummy3[MAXSOURCE];	/* dummy 3 */
static	char	*checkdirs[] = {
   gip_doc,
   gip_exe,
   gip_inc,
   gip_mis,
   gip_sub,
   gip_sys,
   gip_tsk,
};

#define	MAXCHECKDIRS	(sizeof(checkdirs)/sizeof(char *))

static	char	*nocheckinsys[] = {
   "COPYRIGHT",
   "README",
   "XHermes",
   "bookkeeper",
   "grfont.txt",
   "history",
   "meudon.c",
   "mnh.c",
   "offspring",
   "pgpack.f"
};

#define	MAXNOCHECKINSYS	(sizeof(nocheckinsys)/sizeof(char *))


/*
 * Get the unix file descriptor from ANSI-C FILE struct.
 */

#ifndef	fileno
#if	defined(__hp9000s300__)
#if	OS_MAJOR_VERSION < 9
#define	fileno(p)	((p)->__file)
#else
extern	int	fileno(FILE *);
#endif
#elif	defined(__hp9000s700__)
extern	int	fileno(FILE *);
#elif	defined(__linux__)
extern	int	fileno(FILE *);
#elif	defined(__APPLE__)
extern	int	fileno(FILE *);
#else
#define	fileno(p)	((p)->_file)
#endif
#endif

extern	int		access( char *path, int amode );
extern	int		chdir( char *path );
extern	int		chmod( char *path, int mode );
extern	int		fcntl( int fd, int cmd, int val );
extern	int		ftruncate( int fd, off_t length );
#if	defined(__sysv__) | defined(__linux__) | defined(__APPLE__)
extern	char		*getcwd( char *path, int size );
#endif
extern	uid_t		geteuid( void );
#if	defined(__bsd__)
extern	int		gethostname( char *host, int size);
#endif
extern	char		*getlogin( void );
extern	uid_t		getuid( void );
#if	defined(__bsd__)
extern	char		*getwd( char *path );
#endif
extern	int		gip_lock( const char *path );
extern	int		gip_unlock( const char *path );
#if	defined(__hpux__)
#define	seteuid( euid )	setuid( euid )
#endif
extern	int		pclose( FILE *stream );
extern	FILE		*popen( const char *command, const char *type );
extern	int		seteuid( uid_t euid );
extern	int		setruid( uid_t ruid );
extern	int		setuid( uid_t uid );
extern	unsigned int	sleep( unsigned int seconds );

static	uid_t	euid;				/* effective uid */
static	uid_t	guid;				/* uid of gipsy account */
static	uid_t	ruid;				/* real uid */

#if defined(__linux__) | defined(__APPLE__)
/* Copyright (C) 1991, 1992 Free Software Foundation, Inc.
 *
 * Code modified for use in the GIPSY program "compile" only.
 * 
 * In the child branch the real uid is set to the effective uid in order
 * to prevent the called shell to assume the original real uid again.
 *
 */

#include <sys/wait.h>

extern int errno;
extern char **environ;

#define	SHELL_PATH	"/bin/sh"	/* Path of the shell.  */
#define	SHELL_NAME	"sh"		/* Name to give it.  */

/* Execute LINE as a shell command, returning its status.  */
int system(const char *line)
{
   
  int execve(const char *path, char *const argv[], char *const envp[]);
  int setreuid(uid_t ruid, uid_t euid);
  pid_t fork(void);
  
  int status;
  pid_t pid;

  if (line == NULL)
    return 1;

  pid = fork ();
  if (pid == (pid_t) 0)
    {
      /* Child side.  */
      const char *new_argv[4];
      new_argv[0] = SHELL_NAME;
      new_argv[1] = "-c";
      new_argv[2] = line;
      new_argv[3] = NULL;
      
      signal(SIGINT, SIG_DFL);
      setreuid(geteuid(), geteuid());

      /* Exec the shell.  */
      (void)execve(SHELL_PATH, (char *const *) new_argv, environ);
      exit(127);
    }
  else if (pid < (pid_t) 0)
    /* The fork failed.  */
    status = -1;
  else
    /* Parent side.  */
    if (waitpid (pid, &status, 0) != pid)
      status = -1;

  return status;
}
#endif


/*
 * handler handles interrupt signals.
 */

static	void	handler( int sig )
{
   if (sig == SIGINT) {				/* it was an interrupt */
      intcount++;				/* increase interrupt counter */
      signal( sig, handler );			/* set handler again */
   }
}


/*
 * mkpath puts directory + filename together.
 */

static	void	mkpath( char *path, char *dir, char *name )
{
   int	n = 0;

   if (dir != NULL) {
      while (*dir) { path[n++] = *dir++; };
      if (n && path[n-1] != '/') {
         path[n++] = '/';
      }
   }
   if (name != NULL) {
      while (*name) { path[n++] = *name++; };
   }
   path[n] = 0;
}


/*
 * Check server checks the file $gip_loc/server whether it is configured
 * for the old server, and modified it for the new server , if needed.
 */

static	void	check_server( void )
{
   FILE	*s;
   char	name[MAXCHAR];

   mkpath( name, gip_loc, "server" );
   s = fopen( name, "r+" );
   if ( s != NULL ) {
      char	f1[MAXCHAR], f2[MAXCHAR], f3[MAXCHAR];

      if ( xscanf( s, "%s %s %s", f1, f2, f3 ) == 3 ) {
         int	modified = 0;

         if ( !strcmp( f1, "129.125.6.204" ) ) {
            modified++;
            strcpy( f1, "129.125.6.224" );
         }
         if ( !strcmp( f1, "kapteyn.astro.rug.nl" ) ) {
            modified++;
            strcpy( f1, "gipsy.astro.rug.nl" );
         }
         if ( !strcmp( f1, "kapteyn" ) ) {
            modified++;
            strcpy( f1, "gipsy" );
         }
         if ( !strcmp( f2, "gipsy@[129.125.6.204]" ) ) {
            modified++;
            strcpy( f2, "gipsy@[129.125.6.224]" );
         }
         if ( !strcmp( f2, "gipsy@kapteyn.astro.rug.nl" ) ) {
            modified++;
            strcpy( f2, "gipsy@astro.rug.nl" );
         }
         if ( !strcmp( f3, "/dj3/users/gipsy" ) ) {
            modified++;
            strcpy( f3, "/tha3/users/gipsy" );
         }
         if ( !strcmp( f3, "/dj3/users/gipsy/" ) ) {
            modified++;
            strcpy( f3, "/tha3/users/gipsy" );
         }
         if ( modified ) {
            char	text[MAXCHAR+1];
            time_t	now = time( NULL );	/* current time */

            fseek( s, 0, SEEK_SET );		/* set to begin-of-file */
            strftime( text, MAXCHAR, "# server (modified %b %d, %Y)", localtime( &now ) );
            fprintf( s, "%s\n%s:%s:%s\n", text, f1, f2,f3 );
            fprintf( stderr, "%s -- WARNING!! modified %s!\a\n", program, name );
         }
      }
      fclose( s );
   }
}


/*
 * fileage returns the time in seconds since the last modification
 * of a file.
 */

static	double	fileage( char *filename )
{
   double	r = -1.0;
   struct stat	buf;

   if (!stat( filename, &buf )) {
      time_t	mtime;
      time_t	ntime;

      mtime = buf.st_mtime;
      ntime = time( NULL );
      r = difftime( ntime, mtime );
   }
   return( r );
}


/*
 * getguid obtains the uid of the GIPSY account. Usually this is the owner
 * of the GIPSY history file.
 */

static	uid_t	getguid( void )
{
   char		path[FILENAME_MAX+1];
   struct stat	buf;

   mkpath( path, gip_sys, "history" );
   if (stat( path, &buf )) {
      fprintf( stderr, "%s -- cannot stat %s!\a\n", program, path );
      exit( EXIT_FAILURE );
   }
   return( buf.st_uid );
}


/*
 * getmanager reads $gip_sys/manager.mgr
 */

static	int	getmanager( void )
{
   FILE	*f;
   char	filename[FILENAME_MAX+1];

   if (mgrname[0]) return( 0 );			/* already done */
   mkpath( filename, gip_sys, "manager.mgr" );
   f = fopen( filename, "r" );
   if (f == NULL) {
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename );
      return( -1 );
   }
   if (xscanf( f, "%s %s %s", mgrname, mgrmail, mgrlogn ) != 3) {
      fprintf( stderr, "%s -- Error reading %s!\a\n", program, filename );
      mgrname[0] = mgrmail[0] = mgrlogn[0] = 0;
      return( -1 );
   }
   fclose( f );
   return( 0 );
}


/*
 * getuser obtains the name of the user.
 */

static	void	getuser( char *username )
{
   char	*getlogin( );				/* gets login name */
   char	*uname;					/* points to login name */

   uname = getlogin( );				/* obtain login name */
   if (uname == NULL) {				/* no success */
      struct passwd	*pw, *getpwuid( );	/* from /etc/passwd */

      pw = getpwuid( getuid( ) );		/* get pw struct */
      uname = pw->pw_name;			/* get login name */
   }
   strcpy( username, uname );			/* copy user name */
}


/*
 * createlockfile exclusively creates a file and writes the name of
 * the host in it.
 */

static	void	createlockfile( char *filename )
{
   char	bugger[sizeof(chost)];			/* name of locker */
   int	close( );				/* close */
   int	count = 0;				/* loop counter */
   int	ld;					/* lockfile fd */
   int	open( );				/* open */
   int	read( );				/* read */
   int	write( );				/* write */

   while ((ld = open( filename, O_CREAT | O_EXCL | O_RDWR, 0644 )) == -1 && errno == EEXIST) {
      ld = open( filename, O_RDONLY );		/* open for read */
      if (ld != -1) {				/* no error */
         int	nr;				/* number of bytes read */

         nr = read( ld, bugger, sizeof( bugger ) - 1 );
         if (nr == -1) {			/* error reading */
            strcpy( bugger, "?" );
         } else {				/* okay */
            bugger[nr] = '\0';
         }
         close( ld );				/* close it */
         if (fileage( filename ) > SHORT_AGE) {
            fprintf( stderr, "%s -- lockfile %s timed out!\a\n", program, filename );
         } else if (!count) {
            fprintf( stderr, "%s -- %s already created by %s!\a\n", program, filename, bugger );
            count++;
         }
      }
      sleep( 60 );				/* wait 60 seconds */
   }
   if (ld != -1) {				/* no error */
      write( ld, chost, strlen( chost ) );	/* put name of host */
      close( ld );				/* close it */
   } else {
      char	*errmes;			/* error message */

      errmes = strerror( errno );		/* get message */
      if (errmes == NULL) errmes = "Unknown error";
      fprintf( stderr, "%s -- Error creating %s (%s)!\a\n", program, filename, errmes );
   }
}


/*
 * fopenl opens a file for exclusive access by this program. Arguments
 * and return as in fopen.
 */

static	FILE	*fopenl( const char *filename, const char *mode )
{
   FILE	*r = NULL;				/* return */
   char	lfilename[FILENAME_MAX+1];		/* lockfile name */

   sprintf( lfilename, "%s.lock", filename );	/* name of lock file */
   if (!(sflag & FLAG_LCK)) {			/* use rpc.lockd */
      int		fd;			/* file descriptor */
      struct flock	setlock;		/* the lock struct */

      r = fopen( filename, mode );		/* open file */
      if ( r == NULL ) return( r );		/* file was NOT opened */
      fd = fileno( r );				/* get file descriptor */
      setlock.l_type = F_WRLCK;			/* apply exclusive lock */
      setlock.l_whence = 0;			/* from beginning */
      setlock.l_start = 0;			/* start here */
      setlock.l_len = 0;			/* to the end */
      while (fcntl( fd, F_SETLKW, (int)&setlock ) == -1 && errno == EINTR);
      createlockfile( lfilename );		/* make lock file */
   } else {					/* use lckserver */
      if ( gip_lock( filename ) ) {
         fprintf( stderr, "%s -- error locking %s!\a\n", program, filename );
         exit( EXIT_FAILURE );
      }
      createlockfile( lfilename );		/* make lock file */
      r = fopen( filename, mode );		/* open file */
   }
   return( r );					/* return to caller */
}


/*
 * fclosel closes a file which was exclusively locked by this program.
 */

static	int	fclosel( FILE *stream, const char *filename )
{
   char	lfilename[FILENAME_MAX+1];
   int	r;

   sprintf( lfilename, "%s.lock", filename );	/* lock file name */
   fflush( stream );				/* flush it */
   if (!(sflag & FLAG_LCK)) {
      int		fd = fileno( stream );	/* file descriptor */
      struct flock	setlock;		/* lock struct */

      setlock.l_type = F_UNLCK;			/* unlock */
      setlock.l_whence = 0;			/* from beginning */
      setlock.l_start = 0;			/* start here */
      setlock.l_len = 0;			/* to the end */
      remove( lfilename );			/* remove lock file */
      while (fcntl( fd, F_SETLKW, (int)&setlock ) == -1 && errno == EINTR);
      r = fclose( stream );			/* close file */
   } else {
      r = fclose( stream );			/* close file */
      remove( lfilename );			/* remove lock file */
      if ( gip_unlock( filename ) ) {		/* remove lock */
         fprintf( stderr, "%s -- error unlocking %s!\a\n", program, filename );
         exit( EXIT_FAILURE );
      }
   }
   return( r );					/* return to caller */
}


/*
 * getlocalmanager gets the mail address of the local GIPSY manager.
 */

static	void	getlocalmanager( void )
{
   FILE	*mf;
   char	filename[FILENAME_MAX+1];
   char	mhost[sizeof(chost)];
   char	mmail[MAXCHAR+1];

   mkpath( filename, gip_loc, "manager" );
   lmgrmail[0] = '\0';
   mf = fopen( filename, "r" );
   if ( mf != NULL ) {
      int	nf;

      while ( ( nf = xscanf( mf, "%s %s", mhost, mmail ) ) == 2 && strcmp( chost, mhost ) );
      if ( nf == 2 ) {
         strcpy( lmgrmail, mmail );
      }
      fclose( mf );
   }
   if (lmgrmail[0] == '\0') {
      strcpy( lmgrmail, username );
   }
}


/*
 * initprog reads the file $gip_sys/programmers.mgr and stores its
 * contents in the prog_struct. It returns 0 on succes, 1 when the
 * file could not be opened, 2 when internal buffer is too small.
 */

static	int	initprog( void )
{
   FILE	*f;
   char	user[MAXCHAR+1];
   char	name[MAXCHAR+1];
   char	addr[MAXCHAR+1];
   char	filename[FILENAME_MAX+1];
   int	nf = 0;

   if (nprog) return( 0 );
   mkpath( filename, gip_sys, "programmers.mgr" );
   f = fopen( filename, "r" );
   if (f == NULL) {
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename );
      return( 1 );
   }
   while (nprog < MAXPROG && (nf = xscanf( f, "%s %s %s", user, addr, name )) == 3) {
      int	n;

      strcpy( prog[nprog].user, user );
      strcpy( prog[nprog].addr, addr );
      strcpy( prog[nprog].name, name );
      n = strlen( name );
      while ( n && isalnum( name[n-1] ) ) n--;
      strcpy( prog[nprog].last, &name[n] );
      nprog += 1;
   }
   fclose( f );
   if (nf != EOF) {
      fprintf( stderr, "%s -- prog buffer too small!\a\n", program );
      return( 2 );
   }
   return( 0 );
}


/*
 * compar1 compares types of source files. It is called by qsort to sort the
 * source types. compar1 returns < 0 if a1 < a2, 0 if a1 == a2 and > 0
 * if a1 > a2.
 */

static	int	compar1( const void *a1, const void *a2 )
{
   const parent	*b1 = a1;			/* assign */
   const parent	*b2 = a2;			/* assign */

   if (b1->utype == b2->utype) {
      if ((b1->mode & DELETE_MODE) && !(b2->mode & DELETE_MODE)) {
         return( -1 );
      } else if (!(b1->mode & DELETE_MODE) && (b2->mode & DELETE_MODE)) {
         return( 1 );
      } else {
         return( strcmp( b1->fname, b2->fname ) );
      }
   } else {
      return( b1->utype - b2->utype );		/* return to caller */
   }
}


/*
 * compar2 compares module names. It is called by qsort to sort the
 * module names.
 */

static	int	compar2( const void *a1, const void *a2 )
{
   const nm_struct	*b1 = a1;		/* assign */
   const nm_struct	*b2 = a2;		/* assign */

   return( strcmp( b1->module, b2->module ) );	/* return to caller */
}


/*
 * unpack unpacks a mailed file.
 */

static	int	unpack( const char *file )
{
   FILE	*f1;
   FILE	*f2 = NULL;
   char	filename[FILENAME_MAX+1];
   char	line[MAXLINE+1];
   int	lbof = strlen( BOF_MARK );
   int	leof = strlen( EOF_MARK );
   int	r = 0;
   int	size1 = 0, size2 = 0;

   f1 = fopen( file, "r" );
   if (f1 == NULL) {
      fprintf( stderr, "%s -- (unpack) cannot open %s!\a\n", program, file );
      return( FATAL );
   }
   while (fgets( line, MAXLINE, f1 ) != NULL) {
      if (!strncmp( line, BOF_MARK, lbof )) {
         if (f2 == NULL) {
            char	*name;
            char	*size;

            name = strtok( line, " \n" );
            name = strtok( NULL, " \n" );
            strcpy( filename, name );
            size = strtok( NULL, " \n" );
            if (size != NULL) size1 = atoi( size ); else size1 = 0;
            size2 = 0;
            f2 = fopen( filename, "w" );
            if (f2 == NULL) {
               fprintf( stderr, "%s -- cannot create %s!\a\n", program, filename );
            } else {
               fprintf( stdout, "Unpacking %s\n", filename );
            }
         }
      } else if (!strncmp( line, EOF_MARK, leof )) {
         if (f2 != NULL) {
            fclose( f2 );
            f2 = NULL;
            if (size1) {
               if (size1 != size2) {
                  fprintf( stderr, "%s -- incorrect size of %s!\a\n", program, filename );
                  remove( filename );
                  r += 1;
               }
            }
         }
      } else if (f2 != NULL) {
         size2 += fprintf( f2, "%s", line );
      }
   }
   if (f2 != NULL) {
      fclose( f2 );
      fprintf( stderr, "%s -- Error in packed file!\a\n", program );
      remove( filename );
      r += 1;
   }
   return( r );
}


/*
 * nbits counts the number of bits in a 32-bit word which are set. This
 * only works on architectures where a int is 4 8-bit bytes.
 */

static	int	nbits( unsigned int word)
{
   word = ((word & 0xAAAAAAAA)>> 1) + (word & 0x55555555);
   word = ((word & 0xCCCCCCCC)>> 2) + (word & 0x33333333);
   word = ((word & 0xF0F0F0F0)>> 4) + (word & 0x0F0F0F0F);
   word = ((word & 0xFF00FF00)>> 8) + (word & 0x00FF00FF);
   word = ((word & 0xFFFF0000)>>16) + (word & 0x0000FFFF);
   return( word );				/* return to caller */
}

#if	0

/*
 * getchecksum computes the BSD checksum of a file.
 */

static	int	getchecksum( const char *filename )
{
   FILE	*f;
   int	c;
   int	r = 0;

   f = fopen( filename, "rb" );
   if ( f == NULL ) return( -1 );
   while ( (c = fgetc( f ) ) != EOF ) {
      if ( r & 1 ) {
         r = ( r >> 1 ) + 0x8000;
      } else {
         r >>= 1;
      }
      r += c;
      r &= 0xffff;
   }
   if ( ferror( f ) ) r = -2;
   fclose( f );
   return( r );
}
#endif


/*
 * getfilesize determines the size of the file in bytes.
 */

static	int	getfilesize( const char *filename )
{
   FILE	*f;
   int	r = -1;

   f = fopen( filename, "rb" );
   if (f == NULL) {
      return( r );
   }
   fseek( f, 0, SEEK_END );
   r = ftell( f );
   fclose( f );
   return( r );
}


/*
 * putfilesize puts the size of the file in a hidden file in the same
 * directory with name .'filename'size so that remote updates can check
 * the size of the file. The number of bytes is 10 (%9d\n).
 */

static	void	putfilesize( char *filename )
{
   char	f[FILENAME_MAX+1];			/* file name */
   int  len;					/* length of filename */
   int	size;					/* size of history file */

   strcpy( f, filename );			/* copy filename */
   len = strlen( f );				/* length of filename */
   while ( len && f[len] != '/' ) {		/* loop */
      f[len+1] = f[len]; len--;			/* shift upward */
   }
   f[len+1] = '.';				/* hidden file */
   strcat( f, "size" );				/* append 'size' */
   size = getfilesize( filename );		/* size of file */
   if ( size >= 0 ) {
      FILE	*s;

      s = fopenl( f, "w" );
      if ( s != NULL ) {
         fprintf( s, "%9d\n", size );
         fclosel( s, f );
      }
   }
}


/*
 * docmd does the unix command. It returns FATAL when the command could
 * not be executed, otherwize the status of the command is returned.
 */

static	int	docmd( char *cmd )
{
   int	r;					/* return from call to system */

   if (intcount) return( FATAL );		/* interrupted */
   fprintf( stdout, "%s\n", cmd );		/* show command */
   r = system( cmd );				/* system call */
   return( r );					/* return to caller */
}


/*
 * cp copies a file.
 */

static	int	cp( char *source, char *destination )
{
   char	cmd[MAXCMD+1];				/* the command */

   sprintf( cmd, "cp %s %s", source, destination );
   return( docmd( cmd ) );			/* return to caller */
}


/*
 * getfile obtains a file from the gipsy-source-server. It uses gftp
 * to obtain the file. If size is > 0, the filesize of the retrieved
 * file is compared with size. getfile return 0 on success, FATAL on error.
 */

static	int	getfile( char *filename1, char *filename2, int size, int mode )
{
   char	cmd[MAXCMD];
   int	r = 0;
   int	s;

   if (mode & IMPORT_MODE) {
      if ( !access( filename1, F_OK ) ) {
         r = cp( filename1, filename2 );
      } else {
         r = 1;
      }
   } else {
      sprintf( cmd, "%s/gftp get %s %s", gip_exe, filename1, filename2 );
      r = docmd( cmd );
   }
   if (r) {
      remove( filename2 );
      if ( size > -1 ) {
         fprintf( stderr, "%s -- file retrieve error!\a\n", program );
      }
   } else {
      s = getfilesize( filename2 );
      if (size > 0) {
         if (size != s) {
            fprintf( stderr, "%s -- incorrect filesize!\a\n", program );
            r = FATAL;
         }
      } else if (s <= 0 && !size) {
         fprintf( stderr, "%s -- file retrieve error!\a\n", program );
         r = FATAL;
      } else if (s < 0) {
         r = FATAL;
      }
   }
   if (r) remove( filename2 );
   return( r );
}


/*
 * putfile sends a file to the gipsy-source-server. It uses gftp
 * to send the file.
 */

static	int	putfile( char *filename1, char *filename2 )
{
   char	cmd[MAXCMD];
   int	r = 0;

   sprintf( cmd, "%s/gftp put %s", gip_exe, filename1 );
   r = docmd( cmd );
   remove( filename1 );
   return( r );
}


/*
 * ttostr converts the calendar time to string. The formatted time string
 * is written in a static area of which the address is returned.
 */

static	char	*ttostr( time_t tp )
{
   static char	ttostr_b[24];			/* static memory */
   struct tm	*tmp;				/* the time struct */

   tmp = localtime( &tp );			/* local time */
   strftime( ttostr_b, sizeof( ttostr_b ), "%d/%b/%y %H\\:%M\\:%S", tmp );
   return( ttostr_b );				/* return to caller */
}


/*
 * dcp copies a file.
 */

static	int	dcp( FILE *source, FILE *destination )
{
   int	ch;					/* a character */

   fseek( source, 0, SEEK_SET );		/* set to begin-of-file */
   fseek( destination, 0, SEEK_SET );		/* set to begin-of-file */
   ftruncate( fileno( destination ), 0 );	/* truncate output file */
   while ((ch = fgetc( source )) != EOF) {	/* until end-of-file */
      if (fputc( ch, destination ) == EOF) {	/* put out character */
         fprintf( stderr, "%s -- File write error!\a\n", program );
         return( 1 );
      }
   }
   return( 0 );					/* return to caller */
}


/*
 * mv moves a file.
 */

static	int	mv( char *source, char *destination )
{
   char	cmd[MAXCMD+1];				/* the command */

   sprintf( cmd, "mv -f %s %s", source, destination );
   return( docmd( cmd ) );			/* return to caller */
}


/*
 * rm removes a file
 */

static	int	rm( char *filename )
{
   fprintf( stdout, "rm %s\n", filename );	/* show what we intend */
   return( remove( filename ) );		/* return to caller */
}


/*
 * readline reads a line from a file. It returns the number of characters
 * read from the file. The newline is replaced by a zero byte. Trailing
 * blanks are discarded.
 */

static	int	readline( char *line, int maxline,  FILE *f )
{
   int	r = 0;					/* initialize */

   if (!feof(f)) {				/* quit when EOF */
      int	ch;				/* a character */

      while ((ch = fgetc( f )) != EOF) {	/* loop until EOF */
         if (ch == '\n') break;			/* end of line */
         if (r < maxline) line[r++] = ch;	/* store */
      }
      line[r] = 0;				/* add zero byte */
      while (r && line[r-1] == ' ') line[--r] = 0;
   }
   return( r );					/* return to caller */
}


/*
 * copyhead copies the first series of comment lines from file f1
 * to file f2. It alsways returns zero.
 */

static	int	copyhead( FILE *f1, FILE *f2 )
{
   int	ch;					/* a character */

   while ((ch = fgetc( f1 )) == '#') {		/* while comment .. */
      while ( ch != EOF && fputc( ch, f2 ) != '\n' ) ch = fgetc( f1 );
   }
   if (ch != EOF) ungetc( ch, f1 );		/* put last character back */
   return( 0 );					/* return to caller */
}


/*
 * getupath gets the source path.
 */

static	void	getupath( parent *p, int utyp )
{
   p->utype = utyp;
   switch( utyp ) {
      case UTYPE_COL: {
         p->upath = gip_tsk;
         p->rpath = rem_tsk;
         break;
      }
      case UTYPE_DOC: {
         if (p->ftype & (FTYPE_DC1) ) {
            p->upath = gip_tsk;
            p->rpath = rem_tsk;
         } else if (p->ftype & (FTYPE_DC2 | FTYPE_DC3) ) {
            p->upath = gip_sub;
            p->rpath = rem_sub;
         } else {
            p->upath = gip_doc;
            p->rpath = rem_doc;
         }
         break;
      }
      case UTYPE_INC: {
         p->upath = gip_inc;
         p->rpath = rem_inc;
         break;
      }
      case UTYPE_MIS: {
         p->upath = gip_mis;
         p->rpath = rem_mis;
         break;
      }
      case UTYPE_SUB: {
         p->upath = gip_sub;
         p->rpath = rem_sub;
         break;
      }
      case UTYPE_SYS: {
         p->upath = gip_sys;
         p->rpath = rem_sys;
         break;
      }
      case UTYPE_TSK: {
         p->upath = gip_tsk;
         p->rpath = rem_tsk;
         break;
      }
      default: {
         break;
      }
   }
}


/*
 * getutype gets the source type and source path.
 */

static	void	getutype( parent *p )
{
   if (p->ftype & (FTYPE_COL)) {
      p->utype = UTYPE_COL;
      p->upath = gip_tsk;
      p->rpath = rem_tsk;
   } else if (p->ftype & (FTYPE_MEM | FTYPE_NWS | FTYPE_REP)) {
      p->utype = UTYPE_MIS;
      p->upath = gip_mis;
      p->rpath = rem_mis;
   } else if (p->ftype & (FTYPE_CSH | FTYPE_MGR | FTYPE_SH | FTYPE_PY)) {
      p->utype = UTYPE_SYS;
      p->upath = gip_sys;
      p->rpath = rem_sys;
   } else if (p->ftype & (FTYPE_DC0 | FTYPE_DOC | FTYPE_TEX)) {
      p->utype = UTYPE_DOC;
      p->upath = gip_doc;
      p->rpath = rem_doc;
   } else if (p->ftype & (FTYPE_DC1)) {
      p->utype = UTYPE_DOC;
      p->upath = gip_tsk;
      p->rpath = rem_tsk;
   } else if (p->ftype & (FTYPE_DC2 | FTYPE_DC3)) {
      p->utype = UTYPE_DOC;
      p->upath = gip_sub;
      p->rpath = rem_sub;
   } else if (p->ftype & (FTYPE_H)) {
      p->utype = UTYPE_INC;
      p->upath = gip_inc;
      p->rpath = rem_inc;
   } else if (p->ftype & (FTYPE_C | FTYPE_F | FTYPE_SHL | FTYPE_SRC)) {
      if (p->mode & (SUBROUTINE_MODE)) {
         p->utype = UTYPE_SUB;
         p->upath = gip_sub;
         p->rpath = rem_sub;
      } else {
         p->utype = UTYPE_TSK;
         p->upath = gip_tsk;
         p->rpath = rem_tsk;
      }
   }
}


/*
 * getchilddest finds the path of the child.
 */

static  void    getchilddest( parent *p )
{
   int  n;

   for (n = 0; n < p->nchild; n++) {
      int       ftyp = p->child[n].ftype;

      if (ftyp & (FTYPE_DC0 | FTYPE_DOC | FTYPE_TEX)) {
         p->child[n].path = gip_doc;
      } else if (ftyp & (FTYPE_DC1)) {
         p->child[n].path = gip_tsk;
      } else if (ftyp & (FTYPE_DC2 | FTYPE_DC3)) {
         p->child[n].path = gip_sub;
      } else if ((ftyp & FTYPE_H) && (p->utype == UTYPE_SUB)) {
         p->child[n].path = gip_inc;
      } else if (ftyp & (FTYPE_CSH | FTYPE_SH)) {
         p->child[n].path = gip_sys;
      } else if (ftyp & (FTYPE_EXE)) {
         p->child[n].path = gip_exe;
      }
   }
}

/*
 * ftype returns the type of file.
 */

static	int	ftype( char *filename )
{
   int	l;					/* counter */
   int	r = FTYPE_UNKNOWN;			/* default */

   for ( l = strlen( filename ) - 1; l && (filename[l] != '.') && (filename[l] != '/'); l--);
   if (filename[l] == '/') l = 0;		/* no extension */
   if (l) {					/* extension found */
      char	*ext = &filename[l];		/* points to extension */

      if (!strcmp( ext, ".bug" )) {		/* .bug */
         r = FTYPE_BUG;				/* bug report */
      } else if (!strcmp( ext, ".c" )) {	/* .c */
         r = FTYPE_C;				/* c source code */
      } else if (!strcmp( ext, ".col" )) {	/* .col */
         r = FTYPE_COL;				/* cola script */
      } else if (!strcmp( ext, ".csh" )) {	/* .csh */
         r = FTYPE_CSH;				/* csh script */
      } else if (!strcmp( ext, ".dc0" )) {	/* .dc0 */
         r = FTYPE_DC0;				/* dc0 document */
      } else if (!strcmp( ext, ".dc1" )) {	/* .dc1 */
         r = FTYPE_DC1;				/* dc1 document */
      } else if (!strcmp( ext, ".dc2" )) {	/* .dc2 */
         r = FTYPE_DC2;				/* dc2 document */
      } else if (!strcmp( ext, ".dc3" )) {	/* .dc3 */
         r = FTYPE_DC3;				/* dc3 document */
      } else if (!strcmp( ext, ".doc" )) {	/* .doc */
         r = FTYPE_DOC;				/* doc document */
      } else if (!strcmp( ext, ".f" )) {	/* .f */
         r = FTYPE_F;				/* fortran source code */
      } else if (!strcmp( ext, ".fix" )) {	/* .fix */
         r = FTYPE_FIX;				/* fix report */
      } else if (!strcmp( ext, ".h" )) {	/* .h */
         r = FTYPE_H;				/* c include file */
      } else if (!strcmp( ext, ".l" )) {	/* .l */
         r = FTYPE_L;				/* file for lex */
      } else if (!strcmp( ext, ".make" )) {	/* .make */
         r = FTYPE_MAKE;			/* makefile */
      } else if (!strcmp( ext, ".mem" )) {	/* .mem */
         r = FTYPE_MEM;				/* memo */
      } else if (!strcmp( ext, ".mgr" )) {	/* .mgr */
         r = FTYPE_MGR;				/* management file */
      } else if (!strcmp( ext, ".nws" )) {	/* .news */
         r = FTYPE_NWS;				/* GIPSY news file */
      } else if (!strcmp( ext, ".o" )) {	/* .o */
         r = FTYPE_O;				/* object code */
      } else if (!strcmp( ext, ".opt" )) {	/* .opt */
         r = FTYPE_OPT;				/* compiler instructions */
      } else if (!strcmp( ext, ".rep" )) {	/* .rep */
         r = FTYPE_REP;
      } else if (!strcmp( ext, ".py" )) {	/* .py */
         r = FTYPE_PY;
      } else if (!strcmp( ext, ".s" )) {	/* .s */
         r = FTYPE_S;				/* assembler code */
      } else if (!strcmp( ext, ".sh" )) {	/* .sh */
         r = FTYPE_SH;				/* sh script */
      } else if (!strcmp( ext, ".shl" )) {	/* .shl */
         r = FTYPE_SHL;				/* sheltran source code */
      } else if (!strcmp( ext, ".src" )) {	/* .src */
         r = FTYPE_SRC;				/* packed source code */
      } else if (!strcmp( ext, ".syn" )) {	/* .syn */
         r = FTYPE_SYN;				/* synonyms */
      } else if (!strcmp( ext, ".tex" )) {	/* .tex */
         r = FTYPE_TEX;				/* tex document */
      }
   } else {					/* no extension */
      r = FTYPE_EXE;				/* so executable */
   }
   return( r );					/* return to caller */
}


/*
 * get_nm_cmd gets the default nm command to search the objects and
 * libraries for modules.
 */

static	void	get_nm_cmd( void )
{
#if	defined(__hp9000s700__)
   strcpy( nm_cmd, "nm -p" );
#elif	defined(__alpha__) | defined(__sgi__)
   strcpy( nm_cmd, "nm -oB" );
#elif	defined(__aix__)
   strcpy( nm_cmd, "/usr/ucb/nm" );
#elif	defined(__sun__) & defined(__sysv__)
   strcpy( nm_cmd, "nm -hp" );
#else
   strcpy( nm_cmd, "nm" );
#endif
}


/*
 * with_x11 returns true when source needs to be compiled with x11
 */
static	int	with_x11( char *source )
{
   FILE	*S;
   int	r = 0;

   if ( ftype( source ) != FTYPE_C ) return( r );
   if ( With_x11 ) return( With_x11 );
   S = fopen( source, "r" );
   if ( S != NULL ) {
      char	line[MAXLINE+1];

      if ( fgets( line, MAXLINE, S ) != NULL ) {
         char	*p = strstr( line, " -XT" );
         if ( p != NULL ) r = 2; else if ( strstr( line, " -X" ) != NULL ) r = 1;
      }
      fclose( S );
   }
   return( r );
}


/*
 * get_x11_opts finds defaults x11 compiler options.
 */
static	void	get_x11_opts( void )
{
   FILE	*p;
   char	cmd[MAXCMD+1];
   char	xsw[MAXCMD+1];

   sprintf( cmd, "%s/findx.sh includes", gip_sys );
   p = popen( cmd, "r" );
   if (p == NULL) return;
   while (fscanf( p, "%s", xsw ) == 1) {
      int	l;

      l = strlen( x11_opts );
      if (l && x11_opts[l-1] != ' ') strcat( x11_opts, " " );
      strcat( x11_opts, xsw );
   }
   pclose( p );
}


/*
 * get_x11_libs finds defaults x11 linker options.
 */
static	void	get_x11_libs( void )
{
   FILE	*p;
   char	cmd[MAXCMD+1];
   char	xsw[MAXCMD+1];

   sprintf( cmd, "%s/findx.sh library", gip_sys );
   p = popen( cmd, "r" );
   if (p == NULL) return;
   while ( fscanf( p, "%s", xsw ) == 1 ) {
      int	l;

      l = strlen( x11_libs );
      if (l && x11_libs[l-1] != ' ') strcat( x11_libs, " " );
      strcat( x11_libs, xsw );
   }
   pclose( p );
}


/*
 * get_x11_libs finds defaults x11 toolkit linker options.
 */
static	void	get_xt_libs( void )
{
   FILE	*p;
   char	cmd[MAXCMD+1];
   char	xsw[MAXCMD+1];

   sprintf( cmd, "%s/findx.sh libraries", gip_sys );
   p = popen( cmd, "r" );
   if (p == NULL) return;
   while ( fscanf( p, "%s", xsw ) == 1 ) {
      int	l;

      l = strlen( xt_libs );
      if (l && xt_libs[l-1] != ' ') strcat( xt_libs, " " );
      strcat( xt_libs, xsw );
   }
   pclose( p );
}


/*
 * fname returns pointer to name of file. The extension is removed.
 */

static	char	*fname( char *filename )
{
   static char	buf[MAXCHAR+1];			/* static memory */
   int		k, l;				/* counters */

   for ( l = strlen( filename ) - 1; l && filename[l] != '.'; l-- );
   for ( k = 0; k < l; k++ ) buf[k] = filename[k];
   buf[k] = 0;					/* add zero byte */
   return( buf );				/* return to caller */
}


/*
 * help displays the document of this program.
 */

static	int	help( void )
{
   char	cmd[MAXCMD+1];

   sprintf( cmd, "more %s/compile.doc", gip_doc );
   return( docmd( cmd ) );
}


/*
 * client checks whether the current host is a known GIPSY client.
 * GIPSY clients of the same architecture must all have the same
 * operation system, and the same compiler and loader version.
 * This is not checked by compile!!.
 * This routine opens the file $gip_loc/clients and checks whether the
 * current host is in the list. It returns 0 when current host is a
 * client, otherwize FATAL is returned.
 */

static	int	client( void )
{
   FILE	*f;					/* file descriptor */
   char	entry[MAXCHAR+1];			/* name from file */
   char	name[FILENAME_MAX+1];			/* file name */
   int	r;					/* return value */

   strcpy( name, gip_loc );			/* copy first part */
   strcat( name, "/clients" );			/* copy second part */
   f = fopen( name, "r" );			/* open file */
   if (f == NULL) {				/* cannot open file */
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, name );
      return( FATAL );				/* exit with error */
   }
   while ((r = xscanf( f, "%s %s %d", entry, gip_arch, &sflag )) == 6 && strcmp( chost, entry ));
   fclose( f );					/* close file */
   if ( r == 6 ) {				/* o.k. so far */
      if ( !strcmp( gip_arch, architecture ) ) {
         r = 0;
      } else {
         int	l = strlen( architecture );

         r = FATAL;
         if ( strlen( gip_arch ) > l ) {
            if ( !strncmp( gip_arch, architecture, l ) ) {
               if ( gip_arch[l] == '_' ) r = 0;
            }
         }
      }
   } else {
      r = FATAL;				/* not o.k. */
   }
   return( r );
}


/*
 * setup_remote reads the file $gip_sys/server.mgr for the addresses
 * and paths of the gipsy source server.
 */

static	int	setup_remote( int mode )
{
   FILE		*f;
   char		filename[FILENAME_MAX+1];

   if ((sflag & FLAG_FTP)) {
      mkpath( filename, gip_loc, "server" );	/* try local first */
      f = fopen( filename, "r" );		/* open it */
      if (f == NULL) {				/* no local */
         mkpath( filename, gip_sys, "server.mgr" );
         f = fopen( filename, "r" );
         if (f == NULL) {			/* error */
            fprintf( stderr, "%s -- cannot open %s!\a\n", program, filename );
            return( FATAL );			/* FATAL */
         }
      }
      if (xscanf( f, "%s %s %s", gfs_addr, gfs_mail, rem_root ) != 3) {
         fprintf( stderr, "%s -- error reading %s!\a\n", program, filename );
         fclose( f );				/* close it */
         return( FATAL );			/* FATAL */
      }
      fclose( f );
   } else if (mode & IMPORT_MODE) {
      strcpy( rem_root, imp_dir );
   } else if (mode & EXPORT_MODE) {
      strcpy( rem_root, exp_dir );
   } else {
      return( FATAL );
   }
   mkpath( rem_adm, rem_root, "adm" );		/* path to remote adm */
   mkpath( rem_doc, rem_root, "doc" );		/* path to remote doc */
   mkpath( rem_inc, rem_root, "inc" );		/* path to remote inc */
   mkpath( rem_mis, rem_root, "mis" );		/* path to remote mis */
   mkpath( rem_sub, rem_root, "sub" );		/* path to remote sub */
   mkpath( rem_sys, rem_root, "sys" );		/* path to remote sys */
   mkpath( rem_tsk, rem_root, "tsk" );		/* path to remote tsk */
   return( 0 );
}


/*
 * setup reads the GIPSY setup file. This file contains information
 * about compilers, loaders, archivers etc. It first tries
 * $gip_loc/setup.hostname. If the file is not found or there is no entry
 * for the current architecture, it tries $gip_loc/setup.
 */

static	int	setup( void )
{
   FILE	*f;					/* file descriptor */
   char	arch[MAXCHAR+1];			/* architecture */
   char	name[FILENAME_MAX+1];			/* name of setup file */
   int	r;					/* return from xscanf */

   strcpy( name, gip_loc );			/* copy first part */
   strcat( name, "/setup." );			/* copy second part */
   strcat( name, chost );			/* copy third part */
   f = fopen( name, "r" );			/* open file */
   if (f != NULL) {				/* cannot open file */
      while (((r = xscanf( f, "%s %s %s %s %s %s %s %s %s %s %s %s %d %s %s %s %s %s", arch, cc_name, cc_opts1, cc_opts2, cc_libs, fc_name, fc_opts1, fc_opts2, fc_libs, as_cmd, ar_add, ar_del, &ar_len, ranlib, nm_cmd, x11_opts, x11_libs, xt_libs )) >= 14) && (r <= 18) && strcmp( arch, gip_arch));
      fclose( f );				/* close file */
      if (r >= 14) {				/* nothing found */
         cc = cc_name;				/* name of c compiler */
         fc = fc_name;				/* name of fortran compiler */
         if (r == 14) { get_nm_cmd( ); r++; }	/* use default */
         return( 0 );
      }
   }
   strcpy( name, gip_loc );			/* copy first part */
   strcat( name, "/setup" );			/* copy second part */
   f = fopen( name, "r" );			/* open file */
   if (f == NULL) {				/* cannot open file */
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, name );
      return( FATAL );				/* error */
   }
   while (((r = xscanf( f, "%s %s %s %s %s %s %s %s %s %s %s %s %d %s %s %s %s %s", arch, cc_name, cc_opts1, cc_opts2, cc_libs, fc_name, fc_opts1, fc_opts2, fc_libs, as_cmd, ar_add, ar_del, &ar_len, ranlib, nm_cmd, x11_opts, x11_libs, xt_libs )) >= 14) && (r <= 18) && strcmp( arch, gip_arch));
   fclose( f );					/* close file */
   if ((r < 14) || (r > 18)) {			/* nothing found */
      fprintf( stderr, "%s -- No entry found for %s!\a\n", program, gip_arch );
   } else {
      cc = cc_name;				/* name of c compiler */
      fc = fc_name;				/* name of fortran compiler */
      if (r == 14) { get_nm_cmd( ); r++; }	/* use default */
      if (r == 15) { get_x11_opts( ); r++; }
      if (r == 16) { get_x11_libs( ); r++; } 
      if (r == 17) { get_xt_libs( ); r++; } 
   }
   return( (r != 18) ? FATAL : 0 );		/* return to caller */
}


/*
 * init does the initialization. It translates the GIPSY environment
 * variables, gets the default switches etcetera. The default switches
 * and the default compilers are read from a setup file with name
 * $gip_sys/setup. But first it is checked whether the current host is known
 * to the system. If so, there should be an entry in the file $gip_sys/clients.
 * Furthermore the current working directory is determined. It is assumed
 * that all source files on the command line are in this directory.
 * init returns 0 on success or FATAL on error.
 */

static	int	init( void )
{
   char	*envp;					/* environment pointer */

#if	defined(__bsd__)
   if (gethostname( chost, sizeof(chost) - 1 )) {	/* obtain host name */
      fprintf( stderr, "%s -- cannot obtain name of host!\a\n", program );
      return( FATAL );				/* exit with error */
   }
   chost[sizeof(chost)-1] = '\0';		/* add (extra?) zero byte */
#else
   struct utsname	name;

   if (uname( &name ) == -1) {			/* obtain host name */
      fprintf( stderr, "%s -- cannot obtain name of host!\a\n", program );
      return( FATAL );				/* exit with error */
   }
   if (strlen( name.nodename ) < ( sizeof(chost) - 1 )) {
      strcpy( chost, name.nodename );
   } else {
      fprintf( stderr, "%s -- hostname too long!\a\n", program );
      return( FATAL );
   }
#endif
   envp = getenv( "gip_root" );			/* get $gip_root */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_root!\a\n", program );
      return( FATAL );				/* error */
   }
   strcpy( gip_root, envp );			/* copy string */
   envp = getenv( "gip_doc" );			/* get $gip_doc */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_doc!\a\n", program );
      return( FATAL );				/* error */
   }
   strcpy( gip_doc, envp );			/* copy string */
   envp = getenv( "gip_exe" );			/* get $gip_exe */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_exe!\a\n", program );
      return( FATAL );				/* error */
   }
   strcpy( gip_exe, envp );			/* copy string */
   envp = getenv( "gip_inc" );			/* get $gip_lib */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_inc!\a\n", program );
      return( FATAL );				/* error */
   }
   strcpy( gip_inc, envp );			/* copy string */
   envp = getenv( "gip_lib" );			/* get $gip_lib */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_lib!\a\n", program );
      return( FATAL );				/* error */
   }
   strcpy( gip_lib, envp );			/* copy string */
   envp = getenv( "gip_loc" );			/* get $gip_loc */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_loc!\a\n", program );
      return( FATAL );
   }
   strcpy( gip_loc, envp );			/* copy string */
   envp = getenv( "gip_mis" );			/* get $gip_mis */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_mis!\a\n", program );
      return( FATAL );
   }
   strcpy( gip_mis, envp );			/* copy string */
   envp = getenv( "gip_old" );			/* get $gip_old */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_old!\a\n", program );
      return( FATAL );				/* error */
   }
   strcpy( gip_old, envp );			/* copy string */
   envp = getenv( "gip_sub" );			/* get $gip_sub */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_sub!\a\n", program );
      return( FATAL );				/* error */
   }
   strcpy( gip_sub, envp );			/* copy string */
   envp = getenv( "gip_sys" );			/* get $gip_sys */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_sys!\a\n", program );
      return( FATAL );				/* error */
   }
   strcpy( gip_sys, envp );			/* copy string */
   envp = getenv( "gip_tmp" );			/* get $gip_tmp */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_tmp!\a\n", program );
      return( FATAL );				/* error */
   }
   strcpy( gip_tmp, envp );			/* copy string */
   envp = getenv( "gip_tsk" );			/* get $gip_tsk */
   if (envp == NULL) {				/* no translation */
      fprintf( stderr, "%s -- Cannot translate $gip_tsk!\a\n", program );
      return( FATAL );				/* error */
   }
   strcpy( gip_tsk, envp );			/* copy string */
   if (client( )) {				/* host is no client */
      fprintf( stderr, "%s -- Host is not a GIPSY client!\a\n", program );
      return( FATAL );				/* exit with error */
   }
   if (setup( )) {				/* cannot setup */
      fprintf( stderr, "%s -- Problems with setup file!\a\n", program );
      return( FATAL );				/* exit with error */
   }
   return( 0 );					/* return to caller */
}


/*
 * substitute replaces marked strings (starting with a #) by their
 * synonyms. If a marked string is not found, the synonym is appended
 * to the command.
 */

static	void	substitute( char *out, char *cmd, char *subst )
{
   char	sub[MAXCMD+1];				/* work space */
   char	*p;					/* work pointer */

   strcpy( sub, subst );			/* copy substitution commands */
   strcpy( out, cmd );				/* copy the command string */
   p = strtok( sub, "=" );			/* find first marker */
   while (p != NULL) {				/* until no markers found */
      char	*pp;				/* replace with */
      char	*s = out;			/* pointer to out */
      char	*ss;				/* should be replaced */
      int	done = 0;			/* counts number of substs. */
      int	l1, l2;				/* length counters */

      l1 = strlen( p );				/* length of marker */
      pp = strtok( NULL, "," );			/* substitute string */
      l2 = strlen( pp );			/* length of this string */
      while ( ( ss = strstr( s, p ) ) != NULL) {	/* occurence of marker */
         int	l3 = strlen( ss );		/* left after start of marker */
         char	*qq = pp;			/* copy of pp */

         memmove( &ss[l2], &ss[l1], l3 + 1 );	/* do substitution */
         while (*qq) *ss++ = *qq++;		/* copy substitute */
         s = ss;				/* next part of command */
         done += 1;				/* count */
      }
      if ( !done ) {				/* not found */
         strcat( out, " " );			/* insert space */
         strcat( out, pp );			/* append substitute */
      }
      p = strtok( NULL, "=" );			/* next marker */
   }
}


/*
 * copylib copies the GIPSY library from gip_lib to gip_tmp (dir != 0) or
 * from gip_tmp to gip_lib (dir == 0). It keeps track of where the
 * library is at the moment. When putting it back, it will run ranlib
 * first (if this is needed on the system).
 */

static	int	copylib( int dir )
{
   int		r = 0;				/* return value */
   static int	copied = 0;			/* static memory */

   if (dir && !copied) {			/* copy to tmp */
      char	filename1[FILENAME_MAX+1];	/* source */
      char	filename2[FILENAME_MAX+1];	/* destination */

      mkpath( filename1, gip_lib, "giplib.a" );
      if (!access( filename1, F_OK )) {		/* it does exist */
         mkpath( filename2, gip_tmp, "giplib.a" );
         if (access( filename2, F_OK )) {	/* not yet present */
#ifndef	__linux__
            if (cp( filename1, filename2 )) return( FATAL );
#else
            /* KGB.
             * To avoid crashing on Linux Boxes we do the copying of
             * the library to tmp not via cp. I don't know why it crashes,
             * but this seems to avoid the crash.
             */
            if (system( "cp $gip_lib/giplib.a $gip_tmp/giplib.a" )) return( FATAL );
#endif
         } else {				/* already present */
            fprintf( stderr, "%s -- Warning: already a copy of giplib.a present!\a\n", program );
         }
      }
      copied = 1;				/* set to true */
   } else if (!dir && copied) {			/* put it back */
      char	filename1[FILENAME_MAX+1];	/* source */
      char	filename2[FILENAME_MAX+1];	/* destination */

      mkpath( filename1, gip_tmp, "giplib.a" );
      mkpath( filename2, gip_lib, "giplib.a" );
      if (ranlib[0]) {				/* run ranlib first */
         char	cmd[MAXCMD+1];			/* the command */
         char	sub[MAXCMD+1];			/* substitution */

         sprintf( sub, "#library=%s", filename1 );
         substitute( cmd, ranlib, sub );	/* do the substitution */
         r = docmd( cmd );			/* do the command */
         if (r && r != FATAL) {			/* ranlib error */
            fprintf( stderr, "%s -- FATAL error!\a\n", program );
            r = FATAL;				/* FATAL error */
         }
         if (r == FATAL) return( r );		/* exit */
      }
      if (mv( filename1, filename2 )) return( FATAL );
      copied = 0;				/* it's back now */
   }
   return( 0 );					/* return to caller */
}


/*
 * makelist generates a list of all symbols in an object or executable.
 */

static	int	makelist( char *object )
{
   FILE	*f1, *f2;
   char	cmd[MAXCHAR+1];
   char	command[MAXCMD+1];
   char	line[MAXLINE+1];
   char	sub[MAXCHAR+1];
   int	r = 0;

   if (access( "nm.out", F_OK )) {
      f1 = fopen( "nm.out", "w+" );
   } else {
      f1 = fopen( "nm.out", "a+" );
   }
   sprintf( sub, "#object=%s", object );
   substitute( cmd, nm_cmd, sub );
   sprintf( command, "%s > nm.dum", cmd );
   docmd( command );
   f2 = fopen( "nm.dum", "r" );
   while (fgets( line, MAXLINE, f2 ) != NULL) {
      int	l;
      int	len = strlen( line );

      while (len && isspace( line[--len] )) line[len] = 0;
      while (len && !isspace( line[--len] ));
      l = len + 1;
      while (len && isspace( line[--len] ));
      if (len && (line[len] == 'T' || line[len] == 'D')) {
         werr(fprintf( f1, "%s %c\n", &line[l], line[len] ));
         r += 1;
      }
   }
   fclose( f1 );
   fclose( f2 );
   remove( "nm.dum" );
   return( r );
}


/*
 * makedependsub searches for all modules produced out of the source file.
 * It also checks whether the modules produced by this source are in
 * conflict withm modules produced by another source.
 */

static	int	makedependsub( parent *p )
{
   FILE		*f1;
   FILE		*f2;
   char		filename1[FILENAME_MAX+1];
   char		filename2[FILENAME_MAX+1];
   char		module[MAXCHAR+1];
   char		source[MAXCHAR+1];
   int		n;
   int		ndone = 0;
   int		nmods = 0;
   int		r = 0;				/* return value */
   nm_struct	*mods = NULL;

   if (!(p->mode & COMPILE_MODE)) return( 0 );
   if ((p->mode & RETRY_MODE) && (p->status) ) return( 0 );
   if (!(p->mode & DELETE_MODE)) {
      int	n, nchild = p->nchild;

      for (n = 0; n < nchild; n++) {
         if (p->child[n].ftype & FTYPE_O) {
            nmods += makelist( p->child[n].fname );
         }
      }
      if (nmods) {
         FILE	*f;
         int	check = 1;			/* check for document */
         int	k = 0;

         while (k < NOCHECKLIST && strcmp( p->fname, nochecklist[k] )) k++;
         if (k < NOCHECKLIST) check = 0;	/* do not check */

         mods = malloc( sizeof( nm_struct ) * nmods );
         f = fopen( "nm.out", "r" );
         for (n = 0; n < nmods; n++) {
            int	len;

            readline( mods[n].module, MAXCHAR, f );
            len = strlen( mods[n].module );
            mods[n].module[len-2] = 0;
            if (mods[n].module[len-1] == 'T') {
               char	root[MAXCHAR+1];
               int	l, m, nf = 1;

               l = 0;
               while (mods[n].module[l] == '_') l++;
               while (mods[n].module[l] == '.') l++;
               strcpy( root, &mods[n].module[l] );
               l = strlen( root ) - 1;
#ifdef	__F2C__
               while (root[l] == '_') root[l--] = 0;
               if (l > 1 && root[l-1] == '_' && isdigit( root[l] ) ) {
                  nf = 0;
               }
#endif
               if (nf) {
                  while (root[l] == '_') root[l--] = 0;
                  if (l > 1 && root[l-1] == '_' && root[l] == 'c') {
                     root[l--] = 0; root[l--] = 0;
                  }
               }
#ifdef        __cray__
               m = 0;
               while (isupper(root[m])) m++;
               if (m > l) {
                  for ( m = 0; m <= l; root[m] = tolower( root[m] ), m++ );
               }
#endif
               for (m = 0; nf && m < nchild; m++) {
                  if (p->child[m].ftype & (FTYPE_DC2 | FTYPE_DC3)) {
                     if (!strcmp( fname( p->child[m].fname ), root )) nf = 0;
                  }
               }
               if (nf && check) {
                  if (!strstr(root, "get_pc_thunk")) {
                     /*
                      *   Allow for code automatically generated by gcc
                      *   when -fPIC is specified. Otherwise error.
                      */
                     fprintf( stderr, "%s -- Module %s has no docsliceview[Bument!\a\n", program, root );
                     r = FATAL;
                  }
               }
            }
         }
         fclose( f );
         if (nmods > 1) {
            qsort( mods, nmods, sizeof( nm_struct ), compar2 );
         }
      }
      remove( "nm.out" );			/* unlink this file */
   }
   if (r) {
      if (nmods) free( mods );
      return( r );
   }
   mkpath( filename1, gip_lib, "contents" );
   mkpath( filename2, gip_tmp, "contents" );
   f1 = fopen( filename1, "r+" );
   if (f1 == NULL) {				/* not present, create it */
      char	text[MAXLINE+1];		/* for text */
      time_t	now = time( NULL );		/* current time */

      f1 = fopen( filename1, "w+" );
      if (f1 == NULL) {				/* creation error */
         fprintf( stderr, "%s -- Could not open %s!\a\n", program, filename1 );
         return( FATAL );			/* FATAL error */
      }
      werr(fprintf( f1, "# contents\n" ));	/* now add some text ... */
      werr(fprintf( f1, "#\n" ));
      strftime( text, MAXLINE, "#\tCopyright (c) Kapteyn Laboratorium Groningen %Y", localtime( &now ) );
      werr(fprintf( f1, "%s\n", text ));
      werr(fprintf( f1, "#\tAll Rights Reserved.\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Document:     contents\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Purpose:      Lists the modules defined in the GIPSY library.\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Category:     MANAGEMENT\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#File:         contents\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Author:       %s\n", program ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Description:  Each line contains a module name and the GIPSY source name,\n#              separated by a colon.\n" ));
      werr(fprintf( f1, "#\n" ));
      strftime( text, MAXLINE, "#Updates:      %b %d, %Y: Document created", localtime( &now ) );
      werr(fprintf( f1, "%s\n", text ));
      werr(fprintf( f1, "#\n" ));
      fclose( f1 );
      f1 = fopen( filename1, "w+" );
      if (f1 == NULL) {				/* creation error */
         fprintf( stderr, "%s -- Could not open %s!\a\n", program, filename1 );
         return( FATAL );			/* FATAL error */
      }
   } else if (nmods) {
      int	n;				/* loop counter */
      int	nf = 0;				/* reset */

      while (xscanf( f1, "%s %s", module, source ) == 2) {
         for (n = 0; n < nmods; n++) {
            if (!strcmp( module, mods[n].module ) && strcmp( source, p->fname )) {
               if (!strstr(module, "get_pc_thunk")) {
                  /*
                   *   Allow for code automatically generated by gcc
                   *   when -fPIC is specified. Otherwise error.
                   */
                  fprintf( stderr, "%s -- Source %s produces also %s!\a\n", program, source, module );
                  nf++;
               }
            }
         }
      }
      if (nf) {					/* modules from another source */
         r = FATAL;				/* fatal error */
      } else {
         fseek( f1, 0, SEEK_SET );		/* rewind file */
      }
   }
   if (r) {
      fclose( f1 );				/* close file */
      free( mods );				/* free memory */
      return( r );				/* quit */
   }
   f2 = fopen( filename2, "w+" );
   if (f2 == NULL) {
      fprintf( stderr, "%s -- Cannot create %s!\a\n", program, filename2 );
      return(  FATAL );
   }
   copyhead( f1, f2 );
   while (xscanf( f1, "%s %s", module, source ) == 2) {
      int	cmp2 = strcmp( source, p->fname );

      while (ndone < nmods && strcmp( module, mods[ndone].module ) >= 0) {
         werr(fprintf( f2, "%s:%s\n", mods[ndone++].module, p->fname ));
      }
      if (cmp2) {				/* ship out */
         werr(fprintf( f2, "%s:%s\n", module, source ));
      }
   }
   for (n = ndone; n < nmods; n++) {
      werr(fprintf( f2, "%s:%s\n", mods[n].module, p->fname ));
   }
   if (dcp( f2, f1 )) return( FATAL );		/* copy from f2 to f1 */
   fclose( f1 );
   fclose( f2 );
   remove( filename2 );
   if (nmods) free( mods );
   return( 0 );
}


/*
 * makedependtsk.
 */

static	int	makedependtsk( parent *p )
{
   FILE		*f1;
   FILE		*f2;
   FILE		*f3;
   char		filename1[FILENAME_MAX+1];
   char		filename2[FILENAME_MAX+1];
   char		filename3[FILENAME_MAX+1];
   char		exe[MAXCHAR+1];
   char		src[MAXCHAR+1];
   int		m;
   int		n;
   int		ndone = 0;
   int		nmods = 0;
   int		nsrcs = 0;
   nm_struct	*mods = NULL;
   nm_struct	*srcs = NULL;

   if (!(p->mode & COMPILE_MODE)) return( 0 );
   if ((p->mode & RETRY_MODE) && (p->status) ) return( 0 );
   mkpath( filename3, gip_lib, "contents" );
   if (access( filename3, F_OK )) return( 0 );
   if (!(p->mode & DELETE_MODE)) {
      int	nchild = p->nchild;
      int	ndone = 0;

      for (n = 0; n < nchild; n++) {
         if (p->child[n].ftype & FTYPE_EXE) {
            nmods += makelist( p->child[n].fname );
         }
      }
      if (nmods) {
         FILE	*f;
         char	module[MAXCHAR+1];
         char	source[MAXCHAR+1];
         int	nowns = 0;

         mods = malloc( sizeof( nm_struct ) * nmods );
         f = fopen( "nm.out", "r" );
         for (n = 0; n < nmods; n++) {
            int	len;

            readline( mods[n].module, MAXCHAR, f );
            len = strlen( mods[n].module );
            mods[n].module[len-2] = 0;
         }
         fclose( f );
         remove( "nm.out" );
         for (n = 0; n < nchild; n++) {
            if (p->child[n].ftype & FTYPE_O) {
               nowns += makelist( p->child[n].fname );
            }
         }
         if (nowns) {
            FILE	*f;
            char	module[MAXCHAR+1];

            f = fopen( "nm.out", "r" );
            for (n = 0; n < nowns; n++) {
               int	len, m;

               readline( module, MAXCHAR, f );
               len = strlen( module );
               module[len-2] = 0;
               for (m = 0; m < nmods; m++) {
                  if (!strcmp( module, mods[m].module )) {
                     strcpy( mods[m].module, "_?????_" );
                     break;
                  }
               }
            }
            fclose( f );
         }
         qsort( mods, nmods, sizeof( nm_struct ), compar2 );
         f3 = fopen( filename3, "r" );
         while (xscanf( f3, "%s %s", module, source ) == 2) {
            int	m = ndone;

            while (m < nmods && strcmp( module, mods[m].module )) m++;
            if (m < nmods) {
               int	k = 0;

               while (k < nsrcs && strcmp( source, srcs[k].module )) k++;
               if (k == nsrcs) {
                  ndone = m + 1;
                  srcs = realloc( srcs, sizeof( nm_struct ) * ( nsrcs + 1 ) );
                  strcpy( srcs[nsrcs].module, source );
                  nsrcs += 1;
               }
            }
         }
         fclose( f3 );
      }
      remove( "nm.out" );
   }
   if (nmods) free( mods );			/* free memory */
   if (nsrcs) qsort( srcs, nsrcs, sizeof( nm_struct ), compar2 );
   mkpath( filename1, gip_lib, "depend" );
   mkpath( filename2, gip_tmp, "depend" );
   f1 = fopen( filename1, "r+" );
   if (f1 == NULL) {
      char	text[MAXLINE+1];		/* for text */
      time_t	now = time( NULL );		/* current time */

      f1 = fopen( filename1, "w+" );
      if (f1 == NULL) {				/* creation error */
         fprintf( stderr, "%s -- Could not open %s!\a\n", program, filename1 );
         return( FATAL );			/* FATAL error */
      }
      werr(fprintf( f1, "# depend\n" ));	/* now add some text ... */
      werr(fprintf( f1, "#\n" ));
      strftime( text, MAXLINE, "#\tCopyright (c) Kapteyn Laboratorium Groningen %Y", localtime( &now ) );
      werr(fprintf( f1, "%s\n", text ));
      werr(fprintf( f1, "#\tAll Rights Reserved.\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Document:     depend\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Purpose:      Dependency list of GIPSY applications.\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Category:     MANAGEMENT\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#File:         depend\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Author:       %s\n", program ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Description:  Each line contains a GIPSY source and a application name,\n#              separated by a colon.\n" ));
      werr(fprintf( f1, "#\n" ));
      strftime( text, MAXLINE, "#Updates:      %b %d, %Y: Document created", localtime( &now ) );
      werr(fprintf( f1, "%s\n", text ));
      werr(fprintf( f1, "#\n" ));
      fclose( f1 );
      f1 = fopen( filename1, "w+" );
      if (f1 == NULL) {				/* creation error */
         fprintf( stderr, "%s -- Could not open %s!\a\n", program, filename1 );
         return( FATAL );			/* FATAL error */
      }
   }
   f2 = fopen( filename2, "w+" );
   if (f2 == NULL) {
      fprintf( stderr, "%s -- Cannot create %s!\a\n", program, filename2 );
      return(  FATAL );
   }
   copyhead( f1, f2 );
   while (xscanf( f1, "%s %s", exe, src ) == 2) {
      int	cmp = strcmp( exe, p->fname );

      if (ndone < nsrcs) {
         if (cmp >= 0) {
            for (m = 0; m < nsrcs; m++) {
               werr(fprintf( f2, "%s:%s\n", p->fname, srcs[ndone++].module ));
            }
         }
      }
      if (cmp) {				/* put out */
         werr(fprintf( f2, "%s:%s\n", exe, src ));
      }
   }
   if (ndone < nsrcs) {				/* still not done */
      for (m = 0; m < nsrcs; m++) {
         werr(fprintf( f2, "%s:%s\n", p->fname, srcs[ndone++].module ));
      }
   }
   if (nsrcs) free( srcs );			/* free memory */
   if (dcp( f2, f1 )) return( FATAL );		/* copy back */
   fclose( f1 );
   fclose( f2 );
   rm( filename2 );				/* remove temp file */
   return( 0 );					/* return to caller */
}


/*
 * makedepend.
 */

static	int	makedepend( parent *p )
{
   if (p->utype == UTYPE_SUB) {
      return( makedependsub( p ) );
   } else if (p->utype == UTYPE_TSK) {
      return( makedependtsk( p ) );
   } else {
      return( 0 );
   }
}


/*
 * cleanup removes all files created in the tmp directory. It returns zero
 * on success, -1 on error.
 */

static	int	cleanup( parent *p )
{
   char	filename[FILENAME_MAX+1];		/* the file */
   int	n;					/* loop counter */
   int	nchild = p->nchild;			/* number of children */
   int	r = 0;					/* return value */

   for (n = 0; n < nchild; n++) {		/* delete loop */
      mkpath( filename, gip_tmp, p->child[n].fname );
      if (!access( filename, F_OK )) {		/* it exists */
         rm( filename );			/* set return value */
      }
   }
   mkpath( filename, gip_tmp, p->fname );
   if (!access( filename, F_OK )) {		/* it exists */
      rm( filename );				/* remove parent */
   }
   return( r );					/* return to caller */
}


/*
 * add adds an object module to the parent struct. If the name of the object
 * module is too large, it is renamed to a random name (only in non-private
 * mode).
 */

static	void	add( char *filename, int ftyp, parent *p )
{
   int	nchild = p->nchild;			/* number of children */

   if (!(p->mode & PRIVATE_MODE) && (p->mode & SUBROUTINE_MODE) && (ftyp & FTYPE_O)) {
      if (strlen( filename ) > ar_len) {	/* generate random name */
         char	newname[FILENAME_MAX+1];	/* receives new name */
         static int tnum = 0;

         if (tnum) {				/* initialized */
            int	nnum;				/* a nem number */

            while ((nnum = time( NULL )) == tnum) sleep( 1 );
            tnum = nnum;
         } else {
            tnum = time( NULL );
         }
         sprintf( newname, "ar%d.o", tnum );
         mv( filename, newname );		/* rename */
         strcpy( filename, newname );		/* return new name */
      }
   }
   if (nchild == MAXCHILD) {
      fprintf( stderr, "%s -- Not enough buffer space!\a\n", program );
      exit( EXIT_FAILURE );
   }
   strcpy( p->child[nchild].fname, filename );	/* add name */
   p->child[nchild].ftype = ftyp;		/* add type */
   p->nchild = nchild + 1;			/* an extra child */
}

/*
 * addlnk adds an symbolic link to the parent struct. Only executables
 * may be linked.
 */

static	void	addlnk( char *filename, char *linkname, parent *p )
{
   int	nchild = p->nchild;			/* number of children */

   if (nchild == MAXCHILD) {
      fprintf( stderr, "%s -- Not enough buffer space!\a\n", program );
      exit( EXIT_FAILURE );
   }
   strcpy( p->child[nchild].fname, filename );	/* add name */
   strcpy( p->child[nchild].lname, linkname );	/* link to this file */
   p->child[nchild].ftype = FTYPE_LNK;		/* add type */
   p->nchild = nchild + 1;			/* an extra child */
}


/*
 * getmask.
 */

static	int	getmask( FILE *f1, FILE *f2 )
{
   char	a[MAXARCH][MAXCHAR+1];			/* the archtectures */
   int	n = 0;					/* counter */
   int	narch;					/* number of architectures */
   int	r = 0;					/* return value */

   narch = xscanf( f1, "%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s",
      a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
      a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19] );
   if (narch < 0) {
      fprintf( stderr, "%s -- No architecture found!\a\n", program );
      exit( EXIT_FAILURE );
   } else if (narch > MAXARCH) {
      fprintf( stderr, "%s -- Too many architectures! Inform GIPSY MANAGER!\a\n", program );
      exit( EXIT_FAILURE );
   }
   while (n < narch && strcmp( a[n], gip_arch )) n++;
   if (n == narch) {
      fprintf( stderr, "%s -- Architecture unknown!\a\n", program );
      exit( EXIT_FAILURE );
   }
   r = (1 << n);				/* make mask */
   if (f2 != NULL) {				/* copy architectures */
      werr(fprintf( f2, "%s", a[0] ));
      for (n = 1; n < narch; n++) werr(fprintf( f2, ":%s", a[n] ));
      werr(fprintf( f2, "\n" ));
   }
   return( r );					/* return to caller */
}


/*
 * marktasks marks applications whether they should be rebuild because
 * of a subroutine update. Tasks are only marked. The rebuilding is done
 * at a later stage.
 */

static	int	marktasks( parent *p, int np )
{
   FILE		*f, *f1, *f2;
   char		filename[FILENAME_MAX+1];
   char		filename1[FILENAME_MAX+1];
   char		filename2[FILENAME_MAX+1];
   char		exe[MAXCHAR+1];
   char		src[MAXCHAR+1];
   char		name[MAXCHAR+1];
   char		type[MAXCHAR+1];
   int		amask, umask, dmask, emask;
   int		n;
   int		nsubs = 0;
   int		ntsks = 0;
   nm_struct	*subs = dummy1;
   nm_struct	*tsks = dummy2;

   for (n = 0; n < np; n++) {
      if (p[n].utype == UTYPE_SUB && !p[n].status) {
         strcpy( subs[nsubs++].module, p[n].fname );
      }
   }
   if (!nsubs) return( 0 );			/* nothing to do */
   mkpath( filename, gip_lib, "depend" );	/* dependencies are here */
   if (access( filename, F_OK )) return( 0 );	/* no such file */
   f = fopen( filename, "r" );			/* open file with dependencies */
   if (f == NULL) {				/* file not present ? */
      fprintf( stderr, "%s -- Could not open %s\a\n", program, filename );
      return( FATAL );
   }
   while (xscanf( f, "%s %s", exe, src ) == 2) {
      int	m = 0;				/* counter */

      while (m < nsubs && strcmp( src, subs[m].module )) m++;
      if (m < nsubs) {	   			/* we found a match */
         int	l = 0;				/* counter */

         while (l < ntsks && strcmp( exe, tsks[l].module )) l++;
         if (l == ntsks) {			/* no match */
            strcpy( tsks[ntsks++].module, exe );/* add to list */
         }
      }
   }
   fclose( f );					/* close file */
   if (!ntsks) return( 0 );			/* nothing to do */
   qsort( tsks, ntsks, sizeof( nm_struct ), compar2 );
   mkpath( filename1, gip_sys, "bookkeeper" );
   mkpath( filename2, gip_tmp, "bookkeeper" );
   f1 = fopenl( filename1, "r+" );		/* open file */
   if (f1 == NULL) {				/* could not be opened */
      fprintf( stderr, "%s -- Could not open %s\a\n", program, filename1 );
      return( FATAL );
   }
   f2 = fopen( filename2, "w+" );		/* open file */
   if (f2 == NULL) {				/* could not be opened */
      fprintf( stderr, "%s -- Could not open %s\a\n", program, filename2 );
      fclosel( f1, filename1 );
      return( FATAL );
   }
   if (copyhead( f1, f2 )) return( FATAL );	/* error */
   amask = getmask( f1, f2 );			/* obtain mask */
   if (amask == FATAL) return( FATAL );		/* error */
   while (xscanf( f1, "%s %s %d %d %d", name, type, &umask, &dmask, &emask ) == 5) {
      int	ndone = 0;

      while (ndone < ntsks && strcmp( name, tsks[ndone].module )) ndone++;
      if (ndone < ntsks) {			/* bingo */
         umask = (umask | amask) - amask;	/* remove our mask */
      }
      werr(fprintf( f2, "%s:%s:%d:%d:%d\n", name, type, umask, dmask, emask ));
   }
   if (dcp( f2, f1 )) return( FATAL );		/* copy back */
   fclosel( f1, filename1 );			/* close file */
   fclose( f2 );				/* close file */
   rm( filename2 );				/* remove scratch file */
   return( 0 );					/* return to caller */
}


/*
 * dellib removes an entry from the library.
 */

static	int	dellib( char *object )
{
   char	cmd[MAXCMD+1];				/* the command */
   char	sub[MAXCMD+1];				/* the substitutes */

   copylib( 1 );				/* make copy of library */
   sprintf( sub, "#library=giplib.a,#object=%s", object );
   substitute( cmd, ar_del, sub );		/* do the substitution */
   if (docmd( cmd )) return( FATAL );		/* error */
   return( 0 );					/* return to caller */
}


/*
 * addlib adds an object module to the library. It also creates an
 * entry in the already opened file objects.
 */

static	int	addlib( parent *p, FILE *f )
{
   int	n;					/* loop counter */
   int	nchild = p->nchild;			/* number of children */

   for (n = 0; n < nchild; n++) {		/* loop */
      if (p->child[n].ftype & FTYPE_O) {	/* add this one */
         char	cmd[MAXCMD+1];			/* the command */
         char	sub[MAXCMD+1];			/* the substitutes */

         copylib( 1 );				/* make copy of library */
         sprintf( sub, "#library=giplib.a,#object=%s", p->child[n].fname );
         substitute( cmd, ar_add, sub );	/* do the substitution */
         docmd( cmd );				/* do the command */
         werr(fprintf( f, "%s:%s\n", p->fname, p->child[n].fname ));
      }
   }
   return( 0 );					/* return to caller */
}


/*
 * updatelib removes (and adds) object modules from (to) the library.
 * The contents of the library are stored in $gip_lib/objects. This
 * file is also updated.
 * Eventually we have to implement the dependency checks here.
 */

static	int	updatelib( parent *p )
{
   FILE	*f1, *f2;				/* the file descriptors */
   char	filename1[FILENAME_MAX+1], filename2[FILENAME_MAX+1];
   char	source[MAXCHAR+1], object[MAXCHAR+1];
   int	done = 0;

   if (!(p->mode & COMPILE_MODE)) return( 0 );	/* nothing to do */
   if (!(p->mode & SUBROUTINE_MODE)) return( 0 );
   if ((p->mode & RETRY_MODE) && (p->status)) return( 0 );
   if (p->mode & DELETE_MODE) done = 1;		/* do not add objects */
   mkpath( filename1, gip_lib, "objects" );	/* source */
   mkpath( filename2, gip_tmp, "objects" );	/* destination */
   f1 = fopen( filename1, "r" );		/* open source */
   if (f1 == NULL) {				/* create file */
      char	text[MAXLINE+1];		/* for text */
      time_t	now = time( NULL );		/* current time */

      f1 = fopen( filename1, "w" );		/* create file */
      if (f1 == NULL) {				/* creation error */
         fprintf( stderr, "%s -- Could not open %s!\a\n", program, filename1 );
         return( FATAL );			/* FATAL error */
      }
      werr(fprintf( f1, "# objects\n" ));		/* now add some text ... */
      werr(fprintf( f1, "#\n" ));
      strftime( text, MAXLINE, "#\tCopyright (c) Kapteyn Laboratorium Groningen %Y", localtime( &now ) );
      werr(fprintf( f1, "%s\n", text ));
      werr(fprintf( f1, "#\tAll Rights Reserved.\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Document:     objects\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Purpose:      Lists the object files originating from GIPSY sources.\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Category:     MANAGEMENT\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#File:         objects\n" ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Author:       %s\n", program ));
      werr(fprintf( f1, "#\n" ));
      werr(fprintf( f1, "#Description:  Each line contains a GIPSY source name and an object,\n#              separated by a colon.\n" ));
      werr(fprintf( f1, "#\n" ));
      strftime( text, MAXLINE, "#Updates:      %b %d, %Y: Document created", localtime( &now ) );
      werr(fprintf( f1, "%s\n", text ));
      werr(fprintf( f1, "#\n" ));
      fclose( f1 );
      f1 = fopen( filename1, "r" );		/* create file */
      if (f1 == NULL) {				/* creation error */
         fprintf( stderr, "%s -- Could not open %s!\a\n", program, filename1 );
         return( FATAL );			/* FATAL error */
      }
   }
   f2 = fopen( filename2, "w" );		/* open destination */
   if (copyhead( f1, f2 )) return( FATAL );	/* copy header part */
   while (xscanf( f1, "%*s %*s", MAXCHAR, source, MAXCHAR, object ) == 2) {
      int	cmp = strcmp( source, p->fname );

      if (cmp < 0) {				/* skip this entry */
         werr(fprintf( f2, "%s:%s\n", source, object ));
      } else if (cmp == 0) {			/* remove object */
         dellib( object );
      } else if (cmp > 0) {
         if (!done) {				/* add objects */
            addlib( p, f2 );			/* add to library */
            done = 1;				/* set to done */
         }
         werr(fprintf( f2, "%s:%s\n", source, object ));
      }
   }
   if (!done) addlib( p, f2 );			/* append entries */
   fclose( f1 );				/* close source */
   fclose( f2 );				/* close destination */
   mv( filename2, filename1 );			/* replace with new file */
   marktasks( p, 1 );				/* KGB, Sep 21, 1992 */
   return( 0 );					/* return to caller */
}


/*
 * rmexe( char *exe )
 */

static	void	rmexe( char *exe )
{
   char	cmd[MAXCMD+1];				/* command buffer */
   char	exe2[FILENAME_MAX+1];			/* to */

   mkpath( exe2, gip_exe, exe );		/* path to */

   if (!access( exe2, F_OK )) {			/* exe present */
      char	exe3[FILENAME_MAX+1];		/* old */

      strcpy( exe3, exe2 );			/* copy name */
      strcat( exe3, ".old" );			/* append .old */
      if (!access( exe3, F_OK )) {		/* already present */
         rm( exe3 );				/* remove */
      }
      mv( exe2, exe3 );				/* rename */
      if ((sflag & FLAG_OLD)) {			/* remove old exes ? */
         rm( exe3 );				/* yes */
      } else {					/* keep them */
         sprintf( cmd, "touch %s", exe3 );	/* change mdate */
         docmd( cmd );				/* do it */
      }
   }
}


/*
 * saveexe.
 */

static	void	saveexe( char *exe )
{
   char	exe1[FILENAME_MAX+1];			/* from */
   char	exe2[FILENAME_MAX+1];			/* to */

   rmexe( exe );				/* remove executable */
   mkpath( exe1, gip_tmp, exe );		/* path from */
   mkpath( exe2, gip_exe, exe );		/* path to */
   mv( exe1, exe2 );				/* save new exe */
   if (!strcmp( exe, "compile" ) && !(sflag & FLAG_SUI) ) {
      chmod( exe2, CHMOD_SUI );
   } else {
      chmod( exe2, CHMOD_EXE );
   }
}


/*
 * savelnk.
 */

static	void	savelnk( char *exe, char *lnk )
{
   char	cmd[MAXCMD+1];				/* command buffer */
   char	exe1[FILENAME_MAX+1];			/* from */
   char	exe2[FILENAME_MAX+1];			/* to */

   mkpath( exe1, gip_exe, lnk );		/* path from */
   mkpath( exe2, gip_exe, exe );		/* path to */
   rm( exe2 );					/* remove original link */
   sprintf( cmd, "ln -s %s %s", exe1, exe2 );	/* link it */
   docmd( cmd );				/* do it */
}


/*
 * updatetsk.
 */

static	int	updatetsk( parent *p )
{
   int	n;
   int	nchild = p->nchild;

   if (p->mode & MAIL_MODE) return( 0 );
   if (!(p->mode & COMPILE_MODE)) return( 0 );
   if (p->mode & SUBROUTINE_MODE) return( 0 );
   if (!(p->mode & REBUILD_MODE)) return( 0 );
   if (p->mode & DELETE_MODE) return( 0 );
   if ((p->mode & RETRY_MODE) && (p->status)) return( 0 );
   for (n = 0; n < nchild; n++) {
      if (p->child[n].ftype & FTYPE_EXE) {
         saveexe( p->child[n].fname );
      } else if (p->child[n].ftype & FTYPE_LNK) {
         savelnk( p->child[n].fname, p->child[n].lname );
      }
   }
   return( 0 );
}


/*
 * getcmd looks for an options file. If not found it returns 1, otherwize 0
 * is returned. In case of error FATAL is returned.
 */

static	int	getcmd( char *cmd, char *filename, parent *p, int mode )
{
   char	opt[MAXCMD+1];				/* the name of the file */
   int	m;					/* loop counter */
   int	nchild = p->nchild;			/* number of children */
   int	r = 1;					/* return value */

   sprintf( opt, "%s.opt", fname( filename ) );	/* make name */
   for (m = 0; r && m < nchild; m++) {		/* loop */
      if (p->child[m].ftype & FTYPE_OPT && !strcmp( opt, p->child[m].fname )) r = 0;
   }
   if (!r) {					/* we found one */
      FILE	*f;				/* file descriptor */

      f = fopen( opt, "r" );			/* open options file */
      if (!readline( cmd, MAXCMD, f )) {	/* read options */
         fprintf( stderr, "%s -- Cannot get options from %s!\a\n", program, opt );
         r = FATAL;				/* fatal error */
      }
      fclose( f );				/* close options file */
   } else {
      FILE	*of;				/* file with options */
      char	opts[6][MAXCHAR];		/* the options */
      char	optname[FILENAME_MAX+1];	/* file name */
      int	nf;

      mkpath( optname, gip_loc, "options" );
      of = fopen( optname, "r" );
      if (of == NULL) return( r );
      while ( r && ((nf = xscanf( of, "%s %s %s %s %s %s", opts[0], opts[1], opts[2], opts[3], opts[4], opts[5] ) ) == 6 ) ) {
         if (!strcmp( opts[0], gip_arch ) && strstr( opts[1], p->fname )) {
            if (opts[1][strlen(p->fname)] == '(' && strstr( opts[1], fname( filename ) )) {
               r = 0;
            } else if (!strcmp( p->fname, opts[1] )) {
               r = 0;
            }
         }
      }
      fclose( of );
      if (!r) {
      	 char	*c_name, *c_opts1, *c_opts2, *c_libs;

      	 if (opts[2][0]) {
      	    c_name = opts[2];
      	 } else if (ftype( filename ) & FTYPE_C) {
      	    c_name = cc_name;
      	 } else {
      	    c_name = fc_name;
      	 }
      	 if (opts[3][0]) {
      	    c_opts1 = opts[3];
      	 } else if (ftype( filename ) & FTYPE_C ) {
      	    c_opts1 = cc_opts1;
      	 } else {
      	    c_opts1 = fc_opts1;
      	 }
      	 if (opts[4][0]) {
      	    c_opts2 = opts[4];
      	 } else if (ftype( filename ) & FTYPE_C ) {
      	    c_opts2 = cc_opts2;
      	 } else {
      	    c_opts2 = fc_opts2;
      	 }
      	 if (opts[5][0]) {
      	    c_libs = opts[5];
      	 } else if (ftype( filename ) & FTYPE_C ) {
      	    c_libs = cc_libs;
      	 } else {
      	    c_libs = fc_libs;
      	 }
      	 if (mode & SUBROUTINE_MODE) {
      	    if (p->mode & PRIVATE_MODE) {
               sprintf( cmd, "%s -c %s %s", c_name, c_opts2, filename );
      	    } else {
      	       sprintf( cmd, "%s -c %s %s", c_name, c_opts1, filename );
      	    }
      	 } else {
      	    char	exe[MAXCHAR+1];
            char	objs[MAXCMD+1];			/* contains all objects */
            int	m;

            strcpy( exe, fname( filename ) );
            objs[0] = 0;				/* set end of string */
            for (m = 0; m < p->nchild; m++) {		/* search for objects */
               if (p->child[m].ftype & FTYPE_O) {
                  strcat( objs, " " );		/* insert a space */
                  strcat( objs, p->child[m].fname );
               }
            }
            if (p->mode & PRIVATE_MODE && p->mode & OUTPUT_MODE) {
               strcpy( exe, output );
            }
            if (p->mode & PRIVATE_MODE) {
               if (p->mode & PURIFY_MODE) {
                  sprintf( cmd, "purify %s -o %s %s %s%s%s %s", c_name, exe, c_opts2, filename, objs, uobjects, c_libs );
               } else {
                  sprintf( cmd, "%s -o %s %s %s%s%s %s", c_name, exe, c_opts2, filename, objs, uobjects, c_libs );
               }
            } else {
               sprintf( cmd, "%s -o %s %s %s%s %s", c_name, exe, c_opts1, filename, objs, c_libs );
            }
         }
      }
   }
   return( r );					/* return to caller */
}


/*
 * doftoc compiles the fortran to c interface routines created by f2cvv.
 */

static	int	doftoc( parent *p )
{
   char	cmd[MAXCMD];				/* the command */
   char	obj[MAXCHAR+1];				/* the result */
   int	m;					/* loop counter */
   int	nchild = p->nchild;			/* number of children */
   int	r = 0;					/* return value */

   for (m = 0; m < nchild; m++) {		/* loop */
      if (p->child[m].ftype & FTYPE_FTOC && p->child[m].ftype & FTYPE_C) {
         sprintf( cmd, "%s -c %s %s", cc_name, cc_opts1, p->child[m].fname );
         sprintf( obj, "%s.o", fname( p->child[m].fname ) );
         r = docmd( cmd );			/* do it */
         add( obj, FTYPE_O, p );		/* add this child */
      }
   }
   return( r );					/* return to caller */
}


/*
 * checkexe checks whether an executable exists and if its an
 * unsupported executable.
 */

static	int	checkexe( char *exe )
{
   int	r;

   if (access( exe, X_OK )) {			/* no executable */
      int	n = 0;

      fprintf( stderr, "%s -- No executable %s produced!\a\n", program, exe );
      while ( n < NOSUPPORTLIST && strcmp( nosupportlist[n], exe )) n++;
      if ( n < NOSUPPORTLIST) {
         FILE	*x;

         x = fopen( exe, "w" );
         if ( x != NULL ) {
            fprintf( x, "echo \'No %s available (yet) on %s\'", exe, OS_ARCHITECTURE );
            fclose( x );
            chmod( exe, CHMOD_EXE );
            r = 0;
         } else {
            r = -1;
         }
      } else {
         r = -1;
      }
   } else {
      r = 0;
   }
   return( r );
}


/*
 * make runs make. When finished it checks the files created by the
 * make command.
 */

static	int	make( char *makefile, parent *p )
{
   char	cmd[MAXCMD+1];			/* the command */
   char	exe[MAXCHAR+1];				/* name of executable */
   int	r = 0;					/* return value */

   strcpy( exe, fname( makefile ) );		/* name of executable */
   if (p->mode & PRIVATE_MODE) {		/* private options */
      if (p->mode & PURIFY_MODE) {
         sprintf( cmd, "make -f %s \"CC_COMP=purify %s\" \"CC_OPTS=%s\" \"CC_LIBS=%s\" \"FC_COMP=purify %s\" \"FC_OPTS=%s\" \"FC_LIBS=%s\" \"X11_OPTS=%s\" \"X11_LIBS=%s\" \"XT_LIBS=%s\" \"ARCHITECTURE=%s\"", makefile, cc, cc_opts2, cc_libs, fc, fc_opts2, fc_libs, x11_opts, x11_libs, xt_libs, OS_ARCHITECTURE );
      } else {
         sprintf( cmd, "make -f %s \"CC_COMP=%s\" \"CC_OPTS=%s\" \"CC_LIBS=%s\" \"FC_COMP=%s\" \"FC_OPTS=%s\" \"FC_LIBS=%s\" \"X11_OPTS=%s\" \"X11_LIBS=%s\" \"XT_LIBS=%s\" \"ARCHITECTURE=%s\"", makefile, cc, cc_opts2, cc_libs, fc, fc_opts2, fc_libs, x11_opts, x11_libs, xt_libs, OS_ARCHITECTURE );
      }
   } else {					/* system options */
      sprintf( cmd, "make -f %s \"CC_COMP=%s\" \"CC_OPTS=%s\" \"CC_LIBS=%s\" \"FC_COMP=%s\" \"FC_OPTS=%s\" \"FC_LIBS=%s\" \"X11_OPTS=%s\" \"X11_LIBS=%s\" \"XT_LIBS=%s\" \"ARCHITECTURE=%s\"", makefile, cc, cc_opts1, cc_libs, fc, fc_opts1, fc_libs, x11_opts, x11_libs, xt_libs, OS_ARCHITECTURE );
   }
   r = docmd( cmd );				/* run make */
   if (!(p->mode & PRIVATE_MODE)) {		/* check results of make */
      int	n;				/* loop counter */
      int	nchild = p->nchild;		/* number of children */

      for (n = 0; n < nchild; n++) {		/* loop */
         if (p->child[n].ftype & (FTYPE_C | FTYPE_F | FTYPE_L | FTYPE_S | FTYPE_SHL)) {
            char	testfile[FILENAME_MAX+1];/* test file name */
            int		m;			/* loop counter */
            int		nf = 1;			/* logical */

            sprintf( testfile, "%s.o", fname( p->child[n].fname ));
            if (!access( testfile, F_OK )) {	/* it does exist */
               for (m = 0; nf && m < nchild; m++) {
                  nf = strcmp( testfile, p->child[m].fname );
               }
               if (nf) add( testfile, FTYPE_O, p );
            }
            if (p->child[n].ftype & FTYPE_SHL) {
               sprintf( testfile, "%s.f", fname( p->child[n].fname ));
               if (!access( testfile, F_OK )) {	/* it does exist */
                  for (m = 0; nf && m < nchild; m++) {
                     nf = strcmp( testfile, p->child[m].fname );
                  }
                  if (nf) add( testfile, FTYPE_F, p );
               }
            } else if (p->child[n].ftype & FTYPE_L) {
               sprintf( testfile, "%s.c", fname( p->child[n].fname ));
               if (!access( testfile, F_OK )) {	/* it does exist */
                  for (m = 0; nf && m < nchild; m++) {
                     nf = strcmp( testfile, p->child[m].fname );
                  }
                  if (nf) add( testfile, FTYPE_C, p );
               }
            }
         }
      }
      if (!access( exe, F_OK )) {		/* add executable */
         add( exe, FTYPE_EXE, p );
      }
   }
   r = checkexe( exe );
   return( r );					/* return to caller */
}


/*
 * doc does the c compilation.
 */
static	int	doc( char *filename, int mode, parent *p )
{
   char	exe[MAXCHAR+1];				/* name of executable */
   char	obj[MAXCHAR+1];				/* name of object */
   char	cmd[MAXCMD+1];				/* the command */
   int	private = (p->mode & PRIVATE_MODE);	/* private mode */
   int	r = 0;					/* return value */

   strcpy( exe, fname( filename ) );		/* name of executable */
   sprintf( obj, "%s.o", exe );			/* name of object */
   r = getcmd( cmd, filename, p, mode );	/* get options */
   if (r == -1) return( r );			/* error while reading options */
   if (r == 1) {				/* there are no options */
      int	x11 = with_x11( filename );

      r = 0;					/* reset */
      if (mode & SUBROUTINE_MODE) {		/* subroutine */
         if (private) {
            if ( x11 ) {
               sprintf( cmd, "%s -c %s %s %s", cc, cc_opts2, x11_opts, filename );
            } else {
               sprintf( cmd, "%s -c %s %s", cc, cc_opts2, filename );
            }
         } else {
            if ( x11 ) {
               sprintf( cmd, "%s -c %s %s %s", cc, cc_opts1, x11_opts, filename );
            } else {
               sprintf( cmd, "%s -c %s %s", cc, cc_opts1, filename );
            }
         }
      } else {					/* executable */
         char	objs[MAXCMD+1];			/* contains all objects */
         int	m;
         int	nchild = p->nchild;

         objs[0] = 0;				/* set end of string */
         for (m = 0; m < nchild; m++) {		/* search for objects */
            if (p->child[m].ftype & FTYPE_O) {
               strcat( objs, " " );		/* insert a space */
               strcat( objs, p->child[m].fname );
            }
         }
         if (private && p->mode & OUTPUT_MODE) {
            strcpy( exe, output );
         }
         if (private) {
            if (p->mode & PURIFY_MODE) {
               switch ( x11 ) {
                  case 1: {
                     sprintf( cmd, "purify %s -o %s %s %s %s%s%s %s %s", cc, exe, cc_opts2, x11_opts, filename, objs, uobjects, cc_libs, x11_libs );
                     break;
                  }
                  case 2: {
                     sprintf( cmd, "purify %s -o %s %s %s %s%s%s %s %s", cc, exe, cc_opts2, x11_opts, filename, objs, uobjects, cc_libs, xt_libs );
                     break;
                  }
                  default: {
                     sprintf( cmd, "purify %s -o %s %s %s%s%s %s", cc, exe, cc_opts2, filename, objs, uobjects, cc_libs );
                     break;
                  }
               }
            } else {
               switch ( x11 ) {
                  case 1: {
                     sprintf( cmd, "%s -o %s %s %s %s%s%s %s %s", cc, exe, cc_opts2, x11_opts, filename, objs, uobjects, cc_libs, x11_libs );
                     break;
                  }
                  case 2: {
                     sprintf( cmd, "%s -o %s %s %s %s%s%s %s %s", cc, exe, cc_opts2, x11_opts, filename, objs, uobjects, cc_libs, xt_libs );
                     break;
                  }
                  default: {
                     sprintf( cmd, "%s -o %s %s %s%s%s %s", cc, exe, cc_opts2, filename, objs, uobjects, cc_libs );
                     break;
                  }
               }
            }
         } else {
            switch ( x11 ) {
               case 1: {
                  sprintf( cmd, "%s -o %s %s %s %s%s %s %s", cc, exe, cc_opts1, x11_opts, filename, objs, cc_libs, x11_libs );
                  break;
               }
               case 2: {
                  sprintf( cmd, "%s -o %s %s %s %s%s %s %s", cc, exe, cc_opts1, x11_opts, filename, objs, cc_libs, xt_libs );
                  break;
               }
               default: {
                  sprintf( cmd, "%s -o %s %s %s%s %s", cc, exe, cc_opts1, filename, objs, cc_libs );
                  break;
               }
            }
         }
      }
   }
   r = docmd( cmd );				/* do the compilation */
   if (!r) {
      if (mode & SUBROUTINE_MODE) {
         if (access( obj, F_OK )) {
            fprintf( stderr, "%s -- No object produced!\a\n", program );
            r = -1;
         } else {
            add( obj, FTYPE_O, p );
         }
      } else {
         if (checkexe( exe )) {
            r = -1;
         } else {
            if (!access( obj, F_OK )) add( obj, FTYPE_O, p );
            add( exe, FTYPE_EXE, p );
         }
      }
   }
   return( r );					/* return to caller */
}


/*
 * dof does the fortran compilation.
 */

static	int	dof( char *filename, int mode, parent *p )
{
   char	exe[MAXCHAR+1];				/* name of executable */
   char	obj[MAXCHAR+1];				/* name of object */
   char	cmd[MAXCMD+1];				/* the command */
   int	private = (p->mode & PRIVATE_MODE);	/* private mode */
   int	r = 0;					/* return value */

   strcpy( exe, fname( filename ) );		/* name of executable */
   sprintf( obj, "%s.o", exe );			/* name of object */
   r = getcmd( cmd, filename, p, mode );	/* get options */
   if (r == -1) return( r );			/* error reading options */
   if (r == 1) {				/* no options */
      r = 0;					/* reset */
      if (mode & SUBROUTINE_MODE) {		/* subroutine */
         if (private) {
            sprintf( cmd, "%s -c %s %s", fc, fc_opts2, filename );
         } else {
            sprintf( cmd, "%s -c %s %s", fc, fc_opts1, filename );
         }
      } else {					/* executable */
         char	objs[MAXCMD+1];			/* list of objects */
         int	m;
         int	nchild = p->nchild;

         objs[0] = 0;				/* set end of text */
         for (m = 0; m < nchild; m++) {
            if (p->child[m].ftype & FTYPE_O) {
               strcat( objs, " " );		/* insert space */
               strcat( objs, p->child[m].fname );
            }
         }
         if (private && p->mode & OUTPUT_MODE) {
            strcpy( exe, output );
         }
         if (private) {
            if (p->mode & PURIFY_MODE) {
               sprintf( cmd, "purify %s -o %s %s %s%s%s %s", fc, exe, fc_opts2, filename, objs, uobjects, fc_libs );
            } else {
               sprintf( cmd, "%s -o %s %s %s%s%s %s", fc, exe, fc_opts2, filename, objs, uobjects, fc_libs );
            }
         } else {
            sprintf( cmd, "%s -o %s %s %s%s %s", fc, exe, fc_opts1, filename, objs, fc_libs );
         }
      }
   }
   r = docmd( cmd );				/* do the compilation */
   if (!r) {
      if (mode & SUBROUTINE_MODE) {
         if (access( obj, F_OK )) {
            fprintf( stderr, "%s -- No object produced!\a\n", program );
            r = -1;
         } else {
            add( obj, FTYPE_O, p );
         }
      } else {
         if (checkexe( exe )) {
            r = -1;
         } else {
            if (!access( obj, F_OK )) add( obj, FTYPE_O, p );
            add( exe, FTYPE_EXE, p );
         }
      }
   }
   return( r );					/* return to caller */
}


/*
 * dos does the assembler compilation.
 */

static	int	dos( char *filename, parent *p )
{
   char	obj[MAXCHAR+1];				/* name of object */
   char	cmd[MAXCMD+1];				/* the command */
   char	sub[MAXCMD+1];				/* the substitutions */
   int	r = 0;					/* return value */

   sprintf( obj, "%s.o", fname( filename ) );	/* name of object */
   sprintf( sub, "#object=%s,#source=%s", obj, filename );
   substitute( cmd, as_cmd, sub );		/* the substitution */
   r = docmd( cmd );				/* the command */
   if (!r) add( obj, FTYPE_O, p );
   return( r );					/* return to caller */
}


/*
 * doshl does the sheltran compilation.
 */

static	int	doshl( char *filename, int mode, parent *p )
{
   char	cmd[MAXCMD+1];				/* the command */
   int	r = 0;					/* the return value */

   sprintf( cmd, "%s/sheltran %s", gip_exe, filename );
   r = docmd( cmd );				/* do it */
   if (!r) {
      char	fsrc[MAXCHAR+1];

      strcpy( fsrc, fname( filename ) );
      strcat( fsrc, ".f" );
      add( fsrc, FTYPE_F, p );
      r = dof( fsrc, mode, p );
   }
   return( r );					/* return to caller */
}


/*
 * dosrc handles packed source files.
 */

static	int	dosrc( parent *p )
{
   int	m;
   int	r = 0;
   int	ftyp = 0;
   int	nchild = p->nchild;

   for (m = 0; m < nchild; m++) {
      ftyp |= p->child[m].ftype;
   }
   if (ftyp & FTYPE_MAKE) {
      for (m = 0; m < nchild; m++) {
         if (p->child[m].ftype & FTYPE_MAKE) {
            int	rm = 0;

            rm = make( p->child[m].fname, p );
            if (rm) r = rm;
         }
      }
   } else {
      int	mexe = -1;
      int	mtyp = FTYPE_UNKNOWN;

      if (!(p->mode & SUBROUTINE_MODE)) {
         char	*out = fname( p->fname );

         for (m = 0; mexe == -1 && m < nchild; m++) {
            char	*src = fname( p->child[m].fname );

            if (p->child[m].ftype & (FTYPE_C | FTYPE_F | FTYPE_SHL) && !strcmp( out, src )) {
               mexe = m;
               mtyp = p->child[m].ftype;
            }
         }
      }
      for (m = 0; m < nchild; m++) {
         int	rm = 0;

         if (m != mexe) switch( p->child[m].ftype ) {
            case FTYPE_C: {
               rm = doc( p->child[m].fname, SUBROUTINE_MODE, p );
               break;
            }
            case FTYPE_F: {
               rm = dof( p->child[m].fname, SUBROUTINE_MODE, p );
               break;
            }
            case FTYPE_S: {
               rm = dos( p->child[m].fname, p );
               break;
            }
            case FTYPE_SHL: {
               rm = doshl( p->child[m].fname, SUBROUTINE_MODE, p );
               break;
            }
            default: {
               break;
            }
         }
         if (rm) r = rm;
      }
      if (mexe != -1) {
         int	rm = 0;

         switch(mtyp) {
            case FTYPE_C: {
               rm = doc( p->child[mexe].fname, 0, p );
               break;
            }
            case FTYPE_F: {
               rm = dof( p->child[mexe].fname, 0, p );
               break;
            }
            case FTYPE_SHL: {
               rm = doshl( p->child[mexe].fname, 0, p );
               break;
            }
            default: {
               break;
            }
         }
         if (rm) r = rm;
      }
   }
   return( r );
}


/*
 * dosyn creates the synonyms.
 */

static	int	dosyn( parent *p )
{
   char	exe[MAXCHAR+1];
   char	syn[MAXCHAR+1];
   int	m;
   int	nchild = p->nchild;

   strcpy( exe, fname( p->fname ) );
   strcpy( syn, exe );
   strcat( syn, ".syn" );
   for (m = 0; m < nchild; m++) {
      if (!strcmp( syn, p->child[m].fname ) ) break;
   }
   if (m < nchild) {
      FILE	*f;
      char	filename[MAXCHAR+1];

      f = fopen( syn, "r" );
      while (readline( filename, MAXCHAR, f )) {
         if ( sflag & FLAG_LNK ) {
            addlnk( filename, exe, p );
         } else {
            cp( exe, filename );
            add( filename, FTYPE_EXE, p );
         }
      }
      fclose( f );
   }
   return( 0 );					/* return to caller */
}


/*
 * compile compiles the compilables.
 */

static	int	compile( parent *p )
{
   int	mode = p->mode;
   int	r = 0;

   if (mode & DELETE_MODE) return( 0 );		/* nothing to do */
   if (!(mode & COMPILE_MODE)) return( 0 );	/* nothing to do */
   r = doftoc( p );				/* ftoc interface */
   if (r) return( r );
   switch(p->ftype) {
      case FTYPE_MAKE: {
         r = make( p->fname, p );
         break;
      }
      case FTYPE_SRC: {
         r = dosrc( p );
         break;
      }
      case FTYPE_C: {
         r = doc( p->fname, mode, p );
         break;
      }
      case FTYPE_F: {
         r = dof( p->fname, mode, p );
         break;
      }
      case FTYPE_S: {
         r = dos( p->fname, p );
         break;
      }
      case FTYPE_SHL: {
         r = doshl( p->fname, mode, p );
         break;
      }
      default: {
         r = -1;
         break;
      }
   }
   if (!r && !(mode & SUBROUTINE_MODE)) {
      r = dosyn( p );
   }
   p->status = r;
   if (r && (p->mode & RETRY_MODE)) r = 0;
   return( r );					/* return to caller */
}


/*
 * f2c generates the Fortran to C interface.
 */

static	int	f2c( char *file, parent *p )
{
   char	cmd[MAXCMD];
   char	ftoc[MAXCHAR];

   remove( "f2cvv.out" );			/* remove file first */
   sprintf( cmd, "%s/f2cvv %s > f2cvv.out", gip_exe, file );
   if (docmd( cmd )) return( -1 );		/* return with error */
   strcpy( ftoc, fname( file ) );
   if (ftype( file ) == FTYPE_C) {
      strcat( ftoc, "_ftoc.c" );
   } else {
      strcat( ftoc, "_ctof.c" );
   }
   if (!access( ftoc, R_OK )) {
      add( ftoc, FTYPE_C | FTYPE_FTOC, p );
      if (!access( "f2cvv.out", R_OK )) {
         FILE	*f;
         char	fname[MAXCHAR+1];

         f = fopen( "f2cvv.out", "r" );
         while (readline( fname, MAXCHAR, f )) {
            add( fname, FTYPE_FTOC | FTYPE_H, p );
         }
         fclose( f );
      }
   }
   rm( "f2cvv.out" );				/* remove file */
   return( 0 );					/* return to caller */
}


/*
 * doccheck checks whether a known person is stated as the author of the
 * document. It returns -1 when no known author is found, or the entry
 * in the prog_struct prog. It returns nprog when the document is listed
 * in nodocchecklist.
 */

static	int	doccheck( char *docname )
{
   FILE	*f;
   char	line[MAXLINE];
   char	*s;
   int	l;
   int	lauthor = 0;
   int	n = 0;
   int	r = 0;

   while (n < NODOCCHECKLIST && strcmp( docname, nodocchecklist[n] )) n++;
   if (n < NODOCCHECKLIST) return( nprog );
   f = fopen( docname, "r" );
   if (f == NULL) {
      fprintf( stderr, "%s -- cannot open %s!\a\n", program, docname );
      return( -2 );
   }
   while ((s = fgets( line, MAXLINE, f )) == line && strstr( line, "Author:" ) != line);
   fclose( f );
   if (s == NULL) {
      fprintf( stderr, "%s -- 'Author:' not found in %s!\a\n", program, docname );
      return( -1 );
   }
   for ( l = strlen( line ) - 1; isspace( line[l] ) || line[l] == ')'; line[l--] = '\0' );
   while (l && isalnum( line[l] )) l--;
   lauthor = strlen( &line[++l] );
   if (lauthor) {
      while (r < nprog && strcmp( prog[r].last, &line[l] ) ) r++;
      if (r == nprog) {
         fprintf( stderr, "%s -- Author of %s not in programmers.mgr!\a\n", program, docname );
         r = -1;
      }
   } else {
      fprintf( stderr, "%s -- 'Author:' not found in %s!\a\n", program, docname );
      r = -1;
   }
   return( r );
}


/*
 * birth generates offspring (runs $gip_exe/xfile).
 */

static	int	birth( parent *p )
{
   char	cmd[MAXCMD+1];

   if (p->mode & DELETE_MODE) return( 0 );	/* nothing to do */
   if (p->ftype & (FTYPE_MEM | FTYPE_NWS | FTYPE_REP)) return( 0 );
   if (p->ftype & (FTYPE_C | FTYPE_F | FTYPE_SHL)) {
      int	r = f2c( p->fname, p );

      if (r) {
         fprintf( stderr, "%s -- Error running f2cvv!\a\n", program );
         return( FATAL );
      }
   }
   remove( "xfile.out" );
   sprintf( cmd, "%s/xfile %s > xfile.out", gip_exe, p->fname );
   if (docmd( cmd )) return( FATAL );		/* return with error */
   if (!access( "xfile.out", R_OK )) {
      FILE	*f;
      char	fname[MAXCHAR+1];

      f = fopen( "xfile.out", "r" );
      while (readline( fname, MAXCHAR, f )) {
         int	ftyp = ftype( fname );

         if (ftyp == FTYPE_EXE) ftyp = FTYPE_UNKNOWN;
         add( fname, ftyp, p );
         if (ftyp & (FTYPE_C | FTYPE_F | FTYPE_SHL)) {
            int	r = f2c( fname, p );

            if (r) {
               fprintf( stderr, "%s -- Error running f2cvv!\a\n", program );
               fclose( f );
               rm( "xfile.out" );		/* remove file */
               return( FATAL );
            }
         }
      }
      fclose( f );
      rm( "xfile.out" );			/* remove file */
   }
   if (!(p->mode & INSTALL_MODE)) return( 0 );	/* nothing to do */
   if (p->mode & MASTER_MODE) return( 0 );	/* nothing to do */
   if (p->ftype & ( FTYPE_MEM | FTYPE_NWS | FTYPE_REP )) {
   } else if (p->ftype & ( FTYPE_DC0 | FTYPE_DC1 | FTYPE_DC2 | FTYPE_DC3 | FTYPE_DOC )) {
   } else if (p->ftype & FTYPE_TEX) {
      char	fnam[MAXCHAR+1];
      int	doc = (FTYPE_DOC);
      int	n;
      int	nf = 0;
      int	nchild = p->nchild;

      strcpy( fnam, fname( p->fname ) );
      for (n = 0; !nf && n < nchild; n++) {
         if ((p->child[n].ftype & doc) && !strcmp( fnam, fname( p->child[n].fname ))) {
            nf += 1;
         }
      }
      if (!nf) {
         fprintf( stderr, "%s -- No correct documentation found for %s!\a\n", program, p->fname );
         return( -1 );
      }
   } else if (p->ftype & FTYPE_H) {
      char	fnam[MAXCHAR+1];
      int	doc = (FTYPE_DC2 | FTYPE_DC3);
      int	n;
      int	nf = 0;
      int	nchild = p->nchild;

      strcpy( fnam, fname( p->fname ) );
      for (n = 0; !nf && n < nchild; n++) {
         if ((p->child[n].ftype & doc) && !strcmp( fnam, fname( p->child[n].fname ))) {
            nf += 1;
         }
      }
      if (!nf) {
         fprintf( stderr, "%s -- No correct documentation found for %s!\a\n", program, p->fname );
         return( -1 );
      }
   } else if (p->mode & SUBROUTINE_MODE) {
      char	fnam[MAXCHAR+1];
      int	doc = (FTYPE_DC2 | FTYPE_DC3);
      int	n;
      int	nf = 0;
      int	nchild = p->nchild;

      strcpy( fnam, fname( p->fname ) );
      for (n = 0; !nf && n < nchild; n++) {
         if ((p->child[n].ftype & doc) && !strcmp( fnam, fname( p->child[n].fname ))) {
            nf += 1;
         }
      }
      if (!nf) {
         fprintf( stderr, "%s -- No correct documentation found for %s!\a\n", program, p->fname );
         return( -1 );
      }
   } else {
      char	fnam[MAXCHAR+1];
      int	doc = (FTYPE_DOC | FTYPE_DC1 | FTYPE_TEX);
      int	n;
      int	nf = 0;
      int	nchild = p->nchild;

      strcpy( fnam, fname( p->fname ) );
      for (n = 0; !nf && n < nchild; n++) {
         if ((p->child[n].ftype & doc) && !strcmp( fnam, fname( p->child[n].fname ))) {
            nf += 1;
         }
      }
      if (!nf) {
         fprintf( stderr, "%s -- No correct documentation found for %s!\a\n", program, p->fname );
         return( -1 );
      }
   }
   if ((p->ftype & FTYPE_MGR) && !(sflag & FLAG_SLV)) {
      if (getmanager()) return( -1 );		/* error */
      if (strcmp( username, mgrlogn )) {
         fprintf( stderr, "%s -- Only GIPSY manager (%s) may install *.mgr files!\a\n", program, mgrname );
         return( -1 );
      }
   }
   if (p->ftype & ( FTYPE_MEM | FTYPE_NWS | FTYPE_REP )) {
   } else if (p->ftype & ( FTYPE_DC0 | FTYPE_DC1 | FTYPE_DC2 | FTYPE_DC3 | FTYPE_DOC )) {
      if (doccheck( p->fname ) < 0) {
         return( -1 );
      }
   } else {
      int	doc = (FTYPE_DC0 | FTYPE_DC1 | FTYPE_DC2 | FTYPE_DC3 | FTYPE_DOC);
      int	n;
      int	nf = 0;
      int	nchild = p->nchild;

      for (n = 0; n < nchild; n++) {
         if ((p->child[n].ftype & doc) && doccheck( p->child[n].fname ) < 0) {
            nf += 1;
         }
      }
      if (nf) return( -1 );
   }
   return( 0 );
}


/*
 * retrieve retrieves older version of source files.
 */

static	int	retrieve( parent *p, int np )
{
   int	n;

   if (sflag & FLAG_SLV) {
      fprintf( stderr, "%s -- you can not retrieve!\a\n", program );
      return( FATAL );
   }
   for (n = 0; n < np; n++) {
      if (p[n].mode & RETRIEVE_MODE) {
         int	vers = -1;

         if (version == -1) {
            if (p[n].version) vers = p[n].version - 1;
         } else {
            vers = version;
         }
         if (vers > -1 && vers <= p[n].version) {
            char	cmd[MAXCMD+1];
            int		nd = 0;
            int		nf = 0;
            int		v;

            for (v = p[n].version - 1; !nf && v >= vers; v--) {
               char	filename[FILENAME_MAX+1];
               char	filename0[FILENAME_MAX+1];

               sprintf( filename0, "%s.%d", p[n].fname, v );
               mkpath( filename, gip_old, filename0 );
               if (access( filename, F_OK )) {
                  fprintf( stderr, "%s -- %s not present!\a\n", program, filename );
                  nf = 1;
               } else {
                  if (nd) {
                     strcat( cmd, " " );
                  } else {
                     strcpy( cmd, "( cat " );
                  }
                  strcat( cmd, filename );
                  nd += 1;
               }
            }
            if (!nf) {
               char	filename[FILENAME_MAX+1];

               if (nd) {
                  strcat( cmd, " ; echo '1,$p' ) | ed - " );
               } else {
                  strcpy( cmd, "cat " );
               }
               if (!strcmp( p[n].uname, "WASTEBASKET" )) {
                  char	filename0[FILENAME_MAX+1];

                  sprintf( filename0, "%s.%d", p[n].fname, p[n].version );
                  mkpath( filename, gip_old, filename0 );
               } else {
                  mkpath(filename, p[n].upath, p[n].fname );
               }
               if (access( filename, F_OK )) {
                  fprintf( stderr, "%s -- %s not present!\a\n", program, filename );
               } else {
                  strcat( cmd, filename );
                  if (p[n].mode & MAIL_MODE) {
                     strcat( cmd, " | mail " );
                     strcat( cmd, usermail );
                  } else {
                     strcat( cmd, " > " );
                     strcat( cmd, p[n].fname );
                  }
                  docmd( cmd );
               }
            }
         } else {
            fprintf( stderr, "%s -- Version %d not present!\a\n", program, vers );
         }
      }
   }
   return( 0 );
}


/*
 * reserve reserves or unreserves source files.
 */

static	int	reserve( parent *p, int np )
{
   FILE		*f = NULL;
   char		filename[FILENAME_MAX+1];
   int		n;
   time_t	now = time( NULL );

   if (sflag & FLAG_SLV) {
      fprintf( stderr, "%s -- You may not reserve!\a\n", program );
      return( FATAL );
   }
   mkpath( filename, gip_sys, "history" );
   f = fopenl( filename, "a+" );
   if (f == NULL) {
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename );
      return( FATAL );
   }
   for (n = 0; n < np; n++) {			/* loop */
      if (p[n].mode & RESERVE_MODE) {
         werr(fprintf( f, "%s:%s:%d:%s:%s:%s:%d\n", p[n].fname, utypes[p[n].utype], p[n].version, ttostr( now ), username, username, p[n].size ));
         if (p[n].mode & MAIL_MODE) {
            char	cmd[MAXCMD+1];

            sprintf( cmd, "cat %s/%s | mail %s", p[n].upath, p[n].fname, usermail );
            docmd( cmd );
         }
      } else if (p[n].mode & UNRESERVE_MODE) {
         werr(fprintf( f, "%s:%s:%d:%s:%s:GIPSY:%d\n", p[n].fname, utypes[p[n].utype], p[n].version, ttostr( now ), username, p[n].size ));
      }
   }
   fclosel( f, filename );			/* close file */
   putfilesize( filename );			/* update size history file */
   return( 0 );					/* return to caller */
}


/*
 * history obtains (displays) information about the history of
 * GIPSY sources.
 */

static	int	history( parent *p, int np, int mode )
{
   FILE	*f;
   char	filename[FILENAME_MAX+1];
   char	name[MAXCHAR+1];
   char	type[MAXCHAR+1];
   char	date[MAXCHAR+1];
   char	user[MAXCHAR+1];
   char	ownr[MAXCHAR+1];
   int	size;
   int	vers;
   int	nf;
   int	r = 0;


   if (!(mode & PRIVATE_MODE) && (sflag & FLAG_SLV)) return( 0 );
   if (mode & HISTORY_MODE) {
      fprintf( stdout, "SOURCE FILE          TYPE VERSION DATE                 INSTALLER  OWNER\n" );
   }
   mkpath( filename, gip_sys, "history" );
   if (mode & PRIVATE_MODE) {
      f = fopen( filename, "r" );
   } else {
      f = fopenl( filename, "r" );
   }
   if (f == NULL) {
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename );
      return( FATAL );
   }
   while ((nf = xscanf( f, "%s %s %d %s %s %s %d", name, type, &vers, date, user, ownr, &size )) >= 6) {
      int	ftyp = ftype( name );
      int	n;

      for (n = 0; n < np; n++) {
         if (!strcmp( name, p[n].fname )) {
            int	utyp = 1;

            strcpy( p[n].uname, ownr );
            p[n].version = vers;
            if (nf == 6) {
               p[n].size = 0;
            } else {
               p[n].size = size;
            }
            p[n].ftype = ftyp;
            while (utyp < MAXUTYPES && strcmp( type, utypes[utyp] )) utyp += 1;
            getupath( &p[n], utyp );
            break;
         }
      }
      if (((n < np) || !np ) && (mode & HISTORY_MODE) && !(ftyp & (FTYPE_BUG | FTYPE_FIX))) {
         fprintf( stdout, "%-20s %-4.4s %7d %-20s %-10s %-10s\n", name, type, vers, date, user, ownr );
      }
   }
   if (mode & PRIVATE_MODE) {
      fclose( f );
   } else {
      fclosel( f, filename );
   }
   if (mode & (INSTALL_MODE | RESERVE_MODE | UNRESERVE_MODE | DELETE_MODE | REBUILD_MODE)) {
      int	n;

      for (n = 0; n < np; n++) {
         if (p[n].version > -1) {		/* known source */
            switch( p[n].utype ) {		/* which type */
               case UTYPE_SYS: {
                  if (ftype( p[n].fname ) & FTYPE_MGR) {
                     if (p[n].mode & (INSTALL_MODE | RESERVE_MODE | UNRESERVE_MODE | DELETE_MODE)) {
                        if (getmanager()) {
                           r += 1;
                        } else if (strcmp( mgrlogn, username )) {
                           fprintf( stderr, "%s -- Only manager (%s) is allowed to do this!\a\n", program, mgrname );
                           r += 1;
                        }
                     }
                  }
                  break;
               }
               case UTYPE_TSK: {		/* subroutine mode off */
                  if (p[n].mode & SUBROUTINE_MODE) {
                     fprintf( stderr, "%s -- %s is not a subroutine!\a\n", program, p[n].fname );
                     p[n].mode -= SUBROUTINE_MODE;
                  }
                  break;
               }
               case UTYPE_SUB: {		/* subroutine mode on */
                  if (!(p[n].mode & SUBROUTINE_MODE)) {
                     fprintf( stderr, "%s -- %s is a subroutine!\a\n", program, p[n].fname );
                     p[n].mode |= SUBROUTINE_MODE;
                  }
                  break;
               }
               default: {
                  break;
               }
            }
            if (!strcmp( p[n].uname, "GIPSY" )) {
               if (mode & UNRESERVE_MODE) {
                  fprintf( stderr, "%s -- %s not reserved!\a\n", program, p[n].fname );
                  r += 1;
               } else if (mode & (INSTALL_MODE | DELETE_MODE)) {
                  fprintf( stderr, "%s -- %s not reserved by %s!\a\n", program, p[n].fname, username );
                  r += 1;
               }
            } else if (!strcmp( p[n].uname, "WASTEBASKET" )) {
               if (mode & UNRESERVE_MODE) {
                  fprintf( stderr, "%s -- %s not reserved!\a\n", program, p[n].fname );
                  r += 1;
               } else if (mode & (RESERVE_MODE | REBUILD_MODE)) {
                  fprintf( stderr, "%s -- %s has been removed!\a\n", program, p[n].fname );
                  r += 1;
               } else if (mode & DELETE_MODE) {
                  fprintf( stderr, "%s -- %s has already been removed!\a\n", program, p[n].fname );
                  r += 1;
               }
            } else if (!strcmp( p[n].uname, username )) {
               if (mode & RESERVE_MODE) {
                  fprintf( stderr, "%s -- You have already reserved %s!\a\n", program, p[n].fname );
                  r += 1;
               }
            } else {
               if (!(mode & REBUILD_MODE)) {
                  fprintf( stderr, "%s -- %s reserved by %s!\a\n", program, p[n].fname, p[n].uname );
                  r += 1;
               }
            }
         } else {				/* unknown source */
            if (mode & (REBUILD_MODE | DELETE_MODE | RESERVE_MODE | UNRESERVE_MODE)) {
               fprintf( stderr, "%s -- %s not present!\a\n", program, p[n].fname );
               r += 1;
            } else if (ftype( p[n].fname ) & FTYPE_MGR) {
               if (p[n].mode & (INSTALL_MODE | RESERVE_MODE | UNRESERVE_MODE | DELETE_MODE)) {
                  if (getmanager()) {
                     r += 1;
                  } else if (strcmp( mgrlogn, username )) {
                     fprintf( stderr, "%s -- Only manager (%s) is allowed to do this!\a\n", program, mgrname );
                     r += 1;
                  }
               }
            }
         }
      }
   }
   return( r );
}


/*
 * bookkeeper.
 */

static	int	bookkeeper( parent *p )
{
   FILE	*f1, *f2;
   char	filename1[FILENAME_MAX+1], filename2[FILENAME_MAX+1];
   char	name[MAXCHAR+1];
   char	type[MAXCHAR+1];
   int	amask, umask, dmask, emask;
   int	done = 0;
   int	r = 0;

   if (p->utype != UTYPE_SUB && p->utype != UTYPE_TSK) return( 0 );
   mkpath( filename1, gip_sys, "bookkeeper" );
   mkpath( filename2, gip_tmp, "bookkeeper" );
   f1 = fopenl( filename1, "r+" );
   if (f1 == NULL) {
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename1 );
      return( FATAL );
   }
   f2 = fopen( filename2, "w+" );
   if (f2 == NULL) {
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename2 );
      fclosel( f1, filename1 );
      return( FATAL );
   }
   if (copyhead( f1, f2 )) return( FATAL );
   amask = getmask( f1, f2 );
   if (amask == FATAL) {
      fclosel( f1, filename1 );
      return( FATAL );
   }
   while ((r = xscanf( f1, "%s %s %d %d %d", name, type, &umask, &dmask, &emask )) == 5) {
      int	cmp = strcmp( name, p->fname );

      if (cmp < 0) {				/* skip this entry */
         werr(fprintf( f2, "%s:%s:%d:%d:%d\n", name, type, umask, dmask, emask ));
      } else if (cmp == 0) {			/* replace */
         if (p->mode & INSTALL_MODE) {		/* new version */
            if (!p->status) {			/* new version o.k. */
               emask = (emask | amask) - amask;	/* remove our flag */
               if (p->mode & SUBROUTINE_MODE) {
                  werr(fprintf( f2, "%s:sub:%d:%d:%d\n", p->fname, amask, 0, emask ));
               } else {
                  werr(fprintf( f2, "%s:tsk:%d:%d:%d\n", p->fname, amask, 0, emask ));
               }
            } else {				/* old version */
               werr(fprintf( f2, "%s:%s:%d:%d:%d\n", name, type, umask, dmask, emask ));
            }
         } else if (p->mode & DELETE_MODE) {	/* delete current version */
            umask = (umask | amask) - amask;	/* remove our bit */
            emask = (emask | amask) - amask;	/* remove our bit */
            if (umask) {			/* queued for others */
               if (p->mode & SUBROUTINE_MODE) {
                  werr(fprintf( f2, "%s:sub:%d:%d:%d\n", p->fname, umask, 1, emask ));
               } else {
		  werr(fprintf( f2, "%s:tsk:%d:%d:%d\n", p->fname, umask, 1, emask ));
               }
            }
         } else {				/* rebuild or update */
            if (p->status) {
            	emask |= amask;			/* add our flag */
            } else {
            	emask = (emask | amask) - amask;/* remove our flag */
            }
            umask = umask | amask;		/* add our bit */
            if (p->mode & SUBROUTINE_MODE) {
               werr(fprintf( f2, "%s:sub:%d:%d:%d\n", p->fname, umask, 0, emask ));
            } else {
               werr(fprintf( f2, "%s:tsk:%d:%d:%d\n", p->fname, umask, 0, emask ));
            }
         }
         done = 1;				/* it's done */
      } else if (!done) {			/* insert */
         if (p->mode & (INSTALL_MODE | REBUILD_MODE)) {
            int	new_emask = 0;

            if (p->mode & REBUILD_MODE) {
               fprintf( stderr, "%s -- Warning: inserting %s in bookkeeper!\a\n", program, p->fname );
               if (p->status) new_emask = amask;
            }
            if (!p->status) {
               if (p->mode & SUBROUTINE_MODE) {
                  werr(fprintf( f2, "%s:sub:%d:%d:%d\n", p->fname, amask, 0, new_emask ));
               } else {
                  werr(fprintf( f2, "%s:tsk:%d:%d:%d\n", p->fname, amask, 0, new_emask ));
               }
            }
         }
         done = 1;				/* it's done */
         werr(fprintf( f2, "%s:%s:%d:%d:%d\n", name, type, umask, dmask, emask ));
      } else {
         werr(fprintf( f2, "%s:%s:%d:%d:%d\n", name, type, umask, dmask, emask ));
      }
   }
   if (!done) {
      if (p->mode & (INSTALL_MODE | REBUILD_MODE)) {
         int	new_emask = 0;
         if (p->mode & REBUILD_MODE) {
            fprintf( stderr, "%s -- Warning: inserting %s in bookkeeper!\a\n", program, p->fname );
            if (p->status) new_emask = amask;
         }
         if (!p->status) {
            if (p->mode & SUBROUTINE_MODE) {
               werr(fprintf( f2, "%s:sub:%d:%d:%d\n", p->fname, amask, 0, new_emask ));
            } else {
               werr(fprintf( f2, "%s:tsk:%d:%d:%d\n", p->fname, amask, 0, new_emask ));
            }
         }
      }
   }
   if (dcp( f2, f1 )) return( FATAL );		/* copy back */
   fclosel( f1, filename1 );
   fclose( f2 );
   rm( filename2 );
   return( 0 );
}


/*
 * delete.
 */

static	int	delete( parent *p )
{
   FILE	*f;
   FILE	*f1;
   FILE	*f2;
   char	filename[FILENAME_MAX+1];
   char	filename0[FILENAME_MAX+1];
   char	filename1[FILENAME_MAX+1];
   char	filename2[FILENAME_MAX+1];
   char	child[MAXCHAR+1];
   char	source[MAXCHAR+1];
   char	type[4];
   time_t	now;

   now = time( NULL );
   if (!(p->mode & DELETE_MODE)) return( 0 );		/* no action needed */
   if ((sflag & FLAG_SLV) && (!(sflag & FLAG_FTP) && !(p->mode & IMPORT_MODE))) return( FATAL );
   if (!strcmp( p->uname, "WASTEBASKET" )) return( 0 );	/* already removed */
   if ((p->mode & RETRY_MODE) && (p->status)) return( 0 );
   mkpath( filename1, p->upath, p->fname );
   sprintf( filename0, "%s.%d", p->fname, p->version );
   mkpath( filename2, gip_old, filename0 );
   if (access( filename1, F_OK )) return( 0 );		/* not present */
   if (sflag & FLAG_SLV) {
      rm( filename1 );
   } else {
      mv( filename1, filename2 );			/* save it */
   }
   if (!(p->ftype & ( FTYPE_MEM | FTYPE_NWS | FTYPE_REP ))) {
      mkpath( filename1, gip_sys, "offspring" );
      f1 = fopenl( filename1, "r+" );
      if (f1 == NULL) {					/* no such file */
         fprintf( stderr, "%s -- Cannot open file %s!\a\n", program, filename1 );
         return( 0 );
      }
      mkpath( filename2, gip_tmp, "offspring" );
      f2 = fopen( filename2, "w+" );
      if (f2 == NULL) {
         fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename2 );
         fclosel( f1, filename1 );
         return( FATAL );				/* fatal error */
      }
      copyhead( f1, f2 );				/* copy file heading */
      while (xscanf( f1, "%s %s", source, child ) >= 2) {
         if (strcmp( source, p->fname )) {
            werr(fprintf( f2, "%s:%s\n", source, child ));
         } else {
            char	name[FILENAME_MAX+1];
            int	ftyp = ftype( child );

            if (ftyp & (FTYPE_DC0 | FTYPE_DOC | FTYPE_TEX)) {
               mkpath( name, gip_doc, child );
               rm( name );
            } else if (ftyp & (FTYPE_DC1)) {
               mkpath( name, gip_tsk, child );
               rm( name );
            } else if (ftyp & (FTYPE_DC2 | FTYPE_DC3)) {
               mkpath( name, gip_sub, child );
               rm( name );
            } else if (ftyp & (FTYPE_H)) {
               mkpath( name, gip_inc, child );
               rm( name );
            } else if (ftyp & (FTYPE_CSH | FTYPE_SH | FTYPE_PY)) {
               mkpath( name, gip_sys, child );
               rm( name );
            } else if (ftyp & (FTYPE_EXE)) {
               rmexe( child );
            }
         }
      }
      if (dcp( f2, f1 )) return( FATAL );		/* copy back */
      fclosel( f1, filename1 );
      putfilesize( filename1 );				/* size of file */
      fclose( f2 );
      rm( filename2 );
   }
   strcpy( type, utypes[p->utype] );
   mkpath( filename, gip_sys, "history" );
   f = fopenl( filename, "a" );
   if (f == NULL) {
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename );
      return( -1 );
   }
   werr(fprintf( f, "%s:%s:%d:%s:%s:WASTEBASKET:%d\n", p->fname, type, p->version, ttostr( now ), username, p->size ));
   fclosel( f, filename );
   putfilesize( filename );			/* update size history file */
   return( 0 );
}


/*
 * dependinc searches for sources which include the header file h.
 */

static	int	dependinc( char *h )
{
   FILE		*f1, *f2;
   static char	hh[MAXINC][MAXCHAR];		/* list with includes */
   static char	nd[MAXINC];			/* list (done or not done) */
   static int	nh = 0;				/* size of list */
   char		cmd[MAXCMD];
   char		filename[FILENAME_MAX+1];
   char		ofile[FILENAME_MAX+1];
   char		source[MAXCHAR+1], kid[MAXCHAR+1];
   char		line[MAXLINE];
   int		n, nf;
   int		nsrc = 0;
   nm_struct	*src = dummy1;

   /*
    * First check whether the include has already been checked.
    */
   for ( nf = 1, n = 0; nf && n < nh; n++ ) {
      nf = strcmp( hh[n], h );
   }
   if (!nf && (nd[n-1] == 3) ) return( 0 );	/* already checked */
   if (nh < MAXINC) strcpy( hh[nh++], h );	/* add to list */
   /*
    * Simple: create a file h.dep which contains all include files which
    * include the header file h. Then look for all sources which include
    * any of these files.
    */
   sprintf( cmd, "echo $gip_inc/%s > h.dep", h );
   docmd( cmd );
   mkpath( ofile, gip_sys, "offspring" );
   f1 = fopen( "inc.dir", "w" );
   if (f1 == NULL) {
      fprintf( stderr, "%s -- cannot create inc.dir!\a\n", program );
      return( FATAL );
   }
   f2 = fopen( ofile, "r" );
   if (f2 == NULL) {
      fprintf( stderr, "%s -- cannot open %s!\a\n", program, ofile );
      return( FATAL );
   }
   while (xscanf( f2, "%s %s", source, kid ) >= 2) {
      if (ftype( source ) == FTYPE_H) {
         if (strcmp( h, source )) {
            for ( nf = 1, n = 0; nf && n < nh; n++ ) {
               nf = strcmp( hh[n], source );
            }
            if (!nf && ( nd[n-1] == 3 ) ) {	/* already done */
            } else {
               fprintf( f1, "%s\n", source );
               if (nh < MAXINC) strcpy( hh[nh++], source );
            }
         }
      }
   }
   fclose( f1 );
   fclose( f2 );
   f1 = fopen( "inc.dir", "r" );
   if (f1 == NULL) {
      fprintf( stderr, "%s -- cannot open inc.dir!\a\n", program );
      return( FATAL );
   }
   while (fgets( filename, FILENAME_MAX, f1 ) != NULL) {
      int	ll = strlen( filename ) - 1;

      while (ll > 0 && isspace(filename[ll])) filename[ll--] = 0;
      if ((ftype( filename ) & FTYPE_H) && strcmp( h, filename )) {
         sprintf( cmd, "grep -l '\"%s\"' $gip_inc/%s >> h.dep", h, filename );
         docmd( cmd );
      }
   }
   fclose( f1 );
   remove( "inc.dir" );
   f1 = fopen( "h.dep", "r" );
   if (f1 == NULL) return( 0 );
   fclose( f1 );
   /*
    * now we have a list of all include files which have something to
    * do with the include file which is modified. Next we have to
    * check all possible sources which include these files.
    *
    * First the subroutines.
    */
   docmd( "touch sub.dep" );
   sprintf( cmd, "ls $gip_sub > sub.dir" );
   docmd( cmd );
   f1 = fopen( "h.dep", "r" );
   f2 = fopen( "sub.dir", "r" );
   if ( f2 == NULL ) {
      fprintf( stderr, "%s -- cannot open sub.dir!\a\n", program );
      return( FATAL );
   }
   fclose( f2 );
   while (fgets( line, MAXLINE, f1 ) != NULL) {
      int	l = strlen( line );

      while (l && line[l-1] == '\n') line[--l] = 0;
      while (l && line[l-1] != '/') l--;
      for ( nf = 1, n = 0; nf && n < nh; n++) {		/* check */
         nf = strcmp( hh[n], &line[l] );
      }
      if (!nf && ( nd[n-1] & 1 ) ) {			/* already done */
      } else {
         if (!nf) nd[n-1] |= 1;				/* flag */
         f2 = fopen( "sub.dir", "r" );
         while(fgets( filename, FILENAME_MAX, f2 ) != NULL) {
            int	ll = strlen( filename ) - 1;

            while (ll > 0 && isspace(filename[ll])) filename[ll--] = 0;
            if (ftype( filename ) & ( FTYPE_C | FTYPE_SRC )) {
               sprintf( cmd, "grep -l '\"%s\"' $gip_sub/%s >> sub.dep", &line[l], filename );
               docmd( cmd );
            }
         }
         fclose( f2 );
      }
   }
   remove( "sub.dir" );
   fclose( f1 );
   f2 = fopen( "sub.dep", "r" );
   if (f2 != NULL) {
      nsrc = 0;
      while (fgets( line, MAXLINE, f2 ) != NULL) {
         int	l = strlen( line );

         n = 0;
         while (l && line[l-1] == '\n') line[--l] = 0;
         while (l && line[l-1] != '/') l--;
         while (n < nsrc && strcmp( &line[l], src[n].module)) n++;
         if (n == nsrc) {
            if (n == MAXSOURCE) {
               fprintf( stderr, "%s -- Not enough buffer space!\a\n", program );
               fclose( f2 );
               exit( EXIT_FAILURE );
            }
            strcpy( src[n].module, &line[l] );
            nsrc += 1;
         }
      }
      fclose( f2 );
      if (nsrc > 1) {
         qsort( src, nsrc, sizeof( nm_struct ), compar2 );
      }
      remove( "sub.dep" );
      if (nsrc) {
         char	filename1[FILENAME_MAX+1];
         char	filename2[FILENAME_MAX+1];
         char	name[MAXCHAR+1];
         char	type[MAXCHAR+1];
         int	amask, umask, dmask, emask;
         int	r;

         mkpath( filename1, gip_sys, "bookkeeper" );
         mkpath( filename2, gip_tmp, "bookkeeper" );
         f1 = fopenl( filename1, "r+" );
         if (f1 == NULL) {
            fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename1 );
            return( FATAL );
         }
         f2 = fopen( filename2, "w+" );
         if (f2 == NULL) {
            fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename2 );
            fclosel( f1, filename1 );
            return( FATAL );
         }
         if (copyhead( f1, f2 )) return( FATAL );
         amask = getmask( f1, f2 );
         if (amask == FATAL) {
            fclosel( f1, filename1 );
            return( FATAL );
         }
         while ((r = xscanf( f1, "%s %s %d %d %d", name, type, &umask, &dmask, &emask )) == 5) {
            int	ns = 0;

            while (ns < nsrc && strcmp( name, src[ns].module )) ns++;
            if (ns < nsrc) umask = 0;
            werr(fprintf( f2, "%s:%s:%d:%d:%d\n", name, type, umask, dmask, emask ));
         }
         if (dcp( f2, f1 )) return( FATAL );		/* copy back */
         fclosel( f1, filename1 );
         fclose( f2 );
         rm( filename2 );
      }
   }
   /*
    * Second the applications.
    */
   docmd( "touch tsk.dep" );
   sprintf( cmd, "ls $gip_tsk > tsk.dir" );
   docmd( cmd );
   f1 = fopen( "h.dep", "r" );
   f2 = fopen( "tsk.dir", "r" );
   if ( f2 == NULL ) {
      fprintf( stderr, "%s -- cannot open tsk.dir!\a\n", program );
      return( FATAL );
   }
   fclose( f2 );
   while (fgets( line, MAXLINE, f1 ) != NULL) {
      int	l = strlen( line );

      while (l && line[l-1] == '\n') line[--l] = 0;
      while (l && line[l-1] != '/') l--;
      for ( nf = 1, n = 0; nf && n < nh; n++) {		/* check */
         nf = strcmp( hh[n], &line[l] );
      }
      if (!nf && ( nd[n-1] & 2 ) ) {			/* already done */
      } else {
         if (!nf) nd[n-1] |= 2;				/* flag */
         f2 = fopen( "tsk.dir", "r" );
         while(fgets( filename, FILENAME_MAX, f2 ) != NULL) {
            int	ll = strlen( filename ) - 1;

            while (ll > 0 && isspace(filename[ll])) filename[ll--] = 0;
            if (ftype( filename ) & ( FTYPE_C | FTYPE_SRC )) {
               sprintf( cmd, "grep -l '\"%s\"' $gip_tsk/%s >> tsk.dep", &line[l], filename );
               docmd( cmd );
            }
         }
         fclose( f2 );
      }
   }
   remove( "tsk.dir" );
   fclose( f1 );
   remove( "h.dep" );
   f2 = fopen( "tsk.dep", "r" );
   if (f2 != NULL) {
      nsrc = 0;
      while (fgets( line, MAXLINE, f2 ) != NULL) {
         int	l = strlen( line );

         n = 0;
         while (l && line[l-1] == '\n') line[--l] = 0;
         while (l && line[l-1] != '/') l--;
         while (n < nsrc && strcmp( &line[l], src[n].module)) n++;
         if (n == nsrc) {
            if (n == MAXSOURCE) {
               fprintf( stderr, "%s -- Not enough buffer space!\a\n", program );
               fclose( f2 );
               exit( EXIT_FAILURE );
            }
            strcpy( src[n].module, &line[l] );
            nsrc += 1;
         }
      }
      fclose( f2 );
      if (nsrc > 1) {
         qsort( src, nsrc, sizeof( nm_struct ), compar2 );
      }
      remove( "tsk.dep" );
      if (nsrc) {
         char	filename1[FILENAME_MAX+1];
         char	filename2[FILENAME_MAX+1];
         char	name[MAXCHAR+1];
         char	type[MAXCHAR+1];
         int	amask, umask, dmask, emask;
         int	r;

         mkpath( filename1, gip_sys, "bookkeeper" );
         mkpath( filename2, gip_tmp, "bookkeeper" );
         f1 = fopenl( filename1, "r+" );
         if (f1 == NULL) {
            fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename1 );
            return( FATAL );
         }
         f2 = fopen( filename2, "w+" );
         if (f2 == NULL) {
            fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename2 );
            fclosel( f1, filename1 );
            return( FATAL );
         }
         if (copyhead( f1, f2 )) return( FATAL );
         amask = getmask( f1, f2 );
         if (amask == FATAL) {
            fclosel( f1, filename1 );
            return( FATAL );
         }
         while ((r = xscanf( f1, "%s %s %d %d %d", name, type, &umask, &dmask, &emask )) == 5) {
            int	ns = 0;

            while (ns < nsrc && strcmp( name, src[ns].module )) ns++;
            if (ns < nsrc) umask = 0;
            werr(fprintf( f2, "%s:%s:%d:%d:%d\n", name, type, umask, dmask, emask ));
         }
         if (dcp( f2, f1 )) return( FATAL );		/* copy back */
         fclosel( f1, filename1 );
         fclose( f2 );
         rm( filename2 );
      }
   }
   return( 0 );
}


/*
 * install.
 */

static	int	install( parent *p )
{
   FILE		*f, *f1, *f2;
   char		*dest;
   char		filename[FILENAME_MAX+1];
   char		filename1[FILENAME_MAX+1];
   char		filename2[FILENAME_MAX+1];
   char		source[MAXCHAR+1];
   char		child[MAXCHAR+1];
   char		type[4];
   int		done = 0;
   int		r = 0;
   int		vers = -1;
   time_t	now;

   if (!(p->mode & INSTALL_MODE)) return( 0 );		/* nothing to do */
   if ((sflag & FLAG_SLV) && (!(sflag & FLAG_FTP) && !(p->mode & IMPORT_MODE))) return( FATAL );
   if (p->status) return( 0 );				/* cannot install */
   now = time( NULL );
   strcpy( type, utypes[p->utype] );
   dest = p->upath;
   if (!(sflag & FLAG_SLV)) {
      vers = p->version + 1;
   } else {
      vers = p->version;
   }
   if (!(p->ftype & (FTYPE_MEM | FTYPE_NWS | FTYPE_REP))) {
      mkpath( filename1, gip_sys, "offspring" );
      f1 = fopenl( filename1, "r+" );
      if (f1 == NULL) {
         fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename1 );
         return( -1 );
      }
      mkpath( filename2, gip_tmp, "offspring" );
      f2 = fopen( filename2, "w+" );
      if (f2 == NULL) {
         fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename2 );
         fclosel( f1, filename1 );
      }
      copyhead( f1, f2 );
      while ((r = xscanf( f1, "%s %s", source, child )) >= 2) {
         int	cmp = strcmp( source, p->fname );

         if (cmp < 0 || done) {
            werr(fprintf( f2, "%s:%s\n", source, child ));
         } else if (cmp == 0) {
            char	name[FILENAME_MAX+1];
            int	ftyp = ftype( child );

            if (ftyp & (FTYPE_DC0 | FTYPE_DOC | FTYPE_TEX)) {
               mkpath( name, gip_doc, child );
               rm( name );
            } else if (ftyp & (FTYPE_DC1)) {
               mkpath( name, gip_tsk, child );
               rm( name );
            } else if (ftyp & (FTYPE_DC2 | FTYPE_DC3)) {
               mkpath( name, gip_sub, child );
               rm( name );
            } else if (ftyp & (FTYPE_H)) {
               mkpath( name, gip_inc, child );
               rm ( name );
            } else if (ftyp & (FTYPE_CSH | FTYPE_SH)) {
               mkpath( name, gip_sys, child );
               rm( name );
            } else if (ftyp & (FTYPE_EXE)) {
               rmexe( child );
            }
         } else if (!done) {
            char	name1[FILENAME_MAX+1], name2[FILENAME_MAX+1];
            int		n;
            int		nchild = p->nchild;

            for (n = 0; n < nchild; n++) {
               int	ftyp = p->child[n].ftype;

               if (ftyp & (FTYPE_DC0 | FTYPE_DOC | FTYPE_TEX)) {
                  mkpath( name1, gip_tmp, p->child[n].fname );
                  mkpath( name2, gip_doc, p->child[n].fname );
                  mv( name1, name2 );
                  chmod( name2, CHMOD_SRC );
                  werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
               } else if (ftyp & (FTYPE_DC1)) {
                  mkpath( name1, gip_tmp, p->child[n].fname );
                  mkpath( name2, gip_tsk, p->child[n].fname );
                  mv( name1, name2 );
                  chmod( name2, CHMOD_SRC );
                  werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
               } else if (ftyp & (FTYPE_DC2 | FTYPE_DC3)) {
                  mkpath( name1, gip_tmp, p->child[n].fname );
                  mkpath( name2, gip_sub, p->child[n].fname );
                  mv( name1, name2 );
                  chmod( name2, CHMOD_SRC );
                  werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
               } else if (ftyp & FTYPE_EXE) {
                  saveexe( p->child[n].fname );
                  werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
               } else if (ftyp & FTYPE_LNK) {
                  savelnk( p->child[n].fname, p->child[n].lname );
                  werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
               } else if (ftyp & (FTYPE_CSH | FTYPE_SH)) {
                  mkpath( name1, gip_tmp, p->child[n].fname );
                  mkpath( name2, gip_sys, p->child[n].fname );
                  mv( name1, name2 );
                  chmod( name2, CHMOD_EXE );
                  werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
               } else if ((ftyp & FTYPE_H) && (p->mode & SUBROUTINE_MODE)) {
                  mkpath( name1, gip_tmp, p->child[n].fname );
                  mkpath( name2, gip_inc, p->child[n].fname );
                  mv( name1, name2 );
                  chmod( name2, CHMOD_SRC );
                  werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
               }
            }
            werr(fprintf( f2, "%s:%s\n", source, child ));
            done = 1;
         }
      }
      if (!done) {
         char	name1[FILENAME_MAX+1], name2[FILENAME_MAX+1];
         int	n;
         int	nchild = p->nchild;

         for (n = 0; n < nchild; n++) {
            int	ftyp = p->child[n].ftype;

            if (ftyp & (FTYPE_DC0 | FTYPE_DOC | FTYPE_TEX)) {
               mkpath( name1, gip_tmp, p->child[n].fname );
               mkpath( name2, gip_doc, p->child[n].fname );
               mv( name1, name2 );
               chmod( name2, CHMOD_SRC );
               werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
            } else if (ftyp & (FTYPE_DC1)) {
               mkpath( name1, gip_tmp, p->child[n].fname );
               mkpath( name2, gip_tsk, p->child[n].fname );
               mv( name1, name2 );
               chmod( name2, CHMOD_SRC );
               werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
            } else if (ftyp & (FTYPE_DC2 | FTYPE_DC3)) {
               mkpath( name1, gip_tmp, p->child[n].fname );
               mkpath( name2, gip_sub, p->child[n].fname );
               mv( name1, name2 );
               chmod( name2, CHMOD_SRC );
	       werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
            } else if (ftyp & FTYPE_EXE) {
               saveexe( p->child[n].fname );
               werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
            } else if (ftyp & FTYPE_LNK) {
               savelnk( p->child[n].fname, p->child[n].lname );
               werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
            } else if (ftyp & (FTYPE_CSH | FTYPE_SH)) {
               mkpath( name1, gip_tmp, p->child[n].fname );
               mkpath( name2, gip_sys, p->child[n].fname );
               mv( name1, name2 );
               chmod( name2, CHMOD_EXE );
               werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
            } else if ((ftyp & FTYPE_H) && (p->mode & SUBROUTINE_MODE)) {
               mkpath( name1, gip_tmp, p->child[n].fname );
               mkpath( name2, gip_inc, p->child[n].fname );
               mv( name1, name2 );
               chmod( name2, CHMOD_SRC );
               werr(fprintf( f2, "%s:%s\n", p->fname, p->child[n].fname ));
            }
         }
      }
      if (dcp( f2, f1 )) return( FATAL );		/* copy back */
      fclosel( f1, filename1 );
      putfilesize( filename1 );				/* save size of file */
      fclose( f2 );
      rm( filename2 );
      if ((p->ftype & FTYPE_H) && (p->version > 0)) {
         if (!(p->mode & SYSGEN_MODE)) {
            dependinc( p->fname );
         }
      }
   }
   mkpath( filename1, gip_tmp, p->fname );
   mkpath( filename2, dest, p->fname );
   if (vers && !(sflag & FLAG_SLV) && strcmp( p->uname, "WASTEBASKET" )) {
      char	cmd[MAXCMD+1];
      char	filename0[FILENAME_MAX+1];
      char	filename3[FILENAME_MAX+1];

      sprintf( filename0, "%s.%d", p->fname, vers - 1 );
      mkpath( filename3, gip_old, filename0 );
      sprintf( cmd, "diff -e %s %s > %s", filename1, filename2, filename3 );
      docmd( cmd );
   }
   if (mv( filename1, filename2 )) return( -1 );
   if (( p->ftype == FTYPE_CSH ) || ( p->ftype == FTYPE_SH )
      || ( p->ftype == FTYPE_PY )) {
      chmod( filename2, CHMOD_EXE );
   } else {
      chmod( filename2, CHMOD_SRC );
   }
   mkpath( filename, gip_sys, "history" );
   f = fopenl( filename, "a" );
   if (f == NULL) {
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename );
      return( -1 );
   }
   if (sflag & FLAG_FTP) {
      werr(fprintf( f, "%s:%s:%d:%s:%s:%s:%d\n", p->fname, type, vers, ttostr( now ), username, p->uname, getfilesize( filename2 ) ) );
   } else {
      werr(fprintf( f, "%s:%s:%d:%s:%s:GIPSY:%d\n", p->fname, type, vers, ttostr( now ), username, getfilesize( filename2 ) ) );
   }
   fclosel( f, filename );
   putfilesize( filename );			/* update size history file */
   return( 0 );
}


/*
 * checkkids checks the offspring.
 */

static	int	checkkids( parent *p )
{
   FILE	*f;
   char	filename[FILENAME_MAX+1];
   int	r = 0;

   if (!(p->mode & INSTALL_MODE)) return( 0 );
   if (p->ftype & (FTYPE_MEM | FTYPE_NWS | FTYPE_REP)) return( 0 );
   mkpath( filename, gip_sys, "offspring" );
   f = fopenl( filename, "r" );
   if (f != NULL) {
      char	source[MAXCHAR+1];
      char	child[MAXCHAR+1];
      int	n;
      int	nchild = p->nchild;
      int	nf = 0;

      while ((r = xscanf( f, "%s %s", source, child )) >= 2) {
         if (!strcmp( child, p->fname )) {
            fprintf( stderr, "%s -- %s also produced %s!\a\n", program, source, child );
            nf += 1;
         } else {
            for ( n = 0; n < nchild; n++ ) {
               int	check = 0;

               switch( p->child[n].ftype ) {
                  case FTYPE_CSH:
                  case FTYPE_DOC:
                  case FTYPE_DC0:
                  case FTYPE_DC1:
                  case FTYPE_DC2:
                  case FTYPE_DC3:
                  case FTYPE_EXE:
                  case FTYPE_PY:
                  case FTYPE_SH:
                  case FTYPE_TEX: {
                     check = 1;
                     break;
                  }
                  case FTYPE_H: {
                     if (p->mode & SUBROUTINE_MODE) check = 1;
                     break;
                  }
                  default: {
                     break;
                  }
               }
               if (check) {
                  if (!strcmp( p->child[n].fname, source )) {
                     fprintf( stderr, "%s -- %s overwrites source %s!\a\n", program, p->child[n].fname, source );
                     nf += 1;
                  } else if (strcmp( p->fname, source ) && !strcmp( p->child[n].fname, child )) {
                     fprintf( stderr, "%s -- %s also produces %s!\a\n", program, source, child );
                     nf += 1;
                  }
               }
            }
         }
      }
      fclosel( f, filename );
      if (nf) r = FATAL; else r = 0;
   }
   return( r );
}


/*
 * action does the installation, rebuilding etc. of the sources.
 */

static	int	action( parent *p, int np )
{
   int	mode = 0;				/* accumulated mode */
   int	n;					/* loop counter */
   int	private = 0;				/* private mode ? */
   int	r = 0;					/* return value */
   offspring	cbuf[MAXCHILD];

   if (!np) return( 0 );			/* fast solution */
   for (n = 0; n < np; n++) {
      mode |= p[n].mode;			/* accumulate */
      p[n].child = cbuf;
      p[n].nchild = 0;				/* reset */
   }
   if (!(mode & PRIVATE_MODE)) {		/* not private */
      if (!(mode & NOSORT_MODE)) {
         qsort( p, np, sizeof( parent ), compar1 );	/* sort them */
      }
   } else {
      private = 1;				/* private use */
   }
   for (n = 0; !intcount && n < np; n++) {	/* loop */
      char	filename1[FILENAME_MAX+1];	/* input file */
      char	filename2[FILENAME_MAX+1];	/* output file */

      r = 0;
      if (!private) {				/* copy file */
         if (!(p[n].mode & SUBROUTINE_MODE)) {
            r = copylib( 0 );
         }
         if (p[n].mode & INSTALL_MODE) {	/* get it from user */
            chdir( cwd );			/* go to initial directory */
            mkpath( filename1, p[n].fpath, p[n].fname );
            mkpath( filename2, gip_tmp, p[n].fname );
            if ((sflag & FLAG_FTP) || (mode & IMPORT_MODE)) {
               r = getfile( filename1, filename2, p[n].size, mode );
            } else {
               r = cp( filename1, filename2 );	/* copy it from user */
            }
            chdir( gip_tmp );			/* back to temp */
         } else if (p[n].mode & DELETE_MODE) {	/* no copy */
         } else if (p[n].mode & SUBROUTINE_MODE) {
            mkpath( filename1, gip_sub, p[n].fname );
            mkpath( filename2, gip_tmp, p[n].fname );
            r = cp( filename1, filename2 );	/* copy it from GIPSY */
         } else {
            mkpath( filename1, gip_tsk, p[n].fname );
            mkpath( filename2, gip_tmp, p[n].fname );
            r = cp( filename1, filename2 );	/* copy it from GIPSY */
         }
      }
      if (!r) r = birth( &p[n] );		/* generate offspring */
      if (!r) r = compile( &p[n] );		/* compile */
      if (!private) {				/* privilege mode */
         if (!r) r = checkkids( &p[n] );	/* check offspring */
         if (!r) r = makedepend( &p[n] );	/* make dependencies */
         if (!r) r = updatelib( &p[n] );	/* update library */
         if (!r) r = updatetsk( &p[n] );	/* update executable */
         if (!r) r = delete( &p[n] );		/* delete */
         if (!r) r = install( &p[n] );		/* install */
         if (r && !p[n].status) p[n].status = r;
         bookkeeper( &p[n] );			/* do the bookkeeping */
         cleanup( &p[n] );			/* cleanup the mess */
      }
      if (r | p[n].status) {
         fprintf( stderr, "%s -- Error processing %s!\a\n", program, p[n].fname );
      } else {
         fprintf( stderr, "%s -- Ready with %s!\a\n", program, p[n].fname );
      }
      if (!private) {				/* remove the mess */
         if ((p[n].mode & INSTALL_MODE) && (p[n].mode & MAIL_MODE)) {
            chdir( cwd );
            rm( filename1 );
            chdir( gip_tmp );
         }
      }
   }
   if (!private) r = copylib( 0 );		/* save new library */
#if	0
   if (!private) r = marktasks( p, np );	/* mark applications */
#endif
   return( r );					/* return to caller */
}


/*
 * action_remote does a remote privileged action.
 * The compile commands and the files can be extracted by compile on
 * the gipsy source server.
 */

static	int	action_remote( parent *b, int nb )
{
   FILE	*p;
   char	cmd[MAXCMD+1];
   int	mode = 0;
   int	n;

   if (setup_remote( mode )) return( FATAL );
   for ( n = 0; n < nb; n++) {
      mode |= b[n].mode;
   }
   sprintf( cmd, "mail %s", gfs_mail );
   p = popen( cmd, "w" );
   if (p == NULL) {
      fprintf( stderr, "%s -- cannot execute mail!\a\n", program );
      return( FATAL );
   }
   fprintf( p, "Gipsy_Compile_Begin\n" );
   fprintf( p, "%s compile.arg 0\n", BOF_MARK );
   fprintf( p, "compile" );
   switch( mode & (DELETE_MODE | INSTALL_MODE | RESERVE_MODE | RETRIEVE_MODE | UNRESERVE_MODE)) {
      case DELETE_MODE: {
         fprintf( p, " -delete" );
         break;
      }
      case INSTALL_MODE: {
         fprintf( p, " -install" );
         break;
      }
      case RESERVE_MODE: {
         fprintf( p, " -reserve" );
         break;
      }
      case RETRIEVE_MODE: {
         fprintf( p, " -retrieve" );
         if (version != -1) {
            fprintf( p, " -version %d", version );
         }
         break;
      }
      case UNRESERVE_MODE: {
         fprintf( p, " -unreserve" );
         break;
      }
      default: {
         fprintf( stderr, "%s -- illegal mode for remote install!\a\n", program );
         return( FATAL );
      }
   }
   if (mode & SUBROUTINE_MODE) {
      fprintf( p, " -c" );
   }
   fprintf( p, " -user %s", username );
   for ( n = 0; n < nb; n++ ) {
      fprintf( p, " %s", b[n].fname );
   }
   fprintf( p, "\n" );
   fprintf( p, "%s compile.arg\n", EOF_MARK );
   if (mode & INSTALL_MODE) {
      for ( n = 0; n < nb; n++) {
         FILE	*f;
         char	line[MAXLINE];
         char	name[FILENAME_MAX+1];

         chdir( cwd );
         mkpath( name, b[n].fpath, b[n].fname );
         f = fopen( name, "r" );
         if (f == NULL) {
            fprintf( stderr, "%s -- file %s not found!\a\n", program, name );
         } else {
            fprintf( p, "%s %s %d\n", BOF_MARK, b[n].fname, getfilesize( b[n].fname ) );
            while (fgets( line, MAXLINE, f ) != NULL) {
               fprintf( p, "%s", line );
            }
            fprintf( p, "%s %s\n", EOF_MARK, b[n].fname );
            fclose( f );
         }
         chdir( gip_tmp );
      }
   }
   fprintf( p, "Gipsy_Compile_End\n" );
   pclose( p );
   return( 0 );
}


/*
 * updatesub updates the library.
 */

static	int	updatesub( void )
{
   FILE		*f;
   char		name[MAXCHAR+1];
   char		type[MAXCHAR+1];
   char		filename[FILENAME_MAX+1];
   int		amask, umask, dmask, emask;
   int		r = 0;
   int		nsubs = 0;
   parent	*subs = dummy3;

   if (no_update) return( r );
   mkpath( filename, gip_sys, "bookkeeper" );
   f = fopenl( filename, "r" );
   if (f == NULL) {
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename );
      return( FATAL );
   }
   amask = getmask( f, NULL );
   if (amask == FATAL) {
      fclosel( f, filename );
      return( FATAL );
   }
   while ((r = xscanf( f, "%s %s %d %d %d", name, type, &umask, &dmask, &emask )) == 5) {
      if ((!strcmp( type, "sub" )) && ((!(umask & amask) && !dmask) || ((umask & amask) && dmask) || ((umask & amask) && (emask & amask)))) {
         int	mode = (COMPILE_MODE | REBUILD_MODE | SUBROUTINE_MODE);

         if (nsubs == MAXSOURCE) {
            fprintf( stderr, "%s -- Not enough buffer space!\a\n", program );
            fclosel( f, filename );
            exit( EXIT_FAILURE );
         }
         if ((umask & amask) && (emask & amask)) {
            mode |= RETRY_MODE;
         }
         strcpy( subs[nsubs].fname, name );
         subs[nsubs].child = NULL;
         subs[nsubs].nchild = 0;
         subs[nsubs].done = 0;
         subs[nsubs].ftype = ftype( name );
         subs[nsubs].utype = UTYPE_SUB;
         subs[nsubs].upath = gip_sub;
         subs[nsubs].mode = mode;
         if (dmask) {				/* delete it */
            subs[nsubs].mode |= DELETE_MODE;
         }
         subs[nsubs].status = 0;
         subs[nsubs].version = 0;
         nsubs += 1;
      }
   }
   fclosel( f, filename );
   r = action( subs, nsubs );
   return( r );
}


/*
 * updateapp updates the applications
 */

static	int	updateapp( void )
{
   FILE		*f;
   char		name[MAXCHAR+1];
   char		type[MAXCHAR+1];
   char		filename[FILENAME_MAX+1];
   int		amask, umask, dmask, emask;
   int		r = 0;
   int		napps = 0;
   parent	*apps = dummy3;

   if (no_update) return( r );
   mkpath( filename, gip_sys, "bookkeeper" );
   f = fopenl( filename, "r" );
   if (f == NULL) {
      fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename );
      return( -1 );
   }
   amask = getmask( f, NULL );
   if (amask == FATAL) {
      fclosel( f, filename );
      return( FATAL );
   }
   while ((r = xscanf( f, "%s %s %d %d %d", name, type, &umask, &dmask, &emask )) == 5) {
      if ((!strcmp( type, "tsk" )) && ((!(umask & amask) && !dmask) || ((umask & amask) && dmask) || ((umask & amask) && (emask & amask)))) {
         int	mode = ( COMPILE_MODE | REBUILD_MODE );

         if (napps == MAXSOURCE) {
            fprintf( stderr, "%s -- Not enough buffer space!\a\n", program );
            fclosel( f, filename );
            exit( EXIT_FAILURE );
         }
         if ((umask & amask) && (emask & amask)) {
            mode |= RETRY_MODE;
         }
         strcpy( apps[napps].fname, name );
         apps[napps].child = NULL;
         apps[napps].done = 0;
         apps[napps].ftype = ftype( name );
         apps[napps].utype = UTYPE_TSK;
         apps[napps].mode = mode;
         if (dmask) {				/* delete it */
            apps[napps].mode |= DELETE_MODE;
         }
         apps[napps].status = 0;
         apps[napps].version = 0;
         napps += 1;
      }
   }
   fclosel( f, filename );
   r = action( apps, napps );
   return( r );
}


/*
 * lock locks compile for privileged use. If already locked by another user
 * compile will quit if not in master mode. In master mode it will wait
 * until the lock has been removed.
 */

static	int	lock( int mode )
{
   FILE	*f;
   char	lockfile1[FILENAME_MAX+1];
   char	lockfile2[FILENAME_MAX+1];

   if (mode & PRIVATE_MODE) return( 0 );	/* no locking */
   if ((sflag & FLAG_SLV) && !(mode & ((REBUILD_MODE | UPDATE_MODE)))) return( 0 );
   if (euid != guid) {
      fprintf( stderr, "%s -- not allowed to create lock file!\a\n", program );
      return( FATAL );
   }
   mkpath( lockfile1, gip_tmp, "update.lock" );
   if (access( lockfile1, F_OK )) {		/* lock it */
      f = fopen( lockfile1, "w" );
      werr(fprintf( f, "%s\n", username ));
      fclose( f );
   } else if (mode & UNLOCK_MODE) {
      char	user[MAXCHAR+1];

      f = fopen( lockfile1, "r" );
      readline( user, MAXCHAR, f );
      fclose( f );
      fprintf( stdout, "%s -- Overriding lock by %s!\a\n", program, user );
   } else if (mode & MASTER_MODE) {
      if (!access( lockfile1, F_OK )) {
         char	user[MAXCHAR+1];

         f = fopen( lockfile1, "r" );
         readline( user, MAXCHAR, f );
         fclose( f );
         fprintf( stderr, "%s -- Waiting for lock to be removed by %s!\a\n", program, user );
      }
      while (!access( lockfile1, F_OK )) {	/* loop while file exists */
         if (fileage( lockfile1 ) > LONG_AGE) {
            remove( lockfile1 );
            fprintf( stderr, "%s -- Lock file %s timed out!\a\n", program, lockfile1 );
         } else {
            sleep( 60 );			/* wait one minute */
         }
      }
      f = fopen( lockfile1, "w" );
      werr(fprintf( f, "%s\n", username ));
      fclose( f );
   } else {
      char	user[MAXCHAR+1];

      f = fopen( lockfile1, "r" );
      readline( user, MAXCHAR, f );
      fclose( f );
      fprintf( stderr, "%s -- Locked by %s!\a\n", program, user );
      return( FATAL );				/* exit with error */
   }
   /*
    * An extra lock for ftp sites.
    */
   if ((sflag & FLAG_SLV) && (mode & UPDATE_MODE)) {
      mkpath( lockfile2, gip_sys, "architecture.lock" );
      if (access( lockfile2, F_OK )) {		/* lock it */
         f = fopen( lockfile2, "w" );
         werr(fprintf( f, "%s\n", OS_ARCHITECTURE ));
         fclose( f );
      } else if (mode & UNLOCK_MODE) {
         char	arch[MAXCHAR+1];

         f = fopen( lockfile2, "r" );
         readline( arch, MAXCHAR, f );
         fclose( f );
         fprintf( stdout, "%s -- Overriding lock by %s!\a\n", program, arch );
      } else if (mode & MASTER_MODE) {
         if (!access( lockfile2, F_OK )) {
            char	arch[MAXCHAR+1];

            f = fopen( lockfile2, "r" );
            readline( arch, MAXCHAR, f );
            fclose( f );
            fprintf( stderr, "%s -- Waiting for lock to be removed by %s!\a\n", program, arch );
         }
         while (!access( lockfile2, F_OK )) {	/* loop while file exists */
            if (fileage( lockfile2 ) > LONG_AGE) {
               remove( lockfile2 );
               fprintf( stderr, "%s -- Lockfile %s timed out!\a\n", program, lockfile2 );
            } else {
               sleep( 60 );			/* wait one minute */
            }
         }
         f = fopen( lockfile2, "w" );
         werr(fprintf( f, "%s\n", OS_ARCHITECTURE ));
         fclose( f );
      } else {
         char	arch[MAXCHAR+1];

         f = fopen( lockfile2, "r" );
         readline( arch, MAXCHAR, f );
         fclose( f );
         fprintf( stderr, "%s -- Locked by %s!\a\n", program, arch );
         remove( lockfile1 );
         return( FATAL );			/* exit with error */
      }
   }
   return( 0 );
}


/*
 * unlock removes the lock set by lock.
 */

static	void	unlock( int mode )
{
   char	lockfile1[FILENAME_MAX+1];
   char	lockfile2[FILENAME_MAX+1];

   if (mode & PRIVATE_MODE) return;		/* no locking */
   mkpath( lockfile1, gip_tmp, "update.lock" );
   if (!access( lockfile1, F_OK )) {		/* was locked */
      if (intcount) {				/* was interrupted */
         fprintf( stderr, "%s -- %s interrupted, contact system manager!\a\n", program, program );
      } else {
         remove( lockfile1 );			/* remove lock */
      }
   }
   if ((sflag & FLAG_SLV) && (mode & UPDATE_MODE)) {
      mkpath( lockfile2, gip_sys, "architecture.lock" );
      if (!access( lockfile2, F_OK )) {		/* was locked */
         if (!intcount) {
            remove( lockfile2 );		/* remove lock */
         }
      }
   }
}


/*
 * dooptions removes or add an option to the option list.
 */

static	void	dooptions( char *arg, int copts )
{
   char	*ptr = NULL;				/* look for occurence in private options */
   char	*optc;					/* points to c opts */
   char	*optf;					/* points to fortran opts */

   if (copts) {					/* compiler options */
      optc = cc_opts2;				/* c options */
      optf = fc_opts2;				/* fortran options */
   } else {					/* linker options */
      optc = cc_libs;				/* c linker libs */
      optf = fc_libs;				/* fortran linker libs */
   }
   if (arg[0] == '-') {				/* might be removing an option */
      int	len = strlen( arg ) - 1;	/* length of option */

      if (!len) return;				/* no option, quit */
      ptr = strstr( optc, &arg[1] );		/* look for string */
      if (ptr == NULL) {			/* not found */
         ptr = strstr( optf, &arg[1] );		/* second try */
      }
      if (ptr != NULL) {			/* match */
         char	*p = strstr( ptr, " " );	/* end of option */
         int	l;				/* length of option */

         if (p == NULL) {			/* last option */
            l = strlen( ptr );			/* the end */
         } else {				/* there are more options */
            l = p - ptr;			/* here is the end */
         }
         if (ptr != optc && ptr != optf) {	/* first option */
            if (ptr[-1] != ' ') ptr = NULL;	/* no blank */
         }
         if (ptr != NULL) {			/* there is still a chance */
            if (l != len) ptr = NULL;		/* no way! */
         }
      }
   }
   if (ptr != NULL) {			/* matching option */
      int	l1 = strlen( arg );		/* the length */
      int	l2 = strlen( ptr ) + 1;		/* the rest */

      if (l2 - l1) {
         memmove( ptr, &ptr[l1], l2 - l1 );
      } else {
         ptr[0] = 0;
      }
   } else {					/* append options */
      strcat( optc, " " ); strcat( optc, arg );
      strcat( optf, " " ); strcat( optf, arg );
   }
}


/*
 * hostinfo displays some info about the current host.
 */

static	void	hostinfo( FILE *f )
{
   struct hostent	*hp;
   struct hostent	*gethostbyname( );
   time_t		now = time( NULL );

   fprintf( f, "USERNAME      : %s\n", username );
   fprintf( f, "DATE          : %s", ctime( &now ) );
   fprintf( f, "HOSTNAME      : " );
   hp = gethostbyname( chost );
   if ( hp != NULL ) {
      if ( hp->h_name != NULL ) {
         fprintf( f, "%s", hp->h_name );
      } else {
         fprintf( f, "%s", chost );
      }
      if ( hp->h_aliases != NULL ) {
         int	n = 0;

         fprintf( f, " (" );
         while ( hp->h_aliases[n] != NULL ) {
            fprintf( f,  " %s", hp->h_aliases[n] );
            n++;
         }
         fprintf( f, " )" );
      }
      fprintf( f,  "\n" );
      if ( (hp->h_addr_list != NULL) && (hp->h_length == sizeof(int))) {
         int	n = 0;

         fprintf( f, "ADDRESS       :" );
         while ( hp->h_addr_list[n] != NULL ) {
            int	addr1, addr;

            addr1 = (*((int *) hp->h_addr_list[n]));
            addr = ntohl( addr1 );
            fprintf( f, " %d.%d.%d.%d", (addr & 0xff000000) >> 24, (addr & 0x00ff0000) >> 16, (addr & 0x0000ff00) >> 8, (addr & 0x000000ff)  );
            n++;
         }
         fprintf( f, "\n" );
      }
   } else {
      fprintf( f, "%s\n", chost );
   }
   fprintf( f, "ARCHITECTURE  : %s\n", OS_ARCHITECTURE );
   /*
    * get mail address of local manager.
    */
   fprintf( f, "MANAGER       : %s\n", lmgrmail );
#if	defined(__alliant__)
#else
   {
      struct utsname	name;

      if (uname( &name ) != -1) {
         fprintf( f, "SYSNAME       : %s\n", name.sysname );
         fprintf( f, "NODENAME      : %s\n", name.nodename );
         fprintf( f, "RELEASE       : %s\n", name.release );
         fprintf( f, "VERSION       : %s\n", name.version );
         fprintf( f, "MACHINE       : %s\n", name.machine );
      }
   }
#endif
   fprintf( f, "CC            : %s\n", cc_name );
   fprintf( f, "CC OPTIONS 1  : %s\n", cc_opts1 );
   fprintf( f, "CC OPTIONS 2  : %s\n", cc_opts2 );
   fprintf( f, "CC OPTIONS LD : %s\n", cc_libs );
   fprintf( f, "FC            : %s\n", fc_name );
   fprintf( f, "FC OPTIONS 1  : %s\n", fc_opts1 );
   fprintf( f, "FC OPTIONS 2  : %s\n", fc_opts2 );
   fprintf( f, "FC OPTIONS LD : %s\n", fc_libs );
   fprintf( f, "AS            : %s\n", as_cmd );
   fprintf( f, "AR ADD        : %s\n", ar_add );
   fprintf( f, "AR DEL        : %s\n", ar_del );
   fprintf( f, "AR LEN        : %d\n", ar_len );
   fprintf( f, "RANLIB        : %s\n", ranlib );
   fprintf( f, "NM            : %s\n", nm_cmd );
   fprintf( f, "X11 OPTIONS   : %s\n", x11_opts );
   fprintf( f, "X11 OPTIONS L : %s\n", x11_libs );
   fprintf( f, "XT OPTIONS L  : %s\n", xt_libs );
}


/*
 * getregisternumber tries to read the local register number. If not
 * present, it tries to claim a register number.
 */

static	int	getregisternumber( void )
{
   FILE	*rf;
   char	cmd[MAXCMD];
   char	registerfile[FILENAME_MAX+1];
   int	r = 0;

   if (!(sflag & FLAG_FTP)) return( 0 );
   mkpath( registerfile, gip_tmp, "gftp.out" );
   sprintf( cmd, "%s/gftp > %s", gip_exe, registerfile );
   r = system( cmd );
   rf = fopen( registerfile, "r" );
   if (rf != NULL) {
      int	ch;

      while (!r && (ch = fgetc( rf )) != EOF) {
         if (ch == '#') {
            while ((ch = fgetc( rf )) != EOF && isdigit( ch )) {
               r = 10 * r + ch - '0';
            }
         }
      }
      fclose( rf );
   }
   remove( registerfile );
   return( r );
}


/*
 * sendstatus writes the status in the bookkeeper file to a file
 * which is send to the GIPSY source server.
 */

static	void	sendstatus( FILE *rf )
{
   FILE	*b;
   char	a[MAXARCH][MAXCHAR+1];			/* the archtectures */
   char	bookkeeper[FILENAME_MAX];
   char	name[MAXCHAR+1];
   int	umask, dmask, emask;
   int	narch;

   mkpath( bookkeeper, gip_sys, "bookkeeper" );
   b = fopenl( bookkeeper, "r" );
   if (b == NULL) {
      fprintf( stderr, "%s -- cannot open %s!\a\n", program, bookkeeper );
      fprintf( rf, "ERROR         : Cannot open %s\n", bookkeeper );
      return;
   }
   narch = xscanf( b, "%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s",
      a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9],
      a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18], a[19] );
   if (narch < 0) {
      fprintf( stderr, "%s -- No architecture found!\a\n", program );
      fprintf( rf, "ERROR         : No architectures found\n" );
      return;
   } else if (narch > MAXARCH) {
      fprintf( stderr, "%s -- Too many architectures! Inform GIPSY MANAGER!\a\n", program );
      fprintf( rf, "ERROR         : Too many architectures\n" );
      return;
   }
   while (xscanf( b, "%s %d %d %d", name, &umask, &dmask, &emask ) == 4) {
      if (emask) {
         int	n;

         fprintf( rf, "ERROR         : Error compiling %s for", name );
         for ( n = 0; n < narch; n++ ) {
            if (emask & (1 << n)) {
               fprintf( rf, " %s" , a[n] );
            }
         }
         fprintf( rf, "\n" );
      }
   }
   fclosel( b, bookkeeper );
}


/*
 * updatefromremote gets the history file from the gipsy-source-server
 * and compares its contents with the local history file. If there
 * are new versions of sources, they will be installed.
 */

static	int	updatefromremote( int mmode )
{
   FILE		*rf = NULL;
   FILE		*f = NULL;
   char		filename1[FILENAME_MAX+1];
   char		filename2[FILENAME_MAX+1];
   char		gftp[FILENAME_MAX+1];
   char		regfile1[FILENAME_MAX+1];
   char		regfile2[FILENAME_MAX+1];
   char		name[MAXCHAR+1];
   char		type[MAXCHAR+1];
   char		date[MAXCHAR+1];
   char		user[MAXCHAR+1];
   char		ownr[MAXCHAR+1];
   int		size;
   int		vers;
   int		n;
   int		nf;
   int		np = 0;
   int		npn = 0;
   int		ns, nt;
   int		r = 0;
   int		registernumber = 0;
   parent	*pn = dummy3;
   parent	p[MAXSOURCE];

   check_server( );				/* check for new server */
   mmode |= UPDATE_MODE;
   if (setup_remote( mmode )) return( FATAL );
   if (!(mmode & IMPORT_MODE)) {
      mkpath( gftp, gip_exe, "gftp" );
      if (access( gftp, X_OK )) {
         fprintf( stderr, "%s -- no executable %s present!\a\n", program, gftp );
         return( 0 );
      }
   }
   mkpath( filename1, rem_sys, "history" );
   mkpath( filename2, gip_tmp, "history" );
   if ( getfile( filename1, filename2, 0, mmode ) ) {
      /*
       * Do only the local updates.
       */
      fprintf( stderr, "%s -- Cannot contact GIPSY Source Server, doing local update!\a\n", program );
      r = updatesub( );
      if (!r) r = updateapp( );
      return( r );
   }
   getlocalmanager( );				/* get mail address of local manager */
   registernumber = getregisternumber( );
   /*
    * Are there any messages for local manager?
    */
   {
      char	msgfile1[FILENAME_MAX+1];
      char	msgfile2[FILENAME_MAX+1];

      sprintf( msgfile1, "%s/manager.%5.5d", rem_adm, registernumber );
      mkpath( msgfile2, gip_tmp, "manager" );
      if (!getfile( msgfile1, msgfile2, -1, mmode )) {
         char	cmd[MAXCMD+1];

         sprintf( cmd, "mail %s < %s", lmgrmail, msgfile2 );
         docmd( cmd );
      }
   }
   /*
    * Check for new version of COPYRIGHT and README.
    */
   {
      char	*files[] = {
         "COPYRIGHT",
         "README",
      };
      int	i;

      for ( i = 0; i < (sizeof(files) / sizeof(char *)); i++ ) {
         FILE	*f;
         char	line1[MAXCHAR+1];
         char	line2[MAXCHAR+1];
         char	name1[FILENAME_MAX+1];
         char	name2[FILENAME_MAX+1];
         char	name3[FILENAME_MAX+1];
	 int	doget = 0;

         mkpath( name1, gip_sys, files[i] );
         f = fopen( name1, "r" );
         if ( f != NULL ) {
            sprintf( line1, "%s %s", files[i], gipsyrelease );
            readline( line2, MAXCHAR, f );
            fclose( f );
            doget = strcmp( line1, line2 );
         } else {
            doget = 1;
         }
         if (doget) {
            mkpath( name2, rem_sys, files[i] );
            mkpath( name3, gip_tmp, files[i] );
            if (!getfile( name2, name3, -1, mmode )) {
               cp( name3, name1 );
               remove( name3 );
            }
         }
      }
   }
   mkpath( regfile1, gip_tmp, "register" );
   mkpath( regfile2, rem_adm, "register" );
   if (!(mmode & IMPORT_MODE)) rf = fopen( regfile1, "w" );
   hostinfo( rf );
   mkpath( filename1, gip_sys, "history" );
   f = fopenl( filename1, "r" );
   if (f == NULL) {
      fprintf( stderr, "%s -- cannot open %s!\a\n", program, filename1 );
      if (!(mmode & IMPORT_MODE)) {
         fprintf( rf, "ERROR         : Cannot open %s\n", filename1 );
         fclose( rf );
         putfile( regfile1, regfile2 );
      }
      return( FATAL );
   }
   while ((nf = xscanf( f, "%s %s %d %s %s %s %d", name, type, &vers, date, user, ownr, &size ) ) >= 6) {
      int	ftyp = ftype( name );
      int	utyp = 1;

      if (ftyp == FTYPE_UNKNOWN) continue;	/* skip this weird entry */
      if (ftyp & (FTYPE_BUG | FTYPE_FIX)) {	/* old file types */
         char	oldname[FILENAME_MAX];

         mkpath( oldname, gip_mis, name );
         if (access( oldname, F_OK )) {
            remove( oldname );
         }
         continue;				/* skip this weird entry */
      }
      for (n = 0; n < np && strcmp( name, p[n].fname ); n++);
      if (n == MAXSOURCE) {
         fprintf( stderr, "%s -- too many sources!\a\n", program );
         fclosel( f, filename1 );
         if (!(mmode & IMPORT_MODE)) {
            fprintf( rf, "ERROR         : Too many sources in %s\n", filename1 );
            fclose( rf );
            putfile( regfile1, regfile2 );
         }
         return( FATAL );
      }
      if (n == np) {
         np++;
         strcpy( p[n].fname, name );
         p[n].fpath[0] = 0;
         p[n].ftype = ftyp;
      }
      strcpy( p[n].uname, ownr );
      strcpy( p[n].uname_old, ownr );
      p[n].version_old = vers;
      p[n].version = vers;
      while (utyp < MAXUTYPES && strcmp( type, utypes[utyp] )) utyp += 1;
      getupath( &p[n], utyp );
      if (nf > 6) {
         p[n].size = size;
      } else {
         p[n].size = 0;
      }
   }
   fclosel( f, filename1 );
   f = fopen( filename2, "r" );
   if (f == NULL) {
      fprintf( stderr, "%s -- cannot open %s!\a\n", program, filename2 );
      if (!(mmode & IMPORT_MODE)) {
         fprintf( rf, "ERROR         : Cannot open %s\n", filename2 );
         fclose( rf );
         putfile( regfile1, regfile2 );
      }
      remove( filename2 );
      return( FATAL );
   }
   while ((nf = xscanf( f, "%s %s %d %s %s %s %d", name, type, &vers, date, user, ownr, &size )) >= 7) {
      int	ftyp = ftype( name );
      int	utyp = 1;

      if (ftyp == FTYPE_UNKNOWN) continue;
      if (ftyp & (FTYPE_BUG | FTYPE_FIX)) continue;
      for (n = 0; n < np && strcmp( name, p[n].fname ); n++);
      if (n == MAXSOURCE) {
         fprintf( stderr, "%s -- too many sources!\a\n", program );
         fclose( f );
         remove( filename2 );
         if (!(mmode & IMPORT_MODE)) {
            fprintf( rf, "ERROR         : Too many sources in %s\n", filename2 );
            fclose( rf );
            putfile( regfile1, regfile2 );
         }
         return( FATAL );
      }
      if (n == np) {
         np++;
         strcpy( p[n].fname, name );
         p[n].version_old = -1;
         p[n].uname_old[0] = 0;
         p[n].ftype = ftyp;
         while (utyp < MAXUTYPES && strcmp( type, utypes[utyp] )) utyp += 1;
         getupath( &p[n], utyp );
         p[n].fpath[0] = 0;
      }
      strcpy( p[n].uname, ownr );
      p[n].version = vers;
      p[n].size = size;
   }
   fclose( f );
   remove( filename2 );
   for ( npn = 0, n = 0; n < np; n++) {
      FILE	*h;
      int	mode = 0;
      time_t	now = time( NULL );		/* current time */

      if ( p[n].version_old < p[n].version ) {
         if (!strcmp( p[n].uname, "WASTEBASKET" ) && p[n].version_old < 0) {
            h = fopenl( filename1, "a" );
            if (h == NULL) {
               fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename1 );
               if (!(mmode & IMPORT_MODE)) {
                  fprintf( rf, "ERROR         : Cannot open %s\n", filename1 );
                  fclose( rf );
                  putfile( regfile1, regfile2 );
               }
               return( FATAL );
            }
            werr(fprintf( h, "%s:%s:%d:%s:%s:%s:%d\n", p[n].fname, utypes[p[n].utype], p[n].version, ttostr( now ), username, p[n].uname, p[n].size ));
            fclosel( h, filename1 );
            putfilesize( filename1 );		/* update size history file */
         } else if (!strcmp( p[n].uname, "WASTEBASKET" )) {
            if (!strcmp( p[n].uname_old, "WASTEBASKET" )) {
               h = fopenl( filename1, "a" );
               if (h == NULL) {
                  fprintf( stderr, "%s -- Cannot open %s!\a\n", program, filename1 );
                  if (!(mmode & IMPORT_MODE)) {
                     fprintf( rf, "ERROR         : Cannot open %s\n", filename1 );
                     fclose( rf );
                     putfile( regfile1, regfile2 );
                  }
                  return( FATAL );
               }
               werr(fprintf( h, "%s:%s:%d:%s:%s:%s:%d\n", p[n].fname, utypes[p[n].utype], p[n].version, ttostr( now ), username, p[n].uname_old, p[n].size ));
               fclosel( h, filename1 );
               putfilesize( filename1 );	/* update size history file */
            } else {
               strcpy( p[n].uname, p[n].uname_old );
               mode = DELETE_MODE;
            }
         } else {
            mode = INSTALL_MODE;
         }
      } else if (strcmp( p[n].uname, p[n].uname_old )) {
         if (!strcmp( p[n].uname, "WASTEBASKET")) {
            mode = DELETE_MODE;
            strcpy( p[n].uname, p[n].uname_old );
         }
      }
      if (mode) {
         strcpy( pn[npn].fpath, p[n].rpath );
         pn[npn].size = p[n].size;
         strcpy( pn[npn].fname, p[n].fname );
         pn[npn].utype = p[n].utype;
         strcpy( pn[npn].uname, p[n].uname );
         pn[npn].version = p[n].version;
         pn[npn].status = 0;
         pn[npn].ftype = ftype( p[n].fname );
         pn[npn].upath = p[n].upath;
         pn[npn].rpath = p[n].rpath;
         if (p[n].utype == UTYPE_SUB) {
            mode |= ( SUBROUTINE_MODE | COMPILE_MODE );
         } else if (p[n].utype == UTYPE_TSK) {
            mode |= COMPILE_MODE;
         }
         pn[npn].mode = ( mode | mmode | NOSORT_MODE );
         npn += 1;
      }
   }
   /*
    * first we select the subroutines and other files.
    */
   for ( ns = n = 0; n < npn; n++ ) {
      if ((pn[n].utype != UTYPE_TSK) && (pn[n].mode & DELETE_MODE)) {
         p[ns++] = pn[n];
      }
   }
   for ( n = 0; n < npn; n++ ) {
      if ((pn[n].utype != UTYPE_TSK) && !(pn[n].mode & DELETE_MODE)) {
         p[ns++] = pn[n];
      }
   }
   for ( nt = n = 0; n < npn; n++ ) {
      if ((pn[n].utype == UTYPE_TSK) && (pn[n].mode & DELETE_MODE)) {
         p[ns+(nt++)] = pn[n];
      }
   }
   for ( n = 0; n < npn; n++ ) {
      if ((pn[n].utype == UTYPE_TSK) && !(pn[n].mode & DELETE_MODE)) {
         p[ns+(nt++)] = pn[n];
      }
   }
   if (ns) {
      r = action( p, ns );
      {
         int	ne, nm = ns;

         do {
            ne = 0;
            for ( n = 0; n < nm; n++ ) {
               if ( p[n].status ) {
                  p[n].status = 0;
                  if (n != ne ) p[ne] = p[n];
                  ne += 1;
               }
            }
            if (ne >= nm) {
               fprintf( stderr, "%s -- endless loop!\a\n", program );
               fprintf( stdout, "The following files could not be installed:\n" );
               for ( n = 0; n < ne; n++ ) {
                  if (!(mmode & IMPORT_MODE)) {
                     fprintf( rf, "ERROR         : Could not install %s\n", p[n].fname );
                  }
                  fprintf( stdout, "%s\n", p[n].fname );
               }
               updatesub( );
               updateapp( );
               if (!(mmode & IMPORT_MODE)) {
                  sendstatus( rf );
                  fclose( rf );
                  putfile( regfile1, regfile2 );
               }
               return( FATAL );
            }
            nm = ne;
            if (nm) {
               fprintf( stdout, "%s -- retrying %d routines!\a\n", program, nm );
               r = action( p, nm );
            }
         } while ( nm );
      }
   }
   if (!r) r = updatesub( );
   /*
    * next we do the applications
    */
   if (!r && nt) {
      if (ns) {
         for ( n = 0; n < nt; n++ ) {
            p[n] = p[n+ns];
         }
      }
      r = action( p, nt );			/* do IT */
      {
         int	ne, nm = nt;

         do {
            ne = 0;
            for ( n = 0; n < nm; n++ ) {
               if ( p[n].status ) {
                  p[n].status = 0;
                  if (n != ne ) p[ne] = p[n];
                  ne += 1;
               }
            }
            if (ne >= nm) {
               fprintf( stderr, "%s -- endless loop!\a\n", program );
               fprintf( stdout, "The following files could not be installed:\n" );
               for ( n = 0; n < ne; n++ ) {
                  if (!(mmode & IMPORT_MODE)) {
                     fprintf( rf, "ERROR         : Could not install %s\n", p[n].fname );
                  }
                  fprintf( stdout, "%s\n", p[n].fname );
               }
               updateapp( );
               if (!(mmode & IMPORT_MODE)) {
                  sendstatus( rf );
                  fclose( rf );
                  putfile( regfile1, regfile2 );
               }
               return( FATAL );
            }
            nm = ne;
            if (nm) {
               fprintf( stdout, "%s -- retrying %d applications!\a\n", program, nm );
               r = action( p, nm );
            }
         } while ( nm );
      }
   }
   if (!r) r = updateapp( );
   remove( filename2 );
   if (!(mmode & IMPORT_MODE)) {
      sendstatus( rf );
      fclose( rf );
      putfile( regfile1, regfile2 );
   }
   return( r );
}


/*
 * cleantmp cleans the $gip_tmp directory of tmp.* files.
 */

static	int	cleantmp( void )
{
   DIR			*dirp;
#if     defined(__sysv__) | defined(__linux__) | defined(__APPLE__)
   struct dirent	*dp;			/* directory entry pointer */
#else
   struct direct	*dp;			/* directory entry pointer */
#endif

   dirp = opendir( gip_tmp );
   if (dirp == NULL) {
      fprintf( stderr, "%s -- cannot open %s!\a\n", program, gip_tmp );
      return( EXIT_FAILURE );
   }
   while ((dp = readdir( dirp )) != NULL) {
      char	entry[FILENAME_MAX+1];

      strcpy( entry, dp->d_name );
      if ( strstr( entry, "tmp." ) == entry ) {
         char	filename[FILENAME_MAX+1];

         mkpath( filename, gip_tmp, entry );
         remove( filename );
         rewinddir( dirp );
      }
   }
   closedir( dirp );
   return( EXIT_SUCCESS );
}


/*
 * check checks the GIPSY files.
 */

static	int	check( void )
{
   DIR		*dirp;
   FILE		*h, *o;
   char		history[FILENAME_MAX+1];
   char		offspringf[FILENAME_MAX+1];
   char		name[MAXCHAR+1];
   char		type[MAXCHAR+1];
   char		date[MAXCHAR+1];
   char		user[MAXCHAR+1];
   char		ownr[MAXCHAR+1];
   char		child[MAXCHAR+1];
   char		source[MAXCHAR+1];
   int		nchild = 0;
   int		size;
   int		vers;
   int		l, m, n, nf;
   int		nd = 0, np = 0;
   parent	*p = dummy3;
#if     defined(__sysv__) | defined(__linux__) | defined(__APPLE__)
   struct dirent *dp;                           /* directory entry pointer */
#else
   struct direct *dp;                           /* directory entry pointer */
#endif

   mkpath( history, gip_sys, "history" );
   h = fopen( history, "r" );
   if (h == NULL) {
      fprintf( stderr, "%s -- cannot open %s!\a\n", program, history );
      return( EXIT_FAILURE );
   }
   while ((nf = xscanf( h, "%s %s %d %s %s %s %d", name, type, &vers, date, user, ownr, &size ) ) >= 6) {
      int	ftyp = ftype( name );
      int	utyp = 1;

      for ( n = 0; n < np && strcmp( name, p[n].fname ); n++);
      if (n == MAXSOURCE) {
         fprintf( stderr, "%s -- too many sources!\a\n", program );
         fclose( h );
         return( EXIT_FAILURE );
      }
      if (n == np) {
         np++;
         strcpy( p[n].fname, name );
         p[n].fpath[0] = 0;
         p[n].ftype = ftyp;
      }
      strcpy( p[n].uname, ownr );
      strcpy( p[n].uname_old, ownr );
      p[n].version_old = vers;
      p[n].version = vers;
      p[n].status  = 0;
      p[n].nchild  = 0;
      while (utyp < MAXUTYPES && strcmp( type, utypes[utyp] )) utyp += 1;
      getupath( &p[n], utyp );
      if (nf > 6) {
         p[n].size = size;
      } else {
         p[n].size = 0;
      }
   }
   fclose( h );
   for ( n = 0; n < np; n++) {
      if (!strcmp( p[n].uname, "WASTEBASKET")) nd++;
   }
   fprintf( stdout, "Found %5d files in history\n", np );
   mkpath( offspringf, gip_sys, "offspring" );
   o = fopen( offspringf, "r" );
   if ( o == NULL ) {
      fprintf( stderr, "%s -- cannot open %s!\a\n", program, offspringf );
      return( EXIT_FAILURE );
   }
   while ((nf = xscanf( o, "%s %s", source, child )) >= 2) {
      for ( n = 0; n < np && strcmp( p[n].fname, source ); n++);
      if (n == np) {
         fprintf( stderr, "%s -- %s not a GIPSY source!\a\n", program, source );
      } else {
         p[n].child = realloc( p[n].child, (p[n].nchild + 1) * sizeof( offspring ) );
         strcpy( p[n].child[p[n].nchild].fname, child );
         p[n].child[p[n].nchild].ftype = ftype( child );
         p[n].child[p[n].nchild].status = 0;
         p[n].nchild++;
         nchild++;
      }
   }
   fclose( o );
   fprintf( stdout, "Found %5d files in offspring\n", nchild );
   for (n = 0; n < np; n++) {
      getchilddest( &p[n] );
   }
   for (l = 0; l < MAXCHECKDIRS; l++ ) {
      char      *dir =  checkdirs[l];

      fprintf( stdout, ". . . . checking files in %s\n", dir );
      dirp = opendir( dir );
      if (dirp == NULL) {
         fprintf( stderr, "%s -- cannot open %s!\a\n", program, dir );
         return( EXIT_FAILURE );
      }
      while ((dp = readdir( dirp )) != NULL) {
         char	entry[FILENAME_MAX+1];

         strcpy( entry, dp->d_name );
         if (entry[0] != '.') {
            int		d = 0;

            for ( n = 0; n < np && strcmp( p[n].fname, entry ); n++ );
            if (n < np) {
               if (p[n].upath == dir) {
                  char	filename[FILENAME_MAX+1];

                  mkpath( filename, dir, entry );
                  if (getfilesize( filename ) == p[n].size) {
                     d = p[n].status = 1;
                  } else {
                     d = p[n].status = 2;
                  }
               } else {
                  d = p[n].status = 3;
               }
            } else {
               for ( n = 0; !d && n < np; n++ ) {
                  for (m = 0; m < p[n].nchild && strcmp( p[n].child[m].fname, entry ); m++);
                  if (m < p[n].nchild) {
                     if (!strcmp( p[n].child[m].path, dir )) {
                        d = p[n].child[m].status = 1;
                     } else {
                        d = p[n].child[m].status = 3;
                     }
                  }
               }
            }
            if (d == 0) {
               if (dir == gip_sys) {
                  for ( n = 0; n < MAXNOCHECKINSYS && strstr( entry, nocheckinsys[n] ) == NULL; n++ );
                  if (n < MAXNOCHECKINSYS) d = 1;
               } else if (dir == gip_exe) {
                  char	*ptr;

                  if ((ptr = strstr( entry, ".old" )) != NULL && strlen( ptr ) == 4) {
                     char	oentry[FILENAME_MAX+1];

                     strcpy( oentry, entry );
                     oentry[strlen(entry)-4] = 0;
                     for ( n = 0; !d && n < np; n++ ) {
                        for (m = 0; m < p[n].nchild && strcmp( p[n].child[m].fname, oentry ); m++);
                        if (m < p[n].nchild) d = 1;
                     }
                  }
               }
            }
            if (!d) {
               fprintf( stdout, "%s is not a GIPSY file\n", entry );
            }
         }
      }
      closedir( dirp );
   }
   for (n = 0; n < np; n++) {
      if (!strcmp( p[n].uname, "WASTEBASKET" )) {
         if (p[n].status != 0) {
            fprintf( stdout, "%s should have been removed\n", p[n].fname );
         }
         for ( m = 0; m < p[n].nchild; m++ ) {
            if (p[n].child[m].status != 0) {
               fprintf( stdout, "%s should have been removed\n", p[n].child[m].fname );
            }
         }
      } else {
         if (p[n].status == 0) {
            fprintf( stdout, "%s is missing\n", p[n].fname );
         } else if (p[n].status == 2) {
            fprintf( stdout, "%s has wrong size\n", p[n].fname );
         } else if (p[n].status == 3) {
            fprintf( stdout, "%s in wrong directory\n", p[n].fname );
         }
         for ( m = 0; m < p[n].nchild; m++ ) {
            if (p[n].child[m].status == 0) {
               fprintf( stdout, "%s (child of %s) is missing\n", p[n].child[m].fname, p[n].fname );
            } else if (p[n].child[m].status == 3) {
               fprintf( stdout, "%s (child of %s) in wrong directory\n", p[n].child[m].fname, p[n].fname );
            }
         }
      }
   }
   return( EXIT_SUCCESS );
}


/*
 * export exports modified files to a directory which contains an old
 * history file.
 */

static	int	export( int mode )
{
   FILE		*h1, *h2;
   char		filename1[FILENAME_MAX+1];
   char		filename2[FILENAME_MAX+1];
   char		history1[FILENAME_MAX+1];
   char		history2[FILENAME_MAX+1];
   char		name[MAXCHAR+1];
   char		type[MAXCHAR+1];
   char		date[MAXCHAR+1];
   char		user[MAXCHAR+1];
   char		ownr[MAXCHAR+1];
   int		size;
   int		vers;
   int		n, nf, np;
   parent	*p = dummy3;

   if (setup_remote( mode )) return( FATAL );
   mkpath( history1, gip_sys, "history" );
   mkpath( history2, rem_sys, "history" );
   h2 = fopen( history2, "r" );
   if ( h2 == NULL ) {
      fprintf( stderr, "%s -- Cannot open %s!\n\a", program, history2 );
      return( EXIT_FAILURE );
   }
   np = 0;
   while ((nf = xscanf( h2, "%s %s %d %s %s %s %d", name, type, &vers, date, user, ownr, &size ) ) >= 6) {
      int	ftyp = ftype( name );
      int	utyp = 1;

      if (ftyp == FTYPE_UNKNOWN) continue;	/* skip this weird entry */
      if (ftyp & (FTYPE_BUG | FTYPE_FIX)) continue;
      for (n = 0; n < np && strcmp( name, p[n].fname ); n++);
      if (n == MAXSOURCE) {
         fprintf( stderr, "%s -- too many sources!\a\n", program );
         fclose( h2 );
         return( FATAL );
      }
      if (n == np) {
         np++;
         strcpy( p[n].fname, name );
         p[n].fpath[0] = 0;
         p[n].ftype = ftyp;
      }
      strcpy( p[n].uname, ownr );
      strcpy( p[n].uname_old, ownr );
      p[n].version_old = vers;
      p[n].version = vers;
      while (utyp < MAXUTYPES && strcmp( type, utypes[utyp] )) utyp += 1;
      getupath( &p[n], utyp );
      if (nf > 6) {
         p[n].size = size;
      } else {
         p[n].size = 0;
      }
   }
   fclose( h2 );
   h1 = fopen( history1, "r" );
   if ( h1 == NULL ) {
      fprintf( stderr, "%s -- Cannot open %s!\n\a", program, history1 );
      return( EXIT_FAILURE );
   }
   while ((nf = xscanf( h1, "%s %s %d %s %s %s %d", name, type, &vers, date, user, ownr, &size )) >= 7) {
      int	ftyp = ftype( name );
      int	utyp = 1;

      if (ftyp == FTYPE_UNKNOWN) continue;
      if (ftyp & (FTYPE_BUG | FTYPE_FIX)) continue;
      for (n = 0; n < np && strcmp( name, p[n].fname ); n++);
      if (n == MAXSOURCE) {
         fprintf( stderr, "%s -- too many sources!\a\n", program );
         fclose( h1 );
         return( FATAL );
      }
      if (n == np) {
         np++;
         strcpy( p[n].fname, name );
         p[n].version_old = -1;
         p[n].uname_old[0] = 0;
         p[n].ftype = ftyp;
         while (utyp < MAXUTYPES && strcmp( type, utypes[utyp] )) utyp += 1;
         getupath( &p[n], utyp );
         p[n].fpath[0] = 0;
      }
      strcpy( p[n].uname, ownr );
      p[n].version = vers;
      p[n].size = size;
   }
   fclose( h1 );
   mkpath( filename1, gip_sys, "README" );
   mkpath( filename2, rem_sys, "README" );
   cp( filename1, filename2 );
   mkpath( filename1, gip_sys, "COPYRIGHT" );
   mkpath( filename2, rem_sys, "COPYRIGHT" );
   cp( filename1, filename2 );
   h2 = fopen( history2, "a" );
   if ( h2 == NULL ) {
      fprintf( stderr, "%s -- Cannot open %s!\n\a", program, history2 );
      return( EXIT_FAILURE );
   }
   for ( n = 0; n < np; n++ ) {
      int	mode = 0;
      time_t	now = time( NULL );

      if ( p[n].version_old < p[n].version ) {
         if (!strcmp( p[n].uname, "WASTEBASKET" ) && p[n].version_old < 0) {
            werr(fprintf( h2, "%s:%s:%d:%s:%s:%s:%d\n", p[n].fname, utypes[p[n].utype], p[n].version, ttostr( now ), p[n].uname, p[n].uname, p[n].size ));
         } else if (!strcmp( p[n].uname, "WASTEBASKET" )) {
            if (!strcmp( p[n].uname_old, "WASTEBASKET" )) {
               werr(fprintf( h2, "%s:%s:%d:%s:%s:%s:%d\n", p[n].fname, utypes[p[n].utype], p[n].version, ttostr( now ), username, p[n].uname_old, p[n].size ));
            } else {
               strcpy( p[n].uname, p[n].uname_old );
               mode = DELETE_MODE;
            }
         } else {
            mode = INSTALL_MODE;
         }
      } else if (strcmp( p[n].uname, p[n].uname_old )) {
         if (!strcmp( p[n].uname, "WASTEBASKET")) {
            mode = DELETE_MODE;
            strcpy( p[n].uname, p[n].uname_old );
         } else {
            werr(fprintf( h2, "%s:%s:%d:%s:%s:%s:%d\n", p[n].fname, utypes[p[n].utype], p[n].version, ttostr( now ), username, p[n].uname, p[n].size ));
         }
      }
      switch( mode ) {
         case DELETE_MODE: {
            werr(fprintf( h2, "%s:%s:%d:%s:%s:WASTEBASKET:%d\n", p[n].fname, utypes[p[n].utype], p[n].version, ttostr( now ), username, p[n].size ));
            break;
         }
         case INSTALL_MODE: {
            werr(fprintf( h2, "%s:%s:%d:%s:%s:%s:%d\n", p[n].fname, utypes[p[n].utype], p[n].version, ttostr( now ), username, p[n].uname, p[n].size ) );
            mkpath( filename1, p[n].upath, p[n].fname );
            mkpath( filename2, p[n].rpath, p[n].fname );
            cp( filename1, filename2 );
            break;
         }
         default: {
            break;
         }
      }
   }
   fclose( h2 );
   return( EXIT_SUCCESS );
}


/*
 * main_body does the main action.
 */

static	int	main_body( int argc, char *argv[], int mode )
{
   int		copts = 1;			/* turn on compiler options */
   int		ccount = 0;			/* number of compilables */
   int		ocount = 0;			/* number of options */
   int		ftyp;				/* type of file */
   int		iarg;				/* argument counter */
   int		nbase = 0;			/* reset */
   int		r = 0;				/* return code */
   static parent	base[MAXSOURCE];		/* the sources */

   for (iarg = 1; iarg < argc; iarg++) {	/* loop over command line arguments */
      if (!strcmp( argv[iarg], "-c" )) {
         mode |= SUBROUTINE_MODE;		/* subroutine mode on */
      } else if (!strcmp( argv[iarg], "-cc" )) {
         if (++iarg < argc) {
            cc = argv[iarg];			/* name of c compiler */
            cc_opts2[0] = 0;			/* no default options */
         } else {
            fprintf( stderr, "%s -- -cc not followed by compiler name!\a\n", program );
            return( EXIT_FAILURE );		/* exit with error */
         }
      } else if (!strcmp( argv[iarg], "-check" )) {
         mode |= CHECK_MODE;			/* check GIPSY files */
      } else if (!strcmp( argv[iarg], "-copts" )) {
         copts = 1;				/* next switches are compiler switches */
      } else if (!strcmp( argv[iarg], "-delete" )) {
         mode |= DELETE_MODE;			/* set delete mode */
      } else if (!strcmp( argv[iarg], "-export" )) {
         mode |= EXPORT_MODE;			/* set export mode */
         if (++iarg < argc) {
            strcpy( exp_dir, argv[iarg] );
         } else {
            fprintf( stderr, "%s -- -export not followed by name of directory!\a\n", program );
            return( EXIT_FAILURE );		/* exit with error */
         }
      } else if (!strcmp( argv[iarg], "-fc" )) {
         if (++iarg < argc) {
            fc = argv[iarg];			/* name of fortran compiler */
            fc_opts2[0] = 0;			/* no default options */
         } else {
            fprintf( stderr, "%s -- -fc not followed by compiler name!\a\n", program );
            return( EXIT_FAILURE );		/* exit with error */
         }
      } else if (!strcmp( argv[iarg], "-history" )) {
         mode |= HISTORY_MODE;
      } else if (!strcmp( argv[iarg], "-if" )) {
         if (++iarg < argc) {			/* next argument is filename */
            FILE	*f;			/* file descriptor */
            char	fname[MAXCHAR+1];	/* name of file */

            f = fopen( argv[iarg], "r" );
            if (f == NULL) {			/* file does not exist */
               fprintf( stderr, "%s -- Cannot open %s!\a\n", program, argv[iarg] );
               return( EXIT_FAILURE );	/* exit with error */
            }
            while (readline( fname, MAXCHAR, f )) {
               int	ftyp = ftype( fname );

               if (ftyp & FTYPE_O) {
                  ocount += 1;			/* increase counter */
                  strcat( uobjects, " " );
                  strcat( uobjects, fname );
               } else if (ftyp & (FTYPE_BUG | FTYPE_FIX)) {
                  fprintf( stderr, "%s -- %s not allowed anymore!\a\n", program, fname );
               } else {
                  if (nbase == MAXSOURCE) {
                     fprintf( stderr, "%s -- Not enough buffer space!\a\n", program );
                     exit( EXIT_FAILURE );
                  }
                  strcpy( base[nbase].fname, fname );
                  base[nbase].done = 0;
                  base[nbase].nchild = 0;
                  base[nbase].version = -1;
                  base[nbase].status = 0;
                  base[nbase].child = NULL;
                  base[nbase].ftype = ftype( fname );
                  if (base[nbase].ftype & (FTYPE_C | FTYPE_F | FTYPE_MAKE | FTYPE_S | FTYPE_SHL | FTYPE_SRC)) {
                     base[nbase].mode = COMPILE_MODE;
                  } else {
                     base[nbase].mode = 0;
                  }
                  nbase += 1;			/* increase number of arguments */
               }
            }
         } else {
            fprintf( stderr, "%s -- -if not followed by filename!\a\n", program );
            return( EXIT_FAILURE );	/* exit with error */
         }
     } else if (!strcmp( argv[iarg], "-import" )) {
         mode |= IMPORT_MODE;			/* set import mode */
         if (++iarg < argc) {
            strcpy( imp_dir, argv[iarg] );
         } else {
            fprintf( stderr, "%s -- -import not followed by name of directory!\a\n", program );
            return( EXIT_FAILURE );		/* exit with error */
         }
       } else if (!strcmp( argv[iarg], "-install" )) {
         mode |= INSTALL_MODE;			/* install mode on */
      } else if (!strcmp( argv[iarg], "-lopts" )) {
         copts = 0;				/* next switches are loader switches */
      } else if (!strcmp( argv[iarg], "-o" )) {
         mode |= OUTPUT_MODE;			/* output mode on */
         if (++iarg < argc) {			/* next argument */
            output = argv[iarg];		/* output name of executable */
         } else {
            fprintf( stderr, "%s -- -o not followed by filename!\a\n", program );
            return( EXIT_FAILURE );		/* exit with error */
         }
      } else if (!strcmp( argv[iarg], "-purify" )) {
         mode |= PURIFY_MODE;			/* purify mode on */
      } else if (!strcmp( argv[iarg], "-rebuild" )) {
         mode |= REBUILD_MODE;			/* rebuild mode on */
      } else if (!strcmp( argv[iarg], "-reserve" )) {
         mode |= RESERVE_MODE;			/* reserve mode on */
      } else if (!strcmp( argv[iarg], "-retrieve" )) {
         mode |= RETRIEVE_MODE;			/* retrieve mode on */
      } else if (!strcmp( argv[iarg], "-sub" )) {
         mode |= SUBROUTINE_MODE;		/* subroutine mode on */
      } else if (!strcmp( argv[iarg], "-sysgen" )) {
         mode |= SYSGEN_MODE;			/* sysgen mode */
      } else if (!strcmp( argv[iarg], "-unlock" )) {
         mode |= UNLOCK_MODE;			/* unlock */
      } else if (!strcmp( argv[iarg], "-unreserve" )) {
         mode |= UNRESERVE_MODE;		/* unreserve mode on */
      } else if (!strcmp( argv[iarg], "-update" )) {
         mode |= UPDATE_MODE;			/* update mode on */
      } else if (!strcmp( argv[iarg], "-version" )) {
         mode |= VERSION_MODE;			/* version mode on */
         if (++iarg < argc) {
            version = atoi( argv[iarg] );
         } else {
            fprintf( stderr, "%s -- -version not followed by number!\a\n", program );
            return( EXIT_FAILURE );		/* exit with error */
         }
      } else if (!strcmp( argv[iarg], "-X" )) {
         With_x11 = 1;				/* add X incs. and libs */
      } else if (!strcmp( argv[iarg], "-XT" )) {
         With_x11 = 2;				/* add X incs. and toolkit libs */
      } else if (!((ftyp = ftype( argv[iarg] )) & (FTYPE_UNKNOWN | FTYPE_EXE))) {
         if (ftyp & FTYPE_O) {			/* -> loader switch */
            ocount += 1;			/* increase counter */
            strcat( uobjects, " " );
            strcat( uobjects, argv[iarg] );
         } else if (ftyp & (FTYPE_BUG | FTYPE_FIX)) {
            fprintf( stderr, "%s -- %s not allowed anymore!\a\n", program, fname );
         } else {
            if (nbase == MAXSOURCE) {
               fprintf( stderr, "%s -- Not enough buffer space!\a\n", program );
               exit( EXIT_FAILURE );
            }
            strcpy( base[nbase].fname, argv[iarg] );
            base[nbase].done = 0;
            base[nbase].nchild = 0;		/* no children yet */
            base[nbase].child = NULL;		/* no children yet */
            base[nbase].status = 0;		/* reset */
            base[nbase].version = -1;		/* no version */
            base[nbase].ftype = ftyp;		/* save file type */
            if (base[nbase].ftype & (FTYPE_C | FTYPE_F | FTYPE_MAKE | FTYPE_S | FTYPE_SHL | FTYPE_SRC)) {
               base[nbase].mode = COMPILE_MODE;
            } else {
               base[nbase].mode = 0;
            }
            nbase += 1;				/* increase number of arguments */
         }
      } else {					/* options */
         ocount += 1;				/* increase counter */
         dooptions( argv[iarg], copts );	/* add/remove option */
      }
   }
   if (nbits( mode & (CHECK_MODE | INSTALL_MODE | UPDATE_MODE | REBUILD_MODE | RESERVE_MODE | UNRESERVE_MODE | DELETE_MODE | RETRIEVE_MODE | OUTPUT_MODE | SYSGEN_MODE | EXPORT_MODE | IMPORT_MODE)) > 1) {
      fprintf( stderr, "%s -- Illegal combination of options!\a\n", program );
      return( EXIT_FAILURE );
   }
   if ((mode & EXPORT_MODE) && (nbits( mode ) != 1)) {
      fprintf( stderr, "%s -- No other options allowed with -export!\a\n", program );
      return( EXIT_FAILURE );
   }
   if ((mode & IMPORT_MODE) && (nbits( mode ) != 1)) {
      fprintf( stderr, "%s -- No other options allowed with -import!\a\n", program );
      return( EXIT_FAILURE );
   }
   if ((mode & CHECK_MODE)) {
      if (nbits( mode ) != 1) {
         fprintf( stderr, "%s -- other arguments will be discarded!\a\n", program );
      }
      return( check( ) );
   }
   if (nbits( mode & (SUBROUTINE_MODE | OUTPUT_MODE) ) > 1) {
      fprintf( stderr, "%s -- Clash of options (-o and -c or -sub)!\a\n", program );
      return( EXIT_FAILURE );
   }
   if (mode & ( INSTALL_MODE | UPDATE_MODE | REBUILD_MODE | RESERVE_MODE | SYSGEN_MODE | UNRESERVE_MODE | DELETE_MODE )) {
      int	n, nf = 0;

      if (ocount) {				/* not allowed */
         fprintf( stderr, "%s -- Compiler/loader options on command line!\a\n", program );
         return( EXIT_FAILURE );
      }
      if ((mode & (UPDATE_MODE | SYSGEN_MODE)) && nbase) {
         fprintf( stderr, "%s -- No source name allowed on command line!\a\n", program );
         return( EXIT_FAILURE );
      }
      for (n = 0; n < nbase; n++) {
         int	l = strlen( base[n].fname );

         if ((mode & INSTALL_MODE) && access( base[n].fname, F_OK )) {
            fprintf( stderr, "%s -- %s not found!\a\n", program, base[n].fname );
            nf += 1;
         }
         while (l && base[n].fname[l-1] != '/') l--;
         if (l) {
            strncpy( base[n].fpath, base[n].fname, l );
            strcpy( base[n].fname, &base[n].fname[l] );
         }
         base[n].fpath[l] = 0;
         base[n].mode |= mode;				/* set mode */
         if (base[n].ftype & (FTYPE_MAKE | FTYPE_OPT | FTYPE_S | FTYPE_SYN )) {
            fprintf( stderr, "%s -- %s not allowed on command line!\a\n", program, base[n].fname );
            nf += 1;
         } else {
            getutype( &base[n] );
         }
         if (base[n].mode & COMPILE_MODE) ccount++;
      }
      if (nf) return( EXIT_FAILURE );
      if (nbase && !ccount) no_update = 1;
   } else if ( mode & IMPORT_MODE ) {
      mode |= UPDATE_MODE;
   } else {
      int	n, nf = 0;

      mode |= PRIVATE_MODE;
      for (n = 0; n < nbase; n++) {
         base[n].mode |= mode;				/* set mode */
         getutype( &base[n] );
         if (base[n].ftype & (FTYPE_OPT | FTYPE_SYN)) {
            fprintf( stderr, "%s -- %s not allowed on command line!\a\n", program, base[n].fname );
            nf += 1;
         } else if (!(mode & (HISTORY_MODE | RETRIEVE_MODE))) {
            if (access( base[n].fname, F_OK )) {
               fprintf( stderr, "%s -- %s does not exist!\a\n", program, base[n].fname );
               nf += 1;
            } else if (base[n].ftype & (FTYPE_C | FTYPE_F | FTYPE_S | FTYPE_SHL | FTYPE_SRC)) {
               ccount += 1;
            }
         }
      }
      if (nf) return( EXIT_FAILURE );
   }
   if (!(mode & PRIVATE_MODE) && !(sflag & FLAG_INS)) {
      fprintf( stderr, "%s -- You may not install on this machine!\a\n", program );
      return( EXIT_FAILURE );
   }
   if ((mode & PRIVATE_MODE) && !(sflag & FLAG_COM)) {
      fprintf( stderr, "%s -- You may not compile on this machine!\a\n", program );
      return( EXIT_FAILURE );
   }
   if ((mode & IMPORT_MODE) && !(sflag & FLAG_SLV)) {
      fprintf( stderr, "%s -- can only import in slave!\a\n", program );
      return( EXIT_FAILURE );
   }
   if ((mode & IMPORT_MODE) && (sflag & FLAG_FTP)) {
      fprintf( stderr, "%s -- can not import in ftp mode!\a\n", program );
      return( EXIT_FAILURE );
   }
   if ((mode & PRIVATE_MODE) && (euid != ruid)) {
      euid = ruid;
      seteuid( euid );				/* remove the privilege */
   }
   if ((mode & EXPORT_MODE)) {
      return( export( mode ) );
   }
   {
      char	*pwd = NULL;			/* current working dir */

#if	defined(__bsd__)			/* BSD */
      pwd = getwd( cwd );			/* pwd */
#endif
#if	defined(__sysv__) | defined(__linux__) | defined(__APPLE__)			/* SYSV */
      pwd = getcwd( cwd, FILENAME_MAX );	/* pwd */
#endif
      if (pwd == NULL) {			/* error */
         fprintf( stderr, "%s -- Cannot obtain current working directory!\a\n", program );
         return( EXIT_FAILURE );		/* fatal error */
      }
   }
   if (ruid == euid && guid == euid && !(mode & PRIVATE_MODE)) {
      mode |= MASTER_MODE;			/* it's the boss */
   }
   if (mode & MASTER_MODE) {
      fprintf( stderr, "%s -- running in Master Mode!\a\n", program );
   } else if (mode & (SYSGEN_MODE | IMPORT_MODE)) {
      fprintf( stderr, "%s -- must be running in Master Mode!\a\n", program );
      return( EXIT_FAILURE );
   }
   if ((mode & SYSGEN_MODE) && nbits(sflag & (FLAG_SLV | FLAG_FTP)) != 2) {
      fprintf( stderr, "%s -- can only generate system in slave and ftp mode!\a\n", program );
      fprintf( stderr, "%s -- check $gip_loc/clients file\n", program );
      return( EXIT_FAILURE );
   }
   if (!(mode & PRIVATE_MODE)) {
      int	umask( );

      chdir( gip_tmp );				/* change directory */
      umask( 022 );				/* set user mask */
   }
   if (!(mode & PRIVATE_MODE) && !(mode & MASTER_MODE)) {
      if ( (guid != euid) || (sflag & FLAG_SUI) ) {
         if (!(sflag & FLAG_SLV) || (mode & (REBUILD_MODE | UPDATE_MODE | SYSGEN_MODE))) {
            fprintf( stderr, "%s -- You don't have the privilege to install!\a\n", program );
            return( EXIT_FAILURE );
         }
      }
   }
   if ( mode & PURIFY_MODE ) {
      if (!(mode & PRIVATE_MODE)) {
         fprintf( stderr, "%s -- Purify only for private purposes!\a\n", program );
         return( EXIT_FAILURE );
      }
      if (mode & SUBROUTINE_MODE) {
         fprintf( stderr, "%s -- Purify NOT for subroutines!\a\n", program );
         return( EXIT_FAILURE );
      }
      if (!(ccount)) {
         fprintf( stderr, "%s -- There is nothing to purify!\a\n", program );
         return( EXIT_FAILURE );
      }
   }
   if (!(mode & PRIVATE_MODE) && !(mode & MASTER_MODE)) {
      if (initprog( )) {
         return( EXIT_FAILURE );
      }
   }
   if (!(mode & PRIVATE_MODE) && !(mode & MASTER_MODE)) {
      int	n = 0;

      while (n < nprog && strcmp( prog[n].user, username )) n++;
      if (n == nprog) {
         fprintf( stderr, "%s -- You (%s) are not allowed to install! Sorry!!\a\n", program, username );
         return( EXIT_FAILURE );
      }
      strcpy( usermail, prog[n].addr );
   }
   if (lock( mode )) return( FATAL );				/* lock compile */
   if (!(mode & PRIVATE_MODE)) {
      if ( (sflag & FLAG_SLV) && !(mode & (UPDATE_MODE | REBUILD_MODE | SYSGEN_MODE))) {
         r = action_remote( base, nbase );
      } else if ( (sflag & FLAG_FTP) && (mode & (UPDATE_MODE | SYSGEN_MODE))) {
         r = updatefromremote( mode );
      } else if (!(sflag & FLAG_FTP) && (mode & IMPORT_MODE)) {
         r = updatefromremote( mode );
      } else {
         r = history( base, nbase, mode );
         if (!r) {
            if (mode & (INSTALL_MODE | REBUILD_MODE | UPDATE_MODE | DELETE_MODE)) {
               r = updatesub( );
               if (!r) r = action( base, nbase );
               if (!r && ((mode & MASTER_MODE) || ((mode & (UPDATE_MODE | SUBROUTINE_MODE)) == UPDATE_MODE))) r = updateapp( );
            } else {
               r = reserve( base, nbase );
            }
         }
      }
   } else if (mode & RETRIEVE_MODE) {
      if (sflag & FLAG_SLV) {
         r = action_remote( base, nbase );
      } else {
         r = history( base, nbase, mode );
         if (!r) r = retrieve( base, nbase );
      }
   } else if (mode & HISTORY_MODE ) {
      r = history( base, nbase, mode );
   } else {
      r = action( base, nbase );
   }
   if (!(mode & PRIVATE_MODE)) r = cleantmp( );	/* clean up the mess */
   unlock( mode );				/* unlock compile */
   if (r) {
      return( EXIT_FAILURE );			/* exit with error */
   } else {
      return( EXIT_SUCCESS );			/* exit with success */
   }
}


/*
 * main checks the command line arguments and decides what to do.
 */

int	main( int argc, char *argv[] )
{
   char	*file = NULL;
   int	mode = 0;
   int	m;
   int	n;
   int	r = 0;

   uobjects[0] = 0;				/* set end of text */
   signal( SIGINT, handler );			/* set interrupt handler */
   setbuf( stdout, NULL );			/* no buffering */
   getuser( username );				/* get name of user */
   n = strlen( argv[0] );			/* length */
   program = argv[0] + n;			/* get name of program */
   while (n && *(--program) != '/') n--;	/* go back until / */
   if (*program == '/') program++;		/* it was a / */
   if (init( )) return( EXIT_FAILURE );		/* initialize things */
   if (argc < 2) {				/* give user some help */
      return( help( ) );			/* exit */
   }
   euid = geteuid( );				/* get effective uid */
   guid = getguid( );				/* get uid of GIPSY account */
   ruid = getuid( );				/* get real uid */
   for ( n = 1; n < argc; n++ ) {
      if (!strcmp( argv[n], "-mail" )) {
         mode = MAIL_MODE;
         if (++n == argc) {
            fprintf( stderr, "%s -- no mail address specified!\a\n", program );
            return( EXIT_FAILURE );
         } else {
            strcpy( usermail, argv[n] );
         }
         if (argc != 4) {
            fprintf( stderr, "%s -- Must have 4 arguments on command line!\a\n", program );
            return( EXIT_FAILURE );
         }
         if (euid != guid) {
            fprintf( stderr, "%s -- Must run in MASTER mode!\a\n", program );
            return( EXIT_FAILURE );
         }
      } else {
         file = argv[n];
      }
   }
   if (!(mode)) {
      r = main_body( argc, argv, mode );
   } else {
      FILE	*f;
      char	*myargv[MAXARG];
      char	cmd[MAXCMD];
      int	myargc = 0;
      int	n;

      r = unpack( file );
      if (!r) {
         char	*s;

         r = remove( file );
         f = fopen( "compile.arg", "r" );
         if (f == NULL) {
            fprintf( stderr, "%s -- no file compile.arg found!\a\n", program );
            return( EXIT_FAILURE );
         }
         if (fgets( cmd, MAXCMD, f ) == NULL) {
            fprintf( stderr, "%s -- error reading compile.arg!\a\n", program );
            return( EXIT_FAILURE );
         }
         fclose( f );
         r = remove ( "compile.arg" );
         s = strtok( cmd, " \n" );
         do {
            myargv[myargc++] = s;
            s = strtok( NULL, " \n" );
         } while ( s != NULL );
         for ( m = n = 0; n < myargc; n++) {
            if (!strcmp( myargv[n], "-user" )) {
               if (++n == myargc) {
                  fprintf( stderr, "%s -- username not on command line!\a\n", program );
                  return( EXIT_FAILURE );
               }
               strcpy( username, myargv[n] );
            } else {
               if (m != n) myargv[m] = myargv[n];
               m += 1;
            }
         }
         if (m == myargc) {
            fprintf( stderr, "%s -- -user switch not found!\a\n", program );
            return( EXIT_FAILURE );
         }
         myargc = m;
         fprintf( stdout, "%s", myargv[0] );
         for ( n = 1; n < myargc; n++) {
            fprintf( stdout, " %s", myargv[n] );
         }
         fprintf( stdout, "\n" );
         if (initprog()) {
            return( EXIT_FAILURE );
         }
         n = 0;
         while (n < nprog && strcmp( prog[n].addr, usermail ) && strcmp( prog[n].user, username ) ) n++;
         if (n == nprog) {
            fprintf( stderr, "%s -- You (%s) at %s are not allowed to install! Sorry!!\a\n", program, username, usermail );
            return( EXIT_FAILURE );
         }
         r = main_body( myargc, myargv, mode );
      }
   }
   if (r) r = EXIT_FAILURE; else r = EXIT_SUCCESS;
   return( r );
}
