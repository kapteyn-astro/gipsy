/* signal.h

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            signal.dc3

Header:       signal.h

Purpose:      Defines ANSI C signal handling.

File:         signal.h

Author:       K.G. Begeman

Use:          #include "signal.h"

Declares:     void(*signal(int sig, void (*handler)(int)))(int)
              int raise(int sig )

Defines:      SIG_DFL, SIG_IGN and some other signals.

Warning:      System dependent! At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP9000, SILICON GRAPHICS,
              SUN and DEC VMS.

Updates:      Jul 20, 1991: KGB, Document created.
              Jun  8, 2009: JPT, Linux now includes from system

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_SIGNAL_H)
#if	defined(__aix__)
#elif	defined(__alliant__)
#elif	defined(__alpha__)
#elif	defined(__convex__)
#elif	defined(__cray__)
#elif	defined(__hpux__)
#elif	defined(__mips__)
#elif	defined(__sgi__)
#elif	defined(__sun__)
#elif	defined(__vms__)
#else
#include	<signal.h>			/* from system */
#if	!defined(_SIGNAL_H)
#define	_SIGNAL_H
#endif
#endif
#endif

#if	!defined(_SIGNAL_H)
#define	_SIGNAL_H

#if	defined(__aix__)			/* AIX */

#define	SIGHUP	   1	/* hangup, generated when terminal disconnects */
#define	SIGINT	   2	/* interrupt, generated from terminal special char */
#define	SIGQUIT	   3	/* (*) quit, generated from terminal special char */
#define	SIGILL	   4	/* (*) illegal instruction (not reset when caught)*/
#define	SIGTRAP	   5	/* (*) trace trap (not reset when caught) */
#define	SIGABRT    6	/* (*) abort process */
#define SIGEMT	   7	/* EMT intruction */
#define	SIGFPE	   8	/* (*) floating point exception */
#define	SIGKILL	   9	/* kill (cannot be caught or ignored) */
#define	SIGBUS	  10	/* (*) bus error (specification exception) */
#define	SIGSEGV	  11	/* (*) segmentation violation */
#define	SIGSYS	  12	/* (*) bad argument to system call */
#define	SIGPIPE	  13	/* write on a pipe with no one to read it */
#define	SIGALRM	  14	/* alarm clock timeout */
#define	SIGTERM	  15	/* software termination signal */
#define	SIGURG 	  16	/* (+) urgent contition on I/O channel */
#define	SIGSTOP	  17	/* (@) stop (cannot be caught or ignored) */
#define	SIGTSTP	  18	/* (@) interactive stop */
#define	SIGCONT	  19	/* (!) continue (cannot be caught or ignored) */
#define SIGCHLD   20	/* (+) sent to parent on child stop or exit */
#define SIGTTIN   21	/* (@) background read attempted from control terminal*/
#define SIGTTOU   22	/* (@) background write attempted to control terminal */
#define SIGIO	  23	/* (+) I/O possible, or completed */
#define SIGXCPU	  24	/* cpu time limit exceeded (see setrlimit()) */
#define SIGXFSZ	  25	/* file size limit exceeded (see setrlimit()) */
#define SIGMSG    27	/* input data is in the HFT ring buffer */
#define SIGWINCH  28	/* (+) window size changed */
#define SIGPWR    29	/* (+) power-fail restart */
#define SIGUSR1   30	/* user defined signal 1 */
#define SIGUSR2   31	/* user defined signal 2 */
#define SIGPROF   32	/* profiling time alarm (see setitimer) */
#define SIGDANGER 33	/* system crash imminent; free up some page space */
#define SIGVTALRM 34	/* virtual time alarm (see setitimer) */
#define SIGMIGRATE 35	/* migrate process (see TCF)*/
#define SIGPRE	  36	/* programming exception */
#define	SIGKAP    60
#define SIGGRANT  SIGKAP/* HFT monitor mode granted */
#define SIGRETRACT 61   /* HFT monitor mode should be relinguished */
#define SIGSOUND  62    /* HFT sound control has completed */
#define SIGSAK    63	/* secure attention key */
/*
 * additional signal names supplied for compatibility, only 
 */
#define SIGIOINT SIGURG	/* printer to backend error signal */
#define SIGAIO	SIGIO	/* base lan i/o */
#define SIGPTY  SIGIO	/* pty i/o */
#define SIGIOT  SIGABRT /* abort (terminate) process */ 
#define	SIGCLD	SIGCHLD	/* old death of child signal */
#define SIGLOST	SIGIOT	/* old BSD signal ?? */

#define	SIG_DFL		(void (*)(int))0
#define	SIG_IGN		(void (*)(int))1
#define SIG_HOLD	(void (*)(int))2	/* not valid as argument 
						   to sigaction or sigvec */
#define SIG_CATCH	(void (*)(int))3	/* not valid as argument 
						   to sigaction or sigvec */
#define SIG_ERR		(void (*)(int))-1

#elif	defined(__alliant__)			/* ALLIANT */

#define	SIGHUP		1	/* hangup */
#define	SIGINT		2	/* interrupt */
#define	SIGQUIT		3	/* quit */
#define	SIGILL		4	/* illegal instruction */
#define	SIGTRAP		5	/* trace trap */
#define	SIGIOT		6	/* IOT instruction */
#define	SIGABRT		SIGIOT	/* compatibility */
#define	SIGEMT		7	/* EMT instruction */
#define	SIGFPE		8	/* floating point exception */
#define	SIGKILL		9	/* kill */
#define	SIGBUS		10	/* bus error exception */
#define	SIGSEGV		11	/* segmentation violation */
#define	SIGSYS		12	/* bad srgument to system call */
#define	SIGPIPE		13	/* write on pipe with no one to read it */
#define	SIGALRM		14	/* alarm clock */
#define	SIGTERM		15	/* software termination signal from kill */
#define	SIGURG		16	/* urgent condition on IO channel */
#define	SIGSTOP		17	/* sendable stop signal not from tty */
#define	SIGTSTP		18	/* stop signal from tty */
#define	SIGCONT		19	/* continue a stopped process */
#define	SIGCHLD		20	/* to parent on child stop or exit */
#define	SIGCLD		SIGCHLD	/* System V name for SIGCHLD */
#define	SIGTTIN		21	/* to readers pgrp upon background tty read */
#define	SIGTTOU		22	/* like TTIN for output */
#define	SIGIO		23	/* input/output possible signal */
#define	SIGPOLL		SIGIO	/* System V name for SIGIO */
#define	SIGXCPU		24	/* exceeded CPU time limit */
#define	SIGXFSZ		25	/* exceeded file size limit */
#define	SIGVTALRM	26	/* virtual time alarm */
#define	SIGPROF		27	/* profiling time alarm */
#define	SIGWINCH	28	/* window size changes */
#define	SIGLOST		29	/* resource lost (eg, record-lock lost) */
#define	SIGUSR1		30	/* user defined signal 1 */
#define	SIGUSR2		31	/* user defined signal 2 */

#define	SIG_ERR		((void (*) ())-1)	/* error */
#define	SIG_DFL		((void (*) ())0)	/* default */
#define	SIG_IGN		((void (*) ())1)	/* ignore */

#define	raise		raise_x			/* from xclib */
#define	signal		signal_x		/* from xclib */

#elif	defined(__alpha__)			/* DEC ALPHA */

#include	<standards.h>

#define	SIGHUP	   1	/* hangup, generated when terminal disconnects */
#define	SIGINT	   2	/* interrupt, generated from terminal special char */
#define	SIGQUIT	   3	/* (*) quit, generated from terminal special char */
#define	SIGILL	   4	/* (*) illegal instruction (not reset when caught)*/
#define	SIGTRAP	   5	/* (*) trace trap (not reset when caught) */
#define	SIGABRT    6	/* (*) abort process */
#define SIGEMT	   7	/* EMT instruction */
#define	SIGFPE	   8	/* (*) floating point exception */
#define	SIGKILL	   9	/* kill (cannot be caught or ignored) */
#define	SIGBUS	  10	/* (*) bus error (specification exception) */
#define	SIGSEGV	  11	/* (*) segmentation violation */
#define	SIGSYS	  12	/* (*) bad argument to system call */
#define	SIGPIPE	  13	/* write on a pipe with no one to read it */
#define	SIGALRM	  14	/* alarm clock timeout */
#define	SIGTERM	  15	/* software termination signal */
#define	SIGURG 	  16	/* (+) urgent contition on I/O channel */
#define	SIGSTOP	  17	/* (@) stop (cannot be caught or ignored) */
#define	SIGTSTP	  18	/* (@) interactive stop */
#define	SIGCONT	  19	/* (!) continue (cannot be caught or ignored) */
#define SIGCHLD   20	/* (+) sent to parent on child stop or exit */
#define SIGTTIN   21	/* (@) background read attempted from control terminal*/
#define SIGTTOU   22	/* (@) background write attempted to control terminal */
#define SIGIO	  23	/* (+) I/O possible, or completed */
#define SIGXCPU	  24	/* cpu time limit exceeded (see setrlimit()) */
#define SIGXFSZ	  25	/* file size limit exceeded (see setrlimit()) */
#define SIGVTALRM 26	/* virtual time alarm (see setitimer) */
#define SIGPROF   27	/* profiling time alarm (see setitimer) */
#define SIGWINCH  28	/* (+) window size changed */
#define SIGINFO   29    /* information request */
#define SIGUSR1   30	/* user defined signal 1 */
#define SIGUSR2   31	/* user defined signal 2 */
#define SIGIOINT SIGURG	/* printer to backend error signal */
#define SIGAIO	SIGIO	/* base lan i/o */
#define SIGPTY  SIGIO	/* pty i/o */
#define	SIGPOLL	SIGIO	/* STREAMS version of this signal */
#define SIGIOT  SIGABRT /* abort (terminate) process */ 
#define	SIGLOST	SIGIOT	/* old BSD signal ?? */

#define SIG_ERR		(void (*)(int))-1
#define	SIG_DFL		(void (*)(int))0
#define	SIG_IGN		(void (*)(int))1
#define SIG_HOLD        (void (*)(int))2        /* not valid as argument
                                                   to sigaction or sigvec */
#define SIG_CATCH       (void (*)(int))3        /* not valid as argument
                                                   to sigaction or sigvec */

#elif	defined(__convex__)			/* CONVEX */

#define	SIGHUP		1	/* hangup */
#define	SIGINT		2	/* interrupt */
#define	SIGQUIT		3	/* quit */
#define	SIGILL		4	/* illegal instruction */
#define	SIGTRAP		5	/* trace trap */
#define	SIGIOT		6	/* IOT instruction */
#define	SIGABRT		SIGIOT	/* compatibility */
#define	SIGEMT		7	/* EMT instruction */
#define	SIGFPE		8	/* floating point exception */
#define	SIGKILL		9	/* kill */
#define	SIGBUS		10	/* bus error exception */
#define	SIGSEGV		11	/* segmentation violation */
#define	SIGSYS		12	/* bad srgument to system call */
#define	SIGPIPE		13	/* write on pipe with no one to read it */
#define	SIGALRM		14	/* alarm clock */
#define	SIGTERM		15	/* software termination signal from kill */
#define	SIGURG		16	/* urgent condition on IO channel */
#define	SIGSTOP		17	/* sendable stop signal not from tty */
#define	SIGTSTP		18	/* stop signal from tty */
#define	SIGCONT		19	/* continue a stopped process */
#define	SIGCHLD		20	/* to parent on child stop or exit */
#define	SIGCLD		SIGCHLD	/* System V name for SIGCHLD */
#define	SIGTTIN		21	/* to readers pgrp upon background tty read */
#define	SIGTTOU		22	/* like TTIN for output */
#define	SIGIO		23	/* input/output possible signal */
#define	SIGPOLL		SIGIO	/* System V name for SIGIO */
#define	SIGXCPU		24	/* exceeded CPU time limit */
#define	SIGXFSZ		25	/* exceeded file size limit */
#define	SIGVTALRM	26	/* virtual time alarm */
#define	SIGPROF		27	/* profiling time alarm */
#define	SIGWINCH	28	/* window size changes */
#define	SIGLOST		29	/* resource lost (eg, record-lock lost) */
#define	SIGUSR1		30	/* user defined signal 1 */
#define	SIGUSR2		31	/* user defined signal 2 */

#define	SIG_ERR		((void (*) ())-1)	/* error */
#define	SIG_DFL		((void (*) ())0)	/* default */
#define	SIG_IGN		((void (*) ())1)	/* ignore */

#define	signal		signal_x		/* from xclib */

#elif	defined(__cray__)			/* CRAY */

#include	"/usr/include/signal.h"		/* use standard */

#elif	defined(__hpux__)			/* HP UNIX */

#define	SIGHUP		1	/* hangup */
#define	SIGINT		2	/* interrupt */
#define	SIGQUIT		3	/* quit */
#define	SIGILL		4	/* illegal instruction */
#define	SIGTRAP		5	/* trace trap */
#define	SIGIOT		6	/* IOT instruction */
#define	SIGABRT		SIGIOT	/* compatibility */
#define	SIGEMT		7	/* EMT instruction */
#define	SIGFPE		8	/* floating point exception */
#define	SIGKILL		9	/* kill */
#define	SIGBUS		10	/* bus error exception */
#define	SIGSEGV		11	/* segmentation violation */
#define	SIGSYS		12	/* bad srgument to system call */
#define	SIGPIPE		13	/* write on pipe with no one to read it */
#define	SIGALRM		14	/* alarm clock */
#define	SIGTERM		15	/* software termination signal from kill */
#define	SIGUSR1		16	/* user defined signal 1 */
#define	SIGUSR2		17	/* user defined signal 2 */
#define	SIGCHLD		18	/* to parent on child stop or exit */
#define	SIGCLD		SIGCHLD	/* System V name for SIGCHLD */
#define	SIGPWR		19	/* power state indication */
#define	SIGVTALRM	20	/* virtual time alarm */
#define	SIGPROF		21	/* profiling time alarm */
#define	SIGIO		22	/* input/output possible signal */
#define	SIGPOLL		SIGIO	/* System V name for SIGIO */
#define	SIGWINDOW	23	/* windowing signal */
#define	SIGSTOP		24	/* sendable stop signal not from tty */
#define	SIGTSTP		25	/* stop signal from tty */
#define	SIGCONT		26	/* continue a stopped process */
#define	SIGTTIN		27	/* to readers pgrp upon background tty read */
#define	SIGTTOU		28	/* like TTIN for output */
#define	SIGURG		29	/* urgent condition on IO channel */
#define	SIGLOST		30	/* resource lost (eg, record-lock lost) */
#define	SIGRESERVE	31	/* Save for future use */
#define	SIGDIL		32	/* DIL signal */

#define	SIG_ERR		((void (*) ())-1)	/* error */
#define	SIG_DFL		((void (*) ())0)	/* default */
#define	SIG_IGN		((void (*) ())1)	/* ignore */

#elif	defined(__linux__)			/* LINUX */

#define _NSIG             32
#define NSIG		_NSIG

#define SIGHUP		 1
#define SIGINT		 2
#define SIGQUIT		 3
#define SIGILL		 4
#define SIGTRAP		 5
#define SIGABRT		 6
#define SIGIOT		 6
#define SIGUNUSED	 7
#define SIGFPE		 8
#define SIGKILL		 9
#define SIGUSR1		10
#define SIGSEGV		11
#define SIGUSR2		12
#define SIGPIPE		13
#define SIGALRM		14
#define SIGTERM		15
#define SIGSTKFLT	16
#define SIGCHLD		17
#define SIGCONT		18
#define SIGSTOP		19
#define SIGTSTP		20
#define SIGTTIN		21
#define SIGTTOU		22
#define SIGIO		23
#define SIGPOLL		SIGIO
#define SIGURG		SIGIO
#define SIGXCPU		24
#define SIGXFSZ		25
#define SIGVTALRM	26
#define SIGPROF		27
#define SIGWINCH	28
#define SIGPWR		30
#define SIGBUS		SIGUNUSED

/* Type of a signal handler.  */
typedef void (*__sighandler_t)(int);

#define SIG_DFL	((__sighandler_t)0)	/* default signal handling */
#define SIG_IGN	((__sighandler_t)1)	/* ignore signal */
#define SIG_ERR	((__sighandler_t)-1)	/* error return from signal */

#elif	defined(__mips__)			/* DEC ULTRIX */

#define	SIGHUP		1	/* hangup */
#define	SIGINT		2	/* interrupt */
#define	SIGQUIT		3	/* quit */
#define	SIGILL		4	/* illegal instruction */
#define	SIGTRAP		5	/* trace trap */
#define	SIGIOT		6	/* IOT instruction */
#define	SIGABRT		SIGIOT	/* compatibility */
#define	SIGEMT		7	/* EMT instruction */
#define	SIGFPE		8	/* floating point exception */
#define	SIGKILL		9	/* kill */
#define	SIGBUS		10	/* bus error exception */
#define	SIGSEGV		11	/* segmentation violation */
#define	SIGSYS		12	/* bad srgument to system call */
#define	SIGPIPE		13	/* write on pipe with no one to read it */
#define	SIGALRM		14	/* alarm clock */
#define	SIGTERM		15	/* software termination signal from kill */
#define	SIGURG		16	/* urgent condition on IO channel */
#define	SIGSTOP		17	/* sendable stop signal not from tty */
#define	SIGTSTP		18	/* stop signal from tty */
#define	SIGCONT		19	/* continue a stopped process */
#define	SIGCHLD		20	/* to parent on child stop or exit */
#define	SIGCLD		SIGCHLD	/* System V name for SIGCHLD */
#define	SIGTTIN		21	/* to readers pgrp upon background tty read */
#define	SIGTTOU		22	/* like TTIN for output */
#define	SIGIO		23	/* input/output possible signal */
#define	SIGPOLL		SIGIO	/* System V name for SIGIO */
#define	SIGXCPU		24	/* exceeded CPU time limit */
#define	SIGXFSZ		25	/* exceeded file size limit */
#define	SIGVTALRM	26	/* virtual time alarm */
#define	SIGPROF		27	/* profiling time alarm */
#define	SIGWINCH	28	/* window size changes */
#define	SIGLOST		29	/* resource lost (eg, record-lock lost) */
#define	SIGUSR1		30	/* user defined signal 1 */
#define	SIGUSR2		31	/* user defined signal 2 */

#define	SIG_ERR		((void (*) ())-1)	/* error */
#define	SIG_DFL		((void (*) ())0)	/* default */
#define	SIG_IGN		((void (*) ())1)	/* ignore */

#define	raise		raise_x			/* from xclib */
#define	signal		signal_x		/* from xclib */

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

#ifndef __SIGNAL_H__
#define __SIGNAL_H__
#endif

#include	<sys/signal.h>

#elif	defined(__sun__)			/* SUN */

#define	SIGHUP		1	/* hangup */
#define	SIGINT		2	/* interrupt */
#define	SIGQUIT		3	/* quit */
#define	SIGILL		4	/* illegal instruction */
#define	SIGTRAP		5	/* trace trap */
#define	SIGIOT		6	/* IOT instruction */
#define	SIGABRT		SIGIOT	/* compatibility */
#define	SIGEMT		7	/* EMT instruction */
#define	SIGFPE		8	/* floating point exception */
#define	SIGKILL		9	/* kill */
#define	SIGBUS		10	/* bus error exception */
#define	SIGSEGV		11	/* segmentation violation */
#define	SIGSYS		12	/* bad srgument to system call */
#define	SIGPIPE		13	/* write on pipe with no one to read it */
#define	SIGALRM		14	/* alarm clock */
#define	SIGTERM		15	/* software termination signal from kill */

#ifdef	__bsd__			/* SunOS < 5.0 */

#define	SIGURG		16	/* urgent condition on IO channel */
#define	SIGSTOP		17	/* sendable stop signal not from tty */
#define	SIGTSTP		18	/* stop signal from tty */
#define	SIGCONT		19	/* continue a stopped process */
#define	SIGCHLD		20	/* to parent on child stop or exit */
#define	SIGCLD		SIGCHLD	/* System V name for SIGCHLD */
#define	SIGTTIN		21	/* to readers pgrp upon background tty read */
#define	SIGTTOU		22	/* like TTIN for output */
#define	SIGIO		23	/* input/output possible signal */
#define	SIGPOLL		SIGIO	/* System V name for SIGIO */
#define	SIGXCPU		24	/* exceeded CPU time limit */
#define	SIGXFSZ		25	/* exceeded file size limit */
#define	SIGVTALRM	26	/* virtual time alarm */
#define	SIGPROF		27	/* profiling time alarm */
#define	SIGWINCH	28	/* window size changes */
#define	SIGLOST		29	/* resource lost (eg, record-lock lost) */
#define	SIGUSR1		30	/* user defined signal 1 */
#define	SIGUSR2		31	/* user defined signal 2 */

#else				/* SunOS >= 5.0 */

#define	SIGUSR1		16	/* user defined signal 1 */
#define SIGUSR2		17	/* user defined signal 2 */
#define SIGCLD		18	/* child status change */  
#define SIGCHLD		18	/* child status change alias (POSIX) */
#define SIGPWR		19	/* power-fail restart */
#define SIGWINCH	20	/* window size change */
#define SIGURG		21	/* urgent socket condition */
#define SIGPOLL		22	/* pollable event occured */ 
#define SIGIO		SIGPOLL	/* socket I/O possible (SIGPOLL alias) */
#define SIGSTOP		23	/* stop (cannot be caught or ignored) */ 
#define SIGTSTP		24	/* user stop requested from tty */
#define SIGCONT		25	/* stopped process has been continued */
#define SIGCONT		25	/* stopped process has been continued */
#define SIGTTIN		26	/* background tty read attempted */
#define SIGTTOU		27	/* background tty write attempted */
#define SIGVTALRM	28	/* virtual timer expired */
#define SIGPROF		29	/* profiling timer expired */
#define SIGXCPU		30	/* exceeded cpu limit */
#define SIGXFSZ		31	/* exceeded file size limit */
#define SIGWAITING	32	/* process's lwps are blocked */
#define SIGLWP		33	/* special signal used by thread library */

#endif

#define	SIG_ERR		((void (*) ())-1)	/* error */
#define	SIG_DFL		((void (*) ())0)	/* default */
#define	SIG_IGN		((void (*) ())1)	/* ignore */

#ifdef	__bsd__

#define	raise		raise_x			/* from xclib */
#define	signal		signal_x		/* from xclib */

#endif

#elif	defined(__vms__)			/* DEC VMS */

#define	SIGHUP		1	/* hangup */
#define	SIGINT		2	/* interrupt */
#define	SIGQUIT		3	/* quit */
#define	SIGILL		4	/* illegal instruction */
#define	SIGTRAP		5	/* trace trap */
#define	SIGIOT		6	/* IOT instruction */
#define	SIGABRT		SIGIOT	/* compatibility */
#define	SIGEMT		7	/* EMT instruction */
#define	SIGFPE		8	/* floating point exception */
#define	SIGKILL		9	/* kill */
#define	SIGBUS		10	/* bus error exception */
#define	SIGSEGV		11	/* segmentation violation */
#define	SIGSYS		12	/* bad srgument to system call */
#define	SIGPIPE		13	/* write on pipe with no one to read it */
#define	SIGALRM		14	/* alarm clock */
#define	SIGTERM		15	/* software termination signal from kill */

#define	SIG_ERR		((void (*) ())-1)	/* error */
#define	SIG_DFL		((void (*) ())0)	/* default */
#define	SIG_IGN		((void (*) ())1)	/* ignore */

#define	raise		raise_x			/* from xclib */

#endif

extern	void	(*signal(int, void (*) (int)))(int);
extern	int	raise(int sig);

#endif
