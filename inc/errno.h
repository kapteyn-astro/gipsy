/* errno.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            errno.dc3

Header:       errno.h

Purpose:      Defines some common erros.

File:         errno.h

Author:       K.G. Begeman

Use:          #include "errno.h"

Defines:      int errno     contains number of last error.

Warning:      System dependent! At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP 9000, IBM AIX,
              SILICON GRAPHICS, SUN and VMS.

Updates:      May  1, 1990: KGB, Document created.
              June 2, 2004: JPT, Linux now includes system errno.h.
#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_ERRNO_H)
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
#include	<errno.h>			/* from system */
#if	!defined(_ERRNO_H)
#define	_ERRNO_H
#endif
#endif
#endif

#if	!defined(_ERRNO_H)
#define	_ERRNO_H

#if	defined(__aix__)		/* AIX */

extern	int	errno;

#define	EPERM	1	/* Operation not permitted		*/
#define	ENOENT	2	/* No such file or directory		*/
#define	ESRCH	3	/* No such process			*/
#define	EINTR	4	/* interrupted system call		*/
#define	EIO	5	/* I/O error				*/
#define	ENXIO	6	/* No such device or address		*/
#define	E2BIG	7	/* Arg list too long			*/
#define	ENOEXEC	8	/* Exec format error			*/
#define	EBADF	9	/* Bad file descriptor			*/
#define	ECHILD	10	/* No child processes			*/
#define	EAGAIN	11	/* Resource temporarily unavailable	*/
#define	ENOMEM	12	/* Not enough space			*/
#define	EACCES	13	/* Permission denied			*/
#define	EFAULT	14	/* Bad address				*/
#define	ENOTBLK	15	/* Block device required		*/
#define	EBUSY	16	/* Resource busy			*/
#define	EEXIST	17	/* File exists				*/
#define	EXDEV	18	/* Improper link			*/
#define	ENODEV	19	/* No such device			*/
#define	ENOTDIR	20	/* Not a directory			*/
#define	EISDIR	21	/* Is a directory			*/
#define	EINVAL	22	/* Invalid argument			*/
#define	ENFILE	23	/* Too many open files in system	*/
#define	EMFILE	24	/* Too many open files			*/
#define	ENOTTY	25	/* Inappropriate I/O control operation	*/
#define	ETXTBSY	26	/* Text file busy			*/
#define	EFBIG	27	/* File too large			*/
#define	ENOSPC	28	/* No space left on device		*/
#define	ESPIPE	29	/* Invalid seek				*/
#define	EROFS	30	/* Read only file system		*/
#define	EMLINK	31	/* Too many links			*/
#define	EPIPE	32	/* Broken pipe				*/
#define	EDOM	33	/* Domain error within math function	*/
#define	ERANGE	34	/* Result too large			*/
#define	ENOMSG	35	/* No message of desired type		*/
#define	EIDRM	36	/* Identifier removed			*/
#define	ECHRNG	37	/* Channel number out of range		*/
#define	EL2NSYNC 38	/* Level 2 not synchronized		*/
#define	EL3HLT	39	/* Level 3 halted			*/
#define	EL3RST	40	/* Level 3 reset			*/
#define	ELNRNG	41	/* Link number out of range		*/
#define	EUNATCH 42	/* Protocol driver not attached		*/
#define	ENOCSI	43	/* No CSI structure available		*/
#define	EL2HLT	44	/* Level 2 halted			*/
#define	EDEADLK 45	/* Resource deadlock avoided		*/

#define	ENOTREADY	46	/* Device not ready		*/
#define	EWRPROTECT	47	/* Write-protected media 	*/
#define	EFORMAT		48	/* Unformatted media 		*/
#define	ENOLCK		49	/* No locks available 		*/
#define	ENOCONNECT      50      /* no connection                */
#define	ESTALE          52      /* no filesystem                */
#define	EDIST		53 	/* old, currently unused AIX errno*/ 
#define	EWOULDBLOCK     54	/* Operation would block       */
#define	EINPROGRESS     55      /* Operation now in progress */
#define	EALREADY        56      /* Operation already in progress */
#define	ENOTSOCK        57      /* Socket operation on non-socket */
#define	EDESTADDRREQ    58      /* Destination address required */
#define	EMSGSIZE        59      /* Message too long */
#define	EPROTOTYPE      60      /* Protocol wrong type for socket */
#define	ENOPROTOOPT     61      /* Protocol not available */
#define	EPROTONOSUPPORT 62      /* Protocol not supported */
#define	ESOCKTNOSUPPORT 63      /* Socket type not supported */
#define	EOPNOTSUPP      64      /* Operation not supported on socket */
#define	EPFNOSUPPORT    65      /* Protocol family not supported */
#define	EAFNOSUPPORT    66      /* Address family not supported by protocol family */
#define	EADDRINUSE      67      /* Address already in use */
#define	EADDRNOTAVAIL   68      /* Can't assign requested address */
#define	ENETDOWN        69      /* Network is down */
#define	ENETUNREACH     70      /* Network is unreachable */
#define	ENETRESET       71      /* Network dropped connection on reset */
#define	ECONNABORTED    72      /* Software caused connection abort */
#define	ECONNRESET      73      /* Connection reset by peer */
#define	ENOBUFS         74      /* No buffer space available */
#define	EISCONN         75      /* Socket is already connected */
#define	ENOTCONN        76      /* Socket is not connected */
#define	ESHUTDOWN       77      /* Can't send after socket shutdown */
#define	ETIMEDOUT       78      /* Connection timed out */
#define	ECONNREFUSED    79      /* Connection refused */
#define	EHOSTDOWN       80      /* Host is down */
#define	EHOSTUNREACH    81      /* No route to host */
#define	ERESTART	82	/* restart the system call */
#define	EPROCLIM	83	/* Too many processes */
#define	EUSERS		84	/* Too many users */
#define	ELOOP		85	/* Too many levels of symbolic links      */
#define	ENAMETOOLONG	86	/* File name too long			  */
#define	ENOTEMPTY	87	/* Directory not empty */
#define	EDQUOT		88	/* Disc quota exceeded */
#define	EREMOTE		93	/* Item is not local to host */
#define	ENOSYS		109	/* Function not implemented  POSIX */
#define	EMEDIA		110 	/* media surface error */
#define	ESOFT           111     /* I/O completed, but needs relocation */
#define	ENOATTR		112 	/* no attribute found */
#define	ESAD		113	/* security authentication denied */
#define	ENOTRUST	114	/* not a trusted program */ 

#elif	defined(__alliant__)		/* ALLIANT */

extern	volatile int	errno;		/* UNIX style error */

#define	EPERM		1		/* Not owner */
#define	ENOENT		2		/* No such file or directory */
#define	ESRCH		3		/* No such process */
#define	EINTR		4		/* Interrupted system call */
#define	EIO		5		/* I/O error */
#define	ENXIO		6		/* No such device or address */
#define	E2BIG		7		/* Arg list too long */
#define	ENOEXEC		8		/* Exec format error */
#define	EBADF		9		/* Bad file number */
#define	ECHILD		10		/* No children */
#define	EAGAIN		11		/* No more processes */
#define	ENOMEM		12		/* Not enough core */
#define	EACCES		13		/* Permission denied */
#define	EFAULT		14		/* Bad address */
#define	ENOTBLK		15		/* Block device required */
#define	EBUSY		16		/* Mount device busy */
#define	EEXIST		17		/* File exists */
#define	EXDEV		18		/* Cross-device link */
#define	ENODEV		19		/* No such device */
#define	ENOTDIR		20		/* Not a directory*/
#define	EISDIR		21		/* Is a directory */
#define	EINVAL		22		/* Invalid argument */
#define	ENFILE		23		/* File table overflow */
#define	EMFILE		24		/* Too many open files */
#define	ENOTTY		25		/* Not a typewriter */
#define	ETXTBSY		26		/* Text file busy */
#define	EFBIG		27		/* File too large */
#define	ENOSPC		28		/* No space left on device */
#define	ESPIPE		29		/* Illegal seek */
#define	EROFS		30		/* Read-only file system */
#define	EMLINK		31		/* Too many links */
#define	EPIPE		32		/* Broken pipe */
#define	EDOM		33		/* Argument too large */
#define	ERANGE		34		/* Result too large */
#define	EWOULDBLOCK	35		/* Operation would block */
#define	EINPROGRESS	36		/* Operation now in progress */
#define	EALREADY	37		/* Operation already in progress */
#define	ENOTSOCK	38		/* Socket operation on non-socket */
#define	EDESTADDRREQ	39		/* Destination address required */
#define	EMSGSIZE	40		/* Message too long */
#define	EPROTOTYPE	41		/* Protocol wrong type for socket */
#define	ENOPROTOOPT	42		/* Protocol not available */
#define	EPROTONOSUPPORT	43		/* Protocol not supported */
#define	ESOCKTNOSUPPORT	44		/* Socket type not supported */
#define	EOPNOTSUPP	45		/* Operation not supported on socket */
#define	EPFNOSUPPORT	46		/* Protocol family not supported */
#define	EAFNOSUPPORT	47		/* Address family not supported by protocol family */
#define	EADDRINUSE	48		/* Address already in use */
#define	EADDRNOTAVAIL	49		/* Can't assign requested address */
#define	ENETDOWN	50		/* Network is down */
#define	ENETUNREACH	51		/* Network is unreachable */
#define	ENETRESET	52		/* Network dropped connection on reset */
#define	ECONNABORTED	53		/* Software caused connection abort */
#define	ECONNRESET	54		/* Connection reset by peer */
#define	ENOBUFS		55		/* No buffer space available */
#define	EISCONN		56		/* Socket is already connected */
#define	ENOTCONN	57		/* Socket is not connected */
#define	ESHUTDOWN	58		/* Can't send after socket shutdown */
#define	ETOOMANYREFS	59		/* Too many references: can't splice */
#define	ETIMEDOUT	60		/* Connection timed out */
#define	ECONNREFUSED	61		/* Connection refused */
#define	ELOOP		62		/* Too many levels of symbolic links */
#define	ENAMETOOLONG	63		/* File name too long */
#define	EHOSTDOWN	64		/* Host is down */
#define	EHOSTUNREACH	65		/* No route to host */
#define	ENOTEMPTY	66		/* Directory not empty */
#define	EPROCLIM	67		/* Too many processes */
#define	EUSERS		68		/* Too many users */
#define	EDQUOT		69		/* Disc quota exceeded */
#define	ESTALE		70		/* Stale NFS file handle */
#define	EREMOTE 	71		/* Too many levels of remote in path */
#define	EDEADLK 	78		/* Deadlock condition. */
#define	ENOLCK		79		/* No record locks available. */

#elif	defined(__alpha__)		/* DEC ALPHA */

#ifdef	_REENTRANT
extern	int	*_errno();
#define	errno	(*_errno())
#else
extern	int	errno;
#endif

#define ESUCCESS        0               /* Successful */
#define EPERM		1		/* Not owner */
#define ENOENT		2		/* No such file or directory */
#define ESRCH		3		/* No such process */
#define EINTR		4		/* Interrupted system call */
#define EIO		5		/* I/O error */
#define ENXIO		6		/* No such device or address */
#define E2BIG		7		/* Arg list too long */
#define ENOEXEC		8		/* Exec format error */
#define EBADF		9		/* Bad file number */
#define ECHILD		10		/* No children */
#define EDEADLK		11		/* Operation would cause deadlock */
#define ENOMEM		12		/* Not enough core */
#define EACCES		13		/* Permission denied */
#define EFAULT		14		/* Bad address */
#define ENOTBLK		15		/* Block device required */
#define EBUSY		16		/* Mount device busy */
#define EEXIST		17		/* File exists */
#define EXDEV		18		/* Cross-device link */
#define ENODEV		19		/* No such device */
#define ENOTDIR		20		/* Not a directory*/
#define EISDIR		21		/* Is a directory */
#define EINVAL		22		/* Invalid argument */
#define ENFILE		23		/* File table overflow */
#define EMFILE		24		/* Too many open files */
#define ENOTTY		25		/* Not a typewriter */
#define ETXTBSY		26		/* Text file busy */
#define EFBIG		27		/* File too large */
#define ENOSPC		28		/* No space left on device */
#define ESPIPE		29		/* Illegal seek */
#define EROFS		30		/* Read-only file system */
#define EMLINK		31		/* Too many links */
#define EPIPE		32		/* Broken pipe */
#define EDOM		33		/* Argument too large */
#define ERANGE		34		/* Result too large */
#define EWOULDBLOCK	35		/* Operation would block */
#define EAGAIN		EWOULDBLOCK	/* ditto */
#define EINPROGRESS	36		/* Operation now in progress */
#define EALREADY	37		/* Operation already in progress */
#define ENOTSOCK	38		/* Socket operation on non-socket */
#define EDESTADDRREQ	39		/* Destination address required */
#define EMSGSIZE	40		/* Message too long */
#define EPROTOTYPE	41		/* Protocol wrong type for socket */
#define ENOPROTOOPT	42		/* Protocol not available */
#define EPROTONOSUPPORT	43		/* Protocol not supported */
#define ESOCKTNOSUPPORT	44		/* Socket type not supported */
#define EOPNOTSUPP	45		/* Operation not supported on socket */
#define EPFNOSUPPORT	46		/* Protocol family not supported */
#define EAFNOSUPPORT	47		/* Address family not supported by protocol family */
#define EADDRINUSE	48		/* Address already in use */
#define EADDRNOTAVAIL	49		/* Can't assign requested address */
#define ENETDOWN	50		/* Network is down */
#define ENETUNREACH	51		/* Network is unreachable */
#define ENETRESET	52		/* Network dropped connection on reset */
#define ECONNABORTED	53		/* Software caused connection abort */
#define ECONNRESET	54		/* Connection reset by peer */
#define ENOBUFS		55		/* No buffer space available */
#define EISCONN		56		/* Socket is already connected */
#define ENOTCONN	57		/* Socket is not connected */
#define ESHUTDOWN	58		/* Can't send after socket shutdown */
#define ETOOMANYREFS	59		/* Too many references: can't splice */
#define ETIMEDOUT	60		/* Connection timed out */
#define ECONNREFUSED	61		/* Connection refused */
#define ELOOP		62		/* Too many levels of symbolic links */
#define ENAMETOOLONG	63		/* File name too long */
#define EHOSTDOWN	64		/* Host is down */
#define EHOSTUNREACH	65		/* No route to host */
#define ENOTEMPTY	66		/* Directory not empty */
#define EPROCLIM	67		/* Too many processes */
#define EUSERS		68		/* Too many users */
#define EDQUOT		69		/* Disc quota exceeded */
#define ENOLCK		77		/* No locks available */
#define ENOSYS		78		/* Function not implemented */
#define ESTALE		70		/* Stale NFS file handle */
#define EREMOTE		71		/* Too many levels of remote in path */
#define EBADRPC		72		/* RPC struct is bad */
#define ERPCMISMATCH	73		/* RPC version wrong */
#define EPROGUNAVAIL	74		/* RPC prog. not avail */
#define EPROGMISMATCH	75		/* Program version wrong */
#define EPROCUNAVAIL	76		/* Bad procedure for program */
#define EFTYPE		79		/* inappropriate operation for file type */
#define ENOMSG		80		/* No msg matches receive request */
#define EIDRM		81		/* Msg queue id has been removed */
#define	ENOSR		82		/* Out of STREAMS resources */
#define	ETIME		83		/* System call timed out */
#define	EBADMSG		84		/* Next message has wrong type */
#define EPROTO		85		/* STREAMS protocol error */
#define ENODATA		86		/* No message on stream head read q */
#define ENOSTR		87		/* fd not associated with a stream */
#define ECLONEME	88		/* Tells open to clone the device */
#define	EDIRTY		89		/* Mounting a dirty fs w/o force */
#define	EDUPPKG		90		/* duplicate package name on install */
#define	EVERSION	91		/* version number mismatch */
#define	ENOPKG		92		/* unresolved package name */
#define	ENOSYM		93		/* unresolved symbol name */
#define ECANCELED	94		/* operation canceled */
#define EFAIL		95		/* cannot start operation */
#define EINPROG		97		/* operation (now) in progress */
#define EMTIMERS	98		/* too many timers */
#define ENOTSUP		99		/* function not implemented */
#define EAIO		100		/* internal AIO operation complete */
#define	ESOFT		123
#define	EMEDIA		124
#define	ERELOCATED	125

#elif	defined(__convex__)		/* CONVEX */

extern	volatile int	errno;		/* UNIX style error */

#define	EPERM		1		/* Not owner */
#define	ENOENT		2		/* No such file or directory */
#define	ESRCH		3		/* No such process */
#define	EINTR		4		/* Interrupted system call */
#define	EIO		5		/* I/O error */
#define	ENXIO		6		/* No such device or address */
#define	E2BIG		7		/* Arg list too long */
#define	ENOEXEC		8		/* Exec format error */
#define	EBADF		9		/* Bad file number */
#define	ECHILD		10		/* No children */
#define	EAGAIN		11		/* No more processes */
#define	ENOMEM		12		/* Not enough core */
#define	EACCES		13		/* Permission denied */
#define	EFAULT		14		/* Bad address */
#define	ENOTBLK		15		/* Block device required */
#define	EBUSY		16		/* Mount device busy */
#define	EEXIST		17		/* File exists */
#define	EXDEV		18		/* Cross-device link */
#define	ENODEV		19		/* No such device */
#define	ENOTDIR		20		/* Not a directory*/
#define	EISDIR		21		/* Is a directory */
#define	EINVAL		22		/* Invalid argument */
#define	ENFILE		23		/* File table overflow */
#define	EMFILE		24		/* Too many open files */
#define	ENOTTY		25		/* Not a typewriter */
#define	ETXTBSY		26		/* Text file busy */
#define	EFBIG		27		/* File too large */
#define	ENOSPC		28		/* No space left on device */
#define	ESPIPE		29		/* Illegal seek */
#define	EROFS		30		/* Read-only file system */
#define	EMLINK		31		/* Too many links */
#define	EPIPE		32		/* Broken pipe */
#define	EDOM		33		/* Argument too large */
#define	ERANGE		34		/* Result too large */
#define	EWOULDBLOCK	35		/* Operation would block */
#define	EINPROGRESS	36		/* Operation now in progress */
#define	EALREADY	37		/* Operation already in progress */
#define	ENOTSOCK	38		/* Socket operation on non-socket */
#define	EDESTADDRREQ	39		/* Destination address required */
#define	EMSGSIZE	40		/* Message too long */
#define	EPROTOTYPE	41		/* Protocol wrong type for socket */
#define	ENOPROTOOPT	42		/* Protocol not available */
#define	EPROTONOSUPPORT	43		/* Protocol not supported */
#define	ESOCKTNOSUPPORT	44		/* Socket type not supported */
#define	EOPNOTSUPP	45		/* Operation not supported on socket */
#define	EPFNOSUPPORT	46		/* Protocol family not supported */
#define	EAFNOSUPPORT	47		/* Address family not supported by protocol family */
#define	EADDRINUSE	48		/* Address already in use */
#define	EADDRNOTAVAIL	49		/* Can't assign requested address */
#define	ENETDOWN	50		/* Network is down */
#define	ENETUNREACH	51		/* Network is unreachable */
#define	ENETRESET	52		/* Network dropped connection on reset */
#define	ECONNABORTED	53		/* Software caused connection abort */
#define	ECONNRESET	54		/* Connection reset by peer */
#define	ENOBUFS		55		/* No buffer space available */
#define	EISCONN		56		/* Socket is already connected */
#define	ENOTCONN	57		/* Socket is not connected */
#define	ESHUTDOWN	58		/* Can't send after socket shutdown */
#define	ETOOMANYREFS	59		/* Too many references: can't splice */
#define	ETIMEDOUT	60		/* Connection timed out */
#define	ECONNREFUSED	61		/* Connection refused */
#define	ELOOP		62		/* Too many levels of symbolic links */
#define	ENAMETOOLONG	63		/* File name too long */
#define	EHOSTDOWN	64		/* Host is down */
#define	EHOSTUNREACH	65		/* No route to host */
#define	ENOTEMPTY	66		/* Directory not empty */
#define	EPROCLIM	67		/* Too many processes */
#define	EUSERS		68		/* Too many users */
#define	EDQUOT		69		/* Disc quota exceeded */
#define	ESTALE		70		/* Stale NFS file handle */
#define	EREMOTE		71		/* Too many levels of remote in path */
#define	EDEADLK		72		/* Deadlock condition. */
#define	ENOLCK		73		/* No record locks available. */

#elif	defined(__cray__)		/* CRAY */

#include	"/usr/include/errno.h"	/* use standard */

#elif	defined(__hpux__)		/* HP 9000 */

extern	volatile int	errno;		/* UNIX style error */

#define	EPERM		1		/* Not super-user		*/
#define	ENOENT		2		/* No such file or directory	*/
#define	ESRCH		3		/* No such process		*/
#define	EINTR		4		/* interrupted system call	*/
#define	EIO		5		/* I/O error			*/
#define	ENXIO		6		/* No such device or address	*/
#define	E2BIG		7      		/* Arg list too long		*/
#define	ENOEXEC		8		/* Exec format error		*/
#define	EBADF		9		/* Bad file number		*/
#define	ECHILD		10		/* No children			*/
#define	EAGAIN		11		/* No more processes		*/
#define	ENOMEM		12		/* Not enough core		*/
#define	EACCES		13      	/* Permission denied		*/
#define	EFAULT		14		/* Bad address			*/
#define	EBUSY		16		/* Mount device busy		*/
#define	EEXIST		17		/* File exists			*/
#define	EXDEV		18		/* Cross-device link		*/
#define	ENODEV		19		/* No such device		*/
#define	ENOTDIR		20		/* Not a directory		*/
#define	EISDIR		21		/* Is a directory		*/
#define	EINVAL		22		/* Invalid argument		*/
#define	ENFILE		23		/* File table overflow		*/
#define	EMFILE		24		/* Too many open files		*/
#define	ENOTTY		25		/* Not a typewriter		*/
#define	EFBIG		27		/* File too large		*/
#define	ENOSPC		28		/* No space left on device	*/
#define	ESPIPE		29		/* Illegal seek			*/
#define	EROFS		30		/* Read only file system	*/
#define	EMLINK		31		/* Too many links		*/
#define	EPIPE		32		/* Broken pipe			*/
#define	EDOM		33		/* Math arg out of domain of func */
#define	ERANGE		34		/* Math result not representable */
#define	ECHRNG		37		/* Channel number out of range	*/
#define	EL2NSYNC	38		/* Level 2 not synchronized	*/
#define	EL3HLT		39		/* Level 3 halted		*/
#define	EL3RST		40		/* Level 3 reset		*/
#define	ELNRNG		41		/* Link number out of range	*/
#define	EUNATCH 	42		/* Protocol driver not attached	*/
#define	ENOCSI		43		/* No CSI structure available	*/
#define	EL2HLT		44		/* Level 2 halted		*/
#define	EDEADLK 	45		/* A deadlock would occur	*/
#define	ENOLCK  	46		/* System record lock table was full */

#if	defined(__hp9000s700__) | defined(__hp9000s800__)

#define	ENOSTR		50		/* Device not a stream		*/
#define	ENODATA		51		/* no data (for no delay io)	*/
#define	ETIME		52		/* timer expired		*/
#define	ENOSR		53		/* out of streams resources	*/
#define	ENONET		54		/* Machine is not on the network*/
#define	ENOPKG		55		/* Package not installed        */
#define	ENOLINK		57		/* the link has been severed */
#define	EADV		58		/* advertise error */
#define	ESRMNT		59		/* srmount error */
#define	ECOMM		60		/* Communication error on send	*/
#define	EPROTO		61		/* Protocol error		*/
#define	EMULTIHOP 	64		/* multihop attempted */
#define	EDOTDOT 	66		/* Cross mount point (not really error)*/
#define	EBADMSG 	67		/* trying to read unreadable message */

#endif

#define	ESTALE		70		/* Stale NFS file handle */
#define	EREMOTE		71		/* Too many levels of remote in path */


#if	defined(__hp9000s300__) | defined (__hp9000s700__) | defined(__hp9000s800__)

#define	ENOTSOCK	216		/* Socket operation on non-socket */
#define	EDESTADDRREQ	217		/* Destination address required */
#define	EMSGSIZE	218		/* Message too long */
#define	EPROTOTYPE	219		/* Protocol wrong type for socket */
#define	ENOPROTOOPT	220		/* Protocol not available */
#define	EPROTONOSUPPORT	221		/* Protocol not supported */
#define	ESOCKTNOSUPPORT	222		/* Socket type not supported */
#define	EOPNOTSUPP	223		/* Operation not supported */
#define	EPFNOSUPPORT 	224		/* Protocol family not supported */
#define	EAFNOSUPPORT 	225 		/* Address family not supported by protocol family */
#define	EADDRINUSE	226		/* Address already in use */
#define	EADDRNOTAVAIL 	227		/* Can't assign requested address */
#define	ENETDOWN	228		/* Network is down */
#define	ENETUNREACH	229		/* Network is unreachable */
#define	ENETRESET	230		/* Network dropped connection on reset */
#define	ECONNABORTED	231		/* Software caused connection abort */
#define	ECONNRESET	232		/* Connection reset by peer */
#define	ENOBUFS		233		/* No buffer space available */
#define	EISCONN		234		/* Socket is already connected */
#define	ENOTCONN	235		/* Socket is not connected */
#define	ESHUTDOWN	236		/* Can't send after socket shutdown */
#define	ETOOMANYREFS	237		/* Too many references: can't splice */
#define	ETIMEDOUT	238		/* Connection timed out */
#define	ECONNREFUSED	239		/* Connection refused */

#if	defined(__hp9000s700__) | defined(__hp9000s800__)

#define	EREFUSED	ECONNREFUSED	/* Double define for NFS*/

#endif

#define	EREMOTERELEASE	240		/* Remote peer released connection */
#define	EHOSTDOWN	241		/* Host is down */
#define	EHOSTUNREACH	242		/* No route to host */

#endif

#define	ENET  	    	243		/* Network error */
#define	EALREADY    	244		/* Operation already in progress */
#define	EINPROGRESS 	245		/* Operation now in progress */
#define	EWOULDBLOCK 	246		/* Operation would block */
#define	ELOOP	    	249		/* Too many levels of symbolic links */
#define	ENOTEMPTY   	247		/* Directory not empty          */
#define	ENAMETOOLONG 	248		/* File name too long           */
#define	ENOSYS 	  	251     	/* Function not implemented     */

#elif	defined(__linux__)		/* LINUX */

extern int	errno;

#define	EPERM		 1	/* Operation not permitted */
#define	ENOENT		 2	/* No such file or directory */
#define	ESRCH		 3	/* No such process */
#define	EINTR		 4	/* Interrupted system call */
#define	EIO		 5	/* I/O error */
#define	ENXIO		 6	/* No such device or address */
#define	E2BIG		 7	/* Arg list too long */
#define	ENOEXEC		 8	/* Exec format error */
#define	EBADF		 9	/* Bad file number */
#define	ECHILD		10	/* No child processes */
#define	EAGAIN		11	/* Try again */
#define	ENOMEM		12	/* Out of memory */
#define	EACCES		13	/* Permission denied */
#define	EFAULT		14	/* Bad address */
#define	ENOTBLK		15	/* Block device required */
#define	EBUSY		16	/* Device or resource busy */
#define	EEXIST		17	/* File exists */
#define	EXDEV		18	/* Cross-device link */
#define	ENODEV		19	/* No such device */
#define	ENOTDIR		20	/* Not a directory */
#define	EISDIR		21	/* Is a directory */
#define	EINVAL		22	/* Invalid argument */
#define	ENFILE		23	/* File table overflow */
#define	EMFILE		24	/* Too many open files */
#define	ENOTTY		25	/* Not a typewriter */
#define	ETXTBSY		26	/* Text file busy */
#define	EFBIG		27	/* File too large */
#define	ENOSPC		28	/* No space left on device */
#define	ESPIPE		29	/* Illegal seek */
#define	EROFS		30	/* Read-only file system */
#define	EMLINK		31	/* Too many links */
#define	EPIPE		32	/* Broken pipe */
#define	EDOM		33	/* Math argument out of domain of func */
#define	ERANGE		34	/* Math result not representable */
#define	EDEADLK		35	/* Resource deadlock would occur */
#define	ENAMETOOLONG	36	/* File name too long */
#define	ENOLCK		37	/* No record locks available */
#define	ENOSYS		38	/* Function not implemented */
#define	ENOTEMPTY	39	/* Directory not empty */
#define	ELOOP		40	/* Too many symbolic links encountered */
#define	EWOULDBLOCK	EAGAIN	/* Operation would block */
#define	ENOMSG		42	/* No message of desired type */
#define	EIDRM		43	/* Identifier removed */
#define	ECHRNG		44	/* Channel number out of range */
#define	EL2NSYNC	45	/* Level 2 not synchronized */
#define	EL3HLT		46	/* Level 3 halted */
#define	EL3RST		47	/* Level 3 reset */
#define	ELNRNG		48	/* Link number out of range */
#define	EUNATCH		49	/* Protocol driver not attached */
#define	ENOCSI		50	/* No CSI structure available */
#define	EL2HLT		51	/* Level 2 halted */
#define	EBADE		52	/* Invalid exchange */
#define	EBADR		53	/* Invalid request descriptor */
#define	EXFULL		54	/* Exchange full */
#define	ENOANO		55	/* No anode */
#define	EBADRQC		56	/* Invalid request code */
#define	EBADSLT		57	/* Invalid slot */
#define	EDEADLOCK	58	/* File locking deadlock error */
#define	EBFONT		59	/* Bad font file format */
#define	ENOSTR		60	/* Device not a stream */
#define	ENODATA		61	/* No data available */
#define	ETIME		62	/* Timer expired */
#define	ENOSR		63	/* Out of streams resources */
#define	ENONET		64	/* Machine is not on the network */
#define	ENOPKG		65	/* Package not installed */
#define	EREMOTE		66	/* Object is remote */
#define	ENOLINK		67	/* Link has been severed */
#define	EADV		68	/* Advertise error */
#define	ESRMNT		69	/* Srmount error */
#define	ECOMM		70	/* Communication error on send */
#define	EPROTO		71	/* Protocol error */
#define	EMULTIHOP	72	/* Multihop attempted */
#define	EDOTDOT		73	/* RFS specific error */
#define	EBADMSG		74	/* Not a data message */
#define	EOVERFLOW	75	/* Value too large for defined data type */
#define	ENOTUNIQ	76	/* Name not unique on network */
#define	EBADFD		77	/* File descriptor in bad state */
#define	EREMCHG		78	/* Remote address changed */
#define	ELIBACC		79	/* Can not access a needed shared library */
#define	ELIBBAD		80	/* Accessing a corrupted shared library */
#define	ELIBSCN		81	/* .lib section in a.out corrupted */
#define	ELIBMAX		82	/* Attempting to link in too many shared libraries */
#define	ELIBEXEC	83	/* Cannot exec a shared library directly */
#define	EILSEQ		84	/* Illegal byte sequence */
#define	ERESTART	85	/* Interrupted system call should be restarted */
#define	ESTRPIPE	86	/* Streams pipe error */
#define	EUSERS		87	/* Too many users */
#define	ENOTSOCK	88	/* Socket operation on non-socket */
#define	EDESTADDRREQ	89	/* Destination address required */
#define	EMSGSIZE	90	/* Message too long */
#define	EPROTOTYPE	91	/* Protocol wrong type for socket */
#define	ENOPROTOOPT	92	/* Protocol not available */
#define	EPROTONOSUPPORT	93	/* Protocol not supported */
#define	ESOCKTNOSUPPORT	94	/* Socket type not supported */
#define	EOPNOTSUPP	95	/* Operation not supported on transport endpoint */
#define	EPFNOSUPPORT	96	/* Protocol family not supported */
#define	EAFNOSUPPORT	97	/* Address family not supported by protocol */
#define	EADDRINUSE	98	/* Address already in use */
#define	EADDRNOTAVAIL	99	/* Cannot assign requested address */
#define	ENETDOWN	100	/* Network is down */
#define	ENETUNREACH	101	/* Network is unreachable */
#define	ENETRESET	102	/* Network dropped connection because of reset */
#define	ECONNABORTED	103	/* Software caused connection abort */
#define	ECONNRESET	104	/* Connection reset by peer */
#define	ENOBUFS		105	/* No buffer space available */
#define	EISCONN		106	/* Transport endpoint is already connected */
#define	ENOTCONN	107	/* Transport endpoint is not connected */
#define	ESHUTDOWN	108	/* Cannot send after transport endpoint shutdown */
#define	ETOOMANYREFS	109	/* Too many references: cannot splice */
#define	ETIMEDOUT	110	/* Connection timed out */
#define	ECONNREFUSED	111	/* Connection refused */
#define	EHOSTDOWN	112	/* Host is down */
#define	EHOSTUNREACH	113	/* No route to host */
#define	EALREADY	114	/* Operation already in progress */
#define	EINPROGRESS	115	/* Operation now in progress */
#define	ESTALE		116	/* Stale NFS file handle */
#define	EUCLEAN		117	/* Structure needs cleaning */
#define	ENOTNAM		118	/* Not a XENIX named type file */
#define	ENAVAIL		119	/* No XENIX semaphores available */
#define	EISNAM		120	/* Is a named type file */
#define	EREMOTEIO	121	/* Remote I/O error */
#define	EDQUOT		122	/* Quota exceeded */
#define ERESTARTSYS	512
#define ERESTARTNOINTR	513
#define ERESTARTNOHAND	514	/* restart if no handler.. */

#elif	defined(__mips__)		/* DEC ULTRIX */

extern	volatile int	errno;		/* UNIX style error */

#define	EPERM		1		/* Not owner */
#define	ENOENT		2		/* No such file or directory */
#define	ESRCH		3		/* No such process */
#define	EINTR		4		/* Interrupted system call */
#define	EIO		5		/* I/O error */
#define	ENXIO		6		/* No such device or address */
#define	E2BIG		7		/* Arg list too long */
#define	ENOEXEC		8		/* Exec format error */
#define	EBADF		9		/* Bad file number */
#define	ECHILD		10		/* No children */
#define	EAGAIN		11		/* No more processes */
#define	ENOMEM		12		/* Not enough core */
#define	EACCES		13		/* Permission denied */
#define	EFAULT		14		/* Bad address */
#define	ENOTBLK		15		/* Block device required */
#define	EBUSY		16		/* Mount device busy */
#define	EEXIST		17		/* File exists */
#define	EXDEV		18		/* Cross-device link */
#define	ENODEV		19		/* No such device */
#define	ENOTDIR		20		/* Not a directory*/
#define	EISDIR		21		/* Is a directory */
#define	EINVAL		22		/* Invalid argument */
#define	ENFILE		23		/* File table overflow */
#define	EMFILE		24		/* Too many open files */
#define	ENOTTY		25		/* Not a typewriter */
#define	ETXTBSY		26		/* Text file busy */
#define	EFBIG		27		/* File too large */
#define	ENOSPC		28		/* No space left on device */
#define	ESPIPE		29		/* Illegal seek */
#define	EROFS		30		/* Read-only file system */
#define	EMLINK		31		/* Too many links */
#define	EPIPE		32		/* Broken pipe */
#define	EDOM		33		/* Argument too large */
#define	ERANGE		34		/* Result too large */
#define	EWOULDBLOCK	35		/* Operation would block */
#define	EINPROGRESS	36		/* Operation now in progress */
#define	EALREADY	37		/* Operation already in progress */
#define	ENOTSOCK	38		/* Socket operation on non-socket */
#define	EDESTADDRREQ	39		/* Destination address required */
#define	EMSGSIZE	40		/* Message too long */
#define	EPROTOTYPE	41		/* Protocol wrong type for socket */
#define	ENOPROTOOPT	42		/* Protocol not available */
#define	EPROTONOSUPPORT	43		/* Protocol not supported */
#define	ESOCKTNOSUPPORT	44		/* Socket type not supported */
#define	EOPNOTSUPP	45		/* Operation not supported on socket */
#define	EPFNOSUPPORT	46		/* Protocol family not supported */
#define	EAFNOSUPPORT	47		/* Address family not supported by protocol family */
#define	EADDRINUSE	48		/* Address already in use */
#define	EADDRNOTAVAIL	49		/* Can't assign requested address */
#define	ENETDOWN	50		/* Network is down */
#define	ENETUNREACH	51		/* Network is unreachable */
#define	ENETRESET	52		/* Network dropped connection on reset */
#define	ECONNABORTED	53		/* Software caused connection abort */
#define	ECONNRESET	54		/* Connection reset by peer */
#define	ENOBUFS		55		/* No buffer space available */
#define	EISCONN		56		/* Socket is already connected */
#define	ENOTCONN	57		/* Socket is not connected */
#define	ESHUTDOWN	58		/* Can't send after socket shutdown */
#define	ETOOMANYREFS	59		/* Too many references: can't splice */
#define	ETIMEDOUT	60		/* Connection timed out */
#define	ECONNREFUSED	61		/* Connection refused */
#define	ELOOP		62		/* Too many levels of symbolic links */
#define	ENAMETOOLONG	63		/* File name too long */
#define	EHOSTDOWN	64		/* Host is down */
#define	EHOSTUNREACH	65		/* No route to host */
#define	ENOTEMPTY	66		/* Directory not empty */
#define	EPROCLIM	67		/* Too many processes */
#define	EUSERS		68		/* Too many users */
#define	EDQUOT		69		/* Disc quota exceeded */
#define	ESTALE		70
#define	EREMOTE		71
#define	ENOMSG		72		/* No message of desired type */
#define	EIDRM		73		/* Identifier removed */
#define	EALIGN		74		/* alignment error */
#define	EDEADLK		EWOULDBLOCK	/* resource deadlock would occur */
#define	ENOLCK		75		/* LOCK_MAX exceeded	*/

#elif	defined(__sgi__)		/* SILICON GRAPHICS */

#ifndef __ERRNO_H__
#define __ERRNO_H__
#endif

extern	int	errno;

#include	<sys/errno.h>

#elif	defined(__sun__)		/* SUN */

extern	volatile int	errno;		/* UNIX style error */

#define	EPERM		1		/* Not owner */
#define	ENOENT		2		/* No such file or directory */
#define	ESRCH		3		/* No such process */
#define	EINTR		4		/* Interrupted system call */
#define	EIO		5		/* I/O error */
#define	ENXIO		6		/* No such device or address */
#define	E2BIG		7		/* Arg list too long */
#define	ENOEXEC		8		/* Exec format error */
#define	EBADF		9		/* Bad file number */
#define	ECHILD		10		/* No children */
#define	EAGAIN		11		/* No more processes */
#define	ENOMEM		12		/* Not enough core */
#define	EACCES		13		/* Permission denied */
#define	EFAULT		14		/* Bad address */
#define	ENOTBLK		15		/* Block device required */
#define	EBUSY		16		/* Mount device busy */
#define	EEXIST		17		/* File exists */
#define	EXDEV		18		/* Cross-device link */
#define	ENODEV		19		/* No such device */
#define	ENOTDIR		20		/* Not a directory*/
#define	EISDIR		21		/* Is a directory */
#define	EINVAL		22		/* Invalid argument */
#define	ENFILE		23		/* File table overflow */
#define	EMFILE		24		/* Too many open files */
#define	ENOTTY		25		/* Not a typewriter */
#define	ETXTBSY		26		/* Text file busy */
#define	EFBIG		27		/* File too large */
#define	ENOSPC		28		/* No space left on device */
#define	ESPIPE		29		/* Illegal seek */
#define	EROFS		30		/* Read-only file system */
#define	EMLINK		31		/* Too many links */
#define	EPIPE		32		/* Broken pipe */
#define	EDOM		33		/* Argument too large */
#define	ERANGE		34		/* Result too large */

#ifdef	__bsd__				/* SunOS < 5.0 */

#define	EWOULDBLOCK	35		/* Operation would block */
#define	EINPROGRESS	36		/* Operation now in progress */
#define	EALREADY	37		/* Operation already in progress */
#define	ENOTSOCK	38		/* Socket operation on non-socket */
#define	EDESTADDRREQ	39		/* Destination address required */
#define	EMSGSIZE	40		/* Message too long */
#define	EPROTOTYPE	41		/* Protocol wrong type for socket */
#define	ENOPROTOOPT	42		/* Protocol not available */
#define	EPROTONOSUPPORT	43		/* Protocol not supported */
#define	ESOCKTNOSUPPORT	44		/* Socket type not supported */
#define	EOPNOTSUPP	45		/* Operation not supported on socket */
#define	EPFNOSUPPORT	46		/* Protocol family not supported */
#define	EAFNOSUPPORT	47		/* Address family not supported by protocol family */
#define	EADDRINUSE	48		/* Address already in use */
#define	EADDRNOTAVAIL	49		/* Can't assign requested address */
#define	ENETDOWN	50		/* Network is down */
#define	ENETUNREACH	51		/* Network is unreachable */
#define	ENETRESET	52		/* Network dropped connection on reset */
#define	ECONNABORTED	53		/* Software caused connection abort */
#define	ECONNRESET	54		/* Connection reset by peer */
#define	ENOBUFS		55		/* No buffer space available */
#define	EISCONN		56		/* Socket is already connected */
#define	ENOTCONN	57		/* Socket is not connected */
#define	ESHUTDOWN	58		/* Can't send after socket shutdown */
#define	ETOOMANYREFS	59		/* Too many references: can't splice */
#define	ETIMEDOUT	60		/* Connection timed out */
#define	ECONNREFUSED	61		/* Connection refused */
#define	ELOOP		62		/* Too many levels of symbolic links */
#define	ENAMETOOLONG	63		/* File name too long */
#define	EHOSTDOWN	64		/* Host is down */
#define	EHOSTUNREACH	65		/* No route to host */
#define	ENOTEMPTY	66		/* Directory not empty */
#define	EPROCLIM	67		/* Too many processes */
#define	EUSERS		68		/* Too many users */
#define	EDQUOT		69		/* Disc quota exceeded */
#define	ESTALE		70		/* Stale NFS file handle */
#define	EREMOTE		71		/* Too many levels of remote in path */
#define	ENOSTR		72		/* Device is not a stream */
#define	ETIME		73		/* Timer expired */
#define	ENOSR		74		/* Out of streams resources */
#define	ENOMSG		75		/* No message of desired type */
#define	EBADMSG		76		/* Trying to read unreadable message */
#define	EIDRM		77		/* Identifier removed */
#define	EDEADLK		78		/* Deadlock condition. */
#define	ENOLCK		79		/* No record locks available. */
#define	ENONET		80		/* Machine is not on the network */
#define	ERREMOTE	81		/* Object is remote */
#define	ENOLINK		82		/* the link has been severed */
#define	EADV		83		/* advertise error */
#define	ESRMNT		84		/* srmount error */
#define	ECOMM		85		/* Communication error on send */
#define	EPROTO		86		/* Protocol error */
#define	EMULTIHOP	87		/* multihop attempted */
#define	EDOTDOT		88		/* Cross mount point (not an error) */
#define	EREMCHG		89		/* Remote address changed */

#else					/* SunOS >= 5.0 */

#define	ENOMSG		35		/* No message of desired type */
#define	EIDRM		36		/* Identifier removed */
#define	ECHRNG		37		/* Channel number out of range */
#define	EL2NSYNC	38		/* Level 2 not synchronized */
#define	EL3HLT		39		/* Level 3 halted */
#define	EL3RST		40		/* Level 3 reset */
#define	ELNRNG		41		/* Link number out of range */
#define	EUNATCH 	42		/* Protocol driver not attached */
#define	ENOCSI		43		/* No CSI structure available */
#define	EL2HLT		44		/* Level 2 halted */
#define	EDEADLK		45		/* Deadlock condition. */
#define	ENOLCK		46		/* No record locks available. */
#define	EBADE		50		/* invalid exchange */
#define	EBADR		51		/* invalid request descriptor */
#define	EXFULL		52		/* exchange full */
#define	ENOANO		53		/* no anode */
#define	EBADRQC		54		/* invalid request code */
#define	EBADSLT		55		/* invalid slot */
#define	EDEADLOCK	56		/* file locking deadlock error */
#define	EBFONT		57		/* bad font file fmt */
#define	ENOSTR		60		/* Device not a stream */
#define	ENODATA		61		/* no data (for no delay io) */
#define	ETIME		62		/* timer expired */
#define	ENOSR		63		/* out of streams resources */
#define	ENONET		64		/* Machine is not on the network */
#define	ENOPKG		65		/* Package not installed */
#define	EREMOTE		66		/* The object is remote */
#define	ENOLINK		67		/* the link has been severed */
#define	EADV		68		/* advertise error */
#define	ESRMNT		69		/* srmount error */
#define	ECOMM		70		/* Communication error on send */
#define	EPROTO		71		/* Protocol error */
#define	EMULTIHOP	74		/* multihop attempted */
#define	EBADMSG 	77		/* trying to read unreadable message */
#define	ENAMETOOLONG	78		/* path name is too long */
#define	EOVERFLOW	79		/* value too large to be stored in data type */
#define	ENOTUNIQ	80		/* given log. name not unique */
#define	EBADFD		81		/* f.d. invalid for this operation */
#define	EREMCHG		82		/* Remote address changed */
#define	ELIBACC		83		/* Can't access a needed shared lib. */
#define	ELIBBAD		84		/* Accessing a corrupted shared lib. */
#define	ELIBSCN		85		/* .lib section in a.out corrupted. */
#define	ELIBMAX		86		/* Attempting to link in too many libs.	*/
#define	ELIBEXEC	87		/* Attempting to exec a shared library.	*/
#define	EILSEQ		88		/* Illegal byte sequence. */
#define	ENOSYS		89		/* Unsupported file system operation */
#define	ELOOP		90		/* Symbolic link loop */
#define	ERESTART	91		/* Restartable system call */
#define	ESTRPIPE	92		/* if pipe/FIFO, don't sleep in stream head */
#define	ENOTEMPTY	93		/* directory not empty */
#define	EUSERS		94		/* Too many users (for UFS) */
#define	ENOTSOCK	95		/* Socket operation on non-socket */
#define	EDESTADDRREQ	96		/* Destination address required */
#define	EMSGSIZE	97		/* Message too long */
#define	EPROTOTYPE	98		/* Protocol wrong type for socket */
#define	ENOPROTOOPT	99		/* Protocol not available */
#define	EPROTONOSUPPORT	120		/* Protocol not supported */
#define	ESOCKTNOSUPPORT	121		/* Socket type not supported */
#define	EOPNOTSUPP	122		/* Operation not supported on socket */
#define	EPFNOSUPPORT	123		/* Protocol family not supported */
#define	EAFNOSUPPORT	124		/* Address family not supported by */
					/* protocol family */
#define	EADDRINUSE	125		/* Address already in use */
#define	EADDRNOTAVAIL	126		/* Can't assign requested address */
#define	ENETDOWN	127		/* Network is down */
#define	ENETUNREACH	128		/* Network is unreachable */
#define	ENETRESET	129		/* Network dropped connection because */
					/* of reset */
#define	ECONNABORTED	130		/* Software caused connection abort */
#define	ECONNRESET	131		/* Connection reset by peer */
#define	ENOBUFS		132		/* No buffer space available */
#define	EISCONN		133		/* Socket is already connected */
#define	ENOTCONN	134		/* Socket is not connected */
#define	ESHUTDOWN	143		/* Can't send after socket shutdown */
#define	ETOOMANYREFS	144		/* Too many references: can't splice */
#define	ETIMEDOUT	145		/* Connection timed out */
#define	ECONNREFUSED	146		/* Connection refused */
#define	EHOSTDOWN	147		/* Host is down */
#define	EHOSTUNREACH	148		/* No route to host */
#define	EWOULDBLOCK	EAGAIN	
#define	EALREADY	149		/* operation already in progress */
#define	EINPROGRESS	150		/* operation now in progress */
#define	ESTALE		151		/* Stale NFS file handle */

#ifdef XENIX_MERGE

#define	EUCLEAN 	135		/* Structure needs cleaning */
#define	ENOTNAM		137		/* Not a XENIX named type file */
#define	ENAVAIL		138		/* No XENIX semaphores available */
#define	EISNAM		139		/* Is a named type file */
#define	EREMOTEIO	140		/* Remote I/O error */
#define	EINIT		141		/* Reserved for future */
#define	EREMDEV		142		/* Error 142 */

#endif

#endif

#elif	defined(__vms__)		/* DEC VMS */

extern	volatile int noshare	errno;	/* UNIX style error */

#define	EPERM		1		/* Not owner */
#define	ENOENT		2		/* No such file or directory */
#define	ESRCH		3		/* No such process */
#define	EINTR		4		/* Interrupted system call */
#define	EIO		5		/* I/O error */
#define	ENXIO		6		/* No such device or address */
#define	E2BIG		7		/* Arg list too long */
#define	ENOEXEC 	8		/* Exec format error */
#define	EBADF		9		/* Bad file number */
#define	ECHILD		10		/* No children */
#define	EAGAIN		11		/* No more processes */
#define	ENOMEM		12		/* Not enough core */
#define	EACCES		13		/* Permission denied */
#define	EFAULT		14		/* Bad address */
#define	ENOTBLK 	15		/* Block device required */
#define	EBUSY		16		/* Mount device busy */
#define	EEXIST		17		/* File exists */
#define	EXDEV		18		/* Cross-device link */
#define	ENODEV		19		/* No such device */
#define	ENOTDIR 	20		/* Not a directory*/
#define	EISDIR		21		/* Is a directory */
#define	EINVAL		22		/* Invalid argument */
#define	ENFILE		23		/* File table overflow */
#define	EMFILE		24		/* Too many open files */
#define	ENOTTY		25		/* Not a typewriter */
#define	ETXTBSY 	26		/* Text file busy */
#define	EFBIG		27		/* File too large */
#define	ENOSPC		28		/* No space left on device */
#define	ESPIPE		29		/* Illegal seek */
#define	EROFS		30		/* Read-only file system */
#define	EMLINK		31		/* Too many links */
#define	EPIPE		32		/* Broken pipe */
#define	EDOM		33		/* Argument too large */
#define	ERANGE		34		/* Result too large */
#define	EWOULDBLOCK	35		/* Operation would block */

#endif

#endif
