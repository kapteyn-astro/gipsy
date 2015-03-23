/* sockio.c

	Copyright (c) Kapteyn Laboratorium Groningen 1994
	All Rights Reserved.	

#>            sockio.dc3

Document:     sockio

Purpose:      Describes the available routines to read/write from/to a
              socket.

Category:     SYSTEM

File:         sockio.c

Author:       K.G. Begeman

Description:  Reading/writing from/to a socket is handled by the routines
              described in this document. The routines are tuned for
              reading/writing from/to unix sockets and internet sockets,
              but can also be used for general I/O. The routines are
              made so that when a specified amount of data should be
              read/written from/to a file descriptor, it returns when
              the I/O is completed or in case of an error.

              The following routines are available:
              sock_read                read from a socket
              sock_readv               read a vector from a socket
              sock_write               write to a socket
              sock_writev              write a vector to a socket
              
Updates:      Oct 29, 94: KGB, Document created.

#<
#>            sock_read.dc3

Function:     sock_read

Purpose:      Reads data from an open file descriptor, usually a socket.

Category:     SYSTEM

File:         sockio.c

Author:       K.G. Begeman

Call:         int sock_read( int fd, void *data, int ndata )

              sock_read         returns 0, -1 on error
              fd                open file descriptor
              data              receives the data
              ndata             number of bytes to read from fd

Updates:      Oct 29, 94: KGB, Document created.

#<
#>            sock_write.dc3

Function:     sock_write

Purpose:      Writes data to an open file descriptor, usually a socket.

Category:     SYSTEM

File:         sockio.c

Author:       K.G. Begeman

Call:         int sock_write( int fd, void *data, int ndata )

              sock_read         returns 0 on success, -1 on error
              fd                open file descriptor
              data              sends the data
              ndata             number of bytes to write to fd

Updates:      Oct 29, 94: KGB, Document created.

#<
#>            sock_readv.dc3

Function:     sock_readv

Purpose:      Reads a vector from an open file descriptor, usually a socket.

Category:     SYSTEM

File:         sockio.c

Author:       K.G. Begeman

Call:         int sock_readv( int fd, struct sock_iovec *vector, int count )

              struct sock_iovec {
                 void *iov_base;
                 int  iov_len;
              }
 
              sock_readv        returns 0 on success, -1 on error
              fd                open file descriptor
              vector            points to multiple data buffers into
                                which the data is read
              count             number of elements in vector

Updates:      Oct 29, 94: KGB, Document created.

#<
#>            sock_writev.dc3

Function:     sock_writev

Purpose:      Writes a vector to an open file descriptor, usually a socket.

Category:     SYSTEM

File:         sockio.c

Author:       K.G. Begeman

Call:         int sock_writev( int fd, struct sock_iovec *vector, int count )

              struct sock_iovec {
                 void *iov_base;
                 int  iov_len;
              }
 
              sock_writev       returns 0 on success, -1 on error
              fd                open file descriptor
              vector            points to multiple data buffers from
                                which the data is written
              count             number of elements in vector

Updates:      Oct 29, 94: KGB, Document created.

#<
#>            sockio.h
#ifndef	_SOCKIO_H
#define	_SOCKIO_H
struct sock_iovec {
   void	*iov_base;
   int	iov_len;
};
extern	int	sock_read( int fd, void *data, int ndata );
extern	int	sock_readv( int fd, struct sock_iovec *vector, int count );
extern	int	sock_write( int fd, void *data, int ndata );
extern	int	sock_writev( int fd, struct sock_iovec *vector, int count );
#endif
#<
*/

#include	"errno.h"			/* <errno.h> */
#include	"stddef.h"			/* <stddef.h> */
#include	"string.h"			/* <string.h> */

#include	"sockio.h"			/* our own definitions */
#include	<sys/types.h>
#ifdef	__aix__
#include	<sys/select.h>
#endif
#ifdef	__linux__
#include	<sys/time.h>
#endif

extern	int	read( );
extern	int	write( );
extern	int	select( );

#define	SIO_SUCCESS	0
#define	SIO_ERROR	-1

#ifdef	EWOULDBLOCK

static	int	read_wait( int socket )
{
   fd_set	read_fds;
   int		nfound;
   int		r = SIO_SUCCESS;

   do {
      FD_ZERO( &read_fds );
      FD_SET( socket, &read_fds );
      nfound = select( socket + 1, (void *) &read_fds, NULL, NULL, NULL );
      if (nfound == -1 && errno != EINTR) {
         r = SIO_ERROR;
      }
   } while (nfound <= 0 && r == SIO_SUCCESS);
   return( r );
}

static	int	write_wait( int socket )
{
   fd_set	write_fds;
   int		nfound;
   int		r = SIO_SUCCESS;

   do {
      FD_ZERO( &write_fds );
      FD_SET( socket, &write_fds );
      nfound = select( socket + 1, NULL, (void *) &write_fds, NULL, NULL );
      if (nfound == -1 && errno != EINTR) {
         r = SIO_ERROR;
      }
   } while (nfound <= 0 && r == SIO_SUCCESS);
   return( r );
}

#endif

int	sock_write( int socket, void *data, int ndata )
{
   char	*p = (char *) data;			/* make character pointer */
   int	d = 0;					/* current byte count */
   int	l = ndata;				/* number of bytes left  */
   int	r = SIO_SUCCESS;			/* return value */
   int	t = 0;					/* total number of bytes */

   while (l && r == SIO_SUCCESS) {
      while ((d = write( socket, &p[t], l )) == -1 && errno == EINTR);
      if (d > 0) {
         l -= d;
         t += d;
#ifdef	EWOULDBLOCK
      } else if (d == -1 && errno == EWOULDBLOCK) {
         r = write_wait( socket );
#endif
      } else {
         r = SIO_ERROR;
      }
   }
   return( r );					/* return to caller */
}

int	sock_read( int socket, void *data, int ndata )
{
   char	*p = (char *) data;			/* make character pointer */
   int	d = 0;					/* current byte count */
   int	l = ndata;				/* number of bytes left  */
   int	r = SIO_SUCCESS;			/* return value */
   int	t = 0;					/* total number */

   while (l && r == SIO_SUCCESS) {
      while ((d = read( socket, &p[t], l )) == -1 && errno == EINTR);
      if (d > 0) {
         l -= d;
         t += d;
#ifdef	EWOULDBLOCK
      } else if (d == -1 && errno == EWOULDBLOCK) {
         r = read_wait( socket );
#endif
      } else {
         r = SIO_ERROR;
      }
   }
   return( r );					/* return to caller */
}

int	sock_writev( int socket, struct sock_iovec *vp, int vpcount )
{
   int	count;
   int	r = SIO_SUCCESS;

   for ( count = 0; r == SIO_SUCCESS && --vpcount >= 0; count += vp->iov_len, vp++ ) {
      r = sock_write( socket, vp->iov_base, vp->iov_len );
   }
   return( r );
}

int	sock_readv( int socket, struct sock_iovec *vp, int vpcount )
{
   int	count;
   int	r = SIO_SUCCESS;

   for ( count = 0; r == SIO_SUCCESS && --vpcount >= 0; count += vp->iov_len, vp++ ) {
      r = sock_read( socket, vp->iov_base, vp->iov_len );
   }
   return( r );
}
