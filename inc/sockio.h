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
