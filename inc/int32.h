#ifndef	_INT32_H
#define	_INT32_H
extern	void	int_to_int32( int *i, unsigned char *i32, int swap );
extern	void	int32_to_int( unsigned char *i32, int *i, int swap );
extern	void	int_to_int32_n( int *i, unsigned char *i32, int n, int swap );
extern	void	int32_to_int_n( unsigned char *i32, int *i, int n, int swap );
#endif
