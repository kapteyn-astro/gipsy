#ifndef	_INT16_H
#define	_INT16_H
extern	void	int_to_int16( short *s, unsigned char *i16, int swap );
extern	void	int16_to_int( unsigned char *i16, short *s, int swap );
extern	void	int_to_int16_n( short *s, unsigned char *i16, int n, int swap );
extern	void	int16_to_int_n( unsigned char *i16, short *s, int n, int swap );
#endif
