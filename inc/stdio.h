/* stdio.h

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            stdio.dc3

Header:       stdio.h

Purpose:      Defines ANSI C input and output utilities.

File:         stdio.h

Author:       K.G. Begeman

Use:          #include "stdio.h"

Defines:      BUFSIZ        I/O buffer size
              clearerr(FILE *stream) Clears EOF and error indicators
              EOF           End Of File indication code
              feof(FILE *stream) EOF indicator for stream
              ferror(FILE *stream) Error indicator for stream
              FILE          File pointer structure
              FILENAME_MAX  Maximum length of file name
              fpos_t        Used by fgetpos and fsetpos
              getc(FILE *stream) As fgetc, but a macro
              getchar(void) getc(stdin)
              L_tmpnam      Min. length of temp. file name
              FOPEN_MAX     Maximum number of files open per process
              putc(int c, FILE *stream) As fputc, but macro
              putchar(int c) putc(c,stdout)
              SEEK_SET      Position from begin of file
              SEEK_CUR      Position from current file position
              SEEK_END      Position from end of file
              stdin         Standard input
              stdout        Standard output
              stderr        Standard error
              TMP_MAX       Max. number of unique temp. file names
              _IOFBF        Sets full buffering
              _IOLBF        Sets line buffering
              _IONBF        Sets no buffering

Declares:     char *fgets(char *s, int n, FILE *stream);
              char *gets(char *s);
              char *tmpnam(char s[L_tmpnam]);
              FILE *fopen(const char *filename, const char *mode);
              FILE *freopen(const char *filename, const char *mode,
                            FILE *stream);
              FILE *tmpfile(void);
              int fclose(FILE *stream);
              int fflush(FILE *stream);
              int fgetc(FILE *stream);
              int fgetpos(FILE *stream, fpos_t *ptr);
              int fprintf(FILE *stream, const char *format, ...);
              int fputc(int c, FILE *stream);
              int fputs(const char *s, FILE *stream);
              int fscanf(FILE *stream, const char *format, ...);
              int fseek(FILE *stream, long offset, int origin);
              int fsetpos(FILE *stream, const fpos_t *ptr);
              int printf(const char *format, ...);
              int puts(const char *s);
              int remove(const char *filename);
              int rename(const char *oldname, const char *newname);
              int scanf(const char *format, ...);
              int setvbuf(FILE *stream, char *buf, int mode, size_t size);
              int sprintf(char *s, const char *format, ...);
              int sscanf(char *s, const char *format, ...);
              int ungetc(int c, FILE *stream);
              int vfprintf(FILE *stream, const char *format, va_list arg);
              int vprintf(const char *format, va_list arg );
              int vsprintf(char *s, const char*format, va_list arg);
              long ftell(FILE *stream);
              size_t fread(void *ptr, size_t size, size_t nobj, FILE *stream);
              size_t fwrite(void *ptr, size_t size, size_t nobj, FILE *stream);
              void perror(const char *s);
              void rewind(FILE *stream);
              void setbuf(FILE *stream, char *buf);

Warning:      System dependent! At the moment implemented for ALLIANT,
              CONVEX, DEC ALPHA, DEC ULTRIX, HP9000, SUN and DEC VMS.

Updates:      Apr  8, 1990: KGB, Document created.
              Aug 12, 2009: JPT, Linux now includes from system.

#<

*/

#include	"osdef.h"			/* get __'machine'__ */

#if	!defined(_STDIO_H)
#if	defined(__aix__)
#elif	defined(__alliant__)
#elif	defined(__alpha__)
#elif	defined(__convex__)
#elif	defined(__cray__)
#elif	defined(__hp9000s300__)
#elif	defined(__hp9000s700__)
#elif	defined(__mips__)
#elif	defined(__sgi__)
#elif	defined(__sun__)
#elif	defined(__vms__)
#else
#include	<stdio.h>			/* from system */
#if	!defined(_STDIO_H)
#define	_STDIO_H
#endif
#endif
#endif

#if	!defined(_STDIO_H)
#define	_STDIO_H

#if	!defined(_STDARG_H)
#include	"stdarg.h"
#endif

#if	!defined(_STDDEF_H)
#include	"stddef.h"
#endif

#if	defined(__aix__)			/* AIX */

#ifndef _H_STDIO
#define _H_STDIO
#endif

#ifndef _FPOS_T
#define	_FPOS_T
typedef long	fpos_t;
#endif

#ifndef NULL
#define NULL	((void *)0)
#endif
#define BUFSIZ		4096
#define FILENAME_MAX 	255
#define _P_tmpdir       "/tmp/"
#define L_tmpnam	(sizeof(_P_tmpdir) + 15)
#define	FOPEN_MAX	2000			/* Maximum # open files */
#define	OPEN_MAX	20			/* ... */
#define	TMPMAX		16384
#define _IOEOF		0020
#define _IOERR		0040
#define _IOFBF		0000
#define _IOLBF		0100
#define _IONBF		0004

typedef struct {
	unsigned char	*_ptr;
	int	_cnt;
	unsigned char	*_base;
	unsigned char   *_bufendp;
	short	_flag;
	short	_file;
	int	_unused[3];
} FILE;

extern	FILE	_iob[OPEN_MAX];

#define __nonstd   _unused[0]
#define __stdioid  _unused[1]

#define stdin		(&_iob[0])
#define stdout		(&_iob[1])
#define stderr		(&_iob[2])

#define EOF		(-1)

#define getc(p)		(--(p)->_cnt < 0 ? _filbuf(p) : (int) *(p)->_ptr++)
#define putc(x, p)	(--(p)->_cnt < 0 ? \
			_flsbuf((unsigned char) (x), (p)) : \
			(int) (*(p)->_ptr++ = (unsigned char) (x)))
#define getchar()	getc(stdin)
#define putchar(x)	putc((x), stdout)
#define clearerr(p)	((void) ((p)->_flag &= ~(_IOERR | _IOEOF)))
#define feof(p)		((p)->_flag & _IOEOF)
#define ferror(p)	((p)->_flag & _IOERR)

#define	fprintf		fprintf_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#elif	defined(__alliant__)			/* ALLIANT */

#ifndef	_STDIO_H_
#define	_STDIO_H_				/* disable local <stdio.h> */
#endif

#define	BUFSIZ		4096			/* I/O buffer size */
#define	FILENAME_MAX	1024			/* Maximum pathname length */
#define	L_tmpnam	13			/* Number of chars in tmpnam */
#define	FOPEN_MAX	64			/* Maximum number of files */
#define	OPEN_MAX	20			/* ... */
#define	TMP_MAX		0xffff			/* Max. temp. names guaranteed */
#define	_IOEOF		0x010			/* EOF indicator */
#define	_IOERR		0x020			/* error indicator */
#define	_IOFBF		0x000			/* Sets full buffering */
#define _IOLBF		0x080			/* Sets line buffering */
#define	_IONBF		0x004			/* Sets no buffering */

extern	struct	_iobuf {			/* FILE pointer structure */
	int	_cnt;
	char	*_ptr;				/* should be unsigned char */
	char	*_base;				/* ditto */
	int	_bufsiz;
	short	_flag;
	char	_file;				/* should be short */
} _iob[OPEN_MAX];

#define	FILE		struct	_iobuf		/* define FILE pointer */

#define	stdin		(&_iob[0])		/* standard input */
#define	stdout		(&_iob[1])		/* standard output */
#define	stderr		(&_iob[2])		/* standard error */

extern	int		_filbuf( FILE *p );
extern	int		_flsbuf( unsigned char x, FILE *p );

#define	getc(p)		(--(p)->_cnt>=0 ?				\
			(int)(*(unsigned char *)(p)->_ptr++):_filbuf(p))
#define	putc(x,p)	(--(p)->_cnt >= 0 ?				\
			(int)(*(unsigned char *)(p)->_ptr++ = (x)) :	\
			(((p)->_flag & _IOLBF) && -(p)->_cnt < (p)->_bufsiz ?\
			((*(p)->_ptr = (x)) != '\n' ?			\
			(int)(*(unsigned char *)(p)->_ptr++) :		\
			_flsbuf(*(unsigned char *)(p)->_ptr, p)) :	\
			_flsbuf((unsigned char)(x), p)))

typedef	unsigned long	fpos_t;			/* for fgetpos and fsetpos */

#define	clearerr(p)	((p)->_flag &= ~(_IOERR|_IOEOF))
#define	EOF		(-1)			/* End Of File */
#define	getchar()	getc(stdin)		/* Get character from stdin */
#define	feof(p)		(int)(((p)->_flag & _IOEOF) != 0)
#define	ferror(p)	(int)(((p)->_flag & _IOERR) != 0)
#define	putchar(x)	putc(x,stdout)		/* Put character on stdout */

#define fgetpos		fgetpos_x		/* from xclib */
#define	fprintf		fprintf_x		/* from xclib */
#define	fsetpos		fsetpos_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	remove		remove_x		/* from xclib */
#define	setvbuf		setvbuf_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	tmpfile		tmpfile_x		/* from xclib */
#define	tmpnam		tmpnam_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#elif	defined(__alpha__)			/* DEC ALPHA */

#ifndef _STDIO_H_
#define _STDIO_H_
#endif

#include	<standards.h>

#ifndef _FPOS_T
#define	_FPOS_T
typedef long	fpos_t;
#endif

#define BUFSIZ		8192
#define FILENAME_MAX 	255
#define _P_tmpdir       "/tmp/"
#define L_tmpnam	(sizeof(_P_tmpdir) + 15)
#define _NIOBRW		8
#define FOPEN_MAX 	64
#define OPEN_MAX 	_NIOBRW
#define TMP_MAX         16384
#define _IOEOF		0020
#define _IOERR		0040
#define _IOLBF		0200
#define _IONBF		0004
#define _IOFBF		0000

typedef struct {
	int	_cnt;
	unsigned char	*_ptr;
	unsigned char	*_base;
	int	_bufsiz;
	short	_flag;
	short	_file;

	int	_unused[2];
	void	*_lock;			/* lock for thread safe library */

	unsigned char   *_bufendp;
} FILE;

extern FILE	_iob[OPEN_MAX];

#define stdin		(&_iob[0])
#define stdout		(&_iob[1])
#define stderr		(&_iob[2])

extern int _flsbuf(unsigned int, FILE *);
extern int _filbuf(FILE *);

#define getc(p)		(--(p)->_cnt < 0 ? _filbuf(p) : (int) *(p)->_ptr++)
#define putc(x, p)	(--(p)->_cnt < 0 ? \
			_flsbuf((unsigned char) (x), (p)) : \
			(int) (*(p)->_ptr++ = (unsigned char) (x)))

#define EOF        (-1)
#define clearerr(p)	((void) ((p)->_flag &= ~(_IOERR | _IOEOF)))
#define feof(p)		((p)->_flag & _IOEOF)
#define ferror(p)	((p)->_flag & _IOERR)
#define getchar()	getc(stdin)
#define putchar(x)	putc((x), stdout)

#define	fprintf		fprintf_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#elif	defined(__convex__)			/* CONVEX */

#ifndef	__STDIO_H_SEEN				/* disable <stdio.h> */
#define	__STDIO_H_SEEN
#endif

#define	BUFSIZ		1024			/* I/O buffer size */
#define	FILENAME_MAX	256			/* Maximum pathname length */
#define	L_tmpnam	20			/* Number of chars in tmpnam */
#define	FOPEN_MAX	256			/* Maximum number of files */
#define	OPEN_MAX	256			/* ... */
#define	TMP_MAX		11000000		/* Max. temp. names guaranteed */
#define	_IOEOF		0x010			/* EOF indicator */
#define	_IOERR		0x020			/* error indicator */
#define	_IOFBF		0x200			/* Sets full buffering */
#define _IOLBF		0x080			/* Sets line buffering */
#define	_IONBF		0x004			/* Sets no buffering */

#if	defined(__GNUC__)

extern	struct	_iobuf {			/* FILE pointer structure */
	int	_cnt;
	char	*_ptr;				/* should be unsigned char */
	char	*_base;				/* ditto */
	int	_bufsiz;
	short	_flag;
	unsigned char	_file;			/* associated file descriptor */
	unsigned char	_sema_ndx;		/* index to semaphore */
} _iob[OPEN_MAX];

#define	FILE		struct	_iobuf		/* define FILE pointer */

#define	stdin		(&_iob[0])		/* standard input */
#define	stdout		(&_iob[1])		/* standard output */
#define	stderr		(&_iob[2])		/* standard error */

extern	int		use_libc_sema;
extern	int		_filbuf( FILE *p );
extern	int		_flsbuf( unsigned char x, FILE *p );

#define	getc(p)		(use_libc_sema?fgetc(p):(--(p)->_cnt>=0?	\
			*(p)->_ptr++&0377:_filbuf(p)))
#define	putc(x,p)	(use_libc_sema?fputc((x),p):(--(p)->_cnt>=0?	\
			((int)(*(p)->_ptr++=(unsigned)(x))):		\
			_flsbuf((unsigned)(x),p)))

#else

typedef struct __ap$file {
	int		_cnt;
	char		*_ptr;
	char		*_base;
	int		_bufsiz;
	short		_flag;
	unsigned char	_file;                                        
	unsigned char	_sema_ndx;
} FILE;

extern	FILE	__ap$iob[];

#define	stdin	(&__ap$iob[0])
#define	stdout	(&__ap$iob[1])
#define	stderr	(&__ap$iob[2])

extern	int	__ap$use_libc_sema;
extern	int	__ap$filbuf(FILE *);
extern	int	__ap$$flsbuf(int, FILE *);

#define	getc(p)		(__ap$use_libc_sema ? \
			fgetc(p): \
			(--(p)->_cnt>=0? *(p)->_ptr++&0377:__ap$filbuf(p)) \
			)
#define	putc(x,p)	(__ap$use_libc_sema ? \
			fputc((x),p) : \
			(--(p)->_cnt>=0 ? \
			((int)(*(p)->_ptr++=(unsigned)(x))):__ap$flsbuf((x),p)))

#endif

typedef	struct { int __offset[2]; }	fpos_t;		/* for fgetpos and fsetpos */

#define	clearerr(p)	((p)->_flag &= ~(_IOERR|_IOEOF))
#define	EOF		(-1)			/* End Of File */
#define	getchar()	getc(stdin)		/* Get character from stdin */
#define	feof(p)		(int)(((p)->_flag & _IOEOF) != 0)
#define	ferror(p)	(int)(((p)->_flag & _IOERR) != 0)
#define	putchar(x)	putc(x,stdout)		/* Put character on stdout */

#define	fprintf		fprintf_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	remove		remove_x		/* from xclib */
#define	setvbuf		setvbuf_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	tmpfile		tmpfile_x		/* from xclib */
#define	tmpnam		tmpnam_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#elif	defined(__cray__)			/* CRAY */

#include	"/usr/include/stdio.h"		/* Use Standard */

#elif	defined(__hp9000s300__)			/* HP UNIX Series 300 */

#ifndef	_STDIO_INCLUDED				/* disable <stdio.h> */
#define	_STDIO_INCLUDED
#endif

#define	BUFSIZ		1024			/* I/O buffer size */
#define	FILENAME_MAX	255			/* Maximum pathname length */
#define	_P_tmpdir	"/usr/tmp/"
#define L_tmpnam	(sizeof(_P_tmpdir) + 15)/* Number of chars in tmpnam */
#define	FOPEN_MAX	60			/* Maximum number of files */
#define	OPEN_MAX	60			/* ... */
#define	TMP_MAX		17576			/* Max. temp. names guaranteed */
#define	_IOEOF		0x010			/* EOF indicator */
#define	_IOERR		0x020			/* error indicator */
#define	_IOFBF		0x000			/* Sets full buffering */
#define _IOLBF		0x080			/* Sets line buffering */
#define	_IONBF		0x004			/* Sets no buffering */

#if	OS_MAJOR_VERSION < 9

extern struct __iobuf {				/* FILE pointer structure */
	int		__cnt;
	unsigned char	*__ptr;
	unsigned char	*__base;
	short		__flag;
	char		__file;			/* should be short */
} __iob[OPEN_MAX];				/* FILE pointers */

#define	FILE		struct	__iobuf		/* define FILE pointer */

#else

typedef struct {				/* FILE pointer structure */
	int		__cnt;
	unsigned char	*__ptr;
	unsigned char	*__base;
	unsigned short	__flag;
	unsigned char	__fileL;
	unsigned char	__fileH;
} FILE;

extern	FILE		__iob[];

#endif

#define	stdin		(&__iob[0])		/* standard input */
#define	stdout		(&__iob[1])		/* standard output */
#define	stderr		(&__iob[2])		/* standard error */

extern	int		__filbuf( FILE *p );
extern	int		__flsbuf( unsigned char x, FILE *p );

#define	getc(__p)	(--(__p)->__cnt < 0 ? __filbuf(__p) : \
			(int) *(__p)->__ptr++)
#define	putc(__x, __p)	(--(__p)->__cnt < 0 ? \
			__flsbuf((unsigned char) (__x), (__p)) : \
			(int) (*(__p)->__ptr++ = (unsigned char) (__x)))

typedef	long int	fpos_t;			/* for fgetpos and fsetpos */

#define	clearerr(__p)	((void) ((__p)->__flag &= ~(_IOERR | _IOEOF)))
#define	EOF		(-1)			/* End Of File */
#define	getchar()	getc(stdin)		/* Get character from stdin */
#define	feof(__p)	((__p)->__flag & _IOEOF)
#define	ferror(__p)	((__p)->__flag & _IOERR)
#define	putchar(__x)	putc((__x), stdout)

#define	fprintf		fprintf_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#elif	defined(__hp9000s700__)			/* HP UNIX Series 700 */

#ifndef	_STDIO_INCLUDED				/* disable local <stdio.h> */
#define	_STDIO_INCLUDED
#endif
#define	BUFSIZ		1024			/* I/O buffer size */
#define	FILENAME_MAX	255			/* Maximum pathname length */
#define	_P_tmpdir	"/usr/tmp/"
#define L_tmpnam	(sizeof(_P_tmpdir) + 15)/* Number of chars in tmpnam */
#define	FOPEN_MAX	60			/* Maximum number of files */
#define	OPEN_MAX	60			/* ... */
#define	TMP_MAX		17576			/* Max. temp. names guaranteed */
#define	_IOEOF		0x010			/* EOF indicator */
#define	_IOERR		0x020			/* error indicator */
#define	_IOFBF		0x000			/* Sets full buffering */
#define _IOLBF		0x080			/* Sets line buffering */
#define	_IONBF		0x004			/* Sets no buffering */

typedef struct {				/* FILE pointer structure */
	int		__cnt;
	unsigned char	*__ptr;
	unsigned char	*__base;
	unsigned short	__flag;
	unsigned char	__fileL;		/* should be short */
	unsigned char	__fileH;
} FILE;						/* FILE pointers */

extern	FILE		__iob[];		/* define FILE pointer */

#define	stdin		(&__iob[0])		/* standard input */
#define	stdout		(&__iob[1])		/* standard output */
#define	stderr		(&__iob[2])		/* standard error */

extern	int		__filbuf( FILE *p );
extern	int		__flsbuf( unsigned char x, FILE *p );

#define	getc(__p)	(--(__p)->__cnt < 0 ? __filbuf(__p) : \
			(int) *(__p)->__ptr++)
#define	putc(__x, __p)	(--(__p)->__cnt < 0 ? \
			__flsbuf((unsigned char) (__x), (__p)) : \
			(int) (*(__p)->__ptr++ = (unsigned char) (__x)))

#ifndef	_FPOS_T
#define	_FPOS_T
typedef	long int	fpos_t;			/* for fgetpos and fsetpos */
#endif

#define	clearerr(__p)	((void) ((__p)->__flag &= ~(_IOERR | _IOEOF)))
#define	EOF		(-1)			/* End Of File */
#define	getchar()	getc(stdin)		/* Get character from stdin */
#define	feof(__p)	((__p)->__flag & _IOEOF)
#define	ferror(__p)	((__p)->__flag & _IOERR)
#define	putchar(__x)	putc((__x), stdout)

#define	fprintf		fprintf_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#elif	defined(__linux__)			/* LINUX */

#define	fscanf		dummy_fscanf		/* scramble */
#define	fwrite		dummy_fwrite		/* scramble */
#define	sscanf		dummy_sscanf		/* scramble */

#undef	_STDIO_H

#include	"/usr/include/stdio.h"		/* use standard */

#ifndef	_STDIO_H
#define	_STDIO_H
#endif

#undef	fscanf					/* unscramble */
#undef	fwrite					/* unscramble */
#undef	sscanf					/* unscramble */

#define	fprintf		fprintf_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#elif	defined(__mips__)			/* DEC ULTRIX */

#ifndef	_STDIO_H_				/* disable <stdio.h> */
#define	_STDIO_H_
#endif

#define	BUFSIZ		1024			/* I/O buffer size */
#define	FILENAME_MAX	1024			/* Maximum pathname length */
#define	L_tmpnam	25			/* Number of chars in tmpnam */
#define	FOPEN_MAX	64			/* Maximum number of files */
#define	OPEN_MAX	64			/* ... */
#define	TMP_MAX		17576			/* Max. temp. names guaranteed */
#define	_IOEOF		0x010			/* EOF indicator */
#define	_IOERR		0x020			/* error indicator */
#define	_IOFBF		0x000			/* Sets full buffering */
#define _IOLBF		0x080			/* Sets line buffering */
#define	_IONBF		0x004			/* Sets no buffering */

extern	struct	_iobuf {			/* FILE pointer structure */
	int	_cnt;
	char	*_ptr;				/* should be unsigned char */
	char	*_base;				/* ditto */
	int	_bufsiz;
	short	_flag;
	char	_file;				/* should be short */
} _iob[OPEN_MAX];

#define	FILE		struct	_iobuf		/* define FILE pointer */

#define	stdin		(&_iob[0])		/* standard input */
#define	stdout		(&_iob[1])		/* standard output */
#define	stderr		(&_iob[2])		/* standard error */

extern	int		_filbuf( FILE *p );
extern	int		_flsbuf( unsigned char x, FILE *p );

#define	getc(p)		(--(p)->_cnt>=0? *(p)->_ptr++&0377:_filbuf(p))
#define	putc(x,p)	(--(p)->_cnt>=0? ((int)(*(p)->_ptr++=(unsigned)(x))):\
			_flsbuf((unsigned)(x),p))

typedef	unsigned long	fpos_t;			/* for fgetpos and fsetpos */

#define	clearerr(p)	((p)->_flag &= ~(_IOERR|_IOEOF))
#define	EOF		(-1)			/* End Of File */
#define	getchar()	getc(stdin)		/* Get character from stdin */
#define	feof(p)		(int)(((p)->_flag & _IOEOF) != 0)
#define	ferror(p)	(int)(((p)->_flag & _IOERR) != 0)
#define	putchar(x)	putc(x,stdout)		/* Put character on stdout */

#define fgetpos		fgetpos_x		/* from xclib */
#define	fopen		fopen_x			/* from xclib */
#define	fprintf		fprintf_x		/* from xclib */
#define	fsetpos		fsetpos_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	remove		remove_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#elif	defined(__sgi__)			/* SILICON GRAPHICS */

#ifndef __STDIO_H__
#define __STDIO_H__
#endif

extern unsigned char *	_bufendtab[];

#define _bufend(p)	_bufendtab[(p)->_file]
#define _bufsiz(p)	(_bufend(p) - (p)->_base)

#define FOPEN_MAX	100
#define _NFILE		FOPEN_MAX
#define _P_tmpdir	"/usr/tmp/"

#define	BUFSIZ		8192			/* I/O buffer size */
#define FILENAME_MAX	255			/* Maximum pathname length */
#define L_tmpnam	(sizeof(_P_tmpdir) + 15)/* # of chars in tmp name */
#define	OPEN_MAX	FOPEN_MAX		/* Maximum number of files */
#define TMP_MAX		17576			/* Max. tmp names garanteed */

#define _IOFBF		0000	/* fully buffered */
#define _IONBF		0004	/* not buffered */
#define _IOLBF		0100	/* line buffered */
#define _IOREAD		0001
#define _IOWRT		0002
#define _IOMYBUF	0010
#define _IOEOF		0020
#define _IOERR		0040
#define _IORW		0200
#define _SBFSIZ		8

typedef struct __file_s {
	int	_cnt;
	unsigned char	*_ptr;
	unsigned char	*_base;
	char	_flag;
	char	_file;
} FILE;

extern	FILE		_iob[];

#define stdin		(&_iob[0])
#define stdout		(&_iob[1])
#define stderr		(&_iob[2])

extern	int		_flsbuf(unsigned,FILE*);
extern	int		_filbuf(FILE*);
extern	int		_semputc(int, FILE *);
extern	int		_semgetc(FILE *);
extern	void		*_sproced;
extern	int		_us_rsthread_stdio;

#define getc(p)		( _us_rsthread_stdio ? _semgetc(p) : \
			((--(p)->_cnt > 0 || \
	     		(((p)->_cnt == 0 ) && ((p)->_ptr != (p)->_base)))?\
			(int) *(p)->_ptr++:\
			((p)->_cnt == 0 ? (int) *(p)->_ptr:_filbuf(p))))
#define putc(x,p)	( _us_rsthread_stdio ? _semputc(x,p) : \
			(--(p)->_cnt < 0 ? _flsbuf((unsigned char) (x), (p)) : \
			(int) (*(p)->_ptr++ = (unsigned char) (x))))

typedef unsigned fpos_t;

#define clearerr(p)	((void) ((p)->_flag &= ~(_IOERR | _IOEOF)))
#define	EOF		(-1)
#define getchar()	getc(stdin)
#define feof(p)		((p)->_flag & _IOEOF)
#define ferror(p)	((int) (p)->_flag & _IOERR)
#define putchar(x)	putc((x),stdout)

#define	fprintf		fprintf_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#elif	defined(__sun__)			/* SUN */

#define	BUFSIZ		1024			/* I/O buffer size */
#define	FILENAME_MAX	1024			/* Maximum pathname length */
#ifdef	__bsd__
#define	L_tmpnam	25			/* Number of chars in tmpnam */
#else
#define	L_tmpnam	21			/* Number of chars in tmpnam */
#endif
#define	FOPEN_MAX	64			/* Maximum number of files */
#ifdef	__bsd__
#define	OPEN_MAX	64			/* ... */
#else
#define	OPEN_MAX	20			/* ... */
#endif
#define	TMP_MAX		17576			/* Max. temp. names guaranteed */
#define	_IOEOF		0x010			/* EOF indicator */
#define	_IOERR		0x020			/* error indicator */
#define	_IOFBF		0x000			/* Sets full buffering */
#define _IOLBF		0x080			/* Sets line buffering */
#define	_IONBF		0x004			/* Sets no buffering */

#ifdef	__bsd__					/* Sunos < 5.0 */

extern	struct	_iobuf {			/* FILE pointer structure */
	int	_cnt;
	unsigned char	*_ptr;
	unsigned char	*_base;
	int	_bufsiz;
	short	_flag;
	char	_file;				/* should be short */
} _iob[OPEN_MAX];

#define	FILE		struct	_iobuf		/* define FILE pointer */

#define	stdin		(&_iob[0])		/* standard input */
#define	stdout		(&_iob[1])		/* standard output */
#define	stderr		(&_iob[2])		/* standard error */

extern	int		_filbuf( FILE *p );
extern	int		_flsbuf( unsigned char x, FILE *p );

#define	getc(p)		(--(p)->_cnt>=0? ((int)*(p)->_ptr++):_filbuf(p))
#define	putc(x,p)	(--(p)->_cnt >= 0 ?				\
			(int)(*(p)->_ptr++ = (unsigned char)(x)) :	\
			(((p)->_flag & _IOLBF) && -(p)->_cnt < (p)->_bufsiz ?\
			((*(p)->_ptr = (unsigned char)(x)) != '\n' ?	\
			(int)(*(p)->_ptr++) :				\
			_flsbuf(*(unsigned char *)(p)->_ptr, p)) :	\
			_flsbuf((unsigned char)(x), p)))

typedef	unsigned long	fpos_t;			/* for fgetpos and fsetpos */

#define	clearerr(p)	((p)->_flag &= ~(_IOERR|_IOEOF))
#define	EOF		(-1)			/* End Of File */
#define	getchar()	getc(stdin)		/* Get character from stdin */
#define	feof(p)		(int)(((p)->_flag & _IOEOF) != 0)
#define	ferror(p)	(int)(((p)->_flag & _IOERR) != 0)
#define	putchar(x)	putc(x,stdout)		/* Put character on stdout */

#define fgetpos		fgetpos_x		/* from xclib */
#define	fsetpos		fsetpos_x		/* from xclib */
#define	remove		remove_x		/* from xclib */

#else						/* Sunos >= 5.0 */

#define	stdin	(&__iob[0])
#define	stdout	(&__iob[1])
#define	stderr	(&__iob[2])

typedef struct	/* needs to be binary-compatible with old versions */
{
#ifdef _STDIO_REVERSE
	unsigned char	*_ptr;	/* next character from/to here in buffer */
	int		_cnt;	/* number of available characters in buffer */
#else
	int		_cnt;	/* number of available characters in buffer */
	unsigned char	*_ptr;	/* next character from/to here in buffer */
#endif
	unsigned char	*_base;	/* the buffer */
	unsigned char	_flag;	/* the state of the stream */
	unsigned char	_file;	/* UNIX System file descriptor */
} FILE;

extern FILE		__iob[OPEN_MAX];
extern FILE		*_lastbuf;
extern unsigned char	*_bufendtab[];

extern int	__filbuf(FILE *);
extern int	__flsbuf(int, FILE *);

#define	getc(p)		(--(p)->_cnt < 0 ? __filbuf(p) : (int)*(p)->_ptr++)
#define	putc(x, p)	(--(p)->_cnt < 0 ? __flsbuf((unsigned char) (x), (p)) \
				: (int)(*(p)->_ptr++ = (x)))

typedef  long		fpos_t;			/* for fgetpos and fsetpos */

#define	clearerr(p)	((void)((p)->_flag &= ~(_IOERR | _IOEOF)))
#define	EOF		(-1)
#define	getchar()	getc(stdin)
#define	feof(p)		((p)->_flag & _IOEOF)
#define	ferror(p)	((p)->_flag & _IOERR)
#define	putchar(x)	putc((x), stdout)

#endif

#define	fprintf		fprintf_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#elif	defined(__vms__)			/* DEC VMS */

#define	BUFSIZ		512			/* I/O buffer size */
#define	FILENAME_MAX	255			/* Maximum pathname length */
#define	L_tmpnam	256			/* Number of chars in tmpnam */
#define	FOPEN_MAX	64			/* Maximum number of files */
#define	OPEN_MAX	8			/* ... */
#define	TMP_MAX		32			/* Max. temp. names guaranteed */
#define	_IOEOF		0x010			/* EOF indicator */
#define	_IOERR		0x020			/* error indicator */
#define	_IOFBF		0x002			/* Sets full buffering */
#define _IOLBF		0x001			/* Sets line buffering */
#define	_IONBF		0x004			/* Sets no buffering */

extern	struct	_iobuf {			/* FILE pointer structure */
	int	_cnt;
	char	*_ptr;				/* should be unsigned char */
	char	*_base;				/* ditto */
	char	_flag;
	char	_file;				/* should be short */
};

typedef	struct	_iobuf	*FILE;			/* define FILE pointer */

extern	noshare	FILE	*stdin;			/* standard input */
extern	noshare	FILE	*stdout;		/* standard output */
extern	noshare	FILE	*stderr;		/* standard error */

#define	getc(p)		fgetc(p)
#define	putc(x,p)	fputc(x,p)

typedef	struct {
	unsigned : 32;
	unsigned : 32;
} fpos_t;					/* for fgetpos and fsetpos */

#define	clearerr(p)	((*p)->_flag &= ~(_IOERR|_IOEOF))
#define	EOF		(-1)			/* End Of File */
#define	getchar()	fgetc(stdin)		/* Get character from stdin */
#define	feof(p)		(((*p)->_flag & _IOEOF) != 0)
#define	ferror(p)	(((*p)->_flag & _IOERR) != 0)
#define	putchar(x)	fputc(x,stdout)		/* Put character on stdout */

#define	fprintf		fprintf_x		/* from xclib */
#define	printf		printf_x		/* from xclib */
#define	remove		remove_x		/* from xclib */
#define	rename		rename_x		/* from xclib */
#define	sprintf		sprintf_x		/* from xclib */
#define	vprintf		vprintf_x		/* from xclib */
#define	vfprintf	vfprintf_x		/* from xclib */
#define	vsprintf	vsprintf_x		/* from xclib */

#endif

#define	SEEK_SET	0			/* Seek from begin of file */
#define	SEEK_CUR	1			/* Seek from current position */
#define SEEK_END	2			/* Seek from end of file */

extern	char	*fgets(char *s, int n, FILE *stream);
extern	char	*gets(char *s);
extern	char	*tmpnam(char s[]);
extern	FILE 	*fopen(const char *filename, const char *mode);
extern	FILE	*freopen(const char *filename, const char *mode,
		         FILE *stream);
extern	FILE	*tmpfile(void);
extern	int	fclose(FILE *stream);
extern	int	fflush(FILE *stream);
extern	int	fgetc(FILE *stream);
extern	int	fgetpos(FILE *stream, fpos_t *ptr);
extern	int	fprintf(FILE *stream, const char *format, ...);
extern	int	fputc(int c, FILE *stream);
extern	int	fputs(const char *s, FILE *stream);
extern	int	fscanf(FILE *stream, const char *format, ...);
extern	int	fseek(FILE *stream, long offset, int origin);
extern	int	fsetpos(FILE *stream, const fpos_t *ptr);
extern	int	printf(const char *format, ...);
extern	int	puts(const char *s);
extern	int	remove(const char *filename);
extern	int	rename(const char *oldname, const char *newname);
extern	int	scanf(const char *format, ...);
extern	int	setvbuf(FILE *stream, char *buf, int mode, size_t size);
extern	int	sprintf(char *s, const char *format, ...);
extern	int	sscanf(char *s, const char *format, ...);
extern	int	ungetc(int c, FILE *stream);
extern	int	vfprintf(FILE *stream, const char *format, va_list arg);
extern	int	vprintf(const char *format, va_list arg );
extern	int	vsprintf(char *s, const char*format, va_list arg);
extern	long	ftell(FILE *stream);
extern	size_t	fread(void *ptr, size_t size, size_t nobj, FILE *stream);
extern	size_t	fwrite(void *ptr, size_t size, size_t nobj, FILE *stream);
extern	void	perror(const char *s);
extern	void	rewind(FILE *stream);
extern	void	setbuf(FILE *stream, char *buf);

#endif
