/* hms.c
                              COPYRIGHT (c) 1991
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>            hms.dc2

Function:     HMS

Purpose:      Convert degrees to a string containing the hours, 
              minutes and seconds.

File:         hms.c

Category:     COORDINATES

Author:       Fred Lahuis

Use:          CALL HMS(  DEGREES,        Input      double
                         STRING,         Output     char*(*)
                         COOR,           Output     double array (n = 3)
                         PRECISION       Input      integer
                         OUTPUT)         Input      integer

              DEGREES     Input value in degrees
              STRING      Output string, format xxh~xxm~xx.xxs
                          or xxhxx'xx".
              COOR        Array of size 3 to receive the hours,
                          minutes and seconds.
                          Omitted if the array is equal to NULL.
              PRECISION   Indicates the precision in which the
                          seconds are printed.
              OUTPUT      Type of output
                            0 --> with h m s
                            1 --> with h ' ", for use with pgplot

Updates:      Jul 15, 1991: FL, Creation date.
              Feb 12, 1992: MV, New calculation of minuts to avoid
                                rounding errors.
              Oct 19, 1992: FL, Output string changed for 0s.
              Nov 13, 1992: FL, Proper handling of 60s, 60m and 24h.
              Sep 17, 1993: FL, Error in length convstr.
#<

Fortran to C interface:

@ subroutine hms( double precision, character, double precision, integer, integer )

*/
#include "ctype.h"
#include "stdio.h"
#include "stdlib.h"
#include "gipsyc.h"
#include "string.h"
#include "math.h"

#define width(w)	((int)((w)?(3+w):2))
#define nint(a)		(int)((a)+0.5)
#define MAXTEXTLEN	30

void hms_c(	double *deg,
		fchar convstr,
		double *coor,
		fint *prec,
		fint *output )
{
	fint	l1, l2, n, hour, minut ;
	double	second ;
	char	cstring[MAXTEXTLEN] ;

	*deg = fmod( (*deg + 360.0), 360.0 ) ;

	hour = (int) (*deg/15.0) ;
	minut = (int) ( *deg*4.0-(double)(hour*60) );
	minut = abs(minut) ;
	second =  ( ( *deg - (double)(hour*15.0) -
			(double)(minut/4.0)
			)*240.0 ) ;
	second = fabs(second) ;
	if( coor != NULL ){
		coor[0] = (double) hour ;
		coor[1] = (double) minut ;
		coor[2] = second ;
	}

	if( !*prec && nint(second) == 60 ){
		second -= 60.0 ;
		minut += 1 ;
	}
	if( minut == 60 || minut == 61 ){
		minut -= 60 ;
		hour += 1 ;
	}
	if( hour == 24 ) hour = 0 ;

	if( *output == 0 ){
		l1 = sprintf( cstring, "%dh %2dm", hour, minut ) ;
		if( !*prec && second < 0.5 )
			l2 = sprintf( cstring, "%s 00s", cstring ) ;
		else l2 = sprintf( cstring, "%s %*.*fs", cstring, width(*prec), 
					(int)(*prec), second ) ;
	}
	else{
		l1 = sprintf( cstring, "%d\\uh\\d%2d\\um\\d", hour, minut ) ;
		if( !*prec && second < 0.5 )
			l2 = sprintf( cstring, "%s00\\us\\d", cstring ) ;
		else l2 = sprintf( cstring, "%s%*.*f\\us\\d", cstring, 
				width(*prec) , (int)(*prec) , second ) ;
	}
	for( n = 0 ; n < l2 && n < convstr.l ; n++ )convstr.a[n] = cstring[n] ;
	for( ; n < convstr.l ; convstr.a[n++] = ' ' ) ;
	return ;
}

#if	defined(TESTBED)

#define fmake(fchar, size)	{ \
			static char buff[size+1] ; \
			int i ; \
			for( i = 0 ; i < size ; buff[i++] = ' ' ) ; \
			buff[i] = 0 ; \
			fchar.a = buff ; \
			fchar.l = size ; \
}

void	main()
{
double	degree ;
double	coor[3] ;
fint	output ;
fint	prec ;
fchar	Fstring ;

fmake( Fstring, 30 ) ;

	degree = 23.0 + 59.0/60.0 + 59.8/3600.0 ;
	degree *= 15.0 ;
	for( output = 0 ; output <= 1 ; output++ ){
		for( prec = 0 ; prec <= 3 ; prec++ ){
			hms_c( &degree, Fstring, coor, &prec, &output ) ;
			printf( "%s\n", Fstring.a ) ;
			printf( "coor: %d %d %.3f\n", 
					(int)coor[0] ,
					(int)coor[1] ,
					coor[2] ) ;
		}
	}

}
#endif
