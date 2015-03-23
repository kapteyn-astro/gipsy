/* dms.c
                              COPYRIGHT (c) 1991
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>            dms.dc2

Function:     DMS

Purpose:      Convert degrees to a string containing the degrees, 
              minutes and seconds.

Category:     COORDINATES

File:         dms.c

Author:       Fred Lahuis

Use:          CALL DMS(  DEGREES,        Input      double
                         STRING,         Output     char*(*)
                         COOR,           Output     double array (n = 3)
                         PRECISION       Input      integer
                         OUTPUT)         Input      integer

              DEGREES     Input value in degrees
              STRING      Output string, format xxd~xxm~xx.xxs
                          or xxdxx'xx" (see OUTPUT).
              COOR        Array of size 3 to receive the degrees,
                          minutes and seconds.
                          Omitted if coor is equal to NULL.
              PRECISION   Indicates the precision in which the
                          seconds are printed.
              OUTPUT      Type of output
                            0 --> with d m s
                            1 --> with d ' ", for use with pgplot

Updates:      Jul 15, 1991: FL, Creation date.
              Feb 12, 1992: MV, New calculation of minuts to avoid
                                rounding errors.
              Oct 19, 1992: FL, Output string changed for 0s.
              Nov  9, 1992: FL, Error for negative zero degree removed.
              Nov 13, 1992: FL, Proper handling of 60s, 60m and 360d.
              Sep 17, 1993: FL, Error in length convstr.
#<

Fortran to C interface:

@ subroutine dms( double precision, character, double precision, integer, integer )

*/

#include "ctype.h"
#include "stdio.h"
#include "stdlib.h"
#include "gipsyc.h"
#include "math.h"
#include "string.h"
#include "anyout.h"

#define width(w)	((int)((w)?(3+w):2))
#define nint(a)		(int)((a)+0.5)
#define MAXTEXTLEN	30

void dms_c(	double *deg,
		fchar convstr,
		double *coor,
		fint *prec,
		fint *output )
{
	fint	l0, l1, l2, n , negative ;
	fint	degree , minut ;
	double	second ;
	double  deg_in ;
	char	cstring[MAXTEXTLEN] ;

	deg_in = *deg ;
	if( deg_in < 0.0 ){ negative = 1 ; deg_in = -deg_in ; }
	else negative = 0 ;
	degree = (int) deg_in ;
	minut = (int) ( deg_in*60.0 - (double)(degree*60) );
	minut = abs(minut) ;
	second =  ( (deg_in - (double)degree -
			(double)(minut/60.0))*3600.0
			) ;
	second = fabs( second ) ;
	if( coor != NULL ){
		coor[0] = (double)degree ;
		if( negative ) coor[0] = -coor[0] ;
		coor[1] = (double)minut ;
		if( coor[0] == 0.0 && negative ) coor[1] = -coor[1] ;
		coor[2] = second ;
	}

	if( !*prec && nint(second) == 60 ){
		second -= 60.0 ;
		minut += 1 ;
	}
	if( minut == 60 || minut == 61 ){
		minut -= 60 ;
		degree += 1 ;
	}
	if( degree == 360 ) degree = 0 ;

	if( negative ) l0 = sprintf( cstring, "-" ) ;
	else l0 = sprintf( cstring, "" ) ;
	if( *output == 0 ){
		l1 = sprintf( cstring, "%s%dd %2dm", cstring, degree, minut ) ;
		if( !*prec && second < 0.5 )
			l2 = sprintf( cstring, "%s 00s", cstring ) ;
		else l2 = sprintf( cstring, "%s %*.*fs", cstring, width(*prec), 
					(int)(*prec), second ) ;
	}
	else{
		l1 = sprintf( cstring, "%s%d\\uo\\d%2d\\u'\\d", cstring, degree, minut ) ;
		if( !*prec && second < 0.5 )
			l2 = sprintf( cstring, "%s00\\u\"\\d", cstring ) ;
		else l2 = sprintf( cstring, "%s%*.*f\\u\"\\d", cstring, 
					width(*prec), (int)(*prec), second ) ;
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

	degree = 359. + 59.0/60.0 + 59.8/3600.0 ;
	degree = -degree ;
	for( output = 0 ; output <= 1 ; output++ ){
		for( prec = 0 ; prec <= 3 ; prec++ ){
			dms_c( &degree, Fstring, coor, &prec, &output ) ;
			printf( "%s\n", Fstring.a ) ;
			printf( "coor: %d %d %.3f\n", 
					(int)coor[0] ,
					(int)coor[1] ,
					coor[2] ) ;
		}
	}

}
#endif
