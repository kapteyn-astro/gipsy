/* irco_deproject.c

                           Copyright (c) 1991
                Laboratory for Space Research Groningen
                       Kapteyn Laboratory Groningen
                           All Rights Reserved.
                               
#> irco_deproject.dc2

Subroutine:   irco_deproject

Purpose:      Reconstructs celestial sphere from plane projection.

Category:     IRAS

File:         irco_deproject.c

Author:       Fred Lahuis

Use:
       CALL IRCO_DEPROJECT(
                      PRID,     I      integer,
                      X, Y,     I      double precision array(N),
                      XYZ,      O      double precision array(3*N),
                      N)        I      integer.

              PRID    a projection system number (see IRCO_PRNAME),
              X, Y    the coordinates on the plane,
              XYZ     corresponding coordinates on the sphere,       
              N       number of coordinates to be deprojected.      

       This subroutine is the opposite of IRCO_PROJECT.
                                                                      
       See IRCO_PROJECT for common properties of the projection methods.
       Note that at the edge effects may cause loss of accuracy,
       and that maps that are finite (e.g. azimuthal equal-area proj.)
       may behave funny when points beyond the edge are de-projected.

       However, it is guaranteed that any point in the plane will
       be de-projected onto a point of the unit sphere.
       Projections where the formulae lend themselves to a periodic
       representation (e.g.cylindrical) will use this periodicity,
       otherwise points outside the map will be moved to the map edge.

References:   IRCO_PRNAME, IRCO_PROJECT.

Updates:      91 Dec 16 Fred Lahuis,       Creation date

Original:     PRUNPR
#<

@ subroutine irco_deproject( integer, 
@                            double precision,
@                            double precision,
@                            double precision,
@                            integer )
*/

#define	min(a,b)	( ( (a) < (b) ) ? (a) : (b) )
#define	max(a,b)	( ( (a) > (b) ) ? (a) : (b) )

#include "gipsyc.h"
#include "stdio.h"
#include "math.h"

void irco_deproject_c(	fint *prid,
			double *x,
			double *y,
			double *xyz,
			fint *n )
{
	int	i ;
	double	x2, y2, x3, y3, z3, r, r2, cox ;

	for( i = 0 ; i < *n ; i++ ){
		x2 = x[i] ;
		y2 = y[i] ;

		if( *prid <= 5 ) r2 = x2 * x2 + y2 * y2 ;

		switch( *prid ){
		case 1 :			/* 1  'Stereographic' */
			z3 = ( 4.0 - r2 ) / ( r2 + 4.0 ) ;
			r = ( 1.0 + z3 ) / 2.0 ;
			break ;
		case 2 :			/* 2  'Tangent plane' */
			z3 = sqrt( 1.0 / ( 1.0 + r2 ) ) ;
			r = z3 ;
			break ;    	
		case 3 :			/* 3  'Azimuthal equidistant' */
			r = sqrt( r2 ) ;
			z3 = cos( r ) ;
			r = sin(r) / r ;
			break ;
		case 4 :			/* 4  'Azimuthal equal area' */
			if( r2 < 4.0 )z3 = 1.0 - r2 / 2.0 ;
			else{
				r2 = 4.0 - 4.0 / r2 ;
				z3 = -1.0 ;
			}
			r = sqrt( 1.0 - r2 / 4.0 ) ;
			break ;
		case 5 :			/* 5  'Orthographic' */
			if( r2 <= 1.0 ){
				r = 1.0 ;
				z3 = sqrt( 1.0 - r2 ) ;
			}
			else{
				r = 1.0 / sqrt( r2 ) ;
				z3 = 0.0 ;
			}
			break ;
		case 6 :			/* 6  'Cylindrical equal-area' */
			x3 = min( max( y2, -1.0), 1.0 ) ;
			break ;
		case 7 :			/* 7  'Mercator' */
			x3 = exp( 2 * y2 ) ;
			x3 = ( x3 - 1.0 ) / ( x3 + 1.0 ) ;
			break ;
		case 8 :			/* 8  'Sinusoidal equal-area' */
			x3 = sin( y2 ) ;
			x2 /= cos( y2 ) ;
    			break ;
		case 9 :			/* 9  'Hammer-Aitoff equal area' */
			x2 /= 2.0 ;
			r2 = x2 * x2 + y2 * y2 ;
			if( r2 >= 4.0 )r2 = 4.0 - 4.0 / r2 ;
			r = sqrt( 1.0 - r2 / 4.0 ) ;
			x3 = y2 * r ;
    			break ;
		case 10	:			/* 10   'flat ' */
			x3 = sin( y2 ) ;
			break ;
		default :
			break ;
		}

		if( *prid <= 5 ){
			x3 = y2 * r ;
			y3 = x2 * r ;
		}
		else{
			cox = sqrt( 1.0 - x3 * x3 ) ;
			if( *prid == 9 ) x2 = 2 * asin( x2 * r / cox ) ;
			z3 = cox * cos( x2 ) ;
			y3 = cox * sin( x2 ) ;
		}

		xyz[3*i]   = z3 ;	/* !note, x and z reversed */
		xyz[3*i+1] = y3 ;
		xyz[3*i+2] = x3 ;

	}
	return ;
}
