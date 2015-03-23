/* spline.c


#>            spline.dc2

Purpose:      Spline interpolation routines.

Category:     MATH

File:         spline.c

Author:       P.R. Roelfsema

Description:

              Spline.c contains a number of routines relevant for
              spline interpolations, taken from "Numerical recipes
              in C" (modified for use in the GIPSY environment).
              The following are available:

              SPLINE1  - 1D cbic spline interpolation.

Updates:      Jan 29, 1991: PRR, Creation date

#<

*/

#include "gipsyc.h"
#include "stdio.h"
#include "stdlib.h"
#include "setfblank.h"
#include "setnfblank.h"

#define OK		   0		/* no problems */
#define	NOMEMORY	  -1		/* no memory available for buffers */
#define EQALXES		  -2		/* two x-coordinates are equal */

#define true		   1
#define false		   0
#define MAXTXTLEN	  80

#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.l = len ; }

static fint klo = -1 ;
static fint khi = -1 ;


/*
    spline constructs a cubic spline given a set of x and y values, through
these values.

*/
static fint spline( float *x   , float *y   , fint  n   , 
                    float  yp1 , float  ypn , float *y2 )
{
	fint  i,k;
	float p,qn,sig,un,*u;

	u=(float *)malloc((unsigned) (n-1)*sizeof(float));
        if (!u) return( NOMEMORY ) ;
	if (yp1 > 0.99e30)
		y2[0]=u[0]=0.0;
	else {
		y2[0] = -0.5;
		u[0]=(3.0/(x[1]-x[0]))*((y[1]-y[0])/(x[1]-x[0])-yp1);
	}
	for (i=1;i<=n-2;i++) {
		sig=(x[i]-x[i-1])/(x[i+1]-x[i-1]);
		p=sig*y2[i-1]+2.0;
		y2[i]=(sig-1.0)/p;
		u[i]=(y[i+1]-y[i])/(x[i+1]-x[i]) - (y[i]-y[i-1])/(x[i]-x[i-1]);
		u[i]=(6.0*u[i]/(x[i+1]-x[i-1])-sig*u[i-1])/p;
	}
	if (ypn > 0.99e30)
		qn=un=0.0;
	else {
		qn=0.5;
		un=(3.0/(x[n-1]-x[n-2]))*(ypn-(y[n-1]-y[n-2])/(x[n-1]-x[n-2]));
	}
	y2[n-1]=(un-qn*u[n-2])/(qn*y2[n-2]+1.0);
	for (k=n-2;k>=0;k--)
		y2[k]=y2[k]*y2[k+1]+u[k];
	free(u);
        return( OK ) ;
}


/*
    splint uses the cubic spline generated with spline to interpolate values
in the XY  table.

*/
static fint splint( float *xa , float *ya , float *y2a , 
                    fint n    , float x   , float *y   )
{
        fint  r   = 0 ;
	fint  k;
	float h,b,a;

        if ( klo < 0 ){
  	   klo=0;
 	   khi=n-1;
        } else {
           if ( x < xa[klo] ) klo=0;
           if ( x > xa[khi] ) khi=n-1;
        }
	while (khi-klo > 1) {
		k=(khi+klo) >> 1;
		if (xa[k] > x) khi=k;
		else klo=k;
	}
	h=xa[khi]-xa[klo];
	if (h == 0.0) {
           setfblank_c( y ) ;
           r = EQALXES ;
        } else {
	   a=(xa[khi]-x)/h;
	   b=(x-xa[klo])/h;
	   *y=a*ya[klo]+b*ya[khi]+( (a*a*a-a)*y2a[klo]+
                                    (b*b*b-b)*y2a[khi] ) * (h*h) / 6.0;
        }
	return( r ) ;
}


/* 

#>            spline1.dc2

Function:     spline1

Purpose:      1D cubic spline interpolation.

Category:     MATH

File:         spline.c

Author:       P.R. Roelfsema

Use:          INTEGER SPLINE1( XI   ,               Input   real( > NIN )
                               YI   ,               Input   real( > NIN )
                               NIN  ,               Input   integer
                               XO   ,               Input   real( > NOUT )
                               YI   ,               Output  real( > NOUT )
                               NOUT ,               Output  integer

              SPLINE1 returns error codes:
                      >0 - number of undefined values in YI.
                       0 - no problems.
                      -1 - not enough memory to create internal tables.
                      -2 - the input array has two equal x-coordinates or
                           value in XO outside range of input coordinates >
                           interpolation problems.
              XI      Array containing input x-coordinates.
              YI      Array containing input y-coordinates.
              NIN     Number of (XI,YI) coordinate pairs.
              XO      Array containing x-coordinates for which y-coordinates
                      are to be interpolated.
              YO      Array containing interpolated y-coordinates.
                      If ALL YI are undefined, ALL YO are set to undefined.
              NOUT    Number of x-coordinates for which interpolation
                      is wanted.

Description:

		The interpolation is based on the cubic spline interpolation
	routines described in "Numerical recipes in C". For the interpolation
	data points in YI wich are undefined are skipped. Thus if undefined
	values are present, this routine will interpolate across them. 
	XI data must be in increasing order. The number of undefined values 
	in YI is returned.

Updates:      Jan 29, 1991: PRR, Creation date
              Aug  7, 1991: MV,  Reset of 'klo' in 'spline1_c'
                                 Documentation about XI.
              Aug  9, 1993: PRR, Set YO to undefined when all YI undefined.
#<

Fortran to C interface:

@ integer function spline1( real , real , integer , real , real , integer ) 

*/


fint spline1_c( float *xi , float *yi , integer *nin  ,
                float *xo , float *yo , integer *nout )
{
   float yp1 , ypn , *y2 , *xii , *yii , blank , alpha ;
   fint  error , n , m , nblank , niin ;

   setfblank_c( &blank ) ;
   nblank = 0 ;
   for ( n = 0 ; n < *nin ; n++ )
      if ( yi[ n ] == blank ) nblank++ ;

   niin = *nin - nblank ;
   if ( nblank == 0 ) {
      xii = xi ;
      yii = yi ;
   } else if ( nblank == *nin ) {
      for ( n = 0 ; n < *nout ; n++ ) 
         yo[ n ] = blank ;
      return( nblank ) ;
   } else {
      xii = (float *)malloc( ( niin ) * sizeof( float ) ) ;
      yii = (float *)malloc( ( niin ) * sizeof( float ) ) ;
      if ( !xii || !yii ) return( NOMEMORY ) ;
      for ( n = 0 , m = 0 ; ( n < *nin ) && ( m < niin ) ; n++ ) {
         if ( yi[ n ] != blank ) {
            xii[ m ] = xi[ n ] ;
            yii[ m ] = yi[ n ] ;
            m += 1 ;
         }
      }
   }

   y2 = (float *)malloc( ( niin ) * sizeof( float ) ) ;
   if ( !y2 ) return( NOMEMORY ) ;

   alpha = 1.0 ;
   yp1 = 3e30 * alpha ;
   ypn = 3e30 * alpha ;

   error = spline( xii , yii , niin , yp1 , ypn , y2 ) ;
   if ( error < 0 ) return( error ) ;
   
   klo = -1; /* Reset 'klo' before table interpolation */
   for ( n = 0  ; n < *nout ; n++ ) {
      error = splint( xii , yii , y2 , niin , xo[ n ] , &yo[ n ] ) ;
      if ( error < 0 ) return( error ) ;
   }

   if ( nblank != 0 ) {
      free( xii ) ;
      free( yii ) ;
   }
   free( y2  ) ;

   return( nblank ) ;
}

