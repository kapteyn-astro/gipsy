/* cubicspline.c

     Copyright (c) Kapteyn Laboratorium Groningen 2004
     All Rights Reserved.
*/
                    
                   
/*
#>            cubicspline.dc2

Function:     cubicspline

Purpose:      Cubic spline through X and Y positions.
                            
Category:     MATH
                           
File:         cubicspline.c
                          
Author:       M.G.R. Vogelaar
                        
Description:  Piecewise cubic spline trough 'np' two dimensional positions
              stored in arrays 'X' and 'Y', using 'cord' approximation.
              See 'Mathematical Elements for Computer Graphics', 2nd ed. 
              David F. Rogers, J. Alan Adams. p. 262.

Use:          void cubicspline( float  *Xus,
                                float  *Yus,
                                int    np, 
                                float  sampdist,
                                float  **XusO,
                                float  **YusO,
                                int    *nout,
                                fint   *blo,
                                fint   *bhi,
                                float  *splineleningrids )
              
              On input the variables Xus and Yus contain the contol points of 
              the spline. The number of points is 'np'.
              'XusO' and 'YusO' are pointers to arrays and are set to NULL.
              In the function 'cubicspline' memory is allocated for these arrays. 
              You need to free memory in the calling environment.
              
              On output, these arrays 'XusO' and 'YusO' contain 'nout' positions that are 
              interpolated along a cubic spline on a sample distance given 
              by 'sampdist' (usually 1). 
              Also the length of the spline in grids is returned ('splineleningrids').
            
Remarks:      
              
Updates:      Jul 26, 2004: VOG, Document created.


#<
*/

/*
#> cubicspline.h
void cubicspline( float *, float * ,int, float, float **, float **, int *, float * );
#<
*/


#include    "stdio.h"             /* NULL etc */
#include    "gipsyc.h"            /* fint etc */
#include    "math.h"              /* fabs etc */
#include    "stdlib.h"            /* Calloc etc. */
#include    "matrix.h"            /* matrix allocations */
#include    "userfio.h"           /* anyoutf */



#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )

static fint invmat( float **matrix2, int nfree )
/*
 * invmat calculates the inverse of matrix2. The algorithm used is the
 * Gauss-Jordan algorithm described in Stoer, Numerische matematik, 1 Teil.
 */
{
   double even;
   double *hv = NULL;
   double mjk;
   double rowmax;
   fint   evin;
   fint   i;
   fint   j;
   fint   k;
   fint   *per = NULL;
   fint   row;

   hv  = (double *) calloc( nfree, sizeof(double) );
   per  = (fint *) calloc( nfree, sizeof(fint) );

   for (i = 0; i < nfree; i++) per[i] = i;	/* set permutation array */
   for (j = 0; j < nfree; j++) {		/* in j-th column, ... */
      rowmax = fabs( matrix2[j][j] );		/* determine row with ... */
      row = j;					/* largest element. */
      for (i = j + 1; i < nfree; i++) {
         if (fabs( matrix2[i][j] ) > rowmax) {
            rowmax = fabs( matrix2[i][j] );
            row = i;
         }
      }
      if (matrix2[row][j] == 0.0) return( -6 );	/* determinant is zero! */
      if (row > j) {				/* if largest element not ... */
         for (k = 0; k < nfree; k++) {		/* on diagonal, then ... */
            even = matrix2[j][k];		/* permutate rows. */
            matrix2[j][k] = matrix2[row][k];
            matrix2[row][k] = even;
         }
         evin = per[j];				/* keep track of permutation */
         per[j] = per[row];
         per[row] = evin;
      }
      even = 1.0 / matrix2[j][j];		/* modify column */
      for (i = 0; i < nfree; i++) matrix2[i][j] *= even;
      matrix2[j][j] = even;
      for (k = 0; k < j; k++) {
         mjk = matrix2[j][k];
         for (i = 0; i < j; i++) matrix2[i][k] -= matrix2[i][j] * mjk;
         for (i = j + 1; i < nfree; i++) matrix2[i][k] -= matrix2[i][j] * mjk;
         matrix2[j][k] = -even * mjk;
      }
      for (k = j + 1; k < nfree; k++) {
         mjk = matrix2[j][k];
         for (i = 0; i < j; i++) matrix2[i][k] -= matrix2[i][j] * mjk;
         for (i = j + 1; i < nfree; i++) matrix2[i][k] -= matrix2[i][j] * mjk;
         matrix2[j][k] = -even * mjk;
      }
   }
   for (i = 0; i < nfree; i++) {		/* finally, repermute the ... */
      for (k = 0; k < nfree; k++) {		/* columns. */
         hv[per[k]] = matrix2[i][k];
      }
      for (k = 0; k < nfree; k++) {
         matrix2[i][k] = hv[k];
      }
   }
   free( per );
   free( hv );
   return( 0 );					/* all is well */
}




void cubicspline( float  *Xus,
                  float  *Yus,
                  int    np,           /* Number of controls */
                  float  sampdist,
                  float  **XusO,       /* Unscaled output */
                  float  **YusO,
                  int    *nout,
                  float  *splineleningrids )
/*------------------------------------------------------------------*/
/* PURPOSE: Cubic spline through X and Y.                           */
/* Piecewise cubic spline trough 'np' two dimensional positions     */
/* stored in arrays 'X' and 'Y', using 'cord' approximation.        */
/* See 'Mathematical Elements for Computer Graphics', 2nd ed.       */
/* David F. Rogers, J. Alan Adams. p. 262.                          */
/*------------------------------------------------------------------*/
{
   float    **matrix;
   int      row, col;
   float    *t;
   float    *Vx;               /* matrix.P' = Vxy */
   float    *Vy;
   float    *VRx;              /* P' = matrix^-1.VRxy */
   float    *VRy;
   int      i;
   int      seg;
   int      nO = 0;
   float    xlo;
   float    ylo;
   float    xhi;
   float    yhi;
   float    *X, *Y;
   float    *XO = NULL, *YO = NULL;
   float    m;                 /* Slope for scaling */
   float    disttot = 0.0;     /* Total distance along spline in scaled frame */
   float    xold, yold;        /* Used to calculate distance to previous position */   
   float    scaledsampdist;


   if (np == 0 || np == 1)
      return;

   matrix = fmatrix( 0, 0, np-1, np-1 );
   Vx  = (float *) calloc( np, sizeof(float) );
   Vy  = (float *) calloc( np, sizeof(float) );
   VRx = (float *) calloc( np, sizeof(float) );
   VRy = (float *) calloc( np, sizeof(float) );
   t   = (float *) calloc( np, sizeof(float) );
   X   = (float *) calloc( np, sizeof(float) );
   Y   = (float *) calloc( np, sizeof(float) );
   
   XO  = (float *) calloc( 1, sizeof(float) );     
   YO  = (float *) calloc( 1, sizeof(float) );   

   if (matrix == NULL || Vx == NULL || Vy == NULL ||
       VRx == NULL || VRy == NULL || t == NULL || X == NULL || Y == NULL ||
       XO == NULL || YO == NULL )
   {
      anyoutf( 1, "Cannot allocate memory for cubic spline routine!" );
      return;
   }

   /* Scale the positions to numbers between 0 and 1, otherwise you could */
   /* run into problems with calculating splines */
   
   /* Find minimum sized box that encloses the polygon positions */
   /* Initialize */
   xlo = xhi = Xus[0];
   ylo = yhi = Yus[0];
   for (i = 1; i < np; i++)
   {
      float xf = Xus[i], yf = Yus[i];

      if (xf <  xlo)
         xlo =  xf;
      if (xf > xhi)
         xhi = xf;
      if (yf < ylo)
         ylo =  yf;
      if (yf > yhi)
         yhi = yf;
   }  
   
   m = 1.0 / MYMAX( (xhi-xlo),(yhi-ylo) );
   for (i = 0; i < np; i++)
   {
      /* Do not alter the array with unscaled positions */
      X[i] = m*(Xus[i]-xlo);
      Y[i] = m*(Yus[i]-ylo);
   }
   scaledsampdist = sampdist * m;

   t[0] = 0.0;
   for (i = 1; i < np; i++)
   {
      t[i] = sqrt( (X[i]-X[i-1])*(X[i]-X[i-1])+(Y[i]-Y[i-1])*(Y[i]-Y[i-1]) );
   }

   /* Initialize matrix */
   for (row = 0; row < np; row++)
   {
      for (col = 0; col < np; col++)
      {
         matrix[row][col] = 0.0;
      }
   }
   matrix[0][0] = matrix[np-1][np-1] = 1.0;

   for (row = 1; row < np-1; row++)
   {
      col = row - 1;
      matrix[row][col]   = t[row+1];
      matrix[row][col+1] = 2.0*( t[row] + t[row+1] );
      matrix[row][col+2] = t[row];
   }
   /* Now get P' = matrix^-1.VRxy */
   invmat( matrix, np );

   /*--------------------------------------------------*/
   /* Tangent vectors at the end points are set equal  */
   /* to the slope of the connecting lines of the      */
   /* first two and last two points.                   */
   /*--------------------------------------------------*/
   Vx[0] = X[1]-X[0];
   Vy[0] = Y[1]-Y[0];
   /* Is contour closed? */
   if (X[np-1] == X[0] && Y[np-1] ==Y[0])
   {
      Vx[np-1] = X[1] - X[np-2];
      Vy[np-1] = Y[1] - Y[np-2];
      Vx[0] = Vx[np-1];
      Vy[0] = Vy[np-1];      
   }
   else
   {
      Vx[np-1] = X[np-1] - X[np-2];
      Vy[np-1] = Y[np-1] - Y[np-2];
   }

   for (row = 1; row < np -1; row++)
   {
      Vx[row] = (3.0/(t[row]*t[row+1])) *
                (
                ( t[row]*t[row]    *(X[row+1]-X[row]) )
                +
                ( t[row+1]*t[row+1]*(X[row]-X[row-1]) )
                );
      Vy[row] = (3.0/(t[row]*t[row+1])) *
                (
                ( t[row]*t[row]    *(Y[row+1]-Y[row]) )
                +
                ( t[row+1]*t[row+1]*(Y[row]-Y[row-1]) )
                );
   }

   for (row = 0; row < np; row++)
   {
      VRx[row] = 0.0;
      VRy[row] = 0.0;
      for (i = 0; i < np; i++)
      {
         VRx[row] += matrix[row][i]*Vx[i];
         VRy[row] += matrix[row][i]*Vy[i];
      }
   }

   /*--------------------------------------------------*/
   /* For each segment, take a sufficient number of    */
   /* values for tau. If a calculated spline point x,y */
   /* has a cord distance n*sample distance, than      */
   /* store this point as a sample point. This way you */
   /* will not include the spline controls as sample   */
   /* points, but the sample distance is constant.     */
   /* The alternative method includes the controls but */
   /* has not an exact equidistant sampling.           */
   /*--------------------------------------------------*/

   /* First control point is always included in the sample */
   XO[0] = X[0]; YO[0] = Y[0];
   xold = X[0]; yold = Y[0];   
   /* And make space for the next one */
   nO++;
   XO = (float *) realloc( (float *) XO,  (nO+1)*sizeof(float) );
   YO = (float *) realloc( (float *) YO,  (nO+1)*sizeof(float) ); 
   
   /* Number of segments is one less than the number of spline controls */
   for (seg = 0; seg < np-1; seg++)
   {
      float   nt;
      float   nta;
      float   Lseg = sqrt( (X[seg+1]-X[seg])*(X[seg+1]-X[seg]) + 
                           (Y[seg+1]-Y[seg])*(Y[seg+1]-Y[seg]) );
      /*-----------------------------------*/
      /* Divide segment in small parts.    */
      /* The number is a function of the   */
      /* cord length (approximated by the  */
      /* linear distance) and the sampling */
      /* distance. The sampling is refined */
      /* by a constant.                    */
      /*-----------------------------------*/ 
      nta = (float) ( (int) (50.0*Lseg/scaledsampdist) );

      for (nt = 1.0; nt < nta; nt += 1.0)
      {
         float F[4];
         float ta = nt/nta;
         float ta2 = ta * ta;
         float ta3 = ta2 * ta;
         float x, y;
         float D;

         F[0] = 2.0*ta3 - 3.0*ta2 + 1.0;
         F[1] = 1.0 - F[0];
         F[2] = ta*(ta2-2.0*ta+1.0) * t[seg+1];
         F[3] = ta*(ta2-ta) * t[seg+1];

         x = F[0] * X[seg] + F[1]*X[seg+1] + F[2]*VRx[seg] + F[3]*VRx[seg+1];
         y = F[0] * Y[seg] + F[1]*Y[seg+1] + F[2]*VRy[seg] + F[3]*VRy[seg+1];

         D = sqrt( (x-xold)*(x-xold)+(y-yold)*(y-yold) );         
         disttot += D;
         xold = x;
         yold = y;
         /*----------------------------------------*/
         /* Is this calculated position on the     */
         /* spline suitable to be stored as a      */
         /* sample point?                          */
         /*----------------------------------------*/
         if (disttot >= (nO)*scaledsampdist && D < (nO+1)*scaledsampdist)
         {
            XO[nO] = x;
            YO[nO] = y;
            nO++;
            XO = (float *) realloc( (float *) XO,  (nO+1)*sizeof(float) );
            YO = (float *) realloc( (float *) YO,  (nO+1)*sizeof(float) );
         }
      }
   }
            
   /* Rescale total distance to original frame and store */
   *splineleningrids = disttot / m;    
   
   /* Scale box to original coordinates */
   for (i = 0; i < nO; i++)
   {
      XO[i] = xlo + XO[i]/m;
      YO[i] = ylo + YO[i]/m;
#if(0)      
      if (i == 0)
      {
         anyoutf( 1, "Spline sample %d = (%g %g) at  D= 0", i, XO[i] , YO[i] );
      }
      else
      {
         anyoutf( 1, "Spline sample %d = (%g %g) at  D= %g", i, XO[i] , YO[i],
                  sqrt( (XO[i]-XO[i-1])*(XO[i]-XO[i-1])+
                        (YO[i]-YO[i-1])*(YO[i]-YO[i-1]) )  );
      }
#endif      
   }
   
   *nout = nO;
   *XusO = &XO[0];
   *YusO = &YO[0];
  
   free( Y );
   free( X );
   free( t );
   free( VRy );
   free( VRx );
   free( Vy );
   free( Vx );
   freefmatrix( matrix, 0, 0 );
}


