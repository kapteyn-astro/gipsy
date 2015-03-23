/* matrix.c
                           COPYRIGHT (c) 2000
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             matrix.h
#if !defined(_matrix_h_)
#define _matrix_h_
double **dmatrix( int xlo, int ylo, int xhi, int yhi );
void freedmatrix( double **m, int xlo, int ylo );
float **fmatrix( int xlo, int ylo, int xhi, int yhi );
void freefmatrix( float **m, int xlo, int ylo );
int **imatrix( int xlo, int ylo, int xhi, int yhi );
void freeimatrix( int **m, int xlo, int ylo );
#endif
#<                          
*/

/*
#>             matrix.dc2

Document:      MATRIX

Purpose:       Allocate space for a matrix with subscript range
               M[ylo..yhi][xlo..xhi] for floats and doubles
               
Category:      UTILITY

File:          matrix.c

Author:        M. Vogelaar 

Description:   dmatrix       Allocate memory for double array M[y1..yn][x1..xn]
               freedmatrix   Free memory allocated with dmatrix
               fmatrix       Allocate memory for float array M[y1..yn][x1..xn]
               freefmatrix   Free memory allocated with fmatrix
               imatrix       Allocate memory for int array M[y1..yn][x1..xn]
               freeimatrix   Free memory allocated with imatrix
               
Comment:       The routines are NOT callable from FORTRAN.

Updates:       28 Jul,  2000: VOG, Document created. 
#<
*/

/*----------------------------------------------------------------------*/
/*
#>             dmatrix.dc2

Function:      dmatrix

Purpose:       Allocate space for a matrix with subscript range
               M[ylo..yhi][xlo..xhi] for doubles.
               
Category:      UTILITY

File:          matrix.c

Author:        M. Vogelaar 

Use:           double **dmatrix( int xlo, int ylo, int xhi, int yhi );

                        
               dmatrix : Output pointer to array of pointers to array
                         of doubles.
                         e.g. the matrix M[ylo..yhi][xlo..xhi]
               xlo     : First subscript in x
               ylo     : First subscript in y               
               xhi     : Last subscript in x
               yhi     : Last subscript in y
                 
                
Example:       double      **image;
 
               image = dmatrix( blo[0], blo[1], bhi[0], bhi[1] );
               x = 3; y = 0;
               element = M[y][x];
               freedmatrix( blo[0], blo[1] );

Comment:       This routine is NOT callable from FORTRAN.

Notes:         After freeing memory with 'freedmatrix' the pointer to
               the matrix is NOT reset to NULL.
               
Updates:       28 Jul,  2000: VOG, Document created. 
#<
*/
/*----------------------------------------------------------------------*/

/*----------------------------------------------------------------------*/
/*
#>             fmatrix.dc2

Function:      fmatrix

Purpose:       Allocate space for a matrix with subscript range
               M[ylo..yhi][xlo..xhi] for floats.
               
Category:      UTILITY

File:          matrix.c

Author:        M. Vogelaar 

Use:           float **fmatrix( int xlo, int ylo, int xhi, int yhi );

                        
               fmatrix : Output pointer to array of pointers to array
                         of floats.
                         e.g. the matrix M[ylo..yhi][xlo..xhi]
               xlo     : First subscript in x
               ylo     : First subscript in y               
               xhi     : Last subscript in x
               yhi     : Last subscript in y
                 
                
Example:       float      **image;
 
               image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );
               x = 3; y = 0;
               element = M[y][x];
               freefmatrix( blo[0], blo[1] );

Comment:       This routine is NOT callable from FORTRAN.

Notes:         After freeing memory with 'freefmatrix' the pointer to
               the matrix is NOT reset to NULL.
               
Updates:       28 Jul,  2000: VOG, Document created. 
#<
*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
#>             freedmatrix.dc2

Function:      freedmatrix

Purpose:       Free memory previously allocated with 'dmatrix'.
               
Category:      UTILITY

File:          matrix.c

Author:        M. Vogelaar 

Use:           void freedmatrix( int xlo, int ylo );
                        
               xlo     : Input first subscript in x
               ylo     : Input first subscript in y               
                 
                
Example:       double      **image;
 
               image = dmatrix( blo[0], blo[1], bhi[0], bhi[1] );
               x = 3; y = 0;
               element = M[y][x];
               freedmatrix( blo[0], blo[1] );

Comment:       This routine is NOT callable from FORTRAN.

Notes:         After freeing memory with 'freedmatrix' the pointer to
               the matrix is NOT reset to NULL.
               
Updates:       28 Jul,  2000: VOG, Document created. 
#<
*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
#>             freefmatrix.dc2

Function:      freefmatrix

Purpose:       Free memory previously allocated with 'fmatrix'.
               
Category:      UTILITY

File:          matrix.c

Author:        M. Vogelaar 

Use:           void freefmatrix( int xlo, int ylo );
                        
               xlo     : Input first subscript in x
               ylo     : Input first subscript in y               
                 
                
Example:       float      **image;
 
               image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );
               x = 3; y = 0;
               element = M[y][x];
               freefmatrix( blo[0], blo[1] );

Comment:       This routine is NOT callable from FORTRAN.

Notes:         After freeing memory with 'freefmatrix' the pointer to
               the matrix is NOT reset to NULL.
               
Updates:       28 Jul,  2000: VOG, Document created. 
#<
*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
#>             imatrix.dc2

Function:      imatrix

Purpose:       Allocate space for a matrix with subscript range
               M[ylo..yhi][xlo..xhi] for integers.
               
Category:      UTILITY

File:          matrix.c

Author:        M. Vogelaar 

Use:           int **imatrix( int xlo, int ylo, int xhi, int yhi );

                        
               imatrix : Output pointer to array of pointers to array
                         of integers.
                         e.g. the matrix M[ylo..yhi][xlo..xhi]
               xlo     : First subscript in x
               ylo     : First subscript in y               
               xhi     : Last subscript in x
               yhi     : Last subscript in y
                 
                
Example:       int      **image;
 
               image = imatrix( blo[0], blo[1], bhi[0], bhi[1] );
               x = 3; y = 0;
               element = M[y][x];
               freeimatrix( blo[0], blo[1] );

Comment:       This routine is NOT callable from FORTRAN.

Notes:         After freeing memory with 'freedmatrix' the pointer to
               the matrix is NOT reset to NULL.
               
Updates:       28 Jul,  2000: VOG, Document created. 
#<
*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
#>             freeimatrix.dc2

Function:      freeimatrix

Purpose:       Free memory previously allocated with 'imatrix'.
               
Category:      UTILITY

File:          matrix.c

Author:        M. Vogelaar 

Use:           void freeimatrix( int xlo, int ylo );
                        
               xlo     : Input first subscript in x
               ylo     : Input first subscript in y               
                 
                
Example:       int      **image;
 
               image = imatrix( blo[0], blo[1], bhi[0], bhi[1] );
               x = 3; y = 0;
               element = M[y][x];
               freeimatrix( blo[0], blo[1] );

Comment:       This routine is NOT callable from FORTRAN.

Notes:         After freeing memory with 'freeimatrix' the pointer to
               the matrix is NOT reset to NULL.
               
Updates:       28 Jul,  2000: VOG, Document created. 
#<
*/
/*----------------------------------------------------------------------*/

#include    "stdlib.h"
#include    "matrix.h"




double **dmatrix( int xlo,
                  int ylo,
                  int xhi,
                  int yhi )
/*------------------------------------------------------------------*/
/* PURPOSE: Allocate space for a matrix with subscript range        */
/*          M[ylo..yhi][xlo..xhi] for doubles.                      */
/*------------------------------------------------------------------*/
{
   int      rows = yhi - ylo + 1;
   int      cols = xhi - xlo + 1;
   int      i;
   double   **m;


   /* Allocate memory for pointers to rows */
   m = (double **) malloc( rows * sizeof(double*) );
   if (!m)
      return( NULL );
   m -= ylo;                                /* Adjust subscript */

   /* Pointer to first row allocates memory for box */
   m[ylo] = (double *) malloc( rows * cols * sizeof(double) );
   if (!m[ylo])
       return( NULL );
   m[ylo] -= xlo;                           /* Adjust subscript */

   /* Set pointers to rows */
   for (i = ylo+1; i <= yhi; i++)
      m[i] = m[i-1] + cols;

   /* Return pointer to array of pointers to rows */
   return( m );
}



void freedmatrix( double **m,
                  int   xlo,
                  int   ylo )
/*------------------------------------------------------------------*/
/* PURPOSE: Free space allocated by a matrix made with function     */
/*          'dmatrix'.                                              */
/*------------------------------------------------------------------*/
{
   free( m[ylo] + xlo );
   free( m + ylo );
}



float **fmatrix( int xlo,
                 int ylo,
                 int xhi,
                 int yhi )
/*------------------------------------------------------------------*/
/* PURPOSE: Allocate space for a matrix with subscript range        */
/*          M[ylo..yhi][xlo..xhi] for floats.                       */
/*------------------------------------------------------------------*/
{
   int      rows = yhi - ylo + 1;
   int      cols = xhi - xlo + 1;
   int      i;
   float    **m;


   /* Allocate memory for pointers to rows */
   m = (float **) malloc( rows * sizeof(float*) );
   if (!m)
      return( NULL );
   m -= ylo;                                /* Adjust subscript */

   /* Pointer to first row allocates memory for box */
   m[ylo] = (float *) malloc( rows * cols * sizeof(float) );
   if (!m[ylo])
       return( NULL );
   m[ylo] -= xlo;                           /* Adjust subscript */

   /* Set pointers to rows */
   for (i = ylo+1; i <= yhi; i++)
      m[i] = m[i-1] + cols;

   /* Return pointer to array of pointers to rows */
   return( m );
}



void freefmatrix( float **m,
                  int   xlo,
                  int   ylo )
/*------------------------------------------------------------------*/
/* PURPOSE: Free space allocated by a matrix made with function     */
/*          'fmatrix'.                                              */
/*------------------------------------------------------------------*/
{
   free( m[ylo] + xlo );
   free( m + ylo );
}



int **imatrix( int xlo,
               int ylo,
               int xhi,
               int yhi )
/*------------------------------------------------------------------*/
/* PURPOSE: Allocate space for an integers matrix with subscript    */
/*          range M[ylo..yhi][xlo..xhi]                             */
/*------------------------------------------------------------------*/
{
   int      rows = yhi - ylo + 1;
   int      cols = xhi - xlo + 1;
   int      i;
   int      **m;


   /* Allocate memory for pointers to rows */
   m = (int **) malloc( rows * sizeof(int*) );
   if (!m)
      return( NULL );
   m -= ylo;                                /* Adjust subscript */

   /* Pointer to first row allocates memory for box */
   m[ylo] = (int *) malloc( rows * cols * sizeof(int) );
   if (!m[ylo])
       return( NULL );
   m[ylo] -= xlo;                           /* Adjust subscript */

   /* Set pointers to rows */
   for (i = ylo+1; i <= yhi; i++)
      m[i] = m[i-1] + cols;

   /* Return pointer to array of pointers to rows */
   return( m );
}



void freeimatrix( int   **m,
                  int   xlo,
                  int   ylo )
/*------------------------------------------------------------------*/
/* PURPOSE: Free space allocated by a matrix made with function     */
/*          'imatrix'.                                              */
/*------------------------------------------------------------------*/
{
   free( m[ylo] + xlo );
   free( m + ylo );
}

