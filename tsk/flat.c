/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             flat.dc1

Program:       FLAT

Purpose:       Apply a flat field correction to a map by fitting a
               2-D polynomial to the background.

Category:      MODELS, CALCULATION, FITTING, CALCULATION

File:          flat.c

Author:        M. Vogelaar

Keywords:
               
   INSET=      Give set, subsets:
               Maximum number of subsets is 2048.   
               Dimension of the subsets must be 2.               

   BOX=        Area which needs to be flat fielded           [whole map]
   
   OUTSET=     Output Set/Subset(s)  

   ORDER=      Order of polynomial to be fitted to the map           [1]
               highest allowed order is 21


** RANGE=      Give range of levels to include in fit:      [all levels]
               Input consists of two values. The first value may 
               be greater than the second. See the description 
               for the correct use of this keyword.



Description:   This program can be used for example to extract a 
               background by fitting a polynomial to it.
               The program uses a set (INSET=) which usually
               is blotted first. The subsets must be two dimensional.
               The area on which FLAT works is given in BOX=
               It writes the results to OUTSET= in the same box.
               Beware if OUTSET= already exists. It only overwrites
               data in the given box. A polynomial plane will be fitted 
               to the data using the least squares method. The order
               of this polynomial is given in ORDER= This number cannot
               exceed 21.
               Values outside the range defined in RANGE= are treated 
               as blanks, i.e. they are not included in the fit and are 
               interpolated after a plane is fitted to the data.
               Examples for the use of the RANGE= keyword:

               RANGE=2, 5
               (IF 2<value<5 THEN include in fit ELSE value==>blank)

               RANGE=5, 2
               (IF 2<value<5 THEN value==>blank ELSE include in fit)
                             
               At the RANGE= keyword, the values -INF and INF can 
               be input also. These values represent the minimum
               and maximum values of the current system.
               
               RANGE=5, INF
               (IF value>5 THEN include ELSE value==>blank)
               
               The fitted background can be subtracted from the 
               original set with the program COMBIN.

               Polynomial:
               In general 2-dim polynomial of order N: 
               f(x,y) = SIG SIG Amn x^m y^n where n+m <= N
               
Example:       Special case, order = 2:
               Replace Amn by ai, where i is the column position of a in 
               the coefficient matrix. Then the polynomial can be written as:
               f(x,y) = z = a0 + a1.x + a2.y + a3.x^2 + a4.y^2 + a5.x.y
               To minimize Chi-squared we calculate the derivatives to the 
               parameters ai of the expression SIG[ (zfit-f(x,y))^2 ] which
               for this special case results in the coefficient matrix:
               
               SIG(z)     = a0.SIG(1) + a1.SIG(x) + a2.SIG(y) + a3.SIG(x^2) + a4.SIG(y^2) + a5.SIG(x.y)
               SIG(z.x)   = a0.SIG(1.x) + a1.SIG(x.x) + a2.SIG(y.x) + a3.SIG(x^2.x) + a4.SIG(y^2.x) + a5.SIG(x.y.x) 
               SIG(z.y)   = a0.SIG(1.y) + a1.SIG(x.y) + a2.SIG(y.y) + a3.SIG(x^2.y) + a4.SIG(y^2.y) + a5.SIG(x.y.y)
               SIG(z.x^2) = a0.SIG(1.x^2) + ....
               SIG(z.y^2) = a0.SIG(1.y^2) + ....
               SIG(z.x.y) = a0.SIG(1.x.y) + ....
               
               If we call the left side vector B and the coefficient matrix A
               then:
               B = A.(a0,a1,a2,a3,a4,a5)T
               
               The unknown vector (a0,a1,a2,a3,a4,a5) is calculated by 
               multiplying the inverse of A with B.
               
               The algorithm is splitted into two parts. First we fill a
               matrix for an arbitrary order N matrix. Secondly we use
               a Gauss-Jordan algorithm to calculate the inverse. In special
               cases one expects better results by applying the 
               Singular Value Decomposition method but until now we are 
               satisfied with the 'Gauss-Jordan' fit results for the data 
               we processed.
               
               The calculations are in double precision. We noticed that the
               conversion of floats in an image with high values resulted 
               in a small increase of minimum chi-squared, but only by a very 
               small amount, too small to be significant.
              
               For the data points listed below the results are:
               From Mathematica:
               2.78061e+12 - 6.73389e+9 x - 1.60955e+10 x^2 - 1.03662e+9 y 
               - 2.83247e+8 xy - 9.30063e9 y^2
               
               flat.c:
               2.78061e+12 - 6.73389e+09 X**1 Y**0 - 1.03662e+09 X**0 Y**1
               - 1.60955e+10 X**2 Y**0 - 9.30063e+09 X**0 Y**2 
               - 2.83247e+08 X**1 Y**1
               chi2=5.90453e+27
              
               Fit from article (without reference, just as an example):
               # Ax^2 + By^2 + Cxy + Dx + Ey + F
               #
               # A = -16.1e9
               # B = -9.3e9
               # C = -0.28e9
               # D = 6.11e5
               # E = 0.32e5
               # F = 0.47e2
               chi2 = 1.07231e+28
X =     5.0,
        397.0,
        1995.0,
        1994.0,
        10.0,
        1177.0,
        600.0,
        1420.0,
        1141.0,
        990.0,
        574.0,
        1740.0,
        1456.0,
        864.0,
        1604.0,
        1332.0,
        554.0,
        1643.0,
        442.0,
        1266.0
             
Y =     3996.0,
   	3736.0,
   	3994.0,
   	7.0,
   	59.0,
   	16.0,
   	1610.0,
   	2070.0,
   	1160.0,
   	3085.0,
   	2389.0,
   	1586.0,
   	3278.0,
   	3846.0,
   	3030.0,
   	3658.0,
   	3198.0,
   	900.0,
   	1937.0,
   	1661.0

Z =    	-1.485E+17,
   	-1.328E+17,
   	-2.147E+17,
   	-6.402E+16,
   	-3.415E+13,
   	-2.231E+16,
   	-3.017E+16,
   	-7.314E+16,
   	-3.385E+16,
   	-1.052E+17,
   	-5.877E+16,
   	-7.291E+16,
   	-1.354E+17,
   	-1.505E+17,
   	-1.282E+17,
   	-1.544E+17,
   	-1.006E+17,
   	-5.141E+16,
   	-3.828E+16,
   	-5.205E+16


Notes:         

Updates:       Oct 14,  1991: VOG, Document created.
               Mar 03,  2004: VOG, Added to documentation, changed calculations
                                   to double precision and tested the 
                                   algorithm for a test series of 20 data 
                                   points and a second order polynomial for 
                                   which fitted parameters where known from the
                                   literature. FLAT produced better results
                                   because it found a smaller value for 
                                   chi-squared compared to the value from
                                   the literature. 


#<
*/


#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "math.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"
#include "float.h"
#include "gdsinp.h"
#include "gdsasn.h"
#include "gdsout.h"
#include "setfblank.h"
#include "myname.h"
#include "anyout.h"
#include "nelc.h"
#include "presetd.h"
#include "gdsbox.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsc_fill.h"
#include "gdsc_name.h"
#include "gdsi_read.h"
#include "gdsi_write.h"
#include "gdsc_ndims.h"
#include "userchar.h"
#include "userreal.h"
#include "userint.h"
#include "error.h"
#include "reject.h"
#include "getrange.h"
#include "minmax3.h"
#include "wminmax.h"
#include "status.h"


#define AXESMAX    10               /* Max. allowed number of axes in a set */
#define SUBSMAX    2048             /* Max. number of substructures to be specified */
#define MAXELEM    256              /* Max. # coefficients in polynomial */
#define BIGSTORE   80               /* Length of a string */
#define VERSION    "1.0"            /* Version number of this program */
#define NONE       0                /* Default values for use in userxxx routines */
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define false      0
#define true       1             
#define PI         3.141592653589793
#define	MAXPAR     32		    /* number of free parameters */

/* Keywords and messages */

#define KEY_INSET         tofchar("INSET=")
#define MES_INSET         tofchar("Give set (, subsets): ")
#define KEY_BOX           tofchar("BOX=")
#define MES_BOX           tofchar("Area which needs to be flat fielded:   [whole map]")
#define KEY_OUTSET        tofchar("OUTSET=")
#define MES_OUTSET        tofchar("Output set:")
#define KEY_ORDER         tofchar("ORDER=")
#define MES_ORDER         tofchar("Order of polynomial to be fitted:  [1]")
#define KEY_RANGE         tofchar("RANGE=")
#define MES_RANGE         tofchar("Range of levels to include in fit:  [all levels]")


/* Initialize string with macro */
#define fmake(fchr,size) { \
                            static char buff[size+1]; \
                            int i; \
                            for (i = 0; i < size; buff[i++] = ' '); \
                            buff[i] = 0; \
                            fchr.a = buff; \
                            fchr.l = size; \
                         }
                         
                        
/* Malloc version of 'fmake'  */
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }
                                                        
                                                                               
#define MYMAX(a,b) ((a) > (b) ? (a) : (b))                          
#define MYMIN(a,b) ((a) > (b) ? (b) : (a))
#define NINT(a) ( (a)<0 ? (int)((a)-.5) : (int)((a)+.5) )


/* Input of set, subsets: */  

static fchar    Setin;                 /* Name of the set */
static fint     Subin[SUBSMAX];        /* Array for the subset coordinate words */
static fint     Nsubsin;               /* Number of input subsets */
static fint     Dfault;                /* Default option for input etc */
static fint     Axnum[AXESMAX];        /* GDSINP axis numbers array */
static fint     Axcount[AXESMAX];      /* GDSINP axis lengths array */
static fint     Class = 1;             /* Axis is operation axis */
static fint     Setdim;                /* Dimension of the set */
static fint     Subdim;                /* Dimension of the subset */
static fint     Scrnum = 8;            /* Destination of log output */
static fint     Maxaxes  = AXESMAX;    /* Convert parameters to variables */
static fint     Maxsubs  = SUBSMAX;    /* Max. input subsets */
static int      subnrI;                /* Index of current input subset */
static fint     Setlevel = 0;          /* Indicate set level */


/* Output set related */

static fchar    Setout;                /* Name of the set */
static fint     Subout[SUBSMAX];       /* Array for the subset coordinate words */
static fint     Nsubsout;              /* Number of input subsets */
static fint     Axnumout[AXESMAX];     /* GDSOUT axis numbers array */
static fint     Axcountout[AXESMAX];   /* GDSOUT axis lengths array */
   

/* Input of area etc.:*/

static fint     CwloI;                 /* Coordinate words */
static fint     CwhiI;
static fint     CwloO;
static fint     CwhiO;
static fint     GridloI[AXESMAX];      /* Coordinates for input-frame */
static fint     GridhiI[AXESMAX];
static fint     BgridloI[AXESMAX];     /* Coordinates for input-box */
static fint     BgridhiI[AXESMAX];
static fint     Boxopt;                /* Default for gdsbox */


/* Data transfer: */

static fint     Totpixels;             /* Total # pixels in one subset */
static fint     TidI;                  /* Tranfer id for input */
static fint     TidO;                  /* Transfer id for output */
static float    *ImageI = NULL;        /* Multiple buffer for all subsets */   
static fint     Buflen;                /* Max length of read buffer */
static int      Bufheight;             /* To determine max. buffer size */
static fint     LenX, LenY;            /* Size of user given box */
static fint     Ylo, Yhi;              /* Lines in read buffer */
static fint     Readbuf, Afterread;    /* Read buffer administration */
static fint     Writepixels;           /* Num. of pixels to write */


/* Calculations etc.; */

static fint     Arraylen;              /* Length used in presetting arrays */
static fint     Order;                 /* User given order of polynomial */
static fint     Coefficients;          /* # coefficients in polynomial */
static double   matrix[MAXELEM][MAXELEM];    /* least squares matrix */
static double   B[MAXELEM];            /* Data vector */
static double   SOL[MAXELEM];          /* The coefficients */
static int      Xpos, Ypos;            /* Two dimensional positions */
static int      Xoffset, Yoffset;      /* Used to determine 2-dim positions */


/* Related to update of header etc: */

static float    Datamin[SUBSMAX];      /* Min, max of new set */
static float    Datamax[SUBSMAX];
static fint     Nblanks[SUBSMAX];      /* Number of blanks in new set */
static fint     Remove;                /* 0 means that minimum and maximum   */
                                       /* have not changed, anything else    */
                                       /* means that minimum and maximum     */
                                       /* have changed and that the MINMAX   */
                                       /* descriptors at intersecting levels */
                                       /* will be removed by WMINMAX.        */


/* Miscellaneous: */

static fint     Numitems;             /* Max. number of input items in userxxx routines */
static fint     R1, R2;               /* Results of userxxx routines */
static float    Blank;                /* Value of system blank */
static fint     Mcount;               /* Counter for 'minmax3' routine */
static int      agreed;               /* Loop control */
static float    Range[2];             /* User given data in/exclude range */




static void anyoutC( char *anystr )

/*-------------------------------------------------------------------*/
/* The C version of 'anyout' needs a C type string as argument only. */
/* The value of 'Scrnum' is global.                                  */
/*-------------------------------------------------------------------*/
{
   anyout_c( &Scrnum, tofchar( anystr ) );
}



static void polfill( int Xpos, int Ypos, double *B, fint Order )
/*---------------------------------------------------------------*/
/* Xpos, Ypos are the pixel positions, B is an array with        */
/* solution parameters and Order is the order of the polynomial. */
/*---------------------------------------------------------------*/
{
   int       index;                               /* counter in polynomial vector */
   int       i, j, k;
  
   index = 0;
   for (i = 0; i <= (int) Order; i++) {           /* Loop over order */
      for (j = 0; j <= i; j++) {                  /* loop over terms of that order */
         B[index] = 1.0;
         for (k = 1; k <= (i-j); k++) {           /* loop over X-powers */
            B[index] *= (double) Xpos;
         }
         for (k = 1; k <= j; k++) {               /* loop over Y-powers */
            B[index] *= (double) Ypos;
         }
         index++;
      }
   }
}
      


static void fillup( int Xpos, int Ypos, double Value, fint Coefficients )
/*-------------------------------------------------------------*/
/* Fill matrix used for lsq fit                                */
/*-------------------------------------------------------------*/
{
   int      i, j;
   double   C[MAXELEM];
   
   
   polfill( Xpos, Ypos, C, Order );
   
   /* make from the row an array */

   for(i = 0; i < Coefficients; i++) {            /* loop over rows */
      for (j = 0; j < Coefficients; j++) {        /* loop over collums */
         matrix[ j ][ i ] +=  C[ j ] * C[ i ];
      }
   }

   /* Generate  data vector */

   for (i = 0; i <  Coefficients; i++) {          /* loop over rows */
      B[ i ] += Value * C[ i ];
   }
}


static fint invmat( int nfree )
/*-------------------------------------------------------------------------*/
/* invmat calculates the inverse of matrix. The algorithm used is the      */
/* Gauss-Jordan algorithm described in Stoer, Numerische matematik, 1 Teil.*/
/* The routine returns -6 if the determinant was zero. If successful, 0 is */
/* returned. !!!matrix is global!!!                                        */
/*-------------------------------------------------------------------------*/
{
   double even;
   double hv[MAXPAR];
   double mjk;
   double rowmax;
   fint   evin;
   fint   i;
   fint   j;
   fint   k;
   fint   per[MAXPAR];
   fint   row;

   for (i = 0; i < nfree; i++) per[i] = i;	/* set permutation array */
   for (j = 0; j < nfree; j++) {		/* in j-th column, ... */
      rowmax = fabs( matrix[j][j] );		/* determine row with ... */
      row = j;					/* largest element. */
      for (i = j + 1; i < nfree; i++) {
         if (fabs( matrix[i][j] ) > rowmax) {
            rowmax = fabs( matrix[i][j] );
            row = i;
         }
      }
      if (matrix[row][j] == 0.0) return( -6 );	/* determinant is zero! */
      if (row > j) {				/* if largest element not ... */
         for (k = 0; k < nfree; k++) {		/* on diagonal, then ... */
            even = matrix[j][k];		/* permutate rows. */
            matrix[j][k] = matrix[row][k];
            matrix[row][k] = even;
         }
         evin = per[j];				/* keep track of permutation */
         per[j] = per[row];
         per[row] = evin;
      }
      even = 1.0 / matrix[j][j];		/* modify column */
      for (i = 0; i < nfree; i++) matrix[i][j] *= even;
      matrix[j][j] = even;
      for (k = 0; k < j; k++) {
         mjk = matrix[j][k];
         for (i = 0; i < j; i++) matrix[i][k] -= matrix[i][j] * mjk;
         for (i = j + 1; i < nfree; i++) matrix[i][k] -= matrix[i][j] * mjk;
         matrix[j][k] = -even * mjk;
      }
      for (k = j + 1; k < nfree; k++) {
         mjk = matrix[j][k];
         for (i = 0; i < j; i++) matrix[i][k] -= matrix[i][j] * mjk;
         for (i = j + 1; i < nfree; i++) matrix[i][k] -= matrix[i][j] * mjk;
         matrix[j][k] = -even * mjk;
      }
   }
   for (i = 0; i < nfree; i++) {		/* finally, repermute the ... */
      for (k = 0; k < nfree; k++) {		/* columns. */
         hv[per[k]] = matrix[i][k];
      }
      for (k = 0; k < nfree; k++) {
         matrix[i][k] = hv[k];
      }
   }
   return( 0 );					/* all is well */
}



static void matver( double *X, double *B, fint N )
/*----------------------------------------------------------------*/
/* This subroutine multiplies an array with a vector of length N  */
/* The data array is given by matrix. The data vector by X and    */
/* the result vector by B. matrix is global!                      */
/*----------------------------------------------------------------*/
{
   int     i, j;
   
   for (i = 0; i < (int) N; i++) {
      B[i] = 0.0;    /* Reset result */
      for (j = 0; j < (int) N; j++) {             /* Loop over columns */
         B[i] += matrix[i][j] * X[j];
      }
   }
}


static void writeparms( double *SOL, fint Order )
/*---------------------------------------------*/
/* Output parameters to screen and Log file.   */
/*---------------------------------------------*/
{
   int    index;
   char   longbuf[1024];
   int    len1, len2;
   char   messbuf[256];
   int    i,j;
   
  
   index = 1;
   anyoutC("Result of polynomial plane fit");
   anyoutC("------------------------------");
   sprintf( messbuf, "Pol = %g", SOL[0] );
   strcpy( longbuf, messbuf );

   for (i = 1; i <= (int) Order; i++) {          /* loop over orders from 1 */
      for (j = 0; j <= i; j++) {                 /* loop over terms of that order */
         if (SOL[index] < 0.0) {
            sprintf( messbuf, 
                     " - %g X**%d Y**%d", 
                     fabs(SOL[index]), 
                     i-j, j ); 
         }
         else {            
            sprintf( messbuf, 
                     " + %g X**%d Y**%d", 
                     SOL[index], 
                     i-j, j ); 
         }
         len2 = strlen( messbuf );
         len1 = strlen( longbuf );
         if ( (len2+len1) <= 80 ) {
            strcat( longbuf, messbuf );
         }
         else {            
            anyoutC( longbuf );
            strcpy( longbuf, messbuf );
         }         
         index++;
      }
   }
   anyoutC( longbuf );
   anyoutC("------------------------------");   
}



static double polval( int Xpos, int Ypos, double *SOL, 
                      fint Coefficients, fint Order )
/*-------------------------------------------------------------------*/
/* This function calculates the value of a two dimensional           */
/* polynomial for position (X,Y). The order of the polynomial        */
/* is given by Order.                                                */
/*-------------------------------------------------------------------*/
{
   double      B[MAXELEM];
   double      pol;
   int         i;
   
   
   polfill( Xpos, Ypos, B, Order );
   pol = 0.0;
   for (i = 0; i < Coefficients; i++) {
      pol += SOL[i] * B[i];
   }
   return pol;
}


static int inrange( float *value, float *Range )
/*----------------------------------------------------------*/
/* Check whether value of pixel is within user given range. */
/*----------------------------------------------------------*/
{
   if (Range[0] < Range[1]) {
     if (*value >= Range[0] && *value <= Range[1]) return true;
     else return false;
   }
   else {
     if (*value < Range[1] || *value > Range[0]) return true;
     else return false;
   }
}
                                       


MAIN_PROGRAM_ENTRY
/*--------------------------------------------------------------------------*/
/* Because Fortran passes all arguments by reference, all C functions with  */
/* a Fortran equivalent must do this also (GIPSY programmers guide,         */
/* Chapter 9). Variables that can be interchanged between Fortran and C     */
/* all start with a capital.                                                */
/*--------------------------------------------------------------------------*/
{
   int     i,m;
   
	
   init_c();                                      /* contact Hermes */
   /* Task identification */
   {        
      static fchar    Task;                       /* Name of current task */
      fmake( Task, 20 );                          /* Macro 'fmake' must be available */
      myname_c( Task );                           /* Get task name */
      Task.a[nelc_c(Task)] = '\0';                /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, VERSION );          /* Show task and version */      
   }
   

   setfblank_c( &Blank );      
   fmake(Setin, 80);   
   Dfault  = NONE;
   Scrnum  = 8;                                   /* Output device, 8 is experienced mode */
   Subdim  = 2;                                   /* Dimension of subsets must be 2 */
   Class   = 1; 
   Nsubsin = gdsinp_c( Setin, 
                       Subin, 
                       &Maxsubs, 
                       &Dfault, 
                       KEY_INSET,
                       MES_INSET, 
                       &Scrnum, 
                       Axnum, 
                       Axcount, 
                       &Maxaxes, 
                       &Class, 
                       &Subdim );

   Setdim = gdsc_ndims_c( Setin, &Setlevel );

  /*   
   *----------------------------------------------------------------------------
   * Determine the edges of this its frame (GridloI/hiI)
   *----------------------------------------------------------------------------
   */
   R1 = 0;
   (void) gdsc_range_c( Setin, 
                        &Setlevel, 
                        &CwloI, 
                        &CwhiI, 
                        &R1 );
   
   for (m = 0; m < (int) Setdim; m++) {
      R1 = R2 = 0;
      GridloI[m] = gdsc_grid_c( Setin, &Axnum[m], &CwloI, &R1 );
      GridhiI[m] = gdsc_grid_c( Setin, &Axnum[m], &CwhiI, &R2 );
   }


  /*--------------------------------------------------------*/
  /* Prepare a box for INSET. Default is the entire subset. */
  /*--------------------------------------------------------*/
   
   Dfault = REQUEST;
   Boxopt = 0;
   Scrnum = 8;
   (void) gdsbox_c( BgridloI, 
                    BgridhiI, 
                    Setin, 
                    Subin,
                    &Dfault, 
                    KEY_BOX, 
                    MES_BOX, 
                    &Scrnum, 
                    &Boxopt );
   /* Count number of pixels in this substructure */
   Totpixels = 1;
   /* For one subset */   
   for(m = 0; m < (int) Subdim; m++) Totpixels *= Axcount[m];
  
 
   /*---------------------------------------------------------*/
   /* Assign input characteristics to output and ask name for */
   /* output.                                                 */
   /*---------------------------------------------------------*/
   
   (void) gdsasn_c( KEY_INSET, 
                    KEY_OUTSET, 
                    &Class );
   fmake(Setout, BIGSTORE);
   Dfault = NONE;
   do {
      Nsubsout = gdsout_c( Setout, 
                           Subout, 
                           &Nsubsin, 
                           &Dfault, 
                           KEY_OUTSET, 
                           MES_OUTSET,
                           &Scrnum, 
                           Axnumout, 
                           Axcountout, 
                           &Maxaxes );
      agreed = (Nsubsout == Nsubsin);
      if (!agreed) {
         (void) reject_c( KEY_OUTSET, tofchar("Subset(s) error") );
      }
   } while (!agreed);
   
   
   /*------------------------------------*/
   /* Ask order of polynomial to be used */
   /*------------------------------------*/
   
   Order    = 1;
   Dfault   = REQUEST;
   Numitems = 1;
   do {
      R1 = userint_c( &Order, 
                      &Numitems, 
                      &Dfault, 
                      KEY_ORDER, 
                      MES_ORDER );
      agreed = ((Order > 0) && (Order < 21));
      if (!agreed) (void) reject_c( KEY_ORDER, tofchar("Wrong order!") );
   } while (!agreed);
   
  
   /*------------------------------------------------------------------*/
   /* Determine range of levels to include in the fit. The default     */
   /* values are the max. and min. floats of the system, so that all   */
   /* levels are included if the user pressed carriage return.         */
   /*------------------------------------------------------------------*/

   Dfault   = HIDDEN;
   Range[0] = -1.0*FLT_MAX;                       /* Defined in float.h */
   Range[1] = FLT_MAX;
        
   getrange_c( Range, &Dfault, KEY_RANGE, MES_RANGE );


   /*----------------------------------------------*/
   /* Determine number of coefficients of function */
   /*----------------------------------------------*/
   

   Coefficients = 0;                              /* reset element counter */
   for ( i = 0; i <= (int) Order; i++) {          /* loop over all orders */
      Coefficients += (i + 1);                    /* increase the # of elements by the order */
   }



   /*------------------------------------------------------------*/
   /* Try to find the maximum buffer size for the current system */
   /* The total number of elements is given in 'Buflen' and the  */
   /* maximum number of lines is given in 'Bufheight'            */
   /*------------------------------------------------------------*/

   LenX = (BgridhiI[0] - BgridloI[0] + 1); 
   LenY = (BgridhiI[1] - BgridloI[1] + 1); 
   Bufheight = LenY;
   Buflen = LenX * Bufheight;
   do {
      ImageI = (float *) calloc( (int) Buflen, sizeof(float) );
      if (ImageI == NULL) {
         agreed = false;
         status_c(tofchar("Decreasing buffer size"));
         Bufheight /= 2;
         Buflen = LenX * Bufheight;         
      }
      else agreed = true;
   } while( !agreed || (Bufheight < 1) );
   if (Bufheight < 1) {
      static fint    Errlev = 4;                  /* Generate fatal error */
      anyoutC("Cannot allocate space for at least 1 line of input!");
      error_c( &Errlev, 
               tofchar("...Memory Allocation problems!...") );
   }


   for (subnrI = 0; subnrI < Nsubsin; subnrI++) {
            
      /* Preset used arrays to zero */
      double  Zero = 0.0;

      Arraylen = MAXELEM;
      presetd_c( &Zero, B,   &Arraylen ); 
      presetd_c( &Zero, SOL, &Arraylen ); 
      Arraylen *= Arraylen;
      presetd_c( &Zero, &matrix[0][0], &Arraylen ); 
      
      CwloI = gdsc_fill_c( Setin, &Subin[subnrI], BgridloI );
      CwhiI = gdsc_fill_c( Setin, &Subin[subnrI], BgridhiI );          
      TidI  = 0;
      Xoffset = Yoffset = 0;

      /*---------------------------------------------------------------*/
      /* Create buffers with maximum size. If this size cannot contain */
      /* The complete box, split it into smaller parts. Take as much   */
      /* parts as possible with size Bufheight*length and take care of */      
      /* leftovers.                                                    */
      /*---------------------------------------------------------------*/      
      
      Ylo = BgridloI[1];
      Yhi = BgridloI[1] + Bufheight - 1;            


      while ( (Yhi-Ylo) > 0 ) {
         /* Read data in buffer of max size 'Bufheight' * length of box */
         Readbuf = (Yhi - Ylo + 1) * LenX;
         (void) gdsi_read_c( Setin, 
                             &CwloI, 
                             &CwhiI, 
                             ImageI,
                             &Readbuf, 
                             &Afterread,
                             &TidI );
         for (i = 0; i < (int) Afterread; i++) {
            if (Xoffset == LenX) {
               Yoffset++;                         /* Increase Y position */
               Xoffset = 0;                       /* Reset Xoffset */
            }
            Xpos = (int) BgridloI[0] + Xoffset;
            Ypos = (int) BgridloI[1] + Yoffset;
            Xoffset++;
            if (ImageI[i] != Blank) {
               if (inrange( &ImageI[i], Range )) {
                  double X = (double) Xpos;
                  double Y = (double) Ypos;
                  double Z = (double) ImageI[i];                              
                  fillup( X, Y, Z, Coefficients );
               }
            }
         }
            
         Ylo += Bufheight;
         Yhi += Bufheight;         
         Yhi  = MYMIN( BgridhiI[1], Yhi );        /* Yhi never exceeds Ymax in box */
      }
      
     
      /*-----------------------------------------------------*/
      /* We have:  A * SOL = B  ==> SOL = INV( A ) * B       */
      /* First invert matrix A and than multiply by vector B */
      /*-----------------------------------------------------*/
            
      R1 = invmat( Coefficients );               /* Matrix is global */
      if (R1 != 0) {
         fint    Errlev = 4;                     /* Generate fatal error */
         anyoutC("Cannot invert matrix, probably determinant = 0!");
         error_c( &Errlev, tofchar("...Cannot invert matrix!...") );
      }
      (void)  matver( B, SOL, Coefficients );    /* Matrix is global */
      (void)  writeparms( SOL, Order );
    
      
      /*------------------------------------------------------------*/
      /* It is possible to put an arbitrary subset in the data set  */
      /* into just one output subset. The coordinate words will not */
      /* correspond in this case, therefore we have to calculate    */
      /* the coordinate words of the output set separately.         */
      /*------------------------------------------------------------*/

      CwloO  = gdsc_fill_c( Setout, &Subout[subnrI], BgridloI );
      CwhiO  = gdsc_fill_c( Setout, &Subout[subnrI], BgridhiI );          
      TidO   = 0;
      Mcount = 0;
      Xoffset = Yoffset = 0;
            
   
      m = 0;
      for (i = 0; i < (int) Totpixels; i++) {
         if (Xoffset == LenX) {
            Yoffset++;                            /* Increase Y position */
            Xoffset = 0;                          /* Reset Xoffset */
         }
         Xpos = (int) BgridloI[0] + Xoffset;
         Ypos = (int) BgridloI[1] + Yoffset;
         Xoffset++;         
         
         /* Use the same buffer as for the input */
         ImageI[m] = polval( Xpos, Ypos, SOL, Coefficients, Order );
         if (  ((m+1) == Buflen) || ((i+1) == Totpixels)  ) {
            /* Write to output if m is equal to Buflen or if this */
            /* was the last pixel */
            Writepixels = (fint) m + 1;
            
            (void) minmax3_c( ImageI, 
                              &Writepixels, 
                              &Datamin[subnrI], 
                              &Datamax[subnrI], 
                              &Nblanks[subnrI], 
                              &Mcount );
            
            (void) gdsi_write_c( Setout, 
                                 &CwloO, &CwhiO, 
                                 ImageI,
                                 &Writepixels, 
                                 &Writepixels, 
                                 &TidO );                            
            m = 0;                                /* Reset buflen. counter */
         }
         else {
            m++;
         }
      }
      /* Give proceedings message */
      { 
         char messbuf[512];
         sprintf( messbuf, "%d %% completed", 100*(subnrI+1)/Nsubsin );
         status_c( tofchar(messbuf) );
      }
   } /* End of all subsets */
  
  
   /* Remove in 'WMINMAX' old minmax descriptors because program */
   /* changes values! */
   Remove = true;   
   (void) wminmax_c( Setout, 
                     Subout, 
                     Datamin, 
                     Datamax, 
                     Nblanks,
                     &Nsubsout, 
                     &Remove );
   
   finis_c();                                      /* Quit Hermes */
   return( EXIT_SUCCESS );   
}   

