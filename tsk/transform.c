/*
                            COPYRIGHT (c) 1996
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             transform.dc1

Program:       TRANSFORM

Purpose:       Transform a (sub)set using rotation, scaling, translation etc.

Category:      MANIPULATION, MATH, UTILITY

File:          transform.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give input set (,subsets) to transform:
   
               Maximum number of subsets is 2048.
               This set will be transformed to OUTSET=, using a 
               concatenation of simple operations like rotation, 
               scaling, translation etc.


   BOX=        Give box in .....                        [entire subset]
   
               Restrict operations to data within these limits.


   OUTSET=     Give output set (, subsets):
   
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets. 


   POS=        Enter position of centre:                          [0,0]
   
               All operations are defined with respect to a central
               point. This point must be defined here. It can
               be entered in any coordinates (grids/physical). If, for
               example, you want to rotate an object about its centre,
               then determine the position of this centre (e.g. -6.2 8.5)
               and enter these numbers here (e.g. POS= -6.2 8.5).


   OPERATION=  Enter sequence of operations:            [2 (=rotation)]
   
               Identify operations by their numbers and enter one or 
               more numbers. Numbers must be separated by spaces.
               The operations that define the transformation are started
               in the order as entered by the user. The maximum number
               of operations is 31. 
               
               The operation menu as it will appear in the log-file:
               
               Operations: (With respect to POS=)
               ===================================
               1. Translation
               2. Rotation (counter clockwise)
               3. Scaling
               4. Reflection about x-axis
               5. Reflection about y-axis
               6. Reflection wrt. centre
               7. X shear
               8. Y shear

               Example: OPERATION=1 2 1  
               (=translation followed by rotation followed by translation)
               
              
              
               The following keywords are asked as many times as
               the corresponding operation is defined in OPERATION=
               If the same keyword is prompted more than once, an
               index number is inserted in the message to keep track
               of the operation that is involved. 
               =======================================================
               
              
   TRANSLXY=   Enter translation in x and y:                      [0,0]
   
               Enter two numbers for a translation. The numbers need 
               not to be integers. TRANSLXY=a b means: translate
               the image a grids in x and b grids in y.
   
  
   ANGLE=      Enter rotation angle (deg):                          [0]
   
               Enter an angle in degrees. This will rotate the image
               with respect to the grids given in POS= or the position
               defined with a previous translation.
               The rotation is counter-clockwise.
   
  
   SCALEXY=    Enter scaling in x and y:                          [1,1]
   
               Enter two numbers for a scaling. The numbers can be 
               non-integer. A scaling is always with respect to 
               the position in POS= or the position defined with
               a previous translation.
   
  
   XSHEAR=     Enter shear factor in X:                             [0]
                
               Scale an image in X only with a factor that can be a 
               non-integer number. The shear is always with respect to
               the position in POS= or the position defined with
               a previous translation.
               
  
   YSHEAR=     Enter shear factor in Y:                             [0]

               See description at XSHEAR=
               



Description:   Sometimes it is handy to manipulate (test) data without
               using and changing a physical coordinate system.
               This program transforms data using simple linear 
               transformations. If we represent these transformations
               by 3x3 matrices, then:
               
                                | 1   0   0 |
               1) Translation:  | 0   1   0 |
                                | Tx  Ty  1 |
                              
                                                | cos(t)  sin(t) 0 |
               2) Rotation (counter clockwise): |-sin(t)  cos(t) 0 |
                                                | 0       0      1 |
                           
                            | Sx  0  0 |                     
               3) Scaling:  | 0   Sy 0 |
                            | 0   0  1 |
                            
                                       | 1   0  0 |
               4) reflection (x-axis): | 0  -1  0 |
                                       | 0   0  1 |
                                       
                                       |-1  0  0 |
               5) reflection (y-axis): | 0  1  0 |
                                       | 0  0  1 |
               
                                       |-1  0  0 |
               6) reflection (POS=):   | 0 -1  0 |
                                       | 0  0  1 |
               
                             | 1  0  0 |
               7) X shear:   | S  1  0 |
                             | 0  0  1 |

                             | 1  S  0 |
               8) Y shear:   | 0  1  0 |
                             | 0  0  1 |


               Operations are concatenated by multiplying matrices.
               A sequence of operations is automatically prepended
               by a translation of POS= to 0,0 and appended by a
               translation of 0,0 to POS=
               
               To get a regular sampling of output positions, this 
               program has a loop that calculates for each position in 
               the output, a corresponding position in the input using 
               the inverse of the net-transformation matrix.
               This position is usually a non-integer position and
               we can use an interpolation in values of neighbouring 
               pixels to get an image value for the output image.
               Blanks are recognized, but the interpolation will
               not increase the number of blanks.

               
                          
Notes:         If the program transforms data in a way that not all 
               positions have a counterpart within BOX= then these
               positions get a blank as image value in the output set.

Example:       .......

Updates:       Aug 01, 1996: VOG, Document created.
               Sep 24, 1996: KGB, CPOS changed into POS.
               Feb  1, 2000: JPT, Increased number of subsets.
               Jul 27, 2000: VOG, Changed local 'getipval' to 
                                  library  interpol.
                                  

#<
*/

/*  transform.c: include files     */

#include    "stdio.h"        /* Defines ANSI C input and output utilities */
#include    "stdlib.h"       /* Defines the ANSI C functions for number */
                             /* conversion, storage allocation, and similar tasks.*/
#include    "string.h"       /* Declares the ANSI C string functions*/
                             /* like:strcpy, strcat etc.*/
#include    "math.h"         /* Declares the mathematical functions and macros.*/
#include    "cmain.h"        /* Defines the main body of a C program with */
                             /* MAIN_PROGRAM_ENTRY and IDENTIFICATION */
#include    "gipsyc.h"       /* Defines the ANSI-F77 types for Fortran to C intface */
                             /* including def. of char2str,str2char,tofchar,zadd */
                             /* and macros tobool and toflog */
#include    "float.h"        /* Definition of FLT_MAX etc.*/
#include    "ctype.h"        /* Declares ANSI C functions for testing characters */
                             /* like: isalpha, isdigit etc. also tolower, toupper.*/


/* Common includes */

#include    "init.h"         /* Declare task running to HERMES and initialize.*/
#include    "finis.h"        /* Informs HERMES that servant quits and cleans up the mess.*/
#include    "anyout.h"       /* General character output routine for GIPSY programs.*/
#include    "setfblank.h"    /* Subroutine to set a data value to the universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "interpol.h"     /* Bilinear interpolation */
#include    "matrix.h"       /* Read data in M[y1..yn][x1..xn] format */


/* User input routines */

#include    "userfio.h"      /* Easy-C companions for user interface routines.*/
#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"      
#include    "userreal.h"     
#include    "userdble.h"     
#include    "usertext.h"     
#include    "usercharu.h"    
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/


/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "minmax3.h"      /* Find min, max and #blanks in subset. */
#include    "wminmax.h"      /* Writes (new) minimum and maximum and number */
                             /* of blanks of subsets in the descriptor file */
                             /* and optionally deletes the MINMAX descriptors */
                             /* at intersecting levels. */


/* Output set related */

#include    "gdsasn.h"       /* GDSASN copies the coordinate system of a */
                             /* previously opened input set obtained with */
                             /* GDSINP to the output set to be obtained */
                             /* with GDSOUT. */
#include    "gdsout.h"       /* GDSOUT prompts the user to enter the */
                             /* name of an output set and the subsets, */
                             /* and returns the number of subsets entered. */
#include    "gdsi_write.h"   /* Writes data to (part of) an set. */



/* DEFINITIONS: */

/* Initialize Fortran compatible string with macro 'fmake' */

#define fmake(fchr,size) { \
                           static char buff[size+1]; \
                           int i; \
                           for (i = 0; i < size; buff[i++] = ' '); \
                           buff[i] = 0; \
                           fchr.a = buff; \
                           fchr.l = size; \
                         } 

/* Malloc version of 'fmake. Strings allocated with'  */
/* finit, must be freed with free( fc.a ) */
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }  

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )
#define PI             3.141592653589793
#define RAD(a)         ( a * 0.017453292519943295769237 )
#define DEG(a)         ( a * 57.295779513082320876798155 )
#define SWAP(a,b)      { double temp=(a);(a)=(b);(b)=temp; } /* Swap 2 numbers */


#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     2048       /* Max. allowed subsets */
#define MAXBUF         4096       /* Buffer size for I/O */
#define OPERATIONS     8
#define MAXOPERATIONS  32
#define STRLEN         256        /* Max length of strings */
#define FILENAMELEN    256        /* Max length of file names */
#define FITSLEN        20         /* Max length of header items etc.*/
#define NONE           0          /* Default levels in userxxx routines */
#define REQUEST        1          
#define HIDDEN         2          
#define EXACT          4          
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0          

#define TRANS          1
#define ROTATION       2
#define SCALING        3
#define REFLECTX       4
#define REFLECTY       5
#define REFLECTCEN     6
#define XSHEAR         7
#define YSHEAR         8

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (,subsets) to transform:")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set (subset(s)): ")
#define KEY_POS	       tofchar("POS=")
#define MES_POS        tofchar("Enter position of centre:     [0,0]")
#define KEY_OPERATIONS tofchar("OPERATION=")
#define MES_OPERATIONS tofchar("Enter sequence of operations:   [2 (=rotation)]")
#define KEY_TRANSLXY   tofchar("TRANSLXY=")
#define KEY_ANGLE      tofchar("ANGLE=")
#define KEY_SCALEXY    tofchar("SCALEXY=")
#define KEY_XSHEAR     tofchar("XSHEAR=")
#define KEY_YSHEAR     tofchar("YSHEAR=")


typedef struct 
{
   fchar        Name;                /* Set name */
   fint         setdim;              /* Dimensions of set */
   fint         subdim;              /* Dimensions of subset(s) */
   fint         subset[MAXSUBSETS];  /* Subset array */
   fint         nsubs;               /* Number of subsets */
   fint         axnum[MAXAXES];      /* Array of size MAXAXES containing the */
                                     /* axes numbers. The first elements */
                                     /* (upto the dimension of the subset) */ 
                                     /* contain the axes numbers of the */
                                     /* subset, the other ones ontain the */
                                     /* axes numbers outside the */
                                     /* the subset ordered according to the */
                                     /* specification by the user. */
   fint         axcount[MAXAXES];    /* Array of size MAXAXES containing the */
                                     /* number of grids along an axes as */
                                     /* specified by the user. The first elements */
                                     /* (upto the dimension of the subset) contain */
                                     /* the length of the subset axes, the other */
                                     /* ones contain the the number of grids along */
                                     /* an axes outside the subset. */
                                     /* the operation for each subset, Class 2 */
                                     /* is for applications for which the operation */
                                     /* requires an interaction between the different */
                                     /* subsets. */
   fint         blo[MAXAXES];        /* Lower left of box */
   fint         bhi[MAXAXES ];       /* Upper right of box */
   fint         class;               /* Usually 1 (repeat operations for subsets */
   float      **image;               /* Data buffer for this set */
} set;                                        
                             


/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static char     message[STRLEN];    /* All purpose character buffer. */




static int gaussj( double  **a,
                   int       n )
/*-------------------------------------------------------------*/
/* PURPOSE: Linear equation solution by Gauss-Jordan elimi-    */
/* nation (numerical Recipes in C (2nd ed.) $2.1.              */
/* a[1..n][1..n] is the input matrix. b[1..n][1..m] is input   */
/* containing the m(=1) right-hand side vectors. On output a   */
/* is replaced by its matrix inverse, and b is replaced by the */
/* corresponding set of solution vectors.                      */
/* Error return codes:                                         */
/* gaussj = 0        Success                                   */
/* gaussj = -1       Memory allocation problems.               */
/* gaussj = -2, -3   Singular Matrix.                          */
/*-------------------------------------------------------------*/
{
   int       *indxc, *indxr, *ipiv;
   int       *idum = NULL;
   int       i, j, k, l, ll;
   int       icol = 0;
   int       irow = 0;
   double    big, dum, pivinv;


   /*----------------------------------------------------------*/
   /* Allocate memory for bookkeeping arrays. Decrease pointer */
   /* so that first element has index 1.                       */
   /*----------------------------------------------------------*/
   idum = (int *) calloc( n, sizeof(int) );
   if (idum)
      indxc = idum - 1;
   else
      return( -1 );
   idum = (int *) calloc( n, sizeof(int) );
   if (idum)
      indxr = idum - 1;
   else
   {
      indxc++;
      free( indxc );
      return( -1 );
   }
   idum = (int *) calloc( n, sizeof(int) );
   if (idum)
      ipiv = idum - 1;
   else
   {
      indxc++;
      free( indxc );
      indxr++;
      free( indxr );
      return( -1 );                 /* Memory allocation error */
   }
   for (j = 1; j <= n; j++)
      ipiv[j] = 0;
   for (i = 1; i <= n; i++)
   {
      big = 0.0;
      for (j = 1; j <= n; j++)
      {
         if (ipiv[j] != 1)
         {
            for (k = 1; k <= n; k++)
            {
               if (ipiv[k] == 0)
               {
                  if (fabs(a[j][k]) >= big)
                  {
                     big = fabs(a[j][k]);
                     irow = j;
                     icol = k;
                  }
               }
               else if (ipiv[k] > 1)
                 return( -2 );                       /* Singular Matrix-1 */
            }
         }
      }
      ++(ipiv[icol]);
      if (irow != icol)
      {
         for (l = 1; l <= n; l++)
            SWAP( a[irow][l], a[icol][l] );
      }
      indxr[i] = irow;
      indxc[i] = icol;
      if (a[icol][icol] == 0.0)
         return( -3 );                                   /* Singular Matrix-2 */
      pivinv = 1.0 / a[icol][icol];
      a[icol][icol] = 1.0;
      for (l = 1; l <= n; l++)
         a[icol][l] *= pivinv;
      for (ll = 1; ll <= n; ll++)
      {
         if (ll != icol)
         {
            dum = a[ll][icol];
            a[ll][icol] = 0.0;
            for (l = 1; l <= n; l++)
               a[ll][l] -= a[icol][l] * dum;
         }
      }
   }
   for (l = n; l >= 1; l--)
   {
      if (indxr[l] != indxc[l])
         for (k = 1; k <= n; k++)
             SWAP( a[k][indxr[l]], a[k][indxc[l]] );
   }
   /* Free space but do not forget to raise pointers first */
   ipiv++;  free( ipiv );
   indxr++; free( indxr );
   indxc++; free( indxc );
   
   return( 0 );
}



static void transform( float    **Mi,
                       float    **Mo,
                       fint      *blo,
                       fint      *bhi,
                       double     T[3][3] )
/*------------------------------------------------------------------*/
/* PURPOSE: */
/*------------------------------------------------------------------*/
{             
   int      ix, iy;                      
   double   x2, y2;
   double   x1, y1;
   double   a,b,c,d,e,f;
   

   a = T[0][0]; b = T[1][0]; c = T[2][0];
   d = T[0][1]; e = T[1][1]; f = T[2][1];
   for (y2 = (double) blo[1]; y2 <= (double) bhi[1]; y2++)
   {
      for (x2 = (double) blo[0]; x2 <= (double) bhi[0]; x2++)
      {
         x1 = a*x2 + b*y2 + c;
         y1 = d*x2 + e*y2 + f;
         ix = (int) x2;
         iy = (int) y2;
         Mo[iy][ix] = interpol( x1, y1, Mi, blo, bhi, blank );
      } 
   }
}




MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint     maxsubs = MAXSUBSETS;
   fint     maxaxes = MAXAXES;           /* Max num. of axes the program can deal with.*/
   fint     showdev = 1;
   fint     dfault;                      /* Default option for input etc */
   fint     nitems;
   fint     blo[MAXAXES];                /* Low  edge of box in grids */
   fint     bhi[MAXAXES];                /* High edge of box in grids */       
   fint     imagesize;                   /* Counters */
   fint     subnr;                       /* Counter for subset loop. */      
   fint     r1;
   fint     numop;
   fint     transforms[MAXOPERATIONS];   
   int      i, j, op;
   int      agreed;
   int      opcount[OPERATIONS];
   set      inset, outset;
   double   cpos[2];
   double   mat[MAXOPERATIONS][3][3];
   double   res[3][3];
   double   **inv;
   

   init_c();                             /* contact Hermes */
   /* Task identification */
   {
      fchar    Task;                     /* Name of current task */
      fmake( Task, 20 );                 /* Macro 'fmake' must be available */
      myname_c( Task );                  /* Get task name */
      Task.a[nelc_c(Task)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, RELEASE ); /* Show task and version */
   }
   setfblank_c( &blank );

   /*--------------------------------------------------*/
   /* Get the input set. Documentation can be found in */
   /* $gip_sub/gdsinp.dc2                              */
   /*--------------------------------------------------*/
   fmake( inset.Name, STRLEN );
   dfault        = NONE;
   inset.subdim  = 2;                    /* Allow only 2-dim structures */
   inset.class   = 1;
   inset.nsubs   = gdsinp_c( 
                      inset.Name,        /* Name of input set. */
                      inset.subset,      /* Array containing subsets coordinate words. */
                      &maxsubs,          /* Maximum number of subsets in 'subin'.*/
                      &dfault,           /* Default code as is USERxxx. */
                      KEY_INSET,         /* Keyword prompt. */
                      MES_INSET,         /* Keyword message for the user. */
                      &showdev,          /* Device number (as in ANYOUT). */
                      inset.axnum,       /* Array of size 'maxaxes' containing the axes numbers. */
                                         /* The first elements (upto the dimension of the subset) */
                                         /* contain the axes numbers of the subset, */
                                         /* the other ones contain the axes numbers */
                                         /* outside the subset ordered according to the */
                                         /* specification by the user. */
                      inset.axcount,     /* Number of grids on axes in 'axnum' */
                      &maxaxes,          /* Max. number of axes. */
                                         /* the operation for each subset. */
                      &(inset.class),    /* Class 1 is for applications which repeat */
                      &(inset.subdim) ); /* Dimensionality of the subsets for class 1 */

   inset.setdim  = gdsc_ndims_c( inset.Name, &setlevel );


   /*-------------------------------*/
   /* Prepare grid ranges for INSET */
   /*-------------------------------*/
   {
      fint     boxopt = 0;          /* The different options are: */
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */      

      dfault = REQUEST;
      gdsbox_c( inset.blo, inset.bhi, 
                inset.Name, 
                inset.subset,
                &dfault, 
                KEY_BOX, MES_BOX, 
                &showdev, 
                &boxopt );
   }
   
   for (i = 0; i < inset.subdim; i++)
   {
      blo[i] = inset.blo[i];
      bhi[i] = inset.bhi[i];
   }
   
   /*--------------------------------------------------------------*/
   /* Assign 'gdsinp' buffer to 'gdsout'. Output set will get same */
   /* coordinate system as input INSET=.  GDSOUT is a function     */
   /* which prompts the user to enter the name of a set and        */
   /* (optionally) subset(s) and returns the number of subsets     */
   /* entered.                                                     */
   /*--------------------------------------------------------------*/
   gdsasn_c( KEY_INSET, KEY_OUTSET, &(inset.class) );
   dfault  = NONE;
   fmake( outset.Name, STRLEN );
   do 
   {
      outset.nsubs = gdsout_c( 
                        outset.Name,    /* Name of the output set. */
                        outset.subset,  /* Output array with subsets coordinate words.*/
                        &(inset.nsubs), /* Maximum number of subsets in subout. */
                        &dfault,        /* Default code as in USERxxx. */
                        KEY_OUTSET,     /* User keyword prompt. */
                        MES_OUTSET,     /* Message for the user. */
                        &showdev,       /* Device number (as in ANYOUT). */
                        outset.axnum,   /* Array of size 'maxaxes' containing the axes numbers. */
                        outset.axcount, /* Array with the axis sizes. */
                        &maxaxes );     /* Max axes the program can deal with. */
      agreed = (outset.nsubs == inset.nsubs);
      if (!agreed)
         reject_c( KEY_OUTSET, tofchar("#out != #in") );
   }
   while (!agreed);


   /* Give menu with options and let user define the operations */
   /* Note that rotation etc. is defined wrt. a user given centre */
   

   cpos[0] = cpos[1] = 0.0;
   nitems = 1;                                    /* One position (2 numbers) */
   dfault = REQUEST;
   r1 = gdspos_c( cpos,
                  &nitems,
                  &dfault,
                  KEY_POS,
                  MES_POS,  
                  inset.Name,
                  inset.subset );

   
   anyoutf( 8, " " );
   anyoutf( 8, "Operations: (With respect to POS=)" );
   anyoutf( 8, "===================================" );
   anyoutf( 8, "1. Translation" );
   anyoutf( 8, "2. Rotation (counter clockwise)" );
   anyoutf( 8, "3. Scaling" );
   anyoutf( 8, "4. Reflection about x-axis" );
   anyoutf( 8, "5. Reflection about y-axis" );   
   anyoutf( 8, "6. Reflection wrt. centre" );
   anyoutf( 8, "7. X shear" );
   anyoutf( 8, "8. Y shear" );   
   anyoutf( 8, " " );
   anyoutf( 8, "Example: OPERATION=1 2 1  (=translation followed by rotation and translation)" );
   anyoutf( 8, " " );   
   
   nitems = MAXOPERATIONS;
   dfault = REQUEST;
   transforms[0] = 2;
   numop = userint_c( transforms, &nitems, &dfault, KEY_OPERATIONS, MES_OPERATIONS );
   if (!numop)   /* default */
      numop = 1;

   for (i = 0; i < OPERATIONS; i++)
      opcount[i] = 0;

   

   /* First matrix is a translation to 0,0 */   
   res[0][0] = 1.0;      res[0][1] = 0.0;      res[0][2] = 0.0;
   res[1][0] = 0.0;      res[1][1] = 1.0;      res[1][2] = 0.0;
   res[2][0] = -cpos[0]; res[2][1] = -cpos[1]; res[2][2] = 1.0;  
 
   for (i = 0; i < numop; i++)
   {
      int   nr;      
      dfault = REQUEST;      
      /*----------------------------------------*/
      /* Matrices are based on so called        */
      /* 'Homogeneous Coordinates'.             */
      /*----------------------------------------*/              
      switch ( (int)transforms[i] )
      {
         case TRANS:
         {
            double    t[2];
            nr = opcount[TRANS];
            t[0] = t[1] = 0.0;
            nitems = 2;
            if (nr == 0)
               sprintf( message, "Enter translation in x and y:   [0,0]");
            else
               sprintf( message, "enter x, y for %dnd translation:   [0,0]", 
               nr+1 ); 
            r1 = userdble_c( t, &nitems, &dfault,
                             KEY_TRANSLXY, tofchar(message) );
            cancel_c( KEY_TRANSLXY );                             
            opcount[TRANS]++;
            mat[i][0][0] = 1.0;  mat[i][0][1] = 0.0;  mat[i][0][2] = 0.0;
            mat[i][1][0] = 0.0;  mat[i][1][1] = 1.0;  mat[i][1][2] = 0.0;
            mat[i][2][0] = t[0]; mat[i][2][1] = t[1]; mat[i][2][2] = 1.0;
            break;
         }
         case ROTATION:
         {
            double    a;
            nr = opcount[ROTATION];
            a = 0.0;
            nitems = 1;
            if (nr == 0)
               sprintf( message, "Enter rotation angle (deg):    [0]");
            else
               sprintf( message, "Enter angle for %dnd rotation (deg):   [0]", 
               nr+1 );               
            r1 = userdble_c( &a, &nitems, &dfault,
                             KEY_ANGLE, tofchar(message) ); 
            cancel_c( KEY_ANGLE );                             
            opcount[ROTATION]++;
            a = RAD( a );
            mat[i][0][0] = cos(a);   mat[i][0][1] = sin(a);  mat[i][0][2] = 0.0;
            mat[i][1][0] = -sin(a);  mat[i][1][1] = cos(a);  mat[i][1][2] = 0.0;
            mat[i][2][0] = 0.0;      mat[i][2][1] = 0.0;     mat[i][2][2] = 1.0;
            break;
         }
         case SCALING:
         {
            double    s[2];
            nr = opcount[SCALING];
            s[0] = s[1] = 1.0;
            nitems = 2; 
            if (nr == 0)
               sprintf( message, "Enter scaling in x and y:    [1,1]" );
            else
               sprintf( message, "Enter x and y for %dnd scaling  [1,1]", nr+1 );
            r1 = userdble_c( s, &nitems, &dfault,
                             KEY_SCALEXY, tofchar(message) );
            cancel_c( KEY_SCALEXY );                             
            opcount[SCALING]++;
            mat[i][0][0] = s[0]; mat[i][0][1] = 0.0;  mat[i][0][2] = 0.0;
            mat[i][1][0] = 0.0;  mat[i][1][1] = s[1]; mat[i][1][2] = 0.0;
            mat[i][2][0] = 0.0;  mat[i][2][1] = 0.0;  mat[i][2][2] = 1.0;
            break;
         }
         case REFLECTX:
         {
            mat[i][0][0] = 1.0;  mat[i][0][1] = 0.0;  mat[i][0][2] = 0.0;
            mat[i][1][0] = 0.0;  mat[i][1][1] = -1.0; mat[i][1][2] = 0.0;
            mat[i][2][0] = 0.0;  mat[i][2][1] = 0.0;  mat[i][2][2] = 1.0;
            break;
         }
         case REFLECTY:
         {
            mat[i][0][0] = -1.0; mat[i][0][1] = 0.0;  mat[i][0][2] = 0.0;
            mat[i][1][0] = 0.0;  mat[i][1][1] = 1.0;  mat[i][1][2] = 0.0;
            mat[i][2][0] = 0.0;  mat[i][2][1] = 0.0;  mat[i][2][2] = 1.0;
            break;
         }
         case REFLECTCEN:         
         {
            mat[i][0][0] = -1.0; mat[i][0][1] = 0.0;  mat[i][0][2] = 0.0;
            mat[i][1][0] = 0.0;  mat[i][1][1] = -1.0; mat[i][1][2] = 0.0;
            mat[i][2][0] = 0.0;  mat[i][2][1] = 0.0;  mat[i][2][2] = 1.0;
            break;
         }
         case XSHEAR:
         {
            double    xs;
            nr = opcount[XSHEAR];
            xs = 0.0;
            nitems = 1;
            if (nr == 0)
               sprintf( message, "Enter shear factor in X:    [0]");
            else
               sprintf( message, "Enter %dnd shear factor in X:   [0]", 
               nr+1 );               
            r1 = userdble_c( &xs, &nitems, &dfault,
                             KEY_XSHEAR, tofchar(message) ); 
            cancel_c( KEY_XSHEAR );                             
            opcount[XSHEAR]++;
            mat[i][0][0] = 1.0;  mat[i][0][1] = 0.0;  mat[i][0][2] = 0.0;
            mat[i][1][0] = xs;   mat[i][1][1] = 1.0;  mat[i][1][2] = 0.0;
            mat[i][2][0] = 0.0;  mat[i][2][1] = 0.0;  mat[i][2][2] = 1.0;
            break;
         }
         case YSHEAR:
         {
            double    ys;
            nr = opcount[YSHEAR];
            ys = 0.0;
            nitems = 1;
            if (nr == 0)
               sprintf( message, "Enter shear factor in Y:    [0]");
            else
               sprintf( message, "Enter %dnd shear factor in Y:   [0]", 
               nr+1 );               
            r1 = userdble_c( &ys, &nitems, &dfault,
                             KEY_YSHEAR, tofchar(message) ); 
            cancel_c( KEY_YSHEAR );                             
            opcount[YSHEAR]++;
            mat[i][0][0] = 1.0;  mat[i][0][1] = ys;   mat[i][0][2] = 0.0;
            mat[i][1][0] = 0.0;  mat[i][1][1] = 1.0;  mat[i][1][2] = 0.0;
            mat[i][2][0] = 0.0;  mat[i][2][1] = 0.0;  mat[i][2][2] = 1.0;
            break;
         }                  
                                    
      }
   }
  
   /* Translate back to central position. Counter 'i' is increaded */
   /* in previous loop. */
   
   mat[i][0][0] = 1.0;      mat[i][0][1] = 0.0;      mat[i][0][2] = 0.0;
   mat[i][1][0] = 0.0;      mat[i][1][1] = 1.0;      mat[i][1][2] = 0.0;
   mat[i][2][0] = cpos[0];  mat[i][2][1] = cpos[1];  mat[i][2][2] = 1.0;  

   numop++;                                   /* A translation back was added */


   /*--------------------------------------------------*/
   /* Concatenate the operations into one matrix.      */
   /* Start each sequence of operations with a trans-  */
   /* lation to the new origin. Close the sequence     */
   /* with a translation back to 0,0.                  */
   /*--------------------------------------------------*/
   for (op = 0; op < numop; op++)
   {
      int    row, col;
      double dum[3][3];
      for (row = 0; row < 3; row++)
         for (col = 0; col < 3; col++)
            dum[row][col] = res[row][col];
            
      for (col = 0; col < 3; col++)      
      {
         for (row = 0; row < 3; row++)
         {
            res[row][col] = dum[row][0]*mat[op][0][col]+
                            dum[row][1]*mat[op][1][col]+
                            dum[row][2]*mat[op][2][col];
         }
      }
   }   
   
   inv = dmatrix(1, 1, 3, 3);
   for (j = 0; j < 3; j++)
   {
      anyoutf( 1, " " );
      for (i = 0; i < 3; i++)
      {
         anyoutf( 16, "matrix [%d][%d]:  %f", i, j, res[i][j] ) ;
         inv[i+1][j+1] = res[i][j];
      }
   }


   r1 = gaussj( inv, 3 );
   if (r1 != 0)
      errorf( 4, "Cannot invert transformation matrix" );

   for (j = 0; j < 3; j++)
   {
      anyoutf( 1, " " );
      for (i = 0; i < 3; i++)
      {
         res[i][j] = inv[i+1][j+1];
         anyoutf( 16, "inverse  [%d][%d]:  %f", i, j, res[i][j] );
      }
   }
   freedmatrix( inv, 1, 1 );
      
  
   /* Prepare the data buffers for input and output data */
    
   inset.image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );
   if (!inset.image)
      errorf( 4, "Cannot allocate memory for input image buffer!" );
   imagesize = (bhi[0] - blo[0] + 1) * (bhi[1] - blo[1] + 1);
 
   outset.image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );
   if (!outset.image)
      errorf( 4, "Cannot allocate memory for output image buffer!" );


   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/
   for(subnr = 0; subnr < inset.nsubs; subnr++)
   {
      fint  tid = 0;               /* Transfer id's */
      fint  cwlo, cwhi;            /* Coordinate words */
      fint  pixelsread;            /* Number of pixels read by read routine. */
      fint  tidO = 0;
      fint  cwloO, cwhiO;
      fint  pixelswrite;           /* Number of pixels to write to output. */

      cwlo   = gdsc_fill_c( inset.Name, &(inset.subset[subnr]), blo );
      cwhi   = gdsc_fill_c( inset.Name, &(inset.subset[subnr]), bhi );
      
     /* Use input grid coordinates, but connect to output subsets */
           
      cwloO  = gdsc_fill_c( outset.Name, &(outset.subset[subnr]), blo );
      cwhiO  = gdsc_fill_c( outset.Name, &(outset.subset[subnr]), bhi );
      tid    = 0;

      /* Read 'maxIObuf' values in 'image'. */
      gdsi_read_c( inset.Name,
                   &cwlo, &cwhi,
                   &(inset.image[blo[1]][blo[0]]),
                   &imagesize,
                   &pixelsread,
                   &tid );
                   
      transform( inset.image, outset.image, blo, bhi, res );

      pixelswrite = pixelsread;

      /* Write 'pixelswrite' values from 'image' to output. */
      gdsi_write_c( outset.Name,
                    &cwloO, &cwhiO,
                    &(outset.image[blo[1]][blo[0]]), 
                    &imagesize,
                    &pixelswrite,
                    &tidO );
   }

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   freefmatrix( inset.image, blo[0], blo[1] );
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
