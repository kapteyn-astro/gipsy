/*
                            COPYRIGHT (c) 1997
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             fit.dc1

Program:       FIT

Purpose:       Fit a set of data points (Xi,Yi) to a model, that depends 
               nonlinearly on its parameters, using a Chi-squared 
               minimization.

Category:      FITTING, UTILITY

File:          fit.c

Author:        M.G.R. Vogelaar

Keywords:


   X=          Enter X values:
   
               Examples: 
               X=file(fitdata.dat,1,1:)     , read first column of ASCII file 
               X=1:3:0.05                   , generate x=1 to 3, step size 0.05

   
   Y=          Enter Y values:
   
               Examples:
               Y=image(spectr1, -127 0 128 0)

               If the Y-data contains blank values then the 
               corresponding weights will be set to 0.
               

   GRDEVICE=   Plot device:                           [List of devices]

               Destination of plot, Screen or Hardcopy.
               Cursor interaction is possible in X11 and GIDS



               ============= INSIDE THE FITTING LOOP: ===========
               
              
   WEIGHTS=    Enter weights (W_i=1/VAR_i):               [1.0 for all]
   
               The weights are the inverse of the variances in Y_i.
               The maximum number of weights is the same as the number 
               of data points and is initialized to 1.0 (default). If 
               there are less weights specified than the number of 
               data points, then the missing weights are set to 1.0.
               Pressing the left mouse button while in cursor mode 
               (CURSOR=YES) toggles between the original weight and
               weight 0.0 for a marked data point. Weights that are 
               set to 0 with the mouse can only get a new value
               if they are unmarked. Data X values that can not be
               evaluated by the expression evaluation are excluded
               by setting the weight to 0.0.
               
   
   FX=         Enter expression:

               Describe your model as a function of X.
               See description for allowed functions and constants.
               At this point keywords are prompted that
               represent a model parameter (best fit parameter) in 
               your expression. The maximum number of parameters is 31.
               The length of a parameter may not exceed 16 characters.
               The expression is case insensitive.
 
               E.g. FX=A*EXP(-(X-B)*(X-B)/(2.0*SIG*SIG)) generates
               A=, B= and SIG= as best-fit parameters and keywords. 
               This function is also an example of a model that depends 
               NONlinearly on its parameters.
               WARNING: Do not create variable names equal to one 
               of the listed keywords because otherwise you can get
               wrong initial estimates.
             
   ....=
   ....=
   ....=   


   CURSOR=     Exclude data with cursor?                          Y/[N]
   
               After the data is displayed in a window, it is 
               possible to mark data using the cursor. A marked data
               point gets a green colour and weight 0. If you mark it 
               again, it will become yellow again and gets
               its original weight (as in WEIGHTS=).


   QUIT=       Want to quit?                                      Y/[N]
   
               End loop over keywords and fits and end program.


   FILENAME=   Name of output ASCII file:          [No output to file]
               If a name is specified, an ASCII file is created
               where fit parameters are listed in a row. If you
               press carriage return, there will be no output to
               an ASCII file.


   APPEND=     File exists, ok to append?                        [Y]/N
               The file specified in FILENAME= already exists. You
               can append to this file with APPEND=Y. If APPEND=N
               you will be prompted for another name.


               ==== Next keywords are asked in a loop, to be ====
               ==== aborted with INSET=<CR>                  ====




               =========== HIDDEN KEYWORDS: ===========

               Plotting:
               ---------
   
** PGMOSAIC=   View surface sub divisions in x,y:                 [1,1]

               View surface can contain a number of plots in 
               in X and Y direction (mosaic). Default is 1 plot in 
               both X- and Y direction.

   
** PGBOX=      Corners of box Xl,Yl,Xh,Yh:     [default by application]

               It is possible to overrule the calculated 
               PGPLOT box size with PGBOX=  The coordinates (x,y) of 
               the lower point are given first.

 
** PGWIDTH=    Give line width 1..21:                               [2]


** PGHEIGHT=   Give character height:                             [1.0]


** PGFONT=     Give font 1..4:                                      [2]

               1  single stroke 'normal' font
               2  roman font
               3  italic font
               4  script font


** YERRRANGE=  Enter Y range for residues:        [calculated Ylo, Yhi]

               The difference between model and original data is 
               displayed in a second window. The range in Y can be 
               adjusted with this keyword. If you change the default
               values after a fit, use CLEAR=YES to erase the plot 
               first.
   

** CLEAR=      Clear plot page?                                   Y/[N]



               Fitting:
               --------
   
** TOLERANCE=  Fractional tolerance for Chi-squared:            [0.001]

               Fitting is stopped when successive iterations fail
               to produce a decrement in reduced Chi-squared less
               than TOLERANCE=   The value cannot be less than a
               certain minimum set by the system. This means that
               maximum accuracy can be obtained with TOLERANCE=0.0
                                                                           
   
** LAB=        Mixing parameter:                                [0.001]

               The Mixing parameter, LAB= determines the initial
               weight of steepest descent method relative to the 
               Taylor method. LAB= should be a small value (i.e. 0.001). 
               LAB can only be zero when the partial derivatives are 
               independent of the parameters. In fact in this case LAB=
               should be exactly equal to zero.


** MAXITS=     Maximum number of iterations:                      [100]
    
               Stop the fitting process if it needs more than 
               this number of iteration.
         


Description:   Program FIT fits model parameters to data that you
               enter with keywords X= and Y=  The 'best' fit
               parameters are those that minimizes Chi-squared.
               The model is entered as an expression in which you 
               define the variable names yourself. The model may 
               depend non-linearly on its parameters. If for 
               instance you entered FX=A*EXP(-(X-B)*(X-B)/(2.0*SIG*SIG))
               then you are prompted to enter initial estimates for 
               A=, B= and SIG=
                             
               The following functions are recognized:
               
               SQRT, SIN, ASIN, COS, ACOS, TAN, ATAN, EXP, LN, LOG,
               SINH, COSH, TANH, RAD, DEG, SINC.
               
               The following constants are recognized:
               
               PI             3.14159....
               C              speed of light (SI)
               H              Planck (SI)
               K              Boltzmann (SI)
               G              gravitation (SI)
               S              Stefan-Boltzman (SI)
               M              mass of sun (SI)
               P              parsec (SI)
               BLANK          Universal undefined value
               
               To prevent that these constants conflict with any variables
               you must enclose the constants by braces, e.g. FX={C}*X
               
               The maximum number of parameters is 31 and the length of 
               a parameter name may not exceed 16 characters.
               
             
               The (initial) function is plotted (using your initial 
               estimates) so that you have some control over how close the 
               initial estimates are to the data. In most cases the fit will
               succeed, even with very rough initial estimates, but 
               sometimes the fit is more sensitive and you have to think 
               a bit harder about the initial values. 
               The method used for non-linear models is the Marquardt 
               method (Marquardt D.W. 1963, Journal of the Society for 
               Industrial and applied Mathematics, vol 11 pp. 431-441).
               The method is a maximum neighbourhood method which
               performs an optimum interpolation between the Taylor
               series fitting method and the gradient method.
               The Mixing parameter, LAB= determines the initial
               weight of the gradient method relative to the Taylor method.
               The program modifies this value so only a start value 
               is required. A value LAB=0.001 works satisfactorily.
               The fitting process needs a condition for stopping.
               When successive iterations fail to produce a decrement 
               in reduced(!) Chi-squared less than TOLERANCE=
               the fitting process is stopped. Iterating to machine 
               accuracy is possible if you set TOLERANCE=0, but this 
               will not be statistically very meaningful. Therefore
               the default value for the tolerance is 0.001.
               Usually the Marquardt method needs symbolic expressions
               for the partial derivatives. This program is able to 
               create these expressions after the model is entered.
               If you want to check the symbolic partial derivatives
               generated by the program, set Hermes in 'test' mode and 
               watch the log file.
                              
               If a fit is ended successfully, the fitted parameters are 
               displayed in the log file together with their uncertainties
               If N is the number of independent data points and n is the 
               number of fitted parameters, then a reduced Chi-squared_r 
               can be calculated from the Chi-squared:
               
                       Chi-squared_r = Chi-squared/(N-n-1)
                       
               The least squares fit maximizes the probability 
               distribution function for Chi-squared by minimizing
               the function:
               
                                    ( y_i-y0[x_i] )^2
               Chi-squared = SIGMA {-----------------}
                                        sd_i^2  
              
             
               where sd_i is the standard deviation of data point i.
               Note that 1/sd_i^2 is the definition of the weights
               in WEIGHTS=
               
               The maximum of the probability distribution function is 
               also calculated. It describes the probability that a
               RANDOM set of N data points would yield a value of 
               Chi-squared as large or larger when compared with the 
               parent function (Bevington, data reduction and Error Analysis
               for the Physical Sciences). 
             
Notes:         .......

Example:       

      <USER> fit
      (start fit)        06/02/97 09:59:54
      FIT  Version 1.0  (Feb  6 1997)
      <USER> FIT X=file(fitdata.dat,1,1:)
      <USER> FIT Y=file(fitdata.dat,2,1:)
      <USER> FIT GRDEVICE=x11
      <USER> FIT FX=a*exp(-(x-b)*(x-b)/(2.0*sig*sig))
      <USER> FIT SIG=1
      <USER> FIT B=1.5
      <USER> FIT A=3.3

      ==================================RESULTS=======================
      Fitted to the data was FX = A*EXP(-(X-B)*(X-B)/(2.0*SIG*SIG))
      The fitted parameters (with errors):
      SIG = 1.975 (+/- 0.0960817)
      B = 1.56574 (+/- 0.0469892) 
      A = 3.41704 (+/- 0.0168025)
      least squares fit converged to TOL=0 in 5 iterations,
      The least squares fit converged to TOL=0 using a mixing parameter LAB=0.001.
      You can rerun this with new values for FX=, TOLERANCE=, MAXITS=, LAB=
      and new initial estimates.
      =================================================================

      <USER> FIT QUIT=y
      (end   fit)        06/02/97 10:02:38
      <STATUS>  FIT   +++ FINISHED +++



Updates:       Jan 28, 1997: VOG, Document created.
               Nov 21, 2006: VOG, Included output to Ascii file

#<
*/

/*  fit.c: include files     */

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
#include    "status.h"

/* User input routines */

#include    "userfio.h"      /* Easy-C companions for user interface routines.*/
#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"      
#include    "userreal.h"     
#include    "userdble.h"     
#include    "usertext.h"     
#include    "usercharu.h"    
#include    "userfio.h"
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


/* Expression evaluation */

#include    "fieini.h"
#include    "fiedo.h"
#include    "fiepar.h"
#include    "fieclr.h"
#include    "dydx.h"


/* Miscellaneous */

#include "lsqfit.h"
#include "minmax1.h"

/* PGPLOT includes */

#include    "pgplot.h"       /* All PGPLOT includes. */



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
#define finit( fc , len ) { fc.a = calloc( ( len + 1 ), sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }  

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )
#define PI             3.141592653589793
#define RAD(a)         ( a * 0.017453292519943295769237 )
#define DEG(a)         ( a * 57.295779513082320876798155 )

#define RELEASE        "1.0"      /* Version number */
#define STRLEN         512        /* Max length of strings */
#define FILENAMELEN    256        /* Max length of file names */
#define FITSLEN        20         /* Max length of header items etc.*/
#define NONE           0          /* Default levels in userxxx routines */
#define REQUEST        1          
#define HIDDEN         2          
#define EXACT          4          
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0          
#define MAXARRAYLEN    32*1024
#define MAXVAR         32
#define MAXVARLEN      16              /* Max. length of var. name (restricted by 'fiepar') */
#define MAXEXPRLEN     256
#define MAXDERVLEN     32*1024
#define DUMMYARRAYLEN  512


/* PGPLOT fefines (colours, symbols) */

#define BACKGROUND    0
#define FOREGROUND    1
#define WHITE         1
#define RED           2  
#define GREEN         3
#define BLUE          4   
#define CYAN          5   
#define MAGENTA       6
#define YELLOW        7
#define ORANGE        8
#define GREENYELLOW   9
#define GREENCYAN    10
#define BLUECYAN     11
#define BLUEMAGENTA  12
#define REDMAGENTA   13
#define DARKGRAY     14
#define LIGHTGRAY    15

#define DOTCIRCLE     9
#define SMALLCIRCLE   4
#define PLUS          2
#define STAR          3
#define FULL          1
#define DOTTED        4
#define LINES         1
#define POINTS        0


/* Special constants for gamma function evaluations */

#define   ITMAX    100
#define   EPS      3.0e-7
#define   FPMIN    1.0e-30


/* Miscellaneous */

static float    blank;              /* Global value for BLANK. */
static char     message[STRLEN];    /* All purpose character buffer. */

static float    X[MAXARRAYLEN];
static float    Y[MAXARRAYLEN];
static float    diffY[MAXARRAYLEN];
static float    wdat[MAXARRAYLEN];
static float    curwdat[MAXARRAYLEN];

FILE            *fp;
static char     filename[120];



FILE *fopenC( char *filename )
/*------------------------------------------------------------*/
/* Open file to write data extern. The macro 'fmake' must be  */
/* available.                                                 */
/*------------------------------------------------------------*/
{
#include  "usertext.h"
#include  "userlog.h"
#include  "cancel.h"
#include  "reject.h"

#define   KEY_FILENAME    tofchar("FILENAME=")
#define   MES_FILENAME    tofchar("Name of output ASCII file:     [No output to file]")
#define   KEY_APPEND      tofchar("APPEND=")
#define   MES_APPEND      tofchar("File exists, ok to append?    [Y]/N")

#define   YES   1
#define   NO    0

   fchar    Filename;
   bool     append;
   fint     request = 1;
/*   fint     hidden  = 2;*/
   fint     dfault;
   fint     n;
   fint     nitems;
   fint     agreed;
   FILE     *fp;


   dfault = request;
   fmake( Filename, FILENAMELEN );
   do 
   {
      append = toflog(YES);                               /* Default APPEND=Y */
      n = usertext_c( Filename,
                      &dfault,
                      KEY_FILENAME,
                      MES_FILENAME );
      if (n == 0) 
         return NULL;

      strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */

      fp = fopen(filename, "r");
      if (fp != NULL)                                     /* The file exists */
      {
         nitems = 1;
         n = userlog_c( &append,
                        &nitems,
                        &dfault,
                        KEY_APPEND,
                        MES_APPEND );
         append = tobool( append );
         fclose( fp );
         cancel_c( KEY_APPEND );
      }
      if (!append) 
      {
          cancel_c( KEY_FILENAME );
          agreed = NO;
      }
      else 
      {
         fp = fopen(filename, "a");
         agreed = (fp != NULL);
         if (!agreed) 
         {
            reject_c( KEY_FILENAME,
                      tofchar("Cannot open, try another!") );
         }
      }
   } 
   while (!agreed);
   return fp;
}



static void errormess( int result )
/*------------------------------------------------------------*/
/* PURPOSE: Generate an error message for 'lsqfit'.           */
/*------------------------------------------------------------*/
{
   if (result == -1) 
      anyoutf(1,  "LSQFIT: Too many free parameters, maximum is 32." );
   else if (result == -2) 
      anyoutf(1,  "LSQFIT: No free parameters." );
   else if (result == -3) 
      anyoutf(1,  "LSQFIT: Not enough degrees of freedom." );
   else if (result == -4) 
      anyoutf( 1,  "LSQFIT: Maximum number of iterations too small to obtain a solution which satisfies tolerance." );
   else if (result == -5) 
      anyoutf(1,  "LSQFIT: Diagonal of matrix contains elements which are zero." );
   else if (result == -6) 
      anyoutf(1,  "LSQFIT: Determinant of the coefficient matrix is zero." );
   else if (result == -7) 
      anyoutf(1,  "LSQFIT: Square root of negative number." );
}



static void inifieerror( fint err,
                         fint errpos )
/*------------------------------------------------------------*/
/* PURPOSE: Generate an error message for 'inifie'.           */
/*------------------------------------------------------------*/
{
   if (err == -2)    
      anyoutf( 1, "INIFIE: No storage space left.");
   else if (err == -1)
      anyoutf( 1, "INIFIE: Syntax error in expression at position %d",
                   errpos );
}



static void dydxerror( fint err )
/*------------------------------------------------------------*/
/* PURPOSE: Generate an error message for 'inifie'.           */
/*------------------------------------------------------------*/
{
   if (err == -1)
      anyoutf( 1, "DYDX: syntax error" );
   else if (err == -2)
      anyoutf( 1, "DYDX: syntax error" );
   else if (err == -3)
      anyoutf( 1, "DYDX: unknown function" );
   else if (err == -4)
      anyoutf( 1, "DYDX: function too large for output argument" );
   else if (err == -5)
      anyoutf( 1, "DYDX: derivative too large for output argument" );
   else if (err == -6)
      anyoutf( 1, "DYDX: constant name too long for output argument" );
   else if (err == -7)
      anyoutf( 1, "DYDX: too many constants for output argument" );   
}



static float gammln( float xx )
/*-------------------------------------------------------------*/
/* PURPOSE: Return the value Ln(T(xx)) for xx > 0              */
/*-------------------------------------------------------------*/
{
   int       j;
   double    x, y;
   double    tmp, ser;
   double    cof[6] = {  76.18009172947146,
                        -86.50532032941677,
                         24.01409824083091,
                         -1.231739572450155,
                          0.1208650973866179e-2,
                         -0.5395239384953e-5 };
  
   y    = x = xx;
   tmp  = x + 5.5;
   tmp -= (x+0.5) * log(tmp);
   ser  = 1.000000000190015;
   for (j = 0; j < 6; j++)
   {
      ser += cof[j] /++y;      
   }
   return( -tmp+log(2.5066282746310005*ser/x) );
   
}


static void  gser( float *gamser,
                   float  a,
                   float  x, 
                   float *gln )
/*-------------------------------------------------------------*/
/* PURPOSE: returns the incomplete gamma function P(a,x)       */
/*          evaluated by its series representation as 'gamser' */
/*          Also returns ln(T(a)) as 'gln'.                    */
/*-------------------------------------------------------------*/
{
   int     n;
   float   sum, del, ap;
   
   
   *gln = gammln( a );
   if (x <= 0.0)
   {
      if (x < 0.0)
      {
         anyoutf( 16, "GSER: x less than 0" );
         *gamser = blank;
         return;
      }
      else
      {
         *gamser = 0.0;
         return;
      }      
   }
   else
   {
      ap = a;
      del = sum = 1.0 / a;
      for (n = 1; n <= ITMAX; n++)
      {
         ++ap;
         del *= x / ap;
         sum += del;
         if (fabs(del) < fabs(sum)*EPS)
         {
            *gamser = sum * exp(-x+a*log(x)-(*gln));
            return;
         }
      }
      anyoutf( 16, "GSER: a too large, ITMAX too small" );
      *gamser = blank;
      return;
   }
   
}                     


static void gcf( float *gammcf,
                 float  a,
                 float  x,
                 float *gln )
/*-------------------------------------------------------------*/
/* PURPOSE: returns the incomplete gamma function Q(a,x)       */
/*          evaluated by its series representation as gammcf'  */
/*          Also returns ln(T(a)) as 'gln'.                    */
/*-------------------------------------------------------------*/
{
   int      i;
   float    an, b, c, d, del, h;


   *gln = gammln( a );
   b = x + 1.0 - a;
   c = 1.0 / FPMIN;
   d = 1.0 / b;
   h = d;
   for (i = 1; i <= ITMAX; i++)
   {
      an = -i * (i-a);
      b += 2.0;
      d = an * d + b;
      if (fabs(d) < FPMIN)
         d = FPMIN;
      c = b + an / c;
      if (fabs(c) < FPMIN)
         c = FPMIN;
      d = 1.0 / d;
      del = d * c;
      h *= del;
      if (fabs(del-1.0) < EPS)
         break;         
   }
   if (i > ITMAX)
   {
      *gammcf = blank;
      anyoutf( 16, "GCF: a too large, ITMAX too small" );
      return;
   }
   *gammcf = exp( -x+a*log(x)-(*gln)) * h;
}



static float gammq( float   a,
                    float   x )
/*-------------------------------------------------------------*/
/* PURPOSE: Returns the incomplete gamma function Q(a,x)       */
/* The function returns a blank if something did go wrong.     */
/*-------------------------------------------------------------*/
{
   float   gamser, gammcf, gln;
   float   result;
   
   if (x < 0.0 || a <= 0.0)
   {
      anyoutf( 16, "GAMMQ: Invalid arguments" );
      return( blank );
   }
   if (x < (a+1.0))
   {
      gser( &gamser, a, x, &gln );
      result = 1.0 - gamser;
   }
   else
   {
      gcf( &gammcf, a, x, &gln );
      result = gammcf;
   }
   return( result );
}



static void plotpoints( float *X,
                        float *Y,
                        int    ndat,
                        int    color,
                        int    symbol,
                        int    mode )
/*-------------------------------------------------------------*/
/* PURPOSE: Plot one or more markers, but skip data points     */
/*          blanks.                                            */
/*-------------------------------------------------------------*/
{                              
   fint sym = (fint) symbol;
   fint oldcol;
   fint newcol = (fint) color;
   fint N = (fint) ndat;   
   int  i, j;
   
   

   pgqci_c( &oldcol );
   pgsci_c( &newcol );     
   i = 0;
   do
   {
      while (i < ndat && Y[i] == blank) 
         i++;
      j = i;
      N = 0;
      while (i < ndat && Y[i] != blank)
      {
         N++;
         i++;
      }
      if (N > 0)
      {
         if (mode == POINTS)
            pgpt_c( &N, &X[j], &Y[j], &sym );
         else 
            pgline_c( &N, &X[j], &Y[j] );
      }         
   }
   while (i < ndat);
   
   pgsci_c( &oldcol );                              /* Restore previous colour */
}



static void setviewport( float Xlo, 
                         float Ylo,
                         float Xhi,
                         float Yhi )
/*-------------------------------------------------------------*/
/* PURPOSE: */
/*-------------------------------------------------------------*/
{
   pgsvp_c( &Xlo, &Xhi, &Ylo, &Yhi );
}



static void initplot( void )
/*------------------------------------------------------------*/
/* PURPOSE: Initialize PGPLOT.                                */
/* Initialize plot software. Set viewport and output dims.    */
/* If output device is a printer, ask user for line width.    */
/*------------------------------------------------------------*/
{
   fint   unit;            /* Ignored by pgbeg, use unit=0. */
   fchar  Devspec;         /* Device specification. */
   fint   nxysub[2];       /* Number of subdivisions on 1 page. */
   fint   nitems, dfault;
   fint   r;
   bool   paging;          /* Disable PGPLOT's NEXTPAGE keyword. */
   float  xl, xr, yb, yt;  /* Edges of the viewport. */


   /*--------------------------------------------------*/
   /* Initialize PGPLOT with a call to 'pgbeg'.        */
   /* There are 4 arguments for PGBEG:                 */
   /* UNIT, this argument is ignored by PGBEG (use     */
   /*       zero).                                     */
   /* FILE, If this argument is a question mark PGBEG  */
   /*       will prompt the user to supply a string.   */
   /* NXSUB, # sub divisions of the view surface in X. */
   /* NYSUB, # sub divisions of the view surface in Y. */
   /*--------------------------------------------------*/

   nxysub[1] = nxysub[0] = 1;                  /* No sub divisions is default */
   nitems = 2;
   dfault = HIDDEN;
   r = userint_c( nxysub,
                  &nitems,
                  &dfault,
                  tofchar("PGMOSAIC="),
                  tofchar("View surface sub divisions in x,y:   [1,1]") );

   unit = 0;
   Devspec = tofchar("?");
   r = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );
   if (r != 1)                                                 /* Fatal error */
      errorf( 4, "Cannot open output device" );

   /* No PGPLOT's NEXTPAGE= keyword */
   paging = toflog( NO );
   pgask_c( &paging );


   /* Set viewport */ 
   xl = 0.2; xr = 0.95;
   yb = 0.1; yt = 0.9;
   pgsvp_c( &xl, &xr, &yb, &yt );
}



static void drawbox( float  Xmin, 
                     float  Ymin, 
                     float  Xmax, 
                     float  Ymax, 
                     char  *xtitle, 
                     char  *ytitle, 
                     char  *ttitle,
                     int    zeroline,
                     fint   newcol )                     
/*------------------------------------------------------------*/
/* PURPOSE: Draw frame with labels for input box.             */
/* Draw box and labels. Take special care for the y labels    */
/* and title. Colors are defined globally. Xmin etc are the   */
/* corners of the box in world coordinates.                   */
/*------------------------------------------------------------*/
{
   float  charsize = 1.0;
   float  delta;
   fint   lwidth;
   fint   r;
   fint   nitems;
   fint   dfault;
   float  pg_box[4];                 /* Corners of draw box. */
   fint   font;
   fint   nxsub, nysub;
   fint   oldcol;
   float  xtick, ytick;
   char   message[80];
   

      

   pgqci_c( &oldcol );
   pgsci_c( &newcol );   
   /* Increase the size of the box a little */
   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0)
      delta = 1.0;
   Xmin -= delta;
   Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0)
      delta = 1.0;
   Ymin -= delta;
   Ymax += delta;
   pg_box[0] = Xmin;                              /* Get size from user input */
   pg_box[1] = Ymin;
   pg_box[2] = Xmax;
   pg_box[3] = Ymax;
   nitems = 4;
   dfault = HIDDEN;
   sprintf( message, "Corners of box Xl,Yl, Xh,Yh:  [%g,%g,%g,%g]", Xmin,Ymin,Xmax,Ymax );
   r = userreal_c( pg_box,
                    &nitems,
                    &dfault,
                    tofchar("PGBOX="),
                    tofchar( message ) );
   Xmin = pg_box[0];
   Ymin = pg_box[1];
   Xmax = pg_box[2];
   Ymax = pg_box[3];
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );                  /* Set the window */
   

   lwidth = 2;
   nitems = 1;
   dfault = HIDDEN;
   r = userint_c( &lwidth, 
                  &nitems,
                  &dfault,
                  tofchar("PGWIDTH="),
                  tofchar("Give line width 1..21:        [2]") );
   if (lwidth > 21)
      lwidth = 21;
   if (lwidth < 1 )
      lwidth =  1;
   pgslw_c( &lwidth );                                     /* Set line width. */

   charsize = 1.0;
   nitems = 1;
   dfault = HIDDEN;
   r = userreal_c( &charsize, 
                   &nitems,
                   &dfault,
                   tofchar("PGHEIGHT="),
                   tofchar("Give character height:     [1.0]") );
   pgsch_c( &charsize );                                 /* Character height. */

   font   = 2;
   nitems = 1;
   dfault = HIDDEN;
   r = userint_c( &font, 
                  &nitems,
                  &dfault,
                  tofchar("PGFONT="),
                  tofchar("Give font 1..4:        [2]") );
   if (font > 4)
      font = 4; 
   if (font < 1)
      font = 1; 
   pgscf_c( &font );                                             /* Set font. */
   
   /*----------------------------------------------------------------*/
   /* xtick is world coordinate interval between major tick marks    */
   /* on X axis. If xtick=0.0, the interval is chosen by PGBOX, so   */
   /* that there will be at least 3 major tick marks along the axis. */
   /* nxsub is the number of subintervals to divide the major        */
   /* coordinate interval into. If xtick=0.0 or nxsub=0, the number  */
   /* is chosen by PGBOX.                                            */
   /* BCNSTV :                                                       */
   /* B: draw bottom (X) or left (Y) edge of frame.                  */
   /* C: draw top (X) or right (Y) edge of frame.                    */
   /* N: write Numeric labels in the conventional location below     */
   /*    the viewport (X) or to the left of the viewport (Y).        */
   /* S: draw minor tick marks (Subticks).                           */
   /* T: draw major Tick marks at the major coordinate interval.     */
   /* V: orient numeric labels Vertically. This is only applicable   */
   /*    to Y.                                                       */
   /*----------------------------------------------------------------*/
   xtick = ytick = 0.0;
   nxsub = nysub = 0;
   pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
            tofchar("BCNSTV"), &ytick, &nysub );
   
   /* Plot the titles */
   pglab_c( tofchar(xtitle), tofchar(ytitle), tofchar(ttitle) );
   
   if (zeroline)
   {
      float    zero = 0.0;
      fint     linestyle = DOTTED;
      pgsls_c( &linestyle );
      pgmove_c( &Xmin, &zero );
      pgdraw_c( &Xmax, &zero );
      linestyle = FULL;  
      pgsls_c( &linestyle );
   }
   pgsci_c( &oldcol );        
}



static void updateweights( float    *X, 
                           float    *Y, 
                           float    *wdat, 
                           fint      alen,
                           float    *constants, 
                           fint      npar,
                           fint      fieid )
/*------------------------------------------------------------*/
/* PURPOSE: Set the weights of blank data points to zero.     */
/* Not only the data points must be checked but also the      */
/* results of the function evaluation using the initial       */
/* estimates. This way it can be prevented that the 'func'    */
/* routine generates blanks in the lsqfit.                    */
/*------------------------------------------------------------*/
{
   float    Ycheck[MAXARRAYLEN];
   float    fiedata[MAXVAR*MAXARRAYLEN];
   fint     r;
   int      i, n;
   
  
   for (n = 0; n < alen; n++)
   {
      for (i = 0; i < npar; i++)   
      {
         fiedata[i*alen+n] = constants[i];
      }
      fiedata[npar*alen+n] = X[n];
   }
   r = fiedo_c( fiedata, &alen, Ycheck, &fieid );
        
   for (n = 0; n < alen; n++)
   {
      if (Ycheck[n] == blank || Y[n] == blank)
         wdat[n] = 0.0;
   }
   
}



static void plotfunction( float  *constants, 
                          fint    npar, 
                          float  *minmax,
                          int     color,
                          fint    fieid )
/*------------------------------------------------------------*/
/* PURPOSE: Plot the initial or fitted function.              */
/* The function parameters are entered in 'constants'.        */
/* The sampling is such that 'fiedo' calculates at most       */
/* 'DUMMYARRAYLEN' data points.                               */
/*------------------------------------------------------------*/
{
   float X[DUMMYARRAYLEN], Y[DUMMYARRAYLEN];
   float fiedata[MAXVAR*DUMMYARRAYLEN];
   float XX;
   float delta;
   fint  r;
   fint  count;

   
   delta = (minmax[1] - minmax[0]) / (float) DUMMYARRAYLEN;
   XX = minmax[0];
   count = 0;
   while (count < DUMMYARRAYLEN)
   {      
      int   i;
      X[count] = XX;
      for (i = 0; i < npar; i++)
      {
         fiedata[i*DUMMYARRAYLEN+count] = constants[i];
      }
      fiedata[npar*DUMMYARRAYLEN+count] = X[count];         /* Add the X value */
      XX += delta;      
      count++;
   }
   r = fiedo_c( fiedata, &count, Y, &fieid );   
   plotpoints( X, Y, count, color, PLUS, LINES );
}



extern float func_c( float *xdat,
                     float *fpar,
                     fint  *npar,
                     fint  *fieid )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate a function value for the user given     */
/*          expression.                                       */
/* The constants that we want to fit are entered in 'fpar'.   */
/* Only one point per call is calculated.                     */
/*------------------------------------------------------------*/
{
   float result;
   int   i;
   fint  r;
   fint  datalen = 1;
   float datain[MAXVAR];
   
   for (i = 0; i < *npar; i++)            /* First enter all fitted constants */
      datain[i] = fpar[i];
   datain[*npar] = xdat[0];                         /* Then enter the X value */
   /* Take the function id of the user expression */
   r = fiedo_c( datain, &datalen, &result, &fieid[0] );         /* Y = result */

   return( result );      
}



extern void derv_c( float *xdat, 
                    float *fpar, 
                    float *epar, 
                    fint  *npar,  
                    fint  *fieid )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate values for all partial derivatives.     */
/* Note that the function id's for these derivatives start    */
/* with index '1', because element '0' is the function id of  */
/* the user given expression.                                 */
/*------------------------------------------------------------*/
{
   int   i;
   fint  r;
   fint  datalen = 1;
   float datain[MAXVAR];
   
   for (i = 0; i < *npar; i++)
      datain[i] = fpar[i];
   datain[*npar] = xdat[0];                              /* Last value is 'x' */
   
   for (i = 0; i < *npar; i++)
      r = fiedo_c( datain, &datalen, &epar[i], &fieid[i+1] );   /* Note i+1 ! */
}




static void clearfunctions( int     ndervs,
                            fint   *fieid )
/*------------------------------------------------------------*/  
/* PURPOSE:  Clears code previous generated by 'fieini'.      */
/*------------------------------------------------------------*/
{
   int   i;
   
  
   fieclr_c( &fieid[0] );                                  /* Main expression */
   for (i = 0; i < ndervs; i++)         /* The partial derivative expressions */
   {
      fieclr_c( &fieid[i+1] );                                  /* Note index */
   }
}



static fint getexpr( fchar  ExpressionI, 
                     fchar  ExpressionO, 
                     fchar *Partdervs,
                     fchar *Variables,
                     char  *key,
                     char  *mes,
                     fint  *fieid )
/*------------------------------------------------------------*/
/* PURPOSE: Ask user expression of the function that he wants */
/*          to fit.                                           */
/* The string is converted to uppercase and parsed to 'dydx'  */
/* which returns all parameters in the expression, except the */
/* real variable 'X'. For these parameters the symbolic       */
/* partial derivatives are constructed and stored for futher  */
/* use in 'derv' which is part of the least squares fit       */
/* routine.                                                   */
/*------------------------------------------------------------*/
{
   char   dummyvarbuf[MAXVAR*MAXVARLEN];
   fchar  Dummvariables[MAXVAR];
   fint   dfault = NONE;
   fint   r;
   fint   csize = MAXVARLEN;
   fint   nvars, pvars;
   fint   errpos;
   fint   len;
   int    n;

              
   len = usertext_c( ExpressionI, &dfault, tofchar("FX="), tofchar("Enter expression:") );
   for (n = 0; n < len; n++)
      ExpressionI.a[n] = toupper( ExpressionI.a[n] );
   ExpressionI.a[len] = '\0'; 

   r = dydx_c( ExpressionI,     /* expression to be differentiated. */
               tofchar("X"),    /* variable with respect to which will be differentiated. */
               ExpressionO,     /* DYDX's interpretation of ExpressionI */ 
               Partdervs[0],    /* derivative of ExpressionI with respect to 'X'.*/
               Variables[0],    /* names of parameters appearing in ExpressionI (in any order).*/
               &csize,          /* Max. size of a parameter string */
               &nvars );        /* Number of parameters found. */
               
   if (r != 0)
   {
      dydxerror( r );
      return( -1 );
   }
   anyoutf( 16, "DYDX: r=%d, nvars = %d Out. exprs.: [%.*s]", r, nvars,
            nelc_c(ExpressionO), ExpressionO.a );

   /*--------------------------------------------------*/
   /* 'nvars' is the number of variables given by the  */
   /* user. The variable 'X" is also part of the       */
   /* variables set and is added by the program.       */
   /*--------------------------------------------------*/   
   str2char( "X", Variables[nvars] );   
   pvars = nvars + 1;
   r = fiepar_c( Variables[0], &pvars );       /* All variables including 'X' */
   if (r != 0)
   {
      anyoutf( 1, "FIEPAR: Too many parameters in expression or other error" );
      return( -1 );
   }
   /*--------------------------------------------------*/          
   /* Parse the string parsed by 'dydx' to 'fieini'    */
   /* for storage and future evaluation.               */
   /*--------------------------------------------------*/
   errpos = 0;
   pvars  = fieini_c( ExpressionO, &fieid[0], &errpos ); 
   if (pvars < 0)
   {
      inifieerror( pvars, errpos );
      return( -1 );
   }
   
      
   for (n = 0; n < nvars; n++)
   {
      Dummvariables[n].a = &dummyvarbuf[n*MAXVARLEN];
      Dummvariables[n].l = MAXVARLEN;
   }
   for (n = 0; n < nvars; n++) 
   {           
      r = dydx_c( ExpressionI,
                  Variables[n],
                  ExpressionO,
                  Partdervs[n],
                  Dummvariables[0],
                  &csize,
                  &pvars ); 
      if (r != 0)
      {
         dydxerror( r );
         errorf( 4, "FIT: Must abort....." );
      }                        
      anyoutf( 16, "DYDX: DY/D[%.*s] = [%.*s]",  
               nelc_c(Variables[n] ), Variables[n].a,
               nelc_c(Partdervs[n] ), Partdervs[n].a );
      errpos = 0;
      pvars  = fieini_c( Partdervs[n], &fieid[n+1], &errpos ); /* Note index! */
      if (pvars < 0)
      {
         inifieerror( pvars, errpos );
         errorf( 4, "FIT: Must abort....." );
      }
   }
   if (nvars == 0)
   {
      anyoutf( 1, "FIT: There are no parameters to fit..." );
      clearfunctions( nvars, fieid );
      return( -1 );
   }
   return( nvars );
}


   
static fint getdata( float *data,
                     fint   maxlen,
                     char  *key,
                     char  *mes,
                     fint   dfault )
/*------------------------------------------------------------*/
/* PURPOSE: Enter array with data for the fit.                */
/*------------------------------------------------------------*/
{
   return( userreal_c( data, &maxlen, &dfault, tofchar(key), tofchar(mes) ) );
}
   


static void clearstr( char *str,
                      int   len )                     
/*------------------------------------------------------------*/
/* PURPOSE: Avoid garbage in strings.                         */
/*------------------------------------------------------------*/
{
   int i;
   for (i = 0; i < len; i++)
      str[i] = ' ';
}



static void getiniest( fchar  *Variables, 
                       int     nest,
                       float  *estimates )
/*------------------------------------------------------------*/
/* PURPOSE: Ask user an initial estimate for ach parameter.   */
/* The keywords are generated by the program. The get the     */
/* name of each parameter of which we want an initial         */
/* estimate.                                                  */
/*------------------------------------------------------------*/
{
   int    n;
   fint   nitems, dfault;

   
   for (n = 0; n < nest; n++)
   {
      char mes1[80], mes2[80];
      Variables[n].a[nelc_c(Variables[n])] = '\0';
      sprintf( mes2, "Enter estimate for [%s]:     [0.0]", Variables[n].a  );
      nitems = 1;
      dfault = REQUEST;
      estimates[n] = 0.0;
      sprintf( mes1, "%s=", Variables[n].a );
      (void) userreal_c( &estimates[n], &nitems, &dfault, tofchar(mes1), 
                         tofchar(mes2) );             
   }   
}



static void getlsqpars( char   *tolkey, 
                        char   *tolmes,
                        char   *itskey,
                        char   *itsmes,
                        char   *labkey,
                        char   *labmes,
                        float  *tol,
                        fint   *maxits,
                        float  *lab )
/*------------------------------------------------------------*/
/* PURPOSE: Ask user 'lsqfit' parameters tolerance mixing par.*/
/*          and max. number of iterations.                    */
/*------------------------------------------------------------*/
{
   fint   dfault;
   fint   nitems = 1;
   
  
   dfault = HIDDEN;
   *tol = 0.001;
   (void) userreal_c( tol, &nitems, &dfault, tofchar(tolkey), tofchar(tolmes) );
   *tol = ABS( *tol );
   *lab = 0.001;
   (void) userreal_c( lab, &nitems, &dfault, tofchar(labkey), tofchar(labmes) );
   *lab = ABS( *lab );
   *maxits = 100;
   (void) userint_c( maxits, &nitems, &dfault, tofchar(itskey), tofchar(itsmes) );
   *maxits = ABS( *maxits );   
}



static void logfile( fchar     Expression, 
                     fchar    *Variables, 
                     float    *estimates, 
                     float    *epar, 
                     fint      maxpar, 
                     fint      iters,
                     float     tol,
                     float     lab,
                     int       alen,
                     float     chi2,
                     float     chi2prob,
                     FILE      *fp )
/*------------------------------------------------------------*/
/* PURPOSE: Generate output.                                  */
/*------------------------------------------------------------*/
{
   int    n;
   char   datstr[2048];
   static int first = YES;
  
   datstr[0] = '\0';   /* Clear */
   anyoutf( 1, " " );
   anyoutf( 1, "===============================RESULTS============================" );
   anyoutf( 1, "Fitted to %d data points was FX = %s", alen, Expression.a ); /* Was null terminated */
   anyoutf( 1, "The fitted parameters: ");
   
  
   if (fp != NULL && first)                  /* Output the var names to file only once */
   { 
      for (n = 0; n < maxpar; n++)
      {
         sprintf( datstr, "%s %-10s d%-10s", datstr, Variables[n].a, Variables[n].a );
      }
      first = NO;      
      (void) fprintf( fp, "!%s\n", datstr );    
   }    
 
   datstr[0] = '\0';   /* Clear */
   for (n = 0; n < maxpar; n++)
   {
      anyoutf( 1, "%s = %g  +/-  %g", Variables[n].a, estimates[n], epar[n] );
      if (fp != NULL)
      {
         sprintf( datstr, "%s %10g %10g", datstr, estimates[n],  epar[n] );
      }
   }
   if (fp != NULL)
   {
      (void) fprintf( fp, "%s\n", datstr );    
   }
   anyoutf( 1, " " );
   anyoutf( 1, "The least squares fit converged to TOL=%g in %d iterations, ", tol, iters );
   anyoutf( 1, "using a mixing parameter LAB=%g.", lab ); 
   anyoutf( 1, "Chi-squared: %g, reduced Chi-squared: %g", 
                chi2, chi2/(float)(alen-maxpar-1) );
                
   if (chi2prob == blank)
      anyoutf( 1, "Program could not derive a goodness of fit probability" );
   else
   {
      anyoutf( 1, "Probability that a RANDOM set of %d data points would yield", alen );
      anyoutf( 1, "a value of Chi-squared >= %g when compared to the parent", chi2 );
      anyoutf( 1, "function is %f", 1.0 - chi2prob );
   }
   anyoutf( 1, " " );
   anyoutf( 1, "You can rerun this with new values for FX=, TOLERANCE=, MAXITS=," );
   anyoutf( 1, "LAB= and new initial estimates." );
   anyoutf( 1, "==================================================================" );
   anyoutf( 1, " " );
}



static bool wantexit( char *key,
                      char *mes )
/*------------------------------------------------------------*/
/* PURPOSE: Confirm user's exit.                              */
/*------------------------------------------------------------*/ 
{
   bool   quit;
   fint   nitems, dfault;
   
   quit   = toflog( NO );
   dfault = REQUEST;
   nitems = 1;
   (void) userlog_c( &quit, &nitems, &dfault, tofchar(key), tofchar(mes) );
   cancel(key);                     
   quit = tobool( quit );
   return( quit );
}



static bool clearplot( char *key,
                       char *mes )
/*------------------------------------------------------------*/
/* PURPOSE: Ask confirmation to clear plot.                   */
/*------------------------------------------------------------*/ 
{
   bool   clear;
   fint   nitems, dfault;
   
   clear  = toflog( NO );
   dfault = HIDDEN;
   nitems = 1;
   (void) userlog_c( &clear, &nitems, &dfault, tofchar(key), tofchar(mes) );
   clear = tobool( clear );
   return( clear );
}



static void subtract( float   *X,
                      float   *Y,
                      float   *wdat,
                      float   *diffY,
                      fint     alen,
                      float   *constants,
                      fint     npar,
                      fint     fieid,
                      float   *chi2 )
/*------------------------------------------------------------*/
/* PURPOSE: Ask confirmation to clear plot.                   */
/*------------------------------------------------------------*/ 
{
   float  fiedata[MAXARRAYLEN*(MAXVAR+1)];
   float  fx[MAXARRAYLEN];
   int    count, i;
   
  
   *chi2 = 0.0;
   for (count = 0; count < alen; count++)
   {
      for (i = 0; i < npar; i++)  
      {
         fiedata[i*alen+count] = constants[i];
      }
      fiedata[npar*alen+count] = X[count];
   }
   (void) fiedo_c( fiedata, &alen, fx, &fieid );
   for (count = 0; count < alen; count++)
   {
      if (wdat[count] > 0.0)
      {
         diffY[count] = Y[count] - fx[count]; 
         *chi2 += diffY[count] * diffY[count] * wdat[count];
      }
      else
         diffY[count] = blank;
   }
}



static void  cursorinteraction( char   *key,
                                char   *mes,
                                float  *X, 
                                float  *Y,                                 
                                float  *wdat, 
                                float  *curwdat,                                 
                                fint    ndat )
/*------------------------------------------------------------*/
/* PURPOSE: Set weights using the graphics cursor.            */
/*------------------------------------------------------------*/
{
   bool   cursor;
   fint   nitems, dfault;

   
   cursor = toflog( NO );
   dfault = REQUEST;
   nitems = 1;
   (void) userlog_c( &cursor, &nitems, &dfault, tofchar(key), tofchar(mes) );
   cursor = tobool( cursor );   
   if (cursor)   
   {
      fchar   Ch;
      int     exit = NO;
      float   xpos, ypos;
      float   delta = ABS(X[ndat-1] - X[0]) / 10.0;
      
      fmake( Ch, 1 );
      xpos = X[0];
      ypos = Y[0];
      status_c( tofchar("Left mouse button=mark/unmark, Q=Quit") );      
      while (!exit)
      {
         char    upchr;
         pgcurs_c( &xpos, &ypos, Ch );
         upchr = toupper(Ch.a[0]); 
         exit = (upchr == 'Q');
         if (upchr == '1')
         {
            int i;
            if (xpos >= X[0] - delta && xpos <= X[ndat-1] + delta)
            {
               float mindist = ABS( X[ndat-1] - X[0] );
               int   minindx = -1;
               for (i = 0; i < ndat; i++)
               {
                  float dist = ABS( X[i] - xpos );
                  if (dist < mindist)
                  {
                     mindist = dist;   
                     minindx = i;
                  }                  
               }
               if (minindx != -1)
               {
                  if (curwdat[minindx] == 0.0)
                  {
                     curwdat[minindx] = wdat[minindx];
                     plotpoints( &X[minindx], &Y[minindx], 1, YELLOW, PLUS, POINTS );
                  }
                  else
                  {
                     curwdat[minindx] = 0.0;
                     plotpoints( &X[minindx], &Y[minindx], 1, GREEN, PLUS, POINTS );
                  }                  
               }
            }
         }
      }
      status_c( tofchar("......") );      
   }
}                                



static void getyerrorrange( float *Y1,
                            float *Y2,
                            char  *key )
/*------------------------------------------------------------*/
/* PURPOSE:                    */
/*------------------------------------------------------------*/
{
   char    mess[256];
   fint    nitems = 2;
   fint    dfault = HIDDEN;
   fint    r;
   float   yrange[2];
            
           
   yrange[0] = *Y1;
   yrange[1] = *Y2;
   sprintf( mess, "Enter Y range for residues:    [%g %g]",
            yrange[0], yrange[1] );
   r = userreal_c( yrange, &nitems, &dfault, tofchar(key),
                   tofchar(mess) );
   *Y1 = yrange[0];
   *Y2 = yrange[1];
}




MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint     alen, wlen;
   bool     quit;
   float    minmax[4];
   float    minmaxsub[4];

   /* 'lsqfit' related */

   float    tol;
   float    lab;
   fint     maxits;
   fint     mpar[MAXVAR];
   float    estimates[MAXVAR];
   float    epar[MAXVAR];
   fint     xdim;
   fint     iters;
   int      n;
   float    chi2, chi2prob;
   


   /* Expression evaluation */
   
   char     varbuf[MAXVAR*MAXVARLEN];
   fchar    Variables[MAXVAR];
   char     derivates[MAXVAR*MAXDERVLEN];
   fchar    Partdervs[MAXVAR];
   char     expressionI[MAXEXPRLEN];        
   fchar    ExpressionI;                 /* User given expression */
   char     expressionO[MAXEXPRLEN];
   fchar    ExpressionO;                 /* Input expression processed by 'dydx' */
   fint     nvars;
   fint     functionid[MAXVAR];          /* Number 0 is the function itself, the */
                                         /* others are the partial derivatives. */
 

   init_c();                             /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;              /* Name of current task */
      fmake( Task, 20 );                 /* Macro 'fmake' must be available */
      myname_c( Task );                  /* Get task name */
      Task.a[nelc_c(Task)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, RELEASE ); /* Show task and version */
   }
   setfblank_c( &blank );


   /* Prepare fchars */
   for (n = 0; n < MAXVAR; n++)
   {
      Variables[n].a = &varbuf[n*MAXVARLEN];
      Variables[n].l = MAXVARLEN;
   }
   ExpressionI.a = expressionI;
   ExpressionI.l = MAXEXPRLEN;
   ExpressionO.a = expressionO; 
   ExpressionO.l = MAXEXPRLEN;
   for (n = 0; n < MAXVAR; n++)
   {
      Partdervs[n].a = &derivates[n*MAXDERVLEN];
      Partdervs[n].l = MAXDERVLEN;
   }

   /* Prepare for lsqfit */
   xdim = 1;                                             /* Dimension of data */
   for (n = 0; n < MAXVAR; n++)                        /* All free parameters */
      mpar[n] = 1;


   alen = getdata( X, MAXARRAYLEN, "X=", "Enter X values:", NONE );
   sprintf( message, "Enter %d Y values:", alen );
   alen = getdata( Y, alen, "Y=", message, EXACT );   


   fp = fopenC( filename );
   
   initplot();   
   pgpage_c();
   
   minmax1_c( X, &alen, &minmax[0], &minmax[1] );
   minmax1_c( Y, &alen, &minmax[2], &minmax[3] );   
   
   anyoutf( 16, "DEBUG: range in Y: %f, %f", minmax[2], minmax[3] );
   do
   {
      static int first = YES;      
      int        ok;      

      wlen = getdata( wdat, alen, 
                     "WEIGHTS=", 
                     "Enter weights (W_i=1/VAR_i):      [1.0 for all]", 
                      REQUEST );
      /* Fill missing weights with 1.0 */
      if (wlen < alen)
      {
         for (n = wlen; n < alen; n++)
            wdat[n] = 1.0;
      } 
      if (first)
      {
         for (n = 0; n < alen; n++)
            curwdat[n] = wdat[n];      
         first = NO;                     
      }
      setviewport( 0.2, 0.4, 0.95, 0.95 );                      /* Upper panel */
      drawbox( minmax[0], minmax[2], minmax[1], minmax[3], 
               "", "Y", "", 
               NO,
               FOREGROUND );
      plotpoints( X, Y, alen, YELLOW, PLUS, POINTS );
      for (n = 0; n < alen; n++)
      {
         if ( (curwdat[n] == 0.0 || wdat[n] == 0.0) && Y[n] != blank)
            plotpoints( &X[n], &Y[n], 1, GREEN, PLUS, POINTS );
      }
      clearstr( varbuf, MAXVAR*MAXVARLEN );
      clearstr( expressionI, MAXEXPRLEN );
      clearstr( expressionO, MAXEXPRLEN );
      do
      {
         nvars = getexpr( ExpressionI, 
                          ExpressionO, 
                          Partdervs, 
                          Variables,
                         "FX=", 
                         "Enter expression:", 
                          functionid );
         ok = (nvars >= 0);
         if (!ok)
            reject_c( tofchar("FX="), tofchar("Invalid, try again!") );
      }
      while (!ok);

      /*--------------------------------------------------------------*/
      /* At this moment the total number of variables (inclusive 'X') */
      /* is known. Get initial estimates for all vars. except 'X'     */
      /*--------------------------------------------------------------*/
      getiniest( Variables, nvars, estimates );
   
      /*------------------------------------------------------------*/
      /* Check on blanks in 'Y'. For each blank value the           */
      /* corresponding weight is set to 0. This action must be      */
      /* repeated each time the weights are modified.               */
      /*------------------------------------------------------------*/
      updateweights( X, Y, wdat, alen, estimates, nvars, functionid[0] );
      for (n = 0; n < alen; n++)
      {
         if (curwdat[n] != 0.0)
            curwdat[n] = wdat[n];       
      }

      plotfunction( estimates, nvars, minmax, BLUE, functionid[0] );      

      getlsqpars( "TOLERANCE=",  "Fractional tolerance for chi squared:   [0.001]",
                  "MAXITS=",     "Maximum number of iterations:           [100]",
                  "LAB=",        "Mixing parameter:                       [0.001]", 
                   &tol, 
                   &maxits, 
                   &lab );
                   
      cursorinteraction( "CURSOR=", "Exclude data with cursor?    Y/[N]", 
                         X, Y, wdat, curwdat, alen );                   
                  
      iters = lsqfit_c( X,             /* X-coordinates */
                        &xdim,         /* Dimension of fit */
                        Y,             /* Y data */
                        curwdat,       /* Weights after cursor interaction */
                        &alen,         /* Number of data points */
                        estimates,     /* Initial values/fitted pars.*/
                        epar,          /* Error return values */
                        mpar,          /* All parameters free */
                        &nvars,        /* Total number of parameters */
                        &tol,          /* Tolerance */
                        &maxits,       /* Max. num. of iterations */
                        &lab,          /* Mixing parameter */
                        functionid );  /* Option in func/derv */
      if (iters < 0 )
         errormess( iters );
      else
      {
         static   int first = YES;
         static   int color = RED;
         
         plotfunction( estimates, nvars, minmax, color, functionid[0] );

         /* calculate chi-squared and return diff. array */
         subtract( X, Y, curwdat, diffY, alen, estimates, nvars, functionid[0], &chi2 );

         setviewport( 0.2, 0.1, 0.95, 0.4 );
         minmaxsub[0] = minmax[0];
         minmaxsub[1] = minmax[1];
         if (first)
         {
            minmax1_c( diffY, &alen, &minmaxsub[2], &minmaxsub[3] );
            first = NO;
         }

         getyerrorrange( &minmaxsub[2], &minmaxsub[3], "YERRRANGE=" );
         drawbox( minmaxsub[0], minmaxsub[2], minmaxsub[1], minmaxsub[3], 
                  "X", "Y - Yfit", "", YES, FOREGROUND );         
         plotpoints( X, diffY, alen, color++, PLUS, POINTS );         
         color++;
         if (color == BLUE)      /* Skip blue, reserved for initial estimates */
            color++;

         chi2prob = gammq( (alen-(nvars+1))/2.0, chi2/2.0 );
         logfile( ExpressionI, Variables, estimates, epar, nvars, 
                  iters, tol, lab, alen, chi2, chi2prob, fp );         
      }
      quit = wantexit( "QUIT=", "Want to quit?       Y/[N]" );
      if (!quit)
      {
         clearfunctions( nvars, functionid );
         if ( clearplot("CLEAR=", "Clear plot page?          Y/[N]") )
         {
            pgpage_c();
         }
      }
   }
   while (!quit);
                 
   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   pgend_c();
   if (fp != NULL) 
      fclose( fp );                      /* Close the ASCII file */   
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
