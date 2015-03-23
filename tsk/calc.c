/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             calc.dc1

Program:       CALC

Purpose:       Calculate expressions (using variables) and exercise GIPSY
               input syntax

Category:      CALCULATION, UTILITY

File:          calc.c

Author:        M.G.R. Vogelaar

Keywords:

** VARNAME=    Give variable name(s) to use in expression:          [x]

               Specify the variable names you want to use in the
               expression in EXPRESSION= The maximum number of
               variables is 32. A variable name cannot exceed 16
               characters. Default is one  variable called 'x'.


** FILENAME=   Give name of ASCII file:     [write data to screen only]

               It is possible to write results to an ASCII file also.
               FILENAME= expects the name of your file. The default
               will skip writing to file.
               If a file is opened then it will be closed after the
               loop over EXPRESSION= is aborted.


   OVERWRITE=  File exists, ok to overwrite?                      [Y]/N

               Only prompted if FILENAME= is an existing file.


   EXPRESSION= Enter Hermes expr. or math funct. with var....:  [abort]

               The expression is an expression that can be evaluated
               by Hermes (using Hermes variables) or it is an
               mathematical expression which can contain variables
               that were entered in VARNAME=   (default: 'x').
               If variables are part of the expression, then their
               values must be entered with VALUE= before the
               expression can be evaluated.
               examples:
               EXPRESSION=sin(pi/2)
               EXPRESSION=sin(rad(x))  VALUE=0:30:5
               EXPRESSION=descr(AURORA,CRVAL1)
               EXPRESSION=image(AURORA,-2 -2 2 2)
               EXPRESSION=table(AURORA,xytable,x,1:)
               EXPRESSION=file(xvalues.txt,1,1:10)



   VALUE=      Values x(1..n), ... :                            [abort]

               Enter the value(s) for the variable(s).
               The maximum number of variables is limited
               only by available memory. Alternative input is possible
               with 1) recall file
                    2) file and table functions



Description:   CALC can be used as a calculator in GIPSY and you can also
               test the GIPSY input syntax for numbers. An expression can
               contain variables or it follows the Hermes syntax for
               number input. The calculated values are written to screen
               and to the GIPSY log file (GIPSY.LOG) and to a file
               (FILENAME=).

               If your expression is rejected at first (because it is not
               a valid 'Hermes' expression), it will be treated
               as a mathematical expression which can contain variables.
               The expression is given in a loop in EXPRESSION=
               and the variables that can be used are in VARNAME=
               (the default variable name is x).
               If the expression contains one or more variables then
               you are prompted to enter values for the variable(s)
               with VALUE=  This keyword follows the standard GIPSY
               input syntax for numbers.
               After each evaluation the output is printed on screen
               (and written to file if FILENAME= is used) and the
               program returns to the EXPRESSION= prompt.
               If an ASCII file on disk was opened, it will be closed
               after the EXPRESSION= loop is aborted.

               The input string in EXPRESSION= may contain:

               A) Hermes compatible input:

               1 2 3/3  sin(pi)      evaluates to     1.0 2.0 1.0 0.0
               log(10)::4            evaluates to     1.0 1.0 1.0 1.0
               log(10):log(100):2/4  evaluates to     1.0 1.5 2.0
               10**[0 1 5]           evaluates to     1 10 100000
               [1:3]+[90:70:-10]     evaluates to     91 82 73
               [20:30]?[3 4 5]       evaluates to     22 23 24


               B) Mathematical expressions (with variables), containing:

               1) functions; syntax ff(..); where ff is one of the
                  following available functions:
                  abs(x)         absolute value of x
                  sqrt(x)        square root of x
                  sin(x)         sine of x
                  asin(x)        inverse sine of x
                  cos(x)         cosine of x
                  acos(x)        inverse cosine of x
                  tan(x)         tangent of x
                  atan(x)        inverse tan of x
                  atan2(x,y)     inverse tan (mod 2pi) x = sin, y = cos
                  exp(x)         exponential of x
                  ln(x)          natural log of x
                  log(x)         log (base 10) of x
                  sinh(x)        hyperbolic sine of x
                  cosh(x)        hyperbolic cosine of x
                  tanh(x)        hyperbolic tangent of x
                  rad(x)         convert x to radians
                  deg(x)         convert x to degrees
                  erf(x)         error function of x
                  erfc(x)        1-error function
                  max(x,y)       maximum of x and y
                  min(x,y)       minimum of x and y
                  sinc(x)        sin(x)/x (sinc function)
                  sign(x)        sign of x (-1,0,1)
                  mod(x,y)       gives remainder of x/y
                  int(x)         truncates to integer
                  nint(x)        nearest integer
                  ranu(x,y)      generates uniform noise between x and y
                  rang(x,y)      generates gaussian noise with mean x
                                 and dispersion y
                  ranp(x)        generates poisson noise with mean x
                  ifeq(x,y,a,b)  returns a if x == y, else b
                  ifne(x,y,a,b)  returns a if x != y, else b
                  ifgt(x,y,a,b)  returns a if x > y, else b
                  ifge(x,y,a,b)  returns a if x >= y, else b
                  iflt(x,y,a,b)  returns a if x < y, else b
                  ifle(x,y,a,b)  returns a if x <= y, else b
                  ifblank(x,a,b) returns a if x == BLANK, else b
               2) constants; syntax cc; where cc is one of the following
                  available constants:
                  PI             3.14159....
                  C              speed of light (SI)
                  H              Planck (SI)
                  K              Boltzmann (SI)
                  G              gravitation (SI)
                  S              Stefan-Boltzman (SI)
                  M              mass of sun (SI)
                  P              parsec (SI)
                  BLANK          Universal undefined value
                  Note: the Hubble constant is not included.
               3) operators; syntax op; where op is one of the following
                  available operators:
                  +              addition
                  -              subtraction
                  *              multiplication
                  /              division
                  **             power
               4) parameters; up to 32 variable names (less than 16 characters)
                  can be specified.

Notes:

Example:       example 1: Calculate sin(x) for x=0, 5, ..., 30
               <USER> calc
               <USER> CALC EXPRESSION=sin(rad(x))
               <USER> CALC VALUE=0:30:5

               !x         sin(rad(x))
               !====================
               0         +0
               5         +0.0871557
               10        +0.173648
               15        +0.258819
               20        +0.34202
               25        +0.422618
               30        +0.5


               example 2: Calculate 2/6 3/6 4/6 and 5/6 using the
                          expression x/y
                          Note the order of the input in VALUE=
               <USER> calc varname=x y
               <USER> CALC EXPRESSION=x/y
               <USER> CALC VALUE=2 3 4 5 6::4

               !x         y         x/y
               !==============================
               2         6         +0.333333
               3         6         +0.5
               4         6         +0.666667
               5         6         +0.833333

               Note: First the four x values are given and second the
               four y values.


               example 3: Calculate 2/6 3/6 4/6 and 5/6 using the
                          expression x/y and a recall file!
               <USER> calc varname=x y
               <USER> CALC EXPRESSION=x/y
               <USER> CALC VALUE=<recallfile

               !x         y         x/y
               !==============================
               2         6         +0.333333
               3         6         +0.5
               4         6         +0.666667
               5         6         +0.833333

               The contents of 'recallfile.rcl' is:
               2 6
               3 6
               4 6
               5 6


               example 4:

               CALC EXPRESSION=descr(AURORA,CRVAL1)

               Returns the header value of CRVAL1 at set level of set
               AURORA.


               example 5:

               CALC EXPRESSION=image(AURORA,-2 -2 2 2)

               Returns all image values in AURORA in a box with corners
               x,y=-2 -2 and x,y=2 2


               example 6:

               CALC EXPRESSION=table(AURORA,xytable,x,1:)

               Reads column with name 'x' from table 'xytable' in set
               AURORA at set level. All column data (1:) will be displayed.


               example 7:

               CALC EXPRESSION=file(xvalues.txt,1,1:10)

               Returns from ASCII file 'xvalues.txt' the first 10 values.



Updates:       Jan 4,  1993: VOG, Document created.
               Jul 20, 1995: VOG, DECODE function included.

#<
*/

/*  calc.c: include files     */

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
#include    "userfio.h"
#include    "setfblank.h"    /* Subroutine to set a data value to the universal BLANK.*/
#include    "setdblank.h"    /* Subroutine to set a double to the universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/

/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "userchar.h"
#include    "decodedble.h"
#include    "decodereal.h"
#include    "dcderrstr.h"    /* Obtain an error message, given a DECODExxx error */
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/

/* Expression evaluation */

#include    "fieini.h"
#include    "fiedo.h"
#include    "fiepar.h"




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

/* Malloc version of 'fmake'  */
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

#define RELEASE        "1.0"           /* Version number */
#define STRLEN         80              /* Max length of strings */
#define LONGSTR        180
#define KEYLEN         20              /* Max length of keywords */
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0
#define MAXVARS        32              /* Maximum number of vars. in expression */
#define MAXVARLEN      16              /* Max. length of var. name (restricted by 'fiepar') */
#define STARTBUF       1024            /* Initial size of 'fie' arrays */
#define BUFFEROVERFLOW -23             /* Return code for dcd*** functions */
#define FWIDTH         14              /* Width in number formats */


#define KEY_FILENAME   tofchar("FILENAME=")
#define KEY_OVERWRITE  tofchar("OVERWRITE=")




/* Global variables */

static  float    blank;
static  double   dblank;
static  fchar    Varstr;
static  fchar    Key;
static  fchar    Mes;
static  fchar    Expression;
static  char     message[LONGSTR];
static  bool     agreed;
static  int      newexpression = YES;
FILE             *fp = NULL;                 /* File pointer to ASCII file */




static void dcderror( fint dest,
                      fint err )
/*------------------------------------------------------------*/
/* PURPOSE: Print an error message for the 'dcd' functions.   */
/*------------------------------------------------------------*/
{
   fchar  Errstr;
   fmake( Errstr, 40 );
   dcderrstr_c( Errstr, &err );
   anyoutf( dest, "DCD error: %.*s", nelc_c(Errstr), Errstr.a );
}




static FILE *getfileptr( void )
/*------------------------------------------------------------*/
/* PURPOSE: Ask user name of an ASCII file.                   */
/*                                                            */
/* Check whether the file already exists. If so, Ask for      */
/* permission to overwrite. Return a file pointer. This file  */
/* pointer is NULL if no file is wanted.                      */
/*------------------------------------------------------------*/
{
   FILE   *fp = NULL;
   fint   dfault, nitems;
   fint   r1;
   int    agreed;
   bool   overwrite;
   fchar  Filename;


   fmake( Filename, 512 );
   do
   {
      dfault    = HIDDEN;
      overwrite = toflog(YES);
      nitems    = 1;
      r1 = userchar_c( Filename,
                       &nitems,
                       &dfault,
                       KEY_FILENAME,
                       tofchar("Give name of ASCII file:     [No file]") );

      if (!r1)
      {
         fp = NULL;
         agreed = YES;
      }
      else
      {
         Filename.a[nelc_c(Filename)] = '\0';
         fp = fopen( Filename.a, "r" );
         if (fp != NULL)                       /* The file exists */
         {
            nitems = 1;
            dfault = REQUEST;
            r1 = userlog_c( &overwrite,
                            &nitems,
                            &dfault,
                            KEY_OVERWRITE,
                            tofchar("File exists, ok to overwrite?    [Y]/N") );

            fclose( fp );
            cancel_c( KEY_OVERWRITE );
         }
         if (!overwrite)
         {
            cancel_c( KEY_FILENAME );
            agreed = NO;
         }
         else
         {
            fp = fopen( Filename.a, "w" );
            agreed = (fp != NULL);
            if (!agreed)
               reject_c( KEY_FILENAME, tofchar("Cannot open, try another!") );
         }
      }
   }
   while (!agreed);
   return( fp );
}




static void calculate( fchar Varstr, fint npars, fint fieid )
/*------------------------------------------------------------*/
/* PURPOSE: Get 'FIE" data and print results.                 */
/* Loop is aborted with carriage return.                      */
/*------------------------------------------------------------*/
{
   int       i, j, k;
   char      varstr[MAXVARLEN+1];
   fint      r1;
   char      longmes[LONGSTR];
   int       len, totlen;
   fint      dfault;
   bool      docalc;
   fint      ndat, datalen;
   fint      num;
   int       morespace;
   fchar     Key, Mes;
   char      varmes[STRLEN];
   float     *datain = NULL;
   float     *dataout = NULL;


   if (npars == 0)
   {
      /* There are no variables in the expression */
      float dummy = 0.0;
      datalen = 1;
      r1 = fiedo_c( &dummy,
                    &datalen,
                    &dummy,
                    &fieid );
      if (r1 ==  0) {
         if (dummy != blank) {
            (void) sprintf( longmes, "Result: %g", dummy );
         } else {
            (void) sprintf( longmes, "Result: Not defined" );
         }
         anyoutf( 3, longmes );
      } else if (r1 == -1) {
         anyoutf( 1, "id out of range" );
      } else if (r1 == -2) {
         anyoutf( 1, "no code generated" );
      }
      return;
   }

   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   totlen = 0;
   strcpy( varmes, "Values" );
   for (i = 0; i < npars; i++)
   {
      for (j = 0, len = 0; j < MAXVARLEN; j++)
      {
         char     ch = Varstr.a[i*MAXVARLEN+j];
         if (ch != ' ')
         {
            varstr[j] = ch;
            len++;
         } else
         {
            varstr[j] = '\0';
         }
      }
      if ( (totlen + len + 3) < STRLEN )
      {
         totlen = sprintf( varmes, "%.*s %s(1..n)",
                           strlen(varmes), varmes,
                           varstr );
      }
      if (i < npars - 1)
         strcat( varmes, "," );
   }
   strcat( varmes, ":          [abort]" );
   Key    = tofchar("VALUE=");
   Mes    = tofchar( varmes );
   ndat   = STARTBUF;
   datain = (float *) calloc( STARTBUF, sizeof(float) );
   if (datain == NULL)
       errorf( 4, "('datain') Cannot allocate memory for %d floats!", ndat );
   anyoutf( 16, "('datain') Allocated memory for %d floats!", ndat );

   do
   {
      do
      {
         /* Ask values */
         fchar  Values;
         fmake( Values, 1024 );
         dfault = REQUEST;
         if ( usertext_c(Values, &dfault, Key, Mes) )
            num = decodereal_c( Values, datain, &ndat );
         else
            num = 0;

         morespace = (num == BUFFEROVERFLOW);
         while (morespace)
         {
            ndat *= 2;
            datain = realloc( datain, ndat*sizeof(float) );
            if (datain == NULL)
               errorf( 4, "('datain') Cannot allocate memory for %d floats!", ndat );
            anyoutf( 16, "('datain') Reallocated memory for %d floats!", ndat );
            num = decodereal_c( Values, datain, &ndat );
            morespace = (num == BUFFEROVERFLOW);
         }

         agreed = YES;
         if (num < 0)
         {
            fchar  Errstr;
            fmake( Errstr, 40 );
            dcderrstr_c( Errstr, &num );
            Errstr.l = nelc_c( Errstr );
            reject_c( Key, Errstr );
            agreed = NO;
         }
         else
         {
            ndat = num;
            if (npars == 0)
               agreed = YES;
            else
               agreed = ((ndat%npars == 0) || (ndat == 0));
            if (!agreed)
            {
               if (ndat%npars != 0)
                  reject_c( Key, tofchar("All vars. need same num. of entries") );
            }
         }
         cancel_c( Key );
      }
      while (!agreed);

      if (npars == 0)
         datalen = ndat;
      else
         datalen = ndat/npars;

      dataout = (float *) calloc( datalen, sizeof(float) );
      if (dataout == NULL)
         errorf( 4, "('dataout') Cannot allocate memory for %d floats!", datalen );
      anyoutf( 16, "('dataout') Allocated memory for %d floats!", datalen );

      docalc = (ndat != 0);
      if (docalc)
      {
         r1 = fiedo_c( datain,
                       &datalen,
                       dataout,
                       &fieid );
         if (r1 == -1) reject_c( Key, tofchar("id out of range") );
         if (r1 == -2) reject_c( Key, tofchar("no code generated") );
         if (r1 ==  0)
         {
            int slen = 0;
            if (newexpression)
            {
               strcpy( longmes, "!" );    /* File comment */
               for (i = 0; i < npars; i++)
               {
                  for (j = 0, len = 0; j < MAXVARLEN; j++)
                  {
                     char     ch = Varstr.a[i*MAXVARLEN+j];
                     if (ch != ' ')
                     {
                        varstr[j] = ch;
                        len++;
                     }
                     else
                        varstr[j] = '\0';
                  }
                  sprintf( message, "%-*.*s", FWIDTH, FWIDTH, varstr );
                  strcat( longmes, message );
               }
               newexpression = NO;
               sprintf( message, "%.*s", nelc_c(Expression), Expression.a );
               strcat( longmes, message );
               anyoutf( 3, longmes );
               if (fp != NULL)
                  fprintf( fp, "%s\n", longmes );   /* File comment */
               slen = (npars+1) * FWIDTH;
               memset( longmes, '=', slen );
               longmes[0] = '!';
               longmes[slen] = '\0';
               anyoutf( 3, longmes );
               if (fp != NULL)
                  fprintf( fp, "%s\n", longmes );   /* File comment */
            }

            for (k = 0; k < datalen; k++)
            {
               strcpy( longmes, " " );
               for (i = 0; i < npars; i++)
               {
                  if (datain[i*datalen+k] != blank)
                     sprintf( message, "%-*g", FWIDTH, datain[i*datalen+k] );
                  else
                     sprintf( message, "%*.*s", FWIDTH, FWIDTH, "blank" );
                  strcat( longmes, message );
               }
               if (dataout[k] != blank)
                  sprintf( message, "%+g", dataout[k] );
               else
                  sprintf( message, "%s", "Not defined" );
               strcat( longmes, message );
               anyoutf( 3, longmes );
               if (fp != NULL)
                  fprintf( fp, "%s\n", longmes );
            }
         }
      }
      free( dataout );
      anyoutf( 16, "Freeing 'dataout'" );
   } while (docalc);
   free( datain );
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint        dfault;
   fint        nitems;
   fint        nvars;
   fint        npars = 0;
   int         i;
   fint        functionid;
   fint        retcode;
   bool        nextexpr;


   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      fchar  Task;                         /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      (void) myname_c( Task );             /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );
   setdblank_c( &dblank );


   /*---------------------------------------------------------------*/
   /* To evaluate the expression given in EXPRESSION= you need to   */
   /* specify the names of the variables to use and the number of   */
   /* variables in the expression. The default is one variable 'x'. */
   /*---------------------------------------------------------------*/

   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   fmake( Varstr, MAXVARLEN*MAXVARS );         /* Create space for 'MAXVARS' strings */
   Varstr.l = MAXVARLEN;                       /* Length of the sub strings */
   do {
      strcpy( Varstr.a, "x" );                 /* Default var. is 'x' */
      dfault = HIDDEN;
      nitems = MAXVARS;
      Key    = tofchar("VARNAME=");
      Mes    = tofchar("Give variable name(s) to use in expression:   [x]");
      nvars  = userchar_c( Varstr,
                           &nitems,
                           &dfault,
                           Key,
                           Mes );
      if (nvars == 0) nvars = 1;                /* The default */
      retcode = fiepar_c( Varstr, &nvars );
      agreed = (retcode == 0);
      if (!agreed) {
         reject_c( Key, tofchar("Too many parameters") );
      }
   } while (!agreed);


   fp = getfileptr();                           /* Open a file on disk */


   /*--------------------------------------------------------------*/
   /* Ask user to give an expression. If no expression was entered */
   /* abort program.                                               */
   /*--------------------------------------------------------------*/

   fmake( Expression, LONGSTR );
   /* Start loop over expressions until <enter> is pressed */
   do
   {
      bool    exprok;
      fint    errpos;
      char    varstr[MAXVARLEN+1];
      int     j;

      /* Prepare a prompt */
      Key = tofchar("EXPRESSION=");
      newexpression = YES;
      strcpy( message, "Expr. or math. funct. with var. " );
      for (i = 0; i < nvars; i++)
      {
         for (j = 0; j < MAXVARLEN; j++)
         {
            char     ch = Varstr.a[i*MAXVARLEN+j];
            if (ch != ' ')
               varstr[j] = ch;
            else
               varstr[j] = '\0';
         }
         if (i < (nvars - 1))
         {
            sprintf( message, "%.*s%s,",
                     strlen(message), message,
                     varstr );
         }
         else
         {
            sprintf( message, "%.*s%s:  [abort]",
                     strlen(message), message,
                     varstr );
         }
      }

      /* Ask the expression */
      dfault = REQUEST;
      Mes = tofchar(message);
      retcode  = usertext_c( Expression,
                             &dfault,
                             Key,
                             Mes );
      nextexpr = (retcode != 0);

      /* Process the expression or abort */
      if (nextexpr)
      {
         double    *X = NULL;
         fint      ndata = STARTBUF;
         fint      num;
         int       morespace;
         int       i;


         /* Try Hermes input first */
         X = (double *) calloc( ndata, sizeof(double) );
         if (X == NULL)
            errorf( 4, "Cannot allocate memory for %d floats!", ndata );

         num = decodedble_c( Expression, X, &ndata );
         morespace = (num == BUFFEROVERFLOW);
         while (morespace)
         {
            ndata *= 2;
            X = realloc( X, ndata*sizeof(double) );
            if (X == NULL)
               errorf( 4, "('X') Cannot allocate memory for %d floats!", ndata );
            anyoutf( 16, "('X') Reallocated memory for %d floats!", ndata );
            num = decodedble_c( Expression, X, &ndata );
            morespace = (num == BUFFEROVERFLOW);
         }

         /* Give dcd error only in debug mode */
         if (num < 0)
         {
            dcderror( 16, num );
            anyoutf( 16, "Try evaluating it as a mathematical expression!" );
         }
         if (num > 0)
         {
            for (i = 0; i < num; i++)
            {
               if (X[i] == dblank)
                  strcpy( message, "blank" );
               else
                  sprintf( message, "%g", X[i] );
               anyoutf( 3, message );
               if (fp != NULL)
                  fprintf( fp, "%s\n", message );
            }
         }
         else
         {
            npars = fieini_c( Expression,
                              &functionid,
                              &errpos );
            exprok = (npars >= 0);
            if (npars == -2)
            {
               anyoutf( 1, "no storage space left." );
               nextexpr = NO;
            }
            if (npars == -1)
            {
               sprintf( message,
                       "syntax error in expression at position %d",
                        errpos );
               reject_c( Key, tofchar(message) );
            }
            if (exprok)
               calculate( Varstr, npars, functionid );
         }
         free( X );
      }
      cancel_c( Key );
   }
   while (nextexpr);                      /* Continue as long as an expr. was entered */

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   if (fp != NULL)
      fclose( fp );

   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
