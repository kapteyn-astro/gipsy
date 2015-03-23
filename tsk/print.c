/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             print.dc1

Program:       PRINT

Purpose:       print part of a (sub)set on terminal/printer.

Category:      UTILITY

File:          print.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 4096.

   BOX=        Give box in .....                       [entire subset]
   
** ASCIIFILE=  File name for ASCII data:                        [NONE]
               If a name is specified, an ASCII file is created to
               store data. If you press carriage return, there will 
               be no output to an ASCII file. If a given name already
               exists, APPEND= must be specified.

   APPEND=     File exists, ok to append?                        [Y]/N
               The file specified in FILENAME= already exists. 
               You can append to this file with APPEND=Y. If APPEND=N 
               you will be prompted for another filename. 
               
** PRINTER=    Give number of printer:                     [TO SCREEN]
               If an Ascii file name was given:
               Give number of printer:            [ONLY TO ASCII FILE]
               A menu with printer numbers is presented if you give
               a non existing printer number like -1. You are re-
               prompted with the PRINTER= keyword then.
               
** TWIDTH=     Give width of output (in chars.):          [Calculated]
               If a printer was selected, the default is the number 
               of columns for that printer. For the screen option
               the default is 80 characters.
               
** TLEN=       Give page length (in lines):               [Calculated]
               If a printer was selected, the default is the number
               of rows for that printer. TLEN= is the maximum number
               of lines without a header. For the screen option
               the default is 26 lines.
               
   FORMAT=     Give format for output:                    [Calculated]
               Print numbers on screen in user given output format.
               See description for possible formats.           
               
** EXPRESSION= F(X)=                                   [No expression]               
               Give an expression using the syntax in the
               description. Only one variable is allowed (X or x).
               This variable corresponds to an image value. Example:
               if you want to multiply your data by 1000 use:
               EXPRESSION=1000*x
               The default mode leaves the data in the original state.
   
               

Description:   Write data in set (subset(s)) to terminal, printer or
               Ascii file, using a specified format. The program will
               display as many columns as fit in TWIDTH= and as many
               rows as fit in TLEN= If there are more columns or rows
               to display, the left overs are displayed separately.
               The output is repeated for the given subsets ( or for
               the grids on the non-subset axes as specified in BOX= ).
               TLEN= and TWIDTH= have defaults depending on which
               printer is selected. For the screen a width of 80
               characters and a length of 26 lines is selected. The 
               sizes for the printers are listed in a menu with
               printer numbers and characteristics. You can select a
               printer with PRINTER= The default is writing to the
               screen only. The possible output formats are described
               below. The default format is determined with the 
               DATAMIN, DATAMAX descriptors from the header. For
               all subsets the total minimum and maximum is determined.
               Field width and precision are calculated and you are
               prompted with a default format. If the items could 
               not be found, a common format is default. The default
               format has no knowledge of any given expression which
               can change the values of the minimum and the maximum.
               The syntax of EXPRESSION= is described below. If you do 
               not want to alter the original data, use the default.
               If you want f.i. to rescale the data, use something like
               
                                  EXPRESSION=1000*x+10    
                                  
               The variable 'x' corresponds to the image value. Note that
               the calculated numbers are printed in the format as
               given in FORMAT= so if you are using an expression, change
               the default format. If your output contains the '*'
               character, it means that the number could not fit into
               this format. Blank values are printed as 'b'.
               Data can be send to an Ascii file. To activate this,
               use the hidden keyword: ASCIIFILE=<filename> 
               If your file already exists, you are prompted with 
               APPEND= (which accepts Y or N etc.). If a file is sent
               to a printer, you get the message:
                
                           '... Activating printer ...'
                           
               Output is extended with the user name and a date at the 
               start of all output. At the end of the output, the
               minimum and maximum value of the used data and the 
               number of blanks in the data are printed.
               If an expression is used, also this expression is 
               printed.
               
               


               FORMATS:
               =======
               
              
               The specification in FORMAT= is called a 'format image'.              
               A 'format image' is used to print numbers in a user given
               format consisting of characters representing the   
               wanted output format. The syntax is:
 
 flag(s)       Zero or more flags, in any order, which modify the
               meaning of the conversion specification.  The flag
               characters and their meanings are:

      -        The result of the conversion is left-
               justified within the field.

      +        The result of a signed conversion always
               begins with a sign, "+" or "-".
                                                                            
 string        Characters, some with special meaning. 
               If the string (f.i. FFFFF.FF or gggg.gg or wwwww)
               contains no dot, the number of characters specify
               a minimum field width.  For an output field, if the
               converted value has fewer characters than the field
               width, it is padded on the left (or right, if the
               left-adjustment flag, - has been given) to the field
               width.
               If the string contains a dot, the total number of 
               characters including the dot is the minimum field width
               and the number of characters after the dot is the 
               precision.  
 
               The characters are used to determine the conversion
               type. If the string contains an:

               'e' or 'E' 
                      The floating-point-number argument is
                      printed in the style [-]drddde+dd,
                      where there is one digit before the
                      radix character, and the number of
                      digits after it is equal to the
                      precision. The E conversion character
                      produces a number with E introducing
                      the exponent instead of e. The
                      exponent always contains at least two 
                      digits.  However, if the value to be
                      printed requires an exponent greater
                      than two digits, additional exponent
                      digits are printed as necessary.
            
               'g' or 'G'

                      The floating-point-number argument is
                      printed in style f or e (or int style E
                      n the case of a G conversion
                      character), with the precision
                      specifying the number of significant
                      digits.  The style used depends on the
                      value converted; style e is used only
                      if the exponent resulting from the
                      conversion is less than -4 or greater
                      than or equal to the precision.
                      
               others
                      Strings without 'e', 'E', 'g' and 'G'
                      indicate a floating point conversion.
                      The floating point number argument is
                      printed in decimal notation in the
                      style [-]dddrddd, where the number of
                      digits after the radix character, r, is
                      equal to the precision specification.

               If the result of a conversion is longer than the 
               field width, an asterisk is returned. If the 
               input number is a blank, a 'b' is returned.
         


Examples:      Format string: +eeeeee.eeee     
                      Number: 43345.5436
                      Result:  +4.3346e+04
                      Remark: exponential format
                              signed conversion
                              field width: 12
                              precision:    4

               Format string: gggg.ggggg
                      Number: 34.43
                      Result:     34.430
                      Remark: Field width is 10
                              Number of significant digits is 5

               Format string: +ffff.ff
                      Number: 23.456
                      Result:   +23.46
                      Remark: signed conversion
 
               Format string: -ffff
                      Number: 345.87
                      Result: 346
                      Remark: left justified
 
               Format string: -+ffff.fff
                              Number: 34.43
                              Result: +34.430
                              Remark: left justified
                                      signed conversion

               Format string: eee.eeee
                      Number: 234.43
                      Result:        *
                      Remark: Field width too small 
                              for conversion

               Format string: ffff.ff
                      Number: blank
                      Result:       b
                      Remark: input was a blank



              EXPRESSIONS:
              ===========

              The values are calculated from a user given expression. 
              Illegal operations, like dividing by zero or the 
              logarithm of a negative value, will cause the result 
              to be set to BLANK.
              For complicated functions the user is advised to 
              make use of 'recall' files for easy input of the
              function definition. 

              Available mathematics:

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
              4) parameters; syntax; x 



Notes:         .......

Example:       .......

Updates:       Nov 30,  1992: VOG, Document created.
               May  1,  2007: JPT, Renamed conflicting toascii declaration.

#<
*/

/*  print.c: include files     */

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
#include    "math.h"         /* Mathematical functions.*/

/* Common includes */

#include    "init.h"         /* Declare task running to HERMES and initialize.*/
#include    "finis.h"        /* Informs HERMES that servant quits and cleans up the mess.*/
#include    "anyout.h"       /* General character output routine for GIPSY programs.*/
#include    "setfblank.h"    /* Subroutine to set a data value to the universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "printusing.h"
#include    "minmax3.h"

/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"      
#include    "userreal.h"     
#include    "userdble.h"     
#include    "usertext.h"     
#include    "userchar.h"    
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
#include    "gdsc_name.h"
#include    "gdsd_rreal.h"   /* Read FITS (real) data field.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/

/* Printer related */

#include    "prntrnum.h"          /* Total number of printers */
#include    "prntrcom.h"          /* Find comment for this printer */
#include    "prntrdim.h"          /* Find number of columns and rows for printer */
#include    "prntrnam.h"          /* Find name of printers */
#include    "prntract.h"          /* Send file to printer */   

/* User id string */

#include    "getusernam.h"   /* Returns the user name of the current user.*/
#include    "getdate.h"      /* Returns the current time and date as a text string */

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
#define MAXAXES        10              /* Max. axes in a set */
#define MAXSUBSETS     4096            /* Max. allowed subsets */
#define MAXBUF         4096            /* Buffer size for I/O */
#define MAXWIDTH       256
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define OFFSET         7               /* Horizontal table offset (=space for name and numbers) */
#define SCRLEN         26              /* Default table length (repeat output for bigger lengths) */
#define SCRWIDTH       80              /* Def. tab. width (repeat output for bigger width) */
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")

/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     nsubs;              /* Number of input subsets */
static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered ccording to the */
                                    /* specification by the user. */
static fint     showdev;            /* Device number (as in ANYOUT) for info */
static fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
                                    /* number of grids along an axes as */
                                    /* specified by the user. The first elements */
                                    /* (upto the dimension of the subset) contain */
                                    /* the length of the subset axes, the other */
                                    /* ones contain the the number of grids along */
                                    /* an axes outside the subset. */
static fint     maxsubs = MAXSUBSETS;
static fint     maxaxes = MAXAXES;  /* Max num. of axes the program can deal with.*/
static fint     class = 1;          /* Class 1 is for applications which repeat */
                                    /* the operation for each subset, Class 2 */
                                    /* is for applications for which the operation */
                                    /* requires an interaction between the different */
                                    /* subsets. */
static fint     subdim;             /* Dimensionality of the subsets for class 1 applications */
static fint     setdim;             /* Dimension of set. */

/* Box and frame related */

static fint     flo[MAXAXES];       /* Low  edge of frame in grids */
static fint     fhi[MAXAXES];       /* High edge of frame in grids */
static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */
static fint     boxopt;             /* The different options are: */
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */

/* Reading data */

static float    image[MAXBUF];      /* Buffer for read routine. */

/* Miscellaneous */

static fchar    Key, Mes;
static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static char     message[MAXWIDTH];  /* All purpose character buffer. */
static bool     agreed;             /* Loop guard. */
FILE            *asciiptr;          /* File pointer to ASCII file */
static fchar    Formatstr; 
static char     axname[MAXAXES][KEYLEN+1];
FILE            *TMPptr;            /* File pointer for printer files */
static char     TMPprintfilename[2*STRLEN];
static fint     prnnum;
static char     asciifilename[STRLEN];
static bool     toasciiz;
static bool     toprint;
static fint     fid;                 /* Function ID */  
static bool     express;
static float    datamin, datamax;
static fint     nblanks;
static fint     datacount;
static fchar    Expression;


static void anyoutC( int dev, char *anyCstr )
/*------------------------------------------------------------------*/
/* The C version of 'anyout_c' needs two parameters:                */
/* an integer and a C-type string. The integer determines           */
/* the destination of the output which is:                          */
/*    0  use default [set by HERMES to 3 but can be changed by user]*/
/*    1  terminal                                                   */
/*    2  LOG file                                                   */
/*    8  terminal, suppressed in "experienced mode"                 */
/*   16  terminal, only when in "test mode"                         */
/*------------------------------------------------------------------*/
{
   fint ldev = (fint) dev;
   anyout_c( &ldev, tofchar( anyCstr ) ); 
}


static void todest( int dev, char *anystr )
/*------------------------------------------------------------------*/
/* Output can be directed to printer and/or ascii file or to the    */
/* screen.                                                          */
/*------------------------------------------------------------------*/
{
   
   if (toasciiz) {
      fprintf( asciiptr, "%s\n", anystr );
   } else {
      if (toprint) {
         fprintf( TMPptr, "%s\n", anystr );
      } else {
         fint ldev = (fint) dev;
         anyout_c( &ldev, tofchar(anystr) );
      }
   }
}
   


static void printuserid( void )
/*-----------------------------------------------*/
/* Print user name and date in file or on screen */
/*-----------------------------------------------*/
{
   fchar     Idstr;
   
   fmake( Idstr, STRLEN );
   getusernam_c( Idstr );
   (void) sprintf( message, "%.*s", nelc_c( Idstr ), Idstr.a );
   getdate_c( Idstr );
   (void) sprintf( message, "%.*s %.*s", strlen(message), message,
            nelc_c( Idstr ), Idstr.a );  
   todest( 3, message );
   todest( 3, " " );
}



static int prnmenu( fchar keyword, fint *scrnum, fint *printer, 
                    fint *cols, fint *rows )
/*----------------------------------------------------------------------
 * Use:       i = prnmenu(  keyword, Input   fchar
 *                          scrnum,  Input   * fint
 *                          printer, Input   * fint
 *                          cols,    Output  * fint
 *                          rows     Output  * fint
 *                       )
 *
 *                prnmenu   Number of chosen printer.  
 *                scrnum    Destination of menu info. Same numbers as 
 *                          used in ANYOUT routine.
 *                printer   Suggested number of a printer 
 *                cols      Number of columns of selected printer.
 *                rows      Number of rows of selected printer.
 *
 * Description:   If user given printer number does not correspond to 
 *                an available printer (available under current
 *                system), present menu and ask user to select a printer.
 *                Return the number of this printer and its number of 
 *                rows and columns. 
 *                Use the function 'prntract' in the calling environment 
 *                to send a file to the selected printer.
 *----------------------------------------------------------------------
 */
{

#define MAXPRNS 20
   
         
   /*  Miscellaneous: */

   fint            Ires1, Ires2;
   fint            prnnum;             /* Number of selected printer */
   fchar           prnname;            /* Name of selected printer */     
   fchar           prncom;             /* Comments for this printer */
   fint            prncol[MAXPRNS];    /* Number of columns and rows for ... */
   fint            prnrow[MAXPRNS];    /* ... all available printers */
   char            txt[120];           /* Buffer for string manipulations */
   fchar           message;
   fint            dfault;
   fint            valid;              /* Is printer accepted ? */
   fint            i, j;               /* Counters */
   fint            nitems;
   int             printerindex;
   bool            toscreen = NO;
   fchar           Key, Mes;
   bool            menu;
      

   fmake( Key, 20 );
   fmake( Mes, 80 );
   fmake( message, 80 );
   fmake( prnname, 40 );
   fmake( prncom,  60 );
   prnnum = prntrnum_c();              /* Get number of available printers... */
                                       /* in current system */   
   if (prnnum < 0) {                   /* Something went wrong */
      switch((int)prnnum) {
         case -1:
            (void) strcpy( txt, "Cannot obtain hostname" );
            break;
         case -2:
            (void) strcpy( txt, "Cannot obtain translation of printer description file!" );
            break;
         case -3:
            (void) strcpy( txt, "Cannot open printer description file!" );
            break;
         case -4:
            (void) strcpy( txt, "Cannot allocate enough space!" );
            break;
         case -5:
            (void) strcpy( txt, "Printer comment exceeds buffer length!" );
            break;
         case -6:
            (void) strcpy( txt, "Printer name exceeds buffer length!" );
            break;
         case -7:
            (void) strcpy( txt, "Printer comment exceeds buffer length!" );
            break;
         case -8:
            (void) strcpy( txt, "Cannot obtain number of columns!" );
            break;
         case -9:
            (void) strcpy( txt, "Cannot obtain number of rows!" );
      }      
      anyout_c( scrnum, tofchar(txt) );
      return( 0 );                     /* Abort action, return to ... */
                                       /* ... calling environment */
   }
   for (i = 0, j = 1;  i < prnnum; i++, j++) {     /* Get rows and columns */
     Ires1 = prntrdim_c( &j, 
                         &prncol[i],
                         &prnrow[i] );
     if (Ires1 != 0) {                 /* Fill with dummy if no columns ... */
        prncol[i] = -1;                /* ... and rows could be found */
        prnrow[i] = -1;
     }
   }      
   
   /* All characteristics are known at this stage, ask for selection */
  
   Key    = tofchar("PRINTER=");
   Mes    = tofchar("Give number of printer ");
   dfault = HIDDEN;
   nitems = 1;
   Ires1  = userint_c( printer, &nitems, &dfault, Key, Mes ); 
   cancel_c( Key );
   
   /* If the keyword is not specified, a menu is displayed and a printer is asked */
   
   menu = ( (*printer < 0) || (*printer > prnnum) );
   toscreen = ( *printer == 0);   
   if (!menu) {
      printerindex = *printer - 1;      
   } else {
      (void) strcpy( txt, 
      "==============================PRINTERS=============================" );
      anyout_c( scrnum, tofchar(txt) );
      (void) sprintf( txt, "%3.3s   %-20.20s  %4.4s  %4.4s  %-40.40s" , "nr", 
              "    name", "cols", "rows", "   comment" );
      anyout_c( scrnum, tofchar(txt) );
      (void) strcpy(txt,
      "===================================================================" );
      anyout_c( scrnum, tofchar(txt) );
      if (toasciiz) {
         (void) sprintf( txt, "%3d   %s", 0, "Do not display output" );
      } else {
         (void) sprintf( txt, "%3d   %s", 0, "To SCREEN only" );
      }
      anyout_c( scrnum, tofchar(txt) );
      for (i = 0, j = 1; i < prnnum; i++, j++) {
          Ires1 = prntrnam_c( &j, prnname );
          Ires2 = prntrcom_c( &j, prncom );
          if (Ires1 != 0) (void) strcpy(prnname.a, "****");  /* No name available */
          if (Ires2 != 0) (void) strcpy(prncom.a,  "****");  /* No comment available */
          if (prncol[i] < 0) {
             (void) strcpy( txt, "No information about this printer" );
          }          
          else {
             (void) sprintf( txt, "%3d   %-20.20s  %4d  %4d  %-40.*s", j, prnname.a,
                             prncol[i], prnrow[i], nelc_c(prncom), prncom.a );
          }
          anyout_c( scrnum, tofchar(txt) );
      }
      (void) strcpy( txt,
      "===================================================================" );
      anyout_c( scrnum, tofchar(txt) );
      anyout_c( scrnum, tofchar(" ") );
      dfault  = REQUEST;                   /* Change the default */
      if (toasciiz) {
         message = tofchar("Give number of printer:     [ONLY TO ASCII FILE]");
      } else {
         message = tofchar("Give number of printer:        [TO SCREEN]");
      }
      do {
         nitems   = 1;
         *printer = 0;
         Ires1    = userint_c( printer, &nitems, &dfault, Key, message );
         cancel_c( Key );               
         printerindex = *printer - 1;
         valid = ( (*printer > 0) && (*printer <= prnnum) && 
                   (prncol[printerindex] > 0) );
         if (!valid) {
            toscreen = YES;
            /* cancel_c( keyword );
               anyout_c( scrnum, tofchar("Selection not possible!") );                
               message = tofchar("Try again: Select a printer:");
            */
         } else {
            toscreen = NO;
         }
      } while (!valid && !toscreen);                                    
   }
   if (!toscreen) {
      *cols = prncol[printerindex];
      *rows = prnrow[printerindex];
      return ( *printer );                /* Return the number of the printer */
   } else {
      *cols = 0;
      *rows = 0;
      return ( 0 );
   }
}


static void activateprinter( fint prnnum, char *TMPprintfilename, fint remove )
/*--------------------------------------------------------------------------*/
/* Print file with name 'TMPprintfilename'. If 'remove' is equal to zero, the   */
/* file is not removed. Temp. files are always removed.                     */
/*--------------------------------------------------------------------------*/
{
   fint     r2;

   r2 = prntract_c( &prnnum, tofchar( TMPprintfilename ), &remove );
   if (r2 ==  0 ) anyoutC( 1, "... Activating printer ..." );
   if (r2 == -1 ) anyoutC( 1, "cannot obtain hostname" );
   if (r2 == -2 ) anyoutC( 1, "cannot obtain translation of printer description file");
   if (r2 == -3 ) anyoutC( 1, "cannot open printer description file" );
   if (r2 == -4 ) anyoutC( 1, "cannot allocate enough space" );
   if (r2 == -10) anyoutC( 1, "no such printer" );
}



FILE *openfile( fchar Key, fchar Mes, fint dfault, char *filename, 
                char mode )
/*-------------------------------------------------------------*/
/* Key       Keyword                                           */
/* Mes       Message                                           */
/* dfault    Default for user                                  */
/* filename  Default filename, if length equals 0 then the     */
/*           default means no file pointer returned            */
/* mode      character r or w for read/write in fopen C        */
/*           function                                          */
/*                                                             */
/* Open file for writing/reading. Ask filename in GIPSY way    */
/* Check file for existence. Return file pointer and the name  */
/* of the given file. The function introduces the keyword      */
/* APPEND= for 'write' files that already exist.               */
/* If APPEND=N the existing file will be overwritten.          */
/*-------------------------------------------------------------*/
{
#include    "stdio.h"
#include    "ctype.h"
#include    "gipsyc.h"
#include    "usertext.h"
#include    "userlog.h"
#include    "cancel.h"
#include    "reject.h"

   bool      append;
   fint      nitems = 1;
   fint      agreed;
   fint      n;
   FILE     *fp;
   bool      readmode, writemode;
   int       yes = 1, no = 0;
   char      filebuf[132];
   fchar     Filename;
   bool      nodeffile;
   
         
   Filename.a = filebuf;
   Filename.l = 132;
   readmode  = ('R' == toupper(mode) );
   writemode = ('W' == toupper(mode) );
   nodeffile = (strlen(filename) == 0);
   if (readmode) {      
      do {
         n = usertext_c( Filename, &dfault, Key, Mes );
         if (n == 0) {
            if (nodeffile) return(NULL);
         } else { 
            strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */
         }
         fp = fopen(filename, "r");
         if (fp == NULL) {
            (void) reject_c( Key, tofchar("Cannot read file") );
            if (dfault >= 2) dfault = 1;
            nodeffile = yes;
            Mes = tofchar( "Try another file name:" );
         }
      } while (fp == NULL);
      return( fp );
   }
   if (writemode) {
      do {
         n = usertext_c( Filename, &dfault, Key, Mes );
         if (n == 0) {
            if (nodeffile) return(NULL);
         } else {
            strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */
         }
         fp = fopen(filename, "r");
         cancel_c( Key );
         if (fp != NULL) {       /* File exists */
            append = toflog( no );
            n   = userlog_c( &append,
                             &nitems,
                             &dfault,
                             tofchar("APPEND="),
                             tofchar("File exists, append?   Y=append/[N=overwrite]") );
            append = tobool( append );
            fclose( fp );
            cancel_c( tofchar("APPEND=") );
            if (append) {
               fp = fopen(filename, "a");
               agreed = (fp != NULL);
               if (!agreed) {
                  (void) reject_c( Key,
                                   tofchar("Cannot open for appending, try another!") );
               } else {
                  return( fp );
               }
            } else {
               fp = fopen(filename, "w");
               agreed = (fp != NULL);
               if (!agreed) {
                  (void) reject_c( Key,
                         tofchar("Cannot open for writing, try another!") );
               } else {
                  return( fp );
               }
            }
         } else {
            /* File does not exist */
            fp = fopen(filename, "w");
            agreed = (fp != NULL);
            if (!agreed) {
               (void) reject_c( Key, 
                                tofchar("Cannot open for writing, try another!") );
            } else {
               return( fp );
            }
         }
      } while (!agreed);
   }
   return( NULL );                /* Return NULL if not write or read mode */
}



static void printminmax( fchar Formstr, int formlen )
/*------------------------------------------------------------------*/
/* Print minimum and maximum of printed data in same format as      */
/* the numbers. Print also number of blanks and the used expression */
/*------------------------------------------------------------------*/
{
   
   fchar   Prusingstr;
   double  val;
   char    displaystr[MAXWIDTH];
   fint    l;
  
   fmake( Prusingstr, STRLEN );
   (void) sprintf( displaystr, "Min: " );
   val = (double) datamin;  
   l = printusing_c( Formatstr, &val, Prusingstr );   
   (void) sprintf( displaystr,
                   "%.*s%.*s  Max: ",
                   strlen(displaystr), displaystr,
                   formlen, Prusingstr.a );
   val = (double) datamax;
   l = printusing_c( Formatstr, &val, Prusingstr );
   (void) sprintf( displaystr,
                  "%.*s%.*s    Number of blanks: %d  ",
                   strlen(displaystr), displaystr,
                   formlen, Prusingstr.a,
                   nblanks );
   todest( 3, displaystr );
   if (express) {
      (void) sprintf( displaystr,
                      "Results obtained with expression: F(X)=%.*s", 
                      nelc_c( Expression ), Expression.a );
      todest( 3, displaystr );
   }
}
                      

static void displaydata( fint *dlo, fint *dhi, int subdim, int setdim,
                         fint tablen, fint tabwidth, fchar Formstr, int formlen,
                         int maxcols )
/*------------------------------------------------------*/
/* Re permutate the grids, read in data line by line.   */
/*------------------------------------------------------*/
{
   fint    cwlo, cwhi;
   fint    tid = 0;
   fint    maxIObuf = MAXBUF;
   fint    pixelsread;
   int     i, j;   
   char    displaystr[MAXWIDTH];
   fchar   Prusingstr;
   fint    l;
   int     cols;
   fint    rlo[MAXAXES], rhi[MAXAXES];
   int     x, y;
   int     scr = 3;



   fmake( Prusingstr, STRLEN );
   cols  = (dhi[0] - dlo[0] + 1);
   for (i = 0; i < setdim; i++) {
      /* Repermutation */
      rlo[axnum[i]-1] = dlo[i];
      rhi[axnum[i]-1] = dhi[i];
   }
   
   (void) sprintf( message, "%*.*s: ", OFFSET-2, OFFSET-2, axname[0] );
   for (x = dlo[0]; x <= dhi[0]; x++) {
      char    dummy[MAXWIDTH];
      /* Add grids of first axis (take care of one extra space between columns */
      l = sprintf( dummy, "%d", x );
      if (l > formlen) l = sprintf( dummy, "%s", "*" );
      (void) sprintf( message, "%.*s%*.*s ", strlen(message), message, 
                      formlen, formlen, dummy );
   }
   todest( scr, message );
   
   /* Number the vertical subset axis */
   
   if (subdim >= 2) {
      (void) sprintf( message, "%*.*s:", OFFSET-2, OFFSET-2, axname[1] );
      todest( scr, message );
   } 
      
   
   for (y = dhi[1]; y >= dlo[1]; y--) {
      rlo[axnum[1]-1] = rhi[axnum[1]-1] = y;  /* Read in image data line by line */
      cwlo   = gdsc_fill_c( Setin, &setlevel, rlo );
      cwhi   = gdsc_fill_c( Setin, &setlevel, rhi );           
      gdsi_read_c( Setin,
                   &cwlo, &cwhi,
                   image,
                   &maxIObuf,
                   &pixelsread,
                   &tid );
      if (express) {
         int   r1;
         r1 = fiedo_c( image, &pixelsread, image, &fid );  /* Calculate result array */
         if (r1 != 0) {
            anyoutC(1, "Problem evaluating data!");
            return;
         }
      }
      minmax3_c( image, &pixelsread, &datamin, &datamax, &nblanks, &datacount );
      if (subdim >= 2) {
         (void) sprintf( displaystr, "%*d: ", OFFSET-2, y );
      } else {
         (void) sprintf( displaystr, "%*.*s", OFFSET, OFFSET, " " );
      }
      for (j = 0; j < cols; j++) {
         double   val = (double) image[j];
         l = printusing_c( Formatstr, &val, Prusingstr );
         (void) sprintf( displaystr, 
                         "%.*s%.*s ", 
                         strlen(displaystr), displaystr, 
                         formlen, Prusingstr.a );
      }
      todest( scr, displaystr );
   }
}



static void rebox( fint *blo, fint *bhi, int subdim, int setdim, 
                   fint tablen, fint tabwidth, fchar Formstr, int formlen,
                   int maxcols )
/*---------------------------------------------------*/
/* Divide box into parts that fit in maxcols*tablen  */
/* before printing any data.                         */
/*---------------------------------------------------*/
{
   int       i;
   fint      dlo[MAXAXES], dhi[MAXAXES];
   int       len, lenleft;
   int       width, widthleft;
   int       dimstart;
   int       scr = 3;


   for (i = 0; i < setdim; i++) {
      dlo[i] = blo[i];
      dhi[i] = bhi[i];
   }

   dimstart = MYMIN( subdim, 2 );
   (void) sprintf( message, "Set: %.*s", nelc_c(Setin), Setin.a );
   for (i = dimstart; i < setdim; i++) {
      (void) sprintf( message, "%.*s %s=%d ",
                      strlen(message), message,
                      axname[i], blo[i] );
   }
   
   todest( scr, message );
   todest( scr, " " );
                  
   /* divide into parts that fit within tablen*maxcols */
   lenleft = (bhi[1]-blo[1]+1);
   do {
      len       = MYMIN( tablen, lenleft );
      lenleft  -= len;
      dlo[1]    = dhi[1] - len + 1;
      widthleft = (bhi[0]-blo[0]+1);
      dlo[0]    = blo[0];
      do {
         width = MYMIN( maxcols, widthleft);
         widthleft -= width;
         dhi[0]     = dlo[0] + width - 1;
         displaydata( dlo, dhi, subdim, setdim, tablen, tabwidth, Formatstr, formlen, maxcols );
         todest( scr, " " );
         dlo[0]     = dhi[0] + 1;
      } while (widthleft);      
      dhi[1] = dlo[1] - 1;
   } while (lenleft);
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   int        i, j; 
   int        subnr;
   fint       tabwidth, tablen;
   int        runpix, modval;
   fint       dfault, nitems;
   fint       r1, r2;
   int        maxcols;   
   fint       formlen;
   fint       fileremove = YES;
   fint       cols, rows;
   int        dimstart;
   int        size = 0;
   int        prec = 0;
   

   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      (void) myname_c( Task );             /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );
   fmake( Setin, STRLEN );
   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   dfault  = NONE;
   subdim  = 0;
   showdev = 3;
   Key     = KEY_INSET;
   Mes     = MES_INSET;
   nsubs   = gdsinp_c( Setin,      /* Name of input set. */
                       subin,      /* Array containing subsets coordinate words. */
                       &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                       &dfault,    /* Default code as is USERxxx. */
                       Key,        /* Keyword prompt. */
                       Mes,        /* Keyword message for the user. */
                       &showdev,   /* Device number (as in ANYOUT). */
                       axnum,      /* Array of size 'maxaxes' containing the axes numbers. */
                                   /* The first elements (upto the dimension of the subset) */
                                   /* contain the axes numbers of the subset, */
                                   /* the other ones contain the axes numbers */
                                   /* outside the subset ordered according to the */
                                   /* specification by the user. */
                       axcount,    /* Number of grids on axes in 'axnum' */
                       &maxaxes,   /* Max. number of axes. */
                                   /* the operation for each subset. */
                       &class,     /* Class 1 is for applications which repeat */
                       &subdim );  /* Dimensionality of the subsets for class 1 */
   setdim  = gdsc_ndims_c( Setin, &setlevel );

   /*-------------------------------*/
   /* Determine edges of this frame */
   /*-------------------------------*/
   {
      fint cwlo, cwhi;                          /* Local coordinate words */
      int  m;
      r1 = 0;
      gdsc_range_c( Setin, &subin[0], &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++) {
         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
      }
      r1 = 0;
      gdsc_range_c( Setin, &subin[nsubs-1], &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++) {      
         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
      for (m = setdim; m < MAXAXES; m++) {
         fhi[m] = flo[m] = 0;
      }
   }
   
   /*-----------------------------*/
   /* Get the axis names          */
   /*-----------------------------*/
   for (i = 0; i < setdim; i++) {
      fchar   Ctype;
      Ctype.a = axname[i];
      Ctype.l = KEYLEN;
      r1 = 0;
      gdsc_name_c( Ctype, Setin, &axnum[i], &r1 );
      (void) sprintf( axname[i], strtok( Ctype.a, " -" ) );
   }
   

   /*-------------------------------*/
   /* Prepare a box for INSET       */
   /*-------------------------------*/
   boxopt  = 0;
   showdev = 3;
   dfault  = REQUEST;
   Key     = KEY_BOX;
   Mes     = MES_BOX;
   gdsbox_c( flo, fhi, Setin, subin, &dfault, 
             Key, Mes, &showdev, &boxopt );

   for (i = 0; i < setdim; i++) {
      (void) sprintf( message, "max. range %s: flo=%d  fhi=%d", axname[i], flo[i], fhi[i] );
      anyoutC( 16, message );
   }
   


   /*------------------------------------------------------*/
   /* Ask for name of ASCII file                           */
   /*------------------------------------------------------*/

   asciifilename[0] = '\0';     /* <CR> in 'openfile' results in asciiptr==NULL */
   dfault   = HIDDEN;
   Key      = tofchar("ASCIIFILE=");
   Mes      = tofchar("File name for ASCII data:     [NONE]");
   asciiptr = openfile( Key, Mes, dfault, asciifilename, 'w' );
   toasciiz  = (asciiptr != NULL);                           
   if (toasciiz) { 
      (void) sprintf( message, "ASCII data to disk in [%s]", asciifilename );
      anyoutC( 1, message );
   }


   /*---------------------------------------*/
   /* Ask for a printer. If an ascii file   */
   /* specified, use the ascii file name    */
   /* the temp. print file name and do not  */
   /* remove this file after printing.      */
   /*---------------------------------------*/
   prnnum = 0;
   prnnum = prnmenu( Key, &showdev, &prnnum, &cols, &rows );
   if (prnnum != 0) {
      if (!toasciiz) {
         tmpnam( TMPprintfilename );                     /* Create unique name for printer file */
         fileremove = YES;                               /* And remove it afterwards */
         TMPptr  = fopen( TMPprintfilename, "w");
         if (TMPptr == NULL) {
            anyoutC( 1, "Cannot open temp. file" );            
         }         
      } else {                                           /* Use the ascii file name */
         strcpy( TMPprintfilename, asciifilename );
         TMPptr = asciiptr;
         fileremove = NO;                                /* Do not remove the ascii file */
      }      
   } else {
      TMPptr  = NULL;
   }
   toprint = (TMPptr != NULL);
   

   /*-------------------------------------------------------*/
   /* Get the wanted table width. Default is 80 characters  */
   /* for the terminal and if a printer is selected, the    */
   /* width as returned by the printer menu.                */
   /*-------------------------------------------------------*/   

   nitems   = 1;
   dfault   = HIDDEN;
   if (toprint) tabwidth = cols; else tabwidth = SCRWIDTH;     /* From the printer selection */
   Key      = tofchar("TWIDTH=");
   (void) sprintf( message, "Give width of output (in chars.):    [%d]", tabwidth);   
   Mes      = tofchar(message);
   r1       = userint_c( &tabwidth, &nitems, &dfault, Key, Mes );
   if (tabwidth > MAXWIDTH ) tabwidth = MAXWIDTH;
   if (tabwidth < 2) tabwidth = 2;
   
  
   nitems   = 1;
   dfault   = HIDDEN;
   if (toprint) tablen = rows - 6; else tablen = SCRLEN;
   Key      = tofchar("TLEN=");
   (void) sprintf( message, "Give page length (in lines):    [%d]", tablen);   
   Mes      = tofchar(message);
   r1       = userint_c( &tablen, &nitems, &dfault, Key, Mes );
   if (tablen < 1) tablen = 1;


   /*-------------------------------------------------------------*/
   /* If the descriptors DATAMIN, DATAMAX are available, use them */
   /* to construct a reasonable default format.                   */
   /*-------------------------------------------------------------*/   

   datamin = blank;
   datamax = blank;
   for (i = 0; i < nsubs; i++) {
      fint    r2 = r1 = 0;
      float   dmin, dmax;
      gdsd_rreal_c( Setin, tofchar("DATAMIN"), &subin[i], &dmin, &r1 );
      gdsd_rreal_c( Setin, tofchar("DATAMAX"), &subin[i], &dmax, &r2 );      
      if ( (r1 >= 0) && (dmin != blank) ) {
         if (datamin == blank) {
            datamin = dmin;
         } else {
            if (dmin < datamin) datamin = dmin;
         }
      }
      if ( (r2 >= 0) && (dmax != blank) ) {
         if (datamax == blank) {
            datamax = dmax;
         } else {
            if (dmax > datamax) datamax = dmax;
         }
      }
   }
   if ( (datamin != blank) && (datamax != blank) ) {
      double  newmax = MYMAX( fabs(datamax), fabs(datamin) );
      double  diff = fabs( datamax - datamin );
      (void) sprintf( message, "Max, min from header: %f %f", datamin, datamax );
      anyoutC( 16, message );
      if (newmax == 0.0) {
         size = 2;
      } else {
         size = ((int) fabs(log10(newmax))) + 2;
      }
      if ( (diff > 1.0) || (diff == 0.0) ) {
         prec = 1;
      } else {
         prec = ((int) fabs(log10(diff))) + 2;
      }
   }
  
   do {
      double dummy = 0.0;
      fchar  Dummystr;
      
      fmake( Formatstr, STRLEN );
      if ( (datamin != blank) && (datamax != blank) ) {
         char   mes1[STRLEN], mes2[STRLEN];
         if (size < 12) {
            memset( mes1, 'f', size ); mes1[size] = '\0';
            memset( mes2, 'f', prec ); mes2[prec] = '\0';
            (void) sprintf( Formatstr.a, "%s.%s", mes1, mes2 );
         } else {
            memset( mes2, 'e', prec ); mes2[prec] = '\0';
            (void) sprintf( Formatstr.a, "+eeeee.%s", mes2 );
         }
      } else {
        strcpy( Formatstr.a, "fffff.fff" );    
      }

      fmake( Dummystr, STRLEN );
      nitems  = 1;
      dfault  = REQUEST;
      Key     = tofchar("FORMAT=");
      (void) sprintf( message, "Give format for output:     [%s]", Formatstr.a );
      Mes     = tofchar(message);
      r1      = userchar_c( Formatstr, &nitems, &dfault, Key, Mes );
      formlen = printusing_c( Formatstr, &dummy, Dummystr );    
      maxcols = (tabwidth-OFFSET) / (formlen+1);          /* Include space between columns */
      agreed  = (maxcols > 0) ;
      if (!agreed) reject_c( Key, tofchar("Field length too big!") );
   } while (!agreed);
   

   /*------------------------------------*/
   /* For scaling etc, use an expression */
   /*------------------------------------*/
   {
      fint     numpars;
      fint     errpos;
      
      fmake( Expression, MAXWIDTH );
      dfault = HIDDEN;
      do {
         int   len;                  
         Key = tofchar( "EXPRESSION=" );
         Mes = tofchar( "F(X)=            [No expression]" );
         len = usertext_c( Expression, &dfault, Key, Mes );
         if (len == 0) express = NO; else express = YES;      
         if (express) {
            fint  one = 1;
            r1 = fiepar_c( tofchar("X"), &one );
            /* Set function id */
            fid = 0;
            numpars = fieini_c( Expression, &fid, &errpos );
            agreed = (numpars == 1);
            if (!agreed) {
               if (numpars < 0) {
                  (void) sprintf( message, 
                         "Error in expression at or near position %d", errpos );
               }
               if (numpars > 1) {
                  (void) sprintf( message, 
                         "Too many variables in expression!" );
               }
               anyoutC( 1, message );
               reject_c( Key, tofchar("Wrong expression!") );
            }
         }
      } while (!agreed);
   }
  

   /*----------------------------------------------------------*/
   /* Repeat displaying two (or less) dimensional parts of the */
   /* set. The number of these parts, depends on the number of */
   /* non subset axes.                                         */
   /*----------------------------------------------------------*/

   datacount = 0;
   printuserid();     
   
   /* A subset loop is needed because subsets can be given in a non-  */
   /* subsequent way. For each subset, the grids on the subset axis   */
   /* are calculated. */
   
   for (subnr = 0; subnr < nsubs; subnr++) {
      fint cwlo, cwhi;                          /* Local coordinate words */
      int  m;
      r1 = 0;
      gdsc_range_c( Setin, &subin[subnr], &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = subdim; m < (int) setdim; m++) {
         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
   
      /* Initialize */
      for (i = 0; i < setdim; i++) {
         blo[i] = flo[i];
         bhi[i] = fhi[i];
      }
      dimstart = MYMIN( subdim, 2 );   
      runpix = 1;      
      for (i = dimstart; i < setdim; i++) {
         runpix *= (fhi[i] - flo[i] + 1);
      }
   

      for (i = 0; i < runpix; i++) {
         modval = 1;      
         for (j = dimstart; j < setdim; j++) {
            int   len = (fhi[j] - flo[j] + 1);
            bhi[j] = blo[j] = flo[j] + (i/modval)%len;    /* increase non subset axis grids */
            modval *= len;
         }
         rebox( blo, bhi, subdim, setdim, tablen, tabwidth, Formatstr, formlen, maxcols );
      }
   }
   
   printminmax( Formatstr, formlen );
   if (toasciiz) fclose( asciiptr );
   if (toprint) {
      fclose( TMPptr );
      activateprinter( prnnum, TMPprintfilename, fileremove );
   }   
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
