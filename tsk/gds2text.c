/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             gds2text.dc1

Program:       GDS2TEXT

Purpose:       Put GDS image data in ASCII file and vv. Use a format
               for the numbers. Add (physical coordinates) to the data
               if in 'export' mode.

Category:      UTILITY

File:          gds2text.c

Author:        M. Vogelaar

Keywords:

   EXPORTFILE= Name of export ASCII file:         [switch to read mode]

               If a name is specified, an ASCII file is created to
               store data from INSET=. If you press carriage return,
               the program switches to read mode and will ask for
               IMPORTFILE=  If a given name already exists, FILESTAT=
               must be specified.


   FILESTAT=   File exists, Overwrite, Append or New name?      [O]/A/N

               The file specified in EXPORTFILE= already exists.
               Choose one og the following options.
               O: Default, overwrite this file
               A: Append to the existing file.
               N: Prompt with EXPORTFILE= again to give another name.


   IMPORTFILE= Name of import ASCII file:                [stop program]

               Specify the name of an ASCII file with data
               to fill the image of INSET=
               The data must be arranged in one column, the first
               column of the ASCII file.


   INSET=      Give set, subsets:

               Maximum number of subsets is 4096.
               The set is either a set from which the data is
               extracted (EXPORTFILE= is used) or a set to which ASCII
               data is written (IMPORTFILE= is used).


   BOX=        Give box in .....                        [entire subset]

               Either the box from which the data is extracted or the
               box to which data is written.


   POSFORM=    Do you want to import data with positions?       Y/[N]
   
               This keyword is only asked when you are in the 'IMPORT'
               mode. The ASCII file on disk has format:
               
               x1, x2, ... xn, image-value
               
               where n is the dimension of the (sub)set that you 
               specified with INSET=  

               !!! If you entered more than one subset, then only the 
                   FIRST subset is processed !!!


   SHEET=      Do you want a spreadsheet output?                  Y/[N]

               This keyword is only asked when you are in the 'EXPORT'
               mode and selected a 2-dimensional (sub)set.
               Put ASCII data in sheet format i.e the ASCII file has
               rows and columns equal to the number of rows and columns
               in the selected box (BOX=). Therefore the input must be
               2-dimensional. Data is separated by a comma.
               Example: (SHEET=Y, FORMAT=ffff.f)

               -20.0 ,  -15.0 ,  -10.0 ,   -5.0 ..........
               -16.0 ,  -12.0 ,   -8.0 ,   -4.0
               -12.0 ,   -9.0 ,   -6.0 ,   -3.0
                -8.0 ,   -6.0 ,   -4.0 ,   -2.0
                16.0 ,   12.0 ,    8.0 ,      b
                 .
                 .
                 .
                 .

   FORMAT=     Give format for image data output:          [Calculated]

               This keyword is only asked when you are in the 'EXPORT'
               mode.
               Write numbers to ASCII file in a user given output
               format. See description for possible formats.
               (e.g. FORMAT=ffffff.ffff
                     FORMAT=ggggg.ggg
                     FORMAT=ddddd )


   XYZFORM=    Do you want output in 'X-Y-data' format?           Y/[N]

               This keyword is only asked when you are in the 'EXPORT'
               mode and selected a 2-dimensional (sub)set but did
               not want output in 'sheet' format.
               For 2-dim subsets it is possible to write (besides the
               image data) the positions in grid- or physical coordinates
               of each pixel value, to a file. The format of the output
               ASCII file is then:

               X position, Y position, Image value.

               Example contents of file 'num.txt':
               (FORMAT=eeeeee.eeeeeeeeee   TOPHYS=Y)

               44.943470940588 44.959986063360  1.6000000000E+01
               44.957603199423 44.959992163481  1.2000000000E+01
               44.971735463416 44.959996520713  8.0000000000E+00
               44.985867730848 44.959999135052                 b
               45.000000000000 44.960000006498 -0.0000000000E+00
               45.014132269152 44.959999135052 -4.0000000000E+00


   TOPHYS=     Do you want x and y in physical coords.:           Y/[N]

               (Only asked if XYZFORM=Y)
               The X and Y values can be converted to values in units
               given by the header of the input set (e.g. DEGREE, KM/S).
               You can use a program like HEADER or FIXHED to find
               the coordinates of the (sub)set axes.


   BLANK=      Value to replace a blank:               [do not replace]

               A blank in the image can be replaced by another value,
               so that the output file will not contain the text that
               represents a blank value. Usually the text for a blank
               is  'b'. This also the default.

   
** SKIPBLANK=  Skip writing blanks in file:                        [NO]
   
               If you want to create a file with data from a set and,
               at the same time, you want to skip writing blanks in 
               that file, use SKIPBLANK=Y
   

Note:          If you want to write data values and physical coordinates
               for one dimensional (sub)sets use program PPLOT with
               the FILENAME= keyword.


Description:   In GDS (GIPSY Data System) an image is an array of
               floating point numbers and a descriptor is a file which
               describes how this data is arranged in size and dimensio-
               nality. Sometimes you want to examine or process part of
               the image with tools not available in GIPSY. The easiest
               way to exchange data is that of an ASCII file.
               With this program and some knowledge of size and
               dimensionality of the your GIPSY set, it is possible to
               write image values to an ASCII file (EXPORTFILE=) and to
               read values from a text file (IMPORTFILE=) to store in
               the GIPSY set. With INSET= you determine the axis order
               of your subset and with BOX= you specify which part of
               the selected subset is used for extracting or receiving
               data. The FITS keywords in the descriptor describing
               the size and structure are:
               NAXIS    specifies the number of coordinate axes in the
                        image.
               NAXIS1   specifies the number of pixels along the most
                        rapidly varying axis within the image.
               CTYPE1   type of physical coordinate along first axis
               NAXIS2   specifies the number of pixels along the second
                        most rapidly varying axis within the image.
               etc.

               If the set AURORA has for example the axes

               RA         from   -64 to    63
               DEC        from   -64 to    63
               FREQ       from     1 to    32

               then the subset INSET=AURORA FREQ 1 defines a plane
               with the RA axis as most rapidly varying axis and the
               DEC axis as second most rapidly varying axis. If you
               give BOX=-10 -3 10 3  and EXPORTFILE=export.txt,
               the data from set AURORA will be extracted in rows
               from RA=-10 to RA=10 starting at DEC=-3 and ending at
               DEC=3. The numbers will be stored in one column in
               the text file 'export.txt' If you want to examine a
               RA profile at certain DEC, you need to remember the
               width of your box i.e. 10-(-10)+1=21

               Blanks:

               If your set data has blanks, then the export ASCII file
               will contain the text equivalence of a blank. If you
               entered a value for BLANK= then this value will be
               substituted for each blank in the input set. If the
               import ASCII file contains numbers that cannot be
               converted to a float, then a blank is substituted in
               INSET=.


               READING IMAGE DATA FROM ASCII FILE WITH POSITION
               INFORMATION:
               
               The format of the ASCII file is:

                         x1, x2, ... xn, image-value
                         
               The vector x1, x2, ... xn defines one position in 
               an n-dimensional subset. So your INSET= specification
               must also be a n-dimensional structure.

               Example:
               
               ASCII file on disk "data.txt" contains:
               
               2   3 2 23.2
               3   4 3 30.5
               0   0 2 80.0
               10 10 1 67.9 
               
               The positions are grids in RA, DEC and FREQ. The 4th
               number is the image value that you want to write to 
               the 3-dim set AURORA.
             
               <USER> gds2text
               GDS2TEXT  Version 1.0  (May  3 1996)
               <USER> GDS2TEXT EXPORTFILE=
               <USER> GDS2TEXT IMPORTFILE=data.txt
               <USER> GDS2TEXT INSET=AURORA
               Set AURORA has 3 axes
               RA-NCP             from    -4 to     5
               DEC                from    -4 to     5
               FREQ-OHEL          from     0 to     9
               <USER> GDS2TEXT BOX=
               BOX range for set AURORA :
               RA-NCP             from    -4 to     5
               DEC                from    -4 to     5
               FREQ-OHEL          from     0 to     9
               <USER> GDS2TEXT POSFORM=y
               Wrote 3 image value(s) with positions from ASCII file to set
               Found 1 position(s) outside BOX=
               
               


               CREATING AN INPUT FILE FOR A SPREADSHEET:

               If a (sub)set is 2-dimensional, it is possible to
               write to an ASCII file in a spreadsheet format.
               A spreadsheet consists of rows and columns. The
               entries in a row are separated by colons. The rows
               are separated by a newline character. The entries
               are formatted according the string given in
               FORMAT=  Possible formats for the numbers are described
               below. Blank values can be replaced by a unique
               value. If a number cannot be formatted, an asterisk is
               returned.



               CREATING A FILE IN FORMAT X, Y, Z:

               If a (sub)set is 2-dimensional, it is possible to
               write to an ASCII file in a special format.
               Two integer numbers (X, Y of a position) are followed
               by the corresponding image value. This option is disabled
               if SHEET=Y.


               Possible formats:

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
               input number is a blank, the character 'b' is returned.



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



Notes:

Example:       <USER> gds2text
               GDS2TEXT  Version 1.0  (Jul 28 1992)
               <USER> GDS2TEXT EXPORTFILE=
               <USER> GDS2TEXT IMPORTFILE=hallo
               <USER> GDS2TEXT INSET=hi1277 1
               Set hi1277 has 3 axes
               RA---NCP           from   -64 to    63
               DEC--NCP           from   -64 to    63
               PARAM-SUM          from     1 to     1
               <USER> GDS2TEXT BOX=-10 -10 -4 -4
               BOX range for set hi1277 :
               RA---NCP           from   -10 to    -4
               DEC--NCP           from   -10 to    -4
               Converted 49 floats from ASCII file
               <STATUS>  GDS2TEXT   +++ FINISHED +++

Updates:       Jul 27,  1992: VOG, Document created.
               Jul 23,  1993: VOG, Spreadsheet option implemented.
               Sep  1,  1995: VOG, Physical coordinates in XYZ format
               Mar 30,  2001: VOG, New keyword SKIPBLANK=

#<
*/

/*  gds2text.c: include files     */

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
#include    "setdblank.h"    /* Universal 'double' BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "printusing.h"
#include    "cotrans.h"


/* User input routines */

#include    "userfio.h"      /* anyoutf etc. */
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
#include    "gdsi_write.h"   /* Writes ... */


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
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define MAXLINELEN     256*32          /* Max. width of sheet line */
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0


#define FILELEN        256
#define MESLEN         80
#define KEYLEN         20


/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")


/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     nsubs;              /* Number of input subsets */
static fint     dfault;             /* Default option for input etc */
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

static fint     cwlo, cwhi;         /* Coordinate words. */
static fint     tid;                /* Transfer id for read function. */
static fint     maxIObuf = MAXBUF;  /* Maximum size of read buffer. */
static fint     pixelsread;         /* Number of pixels read by read routine. */
static float    image[MAXBUF];      /* Buffer for read routine. */
static fint     subnr;              /* Counter for subset loop. */


/* Miscellaneous */

FILE            *fpim, *fpex;       /* File pointers to ASCII file */
static fchar    Key, Mes;
static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[120];       /* All purpose character buffer. */
static int      i;                  /* Various counters. */
static char     filename[120];
static float    *databuf = NULL;
static fint     subsetpixels;
static fint     pixelsdone;
static fchar    Formatstr;
static bool     sheet;
static double   replace;
static fint     nitems;
static bool     toxyz;
static bool     tophys;
static bool     skipblank;



FILE *getexportfile( char *filename )
/*------------------------------------------------------*/
/* Open file for writing. Ask filename in GIPSY way     */
/* Check file for existence. Return file pointer        */
/* and the name of the given file.                      */
/* The function introduces the keywords EXPORTFILE= and */
/* FILESTAT=. The macro 'fmake' and the definitions for */
/* YES, NO, FILELEN MESLEN and KEYLEN must be available.*/
/*------------------------------------------------------*/
{
#include    "stdio.h"
#include    "usertext.h"
#include    "userlog.h"
#include    "cancel.h"
#include    "reject.h"


   fchar     Filename;
   bool      append, overwrite;
   fint      request = 1;
   fint      dfault;
   fint      nitems;
   fint      agreed;
   fint      n;
   fchar     Key, Mes;
   FILE     *fp;


   dfault = request;
   fmake( Filename, FILELEN );
   fmake( Key,      KEYLEN );
   fmake( Mes,      MESLEN );
   do
   {
      append    = NO;
      overwrite = YES;
      Key    = tofchar("EXPORTFILE=");
      Mes    = tofchar("Name of export ASCII file:   [switch to read mode]");
      n      = usertext_c( Filename,
                           &dfault,
                           Key,
                           Mes );
      if (n == 0)
         return( NULL );                               /* User wants to switch */
      strcpy( filename, strtok(Filename.a, " ") );       /* Delete after space */
      fp = fopen(filename, "r");

      if (fp != NULL)                  /* Opened for reading ==> existing file */
      {
         nitems = 1;
         Key    = tofchar("FILESTAT==");
         Mes    = tofchar("File exists, Overwrite, Append or New name? [O]/A/N");
         {
            fchar Buf;
            fmake( Buf, 20 );
            n = usertext_c( Buf, &dfault, Key, Mes );
            if (toupper(Buf.a[0]) == 'N')
               overwrite = NO;
            if (toupper(Buf.a[0]) == 'A')
            {
               append    = YES;
               overwrite = NO;
            }
         }
         fclose( fp );
         cancel_c( Key );
      }


      Key = tofchar("EXPORTFILE=");
      if (!append && !overwrite)
      {
         cancel_c( Key );
         agreed = NO;
      }
      else
      {
         if (append)
            fp = fopen(filename, "a");
         else
            fp = fopen(filename, "w");
         agreed = (fp != NULL);
         if (!agreed) {
            (void) reject_c( Key, tofchar("Cannot write, try another!") );
         }
      }
   } while (!agreed);
   cancel_c( Key );
   return( fp );                /* Return the file pointer */
}



FILE *getimportfile( char *filename )
/*-----------------------------------------------------*/
/* Open file for reading. Ask filename in GIPSY way    */
/* Check file for existence. Return file pointer       */
/* and the name of the given file.                     */
/* The function introduces the keyword IMPORTFILE=     */
/* The macro 'fmake' and the definitions for           */
/* YES and NO must be available.                       */
/*-----------------------------------------------------*/
{
#include    "stdio.h"
#include    "usertext.h"
#include    "userlog.h"
#include    "cancel.h"
#include    "reject.h"


   fchar     Filename;
   fint      request = 1;
   fint      dfault;
   fint      agreed;
   fint      n;
   fchar     Key, Mes;
   FILE     *fp;

   dfault = request;
   fmake( Filename, FILELEN );
   fmake( Key,      KEYLEN );
   fmake( Mes,      MESLEN );
   do
   {
      Key    = tofchar("IMPORTFILE=");
      Mes    = tofchar("Name of import ASCII file:    [stop program]");
      n      = usertext_c( Filename,
                           &dfault,
                           Key,
                           Mes );
      if (n == 0) return NULL;
      strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */
      fp = fopen(filename, "r");
      agreed = (fp != NULL);
      if (!agreed)
      {
         (void) reject_c( Key, tofchar("Cannot open, try another!") );
      }
   } while (!agreed);
   cancel_c( Key );
   return( fp );                /* Return the file pointer */
}



static void readposdata( fchar   Setin, 
                         fint    subset, 
                         FILE   *importfile, 
                         fint   *blo, 
                         fint   *bhi )
/*--------------------------------------------------*/
/* PURPOSE */
/*--------------------------------------------------*/
{
#define NCHAR     80   

   float   pos[5];
   float   imageval;
   fint    dim;
   char    convstr[NCHAR];
   int     r1;
   int     num = 0;
   int     outsides = 0;


   dim  = gdsc_ndims_c( Setin, &subset );
   if (dim < 1)
   {
      anyoutf( 1, "Cannot read data for 0-dim (sub)set" );
      return;
   }
   if (dim > 5)
   {
      anyoutf( 1, "(sub)set dimensions > 5 not allowed!" );
      return;            
   } 
   while (!feof(importfile))
   {
      convstr[0] = '\0';
      fgets( convstr, NCHAR, importfile );
      r1 = 0;
      if (dim == 1)
      {         
         r1 = sscanf( convstr, "%f %f", &pos[0], &imageval );
      }
      else if (dim == 2)
      {
         r1 = sscanf( convstr, "%f %f %f", &pos[0], &pos[1], &imageval );
      }
      else if (dim == 3)
      {
         r1 = sscanf( convstr, "%f %f %f %f", &pos[0], &pos[1], &pos[2], &imageval );
      }
      else if (dim == 4)
      {
         r1 = sscanf( convstr, "%f %f %f %f %f", &pos[0], &pos[1], &pos[2], &pos[3], &imageval );
      }
      else if (dim == 5)
      {
         r1 = sscanf( convstr, "%f %f %f %f %f %f", &pos[0], &pos[1], 
                      &pos[2], &pos[3], &pos[4], &imageval );
      }
      if (!feof(importfile))
      {
         if (r1 != dim + 1)
            anyoutf( 1, "Could not convert: [%s]", convstr );      
         else
         { 
            
            fint   tid = 0;
            fint   cwlo, cwhi;
            fint   pixelsin = 1;
            fint   pixelsdone;
            fint   ipos[5];
            int    k;
            int    inside = YES;
            
            for (k = 0; k < dim; k++)
            {
               ipos[k] = (int) pos[k];
               if (ipos[k] > bhi[k] || ipos[k] < blo[k])
                  inside = NO;
            }
               
            if (inside)
            {
               cwlo = gdsc_fill_c( Setin, &subset, ipos );
               cwhi = gdsc_fill_c( Setin, &subset, ipos );
               gdsi_write_c( Setin, 
                             &cwlo, &cwhi, 
                             &imageval,
                             &pixelsin, 
                             &pixelsdone, 
                             &tid );
               if (pixelsdone == 1)
                  num++;
            }
            else
               outsides++;
         }
            
      }
   }
   anyoutf( 1, "Wrote %d image value(s) with positions from ASCII file to set", num );
   if (outsides)
      anyoutf( 1, "Found %d position(s) outside BOX=", outsides );   
}                         



void readfromascii( FILE *importfile, int subsetpixels, float *databuf )
/*-----------------------------------------------------------------------*/
/* Read from ASCII file 'subsetpixels' of floats in 'databuf'.           */
/* If there is no conversion possible, a blank is substituted.           */
/*-----------------------------------------------------------------------*/
{
#define NCHAR     80

   int   num = 0;
   char  convstr[NCHAR];
   int   r1;


   while (!feof(importfile))
   {
      convstr[0] = '\0';
      fgets( convstr, NCHAR, importfile);
      r1 = sscanf( convstr, "%f", &databuf[num] );
      if (r1 == 0) databuf[num] = blank;
      if (!feof(importfile)) num++;
   }

   (void) sprintf( message, "Converted %d floats from ASCII file", num );
   anyoutf( 1, message );
   /* If file was smaller than number of required data points: */
   while (num <= subsetpixels)
      databuf[num++] = blank;
}





MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fchar    Prusingstr;
   bool     posform;


   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      fchar    Task;                       /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      (void) myname_c( Task );             /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );

   fpex = getexportfile( filename );
   if (!fpex) 
   {
      fpim = getimportfile( filename );
   }
   if (!(fpim) && (!fpex)) 
      finis_c();


   fmake( Setin, STRLEN );
   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   dfault  = NONE;
   subdim  = 0;
   showdev = 3;
   Key     = KEY_INSET;
   if (fpim) {
      Mes = tofchar("Set, subsets to receive ASCII data:");
   } else {
      Mes = tofchar("Set, subsets to extract ASCII data from:");
   }
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
      gdsc_range_c( Setin, &setlevel, &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++) {
         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
   }

   /*-------------------------------*/
   /* Prepare a box for INSET       */
   /*-------------------------------*/
   boxopt  = 0;
   showdev = 3;
   dfault  = REQUEST;
   Key     = KEY_BOX;
   Mes     = MES_BOX;
   gdsbox_c( blo, bhi, Setin, subin, &dfault,
             Key, Mes, &showdev, &boxopt );

   subsetpixels = 1;
   for(i = 0; i < (int) subdim; i++)
      subsetpixels *= (bhi[i] - blo[i] + 1);

   
   if (fpim)
   {
      nitems = 1;
      dfault = REQUEST;
      posform= toflog(NO);
      Key    = tofchar("POSFORM=");
      Mes    = tofchar("Do you want to import data with positions?   Y/[N]" );
      r1     = userlog_c( &posform,
                          &nitems,
                          &dfault,
                          Key,
                          Mes );
      posform = tobool( posform );
   }
   if (fpim && posform)
   {
      readposdata( Setin, subin[0], fpim, blo, bhi );
      fclose(fpim);
      finis_c();   
      return(EXIT_SUCCESS);
   }
        
   
   if (fpim)
   {
      /* Create buffer with size 'subsetpixels' */
      databuf = (float *) calloc( subsetpixels, sizeof(float) );
      if (databuf == NULL)
         anyoutf( 1, "Cannot allocate memory to read data from file" );
   }

   sheet = NO;
   if ((subdim == 2) && (fpex))
   {
      /* User can create a spreadsheet file */
      nitems = 1;
      dfault = REQUEST;
      sheet  = toflog(NO);
      Key    = tofchar("SHEET=");
      Mes    = tofchar("Do you want a spreadsheet output?    Y/[N]");
      r1     = userlog_c( &sheet,
                          &nitems,
                          &dfault,
                          Key,
                          Mes );
      sheet = tobool( sheet );
   }

   if (fpex)
   {
      fmake( Formatstr, STRLEN );
      strcpy( Formatstr.a, "ffffffffff.fffff" );
      Key     = tofchar("FORMAT=");
      (void) sprintf( message, "Give format for image data output:     [%.*s]",
                      nelc_c(Formatstr), Formatstr.a );
      Mes     = tofchar(message);
      r1      = usercharu_c( Formatstr, &nitems, &dfault, Key, Mes );

      nitems  = 1;
      dfault  = REQUEST;
      setdblank_c( &replace );
      Key     = tofchar("BLANK=");
      Mes     = tofchar("Value to replace a blank:    [do not replace]");
      r1      = userdble_c( &replace,  &nitems, &dfault, Key, Mes );
      
      skipblank = toflog(NO);
      dfault  = HIDDEN;
      r1 = userlog_c( &skipblank, &nitems, &dfault, tofchar("SKIPBLANK="),
                      tofchar("Skip writing blanks in file:   [NO]") );
      skipblank = tobool( skipblank );
   }

   toxyz = NO;
   if (subdim == 2 && fpex && !sheet)
   {
      /* User can create XYZ file */
      nitems = 1;
      dfault = REQUEST;
      toxyz  = toflog(NO);
      Key    = tofchar("XYZFORM=");
      Mes    = tofchar("Do you want output in X-Y-data format?    Y/[N]");
      r1     = userlog_c( &toxyz,
                          &nitems,
                          &dfault,
                          Key,
                          Mes );
      toxyz = tobool( toxyz );

      if (toxyz)
      {
         nitems = 1;
         dfault = REQUEST;
         tophys = toflog(NO);
         Key    = tofchar("TOPHYS=");
         Mes    = tofchar("Do you want x and y in physical coords.:  [N]");
         r1     = userlog_c( &tophys,
                             &nitems,
                             &dfault,
                             Key,
                             Mes );
         tophys = tobool( tophys );
      }
   }



   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/

   fmake( Prusingstr, STRLEN );
   for(subnr = 0; subnr < nsubs; subnr++)
   {
      cwlo   = gdsc_fill_c( Setin, &subin[subnr], blo );
      cwhi   = gdsc_fill_c( Setin, &subin[subnr], bhi );
      if (fpex)
      /*--------------------------------------------------*/
      /* Write to an ASCII file.                          */
      /*--------------------------------------------------*/      
      {
         if (sheet)
         /*--------------------------------------------------*/
         /* Write to file in rows and columns. Data separated*/
         /* by a comma.                                      */
         /*--------------------------------------------------*/
         {
            fint     lx = bhi[0] - blo[0] + 1;
            fint     dlo[2];
            fint     dhi[2];
            fint     y;
            fint     l;
            fint     tot;
            char     line[MAXLINELEN];
            double   value;

            for (y = bhi[1]; y >= blo[1]; y--)
            {
               tid    = 0;
               dlo[0] = blo[0];
               dlo[1] = y;
               dhi[0] = bhi[0];
               dhi[1] = y;

               line[0] = 0;
               cwlo   = gdsc_fill_c( Setin, &subin[subnr], dlo );
               cwhi   = gdsc_fill_c( Setin, &subin[subnr], dhi );
               gdsi_read_c( Setin,
                            &cwlo, &cwhi,
                            image,
                            &lx,
                            &pixelsread,
                            &tid );
               if (image[0] == blank)
                  value = replace;
               else
                 value = (double) image[0];

               tot = printusing_c( Formatstr, &value, Prusingstr );
               (void) sprintf( line, "%.*s", tot, Prusingstr.a );
               for (i = 1; i < pixelsread; i++)
               {
                  if (image[i] == blank)
                     value = replace;
                  else
                     value = (double) image[i];
                  l = printusing_c( Formatstr, &value, Prusingstr );
                  if (tot < (MAXLINELEN-l))
                     tot = sprintf( line , "%.*s , %.*s", strlen(line), line,
                                    l, Prusingstr.a );
               }
               strcat( line, "\n" );
               fprintf( fpex, "%s", line );
            }
         }
         else
         /*--------------------------------------------------*/
         /* Data in 'Z' or 'XYZ' format. One pixel value on  */
         /* a line. Conversion of position X,Y to physical   */
         /* values is possible if TOPHYS=Y                   */
         /*--------------------------------------------------*/
         {
            int    x, y;
            int    xlen;
            int    j = 0;

            tid    = 0;
            xlen   = bhi[0] - blo[0] + 1;

            do
            {
               /* Read 'maxIObuf' values in 'image'. */
               (void) gdsi_read_c( Setin,
                                   &cwlo, &cwhi,
                                   image,
                                   &maxIObuf,
                                   &pixelsread,
                                   &tid );
               for (i = 0; i < pixelsread; i++, j++)
               {
                  double    value;
                  int       len;

                  if (image[i] == blank)
                     value = replace;
                  else
                     value = (double) image[i];

                  len = printusing_c( Formatstr, &value, Prusingstr );
                  if (!toxyz)
                  {
                     if (value == blank && skipblank)
                     { /* Do nothing */
                     }
                     else
                        fprintf( fpex, "%.*s\n", len, Prusingstr.a );
                  }
                  else
                  {
                     /*------------------------------------------*/
                     /* User wants file with  'X, Y, image data' */
                     /*------------------------------------------*/
                     y = blo[1] + j/xlen;
                     x = blo[0] + (j%xlen);
                     if (!tophys)
                        if (value == blank && skipblank) 
                        {}
                        else
                           fprintf( fpex, "%6d %6d %.*s\n", x, y, len, Prusingstr.a );
                     else
                     {
                        fint direction = 1; /* grid coord. -> physical coord. */
                        fint res;
                        double XYin[2];
                        double XYout[MAXAXES];
                        XYin[0] = (double) x;
                        XYin[1] = (double) y;
                        res = cotrans_c( Setin,
                                         &subin[subnr],
                                         XYin,
                                         XYout,
                                         &direction );
                        if (value == blank && skipblank)
                        {}
                        else
                           fprintf( fpex, "%12.12f %12.12f %.*s\n",
                                    XYout[axnum[0]-1],
                                    XYout[axnum[1]-1],
                                    len, Prusingstr.a );
                     }
                  }
               }
            } while (tid != 0);
         }
      }
      if (fpim)
      {
         tid = 0;
         readfromascii( fpim, subsetpixels, databuf );
         gdsi_write_c( Setin, &cwlo, &cwhi, databuf,
                       &subsetpixels, &pixelsdone, &tid );
      }
   }
   /*-------------------------------------------------------*/
   /* To end the program, make sure files opend with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   if (fpim)
      fclose(fpim);
   if (fpex)
      fclose(fpex);
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
