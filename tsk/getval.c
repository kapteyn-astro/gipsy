/*
                            COPYRIGHT (c) 1998
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             getval.dc1

Program:       GETVAL

Purpose:       Get image values at given positions in a set

Category:      UTILITY

File:          getval.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set (,subset):
 
               Maximum number of subsets is 1.
               The set/subset can have any dimension.


   OUTFILE=    Name of output file:                 [GETVAL_output.txt]
 
               ASCII file on disk which contains the positions in
               POSITION= and the corresponding image values. The 
               image values are formatted according the string 
               entered in FORMAT=
                 
  
   APPEND=     Ok to append?                                      [Y]/N
  
               ASCII file on disk exists. Is it ok to append?
               
  
   FORMAT=     Give format for output:                [fffffffff.fffff]
 
               Print numbers on screen or save numbers in file in user 
               given output format.
               See description for possible formats
  
 
   POSITION=   Enter position in (......):                       [STOP]
     
               Positions are entered as grids or physical coordinates
               (or mixed) according to the GIPSY rules for coordinate
               input. Each position consists of n numbers where n is the 
               dimension of INSET=
               You can abort the loop by pressing [enter].
               If you want to read coordinates from file then this file
               must have extension '.rcl' and you can use it in the 
               following way: POSITION=<myfile
               (where myfile.rcl is the file on disk).
               Examples:
               
               POSITION= 10 -24
               Pixel position RA=10, DEC=-24
               
               POSITION= * 10 12 8    -24
               RA = 10 hours, 12 minutes, 8 seconds,
               DEC = pixel -24
               
               POSITION= * 10 12 8 * -67 8 9.6 
               RA = 10 hours, 12 minutes, 8 seconds, 
               DEC = -67 degrees, 8 minutes, 9.6 seconds, in a 2-dimensional
               area and in the epoch as found in the descriptor of the set. 
                     
               POSITION= *2000.0 3 14 38.02 *2000.0 41 13 54.84 
               Input of RA 3 h 14 m 38.02 s, DEC 41 d 13 m 54.84 s in 
               epoch 2000.0 
               
               You can also use the prefixes 'G' and 'E': 
             
               G        Galactic longitude or latitude in degrees
               E        Ecliptic longitude or latitude in degrees
               
              

Description:   Program GETVAL gets image values from INSET= at user
               given positions in POSITION=
               The position keyword is asked in a loop. Coordinates
               within the frame of the set and with an image value
               that is not blank, are plotted on screen and saved to disk 
               in file OUTFILE= together with the image value. The 
               image value is formatted according to the string in FORMAT=
               Example output from RA, DEC positions in a set:
               
               RA         DEC           IMAGEVALUE
               ===================================
                0.000000  0.000000      9854.73340
                1.000000  1.000000     10071.29688
                3.000000  3.000000      7277.25098
                3.000000  3.000000      7277.25098
               -2.000000 -2.000000      7707.51221 
              
               Note: The header is not part of the output.
               Note: Positions that correspond to a blank image value
                     are not saved to file. However a message appears
                     in the GIPSY LOG file.
               
              
               The program is written for people who want to know image 
               values in a set on certain positions without using
               interpolation (as for instance in program TRACE).
               GETVAL is different from PRINT because is works on
               arbitrary positions and not on boxes and its output
               is directed to an ASCII file on disk. Also GETVAL
               differs from GDS2TEXT because the latter works on a
               box only.
            

               FORMATS
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




Notes:         

Example:       See above.

Updates:       Apr 23, 1998: VOG, Document created.

#<
*/

/*  getval.c: include files     */

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


/* User input routines */

#include    "userfio.h"      /* Easy-C companions for user interface routines.*/
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
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "gdsc_name.h" 
#include    "wkey.h"


#include    "printusing.h"


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
#define RAD(a)         ( (a) * 0.017453292519943295769237 )
#define DEG(a)         ( (a) * 57.295779513082320876798155 )

#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     1       /* Max. allowed subsets */
#define MAXBUF         4096       /* Buffer size for I/O */
#define STRLEN         256        /* Max length of strings */
#define FILENAMELEN    256        /* Max length of file names */
#define FITSLEN        20         /* Max length of header items etc.*/
#define NONE           0          /* Default levels in userxxx routines */
#define REQUEST        1          
#define HIDDEN         2          
#define EXACT          4          
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0          


/* Miscellaneous globals */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static char     message[STRLEN];    /* All purpose character buffer. */



/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (,subset):")
#define KEY_FILENAME   tofchar("OUTFILE=")
#define KEY_APPEND     tofchar("APPEND=")





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




FILE *fopenC( char *filename )
/*---------------------------------------------*/
/* Open file to write data extern. The         */
/* macro 'fmake' must be available.            */
/* If file exists, ask user if it is ok        */
/* to append.  'filename' is the default name. */
/* It is also the filename on return.          */
/*---------------------------------------------*/
{
   fchar    Filename;
   bool     append;
   fint     dfault;
   fint     n;
   fint     nitems;
   fint     agreed;
   FILE     *fp;
   char     mes1[STRLEN];


   dfault = REQUEST;   
   Filename.l = STRLEN;
   Filename.a = filename;
   sprintf( mes1, "Name of output file:   [%s]", filename );
   do {
      append = toflog(YES);                               /* Default APPEND=Y */
      n = usertext_c( Filename,
                      &dfault,
                      KEY_FILENAME,
                      tofchar( mes1 ) );
      if (n == 0)
         n = strlen( filename );
         
      if (n == 0) 
         return NULL;

      sprintf( filename, "%.*s", nelc_c(Filename), Filename.a );

      fp = fopen(filename, "r");
      if (fp != NULL) {                                    /* The file exists */
         nitems = 1;
         n = userlog_c( &append,
                        &nitems,
                        &dfault,
                        KEY_APPEND,
                        tofchar("Ok to append?       [Y]/N") );
         append = tobool( append );
         fclose( fp );
         cancel_c( KEY_APPEND );
      }
      if (!append) {
          cancel_c( KEY_FILENAME );
          agreed = NO;
      }
      else {
         fp = fopen(filename, "a");
         agreed = (fp != NULL);
         if (!agreed) {
            reject_c( KEY_FILENAME,
                      tofchar("Cannot open, try another!") );
         }
      }
   } while (!agreed);
   return( fp );
}




static void findaxnames( char  *axnames,
                         fchar  Setin,
                         fint   subdim,
                         fint  *axnum )
/*
 *------------------------------------------------------------------------------
 * Find names of axes in format (RA,DEC) etc.
 *------------------------------------------------------------------------------
 */
{
   int    n;
   char   axis_b[20+1];
   fchar  Ctype;
   fint   err = 0;
   char   dummystr[STRLEN];


   (void) sprintf( axnames,  "(" );
   for (n = 0; n < subdim; n++ ) 
   {
      Ctype.a = axis_b; Ctype.l = 20; axis_b[20] = '\0';
      err = 0;
      gdsc_name_c( Ctype, Setin, &axnum[n], &err );
      if (( n + 1 ) == subdim) 
      {
         /* Space and hyphen! */
         sprintf( dummystr, "%s", strtok( axis_b, " -" ) );
      } 
      else 
      {
         /* Comma added */
         sprintf( dummystr, "%s,", strtok( axis_b, " -" ) );   
      }    
      strcat( axnames, dummystr );
   }
   strcat( axnames, ")" );
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
   fint     class   = 1;                 /* Class 1 is for applications which repeat */
   fint     showdev = 1;
   fint     nsubs;                       /* Number of input subsets */
   fint     dfault;                      /* Default option for input etc */
   fint     nitems;
   fint     r1, r2;
   FILE     *fp;
   char     outfilename[STRLEN];
   char     axnames[STRLEN];
   char     postxt[STRLEN];
   fchar    Formatstr;
   fchar    Prusingstr;
   
   fchar    Setin;              /* Name of input set */
   fint     subin[MAXSUBSETS];  /* Subset coordinate words */
   fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                /* axes numbers.  The first elements (upto */
                                /* the dimension of the subset) contain the */
                                /* axes numbers of the subset, the other */
                                /* ones ontain the axes numbers outside the */
                                /* the subset ordered according to the */
                                /* specification by the user. */
   fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
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
   fint     subdim;             /* Dimensionality of the subsets for class 1 applications */
   fint     setdim;             /* Dimension of set. */

   fint     flo[MAXAXES];       /* Low  edge of frame in grids */
   fint     fhi[MAXAXES];       /* High edge of frame in grids */
   fint     blo[MAXAXES];       /* Low  edge of box in grids */
   fint     bhi[MAXAXES];       /* High edge of box in grids */

  

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

   /*--------------------------------------------------*/
   /* Get the input set. Documentation can be found in */
   /* $gip_sub/gdsinp.dc2                              */
   /*--------------------------------------------------*/
   {
      fmake( Setin, STRLEN );
      dfault  = NONE;
      subdim  = 0;                  /* Allow only all-dim structures */
      nsubs = gdsinp_c( Setin,      /* Name of input set. */
                        subin,      /* Array containing subsets coordinate words. */
                        &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                        &dfault,    /* Default code as is USERxxx. */
                        KEY_INSET,  /* Keyword prompt. */
                        MES_INSET,  /* Keyword message for the user. */
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
   }
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
      for (m = 0; m < (int) setdim; m++)
      {
         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
   }

   strcpy( outfilename, "GETVAL_output.txt" );
   fp = fopenC( outfilename );

   fmake( Prusingstr, STRLEN );
   fmake( Formatstr, STRLEN );
   dfault  = REQUEST;
   nitems  = 1;
   strcpy( Formatstr.a, "fffffffff.fffff" );
   sprintf( message, "Give format for output:     [%s]", Formatstr.a );
   r1 = userchar_c( Formatstr, &nitems, &dfault, tofchar("FORMAT="), 
                    tofchar(message) );

   findaxnames( axnames, Setin, subdim, axnum );   
   sprintf( postxt, "Enter position in %s:     [STOP]", axnames );
   do
   {
      fint  r;
      fint  dfault = REQUEST;
      char  mes2[STRLEN];
      char  mes3[STRLEN];      
      char  mes4[STRLEN];            
      fint  maxpos = 1;
      fint  cwlo, cwhi;
      fint  tid = 0;
      int   ok;
      int   i;
      double pos[MAXAXES];
      fchar  Mes;
      
     
      Mes.a = message;
      Mes.l = STRLEN+1; 
      clearstr( message, STRLEN );
      r = usertext_c( Mes, &dfault, tofchar("POSITION="), tofchar(postxt) );

      if (!r)
         break;
         
      strcpy( mes2, "BOX=" );
      strcat( mes2, message );      
      wkey_c( tofchar(mes2) );
      
      dfault = HIDDEN;
      gdspos_c( pos,
                &maxpos,
                &dfault,
                tofchar("POSITION="),
                tofchar(" "),
                Setin,
                &subin[0] );
                
      cancel_c(tofchar("POSITION="));



      sprintf( mes3, "" );
      ok = YES;
      for (i = 0; i < subdim; i++)
      {
         blo[i] = (float) pos[i];
         bhi[i] = (float) pos[i];
         if (blo[i] < flo[i] || bhi[i] > fhi[i])
         {
            ok = NO;
         }
         sprintf( mes4, "%6f ", pos[i] );
         strcat( mes3, mes4 );         
      }   
      
      if (!ok)
      {
         anyoutf( 3, "%s ==> Outside frame", mes3 );         
      }
      else
      {
         float   imval;
         fint    maxIObuf = 1;
         fint    pixelsread;
         
         tid = 0;
         cwlo   = gdsc_fill_c( Setin, &subin[0], blo );
         cwhi   = gdsc_fill_c( Setin, &subin[0], bhi );
        
         gdsi_read_c( Setin,
                      &cwlo, &cwhi,
                      &imval,
                      &maxIObuf,
                      &pixelsread,
                      &tid );
                      
         if (imval == blank)
         {
            sprintf( mes4, " blank" );
            strcat( mes3, mes4 );
            anyoutf( 3, mes3 );
         }
         else
         {
            fint   l;
            double x = (double) imval;
            l = printusing_c( Formatstr, &x, Prusingstr ); 
            sprintf( mes4, "%.*s\n", nelc_c(Prusingstr), Prusingstr.a );
            strcat( mes3, mes4 );            
            fprintf( fp, mes3 );
            anyoutf( 1, mes3 );
         }         
      }
   }
   while ( 1 );

   fclose( fp );
   anyoutf( 3, "Data written to file [%s] on disk!", outfilename );
    
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
