/*
                            COPYRIGHT (c) 1995
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             median.dc1

Program:       MEDIAN

Purpose:       Calculate median of data from set, file, table or
               expression.
               
Category:      CALCULATION

File:          median.c

Author:        M.G.R. Vogelaar

Keywords:


** FILENAME=   Name of ASCII file:                  [No output to file]

               Write result (median, number of pixels, number of
               blanks) to Ascii file on disk. If the file already 
               exists, the keyword OVERWRITE= is prompted.

               Example of contents:
               
               ! Median ---- total number of pixels ---- number of blanks
               0.000000  1000  0



   APPEND=     File exists, ok to append?                        [Y]/N
   
               Only asked if FILENAME= is a name of an existing
               file on disk.


   INSET=      Give input set (, subsets):                [other input]
               Maximum number of subsets is 2048.
               The default, carriage return, prompts with DATA=
               If subsets are defined, then all data will be 
               treated as one array. The result is ONE median.


   BOX=        Give box in .....                        [entire subset]
   
  
   DATA=       Enter data (from file/table/expression):
               Standard input for floating point numbers.
               See examples.
               


Notes:         This program stores the result in the header of
               INSET= using keyword MEDIAN
               The value is stored as a real, not as a double.


               There is a hidden keyword FAST= that selects an 
               alternative method to calculate a median by selecting
               the k largest elements in an array. The default is FAST=N. 
               To examine the (experimental) method, type FAST=Y


               Given an array with N data values extracted from set, 
               file or table:
               MEDIAN will remove all blanks in that array before
               sorting. After sorting, we have a new array with length M.
               and the median is calculated as:
               1) If M is odd, the median is the k-th element with
                  k = (M+1)/2
               2) When M is even, the median is the arithmetic mean
                  of the elements k=M/2 and k=M/2+1.
               

Examples:      INPUT OF NUMBERS WITH DATA=
               ===========================

               The input of the array data follows the rules of
               input of floating point numbers. Probably you want to
               use one of the database or file functions 'table',
               'image', or 'file'.

               FILE
               ====
               Syntax for reading from file:

                       keyword=file(filename,column,rows)

               and the syntax for 'rows' is as for recall files.


               ex.1:  DATA=file(profile.txt,2,3:20)

               reads from ASCII file profile.txt the second column.
               It starts reading at row 1 and it wil read until
               line 20 is read.

               ex.2:  DATA=file(profile.txt,2,1:)

               reads from ASCII file profile.txt the second column.
               It starts reading at row 1 and it will read until
               the end of that column.


               IMAGE
               =====
               Syntax for reading image data:

                           keyword=image(set, box)

               'set' is the set/subset specification as known from
               the INSET= keywords. 'box' sets the limits as in the
               BOX= keywords. Suppose we have a 2-dim RA/DEC GIPSY set
               called 'profset', then:

               ex.3:  YARRAY=image(profset dec 0, -14 15)

               reads profile data in the RA direction at DEC=0.
               It starts reading at RA=-14 and it stops reading
               image data after RA=15


               TABLE
               =====
               Syntax for reading data from a table:

                      keyword=table(set, tab, col, rows)

               'set' is the set/subset specification as known from
               the INSET= keywords. 'tab' is the name of the GDS table,
               'col' is the name of a column in that table and rows
               indicate the rows in that column.
               Set 'profset' has a table called 'tab1'. This table
               has two columns named 'X' and 'Y':

               ex.4:  DATA=table(profset,tab1,X,1:39)

               reads row 1 to 39 from column 'X' from table 'tab1'
               in set 'profset'.



Updates:       Jun 19, 1995: VOG, Document created.
               Feb  1, 2000: JPT, Increased number of subsets.
#<
*/

/*  median.c: include files     */

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
#include    "timer.h"        /* Returns the cpu time and real time. */


/* User input routines */

#include    "userfio.h"      /* Easy-C companions for user interface routines.*/
#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"      
#include    "userreal.h"     
#include    "userdble.h"     
#include    "usertext.h"     
#include    "usercharu.h"    
#include    "dcdreal.h"
#include    "dcderrstr.h"    /* Obtain an error message, given a DECODExxx error code.*/
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
#include    "gdsd_wreal.h"
#include    "gdsd_wfits.h"


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
#define SWAP(a,b)      temp=(a);(a)=(b);(b)=temp


#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     2048       /* Max. allowed subsets */
#define MAXBUF         4096       /* Buffer size for I/O */
#define STRLEN         256        /* Max length of strings */
#define FILENAMELEN    256        /* Max length of file names */
#define FITSLEN        20         /* Max length of header items etc.*/
#define BUFFEROVERFLOW -23        /* Return code for dcd*** functions */
#define NONE           0          /* Default levels in userxxx routines */
#define REQUEST        1          
#define HIDDEN         2          
#define EXACT          4          
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0          

/* Defines for in/output routines etc.*/

#define KEY_INSET         tofchar("INSET=")
#define MES_INSET         tofchar("Give input set (, subsets):    [other input]")
#define KEY_BOX           tofchar("BOX=")
#define MES_BOX           tofchar(" ")
#define KEY_DATA          tofchar("DATA=")
#define KEY_FILENAME      tofchar("FILENAME=")
#define MES_FILENAME      tofchar("Name of ASCII file:     [No output to file]")
#define KEY_APPEND        tofchar("APPEND=")
#define MES_APPEND        tofchar("File exists, ok to append?    [Y]/N")
#define KEY_FAST          tofchar("FAST=")
#define MES_FAST          tofchar("Use (inaccurate) fast method?     Y/[N]")




/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered ccording to the */
                                    /* specification by the user. */
static fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
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
static fint     subdim;             /* Dimensionality of the subsets for class 1 applications */


/* Box and frame related */

static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */


/* Reading data */


static fint     subnr;              /* Counter for subset loop. */


/* Miscellaneous */

static float    blank;              /* Global value for BLANK. */
static char     message[STRLEN];    /* All purpose character buffer. */
static bool     agreed = NO;        /* Loop guard. */
FILE            *fpOUT = NULL;      /* File for table data */
static char     filename[256];



static void errorC( int  level,
                    char *str )
/*-----------------------------------------------------------*/
/* PURPOSE: User error handling routine.                     */
/* The C version of 'error'.                                 */
/* 1 = Warning, 2 = Minor error, 3 = Serious error,          */
/* 4 = Fatal error                                           */
/*-----------------------------------------------------------*/
{
   fint flev = (fint) level;
   error_c( &flev, tofchar( str ) ); 
}



static int compare( float *f1, 
                    float *f2 )
/*-----------------------------------------------------------*/
/* PURPOSE: */
/*-----------------------------------------------------------*/
{
   if (*f1 < *f2)
      return( -1 );
   else if (*f1 > *f2)
      return( 1 );
   return( 0 );
}



static float getmedian( float *data,
                        int   n )
/*-----------------------------------------------------------*/
/* PURPOSE: Return the median of numbers in 'data'.          */
/*-----------------------------------------------------------*/
{
   qsort( data, n, sizeof(float), (int(*)())compare );   
   if (n%2)                                /* 'n' odd */
      return( data[(n+1)/2-1] );
   return( 0.5 * (data[n/2-1] + data[n/2]) );
}



static void getmedian2( float   *data,
                        int      ndata,
                        float   *median )
/*-----------------------------------------------------------*/
/* PURPOSE: Num. rep. method for selecting M largest in array*/
/*-----------------------------------------------------------*/
{
   int     i, ir, j, k, l, mid;
   float   a, temp = 0.0;


   if (ndata%2)
      k = (ndata+1) / 2;    /* odd */
   else
      k = ndata / 2;        /* even */
      
   l = 1;
   ir = ndata;
   data--;                          /* Subscript must start at 1 !! */
   for (;;)
   {
      if (ir <= l+1)
      {
         if (ir == l+1 && data[ir] < data[l])
            SWAP( data[l], data[ir] );
         *median = data[k];
/*anyoutf( 1, "ndata=%d k=%d k-1=%f k=%f k+1=%f", ndata, k, data[k-1],data[k],data[k+1]);*/
         return;
      }
      else
      {
         mid = (l+ir) >> 1;
         SWAP( data[mid], data[l+1] );
         if (data[l+1] > data[ir])
            SWAP( data[l+1], data[ir] );
         if (data[l] > data[ir])
            SWAP( data[l], data[ir] );
         if (data[l+1] > data[l]) 
            SWAP( data[l+1], data[l] );
         i = l + 1;
         j = ir;
         a = data[l];
         for(;;)
         {
            do 
               i++; 
            while (data[i] < a && i < ndata);
            do 
               j--; 
            while (data[j] > a && j > 1); 
            if (j < i)
               break;
            SWAP( data[i], data[j] );
         }
         data[l] = data[j];
         data[j] = a;
         if (j >= k)
            ir = j - 1;
         if (j <= k)
            l = i;
      }
   } 
}



FILE *fopenC( char  *filename, 
              bool  *appended )
/*--------------------------------------------------*/
/* Open file to write data extern. The              */
/* macro 'fmake' must be available.                 */
/*--------------------------------------------------*/
{
   bool     append;
   bool     fileexist;
   fint     dfault;
   fint     n;
   fint     nitems;
   fint     agreed;
   FILE     *fp = NULL;


   *appended = NO;
   dfault = HIDDEN;
   do 
   {
      fchar  Filename;
      fmake( Filename, FILENAMELEN );      
      
      n = usertext_c( Filename,
                      &dfault,
                      KEY_FILENAME,
                      MES_FILENAME );
      if (n == 0) 
         return( NULL );

      /* Copy string and add a nul */
      filename[  sprintf( filename, "%.*s", nelc_c(Filename), Filename.a )  ]; 
     
      fp = fopen( filename, "r" );
      fileexist = (fp != NULL);
      
      if (fileexist) 
      {                                   /* The file exists */
         nitems = 1;
         append = toflog(YES);            /* Default is appending to existing file */
         dfault = REQUEST;
         n = userlog_c( &append,
                        &nitems,
                        &dfault,
                        KEY_APPEND,
                        MES_APPEND );
         append = tobool( append );
         fclose( fp );
         cancel_c( KEY_APPEND );
      }
      else
         append = NO;
         
      if (fileexist && !append) 
      {
          cancel_c( KEY_FILENAME );
          agreed = NO;
      }
      else 
      {
         fp = fopen(filename, "a");      /* Open for appending */
         agreed = (fp != NULL);
         if (!agreed) 
            reject_c( KEY_FILENAME, tofchar("Cannot open, try another!") );
      }
      dfault = REQUEST;                  /* If something went wrong, unhide keywords */
   } 
   while (!agreed);
   
   *appended = append;
   return( fp );
}



static void storeinheader( fchar  Setin,
                           float  median )
/*-----------------------------------------------------------*/
/* PURPOSE: Store median in FITS item MEDIAN on top level.   */
/*-----------------------------------------------------------*/
{
   fint    r = 0;
   fint    setlevel = 0;
   
   gdsd_wreal_c( Setin, tofchar("MEDIAN"), &setlevel, &median, &r );
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
   fint     r;
   float    *image = NULL;
   float    median = 0.0;
   int      i, j;   
   int      ndata = 0;
   int      nblanks;
   bool     append;
   bool     fastmethod;
   bool     setused;
   double   cputime, realtime;           /* Variables for timer */
   fint     elapse;
   

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

   anyoutf( 1, "Write result to Ascii file with 'hidden' keyword FILENAME=" );
   anyoutf( 1, "==========================================================" );
   anyoutf( 1, " " );


   dfault  = HIDDEN;
   nitems = 1;
   fastmethod = toflog( NO );
   r = userlog_c( &fastmethod, &nitems, &dfault, KEY_FAST, MES_FAST );
   fastmethod = tobool( fastmethod );
   

   fpOUT = fopenC( filename, &append );
   /*--------------------------------------------------*/
   /* Get the input set. Documentation can be found in */
   /* $gip_sub/gdsinp.dc2                              */
   /*--------------------------------------------------*/
   {
      fmake( Setin, STRLEN );
      dfault  = REQUEST;
      subdim  = 0;                  /* Allow only 2-dim structures */
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
   
   elapse = 0;
   timer_c( &cputime, &realtime, &elapse );   /* Reset timer */

   setused = (nsubs > 0);   
   if (setused)
   {
      fint     boxopt = 0;         /* The different options are: */
      int      buflen;

      dfault = REQUEST;
      gdsbox_c( blo, bhi, Setin, subin, &dfault, 
                KEY_BOX, MES_BOX, &showdev, &boxopt );
   
      buflen = 1;
      for (i = 0; i < subdim; i++)
         buflen *= bhi[i] - blo[i] + 1;
         
      /* Try to allocate a buffer of this size */
      image = (float *) calloc( buflen, sizeof(float) );
      if (image == NULL)
      {
         sprintf( message, "Cannot allocate memory for %d floats!", buflen );
         errorC( 4, message );
      }

      /*------------------------------------------------------------*/
      /* Start the main loop over all subsets. Calculate for each   */
      /* subset new coordinate words and reset the transfer id's    */
      /*------------------------------------------------------------*/      
      for(subnr = 0; subnr < nsubs; subnr++)
      {
         fint  tid = 0;               /* Transfer id's */
         fint  cwlo, cwhi;            /* Coordinate words */
         fint  pixelsread;            /* Number of pixels read by read routine. */
         fint  maxIObuf = buflen;     /* Maximum size of read buffer. */

         cwlo   = gdsc_fill_c( Setin, &subin[subnr], blo );
         cwhi   = gdsc_fill_c( Setin, &subin[subnr], bhi );
         tid    = 0;
         gdsi_read_c( Setin,
                      &cwlo, &cwhi,
                      image,
                      &maxIObuf,
                      &pixelsread,
                      &tid );

         ndata = pixelsread;                      
      }
   }
   else
   /*--------------------------------------------------*/
   /* No set was entered. User wants data from file or */
   /* table or a manual entry of data.                 */
   /*--------------------------------------------------*/
   {
      fint   r;
      int buflen = 16*1024;
      image = (float *) calloc( buflen,  sizeof(float) );
      if (image == NULL)
      {
         sprintf( message, "Cannot allocate memory for %d floats!", buflen );
         errorC( 4, message );
      }
         
      /*--------------------------------------------------*/
      /* Repeat the input of data for 'image' until a     */
      /* correct input is entered AND enough memory is    */
      /* (dynamically) allocated).                        */
      /*--------------------------------------------------*/
      dfault = NONE;
      do
      {
         fint   err = 0;
         fint   len = buflen;
         fchar  Dummy;
         agreed = YES;
         fmake( Dummy, 256 );
         (void) usertext_c( Dummy,
                            &dfault,
                            KEY_DATA,
                            tofchar("Enter data (from file/table/expression): ") );
         r = dcdreal_c( Dummy, image, &len, &err );
         agreed = (err >= 0);
         if (err == BUFFEROVERFLOW)
         {
            dfault = REQUEST;
            buflen *= 1.5;
            image = realloc( image, buflen*sizeof(float) );
            if (image == NULL)
            {
               sprintf( message, "Cannot allocate memory for %d floats!", buflen );
               errorC( 4, message );
            }
         }
         if (!agreed && err != BUFFEROVERFLOW)
         {
            fchar  Errstr;
            fmake( Errstr, 40 );
            dfault = NONE;
            dcderrstr_c( Errstr, &err );
            reject_c( KEY_DATA, Errstr );
         }
      }
      while ( !agreed );            
      ndata = r;
   }
   
   for (i = 0, j = 0; i < ndata; i++)
   {
      if (image[i] != blank)
         image[j++] = image[i];
   }
   nblanks = ndata - j;
   ndata = j;
   if (!fastmethod)   
      median = getmedian( image, ndata ); 
   else 
      getmedian2( image, ndata, &median );

   elapse = 1;
   timer_c( &cputime, &realtime, &elapse );   /* Get cpu seconds */
               
   anyoutf( 3, " ");
   if (median == blank)
      anyoutf( 1, "Could not find a median. Perhaps all data are blank?");
   else
   {
      if (fastmethod)
         anyoutf( 3, "(Fast) Median of these %d numbers: %f (%d blanks)", 
                  ndata, median, nblanks );
      else
         anyoutf( 3, "Median of these %d numbers: %f (%d blanks)",
                  ndata, median, nblanks );
      if (setused)
         storeinheader( Setin, median );
   }
   anyoutf( 3, "Program calculated median in %.2f sec (%.2f cpu sec)",
            realtime, cputime );
   
             
   if (fpOUT != NULL)
   {
      if (!append)
         if (fastmethod)
            fprintf( fpOUT, "!       Median --- total # of pixels --- number of blanks\n" );
         else
            fprintf( fpOUT, "!(FAST) Median --- total #  of pixels --- number of blanks\n" );
      fprintf( fpOUT, "%16f  %10d  %10d\n", median, ndata, nblanks );
      fclose( fpOUT );
   }

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   if (image) 
      free( image );
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
