/*
                           COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                  University of Groningen, The Netherlands
                          All Rights Reserved.


#>             coder.dc1

Name:          CODER

Purpose:       Create C-code for simple programs

Category:      UTILITY, COMPILE

File:          coder.c

Author:        M. Vogelaar

Keywords:

   PROGRAM=    Name of C-program:                             [example]
               Name of C-file (without extension '.c') that
               will be created.


   OVERWRITE=  File exists, overwrite?                            [Y]/N
               If the name in PROGRAM= already exists, you are
               warned. If you select OVERWRITE=N you are prompted to
               give a new name.


   PURPOSE=    Purpose line:                                  [.......]
               Each application document has one line explaining
               the purpose of the application.


   CATEGORY=   Category/ies (separated by comma):                [list]
               Each application belongs to a certain category. To
               obtain a list of available categories press carriage
               return. The input can contain more than one categories,
               each category is separated by a comma. A category
               input can contain spaces, but these spaces are removed
               in the output. The categories are matched, so you need
               to give only a couple of characters for a category.


   AUTHOR=     Name of Author:               [default given by program]
               Each application document must include the name of
               the author. The default is your username.


   MAXSUBSETS= Max number of subsets in your program:            [1024]
               The maximum number of subsets the program can
               deal with.


   OUTPUT=     Do you want to create an output set:               Y/[N]
               If OUTPUT=Y, your new program contains routines
               for creating an writing to a new output set.


   TOASCII=    Do you want to create an ASCII file?               Y/[N]
               If TOASCII=Y, a function will be included which
               returns a pointer to a file if a file name is specified.
               There is also a build in check whether a file already
               exists.


   PGPLOT=     Prepare for using PGPLOT routines:                 Y/[N]
               PGPLOT routines are used for graphic purposes.
               To use these routines in a simple way, some setup
               routines are included in your new program if PGPLOT=Y


   UPDATENAME= Update name:                  [default given by program]
               The update name is a 3 character code, used
               to indicate in the application document who installed
               or updated the program.



Description:   CODER is a document and C-code generator for simple 
               GIPSY programs. Purposes of CODER:

               1) Create a standard application document.
               2) Create a well documented C-program that can be used
                  as a program template.
               3) Illustrate the use of input-, output- and plot
                  functions.

               A standard document includes a copyright notice, a
               program name (PROGRAM=), a purpose line (PURPOSE=),
               a category indication (CATEGORY=), the program name
               to which the document belongs, the author name
               (AUTHOR=), a list of descriptions of the used
               keywords, a description of the program, some notes
               and an example of the use of the program. The final
               item is an update notice including a reference to
               a name (UPDATENAME=). If an item cannot be filled in
               by CODER, a dummy or template is created.
               Default, the coder creates a program that can read
               data from a set and calculates the sum of all
               image values in a given box. The maximum number of
               subsets you want to use in your new program is
               given in MAXSUBSETS=. For most applications the
               default number will be enough.
               CODER creates a number of useful macros. It defines
               macros for creating Fortran compatible character
               strings and macros to control array sizes for
               instance. Also the version of your program is set in
               RELEASE (which is identical to 1.0 if you are using
               CODER). Adjust the definitions to your own taste.If
               for example you want to avoid the use of a small I/O
               buffer in your new program, adjust the value of
               the macro MAXBUF manually after CODER is finished.

               There are two major extensions to this elementary
               GIPSY code: first, you can add some output code
               with OUTPUT=Y. This generates code for creating an
               output set and for writing your input data to
               this output set. A simple operation on the input
               data is also included.
               Second, if PGPLOT=Y, the coder generates program
               lines for the use of PGPLOT graphic routines.
               For the setup of PGPLOT a function initplot is
               generated. The keywords used in this function
               are put and explained in the document of your new
               application. A function to draw a box is also
               included. Again, all these generated code can be
               used as example material or as template code.
               If you are completely new to C-programming, learn
               a bit of C first, read the GIPSY programmers manual
               and use this program to write your first application.


Updates:       May 11,  1992: VOG, Document created.
               Dec 20,  1994: VOG, Read category file in $gip_doc
               Apr 20,  1995: VOG, A lot of minor changes
               Mar 30,  1998: VOG, Changes in defines for RAD and ABS
               May  1,  2007: JPT, Renamed conflicting toascii declaration

#<
*/


#include    "stdio.h"
#include    "stdlib.h"
#include    "string.h"
#include    "math.h"
#include    "cmain.h"          /* Defines the main body for GIPSY application */
#include    "gipsyc.h"         /* Defines the ANSI-F77 types for F to C intf. */
#include    "float.h"                                /* Definition of FLT_MAX */
#include    "time.h"
#include    "ctype.h"    /* Declares ANSI C functions for testing characters. */

#include    "usertext.h"
#include    "userchar.h"
#include    "usercharu.h"
#include    "usercharl.h"
#include    "userint.h"
#include    "userlog.h"
#include    "userfio.h"
#include    "reject.h"
#include    "nelc.h"
#include    "anyout.h"
#include    "error.h"
#include    "getusernam.h"
#include    "cancel.h"
#include    "init.h"
#include    "myname.h"
#include    "finis.h"
#include    "time.h"



/* Initialize string with macro */
#define fmake(fchr,size) { \
                            static char buff[size+1]; \
                            int i; \
                            for (i = 0; i < size; buff[i++] = ' '); \
                            buff[i] = 0; \
                            fchr.a = buff; \
                            fchr.l = size; \
                         }

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )

#define ITEM(a)        15,15,a

#define FILENAMELEN    256
#define MAXLINELEN     120
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1
#define NO             0

#define RELEASE       "1.0"
#define MESLEN         128



static fint  n;
static fint  dfault;
static fint  nitems;
static char  message[MESLEN+1];
static int   i;
static fchar Username;
static fint  maxsubsets;
static bool  pgplot;
static bool  out;
static bool  toasciiz;


#define   KEY_PROGNAME    tofchar("PROGRAM= ")
#define   MES_PROGNAME    tofchar("Name of C-program:     [example.c]")
#define   KEY_OVERWRITE   tofchar("OVERWRITE=")
#define   MES_OVERWRITE   tofchar("File exists, overwrite?    [Y]/N")
#define   KEY_CATEGORY    tofchar("CATEGORY=")



static void errorC( int level,
                    char *str )
/*-----------------------------------------------------------*/
/* PURPOSE: User error handling routine.                     */
/* The C version of 'error'.                                 */
/* 1 = Warning, 2 = Minor error, 3 = Serious error,          */
/* 4 = Fatal error                                           */
/*-----------------------------------------------------------*/
{
   fint   flev = (fint) level;
   error_c( &flev, tofchar( str ) );
}



FILE *fopenC( char *basicname )
/*------------------------------------------------------------*/
/* Open file to write data extern. The macro 'fmake' must be  */
/* available. Return the name of the file and a file pointer. */
/* Do not include the extension .c in the file name.          */
/*------------------------------------------------------------*/
{
   fchar    Filename;
   bool     overwrite = toflog( YES );
   fint     dfault;
   fint     n;
   fint     nitems;
   fint     agreed;
   fint     len;
   FILE     *fp;


   fmake( Filename, FILENAMELEN );
   /*--------------------------------------------------*/
   /* Get the name of the program that will be made.   */
   /*--------------------------------------------------*/      
   do 
   {
      overwrite = toflog( YES );                       /* Default OVERWRITE=Y */
      strcpy( Filename.a, "example.c" );
      dfault = REQUEST;      
      nitems = 1;
      n = usercharl_c( Filename,
                       &dfault,
                       &nitems,
                       KEY_PROGNAME,
                       MES_PROGNAME );

      len = MYMIN( FILENAMELEN-1, nelc_c(Filename) );
      Filename.a[len] = '\0';
      if (Filename.a[len-2] == '.' && Filename.a[len-1] == 'c')
         Filename.a[len-2] = '\0';

      strcpy( basicname, Filename.a );
      strcat( Filename.a, ".c" );
      /*--------------------------------------------------*/
      /* Open the file and if it exists, ask permission   */
      /* to overwrite.                                    */
      /*--------------------------------------------------*/   
      fp = fopen( Filename.a, "r" );
      if (fp != NULL)                                      /* The file exists */
      {
         nitems = 1;
         n = userlog_c( &overwrite,
                        &nitems,
                        &dfault,
                        KEY_OVERWRITE,
                        MES_OVERWRITE );
         overwrite = tobool( overwrite );
         fclose( fp );
         cancel_c( KEY_OVERWRITE );
      }
      if (!overwrite) 
      {
          cancel_c( KEY_PROGNAME );
          agreed = NO;
      }
      else 
      {
         fp = fopen( Filename.a, "w" );
         agreed = (fp != NULL);
         if (!agreed) 
            reject_c( KEY_PROGNAME,
                      tofchar("Cannot open, try another!") );
      }
   } while (!agreed);
   return( fp );
}



static int year( void )
/*------------------------------------------------------------*/
/* Get current year. Return year as integer.                  */
/*------------------------------------------------------------*/
{
   struct tm  *ptr;
   time_t     lt;
   
   lt    = time(NULL);                         /* Get the coded calendar time */
   ptr   = localtime(&lt);
   return( ptr->tm_year + 1900.0 );
}



static char *datestr( char *buffer )
/*------------------------------------------------------------*/
/* Description: Return date in format : Oct 14, 1992          */
/*------------------------------------------------------------*/
{
   struct tm  *ptr;
   time_t     lt;

   
   lt    = time(NULL);                         /* Get the coded calendar time */
   ptr   = localtime(&lt);
   strftime( buffer, MESLEN, "%b %d, %Y", ptr );
   return( buffer );
}



static int catok( char *ident, fchar Categories, char *catout )
/*------------------------------------------------------------*/
/* Open de file $gip_doc/categories.doc and list categories   */
/* for given identification ('ident'). This identification is */
/* one of #tasks:  #subroutines:   #general:                  */
/* (See also documentation $gip_doc/categories.doc)           */
/*------------------------------------------------------------*/
{
   FILE       *fp;
   char       *filename = "categories.doc";
   char       *path_file;
   char       fbuf[FILENAMELEN+1];
   char       cbuf[MESLEN+1];   
   int        list;
   int        len;
   int        items  = 0;
   int        valids = 0;   
   int        i;
   char       **cats = NULL;
 

   /* Length = path+filename+ '/' + '0' */    
   len = strlen( getenv("gip_doc") ) + strlen( filename ) + 2;
   path_file = malloc( len );
   if (path_file == NULL)
      errorC( 4, "Cannot allocate memory for path and file name!" );
   
   strcpy( path_file, getenv("gip_doc") );
   strcat( path_file, "/" );
   strcat( path_file, filename );
   fp = fopen( path_file, "r" );
   if (fp == NULL) 
   {
      anyoutf( 1, "Cannot open %.*s", MESLEN, path_file );
      free( path_file );
      return( NO );
   }
   
   /*--------------------------------------------------*/
   /* Read lines from file until a line is encountered */
   /* with the text: #tasks:                           */
   /*--------------------------------------------------*/   
   do
   {
      fgets( fbuf, MAXLINELEN, fp );      
   } 
   while( !(strstr( fbuf, ident ) && fbuf[0] == '#') && !feof(fp) );
   
   
   if (feof(fp))
   {
      anyoutf( 1, "Could not find any categories" );      
      free( path_file );
      return( NO );
   }
   
   /*--------------------------------------------------*/
   /* At this point we have found the line containing  */
   /* #tasks: Read the first line after the current.   */
   /* A list of categories is displayed if the variable*/
   /* 'Categories' is empty.                           */
   /*--------------------------------------------------*/
   fgets( fbuf, MAXLINELEN, fp );      
   list = !nelc_c( Categories );
   
   if (!list)
   /*--------------------------------------------------*/
   /* Extract categories from the input Categories     */
   /* string. Category fields must be separated by     */
   /* comma's. A field can contain blanks which are    */
   /* replaced by one '-' character.                   */
   /*--------------------------------------------------*/
   {
      int j, k, p;
      p = nelc_c( Categories );
      j = 0;
      k = 0;
      while (j < p)           /* As long as there are characters in the input */
      {
         int comma = ( Categories.a[j] == ',' );
         int space = ( Categories.a[j] == ' ' );
         if (!comma)
         {
            if (!(k > 0 && cbuf[k-1] == ' ' && space ))
               cbuf[k++] = toupper( Categories.a[j] );
            j++;
         }

         if ( comma || (!comma && j == p) )
         {
            int i;
            cbuf[k] = '\0';
            while (k > 0)
            {
               if (cbuf[k-1] == ' ')
               {
                  cbuf[k-1] = '\0';                     
                  k--;
               }
               else
                  break;
            }
            for (i = 0; i < k; i++)
            {
               if (cbuf[i] == ' ')
                  cbuf[i] = '-';               
            }            
            cats = realloc( cats, (items+1) * sizeof(char *) ); /* Extra pointer */
            if (!cats)
               errorC( 4, "Cannot allocate memory for category strings!" );
            cats[items] = malloc( strlen(cbuf)+1 );
            if (!cats[items])
               errorC( 4, "Cannot allocate memory for category strings!" );
            strcpy( cats[items++], cbuf );
                        
            j++;                        /* Skip comma */
            /* No leading spaces */
            while (isspace(Categories.a[j]) && j < p)
               j++;
            k = 0;
         }               
      }                           
   }

   catout[0] = '\0';                              /* Reset output */
   do
   /*--------------------------------------------------*/
   /* While there are lines in this files that are not */
   /* empty, check the given categories against the one*/
   /* stored in the line from file.                    */
   /*--------------------------------------------------*/   
   {
      int k;      
      if (list)                                   /* Only a list */
         anyoutf( 1, fbuf );
      else
      {
         /* Cut string at comment character */
         for (k = 0; k < strlen(fbuf); k++)
         {
            if (fbuf[k] == '/')                   /* Stop at comment character */
            {
               fbuf[k] = '\0';
               break;
            }
         }
      }
      k = strlen(fbuf) - 1;                       
      while (k >= 0)                              /* Remove trailing spaces */
      {
         if (isspace(fbuf[k]))
            k--;
         else
            break;  
      }
      len = k + 1;                  
      fbuf[len] = '\0';      
         
      if (!list && len)                           
      /*--------------------------------------------------*/
      /* We found a line containing a category. Check all */
      /* input categories against the one from the line   */
      /* from file.                                       */
      /*--------------------------------------------------*/
      {               
         for (i = 0; i < items; i++)
         {
            if (  !strncmp( fbuf, cats[i], strlen(cats[i]) )  )
            {
               if (valids)
                  strcat( catout, ", " );
               strcat( catout, fbuf );
               valids++;
            }
         }                        
      }
      if (!feof(fp))
         fgets( fbuf, MAXLINELEN, fp );
           
   }
   while (!feof(fp) && len);
   
          
   free( path_file );                   /* Free allocated space for file name */
   fclose( fp );                        /* Close file */
   for (i = 0; i < items; i++)
      free( cats[i] );
   free( cats );
   return( valids >= items );
}




MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/   
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */   
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/   
{
   char  basicname[FILENAMELEN+1];
   char  filename[FILENAMELEN+1];
   char  docname[FILENAMELEN+1];
   FILE  *fp;   
   int   agreed;
  
  
   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    task;                /* Name of current task */
      fmake( task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( task );                    /* Get task name */
      task.a[nelc_c(task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( task.a, RELEASE );   /* Show task and version */
   }


   fp = fopenC( basicname );
   (void) sprintf( filename, "%s.c", basicname );
   (void) sprintf( docname, "%s.dc1", basicname );


   /*====== COPYRIGHT =====*/

   fprintf( fp, "/*\n" );
   fprintf( fp, "                            COPYRIGHT (c) %d\n", year() );
   fprintf( fp, "                      Kapteyn Astronomical Institute\n" );
   fprintf( fp, "                University of Groningen, The Netherlands\n" );
   fprintf( fp, "                           All Rights Reserved.\n" );
   fprintf( fp, "\n\n" );
   fprintf( fp, "#>             %s\n\n", docname );

   /*===== PROGRAM NAME =====*/

   /* Create program name upper case */
   for (i = 0; i < strlen(basicname); i++) 
      message[i] = toupper(basicname[i]);

   message[i] = '\0';
   fprintf( fp, "%-*.*s%s\n\n", ITEM("Program:"), message );


   /*===== PURPOSE LINE =====*/
   {
      fchar Purpose;
      fmake(Purpose, 180);
      dfault = REQUEST;
      strcpy( Purpose.a, "......." );
      n = usertext_c( Purpose,
                      &dfault,
                      tofchar( "PURPOSE=" ),
                      tofchar( "Purpose line:      [.......]" ) );
      fprintf( fp, "%-*.*s%.*s\n\n", ITEM("Purpose:"), 
               nelc_c(Purpose), Purpose.a );
   }


   /*===== GET CATEGORY =====*/
   do
   {
      fchar Category;
      dfault = REQUEST;
      agreed = NO;
      do 
      {
         fmake(Category, MESLEN);           /* Must be empty before prompt! */
         n = usertext_c( Category,
                         &dfault,
                         KEY_CATEGORY,
                         tofchar( "Category/ies (separated by comma):    [list]" ) );
         if (!n) 
         {
            (void) catok( "#tasks:", Category, message );
            cancel_c( KEY_CATEGORY );
         }
      } 
      while (!n);
      agreed = catok( "#tasks:", Category, message );      
      if (!agreed)
         reject_c( KEY_CATEGORY, tofchar("Unvalid category/categories!") );
   }
   while (!agreed);
   anyoutf( 1, "Selected category/ies: %s", message );
   fprintf( fp, "%-*.*s%s\n\n", ITEM("Category:"), message );


   /*===== FILE NAME =====*/
   fprintf( fp, "File:          %s\n\n", filename );



   /*===== AUTHOR =====*/
   {
      fmake(Username, 40);
        getusernam_c( Username );
      Username.a[0] = toupper(Username.a[0]);
      sprintf( message, "Name of Author:        [%.*s]",
               nelc_c( Username), Username.a );
      dfault = REQUEST;
      n = usertext_c( Username,
                      &dfault,
                      tofchar("AUTHOR="),
                      tofchar( message ) );
      fprintf( fp, "Author:        %.*s\n\n", nelc_c( Username), Username.a );
   }

   /*===== KEYWORDS =====*/

   /* Ask user the max. number of subsets he wants to use */

   nitems = 1;
   maxsubsets = 1024;
   dfault = REQUEST;
   n = userint_c( &maxsubsets,
                  &nitems,
                  &dfault,
                  tofchar("MAXSUBSETS="),
                  tofchar("Max number of subsets in your program:  [1024]") );

   fprintf( fp, "Keywords:\n\n" );
   fprintf( fp, "   INSET=      Give set, subsets:\n" );
   fprintf( fp, "               Maximum number of subsets is %d.\n", maxsubsets );

   fprintf( fp, "\n\n" );
   fprintf( fp, "   BOX=        Give box in .....                        [entire subset]\n" );


   /* Does user wants to create an output set? */

   out = tobool( NO );
   nitems = 1;
   dfault = REQUEST;
   n = userlog_c( &out,
                  &nitems,
                  &dfault,
                  tofchar( "OUTPUT=" ),
                  tofchar( "Do you want to create an output set:    Y/[N]" ) );
   out = tobool( out );

   if (out) 
   {
      fprintf( fp, "\n\n" );
      fprintf( fp, "   OUTSET=     Give output set (, subsets):\n" );
      fprintf( fp, "               Output set and subset(s) for the result. The number of\n" );
      fprintf( fp, "               output subsets is the same as the number of input sub-\n" );
      fprintf( fp, "               sets. \n" );
   }



   /* Does user wants to create an ASCII file?  */

   toasciiz = tobool( NO );
   nitems = 1;
   dfault = REQUEST;
   n = userlog_c( &toasciiz,
                  &nitems,
                  &dfault,
                  tofchar( "TOASCII=" ),
                  tofchar( "Do you want to create an ASCII file:    Y/[N]" ) );
   toasciiz = tobool( toasciiz );

   if (toasciiz) 
   {
      fprintf( fp, "\n\n" );
      fprintf( fp, "   FILENAME=   Name of ASCII file:                  [No output to file]\n" );
      fprintf( fp, "               If a name is specified, an ASCII file is created to\n" );
      fprintf( fp, "               store data. If you press carriage return, there will \n" );
      fprintf( fp, "               be no output to an ASCII file. If a given name already\n" );
      fprintf( fp, "               exists, APPEND= must be specified.\n" );
      fprintf( fp, "\n\n" );
      fprintf( fp, "   APPEND=     File exists, ok to append?                        [Y]/N\n" );
      fprintf( fp, "               The file specified in FILENAME= already exists. \n" );
      fprintf( fp, "               You can append to this file with APPEND=Y. If APPEND=N \n" );
      fprintf( fp, "               you will be prompted for another filename. \n" );
   }

   /* Does user wants to include pgplot routines? */

   pgplot = tobool( NO );
   nitems = 1;
   dfault = REQUEST;
   n = userlog_c( &pgplot,
                  &nitems,
                  &dfault,
                  tofchar( "PGPLOT=" ),
                  tofchar( "Prepare for using PGPLOT routines:    Y/[N]" ) );
   pgplot = tobool( pgplot );
   if (pgplot) 
   {
      fprintf( fp, "\n\n" );
      fprintf( fp, "   GRDEVICE=   Plot device:                           [List of devices]\n" );
      fprintf( fp, "               Destination of plot, Screen or Hardcopy.\n" );
      fprintf( fp, "\n\n" );
      fprintf( fp, "** PGMOSAIC=   View surface sub divisions in x,y:                 [1,1]\n" );
      fprintf( fp, "               View surface can contain a number of plots in \n" );
      fprintf( fp, "               in X and Y direction (mosaic). Default is 1 plot in \n" );
      fprintf( fp, "               both X- and Y direction.\n" );
      fprintf( fp, "\n\n" );
      fprintf( fp, "** PGPAPER=    Give width(cm), aspect ratio:               [calc, calc]\n" );
      fprintf( fp, "               Aspect ratio is height/width.\n" );
      fprintf( fp, "\n\n" );
      fprintf( fp, "** PGBOX=      Corners of box Xl,Yl,Xh,Yh:     [default by application]\n" );
      fprintf( fp, "               It is possible to overrule the calculated \n" );
      fprintf( fp, "               PGPLOT box size with PGBOX=. The coordinates (x,y) of \n" );
      fprintf( fp, "               the lower point are given first.\n" );
      fprintf( fp, "\n\n" );
      fprintf( fp, "** PGCOLOR=    Give color 1..15:                                    [1]\n" );
      fprintf( fp, "               See description for the available colors.\n" );
      fprintf( fp, "\n\n" );
      fprintf( fp, "** PGWIDTH=    Give line width 1..21:                               [2]\n" );
      fprintf( fp, "\n\n" );
      fprintf( fp, "** PGHEIGHT=   Give character height:                             [1.0]\n" );
      fprintf( fp, "\n\n" );
      fprintf( fp, "** PGFONT=     Give font 1..4:                                      [2]\n" );
      fprintf( fp, "               1  single stroke 'normal' font\n" );
      fprintf( fp, "               2  roman font\n" );
      fprintf( fp, "               3  italic font\n" );
      fprintf( fp, "               4  script font\n" );
   }




   /*===== DESCRIPTION =====*/

   fprintf( fp, "\n\nDescription:   .......\n\n" );
   if (pgplot) {
      fprintf( fp, "               Color indices:\n\n" );
      fprintf( fp, "               0      Background\n" );
      fprintf( fp, "               1      Default (Black if background is white)\n" );
      fprintf( fp, "               2      Red\n" );
      fprintf( fp, "               3      Green\n" );
      fprintf( fp, "               4      Blue\n" );
      fprintf( fp, "               5      Cyan\n" );
      fprintf( fp, "               6      Magenta\n" );
      fprintf( fp, "               7      Yellow\n" );
      fprintf( fp, "               8      Orange\n" );
      fprintf( fp, "               9      Green + Yellow\n" );
      fprintf( fp, "               10     Green + Cyan\n" );
      fprintf( fp, "               11     Blue + Cyan\n" );
      fprintf( fp, "               12     Blue + Magenta\n" );
      fprintf( fp, "               13     Red + Magenta\n" );
      fprintf( fp, "               14     Dark Gray\n" );
      fprintf( fp, "               15     Light Gray\n" );
      fprintf( fp, "               16-255 Undefined\n\n" );

      fprintf( fp, "               Available fonts:\n\n" );
      fprintf( fp, "               1  single stroke \"normal\" font\n" );
      fprintf( fp, "               2  roman font\n" );
      fprintf( fp, "               3  italic font\n" );
      fprintf( fp, "               4  script font\n\n" );
   }



   /*===== NOTES =====*/

   fprintf( fp, "Notes:         .......\n\n" );

  /*===== EXAMPLE =====*/

   fprintf( fp, "Example:       .......\n\n" );

   /*===== UPDATES =====*/

   {
      fchar Updatename;
      fint  start;
      fmake(Updatename, 4);
      start = nelc_c(Username);
      while ( (Username.a[start-1] != ' ') && (start > 0) ) start--;
      Updatename.a[0] = toupper(Username.a[0]);
      Updatename.a[1] = toupper(Username.a[start]);
      Updatename.a[2] = toupper(Username.a[start+1]);
      Updatename.a[4] = '\0';
      sprintf( message, "Update name:        [%.*s]",
               nelc_c(Updatename), Updatename.a );
      dfault = REQUEST;
      nitems = 1;
      n = usercharu_c( Updatename,
                       &nitems,
                       &dfault,
                       tofchar("UPDATENAME="),
                       tofchar( message ) );

      fprintf( fp, "Updates:       %s: %.*s, Document created.\n\n",
               datestr( message ),
               nelc_c( Updatename ),
               Updatename.a );
   }


   /*===== END OF DOCUMENT =====*/

   fprintf( fp, "#<\n*/\n" );


   /*===== INCLUDE FILES =====*/

   fprintf( fp, "\n/*  %s: include files     */\n\n", filename );
   fprintf( fp, "#include    \"stdio.h\"        /* Defines ANSI C input and output utilities */\n" );
   fprintf( fp, "#include    \"stdlib.h\"       /* Defines the ANSI C functions for number */\n");
   fprintf( fp, "                             /* conversion, storage allocation, and similar tasks.*/\n");
   fprintf( fp, "#include    \"string.h\"       /* Declares the ANSI C string functions*/\n");
   fprintf( fp, "                             /* like:strcpy, strcat etc.*/\n");
   fprintf( fp, "#include    \"math.h\"         /* Declares the mathematical functions and macros.*/\n");
   fprintf( fp, "#include    \"cmain.h\"        /* Defines the main body of a C program with */\n");
   fprintf( fp, "                             /* MAIN_PROGRAM_ENTRY and IDENTIFICATION */\n");
   fprintf( fp, "#include    \"gipsyc.h\"       /* Defines the ANSI-F77 types for Fortran to C intface */\n");
   fprintf( fp, "                             /* including def. of char2str,str2char,tofchar,zadd */\n");
   fprintf( fp, "                             /* and macros tobool and toflog */\n");
   fprintf( fp, "#include    \"float.h\"        /* Definition of FLT_MAX etc.*/\n");
   fprintf( fp, "#include    \"ctype.h\"        /* Declares ANSI C functions for testing characters */\n");
   fprintf( fp, "                             /* like: isalpha, isdigit etc. also tolower, toupper.*/\n");
   fprintf( fp, "\n\n/* Common includes */\n\n");
   fprintf( fp, "#include    \"init.h\"         /* Declare task running to HERMES and initialize.*/\n");
   fprintf( fp, "#include    \"finis.h\"        /* Informs HERMES that servant quits and cleans up the mess.*/\n");
   fprintf( fp, "#include    \"anyout.h\"       /* General character output routine for GIPSY programs.*/\n");
   fprintf( fp, "#include    \"setfblank.h\"    /* Subroutine to set a data value to the universal BLANK.*/\n");
   fprintf( fp, "#include    \"error.h\"        /* User error handling routine. */\n");
   fprintf( fp, "#include    \"myname.h\"       /* Obtain the name under which a GIPSY task is being run.*/\n");
   fprintf( fp, "#include    \"nelc.h\"         /* Characters in F-string discarding trailing blanks.*/\n");
   fprintf( fp, "\n\n/* User input routines */\n\n");
   fprintf( fp, "#include    \"userfio.h\"      /* Easy-C companions for user interface routines.*/\n");      
   fprintf( fp, "#include    \"userint.h\"      /* User input interface routines.*/\n");
   fprintf( fp, "#include    \"userlog.h\"      \n");
   fprintf( fp, "#include    \"userreal.h\"     \n");
   fprintf( fp, "#include    \"userdble.h\"     \n");
   fprintf( fp, "#include    \"usertext.h\"     \n");
   fprintf( fp, "#include    \"usercharu.h\"    \n");
   fprintf( fp, "#include    \"reject.h\"       /* Reject user input.*/\n");
   fprintf( fp, "#include    \"cancel.h\"       /* Remove user input from table maintained by HERMES.*/\n");
   fprintf( fp, "\n\n/* Input of sets */\n\n");
   fprintf( fp, "#include    \"gdsinp.h\"       /* Input of set, subsets, return # subsets.*/\n");
   fprintf( fp, "#include    \"gdspos.h\"       /* Define a position in a subset.*/\n");
   fprintf( fp, "#include    \"gdsbox.h\"       /* Define a box inside/around a subset.*/\n");
   fprintf( fp, "#include    \"gdsc_range.h\"   /* Return lower left and upper right corner of a subset.*/\n");
   fprintf( fp, "#include    \"gdsc_ndims.h\"   /* Return the dimensionality of a coordinate word.*/\n");
   fprintf( fp, "#include    \"gdsc_grid.h\"    /* Extract grid value.*/\n");
   fprintf( fp, "#include    \"gdsc_fill.h\"    /* return coordinate word filled with a grid */\n");
   fprintf( fp, "                             /* value for each axis.*/\n");
   fprintf( fp, "#include    \"gdsi_read.h\"    /* Reads data from (part of) a set.*/\n");
   if (out) {
      fprintf( fp, "#include    \"minmax3.h\"      /* Find min, max and #blanks in subset. */\n");
      fprintf( fp, "#include    \"wminmax.h\"      /* Writes (new) minimum and maximum and number */\n");
      fprintf( fp, "                             /* of blanks of subsets in the descriptor file */\n");
      fprintf( fp, "                             /* and optionally deletes the MINMAX descriptors */\n");
      fprintf( fp, "                             /* at intersecting levels. */\n");
   }


   if (out) {
      fprintf( fp, "\n\n/* Output set related */\n\n");
      fprintf( fp, "#include    \"gdsasn.h\"       /* GDSASN copies the coordinate system of a */\n");
      fprintf( fp, "                             /* previously opened input set obtained with */\n");
      fprintf( fp, "                             /* GDSINP to the output set to be obtained */\n");
      fprintf( fp, "                             /* with GDSOUT. */\n");
      fprintf( fp, "#include    \"gdsout.h\"       /* GDSOUT prompts the user to enter the */\n");
      fprintf( fp, "                             /* name of an output set and the subsets, */\n");
      fprintf( fp, "                             /* and returns the number of subsets entered. */\n");
      fprintf( fp, "#include    \"gdsi_write.h\"   /* Writes data to (part of) an set. */\n");
   }

   if (pgplot) {
      fprintf( fp, "\n\n/* PGPLOT includes */\n\n");
      fprintf( fp, "#include    \"pgplot.h\"       /* All PGPLOT includes. */\n");
   }


   fprintf( fp, "\n\n\n");
   fprintf( fp, "/* DEFINITIONS: */\n");
   fprintf( fp, "\n/* Initialize Fortran compatible string with macro 'fmake' */\n\n");
   fprintf( fp, "#define fmake(fchr,size) { \\\n");
   fprintf( fp, "                           static char buff[size+1]; \\\n");
   fprintf( fp, "                           int i; \\\n");
   fprintf( fp, "                           for (i = 0; i < size; buff[i++] = ' '); \\\n");
   fprintf( fp, "                           buff[i] = 0; \\\n");
   fprintf( fp, "                           fchr.a = buff; \\\n");
   fprintf( fp, "                           fchr.l = size; \\\n");
   fprintf( fp, "                         } \n");
   fprintf( fp, "\n/* Malloc version of 'fmake. Strings allocated with'  */\n");
   fprintf( fp, "/* finit, must be freed with free( fc.a ) */\n");
   fprintf( fp, "#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \\\n");
   fprintf( fp, "                            fc.a[ len ] = '\\0' ; \\\n");
   fprintf( fp, "                            fc.l = len ; }  \n");
   fprintf( fp, "\n" );
   fprintf( fp, "#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )\n" );
   fprintf( fp, "#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )\n" );
   fprintf( fp, "#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )\n" );
   fprintf( fp, "#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )\n" );
   fprintf( fp, "#define PI             3.141592653589793\n" );
   fprintf( fp, "#define RAD(a)         ( (a) * 0.017453292519943295769237 )\n" );
   fprintf( fp, "#define DEG(a)         ( (a) * 57.295779513082320876798155 )\n\n" );


   fprintf( fp, "#define %-14.14s %-10.10s %s\n", 
           "RELEASE", "\"1.0\"", "/* Version number */" );
   fprintf( fp, "#define %-14.14s %-10.10s %s\n", 
           "MAXAXES", "10", "/* Max. axes in a set */" );           
   fprintf( fp, "#define %-14.14s %-10d %s\n",            
           "MAXSUBSETS", maxsubsets, "/* Max. allowed subsets */" );
   fprintf( fp, "#define %-14.14s %-10.10s %s\n", 
           "MAXBUF", "4096", "/* Buffer size for I/O */" );
   fprintf( fp, "#define %-14.14s %-10.10s %s\n",
           "STRLEN", "256", "/* Max length of strings */" );
   fprintf( fp, "#define %-14.14s %-10.10s %s\n",
           "FILENAMELEN", "256", "/* Max length of file names */" );           
   fprintf( fp, "#define %-14.14s %-10.10s %s\n", 
           "FITSLEN", "20", "/* Max length of header items etc.*/" );
   fprintf( fp, "#define %-14.14s %-10.10s %s\n",
           "NONE", "0", "/* Default levels in userxxx routines */" );
   fprintf( fp, "#define %-14.14s %-10.10s %s\n",
           "REQUEST", "1", "" );
   fprintf( fp, "#define %-14.14s %-10.10s %s\n",
           "HIDDEN", "2", "" );
   fprintf( fp, "#define %-14.14s %-10.10s %s\n",
           "EXACT", "4", "" );
   fprintf( fp, "#define %-14.14s %-10.10s %s\n",                   
           "YES", "1", "/* C versions of .TRUE. and .FALSE. */" );
   fprintf( fp, "#define %-14.14s %-10.10s %s\n", 
           "NO", "0", "" ); 


   /*====== DEFINES FOR IN/OUTPUT ROUTINES ======*/

   fprintf( fp, "\n/* Defines for in/output routines etc.*/\n\n" );
   fprintf( fp, "#define KEY_INSET      tofchar(\"INSET=\")\n" );
   fprintf( fp, "#define MES_INSET      tofchar(\"Give input set (, subsets):\")\n" );
   fprintf( fp, "#define KEY_BOX        tofchar(\"BOX=\")\n" );
   fprintf( fp, "#define MES_BOX        tofchar(\" \")\n" );
   if (out) 
   {
      fprintf( fp, "#define KEY_OUTSET     tofchar(\"OUTSET=\")\n" );
      fprintf( fp, "#define MES_OUTSET     tofchar(\"Give output set (subset(s)): \")\n" );
   }


   /*====== VARIABLES FOR INPUT ======*/

   fprintf( fp, "\n\n/* Variables for input */\n\n" );
   fprintf( fp, "static fchar    Setin;              /* Name of input set */\n");
   fprintf( fp, "static fint     subin[MAXSUBSETS];  /* Subset coordinate words */\n");
   fprintf( fp, "static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */\n");
   fprintf( fp, "                                    /* axes numbers.  The first elements (upto */\n");
   fprintf( fp, "                                    /* the dimension of the subset) contain the */\n");
   fprintf( fp, "                                    /* axes numbers of the subset, the other */\n");
   fprintf( fp, "                                    /* ones ontain the axes numbers outside the */\n");
   fprintf( fp, "                                    /* the subset ordered according to the */\n");
   fprintf( fp, "                                    /* specification by the user. */\n");
   fprintf( fp, "static fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */\n");
   fprintf( fp, "                                    /* number of grids along an axes as */\n");
   fprintf( fp, "                                    /* specified by the user. The first elements */\n");
   fprintf( fp, "                                    /* (upto the dimension of the subset) contain */\n");
   fprintf( fp, "                                    /* the length of the subset axes, the other */\n");
   fprintf( fp, "                                    /* ones contain the the number of grids along */\n");
   fprintf( fp, "                                    /* an axes outside the subset. */\n");
   fprintf( fp, "                                    /* the operation for each subset, Class 2 */\n");
   fprintf( fp, "                                    /* is for applications for which the operation */\n");
   fprintf( fp, "                                    /* requires an interaction between the different */\n");
   fprintf( fp, "                                    /* subsets. */\n");
   fprintf( fp, "static fint     subdim;             /* Dimensionality of the subsets for class 1 applications */\n");
   fprintf( fp, "static fint     setdim;             /* Dimension of set. */\n");

   fprintf( fp, "\n\n/* Box and frame related */\n\n" );
   fprintf( fp, "static fint     flo[MAXAXES];       /* Low  edge of frame in grids */\n");
   fprintf( fp, "static fint     fhi[MAXAXES];       /* High edge of frame in grids */\n");
   fprintf( fp, "static fint     blo[MAXAXES];       /* Low  edge of box in grids */\n");
   fprintf( fp, "static fint     bhi[MAXAXES];       /* High edge of box in grids */\n");
   fprintf( fp, "                                    /*  1 box may exceed subset size */\n");
   fprintf( fp, "                                    /*  2 default is in BLO */\n");
   fprintf( fp, "                                    /*  4 default is in BHI */\n");
   fprintf( fp, "                                    /*  8 box restricted to size defined in BHI*/\n");
   fprintf( fp, "                                    /*  These codes work additive.*/\n");
   fprintf( fp, "                                    /*  When boxopt is 0 or 1, the default is the */\n");
   fprintf( fp, "                                    /*  is the entire subset. */\n");

   fprintf( fp, "\n\n/* Reading data */\n\n" );
   fprintf( fp, "static fint     maxIObuf = MAXBUF;  /* Maximum size of read buffer. */\n");
   fprintf( fp, "static float    image[MAXBUF];      /* Buffer for read routine. */\n");
   fprintf( fp, "static fint     subnr;              /* Counter for subset loop. */\n");



   if (out) 
   {
      fprintf( fp, "\n\n/* OUTSET related variables */\n\n" );
      fprintf( fp, "static fchar    Setout;\n");
      fprintf( fp, "static fint     subout[MAXSUBSETS];  /* Output subset coordinate words */\n");
      fprintf( fp, "static fint     nsubsout;\n");
      fprintf( fp, "static fint     axnumout[MAXAXES];\n");
      fprintf( fp, "static fint     axcountout[MAXAXES];\n");
   }

   if (pgplot) 
   {
      fprintf( fp, "\n\n/* PGPLOT variables */\n\n" );
      fprintf( fp, "const  fint     background  =  0;      /* Color definitions for PGPLOT. */\n");
      fprintf( fp, "const  fint     foreground  =  1;      /* Black if background is white. */\n");
      fprintf( fp, "const  fint     red         =  2;\n");
      fprintf( fp, "const  fint     green       =  3;\n");
      fprintf( fp, "const  fint     blue        =  4;\n");
      fprintf( fp, "const  fint     cyan        =  5;\n");
      fprintf( fp, "const  fint     magenta     =  6;\n");
      fprintf( fp, "const  fint     yellow      =  7;\n");
      fprintf( fp, "const  fint     orange      =  8;\n");
      fprintf( fp, "const  fint     greenyellow =  9;\n");
      fprintf( fp, "const  fint     greencyan   = 10;\n");
      fprintf( fp, "const  fint     bluecyan    = 11;\n");
      fprintf( fp, "const  fint     bluemagenta = 12;\n");
      fprintf( fp, "const  fint     redmagenta  = 13;\n");
      fprintf( fp, "const  fint     darkgray    = 14;\n");
      fprintf( fp, "const  fint     lightgray   = 15;\n");
      fprintf( fp, "static fint     symbol      =  2;      /* Take a '+' as plot symbol, see PGPLOT MANUAL */ \n");
      fprintf( fp, "static fint     color;\n");
   }



   fprintf( fp, "\n\n/* Miscellaneous */\n\n" );
   fprintf( fp, "static fint     setlevel = 0;       /* To get header items at set level. */\n");
   fprintf( fp, "static float    blank;              /* Global value for BLANK. */\n");
   fprintf( fp, "static fint     r1, r2;             /* Result values for different routines. */\n");
   fprintf( fp, "static char     message[STRLEN];    /* All purpose character buffer. */\n");
   fprintf( fp, "static float    sum;                /* Sum of image values. */\n");
   fprintf( fp, "static int      i;                  /* Various counters. */\n");
   fprintf( fp, "static bool     agreed = NO;        /* Loop guard. */\n");
   if (out) 
   {
      fprintf( fp, "static float    minval[MAXSUBSETS]; /* Min. value of data for each subset. */\n");
      fprintf( fp, "static float    maxval[MAXSUBSETS]; /* Max. value of data for each subset. */\n");
      fprintf( fp, "static fint     nblanks[MAXSUBSETS];/* Number of blanks in each subset. */\n");
      fprintf( fp, "static fint     change;             /* Used in WMINMAX. change!=0 means */\n");
      fprintf( fp, "                                    /* minimum and maximum have changed and */\n");
      fprintf( fp, "                                    /* that the MINMAX descriptors at */\n");
      fprintf( fp, "                                    /* intersecting levels will be removed. */\n");
   }

   if (toasciiz) {
      
      fprintf( fp, "FILE            *asciifile;         /* File pointer to ASCII file */\n");
   }



   /*===== COMMON FUNCTIONS ===== */

   fprintf( fp, "\n\n\n" );
   fprintf( fp, "static void errorC( int  level,\n" );
   fprintf( fp, "                    char *str )\n" );
   fprintf( fp, "/*-----------------------------------------------------------*/\n" );
   fprintf( fp, "/* PURPOSE: User error handling routine.                     */\n" );
   fprintf( fp, "/* The C version of 'error'.                                 */\n" );
   fprintf( fp, "/* 1 = Warning, 2 = Minor error, 3 = Serious error,          */\n" );
   fprintf( fp, "/* 4 = Fatal error                                           */\n" );
   fprintf( fp, "/*-----------------------------------------------------------*/\n" );
   fprintf( fp, "{\n" );
   fprintf( fp, "   fint flev = (fint) level;\n" );
   fprintf( fp, "   error_c( &flev, tofchar( str ) ); \n" );
   fprintf( fp, "}\n" );


   if (toasciiz) 
   {
      fprintf( fp, "\n\n\n" );
      fprintf( fp, "FILE *openfile( fchar Key, \n");
      fprintf( fp, "                fchar Mes, \n");
      fprintf( fp, "                fint  dfault, \n");
      fprintf( fp, "                char  *filename, \n");
      fprintf( fp, "                char  mode )\n" );
      fprintf( fp, "/*-------------------------------------------------------------*/\n");
      fprintf( fp, "/* PURPOSE:  Open file for writing/reading.                    */\n");
      fprintf( fp, "/*                                                             */\n");
      fprintf( fp, "/* Key      (I)    Keyword used in prompt.                     */\n");
      fprintf( fp, "/* Mes      (I)    Message used in prompt.                     */\n");
      fprintf( fp, "/* dfault   (I)    Default level for input.                    */\n");
      fprintf( fp, "/* filename (I/O)  Default filename, if length equals 0 then   */\n");
      fprintf( fp, "/*                 the default at usertext means that file     */\n");
      fprintf( fp, "/*                 pointer NULL is returned.                   */\n");
      fprintf( fp, "/*                 On output 'filename' contains the name of   */\n");
      fprintf( fp, "/*                 file on disk.                               */\n");
      fprintf( fp, "/* mode     (I)    character r or w for read/write in fopen C  */\n");
      fprintf( fp, "/*                 function                                    */\n");
      fprintf( fp, "/*                                                             */\n");
      fprintf( fp, "/* Ask filename in GIPSY way.                                  */\n");
      fprintf( fp, "/* Check file for existence. Return file pointer and the name  */\n");
      fprintf( fp, "/* of the given file. The function introduces the keyword      */\n");
      fprintf( fp, "/* APPEND= for 'write' files that already exist.               */\n");
      fprintf( fp, "/* If APPEND=N the existing file will be overwritten.          */\n");
      fprintf( fp, "/*-------------------------------------------------------------*/\n");
      fprintf( fp, "{\n");
      fprintf( fp, "   fint      nitems = 1;\n");
      fprintf( fp, "   fint      agreed;\n");
      fprintf( fp, "   fint      n;\n");
      fprintf( fp, "   FILE      *fp;\n");
      fprintf( fp, "   bool      append;\n");      
      fprintf( fp, "   bool      readmode, writemode;\n");
      fprintf( fp, "   bool      nodeffile;\n");      
      fprintf( fp, "   fchar     Filename;\n");
      fprintf( fp, "   \n");
      fprintf( fp, "         \n");
      fprintf( fp, "   fmake( Filename, FILENAMELEN );\n");
      fprintf( fp, "   readmode  = ('R' == toupper(mode) );\n");
      fprintf( fp, "   writemode = ('W' == toupper(mode) );\n");
      fprintf( fp, "   nodeffile = (strlen(filename) == 0);\n");
      fprintf( fp, "   if (readmode)\n");
      fprintf( fp, "   {\n");
      fprintf( fp, "      do\n");
      fprintf( fp, "      {\n");
      fprintf( fp, "         n = usertext_c( Filename, &dfault, Key, Mes );\n");
      fprintf( fp, "         if (!n)\n");
      fprintf( fp, "         {\n");
      fprintf( fp, "            if (nodeffile) \n");
      fprintf( fp, "               return(NULL);\n");
      fprintf( fp, "         }\n");
      fprintf( fp, "         else\n");
      fprintf( fp, "            strcpy( filename, strtok(Filename.a, \" \") );   /* Delete after space */\n");
      fprintf( fp, "         fp = fopen(filename, \"r\");\n");
      fprintf( fp, "         if (fp == NULL)\n");
      fprintf( fp, "         {\n");
      fprintf( fp, "            reject_c( Key, tofchar(\"Cannot read file\") );\n");
      fprintf( fp, "            if (dfault >= 2)\n");
      fprintf( fp, "               dfault = REQUEST;\n");
      fprintf( fp, "            nodeffile = YES;\n");
      fprintf( fp, "            Mes = tofchar( \"Try another file name:\" );\n");
      fprintf( fp, "         }\n");
      fprintf( fp, "      }\n");
      fprintf( fp, "      while (fp == NULL);\n");
      fprintf( fp, "      return( fp );\n");
      fprintf( fp, "   }\n");
      fprintf( fp, "   if (writemode)\n");
      fprintf( fp, "   {\n");
      fprintf( fp, "      do\n");
      fprintf( fp, "      {\n");
      fprintf( fp, "         n = usertext_c( Filename, &dfault, Key, Mes );\n");
      fprintf( fp, "         if (!n)\n");
      fprintf( fp, "         {\n");
      fprintf( fp, "            if (nodeffile)\n");
      fprintf( fp, "               return(NULL);\n");
      fprintf( fp, "         }\n");
      fprintf( fp, "         else\n");
      fprintf( fp, "            strcpy( filename, strtok(Filename.a, \" \") );   /* Delete after space */\n");
      fprintf( fp, "         fp = fopen(filename, \"r\");\n");
      fprintf( fp, "         cancel_c( Key );\n");
      fprintf( fp, "         if (fp != NULL)         /* File exists */\n");
      fprintf( fp, "         {\n");
      fprintf( fp, "            append = toflog( NO );\n");
      fprintf( fp, "            n = userlog_c( &append,\n");
      fprintf( fp, "                           &nitems,\n");
      fprintf( fp, "                           &dfault,\n");
      fprintf( fp, "                           tofchar(\"APPEND=\"),\n");
      fprintf( fp, "                           tofchar(\"File exists, append?   Y=append/[N=overwrite]\") );\n");
      fprintf( fp, "            append = tobool( append );\n");
      fprintf( fp, "            fclose( fp );\n");
      fprintf( fp, "            cancel_c( tofchar(\"APPEND=\") );\n");
      fprintf( fp, "            if (append)\n");
      fprintf( fp, "            {\n");
      fprintf( fp, "               fp = fopen(filename, \"a\");\n");
      fprintf( fp, "               agreed = (fp != NULL);\n");
      fprintf( fp, "               if (!agreed)\n");
      fprintf( fp, "                  reject_c( Key,\n");
      fprintf( fp, "                            tofchar(\"Cannot open for appending, try another!\") );\n");
      fprintf( fp, "               else\n");
      fprintf( fp, "                  return( fp );\n");
      fprintf( fp, "            }\n"); 
      fprintf( fp, "            else\n");
      fprintf( fp, "            {\n");
      fprintf( fp, "               fp = fopen(filename, \"w\");\n");
      fprintf( fp, "               agreed = (fp != NULL);\n");
      fprintf( fp, "               if (!agreed)\n");
      fprintf( fp, "                  reject_c( Key,\n");
      fprintf( fp, "                            tofchar(\"Cannot open for writing, try another!\") );\n");
      fprintf( fp, "               else\n");
      fprintf( fp, "                  return( fp );\n");
      fprintf( fp, "            }\n");
      fprintf( fp, "         }\n"); 
      fprintf( fp, "         else\n");
      fprintf( fp, "         {\n");
      fprintf( fp, "            /* File does not exist */\n");
      fprintf( fp, "            fp = fopen(filename, \"w\");\n");
      fprintf( fp, "            agreed = (fp != NULL);\n");
      fprintf( fp, "            if (!agreed)\n");
      fprintf( fp, "               reject_c( Key, \n");
      fprintf( fp, "                         tofchar(\"Cannot open for writing, try another!\") );\n");
      fprintf( fp, "            else\n");
      fprintf( fp, "               return( fp );\n");
      fprintf( fp, "         }\n");
      fprintf( fp, "      }\n");
      fprintf( fp, "      while (!agreed);\n");
      fprintf( fp, "   }\n");
      fprintf( fp, "   return( NULL );                /* Return NULL if not write or read mode */\n");
      fprintf( fp, "}\n");
   }


   if (pgplot) 
   {
      fprintf( fp, "\n\n\n");
      fprintf( fp, "void initplot( void )\n" );
      fprintf( fp, "/*------------------------------------------------------------*/\n" );
      fprintf( fp, "/* PURPOSE: Initialze PGPLOT.                                 */\n" );
      fprintf( fp, "/* Initialize plot software. Set viewport and output dims.    */\n" );
      fprintf( fp, "/* If output device is a printer, ask user for line width.    */\n" );
      fprintf( fp, "/*------------------------------------------------------------*/\n" );
      fprintf( fp, "{\n" );
      fprintf( fp, "   fint   unit;            /* Ignored by pgbeg, use unit=0. */\n" );
      fprintf( fp, "   fchar  Devspec;         /* Device specification. */\n" );
      fprintf( fp, "   fint   nxysub[2];       /* Number of subdivisions on 1 page. */\n" );
      fprintf( fp, "   float  width;           /* Width of output on paper */\n" );
      fprintf( fp, "   float  aspect;          /* Aspect ratio of output on paper */\n" );
      fprintf( fp, "   fint   nitems, dfault;\n" );
      fprintf( fp, "   fint   r;\n" );
      fprintf( fp, "   bool   paging;          /* Disable PGPLOT's NEXTPAGE keyword. */\n" );
      fprintf( fp, "   float  paper[2];\n" );
      fprintf( fp, "   float  xl, xr, yb, yt;  /* Edges of the viewport. */\n" );
      fprintf( fp, "\n\n" );
      fprintf( fp, "   /*--------------------------------------------------*/\n" );
      fprintf( fp, "   /* Initialize PGPLOT with a call to 'pgbeg'.        */\n" );
      fprintf( fp, "   /* There are 4 arguments for PGBEG:                 */\n" );
      fprintf( fp, "   /* UNIT, this argument is ignored by PGBEG (use     */\n" );
      fprintf( fp, "   /*       zero).                                     */\n" );
      fprintf( fp, "   /* FILE, If this argument is a question mark PGBEG  */\n" );
      fprintf( fp, "   /*       will prompt the user to supply a string.   */\n" );
      fprintf( fp, "   /* NXSUB, # sub divisions of the view surface in X. */\n" );
      fprintf( fp, "   /* NYSUB, # sub divisions of the view surface in Y. */\n" );
      fprintf( fp, "   /*--------------------------------------------------*/\n" );      
      fprintf( fp, "\n" );
      fprintf( fp, "   nxysub[1] = nxysub[0] = 1;           /* No sub divisions is default */\n" );
      fprintf( fp, "   nitems = 2;\n" );
      fprintf( fp, "   dfault = HIDDEN;\n" );
      fprintf( fp, "   r = userint_c( nxysub,\n" );
      fprintf( fp, "                  &nitems,\n" );
      fprintf( fp, "                  &dfault,\n" );
      fprintf( fp, "                  tofchar(\"PGMOSAIC=\"),\n" );
      fprintf( fp, "                  tofchar(\"View surface sub divisions in x,y:   [1,1]\") );\n" );
      fprintf( fp, "\n" );
      fprintf( fp, "   unit = 0;\n" );
      fprintf( fp, "   Devspec = tofchar(\"?\");\n" );
      fprintf( fp, "   r = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );\n" );
      fprintf( fp, "   if (r != 1)                         /* Fatal error */\n" );
      fprintf( fp, "      errorC( 4, \"Cannot open output device\" );\n" );
      fprintf( fp, "\n" );
      fprintf( fp, "   /* No PGPLOT's NEXTPAGE= keyword */\n" );
      fprintf( fp, "   paging = toflog( NO );\n" );
      fprintf( fp, "   pgask_c( &paging );\n" );
      fprintf( fp, "\n" );
      fprintf( fp, "   /* Change size of the view surface to a specified width */ \n" );
      fprintf( fp, "   /* and aspect ratio (=height/width) */ \n" );
      fprintf( fp, "   nitems = 2;\n" );
      fprintf( fp, "   dfault = HIDDEN;\n" );
      fprintf( fp, "   paper[0] = 0.0;\n" );
      fprintf( fp, "   paper[1] = 1.0;\n" );
      fprintf( fp, "   r = userreal_c( paper,\n" );
      fprintf( fp, "                   &nitems,\n" );
      fprintf( fp, "                   &dfault,\n" );
      fprintf( fp, "                   tofchar(\"PGPAPER=\"),\n" );
      fprintf( fp, "                   tofchar(\"Give width(cm), aspect ratio: [calculated]\") );\n" );
      fprintf( fp, "   if (r > 0) \n" );
      fprintf( fp, "   {\n" );
      fprintf( fp, "      /* If width = 0.0 then the program will select the largest view surface */\n" );
      fprintf( fp, "      width  = paper[0] / 2.54;         /* Convert width to inches. */\n" );
      fprintf( fp, "      aspect = paper[1];\n" );
      fprintf( fp, "      pgpap_c( &width, &aspect );\n" );
      fprintf( fp, "   }\n\n" );
      fprintf( fp, "   /* Set viewport */ \n" );
      fprintf( fp, "   xl = 0.2; xr = 0.95;\n" );
      fprintf( fp, "   yb = 0.1; yt = 0.9;\n" );
      fprintf( fp, "   pgsvp_c( &xl, &xr, &yb, &yt );\n" );
      fprintf( fp, "}\n" );


      /* Second PGPLOT function is 'drawbox' */

      fprintf( fp, "\n\n\n");
      fprintf( fp, "void drawbox( float  Xmin, \n" );
      fprintf( fp, "              float  Ymin, \n" );
      fprintf( fp, "              float  Xmax, \n" );
      fprintf( fp, "              float  Ymax, \n" );
      fprintf( fp, "              char  *xtitle, \n" );
      fprintf( fp, "              char  *ytitle, \n" );
      fprintf( fp, "              char  *ttitle ) \n" );            
      fprintf( fp, "/*------------------------------------------------------------*/\n" );
      fprintf( fp, "/* PURPOSE: Draw frame with labels for input box.             */\n" );
      fprintf( fp, "/* Draw box and labels. Take special care for the y labels    */\n" );
      fprintf( fp, "/* and title. Colors are defined globally. Xmin etc are the   */\n" );
      fprintf( fp, "/* corners of the box in world coordinates.                   */\n" );
      fprintf( fp, "/*------------------------------------------------------------*/\n" );
      fprintf( fp, "{\n" );
      fprintf( fp, "   float  charsize = 1.0;\n" );
      fprintf( fp, "   float  delta;\n" );
      fprintf( fp, "   fint   lwidth;\n" );
      fprintf( fp, "   fint   r;\n" );
      fprintf( fp, "   fint   nitems;\n" );
      fprintf( fp, "   fint   dfault;\n" );
      fprintf( fp, "   float  pg_box[4];                 /* Corners of draw box. */\n" );
      fprintf( fp, "   fint   color;\n" );
      fprintf( fp, "   fint   font;\n" );
      fprintf( fp, "   fint   nxsub, nysub;\n" );
      fprintf( fp, "   float  xtick, ytick;\n" );
      fprintf( fp, "   char   message[80];\n" );
      fprintf( fp, "   \n\n" );
      fprintf( fp, "   pgpage_c();                       /* Advance to new page. */\n" );
      fprintf( fp, "   \n" );
      fprintf( fp, "   /* Increase the size of the box a little */\n" );
      fprintf( fp, "   delta = fabs( Xmax - Xmin ) / 10.0;\n" );
      fprintf( fp, "   if (delta == 0.0)\n" );
      fprintf( fp, "      delta = 1.0;\n" );
      fprintf( fp, "   Xmin -= delta;\n" );
      fprintf( fp, "   Xmax += delta;\n" );
      fprintf( fp, "   delta = fabs( Ymax - Ymin ) / 10.0;\n" );
      fprintf( fp, "   if (delta == 0.0)\n" ); 
      fprintf( fp, "      delta = 1.0;\n" );
      fprintf( fp, "   Ymin -= delta;\n" );
      fprintf( fp, "   Ymax += delta;\n" );
      fprintf( fp, "   pg_box[0] = Xmin;                /* Get size from user input */\n" );
      fprintf( fp, "   pg_box[1] = Ymin;\n" );
      fprintf( fp, "   pg_box[2] = Xmax;\n" );
      fprintf( fp, "   pg_box[3] = Ymax;\n" );
      fprintf( fp, "   nitems = 4;\n" );
      fprintf( fp, "   dfault = HIDDEN;\n" );
      strcpy( message, "( message, \"Corners of box Xl,Yl, Xh,Yh:  [%f,%f,%f,%f]\", Xmin,Ymin,Xmax,Ymax );" );
      fprintf( fp, "   sprintf%s\n", message );
      fprintf( fp, "   r = userreal_c( pg_box,\n" );
      fprintf( fp, "                    &nitems,\n" );
      fprintf( fp, "                    &dfault,\n" );
      fprintf( fp, "                    tofchar(\"PGBOX=\"),\n" );
      fprintf( fp, "                    tofchar( message ) );\n" );
      fprintf( fp, "   Xmin = pg_box[0];\n" );
      fprintf( fp, "   Ymin = pg_box[1];\n" );
      fprintf( fp, "   Xmax = pg_box[2];\n" );
      fprintf( fp, "   Ymax = pg_box[3];\n" );
      fprintf( fp, "   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );    /* Set the window */\n" );
      fprintf( fp, "   \n" );
      fprintf( fp, "   color = 1;\n" );
      fprintf( fp, "   nitems = 1;\n" );
      fprintf( fp, "   dfault = HIDDEN;\n" );
      fprintf( fp, "   r = userint_c( &color, \n" );
      fprintf( fp, "                  &nitems,\n" );
      fprintf( fp, "                  &dfault,\n" );
      fprintf( fp, "                  tofchar(\"PGCOLOR=\"),\n" );
      fprintf( fp, "                  tofchar(\"Give color 1..15:        [1]\") );\n" );
      fprintf( fp, "   if (color > 15)\n" );
      fprintf( fp, "      color = 15;\n" );
      fprintf( fp, "   if (color < 1 )\n" );
      fprintf( fp, "      color =  1;\n" );
      fprintf( fp, "   pgsci_c( &color );\n\n" );
      fprintf( fp, "   lwidth = 2;\n" );
      fprintf( fp, "   nitems = 1;\n" );
      fprintf( fp, "   dfault = HIDDEN;\n" );
      fprintf( fp, "   r = userint_c( &lwidth, \n" );
      fprintf( fp, "                  &nitems,\n" );
      fprintf( fp, "                  &dfault,\n" );
      fprintf( fp, "                  tofchar(\"PGWIDTH=\"),\n" );
      fprintf( fp, "                  tofchar(\"Give line width 1..21:        [2]\") );\n" );
      fprintf( fp, "   if (lwidth > 21)\n" );
      fprintf( fp, "      lwidth = 21;\n" );
      fprintf( fp, "   if (lwidth < 1 )\n" );
      fprintf( fp, "      lwidth =  1;\n" );
      fprintf( fp, "   pgslw_c( &lwidth );                  /* Set line width. */\n\n" );
      fprintf( fp, "   charsize = 1.0;\n" );
      fprintf( fp, "   nitems = 1;\n" );
      fprintf( fp, "   dfault = HIDDEN;\n" );
      fprintf( fp, "   r = userreal_c( &charsize, \n" );
      fprintf( fp, "                   &nitems,\n" );
      fprintf( fp, "                   &dfault,\n" );
      fprintf( fp, "                   tofchar(\"PGHEIGHT=\"),\n" );
      fprintf( fp, "                   tofchar(\"Give character height:     [1.0]\") );\n" );
      fprintf( fp, "   pgsch_c( &charsize );               /* Character height. */\n\n" );
      fprintf( fp, "   font   = 2;\n" );
      fprintf( fp, "   nitems = 1;\n" );
      fprintf( fp, "   dfault = HIDDEN;\n" );
      fprintf( fp, "   r = userint_c( &font, \n" );
      fprintf( fp, "                  &nitems,\n" );
      fprintf( fp, "                  &dfault,\n" );
      fprintf( fp, "                  tofchar(\"PGFONT=\"),\n" );
      fprintf( fp, "                  tofchar(\"Give font 1..4:        [2]\") );\n" );
      fprintf( fp, "   if (font > 4)\n" );
      fprintf( fp, "      font = 4; \n" );
      fprintf( fp, "   if (font < 1)\n" );
      fprintf( fp, "      font = 1; \n" );
      fprintf( fp, "   pgscf_c( &font );                   /* Set font. */\n" );
      fprintf( fp, "   \n" );
      fprintf( fp, "   /*----------------------------------------------------------------*/\n" );
      fprintf( fp, "   /* xtick is world coordinate interval between major tick marks    */\n" );
      fprintf( fp, "   /* on X axis. If xtick=0.0, the interval is chosen by PGBOX, so   */\n" );
      fprintf( fp, "   /* that there will be at least 3 major tick marks along the axis. */\n" );
      fprintf( fp, "   /* nxsub is the number of subintervals to divide the major        */\n" );
      fprintf( fp, "   /* coordinate interval into. If xtick=0.0 or nxsub=0, the number  */\n" );
      fprintf( fp, "   /* is chosen by PGBOX.                                            */\n" );
      fprintf( fp, "   /* BCNSTV :                                                       */\n" );
      fprintf( fp, "   /* B: draw bottom (X) or left (Y) edge of frame.                  */\n" );
      fprintf( fp, "   /* C: draw top (X) or right (Y) edge of frame.                    */\n" );
      fprintf( fp, "   /* N: write Numeric labels in the conventional location below     */\n" );
      fprintf( fp, "   /*    the viewport (X) or to the left of the viewport (Y).        */\n" );
      fprintf( fp, "   /* S: draw minor tick marks (Subticks).                           */\n" );
      fprintf( fp, "   /* T: draw major Tick marks at the major coordinate interval.     */\n" );
      fprintf( fp, "   /* V: orient numeric labels Vertically. This is only applicable   */\n" );
      fprintf( fp, "   /*    to Y.                                                       */\n" );
      fprintf( fp, "   /*----------------------------------------------------------------*/\n" );      
      fprintf( fp, "   xtick = ytick = 0.0;\n" );
      fprintf( fp, "   nxsub = nysub = 0;\n" );
      fprintf( fp, "   pgbox_c( tofchar(\"BCNST\" ), &xtick, &nxsub,\n" );
      fprintf( fp, "            tofchar(\"BCNSTV\"), &ytick, &nysub );\n" );
      fprintf( fp, "   \n" );
      fprintf( fp, "   /* Plot the titles */\n" );
      fprintf( fp, "   pglab_c( tofchar(xtitle), tofchar(ytitle), tofchar(ttitle) );\n");
      fprintf( fp, "}\n");
   }



   /*===== MAIN ===== */

   fprintf( fp, "\n\n\n");
   fprintf( fp, "MAIN_PROGRAM_ENTRY\n" );
   fprintf( fp, "/*-------------------------------------------------------------------------*/\n");
   fprintf( fp, "/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */\n");
   fprintf( fp, "/* main body of your GIPSY application. Variables defined as 'fchar' start */\n");
   fprintf( fp, "/* with a capital.                                                         */\n");
   fprintf( fp, "/*-------------------------------------------------------------------------*/\n");
   fprintf( fp, "{\n" );
   fprintf( fp, "   fint     maxsubs = MAXSUBSETS;\n");
   fprintf( fp, "   fint     maxaxes = MAXAXES;           /* Max num. of axes the program can deal with.*/\n");
   fprintf( fp, "   fint     class   = 1;                 /* Class 1 is for applications which repeat */\n");      
   fprintf( fp, "   fint     showdev = 1;\n" );
   fprintf( fp, "   fint     mcount  = 0;                 /* Initialize MINMAX3 */\n");
   fprintf( fp, "   fint     nsubs;                       /* Number of input subsets */\n");   
   fprintf( fp, "   fint     dfault;                      /* Default option for input etc */\n");   
   fprintf( fp, "\n\n" );
   fprintf( fp, "   init_c();                             /* contact Hermes */\n");
   fprintf( fp, "   /* Task identification */\n");
   fprintf( fp, "   {\n");
   fprintf( fp, "      static fchar    Task;              /* Name of current task */\n");
   fprintf( fp, "      fmake( Task, 20 );                 /* Macro 'fmake' must be available */\n");
   fprintf( fp, "      myname_c( Task );                  /* Get task name */\n");
   fprintf( fp, "      Task.a[nelc_c(Task)] = '\\0';       /* Terminate task name with null char. */\n");
   fprintf( fp, "      IDENTIFICATION( Task.a, RELEASE ); /* Show task and version */\n");
   fprintf( fp, "   }\n" );
   fprintf( fp, "   setfblank_c( &blank );\n" );
   fprintf( fp, "\n" );
   fprintf( fp, "   /*--------------------------------------------------*/\n");
   fprintf( fp, "   /* Get the input set. Documentation can be found in */\n");
   fprintf( fp, "   /* $gip_sub/gdsinp.dc2                              */\n");
   fprintf( fp, "   /*--------------------------------------------------*/\n");   
   fprintf( fp, "   {\n" );   
   fprintf( fp, "      fmake( Setin, STRLEN );\n" );
   fprintf( fp, "      dfault  = NONE;\n" );
   fprintf( fp, "      subdim  = 2;                  /* Allow only 2-dim structures */\n" );
   strcpy( message, "/* Name of input set. */" );
   fprintf( fp, "      nsubs = gdsinp_c( Setin,      %s\n", message );
   strcpy( message, "/* Array containing subsets coordinate words. */" );
   fprintf( fp, "                        subin,      %s\n", message );
   strcpy( message, "/* Maximum number of subsets in 'subin'.*/" );
   fprintf( fp, "                        &maxsubs,   %s\n", message );
   strcpy( message, "/* Default code as is USERxxx. */" );
   fprintf( fp, "                        &dfault,    %s\n", message );
   strcpy( message, "/* Keyword prompt. */" );
   fprintf( fp, "                        KEY_INSET,  %s\n", message );
   strcpy( message, "/* Keyword message for the user. */" );
   fprintf( fp, "                        MES_INSET,  %s\n", message );
   strcpy( message, "/* Device number (as in ANYOUT). */" );
   fprintf( fp, "                        &showdev,   %s\n", message );
   strcpy( message, "/* Array of size 'maxaxes' containing the axes numbers. */" );
   fprintf( fp, "                        axnum,      %s\n", message );
   fprintf( fp, "                                    /* The first elements (upto the dimension of the subset) */\n" );
   fprintf( fp, "                                    /* contain the axes numbers of the subset, */\n" );
   fprintf( fp, "                                    /* the other ones contain the axes numbers */\n" );
   fprintf( fp, "                                    /* outside the subset ordered according to the */\n" );
   fprintf( fp, "                                    /* specification by the user. */\n" );
   strcpy( message, "/* Number of grids on axes in 'axnum' */" );
   fprintf( fp, "                        axcount,    %s\n", message );
   strcpy( message, "/* Max. number of axes. */" );
   fprintf( fp, "                        &maxaxes,   %s\n", message );
   strcpy( message, "/* Class 1 is for applications which repeat */" );
   fprintf( fp, "                                    /* the operation for each subset. */\n" );
   fprintf( fp, "                        &class,     %s\n", message );
   strcpy( message, "/* Dimensionality of the subsets for class 1 */" );
   fprintf( fp, "                        &subdim );  %s\n", message );
   fprintf( fp, "   }\n" );

   fprintf( fp, "   setdim  = gdsc_ndims_c( Setin, &setlevel );\n\n" );
   fprintf( fp, "   /*-------------------------------*/\n");
   fprintf( fp, "   /* Determine edges of this frame */\n");
   fprintf( fp, "   /*-------------------------------*/\n");
   fprintf( fp, "   {\n");
   fprintf( fp, "      fint cwlo, cwhi;                          /* Local coordinate words */\n");
   fprintf( fp, "      int  m;\n");
   fprintf( fp, "      r1 = 0;\n");
   fprintf( fp, "      gdsc_range_c( Setin, &setlevel, &cwlo, &cwhi, &r1 );\n");
   fprintf( fp, "      r1 = r2 = 0;\n");
   fprintf( fp, "      for (m = 0; m < (int) setdim; m++)\n");
   fprintf( fp, "      {\n");
   fprintf( fp, "         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );\n");
   fprintf( fp, "         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );\n");
   fprintf( fp, "      }\n");
   fprintf( fp, "   }\n\n");
   fprintf( fp, "   /*-------------------------------*/\n");
   fprintf( fp, "   /* Prepare grid ranges for INSET */\n");
   fprintf( fp, "   /*-------------------------------*/\n");
   fprintf( fp, "   {\n" );
   fprintf( fp, "      fint     boxopt = 0;         /* The different options are: */\n");
   fprintf( fp, "\n" );
   fprintf( fp, "      dfault = REQUEST;\n" );
   fprintf( fp, "      gdsbox_c( blo, bhi, Setin, subin, &dfault, \n");
   fprintf( fp, "                KEY_BOX, MES_BOX, &showdev, &boxopt );\n");
   fprintf( fp, "   }\n" );
   fprintf( fp, "   \n");

   if (out) 
   {
      fprintf( fp, "   /*--------------------------------------------------------------*/\n");
      fprintf( fp, "   /* Assign 'gdsinp' buffer to 'gdsout'. Output set will get same */\n");
      fprintf( fp, "   /* coordinate system as input INSET=.  GDSOUT is a function     */\n");
      fprintf( fp, "   /* which prompts the user to enter the name of a set and        */\n");
      fprintf( fp, "   /* (optionally) subset(s) and returns the number of subsets     */\n");
      fprintf( fp, "   /* entered.                                                     */\n");
      fprintf( fp, "   /*--------------------------------------------------------------*/\n");
      fprintf( fp, "   gdsasn_c( KEY_INSET, KEY_OUTSET, &class );\n");
      fprintf( fp, "   dfault  = NONE;\n");
      fprintf( fp, "   fmake( Setout, STRLEN );\n");
      fprintf( fp, "   do {\n");
      strcpy( message, "/* Name of the output set. */");
      fprintf( fp, "      nsubsout = gdsout_c( Setout,        %s\n", message );
      strcpy( message, "/* Output array with subsets coordinate words.*/");
      fprintf( fp, "                           subout,        %s\n", message );
      strcpy( message, "/* Maximum number of subsets in subout. */");
      fprintf( fp, "                           &nsubs,        %s\n", message );
      strcpy( message, "/* Default code as in USERxxx. */");
      fprintf( fp, "                           &dfault,       %s\n", message );
      strcpy( message, "/* User keyword prompt. */");
      fprintf( fp, "                           KEY_OUTSET,    %s\n", message );
      strcpy( message, "/* Message for the user. */");
      fprintf( fp, "                           MES_OUTSET,    %s\n", message );
      strcpy( message, "/* Device number (as in ANYOUT). */");
      fprintf( fp, "                           &showdev,      %s\n", message );
      strcpy( message, "/* Array of size 'maxaxes' containing the axes numbers. */");
      fprintf( fp, "                           axnumout,      %s\n", message );
      strcpy( message, "/* Array with the axis sizes. */");
      fprintf( fp, "                           axcountout,    %s\n", message );
      strcpy( message, "/* Max axes the program can deal with. */");
      fprintf( fp, "                           &maxaxes );    %s\n", message );

      fprintf( fp, "      agreed = (nsubsout == nsubs);\n");
      fprintf( fp, "      if (!agreed)\n");
      fprintf( fp, "         reject_c( KEY_OUTSET, tofchar(\"#out != #in\") );\n");
      fprintf( fp, "   }\n");
      fprintf( fp, "   while (!agreed);\n\n");
   }


   if (toasciiz) 
   {
      fprintf( fp, "\n");
      fprintf( fp, "   /*--------------------------------------------------*/\n");
      fprintf( fp, "   /* Example writing to ASCII file opened with        */\n");
      fprintf( fp, "   /* 'openfile'.                                      */\n");
      fprintf( fp, "   /*--------------------------------------------------*/\n");
      fprintf( fp, "   {\n");
      fprintf( fp, "      char   writefile[STRLEN];\n");
      fprintf( fp, "      \n");
      strcpy( message, "writefile[0] = '\\0';     /* <CR> in 'openfile' results in asciifile=NULL */");
      fprintf( fp, "      %s\n", message );
      fprintf( fp, "      dfault    = REQUEST;\n");
      fprintf( fp, "      asciifile = openfile( tofchar(\"FILENAME=\"),         /* Keyword */\n");
      fprintf( fp, "                            tofchar(\"File name for ASCII data:     [no file]\"),  /* Message */\n");
      fprintf( fp, "                            dfault,                         /* default */\n");
      fprintf( fp, "                            writefile,                      /* name of file */\n");
      fprintf( fp, "                           'w' );                           /* write mode */\n");
      fprintf( fp, "      if (asciifile != NULL)\n" );
      fprintf( fp, "      {\n" ); 
      strcpy( message, "anyoutf( 1, \"ASCII data to disk in [%s]\", writefile );" );
      fprintf( fp, "         %s\n", message );
      strcpy( message, "fprintf( asciifile, \"Example output to ASCII file\\n\" );" );
      fprintf( fp, "         %s\n", message );
      fprintf( fp, "      }\n" );
      fprintf( fp, "   }\n\n" );
   }

   fprintf( fp, "   /*------------------------------------------------------------*/\n");
   fprintf( fp, "   /* Start the main loop over all subsets. Calculate for each   */\n");
   fprintf( fp, "   /* subset new coordinate words and reset the transfer id's    */\n");
   fprintf( fp, "   /*------------------------------------------------------------*/\n\n");

   fprintf( fp, "   sum = 0.0;\n");
   if (pgplot) 
   {
      fprintf( fp, "   initplot();\n");
   }
   fprintf( fp, "   for(subnr = 0; subnr < nsubs; subnr++)\n");
   fprintf( fp, "   {\n");
   fprintf( fp, "      fint  tid = 0;               /* Transfer id's */\n");
   fprintf( fp, "      fint  cwlo, cwhi;            /* Coordinate words */\n");    
   fprintf( fp, "      fint  pixelsread;            /* Number of pixels read by read routine. */\n");
  
   if (out)
   {
   fprintf( fp, "      fint  tidO = 0;\n");
   fprintf( fp, "      fint  cwloO, cwhiO;\n");
   fprintf( fp, "      fint  pixelswrite;           /* Number of pixels to write to output. */\n");   
   }

   if (pgplot)
   {
   fprintf( fp, "      fint  pcount = 0;\n");
   }

   fprintf( fp, "\n");
   
   if (pgplot) 
   {
   fprintf( fp, "      drawbox( blo[0], blo[1], bhi[0], bhi[1], \n");
   fprintf( fp, "               \"X grid\", \"Y grid\", \"Grid position of blank values\" );\n");
   fprintf( fp, "      color = (subnr % 12) + 2;\n" );
   fprintf( fp, "      pgsci_c( &color );\n" );
   }

   fprintf( fp, "      cwlo   = gdsc_fill_c( Setin, &subin[subnr], blo );\n");
   fprintf( fp, "      cwhi   = gdsc_fill_c( Setin, &subin[subnr], bhi );\n");

   if (out) 
   {
   fprintf( fp, "      /* Use input grid coordinates, but connect to output subsets */\n");
   fprintf( fp, "      cwloO  = gdsc_fill_c( Setout, &subout[subnr], blo );\n");
   fprintf( fp, "      cwhiO  = gdsc_fill_c( Setout, &subout[subnr], bhi );\n");
   }

   fprintf( fp, "      tid    = 0;\n");
   fprintf( fp, "      do\n");
   fprintf( fp, "      {\n");
   fprintf( fp, "         /* Read 'maxIObuf' values in 'image'. */\n");
   fprintf( fp, "         gdsi_read_c( Setin,\n");
   fprintf( fp, "                      &cwlo, &cwhi,\n");
   fprintf( fp, "                      image,\n");
   fprintf( fp, "                      &maxIObuf,\n");
   fprintf( fp, "                      &pixelsread,\n");
   fprintf( fp, "                      &tid );\n");
   fprintf( fp, "         for (i = 0; i < pixelsread; i++)\n");
   fprintf( fp, "         {\n");
   fprintf( fp, "            /* Dummy operations: */\n");
   fprintf( fp, "            if (image[i] != blank)\n");
   fprintf( fp, "            {\n");
   fprintf( fp, "               sum += image[i];\n");
   
   if (out) 
   {
   fprintf( fp, "               image[i] *= 2.2;\n");
   }

   fprintf( fp, "            }\n");

   if (pgplot) 
   {
   fprintf( fp, "            else\n");
   fprintf( fp, "            {\n");
   fprintf( fp, "              /* Plot the positions of blank values */\n");
   fprintf( fp, "              fint  one = 1;\n");
   fprintf( fp, "              float row, col;\n");
   fprintf( fp, "              col = (float) ( blo[0] + pcount % (bhi[0] - blo[0] + 1) );\n");
   fprintf( fp, "              row = (float) ( blo[1] + pcount / (bhi[0] - blo[0] + 1) );\n");
   fprintf( fp, "              pgpt_c( &one, &col, &row, &symbol );\n");
   fprintf( fp, "            } \n");
   fprintf( fp, "            pcount++;\n");
   }

   fprintf( fp, "         } \n");

   if (out) 
   {
   fprintf( fp, "         /* Calculate running min, max & blanks of output */\n");
   fprintf( fp, "         minmax3_c( image,\n");
   fprintf( fp, "                    &pixelsread,\n");
   fprintf( fp, "                    &minval[subnr], &maxval[subnr],\n");
   fprintf( fp, "                    &nblanks[subnr],\n");
   fprintf( fp, "                    &mcount );\n");
   fprintf( fp, "         pixelswrite = pixelsread;\n");
   fprintf( fp, "         /* Write 'pixelswrite' values from 'image' to output. */\n");
   fprintf( fp, "         gdsi_write_c( Setout,\n");
   fprintf( fp, "                       &cwloO, &cwhiO,\n");
   fprintf( fp, "                       image, \n");
   fprintf( fp, "                       &maxIObuf,\n");
   fprintf( fp, "                       &pixelswrite,\n");
   fprintf( fp, "                       &tidO );\n");
   }

   fprintf( fp, "      }\n");
   fprintf( fp, "      while (tid != 0);\n");
   /* Note difference between 'messages' */
   strcpy( message, "message, \"Sum: %f\", sum );");
   fprintf( fp, "      anyoutf( 3, %s\n", message );

   fprintf( fp, "   }\n");
   if (out) 
   {
   fprintf( fp, "   /* Update OUTSET= descriptor with new values */\n");
   fprintf( fp, "   change = YES;\n");
   fprintf( fp, "   wminmax_c( Setout, subout,\n");
   fprintf( fp, "              minval, maxval, nblanks, \n");
   fprintf( fp, "              &nsubsout,\n");
   fprintf( fp, "              &change );\n");
   }

   fprintf( fp, "   /*-------------------------------------------------------*/\n");
   fprintf( fp, "   /* To end the program, make sure files opened with fopen  */\n");
   fprintf( fp, "   /* are closed, allocated memory is released, PGPLOT is   */\n");
   fprintf( fp, "   /* closed and HERMES is instructed to stop.              */\n");
   fprintf( fp, "   /*-------------------------------------------------------*/\n\n");

   if (pgplot) 
   {
   fprintf( fp, "    pgend_c();\n");
   }

   fprintf( fp, "   finis_c();\n");
   fprintf( fp, "   return(EXIT_SUCCESS);   /* Dummy return */\n");
   fprintf( fp, "}\n");


   /*--------------------------------------------------*/
   /* Inform user how to compile the new source.       */
   /*--------------------------------------------------*/
   {
      char *line = NULL;
      int  len;
      len = sprintf( message, "\nCOMPILE YOUR NEW PROGRAM WITH:    p %s", filename );
      line = malloc( len + 1 );
      memset( line, '=', len );
      line[len] = '\0';
      anyoutf( 1, line );
      anyoutf( 1, message );
      anyoutf( 1, line );
   }      
   
   fclose( fp );
   finis_c();
   return(EXIT_SUCCESS);
}
