/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             help.dc1

Program:       HELP

Purpose:       Display purpose lines or categories or complete 
               documentation of tasks (subroutines and general 
               items). Data can be directed to printer also.

Category:      UTILITY

File:          help.c

Author:        M. Vogelaar

Keywords:

               [...] means: the default (pressing carriage return
               if you are prompted with a keyword) is specified
               between the brackets.

               
   SUBJECT=    Give number(s) of subject(s):                  [exit]
               
               Give a number corresponding with one of the 
               following subjects:

               1.  GENERAL HELP ABOUT HERMES
               2.  GENERAL HELP ABOUT APPLICATIONS
               3.  GENERAL HELP ABOUT DOCUMENTATION
               4.  DISPLAY/PRINT DOCUMENTATION: TASKS/SUBROUTINES/GENERAL
               5.  DISPLAY/PRINT PURPOSE LINES OF TASKS etc.   
               6.  SEARCH ON CATEGORY, DISPLAY/PRINT RESULT
               7.  QUESTIONS AND ANSWERS


               SUBJECT=4, 5 or 6
                  
   DOCTYPE=    Documentation:  [1]=TASKS 2=SUBR 3=SUBR(low) 4=GENERAL
               1 selects documentation about applications.
               2 selects documentation about much used subroutines
               3 selects documentation about low level subroutines
               4 selects documentation about general items like 
                 system management, input syntax, compilation etc.  
  
 
               SUBJECT=4:
               
   DOCUMENT=   Give name of doc. to print:           [return to menu]
               Send this document to a printer selected with
               PRINTER=
               The '*' character can be used as a wildcard, f.i.
               DOCUMENT=*gauss* prints all documents with 'gauss'
               in its name.
               DOCUMENT= accepts up to 32 strings in one specification.               
 

               SUBJECT=5:
                             
   DOCUMENT=   Give document(s) for purpose line:    [return to menu]              
               Display purpose line(s) of selected tasks or 
               subroutines. The '*' character can be used as a 
               wildcard f.i. DOCUMENT=*gauss* 
               DOCUMENT= accepts up to 32 strings in one specification.


               SUBJECT=6:
               
   CATEGORY=   Give name of category:                [return to menu]
               A list with categories known to the system 
               is displayed first. CATEGORY= is a name from this list.
               Wildcards (= '*' character) can be used also.
               Special cases:
               CATEGORY=*   will select all categories known to the 
                            system.
               CATEGORY=**  will select all documentation whether
                            its category is known to the system or 
                            not.
               
               CATEGORY= accepts up to 32 strings in one specifi-
               cation.
                                             
   PRINTER=    Give number of printer:                    [TO SCREEN]
               A list with available printers is generated 
               If the number of a displayed printer is 
               selected, output is directed to that printer. The
               default selection however is output to the screen.
               The keyword can be changed just before any output.


Description:   With the keyword SUBJECT= you can select one or more
               subjects from the menu by specifying the corresponding
               menu numbers. For the subjects 4, 5 and 6 the program
               prompts you (DOCTYPE=) to give the document type. 
               This can be one of:
               
               1) Application documentation, 
               2) Documentation about subroutines used in applications
               3) Documentation about low level (system) subroutines
               4) General documentation about input syntax, system 
                  management, compilation, utilities, devices, 
                  etc.
                  
               The default selection is documentation about tasks. 
               
               For the subjects 4 and 5 (print documentation and
               purpose lines from documentation) a document name
               is wanted. First a list is displayed with all available
               documents of type DOCTYPE=   A selection is made with
               DOCUMENT= which accepts up to 32 document names. The 
               names can include the wildcard character '*'. For
               instance DOCUMENT=*GAUSS* will select all files with
               the name GAUSS included. The matching is case insensitive.
               SUBJECT=6 allows you to find applications in a given
               category. You specify the category name with CATEGORY= 
               All category lines in the documentation that include 
               your string will be listed together with program name 
               and purpose line. An asterisk can be used as wildcard 
               character to select your category (or categories).
               If you want to select all documentation (discard cate-
               gory) use: CATEGORY=**
               
               Output for subjects 1 to 6 can be to a printer or
               to screen. The selection is made with PRINTER=
               after the printer menu is displayed.
               Note that on different systems you can get different
               printer options. PRINTER=0 always selects the screen
               as output device. An example of a printer menu:

               ==============================PRINTERS=====================
               nr       name              cols  rows     comment
               ===========================================================
               0   To SCREEN only
               1   l1psprinter            181    73  Printer in room ZG172
               2   l2psprinter            181    73  Printer in room ZG172
               3   p1psprinter             91    82  Printer in room ZG172
               4   p2psprinter             91    82  Printer in room ZG172
               5   lineprinter            132    60  Printer in room ZG170
               6   landscape              132    60  Printer in room ZG172
               7   portrait                96    60  Printer in room ZG172
               ===========================================================


Notes:         

Example:       

Updates:       Aug 6,  1992: VOG, Document created.
               Nov 2,  1992: VOG, Integrated PURPOSE in HELP. 
                                  Send documents to printer.
               Aug 11, 2009: JPT, Renamed getline to GetLine
#<
*/

/*  help.c: include files     */

#include    "stdio.h"        /* Defines ANSI C input and output utilities */
#include    "stdlib.h"       /* Defines the ANSI C functions for number */
                             /* conversion, storage allocation, and similar tasks.*/
#include    "string.h"       /* Declares the ANSI C string functions*/
                             /* like:strcpy, strcat etc.*/
#include    "cmain.h"        /* Defines the main body of a C program with */
                             /* MAIN_PROGRAM_ENTRY and IDENTIFICATION */
#include    "gipsyc.h"       /* Defines the ANSI-F77 types for Fortran to C intface */
                             /* including def. of char2str,str2char,tofchar,zadd */
                             /* and macros tobool and toflog */
#include    "math.h"         /* Declares the mathematical functions and macros.*/
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

#include    "userint.h"      /* User input interface routines.*/
#include    "userchar.h"
#include    "usercharu.h"    /* Upper case */
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/

/* Printer includes */

#include    "prntrdim.h"
#include    "prntrnum.h"
#include    "prntrnam.h"
#include    "prntrcom.h"
#include    "prntract.h"

#include    "flist.h"        /* Lists all files in a directory. */
#include    "wmatch.h"       /* Matching of strings with wildcard */

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

#define KEY_PRINTER    tofchar("PRINTER=")


#define RELEASE        "1.0"           /* Version number */
#define STRLEN         80              /* Max length of strings */
#define LONGLEN        128
#define KEYLEN         20              /* Max length of keywords */
#define MAXSUBJECTS    10              /* Max. number of subjects to select */
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0
#define MAXFILES       2000            /* Max. number of doc. to examine */
#define MAXSEARCH      32              /* Max number of files to send to printer at one time etc.*/
#define TABLEN         8               /* Replace tab char. by TABLEN spaces. */
#define OFFSET         14              /* OFFSET spaces before text start */
#define MAXCATS        128             /* Max. number of category names from category document */


static fchar    Key, Mes;
static float    blank;                 /* Global value for BLANK. */
static fint     r1;                    /* Result values for different routines. */
static int      i;                     /* Various counters. */
static int      scr;
static fint     subjects[MAXSUBJECTS];
static int      subj;
static fint     showdev;
static fint     dfault, nitems;
static char     message[132];
static char     fnames[MAXFILES][STRLEN];   
static char     extension[5];          /* One of 'dc1', 'dc2', 'dc3', 'doc' */
FILE            *fpTMP;                /* File pointer for printer files */
static char     TMPprintfile[2*STRLEN];
static char     categorynames[MAXCATS][STRLEN];
static char     category[MAXCATS][STRLEN];
static fint     caseINsensitive = 0;
static fchar    Wildcard;              /* Probably the '*' character */


void anyoutC( int dev, char *anyCstr )
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


static int prnmenu( fchar Key, fint *scrnum, fint *printer, 
                    fint *cols, fint *rows )
/*----------------------------------------------------------------------
 * Use:       i = prnmenu(  Key,     Input   fchar
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

   fint            Ires1, Ires2;
   fint            prnnum;             /* Number of selected printer */
   fchar           prnname;            /* Name of selected printer */     
   fchar           prncom;             /* Comments for this printer */
   fint            prncol[MAXPRNS];    /* Number of columns and rows for ... */
   fint            prnrow[MAXPRNS];    /* ... all available printers */
   char            txt[120];           /* Buffer for string manipulations */
   fint            dfault;
   fint            valid;              /* Is printer accepted ? */
   fint            i, j;               /* Counters */
   fint            nitems;
   int             printerindex;
   bool            toscreen = NO;
   fchar           Mes;
   static bool     first = YES;
      

   fmake( Mes, 80 );
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
   
  
   /*-----------------------------------------------------------------*/
   /* All characteristics are known at this stage, ask for selection. */
   /* Display a menu only once.                                       */
   /*-----------------------------------------------------------------*/   
   
   if (first) {
      (void) strcpy( txt, 
      "==============================PRINTERS=============================" );
      anyout_c( scrnum, tofchar(txt) );
      (void) sprintf( txt, "%3.3s   %-20.20s  %4.4s  %4.4s  %-40.40s" , "nr", 
              "    name", "cols", "rows", "   comment" );
      anyout_c( scrnum, tofchar(txt) );
      (void) strcpy(txt,
      "===================================================================" );
      anyout_c( scrnum, tofchar(txt) );
      (void) sprintf( txt, "%3d   %s", 0, "To SCREEN only" );
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
      first = NO;
   }   
  
   /*-------------------------------------------------------------------*/
   /* Ask for a printer. The default is a non-existing printer i.e. the */
   /* output is directed to the screen.                                 */
   /*-------------------------------------------------------------------*/   

   dfault   = REQUEST;                   
   Mes      = tofchar("Give number of printer:        [TO SCREEN]");
   nitems   = 1;
   toscreen = YES;
   *printer = 0;          /* A non existing printer */
   Ires1    = userint_c( printer, &nitems, &dfault, Key, Mes );
   printerindex = *printer - 1;
   valid = ( (*printer > 0) && (*printer <= prnnum) && 
             (prncol[printerindex] > 0) );
   if (valid) toscreen = NO;

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


static void activateprinter( fint prnnum, char *TMPprintfile, fint remove,
                             char *filename )
/*--------------------------------------------------------------------------*/
/* Print file with name 'TMPprintfile'. If 'remove' is equal to zero, the   */
/* file is not removed. Temp. files are always removed.                     */
/*--------------------------------------------------------------------------*/
{
   fint     r2;
   char     mess[80];

   (void) sprintf( mess, "... Activating printer for [%.40s] ...", filename );
   r2 = prntract_c( &prnnum, tofchar( TMPprintfile ), &remove );
   if (r2 ==  0 ) anyoutC( 1,  mess );
   if (r2 == -1 ) anyoutC( 1, "cannot obtain hostname" );
   if (r2 == -2 ) anyoutC( 1, "cannot obtain translation of printer description file");
   if (r2 == -3 ) anyoutC( 1, "cannot open printer description file" );
   if (r2 == -4 ) anyoutC( 1, "cannot allocate enough space" );
   if (r2 == -10) anyoutC( 1, "no such printer" );
}



static void emptyline( FILE *fpTMP )
/*--------------------------------------------------*/
/* Put empty line on display or in file             */
/*--------------------------------------------------*/
{
   if (fpTMP != NULL) {
      (void) fprintf( fpTMP, " \n" );
   } else {
       anyoutC( 1, " " );
   }
}


static void getextension( char *extension )
/*--------------------------------------------------*/
/* Document type is one of task, subroutine (low    */
/* level or general. Default is task. If a wrong    */
/* number is entered, the default is taken.         */
/*--------------------------------------------------*/
{
   fchar     Key;
   fchar     Mes;
   fint      nitems, dfault;
   fint      type;
   fint      r1;
   bool      agreed;
   
     
   do {
      strcpy( extension, ".dc1" );
      Key    = tofchar("DOCTYPE=");
      Mes    = tofchar("Documentation:  [1]=TASKS 2=SUBR 3=SUBR(low) 4=GENERAL");
      nitems = 1;
      dfault = REQUEST;
      type   = 1;
      r1     = userint_c( &type, &nitems, &dfault, Key, Mes );
      agreed = ((type >=1) && (type <=4));
      if (!agreed) reject_c( Key, tofchar("Wrong number!"));
   } while(!agreed);
   cancel_c( Key );
   if (type == 2) strcpy( extension, ".dc2" );
   if (type == 3) strcpy( extension, ".dc3" );
   if (type == 4) strcpy( extension, ".doc" );         
} 
    


static int getnames( char *extension, char *path )
/*------------------------------------------------------------------*/
/* The extension determines the path (which is returned by this     */
/* function) where the documents can be found. The extension is one */
/* of '.dc1', '.dc2' etc. An array with filenames without these     */
/* extensions is created (global array 'fnames') and the size of    */
/* this array is returned.                                          */
/*------------------------------------------------------------------*/
{
   fchar        Dirname;
   fchar        Direntry;
   int          fcount;
   int          k;
   char         compstr[STRLEN+1];
   char         ch;
  

   fmake( Dirname,  STRLEN );
   fmake( Direntry, STRLEN );
   fcount = 0;
   if (strstr( extension, "dc1") != NULL) {
      strcpy( Dirname.a, getenv("gip_tsk") );
      strcpy( compstr, ".dc1" );      
   }
   if (strstr( extension, "dc2") != NULL) {
      strcpy( Dirname.a, getenv("gip_sub") );
      strcpy( compstr, ".dc2" );      
   }
   if (strstr( extension, "dc3") != NULL) {
      strcpy( Dirname.a, getenv("gip_sub") );
      strcpy( compstr, ".dc3" );      
   }
   if (strstr( extension, "doc") != NULL) {
      strcpy( Dirname.a, getenv("gip_doc") );
      strcpy( compstr, ".doc" );      
   }

   fcount = 0;
   while (flist_c( Dirname, Direntry ) == 0) {
      if (strstr( Direntry.a, compstr ) != NULL) {
         /*-----------------------------------------*/
         /* Copy dir entry string in 'fnames'       */
         /*-----------------------------------------*/
         for (k = 0; k < STRLEN; k++) {
            ch = Direntry.a[k];
            if (ch != ' ') {
               fnames[fcount][k] = ch;
            } else {
               break;
            }
         }
         fnames[fcount][k-4] = '\0';
         fcount++;
      }
   }
   qsort( fnames, fcount, STRLEN, (int(*)())strcmp );   
   (void) sprintf( path, "%.*s", nelc_c( Dirname ), Dirname.a );   
   return( fcount );
}


static int GetLine( char *line, int maxlen, FILE *fp )
/*------------------------------------------------------------*/
/* Read a line from file, check on end of line character and  */
/* remove trailing spaces.                                    */
/*------------------------------------------------------------*/
{
   int  len = 0;
   char ch;


   if (!feof(fp)) {                              /* Quit when EOF */
      while ((ch = fgetc( fp )) != EOF) {        /* Loop until EOF */
         if (ch == '\n') break;
         if (ch == '\t') {                       /* Replace TAB by TABLEN spaces */
            for (i = 0; i < TABLEN; i++) {
               if (len < maxlen) line[len++] = ' ';
            }
         } else {
            if (len < maxlen) line[len++] = ch;         
         }
      }
      line[len] = '\0'; 
      while (len && line[len-1] == ' ') line[--len] = '\0';
   }
   return( len );
}




static void file2screen( char *path, char *docname, char *extension )
/*-------------------------------------------------------------------------*/
/* Display contents of file on screen.                                     */
/*-------------------------------------------------------------------------*/
{
   FILE            *fp;
   char            docline[STRLEN+1];
   char            filename[2*STRLEN];   
         

   (void) sprintf( filename, "%s/%s%s", path, docname, extension );
   fp = fopen( filename, "r" );
   if (fp == NULL) {
      sprintf( message, "Cannot open %s", filename );
      anyoutC( 1, message );
   } else {
      while (!feof(fp)) {
         fgets( docline, STRLEN, fp );         
         anyoutC( 1, docline );
      }
      fclose( fp );
   }
}



static bool fieldfound( char *docline, char *field )
/*----------------------------------------------------------------*/
/* Is this line a purpose line, i.e. does it contain 'Purpose:' , */
/* 'Category:' etc. ? */
/*----------------------------------------------------------------*/
{
   int    i = 0;
   char   compstr[OFFSET+1];
   char   dumstr[256];
   int    l1, l2;
   
  
   l1 = strlen( field );
   l2 = strlen(docline);
   while (i < OFFSET) {
      compstr[i] = toupper(docline[i]);
      i++;
   }
   compstr[i] = '\0';
   if (strstr( compstr, field ) != NULL) {
      i = l1;
      while( (i < l2) && !isalnum(docline[i]) ) i++;
      (void) sprintf( dumstr, 
                     "%-*.*s%s", 
                      OFFSET, OFFSET, field, 
                      &docline[i] );
      if (strlen(docline) > OFFSET) {
         strcpy( docline, dumstr );
         return(YES);
      } else {
         return( NO );
      }      
   } else {
      return( NO );
   }
}


static void printcatname( char *catname, FILE *fpTMP )
/*--------------------------------------------------------*/
/* Write the name of a selected category on display or to */
/* a file.                                                */
/*--------------------------------------------------------*/
{
   int    l;
   char   border[STRLEN];
          
         
   l = MYMIN( strlen( catname ), STRLEN-1 );
   (void ) sprintf( message, "       %s", catname );      
   memset( border, '=', l ); border[l] = '\0';
   (void ) sprintf( border, "       %s", border );
   if (fpTMP != NULL) {
      (void) fprintf( fpTMP, "\n%s\n", border );
      (void) fprintf( fpTMP, "%s\n",   message);
      (void) fprintf( fpTMP, "%s\n\n", border );            
   } else {
      anyoutC( 1, " "     );
      anyoutC( 1, border  );
      anyoutC( 1, message );
      anyoutC( 1, border  );      
      anyoutC( 1, " "     );
   }
}


static void printdocname( char *docname, FILE *fpTMP, int nr )
/*--------------------------------------------------------------*/
/* Write the name of a document to display or file. Append an   */
/* index number.                                                */
/*--------------------------------------------------------------*/
{
   int   l = 0;
   char  name[256];


   l = sprintf( name, "PROGRAM %d:", nr );
   (void) sprintf( name, "%-*.*s", OFFSET, OFFSET, name );
   l = 0;
   while (isprint(docname[l])) { name[OFFSET+l] = toupper( docname[l] ); l++; }
   name[OFFSET+l] = '\0';
   if (fpTMP != NULL) {      
      (void) fprintf( fpTMP, "%s\n", name );   
   } else {
      anyoutC( 1, name );
   }
}


static int nextpurpose( char *line, int maxlen, FILE *fp )
/*---------------------------------------------------------*/
/* Get (if it exists) the next purpose line. There are no  */
/* purpose lines anymore if the length of a line equals    */
/* zero or if end of file is reached.                      */
/*---------------------------------------------------------*/
{
   int      len   = 0;
   char     ch;
   bool     first = YES;


   if (!feof(fp)) {                              /* Quit when EOF */
      while ((ch = fgetc( fp )) != EOF) {        /* Loop until EOF */
         if (ch == '\n') break;          
         if (first) {
            if (isalnum(ch) && !isspace(ch)) {
                memset( line, ' ', OFFSET );
                len  += OFFSET;
                line[len++] = ch;
                first = NO;
            }
         } else {
            if (ch == '\t') {                       /* Replace TAB by TABLEN spaces */
               for (i = 0; i < TABLEN; i++) {
                  if (len < maxlen) line[len++] = ' ';
               }
            } else {
               if (len < maxlen) line[len++] = ch;         
            }
         }      
      }
      line[len] = '\0';                           
      while (len && line[len-1] == ' ') line[--len] = '\0';
   }
   return( len );   
}


static void purposeline( char *path, char *docname, char *extension, 
                         FILE *fpTMP )
/*-------------------------------------------------------------------------*/
/* Find the purpose line in this file. Write all purpose lines.            */
/*-------------------------------------------------------------------------*/
{
   FILE            *fp;
   char            docline[STRLEN+1];
   char            filename[2*STRLEN];
   bool            found = NO;
   int             len;
      

   (void) sprintf( filename, "%s/%s%s", path, docname, extension );
   fp = fopen( filename, "r" );
   if (fp == NULL) {
      (void) sprintf( message, "Cannot open %s", filename );
      anyoutC( 3, message );
      return;
   }
   
   /* File could be opened. */ 

   while( !(feof(fp) || found) ) {
      len = GetLine( docline, STRLEN, fp );
      if ((len > 0) && fieldfound(docline, "PURPOSE:")) found = YES;
   }
   if (!found) {
      int l = OFFSET - strlen("PURPOSE:");
      (void) sprintf( message, 
                     "PURPOSE:%*.*sFound no purpose line in [%s]",
                      l,l," ", docname );
      if (fpTMP != NULL) {
         (void) fprintf( fpTMP, "%s\n", message );
      } else {
         anyoutC( 1, message );
      }
   }

   /* File has at least one purpose line, include doc.name and print this line */
    
   while (found) {
      if (fpTMP != NULL) {
         (void) fprintf( fpTMP, "%s\n", docline );       /* 'docline' was without end of line char. */
      } else {
          anyoutC( 1, docline ); 
      }
      found = (nextpurpose( docline, STRLEN, fp ) > 0);
   }     
   fclose(fp);       /* Close file */
}


static bool iscategory( char *path, char *docname, char *extension, 
                        FILE *fpTMP, char *catline )
/*--------------------------------------------------------------------*/
/* Is there a category in this file?                                  */
/*--------------------------------------------------------------------*/
{
   FILE            *fp;
   char            filename[2*STRLEN];
   bool            found = NO;
   int             len;
      

   (void) sprintf( filename, "%s/%s%s", path, docname, extension );
   fp = fopen( filename, "r" );
   if (fp == NULL) {
      (void) sprintf( message, "Cannot open %s", filename );
      anyoutC( 3, message );
      return( NO );
   }
   
   /* File could be opened. */ 

   while( !(feof(fp) || found) ) {
      len = GetLine( catline, STRLEN, fp );
      if ((len > 0) && fieldfound(catline, "CATEGORY:")) found = YES;
   }
   fclose(fp);
   return( found );
}




static int decodecat( fchar Searchstr, fint num, int maxlen, int cats )
/*-----------------------------------------------------------------------*/
/* Extract from the fchar 'Searchstr' the C-strings. If a string corres- */
/* ponds with a category known by the system (obtained by the function   */
/* 'displaycategories'), the category is stored in the global array      */
/* 'category'. Wildcards can be used ('wmatch'). Special attention for   */
/* CATEGORY=** This selects all documents.                               */
/*-----------------------------------------------------------------------*/
{
   int    k;
   char   ch;
   int    i, j;
   char   *charbuf = NULL;
   bool   found;
   
   
   charbuf = (char *) calloc( maxlen + 1, sizeof(char) );      
   if (charbuf == NULL) {
       anyoutC( 1, "Cannot allocate memory!" );
       return( 0 );
   }
   for (i = 0, j = 0; i < num; i++) {
      for (k = 0; k < maxlen; k++) {
         ch = Searchstr.a[i*maxlen+k];
         if (ch != ' ') {
             charbuf[k] = ch;
         } else {
             break;
         }
      }
      charbuf[k] = '\0';
      
      /* If category consists of two successive wildcard characters, allow */
      /* all categories, also the illegal. No sorting. */
      
      if ((charbuf[0] == Wildcard.a[0]) && (charbuf[1] == Wildcard.a[0]) ) {
         category[0][1] = category[0][0] = Wildcard.a[0];
         category[0][2] = '\0';
         return(1);
      } else {
         for (k = 0; k < cats; k++) {
            found = wmatch_c( tofchar(categorynames[k]), 
                              tofchar(charbuf), 
                              Wildcard, 
                              &caseINsensitive );
            if (found && (j < MAXSEARCH)) {
               strcpy( category[j], categorynames[k] );
               j++;
            }
         }
      }
   }   
   if (j > 2) qsort( category, j, maxlen, (int(*)())strcmp );   
   free( charbuf );
   return(j);
}



static void note( void )
/*----------------------------------------*/
/* Short note to inform user how to use   */
/* paging in Hermes log file.             */
/*----------------------------------------*/
{
   scr = 1;
   
   anyoutC( scr, "================================================================================");   
   anyoutC( scr, "Note: Use <ctrl> V and <ctrl> Z to page through your log file.");
   anyoutC( scr, "      Use <ctrl> P to lock/unlock log file paging.");
   anyoutC( scr, "      In THermes, use <TAB> to get help about HELP (and <TAB> to continue).");
   anyoutC( scr, "================================================================================");
   anyoutC( scr, " ");      
}


static void displaymenu( void )
/*----------------------------------------*/
/* List all possible selections.          */
/*----------------------------------------*/
{
   scr = 1;
   
   anyoutC( scr, " ");
   anyoutC( scr, "            1.  GENERAL HELP ABOUT HERMES");
   anyoutC( scr, "            2.  GENERAL HELP ABOUT APPLICATIONS (input examples)");
   anyoutC( scr, "            3.  GENERAL HELP ABOUT DOCUMENTATION");
   anyoutC( scr, "            4.  DOCUMENTATION:      TASKS/SUBROUTINES/GENERAL [DISPLAY/PRINT]");
   anyoutC( scr, "            5.  PURPOSE LINES:      TASKS/SUBROUTINES/GENERAL [DISPLAY/PRINT]");
   anyoutC( scr, "            6.  SEARCH ON CATEGORY: TASKS/SUBROUTINES/GENERAL [DISPLAY/PRINT]");
   anyoutC( scr, "            7.  QUESTIONS AND ANSWERS");
   anyoutC( scr, " ");      
}


static int displaycategories( char *extension, bool display )
/*------------------------------------------------------------*/
/* Get known categories from $gip_doc/categories.doc          */
/* Store names in global array 'categorynames'                */
/* The value of 'display' determines whether the names are    */
/* displayed or not.                                          */
/*------------------------------------------------------------*/
{
   int        scr = 1;
   char       docline[STRLEN+1];
   char       path[STRLEN];
   char       cattype[STRLEN];
   int        count = 0;
   FILE       *fp;
   char       filename[2*STRLEN+6];
   bool       found = NO;
   int        len;
   int        l1;


   strcpy( path, getenv("gip_doc") );
   (void) sprintf( filename, "%s/categories.doc", path );
   fp = fopen( filename, "r" );
   if (fp == NULL) {
      (void) sprintf( message, "Cannot open %s", filename );
      anyoutC( 1, message );
      return(0);
   }
    
   if (strstr( extension, "dc1") != NULL) {
      strcpy( cattype, "#tasks:" );      
      strcpy( docline, "Possible categories for tasks:" );
   }
   if (strstr( extension, "dc2") != NULL) {
      strcpy( cattype, "#subroutines:" );         
      strcpy( docline, "Possible categories for application subroutines:" );      
   }
   if (strstr( extension, "dc3") != NULL) {
      strcpy( cattype, "#subroutines:" );         
      strcpy( docline, "Possible categories for low level subroutines:" );      
   }
   if (strstr( extension, "doc") != NULL) {
      strcpy( cattype, "#general:" );      
      strcpy( docline, "Possible categories for general documents:" );
   }
   if (display) {
      anyoutC( scr, docline );
      l1 = strlen( docline );
      memset( docline, '=', l1 );
      anyoutC( scr, docline );
   }
   
   l1 = strlen( cattype );
   while( !(feof(fp) || found) ) {
      len = GetLine( docline, STRLEN, fp );
      if ((len > 0) && (strncmp(docline, cattype, l1) == 0)) found = YES;
   }
   
   if (!found) {
      anyoutC( 1, "Cannot obtain standard categories!" );
      fclose(fp);
      return(0);
   }
   
   while (!feof(fp) && (GetLine( docline, STRLEN, fp ) !=0)) {
      if (display) anyoutC( scr, docline );
      strcpy( categorynames[count++], strtok(docline, " /" ) );
   }    
     
   fclose(fp);         
   if (display) anyoutC( scr, " " );
   return( count );
}


static bool firsttime( char *extension )
/*--------------------------------------------------*/
/* Prevent writing the list of document names more  */
/* than one time in one session.                    */
/*--------------------------------------------------*/
{
   static bool    firstdc1 = YES;
   static bool    firstdc2 = YES;
   static bool    firstdc3 = YES;
   static bool    firstdoc = YES;
   
   if ((strstr( extension, ".dc1" ) != NULL) && firstdc1) {
      firstdc1 = NO;
      return( YES );
   }
   if ((strstr( extension, ".dc2" ) != NULL) && firstdc2) {
      firstdc2 = NO;      
      return( YES );
   }
   if ((strstr( extension, ".dc3" ) != NULL) && firstdc3) {
      firstdc3 = NO;
      return( YES );
   }
   if ((strstr( extension, ".doc" ) != NULL) && firstdoc) {
      firstdoc = NO;
      return( YES );
   }
   return( NO );
}         


static bool firstcat( char *extension )
/*--------------------------------------------------*/
/* Prevent writing the list of category names more  */
/* than one time in one session.                    */
/*--------------------------------------------------*/
{
   static bool    firstcatdc1 = YES;
   static bool    firstcatdc2 = YES;
   static bool    firstcatdc3 = YES;
   static bool    firstcatdoc = YES;
   
   if ((strstr( extension, ".dc1" ) != NULL) && firstcatdc1) {
      firstcatdc1 = NO;
      return( YES );
   }
   if ((strstr( extension, ".dc2" ) != NULL) && firstcatdc2) {
      firstcatdc2 = NO;      
      return( YES );
   }
   if ((strstr( extension, ".dc3" ) != NULL) && firstcatdc3) {
      firstcatdc3 = NO;
      return( YES );
   }
   if ((strstr( extension, ".doc" ) != NULL) && firstcatdoc) {
      firstcatdoc = NO;
      return( YES );
   }
   return( NO );
}         



static bool categoryline( char *path, char *docname, char *extension, 
                          char *category, int cats, FILE *fpTMP )
/*---------------------------------------------------------------------*/
/* Try to find category line in file (iscategory) and compare category */
/* name with given name. If CATEGORY=** , discard all category names.  */
/*---------------------------------------------------------------------*/
{
   bool            found;
   char            catline[STRLEN];
   bool            ok = NO;
   int             i;
   bool            all;


   found = iscategory( path, docname, extension, fpTMP, catline );
   all = ( (category[0] == Wildcard.a[0]) && (category[1] == Wildcard.a[0]) );
   if (!found) {
      int l = OFFSET - strlen("CATEGORY:");      
      (void) sprintf( message, 
                     "CATEGORY:%*.*sNo category found in [%s]",
                      l,l," ", docname );                      
      if (fpTMP != NULL) {
         (void) fprintf( fpTMP, "%s\n", message );
      } else {
         anyoutC( 1, message );
      }
   } else {
      if ( (strstr(catline, category) != NULL) || all ) {
         for (i = 0; i < cats; i++) {
            if (strstr( catline, categorynames[i]) != NULL) {               
               ok = YES;
               break;
            }
         }
         if (!ok) strcat( catline, " (Not in GIPSY category list)" );
         if (fpTMP != NULL) {
            (void) fprintf( fpTMP, "%s\n", catline );
         } else {
            anyoutC( 1, catline );
         }
      }
      else {
         found = NO;
      }
   } 
   return( found );
}




static void catdoc( char *extension )
/*--------------------------------------------------*/
/* Print document or purpose line                   */
/*--------------------------------------------------*/
{
   fint         dfault, nitems;
   fint         r1;
   fint         showdev = 1;
   fint         prnnum;
   fint         cols, rows;
   int          fcount;
   char         docname[STRLEN];
   char         path[STRLEN];
   int          i, j;
   fchar        Searchstr;
   fint         remove;
   int          catnum;
   int          cats;
   int          catsfound;



   cats   = displaycategories( extension, firstcat(extension) );
   fcount = getnames( extension, path );            
   fmake( Searchstr, MAXSEARCH*STRLEN );
   Searchstr.l = STRLEN;
   do {
      nitems  = MAXSEARCH;
      dfault  = REQUEST;
      Key     = tofchar("CATEGORY=");
      Mes     = tofchar("Give name of category(-ies):  [return to menu]" );
      r1      = usercharu_c( Searchstr, &nitems, &dfault, Key, Mes );
      cancel_c( Key );
      showdev = 1;
      if (r1 != 0) {         
         prnnum = prnmenu( KEY_PRINTER, &showdev, &prnnum, &cols, &rows );
         if (prnnum != 0) {
            tmpnam( TMPprintfile );        /* Create unique name for printer file */
            fpTMP  = fopen( TMPprintfile, "w");
            if (fpTMP == NULL) {
               anyoutC( 1, "Cannot open temp. file" );
               return;
            }
         } else {
            fpTMP  = NULL;
         }
         
         catsfound = decodecat( Searchstr, r1, STRLEN, cats );
         for (catnum = 0; catnum < catsfound; catnum++) {
            printcatname( category[catnum], fpTMP );
            for (i = 0, j = 1; i < fcount; i++) {
               strcpy( docname, fnames[i] );
               if (categoryline( path, docname, extension, category[catnum], cats, fpTMP )) {  
                  printdocname( docname, fpTMP, j++ );
                  purposeline( path, docname, extension, fpTMP );
                  emptyline( fpTMP );
               }
            }
         }
         if (fpTMP != NULL) {
            remove = YES;
            fclose( fpTMP );
            activateprinter( prnnum, TMPprintfile, remove, docname );
         } else {
            note();
         }
      }
   } while (r1 != 0);
   cancel_c( KEY_PRINTER );
}
         
         


static void printdoc( char *extension )
/*--------------------------------------------------*/
/* Print document or purpose line                   */
/*--------------------------------------------------*/
{
   fint         dfault, nitems;
   fint         r1;
   fint         showdev;
   fint         prnnum;
   fint         cols, rows;
   int          fcount;
   char         docname[STRLEN];
   char         path[STRLEN];
   int          i, k;
   char         compstr[STRLEN+1];
   fchar        Searchstr;
   char         filename[2*STRLEN+6];  /* Path / docname extension 0 */
   fint         remove;
   bool         found;
   int          name;
   char         ch;
   bool         toscreen;     


   fcount = getnames( extension, path );            

   if (firsttime(extension)) {
      for (i = 0; i < fcount; i++) {
         if ((i%3) == 0) (void) sprintf( message, "%-25s", fnames[i] );
         if (i == (fcount-1)) {
            anyoutC( 1, message );      
         } else {
            if ((i%3) == 1) (void) sprintf( message, "%s %-25s", message, fnames[i] );
            if ((i%3) == 2) {
               (void) sprintf( message, "%s %-25s", message, fnames[i] );
               anyoutC( 1, message );
            }
         }
      }
   }      
   
   fmake( Searchstr, MAXSEARCH*STRLEN );
   Searchstr.l = STRLEN;
   do {
      nitems = MAXSEARCH;
      dfault = REQUEST;
      Key    = tofchar("DOCUMENT=");
      Mes    = tofchar("Give name of doc. to print:   [return to menu]" );
      r1     = userchar_c( Searchstr, &nitems, &dfault, Key, Mes );
      cancel_c( Key );
      if (r1 != 0) {
         for (name = 0; name < r1; name++) {              
            found = NO;
            for (i = 0; i < fcount; i++) {
               /*-----------------------------------------*/
               /* Copy search string in compare string    */
               /*-----------------------------------------*/               
               for (k = 0; k < STRLEN; k++) {
                  ch = Searchstr.a[name*STRLEN+k];
                  if (ch != ' ') {
                     compstr[k] = ch;
                  } else {
                     break;
                  }
               }
               compstr[k] = '\0';
               if (wmatch_c( tofchar(fnames[i]), tofchar(compstr), Wildcard, &caseINsensitive)) {
                  found    = YES;
                  strcpy( docname, fnames[i] );
                  showdev  = 1;
                  prnnum   = prnmenu( KEY_PRINTER, &showdev, &prnnum, &cols, &rows );
                  toscreen = (prnnum == 0);
                  if (toscreen) {
                     /* send document to screen */
                     file2screen( path, docname, extension );
                     note();
                  } else {
                     (void) sprintf( filename, "%s/%s%s", path, docname, extension );
                     remove = NO;
                     activateprinter( prnnum, filename, remove, docname );
                  }
               }
            }
            if (!found) {
               (void) sprintf( message, "%s not in selected list", compstr );
               anyoutC( 1, message );
            }
         } /* End for all search strings */ 
      }
   } while (r1 != 0);           
   cancel_c( KEY_PRINTER );
}



static void purposedoc( char *extension )
/*--------------------------------------------------*/
/* Print document or purpose line                   */
/*--------------------------------------------------*/
{
   fint         dfault, nitems;
   fint         r1;
   fint         showdev;
   fint         prnnum;
   fint         cols, rows;
   int          fcount;
   char         docname[STRLEN];
   char         path[STRLEN];
   int          i, j, k;
   char         compstr[STRLEN+1];
   fchar        Searchstr;
   char         filename[2*STRLEN+6];
   fint         remove;
   bool         found;
   int          name;
   char         ch;
   int          cats;
   char         all[3];


   fcount   = getnames( extension, path );            
   
   if (firsttime(extension)) {  
      for (i = 0; i < fcount; i++) {
         if ((i%3) == 0) (void) sprintf( message, "%-25s", fnames[i] );
         if (i == (fcount-1)) {
            anyoutC( 1, message );      
         } else {
            if ((i%3) == 1) (void) sprintf( message, "%s %-25s", message, fnames[i] );
            if ((i%3) == 2) {
               (void) sprintf( message, "%s %-25s", message, fnames[i] );
               anyoutC( 1, message );
            }
         }
      }
   }      
   
   cats = displaycategories( extension, NO );

   fmake( Searchstr, MAXSEARCH*STRLEN );
   Searchstr.l = STRLEN;
   do {
      nitems  = MAXSEARCH;
      dfault  = REQUEST;
      Key     = tofchar("DOCUMENT=");
      Mes     = tofchar("Give document(s) for purpose line:   [return to menu]" );
      r1      = userchar_c( Searchstr, &nitems, &dfault, Key, Mes );
      showdev = 1;      
      cancel_c( Key );
      if (r1 != 0) {
         prnnum = prnmenu( KEY_PRINTER, &showdev, &prnnum, &cols, &rows );
         if (prnnum != 0) {
            tmpnam( TMPprintfile );        /* Create unique name for printer file */
            fpTMP  = fopen( TMPprintfile, "w");
            if (fpTMP == NULL) {
               anyoutC( 1, "Cannot open temp. file" );
               return;
            }
         } else {
            fpTMP  = NULL;
         }
         all[0] = all[1] = Wildcard.a[0]; all[2] = '\0';
         for (name = 0; name < r1; name++) {              
            found = NO;
            for (i = 0, j = 1; i < fcount; i++) {
               /*-----------------------------------------*/
               /* Copy search string in compare string    */
               /*-----------------------------------------*/               
               for (k = 0; k < STRLEN; k++) {
                  ch = Searchstr.a[name*STRLEN+k];
                  if (ch != ' ') {
                     compstr[k] = ch;
                  } else {
                     break;
                  }
               }
               compstr[k] = '\0';
               if (wmatch_c( tofchar(fnames[i]), tofchar(compstr), Wildcard, &caseINsensitive)) {
                  found = YES;
                  strcpy( docname, fnames[i] );                  
                  (void) sprintf( filename, "%s/%s%s", path, docname, extension );
                  /* display purpose line */
                  printdocname( docname, fpTMP, j++ );
                  (void) categoryline( path, docname, extension, all, cats, fpTMP );
                  purposeline( path, docname, extension, fpTMP ); 
                  emptyline( fpTMP );
               }
            }
            if (!found) {
               (void) sprintf( message, "%s not in selected list", compstr );
               anyoutC( 1, message );
            }
         } /* End for all search strings */ 
         if (fpTMP != NULL) {
            remove = YES;
            fclose( fpTMP );
            activateprinter( prnnum, TMPprintfile, remove, docname );
         } else {
            note();
         }
      }
   } while (r1 != 0);
   cancel_c( KEY_PRINTER );
}


static void questions( void )
/*-------------------------------------------------------------*/
/* Implementation of a small question and answer item. Must be */
/* updated in this source code. Expand to file later.          */
/*-------------------------------------------------------------*/
{
   int         scr = 1;
   fint        r1;
   fint        nitems, dfault;
   fint        question;

   
   anyoutC( scr, " " );
   anyoutC( scr, "================ QUESTIONS ================ ");
   anyoutC( scr, " " );
   anyoutC( scr, "1. How do I report a bug?" );
   anyoutC( scr, "2. Can I make a contour plot without frame and coordinates etc.?" );
   anyoutC( scr, "3. Where can I find documentation about input syntax" );
   anyoutC( scr, "4. How to convert an old VAX GIPSY set to a new style set?" );
   anyoutC( scr, " " );
   do { 
      Key    = tofchar("QUESTION=");
      Mes    = tofchar("Get answer for question nr:       [return to menu]");
      nitems = 1;
      dfault = REQUEST;
      r1     = userint_c( &question, &nitems, &dfault, Key, Mes );
      cancel_c( Key );
      if (r1 == 0) break;
      switch (question) {
         case 1:
            anyoutC( scr, "Use the program REPORT to send a bug report. The name ");
            anyoutC( scr, "of the responsible author is determined by REPORT");
         break;
         case 2:
            anyoutC( scr, "Use CPLOT with MEAGRE=Y to create a plot without frame, ");
            anyoutC( scr, "units and labels.");
         break;
         case 3:
            anyoutC( scr, "Documentation about the input syntax can be found under General type");
            anyoutC( scr, "documentation in this program. SUBJECT=4 DOCTYPE=4 DOCUMENT=input");
            anyoutC( scr, "will display (or send to printer) the information you need.");
         break;
         case 4:
            anyoutC( scr, "On the VAX 8650, use the FITS program FTSOUT with MEDIUM=FILES");
            anyoutC( scr, "to create a FITS file of your old set. Use FTP from the UNIX site");
            anyoutC( scr, "to get the file. Put this file in a subdirectory of your current");
            anyoutC( scr, "directory and rename the file in 'file000001.mt'. Use RFITS with");
            anyoutC( scr, "INTAPE=<your subdirectory> and INFILES=0 to create a new GIPSY set.");
         break;
      }
   } while(1);
   note();
}
   

static void generalhelp( int opt )
/*-----------------------------------------------------------------*/
/* Display general first aid like help about Hermes, applications  */
/* and documentation. Read text from $gip_doc/help.doc             */
/*-----------------------------------------------------------------*/
{
   char         path[STRLEN];
   char         filename[2*STRLEN+6];
   char         docline[LONGLEN+1];
   bool         print = NO;
   bool         quit  = NO;
   char         searchstr[10];
   FILE         *fp;
   int          l;
   fint         prnnum;
   fint         showdev = 1;
   fint         cols, rows;
   bool         toprint;
   
  
   l = sprintf( searchstr, "#start%d", opt );
   strcpy( path, getenv("gip_doc") );
   (void) sprintf( filename, "%s/help.doc", path );       /* Open text file */
   fp = fopen( filename, "r" );
   if (fp == NULL) {
      (void) sprintf( message, "Cannot open %s", filename );
      anyoutC( 1, message );
      return;
   }

   prnnum = prnmenu( KEY_PRINTER, &showdev, &prnnum, &cols, &rows ); /* Select destination */
   cancel_c( KEY_PRINTER );
   if (prnnum != 0) {
      tmpnam( TMPprintfile );                    /* Create unique name for printer file */
      fpTMP  = fopen( TMPprintfile, "w");
      if (fpTMP == NULL) {
         anyoutC( 1, "Cannot open temp. file" );
         return;
      }
   } else {
      fpTMP  = NULL;
   }
   toprint = (fpTMP != NULL);
   
   /* Print line after #start1 etc. was found */
   while (!feof(fp) && !quit) {
      GetLine( docline, LONGLEN, fp );
      if ((strncmp( docline, "#end",   4 ) == 0 ) && print) quit  = YES;      
      if (print && !quit) {
         if (toprint) {
            (void) fprintf( fpTMP, "%s\n", docline );
         } else {
            anyoutC( 1, docline );
         }
      }
      if (strncmp( docline, searchstr, l ) == 0 ) print = YES;
   }
   fclose( fp );   
   
   if (toprint) {
      fint    remove = YES;
      fclose( fpTMP );
      activateprinter( prnnum, TMPprintfile, remove, "General help" );
   } else {  
      note();
   }
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      (void) myname_c( Task );             /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   
   fmake( Wildcard, 1); 
   Wildcard = tofchar( "*" );     
   setfblank_c( &blank );

   displaymenu();
   note();
   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   do {         
      showdev = 1;
      dfault  = REQUEST;
      nitems  = 1;
      Key     = tofchar("SUBJECT=");
      Mes     = tofchar("Give number of subject:     [exit]");
      r1      = userint_c( subjects, &nitems, &dfault, Key, Mes );
      cancel_c( Key );
      for (i = 0; i < (int) r1; i++) {
         subj = (int) subjects[i];
         switch (subj) {
            case 1:
               generalhelp( 1 );
            break;
            case 2:
               generalhelp( 2 );
            break;   
            case 3:          
               generalhelp( 3 ); 
            break;
            case 4:
               getextension( extension );
               printdoc( extension );
            break;
            case 5:
               getextension( extension );           
               purposedoc( extension );
            break;
            case 6:
               getextension( extension );           
               catdoc( extension );
            break;
            case 7:
               questions();
            break;
         }
      }
   } while (r1);

   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
