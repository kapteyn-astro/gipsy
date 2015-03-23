/*
                            COPYRIGHT (c) 1995
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             history.dc1

Program:       HISTORY

Purpose:       read, or update history or comment in set header

Category:      HEADER, UTILITY

File:          history.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set, subsets:

               Maximum number of subsets is 1024.


***STRING=     Enter string to add to hist./comm.:  [interactive mode]
   
               You can use HISTORY in an automatic or interactive mode.
               If you start HISTORY with a STRING= specification,
               then this string will be added to the history or 
               comments depending on ITEM=. Otherwise, the interactive 
               mode is started.
               
                                      
   ITEM=       Select COMMENT or HISTORY items:                  C/[H]

 
   MODE=       Do you want to EDIT or READ history items?        E/[R]
   
  

Description:   In automatic mode:
               Add string in STRING= to history or comments items
               (ITEM=) for all subsets. If STRING= is empty, goto
               interactive mode.
               
               In interactive mode:
               List (or edit) all available information in the header
               of INSET= stored under the commentary (FITS) keywords
               COMMENT or HISTORY. One of these is selected with ITEM=
               If you want a list of the stored information, use
               MODE=R (the default). If you want to edit the infor-
               mation, use MODE=E  In this mode, you enter the
               editor (specified with the environment variable EDITOR)
               and edit or create the file. If you save the changes in 
               the editor, then also the header information will be
               updated. 
               "History text should contain a history of steps and
               procedures associated with the processing of the 
               associated data (NOST)".
               
               INSET= could also select subsets (e.g. 
               INSET=AURORA FREQ 3:10). Read and edit actions are
               repeated for each subset separately. Before starting 
               an edit action, the edit prompt will display which 
               subset is used. Usually you will read/edit history and 
               comment at top level. Then you need only to give the 
               name of the set (e.g. INSET=AURORA)
               
               If you cancel an edit action, or leave the editor
               without writing the changes, the header information
               will not be changed.


Notes:         Use Hermes debug mode (<esc> S) for debug information.


Updates:       Apr 25, 1995: VOG, Document created.
               Apr 25, 1996: VOG, STRING= added (automatic mode)

#<
*/

/*  history.c: include files     */

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
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "editfile.h"     /* User action interface routine to edit a text file. */


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
#include    "gdsd_rewind.h"  /* Set current read position at beginning of descriptor item.*/
#include    "gdsd_delete.h"  /* Delete descriptor item.*/
#include    "gdsd_wvar.h"    /* Write variable length record to descriptor item.*/
#include    "gdsd_rvar.h"    /* Read variable length record from descriptor item.*/
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_name.h"    /* Return the name of an axis.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "gds_errstr.h"   /* Obtain the message string associated with a GDS error code. */



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

#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     1024       /* Max. allowed subsets */
#define MAXBUF         4096       /* Buffer size for I/O */
#define STRLEN         256        /* Max length of strings */
#define FILENAMELEN    256        /* Max length of file names */
#define FITSLEN        20         /* Max length of header items etc.*/
#define EMPTYREAD      -3
#define NOMOREDATA     -4
#define EMPTYREWIND    -7
#define OFFSET         17
#define NONE           0          /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0

/*--------------------------------------------------*/
/* The HISTORY and COMMENT keyword shall have no    */
/* associated value; columns 9-80 may contain any   */
/* ASCII text. NOST, FITS                           */
/*--------------------------------------------------*/
#define MAXRECLEN      80-9       


/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")


/* Variables for input */


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


/* Miscellaneous */

static char     message[STRLEN];    /* All purpose character buffer. */




static void editorerr( int err )
/*-----------------------------------------------------------*/
/* PURPOSE: Print error message for editor action.           */
/*-----------------------------------------------------------*/
{
   if      (err == -1)
      anyoutf( 1, "Warning: user canceled the edit" );
   else if (err == -2)
      anyoutf( 1, "Warning: cannot start editor" );
   else if (err == -3)
      anyoutf( 1, "Warning: editor process exited with error status");
   else if (err == -666)
      anyoutf( 1, "Warning: this version of Hermes does not support EDITTFILE" );
}



static void printGDSerror( int err )
/*-----------------------------------------------------------*/
/* PURPOSE: Print an error message associated with this error*/
/*-----------------------------------------------------------*/
{
    fchar Errstr;
    fint  r = (fint) err;
    fmake( Errstr, STRLEN );
    gds_errstr_c( Errstr, &r );
    anyoutf( 1, "Application has GDS error: %.*s", nelc_c(Errstr), Errstr.a );
}



static int makeFITSstr( char *str )
/*-----------------------------------------------------------*/
/* PURPOSE: Adjust string 'str' according to FITS standard.  */
/* A string read from file will have a newline character     */
/* which must be replaced by a 0. Check the string whether it*/
/* smaller than the FITS length defined in 'MAXRECLEN'.      */
/* Return the length of the manipulated string.              */
/*-----------------------------------------------------------*/
{
   int i;
   int len = strlen(str);

   if (len > MAXRECLEN)
   {
      str[MAXRECLEN-1] = '\0';
      len = MAXRECLEN;
   }
   for (i = len-1; i >= 0; i--)
   {
      if (str[i] == '\n')
      {
         str[i] = '\0';
         break;
      }
   }
   return( nelc_c(tofchar(str)) );
}



static char *showsub( char  *buf, 
                      fchar  Setin, 
                      fint   subset, 
                      fint  *axnum )
/*--------------------------------------------------------------*/
/* PURPOSE: Put subset info in char. buffer.                    */
/* Generate name of subset axis and give the subset.            */
/*--------------------------------------------------------------*/
{
   fint   setdim, subdim;
   int    n;
   fchar  Axisname;
   fint   r1, r2;
   fint   grid;
   fint   setlevel = 0;
   char   buf2[STRLEN];
   


   setdim = gdsc_ndims_c( Setin, &setlevel );                   /* dimension of set */
   subdim = gdsc_ndims_c( Setin, &subset );

   if (setdim == subdim) 
   {
      strcpy( buf, "Set level" );
      return( buf );
   }
   strcpy( buf, " Subset:" );
   fmake( Axisname, FITSLEN );
   for (n = subdim; n < setdim; n++) 
   {
      r2 = r1 = 0;
      gdsc_name_c( Axisname, Setin, &axnum[n], &r1 );
      grid = gdsc_grid_c( Setin, &axnum[n], &subset, &r2 );
      if ( (n + 1) == setdim) 
         sprintf( buf2, "%s=%d ", strtok(Axisname.a, " -"), grid );
      else 
         sprintf( buf2, "%s=%d,", strtok(Axisname.a, " -"), grid );
      strcat( buf, buf2 );
   }
   return( buf );
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
   fint     r1;                          /* Result values for different routines. */
   bool     readhist;
   bool     history;
   bool     comment;
   bool     edit;
   fchar    Setin;                       /* Name of input set */   
   fchar    Option;
   fchar    Descriptor;
   fchar    Addtxt;
   fint     subnr;                       /* Counter for subset loop. */
   fint     subdim;
   int      status;
   int      addtxt;                      /* Add text automatically to H/C */




   init_c();                             /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;              /* Name of current task */
      fmake( Task, 20 );                 /* Macro 'fmake' must be available */
      myname_c( Task );                  /* Get task name */
      Task.a[nelc_c(Task)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, RELEASE ); /* Show task and version */
   }

   /*--------------------------------------------------*/
   /* Get the input set. Documentation can be found in */
   /* $gip_sub/gdsinp.dc2                              */
   /*--------------------------------------------------*/
   {
      fmake( Setin, STRLEN );
      dfault  = NONE;
      showdev = 16;                 /* Show set info only in debug mode */
      subdim  = 0;                  /* Allow only n-dim structures */
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
   
    
   fmake( Option, 1 );   
   (void) str2char( "H", Option );
   dfault   = REQUEST;
   nitems   = 1;
   r1       = usercharu_c( Option, 
                           &nitems, 
                           &dfault, 
                           tofchar("ITEM="),
                           tofchar("Select COMMENT or HISTORY items:       C/[H]") );
   history = (Option.a[0] == 'H');
   if (Option.a[0] != 'H' && Option.a[0] != 'C')
   {
      anyoutf( 1, "No such option ==> exit program!" );
      finis_c();
      return(EXIT_SUCCESS);
   }
   comment = !history;
      

   fmake( Addtxt, MAXRECLEN );
   dfault = HIDDEN;
   nitems = 1;
   if (history)
      strcpy( message, "Enter string to add to history:   [interactive mode]" );
   else
      strcpy( message, "Enter string to add to comments:   [interactive mode]" );
   r1  = usertext_c( Addtxt, &dfault, tofchar("STRING="),
                     tofchar(message) );
   addtxt = (r1 > 0);

   if (!addtxt)
   {
      (void) str2char( "R", Option );   
      if (history)
         strcpy( message, "Do you want to EDIT or READ history items?    E/[R]");
      else 
         strcpy( message, "Do you want to EDIT or READ comment items?    E/[R]");
      dfault   = REQUEST;
      nitems   = 1;
      r1       = usercharu_c( Option, 
                              &nitems, 
                              &dfault, 
                              tofchar("MODE="),
                              tofchar(message) );
      readhist = (Option.a[0] == 'R');
      edit = !readhist;
      if (Option.a[0] != 'R' && Option.a[0] != 'E')
      {
         anyoutf( 1, "No reading or appending ==> exit program!" );
         finis_c();
         return(EXIT_SUCCESS);
      }
   }


   fmake( Descriptor, FITSLEN );
   if (history)
      (void) str2char( "HISTORY", Descriptor );
   else
      (void) str2char( "COMMENT", Descriptor );


   if (addtxt)
   {
      makeFITSstr( Addtxt.a );
      anyoutf( 16, "Writing: %s", Addtxt.a );
      for(subnr = 0; subnr < nsubs; subnr++)
      {
         r1 = 0;
         gdsd_wvar_c( Setin,   
                      Descriptor,
                      &subin[subnr],
                      Addtxt,
                      &r1 );
         if (r1 < 0)
            printGDSerror( r1 );
      }
      finis_c();
      return( EXIT_SUCCESS );
   }


   if (readhist)
   /*--------------------------------------------------*/
   /* User wants to list all history and comment items.*/
   /*--------------------------------------------------*/
   {
      char   item[10];
      char   border[STRLEN];

      if (history)
         strcpy( item, "HISTORY" );
      else
         strcpy( item, "COMMENT" );
      r1 = sprintf( message, "%s from header of set [%.*s]",
                    item, nelc_c(Setin), Setin.a );
      memset( border, '=', r1 );
      border[r1] = '\0';
      anyoutf( 1, " " );
      anyoutf( 1, "%*s%s", OFFSET, " ", border );
      anyoutf( 1, "%*s%s", OFFSET, " ", message );
      anyoutf( 1, "%*s%s", OFFSET, " ", border );

      for(subnr = 0; subnr < nsubs; subnr++)    /* Loop over all subsets */
      {
         r1 = 0;
         gdsd_rewind_c( Setin,                  /* Rewind var.recs. before read action */
                        Descriptor, 
                        &subin[subnr], 
                        &r1 );
         if (r1 < 0)                            /* Something wrong */
         {
            if (r1 == EMPTYREWIND)
               anyoutf( 1, "No items found" );
            else
               printGDSerror( r1 );
         }
         else
         {
            do
            {
               fchar  Headerstr;
               fmake( Headerstr, STRLEN );
               /* Read variable length record from descriptor item */
               r1 = 0;
               gdsd_rvar_c( Setin, 
                            Descriptor, 
                            &subin[subnr], 
                            Headerstr, 
                            &r1 );
               if (r1 < 0 && r1 != NOMOREDATA)  /* Something wrong, but not end of data */
                  printGDSerror( r1 );
               else if (r1 != NOMOREDATA) 
                  anyoutf( 1, "%.*s", nelc_c(Headerstr), Headerstr.a );
            }
            while (r1 >= 0);                    /* No records anymore */
         }
         memset( border, '-', 80 );
         border[80] = '\0';         
         anyoutf( 1, border );
      }
   }
   else
   /*--------------------------------------------------*/
   /* Read items and store in Ascii file. Edit file and*/
   /* store new items from that file in header.        */
   /*--------------------------------------------------*/
   {
      char  tmpfile[STRLEN];
      FILE  *fp;

      for(subnr = 0; subnr < nsubs; subnr++)
      {
         tmpnam( tmpfile );                    /* Temp. file for editor */
         fp  = fopen( tmpfile, "w" );
         if (fp == NULL)
            errorf( 4, "Could not open tmp file: %s", tmpfile );

         r1 = 0;
         gdsd_rewind_c( Setin, 
                        Descriptor, 
                        &subin[subnr], 
                        &r1 );
         anyoutf( 16, "Error return code in gdsd_rewind = %d", r1 );
         status = r1;
         if (r1 < 0 && r1 != EMPTYREWIND)      /* Something wrong */
            printGDSerror( r1 );
         else
         /*--------------------------------------------------*/
         /* There is data. Read the data and write to the    */
         /* temp. file.                                      */
         /*--------------------------------------------------*/
         {
            if (status != EMPTYREWIND)       
            {
               do
               {
                  fchar  Headerstr;
                  fmake( Headerstr, STRLEN );
                  /* Read variable length record from descriptor item */
                  r1 = 0;
                  gdsd_rvar_c( Setin, 
                               Descriptor, 
                               &subin[subnr], 
                               Headerstr, 
                               &r1 );
                  anyoutf( 16, "Error return code in gdsd_rvar = %d", r1 );
                  if (r1 < 0 && r1 != NOMOREDATA)   /* Something wrong, but not end of data */
                     printGDSerror( r1 );
                  else
                     fprintf( fp, "%.*s\n", 
                              nelc_c(Headerstr), Headerstr.a );
               }
               while (r1 >= 0);                     /* No records anymore */
            }
            fclose( fp );                           /* Close tmp file in any case */

            if (subin[subnr] != 0)                  /* If not top level ==> show */
            {
               showsub( message, Setin, subin[subnr], axnum );
               strcat( message, " (^C to abort edit)" );
            }               
            else 
               strcpy( message, "Use ^C to abort edit" );

            r1 = editfile_c( tofchar(tmpfile), tofchar(message) );
            if (r1 != 0)
               editorerr( r1 );
            else
            /*--------------------------------------------------*/
            /* Delete the history items on this level. Start    */
            /* reading from tmp. file and add new items.        */
            /*--------------------------------------------------*/
            {
               fp = fopen( tmpfile, "r" );
               if (fp == NULL)
                   errorf( 4, "Could not read tmp file: %s", tmpfile );
 
               if (status != EMPTYREWIND)
               {
                  anyoutf( 16, "Deleting %.*s item(s)", 
                           nelc_c(Descriptor), Descriptor.a );
                  r1 = 0;               
                  gdsd_delete_c( Setin, 
                                 Descriptor, 
                                 &subin[subnr], 
                                 &r1 );
                  if (r1 < 0)
                     printGDSerror( r1 );               
               }
             
               /*--------------------------------------------------*/
               /* Read from file. But process string first. Its    */
               /* length may not exceed a certain length, and the  */
               /* newline character must be replaced by a 0.       */
               /*--------------------------------------------------*/
               while (!feof(fp))
               {
                  if ( fgets(message, STRLEN, fp) );
                  {
                     if (!feof(fp))                 /* Extra check on end of file */
                     {
                        if ( makeFITSstr(message) )
                        {
                           anyoutf( 16, "Writing: %s", message );
                           r1 = 0;
                           gdsd_wvar_c( Setin, 
                                        Descriptor, 
                                        &subin[subnr], 
                                        tofchar(message), 
                                        &r1 );
                           if (r1 < 0)
                              printGDSerror( r1 );
                        }
                     }
                  }
               }
               fclose( fp );
               r1 = remove( tmpfile );              /* Remove tmp file */
            }
         }
      }
   }

   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
