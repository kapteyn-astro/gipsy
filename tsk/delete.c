/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             delete.dc1

Program:       DELETE

Purpose:       Delete sets

Category:      UTILITY

File:          delete.c

Author:        M. Vogelaar

Keywords:

** LIST=       List with GIPSY set names?                         Y/[N]

               Can be either Y or N. If user specifies LIST=Y, a list
               with set names in the current directory is displayed.
               The default is LIST=N


   INSET=      Give name of set to delete:                       [stop]
   
               Enter:
               1) One set name, e.g.:
                  INSET=AURORA
               2) Set names separated by a semicolon (;) e.g.:
                  INSET=AURORA;ngc3079a;ngc3079b
               3) A path followed by a set name (no wildcard expansion):
                  INSET=./WORK/ngc400
               4) Set name(s) containing the wildcard character '*'.
                  Wildcards can appear everywhere in a name.
                  The wildcard expansion works only in the current
                  directory!
                  INSET=ngc*   (Delete all GIPSY sets that start 
                                with 'ngc')
                  INSET=*      (Delete all GIPSY sets in current dir.)
                  
               The names of sets which the user wants to delete
               are asked in a loop. Press carriage return to abort this
               loop and to stop the program.


   OK=         Ok to delete <setname> ?                           Y/[N]
   
               Confirm the choice for EACH file that you entered
               with INSET=
               Long file names are cut off. A number of dots indicate
               if this has happened.



Description:   DELETE deletes GIPSY sets (image part and descriptor
               part). After the introduction of the GDS server,
               it became more important to be able to delete sets
               within the environment where the server is running.
               If f.i. you delete sets with the unix 'rm' command,
               the program DISK will still list those sets if the
               server is still running. Program DELETE doe the job
               properly. It checks whether a file is a valid GIPSY set
               and will warn you if it is not (program will display
               the corresponding GDS error like: GDS -- bad descriptor 
               header). However, you still can delete these invalid sets.
               The program stores the names of all files that end 
               on '.descr'. This list is necessary if you want to
               use a wildcard expansion using the asterisk character
               '*'. This explains why wildcard expansion can only
               be used in the current directory. If a set (entered
               with INSET=) does not exist, then a warning is displayed
               in your log area. If a set exists, but does not conform
               to the rules for a valid GIPSY set, then a warning
               is displayed in the log area together with the 
               corresponding GDS error, and a warning will be inserted 
               in the OK= prompt. Notice that it is still possible 
               to delete such files. So program DELETE can delete 
               your very important backupless text file 'important.descr'
               if that is what you want.
               
              
Updates:       Jun 15,  1995: VOG, Completely rewritten.
#<

*/

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "ctype.h"
#include "time.h"
#include "math.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"
#include "flist.h"
#include "nelc.h"
#include "myname.h"
#include "anyout.h"
#include "gds_exist.h"
#include "gds_delete.h"
#include "gds_errstr.h"
#include "userfio.h"
#include "userint.h"
#include "userlog.h"
#include "userchar.h"
#include "cancel.h"
#include "status.h"
#include "nelc.h"
#include "error.h"
#include "wmatch.h"              /* Matches a test string with the mask string ...*/

#define  VERSION         "2.0"
#define  MAX_FILNAM_LEN   512    /* Max. length of directory entries */
#define  BIGSTORE         80
#define  NONE              0     /* Default values for userxxx functions */
#define  REQUEST           1
#define  HIDDEN            2
#define  EXACT             4
#define  YES               1
#define  NO                0
#define  PROCESS_AREA_LEN 54


#define  MYMIN(a,b) ((a) > (b) ? (b) : (a))
#define  MYMAX(a,b) ((a) > (b) ? (a) : (b))


#define  KEY_INSET        tofchar("INSET=")
#define  KEY_OK           tofchar("OK=")


/* Macro to create static storage for Fortran type string */

#define fmake(fchr,size) { \
                            static char buff[size+1]; \
                            int i; \
                            for (i = 0; i < size; buff[i++] = ' '); \
                            buff[i] = 0; \
                            fchr.a = buff; \
                            fchr.l = size; \
                         }



static int compare( char **ps1, char **ps2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for use in 'qsort'.              */
/* This function is used by 'qsort' to sort an array with     */
/* pointers to pointers to characters. The numbers in the     */
/* strings are treated differently so that a10 is positioned  */
/* after a2.                                                  */
/*------------------------------------------------------------*/
{
   char   *s1, *s2;
   int    i, j;                   /* Local counters */
   int    diff;                   /* Difference between two chars */


   s1 = *ps1;                     /* copy pointers to char. arrays */
   s2 = *ps2;
   i = j = 0;
   for (;;)
   {
      if  (isdigit( s1[i] ) && isdigit( s2[j]) )
      {
         char number1[20];                      /* strings containing digits */
         char number2[20];
         int  n1 = 0, n2 = 0;
         while (isdigit(s1[i]))
            number1[n1++] = s1[i++];
         while (isdigit(s2[j]))
            number2[n2++] = s2[j++];
         number1[n1] = '\0';                    /* Terminate string */
         number2[n2] = '\0';
         diff = atoi(number1) - atoi(number2);  /* Convert to integers */
         if (diff != 0)
            return(diff);
      }
      else                                      /* Is end of a string reached? */
      {
         if (s1[i] == '\0' && s2[j] != '\0')
            return(-1);
         if (s1[i] != '\0' && s2[j] == '\0')
            return(1);
         if (s1[i] == '\0' && s2[j] == '\0')
            return(0);
         diff = s1[i++] - s2[j++];
         if (diff != 0)
            return( diff );
      }
   }
}



static void delete_set( fchar Setin )
/*------------------------------------------------------------*/
/* PURPOSE: Delete set, but ask for confirmation first.       */
/*------------------------------------------------------------*/
{
   bool  ok;                  /* Confirm the deleting */
   char  messbuf[BIGSTORE];
   fint  nitems;
   fint  dfault;
   fint  r;
   int   slen = nelc_c(Setin);



   r = 0;
   if ( !(tobool(gds_exist_c(Setin, &r))) )
   {
      fchar  Errstr;
      fmake( Errstr, BIGSTORE );
      if (r < 0)
      {
         gds_errstr_c( Errstr, &r );
         anyoutf( 1, "Set [%.*s] is not a valid GIPSY set! (%.*s)",
                  slen,  Setin.a,
                  nelc_c(Errstr), Errstr.a );
         sprintf( messbuf, "Invalid GIPSY set, delete anyway?   Y/[N]" );
      }
      else
      {
         anyoutf( 1, "Set [%.*s] does not exist!",
                  slen,  Setin.a );
         return;
      }
   }
   else
   /*--------------------------------------------------*/
   /* How many characters are left to display the file */
   /* name in the process area?                        */
   /*--------------------------------------------------*/
   {
      int   messlen;         /* Length of message without set name */

      messlen = strlen("Ok to delete  ?       Y/[N]");
      if (slen+messlen+1 < PROCESS_AREA_LEN)
         sprintf( messbuf, "Ok to delete %.*s ?       Y/[N]",
                  slen, Setin.a );
      else
         sprintf( messbuf, "Ok to delete %.*s...?       Y/[N]",
                  PROCESS_AREA_LEN-messlen-5, Setin.a );
   }

   /* Ask confirmation */

   ok     = toflog( NO );
   dfault = REQUEST;
   nitems = 1;
   r      = userlog_c( &ok, &nitems, &dfault,
                       KEY_OK, tofchar(messbuf) );
   ok = tobool( ok );

   r = 0;
   if (ok)
   {
      gds_delete_c( Setin, &r );
      if (r < 0)
      {
         fchar   Errstr;
         fmake( Errstr, BIGSTORE );
         gds_errstr_c( Errstr, &r );
         anyoutf( 1, "Set [%.*s] could not be deleted! (%.*s)",
                  slen,
                  Setin.a,
                  nelc_c(Errstr),
                  Errstr.a );
      }
   }
   cancel_c( KEY_OK );
}




static int isvalidname( char *name, int len )
/*------------------------------------------------------------*/
/* PURPOSE: A name can represent a GIPSY set if it ends on    */
/*          .descr                                            */
/* The length is also an input variable because we need it    */
/* here, but it is already calculated in the calling          */
/* environment. Return a string without the .descr part.      */
/*------------------------------------------------------------*/
{
   if (len < 7)                 /* String too short to contain .descr */
      return( 0 );
   return( !(strcmp(".descr", &name[len-6])) );
}




MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint    nitems;
   fint    dfault;
   fint    r;
   int     entries = 0;
   bool    next;
   bool    cont;
   bool    list;
   char    **files = NULL;                /* Array of pointers to char. arrays */


   init_c();                              /* contact Hermes */

   /* Task identification */
   {
      fchar  Ftask;                       /* Name of current task */
      fmake( Ftask, 20 );                 /* Macro 'fmake' must be available */
      myname_c( Ftask );                  /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';      /* Terminate task name with null char. */
      IDENTIFICATION( Ftask.a, VERSION ); /* Show task and version */
   }


   /* Does user want a list with GIPSY sets? */
   nitems      = 1;
   list        = toflog( NO );
   dfault      = HIDDEN;
   r           = userlog_c( &list,
                            &nitems, &dfault,
                            tofchar("LIST="),
                            tofchar("List with GIPSY set names?    Y/[N]") );
   list = tobool( list );

   /*--------------------------------------------------*/
   /* In order to do a wildcard match, we need a list  */
   /* of file names first. Read files from current     */
   /* directory and store the GIPSY sets.              */
   /*--------------------------------------------------*/
   {
      fchar      Filename;
      fchar      Dirname;

      fmake( Filename, MAX_FILNAM_LEN );
      fmake( Dirname,  MAX_FILNAM_LEN );

      /* Select a directory; a space selects the current directory */

      Dirname = tofchar( " " );
      entries = 0;                         /* Total number of entries in dir. */
      next    = YES;
      status_c( tofchar("Reading files in directory") );
      while ( next )
      {
         r = flist_c(Dirname, Filename);
         next = (r == 0);
         if (r == -1)
            anyoutf( 1, "Entry trunctated because buffer is not large enough" );
         if (r == -3)
            anyoutf( 1, "Error reading directory" );
         if (r == -4)
            anyoutf( 1, "Cannot obtain current directory" );
         if (next)
         {
            int   len = nelc_c(Filename);
            Filename.a[len] = '\0';
            if ( isvalidname(Filename.a, len) )
            {
               files = realloc( files, (entries+1) * sizeof(char *) );  /* new pointer */
               files[entries] = malloc( len + 1 );
               /* Store without .descr */
               sprintf( files[entries], "%.*s", len - 6, Filename.a );
               entries++;
            }
         }
      }
   }
   if (list)
   /*--------------------------------------------------*/
   /* Do a quicksort, first parameter is name of array.*/
   /* Second is number of elements in that array, third*/
   /* is the size of one element and the last parameter*/
   /* is a function for comparison.                    */
   /*--------------------------------------------------*/
   {
      int  i;
      qsort( files, entries, sizeof(char *), (int(*)())compare );
      for (i = 0; i < entries; i++)
            anyoutf( 1, "%4d: %s", i+1, files[i] );
   }


   /*--------------------------------------------------*/
   /* Loop over sets to delete. End loop after default */
   /*--------------------------------------------------*/
   cont = YES;
   while( cont )
   {
      fchar    Delset;
      int      i;

      fmake( Delset, MAX_FILNAM_LEN );
      dfault = REQUEST;
      nitems = 1;
      r = userchar_c( Delset, &nitems, &dfault,
                      KEY_INSET,
                      tofchar("Give name of set to delete:      [stop]" ) );
      cancel_c( KEY_INSET );
      cont = (r > 0);
      if (cont)
      {
         Delset.a[nelc_c(Delset)] = '\0';
         if ( strchr(Delset.a, '*') != NULL )
         /*--------------------------------------------------*/
         /* The wildcard character is part of the INSET=     */
         /* string. Check against all stored file names. De- */
         /* lete if name is matched. The wildcard character  */
         /* is *.                                            */
         /*--------------------------------------------------*/
         {
            int  matched = 0;
            for (i = 0; i < entries; i++)
            {
               fint  casesensitive = YES;
               if ( tobool(wmatch_c( tofchar(files[i]),
                                     tofchar(Delset.a),
                                     tofchar("*"),
                                     &casesensitive )) )
               {
                  matched++;
                  /* Check file and ask confirmation before deleting */
                  delete_set( tofchar(files[i]) );
               }
            }
            if (!matched)
               anyoutf( 1, "No files found to delete!" );            
         }
         else
         {
            /* Check file and ask confirmation before deleting */
            delete_set( Delset );
         }
      }
   }
   /*----------------------------------------*/
   /* Free the allocated character arrays.   */
   /*----------------------------------------*/
   {
      int i;
      for (i = 0; i < entries; i++)
         free( files[i] );
      free( files );
   }
   
   finis_c();
   return(EXIT_SUCCESS);
}
