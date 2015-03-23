/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved

#>             disk.dc1

Program:       DISK

Purpose:       DISK lists the contents of the current directory and
               lists some properties of GIPSY sets.

Category:      UTILITY

File:          disk.c

Author:        M. Vogelaar

Keywords:


** PRINTER=    Give printer number:

               Select destination of output. If user selects nothing,
               all data is written to the terminal. Else if a number
               is given and the corresponding printer exist, the data
               is directed to the selected printer. If the number does
               not correspond to a printer, a menu with available
               printers and their characteristics is presented.
               To be sure to get this menu, type PRINTER=0 at the
               start of the program.
               If for some reason no printers can be found, an error
               message is given.



** ALL=        List ALL directory entries?                        Y/[N]

               (with size in bytes) in the current directory.
               Output of the list is to the screen only.


** DIR=        Give directory:                                [current]

               List contents of this directory.
    

** INSET=      Give set to examine:                     [list all sets]

               If you do NOT want a list of sets, but detailed
               information about a particular set, use the hidden
               keyword INSET=
               It will skip the list of sets. You cannot use the
               wildcard character '*' to select sets.



   INSET=      Give set to examine:                              [stop]

               After DISK listed all GIPSY sets, it is possible
               to get some extra information about a set.
               This keyword is repeated until carriage
               return is pressed. You can use the wildcard character
               '*' to select sets.


Description:   The utility DISK displays a list of the sets in the
               CURRENT directory.
               The size of the files is displayed in kilobytes.
               It is possible to get more detailed information about
               a set with the keyword INSET=
               The complete directory contents (files and sizes) can
               be obtained (ALL=Y), but this extension produces output
               that can be sent to the screen only and not to the
               printer.


Notes:         1) A message is generated if a descriptor is found but
               the file is corrupted, incompatible or another GDS
               error was encountered.
               2) The INSET= info is obtained by deputying HEADER.
               If a printer is selected then also the header info
               will be sent to printer, but on separate pages.


Example:       <USER >disk

Updates:       26 Feb,  1991: VOG, Document created.
               4  Dec,  1991: VOG, Using new gds_exist for corrupted descr.
               26 Apr,  1995: VOG, Print all GDS errors from gds_exist
               19 Jun,  1995: VOG, Changed storage of strings.
               12 Oct,  1995: VOG, Improved 'compare' routine to sort
                                   file names with many digits.
               23 Mar,  2011: JPT, Use 64 bit numbers for file sizes.

#<

*/

/* Include files */

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "ctype.h"
#include "time.h"
#include "gipsyc.h"
#include "cmain.h"
#include "init.h"
#include "finis.h"
#include "gdsinp.h"
#include "gdsc_ndims.h"
#include "myname.h"
#include "anyout.h"
#include "nelc.h"
#include "gdsc_name.h"
#include "fsize.h"
#include "gds_close.h"
#include "gdsc_size.h"
#include "gds_exist.h"
#include "gdsc_grid.h"
#include "gdsc_range.h"
#include "gdsd_rchar.h"
#include "gds_errstr.h"
#include "userfio.h"
#include "userint.h"
#include "usertext.h"
#include "userchar.h"
#include "usercharu.h"
#include "userlog.h"
#include "cancel.h"
#include "flist.h"
#include "deputy.h"
#include "wkey.h"
#include "wmatch.h"
#include "prntrnum.h"          /* Total number of printers */
#include "prntrcom.h"          /* Find comment for this printer */
#include "prntrdim.h"          /* Find number of columns and rows for printer */
#include "prntrnam.h"          /* Find name of printers */
#include "prntract.h"          /* Send file to printer */



/* Definitions */

#define  AXESMAX         10    /* Maximum number of axes in a set */
#define  MAXBUF          20    /* Buffer size for I/O */
#define  VERSION       "2.1"   /* Version number for use in IDENTIFICATION */
#define  NONE             0    /* Default values for userxxx functions */
#define  REQUEST          1
#define  HIDDEN           2
#define  EXACT            4
#define  BORDERLEN       78
#define  FITSLEN         40    /* Max. length of a fits item */
#define  BIGSTORE       256
#define  YES              1
#define  NO               0


#define TSK_HEAD       tofchar("header")

                               /* Macro to create space for Fortran string */
#define fmake(fchr,size) { \
                            static char buff[size+1]; \
                            int i; \
                            for (i = 0; i < size; buff[i++] = ' '); \
                            buff[i] = 0; \
                            fchr.a = buff; \
                            fchr.l = size; \
                         }


                              /* Macro for min., max. of two numbers */

#define MYMAX(a,b) ((a) > (b) ? (a) : (b))
#define MYMIN(a,b) ((a) > (b) ? (b) : (a))


static fint     toplevel = 0;                 /* Coordinate word for entire set */
static char     messbuf[BIGSTORE];            /* A buffer for string actions */




static void to_output( fint  printer,
                       FILE  *fp,
                       char  *str,
                       fint  scrnum )
/*------------------------------------------------------------*/
/* PURPOSE: This routine sends a string without a newline     */
/*          character to the screen (anyout_c).               */
/* If a printer is specified, the string is extended with a   */
/* newline and send to the file associated with the file      */
/* pointer 'fp'.                                              */
/*------------------------------------------------------------*/
{
   if (printer)
      fprintf( fp, "%s\n", str );

   anyout_c( &scrnum, tofchar(str) );
}



static int compare( char **ps1,
                    char **ps2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for use in 'qsort'.              */
/* This function is used by 'qsort' to sort an array with     */
/* pointers to pointers to characters. The numbers in the     */
/* strings are treated differently so that a10 is positioned  */
/* after a2. There can be atmost 8 characters in such a       */
/* number.                                                    */
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
         char number1[10];           /* strings containing digits */
         char number2[10];
         int  n1 = 0, n2 = 0;
         while (isdigit(s1[i]) && n1 < 8)
            number1[n1++] = s1[i++];
         while (isdigit(s2[j]) && n2 < 8)
            number2[n2++] = s2[j++];
         number1[n1] = '\0';                     /* Terminate string */
         number2[n2] = '\0';
         diff = atoi(number1) - atoi(number2);   /* Convert to integers */
         if (diff != 0)
            return( diff );
      }
      else                                       /* Is end of a string reached? */
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



static int prnmenu( fchar  keyword,
                    fint   *scrnum,
                    fint   *printer,
                    fint   *cols,
                    fint   *rows )
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
   fint            i, j;                  /* Counter */
   fint            numitems;
   int             printerindex;


   fmake( message, 80 );
   fmake( prnname, 40 );
   fmake( prncom,  60 );
   prnnum = prntrnum_c();              /* Get number of available printers... */
                                       /* in current system */
   if (prnnum < 0) {                   /* Something went wrong */
      switch((int)prnnum) {
         case -1:
            strcpy( txt, " Cannot obtain hostname" );
            break;
         case -2:
            strcpy( txt,
            " Cannot obtain translation of printer description file!" );
            break;
         case -3:
            strcpy( txt, " Cannot open printer description file!" );
            break;
         case -4:
            strcpy( txt, " Cannot allocate enough space!" );
            break;
         case -5:
            strcpy( txt, " Printer comment exceeds buffer length!" );
            break;
         case -6:
            strcpy( txt, " Printer name exceeds buffer length!" );
            break;
         case -7:
            strcpy( txt, " Printer comment exceeds buffer length!" );
            break;
         case -8:
            strcpy( txt, " Cannot obtain number of columns!" );
            break;
         case -9:
            strcpy( txt, " Cannot obtain number of rows!" );
      }
      anyout_c( scrnum, tofchar(txt) );
      return( 0 );                     /* Abort action, return to ... */
                                       /* ... calling environment */
   }
   for (i = 0, j = 1;  i < prnnum; i++, j++)       /* Get rows and columns */
   {
      Ires1 = prntrdim_c( &j, &prncol[i], &prnrow[i] );
      if (Ires1 != 0)                   /* Fill with dummy if no columns ... */
      {
         prncol[i] = -1;                /* ... and rows could be found */
         prnrow[i] = -1;
      }
   }
   if ((*printer <= 0) || (*printer > prnnum))
   {
      strcpy( txt,
      " ==============================PRINTERS=============================" );
      anyout_c( scrnum, tofchar(txt) );
      sprintf( txt, " %3.3s   %-20.20s  %4.4s  %4.4s  %-40.40s" , "nr",
              "    name", "cols", "rows", "   comment" );
      anyout_c( scrnum, tofchar(txt) );
      strcpy(txt,
      " ===================================================================" );
      anyout_c( scrnum, tofchar(txt) );
      for (i = 0, j = 1; i < prnnum; i++, j++)
      {
          Ires1 = prntrnam_c( &j, prnname );
          Ires2 = prntrcom_c( &j, prncom );
          if (Ires1 != 0) strcpy(prnname.a, "****");  /* No name available */
          if (Ires2 != 0) strcpy(prncom.a,  "****");  /* No comment available */
          if (prncol[i] < 0)
          {
             strcpy( txt, "No information about this printer" );
          }
          else
          {
             sprintf( txt, " %3d   %-20.20s  %4d  %4d  %-40.*s", j, prnname.a,
                      prncol[i], prnrow[i], nelc_c(prncom), prncom.a );
          }
          anyout_c( scrnum, tofchar(txt) );
      }
      strcpy( txt,
      " ===================================================================" );
      anyout_c( scrnum, tofchar(txt) );
      anyout_c( scrnum, tofchar("  ") );
      cancel_c( keyword );
      dfault = NONE;                   /* Change the default */
      message = tofchar("Give number of printer: ");
      do
      {
         numitems = 1;
         Ires1  = userint_c( printer, &numitems, &dfault, keyword, message );
         printerindex = *printer - 1;
         valid = ( (*printer > 0) && (*printer <= prnnum) &&
                   (prncol[printerindex] > 0) );
         if (!valid)
         {
            cancel_c( keyword );
            anyout_c( scrnum, tofchar("Selection not possible!") );
            message = tofchar("Try again: Select a printer:");
         }
      }
      while (!valid);
   }
   else
   {
      printerindex = *printer - 1;
   }
   *cols = prncol[printerindex];
   *rows = prnrow[printerindex];
   return ( *printer );                /* Return the number of the printer */
}



static void axisrange( fchar  messbuf,
                       fchar  axisname,
                       fint   *len,
                       fint   *sizeofax,
                       fint   *numstr )
/*------------------------------------------------------------*/
/* PURPOSE: Construct a line containing axis names and axis   */
/*          lengths like (RA,DEC,FREQ) = (128,128,12)         */
/* The names are associated with 'axisbuf' and the lengths    */
/* with vectbuf. Remember, in 'strncpy' the null char has to  */
/* be copied too. The string axisname is a Fortran type       */
/* string containing 'numstr' names each 'len' characters     */
/* long.                                                      */
/*------------------------------------------------------------*/
{
   int    i;
   int    pos;
   int    offset;

   char   digitstr[20];
   char   vectbuf[80];
   char   axisbuf[80];
   char   axisstr[FITSLEN+1];

   strncpy( axisbuf, "(", 2 );
   strncpy( vectbuf, "(", 2 );
   for(i = 0; i < *numstr; i++)               /* Extract axis name */
   {
      offset = i * (*len);
      strncpy( axisstr, axisname.a+offset, *len-1 );
      axisstr[*len-1] = '\0';
      pos = (int) strcspn( axisstr, "-" );    /* Position of the hyphen */
      pos = MYMIN( pos, (int) strcspn(axisstr, " ") );
      strncat( axisbuf, axisstr, pos );
      sprintf( digitstr, "%-d", sizeofax[i] );/* Left aligned axis length ... */
                                              /* ... integer value */
      strncat(vectbuf, digitstr, strlen(digitstr) );
      if (i < (*numstr-1))
      {
        strncat( axisbuf, ",", 1 );           /* More? then add comma */
        strncat( vectbuf, ",", 1 );
      }
   }
   strncat( axisbuf, ") = ", 4 );             /* Complete axis name string */
   strncat( vectbuf, ")", 1 );                /* Complete axis length string */
   strncpy( messbuf.a, axisbuf, strlen(axisbuf) + 1 );   /* Concatenate the two */
   strncat( messbuf.a, vectbuf, strlen(vectbuf) );       /* strings to one, */
                                                         /* put result is in */
                                                         /* messbuf */
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



#ifdef maybelater
static int toblocks( int bytes )
/*------------------------------------------------------------*/
/* PURPOSE: Convert bytes to system dependent blocks. The size*/
/*          of one block is defined in stdio.h (BUFSIZ)       */
/*------------------------------------------------------------*/
{
   return( (bytes+(BUFSIZ-1)) / BUFSIZ );
}
#endif



static int tokilobyte( fint8 bytes )
/*------------------------------------------------------------*/
/* PURPOSE: Convert bytes kb.                                 */
/*------------------------------------------------------------*/
{
   return( (bytes+1023) / 1024 );
}



static fint8 getfilesize( char *filename,
                        char *extension )
/*------------------------------------------------------------*/
/* PURPOSE: Return file size in bytes of file                 */
/*          'filename.extension'                              */
/*------------------------------------------------------------*/
{
   fint8   result;
   char  newname[FILENAME_MAX];

   strcpy( newname, filename );
   strcat( newname, extension );
   result = fsize_c( tofchar(newname) );
   if (result < 0)
   {
      if (result == -1)
         anyoutf( 16, "error in opening file [%s]", newname );
      else if (result == -2)
         anyoutf( 16, "error in reporting current file size of [%s]", newname );
      else if (result == -3)
         anyoutf( 16, "filename too long" );
   }
   return( result );
}



static void printstatus( fchar Tsk,
                         fint  status )
/*------------------------------------------------------------*/
/* PURPOSE: Display error for deputy call.                    */
/*------------------------------------------------------------*/
{
   if (status == -6)
      anyoutf( 1, "Called task (%.*s) not present", nelc_c(Tsk), Tsk.a );
   if (status == -7)
      anyoutf( 1, "Max. number of tasks already active" );
}




static void displayinfo( char *setname )
/*------------------------------------------------------------*/
/* PURPOSE: Display info about this set by calling task       */
/*          HEADER                                            */
/*------------------------------------------------------------*/
{
   fint   r;
   char   messbuf[80];

   r = 0;
   if (!tobool(gds_exist_c( tofchar(setname), &r )))
   {
      sprintf( messbuf,
               "Set %s does not exist or is invalid!",
               setname );
   }
   else
   {
      fint   status;
      sprintf( messbuf, "INSET=%s", setname );
      wkey_c( tofchar(messbuf) );
      wkey_c( tofchar("HISTORY=N") );
      deputy_c( TSK_HEAD, &status );            /* Spawn HEADER task */
      if (status != 1)                          /* Something wrong? */
         printstatus( TSK_HEAD, status );
   }
}


static void catnames( fchar Dirname,
                      fchar Filename,
                      fchar Longname,
                      fint  directory )
/*------------------------------------------------------------*/
/* PURPOSE: Concatenate directory and filenae.                */
/*------------------------------------------------------------*/
{
   int l1 = nelc_c(Filename);
   int l2 = nelc_c(Dirname);
   
   if (l1+l2 > FILENAME_MAX)
   {
      anyoutf( 1, "File and directory name too long" );
      return;
   }
   if (!directory)
      sprintf( Longname.a, "%.*s", nelc_c(Filename), Filename.a );
   else
      sprintf( Longname.a, "%.*s/%.*s", 
               nelc_c(Dirname), Dirname.a,
               nelc_c(Filename), Filename.a );
               
   anyoutf(1, "[%.*s]", nelc_c(Longname), Longname.a );
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fchar   dummystr;
   fchar   keyword, message;               /* For userxxx functions */
   fchar   Setin;                          /* Name of set to be examined */
   fchar   Strbuf;			   /* Fortran strings */
   fchar   axname;                         /* Name of one axis */
   fchar   axisname;                       /* Storage for axis names */
   fchar   Dirname;   
   fchar   Longname;
   int     onlysetinfo;
   fint8   totsizes = 0;
   int     quit;
   int     entries = 0;                    /* Tot. num. of entries found in dir. */
   int     m, j;                           /* Counters */
   fint    res;                            /* Fortran int. res. of funct. call */
   fint    dfault;                         /* Default option for input etc */
   fint    numitems;                       /* Max. num. to enter in userxxx */
   fint    setdim;                         /* Dimension of the set */
   fint    scrnum;                         /* Destination of log output */
   fint    axnum;                          /* Number of axis, start with 1! */
   fint    dir;
   char    **files = NULL;                 /* Array of pointers to char. arrays */
   char    printfile[FILENAME_MAX];      /* Name of the file to be printed */

   /* Printer related */

   fint    printer;                        /* Is a printer selected ? */
   fint    prnnum;                         /* If selected, what is its number ? */
   fint    cols, rows;                     /* Number of columns and rows */
   FILE    *fileptr = NULL;                /* Pointer to file with data to be printed */



   init_c();                               /* Contact Hermes */
   /* Task identification */
   {
      fchar      Ftask;                    /* Name of current task */
      fmake( Ftask, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Ftask );                   /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }

   /* Advertisements */
   anyoutf( 8, "Use DISK, PRINTER=0 for list of printers!" );

   /*--------------------------------------------------*/
   /* Ask user the destination of the file. Default is */
   /* the terminal. If the user selected the printer   */
   /* option, a menu is presented and the user can make*/
   /* his choice of printer here. Also the number of   */
   /* columns and rows of the selected printer is      */
   /* returned.                                        */
   /*--------------------------------------------------*/
   printer  = 0;
   keyword  = tofchar("PRINTER=");
   message  = tofchar("Give printer number: ");
   dfault   = HIDDEN;
   numitems = 1;
   res      = userint_c( &prnnum,         /* Get number of an available printer */
                         &numitems,
                         &dfault,
                         keyword,
                         message );
   if (res != 0)                          /* A printer is selected */
   {
      scrnum = 8;
      prnnum = prnmenu( keyword,
                        &scrnum,
                        &prnnum,
                        &cols,
                        &rows );
      if (cols > 0)
      {
      	 printer = 1;
      	 tmpnam( printfile );              /* Create unique name for printer file */
      	 if ((fileptr = fopen( printfile, "w") ) == NULL)
      	 {
      	    anyoutf( 8, "Cannot open tmp file %s", printfile );
      	    printer = 0;
      	 }
      }
   }

   fmake( Longname,  FILENAME_MAX );
   fmake( Dirname,  FILENAME_MAX ); 
   keyword = tofchar("DIR=");
   message = tofchar("Give directory:          [current]" );
   str2char( " ", Dirname );
   numitems  = 1;
   dfault    = HIDDEN;
   dir = userchar_c( Dirname, &numitems, &dfault, keyword, message );

   /*--------------------------------------------------*/
   /* If user specified a set at the same time he      */
   /* specified the name of this program, he only wants*/
   /* info for this set ('onlysetinfo').               */
   /*--------------------------------------------------*/
   onlysetinfo = NO;
   scrnum  = 3;
   keyword = tofchar("INSET=");
   message = tofchar("Give set to examine:  [display set list]" );
   dfault  = HIDDEN;
   fmake( dummystr, FILENAME_MAX );
   res     = usertext_c( dummystr, &dfault, keyword, message );
   if (res != 0 )
   {
      onlysetinfo = YES;
      fmake( Setin,     FILENAME_MAX );
      fmake( axname,    FITSLEN );
      fmake( axisname, (AXESMAX*FITSLEN) );
      fmake( Strbuf,    FILENAME_MAX );
   }
   else
   /*--------------------------------------------------*/
   /* Read filenames from current directory.           */
   /*--------------------------------------------------*/
   {
      bool   next;
      bool   exist;
      bool   listall;
      fint8  totdescr = 0;                 /* Counter for the descriptor sizes */
      fint8  totimage = 0;                 /* Counter for the image sizes */
      char   borderstr[BORDERLEN+1];       /* Create a border for an Ascii-table */
      int    diskspaceproblem = NO;

      onlysetinfo = NO;
      keyword  = tofchar("ALL=");
      message  = tofchar("List ALL directory entries?     Y/[N]");
      dfault   = HIDDEN;
      numitems = 1;
      listall  = toflog( NO );
      res      = userlog_c( &listall,
                            &numitems,
                            &dfault,
                            keyword,
                            message );
      listall  = tobool( listall );

      if (listall)
      {
         fchar      Filename;


         fmake( Filename, FILENAME_MAX );
         while ( flist_c(Dirname, Filename) == 0 )
         {
            int    fsize;
            catnames(  Dirname, Filename, Longname, dir );
            fsize = fsize_c( Longname );
            anyoutf( 3, "%.*s (%d)",
                     nelc_c( Filename ),
                     Filename.a,
                     fsize );
            totsizes += fsize;
         }
         anyoutf( 3, "Disk space all files in this directory  : %7d kb",
                  tokilobyte(totsizes) );
      }

      /*--------------------------------------------------*/
      /* We want a sorted list of the sets on disk. There-*/
      /* fore we need to store the file names. For each   */
      /* file name, create a pointer to a string and      */
      /* create space for a string.                       */
      /*--------------------------------------------------*/
      {
         fchar      Filename;

         fmake( Filename, FILENAME_MAX );

         /* Select a directory; a space selects the current directory */

         entries = 0;                         /* Total number of entries in dir. */
         next    = YES;
         while ( next )
         {
            fint  r = flist_c(Dirname, Filename);
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
      /* Do a quicksort with the pointers to the strings */
      qsort( files, entries, sizeof(char *), (int(*)())compare );


      memset( borderstr, '-', BORDERLEN );
      borderstr[BORDERLEN] = '\0';
      to_output( printer, fileptr, "  ", scrnum );          /* Skip a line */
      sprintf( messbuf, "%45.45s",  "***DISK***" );
      to_output( printer, fileptr, messbuf, scrnum);
      sprintf( messbuf, "%s", borderstr );
      to_output( printer, fileptr, messbuf, scrnum );
      sprintf( messbuf, "%-23.23s %7.7s %7.7s   %s",        /* Show a header */
               "  nr     SETNAME", "descr" , "image",
               "Axis name(s) and size(s)" );
      to_output( printer, fileptr, messbuf, scrnum );
      sprintf( messbuf, "%-23.23s %7.7s %7.7s   %s",        /* Show a header */
             "  ", " (kb)" , " (kb)", " " );
      to_output( printer, fileptr, messbuf, scrnum );
      to_output( printer, fileptr, borderstr, scrnum );


      fmake( Setin,    FILENAME_MAX );
      fmake( axname,   FITSLEN );
      fmake( axisname, (AXESMAX*FITSLEN) );
      fmake( Strbuf,   FILENAME_MAX );
      totsizes = 0;

      /* Write all selected set names and their sizes */
      for (j = 0; j < entries; j++)
      {
         char   filebuf[FILENAME_MAX];
         char   descrbuf[10];
         char   imagebuf[10];
         char   longfilename[FILENAME_MAX];
         fint8  descrsize;
         fint8  imagesize;         
         fint   r;



         if (dir)
            sprintf( longfilename, "%.*s/%s", 
                     nelc_c(Dirname), Dirname.a,
                     files[j] );
         else
            sprintf( longfilename, "%s", files[j] );
            
         descrsize = getfilesize( longfilename, ".descr" );
         imagesize = getfilesize( longfilename, ".image" );

         if (strlen( files[j] ) > 17)
         {
            /* Display entire file name on a separate line */
            sprintf( filebuf, "%4d  %.*s", (j + 1), BIGSTORE-1, files[j] );
            to_output( printer, fileptr, filebuf, scrnum );
            sprintf( filebuf, "%23.23s", " " );            /* Reset file name */
         }
         else
            sprintf( filebuf, "%4d  %-17.17s",      /* Write name of set */
                     (j + 1), files[j] );

         if (descrsize >= 0)
         {
            sprintf( descrbuf, "%7d", tokilobyte(descrsize) );
            totdescr += descrsize;
         }
         else
            strcpy( descrbuf, " *" );

         if (imagesize >= 0)
         {
            sprintf( imagebuf, "%7d", tokilobyte(imagesize) );
            totimage += imagesize;
         }
         else
            strcpy( imagebuf, " *" );

         r = 0;
         exist = tobool( gds_exist_c( tofchar(longfilename), &r) );
         if (!exist)
         {
            fchar  Errstr;
            fmake( Errstr, BIGSTORE );
            gds_errstr_c( Errstr, &r );
            sprintf( messbuf, "%23s %7s %7s   Invalid:(%.*s)",
                     filebuf,
                     descrbuf,
                     imagebuf,
                     nelc_c(Errstr), Errstr.a );
            to_output( printer, fileptr, messbuf, 3 );
            r = -56;
            if (r == -56)
               diskspaceproblem = YES;
         }
         else
         /*-------------------------------------------------------------------*/
         /* Name and size are written. Append the names of the axes and their */
         /* sizes.                                                            */
         /*-------------------------------------------------------------------*/
         {
            fint     r1, r2;                       /* Error return values */
            fint     fitslen = FITSLEN;            /* Length of fits item */
            fint     sizeofax[AXESMAX];            /* Array with the size of all axes */

            Setin   = tofchar(longfilename);
            setdim  = gdsc_ndims_c( Setin, &toplevel );
            for (m = 0; m < FITSLEN*AXESMAX; axisname.a[m++] = ' ');
            axisname.a[m] = '\0';
            for (m = 0; m < setdim; m++)
            {
               r1 = r2 = 0;
               axnum = (fint) (m + 1);          /* Axis num. always start with 1! */
               gdsc_name_c( axname, Setin, &axnum, &r1 );
               sizeofax[m] = gdsc_size_c( Setin, &axnum, &r2 );
               strncpy( axisname.a+m*FITSLEN, axname.a, nelc_c(axname) );
            }
            axisrange( Strbuf,
                       axisname,                /* Create string with names+lengths */
                       &fitslen,
                       sizeofax,
                       &setdim );

            sprintf( messbuf, "%23s %7s %7s   %.*s",
                     filebuf,
                     descrbuf,
                     imagebuf,
                     nelc_c(Strbuf), Strbuf.a );
            to_output( printer, fileptr, messbuf, scrnum );
            r1 = 0;
            gds_close_c( Setin, &r1 );          /* Close the file */
         }
      }

      to_output( printer, fileptr, borderstr, scrnum );
      
      sprintf(messbuf, "Descriptors: %7d kb", tokilobyte(totdescr) );
      to_output( printer, fileptr, messbuf, scrnum );
      sprintf(messbuf, "Images     : %7d kb", tokilobyte(totimage) );
      to_output( printer, fileptr, messbuf, scrnum );
      to_output( printer, fileptr, " ", scrnum );
      
      if (diskspaceproblem)
      {
         to_output( printer, fileptr, "   *** Probably exceeding disk space in your home directory!", scrnum );
         to_output( printer, fileptr, "   *** Please rerun program after getting more disk space.", scrnum );
         to_output( printer, fileptr, "   *** If you still have problems then, please remove", scrnum );                  
         to_output( printer, fileptr, "   *** the file(s) .gds_socketsXXXX_XXXX in your home directory", scrnum );         
      }
   }

   quit = NO;
   while (!quit)
   {
      fchar  Setin;                              /* Local variable for set */
      fmake( Setin, FILENAME_MAX );
      keyword = tofchar("INSET=");
      message = tofchar("Give set to examine:      [stop]" );
      dfault  = REQUEST;
      res     = usertext_c( Setin, &dfault, keyword, message );
      if (res ==  0)
      {
         if (onlysetinfo)
            onlysetinfo = NO;
         else
            quit = YES;                           /* Quit the info loop */
      }
      else
      {
         Setin.a[nelc_c(Setin)] = '\0';
         if ( strchr(Setin.a, '*') != NULL )
         /*--------------------------------------------------*/
         /* The wildcard character is part of the INSET=     */
         /* string. Check against all stored file names.     */
         /* Display info if name is matched. The wildcard    */
         /* character is '*'.                                */
         /*--------------------------------------------------*/
         {
            int i;
            int  matched = 0;
            for (i = 0; i < entries; i++)
            {
               fint  casesensitive = YES;
               char  longfilename[FILENAME_MAX];

               if (dir)
                  sprintf( longfilename, "%.*s/%s", 
                            nelc_c(Dirname), Dirname.a,
                            files[i] );
               else
                  strcpy( longfilename, files[i] );
               
               if ( tobool(wmatch_c( tofchar(files[i]),
                                     tofchar(Setin.a),
                                     tofchar("*"),
                                     &casesensitive )) )
               {
                  matched++;
                  /* Check file and display info */
                  displayinfo( longfilename );
               }
            }
            if (!matched)
               anyoutf( 1, "No files found to list" );
         }
         else
         {
            char  longfilename[FILENAME_MAX];
            if (dir)
               sprintf( longfilename, "%.*s/%.*s",
                        nelc_c(Dirname), Dirname.a,
                        nelc_c(Setin), Setin.a );
            else
               sprintf( longfilename, "%.*s", nelc_c(Setin), Setin.a );
            displayinfo( longfilename );
         }
         cancel_c( keyword );
      }
   }
   if (printer)
   {
      fint    removefile = 1;          /* If 0: don't remove file else remove */
      fclose( fileptr );
      /* Send file to printer */
      res = prntract_c( &prnnum, tofchar(printfile), &removefile );
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

   finis_c();                                                  /* Quit Hermes */
   return(EXIT_SUCCESS);
}
