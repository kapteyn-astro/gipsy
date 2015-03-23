/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             header.dc1

Program:       HEADER

Purpose:       Display header information

Category:      HEADER, UTILITY

File:          header.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give set (, subsets) to work on:
               Maximum number of subsets is 2048.

** MODE=       Output mode (Formatted/List/Axes/General/Hist/Res)    [F]


** FILENAME=   Name of file for header output:
               Write HEADER output only to a file and do not ask for a 
               printer destination.


** PRINTER=    Select destination of output. If user selects nothing,
               all data is written to the terminal. Else if a number
               is given and a corresponding printer exist, the data
               is directed to the selected printer. If the number does
               not correspond to a printer, a menu with available
               printers and their characteristics is presented.
               To be sure to get this menu, type PRINTER=0 at the
               start of the program.
               If for some reason no printers can be found, an error
               message is given.

** HISTORY=    Print history and comments?                          [Y]/N
               Disable/enable printing history and comments found
               in the header of the set. This keyword overrules the 
               history setting in the MODE= keyword.

** NOREJECT=   Return at keyword rejection in input:     Y/[N]
               Added to be able to avoid rejection for an invalid INSET= 
          

Description:   This program is used to check the contents of the header
               of your data. The header determines how your data is
               organized, and what information it contains.
               In a GDS-data structure, a header item may refer to the
               structure as a whole, or to a specific substructure.
               At the INSET= key, you may thus give a structure name only,
               or a structure name and a substructure specification.
               If you specify the structure name only, you will get the
               items defined at the top level, that is: for the structure
               as a whole. If you specify one or more substructures, you
               will get, for each substructure, the items as they are
               defined at the level of that substructure or at a higher
               level. Searching at a higher level can be prevented by
               specifying MODE=R.

               Using the keyword MODE=, you can determine how the header
               is displayed. 

#begin section mode               
                              OUTPUT MODES
                              ============
               It is possible to combine more modes at the same time by 
               specifying more characters, for example: MODE=AFG


               MODE=A

               The name, range, grid spacing and projection type of
               each axis is shown.


               MODE=F

               If you choose MODE=F (default) you will be given a
               standard, formatted header. For each substructure,
               you will be supplied with:
               
               - A general formatted header in any case, except for
                 an IRDS set (raw IRAS data).
                 (proj. centre, coordinate axes, maximum value etc).
               - If possible, a special radio or IRAS header.
               - For an IRDS set a special header is given. If the SNIP
                 axis is given as the subset axis, additional information
                 on these subsets is given.


               MODE=G

               All available header items are sorted and displayed
               in FITS style i.e. with keyword, contents and comment.
               If no comment was part of the header information,
               comment is searched in a list with general descriptor
               items.


               MODE=H 
               
               Print the HISTORY and COMMENT fields at set/subset level.
               

               MODE=R

               Restrict the information for substructures to the
               given level by ignoring information found at higher
               levels. The headers then will contain only substructure
               information. This output mode is used in combination
               with the modes F, A or G.


               MODE=L

               Generate a list with general, or common, descriptor items. 
               
#end section mode



Notes:         If possible, physical coordinates are converted to
               natural units (example: Mhz in header is converted to
               natural units Hz).

Example:

Updates:       ??? ??,  1990: VOG, Document created.
               Jun 24,  1991, FL,  formatted header for raw IRDS data added,
               Jun 10,  1993, VOG, Radio telescope names added.
               Oct 24,  1994, VOG, HISTORY= keyword implemented
               Sep 25,  1999, VOG, Display entire CTYPE name for 
                                   parameter type axes.
               Jul 07,  2000, VOG, Added FILENAME= functionality.
               Jul 27,  2000, VOG, Replaced deg2hms_dms routine by 
                                   hms, dms routines because of sign problems
                                   for declinations < 0 and > -1.
               Apr 12,  2009, VOG, Changed grid for projection center
                                   (see code near 'at grid: ..') to
                                   make it compatible with other definitions
                                   of macro NINT for coordinates 
                                   (e.g. cotrans.c, reproj.c, slice.c etc.).
               Dec 01,  2011, VOG, Removed bugs with strings that were too short.
               Aug 02,  2012, VOG, Added NOREJECT keyword
               Dec 19,  2012, VOG, Increased value of BIGSTORE (after crash on Macs)
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
#include "gdsinp.h"
#include "gdsc_ndims.h"
#include "myname.h"
#include "anyout.h"
#include "nelc.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsc_name.h"
#include "axtype.h"
#include "axunit.h"
#include "userchar.h"
#include "userint.h"
#include "userreal.h"
#include "userdble.h"
#include "userlog.h"
#include "usertext.h"
#include "userfio.h"
#include "cancel.h"
#include "gdsd_readc.h"
#include "gdsd_writec.h"
#include "gdsd_find.h"
#include "gdsd_rchar.h"
#include "gdsd_wchar.h"
#include "gdsd_rint.h"
#include "gdsd_wint.h"
#include "gdsd_rdble.h"
#include "gdsd_wdble.h"
#include "gdsd_rreal.h"
#include "gdsd_wreal.h"
#include "gdsd_rlog.h"
#include "gdsd_wlog.h"
#include "gdsd_wvar.h"
#include "gdsd_rvar.h"
#include "gdsd_rfits.h"
#include "gdsd_type.h"
#include "gdsd_delete.h"
#include "gdsd_delall.h"
#include "gdsd_length.h"
#include "gdsd_rewind.h"
#include "gdsc_origin.h"
#include "gdsc_word.h"
#include "irds_basic.h"
#include "error.h"
#include "gdsa_istable.h"
#include "cotrans.h"
#include "factor.h"
#include "axunit.h"
#include "prntrnum.h"                             /* Total number of printers */
#include "prntrcom.h"                        /* Find comment for this printer */
#include "prntrdim.h"          /* Find number of columns and rows for printer */
#include "prntrnam.h"                                /* Find name of printers */
#include "prntract.h"                                 /* Send file to printer */
#include "getaxname.h"

#define VERSION     "1.0"
#define AXESMAX       10
#define SUBSMAX     2048
#define NONE           0
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define FITSLEN       18
#define BIGSTORE      256
#define ALLOC_ENTRIES 10

#define FITSITEMS     48    /* If the list with descriptor items is expanded, */
                            /* don't forget to update this number !! */


#define MYMIN(a,b) ((a) > (b) ? (b) : (a))
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )

/* Macro to create static storage for Fortran type string */

#define fmake(fchr,size) { \
                            static char buff[size+1]; \
                            int i; \
                            for (i = 0; i < size; buff[i++] = ' '); \
                            buff[i] = 0; \
                            fchr.a = buff; \
                            fchr.l = size; \
                         }


/* Input of set, subsets: */

static   fchar    Setin;               /* Name of the set to be examined */
static   fint     subin[SUBSMAX];      /* Max. 'subsmax' subsets to be examined */
static   fint     nsubs;               /* Number of input subsets */
static   fint     dfault;              /* Default option for input etc */
static   fchar    Keyword;             /* Keywords for userxxx routines */
static   fchar    Fmessage;            /* Messages for userxxx routines */
static   fint     axnum[AXESMAX];      /* Array with axis numbers */
static   fint     axcount[AXESMAX];    /* Number of pixels on one axis */
static   fint     class = 1;           /* Repeat action for all subsets */
static   fint     subdim, setdim;      /* Dimension of subset, set */
static   fint     scrnum;              /* Destination of log output */
static   fint     maxaxes  = AXESMAX;  /* Maximum number of axes in a set */
static   fint     maxsubs  = SUBSMAX;  /* Maximum number of subsets to work on */
static   fint     setlevel = 0;        /* Indicates level is top level */
static   int      j;                   /* Counter */
static   fint     FgridLO[AXESMAX];
static   fint     FgridHI[AXESMAX];    /* Frame grid vectors */


/* Printer related */

static   char     printfile[BIGSTORE]; /* Name of the file to be printed */
static   fint     printer;             /* Is a printer selected ? */
static   fint     Fprnnum;             /* If selected, what is its number ? */
static   fint     Fcols, Frows;        /* Number of columns and rows */
static   FILE    *fileptr;             /* Pointer to file with data to be printed */
static   FILE    *logptr = NULL;
static   int      tofile = 0;

/* Miscellaneous: */

static   int      toplevel;            /* Operate on top level? */
static   fint     Fres;                /* Fortran integer result of a function */
static   fint     nitems;              /* Max. num. to enter in userxxx */
static   fchar    Fmode;               /* One of Standard/Full/Diff/Axes/History */
static   char     *fitsstorage = NULL; /* Store header item info */
static   int      kind_of_data;        /* Radio- or IRAS data or else */
static   int      restricted;          /* Items at specified level only */



/*------------------------------------------------------------*/
/* In the structure 'descriptor', a description of an item is */
/* given. It contains 4 fields: Name of item, type, allowed   */
/* level and meaning or use. Allowed levels are: 0 (operate   */
/* on toplevel only), 1 (operate on subset level only) or -1  */
/* (operate on toplevel or subset level). The levels are used */
/* to generate warnings and not to forbid certain operations. */
/* The complete definition of this structure is referred to   */
/* as item LIST.                                              */
/*------------------------------------------------------------*/
static struct descriptor
{
        char     *word;                /* Name of the descriptor item */
        char     *type;                /* Format for storage */
        int      level;                /* Level advise */
        char     *meaning;             /* Use or meaning */
} descrip[FITSITEMS] =
{
   {"BUNIT"   , "CHAR" ,-1, "data units   (WU,MJY/SR,...)"},
   {"POL"     , "CHAR" ,-1, "polarization (I,Q,U,V,XX,...)"},
   {"OBSTYP"  , "CHAR" ,-1, "type of observation (LINE,CONT)"},
   {"FSCRA"   , "DBLE" ,-1, "RA fringe stopping center  (degrees)"},
   {"FSCDEC"  , "DBLE" ,-1, "DEC fringe stopping center (degrees)"},
   {"BANDW"   , "DBLE" ,-1, "total bandwidth of observation"},
   {"EPOCH"   , "DBLE" ,-1, "epoch of observation (years)"},
   {"NINTF"   , "INT " ,-1, "number of interferometers used"},
   {"NPOL"    , "INT " ,-1, "number of polarizations used (1,2,4)"},
   {"NFREQ"   , "INT " ,-1, "number of frequency points used"},
   {"REDCODE" , "CHAR" ,-1, "LINEMAP reduction code"},
   {"MAPCODE" , "CHAR" ,-1, "LINEMAP map code"},
   {"UVGRID"  , "CHAR" ,-1, "convolving function code"},
   {"BLGRAD"  , "CHAR" ,-1, "baseline grading function"},
   {"NBLANK"  , "INT " ,-1, "number of undefined values in map"},
   {"INSTRUME", "CHAR" ,-1, "source of data (WSRT,TAURUS,...)"},
   {"MAPVSN"  , "CHAR" ,-1, "tape volume of map archive"},
   {"MAPLAB"  , "CHAR" ,-1, "tape label of map archive"},
   {"APVSN"   , "CHAR" ,-1, "tape volume of Antenna Pattern"},
   {"APLAB"   , "CHAR" ,-1, "tape label of Antenna Pattern"},
   {"PCRA"    , "DBLE" ,-1, "pointing center RA  (degrees)"},
   {"PCDEC"   , "DBLE" ,-1, "pointing center DEC (degrees)"},
   {"MAXBASE" , "DBLE" ,-1, "maximum baseline (meter)"},
   {"MINBASE" , "DBLE" ,-1, "minimum baseline (meter)"},
   {"BMMIN"   , "DBLE" ,-1, "minor axis (FWHM) of beam (arcsec)"},
   {"BMMAJ"   , "DBLE" ,-1, "major axis (FWHM) of beam (arcsec)"},
   {"RESOL"   , "DBLE" ,-1, "resolution of spectral axis"},
   {"EQUINOX" , "DBLE" ,-1, "equinox of coordinate system (years) ?"},
   {"DATE-OBS", "CHAR" ,-1, "observation date (DD/MM/YY)"},
   {"APSET"   , "TEXT" ,-1, "set number of antenna pattern"},
   {"FILEID"  , "CHAR" ,-1, "identification of file"},
   {"FREQ0"   , "DBLE" ,-1, "Rest frequency of spectral line (Hz)"},
   {"UVFREQ"  , "DBLE" ,-1, "reference freq. for UV coords. (MHz)"},
   {"UVBANDW" , "DBLE" ,-1, "bandwidth of UV coordinates (MHz)"},
   {"NOISE"   , "DBLE" ,-1, "noise of map"},
   {"NORM"    , "DBLE" ,-1, "normalizing factor in FFT"},
   {"DATAMAX" , "REAL" ,-1, "maximum value of map"},
   {"DATAMIN" , "REAL" ,-1, "minimum value of map"},
   {"OBSERVER", "CHAR" ,-1, "observer name"},
   {"OBJECT"  , "CHAR" ,-1, "object name"},
   {"TAPER"   , "CHAR" ,-1, "type of frequency taper (HANNING,...)"},
   {"GRIDTYPE", "CHAR" ,-1, "type of grid"},
   {"ORIGIN"  , "CHAR" ,-1, "tape writing institute"},
   {"DATE"    , "CHAR" ,-1, "tape writing date (DD/MM/YY)"},
   {"OBSTIME" , "CHAR" ,-1, "observation time (HH:MM:SS)"},
   {"BMPA"    , "DBLE" ,-1, "pos. angle of major axis of beam (N->E)"},
   {"HISTORY" , "HIST" ,-1, "History records"},
   {"COMMENT" , "HIST" ,-1, "Comment records"}
};



/*  FUNCTIONS */


static void anyoutC( fint printer, char *anystr )
/*------------------------------------------------------------*/
/* The C version of 'anyout' needs a C type string as argu-   */
/* ment only. The value of scrnum is global. If a printer is  */
/* specified (printer > 0), the string is extended with a     */
/* newline and send to the file associated with the global    */
/* file pointer 'fileptr'. If a file was selected, do not     */
/* write to screen or log file but only to the selected file. */
/*------------------------------------------------------------*/
{
   int   pos;
   char  extendstr[512];


   if (!tofile)
      anyout_c( &scrnum, tofchar( anystr ) );

   if ((int) printer) {
      strcpy(extendstr, anystr);
      pos = strlen(anystr);
      extendstr[pos++] = '\n';
      extendstr[pos] = '\0';
      fprintf( fileptr, "%s", extendstr );
   }
   else if (tofile)
   {
      strcpy(extendstr, anystr);
      pos = strlen(anystr);
      extendstr[pos++] = '\n';
      extendstr[pos] = '\0';
      fprintf( logptr, "%s", extendstr );      
   }   
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
   if (prnnum < 0)
   {                                   /* Something went wrong */
      switch((int)prnnum)
      {
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
   for (i = 0, j = 1;  i < prnnum; i++, j++)      /* Get rows and columns */
   {
      Ires1 = prntrdim_c( &j,
                          &prncol[i],
                          &prnrow[i] );
      if (Ires1 != 0) {                 /* Fill with dummy if no columns ... */
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
             strcpy( txt, "No information about this printer" );
          else
             sprintf( txt, " %3d   %-20.20s  %4d  %4d  %-40.*s", j, prnname.a,
                      prncol[i], prnrow[i], nelc_c(prncom), prncom.a );
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
         valid = ( (*printer > 0) &&
                   (*printer <= prnnum) &&
                   (prncol[printerindex] > 0) );
         if (!valid)
         {
            cancel_c( keyword );
            anyout_c( scrnum, tofchar("Selection not possible!") );
            message = tofchar("Try again: Select a printer:");
         }
      } while (!valid);
   } else
      printerindex = *printer - 1;
   *cols = prncol[printerindex];
   *rows = prnrow[printerindex];
   return ( *printer );                /* Return the number of the printer */
}



static char *makedate( char *buffer,
                       int   maxlen )
/*------------------------------------------------------------*/
/* PURPOSE: Return date in format : 29-NOV-1990               */
/*------------------------------------------------------------*/
{
   struct tm   *ptr;   
   time_t      lt;


   lt    = time(NULL);                         /* Get the coded calendar time */
   ptr   = localtime(&lt);
   strftime( buffer, maxlen, "%d-%b-%Y", ptr );
   return( buffer );
}                              
                             


static int compare( char *s1, char *s2 )
/*------------------------------------------------------------*/
/* This routine is an alternative for the function 'strcmp'.  */
/* The difference is that numbers in the strings starting at  */
/* the same position are treated as characters with an integer*/
/* value equal to the integer equivalence of the sub string   */
/* containing the digits.                                     */
/*------------------------------------------------------------*/
{
   int  i, j;            /* Local counters */
   int  n1, n2;
   char number1[20];     /* Strings containing digits */
   char number2[20];
   int  diff;            /* Difference between two chars */


   i = j = 0;
   for (;;)
   {
      if  (isdigit( s1[i] ) && isdigit( s2[j]) )
      {
         n1 = n2 = 0;                     /* Fill sub strings with digits */
         while (isdigit(s1[i]))
            number1[n1++] = s1[i++];
         while (isdigit(s2[j]))
            number2[n2++] = s2[j++];
         number1[n1] = '\0';              /* Terminate string */
         number2[n2] = '\0';
         diff = atoi(number1) - atoi(number2);  /* Convert to integers */
         if (diff != 0)
            return(diff);
      }
      else                               /* Is end of a string reached? */
      {
         if (s1[i] == '\0' && s2[j] != '\0') return(-1);
         if (s1[i] != '\0' && s2[j] == '\0') return(1);
         if (s1[i] == '\0' && s2[j] == '\0') return(0);
         diff = s1[i++] - s2[j++];
         if (diff != 0)
            return( diff );
      }
   }
}



static void menu()
/*------------------------------------------------------------*/
/* Display all available fits keywords from the item LIST.    */
/*------------------------------------------------------------*/
{
   int  i;                          /* Counter */
   char messbuf[BIGSTORE];          /* Storage for string */


   anyoutC( printer,  " " );
   anyoutC( printer,  "**** COMMON DESCRIPTOR ITEMS ****" );
   anyoutC( printer,  " " );

   for (i = 0; i < FITSITEMS; i++)
   {
      sprintf( messbuf,
              "%-15.15s  : %-50s",
               descrip[i].word,
               descrip[i].meaning );
      anyoutC( printer,  messbuf );
   }
   anyoutC( printer,  " " );
}



static void showsubset( fchar Setin, fint *subin, fint *axnum,
                        fint *subdim, fint *setdim, fchar Fshowstr )
/*------------------------------------------------------------*/
/* Create the string 'Fshowstr' containing information about  */
/* the axes. Example: Fshowstr = "(RA,DEC,FREQ) = (*,*,1)"    */
/*------------------------------------------------------------*/
{
   int    n;
   char   axis_b[20+1];
   fchar  Fctype;
   fint   Ferr = 0;
   fint	  Fgrid;
   char   dummystr[BIGSTORE];
   char   leftbuf[BIGSTORE], rightbuf[BIGSTORE];
   fint   chop = 1;

   sprintf( leftbuf,  "%c", '(' );
   sprintf( rightbuf, "%c", '(' );
   for (n = 0; n < *setdim; n++ )
   {
      Fctype.a = axis_b; Fctype.l = 20; axis_b[20] = '\0';
      getaxname_c( Setin, &axnum[n], &chop, Fctype );
      if (( n + 1 ) == *setdim)
      {
         sprintf( dummystr, "%s", Fctype.a );
      }
      else
      {
         /* Comma added */
         sprintf( dummystr, "%s,", Fctype.a );
      }
      sprintf( leftbuf, "%.*s%s", (int)strlen(leftbuf), leftbuf, dummystr );
      if (n >= *subdim)
      {
         Fgrid = gdsc_grid_c( Setin, &axnum[n], subin, &Ferr );
         sprintf( dummystr, "%d", Fgrid );
      }
      else
         sprintf( dummystr, "%c", '*' );
      if (( n + 1 ) == *setdim)
         sprintf( rightbuf, "%.*s%s", (int)strlen(rightbuf), rightbuf, dummystr );
      else
         sprintf( rightbuf, "%.*s%s,", (int)strlen(rightbuf), rightbuf, dummystr );
   }
   sprintf( leftbuf,  "%.*s%c", (int)strlen(leftbuf),  leftbuf,  ')' );
   sprintf( rightbuf, "%.*s%c", (int)strlen(rightbuf), rightbuf, ')' );
   sprintf( Fshowstr.a, "%s=%s", leftbuf, rightbuf );
}



static void showcoord( fchar Setin, fint *subin, fint *axnum,
                       fint *subdim, fint *setdim, fchar Fshowstr )
/*------------------------------------------------------------*/
/* Create the string 'Fshowstr' containing information about  */
/* the axes. Example: Fshowstr = "(*,29.5 DEGREE,200 KM/S)".  */
/* This string can be appended to the string obtained after a */
/* call to 'showsubset". The definitions for BIGSTORE and     */
/* AXESMAX must be available.                                 */
/*------------------------------------------------------------*/
{
   int    n;
   fchar  Fcunit;
   char   cunitbuf[20+1];
   fint   Ferr = 0;
   fint   Fgrid;
   char   dummystr[BIGSTORE];
   char   rightbuf[BIGSTORE];


   /* Coordinate transformation */

   fint     grid2phys;           /* grid coord. -> physical coord. */
   double   coordin[AXESMAX];    /* Grids before transformation */
   double   coordout[AXESMAX];   /* Physical coordinates after transformation */


   sprintf( rightbuf, "%c", '(' );
   for (n = 0; n < *setdim; n++ )
   {
      if (n >= *subdim)
      {
         Fgrid = gdsc_grid_c( Setin, &axnum[n], subin, &Ferr );
         coordin[ (int) axnum[n]-1 ] = (double) Fgrid;
      }
      else
         coordin[ (int) axnum[n]-1 ] = 0.0;
   }
   grid2phys = 1;                             /* grid coord. -> physical coord. */
   Fres = cotrans_c( Setin, subin, coordin, coordout, &grid2phys );
   for (n = 0; n < *setdim; n++ )
   {
      if (n >= *subdim)
      {
         Fcunit.a = cunitbuf; Fcunit.l = 20; cunitbuf[20] = '\0';
         Fres = axunit_c( Setin, &axnum[n], Fcunit );
         sprintf( dummystr, "%.6g %.*s", coordout[ (int) axnum[n]-1 ],
                  (int) nelc_c( Fcunit ), Fcunit.a );
      }
      else
         sprintf( dummystr, "%c", '*' );
      if (( n + 1 ) == *setdim)
         sprintf( rightbuf, "%.*s%s", (int)strlen(rightbuf), rightbuf, dummystr );
      else
         /* Add comma */
         sprintf( rightbuf, "%.*s%s,", (int)strlen(rightbuf), rightbuf, dummystr );
   }
   sprintf( Fshowstr.a, "%s)", rightbuf );
}



static void write_axis_info()
/*------------------------------------------------------------*/
/* Display all available information about the axes in the    */
/* set.                                                       */
/*------------------------------------------------------------*/
{
   #define MAXBUFLEN    80           /* Max defined by factor.c */
   int     m;
   fint    Fr1, Fr2;	                /* Error return codes */
   fint    Faxis;                    /* Index number of axis */
   char    strbuf[BIGSTORE];         /* Text storage */
   fint    Fcwlo, Fcwhi;             /* Coordinate words */
   fint    Ferrlev;                  /* Error level for error routine */
   fchar   Fctype;                   /* Name of an axis */
   fchar   Fcunit;                   /* Natural units of axis */
   fchar   Fdunit;                   /* Secondary units of axis */
   fint    FCconversion, FDconversion;
   fchar   FheaderCunits;            /* Axis units as found in header */
   fchar   FheaderDunits;
   double  Cfact, Dfact;             /* Conversion from header to natural */
   char    ctypebuf[BIGSTORE+1];     /* Text storage for these characteristics */
   char    cunitbuf[BIGSTORE+1];
   char    dunitbuf[BIGSTORE+1];
   fint    skysys, prosys, velsys;   /* Sky-, projection- and velocity system */
   fint    Ftype;                    /* Axis type number */
   char    typetxt[BIGSTORE];        /* Text storage for type etc. */
   char    skytxt[BIGSTORE];
   char    protxt[BIGSTORE];
   char    veltxt[BIGSTORE];
   double  Crpix, Drpix;             /* Origin of an axis */
   double  Cdelt, Ddelt;             /* Spacing */
   double  Crota, Drota;             /* Orientation */
   double  Crval, Drval;             /* Physical values for origin */
   char    itembuf[20];
   
   
   anyoutC( printer, " ");
   sprintf( strbuf, "****** AXIS INFORMATION for set: %.*s",
            (int) nelc_c( Setin ), Setin.a );
   anyoutC( printer, strbuf );


   /* Initialize the strings */

   for (m = 0; m < MAXBUFLEN-1; m++)
   {
      ctypebuf[m] = ' ';
      cunitbuf[m] = ' ';
      dunitbuf[m] = ' ';
   }
   ctypebuf[m]  = cunitbuf[m]  = dunitbuf[m]  = '\0';
   Fctype.a  = ctypebuf;
   Fcunit.a  = cunitbuf;
   Fdunit.a  = dunitbuf;
   Fctype.l  = Fcunit.l = Fdunit.l  = MAXBUFLEN-1;

   fmake( FheaderCunits, MAXBUFLEN-1 );
   fmake( FheaderDunits, MAXBUFLEN-1 );


   /* Get range and grids */

   Fr1 = 0;
   gdsc_range_c( Setin, &setlevel, &Fcwlo, &Fcwhi, &Fr1 );
   if (Fr1 < 0 )
   {
      Ferrlev = 4;
      error_c( &Ferrlev, tofchar("Something wrong with axes") );
   }
   Fr1 = Fr2 = 0;
   for (m = 0; m < setdim; m++)
   {
      Faxis = (fint) m + 1;
      FgridLO[m] = gdsc_grid_c( Setin, &Faxis, &Fcwlo, &Fr1 );
      FgridHI[m] = gdsc_grid_c( Setin, &Faxis, &Fcwhi, &Fr2 );
      if ( (Fr1 < 0) || (Fr2 < 0) )
      {
         Ferrlev = 4;
         error_c( &Ferrlev, tofchar("Something wrong with grids") );
      }
   }


   /* Loop over all axes and construct info text */
   for (m = 0; m < setdim; m++)
   {
      anyoutC( printer,  " " );
      Fr1 = Fr2 = 0;
      Faxis = (fint) (m + 1);               /* Axis num. always start with 1! */
      gdsc_name_c( Fctype, Setin, &Faxis, &Fr1 );
      if (Fr1 < 0 )
      {
         Ferrlev = 4;
         error_c( &Ferrlev, tofchar("Something wrong with getting axis name") );
      }


      /* The function axtype returns the type of axis, the natural */
      /* units, secondary natural units and the projection type. */

      Ftype = axtype_c( Fctype, Fcunit, Fdunit, &skysys, &prosys, &velsys );
      switch( (int) Ftype)
      {
         case 0:
            strcpy( typetxt, "Unknown type of axis" );
            break;
         case 1:
            strcpy( typetxt, "Spatial axis longitude" );
            break;
         case 2:
            strcpy( typetxt, "Spatial axis latitude" );
            break;
         case 3:
            strcpy( typetxt, "Spectral axis frequency" );
            break;
         case 4:
            strcpy( typetxt, "Spectral axis velocity" );
            break;
         case 5:
            strcpy( typetxt, "Spectral axis wavelength" );
            break;
         case 6:
            strcpy( typetxt, "Spectral axis inverse wavelength" );
            break;
         case 7:
            strcpy( typetxt, "Spectral axis log(wavelength)" );
            break;
         case 8:
            strcpy( typetxt, "Time axis" );
            break;
         case 9:
            strcpy( typetxt, "Polarization axis" );
            break;
         case 10:
            strcpy( typetxt, "Parameter axis" );
            break;
      }
      strcpy( skytxt, "*****" );
      strcpy( protxt, "*****" );
      strcpy( veltxt, "*****" );
      if ( (Ftype == 1) || (Ftype == 2) )
      {
         switch( (int) skysys )
         {
            case 1:
               strcpy( skytxt, "equatorial" );
               break;
            case 2:
               strcpy( skytxt, "galactic" );
               break;
            case 3:
               strcpy( skytxt, "ecliptic" );
               break;
            case 4:
               strcpy( skytxt, "supergalactic" );
               break;
         }
         switch( (int) prosys )
         {
            case 1:
               strcpy( protxt, "AITOFF equal area" );
               break;
            case 2:
               strcpy( protxt, "equivalent cylindrical" );
               break;
            case 3:
               strcpy( protxt, "flat" );
               break;
            case 4:
               strcpy( protxt, "gnomonic" );
               break;
            case 5:
               strcpy( protxt, "orthographic" );
               break;
            case 6:
               strcpy( protxt, "rectangular" );
               break;
            case 7:
               strcpy( protxt, "global sinusoidal" );
               break;
            case 8:
               strcpy( protxt, "north celestial pole (WSRT)" );
               break;
            case 9:
               strcpy( protxt, "stereographic" );
               break;
            case 10:
               strcpy( protxt, "Mercator projection" );
               break;
         }
      }
      if (Ftype == 3)
      {
         if (velsys == 1)
            strcpy( veltxt, "optical" );
         if (velsys == 2)
            strcpy( veltxt, "radio" );
      }
      if (Ftype == 4)
         strcpy( veltxt, "radio" );
      sprintf( strbuf,
              "(%d)  Axis: %-.*s  has length %d and a range from %d to %d",
               Faxis,
               (int) nelc_c( Fctype ),
               Fctype.a,
               (int) FgridHI[m] - FgridLO[m] + 1,
               (int) FgridLO[m],
               (int) FgridHI[m] );
      anyoutC( printer,  strbuf  );

      sprintf( strbuf, "      Type of axis: %s", typetxt );
      anyoutC( printer,  strbuf  );

      /* Get axis properties, examine natural and secondary axis */

      sprintf( itembuf, "CRPIX%d", m + 1 );          /* Note capitals in item */
      Fr1 = 0;
      gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Crpix, &Fr1 );
      if (Fr1 >= 0)
      {
         sprintf( strbuf, "      Reference pixel of this axis: %f", Crpix );
         anyoutC( printer,  strbuf );
         Fr2 = 0;
         sprintf( itembuf, "DRPIX%d", m + 1 );
         gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Drpix, &Fr2 );
         if (Fr2 >= 0)
            sprintf( strbuf, "      Reference pixel of secondary axis: %f", Drpix );
      }
      FCconversion = FDconversion = -1;

      sprintf( itembuf, "CRVAL%d", m + 1 );          /* Note capitals in item */
      Fr1 = 0;
      gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Crval, &Fr1 );
      sprintf( itembuf, "CUNIT%d", m + 1 );          /* Note capitals in item */
      if (Fr1 >= 0)
      {
         Fr2 = 0;
         gdsd_rchar_c( Setin, tofchar(itembuf), &setlevel, FheaderCunits, &Fr2 );
         /* If not found or string is empty, write () */
         FCconversion = factor_c( FheaderCunits, Fcunit, &Cfact );
         if (FCconversion == 0)
         {
            /* The conversion was succesful */
            sprintf( strbuf,
                    "      Reference value of this axis: %f %.*s",
                     Crval*Cfact,
                     (int) nelc_c( Fcunit ),
                     Fcunit.a );
         }
         else
         {
            /* No conversion so instead of natural units, take "CUNIT" */
            sprintf( strbuf,
                    "      Reference value of this axis: %f %.*s",
                     Crval,
                     (int) nelc_c( FheaderCunits ),
                     FheaderCunits.a );
         }
         anyoutC( printer,  strbuf );


         /* Secundary axis */

         Fr1 = 0;
         sprintf( itembuf, "DRVAL%d", m + 1 );
         gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Drval, &Fr1 );
         sprintf( itembuf, "DUNIT%d", m + 1 );          /* Note capitals in item */
         if (Fr1 >= 0)
         {
            Fr2 = 0;
            gdsd_rchar_c( Setin, tofchar(itembuf), &setlevel, FheaderDunits, &Fr2 );
            /* If not found or string is empty, write () */
            FDconversion = factor_c( FheaderDunits, Fdunit, &Dfact );
            if (FDconversion == 0)
            {
            /* The conversion was succesful */
               sprintf( strbuf,
                        "      Reference value of secondary axis: %f %.*s",
                        Drval*Dfact,
                        (int) nelc_c( Fdunit ),
      	                Fdunit.a );
      	    }
      	    else
      	    {
      	       sprintf( strbuf,
                        "      Reference value of secondary axis: %f %.*s",
                        Drval,
                        (int) nelc_c( FheaderDunits ),
      	                FheaderDunits.a );
      	    }
            anyoutC( printer,  strbuf );
         }
      }


      sprintf( itembuf, "CDELT%d", m + 1 );
      Fr1 = 0;
      gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Cdelt, &Fr1 );
      if (Fr1 >= 0) {
         if (FCconversion == 0)
         {
            sprintf( strbuf, "      Spacing of this axis: %f %.*s",
                     Cdelt*Cfact,
                     (int) nelc_c( Fcunit ),
                     Fcunit.a );
         }
         else
         {
            sprintf( strbuf, "      Spacing of this axis: %f %.*s",
                     Cdelt,
                     (int) nelc_c( FheaderCunits ),
                     FheaderCunits.a );
         }
         anyoutC( printer,  strbuf );
         Fr2 = 0;
         sprintf( itembuf, "DDELT%d", m + 1 );
         gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Ddelt, &Fr2 );
         if (Fr2 >= 0)
         {
            if (FDconversion == 0)
            {
               sprintf( strbuf, "      Spacing of secondary axis: %f %.*s",
                        Ddelt*Dfact,
                        (int) nelc_c( Fdunit ),
      	                Fdunit.a );
      	    }
      	    else
      	    {
               sprintf( strbuf, "      Spacing of secondary axis: %f %.*s",
                        Ddelt,
                        (int) nelc_c( FheaderDunits ),
                        FheaderDunits.a );
            }
      	    anyoutC( printer,  strbuf );
         }
      }
      if (skytxt[0] != '*')
      {
         sprintf( itembuf, "CROTA%d", m + 1 );          /* Note capitals in item */
         Fr1 = 0;
         gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Crota, &Fr1 );
         if (Fr1 >= 0)
         {
            sprintf( strbuf, "      Orientation of this axis: %f degrees", Crota );
            anyoutC( printer,  strbuf );
            Fr2 = 0;
            sprintf( itembuf, "DROTA%d", m + 1 );
            gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Drota, &Fr2 );
            if (Fr2 >= 0)
            {
               sprintf( strbuf, "      Orientation of secondary axis: %f degrees", Drota );
               anyoutC( printer,  strbuf );
            }
         }
      }


      /* Print sky-, projection- or velocity system, only when available */
      if (skytxt[0] != '*')
      {
         sprintf( strbuf, "      Sky system: %s", skytxt );
         anyoutC( printer,  strbuf  );
      }
      if (protxt[0] != '*')
      {
         sprintf( strbuf, "      Projection system: %s", protxt );
         anyoutC( printer,  strbuf  );
      }
      if (veltxt[0] != '*')
      {
         sprintf( strbuf, "      Velocity system: %s", veltxt );
         anyoutC( printer,  strbuf  );
      }
   }
}



static void general_header( fchar Setin, fint *adnsubs, fint subin[],
                            fint axnum[], fint *adsetdim,
                            fint *adsubdim, bool printgeneral, 
                            bool printhistory )
/*------------------------------------------------------------*/
/* Give sorted list with header items and their values found  */
/* in the descriptor file. The keyword structure 'descrip' is */
/* global!                                                    */
/*------------------------------------------------------------*/
{
   fint    recordnum;               /* Internal record number for gdsd-find */
   fint    level;                   /* Input level */
   fint    Ferr1;                   /* Level return codes (<0 -> error) */
   fchar   Ffitsstr;                /* Receive string with fits data */
   char    fitsstr[BIGSTORE+1];     /* Storage for the fits string */
   fchar   Fdescrname;              /* Name of a descriptor item */
   char    descrbuf[BIGSTORE+1];    /* Associated buffer */
   int     j, m;                    /* Counters */
   char    message[BIGSTORE];       /* Message string for userxxx routines */
   fint    readbytes;               /* For use in gdsd_readc */
   fint    foundbytes;              /*   "  "   "  "    "    */
   fint    start;                   /*   "  "   "  "    "    */
   char    messbuf[BIGSTORE+1];
   char    messbuf2[BIGSTORE+1];
   fchar   Fshowstr;                /* Subset information */
   char    showbuf[BIGSTORE+1];
   int     pointeroffset;           /* Pointer offset in descriptor storage */
   int     entries;                 /* Number of lines to sort */
   int     len;
   int     max_len_without_comment = 34;  /* Has item its own comment? */
   char    tablebuf[BIGSTORE];      /* Buffer for table related text */
   fint    table_status;            /* Is descriptor associated with a table? */
   fint    nsubs;                   /* Non pointer version of number of subsets */
   fint    setdim, subdim;          /* Non pointer versions of the dimensions */


   nsubs  = *adnsubs;                   /* Avoid pointer arithmetic */
   setdim = *adsetdim;
   subdim = *adsubdim;
   for (m = 0; m < BIGSTORE; fitsstr[m++] = ' ')
      ;
   fitsstr[m] = '\0';
   Ffitsstr.a = fitsstr; Ffitsstr.l = BIGSTORE;
   for (m = 0; m < BIGSTORE; descrbuf[m++] = ' ')
      ;
   descrbuf[m] = '\0';
   Fdescrname.a = descrbuf; Fdescrname.l = BIGSTORE;
   for (m = 0; m < BIGSTORE; showbuf[m++] = ' ')
      ;
   showbuf[BIGSTORE] = '\0';
   Fshowstr.a = showbuf; Fshowstr.l = BIGSTORE;

   for (j = 0; j < nsubs; j++ )
   {
     level = subin[j];      
     if (level > 0)
     {
        strcpy( message, "HEADER: " );
        for (m = 0; m < BIGSTORE; showbuf[m++] = ' ');       /* Clear string */
        /* Working on subset level, show the axes */
        showsubset( Setin, &level, axnum,
                    &subdim, &setdim, Fshowstr );
        strncat( message, Fshowstr.a, (int) (int) nelc_c(Fshowstr) );
     } else
        strcpy( message, "HEADER at top level " );

      anyoutC( printer,  " " );
      anyoutC( printer,  message );
      anyoutC( printer,  "===============================" );
     
     if (printgeneral)
     {      

      recordnum = 0;                              /* Position in header file */
      entries = 0;
      do
      /*-------------------------------------------------------------------*/
      /* The main purpose of this routine is to generate all available     */
      /* keywords with the function 'gdsd_find'. There are several         */
      /* possibilities to read an item (gdsd_rfits, gdsd_readc or as       */
      /* variable length record (History type) ). First, if a generated    */
      /* descriptor name is not HISTORY or COMMENT, try to read the        */
      /* header information as a fits item. If the GDS descriptor          */
      /* contains table info, give a message. If necessary and possible    */
      /* try to add comment from a list with keywords. If a keyword        */
      /* string is constructed, store it to be able to sort the keywords.  */
      /*-------------------------------------------------------------------*/
      {
         Ferr1 = 0;
         gdsd_find_c( Fdescrname, Setin, &level, &recordnum, &Ferr1 );
         if ((recordnum != 0) && (Ferr1 < 0))
            anyoutC( printer,  "*** Found entry but no item ***" );
         if ((recordnum != 0) && (Ferr1 == level))
         {
            /* An item is found. Examine its table status */
            table_status = gdsa_istable_c( Fdescrname );
            switch ((int) table_status)
            {
               case 1 : {
	          sprintf( tablebuf, "%.*s contains table header",
	                   (int) nelc_c( Fdescrname), Fdescrname.a );
	          anyoutC( printer,  tablebuf );
               } break;
               case 2: {
                  sprintf( tablebuf, "%.*s contains column header",
	                   (int) nelc_c( Fdescrname), Fdescrname.a );
                  anyoutC( printer,  tablebuf );
               } break;
               case 3: {
                  sprintf( tablebuf, "%.*s contains column data",
	                   (int) nelc_c( Fdescrname), Fdescrname.a );
                  anyoutC( printer,  tablebuf );
               }
            }

            if ( (table_status == 0) &&
                 (
                  !( (strncmp( Fdescrname.a, "HISTORY", 4 ) == 0) ||
                     (strncmp( Fdescrname.a, "COMMENT", 4 ) == 0)
                   )
                 )
               )
            {
               Ferr1 = 0;

               /* Read FITS descriptor item, keyword is included */

               gdsd_rfits_c( Setin, Fdescrname, &level, Ffitsstr, &Ferr1 );
               if (Ferr1 == level)
               {
                  sprintf( messbuf, "%.*s", (int) nelc_c(Ffitsstr), Ffitsstr.a );

                  /* If the string has more than 'max_len_without_comment'  */
                  /* characters, a comment is included and there is no need */
                  /* to search for a comment in the keyword list            */

                  if ( (int) nelc_c( Ffitsstr ) < max_len_without_comment )
                  {
                     len = (int) nelc_c( Fdescrname );
                     for (m = 0; m < FITSITEMS; m++)
                     {
                        if (strncmp( Fdescrname.a, descrip[m].word, len ) == 0) {
                           sprintf( messbuf2,
                                    "%-33s / %-46s",
                                    messbuf,
                                    descrip[m].meaning );                   
                           strcpy(messbuf, messbuf2);
                           break;               /* We can leave this loop now */
                        }
                     }
                  }
               }
               if (Ferr1 < 0)
               {
                  Ferr1 = 0;
                  start = 1;
                  readbytes = BIGSTORE;
                  for (m = 0; m < BIGSTORE; fitsstr[m++] = ' ');

                  /* Item could not be read as a normal FITS item. */
                  /* Read descriptor item, without keyword in return string. */
                  /* Don't include any comment (prevent space problems) */

                  gdsd_readc_c( Setin, Fdescrname, &level, Ffitsstr,
                                &readbytes, &start, &foundbytes, &Ferr1 );
                  if (Ferr1 == level)
                  {
                     sprintf( messbuf, "%-16.16s= %-.*s",
                              Fdescrname.a,
                              (int) nelc_c( Ffitsstr ), Ffitsstr.a );
                  }
               }
              /*
               *-----------------------------------------------------------
               * 'fitsstorage' is a long string consisting of sub strings
               *  where each sub string contains header information.
               *  In fact it is a pointer to a character array and room for
               *  the item names has to be created first.
               *  This is accomplished by a call to 'realloc'. The first
               *  time it creates room for ALLOC_ENTRIES file names.
               *  Every time after it turns out that more strings have
               *  to be stored, a call to 'realloc' creates more space.
               *  It is possible to store and retrieve the names by means
               *  of pointers. The offset is stored in 'pointeroffset'
               *  and is increased by the maximum length of a string.
                *------------------------------------------------------------
               */
               if (!(entries % ALLOC_ENTRIES) )
               {
                  int s;
                  s = ((entries+ALLOC_ENTRIES)/ALLOC_ENTRIES) * ALLOC_ENTRIES;
                  fitsstorage = realloc( fitsstorage, s * BIGSTORE );
               }
               pointeroffset = entries * BIGSTORE;
               messbuf[BIGSTORE-1] = '\0';
               strcpy( fitsstorage + pointeroffset, messbuf );
               entries++;
               for (m = 0; m < BIGSTORE; messbuf[m++] = ' '); /* Clear string */
            }


         }
      } while (recordnum != 0);                          /* No items anymore */
      qsort( fitsstorage, entries, BIGSTORE, (int(*)())compare );
                                          /* Do a quicksort, compare items in */
                                          /* function compare */

      for (m = 0; m < entries; m++)
      {
        pointeroffset = m * BIGSTORE;
        sprintf( messbuf, "%-80s", fitsstorage + pointeroffset );
        anyoutC( printer,  messbuf );
      }

      /* History fields: */
     }
     if (printhistory)
     {
         anyoutC( printer,  " " );
         anyoutC( printer,  "HISTORY/COMMENT" );
         anyoutC( printer,  "===============" );
         for (m = 0; m < 2; m++ )
         {
            int  k;
            if (m == 0)
               strcpy( Fdescrname.a, "HISTORY" );
            if (m == 1)
               strcpy( Fdescrname.a, "COMMENT" );
            Ferr1 = 0;
            gdsd_rewind_c( Setin, Fdescrname, &level, &Ferr1 );
            if (Ferr1 >= 0)
            {
               do
               {
                  Ferr1 = 0;
                  for (k = 0; k < BIGSTORE; fitsstr[k++] = ' ');

                  /* Read variable length record from descriptor item */

                  gdsd_rvar_c( Setin, Fdescrname, &level, Ffitsstr, &Ferr1 );

                  if (Ferr1 >= 0)
                  {
                     char infobuf[250];
                     sprintf( infobuf, "%-16.16s= %-.*s",
                              Fdescrname.a,
                              (int) nelc_c( Ffitsstr ),
                              Ffitsstr.a );
                     anyoutC( printer,  infobuf );
                  }

                  /* Stop reading HIST type records if return level < 0 */
               } while (Ferr1 >= 0);
            }
         }
      } /* End printhistory */
   }
   anyoutC( printer,  " " );
}



void dms( double degrees, char *convstr, int prec )
/*-----------------------------------------------------------------------------
 * Convert degrees to deg/min/sec
 *-----------------------------------------------------------------------------
 */
{
   double    seconds;
   int       Idegs;
   double    min;
   int       Imin;
   int       negative;
   double    power;
   int       seclen = 2 + 1 + prec; /* ss.sss */

   power = pow( 10.0, (double) prec );
   negative = 0;
   if ( degrees < 0 ) {
      negative = 1;
      degrees = fabs(degrees);
   }
   Idegs   = (int) degrees;
   min     = degrees*60.0 - ((double)Idegs)*60.0;
   Imin    = (int) min;
   seconds = min*60.0 - ((double)Imin*60.0 );
   /* Avoid rounding by formatting */
   seconds = (double) ((int) (seconds * power) ) / power;
   if (negative)
   sprintf( convstr, "-%2dd%2dm%*.*fs", Idegs, Imin, seclen, prec, seconds );
   else
   sprintf( convstr,  "%2dd%2dm%*.*fs", Idegs, Imin, seclen, prec, seconds );
}


void hms( double degrees, char *convstr, int prec )
/*-----------------------------------------------------------------------------
 * Convert degrees to hours/min/sec
 *-----------------------------------------------------------------------------
 */
{
   double    seconds;
   double    hours;
   int       Ihours;
   double    min;
   int       Imin;
   double    power;
   int       seclen = 2 + 1 + prec; /* ss.sss */   


   power   = pow( 10.0, (double) prec );
   degrees = fmod( (degrees + 360.0), 360.0 );
   hours   = degrees / 15.0;
   Ihours  = (int) hours;
   min     = hours*60.0 - ((double)Ihours)*60.0;
   Imin    = (int) ( min );
   seconds = min*60.0 - ((double)Imin)*60.0;
   seconds = (double) ((int) (seconds * power) ) / power;
   sprintf( convstr,  "%2dh%2dm%*.*fs", Ihours, Imin, seclen, prec, seconds );
}




static int equalstr( fchar Fstr1, fchar Fstr2 )
/*------------------------------------------------------------*/
/* Compare two fortran strings and return 1 if they are equal */
/* and 0 if they are not.                                     */
/*------------------------------------------------------------*/
{
   int len;
   int i;

   len = MYMIN( (int) nelc_c(Fstr1), (int) nelc_c(Fstr2) );
   for (i = 0; i < len; i++)
      if (Fstr1.a[i] != Fstr2.a[i]) return(0);
   return(1);
}



static int examine_data()
/*------------------------------------------------------------*/
/* Examine header to find out what kind of data the image     */
/* consists of. Return an integer to indicate type:           */
/*                                                            */
/* 0: Unknown type                                            */
/* 1: Radio data                                              */
/* 2: IRAS processed data        NOT IMPLEMENTED              */
/* 3: IRAS rough data            IMPLEMENTED june 19 1991     */
/* 4: Optical data               NOT IMPLEMENTED              */
/*                                                            */
/*------------------------------------------------------------*/
{
   fchar Finstr , Ftelsc;
   fint  Ferr;
   int   m;
   char  instrbuf[19];

   for (m = 0; m < 18; instrbuf[m++] = ' ' );
   instrbuf[m] = '\0';
   Finstr.a = instrbuf; Finstr.l = 18;
   /* Look in header to find instrument item */
   Ferr = 0;
   gdsd_rchar_c( Setin, tofchar("INSTRUME"), &setlevel, Finstr, &Ferr );
   if (Ferr >= 0)
   {
      if ( strstr( Finstr.a, "WSRT")   ||
           strstr( Finstr.a, "VLA" )   ||
           strstr( Finstr.a, "DRAO")   ||
           strstr( Finstr.a, "VLAP" )  ||
           strstr( Finstr.a, "ATCA" )  ||
           strstr( Finstr.a, "PdB" )   ||
           strstr( Finstr.a, "PDB" )   ||
           strstr( Finstr.a, "pdb" )   ||
           strstr( Finstr.a, "NRO" )   ||
           strstr( Finstr.a, "NMA" )   ||
           strstr( Finstr.a, "BIMA" )  ||
           strstr( Finstr.a, "OVRO" )  ||
           strstr( Finstr.a, "FST" ) )  return(1);
   }
   fmake( Ftelsc , 20 );
   Ferr = 0;
   gdsd_rchar_c( Setin, tofchar("TELESCOP"), &setlevel, Ftelsc, &Ferr );
   if( Ferr >= 0 )
   {
      if( strstr( Ftelsc.a , "IRAS") )
      {
         Ferr = 0;
         if (irds_exist_c( Setin , &Ferr ) <= -1)
            return(2);
         else
            return(3);
      }
   }
   return(0);
}



static void common_format( fint level )
/*------------------------------------------------------------*/
/* Create a table with header information common to all kind  */
/* of data.                                                   */
/*------------------------------------------------------------*/
{
   #define MAXBUFLEN    80            /* Defined in a.o. factor.c*/
   char    line1[2*BIGSTORE+1];
   char    line2[2*BIGSTORE+1];
   fchar   Fshowstr;                  /* Subset information */
   char    showbuf[BIGSTORE+1];
   int     len;
   char    strbuf[BIGSTORE+1];
   fchar   Fstr;
   fint    Ferr;
   fint    Fr1, Fr2;	                /* Error return codes */
   char    convstr[20];               /* Convert degrees */
   int     m;
   fchar   Fctype;                    /* Name of an axis */
   fchar   Fcunit;                    /* Natural units of axis */
   fchar   Fdunit;                    /* Secondary units of axis */
   fint    FCconversion, FDconversion;
   fchar   FheaderCunits;             /* Axis units as found in header */
   fchar   FheaderDunits;
   double  Cfact, Dfact;              /* Conversion from header to natural */
   char    ctypebuf[BIGSTORE+1];      /* Text storage for these characteristics */
   char    cunitbuf[BIGSTORE+1];
   char    dunitbuf[BIGSTORE+1];
   fint    skysys, prosys, velsys;    /* Sky-, projection- and velocity system */
   /*fint    Ftype;*/                     /* Axis type number */
   fint    Faxis;
   char    itembuf[20];
   double  Crval, Drval;
   double  Cdelt, Ddelt;
   double  Crpix;
   fint    Fnblank;
   float   datamin, datamax;
   int     conversion;
   fint    Fcwlo, Fcwhi;
   fint    Ferrlev;
   double  dbleval;


   for (m = 0; m < MAXBUFLEN; m++)
   {
      ctypebuf[m] = ' ';
      cunitbuf[m] = ' ';
      dunitbuf[m] = ' ';
   }
   ctypebuf[m]  = cunitbuf[m]  = dunitbuf[m]  = '\0';
   Fctype.a  = ctypebuf;
   Fcunit.a  = cunitbuf;
   Fdunit.a  = dunitbuf;
   Fctype.l  = Fcunit.l = Fdunit.l  = MAXBUFLEN;


   for (m = 0; m < BIGSTORE; showbuf[m++] = ' ');
   showbuf[BIGSTORE] = '\0';
   Fshowstr.a = showbuf; Fshowstr.l = BIGSTORE;

   anyoutC( printer, "********************************************************************************");


   /* Show the level we are working on */

   if (level == 0)
      strcpy( Fshowstr.a, "top level" );
   else
   {
      /* Create string like (RA,DEC,FREQ)=(*,*,1)  */
      showsubset( Setin, &level, axnum,
                  &subdim, &setdim, Fshowstr );
   }
   sprintf( line1, "Set: %-.*s  %-.*s",
            (int) nelc_c(Setin), Setin.a,
            (int) nelc_c(Fshowstr), Fshowstr.a );
   if (level > 0)
   {
      showcoord( Setin, &level, axnum,
                 &subdim, &setdim, Fshowstr );
      sprintf( line1, "%.*s=%.*s",
               (int) nelc_c( tofchar(line1) ), line1,
               (int) nelc_c(Fshowstr), Fshowstr.a );
   }
   len = (int) nelc_c( tofchar(line1) );


   /* If possible add a date on first line */

   if ( (BIGSTORE - len) >= 14 )
   {
      char   timebuf[14];
      /* There is room for the date on the same line */
      sprintf( line1, "%-68.68s %s", line1, makedate(timebuf,12) );
      anyoutC( printer,  line1 );
   }
   else      /* There is no room for the date on this line, so split the info */
   {
      char   timebuf[14];
      sprintf( line2, "%80s", makedate(timebuf,12) );
      anyoutC( printer,  line2 );
      anyoutC( printer,  line1 );
   }
   for (m = 0; m < BIGSTORE; strbuf[m++] = ' ' );
   strbuf[m] = '\0';
   Fstr.a = strbuf; Fstr.l = BIGSTORE;


   /* Create line with observer name, observer date and object name */

   Ferr = 0;
   gdsd_rchar_c( Setin, tofchar("OBSERVER"), &level, Fstr, &Ferr );
   if ((Ferr < 0) || (restricted && (level != Ferr)))
      strcpy( Fstr.a, "****" );

   /*------------------------------------------------------------------------*/
   /* Fill name with dummy name if                                           */
   /*   1) item could not be found.                                          */
   /*   2) item was found at a higher level, but user wants only information */
   /*      at the specified level (MODE=R).                                  */
   /*------------------------------------------------------------------------*/

   sprintf( line1, "Observer: %.*s", (int) nelc_c(Fstr), Fstr.a );
   Ferr = 0;
   gdsd_rchar_c( Setin, tofchar("DATE-OBS"), &level, Fstr, &Ferr );
   if ((Ferr < 0) || (restricted && (level != Ferr)))
      strcpy( Fstr.a, "****" );
   sprintf( line1,
            "%-26.26s Obs. date: %.*s",
            line1, (int) nelc_c(Fstr), Fstr.a );
   Ferr = 0;
   gdsd_rchar_c( Setin, tofchar("OBJECT"), &level, Fstr, &Ferr );
   if ((Ferr < 0) || (restricted && (level != Ferr)))
      strcpy( Fstr.a, "****" );
   sprintf( line1,
            "%-52.52s Object: %.*s",
             line1, (int) nelc_c(Fstr), Fstr.a );
   anyoutC( printer,  line1 );


   /* Create line with Object name, instrument and polarization type */

   Ferr = 0;
   gdsd_rchar_c( Setin, tofchar("OBSTYP"), &level, Fstr, &Ferr );
   if ((Ferr < 0) || (restricted && (level != Ferr)))
      strcpy( Fstr.a, "****" );
   sprintf( line1, "Obs. type: %.*s", (int) nelc_c(Fstr), Fstr.a );
   Ferr = 0;
   gdsd_rchar_c( Setin, tofchar("INSTRUME"), &level, Fstr, &Ferr );
   if ((Ferr < 0) || (restricted && (level != Ferr)))
      strcpy( Fstr.a, "****" );
   sprintf( line1,
            "%-26.26s Instrument: %.*s",
            line1, (int) nelc_c(Fstr), Fstr.a );
   Ferr = 0;
   gdsd_rchar_c( Setin, tofchar("POL"), &level, Fstr, &Ferr );
   if ((Ferr < 0) || (restricted && (level != Ferr)))
      strcpy( Fstr.a, "****" );
   sprintf( line1,
            "%-52.52s Polarization: %.*s",
             line1, (int) nelc_c(Fstr), Fstr.a );
   anyoutC( printer,  line1 );


   /* Header proj. centre and epoch */

   strcpy( line1, "Projection centre:" );
   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("EPOCH"), &setlevel, &dbleval, &Fr1 );
   if ((Fr1 >= 0) || (Fr1 == -46))
      sprintf( line1, "%-26.26s (Epoch: %.1f)", line1, dbleval );
   else
      sprintf( line1, "%-26.26s (Unknown epoch or not relevant)", line1 );
   anyoutC( printer,  line1 );


   /* Projection center */

   fmake( FheaderCunits, MAXBUFLEN );
   fmake( FheaderDunits, MAXBUFLEN );


   for (m = 0; m < (int) setdim; m++ )
   {
      fint   chop = 1;
      Fr1 = 0;
      Faxis = (fint) (m + 1);               /* Axis num. always start with 1 */
      getaxname_c( Setin, &Faxis, &chop, Fctype );
      (void) axtype_c( Fctype, Fcunit, Fdunit, &skysys, &prosys, &velsys );
      sprintf( itembuf, "CRVAL%d", m + 1 );          /* Note capitals in item */
      FCconversion = FDconversion = -1;
      Fr1 = 0;
      gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Crval, &Fr1 );

      sprintf( itembuf, "CUNIT%d", m + 1 );
      if (Fr1 >= 0)
      {
         Fr2 = 0;
         gdsd_rchar_c( Setin, tofchar(itembuf), &setlevel, FheaderCunits, &Fr2 );
         if (equalstr( FheaderCunits, Fcunit ))
            FCconversion = 1;
         else
            FCconversion = factor_c( FheaderCunits, Fcunit, &Cfact );
         conversion = 0;
         if (strstr( Fctype.a, "RA" ) && strstr( FheaderCunits.a, "DEGREE" ))
         {
            /* Convert to h/m/s */
            hms( Crval, convstr, 3 );
            conversion = 1;
         }
         if (strstr( Fctype.a, "DEC" ) && strstr( FheaderCunits.a, "DEGREE" ))
         {
            /* Convert to deg/m/s */
            dms( Crval, convstr, 2 ); 
            conversion = 1;
         }

         if (FCconversion == 0)
         {
            sprintf( line1, "%-16.16s : %14.8f %.*s = %-.2f %.*s",
                     Fctype.a,
                     Crval,
                     (int) nelc_c( FheaderCunits ),
                     FheaderCunits.a,
                     Crval*Cfact,
                     (int) nelc_c( Fcunit ),
                     Fcunit.a );
         }
         else
         {
            sprintf( line1, "%-16.16s : %14.8f %.*s",
                     Fctype.a,
                     Crval,
                     (int) nelc_c( FheaderCunits ),
                     FheaderCunits.a );
         }
         if (conversion)
         {
            sprintf( line1, "%.*s = %s",
                     (int) nelc_c( tofchar(line1) ), line1,
                     convstr );
         }


         Fr1 = 0;
         sprintf( itembuf, "DRVAL%d", m + 1 );
         gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Drval, &Fr1 );

         if (Fr1 >= 0)                             /* Append info second unit */
         {
            sprintf( itembuf, "DUNIT%d", m + 1 );          /* Note capitals in item */
            Fr2 = 0;
            gdsd_rchar_c( Setin, tofchar(itembuf), &setlevel, FheaderDunits, &Fr2 );
            if (equalstr( FheaderDunits, Fdunit ))
               FDconversion = 1;
            else
               FDconversion = factor_c( FheaderDunits, Fdunit, &Dfact );
            if (FDconversion == 0)
            {
               sprintf( line1, "%.*s = %-.2f %.*s = %-.2f %.*s",
                        (int) nelc_c( tofchar(line1) ),
                        line1,
                        Drval,
                        (int) nelc_c( FheaderDunits ),
                        FheaderDunits.a,
                        Drval*Dfact,
                        (int) nelc_c( Fdunit ),
                        Fdunit.a );
            }
            else
            {
               sprintf( line1, "%.*s = %-.2f %.*s",
                        (int) nelc_c( tofchar(line1) ),
                        line1,
                        Drval,
                        (int) nelc_c( FheaderDunits ),
                        FheaderDunits.a );
            }
         }
         Fr1 = 0;
         sprintf( itembuf, "CRPIX%d", m + 1 );
         gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Crpix, &Fr1 );
         if (Fr1 < 0)
            Crpix = 0.0;
         /* Crpix -= (double) ((int) Crpix); */
         Crpix -= NINT(Crpix);   /* Offsets as in cotrans.c */
         sprintf( line1, "%.*s (at grid: %.2f)",
                  (int) nelc_c( tofchar(line1) ),
                  line1,
                  Crpix );
      }
      else
         sprintf( line1, "CUNIT%d cannot be found", m + 1 );
      anyoutC( printer,  line1 );
   } /* For all axes */


   /* Axis range */

   anyoutC( printer,  "Axis length and range:" );
   Fr1 = 0;
   gdsc_range_c( Setin, &setlevel, &Fcwlo, &Fcwhi, &Fr1 );
   if (Fr1 < 0 )
   {
      Ferrlev = 4;
      error_c( &Ferrlev, tofchar("Something wrong with axes") );
   }
   Fr1 = Fr2 = 0;
   for (m = 0; m < setdim; m++)
   {
      Faxis = (fint) m + 1;
      FgridLO[m] = gdsc_grid_c( Setin, &Faxis, &Fcwlo, &Fr1 );
      FgridHI[m] = gdsc_grid_c( Setin, &Faxis, &Fcwhi, &Fr2 );
      if ( (Fr1 < 0) || (Fr2 < 0) )
      {
         Ferrlev = 4;
         error_c( &Ferrlev, tofchar("Something wrong with grids") );
      }
   }
   for (m = 0; m < (int) setdim; m++ )
   {
      fint   chop = 1;
      Fr1 = 0;
      Faxis = (fint) (m + 1);               /* Axis num. always start with 1! */
      getaxname_c( Setin, &Faxis, &chop, Fctype );
      sprintf( line1, "%-16.16s : %7d   [%-d, %-d]",
               Fctype.a,
               (int) (FgridHI[m] - FgridLO[m] + 1),
               (int)  FgridLO[m],
               (int)  FgridHI[m] );
      anyoutC( printer,  line1 );
   }


   /* Grid spacings */

   anyoutC( printer,  "Grid spacing:" );
   for (m = 0; m < (int) setdim; m++ )
   {
      fint   chop = 1;
      Fr1 = 0;
      Faxis = (fint) (m + 1);               /* Axis num. always start with 1! */
      getaxname_c( Setin, &Faxis, &chop, Fctype );
      (void) axtype_c( Fctype, Fcunit, Fdunit, &skysys, &prosys, &velsys );
      sprintf( itembuf, "CDELT%d", m + 1 );          /* Note capitals in item */
      Fr1 = 0;
      gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Cdelt, &Fr1 );
      sprintf( itembuf, "CUNIT%d", m + 1 );          /* Note capitals in item */
      if (Fr1 >= 0)
      {
         Fr2 = 0;
         gdsd_rchar_c( Setin, tofchar(itembuf), &setlevel, FheaderCunits, &Fr2 );
         FCconversion = factor_c( FheaderCunits, Fcunit, &Cfact );
         
         if (FCconversion == 0)
         {
            if ((Cdelt*Cfact) > 0.0)
            {
               sprintf( line1, "%-16.16s : +%f %.*s",
                        Fctype.a,
                        fabs(Cdelt*Cfact),
                        (int) nelc_c( Fcunit ),
                        Fcunit.a );
            }
            else
            {
               sprintf( line1, "%-16.16s : -%f %.*s",
                        Fctype.a,
                        fabs(Cdelt*Cfact),
                        (int) nelc_c( Fcunit ),
                        Fcunit.a );
            }
         }
         else
         {
            if (Cdelt > 0.0)
            {
               sprintf( line1, "%-16.16s : +%f %.*s",
                        Fctype.a,
                        fabs(Cdelt),
                        (int) nelc_c( FheaderCunits ),
                        FheaderDunits.a );
            }
            else
            {
               sprintf( line1, "%-16.16s : -%f %.*s",
                        Fctype.a,
                        fabs(Cdelt),
                        (int) nelc_c( FheaderCunits ),
                        FheaderDunits.a );
            }
         }
         /* Can this be converted to seconds of arc? */
         if (equalstr( FheaderCunits, tofchar("ARCSEC") ) )
            FCconversion = 1;
         else
            FCconversion = factor_c( FheaderCunits, tofchar("ARCSEC"), &Cfact );
         if (FCconversion == 0)
         {
            if ((Cdelt*Cfact) > 0.0)
            {
               sprintf( line1, "%.*s = +%f arcsec",
                        (int) nelc_c( tofchar(line1) ),
                        line1,
                        fabs(Cdelt*Cfact) );
            }
            else
            {
               sprintf( line1, "%.*s = -%f arcsec",
                        (int) nelc_c( tofchar(line1) ),
                        line1,
                        fabs(Cdelt*Cfact) );
            }
         }

         Fr2 = 0;
         sprintf( itembuf, "DDELT%d", m + 1 );
         gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &Ddelt, &Fr2 );
         if (Fr2 >= 0)                             /* Append info second unit */
         {
            sprintf( itembuf, "DUNIT%d", m + 1 );
            Fr2 = 0;
            gdsd_rchar_c( Setin, tofchar(itembuf), &setlevel, FheaderDunits, &Fr2 );
            FDconversion = factor_c( FheaderDunits, Fdunit, &Dfact );
            if (FDconversion == 0)
            {
               sprintf( line1, "%.*s = %f %.*s",
                        (int) nelc_c( tofchar(line1) ),
                        line1,
                        Ddelt*Dfact,
                        (int) nelc_c( Fdunit ),
                        Fdunit.a );
            }
            else
            {
               sprintf( line1, "%.*s = %f %.*s",
                        (int) nelc_c( tofchar(line1) ),
                        line1,
                        Ddelt,
                        (int) nelc_c( FheaderDunits ),
                        FheaderDunits.a );
            }
         }
      }
      else
         sprintf( line1, "CUNIT%d cannot be found", m + 1 );
      anyoutC( printer,  line1 );
   } /* For all axes */


   /* Make info line with data min & max and number of blanks */

   strcpy( line1, "Data range = [ " );
   Fr1 = 0;
   gdsd_rreal_c( Setin, tofchar("DATAMIN"), &level, &datamin, &Fr1 );
   if ((Fr1 < 0) || (restricted && (level != Fr1)))
      strcpy( itembuf, "****" );
   else
      sprintf( itembuf, "%.6g", datamin );
   sprintf( line1, "%.*s%-.*s, ",
            (int) nelc_c( tofchar(line1) ), line1,
            (int) nelc_c( tofchar(itembuf) ), itembuf );
   Fr1 = 0;
   gdsd_rreal_c( Setin, tofchar("DATAMAX"), &level, &datamax, &Fr1 );
   if ((Fr1 < 0) || (restricted && (level != Fr1)))
      strcpy( itembuf, "****" );
   else
      sprintf( itembuf, "%.6g", datamax );
   sprintf( line1, "%.*s%-.*s ] ",
            (int) nelc_c( tofchar(line1) ), line1,
            (int) nelc_c( tofchar(itembuf) ), itembuf );
   Fr1 = 0;
   gdsd_rchar_c( Setin, tofchar("BUNIT"), &level, Fstr, &Fr1 );
   if ((Fr1 < 0) || (restricted && (level != Fr1)))
      strcpy( itembuf, "****" );
   else
      sprintf( itembuf, "%.*s", (int) nelc_c( Fstr ), Fstr.a );
   sprintf( line1, "%.*s (%.*s)",
            (int) nelc_c( tofchar(line1) ), line1,
            (int) nelc_c( tofchar(itembuf) ), itembuf );

   {
   	char infobuf[50];
        Fr1 = 0;
        gdsd_rint_c( Setin, tofchar("NBLANK"), &level, &Fnblank, &Fr1 );
        if (Fr1 < 0)
           strcpy( infobuf, "Number of blanks not in header" );
        else 
           sprintf( infobuf, "     Number of blanks: %-d", (int) Fnblank  );
        sprintf( line1, "%.*s  %.*s",
               (int) nelc_c( tofchar(line1) ), line1,
               (int) nelc_c( tofchar(infobuf) ), infobuf );
        anyoutC( printer,  line1 );
   }
   anyoutC( printer, "********************************************************************************");
   anyoutC( printer, " " );
}



static void radio_format( fint level, bool printhistory )
/*------------------------------------------------------------*/
/* Create a table with radio related header information.      */
/* The variable 'restricted' is global. If an item cannot be  */
/* found at a given level, searching usually continues at a   */
/* higher level, but if restricted != 0 only the items found  */
/* at the specified level are displayed.                      */
/*------------------------------------------------------------*/
{
   double   dbleval;
   fint     Fintval;
   fchar    Fstrval;
   char     strbuf[BIGSTORE+1];
   fint     Fr1;
   char     itembuf[20];
   char     convstr[20];                  /* Convert degrees */
   char     line1[80];
   fchar    Fctype;                       /* Name of an axis */
   char     ctypebuf[BIGSTORE+1];         /* Text storage for ctype */
   int      m;
   int      found;
   fint     Faxis;


   for (m = 0; m < BIGSTORE; ctypebuf[m++] = ' ')
      ;
   ctypebuf[m] = '\0';
   Fctype.a = ctypebuf; Fctype.l = BIGSTORE;


   /* Fringe stopping center */

   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("FSCRA"), &level, &dbleval, &Fr1 );
   if (((Fr1 < 0) && (Fr1 != -46)) || (restricted && (level != Fr1)))
      strcpy(  line1, "Fringe stopping center  RA  = ****" );
   else
   {
      hms( dbleval, convstr, 3 );
      sprintf( line1, "Fringe stopping center  RA  = %8.2f = %s", dbleval, convstr );
   }
   anyoutC( printer,  line1 );
   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("FSCDEC"), &level, &dbleval, &Fr1 );
   if (((Fr1 < 0)  && (Fr1 != -46)) || (restricted && (level != Fr1)))
      strcpy(  line1, "                        DEC = ****" );
   else
   {
      dms( dbleval, convstr, 2 );
      sprintf( line1, "                        DEC = %8.2f = %s",
               dbleval, convstr );
   }
   anyoutC( printer,  line1 );


   /* Pointing center */

   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("PCRA"), &level, &dbleval, &Fr1 );
   if (((Fr1 < 0)  && (Fr1 != -46)) || (restricted && (level != Fr1)))
      strcpy( line1,  "Pointing center         RA  = ****" );
   else
   {
      hms( dbleval, convstr, 3 );
      sprintf( line1, "Pointing center         RA  = %8.2f = %s",
               dbleval, convstr );
   }
   anyoutC( printer,  line1 );

   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("PCDEC"), &level, &dbleval, &Fr1 );
   if (((Fr1 < 0)  && (Fr1 != -46)) || (restricted && (level != Fr1)))
      strcpy(  line1, "                        DEC = ****" );
   else
   {
      dms( dbleval, convstr, 2 );
      sprintf( line1, "                        DEC = %8.2f = %s",
               dbleval, convstr );
   }
   anyoutC( printer,  line1 );
   anyoutC( printer, " ");

   /* Rest frequency and number of interferometers */

   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("FREQ0"), &level, &dbleval, &Fr1 );
   if (((Fr1 < 0) && (Fr1 != -46)) || (restricted && (level != Fr1)))
      strcpy(  line1, "Rest frequency     ****" );
   else
   {
      dbleval = dbleval / 1000000.0;                     /* Convert Hz to MHz */
      sprintf( line1, "Rest frequency     %14.3f MHZ", dbleval );
   }
   Fr1 = 0;
   gdsd_rint_c( Setin, tofchar("NINTF"), &level, &Fintval, &Fr1 );
   if ((Fr1 < 0) || (restricted && (level != Fr1)))
      sprintf( line1, "%-40.40s Interferometers  ****", line1 );
   else
      sprintf( line1, "%-40.40s Interferometers  %d",
               line1, (int) Fintval );
   anyoutC( printer,  line1 );


   /* Total bandwidth (MHZ in header) and number of polarizations */

   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("BANDW"), &level, &dbleval, &Fr1 );
   if (((Fr1 < 0)  && (Fr1 != -46)) || (restricted && (level != Fr1)))
      strcpy(  line1, "Total bandwidth    ****" );
   else
      sprintf( line1, "Total bandwidth    %14.5f MHZ", dbleval );
   Fr1 = 0;
   gdsd_rint_c( Setin, tofchar("NPOL"), &level, &Fintval, &Fr1 );
   if ((Fr1 < 0) || (restricted && (level != Fr1)))
      sprintf( line1, "%-40.40s Polarizations    ****", line1 );
   else
      sprintf( line1, "%-40.40s Polarizations    %d",
               line1, (int) Fintval );
   anyoutC( printer,  line1 );


   /* Channel separation and number of frequency points */

   found = 0;
   for (m = 0; m < (int) setdim; m++ )
   {
      Fr1 = 0;
      Faxis = (fint) (m + 1);               /* Axis num. always start with 1! */
      gdsc_name_c( Fctype, Setin, &Faxis, &Fr1 );
      if (strstr( Fctype.a, "FREQ"))
      {
         found = 1;
         break;
      }
   }
   strcpy(  line1, "Channel separation ****" );
   if (found)
   {
      fint    Fr2;
      fchar   FheaderCunits;
      fmake( FheaderCunits, BIGSTORE );
      sprintf( itembuf, "CDELT%d", (int) Faxis );    /* Note capitals in item */
      Fr1 = 0;
      gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &dbleval, &Fr1 );
      if (Fr1 >= 0)
      {
          sprintf( itembuf, "CUNIT%d", (int) Faxis );
          Fr2 = 0;
          gdsd_rchar_c( Setin, tofchar(itembuf), &setlevel, FheaderCunits, &Fr2 );
          sprintf( line1,
                  "Channel separation %14f %.*s",
                   dbleval,
                   (int) nelc_c( FheaderCunits ),
                   FheaderCunits.a );
      }
   }
   Fr1 = 0;
   gdsd_rint_c( Setin, tofchar("NFREQ"), &level, &Fintval, &Fr1 );
   if ((Fr1 < 0) || (restricted && (level != Fr1)))
      sprintf( line1, "%-40.40s Frequency points ****", line1 );
   else
      sprintf( line1, "%-40.40s Frequency points %d",
               line1, (int) Fintval );
   anyoutC( printer,  line1 );


   /* Center velocity and taper */

   for (m = 0; m < BIGSTORE; strbuf[m++] = ' ');
   strbuf[m] = '\0';
   Fstrval.a = strbuf; Fstrval.l = BIGSTORE;

   strcpy( line1, "Center velocity    ****" );
   if (found)
   {
      fint    Fr2;
      fchar   FheaderDunits;
      fmake( FheaderDunits, BIGSTORE );
      sprintf( itembuf, "DRVAL%d", (int) Faxis );
      Fr1 = 0;
      gdsd_rdble_c( Setin, tofchar(itembuf), &setlevel, &dbleval, &Fr1 );
      if (Fr1 >= 0)
      {
          sprintf( itembuf, "DUNIT%d", (int) Faxis );
          Fr2 = 0;
          gdsd_rchar_c( Setin, tofchar(itembuf), &setlevel, FheaderDunits, &Fr2 );
          sprintf( line1,
                  "Center velocity   %14f %.*s",
                   dbleval,
                   (int) nelc_c( FheaderDunits ),
                   FheaderDunits.a );
      }
   }

   Fr1 = 0;
   gdsd_rchar_c( Setin, tofchar("TAPER"), &level, Fstrval, &Fr1 );
   if ((Fr1 < 0) || (restricted && (level != Fr1)))
      sprintf( line1, "%-40.40s Taper            ****",
               line1 );
   else
      sprintf( line1, "%-40.40s Taper            %.*s",
               line1, (int) nelc_c(Fstrval), Fstrval.a );
   anyoutC( printer,  line1 );
   anyoutC( printer, " ");


   /* Minimum baseline and tape volume */

   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("MINBASE"), &level, &dbleval, &Fr1 );
   if (((Fr1 < 0)  && (Fr1 != -46)) || (restricted && (level != Fr1)))
      strcpy(  line1, "Minimum baseline   ****" );
   else
      sprintf( line1, "Minimum baseline   %-.1f meter", dbleval );
   Fr1 = 0;
   gdsd_rchar_c( Setin, tofchar("MAPVSN"), &level, Fstrval, &Fr1 );
   if ((Fr1 < 0) || (restricted && (level != Fr1)))
      sprintf( line1, "%-40.40s Tape volume      ****",
               line1 );
   else
      sprintf( line1, "%-40.40s Tape volume      %.*s",
               line1, (int) nelc_c(Fstrval), Fstrval.a );
   anyoutC( printer,  line1 );


   /* Maximum baseline and tape label */

   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("MAXBASE"), &level, &dbleval, &Fr1 );
   if (((Fr1 < 0)  && (Fr1 != -46)) || (restricted && (level != Fr1)))
      strcpy(  line1, "Maximum baseline   ****" );
   else
      sprintf( line1, "Maximum baseline   %-.1f meter", dbleval );
   Fr1 = 0;
   gdsd_rchar_c( Setin, tofchar("MAPLAB"), &level, Fstrval, &Fr1 );
   if ((Fr1 < 0) || (restricted && (level != Fr1)))
      sprintf( line1, "%-40.40s Tape label       ****",
               line1 );
   else
      sprintf( line1, "%-40.40s Tape label       %.*s",
               line1, (int) nelc_c(Fstrval), Fstrval.a );
   anyoutC( printer,  line1 );
   anyoutC( printer, " ");


   /* Beam min x maj, position angle */

   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("BMMIN"), &level, &dbleval, &Fr1 );
   if ((Fr1 < 0) || (restricted && (level != Fr1)))
      strcpy(  line1, "Beam (FWHM) = **** x" );
   else
      sprintf( line1, "Beam (FWHM) = %-.2f x", dbleval );
   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("BMMAJ"), &level, &dbleval, &Fr1 );
   if (((Fr1 < 0)  && (Fr1 != -46)) || (restricted && (level != Fr1)))
      sprintf( line1, "%.*s ****",
               (int) nelc_c( tofchar(line1) ), line1 );
   else
      sprintf( line1, "%.*s %-.2f arcsec (min x maj)",
               (int) nelc_c( tofchar(line1) ), line1, dbleval );
   anyoutC( printer,  line1 );

   /* pos. angle of major axis of beam (N->E) */

   Fr1 = 0;
   gdsd_rdble_c( Setin, tofchar("BMPA"), &level, &dbleval, &Fr1 );
   if (((Fr1 < 0)  && (Fr1 != -46)) || (restricted && (level != Fr1)))
      sprintf( line1, "Position angle major axis (N->E): ****" );
   else
      sprintf( line1, "Position angle major axis (N->E): %-.2f degrees",
               dbleval );
   anyoutC( printer,  line1 );


   /* The associated antenna pattern */

   {
      fint   readbytes;        /* Number of bytes to write in writec routine */
      fint   foundbytes;       /* Num. of bytes actually written */
      fint   start;            /* Relative position to start to write */

      Fr1 = 0;
      start = 1;
      readbytes = BIGSTORE;
      gdsd_readc_c( Setin, tofchar("APSET"), &level, Fstrval,
                    &readbytes, &start, &foundbytes, &Fr1 );
      if ((Fr1 < 0) || (restricted && (level != Fr1)))
         sprintf( line1, "Antenna pattern: ****" );
      else
         sprintf( line1, "Antenna pattern: %.*s",
                  (int) nelc_c( Fstrval ), Fstrval.a );
      anyoutC( printer,  line1 );
   }


   /* Write History/Comments */

   if (printhistory)
   {
      anyoutC( printer,  " " );
      anyoutC( printer,  "HISTORY/COMMENT" );
      anyoutC( printer,  "===============" );
      for (m = 0; m < 2; m++ )
      {
         int  k;
         if (m == 0)
            strcpy( itembuf, "HISTORY" );
         if (m == 1)
            strcpy( itembuf, "COMMENT" );
         Fr1 = 0;
         gdsd_rewind_c( Setin, tofchar(itembuf), &level, &Fr1 );
         if (Fr1 >= 0)
         {
            do
            {
               Fr1 = 0;
               for (k = 0; k < BIGSTORE; strbuf[k++] = ' ');

               /* Read variable length record from descriptor item */

               gdsd_rvar_c( Setin, tofchar(itembuf), &level, Fstrval, &Fr1 );

               if ((Fr1 < 0) || (restricted && (level != Fr1)))
                  strcpy( line1, "****" );
               else
               {
                  sprintf( line1, "%.*s",
                           (int) nelc_c( Fstrval ),
                           Fstrval.a );
                  anyoutC( printer,  line1 );
               }

               /* Stop reading HIST type records if return level < 0 */
            } while (Fr1 >= 0);
         }
      }
   }


   anyoutC( printer, "********************************************************************************");

}



static void iras_proc_format( fint subin )
/*------------------------------------------------------------*/
/* Create a table with IRAS processed data related header     */
/* information.                                               */
/*------------------------------------------------------------*/
{
   common_format( subin );
   anyout_c( &scrnum, tofchar("IRAS processed data format") );
}



static void iras_rough_format()
/*------------------------------------------------------------*/
/* Create a table with IRAS unprocessed data related header   */
/* information (Fred Lahuis).                                 */
/*------------------------------------------------------------*/
{
   fint   equatorial = 1 ;
   fint   n , no ;
   fint   no_snip = 0, add ;

   fint   error, status ;
   fint   naxis, axis[4] ;
   fint   snip[250], detno ;
   fint   scancal, scandur, snipcal, snipdur, sop, obs, att ;
   fint   Ferr ;
   fint   cwlo, cwhi, cw ;
   fint   nul = 0, three = 3, four = 4 ;

   float  epoch, theta, psi, psirate ;
   double dble, centre[2], size[2] ;
   char   line[250] , coord_str[20] ;

   fchar  object, instrument, coor, scantype, fstring ;

   fmake( fstring, 20 );
   fmake( object, 20 );
   fmake( instrument, 20 );
   fmake( coor, 20 );
   fmake( scantype, 20 );

   irds_enquire_c(	Setin, object, instrument, &naxis, axis, centre, size,
   			coor, &epoch, &error ) ;

   anyoutC( printer , "" ) ;
   sprintf( line , "" ) ;
   for( n = 0 ; n < 80 ; n++ )sprintf( line , "%s*" , line ) ;
   anyoutC( printer , line ) ;

   sprintf( line , "IRDS set: %-.*s", (int) nelc_c(Setin) , Setin.a ) ;
   Ferr = 0 ; gdsd_rchar_c( Setin , tofchar("OBSERVER") , &setlevel , fstring , &Ferr );
   if( Ferr < 0 )strcpy( fstring.a, "" ) ;
   sprintf( line , "%-26.26sObserver: %.*s" , line , (int) nelc_c(fstring) , fstring.a ) ;
   sprintf( line , "%-52.52sObject: %.*s " , line , (int)nelc_c(object) , object.a ) ;
   anyoutC( printer,  line ) ;

   Ferr = 0 ; gdsc_range_c( Setin , &nul , &cwlo , &cwhi , &Ferr ) ;
         /*    look for SRLON starting at level of lowest coordinate word
            (gdsd_rdble moves upwards until it is found or else
            returns an error). SRLON is only added to the
            keyword list if BPHF is added to the IRDS set.
         */
   Ferr = 0 ; gdsd_rdble_c( Setin, tofchar("SRLON"), &cwlo, &dble, &Ferr ) ;
   if( Ferr >= 0 || Ferr == -46 )sprintf( line, "Boresight added" ) ;
   else sprintf( line, "Boresight not added" ) ;
   sprintf( line, "%-26.26sInstrument: %.*s ", line, nelc_c(instrument), instrument.a ) ;
   Ferr = 0 ; gdsd_rchar_c( Setin, tofchar("BUNIT"), &setlevel, fstring, &Ferr );
   if( Ferr < 0 )strcpy( fstring.a, "" ) ;
   sprintf( line, "%-52.52sUnits: %.*s ", line, nelc_c(fstring), fstring.a ) ;
   anyoutC( printer, line ) ;
   anyoutC( printer, "" ) ;

   anyoutC( printer, "Area parameters:" ) ;
   if( strstr( coor.a, "EQU" ) != NULL )equatorial = 1 ;
   else equatorial = 0 ;
   sprintf( line, "      Coordinate system: %.*s %.1f", nelc_c(coor), coor.a, epoch ) ;
   anyoutC( printer, line ) ;
   if( equatorial ){      /** coordinates in hms **/
      hms( centre[0], coord_str, 3 );
      sprintf( line , "      Centre: RA :%18s (%7.2f deg)", coord_str, centre[0] ) ;
      anyoutC( printer , line ) ;      
      dms( centre[1], coord_str, 2 );
      sprintf( line , "              DEC:%18s (%7.2f deg)", coord_str, centre[1] ) ;
      anyoutC( printer , line ) ;
   }
   else            /** coordinates in dms **/
   {
      hms( centre[0], coord_str, 3 ); 
      sprintf( line, "      Centre: lon:%18s (%7.2f deg)", coord_str, centre[0] ) ;
      anyoutC( printer, line ) ;
      dms( centre[1], coord_str, 2 );
      sprintf( line, "              lat:%18s (%7.2f deg)", coord_str, centre[1] ) ;
      anyoutC( printer, line ) ;
   }
   sprintf( line , "      Size:   %.2f x %.2f degrees", size[0], size[1] ) ;
   anyoutC( printer , line ) ;
   anyoutC( printer , "" ) ;

   anyoutC( printer , "Snip parameters:" ) ;
   sprintf( line , "      %3d samples per tick       (SAMPLE 1 .. %-3d)",
   			axis[0], axis[0] ) ;
   anyoutC( printer , line ) ;
   sprintf( line , "      %3d ticks in longest snip  (TICK   1 .. %-3d)",
   			axis[1], axis[1] ) ;
   anyoutC( printer , line ) ;
   sprintf( line , "      %3d detectors              (SDET   1 .. %-3d)",
   			axis[2], axis[2] ) ;
   anyoutC( printer , line ) ;
   sprintf( line , "      %3d snips in total         (SNIP   1 .. %-3d)",
   			axis[3], axis[3] ) ;
   anyoutC( printer , line ) ;

   sprintf( line , "Detector no's: " ) ;
   for( no = 1 ; no <= axis[2] ; no++ )
   {
      cw = Ferr = 0 ;
      cw = gdsc_word_c( Setin, &three, &no,  &cw, &Ferr ) ;
      Ferr = 0 ;
      gdsd_rint_c( Setin , tofchar("DETNO") , &cw , &detno , &Ferr ) ;
      if( Ferr < 0 )strcpy( fstring.a, " * " ) ;
      sprintf( line , "%s%2d " , line , detno ) ;
      if( nelc_c( tofchar(line) ) > 70 )
      {
         anyoutC( printer , line ) ;
         sprintf( line , "%15s" , "" ) ;
      }
   }
   if( nelc_c( tofchar(line) ) > 15 )
      anyoutC( printer , line ) ;

   anyoutC( printer , "" ) ;
   sprintf( line , "" ) ;
   for( no = 0 ; no < 80 ; no++ )
      sprintf( line , "%s*" , line ) ;
   anyoutC( printer , line ) ;

   no_snip = 0 ;
   for( no = 0 ; no < nsubs ; no++ ){    /** check to see if one of the subsets **/
      Ferr = 0 ;                          /** is a SNIP subset **/
      snip[no_snip] = gdsc_grid_c( Setin , &four , &subin[no] , &Ferr ) ;
	if( !Ferr && snip[no_snip] != 0 ){
      	 add = 1 ;
         for( n = 0 ; n < no_snip ; n++ ){
            if( snip[no_snip] == snip[n] ){
               add = 0 ;
            }
         }
         if( add || no_snip == 0 ) no_snip++ ;
      }
   }
   if( no_snip != 0 ){
	sprintf( line, "" ) ;
	sprintf( line, "%10s%4s%10s%10s%10s%10s%10s",
			"", "SNIP", "SOP", "OBS", "ATT", "SATCAL", "SNIPDUR" ) ;
	anyoutC( printer , line ) ;
	for( no = 0 ; no < no_snip ; no++ ){
		status = 0 ;
		irds_enquire_snip_c( Setin, &snip[no], &sop, &obs,  &att,
				scantype, &scancal, &scandur, &snipcal,
				&snipdur, &psi, &psirate,
				&theta, &status ) ;
		sprintf( line, "%10s%4d%10d%10d%10d%10d%10d",
				"", snip[no], sop, obs, att,
				snipcal+scancal , snipdur ) ;
		anyoutC( printer , line ) ;
	}
     sprintf( line , "" ) ;
     for( n = 0 ; n < 80 ; n++ )sprintf( line , "%s*" , line ) ;
     anyoutC( printer , line ) ;
   }
   anyoutC( printer , "" ) ;
   return ;
}



MAIN_PROGRAM_ENTRY
/*------------------------------------------------------------*/
/* main.                                                      */
/*------------------------------------------------------------*/
{
   bool    printhistory = 1;
   bool    printgeneral = 1;   
   bool    noreject = 0;
   fchar   Filename;

   init_c();                               /* contact Hermes */

   /* Task identification */
   {
      static fchar    Ftask;               /* Name of current task */
      fmake( Ftask, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Ftask );                   /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }

   scrnum = 8;
   fmake( Setin, FILENAME_MAX );           /* As defined in stdio.h */

   /*------------------------------------------------------------*/
   /* Display mode: If user wants the list with keywords only,   */
   /* he selects MODE=L. If he wants this list and wants to      */
   /* continue the program MODE=L has to be specified after the  */
   /* INSET= specification or more than one mode has to be       */
   /* specified.                                                 */
   /*------------------------------------------------------------*/

   dfault = HIDDEN;
   fmake( Fmode, 8 );
   strcpy( Fmode.a, "F" );                    /* Initialize mode to formatted */
   Fres = usertext_c( Fmode, &dfault, tofchar( "MODE=" ),
          tofchar( "Output mode (Formatted/List/Axes/General/Hist/Res) [F]" ) );

          /*------------------------------------------------------------*/
          /* FORMATTED   gives a formatted general- , and if possible,  */
          /*             a specific header.                             */
          /* LIST        Gives a list with all the keywords available   */
          /*             in a keyword list                              */
          /* AXES        Gives detailed information about all set axes  */
          /* GENERAL     Gives the complete header in a unformatted way.*/
          /*             Items are restricted to specified level.       */
          /* RESTRICTED  Give info for specified level only, don't      */
          /*             search at higher levels.                       */
          /*------------------------------------------------------------*/

   /* Uppercase this text */
   {
      int p;
      for (p = 0; p < (int) nelc_c(Fmode); p++)
         Fmode.a[p] = toupper(Fmode.a[p]);
   }



   restricted = 0;                                       /* Globally declared */
   if (strchr( Fmode.a, 'R' ))
      restricted = 1;


   /* User wants to write output to a file */
   fmake( Filename, 256 ); 
   Keyword  = tofchar("FILENAME=");
   Fmessage = tofchar(" ");
   dfault   = HIDDEN;
   nitems   = 1;
   Fres     = usertext_c( Filename, &dfault, Keyword, Fmessage ); 
   if (Fres > 0)
   {
      Filename.a[nelc_c(Filename)] = '\0';
      tofile = 1;
      if ((logptr = fopen( Filename.a, "w") ) == NULL)
      {
         anyoutC( 0, "Cannot open file" );
         tofile = 0;
      }       
   }

   /*------------------------------------------------------------*/
   /* Ask user the destination of the file. Default is the       */
   /* terminal. If the user selected the printer option, a menu  */
   /* is presented and the user can make his choice of printer   */
   /* here. Also the number of columns and rows of the selected  */
   /* printer is returned.                                       */
   /*------------------------------------------------------------*/
   printer = 0;   
   if (!tofile)
   {
      Keyword  = tofchar("PRINTER=");
      Fmessage = tofchar(" ");
      dfault   = HIDDEN;
      nitems   = 1;
      Fres     =  userint_c( &Fprnnum,                    /* Get number of an ...*/
                            &nitems, &dfault,            /* available printer   */
                            Keyword, Fmessage );
      if (Fres != 0)                                     /* A printer is selected */
      {
         scrnum = 11;
         Fprnnum = prnmenu( Keyword, &scrnum, &Fprnnum, &Fcols, &Frows );
         if (Fcols > 0)
         {
            printer = 1;
                        
            tmpnam( printfile );           /* Create unique name for printer file */
            if ((fileptr = fopen( printfile, "w") ) == NULL)
            {
               anyoutC( 0, "Cannot open file" );
               printer = 0;
            }
         }
      }
   }

   if (strchr( Fmode.a, 'L' ) && !strpbrk( Fmode.a, "FAG"))
   {
      /* Special case: Display all keywords from a list */
      /* and stop program */
      menu();
      finis_c();
   }

   dfault   = HIDDEN;
   Keyword  = tofchar("NOREJECT=");
   Fmessage = tofchar("Return at keyword rejection in input:     Y/[N]");
   nitems   = 1;
   (void) userlog_c( &noreject, &nitems, &dfault, Keyword, Fmessage );
   noreject = tobool(noreject);
                                        

   /* Get input set, subsets */

   Keyword  = tofchar("INSET=");
   Fmessage = tofchar("Give set (, subsets) to work on: ");
   dfault   = NONE;
   if (noreject)
   {
   	dfault += 100;
   }
   subdim   = 0;
   scrnum   = 16;                             /* Don't display axis info now */
   nsubs    = gdsinp_c( Setin, subin, &maxsubs, &dfault, Keyword,
                        Fmessage, &scrnum, axnum, axcount, &maxaxes,
                        &class, &subdim );
   if (nsubs == -1)
   {
      finis_c();
      return(0);   	
   }
   setdim   = gdsc_ndims_c( Setin, &setlevel );
   scrnum   = 3;                       /* Direct to screen and log file again */



   /* Specify working modes */

   dfault = HIDDEN;
   Fres   = usertext_c( Fmode, &dfault, tofchar( "MODE=" ), tofchar( " " ) );

   /* Uppercase this text */
   {
      int p;
      for (p = 0; p < (int) nelc_c(Fmode); p++)
         Fmode.a[p] = toupper(Fmode.a[p]);
   }


   if (strchr( Fmode.a, 'L' ) )               /* Give list with keywords */
      menu();

   if (strchr( Fmode.a, 'G' ) )               /* Give list with keywords */
      printgeneral = 1;
   else
      printgeneral = 0;
      
   if (strchr( Fmode.a, 'H' ))
   {
      printhistory = 1;
   }

   /* Overrule the mode with this keyword */
   dfault   = HIDDEN;
   Keyword  = tofchar("HISTORY=");
   Fmessage = tofchar("Print history and comments?     [Y]/N");
   nitems   = 1;
   (void) userlog_c( &printhistory, &nitems, &dfault,
                         Keyword, Fmessage );
   printhistory = tobool( printhistory );


   toplevel = (setdim == subdim);

   if ( strchr( Fmode.a, 'A' ) )
   {
      /* Give list with axis properties. */
      write_axis_info();
   }

   if ( strchr( Fmode.a, 'F') )               /* Give a Formatted header */
   {
      kind_of_data = examine_data();
      if(kind_of_data == 3)
      {
         /* Rough IRAS data: */
         iras_rough_format( subin[j] );
      }
      else
         for (j = 0; j < nsubs; j++ )
         {
            if(kind_of_data == 2)             /* Processed IRAS data: */
               iras_proc_format( subin[j] );
            else
            {
	            common_format( subin[j] );
               if (kind_of_data == 1)         /* It is radio data: */
                  radio_format( subin[j], printhistory );
            }
         }
   }
   if (printgeneral || printhistory )
   {
      /* Give a General header for all subsets */
      general_header( Setin, &nsubs, subin, axnum,
                      &setdim, &subdim, printgeneral, printhistory );
   }
   
   if (printer)
   {
       fint    Fremovefile = 1;               /* If 0: don't remove file else remove */
       fclose( fileptr );
       /* Send file to printer */
       Fres = prntract_c( &Fprnnum, tofchar(printfile), &Fremovefile );
   }
   if (tofile)
   {
      fclose( logptr );
   }
   finis_c();
   return(0);
}
