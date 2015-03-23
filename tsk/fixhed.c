/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             fixhed.dc1

Program:       FIXHED

Purpose:       Add, change, delete or list item(s) in set header

Category:      HEADER, UTILITY

File:          fixhed.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give set (, subsets) to work on:

               Maximum number of subsets is 2048.
               Usually descriptors are added or changed at top level,
               but if you work on subset level, it will be possible
               to work on all given subsets at the same time.


   ITEM=       Give name of header item:                  [show header]
               or (after the first prompt):
               Give name of header item:                  [end program]

               This keyword is asked in a loop. You end the
               program if you press carriage return.
               Input is a name associated with a keyword in the header.
               After the first input, the prompt will also show
               the mode that you are currently working in (see MODE=).
               The length of the string that you enter may not exceed
               8 characters.


               There are two special entries for ITEM=

   ITEM=LIST   If you enter ITEM=LIST, a list with recommended GIPSY
               keywords with explanation is displayed.
   ITEM=HEAD   If you specify ITEM=HEAD, the sorted contents of the
               header at top- or subset level is displayed.


** MODE=       Operation mode: A(dd),D(elete),C(hange):             [A]

               You need to specify only one character.
               FIXHED can Add, Change and Delete header items.
               If ITEM= is found in the header of INSET= then the
               default is A(dd).
               If ITEM= is NOT found in the header of INSET= then the
               the default is C(hange).
               If there is some kind of conflict (for example changing
               an item that isn't present in the header) then the
               keyword is asked UN-hidden.
               The mode of operation can be changed within the ITEM=
               loop (e.g. ITEM=maxbase MODE=d).


** TYPE=       Give type of item I/R/D/L/C/T/H:     [back to item loop]

               The type of keyword can be one of:
               I     -Integer numbers 
               R     -Real numbers
               D     -Double precision numbers
               L     -Logical variables as T or F in header,
                      but input can be YES, JA and TRUE,  NO, NEE and
                      FALSE, or any abbreviations.
               C     -Character variable. A string that can contain
                      spaces. Quotes are added by the program. The
                      string cannot exceed 18 characters              
               H     -Commentary FITS keywords COMMENT or HISTORY.
                      Keyword and contents cannot exceed 80 characters.
               T     -Text entry NOT FITS. Only used in special cases!
                      (See description)
                             
               The keyword TYPE= is hidden if the specified
               item (and its type) is found in a list (ITEM=LIST) with 
               general keys or in the header of the specified input 
               set.



               It is possible to apply adding or deleting (not
               changing) to all subsets.


               1) Add on subset level(s):
** ALL=        Apply to all remaining levels?                    Y/[N]

               ALL=Y can be typed any time to start copying the
               last entered value of an item to all remaining
               subset levels.


               2) Delete on top level (no subset(s)):
   ALL=        Delete item on ALL levels?                        Y/[N]


               3) Delete on subset level:
** ALL=        Delete on all remaining levels?                   Y/[N]

               ALL=Y can be typed any time to start deleting the
               last entered value of an item to all remaining
               subset levels.
               For ALL=N you will be prompted at each level with
               OK= before an item is removed on the current subset
               level.


   OK=         Subset nr ...: Ok to delete item?             Y/N/[stop]
               Note that 'stop' only stops the current action 
               and returns to the ITEM= loop.


   VALUE=      MODE=Add:
               Add (type) for  (descriptor)=                     [stop]
               (on subset level)
               Subset nr ...: Add (type) for  (descriptor)=      [stop]

               MODE=Change:
               New value for %.*s=                      [do not change]
               (on subset level)
               Subset nr ...: New value for %.*s=       [do not change]


   COMMENT=    MODE=Add:
               Add a comment                                     [none]

               MODE=Change:
               Give new comment                         [do not change]


               If the operation mode is A(dd), it is possible to
               create a FITS style comment here. If the mode is
               C(hange), first the old comment is displayed and
               the user is prompted to give a new one. There are
               no comments for the T(ext) and H(istory) type.


   LINE=       Give number of line to change:                    [stop]

               If items HISTORY or COMMENT are specified, the old
               contents at top- or subset level is displayed with a
               line number. You can change one line at a time. If you
               press carriage return the changed lines are written to
               the  descriptor file and the program returns to the
               ITEM= prompt.


Description:   GIPSY data consists of an image file with floating point
               numbers and an associated file called 'descriptor' or
               'header' file. This file describes structure and coordi-
               nate system of the image data by means of FITS (Flexible
               Image Transport System) keywords. Each item is stored in
               the header in a special format with the basic grammar:

                              keyword = value / comment

               In GIPSY descriptor files we distinguish the formats:
               Integer, Real, Double, Logical, Character and the so
               called commentary formats (HISTORY and COMMENT).
               
               There is also one non FITS format implemented in FIXHED
               called Text. This format will be used only in special 
               cases (See APSET keyword in the examples)..
               
              
               Examples: 
               
               The number of interferometers used in an observa-
               tion is stored as an integer, its associated header 
               keyword is 'NINTF'.
               The maximum of a map is stored as a real in 'DATAMAX'.
               The total bandwidth of an observation is stored as a
               double in 'BANDW'.
               The date of the  observation is stored as a character
               string and its associated keyword is 'DATE-OBS'.

               The set, subset used for an antenna pattern is stored in
               text form (one or more character strings). Input looks
               like: APSET=ANTPAT FREQ 1.
               This string can be a long string so it can not be stored
               in character format. It will be stored in Text format.
               However, the item will not be recognized as a real
               FITS item and therefore cannot be transported.

               Two keywords, COMMENT and HISTORY are of type History,
               but have a text format also. New information is appended
               to existing data in lines with a maximum of 80 characters.
               With the program FIXHED you can add, change, delete or
               list these items on top level (specify only the name of
               a set at the INSET= prompt) or at subset level (specify
               name and subsets). For each item of type Integer, Real,
               Double, Logical or Character, a corresponding FITS
               comment can be added or changed.

               The keywords describing the structure of the set must be
               given with an integer number appended to it like CTYPE3.
               FIXHED checks name, number and level for these keywords.
               Only keywords with NAXIS cannot be changed.

               If ITEM=HEAD all descriptors on current level will be
               displayed in alphabetical order. If tables are encountered
               name and type are listed before the list of keywords only
               if the output mode is set to TEST.


Example:       Change position angle item (BMPA) in set AURORA on top level,
               after displaying the contents of its header.

Updates:       Jul 14,  1990: VOG, Document created.
               May 18,  1992: VOG, Table detection.
               May 29,  1994: VOG, Display GDS errors. 
                                   Update of documentation.


Example:

<USER> FIXHED,INSET=TESTSET
Set TESTSET has 3 axes
RA-NCP             from    -9 to    10
DEC-NCP            from    -9 to    10
FREQ-OHEL          from     0 to     9
FIXHED working at top level
Use ITEM=HEAD to get a list of ALL descriptors on current level.
Use ITEM=LIST to get a list of recommended GIPSY keywords
<USER> FIXHED,ITEM=

HEADER at top level
===============================
APSET   = GAUSS FREQ 0
BANDW   = 0.31250000000000D+00    / total bandwidth of observation
BMMAJ   = 0.35966360473633D+03    / major axis (FWHM) of beam (arcsec)
BMMIN   = 0.35966360473633D+03    / minor axis (FWHM) of beam (arcsec)
BMPA    = 0.00000000000000D+00    / pos. angle of major axis of beam (N->E)
BUNIT   = 'WU                '    / data units   (WU,MJY/SR,...)
CDELT1  = 0.10000000000000D-01
CDELT2  = 0.10000000000000D-01
CDELT3  = 0.10000000000000D+09
CROTA1  = 0.00000000000000D+00
CROTA2  = 0.00000000000000D+00
CROTA3  = 0.00000000000000D+00
CRPIX1  = 0.10000000000000D+02
....etc...

HISTORY/COMMENT
===============
HISTORY : DUMMY SET FOR TESTING 29/11/90
COMMENT : NOT A REAL SET!

<USER> FIXHED,ITEM=BMPA

============================= FIXHED ========================
FIXHED tries to CHANGE item [BMPA] at set level.
Item BMPA (pos. angle of major axis of beam (N->E)) is known
and stored as a double.
Item found in HEADER at SET level.
=============================================================
Old value BMPA= 0.000000
<USER> FIXHED,VALUE=45
<USER> FIXHED,COMMENT=Vavlue changed from 0 to 45
<USER> FIXHED,ITEM=
<STATUS> FIXHED - +++ FINISHED +++

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
#include "reject.h"
#include "nelc.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsc_name.h"
#include "usercharu.h"
#include "userint.h"
#include "userreal.h"
#include "userdble.h"
#include "userlog.h"
#include "usertext.h"
#include "userfio.h"
#include "cancel.h"
#include "error.h"
#include "gds_extend.h"
#include "gds_errstr.h"
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
#include "gdsd_wfits.h"
#include "gdsd_type.h"
#include "gdsd_delete.h"
#include "gdsd_delall.h"
#include "gdsd_length.h"
#include "gdsd_rewind.h"
#include "gdsa_istable.h"


#define VERSION       "1.2"
#define AXESMAX        10
#define SUBSMAX        2048
#define NONE           0
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define FITSLEN        18
#define BIGSTORE       160
#define RVARLEN        150
#define CHARLEN        81
#define COMMENTSTART   34
#define ALLOC_ENTRIES  100
#define SPACE          ' '
#define ITEM_NOT_FITS  -18

#define false      0
#define true       1


#define KEY_ITEM      tofchar("ITEM=")
#define KEY_TYPE      tofchar("TYPE=")
#define KEY_ALL       tofchar("ALL=")
#define KEY_COMMENT   tofchar("COMMENT=")
#define KEY_VALUE     tofchar("VALUE=")

#define MYMAX(a,b) ((a) > (b) ? (a) : (b))
#define MYMIN(a,b) ((a) > (b) ? (b) : (a))


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

static fchar    Setin;                   /* Name of the set to be examined */
static fint     subsin[SUBSMAX];         /* Max. 'subsmax' subsets to be examined */
static fint     dfault, dfault1;         /* Default option for input etc */
static fint     Faxnum[AXESMAX];         /* Array with axis numbers */
static fint     Faxcount[AXESMAX];       /* Number of pixels on one axis */
static fint     Fclass = 1;              /* Repeat action for all subsets */
static fint     subdim, setdim;          /* Dimension of subset, set */
static fint     scrnum;                  /* Destination of log output */
static fint     Fmaxaxes  = AXESMAX;     /* Maximum number of axes in a set */
static fint     Fmaxsubs  = SUBSMAX;     /* Maximum number of subsets to work on */
static fint     toplevel = 0;            /* Indicates level is top level */
static fint     setlevel = 0;
static int      m;                       /* Counters */
static char     messbuf[BIGSTORE];       /* Character buffer for strings */

/* Miscellaneous: */

static int      agreed;                  /* Guard for several loops */

static int      itemnum;                 /* Index of item found in list with items */
static int      inlist;                  /* Item is found in item list */
static int      inhead_set;              /* Count occurences at set level */
static int      inhead_sub;              /* Count occurences at subset level */
static fint     Fres;                    /* Fortran integer result of a function */
static fint     nitems;                  /* Max. num. to enter in userxxx */
static int      notype;                  /* No type was found in list or header */
static bool     all;                     /* Does user wants to copy previous action? */
static fint     dfault_remote;           /* Change default in remote userxxx routines */
static char     *fitsstorage = NULL;     /* Store header item info */
static int      hidaxis;                 /* Is item connected to hidden axis? */


/*------------------------------------------------------------*/
/* In the structure 'descriptor', a description of an item is */
/* given. It contains 4 fields: Name of item, type, allowed   */
/* level and short description or useage. Allowed level are:  */
/* 0 (operate on toplevel only), 1 (operate on subset level   */
/* only) or -1 (operate on toplevel or subset level). The     */
/* levels are used to generate warnings and not to forbid     */
/* certain operations. The complete definition of this        */
/* structure is referred to as item LIST.                     */
/*------------------------------------------------------------*/


typedef struct 
{
   char     *word;                  /* Name of the descriptor item */
   char     *type;                  /* Format for storage */
   int      level;                  /* Level advise */  
   char     *meaning;               /* Useage or short description */
} descriptor;


descriptor descrip[] =    
{
"BUNIT"   , "CHAR" ,-1, "data units   (WU,MJY/SR,...)",
"POL"     , "CHAR" ,-1, "polarization (I,Q,U,V,XX,...)",
"OBSTYP"  , "CHAR" ,-1, "type of observation (LINE,CONT)",
"FSCRA"   , "DBLE" ,-1, "RA fringe stopping center  (degrees)",
"FSCDEC"  , "DBLE" ,-1, "DEC fringe stopping center (degrees)",
"BANDW"   , "DBLE" ,-1, "total bandwidth of observation",
"EPOCH"   , "DBLE" ,-1, "epoch of observation (years)",
"NINTF"   , "INT " ,-1, "number of interferometers used",
"NPOL"    , "INT " ,-1, "number of polarizations used (1,2,4)",
"NFREQ"   , "INT " ,-1, "number of frequency points used",
"REDCODE" , "CHAR" ,-1, "LINEMAP reduction code",
"MAPCODE" , "CHAR" ,-1, "LINEMAP map code",
"UVGRID"  , "CHAR" ,-1, "convolving function code",
"BLGRAD"  , "CHAR" ,-1, "baseline grading function",
"NBLANK"  , "INT " ,-1, "number of undefined values in map",
"INSTRUME", "CHAR" ,-1, "source of data (WSRT,TAURUS,...)",
"MAPVSN"  , "CHAR" ,-1, "tape volume of map archive",
"MAPLAB"  , "CHAR" ,-1, "tape label of map archive",
"APVSN"   , "CHAR" ,-1, "tape volume of Antenna Pattern",
"APLAB"   , "CHAR" ,-1, "tape label of Antenna Pattern",
"PCRA"    , "DBLE" ,-1, "pointing center RA  (degrees)",
"PCDEC"   , "DBLE" ,-1, "pointing center DEC (degrees)",
"MAXBASE" , "DBLE" ,-1, "maximum baseline (meter)",
"MINBASE" , "DBLE" ,-1, "minimum baseline (meter)",
"BMMIN"   , "DBLE" ,-1, "minor axis (FWHM) of beam (arcsec)",
"BMMAJ"   , "DBLE" ,-1, "major axis (FWHM) of beam (arcsec)",
"RESOL"   , "DBLE" ,-1, "resolution of spectral axis",
"EQUINOX" , "DBLE" ,-1, "equinox of coordinate system (years) ?",
"DATE-OBS", "CHAR" ,-1, "observation date (DD/MM/YY)",
"APSET"   , "TEXT" ,-1, "set number of antenna pattern",
"FILEID"  , "CHAR" ,-1, "identification of file",
"FREQ0"   , "DBLE" ,-1, "Rest frequency of spectral line (Hz)",
"UVFREQ"  , "DBLE" ,-1, "reference freq. for UV coords. (MHz)",
"UVBANDW" , "DBLE" ,-1, "bandwidth of UV coordinates (MHz)",
"NOISE"   , "DBLE" ,-1, "noise of map",
"NORM"    , "DBLE" ,-1, "normalizing factor in FFT",
"DATAMAX" , "REAL" ,-1, "maximum value of map",
"DATAMIN" , "REAL" ,-1, "minimum value of map",
"OBSERVER", "CHAR" ,-1, "observer name",
"OBJECT"  , "CHAR" ,-1, "object name",
"TAPER"   , "CHAR" ,-1, "type of frequency taper (HANNING,...)",
"GRIDTYPE", "CHAR" ,-1, "type of grid",
"ORIGIN"  , "CHAR" ,-1, "tape writing institute",
"DATE"    , "CHAR" ,-1, "tape writing date (DD/MM/YY)",
"OBSTIME" , "CHAR" ,-1, "observation time (HH:MM:SS)",
"BMPA"    , "DBLE" ,-1, "pos. angle of major axis of beam (N->E)",
"HISTORY" , "HIST" ,-1, "History records",
"COMMENT" , "HIST" ,-1, "Comment records",
"CTYPE***", "CHAR" , 0, "Name of primary coordinate axis",
"DTYPE***", "CHAR" , 0, "Name of secondary coordinate axis",
"CRVAL***", "DBLE" , 0, "Value at (primary) reference pixel",
"DRVAL***", "DBLE" , 0, "Value at (secondary) reference pixel",
"CDELT***", "DBLE" , 0, "Grid spacing between pixels (prim.axis)",
"DDELT***", "DBLE" , 0, "Grid spacing between pixels (sec.axis)",
"CRPIX***", "DBLE" , 0, "Reference pixel on primary axis",
"DRPIX***", "DBLE" , 0, "Reference pixel on secondary axis",
"CROTA***", "DBLE" , 0, "Rotation angle of primary axis",
"DROTA***", "DBLE" , 0, "Rotation angle of secondary axis"
};        

/*--------------------------------------------------*/
/* The next define calculates the number of struc-  */
/* ture elements in declaration above.              */
/*--------------------------------------------------*/
#define MAXLIST        (sizeof(descrip)/sizeof(descriptor))



static void errorC( int level,
                    char *str )
/*------------------------------------------------------------*/
/* The C version of 'error'.                                  */
/*------------------------------------------------------------*/
{
   fint   flev = (fint) level;
   error_c( &flev, tofchar( str ) );
}


static void displayGDSerror( int  level, 
                             fint err )
/*------------------------------------------------------------*/
/* PURPOSE: Display a message string associated with a GDS    */
/* error code.                                                */
/*------------------------------------------------------------*/
{
   fchar   Errstr;
   fmake( Errstr, 180 );
   gds_errstr_c( Errstr, &err );
   anyoutf( level, "GDS error (%d): %.*s",
            (int) err,
            nelc_c(Errstr), Errstr.a );
}



static int compare( char *s1, 
                    char *s2 )
/*------------------------------------------------------------*/
/* This routine is an alternative for the function 'strcmp'.  */
/* The difference is that numbers in the strings starting at  */
/* the same position are treated as characters with an integer*/
/* value equal to the integer equivalence of the sub string   */
/* containing the digits.                                     */
/*------------------------------------------------------------*/ 
{
   int  i, j;                            /* Local counters */
   int  n1, n2;
   char number1[20];                     /* strings containing digits */
   char number2[20];
   int  diff;                            /* Difference between two chars */


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
      else                                /* Is end of a string reached? */
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



void showsubset( fchar Setin,
                 fint *subsin,
                 fint *Faxnum,
                 fint *subdim,
                 fint *setdim,
                 fchar Subsetstr )
/*------------------------------------------------------------*/
/* PURPOSE: Create the string 'Subsetstr' containing          */
/*          information about the running axis/axes.          */
/* Example: Subsetstr == "FREQ=1, RA=2"                       */
/*------------------------------------------------------------*/
{
   int    n;
   char   axis_b[20+1];
   fchar  Fctype;
   fint   err;
   fint   Fgrid;
   char   dummystr[BIGSTORE];
   char   showbuf[BIGSTORE];


   err = 0;
   showbuf[0] = '\0';
   for (n = *subdim; n < *setdim; n++)
   {
      Fctype.a   = axis_b; Fctype.l = 20;
      axis_b[20] = '\0';
      gdsc_name_c( Fctype, Setin, &Faxnum[n], &err );
      if (err < 0)
      {
         anyoutf( 1, "Cannot get axis name. Reason:" );
         displayGDSerror( 1, err );
      }
      Fgrid = gdsc_grid_c( Setin, &Faxnum[n], subsin, &err );
      if (err < 0)
      {
         anyoutf( 1, "Cannot get axis name. Reason:" );
         displayGDSerror( 1, err );
      }
      if (( n + 1 ) == *setdim)
      {
         sprintf( dummystr, "%s=%d ",
                  strtok( axis_b, " -" ),
                  Fgrid );
      }
      else
      {
         sprintf( dummystr, "%s=%d,",
                  strtok( axis_b, " -" ),
                  Fgrid );                                     /* Comma added */
      }
      sprintf( showbuf, "%.*s%s",
               strlen(showbuf),
               showbuf,
               dummystr );
   }
   strcpy( Subsetstr.a, showbuf);        /* Copy the info to the F-type string */
}



static void menu( void )
/*------------------------------------------------------------*/
/* PURPOSE: Display all available fits keywords from the item */
/*          LIST.                                             */
/*------------------------------------------------------------*/
{
   int  i;                              /* Counter */


   anyoutf( 1, " " );
   anyoutf( 1, "**** RECOMMENDED GIPSY DESCRIPTOR ITEMS ****" );
   anyoutf( 1, " " );

   for (i = 0; i < MAXLIST; i++)
   {
      anyoutf( 1, "%-15.15s  : %-50s",
               descrip[i].word,
               descrip[i].meaning );
   }
   anyoutf( 1, " " );
}



static void general_header( fchar Setin,
                            fint *adnsubs,
                            fint *subsin,
                            fint *Faxnum,
                            fint *adsetdim,
                            fint *adsubdim )
/*------------------------------------------------------------*/
/* PURPOSE: Give list with header items and their values      */
/*          found in the descriptor file.                     */
/* The keyword structure 'descrip' is global!                 */
/*------------------------------------------------------------*/
{
   fint    recordnum;              /* Internal record number for gdsd-find */
   fint    slevel;                 /* Input level */
   fint    err1;		   /* Level return codes (<0 -> error) */
   fchar   Fitsstr;                /* Receive string with fits data */
   fchar   Descrname;
   int     j, m;                   /* Counters */
   char    message[BIGSTORE];      /* Message string for userxxx routines */
   fint    readbytes;              /* For use in gdsd_readc */
   fint    foundbytes;             /*   "  "   "  "    "    */
   fint    start;                  /*   "  "   "  "    "    */
   char    messbuf[BIGSTORE+1];
   fchar   Subsetstr;              /* Subset information */
   int     pointeroffset;          /* Pointer offset in descriptor storage */
   int     entries;                /* Number of lines to sort */
   fint    table_status;           /* Is descriptor associated with a table? */
   fint    nsubs;	           /* Non pointer version of number of subsets */
   fint    setdim, subdim;         /* Non pointer versions of the dimensions */
   char    tablebuf[BIGSTORE];     /* Buffer for table related text */


   nsubs  = *adnsubs;              /* Avoid pointer arithmetic */
   setdim = *adsetdim;
   subdim = *adsubdim;

   fmake( Fitsstr, RVARLEN );
   fmake( Descrname, BIGSTORE );
   fmake( Subsetstr, BIGSTORE );

   for (j = 0; j < nsubs; j++ )
   {
      slevel = subsin[j];
      if (slevel > 0)
      {
         strcpy( message, "HEADER: " );
         memset( Subsetstr.a, SPACE, BIGSTORE );
         /* Working on subset level, show the axes */
         showsubset( Setin, &slevel, Faxnum,
                     &subdim, &setdim, Subsetstr );
         strncat( message, Subsetstr.a, (int) nelc_c(Subsetstr) );
      }
      else
         strcpy( message, "HEADER at top level " );

      anyoutf( 1, " " );
      anyoutf( 1, message );
      anyoutf( 1, "===============================" );

      recordnum = 0;                              /* Position in header file */
      entries = 0;
      do
      /*-----------------------------------------------------------------*/
      /* The main purpose of this routine is to generate all available   */
      /* keywords with the function 'gdsd_find'. There are several       */
      /* possibilities to read an item (gdsd_rfits, gdsd_readc or as     */
      /* variable length record (History type) ). First, if a generated  */
      /* descriptor name is not HISTORY or COMMENT, try to read the      */
      /* header information as a fits item. If the GDS descriptor        */
      /* contains table info, give a message. If necessary and possible  */
      /* try to add comment from a list with keywords. If a keyword      */
      /* string is constructed, store it to be able to sort the keywords.*/
      /*-----------------------------------------------------------------*/
      {
         err1 = 0;
         gdsd_find_c( Descrname, Setin, &slevel, &recordnum, &err1 );
         if ((recordnum != 0) && (err1 < 0))
            anyoutf( 1, "*** Found entry but no item ***" );

         if ((recordnum != 0) && (err1 == slevel))
         {
            /* An item is found. Examine its table status */
            table_status = gdsa_istable_c( Descrname );

            switch ((int) table_status)
            /*--------------------------------------------------*/
            /* If there is some table info, show it in debug    */
            /* mode only.                                       */
            /*--------------------------------------------------*/
            {
               case 1 : {
	          sprintf( tablebuf, "%.*s contains table header",
	                   nelc_c( Descrname), Descrname.a );
	          anyoutf( 16, tablebuf );
               } break;
               case 2: {
                  sprintf( tablebuf, "%.*s contains column header",
	                   nelc_c( Descrname), Descrname.a );
                  anyoutf( 16, tablebuf );
               } break;
               case 3: {
                  sprintf( tablebuf, "%.*s contains column data",
	                   nelc_c( Descrname), Descrname.a );
                  anyoutf( 16, tablebuf );
               }
            }


            if ( (table_status == 0) &&
            (
                !((strncmp( Descrname.a, "HISTORY", 4 ) == 0) ||
                  (strncmp( Descrname.a, "COMMENT", 4 ) == 0))
               ) )
            {
               /* Read FITS descriptor item, keyword is included */
               err1 = 0;
               gdsd_rfits_c( Setin, Descrname, &slevel, Fitsstr, &err1 );
               if (err1 == slevel)
                  sprintf( messbuf, "%.*s", nelc_c(Fitsstr), Fitsstr.a );

               if (err1 < 0)
               /*--------------------------------------------------*/
               /* Item could not be read as a normal FITS item.    */
               /* Read descriptor item, without keyword in return  */
               /* string. Don't include any comment (prevent space */
               /* problems).                                       */
               /*--------------------------------------------------*/
               { 
                  if (err1 != ITEM_NOT_FITS)
                  {
                     anyoutf( 1, "Problems reading FITS descriptor [%.*s]. Reason:",
                              nelc_c(Descrname), Descrname.a );
                     displayGDSerror( 1, err1 );
                  }
                  else
                  {
                     anyoutf( 16, "Trying to read item as non-FITS...");
                     err1  = 0;
                     start = 1;
                     readbytes = BIGSTORE;
                     memset( Fitsstr.a, SPACE, RVARLEN );   /* Clean */
                     gdsd_readc_c( Setin, Descrname, &slevel, Fitsstr,
                                   &readbytes, &start, &foundbytes, &err1 );
                     if (err1 == slevel)
                     {
                        sprintf( messbuf, "%-8.8s= %-.*s",
                                 Descrname.a,
                                 nelc_c( Fitsstr ), Fitsstr.a );
                     }
                     if (err1 < 0)
                     {
                        anyoutf( 1, "Problems reading CHARACTER-typed descriptor item [%.*s]. Reason:",
                                 nelc_c(Descrname), Descrname.a );
                        displayGDSerror( 1, err1 );
                     }
                  }
               }
               /*--------------------------------------------------*/
               /* 'fitsstorage' is a long string consisting of sub */
               /* strings where each sub string contains header    */
               /* information. In fact it is a pointer to one      */
               /* character string  and room for the item names has*/
               /* to be created. This is accomplished by a call to */
               /* 'realloc'. The first time it creates room for    */
               /* ALLOC_ENTRIES file names. Every time that more   */
               /* strings have to be stored, a call to 'realloc'   */
               /* creates more space. It is possible to store and  */
               /* retrieve the names by means of pointers. The     */
               /* offset is stored in 'pointeroffset' and is       */
               /* increased by the maximum length of a string.     */
               /*--------------------------------------------------*/
               if ( !(entries % ALLOC_ENTRIES) )
               {
                  int s;
                  s = ((entries+ALLOC_ENTRIES)/ALLOC_ENTRIES) * ALLOC_ENTRIES;
                  fitsstorage = realloc( fitsstorage, s * BIGSTORE );
               }
               pointeroffset = entries * BIGSTORE;
               messbuf[BIGSTORE-1] = '\0';
               strcpy( fitsstorage + pointeroffset, messbuf );
               entries++;
            }
         }
      }
      while (recordnum != 0);                          /* No items anymore */

      /* Do a quicksort, compare items in function 'compare'. */
      qsort( fitsstorage, entries, BIGSTORE, (int(*)())compare );

      for (m = 0; m < entries; m++)
      {
        pointeroffset = m * BIGSTORE;
        sprintf( messbuf, "%-80s", fitsstorage + pointeroffset );
        anyoutf( 1, messbuf );
      }


      for (m = 0; m < 2; m++)
      /*--------------------------------------------------*/
      /* Before reading any variable length records, you  */
      /* have to set the current read position at the     */
      /* beginning of the descriptor item (HISTORY,       */
      /* COMMENT).                                        */
      /*--------------------------------------------------*/
      {
         if (m == 0)
         {
            strcpy( Descrname.a, "HISTORY" );
            anyoutf( 1, " " );
            anyoutf( 1, "HISTORY" );
            anyoutf( 1, "=======" );
         }
         else
         {
            strcpy( Descrname.a, "COMMENT" );
            anyoutf( 1, " " );
            anyoutf( 1, "COMMENT" );
            anyoutf( 1, "=======" );
         }
         err1 = 0;
         gdsd_rewind_c( Setin, Descrname, &slevel, &err1 );
         if (err1 < 0)
         {
            if (err1 == -7)
               anyoutf( 1, "<empty>" );
            else
            {
               anyoutf( 1, "Problems putting read position at beginning of [%.*s]",
                            nelc_c(Descrname), Descrname.a );
               displayGDSerror( 1, err1 );
            }
         }
         if (err1 >= 0)
         {
            do
            {
               err1 = 0;
               memset( Fitsstr.a, SPACE, RVARLEN );
               /* Read variable length record from descriptor item */
               gdsd_rvar_c( Setin, Descrname, &slevel, Fitsstr, &err1 );
               if (err1 >= 0)
               {
                  sprintf( messbuf, "%-8.8s: %-.*s",
                           Descrname.a,
                           nelc_c( Fitsstr ),
                           Fitsstr.a );
                  anyoutf( 1, messbuf );
               }
               else if (err1 != -4)
               {                  
                  anyoutf( 1, "Problems reading variable length record from [%.*s]",
                               nelc_c(Descrname), Descrname.a );
                  displayGDSerror( 1, err1 );
               }

               /* Stop reading HIST type records if return level < 0 */
            } while (err1 >= 0);
         }
      }
   }
   anyoutf( 1, " " );
}



static int match( char *itemname, 
                  int  *itemnum )
/*------------------------------------------------------------*/
/* PURPOSE: Is a given descriptor item one of a list with     */
/*          known header items? Return the number of hits.    */
/*------------------------------------------------------------*/
{
   int    found;                            /* Number of occurences in list */
   int    i;


   *itemnum = -1;                           /* Set item number to non existent index */
   found    = 0;                            /* Item not found (yet) */
   for (i = 0; i < MAXLIST; i++)
   {
      if ( strstr(descrip[i].word, "***") != NULL )
      /*-----------------------------------------------------------*/
      /* Compare string with axis related keyword like CTYPE etc   */
      /* If item is one of these special keywords, return original */
      /* string.                                                   */
      /*-----------------------------------------------------------*/
      {
         int len1, len2;
         len1 = strlen( descrip[i].word ) - 3;
         len2 = MYMIN( strlen(itemname), len1 );
         if (strncmp( descrip[i].word, itemname, len2 ) == 0)
         {
            *itemnum = i;                   /* Item was found in list */
            found++;
         }
      }
      else
      {
         if ( strcmp(descrip[i].word, itemname) == 0 )
         {
            *itemnum = i;                   /* Item was found in list */
            found++;
         }
      }
   }
   return( found );                         /* Return with match or original input */
}



static int needuchar( fchar Itemname )
/*------------------------------------------------------------*/
/* PURPOSE: Some keywords are text items but need uppercase   */
/*          only                                              */
/*------------------------------------------------------------*/
{
   int len;
   len = nelc_c( Itemname );
   if ((strncmp( "CTYPE",    Itemname.a, 5 ) == 0) ||
       (strncmp( "DTYPE",    Itemname.a, 5 ) == 0) ||
       (strncmp( "POL",      Itemname.a, len ) == 0) ||
       (strncmp( "INSTRUME", Itemname.a, len ) == 0) )
   {
      return true;
   }
   return false;
}



static int special_keys( fchar Itemname, 
                         int   toplevel, 
                         int   *hidaxis )
/*------------------------------------------------------------*/
/* PURPOSE: Check whether Itemname is a 'forbidden' keyword.  */
/* If it is allowed to change this keyword, check appended    */
/* integer. The routine returns 'true' if the item can be     */
/* changed, i.e. is not a special item, or is a special item  */
/* with the correct number appended. The variable 'hidaxis' is*/
/* a flag for an item connected to a 'hidden' axis.           */
/*------------------------------------------------------------*/
{
   *hidaxis = false;                               /* Reset 'Hidden' axis flag */

   if (strncmp( "NAXIS", Itemname.a, 5 ) == 0)
   {
       anyoutf( 1, "You are not allowed to change NAXIS items" );
       anyoutf( 1, "If you want to extend your set with axes, use program EXTEND.");
       anyoutf( 1, "If you want to decrease dimensionality, use program COPY." );
       return( 0 );
   }

   /* Check appended integer on validity */

   if ((strncmp( "CTYPE", Itemname.a, 5 ) == 0) ||
       (strncmp( "CRVAL", Itemname.a, 5 ) == 0) ||
       (strncmp( "CDELT", Itemname.a, 5 ) == 0) ||
       (strncmp( "CRPIX", Itemname.a, 5 ) == 0) ||
       (strncmp( "CROTA", Itemname.a, 5 ) == 0) ||
       (strncmp( "DTYPE", Itemname.a, 5 ) == 0) ||
       (strncmp( "DRVAL", Itemname.a, 5 ) == 0) ||
       (strncmp( "DDELT", Itemname.a, 5 ) == 0) ||
       (strncmp( "DRPIX", Itemname.a, 5 ) == 0) ||
       (strncmp( "DROTA", Itemname.a, 5 ) == 0))
   {

      int  len;
      char digitstr[BIGSTORE];
      int  number;
      int  i;
      int  ok;


      if (!toplevel)
      {
         anyoutf( 1, "Operations on top level only!" );
         return( 0 );
      }

      len = nelc_c( Itemname );              /* Length of item string */
      ok  = true;
      number = -1;
      for (i = 5; i < len; i++)
      {
      	digitstr[i-5] = Itemname.a[i];       /* Copy digits to another string */
      	if ( !isdigit(digitstr[i-5]) )
      	   ok = false;                       /* All chars are digits? */
      }
      digitstr[i-5] = '\0';
      if (ok)
         number = atoi( digitstr );          /* Convert string to integer */
      ok = (number != 0);

      /* The converted number is associated with an axis number, */
      /* so it has to be greater or equal to 1 and less or equal to */
      /* the dimension of the set. */

      if (!ok)
      {
         anyoutf( 1, "Name not followed by valid number" );
         return( 0 );
      }
      else
      {
         if (number < 1)
         {
            anyoutf( 1, "Number must be greater than 1!" );
            return( 0 );
         }

         /*----------------------------------------------------------*/
         /* The valid number is known and greater than the dimension */
         /* of the set. But the number can still refer to a hidden   */
         /* axis. In order to find out, read the header.             */
         /*----------------------------------------------------------*/
         if (number > (int) setdim)
         {
            fint    R1;
            fint    Top;
            fchar   Dummyfchar;
            double  Dummydouble;


            R1 = 0;
            fmake( Dummyfchar, 80 );
            Top = (fint) toplevel;
            if (strstr(Itemname.a, "CTYPE") || strstr(Itemname.a, "DTYPE"))
            {
               gdsd_rchar_c( Setin,                /* Global name */
                             Itemname,             /* f.i. CTYPE4 */
                             &Top,                 /* Only at set level */
                             Dummyfchar,           /* Receive dummy information */
                             &R1 );                /* Not found if < 0 */
            }
            else
            {
               gdsd_rdble_c( Setin,
                             Itemname,             /* f.i. CRVAL4 */
                             &Top,
                             &Dummydouble,
                             &R1 );
            }
            if ((int) R1 < 0)
            {
               anyoutf( 1, "Name not followed by a valid number. Number is greater" );
               anyoutf( 1, "than the dimension of the set (hidden axes included)." );
               return( 0 );
            }
            else
               *hidaxis = true;                    /* This was a hidden axis */
         }
      }
   }
   return( true );                                 /* Item accepted */
}



static void addcomment( fchar Setin,
                        fint  level,
                        fchar Descrname,
                        fchar Comment )
/*------------------------------------------------------------*/
/* PURPOSE: Add a comment to descriptor item + contents.      */
/* The FITS standard wants the keyword to be <= 8 characters, */
/* the '=' character must be in column 9. Then there is one   */
/* blank character followed by a slash to indicate that a     */
/* comment follows. The comment can contain both upper and    */
/* lower case ASCII characters.                               */
/*------------------------------------------------------------*/
{
   fint   err = 0;
   int    len;
   fchar  Datastr;
   char   buf[80+1];   


   fmake( Datastr, 80 );
   gdsd_rfits_c( Setin, Descrname, &level, Datastr, &err );
   if (err < 0)
   {
      anyoutf( 1, "Did not add comment. Reason:" );
      displayGDSerror( 1, err );
      return;
   }

   anyoutf( 16, "Descr.:  %.*s|", nelc_c(Datastr),   Datastr.a );
   anyoutf( 16, "Item:    %.*s|", nelc_c(Descrname), Descrname.a );
   anyoutf( 16, "Comment: %.*s|", nelc_c(Comment),   Comment.a );

   len = nelc_c(Comment);
   (void) sprintf( buf, "%-30.30s / %-*.*s",
                   Datastr.a,
                   len, len,
                   Comment.a );

   anyoutf( 16, "String to write:: %s", buf);

   err = 0;
   gdsd_wfits_c( Setin, Descrname, &level, tofchar(buf), &err );
   if (err < 0)
   {
       anyoutf( 1, "Problems updating this item. Reason:" );
       displayGDSerror( 1, err );
   }
   return;
}



static int additem( fchar Itemname,
                    char  keytype,
                    fint  slevel,
                    fint  dfault,
                    int   subnr )
/*------------------------------------------------------------*/
/* PURPOSE: Add value for a new descriptor.                   */
/* The VALUE= keyword can be asked hidden or unhidden         */
/* depending on the value of 'dfault'. The hidden mode is     */
/* used to add items to subsets in a loop. In the unhidden    */
/* mode it is possible that the user wants to abort adding    */
/* items at (some) subset level. If carriage return is        */
/* pressed, this routine will return with the value 0 to      */
/* indicate the adding can be stopped in MAIN                 */
/*------------------------------------------------------------*/
{
   fint   err1;
   fint   intval;
   fint   nitems;           /* Max num. of items to read in userXXXX routines */
   fint   Fres, Frescom;
   float  realval;
   double dbleval;
   bool   logval;
   char   message[BIGSTORE];
   char   messbuf[BIGSTORE];
   fchar  Charval;             /* F-type variables to store user given values */


   Fres = 0;
   fmake( Charval, 70 );

   if (dfault == REQUEST)
   {
      switch (keytype)
      {
         case 'I' :
            strcpy( messbuf, "Add INTEGER for " );
            break;
         case 'R' :
            strcpy( messbuf, "Add REAL for " );
            break;
         case 'D' :
            strcpy( messbuf, "Add DOUBLE for " );
            break;
         case 'L' :
            strcpy( messbuf, "Add LOGICAL for " );
            break;
         case 'C' :
            strcpy( messbuf, "Add STRING for " );
            break;
         case 'T' :
         case 'H' :
            strcpy( messbuf, "Add TEXT for " );
            break;
         default :
             anyoutf( 1, "Unknown type");
      }
      if (slevel != 0)
      {
         sprintf( message  , "Subset nr %d: %s %.*s=     [stop]",
                  subnr,
                  messbuf,
                  nelc_c( Itemname ), Itemname.a );
      }
      else
      {		                                         /* Top level */
         sprintf( message, "%s %.*s=     [stop]",
                  messbuf,
                  nelc_c( Itemname ), Itemname.a );
      }
   }
   else
   {
      /* Keyword is supposed to be hidden in this case */
      strcpy( message, " " );
   }


   /* Get new value and write to header */

   nitems = 1;

   /* CHARACTER TYPE */
   if (keytype == 'C')
   {
      /* The user can give more than one string here */
      Fres = usertext_c( Charval,
                         &dfault,
                         KEY_VALUE,
                         tofchar(message) );
      if ((Fres != 0) || (dfault == HIDDEN))
      {
         if ( needuchar(Itemname) )                 /* Uppercase text or not? */
         {
            int   p;
            for (p = 0; p < (int) nelc_c(Charval); p++)  /* Uppercase this text */
               Charval.a[p] = toupper(Charval.a[p]);
         }
         err1  = 0;
         gdsd_wchar_c( Setin, Itemname, &slevel, Charval, &err1 );
         if (err1 < 0)
         {
            anyoutf( 1, "Problems writing contents for [%.s]. Reason:",
                     nelc_c(Itemname), Itemname.a );
            displayGDSerror( 1, err1 );
         }
      }
   }

   /* INTEGER TYPE */
   if (keytype == 'I')
   {
      Fres = userint_c( &intval,
                        &nitems, &dfault,
                        KEY_VALUE,
                        tofchar( message ) );
      if ((Fres != 0) || (dfault == HIDDEN))
      {
         err1  = 0;
         gdsd_wint_c( Setin, Itemname, &slevel, &intval, &err1 );
         if (err1 < 0)
         {
            anyoutf( 1, "Problems writing an integer for [%.s]. Reason:",
                     nelc_c(Itemname), Itemname.a );
            displayGDSerror( 1, err1 );
         }
      }
   }

   /* REAL TYPE */
   if (keytype == 'R')
   {
      Fres = userreal_c( &realval,
                         &nitems, &dfault,
                         KEY_VALUE,
                         tofchar( message ) );
      if ((Fres != 0) || (dfault == HIDDEN))
      {
         err1  = 0;
         gdsd_wreal_c( Setin, Itemname, &slevel, &realval, &err1 );
         if (err1 < 0)
         {
            anyoutf( 1, "Problems writing a real for [%.s]. Reason:",
                     nelc_c(Itemname), Itemname.a );
            displayGDSerror( 1, err1 );
         }
      }
   }

   /* DOUBLE TYPE */
   if (keytype == 'D')
   {
      Fres = userdble_c( &dbleval,
                         &nitems, &dfault,
                         KEY_VALUE,
                         tofchar( message ) );
      if ((Fres != 0) || (dfault == HIDDEN))
      {
         err1  = 0;
         gdsd_wdble_c( Setin, Itemname, &slevel, &dbleval, &err1 );
         if (err1 < 0)
         {
            anyoutf( 1, "Problems writing a double for [%.s]. Reason:",
                     nelc_c(Itemname), Itemname.a );
            displayGDSerror( 1, err1 );
         }
      }
   }


   /* LOGICAL TYPE */
   if (keytype == 'L')
   /*--------------------------------------------------*/
   /* Logical variable T or F in column 30.            */
   /*--------------------------------------------------*/
   {
      Fres = userlog_c( &logval,
                        &nitems, &dfault,
                        KEY_VALUE,
                        tofchar( message ) );
      if ((Fres != 0) || (dfault == HIDDEN))
      {
         err1  = 0;
         gdsd_wlog_c( Setin, Itemname, &slevel, &logval, &err1 );
         if (err1 < 0)
         {
            anyoutf( 1, "Problems writing a logical for [%.s]. Reason:",
                     nelc_c(Itemname), Itemname.a );
            displayGDSerror( 1, err1 );
         }
      }
   }

   if ( (keytype != 'T') &&
        (keytype != 'H') &&
        (dfault == REQUEST) &&
        (Fres == 0))
      return( 0 );


   /*---------------------------*/
   /* Add a user given comment. */
   /*---------------------------*/
   if ( (strpbrk( &keytype, "IRDLC" ) != NULL) &&
        (!((dfault == REQUEST) && (Fres == 0))) )
   {
      /* For the types defined above, it is possible to add a comment */
      /* In 'request' mode no carriage return was given */

      fchar   Commentstr;

      fmake( Commentstr, BIGSTORE );
      Frescom = usertext_c( Commentstr,
                            &dfault,
                            KEY_COMMENT,
                            tofchar( "Add a comment:            [none]" ) );
      if (dfault == REQUEST)
         cancel_c( KEY_COMMENT );

      if ((Frescom != 0) || (dfault == HIDDEN))
         addcomment( Setin, slevel, Itemname, Commentstr );

   }


   /* TEXT TYPE */
   if (keytype == 'T')
   {
      fint   writebytes;   /* Number of bytes to write in writec routine */
      fint   foundbytes;   /* Num. of bytes actually written */
      fint   start;        /* Relative position to start to write */


      Fres = usertext_c( Charval,
                         &dfault,
                         KEY_VALUE,
                         tofchar( message ) );
      if ((Fres != 0) || (dfault == HIDDEN))
      /*--------------------------------------------------*/
      /* Relative position, starting at 1, at which will  */
      /* be written. If 0, writing takes place at the end */
      /* of the descriptor item                           */
      /*--------------------------------------------------*/
      {
         err1   = 0;
         start = 1;
         writebytes = nelc_c( Charval );
         gdsd_writec_c( Setin,
                        Itemname,
                        &slevel,
                        Charval,
                        &writebytes,
                        &start,
                        &foundbytes,
                        &err1 );
         if (err1 < 0)
         {
            anyoutf( 1, "Problems writing character typed contents for [%.s]. Reason:",
                     nelc_c(Itemname), Itemname.a );
            displayGDSerror( 1, err1 );
         }
      }
   }

   /* HISTORY TYPE */
   if (keytype == 'H')
   {
      int i;
      for (i = 0; i < BIGSTORE; Charval.a[i++] = ' ' );
      Fres = usertext_c( Charval,
                         &dfault,
                         KEY_VALUE,
                         tofchar(message) );
      if ((Fres != 0) || (dfault == HIDDEN))
      {
         err1 = 0;
         gdsd_wvar_c( Setin, Itemname, &slevel, Charval, &err1 );
         if (err1 < 0)
         {
            anyoutf( 1, "Problems writing contents for [%.*s]. Reason:",
                     nelc_c(Itemname), Itemname.a );
            displayGDSerror( 1, err1 );
         }
      }
   }

   if (dfault == REQUEST)
      cancel_c( KEY_VALUE );

   if ((dfault == REQUEST) && (Fres == 0))
      return( 0 );

   return( 1 );
}



static void chgitem( fchar Itemname,
                     char  keytype,
                     fint  slevel,
                     int   subnr,
                     int   hidaxis )
/*------------------------------------------------------------*/
/* PURPOSE: Change contents of existing descriptor.           */
/* An item is found in the header at 'slevel'. Create a       */
/* default for the userXXX routines with the old header value.*/
/* Then ask for a new value and update the header. If the item*/
/* is of HIST(ory) type like HISTORY or COMMENT, first show   */
/* all old contents in lines of 80 characters or less. Each   */
/* line can be changed separately. After pressing carriage    */
/* return to the LINE= prompt, all changed and unchanged      */
/* history data is written to the header.                     */
/*------------------------------------------------------------*/
{
   fint    err1;
   fchar   Charval;                  /* Storage for userXXX routines */
   fint    intval;
   float   realval;
   double  dbleval;
   bool    logval;
   char    message[BIGSTORE];
   fint    dfault;
   char    *storestr = NULL;
   int     histcount;
   int     writeval;


   fmake( Charval, CHARLEN );
   histcount = 0;
   writeval  = 0;                    /* No items to write yet */
   err1      = 0;
   if (keytype == 'I')
      gdsd_rint_c(  Setin, Itemname, &slevel, &intval,  &err1 );
   else if (keytype == 'R')
      gdsd_rreal_c( Setin, Itemname, &slevel, &realval, &err1 );
   else if (keytype == 'D')
      gdsd_rdble_c( Setin, Itemname, &slevel, &dbleval, &err1 );
   else if (keytype == 'L')
      gdsd_rlog_c(  Setin, Itemname, &slevel, &logval,  &err1 );
   else if (keytype == 'C')
      gdsd_rchar_c( Setin, Itemname, &slevel, Charval,  &err1 );

   if (err1 < 0)
   {
      anyoutf( 1, "Problems reading contents type '%c', for [%.*s]. Reason:",
               keytype, nelc_c(Itemname), Itemname.a );
      displayGDSerror( 1, err1 );
      /* Do NOT return here. A new value can be entered hereafter! */
   }

   if (keytype == 'T')
   {
      fint    readbytes;
      fint    foundbytes;
      fint    start;
      fint    err2;

      start = 1;
      err1  = err2 = 0;
      readbytes = gdsd_length_c( Setin, Itemname, &slevel, &err2 );
      if (err2 < 0)
      {
         anyoutf( 1, "Problem to determine length of record for [%.*s]. Reason:",
                  nelc_c(Itemname), Itemname.a );
         displayGDSerror( 1, err2 );
      }
      else
         readbytes = CHARLEN;

      gdsd_readc_c( Setin, Itemname, &slevel, Charval,
                    &readbytes, &start, &foundbytes, &err1 );
      if (err1 < 0)
      {
         anyoutf( 1, "Problems reading CHARACTER-typed descriptor item [%.*s]. Reason:",
                  nelc_c(Itemname), Itemname.a );
         displayGDSerror( 1, err1 );
         return;
      }
   }
   else if (keytype == 'H')
   {
      /* Memory occupied by one line with data of history type: */

      int    space;
      int    len;
      char   dummystr[BIGSTORE+1];
      char   headerstr[40];


      space = BIGSTORE * sizeof(char);
      histcount = 0;
      err1 = 0;
      sprintf( headerstr, "Contents subset nr %d:", subnr );
      anyoutf( 1, headerstr );
      gdsd_rewind_c( Setin, Itemname, &slevel, &err1 );
      if (err1 < 0)
      {
         anyoutf( 1, "Problems rewinding item [%.*s]. Reason:",
                  nelc_c(Itemname), Itemname.a );
         displayGDSerror( 1, err1 );
         return;
      }
      else
      {
         do
         {
            fchar Charbuf;

            fmake( Charbuf, CHARLEN );
            err1 = 0;
            gdsd_rvar_c( Setin, Itemname, &slevel, Charval, &err1 );
            if (err1 >= 0)
            /*--------------------------------------------------*/
            /* Store this line, allocate memory each time a line*/
            /* is read. Append the line to the existing         */
            /* character string containing all previous         */
            /* information in blocks of 'BIGSTORE' characters.  */
            /*--------------------------------------------------*/
            {
               storestr = (char *) realloc( storestr, (histcount+1) * space );
               if (storestr == NULL)
                  errorC( 4, "Memory allocation error!" );
               len = nelc_c( Charbuf );
               strncpy( dummystr, Charbuf.a, len );
               dummystr[len] = '\0';               /* String is 0 terminated now */
               strcpy( storestr + (histcount*BIGSTORE), dummystr );    /* Append */
               histcount++;
               anyoutf( 1, " %d : %s", histcount, dummystr );
            }
         }
         while (err1 >= 0);
      }
      err1 = 0;     /* Reset error code only for this type */
   }

   /*--------------------------------------------------*/
   /* If an old value is found, create a message and   */
   /* use this value as a default. Note that the error */
   /* message could have indicated that the type was   */
   /* not the requested type (errors -45,-46,-47).     */
   /* Continue in that case.                           */
   /*--------------------------------------------------*/   
   if (err1 >= 0 || err1 == -45 || err1 == -46 || err1 == -47)
   {
      char   mbuf[120];
      if (hidaxis)
      {
         /* If item was connected to hidden axis: */
         sprintf( message,
                  "OLD (hidden axis) VALUE %.*s= ",
                  nelc_c( Itemname ),
                  Itemname.a );
      }
      else
      {
         sprintf( message,
                  "OLD VALUE %.*s= ",
                  nelc_c( Itemname ),
                  Itemname.a );
      }

      if (keytype == 'C' || keytype == 'T')
      {
         sprintf( mbuf, "%s%.*s", message,
                  nelc_c(Charval), Charval.a );
      }
      if (keytype == 'I')
         sprintf( mbuf, "%s%d", message, intval  );
      if (keytype == 'R')
         sprintf( mbuf, "%s%f", message, realval );
      if (keytype == 'D')
         sprintf( mbuf, "%s%f", message, dbleval );
      if (keytype == 'L')
      {
      	 if (logval)
            sprintf( mbuf, "%s YES", message );
         else
      	   sprintf( mbuf, "%s NO", message );
      }
      /* Write the created message except if item is of History type */
      if (keytype != 'H') 
         anyoutf( 1, mbuf );
   }
   else
      anyoutf( 1, "CANNOT READ THE STORED VALUE." );


   /*--------------------------------------------------*/
   /* Read the new value.                              */   
   /*--------------------------------------------------*/   
   dfault = REQUEST;
   nitems = 1;
   if (slevel != 0)
   {
      sprintf( message,
              "Subset nr %d: New value for %.*s=    [do not change]",
               subnr,
               nelc_c( Itemname ), Itemname.a );
   }
   else
   {
      sprintf( message,
              "New value for %.*s=     [do not change]",
               nelc_c( Itemname ),
               Itemname.a );
   }
   Fres = 0;
   if (keytype == 'I')
      Fres = userint_c(  &intval, &nitems, &dfault,
                         KEY_VALUE, tofchar( message ) );
   else if (keytype == 'R')
      Fres = userreal_c( &realval, &nitems, &dfault,
                         KEY_VALUE, tofchar( message ) );
   else if (keytype == 'D')
      Fres = userdble_c( &dbleval, &nitems, &dfault,
                         KEY_VALUE, tofchar( message ) );
   else if (keytype == 'L')
      Fres = userlog_c(  &logval, &nitems, &dfault,
                         KEY_VALUE, tofchar( message ) );
   writeval = (Fres > 0);


   if (keytype == 'C')
   {
      Fres = usertext_c( Charval, &dfault,
                         KEY_VALUE, tofchar( message ) );
      writeval = (Fres > 0);                  /* Is there something to write? */
      if (writeval)
      {
         if (needuchar( Itemname ))                 /* Uppercase text or not? */
         {
            int   p;
            for (p = 0; p < (int) nelc_c(Charval); p++)   /* Uppercase this text */
               Charval.a[p] = toupper(Charval.a[p]);
         }
      }
   }

   
   if (keytype == 'T')
   {
      fint   writebytes;
      fint   foundbytes;
      fint   start;

      start = 1;
      sprintf( message,
              "Give value for %.*s     [do not change]",
               nelc_c( Itemname ), Itemname.a );
      memset( Charval.a, SPACE, CHARLEN );
      Fres = usertext_c( Charval, &dfault,
                         KEY_VALUE, tofchar( message ) );
      writeval = (Fres > 0);
      if (writeval)
      {
         writebytes = Fres;
         err1 = 0;
         gdsd_delete_c( Setin, 
                        Itemname, 
                        &slevel, 
                        &err1 );
         if (err1 < 0)
         {
            anyoutf( 1, "Problems deleting item [%.*s]. Reason:",
                     nelc_c(Itemname), Itemname.a );
            displayGDSerror( 1, err1 );
            return;
         }
         gdsd_writec_c( Setin, 
                        Itemname, 
                        &slevel, 
                        Charval,
                        &writebytes, 
                        &start, 
                        &foundbytes, 
                        &err1 );
         if (err1 < 0)
         {
            anyoutf( 1, "Problems writing item [%.*s]. Reason:",
                     nelc_c(Itemname), Itemname.a );
            displayGDSerror( 1, err1 );         
         }
      }
      writeval = 0;
   }


   if ((keytype == 'H') && (histcount > 0))
   {
      fint   nitems;
      int    len;
      int    next;
      fint   linenumber;           /* Number of the line to be changed */
      fint   dfault;
      fint   Fres;
      fchar  Dummystr;
      int    ptr_offset;           /* Pointer offset in 'storestr' */


      fmake( Dummystr, BIGSTORE );
      nitems   = 1;
      dfault   = REQUEST;
      writeval = 0;                /* Later, if line is changed, increase writeval */
      do 
      {
      	 cancel_c( tofchar("LINE=") );
         Fres = userint_c( &linenumber, 
                           &nitems, 
                           &dfault, 
                           tofchar( "LINE=" ),
                           tofchar( "Give number of line to change:    [stop]") );
         if (Fres == 0)
            next = 0;        
         else 
         /*--------------------------------------------------*/
         /* User wants to change a line. Check whether the   */
         /* given line number really exists.                 */
         /*--------------------------------------------------*/
         {
            next = 1;
            if (linenumber > 0 && linenumber <= histcount) 
            {
               Fres = usertext_c( Dummystr, 
                                  &dfault, 
                                  tofchar( "VALUE=" ),
                                  tofchar( "Give text for history:    [do not change]" ) );

               /* If a new text is entered, replace associated stored */
               /* string with the new one */

               if (Fres > 0) 
               {
                  writeval++;               /* Now there is something changed */
                  len = (int) nelc_c( Dummystr );
                  Dummystr.a[len] = '\0';
                  ptr_offset = (int) (linenumber - 1) * BIGSTORE;
                  strcpy( storestr + ptr_offset, Dummystr.a );
               }
	       cancel_c( tofchar( "VALUE=" ) );
            }
            else
               anyoutf( 1, "NUMBER NOT IN RANGE!" );
         }
      } 
      while (next);
   }

   cancel_c( KEY_VALUE );

   /*--------------------------------------------------*/
   /* Write the new value for the IRDLC & H types.     */
   /* The T type is already written.                   */
   /*--------------------------------------------------*/
   if (writeval)                                  /* Some value is specified */
   {
      err1 = 0;
      if (keytype == 'I')
         gdsd_wint_c(  Setin, Itemname, &slevel, &intval, &err1 );
      if (keytype == 'R') 
         gdsd_wreal_c( Setin, Itemname, &slevel, &realval, &err1 );
      if (keytype == 'L') {
         gdsd_wlog_c(  Setin, Itemname, &slevel, &logval, &err1 );
      }
      if (keytype == 'C') {
         gdsd_wchar_c( Setin, Itemname, &slevel, Charval, &err1 );
      }         
      if (err1 < 0)
      {
         anyoutf( 1, "Problems writing contents type '%c', for [%.*s]. Reason:",
                  keytype, nelc_c(Itemname), Itemname.a );
         displayGDSerror( 1, err1 );
         return;
      }

      if (keytype == 'D') 
      {
         gdsd_wdble_c( Setin, Itemname, &slevel, &dbleval, &err1 );
         /*----------------------------------------*/
         /* Special care for the CRPIX item. First */
         /* determine the number following CRPIX.  */
         /* Use this number to determine the name  */
         /* of the corresponding axis. Use the     */
         /* 'extend' routine to shift the origin.  */
         /* This is an addition to the header      */
         /* update!                                */
         /*----------------------------------------*/
         if (strstr(Itemname.a, "CRPIX") != NULL) 
         {
            int    len;
            int    i;
            fint   err3;
            fint   number;
            char   digitstr[4];
            fchar  Axname;

            len = (int) nelc_c( Itemname );         /* Length of item string */
            for (i = 5; i < len; i++ )
               digitstr[i-5] = Itemname.a[i];       /* Copy digits to another string */

            digitstr[i-5] = '\0';
            number = (fint) atoi( digitstr );        /* Convert string to integer */
            fmake(Axname, 20);
            err3 = 0;
            gdsc_name_c( Axname, Setin, &number, &err3 );
            err3 = 0;
            gds_extend_c( Setin, Axname, &dbleval, NULL, &err3 );
            if (err3 < 0) 
            {
               anyoutf( 1, "Problems updating axis [%.*s]. Reason:",
                        nelc_c(Axname), Axname.a );
               displayGDSerror( 1, err3 );
            }
         }
      }
      
      
      if (keytype == 'H') 
      /*--------------------------------------------------*/
      /* First delete all existing history lines at this  */
      /* level, before re-writing.                        */
      /*--------------------------------------------------*/            
      {
         int m;

         err1 = 0;
         gdsd_delete_c( Setin, Itemname, &slevel, &err1 );
         if (err1 < 0)
         {
            anyoutf( 1, "Problems deleting item [%.*s]. Reason:",
                     nelc_c(Itemname), Itemname.a );
            displayGDSerror( 1, err1 );
         }
         for (m = 0; m < histcount; m++)
         {
            err1 = 0;
            Charval.a = storestr + m * BIGSTORE;
            Charval.l = BIGSTORE;
            gdsd_wvar_c( Setin, Itemname, &slevel, Charval, &err1 );
            if (err1 < 0)
            {
               anyoutf( 1, "Problems deleting item [%.*s]. Reason:",
                        nelc_c(Itemname), Itemname.a );
               displayGDSerror( 1, err1 );
            }
         }
         free( storestr );               /* Free memory alocated with realloc */
      }
   }

   /* For the types defined above, it is possible to change */
   /* or add a comment */

   if (strpbrk( &keytype, "IRDLC" ) != NULL)
   /*--------------------------------------------------*/
   /* For the types defined above, it is possible to   */
   /* change or add a comment. To get a default, try   */
   /* to extract the existing comment.                 */
   /*--------------------------------------------------*/   
   {
      fchar   Commentstr;

      fmake( Commentstr, BIGSTORE );
      err1 = 0;
      gdsd_rfits_c( Setin, Itemname, &slevel, Charval, &err1 );
      if (err1 < 0)
      {
          anyoutf( 1, "Problems reading old comment from item [%.*s]. Reason:",
                   nelc_c(Itemname), Itemname.a );
          displayGDSerror( 1, err1 );
      }
      else
      {
         strcpy( Commentstr.a, &Charval.a[COMMENTSTART-1] );
         anyoutf( 1, "OLD COMMENT: %.*s", 
                  nelc_c(Commentstr), Commentstr.a  );
         Fres = usertext_c( Commentstr,           /* Get user's comment */
                            &dfault,
                            KEY_COMMENT,
                            tofchar( "New comment:              [do not change]" ) );
         cancel_c( KEY_COMMENT );
         if ((Fres != 0) || (dfault == HIDDEN))
            addcomment( Setin, slevel, Itemname, Commentstr );
      }
   }
}



static void delitem( fchar Itemname, 
                     fint  slevel )
/*------------------------------------------------------------*/
/* PURPOSE: Delete item on specified level.                   */
/* Setin is the name of the current set and is known globally.*/
/*------------------------------------------------------------*/
{
   fint err1;

   err1 = 0;
   gdsd_delete_c( Setin, Itemname, &slevel, &err1 );
   if (err1 < 0)
   {
      anyoutf( 1, "Problems deleting item [%.*s]. Reason:",
               nelc_c(Itemname), Itemname.a );
      displayGDSerror( 1, err1 );
   }
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/* Known keywords are stored in a list (item list). Keywords associated    */
/* with a set are stored in a descriptor file (header). The routine        */
/* 'special_keys' contains a list with special structure related keywords. */
/*-------------------------------------------------------------------------*/
{
   fint     nsubs;
   fint     recordnum;                     /* Used in gdsd_find_c to indicate index of the rec. */
   fint     slevel;
   fint     err;
   fchar    Itemname;                      /* Name of a header item */
   fchar    Descrtype;                     /* One of Char/Int/Real/Dble/Log/Text/Hist */
   fchar    Descrname;                     /* Name of the FITS item */
   fchar    Mes;
   fchar    Operationmode;                 /* One of Add/Change/Delete */
   char     itemstr[FITSLEN];


   init_c();                               /* contact Hermes */

   /* Task identification */
   {
      fchar    Ftask;                      /* Name of current task */
      fmake( Ftask, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Ftask );                   /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }


   fmake(Setin, BIGSTORE);

   dfault  = NONE;
   subdim  = 0;
   scrnum  = 3;

   nsubs   = gdsinp_c( Setin,
                       subsin,
                       &Fmaxsubs,
                       &dfault,
                       tofchar("INSET="),
                       tofchar("Give set (, subsets) to work on: " ),
                       &scrnum,
                       Faxnum,
                       Faxcount,
                       &Fmaxaxes,
                       &Fclass,
                       &subdim );

   setdim   = gdsc_ndims_c( Setin, &setlevel );

   toplevel = (setdim == subdim);
   if (toplevel)
      anyoutf( 1, "FIXHED working at top level" );
   else
      anyoutf( 1, "FIXHED working at subset level" );

   fmake( Itemname, FITSLEN );
   fmake( Descrname, FITSLEN );                    /* Name of item found with gdsd_find */
   fmake( Operationmode, 8 );

   strcpy( Operationmode.a, "UNKNOWN" );                           /* Initialize mode */
   anyoutf( 1, "Use ITEM=HEAD to get a list of ALL descriptors on current level.");
   anyoutf( 1, "Use ITEM=LIST to get a list of recommended GIPSY keywords" );

   dfault = REQUEST;
   nitems = 1;
   Mes    = tofchar("Give header item name:      [show header]");
   (void) str2char( "HEAD", Itemname );
   /*------------------------------------------------------------*/
   /* Start loop over user given descriptor names. Process       */
   /* names until <enter> was pressed.                           */
   /* NOTE that 'Itemname' cannot be modified because it will be */
   /* used in several places. So do not include a 0 to convert   */
   /* it to a C string.                                          */
   /*------------------------------------------------------------*/
   usercharu_c( Itemname, &nitems, &dfault, KEY_ITEM, Mes );
   do
   {
      int      attempt = 1;                      /* Is it possible to continue item loop ? */
      cancel_c( KEY_ITEM );
      strncpy( itemstr, Itemname.a, nelc_c(Itemname) );
      itemstr[nelc_c(Itemname)] = '\0';
      if ( strlen(itemstr) > 8 )
      {
      	 attempt = 0;
      	 anyoutf( 1, "Item name longer than 8 characters!");
      }
      if ( strncmp("LIST", itemstr, 4) == 0 )    /* User wants a list */
      {
         menu();                                 /* Give list with keywords */
         attempt = 0;                            /* Nothing found in list */
      }
      if ( strncmp("HEAD", itemstr, 4) == 0 )    /* User wants header */
      {
         general_header( Setin, &nsubs, subsin, Faxnum, &setdim, &subdim );
         attempt= 0;                             /* Nothing from list */
      }
      if (attempt)
      {
         inlist = match( itemstr, &itemnum );    /* Try to match user input */
         if (inlist == 0)                        /* Not in list */
            anyoutf( 16, "%s: Not in keyword list!", Itemname.a );

         if (inlist == 1)                        /* Give warning if level is suspicious */
         {
            if (toplevel && (descrip[itemnum].level == 1))
               anyoutf( 1, "You're supposed to operate on subset level now!");
            if ((!toplevel) && (descrip[itemnum].level == 0))
               anyoutf( 1, "You're supposed to operate on top level now!");
         }
      }
      if (attempt)
      /*------------------------------------------------------------*/
      /* Now check whether this descriptor item could be found in   */
      /* the header.                                                */
      /*------------------------------------------------------------*/
      {
         int   hit;           /* Number of times same item is found in header */

         inhead_set = 0;                         /* Occurrences at set level */
         inhead_sub = 0;                         /* Occurrences at subset level */
         recordnum  = 0;                         /* Position in header file */
         hit = 0;

         do
         /*--------------------------------------------------*/
         /* Examine whole descr. for occurrences, i.e. repeat*/
         /* a call to gdsd_find until 'recordnum' gets the   */
         /* value 0.                                         */
         /*--------------------------------------------------*/
         {
            slevel = 0;
            gdsd_find_c( Descrname, Setin, NULL, &recordnum, &slevel );
            if (slevel < 0)
            {
               anyoutf( 1, "Error reading descriptor. Reason:" );
               displayGDSerror( 1, slevel );
               break;
            }
            Descrname.a[nelc_c(Descrname)] = '\0';        /* Make a C string */
            if (recordnum != 0)
            {
               if ( strcmp(Descrname.a, itemstr) == 0 )
               {
                  hit++;
                  if (slevel == 0) inhead_set++;            /* Set level */
                  if (slevel >  0) inhead_sub++;            /* Subset level */
               }
            }
         }
         while (recordnum != 0);

         if (hit > 1)
         {
            anyoutf( 1, "%s was found in header %d times", itemstr, hit );
            anyoutf( 1, "Only last hit is used!" );
         }
      }

      /* Check whether structure related item is forbidden or  */
      /* incorrect specified */

      if (attempt)
         attempt = special_keys( Itemname, toplevel, &hidaxis );
      /*------------------------------------------------------------*/
      /* The keyword TYPE= is used to determine the FITS item type. */
      /* Standard types are INT, REAL, DBLE, LOG and CHAR. Non-     */
      /* standard types are TEXT as used in APSET for example, and  */
      /* HIST for items like comment and history.                   */
      /*------------------------------------------------------------*/
      if (attempt)
      {
         fmake( Descrtype, 4 );
         Descrtype.a[0] = 'Q';                          /* Set type to dummy */
         if (inlist)
            strcpy( Descrtype.a, descrip[itemnum].type );      /* Fixed type */
         else
         {
            notype = 1;
            if (inhead_set)
            {
               /* Item in header (at set level), but not in list */
               err = 0;
               gdsd_type_c( Descrtype, Setin, Itemname, &toplevel, &err );
               notype = (err < 0);
               if (notype)
               {
                  anyoutf( 1, "Error reading descriptor type. Reason:" );
                  displayGDSerror( 1, err );
               }
               else
               {
                  anyoutf( 16, "Read %s as type : %.*s",
                           Itemname.a,
                           nelc_c(Descrtype), Descrtype.a );
               }
            }
            if (inhead_sub && !(inhead_set))
            /*--------------------------------------------------*/
            /* Item in header at subset level, but not in list. */
            /* To determine its type, make use of gdsd_type     */
            /* routine for a second attempt.                    */
            /*--------------------------------------------------*/
            {
               err = 0;
               m = 0;
               notype = 0;
               do
               {
                  slevel = subsin[m++];
                  err = 0;
                  gdsd_type_c( Descrtype, Setin, Itemname, &slevel, &err );
                  if (err < 0)
                  {
                     anyoutf( 1, "Error reading descriptor type. Reason:" );
                     displayGDSerror( 1, err );
                  }
                  notype = (err < 0);
                }
                while ( notype && (m<nsubs-1) );
            }
            if (notype)
            {
               dfault1 = REQUEST;
               nitems = 1;
               do
               {
                  fchar Mes1;
                  Mes1 = tofchar("Give type of item I/R/D/L/C/T/H:     [back to item loop]");
                  Fres = usercharu_c( Descrtype,
                                      &nitems,
                                      &dfault1,
                                      KEY_TYPE,
                                      Mes1
                                    );
                  agreed = ( strchr("IRDLCTH", Descrtype.a[0]) != NULL );
                  if (!agreed)
                  {
                     if (Fres == 0)
                        attempt = 0;
                     else
                        reject_c( KEY_TYPE, tofchar("Not a valid type!") );
                  }
               }
               while (!agreed && attempt);
            }
         }                                 /* End of 'item not in list' block */
      }                                                /* End of type request */
      cancel_c( tofchar("TYPE=") );

      if (attempt)
      /*--------------------------------------------------*/
      /* The MODE= keyword is used to determine whether an*/
      /* item has to be Changed, Added or Deleted. The    */
      /* keyword is asked until a correct mode is         */
      /* specified.                                       */
      /*--------------------------------------------------*/
      {
         fchar   Mes1;

         dfault1 = HIDDEN;
         nitems  = 1;
         Mes1    = tofchar("Operation mode: A(dd),D(elete),C(hange):  [A]");
         agreed  = 1;

         /*--------------------------------------------------*/
         /* If an item was found in the header, set default  */
         /* mode to 'change'. If nothing was found, set      */
         /* default to 'add'.                                */
         /*--------------------------------------------------*/

         Operationmode.a[0] = 'A';           /* Not in header on this level, add item */

         /* If item is found in the header, change to 'change' mode, except */
         /* if item type was history. Adding in this case means appending! */

         if (!(Descrtype.a[0] == 'H'))
         {
            if (inhead_set &&  toplevel)
               Operationmode.a[0] ='C';
            if (inhead_sub && !toplevel)
               Operationmode.a[0] ='C';
         }

         do
         {
            agreed = 1;
            Fres = usercharu_c( Operationmode,
                                &nitems,
                                &dfault1,
                                tofchar("MODE="),
                                Mes1 );

            /* First time, the keyword is asked unhidden. If there is */
            /* some kind of conflict, the user can try again to set the */
            /* mode and this time the keyword is not hidden. */
            /* Abort the mode loop if the user presses return when he */
            /* has to specify something at the MODE= prompt. */

            if ((dfault1 == REQUEST) && (Fres == 0))
               attempt = 0;                                        /* Abort */
            if (attempt)
            {
               if ( ( (inhead_set && toplevel) ||
                      (inhead_sub && !toplevel) ) &&
                       (Descrtype.a[0] != 'H') &&
                       (Operationmode.a[0] == 'A') )
               {
                  /* Cannot add what is already there except for history */
                  agreed = 0;
                  anyoutf( 1, "You can only (C)hange or (D)elete this item." );
                  Mes1 = tofchar("Give operation mode (C/D)   [stop]");
               }
               if ( ( (toplevel && !inhead_set) ||
                     (!toplevel && !inhead_sub) ) &&
                     ((Operationmode.a[0] == 'D') ||
                     (Operationmode.a[0] == 'C')) )
               {
                  /* Cannot change what isn't there */
                  agreed = 0;
                  if (Operationmode.a[0] == 'D')
                     anyoutf( 1, "Item not in header at this level, cannot delete." );
                  if (Operationmode.a[0] == 'C')
                     anyoutf( 1, "Item not in header at this level, cannot change." );
                  Mes1 = tofchar("Operation mode:    A(dd)/[stop]");
               }
               if (!(strpbrk( Operationmode.a, "ACD" )))
               {
                  /* Mode is not one of Add/Change/Delete */
                  agreed = 0;
                  anyoutf( 1, "Unknown mode" );
                  if (inhead_set || inhead_sub)
                     Mes1 = tofchar("Give operation mode (C/D)  [stop]");
                  else
                     Mes1 = tofchar("Give operation mode (A/C/D)  [stop]");
               }

               /* The mode input was not appropriate or valid, so ask again */
               /* but now with the keyword unhidden. */

               if (!agreed)
               {
                  dfault1 = REQUEST;
                  cancel_c( tofchar("MODE=") );
               }
            }
         } while (!agreed && attempt);
      }                                                     /* End of attempt */

      if (attempt)
      {
         char  modetxt[7];
         char  leveltxt[7];
         char  typetxt[30];
         int   cont;

         /* Create text for information about selected item */

         anyoutf( 1, " " );
         anyoutf( 1, "============================= FIXHED ========================");

         if (Operationmode.a[0] == 'A')
            strcpy( modetxt, "ADD" );
         if (Operationmode.a[0] == 'C')
            strcpy( modetxt, "CHANGE" );
         if (Operationmode.a[0] == 'D')
            strcpy( modetxt, "DELETE" );
         if (toplevel)
            strcpy( leveltxt, "set" );
         else
            strcpy( leveltxt, "subset" );

         switch (Descrtype.a[0])
         {
            case 'I' : {
               strcpy( typetxt, "an integer" );
            } break;
            case 'R' : {
               strcpy( typetxt, "a real" );
            } break;
            case 'D' : {
               strcpy( typetxt, "a double" );
            } break;
            case 'L' : {
               strcpy( typetxt, "a logical" );
            } break;
            case 'C' : {
               strcpy( typetxt, "a character string" );
            } break;
            case 'T' :
            case 'H' : {
               strcpy( typetxt, "text (strings)" );
            } break;
            default : strcpy( typetxt, "???" );
         }
         anyoutf( 1, "FIXHED tries to %s item [%.*s] at %s level.",
                  modetxt,
                  (int) nelc_c(Itemname), Itemname.a,
                  leveltxt );
         if (inlist)
         {
            anyoutf( 1, "Item %.*s (%s) is known",
                     (int) nelc_c(Itemname), Itemname.a,
                     descrip[itemnum].meaning );
            anyoutf( 1, "and stored as %s.", typetxt );
         }
         else
            anyoutf( 1, "The contents is stored as %s.", typetxt );

         if (!inlist)
            anyoutf( 1, "Item NOT found in list with general keywords." );
         if (inhead_set && !inhead_sub)
            anyoutf( 1, "Item found in HEADER at SET level." );
         if (inhead_sub && !inhead_set)
            anyoutf( 1, "Item found in HEADER at SUBSET level." );
         if (inhead_sub && inhead_set)
            anyoutf( 1, "Item found in HEADER at SET and SUBSET level." );
         if (!(inhead_set || inhead_sub))
            anyoutf( 1, "Item not found in HEADER." );
         anyoutf( 1, "=============================================================");

         /* Do the adding/changing/deleting */

         switch (Operationmode.a[0])
         {
            case 'A' : {
               if (toplevel)
               {
                  dfault_remote = REQUEST;
                  slevel = 0;
                  cont = additem( Itemname, Descrtype.a[0],
                                  slevel, dfault_remote, 0 );
               }
               else
               {
                  int   j;

                  nitems = 1;
                  all = toflog( 0 );                      /* Reset to false */
                  dfault1 = HIDDEN;
                  dfault_remote = REQUEST;
                  cont = 1;
                  for (j = 0; ((j < nsubs) && cont); j++)
                  {
                     cont = additem( Itemname, Descrtype.a[0],
                                     subsin[j], dfault_remote, j );
                     Fres = userlog_c( &all, &nitems, &dfault1,
                                       KEY_ALL,
                                       tofchar("Apply to all remaining levels?   Y/[N]") );
                     all = tobool( all );

                     /* If user selected ALL=Y, all remaining subsets */
                     /* will get the same value. */

                     if (all) dfault_remote = HIDDEN;
                  }
                  cancel_c( KEY_ALL );
               }
               cancel_c( KEY_COMMENT );
            } break;
            case 'C' :
            {
               if (toplevel)
               {
                  slevel = 0;
                  chgitem( Itemname, Descrtype.a[0], slevel, 0, hidaxis );
               }
               else
               {
                  int j;
                  for (j = 0; j < nsubs; j++)
                  {
                     chgitem( Itemname, Descrtype.a[0], subsin[j], j, hidaxis );
                  }
               }
               cancel_c( KEY_COMMENT );
            } break;
            case 'D' :
            {
               /*--------------------------------------------------*/
               /* Deleting at top level has two modes: first, it   */
               /* is possible to delete the selected item on the   */
               /* current level only. Second, it is possible to delete the */
               /* selected item on all levels at once by specifying*/
               /* Y at the unhidden ALL= prompt. If deleting at    */
               /* subset level, confirmation is asked for each     */
               /* selected subset. If specifying the hidden keyword*/
               /* ALL=Y, no confirmation is asked and the item is  */
               /* deleted for all subsequent subsets.              */
               /*--------------------------------------------------*/
               if (toplevel)
               {
                  nitems = 1;
                  dfault1 = REQUEST;
                  all = toflog( false );                      /* Reset to false */
                  Fres = userlog_c( &all,
                                    &nitems,
                                    &dfault1,
                                    KEY_ALL,
                                    tofchar("Delete item on ALL levels?   Y/[N]") );
                  all = tobool( all );
                  if (all)
                  {
                     err = 0;
                     gdsd_delall_c( Setin, Itemname, &err );
                     cancel_c( KEY_ALL );
                  }
                  else
                  {
                     slevel = 0;
                     delitem( Itemname, slevel );
                  }
               }
               else
               {
                  int j;

                  nitems = 1;
                  all = toflog( false );                      /* Reset to false */
                  cont = 1;
                  anyoutf( 1, "Deleting item on specified subset levels");
                  for (j = 0; ((j < nsubs) && cont); j++)
                  {
                     dfault1 = HIDDEN;
                     Fres = userlog_c( &all, &nitems, &dfault1,
                                       KEY_ALL, tofchar("Delete on all remaining levels?   Y/[N]") );
                     if (all)
                        delitem( Itemname, subsin[j] );
                     else
                     {
                        bool ok;
                        char okstr[50];

                        sprintf( okstr,
                                "Subset nr %d: Ok to delete item?   Y/N/[stop]",
                                 j );
                        dfault1 = REQUEST;
                        Fres = userlog_c( &ok,
                                          &nitems,
                                          &dfault1,
                                          tofchar("OK="),
                                          tofchar( okstr ) );
                        if (Fres == 0)
                           cont = 0;                         /* Abort loop */
                        else
                        {
                           if (ok)
                              delitem( Itemname, subsin[j] );
                        }
                        cancel_c( tofchar("OK=") );
                     }
                  }
               }
               cancel_c( KEY_ALL );
            }
            break;
            default :  anyoutf( 1, "Unknown mode" );
         }
      }                                                      /* Attempt was 1 */
      if (Operationmode.a[0] != 'U')       /* Alter item message if mode is used once */
      {
         sprintf( messbuf,
                 "(MODE=%1.1s) Give header item name:    [end program]",
                  Operationmode.a );
         Mes = tofchar( messbuf );
      }
      else
         Mes = tofchar("Give header item name:    [end program]");
   }
   while( usercharu_c(Itemname, &nitems, &dfault, KEY_ITEM, Mes) );

   finis_c();
   return 1;                                                  /* Dummy return */
}

