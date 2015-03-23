/*
                            COPYRIGHT (c) 1995
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             plotsym.dc1

Program:       PLOTSYM

Purpose:       Plot PGPLOT's marker- and Hershey symbols

Category:      PLOTTING, UTILITY

File:          plotsym.c

Author:        M.G.R. Vogelaar

Keywords:


   GRDEVICE=   Plot device:                           [LIST OF DEVICES]
               Destination of plot, Screen or Hardcopy.

   OPTION=     1=Plot markers, 2=Plot Hershey symbols:              [1]
               1: Plot one page with rows x columns graph markers 
                  starting with marker index -1.
               2: Plot Hershey symbols in pages with rows x columns 
                  symbols. Boxes with undefined Hershey symbols are not 
                  displayed, so the symbol numbers are not consecutive.
                  Page numbers are set with PAGENR=.
                     
   ROWCOL=     Give number of rows, columns to display:    [CALCULATED]
               One page has rows x columns boxes with symbols.
   
   CHARHEI=    Give char.height in mm for symbols,numbers: [CALCULATED] 
               In the default situation, both symbol- and number
               heights are the same.
               
** PGWIDTH=    Give line width 1..21:                               [1]
               Width for characters, symbols and lines.
               
** PGFONT=     Give font 1..4:                                      [2]
               1: (default) a simple single-stroke font ("normal" font)
               2: roman font
               3: italic font
               4: script font
                
   PAGENR=     Give page number <= %d (0=exit):             [NEXT PAGE]   
               This keyword is used to browse through the plot
               pages for OPTION=2 (Hershey symbols).


  
Description:   A Graph Marker is a symbol, such as a cross, dot, or circle, 
               drawn on a graph to mark a specific point. Usually the symbol 
               used will be chosen to be symmetrical with a well-defined 
               center. In most GIPSY applications a marker is identified 
               by a number given with a keyword like SYMBOL=. 
               The symbol number can be: -1, to draw a dot of the 
               smallest possible size (one pixel); 0--31, to draw any one 
               of the symbols in Figure obtained with OPTION=1
               or 33--127, to draw the corresponding ASCII character  
               (the character is taken from the currently selected text 
               font); or >127, to draw one of the Hershey symbols in the 
               plot obtained with OPTION=2.

               Text in plots can contain special symbols. 
               The total number of different symbols available is about 1000. 
               Each symbol is composed of a set of vectors, based on 
               digitized type fonts devised by A. V. Hershey of the US Naval 
               Postgraduate School, and each symbol is assigned a
               number in the range 0--4000.
               Any character can be inserted in a text string using an escape 
               sequence of the form \(nnnn), where nnnn is the Hershey number.
               
Notes:         The Hershey symbols 0--127 are not the same symbols as the
               graph markers with corresponding numbers. For higher numbers
               all Hershey symbols can be used as graph markers.

Example:       

Updates:       Dec 22,  1994: VOG, Document created.

#<
*/

/*  plotsym.c: include files     */

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

/* PGPLOT includes */

#include    "pgbeg.h"        /* Begin PGPLOT, open output device. */
#include    "pgend.h"        /* Terminate PGPLOT. */
#include    "pgask.h"        /* Control new page prompting. */
#include    "pgslw.h"        /* Set line width.*/
#include    "pgsvp.h"        /* Set viewport (normalized device coordinates). */
#include    "pgsci.h"        /* Set color index. */
#include    "pgsch.h"        /* Set character height. */
#include    "pgpage.h"       /* Advance to new page. */
#include    "pgswin.h"       /* Set window. */
#include    "pgscf.h"        /* Set character font.*/
#include    "pgdraw.h"       /* Draw a line from the current pen position to a point.*/
#include    "pgmove.h"       /* Move pen (change current pen position).*/
#include    "pgpt.h"         /* Plot one or more points.*/
#include    "pgexist.h"      /* Does number correspond to a Hershey symbol? */


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
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define MAXHERSEY      3000
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0


/* PGPLOT variables */

const  fint  background  =  0;      /* Color definitions for PGPLOT. */
const  fint  foreground  =  1;      /* Black if background is white. */
const  fint  red         =  2;
const  fint  green       =  3;
const  fint  blue        =  4;
const  fint  cyan        =  5;
const  fint  magenta     =  6;
const  fint  yellow      =  7;
const  fint  orange      =  8;
const  fint  greenyellow =  9;
const  fint  greencyan   = 10;
const  fint  bluecyan    = 11;
const  fint  bluemagenta = 12;
const  fint  redmagenta  = 13;
const  fint  darkgray    = 14;
const  fint  lightgray   = 15;

/* Miscellaneous */

static char     message[120];       /* All purpose character buffer. */


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



static int hardcopydevice( void )
/*------------------------------------------------------------------*/
/* Is current plot device a hard copy device?                       */
/*------------------------------------------------------------------*/
{
   fint    len;
   fchar   Inquiry;


   fmake( Inquiry, 4 );                /* Return value can be YES or NO only */
                                       /* The number of characters returned  */
                                       /* in 'Inquiry' is 'len' */
   pgqinf_c( tofchar( "HARDCOPY" ), 
             Inquiry, &len );
   if (len == 3)                       /* If 'YES' is returned */
   {
      anyoutC( 16, "Device is a hard copy device" );
      return( YES );
   }
   else 
   {
      anyoutC( 16, "Device is a screen device" );
      return( NO );
   }
}



void initplot( void )
/*------------------------------------------------------------------*/
/* Initialize plot software.                                        */
/*------------------------------------------------------------------*/
{
   fint   unit;            /* Ignored by pgbeg, use unit=0. */
   fchar  Devspec;         /* Device specification. */
   fint   nxysub[2];       /* Number of subdivisions on 1 page. */
   fint   nitems, dfault;
   fint   r1;
   fint   errlev = 4;      /* Set error level to fatal. */
   bool   pageoff;         /* Disable PGPLOT's NEXTPAGE keyword. */
   float  xl, xr, yb, yt;  /* Edges of the viewport. */


   /* Begin PGPLOT, open output device. A return value of 1 indicates */
   /* successful completion. There are 4 arguments for PGBEG:         */
   /* UNIT, this argument is ignored by PGBEG (use zero).             */
   /* FILE, If this argument is a question mark PGBEG will prompt the */
   /*       user to supply a string.                                  */
   /* NXSUB,the number of subdivisions of the view surface in X.      */
   /* NYSUB,the number of subdivisions of the view surface in Y.      */

   nxysub[1] = nxysub[0] = 1;           /* Default no subdivisions in plot.*/ 
   unit = 0;
   Devspec = tofchar("?");
   r1 = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );
   if (r1 != 1) 
      error_c( &errlev, tofchar("Cannot open output device") );

   /* No PGPLOT's NEXTPAGE= keyword */
   pageoff = toflog( NO );
   pgask_c( &pageoff );

   {
      /* Set the line width */
      fint    lwidth;
      
      if (hardcopydevice())
         lwidth = 2;
      else 
         lwidth = 1;
      nitems    = 1;
      dfault    = HIDDEN;
      (void) sprintf( message, "Give line width 1..21:          [%d]",
                      lwidth );
      r1 = userint_c( &lwidth, &nitems, &dfault, tofchar("PGWIDTH="),
                      tofchar( message ) );
      if (lwidth > 21) 
         lwidth = 21;
      if (lwidth < 1 ) 
         lwidth =  1;
      pgslw_c( &lwidth );
   }
     
   {
      /* Set the character font */
      fint    font;
      
      font   = 2;
      nitems = 1;
      dfault = HIDDEN;
      r1 = userint_c( &font, &nitems, &dfault, tofchar("PGFONT="),
                      tofchar("Give font 1..4:        [2]") );    
      if (font > 4) 
         font = 4;
      if (font < 1) 
         font = 1;
      pgscf_c( &font );                     /* Set font. */
   }
}



static void plmove( float x, float y )
/*------------------------------------------------------------*/
/*------------------------------------------------------------*/
{
   pgmove_c( &x, &y );      
}



static void pldraw( float x, float y )
/*------------------------------------------------------------*/
/*------------------------------------------------------------*/
{
   pgdraw_c( &x, &y );
}



static void plmark( float x, float y, fint symbol )
/*------------------------------------------------------------*/
/* Plot a marker at position x,y.                             */
/*------------------------------------------------------------*/
{
   fint   one = 1;
   pgpt_c( &one, &x, &y, &symbol );
}



static void plhtxt( float x, float y, char *txt )
/*------------------------------------------------------------*/
/* Plot a horizontal centered text at position x,y.           */
/*------------------------------------------------------------*/
{
   float just = 0.5;
   float angle = 0.0;
   
   pgptxt_c( &x, &y, &angle, &just, tofchar(txt) );
}



static void drawframe( float x1, float x2, float y2, 
                       float xoff, float yoff, 
                       float xstep, float ystep, 
                       int rows, int columns ,
                       char *title, fint color )
/*------------------------------------------------------------*/
/* Plot a frame and write title.                              */
/*------------------------------------------------------------*/
{
   int   i, j; 
   fint  oldcolor;
  
   
   pgqci_c( &oldcolor );
   pgsci_c( &color );   
   plmove( xoff, yoff );                 /* Draw frame */
   pldraw( xoff, y2 - yoff );
   pldraw( x2 - xoff, y2 - yoff );
   pldraw( x2 - xoff, yoff );
   pldraw( xoff, yoff );

     
   /* Lines of constant y */
   for (i = 0; i < rows; i++)
   {
      float   y;      
      y = y2 - yoff - ((float)i)*ystep;
      plmove( xoff, y );
      pldraw( x2 - xoff, y );
   }
   
   /* Lines of constant x */
   for (j = 0; j < columns; j++)
   {
      float  x;
      x = x = x1 + xoff + ((float)j)*xstep;
      plmove( x, yoff );
      pldraw( x, y2 - yoff );
   }
   plhtxt( x2/2.0, y2 - yoff + 2.0, title );  /* Title */   
   pgsci_c( &oldcolor );                 /* Reset colour */   
}   
   
  
 
static void plotmarkers( int rows, int columns, float *charhei )
/*------------------------------------------------------------*/
/* Plot markers with numbers -1 to 'rows*columns-2' in 'rows' */
/* rows and 'columns' columns. Put number of symbol below     */
/* the plotted symbol.                                        */
/*------------------------------------------------------------*/
{
   fint   mm = 2;
   float  nx1 = 0.0, nx2 = 1.0, ny1 = 0.0, ny2 = 1.0;
   float  x1, x2, y1, y2;
   float  xoff, yoff;
   float  xstep, ystep;
   float  h;
   int    i, j;
   int    symnr;
 

   /* Get the size of the device in mm */           
   pgsvp_c( &nx1, &nx2, &ny1, &ny2 );
   pgqvp_c( &mm, &x1, &x2, &y1, &y2 );
   pgswin_c( &x1, &x2, &y1, &y2 );
   
   /* From this moment on, all plotting is done in mm. */
   /* There are 'rows*columns' symbols. Arrange in x rows 
   /* of y columns. */
   
   xoff  = 10.0;                         /* In mm */
   yoff  = 15.0;
   xstep = (x2-x1-2.0*xoff) / (float)columns; 
   ystep = (y2-y1-2.0*yoff) / (float)rows;    
   symnr = -1;
   h = MYMIN(x2, y2) / 40.0;             /* The height in mm of one character */
   
   /* Convert character height from mm to scales. */
   for (i = 0; i < 2; i++)
   {
      if (charhei[i] == 0.0)
         charhei[i] = ystep / 5.0;  
      charhei[i] = ABS(charhei[i]) / h;      
   }
  
   pgbbuf_c();                           /* Start buffering */
   drawframe( x1, x2, y2, 
              xoff, yoff, 
              xstep, ystep, 
              rows, columns,
             "PGPLOT (marker) symbols:", 
             green );
  
   /* Plot the symbols */
   for (i = 0; i < rows; i++)
   {
      float   x, y;
      y = y2 - yoff - ((float)i+0.5)*ystep;
      for (j = 0; j < columns; j++)
      {
         x = x1 + xoff + ((float)j+0.5)*xstep;
         (void) sprintf( message, "%d", symnr );
         pgsch_c( &charhei[0] );                  /* Set character height */
         plmark( x, y, symnr++ );
         pgsch_c( &charhei[1] );  
         plhtxt( x, y-0.5*ystep+2.0, message );
      }      
   }
   pgebuf_c();
}



static void plothershey( int rows, int columns, float *charhei )
/*------------------------------------------------------------*/
/* Plot Hershey symbols per page in 'rows' rows and 'columns' */
/* columns. Put number of Hershey symbol below the plotted    */
/* character.                                                 */
/*------------------------------------------------------------*/
{
   fint   mm = 2;
   fint   nitems, dfault;        
   float  nx1 = 0.0, nx2 = 1.0, ny1 = 0.0, ny2 = 1.0;
   float  x1, x2, y1, y2;
   float  xoff, yoff;
   float  xstep, ystep;
   float  h;
   int    i, j, symnr;
   fint   pagenr, basenr;
   fint   *symbolnr = NULL;
   int    maxsymbols;
   int    maxpages;
   int    quit;
   int    hardcopy;
   fint   snr;
 

   symbolnr = (fint *) calloc( MAXHERSEY + 1, sizeof(fint) );
   if (symbolnr == NULL)
   {
      anyoutC( 1, "Cannot allocate space for index array" );
      return;
   }
   for (i = 1, snr = 1; snr <= MAXHERSEY; snr++) 
      if (pgexist_c(&snr))
         symbolnr[i++] = snr;
   maxsymbols = i - 1;
   
  
   /* Get the size of the device in mm and set window in mm */           
   
   pgsvp_c(  &nx1, &nx2, &ny1, &ny2 );
   pgqvp_c(  &mm, &x1, &x2, &y1, &y2 );
   pgswin_c( &x1, &x2, &y1, &y2 );
  
  
   /* From this moment on, all plotting is done in mm. */

   xoff  = 10.0;                         /* In mm */
   yoff  = 15.0;   
   xstep = (x2-x1-2.0*xoff) / (float)columns; 
   ystep = (y2-y1-2.0*yoff) / (float)rows;    
   symnr = -1;
   h = MYMIN(x2, y2) / 40.0;             /* The height in mm of one character */
   
  
   /* Convert character height from mm to scales. */
   
   for (i = 0; i < 2; i++)
   {
      if (charhei[i] == 0.0)
          charhei[i] = ystep / 4.0;      
      charhei[i] = ABS(charhei[i]) / h;
   }
           
   hardcopy = hardcopydevice();
   pagenr   = 1;
   maxpages = (maxsymbols - 1) / (rows * columns) + 1;
   do  /* Loop over pages */
   {
      nitems = 1;
      dfault = REQUEST;      
      if (pagenr > maxpages)
         pagenr = 0;
      if (!hardcopy)
      {
         if (pagenr)
            (void) sprintf( message, 
                           "Give page number <= %d (0=exit):   [%d]", 
                            maxpages, pagenr );
         else
            (void) sprintf( message, 
                           "Give page number <= %d (0=exit):   [EXIT]", 
                            maxpages, pagenr );      
         (void) userint_c( &pagenr, &nitems, &dfault, tofchar("PAGENR="),
                           tofchar(message) );
         cancel_c( tofchar("PAGENR=") );
      }
      quit   = (pagenr == 0);
      pagenr = MYMIN( pagenr, maxpages );
      basenr = rows * columns * (pagenr-1) + 1;
      if (!quit)
      {
         pgpage_c();         
         pgbbuf_c();
         (void) sprintf( message, "PGPLOT Hershey symbols  %d", pagenr );
         drawframe( x1, x2, y2, 
                    xoff, yoff, 
                    xstep, ystep, 
                    rows, columns,
                    message, 
                    green );
         for (i = 0; i < rows; i++)
         {
            float   x, y;
            y = y2 - yoff - ((float)i+0.5)*ystep;
            for (j = 0; j < columns; j++)
            {               
               if (basenr <= maxsymbols)
               {
                  x = x1 + xoff + ((float)j+0.5)*xstep;
                  (void) sprintf( message, "\\(%04d)", symbolnr[basenr] );
                  pgsch_c( &charhei[0] );             /* Set character height */
                  plhtxt( x, y, message );
                  (void) sprintf( message, "%04d", symbolnr[basenr] );
                  pgsch_c( &charhei[1] );                   
                  plhtxt( x, y-0.5*ystep+2.0, message );
               }
               basenr++;                  
            }
         }
         pgebuf_c();
      }
      pagenr++;
   } while (!quit);
}




MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint    rowcol[2];
   float   charhei[2];
   fint    nitems, dfault;   
   fint    r1;
   fint    option;
   
   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }

   initplot();

   /* Make a selection. Plot the marker symbols on one page (auto scaling) */
   /* or plot the Hershey symbols per page.                                */

   nitems = 1;
   dfault = REQUEST;
   option = 1;
   (void) sprintf( message,
                  "1=Plot markers, 2=Plot Hershey symbols:   [%d]", option );
   (void) userint_c( &option, &nitems, &dfault, tofchar("OPTION="),
                     tofchar(message) );
   if (option != 2)
      option = 1;
  
   nitems = 2;
   dfault = REQUEST;
   if (option == 1)
      rowcol[0] = rowcol[1] = 6;
   else 
   {
      rowcol[0] = 12;
      rowcol[1] = 8;
   }
   (void) sprintf( message, 
                  "Give number of rows, columns to display:  [%d %d]",
                   rowcol[0], rowcol[1] );      
   (void) userint_c( rowcol, &nitems, &dfault, tofchar("ROWCOL="),
                     tofchar(message) );
   nitems     = 2;
   dfault     = REQUEST;
   charhei[0] = 0.0;
   charhei[1] = 0.0;   
   r1 = userreal_c( charhei, &nitems, &dfault, tofchar("CHARHEI="),
                    tofchar("Give char.height (in mm) for symbols, numbers: [calculated]") );   

   if (option == 1)
   {
      /* Plot the markers */
      plotmarkers( rowcol[0], rowcol[1], charhei );      
   }
   else 
   {
      /* Plot the Hershey symbols */
      plothershey( rowcol[0], rowcol[1], charhei );      
   }

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   pgend_c();
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
