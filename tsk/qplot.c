/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             qplot.dc1

Program:       QPLOT

Purpose:       QPLOT plots an one dimensional slice through a set.

Category:      ANALYSIS, PROFILES, PLOTTING

File:          qplot.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give set (, subset):
               Maximum number of subsets is 2048.

   FILENAME=   Write plot data to:                          [qplot.dat]
               Result table is written in Ascii in the file with this
               name. If the file already exists, the keyword OVERWRITE=
               is prompted.

   OVERWRITE=  File exists, ok to overwrite?                      [Y]/N


   STARTPOS=   Give start pos. (x1..xn):
               n is subset dimension. Start profile at this position.

   ENDPOS=     Give start pos. (y1..yn):
               n is subset dimension. End profile at this position.

   POINTS=     Number of points in plot:                           [50]
               Maximum is 8192

** XMINMAX=    Give Xmin, Xmax in plot:                    [calculated]

** YMINMAX=    Give Ymin, Ymax in plot:                    [calculated]

   CONTINUE=   C(ontinue), D(evice), Q(uit):                        [C]
               C: Continue, get nest positions.
               D: Continue but initialize plots again.
               Q: Quit program.



               Keywords to initialize plots:

** SUBDIV=     View surface subdivisions x,y:                     [1,1]
               It is possible to have more plots on the view surface.
               The number of plots can vary in both directions x and y.

   GRDEVICE=   Give graphics device to      [list of available devices]
               plot on:
               If you writing to a TEK window, open window first.

** PAPER=      Give width (cm), aspect ratio:                [0.0, 1.0]
               Change the size of the (output) view surface.
               The aspect ratio is defined as height/width.
               The default is a calculated size and aspect ratio 1.

** LINEWIDTH=  Give line width (1-21):                              [2]
               Only required if a hardcopy device is selected.



Description:   This program takes a one dimensional slice of an
               image selected by INSET=. The length and direction of
               the slice is fixed by two points given in STARTPOS=
               and ENDPOS= as X1, X2, .. Xn, and  Y1, Y2, ..Yn
               either in pixel or physical coordinates (n is the subset
               dimension). These keywords apply to all selected subsets.
               The set values at the nearest integer positions of the
               start- and endpoint will always be plotted. Interpolated
               positions will be rounded off to their nearest integer
               values. If there are pixel positions outside the subset,
               the program will warn you, but it
               will plot as long as there are pixels inside the subset.
               The number of positions to examine is specified with
               POINTS=
               Each time a plot is made, it is possible to select a
               new position until CONTINUE=Q which stops the program.
               CONTINUE=D selects a new device and reinitializes the
               plot software. SUBDIV= selects the number of plots on
               one view surface in x- and y direction. GRDEVICE=
               selects the device. The options can be obtained by
               pressing carriage return. PAPER= and LINEWIDTH= are
               important if your device is a hardcopy device.
               Plot data can be stored in a file (FILENAME=).
               Data of different plots sent to this file are separated
               by an empty line.

Updates:       Aug 19, 1991: VOG, Document created.
               Oct 10, 1991: WZ,  PGPLOT standard names implemented
               Jun, 9, 1993: VOG, Slices can contain blanks


#<

*/

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "math.h"
#include "cmain.h"
#include "gipsyc.h"
#include "ctype.h"
#include "init.h"
#include "finis.h"
#include "float.h"
#include "gdsinp.h"
#include "setfblank.h"
#include "myname.h"
#include "anyout.h"
#include "reject.h"
#include "cancel.h"
#include "nelc.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsc_ndims.h"
#include "gdsc_fill.h"
#include "gdsc_name.h"
#include "gdsi_read.h"
#include "gdsd_rchar.h"
#include "cotrans.h"
#include "error.h"
#include "stabar.h"
#include "axunit.h"
#include "userreal.h"
#include "userlog.h"
#include "userint.h"
#include "usertext.h"
#include "usercharu.h"
#include "presetd.h"
#include "minmax2.h"
#include "status.h"
#include "pgqinf.h"
#include "pgenv.h"
#include "pgbox.h"
#include "pgswin.h"
#include "pgbin.h"
#include "pgslw.h"
#include "pgsls.h"
#include "pglab.h"
#include "pgbeg.h"
#include "pgsch.h"
#include "pgqch.h"
#include "pgpap.h"
#include "pgend.h"
#include "pgpage.h"
#include "pgask.h"
#include "pgmtxt.h"
#include "pgline.h"
#include "pgsvp.h"
#include "pgpt.h"
#include "pgmove.h"
#include "pgdraw.h"
#include "pgsci.h"
#include "pgscf.h"
#include "gdspos.h"


#define AXESMAX    10               /* Max. allowed number of axes in a set */
#define SUBSMAX    2048             /* Max. number of substructures to be specified */
#define MAXBUF     4096             /* Buffer size for I/O */
#define MAXPOINTS  8192             /* Max num. of points in plot */
#define BIGSTORE   80               /* Length of a string */
#define VERSION    "1.0"            /* Version number of this program */
#define NONE       0                /* Default values for use in userxxx routines */
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define FULL_LINE  1
#define DASHED     2
#define DOTTED     4
#define false      0
#define true       1


/* Keywords and messages */

#define KEY_INSET         tofchar("INSET=")
#define MES_INSET         tofchar("Give set (and subset(s)): " )
#define KEY_ENDPOS        tofchar("ENDPOS=")
#define MES_ENDPOS        tofchar("Give end pos. (y1..yn):" )
#define KEY_STARTPOS      tofchar("STARTPOS=")
#define MES_STARTPOS      tofchar("Give start pos. (x1..xn):" )
#define KEY_POINTS        tofchar("POINTS")
#define MES_POINTS        tofchar("Number of positions to plot along slice:      [50]" )
#define KEY_CONTINUE      tofchar("CONTINUE=")
#define MES_CONTINUE      tofchar("C(ontinue), D(evice), Q(uit):   [C]" )
#define KEY_SUBDIV        tofchar("SUBDIV=")
#define MES_SUBDIV        tofchar("View surface subdivisions x,y:   [1,1]")
#define KEY_XMINMAX       tofchar("XMINMAX=")
#define MES_XMINMAX       tofchar("Give Xmin, Xmax in plot:    [calculated]" )
#define KEY_YMINMAX       tofchar("YMINMAX=")
#define MES_YMINMAX       tofchar("Give Ymin, Ymax in plot:    [calculated]" )
#define KEY_FILENAME      tofchar("FILENAME=")
#define MES_FILENAME      tofchar("Write plot data to:       [No file]")
#define KEY_OVERWRITE     tofchar("OVERWRITE=")
#define MES_OVERWRITE     tofchar("File exists, ok to overwrite?    [Y]/N")



/* Initialize string with macro */
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


#define MYMAX(a,b) ((a) > (b) ? (a) : (b))
#define MYMIN(a,b) ((a) > (b) ? (b) : (a))
/* Pre Apr 2009 def.: #define NINT(a)    ( (a)<0 ? (int)((a)-.5) : (int)((a)+.5) ) */
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )



/* Input of set, subsets: */

static fchar    Fsetin;                /* Name of the set */
static fint     Fsubin[SUBSMAX];       /* Array for the subset coordinate words */
static fint     FnsubsI;               /* Number of input subsets */
static fint     Fdfault;               /* Default option for input etc */
static fint     Faxnum[AXESMAX];       /* GDSINP axis numbers array */
static fint     Faxcount[AXESMAX];     /* GDSINP axis lengths array */
static fint     Fclass = 1;            /* Axis is operation axis */
static fint     Fsetdim;               /* Dimension of the set */
static fint     Fsubdim;               /* Dimension of the subset */
static fint     Fscrnum = 11;          /* Destination of log output */
static fint     Fmaxaxes  = AXESMAX;   /* Convert parameters to variables */
static fint     Fmaxsubs  = SUBSMAX;   /* Max. num. subsets in 'gdsinp' */
static int      i,m;                   /* Counters */
static fint     Fsetlevel = 0;         /* Indicate set level */


/* Input of area etc.:*/

static fint     Fcwlo;                 /* Coordinate words */
static fint     Fcwhi;
static fint     FgridLO[AXESMAX];      /* Grids for frame */
static fint     FgridHI[AXESMAX];
static fint     Bgrid[AXESMAX];        /* A grid position */


/* Data transfer: */

static fint     Fpixelsdone;
static fint     FtidIN;                /* Transfer id. */
static float    imageIN[MAXBUF];       /* Multiple buffer for all subsets */
static fint     Fpixels;

/* Related to update of header etc: */

static fchar    Fdataunits;            /* Units of data in the set */


/* Miscellaneous: */

static int      subcount;              /* Subset counter */
static fint     Fr1, Fr2;              /* Results of userxxx routines */
static float    blank;                 /* Value of system blank */
static fint     nblanks;               /* Number of blanks in profile */
static fint     Fmaxtoread;            /* Max num pixels in 1 read action */
static fint     Fpoints;
static double   stepsize;
static fint     Fmaxpos;
static double   lambda;
static float    Xdata[MAXPOINTS];
static fint     Fcontinue;
static char     messbuf[BIGSTORE];
static fint     Fnumitems;
static fchar    Fcont;
static int      outside;
static int      numoutside;
static int      tofile;
FILE            *fpOUT;               /* File for table data */
static int      agreed;

/* 'pgplot' related */

static fint     Fstyle;                /* Line style attribute, sect. 5.4 PGPLOT */
static float    Xmin, Xmax, Ymin, Ymax;
static fint     Fsymbol;               /* Graph Marker, see sect. 4.4 PGPLOT */
static fint     Fcolor;


void anyoutC( char *anystr )
/*
 *------------------------------------------------------------------------------
 * The C version of 'anyout' needs a C type string as argument only. The value
 * of Fscrnum is global.
 *------------------------------------------------------------------------------
 */
{
   anyout_c( &Fscrnum, tofchar( anystr ) );
}



static void showcoord( fchar Fsetin, fint *Fsubin, fint *Faxnum,
                       fint *Fsubdim, fint *Fsetdim, fchar Fshowstr )
/*
 *------------------------------------------------------------------------------
 * Create the string 'Fshowstr' containing information about the axes.
 * Example: Fshowstr = "(*,29.5 DEGREE,200 KM/S)". This string can be appended
 * to the string obtained after a call to 'showsubset".
 * The definitions for BIGSTORE and AXESMAX must be available.
 *------------------------------------------------------------------------------
 */
{
static   int    n;
static   fchar  Fcunit;
static   char   cunitbuf[20+1];
static   fint   Ferr = 0;
static   fint   Fres;
static   fint   Fgrid;
static   char   dummystr[80];
static   char   rightbuf[80];

   /* Coordinate transformation */

static   fint     Fdirect;             /* grid coord. -> physical coord. */
static   double   coordin[AXESMAX];    /* Grids before transformation */
static   double   coordout[AXESMAX];   /* Physical coordinates after transformation */


   sprintf( rightbuf, "%c", '(' );
   for (n = 0; n < *Fsetdim; n++ ) {
      if (n >= *Fsubdim) {
         Fgrid = gdsc_grid_c( Fsetin, &Faxnum[n], Fsubin, &Ferr );
         coordin[ (int) Faxnum[n]-1 ] = (double) Fgrid;
      }
      else {
         coordin[ (int) Faxnum[n]-1 ] = 0.0;
      }
   }
   Fdirect = 1;                             /* grid coord. -> physical coord. */
   Fres = cotrans_c( Fsetin, Fsubin, coordin, coordout, &Fdirect );
   if (Fres != 0) {
      Fshowstr = tofchar("?");
      return;
   }
   for (n = 0; n < *Fsetdim; n++ ) {
      if (n >= *Fsubdim) {
         Fcunit.a = cunitbuf; Fcunit.l = 20; cunitbuf[20] = '\0';
         Fres = axunit_c( Fsetin, &Faxnum[n], Fcunit );
         sprintf( dummystr, "%.6g %.*s", coordout[ (int) Faxnum[n]-1 ],
                  (int) nelc_c( Fcunit ), Fcunit.a );
      }
      else {
         sprintf( dummystr, "%c", '*' );
      }
      if (( n + 1 ) == *Fsetdim) {
         sprintf( rightbuf, "%.*s%s", strlen(rightbuf), rightbuf, dummystr );
      }
      else { /* Add comma */
         sprintf( rightbuf, "%.*s%s,", strlen(rightbuf), rightbuf, dummystr );
      }
   }
   sprintf( Fshowstr.a, "%s)", rightbuf );
}



void initplot( void )
/*------------------------------------------------------------------------------
 * Description: Initialize plot software. Set viewport and output dimensions.
 *              If output device is a printer, ask user for line width.
 *------------------------------------------------------------------------------
 */
{
   static fint    Funit;                  /* Ignored by 'pgbeg', use 0 */
   static fchar   Ffile;                  /* Device specification */
   static fint    Fnxysub[2];             /* Number of subdivisions */
   static float   width;                  /* Width of output on paper */
   static float   aspect;                 /* Aspect ratio of output on paper */
   static float   uservals[2];            /* Array version of above */
   static fint    Fnumitems;              /* Use in userxxx routines */
   static fint    Fdfault;                /* Use in userxxx routines */
   static fint    Fr1;                    /* Return value or level */
   static fint    len;                    /* Length of a string */
   static fint    Flinewidth;             /* Width of lines on output device */
   static fchar   devtype;                /* Device specified in 'pgbeg' */
   static fint    Ferrlev;
   static fint    Foff;



   Funit = 0;                             /* Ignored by 'pgbeg' */
   fmake( Ffile, 10 );
   Ffile = tofchar( "?" );                /* 'pgbeg' will prompt the user
                                             to supply a string. */
   Fnxysub[0] = 1;                        /* Default no subdivisions in plot */
   Fnxysub[1] = 1;
   Fnumitems  = 2;
   Fdfault    = HIDDEN;
   Fr1 = userint_c( Fnxysub,
                    &Fnumitems,
                    &Fdfault,
                    KEY_SUBDIV,
                    MES_SUBDIV );


   /* Set window and viewport */
   Fr1 = pgbeg_c( &Funit, Ffile, &Fnxysub[0], &Fnxysub[1] );
   if (Fr1 != 1) {
      Ferrlev = 4;
      error_c( &Ferrlev, tofchar("Cannot open output device") );
   }


   /* No NEXTPAGE= keyword */
   Foff = tobool( 0 );
   (void) pgask_c( &Foff );

   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */

   Fnumitems = 2;
   Fdfault = HIDDEN;
   uservals[0] = 0.0;
   uservals[1] = 1.0;
   Fr1 = userreal_c( uservals,
                     &Fnumitems,
                     &Fdfault,
                     tofchar("PAPER="),
                     tofchar("Give width(cm), aspect ratio: [0.0,1.0]") );
   if (Fr1 > 0) {
      /* If width = 0.0 then the program will select the largest */
      /* view surface */
      width  = uservals[0];
      /* Convert from cm to inches */
      width /= 2.54;
      aspect = uservals[1];
      (void) pgpap_c( &width, &aspect );
   }

   /* Get device-type code name of the current PGPLOT device */
   /* If the destination is a printer (=every destination  */
   /* except the Tektronix device), use thick lines in the plot */

   len = 20;
   finit(devtype, len);
   (void) pgqinf_c( tofchar("TYPE"), devtype, &len );
   if (strncmp(devtype.a, "TEK4010", 6) == 0) {
      /* It is a Tektronix */
   }
   else {
      Fnumitems = 1;
      Fdfault = HIDDEN;
      do {
         Flinewidth = 2;
         Fr1 = userint_c( &Flinewidth,
                          &Fnumitems,
                          &Fdfault,
                          tofchar("LINEWIDTH="),
                          tofchar("Give line width (1-21):  [2]") );
         agreed = ((Flinewidth >= 1) && (Flinewidth <= 21));
         if (!agreed) {
            (void) reject_c( tofchar("LINEWIDTH="),
                             tofchar("Invalid number") );
         }
      } while  (!agreed);
      (void) pgslw_c( &Flinewidth );
   }
   { /* Set viewport */
     static float Xl, Xr, Yb, Yt;

     Xl = 0.2;
     Xr = 0.9;
     Yb = 0.1;
     Yt = 0.9;
     (void) pgsvp_c(&Xl, &Xr, &Yb, &Yt);
   }
}



void stamp( fchar Setin, fint subnr )
/*----------------------------------------*/
/*----------------------------------------*/
{
   fchar    info;
   float    disp, coord, fjust;
   float    size, newsize;
   char     messbuf[200];
   fchar    Fshowstr;


   fmake( Fshowstr, 80 );
   if (Fsetdim > Fsubdim) {
      (void) showcoord( Setin, &Fsubin[subnr], Faxnum,
                        &Fsubdim, &Fsetdim, Fshowstr );
   } else {
      Fshowstr = tofchar("Top");
   }

   fmake( info, 40 );
   pgqinf_c( tofchar( "NOW" ), info, &info.l );
   sprintf( messbuf, "(%.*s) Set: %.*s,  level: %.*s",
            nelc_c( info ), info.a,
            nelc_c( Setin ), Setin.a,
            nelc_c( Fshowstr ), Fshowstr.a );
   coord = 0.5;
   fjust = 0.5;
   disp  = 3.0;
   pgqch_c( &size );
   newsize = 0.75;
   pgsch_c( &newsize );
   (void) pgmtxt_c( tofchar("R"), &disp, &coord, &fjust, tofchar(messbuf) );
   pgsch_c( &size );
}



void drawbox( float Xmin, float Xmax, float Ymin, float Ymax,
              double *Fposition1, double *Fposition2 )
/*------------------------------------------------------------------------------
 * Description: Draw box and labels etc.
 *------------------------------------------------------------------------------
 */
{
   static float   charsize;
   static float   Xtick, Ytick;           /* Tick values for plot box */
   static fint    Fnxsub, Fnysub;         /* Subintervals of major coordinate interval */
   static float   delta;
   static fchar   FXlabel, FYlabel, FTOPlabel;
   static char    buf1[BIGSTORE];         /* Text buffers */
   static char    buf2[BIGSTORE];
   static char    buf3[BIGSTORE];
   static float   disp, coord, fjust;     /* Determine position Y axis label */
   static float   Xminmax[2], Yminmax[2];
   static fint    Fdfault;
   static fint    Fnumitems;
   static fint    Fcolor;
   static fint    Ffont;
   static int     m;



   Fcolor = 1;                                                 /* Black color */
   (void) pgsci_c( &Fcolor );
   charsize = 1.0;
   (void) pgsch_c( &charsize );
   (void) pgpage_c();
   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Xmin -= delta; Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Ymin -= delta; Ymax += delta;
   Xminmax[0] = Xmin;
   Xminmax[1] = Xmax;
   Fnumitems = 2;
   Fdfault = HIDDEN;
   Fr1 = userreal_c( Xminmax,
                     &Fnumitems,
                     &Fdfault,
                     KEY_XMINMAX,
                     MES_XMINMAX );
   Xmin = Xminmax[0];
   Xmax = Xminmax[1];

   Yminmax[0] = Ymin;
   Yminmax[1] = Ymax;
   Fr1 = userreal_c( Yminmax,
                     &Fnumitems,
                     &Fdfault,
                     KEY_YMINMAX,
                     MES_YMINMAX );
   Ymin = Yminmax[0];
   Ymax = Yminmax[1];
   (void) pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );

   Xtick = 0.0; Ytick = 0.0;

   /* nx/nysub = the number of subintervals to divide the major
      coordinate interval into. If xtick=0.0 or nxsub=0,
      the number is chosen by PGBOX. */

   Fcolor = 1;   (void) pgsci_c( &Fcolor );             /* Default foreground */
   Ffont = 1;    (void) pgscf_c( &Ffont );                     /* Normal font */


   Fnxsub = 0; Fnysub = 0;

   (void) pgbox_c( tofchar("BCNST"), &Xtick, &Fnxsub,
                   tofchar("BCNSTV"), &Ytick, &Fnysub );

   fmake( FXlabel,    BIGSTORE );
   fmake( FYlabel,    BIGSTORE );
   fmake( FTOPlabel,  BIGSTORE );

   fmake( Fdataunits, BIGSTORE );
   Fr1 = 0;
   (void) gdsd_rchar_c( Fsetin,
                        tofchar("BUNIT"),
                        &Fsetlevel,
                        Fdataunits,
                        &Fr1 );
   sprintf( buf1,
            "Amplitude (%.*s)",
            nelc_c(Fdataunits),
            Fdataunits.a );
   FYlabel = tofchar( buf1 );
   sprintf( buf2, "Position index in slice" );
   FXlabel = tofchar( buf2 );

   sprintf( buf3, "One dimensional slice from ( " );
   for (m = 0; m < (int) Fsubdim; m++) {
      sprintf( buf3,
              "%.*s%d,",
               strlen(buf3),
               buf3,
               (int) Fposition1[m] );
   }
   sprintf( buf3, "%.*s ) to ( ", strlen(buf3)-1, buf3 );
   for (m = 0; m < (int) Fsubdim; m++) {
      sprintf( buf3,
              "%.*s%d,",
               strlen(buf3),
               buf3,
               (int) Fposition2[m] );
   }
   sprintf( buf3, "%.*s )", strlen(buf3)-1, buf3 );
   FTOPlabel = tofchar( buf3 );

   /* Select roman font for the labels */
   Ffont = 2;                                                   /* Roman font */
   (void) pgscf_c( &Ffont );
   (void) pglab_c( FXlabel, tofchar(" "), FTOPlabel );
   /* In pgbox a routine pgnumb is called. It converts numbers in */
   /* strings to plot along the axes. For any number greater than */
   /* 10000, the exponential format is used. For this format, put */
   /* the Y-axis label more to the left ('disp').                 */
   /* coord gives the location of the character string along the  */
   /* specified edge of the viewport, as a fraction of the length */
   /* of the edge.                                                */
   /* fjust controls justification of the string parallel to      */
   /* the specified edge of the viewport. If fjust = 0.5, the     */
   /* center of the string will be placed at coord.               */


   {
      static int     len1, len2;
      static char    smallbuf1[20];
      static char    smallbuf2[20];


      sprintf( smallbuf1, "%f", Ymax );
      sprintf( smallbuf2, "%f", Ymin );
      len1 = strlen( smallbuf1 );
      len2 = strlen( smallbuf2 );
      len1 = MYMAX( len1, len2 );
      if (len1 > 3) disp = 5.0; else disp = 3.0;
      coord = 0.5;
      fjust = 0.5;
      (void) pgmtxt_c( tofchar("L"), &disp, &coord, &fjust, FYlabel );
   }

}




MAIN_PROGRAM_ENTRY
{
   fchar key, mes;

   (void) init_c();                        /* contact Hermes */
   Fscrnum = 1;
   anyoutC( "Applications 'QPLOT' and 'PPLOT' are both replaced by a new" );
   anyoutC( "program 'PPLOT'. Please use 'PPLOT' to plot your profiles" );
   finis_c();                                           /* Quit Hermes */
   return( 0 ); 
  
   /* Task identification */
   {
      static fchar    Ftask;               /* Name of current task */
      fmake( Ftask, 20 );                  /* Macro 'fmake' must be available */
      (void) myname_c( Ftask );            /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }

   (void) setfblank_c( &blank );
   fmake(Fsetin, 80);
  /*
   *----------------------------------------------------------------------------
   * Because Fortran passes all arguments by reference, all C functions with
   * a Fortran equivalent must do this also (GIPSY programmers guide,
   * Chapter 9).
   *----------------------------------------------------------------------------
   */
   Fdfault = NONE;
   Fsubdim = 0;
   Fscrnum = 11;
   Fclass  = 1;

   FnsubsI  = gdsinp_c( Fsetin, Fsubin, &Fmaxsubs, &Fdfault, KEY_INSET,
                        MES_INSET, &Fscrnum, Faxnum, Faxcount, &Fmaxaxes,
                        &Fclass, &Fsubdim );
   Fsetdim  = gdsc_ndims_c( Fsetin, &Fsetlevel );
  /*
   *----------------------------------------------------------------------------
   * Determine the edges of this its frame (FgridLO/HI)
   *----------------------------------------------------------------------------
   */
   Fr1 = 0;
   (void) gdsc_range_c( Fsetin, &Fsetlevel, &Fcwlo, &Fcwhi, &Fr1 );

   for (m = 0; m < (int) Fsetdim; m++) {
      Fr1 = Fr2 = 0;
      FgridLO[m] = gdsc_grid_c( Fsetin, &Faxnum[m], &Fcwlo, &Fr1 );
      FgridHI[m] = gdsc_grid_c( Fsetin, &Faxnum[m], &Fcwhi, &Fr2 );
   }



   (void) initplot();

   /*----------------------------------------*/
   /* Open file to write obtained statistics */
   /*----------------------------------------*/
   {
      fchar    Dummystr;
      char     filenameOUT[BIGSTORE];
      fint     Overwrite;
      fint     Dfault;
      fint     R1;
      fint     Numitems;


      Dfault = REQUEST;
      fmake( Dummystr, BIGSTORE );
      do {
         Overwrite = toflog(true);
         strcpy( Dummystr.a, " " );
         R1 = usertext_c( Dummystr,
                          &Dfault,
                          KEY_FILENAME,
                          MES_FILENAME );
         if (R1 == 0) {
            tofile = false;
            agreed = true;
         }
         else {  /* User wants to write to a file */
            strcpy( filenameOUT, strtok(Dummystr.a, " ") ); /* Delete spaces in name */
            fpOUT = fopen(filenameOUT, "r");
            if (fpOUT != NULL) {                           /* The file exists */
               Numitems = 1;
               R1 = userlog_c( &Overwrite,
                               &Numitems,
                               &Dfault,
                               KEY_OVERWRITE,
                               MES_OVERWRITE );
               fclose( fpOUT );
               cancel_c( KEY_OVERWRITE );
            }
            if (!Overwrite) {
               cancel_c( KEY_FILENAME );
               agreed = false;
            }
            else {
               fpOUT = fopen(filenameOUT, "w");
               agreed = (fpOUT != NULL);
               if (!agreed) {
                  (void) reject_c( KEY_FILENAME, tofchar("Cannot open, try another!") );
               }
               else {
                  tofile = true;
               }
            }
         }
      } while (!agreed);
   }


   fmake( key, 20 );
   fmake( mes, 80 );
   do {
      double   Fposition1[AXESMAX];
      double   Fposition2[AXESMAX];

      Fdfault = REQUEST;
      Fmaxpos = 1;
      key = KEY_STARTPOS;
      mes = MES_STARTPOS;
      Fr1 = gdspos_c( Fposition1,    /* One position occupies 'Subdim' items. */
                      &Fmaxpos,           /* Maximum number of items to enter */
                      &Fdfault,
                      key,
                      mes,
                      Fsetin,
                      &Fsubin[0] );


      key = KEY_ENDPOS;
      mes = MES_ENDPOS;
      Fr1 = gdspos_c( Fposition2,    /* One position occupies 'Subdim' items. */
                      &Fmaxpos,           /* Maximum number of items to enter */
                      &Fdfault,
                      key,
                      mes,
                      Fsetin,
                      &Fsubin[0] );


      key = KEY_POINTS;
      mes = MES_POINTS;
      Fdfault   = REQUEST;
      Fnumitems = 1;
      do {
         Fpoints   = 50;
         Fr1       = userint_c( &Fpoints,
                                &Fnumitems,
                                &Fdfault,
                                key,
                                mes );
         agreed = ( (Fpoints > 1) && (Fpoints <= MAXPOINTS) );
         if (!agreed) {
            sprintf( messbuf, "2 <= N <= %d", MAXPOINTS );
            (void) reject_c( KEY_POINTS, tofchar(messbuf) );
         }
      } while (!agreed);



      stepsize = 1.0 / (double) (Fpoints-1);
      for (subcount = 0; subcount < (int) FnsubsI; subcount++) {
         numoutside = 0;
         Fpixels = 0;
         for (i = 0; i < Fpoints; i++) {
            for (m = 0; m < (int) Fsubdim; m++) {
               lambda = (double) i * stepsize;
               Bgrid[m] = (int) NINT(
                          Fposition1[m] +
                          lambda * ( Fposition2[m] - Fposition1[m] ) );
            }
            Xdata[i] = (float) i;
            outside = false;
            for (m = 0; m < (int) Fsubdim; m++) {
               if ( (Bgrid[m] < FgridLO[m]) ||
                    (Bgrid[m] > FgridHI[m]) ) {
                     numoutside++;
                     outside = true;
               }
            }

            if (!outside) {
               Fpixels++;
               /* First value of Xdata array is 1 !!! */
               Xdata[(int)Fpixels-1] = (float) Fpixels;
               Fcwlo = gdsc_fill_c( Fsetin, &Fsubin[subcount], Bgrid );
               Fcwhi = Fcwlo;
               FtidIN = 0;
               Fmaxtoread = 1;
               (void) gdsi_read_c( Fsetin,
                                   &Fcwlo,   /* Data from one pixel position! */
                                   &Fcwhi,
                                   &imageIN[(int)Fpixels-1],
                                   &Fmaxtoread,
                                   &Fpixelsdone,
                                   &FtidIN );

            }
         }
         if (outside) {
            if (numoutside == 1) {
               sprintf( messbuf, "%d pixel outside frame!", numoutside );
            }
            else {
               sprintf( messbuf, "%d pixels outside frame!", numoutside );
            }
            anyoutC( messbuf );
         }

         if (numoutside < (int) Fpoints) {
            /* There is data inside subset, make a plot. */

            minmax2_c(imageIN , &Fpixels, &Ymin, &Ymax, &nblanks );
            if (nblanks > 0) anyoutC("Slice contains blanks");
            if (Ymin == Ymax) Ymax++;
            if ( (Ymin == blank) || (Ymax == blank) ) {
               anyoutC("Cannot scale, perhaps all image values are blank?");
            } else {
               int   f;
               fint  start, len;
               Xmin = 1; Xmax = Fpixels;
               drawbox( Xmin, Xmax, Ymin, Ymax, Fposition1, Fposition2 );
               stamp( Fsetin, subcount );
               Fsymbol = 23;                                     /* Small circle */
               for (f = 0, len = 0, start = 0; f < (int) Fpixels; f++) {
                  if ( (Xdata[f] != blank) && (imageIN[f] != blank) ) {
                     len++;
                  } else {
                     Fcolor  = 1;          pgsci_c( &Fcolor );
                     pgpt_c( &len, &Xdata[start], &imageIN[start], &Fsymbol );
                     Fcolor  = 2;          pgsci_c( &Fcolor );
                     Fstyle  = FULL_LINE;  pgsls_c( &Fstyle );
                     pgline_c( &len, &Xdata[start], &imageIN[start] );
                     start += len + 1; /* Start after the blank */
                     len = 0;
                  }
               }
               if (len != 0) { /* Some or all left? */
                  Fcolor  = 1;          pgsci_c( &Fcolor );
                  pgpt_c( &len, &Xdata[start], &imageIN[start], &Fsymbol );
                  Fcolor  = 2;          pgsci_c( &Fcolor );
                  Fstyle  = FULL_LINE;  pgsls_c( &Fstyle );
                  pgline_c( &len, &Xdata[start], &imageIN[start] );
               }
               if (tofile) {
                  for (f = 0; f < (int) Fpixels; f++) {
                     fprintf( fpOUT, "%f %f\n", Xdata[f], imageIN[f] );
                  }
                  fprintf( fpOUT, "\n");
               }
            }
         }
         else {
            anyoutC("Cannot make a plot!");
         }


         Fdfault    = REQUEST;
         Fnumitems  = 1;
         fmake( Fcont, 10 );
         Fcont.a[0] =  'C';
         Fr1 = usercharu_c( Fcont,
                            &Fnumitems,
                            &Fdfault,
                            KEY_CONTINUE,
                            MES_CONTINUE );
         cancel_c( KEY_CONTINUE );
         Fcontinue = true;
         if ( (Fcont.a[0] == 'C') || (Fcont.a[0] == 'D') ) {
            Fcontinue = true;
            cancel_c( KEY_STARTPOS );
            cancel_c( KEY_ENDPOS );
         }
         if (Fcont.a[0] == 'Q') {
            /* Abort subset loop, leave program */
            Fcontinue = false;
            pgend_c();
            finis_c();                                  /* Quit Hermes */
            return 0;                                /* Dummy return for main */
         }
         if (Fcont.a[0] == 'D') {
            /* New device */
             pgend_c();
             cancel_c( tofchar("GRDEVICE=") );    /* Both in 'initplot' */
             cancel_c( KEY_SUBDIV );
             initplot();
         }
      } /* All subsets examined ? */

   } while (Fcontinue);

   if (tofile) fclose( fpOUT );                       /* Close data disk file */
      pgend_c();
      finis_c();                                           /* Quit Hermes */
   return 0;                                         /* Dummy return for main */
}
