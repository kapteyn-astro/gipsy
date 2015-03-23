/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             trace.dc1

Program:       TRACE

Purpose:       TRACE plots image values at user given positions
               as function of distance to a plot centre (in 2-d
               (sub)sets).

Category:      ANALYSIS, PLOTTING

File:          trace.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set (, subset):
               Only one subset is allowed. The dimension of the subset
               must be 2.

   XYBOX=      Give size of box around one pixel:                 [3,3]
               The resulting value at a selected position is an
               average of the pixels in the defined box around that
               position.

   MODE=       Manual mode or File mode?                          [M]/F
               Input pixel positions manual or by file.

   ASCINPUT=   Give name of input Ascii input:
               If MODE=F, ask for input file name. The file format is:
               x1 y1
               x2 y2
               ...
               Maximum number of position pairs is 10000.
               Numbers are separated by a space.

** GRIDS=      Give grid spacings x, y:                        [header]
               If nothing was found in the header of the set, this
               keyword is asked unhidden with default [1,1].

   STEPSIZE=   Give step size for interpolation:     [no interpolation]

   CENTPOS=    Give central pos.(x1, x2):
               This position is used to determine the distance of user
               selected positions with respect to the center. Physical
               values are allowed.

   INCL=       Inclination of trace in degrees:                   [0.0]
               If i is the inclination in degrees then
               minor / major = cos(i).
               For the number of inclinations see remarks at WIDTH=

   PA=         Position angle of major axis:                      [0.0]
               Measured as per astronomical convention from north
               to east.

   POSITION=   Give position x, y:
               Physical values are allowed.

   CONTINUE=   C(ontinue), D(evice), P(lot), W(rite), Q(uit):       [C]
               C: Continue, get next position.
               D: Continue but initialize plots again first.
               P: Plot results.
               W: Write results to file.
               Q: Quit program.

   DISTANCE=   Distance in (T)race or wrt. (C)entre:                [C]
               If you want to plot or write the results, select what
               kind of distance you want to use. Default is the
               distance measured with respect to the centre. The other
               option, DISTANCE=T, gives the distance in the trace you
               followed in physical coordinates.


               Before plotting, it is possible to adjust the size of
               the plot with the hidden keywords:

** XMINMAX=    Give Xmin, Xmax in plot:                    [calculated]

** YMINMAX=    Give Ymin, Ymax in plot:                    [calculated]



               Keywords to initialize plots:

** SUBDIV=     View surface subdivisions x,y:                     [1,1]
               It is possible to have more plots on the view surface.
               The number of plots can vary in both directions x and y.

   GRDEVICE=   Give graphics device to      [list of available devices]
               plot on:

** PAPER=      Give width (cm), aspect ratio:                [0.0, 1.0]
               Change the size of the (output) view surface.
               The aspect ratio is defined as height/width.
               The default is a calculated size and aspect ratio 1.

** LINEWIDTH=  Give line width (1-21):                              [2]
               Only required if a hardcopy device is selected.



Description:   TRACE can be used to trace a curve in a 2-dimensional
               subset. The plot will show amplitude versus distance
               in physical coordinates. The distance can be measured
               with respect to a user given center (DISTANCE=C) or
               as a distance in the trace you follow (DISTANCE=T).
               The positions in your trace are asked in a loop with
               POSITION=. You can give this position in pixels or in
               physical coordinates. A distance is calculated as the
               distance in pixels times the grid spacings and
               therefore plotted in physical coordinates. The distance
               in a trace is calculated as the sum of the previous
               distance in a trace and the distance to the previous
               position. The positions can be read from the file
               specified in ASCINPUT=, but you have to chose MODE=F
               first then. If you want to see some results, select
               CONTINUE=P for a plot. Plot software is initialized
               with the keywords SUBDIV=, GRDEVICE=, PAPER= and
               LINEWIDTH=. The size of the plot box is calculated
               but can be changed by the hidden keywords XMINMAX=
               and YMINMAX=. Before plotting the keyword DISTANCE=
               must be specified with 'T' ( distance in trace) or 'C'
               (distance with respect to the center given in CENTVAL=).
               If you want the distance expressed in pixels, use the
               hidden keyword GRIDS=1,1
               The program interpolates between two positions in
               steps defined by STEPSIZE=. The default is no inter-
               polation. The grid spacings are displayed in the log
               file and can be used as an indication how to chose
               the stepsize.
               It is possible to redirect the output to a file
               specified by ASCOUTPUT=. This file will contain two
               columns, the first one is the distance, the second one
               is the amplitude. The results are in Ascii. The maximum
               size of the input and output files are 10000 pairs of
               numbers. An extra file with the same name and the
               extension '.position' contains all (interpolated)
               positions.


Updates:       Aug 21, 1991: VOG, Document created.
               Oct 10, 1991: WZ, PGPLOT standard names implemented
               Jul 22, 1996: VOG, Changed CONTINUE= for use in COLA files
               Apr 14, 2009: VOG, Changed definition of NINT to one that
                                  uses floor() for consistency with other
                                  routines that process coordinates.

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
#include "gdspos.h"
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
#include "gdsd_rdble.h"
#include "error.h"
#include "axunit.h"
#include "userreal.h"
#include "userdble.h"
#include "userlog.h"
#include "userint.h"
#include "usertext.h"
#include "usercharu.h"
#include "minmax1.h"
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



#define AXESMAX    10               /* Max. allowed number of axes in a set */
#define SUBSMAX    2048             /* Max. number of substructures to be specified */
#define MAXBUF     4096             /* Buffer size for I/O */
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
#define MAXPOINTS  10000


/* Keywords and messages */

#define KEY_INSET         tofchar("INSET=")
#define MES_INSET         tofchar("Give set (, subset): ")
#define KEY_GRIDS         tofchar("GRIDS=")
#define KEY_STEPSIZE      tofchar("STEPSIZE=")
#define MES_STEPSIZE      tofchar("Give step size for interpolation:  [nointerp.]")
#define KEY_CENTPOS       tofchar("CENTPOS=")
#define MES_CENTPOS       tofchar("Give central pos.(x,y): ")
#define KEY_XYBOX         tofchar("XYBOX=")
#define MES_XYBOX         tofchar("Size of box around one pixel:  [3,3]")
#define KEY_MODE          tofchar("MODE=")
#define MES_MODE          tofchar("Manual mode or File mode?    [M]/F")
#define KEY_ASCINPUT      tofchar("ASCINPUT=")
#define MES_ASCINPUT      tofchar("Give name of Ascii input: ")
#define KEY_ASCOUTPUT     tofchar("ASCOUTPUT=")
#define MES_ASCOUTPUT     tofchar("Give name of Ascii output: ")
#define KEY_ANGLE         tofchar("PA=")
#define MES_ANGLE         tofchar("Position angle of major axis (deg):  [0] ")
#define KEY_INCLINATION   tofchar("INCL=")
#define MES_INCLINATION   tofchar("Inclination (deg):    [0.0]")
#define KEY_POSITION      tofchar("POSITION=")
#define MES_POSITION      tofchar("Give position x, y:" )
#define KEY_CONTINUE      tofchar("CONTINUE=")
#define MES_CONTINUE      tofchar("C(ont.),D(evice),P(lot),W(rite),Q(uit):   [C]" )
#define KEY_SUBDIV        tofchar("SUBDIV=")
#define MES_SUBDIV        tofchar("View surface subdivisions x,y:   [1,1]")
#define KEY_XMINMAX       tofchar("XMINMAX=")
#define MES_XMINMAX       tofchar("Give Xmin, Xmax in plot:    [calculated]" )
#define KEY_YMINMAX       tofchar("YMINMAX=")
#define MES_YMINMAX       tofchar("Give Ymin, Ymax in plot:    [calculated]" )
#define KEY_DISTANCE      tofchar("DISTANCE=")
#define MES_DISTANCE      tofchar("Distance in (T)race or wrt. (C)entre:  [C]")


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
#define TORAD(a)   ( a * 0.017453292519943295769237 )
#define TODEG(a)   ( a * 57.295779513082320876798155 )



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
static fint     FgridLO[AXESMAX];      /* Coordinate words for frame */
static fint     FgridHI[AXESMAX];


/* Data transfer: */

static float    imageIN[MAXBUF];       /* Multiple buffer for all subsets */


/* Miscellaneous: */

static fint     Fr1, Fr2;              /* Results of userxxx routines */
static float    blank;                 /* Value of system blank */
static fint     Fmaxpos;
static double   position[2*AXESMAX];
static double   centpos[2];
static double   prevpos[2];
static double   previppos[2];
static double   ippos[2];
static fint     Fcontinue;
static char     messbuf[BIGSTORE];
static fint     Fnumitems;
static fchar    Fcont;

static fint     XYbox[2];
static fint     deltaX, deltaY;
static fchar    Fmode;
static fchar    Fdummystr;
static char     filenameIN[BIGSTORE];
static char     filenameOUT[BIGSTORE];
FILE            *fpIN;
FILE            *fpOUT;
static fint     Fvalidpositions;
static int      agreed;
static float    imval;
static float    Xplot[MAXPOINTS], Yplot[MAXPOINTS];
static float    x[MAXPOINTS], y[MAXPOINTS];
static float    storeposX[MAXPOINTS], storeposY[MAXPOINTS];
static int      result;
static int      pixcount;
static int      filepos;
static fchar    Fdistance;
static float    tracedistance[MAXPOINTS];
static double   gridspac[2];
static double   Crota;
static double   stepsize;
static int      interpolate;
static float    Phi, Inc;
static float    Cosphi, Sinphi, Cosinc;


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


static float toangle( float Angle )
/*------------------------------------*/
/* Return angle between 0 and < 360.0 */
/*------------------------------------*/
{
   while (Angle < 0.0) Angle +=360.0;
   while (Angle > 360.0) Angle -=360.0;
   return Angle;
}



void initplot( void )
/*------------------------------------------------------------------------------
 * Description: Initialize plot software. Set viewport and output dimensions.
 *              If output device is a printer, ask user for line width.
 *------------------------------------------------------------------------------
 */
{
   fint    Funit;                  /* Ignored by 'pgbeg', use 0 */
   fchar   Ffile;                  /* Device specification */
   fint    Fnxysub[2];             /* Number of subdivisions */
   float   width;                  /* Width of output on paper */
   float   aspect;                 /* Aspect ratio of output on paper */
   float   uservals[2];            /* Array version of above */
   fint    Fnumitems;              /* Use in userxxx routines */
   fint    Fdfault;                /* Use in userxxx routines */
   fint    Fr1;                    /* Return value or level */
   fint    len;                    /* Length of a string */
   fint    Flinewidth;             /* Width of lines on output device */
   fchar   devtype;                /* Device specified in 'pgbeg' */
   fint    agreed;                 /* Loop guard */
   fint    Ferrlev;
   fint    Foff;



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
   pgask_c( &Foff );

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
      pgpap_c( &width, &aspect );
   }

   /* Get device-type code name of the current PGPLOT device */
   /* If the destination is a printer (=every destination  */
   /* except the Tektronix device), use thick lines in the plot */

   len = 20;
   finit(devtype, len);
   pgqinf_c( tofchar("TYPE"), devtype, &len );
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
      pgslw_c( &Flinewidth );
   }
   { /* Set viewport */
       float Xl, Xr, Yb, Yt;

     Xl = 0.2;
     Xr = 0.95;
     Yb = 0.1;
     Yt = 0.9;
     pgsvp_c(&Xl, &Xr, &Yb, &Yt);
   }
}


void drawbox( float Xmin, float Xmax, float Ymin, float Ymax )
/*------------------------------------------------------------------------------
 * Description: Draw box and labels etc.
 *------------------------------------------------------------------------------
 */
{
   float   charsize;
   float   Xtick, Ytick;           /* Tick values for plot box */
   fint    Fnxsub, Fnysub;         /* Subintervals of major coordinate interval */
   float   delta;
   fchar   FXlabel, FYlabel, FTOPlabel;
   char    buf1[BIGSTORE];         /* Text buffers */
   char    buf2[BIGSTORE];
   char    buf3[BIGSTORE];
   float   Xminmax[2], Yminmax[2];
   fint    Fdfault;
   fint    Fnumitems;
   fint    Fcolor;
   fint    Ffont;
   fchar   Faxunits;
   fchar   Fdataunits;             /* Units of data in the set */



   Fcolor = 1;                                                 /* Black color */
   pgsci_c( &Fcolor );
   charsize = 1.0;
   pgsch_c( &charsize );
   pgpage_c();
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
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );

   Xtick = 0.0; Ytick = 0.0;

   /* nx/nysub = the number of subintervals to divide the major
      coordinate interval into. If xtick=0.0 or nxsub=0,
      the number is chosen by PGBOX. */

   Fcolor = 1;   pgsci_c( &Fcolor );             /* Default foreground */
   Ffont = 1;    pgscf_c( &Ffont );                     /* Normal font */


   Fnxsub = 0; Fnysub = 0;

   pgbox_c( tofchar("BCNST"), &Xtick, &Fnxsub,
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
   FYlabel = tofchar(buf1 );


   fmake( Faxunits, BIGSTORE );
   Fr1 = axunit_c( Fsetin, &Faxnum[0], Faxunits );

   if (Fdistance.a[0] == 'C') {
      sprintf( buf2, "Distance wrt. centre in %.*s", nelc_c(Faxunits), Faxunits.a );
   }
   else {
      sprintf( buf2, "Distance in trace in %.*s", nelc_c(Faxunits), Faxunits.a );
   }
   FXlabel = tofchar( buf2 );

   sprintf( buf3, "Trace in set: %.*s", nelc_c(Fsetin), Fsetin.a );
   FTOPlabel = tofchar( buf3 );

   /* Select roman font for the labels */
   Ffont = 2;                                                   /* Roman font */
   pgscf_c( &Ffont );
   pglab_c( FXlabel, FYlabel, FTOPlabel );
}



float getimage( double *position, fint *FgridLO, fint *FgridHI,
                fint deltaX, fint deltaY, fchar Fsetin, fint Fsubset )
/*------------------------------------------------------------------------------
 * Description: Get average value in box between position vectors determined
 * by 'position' and 'deltaX/Y'
 *------------------------------------------------------------------------------
 */
{
   fint     VectLO[2], VectHI[2];
   fint     Fcwlo, Fcwhi;
   fint     FtidIN;
   fint     Fmaxtoread;
   fint     Fpixelsdone;
   float    sum;
   int      m;


   VectLO[0] = (fint) NINT((position[0] - (double)deltaX));
   VectLO[1] = (fint) NINT((position[1] - (double)deltaY));
   VectHI[0] = (fint) NINT((position[0] + (double)deltaX));
   VectHI[1] = (fint) NINT((position[1] + (double)deltaY));
   /* New vectors must be inside frame */
   VectLO[0] = MYMAX( FgridLO[0], VectLO[0] );
   VectHI[0] = MYMIN( FgridHI[0], VectHI[0] );
   VectLO[1] = MYMAX( FgridLO[1], VectLO[1] );
   VectHI[1] = MYMIN( FgridHI[1], VectHI[1] );
   Fcwlo = gdsc_fill_c( Fsetin, &Fsubset, VectLO );
   Fcwhi = gdsc_fill_c( Fsetin, &Fsubset, VectHI );

   /*------------------------*/
   /* Read pixels in sub box */
   /*------------------------*/
   FtidIN = 0;
   Fmaxtoread = (VectHI[0] - VectLO[0] + 1) * (VectHI[1] - VectLO[1] + 1);
   (void) gdsi_read_c( Fsetin,
                       &Fcwlo,
                       &Fcwhi,
                       imageIN,                               /* Global array */
                       &Fmaxtoread,
                       &Fpixelsdone,
                       &FtidIN );

   /*------------------------------------*/
   /* Determine average for these pixels */
   /*------------------------------------*/
   sum = 0;
   for (m = 0; m < (int) Fpixelsdone; m++) sum += imageIN[m];
   return  (sum / (float) Fpixelsdone);                     /* Return average */
}





MAIN_PROGRAM_ENTRY
{
   int    startplot;
   
  
   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    Ftask;               /* Name of current task */
      fmake( Ftask, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Ftask );                   /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }

   setfblank_c( &blank );
   fmake(Fsetin, 80);
  /*
   *----------------------------------------------------------------------------
   * Because Fortran passes all arguments by reference, all C functions with
   * a Fortran equivalent must do this also (GIPSY programmers guide,
   * Chapter 9).
   *----------------------------------------------------------------------------
   */
   Fdfault = NONE;
   Fsubdim = 2;                                       /* Dimension must be 2! */
   Fscrnum = 3;
   Fclass  = 1;

   FnsubsI  = gdsinp_c( Fsetin, Fsubin, &Fmaxsubs, &Fdfault, KEY_INSET,
                        MES_INSET, &Fscrnum, Faxnum, Faxcount, &Fmaxaxes,
                        &Fclass, &Fsubdim );
   if (FnsubsI > 1) anyoutC( "Only first subset is used.");
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
   /* Determine size of box around one pixel */
   /*----------------------------------------*/
   Fdfault = REQUEST;
   Fnumitems = 2;
   XYbox[0] = 3;
   XYbox[1] = 3;
   Fr1 = userint_c( XYbox, &Fnumitems, &Fdfault, KEY_XYBOX, MES_XYBOX );
   XYbox[0] = MYMAX( 1, XYbox[0] );
   XYbox[1] = MYMAX( 1, XYbox[1] );
   deltaX = (XYbox[0] - 1) / 2;
   deltaY = (XYbox[1] - 1) / 2;

   /*--------------------------------*/
   /* Choice of mode, Manual or File */
   /*--------------------------------*/
   fmake( Fmode, 10  );
   do {
      Fmode.a[0] = 'M';
      Fdfault = REQUEST;
      Fnumitems = 1;
      Fr1 = usercharu_c( Fmode,
                         &Fnumitems,
                         &Fdfault,
                         KEY_MODE,
                         MES_MODE );
      agreed = (Fmode.a[0] == 'M' || Fmode.a[0] == 'F');
      if (!agreed) (void) reject_c( KEY_MODE, tofchar("Only F/M allowed") );
   } while (!agreed);

   /*---------------------------------------------*/
   /* If in file mode, get filename and open file */
   /*---------------------------------------------*/
   if (Fmode.a[0] == 'F') {
      Fdfault = NONE;
      fmake( Fdummystr, BIGSTORE );
      do {
         Fr1 = usertext_c( Fdummystr,
                           &Fdfault,
                           KEY_ASCINPUT,
                           MES_ASCINPUT );
         strcpy( filenameIN, strtok(Fdummystr.a, " ") ); /* Delete spaces in name */
         fpIN = fopen(filenameIN, "r");
         agreed = (fpIN != NULL);
         if (!agreed) (void) reject_c( KEY_ASCINPUT, tofchar("Try again") );
      } while (!agreed);

      /*-----------------------------------------------------*/
      /* Read the data from the file (max MAXPOINTS entries) */
      /*-----------------------------------------------------*/
      i = 0;
      while (!feof(fpIN)) {
         fgets( messbuf, 80, fpIN);
         result = sscanf( messbuf, "%f%f", &x[i], &y[i] );
         if (result == 2) i++;
      }
      filepos = i;
      fclose( fpIN );
   }

   /*------------------------------------------------------*/
   /* Get the grid spacings from header or else, from user */
   /*------------------------------------------------------*/
   Fdfault = HIDDEN;
   Fr1 = 0;
   sprintf( messbuf, "Grid spacing:       [from header" );
   /* Get the pixel separation of the axes */
   gdsd_rdble_c( Fsetin, tofchar("CDELT1"), &Fsetlevel, &gridspac[0], &Fr1 );
   if (Fr1 < 0) {
      Fdfault = REQUEST;
      anyoutC("Cannot find gridspacing in header!");
      sprintf( messbuf, "Grid spacing:       [1,1]" );
      gridspac[0] = 1.0;
   }
   Fr1 = 0;
   /* Get the pixel separation of the axes */
   gdsd_rdble_c( Fsetin, tofchar("CDELT2"), &Fsetlevel, &gridspac[1], &Fr1 );
   if (Fr1 < 0) {
      Fdfault = REQUEST;
      anyoutC("Cannot find gridspacing in header!");
      sprintf( messbuf, "Grid spacing:       [1,1]" );
      gridspac[1] = 1.0;
   }
   Fnumitems= 2;
   Fr1 = userdble_c( gridspac,
                     &Fnumitems,
                     &Fdfault,
                     KEY_GRIDS,
                     tofchar(messbuf) );


   /* Ask for step size to use in interpolation */
   sprintf( messbuf, "Grid spacing x, y:%g, %g", gridspac[0], gridspac[1] );
   anyoutC( messbuf );
   Fnumitems = 1;
   Fdfault = REQUEST;
   Fr1 = userdble_c( &stepsize,
                     &Fnumitems,
                     &Fdfault,
                     KEY_STEPSIZE,
                     MES_STEPSIZE );
   if (Fr1 == 0) interpolate = false; else interpolate = true;



   /*------------------------------------------*/
   /* Ask for a central position in the subset */
   /*------------------------------------------*/
   Fdfault = REQUEST;
   Fmaxpos = 1;
   do {
      Fr1 = gdspos_c( centpos,     /* One position occupies 'Subdim' items. */
                      &Fmaxpos,           /* Maximum number of items to enter */
                      &Fdfault,
                      KEY_CENTPOS,
                      MES_CENTPOS,
                      Fsetin,
                      &Fsubin[0] );
      agreed = ( centpos[0] >= FgridLO[0] &&
                 centpos[0] <= FgridHI[0] &&
                 centpos[1] >= FgridLO[1] &&
                 centpos[1] <= FgridHI[1] );
      if (!agreed) (void) reject_c( KEY_CENTPOS, tofchar("position outside frame!") );
   } while(!agreed);


   Fdfault   = REQUEST;
   Fnumitems = 1;
   Inc = 0.0;
   Fr1 = userreal_c( &Inc,
                     &Fnumitems,
                     &Fdfault,
                     KEY_INCLINATION,
                     MES_INCLINATION );
   Cosinc = (float) cos( TORAD(Inc) );


   Fdfault   = REQUEST;
   Fnumitems = 1;
   Phi = 0.0;
   Fr1 = userreal_c( &Phi,
                     &Fnumitems,
                     &Fdfault,
                     KEY_ANGLE,
                     MES_ANGLE );

   Phi = toangle( Phi );
   /*---------------------------------------------------------------*/
   /* This Phi is needed to rotate back so invert sign of angle     */
   /* The angle is Phi, corrected for the map PA and corrected 90   */
   /* deg. so it is wrt. horizontal axis.                           */
   /*---------------------------------------------------------------*/
   Fr1 = 0;
   gdsd_rdble_c( Fsetin, tofchar("CROTA2"), &Fsetlevel, &Crota, &Fr1 );
   if (Fr1 < 0) Crota = 0.0;
   Cosphi = (float) cos( TORAD( -1.0*(Phi+Crota+90.0)) );
   Sinphi = (float) sin( TORAD( -1.0*(Phi+Crota+90.0)) );






   /*-------------------------------*/
   /* Begin loop over the positions */
   /*-------------------------------*/
   Fvalidpositions = 0;
   startplot = 0;
   pixcount = 0;
   do 
   {
      Fdfault = REQUEST;
      Fmaxpos = 1;
      if (Fmode.a[0] == 'F') 
      {
         do 
         {
            if (pixcount >= filepos) 
            {
               agreed = false;
            }
            if (agreed) 
            {
               position[0] = x[pixcount];
               position[1] = y[pixcount];
               agreed = ( position[0] >= FgridLO[0] &&
                          position[0] <= FgridHI[0] &&
                          position[1] >= FgridLO[1] &&
                          position[1] <= FgridHI[1] );
               pixcount++;
            }
         } 
         while(!agreed);
      }
      else 
      {
         do 
         {
            Fr1 = gdspos_c( position,     /* One position occupies 'Subdim' items. */
                            &Fmaxpos,           /* Maximum number of items to enter */
                            &Fdfault,
                            KEY_POSITION,
                            MES_POSITION,
                            Fsetin,
                            &Fsubin[0] );
            agreed = ( position[0] >= FgridLO[0] &&
                       position[0] <= FgridHI[0] &&
                       position[1] >= FgridLO[1] &&
                       position[1] <= FgridHI[1] );

            if (!agreed) 
               reject_c( KEY_POSITION, tofchar("position outside frame!") );
         } 
         while(!agreed);
      }


      /*----------------------------------------*/
      /* Do a linear interpolation between this */
      /* position and the previous one.         */
      /*----------------------------------------*/

      if (Fvalidpositions == 0) 
      {
         /* Initialize first position to be the previous position */
         prevpos[0] = position[0];
         prevpos[1] = position[1];
         storeposX[Fvalidpositions] = position[0];
         storeposY[Fvalidpositions] = position[1];

         /*-------------------------------------------------------*/
         /* Now there is a position inside the frame, read data.  */
         /*-------------------------------------------------------*/
         imval = getimage( position, FgridLO, FgridHI,
                           deltaX, deltaY, Fsetin, Fsubin[0] );

         /*------------------------------*/
         /* Determine distance to centre */
         /*------------------------------*/
         {
            double   xl, yl;
            float    Xx, Yy;

            xl = (position[0] - centpos[0] ) * gridspac[0];
            yl = (position[1] - centpos[1] ) * gridspac[1];

            Xx =  xl * Cosphi - yl * Sinphi;
            Yy = (xl * Sinphi + yl * Cosphi) / Cosinc;

            Xplot[Fvalidpositions] = (float) sqrt( Xx*Xx + Yy*Yy );
            Yplot[Fvalidpositions] = imval;
         }
         tracedistance[0] = 0;
         Fvalidpositions++;
      }
      else 
      {
         double   xl, yl, Xx, Yy;
         double   dist;
         double   trdist;
         double   Xxx, Yyy;
         double   steps;
         double   lambda, deltalambda;
         xl = (position[0] - prevpos[0]) * gridspac[0];
         yl = (position[1] - prevpos[1]) * gridspac[1];


         dist = sqrt( xl*xl + yl*yl );
         if (!interpolate) {
            steps = 2;
            deltalambda = 1.0;                      /* Take new position only */
         }
         else 
         {
            steps = dist / stepsize;
            deltalambda = 1.0 / steps;
         }
         lambda = 0.0;
         previppos[0] = prevpos[0];
         previppos[1] = prevpos[1];
         for (m = 0; m < (int) steps-1; m++) 
         {
            /* Start nearby previous pixel and end at new position exactly */
            lambda += deltalambda;              /* Run lambda between 0 and 1 */
            ippos[0] = prevpos[0] + lambda*(position[0]-prevpos[0]);
            ippos[1] = prevpos[1] + lambda*(position[1]-prevpos[1]);
            /* The real interpolation */
            storeposX[Fvalidpositions] = ippos[0];
            storeposY[Fvalidpositions] = ippos[1];

            /*-------------------------------------------------------*/
            /* Now there is a position inside the frame, read data.  */
            /*-------------------------------------------------------*/
            imval = getimage( ippos, FgridLO, FgridHI,
                              deltaX, deltaY, Fsetin, Fsubin[0] );

            /*------------------------------*/
            /* Determine distance to centre */
            /*------------------------------*/
            xl = (ippos[0] - centpos[0] ) * gridspac[0];
            yl = (ippos[1] - centpos[1] ) * gridspac[1];
            Xx =  xl * Cosphi - yl * Sinphi;
            Yy = (xl * Sinphi + yl * Cosphi) / Cosinc;

            Xplot[Fvalidpositions] = (float) sqrt( Xx*Xx + Yy*Yy );
            Yplot[Fvalidpositions] = imval;

            /*------------------------------------------*/
            /* Determine also the distance in the trace */
            /*------------------------------------------*/

            xl  = (previppos[0] - centpos[0] ) * gridspac[0];
            yl  = (previppos[1] - centpos[1] ) * gridspac[1];
            Xxx =  xl * Cosphi - yl * Sinphi;
            Yyy = (xl * Sinphi + yl * Cosphi) / Cosinc;

            trdist = (Xxx-Xx)*(Xxx-Xx) + (Yyy-Yy)*(Yyy-Yy);

/*            xl = (ippos[0] - previppos[0]) * gridspac[0];
              yl = (ippos[1] - previppos[1]) * gridspac[1];
*/

            /* ADD new value */
            tracedistance[Fvalidpositions] = tracedistance[Fvalidpositions-1] +
                                             (float) sqrt( trdist );
            previppos[0] = ippos[0];  /* This position becomes the previous one */
            previppos[1] = ippos[1];
            Fvalidpositions++;
         } /* End of interpolation loop */
         prevpos[0] = position[0];
         prevpos[1] = position[1];
      }



      /*--------------------------------------------------*/
      /* How to proceed? Continue with position input or  */
      /* continue after selection of new device,          */
      /* write data to file or quit?                      */
      /*--------------------------------------------------*/
      do 
      {
         Fnumitems  = 1;
         fmake( Fcont, 10 );
/*         if (Fmode.a[0] == 'F') 
         {
            Fdfault = HIDDEN;
            Fcont.a[0] =  's';
         }
         else 
*/
         {
           Fdfault    = REQUEST;
           Fcont.a[0] =  'C';
         }
         cancel_c( KEY_POSITION );      
         Fcontinue = true;         
         startplot = (Fmode.a[0] == 'F' && pixcount == (filepos-1));
         if (Fmode.a[0] != 'F')
         {
            Fr1 = usercharu_c( Fcont,
                               &Fnumitems,
                               &Fdfault,
                               KEY_CONTINUE,
                               MES_CONTINUE ); 
            cancel_c( KEY_CONTINUE );
         }
         
         startplot = (startplot || Fcont.a[0] == 'P');
         if (startplot)
         {
            Fmode.a[0] = 'M';                              /* Switch to manual */
         }
         if (Fcont.a[0] == 'Q') 
         {
            Fcontinue = false;
         }
         if (Fcont.a[0] == 'D') 
         {
            /* New device */
            pgend_c();
            cancel_c( tofchar("GRDEVICE=") );    /* Both in 'initplot' */
            cancel_c( KEY_SUBDIV );
            initplot();
         }
         if (startplot || (Fcont.a[0] == 'W')) 
         {
            /* Make choice about which distance is to be used */
            Fdfault = REQUEST;
            Fnumitems = 1;
            fmake( Fdistance, 10 );
            Fdistance.a[0] = 'C';
            Fr1 = usercharu_c( Fdistance,
                               &Fnumitems,
                               &Fdfault,
                               KEY_DISTANCE,
                               MES_DISTANCE );
            cancel_c( KEY_DISTANCE );
         }
 
         if (startplot)
         {
            if (Fvalidpositions >= 1) 
            {
               /* There is data inside subset, make a plot. */
 
               if (Fdistance.a[0] == 'C') 
               {
                  (void) minmax1_c(Yplot, &Fvalidpositions, &Ymin, &Ymax );
                  (void) minmax1_c(Xplot, &Fvalidpositions, &Xmin, &Xmax );
                  (void) drawbox( Xmin, Xmax, Ymin, Ymax );
                  Fsymbol = 23;                                /* Small circle */
                  pgpt_c( &Fvalidpositions, Xplot, Yplot, &Fsymbol );
                  Fcolor = 2;         pgsci_c( &Fcolor );
                  Fstyle = FULL_LINE; pgsls_c( &Fstyle );
                  pgline_c( &Fvalidpositions, Xplot, Yplot );
                  Fcolor = 1;         pgsci_c( &Fcolor );
               }
               else 
               {
                  (void) minmax1_c(Yplot, &Fvalidpositions, &Ymin, &Ymax );
                  (void) minmax1_c(tracedistance, &Fvalidpositions, &Xmin, &Xmax );
                  (void) drawbox( Xmin, Xmax, Ymin, Ymax );
                  Fsymbol = 23;                                /* Small circle */
                  pgpt_c( &Fvalidpositions, tracedistance, Yplot, &Fsymbol );
                  Fcolor = 2;         pgsci_c( &Fcolor );
                  Fstyle = FULL_LINE; pgsls_c( &Fstyle );
                  pgline_c( &Fvalidpositions, tracedistance, Yplot );
                  Fcolor = 1;         pgsci_c( &Fcolor );
               }
            }
            else 
            {
               anyoutC("Cannot make a plot!");
            }
         }
         if (Fcont.a[0] == 'W') 
         {
            Fdfault = NONE;
            fmake( Fdummystr, BIGSTORE );
            do 
            {
               Fr1 = usertext_c( Fdummystr,
                                 &Fdfault,
                                 KEY_ASCOUTPUT,
                                 MES_ASCOUTPUT );
               strcpy( filenameOUT, strtok(Fdummystr.a, " ") ); /* Delete spaces in name */
               fpOUT = fopen(filenameOUT, "w");
               agreed = (fpOUT != NULL);
               if (!agreed) 
                  reject_c( KEY_ASCOUTPUT, tofchar("Cannot open, try again!") );
 
            }            
            while (!agreed);
            
            cancel_c( KEY_ASCOUTPUT );
            for (i = 0; i < (int) Fvalidpositions; i++) 
            {
               if (Fdistance.a[0] == 'C') 
               {
                  fprintf( fpOUT, "%f  %f\n", Xplot[i], Yplot[i] );
               }
               else 
               {
                  fprintf( fpOUT, "%f  %f\n", tracedistance[i], Yplot[i] );
               }
            }
            fclose( fpOUT );
            /* Write the positions too */
            strcat(filenameOUT, ".position" );
            fpOUT = fopen( filenameOUT, "w" );
            for (i = 0; i < (int) Fvalidpositions; i++) 
            {
               fprintf( fpOUT, "%f  %f\n", storeposX[i], storeposY[i] );
            }
            fclose( fpOUT );
         }
      } 
      while( Fcont.a[0] == 'W' || startplot || Fcont.a[0] == 'D' );
   } 
   while( Fcontinue );

   pgend_c();
   finis_c();                                        /* Quit Hermes */
   return 0;                                         /* Dummy return for main */
}
