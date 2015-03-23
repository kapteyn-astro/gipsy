/*
                           COPYRIGHT (c) 1995
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             gauprof.dc1

Warning:       This program is not maintained anymore. In the near future, 
               it will be removed from the set of GIPSY tasks. An
               improved version with graphical user interface is called 
               XGAUPROF.


Program:       GAUPROF

Purpose:       GAUPROF examines influence of parameters in GAUFIT by 
               plotting Gaussian estimate and fit of a selected profile.

Category:      ANALYSIS, PROFILES, PLOTTING

File:          gauprof.c

Author:        M.G.R. Vogelaar

Keywords:
               
   STARTNEW=   Start improved version of this program?           [Y]/N
   
               A new version with graphical user interface is called 
               XGAUPROF. You can start it here.
 
             
   INSET=      Give set name and operation axis ( & range ):
               Maximum number of subsets is 2048.   


** SUBDIV=     View surface subdivisions x,y:                     [1,1]
               It is possible to have more plots on the view surface.
               The number of plots can vary in both directions x and y.


   GRDEVICE=   Graphics device           [list of all graphics devices]

               
** PAPER=      Give width (cm), aspect ratio:                 [0.0,1.0]
               Change the size of the (output) view surface.
               The aspect ratio is defined as height/width.
               The default is a calculated size and aspect ratio 1.

               
** LINEWIDTH=  Give line width (1-21):                              [2]
               Only required if a hardcopy device is selected.


** XMINMAX=    Give Xmin, Xmax in plot:                    [calculated]


** YMINMAX=    Give Ymin, Ymax in plot:                    [calculated]
               

   BOX=        Enter profile position(s) in ..., ...:            [quit]
               Examples:
               BOX=3 4        Examine profile at (3,4)
               BOX=0 0 3 3    Examine profiles from (0,0) to (3,3)
               or:
               BOX=D 3 3
               CPOS=0 0       Examine profiles from (-1,-1) to (1,1)

               
   PROFILE=    (Q)uit, (S)ame, (P)revious, (B)ox, [N]ext profile
               1) (Q)uit program
               2) (S)ame profile (you can change parameters now)
               3) (P)revious profile if it exists.
               4) (N)ext profile


               Next keywords become hidden after their first 
               specification. 

        
   NGAUSS=     Give max. number of gaussians in profile:            [1]
               The maximum number is 5. 



               The calculation of the initial estimates.
               =========================================
               The fit routine in GAUFIT needs initial estimates for
               the parameters. These estimates are calculated by the 
               program using
               a method described by Schwarz, 1968, Bull.Astr.Inst.
               Netherlands, Volume 19, 405. The associated routine
               reads a number of parameters. It uses NGAUSS=,
               and the hidden keywords:
               ESTRMS=, ESTCUTAMP= and ESTCUTDISP=.
                                                                           
                        
** ESTRMS=     Give rms noise level in profile:                   [0.0]
               Rms of noise in profile in units of the data.
               The value is used in an automatic window method to 
               define the signal region in the routine that calcu-
               lates initial estimates for each profile.

                  
** ESTCUTAMP=  Give cutoff for profile:                           [0.0]
               Estimated gaussians below this amplitude will be 
               discarded. The amplitude is in units of the data.


** ESTCUTDISP= Give critical dispersion of gaussian:              [0.0]
               Estimated gaussians with dispersion smaller than the 
               critical dispersion will be discarded.
               The critical dispersion is given in units of the
               operation axis.


   Q=          Give smoothing parameter:                            [2]
               See description.
               

              
               Calculation of the fitted parameters.
               ====================================               
               The actual fitting is a least-squares fit of a function 
               to a set of data points. The method used is described 
               in: Marquardt, J.Soc.Ind.Appl.Math. 11, 431 (1963).
               This  method is a mixture of the steepest descent 
               method and the Taylor method. The weights for the 
               data points are all equal to 1.0.


   TOLERANCE=  Fractional tolerance for the chi square:          [0.01]

               Fitting is stopped when successive iterations fail
               to produce a decrement in reduced chi-squared less
               than TOLERANCE=   The value cannot be less than a
               certain minimum as set by the system. This means that
               maximum accuracy can be obtained with TOLERANCE=0.0


** MAXITS=     Maximum number of iterations in fit:                [50]

               If this number is exceeded, no parameters could be
               fitted.
               

** LAB=        Value for mixing parameter:                      [0.001]

               Mixing parameter in the least squares fit function.
               LAB= determines the initial weight of steepest descent  
               method relative to the Taylor method. LAB= should be
               a small value (i.e. 0.001).




               Edge fitting
               ============
               
** CENTVAL=    Central value for edge fit in physical coordinates:
                                        [Central value on profile axis]
               The position in physical units, needed to determine in
               which direction data along profile is stored to be used 
               in a least squares fit, now called 'edge' fit.
               

** EDGEFIT=    Do you want an 'edge' fit?                         Y/[N]
               Default: skip the 'edge 'fitting'.


Description:   The decomposition of profiles into Gaussian components
               can be a powerful method in the study of the kinematics 
               of neutral hydrogen. The program GAUFIT can make initial 
               estimates of Gaussian parameters that can be stored or 
               used as initial values for a least-squares fit. This 
               program (GAUPROF) helps you to examine the influence
               of different variables like the ones specified with the 
               keywords NGAUSS=, ESTCUTAMP=, ESTCUTDISP=, Q= and TOLERANCE=. 
               There is a loop facility that enables you to change
               keywords for the same profile. In the observed 
               profile the second derivative of a fitted second order 
               polynomial is used to estimate the parameters of the 
               Gaussian components. The polynomial is fitted at each 
               pixel using q points distributed symmetrically around 
               any pixel. The number q equals 2*Q+1 where Q is given by
               the smoothing parameter (Q=). The threshold
               values given by ESTRMS= (rms noise of the profile), and
               Q=, are used to discriminate against spurious components.
               Other criteria to select valid Gaussians are a 
               critical amplitude (ESTCUTAMP=) and a critical dispersion 
               (ESTCUTDISP=) in units selected by the program.
               Gaussians with amplitude smaller than ESTCUTAMP= or with
               dispersion smaller than ESTCUTDISP= will be discarded.
               It is assumed that the observed profile can be approxi-
               mated by the sum of a few Gaussian functions. The 
               number of Gaussians you allow in the calculations is
               set by NGAUSS=. The maximum number is 5.               
               (For more details: Schwarz, 1968, Bull.Astr.Inst.
               Netherlands, Volume 19, 405-413.)
               
               The fit of the estimated set of components could be
               improved by means of a least-squares analysis (Full 
               line in the plot).
             
               Fitting of the profile stops when successive iterations 
               fail to produce a decrement in reduced chi-squared less 
               than TOLERANCE=. If its value is less than the minimum 
               tolerance possible, it will be set to this value. This 
               means that maximum accuracy can be obtained by setting 
               TOLERANCE=0.0. 
               
               The weights for all data points is set to 1.0.
                             
               In order to fit Gaussians in profiles you first need to
               specify the input set and the subsets (INSET=). The 
               number of subsets must be greater than 1 and less than 
               2048 and the subsets are specified by giving the name of 
               the axis in the direction in which you want your profiles. 
               Suppose your set AURORA has the axes:
               RA-NCP             from    -7 to     8
               DEC-NCP            from    -7 to     8
               FREQ-OHEL          from     1 to    59
               and you want an estimate and fit of Gaussian components 
               of a profile in frequency direction at RA=2, DEC=4,
               specify:                              
                                INSET=AURORA FREQ 1:59
                                BOX=2 4


               The 'edge' fitting routine selects data from the position
               (in physical coordinates) of the estimated Gaussian with 
               maximum amplitude ('center') to a profile edge and performs 
               a 'lsq' fit. The edge is chosen in the following way: If 
               the value in CENTVAL= is smaller than 'center', all data 
               with axis values (in physical coordinates) greater than 
               center are assembled for the 'lsq' fit etc.
               Sometimes the amplitude of the edge fit is larger than 
               the maximum value of the used data. To adjust your plot
               use XMINMAX= and YMINMAX=. These keywords are hidden, so
               you have to specify them for instance at the same time 
               as PROFILE= in the plot loop.

               
Example:       <USER >gauprof
               <USER >INSET=m8320 f 1:58
               Set M8320 has 3 axes
               RA-NCP             from   -50 to    40
               DEC-NCP            from   -50 to    20
               FREQ-OHEL          from     1 to    59
               <USER >GRDEVICE=tek
               <USER >BOX=-3 -3 4 4
               BOX range for set M8320 :
               RA-NCP             from    -3 to     4
               DEC-NCP            from    -3 to     4
               <USER >NGAUSS=2
               <USER >ESTRMS=0.3
               <USER >ESTCUTAMP=0.6
               <USER >ESTCUTDISP=0.4
               <USER >Q=2
               <USER >CENTVAL=
 
               GAUPROF:        ESTIMATE                         FIT   
               =======================================================...
               gauss    ampl.   centre    disp.        ampl.   centre ...
                        W.U.     KM/S     KM/S         W.U.     KM/S  ...
               =======================================================...
                    1     2.89   212.01     8.61         3.43   214.34...
                    2     0.45   192.04     3.56         0.61   192.63...
               NGAUSS=2  RMS=0.30    CUTAMP=0.60    CUTDISP=0.40    Q=...
               <USER >PROFILE=n
                    1     0.88   225.13     3.69         2.54   217.01...
                    2     0.58   192.50     3.49         0.42   195.15...
               <USER >PROFILE=q
               <STATUS> gauprof -  +++ FINISHED +++

                              
Updates:       Jun 11, 1991: VOG, Document created.
               Oct 10, 1991: WZ,  PGPLOT standard names implemented
               Mar 25, 1994: VOG, Rearranged keywords. 
               May 30, 1995: VOG, Copied 'func' and 'derv' from GAUFIT.
                                  Added MAXITS= and LAB= keywords.
                                  Updated documentation 


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
#include "gdsbox.h"
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
#include "userfio.h"
#include "presetd.h"
#include "minmax1.h"
#include "gauest.h"
#include "lsqfit.h"
#include "status.h"

#include "pgplot.h"


#define AXESMAX    10               /* Max. allowed number of axes in a set */
#define SUBSMAX    2048             /* Max. number of substructures to be specified */
#define MAXBUF     16*4096          /* Buffer size for I/O */
#define MAXGAUSS   8                /* Max. number of gaussians in profile */
#define MAXPAR     3                /* Max. parameters in one gaussian */
#define MAXPARALL  MAXPAR*MAXGAUSS  /* Max. parameters in all gaussians */
#define PLOTPOINTS 200              /* Max number of points to plot */
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
#define MES_INSET         tofchar("Give set name and operation axis (& range):")
#define KEY_BOX           tofchar("BOX=")
#define KEY_OPTION        tofchar("OPTION=")
#define MES_OPTION        tofchar("Give option(s): 0/1/2   [list options]")
#define KEY_TOLERANCE     tofchar("TOLERANCE=")
#define MES_TOLERANCE     tofchar("Tolerance for least-squares fit:   [0.01]")
#define KEY_LAB           tofchar("LAB=")
#define MES_LAB           tofchar("Value for mixing parameter:          [0.001]")
#define KEY_MAXITS        tofchar("MAXITS=")
#define MES_MAXITS        tofchar("Maximum number of iterations in fit:   [50]")
#define KEY_ESTRMS        tofchar("ESTRMS=")
#define MES_ESTRMS        tofchar("Give rms noise level for estimate routine: [0.0]")
#define KEY_FITRMS        tofchar("RMS=")
#define KEY_CUTAMP        tofchar("ESTCUTAMP=")
#define MES_CUTAMP        tofchar("Give cutoff for profile")
#define KEY_CUTDISP       tofchar("ESTCUTDISP=")
#define KEY_Q             tofchar("Q=")
#define MES_Q             tofchar("Give smoothing parameter:   [2]")
#define KEY_NGAUSS        tofchar("NGAUSS=")
#define MES_NGAUSS        tofchar("Give max. number of gaussians in profile:  [1]")
#define KEY_PROFILE       tofchar("PROFILE=")
#define MES_PROFILE       tofchar("(Q)uit, (S)ame, (P)revious, (B)ox, [N]ext profile" )
#define KEY_CENTVAL       tofchar("CENTVAL=")
#define KEY_SUBDIV        tofchar("SUBDIV=")
#define MES_SUBDIV        tofchar("View surface subdivisions x,y:   [1,1]")
#define KEY_XMINMAX       tofchar("XMINMAX=")
#define MES_XMINMAX       tofchar("Give Xmin, Xmax in plot:    [calculated]" ) 
#define KEY_YMINMAX       tofchar("YMINMAX=")
#define MES_YMINMAX       tofchar("Give Ymin, Ymax in plot:    [calculated]" ) 
#define KEY_EDGEFIT       tofchar("EDGEFIT=")
#define MES_EDGEFIT       tofchar("Do you want an 'edge' fit?         Y/[N]")

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


/* Input of set, subsets: */  

static fchar    Fsetin;                /* Name of the set */
static fint     Fsubin[SUBSMAX];       /* Array for the subset coordinate words */
static fint     FnsubsI;               /* Number of input subsets */
static fint     dfault;                /* Default option for input etc */
static fint     Faxnum[AXESMAX];       /* GDSINP axis numbers array */
static fint     Faxcount[AXESMAX];     /* GDSINP axis lengths array */
static fint     Fclass = 2;            /* Axis is operation axis */
static fint     Fsetdim;               /* Dimension of the set */
static fint     Fsubdim;               /* Dimension of the subset */
static fint     Fscrnum = 11;          /* Destination of log output */
static fint     Fmaxaxes  = AXESMAX;   /* Convert parameters to variables */
static fint     Fmaxsubs  = SUBSMAX;
static int      subnr;                 /* Index of current subset */
static int      i,m;                   /* Counters */
static fint     Fsetlevel = 0;         /* Indicate set level */
static fint     Faxesoutsidesub;       /* Allow for one 'profile' axis */

   
/* Input of area etc.:*/

static fint     Fcwlo[SUBSMAX];        /* Coordinate words */
static fint     Fcwhi[SUBSMAX];
static fint     FgridLO[AXESMAX];      /* Coordinate words for frame */
static fint     FgridHI[AXESMAX];
static fint     BgridLO[AXESMAX];      /* Coordinate words for box */
static fint     BgridHI[AXESMAX];
static fint     Fboxopt;               /* Input option for 'gdsbox' */


/* Data transfer: */

static fint     Ftotpixels;            /* Total number of pixels in input */   
static fint     Fpixelsdone;
static fint     FtidIN[SUBSMAX];       /* Transfer id. */
static float    imageIN[MAXBUF];       /* Multiple buffer for all subsets */   


/* Related to update of header etc: */

static fchar    Faxunits;              /* Units along profile axis */
static fchar    Fdataunits;            /* Units of data in the set */


/* 'lsqfit' related */

static float    tol;                   /* Tolerance in 'lsq' fit */
static float    lab;                   /* Mixing parameter, see description */
static float    xdat[SUBSMAX];         /* Data to be fitted */
static float    wdat[SUBSMAX];         /* Weights in fit */
static fint     its;                   /* Max. allowed number of iterations */
static fint     mpar[MAXPARALL];       /* Fit parameters free or fixed (0/1)? */
static float    estimates[MAXPARALL];  /* 0=Ampl, 1=central offset, 2=sigma */
static float    edge_estimates[MAXPAR];/* Same for 'edge' fitting */
static float    storeparms[MAXPARALL]; 
static float    epar[MAXPARALL];       /* Errors in fitted parameters */
static fint     xdim;                  /* Dimension of fit data (=1) */
static fint     npar;                  /* Number of parameters in lsqfit: n*MAXPAR */
static fint     iters;                 /* Counter */

/* 'gauest' related */

static float    amplitudes[SUBSMAX];   /* Max. # points is 'SUBSMAX' */
static float    workdummy[SUBSMAX];    /* Gauest routine needs workspace */
static float    critampl;
static float    critdisp;
static float    profilerms;
static fint     Fsmoothpar;
static fint     Fmaxparameters;
static fint     Fgaussians;            /* # Gaussians found in 'gauest' */
static fint     Fgaussians_edge;       /* # Gaussians found in 'edge' lsqfit */
static fint     Fmaxgaussians;         /* Max. # of Gaussians you allow to find */


/* Edge fitting related */

static float    vcent_user;


/* Miscellaneous: */

static fint     Fprofiles;             /* Number of profiles */
static int      num_blanks;            /* # blanks in profile */
static fint     Fr1, Fr2;              /* Results of userxxx routines */
static float    blank;                 /* Value of system blank */
static char     longbuf[2*BIGSTORE];   /* Buffer for long text message */
static char     positionstr[BIGSTORE]; /* Text buffer for current position */
static fint     Fmaxtoread;            /* Max num pixels in 1 read action */
static int      offset;                /* Offset in input buffer */

static float    physcoord[SUBSMAX];    /* Array with converted profile coordinates */
static int      sameprof;              /* User wants to examine same profile again */
static int      nofit;                 /* No lsq fit possible */
static int      edgefit;               /* lsq 'edge' fit */ 
static int      newbox;                /* User wants other profile(s) */
static int      changed;               /* One of the keywords changed a value */
static int      quitprogram;        

static float    gridspac;              /* Average grid spacing for dispersion conversion */
static float    Xplot[PLOTPOINTS];     /* Draw Gaussian with 'PLOTPOINTS' points */
static float    Yplot[PLOTPOINTS];   
static fint     Fnpoints;              /* Variable with value 'PLOTPOINTS' */
static fint     Ferrlev;               /* Level for use in 'error' routine */
static fchar    Fdummystr;
static fint     modvec[AXESMAX];       /* Number of pixels in each dim, 1,2,... */
static fint     curvec[AXESMAX];       /* subdim dim. equivalent of 1 dim. pos */
static int      curpos;                /* Current 1-dim position */
static int      first;                 /* First plot */
static fchar    Fcontinue;
static fint     Fcolor;


/* 'pgplot' related */

static fint     Fstyle;                /* Line style attribute, sect. 5.4 PGPLOT */
static float    Xmin, Xmax, Ymin, Ymax;
static fint     Fsymbol;               /* Graph Marker, see sect. 4.4 PGPLOT */



/* Conversion float <--> double. */
      
static float    fltmax;                /* maximum floating-point number */
static float    flteps;                /* smallest number x such that 1.0+x != 1.0 */
static double   dfltmax;               /* 'fltmax' converted to double */
static double   maxarg;                /* Max. arg for exp function so that */
                                       /* result > -dfltmax. */
static double   exparg[MAXGAUSS];      /* Store part of function calculation */




static void errorC( int level,
                    char *str )
/*------------------------------------------------------------------*/
/* The C version of 'error'.                                        */
/*------------------------------------------------------------------*/
{
   fint   flev = (fint) level;
   error_c( &flev, tofchar( str ) );
}




static void initplot( void )
/*------------------------------------------------------------------------------
 * Description: Initialize plot software. Set viewport and output dimensions.
 *              If output device is a printer, ask user for linewidth.
 *------------------------------------------------------------------------------
 */                

{ 
   fint    Funit;                  /* Ignored by 'pgbeg', use 0 */
   fchar   Ffile;                  /* Device specification */
   fint    Fnxysub[2];             /* Number of subdivisions */
   float   width;                  /* Width of output on paper */
   float   aspect;                 /* Aspect ratio of output on paper */ 
   float   uservals[2];            /* Array version of above */
   fint    nitems;                 /* Use in userxxx routines */
   fint    dfault;                 /* Use in userxxx routines */
   fint    Fr1;                    /* Return value or level */
   fint    len;                    /* Length of a string */
   fint    Flinewidth;             /* Width of lines on output device */
   fchar   devtype;                /* Device specified in 'pgbeg' */
   fint    agreed;                 /* Loop guard */
   fint    Foff;

      
 
   Funit = 0;                             /* Ignored by 'pgbeg' */
   fmake( Ffile, 10 ); 
   Ffile = tofchar( "?" );                /* 'pgbeg' will prompt the user 
                                             to supply a string. */
   Fnxysub[0] = 1;                        /* Default no subdivisions in plot */
   Fnxysub[1] = 1;
   nitems     = 2;
   dfault     = HIDDEN;
   Fr1 = userint_c( Fnxysub, 
                    &nitems,
                    &dfault,
                    KEY_SUBDIV,
                    MES_SUBDIV );
   
   
   /* Set window and viewport */   
   Fr1 = pgbeg_c( &Funit, Ffile, &Fnxysub[0], &Fnxysub[1] );
   if (Fr1 != 1)
      errorC( 4, "Cannot open output device" );

   
   /* No NEXTPAGE= keyword */
   Foff = tobool( 0 );
   pgask_c( &Foff );   
   
   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */
      
   nitems  = 2;
   dfault  = HIDDEN;
   uservals[0] = 0.0;
   uservals[1] = 1.0;
   Fr1 = userreal_c( uservals, &nitems, &dfault, 
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
   
   /* Get device-type code name of the current PGPLOT device    */
   /* If the destination is a printer (=every destination)      */
   /* except the Tektronix device), use thick lines in the plot */
          
   len = 20;
   finit(devtype, len);
   pgqinf_c( tofchar("TYPE"), devtype, &len );
   if (strncmp(devtype.a, "TEK4010", 6) == 0) {
      /* It is a Tektronix */
   }
   else {      
      nitems  = 1;
      dfault  = HIDDEN;
      do {
         Flinewidth = 2;
         Fr1 = userint_c( &Flinewidth, &nitems, &dfault,
                          tofchar("LINEWIDTH="),
                          tofchar("Give line width (1-21):  [2]") );
         agreed = ((Flinewidth >= 1) && (Flinewidth <= 21));
         if (!agreed) {
            reject_c( tofchar("LINEWIDTH="),
                      tofchar("Invalid number") );
         }
      } while  (!agreed);
      pgslw_c( &Flinewidth );
   }
   { /* Set viewport */
     static float Xl, Xr, Yb, Yt;
     
     Xl = 0.2;
     Xr = 0.95;
     Yb = 0.1;
     Yt = 0.9;
     pgsvp_c(&Xl, &Xr, &Yb, &Yt);
   }
}
      


static void drawbox( float Xmin, 
                     float Xmax, 
                     float Ymin, 
                     float Ymax, 
                     char  *toplabel )
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
   float   Xminmax[2], Yminmax[2];
   fint    dfault;
   fint    nitems;
   fint    color;   
   fint    Ffont;

  
  
   /* Black color */
   color      = 1;
   pgsci_c( &color );
   charsize   = 1.0;
   pgsch_c( &charsize );
   pgpage_c();  
   delta      = fabs( Xmax - Xmin ) / 10.0;
   Xmin      -= delta; Xmax += delta;
   delta      = fabs( Ymax - Ymin ) / 10.0;
   Ymin      -= delta; Ymax += delta;
   Xminmax[0] = Xmin;
   Xminmax[1] = Xmax;
   nitems     = 2;
   dfault     = HIDDEN;
   Fr1        = userreal_c( Xminmax, 
                            &nitems, 
                            &dfault, 
                            KEY_XMINMAX,
                            MES_XMINMAX );
   Xmin       = Xminmax[0];                     
   Xmax       = Xminmax[1];
   
   Yminmax[0] = Ymin;
   Yminmax[1] = Ymax;
   Fr1        = userreal_c( Yminmax, 
                            &nitems, 
                            &dfault, 
                            KEY_YMINMAX,
                            MES_YMINMAX );
   Ymin       = Yminmax[0];                     
   Ymax       = Yminmax[1];
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );
   
   Xtick      = 0.0; 
   Ytick      = 0.0;
   
   /* nx/nysub = the number of subintervals to divide the major
      coordinate interval into. If xtick=0.0 or nxsub=0,
      the number is chosen by PGBOX. */ 
      
     
   Ffont      = 1;  /* Normal font */
   pgscf_c( &Ffont );
   
   Fnxsub     = 0; 
   Fnysub     = 0;
   
   pgbox_c( tofchar("BCNST"), &Xtick, &Fnxsub,
            tofchar("BCNSTV"), &Ytick, &Fnysub );

     
   fmake( FXlabel, BIGSTORE );
   fmake( FYlabel, BIGSTORE );
   fmake( FTOPlabel, BIGSTORE );
   
   sprintf( buf1, "Amplitude (%.*s)", 
            nelc_c(Fdataunits),
            Fdataunits.a );
   FYlabel = tofchar( buf1 );
   sprintf( buf2, "(%.*s)", 
            nelc_c(Faxunits),
            Faxunits.a );
   FXlabel = tofchar( buf2 );

   FTOPlabel = tofchar( toplabel );

   /* Select roman font for the labels */
   Ffont = 2;  /* Roman font */
   pgscf_c( &Ffont );
   pglab_c( FXlabel, FYlabel, FTOPlabel );
} 



#ifdef ERRORMESS
static void errormess( int result )
/*
 *------------------------------------------------------------------------------
 * Error message for 'lsqfit'
 *------------------------------------------------------------------------------
 */
{
   if (result == -1) anyoutf( 1, "Too many free parameters, maximum is 32." );
   if (result == -2) anyoutf( 1, "No free parameters." );
   if (result == -3) anyoutf( 1, "Not enough degrees of freedom." );
   if (result == -4) {
      anyoutC( "Maximum number of iterations too small to ..." );
      anyoutC( "... obtain a solution which satisfies TOL." );
   }
   if (result == -5) anyoutf( 1, "Diagonal of matrix contains elements which are zero." );
   if (result == -6) anyoutf( 1, "Determinant of the coefficient matrix is zero." );
   if (result == -7) anyoutf( 1, "Square root of negative number." );                  
}
#endif




extern float func_c( float *xdat,
                     float *fpar,
                     fint  *npar,
                     fint  *fopt )
/*------------------------------------------------------------*/
/* PURPOSE: Fit routine needs evaluation of 1-dim gauss func- */
/*          tion on arbitrary position.                       */
/* Fi(x) = Ai * exp( -1/2 * (Ci - x)^2 / Di^2  )              */
/* (zero level == 0.0)                                        */
/* ==> F(x) = SUM[ Fi(x) ] and fopt determines how            */
/* many gaussians there are to sum.                           */
/* A = fpar[0],  C = fpar[1],  D = fpar[2];                   */
/*------------------------------------------------------------*/
{
   int      i;
   double   amp, cen, dis;
   double   arg;
   double   result = 0.0;;


   for (i = 0; i < (*fopt); i++)
   {
      int offset = i * 3;
      amp = (double)   fpar[  offset];
      cen = (double) ( fpar[1+offset] - xdat[0] );
      dis = (double)   fpar[2+offset];
      if (dis < 0.0) fpar[2+offset] = fabs(fpar[2+offset]);
      arg = 0.5 * cen*cen/dis/dis;
      if (dis != 0.0 && arg < maxarg)
         exparg[i] = exp( -arg );
      else
         exparg[i] = 0.0;

      result += amp * exparg[i];
   }
   if (result < -dfltmax)
      return( -fltmax );
   if (result > dfltmax)
      return( fltmax );

   return( (float) result );
}



extern void derv_c( float *xdat,
                    float *fpar,
                    float *epar,
                    fint  *npar,
                    fint  *fopt )
/*------------------------------------------------------------*/
/* PURPOSE: Fit routine needs evaluation of derivative of     */
/*          1-dim gauss function on arbitrary position.       */
/* Fi(x) = Ai * exp( -1/2 * (Ci - x)^2 / Di^2 )               */
/* (zero level == 0.0)                                        */
/* A = fpar[0],  C = fpar[1],  D = fpar[2];                   */
/* Note that the function includes an ABS! Negative disper-   */
/* sions can enter the expression.                            */
/*------------------------------------------------------------*/
{
   double   amp, cen, dis;
   double   ep0, ep1, ep2;
   int      i;


   for (i = 0; i < (*fopt); i++)
   {
      int offset = i * 3;
      amp = fpar[  offset];
      cen = fpar[1+offset] - xdat[0];
      dis = fpar[2+offset];
      /*--------------------------------------------------*/
      /* Global variable 'exparg' is storage for the      */
      /* expression exp(-0.5 * cen*cen/dis/dis) calculated*/
      /* in 'func'. It is also a check on dis == 0.0 and  */
      /* arg < maxarg.                                    */
      /*--------------------------------------------------*/

      if (exparg[i] != 0.0)
      {
         ep0 = exparg[i];
         epar[  offset] = (float) ep0;

         ep1 = -amp * ep0 * cen/dis/dis;
         if (ep1 > dfltmax)
            epar[1+offset] = fltmax;
         else if (ep0 < -dfltmax)
            epar[1+offset] = -fltmax;
         else
            epar[1+offset] = (float) ep1;

         ep2 = -ep1 * cen/dis;
         if (ep2 > dfltmax)
            epar[2+offset] = fltmax;
         else if (ep0 < -dfltmax)
            epar[2+offset] = -fltmax;
         else
            epar[2+offset] = (float) ep2;
      }
      else
         epar[offset] = epar[1+offset] = epar[2+offset] = 0.0;
   }
}



static double getphyscoord( fchar Fsetname, fint cowosubset, int subnr )      
/*
 *------------------------------------------------------------------------------
 * This function return the physical coordinate of a grid position on
 * the operation axis. 
 *------------------------------------------------------------------------------
 */
{
   fint    Fdirect = 1;              /* grid coord. -> physical coord. */
   double  coordin[AXESMAX];         /* grids before cotrans */
   double  coordout[AXESMAX];        /* Phys. coords after cotrans */
   fint    Fres;
   int     n;
   double  zero = 0.0;               /* Preset array to zero */
   fint    Fsetlevel = 0;
   fint    r1;

   
   Fsetdim  = gdsc_ndims_c( Fsetin, &Fsetlevel );
   presetd_c( &zero, coordin, &Fsetdim );
   n = (int) Fsetdim - 1;                               /* Axnum = 1, 2, ... */ 
   r1 = 0;
   coordin[(int) Faxnum[n]-1] = (double) gdsc_grid_c( Fsetin, 
                                                      &Faxnum[n], 
                                                      &cowosubset,
                                                      &r1 );
   Fres = cotrans_c( Fsetin, 
                     &cowosubset, 
                     coordin, coordout, 
                     &Fdirect );
   if ((int) Fres != 0) 
   {
      if (subnr == 0) 
      {
         anyoutf( 1, "Cannot find physical coordinates!" );
         anyoutf( 1, "Substituting subset numbers." );      
      }
      coordout[(int) Faxnum[n]-1] = (double) subnr + 1;
   }
   return ( coordout[(int) Faxnum[n] - 1] );
}



static float tophys( float *physcoord, float pixelval )
/*
 *------------------------------------------------------------------------------
 * Pixels along the subset axis are numbered from 0 to n. Given a pixel-
 * value, we interpolate to get the physical value.
 *------------------------------------------------------------------------------
 */
{
   int    intval;
   float  delt, physdelt;
   
   intval = (int) fabs(pixelval);
   if (intval < 0) 
      intval = 0;
   if (intval > (int) FnsubsI-1) 
      intval = (int) FnsubsI - 2;
   delt = pixelval - (float) intval;
   physdelt = physcoord[intval+1] - physcoord[intval];
   return( physcoord[intval] + delt * physdelt );
} 


                                   
static void getparms( fint  *AFmaxgaussians, 
                      float *Aprofilerms,
                      float *Acritampl, 
                      float *Acritdisp, 
                      fint  *AFsmoothpar, 
                      float *Atol, 
                      float *Alab,
                      fint  *Amaxits,
                      float *Avcent,
                      int   first, 
                      int   *Aedgefit, 
                      int   *Achanged)
/*
 *------------------------------------------------------------------------------
 * Get keywords. If keywords are asked once, they will be hidden later.
 *'Faxunits' and 'Fdataunits' are global.
 *------------------------------------------------------------------------------
 */
{
   fint     nitems;
   fint     dfault; 
   fint     Fr1;
   fint     agreed;
   char     messbuf[2*BIGSTORE];     /* Buffer for text message */
   
   fint     Fmaxgaussians;
   float    profilerms;
   float    critampl;
   float    critdisp;
   fint     Fsmoothpar;
   float    tol;
   float    lab;
   float    vcent;
   int      changed;
   fint     maxits;

   /* Save previous values. Static is essential here */
   
   static fint     OFmaxgaussians;
   static float    Oprofilerms;
   static float    Ocritampl;
   static float    Ocritdisp;
   static fint     OFsmoothpar;
   static float    Otol;
   static float    Olab;
   static float    Ovcent;
   static fint     Omaxits;


   if (first) 
   {
      changed = true; 
      dfault  = REQUEST;
   }
   else
   { 
      changed = false;
      dfault  = HIDDEN;
   }
   nitems        = 1;
   Fmaxgaussians = 1;
   do 
   {
      Fr1 = userint_c( &Fmaxgaussians, 
                       &nitems, 
                       &dfault, 
                       KEY_NGAUSS, 
                       MES_NGAUSS ); 
      agreed = ( (Fmaxgaussians > 0) && (Fmaxgaussians < MAXGAUSS) );
      if (!agreed) 
         reject_c( KEY_NGAUSS, tofchar("Not allowed!") );
   } while (!agreed);      

   if ( (!first) && (Fmaxgaussians != OFmaxgaussians) ) 
      changed = true;   
   *AFmaxgaussians = Fmaxgaussians;               
  /*
   *----------------------------------------------------------------------------
   * ('gauest') The r.m.s. noise level of the profile.
   *----------------------------------------------------------------------------
   */
   if (first) 
   {
      changed = true; 
      dfault  = NONE;
   }
   else
   { 
      changed = false;
      dfault  = HIDDEN;
   }   
   nitems = 1;   
   (void) sprintf( messbuf, 
                  "Give rms noise for estimate routine (in %.*s):", 
                   nelc_c(Fdataunits),
                   Fdataunits.a );
   
   Fr1 = userreal_c( &profilerms, 
                     &nitems, 
                     &dfault, 
                     KEY_ESTRMS, 
                     tofchar(messbuf) ); 
   if ( (!first) && (profilerms != Oprofilerms) ) 
      changed = true;
   *Aprofilerms = profilerms;
        

  /*
   *----------------------------------------------------------------------------
   * ('gauest')  Critical amplitude of gaussian. Gaussians below this 
   * amplitude will be discarded.                               
   *----------------------------------------------------------------------------
   */
   if (first) 
      dfault = REQUEST; 
   else 
      dfault = HIDDEN;
   nitems = 1;
   critampl  = 0.0;
   (void) sprintf( messbuf, 
                  "Give crit. amplitude in %.*s:    [%g]", 
                   nelc_c(Fdataunits),
                   Fdataunits.a,
                   critampl );  
   Fr1 = userreal_c( &critampl, 
                     &nitems, 
                     &dfault, 
                     KEY_CUTAMP, 
                     tofchar(messbuf) ); 
   if ( (!first) && (critampl != Ocritampl) ) 
      changed = true;    
   *Acritampl = critampl;

  /*
   *----------------------------------------------------------------------------
   * ('gauest')  Critical dispersion of gaussian. Gaussians with dispersion
   * below this will be discarded. Use axis units for message.
   *----------------------------------------------------------------------------
   */
   if (first) 
      dfault = REQUEST; 
   else 
      dfault = HIDDEN;
   nitems = 1;
   critdisp  = 0.0;
   (void) sprintf( messbuf, 
                  "Give crit. dispersion in %.*s:   [%g]", 
                   nelc_c(Faxunits),
                   Faxunits.a,
                   critdisp ); 
   Fr1 = userreal_c( &critdisp, 
                     &nitems, 
                     &dfault, 
                     KEY_CUTDISP, 
                     tofchar(messbuf) ); 
   if ( (!first) && (critdisp != Ocritdisp) ) 
      changed = true;    
   /* Convert dispersion from physical value to grids */   
   *Acritdisp = critdisp / gridspac;
   
  /*
   *----------------------------------------------------------------------------
   * ('gauest')  Smoothing parameter for calculating initial estimates
   * Q= determines the number of points (=2*Q+1) used in calculating the
   * second derivative of the profile.
   *----------------------------------------------------------------------------
   */
   if (first) 
      dfault = REQUEST; 
   else 
      dfault = HIDDEN;
   nitems     = 1;
   Fsmoothpar = 2;
   do 
   {
      Fr1 = userint_c( &Fsmoothpar, 
                       &nitems, 
                       &dfault, 
                       KEY_Q, 
                       MES_Q ); 
      agreed = ( (Fsmoothpar >= 1) || ((2*Fsmoothpar+1) <= FnsubsI) );
      if (!agreed) 
         reject_c( KEY_Q, tofchar("Not allowed!") );
   } while (!agreed);
   if ( (!first) && (Fsmoothpar != OFsmoothpar) ) 
      changed = true;
   *AFsmoothpar = Fsmoothpar;
   
  /*
   *----------------------------------------------------------------------------
   *  ('lsqfit') Fitting of the profile stops when successive iterations fail 
   *  to produce a decrement in reduced chi-squared less than TOLERANCE. If 
   *  its value is less than the minimum tolerance possible, it will be set 
   *  to this value. This means that maximum accuracy can be obtained by 
   *  setting TOLERANCE=0.0.
   *----------------------------------------------------------------------------
   */   
   tol       = 0.01;
   dfault    = REQUEST;
   nitems    = 1;
   Fr1 = userreal_c( &tol, 
                     &nitems, 
                     &dfault, 
                     KEY_TOLERANCE, 
                     MES_TOLERANCE );

   if ( (!first) && (tol != Otol) ) 
      changed = true;          
   tol   = fabs( tol );
   *Atol = tol;


  /*
   *----------------------------------------------------------------------------
   * Mixing parameter in 'lsqfit' , 'lab' determines the initial weight of 
   * steepest descent method relative to the Taylor method. 'lab' should be 
   * a small value (i.e. 0.01). 'lab' can only be zero when the partial 
   * derivatives are independent of the parameters. In fact in this case 
   * 'lab' should be exactly equal to zero. 
   *----------------------------------------------------------------------------
   */

   lab       = 0.001;
   dfault    = REQUEST;
   nitems    = 1;
   Fr1 = userreal_c( &lab, 
                     &nitems, 
                     &dfault, 
                     KEY_LAB,
                     MES_LAB );

   if ( (!first) && (lab != Olab) ) 
      changed = true;          
   lab   = fabs( lab );
   *Alab = lab;


   maxits = 50;
   dfault = REQUEST;
   nitems = 1;
   Fr1 = userint_c( &maxits,
                    &nitems,
                    &dfault,
                    KEY_MAXITS,
                    MES_MAXITS );

   if ( (!first) && (maxits != Omaxits) )
      changed = true;
   maxits   = fabs( maxits );
   *Amaxits = maxits;

   
   /* For the edge fitting we need a central value (usually the central   */
   /* velocity). If the position of the peak has a value greater than     */
   /* the initial estimate, all lower values are taken into account in    */
   /* lsqfit etc.  */  
 
   
   vcent = physcoord[ (int) FnsubsI/2 ];
   if (first) 
   {
      (void) sprintf( messbuf, 
                     "Central val. for edge fit in %.*s:  [%f]",   
                      nelc_c(Faxunits),
                      Faxunits.a,
                      vcent );          
      dfault = REQUEST;
   }
   else 
   {
      dfault = HIDDEN;
      strcpy( messbuf, " " );  
   }
   nitems = 1;                      
   Fr1 = userreal_c( &vcent, 
                     &nitems, 
                     &dfault, 
                     KEY_CENTVAL, 
                     tofchar(messbuf) );
   if ( (!first) && (vcent != Ovcent) ) 
      changed = true;
   *Avcent = vcent;


   {
      bool edge;
      edge    = toflog( false );
      dfault  = HIDDEN;
      nitems  = 1;
      Fr1     = userlog_c( &edge, &nitems, &dfault,
                           KEY_EDGEFIT, MES_EDGEFIT );
      edge = tobool( edge );
      *Aedgefit = edge;
   }
                                                         
   /* Store values */
   OFmaxgaussians = Fmaxgaussians;
   Oprofilerms    = profilerms;
   Ocritampl      = critampl;
   Ocritdisp      = critdisp;   
   OFsmoothpar    = Fsmoothpar;
   Otol           = tol;
   Olab           = lab;
   Omaxits        = maxits;
   Ovcent         = vcent;

   *Achanged = changed;
}



static void findaxnames( char *axnames )
/*
 *------------------------------------------------------------------------------
 * Find names of axes in format (RA,DEC) etc.
 *------------------------------------------------------------------------------
 */
{
   int    n;
   char   axis_b[20+1];
   fchar  Fctype;
   fint   Ferr = 0;
   char   dummystr[BIGSTORE];


   (void) sprintf( axnames,  "%c", '(' );
   for (n = 0; n < Fsubdim; n++ ) 
   {
      Fctype.a = axis_b; Fctype.l = 20; axis_b[20] = '\0';
      gdsc_name_c( Fctype, Fsetin, &Faxnum[n], &Ferr );
      if (( n + 1 ) == Fsubdim) 
      {
         /* Space and hyphen! */
         (void) sprintf( dummystr, "%s", strtok( axis_b, " -" ) );
      } 
      else 
      {
         /* Comma added */
         (void) sprintf( dummystr, "%s,", strtok( axis_b, " -" ) );   
      }    
      (void) sprintf( axnames, "%.*s%s", strlen(axnames), axnames, dummystr );
   }
   (void) sprintf( axnames,  "%.*s%c", strlen(axnames),  axnames,  ')' );      
}
   


static void putinlogfile( fint Fgaussians, char *positionstr, float *estparms, 
                          float *fitparms, float *edgeparms, int nofit, 
                          int edgefit, int first )
/*
 *------------------------------------------------------------------------------
 * 'Faxunits', Fdataunits', 'physcoord' and 'gridspac' are global.
 *------------------------------------------------------------------------------
 */
{
   int      j;
   char     messbuf[200];
   char     messbuf2[200];   
   char     axnames[40];
   

/*      
                ESTIMATE                         FIT
gauss    ampl.   centre    disp.        ampl.   centre    disp.    position
===============================================================================
12345__-1234.56_-1234.56_-1234.56_____-1234.56_-1234.56_-1234.56___(...,...)
________(w.u)    (km/s)   (km/s)       (w.u)    (km/s)   (km/s)
________123456___123456___123456_______123456___123456___123456___
*/    

   if (first)                                       /* Create log-file header */
   {
      anyoutf( 3, " " );
      anyoutf( 3, "GAUPROF:        ESTIMATE                         FIT                         EDGE FIT");
      anyoutf( 3, "==========================================================================================================================");                     
      anyoutf( 3, "gauss    ampl.   centre    disp.        ampl.   centre    disp.        apml.   centre   disp.       position");                   
      findaxnames( axnames );
      anyoutf( 3, 
               "         %-9.9s%-9.9s%-9.9s    %-9.9s%-9.9s%-9.9s    %-9.9s%-9.9s%-9.9s  %-s",
               Fdataunits.a, Faxunits.a, Faxunits.a,
               Fdataunits.a, Faxunits.a, Faxunits.a,
               Fdataunits.a, Faxunits.a, Faxunits.a,
               axnames );
      anyoutf( 3, "=========================================================================================================================="); 
   }
   
    

   if (Fgaussians == 0) 
   {
      anyoutf( 3, "  *       *         *        *           *         *        *             *          *         *    %-s",
               positionstr );
   }
 
   for (j = 0; j < (int) Fgaussians; j++) 
   {
      int offset = j * MAXPAR;
      if (nofit) 
      {
         (void) sprintf( messbuf,
                         "%6d %8.2f %8.2f %8.2f         *        *        *        ",
                         j+1,
                         estparms[0+offset],
                         tophys( physcoord, estparms[1+offset] ),
                         estparms[2+offset] * gridspac );
      }
      else 
      {         
         (void) sprintf( messbuf,
                         "%6d %8.2f %8.2f %8.2f     %8.2f %8.2f %8.2f",
                         j+1,
                         estparms[0+offset],
                         tophys( physcoord, estparms[1+offset] ),
                         estparms[2+offset] * gridspac,
                         fitparms[0+offset],
                         tophys( physcoord, fitparms[1+offset] ),
                         fitparms[2+offset] * gridspac );
                         

      }
      if (!edgefit) 
      {
         (void) sprintf( messbuf2, 
                         "%.67s      *        *         *     %-s",
                         messbuf,
                         positionstr );
      }
      else 
      {
         (void) sprintf( messbuf2, 
                         "%.67s %8.2f %8.2f %8.2f    %-s",
                         messbuf,          
                         edgeparms[0],
                         tophys( physcoord, edgeparms[1] ),
                         edgeparms[2] * gridspac,
                         positionstr );
      }
      anyoutf( 3, messbuf2 );
   }
}                  


#ifdef GETRMS
static float getrms( float *amplitudes, int n )
/*----------------------------------------------*/
/* Get rms of a profile                         */
/*----------------------------------------------*/
{
   int   i;
   float sum, sumdev;
   float average;

   for (i = 0, sum = 0.0; i < n; i++) 
   {
      if (amplitudes[i] != blank) sum += amplitudes[i];
   }
   average = sum / n;
   for (i = 0, sumdev = 0.0; i < n; i++) 
   {
      if (amplitudes[i] != blank) 
      {
         float  dev;
         dev = amplitudes[i] - average;
         dev *= dev;
         sumdev += dev;
      }
   }
   return( (float) sqrt( sumdev/n ) );
}
#endif



static void getboxmessage( fchar Setin,
                           fint  subset,
                           fint  *axnum,
                           char  *boxtxt )
/*------------------------------------------------------------*/
/* PURPOSE: Create a text that can be used in the box prompt. */
/* Get axis names of the input subset. Note that the set input*/
/* was a class 2 input.                                       */
/*------------------------------------------------------------*/
{
   fint    subdim;
   int     n;
   
     
   subdim = gdsc_ndims_c( Setin, &subset );
   strcpy( boxtxt, "Enter profile position(s) in " );
   for (n = 0; n < subdim; n++) 
   {
      fint   r1 = 0;  
      fchar  Axisname;
            
      fmake( Axisname, 20 );
      gdsc_name_c( Axisname, Setin, &axnum[n], &r1 );
      strcat( boxtxt, strtok(Axisname.a, " -") );      
      if (n != subdim - 1)
         strcat( boxtxt, "," );
   }
   strcat( boxtxt, ":       [quit]" );
}


 
MAIN_PROGRAM_ENTRY
/*------------------------------------------------------------*/
/* Because Fortran passes all arguments by reference, all C   */
/* functions with a Fortran equivalent must do this also      */
/* (GIPSY programmers guide, * Chapter 9).                    */
/*------------------------------------------------------------*/
{
   char     boxmes[80];
    
   init_c();                               /* contact Hermes */
   /* Task identification */
   {        
      static fchar    Ftask;               /* Name of current task */
      fmake( Ftask, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Ftask );                   /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }


   /* Start the new version? */
   {
      fint   r;
      fint   dfault = REQUEST;
      fint   nitems = 1;
      bool   new = toflog( 1 );
      
      r = userlog_c( &new, &nitems, &dfault, tofchar("STARTNEW="), 
          tofchar("Start improved version of this program?           [Y]/N") );
      new = tobool( new );
      if (new)
      {
         anyoutf( 3, "GAUPROF started XGAUPROF" );
         r = 0;
         deputy_c( tofchar("XGAUPROF") , &r );   /* Start new task */
         finis_c();                         /* Quit Hermes */          
      }
   }   



   
   setfblank_c( &blank );      
   fmake(Fsetin, 80);


   fltmax  = FLT_MAX;                      /* Defines are functions, so initialize */
   flteps  = FLT_EPSILON;
   dfltmax = (double) FLT_MAX;             /* Cast function result to double */
   maxarg  = -log( (double) FLT_EPSILON ); /* Limit range in exp function */


   dfault  = NONE;
   Faxesoutsidesub = 1;                     
   Fscrnum = 0;
   Fclass  = 2; 
   FnsubsI = gdsinp_c( Fsetin, 
                       Fsubin, 
                       &Fmaxsubs, 
                       &dfault, 
                       KEY_INSET,
                       MES_INSET, 
                       &Fscrnum, 
                       Faxnum, 
                       Faxcount, 
                       &Fmaxaxes, 
                       &Fclass, 
                       &Faxesoutsidesub );
   Fsetdim  = gdsc_ndims_c( Fsetin, &Fsetlevel );
   Fsubdim  = Fsetdim - Faxesoutsidesub;
  /*   
   *----------------------------------------------------------------------------
   * Determine the edges of this its frame (FgridLO/HI)
   *----------------------------------------------------------------------------
   */
   Fr1 = 0;
   gdsc_range_c( Fsetin, &Fsetlevel, &Fcwlo[0], &Fcwhi[0], &Fr1 );
   
   for (m = 0; m < (int) Fsetdim; m++) 
   {
      Fr1 = Fr2 = 0;
      FgridLO[m] = gdsc_grid_c( Fsetin, &Faxnum[m], &Fcwlo[0], &Fr1 );
      FgridHI[m] = gdsc_grid_c( Fsetin, &Faxnum[m], &Fcwhi[0], &Fr2 );
   }

 


  /*
   *----------------------------------------------------------------------------
   * Get units of operation axis and data units. The axis units are determined
   * with the 'cotrans' routine (could be secundary axis units) and the data
   * units are read from the header.
   *----------------------------------------------------------------------------
   */   
   {             
      fint Fresult;
      fint n = Fsetdim - 1;
      
      fmake( Faxunits, BIGSTORE );
      Fresult = axunit_c( Fsetin, &Faxnum[n], Faxunits );
      if (Fresult == 1) 
         errorC( 4, "A cotrans error occured!" );
      else if (Fresult == 2) 
         errorC( 4, "Axis not present in set!" );
      else if (Fresult == 3) 
         errorC( 4, "Output character string not large enough!" );
      fmake( Fdataunits, BIGSTORE );
      Fr1 = 0; 
      gdsd_rchar_c( Fsetin, tofchar("BUNIT"), &Fsetlevel, Fdataunits, &Fr1 );
      if (Fr1 < 0) 
      {
         Fdataunits.l = sprintf( Fdataunits.a, "?" );
         anyoutf( 1, "Cannot find data units in header" );
      }
   }
   
   
  /*
   *----------------------------------------------------------------------------
   * Other 'lsqfit' parameters
   *----------------------------------------------------------------------------
   */
   { 
      int n;
      for (n = 0; n < (int) FnsubsI; n++) 
      { 
         xdat[n] = (float) n;                     /* X-coordinates are pixels */
         wdat[n] = 1.0;                            /* No blanks ==> Weights=1 */
      }
      xdim = 1;                                           /* Dimension of fit */
      for (n = 0; n < MAXPARALL; n++) 
         mpar[n] = 1.0;                            /* All parameters are free */
   } 
   
  /*
   *----------------------------------------------------------------------------
   * The input buffer stores data of a certain area sequentially of all
   * subsets. This data is read in a loop (over the number of input subsets).
   * If the total size of the buffer is 'MAXBUF', than in each read action a 
   * maximum of MAXBUF/(nsubs) pixels can be stored.
   *----------------------------------------------------------------------------
   */ 
   Fmaxtoread = (fint) (MAXBUF / FnsubsI);
   

  /*
   *----------------------------------------------------------------------------
   * Get physical values for operation axis.
   *----------------------------------------------------------------------------
   */
   status_c( tofchar("Calculating physical coordinates...") );
   for(subnr = 0; subnr < (int) FnsubsI; subnr++) 
   {
      /* For each grid on the operation axis, get physical value */
      physcoord[subnr] = (float) getphyscoord( Fsetin, Fsubin[subnr], subnr );
      /* sprintf(messbuf, "%d %f", subnr, physcoord[subnr]);
         anyoutf( 1, messbuf );      */
   }
   /* Calculate average gridspacing. The dispersion will be converted */
   /* from grids to physical values by multiplying with the gridspacing */
   
   gridspac = fabs( physcoord[0] - physcoord[FnsubsI-1] ) / (float) FnsubsI;
  


   initplot();
   /* Before entering the main loop, calculate min & max of phys. coords. */ 
   minmax1_c( physcoord, &FnsubsI, &Xmin, &Xmax );

   
   fmake( Fdummystr, BIGSTORE );
   quitprogram = false;
   first = true;                   /* Check whether keywords are asked before */
   getboxmessage( Fsetin, Fsubin[0], Faxnum, boxmes );
   do 
   {
     /*
      *----------------------------------------------------------------------------
      * Prepare a box for INSET. Default is a box with length 1 in every
      * direction. The size default is in BHI, the 'box' option therefore is 4.
      * Carriage return stops the program.
      *----------------------------------------------------------------------------
      */
      dfault  = REQUEST;
      Fr1     = usertext_c( Fdummystr, 
                            &dfault, 
                            KEY_BOX,
                            tofchar(boxmes) );
                               
      if (Fr1 == 0) 
         quitprogram = true;                        
      if (!quitprogram) 
      { 
         dfault = HIDDEN;
         Fboxopt = 4;
         /* Default lengths in BgridHI */
         for (i = 0; i < (int) Fsubdim; i++) 
            BgridHI[i] = 1;
         Fscrnum = 0;
         gdsbox_c( BgridLO, 
                   BgridHI, 
                   Fsetin, 
                   Fsubin,
                   &dfault, 
                   KEY_BOX, 
                   tofchar(" "), 
                   &Fscrnum, 
                   &Fboxopt );
         /* Count number of pixels in this substructure */
         Ftotpixels = 1;
         for(m = 0; m < (int) Fsubdim; m++) 
         {
            /* Number of pixels in box of one subset!! */   
            Ftotpixels *= (BgridHI[m] - BgridLO[m] + 1);
         }            
         
        /*  
         *----------------------------------------------------------------------------
         * Calculate number of pixels of underlying substructures of 1, 2, ... 
         * (subdim - 1) dimensions. Put these numbers in the vector 'modvec'. The 
         * numbers are used in the function 'postovec' to convert a one dimensional 
         * position to a 'subdim' dimensional position.
         *----------------------------------------------------------------------------
         */
         modvec[0] = 1;
         for (i = 1; i < Fsubdim; i++) 
            modvec[i] = modvec[i-1] * (BgridHI[i-1] - BgridLO[i-1] + 1);   
      
         
        /*
         *----------------------------------------------------------------------------
         * Initialize coordinate words etc. and start calculating. 
         *----------------------------------------------------------------------------
         */ 
   
         for(subnr = 0; subnr < (int) FnsubsI; subnr++) 
         {
            /* Create coordinate words for the input subsets */  
            Fcwlo[subnr] = gdsc_fill_c( Fsetin, &Fsubin[subnr], BgridLO );
            Fcwhi[subnr] = gdsc_fill_c( Fsetin, &Fsubin[subnr], BgridHI );
            /* Reset tranfer id's */
            FtidIN[subnr] = 0;
         }


         do 
         {             
            for (subnr = 0; subnr < (int) FnsubsI; subnr++) 
            {  
               offset = subnr * (int) Fmaxtoread;
               gdsi_read_c( Fsetin, 
                            &Fcwlo[subnr],    /* Data from box! */
                            &Fcwhi[subnr], 
                            &imageIN[offset],
                            &Fmaxtoread, 
                            &Fpixelsdone, 
                            &FtidIN[subnr] );         
                                      
            }
           /*------------------------------------------------------------
            * Now we have 'Fpixelsdone' small sub buffers of length 
            * 'FnsubsI'. The data is collected for each profile in the 
            * array 'amplitudes' on which the calculations are done.
            *------------------------------------------------------------
            */
            Fprofiles = Fpixelsdone;
            i = 0;
            do   /* For all profiles */
            {
               do   /* Repeat calculations same profile */
               {
                  num_blanks = 0;
                  for (subnr = 0; subnr < (int) FnsubsI; subnr++) 
                  {
                     /* collect data in new array */
                     offset = subnr * (int) Fmaxtoread;
                     if (imageIN[offset+i] == blank) 
                     {
                        num_blanks++;
                        wdat[subnr] = 0.0;
                        amplitudes[subnr] = imageIN[offset+i];
                     }
                     else
                     {
                        wdat[subnr] = 1.0;
                        amplitudes[subnr] = imageIN[offset+i];
                     }
                  }
                  
                  getparms( &Fmaxgaussians, &profilerms,
                            &critampl, &critdisp, 
                            &Fsmoothpar, &tol, &lab, &its, &vcent_user, 
                            first, &edgefit, &changed);

                  {  /* 'i' is the current one dim. position which has to
                     be converted to a n-dim. vector */
                     
                     int k;
                     int j;
                     
                     curpos = i;
                     for (k = Fsubdim - 1; k >= 0; k--) 
                     {
                        j = curpos / modvec[k];		
   	                curvec[k] = BgridLO[k] + j;
    	                curpos -= j * modvec[k];
    	             }    	          
                     strcpy( positionstr, "(" );
                     for (k = 0; k < Fsubdim ; k++) 
                     {
                        if (k < (int) Fsubdim - 1) 
                           (void) sprintf( positionstr, "%s%d,", positionstr, curvec[k] );
                        else 
                           (void) sprintf( positionstr, "%s%d)", positionstr, curvec[k] );
                     }
                  }
                  
                  Xmin = MYMIN( physcoord[0], physcoord[(int)FnsubsI-1] );
                  Xmax = MYMAX( physcoord[0], physcoord[(int)FnsubsI-1] );
                                    
                  minmax1_c( amplitudes, &FnsubsI, &Ymin, &Ymax );
                  
                  {
                     float x, y;               
                     char toplabel[256];
                     if (edgefit)
                        (void) sprintf( toplabel, "Gaussian est. (dashed),"
                                      " fit (full line) &"
                                      " edge fit (dotted) at %s", 
                                        positionstr );
                     else
                        (void) sprintf( toplabel, "Gaussian est. (dashed),"
                                      " fit (full line) at %s", 
                                        positionstr );                     
                
                     drawbox( Xmin, Xmax, Ymin, Ymax, toplabel );

                     Fcolor  = 4;                                     /* Blue */
                     pgsci_c( &Fcolor );                     
                     x = Xmin; y = 0.0;     /* Base line */
                     pgmove_c( &x, &y );
                     x = Xmax; y = 0.0;
                     pgdraw_c( &x, &y );          
                                        
                     x = Xmin; y = critampl; /* Cutoff */
                     pgmove_c( &x, &y );
                     x = Xmax; y = critampl;
                     pgdraw_c( &x, &y );
                  }
     
                  Fsymbol = 3;            
                  Fcolor  = 1;                          /* Default foreground */
                  pgsci_c( &Fcolor );
                  pgpt_c( &FnsubsI, physcoord, amplitudes, &Fsymbol );

                  if (num_blanks < FnsubsI-6) 
                  {                            /* For each gauss 3 parameters */
                     Fmaxparameters = MAXPAR * Fmaxgaussians; 
                     Fgaussians = gauest_c( amplitudes, 
                                            workdummy, 
                                            &FnsubsI, 
                                            estimates, 
                                            &Fmaxparameters, 
                                            &profilerms, 
                                            &critampl, 
                                            &critdisp, 
                                            &Fsmoothpar );
                  }
                  else 
                  {
                     Ferrlev = 4;
                     error_c( &Ferrlev, tofchar("Profile contains too many blanks") );
                  }
            
                  if (Fgaussians > Fmaxgaussians) 
                     Fgaussians = Fmaxgaussians;
                  if (Fgaussians == 0) 
                     anyoutf( 1, "No estimates found!" );
                     
                  npar = Fgaussians * MAXPAR;
                  Fnpoints = PLOTPOINTS;
                  { /* Store estimates: */
                     int j;
                     for (j = 0; j < (int) Fgaussians; j++) 
                     {
                        int offst = j * MAXPAR;
                        storeparms[0+offst] = estimates[0+offst];
                        storeparms[1+offst] = estimates[1+offst];
                        storeparms[2+offst] = estimates[2+offst];
                     }
                  }
                  
                    
                  /* Create array with Gaussian values based on these esitmates */                      
                  { 
                     int   j;
                     float x;
                     float delta;
         
                     x = 0.0;
                     /* 'x' is a non-integer index, to be converted to an integer */
                     /* in de function 'physcoord' */
                     delta = (float) (FnsubsI - 1) / (float) Fnpoints;
                     for (j = 0; j < (int) Fnpoints; j++) 
                     {
                        Xplot[j] = tophys( physcoord, x);
                        Yplot[j] = func_c( &x, estimates, &npar, &Fgaussians );
                        /* sprintf(messbuf, "%d %f %f delta=%f", j, Xplot[j], Yplot[j], x);
                           anyoutf( 1, messbuf ); */
                        x += delta;
                     }
                  }
                     
                  Fstyle = DASHED; pgsls_c( &Fstyle );         
                  Fcolor = 2;                                          /* Red */
                  pgsci_c( &Fcolor );
                  pgline_c( &Fnpoints, Xplot, Yplot );
                                       
                   
                  iters = lsqfit_c( xdat,          /* X-coordinates */
                                    &xdim,         /* Dimension of fit */
                                    amplitudes,    /* Y data */
                                    wdat,          /* Weights */
                                    &FnsubsI,      /* Number of data points */
                                    estimates,     /* Initial values */
                                    epar,          /* Error return values */
                                    mpar,          /* All parameters free */
                                    &npar,         /* Total number of parameters */
                                    &tol,          /* Tolerance */ 
                                    &its,          /* Max. num. of iterations */
                                    &lab,          /* Mixing parameter */
                                    &Fgaussians ); /* Number of gaussians */
                                                     
   
   
                  if (iters < 0) 
                  {
                     nofit = true;
                     /* errormess(iters); */
                     Fstyle = FULL_LINE; pgsls_c( &Fstyle );/* Reset line style */
                  }
                  else {
                     nofit = false;
                          
                     /* Create array with Gaussian values based on the fit */
                     {
                        int   j;
                        float x;
                        float delta;
                     
                        x = 0.0;
                        delta = (float) (FnsubsI - 1) / (float) Fnpoints;
                        for (j = 0; j < Fnpoints; j++) 
                        {
                           Xplot[j] = tophys( physcoord, x);
                           Yplot[j] = func_c( &x, estimates, &npar, &Fgaussians );
                           x += delta;
                        }
                     }
                     Fstyle = FULL_LINE; pgsls_c( &Fstyle );
                     Fcolor = 3;                                     /* Green */
                     pgsci_c( &Fcolor );
                     pgline_c( &Fnpoints, Xplot, Yplot );
                  }
                                 
                                
                  /*
                   *-----------------------------------------------------------
                   * Do the edge fitting. First examine if there is a gaussian
                   * found in the estimates. If there is more than one, 
                   * determine which one has the greatest amplitude (estimate
                   * index 0). Compare its central value (velocity) with
                   * the value given in CENTVAL=. Select all data values
                   * from this point to the edge, increasing the distance 
                   * to the position in CENTVAL=
                   *-----------------------------------------------------------
                   */
                                                  
                   if ( Fgaussians > 0 && edgefit )
                   {
                      float    maxampl         = storeparms[0];
                      float    pos_on_profile  = storeparms[1];
                      float    dispers         = storeparms[2];
                      int      j;
                      float    xdat_edge[SUBSMAX];
                      float    amplitudes_edge[SUBSMAX];
                      float    physcoord_edge[SUBSMAX];
                      fint     Fedgecount;
                     
                      /* Determine gauss with greatest amplitude */
                      for (j = 1; j < (int) Fgaussians; j++) 
                      {
                         int offst = j * MAXPAR;
                         if (storeparms[0+offst] > maxampl) 
                         {
                            maxampl = storeparms[0+offst];
                            pos_on_profile  = storeparms[1+offst];
                            dispers = storeparms[2+offst];
                         }
                      }
                      /* 'vcent_user' is in physical coordinates */
                      /* Convert position of max. ampl. to phys. coord. */
                      if (tophys(physcoord, pos_on_profile) > vcent_user) 
                      {
                         Fedgecount = 0;
                         for (j = 0; j < (int) FnsubsI; j++) 
                         {
                            if (tophys(physcoord, xdat[j]) >= 
                                tophys(physcoord, pos_on_profile) ) 
                            {
                               xdat_edge[Fedgecount] = xdat[j];
                               amplitudes_edge[Fedgecount] = amplitudes[j];
                               physcoord_edge[Fedgecount++] = tophys( physcoord, xdat[j]);
                            }
                         }
                      }
                      if (tophys(physcoord, pos_on_profile) <= vcent_user) 
                      {
                         Fedgecount = 0;
                         for (j = 0; j < (int) FnsubsI; j++) 
                         {
                            if (tophys(physcoord, xdat[j]) <= 
                                tophys(physcoord, pos_on_profile) ) 
                            {
                               xdat_edge[Fedgecount] = xdat[j];
                               amplitudes_edge[Fedgecount] = amplitudes[j];
                               physcoord_edge[Fedgecount++] = tophys( physcoord, xdat[j]);
                            }
                         }
                      }
                      /* At this point we have two new arrays with fit data */
                      /* and a set of initial estimates for one(!) Gaussian */
                      /* First plot these points, and make a lsqfit then.   */
                      
                      Fsymbol = 23;
                      Fcolor   = 4;                                   /* Blue */
                      pgsci_c( &Fcolor );
                      pgpt_c( &Fedgecount, physcoord_edge,
                               amplitudes_edge, &Fsymbol );
                      
                      /* Estimates for lsq 'edge' fit */                     
                      edge_estimates[0] = maxampl;
                      edge_estimates[1] = pos_on_profile;
                      edge_estimates[2] = dispers;
                                          
                      Fgaussians_edge = 1;
                      npar = 3 * Fgaussians_edge;
                      
                      iters = lsqfit_c( xdat_edge,       /* X-coordinates */
                                        &xdim,           /* Dimension of fit */
                                        amplitudes_edge, /* Y data */
                                        wdat,            /* Weights */
                                        &Fedgecount,     /* Number of data points */
                                        edge_estimates,  /* Initial values */
                                        epar,            /* Error return values */
                                        mpar,            /* All parameters free */
                                        &npar,           /* Total number of parameters */
                                        &tol,            /* Tolerance */ 
                                        &its,            /* Max. num. of iterations */
                                        &lab,            /* Mixing parameter */
                                        &Fgaussians_edge ); /* Number of gaussians */
                                        
                       
                      if (iters < 0) 
                      {
                         edgefit = false;
                         /* errormess(iters);  */
                         
                         Fstyle = FULL_LINE; pgsls_c( &Fstyle );     /* Reset line style */
                      }
                      else 
                      {
                         edgefit = true;
                           
                         /* Create array with Gaussian values based on the fit */
                         {
                            int   j;
                            float x;
                            float delta;
                       
                    
                            Fnpoints = PLOTPOINTS;  
                            x = 0.0;
                            delta = (float) (FnsubsI - 1) / (float) Fnpoints;
                            for (j = 0; j < Fnpoints; j++) 
                            {
                               Xplot[j] = tophys( physcoord, x);
                               Yplot[j] = func_c( &x, 
                                                  edge_estimates, 
                                                  &npar, 
                                                  &Fgaussians_edge );
                               x += delta;
                            }
                         }
                         Fstyle = DOTTED; pgsls_c( &Fstyle );
                         Fcolor = 7;                                /* Yellow */
                         pgsci_c( &Fcolor );
                         pgline_c( &Fnpoints, Xplot, Yplot );
                      }                                                     
                      /* Reset plot style */
                      Fstyle = FULL_LINE; pgsls_c( &Fstyle );
                   }

                  putinlogfile( Fgaussians, 
                                positionstr, 
                                storeparms, 
                                estimates, 
                                edge_estimates,
                                nofit,
                                edgefit,
                                first );
                  
                  first = false;           /* Keywords are hidden from now on */
                
                  /* Put status of keywords in log file if one or more */
                  /* changed value */  
                  if (changed) 
                  {
                     sprintf( longbuf,                      
                             "ESTRMS=%-7.2f ESTCUTAMP=%-7.2f "
                             "ESTCUTDISP=%-7.2f Q=%d  NGAUSS=%d "
                             "TOLERANCE=%-7.2f CENTVAL=%-7.2f",
                              profilerms,
                              critampl,
                              critdisp*gridspac,
                              (int) Fsmoothpar,
                              (int) Fmaxgaussians,                              
                              tol,
                              vcent_user );
                     anyoutf( 3, longbuf ); 
                  }                                      
                                     
                  
                  dfault = REQUEST;
                  fmake( Fcontinue, BIGSTORE );
                  Fcontinue.a[0] = 'N';
                  cancel_c( KEY_PROFILE );                   
                  Fr1 = usertext_c( Fcontinue, 
                                    &dfault, 
                                    KEY_PROFILE, 
                                    MES_PROFILE );
                  if ( (toupper(Fcontinue.a[0]) == 'A') ||
                       (toupper(Fcontinue.a[0]) == 'Q') ) 
                  {
                      pgend_c();
                      finis_c();                               /* Quit Hermes */
                  }
                  sameprof = (toupper(Fcontinue.a[0]) == 'S');     
                  newbox = (toupper(Fcontinue.a[0]) == 'B');
                  if (newbox) 
                  {
                     sameprof = false;
                     FtidIN[0] = 0;
                  }                      
               } while (sameprof);
               if (toupper(Fcontinue.a[0]) == 'P') 
               {
                  /* user wants previous profile */
                  i--;
                  if (i < 0) i = 0;
               }
               else 
                  i++;                                  /* Go to next profile */
               if (newbox) 
                  i = (int) Fprofiles;                  /* User wants new box */
            } while ( i < (int) Fprofiles );
         } while (FtidIN[0] != 0);                    /* All pixels examined? */
      }
      cancel_c( KEY_BOX );
   } while(!quitprogram);
   
   pgend_c();
   finis_c();                                                  /* Quit Hermes */
   return( 0 );  
}

