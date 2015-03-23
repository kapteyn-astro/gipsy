/*
                           COPYRIGHT (c) 1995
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             gaufit.dc1

Warning:       This program is not maintained anymore. In the near future, 
               it will be removed from the set of GIPSY tasks. An
               improved version with graphical user interface is called 
               XGAUFIT.
               
Program:       GAUFIT

Purpose:       GAUFIT estimates parameters for multi component one
               dimensional gauss functions using data in selected
               profiles. It can use these estimates to fit the
               parameters to the data in a least squares fit.

Category:      ANALYSIS, PROFILES, FITTING

File:          gaufit.c

Author:        M. Vogelaar

Keywords:

 
   STARTNEW=   Start improved version of this program?           [Y]/N
   
               A new version with graphical user interface is called 
               XGAUFIT. You can start it here.
               

   INSET=      Give set and profile direction:

               Set (subsets) and one operation axis which is the set
               axis that indicates the direction of the profile.
               The subsets must be a contiguous block. This is
               checked by the program.
               (Maximum number of subsets is 2048.)


   BOX=        Area of operation:                       [entire subset]


   OPTION=     Option(s) for output:                     [list options]

               This input determines the size of your output set
               and what is stored in the output:
               0 Initial estimate(s)
               1 Fitted gaussian(s)
               2 Errors on the fitted parameters (See description.)


   NGAUSS=     Maximum number of gaussians in profile:              [1]

               (The maximum number is 8.)


   OUTSET=     Give output set (subsets):

               You need only to give a name. The number of output
               subsets depends on OPTION= and the maximum number of
               gaussians fitted to the profile (see description).


               The calculation of the initial estimates.
               =========================================
               The fit routine in GAUFIT needs initial estimates for
               the parameters. These estimates can be read from set
               (ESTSET=) or can be calculated by the program using
               a method described by Schwarz, 1968, Bull.Astr.Inst.
               Netherlands, Volume 19, 405. The associated routine
               needs the r.m.s. noise in the profile (ESTRMS=) and
               a smoothing parameter (Q=). Estimates can be rejected
               before gaussians are fitted with the hidden keywords
               ESTCUTAMP= and ESTCUTDISP=. The default is that no
               estimates are rejected.


   ESTSET=     Input set/subsets with estimates for fit:    [calculate]

               Keyword is asked hidden if 0 (Initial estimate(s)) was
               part of the list in OPTION=

               The number of subsets must be 3*NGAUSS=
               For each gauss the subsets contain:
               1:  Estimated amplitudes of component 1
               2:  Estimated centre of component 1
               3:  Estimated dispersion of component 1

               4:  Estimated amplitudes of component 2
               5:  .... etc.


   ESTRMS=     Give r.m.s. noise level in profiles:

               (Only asked if no ESTSET= is present)
               The r.m.s. noise is used in the calculation of the
               initial estimates for determining the region in the
               profile with signal.


   NCORR=      Give Number of correlated subsets:                 [1.0]

               If Hanning smoothing has been applied, the subsets are
               two by two correlated as is specified by the default.
               This is important to correct the r.m.s. noise for making
               the initial estimates, and also to get correct errors
               on the parameters; they are a factor sqrt(NCORR=) higher
               if the subsets are correlated.


** ESTCUTAMP=  Lower cutoff for amplitude of est. in <units>:     [0.0]

               (Only asked if no ESTSET= is present)
               Initial estimates below this amplitude are discarded.


** ESTCUTDISP= Lower cutoff for dispersion of est. in <units>: [minimum]

               (Only asked if no ESTSET= is present)
               Initial estimates with a dispersion below this value
               are discarded.
               The default is the equivalent of 1 pixel. If a value
               entered by the user corresponds to a value in pixels
               less than 1, the cutoff value is set to the
               equivalent of 1 pixel (usually the channel separation).


   Q=          Give smoothing parameter:                            [2]

               Number of data points used in the calculation of the
               initial estimate is 2Q + 1 around the peak.
               Q= accepts more than one value. If the fitting fails
               with the first value, it tries again with the next
               etc. Example: Q=3 4 5 2


               Calculation of the fitted parameters.
               =====================================
               The actual fitting is a least-squares fit of a function
               to a set of data points. The method used is described
               in: Marquardt, J.Soc.Ind.Appl.Math. 11, 431 (1963).
               It is a mixture of the steepest descent method and the
               Taylor method.  The keywords CUTAMP= and CUTDISP= are
               filters used after fitting the profiles.


   CUTAMP=     Lower cutoff for fitted amplitude in <units>:      [0.0]

               Fitted Gaussians with amplitude lower than this value
               are rejected. The units are generated by the keyword
               prompt.


   CUTDISP=    Lower cutoff for fitted dispersion in <units>:     [0.0]

               Gaussians with dispersion lower than this value are
               rejected. The units are generated by the keyword
               prompt.


   TOLERANCE=  Fractional tolerance for the chi square:           [0.0]

               Fitting is stopped when successive iterations fail
               to produce a decrement in reduced chi-squared less
               than TOLERANCE=   The value cannot be less than a
               certain minimum as set by the system. This means that
               maximum accuracy can be obtained with TOLERANCE=0.0

               If you have profiles with a lot of signal, you can
               increase the tolerance (e.g. 0.01, or 0.1).


 **MAXITS=     Maximum number of iterations in fit:                [50]

               If this number is exceeded, no parameters could be
               fitted and blanks are written to the output set.


 **LAB=        Value for mixing parameter:                      [0.001]

               Mixing parameter in the least squares fit function.
               LAB= determines the initial weight of steepest descent
               method relative to the Taylor method. LAB= should be
               a small value (i.e. 0.001).


               Sorting of gaussians.
               =====================

   SORT=       Sort output-mode:                              [0]/1/2/3

               (Only asked if option 1 and/or 2 is selected)
               0: Do NOT sort.
               1: Sort components in DECREASING order of peak value.
               2: Sort components in INCREASING order of distance
                  to user given value on operation axis (often the
                  central velocity) as given in CENTVAL=
               3: Sort components in DECREASING order of dispersion.


  CENTVAL=     Give centre for sorting in <units>:         [calculated]

               Only asked if SORT=2. Sorting is with respect to
               this value. The default is the physical coordinate
               of the centre of the operation axis.
               The units are generated by the keyword prompt.



Description:   The decomposition of profiles into Gaussian components
               can be a powerful method in the study of the kinematics
               of neutral hydrogen.

               This program calculates estimates (or reads them from
               another set) and fits multi component gaussians. If
               the estimates are not supplied, they are calculated
               by fitting second degree polynomials to the profiles
               (See: Schwarz, 1968, Bull.Astr.Inst. Netherlands,
               Volume 19, 405-413).

               The errors on the fitted parameters are calculated as
               real errors by means of the covariance matrix in the fit
               and the average deviation between data and fit over the
               entire profile. However, if the region with signal is
               short compared to the full length of the profile, the
               average deviation between data and fit is primarily
               determined along the part of the baseline where no signal
               is present and then it becomes almost equal to the noise
               in the profile and the real errors determined this way
               virtually become formal errors.


               How to use the TOLERANCE= keyword:
               =================================

               Usually only signal is present in a small region in the
               profile while the chi square is calculated over the entire
               baseline. In this case the Chi square primarily represents
               the fitting of the baseline instead of the gauss and then
               a very tiny decrease of the chi square might very well be
               significant for the fitting of the gauss to the signal.
               So one should be very careful not setting TOLERANCE= too
               high. If TOLERANCE=0 then the minimization is stopped at
               machine precision which is of the order of 1e-5.

Example:       In order to fit gaussians to profiles you need to
               specify first the input set and the subsets (INSET=).
               The number of subsets must be greater than 1 and less
               than 2048 and the subsets are specified by the name of
               the axis in the direction of your profiles.

               Suppose you have a set NGC3198 with axes:
               RA-NCP             from  -134 to   115
               DEC-NCP            from  -116 to   135
               FREQ-OHEL          from     2 to   116

               and you want to create an output set where the subsets
               represent the estimated and the fitted parameters
               (amplitude, central value [central velocity] and
               dispersion) of gaussians using data in frequency
               direction (in this case velocities). You allow a maximum
               of two gaussians in a profile and want to sort the
               output in order of decreasing amplitude.

               Then:
               INSET=NGC3198 freq
               BOX=
               OPTION=0 1 2
               SORT=1
               NGAUSS=2

               You have to know the r.m.s. level of the noise in your
               profiles otherwise it will be impossible to find reasonable
               estimates. E.g. ESTRMS=0.21 (in units of your image data).
               This value is modified if the subsets are correlated.
               For instance with NCORR=2 the value for the r.m.s. is
               adjusted and also the errors on the fitted parameters will
               be modified. For the smoothing parameters the array
               Q=2 1 3 4 5 is used. So we can add the keywords:

               ESTRMS=0.21
               NCORR=2
               Q=2 1 3 4 5

               Next the set for the output must be supplied (OUTSET=).
               If it did not exist the program will create a new set
               with the FREQ axis replaced by a PARAM axis. Its length
               depends on the selected options for output (OPTION=) and
               the maximum number of gaussians allowed to be fitted to
               one profile (NGAUSS=) as follows:

               For each individual estimated gaussian, fitted gaussians and
               set of errors on its parameters 3 output subsets are reserved.
               Suppose you selected:

               OUTSET=NGC3198PARAM

               Then your NGC3198PARAM subsets will contain:

               param 1:  Estimated amplitudes of component 1
               param 2:  Estimated centre of component 1
               param 3:  Estimated dispersion of component 1

               param 4:  Estimated amplitudes of component 2
               param 5:  Estimated centre of component 2
               param 6:  Estimated dispersion of component 2

               param 7:  Fitted amplitudes of component 1
               param 8:  Fitted centre of component 1
               param 9:  Fitted dispersion of component 1

               param 10: Fitted amplitudes of component 2
               param 11: Fitted centre of component 2
               param 12: Fitted dispersion of component 2

               param 13: Error in fitted amplitudes of component 1
               param 14: Error in fitted centre of component 1
               param 15: Error in fitted dispersion of component 1

               param 16: Error in fitted amplitudes of component 2
               param 17: Error in fitted centre of component 2
               param 18: Error in fitted dispersion of component 2

               If the components are sorted in amplitude then subset 1
               contains the highest amplitude estimates (Schwarz method)
               and 7 contains the highest fitted amplitudes (Least
               squares method). If no parameters could be estimated or
               fitted then the components are set to blank.

               The sizes of the other axes of the output are copied
               from the input and the choice for BOX= does not influence
               their sizes. If the set already existed, the program will
               only write output into the region supplied by the user
               in BOX=. This way an already existing set with estimates
               or fits can be improved by overwriting the contents in BOX=
               (For example with other initial estimates or different Q=,
               TOLERANCE= or MAXITS=)


               The output of a GAUFIT run:
               ==========================

               Profiles from set:  NGC3198
               Results in set:     NGC3198PARAM
               param 1: Estimated amplitudes of component 1
               param 2: Estimated centre of component 1
               ....
               ....
               Max. number of gaussians in profile is 2
               Range operation axis: [393.465,865.341] KM/S
               Fitted gaussians will have amplitude > 0.000000 W.U.
               Fitted gaussians will have dispersion > 0.000000 KM/S
               One pixel on operation axis has (average) width 4.139263 KM/S
               Processed 63000 profiles in 1998.01 sec (1730.10 cpu sec)
               Number of profiles fitted with Q= 2:  55816
               Number of profiles fitted with Q= 1:  3325
               Number of profiles fitted with Q= 3:  740
               Number of profiles fitted with Q= 4:  177
               Number of profiles fitted with Q= 5:  56
               For 2872 profiles no estimate was found.
               For 2886 profiles the fitting failed.
               421 times the max. no. iterations was exceeded.


               In general, the program will work for profiles in any
               direction and instead of velocity you could think of any
               kind of physical coordinate. The program automatically
               inserts the correct units in the header of the
               output set.

Literature:    An alternative least squares method is described in:
               1968, Bull.Astr.Inst. Netherlands, Volume 118, 465-487.


Updates:       Apr 22, 1991: VOG, Document created.
               Mrt 15, 1994: VOG, Separate cutoff keywords in cutoffs
                                  for estimate- and lsqfit routine.
               Oct 18, 1994: VOG, Included FJ Sickings NCORR= keyword and
                                  looping over numbers in Q= array.
               Mar 03, 1995: VOG, Keep floats between -FLT_MAX and FLT_MAX.
                                  Sorting of estimates allowed.
               Jan 08, 1997: VOG, New minimum for estimated dispersion in 
                                  gauest_c. Bug removed in increasing 'qcount'.
                                  (program crashed on alpha only).
               Nov 30, 1998: VOG, Set initial value of 'ncorr' to 1 in 
                                  all cases.

#<

*/

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "math.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"
#include "assert.h"
#include "float.h"
#include "gdsinp.h"
#include "gdsasn.h"
#include "gdscpa.h"
#include "gdsout.h"
#include "gdsc_ndims.h"
#include "setfblank.h"
#include "myname.h"
#include "anyout.h"
#include "userfio.h"
#include "nelc.h"
#include "cancel.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsbox.h"
#include "gdsc_fill.h"
#include "gdsi_read.h"
#include "gdsi_write.h"
#include "userint.h"
#include "userlog.h"
#include "usertext.h"
#include "cotrans.h"
#include "cancel.h"
#include "gdsd_rchar.h"
#include "error.h"
#include "stabar.h"
#include "axunit.h"
#include "gdsc_name.h"
#include "userreal.h"
#include "userdble.h"
#include "userlog.h"
#include "userint.h"
#include "reject.h"
#include "getrange.h"
#include "minmax3.h"
#include "wminmax.h"
#include "statr.h"
#include "clipper.h"
#include "presetd.h"
#include "gdsd_wchar.h"
#include "gdsd_wvar.h"
#include "gauest.h"
#include "lsqfit.h"
#include "status.h"
#include "time.h"
#include "timer.h"                  /* Used for timing the calculations. */
#include "clspfp.h"
#include "axtype.h"
#include "gdsd_rchar.h"
#include "deputy.h" 

#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )

#define AXESMAX    10               /* Max. allowed number of axes in a set */
#define SUBSMAX    2048             /* Max. number of subsets. */
#define MAXBUF     32*4096          /* Buffer size for I/O */
#define MAXOPTIONS 3                /* Total numer of possible options */
#define MAXGAUSS   8                /* Max. number of gaussians in profile */
#define MAXPAR     3*MAXGAUSS       /* 3 parameters per gaussian */
#define MAXOUT     MAXOPTIONS*MAXPAR
#define MAXEST     MAXOPTIONS*MAXGAUSS
#define MAXQPARS   16
#define LAMBDA     0.001            /* Mixing parameter for lsq fit */
#define BIGSTORE   160              /* Length of a string */
#define FITSLEN    20
#define VERSION    "2.0"            /* Version number of this program */
#define NONE       0                /* Default values for use in userxxx routines */
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define false      0
#define true       1


/* Keywords and messages */

#define KEY_INSET         tofchar("INSET=")
#define MES_INSET         tofchar("Give set and profile direction:" )
#define KEY_NCORR         tofchar("NCORR=")
#define MES_NCORR         tofchar("Give number of correlated subsets:  [1.0]")
#define KEY_OUTSET        tofchar("OUTSET=")
#define MES_OUTSET        tofchar("Give output set (subsets):")
#define KEY_ESTSET        tofchar("ESTSET=")
#define MES_ESTSET        tofchar("Input set/subsets with estimates for fit:    [calculate]")
#define KEY_BOX           tofchar("BOX=")
#define MES_BOX           tofchar(" ")
#define KEY_OPTION        tofchar("OPTION=")
#define MES_OPTION        tofchar("Give option(s) 0,1,2 [list options]")
#define KEY_TOLERANCE     tofchar("TOLERANCE=")
#define MES_TOLERANCE     tofchar("Fractional tolerance for chi square:   [0.0]")
#define KEY_LAB           tofchar("LAB=")
#define KEY_ESTRMS        tofchar("ESTRMS=")
#define MES_ESTRMS        tofchar("Give r.m.s. noise level in profiles:")
#define KEY_ESTCUTAMP     tofchar("ESTCUTAMP=")
#define KEY_ESTCUTDISP    tofchar("ESTCUTDISP=")
#define KEY_CUTAMP        tofchar("CUTAMP=")
#define KEY_CUTDISP       tofchar("CUTDISP=")
#define KEY_Q             tofchar("Q=")
#define MES_Q             tofchar("Give smoothing parameter:     [2]")
#define KEY_SORT          tofchar("SORT=")
#define KEY_NGAUSS        tofchar("NGAUSS=")
#define MES_NGAUSS        tofchar("Give max. number of gaussians in profile [1]")
#define KEY_MAXITS        tofchar("MAXITS=")
#define MES_MAXITS        tofchar("Maximum number of iterations in fit:   [50]")

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

typedef struct        /* 'qsort' struct for comparisons */
{
   int indx;
   float number;
} sortnum;


/* Input of set, subsets: */

static fchar    Setin;                 /* Name of the set */
static fint     subin[SUBSMAX];        /* Array for the subset coordinate words */
static fint     nsubsI;                /* Number of input subsets */
static double   ncorr;                 /* Number of correlated subsets */

static fint     axnum[AXESMAX];        /* GDSINP axis numbers array */
static fint     axcount[AXESMAX];      /* GDSINP axis lengths array */
static fint     setdim;                /* Dimension of the set */
static fint     subdim;                /* Dimension of the subset */
static fint     scrnum = 8;            /* Destination of log output */
static fint     maxaxes = AXESMAX;     /* Convert parameters to variables */
static fint     maxsubs = SUBSMAX;     /* Max. allowed number of subsets */
static fint     setlevel = 0;          /* Indicate set level */
static fint     options[MAXOPTIONS];


/* Related to set with estimates */

static fchar    Estimateset;
static fint     subest[MAXEST];
static fint     nsubsE;                /* Number of estimate input subsets */
static fint     axnumE[AXESMAX];       /* GDSINP axis numbers array */
static fint     axcountE[AXESMAX];     /* GDSINP axis lengths array */
static fint     subdimE;               /* Dimension of the subset with estimates */
static int      estimateset;           /* Is a set with estimates available? */


/* Output set related */

static fchar    Setout;                /* Name of the set */
static fint     subout[MAXOUT];        /* Array for the subset coordinate words */
static fint     nsubsO;                /* Number of input subsets */
static fint     axnumO[AXESMAX];       /* GDSINP axis numbers array */
static fint     axcountO[AXESMAX];     /* GDSINP axis lengths array */


/* Input of area etc.:*/

static fint     cwlo[SUBSMAX];         /* Coordinate words */
static fint     cwhi[SUBSMAX];
static fint     cwloOUT[MAXOUT];
static fint     cwhiOUT[MAXOUT];
static fint     cwloEST[MAXEST];
static fint     cwhiEST[MAXEST];
static fint     gridLO[AXESMAX];       /* Coordinate words for frame */
static fint     gridHI[AXESMAX];
static fint     BgridLO[AXESMAX];      /* Coordinate words for box */
static fint     BgridHI[AXESMAX];
static fint     gridvect[AXESMAX];


/* Data transfer: */


static fint     tidIN[SUBSMAX];
static fint     tidEST[MAXEST];
static fint     tidOUT[MAXOUT];        /* Transfer id's */
static float    imageIN[MAXBUF];       /* Multiple buffer for all subsets */


/* Related to update of header etc: */

static float    datamin[MAXOUT];
static float    datamax[MAXOUT];
static fint     nblanks[MAXOUT];
static fchar    Axunits;
static fchar    Dataunits;



/* 'lsqfit' related */

static float    tol;
static float    lab;
static float    wdat[SUBSMAX];
static fint     its;
static fint     mpar[MAXPAR];
static float    estimates[MAXPAR];
static float    epar[MAXPAR];
static fint     xdim;
static fint     npar;
static fint     iters;


/* 'gauest' related */

static float    amplitudes[SUBSMAX];
static float    workdummy[SUBSMAX];
static float    estcritampl = 0.0;
static float    estcritdisp = 0.0;
static float    ecdisppix = 0.0;
static float    estrms   = 0.0;
static float    critampl = 0.0;
static float    critdisp = 0.0;
static fint     smoothpar[MAXQPARS];
static int      nQfit[MAXQPARS];
static fint     maxQpars;
static int      qcount;
static fint     maxparams;
static fint     gaussians;
static fint     maxgaussians;


/* Output buffers */

static float    gauest_ampl[MAXGAUSS*MAXBUF];
static float    gauest_cent[MAXGAUSS*MAXBUF];
static float    gauest_disp[MAXGAUSS*MAXBUF];
static float    lsq_gauss_ampl[MAXGAUSS*MAXBUF];
static float    lsq_gauss_cent[MAXGAUSS*MAXBUF];
static float    lsq_gauss_disp[MAXGAUSS*MAXBUF];
static float    err_gauss_ampl[MAXGAUSS*MAXBUF];
static float    err_gauss_cent[MAXGAUSS*MAXBUF];
static float    err_gauss_disp[MAXGAUSS*MAXBUF];


/* Sorting */

static fint     sort;
static float    centval;


/* Miscellaneous: */

static float    blank;                 /* Value of system blank */
static char     messbuf[BIGSTORE];     /* Buffer for text message */
static fint     mcount[MAXOUT];        /* Counter for 'minmax3' routine */
static int      optionflag[MAXOPTIONS];
static float    physcoord[SUBSMAX];    /* X data in lsqfit */
static float    gridcoord[SUBSMAX];


/* Conversion float <--> double. */

static float    fltmax;                /* maximum floating-point number */
static float    flteps;                /* smallest number x such that 1.0+x != 1.0 */
static double   dfltmax;               /* 'fltmax' converted to double */
static double   maxarg;                /* Max. arg for exp function so that */
                                       /* result > -dfltmax. */
static double   exparg[MAXGAUSS];      /* Store part of function calculation */



static void anyoutC( int dev, char *anyCstr )
/*------------------------------------------------------------*/
/* The C version of 'anyout_c' needs two parameters:          */
/* an integer and a C-type string. The integer determines     */
/* the destination of the output which is:                    */
/*    0  use default [set by HERMES to 3 but can be changed   */
/*       by user]                                             */
/*    1  terminal                                             */
/*    2  LOG file                                             */
/*    8  terminal, suppressed in "experienced mode"           */
/*   16  terminal, only when in "test mode"                   */
/*------------------------------------------------------------*/
{
   fint ldev = (fint) dev;
   anyout_c( &ldev, tofchar( anyCstr ) );
}



static void errorC( int   level,
                    char *str )
/*------------------------------------------------------------*/
/* The C version of 'error'.                                  */
/*------------------------------------------------------------*/
{
    fint   flev = (fint) level;
    error_c( &flev, tofchar( str ) );
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



static void optionmenu( void )
/*------------------------------------------------------------*/
/* PURPOSE: Show user Main menu with options.                 */
/*------------------------------------------------------------*/
{
   anyoutC(1, "           ======GAUFIT OPTIONS======");
   anyoutC(1, " 0 Store calculated initial estimates for gauss parameters." );
   anyoutC(1, " 1 Store fitted gauss parameters." );
   anyoutC(1, " 2 Store errors in fitted gauss parameters." );
}


#ifdef MYERRORMESSAGE
static void errormess( int result )
{
   if (result == -1) anyoutC(1,  "Too many free parameters, maximum is 32." );
   if (result == -2) anyoutC(1,  "No free parameters." );
   if (result == -3) anyoutC(1,  "Not enough degrees of freedom." );
   if (result == -4) {
      anyoutf( 1,  "Maximum number of iterations too small to ..." );
      anyoutf( 1,  "... obtain a solution which satisfies TOL." );
   }
   if (result == -5) anyoutC(1,  "Diagonal of matrix contains elements which are zero." );
   if (result == -6) anyoutC(1,  "Determinant of the coefficient matrix is zero." );
   if (result == -7) anyoutC(1,  "Square root of negative number." );
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


static double getphyscoord( fchar  Setin,
                            fint   subsetcw,
                            fint   *axnum,
                            float  *grid )
/*------------------------------------------------------------*/
/* PURPOSE: This function returns the physical coordinate of  */
/*          a grid position on the operation axis.            */
/* INPUT:   Setin:    The name of the input set.              */
/*          subsetcw: Coordinate word which defines the       */
/*          subset part of the set.                           */
/* OUTPUT:  grid:     The grid value of this subset.          */
/*          Result is the physical coordinate of the grid.    */
/*------------------------------------------------------------*/
{
   fint    grid2phys = 1;            /* grid coord. -> physical coord. */
   double  coordin[AXESMAX];         /* grids before cotrans */
   double  coordout[AXESMAX];        /* Phys. coords after cotrans */
   fint    notrans;
   fint    setlevel = 0;
   fint    r1;
   int     i, n;
   int     axisnr;

   setdim = gdsc_ndims_c( Setin, &setlevel );
   n  = setdim - 1;                  /* Number of last axis in 'axnum' */
   r1 = 0;

   *grid = (float) gdsc_grid_c( Setin,   /* Extract a grid value from a coord.word */
                                &axnum[n],
                                &subsetcw,
                                &r1 );

   for (i = 0; i < setdim; i++)
      coordin[i] = 0.0;

   axisnr = axnum[n] - 1;
   coordin[axisnr] = (double) *grid;

   notrans = cotrans_c( Setin,
                        &subsetcw,
                        coordin, coordout,
                        &grid2phys );
   if (notrans)
      errorC( 4, "Cannot find physical coordinates!. Aborting task..." );

   return( coordout[axisnr] );
}



static float tophys( float  x,
                     fint   *gridvect,
                     fint   setdim,
                     fint   *axnum )
/*------------------------------------------------------------*/
/* PURPOSE: Transform a grid to a physical coordinate.        */
/* INPUT:   x: the grid value.                                */
/*          setdim: dimensionality of cube.                   */
/*          gridvect: is a vector with n coordinates and n is */
/*                    the dimension of the subset.            */
/*          axnum: Axis number array.                         */
/* OUTPUT:  Corresponding physical coordinate.                */
/*------------------------------------------------------------*/
{
   fint    grid2phys = 1;            /* grid coord. -> physical coord. */
   double  coordin[AXESMAX];         /* grids before cotrans */
   double  coordout[AXESMAX];        /* Phys. coords after cotrans */
   fint    notrans;
   fint    setlevel = 0;
   int     i, n;


   for (i = 0; i < setdim - 1; i++)
      coordin[axnum[i]-1] = (double) gridvect[i];

   n = axnum[setdim-1] - 1;
   coordin[n] = (double) x;


   notrans = cotrans_c( Setin,
                        &setlevel,
                        coordin, coordout,
                        &grid2phys );
   if (notrans)
      errorC( 4, "COTRANS function cannot transform to grids!" );

   return ( (float) coordout[n] );
}



static void writeoutput( float *data, fint ndat, int paramnr )
/*------------------------------------------------------------*/
/* PURPOSE: Write data to output subsets.                     */
/*                                                            */
/* Most of the arrays are global variables! The length of the */
/* arrays is 'ndat' and the contents of 'paramnr' is the      */
/* subset to write to.                                        */
/*------------------------------------------------------------*/
{
   fint   pixelsdone;

   gdsi_write_c( Setout,
                 &cwloOUT[paramnr], &cwhiOUT[paramnr],
                 data,
                 &ndat, &pixelsdone,
                 &tidOUT[paramnr] );
   minmax3_c( data,
              &ndat,
              &datamin[paramnr], &datamax[paramnr],
              &nblanks[paramnr],
              &mcount[paramnr] );
}



int comp1( sortnum *s1, sortnum *s2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for 'qsort' only!                */
/* Sort in decreasing order.                                  */
/*------------------------------------------------------------*/
{
   if (s1->number == blank)       /* Sort blanks to end of array */
      return( 1 );
   if (s2->number == blank)
      return( -1 );
   if (s1->number == s2->number)
      return( 0 );
   if (s1->number > s2->number)
      return( -1 );
   return( 1 );
}



int comp2( sortnum *s1, sortnum *s2 )
/*------------------------------------------------------------*/
/* PURPOSE: Compare function for 'qsort' only!                */
/* Sort in increasing order of distance to global variable    */
/* 'centval'.                                                 */
/*------------------------------------------------------------*/
{
   if (s1->number == blank)       /* Sort blanks to end of array */
      return( 1 );
   if (s2->number == blank)
      return( -1 );
   if (s1->number == s2->number)
      return( 0 );
   if (fabs(s1->number-centval) > fabs(s2->number-centval))
      return( 1 );
   return( -1 );
}



static void getsortedindex( int      *index,
                            float    *A,
                            int      fienr,
                            int      ndat,
                            sortnum  *sorts,
                            float    centval )
/*------------------------------------------------------------*/
/* PURPOSE: Given an array 'A' of floating point numbers,     */
/*          return an array of same length with integer       */
/*          indices corresponding to a specially sorted 'A'   */
/*          while leaving 'A' intact.                         */
/* INPUT:   Array A, length of that array 'ndat' and a func-  */
/*          tion number 'fienr' to indicate which compare     */
/*          function must be used in 'qsort'.                 */
/* OUTPUT   Index array 'index' corresponding to a sorted 'A' */
/*          but A itself will not be sorted on output.        */
/*------------------------------------------------------------*/
{
   int i;

   if (ndat == 2)                  /* Special situation: only two elements */
   {
      index[0] = 0;                /* Initialize */
      index[1] = 1;
      if (A[0] == blank)           /* First element blank, swap values always */
      {
         index[0] = 1;
         index[1] = 0;
      }
      else if (A[1] != blank)      /* Both elements are not blank */
      {
         if (fienr == 1)
         {
            if (A[0] < A[1])       /* Swap indices */
            {
               index[0] = 1;
               index[1] = 0;
            }
         }
         else if (fienr == 2)      /* Sort in decreasing order */
         {
            if (fabs(A[0]-centval) > fabs(A[1]-centval))
            {
               index[0] = 1;
               index[1] = 0;
            }
         }
      }
   }
   else                            /* More than 2 elements to sort */
   {
      for (i = 0; i < ndat; i++)
      {
         sorts[i].indx = i;
         sorts[i].number = A[i];
      }

      if (fienr == 1)
         qsort( sorts, ndat, sizeof(sortnum), (int(*)())comp1 );
      else if (fienr == 2)
         qsort( sorts, ndat, sizeof(sortnum), (int(*)())comp2 );

      for (i = 0; i < ndat; i++)
         index[i] = sorts[i].indx;
   }
}



static void showbar( float current,
                     float total,
                     int   *optionflag,
                     int   est_failed,
                     int   fit_failed,
                     int   its_exceed )
/*------------------------------------------------------------*/
/* PURPOSE: Show bar with progress info.                      */
/*------------------------------------------------------------*/
{
   if (optionflag[1] || optionflag[2])
      (void) sprintf( messbuf,
                     "Processed:%d%% est_fail:%d fit_fail:%d its_exceed:%d",
                      (int) (current*100.0/total),
                      est_failed,
                      fit_failed,
                      its_exceed );
   else
      (void) sprintf( messbuf,
                     "Processed:%d%%  est_failed:%d  its_exceed:%d",
                      (int) (current*100.0/total),
                      est_failed,
                      its_exceed );
   status_c( tofchar(messbuf) );
}



static void axinfo( int typenum )
/*------------------------------------------------------------*/
/* PURPOSE: Display type of axis.                             */
/*------------------------------------------------------------*/
{
   switch ( typenum )
   {
      case 0:
         anyoutC( 1, "Profile axis type: unknown type" );
         break;
      case 1:
         anyoutC( 1, "Profile axis type: spatial axis longitude" );
         break;
      case 2:
         anyoutC( 1, "Profile axis type: spatial axis latitude" );
         break;
      case 3:
         anyoutC( 1, "Profile axis type: spectral axis frequency" );
         break;
      case 4:
         anyoutC( 1, "Profile axis type: spectral axis velocity" );
         break;
      case 5:
         anyoutC( 1, "Profile axis type: spectral axis wavelength" );
         break;
      case 6:
         anyoutC( 1, "Profile axis type: spectral axis inverse wavelength" );
         break;
      case 7:
         anyoutC( 1, "Profile axis type: spectral axis log(wavelength)" );
         break;
      case 8:
         anyoutC( 1, "Profile axis type: time axis" );
         break;
      case 9:
         anyoutC( 1, "Profile axis type: polarisation axis" );
         break;
      case 10:
         anyoutC( 1, "Profile axis type: parameter axis" );
         break;
      case 11:
         anyoutC( 1, "Profile axis type: sample axis of iras data" );
         break;
      case 12:
         anyoutC( 1, "Profile axis type: tick axis of iras data" );
         break;
      case 13:
         anyoutC( 1, "Profile axis type: detector axis of iras data" );
         break;
      case 14:
         anyoutC( 1, "Profile axis type: snip axis of iras data" );
         break;
   }
}



MAIN_PROGRAM_ENTRY
/*------------------------------------------------------------*/
/* Because Fortran passes all arguments by reference all C    */
/* functions with a Fortran equivalent must do this also      */
/* (GIPSY programmers guide, Chapter 9)                       */
/*------------------------------------------------------------*/
{
   double   cputime, realtime;      /* Variables for timer */
   fint     elapse;
   fint     dev;                    /* Destination of output in anyout */
   fint     class;                  /* Axis operation mode */
   fint     dfault;                 /* Default option for input etc */
   fint     nitems;                 /* Max. number of input items in userxxx routines */
   fint     r1, r2;                 /* Results of userxxx routines */
   fint     totprofiles;            /* Total number of pixels in input */
   fint     pixelsdone;             /* Number of pixels after 1 _read */
   fint     nprofiles;              /* Number of profiles to process */
   fint     outaxislen;             /* Number of subsets in output */
   fint     maxtoread;              /* Max num pixels in 1 read action */
   int      gnr;                    /* Function nr in multi component gaussian */
   int      subnr;                  /* Index of current subset */
   int      i, j, k, m;             /* Counters */
   int      oldprofnum;             /* Is current profile a new one? */
   int      paramnr;                /* Index of current output subset */
   int      offset;                 /* Offset in input buffer */
   int      agreed;
   float    gridspac = 0.0;         /* Current average gridspacing */
   float    avgridspac = 0.0;       /* Global average gridspacing */
   sortnum  *worksort = NULL;       /* Work array for sorting purposes */
   int      estimate_failed = 0;    /* Profile administration */
   int      iters_exceed = 0;
   int      fit_failed = 0;
   int      profiles_with_blanks = 0;
   float    STBend;
   float    STBcurrent;
   bool     spatialaxis;


   init_c();                               /* contact Hermes */

   /* Task identification */
   {
      static fchar    ftask;               /* Name of current task */
      fmake( ftask, 20 );                  /* Macro 'fmake' must be available */
      myname_c( ftask );                   /* Get task name */
      ftask.a[nelc_c(ftask)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( ftask.a, VERSION );  /* Show task and version */
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
         r = 0;
         deputy_c( tofchar("XGAUFIT") , &r );   /* Start new task */
         finis_c();                           /* Quit Hermes */          
      }
   }   


   setfblank_c( &blank );                  /* Get system blank */

   fltmax  = FLT_MAX;                      /* Defines are functions, so initialize */
   flteps  = FLT_EPSILON;
   dfltmax = (double) FLT_MAX;             /* Cast function result to double */
   maxarg  = -log( (double) FLT_EPSILON ); /* Limit range in exp function */

   /*--------------------------------------------------*/
   /* Prepare an input set. This is a class 2 program, */
   /* so you have to give the number of axes outside   */
   /* the subset.                                      */
   /*--------------------------------------------------*/
   {
      fint     axesoutsidesub;

      fmake( Setin, BIGSTORE );
      dfault  = NONE;
      axesoutsidesub = 1;
      scrnum  = 3;
      class   = 2;
      nsubsI  = gdsinp_c( Setin,
                          subin,
                          &maxsubs,
                          &dfault,
                          KEY_INSET,
                          MES_INSET,
                          &scrnum,
                          axnum,
                          axcount,
                          &maxaxes,
                          &class,
                          &axesoutsidesub );         /* Class 2! */
      setdim  = gdsc_ndims_c( Setin, &setlevel );
      subdim  = setdim - axesoutsidesub;
   }

   /*--------------------------------------------------*/
   /* Examine operation axis. Is it spatial?           */
   /*--------------------------------------------------*/
   {
      fint   skysys, prosys, velsys;
      fchar  Ctype, Cunit, Dunit;
      fmake( Ctype, FITSLEN );
      fmake( Cunit, FITSLEN );
      fmake( Dunit, FITSLEN );
      (void) sprintf( messbuf, "CTYPE%d", axnum[setdim-1] );
      r2 = 0;
      gdsd_rchar_c( Setin, tofchar(messbuf), &setlevel, Ctype, &r2 );
      r1 = axtype_c( Ctype, Cunit, Dunit, &skysys, &prosys, &velsys );
      if (r1 == 1 || r1 == 2)
         spatialaxis = true;
      else
         spatialaxis = false;

      axinfo( (int) r1 );
   }


   /*--------------------------------------------------*/
   /* Determine the edges of this its frame (gridLO/HI)*/
   /*--------------------------------------------------*/
   r1 = 0;
   gdsc_range_c( Setin, &setlevel, &cwlo[0], &cwhi[0], &r1 );
   for (m = 0; m < (int) setdim; m++)
   {
      r1 = r2 = 0;
      gridLO[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo[0], &r1 );
      gridHI[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi[0], &r2 );
   }

   /*--------------------------------------------------*/
   /* Prepare a box for INSET. Default is a box equal  */
   /* to the frame.                                    */
   /*--------------------------------------------------*/
   {
      fint     boxopt = 0;                  /* Entire subset is default */
      dfault  = REQUEST;
      scrnum  = 3;
      gdsbox_c( BgridLO, BgridHI,
                Setin, subin,
                &dfault,
                KEY_BOX, MES_BOX,
                &scrnum,
                &boxopt );
   }

   /*--------------------------------------------------*/
   /* Count number of pixels in ONE subset.            */
   /*--------------------------------------------------*/
   totprofiles = 1;
   for(m = 0; m < (int) subdim; m++)
      totprofiles *= (BgridHI[m] - BgridLO[m] + 1);

   /*--------------------------------------------------*/
   /* GDSASN is needed before the call to GDSOUT to    */
   /* obtain an output set. GDSASN copies the axis     */
   /* names from the set previously opened by GDSINP   */
   /* in the order as determined by the user at the    */
   /* associated GDSINP request. This buffer will be   */
   /* read by GDSOUT, which will create an output set  */
   /* according to the specifications in this buffer.  */
   /*--------------------------------------------------*/

   gdsasn_c( KEY_INSET, KEY_OUTSET, &class );

   /*--------------------------------------------------*/
   /* We want to know how many output subsets (=length */
   /* of param axis) we have to create. For options 0  */
   /* 1 & 2 more than one gaussian can be found, each  */
   /* gaussian has three parameters, so per gaussian   */
   /* we need three subsets.                           */
   /*--------------------------------------------------*/

   nitems = MAXOPTIONS;
   dfault = REQUEST;
   do
   {
      r1 = userint_c( options,
                      &nitems,
                      &dfault,
                      KEY_OPTION,
                      MES_OPTION );
      agreed = false;
      if (r1 == 0)
      /*--------------------------------------------------*/
      /* Generate menu and cancel keyword if user pressed */
      /* <enter>.                                         */
      /*--------------------------------------------------*/
      {
         optionmenu();
         cancel_c( KEY_OPTION );
      }
      else
      {
         int   optcount;
         /*--------------------------------------------------*/
         /* Set a flag for the used options.                 */
         /*--------------------------------------------------*/
         for (optcount = 0; optcount < MAXOPTIONS; optcount++)
         {
            optionflag[optcount] = 0;
            for (j = 0; j < (int) r1; j++)
            {
               if (optcount == options[j])
               {
                  optionflag[optcount] = 1;
                  agreed = true;
               }
            }
         }
         if (!agreed)
            reject_c( KEY_OPTION, tofchar("Nothing selected!") );
      }
   }
   while (!agreed);

   /*--------------------------------------------------*/
   /* Calculate the number of output subsets that has  */
   /* to be created. But first ask the number of       */
   /* gaussians one allows in the fit.                 */
   /*--------------------------------------------------*/
   nitems       = 1;
   dfault       = REQUEST;
   maxgaussians = 1;
   (void) sprintf( messbuf,
                  "Give max. number of gaussians (<%d) in profile:  [%d]",
                   MAXGAUSS,
                   maxgaussians );
   do
   {
      r1 = userint_c( &maxgaussians,
                      &nitems,
                      &dfault,
                      KEY_NGAUSS, tofchar(messbuf) );
      agreed = ( maxgaussians > 0 && maxgaussians < MAXGAUSS );
      if (!agreed)
         reject_c( KEY_NGAUSS, tofchar("Not allowed!") );
   }
   while (!agreed);


   /*--------------------------------------------------*/
   /* GDSCPA changes the primary axis of an output set */
   /* to be obtained by GDSOUT. In this case add       */
   /* 'PARAM' axis to GDSOUT buffer.                   */
   /*--------------------------------------------------*/
   {
      fint    pmask;
      fint    axisnumber;
      double  outcdelt = 1.0;
      double  outcrota = 0.0;
      double  outcrpix = 0.0;
      double  outcrval = 0.0;
      int     outsubs;

      outsubs = 0;
      if (optionflag[0])
         outsubs += 3 * maxgaussians;
      if (optionflag[1])
         outsubs += 3 * maxgaussians;
      if (optionflag[2])
         outsubs += 3 * maxgaussians;


      outaxislen = outsubs;      /* Length of param axis */
      pmask = 14;                /* pmask=14=(2 + 4 + 8) means CTYPE,CRPIX & CRVAL are defined.*/
      axisnumber = subdim + 1;

      gdscpa_c( KEY_OUTSET,              /* Keyword associated with a GDSOUT call.*/
                &axisnumber,             /* The axis number of the axis to be changed.*/
                &outaxislen,             /* Size of the axis.*/
                &outcdelt,               /* Increment in physical units along axis.*/
                &outcrota,               /* Rotation angle of axis.*/
                &outcrpix,               /* Reference pixel of axis.*/
                &outcrval,               /* Physical reference value at reference pixel.*/
                tofchar("PARAM-GAUFIT"), /* New axis name.*/
                tofchar(" "),            /* Physical units of axis.*/
                &pmask );                /* Bit mask denoting which of the six above values are defined.*/
   }

   /*--------------------------------------------------*/
   /* Get output set. Number of subsets is just        */
   /* calculated 'outaxislen'.                         */
   /*--------------------------------------------------*/
   fmake( Setout, BIGSTORE );
   dfault = NONE;
   nsubsO = gdsout_c( Setout,
                      subout,
                      &outaxislen,
                      &dfault,
                      KEY_OUTSET,
                      MES_OUTSET,
                      &scrnum,
                      axnumO,
                      axcountO,
                      &maxaxes );


   /*--------------------------------------------------*/
   /* Prepare for an input set with estimates. This is */
   /* a class 1 input.                                 */
   /*--------------------------------------------------*/
   fmake( Estimateset, BIGSTORE );
   if (optionflag[0])
      dfault = HIDDEN;
   else
      dfault = REQUEST;
   subdimE  = 0;
   scrnum   = 3;
   class    = 1;
   maxsubs  = MAXEST;
   do
   {
      estimateset = false;
      nsubsE  = gdsinp_c( Estimateset,
                          subest,
                          &maxsubs,
                          &dfault,
                          KEY_ESTSET,
                          MES_ESTSET,
                          &scrnum,
                          axnumE,
                          axcountE,
                          &maxaxes,
                          &class,
                          &subdimE );
      if (nsubsE == 0)
         agreed = true;
      else
      {
         if (nsubsE != 3 * maxgaussians)
         {
            (void) sprintf( messbuf, "# subsets must be %d", 3 * maxgaussians);
            agreed = false;
         }
         else
         {
            agreed = true;
            estimateset = true;
         }
      }
      if (!agreed)
      {
         reject_c( KEY_ESTSET, tofchar(messbuf) );
         dfault = REQUEST;
      }
   } while(!agreed);


   /*--------------------------------------------------*/
   /* Get units of operation axis and data units. The  */
   /* axis units are determined with the 'cotrans'     */
   /* routine (could be secundary axis units) and the  */
   /* data units are read from the header.             */
   /*--------------------------------------------------*/
   {
      fint result;
      fint n = setdim - 1;

      fmake( Axunits, BIGSTORE );
      result = axunit_c( Setin, &axnum[n], Axunits );
      if (result == 1)
         errorC( 4, "A cotrans error occured!" );
      if (result == 2)
         errorC( 4, "Axis not present in set!" );
      if (result == 3)
         errorC( 4, "Output character string not large enough!" );
      fmake( Dataunits, BIGSTORE );
      r1 = 0;
      gdsd_rchar_c( Setin,
                    tofchar("BUNIT"),
                    &setlevel,
                    Dataunits,
                    &r1 );
      if (r1 < 0)
         Dataunits.a[0] = '?';
   }

   /* Somewhere in the code 'ncorr' is used regardless whether it is */
   /* specified by the user or not. Therefore set to an initial value. */
   ncorr = 1.0;               
   if(!estimateset)
   {
      /*--------------------------------------------------*/
      /* ('gauest') The r.m.s. noise level of the profile.*/
      /*--------------------------------------------------*/
      dfault = NONE;
      nitems = 1;
      (void) sprintf( messbuf,
                     "Give rms noise level for est. in %.*s:",
                      nelc_c(Dataunits),
                      Dataunits.a,
                      estrms );
      r1 = userreal_c( &estrms,
                       &nitems,
                       &dfault,
                       KEY_ESTRMS,
                       tofchar(messbuf) );


      /*--------------------------------------------------*/
      /* Get number of correlated subsets, e.g. if subsets*/
      /* were Hanning smoothed.                           */
      /*--------------------------------------------------*/
      dfault = REQUEST;
      nitems = 1;
      do
      {
         ncorr  = 1.0;
         r1 = userdble_c( &ncorr,
                          &nitems,
                          &dfault,
                          KEY_NCORR,
                          MES_NCORR );
         agreed = (ncorr > 0.0);
         if (!agreed)
            reject_c( KEY_NCORR,tofchar("Must be > 0") );
      }
      while (!agreed);
      estrms *= (float) sqrt( ncorr );


      /*--------------------------------------------------*/
      /* ('gauest')  Critical amplitude of estimate.      */
      /* ESTIMATED Gaussians below this amplitude will be */
      /* discarded.                                       */
      /*--------------------------------------------------*/
      dfault      = HIDDEN;
      nitems      = 1;
      estcritampl = 0.0;
      (void) sprintf( messbuf,
                     "Lower cutoff for amplitude of est. in %.*s:     [%g]",
                      nelc_c(Dataunits),
                      Dataunits.a,
                      estcritampl );
      r1 = userreal_c( &estcritampl,
                       &nitems,
                       &dfault,
                       KEY_ESTCUTAMP,
                       tofchar(messbuf) );

      /*--------------------------------------------------*/
      /* ('gauest')  Critical dispersion of estimate.     */
      /* Gaussians with dispersion below this will be     */
      /* discarded. Use axis units for message.           */
      /*--------------------------------------------------*/
      dfault      = HIDDEN;
      nitems      = 1;
      estcritdisp = 0.0;
      (void) sprintf( messbuf,
                     "Lower cutoff for dispersion of est. in %.*s:   [minimum]",
                      nelc_c(Axunits),
                      Axunits.a,
                      estcritdisp );
      r1 = userreal_c( &estcritdisp,
                       &nitems,
                       &dfault,
                       KEY_ESTCUTDISP,
                       tofchar(messbuf) );

      /*--------------------------------------------------*/
      /* Later, this dispersion value will be converted   */
      /* to grids when we have the physical coordinates of*/
      /* the operation axis and the average gridspacing.  */
      /*--------------------------------------------------*/


      /*--------------------------------------------------*/
      /* ('gauest')  Smoothing parameter for calculating  */
      /* initial estimates Q= determines the number of    */
      /* points (=2*Q+1) used in calculating the second   */
      /* derivative of the profile.                       */
      /*--------------------------------------------------*/

      dfault = REQUEST;
      nitems = MAXQPARS;
      do
      {
         int    i;
         smoothpar[0] = 2;
         maxQpars = userint_c( smoothpar,
                               &nitems,
                               &dfault,
                               KEY_Q,
                               MES_Q );
         if (maxQpars == 0)
            maxQpars = 1;
         for (i = 0; i < maxQpars; i++)
         {
            agreed = ( smoothpar[i] >= 1 && (2*smoothpar[i]+1) <= nsubsI );
            if (!agreed)
            {
               (void) sprintf( messbuf, "Q=%d Not allowed!", smoothpar[i] );
               reject_c( KEY_Q, tofchar(messbuf) );
               break;
            }
         }
      } while (!agreed);
   }

   if (optionflag[1] || optionflag[2])
   {
      /*--------------------------------------------------*/
      /* ('lsqfit')  Critical amplitude of gaussian.      */
      /* Gaussians below this amplitude will be discarded.*/
      /*--------------------------------------------------*/
      dfault   = REQUEST;
      nitems   = 1;
      critampl = 0.0;
      (void) sprintf( messbuf,
                     "Lower cutoff for fitted amplitude in %.*s:     [%g]",
                      nelc_c(Dataunits),
                      Dataunits.a,
                      critampl );
      r1 = userreal_c( &critampl,
                       &nitems,
                       &dfault,
                       KEY_CUTAMP,
                       tofchar(messbuf) );

      /*--------------------------------------------------*/
      /* ('lsqfit')  Critical dispersion of gaussian.     */
      /* Gaussians with dispersion below this will be     */
      /* discarded. Use axis units for message.           */
      /*--------------------------------------------------*/
      dfault   = REQUEST;
      nitems   = 1;
      critdisp = 0.0;
      (void) sprintf( messbuf,
                     "Lower cutoff for fitted dispersion in %.*s:     [%g]",
                      nelc_c(Axunits),
                      Axunits.a,
                      critdisp );
      r1 = userreal_c( &critdisp,
                       &nitems,
                       &dfault,
                       KEY_CUTDISP,
                       tofchar(messbuf) );

      /*--------------------------------------------------*/
      /* ('lsqfit') Fitting of the profile stops when     */
      /* successive iterations fail to produce a decrement*/
      /* in reduced chi-squared less than TOLERANCE. If   */
      /* its value is less than the minimum tolerance     */
      /* possible, it will be set to this value. This     */
      /* means that maximum accuracy can be obtained by   */
      /* setting TOLERANCE=0.0.                           */
      /*--------------------------------------------------*/
      dfault = REQUEST;
      nitems = 1;
      do
      {
         tol = 0.0;
         r1  = userreal_c( &tol,
                           &nitems,
                           &dfault,
                           KEY_TOLERANCE,
                           MES_TOLERANCE );
         if( tol < 0.0 )
            reject_c( KEY_TOLERANCE,tofchar("Must be > 0") );
      }
      while( tol < 0.0 );

      /*--------------------------------------------------*/
      /* Maximum number of iterations in 'lsqfit'.        */
      /*--------------------------------------------------*/
      dfault  = HIDDEN;
      nitems  = 1;
      do
      {
         its = 50;
         r1  = userint_c( &its,
                          &nitems,
                          &dfault,
                          KEY_MAXITS,
                          MES_MAXITS );
         if( its < 1)
            reject_c( KEY_MAXITS, tofchar("Must be > 1") );
      }
      while( its < 1 );

      /*--------------------------------------------------*/
      /* Mixing parameter in 'lsqfit' , 'lab' determines  */
      /* the initial weight of steepest descent method    */
      /* relative to the Taylor method. 'lab' should be   */
      /* a small value (i.e. 0.01). 'lab' can only be     */
      /* zero when the partial derivatives are independent*/
      /* of the parameters. In fact in this case 'lab'    */
      /* should be exactly equal to zero.                 */
      /*--------------------------------------------------*/
      lab     = LAMBDA;
      dfault  = HIDDEN;
      nitems  = 1;
      (void) sprintf( messbuf, "Value for mixing parameter:   [%f]", lab );
      r1      = userreal_c( &lab,
                            &nitems,
                            &dfault,
                            KEY_LAB,
                            tofchar( messbuf ) );
      lab = fabs( lab );
   }


   /*--------------------------------------------------*/
   /* Other 'lsqfit' and 'gauest' parameters:          */
   /*--------------------------------------------------*/
   {
      int n;
      for (n = 0; n < (int) nsubsI; n++)
      {
         wdat[n] = 1.0;                    /* No blanks ==> Weights=1 */
      }
      xdim = 1;                            /* Dimension of fit */
      for (n = 0; n < MAXPAR; n++)
         mpar[n] = 1.0;                    /* All parameters are free */
   }

   /*--------------------------------------------------*/
   /* Get physical values for operation axis.          */
   /*--------------------------------------------------*/
   for(subnr = 0; subnr < (int) nsubsI; subnr++)
   {
      /* For each grid on the operation axis, get physical value */
      physcoord[subnr] = (float) getphyscoord( Setin,
                                               subin[subnr],
                                               axnum,
                                               &gridcoord[subnr] );

      /* anyoutf( 1, "phys, grid coord = %f %f",
                 physcoord[subnr], gridcoord[subnr] ); */
   }
   /* Initialize average grid spacing for non spatial axes */
   avgridspac = fabs(physcoord[0] - physcoord[nsubsI-1]) / (float)(nsubsI-1.0);

   /*--------------------------------------------------*/
   /* Check whether subsets are a contiguous block.    */
   /*--------------------------------------------------*/
   if (nsubsI != ABS((int)gridcoord[nsubsI-1] - (int)gridcoord[0]) + 1)
      errorC( 4, "Subsets are not a contiguous block!" );


   /*--------------------------------------------------*/
   /* If components have to be sorted, ask user which  */
   /* component has to be sorted. Each sort can have   */
   /* a different compare function used in the qsort   */
   /* routine.                                         */
   /*--------------------------------------------------*/
   sort = 0;

   if (maxgaussians > 1)
   {
      dfault = REQUEST;
      nitems = 1;
      do
      {
         sort = 0;
         strcpy( messbuf, "Give sort opt. 0=None 1=ampl 2=centval 3=disp:   [0]");
         r1 = userint_c( &sort,
                         &nitems,
                         &dfault,
                         KEY_SORT,
                         tofchar(messbuf) );
         agreed = ( sort >= 0 && sort <= 3 );
         if (!agreed)
            reject_c( KEY_SORT, tofchar("Not allowed!") );
      }
      while (!agreed);

      if (sort == 2)
      {
         dfault  = REQUEST;
         nitems  = 1;
         centval = (float) physcoord[ (int) nsubsI/2 ];
         (void) sprintf( messbuf,
                        "Give centre for sorting in %.*s:   [%g]",
                         nelc_c(Axunits),
                         Axunits.a,
                         centval);
         r1 = userreal_c( &centval,
                          &nitems,
                          &dfault,
                          tofchar("CENTVAL="),
                          tofchar(messbuf) );
      }
   }

   if (sort)
   /*--------------------------------------------------*/
   /* Now the maximum number of gaussians is known,    */
   /* create a work array for sorting purposes.        */
   /*--------------------------------------------------*/
   {
      worksort = (sortnum *) malloc( maxgaussians * sizeof(sortnum) );
      if (!worksort)
         errorC( 4, "Memory allocation problem creating work array for sorting!" );
   }

   /*--------------------------------------------------*/
   /* Initialize coordinate words etc.                 */
   /*--------------------------------------------------*/
   for(subnr = 0; subnr < (int) nsubsI; subnr++)
   {
      /* Create coordinate words for the input subsets */
      cwlo[subnr] = gdsc_fill_c( Setin, &subin[subnr], BgridLO );
      cwhi[subnr] = gdsc_fill_c( Setin, &subin[subnr], BgridHI );
      /* Reset tranfer id's */
      tidIN[subnr] = 0;
   }

   /*--------------------------------------------------*/
   /* Create coordinate words for the output subsets.  */
   /*--------------------------------------------------*/
   for(paramnr = 0; paramnr < (int) nsubsO; paramnr++)
   {
      cwloOUT[paramnr] = gdsc_fill_c( Setout, &subout[paramnr], BgridLO );
      cwhiOUT[paramnr] = gdsc_fill_c( Setout, &subout[paramnr], BgridHI );
      tidOUT[paramnr] = 0;
      mcount[paramnr] = 0;
   }

   /*--------------------------------------------------*/
   /* Create coordinate words for the estimate subsets.*/
   /*--------------------------------------------------*/
   if (estimateset)
   {
      for(paramnr = 0; paramnr < (3 * maxgaussians); paramnr++)
      {
         cwloEST[paramnr] = gdsc_fill_c( Estimateset, &subest[paramnr], BgridLO );
         cwhiEST[paramnr] = gdsc_fill_c( Estimateset, &subest[paramnr], BgridHI );
         tidEST[paramnr] = 0;
      }
   }

   /* Initialize stabar */

   STBend     = (float) (totprofiles);
   STBcurrent = 0.0;
   showbar( STBcurrent, STBend, optionflag,
            estimate_failed, fit_failed, iters_exceed );

   /*--------------------------------------------------*/
   /* The input buffer stores data of a certain area   */
   /* sequentially of all subsets. This data is read   */
   /* in a loop (over the number of input subsets).    */
   /* If the total size of the buffer is 'MAXBUF',     */
   /* than in each read action a maximum of            */
   /* MAXBUF/(nsubs) pixels can be stored.             */
   /*--------------------------------------------------*/

   maxtoread = (fint) (MAXBUF / nsubsI);

   /*--------------------------------------------------*/
   /* Preset array which keeps track of fits made by   */
   /* certain Q= value                                 */
   /*--------------------------------------------------*/
   for (i = 0; i < MAXQPARS; i++)
      nQfit[i] = 0;

   for (i = 0; i < subdim; i++)                /* Initialize position vector */
      gridvect[i] = BgridLO[i];
   gridvect[0]--;                              /* Increased at start of loop */

   elapse = 0;
   timer_c( &cputime, &realtime, &elapse );    /* Set timer */
   do
   {
      for (subnr = 0; subnr < (int) nsubsI; subnr++)
      /*--------------------------------------------------*/
      /* Read same area for all subsets.                  */
      /*--------------------------------------------------*/
      {
         offset = subnr * (int) maxtoread;
      	 gdsi_read_c( Setin,
      	              &cwlo[subnr],    /* Data from box! */
      	              &cwhi[subnr],
      	              &imageIN[offset],
                      &maxtoread,
                      &pixelsdone,
                      &tidIN[subnr] );
      }

      if (estimateset)
      /*--------------------------------------------------*/
      /* Read same area for all subsets in estimate set.  */
      /* The number of subsets here is 'maxgaussians'.    */
      /*--------------------------------------------------*/
      {
         for (gnr = 0; gnr < maxgaussians; gnr++)
         {
            fint   dummy;
            fint   offs = gnr*3;
            gdsi_read_c( Estimateset, &cwloEST[0+offs], &cwhiEST[0+offs],
                         &gauest_ampl[gnr*pixelsdone], &pixelsdone,
                         &dummy, &tidEST[0+offs] );
            gdsi_read_c( Estimateset, &cwloEST[1+offs], &cwhiEST[1+offs],
                         &gauest_cent[gnr*pixelsdone], &pixelsdone,
                         &dummy, &tidEST[1+offs] );
            gdsi_read_c( Estimateset, &cwloEST[2+offs], &cwhiEST[2+offs],
                         &gauest_disp[gnr*pixelsdone], &pixelsdone,
                         &dummy, &tidEST[2+offs] );
         }
      }

      /*--------------------------------------------------*/
      /* Now we have 'pixelsdone' small sub buffers of    */
      /* length 'nsubsI'. The data is collected for each  */
      /* profile in the array 'amplitudes' where the      */
      /* calculations are done.                           */
      /*--------------------------------------------------*/
      nprofiles = pixelsdone;
      oldprofnum = -1;
      for (i = 0; i < nprofiles; i++)
      {
         fint   num_blanks = 0;

         if (i != oldprofnum)
         {
            int   m;
            float physlo, physhi;

            qcount = 0;
            oldprofnum = i;
            for(subnr = 0; subnr < nsubsI; subnr++)  /* Collect data in new array */
            {
               offset = subnr * maxtoread;
               amplitudes[subnr] = imageIN[offset+i];
               if (amplitudes[subnr] == blank)
                  num_blanks++;
            }
            gridvect[0]++;                   /* Calculate the position vector */
            for (m = 1; m < subdim; m++)
            {
               if (gridvect[m-1] > BgridHI[m-1])
               {
                  gridvect[m-1] = BgridLO[m-1];
                  gridvect[m]++;
               }
            }
            /*--------------------------------------------------*/
            /* The critical value for the dispersion in 'gauest'*/
            /* is a value in pixels. But the user entered the   */
            /* value in physical coordinates. To convert the    */
            /* dispersion to pixels, we need to know an average */
            /* grid spacing. The variable that changes is the   */
            /* grid vector 'gridvect'.                          */
            /*--------------------------------------------------*/
            if (spatialaxis)
            {
               physlo = tophys( gridcoord[0], gridvect, setdim, axnum );
               physhi = tophys( gridcoord[nsubsI-1], gridvect, setdim, axnum );
               gridspac = fabs( physhi - physlo ) / (float) ( nsubsI - 1.0 );
               if (gridspac == 0.0)
                  errorC( 4, "Found an average grid spacing == 0.0!" );
            }
            else
               gridspac = avgridspac;

            ecdisppix = estcritdisp / gridspac;
            if (ecdisppix < 1.0)
               ecdisppix = 1.0;
         }

         /*--------------------------------------------------*/
         /* Now we filled one profile in 'amplitudes' array. */
         /* Next, get estimates (from set or calculation)    */
         /* do the fitting.                                  */
         /*--------------------------------------------------*/
         if (!estimateset)
         {
            /* Initialize estimates.*/
            for (gnr = 0; gnr < maxgaussians; gnr++)
            {
               gauest_ampl[i+gnr*nprofiles] = blank;
               gauest_cent[i+gnr*nprofiles] = blank;
               gauest_disp[i+gnr*nprofiles] = blank;
            }

            if (num_blanks == 0)
            /*--------------------------------------------------*/
            /* Calculate the estimates. A profile for 'gauest'  */
            /* cannot contain blanks or gaps. The estimates are */
            /* in PIXELS! If 'gauest fails and there are more   */
            /* values in Q=, repeat the action.                 */
            /*--------------------------------------------------*/
            {
               float A[MAXGAUSS], V[MAXGAUSS], D[MAXGAUSS];
               int   index[MAXGAUSS];

               maxparams = 3 * maxgaussians;
               do
               {
                  gaussians = gauest_c( amplitudes,
                                        workdummy,
                                        &nsubsI,
                                        estimates,
                                        &maxparams,
                                        &estrms,
                                        &estcritampl,
                                        &ecdisppix,
                                        &smoothpar[qcount] );
                  if (gaussians <= 0 )
                     qcount++;
                  if (qcount == maxQpars)
                     estimate_failed++;
               }
               while (gaussians <= 0 && qcount < maxQpars);

               if (gaussians > maxgaussians)
                  gaussians = maxgaussians;

               for (gnr = 0; gnr < gaussians; gnr++)
               /*--------------------------------------------------*/
               /* Transform to physical coordinates before         */
               /* writing to disk or starting fitting routine.     */
               /* The grid returned by the estimate routine is     */
               /* a number between 0 and the number of subsets.    */
               /* To transform this to a grid on the profile       */
               /* axis, one has to add the grid value of the       */
               /* first subset coordinate word as stored in        */
               /* 'gridcoord[0]'.                                  */
               /*--------------------------------------------------*/
               {
                  float   grid =  estimates[1+gnr*3] + gridcoord[0];
                  int     i0 = 0+gnr*3;
                  int     i1 = 1+gnr*3;
                  int     i2 = 2+gnr*3;
                  int     k  = i+gnr*nprofiles;
                  estimates[i1]  = tophys( grid, gridvect, setdim, axnum );
                  estimates[i2] *= gridspac;
                  if (estimates[i2] < 0.0)
                  {
                     estimates[i0] = blank;
                     estimates[i1] = blank;
                     estimates[i2] = blank;
                  }
                  A[gnr] = gauest_ampl[k] = estimates[i0];
                  V[gnr] = gauest_cent[k] = estimates[i1];
                  D[gnr] = gauest_disp[k] = estimates[i2];
               } /* End for loop over all gaussians found in 'gauest' */

               if (sort && optionflag[0] && gaussians > 1)
               /*--------------------------------------------------*/
               /* Sort the gauest_arrays if user wanted sorted     */
               /* estimates. Do not change order in estimates      */
               /* array.                                           */
               /*--------------------------------------------------*/
               {
                  if       (sort == 1)
                     getsortedindex( index, A, 1, gaussians, worksort, centval );
                  else  if (sort == 2)
                     getsortedindex( index, V, 2, gaussians, worksort, centval );
                  else  if (sort == 3)
                     getsortedindex( index, D, 1, gaussians, worksort, centval );
                  for (gnr = 0; gnr < gaussians; gnr++)
                  {
                     int j = index[gnr];
                     int k = i+gnr*nprofiles;
                     gauest_ampl[k] = A[j];
                     gauest_cent[k] = V[j];
                     gauest_disp[k] = D[j];
                  }
               }
            }
            else
               profiles_with_blanks++;
         }
         else
         {
            /*--------------------------------------------------*/
            /* The estimates came from a set.                   */
            /*--------------------------------------------------*/
            gaussians = 0;
            for (gnr = 0; gnr < maxgaussians; gnr++)
            {
               float val1, val2, val3;
               val1 = gauest_ampl[i+gnr*nprofiles];
               val2 = gauest_cent[i+gnr*nprofiles];
               val3 = gauest_disp[i+gnr*nprofiles];
               if (val1 != blank && val2 != blank && val3 != blank)
               {
                  int   gnr3 = gnr*3;
                  estimates[0+gnr3] = val1;
                  estimates[1+gnr3] = val2;
                  estimates[2+gnr3] = val3;
                  gaussians++;
               }
               else
                  break;
            }
         }

         if (optionflag[1] || optionflag[2])
         /*--------------------------------------------------*/
         /* Start preparation for the least squares routine. */
         /*--------------------------------------------------*/
         {
            for (gnr = 0; gnr < maxgaussians; gnr++)
            {
               int k = i+gnr*nprofiles;
               lsq_gauss_ampl[k] = blank;
               lsq_gauss_cent[k] = blank;
               lsq_gauss_disp[k] = blank;
               err_gauss_ampl[k] = blank;
               err_gauss_cent[k] = blank;
               err_gauss_disp[k] = blank;
            }
            if (num_blanks == 0)
            {
               if (spatialaxis)
               /*--------------------------------------------------*/
               /* The axis is spatial, so recalculate the physical */
               /* coordinates for the new grid vector.             */
               /*--------------------------------------------------*/
               {
                  int v;
                  for (v = 0; v < nsubsI; v++)
                     physcoord[v] = tophys( gridcoord[v],
                                            gridvect,
                                            setdim,
                                            axnum );
               }

               /* All calculations are done in physical coordinates! */
               npar = gaussians * 3;
               iters = lsqfit_c( physcoord,     /* X-coordinates */
                                 &xdim,         /* Dimension of fit */
                                 amplitudes,    /* Y data */
                                 wdat,          /* Weights */
                                 &nsubsI,       /* Number of data points */
                                 estimates,     /* Initial values/fitted pars.*/
                                 epar,          /* Error return values */
                                 mpar,          /* All parameters free */
                                 &npar,         /* Total number of parameters */
                                 &tol,          /* Tolerance */
                                 &its,          /* Max. num. of iterations */
                                 &lab,          /* Mixing parameter */
                                 &gaussians );  /* Number of gaussians */
               if (iters >= 0)
               /*--------------------------------------------------*/
               /* After a successful fit, sort and store the pars. */
               /*--------------------------------------------------*/
               {
                  int   v;
                  float corr = (float) sqrt( ncorr );

                  nQfit[qcount]++;      /* Another fit was made with this Q */
                  qcount = 0;

                  /*--------------------------------------------------*/
                  /* Incorporate correlation (if any) of subsets in   */
                  /* errors on fitted parameters.                     */
                  /*--------------------------------------------------*/
                  for (v = 0; v < npar; v++)
                     epar[v] *= corr;

                  /*--------------------------------------------------*/
                  /* Check local single precision floats whether they */
                  /* are legal floating point numbers. If not, they   */
                  /* are replaced by BLANKs.                          */
                  /*--------------------------------------------------*/
                  (void) clspfp_c( estimates, &npar );
                  (void) clspfp_c( epar, &npar );

                  /*--------------------------------------------------*/
                  /* Store the values returned in 'estimates', but if */
                  /* user wants, sort first!                          */
                  /*--------------------------------------------------*/
                  if (!sort)
                  {
                     for (gnr = 0; gnr < gaussians; gnr++)
                     {
                        int k = i+gnr*nprofiles;
                        int i0 = 0+gnr*3;
                        int i1 = 1+gnr*3;
                        int i2 = 2+gnr*3;
                        if (estimates[i0] >= critampl &&
                            estimates[i2] >= critdisp)
                        {
                           lsq_gauss_ampl[k] = estimates[i0];
                           err_gauss_ampl[k] = epar[i0];
                           lsq_gauss_cent[k] = estimates[i1];
                           err_gauss_cent[k] = epar[i1];
                           lsq_gauss_disp[k] = estimates[i2];
                           err_gauss_disp[k] = epar[i2];
                        }
                     }
                  }
                  else  /* Sort the arrays before storing them */
                  {
                     float A[MAXGAUSS], V[MAXGAUSS], D[MAXGAUSS];
                     float EA[MAXGAUSS], EV[MAXGAUSS], ED[MAXGAUSS];
                     int   index[MAXGAUSS];
                     for (gnr = 0; gnr < gaussians; gnr++)
                     {
                        int i0  = 0+gnr*3;
                        int i1  = 1+gnr*3;
                        int i2  = 2+gnr*3;
                        A[gnr]  = estimates[i0];
                        V[gnr]  = estimates[i1];
                        D[gnr]  = estimates[i2];
                        EA[gnr] = epar[i0];
                        EV[gnr] = epar[i1];
                        ED[gnr] = epar[i2];
                     }
                     if (sort == 1)
                        getsortedindex( index, A, 1, gaussians, worksort, centval );
                     else  if (sort == 2)
                        getsortedindex( index, V, 2, gaussians, worksort, centval );
                     else  if (sort == 3)
                        getsortedindex( index, D, 1, gaussians, worksort, centval );

                     for (gnr = 0; gnr < gaussians; gnr++)
                     {
                        int j = index[gnr];
                        int k = i+gnr*nprofiles;
                        if ( (A[j] >= critampl) &&
                           ((float) fabs(D[j]) >= critdisp) )
                        {
                           lsq_gauss_ampl[k] =  A[j];
                           err_gauss_ampl[k] = EA[j];
                           lsq_gauss_cent[k] =  V[j];
                           err_gauss_cent[k] = EV[j];
                           lsq_gauss_disp[k] =  D[j];
                           err_gauss_disp[k] = ED[j];
                        }
                     }
                  } /* End sort/not sort */
               }
               else
               {
                  if (iters == -4)
                     iters_exceed++;
                  if (iters <= -4 && !estimateset && qcount < (maxQpars-1) )
                  /*------------------------------------------------------*/
                  /* Try estimating and fitting again with new value of Q */
                  /* only if fit failed with error <= -4.                 */
                  /*------------------------------------------------------*/
                  {
                      qcount++;    /* For the next value in Q=  */
                      i--;         /* ... process same profile again! */
                  }
                  else
                  {
                     /* Fitting or estimate really failed, do not try again. */
                     fit_failed++;
                  }
               }            /* End if lsqfit handled more or less successful */
            }           /* End if there are no blanks in this profile */
         }          /* End if lsqfit or error option */
      }         /* End for loop over all profiles in current buffer. */


      /* Display bar showing progress */
      STBcurrent += (float) (pixelsdone);
      showbar( STBcurrent, STBend, optionflag,
               estimate_failed, fit_failed, iters_exceed );

      paramnr = 0;
      if (optionflag[0])
      {
         /* Write the estimated parameters */
         for (gnr = 0; gnr < maxgaussians; gnr++)
         {
            writeoutput( &gauest_ampl[gnr*nprofiles], pixelsdone, paramnr++ );
            writeoutput( &gauest_cent[gnr*nprofiles], pixelsdone, paramnr++ );
            writeoutput( &gauest_disp[gnr*nprofiles], pixelsdone, paramnr++ );
         }
      }

      if (optionflag[1])
      {
         /* Write the fitted parameters */
         for (gnr = 0; gnr < maxgaussians; gnr++)
         {
            writeoutput( &lsq_gauss_ampl[gnr*nprofiles], pixelsdone, paramnr++ );
            writeoutput( &lsq_gauss_cent[gnr*nprofiles], pixelsdone, paramnr++ );
            writeoutput( &lsq_gauss_disp[gnr*nprofiles], pixelsdone, paramnr++ );
         }
      }

      if (optionflag[2])
      {
         /* Write the errors in fitted parameters */
         for (gnr = 0; gnr < maxgaussians; gnr++)
         {
            writeoutput( &err_gauss_ampl[gnr*nprofiles], pixelsdone, paramnr++ );
            writeoutput( &err_gauss_cent[gnr*nprofiles], pixelsdone, paramnr++ );
            writeoutput( &err_gauss_disp[gnr*nprofiles], pixelsdone, paramnr++ );
         }
      }
   } while (tidIN[0] != 0);                    /* All pixels examined? */

   /* Inform user about status of results. */
   elapse = 1;
   timer_c( &cputime, &realtime, &elapse );    /* Get the elapsed time */


   /*--------------------------------------------------*/
   /* Give user data in log-file: Input set, output    */
   /* set(s), units, used options etc.                 */
   /*--------------------------------------------------*/
   dev = 3;
   anyoutC( dev, " ");
   anyoutC( dev, "=========================== RESULTS =========================" );

   anyoutf( dev, "  Profiles from set:  %.*s", nelc_c(Setin),  Setin.a );
   anyoutf( dev, "  Results in set:     %.*s", nelc_c(Setout), Setout.a );

   paramnr = 1;
   if (optionflag[0])
   {
      for (k = 0; k < maxgaussians; k++)
      {
         anyoutf( dev, "  param %d: Estimated amplitudes of component %d",
                  paramnr++, k+1 );
         anyoutf( dev, "  param %d: Estimated centre of component %d",
                  paramnr++, k+1 );
         anyoutf( dev, "  param %d: Estimated dispersion of component %d",
                  paramnr++, k+1 );
      }
   }
   if (optionflag[1])
   {
      for (k = 0; k < maxgaussians; k++)
      {
         anyoutf( dev, "  param %d: fitted amplitudes of component %d",
                  paramnr++, k+1 );
         anyoutf( dev, "  param %d: fitted centre of component %d",
                  paramnr++, k+1 );
         anyoutf( dev, "  param %d: fitted dispersion of component %d",
                  paramnr++, k+1 );
      }
   }
   if (optionflag[2])
   {
      for (k = 0; k < maxgaussians; k++)
      {
         anyoutf( dev, "  param %d: Error in fitted amplitudes of component %d",
                  paramnr++, k+1 );
         anyoutf( dev, "  param %d: Error in fitted centre of component %d",
                  paramnr++, k+1 );
         anyoutf( dev, "  param %d: Error in fitted dispersion of component %d",
                  paramnr++, k+1 );
      }
   }


   if (estimateset)
      anyoutf( dev, "  Estimates from set: %.*s",
               nelc_c(Estimateset),
               Estimateset.a );

   anyoutf( dev, "  Max. number of gaussians in profile is %d", maxgaussians );
   anyoutf( dev, "  Range operation axis: [%g,%g] %.*s",
            physcoord[0], physcoord[nsubsI-1],
            nelc_c(Axunits),
            Axunits.a );
   anyoutf( dev, "  Fitted gaussians will have amplitude > %f %.*s",
            critampl,
            nelc_c(Dataunits),
            Dataunits.a );
   anyoutf( dev, "  Fitted gaussians will have dispersion > %f %.*s",
            critdisp,
            nelc_c(Axunits),
            Axunits.a );
   anyoutf( dev, "  One pixel on operation axis has (average) width %f %.*s",
            avgridspac,
            nelc_c(Axunits),
            Axunits.a );
   anyoutf( dev, "  Processed %d profiles in %.2f sec (%.2f cpu sec)",
            totprofiles,
            realtime,
            cputime );

   if (!estimateset)
   {
      for (qcount = 0; qcount < maxQpars; qcount++)
         anyoutf( dev, "  Number of profiles fitted with Q=%2d:  %d",
                  smoothpar[qcount], nQfit[qcount] );
   }

   if (profiles_with_blanks > 0)
      anyoutf( dev, "  %d profile(s) contained blanks", profiles_with_blanks );


   if (estimate_failed > 0)
      anyoutf( dev, "  For %d profiles no estimate was found.", estimate_failed );

   if (fit_failed > 0)
      anyoutf( dev, "  For %d profiles the fitting failed.", fit_failed );

   if (iters_exceed > 0)
      anyoutf( dev, "  %d times the max. no. iterations was exceeded.", iters_exceed );

   /* Update units at set level */
   {
      int k;
      for (k = 0; k < (int) nsubsO / 3; k++)
      {
         r1 = 0;
         /* Amplitude units are units of the data */
         gdsd_wchar_c( Setout, tofchar("BUNIT"), &subout[0+k*3],
                       Dataunits, &r1 );
         r1 = 0;
         /* Centre units are units of the axis */
         gdsd_wchar_c( Setout, tofchar("BUNIT"), &subout[1+k*3],
                       Axunits, &r1 );
         r1 = 0;
         /* Dispersion units are units of the axis */
         gdsd_wchar_c( Setout, tofchar("BUNIT"), &subout[2+k*3],
                       Axunits, &r1 );
      }
   }

   {
      fint  rmove = true;                     /* Update header of new set */
      wminmax_c( Setout, subout,
                 datamin, datamax,
                 nblanks,
                 &nsubsO,
                 &rmove );
   }


   {
      char timebuf[21];

      sprintf( messbuf,
              "Header copied by program GAUFIT from set %.*s at %s",
               nelc_c(Setin),                /* Output setname */
               Setin.a,
               makedate(timebuf,20) );       /* Current date */
   }


   r1 = 0;                                   /* Put comment at set level */
   gdsd_wvar_c( Setout,
                tofchar("COMMENT"),
                &setlevel,
                tofchar(messbuf),
                &r1 );


   if (worksort != NULL)
      free( worksort );
   finis_c();                                  /* Quit Hermes */
   return( 0 );
}
