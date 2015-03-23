/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             moments.dc1

Program:       MOMENTS

Purpose:       Program to calculate moments and other properties
               of a profile

Category:      MANIPULATION, CALCULATION, PROFILES

File:          moments.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give set (, subsets):
               Maximum number of subsets is 2048.

   BOX=        Frame for input subsets.               [entire subset]

   OPTION=     Give option(s) 0..7                     [List options]

               0: Sum of profile amplitudes (zeroth moment,
                  without multiplication with the channel spacing).
               1: Intensity weighted mean of physical coordinates
                  along profile (first moment).
               2: Dispersion in intensity weighted physical
                  coordinates along profile (second moment).
               3: Physical coordinate of peak value in profile.
               4: Peak value in profile.
               5: Dispersion in intensities.
               6: Number of subset contributions.
               7: Intensity weighted Mode (only with WINDOW=0).

   OUTSET=     Output set (,subsets). Maximum number of subsets is
               equal to the maximum number of different options.

   AUTO=       Automatically set minimum per profile?           Y/[N]
               The physical coordinates in a profile are 
               weighted with the intensities of the pixels in that 
               profile. An intensity weighted mean can only be weighted 
               with positive pixel values. To avoid bias caused by noise
               contributed by pixels with a small positive value, 
               this option calculates a threshold per profile.
               This threshold is the absolute value of the pixel in the
               profile with the highest negative value and only pixel 
               values higher than the threshold are included in the
               moment analysis.

   RANGE=      Give range of levels to work on.            [0.0  inf]
               The keyword is prompted if AUTO=N
               Input consists of two values. The first value may
               be greater than the second. See the description
               for the correct use of this keyword.
               The threshold is set to zero because negative weights
               are not allowed by definition.               
               RANGE= accepts the strings INF and -INF as input.

   WINDOW=     Give window for data selection:    [0, i.e. no window]
               See description.

   WINMODE=    Stat. in most sign. peak(0), or all peaks(1):    [0]/1
               See description.


Description:   To calculate moments in profiles you first need to
               specify the set and the subsets. The number of subsets
               must be greater than 1 and less than 2048 and the sub-
               sets are specified by giving the name of the axis in
               the direction in which you want your profiles. Suppose
               your set AURORA has the axes:
               RA-NCP             from    -7 to     8
               DEC-NCP            from    -7 to     8
               FREQ-OHEL          from     1 to    59

               and you want to make an integrated line emission map;
                        SUM[I] . Channel interval    (in mapunits.channel)
                                                        (e.g. K km/s)

               a velocity map;
                        <V> = SUM[I.V]/SUM[I]  (in CUNITs of V axis)

               and a velocity dispersion map;
                        <sigma> = SQRT[ <V.V> - <V><V> ]

               of all channel maps, you specify:

                                INSET=AURORA FREQ

               Choose option 0, 1 and 2 from the OPTION menu and give
               a name for the output set. The program will create a new
               set in which the FREQ axis disappeared and a new axis
               (PARAM) is made with length 3. The first subset along
               the PARAM axis contains the integrated subsets (in units
               of the original data) and the second one contains the
               velocity field (m/s) and the third the dispersion in m/s.
               The velocity is an intensity weighted mean of the
               converted velocities along your operation axis (the FREQ
               axis in the example).
               In general, the program will work for profiles in any
               direction and instead of velocity you could think of any
               kind of physical coordinate. The program inserts
               automatically the correct units in the header of the
               output set.

               Use can be made of a cutoff in the data values. This
               is achieved by specifying a range in amplitudes with
               the keyword RANGE=
               This keyword is prompted if AUTO=N (default). If AUTO=Y
               then a lower cutof value is calculated per profile.
               This threshold is the absolute value of the pixel in the
               profile with the highest negative value and only pixel
               values higher than the threshold are included in the
               moment analysis.


               Examples of the use of the RANGE= keyword:

               RANGE=0, 20
               (If 0<value<20 then it is taken into account, else it
               is set to blank)

               RANGE=3, -3
               (Values greater than 3 and values less than -3 are
               taken into account, others are set to blank). However
               this will include negative weights, wich results in
               incorrect statistics.

               At the RANGE= keyword, the values -INF and INF can
               be input also. These values represent the minimum
               and maximum values of the current system.

               So, all values outside the range are set to blank and
               calculations are done on all non blank values in a profile,
               except when a window was selected i.e. at the WINDOW=
               prompt a value greater than 0 was given.
               If there are a certain number of adjacent subsets with
               amplitudes within the given range, and this number of
               subsets is greater than or equal to WINDOW, these sub-
               sets are taken into account. If this number of subsets
               is less than WINDOW, they are not taken into account.
               If there are more peaks with width equal to or greater
               than WINDOW, you have to select an operation mode. The de-
               fault (WINMODE=0) selects the peak with the greatest flux
               and its data contributes to the statistics. If however
               WINMODE=1 all valid peaks contribute to the statistics.
               In this mode, it is, for example, possible to exclude one
               pixel wide noise peaks from the profile with:

                            MOMENTS WINDOW=2 WINMODE=1

               The program updates minimum, maximum, number of blanks
               and the units of the output at subset level and writes
               a comment, (source of header information and date) at
               set level.

               Option 7 is implemented as an alternative method of
               determining the (weighted) physical coordinate at
               the maximum in the profile. It uses the calculation
               of the 'mode'. For non-symmetrical profiles, the
               method should result in better approximations than
               the intensity weighted mean.
               
               The intensity weighted mode is calculated using the relation:
               
               skewness = (mean-MODE)/(dispersion)

               where mean, dispersion and skewness are the first, 
               second and third weighted moments. The skewness is not part 
               of the output of moments, but it can be retrieved using the 
               above formula.




Example:       <USER> moments
               MOMENTS  Version 1.0  (Oct  9 1992)
               <USER> MOMENTS INSET=M8320 f
               Set M8320 has 3 axes
               RA-NCP             from   -50 to    40
               DEC-NCP            from   -50 to    20
               FREQ-OHEL          from     1 to    59
               <USER> MOMENTS BOX=-10 -10 10 10
               BOX range for set M8320 :
               RA-NCP             from   -10 to    10
               DEC-NCP            from   -10 to    10
               <USER> MOMENTS OPTION=1 3 4:6
               <USER> MOMENTS OUTSET=M8320moments
               Set M8320moments has 3 axes
               RA-NCP             from   -50 to    40
               DEC-NCP            from   -50 to    20
               PARAM-MOMENTS      from     0 to     4
               <USER> MOMENTS RANGE=
               <USER> MOMENTS WINDOW=
               ============================= MOMENTS ===========================
               Profiles examined from set [M8320]
               Results in output set [M8320moments]
                  -subset 0: Intensity weighted mean of physical coords.
                  -subset 1: Physical coordinate of peak value in profile.
                  -subset 2: Peak value in profile.
                  -subset 3: Dispersion in intensities.
                  -subset 4: Number of subset contributions.
               Units of operation axis: KM/S
               Units of data: W.U.
               <STATUS>  MOMENTS   +++ FINISHED +++


Updates:       Apr 22, 1991: VOG, Document created.
               Oct  8, 1992: VOG, Store physical coordinate of peak
                                  also (option 3).
               Jan 20, 1993: VOG, Profile statistics in function, keyword
                                  WINMODE= added.
               Jul  1, 1994: VOG, 'Mode' option implemented.
               Oct 25, 1995: VOG, Changed sigma in 3rd moment (skewness)
               Feb  1, 2000: JPT, Increased number of subsets.
               May 26, 2000: VOG, Added option flag for st_weights to be able
                                  to get a stand alone dispersion set.
               Apr 20, 2007: VOG, Changed documentation about profile integration
               Dec  5, 2014: VOG, Promoted calculations to double precision.
                                  More attention to restriction of positive weights.
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
#include "float.h"
#include "gdsinp.h"
#include "gdsasn.h"
#include "gdscpa.h"
#include "gdsout.h"
#include "gdsc_ndims.h"
#include "setfblank.h"
#include "setdblank.h"
#include "myname.h"
#include "anyout.h"
#include "nelc.h"
#include "cancel.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsbox.h"
#include "gdsc_fill.h"
#include "gdsi_read.h"
#include "gdsi_write.h"
#include "userint.h"
#include "usertext.h"
#include "cotrans.h"
#include "cancel.h"
#include "gdsd_rchar.h"
#include "gdsd_wchar.h"
#include "gdsd_wvar.h"
#include "error.h"
#include "stabar.h"
#include "axunit.h"
#include "gdsc_name.h"
#include "userreal.h"
#include "userlog.h"
#include "userint.h"
#include "reject.h"
#include "getrange.h"
#include "minmax3.h"
#include "wminmax.h"
#include "setnfblank.h"
#include "presetd.h"
#include "preseti.h"
#include "presetr.h"
#include "clipper.h"
#include "status.h"
#include "time.h"


#define AXESMAX    10               /* Max. allowed number of axes in a set */
#define SUBSMAX    2048             /* Max. number of substructures to be specified */
#define MAXBUF     1024             /* Buffer size for I/O */
#define MAXMOMENTS 8                /* Total numer of possible options */
#define BIGSTORE   80               /* Length of a string */
#define VERSION    "1.0"            /* Version number of this program */
#define NONE       0                /* Default values for use in userxxx routines */
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define EPSILON    0.000000000001
#define false      0
#define true       1


/* Keywords and messages */

#define KEY_INSET         tofchar("INSET=")
#define MES_INSET         tofchar("Give set (, subsets): " )
#define KEY_OUTSET        tofchar("OUTSET=")
#define MES_OUTSET        tofchar("Give output set: ")
#define KEY_BOX           tofchar("BOX=")
#define MES_BOX           tofchar(" ")
#define KEY_OPTION        tofchar("OPTION=")
#define MES_OPTION        tofchar("Give option(s) 0..7        [List options]")
#define KEY_AUTO          tofchar("AUTO=")
#define MES_AUTO          tofchar("Automatically set minimum per profile:    Y/[N]")
#define KEY_RANGE         tofchar("RANGE=")
#define MES_RANGE         tofchar("Give range of levels to work on:  [0 inf]")
#define KEY_WINDOW        tofchar("WINDOW=")
#define MES_WINDOW        tofchar("Give window:       [0, i.e. no window]")
#define KEY_WINMODE       tofchar("WINMODE=")
#define MES_WINMODE       tofchar("Stat. in most sign. peak(0), or all peaks(1):    [0]/1")

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
static fint     Fdfault;               /* Default option for input etc */
static fint     Faxnum[AXESMAX];       /* GDSINP axis numbers array */
static fint     Faxcount[AXESMAX];     /* GDSINP axis lengths array */
static fint     Fclass = 2;            /* Axis is operation axis */
static fint     Fsetdim;               /* Dimension of the set */
static fint     Fsubdim;               /* Dimension of the subset */
static fint     Fscrnum = 11;          /* Destination of log output */
static fint     Fmaxaxes  = AXESMAX;   /* Convert parameters to variables */
static fint     Fmaxsubs  = SUBSMAX;
static fint     FmaxIObuf = MAXBUF;
static int      subnr;                 /* Index of current subset */
static int      paramnr;               /* Index of current output subset */
static int      i,j, m;                /* Counters */
static fint     Fwholeset = 0;         /* Indicate set level */
static fint     Faxesoutsidesub;
static fint     Foptions[MAXMOMENTS];

/* Output set related */

static fchar    Fsetout;               /* Name of the set */
static fint     Fsubout[SUBSMAX];      /* Array for the subset coordinate words */
static fint     FnsubsO;               /* Number of input subsets */
static fint     FaxnumO[AXESMAX];      /* GDSINP axis numbers array */
static fint     FaxcountO[AXESMAX];    /* GDSINP axis lengths array */


/* Input of area etc.:*/

static fint     Fcwlo[SUBSMAX];        /* Coordinate words */
static fint     Fcwhi[SUBSMAX];
static fint     FcwloOUT[MAXMOMENTS];
static fint     FcwhiOUT[MAXMOMENTS];
static fint     FgridLO[AXESMAX];      /* Coordinate words for frame */
static fint     FgridHI[AXESMAX];
static fint     FgridLOO[AXESMAX];     /* Coordinate words for frame */
static fint     FgridHIO[AXESMAX];
static fint     BgridLO[AXESMAX];      /* Coordinate words for box */
static fint     BgridHI[AXESMAX];
static fint     Fboxopt;               /* Input option for 'gdsbox' */


/* Data transfer: */

static fint     Ftotpixels;            /* Total number of pixels in input */
static fint     Fpixelsdone;
static fint     FtidIN[SUBSMAX];
static fint     FtidOUT[MAXMOMENTS];   /* Transfer id's */
static float    imageIN[MAXBUF];       /* Multiple buffer for all subsets */


/* Related to update of header etc: */

static float    datamin[MAXMOMENTS];
static float    datamax[MAXMOMENTS];
static fint     Fnblanks[MAXMOMENTS];
static fint     Fremove;
static fchar    Faxunits;
static fchar    Fdataunits;


/* Stabar related: */

static float    STBstart;
static float    STBend;
static float    STBcurrent;


/* Miscellaneous: */

static fint     Fnumitems;             /* Max. number of input items in userxxx routines */
static fint     Fr1, Fr2;              /* Results of userxxx routines */
static float    blank;                 /* Value of system blank */
static double   dblank;
static char     messbuf[BIGSTORE];     /* Buffer for text message */
static fint     Foutaxislen;
static fint     Fmcount[MAXMOMENTS];   /* Counter for 'minmax3' routine */
static float    range[2];              /* User given data in or out range */
static bool     automatic;             /* Calculate ranges per profile automatically? */

static float    st_sum[MAXBUF];        /* Same arrays, but only for 1 peak */
static float    st_weidisp[MAXBUF];
static float    st_peakwidth[MAXBUF];
static float    st_maximum[MAXBUF];
static float    st_maxphys[MAXBUF];
static float    st_profdisp[MAXBUF];
static float    st_wmean[MAXBUF];
static float    st_mode[MAXBUF];

static fint     Fwindow;               /* Width of selected window */
static fint     Fmode;
static float    value;
static int      moment;

static float    physcoord[SUBSMAX];        /* Physical coordinates */
static int      agreed;
static int      optionflag[MAXMOMENTS];    /* Which options were selected? */
static int      optionparam[MAXMOMENTS];   /* Select output subset number */
static bool     blankcube;
static float    profiles[MAXBUF][SUBSMAX]; /* MAXBUF profiles of length SUBSMAX each */


static void anyoutC( int dev, char *anyCstr )
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
   fint    Fwholeset = 0;
   fint    Fr1;


   Fsetdim  = gdsc_ndims_c( Fsetin, &Fwholeset );
   (void) presetd_c( &zero, coordin, &Fsetdim );
   n = (int) Fsetdim - 1;                               /* Axnum = 1, 2, ... */
   Fr1 = 0;
   coordin[(int) Faxnum[n]-1] = (double) gdsc_grid_c( Fsetin,
                                                      &Faxnum[n],
                                                      &cowosubset,
                                                      &Fr1 );
   Fres = cotrans_c( Fsetin,
                     &cowosubset,
                     coordin, coordout,
                     &Fdirect );
   if ((int) Fres != 0) {
      if (subnr == 0) {
         anyoutC(3, "MOMENTS: Cannot find physical coordinates!");
         anyoutC(3, "MOMENTS: Substituting subset numbers.");
      }
      coordout[(int) Faxnum[n]-1] = (double) subnr + 1;
   }
   return ( coordout[(int) Faxnum[n] - 1] );
}



static char *makedate( char *buffer,
                       int   maxlen )
/*------------------------------------------------------------*/
/* PURPOSE: Return date in format : 29-NOV-1990               */
/*         'maxlen' gives max. length of buffer.              */
/*------------------------------------------------------------*/
{
   struct tm   *ptr;
   time_t      lt;


   lt    = time(NULL);                         /* Get the coded calendar time */
   ptr   = localtime(&lt);
   strftime( buffer, maxlen, "%d-%b-%Y", ptr );
   return( buffer );
}




static void writeandupdate( float *imageOUT, int paramnr )
/*
 *------------------------------------------------------------------------------
 * Write part of image to disk, and keep administration for header update.
 * Note: Almost all variables are global!
 *------------------------------------------------------------------------------
 */
{
    gdsi_write_c( Fsetout,
                  &FcwloOUT[paramnr], &FcwhiOUT[paramnr],
                  imageOUT,
                  &Fpixelsdone, &Fpixelsdone,
                  &FtidOUT[paramnr] );

    (void) minmax3_c( imageOUT,
                      &Fpixelsdone,
                      &datamin[paramnr], &datamax[paramnr],
                      &Fnblanks[paramnr],
                      &Fmcount[paramnr] );
}



static void blankoutset( fchar Setout )
/*---------------------------------------------*/
/* Blank the output set                        */
/*---------------------------------------------*/
{
   float   blankarray[MAXBUF];
   fint    maxlen = MAXBUF;
   fint    pixelsdone;
   fint    tid = 0;
   fint    setlevel = 0;
   fint    r1 = 0;
   fint    cwlo, cwhi;


   (void) gdsc_range_c( Setout, &setlevel, &cwlo, &cwhi, &r1 );
   setnfblank_c( blankarray, &maxlen );
   do {
      gdsi_write_c( Setout,
                    &cwlo, &cwhi,
                    blankarray,
                    &maxlen,
                    &pixelsdone,
                    &tid );
   } while( tid != 0 );
}



static void profstat( float *profile, int ndat, int *optionflag,
                      int minwinwidth, int winmode,
                      float *vsum, float *vweimean, float *vweidisp,
                      float *vmaxphys, float *vmaximum, float *vprofdisp,
                      float *vpeakwidth, float *vmode )
/*--------------------------------------------------------------------------*/
/* If no window was specified (window == 0) then calculate sum etc of all   */
/* non blank elements in this profile. If there is a window, there are two  */
/* modes of operation: 1) (winmode == 0) Return values of peak with grea-   */
/* test flux, or 2) (winmode == 1) return values of all selected peaks.     */
/*--------------------------------------------------------------------------*/
{
   double     sum;
   double     weightedsum;
   double     sumvel;
   double     sumweights;
   double     peakwidth;
   double     sumsqr;
   double     sumsqrunw;
   double     sumsqrAmpl;
   float      maximum;
   float      maxphys;
   double     value;
   int        i,j;                        /* Index from 0 to profile length (ndat) */
   bool       winselect;
   double     st_sum = dblank;             /* Blank for initialization */
   double     st_peakwidth   = 0.0;
   double     st_weightedsum = 0.0;
   double     st_sumweights  = 0.0;
   double     st_sumvel      = 0.0;
   double     st_sumsqr      = 0.0;
   double     st_sumsqrunw   = 0.0;
   double     st_sumsqrAmpl  = 0.0;
   float      st_maximum     = blank;
   float      st_maxphys     = blank;



   *vsum       = dblank;                  /* In case profile contains blanks only */
   *vweimean   = dblank;
   *vweidisp   = dblank;
   *vmaxphys   = dblank;
   *vmaximum   = dblank;
   *vprofdisp  = dblank;
   *vpeakwidth = dblank;

   winselect = (minwinwidth > 0);


   /* Find first non blank in profile */

   i = 0;
   while ((profile[i] == blank) && (i < ndat)) i++;
   if (i == ndat-1) {
      /* All data blank, return. */
      return;
   }

   sum          = 0.0;
   weightedsum  = 0.0;
   sumvel       = 0.0;
   sumweights   = 0.0;
   peakwidth    = 0.0;
   sumsqr       = 0.0;
   sumsqrunw    = 0.0;
   sumsqrAmpl   = 0.0;
   maximum      = profile[i];                         /* value of first non blank pixel */
   maxphys      = physcoord[i];

   for (j = i; j < ndat; j++) {
      double physval;
      value = (double) profile[j];
      if (profile[j] != blank) {
         physval = (double) physcoord[j];         
         sum += value;
         peakwidth += 1.0;
         if ((optionflag[1] || optionflag[2] || optionflag[7])) {      /* Intensity weighted mean */
            weightedsum += physval * value;
            sumvel      += physval;      /* Included for option 7 */
            sumweights  += value;
         }
         if (optionflag[2] || optionflag[7]) {                         /* Dispersion in operation axis units */
            sumsqr += value * physval * physval;
            sumsqrunw += physval * physval;    /* Included for option 7 */
         }
         if (optionflag[3] || optionflag[4]) {
            if (maximum != blank) {
               if (value > maximum) {
                  maximum = value;
                  maxphys = physcoord[j];
               }
            } else {
               maximum = value;
               maxphys = physcoord[j];
            }
         }
         if (optionflag[5]) {                         /* Dispersion in amplitude */
            sumsqrAmpl += value * value;
         }
      }
      if (winselect) {                                    /* Is windowing on? */
         if ( (profile[j] == blank) || (j == ndat - 1) ) {     /* Is this the end of a window? */
            if (winmode == 0) {
               /*-------------------------------------------*/
               /* Store if this window was ok and it is the */
               /* first peak. If it is not the first peak,  */
               /* compare with the previous one.            */
               /*-------------------------------------------*/
               if ((int) peakwidth >= minwinwidth) {      /* Width is ok! */
                  if ( (st_sum == dblank) || (st_sum != dblank && sum > st_sum) )
                  {
                     st_sum = sum;
                     st_peakwidth = peakwidth;
                     if (optionflag[1] || optionflag[2] || optionflag[7]) {
                        st_weightedsum = weightedsum;
                        st_sumweights  = sumweights;
                     }
                     if (optionflag[2]) st_sumsqr  = sumsqr;
                     if (optionflag[3]) st_maxphys = maxphys;
                     if (optionflag[4]) st_maximum = maximum;
                     if (optionflag[5]) st_sumsqrAmpl = sumsqrAmpl;
                     if (optionflag[7]) st_sumsqrunw  = sumsqrunw;
                  }
               }
               maximum = blank;                           /* Forget previous values */
               maxphys = blank;
            } else {
               /*---------------------------------------------------*/
               /* If this window was ok, add sums etc. to previous  */
               /* windows.                                          */
               /*---------------------------------------------------*/
               if ((int) peakwidth >= minwinwidth) {      /* Width is ok! */
                  if (st_sum == dblank) {
                     st_sum = sum;
                     st_peakwidth = peakwidth;
                     if (optionflag[1] || optionflag[2] || optionflag[7]) {
                        st_weightedsum = weightedsum;
                        st_sumweights  = sumweights;
                     }
                     if (optionflag[2]) st_sumsqr  = sumsqr;
                     if (optionflag[3]) st_maxphys = maxphys;
                     if (optionflag[4]) st_maximum = maximum;
                     if (optionflag[5]) st_sumsqrAmpl = sumsqrAmpl;
                     if (optionflag[7]) st_sumsqrunw  = sumsqrunw;
                  } else {
                     st_sum += sum;                       /* Just ADD to the total */
                     st_peakwidth += peakwidth;
                     if (optionflag[1] || optionflag[2] || optionflag[7]) {
                        st_weightedsum += weightedsum;
                        st_sumweights  += sumweights;
                     }
                     if (optionflag[2]) st_sumsqr  += sumsqr;
                     if (optionflag[5]) st_sumsqrAmpl += sumsqrAmpl;
                     if (optionflag[3]) st_maxphys = maxphys;    /*Current max.*/
                     if (optionflag[4]) st_maximum = maximum;
                     if (optionflag[7]) st_sumsqrunw += sumsqrunw;
                  }
               }
            }
            /* End of window, so reset arrays */
            sum          = 0.0;
            peakwidth    = 0.0;
            weightedsum  = 0.0;
            sumweights   = 0.0;
            sumsqr       = 0.0;
            sumsqrunw    = 0.0;
         }
      }
   }
   if (!winselect) {
      st_sum = sum;
      if (optionflag[1] || optionflag[2] || optionflag[7]) {
         st_weightedsum = weightedsum;
         st_sumweights  = sumweights;
         st_sumvel      = sumvel;
      }
      if (optionflag[2] || optionflag[7]) st_sumsqr += sumsqr;
      if (optionflag[3]) st_maxphys = maxphys;
      if (optionflag[4]) st_maximum = maximum;
      if (optionflag[5]) st_sumsqrAmpl = sumsqrAmpl;
      if (optionflag[7]) st_sumsqrunw += sumsqrunw;
      st_peakwidth = peakwidth;
   }

   *vsum = (float) st_sum;
   *vpeakwidth = (float) st_peakwidth;


   if (optionflag[1] || optionflag[2] || optionflag[7]) {
      value = st_sumweights;
      if ((value == dblank) || (fabs(value) < EPSILON)) {
          *vweimean = blank;
      } else {
         *vweimean = (float) (st_weightedsum / st_sumweights);
          /*sprintf( messbuf, "mode, weisum, sumwe, mean=%d %f %f %f", winmode, st_weightedsum,st_sumweights,*vweimean );
            anyoutC( 1, messbuf ); */

      }
   }

   if (optionflag[2]) {
      value = st_sumweights;
      if ((value == dblank) || (fabs(value) < EPSILON) || (*vweimean == blank)) {
         *vweidisp = blank;
         /*sprintf( messbuf, "Rejected: st_sumweights = %f", st_sumweights);
           anyoutC( 1, messbuf );*/
      }
      else {
         /* double Vwmean = st_weightedsum / st_sumweights; */
         /* double Vwdisp = st_sumsqr/st_sumweights - (Vwmean * Vwmean);*/
         double Vwdisp = (st_sumsqr - st_weightedsum*st_weightedsum/st_sumweights)/st_sumweights;
         
         if (Vwdisp >= 0.0) {
            /* sprintf( messbuf, "st_sumweights = %f Vwdisp=%f, sqrt()=%f", st_sumweights, Vwdisp, sqrt(Vwdisp));
               anyoutC( 1, messbuf ); */
            *vweidisp = (float) sqrt(Vwdisp);
         }
         else {            
            *vweidisp = blank;
         }
      }
   }

   if (optionflag[3]) {
      *vmaxphys = st_maxphys;
   }

   if (optionflag[4]) {
      *vmaximum = st_maximum;
   }

   if (optionflag[5]) {
      value = st_peakwidth;
      if ((value == dblank) || (value == 0.0)) {
          *vprofdisp = blank;
      }
      else {
         double Vprofdisp = st_sumsqrAmpl/st_peakwidth  - (st_sum/st_peakwidth)*(st_sum/st_peakwidth);
         if (Vprofdisp >= 0.0) {
             *vprofdisp = sqrt( Vprofdisp );
         }
         else {
             *vprofdisp = blank;
         }
      }
   }
   if (optionflag[7])
   {
      /* Added on june 29 1994. Calculate mode using third moment. */
      /* You need an extra loop through the profile. */
      /* The use of a window is not yet implemented. */


      if ((st_sumsqr == dblank) || (*vweimean == blank))
      {
          *vmode = blank;
      }
      else
      {
         double    skewhalf = 0.0;
         double    ave_vel;
         i = 0;
         while ((profile[i] == blank) && (i < ndat)) i++;
         if (i == ndat-1)
         {
            /* All data blank, return. */
            *vmode = blank;
            return;
         }
         for (j = i; j < ndat; j++)
         {
            value = (double) profile[j];
            if (profile[j] != blank)
            {
               double Vwmean = st_weightedsum / st_sumweights;
               double physval = (double) physcoord[j];
               skewhalf += pow( (physval - Vwmean), 3 ) * value;
            }
         }
         skewhalf /= st_sumweights;
         ave_vel = st_sumvel/st_peakwidth;
         
         st_sumsqrunw = (double) (*vweidisp);
         /*----------------------------------------------------------*/
         /* Note that the dispersion used in the skewness formula is */
         /* the intensity weighted dispersion. This is changed at    */
         /* 25 oct 1995.                                             */
         /*----------------------------------------------------------*/
/*         st_sumsqrunw = (st_sumsqrunw/(st_peakwidth)) - (ave_vel*ave_vel);
           st_sumsqrunw = sqrt(st_sumsqrunw); */
           
         if (st_sumsqrunw <= 0.0)
            *vmode = blank;
         else
         {
            double Vwmean = st_weightedsum / st_sumweights;
            *vmode = (float) (Vwmean - (skewhalf / (st_sumsqrunw*st_sumsqrunw) ));
         }

         /* sprintf( messbuf, "vmode=%f, weimean=%f skewhalf=%f st_sumsqr=%f vweidisp=%f ave_vel=%f",
            *vmode, *vweimean , skewhalf, st_sumsqrunw, *vweidisp, ave_vel );
            anyoutC(1, messbuf );
         */
      }
   }
}



MAIN_PROGRAM_ENTRY
{

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
   setdblank_c( &dblank );
   fmake(Fsetin, 80);
  /*
   *----------------------------------------------------------------------------
   * Because Fortran passes all arguments by reference, all C functions with
   * a Fortran equivalent must do this also (GIPSY programmers guide,
   * Chapter 9).
   * If the subset dimension 'Fsubdim' is set to 0 before the call to 'gdsinp',
   * the input routine accepts subsets of arbitrary dimensions smaller than
   * the dimension of the set, and it returns the dimension of the user created
   * subset.
   *----------------------------------------------------------------------------
   */
   Fdfault = NONE;
   Faxesoutsidesub = 1;
   Fscrnum = 3;
   Fclass  = 2;
   FnsubsI  = gdsinp_c( Fsetin, Fsubin, &Fmaxsubs, &Fdfault, KEY_INSET,
                        MES_INSET, &Fscrnum, Faxnum, Faxcount, &Fmaxaxes,
                        &Fclass, &Faxesoutsidesub );
   Fsetdim  = gdsc_ndims_c( Fsetin, &Fwholeset );
   Fsubdim  = Fsetdim - Faxesoutsidesub;
  /*
   *----------------------------------------------------------------------------
   * Determine the edges of this its frame (FgridLO/HI)
   *----------------------------------------------------------------------------
   */
   Fr1 = 0;
   (void) gdsc_range_c( Fsetin, &Fwholeset, &Fcwlo[0], &Fcwhi[0], &Fr1 );
   Fr1 = Fr2 = 0;
   for (m = 0; m < (int) Fsetdim; m++) {
      FgridLO[m] = gdsc_grid_c( Fsetin, &Faxnum[m], &Fcwlo[0], &Fr1 );
      FgridHI[m] = gdsc_grid_c( Fsetin, &Faxnum[m], &Fcwhi[0], &Fr2 );
   }

  /*
   *----------------------------------------------------------------------------
   * Prepare a box for INSET. Default is a box equal to the frame.
   *----------------------------------------------------------------------------
   */
   Fdfault = REQUEST;
   Fboxopt = 0;
   Fscrnum = 3;
   (void) gdsbox_c( BgridLO, BgridHI, Fsetin, Fsubin,
                    &Fdfault, KEY_BOX, MES_BOX, &Fscrnum, &Fboxopt );
   /* Count number of pixels in this substructure */
   Ftotpixels = 1;
   /* Total number of pixels in (boxed) subset */
   for(m = 0; m < (int) Fsubdim; m++) Ftotpixels *= (BgridHI[m] - BgridLO[m] + 1);


  /*
   *----------------------------------------------------------------------------
   * GDSASN is needed before the call to GDSOUT to obtain
   * an output set. GDSASN copies the axis names from the
   * set previously opened by GDSINP in the order as determined
   * by the user at the associated GDSINP request. This buffer
   * will be read by GDSOUT, which will create an output set
   * according to the specifications in this buffer.
   *----------------------------------------------------------------------------
   */
   (void) gdsasn_c( KEY_INSET, KEY_OUTSET, &Fclass );
   /*
   *----------------------------------------------------------------------------
   * GDSCPA changes the primary axis of an output set to be
   * obtained by GDSOUT. In this case add 'PARAM' axis to GDSOUT buffer
   *----------------------------------------------------------------------------
   */
   {
#define AXNAME      tofchar("PARAM-MOMENTS")
#define CUNIT       tofchar(" ")

      fint    Fpmask;
      fint    Faxisnumber;
      double  outcdelt = 1.0;
      double  outcrota = 0.0;
      double  outcrpix = 1.0;        /* ==> param 0 = moment 0 */
      double  outcrval = 0.0;
      int     outsubs  = 0;

      /* Get options from user to determine the length of the param axis */

      Fnumitems = MAXMOMENTS;
      Fdfault   = REQUEST;
      do {
         Fr1    = userint_c( Foptions, &Fnumitems, &Fdfault,
                             KEY_OPTION, MES_OPTION );
         agreed = false;
         if ((int) Fr1 == 0) {
            anyoutC(3, "           ======MOMENTS======");
            anyoutC(3, "0 Integrate subsets along operation axis" );
            anyoutC(3, "1 Intensity-weighted mean in profile" );
            anyoutC(3, "2 Intensity weighted dispersion in profile" );
            anyoutC(3, "3 Physical coordinate of maximum value on operating axis");
            anyoutC(3, "4 Maximum values on operating axis" );
            anyoutC(3, "5 Dispersion in intensities" );
            anyoutC(3, "6 Number of subsets" );
            anyoutC(3, "7 Intensity weighted Mode (only with WINDOW=0)" );
            (void) cancel_c( KEY_OPTION );
         }
         else {
            for (moment = 0; moment < MAXMOMENTS; moment++) {
               optionflag[moment] = 0;
               for (j = 0; j < (int) Fr1; j++) {
                  if (moment == Foptions[j]) {
                     optionflag[moment] = 1;
                  }
               }
            }

            /* Check whether at least one flag is set and give each
               moment a unique output subset number. */

            outsubs = 0;
            for (moment = 0; moment < MAXMOMENTS; moment++) {
               if (optionflag[moment]) {
                  agreed = true;
                  optionparam[moment] = outsubs++;
               }
            }
            if (!agreed) (void) reject_c( KEY_OPTION, tofchar("Nothing selected!") );
         }
      } while (!agreed);

      Foutaxislen = outsubs;
      Fpmask = 14;                /* pmask = 14 (2 + 4 + 8) means that CTYPE,
                                     CRPIX and CRVAL are defined.*/
      Faxisnumber = Fsubdim + 1;

      (void) gdscpa_c( KEY_OUTSET,    /* Keyword associated with a GDSOUT call.*/
                       &Faxisnumber,  /* The axis number of the axis to be changed.*/
                       &Foutaxislen,  /* Size of the axis.*/
                       &outcdelt,     /* Increment in physical units along axis.*/
                       &outcrota,     /* Rotation angle of axis.*/
                       &outcrpix,     /* Reference pixel of axis.*/
                       &outcrval,     /* Physical reference value at reference pixel.*/
                       AXNAME,        /* New axis name.*/
                       CUNIT,         /* Physical units of axis.*/
                       &Fpmask );     /* Bit mask denoting which of the six above
                                         values are defined.*/
   }


  /*
   *----------------------------------------------------------------------------
   * Get output set:
   *----------------------------------------------------------------------------
   */
   fmake(Fsetout, BIGSTORE);
   Fdfault = NONE;
   FnsubsO = gdsout_c( Fsetout, Fsubout, &Foutaxislen, &Fdfault,
                       KEY_OUTSET, MES_OUTSET,
                       &Fscrnum, FaxnumO, FaxcountO, &Fmaxaxes );



   Fr1 = 0;
   (void) gdsc_range_c( Fsetout, &Fwholeset, &FcwloOUT[0], &FcwhiOUT[0], &Fr1 );
   Fr1 = Fr2 = 0;
   for (m = 0; m < (int) Fsetdim; m++) {
      FgridLOO[m] = gdsc_grid_c( Fsetout, &FaxnumO[m], &FcwloOUT[0], &Fr1 );
      FgridHIO[m] = gdsc_grid_c( Fsetout, &FaxnumO[m], &FcwhiOUT[0], &Fr2 );
   }

   blankcube = 0;
   for (m = 0; m < (int) Fsubdim; m++) {
      blankcube = (blankcube || (BgridLO[m] != FgridLO[m]) || (BgridHI[m] != FgridHI[m]));
   }
   if (blankcube) {
      status_c(tofchar("Blanking outside box"));
      blankoutset( Fsetout );
   }

   Fnumitems = 1;
   Fdfault  = REQUEST;
   automatic = tobool(false);
   Fr1 = userlog_c( &automatic, &Fnumitems, &Fdfault, KEY_AUTO, MES_AUTO );
   
   /* Define a data range. Changed at 2-12-2014 */

   if (!automatic) {
      range[0] = 0.0;    /*-1.0*FLT_MAX; do not accept negative weights*/ /* Defined in float.h */
      range[1] = FLT_MAX;
      Fdfault  = REQUEST;
      getrange_c( range, &Fdfault, KEY_RANGE, MES_RANGE );
   }

   

   Fdfault = REQUEST;
   Fnumitems = 1;
   do {
      Fwindow  = 0;
      Fr1 = userint_c( &Fwindow, &Fnumitems, &Fdfault,
                       KEY_WINDOW, MES_WINDOW );
      agreed = ((Fwindow >= 0) && (Fwindow <= FnsubsI));
      if (!agreed) reject_c( KEY_WINDOW, tofchar("Not allowed!") );
   } while (!agreed);

   if (Fwindow > 0) {
      Fdfault = REQUEST;
      Fnumitems = 1;

      do {
         Fr1 = userint_c( &Fmode, &Fnumitems, &Fdfault,
                          KEY_WINMODE, MES_WINMODE );
         agreed = ((Fmode == 0) || (Fmode == 1));
         if (!agreed) reject_c( KEY_WINMODE, tofchar("Not allowed!") );
      } while (!agreed);
   }


   /* Get units of operation axis and data units */
   {
      fint Fresult;
      fint Ferrlev = 4;
      fint n = Fsetdim - 1;

      fmake( Faxunits, BIGSTORE );
      Fresult = axunit_c( Fsetin, &Faxnum[n], Faxunits );
      if (Fresult == 1) (void) error_c( &Ferrlev, tofchar("A cotrans error occured!") );
      if (Fresult == 2) (void) error_c( &Ferrlev, tofchar("Axis not present in set!") );
      if (Fresult == 3) (void) error_c( &Ferrlev, tofchar("Output character string not large enough!") );
      fmake( Fdataunits, BIGSTORE );
      Fr1 = 0;
      gdsd_rchar_c( Fsetin, tofchar("BUNIT"), &Fwholeset, Fdataunits, &Fr1 );
      if (Fr1 < 0) {
         anyoutC(3, "MOMENTS: Cannot find map units (BUNIT) in header");
         strcpy( Fdataunits.a, "?" );
      }
   }


  /*
   *----------------------------------------------------------------------------
   * Give user some data in log-file
   *----------------------------------------------------------------------------
   */
   sprintf( messbuf,
   "=================================== MOMENTS =================================" );
   anyoutC( 3, messbuf );
   sprintf( messbuf,
           "Profiles examined from set [%.*s]",
            nelc_c(Fsetin),
            Fsetin.a );
   anyoutC( 3, messbuf );
   sprintf( messbuf,
           "Results in output set [%.*s]",
            nelc_c(Fsetout),
            Fsetout.a );
   anyoutC( 3, messbuf );

   for(m = 0, subnr = 0; m < MAXMOMENTS; m++) {
      if (optionflag[m]) {
         sprintf( messbuf, "   -subset %d:", subnr++ );
         switch (m) {
            case 0:
            sprintf( messbuf, "%.*s Sum of profile amplitudes (0 moment).",
                     (int) strlen(messbuf), messbuf );
            break;
            case 1:
            sprintf( messbuf, "%.*s Intensity weighted mean of physical coords. (1 moment).",
                     (int) strlen(messbuf), messbuf );
            break;
            case 2:
            sprintf( messbuf, "%.*s Dispersion in intensity weighted physical coords. (2 moment).",
                     (int) strlen(messbuf), messbuf );
            break;
            case 3:
            sprintf( messbuf, "%.*s Physical coordinate of peak value in profile.",
                     (int) strlen(messbuf), messbuf );
            break;
            case 4:
            sprintf( messbuf, "%.*s Peak value in profile.",
                     (int) strlen(messbuf), messbuf );
            break;
            case 5:
            sprintf( messbuf, "%.*s Dispersion in intensities.",
                     (int) strlen(messbuf), messbuf );
            break;
            case 6:
            sprintf( messbuf, "%.*s Number of subset contributions.",
                     (int) strlen(messbuf), messbuf );
            break;
            case 7:
            sprintf( messbuf, "%.*s Intensity weighted Mode.",
                     (int) strlen(messbuf), messbuf );
            break;
         }
         anyoutC( 3, messbuf );
      }
   }

   sprintf( messbuf, "Units of operation axis: %.*s",
                      nelc_c(Faxunits),
                      Faxunits.a );
   anyoutC( 3, messbuf );
   sprintf( messbuf, "Units of data: %.*s",
                      nelc_c(Fdataunits),
                      Fdataunits.a );
   anyoutC( 3, messbuf );



  /*
   *----------------------------------------------------------------------------
   * Select mode: Initialize coordinate words etc. and start calculating.
   *----------------------------------------------------------------------------
   */

   for(subnr = 0; subnr < (int) FnsubsI; subnr++) {
      /* Create coordinate words for the input subsets */
      Fcwlo[subnr] = gdsc_fill_c( Fsetin, &Fsubin[subnr], BgridLO );
      Fcwhi[subnr] = gdsc_fill_c( Fsetin, &Fsubin[subnr], BgridHI );
      /* Reset tranfer id's */
      FtidIN[subnr] = 0;
      /* For each grid on the operation axis, get physical value */
      physcoord[subnr] = (float) getphyscoord( Fsetin, Fsubin[subnr], subnr );
      /* sprintf(messbuf, "%d %f", subnr, physcoord[subnr]);
         anyoutC( 3, messbuf );     */
   }

   /* Create coordinate words for the output subsets */
   for(paramnr = 0; paramnr < (int) FnsubsO; paramnr++) {
      FcwloOUT[paramnr] = gdsc_fill_c( Fsetout, &Fsubout[paramnr], BgridLO );
      FcwhiOUT[paramnr] = gdsc_fill_c( Fsetout, &Fsubout[paramnr], BgridHI );
      FtidOUT[paramnr] = 0;
      Fmcount[paramnr] = 0;
   }


   /* Initialize stabar */

   STBstart   = 0.0;
   STBend     = (float) Ftotpixels * FnsubsI;
   STBcurrent = 0.0;
   (void) stabar_c( &STBstart, &STBend, &STBcurrent );


   /*----------------------------------------------------------------*/
   /* Start reading data until Transfer id == 0. Read MAXBUF pixels  */
   /* of each subset and store in array 'profiles'.                  */
   /*----------------------------------------------------------------*/

   do {
      float    rangelo  = range[0];
      float    rangehi  = range[1];


      /* Loop through input subsets */
      for (subnr = 0; subnr < (int) FnsubsI; subnr++) {

         /* Read data from disk */
         (void) gdsi_read_c( Fsetin,
      	                     &Fcwlo[subnr],
      	                     &Fcwhi[subnr],
      	                     imageIN,
                             &FmaxIObuf,
                             &Fpixelsdone,
                             &FtidIN[subnr] );


         /* Show progress */
         STBcurrent += (float) Fpixelsdone;
         (void) stabar_c( &STBstart, &STBend, &STBcurrent );

         
         if (!automatic) {
            /* Blank values in or out range. Store the data */
            if (rangelo <= rangehi) {
               for (i = 0; i < (int) Fpixelsdone; i++) {
                  value = imageIN[i];
                  if (value <= rangelo) {
                     value = blank;
                  } else {
                     if (value >= rangehi) value = blank;
                  }
                  profiles[i][subnr] = value;
               }
            } else {
               /* rangelo greater than rangehi */
               for (i = 0; i < (int) Fpixelsdone; i++) {
                  value = imageIN[i];
                  if ((value >= rangehi) && (value <= rangelo)) {
                     value = blank;
                  }
                  profiles[i][subnr] = value;
               }
            }
         }
         else {   /* Just copy */
            for (i = 0; i < (int) Fpixelsdone; i++) {
                 profiles[i][subnr] = imageIN[i];
            }
         }            
      } /* Profiles filled for all subsets */


      /*---------------------------------------------------------------*/
      /* Now loop over all profiles and do statistics for each profile */
      /*---------------------------------------------------------------*/

      for (i = 0; i < (int) Fpixelsdone; i++) {
         int    proflen    = (int) FnsubsI;
         int    window     = (int) Fwindow;
         int    windowmode = (int) Fmode;
         int    j;
         float  rangelo = 0.0;
         
         /* If a user selected 'automatic' as option to set ranges for individual profiles,  */
         /* then look in the current profile for the lowest value < 0. This is a measure for */
         /* the noise level if we take the absolute value of this minimum. Note that this is */
         /* only valid because weights must be greater than 0 and we don't want to include   */
         /* positive noise peaks. */
         
         if (automatic) {            
            for (j = 0; j < proflen; j++) {               
               value = profiles[i][j];               
               if (value != blank) {
                  if (value < rangelo) {
                     rangelo = value;
                  }
               }
            }
            rangelo = fabs(rangelo);
            /*
            if (rangelo > 0.0) {
               sprintf(messbuf, "rangelo=%f", rangelo);            
               anyoutC(1, messbuf);
            }
            */
            for (j = 0; j < proflen; j++) {
               value = profiles[i][j];
               if (value != blank) {
                  if (value < rangelo) {
                      profiles[i][j] = blank;
                  }
               }
            }
         }
         
         profstat( &profiles[i][0], proflen, optionflag, window, windowmode,
                   &st_sum[i],
                   &st_wmean[i],
                   &st_weidisp[i],
                   &st_maxphys[i],
                   &st_maximum[i],
                   &st_profdisp[i],
                   &st_peakwidth[i],
                   &st_mode[i] );
      }

      /*--------------------------------------------------------------------*/
      /* Write the calculated arrays to disk and keep administration for    */
      /* header update.                                                     */
      /*--------------------------------------------------------------------*/

      if (optionflag[0]) {  /* Write the sum of profile values */
         (void) writeandupdate( st_sum, optionparam[0] );
      }
      if (optionflag[1]) {  /* Write weighted mean */
         (void) writeandupdate( st_wmean, optionparam[1] );
      }
      if (optionflag[2]) {  /* Write dispersion */
         (void) writeandupdate( st_weidisp, optionparam[2] );
      }
      if (optionflag[3]) {  /* Write peak in profile */
         (void) writeandupdate( st_maxphys, optionparam[3] );
      }
      if (optionflag[4]) {  /* Write peak in profile */
         (void) writeandupdate( st_maximum, optionparam[4] );
      }
      if (optionflag[5]) {  /* Write dispersion in profile amplitudes */
         (void) writeandupdate( st_profdisp, optionparam[5] );
      }
      if (optionflag[6]) {  /* Write number of valid subsets */
         (void) writeandupdate( st_peakwidth, optionparam[6] );
      }
      if (optionflag[7]) {  /* Write number of valid subsets */
         (void) writeandupdate( st_mode, optionparam[7] );
      }
   } while( FtidIN[0] != 0 );


   if (optionflag[0]) {
      Fr1 = 0;
      /* Units are units of the data */
      gdsd_wchar_c( Fsetout, tofchar("BUNIT"), &Fsubout[optionparam[0]],
                    Fdataunits, &Fr1 );
   }
   if (optionflag[1]) {
      Fr1 = 0;
      /* Operation axis units */
      gdsd_wchar_c( Fsetout, tofchar("BUNIT"), &Fsubout[optionparam[1]],
                    Faxunits, &Fr1 );
   }
   if (optionflag[2]) {
      Fr1 = 0;
      /* Operation axis units */
      gdsd_wchar_c( Fsetout, tofchar("BUNIT"), &Fsubout[optionparam[2]],
                    Faxunits, &Fr1 );
   }
   if (optionflag[3]) {
      Fr1 = 0;
      /* Operation axis units */
      gdsd_wchar_c( Fsetout, tofchar("BUNIT"), &Fsubout[optionparam[3]],
                    Faxunits, &Fr1 );
   }
   if (optionflag[4]) {
      Fr1 = 0;
      /* Data units */
      gdsd_wchar_c( Fsetout, tofchar("BUNIT"), &Fsubout[optionparam[4]],
                    Fdataunits, &Fr1 );
   }
   if (optionflag[5]) {
      Fr1 = 0;
      /* Data units */
      gdsd_wchar_c( Fsetout, tofchar("BUNIT"), &Fsubout[optionparam[5]],
                    Fdataunits, &Fr1 );
   }
   if (optionflag[6]) {
      Fr1 = 0;
      /* No units, just numbers */
      gdsd_wchar_c( Fsetout, tofchar("BUNIT"), &Fsubout[optionparam[6]],
                    tofchar("Numbers"), &Fr1 );
   }
   if (optionflag[7]) {
      Fr1 = 0;
      /* Operation axis units */
      gdsd_wchar_c( Fsetout, tofchar("BUNIT"), &Fsubout[optionparam[7]],
                    Faxunits, &Fr1 );
   }


   Fremove = 1;                                 /* Update header of new set */
   (void) wminmax_c( Fsetout, Fsubout,
                     datamin, datamax,
                     Fnblanks,
                     &FnsubsO,
                     &Fremove );



   {
      char timebuf[21];

      sprintf( messbuf,
              "Header copied by program MOMENTS from set %.*s at %s",
               nelc_c(Fsetin),                /* Output setname */
               Fsetin.a,
               makedate(timebuf,20) );        /* Current date */
   }

   Fr1 = 0;
   gdsd_wvar_c( Fsetout,
                tofchar("COMMENT"),
                &Fwholeset,
                tofchar(messbuf),
                &Fr1 );

   finis_c();                                   /* Quit Hermes */
   return 1;                                    /* Dummy return for main */
}
