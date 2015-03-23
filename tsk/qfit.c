/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                 University of Groningen, The Netherlands
                          All Rights Reserved.

#>             qfit.dc1


Program:       QFIT

Purpose:       Interactive fit of 2-D gaussians

Category:      ANALYSIS, PROFILES, MODELS, FITTING

File:          qfit.c

Author:        M. Vogelaar

Keywords:


   FILENAME=   Name of output ASCII file:          [No output to file]
               If a name is specified, an ASCII file is created
               where fit parameters are listed in a row. If you
               press carriage return, there will be no output to
               an ASCII file.


   APPEND=     File exists, ok to append?                        [Y]/N
               The file specified in FILENAME= already exists. You
               can append to this file with APPEND=Y. If APPEND=N
               you will be prompted for another name.


               ==== Next keywords are asked in a loop, to be ====
               ==== aborted with INSET=<CR>                  ====


   INSET=      Give input set, subset(s):                [STOP Program]
               Maximum number of subsets is 2048.
               For each subset a fit is made with the same parameters
               as the first subset. The INSET= keyword is asked in a
               loop that is aborted with carriage return ( <CR> ).


   AUTOVIEW=   Do you want to display input set in GIDS?          Y/[N]
               For interactive fitting it is convenient to determine
               a 'fitbox' on the display. If AUTOVIEW=Y, the program
               VIEW will be started to display the current INSET=
               (See also keywords in 'VIEW' like CLIP=, NEXT=)


               ==== Next keywords are asked in a loop, to be ====
               ==== aborted with BOX=<CR>                    ====


   BOX=        Frame for input subsets.                          [STOP]
               Total number of pixels in box must not
               exceed 128x128 . Keyword is prompted in a loop which is
               aborted with BOX=<CR>
               For each box, all specified subsets are examined before
               asking the next box.


               Estimates and fixed values
               ==========================

               The following keywords are used to give a value
               to the parameters that are kept fixed in the fit, or
               to give a reasonable initial estimate for the free
               parameters.

   AMPLITUDE=  Amplitude (map units):                      [Calculated]

   BEAM=       Beam, fwhm in x & y:                        [Calculated]
               If indicated, in seconds of arc.

   X0Y0=       Centre in grids:                            [Calculated]

   ANGLE=      Angle of FWHM in x wrt. pos. X-axis (deg):  [Calculated]

   ZERO=       Zero level (map units):                     [Calculated]



   MASK=       Mask (0=fixed,1=free):                  [parameter list]


** TOLERANCE=  Convergence criterion.                            [0.01]
               Relative tolerance. Fitting of the function stops when
               successive iterations fail to produce a decrement in
               reduced chi-squared less than TOLERANCE. If its value
               is less than the minimum tolerance possible, it will be
               set to this value. This means that maximum accuracy can
               be obtained by setting TOLERANCE=0.0.


** LAB=        Mixing parameter:                                 [0.01]
               Mixing parameter, LAB determines the initial
               weight of steepest descent method relative to the Taylor
               method. LAB should be a small value (i.e. 0.01).


** CLIPLOHI=   Give clip levels:                     [Include all data]
               Examples:
               1) CLIPLOHI=     include all data
               2) CLIPLOHI=3    include all data >= 3
               3) CLIPLOHI=3 5  include all data >=3 and <= 5

Description:   QFIT fits 2-dim gaussian to data from INSET= in a given
               box in BOX= with a limit of 128x128 pixels. The program
               can be used for example as an flexible alternative for
               ANTPAT to find beam parameters or to find the exact
               position of a source.

               The 2d-gaussian is:

               F(x,y) = par(0) * EXP( -4.0*ALOG(2.0) *
                     [(xr / par(1))**2 + (yr / par(2))**2] + par(6)


               where: xr =  xo * cos(par(5)) + yo * sin(par(5))
                      yr = -xo * sin(par(5)) + yo * cos(par(5))

               and:   xo = x - par(3)
                      yo = y - par(4)

               The gauss parameters are:

               1. Amplitude in Map units
               2. FWHMx in arcs
               3. FWHMy in arcs
               4. X0, center of gauss in grids
               5. Y0, center of gauss in grids
               6. Rotation angle deg. wrt. pos. x-axis.
               7. Zero level in map units

               The fit routine needs estimates for all parameters.
               If you use the default of the AMPLITUDE=, BEAM=, X0Y0=,
               ANGLE= and ZERO= keywords, then, in most circumstances
               you are using reasonable values. The estimated zero level
               is the mean of all the pixels on the borders of your
               selected box (BOX=). The estimated amplitude
               is the calculated maximum of the data in the box minus
               the zero level.
               The estimated FWHM's and position angle are calculated
               from the data. The center positions are copied from the
               positions of the calculated maximum. If you want to change
               these estimates, specify alternative values for one or more
               keywords.

               If, for example, you try to fit a 2d-gaussian for which
               the major axis equals the minor axis but leaving the
               rotation angle free (in this case the PA has no meaning),
               there will be probably an error in the fit. In this case you
               want to do the fit with the rotation angle fixed.
               Parameters can be used as fixed or free parameters
               depending on the setting in MASK=  To fix a parameter
               use 0. A free parameter is indicated with 1. The keywords
               TOLERANCE= and LAB= are fine tuning parameters for a
               least squares routine.

               The table, created in the ASCII file (FILENAME=) has
               a layout set by the user which can easily be changed to suit
               personal whishes..

Example:       Fit gaussian in set 'n4736ave' subset 1. Put results in
               ASCII file with name 'data'. Use box -4 -10 9 5 and let
               all parameters be free in the fit:

   <USER> qfit
   <USER> QFIT FILENAME=data
   <USER> QFIT APPEND=
   <USER> QFIT AUTOVIEW=y
   <USER> QFIT INSET=n4736ave 1
   Set n4736ave has 3 axes
   RA                 from   -20 to    20
   DEC                from   -31 to    20
   LAMBDA             from     0 to     1
   BOX range for set n4736ave :
   RA                 from   -20 to    20
   DEC                from   -31 to    20
   <USER> QFIT CLIPLOHI=
   <USER> QFIT BOX=-4 -10 9 5
   BOX range for set n4736ave :
   RA                 from    -4 to     9
   DEC                from   -10 to     5

   Initial estimates (inserted without the units):
   Amplitude:    347.534729  (MJY / S)
   FWHMx:        123.429893 (ARCSEC)
   FWHMy:        63.259651 (ARCSEC)
   X0:           2.000000 (grids)
   Y0:           -1.000000 (grids)
   Angle:        84.357887 (deg) wrt pos. x-axis:
   Zero level:   10.616884 (MJY / S)

   <USER> QFIT AMPLITUDE=
   <USER> QFIT BEAM=
   <USER> QFIT X0Y0=
   <USER> QFIT ANGLE=
   <USER> QFIT ZERO=            
   <USER> QFIT MASK=1 1 1 1 1 1 1

       Fitted Gaussian in box: [-4 -10, 9 5] in set: n4736ave (iters:4)
   =======================================================================
   (free)  Amplitude:        356.619019 +/- 5.177855 (MJY / S)
   (free)  FWHMx:            120.598099 +/- 2.123478 (ARCSEC)
   (free)  FWHMy:            117.960548 +/- 2.053111 (ARCSEC)
   (free)  Center X0:        1.82 +/- 0.04 (grids) = 192.119869 (DEGREE)
                             = 192d  7m 11.5s = 12h 48m 28.77s
   (free)  Center Y0:        -1.87 +/- 0.04 (grids) = 41.389586 (DEGREE)
                             = 41d 23m 22.5s
   (free)  PA major axis:    1.22 +/- 25.28 (deg) wrt to North
   (free)  Zero level:       8.160992 +/- 1.876477 (MJY / S)

   No offset rotation in header, 0 is assumed.
   Grid spacing x-direction: -20.00 (ARCSEC)
   Grid spacing y-direction: 20.00 (ARCSEC)
   Level: (*,*,0.0001 METER)
   Tolerance = 0.010000, mixing factor lab = 0.010000
   =======================================================================
   <USER> QFIT BOX=
   <USER> QFIT INSET=
   <STATUS>  QFIT   +++ FINISHED +++


Updates:       Feb 3,  1992: VOG, Document created
               Apr 1,  1992: VOG, Rewritten in C
               Sep 21, 1992: VOG, New FITGAUSS2D routine implemented.
               May 19, 1993: FL,  Coordinate output improved.
               Sep 20, 1995: VOG, Minor improvements. Documentation updated.
               Feb  1, 2000: JPT, Increased number of subsets.
#<
*/


#include    "stdio.h"
#include    "stdlib.h"
#include    "string.h"
#include    "math.h"
#include    "cmain.h"
#include    "gipsyc.h"
#include    "init.h"
#include    "finis.h"
#include    "gdsinp.h"
#include    "gdsc_ndims.h"
#include    "setfblank.h"
#include    "myname.h"
#include    "anyout.h"
#include    "nelc.h"
#include    "cotrans.h"
#include    "hms.h"
#include    "dms.h"
#include    "gdsc_range.h"
#include    "gdsc_grid.h"
#include    "gdsbox.h"
#include    "gdsc_fill.h"
#include    "gdsi_read.h"
#include    "userint.h"
#include    "userlog.h"
#include    "userfio.h"
#include    "cancel.h"
#include    "gdsasn.h"
#include    "gdscss.h"
#include    "gdscpa.h"
#include    "gdsd_rdble.h"
#include    "error.h"
#include    "gdsout.h"
#include    "gdsi_write.h"
#include    "minmax3.h"
#include    "stabar.h"
#include    "wminmax.h"
#include    "gdsc_name.h"
#include    "axunit.h"
#include    "userreal.h"
#include    "userint.h"
#include    "lsqfit.h"
#include    "gauest.h"
#include    "gdsd_rchar.h"
#include    "axtype.h"
#include    "reject.h"
#include    "deputy.h"
#include    "gds_close.h"
#include    "fitgauss2d.h"



#define    AXESMAX    10
#define    SUBSMAX    2048
#define    MAXBUF     128*128                             /* Buffer size for I/O */
#define    MESSLEN    120
#define    VERSION    "1.0"
#define    NONE       0
#define    REQUEST    1
#define    HIDDEN     2
#define    EXACT      4

#define    fmake(fchr,size) { \
                            static char buff[size+1]; \
                            int i; \
                            for (i = 0; i < size; buff[i++] = ' '); \
                            buff[i] = 0; \
                            fchr.a = buff; \
                            fchr.l = size; \
                         }


/* Malloc version of 'fmake'  */
#define    finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                               fc.a[ len ] = '\0' ; \
                               fc.l = len ; }

#define    MYMAX(a,b) ( (a) > (b) ? (a) : (b) )
#define    MYMIN(a,b) ( (a) > (b) ? (b) : (a) )
#define    NINT(a)    ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define    ABS(a)     ( (a) < 0 ? (-(a)) : (a) )
#define    PI         3.141592653589793
#define    TORAD(a)   ( (a)*PI/180.0 )             /* Convert degrees to radians */
#define    TODEG(a)   ( (a)*180.0/PI )             /* Convert radians to degrees */
#define    YES        1
#define    NO         0


static fchar    setin;
static fint     subin[SUBSMAX];
static fint     nsubsI;                     /* Number of input subsets */
static fint     class = 1;
static fint     dfault;                     /* Default option for input etc */
static fchar    keyword, fmessage;
static fint     axnum[AXESMAX];
static fint     axcount[AXESMAX];
static fint     subdim = 2;
static fint     setdim;
static fint     result;
static fint     maxsubs = SUBSMAX;
static fint     maxaxes = AXESMAX;

static fint     boxlo[AXESMAX], boxhi[AXESMAX];
static fint     framelo[AXESMAX], framehi[AXESMAX];
static fint     option;
static fint     scrnum;


/* Set properties */

static double   cdelt[2];
static double   gridspac[2];
static fchar    cunit[2];
static fchar    ctype[2];
static fchar    bunit;
static double   crota;
static fint     skysys, prosys, velsys;
static fint     axistype[2];


/* read data */

static float    image[MAXBUF];


/* global fit data */

static int      i;
static float    parlist[7];
static float    errlist[7];
static fint     nitems;


/* miscellaneous */

static fint     setlevel = 0;
static float    blank;
static char     message[MESSLEN];
static fint     r1, r2;
static bool     agreed;
static fint     fitlen[2];
static double   physXY[2];
static bool     cotransok;
static bool     fromheader;
static fint     subnr;
static bool     autoview;
FILE            *fp;
static char     filename[120];
static fchar    convlon, convlat;
static fint     first = YES;
static float    clip[2];



static void anyborder( int dest, char ch, int len )
/*------------------------------------------------------------*/
/* PURPOSE: Create a string with length 'len' filled with     */
/* 'ch' characters.                                           */
/*------------------------------------------------------------*/
{
   char *str;
   str = malloc( len + 1 );   
   if (str == NULL)
   {
      anyoutf( 1, "Cannot allocate %d characters for border!", len );
      return;
   }
   memset( str, ch, len );
   str[len] = '\0';
   anyoutf( dest, str );
   free( str );
}


FILE *fopenC( char *filename )
/*------------------------------------------------------------*/
/* Open file to write data extern. The macro 'fmake' must be  */
/* available.                                                 */
/*------------------------------------------------------------*/
{
#include  "usertext.h"
#include  "userlog.h"
#include  "cancel.h"
#include  "reject.h"

#define   FILENAMELEN     80
#define   KEY_FILENAME    tofchar("FILENAME=")
#define   MES_FILENAME    tofchar("Name of output ASCII file:     [No output to file]")
#define   KEY_APPEND      tofchar("APPEND=")
#define   MES_APPEND      tofchar("File exists, ok to append?    [Y]/N")

#define   YES   1
#define   NO    0

   fchar    Filename;
   bool     append;
   fint     request = 1;
   fint     hidden  = 2;
   fint     dfault;
   fint     n;
   fint     nitems;
   fint     agreed;
   FILE     *fp;


   dfault = request;
   fmake( Filename, FILENAMELEN );
   do 
   {
      append = toflog(YES);                               /* Default APPEND=Y */
      n = usertext_c( Filename,
                      &dfault,
                      KEY_FILENAME,
                      MES_FILENAME );
      if (n == 0) 
         return NULL;

      strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */

      fp = fopen(filename, "r");
      if (fp != NULL)                                     /* The file exists */
      {
         nitems = 1;
         n = userlog_c( &append,
                        &nitems,
                        &dfault,
                        KEY_APPEND,
                        MES_APPEND );
         append = tobool( append );
         fclose( fp );
         cancel_c( KEY_APPEND );
      }
      if (!append) 
      {
          cancel_c( KEY_FILENAME );
          agreed = NO;
      }
      else 
      {
         fp = fopen(filename, "a");
         agreed = (fp != NULL);
         if (!agreed) 
         {
            reject_c( KEY_FILENAME,
                      tofchar("Cannot open, try another!") );
         }
      }
   } 
   while (!agreed);
   return fp;
}






static void showcoord( fchar Fsetin, 
                       fint *Fsubin, 
                       fint *Faxnum,
                       fint *Fsubdim, 
                       fint *Fsetdim, 
                       fchar Fshowstr )
/*------------------------------------------------------------*/
/* Create the string 'Fshowstr' containing information about  */
/* the axes. Example: Fshowstr = "(*,29.5 DEGREE,200 KM/S)".  */
/* This string can be appended to the string obtained after   */
/* a call to 'showsubset". The definitions for BIGSTORE and   */
/* AXESMAX must be available.                                 */
/*------------------------------------------------------------*/
{
   int    n;
   fchar  Fcunit;
   char   cunitbuf[20+1];
   fint   Ferr = 0;
   fint   Fres;
   fint   Fgrid;
   char   dummystr[80];
   char   rightbuf[80];

   /* Coordinate transformation */

   fint     Fdirect;             /* grid coord. -> physical coord. */
   double   coordin[AXESMAX];    /* Grids before transformation */
   double   coordout[AXESMAX];   /* Physical coordinates after transformation */


   sprintf( rightbuf, "%c", '(' );
   for (n = 0; n < *Fsetdim; n++ ) 
   {
      if (n >= *Fsubdim) 
      {
         Fgrid = gdsc_grid_c( Fsetin, &Faxnum[n], Fsubin, &Ferr );
         coordin[ (int) Faxnum[n]-1 ] = (double) Fgrid;
      }
      else 
      {
         coordin[ (int) Faxnum[n]-1 ] = 0.0;
      }
   }
   Fdirect = 1;                             /* grid coord. -> physical coord. */
   Fres = cotrans_c( Fsetin, Fsubin, coordin, coordout, &Fdirect );
   for (n = 0; n < *Fsetdim; n++ ) 
   {
      if (n >= *Fsubdim) 
      {
         Fcunit.a = cunitbuf; Fcunit.l = 20; cunitbuf[20] = '\0';
         Fres = axunit_c( Fsetin, &Faxnum[n], Fcunit );
         sprintf( dummystr, "%.6g %.*s", coordout[ (int) Faxnum[n]-1 ],
                  (int) nelc_c( Fcunit ), Fcunit.a );
      }
      else 
      {
         sprintf( dummystr, "%c", '*' );
      }
      if (( n + 1 ) == *Fsetdim) 
      {
         sprintf( rightbuf, "%.*s%s", strlen(rightbuf), rightbuf, dummystr );
      }
      else  /* Add comma */
      { 
         sprintf( rightbuf, "%.*s%s,", strlen(rightbuf), rightbuf, dummystr );
      }
   }
   sprintf( Fshowstr.a, "%s)", rightbuf );
}




static float toangle( float Angle, float Maxangle )
/*---------------------------------------------------*/
/* Return angle between 0 and < 180.'Maxangle'       */
/*---------------------------------------------------*/
{
   while (Angle < 0.0) Angle += Maxangle;
   while (Angle > Maxangle) Angle -= Maxangle;
   return Angle;
}



void errormess( int result )
{
   switch (result) {
      case -1 : {
         anyoutf( 1, "Too many free parameters in fit" );
      } break;
      case -2 : {
         anyoutf( 1, "No free parameters in fit" );
      } break;
      case -3 : {
         anyoutf( 1, "Not enough degrees of freedom" );
      } break;
      case -4 : {
         anyoutf( 1, "Max number of iterations too small" );
      } break;
      case -5 : {
         anyoutf( 1, "Diagonal of matrix contains zeros" );
      } break;
      case -6 : {
         anyoutf( 1, "Determinant of coeff. matrix is zero" );
      } break;
      case -7 : {
         anyoutf( 1, "Square root of negative number" );
      } break;
   }
}


float func_c( float *xdat, float *parlist, fint *npar, fint *fopt )
/*----------------------------------------------------------------------
 PURPOSE: Calculate the value of a gaussian with parameters P at the
          position Xdat.
          The parameters are:
          parlist(0) : Amplitude
          parlist(1) : Axis (FWHM) in X-direction
          parlist(2) : Axis (FWHM) in Y-direction
          parlist(3) : X0, center of gauss wrt center of
                       subset
          parlist(4) : Y0, center of gauss wrt center of
                       subset
          parlist(5) : Rotation angle in radians wrt. positive X-axis
                       counted anti-clockwise
          parlist(6) : Zero level
          Units center and FWHM are the same

          The 2d-gaussian is:

          F(x,y) = par(0) * EXP( -4.0*ALOG(2.0) *
                   [(xr / par(1))**2 + (yr / par(2))**2] + par(6)

               where: xr =  xo * cos(par(5)) + yo * sin(par(5))
                      yr = -xo * sin(par(5)) + yo * cos(par(5))

               and:   xo = x - par(3)
                      yo = y - par(4)

----------------------------------------------------------------------*/
{
      double     xd, yd;            /* FWHM's of  ellipse */
      double     x, y;              /* Position */
      double     sinpa, cospa;      /* Sine, cosine of position angle */
      double     argXD, argYD;      /* Arguments in the exponent */


      xd = fabs((double) parlist[1]);
      yd = fabs((double) parlist[2]);
      x   = (double)xdat[0] - (double)parlist[3];
      y   = (double)xdat[1] - (double)parlist[4];
      cospa = cos( parlist[5] );
      sinpa = sin( parlist[5] );
      /* What are the values of x,y in an unrotated frame?       */
      argXD =   x * cospa + y * sinpa;
      argYD =  -x * sinpa + y * cospa;
      return ( (float)
             (
               (double)parlist[0] * exp( -4.0*log(2.0) *
               ( (argXD/xd)*(argXD/xd) + (argYD/yd)*(argYD/yd) ) )
               + (double)parlist[6]
             )
             );
}



void derv_c( float *xdat, float *fpar, float *dervs,
             fint *npar, fint *fopt )
/*----------------------------------------------------------------------
 PURPOSE: Calculate the partial derivatives wrt. the parameters for a
          gaussian with parameters 'parlist' at position Xdat
          The parameters are:
          parlist(0) : Amplitude
          parlist(1) : Axis (FWHM) in X-direction
          parlist(2) : Axis (FWHM) in Y-direction
          parlist(3) : X0, center of gauss wrt center of
                       subset
          parlist(4) : Y0, center of gauss wrt center of
                       subset
          parlist(5) : Rotation angle in radians wrt. positive X-axis
                       counted anti-clockwise
          parlist(6) : Zero level
          Units center and FWHM are the same
----------------------------------------------------------------------*/
{
      /* Major and minor axis */
      double                XD, YD;
      /* Position */
      double                x, y;
      /* Sine, cosine of rotation angle */
      double                sinpa, cospa;
      /* Arguments in the exponent */
      double                argXD, argYD;
      double                expon;


      /* Positive widths  */
      XD = fabs((double) parlist[1]);
      YD = fabs((double) parlist[2]);
      /* Offset from position peak       */
      x = (double)xdat[0] - (double)parlist[3];
      y = (double)xdat[1] - (double)parlist[4];
      cospa = cos( (double)parlist[5] );
      sinpa = sin( (double)parlist[5] );
      argXD =   x * cospa + y * sinpa;
      argYD =  -x * sinpa + y * cospa;

      /* Determine the derivatives: */

      expon    =  -4.0*log(2.0) *
                  ( (argXD/XD)*(argXD/XD) + (argYD/YD)*(argYD/YD) );

      expon    = exp( expon );

      /* Partial derivative amplitude */
      dervs[0] = (float) expon;
      /* Calculate A.exp(-arg)        */
      expon    = (double) parlist[0] * expon;

      /* Partial derivative fwhm x */
      dervs[1] = (float) (
                 expon * 8.0 * log(2.0) * argXD*argXD / (XD*XD*XD)
                 );

      /* Partial derivative fwhm y axis */
      dervs[2] = (float) (
                 expon * 8.0 * log(2.0) * argYD*argYD / (YD*YD*YD)
                 );

      /* Partial derivative x-position*/
      dervs[3] = (float) (
                 expon * -8.0*log(2.0) *
                 ( -argXD*cospa/(XD*XD) + argYD*sinpa/(YD*YD) )
                 );
      /* Partial derivative y-position */
      dervs[4] = (float) (
                 expon * -8.0 * log(2.0) *
                 ( -argXD*sinpa/(XD*XD) - argYD*cospa/(YD*YD) )
                 );

      /* Partial derivative rotation angle*/
      dervs[5] = (float) (
                 expon * -8.0 * log(2.0) * argYD * argXD *
                 ( 1.0/(XD*XD) - 1.0/(YD*YD) )
                 );

      /* Partial derivative zero level     */
      dervs[6] = 1.0;
}



static void fitgauss( fint  *boxlo, 
                      fint  *boxhi, 
                      float *parlist,
                      float *errlist, 
                      fint  subnr )
/*-----------------------------------------------------*/
/* Fit a 2-dim gauss (see func_c) to data in input box */
/*-----------------------------------------------------*/
{
   fint     mpar[7];
   float    tol;
   fint     xdim;
   fint     its;
   float    lab;
   fint     iters;
   int      i;
   int      slen;

   /* read data */

   fint     tid;
   fint     totpixels;
   fint     pixelsdone;
   fint     cwlo, cwhi;

   fchar    showstr;


   cwlo = gdsc_fill_c( setin, &subin[subnr], boxlo );
   cwhi = gdsc_fill_c( setin, &subin[subnr], boxhi );
   totpixels = fitlen[0] * fitlen[1];

   tid = 0;
   gdsi_read_c( setin,
                &cwlo, &cwhi,
                image,
                &totpixels,
                &pixelsdone,
                &tid );



   /*--------------------------------------------------*/
   /* Put all data in arrays with positions, values    */
   /* and weights suitable for the LSQFIT function:    */
   /*--------------------------------------------------*/
   nitems  = 7;
   for( i = 0; i < nitems; i++ )
      mpar[i] = 1;
   dfault = REQUEST;
   do
   {
      result = userint_c(  mpar,
                           &nitems,
                           &dfault,
                           tofchar("MASK="),
                           tofchar("Mask (0=fixed,1=free):     [parameter list]"));
      if (result == 0)
      {
         slen = sprintf( message, 
                     "=================== PARAMETERS =====================" );
         anyoutf( 1, message );
         anyoutf( 1, "   1: Amplitude");
         anyoutf( 1, "   2: Axis (FWHM) in X-direction");
         anyoutf( 1, "   3: Axis (FWHM) in Y-direction");
         anyoutf( 1, "   4: X0, horizontal center of gauss");
         anyoutf( 1, "   5: Y0, vertical center of gauss");
         anyoutf( 1, "   6: Rotation angle");
         anyoutf( 1, "   7: Zero level");
         anyoutf( 1, " ");
         anyoutf( 1, "Use 0 or 1 for each parameter: 0=fixed, 1=free");
         anyborder( 1, '=', slen );
         anyoutf( 1, " ");         
         cancel_c( tofchar("MASK=") );
      }
   } while (!result);

   slen = sprintf( message,
                  "======================= YOUR CHOICE ============================");
   if (!mpar[0])
      anyoutf( 1, "   1:(FIXED) Amplitude fixed to AMPLITUDE=");
   else
      anyoutf( 1, "   1:(FREE)	Amplitude");
   if (!mpar[1])
      anyoutf( 1, "   2:(FIXED) Axis (FWHM) in X-direction fixed to BEAM=");
   else
      anyoutf( 1, "   2:(FREE)  Axis (FWHM) in X-direction");
   if (!mpar[2])
      anyoutf( 1, "   3:(FIXED) Axis (FWHM) in Y-direction fixed to BEAM=");
   else
      anyoutf( 1, "   3:(FREE)  Axis (FWHM) in Y-direction");;
   if (!mpar[3])
      anyoutf( 1, "   4:(FIXED) X0, horizontal center of gauss fixed to X0Y0=");
   else
      anyoutf( 1, "   4:(FREE)  X0, horizontal center of gauss");
   if (!mpar[4])
      anyoutf( 1, "   5:(FIXED) Y0, vertical center of gauss fixed to X0Y0=");
   else
      anyoutf( 1, "   5:(FREE)  Y0, vertical center of gauss");
   if (!mpar[5])
      anyoutf( 1, "   6:(FIXED) Rotation angle fixed to ANGLE=");
   else
      anyoutf( 1, "   6:(FREE)  Rotation angle");
   if (!mpar[6])
      anyoutf( 1, "   7:(FIXED) Zero level fixed to ZERO=");
   else
      anyoutf( 1, "   7:(FREE)  Zero level");
   anyborder( 1, '=', slen );
   anyoutf( 1, " ");

   xdim     = 2;
   its      = 100;
   tol      = 0.01;                                    /* Get the tolerance */
   dfault   = HIDDEN;
   keyword  = tofchar("TOLERANCE=");
   fmessage = tofchar("Tolerance in fit:               [0.01] ");
   nitems   = 1;
   result   = userreal_c( &tol,
                          &nitems,
                          &dfault,
                          keyword,
                          fmessage );
   lab      = 0.01;
   dfault   = HIDDEN;
   keyword  = tofchar("LAB=");
   fmessage = tofchar("Mixing parameter:              [0.01] ");
   nitems   = 1;
   result   = userreal_c( &lab,
                          &nitems,
                          &dfault,
                          keyword,
                          fmessage );

   nitems   = 7;
   its      = 100;
   for( i = 0; i < nitems; i++ ) 
      parlist[i] = blank;

   gridspac[0] = fabs(cdelt[0]);
   gridspac[1] = fabs(cdelt[1]);

   dfault   = REQUEST;
   keyword  = tofchar( "AMPLITUDE=" );
   fmessage = tofchar( "Amplitude (map units):    [Calculated]");
   nitems   = 1;
   result   = userreal_c( &parlist[0], &nitems, &dfault, keyword, fmessage );

   keyword  = tofchar( "BEAM=" );
   if (strstr( cunit[0].a, "DEGREE" ) && strstr( cunit[1].a, "DEGREE" )) 
   {
      fmessage = tofchar( "Beam, fwhm in x & y (arcsec):        [Calculated]");
   } 
   else 
   {
      (void) sprintf( message, "Beam, fwhm in x & y (%.*s x %.*s)",
               nelc_c(cunit[0]), cunit[0].a, nelc_c(cunit[1]), cunit[1].a );
      fmessage = tofchar( message );
   }
   nitems   = 2;
   result   = userreal_c( &parlist[1], &nitems, &dfault, keyword, fmessage );
   if (strstr( cunit[0].a, "DEGREE" ) && strstr( cunit[1].a, "DEGREE" )) 
   {
      if (parlist[1] != blank) parlist[1] /= 3600.0;
      if (parlist[2] != blank) parlist[2] /= 3600.0;
   }

   keyword  = tofchar( "X0Y0=" );
   fmessage = tofchar( "Centre in grids:        [Calculated]");
   nitems   = 2;
   result   = userreal_c( &parlist[3], &nitems, &dfault, keyword, fmessage );
   if (parlist[3] != blank) parlist[3] = (parlist[3] - boxlo[0]) * gridspac[0];
   if (parlist[4] != blank) parlist[4] = (parlist[4] - boxlo[1]) * gridspac[1];

   keyword  = tofchar( "ANGLE=" );
   fmessage = tofchar( "Angle of FWHM in x wrt. pos. X-axis (deg):  [Calculated]");
   nitems   = 1;
   result   = userreal_c( &parlist[5], &nitems, &dfault, keyword, fmessage );
   if (parlist[5] != blank) parlist[5] = TORAD(parlist[5]);

   keyword  = tofchar( "ZERO=" );
   fmessage = tofchar( "Zero level (map units):    [Calculated]");
   nitems   = 1;
   result   = userreal_c( &parlist[6], &nitems, &dfault, keyword, fmessage );

   clip[0]  = blank;
   clip[1]  = blank;
   dfault   = HIDDEN;
   nitems   = 2;
   keyword  = tofchar( "CLIPLOHI=" );
   fmessage = tofchar("Give clip levels:     [No clip levels]");
   result   = userreal_c( clip, &nitems, &dfault, keyword, fmessage );

   iters = fitgauss2d_c(  image,
                          &fitlen[0],
                          &fitlen[1],
                          &gridspac[0],
                          &gridspac[1],
                          parlist,
                          errlist,
                          mpar,
                          &tol,
                          &its,
                          &lab,
                          clip );
   if (iters < 0) 
   {
      switch( (int) iters ) 
      {
      case -1:
         anyoutf( 1, "LSQFIT: Too many free parameters, maximum is 32.");
         break;
      case -2:
         anyoutf( 1, "LSQFIT: No free parameters. ");
         break;
      case -3:
         anyoutf( 1, "LSQFIT: Not enough degrees of freedom.");
         break;
      case -4:
         anyoutf( 1, "LSQFIT: Maximum number of iterations too small to obtain a solution " );
         anyoutf( 1, "        which satisfies TOL.");
         break;
      case -5:
         anyoutf( 1, "LSQFIT: Diagonal of matrix contains elements which are zero.");
         break;
      case -6:
         anyoutf( 1, "LSQFIT: Determinant of the coefficient matrix is zero.");
         break;
      case -7:
         anyoutf( 1, "LSQFIT: Square root of negative number.");
         break;
      case -8:
         anyoutf( 1, "FITGAUSS2D: Box too big.");
         break;
      case -9:
         anyoutf( 1, "FITGAUSS2D: Box contains only blanks.");
         break;
      case -10:
         anyoutf( 1, "FITGAUSS2D: Box length == 1 in x or y direction.");
      }
      anyoutf( 1, "No fit ..." );
   } 
   else 
   {
      /*  Write the results to screen and logfile: */
      anyoutf( 3, " " );
      sprintf( message,
               "    Fitted Gaussian in box: [%d %d, %d %d] in set: %.*s (iters:%d)",
               boxlo[0], boxlo[1], boxhi[0], boxhi[1],
               nelc_c( setin ), setin.a, iters );
      anyoutf( 3, message );
      slen = 76;
      anyborder( 3, '=', slen );

      if (mpar[0])
      {
                         /*"+++++++_++++++++++++++++++%*/
         sprintf( message, "(free)  Amplitude:        %f +/- %f (%.*s)",
                  parlist[0],
                  errlist[0],
                  nelc_c( bunit ),
                  bunit.a );
      } 
      else 
      {
                         /*"+++++++_++++++++++++++++++%*/
         sprintf( message, "(fixed) Amplitude:        %f (%.*s)",
                  parlist[0],
                  nelc_c( bunit ),
                  bunit.a );
      }
      anyoutf( 3, message );
      if (strstr( cunit[0].a, "DEGREE" )) 
      {
         parlist[1] *= 3600.0;
         errlist[1] *= 3600.0;
         if (mpar[1]) 
         {
            if (parlist[1] < 0.0) 
            {
                               /*"+++++++_++++++++++++++++++%*/
               sprintf( message, "(free)  FWHMx:(suspicious)%f +/- %f (ARCSEC)",
                        parlist[1], errlist[1] );
            } 
            else 
            {
               if (parlist[1] > 3600.0*parlist[2]) 
               {
                                  /*"+++++++_++++++++++++++++++%*/
                  sprintf( message, "(free)  FWHMx (Major):    %f +/- %f (ARCSEC)",
                           parlist[1], errlist[1] );
               } 
               else 
               {
                                  /*"+++++++_++++++++++++++++++%*/
                  sprintf( message, "(free)  FWHMx (Minor):    %f +/- %f (ARCSEC)",
                           parlist[1], errlist[1] );
               }
            }
         } 
         else 
         {
            if (parlist[1] > 3600.0*parlist[2]) 
            {
                               /*"+++++++_++++++++++++++++++%*/
               sprintf( message, "(fixed) FWHMx (Major):    %f (ARCSEC)", parlist[1] );
            } 
            else 
            {
                               /*"+++++++_++++++++++++++++++%*/
               sprintf( message, "(fixed) FWHMx (Minor):    %f (ARCSEC)", parlist[1] );
            }
         }
      } 
      else 
      {
         if (mpar[1]) 
         {
            if (parlist[1] < 0.0) 
            {
                               /*"+++++++_++++++++++++++++++%*/
               sprintf( message, "(free)  FWHMx:(suspicious)%f +/- %f (%.*s)",
                        parlist[1],
                        errlist[1],
                        nelc_c( cunit[0] ),
                        cunit[0].a );
            } 
            else 
            {
               if (parlist[1] > parlist[2]) 
               {
                                  /*"+++++++_++++++++++++++++++%*/
                  sprintf( message, "(free)  FWHMx (Major):    %f +/- %f (%.*s)",
                           parlist[1],
                           errlist[1],
                           nelc_c( cunit[0] ),
                           cunit[0].a );
               } 
               else 
               {
                                  /*"+++++++_++++++++++++++++++%*/
                  sprintf( message, "(free)  FWHMx (Minor):    %f +/- %f (%.*s)",
                           parlist[1],
                           errlist[1],
                           nelc_c( cunit[0] ),
                           cunit[0].a );
               }
            }
         } 
         else 
         {
                            /*"+++++++_++++++++++++++++++%*/
            sprintf( message, "(fixed) FWHMx:            %f (%.*s)",
                     parlist[1],
                     nelc_c( cunit[0] ),
                     cunit[0].a );
         }
      }
      anyoutf( 3, message );
      if (strstr( cunit[1].a, "DEGREE" )) 
      {
         parlist[2] *= 3600.0;
         errlist[2] *= 3600.0;
         if (mpar[2]) 
         {
            if (parlist[2] < 0.0) 
            {
                               /*"+++++++_++++++++++++++++++%*/
               sprintf( message, "(free)  FWHMy:(suspicious)%f +/- %f (ARCSEC)",
                        parlist[2], errlist[2] );
            } 
            else 
            {
                               /*"+++++++_++++++++++++++++++%*/
               sprintf( message, "(free)  FWHMy:            %f +/- %f (ARCSEC)",
                        parlist[2], errlist[2] );
            }
         } 
         else 
         {
                            /*"+++++++_++++++++++++++++++%*/
            sprintf( message, "(fixed) FWHMy:            %f (ARCSEC)", parlist[2] );
         }
      } 
      else 
      {
         if (mpar[2]) 
         {
            if (parlist[2] < 0.0) 
            {
                               /*"+++++++_++++++++++++++++++%*/
               sprintf( message, "(free)  FWHMy:(suspicious)%f +/- %f (%.*s)",
                        parlist[2],
                        errlist[2],
                        nelc_c( cunit[1] ),
                        cunit[1].a );
            } 
            else 
            {
                               /*"+++++++_++++++++++++++++++%*/
               sprintf( message, "(free)  FWHMy:            %f +/- %f (%.*s)",
                        parlist[2],
                        errlist[2],
                        nelc_c( cunit[1] ),
                        cunit[1].a );
            }
         } 
         else 
         {
                            /*"+++++++_++++++++++++++++++%*/
            sprintf( message, "(fixed) FWHMy:            %f (%.*s)",
                     parlist[2],
                     nelc_c( cunit[1] ),
                     cunit[1].a );
         }
      }
      anyoutf( 3, message );
      parlist[3] /= fabs(gridspac[0]);
      errlist[3] /= fabs(gridspac[0]);
      parlist[4] /= fabs(gridspac[1]);
      errlist[4] /= fabs(gridspac[1]);
      parlist[3] += boxlo[0];
      parlist[4] += boxlo[1];
      {
         fint     grid2phys = 1;
         double   phys[AXESMAX];
         double   grids[AXESMAX];
         grids[0]  = (double) parlist[3];
         grids[1]  = (double) parlist[4];
         r1        = cotrans_c( setin, &subin[subnr], grids, phys, &grid2phys );
         if (r1 == 0) 
         {
            cotransok = YES;
            physXY[0] = phys[axnum[0]-1];
            physXY[1] = phys[axnum[1]-1];
         } 
         else 
         {
            cotransok = NO;
            physXY[0] = grids[0];
            physXY[1] = grids[1];
         }
      }
      if (mpar[3]) 
      {
                         /*"+++++++_++++++++++++++++++%*/
         sprintf( message, "(free)  Center X0:        %.2f +/- %.2f (grids)",
                  parlist[3],
                  errlist[3] );
      } 
      else 
      {
                         /*"+++++++_++++++++++++++++++%*/
         sprintf( message, "(fixed) Center X0:        %.2f (grids)", parlist[3] );
      }
      if (cotransok) 
      {
         sprintf( message, "%.*s = %f (%.*s)",
                  strlen( message ), message,
                  physXY[0],
                  nelc_c( cunit[0] ), cunit[0].a );
      }
      anyoutf( 3, message );
      fmake( convlon, 40 );
      fmake( convlat, 40 );
      strcpy( convlon.a, "hms?" );
      strcpy( convlat.a, "dms?" );
      if (axistype[0] == 1) 
      {
         /* spatial axis longitude */
         if (skysys == 1) 
         {
            /* equatorial */
            fint   output = 0, prec = 1;
            hms_c( &physXY[0], convlon, NULL, &prec, &output );
                   /*"+++++++_++++++++++++++++++%*/
            (void) sprintf( message, "                          = %.*s",
                   nelc_c( convlon ), convlon.a );
         }
         else 
         {
            /* not equatorial */
            fint   output = 0, prec = 1;
            dms_c( &physXY[0], convlon, NULL, &prec, &output );
                   /*"+++++++_++++++++++++++++++%*/
            sprintf( message, "                          = %.*s",
                     nelc_c( convlon ), convlon.a );
         }
      }
      if (axistype[0] == 2) 
      {
         /* spatial axis latitude */
         fint   output = 0, prec = 1;
         dms_c( &physXY[0], convlat, NULL, &prec, &output );
               /*"+++++++_++++++++++++++++++%*/
         sprintf( message, "                          = %.*s",
                  nelc_c( convlat ), convlat.a );
      }
      anyoutf( 3, message );
      if (mpar[4]) 
      {
                         /*"+++++++_++++++++++++++++++%*/
         sprintf( message, "(free)  Center Y0:        %.2f +/- %.2f (grids)",
                  parlist[4],
                  errlist[4] );
      } 
      else 
      {
                         /*"+++++++_++++++++++++++++++%*/
         sprintf( message, "(fixed) Center Y0:        %.2f (grids)", parlist[4] );
      }
      if (cotransok) 
      {
         sprintf( message, "%.*s = %f (%.*s)",
                  strlen( message ), message,
                  physXY[1],
                  nelc_c( cunit[1] ), cunit[1].a );
      }
      anyoutf( 3, message );
      if (axistype[1] == 1) 
      {
         /* spatial axis longitude */
         if (skysys == 1) 
         {
            /* equatorial */
            fint   output = 0, prec = 1;
            hms_c( &physXY[1], convlon, NULL, &prec, &output );
                   /*"+++++++_++++++++++++++++++%*/
            (void) sprintf( message, "                          = %.*s",
                     nelc_c( convlon ), convlon.a );
         }
         else 
         {
            /* not equatorial */
            fint   output = 0, prec = 1;
            dms_c( &physXY[1], convlon, NULL, &prec, &output );
                   /*"+++++++_++++++++++++++++++%*/
            sprintf( message, "                          = %.*s",
                     nelc_c( convlon ), convlon.a );
         }
      }
      if (axistype[1] == 2) 
      {
         /* spatial axis latitude */
         fint   output = 0, prec = 1;
         dms_c( &physXY[1], convlat, NULL, &prec, &output );
                /*"+++++++_++++++++++++++++++%*/
         sprintf( message, "                          = %.*s",
                  nelc_c( convlat ), convlat.a );
      }
      anyoutf( 3, message );
      /* Correct for rotation direction */
      parlist[5] = TODEG(parlist[5]);
      if (parlist[1] > parlist[2]) 
      {
         /* FWHMx > FWHMy */
         parlist[5] -= 90.0;
      }
      parlist[5] += crota;       /* Correct for rotation angle from header */
      parlist[5] = toangle( parlist[5], 180.0 );
      if (mpar[5]) 
      {
         errlist[5] = TODEG(errlist[5]);
         sprintf( message,
               /*"+++++++_++++++++++++++++++%*/
                 "(free)  PA major axis:    %.2f +/- %.2f (deg) wrt to North",
                  parlist[5],
                  errlist[5] );
      } 
      else 
      {
         sprintf( message,
               /*"+++++++_++++++++++++++++++%*/
                 "(fixed) PA major axis:    %f (deg)  wrt to North",
                  parlist[5] );
      }
      anyoutf( 3, message );
      if (mpar[6]) {
                         /*"+++++++_++++++++++++++++++%*/
         sprintf( message, "(free)  Zero level:       %f +/- %f (%.*s)",
                  parlist[6],
                  errlist[6],
                  nelc_c( bunit ),
                  bunit.a );
      } 
      else 
      {
                         /*"+++++++_++++++++++++++++++%*/
         sprintf( message, "(fixed) Zero level:       %f (%.*s)",
                  parlist[6],
                  nelc_c( bunit ),
                  bunit.a );
      }
      anyoutf( 3, message );
      anyoutf( 3, " " );
      if (fromheader) 
      {
         sprintf( message, "PA latitude axis from header: %.2f deg.", crota );
         anyoutf( 3, message );
      } 
      else 
      {
         anyoutf( 3, "No offset rotation in header, 0 is assumed." );
      }
      if (strstr( cunit[0].a, "DEGREE" )) 
      {
         sprintf( message, "Grid spacing x-direction: %.2f (ARCSEC)",
                  cdelt[0]*3600.0 );
      } 
      else 
      {
         sprintf( message, "Grid spacing x-direction: %.2f (%.*s)",
                  cdelt[0],
                  nelc_c( cunit[0] ),
                  cunit[0].a );
      }
      anyoutf( 3, message );
      if (strstr( cunit[1].a, "DEGREE" )) 
      {
         sprintf( message, "Grid spacing y-direction: %.2f (ARCSEC)",
                  cdelt[1]*3600.0 );
      } 
      else 
      {
         sprintf( message, "Grid spacing y-direction: %.2f (%.*s)",
                  cdelt[1],
                  nelc_c( cunit[1] ),
                  cunit[1].a );
      }
      anyoutf( 3, message );
      fmake( showstr, 80 );
      if (setdim > subdim ) 
      {
         showcoord( setin, &subin[subnr], axnum,
                    &subdim, &setdim, showstr );
         (void) sprintf( message, "Level: %.*s",
                         nelc_c( showstr ), showstr.a );
         anyoutf( 3, message );
      }
      sprintf( message, "Tolerance = %f, mixing factor lab = %f", tol, lab );
      anyoutf( 3, message );
      if (clip[0] != blank) 
      {
         if (clip[1] == blank) 
         {
            (void) sprintf( message, "Clip level: %f", clip[0] );
         } 
         else 
         {
            (void) sprintf( message, "Clip levels: %f %f", clip[0], clip[1] );
         }
      } 
      else 
      {
         (void) sprintf( message, "No clip levels selected.");
      }
      anyoutf( 3, message );
      anyborder( 3, '=', slen );

      /* Is output to an ASCII file requested? */
      if (fp != NULL) 
      {
         float major, minor, majerr, minerr;
         fint  subgrid;
         char  subtxt[10];
         if (parlist[1] > parlist[2]) 
         {
            major  = parlist[1]; minor  = parlist[2];
            majerr = errlist[1]; minerr = errlist[2];
         } 
         else 
         {
            major  = parlist[2]; minor  = parlist[1];
            majerr = errlist[2]; minerr = errlist[1];
         }
         if (setdim == subdim) {
            strcpy( subtxt, "Top" );
         } 
         else 
         {
            r1 = 0;
            subgrid = gdsc_grid_c( setin, &axnum[setdim-1], &subin[subnr], &r1 );
            if (r1 >= 0) 
            {
               (void) sprintf( subtxt, "%d", subgrid );
            } 
            else 
            {
               (void) strcpy( subtxt, "?" );
            }
         }
         if (first) 
         {
            /* Print a header */
            first = NO;
            if( skysys == 1 )
            {
               (void) fprintf( fp,
/*------------#---#-------#+-#------#-------#+-#-----#--------------#--------------#------#+-#---#------#+-#---#-----#+-#---#--------#--------#----#----#----#----#--*/
 "Setname      sub         X0                Y0             RA             DEC            Major         Minor      Pos. angle    Ampl.  Zero lev.       Box         itr.\n");
               (void) fprintf( fp,
 "============================================================================================================================================================\n");
            }
            else {
               (void) fprintf( fp,
/*------------#---#-------#+-#------#-------#+-#------#--------------#--------------#------#+-#---#------#+-#---#-----#+-#---#--------#--------#----#----#----#----#--*/
 "Setname      sub         X0                Y0             lon            lat            Major         Minor      Pos. angle    Ampl.  Zero lev.       Box         itr.\n");
               (void) fprintf( fp,
 "============================================================================================================================================================\n");
            }
         }

         (void) fprintf( fp,
"%-12.12s %3.3s %7.1g +- %6.1g %7.1g +- %6.1g %14.14s %14.14s %6.1f +- %3.1f %6.1f +- %3.1f %5.1f +- %3.1f %8g %8g %4d %4d %4d %4d %2d\n",
                  setin.a,
                  subtxt,
                  parlist[3], errlist[3],
                  parlist[4], errlist[4],
                  convlon.a, convlat.a,
                  major, majerr,
                  minor, minerr,
                  parlist[5], errlist[5],
                  parlist[0],
                  parlist[6],
                  boxlo[0], boxlo[1], boxhi[0], boxhi[1],
                  iters
                );
      }
   } /* End display if fit was found */
} /* End of fitgauss */



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------*/
/* QFIT main.                                                  */
/*-------------------------------------------------------------*/
{
   bool    quit;
   
   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      fchar    Ftask;                      /* Name of current task */
      fmake( Ftask, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Ftask );                   /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';       /* Terminate task name with null */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }

   setfblank_c( &blank );
   
   fp = fopenC( filename );

   do 
   {
      autoview = toflog( NO );
      dfault   = REQUEST;
      nitems   = 1;
      r1 = userlog_c( &autoview,
                      &nitems,
                      &dfault,
                      tofchar("AUTOVIEW="),
                      tofchar("Do you want to display input set in GIDS?    Y/[N]") );
      autoview = tobool( autoview );

      fmake(setin, 80);
      keyword  = tofchar("INSET=");
      fmessage = tofchar("Give set (, subsets):    [STOP Program]");
      dfault   = REQUEST;
      subdim   = 2;
      scrnum   = 3;
      nsubsI   = gdsinp_c( setin,
                           subin,
                           &maxsubs,
                           &dfault,
                           keyword,
                           fmessage,
                           &scrnum,
                           axnum,
                           axcount,
                           &maxaxes,
                           &class,
                           &subdim );
      if (nsubsI == 0) 
         /* Abort program because <CR> was pressed */
         quit = YES;
      else 
         quit = NO;


      if (!quit)
      {
         bool   exitloop = NO;         
         /* Continue program if setname was given */
         setdim  = gdsc_ndims_c( setin, &setlevel );
   
        /*
         *------------------------------------------------------
         * Determine the edges of this its frame (FgridLO/HI)
         *------------------------------------------------------
         */
         {
            fint cwloI, cwhiI;
            r1 = 0;
            gdsc_range_c( setin, &setlevel, &cwloI, &cwhiI, &r1 );
            r1 = r2 = 0;
            for (i = 0; i < subdim; i++) {
                framelo[i] = gdsc_grid_c( setin, &axnum[i], &cwloI, &r1 );
                framehi[i] = gdsc_grid_c( setin, &axnum[i], &cwhiI, &r2 );
            }
         }
   
   
   
         /* Get grid spacings from header */
   
         for (i = 0; i < subdim; i++)                     /* Append number to name */
         {
            result = sprintf( message, "CDELT%d", axnum[i] );
            r1 = 0;
            /* Get the pixel separation of the axes */
            gdsd_rdble_c( setin, tofchar(message), &setlevel, &cdelt[i], &r1 );
            if (r1 < 0)  
               errorf( 4, "No grid spacings in header of this set!" );
   
            result = sprintf( message, "CUNIT%d", axnum[i] );
            r1 = 0;
            /* Get the units of the axes */
            finit( cunit[i], 40 );
            gdsd_rchar_c( setin, tofchar(message), &setlevel, cunit[i], &r1 );
            if (r1 < 0) 
               strcpy( cunit[i].a, "?" );
   
            finit( ctype[i], 40 );
            r1 = 0;
            (void) sprintf( message, "CTYPE%d", axnum[i] );
            gdsd_rchar_c( setin, tofchar(message), &setlevel, ctype[i], &r1 );
            if (r1 < 0) 
            {
               (void) strcpy( ctype[i].a, "PIXELS" );
               axistype[i] = 0;
            }
            else 
            {
               fchar   dunit, nunit;
               fmake(  dunit, 20 );
               fmake(  nunit, 20 );
               axistype[i] = axtype_c( ctype[i],
                                       nunit,                      /* Natural units */
                                       dunit,
                                       &skysys,
                                       &prosys,
                                       &velsys );
            }
         }
         r1 = 0;
         finit( bunit, 40 );
         gdsd_rchar_c( setin, tofchar("BUNIT"), &setlevel, bunit, &r1 );
         if (r1 < 0) 
            strcpy( bunit.a, "units ?" ); bunit.l = 7;
   
   
         /* Get rotation angle */
   
         for (i = 0; i < setdim; i++)                      /* Append number to name */
         {
            result = sprintf( message, "CROTA%d", axnum[i] );
            r1 = 0;
            gdsd_rdble_c( setin, tofchar(message), &setlevel, &crota, &r1 );
            if (r1 < 0) 
            {
               fromheader = NO;
               crota = 0.0;
            } 
            else 
            {
               if (crota != 0.0) 
               {
                  fromheader = YES;
                  break;
               }
            }
         }
   
         if (autoview) 
         {
            fint status;
            deputy_c( tofchar("VIEW"), &status );
            if (status == 1) anyoutf( 1, "Starting VIEW.");
            if (status == 5) anyoutf( 1, "Called task (VIEW) disconnected" );
            if (status ==-6) anyoutf( 1, "Called task (VIEW) not present" );
            if (status ==-7) anyoutf( 1, "Max. number of tasks already active" );
         }
         exitloop = NO;
         while (!exitloop)
         {
            dfault    = REQUEST;
            keyword   = tofchar( "BOX=" );
            (void)   sprintf( message, "Give box in %s %s    [STOP]",
                     strtok( ctype[0].a, " -" ),
                     strtok( ctype[1].a, " -" ) );
            fmessage  = tofchar( message );
            /*  Default is entire subset */
            option    = 0;
            scrnum    = 3;
            do 
            {
               gdsbox_c( boxlo,
                         boxhi,
                         setin,
                         subin,
                         &dfault,
                         keyword,
                         fmessage,
                         &scrnum,
                         &option  );
   
               (void) cancel_c( keyword );
               fitlen[0] = boxhi[0] - boxlo[0] + 1;
               fitlen[1] = boxhi[1] - boxlo[1] + 1;
               if ((boxlo[0] == framelo[0]) &&
                   (boxlo[1] == framelo[1]) &&
                   (boxhi[0] == framehi[0]) &&
                   (boxhi[1] == framehi[1])) 
               {
                   /* End loop */
                   exitloop = YES;
                   agreed   = YES;
               }
               if (!exitloop) 
               {
                  agreed = (fitlen[0]*fitlen[1] <= MAXBUF);
                  if (!agreed) 
                     reject_c( keyword, tofchar("# > 128*128") );
                  else 
                  {
                     agreed = ( (boxlo[0] >= framelo[0]) &&
                                (boxlo[1] >= framelo[1]) &&
                                (boxhi[0] <= framehi[0]) &&
                                (boxhi[1] <= framehi[1]) );
                     if (!agreed)
                        reject_c( keyword, tofchar("value(s) outside frame") );
                  }
               }
            } while (!agreed);
            
            if (!exitloop) 
            {
               for (subnr = 0; subnr < (int) nsubsI; subnr++) 
                  fitgauss( boxlo, boxhi, parlist, errlist, subnr );
            }
         }
         cancel_c( tofchar("BOX=") );
         cancel_c( tofchar("INSET=") );
         r1 = 0;
         gds_close_c( setin, &r1 );
      }
   } 
   while (!quit);
      
   if (fp != NULL) 
   fclose( fp );                      /* Close the ASCII file */
   finis_c( );
}
