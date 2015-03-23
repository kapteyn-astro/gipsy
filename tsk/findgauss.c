/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             findgauss.dc1

Program:       FINDGAUSS

Purpose:       Fitting and subtraction of 2-D gaussians

Category:      FITTING, MODELS, TABLES

File:          findgauss.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 2048.

   BOX=        Give box in .....                        [entire subset]

** CUTOFF=     Give cutoff ratio for gaussian                   [1/100]
               This is the ratio of the gaussian function values
               at central point and a point x on the boundary.


   OUTSET=     Give output set (, subsets):
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets.

   SUBFILE=    Use parameter file to subtract gaussians?          Y/[N]
               Instead of calculating gaussians, it is possible to
               use parameters from a previous session to subtract
               the fitted gaussians. The parameters are written to
               an ASCII file and can be edited before the actual sub-
               traction.

               If SUBFILE=Y:

   PARAMFILE=  Name of file with stored parameters:      [Stop program]
               File must be created in a previous run of this program.
               Only a subtraction on INSET= is performed.


               If SUBFILE=N:

   PARAMFILE=  Name of file to store parameters:              [No file]
               Write gauss parameters to file so that these parameters
               can be edited and used for subtraction.
               If a given file name already exists, APPEND= must be
               specified.

   APPEND=     File exists, ok to append?                         [Y]/N
               A specified file name already exists.
               You can append to this file with APPEND=Y. If APPEND=N
               you will be prompted for another filename.


               Pre specification of initial parameters in the fit:

   AMPLITUDE=  Amplitude (map units):                      [Calculated]

   BEAM=       Beam: major, minor:                         [Calculated]
               If indicated, in seconds of arc.

   X0Y0=       Centre in grids:                            [Calculated]

   ANGLE=      Angle of major axis wrt pos. Y-axis (deg):  [Calculated]

   ZERO=       Zero level (map units):                     [Calculated]


   MASK=       Mask (0=fixed,1=free):                   [1,1,1,1,1,1,1]

   FITSIZE=    Give size (x*y) of fit box:                 [Calculated]
               Give a fixed size for the 'fitbox' instead of
               determination of suitable sizes by the program.

   SIGMA=      Rms of noise in map:                              [None]

   VALRANGE=   Range of values in search area:            [4*sigma, ->]

               Or if SIGMA= was not specified:

               Range of values in search area:               [All data]
               Data values outside given range cannot be candidates
               for a gauss fit.

** TOLERANCE=  Convergence criterion.                            [0.01]
               Fitting stops if successive iterations fail to
               produce a decrement in reduced chi-squared less than
               TOLERANCE=

** LAB=        Mixing parameter:                                 [0.01]
               Mixing parameter, LAB determines the initial
               weight of steepest descent method relative to the Taylor
               method. LAB should be a small value (i.e. 0.01).

** CLIPLOHI=   Give clip levels:                     [Include all data]
               Examples:
               1) CLIPLOHI=     include all data
               2) CLIPLOHI=3    include all data >= 3
               3) CLIPLOHI=3 5  include all data >=3 and <= 5


               Subtract only gaussians with amplitudes and beams in
               range AMPRANGE= and BEAMRANGE=:

   AMPRANGE=   Amplitude range to subtract:            [All amplitudes]

   BEAMRANGE=  Beam range to subtract:                     [All values]

   TABNAME=    Name of the table to store table data:           [GAUSS]


Description:   The program FINDGAUSS fits 2 dimensional gaussians to
               sources in an image. The fitting is used for example
               to get rid of point sources in a map or to find positi-
               on and flux of these point sources. The set that is
               examined is INSET=. The defined subset must be two
               dimensional. The number of subsets can be greater than
               1 but cannot exceed 2048. The items corresponding with
               the grid spacings (CDELTn) must be available from the
               the header of the input set, otherwise the program will
               abort. If it can find the grid spacing, and the units
               of the spacing is in degrees (along both subset axes),
               then input will be done in seconds of arc else it will
               be done in units as found in the header.
               With BOX= the area that will be searched for gaussians
               can be made smaller. The default is the entire (sub)set.
               OUTSET= defines the name of the set where the result is
               stored. This result is always the subtracted INSET= and
               therefore can be used as an inspection set. If you want
               to write to an already existing set, you will be warned
               with the message: Will overwrite data, okay ? [Y] and
               at the OKAY= prompt.
               The application tries to allocate memory for your image
               and when it is not able to allocate enough memory, it
               aborts. Then you can try again with a smaller box.

               If the result of the subtractions is not exactly what
               you wanted or expected because a couple of unwanted
               fits were extracted, there is no need to rerun the
               application completely. It is possible to write the
               gauss parameters to an ASCII file with PARAMFILE=
               This file can be edited. To use it for subtraction,
               you must give SUBFILE=Y first. Then you are prompted
               with PARAMFILE=  Pressing carriage return will stop the
               program. If a file name is given and the file can be
               opened, the parameters in this file are used to define
               the gaussians. The parameter file is made in a run
               of FINDGAUSS with SUBFILE=N   For each subset a file
               name can be specified. For each fit, the file is
               extended with one line with 12 items:

                1) An integer 0 or 1 (0: not selected for subtraction)
                                     (1: selected for subtraction)
                2) Value of fitted amplitude in map units.
                3) Major axis in (if possible) seconds of arc.
                4) Minor axis in (if possible) seconds of arc.
                5) Centre X coordinate in grids.
                6) Centre Y coordinate in grids.
                7) Angle between major axis and positive y-axis in
                   degrees (In most cases the pos. y-direction is
                   also the direction of the North).
                8) Zero level in map units.
                9) Lower X value of (fit)box where gauss is fitted.
               10) Lower Y value.
               11) Upper X value.
               12) Upper Y value.

               The fitting:

               FINDGAUSS finds the maximum in a subset by examining
               data with values above a certain level or between two
               levels (VALRANGE=) If SIGMA= has a value, the default
               for VALRANGE= is one (lower) clip equal to 4 times the
               value in SIGMA= If SIGMA= was not specified, all data
               in a subset is used, but this will result in attempts to
               fit gaussians with an amplitude comparable to the noise,
               so it is better to define a threshold. One value, say x
               will result in a value range starting with x and
               including all values greater than x. Two values, say x,y
               indicate a range where the search area is determined by
               all values between x and y (x, y values included).

               As mentioned, the fit parameters are:

               1) Amplitude,
               2) Major axis,
               3) Minor axis,
               4) Centre X,
               5) Centre Y,
               6) Position angle of major axis,
               7) Zero level.

               In the default situation, all parameters are free in the
               fit (i.e. after the fit they can have other values than
               the pre specified or estimated values).
               You can fix parameters in the fit with the keyword
               MASK= The default is MASK=1 1 1 1 1 1 1 (All free). If
               MASK=1 0 0 1 1 1 1 you fix the major and minor axis etc.
               Fixed parameters must have an initial value. This can be
               a value estimated by the program, or it can be a pre
               specified value. You can give pre specified values for
               fixed and free parameters (which will in both cases
               overrule the calculated estimates). Pre specifications
               are given with the keywords:
               AMPLITUDE=      amplitude in map units,
               ANGLE=          position angle in degrees (angle between
                               major axis and positive Y-axis counted
                               counter clockwise). You will be warned
                               if the pos. Y-axis does not align with the
                               north in your map.
               BEAM=           major, minor axes (in units as prompted).
               X0Y0=           centre X, Y in grids.
               ZERO=           zero level in map units.

               The fitting can be influenced by two variables given in
               TOLERANCE= and LAB= TOLERANCE= is the relative tolerance.
               Fitting of the gaussian function stops when
               successive iterations fail to produce a decrement in
               reduced chi-squared less than TOLERANCE=. If its value
               is less than the minimum tolerance possible, it will be
               set to this value. This means that maximum accuracy can
               be obtained by setting TOLERANCE=0.0.
               LAB= is the mixing parameter, LAB= determines the initial
               weight of steepest descent method relative to the Taylor
               method. LAB= should be a small value (i.e. 0.01).

               The data that is used in a fit can be limited with the
               keyword CLIPLOHI= If you give no values (default) than
               all data in the 'fitbox' is used in the fit. If you give
               one value, then all data greater than or equal to that
               value will be included and if you give two values, then
               data with values greater than (or equal to) the first
               clip and less than (or equal to) the second clip
               are included in the fit.

               The decision to subtract or not is limited by the key-
               words AMPRANGE= and BEAMRANGE= Default is the subtraction
               of all fitted gaussians. If you give one value for a
               range keyword, then all amplitudes (or beams) with a
               value greater than this value will be included in the
               subtraction. If you give two values, then all
               amplitudes (or beams) with values greater than (or equal
               to) the first range value and less than (or equal to)
               the second value will be included in the subtraction.


               Finding a 'fitbox':

               If the maximum in a subset is found, a box is created
               by appending a 'border' around this pixel. This box
               has size 3x3. Then the box is extended with a new
               surrounding border. If the maximum in the new extension
               exceeds the maximum in the previous extension, the
               first box becomes the 'fitbox'. This is also true if
               the mean in the new extension exceeds the mean in the
               previous extension or if the decrease in the mean is
               less than SIGMA= (i.e. if SIGMA= was specified).
               The last criterion is that the mean becomes smaller
               than the maximum times a cutoff factor given in
               CUTOFF= The default is 1/100. You have to experiment
               with the keywords SIGMA= and CUTOFF= to get an idea
               how the growth of a fitbox is stopped.


               Fixing parameters:

               You have the possibility to combine the fixing of
               parameters with the pre specification of them. Good
               results can be obtained by pre specifying the
               beam (BEAM=) and angle (ANGLE=) but with all parameters
               free in the fit (MASK=1 1 1 1 1 1 1)
               If, for instance, you fix beam and angle, then more
               gaussians will be found, but after the subtraction you
               will also find more residuals because not all sources
               have the pre specified properties, but just gave a 'good'
               fit.


               Use of tables:

               The results of this application are stored in a table.
               The default name of the table is GAUSS and the columns
               are AMPLITUDE, MINOR, MAJOR, X0grid, Y0grid, X0phys,
               Y0phys (physical counterparts of X0grid, Y0grid), ANGLE,
               ZEROLEV, AMPerr, MAJerr, MINerr, X0err, Y0err, ANGerr,
               ZEROerr, XLO, YLO, XHI, YHI (4 box coordinates in grids).
               The name of the table can be changed, the name of the
               columns not. If one table name is used twice, the
               contents of the last table will overwrite the first one.

Notes:         .......

Example:       Example of a default file:

               AMPLITUDE=                 / Program calculates estimate
               AMPRANGE= 0.04 12          / Use in subtraction only fits
                                            with ampl. in this range
               ANGLE= 0                   / Pre specify angle, 0 degrees
                                            wrt north (to east)
               BEAM= 19 13                / Pre specify beam, major axis
                                            axis 19 arcsec, minor 13 arcsec.
               BEAMRANGE= 5 40            / Use in subtraction only fits
                                            with beams in this range
               BOX=                       / Operate on entire subset
               CUTOFF= 1/100              / Limit 'fitbox' to boundaries
                                            with mean > cutoff*maxampl.
               PARAMFILE=storeparm.txt    / Sore fit parameters in ASCII
                                            file
               FITSIZE=                   / Do not use a fixed size for
                                            the fitbox.
               INSET=AURORA freq 0        / Set and subset
               MASK= 1 1 1 1 1 1 1        / All parameters free in fit
               OUTSET=SUBAURORA           / Name of subtracted data
               SIGMA= 0.01                / Rms of noise
               SUBFILE=N                  / Do not use an ASCII file
                                            with gauss parameters for
                                            subtraction
               TABNAME=                   / Use default name for table
               VALRANGE=                  / Sigma was specified so
                                            default is VALRANGE=4*0.01
                                            which implies that only
                                            data > 0.04 can be candidates
                                            for a fit
               X0Y0=                      / Program calculates estimate
               ZERO=                      / Program calculates estimate


Updates:       Sep 22,  1992: VOG, Document created.
               Oct  4,  1993: VOG, Repaired bug in preset of FWHM
               Mar,25   1994: VOG, Replaced some 'tofchar's by str2char
                                   in routine 'putintable'.

#<
*/

/*  findgauss.c: include files     */

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
#include    "userchar.h"
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
#include    "gdsd_rdble.h"
#include    "gdsd_rchar.h"
#include    "minmax3.h"      /* Find min, max and #blanks in subset. */
#include    "wminmax.h"      /* Writes (new) minimum and maximum and number */
                             /* of blanks of subsets in the descriptor file */
                             /* and optionally deletes the MINMAX descriptors */
                             /* at intersecting levels. */

/* Output set related includes */

#include    "gdsasn.h"       /* GDSASN copies the coordinate system of a */
                             /* previously opened input set obtained with */
                             /* GDSINP to the output set to be obtained */
                             /* with GDSOUT. */
#include    "gdsout.h"       /* GDSOUT prompts the user to enter the */
                             /* name of an output set and the subsets, */
                             /* and returns the number of subsets entered. */
#include    "gdsi_write.h"   /* Writes data to (part of) an set. */


/* Miscellaneous */

#include    "fitgauss2d.h"
#include    "status.h"
#include    "cotrans.h"
#include    "timer.h"


/* Related to tables */

#include    "gdsa_crecol.h"
#include    "gdsa_delcol.h"
#include    "gdsa_colinq.h"
#include    "gdsa_wcreal.h"
#include    "gdsa_wcint.h"

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
#define MAXAXES        10              /* Max. axes in a set */
#define MAXSUBSETS     2048            /* Max. allowed subsets */
#define MAXBUF         4096            /* Buffer size for I/O */
#define MAXFITXY       64*64
#define MAXITERS       100
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set (subset(s)): ")

/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     nsubs;              /* Number of input subsets */
static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered ccording to the */
                                    /* specification by the user. */
static fint     showdev;            /* Device number (as in ANYOUT) for info */
static fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
                                    /* number of grids along an axes as */
                                    /* specified by the user. The first elements */
                                    /* (upto the dimension of the subset) contain */
                                    /* the length of the subset axes, the other */
                                    /* ones contain the the number of grids along */
                                    /* an axes outside the subset. */
static fint     maxsubs = MAXSUBSETS;
static fint     maxaxes = MAXAXES;  /* Max num. of axes the program can deal with.*/
static fint     class = 1;          /* Class 1 is for applications which repeat */
                                    /* the operation for each subset, Class 2 */
                                    /* is for applications for which the operation */
                                    /* requires an interaction between the different */
                                    /* subsets. */
static fint     subdim;             /* Dimensionality of the subsets for class 1 applications */
static fint     setdim;             /* Dimension of set. */

/* Box and frame related */

static fint     flo[MAXAXES];       /* Low  edge of frame in grids */
static fint     fhi[MAXAXES];       /* High edge of frame in grids */
static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */
static fint     boxopt;             /* The different options are: */
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */

/* Reading data */

static fint     cwlo, cwhi;         /* Coordinate words. */
static fint     tid;                /* Transfer id for read function. */
static fint     pixelsread;         /* Number of pixels read by read routine. */
static fint     pixelswrite;        /* Number of pixels to write to output. */
static float    *image = NULL;      /* Buffer for read routine. */
static int      *mask  = NULL;      /* Mask for image */
static int      maskblank;
static float    gaussian[MAXFITXY];
static fint     subnr;              /* Counter for subset loop. */

/* OUTSET related variables */

static fchar    Setout;
static fint     suboutA[MAXSUBSETS]; /* Output subset coordinate words */
static fint     nsubsout;
static fint     axnumout[MAXAXES];
static fint     axcountout[MAXAXES];
static fint     cwloO, cwhiO;        /* Output Coordinate words. */
static fint     tidO;                /* Transfer id for write function. */

/* Miscellaneous */

static fchar    Key, Mes;
static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[120];       /* All purpose character buffer. */
static int      i;                  /* Various counters. */
static bool     agreed;             /* Loop guard. */
static float    minvalA[MAXSUBSETS]; /* Min. value of data for each subset. */
static float    maxvalA[MAXSUBSETS]; /* Max. value of data for each subset. */
static fint     nblanksA[MAXSUBSETS];/* Number of blanks in each subset. */
static fint     mcount;             /* Initialize MINMAX3. */
static fint     change;             /* Used in WMINMAX. change!=0 means */
                                    /* minimum and maximum have changed and */
                                    /* that the MINMAX descriptors at */
                                    /* intersecting levels will be removed. */
FILE            *asciifile;         /* File pointer to ASCII file */
static fint     lenX, lenY;         /* Box lengths */
static fint     buflen;             /* Total number of pixels in box */
static double   cdelt[2];
static fchar    cunit[2];
static float    parlist[7];
static float    storelist[7];
static float    errlist[7];
static fint     mpar[7];
static float    cutoffratio;
static fint     fitlen[2];
static float    sigma;
static fchar    bunit;
static bool     degreeX, degreeY;
static float    clip[2];
static float    tol, lab;
static fint     its;
static double   gridspac[2];
static float    amprange[2];
static float    beamrange[2];
static float    valrange[2];
static bool     subtractfromfile;
FILE            *paramreadfile;
FILE            *paramwritefile;
static bool     fromparam, toparam;
static bool     unknownunits;
static char     readfilename[120], writefilename[120];
static fchar    Tname;
static double   cputime, realtime;
static fint     elapse;


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


FILE *openfile( fchar Key, fchar Mes, char *filename, char *mode )
/*-------------------------------------------------------------*/
/* Open file for writing/reading. Ask filename in GIPSY way    */
/* Check file for existence. Return file pointer and the name  */
/* of the given file. The function introduces the keyword      */
/* APPEND= for 'write' files that already exist. The macro     */
/* 'fmake' must be available.                                  */
/*-------------------------------------------------------------*/
{
#include    "stdio.h"
#include    "usertext.h"
#include    "userlog.h"
#include    "cancel.h"
#include    "reject.h"

#define      NO    0

   fchar     Filename;
   bool      append;
   fint      request = 1;
   fint      dfault;
   fint      nitems = 1;
   fint      agreed;
   fint      n;
   FILE     *fp;
   bool      readmode, writemode;


   readmode  = (strstr( mode, "r") != NULL);
   writemode = (strstr( mode, "w") != NULL);

   fmake( Filename, 120 );
   if (readmode) {
      dfault = request;
      do {
         n = usertext_c( Filename,
                         &dfault,
                         Key,
                         Mes );
         if (n == 0) return(NULL);
         strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */
         fp = fopen(filename, "r");
         cancel_c( Key );
      } while (fp == NULL);
      return( fp );
   }
   if (writemode) {
      dfault = request;
      do {
         n = usertext_c( Filename,
                         &dfault,
                         Key,
                         Mes );
         if (n == 0) return(NULL);
         strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */
         fp = fopen(filename, "r");
         cancel_c( Key );
         if (fp != NULL) {       /* File exists */
            append = toflog( NO );
            n   = userlog_c( &append,
                             &nitems,
                             &dfault,
                             tofchar("APPEND="),
                             tofchar("File exists, ok to append?    Y/[N]") );
            append = tobool( append );
            fclose( fp );
            cancel_c( tofchar("APPEND=") );
            if (append) {
               fp = fopen(filename, "a");
               agreed = (fp != NULL);
               if (!agreed) {
                  (void) reject_c( Key,
                                   tofchar("Cannot open for appending, try another!") );
               } else {
                  return( fp );
               }
            } else {
               agreed = NO;
            }
         } else {
            /* File does not exist */
            fp = fopen(filename, "w");
            agreed = (fp != NULL);
            if (!agreed) {
               (void) reject_c( Key,
                                tofchar("Cannot open for writing, try another!") );
            } else {
               return( fp );
            }
         }
      } while (!agreed);
   }
   return( NULL );                /* Return NULL if not write or read mode */
}


static float toangle( float Angle, float Maxangle )
/*---------------------------------------------------*/
/* Return angle between 0 and 'Maxangle'             */
/*---------------------------------------------------*/
{
   while (Angle < 0.0) Angle += Maxangle;
   while (Angle > Maxangle) Angle -= Maxangle;
   return Angle;
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


      if (parlist[5] == blank) parlist[5] = 0.0;
      if ((parlist[5] > 10) || (parlist[5] < -10)) parlist[5] = 0.0;

      if (parlist[5] == blank) anyoutC( 3, "Angle = blank" );
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


      if (parlist[5] == blank) parlist[5] = 0.0;
      if ((parlist[5] > 10) || (parlist[5] < -10)) parlist[5] = 0.0;
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



static int inbox( fint x, fint y, fint *blo, fint *bhi )
{
   if ( (x >= blo[0]) && (x <= bhi[0]) && (y >= blo[1]) && (y <= bhi[1]) ) {
      return(YES);
   } else {
      return(NO);
   }
}


static float getval( fint x, fint y, fint *blo, fint *bhi )
/*--------------------------------------------------------*/
/* Find 1 dim. pos in box defined by blo, bhi and return  */
/* image value at that point.                             */
/*--------------------------------------------------------*/
{
   return( image[(x-blo[0])+(y-blo[1])*(bhi[0]-blo[0]+1)] );
}


static void setval( float value, fint x, fint y,
                    fint *blo, fint *bhi )
/*--------------------------------------------------------*/
/* Find 1 dim. pos in box defined by blo, bhi and set     */
/* image value at that point.                             */
/*--------------------------------------------------------*/
{
   image[(x-blo[0])+(y-blo[1])*(bhi[0]-blo[0]+1)] = value;
}



static void zeromask( fint x, fint y, fint *blo, fint *bhi )
/*--------------------------------------------------------*/
/* Find 1 dim. pos in box defined by blo, bhi and set     */
/* mask value at that point.                             */
/*--------------------------------------------------------*/
{
   mask[(x-blo[0])+(y-blo[1])*(bhi[0]-blo[0]+1)] = 0;
}



static int maxpos( float *varmaxval, int ndat, int subtnum )
/*--------------------------------------------------------*/
/* Determine maximum and the position of this maximum in  */
/* the array 'image' with length 'ndat'. The function is  */
/* also used to indicate the status of the proceedings.   */
/*--------------------------------------------------------*/
{
   float      maxval;
   int        mpos;
   int        i;
   float      value;
   int        nblank;
   float      stalo, stahi, stacur;

   nblank = 0;
   mpos   = -1;                   /* Return -1 if all data are examined */
   maxval = blank;
   for (i = 0; i < ndat; i++) {
      value = image[i];
      if (mask[i]) {
         if (maxval == blank) {
            if (value != blank) {
               maxval = value;
               mpos = i;
            }
         } else {
            if (value > maxval) {
               maxval = value;
               mpos = i;
            }
         }
      } else {
         nblank++;
      }
   }
   /* How far is the subtraction? */
   stalo = (float) maskblank; stahi = (float) ndat; stacur = (float) nblank;
   sprintf( message, "Subtract gauss: %d, Examined area: %d%",
            subtnum, (int)( 100.0*(stacur-stalo)/(stahi-stalo) )  );
   (void) status_c( tofchar(message) );

   *varmaxval = maxval;
   return( mpos );
}



static int xy2pos( fint x, fint y, fint *blo, fint *bhi )
/*----------------------------------------------------------*/
/* Convert a two dim position into an array index.          */
/*----------------------------------------------------------*/
{
   return(x-blo[0])+(y-blo[1])*(bhi[0]-blo[0]+1);
}



static void pos2xy( int pos, fint *blo, fint *bhi,
                    int *x, int *y )
/*--------------------------------------------------------*/
/* Convert a one dim. pos. position to a two dim. pos.    */
/* related to the box coordinates given in blo, bhi.      */
/*--------------------------------------------------------*/
{
   int      ny;

   lenX = (int) bhi[0] - blo[0] + 1;
   ny = pos / lenX;
   *y = (int) blo[1] + ny;
   *x = (int) blo[0] + pos - ny * lenX;
}


static void  putintable( fchar Tname, float amplitude,
                         float major, float minor,
                         float x0, float y0, float x0phys,
                         float y0phys, float angle,
                         float zerolevel, float *errlist,
                         fint *sblo, fint *sbhi,
                         fchar Setin, fint subin )
/*-------------------------------------------------------------------*/
/* Put values in columns in table with name "Tabname". If the column */
/* already exists, delete and (re)create the column.                 */
/*-------------------------------------------------------------------*/
{
#define            VARLEN         132       /* Length of variable GDS records */

   fint            r1, r2;
   fint            one = 1;
   static fint     count = 1;
   fint            nrows;
   fchar           Type;
   fchar           Comment;
   fchar           Units;
   fchar           Cname;


   r1 = 0;
   fmake( Type,     VARLEN );
   fmake( Comment,  VARLEN );
   fmake( Units,    VARLEN );
   fmake( Cname, 10 );


   (void) str2char( "AMPLITUD", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Amplitude", Comment );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, bunit, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &amplitude, &count, &one, &r1 );

   (void) str2char( "MAJOR", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char( "REAL", Type );
      (void) str2char( "Major beam", Comment );
      if ((degreeX) && (degreeY))
         (void) str2char("ARCSEC", Units);
      else
         (void) str2char("DEGREE", Units);

      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &major, &count, &one, &r1 );

   (void) str2char( "MINOR", Cname );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char( "REAL", Type );
      (void) str2char( "Minor beam", Comment );
      if ((degreeX) && (degreeY))
         (void) str2char("ARCSEC", Units);
      else
         (void) str2char("DEGREE", Units);

      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &minor, &count, &one, &r1 );

   (void) str2char( "X0grid", Cname );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char( "X centre (grids)", Comment );
      (void) str2char("GRIDS", Units );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &x0, &count, &one, &r1 );

   (void) str2char( "Y0grid", Cname );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char( "Y centre (grids)", Comment );
      (void) str2char("GRIDS", Units );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &y0, &count, &one, &r1 );


   (void) str2char( "X0phys", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char( "REAL", Type );
      (void) str2char( "X centre (physical)", Comment );
      (void) str2char( "DEGREE", Units );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &x0phys, &count, &one, &r1 );

   (void) str2char( "Y0phys", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Y centre (physical)", Comment );
      (void) str2char("DEGREE", Units );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &y0phys, &count, &one, &r1 );

   (void) str2char( "ANGLE", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Angle (degrees)", Comment );
      (void) str2char("DEGREE", Units );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &angle, &count, &one, &r1 );

   (void) str2char( "ZEROLEV", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Zero level", Comment );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, bunit, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &zerolevel, &count, &one, &r1 );

   (void) str2char( "AMPerr", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Error in Amplitude", Comment );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, bunit, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &errlist[0], &count, &one, &r1 );


   (void) str2char( "MAJerr", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Error in major beam.", Comment );
      if ((degreeX) && (degreeY))
         (void) str2char("ARCSEC", Units);
      else
         (void) str2char("DEGREE", Units);

      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &errlist[1], &count, &one, &r1 );

   (void) str2char( "MINerr", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Error in minor beam.", Comment );
      if ((degreeX) && (degreeY))
         (void) str2char("ARCSEC", Units);
      else
         (void) str2char("DEGREE", Units);

      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &errlist[2], &count, &one, &r1 );

   (void) str2char( "X0err", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Error in x pos (grids)", Comment );
      (void) str2char("GRIDS", Units );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units,  &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &errlist[3], &count, &one, &r1 );

   (void) str2char( "Y0err", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
      gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Error in y pos (grids)", Comment );
      (void) str2char("GRIDS", Units );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &errlist[4], &count, &one, &r1 );

   (void) str2char( "ANGLEerr", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Error in angle (deg)", Comment );
      (void) str2char("DEGREE", Units );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &errlist[5], &count, &one, &r1 );

   (void) str2char( "ZEROerr", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("REAL", Type );
      (void) str2char("Error in zero level", Comment );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, bunit, &r1 );
   }
   r1 = 0;
   gdsa_wcreal_c( Setin, &subin, Tname, Cname, &errlist[6], &count, &one, &r1 );

   (void) str2char("GRIDS", Units );
   (void) str2char( "XLO", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("INT", Type );
      (void) str2char("Box: X low", Comment );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcint_c( Setin, &subin, Tname, Cname, &sblo[0], &count, &one, &r1 );

   (void) str2char( "YLO", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("INT", Type );
      (void) str2char("Box: Y low", Comment );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcint_c( Setin, &subin, Tname, Cname, &sblo[1], &count, &one, &r1 );

   (void) str2char( "XHI", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
         gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("INT", Type );
      (void) str2char("Box: X high", Comment );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcint_c( Setin, &subin, Tname, Cname, &sbhi[0], &count, &one, &r1 );

   (void) str2char( "YHI", Cname  );
   if (count == 1)
   {
      (void) gdsa_colinq_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
      if (r1 >= 0)
      gdsa_delcol_c( Setin, &subin, Tname, Cname, &r2 );
      r1 = 0;
      (void) str2char("INT", Type );
      (void) str2char("Box: Y high", Comment );
      gdsa_crecol_c( Setin, &subin, Tname, Cname, Type, Comment, Units, &r1 );
   }
   r1 = 0;
   gdsa_wcint_c( Setin, &subin, Tname, Cname, &sbhi[1], &count, &one, &r1 );

   count++;
}




static void displayvalues( fchar Tname, float *parlist,
                           fint *sblo, fint *sbhi,
                           double *gridspac,
                           fint iters, bool subtract,
                           FILE *paramwritefile,
                           fchar Setin, fint subin )
/*--------------------------------------------------------*/
/* Display results and put results in table.              */
/*--------------------------------------------------------*/
{
   float   x0, y0;
   char    display[120];
   bool    toparm = (paramwritefile != NULL);
   float   angle;
   float   amplitude;
   float   zerolevel;
   fint    r1;
   double  coordin[2];          /* Grids before transformation */
   double  coordout[MAXAXES];   /* Physical coordinates after transformation */
   float   x0phys, y0phys;
   fint    direction;
   float   major, minor;
   float   dum1;
   int     len;
   static int first = YES;


   amplitude = parlist[0];
   minor = parlist[1];
   major = parlist[2];
   if ((degreeX) && (degreeY)) {
      minor *= 3600.0;
      major *= 3600.0;
      errlist[1] *= 3600.0;
      errlist[2] *= 3600.0;
   }
   x0 = ((float)sblo[0]) + (parlist[3]/(float)gridspac[0]);
   y0 = ((float)sblo[1]) + (parlist[4]/(float)gridspac[1]);
   errlist[3] /= gridspac[0];
   errlist[4] /= gridspac[1];
   angle = toangle(DEG(parlist[5]), 180.0 );
   errlist[5] = DEG( errlist[5] );
   /* Sort in major, minor axes */
   if (minor > major) {
      dum1  = minor;
      minor = major;
      major = dum1;
      dum1  = errlist[1];
      errlist[1] = errlist[2];
      errlist[2] = dum1;
      angle -= 90.0;
      angle = toangle( angle, 180.0 );
   }
   zerolevel  = parlist[6];

   if (first) {
      len = sprintf( display,
                    "%2s|%11.11s|%7.7s|%7.7s|%7.7s|%6.6s|%6.6s|%7.7s|%7.7s|%s",
                    "SR", "START POS", "AMPL", "MAJ", "MIN", "X0", "Y0", "ANGLE", "ZLEV", "  FIT BOX" );
      anyoutC( 3, display );
      memset( display, '=', len ); display[len] = '\0';
      anyoutC( 3, display );
      first = NO;
   }

   if (subtract) {
      sprintf( display, "S  [%+4d,%+4d]", NINT(x0), NINT(y0) );
   } else {
      sprintf( display, "R  [%+4d,%+4d]", NINT(x0), NINT(y0) );
   }
   sprintf( display, "%.*s %7.2f", strlen(display), display, amplitude );
   sprintf( display, "%.*s %7.2f", strlen(display), display, major );
   sprintf( display, "%.*s %7.2f", strlen(display), display, minor );
   sprintf( display, "%.*s %6.2f", strlen(display), display, x0 );
   sprintf( display, "%.*s %6.2f", strlen(display), display, y0 );
   sprintf( display, "%.*s %7.2f", strlen(display), display, angle );
   sprintf( display, "%.*s %7.2f", strlen(display), display, zerolevel );
   sprintf( display, "%.*s [%d %d %d %d] (%d iterations)",
            strlen(display), display,
            sblo[0], sblo[1], sbhi[0], sbhi[1], iters );
   anyoutC( 3, display );
   if (toparm) {
      fprintf( paramwritefile,
               "%1d %10f %10f %10f %10f %10f %10f %10f  %d %d %d %d\n",
               subtract, parlist[0], major, minor, x0, y0, angle,
               parlist[6], sblo[0], sblo[1], sbhi[0], sbhi[1] );
   }
   if (subtract) {
      /* Convert x0, y0 into physical coordinates */
      coordin[0] = (double) x0;
      coordin[1] = (double) y0;
      direction = 1;                           /* grid coord. -> physical coord. */
      r1 = cotrans_c( Setin, &subin, coordin, coordout, &direction );
      x0phys = (float) coordout[ axnum[0]-1 ];
      y0phys = (float) coordout[ axnum[1]-1 ];
      putintable( Tname, amplitude, major, minor, x0,y0, x0phys, y0phys,
                  angle, zerolevel,
                  errlist, sblo, sbhi, Setin, subin );
   }
}



static int extendbox( int xlo, int xhi, int ylo, int yhi,
                      fint *blo, fint *bhi, float maxval,
                      float minval )
/*----------------------------------------------------------------*/
/* A box can be extended if the new row or column has a maximum   */
/* value that is not greater than the box maximum. It also cannot */
/* be smaller than the minimum value (used as cutoff for the      */
/* gaussian).                                                     */
/*----------------------------------------------------------------*/
{
   int            x, y;
   float          value;
   float          localmax;
   int            extend;


   localmax = blank;
   for (y = ylo; y <= yhi; y++) {
      for (x = xlo; x <= xhi; x++) {
         value = getval( x, y, blo, bhi );
         if (localmax == blank) {
            if (value != blank) localmax = value;
         } else {
            if (value > localmax) localmax = value;
         }
      }
   }
   if (localmax == blank) {
      extend = NO;
   } else {
      extend = ((localmax < maxval) && (localmax >= minval));
   }
   return( extend );
}


static void getstat( int x, int y, fint *blo, fint *bhi,
                     float *sum, fint *ndat, float *minval,
                     float *maxval, fint *nblank )
/*----------------------------------------------------------*/
/* Running min, max, sum and num. of blanks.                */
/*----------------------------------------------------------*/
{
   float     value;

   if (inbox( x, y, blo, bhi )) {
      value = getval( x, y, blo, bhi );
      if (value != blank) {
         (*ndat)++;
         *sum += value;
         if (*maxval == blank) {
            *maxval = value;
         } else {
           if (value > *maxval) *maxval = value;
         }
         if (*minval == blank) {
            *minval = value;
         } else {
            if (value < *minval) *minval = value;
         }
      } else {
         (*nblank)++;
      }
   } else {
      (*nblank)++;
   }
}



void borderstat( fint *sblo, fint *sbhi, fint *blo, fint *bhi,
                 float *min, float *max, float *mean, float *rms,
                 fint *nblank )
{
   float   sum;
   fint    ndat;
   int     x, y;
   float   maxval, minval;


   sum     = 0.0;
   *nblank = 0;
   maxval  = blank; minval = blank;
   ndat    = 0;
   x = sblo[0];
   for (y = sblo[1]; y <= sbhi[1]; y++) {
      (void) getstat( x, y, blo, bhi, &sum, &ndat, &minval, &maxval, nblank );
   }
   x = sbhi[0];
   for (y = sblo[1]; y <= sbhi[1]; y++) {
      (void) getstat( x, y, blo, bhi, &sum, &ndat, &minval, &maxval, nblank );
   }
   y = sblo[1];
   for (x = sblo[0]; x <= sbhi[0]; x++) {
      (void) getstat( x, y, blo, bhi, &sum, &ndat, &minval, &maxval, nblank );
   }
   y = sbhi[1];
   for (x = sblo[0]; x <= sbhi[0]; x++) {
      (void) getstat( x, y, blo, bhi, &sum, &ndat, &minval, &maxval, nblank );
   }
   if (ndat == 0) {
      *mean = blank;
   } else {
      *mean = sum / ndat;
   }
   *min = minval;
   *max = maxval;
   *rms = 0.0;
}


static int findbox( fint *blo, fint *bhi, fint Xm, fint Ym,
                    fint *sblo, fint *sbhi, fint *fitlen,
                    float maxval, float sigma, float cutoffratio )
/*----------------------------------------------------------------------*/
/* Find a box around the position Xm, Ym. If the values of 'fitlen'     */
/* are not negative on entry, a box will be created with centre Xm,     */
/* Ym and length 'fitlen[0]*fitlen[1]'. Else, a suitable box will       */
/* be created. The criteria for construction are listed where suitable. */
/*----------------------------------------------------------------------*/
{
   fint     semiX, semiY;
   fint     slenX, slenY;
   int      quit;
   fint     extendval;
   int      goodbox;
   float    max1, max2, min1, min2, mean1, mean2, rms1, rms2;
   fint     nblank1, nblank2;


   if (fitlen[0] > 0) {
      semiX = fitlen[0]/2;
      if (fitlen[1] <= 0) fitlen[1] = fitlen[0];
      semiY = fitlen[1]/2;
      sblo[0] = MYMAX( blo[0], (Xm - semiX ) );
      sblo[1] = MYMAX( blo[1], (Ym - semiY ) );
      sbhi[0] = MYMIN( bhi[0], (Xm + semiX ) );
      sbhi[1] = MYMIN( bhi[1], (Ym + semiY ) );
      slenX = sbhi[0] - sblo[0] + 1;
      slenY = sbhi[1] - sblo[1] + 1;
   } else {
      /*----------------------*/
      /* Find a suitable box: */
      /*----------------------*/
      extendval = 1;
      sblo[0] = Xm - extendval;
      sblo[1] = Ym - extendval;
      sbhi[0] = Xm + extendval;
      sbhi[1] = Ym + extendval;
      (void) sprintf( message,
                     "<FINDBOX> Start extending the fitbox: %d %d %d %d",
                      sblo[0], sblo[1], sbhi[0], sbhi[1] );
      anyoutC( 16, message );
      /*-----------------------------------------------*/
      /* Do some statistics on the border of this box. */
      /*-----------------------------------------------*/
      borderstat( sblo, sbhi, blo, bhi, &min1, &max1, &mean1, &rms1, &nblank1 );
      do {
         extendval++;
         sblo[0] = Xm - extendval;
         sblo[1] = Ym - extendval;
         sbhi[0] = Xm + extendval;
         sbhi[1] = Ym + extendval;
         slenX   = sbhi[0] - sblo[0] + 1;
         slenY   = sbhi[1] - sblo[1] + 1;
         borderstat( sblo, sbhi, blo, bhi, &min2, &max2, &mean2, &rms2, &nblank2 );
/*         if ((max2 == blank) || (min2 == blank) || (mean2 == blank) || (rms2 == blank)) {
            anyoutC( 3, "blank values" );
         } else {
            sprintf( message, "%f %f %f %f %d ", max2, min2, mean2, rms2, nblank2);
            anyoutC( 3, message );
         }
*/
         /*--------------------------------------------------------------------*/
         /* Set up criteria to abort expanding the box.                        */
         /* 1) The max of the new border cannot exceed the max of the old one. */
         /* 2) If the mean changes less than 1% abort expanding.               */
         /* 3) Abort if mean reaches a given minimum.                          */
         /* 4) Abort if box greater than max size.                             */
         /*--------------------------------------------------------------------*/
         quit    = (max2 > max1);
         if (quit) {
            sprintf( message, "<FINDBOX> box [%d %d %d %d] : max2>max1: %f %f",
                     sblo[0], sblo[1], sbhi[0], sbhi[1],
                     max2, max1 );
            anyoutC( 16, message );
         }
         quit    = quit || (mean2 > mean1) ;
         if (quit) {
            sprintf( message, "<FINDBOX> box [%d %d %d %d] : mean2>?mean1: %f %f",
                     sblo[0], sblo[1], sbhi[0], sbhi[1],
                     mean2, mean1 );
            anyoutC( 16, message );
         }
         if (sigma != blank) {
            quit    = quit || ( fabs((mean1-mean2)/mean1) < sigma);
            if (quit) {
               sprintf( message, "<FINDBOX> box [%d %d %d %d] : delta=%f",
                        sblo[0], sblo[1], sbhi[0], sbhi[1],
                        fabs((mean1-mean2)/mean1) );
               anyoutC( 16, message );
            }
         }
         quit    = quit || (mean2 < (maxval*cutoffratio));
         if (quit) {
            sprintf( message, "<FINDBOX> box [%d %d %d %d] : mean <? maxval*cutoffratio %f %f",
                     sblo[0], sblo[1], sbhi[0], sbhi[1],
                     mean2, maxval*cutoffratio );
            anyoutC( 16, message );
         }
         /*--------------------------------------------------------------------*/
         /* The size of the box cannot exceed MAXFITXY. This must be checked   */
         /* before the size is increased. Note that the increment in size is 2 */
         /* i.e. 1 for each end.                                               */
         /*--------------------------------------------------------------------*/
         quit    = quit || ((slenX+2)*(slenY+2) > MAXFITXY);
         max1    = max2;
         mean1   = mean2;
         rms1    = rms2;
         nblank1 = nblank2;
         sprintf( message, "<FINDBOX> Expanded fit box: %d %d %d %d",
                  sblo[0], sblo[1], sbhi[0], sbhi[1] );
         anyoutC( 16, message );

      } while (!quit);
      sblo[0] = MYMAX( blo[0], (Xm - extendval) );
      sblo[1] = MYMAX( blo[1], (Ym - extendval) );
      sbhi[0] = MYMIN( bhi[0], (Xm + extendval) );
      sbhi[1] = MYMIN( bhi[1], (Ym + extendval) );
      slenX   = sbhi[0] - sblo[0] + 1;
      slenY   = sbhi[1] - sblo[1] + 1;
   }
   goodbox = ((slenX >= 3) && (slenY >= 3));
   return( goodbox );
}


static void subtractgauss( float *parlist, fint *sblo, fint *sbhi,
                           double *gridspac )
/*--------------------------------------------------------------------*/
/* Subtract in the 'fitbox' the gaussian with parameters in 'parlist. */
/* Note that the lower left pixel in the 'fitbox' is (0,0) and the    */
/* coordinates are in physical coordinates.                           */
/*--------------------------------------------------------------------*/
{
   int    x, y;
   int    pos;
   float  xydat[2];
   fint   npar = 7;
   fint   fopt = 0;
   float  gridX, gridY;
   float  value;
   int    Xlo, Xhi, Ylo, Yhi;
   int    centX, centY;


   centX = ( sbhi[0] + sblo[0] ) / 2;
   centY = ( sbhi[1] + sblo[1] ) / 2;
   Xlo = centX - 30;
   Xhi = centX + 30;
   Ylo = centY - 30;
   Yhi = centY + 30;
   Xlo = MYMAX( blo[0], Xlo );
   Ylo = MYMAX( blo[1], Ylo );
   Xhi = MYMIN( bhi[0], Xhi );
   Yhi = MYMIN( bhi[1], Yhi );


   gridX = (float) fabs( gridspac[0] );
   gridY = (float) fabs( gridspac[1] );
   /* Set zero level to 0.0 to avoid creating holes where the */
   /* fitted zero level was not equal to 0.0 */
   parlist[6] = 0.0;
/*   for (y = sblo[1]; y <= sbhi[1]; y++) {
      for (x = sblo[0]; x <= sbhi[0]; x++) {*/
   for (y = Ylo; y <= Yhi; y++) {
      for (x = Xlo; x <= Xhi; x++) {
         pos = xy2pos( x, y, blo, bhi );
         xydat[0] = ((float)(x - sblo[0])) * gridX;
         xydat[1] = ((float)(y - sblo[1])) * gridY;
         value = func_c( xydat, parlist, &npar, &fopt );
         image[pos] -= value;
      }
   }
}


static int  findandsubtract( fint *blo, fint *bhi, float sigma,
                             float cutoffratio,
                             fint *fitlen, float tol, float lab,
                             fint its, float *clip, float *parlist,
                             float *storelist, fint *mpar,
                             double *gridspac, float *amprange,
                             float *beamrange, FILE *paramwritefile,
                             fchar Setin, fint subin, fchar Tname )
/*--------------------------------------------------------------------*/
/* 'image' contains the data in a lenX * lenY box. Find the maximum   */
/* in the map, determine a small box around that maximum, feed data   */
/* to fit routine, store parameters in ascii file, blank this box,    */
/* find next maximum etc. The routine ends if all image data is       */
/* blanked.                                                           */
/*--------------------------------------------------------------------*/
{
   int        x, y;
   fint       lenX, lenY;
   int        mpos;
   float      maxval;
   float      minval = 0.0;
   int        Xm, Ym;
   fint       sblo[2], sbhi[2];
   fint       slenX, slenY;
   int        index;
   fint       iters;
   int        i;
   int        goodbox;
   int        nblanks;
   bool       subtract;
   int        subs = 0;            /* Number of gaussians that are subtracted from INSET */


   lenX = bhi[0] - blo[0] + 1;
   lenY = bhi[1] - blo[1] + 1;

   /*------------------------------------------------------------------------*/
   /* Start a loop to find all candidates for a gauss fit. The loop ends if  */
   /* the array 'mask' contains blanks only. 'mpos' is the one dim. position */
   /* of the maximum in the array 'image'.                                   */
   /*------------------------------------------------------------------------*/
   while (  ( mpos = maxpos(&maxval, lenX*lenY, subs) ) != -1  ) {
      pos2xy( mpos, blo, bhi, &Xm, &Ym );    /* Convert position of max. to 2-d pos. */
      goodbox = findbox( blo, bhi,           /* In:  The input box. */
                         Xm, Ym,             /* In:  The 2-dim pos. of the max. */
                         sblo, sbhi,         /* Out: The sizes of the constructed box */
                         fitlen,             /* In:  User given fixed sizes */
                         maxval,             /* In:  Image value of the max. */
                         sigma,              /* In:  The sigma of all values in 'image' */
                         cutoffratio );      /* In:  Cutoff ratio for the gaussian. */


      /*-----------------------------------------------------------------*/
      /* All values in this box cannot participate anymore as candidates */
      /* for finding a maximum. Values in this box are stored in array   */
      /* 'gaussian'.                                                     */
      /*-----------------------------------------------------------------*/
      for (y = sblo[1], index = 0, nblanks = 0; y <= sbhi[1]; y++) {
         for (x = sblo[0]; x <= sbhi[0]; x++, index++) {
            float   val;
            val = getval( x, y, blo, bhi );
            if (val == blank) {
               nblanks++;
            } else {
               if (minval == blank ){
                  minval = val;
               } else {
                  if (val < minval) minval = val;
               }
            }
            gaussian[index] = val;
            zeromask( x, y, blo, bhi );
         }
      }

      /*-------------------------------------------------------*/
      /* Is the found box a real good candidate? If so, try to */
      /* fit a 2-d gaussian to the date in this box.           */
      /*-------------------------------------------------------*/
      goodbox = ((index - nblanks) > 8);         /* Enough degrees of freedom in fit? */
      if (goodbox) goodbox = (minval < maxval);  /* Second restriction */
      if (goodbox) {
         slenX = sbhi[0] - sblo[0] + 1;
         slenY = sbhi[1] - sblo[1] + 1;
         iters = fitgauss2d_c(  gaussian,
                                &slenX,
                                &slenY,
                                &gridspac[0],
                                &gridspac[1],
                                parlist,
                                errlist,
                                mpar,
                                &tol,
                                &its,
                                &lab,
                                clip );




         { float  X0, Y0;
            X0 = ((float)sblo[0]) + (parlist[3]/(float)gridspac[0]);
            Y0 = ((float)sblo[1]) + (parlist[4]/(float)gridspac[1]);
            if (( X0 < (float)sblo[0]) || (X0 > (float)sbhi[0])  ||
                (Y0 < (float)sblo[1]) || (Y0 > (float)sbhi[1]) ) {
               iters = -15;
            }
         }
         if (iters < 0) {
            sprintf( message, "No gaussian for box around %d, %d (err:%d)", Xm, Ym, iters );
            anyoutC( 16, message );
         } else {
            subtract = YES;
            if (amprange[0] != blank) {
               subtract = (parlist[0] >= amprange[0]);
            }
            if (amprange[1] != blank) {
               if (subtract) subtract = (parlist[0] <= amprange[1]);
            }
            if (beamrange[0] != blank) {
               if (subtract) subtract = (parlist[1] >= beamrange[0]);
            }
            if (beamrange[1] != blank) {
               if (subtract) subtract = (parlist[1] <= beamrange[1]);
            }
            if (beamrange[0] != blank) {
               if (subtract) subtract = (parlist[2] >= beamrange[0]);
            }
            if (beamrange[1] != blank) {
               if (subtract) subtract = (parlist[2] <= beamrange[1]);
            }
            displayvalues( Tname, parlist, sblo, sbhi, gridspac, iters,
                           subtract, paramwritefile, Setin, subin );
            if (subtract) {
               subs++;
               subtractgauss( parlist, sblo, sbhi, gridspac );
            }
         }
         for (i = 0; i < 7; i++) {
            parlist[i] = storelist[i];
         }
      }
   }
   return( subs );
}


static void onlysubtract( fchar Setin, fint subin,
                          fchar Setout, fint subout,
                          fint *blo, fint *bhi, fint buflen,
                          double *gridspac, FILE *paramreadfile )
{
   fint     cwlo, cwhi;
   fint     cwloO, cwhiO;
   fint     mcount;
   fint     tid, tidO;
   fint     pixelsread;
   fint     nsubsout;
   fint     nblanks;
   float    minval, maxval;
   char     readstr[130];
   fint     sblo[2], sbhi[2];
   float    major, minor;
   float    X0, Y0;
   float    angle;
   float    amplitude, zerolevel;
   int      subtract;
   int      subnum = 0;


   cwlo    = gdsc_fill_c( Setin, &subin, blo );
   cwhi    = gdsc_fill_c( Setin, &subin, bhi );
   /* Use input grid coordinates, but connect to output subsets */;
   cwloO   = gdsc_fill_c( Setout, &subout, blo );
   cwhiO   = gdsc_fill_c( Setout, &subout, bhi );
   mcount  = 0;
   tidO    = tid = 0;
   /* Read 'maxIObuf' values in 'image'. */
   (void) gdsi_read_c( Setin,
                       &cwlo, &cwhi,
                       image,
                       &buflen,
                       &pixelsread,
                       &tid );

   while (!feof(paramreadfile)) {
      readstr[0] = '\0';
      fgets( readstr, 120, paramreadfile);
      r1 = sscanf( readstr,
                   "%d %f %f %f %f %f %f %f %d %d %d %d",
                   &subtract, &amplitude, &major, &minor, &X0, &Y0,
                   &angle, &zerolevel, &sblo[0], &sblo[1], &sbhi[0], &sbhi[1] );

      if (r1 == 12) {
         if (subtract) {
            subnum++;
            sprintf( message, "Subtracting gauss number %d", subnum );
            (void) status_c( tofchar(message) );
            parlist[0] = amplitude;
            if (angle > 90.0) {
               parlist[1] = major;
               parlist[2] = minor;
               angle -= 90.0;
            } else {
               parlist[1] = minor;
               parlist[2] = major;
            }
            if ((degreeX) && (degreeY)) {
               parlist[1] /= 3600.0;
               parlist[2] /= 3600.0;
            }
            parlist[3] = ( X0 - (float)sblo[0] ) * (float)gridspac[0];
            parlist[4] = ( Y0 - (float)sblo[1] ) * (float)gridspac[1];
            parlist[5] = RAD(angle);
            parlist[6] = zerolevel;
            subtractgauss( parlist, sblo, sbhi, gridspac );
         }
      }
   }

   /* Calculate running min, max & blanks of output */
   minmax3_c( image,
              &pixelsread,
              &minval, &maxval,
              &nblanks,
              &mcount );
   pixelswrite = pixelsread;
   /* Write 'pixelswrite' values from 'image' to output. */
   (void) gdsi_write_c( Setout,
                        &cwloO, &cwhiO,
                        image,
                        &buflen,
                        &pixelswrite,
                        &tidO );
   /* Update OUTSET= descriptor for one subset with new values */
   change   = YES;
   nsubsout = 1;
   (void) wminmax_c( Setout, suboutA,
                     &minval, &maxval, &nblanks,
                     &nsubsout,
                     &change );
}



static void notes( int found, double cputime, double realtime,
                   bool toparamfile, char *paramfilename )
/*-------------------------------------------------------------------*/
/* Display status.                                                   */
/*-------------------------------------------------------------------*/
{
   char    border[80];
   int     l;

   anyoutC( 3, " " );
   l = sprintf( message,
               "FINDGAUSS subtracted %d gaussians in %.2f sec (%.2f cpu sec)",
                found, realtime, cputime );
   memset( border, '=', l ); border[l] = '\0';
   anyoutC( 3, border );
   anyoutC( 3, message );
   if (toparamfile) {
      sprintf( message, "Gauss parameters can be edited in file [%s]",
               paramfilename );
      anyoutC( 3, message );
   }
   anyoutC( 3, border );
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint    dfault;
   fint    nitems;
   int     found;


   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      (void) myname_c( Task );             /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );
   fmake( Setin, STRLEN );
   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   dfault  = NONE;
   subdim  = 0;
   showdev = 3;
   Key     = KEY_INSET;
   Mes     = MES_INSET;
   nsubs   = gdsinp_c( Setin,      /* Name of input set. */
                       subin,      /* Array containing subsets coordinate words. */
                       &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                       &dfault,    /* Default code as is USERxxx. */
                       Key,        /* Keyword prompt. */
                       Mes,        /* Keyword message for the user. */
                       &showdev,   /* Device number (as in ANYOUT). */
                       axnum,      /* Array of size 'maxaxes' containing the axes numbers. */
                                   /* The first elements (upto the dimension of the subset) */
                                   /* contain the axes numbers of the subset, */
                                   /* the other ones contain the axes numbers */
                                   /* outside the subset ordered according to the */
                                   /* specification by the user. */
                       axcount,    /* Number of grids on axes in 'axnum' */
                       &maxaxes,   /* Max. number of axes. */
                                   /* the operation for each subset. */
                       &class,     /* Class 1 is for applications which repeat */
                       &subdim );  /* Dimensionality of the subsets for class 1 */
   setdim  = gdsc_ndims_c( Setin, &setlevel );

   /*-------------------------------*/
   /* Determine edges of this frame */
   /*-------------------------------*/
   {
      fint cwlo, cwhi;                          /* Local coordinate words */
      int  m;
      r1 = 0;
      gdsc_range_c( Setin, &setlevel, &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++) {
         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
   }

   /*-----------------------*/
   /* Get the grid spacings */
   /*-----------------------*/

   for (i = 0; i < subdim; i++) {                    /* Append number to name */
      (void) sprintf( message, "CDELT%d", axnum[i] );
      r1 = 0;
      (void) gdsd_rdble_c( Setin, tofchar(message), &setlevel, &cdelt[i], &r1 );
      if (r1 < 0)  {
         r2 = 4;
         (void) error_c( &r2, tofchar( "No grid spacings in header of this set!") );
      }
      (void) sprintf( message, "CUNIT%d", axnum[i] );
      r1 = 0;
      /* Get the units of the axes */
      finit( cunit[i], 40 );
      (void) gdsd_rchar_c( Setin, tofchar(message), &setlevel, cunit[i], &r1 );
   }
   if (r1 < 0) strcpy( cunit[i].a, "?" );
   degreeX = ( strstr(cunit[0].a, "DEGREE") != NULL );
   degreeY = ( strstr(cunit[1].a, "DEGREE") != NULL );
   gridspac[0] = fabs(cdelt[0]);
   gridspac[1] = fabs(cdelt[1]);

   /*-------------------------------*/
   /* Prepare a box for INSET       */
   /*-------------------------------*/
   boxopt  = 0;
   showdev = 3;
   dfault  = REQUEST;
   Key     = KEY_BOX;
   Mes     = MES_BOX;
   gdsbox_c( blo, bhi, Setin, subin, &dfault,
             Key, Mes, &showdev, &boxopt );



   /*--------------------------------------------------------------*/
   /* Assign 'gdsinp' buffer to 'gdsout'. Output set will get same */
   /* coordinate system as input INSET=.  GDSOUT is a function     */
   /* which prompts the user to enter the name of a set and        */
   /* (optionally) subset(s) and returns the number of subsets     */
   /* entered.                                                     */
   /*--------------------------------------------------------------*/

   gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
   dfault  = NONE;
   showdev = 3;
   Key     = KEY_OUTSET;
   Mes     = MES_OUTSET;
   fmake( Setout, STRLEN );
   do {
      nsubsout = gdsout_c( Setout,        /* Name of the output set. */
                           suboutA,       /* Output array with subsets coordinate words.*/
                           &nsubs,        /* Maximum number of subsets in subout. */
                           &dfault,       /* Default code as in USERxxx. */
                           Key,           /* User keyword prompt. */
                           Mes,           /* Message for the user. */
                           &showdev,      /* Device number (as in ANYOUT). */
                           axnumout,      /* Array of size 'maxaxes' containing the axes numbers. */
                           axcountout,    /* Array with the axis sizes. */
                           &maxaxes );    /* Max axes the program can deal with. */
      agreed = (nsubsout == nsubs);
      if (!agreed) (void) reject_c( KEY_OUTSET, tofchar("#out != #in") );
   } while (!agreed);


   /*---------------------------------------------*/
   /* Allocate memory for a buffer to contain box */
   /*---------------------------------------------*/

   lenX = bhi[0] - blo[0] + 1;
   lenY = bhi[1] - blo[1] + 1;
   buflen = lenX * lenY;
   image  = (float *) calloc( (int) buflen, sizeof(float) );
   if (image == NULL) {
      fint   errlev = 4;
      anyoutC( 8, "FINDGAUSS: Try smaller box.");
      (void) error_c( &errlev, tofchar("Cannot allocate enough memory!"));
   }


   /*-------------------------------------------------------------*/
   /* If user wants subtraction with parameters from file instead */
   /* calculated parameters (SUBFILE=Y), a file name is asked for */
   /* each subset and after the subtraction the program stops.    */
   /*-------------------------------------------------------------*/

   subtractfromfile = toflog( NO );
   nitems = 1;
   dfault = REQUEST;
   Key    = tofchar("SUBFILE=");
   Mes    = tofchar("Use parameter file to subtract gaussians?      Y/[N]");
   r1     = userlog_c( &subtractfromfile,
                       &nitems,
                       &dfault,
                       Key,
                       Mes );
   subtractfromfile = tobool( subtractfromfile );


   /*--------------------------------------------------------------*/
   /* If parameters are read from file, subtract the gaussians and */
   /* stop Hermes.                                                 */
   /*--------------------------------------------------------------*/
   if (subtractfromfile) {
      for(subnr = 0; subnr < nsubs; subnr++) {
         Key = tofchar( "PARAMFILE=" );
         Mes = tofchar( "Name of file with stored parameters:      [Stop program]");
         paramreadfile = openfile( Key, Mes, readfilename, "r" );
         fromparam = (paramreadfile != NULL);
         if (fromparam) {
            onlysubtract( Setin, subin[subnr], Setout, suboutA[subnr],
                          blo, bhi, buflen, gridspac, paramreadfile );
         } else {
            free( image );
            finis_c();
            return(EXIT_SUCCESS);   /* Dummy return */
         }
      }
      fclose( paramreadfile );
      free( image );
      finis_c();
      return(EXIT_SUCCESS);   /* Dummy return */
   }


   /*----------------------------------------------------------------*/
   /* Ask for cutoff ratio to determine boundaries off the gaussian. */
   /*----------------------------------------------------------------*/

   cutoffratio = 1.0 / 100.0;
   dfault = HIDDEN;
   nitems = 1;
   Key = tofchar( "CUTOFF=" );
   Mes = tofchar( "Give cutoff ratio for gaussian  [1/100]" );
   r1 = userreal_c( &cutoffratio, &nitems, &dfault, Key, Mes );


   /*--------------------------------------------------------*/
   /* Instead of calculated boxes, the user can give his own */
   /* box size.                                              */
   /*--------------------------------------------------------*/

   nitems    = 2;
   dfault    = HIDDEN;
   fitlen[0] = fitlen[1] = -1;
   Key = tofchar("FITSIZE=");
   Mes = tofchar("Give size (x*y) of fit box:    [Calculated]");
   r1  = userint_c( fitlen, &nitems, &dfault, Key, Mes );



   /*----------------------------------------------------------------*/
   /* The array 'mask' is used to indicate whether pixels have been  */
   /* examined or not. There are pixels that need not to be examined */
   /* as possible candidates because the would result in a gaussian  */
   /* with an amplitude below a certain threshold, for example the   */
   /* noise in a map.                                                */
   /*----------------------------------------------------------------*/

   mask = (int *) calloc( (int) buflen, sizeof(int) );
   if (mask == NULL) {
      fint   errlev = 4;
      anyoutC( 8, "FINDGAUSS: Try smaller box.");
      (void) error_c( &errlev, tofchar("Cannot allocate enough memory!"));
   }
   nitems  = 1;
   dfault  = REQUEST;
   sigma   = blank;
   Key     = tofchar("SIGMA=");
   Mes     = tofchar("Rms of noise in map:    [None]");
   r1      = userreal_c( &sigma, &nitems, &dfault, Key, Mes );

   valrange[0] = blank;
   valrange[1] = blank;
   dfault      = REQUEST;
   nitems      = 2;
   Key         = tofchar( "VALRANGE=" );
   if (sigma != blank) {
      valrange[0] = 4.0 * sigma;
      Mes         = tofchar( "Range of values in search area:    [4*sigma, ->]");
   } else {
      Mes         = tofchar( "Range of values in search area:    [All values]");
   }
   r1 = userreal_c( valrange, &nitems, &dfault, Key, Mes );

   /*-----------------------------*/
   /* Give overview of parameters */
   /*-----------------------------*/

   fmake( bunit, 20 );
   r1 = 0;
   gdsd_rchar_c( Setin, tofchar( "BUNIT" ), &setlevel, bunit, &r1);
   if (r1 < 0) unknownunits = YES; else unknownunits = NO;
   if (unknownunits) {
      strcpy( bunit.a, "unknown units" );
      sprintf( message,
                  "Estimate list: 1) Amplitude (unknown map units)" );
   } else {
      sprintf( message,
                  "Estimate list: 1) Amplitude (%.*s)",
                   nelc_c(bunit), bunit.a );
   }
   anyoutC( 3, message );
   if (degreeX && degreeY) {
      anyoutC( 3, "               2) Full width at half maximum in x-direction (arcsec)");
      anyoutC( 3, "               3) Full width at half maximum in y-direction (arcsec)");
   } else {
      sprintf( message,
                  "               2) Full width at half maximum in x-direction (%.*s)",
                   nelc_c(cunit[0]), cunit[0].a );
      sprintf( message,
                  "               3) Full width at half maximum in y-direction (%.*s)",
                  nelc_c(cunit[1]), cunit[1].a );
   }
   anyoutC( 3,    "               4) Centre X in grids");
   anyoutC( 3,    "               5) Centre Y in grids");
   anyoutC( 3,    "               6) Angle between FWHMx and positive x-axis (degrees)");
   if (unknownunits) {
      sprintf( message,
                  "               7) Zero level (unknown map units)" );
   } else {
      sprintf( message,
                  "               7) Zero level (%.*s)", nelc_c(bunit), bunit.a );
   }
   anyoutC( 3, message );

   nitems = 7;
   for( i = 0; i < nitems; i++ ) mpar[i] = 1;
   dfault = REQUEST;
   Key    = tofchar( "MASK=" );
   Mes    = tofchar( "Parameter mask (0=fixed,1=free):     [1,1,1,1,1,1,1]");
   r1     = userint_c(  mpar, &nitems, &dfault, Key, Mes );

   tol    = 0.01;
   dfault = HIDDEN;
   Key    = tofchar("TOLERANCE=");
   Mes    = tofchar("Tolerance in fit:               [0.01]");
   nitems = 1;
   r1     = userreal_c( &tol, &nitems, &dfault, Key, Mes );

   lab    = 0.01;
   dfault = HIDDEN;
   Key    = tofchar("LAB=");
   Mes    = tofchar("Mixing parameter:               [0.01]");
   nitems = 1;
   r1     = userreal_c( &lab, &nitems, &dfault, Key, Mes );

   nitems = 7;
   for( i = 0; i < nitems; i++ ) parlist[i] = blank;

   sprintf( message, "Amplitude (%.*s):       [Calculated]",
            nelc_c(bunit), bunit.a);
   dfault = REQUEST;
   Key    = tofchar( "AMPLITUDE=" );
   Mes    = tofchar( message );
   nitems = 1;
   r1     = userreal_c( &parlist[0], &nitems, &dfault, Key, Mes );

   Key    = tofchar( "ANGLE=" );
   Mes    = tofchar( "Angle of FWHM in x wrt. pos. X-axis (deg):  [Calculated]");
   nitems = 1;
   r1     = userreal_c( &parlist[5], &nitems, &dfault, Key, Mes );
   if (parlist[5] != blank) parlist[5] = RAD(parlist[5]);  /* Convert to radians */


   /*--------------------------------------------------------------*/
   /* Read the beam as major and minor axis. The conversion to the */
   /* parameter list depends on the angle.                         */
   /*--------------------------------------------------------------*/
   {
      float    majmin[2];
      float    angle;


      Key    = tofchar( "BEAM=" );
      if (degreeX && degreeY) {
         Mes = tofchar( "Beam: major, minor: (arcsec):        [Calculated]");
      } else {
         sprintf( message, "Beam: major, minor: (%.*s x %.*s)",
                  nelc_c(cunit[0]), cunit[0].a, nelc_c(cunit[1]), cunit[1].a );
         Mes = tofchar( message );
      }
      angle  = parlist[5];
      if (angle != blank) angle = DEG(angle);
      nitems = 2;
      majmin[0] = majmin[1] = blank;
      r1     = userreal_c( majmin, &nitems, &dfault, Key, Mes );
      if (angle == blank) {
         parlist[1] = majmin[1];
         parlist[2] = majmin[0];
      } else {
         if (angle > 90.0) {
            parlist[1] = majmin[0];
            parlist[2] = majmin[1];
            angle -= 90.0;
            parlist[5] = RAD(angle);
         } else {
            parlist[1] = majmin[1];
            parlist[2] = majmin[0];
         }
      }
      if (degreeX && degreeY) {
         if (parlist[1] != blank) parlist[1] /= 3600.0;   /* Back to original units (degrees) */
         if (parlist[2] != blank) parlist[2] /= 3600.0;
      }
   }


   Key    = tofchar( "X0Y0=" );
   Mes    = tofchar( "Centre in grids:        [Calculated]");
   nitems = 2;
   r1     = userreal_c( &parlist[3], &nitems, &dfault, Key, Mes );
   /* Must be an offset wrt. lower left corner of box and in */
   /* physical coordinates */
   if (parlist[3] != blank) parlist[3] = (parlist[3] - blo[0]) * gridspac[0];
   if (parlist[4] != blank) parlist[4] = (parlist[4] - blo[1]) * gridspac[1];

   Key    = tofchar( "ZERO=" );
   Mes    = tofchar( "Zero level (map units):    [Calculated]");
   nitems = 1;
   r1     = userreal_c( &parlist[6], &nitems, &dfault, Key, Mes );

   clip[0]  = blank;
   clip[1]  = blank;
   dfault   = HIDDEN;
   nitems   = 2;
   Key      = tofchar( "CLIPLOHI=" );
   Mes      = tofchar("Give clip levels:     [No clip levels]");
   r1       = userreal_c( clip, &nitems, &dfault, Key, Mes );


   amprange[0] = blank;
   amprange[1] = blank;
   dfault      = HIDDEN;
   nitems      = 2;
   Key         = tofchar( "AMPRANGE=" );
   Mes         = tofchar( "Amplitude range to subtract:    [All amplitudes]");
   r1          = userreal_c( amprange, &nitems, &dfault, Key, Mes );

   beamrange[0] = blank;
   beamrange[1] = blank;
   dfault       = HIDDEN;
   nitems       = 2;
   Key          = tofchar( "BEAMRANGE=" );
   Mes          = tofchar( "Beam range to subtract:    [All values]");
   r1           = userreal_c( beamrange, &nitems, &dfault, Key, Mes );

   if (degreeX && degreeY) {
      if (beamrange[0] != blank) beamrange[0] /= 3600.0;   /* Back to original units (degrees) */
      if (beamrange[1] != blank) beamrange[1] /= 3600.0;
   }

   /* Ask name of table: */

   fmake( Tname, 10 );
   str2char( "GAUSS", Tname );
   dfault = HIDDEN;
   nitems = 1;
   Key    = tofchar("TABNAME=");
   Mes    = tofchar("Give name of table:      [GAUSS]");
   r1     = userchar_c( Tname, &nitems, &dfault, Key, Mes );


   /* parlist values are changing while fitting. To start a next fit */
   /* with the same user given initial estimates, store these values */

   for( i = 0; i < 7; i++ ) storelist[i] = parlist[i];
   its = MAXITERS;

   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/

   for(subnr = 0; subnr < nsubs; subnr++) {
      Key = tofchar( "PARAMFILE=" );
      Mes = tofchar( "Name of file to store parameters:              [No file]");
      paramwritefile = openfile( Key, Mes, writefilename, "w" );
      toparam = (paramwritefile != NULL);
      cwlo    = gdsc_fill_c( Setin, &subin[subnr], blo );
      cwhi    = gdsc_fill_c( Setin, &subin[subnr], bhi );
      /* Use input grid coordinates, but connect to output subsets */;
      cwloO   = gdsc_fill_c( Setout, &suboutA[subnr], blo );
      cwhiO   = gdsc_fill_c( Setout, &suboutA[subnr], bhi );
      mcount  = 0;
      tidO    = 0;
      tid     = 0;
      /*-----------------------------------------------------------*/
      /* Read 'buflen' values in 'image'. 'buflen' is equal to the */
      /* read area (box).                                          */
      /*-----------------------------------------------------------*/
      gdsi_read_c( Setin,
                   &cwlo, &cwhi,
                   image,
                   &buflen,
                   &pixelsread,
                   &tid );

      maskblank = 0;
      if (sigma == blank) {
         /*----------------------------------------------------------*/
         /* There is no sigma defined, so all pixels are candidates. */
         /*----------------------------------------------------------*/
         for (i = 0; i < pixelsread; i++) {
            mask[i] = 1;
         }
      } else {
         for (i = 0; i < pixelsread; i++) {
            mask[i] = 1;
            if (valrange[0] != blank) {
               if (image[i] < valrange[0]) mask[i] = 0;
            }
            if (valrange[1] != blank) {
               if (image[i] > valrange[1]) mask[i] = 0;
            }
            if (mask[i] == 0) maskblank++;
         }
      }

      elapse = 0;
      timer_c( &cputime, &realtime, &elapse );         /* Set the cpu timer */

      /*-----------------------------------------------------*/
      /* Do the search in the selected box in current subset */
      /*-----------------------------------------------------*/
      found = findandsubtract( blo, bhi, sigma, cutoffratio,
                               fitlen, tol, lab, its, clip,
                               parlist, storelist, mpar, gridspac,
                               amprange, beamrange, paramwritefile,
                               Setin, subin[subnr], Tname );

      elapse = 1;
      timer_c( &cputime, &realtime, &elapse );          /* Get the used cpu */
      notes( found, cputime, realtime, toparam, writefilename );    /* Info */

      /* Calculate running min, max & blanks of output */
      minmax3_c( image,
                 &pixelsread,
                 &minvalA[subnr], &maxvalA[subnr],
                 &nblanksA[subnr],
                 &mcount );
      pixelswrite = pixelsread;
      /* Write 'pixelswrite' values from 'image' to output. */
      gdsi_write_c( Setout,
                    &cwloO, &cwhiO,
                    image,
                    &buflen,
                    &pixelswrite,
                    &tidO );
      if (toparam) fclose( paramwritefile );
   } /* All subsets done ? */

   /* Update OUTSET= descriptor with new values */
   change = YES;
   wminmax_c( Setout, suboutA,
              minvalA, maxvalA, nblanksA,
              &nsubsout,
              &change );


   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   free( image );
   free( mask );
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
