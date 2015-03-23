/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             astrom.dc1

Program:       ASTROM

Purpose:       Determine the sky-projection parameters CRVAL, CDELT, 
               CRPIX and CROTA

Category:      FITTING, COORDINATES

File:          astrom.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 1.

** PROSYS=     Give projection system 1..10                    [header]
               If there is no reference to a projection system
               in the header, 'gnomonic (4)' is assumed. You can
               overrule the default projection with PROSYS=
                                           
   COORDSXY=   Give position _h_m_s _d_m_s :          [NO MORE COORDS.]
               This keyword is asked in a loop. A recall file can 
               be used to give the positions. The positions are given
               in hms dms format e.g.: 12h14m31.08s 47d31m50.3s or 
               in degrees e.g.: 183.6295 47.530639
               See also the description.

   GRIDXY=     n: Grid for the n-th position:
               Give longitude, latitude in grids. If SEARCHBOX= has
               at least one size greater than 1, this position is the 
               center of a 'search box' in which the position af the 
               local maximum is determined. If a size of the search box 
               is 1, the corresponding user given value of GRIDXY= is 
               returned.

   SEARCHBOX=  Give size of 'search' box:                       [21 21]
               Width and height of box in which a maximum 
               must be found. Each position in COORDSXY= must corres-
               pond to a grid given in GRIDXY= The value given in 
               GRIDXY= is the center of a 'search box' of size 
               SEARCHBOX= In this box the position of the maximum is
               determined and returned as the wanted grid position.
               If you want to use the grid position given in GRIDXY=
               itself, use SEARCHBOX=1 1

** DISPLAY=    Display found and fitted positions:                Y/[N]
               Write a table in the log file with the input physical 
               positions, the found grid positions of the local maximum
               (in the search box), the (output) fitted grid positions, 
               and the difference between found and fitted grid 
               positions.
               
                            
               ===== Keywords asked in the 'fit' loop: =====

               (The coordinate parameters are CRVAL, CROTA, CDELT and
               CRPIX. If a value could be found in the header, then
               this value will be the default initial estimate for the 
               fit. Else, there is no default.)

               
   CRVALX=     long. proj. centre:                       [header value]
               holds the physical coordinate at the reference pixel 
               location in some units.  The units are defined in the
               non-standard FITS descriptor CUNIT.
               
   CRVALY=     lat. proj. centre :                       [header value]

   CROTA=      rotation angle    :                       [header value]
               Holds the rotation angle in degrees of the actual axis.
               It is assumed that the spatial latitude axis carries the 
               rotation information.   
  
   CDELTX=     grid size x       :                       [header value]
               The pixel separation in units specified in the header 
               item CUNIT. Note that CDELT may be negative.
   
   CDELTY=     grid size y       :                       [header value]  
     
   CRPIXX=     ref. x grid pos.  :                       [header value]     
               CRPIX indicates the location of the reference pixel on
               an axis. The grid position of the reference pixel is 
               always  zero.  For this position the physical coordinate
               is precisely defined in CRVAL.
               
   CRPIXY=     ref. x grid pos.  :                       [header value]
              
   MASK=       Give mask for parms.0=fixed,1=free: [default by program]
               For all parameters where ASTROM could find an value in
               the header, 
               
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

   NEXT=       (Q)uit, (U)pdate header and quit, (R)epeat fit:      [Q]
               Fit again with different initial estimates or 
               parameters. The default 'Q" stops the program. 'R' gives 
               you the opportunity to change values and to fit again.
               i.e. NEXT=R MASK=1 1 1 CROTA=30
               'U' stops the program and updates the header with the 
               fitted values for CRVAL, CROTA, CDELT and CRPIX. 
               Additional remarks are stored in the history.
               
   REMOVE=     Give number(s) of position(s) to remove:          [NONE]n
               Exclude one or more positions by specifying the 
               position number(s) as displayed in the table 
               (DISPLAY=YES). e.g. REMOVE=2:5  will remove entries
               2 3 4 and 5 of your array with positions.


Description:   ASTROM determines coordinate parameters of the (sub)set
               specified in INSET=. The (sub)set must be 2-dimensional
               and must have spatial axes only. Your set can have 
               corrupted coordinate parameters like the physical 
               position of the reference pixel (CRVAL), a rotation angle 
               (CROTA), a grid spacing (CDELT) or the position of the 
               reference pixel (CRPIX, the pixel with grid coordinate 0). 
               Note that CRVAL and CRPIX are coupled. To fit these 
               parameters, you need a list of physical positions and
               the corresponding grids. If the physical position 
               represent sources in your map, then only an approximate
               grid position is needed. The program creates a box 
               with sizes SEARCHBOX= around your position and will find
               the position of the local maximum (using the values of
               the neighbouring pixels and a second degree
               polynomial). The physical coordinates are given in
               COORDSXY= You can use a recall file as input. 
               
               Longitude and latitude are entered in COORDSXY= as two 
               strings. Each string can consist of 3 (or less) numbers
               separated by a delimiter from the set 'dDhHmMsSrR'. They 
               mean the obvious: the degrees (or hours) are separated by 
               a 'd' ('h') from the minutes which in turn are separated 
               by an 'm' from the seconds. The seconds may be closed by 
               an 's'. Some examples:
               
               120d30m15s    ==>       120 + 30/60. + 15/3600.
               -60d12.5m     ==>      -( 60 + 12.5/60. )
               12h30         ==>       ( 12 + 30/60.            ) * 15
               12h30s        ==>       ( 12 +        + 30/3600. ) * 15
               12.3          ==>       12.3
               4r            ==>       4 * 180 / pi               
               
              
               The grids are entered in GRIDXY= For each physical position
               there must be a grid. The position input loop is aborted
               with carriage return. The calculated position can be 
               displayed in the log file if DISPLAY=Y. 
               
               If you specified all coordinates, you have to enter values 
               for the coordinate parameters. These values are used as initial 
               estimates for the fitting routine. If values could be found
               in the header of the set, then these values are the defaults.
               If all parameters are specified, i.e. CRVALX CRVALY= CROTA= 
               CDELTX= CDELTY= CRPIXX= CRPIXY= all have a value, it is 
               possible to set a mask with MASK=  Input for this keyword
               can be 0 or 1, 0 is a fixed parameter and 1 is a free parameter.
               e.g. MASK=1 1 1 1 1 0 0 means that you specified CRVALX CRVALY= 
               CROTA= CDELTX= and CDELTY= as free parameters and CRPIXX= and
               CRPIXY= as fixed parameters. The output is a list of values of
               the fitted parameters and the errors. The fit can be repeated
               if you give NEXT=R Together with this keyword you can change
               the initial estimates, the mask or the values for TOLERANCE=
               and LAB= These keywords are described in the keyword list.
               If you want to put the fitted values in your header, use
               NEXT=U. The items will be updated and some history is written
               like:
               
               HISTORY
               =======
               ASTROM,Mon Jun 21 17:26:19 1993 CRPIX2: 257 replaced by 257
               
               The program is ended with NEXT=Q (default).

Notes:         .......

Example:       

 <USER> ASTROM INSET=continuum 0
 ASTROM  Version 1.0  (Jun 21 1993) 
 Set continuum has 3 axes
 RA---NCP           from  -256 to   255
 DEC--NCP           from  -256 to   255
 PARAM-MEAN         from     0 to     0
 No rotation angle in header

 =========== SKYFIT parameters for set [continuum] ===========
 longitude projection centre (CRVAL): -176.667 DEGREE
 latitude projection centre (CRVAL) : 47.3333 DEGREE
 rotation angle (CROTA)             : unknown!
 grid size x grid (CDELT)           : -0.00138889 DEGREE
 grid size y grid (CDELT)           : 0.00188885 DEGREE
 reference x grid position (CRPIX)  : 257
 reference y grid position (CRPIX)  : 257

 Sky system:        equatorial, Epoch: 1950.0
 Projection system: north celestial pole (WSRT)
 <USER> ASTROM COORDSXY=<gf
 <USER> ASTROM GRIDXY=<cg
 <USER> ASTROM COORDSXY=
 Read 6 positions
 <USER> ASTROM SEARCHBOX=
 <USER> ASTROM CRVALX=
 <USER> ASTROM CRVALY=
 <USER> ASTROM CROTA=
 <USER> ASTROM CROTA=0
 <USER> ASTROM CDELTX=
 <USER> ASTROM CDELTY=
 <USER> ASTROM CRPIXX=
 <USER> ASTROM CRPIXY=
 <USER> ASTROM MASK=1 1 1 1 1

 ====PARAMETER=====================ESTIMATE==STATUS======FIT============ERROR====
 (CRVALX=) long. proj. centre:  -176.666700   free  -176.668040 +/-    0.0004682
 (CRVALY=) lat. proj. centre :    47.333330   free    47.332966 +/-    0.0004559
 ( CROTA=) rotation angle    :     0.000000   free     0.139005 +/-    0.0807541
 (CDELTX=) grid size x       :    -0.001389   free    -0.001391 +/-    0.0000021
 (CDELTY=) grid size y       :     0.001889   free     0.001878 +/-    0.0000051
 (CRPIXX=) ref. x grid pos.  :   257.000000  fixed   257.000000 +/-    0.0000000
 (CRPIXY=) ref. y grid pos.  :   257.000000  fixed   257.000000 +/-    0.0000000

 Number of iterations in fit: 2, TOLERANCE=0.01, mixing parameter LAB=0.01
 <USER> ASTROM NEXT=u
 <STATUS>  ASTROM   +++ FINISHED +++



Updates:       Jun 15,  1993: VOG, Document created.
               Aug 23,  1993: VOG, Program changed for new skyfit.
                                   New table with DISPLAY=Y
               Mar 29,  1994: VOG, Extended length of datum string

#<
*/

/*  astrom.c: include files     */

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
#include    "status.h"       /* Display additional information in the "RUNNING" status displ.*/
#include    "skyfit.h"
#include    "getdate.h"      /* Returns the current time and date as a text string */


/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"      
#include    "userreal.h"     
#include    "userdble.h"     
#include    "usertext.h"     
#include    "usercharu.h"    
#include    "userangle.h"
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
#include    "gdsd_wdble.h"
#include    "gdsd_wvar.h"
#include    "gds_extend.h"

/* axes related */

#include    "axunit.h"
#include    "axtype.h"
#include    "gdsc_name.h"


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
#define MAXSUBSETS        1            /* Max. allowed subsets */
#define MAXBUF         4096            /* Buffer size for I/O */
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0
#define FIXED          0
#define FREE           1
#define SKYPARAMS      7
#define MAXPOS         256


/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")

/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     nsubs;              /* Number of input subsets */
static fint     dfault;             /* Default option for input etc */
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


/* Axes related */

static	fchar	Ctype[MAXAXES];	    /* axis names */
static	fchar	Cunit[MAXAXES];	    /* axes units */
static  fchar   Nunit;              /* Natural units */
static  fchar   Dunit;              /* Secondary units */
static  fint    prosys[MAXAXES];    /* Projection system */
static  fint    skysys[MAXAXES];    /* Sky system */
static  fint    velsys;   
static  fint    axistype[MAXAXES];  /* Type of axis as a number */
static  double  crval[2], cdelt[2];
static  double  crpix[2];
static  double  crota;
static  double  epoch;


/* related to the lsq fit */

static double   fpar[SKYPARAMS];
static double   fparstore[SKYPARAMS];
static double   epar[SKYPARAMS];
static fint     mpar[SKYPARAMS];


/* Miscellaneous */

static fchar    Task;               /* Name of current task */
static fchar    Key, Mes;
static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[180];       /* All purpose character buffer. */
static char     message2[180];      /* All purpose character buffer. */
static int      i;                  /* Various counters. */
static bool     agreed;             /* Loop guard. */
static int      quit;               /* Loop guard. */
static fint     nitems;
static char     txt[SKYPARAMS][50];
static char     key[SKYPARAMS][20];
static int      dest;               /* Device for anyout routine */
static double   posx[MAXPOS], posy[MAXPOS];     /* Input physical positions */
static double   gridx[MAXPOS], gridy[MAXPOS];   /* Input grids */
static double   fgridx[MAXPOS], fgridy[MAXPOS]; /* The fitted grids */
static fint     numpos;
static fint     boxsize[2];
static float    image[MAXBUF];
static float    tol;
static float    lab;
static bool     display;
static fint     removs[MAXPOS];   


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



static void updateheader( fchar Setin, double *fpar, fint *axnum )
/*--------------------------------------------------------------*/
/* Put fitted parameters in header on top level. All the fitted */
/* parameters will be put in the header. The history is         */
/* updated for the changes.                                     */
/*--------------------------------------------------------------*/
{
   fint  axis;
   char  datum[60];
   char  history[120];
   fchar Date;
   
      
   /* Create date string e.g.: ASTROM,Mon Jun 21 17:08:40 1993 */
   
   fmake( Date, 40 );
   getdate_c( Date );    
   (void) sprintf( datum, 
                   "%.*s,%.*s", 
                   nelc_c(Task), Task.a, 
                   nelc_c(Date), Date.a );
   for (i = 0; i < 2; i++) {
      axis = axnum[i];      
 
      (void) sprintf( message, "CRVAL%d", axis );
      r1 = 0;         
      gdsd_wdble_c( Setin, tofchar(message), &setlevel, &fpar[i], &r1 );
      (void) sprintf( history, "%s %s: %g replaced by %g", 
                      datum, message, crval[i], fpar[i] );
      r1 = 0;                      
      gdsd_wvar_c( Setin, tofchar("HISTORY"), &setlevel, tofchar(history), &r1 );
  
      (void) sprintf( message, "CROTA%d", axis );
      r1 = 0;
      gdsd_wdble_c( Setin, tofchar(message), &setlevel, &fpar[2], &r1 );
      (void) sprintf( history, "%s %s: %g replaced by %g", 
                      datum, message, crota, fpar[2] );
      r1 = 0;                      
      gdsd_wvar_c( Setin, tofchar("HISTORY"), &setlevel, tofchar(history), &r1 );
      
      (void) sprintf( message, "CDELT%d", axis );
      r1 = 0;
      gdsd_wdble_c( Setin, tofchar(message), &setlevel, &fpar[3+i], &r1 );

      (void) sprintf( history, "%s %s: %g replaced by %g", 
                      datum, message, cdelt[i], fpar[3+i] );
      r1 = 0;                
      gdsd_wvar_c( Setin, tofchar("HISTORY"), &setlevel, tofchar(history), &r1 );
      
      (void) sprintf( message, "CRPIX%d", axis );
      r1 = 0;         
      gdsd_wdble_c( Setin, tofchar(message), &setlevel, &fpar[5+i], &r1 );
      r1 = 0;
      gds_extend_c( Setin, Ctype[i], &fpar[5+i], NULL, &r1 );
      if (r1 < 0) 
         anyoutC( 1, "GDS error in gds_extend after update of CRPIX");
    
      (void) sprintf( history, "%s %s: %g replaced by %g", 
                      datum, message, crpix[i], fpar[5+i] );
      r1 = 0;                      
      gdsd_wvar_c( Setin, tofchar("HISTORY"), &setlevel, tofchar(history), &r1 );      
   }
}



static int dofit( double *gridx, double *gridy, double *posx, double *posy,
                  fint numpos, double *fgridx, double *fgridy, double *fpar, 
                  double *epar, fint *mpar, fint prosys )
/*---------------------------------------------------------------------------*/
/* Do the actual fit.                                                        */
/*---------------------------------------------------------------------------*/
{               
   int    devnum = 3;
   fint   its = 100;   


   dfault = HIDDEN;
   nitems = 1;
   tol    = 0.01;   
   Key    = tofchar("TOLERANCE=");
   Mes    = tofchar("Tolerance in fit:               [0.01] ");
   r1     = userreal_c( &tol, &nitems, &dfault, Key, Mes );

   lab    = 0.01;
   Key    = tofchar("LAB=");
   Mes    = tofchar("Mixing parameter:              [0.01] ");
   r1     = userreal_c( &lab, &nitems, &dfault, Key, Mes );
   
   r1 = skyfit_c( gridx, gridy, posx, posy, &numpos, fgridx, fgridy, 
                  fpar, epar, mpar, &tol, &its, &lab, &prosys );
                  
   if (r1 >= 0) {
      return((int) r1);
   } else {
      switch( r1 ) {
         case -1 : {
            anyoutC( devnum, "No fit: Unknown projection." );
            break;
         }
         case -2 : {
            anyoutC( devnum, "No fit: No free parameters." );
            break;
         }
         case -3 : {
            anyoutC( devnum, "No fit: Not enough degrees of freedom." );
            break;
         }
         case -4 : {
            anyoutC( devnum, "No fit: Maximum number of iterations too small to" );
            anyoutC( devnum, "        obtain a solution which satisfies TOL." );
            break;
         }
         case -5 : {
            anyoutC( devnum, "No fit: Diagonal of matrix contains elements which are zero." );
            break;
         }
         case -6 : {
            anyoutC( devnum, "No fit: " );
            break;
         }
         case -7 : {
            anyoutC( devnum, "No fit: " );
            break;
         }
      }
      return(0);
   }
}                 


static void to2dim( int pos, int *x, int *y, int lx )
{
   *y = pos / lx;
   *x = pos - (*y) * lx;
}


static int to1dim( int x, int y, int lx )
{
   return( y*lx + x );
}



static void getposmax( double *xy, fchar Setin, fint subset, 
                       fint *flo, fint *fhi, fint *boxsize )
/*-------------------------------------------------------------------------*/
/* Construct a box centered around 'xy[0], xy[1]' with sizes 'boxsize[0] x */
/* boxsize[1]'. Part of the box must be contained in the frame of the      */
/* (sub)set. If this is not so, return without changing 'xy'. If the box   */
/* is within the frame, determine the position of the maximum and store    */
/* the values of the neighbouring pixels. Determine a more accurate        */
/* position with a second order polynomial. The polynomial is              */
/* Y=aX^2+bX+c and the values are (-1,y1), (0,y2) and (1,y3). Solving the  */
/* equation you can derive the position of the maximum:                    */
/* x = 1/2 * (y1=y3) / (y1-2y2+y3).                                        */
/* This value is added to the previous position of the maximum. If there   */
/* are no neighbouring pixels, add nothing to the previous position of the */
/* maximum. If the boxsize is 1, the value of 'xy' will be returned.       */
/*-------------------------------------------------------------------------*/
{

   fint   nread;
   fint   tid = 0;
   fint   cwlo, cwhi;
   fint   lx, ly;
   int    i,j;
   fint   maxdata;
   int    mp, k;
   fint   x0 = (fint) xy[0];
   fint   y0 = (fint) xy[1];
   double y1, y2, y3;


   /* Determine a box inside flo, fhi */
   blo[0] = MYMAX( flo[0], (x0 - boxsize[0]/2) );
   bhi[0] = MYMIN( fhi[0], (x0 + boxsize[0]/2) );
   blo[1] = MYMAX( flo[1], (y0 - boxsize[1]/2) );
   bhi[1] = MYMIN( fhi[1], (y0 + boxsize[1]/2) );
   if ((blo[0] > fhi[0]) || (bhi[0] < flo[0]) || 
       (blo[1] > fhi[1]) || (bhi[1] < flo[1])) 
   {
      anyoutC( 1, "Search box outside frame of (sub)set!" );
      return;
   }
   lx = bhi[0] - blo[0] + 1;
   ly = bhi[1] - blo[1] + 1;   
   if ((lx * ly) > MAXBUF) 
   {
      anyoutC( 1, "Search box too big!" );
      return;
   }

/*   (void) sprintf( message, "Box: %d %d %d %d", blo[0], blo[1], bhi[0], bhi[1] );
     anyoutC( 3, message );
*/   
   maxdata = lx * ly;
   cwlo = gdsc_fill_c( Setin, &subset, blo );
   cwhi = gdsc_fill_c( Setin, &subset, bhi );
   gdsi_read_c( Setin, &cwlo, &cwhi, image, &maxdata, &nread, &tid  );
   mp = 0;
   for (j = 0, k = 0; j < ly; j++) 
   {
      for (i = 0; i < lx; i++) 
      {
         if (image[k] > image[mp]) mp = k;
         k++;
      }
   }     
   to2dim( mp, &i, &j, lx );
/*  
   sprintf( message, "pos in array for max=%d, %f (%d %d)", mp, image[mp],i,j );
   anyoutC( 1, message );
*/
   if (boxsize[0] > 1)
   {
      if ((i-1) >= 0 ) 
         y1 = (double) image[to1dim(i-1,j,lx)]; 
      else 
         y1 = blank;
      y2 = (double) image[mp];
      if ((i+1) < lx) 
         y3 = (double) image[to1dim(i+1,j,lx)]; 
      else 
         y3 = blank; 
      if ((y1 != blank) && (y2 != blank) && (y3 != blank)) 
         xy[0] = (double) (blo[0]+i) + 0.5 * ( (y1-y3)/(y1-2.0*y2+y3) );
      else 
         xy[0] = (double) (blo[0]+i);
   }
/*
   (void) sprintf( message, "y123=%f %f %f, x=%f", y1,y2,y3,xy[0] );
   anyoutC( 1, message );
*/ 
   
   if (boxsize[1] > 1)
   {
      if ((j-1) >= 0 ) 
         y1 = (double) image[to1dim(i,j-1,lx)]; 
      else 
         y1 = blank;
      y2 = (double) image[mp];
      if ((j+1) < ly) 
         y3 = (double) image[to1dim(i,j+1,lx)]; 
      else 
         y3 = blank; 
      if ((y1 != blank) && (y2 != blank) && (y3 != blank)) 
         xy[1] = (double) (blo[1]+j) + 0.5 * ( (y1-y3)/(y1-2.0*y2+y3) );
      else 
         xy[1] = (double) (blo[1]+j);
   }
/*   
   (void) sprintf( message, "y123=%f %f %f, x=%f", y1,y2,y3,xy[1] );
   anyoutC( 1, message );
*/      
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fchar    choice;   
   fint     proj;
   
  
   init_c();                               /* contact Hermes */
   /* Task identification */
   {
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
   subdim  = 2;                    /* Dimension of subset must be 2! */
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
      for (m = 0; m < (int) setdim; m++) 
      {
         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
   }


   /* Allocate space for ARRAY of fchars (use finit, not fmake) */
   
   fmake( Nunit, KEYLEN );
   fmake( Dunit, KEYLEN );
   for (i = 0; i < subdim; i++) 
   {
      finit( Ctype[i], KEYLEN );      
      finit( Cunit[i], KEYLEN );

   }
   
  
   /* Assemble header information */
      
   crota = blank;       /* initialize rotation angle */
   for (i = 0; i < subdim; i++) 
   {
      fint     axis = axnum[i];      
      r1 = 0;
      gdsc_name_c( Ctype[i], Setin, &axis, &r1 );
      axunit_c( Setin, &axis, Cunit[i] );
      axistype[i] = axtype_c( Ctype[i],          
                              Nunit,             /* Natural axis units. */
                              Dunit,             /* Secondary axis units. */
                              &skysys[i],        /* If AXTYPE equals 1 or 2, the sky system id. */
                              &prosys[i],        /* if AXTYPE equals 1 or 2, the sky projection
                                                    system id: */
                              &velsys );         /* If AXTYPE equals 3 the velocity system id: */

      (void) sprintf( message, "CRVAL%d", axis );
      r1 = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crval[i], &r1 );
      if (r1 < 0) 
      {
         crval[i] = blank;
         anyoutC( 3,  "No CRVAL in header" );
      }
      (void) sprintf( message, "CDELT%d", axis );
      r1 = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &cdelt[i], &r1 );
      if (r1 < 0) 
      {
         cdelt[i] = blank;
         anyoutC( 3, "No CDELT in header" );
      }
      (void) sprintf( message, "CRPIX%d", axis );
      r1 = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crpix[i], &r1 );
      if (r1 < 0) 
      {
         crpix[i] = blank;
         anyoutC( 3, "No CRPIX in header" );
      }                  

      /* Is there a rotation angle attached to the spatial axis latitude? */
      (void) sprintf( message, "CROTA%d", axis );
      r1 = 0;      
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crota, &r1 );
      if (axistype[i] == 2) 
      {
         if (r1 < 0) 
         {
            crota = blank;
            anyoutC( 3, "No rotation angle in header" );
         }
      }                        
   }
     
   /* Display what is known */
   anyoutC( 3, " " );
   (void) sprintf( message, 
                   "=========== SKYFIT parameters for set [%.*s] ===========",
                   nelc_c( Setin ), Setin.a );
   anyoutC( 3, message );
   if (crval[0] != blank) 
   {
      (void) sprintf( message, "longitude projection centre (CRVAL): %g %.*s", 
                      crval[0], nelc_c(Cunit[0]), Cunit[0].a );
   } 
   else 
   {
      (void) sprintf( message, 
      "longitude projection centre (CRVAL): unknown!" );      
   }
   anyoutC( 3, message );
   if (crval[1] != blank) 
   {
      (void) sprintf( message, "latitude projection centre (CRVAL) : %g %.*s", 
                      crval[1], nelc_c(Cunit[1]), Cunit[1].a );   
   } 
   else 
   {
      (void) sprintf( message, "latitude projection centre (CRVAL) : unknown!" );
   }
   anyoutC( 3, message );
   if (crota != blank) 
   {   
      (void) sprintf( message, "rotation angle (CROTA)             : %g DEGREE", crota );
   } 
   else 
   {
      (void) sprintf( message, "rotation angle (CROTA)             : unknown!" ); 
   }
   anyoutC( 3, message );
   if (cdelt[0] != blank) 
   {
      (void) sprintf( message, "grid size x grid (CDELT)           : %.3g %.*s", 
                      cdelt[0], nelc_c(Cunit[0]), Cunit[0].a );
   } 
   else 
   {
      (void) sprintf( message, "grid size x grid (CDELT)           : unknown!" );
   }
   anyoutC( 3, message );
   if (cdelt[1] != blank) 
   {
      (void) sprintf( message, "grid size y grid (CDELT)           : %.3g %.*s", 
                      cdelt[1], nelc_c(Cunit[1]), Cunit[1].a );   
   } 
   else 
   {
      (void) sprintf( message, "grid size y grid (CDELT)           : unknown!" );
   }
   anyoutC( 3, message );
   if (crpix[0] != blank) 
   {
      (void) sprintf( message, "reference x grid position (CRPIX)  : %g", crpix[0] );
   } 
   else 
   {
      (void) sprintf( message, "reference x grid position (CRPIX)  : unknown!" );
   }
   anyoutC( 3, message );
   if (crpix[1] != blank) 
   {
      (void) sprintf( message, "reference y grid position (CRPIX)  : %g", crpix[1] );
   } 
   else 
   {      
      (void) sprintf( message, "reference y grid position (CRPIX)  : unknown!" );      
   } 
   anyoutC( 3, message );
   anyoutC( 3, " " );
   
   agreed = ( (axistype[0] == 1) || (axistype[0] == 2) );
   
   if (!agreed) 
      anyoutC( 3, "Cannot do a sky fit because first axis is not spatial" );
   if (agreed) 
   {   
      agreed = ( (axistype[1] == 1) || (axistype[1] == 2) );
      if (!agreed) 
         anyoutC( 3, "Cannot do a sky fit because second axis is not spatial" );
   }
   if (agreed) 
   {
      agreed = (axistype[0] != axistype[1]);
      if (!agreed) 
      anyoutC( 3, "Cannot do a sky fit because axes have same direction" );
   }
   if (agreed) 
   {
      agreed = (skysys[0] == skysys[1]);   
      if (!agreed) 
      {
         anyoutC( 3, "Cannot do a sky fit because axes have different sky systems" );
      } 
      else 
      {
         if      (skysys[0] == 1) (void) sprintf( message, "Sky system:        equatorial" );
         else if (skysys[0] == 2) (void) sprintf( message, "Sky system:        galactic" );
         else if (skysys[0] == 3) (void) sprintf( message, "Sky system:        ecliptic" );
         else if (skysys[0] == 4) (void) sprintf( message, "Sky system:        supergalactic" );         
         if (skysys[0] == 1) 
         {
            r1 = 0;
            gdsd_rdble_c( Setin, tofchar("EPOCH"), &setlevel, &epoch, &r1 );
            if (r1 >= 0) 
            {
               (void) sprintf( message, "%.*s, Epoch: %.1f",
                               strlen(message), message, epoch );      
            }
         }
         anyoutC( 3, message );
      }
   }
   if (!agreed) 
   {
      /* No use to go on... */
      finis_c();
      return(EXIT_SUCCESS);
   }
   
   /* Now it must have some projection system */
   agreed = (prosys[0] == prosys[1]);
   if (!agreed) 
   {
      anyoutC( 3, "Axes have different projection systems, I take projection of first axis" );
   } 
   dfault = HIDDEN;
   do 
   {
      proj   = prosys[0];
      nitems = 1;
      Key    = tofchar("PROSYS="), 
      r1     = userint_c( &proj, &nitems, &dfault, Key, 
                          tofchar("Give projection system 1..10     [header value]") );
      agreed = ((proj >= 1) && (proj <= 10) );
      if (!agreed) 
      {
         reject_c( Key, tofchar("Must be between 1 & 10") );
         dfault = REQUEST;         
      }
   } while (!agreed);
   prosys[0] = proj;
   
   /* Display which projection system is in use */
   if      (prosys[0] == 1)  anyoutC( 3, "Projection system: AITOFF equal area" );
   else if (prosys[0] == 2)  anyoutC( 3, "Projection system: equivalent cylindrical" );
   else if (prosys[0] == 3)  anyoutC( 3, "Projection system: flat" );
   else if (prosys[0] == 4)  anyoutC( 3, "Projection system: gnomonic" );
   else if (prosys[0] == 5)  anyoutC( 3, "Projection system: orthographic" );
   else if (prosys[0] == 6)  anyoutC( 3, "Projection system: rectangular" );
   else if (prosys[0] == 7)  anyoutC( 3, "Projection system: global sinusoidal" );
   else if (prosys[0] == 8)  anyoutC( 3, "Projection system: north celestial pole (WSRT)" );
   else if (prosys[0] == 9)  anyoutC( 3, "Projection system: stereographic" );
   else if (prosys[0] == 10) anyoutC( 3, "Projection system: mercator projection" );


   /* Define the keywords for the parameters */          
   strcpy( key[0], "CRVALX= " );
   strcpy( key[1], "CRVALY=" );
   strcpy( key[2], "CROTA=" );
   strcpy( key[3], "CDELTX=" );
   strcpy( key[4], "CDELTY=" );
   strcpy( key[5], "CRPIXX=" );
   strcpy( key[6], "CRPIXY=" );
                    
   /* We need to pre specify the 7 skyfit parameters as initial estimates */
   (void) sprintf( txt[0], "(%7.7s) long. proj. centre:", key[0] );
   (void) sprintf( txt[1], "(%7.7s) lat. proj. centre :", key[1] );
   (void) sprintf( txt[2], "(%7.7s) rotation angle    :", key[2] );
   (void) sprintf( txt[3], "(%7.7s) grid size x       :", key[3] );
   (void) sprintf( txt[4], "(%7.7s) grid size y       :", key[4] );
   (void) sprintf( txt[5], "(%7.7s) ref. x grid pos.  :", key[5] );
   (void) sprintf( txt[6], "(%7.7s) ref. y grid pos.  :", key[6] );   


   /* Loop over physical positions */
   nitems = 2;
   i      = 0;   
   Mes = tofchar("Give position ..h/d..m..s ..d..m..s :   [NO MORE COORDS.]" );
   do {
      do 
      {
         double   dumxy[2];
         dfault = REQUEST;         
         Key    = tofchar("COORDSXY=");         
         r1     = userangle_c( dumxy, &nitems, &dfault, Key, Mes );
         if (r1 == 1) 
         {
            reject_c( Key, tofchar("Need 2 coordinates!") );
         }
         if (r1 == 2) 
         {
            if (i < MAXPOS) 
            {
               posx[i] = dumxy[0];
               posy[i] = dumxy[1];
               dfault  = EXACT;
               (void) sprintf( message, "(%d) Give grid for coord. (%g %g) deg.:", 
                               i, dumxy[0], dumxy[1]);
               r2      = userdble_c( dumxy, &nitems, &dfault,
                                     tofchar("GRIDXY="),
                                     tofchar(message) );               
               gridx[i] = dumxy[0];
               gridy[i] = dumxy[1];                      
               cancel_c( tofchar("GRIDXY=") );
               i++;
            } 
            else 
            {
               (void) sprintf( message, "More than %d positions entered!" );
               anyoutC( 3, message );
            }
         }
         cancel_c( Key );
      } while (r1 == 1);      
   } while ((r1 == 2) && (i < MAXPOS));
   
   numpos = (fint) i;   
   if (numpos == MAXPOS) 
   {
      (void) sprintf( message, "Read %d (=max) positions", numpos );
   } 
   else 
   {
      (void) sprintf( message, "Read %d positions", numpos );
   }
   anyoutC( 3, message );
   

   /* Ask for the sizes of a search box */
   
   boxsize[0] = boxsize[1] = 21;
   nitems     = 7;
   dfault     = REQUEST;
   Key        = tofchar("SEARCHBOX=");
   (void) sprintf( message, "Give size of 'search' box:   [%d %d]", 
                   boxsize[0], boxsize[1] );
   r1         = userint_c( boxsize, &nitems, &dfault, Key, tofchar(message) );
   boxsize[0] = MYMAX( boxsize[0], 1 );
   boxsize[1] = MYMAX( boxsize[1], 1 );
 

   
   /* Get the more accurate grid positions */
   for (i = 0; i < (int) numpos; i++) 
   {    
      double     dumxy[2];
      dumxy[0] = gridx[i];
      dumxy[1] = gridy[i];      
      getposmax( dumxy, Setin, subin[0], flo, fhi, boxsize );      
      gridx[i] = dumxy[0];      
      gridy[i] = dumxy[1];
   }  

 
   fmake( choice, 1 );
   do 
   {
      nitems = 1;
      fpar[0] = crval[0];
      fpar[1] = crval[1];
      fpar[2] = crota;
      fpar[3] = cdelt[0];
      fpar[4] = cdelt[1];
      fpar[5] = crpix[0];
      fpar[6] = crpix[1];
      
      for (i = 0; i < SKYPARAMS; i++) 
      {
         if (fpar[i] != blank) 
         {
            (void) sprintf( message, "%s [%g]", txt[i], fpar[i] );
            dfault = REQUEST;
            mpar[i] = FIXED;
         } 
         else 
         {
            (void) sprintf( message, "%s", txt[i] );
            dfault = NONE;
            mpar[i] = FREE;
         }
         r1 = userdble_c( &fpar[i], &nitems, &dfault, tofchar( key[i] ), tofchar( message ) );
      }
      
      /* Ask for mask (free or fixed parameters in fit) */      
      nitems = 7;
      dfault = REQUEST;
      Key    = tofchar("MASK="); 
      (void) sprintf( message, "Give mask for parms. 0=fixed, 1=free: [%d %d %d %d %d %d %d]",
                      mpar[0], mpar[1], mpar[2], mpar[3], mpar[4], mpar[5], mpar[6] );
      r1 = userint_c( mpar, &nitems, &dfault, Key, tofchar(message) );
      
      for (i = 0; i < SKYPARAMS; i++) 
      {
         fparstore[i] = fpar[i];
      }

      /* Do the fit. Fit is wrt. to 0,0 , adjust crpix first */
      fpar[5] -= crpix[0];
      fpar[6] -= crpix[1];
      r1 = dofit( gridx, gridy, posx, posy, numpos, fgridx, fgridy, 
                  fpar, epar, mpar, prosys[0] );
      agreed = r1;
      if (agreed) 
      {  
         /* Re-adjust crpix */
         fpar[5] += crpix[0];
         fpar[6] += crpix[1];
         /* Create an output table */
         dest = 3;
         anyoutC( dest, " " );
         anyoutC( dest, 
         "====PARAMETER=====================ESTIMATE==STATUS======FIT============ERROR====" );
         for (i = 0; i < SKYPARAMS; i++) 
         {
            if (mpar[i]) 
               strcpy ( message2, "free" );
            else 
               strcpy ( message2, "fixed" );
            (void) sprintf( message, "%s %12.6f  %5.5s %12.6f +/- %12.7f", 
                            txt[i], fparstore[i], message2, fpar[i], epar[i] );
            anyoutC( dest, message );
         } 
         anyoutC( dest, " " );
         (void) sprintf( message, 
               "Number of iterations in fit: %d, TOLERANCE=%g, mixing parameter LAB=%g", 
                r1, tol, lab );
         anyoutC( dest, message );         
         anyoutC( dest, " " );      
         
         display = toflog( NO );
         dfault  = HIDDEN;
         nitems  = 1;
         Key     = tofchar("DISPLAY=");
         Mes     = tofchar("Display found and fitted positions:        Y/[N]");
         r1      = userlog_c( &display, &nitems, &dfault, Key, Mes );
         display = tobool( display );
        
         if (display) 
         {
            int sl;
            (void) sprintf( message, 
            "INDX|     PHYSICAL    |  POS. MAX. IN SBOX  |   FITTED POSITION   |       DIFFERENCE      |" );
            anyoutC( dest, message );
            sl = sprintf( message, 
            "  # |  long  |   lat  |   grid x |   grid y |   grid x |   grid y |   delta x |   delta y |" );
            anyoutC( dest, message );
            memset( message, '-', sl );
            anyoutC( dest, message );
            for (i = 0; i < (int) numpos; i++) 
            {
               (void) sprintf( message, 
                               "%4d|%8g|%8g|%+10.3f|%+10.3f|%+10.3f|%+10.3f|%11.4f|%11.4f|", 
                               i, posx[i], posy[i], gridx[i], gridy[i], fgridx[i], fgridy[i],
                               gridx[i]-fgridx[i], gridy[i]-fgridy[i] );
               anyoutC( dest, message );
            }
         }
      }
        
      /* Quit loop or repeat fit */
      dfault      = REQUEST;
      nitems      = 1;
      quit        = NO;
      choice.a[0] = 'Q';
      Key = tofchar("NEXT=");
      Mes = tofchar("(Q)uit, (U)pdate header and quit, (R)epeat fit:  [Q]");
      r1  = usercharu_c( choice, &nitems, &dfault, Key, Mes );
     
      if (choice.a[0] == 'Q') 
         quit = YES;
      if (choice.a[0] == 'U') 
      {
         quit = YES;
         status_c( tofchar("Updating the header") );
         updateheader( Setin, fpar, axnum );
      }
      cancel_c( Key );
      /* User wants to go on. Give possibility to remove one or more positions */
      if (!quit) 
      {
         int k,n;
         dfault = REQUEST;
         nitems = numpos;
         Key = tofchar("REMOVE=");
         Mes = tofchar("Give number(s) of position(s) to remove:   [NONE]");
         r1  = userint_c( removs, &nitems, &dfault, Key, Mes );
         cancel_c( Key );
         if (r1 > 0) 
         {
            for (k = 0; k < r1; k++) 
            {
               if ((removs[k] >= 0) && (removs[k] < numpos)) 
                  posx[removs[k]] = blank;
            }
            for (n = 0, k = 0; n < numpos; n++) 
            {
               if (posx[n] != blank) 
               {
                  posx[k]  = posx[n];
                  posy[k]  = posy[n];
                  gridx[k] = gridx[n];
                  gridy[k] = gridy[n];
                  k++;
               }
            }
            numpos = k;            
         }         
      }
   } while (!quit);
                        
   


   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
