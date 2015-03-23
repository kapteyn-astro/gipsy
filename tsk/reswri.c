/* reswri.c

        Copyright (c) Kapteyn Astronomical Institute 1995
        All Rights Reserved.

#>            reswri.dc1

Program:      reswri

Purpose:      RESWRI derives the kinematical parameters from the observed
              velocity field by fitting tilted-rings to the velocity field
              and fits harmonic terms after convergence of the ROTCUR part of
              code. RESWRI uses the 1 sigma error-field to calculate its
              errors.

Category:     CALCULATION, DYNAMICS, ROTATION CURVES, VELOCITY FIELDS

File:         reswri.c

Author:       K.G. Begeman 
              ROTCUR modified to RESWRI by R.H.M Schoenmakers

Keywords:

   INSET=     Set (and subset) of observed velocity field.

** INSET2=    Input set (and subset) of total HI field.

   OUTSET=    Output set (and subset) of residual of residual:   [none]
   
   BOX=       Select area of velocity field [entire velocity field].

   BUNIT=     If the map units are not found in the header, they must be
              supplied by the user [KM/S].

   RADII=     Give central radii of concentric rings. Maximum number of rings
              is 512. Units are arcsec.

   WIDTHS=    Give width of rings. If number of widths is less than the
              number of radii, the last supplied width will be used for the
              rest of the rings. Units are arcsec.

   VSYS=      Give initial estimate of systemic velocity in km/s.

   VROT=      Initial estimate(s) rotation velocity(ies) in km/s.
              If the number of rotation velocities is less than the number
              of rings, the last supplied rotation velocity will be used
              for the rest of the rings.

   VEXP=      Initial estimate(s) expansion velocity(ies) in km/s [0.0].
              If the number of expansion velocities is less than the number
              of rings, the last supplied expansion velocity will be used
              for the rest of the rings.

   PA=        Initial estimate(s) position angle(s) in degrees.
              If the number of position angles is less than the number
              of rings, the last supplied position angle will be used
              for the rest of the rings.

   INCL=      Initial estimate(s) inclination(s) in degrees.
              If the number of inclinations is less than the number
              of rings, the last supplied inclination will be used
              for the rest of the rings.

   CENTRE=    Initial estimates of centre of rotation in any coordinates.

   FREEANGLE= Free angle around minor axis:                   [0.0]
              Angle around minor axis in degrees within which radial
              velocities are discarded. For RESWRI this value is 
              usually 0.0.

   SIDE=      Which half of the velocity field should be used in the
              fitting [RECEDING and APPROACHING half].

   WEIGHT=    Weighting function:                          [UNIFORM]
              For RESWRI always use UNIFORM !!          
              There are three weighting functions available: the
              UNIFORM weighting function, were all points in a
              ring have equal weights, the [COSINE] weighting function,
              were each point in a ring is weighted with |cos(theta)|,
              and the COS-SQUARED weighting function, were each point is
              weighted with cos(theta)^2.

   FIXED=     Which parameter(s) should be kept fixed [NONE]. The parameters
              are named VSYS, VROT, PA, INCL, XPOS and YPOS. If you don't
              want to fit the inclination and the systemic velocity, you
              should type: INCL VSYS. If a fit is wanted to only one half of
              the velocity field the parameters VSYS, XPOS and YPOS are
              automatically kept fixed.

** TOLERANCE= Tolerance of least-squares fitting [0.001].

   FILENAME=  Name of text file to save results [reswri**]. Default the
              name of the file is equal to the name of the table.

   FILECOEFF= Filename kinematic harmonic coeff.:      [coefficients.txt]
              File on disk with kinematic harmonic coefficients.

   FILEHICOEF= Filename surface density harmonic coeff.:      [hicoef.txt]
              File on disk with surface density harmonic coefficients.
              Prompted only if INSET2= was specified.



Description:  This program does a least-squares-fitting with errors on the value
              of v(x,y) to the function:

              v(x,y) = VSYS + VROT * cos(theta) * sin(INCL)
                            + VEXP * sin(theta) * sin(INCL)

                               - (x-XPOS) * sin(PA) + (y-YPOS) * cos(PA)
              with: cos(theta) = -------------------------------------------
                                                      r

                               - (x-XPOS) * cos(PA) - (y-YPOS) * sin(PA)
              and:  sin(theta) = ---------------------------------------------
                                                r * cos(INCL)

              Here  v(x,y)  denotes the radial velocity at rectangular sky
              coordinates x and y, VSYS the systemic velocity, VROT the
              rotational velocity, VEXP the expansion velocity, INCL the
              inclination angle and theta the azimuthal distance from the
              major axis in the plane of the galaxy. Theta is a function of
              the inclination (INCL) and the position angle (PA) of the
              major axis. XPOS and YPOS denote the position of the rotation
              centre.
              This program will fit for each ring the parameters VSYS, VROT,
              VEXP, INCL, PA, XPOS and YPOS.
              Note that the position angle (PA) of the major axis is
              defined as the angle, taken in anti-clockwise direction
              between the north direction on the sky and the major axis of
              the receding half of the galaxy.
              When this fitting procedure has converged, the program fits a
              basis a(0) + SUM(i=1,fitdeg) a(i)cos(i*theta) + b(i)sin(i*theta)
              to the points with angle theta and velocity v(x,y).
              The code uses a singular value decomposition fitting routine
              to do this (taken from Numerical Recipes, Press et al.).


Notes:        The results of the fitting are saved in a GDS table reswri**,
              where ** is a number from 1 to 99. If table reswri99 is
              already present, table reswri00 will be used. The results
              are also stored in a file named reswri.** or as defined
              by the user (keyword FILENAME=).
              Note also that the parts of the code between
                   "================================="
              -type of comments have been added by RHS to the original
              ROTCUR-code.


Example:      <USER> reswri
              reswri  Version 1.2  (Nov 24 1998)
              <USER> RESWRI INSET=drop 2 inset2=drop 1
              Set drop has 3 axes
              RA-NCP             from  -134 to   115
              DEC-NCP            from  -116 to   135
              PARAM-GAUSSFIT     from     1 to    11
              Set drop has 3 axes
              RA-NCP             from  -134 to   115
              DEC-NCP            from  -116 to   135
              PARAM-GAUSSFIT     from     1 to    11
              <USER> RESWRI BOX=
              BOX range for set drop :
              RA-NCP             from  -134 to   115
              DEC-NCP            from  -116 to   135
              <USER> RESWRI OUTSET=martintest
              Set martintest has 3 axes
              RA-NCP             from  -134 to   115
              DEC-NCP            from  -116 to   135
              PARAM-GAUSSFIT     from     2 to     2
              <USER> RESWRI VSYS=600
              <USER> RESWRI VROT=150
              <USER> RESWRI VROT=
              <USER> RESWRI VEXP=
              <USER> RESWRI PA=216
              <USER> RESWRI PA=
              <USER> RESWRI INCL=70
              <USER> RESWRI INCL=
              <USER> RESWRI CENTRE=8 -9
              <USER> RESWRI FREEANGLE=
              <USER> RESWRI SIDE=
              <USER> RESWRI WEIGHT=
              <USER> RESWRI FIXED=vexp xpos ypos
              <USER> RESWRI FITORDER=9
              <USER> RESWRI FILECOEFF=
              <USER> RESWRI OVERWRITE=y
              <USER> RESWRI FILEHICOEFF=
              <USER> RESWRI OVERWRITE=y
              <USER> RESWRI FILEHICOEFF=
              <USER> RESWRI OVERWRITE=y
              radius of ring:   80.00 arcsec
              iter. systemic rotation position incli- points    sigma
              number velocity velocity   angle  nation         velocity
              0     600.00   150.00   216.00  70.00    102      212.
              1     789.76    29.35   254.30  53.84    165      31.0
              2     766.41    41.63   255.90  44.94    196      21.0
              3     763.00    36.60   253.60  56.48    166      20.6
              4     763.08    36.61   253.69  56.45    168      20.5
              5     763.08    36.61   253.69  56.45    168      20.5

              ........

              RESWRI wrote kinematic harmonic coeff. in file [coefficients.txt]
              RESWRI wrote surface density harmonic coeff. in file [hicof.txt]
              <USER> RESWRI FILENAME=
              Results will be stored in TABLE reswri00 and FILE reswri.00
             
              The output file in FILECOEFF= (default coefficients.txt) is an 
              ASCII file with the following columns:
              radius(arcsec) c0, error c0, 
              c1, error c1, s1, error s1, 
              c2, error c2, s2, error s2,
              c3, error c3, s3, error s3, etc...
              
              All c and s values are in Km/s. Use your own plot program
              (GPLOT, sm, etc.) to visualize this data.
             


Updates:      Jul 17, 1992: KGB, Document created.
              Mar 20, 1993: KGB, fitting one half of vfield repaired.
              Mar 23, 1993: KGB, fitting of expansion velocities.
              Jan 13, 1994: KGB, keyword FILENAME= added.
              May 20, 1994: VOG, MAXRING increased from 60 to 512
              Feb 15, 1995: RHS, Fitting of higher order harmonic terms
              Nov 24, 1998: RHS, Added fitting of harmonic terms
                                 and renamed modified ROTCUR to RESWRI.
              Nov 24, 1998: VOG, Installed RESWRI with new filename
                                 keywords FILECOEFF= and FILEHICOEFF=
              Apr 15, 2009: VOG, -Changed definition of nint() to use
                                 floor() for the nearest integer.
                                 Change was necessary for consistency 
                                 with other coordinate routines.
                                 -Change '|' to '||' in if statement
#<

*/


/*
 * Includes:
 */

#include        "float.h"               /* <float.h> */
#include        "math.h"                /* <math.h> */
#include        "stddef.h"              /* <stddef.h> */
#include        "stdio.h"               /* <stdio.h> */
#include        "stdlib.h"              /* <stdlib.h> */
#include        "string.h"              /* <string.h> */
#include        "time.h"                /* <time.h> */
#include        "gipsyc.h"              /* GIPSY definitions */
#include        "cmain.h"               /* C programme */
#include        "anyout.h"              /* anyout_c */
#include        "axtype.h"              /* axtype_c */
#include        "cancel.h"              /* cancel_c */
#include        "error.h"               /* error_c */
#include        "factor.h"              /* factor_c */
#include        "finis.h"               /* finis_c */
#include        "gdsa_crecol.h"         /* gdsa_crecol_c */
#include        "gdsa_tabinq.h"         /* gdsa_tabinq_c */
#include        "gdsa_wcint.h"          /* gdsa_wcint_c */
#include        "gdsa_wrcom.h"          /* gdsa_wrcom_c */
#include        "gdsa_wcreal.h"         /* gdsa_wcreal_c */
#include        "gdsbox.h"              /* gdsbox_c */
/* =========================================== */
#include        "gdsasn.h"
#include        "gdsout.h"
#include        "gdsi_write.h"
/* =========================================== */
#include        "gdsc_fill.h"           /* gdsc_fill_c */
#include        "gdsc_name.h"           /* gdsc_name_c */
#include        "gdsd_rchar.h"          /* gdsd_rchar_c */
#include        "gdsd_rdble.h"          /* gdsd_rdble_c */
#include        "gdsd_wchar.h"          /* gdsd_wchar_c */
#include        "gdsinp.h"              /* gdsinp_c */
#include        "gdsi_read.h"           /* gdsi_read_c */
#include        "gdspos.h"              /* gdspos_c */
#include        "init.h"                /* init_c */
#include        "lsqfit.h"              /* lsqfit_c */
#include        "match.h"               /* match_c */
#include        "nelc.h"                /* nelc_c */
#include        "setfblank.h"           /* setfblank_c */
#include        "status.h"              /* status_c */
#include        "userchar.h"            /* userchar_c */
#include        "usercharu.h"           /* usercharu_c */
#include        "userreal.h"            /* userreal_c */
#include        "usertext.h"            /* usertext_c */
#include        "userint.h"             /* userint_c */
#include        "userlog.h"
#include        "reject.h"
#include        "userfio.h"
#include        "matrix.h"

/*
 * Keywords and messages:
 */

#define KEY_BOX         tofchar("BOX=")
#define KEY_BUNIT       tofchar("BUNIT=")
#define KEY_CENTRE      tofchar("CENTRE=")
#define KEY_FILENAME    tofchar("FILENAME=")
#define KEY_FIXED       tofchar("FIXED=")
#define KEY_FREEANGLE   tofchar("FREEANGLE=")
#define KEY_INCL        tofchar("INCL=")
#define KEY_INSET       tofchar("INSET=")
/* =========================================== */
#define KEY_INSET2      tofchar("INSET2=")
#define KEY_OUTCRE      tofchar("OUTCRE=")
#define KEY_OUTSET      tofchar("OUTSET=")
#define KEY_FILE1       tofchar("FILECOEFF=")
#define KEY_FILE2       tofchar("FILEHICOEFF=")
/* =========================================== */
#define KEY_PA          tofchar("PA=")
#define KEY_RADII       tofchar("RADII=")
#define KEY_SIDE        tofchar("SIDE=")
#define KEY_TOLERANCE   tofchar("TOLERANCE=")
#define KEY_VEXP        tofchar("VEXP=")
#define KEY_VROT        tofchar("VROT=")
#define KEY_VSYS        tofchar("VSYS=")
#define KEY_WEIGHT      tofchar("WEIGHT=")
#define KEY_WIDTHS      tofchar("WIDTHS=")
/* =========================================== */
#define KEY_FITORD      tofchar("FITORDER=")
/* =========================================== */

#define MES_BOX         tofchar("Area of velocity field [whole map]")
#define MES_BUNIT       tofchar("Units of velocity field [KM/S]")
#define MES_CENTRE      tofchar("Central position of velocity field")
#define MES_FILENAME    "File for tilted ring results:   [%s]"
#define MES_FIXED       tofchar("Fixed parameters [NONE]")
#define MES_FREEANGLE   tofchar("Free angle around minor axis:    [0.0]")
#define MES_INCL        tofchar("Inclination of rings in degrees")
#define MES_INSET       tofchar("Input set (and subset) of velocity field")
/* =========================================== */
#define MES_INSET2      tofchar("Input set (and subset) of total HI field")
#define MES_OUTSET      tofchar("Output set (and subset) of residual of residual  [none]")
/* =========================================== */
#define MES_PA          tofchar("Position Angles of rings in degrees")
#define MES_RADII       tofchar("Central radii of rings in arcsec")
#define MES_SIDE        tofchar("Which half of velocity field [BOTH]")
#define MES_TOLERANCE   tofchar("Tolerance of fit [0.001]")
#define MES_VEXP        tofchar("Expansion velocities of rings in km/s [0.0]" )
#define MES_VROT        tofchar("Rotation velocities of rings in km/s")
#define MES_VSYS        tofchar("Systemic velocity in km/s")
#define MES_WEIGHT      tofchar("Weighting function [UNIFORM]")
#define MES_WIDTHS      tofchar("Widths of rings in arcsec")
/* =========================================== */
#define MES_FITORD      tofchar("Highest order in harmonic fit")
/* =========================================== */



/*
 * Defines:
 */

#define CLASS           1               /* class 1 programme */
#define CLASSDIM        2               /* dimension of subset */
#define F               0.0174532925    /* degrees to radians */
#define G               0.4246609001    /* ? */
#define PI              3.141592654
#define MAXAXES         10              /* max. number of axes */
#define MAXFITSLEN      18              /* max. length fits char */
#define MAXLEN          20              /* max. length of text */
#define MAXMESLEN       128             /* max. lenght of messages */
#define MAXOPT          8               /* max. number of options */
#define MAXPAR          7               /* max. number of parameters */
#define MAXPIXEL        8000            /* max. numberof pixels in ring */
#define MAXRING         512             /* max. number of rings */
#define MAXSETNAMLEN    80              /* max. length of set name */
#define MAXTEXTLEN      320             /* max. length of text input */
/* =========================================== */
#define MAXFITDEG       41              /* max. degree of harmonic fit */
#define NO              0
#define YES             1
#define REQUEST         1
/* =========================================== */
#define VERSION         "1.2"           /* version.subversion number */

#define fcopy( f, c )                   \
        {int k;for(k=0;c[k]&&k<f.l;f.a[k]=c[k],k++);while(k<f.l)f.a[k++]=' ';}
#define fmake( f, c )                   \
        {f.a=c;f.l=sizeof(c);}
#define max( x, y )                     \
        (x>y?x:y)
#define min( x, y )                     \
        (x<y?x:y)
#define nint( x )                       \
        ((int) floor((x)+0.5))
        /* (x>0.0?(int)(x+0.5):(int)(x-0.5)) */
        
/*
 * variables:
 */

static  char    bset[MAXSETNAMLEN];     /* buffer for name of set */
static  double  cdelt[CLASSDIM];        /* grid separations */
static  double  mapfac;                 /* convert map values to KM/S */
static  double  mapphi;                 /* rotation of map */
static  fchar   set;                    /* points to bset */
static  fint    blo[CLASSDIM];          /* lower edge of box */
static  fint    bup[CLASSDIM];          /* upper edge of box */
static  fint    subset;                 /* subset level */
static  float   blank;                  /* system blank */
static  float   *vfield;                /* buffer for velocity field */
/* =========================================== */
static  char    osetb[MAXSETNAMLEN];    /* buffer for name of error set */
static  fchar   oset = {osetb, MAXSETNAMLEN} ;
static  fint    osubset;                /* subset level */
static  fint    ocwlo;
static  fint    ocwup;
static  fint    oaxperm[MAXAXES];
static  fint    oaxsize[MAXAXES];
static  char    bset2[MAXSETNAMLEN];    /* buffer for name of error set */
static  fchar   set2;                   /* points to bset2 */
static  fint    subset2;                /* subset level */
static  float   *HIfield;               /* buffer for HI field */

        FILE    *mp;                    /* file pointer */
        FILE    *hicof;
/* =========================================== */

/*
 * the parameters:
 */

static  fint    cor[2] = { -1.0, -1.0 };/* correlation ellips */
static  fint    mask[MAXPAR];           /* parameter fit mask */
static  fint    side;                   /* which half of velocity field */
static  fint    wpow;                   /* weighting power */
static  float   elp4[4];                /* coef matrix */
static  float   thetaf;                 /* free angle */
static  float   tol = 0.001;            /* tolerance of fit */

static  fint    nfit;                   /* number of fits */
static  fint    npts[MAXRING];          /* number of points */
static  fint    nrad;                   /* number of rings */
static  float   chis[MAXRING];          /* chis */
static  float   elp[MAXRING][4];        /* coef. matrices */
static  float   rads[MAXRING];          /* radii of rings */
static  float   wids[MAXRING];          /* width of rings */

static  float   incle[MAXRING];         /* error in incl */
static  float   inclf[MAXRING];         /* fitted incl */
static  float   incli[MAXRING];         /* initial incl */
static  float   posae[MAXRING];         /* error in posa */
static  float   posaf[MAXRING];         /* fitted posa */
static  float   posai[MAXRING];         /* initial posa */
static  float   vexpe[MAXRING];         /* error in vexp */
static  float   vexpf[MAXRING];         /* fitted vexp */
static  float   vexpi[MAXRING];         /* initial vexp */
static  float   vrote[MAXRING];         /* error in vrot */
static  float   vrotf[MAXRING];         /* fitted vrot */
static  float   vroti[MAXRING];         /* initial vrot */
static  float   vsyse[MAXRING];         /* error in vsys */
static  float   vsysf[MAXRING];         /* fitted vsys */
static  float   vsysi;                  /* initial vsys */
static  float   xpose[MAXRING];         /* error in xpos */
static  float   xposf[MAXRING];         /* fitted xpos */
static  float   xposi;                  /* initial xpos */
static  float   ypose[MAXRING];         /* error in ypos */
static  float   yposf[MAXRING];         /* fitted ypos */
static  float   yposi;                  /* initial ypos */
/* =========================================== */
static  float   outx[MAXPIXEL];         /* array with output for total HI*/
static  float   outy[MAXPIXEL];         /* array with output for total HI*/
static  float   outth[MAXPIXEL];        /* theta for harm.fit */
static  float   outres[MAXPIXEL];       /* velocityfield for harm.fit */
static  float   outHI[MAXPIXEL];        /* HIfield for harm.fit */
static  float   outwei[MAXPIXEL];       /* weight for harm.fit */
static  fint    fitdeg;                 /* degree of fit */
static  float   **outmat;
static  int     outwri;                 /* giving whether or not an outputset */
                                        /* should be created and written */

static  char    coeffb[256];
static  char    hicofb[256];
static  fchar   Coeff;
static  fchar   Hicof;
static  int     yesinset2;

/* =========================================== */


/*
 * func_c calculates radial velocity from rotation curve.
 */

float   func_c( float   c[CLASSDIM] ,   /* grid position in plane of galaxy */
                float   p[MAXPAR] ,     /* parameters of ring */
                fint    *m ,            /* dummy (number of parameters) */
                fint    *fopt )         /* option */
{
   float        vs, vc, vr;                     /* parameters of velocity field */
   float        x, y;                           /* sky coordinates */
   float        cost1, cost2, sint1, sint2, x1, y1, r;
   static float phi= 0.0, inc = 0.0;            /* saved parameters */
   static float cosp1 = 1.0, cosp2 = 1.0;
   static float sinp1 = 0.0, sinp2 = 0.0;
   static float cosi1 = 1.0, cosi2 = 1.0;
   static float sini1 = 0.0, sini2 = 0.0;

   vs = p[0];                                   /* systemic velocity */
   vc = p[1];                                   /* circular velocity */
   vr = p[2];                                   /* expansion velocity */
   if (p[3] != phi) {                           /* new position angle ? */
      phi = p[3];                               /* position angle */
      cosp1 = cos( F * phi );                   /* cosine */
      cosp2 = cosp1 * cosp1;                    /* cosine squared */
      sinp1 = sin ( F * phi );                  /* sine */
      sinp2 = sinp1 * sinp1;                    /* sine squared */
   }
   if (p[4] != inc) {                           /* new inclination ? */
      inc = p[4];                               /* inclination */
      cosi1 = cos( F * inc );                   /* cosine */
      cosi2 = cosi1 * cosi1;                    /* cosine squared */
      sini1 = sin ( F * inc );                  /* sine */
      sini2 = sini1 * sini1;                    /* sine squared */
   }
   x = c[0] - p[5] * cdelt[0];                  /* calculate x */
   y = c[1] - p[6] * cdelt[1];                  /* calculate y */
   x1 = ( -x * sinp1 + y * cosp1 );             /* x in plane of galaxy */
   y1 = ( -x * cosp1 - y * sinp1 ) / cosi1;     /* y in plane of galaxy */
   r = sqrt( x1 * x1 + y1 * y1 );               /* distance from centre */
   cost1 = x1 / r;                              /* cosine of angle in plane of galaxy */
   sint1 = y1 / r;                              /* sine of angle in plane of galaxy */
   cost2 = cost1 * cost1;                       /* cosine squared */
   sint2 = sint1 * sint1;                       /* sine squared */

   return( vs + ( vc * cost1 + vr * sint1 ) * sini1 );  /* return to caller */
}


/*
 * function derv calculates the partial derivatives with respect
 * to the parameters.
 */

void    derv_c( float   c[CLASSDIM] ,   /* grid position in plane of galaxy */
                float   p[MAXPAR] ,     /* parameters of ring */
                float   d[MAXPAR] ,     /* for partial derivatives */
                fint    *m ,            /* dummy variable */
                fint    *fopt )         /* option */
{
   float        vc, vr;                         /* parameters of velocity field */
   float        x, y;                           /* sky coordinates */
   float        cost1, cost2, sint1, sint2, x1, y1, r;
   static float phi= 0.0, inc = 0.0;            /* saved parameters */
   static float cosp1 = 1.0, cosp2 = 1.0;
   static float sinp1 = 0.0, sinp2 = 0.0;
   static float cosi1 = 1.0, cosi2 = 1.0;
   static float sini1 = 0.0, sini2 = 0.0;

   vc = p[1];                                   /* circular velocity */
   vr = p[2];                                   /* expansion velocity */
   if (p[3] != phi) {                           /* new position angle ? */
      phi = p[3];                               /* position angle */
      cosp1 = cos( F * phi );                   /* cosine */
      cosp2 = cosp1 * cosp1;                    /* cosine squared */
      sinp1 = sin ( F * phi );                  /* sine */
      sinp2 = sinp1 * sinp1;                    /* sine squared */
   }
   if (p[4] != inc) {                           /* new inclination ? */
      inc = p[4];                               /* inclination */
      cosi1 = cos( F * inc );                   /* cosine */
      cosi2 = cosi1 * cosi1;                    /* cosine squared */
      sini1 = sin ( F * inc );                  /* sine */
      sini2 = sini1 * sini1;                    /* sine squared */
   }
   x = c[0] - p[5] * cdelt[0];                  /* calculate x */
   y = c[1] - p[6] * cdelt[1];                  /* calculate y */
   x1 = ( -x * sinp1 + y * cosp1 );             /* x in plane of galaxy */
   y1 = ( -x * cosp1 - y * sinp1 ) / cosi1;     /* y in plane of galaxy */
   r = sqrt( x1 * x1 + y1 * y1 );               /* distance from centre */
   cost1 = x1 / r;                              /* cosine of angle in plane of galaxy */
   sint1 = y1 / r;                              /* sine of angle in plane of galaxy */
   cost2 = cost1 * cost1;                       /* cosine squared */
   sint2 = sint1 * sint1;                       /* sine squared */

   d[0] = 1.0;                                  /* partial derivative VSYS */
   d[1] = sini1 * cost1;                        /* .................. VROT */
   d[2] = sini1 * sint1;                        /* .................. VEXP */
                                                /* .................. PA */
   d[3] = F * vc * ( 1.0 - sini2 * sint2 ) * sint1 * sini1 / cosi1 -
          F * vr * ( 1.0 - sini2 * cost2 ) * cost1 * sini1 / cosi1;
                                                /* .................. INCL */
   d[4] = F * vc * ( cosi2 - sini2 * sint2 ) * cost1 / cosi1 +
          F * vr * ( cosi2 + sini2 * cost2 ) * sint1 / cosi1;
                                                /* .................. XPOS */
   d[5] =  cdelt[0] * vc * ( sint1 * sinp1 - cost1 * cosp1 / cosi1 ) * sint1 * sini1 / r -
           cdelt[0] * vr * ( sint1 * sinp1 - cost1 * cosp1 / cosi1 ) * cost1 * sini1 / r;
                                                /* .................. YPOS */
   d[6] = -cdelt[1] * vc * ( sint1 * cosp1 + cost1 * sinp1 / cosi1 ) * sint1 * sini1 / r +
           cdelt[1] * vr * ( sint1 * cosp1 + cost1 * sinp1 / cosi1 ) * cost1 * sini1 / r;
}


/*
 * getdat selects the data from the buffer and calculates differences.
 */

static  fint    getdat( float x[] ,     /* sky coords of pixels inside ring */
                        float y[] ,     /* radial velocities */
                        float w[] ,     /* weights of radial velocities */
                        float p[] ,     /* parameters of ring */
                        float ri ,      /* inner radius of ring */
                        float ro ,      /* outer radius of ring */
                        float *q ,      /* chi-squared */
                        fint nfr )      /* number of degrees of freedom */
{
   fint         fopt;                   /* function option */
   fint         llo, lup, mlo, mup;     /* corners */
   fint         l, m;                   /* counters */
   fint         n = 0;                  /* return value */
   fint         nlt, nmt;               /* box sizes */
   float        phi, inc, x0, y0;       /* define ellipse */
   float        free;                   /* free angle */
   float        cosp, cosi, sinp, sini; /* (co)sines */
   float        a, b;
   float        wi;

   (*q) = 0.0;                          /* reset sigma */
   phi = p[3] + mapphi;                 /* position angle plus map p.a. */
   inc = p[4];                          /* inclination */
   x0 = p[5];                           /* x-position of centre */
   y0 = p[6];                           /* y-position of centre */
   free = fabs( sin( F * thetaf ) );    /* sine of free angle */
   sinp = sin( F * phi );               /* sine of pa. */
   cosp = cos( F * phi );               /* cosine of pa. */
   sini = sin( F * inc );               /* sine of inc. */
   cosi = cos( F * inc );               /* cosine of inc. */
   a = sqrt( 1.0 - cosp * cosp * sini * sini );
   b = sqrt( 1.0 - sinp * sinp * sini * sini );
   llo = max( blo[0], nint( x0 - a * ro / cdelt[0] ) );
   lup = min( bup[0], nint( x0 + a * ro / cdelt[0] ) );
   mlo = max( blo[1], nint( y0 - b * ro / cdelt[1] ) );
   mup = min( bup[1], nint( y0 + b * ro / cdelt[1] ) );
   if ( ( llo > lup ) || ( mlo > mup ) ) {
      fint      error_level = 4;

      error_c( &error_level, tofchar( "Ring not inside map!" ) );
   }
   nlt = bup[0] - blo[0] + 1;           /* number of pixels in X */
   nmt = bup[1] - bup[1] + 1;           /* number of pixels in Y */
   for ( m = mlo; m < mup; m++) {
      float     ry = cdelt[1] * m;      /* Y position in plane of galaxy */

      for ( l = llo; l < lup; l++) {
         float  rx = cdelt[0] * l;      /* X position in plane of galaxy */
         float  v;
         int    ip;                     /* array pointer */
         ip = ( m - blo[1] ) * nlt + l - blo[0];
         v = vfield[ip];                /* radial velocity at this position */
         if ( v != blank ) {
            float       costh, r, theta, xr, yr;

            xr = ( - ( rx - cdelt[0] * x0 ) * sinp +
               ( ry - cdelt[1] * y0 ) * cosp );
            yr = ( - ( rx - cdelt[0] * x0 ) * cosp -
               ( ry - cdelt[1] * y0 ) * sinp ) / cosi;
            r = sqrt( xr * xr + yr * yr );      /* distance from centre */
            if ( r < 0.1 ) {                    /* radius to small ? */
               theta = 0.0;

            } else {
               theta = atan2( yr, xr ) / F;

            }
            costh = fabs( cos ( F * theta ) );
            if ( r > ri && r < ro && costh > free ) {   /* point inside ring ? */
               float    xx[2];
               int      use = 0;

               wi = pow( costh, (double) wpow );        /* calculate weight of this point */
               xx[0] = rx;                              /* x position */
               xx[1] = ry;                              /* y position */
               switch( side ) {                         /* which side of galaxy */
                  case 1: {                             /* receding half */
                     use = ( fabs( theta ) <= 90.0 );
                     break;
                  }
                  case 2: {                             /* approaching half */
                     use = ( fabs( theta ) >= 90.0 );
                     break;
                  }
                  case 3: {                             /* both halves */
                     use = 1;
                     break;
                  }
                  default: {
                     break;
                  }
               }
               if ( use ) {                             /* load data point ? */
                  n += 1;                               /* increase */
                  if ( n < MAXPIXEL ) {                 /* buffers not full */
                     fint       maxpar = MAXPAR;        /* number of parms. */
                     float      s, vz;

                     vz = func_c( xx, p, &maxpar, &fopt );
                     s = v - vz;


/* =========================================== */

                     outx[n]=rx;                /* load x and y coord */
                     outy[n]=ry;                /* in galaxy plane */
                     outth[n]=theta*F;          /* theta in radians */
                     outres[n]=v;               /* velocity at rx,ry */
                     outHI[n]=HIfield[ip];   /* total HI at rx ry */
                     outwei[n]=1./wi;           /* sigma */
/* =========================================== */

                     x[2*n-2] = rx;                     /* load X-coordinate */
                     x[2*n-1] = ry;                     /* load Y-coordinate */
                     y[n-1] = v;                        /* load radial velocity */

/* =========================================== */
                     w[n-1] = wi;               /* load weight */
                     (*q) += s * s * wi;        /* calculate chi-squared */
/* =========================================== */
                  }
               }
            }
         }
      }
   }
   if ( n > MAXPIXEL ) {
      char      m[80];
      fint      error_level = 1;

      sprintf( m, "Too many points in ring (%d); Maximum is %d", n, MAXPIXEL );
      error_c( &error_level, tofchar( m ) );
      n = MAXPIXEL;
   }
   if ( n > nfr ) {                             /* enough data points ? */
      (*q) = sqrt( (*q) / (float) ( n - nfr ) );/* calculate sigma */
   } else {                                     /* no free parameters */
      (*q) = FLT_MAX;
   }
   return( n );                                 /* return to caller */
}


/* =========================================== */

/* Function fbasis
 *
 * function fbasis returns values of basisfunctions
 * in the point th
 */

void fbasis (   float th,       /* angle theta in radians of pnt in velfield */
                float hb[],     /* array with values of each basis function in theta */
                int mba)        /* total number of basis functions */
{
        int j;

        hb[1] = 1.0;
        for (j=2;j<=mba;j++) {  /* loop over all harmonic terms */
                if (j%2 != 0)
                        hb[j] = sin(((j-1.0)/2.0)*th);
                else {
                        hb[j] = cos((j/2.0)*th);
                        }
        }
}
/* =========================================== */




/*
 * function: rotfit
 *
 * This function does a least squares fit to the radial velocity field.
 */

static  fint    rotfit( float   ri ,    /* inner radius of ring */
                        float   ro ,    /* outer radius of ring */
                        float   p[] ,   /* estimated/fitted parameters */
                        float   e[] ,   /* errors in parameters */
                        fint    *n ,    /* nr of points in the fit */
                        float   *q )    /* chi-squared */
{
   static char  *fmt[MAXPAR] = {
      "%9.2f", "%9.2f", "%10.2f", "%9.2f", "%7.2f", "%9.2f", "%9.2f"
   };
   static char  *hed1[MAXPAR] = {
      " systemic", " rotation", " expansion", " position", " incli-",
      " x-centre", " y-centre"
   };
   static char  *hed2[MAXPAR] = {
      " velocity", " velocity", "  velocity", "   angle ", " nation",
      " position", " position"
   };
   char         fb[MAXMESLEN];          /* format buffer */
   char         mess[MAXMESLEN];        /* buffer for messages */
   fint         error_level = 1;        /* level of error */
   fint         fopt;                   /* function option */
   fint         h, i, j, xi;            /* counters */
   fint         ier = 0;                /* error return */
   fint         nfr;                    /* number of free parameters */
   fint         nrt;                    /* return code from lsqfit */
   fint         output_level = 3;       /* level of output */
   fint         stop;                   /* stop ? */
   fint         t = 50;                 /* max. number of iterations */
   float        b[MAXPAR];              /* partial derivatives */
   float        chi;                    /* old chi-squared */
   float        df[MAXPAR];             /* difference vector */
   float        eps[MAXPAR];            /* contains stop criterium */
   float        flip;                   /* direction */
   float        lab = 0.001;            /* mixing parameter */
   float        pf[MAXPAR];             /* intermediate results */
   float        r;                      /* mean radius of ring */
   float        x[2*MAXPIXEL];          /* (x,y) */
   float        y[MAXPIXEL];            /* f(x,y) */
   float        w[MAXPIXEL];            /* w(x,y) */
/* =========================================== */
   float        fit[MAXFITDEG];         /* result of harmonic fit */
   float        ww[MAXFITDEG];          /* weight of parameters */
   float        chisq;                  /* chisquare of harmonic fit */
   float        **u;                    /* matrix */
   float        **v;
   float        **cvam;                 /* covariance matrix */
   void         svdfit();               /* the fitroutine */
   void         sort2();                /* Heapsort */
   float        **matrix();             /* allocates memory for a matrix */
   void         free_matrix();          /* destroys matrix */
   float        sini,sini2,cosi,dgamma,sysv,errsys,rotv,errrot;
   fint         nwrite;
   fint         tid=0;
/* =========================================== */

   for ( nfr = 0, i = 0; i < MAXPAR; i++ ) {
      eps[i] = 0.1;                     /* convergence criterium */
      nfr += mask[i];
   }
   r = 0.5 * ( ri + ro );               /* mean radius of ring */
   sprintf( mess, "working on radius: %7.2f arcsec", r );
   status_c( tofchar( mess ) );
   sprintf( mess, " radius of ring: %7.2f arcsec", r );
   anyout_c( &output_level, tofchar( mess ) );
   strcpy( mess, "  iter." );
   for ( j = 0; j < MAXPAR; j++ ) {
      if (mask[j]) strcat( mess, hed1[j] );
   }
   strcat( mess, " points    sigma" );
   anyout_c( &output_level, tofchar( mess ) );
   strcpy( mess, " number" );
   for ( j = 0; j < MAXPAR; j++ ) {
      if (mask[j]) strcat( mess, hed2[j] );
   }
   strcat( mess, "         velocity" );
   anyout_c( &output_level, tofchar( mess ) );
   h = 0;                               /* reset iteration counter */
   (*n) = getdat( x, y, w, p, ri, ro, q, nfr );
   sprintf( mess, "%7d", h );
   for ( j = 0; j < MAXPAR; j++ ) {
      if (mask[j]) {
         sprintf( fb, fmt[j], (double) p[j] );
         strcat( mess, fb );
      }
   }
   sprintf( fb, "%7d %#9.3g", (int) (*n), (double) (*q) );
   strcat( mess, fb );
   anyout_c( &output_level, tofchar( mess ) );
   stop = 0;
   do {                                 /* start of loop */
      fint      npar = MAXPAR;          /* number of parameters */
      fint      xdim = 2;               /* function is two-dimensional */

      h += 1;                           /* next iteration */
      chi = (*q);                       /* save chi-squared */
      for ( i = 0; i < MAXPAR; i++ ) {  /* loop to save initial estimates */
         pf[i] = p[i];
      }
      nrt = lsqfit_c( x, &xdim, y, w, n, pf, e, mask, &npar, &tol, &t,
         &lab, &fopt );
      if (nrt < 0 ) break;              /* stop because of error */
      for ( i = 0; i < MAXPAR; i++ ) {  /* calculate difference vector */
         df[i] = pf[i] - p[i];
      }
      flip = 1.0;                       /* factor for inner loop */
      while (1) {                       /* inner loop */
         for ( i = 0; i < MAXPAR; i++ ) {       /* calculate new parameters */
            pf[i] = flip * df[i] + p[i];
         }
         if ( pf[4] > 90.0 ) pf[4] -= 180.0;    /* in case inclination > 90 */
         (*n) = getdat( x, y, w, pf, ri, ro, q, nfr );
         if ( (*q) < chi ) {                    /* better fit */
            sprintf( mess, "%7d", h );
            for ( j = 0; j < MAXPAR; j++ ) {
               if (mask[j]) {
                  sprintf( fb, fmt[j], (double) pf[j] );
                  strcat( mess, fb );
               }
            }
            sprintf( fb, "%7d %#9.3g", (int) (*n), (double) (*q) );
            strcat( mess, fb );
            anyout_c( &output_level, tofchar( mess ) );
            for ( i = 0; i < MAXPAR; i++ ) {    /* save new parameters */
               p[i] = pf[i];
            }
            break;                              /* leave inner loop */
         } else {
            if ( (2 * h) > t ) {
               for ( stop = 1, i = 0; i < MAXPAR; i++ ) {
                  stop = ( stop && ( fabs( flip * df[i] ) < eps[i] ) );
               }
            } else {
               stop = ( ( fabs( (*q) - chi ) / chi ) < tol );
            }
            if (stop) {
               (*q) = chi;
               break;
            }
         }
         if ( flip > 0.0 ) {
            flip *= -1.0;
         } else {
            flip *= -0.5;
         }
      }
   } while ( !stop && h < t );
   /*
    * find out why we quit fitting.
    */
   if (stop) {                                  /* good fit */
      ier = h;                                  /* number of big loops */
   } else if ( nrt < 0 ) {                      /* error from lsqfit */
      ier = nrt;
   } else if ( h == t ) {                       /* max. number of iterations */
      ier = -4;
   }

   switch( ier ) {
      case -1: {
         error_c( &error_level,
            tofchar( "reswri: -1 Too many free parameters!" ) );
         break;
      }
      case -2: {
         error_c( &error_level,
            tofchar( "reswri: -2 No free parameters!" ) );
         break;
      }
      case -3: {
         error_c( &error_level,
            tofchar( "reswri: -3 Not enough degrees of freedom!" ) );
         break;
      }
      case -4: {
         error_c( &error_level,
            tofchar( "reswri: -4 Maximum number of iterations too small!" ) );
         break;
      }
      case -5: {
         error_c( &error_level,
            tofchar( "reswri: -5 Diagonal of matrix contains zeroes!" ) );
         break;
      }
      case -6: {
         error_c( &error_level,
            tofchar( "reswri: -6 Det. of the coeff. matrix is zero!" ) );
         break;
      }
      case -7: {
         error_c( &error_level,
            tofchar( "reswri: -7 Square root of negative number!" ) );
         break;
      }
      default: {
         sprintf( mess, "%7d", h );
         for ( j = 0; j < MAXPAR; j++ ) {
            if (mask[j]) {
               sprintf( fb, fmt[j], (double) p[j] );
               strcat( mess, fb );
            }
         }
         sprintf( fb, "%7d %#9.3g", (int) (*n), (double) (*q) );
         strcat( mess, fb );
         anyout_c( &output_level, tofchar( mess ) );
         break;
      }
   }
   /*
    * calculate ellipse parameters.
    */
   if ( ier == 1 && cor[0] > -1 && cor[1] > -1 ) {
      fint      i;
      fint      maxpar = MAXPAR;                /* max. number of parms. */
      float     a11 = 0.0;
      float     a12 = 0.0;
      float     a22 = 0.0;
      float     sigma2 = 0.0;

      for ( i = 0; i < (*n); i++ ) {
         derv_c( &x[2*i], p, b, &maxpar, &fopt );       /* calculate derivatives */
         a11 = a11 + w[i] * b[cor[0]] * b[cor[0]];
         a22 = a22 + w[i] * b[cor[1]] * b[cor[1]];
         a12 = a12 + w[i] * b[cor[0]] * b[cor[1]];
         sigma2 = sigma2 +
            w[i] * pow( y[i] - func_c( &x[2*i], p, &maxpar, &fopt), 2.0 );
      }
      sigma2 = sigma2 / (float) (*n);
      elp4[0] = a11;
      elp4[1] = a12;
      elp4[2] = a22;
      elp4[3] = sigma2;
   }


/* ================================================================================= */
/* Go and do the first harmonic fit for vsys and vrot*/



      sini = sin (p[4]*F);                      /* sine inclination */
      sini2 = sini*sini;                        /* sine inclination squared */
      cosi = cos (p[4]*F);
      sort2((*n),outth,outres,outwei,outx,outy,outHI);
 /* Call Heapsort in order to sort the data according to increasing theta */

/* Subtract vsys and vrot from data */
      sysv = p[0];                      /* save these parameters for addition to 2nd fit */
      rotv = p[1]*sini;
      errsys = e[0];
      errrot = e[1];
      for (xi=1;xi<=(*n);xi++) {
        outres[xi] -= (sysv + rotv*cos(outth[xi])); /* subtract systemic and rotational velocity */
        }

/* Now do the second fit */
      u = matrix(1,(*n),1,fitdeg);              /* first create neccesary martices */
      v = matrix(1,fitdeg,1,fitdeg);
      cvam = matrix(1,(*n),1,(*n));

      svdfit(outth,outres,outwei,(*n),fit,fitdeg,u,v,ww,cvam,&chisq,(void(*)())fbasis);


      fit[1] += sysv;
      fit[2] += rotv;

      for(xi=1;xi<=(*n);xi++){ /* Calculate residual field of residual field */
         outres[xi] += (sysv + rotv*cos(outth[xi]));
         outres[xi] -= (fit[1] + fit[2]*cos(outth[xi]));
         for (i=3;i<=fitdeg;i++) {
                if (i%2 != 0)
                        outres[xi] -= fit[i]*sin( ((i-1.0)/2.0) * outth[xi]);
                else {
                        outres[xi] -= fit[i]*cos( (i/2.0)*outth[xi]);
                }
         }
         outmat[(int)((outy[xi]/cdelt[1])+0.5)][(int)((outx[xi]/cdelt[0])+0.5)]=outres[xi];
      } /* outmat will be used as outputset */



      for (i=2;i<=fitdeg;i++){
            fit[i] = fit[i] / sini;     /* correct fitted par. for inclination */
            cvam[i][i] = (cvam[i][i]/ sini2)*(chisq / ( (*n)-fitdeg ));
      }

      cvam[1][1] = cvam[1][1]*chisq / ((*n)-fitdeg);

/* Write to screen and file */
      anyoutf(1,"vsys %g %g \n",fit[1],sqrt(cvam[1][1]));
      for (i=2;i<=fitdeg;i++){
                if (i%2 != 0)
                        anyoutf(1,"sin %g %g %g",(i-1.0)/2.0,fit[i],sqrt(cvam[i][i]));
                else {
                        anyoutf(1,"cos %g %g %g",i/2.0,fit[i],sqrt(cvam[i][i]));
                }
      }

      fprintf(mp,"%g  ",r);  /* output to outputfile */
      for (xi=1 ; xi<=fitdeg ; xi++) {
                fprintf(mp,"%g  %g  ",fit[xi],sqrt(cvam[xi][xi]));
        }
      fprintf(mp,"\n");
      free_matrix(u,1,(*n),1,fitdeg);           /* Destroy the matrices */
      free_matrix(v,1,fitdeg,1,fitdeg);
      free_matrix(cvam,1,(*n),1,(*n));


/* Now do the fit to the total HI field */
      if (yesinset2)
      {
         u = matrix(1,(*n),1,fitdeg);              /* first create neccesary martices */
         v = matrix(1,fitdeg,1,fitdeg);
         cvam = matrix(1,(*n),1,(*n));
         svdfit(outth,outHI,outwei,(*n),fit,fitdeg,u,v,ww,cvam,&chisq,(void(*)())fbasis);
      for (i=1;i<=fitdeg;i++){   /* correct error estimates */
                    cvam[i][i] = (cvam[i][i])*(chisq / ( (*n)-fitdeg ));
      }
         /* Write to screen and file */
         anyoutf(1,"average %g %g \n",fit[1],sqrt(cvam[1][1]));
         for (i=2;i<=fitdeg;i++)
         {
                   if (i%2 != 0)
                      anyoutf(1,"sin %g %g %g",(i-1.0)/2.0,fit[i],sqrt(cvam[i][i]));
                   else
                      anyoutf(1,"cos %g %g %g",i/2.0,fit[i],sqrt(cvam[i][i]));
         }
         fprintf(hicof,"%g  ",r);
         for (xi=1 ; xi<=fitdeg ; xi++)
         {
            fprintf(hicof,"%g  %g  ",fit[xi],sqrt(cvam[xi][xi]));
         }
         fprintf(hicof,"\n");
         free_matrix(u,1,(*n),1,fitdeg);           /* Destroy the matrices */
         free_matrix(v,1,fitdeg,1,fitdeg);
         free_matrix(cvam,1,(*n),1,(*n));
      }
      return( ier );                            /* return to caller */
}
/* =========================================================================== */
void sort2( int n,
            float *ra,
            float *rb,
            float *rc,
            float *rd,
            float *re,
            float *rf )
{
        int l,j,ir,i;
        float rrf,rre,rrd,rrc,rrb,rra;

        l=(n >> 1)+1;
        ir=n;
        for (;;) {
                if (l > 1) {
                        rra=ra[--l];
                        rrb=rb[l];
                        rrc=rc[l];
                        rrd=rd[l];
                        rre=re[l];
                        rrf=rf[l];
                } else {
                        rra=ra[ir];
                        rrb=rb[ir];
                        rrc=rc[ir];
                        rrd=rd[ir];
                        rre=re[ir];
                        rrf=rf[ir];
                        ra[ir]=ra[1];
                        rb[ir]=rb[1];
                        rc[ir]=rc[1];
                        rd[ir]=rd[1];
                        re[ir]=re[1];
                        rf[ir]=rf[1];
                        if (--ir == 1) {
                                ra[1]=rra;
                                rb[1]=rrb;
                                rc[1]=rrc;
                                rd[1]=rrd;
                                re[1]=rre;
                                rf[1]=rrf;
                                return;
                        }
                }
                i=l;
                j=l << 1;
                while (j <= ir) {
                        if (j < ir && ra[j] < ra[j+1]) ++j;
                        if (rra < ra[j]) {
                                ra[i]=ra[j];
                                rb[i]=rb[j];
                                rc[i]=rc[j];
                                rd[i]=rd[j];
                                re[i]=re[j];
                                rf[i]=rf[j];
                                j += (i=j);
                        }
                        else j=ir+1;
                }
                ra[i]=rra;
                rb[i]=rrb;
                rc[i]=rrc;
                rd[i]=rrd;
                re[i]=rre;
                rf[i]=rrf;
        }
}

/* =========================================================================== */



/* ============================================================================*/
/* ----------------------start of numerical recipes subroutines ---------------*/
/* ============================================================================*/

void svbksb(float **u, float w[], float **v, int m, int n, float b[],float x[])


{
        int jj,j,i;
        float s,*tmp,*vector();
        void free_vector();

        tmp=vector(1,n);
        for (j=1;j<=n;j++) {
                s=0.0;
                if (w[j]) {
                        for (i=1;i<=m;i++) s += u[i][j]*b[i];
                        s /= w[j];
                }
                tmp[j]=s;
        }
        for (j=1;j<=n;j++) {
                s=0.0;
                for (jj=1;jj<=n;jj++) s += v[j][jj]*tmp[jj];
                x[j]=s;
        }
        free_vector(tmp,1,n);
}

void svdvar(float **v , int ma , float w[] , float **cvm)
{
        int k,j,i;
        float sum,*wti,*vector();
        void free_vector();

        wti=vector(1,ma);
        for (i=1;i<=ma;i++) {
                wti[i]=0.0;
                if (w[i]) wti[i]=1.0/(w[i]*w[i]);
        }
        for (i=1;i<=ma;i++) {
                for (j=1;j<=i;j++) {
                        for (sum=0.0,k=1;k<=ma;k++) sum += (v[i][k]*v[j][k]*wti[k]);
                        cvm[j][i]=cvm[i][j]=sum;
                }
        }
        free_vector(wti,1,ma);
}

#define TOL 1.0e-5

void svdfit( float *x,
             float *y,
             float *sig,
             int   ndata,
             float *a,
             int   ma,
             float **u,
             float **v,
             float *w,
             float **cvm,
             float *chisq,
             void (*funcs)(float,float *,int) )
{
        int j,i;
        float sini,wmax,tmp,thresh,sum,*b,*afunc,*vector();
        void svdksb(float **u,float w[],float **v, int m, int n, float b[],float x[]);
        void svdcmp(float **a,int m,int n, float w[], float **v);
        void free_vector();


        b=vector(1,ndata);
        afunc=vector(1,ma);
        for (i=1;i<=ndata;i++) {

                (*funcs)(x[i],afunc,ma);
                tmp=1.0/sig[i];
                for (j=1;j<=ma;j++) {
                        u[i][j]=afunc[j]*tmp;
                }
                b[i]=y[i]*tmp;
        }
        svdcmp(u,ndata,ma,w,v);

        wmax=0.0;
        for (j=1;j<=ma;j++)
                if (w[j] > wmax) wmax=w[j];
        thresh=TOL*wmax;
        for (j=1;j<=ma;j++) {
                if (w[j] < thresh) {
                        w[j]=0.0;
                        anyoutf(1,"WARNING : SVDFIT detected singular value in fit !");
                }
        }
        svbksb(u,w,v,ndata,ma,b,a);
        *chisq=0.0;
        for (i=1;i<=ndata;i++) {
                (*funcs)(x[i],afunc,ma);
                for (sum=0.0,j=1;j<=ma;j++) sum += a[j]*afunc[j];
                *chisq += (tmp=(y[i]-sum)/sig[i],tmp*tmp);
        }
        free_vector(afunc,1,ma);
        free_vector(b,1,ndata);
        svdvar(v,ma,w,cvm);

}

#undef TOL



static float at,bt,ct;
#define PYTHAG(a,b) ((at=fabs(a)) > (bt=fabs(b)) ? \
(ct=bt/at,at*sqrt(1.0+ct*ct)) : (bt ? (ct=at/bt,bt*sqrt(1.0+ct*ct)): 0.0))


static float maxarg1,maxarg2;
#define MAX(a,b) (maxarg1=(a),maxarg2=(b),(maxarg1) > (maxarg2) ?\
        (maxarg1) : (maxarg2))
#define SIGN(a,b) ((b) >= 0.0 ? fabs(a) : -fabs(a))

void svdcmp(float **a,int m,int n,float w[],float **v)
{
        int flag,i,its,j,jj,k,l,nm;
        float c,f,h,s,x,y,z;
        float anorm=0.0,g=0.0,scale=0.0;
        float *rv1,*vector();
        void nrerror(),free_vector();

        if (m < n) nrerror("SVDCMP: You must augment A with extra zero rows");
        rv1=vector(1,n);
        for (i=1;i<=n;i++) {
                l=i+1;
                rv1[i]=scale*g;
                g=s=scale=0.0;
                if (i <= m) {
                        for (k=i;k<=m;k++) scale += fabs(a[k][i]);
                        if (scale) {
                                for (k=i;k<=m;k++) {
                                        a[k][i] /= scale;
                                        s += a[k][i]*a[k][i];
                                }
                                f=a[i][i];

                                g = -SIGN(sqrt(s),f);
                                h=f*g-s;
                                a[i][i]=f-g;
                                if (i != n) {
                                        for (j=l;j<=n;j++) {
                                                for (s=0.0,k=i;k<=m;k++) s += a[k][i]*a[k][j];
                                                f=s/h;
                                                for (k=i;k<=m;k++) a[k][j] += f*a[k][i];
                                        }
                                }
                                for (k=i;k<=m;k++) a[k][i] *= scale;
                        }
                }
                w[i]=scale*g;
                g=s=scale=0.0;
                if (i <= m && i != n) {
                        for (k=l;k<=n;k++) scale += fabs(a[i][k]);
                        if (scale) {
                                for (k=l;k<=n;k++) {
                                        a[i][k] /= scale;
                                        s += a[i][k]*a[i][k];
                                }
                                f=a[i][l];

                                g = -SIGN(sqrt(s),f);
                                h=f*g-s;
                                a[i][l]=f-g;
                                for (k=l;k<=n;k++) rv1[k]=a[i][k]/h;
                                if (i != m) {
                                        for (j=l;j<=m;j++) {
                                                for (s=0.0,k=l;k<=n;k++) s += a[j][k]*a[i][k];
                                                for (k=l;k<=n;k++) a[j][k] += s*rv1[k];
                                        }
                                }
                                for (k=l;k<=n;k++) a[i][k] *= scale;
                        }
                }
                anorm=MAX(anorm,(fabs(w[i])+fabs(rv1[i])));
        }

        for (i=n;i>=1;i--) {
                if (i < n) {
                        if (g) {
                                for (j=l;j<=n;j++)
                                        v[j][i]=(a[i][j]/a[i][l])/g;
                                for (j=l;j<=n;j++) {
                                        for (s=0.0,k=l;k<=n;k++) s += a[i][k]*v[k][j];
                                        for (k=l;k<=n;k++) v[k][j] += s*v[k][i];
                                }
                        }
                        for (j=l;j<=n;j++) v[i][j]=v[j][i]=0.0;
                }
                v[i][i]=1.0;
                g=rv1[i];
                l=i;
        }
        for (i=n;i>=1;i--) {
                l=i+1;
                g=w[i];
                if (i < n)
                        for (j=l;j<=n;j++) a[i][j]=0.0;
                if (g) {
                        g=1.0/g;
                        if (i != n) {
                                for (j=l;j<=n;j++) {
                                        for (s=0.0,k=l;k<=m;k++) s += a[k][i]*a[k][j];
                                        f=(s/a[i][i])*g;
                                        for (k=i;k<=m;k++) a[k][j] += f*a[k][i];
                                }
                        }
                        for (j=i;j<=m;j++) a[j][i] *= g;
                } else {
                        for (j=i;j<=m;j++) a[j][i]=0.0;
                }
                ++a[i][i];
        }
        for (k=n;k>=1;k--) {
                for (its=1;its<=30;its++) {
                        flag=1;
                        for (l=k;l>=1;l--) {
                                nm=l-1;
                                if (fabs(rv1[l])+anorm == anorm) {
                                        flag=0;
                                        break;
                                }
                                if (fabs(w[nm])+anorm == anorm) break;
                        }
                        if (flag) {
                                c=0.0;
                                s=1.0;
                                for (i=l;i<=k;i++) {
                                        f=s*rv1[i];
                                        if (fabs(f)+anorm != anorm) {
                                                g=w[i];

                                                h=PYTHAG(f,g);
                                                w[i]=h;
                                                h=1.0/h;
                                                c=g*h;
                                                s=(-f*h);
                                                for (j=1;j<=m;j++) {
                                                        y=a[j][nm];
                                                        z=a[j][i];
                                                        a[j][nm]=y*c+z*s;
                                                        a[j][i]=z*c-y*s;
                                                }
                                        }
                                }
                        }
                        z=w[k];
                        if (l == k) {
                                if (z < 0.0) {
                                        w[k] = -z;
                                        for (j=1;j<=n;j++) v[j][k]=(-v[j][k]);
                                }
                                break;
                        }
                        if (its == 30) nrerror("No convergence in 30 SVDCMP iterations");
                        x=w[l];
                        nm=k-1;
                        y=w[nm];
                        g=rv1[nm];
                        h=rv1[k];
                        f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);

                        g=PYTHAG(f,1.0);
                        f=((x-z)*(x+z)+h*((y/(f+SIGN(g,f)))-h))/x;
                        c=s=1.0;
                        for (j=l;j<=nm;j++) {
                                i=j+1;
                                g=rv1[i];
                                y=w[i];
                                h=s*g;
                                g=c*g;

                                z=PYTHAG(f,h);
                                rv1[j]=z;
                                c=f/z;
                                s=h/z;
                                f=x*c+g*s;
                                g=g*c-x*s;
                                h=y*s;
                                y=y*c;
                                for (jj=1;jj<=n;jj++) {
                                        x=v[jj][j];
                                        z=v[jj][i];
                                        v[jj][j]=x*c+z*s;
                                        v[jj][i]=z*c-x*s;
                                }

                                z=PYTHAG(f,h);
                                w[j]=z;
                                if (z) {
                                        z=1.0/z;
                                        c=f*z;
                                        s=h*z;
                                }
                                f=(c*g)+(s*y);
                                x=(c*y)-(s*g);
                                for (jj=1;jj<=m;jj++) {
                                        y=a[jj][j];
                                        z=a[jj][i];
                                        a[jj][j]=y*c+z*s;
                                        a[jj][i]=z*c-y*s;
                                }
                        }
                        rv1[l]=0.0;
                        rv1[k]=f;
                        w[k]=x;
                }
        }
        free_vector(rv1,1,n);
}

#undef SIGN
#undef MAX
#undef PYTHAG

#define NR_END 1
#define FREE_ARG char*

void nrerror(char error_text[])
{
        fprintf(stderr,"Runtime ERROR in Numerical Recipes routine...\n");
        fprintf(stderr,"%s\n",error_text);
        fprintf(stderr,"exiting system \n");
        exit(1);

}

float *vector(long nl, long nh)
{
        float *v;

        v=(float *)malloc((size_t) ((nh-nl+1+NR_END)*sizeof(float)));
        if (!v) nrerror("allocation failure in vector()");
        return v-nl+NR_END;

}

void free_vector(float *v, long nl , long nh)

{
        free((FREE_ARG) (v+nl-NR_END));
}

float **matrix(long nrl, long nrh, long ncl, long nch)
{
        long i, nrow=nrh-nrl+1,ncol=nch-ncl+1;
        float **m;

        m=(float **) malloc((size_t)((nrow+NR_END)*sizeof(float*)));
        if (!m) nrerror("aloccation failure 1 in matrix()");
        m += NR_END;
        m -= nrl;

        m[nrl] = (float *) malloc((size_t)((nrow*ncol+NR_END)*sizeof(float)));
        if (!m[nrl]) nrerror("allocation failure 2 in matrix()");
        m[nrl] += NR_END;
        m[nrl] -= ncl;

        for(i=nrl+1;i<=nrh;i++) m[i] = m[i-1]+ncol;
        return m;
}

void free_matrix(float **m,long nrl, long nrh, long ncl, long nch)
{
        free((FREE_ARG)(m[nrl]+ncl-NR_END));
        free((FREE_ARG)(m+nrl-NR_END));
}




static bool usercont( fchar Keyword,
                      char  *message,
                      bool  def,
                      fint  dfault)
/*-------------------------------------------------------------*/
/* PURPOSE: Ask user confirmation to continue.                 */
/*-------------------------------------------------------------*/
{
        bool    cont;
        fint    r1;
        fint    nitems;


        cont   = toflog( def );
        nitems = 1;
        r1     = userlog_c( &cont, &nitems, &dfault, Keyword,
                   tofchar( message ) );
        cancel_c( Keyword );
        cont   = tobool( cont );
        return( cont );
}




/* ============================================================================*/
/* ------------------End of numerical recipes subroutines----------------------*/
/* ============================================================================*/




MAIN_PROGRAM_ENTRY
{
   init_c( );                                   /* contact HERMES */
   IDENTIFICATION( "reswri", VERSION );         /* our id */
   fmake( set, bset );                          /* make fchar */
   fmake( set2, bset2 );                        /* make fchar */
   setfblank_c( &blank );                       /* get system blank */
   /*
    * First we need the set (and subset) which contains the velocity field,
    * and the box which includes the data. Then we check whether the
    * axis units are correct (should be convertable to arcsecs) and
    * the map units.
    */
   Coeff.a = coeffb;
   Coeff.l = 255;
   Hicof.a = hicofb;
   Hicof.l = 255;
   {
      char      bbunit[MAXFITSLEN+1];
      char      descriptor[MAXFITSLEN+1];       /* buffer for descriptor name */
      fchar     bunit;
      fint      axcount[MAXAXES];               /* axis sizes */
      fint      axperm[MAXAXES];                /* axis permutation */
      fint      box_mode = 0;                   /* mode for gdsbox */
      fint      class = CLASS;                  /* class of programme */
      fint      classdim = CLASSDIM;            /* dimension of subset */
      fint      default_level = 0;              /* no default */
      fint      error_level = 4;                /* fatal error */
      fint      gerror = 0;                     /* GDS error return */
      fint      i;                              /* loop counter */
      fint      maxaxes = MAXAXES;              /* max. number of axes */
      fint      maxsub = 1;                     /* max. number of subsets */
      fint      output_level = 11;              /* level of output */
      fint      tcount = 0;                     /* add ax types */
      fint      hidden = 2;
      fint      nsub;

      fmake( bunit, bbunit );
      (void) gdsinp_c( set, &subset, &maxsub, &default_level, KEY_INSET,
         MES_INSET, &output_level, axperm, axcount, &maxaxes, &class,
         &classdim );
      nsub = gdsinp_c( set2, &subset2, &maxsub, &hidden, KEY_INSET2,
          MES_INSET2, &output_level, axperm, axcount, &maxaxes, &class,
          &classdim );

      yesinset2 = (nsub > 0);

      default_level = 1;                        /* default level for box */
      gdsbox_c( blo, bup, set, &subset, &default_level, KEY_BOX, MES_BOX,
         &output_level, &box_mode );
      /*
       * Now we create the buffer for the velocity field and read in the
       * Radial velocities.
       */
      {
         fint   cwlo, cwhi;                     /* coordinate words */
         fint   ndone;                          /* number of points read */
         fint   npoints;                        /* size of buffer */
         fint   tid = 0;                        /* transfer identifier */

         npoints = ( bup[1] - blo[1] + 1 ) * ( bup[0] - blo[0] + 1 );
         vfield = calloc( sizeof( float ), npoints );
         HIfield  = calloc( sizeof( float ), npoints );

         if ( vfield == NULL || HIfield==NULL) {
            fint        error_level = 4;                /* fatal error */

            error_c( &error_level,
               tofchar( "Error allocating buffer space!" ) );
         }
         cwlo = gdsc_fill_c( set, &subset, blo );
         cwhi = gdsc_fill_c( set, &subset, bup );
         gdsi_read_c( set, &cwlo, &cwhi, vfield, &npoints, &ndone, &tid );
         if (yesinset2)
         {
            cwlo = gdsc_fill_c( set2, &subset2, blo );
            cwhi = gdsc_fill_c( set2, &subset2, bup );
            gdsi_read_c( set2, &cwlo, &cwhi, HIfield, &npoints, &ndone, &tid );
         }
      }
/* Get outputset */
      {
        fint class = CLASS;
        fint input_level=1;
        fint items=1;
        fint maxaxes=MAXAXES;
        fint output_level=11;
        fint r;

        gdsasn_c(KEY_INSET,KEY_OUTSET,&class);
        outwri = (int) gdsout_c(oset,&osubset,&items,&input_level,KEY_OUTSET,
                       MES_OUTSET,&output_level,oaxperm,oaxsize,&maxaxes);
        if (outwri)
        {
           ocwlo = gdsc_fill_c(oset,&osubset,blo);
           ocwup = gdsc_fill_c(oset,&osubset,bup);
        }
      }

      for ( i = 0; i < CLASSDIM; i++ ) {
         char   bctype[MAXFITSLEN+1];
         char   bcunit1[MAXFITSLEN+1];
         char   bcunit2[MAXFITSLEN+1];
         char   bdunit[MAXFITSLEN+1];
         fchar  ctype, cunit, cunit1, cunit2, dunit;
         fint   axnum = axperm[i];
         fint   axtyp;
         fint   level = 0;
         fint   skysys, prosys, velsys;

         fmake( ctype, bctype );
         fmake( cunit1, bcunit1 );
         fmake( cunit2, bcunit2 );
         fmake( dunit, bdunit );
         gdsc_name_c( ctype, set, &axnum, &gerror );
         axtyp = axtype_c( ctype, cunit1, dunit, &skysys, &prosys, &velsys );
         switch( axtyp ) {
            case 2: {                           /* Latitude (get rotation) */
               sprintf( descriptor, "CROTA%d", axnum );
               gerror = 0;
               gdsd_rdble_c( set, tofchar( descriptor ), &level, &mapphi,
                  &gerror );
               if (gerror) { mapphi = 0.0; }
            }
            case 1: {                           /* get cdelt in arcsec */
               sprintf( descriptor, "CUNIT%d", axnum );
               gerror = 0;
               gdsd_rchar_c( set, tofchar( descriptor ), &level, cunit2, &gerror );
               if (gerror) {
                  cunit = cunit1;
               } else {
                  cunit = cunit2;
               }
               sprintf( descriptor, "CDELT%d", axnum );
               gerror = 0;
               gdsd_rdble_c( set, tofchar( descriptor ), &level, &cdelt[i],
                  &gerror );
               if (gerror) {
                  error_c( &error_level,
                     tofchar( "No gridseparation found!" ) );
               } else {
                  double        f;

                  (void) factor_c( cunit, tofchar( "ARCSEC" ), &f );
                  cdelt[i] *= f;
               }
               cdelt[i] = fabs( cdelt[i] );     /* make positive */
               break;
            }
            default: {
               axtyp = 0;
               break;
            }
         }
         tcount += axtyp;
      }
      if (tcount != 3) {
         error_c( &error_level,
            tofchar( "No Longitude & Latitude axes pair!" ) );
      }
      sprintf( descriptor, "BUNIT" );
      gerror = 0;
      gdsd_rchar_c( set, tofchar( descriptor ), &subset, bunit, &gerror );
      if (gerror < 0) {
         fint   items = 1;

         default_level = 1;
         strcpy( bunit.a, "KM/S" );
         for ( i = strlen( bunit.a ); i < MAXFITSLEN; i++) {
            bunit.a[i] = ' ';
         }
         (void) userchar_c( bunit, &items, &default_level, KEY_BUNIT,
            MES_BUNIT );
         gerror = 0;
         gdsd_wchar_c( set, tofchar( descriptor ), &subset, bunit, &gerror );
      }
      if (factor_c( bunit, tofchar( "KM/S" ), &mapfac )) {
         error_c( &error_level, tofchar( "BUNIT not convertable to KM/S!" ) );
      }
   }
   /*
    * Now we need concentric rings and the initial parameters.
    * If we already have made a run of the programme, we must have a table
    * which contains the results of the previous run. In the future, we
    * will make use of that information.
    */
   {
      fint      count;                          /* input counter */
      fint      i;                              /* loop counter */
      fint      input_level = 0;                /* no default */
      fint      items = MAXRING;                /* max. number of radii */
      double    cpos[CLASSDIM];                 /* central position */

      nrad = userreal_c( rads, &items, &input_level, KEY_RADII, MES_RADII );
      items = userreal_c( wids, &nrad, &input_level, KEY_WIDTHS, MES_WIDTHS );
      for ( i = items; i < nrad; i++ ) wids[i] = wids[i-1];
      items = 1;
      (void) userreal_c( &vsysi, &items, &input_level, KEY_VSYS, MES_VSYS );
      items = 0;
      input_level = 0;                          /* no default */
      do {
         fint   ninp = nrad - items;            /* number of inputs */

         if (items) cancel_c( KEY_VROT );       /* cancel keyword */
         count = userreal_c( &vroti[items], &ninp, &input_level, KEY_VROT, MES_VROT );
         input_level = 1;                       /* default allowed */
         items += count;                        /* number of entries */
      } while (count && items < nrad);          /* loop control */
      for ( i = items; i < nrad; i++ ) vroti[i] = vroti[i-1];
      items = 0;
      input_level = 1;                          /* default */
      vexpi[0] = 0.0;                           /* 0.0 is default */
      do {
         fint   ninp = nrad - items;            /* number of inputs */

         if (items) cancel_c( KEY_VEXP );       /* cancel keyword */
         count = userreal_c( &vexpi[items], &ninp, &input_level, KEY_VEXP, MES_VEXP );
         input_level = 1;                       /* default allowed */
         items += count;                        /* number of entries */
      } while (count && items < nrad);          /* loop control */
      for ( i = ( items ? items : 1 ); i < nrad; i++ ) vexpi[i] = vexpi[i-1];
      items = 0;
      input_level = 0;                          /* no default */
      do {
         fint   ninp = nrad - items;            /* number of inputs */

         if (items) cancel_c( KEY_PA );         /* cancel keyword */
         count = userreal_c( &posai[items], &ninp, &input_level, KEY_PA, MES_PA );
         input_level = 1;                       /* default allowed */
         items += count;                        /* number of entries */
      } while (count && items < nrad);          /* loop control */
      for ( i = items; i < nrad; i++ ) posai[i] = posai[i-1];
      items = 0;
      input_level = 0;                          /* no default */
      do {
         fint   ninp = nrad - items;            /* number of inputs */

         if (items) cancel_c( KEY_INCL );       /* cancel keyword */
         count = userreal_c( &incli[items], &ninp, &input_level, KEY_INCL, MES_INCL );
         input_level = 1;                       /* default allowed */
         items += count;                        /* number of entries */
      } while (count && items < nrad);          /* loop control */
      for ( i = items; i < nrad; i++ ) incli[i] = incli[i-1];
      items = 1;
      (void) gdspos_c( cpos, &items, &input_level, KEY_CENTRE, MES_CENTRE,
         set, &subset );
      xposi = cpos[0];
      yposi = cpos[1];
      thetaf = 0.0;
      (void) userreal_c( &thetaf, &items, &input_level, KEY_FREEANGLE,
         MES_FREEANGLE );
   }
   /*
    * Next we enable the user to choose one (or more) out of several
    * options.
    */
   {
      char      inputbuf[MAXOPT*MAXLEN];
      char      listbuf[MAXOPT*MAXLEN];
      fchar     input[MAXOPT];
      fchar     list[MAXOPT];
      fint      error_level = 4;
      fint      input_level;
      fint      items;
      fint      nfixed;
      fint      ninp;
      fint      nlist;
      int       i;

      for (i = 0; i < MAXOPT; i++) {
         input[i].a = &inputbuf[i*MAXLEN];
         list[i].a = &listbuf[i*MAXLEN];
         input[i].l = MAXLEN;
         list[i].l = MAXLEN;
      }
      fcopy( input[0], "BOTH" );
      fcopy( list[0], "RECEDING" );
      fcopy( list[1], "APPROACHING" );
      fcopy( list[2], "BOTH" );
      input_level = 1;
      items = 1;
      nlist = 3;
      ninp = usercharu_c( input[0], &items, &input_level, KEY_SIDE, MES_SIDE );
      side = match_c( list[0], &nlist, input[0] );
      /*
       *  side =  1  ==>  receding,
       *          2  ==>  approaching,
       *          3  ==>  both;
       *
       */
      if ( side < 1 || side > 3 ) {
         error_c( &error_level, tofchar( "WRONG side of Galaxy!" ) );
      }
      fcopy( input[0], "UNIFORM" );
      fcopy( list[0], "UNIFORM" );
      fcopy( list[1], "COSINE" );
      fcopy( list[2], "COS-SQUARED" );
      input_level = 1;
      items = 1;
      nlist = 3;
      ninp = usercharu_c( input[0], &items, &input_level, KEY_WEIGHT,
         MES_WEIGHT );
      nlist = 3;
      switch( match_c( list[0], &nlist, input[0] ) ) {
         case 1: {
            wpow = 0;
            break;
         }
         case 2: {
            wpow = 1;
            break;
         }
         case 3: {
            wpow = 2;
            break;
         }
         default: {
            error_c( &error_level, tofchar( "WRONG weighting function!" ) );
            break;
         }
      }
      fcopy( list[0], "VSYS" );
      fcopy( list[1], "VROT" );
      fcopy( list[2], "VEXP" );
      fcopy( list[3], "PA" );
      fcopy( list[4], "INCL" );
      fcopy( list[5], "XPOS" );
      fcopy( list[6], "YPOS" );
      nlist = 7;
      items = 7;
      for ( i = 0; i < MAXPAR; mask[i++] = 1 );         /* reset */
      ninp = userchar_c( input[0], &items, &input_level, KEY_FIXED,
         MES_FIXED );
      for ( i = 0; i < ninp; i++ ) {
         fint   im = match_c( list[0], &nlist, input[i] );

         if ( im > 0 ) mask[im-1] = 0;
      }
      /*
       * We allow only fitting of systemic velocity and centre position
       * when both halves of the galaxy are used.
       */
      if ( side != 3 ) {
         mask[0] = mask[5] = mask[6] = 0;
      }
      /*
       * Count the number of fixed parameters.
       */
      for ( nfixed = 0, i = 0; i < MAXPAR; i++ ) {
         nfixed += ( 1 - mask[i] );
      }
      if ( nfixed == MAXPAR ) {
         error_c( &error_level, tofchar( "NO free parameters!" ) );
      }
/* =========================================== */
      items = 1;
      input_level = 0;
      (void) userint_c(&fitdeg,&items,&input_level,KEY_FITORD,MES_FITORD);
      fitdeg = 2*fitdeg + 1;
/* =========================================== */
   }


   /*
    * Get the parameters necessary for the Least-Squares fit.
    */
   {
      fint      input_level = 2;
      fint      items = 1;

      (void) userreal_c( &tol, &items, &input_level, KEY_TOLERANCE, MES_TOLERANCE );

   }

   {
     int i,j;

     outmat = fmatrix(blo[0],blo[1],bup[0],bup[1]);
     for(i=blo[0];i<=bup[0];i++){
        for(j=blo[1];j<=bup[1];j++){
                outmat[j][i] = blank;
        }
     }
   }
   /*
    * Loop over all concentric rings and do the fit.
    */
   {
     int        iring;

/* =========================================== */
     /* Ask user names of text files */
     {
      fint     dfault;
      fint     nitems;
      fint     agreed;
      bool     overwrite;
      char     message[256];

      do
      {
         agreed = YES;
         dfault = 1;   /* Request with default */
         strcpy( coeffb, "coefficients.txt" );
         sprintf( message, "Filename kinematic harmonic coeff.:  [%s]", coeffb );
         (void) usertext_c( Coeff, &dfault, KEY_FILE1, tofchar(message) );
         Coeff.a[nelc_c(Coeff)] = '\0';
         if (agreed)
         {
            /* Is this an existing file? */
            mp = fopen( coeffb, "r" );
            if (mp != NULL)
            {
               overwrite = toflog( NO );
               nitems = 1;
               dfault = 1;
               sprintf( message, "Ok to overwrite [%s]?    Y/[N]", coeffb );
               (void) userlog_c( &overwrite, &nitems, &dfault,
                                 tofchar("OVERWRITE="), tofchar(message) );
               overwrite = tobool( overwrite );
               cancel_c( tofchar("OVERWRITE=") );
               if (!overwrite)
               {
                  /* User does not want to overwrite file */
                  cancel_c( KEY_FILE1 );
                  agreed = NO;
               }
               fclose( mp );
            }
         }
         if (agreed)
         {
            mp = fopen( coeffb, "w" );
            if (mp == NULL)
            {
               anyoutf( 1, "Cannot open file [%s]! Reason unknown", coeffb );
               reject_c( KEY_FILE1, tofchar("Cannot open file !") );
               agreed = NO;
            }
         }
      }
      while (!agreed);


      if (yesinset2)
      {
         dfault = 1;
         do
         {
            agreed = YES;
            strcpy( hicofb, "hicoef.txt" );
            sprintf( message, "Filename surface density harmonic coeff.:  [%s]", hicofb );
            (void) usertext_c( Hicof, &dfault, KEY_FILE2, tofchar(message) );
            Hicof.a[nelc_c(Hicof)] = '\0';
            if (agreed)
            {
               /* Is this an existing file? */
               hicof = fopen( hicofb, "r" );
               if (hicof != NULL)
               {
                  overwrite = toflog( NO );
                  nitems = 1;
                  dfault = 1;
                  sprintf( message, "Ok to overwrite [%s]?    Y/[N]", hicofb );
                  (void) userlog_c( &overwrite, &nitems, &dfault,
                                    tofchar("OVERWRITE="), tofchar(message) );
                  overwrite = tobool( overwrite );
                  cancel_c( tofchar("OVERWRITE=") );
                  if (!overwrite)
                  {
                     /* User does not want to overwrite file */
                     cancel_c( KEY_FILE2 );
                     agreed = NO;
                  }
                  fclose( hicof );
               }
            }
            if (agreed)
            {
               hicof = fopen( hicofb, "w" );
               if (hicof == NULL)
               {
                  anyoutf( 1, "Cannot open [%s]! Reason unknown", hicofb );
                  reject_c( KEY_FILE2, tofchar("Cannot open file !") );
                  agreed = NO;
               }
            }
         }
         while (!agreed);
      }
    }
/* =========================================== */

    for ( nfit = 0, iring = 0; iring < nrad; iring++ ) {
         fint   n;
         float  e[MAXPAR];
         float  p[MAXPAR];
         float  ri, ro;
         float  q = 0.0;


         p[0] = vsysi;
         p[1] = vroti[iring];
         p[2] = vexpi[iring];
         p[3] = posai[iring];
         p[4] = incli[iring];
         p[5] = xposi;
         p[6] = yposi;
         ri = rads[iring] - 0.5 * wids[iring];
         ro = rads[iring] + 0.5 * wids[iring];
         if ( ri < 0.0 ) ri = 0.0;
         if ( rotfit( ri, ro, p, e, &n, &q ) > 0 ) {
            if (nfit < iring) {
               rads[nfit] = rads[iring];
               wids[nfit] = wids[iring];
            }
            vsysf[nfit] = p[0];
            if ( e[0] < 999.99 ) {
               vsyse[nfit] = e[0];
            } else {
               vsyse[nfit] = 999.99;
            }
            vrotf[nfit] = p[1];
            if ( e[1] < 999.99 ) {
               vrote[nfit] = e[1];
            } else {
               vrote[nfit] = 999.99;
            }
            vexpf[nfit] = p[2];
            if ( e[2] < 999.99 ) {
               vexpe[nfit] = e[2];
            } else {
               vexpe[nfit] = 999.99;
            }
            posaf[nfit] = p[3];
            if ( e[3] < 999.99 ) {
               posae[nfit] = e[3];
            } else {
               posae[nfit] = 999.99;
            }
            inclf[nfit] = p[4];
            if ( e[4] < 999.99 ) {
               incle[nfit] = e[4];
            } else {
               incle[nfit] = 999.99;
            }
            xposf[nfit] = p[5];
            if ( e[5] < 999.99 ) {
               xpose[nfit] = e[5];
            } else {
               xpose[nfit] = 999.99;
            }
            yposf[nfit] = p[6];
            if ( e[6] < 999.99 ) {
               ypose[nfit] = e[6];
            } else {
               ypose[nfit] = 999.99;
            }
            elp[nfit][0] = elp4[0];
            elp[nfit][1] = elp4[1];
            elp[nfit][2] = elp4[2];
            elp[nfit][3] = elp4[3];
            npts[nfit] = n;
            chis[nfit] = q;
            nfit += 1;
         }
      }
   }

/* =========================================== */
  fclose(mp);
  anyoutf( 1, "RESWRI wrote kinematic harmonic coefficients in file [%s]",
           coeffb );

  if (yesinset2)
  {
     fclose(hicof);
     anyoutf( 1, "RESWRI wrote surface density harmonic coefficients in file [%s]",
              hicofb );
  }

  {
     fint tid=0;
     fint i,j,xip,nwrite;

     if (outwri == 1)
     {
         xip = (bup[0]-blo[0]+1)*(bup[1]-blo[1]+1);
         gdsi_write_c( oset,&ocwlo,&ocwup,&outmat[blo[1]][blo[0]],&xip,&nwrite,&tid);
     }
  }
  freefmatrix(outmat,blo[0],blo[1]);

/* =========================================== */


   /*
    * Now we save the results in a table and a file.
    */
   if (nfit) {
      FILE      *f;
      char      filename[FILENAME_MAX+1];
      char      message[MAXMESLEN];
      char      tabnamb[9];
      char      text[MAXMESLEN];
      fchar     nulc;
      fchar     string;
      fchar     tabnam;
      fint      gerror;
      fint      i;
      fint      input_level = 1;
      fint      nfound;
      fint      nitems = 0;
      fint      error_level = 1;
      fint      output_level = 3;
      fint      one = 1;
      struct tm *lt;
      time_t    tp;

      tabnam.a = tabnamb; tabnam.l = 8;
      nulc.a = NULL; nulc.l = 0;
#ifdef TABLE
      for ( i = 99; i > 0; i-- ) {
         gerror = 0;
         sprintf( tabnamb, "reswri%2.2d", i );
         gdsa_tabinq_c( set, &subset, tabnam, nulc, &nitems, &nfound,
            &gerror );
         if ( gerror == -71 ) {
            break;
         }
      }
      i += 1;
      if (i == 100) {                   /* use scratch */
         i = 0;
      }
#endif
      sprintf( tabnamb, "reswri%2.2d", i );
      sprintf( filename, "reswri.%2.2d", i );
      {
         fchar  name;                   /* points to filename */
         int    n;                      /* number of characters entered */

         name.a = filename; name.l = sizeof( filename ) - 1;
         sprintf( message, MES_FILENAME, filename );
         n = usertext_c( name, &input_level, KEY_FILENAME, tofchar( message ) );
         if ( n ) filename[n] = 0;
      }
      sprintf( message, "Results will be stored in TABLE %s and FILE %s",
         tabnamb, filename );
      anyout_c( &output_level, tofchar( message ) );

#ifdef TABLE
      gerror = 0;
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "RADII" ),
         tofchar( "REAL" ), tofchar( "Radii of Rings" ), tofchar( "ARCSEC" ),
         &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "RADII" ), rads, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "WIDTHS" ),
         tofchar( "REAL" ), tofchar( "Width of Rings" ), tofchar( "ARCSEC" ),
         &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "WIDTHS" ), wids, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "VSYS" ),
         tofchar( "REAL" ), tofchar( "Systemic Velocity" ), tofchar( "KM/S" ),
         &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "VSYS" ), vsysf, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "EVSYS" ),
         tofchar( "REAL" ), tofchar( "Error in Systemic Velocity" ),
         tofchar( "KM/S" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "EVSYS" ), vsyse, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "VROT" ),
         tofchar( "REAL" ), tofchar( "Rotation Velocity" ), tofchar( "KM/S" ),
         &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "VROT" ), vrotf, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "EVROT" ),
         tofchar( "REAL" ), tofchar( "Error in Rotation Velocity" ),
         tofchar( "KM/S" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "EVROT" ), vrote, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "VEXP" ),
         tofchar( "REAL" ), tofchar( "Expansion Velocity" ), tofchar( "KM/S" ),
         &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "VEXP" ), vexpf, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "EVEXP" ),
         tofchar( "REAL" ), tofchar( "Error in Expansion Velocity" ),
         tofchar( "KM/S" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "EVEXP" ), vexpe, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "PA" ),
         tofchar( "REAL" ), tofchar( "Position Angle" ),
         tofchar( "DEGREES" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "PA" ), posaf, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "EPA" ),
         tofchar( "REAL" ), tofchar( "Error in Position Angle" ),
         tofchar( "DEGREES" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "EPA" ), posae, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "INCL" ),
         tofchar( "REAL" ), tofchar( "Inclination Angle" ),
         tofchar( "DEGREES" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "INCL" ), inclf, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "EINCL" ),
         tofchar( "REAL" ), tofchar( "Error in Inclination Angle" ),
         tofchar( "DEGREES" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "EINCL" ), incle, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "XPOS" ),
         tofchar( "REAL" ), tofchar( "X-position centre" ),
         tofchar( "GRIDS" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "XPOS" ), xposf, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "EXPOS" ),
         tofchar( "REAL" ), tofchar( "Error in X-position" ),
         tofchar( "GRIDS" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "EXPOS" ), xpose, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "YPOS" ),
         tofchar( "REAL" ), tofchar( "Y-position centre" ),
         tofchar( "GRIDS" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "YPOS" ), yposf, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "EYPOS" ),
         tofchar( "REAL" ), tofchar( "Error in Y-position" ),
         tofchar( "GRIDS" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "EYPOS" ), ypose, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "NPTS" ),
         tofchar( "INT" ), tofchar( "Points in Ring" ),
         tofchar( " " ), &gerror );
      gdsa_wcint_c( set, &subset, tabnam, tofchar( "NPTS" ), npts, &one,
         &nfit, &gerror );
      gdsa_crecol_c( set, &subset, tabnam, tofchar( "SIGMA" ),
         tofchar( "REAL" ), tofchar( "Sigma Velocity" ),
         tofchar( "KM/S" ), &gerror );
      gdsa_wcreal_c( set, &subset, tabnam, tofchar( "SIGMA" ), chis, &one,
         &nfit, &gerror );
#endif
      f = fopen( filename, "w" );
      if ( f == NULL ) {
         error_c( &error_level, tofchar( "Error opening file!" ) );
      }
      fmake( string, text );
      sprintf( message, "reswri  version %s (%s)", VERSION, __DATE__ );
#ifdef TABLE
      gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
#endif
      if ( f != NULL ) fprintf( f, "# %s\n", message );
      tp = time( NULL );
      lt = localtime( &tp );
      strftime( message, MAXMESLEN, "Date               : %b %d,%Y", lt );
#ifdef TABLE
      gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
#endif
      if (f != NULL) fprintf( f, "# %s\n", message );
      (void) usertext_c( string, &input_level, KEY_INSET, MES_INSET );
      sprintf( message, "Set                : %.*s", nelc_c( string ), text );
      if ( f != NULL ) fprintf( f, "# %s\n", message );
      sprintf( message, "free angle         : %5.1f(degrees)", thetaf );
#ifdef TABLE
      gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
#endif
      if ( f != NULL ) fprintf( f, "# %s\n", message );
      sprintf( message, "velocity field     : " );
      switch( side ) {
         case 1: { strcat( message, "RECEDING HALF" ); break; }
         case 2: { strcat( message, "APPROACHING HALF" ); break; }
         case 3: { strcat( message, "BOTH HALVES" ); break; }
         default: { strcat( message, "?" ); break; }
      }

#ifdef TABLE
      gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
#endif
      if ( f != NULL ) fprintf( f, "# %s\n", message );
      sprintf( message, "weighting function : " );
      switch( wpow ) {
         case 0: { strcat( message, "UNIFORM" ); break; }
         case 1: { strcat( message, "COSINE" ); break; }
         case 2: { strcat( message, "COS-SQUARED" ); break; }
         default: { strcat( message, "?" ); break; }
      }
#ifdef TABEL
      gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
#endif
      if ( f != NULL ) fprintf( f, "# %s\n", message );
      sprintf( message, "parameters         :" );
      for ( i = 0; i < MAXPAR; i++ ) {
         if ( mask[i] ) {
            switch( i ) {
               case 0: { strcat( message, " VSYS" ); break; }
               case 1: { strcat( message, " VROT" ); break; }
               case 2: { strcat( message, " VEXP" ); break; }
               case 3: { strcat( message, " PA" ); break; }
               case 4: { strcat( message, " INCL" ); break; }
               case 5: { strcat( message, " XPOS" ); break; }
               case 6: { strcat( message, " YPOS" ); break; }
               default: {
                  break;
               }
            }
         }
      }

#ifdef TABLE
      gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
#endif
      if ( f != NULL ) fprintf( f, "# %s\n# \n", message );
      if ( f != NULL ) {
         fprintf( f, "#   radius    width systemic  error rotation  error" );
         fprintf( f, " expansion  error    pos.  error" );
         fprintf( f, " incli-  error x-pos.  error y-pos." );
         fprintf( f, "  error npts   sigma\n" );
         fprintf( f, "#                   velocity        velocity       " );
         fprintf( f, "  velocity          angle       " );
         fprintf( f, " nation        centre        centre" );
         fprintf( f, "             velocity\n" );
         fprintf( f, "# (arcsec) (arcsec)   (km/s) (km/s)   (km/s) (km/s)" );
         fprintf( f, "    (km/s) (km/s)  (deg.) (deg.)" );
         fprintf( f, " (deg.) (deg.) (grid) (grid) (grid)" );
         fprintf( f, " (grid)        (km/s)\n" );

         for ( i = 0; i < nfit; i++ ) {
            fprintf( f, "%10.2f%9.2f%9.2f%7.2f%9.2f%7.2f%10.2f",
               (double) rads[i], (double) wids[i], (double) vsysf[i],
               (double) vsyse[i], (double) vrotf[i], (double) vrote[i],
               (double) vexpf[i] );
            fprintf( f, "%7.2f%8.2f%7.2f%7.2f%7.2f%7.2f",
               (double) vexpe[i], (double) posaf[i], (double) posae[i],
               (double) inclf[i], (double) incle[i], (double) xposf[i] );
            fprintf( f, "%7.2f%7.2f%7.2f%5d%#9.3g\n",
               (double) xpose[i], (double) yposf[i], (double) ypose[i],
               (int) npts[i], (double) chis[i] );
         }
         fclose( f );
      }
   }
   finis_c( );
   return( 0 );
}
