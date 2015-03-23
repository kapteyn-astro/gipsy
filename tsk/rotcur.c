/* rotcur.c

	Copyright (c) Kapteyn Astronomical Institute 1992
	All Rights Reserved.

#>            rotcur.dc1

Program:      ROTCUR

Purpose:      ROTCUR derives the kinematical parameters from the observed
              velocity field by fitting tilted-rings to the velocity field.

Category:     CALCULATION, DYNAMICS, ROTATION CURVES, VELOCITY FIELDS

File:         rotcur.c

Author:       K.G. Begeman

Keywords:

   INSET=     Set (and subset) of observed velocity field.

   BOX=       Select area of velocity field [entire velocity field].

   BUNIT=     If the map units are not found in the header, they must be
              supplied by the user [KM/S].

   RADII=     Give central radii of concentric rings. Maximum number of rings
              is 512. Units are arcsec.

   WIDTHS=    Give width of rings. If number of widths is less than the
              number of radii, the last supplied width will be used for the
              rest of the rings. Units are arcsec.

   VSYS=      Give initial estimate(s) of systemic velocity(ies) in km/s.
              If the number of systemic velocities is less than the number
              of rings, the last supplied systemic velocity will be used
              for the rest of the rings.

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

   FREEANGLE= Angle around minor axis in degrees within which radial
              velocities are discarded.

   SIDE=      Which half of the velocity field should be used in the
              fitting [RECEDING and APPROACHING half].

   WEIGHT=    There are three weighting functions available: the
              UNIFORM weighting function, were all points in a
              ring have equal weights, the [COSINE] weighting function,
              were each point in a ring is weighted with |cos(theta)|,
              and the COS-SQUARED weighting function, were each point is
              weighted with cos(theta)^2.

   FIXED=     Which parameter(s) should be kept fixed [NONE]. The parameters
              are named VSYS, VROT, VEXP, PA, INCL, XPOS and YPOS. If you don't
              want to fit the inclination and the systemic velocity, you
              should type: INCL VSYS. If a fit is wanted to only one half of
              the velocity field the parameters VSYS, XPOS and YPOS are
              automatically kept fixed.

** TOLERANCE= Tolerance of least-squares fitting [0.001].

** TABLE=     Store results in a table [Y].
              The results of the fitting are saved by defaults in a GDS
              table ROTCUR**, where ** is a number from 1 to 99. If
              table ROTCUR99 is already present, table ROTCUR00 will be
              used.

   FILENAME=  Name of text file to save results [rotcur**]. Default the
              name of the file is equal to the name of the table.

Description:  This program does a least-squares-fitting to the function:

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

Updates:      Jul 17, 1992: KGB, Document created.
              Mar 20, 1993: KGB, fitting one half of vfield repaired.
              Mar 23, 1993: KGB, fitting of expansion velocities.
              Jan 13, 1994: KGB, keyword FILENAME= added.
              May 20, 1994: VOG, MAXRING increased from 60 to 512
              Sep 26, 1995: KGB, keyword TABLE added, modified code because
                                 of slow GDS.
              Jul 16, 1996: KGB, allow different VSYS for each ring.
              Apr 01, 1998: VOG, Increased MAXPIXEL   
              May 06, 2000: VOG, Handle rings where all pixels have same
                                 value.
              Feb 17, 2011: VOG, Removed bug with table name

#<

*/


/*
 * Includes:
 */

#include	"float.h"		/* <float.h> */
#include	"math.h"		/* <math.h> */
#include	"stddef.h"		/* <stddef.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"time.h"		/* <time.h> */
#include	"gipsyc.h"		/* GIPSY definitions */
#include	"cmain.h"		/* C programme */
#include	"anyout.h"		/* anyout_c */
#include	"axtype.h"		/* axtype_c */
#include	"cancel.h"		/* cancel_c */
#include	"error.h"		/* error_c */
#include	"factor.h"		/* factor_c */
#include	"finis.h"		/* finis_c */
#include	"gdsa_crecol.h"		/* gdsa_crecol_c */
#include	"gdsa_tabinq.h"		/* gdsa_tabinq_c */
#include	"gdsa_wcint.h"		/* gdsa_wcint_c */
#include	"gdsa_wrcom.h"		/* gdsa_wrcom_c */
#include	"gdsa_wcreal.h"		/* gdsa_wcreal_c */
#include	"gdsbox.h"		/* gdsbox_c */
#include	"gdsc_fill.h"		/* gdsc_fill_c */
#include	"gdsc_name.h"		/* gdsc_name_c */
#include	"gdsd_rchar.h"		/* gdsd_rchar_c */
#include	"gdsd_rdble.h"		/* gdsd_rdble_c */
#include	"gdsd_rint.h"		/* gdsd_rint_c */
#include	"gdsd_wchar.h"		/* gdsd_wchar_c */
#include	"gdsd_wint.h"		/* gdsd_wint_c */
#include	"gdsinp.h"		/* gdsinp_c */
#include	"gdsi_read.h"		/* gdsi_read_c */
#include	"gdspos.h"		/* gdspos_c */
#include	"init.h"		/* init_c */
#include	"lsqfit.h"		/* lsqfit_c */
#include	"match.h"		/* match_c */
#include	"nelc.h"		/* nelc_c */
#include	"setfblank.h"		/* setfblank_c */
#include	"status.h"		/* status_c */
#include	"userchar.h"		/* userchar_c */
#include	"usercharu.h"		/* usercharu_c */
#include	"userlog.h"		/* userlog_c */
#include	"userreal.h"		/* userreal_c */
#include	"usertext.h"		/* usertext_c */

/*
 * Keywords and messages:
 */

#define	KEY_BOX		tofchar("BOX=")
#define	KEY_BUNIT	tofchar("BUNIT=")
#define	KEY_CENTRE	tofchar("CENTRE=")
#define	KEY_FILENAME	tofchar("FILENAME=")
#define	KEY_FIXED	tofchar("FIXED=")
#define	KEY_FREEANGLE	tofchar("FREEANGLE=")
#define	KEY_INCL	tofchar("INCL=")
#define	KEY_INSET	tofchar("INSET=")
#define	KEY_PA		tofchar("PA=")
#define	KEY_RADII	tofchar("RADII=")
#define	KEY_SIDE	tofchar("SIDE=")
#define	KEY_TABLE	tofchar("TABLE=")
#define	KEY_TOLERANCE	tofchar("TOLERANCE=")
#define	KEY_VEXP	tofchar("VEXP=")
#define	KEY_VROT	tofchar("VROT=")
#define	KEY_VSYS	tofchar("VSYS=")
#define	KEY_WEIGHT	tofchar("WEIGHT=")
#define	KEY_WIDTHS	tofchar("WIDTHS=")

#define	MES_BOX		tofchar("Area of velocity field [whole map]")
#define	MES_BUNIT	tofchar("Units of velocity field [KM/S]")
#define	MES_CENTRE	tofchar("Central position of velocity field")
#define	MES_FILENAME	"Enter name of text file for results [%s]"
#define	MES_FIXED	tofchar("Fixed parameters [NONE]")
#define	MES_FREEANGLE	tofchar("Free angle around minor axis")
#define	MES_INCL	tofchar("Inclination of rings in degrees")
#define	MES_INSET	tofchar("Input set (and subset) of velocity field")
#define	MES_PA		tofchar("Position Angles of rings in degrees")
#define	MES_RADII	tofchar("Central radii of rings in arcsec")
#define	MES_SIDE	tofchar("Which half of velocity field [BOTH]")
#define	MES_TABLE	tofchar("Store results in a GDS table [Y]")
#define	MES_TOLERANCE	tofchar("Tolerance of fit [0.001]")
#define	MES_VEXP	tofchar("Expansion velocities of rings in km/s [0.0]" )
#define	MES_VROT	tofchar("Rotation velocities of rings in km/s")
#define	MES_VSYS	tofchar("Systemic velocities of rings in km/s")
#define	MES_WEIGHT	tofchar("Weighting function [COSINE]")
#define	MES_WIDTHS	tofchar("Widths of rings in arcsec")


/*
 * Defines:
 */

#define	CLASS		1		/* class 1 programme */
#define	CLASSDIM	2		/* dimension of subset */
#define	F		0.0174532925	/* degrees to radians */
#define	G		0.4246609001	/* ? */
#define	MAXAXES		10		/* max. number of axes */
#define	MAXFITSLEN	18		/* max. length fits char */
#define	MAXLEN		20		/* max. length of text */
#define	MAXMESLEN	128		/* max. lenght of messages */
#define	MAXOPT		8		/* max. number of options */
#define	MAXPAR		7		/* max. number of parameters */
#define	MAXPIXEL	8192		/* max. numberof pixels in ring */
#define	MAXRING		512		/* max. number of rings */
#define	MAXSETNAMLEN	80		/* max. length of set name */
#define	MAXTEXTLEN	320		/* max. length of text input */
#define	VERSION		"1.4"		/* version.subversion number */

#define	fcopy( f, c )			\
	{int k;for(k=0;c[k]&&k<f.l;f.a[k]=c[k],k++);while(k<f.l)f.a[k++]=' ';}
#define	fmake( f, c )			\
	{f.a=c;f.l=sizeof(c);}
#define	max( x, y )			\
	(x>y?x:y)
#define	min( x, y )			\
	(x<y?x:y)
#define	nint( x )			\
	(x>0.0?(int)(x+0.5):(int)(x-0.5))
/*
 * variables:
 */

static	char	bset[MAXSETNAMLEN];	/* buffer for name of set */
static	double	cdelt[CLASSDIM];	/* grid separations */
static	double	mapfac;			/* convert map values to KM/S */
static	double	mapphi;			/* rotation of map */
static	fchar	set;			/* points to bset */
static	fint	blo[CLASSDIM];		/* lower edge of box */
static	fint	bup[CLASSDIM];		/* upper edge of box */
static	fint	subset;			/* subset level */
static	float	blank;			/* system blank */
static	float	*vfield;		/* buffer for velocity field */

/*
 * the parameters:
 */

static	fint	cor[2] = { -1.0, -1.0 };/* correlation ellips */
static	fint	mask[MAXPAR];		/* parameter fit mask */
static	fint	side;			/* which half of velocity field */
static	fint	wpow;			/* weighting power */
static	float	elp4[4];		/* coef matrix */
static	float	thetaf;			/* free angle */
static	float	tol = 0.001;		/* tolerance of fit */

static	fint	nfit;			/* number of fits */
static	fint	npts[MAXRING];		/* number of points */
static	fint	nrad;			/* number of rings */
static	float	chis[MAXRING];		/* chis */
static	float	elp[MAXRING][4];	/* coef. matrices */
static	float	rads[MAXRING];		/* radii of rings */
static	float	wids[MAXRING];		/* width of rings */

static	float	incle[MAXRING];		/* error in incl */
static	float	inclf[MAXRING];		/* fitted incl */
static	float	incli[MAXRING];		/* initial incl */
static	float	posae[MAXRING];		/* error in posa */
static	float	posaf[MAXRING];		/* fitted posa */
static	float	posai[MAXRING];		/* initial posa */
static	float	vexpe[MAXRING];		/* error in vexp */
static	float	vexpf[MAXRING];		/* fitted vexp */
static	float	vexpi[MAXRING];		/* initial vexp */
static	float	vrote[MAXRING];		/* error in vrot */
static	float	vrotf[MAXRING];		/* fitted vrot */
static	float	vroti[MAXRING];		/* initial vrot */
static	float	vsyse[MAXRING];		/* error in vsys */
static	float	vsysf[MAXRING];		/* fitted vsys */
static	float	vsysi[MAXRING];		/* initial vsys */
static	float	xpose[MAXRING];		/* error in xpos */
static	float	xposf[MAXRING];		/* fitted xpos */
static	float	xposi;			/* initial xpos */
static	float	ypose[MAXRING];		/* error in ypos */
static	float	yposf[MAXRING];		/* fitted ypos */
static	float	yposi;			/* initial ypos */


/*
 * func_c calculates radial velocity from rotation curve.
 */

float	func_c( float	c[CLASSDIM] ,	/* grid position in plane of galaxy */
                float	p[MAXPAR] ,	/* parameters of ring */
                fint	*m ,		/* dummy (number of parameters) */
                fint	*fopt )		/* option */
{
   float	vs, vc, vr;			/* parameters of velocity field */
   float	x, y;				/* sky coordinates */
   float	cost1, cost2, sint1, sint2, x1, y1, r;
   static float	phi= 0.0, inc = 0.0;		/* saved parameters */
   static float	cosp1 = 1.0, cosp2 = 1.0;
   static float	sinp1 = 0.0, sinp2 = 0.0;
   static float	cosi1 = 1.0, cosi2 = 1.0;
   static float	sini1 = 0.0, sini2 = 0.0;

   vs = p[0];					/* systemic velocity */
   vc = p[1];					/* circular velocity */
   vr = p[2];					/* expansion velocity */
   if (p[3] != phi) {				/* new position angle ? */
      phi = p[3];				/* position angle */
      cosp1 = cos( F * phi );			/* cosine */
      cosp2 = cosp1 * cosp1;			/* cosine squared */
      sinp1 = sin ( F * phi );			/* sine */
      sinp2 = sinp1 * sinp1;			/* sine squared */
   }
   if (p[4] != inc) {				/* new inclination ? */
      inc = p[4];				/* inclination */
      cosi1 = cos( F * inc );			/* cosine */
      cosi2 = cosi1 * cosi1;			/* cosine squared */
      sini1 = sin ( F * inc );			/* sine */
      sini2 = sini1 * sini1;			/* sine squared */
   }
   x = c[0] - p[5] * cdelt[0];			/* calculate x */
   y = c[1] - p[6] * cdelt[1];			/* calculate y */
   x1 = ( -x * sinp1 + y * cosp1 );		/* x in plane of galaxy */
   y1 = ( -x * cosp1 - y * sinp1 ) / cosi1;	/* y in plane of galaxy */
   r = sqrt( x1 * x1 + y1 * y1 );		/* distance from centre */
   cost1 = x1 / r;				/* cosine of angle in plane of galaxy */
   sint1 = y1 / r;				/* sine of angle in plane of galaxy */
   cost2 = cost1 * cost1;			/* cosine squared */
   sint2 = sint1 * sint1;			/* sine squared */

   return( vs + ( vc * cost1 + vr * sint1 ) * sini1 );	/* return to caller */
}


/*
 * function derv calculates the partial derivatives with respect
 * to the parameters.
 */

void	derv_c( float	c[CLASSDIM] ,	/* grid position in plane of galaxy */
                float	p[MAXPAR] ,	/* parameters of ring */
                float	d[MAXPAR] ,	/* for partial derivatives */
                fint	*m ,		/* dummy variable */
                fint	*fopt )		/* option */
{
   float	vc, vr;				/* parameters of velocity field */
   float	x, y;				/* sky coordinates */
   float	cost1, cost2, sint1, sint2, x1, y1, r;
   static float	phi= 0.0, inc = 0.0;		/* saved parameters */
   static float	cosp1 = 1.0, cosp2 = 1.0;
   static float	sinp1 = 0.0, sinp2 = 0.0;
   static float	cosi1 = 1.0, cosi2 = 1.0;
   static float	sini1 = 0.0, sini2 = 0.0;

   vc = p[1];					/* circular velocity */
   vr = p[2];					/* expansion velocity */
   if (p[3] != phi) {				/* new position angle ? */
      phi = p[3];				/* position angle */
      cosp1 = cos( F * phi );			/* cosine */
      cosp2 = cosp1 * cosp1;			/* cosine squared */
      sinp1 = sin ( F * phi );			/* sine */
      sinp2 = sinp1 * sinp1;			/* sine squared */
   }
   if (p[4] != inc) {				/* new inclination ? */
      inc = p[4];				/* inclination */
      cosi1 = cos( F * inc );			/* cosine */
      cosi2 = cosi1 * cosi1;			/* cosine squared */
      sini1 = sin ( F * inc );			/* sine */
      sini2 = sini1 * sini1;			/* sine squared */
   }
   x = c[0] - p[5] * cdelt[0];			/* calculate x */
   y = c[1] - p[6] * cdelt[1];			/* calculate y */
   x1 = ( -x * sinp1 + y * cosp1 );		/* x in plane of galaxy */
   y1 = ( -x * cosp1 - y * sinp1 ) / cosi1;	/* y in plane of galaxy */
   r = sqrt( x1 * x1 + y1 * y1 );		/* distance from centre */
   cost1 = x1 / r;				/* cosine of angle in plane of galaxy */
   sint1 = y1 / r;				/* sine of angle in plane of galaxy */
   cost2 = cost1 * cost1;			/* cosine squared */
   sint2 = sint1 * sint1;			/* sine squared */

   d[0] = 1.0;					/* partial derivative VSYS */
   d[1] = sini1 * cost1;			/* .................. VROT */
   d[2] = sini1 * sint1;			/* .................. VEXP */
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

static	fint	getdat( float x[] ,	/* sky coords of pixels inside ring */
                        float y[] ,	/* radial velocities */
                        float w[] ,	/* weights of radial velocities */
                        float p[] ,	/* parameters of ring */
                        float ri ,	/* inner radius of ring */
                        float ro ,	/* outer radius of ring */
                        float *q ,	/* chi-squared */
                        fint nfr )	/* number of degrees of freedom */
{
   fint		fopt;			/* function option */
   fint		llo, lup, mlo, mup;	/* corners */
   fint		l, m;			/* counters */
   fint		n = 0;			/* return value */
   fint		nlt, nmt;		/* box sizes */
   float	phi, inc, x0, y0;	/* define ellipse */
   float	free;			/* free angle */
   float	cosp, cosi, sinp, sini;	/* (co)sines */
   float	a, b;
   float	wi;

   (*q) = 0.0;				/* reset sigma */
   phi = p[3] + mapphi;			/* position angle plus map p.a. */
   inc = p[4];				/* inclination */
   x0 = p[5];				/* x-position of centre */
   y0 = p[6];				/* y-position of centre */
   free = fabs( sin( F * thetaf ) );	/* sine of free angle */
   sinp = sin( F * phi );		/* sine of pa. */
   cosp = cos( F * phi );		/* cosine of pa. */
   sini = sin( F * inc );		/* sine of inc. */
   cosi = cos( F * inc );		/* cosine of inc. */
   a = sqrt( 1.0 - cosp * cosp * sini * sini );
   b = sqrt( 1.0 - sinp * sinp * sini * sini );
   llo = max( blo[0], nint( x0 - a * ro / cdelt[0] ) );
   lup = min( bup[0], nint( x0 + a * ro / cdelt[0] ) );
   mlo = max( blo[1], nint( y0 - b * ro / cdelt[1] ) );
   mup = min( bup[1], nint( y0 + b * ro / cdelt[1] ) );
   if ( ( llo > lup ) || ( mlo > mup ) ) {
      fint	error_level = 1;

      error_c( &error_level, tofchar( "Ring not inside map!" ) );
      (*q) = FLT_MAX;
      return( 0 );
   }
   nlt = bup[0] - blo[0] + 1;		/* number of pixels in X */
   nmt = bup[1] - blo[1] + 1;		/* number of pixels in Y */
   for ( m = mlo; m < mup; m++) {
      float	ry = cdelt[1] * m;	/* Y position in plane of galaxy */

      for ( l = llo; l < lup; l++) {
         float	rx = cdelt[0] * l;	/* X position in plane of galaxy */
         float	v;
         int	ip;			/* array pointer */

         ip = ( m - blo[1] ) * nlt + l - blo[0];
         v = vfield[ip];		/* radial velocity at this position */
         if ( v != blank ) {
            float	costh, r, theta, xr, yr;

            xr = ( - ( rx - cdelt[0] * x0 ) * sinp +
               ( ry - cdelt[1] * y0 ) * cosp );
            yr = ( - ( rx - cdelt[0] * x0 ) * cosp -
               ( ry - cdelt[1] * y0 ) * sinp ) / cosi;
            r = sqrt( xr * xr + yr * yr );	/* distance from centre */
            if ( r < 0.1 ) {			/* radius to small ? */
               theta = 0.0;			/* set theta */
            } else {
               theta = atan2( yr, xr ) / F;
            }
            costh = fabs( cos ( F * theta ) );
            if ( r > ri && r < ro && costh > free ) {	/* point inside ring ? */
               float	xx[2];
               int	use = 0;

               wi = pow( costh, (double) wpow );	/* calculate weight of this point */
               xx[0] = rx;				/* x position */
               xx[1] = ry;				/* y position */
               switch( side ) {				/* which side of galaxy */
                  case 1: {				/* receding half */
                     use = ( fabs( theta ) <= 90.0 );
                     break;
                  }
                  case 2: {				/* approaching half */
                     use = ( fabs( theta ) >= 90.0 );
                     break;
                  }
                  case 3: {				/* both halves */
                     use = 1;
                     break;
                  }
                  default: {
                     break;
                  }
               }
               if ( use ) {				/* load data point ? */
                  n += 1;				/* increase */
                  if ( n < MAXPIXEL ) {			/* buffers not full */
                     fint	maxpar = MAXPAR;	/* number of parms. */
                     float	s, vz;

                     vz = func_c( xx, p, &maxpar, &fopt );
                     s = v - vz;			/* corrected difference */
                     x[2*n-2] = rx;			/* load X-coordinate */
                     x[2*n-1] = ry;			/* load Y-coordinate */
                     y[n-1] = v;			/* load radial velocity */
                     w[n-1] = wi;			/* load weight */
                     (*q) += s * s * wi;		/* calculate chi-squared */
                  }
               }
            }
         }
      }
   }
   if ( n > MAXPIXEL ) {
      char	m[80];
      fint	error_level = 1;

      sprintf( m, "Too many points in ring (%d); Maximum is %d", n, MAXPIXEL );
      error_c( &error_level, tofchar( m ) );
      n = MAXPIXEL;
   }
   if ( n > nfr ) {				/* enough data points ? */
      (*q) = sqrt( (*q) / (float) ( n - nfr ) );/* calculate sigma */
   } else {					/* no free parameters */
      (*q) = FLT_MAX;
   }
   return( n );					/* return to caller */
}

/*
 * function: rotfit
 *
 * This function does a least squares fit to the radial velocity field.
 */

static	fint	rotfit( float	ri ,	/* inner radius of ring */
                        float	ro ,	/* outer radius of ring */
                        float	p[] ,	/* estimated/fitted parameters */
                        float	e[] ,	/* errors in parameters */
                        fint	*n ,	/* nr of points in the fit */
                        float	*q )	/* chi-squared */
{
   static char	*fmt[MAXPAR] = {
      "%9.2f", "%9.2f", "%10.2f", "%9.2f", "%7.2f", "%9.2f", "%9.2f"
   };
   static char	*hed1[MAXPAR] = {
      " systemic", " rotation", " expansion", " position", " incli-",
      " x-centre", " y-centre"
   };
   static char	*hed2[MAXPAR] = {
      " velocity", " velocity", "  velocity", "   angle ", " nation",
      " position", " position"
   };
   char		fb[MAXMESLEN];		/* format buffer */
   char		mess[MAXMESLEN];	/* buffer for messages */
   fint		error_level = 1;	/* level of error */
   fint		fopt;			/* function option */
   fint		h, i, j;		/* counters */
   fint		ier = 0;		/* error return */
   fint		nfr;			/* number of free parameters */
   fint		nrt;			/* return code from lsqfit */
   fint		output_level = 3;	/* level of output */
   fint		stop;			/* stop ? */
   fint		t = 50;			/* max. number of iterations */
   float	b[MAXPAR];		/* partial derivatives */
   float	chi;			/* old chi-squared */
   float	df[MAXPAR];		/* difference vector */
   float	eps[MAXPAR];		/* contains stop criterium */
   float	flip;			/* direction */
   float	lab = 0.001;		/* mixing parameter */
   float	pf[MAXPAR];		/* intermediate results */
   float 	r;			/* mean radius of ring */
   float	x[2*MAXPIXEL];		/* (x,y) */
   float	y[MAXPIXEL];		/* f(x,y) */
   float	w[MAXPIXEL];		/* w(x,y) */

   for ( nfr = 0, i = 0; i < MAXPAR; i++ ) {
      eps[i] = 0.1;			/* convergence criterium */
      nfr += mask[i];
   }
   r = 0.5 * ( ri + ro );		/* mean radius of ring */
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
   h = 0;				/* reset iteration counter */
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
   do {					/* start of loop */
      fint	npar = MAXPAR;		/* number of parameters */
      fint	xdim = 2;		/* function is two-dimensional */

      h += 1;				/* next iteration */
      chi = (*q);			/* save chi-squared */
      for ( i = 0; i < MAXPAR; i++ ) {	/* loop to save initial estimates */
         pf[i] = p[i];
      }
      nrt = lsqfit_c( x, &xdim, y, w, n, pf, e, mask, &npar, &tol, &t,
         &lab, &fopt );
      if (nrt < 0 ) break;		/* stop because of error */
      for ( i = 0; i < MAXPAR; i++ ) {	/* calculate difference vector */
         df[i] = pf[i] - p[i];
      }
      flip = 1.0;			/* factor for inner loop */
      while (1) {			/* inner loop */
         for ( i = 0; i < MAXPAR; i++ ) {	/* calculate new parameters */
            pf[i] = flip * df[i] + p[i];
         }
         if ( pf[4] > 90.0 ) pf[4] -= 180.0;	/* in case inclination > 90 */
         (*n) = getdat( x, y, w, pf, ri, ro, q, nfr );
         if ( (*q) < chi ) {			/* better fit */
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
            for ( i = 0; i < MAXPAR; i++ ) {	/* save new parameters */
               p[i] = pf[i];
            }
            break;				/* leave inner loop */
         } else {
            if ( (2 * h) > t ) {
               for ( stop = 1, i = 0; i < MAXPAR; i++ ) {
                  stop = ( stop && ( fabs( flip * df[i] ) < eps[i] ) );
               }
            } else {
               /* Vog: may 6, 2000. Added condition for chi == 0 */
               /* to catch exception where all data is equal in a ring */
               if ((*q) == chi && chi == 0.0)
                  stop = 1;
               else
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
   if (stop) {					/* good fit */
      ier = h;					/* number of big loops */
   } else if ( nrt < 0 ) {			/* error from lsqfit */
      ier = nrt;
   } else if ( h == t ) {			/* max. number of iterations */
      ier = -4;
   }
   switch( ier ) {
      case -1: {
         error_c( &error_level,
            tofchar( "ROTCUR: -1 Too many free parameters!" ) );
         break;
      }
      case -2: {
         error_c( &error_level,
            tofchar( "ROTCUR: -2 No free parameters!" ) );
         break;
      }
      case -3: {
         error_c( &error_level,
            tofchar( "ROTCUR: -3 Not enough degrees of freedom!" ) );
         break;
      }
      case -4: {
         error_c( &error_level,
            tofchar( "ROTCUR: -4 Maximum number of iterations too small!" ) );
         break;
      }
      case -5: {
         error_c( &error_level,
            tofchar( "ROTCUR: -5 Diagonal of matrix contains zeroes!" ) );
         break;
      }
      case -6: {
         error_c( &error_level,
            tofchar( "ROTCUR: -6 Det. of the coeff. matrix is zero!" ) );
         break;
      }
      case -7: {
         error_c( &error_level,
            tofchar( "ROTCUR: -7 Square root of negative number!" ) );
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
      fint	i;
      fint	maxpar = MAXPAR;		/* max. number of parms. */
      float	a11 = 0.0;
      float	a12 = 0.0;
      float	a22 = 0.0;
      float	sigma2 = 0.0;

      for ( i = 0; i < (*n); i++ ) {
         derv_c( &x[2*i], p, b, &maxpar, &fopt );	/* calculate derivatives */
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
   return( ier );				/* return to caller */
}

MAIN_PROGRAM_ENTRY
{
   init_c( );					/* contact HERMES */
   IDENTIFICATION( "ROTCUR", VERSION );		/* our id */
   fmake( set, bset );				/* make fchar */
   setfblank_c( &blank );			/* get system blank */
   /*
    * First we need the set (and subset) which contains the velocity field,
    * and the box which includes the data. Then we check whether the
    * axis units are correct (should be convertable to arcsecs) and
    * the map units.
    */
   {
      char	bbunit[MAXFITSLEN+1];
      char	descriptor[MAXFITSLEN+1];	/* buffer for descriptor name */
      fchar	bunit;
      fint	axcount[MAXAXES];		/* axis sizes */
      fint	axperm[MAXAXES];		/* axis permutation */
      fint	box_mode = 0;			/* mode for gdsbox */
      fint	class = CLASS;			/* class of programme */
      fint	classdim = CLASSDIM;		/* dimension of subset */
      fint	default_level = 0;		/* no default */
      fint	error_level = 4;		/* fatal error */
      fint	gerror = 0;			/* GDS error return */
      fint	i;				/* loop counter */
      fint	maxaxes = MAXAXES;		/* max. number of axes */
      fint	maxsub = 1;			/* max. number of subsets */
      fint	output_level = 11;		/* level of output */
      fint	tcount = 0;			/* add ax types */

      fmake( bunit, bbunit );
      (void) gdsinp_c( set, &subset, &maxsub, &default_level, KEY_INSET,
         MES_INSET, &output_level, axperm, axcount, &maxaxes, &class,
         &classdim );
      default_level = 1;			/* default level for box */
      gdsbox_c( blo, bup, set, &subset, &default_level, KEY_BOX, MES_BOX,
         &output_level, &box_mode );
      /*
       * Now we create the buffer for the velocity field and read in the
       * Radial velocities.
       */
      {
         fint	cwlo, cwhi;			/* coordinate words */
         fint	ndone;				/* number of points read */
         fint	npoints;			/* size of buffer */
         fint	tid = 0;			/* transfer identifier */

         npoints = ( bup[1] - blo[1] + 1 ) * ( bup[0] - blo[0] + 1 );
         vfield = calloc( sizeof( float ), npoints );
         if ( vfield == NULL ) {
            fint	error_level = 4;		/* fatal error */

	    error_c( &error_level,
	       tofchar( "Error allocating buffer space!" ) );
	 }
         cwlo = gdsc_fill_c( set, &subset, blo );
         cwhi = gdsc_fill_c( set, &subset, bup );
         gdsi_read_c( set, &cwlo, &cwhi, vfield, &npoints, &ndone, &tid );
      }
      for ( i = 0; i < CLASSDIM; i++ ) {
         char	bctype[MAXFITSLEN+1];
         char	bcunit1[MAXFITSLEN+1];
         char	bcunit2[MAXFITSLEN+1];
         char	bdunit[MAXFITSLEN+1];
         fchar	ctype, cunit, cunit1, cunit2, dunit;
         fint	axnum = axperm[i];
         fint	axtyp;
         fint	level = 0;
         fint	skysys, prosys, velsys;

         fmake( ctype, bctype );
         fmake( cunit1, bcunit1 );
         fmake( cunit2, bcunit2 );
         fmake( dunit, bdunit );
         gdsc_name_c( ctype, set, &axnum, &gerror );
         axtyp = axtype_c( ctype, cunit1, dunit, &skysys, &prosys, &velsys );
         switch( axtyp ) {
            case 2: {				/* Latitude (get rotation) */
               sprintf( descriptor, "CROTA%d", axnum );
               gerror = 0;
               gdsd_rdble_c( set, tofchar( descriptor ), &level, &mapphi,
                  &gerror );
               if (gerror) { mapphi = 0.0; }
            }
            case 1: {				/* get cdelt in arcsec */
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
                  double	f;

                  (void) factor_c( cunit, tofchar( "ARCSEC" ), &f );
                  cdelt[i] *= f;
               }
               cdelt[i] = fabs( cdelt[i] );	/* make positive */
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
         fint	items = 1;

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
      fint	count;				/* input counter */
      fint	i;				/* loop counter */
      fint	input_level = 0;		/* no default */
      fint	items = MAXRING;		/* max. number of radii */
      double	cpos[CLASSDIM];			/* central position */

      nrad = userreal_c( rads, &items, &input_level, KEY_RADII, MES_RADII );
      items = userreal_c( wids, &nrad, &input_level, KEY_WIDTHS, MES_WIDTHS );
      for ( i = items; i < nrad; i++ ) wids[i] = wids[i-1];
      items = userreal_c( vsysi, &nrad, &input_level, KEY_VSYS, MES_VSYS );
      for ( i = items; i < nrad; i++ ) vsysi[i] = vsysi[i-1];
      items = 0;
      input_level = 0;				/* no default */
      do {
         fint	ninp = nrad - items;		/* number of inputs */

         if (items) cancel_c( KEY_VROT );	/* cancel keyword */
         count = userreal_c( &vroti[items], &ninp, &input_level, KEY_VROT, MES_VROT );
         input_level = 1;			/* default allowed */
         items += count;			/* number of entries */
      } while (count && items < nrad);		/* loop control */
      for ( i = items; i < nrad; i++ ) vroti[i] = vroti[i-1];
      items = 0;
      input_level = 1;				/* default */
      vexpi[0] = 0.0;				/* 0.0 is default */
      do {
         fint	ninp = nrad - items;		/* number of inputs */

         if (items) cancel_c( KEY_VEXP );	/* cancel keyword */
         count = userreal_c( &vexpi[items], &ninp, &input_level, KEY_VEXP, MES_VEXP );
         input_level = 1;			/* default allowed */
         items += count;			/* number of entries */
      } while (count && items < nrad);		/* loop control */
      for ( i = ( items ? items : 1 ); i < nrad; i++ ) vexpi[i] = vexpi[i-1];
      items = 0;
      input_level = 0;				/* no default */
      do {
         fint	ninp = nrad - items;		/* number of inputs */

         if (items) cancel_c( KEY_PA );		/* cancel keyword */
         count = userreal_c( &posai[items], &ninp, &input_level, KEY_PA, MES_PA );
         input_level = 1;			/* default allowed */
         items += count;			/* number of entries */
      } while (count && items < nrad);		/* loop control */
      for ( i = items; i < nrad; i++ ) posai[i] = posai[i-1];
      items = 0;
      input_level = 0;				/* no default */
      do {
         fint	ninp = nrad - items;		/* number of inputs */

         if (items) cancel_c( KEY_INCL );	/* cancel keyword */
         count = userreal_c( &incli[items], &ninp, &input_level, KEY_INCL, MES_INCL );
         input_level = 1;			/* default allowed */
         items += count;			/* number of entries */
      } while (count && items < nrad);		/* loop control */
      for ( i = items; i < nrad; i++ ) incli[i] = incli[i-1];
      items = 1;
      (void) gdspos_c( cpos, &items, &input_level, KEY_CENTRE, MES_CENTRE,
         set, &subset );
      xposi = cpos[0];
      yposi = cpos[1];
      (void) userreal_c( &thetaf, &items, &input_level, KEY_FREEANGLE,
         MES_FREEANGLE );
   }
   /*
    * Next we enable the user to choose one (or more) out of several
    * options.
    */
   {
      char	inputbuf[MAXOPT*MAXLEN];
      char	listbuf[MAXOPT*MAXLEN];
      fchar	input[MAXOPT];
      fchar	list[MAXOPT];
      fint	error_level = 4;
      fint	input_level;
      fint	items;
      fint	nfixed;
      fint	ninp;
      fint	nlist;
      int	i;

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
      fcopy( input[0], "COSINE" );
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
      for ( i = 0; i < MAXPAR; mask[i++] = 1 );		/* reset */
      ninp = userchar_c( input[0], &items, &input_level, KEY_FIXED,
         MES_FIXED );
      for ( i = 0; i < ninp; i++ ) {
         fint	im = match_c( list[0], &nlist, input[i] );

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
   }
   /*
    * Get the parameters necessary for the Least-Squares fit.
    */
   {
      fint	input_level = 2;
      fint	items = 1;

      (void) userreal_c( &tol, &items, &input_level, KEY_TOLERANCE, MES_TOLERANCE );

   }
   /*
    * Loop over all concentric rings and do the fit.
    */
   {
      int	iring;

      for ( nfit = 0, iring = 0; iring < nrad; iring++ ) {
         fint	n;
         float	e[MAXPAR];
         float	p[MAXPAR];
         float	ri, ro;
         float	q = 0.0;

         p[0] = vsysi[iring];
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
   /*
    * Now we save the results in a table and a file.
    */
   if (nfit) {
      FILE	*f;
      bool	table = TRUE;
      char	filename[FILENAME_MAX+1];
      char	message[MAXMESLEN];
      char	tabnamb[9];
      char	text[MAXMESLEN];
      fchar	nulc;
      fchar	string;
      fchar	tabnam;
      fint	gerror;
      fint	i;
      fint	input_level;
      fint	nfound;
      fint	nitems = 0;
      fint	error_level = 1;
      fint	output_level = 3;
      fint	one = 1;
      fint	runnr;
      struct tm	*lt;
      time_t	tp;

      input_level = 2;
      (void) userlog_c( &table, &one, &input_level, KEY_TABLE, MES_TABLE );
      gerror = 0;
      gdsd_rint_c( set, tofchar( "ROTCURNR" ), &subset, &runnr, &gerror );
      tabnam.a = tabnamb; tabnam.l = 8;      
      if ( gerror != subset ) {
         /* tabnam.a = tabnamb; tabnam.l = 8;  Moved to outside block. Otherwise could remain unitialized (VOG)*/
         nulc.a = NULL; nulc.l = 0;
         for ( runnr = 1; runnr < 100; runnr++ ) {
            gerror = 0;
            sprintf( tabnamb, "ROTCUR%2.2d", runnr );
            gdsa_tabinq_c( set, &subset, tabnam, nulc, &nitems, &nfound,
               &gerror );
            if ( gerror != -71 ) {
               break;
            }
         }
      }
      runnr += 1;
      if (runnr >= 100) {			/* use scratch */
         runnr = 0;
      } else if ( tobool( table ) ) {
         gerror = 0;
         gdsd_wint_c( set, tofchar( "ROTCURNR" ), &subset, &runnr, &gerror );
      }
      sprintf( tabnamb, "ROTCUR%2.2d", runnr );
      sprintf( filename, "rotcur.%2.2d", runnr );
      {
         fchar	name;			/* points to filename */
         int	n;			/* number of characters entered */

         name.a = filename; name.l = sizeof( filename ) - 1;
         sprintf( message, MES_FILENAME, filename );
         input_level = 1;
         n = usertext_c( name, &input_level, KEY_FILENAME, tofchar( message ) );
         if ( n ) filename[n] = 0;
      }
      if ( tobool( table ) ) {
         sprintf( message, "Results will be stored in TABLE %s and FILE %s",
            tabnamb, filename );
      } else {
         sprintf( message, "Results will be stored in FILE %s", filename );
      }
      anyout_c( &output_level, tofchar( message ) );
      if ( tobool( table ) ) {
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
            tofchar( "REAL" ), tofchar( "Systemic Velocity" ),
            tofchar( "KM/S" ), &gerror );
         gdsa_wcreal_c( set, &subset, tabnam, tofchar( "VSYS" ), vsysf, &one,
            &nfit, &gerror );
         gdsa_crecol_c( set, &subset, tabnam, tofchar( "EVSYS" ),
            tofchar( "REAL" ), tofchar( "Error in Systemic Velocity" ),
            tofchar( "KM/S" ), &gerror );
         gdsa_wcreal_c( set, &subset, tabnam, tofchar( "EVSYS" ), vsyse, &one,
            &nfit, &gerror );
         gdsa_crecol_c( set, &subset, tabnam, tofchar( "VROT" ),
            tofchar( "REAL" ), tofchar( "Rotation Velocity" ),
            tofchar( "KM/S" ), &gerror );
         gdsa_wcreal_c( set, &subset, tabnam, tofchar( "VROT" ), vrotf, &one,
            &nfit, &gerror );
         gdsa_crecol_c( set, &subset, tabnam, tofchar( "EVROT" ),
            tofchar( "REAL" ), tofchar( "Error in Rotation Velocity" ),
            tofchar( "KM/S" ), &gerror );
         gdsa_wcreal_c( set, &subset, tabnam, tofchar( "EVROT" ), vrote, &one,
            &nfit, &gerror );
         gdsa_crecol_c( set, &subset, tabnam, tofchar( "VEXP" ),
            tofchar( "REAL" ), tofchar( "Expansion Velocity" ),
            tofchar( "KM/S" ), &gerror );
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
      }
      f = fopen( filename, "w" );
      if ( f == NULL ) {
         error_c( &error_level, tofchar( "Error opening file!" ) );
      }
      fmake( string, text );
      sprintf( message, "ROTCUR  version %s (%s)", VERSION, __DATE__ );
      if ( tobool( table ) ) {
         gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
      }
      if ( f != NULL ) fprintf( f, "! %s\n", message );
      tp = time( NULL );
      lt = localtime( &tp );
      strftime( message, MAXMESLEN, "Date               : %b %d, %Y", lt );
      if ( tobool( table ) ) {
         gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
      }
      if (f != NULL) fprintf( f, "! %s\n", message );
      input_level = 2;
      (void) usertext_c( string, &input_level, KEY_INSET, MES_INSET );
      sprintf( message, "Set                : %.*s", nelc_c( string ), text );
      if ( f != NULL ) fprintf( f, "! %s\n", message );
      sprintf( message, "free angle         : %5.1f (degrees)", thetaf );
      if ( tobool( table ) ) {
         gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
      }
      if ( f != NULL ) fprintf( f, "! %s\n", message );
      sprintf( message, "velocity field     : " );
      switch( side ) {
         case 1: { strcat( message, "RECEDING HALF" ); break; }
         case 2: { strcat( message, "APPROACHING HALF" ); break; }
         case 3: { strcat( message, "BOTH HALVES" ); break; }
         default: { strcat( message, "?" ); break; }
      }
      if ( tobool( table ) ) {
         gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
      }
      if ( f != NULL ) fprintf( f, "! %s\n", message );
      sprintf( message, "weighting function : " );
      switch( wpow ) {
         case 0: { strcat( message, "UNIFORM" ); break; }
         case 1: { strcat( message, "COSINE" ); break; }
         case 2: { strcat( message, "COS-SQUARED" ); break; }
         default: { strcat( message, "?" ); break; }
      }
      if ( tobool( table ) ) {
         gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
      }
      if ( f != NULL ) fprintf( f, "! %s\n", message );
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
      if ( tobool( table ) ) {
         gdsa_wrcom_c( set, &subset, tabnam, tofchar( message ), &gerror );
      }
      if ( f != NULL ) fprintf( f, "! %s\n! \n", message );
      if ( f != NULL ) {
         fprintf( f, "!   radius    width systemic  error rotation  error" );
         fprintf( f, " expansion  error    pos.  error" );
         fprintf( f, " incli-  error x-pos.  error y-pos." );
         fprintf( f, "  error npts   sigma\n" );
         fprintf( f, "!                   velocity        velocity       " );
         fprintf( f, "  velocity          angle       " );
         fprintf( f, " nation        centre        centre" );
         fprintf( f, "             velocity\n" );
         fprintf( f, "! (arcsec) (arcsec)   (km/s) (km/s)   (km/s) (km/s)" );
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
