/* dssload.c

        Copyright (c) Kapteyn Laboratorium Groningen 1996
        All Rights Reserved.

#>            dssload.dc1

Program:      DSSLOAD

Purpose:      Loads a Digitized Sky Survey FITS file into a GIPSY set.

Category:     COORDINATES, FITS, UTILITY

File:         dssload.c

Author:       K.G. Begeman

Keywords:

   FILENAME=  Name of FITS file containing DSS image. This is the file
              you probably obtained with the GetImage program.

   OUTSET=    Name of output set [Object name from FITS file].

Description:  The Digitized Sky Survey image in FITS format will be
              imported into GIPSY by fitting a rectangular projection
              to the image. The program will report the accuracy of the
              fit. The input FITS file is usually obtained with the
              GetImage program supplied with the DSS.

Updates:      May 20, 1996: KGB Document created.
              Apr 28, 1998: KGB Automatically decreasing lambda.
              May  1, 2007: JPT Renamed bzero to b_zero.
              Aug 06, 2013: VOG Added extra hyphen in CTYPE (RA---ARC-DSS) to
                                comply with FITS standard

#<

*/

/*
 * Standard includes:
 */

#include	"math.h"		/* <math.h> */
#include	"stdarg.h"		/* <stdarg.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */

/*
 * GIPSY setup:
 */

#include	"gipsyc.h"		/* GIPSY Symbols and Definitions */
#include	"cmain.h"		/* GIPSY program in c */

/*
 * Function templates:
 */

#include	"finis.h"		/* finis_c */
#include	"ftsd_find.h"		/* ftsd_find_c */
#include	"ftsd_rchar.h"		/* ftsd_rchar_c */
#include	"ftsd_rdble.h"		/* ftsd_rdble_c */
#include	"ftsd_rint.h"		/* ftsd_rint_c */
#include	"ftsd_rreal.h"		/* ftsd_rreal_c */
#include	"gds_close.h"		/* gds_close_c */
#include	"gds_create.h"		/* gds_create_c */
#include	"gds_exist.h"		/* gds_exist_c */
#include	"gds_extend.h"		/* gds_extend_c */
#include	"gdsc_range.h"		/* gdsc_range_c */
#include	"gdsd_wchar.h"		/* gdsc_wchar_c */
#include	"gdsd_wdble.h"		/* gdsd_wdble_c */
#include	"gdsd_wfits.h"		/* gdsd_wfits_c */
#include	"gdsd_wint.h"		/* gdsd_wint_c */
#include	"gdsd_wreal.h"		/* gdsd_wreal_c */
#include	"gdsi_write.h"		/* gdsi_write_c */
#include	"init.h"		/* init_c */
#include	"minmax4.h"		/* minmax4_c */
#include	"nelc.h"		/* nelc_c */
#include	"proco.h"		/* proco_c */
#include	"skyfit.h"		/* skyfit_c */
#include	"stabar.h"		/* stabar_c */
#include	"userfio.h"		/* userfxxx stuff */

typedef	unsigned char	ubyte;		/* define unsigned byte */
typedef	signed char	sbyte;		/* define signed byte */

#define ARCSEC_PER_RADIAN	(3600.0*DEGREES_PER_RADIAN)
#define	DEGREES_PER_RADIAN	57.295779513082320876798155
#define	FITS_RECLEN		80
#define	FITS_BLOCKLEN		2880
#define	FITS_BLOCKED		10
#define	MAX_AMD			20
#define	MAX_DATA		(FITS_BLOCKLEN*FITS_BLOCKED)
#define	MAX_DIFF		51
#define	MAX_ITERATIONS		500
#define	MAX_LOOP_1		4
#define	MAX_POS			13
#define	PI			3.1415926535
#define	RADIANS_PER_ARCSEC	(RADIANS_PER_DEGREE/3600.0)
#define	RADIANS_PER_DEGREE	0.017453292519943295769237
#define	TOLERANCE		0.0000005

static	char	*Keys[] = {
   "DATE"    , "ORIGIN"  , "PLTLABEL", "PLATEID" , "REGION"  , "DATE-OBS",
   "UT"      , "PLTRAH"  , "PLTRAM"  , "PLTRAS"  , "PLTDECSN", "PLTDECD" ,
   "PLTDECM" , "PLTDECS" , "EQUINOX" , "EXPOSURE", "BANDPASS", "PLTGRADE",
   "PLTSCALE", "SITELAT" , "SITELONG", "TELESCOP", "CNPIX1"  , "CNPIX2"  ,
   "DATATYPE", "SCANIMG" , "SCANNUM" , "DCHOPPED", "DSHEARED", "DSCNDNUM",
   "XPIXELSZ", "YPIXELSZ", "PPO1"    , "PPO2"    , "PPO3"    , "PPO4"    ,
   "PPO5"    , "PPO6"    , "AMDX1"   , "AMDX2"   , "AMDX3"   , "AMDX4"   ,
   "AMDX5"   , "AMDX6"   , "AMDX7"   , "AMDX8"   , "AMDX9"   , "AMDX10"  ,
   "AMDX11"  , "AMDX12"  , "AMDX13"  , "AMDX14"  , "AMDX15"  , "AMDX16"  ,
   "AMDX17"  , "AMDX18"  , "AMDX19"  , "AMDX20"  , "AMDY1"   , "AMDY2"   ,
   "AMDY3"   , "AMDY4"   , "AMDY5"   , "AMDY6"   , "AMDY7"   , "AMDY8"   ,
   "AMDY9"   , "AMDY10"  , "AMDY11"  , "AMDY12"  , "AMDY13"  , "AMDY14"  ,
   "AMDY15"  , "AMDY16"  , "AMDY17"  , "AMDY18"  , "AMDY19"  , "AMDY20"  ,
   "OBJECT"  , "OBJCTRA" , "OBJCTDEC", "OBJCTX"  , "OBJCTY"  ,
};

static	char	Object[FITS_RECLEN];
static	char	record[FITS_RECLEN];
static	double	amdx[MAX_AMD], amdy[MAX_AMD];
static	double	a_c, d_c, x_c, y_c, d_x, d_y, r_x, r_y, O_X, O_Y, O_R, O_D;
static	double	plt_scale;
static	double	cdelt1, cdelt2;
static	double	crval1, crval2;
static	double	crota2;
static	double	crpix1, crpix2;
static	fchar	Head, Record;
static	fint	bitpix;
static	fint	naxis, naxis1, naxis2;
static	float	bscale, b_zero;

static	int	HeadOnly = 0;

static	fchar	tofcharf( char fmt[], ... )
{
   static char	buffer[10][1000];
   static int	nbuf = 0;
   fchar	r;
   va_list	args;

   va_start( args, fmt );
   r.a = buffer[nbuf++]; nbuf %= 10;
   r.l = vsprintf( r.a, fmt, args );
   return( r );
}

static	void	Error( int level, char fmt[], ... )
{
   char		string[2000];
   va_list	args;

   va_start( args, fmt );
   vsprintf( string, fmt, args );
   errorf( level, string );
}

static	void	Output( int level, char fmt[], ... )
{
   char		string[2000];
   va_list	args;

   va_start( args, fmt );
   vsprintf( string, fmt, args );
   anyoutf( level, string );
}

static	void	amdinv( double R, double D, double *X, double *Y )
{
   double	xi, eta, o_x, o_y;
   int		i;

   {
      double	rr = R * RADIANS_PER_DEGREE;
      double	dd = D * RADIANS_PER_DEGREE;
      double	dc = d_c * RADIANS_PER_DEGREE;
      double	ac = a_c * RADIANS_PER_DEGREE;
      double	div;

      div = ( sin( dd ) * sin( dc ) + cos ( dd ) * cos( dc ) * cos( rr - ac ) );
      xi  = cos( dd ) * sin( rr - ac ) * (ARCSEC_PER_RADIAN) / div;
      eta = ( sin( dd ) * cos( dc ) - cos( dd ) * sin( dc ) * cos( rr - ac ) ) *
         (ARCSEC_PER_RADIAN) / div;
   }
   o_x = xi  / plt_scale;
   o_y = eta / plt_scale;
   for ( i = 0; i < MAX_ITERATIONS; i++ ) {
      double	delta_x, delta_y, f, fx, fy, g, gx, gy;

      {
         double	x = o_x, y = o_y;
         double	cjunk, x4, y4, xy2;

         x4 = ( x * x ) * ( x * x );
         y4 = ( y * y ) * ( y * y );
         xy2 = ( x * x + y * y );
         cjunk = xy2 * xy2;
         f =  amdx[0]  * x +
              amdx[1]  * y +
              amdx[2]  +
              amdx[3]  * x * x +
              amdx[4]  * x * y +
              amdx[5]  * y * y +
              amdx[6]  * xy2 +
              amdx[7]  * x * x * x +
              amdx[8]  * x * x * y +
              amdx[9]  * x * y * y +
              amdx[10] * y * y * y +
              amdx[11] * x * xy2 +
              amdx[12] * x * cjunk;
         fx = amdx[0]  +
              amdx[3]  * 2.0 * x +
              amdx[4]  * y +
              amdx[6]  * 2.0 * x +
              amdx[7]  * 3.0 * x * x +
              amdx[8]  * 2.0 * x * y +
              amdx[9]  * y * y +
              amdx[11] * ( 3.0 * x * x + y * y ) +
              amdx[12] * ( 5.0 * x4 + 6.0 * x * x * y * y + y4 );
         fy = amdx[1]  +
              amdx[4]  * x +
              amdx[5]  * 2.0 * y +
              amdx[6]  * 2.0 * y +
              amdx[8]  * x * x +
              amdx[9]  * 2.0 * x * y +
              amdx[10] * 3.0 * y * y +
              amdx[11] * 2.0 * x * y +
              amdx[12] * 4.0 * x * y * xy2;
         g  = amdy[0]  * y +
              amdy[1]  * x +
              amdy[2]  +
              amdy[3]  * y * y +
              amdy[4]  * y * x +
              amdy[5]  * x * x +
              amdy[6]  * xy2 +
              amdy[7]  * y * y * y +
              amdy[8]  * y * y * x +
              amdy[9]  * y * x * x +
              amdy[10] * x * x * x +
              amdy[11] * y * xy2 +
              amdy[12] * y * cjunk;
         gx = amdy[1]  +
              amdy[4]  * y +
              amdy[5]  * 2.0 * x +
              amdy[6]  * 2.0 * x +
              amdy[8]  * y * y +
              amdy[9]  * 2.0 * y * x +
              amdy[10] * 3.0 * x * x +
              amdy[11] * 2.0 * x * y +
              amdy[12] * 4.0 * x * y * xy2;
         gy = amdy[0]  +
              amdy[3]  * 2.0 * y +
              amdy[4]  * x +
              amdy[6]  * 2.0 * y +
              amdy[7]  * 3.0 * y * y +
              amdy[8]  * 2.0 * y * x +
              amdy[9]  * x * x +
              amdy[11] * ( x * x + 3.0 * y * y ) +
              amdy[12] * ( 5.0 * y4 + 6.0 * x * x * y * y + x4 );
      }
      f = f - xi;
      g = g - eta;
      delta_x = ( -f * gy + g * fy ) / ( fx * gy - fy * gx );
      delta_y = ( -g * fx + f * gx ) / ( fx * gy - fy * gx );
      o_x += delta_x;
      o_y += delta_y;
      if ( ( fabs( delta_x ) < TOLERANCE ) && ( fabs( delta_y ) < TOLERANCE ) ) break;
   }
   (*X) = ( x_c - o_x * 1000.0 ) / d_x;
   (*Y) = ( y_c + o_y * 1000.0 ) / d_y;
}

static	void	amdpos( double X, double Y, double *R, double *D )
{
   double	x, y;
   double	xi, eta;
   double	dc = d_c * RADIANS_PER_DEGREE;
   double	ac = a_c * RADIANS_PER_DEGREE;

   x = ( x_c - d_x * X ) / 1000.0;
   y = ( d_y * Y - y_c ) / 1000.0;
   xi  = amdx[0]  * x +
         amdx[1]  * y +
         amdx[2]  +
         amdx[3]  * x * x +
         amdx[4]  * x * y +
         amdx[5]  * y * y +
         amdx[6]  * ( x * x + y * y ) +
         amdx[7]  * x * x * x +
         amdx[8]  * x * x * y +
         amdx[9]  * x * y * y +
         amdx[10] * y * y * y +
         amdx[11] * x * ( x * x + y * y ) +
         amdx[12] * x * ( x * x + y * y ) * ( x * x + y * y );
   eta = amdy[0]  * y +
         amdy[1]  * x +
         amdy[2]  +
         amdy[3]  * y * y +
         amdy[4]  * y * x +
         amdy[5]  * x * x +
         amdy[6]  * ( y * y + x * x ) +
         amdy[7]  * y * y * y +
         amdy[8]  * y * y * x +
         amdy[9]  * y * x * x +
         amdy[10] * x * x * x +
         amdy[11] * y * ( y * y + x * x ) +
         amdy[12] * y * ( y * y + x * x ) * ( y * y + x * x );
   xi  *= RADIANS_PER_ARCSEC;
   eta *= RADIANS_PER_ARCSEC;
   (*R) = atan( ( xi / cos( dc ) ) / ( 1.0 - eta * tan( dc ) ) ) + ac;
   (*D) = atan( ( ( eta + tan( dc ) ) * cos( (*R) - ac ) ) /
          ( 1.0 - eta * tan( dc ) ) );
   (*R) *= DEGREES_PER_RADIAN;
   (*D) *= DEGREES_PER_RADIAN;
}

static	int	setup_coords( fchar Head, fchar Record )
{
   double	a_g, d_g;
   double	d1, d2, d3;
   fint		nrec;
   int		n;

   nrec = ftsd_rchar_c( Head, tofchar( "REGION" ), Record );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item REGION!", nrec );
   Output( 0, "Region                      : %.*s", (int) nelc_c( Record ), Record.a );
   nrec = ftsd_rint_c( Head, tofchar( "NAXIS" ), &naxis );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item NAXIS!", nrec );
   if ( naxis != 2 ) Error( FATAL, "Naxis (=%1d) must be 2|", naxis );
   nrec = ftsd_rint_c( Head, tofchar( "NAXIS1" ), &naxis1 );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item NAXIS1!", nrec );
   nrec = ftsd_rint_c( Head, tofchar( "NAXIS2" ), &naxis2 );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item NAXIS2!", nrec );
   nrec = ftsd_rdble_c( Head, tofchar( "PLTRAH" ), &d1 );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item PLTRAH!", nrec );
   nrec = ftsd_rdble_c( Head, tofchar( "PLTRAM" ), &d2 );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item PLTRAM!", nrec );
   nrec = ftsd_rdble_c( Head, tofchar( "PLTRAS" ), &d3 );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item PLTRAS!", nrec );
   a_c = 15.0 * ( d1 + d2 / 60.0 + d3 / 3600.0 );
   nrec = ftsd_rdble_c( Head, tofchar( "PLTDECD" ), &d1 );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item PLTDECD!", nrec );
   nrec = ftsd_rdble_c( Head, tofchar( "PLTDECM" ), &d2 );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item PLTDECM!", nrec );
   nrec = ftsd_rdble_c( Head, tofchar( "PLTDECS" ), &d3 );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item PLTDECS!", nrec );
   d_c = ( d1 + d2 / 60.0 + d3 / 3600.0 );
   nrec = ftsd_rdble_c( Head, tofchar( "PLTSCALE" ), &plt_scale );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item PLTSCALE!", nrec );
   for ( n = 0; n < MAX_AMD; n++ ) {
      nrec = ftsd_rdble_c( Head, tofcharf( "AMDX%1d", n + 1 ), &amdx[n] );
      if ( nrec < 0 ) Error( FATAL, "Error obtaining item AMDX%1d", n + 1 );
      nrec = ftsd_rdble_c( Head, tofcharf( "AMDY%1d", n + 1 ), &amdy[n] );
      if ( nrec < 0 ) Error( FATAL, "Error obtaining item AMDY%1d", n + 1 );
   }
   nrec = ftsd_rchar_c( Head, tofchar( "PLTDECSN" ), Record );
   if ( Record.a[0] == '+' ) {
   } else if ( Record.a[0] == '-' ) {
      d_c *= -1.0;
   } else {
      Error( FATAL, "Unexpected Error!" );
   }
   nrec = ftsd_rdble_c( Head, tofchar( "XPIXELSZ" ), &d_x );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item XPIXELSZ!", nrec );
   nrec = ftsd_rdble_c( Head, tofchar( "YPIXELSZ" ), &d_y );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item YPIXELSZ!", nrec );
   nrec = ftsd_rdble_c( Head, tofchar( "PPO3" ), &x_c );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item PPO3!", nrec );
   nrec = ftsd_rdble_c( Head, tofchar( "PPO6" ), &y_c );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item PPO6!", nrec );
   Output( 0, "Plate Centres (alpha,delta) : %#13.7g, %#13.7g", a_c, d_c );
   amdinv( a_c, d_c, &a_g, &d_g );
   {
      double	ac, dc;
      amdpos( a_g, d_g, &ac, &dc );
   }
   crval1 = a_c;
   crval2 = d_c;
   {
      double	crval1, crval2;

      amdpos( a_g, d_g, &crval1, &crval2 );
   }
   nrec = ftsd_rdble_c( Head, tofchar( "CNPIX1" ), &r_x );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item CNPIX1!", nrec );
   nrec = ftsd_rdble_c( Head, tofchar( "CNPIX2" ), &r_y );
   if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item CNPIX2!", nrec );
   Output( 0, "Pixel Corner  (pixel,pixel) : %#13.7g, %#13.7g", r_x, r_y );
   if ( !HeadOnly ) {
      nrec = ftsd_rint_c( Head, tofchar( "BITPIX" ), &bitpix );
      if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item BITPIX!", nrec );
      nrec = ftsd_rreal_c( Head, tofchar( "BSCALE" ), &bscale );
      if ( nrec < 0 ) bscale = 1.0;
      nrec = ftsd_rreal_c( Head, tofchar( "BZERO" ), &b_zero );
      if ( nrec < 0 ) b_zero = 0.0;
      nrec = ftsd_rchar_c( Head, tofchar( "OBJECT" ), Record );
      if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item OBJECT!", nrec );
      Output( 0, "Object                      : %.*s", (int)nelc_c( Record ), Record.a );
      strncpy( Object, Record.a, nelc_c( Record ) );
      Object[nelc_c( Record )] = 0;
      nrec = ftsd_rchar_c( Head, tofchar( "OBJCTRA" ), Record );
      if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item OBJCTRA!", nrec );
      Output( 0, "Object R.A.                 : %.*s", (int)nelc_c( Record ), Record.a );
      nrec = ftsd_rchar_c( Head, tofchar( "OBJCTDEC" ), Record );
      if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item OBJCTDEC!", nrec );
      Output( 0, "Object DEC.                 : %.*s", (int)nelc_c( Record ), Record.a );
      nrec = ftsd_rdble_c( Head, tofchar( "OBJCTX" ), &O_X );
      if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item OBJCTX!", nrec );
      nrec = ftsd_rdble_c( Head, tofchar( "OBJCTY" ), &O_Y );
      if ( nrec < 0 ) Error( FATAL, "Error (%d) obtaining item OBJCTY!", nrec );
      amdpos( O_X, O_Y, &O_R, &O_D );
      Output( 0, "Position of Object (grid)   : %#13.7g, %#13.7g", O_X, O_Y );
      Output( 0, "Position of Object (phys)   : %#13.7g, %#13.7g", O_R, O_D );
      amdinv( O_R, O_D, &O_X, &O_Y );
      Output( 0, "Position of Object (grid)   : %#13.7g, %#13.7g", O_X, O_Y );
      d1 = O_R / 15.0;
      d2 = (int) d1;
      d1 = ( d1 - d2 ) * 60;
      d3 = (int) d1;
      d1 = ( d1 - d3 ) * 60;
      Output( 0, "R.A. of Object              : %3d %2d %6.3f",
         (int) d2, (int) d3, d1 );
      d1 = fabs( O_D );
      d2 = (int) d1;
      d1 = ( d1 - d2 ) * 60;
      d3 = (int) d1;
      d1 = ( d1 - d3 ) * 60;
      Output( 0, "DEC. of Object              : %c%2d %2d %6.3f",
         (O_D<0.0?'-':'+'),(int) d2, (int) d3, d1 );
   }
   cdelt1 = -plt_scale / 3600.0 * d_x / 1000.0;
   cdelt2 =  plt_scale / 3600.0 * d_y / 1000.0;
   crpix1 = a_g;
   crpix2 = d_g;
   crota2 = 0.0;
   {
      double	X[MAX_POS], Y[MAX_POS], A[MAX_POS], D[MAX_POS];
      double	XF[MAX_POS], YF[MAX_POS];
      double	FPAR[7], EPAR[7];
      fint	MPAR[7] = { 0, 0, 1, 1, 1, 1, 1 }, ITS = 500, PROJ = 6;
      fint	go = MAX_LOOP_1, i, N = MAX_POS;
      float	LAB = 0.01, TOL = 0.00001;

      X[0] = X[3] = X[6] = 1.0;
      X[1] = X[4] = X[7] = ( naxis1 + 1.0 ) / 2.0;
      X[2] = X[5] = X[8] = naxis1;
      X[9] = X[11] = ( X[0] + X[1] ) / 2.0;
      X[10] = X[12] = ( X[2] + X[1] ) / 2.0;
      Y[0] = Y[1] = Y[2] = 1.0;
      Y[3] = Y[4] = Y[5] = ( naxis2 + 1.0 ) / 2.0;
      Y[6] = Y[7] = Y[8] = naxis2;
      Y[9] = Y[10] = ( Y[0] + Y[3] ) / 2.0;
      Y[11] = Y[12] = ( Y[3] + Y[6] ) / 2.0;
      for ( i = 0; i < MAX_POS; i++ ) {
         double	x, y;

         x = X[i] + ( r_x - 1.0 ); y = Y[i] + ( r_y - 1.0 );
         amdpos( x, y, &A[i], &D[i] );
      }
      do {
         FPAR[0] = crval1;
         FPAR[1] = crval2;
         FPAR[2] = crota2;
         FPAR[3] = cdelt1;
         FPAR[4] = cdelt2;
         FPAR[5] = crpix1;
         FPAR[6] = crpix2;
         i = skyfit_c( X, Y, A, D, &N, XF, YF, FPAR, EPAR, MPAR, &TOL, &ITS, &LAB, &PROJ );
         Output( 0, "skyfit returns: %d", i );
         LAB *= 0.1; go--;
      } while ( i <= 0 && go );
      if ( i > 0 ) {
         double	sp = 0.0;
         fint	count, ccount;
         fint	dcount[MAX_DIFF];
         fint	MODE = 0;
         int	i, j;

         Output( 0, "CRVAL1  = %#20.14g [+/- %#13.7g]", FPAR[0], EPAR[0] );
         crval1 = FPAR[0];
         Output( 0, "CRVAL2  = %#20.14g [+/- %#13.7g]", FPAR[1], EPAR[1] );
         crval2 = FPAR[1];
         Output( 0, "CROTA2  = %#20.14g [+/- %#13.7g]", FPAR[2], EPAR[2] );
         crota2 = FPAR[2];
         Output( 0, "CDELT1  = %#20.14g [+/- %#13.7g]", FPAR[3], EPAR[3] );
         cdelt1 = FPAR[3];
         Output( 0, "CDELT2  = %#20.14g [+/- %#13.7g]", FPAR[4], EPAR[4] );
         cdelt2 = FPAR[4];
         Output( 0, "CRPIX1  = %#20.14g [+/- %#13.7g]", FPAR[5], EPAR[5] );
         crpix1 = FPAR[5] - 0.5;
         Output( 0, "CRPIX2  = %#20.14g [+/- %#13.7g]", FPAR[6], EPAR[6] );
         crpix2 = FPAR[6] - 0.5;
         for ( count = 0.0, j = 0; j < naxis2; j += 10 ) {
            for ( i = 0; i < naxis1; i += 10 ) {
               double	x, y, a, d, xx, yy;

               count = count + 1;
               x = r_x - 1 + i; y = r_y - 1 + j;
               amdpos( x, y, &a, &d );
               if (!proco_c( &a, &d, &xx, &yy, &FPAR[0], &FPAR[1], &FPAR[3],
                  &FPAR[4], &FPAR[2], &PROJ, &MODE ) ) {
                  double	dx, dy, d;
                  int		n;

                  xx += FPAR[5]; yy += FPAR[6];
                  x -= ( r_x - 1 ); y -= ( r_y - 1 );
                  dx = ( xx - i ) * 3600.0 * FPAR[3];
                  dy = ( yy - j ) * 3600.0 * FPAR[4];
                  d = sqrt( dx * dx + dy * dy );
                  n = ( 10.0 * d );
                  if ( n >= MAX_DIFF ) n = MAX_DIFF - 1;
                  dcount[n] += 1;
               }
            }
         }
         for ( ccount = 0, n = 0; n < (MAX_DIFF-1) && ccount < count ; n++ ) {
            ccount += dcount[n];
            sp = ( (double) ccount / (double) count * 100.0 );
            Output( 0, "Diffs <  %3.1f\" : %7.3f%%", 0.1*((double)(n+1)), sp );
         }
         Output( 0, "Diffs >= %3.1f\" : %7.3f%%", 0.1*((double)(n)), 100.0 - sp );
      }
      return( i );
   }
}

static	void	statusb( fint left )
{
   float	Min = 0.0;
   float	Max = naxis1 * naxis2;
   float	Cur = naxis1 * naxis2 - left;

   stabar_c( &Min, &Max, &Cur );
}

static	int	create_set( FILE *poss, fchar set )
{
   double	epoch = 2000.0;
   fint		clow, cupp;
   fint		gerror = 0;
   fint		nleft = naxis1 * naxis2;
   fint		subset = 0;
   fint		tid = 0;
   fint		n;
   fint		imin, imax, nblank, count = 0;
   float	amin, amax;

   gds_create_c( set, &gerror );
   gdsd_wchar_c( set, tofchar( "INSTRUME" ), &subset, tofchar( "DSS" ), &gerror );
   gdsd_wdble_c( set, tofchar( "EPOCH" ), &subset, &epoch, &gerror );
   gds_extend_c( set, tofchar( "RA---ARC-DSS" ), &crpix1, &naxis1, &gerror );
   gds_extend_c( set, tofchar( "DEC--ARC-DSS" ), &crpix2, &naxis2, &gerror );
   gdsd_wchar_c( set, tofchar( "CUNIT1" ), &subset, tofchar( "DEGREE" ), &gerror );
   gdsd_wchar_c( set, tofchar( "CUNIT2" ), &subset, tofchar( "DEGREE" ), &gerror );
   gdsd_wdble_c( set, tofchar( "CDELT1" ), &subset, &cdelt1, &gerror );
   gdsd_wdble_c( set, tofchar( "CDELT2" ), &subset, &cdelt2, &gerror );
   gdsd_wdble_c( set, tofchar( "CROTA2" ), &subset, &crota2, &gerror );
   gdsd_wdble_c( set, tofchar( "CRVAL1" ), &subset, &crval1, &gerror );
   gdsd_wdble_c( set, tofchar( "CRVAL2" ), &subset, &crval2, &gerror );
   gdsc_range_c( set, &subset, &clow, &cupp, &gerror );
   statusb( nleft );
   do {
      char  	idata[MAX_DATA];
      fint	nd, nr, nw;
      float	odata[MAX_DATA];

      nr = fread( idata, sizeof( char ), MAX_DATA, poss );
      if ( !nr ) {
         Error( FATAL, "Error reading from file!" );
      }
      switch( bitpix ) {
         case 8: {
            fint	n, m;

            nw = nr;
            if ( nw > nleft ) nw = nleft;
            for ( m = n = 0; n < nw; n++ ) {
               sbyte	sb;

               sb = idata[m++];
               odata[n] = (float) sb * bscale + b_zero;
            }
            nleft -= nw;
            break;
         }
         case 16: {
            fint	n, m;

            nw = nr / 2;
            if ( nw > nleft ) nw = nleft;
            for ( m = n = 0; n < nw; n++ ) {
               sbyte	sb;
               ubyte	ub1;

               sb = idata[m++];
               ub1 = idata[m++];
               odata[n] = ( (float) sb * 256.0 + (float) ub1 ) * bscale + b_zero;
            }
            nleft -= nw;
            break;
         }
         case 32: {
            fint	n, m;

            nw = nr / 4;
            if ( nw > nleft ) nw = nleft;
            for ( m = n = 0; n < nw; n++ ) {
               sbyte	sb;
               ubyte	ub1, ub2, ub3;

               sb = idata[m++];
               ub1 = idata[m++];
               ub2 = idata[m++];
               ub3 = idata[m++];
               odata[n] = ( ( ( (float) sb * 256.0 + (float) ub1 ) * 256.0 + (float) ub2 ) * 256.0 + (float) ub3 ) * bscale + b_zero;
            }
            nleft -= nw;
            break;
         }
         default: {
            Error( FATAL, "Don't understand BITBIX = %d", bitpix );
            break;
         }
      }
      gdsi_write_c( set, &clow, &cupp, odata, &nw, &nd, &tid );
      minmax4_c( odata, &nw, &amin, &amax, &imin, &imax, &nblank, &count );
      if ( tid < 0 ) {
         Error( FATAL, "Error writing to set!" );
      }
      statusb( nleft );
   } while ( nleft );
   gdsd_wreal_c( set, tofchar( "DATAMIN" ), &subset, &amin, &gerror );
   gdsd_wreal_c( set, tofchar( "DATAMAX" ), &subset, &amax, &gerror );
   gdsd_wint_c( set, tofchar( "NBLANK" ), &subset, &nblank, &gerror );
   gdsd_wint_c( set, tofchar( "MINPOS" ), &subset, &imin, &gerror );
   gdsd_wint_c( set, tofchar( "MAXPOS" ), &subset, &imax, &gerror );
   for ( n = 0; n < sizeof( Keys ) / sizeof( char * ); n++ ) {
       if ( ftsd_find_c( Head, tofchar( Keys[n] ), Record ) >= 0 ) {
         gdsd_wfits_c( set, tofchar( Keys[n] ), &subset, Record, &gerror );
      }
   }
   gds_close_c( set, &gerror );
   return( 0 );
}

static	int	save_set( FILE *poss )
{
   char		setname[FILENAME_MAX-5];
   fchar	set;

   set.a = setname; set.l = sizeof( setname );
   do {
      fint	gerror = 0;
      int	n;

      strcpy( setname, Object );
      for ( n = strlen( Object ); n < set.l; setname[n++] = ' ');
      n = userfchar( set, 1, 1, "OUTSET=", "Name of output set [%s]", Object );
      if ( !tobool( gds_exist_c( set, &gerror ) ) ) {
         create_set( poss, set );
         break;
      }
      cancel( "OUTSET=" );
      Error( WARNING, "Set %.*s already exists!", (int) nelc_c( set ), set.a );
   } while ( 1 );
   return( 0 );
}

/*
 * Main:
 */

MAIN_PROGRAM_ENTRY
{
   FILE		*poss;
   char		filename[FILENAME_MAX+1];
   char		*p_head = NULL;
   int		np_head = 0;

   Record.a = record; Record.l = sizeof( record );
   init_c( );				/* contact Hermes */
   poss = NULL;				/* reset */
   do {					/* get name of FITS file */
      fchar	Filename;
      fint	nchr;

      Filename.a = filename; Filename.l = sizeof( filename ) - 1;
      nchr = userftext( Filename, 0, "FILENAME=",
         "Name of FITS file containing DSS image" );
      filename[nchr] = 0;
      poss = fopen( filename, "rb" );
      if ( poss == NULL ) {
         Error( WARNING, "Cannot open %s!", filename );
         cancel( "FILENAME=" );
      }
   } while ( poss == NULL );
   do {					/* load FITS header into memory */
      int	nr;

      p_head = realloc( p_head, sizeof( char ) * ( np_head + FITS_BLOCKLEN ) );
      if ( p_head == NULL ) {
         Error( FATAL, "Unable to create enough memory (%d bytes)!",
            np_head + FITS_BLOCKLEN );
      }
      nr = fread( &p_head[np_head], sizeof( char ), FITS_BLOCKLEN, poss );
      if ( nr % FITS_RECLEN ) {
         Error( FATAL, "Error (%d) reading from %s!", nr, filename );
      }
      if ( nr != FITS_BLOCKLEN ) HeadOnly = 1;
      np_head += nr;
      Head.a = p_head; Head.l = np_head;
   } while ( ftsd_find_c( Head, tofchar( "END" ), Record ) == -1 );

   if ( setup_coords( Head, Record ) > 0 && !HeadOnly ) {
      save_set( poss );
   }
   fclose( poss );
   finis_c( );
   return( EXIT_SUCCESS );
}
