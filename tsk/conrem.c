/* conrem.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            conrem.dc1

Program:      CONREM

Purpose:      Removes continuum from channel maps by fitting a
              polynomial to the continuum channels.

Category:     ANALYSIS, COMBINATION

File:         conrem.c

Author:       K.G. Begeman

Keywords:

   INSET=     Set and subsets from which to subtract the continuum.
              Maximum number of subsets is 2048.

   FITSET=    Set and subsets for which to fit the continuum.
              Maximum number of subsets is 2048.

   NPOLY=     Degree of polynomial to be fitted [1].
              Maximum degree is 5.

   OUTSET=    Set and subsets for the continuum subtracted data
              [no continuum will be subtracted].

   CONPOS=    Coordinate for which to calculate continuum [No continuum
              is calculated]. If the interpolated continuum is wanted,
              enter here the coordinate for which it should be interpolated.

   CONSET=    Set and subset for the continuum. The interpolated continuum
              is calculated for the coordinate entered at CONPOS=.

Description:  A polynomial of degree NPOLY is fitted to the subsets
              selected with FITSET for each grid position in the subset
              separately. This fitted polynomial can then be subtracted
              from the subsets selected with INSET and/or can be used
              to calculate an interpolated continuum in CONSET at user
              selected coordinate CONPOS. The program is especially
              useful for removing the continuum from a series of line
              maps.

Example:      <USER> CONREM
              CONREM  Version 1.0  (Jul 30 1991)
              <USER> CONREM INSET=NGC4214 3:58
              Set NGC4214 has 3 axes
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              FREQ-OHEL          from     1 to    63
              <USER> CONREM FITSET=NGC4214 3:16 42:58
              Set NGC4214 has 3 axes
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              FREQ-OHEL          from     1 to    63
              <USER> CONREM NPOLY=1
              <USER> CONREM OUTSET=NGC4214_SUB
              Set NGC4214_SUB has 3 axes
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              FREQ-OHEL          from     3 to    58
              <USER> CONREM CONPOS=300 km/s
              Continuum calculated at 300.000000 KM/S
              <USER> CONREM CONSET=NGC4214_CON
              Set NGC4214_CON has 3 axes
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              FREQ-OHEL          from    32 to    32
              <STATUS>  CONREM   +++ FINISHED +++

Updates:      May 13, 1990: KGB Document created.
              Dec 11, 1991: KGB Cancel replace by reject.

#<

*/

/*
 * Includes:
 */

#include	"math.h"			/* <math.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"gipsyc.h"			/* GIPSY symbols */
#include	"cmain.h"			/* main c programme */
#include	"anyout.h"			/* defines anyout_c */
#include	"axunit.h"			/* defines axunit_c */
#include	"cotrans.h"			/* defines contrans_c */
#include	"error.h"			/* defines error_c */
#include	"finis.h"			/* defines finis_c */
#include	"gdsasn.h"			/* defines gdsasn_c */
#include	"gdsc_grid.h"			/* defines gdsc_grid_c */
#include	"gdsc_ndims.h"			/* defines gdsc_ndims_c */
#include	"gdsc_range.h"			/* defines gdsc_range_c */
#include	"gdsc_word.h"			/* defines gdsc_word_c */
#include	"gdscpa.h"			/* defines gdscpa_c */
#include	"gdsi_read.h"			/* defines gdsi_read_c */
#include	"gdsi_write.h"			/* defines gdsi_write_c */
#include	"gdsinp.h"			/* defines gdsinp_c */
#include	"gdsout.h"			/* defines gdsout_c */
#include	"gdspos.h"			/* defines gdspos_c */
#include	"init.h"			/* defines init_c */
#if	0
#include	"invmatd.h"			/* defines invmatd_c */
#endif
#include	"minmax3.h"			/* defines minmax3_c */
#include	"presetr.h"			/* defines presetr_c */
#include	"qcnvl2.h"			/* defines qcnvl2_c */
#include	"reject.h"			/* defines reject_c */
#include	"setfblank.h"			/* defines setfblank_c */
#include	"stabar.h"			/* defines stabar_c */
#include	"userint.h"			/* defines userint_c */
#include	"wminmax.h"			/* defines wminmax_c */


/*
 * Defines:
 */

#define	KEY_CONPOS	tofchar("CONPOS=")	/* keyword CONPOS= */
#define	KEY_CONSET	tofchar("CONSET=")	/* keyword CONSET= */
#define	KEY_FITSET	tofchar("FITSET=")	/* keyword FITSET= */
#define	KEY_INSET	tofchar("INSET=")	/* keyword INSET= */
#define	KEY_NPOLY	tofchar("NPOLY=")	/* keyword NPOLY= */
#define	KEY_OUTSET	tofchar("OUTSET=")	/* keyword OUTSET= */
#define	MAXAXES		10			/* max. number of axes */
#define	MAXDATA		4096			/* size of data buffers */
#define	MAXFSUB		2048			/* max. subsets for fit */
#define	MAXISUB		2048			/* max. subsets for subtract */
#define	MAXMESLEN	80			/* max. length of messages */
#define	MAXOSUB		MAXISUB			/* max. output subsets */
#define	MAXPOLY		5			/* max. degree of polynomial */
#define	MAXSETNAMLEN	80			/* max. length of set name */
#define	MES_CONPOS	tofchar("Continuum coordinate [no continuum]")
#define	MES_CONSET	tofchar("Set for interpolated continuum")
#define	MES_FITSET	tofchar("Set and subsets to fit polynomial to")
#define	MES_INSET	tofchar("Set and subset(s) from which to subtract continuum")
#define	MES_NPOLY	tofchar("Degree of polynomial [1]")
#define	MES_OUTSET	tofchar("Set for continuum subtracted data [no subtraction]")
#define	VERSION		"1.0"			/* change version number here */


/*
 * Variables for input set:
 */

static	char	isetb[MAXSETNAMLEN];		/* buffer for input set name */
static	fchar	iset = { isetb, MAXSETNAMLEN };	/* points to buffer above */
static	fint	iaxperm[MAXAXES];		/* axes permutation */
static	fint	iaxsize[MAXAXES];		/* axes sizes */
static	fint	icwlo[MAXISUB];			/* lower coordinate words */
static	fint	icwup[MAXISUB];			/* upper coordinate words */
static	fint	insub;				/* number of input subsets */
static	fint	isetdim;			/* dimension of input set */
static	fint	isubdim;			/* dimension of input subsets */
static	fint	isubset[MAXISUB];		/* input subset coord. words */
static	fint	itid[MAXISUB];			/* transfer identifiers */
static	float	idata[MAXDATA];			/* data buffer */
static	float	ifac[MAXPOLY+1][MAXISUB];	/* x factors */

/*
 * Variables for input fitset:
 */

static	char	fsetb[MAXSETNAMLEN];		/* buffer for fit set name */
static	fchar	fset = { fsetb, MAXSETNAMLEN };	/* points to buffer above */
static	fint	faxperm[MAXAXES];		/* axes permutation */
static	fint	faxsize[MAXAXES];		/* axes sizes */
static	fint	fcwlo[MAXFSUB];			/* lower coordinate words */
static	fint	fcwup[MAXFSUB];			/* upper coordinate words */
static	fint	fgrlo[MAXAXES];			/* lower grids */
static	fint	fgrup[MAXAXES];			/* upper grids */
static	fint	fnsub;				/* number of fit subsets */
static	fint	fsetdim;			/* dimension of fit set */
static	fint	fsubdim;			/* dimension of fit subsets */
static	fint	fsubset[MAXFSUB];		/* fit subset coord. words */
static	fint	ftid[MAXFSUB];			/* transfer identifiers */
static	float	fdata[MAXDATA];			/* data buffer */
static	float	ffac[2*MAXPOLY+1][MAXFSUB];	/* x factors */


/*
 * Variables for output set:
 */

static	char	osetb[MAXSETNAMLEN];		/* buffer for output set name */
static	fchar	oset = { osetb, MAXSETNAMLEN };	/* points to buffer above */
static	fint	oaxperm[MAXAXES];		/* axes permutation */
static	fint	oaxsize[MAXAXES];		/* axes sizes */
static	fint	ocount[MAXOSUB];		/* counter for minmax3 */
static	fint	ocwlo[MAXOSUB];			/* lower coordinate words */
static	fint	ocwup[MAXOSUB];			/* upper coordinate words */
static	fint	onblank[MAXOSUB];		/* running number of blanks */
static	fint	onsub;				/* number of output subsets */
static	fint	osubset[MAXOSUB];		/* output subset coord. words */
static	fint	otid[MAXOSUB];			/* transfer identifiers */
static	float	odatamax[MAXOSUB];		/* running maximum */
static	float	odatamin[MAXOSUB];		/* running minimum */

/*
 * Variables for the interpolated continuum set:
 */

static	char	csetb[MAXSETNAMLEN];		/* buffer for continuum set name */
static	fchar	cset = { csetb, MAXSETNAMLEN };	/* points to buffer above */
static	fint	caxperm[MAXAXES];		/* axes permutation */
static	fint	caxsize[MAXAXES];		/* axes sizes */
static	fint	ccount;				/* counter for minmax3 */
static	fint	ccwlo;				/* lower cooridnate word */
static	fint	ccwup;				/* upper coordinate word */
static	fint	cnblank;			/* number of blanks */
static	fint	cnsub;				/* number of subsets */
static	fint	csubset;			/* the subset */
static	fint	ctid;				/* the transfer id */
static	float	cdata[MAXDATA];			/* continuum data buffer */
static	float	cdatamax;			/* running maximum */ 
static	float	cdatamin;			/* running minimum */
static	float	cfac[MAXPOLY+1];		/* x factors */

/*
 * Variables for the polynomial fitting:
 */

static	double	mat1[(MAXPOLY+1)*(MAXPOLY+1)];	/* matrix for full poly. */
static	double	mat2[(MAXPOLY+1)*(MAXPOLY+1)];	/* matrix for smaller poly. */
static	fint	npoly;				/* degree of polynomial */
static	float	poly[MAXPOLY+1][MAXDATA];	/* polynomial coefficients */
static	float	sumx[2*MAXPOLY+1][MAXDATA];	/* sum of x factors */
static	float	sumy[MAXPOLY+1][MAXDATA];	/* sum of y factors */

/*
 * Other variables:
 */

static	char	msg[MAXMESLEN];			/* buffer for message */
static	fint	gerror = 0;			/* GDS errors */
static	fint	ndone = 0;			/* number of points done */
static	fint	ntotal;				/* total number of points */
static	float	blank;				/* the system defined blank */
static	float	stat[3];			/* for stabar */


static fint invmat( double *mat, fint ndim )
/*
 * invmat calculates the inverse of matrix. The algorithm used is the
 * Gauss-Jordan algorithm described in Stoer, Numerische matematik, 1 Teil.
 */
{
   double	even;
   double	hv[MAXPOLY];
   double	*matrix[MAXPOLY];
   double	mjk;
   double	rowmax;
   fint		evin;
   fint		i;
   fint		j;
   fint		k;
   fint		per[MAXPOLY];
   fint		row;


   for (k = i = 0; i < ndim; i++) {		/* initialize */
      per[i] = i;				/* set permutation array */
      matrix[i] = &mat[k];			/* assign pointer */
      k += ndim;				/* go to next position in mat */
   }
   for (j = 0; j < ndim; j++) {			/* in j-th column, ... */
      rowmax = fabs( matrix[j][j] );		/* determine row with ... */
      row = j;					/* largest element. */
      for (i = j + 1; i < ndim; i++) {
         if (fabs( matrix[i][j] ) > rowmax) {
            rowmax = fabs( matrix[i][j] );
            row = i;
         }
      }
      if (matrix[row][j] == 0.0) return( -6 );	/* determinant is zero! */
      if (row > j) {				/* if largest element not ... */
         for (k = 0; k < ndim; k++) {		/* on diagonal, then ... */
            even = matrix[j][k];		/* permutate rows. */
            matrix[j][k] = matrix[row][k];
            matrix[row][k] = even;
         }
         evin = per[j];				/* keep track of permutation */
         per[j] = per[row];
         per[row] = evin;
      }
      even = 1.0 / matrix[j][j];		/* modify column */
      for (i = 0; i < ndim; i++) matrix[i][j] *= even;
      matrix[j][j] = even;
      for (k = 0; k < j; k++) {
         mjk = matrix[j][k];
         for (i = 0; i < j; i++) matrix[i][k] -= matrix[i][j] * mjk;
         for (i = j + 1; i < ndim; i++) matrix[i][k] -= matrix[i][j] * mjk;
         matrix[j][k] = -even * mjk;
      }
      for (k = j + 1; k < ndim; k++) {
         mjk = matrix[j][k];
         for (i = 0; i < j; i++) matrix[i][k] -= matrix[i][j] * mjk;
         for (i = j + 1; i < ndim; i++) matrix[i][k] -= matrix[i][j] * mjk;
         matrix[j][k] = -even * mjk;
      }
   }
   for (i = 0; i < ndim; i++) {		/* finally, repermute the ... */
      for (k = 0; k < ndim; k++) {		/* columns. */
         hv[per[k]] = matrix[i][k];
      }
      for (k = 0; k < ndim; k++) {
         matrix[i][k] = hv[k];
      }
   }
   return( 0 );					/* all is well */
}



MAIN_PROGRAM_ENTRY				/* main programme starts here */
{
   fchar	key, mes;			/* character strings */

   init_c( );					/* open channel with user */
   setfblank_c( &blank );			/* obtain BLANK value */
   IDENTIFICATION( "CONREM", VERSION );		/* identify ! */
   /*
    * Here we get the set and subset(s) from which to subtract the
    * continuum. This program is essential a class two program,
    * but we may want to allow to subtract from one subset only,
    * so we have to solve this problem ourselves by looping and checking
    * whether the subset dimension is one less than the set dimension.
    */
   {
      fint	class = 1;			/* class for gdsinp */
      fint	input_level = 0;		/* no default */
      fint	level = 0;			/* at set level */
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	nitems = MAXISUB;		/* maximum number of items */
      fint	okay;				/* loop control */
      fint	output_level = 11;		/* output level */

      do {					/* loop until input o.k. */
         okay = 1;				/* reset */
         isubdim = 0;				/* the required subset dimension */
         key = KEY_INSET; mes = MES_INSET;
         insub = gdsinp_c( iset ,		/* input set name */
                           isubset ,		/* input subsets */
                           &nitems ,		/* max. # of input subsets */
                           &input_level ,	/* input level */
                           key ,		/* keyword */
                           mes ,		/* message */
                           &output_level ,	/* output level */
                           iaxperm ,		/* axes permutation */
                           iaxsize ,		/* size of axes */
                           &maxaxes ,		/* max. dimension */
                           &class ,		/* class of subsets */
                           &isubdim );		/* subset dimension */
         isetdim = gdsc_ndims_c( iset, &level );/* Dimension of iset */
         if (isetdim != (isubdim + 1)) {
            sprintf( msg, "Wrong subset dimension, must be %d!", isetdim - 1 );
            reject_c( key, tofchar( msg ) );	/* reject keyword */
            okay = 0;
         }
      } while (!okay);				/* end of loop */
   }
   /*
    * Here we get the coordinates (grids) along the operation axis.
    * Also the coordinate words and the transfer identifiers are set.
    */
   {
      fint	isub;				/* subset counter */
      fint	np;				/* loop counter */

      for (isub = 0; isub < insub; isub++) {	/* loop */
         float	icoord;				/* x coordinate */

         itid[isub] = 0;			/* Reset transfer identifiers */
         gdsc_range_c( iset ,			/* name of set */
                       &isubset[isub] ,		/* the subset */
                       &icwlo[isub] ,		/* lower cw */
                       &icwup[isub] ,		/* upper cw */
                       &gerror );		/* error status */
         icoord = gdsc_grid_c( iset ,		/* name of set */
                               &iaxperm[isetdim-1] ,
                               &isubset[isub] ,
                               &gerror );
         ifac[0][isub] = 1.0;			/* Set X factor */
         for (np = 1; np < (MAXPOLY + 1); np++) {
            ifac[np][isub] = ifac[np-1][isub] * icoord;
         }
      }
   }
   /*
    * Now we want the set and subsets to which we are going to fit the
    * continuum (polynomials). Here we use the CLASS = 2 facility of
    * GDSINP. We still have to loop, since we need to check the sizes
    * and dimensions of the subsets and the coordinate type along the
    * operation axis.
    */
   {
      fint	class = 2;			/* class off gdsinp */
      fint	classdim = 1;			/* wanted dimension */
      fint	input_level = 0;		/* no default */
      fint	level = 0;			/* set top level */
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	maxfsubs = MAXFSUB;		/* maximum number of subsets */
      fint	n;				/* loop counter */
      fint	okay;				/* loop control */
      fint	output_level = 11;		/* output level */

      do {					/* loop until input o.k. */
         okay = 1;				/* reset */
         key = KEY_FITSET; mes = MES_FITSET;
         fnsub = gdsinp_c( fset ,		/* name of set */
                           fsubset ,		/* subsets */
                           &maxfsubs ,		/* max # of subsets */
                           &input_level ,	/* input level */
                           key ,		/* keyword */
                           mes ,		/* message */
                           &output_level ,	/* output level */
                           faxperm ,		/* permutation array */
                           faxsize ,		/* size of axes */
                           &maxaxes ,		/* max dimension */
                           &class ,		/* class of subsets */
                           &classdim );		/* dimension of subsets */
         fsetdim = gdsc_ndims_c( fset, &level );
         fsubdim = fsetdim - 1;
         if (fsetdim != isetdim) {
            sprintf( msg, "Wrong dimension of set, must be %d!", isetdim );
            okay = 0;
         } else {
            for (n = 0, ntotal = 1; n < isubdim && okay; n++) {
               if (iaxsize[n] != faxsize[n]) {
                  sprintf( msg, "Wrong size of FITSET!" );
                  okay = 0;
               } else {
                  ntotal *= faxsize[n];
               }
            }
         }
         if (!okay) reject_c( KEY_FITSET, tofchar( msg ) );	/* reject */
      } while (!okay);
      {
         fint	cwlo, cwup, gerror = 0.0, zero = 0.0;

         gdsc_range_c( fset, &zero, &cwlo, &cwup, &gerror );
         for ( n = 0; n < fsubdim; n++ ) {
            fgrlo[n] = gdsc_grid_c( fset, &faxperm[n], &cwlo, &gerror );
            fgrup[n] = gdsc_grid_c( fset, &faxperm[n], &cwup, &gerror );
         }
      }
   }
   /*
    * Now we need the degree of the polynomial. The minimum degree is zero,
    * the maximum depends on MAXPOLY and the number of subsets in the FIT set.
    */
   {
      fint	input_level = 1;		/* default allowed */
      fint	nitems = 1;			/* only one item */

      do {					/* loop until input o.k. */
         key = KEY_NPOLY; mes = MES_NPOLY;
         if (!userint_c( &npoly ,		/* the degree */
                         &nitems ,		/* number of degrees (one) */
                         &input_level ,		/* input level */
                         key ,			/* keyword */
                         mes) ) {		/* message */
            npoly = 1;				/* default */
         } else if (npoly < 0 || npoly > MAXPOLY || npoly > fnsub) {
            sprintf( msg, "Wrong degree of polynomial!" );
            reject_c( KEY_NPOLY, tofchar( msg ) );	/* reject */
            npoly = -1;
         }
      } while (npoly < 0);			/* end of loop */
   }
   /*
    * In the next part we determine the X^n factors for each subset.
    * For X we use the coordinate (grid) attached with the operation
    * axis.
    */
   {
      fint	fsub;				/*  subset counter */

      for (fsub = 0; fsub < fnsub; fsub++) {	/* loop to get factors */
         fint	np;				/* loop counter */
         float	fcoord;				/* coordinate */

         ftid[fsub] = 0;			/* reset transfer identifiers */
         gdsc_range_c( fset ,			/* name of set */
                       &fsubset[fsub] ,		/* the subset */
                       &fcwlo[fsub] ,		/* lower cw */
                       &fcwup[fsub] ,		/* upper cw */
                       &gerror );		/* error status */
         fcoord = gdsc_grid_c( fset ,		/* name of set */
                               &faxperm[fsetdim-1] ,
                               &fsubset[fsub] ,
                               &gerror );
         ffac[0][fsub] = 1.0;			/* Set X factor */
         for (np = 1; np < (2*npoly+1); np++) {	/* calculate the rest */
            ffac[np][fsub] = ffac[np-1][fsub] * fcoord;
         }
      }
   }
   /*
    * The following part gets the set and subsets where we should store
    * the continuum subtracted subsets.
    */
   {
      fint	class = 1;			/* class one output set */
      fint	input_level = 5;		/* exact number wanted */
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	n;				/* loop counter */
      fint	osub;				/* subset counter */
      fint	output_level = 11;		/* output level */

      for (n = 0; n < oset.l; oset.a[n++] = ' ');
      gdsasn_c( KEY_INSET, KEY_OUTSET, &class );/* assign buffer */
      key = KEY_OUTSET; mes = MES_OUTSET;
      onsub = gdsout_c( oset ,			/* name of output set */
                        osubset ,		/* output subsets */
                        &insub ,		/* exact # of subsets */
                        &input_level ,		/* exact number wanted */
                        key ,			/* keyword */
                        mes ,			/* message */
                        &output_level ,		/* output level */
                        oaxperm ,		/* axes permutation */
                        oaxsize ,		/* size of axes */
                        &maxaxes );		/* max. dimension */
      for (osub = 0; osub < onsub; osub++) {	/* loop */
         ocount[osub] = 0;			/* reset minmax3 counter */
         otid[osub] = 0;			/* reset transfer identifiers */
         gdsc_range_c( oset ,			/* name of output set */
                       &osubset[osub] ,		/* output subset level */
                       &ocwlo[osub] ,		/* lower cw */
                       &ocwup[osub] ,		/* upper cw */
                       &gerror );		/* error status */
      }
   }
   /*
    * Here we allow the user to specify a set where he can store
    * the fitted polynomial coefficients (although at the moment we do
    * not have a way to deal with these coefficients).
    */
   {
   }
   /*
    * Now we allow the user to enter a coordinate for which we will
    * interpolate the continuum. We use gdspos for this.
    */
   {
      double	fcoord;				/* the coordinate */
      fchar	NULLC;				/* the null character */
      fint	input_level = 1;		/* default allowed */
      fint	level = 0;			/* all levels cleared */
      fint	n;				/* loop counter */
      fint	one = 1;			/* just 1 */
      fint	zero;				/* centre grid */

      for (n = 0; n < fsubdim; n++) {		/* loop to fill in levels */
         zero = ( fgrup[n] + fgrlo[n] ) / 2;	/* centre of range */
         level = gdsc_word_c( fset ,		/* name of set */
                              &faxperm[n] ,	/* axis sequence number */
                              &zero ,		/* the grid */
                              &level ,		/* old level */
                              &gerror );	/* error return */
      }
      key = KEY_CONPOS; mes = MES_CONPOS;
      cnsub = gdspos_c( &fcoord ,		/* the grid value */
                        &one ,			/* only one grid */
                        &input_level ,		/* the default level */
                        key ,			/* the keyword */
                        mes ,			/* the message */
                        fset ,			/* the set name */
                        &level );		/* the subset level */
      if (cnsub) {				/* user wants continuum */
         double	crpix;				/* new reference value */
         fint	class = 1;			/* class 1 output set */
         fint	input_level = 0;		/* no default allowed */
         fint	maxaxes = MAXAXES;		/* maximum number of axes */
         fint	np;				/* loop counter */
         fint	output_level = 11;		/* output level */
         fint	pmask = 8;			/* change mask */

         NULLC.a = NULL; NULLC.l = 0;		/* empty character */
         cfac[0] = 1.0;				/* set x factor */
         for (np = 1; np < (npoly+1); np++) {	/* calculate rest */
            cfac[n] = cfac[np-1] * fcoord;	/* calculate the x factors */
         }
         crpix = 1.0 - fcoord;			/* new reference value */
         {
            char	cunitb[18];		/* buffer for ax unit */
            double	coords[MAXAXES];	/* for coordinates */
            fchar	cunit;			/* pointer to cunitb */
            fint	dir = 1;		/* grids -> phyical coords */
            fint	output_level = 0;	/* output level */

            cunit.a = cunitb; cunit.l = sizeof( cunitb );
            cotrans_c( fset, &level, &fcoord, coords, &dir );
            axunit_c( fset, &faxperm[fsubdim], cunit );	/* get units */
            sprintf( msg, "Continuum calculated at %f %.*s", coords[fsubdim], (int) cunit.l, cunit.a );
            anyout_c( &output_level, tofchar( msg ) );
         }
         /*
          * If the interpolated continuum is wanted, we need to know where to
          * store it. The next part takes care of that.
          */
         gdsasn_c( KEY_INSET ,			/* input key */
                   KEY_CONSET ,			/* output key */
                   &class );			/* the class */
         gdscpa_c( KEY_CONSET , 		/* output key */
                   &isetdim ,			/* last axis */
                   &one ,			/* size of axis */
                   NULL ,			/* CDELT doesn't change */
                   NULL ,			/* CROTA doesn't change */
                   &crpix ,			/* CRPIX does change */
                   NULL ,			/* CRVAL doesn't change */
                   NULLC ,			/* CTYPE doesn't change */
                   NULLC ,			/* CUNIT doesn't change */
                   &pmask );			/* change mask */
         key = KEY_CONSET; mes = MES_CONSET;
         gdsout_c( cset ,			/* the continuum set name */
                   &csubset ,			/* the subset level */
                   &one ,			/* only one */
                   &input_level ,		/* the default */
                   key ,			/* the keyword */
                   mes ,			/* the message */
                   &output_level ,		/* the output level */
                   caxperm ,			/* the axes permutation */
                   caxsize ,			/* the axes sizes */
                   &maxaxes );			/* the max # of axes */
         ccount = 0;				/* reset minmax3 counter */
         ctid = 0;				/* reset transfer identifier */
         gdsc_range_c( cset ,			/* name of continuum set */
                       &csubset ,		/* output subset level */
                       &ccwlo ,			/* lower cw */
                       &ccwup ,			/* upper cw */
                       &gerror );		/* error status */
      }
   }
   /*
    * Next we have the big repeat loop over the subset box. In the
    * first part we determined the SUMs, the second part calculates
    * the coefficients and the third part subtracts the baseline from
    * the input subsets.
    * But first, we prepare the buffers for stabar.
    */
   stat[0] = 0.0; stat[1] = ntotal; stat[2] = ndone;
   stabar_c( &stat[0], &stat[1], &stat[2] );	/* give user the status */
   do {
      fint	fsub;				/* fit subset counter */
      fint	np;				/* loop counter */
      fint	nread = MAXDATA;		/* number to read */
      float	zero = 0.0;			/* just zero */

      /*
       * In the next part the polynomials are fitted. This is done by
       * calculating SUM Xm^n*Ym, where Ym is the data value in subset
       * m. If a data value is BLANK, it is not used in the fit.
       */
      for (np = 0; np < (2*npoly+1); np++) {	/* reset loop */
         presetr_c( &zero, sumx[np], &nread );	/* Reset the X sums */
      }
      for (np = 0; np < (npoly+1); np++) {	/* reset loop */
         presetr_c( &zero, sumy[np], &nread );	/* Reset the Y sums */
         presetr_c( &zero, poly[np], &nread );	/* reset the coefficients */
      }
      for (fsub = 0; fsub < fnsub; fsub++) {	/* Loop over continuum subsets */
         fint	nr;				/* loop counters */

         gdsi_read_c( fset ,			/* the set */
                      &fcwlo[fsub] ,		/* lower cw */
                      &fcwup[fsub] ,		/* upper cw */
                      fdata ,			/* the data */
                      &nread ,			/* the number to read */
                      &nread ,			/* the number read */
                      &ftid[fsub] );		/* transfer id */
         for (nr = 0; nr < nread; nr++) {	/* Loop to calculate sums */
            float	val;			/* datum */

            val = fdata[nr];			/* get datum */
            if (val != blank) {			/* Sum if not blank */
               for (np = 0; np < (2*npoly+1); np++) {
                  sumx[np][nr] += ffac[np][fsub];/* Add to X sums */
               }
               for (np = 0; np < (npoly+1); np++) {
                  sumy[np][nr] += val * ffac[np][fsub];	/* Add to Y sums */
               }
            }
         }
      }
      /*
       * Next we calculate the coefficients. If there are not enough
       * independent data values, the degree of the polynomial is
       * decreased so that a fit is still possible.
       */
      {
         fint		nr;			/* loop counter */
         static	fint	inimat = 0;		/* matrix initialized ? */

         for (nr = 0; nr < nread; nr++) {
            fint	ndim = npoly + 1;	/* dimension of matrix */
            fint	npsub = (fint) (sumx[0][nr] + 0.5);
            double	*mat = mat1;		/* points to matrix */

            if (npsub != fnsub || !inimat) {	/* build the matrix */
               fint	i, j, m;		/* loop counters */

               if (npsub < (npoly+1)) {		/* decrease matrix dimension */
                  ndim = npsub;			/* new dimension */
               } else {				/* dimension okay */
                  ndim = npoly + 1;		/* max. dimension */
               }
               if (npsub == fnsub) {		/* now points left out */
                  mat = mat1;			/* full matrix */
                  inimat = 1;			/* full matrix initialized */
               } else {				/* points skipped */
                  mat = mat2;			/* partial matrix */
               }
               for (m = i = 0; i < ndim; i++) {	/* loop to fill matrix */
                  for (j = 0; j < ndim; j++) {	/* so that matrix */
                     mat[m++] = sumx[i+j][nr];	/* is symmetric */
                  }
               }
#if	0
               invmatd_c( mat, &ndim, &ndim );	/* do the inversion */
#endif
               invmat( mat, ndim );		/* do the inversion */
            }
            if (ndim) {				/* find coefficients */
               fint	i, j, m;		/* loop counters */

               for (m = i = 0; i < ndim; i++) {	/* calculate the vector */
                  for (j = 0; j < ndim; j++) {	/* i.e. the coefficients */
                     poly[i][nr] += ( mat[m++] * sumy[j][nr] );
                  }
               }
            } else {
               poly[0][nr] = blank;		/* set to blank */
            }
         }
      }
      /*
       * Now we subtract the fitted baselines from the input data.
       * Only if the user wants it, of course.
       */
      if (onsub) {				/* output subsets ? */
         fint	isub;				/* input subset counter */

         for (isub = 0; isub < insub; isub++) {	/* subtract loop */
            fint	np;			/* loop counter */

            gdsi_read_c( iset ,			/* input set */
                         &icwlo[isub] ,		/* lower c.w. */
                         &icwup[isub] ,		/* upper c.w. */
                         idata ,		/* the raw data */
                         &nread ,		/* number to read */
                         &nread ,		/* number read */
                         &itid[isub] );		/* transfer id */
            for (np = 0; np < (npoly+1); np++) {
               float	cf;			/* the factor */

               cf = -ifac[np][isub];		/* negate */
               qcnvl2_c( poly[np], idata, &cf, &nread );
            }
            gdsi_write_c( oset ,		/* output set */
                          &ocwlo[isub] ,	/* lower c.w. */
                          &ocwup[isub] ,	/* upper c.w. */
                          idata ,		/* continuum subtracted */
                          &nread ,		/* number to write */
                          &nread ,		/* number written */
                          &otid[isub] );	/* transfer id */
            minmax3_c( idata ,			/* the data */
                       &nread ,			/* the number of data */
                       &odatamin[isub] ,	/* running minimum */
                       &odatamax[isub] ,	/* running maximum */
                       &onblank[isub] ,		/* running number of blanks */
                       &ocount[isub] );		/* minmax3 counter */
         }
      }
      /*
       * Finally, we calculate the interpolated continuum, if the
       * user wants it.
       */
      if (cnsub) {				/* continuum wanted */
         fint	np;
         float	zero = 0.0;			/* just zero */

         presetr_c( &zero, cdata, &nread );	/* reset */
         for (np = 0; np < (npoly+1); np++) {
            float	cf;			/* the factor */

            cf = cfac[np];			/* the factor */
            qcnvl2_c( poly[np], cdata, &cf, &nread );
         }
         gdsi_write_c( cset ,			/* continuum set */
                       &ccwlo ,			/* lower c.w. */
                       &ccwup ,			/* upper c.w. */
                       cdata ,			/* continuum */
                       &nread ,			/* number to write */
                       &nread ,			/* number written */
                       &ctid );			/* transfer id */
         minmax3_c( cdata ,			/* the data */
                    &nread ,			/* the number of data */
                    &cdatamin ,			/* running minimum */
                    &cdatamax ,			/* running maximum */
                    &cnblank ,			/* running number of blanks */
                    &ccount );			/* minmax3 counter */
      }
      ndone += nread;				/* new number of points done */
      stat[2] = ndone;				/* new level of completion */
      stabar_c( &stat[0], &stat[1], &stat[2] );	/* give user the status */
   } while (ndone != ntotal);			/* we're finished */
   if (onsub) {					/* write the extrema */
      fint	change = 1;			/* delete intersecting levels */

      wminmax_c( oset ,				/* the output set name */
                 osubset ,			/* the subsets */
                 odatamin ,			/* the datamin's */
                 odatamax ,			/* the datamax's */
                 onblank ,			/* the blanks */
                 &onsub ,			/* the number of subsets */  
                 &change );			/* change */
   }
   if (cnsub) {					/* write the extrema */
      fint	change = 1;			/* delete intersecting levels */

      wminmax_c( cset ,				/* the output set name */
                 &csubset ,			/* the subsets */
                 &cdatamin ,			/* the datamin's */
                 &cdatamax ,			/* the datamax's */
                 &cnblank ,			/* the blanks */
                 &cnsub ,			/* the number of subsets */  
                 &change );			/* change */
   }
   finis_c( );					/* close channel with user */
   return( EXIT_SUCCESS );			/* exit with success */
}
