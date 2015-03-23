/* shuffle.c

	Copyright (c) Kapteyn Astronomical Institute 1992
	All Rights Reserved.

#>            shuffle.dc1

Program:      SHUFFLE

Purpose:      The programme shuffles profiles in a set so that they are
              aligned on positions taken from another set.

Category:     ANALYSIS, COMBINATION, MANIPULATION

File:         shuffle.c

Author:       K.G. Begeman

Keywords:

   INSET=     Set and subsets of data to be shuffled. Maximum number
              of subsets is 128.

   CSET=      Set and subset which contains the positions of the
              profile centres. CSET could be a velocify field created by
              MOMENTS or GAUFIT.

   NMAX=      Number of output subsets to one side of the profile centres.
              The total number of output subsets is 2*NMAX+1. Maximum
              value for NMAX is 128.

   CDELT=     Grid separation in units of the output subsets. A default
              is calculated from the input subsets. CDELT must be positive.

   OUTSET=    Set (and subsets) where the shuffled profiles are to be
              stored. The number of output subsets is 2*NMAX+1.

Description:  With the input data cube and one input velocity field,
              SHUFFLE shifts all spectra such that their new origins
              correspond to the velocities in the velocity field. These
              origins are set to the coordinate 0 in the output set.
              The interpolation required along the spectral axis is
              done with a simple linear interpolation.

Example:      <USER> SHUFFLE INSET=n4214 freq-ohel
              Set n4214 has 3 axes
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              FREQ-OHEL          from     1 to    63
              <USER> SHUFFLE CSET=n4214vel param 1
              Set n4214vfield has 3 axes
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              PARAM-GAUFIT       from     1 to     3
              <USER> SHUFFLE NMAX=32
              <USER> SHUFFLE CDELT=8
              <USER> SHUFFLE OUTSET=shuffle
              Set shuffle has 3 axes
              RA-NCP             from  -127 to   128
              DEC-NCP            from  -127 to   128
              VELO               from   -32 to    32
              <STATUS>  SHUFFLE   +++ FINISHED +++

Notes:        In general the axes names of the output set will be the
              same as the ais names of the input set, except for
              frequency axis with velocity information. In these cases
              the axis name will be converted to VELO.

Updates:      Jul 15, 1992: KGB, Document created.
              Mar 09, 2000: JMH, increased NMAX to 128
#<

*/


/*
 * Includes:
 */

#include	"math.h"			/* <math.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"string.h"			/* <string.h> */
#include	"gipsyc.h"			/* GIPSY symbols */
#include	"cmain.h"			/* main c programme */
#include	"anyout.h"			/* defines anyout_c */
#include	"axtype.h"			/* defines axtype_c */
#include	"axunit.h"			/* defines axunit_c */
#include	"cotrans.h"			/* defines contrans_c */
#include	"error.h"			/* defines error_c */
#include	"finis.h"			/* defines finis_c */
#include	"gdsasn.h"			/* defines gdsasn_c */
#include	"gdsc_grid.h"			/* defines gdsc_grid_c */
#include	"gdsc_name.h"			/* defines gdsc_name_c */
#include	"gdsc_ndims.h"			/* defines gdsc_ndims_c */
#include	"gdsc_range.h"			/* defines gdsc_range_c */
#include	"gdsc_word.h"			/* defines gdsc_word_c */
#include	"gdscpa.h"			/* defines gdscpa_c */
#include	"gdsi_read.h"			/* defines gdsi_read_c */
#include	"gdsi_write.h"			/* defines gdsi_write_c */
#include	"gdsinp.h"			/* defines gdsinp_c */
#include	"gdsout.h"			/* defines gdsout_c */
#include	"init.h"			/* defines init_c */
#include	"minmax3.h"			/* defines minmax3_c */
#include	"moved.h"			/* defines moved_c */
#include	"movei.h"			/* defines movei_c */
#include	"nelc.h"			/* defines nelc_c */
#include	"rankda.h"			/* defines rankda_c */
#include	"reject.h"			/* defines reject_c */
#include	"setfblank.h"			/* defines setfblank_c */
#include	"setnfblank.h"			/* defines setnfblank_c */
#include	"stabar.h"			/* defines stabar_c */
#include	"userdble.h"			/* defines userdble_c */
#include	"userint.h"			/* defines userint_c */
#include	"wminmax.h"			/* defines wminmax_c */


/*
 * Defines:
 */

#define	FITSNAMLEN	18			/* length fits item */
#define	KEY_CDELT	tofchar("CDELT=")	/* keyword CDELT= */
#define	KEY_CSET	tofchar("CSET=")	/* keyword CSET= */
#define	KEY_INSET	tofchar("INSET=")	/* keyword INSET= */
#define	KEY_NMAX	tofchar("NMAX=")	/* keyword NMAX= */
#define	KEY_OUTSET	tofchar("OUTSET=")	/* keyword OUTSET= */
#define	MAXAXES		10			/* max. number of axes */
#define	MAXBUF		2048			/* max. size of buffers */
#define	MAXISUB		128			/* max. # of input subsets */
#define	NMAX		128			/* (max. output subs - 1) / 2 */
#define	MAXOSUB		(2*NMAX+1)		/* max. # of output subsets */
#define	MESLEN		80			/* length of messages */
#define	MES_CSET	tofchar("Set and subset of shuffle coordinates")
#define	MES_INSET	tofchar("Set and subsets of data to be shuffled")
#define	MES_NMAX	tofchar("Number of subsets on one side of centre")
#define	MES_OUTSET	tofchar("Set (and subsets) of shuffled data")
#define	SETNAMLEN	256			/* length of set name */
#define	VERSION		"1.0"			/* change version number here */


/*
 * Variables for input set:
 */

static	char	isetb[SETNAMLEN];		/* buffer for input set name */
static	double	icoord[MAXISUB];		/* subset coordinates */
static	fchar	iset = { isetb, SETNAMLEN };	/* points to buffer above */
static	fint	iaxperm[MAXAXES];		/* axes permutation */
static	fint	iaxsize[MAXAXES];		/* axes sizes */
static	fint	icwlo[MAXISUB];			/* lower coordinate words */
static	fint	icwup[MAXISUB];			/* upper coordinate words */
static	fint	insub;				/* number of input subsets */
static	fint	isetdim;			/* dimension of input set */
static	fint	isubdim;			/* dimension of input subsets */
static	fint	isubset[MAXISUB];		/* input subset coord. words */
static	fint	itid[MAXISUB];			/* transfer identifiers */
static	float	idata[2][MAXBUF];		/* data buffer */


/*
 * Variables for input shuffle set:
 */

static	char	csetb[SETNAMLEN];		/* buffer for shuffle set name */
static	fchar	cset = { csetb, SETNAMLEN };	/* points to buffer above */
static	fint	caxperm[MAXAXES];		/* axes permutation */
static	fint	caxsize[MAXAXES];		/* axes sizes */
static	fint	ccwlo;				/* lower coordinate words */
static	fint	ccwup;				/* upper coordinate words */
static	fint	cnsub;				/* number of shuffle subsets */
static	fint	csetdim;			/* dimension of shuffle set */
static	fint	csubdim;			/* dimension of shuffle subset */
static	fint	csubset;			/* fit subset coord. words */
static	fint	ctid;				/* transfer identifiers */
static	float	cdata[MAXBUF];			/* data buffer */


/*
 * Variables for output set:
 */

static	char	osetb[SETNAMLEN];		/* buffer for output set name */
static	fchar	oset = { osetb, SETNAMLEN };	/* points to buffer above */
static	fint	oaxperm[MAXAXES];		/* axes permutation */
static	fint	oaxsize[MAXAXES];		/* axes sizes */
static	fint	ocount[MAXOSUB];		/* counter for minmax3 */
static	fint	ocwlo[MAXOSUB];			/* lower coordinate words */
static	fint	ocwup[MAXOSUB];			/* upper coordinate words */
static	fint	onblank[MAXOSUB];		/* running number of blanks */
static	fint	onsub;				/* number of output subsets */
static	fint	osetdim;			/* dimension of output set */
static	fint	osubdim;			/* dimension of output subs */
static	fint	osubset[MAXOSUB];		/* output subset coord. words */
static	fint	otid[MAXOSUB];			/* transfer identifiers */
static	float	odata[MAXOSUB][MAXBUF];		/* buffer for output data */
static	float	odatamax[MAXOSUB];		/* running maximum */
static	float	odatamin[MAXOSUB];		/* running minimum */


/*
 * Other variables:
 */

static	char	ctypeb[FITSNAMLEN];		/* buffer for ctype */
static	char	cunitb[FITSNAMLEN];		/* buffer for cunit */
static	char	mes[MESLEN];			/* message buffer */
static	double	cdelt;				/* grid separation */
static	double	crota;				/* axis rotation */
static	double	crpix;				/* axis reference */
static	double	crval;				/* axis value */
static	fchar	ctype = { ctypeb, FITSNAMLEN };	/* axis name */
static	fchar	cunit = { cunitb, FITSNAMLEN };	/* axis unit */
static	fint	gerror = 0;			/* GDS error return */
static	fint	naxis;				/* axis length output set */
static	fint	ndone = 0;			/* number of pixels done */
static	fint	nmax;				/* ( # of output sets - 1 ) / 2 */
static	fint	ntotal;				/* total number of pixels */
static	float	blank;				/* BLANK value */
static	float	stat[3];			/* for stabar */


/*
 * The main programme.
 */

MAIN_PROGRAM_ENTRY
{
   init_c( );					/* contact hermes */
   setfblank_c( &blank );			/* obtain BLANK value */
   IDENTIFICATION( "SHUFFLE", VERSION );	/* identify ! */
   /*
    * Here we get the set and subset(s) of the data which should be
    * shuffled.
    */
   {
      fint	class = 2;			/* class for gdsinp */
      fint	classdim = 1;			/* dimension outside subset */
      fint	input_level = 0;		/* no default */
      fint	level = 0;			/* at set level */
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	nitems = MAXISUB;		/* maximum number of items */
      fint	output_level = 11;		/* output level */

      insub = gdsinp_c( iset, isubset, &nitems, &input_level, KEY_INSET,
         MES_INSET, &output_level, iaxperm, iaxsize, &maxaxes, &class,
         &classdim );
      isetdim = gdsc_ndims_c( iset, &level );	/* Dimension of iset */
      isubdim = isetdim - classdim;		/* Dimension of subset */
   }
   /*
    * Now we get the coordinates along the operation axis. The subsets
    * are then sorted in order of increasing coordinate.
    * Also the coordinate words and the transfer identifiers are set.
    */
   {
      double	coords[MAXAXES];		/* physical coordinates */
      double	grids[MAXAXES-1];		/* grids */
      double	ddummy[MAXISUB];		/* dummy double array */
      fint	idummy[MAXISUB];		/* dummy integer array */
      fint	isub;				/* subset counter */
      fint	n;				/* loop counter */
      fint	rank[MAXISUB];			/* ranker */

      for ( n = 0; n < isubdim; n++ ) {		/* loop */
         grids[n] = 0.0;			/* set to zero */
      }
      for ( isub = 0; isub < insub; isub++ ) {	/* loop */
         fint	dir = 1;			/* grids -> physical coords */
         fint	r;				/* return from cotrans */

         r = cotrans_c( iset, &isubset[isub], grids, coords, &dir );
         icoord[isub] = coords[isubdim];	/* save coordinates */
      }
      rankda_c( icoord, rank, &insub );		/* get new ranking */
      /*
       * rearrange subset coordinate words and coordinates according to rank.
       */
      moved_c( icoord, ddummy, &insub );	/* copy coordinates */
      movei_c( isubset, idummy, &insub );	/* copy subset cw's */
      for ( n = 0; n < insub; n++ ) {		/* loop */
         icoord[n]  = ddummy[rank[n]];		/* new rank */
         isubset[n] = idummy[rank[n]];		/* new rank */
      }
      /*
       * Now get coordinate words and reset transfer identifiers.
       */
      for ( isub = 0; isub < insub; isub++ ) {	/* loop */
         itid[isub] = 0;			/* Reset transfer identifiers */
         gdsc_range_c( iset ,			/* name of set */
                       &isubset[isub] ,		/* the subset */
                       &icwlo[isub] ,		/* lower cw */
                       &icwup[isub] ,		/* upper cw */
                       &gerror );		/* error status */
      }
   }
   /*
    * Here we prompt the user for the set and subset which contains the
    * shuffle coordinates. The dimensions of the subset have the match
    * those of the input subsets.
    */
   {
      fint	class = 1;			/* class off gdsinp */
      fint	classdim = isubdim;		/* wanted dimension */
      fint	input_level = 0;		/* no default */
      fint	level = 0;			/* set top level */
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	maxcsubs = 1;			/* maximum number of subsets */
      fint	n;				/* loop counter */
      fint	okay;				/* loop control */
      fint	output_level = 11;		/* output level */

      do {					/* loop until input o.k. */
         okay = 1;				/* reset */
         cnsub = gdsinp_c( cset, &csubset, &maxcsubs, &input_level,
            KEY_CSET, MES_CSET, &output_level, caxperm, caxsize, &maxaxes,
            &class, &classdim );
         csetdim = gdsc_ndims_c( cset, &level );
         csubdim = classdim;
         for ( n = 0, ntotal = 1; n < isubdim && okay; n++ ) {
            if (iaxsize[n] != caxsize[n]) {
               sprintf( mes, "Wrong size of CSET!" );
               okay = 0;
            } else {
               ntotal *= caxsize[n];
            }
         }
         if (!okay) reject_c( KEY_CSET, tofchar( mes ) );
      } while (!okay);				/* input o.k. */
      ctid = 0;					/* reset */
      gdsc_range_c( cset, &csubset, &ccwlo, &ccwup, &gerror );
   }
   /*
    * Next we prompt the user for the number of output subsets on each
    * side of the coordinate which is in the shuffle set (cset). We also
    * need to know the grid separation of the output set.
    */
   {
      char	dunitb[FITSNAMLEN];		/* buffer for sec. units */
      fchar	dunit;				/* secondary ax units */
      fint	at;				/* axis type */
      fint	axnum;				/* axis number */
      fint	input_level = 0;		/* input level */
      fint	nitems = 1;			/* # of items */
      fint	okay = 0;			/* loop control */
      fint	r;				/* return from axunit */
      fint	prosys, skysys, velsys;		/* projection codes */

      dunit.a = dunitb; dunit.l = FITSNAMLEN;	/* make fchar */
      axnum = iaxperm[isubdim];			/* axis number */
      gdsc_name_c( ctype, iset, &axnum, &gerror );
      at = axtype_c( ctype, cunit, dunit, &skysys, &prosys, &velsys );
      if ( at == 3 && velsys ) {		/* new axis -> VELO */
         int	n = 4;

         strncpy( ctype.a, "VELO", 4 );		/* copy name */
         while ( n < FITSNAMLEN ) ctype.a[n++] = ' ';
      }
      do {					/* loop */
         (void) userint_c( &nmax, &nitems, &input_level, KEY_NMAX, MES_NMAX );
         if (nmax < 0 || nmax > NMAX) {		/* error */
            sprintf( mes, "NMAX < 0 or NMAX > %d", NMAX );
         } else {				/* o.k. */
            okay = 1;
         }
         if (!okay) reject_c( KEY_NMAX, tofchar( mes ) );
      } while (!okay);				/* until o.k. */
      naxis = 2 * nmax + 1;			/* number of output subsets */
      r = axunit_c( iset, &axnum, cunit );	/* get cunits */
      cdelt = ( icoord[insub-1] - icoord[0] ) / ( 2 * nmax );
      sprintf( mes, "New grid separation in %.*s [%g]", (int) nelc_c( cunit ), cunit.a, cdelt );
      input_level = 1;				/* default possible */
      okay = 0;					/* not okay */
      do {
         (void) userdble_c( &cdelt, &nitems, &input_level, KEY_CDELT,
            tofchar( mes ) );
         if (cdelt > 0.0) {			/* good */
            okay = 1;
         }
         if (!okay) reject_c( KEY_CDELT, tofchar( "CDELT must be positive!" ) );
      } while (!okay);				/* until o.k. */
      crota = 0.0;				/* no axis rotation */
      crpix = nmax + 1;				/* reference pixel */
      crval = 0.0;				/* reference value */
   }
   /*
    * Here we ask the user to enter the output set.
    */
   {
      fint	class = 2;			/* class */
      fint	input_level = 4;		/* input level */
      fint	maxaxes = MAXAXES;		/* max. # of axis */
      fint	osub;				/* subset counter */
      fint	output_level = 11;		/* output_level */
      fint	pmask = 47;			/* axis mask */

      osetdim = isubdim + 1;			/* dimension output set */
      osubdim = isubdim;			/* subset dimension */
      gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
      gdscpa_c( KEY_OUTSET, &osetdim, &naxis, &cdelt, &crota, &crpix, &crval,
         ctype, cunit, &pmask );
      onsub = gdsout_c( oset, osubset, &naxis, &input_level, KEY_OUTSET,
         MES_OUTSET, &output_level, oaxperm, oaxsize, &maxaxes );
      for ( osub = 0; osub < onsub; osub++ ) {
         ocount[osub] = 0;			/* reset minmax3 counter */
         otid[osub] = 0;			/* reset transfer id */
         gdsc_range_c( oset, &osubset[osub], &ocwlo[osub], &ocwup[osub],
            &gerror );
      }
   }
   /*
    * Now we do the shuffle.
    */
   stat[0] = 0.0; stat[1] = ntotal; stat[2] = ndone;
   stabar_c( &stat[0], &stat[1], &stat[2] );	/* give user the status */
   do {						/* big loop */
      double	c1, c2;				/* coordinates */
      fint	isub;				/* subset counter */
      fint	maxbuf = MAXBUF;		/* size of buffers */
      fint	n;				/* counter */
      fint	ndummy;				/* dummy */
      fint	nread;				/* # of pixels read */
      fint	osub;				/* subset counter */
      float	*data1, *data2;			/* pointers to buffers */

      data1 = idata[0]; data2 = idata[1];	/* set pointers */
      /*
       * Now we read the subset which contains the coordinates of the
       * centre of the new set (i.e. we shuffle the data around the
       * coordinates read from this subset).
       */
      gdsi_read_c( cset, &ccwlo, &ccwup, cdata, &maxbuf, &nread, &ctid );
      /*
       * Blank the output subsets.
       */
      for ( osub = 0; osub < onsub; osub++ ) {
         setnfblank_c( odata[osub], &nread );
      }
      /*
       * Next we read the first subset of the input set.
       */
      gdsi_read_c( iset, &icwlo[0], &icwup[0], data1, &nread, &ndummy,
         &itid[0] );
      c1 = icoord[0];				/* coord. first input subset */
      /*
       * In the following loop, we read the next input subset, determine
       * whether there is any data we need for the output set, and if so
       * we interpolate between data in buffer data1 (previous input subset)
       * and data in buffer data2 (current input subset).
       */
      for ( isub = 1; isub < insub; isub++ ) {
         /*
          * read data from next input subset.
          */
         gdsi_read_c( iset, &icwlo[isub], &icwup[isub], data2, &nread,
             &ndummy, &itid[isub] );
         c2 = icoord[isub];			/* coord. current input subset */
         /*
          * the next loop goes over all data currently in the read in
          * buffers (data1 and data2) and interpolates a value for the
          * output set.
          */
         for ( n = 0; n < nread; n++ ) {	/* loop */
            double	cn = cdata[n];
            float	i1 = data1[n];
            float	i2 = data2[n];

            if ( cn != blank && i1 !=blank && i2 != blank ) {
               fint	l, le, ls;		/* counters */

               ls = ceil( ( c1 - cn ) / cdelt );
               le = floor( ( c2 - cn ) / cdelt );
               if ( ls < -nmax ) ls = -nmax;
               if ( le > nmax ) le = nmax;
               for ( l = ls; l <= le; l++ ) {
                  double	c = cn + l * cdelt;
                  float		v = odata[l+nmax][n];

                  if ( v == blank) {
                     float	w;

                     w = ( c2 - c ) / ( c2 - c1 );
                     v = i1 * w + i2 * ( 1 - w );
                  }
                  odata[l+nmax][n] = v;
               }
            }
         }
         /*
          * switch buffers and coordinates.
          */
         {
            float	*d, c;

            d = data1; data1 = data2; data2 = d;
            c = c1; c1 = c2; c2 = c;
         }
      }
      /*
       * put out the shuffled data.
       */
      for ( osub = 0; osub < onsub; osub++ ) {
         gdsi_write_c( oset, &ocwlo[osub], &ocwup[osub], odata[osub],
            &nread, &ndummy, &otid[osub] );
         minmax3_c( odata[osub], &nread, &odatamin[osub], &odatamax[osub],
            &onblank[osub], &ocount[osub] );
      }
      /*
       * update statistics.
       */
      ndone += nread;				/* new number of pixels */
      stat[2] = ndone;				/* new stats */
      stabar_c( &stat[0], &stat[1], &stat[2] );	/* give user the status */
   } while ( ndone < ntotal );			/* until done */
   /*
    * update DATAMIN, DATAMAX and NBLANK.
    */
   {
      fint	change = 1;			/* delete intersecting levels */

      wminmax_c( oset, osubset, odatamin, odatamax, onblank, &onsub, &change );
   }
   finis_c( );					/* say goodbye to hermes */
   return( 0 );					/* return status */
}
