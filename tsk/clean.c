/* clean.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            clean.dc1

Program:      CLEAN

Purpose:      This program cleans maps or lines.

Category:     CLEAN, MANIPULATION, RADIO, TABLES

File:         clean.c

Author:       K.G. Begeman

Description:  CLEAN finds point source components and subtracts them with
              their antenna response. This is the usual method of
              decomposition of extended sources into delta functions.
              (This  method  was developed by Hogbom, A&A Suppl. 15, p.417
              (1974))
              Note: You have to use the program RESTORE to get your 'clean'
              source back.

Keywords:

   INSET=     Set and subset(s) of dirty map(s)/line(s). Maximum number of
              subset(s) is 2048. The dimension of the subsets can be one
              or two.

   APSET=     Set and subset(s) of antenna pattern(s).

   OUTSET=    Set and subset(s) of residual(s).

   DEFSET=    Set and subset(s) for definition of search area
              [Search area defined by box].
              CLEAN looks for components only where the values in these
              maps/lines are not equal to BLANK. These maps/lines act as
              logical masks for the search area, and can easily be obtained
              by using e.g. the program BLOT on the dirty maps. The number
              of subsets entered must be equal to the number of dirty maps
              or one, which means that the same mask will be used for all
              dirty maps.

   SEARCHBOX= Search area. Here the components are searched and subtracted.
              Note: the search area does not have to be inside clean area.

   CLEANBOX=  Clean area [whole subset]. Here the components are subtracted.

   NMAX=      Maximum number of components [10000].
              Maximum number allowed 30000.

   CUTOFF=    Cutoff level [0.0].
              Iterations stop when abs of minimum and maximum are less
              than cutoff.

** GAIN=      Loop gain factor (standard Hogbom gain) [0.7].

** FACTOR=    Factor to govern when to start subtracting negative
              components [1.0].
              If factor is equal to 1.0 then both positive and negative
              components are searched. If component amplitudes are less
              than factor*maximum in map/line, then both positive and negative
              components are searched. Otherwise only positive components.

** NPOS=      Number of positive components to be searched before negative
              components are considered [0].

** CUTPOS=    Cutoff value for positive components [no cutoff].
              No negative components will be subtracted until the
              maximum map/line value is less than CUTPOS.

** CUTOPT=    Cutoff for optimized clean [no optimization].
              With this keyword it is possible to 'optimize' the cleaning
              process (Schwarz, A&A 65, p.345 (1978)). Optimization means
              that first the dirty map/line in cleaned in the normal way
              with a large cutoff (> 2 sigma) (specified by CUTOPT=). Next
              the cleaning resumes with a gain of one, and the search area
              is restricted to those positions where clean already found
              components. This second run is controlled by the keyword CUT=,
              which should be about 5 times less than CUTPOS=.

** CONTINUE=  Continuation of a previous clean [N]?
              If No then an already existing component table will be
              deleted. If Yes then the component table will be extended
              with the components found by CLEAN.

** TEST=      Extra output for debugging [N]?
              If you find that CLEAN does not work properly, run the CLEAN
              again with TEST=Y and send the output to the author.

Notes:        1. The maximum size of the search area depends on the
                 amount of available memory.

              2. You can stop the iterations by typing CLEAN, STOP=Y.

              3. Speed of two-dimensional CLEAN

                 -----------------------------------------------------------
                 | clean area |             components/minute              |
                 |            |   VAX8600    | ALLIANT FX80 |   SPARC 1+   |
                 -----------------------------------------------------------
                 | 128 * 128  |       830    |              |              |
                 | 256 * 256  |       135    |      364     |      543     |
                 | 512 * 512  |        28    |              |              |
                 -----------------------------------------------------------

              4. With the hidden keyword TEST=Y you will get some extra
                 information which is only useful for me (KGB). When you
                 have complaints about CLEAN, always show me this output.
                 Thanx!

              5. CLEAN checks for BLANKS (undefined values) before it starts
                 cleaning. When CLEAN encounters BLANKS in the dirty map/line
                 when it is in the search phase it skips this map/line. In the
                 subtract phase BLANKS are replaced by zeroes! BLANKS in the
                 antenna pattern are always replaced by zeroes! CLEAN ALWAYS
                 reports BLANKS! So be careful when this happens.

              6. CLEAN saves the primary beam corrected component flux, the
                 dirty flux and the residual flux in header items CFLUX1,
                 DFLUX1 and RFLUX1 resp. at the subset level in the output
                 set, and the uncorrected fluxes in resp. UCFLUX1, UDFLUX1
                 and URFLUXU. Also the number of components are stored in
                 NCOMP1. If these header items already exist, CLEAN saves the
                 values in CFLUX2 etc, unless CONTINUE=N.

Updates:      Apr 17, 1991: KGB Document created.
              Dec 11, 1991: KGB Cancel replaced by reject.
              Feb 12, 1992: KGB Check for BLANKS implemented.
              Apr  5, 1992: KGB One-dimensional clean implemented.
              Feb 15, 1994: KGB History, comment and header items added.
              Mar  8, 2006: JPT Fixed freeing allocated memory.

#<

*/



/*
 * includes:
 */

#include	"float.h"		/* <float.h> */
#include	"math.h"		/* <math.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"time.h"		/* <time.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"cmain.h"		/* application written in C */
#include	"anyout.h"		/* define anyout_c */
#include	"axunit.h"		/* define axunit_c */
#include	"cancel.h"		/* define cancel_c */
#include	"cotrans.h"		/* define cotrans_c */
#include	"error.h"		/* define error_c*/
#include	"finis.h"		/* define finis_c */
#include	"gdsa_colinq.h"		/* define gdsa_colinq_c */
#include	"gdsa_crecol.h"		/* define gdsa_crecol_c */
#include	"gdsa_delcol.h"		/* define gdsa_delcol_c */
#include	"gdsa_wcint.h"		/* define gdsa_wcint_c */
#include	"gdsa_wcreal.h"		/* define gdsa_wcreal_c */
#include	"gdsasn.h"		/* define gdsasn_c */
#include	"gdsbox.h"		/* define gdsbox_c */
#include	"gdsc_fill.h"		/* define gdsc_fill_c */
#include	"gdsc_grid.h"		/* define gdsc_grid_c */
#include	"gdsc_name.h"		/* define gdsc_name_c */
#include	"gdsc_ndims.h"		/* define gdsc_ndims_c */
#include	"gdsc_range.h"		/* define gdsc_range_c */
#include	"gdsd_delete.h"		/* define gdsd_delete_c */
#include	"gdsd_history.h"	/* define gdsd_history_c */
#include	"gdsd_rchar.h"		/* define gdsd_rchar_c */
#include	"gdsd_rint.h"		/* define gdsd_rint_c */
#include	"gdsd_rreal.h"		/* define gdsd_rreal_c */
#include	"gdsd_wint.h"		/* define gdsd_wint_c */
#include	"gdsd_wreal.h"		/* define gdsd_wreal_c */
#include	"gdsd_wvar.h"		/* define gdsd_wvar_c */
#include	"gdsi_read.h"		/* define gdsi_read_c */
#include	"gdsi_write.h"		/* define gdsi_write_c */
#include	"gdsinp.h"		/* define gdsinp_c */
#include	"gdsout.h"		/* define gdsout_c */
#include	"init.h"		/* define init_c */
#include	"listctrl.h"		/* define listctrl_c */
#include	"minmax2.h"		/* define minmax2_c */
#include	"nelc.h"		/* define nelc_c */
#include	"pribeam.h"		/* define pribeam_c */
#include	"qcln1.h"		/* define qcln1_c */
#include	"qcln2.h"		/* define qcln2_c */
#include	"reject.h"		/* define reject_c */
#include	"setfblank.h"		/* define setfblank_c */
#include	"stabar.h"		/* define stabar_c */
#include	"statr.h"		/* define statr_c */
#include	"status.h"		/* define status_c */
#include	"timer.h"		/* define timer_c */
#include	"userint.h"		/* define userint_c */
#include	"userlog.h"		/* define userlog_c */
#include	"userreal.h"		/* define userreal_c */



/*
 * defines:
 */

#define	CLASS		1		/* class of application */
#define	CLASSDIM	2		/* maximum dimension of subsets */
#define	MAXAXES		10		/* maximum number of axes */
#define	MAXCOLNAMLEN	8		/* maximum length of column name */
#define	MAXCOMP		50000		/* maximum number of components */
#define	MAXDATA		4096		/* maximum buffer length */
#define	MAXFITSCHARLEN	18		/* maximum length of FITS character */
#define	MAXMAPS		2048		/* maximum number of maps */
#define	MAXSETNAMLEN	80		/* maximum length of set names */
#define	MAXSTRINGLEN	80		/* maximum length of text strings */
#define	VERSION		"1.1"		/* change version number here */

#define	SUBTRACT(x1,y1,x2,y2)		subtract( oset , \
                                                  omaps[nmap] , \
                                                  aset , \
                                                  amaps[nmap] , \
                                                  map , \
                                                  mapsize , \
                                                  apt , \
                                                  aptsize , \
                                                  amps , \
                                                  xpos , \
                                                  ypos , \
                                                  ncc , \
                                                  sllo , \
                                                  smlo , \
                                                  slup , \
                                                  smup , \
                                                  x1 , \
                                                  y1 , \
                                                  x2 , \
                                                  y2 , \
                                                  &dd , \
                                                  &dt , \
                                                  &cs1 , \
                                                  &cs2 )



/*
 * Keywords:
 */

#define	KEY_INSET	tofchar("INSET=")
#define	KEY_APSET	tofchar("APSET=")
#define	KEY_OUTSET	tofchar("OUTSET=")
#define	KEY_DEFSET	tofchar("DEFSET=")
#define	KEY_SEARCHBOX	tofchar("SEARCHBOX=")
#define	KEY_CLEANBOX	tofchar("CLEANBOX=")
#define	KEY_NMAX	tofchar("NMAX=")
#define	KEY_CUTOFF	tofchar("CUTOFF=")
#define	KEY_GAIN	tofchar("GAIN=")
#define	KEY_FACTOR	tofchar("FACTOR=")
#define	KEY_NPOS	tofchar("NPOS=")
#define	KEY_CUTPOS	tofchar("CUTPOS=")
#define	KEY_CUTOPT	tofchar("CUTOPT=")
#define	KEY_CONTINUE	tofchar("CONTINUE=")
#define	KEY_TEST	tofchar("TEST=")
#define	KEY_STOP	tofchar("STOP=")



/*
 * Messages:
 */

#define	MES_INSET	tofchar("Set and subset(s) of dirty maps")
#define	MES_APSET	tofchar("Set and subset(s) of antenna pattern(s)")
#define	MES_OUTSET	tofchar("Set and subset(s) for residual map(s)")
#define	MES_DEFSET	tofchar("Set and subset(s) which define search area")
#define	MES_SEARCHBOX	tofchar("Search box")
#define	MES_CLEANBOX	tofchar("Clean box [whole map]")
#define	MES_NMAX	tofchar("Maximum number of components [10000]")
#define	MES_CUTOFF	tofchar("Cutoff level [0.0]")
#define	MES_GAIN	tofchar("Loop gain factor [0.7]")
#define	MES_FACTOR	tofchar("Factor [1.0]")
#define	MES_NPOS	tofchar("Number of positive components [0]")
#define	MES_CUTPOS	tofchar("Cuttoff for positive components [no cutoff]")
#define	MES_CUTOPT	tofchar("Cutoff for optimized clean [no optimization]")
#define	MES_CONTINUE	tofchar("Continuation of previous clean [N]?")
#define	MES_TEST	tofchar("Extra output for debugging [N]?")
#define	MES_STOP	tofchar("Stop clean [N]?")



/*
 * Tables:
 */

#define	TABLE_NAME	tofchar("CLEAN")
#define	TABLE_COLUMN_A	tofchar("AMP")
#define	TABLE_COLUMN_X	tofchar("XPOS")
#define	TABLE_COLUMN_Y	tofchar("YPOS")



/*
 * global static variables:
 */

static	char	*dim[] = { "Line", "Map" };	/* working on dim */
static	fint	classdim = 0;			/* 1 or 2 dimensional clean */
static	float	blank;				/* Universal BLANK */



/*
 * imin returns the minimum of the two fint arguments.
 */

static	fint	IMIN( fint arg1 ,		/* first integer */
                      fint arg2 )		/* second integer */
{
   if (arg1 < arg2) return( arg1 ); else return( arg2 );
}



/*
 * imax returns the maximum of the two fint arguments.
 */

static	fint	IMAX( fint arg1 ,		/* first integer */
                      fint arg2 )		/* second integer */
{
   if (arg1 > arg2) return( arg1 ); else return( arg2 );
}



/*
 * addr_descriptor saves the value v in descriptor d<n>, where <n> is
 * next number, or in d1 if cont is false.
 */

static	void	addr_descriptor( fchar set,	/* name of set */
                                fint  subset,	/* subset level */
                                char  *d,	/* name of descriptor */
                                float v,	/* the value */
                                bool  cont )	/* continue */
{
   char		dname[20];			/* name of descriptor */
   fint		gerror;				/* gds error return */
   float	oldv;				/* old value */
   int		n = 0;				/* counter */

   do {
      gerror = 0;
      n += 1;
      sprintf( dname, "%s%d", d, n );
      gdsd_rreal_c( set, tofchar( dname ), &subset, &oldv, &gerror );
      if ( !cont && gerror == subset ) {
         fint	gerror = 0;

         gdsd_delete_c( set, tofchar( dname ), &subset, &gerror );
      }
   } while ( gerror == subset );
   if ( !cont ) n = 1;
   sprintf( dname, "%s%d", d, n );
   gerror = 0;
   gdsd_wreal_c( set, tofchar( dname ), &subset, &v, &gerror );
}



/*
 * adds text to the comments.
 */

static	void	comment( fchar set,		/* name of set */
                         fint  subset,		/* subset level */
                         char  *text )		/* the text */
{
   fint	gerror = 0;				/* gds error return */

   gdsd_wvar_c( set, tofchar( "COMMENT" ), &subset, tofchar( text ), &gerror );
}



/*
 * addi_descriptor saves the value v in descriptor d<n>, where <n> is
 * next number, or in d1 if cont is false.
 */

static	void	addi_descriptor( fchar set,	/* name of set */
                                fint  subset,	/* subset level */
                                char  *d,	/* name of descriptor */
                                fint  v,	/* the value */
                                bool  cont )	/* continue */
{
   char	dname[20];				/* name of descriptor */
   fint	gerror;					/* gds error return */
   fint	oldv;					/* old value */
   int	n = 0;					/* counter */

   do {
      gerror = 0;
      n += 1;
      sprintf( dname, "%s%d", d, n );
      gdsd_rint_c( set, tofchar( dname ), &subset, &oldv, &gerror );
      if ( !cont && gerror == subset ) {
         fint	gerror = 0;

         gdsd_delete_c( set, tofchar( dname ), &subset, &gerror );
      }
   } while ( gerror == subset );
   if ( !cont ) n = 1;
   sprintf( dname, "%s%d", d, n );
   gerror = 0;
   gdsd_wint_c( set, tofchar( dname ), &subset, &v, &gerror );
}



/*
 * readxy reads a part of a one or two-dimensional subset.
 */

static	void	readxy( fchar set,		/* name of set */
                        fint  subset,		/* subset level */
                        fint  xlo,		/* lower x grid */
                        fint  ylo,		/* lower y grid */
                        fint  xup,		/* upper x grid */
                        fint  yup,		/* upper y grid */
                        float *data )		/* the data to read */
{
   fint	cwlo;					/* lower coordinate word */
   fint	cwup;					/* upper coordinate word */
   fint	glo[CLASSDIM];				/* lower grid coordinates */
   fint	gup[CLASSDIM];				/* upper grid coordinates */
   fint	tid = 0;				/* transfer id */
   fint	ndone;					/* number of pixels read */
   fint	nread;					/* number of pixels to read */

   glo[0] = xlo; glo[1] = ylo;			/* lower grids */
   gup[0] = xup; gup[1] = yup;			/* upper grids */
   nread = (yup-ylo+1) * (xup-xlo+1);		/* number of pixels */
   cwlo = gdsc_fill_c( set, &subset, glo );	/* lower c.w. */
   cwup = gdsc_fill_c( set, &subset, gup );	/* upper c.w. */
   gdsi_read_c( set, &cwlo, &cwup, data, &nread, &ndone, &tid );
   if (tid) {					/* error ? */
      fint	error_level = 4;		/* FATAL error */

      error_c( &error_level , tofchar( "Error reading data!" ) );
   }
}



/*
 * writxy writes a part of a one or two-dimensional subset.
 */

static	void	writxy( fchar set,		/* name of set */
                        fint  subset,		/* subset level */
                        fint  xlo,		/* lower x grid */
                        fint  ylo,		/* lower y grid */
                        fint  xup,		/* upper x grid */
                        fint  yup,		/* upper y grid */
                        float *data )		/* the data to write */
{
   fint	cwlo;					/* lower coordinate word */
   fint	cwup;					/* upper coordinate word */
   fint	glo[CLASSDIM];				/* lower grid coordinates */
   fint	gup[CLASSDIM];				/* upper grid coordinates */
   fint	tid = 0;				/* transfer id */
   fint	ndone;					/* number of pixels written */
   fint	nwrit;					/* number of pixels to write */

   glo[0] = xlo; glo[1] = ylo;			/* lower grids */
   gup[0] = xup; gup[1] = yup;			/* upper grids */
   nwrit = ( yup - ylo + 1 ) * ( xup - xlo + 1 );	/* number of pixels */
   cwlo = gdsc_fill_c( set, &subset, glo );	/* lower c.w. */
   cwup = gdsc_fill_c( set, &subset, gup );	/* upper c.w. */
   gdsi_write_c( set, &cwlo, &cwup, data, &nwrit, &ndone, &tid );
   if (tid) {					/* error ? */
      fint	error_level = 4;		/* FATAL error */

      error_c( &error_level , tofchar( "Error writing data!" ) );
   }
}



/*
 * subtract subtracts the components found in routine clean_search
 * from part of the dirty map.
 */

static	void	subtract( fchar oset ,		/* output set name */
                          fint  omap ,		/* output subset level */
                          fchar aset ,		/* antenna set name */
                          fint  amap ,		/* antenna subset level */
                          float *map ,		/* buffer for map data */
                          fint  mapsize ,	/* size of map data */
                          float *apt ,		/* buffer for ap data */
                          fint  aptsize ,	/* size of apt data */
                          float *amps ,		/* the amplitudes*/
                          fint  *xpos ,		/* the x positions */
                          fint  *ypos ,		/* the y positions */
                          fint  ncc ,		/* the number of components */
                          fint  sllo ,		/* lower x of search area */
                          fint  smlo ,		/* lower y of search area */
                          fint  slup ,		/* upper x of search area */
                          fint  smup ,		/* upper y of search area */
                          fint  lblo ,		/* lower x of clean area */
                          fint  mblo ,		/* lower y of clean area */
                          fint  lbup ,		/* upper x of clean area */
                          fint  mbup ,		/* upper y of clean area */
                          float *dd ,		/* number of pixels done */
                          float *dt ,		/* total number of pixels */
                          float	*cs1 ,		/* sum clean area */
                          float	*cs2 )		/* sum squared clean area */
{
   char		string[MAXSTRINGLEN];		/* text buffer */
   fint		i, j,  m;			/* loop counters*/
   fint		iapt, imap;			/* ap and map counters */
   fint		lclo, lcup, mdim;		/* area counters */
   fint		lengt;				/* length of x block */
   fint		lleft, lstep, ltotl;		/* x loop control */
   fint		mleft, mstep, mtotl;		/* y loop control */
   fint		nls = slup - sllo + 1;		/* x size of search area */
   fint		nms = smup - smlo + 1;		/* y size of search area */

   /*
    * First message to user.
    */
   sprintf( string, "Ready with %6.2f%% of %s", (*dd) / (*dt) * 100.0, dim[classdim-1] );
   status_c( tofchar( string ) );		/* message for user */
   /*
    * In the following loop we try to determine the size of the
    * sub blocks (lstep and mstep).
    */
   lstep = lbup - lblo + 1;			/* step in x */
   do {						/* loop to get lstep right */
      lengt = lstep + nls - 1;			/* length of block in x */
      mstep = IMIN( mapsize / lstep, aptsize / lengt - nms + 1 );
      if (mstep <= 0) lstep /= 2;		/* decrease lstep */
      if (lstep <= 0) {				/* this should not happen */
         fint	error_level = 4;		/* FATAL error */

         error_c( &error_level, tofchar( "Out of Memory!" ) );
      }
   } while (!mstep);				/* stop if largest mstep found */
   lleft = lbup - lblo + 1;			/* number of columns left */
   ltotl = 0;					/* number of columns done */
   do {						/* the x loop */
      lstep = IMIN( lleft, lstep );		/* number of columns */
      lclo = lblo + ltotl;			/* first column */
      lcup = lclo + lstep - 1;			/* last column */
      lengt = lstep + nls - 1;			/* size of antenna pattern */
      mleft = mbup - mblo + 1;			/* number of rows left */
      mtotl = 0;				/* number of rows done */
      do {					/* the y loop */
         m = mblo + mtotl;			/* first row */
         mdim = IMIN( mleft, mstep );		/* number of rows */
         readxy( aset ,				/* antenna set name */
                 amap ,				/* antenna subset level */
                 lclo - slup ,			/* lower x */
                 m - smup ,			/* lower y */
                 lcup - sllo ,			/* upper x */
                 m - smlo + mdim - 1 ,		/* upper y */
                 apt );				/* the buffer */
         {
            fint	count = 0;
            fint	i;
            fint	n;
            fint	ni = ( lstep - sllo + slup );
            fint	nj = ( mdim + smup - smlo );

            n = ni * nj;
            for (i = 0; i < n; i++) {
               if (apt[i] == blank) {
                  apt[i] = 0.0; count++;
               }
            }
            if (count) {
               fint	error_level = 1;

               error_c( &error_level ,
                        tofchar( "Found BLANKS in Antenna Pattern (replaced by 0.0)!" ) );
            }
         }
         readxy( oset ,				/* output set name */
                 omap ,				/* output subset level */
                 lclo ,				/* lower x */
                 m ,				/* lower y */
                 lstep + lclo - 1 ,		/* upper x */
                 mdim + m - 1 ,			/* upper y */
                 map );				/* the buffer */
         {
            fint	count = 0;
            fint	i;
            fint	n;
            fint	ni = ( lstep );
            fint	nj = ( mdim );

            n = ni * nj;
            for (i = 0; i < n; i++) {
               if (map[i] == blank) {
                  map[i] = 0.0; count++;
               }
            }
            if (count) {
               fint	error_level = 1;

               error_c( &error_level ,
                        tofchar( "Found BLANKS in Map (replaced by 0.0)!" ) );
            }
         }
         for (i = 0; i < ncc; i++) {		/* subtract loop */
            fint	lc = xpos[i];		/* x position component */
            fint	mc = ypos[i];		/* y position component */
            float	am = amps[i];		/* amplitude component */

            for (j = 0; j < mdim; j++) {	/* loop over all lines */
               imap = j * lstep;		/* pointer in map */
               iapt = ( j - mc + smup ) * lengt + slup - lc;
               qcln2_c( &map[imap], &apt[iapt], &am, &lstep );
            }
         }
         writxy( oset ,				/* output set name */
                 omap ,				/* output subset level */
                 lclo ,				/* lower x */
                 m ,				/* lower y */
                 lstep + lclo - 1 ,		/* upper x */
                 mdim + m - 1 ,			/* upper y */
                 map );				/* buffer */
         for (i = 0; i < mdim * lstep; i++) {
            (*cs1) += map[i];
            (*cs2) += ( map[i] * map[i] );
         }
         mleft -= mdim;				/* number of rows left */
         mtotl += mdim;				/* number of rows done */
         (*dd) += lstep * mdim;			/* number of points done */
         /*
          * Next message to user.
          */
         sprintf( string, "Ready with %6.2f%% of map", (*dd) / (*dt) * 100.0 );
         status_c( tofchar( string ) );		/* message for user */
      } while (mleft);				/* until all rows done */
      lleft -= lstep;				/* number of columns left */
      ltotl += lstep;				/* number of columns done */
   } while (lleft);				/* until all columns done */
}



/*
 * Variables for input (dirty) set:
 */

static	char	ictypeb[MAXAXES*MAXFITSCHARLEN];/* buffer for axis names */
static	char	icunitb[MAXAXES*MAXFITSCHARLEN];/* buffer for axis units */
static	char	isetb[MAXSETNAMLEN];		/* buffer for set name */
static	fchar	icunit[MAXAXES];		/* axis units */
static	fchar	ictype[MAXAXES];		/* axis names */
static	fchar	iset = { isetb, MAXSETNAMLEN };	/* input set name */
static	fint	imaps[MAXMAPS];			/* input subsets */
static	fint	iperm[MAXAXES];			/* axes permutation */
static	fint	isize[MAXAXES];			/* size of axes */
static	fint	inmap;				/* number of input subsets */
static	fint	imin[CLASSDIM], imax[CLASSDIM];	/* edges of subsets */
static	fint	isetdim;			/* dimension of set */



/*
 * Variables for antenna (dirty) set:
 */

static	char	asetb[MAXSETNAMLEN];		/* buffer for set name */
static	fchar	aset = { asetb, MAXSETNAMLEN };	/* antenna set name */
static	fint	amaps[MAXMAPS];			/* antenna subsets */
static	fint	aperm[MAXAXES];			/* axes permutation */
static	fint	asize[MAXAXES];			/* size of axes */
static	fint	anmap;				/* number of antenna subsets */
static	fint	amin[CLASSDIM], amax[CLASSDIM];	/* edges of subsets */
static	fint	alow[CLASSDIM], aupp[CLASSDIM];	/* real edges of subsets */



/*
 * Variables for output (clean) set:
 */

static	char	osetb[MAXSETNAMLEN];		/* buffer for set name */
static	fchar	oset = { osetb, MAXSETNAMLEN };	/* output set name */
static	fint	omaps[MAXMAPS];			/* output subsets */
static	fint	operm[MAXAXES];			/* axes permutation */
static	fint	osize[MAXAXES];			/* size of axes */
static	fint	onmap;				/* number of output subsets */
static	fint	omin[CLASSDIM], omax[CLASSDIM];	/* edges of subsets */



/*
 * Variables for definition (blotch) set:
 */

static	char	dsetb[MAXSETNAMLEN];		/* buffer for set name */
static	fchar	dset = { dsetb, MAXSETNAMLEN };	/* definition set name */
static	fint	dmaps[MAXMAPS];			/* definition subsets */
static	fint	dperm[MAXAXES];			/* axes permutation */
static	fint	dsize[MAXAXES];			/* size of axes */
static	fint	dnmap;				/* number of definition subsets */
static	fint	dmin[CLASSDIM], dmax[CLASSDIM];	/* edges of subsets */



/*
 * Variables for search area:
 */

static	fint	smin[CLASSDIM], smax[CLASSDIM];	/* edges of search area */



/*
 * Variables for clean area:
 */

static	fint	cmin[CLASSDIM], cmax[CLASSDIM];	/* edges of clean area */
static	fint	mmin[CLASSDIM], mmax[CLASSDIM];	/* edges minimum clean area */
static	fint	msize[CLASSDIM];		/* size of core area */



/*
 * Variables for clean control:
 */

static	bool	cont;				/* continuation of previous run */
static	bool	test;				/* test output */
static	fint	nmax;				/* maximum number of components */
static	fint	npos;				/* first number of positive components */
static	float	cutoff;				/* cutoff */
static	float	cutopt;				/* cutoff for optimization */
static	float	cutpos;				/* positive cutoff */
static	float	gain;				/* loop gain */
static	float	factor;				/* the factor */



/*
 * Variables for the components:
 */

static	fint	xpos[MAXCOMP];			/* x positions of components */
static	fint	ypos[MAXCOMP];			/* y positions of components */
static	float	amps[MAXCOMP];			/* amplitudes of components */



/*
 * Variables for essential header items:
 */

static	char	bunitb[MAXFITSCHARLEN];		/* buffer for FITS character */
static	fchar	bunit = { bunitb, MAXFITSCHARLEN };



/*
 * main program:
 */

MAIN_PROGRAM_ENTRY
{
   char		string[MAXSTRINGLEN];		/* text buffer */
   fchar	errmes;				/* error message */
   fint		aptsize = 0;			/* size of ap buffer */
   fint		gerror = 0;			/* GDS error return */
   fint		mapsize = 0;			/* size of map buffer */
   fint		nmap;				/* map counter */
   float	*apt = NULL;			/* the antenna data */
   float	*map = NULL;			/* the map data */

   init_c( );					/* start of GIPSY application */
   IDENTIFICATION( "CLEAN", VERSION );		/* show user who we are */
   errmes.a = string; errmes.l = sizeof( string );
   setfblank_c( &blank );			/* system defined blank */
   /*
    * First we ask the user for the set and subsets which he or she wants
    * to clean. Then we get the grids coordinates of the lower and upper
    * edges of the maps.
    */
   {
      fint	class = CLASS;			/* class of application */
      fint	cwlo;				/* lower c.w. */
      fint	cwup;				/* upper c.w. */
      fint	i;				/* loop counter */
      fint	input_level = 0;		/* no default*/
      fint	level = 0;			/* top subset level */
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	maxmaps = MAXMAPS;		/* maximum number of maps */
      fint	n;				/* loop counter */
      fint	output_level = 11;		/* output level */

      inmap = gdsinp_c( iset ,			/* input set name */
                        imaps ,			/* input subsets */
                        &maxmaps ,		/* maximum number of subsets */
                        &input_level ,		/* input level */
                        KEY_INSET ,		/* keyword */
                        MES_INSET ,		/* message */
                        &output_level ,		/* output level */
                        iperm ,			/* axes permutation */
                        isize ,			/* axes counter */
                        &maxaxes ,		/* maximum number of axis */
                        &class ,		/* class of program */
                        &classdim );		/* wanted dimension */
      if (classdim != 1 && classdim != 2) {	/* wrong dimension */
         fint	error_level = 4;		/* the error level */

         error_c( &error_level,
                  tofchar( "Subset dimension must be 1 or 2!" ) );
      }
      isetdim = gdsc_ndims_c( iset ,		/* input set name */
                              &level );		/* top subset level */
      for (n = 0; n < isetdim; n++) {
         fint	axnum = n + 1;

         ictype[n].a = &ictypeb[n*MAXFITSCHARLEN];
         ictype[n].l = MAXFITSCHARLEN;
         gdsc_name_c( ictype[n], iset, &axnum, &gerror );
         icunit[n].a = &icunitb[n*MAXFITSCHARLEN];
         icunit[n].l = MAXFITSCHARLEN;
         (void) axunit_c( iset, &axnum, icunit[n] );
      }
      gdsc_range_c( iset ,			/* set name */
                    imaps ,			/* first subset */
                    &cwlo ,			/* lower c.w. */
                    &cwup ,			/* upper c.w. */
                    &gerror );			/* error return */
      for ( i = 0; i < classdim; i++) {
         imin[i] = gdsc_grid_c( iset, &iperm[i], &cwlo, &gerror );
         imax[i] = gdsc_grid_c( iset, &iperm[i], &cwup, &gerror );
      }
   }
   /*
    * Now we need the set which contains the antenna pattern(s).
    * If the user enters here less subsets than the number of input
    * subsets, the amaps array will be filled with the last subset
    * entered here.
    */
   {
      fint	class = CLASS;			/* class of application */
      fint	cwlo;				/* lower c.w. */
      fint	cwup;				/* upper c.w. */
      fint	i;				/* loop counter */
      fint	input_level = 0;		/* no default*/
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	output_level = 11;		/* output level */

      anmap = gdsinp_c( aset ,			/* antenna set name */
                        amaps ,			/* antenna subsets */
                        &inmap ,		/* maximum number of subsets */
                        &input_level ,		/* input level */
                        KEY_APSET ,		/* keyword */
                        MES_APSET ,		/* message */
                        &output_level ,		/* output level */
                        aperm ,			/* axes permutation */
                        asize ,			/* axes counter */
                        &maxaxes ,		/* maximum number of axis */
                        &class ,		/* class of program */
                        &classdim );		/* wanted dimension */
      for (; anmap < inmap; amaps[anmap] = amaps[anmap-1], anmap++);
      gdsc_range_c( aset ,			/* set name */
                    amaps ,			/* first subset */
                    &cwlo ,			/* lower c.w. */
                    &cwup ,			/* upper c.w. */
                    &gerror );			/* error return */
      for ( i = 0; i < classdim; i++) {
         amin[i] = gdsc_grid_c( aset, &aperm[i], &cwlo, &gerror );
         amax[i] = gdsc_grid_c( aset, &aperm[i], &cwup, &gerror );
         if ( amin[i] > 0 || amax[i] < 0 ) {
            char	mes[80];
            fint	error_level = 4;	/* FATAL error */

            sprintf( mes, "%.*s centre (0) not in Antenna Pattern", ictype[aperm[i]-1].l, ictype[aperm[i]-1].a );
            error_c( &error_level , tofchar( mes ) );
         }
      }
      /*
       * Check whether ap has undefined values at edges.
       */
      {
         float	*data;				/* pointer */
         int	l;

         l = amax[0] - amin[0] + 1;
         data = malloc( sizeof( float ) * l );
         readxy( aset, amaps[0], amin[0], 0, amax[0], 0, data );
         i = 0;
         while ( i < l && data[i] == blank ) i++;
         amin[0] += i;
         i = 0;
         while ( i < l && data[l-i-1] == blank ) i++;
         amax[0] -= i;
         free( data );
         if (classdim > 1) {
            l = amax[1] - amin[1] + 1;
            data = malloc( sizeof( float ) * l );
            readxy( aset, amaps[0], 0, amin[1], 0, amax[1], data );
            i = 0;
            while ( i < l && data[i] == blank ) i++;
            amin[1] += i;
            i = 0;
            while ( i < l && data[l-i-1] == blank ) i++;
            amax[1] -= i;
            free( data );
         }
      }
      for ( i = 0; i < classdim; i++) {
         if ( amin[i] > amax[i] ) {
            fint	error_level = 4;	/* FATAL error */

            error_c( &error_level, tofchar( "No defined points in Antenna Pattern" ) );
         } else if ( amin[i] > 0 || amax[i] < 0 ) {
            char	mes[80];
            fint	error_level = 4;	/* FATAL error */

            sprintf( mes, "%.*s centre (0) not in Antenna Pattern", ictype[aperm[i]-1].l, ictype[aperm[i]-1].a );
            error_c( &error_level , tofchar( mes ) );
         } else {
            alow[i] = amin[i];
            aupp[i] = amax[i];
            if ( -amin[i] > amax[i] ) amin[i] = -amax[i];
         }
      }
   }
   /*
    * Now we want to know where to put the cleaned maps. The component
    * tables will also be put here.
    */
   {
      fint	class = CLASS;			/* class of application */
      fint	cwlo;				/* lower c.w. */
      fint	cwup;				/* upper c.w. */
      fint	i;				/* loop counter */
      fint	input_level = 4;		/* no default, exact */
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	output_level = 11;		/* output level */

      gdsasn_c( KEY_INSET ,			/* input set key */
                KEY_OUTSET ,			/* output set key */
                &class );			/* class of application */
      onmap = gdsout_c( oset ,			/* output set name */
                        omaps ,			/* output subsets */
                        &inmap ,		/* maximum number of subsets */
                        &input_level ,		/* input level */
                        KEY_OUTSET ,		/* keyword */
                        MES_OUTSET ,		/* message */
                        &output_level ,		/* output level */
                        operm ,			/* axes permutation */
                        osize ,			/* axes counter */
                        &maxaxes );		/* maximum number of axis */
      gdsc_range_c( oset ,			/* set name */
                    omaps ,			/* first subset */
                    &cwlo ,			/* lower c.w. */
                    &cwup ,			/* upper c.w. */
                    &gerror );			/* error return */
      for ( i = 0; i < classdim; i++ ) {
         omin[i] = gdsc_grid_c( oset, &operm[i], &cwlo, &gerror );
         omax[i] = gdsc_grid_c( oset, &operm[i], &cwup, &gerror );
      }
   }
   /*
    * The search area can be defined by a set which is used as a mask:
    * NON-BLANK values mask the search area. The user can enter the
    * name of the mask (definition) set below. If no set name is given,
    * the search area is obtained via gdsbox.
    */
   {
      bool	okay;				/* loop control */
      fint	class = CLASS;			/* class of application */
      fint	cwlo;				/* lower c.w. */
      fint	cwup;				/* upper c.w. */
      fint	i;				/* loop counter */
      fint	input_level = 1;		/* default */
      fint	maxaxes = MAXAXES;		/* maximum number of axes */
      fint	output_level = 11;		/* output level */

      do {
         okay = 1;				/* reset */
         dnmap = gdsinp_c( dset ,		/* antenna set name */
                           dmaps ,		/* antenna subsets */
                           &inmap ,		/* maximum number of subsets */
                           &input_level ,	/* input level */
                           KEY_DEFSET ,		/* keyword */
                           MES_DEFSET ,		/* message */
                           &output_level ,	/* output level */
                           dperm ,		/* axes permutation */
                           dsize ,		/* axes counter */
                           &maxaxes ,		/* maximum number of axis */
                           &class ,		/* class of program */
                           &classdim );		/* wanted dimension */
         if (dnmap) {
            if (dnmap == 1) {
               for ( ; dnmap < inmap; dmaps[dnmap++] = dmaps[0] );
            } else if (dnmap != inmap) {
               fint	error_level = 1;

               okay = 0;
               error_c( &error_level, tofchar( "Unequal number of subsets!" ) );
               cancel_c( KEY_DEFSET );
            }
         }
      } while ( !okay );			/* loop control */
      if (dnmap) {				/* definition set entered */
         gdsc_range_c( dset ,			/* set name */
                       dmaps ,			/* first subset */
                       &cwlo ,			/* lower c.w. */
                       &cwup ,			/* upper c.w. */
                       &gerror );		/* error return */
         for ( i = 0; i < classdim; i++ ) {
            dmin[i] = gdsc_grid_c( dset, &dperm[i], &cwlo, &gerror );
            dmax[i] = gdsc_grid_c( dset, &dperm[i], &cwup, &gerror );
         }
      } else do {				/* we need a box */
         fint	blo[CLASSDIM];			/* lower grids */
         fint	bup[CLASSDIM];			/* upper grids */
         fint	input_level = 0;		/* no default */
         fint	output_level = 11;		/* output level */
         fint	option = 0;			/* restricted size */
         int	reject = 0;			/* input okay */

         gdsbox_c( blo ,			/* lower edge */
                   bup ,			/* upper edge */
                   iset ,			/* name input set */
                   imaps ,			/* first input subset */
                   &input_level ,		/* input level */
                   KEY_SEARCHBOX ,		/* the keyword */
                   MES_SEARCHBOX ,		/* the message */
                   &output_level ,		/* output device */
                   &option );			/* the box option */
         for ( i = 0; i < classdim; i++) {
            smin[i] = blo[i];			/* lower edge */
            smax[i] = bup[i];			/* upper edge */
            if ((smax[i] - smin[i] + 1) > (1 - amin[i])) {
               reject = 1;
            }
         }
         if (reject) {
            reject_c( KEY_SEARCHBOX, tofchar( "Searchbox too BIG!" ) );
         } else {				/* ok */
            break;				/* leave loop */
         }
      } while (1);				/* and on and on and on .. */
   }
   /*
    * Below we get the area to be cleaned from the user. The default is
    * the whole map, although this might be impossible.
    */
   {
      fint	blo[CLASSDIM];			/* lower grids */
      fint	bup[CLASSDIM];			/* upper grids */
      fint	i;				/* loop counter */
      fint	input_level = 1;		/* default */
      fint	output_level = 11;		/* output level */
      fint	option = 0;			/* default entire subset */

      gdsbox_c( blo ,				/* lower edge */
                bup ,				/* upper edge */
                iset ,				/* name input set */
                imaps ,				/* first input subset */
                &input_level ,			/* input level */
                KEY_CLEANBOX ,			/* the keyword */
                MES_CLEANBOX ,			/* the message */
                &output_level ,			/* output level */
                &option );			/* the box option */
      for ( i = 0; i < classdim; i++ ) {
         cmin[i] = blo[i];			/* lower edge */
         cmax[i] = bup[i];			/* upper edge */
      }
   }
   /*
    * Now we need to know some clean parameters. They are asked below.
    */
   {
      char	mes[80];			/* message buffer */
      fint	input_level;			/* input level */
      fint	ok = 1;				/* loop control */
      fint	one = 1;			/* just one */

      /*
       * Get the number of components to search and subtract.
       */
      do {					/* get number of components */
         input_level = 1;			/* default */
         nmax = 10000;				/* the default */
         userint_c( &nmax ,			/* the value */
                    &one ,			/* one value */
                    &input_level ,		/* input level */
                    KEY_NMAX ,			/* the keyword */
                    MES_NMAX );			/* the message */
         if (nmax < 0 || nmax > MAXCOMP) {	/* error */
            sprintf( mes, "Illegal number of components! Maximum is %d", MAXCOMP );
            reject_c( KEY_NMAX, tofchar( mes ) );	/* the message */
            ok = 0;				/* not o.k. */
         } else {				/* input o.k. */
            ok = 1;				/* o.k. */
         }
      } while (!ok);				/* end of loop */
      /*
       * Get the cutoff level when to stop searching.
       */
      do {					/* get cutoff */
         input_level = 1;			/* default */
         cutoff = 0.0;				/* the default */
         userreal_c( &cutoff ,			/* the value */
                     &one ,			/* one value */
                     &input_level ,		/* the input level */
                     KEY_CUTOFF ,		/* the keyword */
                     MES_CUTOFF );		/* the message */
         if (cutoff < 0.0) {			/* wrong input */
            sprintf( mes, "Cutoff cannot be negative!" );
            reject_c( KEY_CUTOFF, tofchar( mes ) );	/* the message */
            ok = 0;				/* not o.k. */
         } else {				/* input o.k. */
            ok = 1;				/* o.k. */
         }
      } while (!ok);				/* end of loop */
      /*
       * Get the gain factor.
       */
      do {					/* get gain */
         if (ok) {
            input_level = 2;			/* hidden */
         } else {
            input_level = 1;			/* not hidden */
         }
         gain = 0.7;				/* the default */
         userreal_c( &gain ,			/* the value */
                     &one ,			/* one value */
                     &input_level ,		/* the input level */
                     KEY_GAIN ,			/* the keyword */
                     MES_GAIN );		/* the message */
         if (gain < 0.0) {			/* wrong input */
            sprintf( mes, "Gain cannot be negative!" );
            reject_c( KEY_GAIN, tofchar( mes ) );	/* the message */
            ok = 0;				/* not o.k. */
         } else {				/* input o.k. */
            ok = 1;				/* o.k. */
         }
      } while (!ok);				/* end of loop */
      /*
       * Get factor.
       */
      do {					/* get factor */
         if (ok) {
            input_level = 2;			/* hidden */
         } else {
            input_level = 1;			/* not hidden */
         }
         factor = 1.0;				/* the default */
         userreal_c( &factor ,			/* the value */
                     &one ,			/* one value */
                     &input_level ,		/* the input level */
                     KEY_FACTOR ,		/* the keyword */
                     MES_FACTOR );		/* the message */
         if (factor < 0.0) {			/* wrong input */
            sprintf( mes, "Factor cannot be negative!" );
            reject_c( KEY_FACTOR, tofchar( mes ) );	/* the message */
            ok = 0;				/* not o.k. */
         } else {				/* input o.k. */
            ok = 1;				/* o.k. */
         }
      } while (!ok);				/* end of loop */
      /*
       * Get number of positive components.
       */
      do {					/* get npos */
         if (ok) {
            input_level = 2;			/* hidden */
         } else {
            input_level = 1;			/* not hidden */
         }
         npos = 0;				/* the default */
         userint_c( &npos ,			/* the value */
                    &one ,			/* one value */
                    &input_level ,		/* the input level */
                    KEY_NPOS ,			/* the keyword */
                    MES_NPOS );			/* the message */
         if (npos < 0) {			/* wrong input */
            sprintf( mes, "Npos cannot be negative!" );
            reject_c( KEY_NPOS, tofchar( mes ) );	/* the message */
            ok = 0;				/* not o.k. */
         } else {				/* input o.k. */
            ok = 1;				/* o.k. */
         }
      } while (!ok);				/* end of loop */
      /*
       * Get positive cutoff.
       */
      do {					/* get positive cutpos */
         if (ok) {
            input_level = 2;			/* hidden */
         } else {
            input_level = 1;			/* not hidden */
         }
         cutpos = 0.0;				/* the default */
         userreal_c( &cutpos ,			/* the value */
                     &one ,			/* one value */
                     &input_level ,		/* the input level */
                     KEY_CUTPOS ,		/* the keyword */
                     MES_CUTPOS );		/* the message */
         if (cutpos < 0.0) {			/* wrong input */
            sprintf( mes, "Cutpos cannot be negative!" );
            reject_c( KEY_CUTPOS, tofchar( mes ) );	/* the message */
            ok = 0;				/* not o.k. */
         } else {				/* input o.k. */
            ok = 1;				/* o.k. */
         }
      } while (!ok);				/* end of loop */
      /*
       * Get cutoff for optimization.
       */
      do {
         if (ok) {
            input_level = 2;			/* hidden */
         } else {
            input_level = 1;			/* not hidden */
         }
         cutopt = 0.0;				/* the default */
         userreal_c( &cutopt ,			/* the value */
                     &one ,			/* one value */
                     &input_level ,		/* the input level */
                     KEY_CUTOPT ,		/* the keyword */
                     MES_CUTOPT );		/* the message */
         if (cutopt != 0.0 && cutopt <= cutoff) {	/* wrong input */
            sprintf( mes, "Cutopt cannot be less than CUTOFF!" );
            reject_c( KEY_CUTOPT, tofchar( mes ) );	/* the message */
            ok = 0;				/* not o.k. */
         } else {				/* input o.k. */
            ok = 1;				/* o.k. */
         }
      } while (!ok);				/* end of loop */
      /*
       * Continuation of a previous run ?
       */
      input_level = 2;				/* hidden */
      cont = FALSE;				/* default value */
      (void) userlog_c( &cont ,			/* the value */
                        &one ,			/* one value */
                        &input_level ,		/* input level */
                        KEY_CONTINUE ,		/* the keyword */
                        MES_CONTINUE );		/* the message */
      cont = tobool( cont );			/* to C logical */
      /*
       * Shall we produce test output ?
       */
      input_level = 2;				/* hidden */
      test = FALSE;				/* default value */
      (void) userlog_c( &test ,			/* the value */
                        &one ,			/* one value */
                        &input_level ,		/* input level */
                        KEY_TEST ,		/* the keyword */
                        MES_TEST );		/* the message */
      test = tobool( test );			/* to C logical */
   }
   /*
    * Before starting the CLEAN, we output some parameters for the user.
    */
   {
      char	string[MAXSTRINGLEN];		/* text buffer */
      fint	output_level = 3;		/* to screen and logfile */

      sprintf( string, "Maximum # of components    : %20d", nmax );
      anyout_c( &output_level, tofchar( string ) );
      sprintf( string, "Cutoff level               : %20f", cutoff );
      anyout_c( &output_level, tofchar( string ) );
      sprintf( string, "Gain factor                : %20f", gain );
      anyout_c( &output_level, tofchar( string ) );
      sprintf( string, "Factor                     : %20f", factor );
      anyout_c( &output_level, tofchar( string ) );
      sprintf( string, "Minimum # of pos. comps.   : %20d", npos );
      anyout_c( &output_level, tofchar( string ) );
      if (cutpos > 0.0) {			/* positive cutoff ? */
         sprintf( string, "Cutoff for pos. comps.     : %20f", cutpos );
         anyout_c( &output_level, tofchar( string ) );
      }
      if (cutopt > 0.0) {			/* optimization ? */
         sprintf( string, "Cutoff for optimization    : %20f", cutopt );
         anyout_c( &output_level, tofchar( string ) );
      }
   }
   /*
    * We loop over all input maps. But before doing that, we fill in
    * the second range (a dummy range) for the one dimensional clean.
    */
   {
      fint	n;

      for ( n = classdim; n < CLASSDIM; n++) {
         msize[n] = 1;
         imax[n] = amax[n] = dmax[n] = smax[n] = cmax[n] = 0;
         imin[n] = amin[n] = dmin[n] = smin[n] = cmin[n] = 0;
      }
   }
   for (nmap = 0; nmap < inmap; nmap++) {	/* the big loop */
      double	ct;				/* cpu time counter */
      double	rt;				/* real time counter */
      fint	asize[CLASSDIM];		/* x size antenna pattern */
      fint	cllo = 0;			/* edges of clean area */
      fint	clup = 0;			/*  ..    .   ..   ..  */
      fint	cmlo = 0;			/*  ..    .   ..   ..  */
      fint	cmup = 0;			/*  ..    .   ..   ..  */
      fint	ia;				/* centre antenna pattern */
      fint	ccnt = 0, scnt = 0;		/* counts pixels */
      fint	ncc = 0;			/* number of components done */
      fint	nccopt = 0;			/* optimization */
      fint	ndum = 0;			/* position dummy component */
      fint	okay = 1;			/* problems ? */
      fint	pbcopt = 0;			/* primary beam correction */
      float	cs1 = 0.0, cs2 = 0.0;
      float	ss1 = 0.0, ss2 = 0.0;
      float	scoff;				/* offset parameter */
      float	sumapt = 0.0;			/* integrated apt */
      float	summap = 0.0;			/* integrated map */
      float	pbcmap = 0.0;			/* integrated map pbc */
      float	sumcmp = 0.0;			/* integrated components */
      float	pbccmp = 0.0;			/* integrated components pbc */
      float	sumres = 0.0;			/* integrated residual */
      float	pbcres = 0.0;			/* integrated residual pbc */

      /*
       * Initialize the timers.
       */
      {
         fint	mode = 0;			/* reset mode */

         timer_c( &ct, &rt, &mode );		/* set timers */
      }
      /*
       * Get the units of the data.
       */
      {
         fint	derror = 0;			/* GDS error return */

         gdsd_rchar_c( iset ,			/* input set name */
                       tofchar( "BUNIT" ) ,	/* FITS item */
                       &imaps[nmap] ,		/* input subset level */
                       bunit ,			/* the value */
                       &derror );		/* GDS return code */
         if (derror < 0) {			/* not found */
            fint	l;			/* loop counter */

            for (l = 0; l < MAXFITSCHARLEN; bunit.a[l++] = ' ');
         }
      }
      /*
       * First we copy the dirty map to the output set.
       */
      if (strncmp( iset.a, oset.a, iset.l ) || imaps[nmap] != omaps[nmap]) {
         fint	cwlo1, cwup1;			/* c.w. input */
         fint	cwlo2, cwup2;			/* c.w. output */
         fint	nbuff = MAXDATA;		/* size of buffer */
         fint	ndone;				/* pixels done */
         fint	tid1 = 0;			/* transfer id input */
         fint	tid2 = 0;			/* transfer id output */
         float	data[MAXDATA];			/* copy buffer */

         status_c( tofchar( "Copying data" ) );	/* message for user */
         gdsc_range_c( iset ,			/* set name */
                       &imaps[nmap] ,		/* first subset */
                       &cwlo1 ,			/* lower c.w. */
                       &cwup1 ,			/* upper c.w. */
                       &gerror );		/* error return */
         gdsc_range_c( oset ,			/* set name */
                       &omaps[nmap] ,		/* first subset */
                       &cwlo2 ,			/* lower c.w. */
                       &cwup2 ,			/* upper c.w. */
                       &gerror );		/* error return */
         do {					/* copy loop */
            gdsi_read_c( iset, &cwlo1, &cwup1, data, &nbuff, &ndone, &tid1 );
            gdsi_write_c( oset, &cwlo2, &cwup2, data, &ndone, &ndone, &tid2 );
         } while (tid1 && tid2);
      }
      /*
       * If the search area is defined in another map, get the smallest
       * box which includes the search area.
       */
      if (dnmap) {				/* definition maps */
         fint	i;				/* loop counter */
         fint	lsize;				/* number of x pixels */
         fint	m1, m2;				/* lower and upper line */
         fint	maxdata;			/* size of data buffer */
         fint	mstep;				/* number of lines */
         fint	ndef = 0;			/* number of non blanks */
         float	*data;				/* data buffer */

         status_c( tofchar( "Defining Search Area" ) );
         for ( i = 0; i < classdim; i++) {
            smin[i] = dmax[i];			/* initial guess */
            smax[i] = dmin[i];			/* initial guess */
         }
         lsize = dmax[0] - dmin[0] + 1;	/* number of x grids */
         maxdata = IMAX( MAXDATA, lsize );	/* for at least one line */
         mstep = maxdata / lsize;		/* number of lines */
         data = calloc( maxdata, sizeof( float ) );
         m1 = dmin[1];				/* first line */
         do {					/* definition loop */
            fint	i, l, m;		/* counters */

            m2 = IMIN( dmax[1], m1 + mstep - 1 );	/* last line */
            readxy( dset ,			/* the set name */
                    dmaps[nmap] ,		/* the subset level */
                    dmin[0] ,			/* first x grid */
                    m1 ,			/* first y grid */
                    dmax[0] ,			/* last x grid */
                    m2 ,			/* last y grid */
                    data );			/* the data */
            for (i = 0, m = m1; m <= m2; m++) {	/* loop over lines */
               for (l = dmin[0]; l <= dmax[0]; l++) {
                  if (data[i++] != blank) {	/* a datum */
                     ndef += 1;			/* increase counter */
                     smin[0] = IMIN( smin[0], l );	/* new lower x */
                     smin[1] = IMIN( smin[1], m );	/* new lower y */
                     smax[0] = IMAX( smax[0], l );	/* new upper x */
                     smax[1] = IMAX( smax[1], m );	/* new upper y */
                  }
               }
            }
            m1 = m2 + 1;			/* next block */
         } while (m1 < dmax[1]);		/* until all line done */
         free( data );				/* free memory */
         if (!ndef) {				/* no search area defined */
            fint	error_level = 1;	/* warning */

            error_c( &error_level ,		/* the error level */
                     tofchar( "No search area defined!" ) );
            okay = 0;				/* problems */
         } else {				/* check with input maps */
            fint	outside = 0;		/* outside input map ? */

            for ( i = 0; i < classdim; i++) {
               if (smin[i] < imin[i]) {		/* outside input map */
                  smin[i] = imin[i]; outside = 1;	/* problems */
               }
               if (smax[i] > imax[i]) {		/* outside input map */
                  smax[i] = imax[i]; outside = 1;	/* problems */
               }
               if (smin[i] > smax[i]) {
                  outside = 1; okay = 0;	/* problems */
               }
            }
            if (outside && !okay) {
               char	string[MAXSTRINGLEN];	/* message string */
               fint	error_level = 1;	/* warning */

               sprintf( string, "Search area outside %s!", dim[classdim-1] );
               error_c( &error_level , tofchar( string ) );
            } else if (outside) {		/* partly outside */
               char	string[MAXSTRINGLEN];	/* message string */
               fint	error_level = 1;	/* warning */

               sprintf( string, "Search area partly outside %s!", dim[classdim-1] );
               error_c( &error_level , tofchar( string ) );
            }
            if (okay) {
               fint	error_level = 1;	/* warning */
               int	reject = 0;		/* reject ? */

               for ( i = 0; i < classdim; i++) {
                  if ((smax[i] - smin[i] + 1) > (1 - amin[i])) {
                     reject = 1;
                  }
               }
               if (reject) {
                  okay = 0;
                  error_c( &error_level, tofchar( "Search area too BIG!" ) );
               }
            }
         }
      }
      /*
       * In case of test mode, output the search area as found above.
       */
      if (test) {
         char	string[MAXSTRINGLEN];		/* text buffer */
         fint	output_level = 3;		/* to screen and log file */

         switch( classdim ) {
            case 1: {
               sprintf( string, "SEARCHBOX= %10d %10d", smin[0], smax[0] );
               break;
            }
            case 2: {
               sprintf( string, "SEARCHBOX= %10d %10d %10d %10d", smin[0], smin[1], smax[0], smax[1] );
               break;
            }
            default: {
               break;
            }
         }
         anyout_c( &output_level, tofchar( string ) );
      }
      /*
       * Next we try to define the minimum clean area. This can only be
       * done when the search area is entirely inside the clean area.
       * The second step is to allocate the memory for the dirty map
       * and dirty antenna pattern. If this does not succeed we will
       * try the smallest area.
       */
      if (okay) {				/* go on */
         fint	i;				/* loop counter */

         for ( i = 0; i < CLASSDIM; i++) {
            if (cmax[i] < smax[i] || cmin[i] > smin[i]) {
               mmin[i] = smin[i];
               mmax[i] = smax[i];
            } else {
               mmin[i] = IMAX( cmin[i], smax[i] + amin[i] );
               mmax[i] = IMIN( cmax[i], smin[i] - amin[i] );
            }
            msize[i] = mmax[i] - mmin[i] + 1;
         }
         {
            fint	allo = mmin[0] - smax[0];
            fint	alup = mmax[0] - smin[0];
            fint	amlo = mmin[1] - smax[1];
            fint	amup = mmax[1] - smin[1];

            asize[0] = 2 * IMAX( -allo, alup ) + 1;
            asize[1] = IMAX( -amlo, amup ) + 1;
         }
         mapsize = msize[0] * msize[1];
         aptsize = asize[0] * asize[1];
         map = calloc( mapsize, sizeof( float ) );
         apt = calloc( aptsize, sizeof( float ) );
         if (apt == NULL || map == NULL) {
            if (apt == NULL) free( apt );
            if (map == NULL) free( map );
            apt = NULL; map = NULL;
            mmin[0] = smin[0]; mmin[1] = smin[1];	/* lower edge */
            mmax[0] = smax[0]; mmax[1] = smax[1];	/* upper edge */
            msize[0] = mmax[0] - mmin[0] + 1;
            msize[1] = mmax[1] - mmin[1] + 1;
            asize[0] = 2 * msize[0] - 1;
            asize[1] = msize[1];
            mapsize = msize[0] * msize[1];
            aptsize = asize[0] * asize[1];
            map = calloc( mapsize, sizeof( float ) );
            apt = calloc( aptsize, sizeof( float ) );
            if (apt == NULL || map == NULL) {
               fint	error_level = 2;	/* more severe */

               if (apt == NULL) free( apt );
               if (map == NULL) free( map );
               apt = NULL; map = NULL;
               error_c( &error_level ,	/* the error level */
                        tofchar( "Cannot allocate enough memory!" ) );
               okay = 0;
            }
         }
      }
      /*
       * For testing, we output the core box.
       */
      if (test) {
         char	string[MAXSTRINGLEN];		/* text buffer */
         fint	output_level = 3;		/* to screen and logfile */

         switch( classdim ) {
            case 1: {
               sprintf( string, "COREBOX=   %10d %10d", mmin[0], mmax[0] );
               break;
            }
            case 2: {
               sprintf( string, "COREBOX=   %10d %10d %10d %10d", mmin[0], mmin[1], mmax[0], mmax[1] );
               break;
            }
            default: {
               break;
            }
         }
         anyout_c( &output_level, tofchar( string ) );
         sprintf( string, "APTSIZE=   %10d", aptsize );
         anyout_c( &output_level, tofchar( string ) );
         sprintf( string, "MAPSIZE=   %10d", mapsize );
         anyout_c( &output_level, tofchar( string ) );
      }
      /*
       * Now we integrate the antenna pattern over the minimum search area.
       */
      if (okay) {
         fint	allo, alup, amlo, amup;		/* apt limits */
         fint	n;				/* loop counter */
         fint	nl = smax[0] - smin[0] + 1;	/* l size */
         fint	nm = smax[1] - smin[1] + 1;	/* m size */
         fint	np;				/* total number of points */

         allo = ( 1 - nl ) / 2;			/* lower x of apt */
         alup = nl + allo - 1;			/* upper x of apt */
         amlo = ( 1 - nm ) / 2;			/* lower y of apt */
         amup = nm + amlo - 1;			/* upper y of apt */
         /*
          * load apt data in map array. It will always fit in.
          */
         readxy( aset ,				/* antenna set name */
                 amaps[nmap] ,			/* antenna subset level */
                 allo ,				/* lower x */
                 amlo ,				/* lower y */
                 alup ,				/* upper x */
                 amup ,				/* upper y */
                 map );				/* data */
         np = nl * nm;				/* number of points */
         for (n = 0; n < np; sumapt += map[n++]);
      }
      /*
       * Now we initialize the primary beam correction routine.
       */
      if (okay) {
         fint	dstat = 0;			/* new list status */

         dstat = listctrl_c( &dstat );		/* get and set list status */
         (void) pribeam_c( iset ,		/* name of input set */
                           &imaps[nmap] ,	/* input subset level */
                           iperm ,		/* permutation array */
                           NULL ,		/* dummy */
                           NULL ,		/* dummy */
                           &pbcopt ,		/* initialize */
                           errmes );		/* error message */
         dstat = listctrl_c( &dstat );		/* get and set list status */
         if (pbcopt > 0) {			/* no error */
            pbcopt = 3;				/* go on till infinity */
         } else {				/* an error */
            fint	error_level = 1;	/* warning */

            error_c( &error_level, errmes );	/* show error message */
            pbcopt = 0;				/* no pbc */
         }
      }
      /*
       * Next we load in the dirty data. If a definition set was entered,
       * it will be read into a local buffer.
       */
      if (okay) {
         char	string[MAXSTRINGLEN];		/* message buffer */
         fint	i, l, m;
         fint	nblank;
         float	datamin, datamax;

         sprintf( string, "Loading Dirty %s", dim[classdim-1] );
         status_c( tofchar( string ) );
         readxy( iset ,				/* name input set */
                 imaps[nmap] ,			/* the subset */
                 mmin[0] ,			/* lower x grid */
                 mmin[1] ,			/* lower y grid */
                 mmax[0] ,			/* upper x grid */
                 mmax[1] ,			/* upper y grid */
                 map );				/* receives the data */
         /*
          * Now we integrate the dirty map within the minimum search area.
          */
         for (i = 0, m = mmin[1]; m <= mmax[1]; m++) {
            for (l = mmin[0]; l <= mmax[0]; l++, i++) {
               if (m < smin[1] || m > smax[1] || l < smin[0] || l > smax[0]) {
               } else {
                  summap += map[i];
                  if (pbcopt) {
                     float fm, fl;
                     fl = (float)l, fm = (float)m;
                     pbcmap += ( map[i] / pribeam_c( iset , &imaps[nmap], iperm, &fl, &fm, &pbcopt, errmes ) );
                  }
               }
            }
         }
         minmax2_c( map, &mapsize, &datamin, &datamax, &nblank );
         if (nblank != 0) {
            char	string[MAXSTRINGLEN];	/* message buffer */
            fint	error_level = 1;	/* the error level */

            sprintf( string, "Cannot CLEAN because of %d BLANK(s) in %s", nblank, dim[classdim-1] );
            error_c( &error_level , tofchar( string ) );
            okay = 0;
         } else if (datamax == datamin) {
            fint	error_level = 1;	/* the error level */

            error_c( &error_level ,		/* the error level */
                     tofchar( "Cannot CLEAN because DATAMIN = DATAMAX!" ) );
            okay = 0;
         } else {
            scoff = 4.0 * ( datamax - datamin );/* the offset */
            {
               int	n, np;

               np = msize[0] * msize[1];
               for ( n = 0; n < np; map[n++] -= scoff );
            }
            if (dnmap) {			/* mask from map */
               fint	cwlo, cwup;		/* c.w.'s of frame */
               fint	i = 0, j;		/* data counter */
               fint	nbuf = MAXDATA;		/* size of data buffer */
               fint	ndone;			/* number of pixels read */
               fint	tid = 0;		/* transfer id */
               float	data[MAXDATA];		/* data buffer */

               cwlo = gdsc_fill_c( dset ,	/* definition set name */
                                   &dmaps[nmap] ,	/* definition subset level */
                                   smin );	/* lower edge of frame */
               cwup = gdsc_fill_c( dset ,	/* definition set name */
                                   &dmaps[nmap] ,	/* definition subset level */
                                   smax );	/* upper edge of frame */
               j = ( smin[1] - mmin[1] ) * ( mmax[0] - mmin[0] + 1 ) +
                  smin[0] - mmin[0];
               do {				/* definition loop */
                  fint	n;			/* loop counter */

                  gdsi_read_c( dset ,		/* definition set name */
                               &cwlo ,		/* lower c.w. of frame */
                               &cwup ,		/* upper c.w. of frame */
                               data ,		/* data buffer */
                               &nbuf ,		/* size of buffer */
                               &ndone ,		/* pixels done */
                               &tid );		/* transfer id */
                  for (n = 0; n < ndone; n++) {	/* inner loop */
                     if (data[n] != blank) {	/* search here */
                        ndum = j;		/* dummy component */
                        map[j] += 2 * scoff;	/* make search area */
                     }
                     i += 1;
                     if ( i % ( smax[0] - smin[0] + 1 ) ) {
                        j += 1;
                     } else {
                        j += mmax[0] - smax[0] + smin[0] - mmin[0] + 1;
                     }
                  }
               } while (tid);			/* until all done */
            } else {
               for (i = 0, m = mmin[1]; m <= mmax[1]; m++) {
                  for (l = mmin[0]; l <= mmax[0]; l++) {
                     if (m < smin[1] || m > smax[1] || l < smin[0] || l > smax[0]) {
                        i += 1;
                     } else {
                        ndum = i;		/* dummy component */
                        map[i++] += 2 * scoff;
                     }
                  }
               }
            }
         }
         if (okay) {
            fint	count = 0;
            fint	i;

            status_c( tofchar( "Loading Dirty Antenna Pattern" ) );
            readxy( aset ,			/* name input set */
                    amaps[nmap] ,		/* the subset */
                    ( 1 - asize[0] ) / 2,	/* lower x grid */
                    ( 1 - asize[1] ),		/* lower y grid */
                    ( asize[0] - 1 ) / 2,	/* upper x grid */
                    0 ,				/* upper y grid */
                    apt );			/* receives the data */
            ia = ( asize[1] - 1 ) * asize[0] + ( asize[0] - 1 ) / 2 + 1;
            if (fabs( 1.0 - apt[ia-1] ) > 100.0 * FLT_EPSILON) {
               char	string[MAXSTRINGLEN];	/* text buffer */
               fint	error_level = 2;	/* severe warning */

               sprintf( string, "Peak antenna pattern (%f) <> 1.0", apt[ia-1] );
               error_c( &error_level, tofchar( string ) );
            }
            for ( i = 0; i < asize[0] * asize[1]; i++) {
               if (apt[i] == blank) {
                  apt[i] = 0.0; count++;
               }
            }
            if (count) {
               fint	error_level = 1;

               error_c( &error_level,
                        tofchar( "Found BLANKS in antenna pattern (will use zero)!" ) );
            }
         }
      }
      /*
       * In test mode, put out the offset.
       */
      if (test && okay) {
         char	string[MAXSTRINGLEN];		/* text buffer */
         fint	output_level = 3;		/* to screen and logfile */

         sprintf( string, "SCOFF=     %10f", scoff );
         anyout_c( &output_level, tofchar( string ) );
      }
      /*
       * Here we do the actual cleaning. We make use of a special purpose
       * routine qcln1. This routine does the search and subtract in one call.
       * It can tell the difference between search and clean area by looking
       * at the amplitude at a certain position. If this amplitude is greater
       * than zero then this point belongs to the search area, else to the
       * clean area. In the part above we defined the search area.
       */
      if (okay) {
         bool	quit = FALSE;			/* user can change this */
         fint	icmax;				/* position of new maximum */
         fint	icmin;				/* position of new minimum */
         fint	i, l, m;			/* loop counters */
         fint	ncmp;				/* position of component */
         fint	npoints;			/* number of points in dirty map */
         fint	optimize = 0;			/* optimization ? */
         float	acmp;				/* amplitude of component */
         float	datamax;			/* new maximum of dirty map */
         float	datamin;			/* new minimum of dirty map */
         float	g = gain;			/* the gain factor */
         float	mcutpos;			/* modified positive cutoff */
         float	mcut;				/* modified cutoff */
         float	pmax;
         time_t	t1, t2;				/* timers */

         status_c( tofchar( "Searching and Subtracting" ) );
         t1 = time( NULL );			/* start time */
         npoints = msize[0] * msize[1];		/* number of points in dirty map */
         mcut = 10.0 * FLT_EPSILON * scoff;	/* this is real zero */
         if (mcut < cutoff) mcut = cutoff;	/* modified cutoff */
         acmp = 0.0;				/* amplitude dummy component */
         ncmp = ndum;				/* position dummy component */
         qcln1_c( map ,				/* the dirty data */
                  apt ,				/* the antenna pattern */
                  &msize[0] ,			/* number of x grids */
                  &msize[1] ,			/* number of y grids */
                  &asize[0] ,			/* number of x grids */
                  &ia ,				/* here's the centre */
                  &scoff ,			/* offset parameter */
                  &acmp ,			/* amplitude of component */
                  &ncmp ,			/* position of component */
                  &datamax ,			/* maximum after subtraction */
                  &icmax ,			/* position maximum */
                  &datamin ,			/* minimum after subtraction */
                  &icmin );			/* position minimum */
         if (cutpos == 0.0) {			/* no positive cutoff */
            mcutpos = datamax + scoff;		/* make large enough */
         } else {				/* a positive cutoff */
            mcutpos = cutpos;			/* set to cutoff value */
         }
         pmax = factor * datamax;			/* set pmax value */
         if (datamax > -datamin || datamax > pmax || datamax > mcutpos || npos > 0) {
            acmp = g * datamax; ncmp = icmax;	/* first component is positive */
         } else {
            acmp = g * datamin; ncmp = icmin;	/* first component is negative */
         }
         if (cutopt != 0.0) {			/* optimization is wanted */
            optimize = 1;
            mcut = 10.0 * FLT_EPSILON * scoff;	/* this is real zero */
            if (mcut < cutopt) mcut = cutopt;	/* modified cutoff */
         }
         while (ncc < nmax) {			/* loop until nmax components */
            fint	lc, mc;			/* relative component grids */
            fint	x, y;			/* real component grids */

            mc = (ncmp - 1) / msize[0] + 1;	/* relative y grid component */
            lc = ncmp - (mc - 1) * msize[0];	/* relative x grid component */
            x = lc + mmin[0] - 1;		/* real x grid position */
            y = mc + mmin[1] - 1;		/* real y grid position */
            /*
             * If somehow we find a component outside the valid area,
             * we quit with a message.
             */
            if (x < smin[0] || x > smax[0] || y < smin[1] || y > smax[1]) {
               char	string[MAXSTRINGLEN];
               fint	output_level = 3;

               sprintf( string, "Inconsistency in algorithm" );
               anyout_c( &output_level, tofchar( string ) );
               sprintf( string, "Position  %12d, icmin %14d, icmax %14d", ncmp, icmin, icmax );
               anyout_c( &output_level, tofchar( string ) );
               sprintf( string, "Amplitude %12e, datamin %12e, datamax %12e", acmp, datamin, datamax );
               anyout_c( &output_level, tofchar( string ) );
               break;
            }
            sumcmp += acmp;			/* component flux */
            amps[ncc] = acmp;			/* save amplitude */
            xpos[ncc] = x;			/* save real x grid */
            ypos[ncc] = y;			/* save real y grid */
            if (pbcopt) {
               float fx, fy;
               fx = (float) x, fy = (float) y;
               pbccmp += ( acmp / pribeam_c( iset, &imaps[nmap], iperm, &fx, &fy, &pbcopt, errmes ) );
            }
            ncc += 1;				/* increase number of components */
            qcln1_c( map ,			/* the dirty data */
                     apt ,			/* the antenna pattern */
                     &msize[0] ,		/* number of x grids */
                     &msize[1] ,		/* number of y grids */
                     &asize[0] ,		/* number of x grids */
                     &ia ,			/* here's the centre */
                     &scoff ,			/* offset parameter */
                     &acmp ,			/* amplitude of component */
                     &ncmp ,			/* position of component */
                     &datamax ,			/* maximum after subtraction */
                     &icmax ,			/* position maximum */
                     &datamin ,			/* minimum after subtraction */
                     &icmin );			/* position minimum */
            t2 = time( NULL );			/* second timer */
            if (((int) t2 - (int) t1) > 4) {	/* 5 seconds elapsed */
               char	string[MAXSTRINGLEN];	/* text buffer */
               fint	input_level = 2;	/* hidden keyword */
               fint	ninp;			/* number of items */
               fint	one = 1;		/* just one */

               sprintf( string, "Found and subtracted %d components (%7.2f%%)", ncc, sumcmp / summap * sumapt * 100.0 );
               status_c( tofchar( string ) );	/* message to user */
               t1 = t2;				/* reset first timer */
               ninp = userlog_c( &quit ,	/* the value */
                                 &one ,		/* one value */
                                 &input_level ,	/* the input level */
                                 KEY_STOP ,	/* the keyword */
                                 MES_STOP );	/* the message */
               cancel_c( KEY_STOP );		/* cancel keyword */
               if (tobool( quit )) break;	/* user wants to stop */
            }
            if ((datamax < mcut && -datamin < mcut) || (datamax < mcutpos && npos != 0)) {
               if (optimize) {			/* do not quit yet */
                  optimize = 0;			/* turn it off */
                  mcut = 10.0 * FLT_EPSILON * scoff;	/* this is real zero */
                  if (mcut < cutoff) mcut = cutoff;	/* modified cutoff */
                  g = 1.0;				/* maximum gain */
                  for (i = 0; i < npoints; i++) {	/* make all clean area */
                     if (map[i] >= 0.0) {		/* was search area */
                        map[i] -= ( 2.0 * scoff );	/* make clean area */
                     }
                  }
                  for (i = 0; i < ncc; i++) {		/* loop over components */
                     fint	n;

                     n = ( ypos[i] - mmin[1] ) * msize[0] + ( xpos[i] - mmin[0] );
                     if (map[n] < 0.0) {		/* was clean area */
                        map[n] += ( 2.0 * scoff );	/* make search area */
                     }
                  }
                  nccopt = ncc;				/* remember this */
               } else {
                  break;				/* we quit */
               }
            } else if (datamax > -datamin || datamax > pmax || datamax > mcutpos || ncc < npos) {
               acmp = g * datamax; ncmp = icmax;	/* next component is positive */
            } else {
               acmp = g * datamin; ncmp = icmin;	/* next component is negative */
            }
         }
         /*
          * Now we integrate the residual within the minimum search area.
          */
         for (i = 0, m = mmin[1]; m <= mmax[1]; m++) {
            for (l = mmin[0]; l <= mmax[0]; l++, i++) {
               if (map[i] >= 0.0) {		/* was search area */
                  map[i] -= scoff;		/* substract offset */
                  scnt += 1;			/* increase */
                  ss1 += map[i];
                  ss2 += ( map[i] * map[i] );
               } else {				/* was clean area */
                  map[i] += scoff;		/* add offset */
                  ccnt += 1;			/* increase */
                  cs1 += map[i];
                  cs2 += ( map[i] * map[i] );
               }
               if (m < smin[1] || m > smax[1] || l < smin[0] || l > smax[0]) {
               } else {
                  sumres += map[i];
                  if (pbcopt) {
                     float fm, fl;
                     fl = (float)l, fm = (float)m;                     
                     pbcres += ( map[i] / pribeam_c( iset, &imaps[nmap], iperm, &fl, &fm, &pbcopt, errmes ) );
                  }
               }
            }
         }
         writxy( oset ,				/* name of output set */
                 omaps[nmap] ,			/* level of output subset */
                 mmin[0] ,			/* lower x grid */
                 mmin[1] ,			/* lower y grid */
                 mmax[0] ,			/* upper x grid */
                 mmax[1] ,			/* upper y grid */
                 map );				/* the cleaned data */
      }
      /*
       * Now we have found the components, we will have to subtract them
       * from the clean area outside the search area.
       */
      if (okay) {
         fint	l;				/* loop counter */
         fint	sllo, slup, smlo, smup;		/* real search area */
         fint	nls, nms;
         float	dd, dt;				/* progress counters */

         sllo = slup = xpos[0];			/* start value */
         smlo = smup = ypos[0];			/* start value */
         for (l = 0; l < ncc; l++) {
            sllo = IMIN( sllo, xpos[l] );	/* new min x */
            smlo = IMIN( smlo, ypos[l] );	/* new min y */
            slup = IMAX( slup, xpos[l] );	/* new max x */
            smup = IMAX( smup, ypos[l] );	/* new max y */
         }
         /*
          * In test mode, put out the modified searchbox.
          */
         if (test) {
            char	string[MAXSTRINGLEN];	/* text buffer */
            fint	output_level = 3;	/* to screen and logfile */

            switch( classdim ) {
               case 1: {
                  sprintf( string, "SEARCHBOX= %10d %10d", sllo, slup );
                  break;
               }
               case 2: {
                  sprintf( string, "SEARCHBOX= %10d %10d %10d %10d", sllo, smlo, slup, smup );
                  break;
               }
               default: {
                  break;
               }
            }
            anyout_c( &output_level, tofchar( string ) );
         }
         cllo = IMAX( cmin[0], alow[0] + slup );	/* lower x of clean area */
         clup = IMIN( cmax[0], aupp[0] + sllo );	/* upper x of clean area */
         cmlo = IMAX( cmin[1], alow[1] + smup );	/* lower y of clean area */
         cmup = IMIN( cmax[1], aupp[1] + smlo );	/* upper y of clean area */
         /*
          * In test mode, put out the modified cleanbox.
          */
         if (test) {
            char	string[MAXSTRINGLEN];	/* text buffer */
            fint	output_level = 3;	/* to screen and logfile */

            switch( classdim ) {
               case 1: {
                  sprintf( string, "CLEANBOX=  %10d %10d", cllo, clup );
                  break;
               }
               case 2: {
                  sprintf( string, "CLEANBOX=  %10d %10d %10d %10d", cllo, cmlo, clup, cmup );
                  break;
               }
               default: {
                  break;
               }
            }
            anyout_c( &output_level, tofchar( string ) );
         }
         nls = slup - sllo + 1;			/* x size of clean area */
         nms = smup - smlo + 1;			/* y size of clean area */
         dd = msize[0] * msize[1];		/* number of points already cleaned */
                                         	/* search area outside clean area */
         if (cllo > mmax[0] || clup < mmin[0] || cmlo > mmax[1] || cmup < mmin[1]) {
            dt = dd + ( clup - cllo + 1 ) * ( cmup - cmlo + 1 );
         } else {
            dt = dd;				/* number of points done */
            if (cmlo < mmin[1]) {		/* part below search area */
               dt += ( clup - cllo + 1 ) * ( mmin[1] - cmlo );
            }
            if (cllo < mmin[0] && cmup >= mmin[1] && cmlo <= mmax[1]) {
               dt += ( mmin[0] - cllo ) * ( IMIN( cmup, mmax[1] ) - IMAX( cmlo, mmin[1] ) + 1 );
            }
            if (clup > mmax[0] && cmup >= mmin[1] && cmlo <= mmax[1] ) {
               dt += ( clup - mmax[0] ) * ( IMIN( cmup, mmax[1] ) - IMAX( cmlo , mmin[1] ) + 1 );
            }
            if (cmup > mmax[1]) {
               dt += ( clup - cllo + 1 ) * ( cmup - mmax[1] );
            }
         }
         ccnt += ( dt - dd );
         if (cllo > mmax[0] || clup < mmin[0] || cmlo > mmax[1] || cmup < mmin[1]) {
            SUBTRACT( cllo ,			/* lower x of clean area */
                      cmlo ,			/* lower y of clean area */
                      clup ,			/* upper x of clean area */
                      cmup );			/* upper y of clean area */
         } else {
            if (cmlo < mmin[1]) {
               SUBTRACT( cllo ,			/* lower x of clean area */
                         cmlo ,			/* lower y of clean area */
                         clup ,			/* upper x of clean area */
                         mmin[1] - 1 );		/* upper y of clean area */
            }
            if (cllo < mmin[0] && cmup >= mmin[1] && cmlo <= mmax[1]) {
               SUBTRACT( cllo ,			/* lower x of clean area */
                         IMAX( cmlo, mmin[1] ) ,/* lower y of clean area */
                         mmin[0] - 1 ,		/* upper x of clean area */
                         IMIN( cmup, mmax[1] ) );	/* upper y of clean area */
            }
            if (clup > mmax[0] && cmup >= mmin[1] && cmlo <= mmax[1]) {
               SUBTRACT( mmax[0] + 1 ,		/* lower x of clean area */
                         IMAX( cmlo, mmin[1] ) ,/* lower y of clean area */
                         clup ,			/* upper x of clean area */
                         IMIN( cmup, mmax[1] ) );	/* upper y of clean area */
            }
            if (cmup > mmax[1]) {
               SUBTRACT( cllo ,			/* lower x of clean area */
                         mmax[1] + 1 ,		/* lower y of clean area */
                         clup ,			/* upper x of clean area */
                         cmup );		/* upper y of clean area */
            }
         }
      }
      /*
       * Now we put the components in a GDS table. The table has three
       * columns, amplitude, x grid and y grid of components.
       */
      if (okay) {
         bool	create;				/* create columns ? */
         bool	delete;				/* delete columns ? */
         char	ccomms[MAXSTRINGLEN];		/* column comment */
         char	ctypes[MAXSTRINGLEN];		/* column types */
         char	cunits[MAXSTRINGLEN];		/* column units */
         fchar	ccomm;				/* points to buffer above */
         fchar	ctype;				/* points to buffer above */
         fchar	cunit;				/* points to buffer above */
         fint	aerror = 0;			/* GDS return code */
         fint	nrows[3];			/* number of rows */

         status_c( tofchar( "Writing Component Table" ) );
         ccomm.a = ccomms; ccomm.l = MAXSTRINGLEN;
         ctype.a = ctypes; ctype.l = MAXSTRINGLEN;
         cunit.a = cunits; cunit.l = MAXSTRINGLEN;
         gdsa_colinq_c( oset ,			/* output set name */
                        &omaps[nmap] ,		/* output subset level */
                        TABLE_NAME ,		/* name of table */
                        TABLE_COLUMN_A ,	/* name of column */
                        ctype ,			/* column type */
                        ccomm ,			/* column comment */
                        cunit ,			/* column units */
                        &nrows[0] ,		/* number of rows */
                        &aerror );		/* GDS return code */
         if (aerror < 0) {			/* an error occurred */
            nrows[0] = -1;			/* error coding */
            aerror = 0;				/* reset */
         }
         gdsa_colinq_c( oset ,			/* output set name */
                        &omaps[nmap] ,		/* output subset level */
                        TABLE_NAME ,		/* name of table */
                        TABLE_COLUMN_X ,	/* name of column */
                        ctype ,			/* column type */
                        ccomm ,			/* column comment */
                        cunit ,			/* column units */
                        &nrows[1] ,		/* number of rows */
                        &aerror );		/* GDS return code */
         if (aerror < 0) {			/* an error occurred */
            nrows[1] = -1;			/* error coding */
            aerror = 0;				/* reset */
         }
         switch( classdim ) {
            case 1: {
               nrows[2] = nrows[1];
               break;
            }
            case 2: {
               gdsa_colinq_c( oset ,		/* output set name */
                              &omaps[nmap] ,	/* output subset level */
                              TABLE_NAME ,	/* name of table */
                              TABLE_COLUMN_Y ,	/* name of column */
                              ctype ,		/* column type */
                              ccomm ,		/* column comment */
                              cunit ,		/* column units */
                              &nrows[2] ,	/* number of rows */
                              &aerror );	/* GDS return code */
               if (aerror < 0) {		/* an error occurred */
                   nrows[2] = -1;		/* error coding */
                   aerror = 0;			/* reset */
               }
               break;
            }
            default: {
               break;
            }
         }
         if (nrows[0] != nrows[1] || nrows[1] != nrows[2]) {
            delete = 1;				/* delete old columns */
            create = 1;				/* create new columns */
         } else if (nrows[0] == -1) {		/* neither of the columns exist */
            delete = 0;				/* no delete */
            create = 1;				/* create new columns */
         } else if (!cont) {			/* no continuation */
            delete = 1;				/* delete columns */
            create = 1;				/* create new columns */
         } else {				/* continuation */
            delete = 0;				/* no delete */
            create = 0;				/* no create */
         }
         if (delete) {				/* delete columns */
            gdsa_delcol_c( oset ,		/* output set name */
                           &omaps[nmap] ,	/* output subset level */
                           TABLE_NAME ,		/* name of table */
                           TABLE_COLUMN_A ,	/* name of column */
                           &aerror );		/* GDS return code */
            aerror = 0;				/* reset */
            gdsa_delcol_c( oset ,		/* output set name */
                           &omaps[nmap] ,	/* output subset level */
                           TABLE_NAME ,		/* name of table */
                           TABLE_COLUMN_X ,	/* name of column */
                           &aerror );		/* GDS return code */
            aerror = 0;				/* reset */
            if (classdim == 2) {
               gdsa_delcol_c( oset ,		/* output set name */
                              &omaps[nmap] ,	/* output subset level */
                              TABLE_NAME ,	/* name of table */
                              TABLE_COLUMN_Y ,	/* name of column */
                              &aerror );	/* GDS return code */
               aerror = 0;			/* reset */
	    }
         }
         if (create) {				/* create columns */
            gdsa_crecol_c( oset ,		/* output set name */
                           &omaps[nmap] ,	/* output subset level */
                           TABLE_NAME ,		/* name of table */
                           TABLE_COLUMN_A ,	/* name of column */
                           tofchar("REAL") ,	/* type of column */
                           tofchar("Amplitude") ,
                           bunit ,		/* unit of column */
                           &aerror );		/* GDS error return */
            gdsa_crecol_c( oset ,		/* output set name */
                           &omaps[nmap] ,	/* output subset level */
                           TABLE_NAME ,		/* name of table */
                           TABLE_COLUMN_X ,	/* name of column */
                           tofchar("INT") ,	/* type of column */
                           tofchar("X grid position") ,
                           tofchar("GRIDS") ,	/* unit of column */
                           &aerror );		/* GDS error return */
            if (classdim == 2) {
               gdsa_crecol_c( oset ,		/* output set name */
                              &omaps[nmap] ,	/* output subset level */
                              TABLE_NAME ,	/* name of table */
                              TABLE_COLUMN_Y ,	/* name of column */
                              tofchar("INT") ,	/* type of column */
                              tofchar("Y grid position") ,
                              tofchar("GRIDS") ,/* unit of column */
                              &aerror );	/* GDS error return */
            }
         }
         nrows[0] = nrows[1] = nrows[2] = 0;	/* reset */
         gdsa_wcreal_c( oset ,			/* output set name */
                        &omaps[nmap] ,		/* output subset level */
                        TABLE_NAME ,		/* table name */
                        TABLE_COLUMN_A ,	/* column name */
                        amps ,			/* the amplitudes */
                        &nrows[0] ,		/* starting position */
                        &ncc ,			/* number of rows */
                        &aerror );		/* GDS error return */
         gdsa_wcint_c( oset ,			/* output set name */
                       &omaps[nmap] ,		/* output subset level */
                       TABLE_NAME ,		/* name of table */
                       TABLE_COLUMN_X ,		/* name of column */
                       xpos ,			/* x positions */
                       &nrows[1] ,		/* starting position */
                       &ncc ,			/* number of rows */
                       &aerror );		/* GDS error return */
         if (classdim == 2) {
            gdsa_wcint_c( oset ,		/* output set name */
                          &omaps[nmap] ,	/* output subset level */
                          TABLE_NAME ,		/* name of table */
                          TABLE_COLUMN_Y ,	/* name of column */
                          ypos ,		/* y positions */
                          &nrows[2] ,		/* starting position */
                          &ncc ,		/* number of rows */
                          &aerror );		/* GDS error return */
         }
      }
      /*
       * Finally, we produce some output for the user.
       */
      if (okay) {
         char	string[MAXSTRINGLEN];		/* text buffer */
         fint	mode = 1;			/* difference mode */
         fint	n;				/* axes counter */
         fint	output_level = 3;		/* to screen and logfile */
         time_t now;

         timer_c( &ct, &rt, &mode );		/* set timers */
         now = time( (time_t *) NULL );		/* get time */
         strftime( string, sizeof( string ), "CLEAN %c", localtime( &now ) );
         comment( oset, omaps[nmap], string );
         sprintf( string, "------------------------------------------------------------------------------" );
         anyout_c( &output_level, tofchar( string ) );
         sprintf( string, "CLEAN statistics for set   : %.*s", (int) nelc_c( iset ), iset.a );
         for ( n = classdim; n < isetdim; n++) {
            char	buffer[MAXSTRINGLEN];
            fint	grid;
            fint	gerror = 0;

            grid = gdsc_grid_c( iset ,
                                &iperm[n] ,
                                &imaps[nmap] ,
                                &gerror );
            sprintf( buffer, " %.*s %d", (int) nelc_c( ictype[iperm[n]-1] ), ictype[iperm[n]-1].a, grid );
            strcat( string, buffer );
         }
         anyout_c( &output_level, tofchar( string ) );
         comment( oset, omaps[nmap], string );
         {					/* show physical coordinates */
            double	c1[CLASSDIM];
            double	c2[MAXAXES];
            fint	dir = 1;

            for (n = 0; n < classdim; c1[n++] = 0.0);
            if (!cotrans_c( iset, &imaps[nmap], c1, c2, &dir ) && isetdim > classdim) {
               for ( n = classdim; n < isetdim; n++) {
                  sprintf( string, "Subset Coordinates         : %20f %.*s", c2[iperm[n]-1], MAXFITSCHARLEN, icunit[iperm[n]-1].a );
                  anyout_c( &output_level, tofchar( string ) );
                  comment( oset, omaps[nmap], string );
               }
            }
         }
         if (n = nelc_c( bunit )) {		/* units of map */
            sprintf( string, "Units of %-18s: %.*s", dim[classdim-1], n, bunit.a );
            anyout_c( &output_level, tofchar( string ) );
         }
         sprintf( string, "Number of components       : %20d", ncc );
         anyout_c( &output_level, tofchar( string ) );
         comment( oset, omaps[nmap], string );
         addi_descriptor( oset, omaps[nmap], "NCOMP", ncc, cont );
         if (cutopt > 0.0) {
            sprintf( string, "Optimization starts after  : %20d components", nccopt );
            anyout_c( &output_level, tofchar( string ) );
            comment( oset, omaps[nmap], string );
         }
         switch( classdim ) {
            case 1: {
               sprintf( string, "Minimum Search area        : %10d%10d", smin[0], smax[0] );
               break;
            }
            case 2: {
               sprintf( string, "Minimum Search area        : %5d%5d%5d%5d", smin[0], smin[1], smax[0], smax[1] );
               break;
            }
            default: {
               break;
            }
         }
         anyout_c( &output_level, tofchar( string ) );
         comment( oset, omaps[nmap], string );
         switch( classdim ) {
            case 1: {
               sprintf( string, "Effective Clean area       : %10d%10d", cllo, clup );
               break;
            }
            case 2: {
               sprintf( string, "Effective Clean area       : %5d%5d%5d%5d", cllo, cmlo, clup, cmup );
               break;
            }
            default: {
               break;
            }
         }
         anyout_c( &output_level, tofchar( string ) );
         comment( oset, omaps[nmap], string );
         sprintf( string, "Integrated Antenna Pattern : %20f", sumapt );
         anyout_c( &output_level, tofchar( string ) );
         comment( oset, omaps[nmap], string );
         if (pbcopt) {
            sprintf( string, "Dirty Flux                 : %20f (PBC = %20f)", summap / sumapt, pbcmap / sumapt );
            addr_descriptor( oset, omaps[nmap], "DFLUX", pbcmap / sumapt, cont );
         } else {
            sprintf( string, "Dirty Flux                 : %20f", summap / sumapt );
         }
         addr_descriptor( oset, omaps[nmap], "UDFLUX", summap / sumapt, cont );
         anyout_c( &output_level, tofchar( string ) );
         comment( oset, omaps[nmap], string );
         if (pbcopt) {
            sprintf( string, "Component Flux             : %20f (PBC = %20f)", sumcmp, pbccmp );
            addr_descriptor( oset, omaps[nmap], "CFLUX", pbccmp, cont );
         } else {
            sprintf( string, "Component Flux             : %20f", sumcmp );
         }
         addr_descriptor( oset, omaps[nmap], "UCFLUX", sumcmp, cont );
         anyout_c( &output_level, tofchar( string ) );
         comment( oset, omaps[nmap], string );
         if (pbcopt) {
            sprintf( string, "Residual Flux              : %20f (PBC = %20f)", sumres / sumapt, pbcres / sumapt );
            addr_descriptor( oset, omaps[nmap], "RFLUX", pbcres / sumapt, cont );
         } else {
            sprintf( string, "Residual Flux              : %20f", sumres / sumapt );
         }
         addr_descriptor( oset, omaps[nmap], "URFLUX", sumres / sumapt, cont );
         anyout_c( &output_level, tofchar( string ) );
         comment( oset, omaps[nmap], string );
         if (scnt > 0) {
            sprintf( string, "Mean value Search Area     : %20f", ss1 / scnt );
            anyout_c( &output_level, tofchar( string ) );
            comment( oset, omaps[nmap], string );
         }
         if (scnt > 1) {
            sprintf( string, "R.M.S. Search Area         : %20f", sqrt( ( ss2 - ss1 * ss1 / scnt ) / ( scnt - 1 ) ) );
            anyout_c( &output_level, tofchar( string ) );
            comment( oset, omaps[nmap], string );
         }
         if (ccnt > 0) {
            sprintf( string, "Mean value Clean Area      : %20f", cs1 / ccnt );
            anyout_c( &output_level, tofchar( string ) );
            comment( oset, omaps[nmap], string );
         }
         if (ccnt > 1) {
            sprintf( string, "R.M.S. Clean Area          : %20f", sqrt( ( cs2 - cs1 * cs1 / ccnt ) / ( ccnt - 1 ) ) );
            anyout_c( &output_level, tofchar( string ) );
            comment( oset, omaps[nmap], string );
         }
         sprintf( string, "CPU time used              : %20.3f seconds", ct );
         anyout_c( &output_level, tofchar( string ) );
         comment( oset, omaps[nmap], string );
         sprintf( string, "REAL time used             : %20.3f seconds", rt );
         anyout_c( &output_level, tofchar( string ) );
         comment( oset, omaps[nmap], string );
         sprintf( string, "------------------------------------------------------------------------------" );
         anyout_c( &output_level, tofchar( string ) );
      }
      /*
       * Free the allocated memory.
       */
      if (apt != NULL) {
         free( apt ); aptsize = 0; apt = NULL;	/* release memory */
      }
      if (map != NULL) {
         free( map ); mapsize = 0; map = NULL;	/* release memory */
      }
   }
   gerror = 0;					/* reset */
   gdsd_history_c( oset, &gerror );		/* write history */
   finis_c( );					/* quit with HERMES */
   return( EXIT_SUCCESS );			/* exit with status */
} 
