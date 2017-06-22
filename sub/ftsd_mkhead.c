/* ftsd_mkhead.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            ftsd_mkhead.dc2

Function:     FTSD_MKHEAD

Purpose:      Creates the FITS descriptors describing the coordinate
              system from a GDS (sub)set.

Category:     FITS

File:         ftsd_mkhead.c

Author:       K.G. Begeman

Use:          INTEGER FTSD_MKHEAD( SET ,      Input    CHARACTER*(*)
                                   CWLO ,     Input    INTEGER(8)
                                   CWHI ,     Input    INTEGER(8)
                                   BLOCKED ,  Input    LOGICAL
                                   HEADER ,   Output   CHARACTER*(80)
                                   MAXREC )   Input    INTEGER

              FTSD_MKHEAD      Returns:
                               >0: Number of records added to HEADER.
                               -1: Set does not exist.
                               -2: memory allocation error.
                               -3: Not enough space in HEADER.
              SET              Name of set.
              CWLO             Lower coordinate word of subset frame.
              CWHI             Upper coordinate word of subset frame.
              BLOCKED          True is tape maybe blocked, false if not.
              HEADER           Character array to receive the FITS
                               descriptors.
              MAXREC           Maximum number of FITS records in HEADER.

Updates:      Dec 16, 1990: KGB Document created.
              Oct  9, 1999: VOG Fix number of dashes between ctypes's 
                                RA/DEC and their projections so that 
                                projections are correctly read in AIPS 
                                and IRAF.

#<

Fortran to C interface:

@ integer function ftsd_mkhead( character, integer*8, integer*8, logical, character,
@                               integer )

*/

/*
 * includes:
 */

#include	"stddef.h"			/* <stddef.h> */
#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include        "string.h"
#include	"gipsyc.h"			/* GIPSY symbols */
#include	"gds_exist.h"			/* define gds_exist_c */
#include	"gdsc_grid.h" 			/* define gdsc_grid_c */
#include	"gdsc_ndims.h"			/* define gdsc_ndims_c */
#include	"gdsc_origin.h"			/* define gdsc_origin_c */
#include	"gdsc_range.h"			/* define gdsc_range_c */
#include	"gdsc_substruct.h"		/* define gdsc_substruct_c */
#include	"gdsd_rchar.h"			/* define gdsd_rchar_c */
#include	"gdsd_rdble.h"			/* define gdsd_rdble_c */
#include	"gdsd_rint.h"			/* define gdsd_rint_c */
#include        "nelc.h"

/*
 *  defines:
 */

#define	FITSFIELDLEN	20			/* length of FITS data field */
#define	FITSNAMELEN	8			/* length of FITS descriptor */
#define	FITSCOMMLEN	50			/* lenght of FITS comments */
#define	FITSRECORDLEN	80			/* length of FITS record */
#define	FITSSTRINGLEN	(FITSFIELDLEN-2)	/* length of FITS string */

#define	FITS_CHAR_DSC(record,dsc,c,comm)	((void)	sprintf( record, \
   "%-*s= '%-*.*s'%-*s", FITSNAMELEN, dsc, FITSFIELDLEN-2, FITSFIELDLEN-2, \
   c, FITSCOMMLEN, comm ))
#define	FITS_DBLE_DSC(record,dsc,d,comm)	((void)	sprintf( record, \
   "%-*s= %*.*E%-*s", FITSNAMELEN, dsc, FITSFIELDLEN, FITSFIELDLEN-8, d, \
   FITSCOMMLEN, comm ))
#define	FITS_INT_DSC(record,dsc,i,comm)		((void)	sprintf( record, \
   "%-*s= %*d%-*s", FITSNAMELEN, dsc, FITSFIELDLEN, i, FITSCOMMLEN, comm ))
#define	FITS_LOG_DSC(record,dsc,l,comm)		((void) sprintf( record, \
   "%-*s= %*.1s%-*s", FITSNAMELEN, dsc, FITSFIELDLEN, (toflog(l) ? "T" : "F"), \
   FITSCOMMLEN, comm ))
#define	ADD_RECORD( record, header, inhead )		{ \
   int	i; \
   for (i = 0; i < FITSRECORDLEN; header.a[inhead++] = record[i++]); \
}


fint	ftsd_mkhead_c( fchar	set     ,	/* name of set */
                       fint8	*cwblo  ,	/* lower cw of subset frame */
                       fint8	*cwbhi  ,	/* upper cw of subset frame */
                       bool	*blocked ,	/* blocked tape ? */
                       fchar	header  ,	/* the fits header */
                       fint	*maxrec )	/* maximum number of records */
{
   char		bval[FITSSTRINGLEN+1];		/* buffer */
   char		record[FITSRECORDLEN+1];	/* buffer for one FITS record */
   double	dval;				/* double precision dummy */
   fchar	cval;				/* f character */
   fint8		cwfhi;				/* c.w. upper frame */
   fint8		cwflo;				/* c.w. lower frame */
   fint8		*gbhi;				/* grids upper box */
   fint8		*gblo;				/* grids lower box */
   fint 	gerror = 0;			/* GDS error return code */
   fint8		*gfhi;				/* grids upper frame */
   fint8		*gflo;				/* grids lower frame */
   fint8		level = 0;			/* level in set */
   fint		naxis;				/* number of axes */
   fint		*perm;				/* axis permutation */
   fint		r = 0;				/* return value */
   fint		setdim;				/* dimension of set */
   fint8		subset;				/* subset level */
   fint		subdim;				/* dimension of subset */
   int		inhead = 0;			/* number of characters */
   int		n;				/* counter */
   int		nd;				/* number of axis descriptors */
   int		ni;				/* inside subset counter */
   int		no;				/* outside subset counter */

   cval.a = bval;				/* address */
   cval.l = FITSSTRINGLEN;			/* length */
   bval[FITSSTRINGLEN] = 0;			/* trailing zero byte */
   if (!tobool(gds_exist_c( set, &gerror ))) {	/* set does not exist */
      return( -1 );				/* code: set doesn not exist */
   }
   setdim = gdsc_ndims_c( set, &level );	/* dimension of set */
						/* get subset level */
   subset = gdsc_substruct_c( set, cwblo, cwbhi, &gerror );
   subdim = gdsc_ndims_c( set, &subset );	/* dimensions of subset */
						/* get frame of set */
   gdsc_range_c( set, &level, &cwflo, &cwfhi, &gerror );
   gbhi = calloc( sizeof( fint ), setdim );	/* allocate memory */
   if (gbhi == NULL) {				/* allocation problems */
      return( -2 );				/* code: allocation error */
   }
   gblo = calloc( sizeof( fint ), setdim );	/* allocate memory */
   if (gblo == NULL) {				/* allocation problems */
      free( gbhi );				/* deallocate */
      return( -2 );				/* code: allocation error */
   }
   gfhi = calloc( sizeof( fint ), setdim );	/* allocate memory */
   if (gfhi == NULL) {				/* allocation problems */
      free( gbhi );				/* deallocate */
      free( gblo );				/* deallocate */
      return( -2 );				/* code: allocation error */
   }
   gflo = calloc( sizeof( fint ), setdim );	/* allocate memory */
   if (gflo == NULL) {				/* allocation problems */
      free( gbhi );				/* deallocate */
      free( gblo );				/* deallocate */
      free( gfhi );				/* deallocate */
      return( -2 );				/* code: allocation error */
   }
   perm = calloc( sizeof( fint ), setdim );	/* allocate memory */
   if (perm == NULL) {				/* allocation problems */
      free( gbhi );				/* deallocate */
      free( gblo );				/* deallocate */
      free( gfhi );				/* deallocate */
      free( gflo );				/* deallocate */
      return( -2 );				/* code: allocation error */
   }
						/* loop to get axis order */
   for (ni = 0, no = subdim, n = 0; n < setdim; n++) {
      fint	axnum = n + 1;			/* axis sequence number */
      fint	grid;
      fint	idx;				/* axis index */

      grid = gdsc_grid_c( set, &axnum, &subset, &gerror );
      if (gerror) {				/* inside subset */
         idx = ni++;
         gerror = 0;
         gbhi[idx] = gdsc_grid_c( set, &axnum, cwbhi, &gerror );
         gblo[idx] = gdsc_grid_c( set, &axnum, cwblo, &gerror );
      } else {					/* outside subset */
         idx = no++;
         gbhi[idx] = gblo[idx] = grid;
      }
      perm[n] = idx;				/* set permutation */
      gfhi[idx] = gdsc_grid_c( set, &axnum, &cwfhi, &gerror );
      gflo[idx] = gdsc_grid_c( set, &axnum, &cwflo, &gerror );
   }
   
   FITS_INT_DSC( record, "NAXIS", (int) setdim, " / NUMBER OF AXES" );
   if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
   for (n = 0; n < setdim; n++) {
      char	dsco[FITSNAMELEN+1];

      (void) sprintf( dsco, "NAXIS%d", n + 1 );
      naxis = gbhi[n] - gblo[n] + 1;
      FITS_INT_DSC( record, dsco, (int) naxis, " / LENGTH OF AXIS" );
      if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
   }
   FITS_LOG_DSC( record, "BLOCKED", *blocked, " / TAPE MAY BE BLOCKED" );
   if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
   for (nd = 1, n = 0; nd; n++) {
      char	dsci[FITSNAMELEN+1];
      char	dsco[FITSNAMELEN+1];
      int	axnumi;
      int	axnumo = n + 1;

      nd = 0;
      if (n < setdim) {
         axnumi = perm[n] + 1;
      } else {
         axnumi = axnumo;
      }
      if (n < setdim) {
         nd += 1;
      }
      (void) sprintf( dsci, "CDELT%d", axnumi );
      (void) sprintf( dsco, "CDELT%d", axnumo );
      gdsd_rdble_c( set, tofchar( dsci ), &level, &dval, &gerror );
      if (!gerror) {
         nd += 1;
         FITS_DBLE_DSC( record, dsco, dval, " / PRIMARY PIXEL SEPARATION" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "CROTA%d", axnumi );
      (void) sprintf( dsco, "CROTA%d", axnumo );
      gdsd_rdble_c( set, tofchar( dsci ), &level, &dval, &gerror );
      if (!gerror) {
         nd += 1;
         FITS_DBLE_DSC( record, dsco, dval, " / PRIMARY ROTATION OF AXIS" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "CRPIX%d", axnumi );
      (void) sprintf( dsco, "CRPIX%d", axnumo );
      if (n < setdim) {
         gdsd_rdble_c( set, tofchar( dsci ), &level, &dval, &gerror );
      } else {
         fint	axnumber = axnumi;

         dval = gdsc_origin_c( set, &axnumber, &gerror );
      }
      if (!gerror) {
         nd += 1;
         if (n < setdim) {
            dval += ( gflo[n] - gblo[n] );
         }
         FITS_DBLE_DSC( record, dsco, dval, " / PRIMARY REFERENCE PIXEL" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "CRVAL%d", axnumi );
      (void) sprintf( dsco, "CRVAL%d", axnumo );
      gdsd_rdble_c( set, tofchar( dsci ), &level, &dval, &gerror );
      if (!gerror) {
         nd += 1;
         FITS_DBLE_DSC( record, dsco, dval, " / PRIMARY REFERENCE VALUE" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "CTYPE%d", axnumi );
      (void) sprintf( dsco, "CTYPE%d", axnumo );
      gdsd_rchar_c( set, tofchar( dsci ), &level, cval, &gerror );
      if (!gerror) 
      {
         /* Number of programs follow the AIPS convention i.e the   */
         /* axis names must be filled up to 4 positions with dashes */
         /* followed by a dash and the projection.                  */
         /* Only RA and DEC have to be checked.                     */

         nd += 1;
         if (strncmp(bval,"RA",2)==0 || strncmp(bval,"DEC",3)==0)
         {
            char  buf[FITSSTRINGLEN+1];
            int   i = 0, j = 0;            
            while (i < nelc_c(cval) )
            {
               if (bval[i] != '-')
               {
                  buf[j++] = bval[i++];
               }
               else
               {
                  while (j < 5)
                     buf[j++] = '-';
                  while (bval[i] == '-')
                     i++;
               }
            }
            buf[j] = '\0';
            FITS_CHAR_DSC( record, dsco, buf, " / PRIMARY AXIS NAME" );
         }
         else
         {
            FITS_CHAR_DSC( record, dsco, bval, " / PRIMARY AXIS NAME" );
         }

         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "CUNIT%d", axnumi );
      (void) sprintf( dsco, "CUNIT%d", axnumo );
      gdsd_rchar_c( set, tofchar( dsci ), &level, cval, &gerror );
      if (!gerror) {
         nd += 1;
         FITS_CHAR_DSC( record, dsco, bval, " / PRIMARY AXIS UNITS" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "DDELT%d", axnumi );
      (void) sprintf( dsco, "DDELT%d", axnumo );
      gdsd_rdble_c( set, tofchar( dsci ), &level, &dval, &gerror );
      if (!gerror) {
         nd += 1;
         FITS_DBLE_DSC( record, dsco, dval, " / SECONDARY PIXEL SEPARATION" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "DROTA%d", axnumi );
      (void) sprintf( dsco, "DROTA%d", axnumo );
      gdsd_rdble_c( set, tofchar( dsci ), &level, &dval, &gerror );
      if (!gerror) {
         nd += 1;
         FITS_DBLE_DSC( record, dsco, dval, " / SECONDARY ROTATION OF AXIS" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "DRPIX%d", axnumi );
      (void) sprintf( dsco, "DRPIX%d", axnumo );
      gdsd_rdble_c( set, tofchar( dsci ), &level, &dval, &gerror );
      if (!gerror) {
         nd += 1;
         if (n < setdim) {
            dval += ( gflo[n] - gblo[n] );
         }
         FITS_DBLE_DSC( record, dsco, dval, " / SECONDARY REFERENCE PIXEL" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "DRVAL%d", axnumi );
      (void) sprintf( dsco, "DRVAL%d", axnumo );
      gdsd_rdble_c( set, tofchar( dsci ), &level, &dval, &gerror );
      if (!gerror) {
         nd += 1;
         FITS_DBLE_DSC( record, dsco, dval, " / SECONDARY REFERENCE VALUE" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "DTYPE%d", axnumi );
      (void) sprintf( dsco, "DTYPE%d", axnumo );
      gdsd_rchar_c( set, tofchar( dsci ), &level, cval, &gerror );
      if (!gerror) {
         nd += 1;
         FITS_CHAR_DSC( record, dsco, bval, " / SECONDARY AXIS NAME" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
      (void) sprintf( dsci, "DUNIT%d", axnumi );
      (void) sprintf( dsco, "DUNIT%d", axnumo );
      gdsd_rchar_c( set, tofchar( dsci ), &level, cval, &gerror );
      if (!gerror) {
         nd += 1;
         FITS_CHAR_DSC( record, dsco, bval, " / SECONDARY AXIS UNITS" );
         if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
      }
      gerror = 0;
   }
   gdsd_rdble_c( set, tofchar( "EPOCH" ), &level, &dval, &gerror );
   if (!gerror) {
      FITS_DBLE_DSC( record, "EPOCH", dval, " / EPOCH" );
      if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
   }
   gerror = 0;
   gdsd_rdble_c( set, tofchar( "FREQ0" ), &level, &dval, &gerror );
   if (!gerror) {
      FITS_DBLE_DSC( record, "FREQ0", dval, " / REST FREQUENCY" );
      if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
   }
   gerror = 0;
   gdsd_rchar_c( set, tofchar( "INSTRUME" ), &level, cval, &gerror );
   if (!gerror) {
      FITS_CHAR_DSC( record, "INSTRUME", bval, " / INSTRUMENT" );
      if (r++ < *maxrec) ADD_RECORD( record, header, inhead );
   }
   free( gbhi );				/* deallocate */
   free( gblo );				/* deallocate */
   free( gfhi );				/* deallocate */
   free( gflo );				/* deallocate */
   free( perm );				/* deallocate */
   if (r > *maxrec) r = -3;			/* header too small */
   return( r );					/* return to caller */
}

#if	defined(TESTBED)

#include	"cmain.h"
#include	"gdsinp.h"
#include	"gdsbox.h"
#include	"init.h"
#include	"finis.h"
#include	"anyout.h"
#include	"gdsc_fill.h"

#define	CLASSDIM	0
#define	KEY_BOX		tofchar(" ")
#define	KEY_INSET	tofchar("INSET=")
#define	MAXAXES		16
#define	MAXHEADLEN	28800
#define	MAXSETNAMLEN	80
#define	MAXSUBSETS	128
#define	MES_BOX		tofchar(" ")
#define	MES_INSET	tofchar("Set and subset(s) to be displayed")

#define	finit(f,s)	{\
				static	char	buf[s+1];\
				int		i;\
				for (i = 0; i < s; buf[i++] = ' ');\
				buf[s] = '\0';\
				f.a = buf;\
				f.l = s;\
			}

MAIN_PROGRAM_ENTRY
{
   fchar		head;			/* the header */
   fchar		set;			/* name of set */
   fint			axperm[MAXAXES];	/* axis permutation array */
   fint			axsize[MAXAXES];	/* axis size array */
   fint			blo[MAXAXES];		/* lower box coordinates */
   fint			bhi[MAXAXES];		/* upper box coordinates */
   fint			cwhi;			/* upper coordinate word */
   fint			cwlo;			/* lower coordinate word */
   fint			ns;			/* subset counter */
   fint			nsubs;			/* number of subsets */
   fint			subsets[MAXSUBSETS];	/* buffer for subset levels */

   init_c( );					/* get in touch with HERMES */
   finit( set, MAXSETNAMLEN );			/* initialize f character */
   finit( head, MAXHEADLEN );
   {						/* get set and subsets */
      fint	class = 1;			/* class two application */
      fint	classdim = CLASSDIM;		/* dimension of subsets */
      fint	input_level = 0;		/* input level (no default) */
      fint	maxaxes = MAXAXES;
      fint	maxsubsets = MAXSUBSETS;	/* maximum number of subsets */
      fint	output_level = 11;		/* output level */

      nsubs = gdsinp_c( set ,			/* name of set */
                        subsets ,		/* levels of subsets */
                        &maxsubsets ,		/* maximum number of subsets */
                        &input_level ,		/* default level */
                        KEY_INSET ,		/* keyword */
                        MES_INSET ,		/* message */
                        &output_level ,		/* output level */
                        axperm ,		/* axis permutation */
                        axsize ,		/* axis size */
                        &maxaxes ,		/* maximum number of axes */
                        &class ,		/* class of application */
                        &classdim );		/* dimension of class */
   }						/* we've got the data */
   {						/* now get the area */
      fint	box_mode = 0;			/* mode for gdsbox */
      fint	input_level = 2;		/* hidden keyword */
      fint	output_level = 11;		/* to screen etc. */

      gdsbox_c( blo ,				/* lower box coordinates */
                bhi ,				/* upper box coordinates */
                set ,				/* name of set */
                subsets ,			/* first subset */
                &input_level ,			/* input level */
                KEY_BOX ,			/* keyword */
                MES_BOX ,			/* message */
                &output_level ,			/* output level */
                &box_mode );			/* box option */
   }						/* now we've got the box */
   for (ns = 0; ns < nsubs; ns++) {
      fint	maxrec = MAXHEADLEN / 80;
      fint	nr, nrec = 0;

      cwlo = gdsc_fill_c( set, &subsets[ns], blo );
      cwhi = gdsc_fill_c( set, &subsets[ns], bhi );
      nrec = ftsd_mkhead_c( set, &cwlo, &cwhi, head, &maxrec );
      for (nr = 0; nr < nrec; nr++) {
         fchar	mes;
         fint	output_level = 0;

         mes.a = &head.a[nr*80];
         mes.l = 80;
         anyout_c( &output_level, mes );
      }
   }
   finis_c( );
}
#endif
