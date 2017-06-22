/* cotrans.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

*/

#include	"stddef.h"		/* <stddef.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"ctype.h"		/* <ctype.h> */
#include	"string.h"		/* <string.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"math.h"		/* <math.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"axtype.h"		/* defines axtype_c */
#include	"dcddble.h"		/* defines dcddble_c */
#include	"epoco.h"		/* defines epoco_c */
#include	"factor.h"		/* defines factor_c */
#include	"nelc.h"		/* defines nelc_c */
#include	"skyco.h"		/* defines skyco_c */
#include	"skypro.h"		/* defines skypro_c */
#include	"velpro.h"		/* defines velpro_c */
#include	"gds_exist.h"		/* defines gds_exist_c */
#include	"gdsc_grid.h"		/* defines gdsc_grid_c */
#include	"gdsc_ndims.h"		/* defines gdsc_ndims_c */
#include	"gdsc_origin.h"		/* defines gdsc_origin_c */
#include	"gdsd_rchar.h"		/* defines gdsd_rchar_c */
#include	"gdsd_rdble.h"		/* defines gdsd_rdble_c */
#include	"gdsd_rint.h"		/* defines gdsd_rint_c */
#include	"dblank.h"		/* defines dblank_c */

#define	GETGRID( n )		\
	set_info.ax[n].grid - set_info.ax[n].offset
#define	SETGRID( n )		\
	set_info.ax[n].grid += set_info.ax[n].offset
#define	MAXDSCLEN	8		/* maximum length fits descriptor */
#define	MAXSETBUF	10		/* maximum length buffer */
#define MAXNAMLEN	18		/* maximum length of axis name */
#define MAXSETNAMLEN	80		/* maximum length of set name */

#define	MIN(x,y)	(((x) < (y)) ? (x) : (y))	/* MIN(x,y) */
/* Old definition: #define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )*/
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )

typedef struct {
   fint   naxis;                        /* length of axis */
   fint   pmask;                        /* masks defined primary descriptors */
   char   cunit[MAXNAMLEN];             /* units of axis */
   char   ctype[MAXNAMLEN];             /* name of axis */
   double crval;                        /* reference value at crpix */
   double crpix;                        /* reference pixel */
   double crota;                        /* rotation axis */
   double cdelt;                        /* grid separation */
   fint   smask;                        /* masks defined secondary descriptors */
   char   dunit[MAXNAMLEN];             /* secondary units of axis */
   char   dtype[MAXNAMLEN];             /* name of secondary axis */
   double drval;                        /* secondary reference value at crpix */
   double drpix;                        /* reference pixel of secondary axis */
   double drota;                        /* rotation angle of secondary axis */
   double ddelt;                        /* grid separation of secondary axis */
   char   nunit[MAXNAMLEN];             /* natural units of axis */
   char   sunit[MAXNAMLEN];             /* secondary natural units */
   double cfact;                        /* primary unit conversion factor */
   double dfact;                        /* secondary unit conversion factor */
   fint   axtype;                       /* axis type code */
   fint   skysys;                       /* sky system code */
   fint   skybase;                      /* base sky system */
   fint   prosys;                       /* sky projection system code */
   fint   velsys;                       /* velocity code for spectral axis */
   fint   epoco;                        /* should we call epoco */
   double grid;                         /* grid coordinate */
   double coord;                        /* physical coordinate */
   double offset;			/* offset to real grid */
   fint   def;                          /* grid defined ? */
   fint   done;                         /* transformation already done ? */
   fint   paired;                       /* pointer to paired axis */
} ax_struct;

typedef struct {
   fint       age;                      /* age count */
   fint       dir;			/* direction of transform */
   fint       err;                      /* last error */
   char       set[MAXSETNAMLEN];        /* name of set */
   fint       nel;                      /* length of set name */
   fint       naxis;                    /* number of axis in set */
   fint       maxis;			/* above + hidden axis */
   fint       subset;                   /* coordinate word of subset */
   fint       subdim;                   /* dimensions of subset */
   double     freq0;                    /* rest frequency */
   double     epoch;                    /* epoch of equatorial coordinates */
   char       instrume[MAXNAMLEN];      /* name of instrument */
   ax_struct *ax;                       /* pointer to buffer with axis info */
} set_struct;

static	set_struct	set_info_buf[MAXSETBUF];/* set info buffer */

static	char		dscbuf[MAXDSCLEN+1];	/* buffer for tmp descriptor */

static fchar descr( char *name, fint axnum )
/*
 * This procedure creates a fits descriptor consisting of name plus
 * the axis number attached.
 */
{
   fchar r;					/* return value */
   fint  l;					/* counter */

   sprintf( dscbuf, "%s%d", name, axnum );	/* encode descriptor */
   l = strlen( dscbuf );			/* length of descriptor */
   while (l < MAXDSCLEN) dscbuf[l++] = ' ';	/* fill with blanks */
   r.a = dscbuf; r.l = MAXDSCLEN;		/* initialize return value */
   return( r );					/* return to caller */
}


static double offset( double x )
/*
 * This function finds the closest integer to x (based on function floor() and subtracts it from x.
 */
{
   return( x - NINT(x) );
}

static set_struct get_set_info( fchar set, fint subset, fint dir )
/*
 * This procedure gets the necessary set info and returns it to the caller.
 */
{
   fint setn = 0;
   fint d;
   fint	l = nelc_c( set );
   fint m;
   fint n;

   /*
    * First we try to find whether the information we need is in the internal
    * buffer.
    */
   do {						/* loop through info buffer */
      if ( set_info_buf[setn].age ) {		/* buffer not empty */
         d = l - set_info_buf[setn].nel;	/* diff in length of name */
         if (!d) {				/* equal length, check for equal names */
            d = strncmp( set.a, set_info_buf[setn].set, l );
         }
      } else {					/* buffer empty */
         d = 1;					/* so no match */
      }
      if (!d) break;				/* entry in info buffer */
      setn += 1;
   } while (setn < MAXSETBUF);			/* until all searched */
   /*
    * If not found in the info buffer, find a place to put the new set info.
    * This is by first looking for an unused buffer. When all buffers are in
    * use, then the age count of the buffers are examined. A new buffer to
    * be used gets and age count of MAXSETBUF. When the next new buffer is to be
    * used, the age count of the first buffer is decreased. The oldest buffers,
    * i.e. those with the lowest age count, are always overwritten in case there
    * are no free buffers;
    */
   if (d) {
      fchar instr;
      fint  derror = 0;
      fint8  level = 0;
      fint  setc;

      for (setc = 0; setc < MAXSETBUF; setc++) {
         if (set_info_buf[setc].age) {
            set_info_buf[setc].age -= 1;	/* increase age */
            if (!set_info_buf[setc].age && (set_info_buf[setc].ax != NULL)) {
               free( (char *) set_info_buf[setc].ax);
               set_info_buf[setc].ax = NULL;
            }
         }
         if (!set_info_buf[setc].age) setn = setc;
      }
      set_info_buf[setn].age = MAXSETBUF;	/* set the age */
      set_info_buf[setn].dir = dir;		/* set direction */
      set_info_buf[setn].err = 0;		/* reset */
      set_info_buf[setn].nel = MIN(l,MAXSETNAMLEN);	/* length of set name */
      strncpy( set_info_buf[setn].set , set.a, set_info_buf[setn].nel );
      set_info_buf[setn].naxis = gdsc_ndims_c( set, &level );	/* # of axis */
      set_info_buf[setn].maxis = set_info_buf[setn].naxis;
      set_info_buf[setn].subset = 0;		/* reset subset coordinate word */
      /*
       * get name of instrument/telescope
       */
      instr.a = set_info_buf[setn].instrume; instr.l = MAXNAMLEN;
      gdsd_rchar_c( set, tofchar( "INSTRUME" ), &level, instr, &derror );
      if (derror) {
         fint k;
         for (k = 0; k < MAXNAMLEN; set_info_buf[setn].instrume[k++] = ' ');
         derror = 0;
      }
      /*
       * Get rest frequency of observation in Hz.
       */
      gdsd_rdble_c( set, tofchar( "FREQ0" ), &level, &set_info_buf[setn].freq0, &derror );
      if (derror) { set_info_buf[setn].freq0 = 0.0; derror = 0; }
      /*
       * Get epoch of equatorial coordinate system.
       */
      gdsd_rdble_c( set, tofchar( "EPOCH" ), &level, &set_info_buf[setn].epoch, &derror );
      if (derror == -46) { derror = 0; }
      if (derror) { set_info_buf[setn].epoch = 1950.0; derror = 0; }
      /*
       * generate the buffer space for the axis information.
       */
      set_info_buf[setn].ax = (ax_struct *) calloc( set_info_buf[setn].naxis, sizeof(ax_struct) );
      /*
       * Now read the axis information
       */
      for (n = 0; n < set_info_buf[setn].naxis; n++) {
         fchar  ctype;
         fchar  dtype;
         fchar  nunit;
         fchar  cunit;
         fchar  dunit;
         fchar  sunit;
         fint   pmask;
         fint   smask;
         fint   naxis;
         fint   skybase;
         fint   prosys;
         fint   velsys;
         double cdelt;
         double crota;
         double crpix;
         double crval;
         double ddelt;
         double drota;
         double drpix;
         double drval;
         double cfact;
         double dfact;

         /*
          * Get the number of pixels along the axis.
          */
         gdsd_rint_c( set, descr( "NAXIS", n + 1 ), &level, &naxis, &derror );
         if (derror) {
            set_info_buf[setn].err = derror;
            return( set_info_buf[setn] );
         }
         set_info_buf[setn].ax[n].naxis = naxis;
         /***************************/
         /* P R I M A R Y   A X I S */
         /***************************/
         pmask = 0;
         /*
          * Get grid separation along primary axis.
          */
         gdsd_rdble_c( set, descr( "CDELT", n + 1 ), &level, &cdelt, &derror );
         if (derror) { derror = 0; cdelt = 1.0; } else { pmask |= 32; }
         /*
          * Get rotation angle of primary axis.
          */
         gdsd_rdble_c( set, descr( "CROTA", n + 1 ), &level, &crota, &derror );
         if (derror) { derror = 0; crota = 0.0; } else { pmask |= 16; }
         /*
          * Get reference pixel of primary axis.
          */
         /*
          * Do not read descriptor since it is also stored in binary format.
          */
#if   0
         gdsd_rdble_c( set, descr( "CRPIX", n + 1 ), &level, &crpix, &derror );
#else
         {
            fint	axnumber = n + 1;

            crpix = gdsc_origin_c( set, &axnumber, &derror );
         }
#endif
         if (derror) { derror = 0; crpix = 0.0; } else { pmask |= 8; }
         /*
          * Get reference value/ projection centre
          */
         gdsd_rdble_c( set, descr( "CRVAL", n + 1 ), &level, &crval, &derror );
         if (derror) { derror = 0; crval = 0.0; } else { pmask |= 4; }
         /*
          * Get name of primary axis.
          */
         ctype.a = set_info_buf[setn].ax[n].ctype; ctype.l = MAXNAMLEN;
         gdsd_rchar_c( set, descr( "CTYPE", n + 1 ), &level, ctype, &derror );
         if (derror) {
            fint k;

            for (k = 0; k < MAXNAMLEN; ctype.a[k++] = ' ');          /* blank */
            derror = 0;
         } else { pmask |= 2; }
         /*
          * Now find type of axis.
          */
         nunit.a = set_info_buf[setn].ax[n].nunit; nunit.l = MAXNAMLEN;
         sunit.a = set_info_buf[setn].ax[n].sunit; sunit.l = MAXNAMLEN;
         set_info_buf[setn].ax[n].axtype = axtype_c( ctype, nunit, sunit,
            &skybase, &prosys, &velsys );
         set_info_buf[setn].ax[n].epoco = 0;                         /* reset */
         if (skybase == 1) {
            double epoch = set_info_buf[setn].epoch;

            if (epoch > 1975.0) {
               skybase = 5;
               if (fabs( 2000.0 - epoch ) > 0.001) set_info_buf[setn].ax[n].epoco = 1;
            } else {
               if (fabs( 1950.0 - epoch ) > 0.001) set_info_buf[setn].ax[n].epoco = 1;
            }
         }
         set_info_buf[setn].ax[n].skysys = skybase;
         set_info_buf[setn].ax[n].skybase = skybase;
         set_info_buf[setn].ax[n].prosys = prosys;
         set_info_buf[setn].ax[n].velsys = velsys;
         /*
          * Get units of primary axis.
          */
         cunit.a = set_info_buf[setn].ax[n].cunit; cunit.l = MAXNAMLEN;
         gdsd_rchar_c( set, descr( "CUNIT", n + 1 ), &level, cunit, &derror );
         if (derror) {
            fint k;

            for (k = 0; k < MAXNAMLEN; k++) cunit.a[k] = nunit.a[k];
            derror = 0;
         } else { pmask |= 1; }
         if (pmask & 1) {                               /* get scaling factor */
            if (factor_c( cunit, nunit, &cfact )) cfact = 1.0;
         } else { cfact = 1.0; }
         if (pmask & 32) cdelt *= cfact;
         if (pmask & 4) crval *= cfact;
         set_info_buf[setn].ax[n].pmask = pmask;
         set_info_buf[setn].ax[n].cdelt = cdelt;
         set_info_buf[setn].ax[n].crota = crota;
         set_info_buf[setn].ax[n].crpix = crpix;
         set_info_buf[setn].ax[n].crval = crval;
         set_info_buf[setn].ax[n].cfact = cfact;
         set_info_buf[setn].ax[n].offset = offset( crpix );
         /*******************************/
         /* S E C O N D A R Y   A X I S */
         /*******************************/
         smask = 0;
         /*
          * Get grid separation along secondary axis
          */
         gdsd_rdble_c( set, descr( "DDELT", n + 1 ), &level, &ddelt, &derror );
         if (derror) { derror = 0; ddelt = 0.0; } else { smask |= 32; }
         /*
          * Get rotation angle of secondary axis
          */
         gdsd_rdble_c( set, descr( "DROTA", n + 1 ), &level, &drota, &derror );
         if (derror) { derror = 0; drota = 0.0; } else { smask |= 16; }
         /*
          * Get reference pixel of secondary axis
          */
         gdsd_rdble_c( set, descr( "DRPIX", n + 1 ), &level, &drpix, &derror );
         if (derror) { derror = 0; drpix = 0.0; } else { smask |= 8; }
         /*
          * Get second reference value of axis
          */
         gdsd_rdble_c( set, descr( "DRVAL", n + 1 ), &level, &drval, &derror );
         if (derror) { derror = 0; drval = 0.0; } else { smask |= 4; }
         /*
          * Get secondary axis name
          */
         dtype.a = set_info_buf[setn].ax[n].dtype; dtype.l = MAXNAMLEN;
         gdsd_rchar_c( set, descr( "DTYPE", n + 1 ), &level, dtype, &derror );
         if (derror) {
            fint k;

            for (k = 0; k < MAXNAMLEN; dtype.a[k++] = ' ');
            derror = 0;
            if (set_info_buf[setn].ax[n].axtype == 3 && velsys) {
               strncpy( dtype.a, "VELO", 4 );
               smask |= 2;
            }
         } else { smask |= 2; }
         /*
          * Get secondary units of axis
          */
         dunit.a = set_info_buf[setn].ax[n].dunit; dunit.l = MAXNAMLEN;
         gdsd_rchar_c( set, descr( "DUNIT", n + 1 ), &level, dunit, &derror );
         if (derror) {                                      /* default dunits */
            fint k;

            for (k = 0; k < MAXNAMLEN; k++) dunit.a[k] = sunit.a[k];
            derror = 0;
         } else { smask |= 1; }
         /*
          * Get scaling factor
          */
         if (smask & 1) {                               /* get scaling factor */
            if (factor_c( dunit, sunit, &dfact )) dfact = 1.0;
         } else { dfact = 1.0; }
         if (smask & 32) ddelt *= dfact;
         if (smask & 4) drval *= dfact;
         set_info_buf[setn].ax[n].smask = smask;
         set_info_buf[setn].ax[n].ddelt = ddelt;
         set_info_buf[setn].ax[n].drota = drota;
         set_info_buf[setn].ax[n].drpix = drpix;
         set_info_buf[setn].ax[n].drval = drval;
         set_info_buf[setn].ax[n].dfact = dfact;
         set_info_buf[setn].ax[n].paired = -1;
         set_info_buf[setn].ax[n].def = 0;
         set_info_buf[setn].ax[n].done = 0;
      }
      /*
       * Search for hidden axes.
       */
      while (1) {
         ax_struct	ax;
         fchar  ctype;
         fchar  dtype;
         fchar  nunit;
         fchar  cunit;
         fchar  dunit;
         fchar  sunit;
         fint   pmask;
         fint   smask;
         fint   skybase;
         fint   prosys;
         fint   velsys;
         double cdelt;
         double crota;
         double crpix;
         double crval;
         double ddelt;
         double drota;
         double drpix;
         double drval;
         double cfact;
         double dfact;

         ax.naxis = 1;					/* only one pixel */
         /********************************************/
         /* P R I M A R Y   A X I S  ( H I D D E N ) */
         /********************************************/
         pmask = 0;
         /*
          * Get grid separation along primary axis.
          */
         gdsd_rdble_c( set, descr( "CDELT", n + 1 ), &level, &cdelt, &derror );
         if (derror) { derror = 0; cdelt = 1.0; } else { pmask |= 32; }
         /*
          * Get rotation angle of primary axis.
          */
         gdsd_rdble_c( set, descr( "CROTA", n + 1 ), &level, &crota, &derror );
         if (derror) { derror = 0; crota = 0.0; } else { pmask |= 16; }
         /*
          * Get reference pixel of primary axis.
          */
         gdsd_rdble_c( set, descr( "CRPIX", n + 1 ), &level, &crpix, &derror );
         if (derror) { derror = 0; crpix = 0.0; } else { pmask |= 8; }
         /*
          * Get reference value/ projection centre
          */
         gdsd_rdble_c( set, descr( "CRVAL", n + 1 ), &level, &crval, &derror );
         if (derror) { derror = 0; crval = 0.0; } else { pmask |= 4; }
         /*
          * Get name of primary axis.
          */
         ctype.a = ax.ctype; ctype.l = MAXNAMLEN;
         gdsd_rchar_c( set, descr( "CTYPE", n + 1 ), &level, ctype, &derror );
         if (derror) {
            fint k;

            for (k = 0; k < MAXNAMLEN; ctype.a[k++] = ' ');          /* blank */
            derror = 0;
         } else { pmask |= 2; }
         /*
          * Now find type of axis.
          */
         nunit.a = ax.nunit; nunit.l = MAXNAMLEN;
         sunit.a = ax.sunit; sunit.l = MAXNAMLEN;
         ax.axtype = axtype_c( ctype, nunit, sunit,
            &skybase, &prosys, &velsys );
         ax.epoco = 0;                         /* reset */
         if (skybase == 1) {
            double epoch = set_info_buf[setn].epoch;

            if (epoch > 1975.0) {
               skybase = 5;
               if (fabs( 2000.0 - epoch ) > 0.001) ax.epoco = 1;
            } else {
               if (fabs( 1950.0 - epoch ) > 0.001) ax.epoco = 1;
            }
         }
         ax.skysys = skybase;
         ax.skybase = skybase;
         ax.prosys = prosys;
         ax.velsys = velsys;
         /*
          * Get units of primary axis.
          */
         cunit.a = ax.cunit; cunit.l = MAXNAMLEN;
         gdsd_rchar_c( set, descr( "CUNIT", n + 1 ), &level, cunit, &derror );
         if (derror) {
            fint k;

            for (k = 0; k < MAXNAMLEN; k++) cunit.a[k] = nunit.a[k];
            derror = 0;
         } else { pmask |= 1; }
         if (pmask & 1) {                               /* get scaling factor */
            if (factor_c( cunit, nunit, &cfact )) cfact = 1.0;
         } else { cfact = 1.0; }
         if (pmask & 32) cdelt *= cfact;
         if (pmask & 4) crval *= cfact;
         ax.pmask = pmask;
         ax.cdelt = cdelt;
         ax.crota = crota;
         ax.crpix = crpix;
         ax.crval = crval;
         ax.cfact = cfact;
         ax.offset = offset( crpix );
         if (!pmask) break;				/* no more hidden axes */
         set_info_buf[setn].maxis += 1;
         set_info_buf[setn].ax = realloc( set_info_buf[setn].ax, set_info_buf[setn].maxis * sizeof( ax_struct ) );
         memcpy( &set_info_buf[setn].ax[n], &ax, sizeof( ax_struct ) );
         /************************************************/
         /* S E C O N D A R Y   A X I S  ( H I D D E N ) */
         /************************************************/
         smask = 0;
         /*
          * Get grid separation along secondary axis
          */
         gdsd_rdble_c( set, descr( "DDELT", n + 1 ), &level, &ddelt, &derror );
         if (derror) { derror = 0; ddelt = 0.0; } else { smask |= 32; }
         /*
          * Get rotation angle of secondary axis
          */
         gdsd_rdble_c( set, descr( "DROTA", n + 1 ), &level, &drota, &derror );
         if (derror) { derror = 0; drota = 0.0; } else { smask |= 16; }
         /*
          * Get reference pixel of secondary axis
          */
         gdsd_rdble_c( set, descr( "DRPIX", n + 1 ), &level, &drpix, &derror );
         if (derror) { derror = 0; drpix = 0.0; } else { smask |= 8; }
         /*
          * Get second reference value of axis
          */
         gdsd_rdble_c( set, descr( "DRVAL", n + 1 ), &level, &drval, &derror );
         if (derror) { derror = 0; drval = 0.0; } else { smask |= 4; }
         /*
          * Get secondary axis name
          */
         dtype.a = set_info_buf[setn].ax[n].dtype; dtype.l = MAXNAMLEN;
         gdsd_rchar_c( set, descr( "DTYPE", n + 1 ), &level, dtype, &derror );
         if (derror) {
            fint k;

            for (k = 0; k < MAXNAMLEN; dtype.a[k++] = ' ');
            derror = 0;
            if (set_info_buf[setn].ax[n].axtype == 3 && velsys) {
               strncpy( dtype.a, "VELO", 4 );
               smask |= 2;
            }
         } else { smask |= 2; }
         /*
          * Get secondary units of axis
          */
         dunit.a = set_info_buf[setn].ax[n].dunit; dunit.l = MAXNAMLEN;
         gdsd_rchar_c( set, descr( "DUNIT", n + 1 ), &level, dunit, &derror );
         if (derror) {                                      /* default dunits */
            fint k;

            for (k = 0; k < MAXNAMLEN; k++) dunit.a[k] = sunit.a[k];
            derror = 0;
         } else { smask |= 1; }
         /*
          * Get scaling factor
          */
         if (smask & 1) {                               /* get scaling factor */
            if (factor_c( dunit, sunit, &dfact )) dfact = 1.0;
         } else { dfact = 1.0; }
         if (smask & 32) ddelt *= dfact;
         if (smask & 4) drval *= dfact;
         set_info_buf[setn].ax[n].smask = smask;
         set_info_buf[setn].ax[n].ddelt = ddelt;
         set_info_buf[setn].ax[n].drota = drota;
         set_info_buf[setn].ax[n].drpix = drpix;
         set_info_buf[setn].ax[n].drval = drval;
         set_info_buf[setn].ax[n].dfact = dfact;
         set_info_buf[setn].ax[n].paired = -1;
         set_info_buf[setn].ax[n].def = 0;
         set_info_buf[setn].ax[n].done = 0;
         n++;						/* next axis */
      }
      /*
       * Search for paired axis (sky axis)
       */
      for (n = 0; n < set_info_buf[setn].maxis; n++) {
         if (set_info_buf[setn].ax[n].paired == -1) {
            switch(set_info_buf[setn].ax[n].axtype) {
               case 1:
               case 2: {
                  m = 0;
                  while ((m < set_info_buf[setn].maxis) && (set_info_buf[setn].ax[n].paired ==-1)) {
                     if ((set_info_buf[setn].ax[n].axtype + set_info_buf[setn].ax[m].axtype) == 3) {
                        if (set_info_buf[setn].ax[n].skybase == set_info_buf[setn].ax[m].skybase) {
                           if (set_info_buf[setn].ax[n].prosys == set_info_buf[setn].ax[m].prosys) {
                              set_info_buf[setn].ax[n].paired = m;
                              set_info_buf[setn].ax[m].paired = n;
                           }
                        }
                     }
                     m++;
                  }
                  break;
               }
               default: break;
            }
         }
      }
   } else if (set_info_buf[setn].err) return( set_info_buf[setn] );
   /*
    * Here we determine whether the info for the subset is correct in the
    * set buffer.
    */
   if (subset != set_info_buf[setn].subset || dir != set_info_buf[setn].dir) {
      fint  axnum;
      fint  cerror = 0;
      fint  g;

      set_info_buf[setn].dir = dir;
      set_info_buf[setn].subset = subset;
      set_info_buf[setn].subdim = gdsc_ndims_c( set, &subset );
      for (n = 0; n < set_info_buf[setn].maxis; n++) {
         axnum = n + 1;
         set_info_buf[setn].ax[n].done = 0;
         if (n < set_info_buf[setn].naxis) {
            g = gdsc_grid_c( set, &axnum, &subset, &cerror );
            if (cerror) {
               set_info_buf[setn].ax[n].def = 0;
               cerror = 0;
            } else {
               set_info_buf[setn].ax[n].def = 1;
               set_info_buf[setn].ax[n].grid = (double) g;
               if (!dir) set_info_buf[setn].ax[n].done = 1;
            }
         } else {
            set_info_buf[setn].ax[n].def = 1;
            set_info_buf[setn].ax[n].grid = set_info_buf[setn].ax[n].crpix - 1.0;
            if (!dir) set_info_buf[setn].ax[n].done = 1;
         }
      }
   }
   return( set_info_buf[setn] );
}

static fint get_coords( set_struct set_info )
/*
 * This routine does the transform from grids to physical coordinates.
 * The units of the physical coordinates are all natural units, as defined
 * in AXTYPE.
 */
{
   fint m = -1, mode, n, r = 0;

   for (n = 0; !r && n < set_info.maxis; n++) {
      if (!set_info.ax[n].done) {
         switch(set_info.ax[n].axtype) {
            case 1: {                     /* need other axis to make a couple */
               m = set_info.ax[n].paired;
               if (m != -1) {
                  double	x, y;

                  x = GETGRID( n );
                  y = GETGRID( m );
                  set_info.ax[n].done = 1;
                  set_info.ax[m].done = 1;
                  mode = 3;
                  if (set_info.ax[n].epoco && set_info.ax[n].skysys != set_info.ax[n].skybase) {
                     double epoch1 = set_info.epoch;
                     double epoch2 = 1950.0;
                     double a, d;
                     fint   skysys = 1;

                     r = skypro_c( &x,
                                   &y,
                                   &a,
                                   &d,
                                   &set_info.ax[n].crval,
                                   &set_info.ax[m].crval,
                                   &set_info.ax[n].cdelt,
                                   &set_info.ax[m].cdelt,
                                   &set_info.ax[m].crota,
                                   &set_info.ax[n].skybase,
                                   &set_info.ax[n].skybase,
                                   &set_info.ax[n].prosys,
                                   &mode );
                     epoco_c( &a,
                              &d,
                              &epoch1,
                              &a,
                              &d,
                              &epoch2 );
                     r = skyco_c( &a,
                                  &d,
                                  &skysys,
                                  &set_info.ax[n].coord,
                                  &set_info.ax[m].coord,
                                  &set_info.ax[n].skysys );
                  } else {
                     r = skypro_c( &x,
                                   &y,
                                   &set_info.ax[n].coord,
                                   &set_info.ax[m].coord,
                                   &set_info.ax[n].crval,
                                   &set_info.ax[m].crval,
                                   &set_info.ax[n].cdelt,
                                   &set_info.ax[m].cdelt,
                                   &set_info.ax[m].crota,
                                   &set_info.ax[n].skysys,
                                   &set_info.ax[n].skybase,
                                   &set_info.ax[n].prosys,
                                   &mode );
                  }
               } else {                                 /* no matching couple */
                  r = 13;
               }
               break;
            }
            case 2: {                     /* need other axis to make a couple */
               m = set_info.ax[n].paired;
               if (m != -1) {
                  double	x, y;

                  x = GETGRID( m );
                  y = GETGRID( n );
                  set_info.ax[n].done = 1;
                  set_info.ax[m].done = 1;
                  mode = 3;
                  if (set_info.ax[n].epoco && set_info.ax[n].skysys != set_info.ax[n].skybase) {
                     double epoch1 = set_info.epoch;
                     double epoch2 = 1950.0;
                     double a, d;
                     fint   skysys = 1;

                     r = skypro_c( &x,
                                   &y,
                                   &a,
                                   &d,
                                   &set_info.ax[m].crval,
                                   &set_info.ax[n].crval,
                                   &set_info.ax[m].cdelt,
                                   &set_info.ax[n].cdelt,
                                   &set_info.ax[n].crota,
                                   &set_info.ax[n].skybase,
                                   &set_info.ax[n].skybase,
                                   &set_info.ax[n].prosys,
                                   &mode );
                     epoco_c( &a,
                              &d,
                              &epoch1,
                              &a,
                              &d,
                              &epoch2 );
                     r = skyco_c( &a,
                                  &d,
                                  &skysys,
                                  &set_info.ax[m].coord,
                                  &set_info.ax[n].coord,
                                  &set_info.ax[n].skysys );
                  } else {
                     r = skypro_c( &x,
                                   &y,
                                   &set_info.ax[m].coord,
                                   &set_info.ax[n].coord,
                                   &set_info.ax[m].crval,
                                   &set_info.ax[n].crval,
                                   &set_info.ax[m].cdelt,
                                   &set_info.ax[n].cdelt,
                                   &set_info.ax[n].crota,
                                   &set_info.ax[n].skysys,
                                   &set_info.ax[n].skybase,
                                   &set_info.ax[n].prosys,
                                   &mode );
                  }
               } else {                                 /* no matching couple */
                  r = 13;
               }
               break;
            }
            case 3: {                                       /* Frequency axis */
               double	g;

               g = GETGRID( n );
               set_info.ax[n].done = 1;
               if (set_info.ax[n].smask) {                     /* to velocity */
                  fint		dir = 1;

                  r = velpro_c( &g,
                                &set_info.ax[n].coord,
                                &set_info.ax[n].crval,
                                &set_info.ax[n].cdelt,
                                &set_info.ax[n].drval,
                                &set_info.freq0,
                                &set_info.ax[n].velsys,
                                &dir );
               } else {                                       /* to frequency */
                  set_info.ax[n].coord = set_info.ax[n].crval +
                     set_info.ax[n].cdelt * g;
               }
               break;
            }
            default: {          /* All other axis get standard transformation */
               double	g;

               g = GETGRID( n );
               set_info.ax[n].done = 1;
               set_info.ax[n].coord = set_info.ax[n].crval +
                  set_info.ax[n].cdelt * g;
               break;
            }
         }
      }
   }
   return( r );
}

static fint get_grids( set_struct set_info )
/*
 * This routine does the transform from physical coordinates to grids.
 * The units of the physical coordinates must be natural units.
 */
{
   fint m, mode, n, r = 0;

   for (n = 0; !r && n < set_info.maxis; n++) {
      if (!set_info.ax[n].done) {
         switch(set_info.ax[n].axtype) {
            case 1: {                     /* need other axis to make a couple */
               m = set_info.ax[n].paired;
               if (m != -1) {
                  fint		done = set_info.ax[m].done;

                  set_info.ax[n].done = 1;
                  set_info.ax[m].done = 1;
                  if (set_info.ax[m].def || done) {
                     mode = 2;
                     if (set_info.ax[n].epoco) {
                        r = 15;         /* epoch transformations not possible */
                     } else {
                        double	y;

                        y = GETGRID( m );
                        r = skypro_c( &set_info.ax[n].coord,
                                      &y,
                                      &set_info.ax[n].grid,
                                      &set_info.ax[m].coord,
                                      &set_info.ax[n].crval,
                                      &set_info.ax[m].crval,
                                      &set_info.ax[n].cdelt,
                                      &set_info.ax[m].cdelt,
                                      &set_info.ax[m].crota,
                                      &set_info.ax[n].skysys,
                                      &set_info.ax[n].skybase,
                                      &set_info.ax[n].prosys,
                                      &mode );
                        SETGRID( n );
                     }
                  } else if (set_info.ax[n].skysys != set_info.ax[m].skysys) {
                     r = 14;                      /* incompatible sky systems */
                  } else {
                     mode = 0;
                     if (set_info.ax[n].epoco && set_info.ax[n].skysys != set_info.ax[n].skybase) {
                        double epoch1 = 1950.0;
                        double epoch2 = set_info.epoch;
                        double a, d;
                        fint   skysys = 1;

                        r = skyco_c( &set_info.ax[n].coord,
                                     &set_info.ax[m].coord,
                                     &set_info.ax[n].skysys,
                                     &a,
                                     &d,
                                     &skysys );
                        epoco_c( &a,
                                 &d,
                                 &epoch1,
                                 &a,
                                 &d,
                                 &epoch2 );
                        r = skypro_c( &a,
                                      &d,
                                      &set_info.ax[n].grid,
                                      &set_info.ax[m].grid,
                                      &set_info.ax[n].crval,
                                      &set_info.ax[m].crval,
                                      &set_info.ax[n].cdelt,
                                      &set_info.ax[m].cdelt,
                                      &set_info.ax[m].crota,
                                      &set_info.ax[n].skybase,
                                      &set_info.ax[n].skybase,
                                      &set_info.ax[n].prosys,
                                      &mode );
                        SETGRID( n );
                        SETGRID( m );
                     } else {
                        r = skypro_c( &set_info.ax[n].coord,
                                      &set_info.ax[m].coord,
                                      &set_info.ax[n].grid,
                                      &set_info.ax[m].grid,
                                      &set_info.ax[n].crval,
                                      &set_info.ax[m].crval,
                                      &set_info.ax[n].cdelt,
                                      &set_info.ax[m].cdelt,
                                      &set_info.ax[m].crota,
                                      &set_info.ax[n].skysys,
                                      &set_info.ax[n].skybase,
                                      &set_info.ax[n].prosys,
                                      &mode );
                        SETGRID( n );
                        SETGRID( m );
                     }
                  }
               } else {                                 /* no matching couple */
                  r = 13;
               }
               break;
            }
            case 2: {                     /* need other axis to make a couple */
               m = set_info.ax[n].paired;
               if (m != -1) {
                  fint		done = set_info.ax[m].done;

                  set_info.ax[n].done = 1;
                  set_info.ax[m].done = 1;
                  if (set_info.ax[m].def || done) {
                     mode = 1;
                     if (set_info.ax[n].epoco) {
                        r = 15;         /* epoch transformations not possible */
                     } else {
                        double	x;

                        x = GETGRID( m );
                        r = skypro_c( &x,
                                      &set_info.ax[n].coord,
                                      &set_info.ax[m].coord,
                                      &set_info.ax[n].grid,
                                      &set_info.ax[m].crval,
                                      &set_info.ax[n].crval,
                                      &set_info.ax[m].cdelt,
                                      &set_info.ax[n].cdelt,
                                      &set_info.ax[n].crota,
                                      &set_info.ax[n].skysys,
                                      &set_info.ax[n].skybase,
                                      &set_info.ax[n].prosys,
                                      &mode );
                        SETGRID( n );
                     }
                  } else if (set_info.ax[n].skysys != set_info.ax[m].skysys) {
                     r = 14;                      /* incompatible sky systems */
                  } else {
                     mode = 0;
                     if (set_info.ax[n].epoco && set_info.ax[n].skysys != set_info.ax[n].skybase) {
                        double epoch1 = 1950.0;
                        double epoch2 = set_info.epoch;
                        double a, d;
                        fint   skysys = 1;

                        r = skyco_c( &set_info.ax[m].coord,
                                     &set_info.ax[n].coord,
                                     &set_info.ax[n].skysys,
                                     &a,
                                     &d,
                                     &skysys );
                        epoco_c( &a,
                                 &d,
                                 &epoch1,
                                 &a,
                                 &d,
                                 &epoch2 );
                        r = skypro_c( &a,
                                      &d,
                                      &set_info.ax[m].grid,
                                      &set_info.ax[n].grid,
                                      &set_info.ax[m].crval,
                                      &set_info.ax[n].crval,
                                      &set_info.ax[m].cdelt,
                                      &set_info.ax[n].cdelt,
                                      &set_info.ax[n].crota,
                                      &set_info.ax[n].skybase,
                                      &set_info.ax[n].skybase,
                                      &set_info.ax[n].prosys,
                                      &mode );
                        SETGRID( m );
                        SETGRID( n );
                     } else {
                        r = skypro_c( &set_info.ax[m].coord,
                                      &set_info.ax[n].coord,
                                      &set_info.ax[m].grid,
                                      &set_info.ax[n].grid,
                                      &set_info.ax[m].crval,
                                      &set_info.ax[n].crval,
                                      &set_info.ax[m].cdelt,
                                      &set_info.ax[n].cdelt,
                                      &set_info.ax[n].crota,
                                      &set_info.ax[n].skysys,
                                      &set_info.ax[n].skybase,
                                      &set_info.ax[n].prosys,
                                      &mode );
                        SETGRID( m );
                        SETGRID( n );
                     }
                  }
               } else {                                 /* no matching couple */
                  r = 13;
               }
               break;
            }
            case 3: {                                       /* Frequency axis */
               set_info.ax[n].done = 1;
               if (set_info.ax[n].smask) {                   /* from velocity */
                  fint dir = 0;
                  r = velpro_c( &set_info.ax[n].coord,
                                &set_info.ax[n].grid,
                                &set_info.ax[n].crval,
                                &set_info.ax[n].cdelt,
                                &set_info.ax[n].drval,
                                &set_info.freq0,
                                &set_info.ax[n].velsys,
                                &dir );
                  SETGRID( n );
               } else {                                     /* from frequency */
                  set_info.ax[n].grid = (set_info.ax[n].coord -
                     set_info.ax[n].crval) / set_info.ax[n].cdelt;
                  SETGRID( n );
               }
               break;
            }
            default: {          /* All other axis get standard transformation */
               set_info.ax[n].done = 1;
               set_info.ax[n].grid = (set_info.ax[n].coord -
                  set_info.ax[n].crval) / set_info.ax[n].cdelt;
               SETGRID( n );
               break;
            }
         }
      }
      set_info.ax[n].skysys = set_info.ax[n].skybase;	/* reset */
   }
   return( r );
}


/*
#>            cotrans.dc2

Function:     COTRANS

Purpose:      Transformation from grid coordinates to physical coordinates
              and vice versa.

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Use:          INTEGER COTRANS ( SET    ,  Input   character
                                SUBSET ,  Input   integer*8
                                COORD1 ,  Input   double precision array
                                COORD2 ,  Output  double precision array
                                DIR    )  Input   integer

              COTRANS    Returns:
                          0 transformation successful
                          1 unknown projection
                          2 unknown mode
                          3 CROTA2 = 90.0 for mode 1 and 2
                          4 CDELT1 and/or CDELT2 equal to zero
                          5 input sky system unknown
                          6 output sky system unknown
                          7 input and output sky system unknown
                          8 skypro error
                          9 unknown velocity system
                         10 rest frequency less than or equal to zero
                         11 crval equal to zero
                         12 cdelt equal to zero
                         13 no matching axis pair found
                         14 incompatible sky systems
                         15 cannot do epoch transformations
              SET        Name of data set.
              SUBSET     Subset coordinate word.
              COORD1     Array containing grid coordinates or physical
                         coordinates in substructure defined by SUBSET.
                         The number of coordinates must be equal to the
                         dimension of SUBSET.
              COORD2     Array containing the corresponding physical
                         coordinates or grid coordinates of the coordinates
                         specified in COORD1 and by SUBSET. The units of the
                         physical coordinates is converted to the units
                         as defined by CUNIT and DUNIT in the set header.
                         The number of coordinates returned in COORD2
                         can be obtained via NCOORDS (see NCOORDS.DC2).
                         This number can be larger than the number of
                         axes in the set because hidden axes coordinates
                         are also returned. The order or coordinates
                         returned in COORD2 is the axis order, so the
                         coordinate of the first axis is in COORD2(1),
                         the coordinate of the secoind axis is in COORD2(2)
                         etc.
              DIR        Direction of transformation:
                         DIR == 0:  physical coordinates -> grid coordinates
                         DIR != 0:  grid coordinates -> physical coordinates

Related Docs: ncoords.dc2

Updates:      Dec 19, 1989: KGB, Document created.
              Nov 22, 1991: KGB, Hidden axes implemented.
              Apr  9, 1992: KGB, Returns also hidden coordinates.
              May 18, 1992: KGB, Bug in looking for matching axes repaired.
              Nov  4, 1996: KGB, Epoch can be in single precision.
              Apr 09, 2009: VOG, Replaced NINT definition with one that
                                 uses floor(). Several other routines
                                 dealing with coordinates now use the
                                 same definition. These updated routines can deal
                                 properly with CRPIX values that end on 0.5
                                 (tested for both negative and positive CRPIX).

#<

Fortran to C interface:

@ integer function cotrans( character        ,
@                           integer*8          ,
@                           double precision ,
@                           double precision ,
@                           integer          )

*/

fint cotrans_c( fchar   set      ,
                fint8   *subset   ,
                double *coordin  ,
                double *coordout ,
                fint   *dir      )
{
   fint       r = 0;
   fint       m;
   fint       n;
   set_struct set_info;

   /*
    * Here starts the real hard part.
    */
   if (*dir) {              /* from grid coordinates to phycsical coordinates */
      set_info = get_set_info( set, *subset, 1 );             /* get set info */
      m = 0;
      for (n = 0; n < set_info.naxis; n++) {
         if (!set_info.ax[n].def) {
            set_info.ax[n].grid = coordin[m++];
            set_info.ax[n].done = 0;
         }
      }
      r = get_coords( set_info );
      for (n = 0; n < set_info.maxis; n++) {
         if (set_info.ax[n].smask) {
            coordout[n] = set_info.ax[n].coord / set_info.ax[n].dfact;
         } else {
            coordout[n] = set_info.ax[n].coord / set_info.ax[n].cfact;
         }
      }
   } else {                  /* from physical coordinates to grid coordinates */
      set_info = get_set_info( set, *subset, 0 );             /* get set info */
      m = 0;
      for (n = 0; n < set_info.naxis; n++) {
         if (!set_info.ax[n].def) {
            if (set_info.ax[n].smask) {
               set_info.ax[n].coord = coordin[m++] * set_info.ax[n].dfact;
            } else {
               set_info.ax[n].coord = coordin[m++] * set_info.ax[n].cfact;
            }
            set_info.ax[n].done = 0;
         }
      }
      r = get_grids( set_info );
      for (n = 0; n < set_info.maxis; n++) {
         coordout[n] = set_info.ax[n].grid;
      }
   }
#if	defined(TESTBED)
   {
      int	m;

      for ( m = 0; m < set_info.maxis; m++) {
         char	mes[80];
         fint	pmask = set_info.ax[m].pmask;
         fint	smask = set_info.ax[m].smask;
         fint	o = 0;

         if (pmask & 32) {
            sprintf( mes, "CDELT%d = %f", m + 1, set_info.ax[m].cdelt );
            anyout_c( &o, tofchar( mes ) );
         }
         if (pmask & 16) {
            sprintf( mes, "CROTA%d = %f", m + 1, set_info.ax[m].crota );
            anyout_c( &o, tofchar( mes ) );
         }
         if (pmask & 8) {
            sprintf( mes, "CRPIX%d = %f", m + 1, set_info.ax[m].crpix );
            anyout_c( &o, tofchar( mes ) );
         }
         if (pmask & 4) {
            sprintf( mes, "CRVAL%d = %f", m + 1, set_info.ax[m].crval );
            anyout_c( &o, tofchar( mes ) );
         }
         if (pmask & 2) {
            sprintf( mes, "CUNIT%d = %.*s", m + 1, MAXNAMLEN, set_info.ax[m].cunit );
            anyout_c( &o, tofchar( mes ) );
         }
         if (pmask & 1) {
            sprintf( mes, "CTYPE%d = %.*s", m + 1, MAXNAMLEN, set_info.ax[m].ctype );
            anyout_c( &o, tofchar( mes ) );
         }
         if (smask & 32) {
            sprintf( mes, "CDELT%d = %f", m + 1, set_info.ax[m].ddelt );
            anyout_c( &o, tofchar( mes ) );
         }
         if (smask & 16) {
            sprintf( mes, "CROTA%d = %f", m + 1, set_info.ax[m].drota );
            anyout_c( &o, tofchar( mes ) );
         }
         if (smask & 8) {
            sprintf( mes, "CRPIX%d = %f", m + 1, set_info.ax[m].drpix );
            anyout_c( &o, tofchar( mes ) );
         }
         if (smask & 4) {
            sprintf( mes, "CRVAL%d = %f", m + 1, set_info.ax[m].drval );
            anyout_c( &o, tofchar( mes ) );
         }
         if (smask & 2) {
            sprintf( mes, "CUNIT%d = %.*s", m + 1, MAXNAMLEN, set_info.ax[m].dunit );
            anyout_c( &o, tofchar( mes ) );
         }
         if (smask & 1) {
            sprintf( mes, "CTYPE%d = %.*s", m + 1, MAXNAMLEN, set_info.ax[m].dtype );
            anyout_c( &o, tofchar( mes ) );
         }
         sprintf( mes, "GRID%d  = %f", m + 1, set_info.ax[m].grid );
         anyout_c( &o, tofchar( mes ) );
         sprintf( mes, "COORD%d = %f", m + 1, set_info.ax[m].coord );
         anyout_c( &o, tofchar( mes ) );
      }
   }
#endif
   return(r);
}


/*
#>            grtoph.dc2

Function:     GRTOPH

Purpose:      Transformation from grid coordinates to physical coordinates
              in a subset.

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Use:          INTEGER GRTOPH ( SET    ,  Input   character
                               SUBSET ,  Input   integer
                               COORD1 ,  Input   double precision array
                               COORD2 )  Output  double precision array

              GRTOPH      Returns:
                          0 transformation successful
                          1 unknown projection
                          2 unknown mode
                          3 CROTA2 = 90.0 for mode 1 and 2
                          4 CDELT1 and/or CDELT2 equal to zero
                          5 input sky system unknown
                          6 output sky system unknown
                          7 input and output sky system unknown
                          8 skypro error
                          9 unknown velocity system
                         10 rest frequency less than or equal to zero
                         11 crval equal to zero
                         12 cdelt equal to zero
                         13 no matching axis pair found
                         14 incompatible sky systems
                         15 cannot do epoch transformations
              SET        Name of data set.
              SUBSET     Subset coordinate word.
              COORD1     Array containing grid coordinates in substructure
                         defined by SUBSET. The number of coordinates must
                         be equal to the dimension of SUBSET.
              COORD2     Array containing the corresponding physical
                         coordinates of the coordinates specified in COORD1.
                         The units of the physical coordinates is converted
                         to the units as defined by CUNIT and DUNIT in the
                         set header.

Related Docs: cotrans.dc2

Updates:      Sep 13, 1995: KGB, Document Created.

#<

Fortran to C interface:

@ integer function grtoph( character        ,
@                          integer          ,
@                          double precision ,
@                          double precision )

*/

fint grtoph_c( fchar   set      ,
               fint   *subset   ,
               double *coordin  ,
               double *coordout )
{
   fint       r = 0;
   fint       m;
   fint       n;
   set_struct set_info;

   /*
    * Here starts the real hard part.
    */
   set_info = get_set_info( set, *subset, 1 );             /* get set info */
   for (m = n = 0; n < set_info.naxis; n++) {
      if (!set_info.ax[n].def) {
         set_info.ax[n].grid = coordin[m++];
         set_info.ax[n].done = 0;
      }
   }
   r = get_coords( set_info );
   for (m = n = 0; n < set_info.naxis; n++) {
      if (!set_info.ax[n].def) {
         if (set_info.ax[n].smask) {
            coordout[m++] = set_info.ax[n].coord / set_info.ax[n].dfact;
         } else {
            coordout[m++] = set_info.ax[n].coord / set_info.ax[n].cfact;
         }
      }
   }
   return(r);
}


/*
#>            phtogr.dc2

Function:     PHTOGR

Purpose:      Transformation from pysical coordinates to grid coordinates
              in a subset.

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Use:          INTEGER PHTOGR ( SET    ,  Input   character
                               SUBSET ,  Input   integer
                               COORD1 ,  Input   double precision array
                               COORD2 )  Output  double precision array

              PHTOGR      Returns:
                          0 transformation successful
                          1 unknown projection
                          2 unknown mode
                          3 CROTA2 = 90.0 for mode 1 and 2
                          4 CDELT1 and/or CDELT2 equal to zero
                          5 input sky system unknown
                          6 output sky system unknown
                          7 input and output sky system unknown
                          8 skypro error
                          9 unknown velocity system
                         10 rest frequency less than or equal to zero
                         11 crval equal to zero
                         12 cdelt equal to zero
                         13 no matching axis pair found
                         14 incompatible sky systems
                         15 cannot do epoch transformations
              SET        Name of data set.
              SUBSET     Subset coordinate word.
              COORD1     Array containing the physical coordinates in
                         substructure defined by SUBSET. The number of
                         coordinates must be equal to the dimension of SUBSET.
                         The units of the  physical coordinates should be
                         converted to the units as defined by CUNIT and DUNIT
                         in the set header.
              COORD2     Array containing the corresponding grid coordinates
                         of the coordinates specified in COORD1.

Related Docs: cotrans.dc2

Updates:      Sep 13, 1995: KGB, Document created.

#<

Fortran to C interface:

@ integer function phtogr( character        ,
@                          integer          ,
@                          double precision ,
@                          double precision )

*/

fint phtogr_c( fchar   set      ,
               fint   *subset   ,
               double *coordin  ,
               double *coordout )
{
   fint       r = 0;
   fint       m;
   fint       n;
   set_struct set_info;

   /*
    * Here starts the real hard part.
    */
   set_info = get_set_info( set, *subset, 0 );             /* get set info */
   for (m = n = 0; n < set_info.naxis; n++) {
      if (!set_info.ax[n].def) {
         if (set_info.ax[n].smask) {
            set_info.ax[n].coord = coordin[m++] * set_info.ax[n].dfact;
         } else {
            set_info.ax[n].coord = coordin[m++] * set_info.ax[n].cfact;
         }
         set_info.ax[n].done = 0;
      }
   }
   r = get_grids( set_info );
   for (m = n = 0; n < set_info.maxis; n++) {
      if (!set_info.ax[n].def) {
         coordout[m++] = set_info.ax[n].grid;
      }
   }
   return(r);
}


/*
#>            ncoords.dc2

Function:     NCOORDS

Purpose:      Returns the number of coordinates returned by COTRANS.
              (Including the hidden coordinates.)

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Use:          INTEGER NCOORDS ( SET    )  Input   character

              NCOORDS    Returns:
                          0 An error occurred.
                         >0 Number of coordinates returned by COTRANS.
              SET        Name of data set.

Related Docs: cotrans.dc2

Updates:      Apr  9, 1992: KGB, Document created.

#<

Fortran to C interface:

@ integer function ncoords( character        )

*/

fint ncoords_c( fchar   set      )
{
   fint		r = 0;
   set_struct	set_info;

   set_info = get_set_info( set, 0, 1 );	/* get set info */
   r = set_info.maxis;
   return( r );
}

/*
#>            skyrot.dc2

Function:     SKYROT

Purpose:      Returns the rotation angle of the sky in a set.

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Use:          INTEGER SKYROT ( SET    ,      Input   character
                               CROTA  )      Output  double precision

              SKYROT     Returns:
                         -1 No sky coordinates found in set.
                          0 No error.
              SET        Name of data set.
              CROTA      Rotation angle of sky.

Related Docs: cotrans.dc2

Updates:      Aug 17, 1995: KGB, Document created.

#<

Fortran to C interface:

@ integer function skyrot( character, double precision )

*/

fint skyrot_c(	fchar	set,
		double	*crota )
{
   fint		n;
   fint		r = -1;
   set_struct	set_info;

   set_info = get_set_info( set, 0, 1 );
   for ( n = 0; n < set_info.maxis && r; n++ ) {
      fint	m = set_info.ax[n].paired;

      if ( m != -1 ) {
         if ( set_info.ax[n].axtype == 1 ) {
            *crota = set_info.ax[m].crota;
            r += 1;
         }
      }
   }
   return( r );
}

typedef	struct {
   fint		i;
   double	d;
} r_struct;

static	r_struct	getskyinfo( fchar set, fint subset, int option )
{
   fint		gds_error = 0;			/* GDS error return */
   fint		m, n, nd, pair[2];
   r_struct	r;
   set_struct	set_info;			/* struct holds set info */

   if ( !tobool( gds_exist_c( set, &gds_error ) ) || ( gds_error < 0 ) ) {
      r.i = -1; return( r );			/* set not present */
   }
   nd = gdsc_ndims_c( set, &subset );
   if ( ( nd < 1 ) || ( nd > 2 ) ) {
      r.i = -2; return( r );			/* not 1D or 2D */
   }
   set_info = get_set_info( set, subset, 1 );
   for ( m = n = 0; n < set_info.maxis; n++ ) {
      if ( !set_info.ax[n].def ) pair[m++] = n;
   }
   if ( m != nd ) { r.i = -2; return( r ); }
   if ( set_info.ax[pair[0]].paired == -1 ) {
      r.i = -3; return( r );
   }
   if ( m == 1 ) pair[1] = set_info.ax[pair[0]].paired;
   switch( option ) {
      case 1: {
         r.i = set_info.ax[pair[0]].skysys;
         break;
      }
      case 2: {
         r.i = set_info.ax[pair[0]].prosys;
         break;
      }
      case 3: {
         r.i = 0;
         if ( set_info.ax[pair[0]].axtype == 2 ) {
            r.d = set_info.ax[pair[0]].crota;
         } else {
            r.d = set_info.ax[pair[1]].crota;
         }
         break;
      }
      default: {
         r.i = -4;
         break;
      }
   }
   return( r );
}

/*
#>            getsky.dc2

Function:     GETSKY

Purpose:      Returns the type of sky system of a subset.

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Use:          INTEGER GETSKY ( SET    ,      Input   character
                               SUBSET )      Input   integer

              GETSKY     Returns:
                         -4 Internal error
                         -3 No sky coordinates found in set.
                         -2 Error in subset dimensions.
                         -1 Set does not exist. 
                          1 Equatorial
                          2 Galactic
                          3 Ecliptic
                          4 Supergalactic.
              SET        Name of data set.
              SUBSET     Subset coordinate word.

Related Docs: cotrans.dc2

Updates:      May 21, 1996: KGB, Document created.

#<

Fortran to C interface:

@ integer function getsky( character, integer )

*/

fint	getsky_c( fchar set, fint *subset )
{
   r_struct	r;

   r = getskyinfo( set, *subset, 1 );
   return( r.i );
}

/*
#>            getpro.dc2

Function:     GETPRO

Purpose:      Returns the type of sky projection of a subset.

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Use:          INTEGER GETPRO ( SET    ,      Input   character
                               SUBSET )      Input   integer

              GETSKY     Returns:
                         -4 Internal error
                         -3 No sky coordinates found in set.
                         -2 Error in subset dimensions.
                         -1 Set does not exist. 
                          1 AITOFF equal area
                          2 Equivalent Cylindrical
                          3 Flat
                          4 Gnomonic
                          5 Orthographic
                          6 Rectangular
                          7 Global Sinusoidal
                          8 North Celestial Pole (WSRT)
                          9 Stereographic
                         10 Mercator
              SET        Name of data set.
              SUBSET     Subset coordinate word.

Related Docs: cotrans.dc2

Updates:      May 21, 1996: KGB, Document created.

#<

Fortran to C interface:

@ integer function getpro( character, integer )

*/

fint	getpro_c( fchar set, fint *subset )
{
   r_struct	r;

   r = getskyinfo( set, *subset, 2 );
   return( r.i );
}

/*
#>            getrot.dc2

Function:     GETROT

Purpose:      Returns the rotation angle of a subset.

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Use:          INTEGER GETROT ( SET    ,      Input   character
                               SUBSET ,      Input   integer
                               CROTA  )      Output  double precision

              GETROT     Returns:
                         -4 Internal error
                         -3 No sky coordinates found in set.
                         -2 Error in subset dimensions.
                         -1 Set does not exist. 
                          0 No error
              SET        Name of data set.
              SUBSET     Subset coordinate word.
              CROTA      The rotation angle of the subset.

Related Docs: cotrans.dc2

Updates:      May 21, 1996: KGB, Document created.

#<

Fortran to C interface:

@ integer function getrot( character, integer, double precision )

*/

fint	getrot_c( fchar set, fint *subset, double *crota )
{
   r_struct	r;

   r = getskyinfo( set, *subset, 3 );
   if ( r.i == 0 ) { *crota = r.d; }
   return( r.i );
}

/*
#>            axunit.dc2

Function:     AXUNIT

Purpose:      Returns the units of the physical coordinates of an
              axis in a set.

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Declaration:  INTEGER AXUNIT

Use:          INTEGER AXUNIT( SET   ,  Input    character*(*)
                              AXNUM ,  Input    integer
                              CUNIT )  Output   character*(*)

              AXUNIT  Returns:
                      0 successful return of axis units.
                      1 a cotrans error occured.
                      2 axis not present in set.
                      3 output character string not large enough.
              SET     Name of GDS database.
              AXNUM   Axis number for which the units should be returned.
              CUNIT   The physical units of the axis (as returned by
                      cotrans).

Updates:      Jan  3, 1990: KGB Original document.

#<

Fortran to C interface:

@ integer function axunit( character, integer , character )

*/

fint axunit_c( fchar set, fint *axnum, fchar cunit )
{
   fchar      unit;                       /* fortran character points to unit */
   fint       l;                              /* number of characters in unit */
   fint       n;                                                   /* counter */
   set_struct set_info;                              /* struct holds set info */

   for (n = 0; n < cunit.l; cunit.a[n++] = ' ');                     /* reset */
   set_info = get_set_info( set, 0, 0 );
   if (set_info.err) return( 1 );
   if (set_info.maxis < *axnum) return( 2 );                /* axis not found */
   if (set_info.ax[*axnum-1].smask) {                       /* secondary axis */
      unit.a = set_info.ax[*axnum-1].dunit;                         /* DUNITS */
   } else {
      unit.a = set_info.ax[*axnum-1].cunit;                         /* CUNITS */
   }
   unit.l = MAXNAMLEN;                          /* length of internal buffers */
   l = nelc_c( unit );                   /* length of output character string */
   if (l > cunit.l) return( 3 );           /* not enough space to return unit */
   for (n = 0; n < l; n++) cunit.a[n] = unit.a[n];               /* copy unit */
   return( 0 );                                                /* all is well */
}


/*
#>            axcoord.dc2

Function:     AXCOORD

Purpose:      Returns the coordinate type and units as returned by cotrans.

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Declaration:  INTEGER AXCOORD

Use:          INTEGER AXCOORD( SET   ,  Input    CHARACTER*(*)
                               AXNUM ,  Input    INTEGER
                               TYPE  ,  Output   CHARACTER*(*)
                               UNIT  ,  Output   CHARACTER*(*)
                               COLEV )  Output   INTEGER

              AXCOORD  Returns:
                       0 successful return of axis units.
                       1 a cotrans error occured.
                       2 axis not present in set.
                       3 output character string not large enough.
              SET      Name of GDS database.
              AXNUM    Axis number for which the units should be returned.
              TYPE     The axis name (type) as used by cotrans).
              UNIT     The physical units of the axis (as returned by
                       cotrans).
              COLEV    Returns 1 when primary axis was used, 2 when
                       secondary axis was used.

Updates:      Feb 18, 1991: KGB Original document.

#<

Fortran to C interface:

@ integer function axcoord( character, integer , character, character, integer )

*/

fint axcoord_c( fchar set, fint *axnum, fchar ctype, fchar cunit, fint *colev )
{
   fchar      type;
   fchar      unit;                       /* fortran character points to unit */
   fint       l;                              /* number of characters in unit */
   fint       n;                                                   /* counter */
   set_struct set_info;                              /* struct holds set info */

   for (n = 0; n < ctype.l; ctype.a[n++] = ' ');                     /* reset */
   for (n = 0; n < cunit.l; cunit.a[n++] = ' ');                     /* reset */
   set_info = get_set_info( set, 0, 0 );
   if (set_info.err) return( 1 );
   if (set_info.maxis < *axnum) return( 2 );                /* axis not found */
   if (set_info.ax[*axnum-1].smask) {                       /* secondary axis */
      (*colev) = 2;
      type.a = set_info.ax[*axnum-1].dtype;
      unit.a = set_info.ax[*axnum-1].dunit;                         /* DUNITS */
   } else {
      (*colev) = 1;
      type.a = set_info.ax[*axnum-1].ctype;
      unit.a = set_info.ax[*axnum-1].cunit;                         /* CUNITS */
   }
   type.l = MAXNAMLEN;
   unit.l = MAXNAMLEN;                          /* length of internal buffers */
   l = nelc_c( type );
   if (l > ctype.l) return( 3 );
   for (n = 0; n < l; n++) ctype.a[n] = type.a[n];
   l = nelc_c( unit );                   /* length of output character string */
   if (l > cunit.l) return( 3 );           /* not enough space to return unit */
   for (n = 0; n < l; n++) cunit.a[n] = unit.a[n];               /* copy unit */
   return( 0 );                                                /* all is well */
}


/*
 * atod converts a string to a double precision float.
 * It uses dcddble.
 */

static	double	atod( char *string, fint *dcderr )
{
   double	outv = 0.0;			/* return value */
   fint		nout = 1;			/* number of values */
   fint		ierh;				/* error return */

   (void) dcddble_c( tofchar( string ), &outv, &nout, &ierh );
   if (ierh) {
      outv = 0.0;
      (*dcderr) = -10;				/* dcd error */
   } else if (tobool(dblank_c( &outv ) ) ) {
      outv = 0.0;
      (*dcderr) = -11;				/* dcd error */
   }
   return( outv );
}

/*
#>            dcdpos.dc3

Function:     DCDPOS

Purpose:      The function dcdpos decodes position input for boxinp
              and posinp.

Category:     PHYSICAL COORDINATES

File:         cotrans.c

Author:       K.G. Begeman

Use:          INTEGER DCDPOS( SET    ,   input    character*(*)
                              SUBSET ,   input    integer
                              STRING ,   input    character*(*)
                              POS    ,   output   double precision array
                              OPTION )   input    integer

              DCDPOS      Depends on option. If OPTION greater than 0
                          (POSINP) DCDPOS returns the number of positions
                          decoded. If OPTION equals 0 (BOXINP) then DCDPOS
                          returns a coded number which can have the following
                          values:
                           0 - no position found in string
                           1 - one position found in string
                           2 - only a size found in string
                           3 - position and size found in string
                           4 - two positions found in string
                          In all case, a negative return value indicates
                          an error. The following errors might occur:
                          -1  - illegal use of 'PC', 'AC' or 'D'
                          -2  - prefix incompatible with axis
                          -3  - position incomplete
                          -4  - error reading descriptor info
                          -5  - 'D' not allowed for positions
                          -6  - No grid separation defined for units
                          -7  - Too many positions
                          -8  - Cannot obtain header information
                          -9  - No mixed epochs allowed
                          -10 - General decode error (detected by dcddble).
                          -11 - BLANKS decoded.
              SET         Input set name.
              SUBSET      Input subset coordinate word. Needed to
                          determine the dimension of the subset.
              STRING      String containing input to be decoded.
              POS         The decoded positions. The positions are stored
                          sequentially, meaning that the first N items
                          contain the first position (N is dimension of
                          subset), the second N items the second, etc.
              OPTION      If OPTION equals zero, then BOX decoding will
                          be done. If OPTION is greater than 0 then
                          position decoding will be done, where OPTION
                          indicated the maximum number of positions to
                          decode.

Updates:      Jan 18, 1990: KGB, Document created.
              Feb 17, 1992: KGB, Bug in trailing units repaired.
              Mar 27, 1995: KGB, Errors -10 and -11 implemented.

#<

Fortran to C interface:

@ integer function dcdpos( character        ,
@                          integer          ,
@                          character        ,
@                          double precision ,
@                          integer          )

*/

static char *syntax[] = {
   "PC",                                /* projection centre */
   "AC",                                /* axis centre */
   "*",                                 /* Equatorial (hms or dms) */
   "*1950",                             /* Equatorial 1950.0 (hms or dms) */
   "*2000",                             /* Equatorial 2000.0 (hms or dms) */
   "G",                                 /* Galactic (degrees) */
   "E",                                 /* Ecliptic (degrees) */
   "S",                                 /* Supergalactic (degrees) */
   "D",                                 /* size of box */
   "U"                                  /* in physical units without postfix */
};

#define	MAXSYNTAX	(sizeof(syntax)/sizeof(char *))

fint dcdpos_c( fchar   set    ,
               fint8   *subset ,
               fchar   string ,
               double *pos    ,
               fint   *option )
{
   fint        axnum;                              /* sequence number of axis */
   fint        dcderr = 0;                        /* internal error indicator */
   fint        done;       /* do we have to do the coordinate transformations */
   fint        i;
   fint        l;
   fint        m;
   fint        n;
   fint        np;
   fint        npos;
   fint       *perm;                /* array contains axis sequence in subset */
   fint        r = 0;                                         /* return value */
   fint        subdim;
   fint       *type;                             /* contains skysys of values */
   char       *buf;             /* buffer for temporary storage of input text */
   char       *sep = " ,";                                /* token separators */
   char       *sub;
   set_struct  set_info;

   l = nelc_c( string );                              /* length of input text */
   if (!l) return( 0 );                   /* empty string, so simple solution */
   subdim = gdsc_ndims_c( set, subset );               /* dimension of subset */
   perm = (fint *) calloc( subdim, sizeof( fint ) ); /* buffer for perm array */
   type = (fint *) calloc( subdim, sizeof( fint ) ); /* buffer for type array */
   m = 0;                                                            /* reset */
   set_info = get_set_info( set, *subset, 0 );                /* get set info */
   if (set_info.err) return( -8 );                           /* cotrans error */
   for (n = 0; n < set_info.naxis; n++) {                /* permutation array */
      if (!set_info.ax[n].def) perm[m++] = n;
   }
   buf = calloc( l + 1, sizeof( char ) );            /* buffer for input text */
   for (n = 0; n < l; n++) buf[n] = toupper( string.a[n] );   /* to UPPERCASE */
   buf[n] = 0;					/* add zero byte */
   sub = strtok( buf, sep );                               /* get first token */
   if (*option) npos = *option; else npos = 2;            /* POSINP or BOXINP */
   np = 0;                                          /* reset position counter */
   while (np < npos && !dcderr) {     /* loop until no more space left in pos */
      double epoch;                                      /* epoch set by user */
      fint   nepoch = 0;                            /* number of epochs given */

      done = 0;                   /* no transformations done on next position */
      for (n = 0; n < subdim; n++) {
         type[n] = set_info.ax[perm[n]].done = 0;
      }
      i = 0;                                        /* reset position pointer */
      while (i < subdim && !dcderr) {          /* loop to decode one position */
         fint ns = 0;                                /* indicates prefix type */

         if (sub != NULL) {             	/* still something to decode */
            fint d;

            do {                                      /* is it a known prefix */
               d = strcmp( sub, syntax[ns++] );
            } while ((d) && (ns < MAXSYNTAX));
            if (d) ns = 0;                                       /* no prefix */
            if (!ns && sub[0] == '*') {                   /* user wants epoch */
               double ep = atod( &sub[1], &dcderr );             /* get epoch */

               if (dcderr) break;			     /* skip the rest */
               if (ep == 2000.0) {
                  ns = 5;                            /* epoch 2000.0 is known */
               } else if (ep == 1950.0) {
                  ns = 4;                            /* epoch 1950.0 is known */
               } else if (nepoch && ep != epoch) {
                  dcderr = -9; break;                      /* no mixed epochs */
               } else {
                  ns = 3;                                       /* equatorial */
                  if (!nepoch++) epoch = ep;
                  type[i] = 6;                                    /* set code */
               }
            }
            switch(ns) {                                   /* what to do next */
               case 1: {                                 /* projection centre */
                  if (i) { dcderr = -1; break; }   /* illegal use of PC or AC */
                  do {
                     fint n = perm[i];                    /* this axis number */

                     set_info.ax[n].grid = set_info.ax[n].offset;
                     set_info.ax[n].done = 1;         /* no cotrans necessary */
                  } while (++i < subdim);
                  sub = strtok( NULL, sep );                /* get next token */
                  break;
               }
               case 2: {                                       /* axis centre */
                  if (i) { dcderr = -1; break; }   /* illegal use of PC or AC */
                  do {
                     fint n = perm[i];                     /* get axis number */

                     set_info.ax[n].grid = 0.5 * ( set_info.ax[n].naxis + 1.0 ) - set_info.ax[n].crpix + set_info.ax[n].offset;
                     set_info.ax[n].done = 1;         /* no cotrans necessary */
                  } while (++i < subdim);
                  sub = strtok( NULL, sep );                /* get next token */
                  break;
               }
               case 3:                       /* Default Equatorial coordinate */
               case 4:                                   /* Equatorial 1950.0 */
               case 5: {                                 /* Equatorial 2000.0 */
                  double f = 1.0, s, v;
                  fint   skysys;

                  axnum = perm[i++];                       /* get axis number */
                  switch(set_info.ax[axnum].axtype) {   /* get scaling factor */
                     case 1: f = 15.0; break;                  /* hour angles */
                     case 2: f =  1.0; break;                /* circle angles */
                     default: dcderr = -2; break;    /* prefix for wrong axis */
                  }
                  if (dcderr) break;                         /* skip the rest */
                  sub = strtok( NULL, sep );                /* get next token */
                  if (sub == NULL) { dcderr = -3; break; }      /* missing text */
                  if (sub[0] == '-') s = -1.0; else s = 1.0;      /* get sign */
                  v = fabs( atod( sub, &dcderr ) );                /* degrees */
                  if (dcderr) break;			     /* skip the rest */
                  sub = strtok( NULL, sep );                /* get next token */
                  if (sub == NULL) { dcderr = -3; break; }      /* missing text */
                  v += fabs( atod( sub, &dcderr ) ) / 60.0;        /* minutes */
                  if (dcderr) break;			     /* skip the rest */
                  sub = strtok( NULL, sep );                /* get next token */
                  if (sub == NULL) { dcderr = -3; break; }      /* missing text */
                  v += fabs( atod( sub, &dcderr ) ) / 3600.0;      /* seconds */
                  if (dcderr) break;			     /* skip the rest */
                  set_info.ax[axnum].coord = v * f * s;         /* coordinate */
                  switch(ns) {                        /* set input sky system */
                     case 3: {              /* default equatorial coordinates */
                        switch(set_info.ax[axnum].skybase) {
                           case 1: skysys = 1; break;         /* EPOCH 1950.0 */
                           case 5: skysys = 5; break;         /* EPOCH 2000.0 */
                           default: skysys = 1; break;/* default EPOCH 1950.0 */
                        }
                        if (!type[i-1]) type[i-1] = skysys;
                        break;
                     }
                     case 4: {                           /* Equatorial 1950.0 */
                        type[i-1] = skysys = 1;               /* EPOCH 1950.0 */
                        break;
                     }
                     default: {                          /* Equatorial 2000.0 */
                        type[i-1] = skysys = 5;               /* EPOCH 1950.0 */
                        break;
                     }
                  }
                  set_info.ax[axnum].skysys = skysys;       /* set sky system */
                  sub = strtok( NULL, sep );                /* get next token */
                  break;
               }
               case 6:                                            /* Galactic */
               case 7:                                            /* Ecliptic */
               case 8: {                                     /* Supergalactic */
                  axnum = perm[i++];                       /* get axis number */
                  switch(set_info.ax[axnum].axtype) {
                     case 1: break;                              /* longitude */
                     case 2: break;                               /* latitude */
                     default: dcderr = -2; break;                    /* wrong */
                  }
                  if (dcderr) break;                         /* skip the rest */
                  sub = strtok( NULL, sep );                /* get next token */
                  if (sub == NULL) { dcderr = -3; break; }      /* missing text */
                  set_info.ax[axnum].coord = atod( sub, &dcderr ); /* coordinate */
                  if (dcderr) break;			     /* skip the rest */
                  set_info.ax[axnum].skysys = ns - 4;           /* sky system */
                  sub = strtok( NULL, sep );                    /* next token */
                  break;
               }
               case 9: {                         /* Now comes the size of box */
                  if (*option) { dcderr = -5; break; }          /* wrong text */
                  if (i) { dcderr = -1; break; }               /* wrong place */
                  if (!np) { np = 1; r = 2; } else { r = 3; }     /* set code */
                  done = 1;                                     /* no cotrans */
                  sub = strtok( NULL, sep );                /* get next token */
                  do {
                     double s;

                     if (sub == NULL) { dcderr = -3; break; }/* skip the rest */
                     s = fabs( atod( sub, &dcderr ) );    /* this is the size */
                     if (dcderr) break;			     /* skip the rest */
                     sub = strtok( NULL, sep );             /* get next token */
                     if (sub != NULL) {         /* there is something */
                        double f;                  /* factor to natural units */
                        fchar  unit1, unit2;
                        fint   u = 0;                       /* it is a unit ? */

                        axnum = perm[i];                   /* get axis number */
                        unit1.a = sub; unit1.l = strlen( sub );     /* unit ? */
                        if (set_info.ax[axnum].smask) {   /* secondary axis ? */
                           unit2.a = set_info.ax[axnum].sunit;
                           unit2.l = MAXNAMLEN;
                           if (!factor_c( unit1, unit2, &f )) { /* get factor */
                              u = 2;                /* unit of secondary axis */
                              s = s * f;                          /* scale it */
                           }
                        }
                        if (!u) {                         /* try primary axis */
                           unit2.a = set_info.ax[axnum].nunit;
                           unit2.l = MAXNAMLEN;
                           if (!factor_c( unit1, unit2, &f )) { /* get factor */
                              u = 1;                  /* unit of primary axis */
                              s = s * f;                          /* scale it */
                           }
                        }
                        if (u) {                             /* it was a unit */
                           sub = strtok( NULL, sep );       /* get next token */
                           if ((u == 2) && (set_info.ax[axnum].smask & 32)) {
                              s = s / fabs( set_info.ax[axnum].ddelt );
                           } else if (u == 1 && (set_info.ax[axnum].pmask & 32)) {
                              s = s / fabs( set_info.ax[axnum].cdelt );
                           } else {
                              dcderr = -6;
                           }
                        }
                     }
                     if (dcderr) break;                      /* skip the rest */
                     pos[subdim+i] = s;                    /* put in the size */
                  } while (++i < subdim);
                  break;
               }
               case 10: {                        /* now follow physical units */
                  axnum = perm[i++];                       /* get axis number */
                  sub = strtok( NULL, sep );                /* get next token */
                  if (sub == NULL) { dcderr = -3; break; }      /* missing text */
                  set_info.ax[axnum].coord = atod( sub, &dcderr );/* coordinate */
                  if (dcderr) break;			     /* skip the rest */
                  if (set_info.ax[axnum].smask) {         /* to natural units */
                     set_info.ax[axnum].coord *= set_info.ax[axnum].dfact;
                  } else {                                /* to natural units */
                     set_info.ax[axnum].coord *= set_info.ax[axnum].cfact;
                  }
                  sub = strtok( NULL, sep );                /* get next token */
                  break;
               }
               default: {
                  double val;                                        /* value */
                  fint   u = 0;                               /* is it a unit */

                  axnum = perm[i++];                       /* get axis number */
                  val = atod( sub, &dcderr );                    /* get value */
                  if (dcderr) break;			     /* skip the rest */
                  sub = strtok( NULL, sep );                /* get next token */
                  if (sub != NULL) {            /* is it a unit */
                     double f;
                     fchar  unit1, unit2;

                     unit1.a = sub; unit1.l = strlen( sub );
                     if (set_info.ax[axnum].smask) {        /* secondary axis */
                        unit2.a = set_info.ax[axnum].sunit;
                        unit2.l = MAXNAMLEN;
                        if (!factor_c( unit1, unit2, &f )) {    /* get factor */
                           u = 2;                      /* secondary axis unit */
                           val *= f;                              /* scale it */
                        }
                     }
                     if (!u) {                              /* primary axis */
                        unit2.a = set_info.ax[axnum].nunit;
                        unit2.l = MAXNAMLEN;
                        if (!factor_c( unit1, unit2, &f )) {    /* get factor */
                           u = 1;                        /* primary axis unit */
                           val *= f;                              /* scale it */
                        }
                     }
                     if (u) {                /* unit found, so get next token */
                        sub = strtok( NULL, sep );
                     }
                  }
                  if (u) {
                     set_info.ax[axnum].coord = val;
                  } else {
                     set_info.ax[axnum].grid = val;
                     set_info.ax[axnum].done = 1;
                  }
                  break;
               }
            }
         } else if (i) {                  /* end-of-information came too soon */
            dcderr = -2;
         }
      }
      /*
       * Here we check whether we have to do some epoch transformations.
       * Since this can only be done when two coordinates are given in the
       * same epoch, we do it here. By the way, get_grid can not handle
       * this.
       */
      if (nepoch) {
         if (nepoch != 2) dcderr = -9; else {
            double ep = 1950.0;
            fint   p[2];

            for (n = 0; n < subdim; n++) {
               if (type[n] == 6) {
                  p[set_info.ax[perm[n]].axtype-1] = perm[n];
               }
            }
            epoco_c( &set_info.ax[p[0]].coord,
                     &set_info.ax[p[1]].coord,
                     &epoch,
                     &set_info.ax[p[0]].coord,
                     &set_info.ax[p[1]].coord,
                     &ep );
            set_info.ax[p[0]].skysys = set_info.ax[p[1]].skysys = 1;
         }
      }
      if (!dcderr && !done) {                         /* do some transforming */
         if (get_grids( set_info )) {
            dcderr = -4;                                     /* cotrans error */
         } else {
            fint n;

            for (n = 0; n < subdim; n++) {
               pos[np*subdim+n] = set_info.ax[perm[n]].grid;
            }
         }
      }
      if (!dcderr) {
         np += 1;                                       /* added one position */
         if (sub == NULL) break;        /* we have reached the end-of-information */
      }
   }
   if (!dcderr && sub) dcderr = -7;                     /* too many positions */
   if (dcderr) {
      r = dcderr;                                        /* return error code */
   } else if (*option) {
      r = np;                           /* POSINP: return number of positions */
   } else if (!r && np == 1) {
      r = 1;                                   /* BOXINP: return one position */
   } else if (!r && np == 2) {
      r = 4;                                  /* BOXINP: return two positions */
   }
   free( perm ); free(type); free( buf );            /* free allocated memory */
   return( r );                              /* return the code to the caller */
}
