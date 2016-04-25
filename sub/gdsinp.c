/* gdsinp.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

*/

#include	"float.h"		/* <float.h> */
#include	"stddef.h"		/* <stddef.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"ctype.h"		/* <ctype.h> */
#include	"string.h"		/* <string.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include        "math.h"
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"anyout.h"		/* defines anyout_c */
#include	"cancel.h"		/* defines cancel_c */
#include	"error.h"		/* defines error_c */
#include	"dcdint.h"		/* defines dcdint_c */
#include	"reject.h"		/* defines reject_c */
#include	"nelc.h"		/* defines nelc_c */
#include	"userlog.h"		/* defines userlog_c */
#include	"usertext.h"		/* defines usertext_c */
#include	"gds_create.h"		/* defines gds_create_c */
#include	"gds_exist.h"		/* defines gds_exist_c */
#include	"gds_extend.h"		/* defines gds_extend_c */
#include	"gdsc_grid.h"		/* defines gdsc_grid_c */
#include	"gdsc_ndims.h"		/* defines gdsc_ndims_c */
#include	"gdsc_origin.h"		/* defined gdsc_origin_c */
#include	"gdsc_word.h"		/* defines gdsc_word_c */
#include	"gdsd_delete.h"		/* defines gdsd_delete_c */
#include	"gdsd_find.h"		/* defines gdsd_find_c */
#include	"gdsd_length.h"		/* defines gdsd_length_c */
#include	"gdsd_rchar.h"		/* defines gdsd_rchar_c */
#include	"gdsd_rdble.h"		/* defines gdsd_rdble_c */
#include	"gdsd_read.h"		/* defines gdsd_read_c */
#include	"gdsd_rint.h"		/* defines gdsd_rint_c */
#include	"gdsd_wchar.h"		/* defines gdsd_wchar_c */
#include	"gdsd_wdble.h"		/* defines gdsd_wdble_c */
#include	"gdsd_write.h"		/* defines gdsd_write_c */
#include        "swapfint.h"
#include        "gds_itype.h"
#include        "gds_ftype.h"
#include        "gdsa_istable.h"
#include        "gdsa_colinq.h"
#include        "spfpfl.h"
#include        "spfplf.h"
#include        "dpfpfl.h"
#include        "dpfplf.h"

/* Old definition: #define	NINT(x)	( x > 0.0 ? (fint) ( x + 0.5 ) : (fint) ( x - 0.5 ) )*/
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )


#define  MAXFTSDSCLEN     8              /* maximum length of FITS descriptor */
#define  MAXFTSNAMLEN    18         /* maximum length of FITS character value */
#define  MAXKEYLEN       20               /* maximum length of HERMES keyword */
#define  MAXDSCNAMLEN    20         /* maximum length of GDS descriptor names */
#define  MAXSETNAMLEN   256                 /* maximum length of GDS set name */
#define  MAXSTRLEN      132                         /* length of text strings */
#define  MAXBUFLEN      512                    /* length of input text buffer */
#define  MAXDSCBUFLEN  1024             /* size of descriptor transfer buffer */

typedef struct { char *dsc; int len; } key_struct;       /* for reserved keys */
typedef enum   {CHAR, INT, LOG, REAL, DBLE} dsctyp;

static key_struct reserved_key[] = {                 /* list of reserved keys */
   { "EPOCH   ", 8 },
   { "FREQ0   ", 8 },
   { "INSTRUME", 8 },
   { "NAXIS   ", 5 },
   { "CDELT   ", 5 },
   { "CROTA   ", 5 },
   { "CRPIX   ", 5 },
   { "CRVAL   ", 5 },
   { "CTYPE   ", 5 },
   { "CUNIT   ", 5 },
   { "DDELT   ", 5 },
   { "DROTA   ", 5 },
   { "DRPIX   ", 5 },
   { "DRVAL   ", 5 },
   { "DTYPE   ", 5 },
   { "DUNIT   ", 5 }
};

#define MAXRESKEY (sizeof(reserved_key)/sizeof(key_struct))      /* # of keys */

static key_struct special_key[] = {                  /* list of reserved keys */
   { "DATAMIN ", 8 },
   { "DATAMAX ", 8 },
   { "NBLANK  ", 8 }
};

#define MAXSPCKEY (sizeof(special_key)/sizeof(key_struct))       /* # of keys */

typedef struct {               /* struct to hold descriptors to be transfered */
   char dsc[MAXDSCNAMLEN];                              /* name of descriptor */
   fint level1;                                   /* level where to copy from */
   fint level2;                                     /* level where to copy to */
   dsctyp kind;                                     /* type (for tables)      */
} trf_struct;

typedef struct {       /* structure holds descriptors which must be destroyed */
   char dsc[MAXDSCNAMLEN];                              /* name of descriptor */
   fint level;                       /* level where it should be removed from */
} des_struct;

typedef struct {                          /* struct contains axes information */
   fint   naxis;                                         /* length of an axis */
   double cdelt;                    /* increment in physical units along axis */
   double crota;                                    /* rotation angle of axis */
   double crpix;                                   /* reference pixel of axis */
   double crval;                        /* reference value at reference pixel */
   char   ctype[MAXFTSNAMLEN];                                   /* axis name */
   char   cunit[MAXFTSNAMLEN];                               /* units of axis */
   double ddelt;          /* increment in physical units along secondary axis */
   double drota;                          /* secondary rotation angle of axis */
   double drpix;                         /* secondary reference pixel of axis */
   double drval;              /* reference value at secondary reference pixel */
   char   dtype[MAXFTSNAMLEN];                         /* secondary axis name */
   char   dunit[MAXFTSNAMLEN];                     /* secondary units of axis */
   fint   pmask;                                     /* mask for primary axis */
   fint   smask;                                   /* mask for secondary axis */
   fint   def;                             /* axis defined (outside subset) ? */
   fint   npos;                            /* number of grids along this axis */
   fint8  *pos;                       /* array containing the grids along axis */
   fint   low;                                    /* lower grid value on axis */
   fint   upp;                                    /* upper grid value on axis */
} ax_struct;

typedef struct {                           /* struct contains set information */
   fint       exist;                                      /* does set exist ? */
   char       key[MAXKEYLEN];             /* associated GDSINP/GDSOUT keyword */
   char       set[MAXSETNAMLEN];                         /* name of input set */
   fint       naxis;                                 /* number of axes in set */
   fint       maxis;                                 /* number of hidden axes */
   fint       subdim;                                 /* dimensions of subset */
   fint       change;                               /* change flag for GDSOUT */
   fint       ip;                      /* pointer to buffer where copied from */
   fint       rmask;                              /* reserved descriptor mask */
   double     epoch;                            /* epoch of coordinate system */
   double     freq0;                                        /* rest frequency */
   char       instrume[MAXFTSNAMLEN];                       /* for instrument */
   ax_struct *ax;                                     /* for axis information */
   ax_struct *hx;                                          /* for hidden axes */
} set_struct;

typedef struct {                      /* struct contains grids along one axis */
   fint8 *pos;                                                  /* grid values */
   fint  count;                             /* internal counter for gdsc_word */
   fint  npos;                                  /* number of grids along axis */
   fint  axnum;                                        /* axis number of axis */
} pos_struct;

static set_struct *in_buf = NULL;          /* set structure buffer for GDSINP */
static fint        in_buf_size = 0;              /* number of items in in_buf */
static set_struct *out_buf = NULL;         /* set structure buffer for GDSOUT */
static fint        out_buf_size = 0;            /* number of items in out_buf */
static char        buf[MAXBUFLEN+1];                 /* buffer for user input */
static char        dscbuf[MAXFTSDSCLEN+1]; /* buffer for temporary descriptor */
static fint        ftype1, ftype2, itype1, itype2, ierr; /* for convert_type  */

static char	*parse( char *s, const char *ct )
{
   static	char	*parse_p = NULL;
   const	char	*set;
   char			*rs = NULL;
   int			def = 0;

   if ((s == NULL) && (parse_p == NULL)) return( NULL );
   if (s != NULL) parse_p = s;
   while (*parse_p) {
      set = ct;
      while ((*set) && (*parse_p != *set)) set++;
      if (!(*set)) {
         if (!def) { rs = parse_p++; def = 1; } else { parse_p++; }
      } else {
         if (!def) { parse_p++; } else { *parse_p++ = 0; return( rs ); }
      }
   }
   if (!def) return( NULL ); else return( rs );
}

static fchar descr( char *name, fint axnum )
/*
 * This procedure creates a fits descriptor consisting of name plus
 * the axis number attached.
 */
{
   fchar r;
   fint  l;

   sprintf( dscbuf, "%s%d", name, axnum );
   l = strlen( dscbuf );
   while (l < MAXFTSDSCLEN) dscbuf[l++] = ' ';
   r.a = dscbuf; r.l = MAXFTSDSCLEN;
   return( r );
}

static fint reserved_descriptor( fchar descriptor )
/*
 * This function determines when to transfer a descriptor. It compares a
 * descriptor with an internal list (reserved_key) of reserved descriptor
 * names.
 */
{
   fint d;
   fint nkey = 0;

   do {
      d = strncmp( descriptor.a, reserved_key[nkey].dsc, reserved_key[nkey].len );
      nkey += 1;
   } while (d && nkey < MAXRESKEY);
   return( d );
}

static fint special_descriptor( fchar descriptor )
/*
 * This function determines whether a descriptor is special. It compares a
 * descriptor with an internal list (special_key) of special descriptor
 * names.
 */
{
   fint d;
   fint nkey = 0;

   do {
      d = strncmp( descriptor.a, special_key[nkey].dsc, special_key[nkey].len );
      nkey += 1;
   } while (d && nkey < MAXSPCKEY);
   if (d) return( 0 ); else return( 1 );
}

static void add_set_info( set_struct set_info )
/*
 * add_set_info adds the set information of an input set obtained with
 * GDSINP to the input set buffer (in_buf).
 */
{
   fint n;
   fint i = -1;

   for (n = 0; n < in_buf_size; n++) {
      if (!strncmp( set_info.key, in_buf[n].key, MAXKEYLEN )) i = n;
   }
   if (i == -1) {
      i = in_buf_size++;
      in_buf = (set_struct *) realloc( (char *) in_buf,
         sizeof( set_struct ) * in_buf_size );
   } else {
      free( in_buf[i].ax );
   }
   in_buf[i] = set_info;
   in_buf[i].ax = calloc( set_info.naxis, sizeof( ax_struct ) );
   memcpy( in_buf[i].ax, set_info.ax, set_info.naxis * sizeof( ax_struct ) );
}

/*
 * copy_set_info returns a copy of the set info in src.
 */

static set_struct copy_set_info( set_struct src )
{
   int		n;
   set_struct	r;

   r = src;					/* copy the information */
   r.ax = calloc( src.naxis, sizeof( ax_struct ) );
   memcpy( r.ax, src.ax, src.naxis * sizeof( ax_struct ) );
   for (n = 0; n < src.subdim; n++) {		/* for decoding */
      r.ax[n].def = n - src.subdim;
   }
   if (src.maxis) {
      r.hx = calloc( src.maxis, sizeof( ax_struct ) );
      memcpy( r.hx, src.hx, src.maxis * sizeof( ax_struct ) );
   }
   return( r );
}

static set_struct get_set_info( fchar set )
/*
 * get_set_info obtaines axis information for an existing set.
 */
{
   fchar      instrume;
   fint       derror = 0;
   fint       fatal = 4;
   fint       level = 0;
   fint       n;
   fint       setdim;
   set_struct r;

   r.exist = 1;					/* set does exist */
   /*
    * First copy name of set to the set_struct.
    */
   for (n = 0; n < set.l; n++) r.set[n] = set.a[n];
   while (n < MAXSETNAMLEN) r.set[n++] = ' ';
   /*
    * Then read the number of axes in this set.
    */
   gdsd_rint_c( set, tofchar( "NAXIS" ), &level, &setdim, &derror );
   if (derror) error_c( &fatal, tofchar( "GDSXXX <NAXIS not found>" ) );
   r.naxis = setdim;
   /*
    * Next read the reserved FITS descriptors necessary for the coordinate
    * system. When a reserved descriptor is found, the corresponding bit in
    * rmask is set.
    */
   r.rmask = 0;
   gdsd_rdble_c( set, tofchar( "EPOCH" ), &level, &r.epoch, &derror );
   if (derror) { derror = 0; } else { r.rmask |= 1; }
   gdsd_rdble_c( set, tofchar( "FREQ0" ), &level, &r.freq0, &derror );
   if (derror) { derror = 0; } else { r.rmask |= 2; }
   instrume.a = r.instrume; instrume.l = MAXFTSNAMLEN;
   gdsd_rchar_c( set, tofchar( "INSTRUME" ), &level, instrume, &derror );
   if (derror) { derror = 0; } else { r.rmask |= 4; }
   r.ax = (ax_struct *) calloc( setdim, sizeof( ax_struct ) );
   for (n = 0; n < setdim; n++) {
      double cdelt;
      double crota;
      double crpix;
      double crval;
      double ddelt;
      double drota;
      double drpix;
      double drval;
      fchar  ctype;
      fchar  cunit;
      fchar  dtype;
      fchar  dunit;
      fint   axnum = n + 1;
      fint   naxis;
      fint   pmask = 0;
      fint   smask = 0;

      /*
       * Get the length of the axis
       */
      gdsd_rint_c( set, descr( "NAXIS", axnum ), &level, &naxis, &derror );
      if (derror) error_c( &fatal, tofchar( "GDSXXX <NAXIS%% missing>" ) );
      r.ax[n].naxis = naxis;                                /* length of axis */
      /*
       * Get primary and secondary axis units
       */
      cunit.a = r.ax[n].cunit; cunit.l = MAXFTSNAMLEN;
      gdsd_rchar_c( set, descr( "CUNIT", axnum ), &level, cunit, &derror );
      if (derror) derror = 0; else pmask += 1;
      dunit.a = r.ax[n].dunit; dunit.l = MAXFTSNAMLEN;
      gdsd_rchar_c( set, descr( "DUNIT", axnum ), &level, dunit, &derror );
      if (derror) derror = 0; else smask += 1;
      /*
       * Get primary and secondary axis name
       */
      ctype.a = r.ax[n].ctype; ctype.l = MAXFTSNAMLEN;
      gdsd_rchar_c( set, descr( "CTYPE", axnum ), &level, ctype, &derror );
      if (derror) {
         error_c( &fatal, tofchar( "GDSXXX <CTYPE%% missing>" ) );
      } else {
         pmask += 2;
      }
      dtype.a = r.ax[n].dtype; dtype.l = MAXFTSNAMLEN;
      gdsd_rchar_c( set, descr( "DTYPE", axnum ), &level, dtype, &derror );
      if (derror) derror = 0; else smask += 2;
      /*
       * Get primary and secondary reference value
       */
      gdsd_rdble_c( set, descr( "CRVAL", axnum ), &level, &crval, &derror );
      if (derror) derror = 0; else { pmask += 4; r.ax[n].crval = crval; }
      gdsd_rdble_c( set, descr( "DRVAL", axnum ), &level, &drval, &derror );
      if (derror) derror = 0; else { smask += 4; r.ax[n].drval = drval; }
      /*
       * Get primary and secondary reference pixel
       */
      /*
       * Do not read descriptor since it is also stored in binary format.
       */
#if	0
      gdsd_rdble_c( set, descr( "CRPIX", axnum ), &level, &crpix, &derror );
      if (derror == -46) derror = 0;       /* was it a single precision float */
#else
      {
         fint	axnumber = n + 1;

         crpix = gdsc_origin_c( set, &axnumber, &derror );
      }
#endif
      if (derror) {
         error_c( &fatal, tofchar( "GDSXXX <CRPIX%% missing>" ) );
      } else {
         pmask += 8; r.ax[n].crpix = crpix;
      }
      gdsd_rdble_c( set, descr( "DRPIX", axnum ), &level, &drpix, &derror );
      if (derror) derror = 0; else { smask += 8; r.ax[n].drpix = drpix; }
      /*
       * Get primary and secondary rotation angle
       */
      gdsd_rdble_c( set, descr( "CROTA", axnum ), &level, &crota, &derror );
      if (derror) derror = 0; else { pmask += 16; r.ax[n].crota = crota; }
      gdsd_rdble_c( set, descr( "DROTA", axnum ), &level, &drota, &derror );
      if (derror) derror = 0; else { smask += 16; r.ax[n].drota = drota; }
      /*
       * Get primary and secondary unit increment
       */
      gdsd_rdble_c( set, descr( "CDELT", axnum ), &level, &cdelt, &derror );
      if (derror) derror = 0; else { pmask += 32; r.ax[n].cdelt = cdelt; }
      gdsd_rdble_c( set, descr( "DDELT", axnum ), &level, &ddelt, &derror );
      if (derror) derror = 0; else { smask += 32; r.ax[n].ddelt = ddelt; }
      /*
       * Set mask for primary and secondary descriptors
       */
      r.ax[n].pmask = pmask;
      r.ax[n].smask = smask;
      /*
       * Reset definition axis counter (axes outside subset)
       */
      r.ax[n].def = 0;
      /*
       * Reset the positions
       */
      r.ax[n].pos = NULL;
      r.ax[n].npos = 0;
      /*
       * Get grids at beginning and end of axis
       */
      r.ax[n].low = 1 - NINT( crpix );
      r.ax[n].upp = naxis - NINT( crpix );
   }
   /*
    * In the next block the hidden axes are searched for.
    */
   {
      char   ctype_buf[MAXFTSNAMLEN];
      char   cunit_buf[MAXFTSNAMLEN];
      char   dtype_buf[MAXFTSNAMLEN];
      char   dunit_buf[MAXFTSNAMLEN];
      double cdelt;
      double crpix;
      double crota;
      double crval;
      double ddelt;
      double drpix;
      double drota;
      double drval;
      fchar  ctype;
      fchar  cunit;
      fchar  dtype;
      fchar  dunit;
      fint   derror = 0;
      fint   pmask = 0;
      fint   smask = 0;
      fint   axnum = setdim;

      r.maxis = 0;                             /* reset number of hidden axis */
      r.hx = NULL;
      ctype.a = ctype_buf; ctype.l = MAXFTSNAMLEN;
      cunit.a = cunit_buf; cunit.l = MAXFTSNAMLEN;
      dtype.a = dtype_buf; dtype.l = MAXFTSNAMLEN;
      dunit.a = dunit_buf; dunit.l = MAXFTSNAMLEN;
      do {
         if (pmask) {
            r.hx = (ax_struct *) realloc( (char *) r.hx, ( r.maxis + 1 ) * sizeof( ax_struct ) );
            r.hx[r.maxis].cdelt = cdelt;
            r.hx[r.maxis].crpix = crpix;
            r.hx[r.maxis].crota = crota;
            r.hx[r.maxis].crval = crval;
            strncpy( r.hx[r.maxis].ctype, ctype.a, MAXFTSNAMLEN );
            strncpy( r.hx[r.maxis].cunit, cunit.a, MAXFTSNAMLEN );
            r.hx[r.maxis].pmask = pmask;
            r.hx[r.maxis].ddelt = ddelt;
            r.hx[r.maxis].drpix = drpix;
            r.hx[r.maxis].drota = drota;
            r.hx[r.maxis].drval = drval;
            strncpy( r.hx[r.maxis].dtype, dtype.a, MAXFTSNAMLEN );
            strncpy( r.hx[r.maxis].dunit, dunit.a, MAXFTSNAMLEN );
            r.hx[r.maxis].smask = smask;
            pmask = smask = 0;
            r.maxis += 1;                                     /* add one axis */
         }
         axnum += 1;                                      /* next axis number */
         gdsd_rchar_c( set, descr( "CUNIT", axnum ), &level, cunit, &derror );
         if (derror) derror = 0; else pmask += 1;
         gdsd_rchar_c( set, descr( "CTYPE", axnum ), &level, ctype, &derror );
         if (derror) derror = 0; else pmask += 2;
         gdsd_rdble_c( set, descr( "CRVAL", axnum ), &level, &crval, &derror );
         if (derror) derror = 0; else pmask += 4;
         gdsd_rdble_c( set, descr( "CRPIX", axnum ), &level, &crpix, &derror );
         if (derror) derror = 0; else pmask += 8;
         gdsd_rdble_c( set, descr( "CROTA", axnum ), &level, &crota, &derror );
         if (derror) derror = 0; else pmask += 16;
         gdsd_rdble_c( set, descr( "CDELT", axnum ), &level, &cdelt, &derror );
         if (derror) derror = 0; else pmask += 32;
         if (pmask) {
            gdsd_rchar_c( set, descr( "DUNIT", axnum ), &level, dunit, &derror );
            if (derror) derror = 0; else smask += 1;
            gdsd_rchar_c( set, descr( "DTYPE", axnum ), &level, dtype, &derror );
            if (derror) derror = 0; else smask += 2;
            gdsd_rdble_c( set, descr( "DRVAL", axnum ), &level, &drval, &derror );
            if (derror) derror = 0; else smask += 4;
            gdsd_rdble_c( set, descr( "DRPIX", axnum ), &level, &drpix, &derror );
            if (derror) derror = 0; else smask += 8;
            gdsd_rdble_c( set, descr( "DROTA", axnum ), &level, &drota, &derror );
            if (derror) derror = 0; else smask += 16;
            gdsd_rdble_c( set, descr( "DDELT", axnum ), &level, &ddelt, &derror );
            if (derror) derror = 0; else smask += 32;
         }
      } while (pmask);
   }
   return( r );
}

static void free_set_info( set_struct set_info )
/*
 * free_set_info frees the allocated memory in a set_struct.
 */
{
   if (set_info.ax != NULL) free( (char *) set_info.ax );
}

/*
 * descr_type() determines the type of table columns.
 */
static dsctyp descr_type(fchar set, fchar descr, fint level)
{
   static char  ctypec[11], ccommc[11], cuntsc[11];
   dsctyp result=CHAR;
    
   if (gdsa_istable_c(descr)==3) {
      fchar ctype, ccomm, cunts;
      fint  nrows, ierr=0;
      char cdescr[MAXDSCNAMLEN];
      char *tab, *col;

      ctype.a = ctypec; ctype.l = 10;
      ccomm.a = ccommc; ccomm.l = 10;
      cunts.a = cuntsc; cunts.l = 10;
      strncpy(cdescr, descr.a, descr.l);
      tab = strtok(cdescr+2, "_?");
      col = strtok(NULL, "_?");
      gdsa_colinq_c(set, &level, tofchar(tab), tofchar(col),
                    ctype, ccomm, cunts, &nrows, &ierr);
      if (ierr>=0 || ierr==-25) {
         if (!strncmp(ctypec, "INT", 3)) {
            result = INT;
         } else if (!strncmp(ctypec, "LOG", 3)) {
            result = LOG;
         } else if (!strncmp(ctypec, "REAL", 4)) {
            result = REAL;
         } else if (!strncmp(ctypec, "DBLE", 4)) {
            result = DBLE;
         }
      }
   }
   return result;
}


/*
 * convert_type converts source type to destination type.
 * currently only relevant for table columns.
 */
static void convert_type(char *buffer, fint nbytes, dsctyp kind)
{
   switch (kind) {
      case LOG:
      case INT: {
         if (itype1 != itype2) {
            fint *ibuf = (fint*)buffer;
            int  n = nbytes/sizeof(fint);
            while (n--) {
               *ibuf = swapfint(*ibuf);
               ibuf++;
            }
         }
         break;
      }
      case REAL: {
         if (ftype1 != ftype2) {
            float *fbuf = (float*)buffer;
            fint n = nbytes/sizeof(float);
            if (ftype1 != OS_FLOATING_TYPE) {
               (void)spfpfl_c(&ftype1, fbuf, fbuf, &n);
            }
            if (OS_FLOATING_TYPE != ftype2) {
               (void)spfplf_c(&ftype2, fbuf, fbuf, &n);
            }
         }
         break;
      }
      case DBLE: {
         if (ftype1 != ftype2) {
            double *dbuf = (double*)buffer;
            fint n = nbytes/sizeof(double);
            if (ftype1 != OS_FLOATING_TYPE) {
               (void)dpfpfl_c(&ftype1, dbuf, dbuf, &n);
            }
            if (OS_FLOATING_TYPE != ftype2) {
               (void)dpfplf_c(&ftype2, dbuf, dbuf, &n);
            }
         }
         break;
      }
      default: {
         /* CHAR or unspecified: no conversion */
      }
   }
}

static void copy_descriptors( set_struct set1 , set_struct set2 )
/*
 * This procedure copies the descriptor items from input set to the
 * output set. Some descriptor items are not copied. These are so called
 * reserved descriptor names which have all something to do with
 * coordinate systems and such.
 */
{
   char        dscbuf[MAXDSCNAMLEN];     /* buffer to store a descriptor name */
   fchar       dsc;  /* fortran character equivalent pointing to above buffer */
   fchar       name1;         /* fortran character pointing to input set name */
   fchar       name2;        /* fortran character pointing to output set name */
   fint       *axnum1;   /* array containing sorted axis numbers of input set */
   fint       *axnum2;  /* array containing sorted axis numbers of output set */
   fint       *def1;   /* array flags whether a grid is defined for input set */
   fint       *def2;  /* array flags whether a grid is defined for output set */
   fint        dscerr = 0;   /* error occurred when searching for descriptors */
   fint        not_equal;                /* input and output set are the same */
   fint       *flg;          /* flags whether grids are continuous along axis */
   fint8       *grid1;         /* array with grids defined for input set level */
   fint8       *grid2;        /* array with grids defined for output set level */
   fint        k;
   fint        m;
   fint       *min;       /* array with the smallest grid position on an axis */
   fint       *max;        /* array with the largest grid position on an axis */
   fint        recno = 0;         /* record number for gdsd_find, initialized */
   fint        subdim = set1.subdim;                   /* dimension of subset */
   trf_struct *trf_buf = NULL;                           /* pointer to struct */
   fint        trf_num = 0;                /* number of descriptors in struct */
   set_struct  set0;

   set0 = in_buf[set1.ip];
   /*
    * First we compare the input set name with the output set name.
    * If they are equal, and also the levels at which descriptors
    * should be transfered compare (this is checked later in this
    * procedure) then don't copy it. This saves some time.
    */
   not_equal = strncmp( set1.set, set2.set, MAXSETNAMLEN );
   /*
    * Create the arrays which hold the minimum and maximum grid
    * along an axis. Also an array is created which flags whether
    * the grids are distributed uniformly along an axis. This is
    * handy when we have to compare the grids outside the subset
    * with the grids defined in the level at which a descriptor
    * was found.
    */
   if (set1.naxis > set2.naxis) {
      min = (fint *) calloc( set2.naxis, sizeof( fint ) );
      max = (fint *) calloc( set2.naxis, sizeof( fint ) );
      flg = (fint *) calloc( set2.naxis, sizeof( fint ) );
   } else {
      min = (fint *) calloc( set1.naxis, sizeof( fint ) );
      max = (fint *) calloc( set1.naxis, sizeof( fint ) );
      flg = (fint *) calloc( set2.naxis, sizeof( fint ) );
   }
   /*
    * Create the arrays which hold the grids defined in a coordinate
    * word. Also an array is created which informs us whether a grid
    * was defined in a coordinate word. Furthermore the arrays which
    * hold the axis sequence are created.
    */
   axnum1 = (fint *) calloc( set1.naxis, sizeof( fint ) );
   axnum2 = (fint *) calloc( set2.naxis, sizeof( fint ) );
   grid1 = (fint8 *) calloc( set1.naxis, sizeof( fint8 ) );
   grid2 = (fint8 *) calloc( set2.naxis, sizeof( fint8 ) );
   def1 = (fint *) calloc( set1.naxis, sizeof( fint ) );
   def2 = (fint *) calloc( set2.naxis, sizeof( fint ) );
   /*
    * Create here the equivalent of a fortran character in C. We only
    * have to do this for the set names and the descriptors.
    */
   name1.a = set1.set; name1.l = MAXSETNAMLEN;
   name2.a = set2.set; name2.l = MAXSETNAMLEN;
   itype1 = gds_itype_c(name1, &ierr);     /* integer type of source */
   itype2 = gds_itype_c(name2, &ierr);     /* integer type of destination */
   ftype1 = gds_ftype_c(name1, &ierr);     /* floating type of source */
   ftype2 = gds_ftype_c(name2, &ierr);     /* floating type of destination */
   dsc.a = dscbuf; dsc.l = MAXDSCNAMLEN;
   /*
    * Sort here the axis numbers in the order of first subset axis,
    * second subset axis, etc., first defined axis outside subset,
    * second defined axis outside subset, etc. This is just done for
    * convenience.
    */
   m = 0;
   for (k = 0; k < set1.naxis; k++) {
      if (set1.ax[k].def > 0) {                             /* outside subset */
         axnum1[set1.ax[k].def+subdim-1] = k + 1;
      } else {                                               /* inside subset */
         axnum1[m++] = k + 1;
      }
   }
   m = 0;
   for (k = 0; k < set2.naxis; k++) {
      if (set2.ax[k].def > 0) {                             /* outside subset */
         axnum2[set2.ax[k].def+subdim-1] = k + 1;
      } else {                                               /* inside subset */
         axnum2[m++] = k + 1;
      }
   }
   /*
    * Find the limits of the grids which lie in set1 and set2. We also
    * determine here whether the grids are distributed uniformly along
    * an axis. For subset axes there is no problem, but for the axes
    * outside the subset we need to do some work.
    */
   for (k = 0; k < set1.naxis && k < set2.naxis; k++) {
      fint low1 = set1.ax[axnum1[k]-1].low;
      fint low2 = set2.ax[axnum2[k]-1].low;
      fint upp1 = set1.ax[axnum1[k]-1].upp;
      fint upp2 = set2.ax[axnum2[k]-1].upp;

      if (k < subdim) {                                      /* inside subset */
         if (low1 < low2) min[k] = low2; else min[k] = low1;
         if (upp1 > upp2) max[k] = upp2; else max[k] = upp1;
         flg[k] = 1;               /* uniformly distributed along subset axis */
      } else if (set1.ax[axnum1[k]-1].pos != NULL) {         /* outside subset */
         fint  m;
         fint  n;
         fint  npos = set1.ax[axnum1[k]-1].npos;
         fint8 *pos1 = set1.ax[axnum1[k]-1].pos;

         min[k] = max[k] = pos1[0];
         for (n = 1; n < npos; n++) {
            if (pos1[n] < min[k]) {
               min[k] = pos1[n];
            } else if (pos1[n] > max[k]) {
               max[k] = pos1[n];
            }
         }
         /*
          * Next we determine whether the grids are defined over the
          * whole range.
          */
         flg[k] = 1;
         for (m = min[k]; m <= max[k] && flg[k]; m++) {
            flg[k] = 0;
            for (n = 0; n < npos && !flg[k]; n++) {
               if (m == pos1[n]) flg[k] = 1;
            }
         }
         /*
          * Now check whether the whole input axis is used.
          */
         if (flg[k]) {
            if (min[k] != set1.ax[axnum1[k]-1].low) {
               flg[k] = 0;
            } else if (max[k] != set1.ax[axnum1[k]-1].upp) {
               flg[k] = 0;
            }
         }
      } else {                       /* outside subset, no matching positions */
         flg[k] = 0;
      }
   }
   if (set2.exist) {
      /*
       * The output set already exists, so we have to check for special
       * descriptors which might become invalid. This is done by searching
       * the descriptor file of the existing output set for special
       * descriptors. When found, their levels are compared. When the
       * level at which a descriptor is found will be overwritten, the
       * descriptors should be removed since we do not know whether they
       * will be replace by (correct) values from the input set.
       */
      des_struct *des_buf = NULL;              /* initialize des-truct struct */
      fint        des_num = 0;                /* initialize number in des_buf */
      fint        recno = 0;                         /* counter for gdsd_find */

      do {
         fint defin = 0;
         fint defout = 0;
         fint destruct = 0;
         fint8 level = 0;
         fint level_fint = 0;

         /*
          * Find the next descriptor in input set.
          */
         gdsd_find_c( dsc, name2, NULL, &recno, &level_fint );
         if (!recno) { level_fint = 0; break; }    /* nothing found, so leave loop */
         if (level_fint < 0) { dscerr = level_fint; level_fint = 0; break; }
         /*
          * is it a special descriptor ?
          */
         if (special_descriptor( dsc)) {
            /*
             * Next, if descriptor in special list, we have to extract the
             * grids from the level at which the descriptor was found.
             */
            for (k = 0; k < set2.naxis; k++) {
               fint cerror = 0;
               level = level_fint;
               grid2[k] = gdsc_grid_c( name2, &axnum2[k], &level, &cerror );
               level_fint = (fint)level;
               if (cerror) {                                   /* not defined */
                  def2[k] = 0;
               } else {                                            /* defined */
                  def2[k] = 1;
               }
               if (k < subdim) defin += def2[k]; else defout += def2[k];
            }
            /*
             * We only have to check those levels which have defined grids
             * outside the subset.
             */
            if (!defout) {
               destruct = 1 ;                            /* must be destroyed */
            } else {
               fint ovrw = 1;

               for (k = 0; ovrw && k < set2.naxis; k++) {
                  if (def2[k]) {
                     fint  n;
                     fint  npos = set2.ax[axnum2[k]-1].npos;
                     fint8 *pos2 = set2.ax[axnum2[k]-1].pos;

                     for (n = 0; n < npos; n++) {
                        if (pos2[n] == grid2[k]) break;
                     }
                     if (n == npos) ovrw = 0;      /* will not be overwritten */
                  }
               }
               if (ovrw) destruct = 1;
            }
            if (destruct) {                           /* put in destruct list */
               des_buf = (des_struct *) realloc( des_buf, sizeof(des_struct) * ++des_num );
               strncpy( des_buf[des_num-1].dsc, dsc.a, MAXDSCNAMLEN );
               des_buf[des_num-1].level = level_fint;
            }
         }
      } while (recno);
      /*
       * Now that we have found all the descriptors which should be removed
       * from the existing output set, we can remove them.
       */
      for (k = 0; k < des_num; k++) {
         fchar dsc;
         fint  derror = 0;
         fint  level = des_buf[k].level;

         dsc.a = des_buf[k].dsc; dsc.l = MAXDSCNAMLEN;
         gdsd_delete_c( name2, dsc, &level, &derror );
      }
      free( des_buf );                                         /* free memory */
   }
   /*
    * The next loop finds all descriptors defined in the input set. A
    * check is made whether these descriptors should be transfered, and
    * if so, they are put in the transfer list.
    */
   do {                           /* loop to get all descriptors in input set */
      fint defin = 0;                 /* nuber of defined grids inside subset */
      fint defout = 0;               /* nuber of defined grids outside subset */
      fint flag;          /* flag, when 1 do copy, else don't copy descriptor */
      fint8 level1 = 0;                 /* level at which descriptor was found */
      fint8 level2 = 0;           /* level to which descriptor is to be copied */
      /*
       * Find the next descriptor in input set.
       */
      gdsd_find_c( dsc, name1, NULL, &recno, &level1 );
      if (!recno) { level1 = 0; break; }      /* nothing found, so leave loop */
      if (level1 < 0) { dscerr = level1; level1 = 0; break; }
      flag = reserved_descriptor( dsc );                 /* compare with list */
      if (flag) {                           /* not reserved, so compare grids */
         /*
          * Next, if descriptor not in reserved list, we have to extract the
          * grids from the level at which the descriptor was found.
          */
         for (k = 0; k < set1.naxis; k++) {
            fint cerror = 0;

            //level1_fint8 = level1;
            grid1[k] = gdsc_grid_c( name1, &axnum1[k], &level1, &cerror );
            //level1 = (fint)level1_fint8;
            if (cerror) {                                      /* not defined */
               def1[k] = 0;
            } else {                                               /* defined */
               def1[k] = 1;
            }
            if (k < subdim) defin += def1[k]; else defout += def1[k];
         }
         /*
          * CLASS 1 or CLASS 2 program?
          * This is indicated by the change flag in set_struct.
          * GDSASN and GDSCPA check whether the application changes
          * one of the axes outside the subset. Here the CLASS is
          * determined, not via the GDSINP call.
          */
         if (set2.change && defout) flag = 0;
      }
      if (flag) {
         /*
          * Is it a special descriptor we found?
          * If so, we only transfer it when it remains valid in the
          * output set.
          */
         if (special_descriptor( dsc )) {
            for (k = 0; flag && k < set1.naxis; k++) {
               if (!def1[k] && !flg[k]) flag = 0;
            }
         }
      }
      if (flag) {                                         /* level sofar okay */
         /*
          * In the next loop we determine whether this header item is
          * inside the transfer window. If it is, then calculate the
          * grid in the output set.
          */
         for (k = 0; flag && k < set1.naxis && k < set2.naxis; k++) {
            def2[k] = def1[k];
            if (def1[k]) {                          /* is this grid defined ? */
               if (grid1[k] < min[k] || grid1[k] > max[k]) {
                  flag = 0;                        /* outside transfer window */
               } else {
                  flag = 1;                         /* inside transfer window */
               }
               if (flag) {                            /* still okay to copy ? */
                  if (k < subdim) {          /* does it fit inside the subset */
                     grid2[k] = grid1[k];      /* grids correspond one to one */
                  } else {                                  /* outside subset */
                     fint  found = 0;
                     fint  n;
                     fint  npos = set1.ax[axnum1[k]-1].npos;
                     fint8 *pos1 = set1.ax[axnum1[k]-1].pos;
                     fint8 *pos2 = set2.ax[axnum2[k]-1].pos;

                     for (n = 0; !found && n < npos; n++) {
                        if (pos1[n] == grid1[k]) found = n + 1;
                     }
                     if (!found) {
                        flag = 0;                                /* not found */
                     } else {
                        grid2[k] = pos2[found-1];            /* corresponding */
                     }
                  }
               }
            }
         }
         while (k < set2.naxis) def2[k++] = 0; /* zero if ouput set more axes */
      }
      if (flag) {
         /*
          * Here we set out to calculate the corresponding coordinate
          * word of the destination level.
          */

         for (k = 0; k < set2.naxis; k++) {
            fint cerror = 0;

            if (def2[k]) {
               level2 = gdsc_word_c( name2, &axnum2[k], &grid2[k],
                  &level2, &cerror );
               //level2 = level2_fint8;
            }
         }
         /*
          * Here we have the last check. If input set is the same as the
          * output set, and destination level is equal to source level,
          * then we do not have to bother with copying stuff.
          */
         if (!not_equal && level2 == level1) flag = 0;
      }
      if (flag) {
         /*
          * Now we can finally store this descriptor in the transfer list.
          * Later we will read this list and do the transfer.
          */
         trf_buf = (trf_struct *) realloc( trf_buf, sizeof( trf_struct ) * ++trf_num );
         strncpy( trf_buf[trf_num-1].dsc, dsc.a, MAXDSCNAMLEN );
         trf_buf[trf_num-1].level1 = level1;
         trf_buf[trf_num-1].level2 = level2;
         trf_buf[trf_num-1].kind   = descr_type(name1, dsc, level1);
      }
   } while (recno);                 /* until no more descriptors left to read */
   /*
    * Do here the transfer of descriptors. We read the transfer list and
    * copy descriptor after descriptor.
    */
   for (k = 0; k < trf_num; k++) {
      /*
       * Now we can finally copy the descriptor (in chuncks of
       * MAXDSCBUFLEN bytes) from input set to output set. We do this
       * with GDSD_READ and GDSD_WRITE so that any type of descriptor
       * (FITS descriptor, TABLE descriptor, etc.) will be copied
       * correctly.
       */
      char buffer[MAXDSCBUFLEN];
      fint bytes_done_r;
      fint bytes_done_w;
      fint bytes_left;
      fint derror = 0;
      fint level1 = trf_buf[k].level1;
      fint level2 = trf_buf[k].level2;
      dsctyp kind = trf_buf[k].kind;
      fint position_r = 1;
      fint position_w = 1;

      dsc.a = trf_buf[k].dsc;
      bytes_left = gdsd_length_c( name1, dsc, &level1, &derror );
      while (bytes_left) {             /* loop until everything transfered */
         if (bytes_left < MAXDSCBUFLEN) {    /* how many bytes to transfer */
            bytes_done_r = bytes_left;
         } else {
            bytes_done_r = MAXDSCBUFLEN;
         }
         gdsd_read_c( name1, dsc, &level1, (fint *) buffer, &bytes_done_r,
            &position_r, &bytes_done_r, &derror );      /* read descriptor */
         convert_type(buffer, bytes_done_r, kind);
         position_r += bytes_done_r;
         bytes_done_w = bytes_done_r;
         gdsd_write_c( name2, dsc, &level2, (fint *) buffer, &bytes_done_w,
            &position_w, &bytes_done_w, &derror );     /* write descriptor */
         if (derror < 0) {
            char	errmes[80];
            fint	error_level = 4;

            sprintf( errmes, "GDSD_WRITE error %d writing %.*s",
               (int) derror, (int) dsc.l, dsc.a );
            error_c( &error_level, tofchar( errmes ) );
         }
         position_w += bytes_done_w;
         bytes_left -= bytes_done_r;   /* number of bytes left to transfer */
      }
   }
   /*
    * Final, we free the memory allocated for some arrays.
    */
   free( trf_buf );
   free( axnum1 ); free( axnum2 );
   free( def1 ); free( def2 );
   free( grid1 ); free( grid2 );
   free( min ); free( max ); free( flg );
   /*
    * When transfer loop was left because of an error, then inform the
    * user with an error message at level 3.
    */
   if (dscerr) {
      fint errlev = 3;

      error_c( &errlev, tofchar( "Error copying descriptors!" ) );
   }
}

/*

#>            gdsinp.dc2

Function:     GDSINP

Purpose:      GDSINP prompts the user to enter the name of a set and
              the subsets, and returns the number of subsets entered.

Category:     USER IO

Files:        gdsinp.c

Author:       K.G. Begeman

Use:          INTEGER GDSINP( SET      ,  I/O  character*(*)
                              SUBSET   ,   O   integer array*8
                              MAXSUB   ,   I   integer
                              DEFAULT  ,   I   integer
                              KEYWORD  ,   I   character*(*)
                              MESSAGE  ,   I   character*(*)
                              SHOWDEV  ,   I   integer
                              AXPERM   ,   O   integer array
                              AXCOUNT  ,   O   integer array
                              MAXAXES  ,   I   integer
                              CLASS    ,   I   integer
                              CLASSDIM )  I/O  integer

              GDSINP   Returns the number of subsets entered by user or
                       -1 in case of unrecoverable errors.
              SET      On input name of default set (and subsets) as the
                       user would have to type in and is appended in the
                       standard MESSAGE (not when an own message is used).
                       On output SET will contain the name only, even when
                       default subsets were specified.
                       If defaults are allowed and used by user, and SET is
                       on input blank, then GDSINP returns 0 for the number
                       of subsets.
              SUBSET   Array containing subsets coordinate words.
              MAXSUB   Maximum number of subsets in SUBSET.
              DEFAULT  Default code as in USERxxx. It determines whether
                       the user is prompted for a SET and/or a default
                       value of SET is accepted or not and whether an
                       exact number of subsets is required (specified in
                       MAXSUB).
                       If 100 is added to the default code, then the user is
                       not prompted and in case of errors GDSINP rejects
                       the keyword and returns -1 immediately.
              KEYWORD  Keyword prompts the user for set and subset(s).
                       Normally KEYWORD = 'SET='.
              MESSAGE  Message for the user. If MESSAGE is blank, standard
                       message 'give set information' will be used.
              SHOWDEV  Device number (as in ANYOUT) to which GDSINP sends
                       some info about the input set.
              AXPERM   Array of size MAXAXES containing the axes numbers.
                       The first elements (upto the dimension of the subset)
                       contain the axes numbers of the subset, the other ones
                       contain the axes numbers outside the subset ordered
                       according to the specification by the user.
              AXCOUNT  Array of size MAXAXES containing the number of grids
                       along an axes as specified by the user. The first
                       elements (upto the dimension of the subset) contain
                       the length of the subset axes, the other ones contain
                       the number of grids along an axes outside the subset.
              MAXAXES  Maximum number of axes the program can deal with.
                       This is the size of AXPERM and AXCOUNT.
              CLASS    What class of input is wanted. Class 1 is for
                       applications which repeat the operation for each
                       subset (MNMX), Class 2 is for applications for
                       which the operation requires an interaction between
                       the different subsets (MEAN). Class 2 programs need
                       always more than one subset (See GIPSY MEMO #1).
              CLASSDIM Dimensionality of the subsets for class 1
                       applications, number of axes outside the subset
                       for class 2 programs. If on input CLASSDIM is zero
                       then the user is free to determine the number of
                       axes of the subsets for class 1 and the number of
                       axes outside the subset for class 2 applications.
                       Then on output CLASSDIM will contain the dimensions
                       of the subset (class 1) or the number of axes
                       outside the subset (class 2).

Description:  GDSINP is a function which prompts the user to enter the name
              of a set and (optionally) subset(s) and returns the number of
              subsets entered. This routine checks whether the set and
              subsets entered are present. If not present, it will inform
              the user and prompts again for set and subsets.

Related Docs: gdsout.dc2, gdsasn.dc2, gdscss.dc2, gdscpa.dc2, gdscsa.dc2

Updates:      Jan 22, 1990: KGB, Document created.
              Dec 11, 1991: KGB, cancel replaced by reject.
              Jul 14, 1994: VOG, Inserted new KGB '1.0+DBL_EPSILON' code
              Nov  7, 1997: JPT, Implemented default code +100
              Jul 12, 2000: JPT, Increased set name length to 256
              Apr 09, 2009: VOG, Replaced NINT definition with one that
                                 uses floor(). Several other routines
                                 dealing with coordinates now use the
                                 same definition. The routines now can deal
                                 properly with CRPIX values that end on 0.5
                                 (tested for both negative and positive CRPIX)

#<

Fortran to C interface:

@integer function gdsinp( character,
@                         integer*8  ,
@                         integer  ,
@                         integer  ,
@                         character,
@                         character,
@                         integer  ,
@                         integer  ,
@                         integer  ,
@                         integer  ,
@                         integer  ,
@                         integer  )

*/

fint gdsinp_c( fchar  set      ,
               fint8  *subsets  ,
               fint  *maxsub   ,
               fint  *defmode  ,
               fchar  keyword  ,
               fchar  message  ,
               fint  *showdev  ,
               fint  *axperm   ,
               fint  *axcount  ,
               fint  *maxaxes  ,
               fint  *class    ,
               fint  *classdim )
{
   char        errmes[MAXSTRLEN];                /* string for error messages */
   char       *sub;                                             /* sub-string */
   char       *sep = " '";                                /* token separators */
   char        b[MAXSTRLEN+1];                             /* buffer for text */
   fchar       name;
   fchar       string;
   fint        fatal = 4;                                     /* fatal errors */
   fint        gerror;                               /* gds error return code */
   fint        l;
   fint8        level = 0;                                /* subset top level  */
   fint        mode = (*defmode & 3);                /* stripped default mode */
   fint        n;
   fint        ndef = 0;                     /* number of axes outside subset */
   fint        nel = 0;
   fint        r = 0;                            /* returns number of subsets */
   fint        seterr = 0;                     /* error code for internal use */
   fint        subdim = 0;                             /* dimension of subset */
   bool        exitflag;      /* if set, immediately return in case of errors */
   set_struct  set_info;


   exitflag = (*defmode)>99;
   if (exitflag) *defmode -= 100;

   /*
    * First we check for some common errors made by the programmer.
    * These errors are all fatal!
    */
   if (*maxsub < 1) {                                         /* MAXSUB check */
      error_c( &fatal, tofchar( "GDSINP <MAXSUB < 1>!" ) );
   }
   if (*maxaxes < 1) {                                       /* MAXAXES check */
      error_c( &fatal, tofchar( "GDSINP <MAXAXES < 1>!" ) );
   }
   if ((*class != 1) && (*class != 2)) {                       /* CLASS check */
      error_c( &fatal, tofchar( "GDSINP <illegal CLASS>!" ) );
   }
   if ((*class == 2) && (*maxsub == 1)) {           /* MAXSUB and CLASS check */
      error_c( &fatal, tofchar( "GDSINP <MAXSUB = 1 for CLASS = 2>!" ) );
   }
   if (*classdim < 0) {                                     /* CLASSDIM check */
      error_c( &fatal, tofchar( "GDSINP <CLASSDIM < 0>!" ) );
   }
   if (*classdim > *maxaxes) {                              /* CLASSDIM check */
      error_c( &fatal, tofchar( "GDSINP <CLASSDIM > MAXAXES>!" ) );
   }
   do {
      set_info.ax = NULL;
      gerror = 0;                                                    /* reset */
      if (seterr && (mode & 2)) mode -= 2;			    /* unhide */
      if (seterr) {
         reject_c( keyword, tofchar( errmes ) );                     /* retry */
         if (exitflag) return -1;
      }
      seterr = 0;                                                    /* reset */
      string.a = buf; string.l = MAXBUFLEN;      /* make the character buffer */
      if (nelc_c( message )) {
         nel = usertext_c( string, &mode, keyword, message );    /* get input */
      } else {
         if (*class == 1) {
            nel = usertext_c( string, &mode, keyword, tofchar( "Give set (and subset(s))" ) );
         } else if (*class == 2) {
            nel = usertext_c( string, &mode, keyword, tofchar( "Give set and subsets" ) );
         }
      }
      if (!nel) {
         if ((nel = nelc_c( set )) > MAXBUFLEN) nel = MAXBUFLEN;
         if (nel) {
            strncpy( string.a, set.a, nel );
         } else {
            return( 0 );                         /* number of subsets is zero */
         }
      }
      buf[nel] = 0;                                          /* add zero byte */
      name.a = parse( buf, sep );  /* separate first token, which is set name */
      if (!name.a) seterr = 1; else name.l = strlen( name.a );     /* error ? */
      if (!seterr) if (!tobool(gds_exist_c( name, &gerror )) || gerror) seterr = 2;
      if (!seterr) if (*maxaxes < gdsc_ndims_c( name, &level )) seterr = 3;
      if (!seterr) if (set.l < name.l) seterr = 4;
      anyoutf(1, "debug gdsinp.c 1");
      if (!seterr) {
         fint axnum;

         ndef = 0;
         set_info = get_set_info( name );
         axnum = set_info.naxis;                         /* default last axis */
         sub = parse( NULL, sep );                          /* get next token */
         while (sub != NULL && !seterr) {
            fint len = strlen( sub );
            fint m;
            fint n;

            m = 0;
            for (n = 0; n < len; n++) sub[n] = toupper( sub[n] );
            for (n = 0; n < set_info.naxis; n++) {
               if (!strncmp( sub, set_info.ax[n].ctype, len )) {
                  if (m) m = -1; else m = n + 1;
               }
            }
            anyoutf(1, "debug gdsinp.c 2");
            switch(m) {
               case -1: {                             /* axis name not unique */
                  seterr = 5;
                  break;
               }
               case  0: {                 /* axis name not present, so decode */
                  fchar  vals;
                  fint   nval;
                  fint  *ints = NULL;
                  fint   nint = 0;
                  fint   nier;

                  if (!ndef) set_info.ax[axnum-1].def = ++ndef;   /* sequence */
                  vals.a = sub; vals.l = len;        /* make character string */
                  do {
                     nint += 512;
                     ints = (fint *) realloc( (char *) ints, sizeof(fint) * nint );
                     nval = dcdint_c( vals, ints, &nint, &nier );
                  } while (nier == -23);          /* until all values decoded */
                  if (nier) {                               /* decoding error */
                     seterr = 8;
                  } else {
                     fint  n;
                     fint  nold;
                     fint  ntot;
                     fint8 *nptr;

                     nold = set_info.ax[axnum-1].npos;
                     if (nold) nptr = set_info.ax[axnum-1].pos; else nptr = NULL;
                     ntot = nold + nval;
                     nptr = (fint8 *) realloc( (char *) nptr, ntot * sizeof( fint8 ) );
                     set_info.ax[axnum-1].npos = ntot;
                     set_info.ax[axnum-1].pos = nptr;
                     for (n = 0; n < nval; n++) nptr[nold+n] = ints[n];
                     anyoutf(1, "setting nptr[nold+n] = ints[n] = %d", ints[n]);
                  }
                  free( ints );
                  break;
               }
               default: {                    /* we found a matching axis name */
                  if (set_info.ax[m-1].def) {
                     seterr = 7;                           /* already defined */
                  } else {
                     set_info.ax[m-1].def = ++ndef;               /* sequence */
                     axnum = m;                          /* save this axis id */
                  }
                  break;
               }
            }
            sub = parse( NULL, sep );               /* get next position/axis */
         }
      }
      anyoutf(1, "debug gdsinp.c 3");
      if (!seterr) {                            /* check dimensions of subset */
         subdim = set_info.naxis - ndef;               /* dimension of subset */
         set_info.subdim = subdim;                           /* put in buffer */
         if (*class == 1) {                                  /* CLASS 1 input */
            if ((*classdim) && (*classdim != subdim)) {
               seterr = 9;                       /* wrong dimension of subset */
            }
         } else if (*class == 2) {                           /* CLASS 2 input */
            if ((*classdim) && (*classdim != ndef)) {
               seterr = 9;                  /* wrong number of operation axes */
            }
         }
      }
      anyoutf(1, "debug gdsinp.c 4");
      if (!seterr) {                             /* fill in default positions */
         fint m;

         r = 1;
         for (m = 0; m < set_info.naxis; m++) {
            if (set_info.ax[m].def) {
               fint npos = set_info.ax[m].npos;
               if (!npos) {               /* default is all positions on axis */
                  fint n;
                  npos = set_info.ax[m].npos = set_info.ax[m].naxis;
                  set_info.ax[m].pos = (fint8 *) calloc( npos, sizeof(fint8) );
                  for (n = 0; n < npos; n++) {
                     set_info.ax[m].pos[n] = set_info.ax[m].low + n;
                     anyoutf(1, "setting pos[%d] = %ld", n, set_info.ax[m].pos[n]);
                  }
               } else {
                  fint n;
                  fint low = set_info.ax[m].low;
                  fint upp = set_info.ax[m].upp;
                  for (n = 0; n < npos; n++) {
                     fint p = set_info.ax[m].pos[n];
                     if ((p < low) || (p > upp)) seterr = 10;  /* outside set */
                  }
               }
               r *= npos;
            }
         }
      }
      anyoutf(1, "debug gdsinp.c 5");
      if (!seterr) {                      /* check here the number of subsets */
         if (*defmode & 4) {
            if (r != *maxsub) seterr = 11;       /* Unequal number of subsets */
         } else {
            if (r > *maxsub) seterr = 12;                 /* Too many subsets */
         }
         if ((*class == 2) && (r == 1)) seterr = 13;
      }
      anyoutf(1, "debug gdsinp.c 6");
      if (!seterr) {             /* now calculate the subset coordinate words */
         fint        cerror = 0;
         fint        done = 0;
         fint        m1, m2;
         fint        n;
         pos_struct *pos;

         if (!*classdim) {
            if (*class == 1) *classdim = subdim; else *classdim = ndef;
         }
         m1 = 0;
         pos = (pos_struct *) calloc( set_info.naxis, sizeof(pos_struct) );
         anyoutf(1, "calc subset coord words: %d", set_info.naxis);
         for (n = 0; n < set_info.naxis; n++) {
            fint m = set_info.ax[n].def;
            anyoutf(1, "set_info.ax[n].pos....");
            //anyoutf(1, "set_info.ax[n].pos[0]=%ld", set_info.ax[n].pos[0]);
            if (m) {
               m2 = subdim + m - 1;
               axperm[m2] = n + 1;
               axcount[m2] = set_info.ax[n].npos;
               pos[m-1].pos = set_info.ax[n].pos;
               pos[m-1].count = 0;
               pos[m-1].npos = set_info.ax[n].npos;
               pos[m-1].axnum = n + 1;
            } else {
               axperm[m1] = n + 1;
               axcount[m1++] = set_info.ax[n].naxis;
            }
         }
         while (done < r) {
            fint8 cw = 0;

            for (n = 0; n < ndef; n++) {
               fint c = pos[n].count;
               if (c == pos[n].npos) {
                  pos[n].count = c = 0;
                  pos[n+1].count += 1;
               }
               anyoutf(1, "ok, creating word: cw=%ld axnum=%d pos=%ld name=%s", cw, pos[n].axnum, pos[n].pos[c], name.a);
               cw = gdsc_word_c( name, &pos[n].axnum, &pos[n].pos[c], &cw, &cerror );
               anyoutf(1, "ok, created  word: cw=%ld", cw);
            }
            pos[0].count += 1;
            subsets[done++] = cw;
         }
         free( pos );                                    /* deallocate memory */
      }
      anyoutf(1, "debug gdsinp.c 7");
      switch(seterr) {                           /* print the errors (if any) */
         case 1: {
            strcpy( errmes, "No set name entered!" );
            break;
         }
         case 2: {                                      /* Set does not exist */
            strcpy( errmes, "Set does not exist!" );
            break;
         }
         case 3: {                                       /* MAXAXES too small */
            sprintf( errmes, "Too many dimensions in set, MAXIMUM is %d!", *maxaxes );
            break;
         }
         case 4: {                                        /* Set name to long */
            strcpy( errmes, "Set name too long!" );
            break;
         }
         case 5: {                                    /* Axis name not unique */
            strcpy( errmes, "Not unique axis name entered!" );
            break;
         }
         case 6: {                                            /* No such axes */
            strcpy( errmes, "Axis not present!" );
            break;
         }
         case 7: {                             /* Multiple definition of axes */
            strcpy( errmes, "Axis already defined!" );
            break;
         }
         case 8: {                                            /* Syntax error */
            strcpy( errmes, "Syntax error!" );
            break;
         }
         case 9: {                                 /* Wrong subset dimensions */
            if (*class == 1) {
               sprintf( errmes, "Wrong subset dimension, MUST be %d!", *classdim );
            } else if (*class == 2) {
               sprintf( errmes, "Wrong number of operation axes, MUST be %d!", *classdim );
            }
            break;
         }
         case 10: {                                  /* Subset does not exist */
            strcpy( errmes, "Non existent subset!" );
            break;
         }
         case 11: {                              /* Unequal number of subsets */
            sprintf( errmes, "Unequal number of subsets, MUST be %d!", *maxsub );
            break;
         }
         case 12: {                                       /* Too many subsets */
            sprintf( errmes, "Too many subsets entered, MAXIMUM is %d!", *maxsub );
            break;
         }
         case 13: {                    /* Only one subset given for CLASS = 2 */
            strcpy( errmes, "Need more than one subset!" );
            break;
         }
         default: {                                    /* No error, finish up */
            char format[30];              /* holds the format for output info */

            for (n = 0; n < name.l; n++) set.a[n] = name.a[n];        /* copy */
            while (n < set.l) set.a[n++] = ' ';                 /* blank fill */
            l = nelc_c( set );                          /* length of set name */
            sprintf( format, "Set %%.%ds has %%d axes", l );
            sprintf( b, format, set.a, set_info.naxis );
            anyout_c( showdev, tofchar( b ) );
            for (n = 0; n < set_info.naxis; n++) {
               sprintf( b, "%.18s from %5d to %5d", set_info.ax[n].ctype,
                  set_info.ax[n].low, set_info.ax[n].upp );
               anyout_c( showdev, tofchar( b ) );
            }
            for (n = 0; n < MAXKEYLEN && n < keyword.l; n++) {
               set_info.key[n] = keyword.a[n];
            }
            while (n < MAXKEYLEN) set_info.key[n++] = ' ';
            add_set_info( set_info );               /* add to internal buffer */
            break;
         }
      }
      free_set_info( set_info );			/* free memory */
   } while (seterr);
   return( r );                                          /* number of subsets */
}

/*

#>            gdsout.dc2

Function:     GDSOUT

Purpose:      GDSOUT prompts the user to enter the name of an output set and
              the subsets, and returns the number of subsets entered.

Category:     USER IO

File:         gdsinp.c

Author:       K.G. Begeman

Use:          INTEGER GDSOUT( SET     ,   I/O  character*(*)
                              SUBSET  ,    O   integer array
                              MAXSUB  ,    I   integer
                              DEFAULT ,    I   integer
                              KEYWORD ,    I   character*(*)
                              MESSAGE ,    I   character*(*)
                              SHOWDEV ,    I   integer
                              AXPERM  ,    O   integer array
                              AXCOUNT ,    O   integer array
                              MAXAXES )    I   integer

              GDSOUT   Returns the number of subsets entered by user.
              SET      On input name of default set (and subsets) as the
                       user would have to type in and is appended in the
                       standard MESSAGE (not when an own message is used).
                       On output SET will contain the name only, even when
                       default subsets were specified.
                       If defaults are allowed and used by user, and SET is
                       on input blank, then GDSOUT returns 0 for the number
                       of subsets.
              SUBSET   Array containing subsets coordinate words.
              MAXSUB   Maximum number of subsets in SUBSET.
              DEFAULT  Default code as is USERxxx. It determines whether
                       the user is prompted for a SET and/or a default
                       value of SET is accepted or not and whether an
                       exact number of subsets is required (specified in
                       MAXSUB).
                       If 100 is added to the default code, then the user is
                       not prompted and in case of errors GDSOUT rejects
                       the keyword and returns -1 immediately.
              KEYWORD  Keyword prompts the user for set and subset(s).
                       Normally KEYWORD = 'SETOUT='.
              MESSAGE  Message for the user. If MESSAGE is blank, standard
                       message 'give set information' will be used.
              SHOWDEV  Device number (as in ANYOUT) to which GDSOUT sends
                       some info about the input set.
              AXPERM   Array of size MAXAXES containing the axes numbers.
                       The first elements (upto the dimension of the subset)
                       contain the axes numbers of the subset, the other ones
                       contain the axes numbers outside the subset ordered
                       according to the specification by the user.
              AXCOUNT  Array of size MAXAXES containing the number of grids
                       along an axes as specified by the user. The first
                       elements (upto the dimension of the subset) contain
                       the length of the subset axes, the other ones contain
                       the number of grids along an axes outside the subset.
              MAXAXES  Maximum number of axes the program can deal with.
                       This is the size of AXPERM and AXCOUNT.

Description:  GDSOUT is a function which prompts the user to enter
              the name of a set and (optionally) subset(s) and
              returns the number of subsets entered. This routine
              checks whether the set and subsets entered are present
              on disk. If not present, it is created.

Related Docs: gdsinp.dc2, gdsasn.dc2, gdscss.dc2, gdscpa.dc2, gdscsa.dc2

Updates:      Jan 23, 1990: KGB, document created.
              Dec 11, 1991: KGB, cancel replaced by reject.
              Jul  8, 1999: JPT, Implemented default code +100.
              Aug  4, 1999: JPT, allow multiple calls for different sets.
              Sep 24, 1999: JPT, fixed byte order bug in table copy.
#<

Fortran to C interface:

@ integer function gdsout( character ,
@                          integer   ,
@                          integer   ,
@                          integer   ,
@                          character ,
@                          character ,
@                          integer   ,
@                          integer   ,
@                          integer   ,
@                          integer   )

*/

fint gdsout_c( fchar  set      ,
               fint  *subsets  ,
               fint  *maxsub   ,
               fint  *defmode  ,
               fchar  keyword  ,
               fchar  message  ,
               fint  *showdev  ,
               fint  *axperm   ,
               fint  *axcount  ,
               fint  *maxaxes  )
{
   bool        exist;                                /* does output set exist */
   char        errmes[MAXSTRLEN];                /* buffer for error messages */
   char        kbuf[MAXKEYLEN];                         /* buffer for keyword */
   char       *sub;                                             /* sub-string */
   char       *sep = " '";                                /* token separators */
   char        b[MAXSTRLEN+1];
   fchar       name;
   fchar       string;
   fint        fatal = 4;                                     /* fatal errors */
   fint        gerror;                               /* gds error return code */
   fint        in_buf_ptr;         /* pointer to related gdsinp call (in_buf) */
   fint        k = -1;
   fint        l;
   fint        level = 0;                                /* subset top level  */
   fint8       level_fint8 = 0;
   fint        mode = (*defmode & 3);                /* stripped default mode */
   fint        n;
   fint        ndef = 0;                     /* number of axes outside subset */
   fint        nel;
   fint        ovrw;
   fint        r;                                /* returns number of subsets */
   fint        seterr = 0;                     /* error code for internal use */
   bool        exitflag;      /* if set, immediately return in case of errors */
   set_struct  set_info;


   exitflag = (*defmode)>99;
   if (exitflag) *defmode -= 100;
      
   /*
    * First we check for some common errors made by the programmer.
    * These errors are all fatal!
    */
   if (*maxsub < 1) {                                         /* MAXSUB check */
      error_c( &fatal, tofchar( "GDSOUT <MAXSUB < 1>!" ) );
   }
   if (*maxaxes < 1) {                                       /* MAXAXES check */
      error_c( &fatal, tofchar( "GDSOUT <MAXAXES < 1>!" ) );
   }
   for (n = 0; n < keyword.l && n < MAXKEYLEN; n++) kbuf[n] = keyword.a[n];
   while (n < MAXKEYLEN) kbuf[n++] = ' ';
   for (n = 0; n < out_buf_size; n++) {
      if (!strncmp( kbuf, out_buf[n].key, MAXKEYLEN )) k = n;
   }
   if (k == -1) {
      error_c( &fatal, tofchar( "GDSOUT <KEYWORD not in buffer>" ) );
   }
   if ((out_buf[k].naxis == out_buf[k].subdim) && (*maxsub != 1)) {
      error_c( &fatal, tofchar( "GDSOUT <MAXSUB > 1>" ) );
   }
   in_buf_ptr = out_buf[k].ip;                         /* save in_buf pointer */
   do {
      set_info.ax = NULL;
      gerror = 0;                                                    /* reset */
      if (seterr && (mode & 2)) mode -= 2;			    /* unhide */
      if (seterr) {
         reject_c( keyword, tofchar( errmes ) );                     /* retry */
         if (exitflag) return -1;
      }
      seterr = 0; ovrw = 0; r = 1;                                   /* reset */
      string.a = buf; string.l = MAXBUFLEN;      /* make the character buffer */
      if (nelc_c( message )) {
         nel = usertext_c( string, &mode, keyword, message );    /* get input */
      } else {
         if (*maxsub == 1) {
            nel = usertext_c( string, &mode, keyword, tofchar( "Give set (and subset)" ) );
         } else {
            nel = usertext_c( string, &mode, keyword, tofchar( "Give set (and subset(s))" ) );
         }
      }
      if (!nel) {
         if ((nel = nelc_c( set )) > MAXBUFLEN) nel = MAXBUFLEN;
         if (nel) {
            strncpy( string.a, set.a, nel );
         } else {
            return( 0 );                         /* number of subsets is zero */
         }
      }
      buf[nel] = 0;                                          /* add zero byte */
      name.a = parse( buf, sep );  /* separate first token, which is set name */
      if (!name.a) seterr = 1; else name.l = strlen( name.a );     /* error ? */
      exist = tobool(gds_exist_c( name, &gerror ));/* does set exist already ? */
      if (exist) {
         if (gerror) {			                      /* set not okay */
            seterr = -1;
         } else {
            if (*maxaxes < gdsc_ndims_c( name, &level_fint8 )) seterr = 3;
            level = (fint)level_fint8;
            if (!seterr) {
               set_info = get_set_info( name );               /* get set info */
               set_info.change = out_buf[k].change;
            }
         }
      } else {
         set_info = copy_set_info( out_buf[k] );	     /* copy set info */
      }
      if (!seterr) if (set.l < name.l) seterr = 4;   /* set name does not fit */
      /*
       * Decode here the text typed in by the user into axes and grids.
       */
      if (!seterr) {
         fint axnum;

         ndef = 0;
         sub = parse( NULL, sep );                          /* get next token */
         if (sub) {                             /* defaults should be removed */
            for (n = 0; n < set_info.naxis; n++) {
               if (set_info.ax[n].def > 0) set_info.ax[n].def = 0;
               set_info.ax[n].npos = 0;
               set_info.ax[n].pos = NULL;
            }
         }
         axnum = set_info.naxis;                         /* default last axis */
         while (sub != NULL && !seterr) {
            fint len = strlen( sub );
            fint m;
            fint n;

            m = 0;
            for (n = 0; n < len; n++) sub[n] = toupper( sub[n] );
            for (n = 0; n < set_info.naxis; n++) {
               if (!strncmp( sub, set_info.ax[n].ctype, len )) {
                  if (m) m = -1; else m = n + 1;
               }
            }
            switch(m) {
               case -1: {                             /* axis name not unique */
                  seterr = 5;
                  break;
               }
               case  0: {                 /* axis name not present, so decode */
                  fchar  vals;
                  fint   nval;
                  fint  *ints = NULL;
                  fint   nint = 0;
                  fint   nier;

                  if (!ndef) set_info.ax[axnum-1].def = ++ndef;   /* sequence */
                  vals.a = sub; vals.l = len;        /* make character string */
                  do {
                     nint += 512;
                     ints = (fint *) realloc( (char *) ints, sizeof(fint) * nint );
                     nval = dcdint_c( vals, ints, &nint, &nier );
                  } while (nier == -23);          /* until all values decoded */
                  if (nier) {                               /* decoding error */
                     seterr = 8;
                  } else {
                     fint  n;
                     fint  nold;
                     fint  ntot;
                     fint8 *nptr;

                     nold = set_info.ax[axnum-1].npos;
                     if (nold) nptr = set_info.ax[axnum-1].pos; else nptr = NULL;
                     ntot = nold + nval;
                     nptr = (fint8 *) realloc( (char *) nptr, ntot * sizeof( fint8 ) );
                     set_info.ax[axnum-1].npos = ntot;
                     set_info.ax[axnum-1].pos = nptr;
                     for (n = 0; n < nval; n++) nptr[nold+n] = ints[n];
                  }
                  free( ints );
                  break;
               }
               default: {                    /* we found a matching axis name */
                  if (set_info.ax[m-1].def) {
                     if (set_info.ax[m-1].def > 0) {
                        seterr = 7;                        /* already defined */
                     } else {
                        seterr = 9;                 /* axis belongs to subset */
                     }
                  } else {
                     set_info.ax[m-1].def = ++ndef;               /* sequence */
                     axnum = m;                          /* save this axis id */
                  }
                  break;
               }
            }
            sub = parse( NULL, sep );               /* get next position/axis */
         }
      }
      if (!seterr && exist && !ndef) {                     /* fill in default */
         if (set_info.naxis != out_buf[k].naxis) {
            seterr = 10;                             /* default does not work */
         } else {
            for (n = 0; !seterr && n < set_info.naxis; n++) {
               fint d;
               fint m = 0;

               do {
                  d = strncmp( set_info.ax[n].ctype, out_buf[k].ax[m++].ctype, MAXFTSNAMLEN );
               } while ((d) && (m < out_buf[k].naxis));
               if (d) {
                  seterr = 11;                         /* ctypes not matching */
               } else {
                  set_info.ax[n].def = out_buf[k].ax[m-1].def;
                  set_info.ax[n].npos = out_buf[k].ax[m-1].npos;
                  set_info.ax[n].pos = out_buf[k].ax[m-1].pos;
               }
            }
            if (!seterr) ndef = out_buf[k].naxis - out_buf[k].subdim;
            if (!seterr && !ndef) ovrw |= 1;        /* overwrite complete set */
         }
      }
      if (!seterr && !exist && !ndef) {             /* default subsets wanted */
         ndef = out_buf[k].naxis - out_buf[k].subdim;
      }
      if (!seterr) {                            /* check dimensions of subset */
         set_info.subdim = set_info.naxis - ndef;
         if (set_info.subdim != out_buf[k].subdim) {
            seterr = 12;                         /* wrong dimension of subset */
         } else if (exist) {                          /* check size of subset */
            fint m = 0;

            for (n = 0; n < set_info.naxis; n++) {
               if (set_info.ax[n].def <= 0) {
                  if (set_info.ax[n].naxis != out_buf[k].ax[m].naxis) {
                     seterr = 13;                      /* sizes are not equal */
                  } else if (set_info.ax[n].low != out_buf[k].ax[m].low) {
                     seterr = 14;
                  } else if (set_info.ax[n].crpix != out_buf[k].ax[m].crpix) {
                     ovrw |= 2;               /* reference pixel not the same */
                  }
                  m += 1;
               }
            }
         }
      }
      if (!seterr && ndef) {                     /* fill in default positions */
         fint m;

         for (m = 0; m < set_info.naxis; m++) {
            if (set_info.ax[m].def > 0) {
               fint npos = set_info.ax[m].npos;

               if (!npos) {               /* default is all positions on axis */
                  npos = set_info.ax[m].npos = set_info.ax[m].naxis;
                  set_info.ax[m].pos = (fint8 *) calloc( npos, sizeof(fint8) );
                  for (n = 0; n < npos; n++) {
                     set_info.ax[m].pos[n] = set_info.ax[m].low + n;
                  }
                  if (exist) ovrw |= 1;        /* subsets will be overwritten */
               } else if (exist) {     /* if exist, check whether they fit in */
                  fint low = set_info.ax[m].low;
                  fint upp = set_info.ax[m].upp;

                  for (n = 0; !seterr && n < npos; n++) {
                     fint p = set_info.ax[m].pos[n];

                     if ((m + 1) == set_info.naxis) {            /* last axis */
                        if (p < low) {
                           seterr = 15;                  /* cannot write here */
                        } else if (p <= upp) {
                           ovrw |= 1;           /* subset will be overwritten */
                        }
                     } else {                                   /* other axis */
                        if (p < low) {
                           seterr = 15;                  /* cannot write here */
                        } else if (p > upp) {
                           seterr = 15;                  /* cannot write here */
                        }
                     }
                  }
               } else {             /* set does not exist, so ranges are free */
                  fint n;
                  fint low = set_info.ax[m].pos[0];
                  fint upp = set_info.ax[m].pos[0];

                  for (n = 0; n < npos; n++) {
                     fint p = set_info.ax[m].pos[n];

                     if (p < low) low = p; else if (p > upp) upp = p;
                  }
                  set_info.ax[m].crpix += set_info.ax[m].low - low;
                  /* We want: NINT(set_info.ax[m].crpix) == ( 1 - low). */

                  {
                     long dummy;
                     /*--------------------------------------------------*/
                     /* VOG (12-04-2009):                                */
                     /* Conditions to enter this code:                   */
                     /* 1) A new output set is created                   */
                     /* 2) No 'seterr' errors occured                    */
                     /* 3) There must be axes outside the subset         */
                     /*                                                  */
                     /* Assuming a CRPIX3 = 20.5 and a two dim subset    */
                     /* with axis numbers 1 and 2 then we observe 1      */
                     /* iteration when a previous version of macro NINT  */
                     /* was used. This iteration does not occur when the */
                     /* NINT macro uses floor().                         */
                     /* In theory the new definition makes the iteration */
                     /* below superfluous, but we keep on the safe side  */
                     /* and did not remove the code.                     */
                     /*                                                  */
                     /* Note that gdscps() suffers from the same problem */
                     /* but that code does not have a correction. The    */
                     /* new definition of NINT solves that problem too ! */
                     /*--------------------------------------------------*/
                     while ( ( dummy = NINT( set_info.ax[m].crpix ) ) != ( 1 - low ) ) {
                        if ( dummy > ( 1 - low ) ) {
                           if ( set_info.ax[m].crpix > 0.0 ) {
                              set_info.ax[m].crpix /= ( 1.0 + DBL_EPSILON );
                           } else {
                              set_info.ax[m].crpix *= ( 1.0 + DBL_EPSILON );
                           }
                        } else {
                           if ( set_info.ax[m].crpix > 0.0 ) {
                              set_info.ax[m].crpix *= ( 1.0 + DBL_EPSILON );
                           } else {
                              set_info.ax[m].crpix /= ( 1.0 + DBL_EPSILON );
                           }
                        }
                     }
                  }                                 
                  set_info.ax[m].naxis = upp - low + 1;
                  set_info.ax[m].low = low;
                  set_info.ax[m].upp = upp;
               }
               r *= npos;                          /* total number of subsets */
            }
         }
      }
      if (!seterr) {                      /* check here the number of subsets */
         if (*defmode & 4) {
            if (r != *maxsub) seterr = 16;       /* Unequal number of subsets */
         } else {
            if (r > *maxsub) seterr = 17;                 /* Too many subsets */
         }
      }
      /*
       * Next we check whether the number of grids defined along an axis outside
       * the input subset is equal to the number of grids defined along the
       * corresponding axis of the output subset. This check should only be
       * done when the axis of the output set were not changed with
       * GDSASN or GDSCPA.
       */
      if (!seterr && !set_info.change) {
         fint m;
         fint n;

         for (n = 0; !seterr && n < set_info.naxis; n++) {
            fint q = set_info.ax[n].def;

            if (q > 0) {
               for (m = 0; !seterr && m < out_buf[k].naxis; m++) {
                  fint r = out_buf[k].ax[m].def;

                  if (r == q) {
                     if (out_buf[k].ax[m].npos != set_info.ax[n].npos) {
                        seterr = 18;                  /* numbers do not match */
                     }
                  }
               }
            }
         }
      }
      if (!seterr && ovrw) {
         bool o = TRUE;
         fint d = 1;
         fint n = 1;

         switch(ovrw) {
            case 1: {
               (void) userlog_c( &o, &n, &d, tofchar( "OKAY=" ),
                  tofchar( "Will overwrite data, okay ? [Y]" ) );
               break;
            }
            case 2: {
               (void) userlog_c( &o, &n, &d, tofchar( "OKAY=" ),
                  tofchar( "Different CRPIX, okay ? [Y]" ) );
               break;
            }
            default: {
               (void) userlog_c( &o, &n, &d, tofchar( "OKAY=" ),
                  tofchar( "Overwrite data and different CRPIX, okay ? [Y]" ) );
               break;
            }
         }
         cancel_c( tofchar( "OKAY=" ) );
         if (!tobool(o)) seterr = 2;       /* retry because user reconsidered */
      }
      /*
       * If output set does not exist, we will have to create it.
       */
      if (!seterr && !exist) {                       /* create the output set */
         double crpix;
         fchar  ctype;
         fint   derror = 0;
         fint   naxis;
         fint   pmask;
         fint   smask;

         /*
          * Next we create the output set. The reserved descriptors, when
          * present, are copied from input set to output set.
          */
         gds_create_c( name, &gerror );                 /* output set created */
         if (set_info.rmask & 1) {
            double epoch = set_info.epoch;

            gdsd_wdble_c( name, tofchar( "EPOCH" ), &level, &epoch, &derror );
         }
         if (set_info.rmask & 2) {
            double freq0 = set_info.freq0;

            gdsd_wdble_c( name, tofchar( "FREQ0" ), &level, &freq0, &derror );
         }
         if (set_info.rmask & 4) {
            fchar instrume;

            instrume.a = set_info.instrume; instrume.l = MAXFTSNAMLEN;
            gdsd_wchar_c( name, tofchar( "INSTRUME" ), &level, instrume, &derror );
         }
         for (n = 0; n < set_info.naxis; n++) {
            fint a = n + 1;

            ctype.a = set_info.ax[n].ctype; ctype.l = MAXFTSNAMLEN;
            crpix = set_info.ax[n].crpix;
            naxis = set_info.ax[n].naxis;
            gds_extend_c( name, ctype, &crpix, &naxis, &gerror );   /* extend */
            pmask = set_info.ax[n].pmask;
            smask = set_info.ax[n].smask;
            if (pmask & 1) {
               fchar cunit;
               cunit.a = set_info.ax[n].cunit; cunit.l = MAXFTSNAMLEN;
               gdsd_wchar_c( name, descr( "CUNIT", a ), &level, cunit, &derror );
            }
            if (pmask & 4) {
               double crval = set_info.ax[n].crval;
               gdsd_wdble_c( name, descr( "CRVAL", a ), &level, &crval, &derror );
            }
            if (pmask & 16) {
               double crota = set_info.ax[n].crota;
               gdsd_wdble_c( name, descr( "CROTA", a ), &level, &crota, &derror );
            }
            if (pmask & 32) {
               double cdelt = set_info.ax[n].cdelt;
               gdsd_wdble_c( name, descr( "CDELT", a ), &level, &cdelt, &derror );
            }
            if (smask & 1) {
               fchar dunit;
               dunit.a = set_info.ax[n].dunit; dunit.l = MAXFTSNAMLEN;
               gdsd_wchar_c( name, descr( "DUNIT", a ), &level, dunit, &derror );
            }
            if (smask & 2) {
               fchar dtype;
               dtype.a = set_info.ax[n].dtype; dtype.l = MAXFTSNAMLEN;
               gdsd_wchar_c( name, descr( "DTYPE", a ), &level, dtype, &derror );
            }
            if (smask & 4) {
               double drval = set_info.ax[n].drval;
               gdsd_wdble_c( name, descr( "DRVAL", a ), &level, &drval, &derror );
            }
            if (smask & 8) {
               double drpix = set_info.ax[n].drpix;
               gdsd_wdble_c( name, descr( "DRPIX", a ), &level, &drpix, &derror );
            }
            if (smask & 16) {
               double drota = set_info.ax[n].drota;
               gdsd_wdble_c( name, descr( "DROTA", a ), &level, &drota, &derror );
            }
            if (smask & 32) {
               double ddelt = set_info.ax[n].ddelt;
               gdsd_wdble_c( name, descr( "DDELT", a ), &level, &ddelt, &derror );
            }
         }
         /*
          * Put the hidden axes in place.
          */
         for (n = 0; n < set_info.maxis; n++) {
            fint a = n + set_info.naxis + 1;

            pmask = set_info.hx[n].pmask;
            smask = set_info.hx[n].smask;
            if (pmask & 1) {
               fchar cunit;
               cunit.a = set_info.hx[n].cunit; cunit.l = MAXFTSNAMLEN;
               gdsd_wchar_c( name, descr( "CUNIT", a ), &level, cunit, &derror );
            }
            if (pmask & 2) {
               fchar ctype;
               ctype.a = set_info.hx[n].ctype; ctype.l = MAXFTSNAMLEN;
               gdsd_wchar_c( name, descr( "CTYPE", a ), &level, ctype, &derror );
            }
            if (pmask & 4) {
               double crval = set_info.hx[n].crval;
               gdsd_wdble_c( name, descr( "CRVAL", a ), &level, &crval, &derror );
            }
            if (pmask & 8) {
               double crpix = set_info.hx[n].crpix;
               gdsd_wdble_c( name, descr( "CRPIX", a ), &level, &crpix, &derror );
            }
            if (pmask & 16) {
               double crota = set_info.hx[n].crota;
               gdsd_wdble_c( name, descr( "CROTA", a ), &level, &crota, &derror );
            }
            if (pmask & 32) {
               double cdelt = set_info.hx[n].cdelt;
               gdsd_wdble_c( name, descr( "CDELT", a ), &level, &cdelt, &derror );
            }
            if (smask & 1) {
               fchar dunit;
               dunit.a = set_info.hx[n].dunit; dunit.l = MAXFTSNAMLEN;
               gdsd_wchar_c( name, descr( "DUNIT", a ), &level, dunit, &derror );
            }
            if (smask & 2) {
               fchar dtype;
               dtype.a = set_info.hx[n].dtype; dtype.l = MAXFTSNAMLEN;
               gdsd_wchar_c( name, descr( "DTYPE", a ), &level, dtype, &derror );
            }
            if (smask & 4) {
               double drval = set_info.hx[n].drval;
               gdsd_wdble_c( name, descr( "DRVAL", a ), &level, &drval, &derror );
            }
            if (smask & 8) {
               double drpix = set_info.hx[n].drpix;
               gdsd_wdble_c( name, descr( "DRPIX", a ), &level, &drpix, &derror );
            }
            if (smask & 16) {
               double drota = set_info.hx[n].drota;
               gdsd_wdble_c( name, descr( "DROTA", a ), &level, &drota, &derror );
            }
            if (smask & 32) {
               double ddelt = set_info.hx[n].ddelt;
               gdsd_wdble_c( name, descr( "DDELT", a ), &level, &ddelt, &derror );
            }
         }
      }
      /*
       * If set does exist, we may have to extend the last axis.
       */
      if (!seterr && exist) {
         fchar  ctype;
         fint   l = 0;
         fint   n;
         fint   naxis;
         fint   upp;
         double crpix;

         while (set_info.ax[l].def != ndef) l++;
         upp = set_info.ax[l].upp;
         ctype.a = set_info.ax[l].ctype; ctype.l = MAXFTSNAMLEN;
         crpix = set_info.ax[l].crpix;
         for (n = 0; n < set_info.ax[l].npos; n++) {
            if (set_info.ax[l].pos[n] > upp) upp = set_info.ax[l].pos[n];
         }
         naxis = upp - set_info.ax[l].low + 1;
         if (naxis > set_info.ax[l].naxis) {
            gds_extend_c( name, ctype, &crpix, &naxis, &gerror );   /* extend */
            set_info.ax[l].naxis = naxis;
            set_info.ax[l].upp = upp;
         }
      }
      if (!seterr) {             /* now calculate the subset coordinate words */
         fint        cerror = 0;
         fint        done = 0;
         fint        m1, m2;
         fint        n;
         pos_struct *pos;

         m1 = 0;
         pos = (pos_struct *) calloc( set_info.naxis, sizeof(pos_struct) );
         for (n = 0; n < set_info.naxis; n++) {
            fint m = set_info.ax[n].def;

            if (m > 0) {
               m2 = set_info.subdim + m - 1;
               axperm[m2] = n + 1;
               axcount[m2] = set_info.ax[n].npos;
               pos[m-1].pos = set_info.ax[n].pos;
               pos[m-1].count = 0;
               pos[m-1].npos = set_info.ax[n].npos;
               pos[m-1].axnum = n + 1;
            } else {
               axperm[m1] = n + 1;
               axcount[m1++] = set_info.ax[n].naxis;
            }
         }
         while (done < r) {
            fint8 cw = 0;

            for (n = 0; n < ndef; n++) {
               fint c = pos[n].count;
               if (c == pos[n].npos) {
                  pos[n].count = c = 0;
                  pos[n+1].count += 1;
               }
               cw = gdsc_word_c( name, &pos[n].axnum, &pos[n].pos[c], &cw, &cerror );
            }
            pos[0].count += 1;
            subsets[done++] = cw;
         }
         free( pos );                                    /* deallocate memory */
      }
      switch(seterr) {                           /* print the errors (if any) */
         case -1: {
            strcpy( errmes, "Set incompatible or corrupted!" );
            break;
         }
         case 1: {
            strcpy( errmes, "No set name entered!" );
            break;
         }
         case 2: {                       /* Retry, not permitted to overwrite */
            strcpy( errmes, "Set rejected by user!" );
            break;
         }
         case 3: {                                       /* MAXAXES too small */
            sprintf( errmes, "Too many dimensions in set, MAXIMUM is %d!", *maxaxes );
            break;
         }
         case 4: {                                        /* Set name to long */
            strcpy( errmes, "Set name too long!" );
            break;
         }
         case 5: {                                    /* Axis name not unique */
            strcpy( errmes, "Not unique axis name entered!" );
            break;
         }
         case 6: {                                            /* No such axes */
            strcpy( errmes, "Axis not present!" );
            break;
         }
         case 7: {                             /* Multiple definition of axes */
            strcpy( errmes, "Axis already defined!" );
            break;
         }
         case 8: {                                            /* Syntax error */
            strcpy( errmes, "Syntax error!" );
            break;
         }
         case 9: {                                  /* Axis belongs to subset */
            strcpy( errmes, "Axis already in subset!" );
            break;
         }
         case 10: {                          /* wrong dimension of output set */
            strcpy( errmes, "Output set has wrong dimensions!" );
            break;
         }
         case 11: {
            strcpy( errmes, "Output set has different axes!" );
            break;
         }
         case 12: {                                /* Wrong subset dimensions */
            sprintf( errmes, "Wrong subset dimension, MUST be %d!", out_buf[k].subdim );
            break;
         }
         case 13: {                            /* wrong size of output subset */
            strcpy( errmes, "Output subset wrong size!" );
            break;
         }
         case 14: {                           /* wrong edges of output subset */
            strcpy( errmes, "Output subset does not fit!" );
            break;
         }
         case 15: {                                  /* Subset does not exist */
            strcpy( errmes, "Subset cannot be created!" );
            break;
         }
         case 16: {                              /* Unequal number of subsets */
            sprintf( errmes, "Unequal number of subsets, MUST be %d!", *maxsub );
            break;
         }
         case 17: {                                       /* Too many subsets */
            sprintf( errmes, "Too many subsets entered, MAXIMUM is %d!", *maxsub );
            break;
         }
         case 18: {             /* number of grids along an axis do not match */
            strcpy( errmes, "Unequal number of grids outside subset!" );
            break;
         }
         default: {                                    /* No error, finish up */
            char format[30];              /* holds the format for output info */

            for (n = 0; n < name.l; n++) {                            /* copy */
               set_info.set[n] = set.a[n] = name.a[n];
            }
            for (n = name.l; n < set.l; set.a[n++] = ' ');      /* blank fill */
            for (n = name.l; n < MAXSETNAMLEN; set_info.set[n++] = ' ');
            copy_descriptors( out_buf[k], set_info );          /* copy header */
            l = nelc_c( set );                          /* length of set name */
            sprintf( format, "Set %%.%ds has %%d axes", l );
            sprintf( b, format, set.a, set_info.naxis );
            anyout_c( showdev, tofchar( b ) );
            for (n = 0; n < set_info.naxis; n++) {
               sprintf( b, "%.18s from %5d to %5d", set_info.ax[n].ctype,
                  set_info.ax[n].low, set_info.ax[n].upp );
               anyout_c( showdev, tofchar( b ) );
            }
            break;
         }
      }
     free_set_info( set_info );			/* free allocated memory */
   } while (seterr);				/* stop when no more errors */
   return( r );					/* number of subsets */
}

/*

#>            gdsdmp.dc3

Function:     GDSDMP

Purpose:      Displays the contents of the internal GDSINP/GDSOUT
              buffers (only for testing purposes).

Category:     SYSTEM

File:         gdsinp.c

Author:       K.G. Begeman

Use:          CALL GDSDMP( )

Updates:      Aug 24, 1991: KGB, Document created.

#<

Fortran to C interface:

@ subroutine gdsdmp( )

*/

void gdsdmp_c( void )
{
   char pb[81];
   fint d = 1;
   fint m;
   fint n;

   anyout_c( &d, tofchar( "Contents of GDSINP buffer" ) );
   for (n = 0; n < in_buf_size; n++) {
      sprintf( pb, "KEYWORD: %.20s", in_buf[n].key );
      anyout_c( &d, tofchar( pb ) );
      sprintf( pb, "SET    : %.20s", in_buf[n].set );
      anyout_c( &d, tofchar( pb ) );
      sprintf( pb, "NAXIS  : %d", in_buf[n].naxis );
      anyout_c( &d, tofchar( pb ) );
      sprintf( pb, "SUBDIM : %d", in_buf[n].subdim );
      anyout_c( &d, tofchar( pb ) );
      for (m = 0; m < in_buf[n].naxis; m++) {
         fint pmask = in_buf[n].ax[m].pmask;
         fint smask = in_buf[n].ax[m].smask;

         sprintf( pb, "NAXIS%d : %d", m + 1, in_buf[n].ax[m].naxis );
         anyout_c( &d, tofchar( pb ) );
         if (pmask & 32) {
            sprintf( pb, "CDELT%d : %f", m + 1, in_buf[n].ax[m].cdelt );
            anyout_c( &d, tofchar( pb ) );
         }
         if (pmask & 16) {
            sprintf( pb, "CROTA%d : %f", m + 1, in_buf[n].ax[m].crota );
            anyout_c( &d, tofchar( pb ) );
         }
         if (pmask & 8) {
            sprintf( pb, "CRPIX%d : %f", m + 1, in_buf[n].ax[m].crpix );
            anyout_c( &d, tofchar( pb ) );
         }
         if (pmask & 4) {
            sprintf( pb, "CRVAL%d : %f", m + 1, in_buf[n].ax[m].crval );
            anyout_c( &d, tofchar( pb ) );
         }
         if (pmask & 2) {
            sprintf( pb, "CTYPE%d : %.18s", m + 1, in_buf[n].ax[m].ctype );
            anyout_c( &d, tofchar( pb ) );
         }
         if (pmask & 1) {
            sprintf( pb, "CUNIT%d : %.18s", m + 1, in_buf[n].ax[m].cunit );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 32) {
            sprintf( pb, "DDELT%d : %f", m + 1, in_buf[n].ax[m].ddelt );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 16) {
            sprintf( pb, "DROTA%d : %f", m + 1, in_buf[n].ax[m].drota );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 8) {
            sprintf( pb, "DRPIX%d : %f", m + 1, in_buf[n].ax[m].drpix );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 4) {
            sprintf( pb, "DRVAL%d : %f", m + 1, in_buf[n].ax[m].drval );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 2) {
            sprintf( pb, "DTYPE%d : %.18s", m + 1, in_buf[n].ax[m].dtype );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 1) {
            sprintf( pb, "DUNIT%d : %.18s", m + 1, in_buf[n].ax[m].dunit );
            anyout_c( &d, tofchar( pb ) );
         }
         sprintf( pb, "DEF    : %d", in_buf[n].ax[m].def );
         anyout_c( &d, tofchar( pb ) );
         sprintf( pb, "NPOS   : %d", in_buf[n].ax[m].npos );
         anyout_c( &d, tofchar( pb ) );
         sprintf( pb, "LOW    : %d", in_buf[n].ax[m].low );
         anyout_c( &d, tofchar( pb ) );
         sprintf( pb, "UPP    : %d", in_buf[n].ax[m].upp );
         anyout_c( &d, tofchar( pb ) );
      }
   }
   anyout_c( &d, tofchar( "Contents of GDSOUT buffer" ) );
   for (n = 0; n < out_buf_size; n++) {
      sprintf( pb, "KEYWORD: %.20s", out_buf[n].key );
      anyout_c( &d, tofchar( pb ) );
      sprintf( pb, "SET    : %.20s", out_buf[n].set );
      anyout_c( &d, tofchar( pb ) );
      sprintf( pb, "NAXIS  : %d", out_buf[n].naxis );
      anyout_c( &d, tofchar( pb ) );
      sprintf( pb, "SUBDIM : %d", out_buf[n].subdim );
      anyout_c( &d, tofchar( pb ) );
      for (m = 0; m < out_buf[n].naxis; m++) {
         fint pmask = out_buf[n].ax[m].pmask;
         fint smask = out_buf[n].ax[m].smask;

         sprintf( pb, "NAXIS%d : %d", m + 1, out_buf[n].ax[m].naxis );
         anyout_c( &d, tofchar( pb ) );
         if (pmask & 32) {
            sprintf( pb, "CDELT%d : %f", m + 1, out_buf[n].ax[m].cdelt );
            anyout_c( &d, tofchar( pb ) );
         }
         if (pmask & 16) {
            sprintf( pb, "CROTA%d : %f", m + 1, out_buf[n].ax[m].crota );
            anyout_c( &d, tofchar( pb ) );
         }
         if (pmask & 8) {
            sprintf( pb, "CRPIX%d : %f", m + 1, out_buf[n].ax[m].crpix );
            anyout_c( &d, tofchar( pb ) );
         }
         if (pmask & 4) {
            sprintf( pb, "CRVAL%d : %f", m + 1, out_buf[n].ax[m].crval );
            anyout_c( &d, tofchar( pb ) );
         }
         if (pmask & 2) {
            sprintf( pb, "CTYPE%d : %.18s", m + 1, out_buf[n].ax[m].ctype );
            anyout_c( &d, tofchar( pb ) );
         }
         if (pmask & 1) {
            sprintf( pb, "CUNIT%d : %.18s", m + 1, out_buf[n].ax[m].cunit );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 32) {
            sprintf( pb, "DDELT%d : %f", m + 1, out_buf[n].ax[m].ddelt );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 16) {
            sprintf( pb, "DROTA%d : %f", m + 1, out_buf[n].ax[m].drota );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 8) {
            sprintf( pb, "DRPIX%d : %f", m + 1, out_buf[n].ax[m].drpix );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 4) {
            sprintf( pb, "DRVAL%d : %f", m + 1, out_buf[n].ax[m].drval );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 2) {
            sprintf( pb, "DTYPE%d : %.18s", m + 1, out_buf[n].ax[m].dtype );
            anyout_c( &d, tofchar( pb ) );
         }
         if (smask & 1) {
            sprintf( pb, "DUNIT%d : %.18s", m + 1, out_buf[n].ax[m].dunit );
            anyout_c( &d, tofchar( pb ) );
         }
         sprintf( pb, "DEF    : %d", out_buf[n].ax[m].def );
         anyout_c( &d, tofchar( pb ) );
         sprintf( pb, "NPOS   : %d", out_buf[n].ax[m].npos );
         anyout_c( &d, tofchar( pb ) );
         sprintf( pb, "LOW    : %d", out_buf[n].ax[m].low );
         anyout_c( &d, tofchar( pb ) );
         sprintf( pb, "UPP    : %d", out_buf[n].ax[m].upp );
         anyout_c( &d, tofchar( pb ) );
      }
   }
}

/*

#>            gdsasn.dc2

Function:     GDSASN

Purpose:      GDSASN copies the coordinate system of a previously opened
              input set obtained with GDSINP to the output set to be
              obtained with GDSOUT.

Category:     USER IO

File:         gdsinp.c

Author:       K.G. Begeman

Use:          CALL GDSASN( KEYIN  ,    Input      character*(*)
                           KEYOUT ,    Input      character*(*)
                           CLASS  )    Input      integer

              KEYIN      Keyword associated with the GDSINP call.
              KEYOUT     Keyword associated with the GDSOUT call.
              CLASS      Class of output set.

Description:  GDSASN is needed before each call to GDSOUT to obtain
              an output set. GDSASN copies the axis names from a
              set previously opened by GDSINP in the order as determined
              by the user at the associated GDSINP request. This buffer
              will be read by GDSOUT, which will create an output set
              according to the specifications in this buffer. Routines like
              GDSCSS and GDSCPA allow the application programmer to
              change the subset size and change the coordinate system of
              the output set.

Related Docs: gdsout.dc2, gdscss.dc2, gdscpa.dc2, gdscsa.dc2

Updates:      Jan 24, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine gdsasn( character, character, integer )

*/

void gdsasn_c( fchar keyin, fchar keyout, fint *class )
{
   char        kbufi[MAXKEYLEN];
   char        kbufo[MAXKEYLEN];
   fint       *axnum;
   fint        change = 0;
   fint        fatal = 4;
   fint        i;
   fint        ki = -1;
   fint        ko = out_buf_size;
   fint        m;
   fint        n;
   fint        naxis = 0;
   set_struct  set_info;

   for (i = 0; i < keyin.l && i < MAXKEYLEN; i++) kbufi[i] = keyin.a[i];
   while (i < MAXKEYLEN) kbufi[i++] = ' ';
   for (i = 0; i < keyout.l && i < MAXKEYLEN; i++) kbufo[i] = keyout.a[i];
   while (i < MAXKEYLEN) kbufo[i++] = ' ';
   for (n = 0; n < in_buf_size; n++) {
      if (!strncmp( kbufi, in_buf[n].key, MAXKEYLEN )) ki = n;
   }
   if (ki == -1) error_c( &fatal, tofchar( "GDSASN <UNrelated KEYIN>" ) );
   for (n = 0; n < out_buf_size; n++) {
      if (!strncmp( kbufo, out_buf[n].key, MAXKEYLEN )) ko = n;
   }
   set_info = in_buf[ki];
   set_info.ip = ki;                            /* pointer to entry in in_buf */
   set_info.exist = 0;                            /* set does not (yet) exist */
   for (i = 0; i < MAXKEYLEN; i++) set_info.key[i] = kbufo[i];  /* GDSOUT key */
   if (*class == 1) {
      change = 0;                                      /* CLASS 1 application */
      naxis = set_info.naxis;
   } else if (*class == 2) {
      change = 1;                                      /* CLASS 2 application */
      naxis = set_info.subdim;
   } else {
      error_c( &fatal, tofchar( "GDSASN <CLASS != 1 AND CLASS != 2>" ) );
   }
   set_info.naxis = naxis;
   set_info.change = change;
   axnum = (fint *) calloc( in_buf[ki].naxis, sizeof( fint ) );
   set_info.ax = (ax_struct *) calloc( set_info.naxis, sizeof( ax_struct ) );
   m = 0;
   for (n = 0; n < in_buf[ki].naxis; n++) {              /* get axis sequence */
      if (in_buf[ki].ax[n].def > 0) {
         axnum[in_buf[ki].ax[n].def+in_buf[ki].subdim-1] = n;
      } else {
         axnum[m++] = n;
      }
   }
   for (n = 0; n < naxis; n++) {
      set_info.ax[n] = in_buf[ki].ax[axnum[n]];
   }
   if (*class == 2) {                                       /* hide some axes */
      set_info.maxis = 0;
      set_info.hx = NULL;
      for (n = naxis; n < in_buf[ki].naxis; n++) {
         fint  np;
         fint  npos = in_buf[ki].ax[axnum[n]].npos;
         fint8 *pos = in_buf[ki].ax[axnum[n]].pos;
         fint  sum = 0;
         fint  pmask = 0;
         fint  smask = 0;
         
         set_info.hx = (ax_struct *) realloc( (char *) set_info.hx,
            sizeof( ax_struct ) * (set_info.maxis + 1) );
         for (np = 0; np < npos; sum += pos[np++]);
         pmask = in_buf[ki].ax[axnum[n]].pmask;
         if (pmask & 32) {
            set_info.hx[set_info.maxis].cdelt = in_buf[ki].ax[axnum[n]].cdelt;
         }
         if (pmask & 16) {
            set_info.hx[set_info.maxis].crota = in_buf[ki].ax[axnum[n]].crota;
         }
         if (pmask & 8) {
            set_info.hx[set_info.maxis].crpix = in_buf[ki].ax[axnum[n]].crpix +
               (double) in_buf[ki].ax[axnum[n]].low  - (double) sum / (double) npos;
         }
         if (pmask & 4) {
            set_info.hx[set_info.maxis].crval = in_buf[ki].ax[axnum[n]].crval;
         }
         if (pmask & 2) {
            strncpy( set_info.hx[set_info.maxis].ctype,
               in_buf[ki].ax[axnum[n]].ctype, MAXFTSNAMLEN );
         }
         if (pmask & 1) {
            strncpy( set_info.hx[set_info.maxis].cunit,
               in_buf[ki].ax[axnum[n]].cunit, MAXFTSNAMLEN );
         }
         set_info.hx[set_info.maxis].pmask = pmask;
         smask = in_buf[ki].ax[axnum[n]].smask;
         if (smask & 32) {
            set_info.hx[set_info.maxis].ddelt = in_buf[ki].ax[axnum[n]].ddelt;
         }
         if (smask & 16) {
            set_info.hx[set_info.maxis].drota = in_buf[ki].ax[axnum[n]].drota;
         }
         if (smask & 8) {
            set_info.hx[set_info.maxis].drpix = in_buf[ki].ax[axnum[n]].drpix +
               (double) in_buf[ki].ax[axnum[n]].low  - (double) sum / (double) npos;
         }
         if (smask & 4) {
            set_info.hx[set_info.maxis].drval = in_buf[ki].ax[axnum[n]].drval;
         }
         if (smask & 2) {
            strncpy( set_info.hx[set_info.maxis].dtype,
               in_buf[ki].ax[axnum[n]].dtype, MAXFTSNAMLEN );
         }
         if (smask & 1) {
            strncpy( set_info.hx[set_info.maxis].dunit,
               in_buf[ki].ax[axnum[n]].dunit, MAXFTSNAMLEN );
         }
         set_info.hx[set_info.maxis].smask = smask;
         set_info.maxis += 1;               /* increase number of hidden axes */
      }
      for (n = 0; n < in_buf[ki].maxis; n++) {
         set_info.hx = (ax_struct *) realloc( (char *) set_info.hx,
            sizeof( ax_struct ) * (set_info.maxis + 1) );
         set_info.hx[set_info.maxis] = in_buf[ki].hx[n];
         set_info.maxis += 1;               /* increase number of hidden axes */
      }
   }
   if (ko==out_buf_size) {
      out_buf = (set_struct *) realloc( (char *) out_buf,
         sizeof( set_struct ) * (out_buf_size + 1) );
         out_buf_size++;
   }
   out_buf[ko] = set_info;
   free( axnum );
}

/*

#>            gdscss.dc2

Function:     GDSCSS

Purpose:      GDSCSS changes the size of the subsets of the output set.

Category:     USER IO

File:         gdsinp.c

Author:       K.G. Begeman

Use:          CALL GDSCSS( KEYOUT ,   Input     character*(*)
                           BLO    ,   Input     integer array
                           BHI    )   Input     integer array

              KEYOUT      Keyword associated with a call to GDSOUT
              BLO         Array which contains grids of the new
                          lower edge of the subset. The number of
                          grids in BLO must be equal to the subset
                          dimension.
              BHI         Array which contains grids of the new
                          upper edge of the subset. The number of
                          grids in BHI must be equal to the subset
                          dimension.

Description:  After a call to GDSINP to obtain an input set and after
              a call to GDSASN to define the coordinate frame of the
              output set, GDSCSS can be used to change the subset
              frame. The input edges of the output subset could be
              obtained by BOXINP.

Related Docs: gdsinp.dc2, gdsout.dc2, gdsasn.dc2, gdscpa.dc2, gdscsa.dc2

Updates:      Jan 24, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine gdscss( character, integer, integer )

*/

void gdscss_c( fchar keyout, fint *blo, fint *bhi )
{
   char kbuf[MAXKEYLEN];
   fint i;
   fint fatal = 4;
   fint k = -1;
   fint n;

   for (i = 0; i < keyout.l && i < MAXKEYLEN; i++) kbuf[i] = keyout.a[i];
   while (i < MAXKEYLEN) kbuf[i++] = ' ';
   for (n = 0; n < out_buf_size; n++) {
      if (!strncmp( kbuf, out_buf[n].key, MAXKEYLEN )) k = n;
   }
   if (k == -1) error_c( &fatal, tofchar( "GDSCSS <UNrelated KEYOUT>" ) );
   for (n = 0; n < out_buf[k].subdim; n++) {
      out_buf[k].ax[n].naxis = bhi[n] - blo[n] + 1;
      out_buf[k].ax[n].crpix += (double) ( out_buf[k].ax[n].low - blo[n] );
      out_buf[k].ax[n].low = blo[n];
      out_buf[k].ax[n].upp = bhi[n];
   }
}

/*

#>            gdscpa.dc2

Function:     GDSCPA

Purpose:      GDSCPA changes the primary axis of an output set to be
              obtained by GDSOUT.

Category:     USER IO

File:         gdsinp.c

Author:       K.G. Begeman

Use:          CALL GDSCPA( KEYOUT,    Input    CHARACTER*(*)
                           AXNUM ,    Input    INTEGER
                           NAXIS ,    Input    INTEGER
                           CDELT ,    Input    DOUBLE PRECISION
                           CROTA ,    Input    DOUBLE PRECISION
                           CRPIX ,    Input    DOUBLE PRECISION
                           CRVAL ,    Input    DOUBLE PRECISION
                           CTYPE ,    Input    CHARACTER*(*)
                           CUNIT ,    Input    CHARACTER*(*)
                           PMASK )    Input    INTEGER

              KEYOUT     Keyword associated with a GDSOUT call.
              AXNUM      The axis number of the axis to be changed.
              NAXIS      Size of the axis.
              CDELT      Increment in physical units along axis.
              CROTA      Rotation angle of axis.
              CRPIX      Reference pixel of axis.
              CRVAL      Physical reference value at reference pixel.
              CTYPE      Axis name.
              CUNIT      Physical units of axis.
              PMASK      Bit mask denoting which of the six above values
                         are defined. The codes are as follows:
                         item        bit      add
                         CDELT        5        32
                         CROTA        4        16
                         CRPIX        3         8
                         CRVAL        2         4
                         CTYPE        1         2
                         CUNIT        0         1
                         So PMASK = 14 (2 + 4 + 8) means that CTYPE,
                         CRPIX and CRVAL are defined. These values must
                         always be defined if an axis is added to the
                         output set.

Related Docs: gdsinp.dc2, gdsout.dc2, gdsasn.dc2, gdscss.dc2, gdscsa.dc2

Updates:      Jan 25, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine gdscpa( character       ,
@                    integer         ,
@                    integer         ,
@                    double precision,
@                    double precision,
@                    double precision,
@                    double precision,
@                    character       ,
@                    character       ,
@                    integer         )

*/

void gdscpa_c( fchar   keyout,
               fint   *axnum ,
               fint   *naxis ,
               double *cdelt ,
               double *crota ,
               double *crpix ,
               double *crval ,
               fchar   ctype ,
               fchar   cunit ,
               fint   *pmask )
{
   char kbuf[MAXKEYLEN];
   fint fatal = 4;
   fint i;
   fint k = -1;
   fint n;
   fint ndim;

   for (i = 0; i < keyout.l && i < MAXKEYLEN; i++) kbuf[i] = keyout.a[i];
   while (i < MAXKEYLEN) kbuf[i++] = ' ';
   for (n = 0; n < out_buf_size; n++) {
      if (!strncmp( kbuf, out_buf[n].key, MAXKEYLEN )) k = n;
   }
   if (k == -1) error_c( &fatal, tofchar( "GDSCPA <UNrelated KEYOUT>" ) );
   ndim = out_buf[k].naxis;
   if ((*axnum - ndim) > 1) {
      error_c( &fatal, tofchar( "GDSCPA <AXNUM too large>" ) );
   } else if ((*axnum - ndim) == 1) {
      if (!out_buf[k].change) error_c( &fatal, tofchar( "GDSCPS <CLASS == 1>" ) );
      if (!(*pmask & 14)) error_c( &fatal, tofchar( "GDSCPA <wrong PMASK>" ) );
   }
   if (*axnum > ndim) {
      out_buf[k].ax = (ax_struct *) realloc( (char *) out_buf[k].ax,
         sizeof( ax_struct ) * (ndim + 1) );
      out_buf[k].naxis += 1;
   }
   if (*axnum > out_buf[k].subdim) out_buf[k].change = 1;
   n = *axnum - 1;
   out_buf[k].ax[n].naxis = *naxis;
   if (*pmask & 1) {
      for (i = 0; i < cunit.l && i < MAXFTSNAMLEN; i++) {
         out_buf[k].ax[n].cunit[i] = cunit.a[i];
      }
      while (i < MAXFTSNAMLEN) out_buf[k].ax[n].cunit[i++] = ' ';
   }
   if (*pmask & 2) {
      for (i = 0; i < ctype.l && i < MAXFTSNAMLEN; i++) {
         out_buf[k].ax[n].ctype[i] = ctype.a[i];
      }
      while (i < MAXFTSNAMLEN) out_buf[k].ax[n].ctype[i++] = ' ';
   }
   if (*pmask & 4) out_buf[k].ax[n].crval = *crval;
   if (*pmask & 8) out_buf[k].ax[n].crpix = *crpix;
   if (*pmask & 16) out_buf[k].ax[n].crota = *crota;
   if (*pmask & 32) out_buf[k].ax[n].cdelt = *cdelt;
   out_buf[k].ax[n].pmask |= *pmask;
/* out_buf[k].ax[n].smask = 0; */		/* KGB Jul 29, 1991 */
   if (n < out_buf[k].subdim) {                                /* subset axis */
      out_buf[k].ax[n].def = 0;
   } else {                                            /* axis outside subset */
      out_buf[k].ax[n].def = n - out_buf[k].subdim + 1;
   }
   out_buf[k].ax[n].npos = 0;
   out_buf[k].ax[n].pos = NULL;
   out_buf[k].ax[n].low = 1 - NINT( out_buf[k].ax[n].crpix );
   out_buf[k].ax[n].upp = *naxis - NINT( out_buf[k].ax[n].crpix );
}

/*

#>            gdscsa.dc2

Function:     GDSCSA

Purpose:      GDSCSA changes the secondary axis of an output set to be
              obtained by GDSOUT.

Category:     USER IO

File:         gdsinp.c

Author:       K.G. Begeman

Use:          CALL GDSCSA( KEYOUT,    Input    character*(*)
                           AXNUM ,    Input    integer
                           DDELT ,    Input    double precision
                           DROTA ,    Input    double precision
                           DRPIX ,    Input    double precision
                           DRVAL ,    Input    double precision
                           DTYPE ,    Input    character*(*)
                           DUNIT ,    Input    character*(*)
                           SMASK )    Input    integer

              KEYOUT     Keyword associated with a GDSOUT call.
              AXNUM      The axis number of the axis to be changed.
              DDELT      Increment in physical units along axis.
              DROTA      Rotation angle of axis.
              DRPIX      Reference pixel of axis.
              DRVAL      Physical reference value at reference pixel.
              DTYPE      Axis name.
              DUNIT      Physical units of axis.
              SMASK      Bit mask denoting which of the six above values
                         are defined. The codes are as follows:
                         item        bit      add
                         DDELT        5        32
                         DROTA        4        16
                         DRPIX        3         8
                         DRVAL        2         4
                         DTYPE        1         2
                         DUNIT        0         1
                         So SMASK = 14 (2 + 4 + 8) means that DTYPE,
                         DRPIX and DRVAL are defined.
                         Note: If SMASK==0, the secondary axis is removed.

Related Docs: gdsinp.dc2, gdsout.dc2, gdsasn.dc2, gdscss.dc2, gdscpa.dc2

Updates:      Jan 25, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine gdscsa( character       ,
@                    integer         ,
@                    double precision,
@                    double precision,
@                    double precision,
@                    double precision,
@                    character       ,
@                    character       ,
@                    integer         )

*/

void gdscsa_c( fchar   keyout,
               fint   *axnum ,
               double *ddelt ,
               double *drota ,
               double *drpix ,
               double *drval ,
               fchar   dtype ,
               fchar   dunit ,
               fint   *smask )
{
   char kbuf[MAXKEYLEN];
   fint fatal = 4;
   fint i;
   fint k = -1;
   fint n;
   fint naxis;

   for (i = 0; i < keyout.l && i < MAXKEYLEN; i++) kbuf[i] = keyout.a[i];
   while (i < MAXKEYLEN) kbuf[i++] = ' ';
   for (n = 0; n < out_buf_size; n++) {
      if (!strncmp( kbuf, out_buf[n].key, MAXKEYLEN )) k = n;
   }
   if (k == -1) error_c( &fatal, tofchar( "GDSCSA <UNrelated KEYOUT>" ) );
   naxis = out_buf[k].naxis;
   if ((*axnum - naxis) > 0) {
      error_c( &fatal, tofchar( "GDSCSA <AXNUM too large>" ) );
   }
   n = *axnum - 1;
   if (*smask & 1) {
      for (i = 0; i < dunit.l && i < MAXFTSNAMLEN; i++) {
         out_buf[k].ax[n].dunit[i] = dunit.a[i];
      }
      while (i < MAXFTSNAMLEN) out_buf[k].ax[n].dunit[i++] = ' ';
   }
   if (*smask & 2) {
      for (i = 0; i < dtype.l && i < MAXFTSNAMLEN; i++) {
         out_buf[k].ax[n].dtype[i] = dtype.a[i];
      }
      while (i < MAXFTSNAMLEN) out_buf[k].ax[n].dtype[i++] = ' ';
   }
   if (*smask & 4) out_buf[k].ax[n].drval = *drval;
   if (*smask & 8) out_buf[k].ax[n].drpix = *drpix;
   if (*smask & 16) out_buf[k].ax[n].drota = *drota;
   if (*smask & 32) out_buf[k].ax[n].ddelt = *ddelt;
   if (*smask) {
      out_buf[k].ax[n].smask |= *smask;
   } else {
      out_buf[k].ax[n].smask = 0;		/* KGB Jul 29, 1991 */
   }
}

/*

#>            gdscas.dc2

Function:     GDSCAS

Purpose:      Changes the subset set selection for CLASS 1 applications.

Category:     USER IO

File:         gdsinp.c

Author:       K.G. Begeman

Use:          CALL GDSCAS( GDSOUTKEY ,   Input    CHARACTER*(*)
                           SELSUBSET ,   Input    INTEGER ARRAY
                           AXCOUNT   )   Input    INTEGER ARRAY

              GDSOUTKEY    Keyword associated with a following call
                           to GDSOUT.
              SELSUBSET    Array with selected input subset coordinate
                           words.
              AXCOUNT      Axcount array as would be returned by GDSOUT.

Description:  Function GDSCAS changes the subset axis selection and is
              used by applications which are essentially CLASS 2 but
              leave the names of the operation axes intact. VELSMO is
              an example program which demonstrates the use of GDSCAS.

Updates:      Mar  6, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine gdscas( character, integer*8, integer )

*/

void gdscas_c( fchar gdsoutkey, fint8 *selsubset, fint *axcount )
{
   char   kbuf[MAXKEYLEN];
   fchar  orig_set;
   fint  *orig_axnum;
   fint   fatal = 4;
   fint   i;
   fint   k = -1;
   fint   l;
   fint   n;
   fint   na;
   fint   np;
   fint   ns;

   for (i = 0; i < gdsoutkey.l && i < MAXKEYLEN; i++) kbuf[i] = gdsoutkey.a[i];
   while (i < MAXKEYLEN) kbuf[i++] = ' ';
   for (n = 0; n < out_buf_size; n++) {
      if (!strncmp( kbuf, out_buf[n].key, MAXKEYLEN )) k = n;
   }
   if (k == -1) error_c( &fatal, tofchar( "GDSCAS <UNrelated KEYOUT>" ) );
   if (out_buf[k].change) {
      error_c( &fatal, tofchar( "GDSCAS <CLASS != 1>" ) );
   }
   if (out_buf[k].naxis == out_buf[k].subdim) {
      error_c( &fatal, tofchar( "GDSCAS <SET == SUBSET>" ) );
   }
   orig_axnum = calloc( out_buf[k].naxis - out_buf[k].subdim, sizeof( fint ) );
   l = out_buf[k].ip;                             /* pointer to gdsinp buffer */
   orig_set.a = in_buf[l].set; orig_set.l = MAXSETNAMLEN;       /* gdsinp set */
   for (n = out_buf[k].subdim; n < out_buf[k].naxis; n++) {
      out_buf[k].ax[n].pos = calloc( axcount[n], sizeof( fint ) );
      out_buf[k].ax[n].npos = axcount[n];
   }
   for (n = 0; n < in_buf[l].naxis; n++) {
      if (in_buf[l].ax[n].def > 0) {
         orig_axnum[in_buf[l].ax[n].def-1] = n + 1;
      }
   }
   na = 0;
   ns = 1;
   for (n = out_buf[k].subdim; n < out_buf[k].naxis; n++) {
      fint an = orig_axnum[na++];
      fint cp = 0;
      
      for (np = 0; np < axcount[n]; np++) {
         fint gerror = 0;

         out_buf[k].ax[n].pos[np] = gdsc_grid_c( orig_set, &an, &selsubset[cp], &gerror );
         cp += ns;
      }
      ns *= axcount[n];
   }
   free( orig_axnum );
}
