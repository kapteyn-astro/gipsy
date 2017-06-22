/* gdsi_read.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990, 1991,
                      Kapteyn Astronomical Institute 2011
        All Rights Reserved.

        This source contains the code to access the GDS data sets.
        It read/writes single precision floating point numbers to/from
        disk. At the moment no conversion of floating point type is done,
        but the software has already been included (although it will not
        be compiled). It is assumed that the type of floating point on
        the current machine is defined in OS_FLOATING_TYPE, and that the
        type of floating point in the data set can be obtained from the
        set descriptor. At the moment the first 8 bits of spare_long[0]
        are used to determine the floating point type (gives us 256
        possible types). The routines spfpfl and spfplf take care of the
        conversion.

        No locking is done. Applications can read/write the same data
        set without checking whether another application is reading or
        writing the same data. If locking is wanted, it could simply be
        implemented by placing a call to gds_lock_c before and
        gds_unlock_c after the call to read/write.

        Mar 01, 2011:
        Use off_t for offsets in image files so that larger files can be
        handled if the system supports them. Coordinate words still are fints.

*/


#define FPC                           /* floating point conversion is wanted */

#include        <errno.h>
#include        <stdio.h>
#include        <stdlib.h>
#include        <unistd.h>
#include        <string.h>
#include        <sys/file.h>
#include        <fcntl.h>

#include        "gipsyc.h"              /* GIPSY symbols and definitions */
#include        "error.h"               /* defines error_c */
#include        "gdsparams.h"           /* some GDS parameters */
#include        "gdserrors.h"           /* GDS error codes */
#include        "gdsc_grid.h"           /* defines gdsc_grid_c */
#include        "gdsc_ndims.h"          /* defines gdsc_ndims_c */
#include        "gdsc_range.h"          /* defines gdsc_range_c */
#include        "nelc.h"                /* defines nelc_c */
#include        "setnfblank.h"          /* defines setnfblank_c */
#include        "gds___error.h"
#include        "gdsd_basic.h"
#include        "gds_ftype.h"
#include        "presentn.h"
#ifdef  FPC                             /* floating point conversion */
#include        "spfpfl.h"              /* define spfpfl_c */
#include        "spfplf.h"              /* define spfplf_c */
#endif                                  /* floating point conversion */

#define NFLOATS 65536                   /* number of floats in block */

#define E_TID   0                       /* tid not used */
#define R_TID   1                       /* tid used for reads */
#define W_TID   2                       /* tid used for writes */

typedef struct {                        /* bookkeeping per transfer */
   fint dims;                           /* dimension of database */
   off_t fptr;                          /* points to next file position */
   fint8 left[GDS_MAXDIM];               /* counters along sub database */
   fint8 size[GDS_MAXDIM];               /* size of sub database */
   fint8 step[GDS_MAXDIM];               /* step when overflow */
   fint8 todo;                           /* number of pixels still to handle */
#ifdef  FPC                             /* floating point conversion */
   fint type;                           /* floating point type */
#endif                                  /* floating point conversion */
   int  ufid;                           /* unix file descriptor */
   int  qtid;                           /* tid for reads/writes */
} tid_struct;

static  fint            ntid = 0;       /* number of table entries */
static  tid_struct      *tids = NULL;   /* table with transfer id's */

static  float           floats[NFLOATS];        /* buffer with floats */


/*
 * gdsi_close is called by the lower level gds routines when the
 * file is closed. It is used to clean up the transfer id bookkeeping.
 */

static  void    gdsi_close( int fi )    /* image file descriptor */
{
   fint itid;                   /* tid counter */
   for (itid = 0; itid < ntid; itid++) {
      if (tids[itid].qtid != E_TID && tids[itid].ufid == fi) {
         tids[itid].todo = 0;   /* nothing left to transfer */
         tids[itid].qtid = E_TID;       /* empty tid */
      }
   }
}


/*
 * initfptr sets up the bookkeeping for the data transfer. We keep
 * track of the number of pixels along each box axis, the number of
 * pixels to transfer, the parts to skip and the position in the data file.
 * initfptr returns the setting in the transfer struct.
 */

static  tid_struct      initfptr( fchar set ,   /* the set name */
                                  fint8  *cwlo , /* lower c.w. */
                                  fint8  *cwup ) /* upper c.w. */
{
   fint8         b1;                     /* lower box limit */
   fint8         b2;                     /* upper box limit */
   fint8         f1;                     /* lower frame limit */
   fint8         f2;                     /* upper frame limit */
   fint8         ftotal = 1;             /* pixel counter */
   fint         gerror = 0;             /* GDS error return */
   fint         n;                      /* counter */
   fint8         zero = 0;               /* just zero */
   tid_struct   r;                      /* return value */

   r.dims = gdsc_ndims_c( set, &zero ); /* get number of axes */
   r.fptr = 0;                          /* reset file pointer */
   r.todo = 1;                          /* at least one */
   if (!tobool( presentn_c( cwlo ) )) { /* argument not present */
      b1 = 0;                           /* whole set */
   } else {                             /* argument present */
      b1 = (*cwlo);                     /* from argument list */
   }
   if (!tobool(presentn_c( cwup ) )) {  /* argument not present */
      gdsc_range_c( set, &b1, &b1, &b2, &gerror );
   } else {                             /* argument present */
      b2 = (*cwup);                     /* from argument list */
   }
   gdsc_range_c( set, &zero, &f1, &f2, &gerror );
   for (n = 0; n < r.dims; n++) {       /* loop to initialize the struct */
      fint      axnum = n + 1;          /* the axis number */
      fint8      blo;                    /* lower box grid */
      fint8      bup;                    /* upper box grid */
      fint8      bsize;                  /* number of box grids */
      fint8      flo;                    /* lower frame grid */
      fint8      fup;                    /* upper frame grid */
      fint8      fsize;                  /* number of frame grids */

      blo = gdsc_grid_c( set, &axnum, &b1, &gerror );
      bup = gdsc_grid_c( set, &axnum, &b2, &gerror );
      /*
       * The following statement checks whether the programmer
       * wants to read/write in reverse direction. Since that is
       * not allowed, we silently change the direction.
       * Only reads and writes along increasing axis coordinates
       * are allowed!
       */
      if (blo > bup) { fint8 sav = blo; blo = bup; bup = sav; }
      flo = gdsc_grid_c( set, &axnum, &f1, &gerror );
      fup = gdsc_grid_c( set, &axnum, &f2, &gerror );
      bsize = bup - blo + 1;            /* size of box */
      fsize = fup - flo + 1;            /* size of frame */
      r.fptr += ftotal * ( blo - flo ); /* update file pointer */
      r.left[n] = bsize;                /* left to do on this line */
      r.size[n] = bsize;                /* length of line */
      r.step[n] = ftotal * ( fsize - bsize );
      r.todo *= bsize;                  /* update */
      ftotal *= fsize;                  /* update */
   }
   if (gerror < 0) gds___error_c( &gerror );
   return( r );                         /* return to caller */
}


/*
 * movefptr places the pointer to the next file position inside the
 * box if the current file position is outside this box. It is not checked
 * whether the transfer is completed. This has to be checked by the
 * calling routine.
 * movefptr returns the number of file positions advanced. Note that
 * file positions must be multiplied by sizeof( float ) to get the
 * position in bytes.
 */

static  off_t    movefptr( tid_struct *r )       /* the transfer struct */
{
   fint n = 0;                          /* loop counter */
   off_t fptr = r->fptr;                /* current file position */

   while (r->left[n] == 0 && n < r->dims) {     /* loop trough dimensions */
      r->left[n] = r->size[n];          /* reset */
      r->fptr += r->step[n];            /* move file pointer forward */
      r->left[++n] -= 1;                /* one less left */
   }
   return( r->fptr - fptr );            /* return pointer movement */
}


/*
 * nextfptr returns the next file position where we can find data to
 * transfer. It returns the number of pixels which can be transfered.
 * This routine is called by the actual read and write routines in a loop.
 * Loop control is done with the number of pixels left to transfer.
 */

static  off_t   nextfptr( tid_struct *r ,       /* transfer structure */
                          fint8       *nbuf ,    /* number of pixels to do */
                          fint8       *np )      /* pixels we can do */
{
   off_t fptr = r->fptr;                /* current file position */
   fint8 step;                           /* the step size */

   if ((*nbuf) > r->todo) (*nbuf) = r->todo;
   (*np) = 0;                           /* reset */
   do {                                 /* loop */
      if (r->left[0] < (*nbuf)) {       /* not enough left on this line */
         (*np) += step = r->left[0];    /* the step */
      } else {                          /* enough on this line */
         (*np) += step = (*nbuf);       /* the step */
      }
      (*nbuf) -= step;                  /* decrease */
      r->todo -= step;                  /* decrease */
      r->left[0] -= step;               /* decrease */
      r->fptr += step;                  /* increase */
   } while (movefptr( r ) == 0 && (*nbuf));     /* etc */
   return( fptr );                      /* return old file position */
}


/*
 * gdsi_open tries to get the image file descriptor which is stored in the
 * set status block. This ensures that we have only one file descriptor per set.
 * gdsi_open returns the transfer id or an error code.
 * At the moment the memory associated with transfer id's is not released
 * if the transfer has been completed!!
 */

static  fint    gdsi_open( fchar set ,          /* name of set */
                           fint8  *cwlo ,        /* lower c.w. of box */
                           fint8  *cwup ,        /* upper c.w. of box */
                           int   qtid )         /* read or write tid */
{
   fint         itid = 0;               /* transfer id */
   tid_struct   newtid;                 /* transfer struct */
   int          fi;                     /* image file descriptor */
   fint         err_i=0;

   fi = gds___image(set, gdsi_close);
   if (!fi) return GDS_IOPENFAIL;
   newtid = initfptr( set, cwlo, cwup );/* get new tid struct */
   newtid.ufid = fi;                    /* image file descriptor */
   newtid.qtid = qtid;                  /* set type of tid */
#ifdef  FPC                             /* floating point conversion */
   /*
    * get type of floating point.
    */
   newtid.type = gds_ftype_c(set, &err_i);
#endif                                  /* floating point conversion */
   while (itid < ntid && tids[itid].qtid != E_TID) itid++;
   if (itid == ntid) {                  /* no unused transfer struct found */
      tid_struct        *newtidptr;     /* create new one */

      newtidptr = realloc( tids, ( ntid + 1 ) * sizeof( tid_struct ) );
      if (newtidptr == NULL) {          /* error */
         return( GDS_ALLOCFAIL );       /* severe error */
      } else {                          /* okay */
         tids = newtidptr;              /* new pointer */
         ntid += 1;                     /* increase */
      }
   }
   memcpy( &tids[itid], &newtid, sizeof( tid_struct ) );
   return( itid + 1 );                  /* return tranfer id */
}


/*

#>            gdsi_read.dc2

Function:     GDSI_READ

Purpose:      Reads data from (part of) a set.

Category:     DATA, IMAGE-IO

File:         gdsi_read.c

Author:       K.G. Begeman

Use:          CALL GDSI_READ( SET,                Input    CHARACTER*(*)
                              CLOW,               Input    INTEGER
                              CUPP,               Input    INTEGER
                              BUFFER,             Output   REAL ARRAY
                              BUF_LEN,            Input    INTEGER
                              PIXELS_DONE,        Output   INTEGER
                              TRANS_ID )          In/Out   INTEGER

              SET           set name
              CLOW          coordinate word of lower left corner of the
                            subset to read.
                            If omitted: read the whole set.
              CUPP          coordinate word of upper right corner of the
                            subset to read.
                            If omitted: CLOW is a subset to read.
              BUFFER        array where pixels are stored.
              BUF_LEN       max. number of pixels to read.
                            If omitted: 1 is assumed.
              PIXELS_DONE   number of pixels actually read in one call.
                            Not cumulative for a complete transfer.
                            If omitted: PIXELS_DONE must be equal to BUF_LEN,
                                        otherwise error code -30 will be
                                        returned in TRANS_ID.
              TRANS_ID      transfer identification.
                            = 0  read successful completed.
                            > 0  unique indentifer if read could not be
                                 completed. This unmodified TRANS_ID must be
                                 used until the read has been completed with
                                 TRANS_ID = 0.
                            < 0  a GDS error has been detected.
                                 Some special GDSI_READ error codes:
                                 -30 argument PIXELS_DONE omitted, but was
                                     unequal to BUF_LEN.
                                 -31 illegal transfer identifier.
                                 -32 Unable to allocate enough memory.
                                 -36 cannot open data file.
                                 -38 maximum open sets exceeded.

Related Docs: gdsi_write.dc2, gdsi_cancel.dc2.

Updates:      May 21, 1987: WZ, istalled.
              Dec 21, 1989: WZ, migrated to C.
              Jul  5, 1990: KGB, modified document.
              Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file.
              Aug 17, 1991: KGB, new algorithm, replaced fopen by open.
              Jan 31, 1994: JPT, attempt to open read-only if read-write fails.
              Mar 24, 1994: JPT, modified for GDS server.
              Jan 15, 1998: VOG, replaced tell() by more common lseek()
              May  1, 2007: JPT, conditional code for Apple Mac included.
              Mar  1, 2011: JPT, use off_t for file offsets.

#<

Fortran to C interface:

@ subroutine gdsi_read( character, integer*8, integer*8, real, integer, integer, integer)

*/

void    gdsi_read_c( fchar set ,                /* the set name */
                     fint8  *cwlo ,              /* lower coordinate word */
                     fint8  *cwup ,              /* upper coordinate word */
                     float *data ,              /* the data buffer */
                     fint  *size ,              /* size of buffer above */
                     fint  *done ,              /* number done */
                     fint  *tid )               /* transfer id */
{
   fint         ltid;                   /* local transfer id */
   fint8         ndone;                  /* number of pixels done */
   fint8         nleft;                  /* number of pixels left */
   fint8         nread;                  /* number done */
   fint8         ntotl;                  /* number to do */
   int          f;                      /* file descriptor */
   tid_struct   *ctid;                  /* transfer struct */

   if (tobool(presentn_c( tid ))) {     /* argument present */
      ltid = (*tid);                    /* then copy it */
   } else {                             /* argument not present */
      ltid = 0;                         /* make it zero */
   }
   if (ltid < 0) {                      /* error from previous call */
      gds___error_c( tid );             /* generate standard errors */
   } else if (ltid > ntid) {            /* illegal tid */
      (*tid) = -31;                     /* error */
      return;                           /* return to caller */
   }
   if (ltid == 0) ltid = gdsi_open( set, cwlo, cwup, R_TID );
   if (ltid < 0) {                      /* a fatal error */
      if (tobool(presentn_c( tid ))) {  /* argument was present */
         (*tid) = ltid;                 /* return to caller */
      } else {                          /* we do the error handling */
         gds___error_c( &ltid );        /* gds error routine */
      }
      return;                           /* return to caller */
   }
   if (tobool(presentn_c( size ))) {    /* argument defined */
      nleft = (*size);                  /* get it */
   } else {                             /* argument not defined */
      nleft = 1;                        /* default */
   }
   ctid = &tids[ltid-1];                /* get the struct */
   f = ctid->ufid;                      /* get file descriptor */
   nread = 0;                           /* reset */
   ntotl = nleft;                       /* number to do */
   do {
      off_t     eptr;                   /* end position */
      off_t     fptr = nextfptr( ctid, &nleft, &ndone );
      fint8      nptr;                   /* number of positions */
      fint8    nd;                     /* number of bytes done */
      fint8    nr;                     /* number of bytes to be read */

      eptr = lseek(f, 0, SEEK_END) / sizeof( float );   /* get end position */
      nptr = eptr - fptr;               /* number of positions */
      if (nptr > ndone) nptr = ndone;   /* decrease */
      if (nptr > 0) {                   /* we can read */
         lseek( f, fptr * sizeof( float ), SEEK_SET );
         nr = nptr * sizeof( float );   /* number of bytes */
#if     0
         nd = read( f, &data[nread], nr );      /* do the read */
#else
         while ((nd = read( f, &data[nread], nr )) == -1 && errno == EINTR);
#endif
         if (nd == -1) {                /* read error */
            char        *estr;          /* pointer to error string */
            fint        fatal = 4;      /* fatal error */

            estr = strerror( errno );
            error_c( &fatal, tofchar( estr ) );
         } else if (nd != nr) {         /* should not happen */
            fint        fatal = 4;      /* fatal error */

            error_c( &fatal, tofchar( "DATA READ ERROR!!" ) );
         }
#ifdef  FPC                             /* floating point conversion */
         if (OS_FLOATING_TYPE != ctid->type) {
        	fint nptr_fint;
            spfpfl_c( &ctid->type, &data[nread], &data[nread], &nptr_fint );
            nptr = nptr_fint;
         }
#endif                                  /* floating point conversion */
         nread += nptr;                 /* update */
         ndone -= nptr;                 /* update */
      }
      if (ndone) {                      /* not yet done */
    	 fint ndone_fint;
         setnfblank_c( &data[nread], &ndone_fint );  /* fill with blanks */
         ndone = ndone_fint ;
         nread += ndone;                /* update */
      }
   } while (nleft);                     /* until all done */
   if (!ctid->todo) {                   /* all done */
      ltid = 0;                         /* free transfer id */
      ctid->qtid = E_TID;               /* make it empty */
   }
   if (tobool(presentn_c( done ))) {    /* argument present */
      (*done) = nread;                  /* return to caller */
   } else {                             /* argument not present */
      if (nread != ntotl) ltid = -30;   /* error code */
   }
   if (tobool(presentn_c( tid ))) {     /* argument present */
      (*tid) = ltid;                    /* return transfer id */
   } else if (ltid) {                   /* not present and no zero */
      ltid = -30;                       /* error code */
      gds___error_c( &ltid );           /* GDS error routine */
   }
}


/*

#>            gdsi_write.dc2

Function:     GDSI_WRITE

Purpose:      Writes data to (part of) an set.

Category:     DATA, IMAGE-IO

File:         gdsi_read.c

Author:       K.G. Begeman

Use:          CALL  GDSI_WRITE( SET,             Input       CHARACTER*(*)
                                CLOW,            Input       INTEGER
                                CUPP,            Input       INTEGER
                                BUFFER,          Input       REAL ARRAY
                                BUF_LEN,         Input       INTEGER
                                PIXELS_DONE,     Output      INTEGER
                                TRANS_ID )       In/Out      INTEGER

              SET           set name
              CLOW          coordinate word of lower left corner of the
                            subset to write.
                            If omitted: write the whole set.
              CUPP          coordinate word of upper right corner of the
                            subset to write.
                            If omitted: CLOW is a subset to write.
              BUFFER        array with pixels to write.
              BUF_LEN       max. number of pixels to write.
                            If omitted: 1 is assumed.
              PIXELS_DONE   number of pixels actually written in one call.
                            Not cumulative for a complete transfer.
                            If omitted: PIXELS_DONE must be equal to BUF_LEN,
                                        otherwise error code -30 will be
                                        returned in TRANS_ID.
              TRANS_ID      transfer identification.
                            = 0  write successful completed.
                            > 0  unique indentifer if write could not be
                                 completed. This unmodified TRANS_ID must be
                                 used until the write has been completed with
                                 TRANS_ID = 0.
                            < 0  a GDS error has been detected.
                                 Some special GDSI_WRITE error codes:
                                 -30 argument PIXELS_DONE omitted, but was
                                     unequal to BUF_LEN.
                                 -31 illegal transfer identifier.
                                 -32 Unable to allocate enough memory.
                                 -36 cannot open data file.
                                 -38 maximum open sets exceeded.

Related Docs: gdsi_read.dc2, gdsi_cancel.dc2.

Updates:      May 21, 1987: WZ, installed.
              Dec 21, 1989: WZ, migrated to C.
              Jul  5, 1990: KGB, modified document.
              Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file.
              Aug 17, 1991: KGB, new algorithm, replace fopen by open.
              Jan 31, 1994: JPT, attempt to open read-only if read-write fails.
              Mar  1, 2011: JPT, use off_t for file offsets.

#<

Fortran to C interface:

@ subroutine gdsi_write( character, integer*8, integer*8, real, integer, integer, integer )

*/

void    gdsi_write_c( fchar set ,               /* the set name */
                      fint8  *cwlo ,             /* lower coordinate word */
                      fint8  *cwup ,             /* upper coordinate word */
                      float *data ,             /* the data to write */
                      fint  *size ,             /* size of buffer above */
                      fint  *done ,             /* number done */
                      fint  *tid )              /* transfer id */
{
   fint         ltid;                   /* local transfer id */
   fint8         ndone;                  /* number of pixels done */
   fint8         nleft;                  /* number of pixels left */
   fint8         ntotl;                  /* number of pixels to do */
   fint8         nwrit;                  /* counts number of pixels done */
   int          f;                      /* file descriptor */
   tid_struct   *ctid;                  /* transfer struct */

   /*anyoutf(1, "gds write: %lld %lld\n", cwlo, cwup, size);*/

   if (tobool(presentn_c( tid ))) {     /* argument present */
      ltid = (*tid);                    /* then copy it */
   } else {                             /* argument not present */
      ltid = 0;                         /* make it zero */
   }
   if (ltid < 0) {                      /* error from previous call */
      gds___error_c( tid );             /* generate standard errors */
   } else if (ltid > ntid) {            /* illegal tid */
      (*tid) = -31;                     /* error */
      return;                           /* return to caller */
   }
   if (ltid == 0) ltid = gdsi_open( set, cwlo, cwup, W_TID );
   if (ltid < 0) {                      /* a fatal error */
      if (tobool(presentn_c( tid ))) {  /* argument was present */
         (*tid) = ltid;                 /* return to caller */
      } else {                          /* we do the error handling */
         gds___error_c( &ltid );        /* gds error routine */
      }
      return;                           /* return to caller */
   }
   if (tobool(presentn_c( size ))) {    /* argument defined */
      nleft = (*size);                  /* get it */
   } else {                             /* argument not defined */
      nleft = 1;                        /* default */
   }
   ctid = &tids[ltid-1];                /* get the struct */
   f = ctid->ufid;                      /* get file descriptor */
   ntotl = nleft;                       /* number to do */
   nwrit = 0;                           /* reset */
   do {
      off_t     eptr;                   /* end position in image */
      off_t     fptr = nextfptr( ctid, &nleft, &ndone );
      size_t    nd;                     /* number of bytes done */
      size_t    nw;                     /* number of writes */

     /* lseek( f, 0, SEEK_END );*/              /* set at end position */
      eptr = lseek(f, 0, SEEK_END) / sizeof( float );   /* get end position */
      if (fptr > eptr) {                /* we skip a part */
         fint   nfloats = NFLOATS;      /* floats in buffer */

         setnfblank_c( floats, &nfloats );      /* fill with blanks */
#ifdef  FPC                             /* floating point conversion */
         if (OS_FLOATING_TYPE != ctid->type) {
            spfplf_c( &ctid->type, floats, floats, &nfloats );
         }
#endif                                  /* floating point conversion */
         while (eptr < fptr) {          /* blank fill loop */
            nfloats = fptr - eptr;      /* size of hole */
            if (nfloats > NFLOATS) nfloats = NFLOATS;
            nw = nfloats * sizeof( float );     /* number of bytes */
#if     0
            nd = write( f, floats, nw );/* do the write */
#else
            while ((nd = write( f, floats, nw )) == -1 && errno == EINTR);
#endif
            if (nd == -1) {             /* write error */
               char     *estr;          /* pointer to error string */
               fint     fatal = 4;      /* fatal error */

               estr = strerror( errno );
               error_c( &fatal, tofchar( estr ) );
            } else if (nd != nw) {      /* should not happen */
               fint     fatal = 4;      /* fatal error */

               error_c( &fatal, tofchar( "DATA WRITE ERROR!!" ) );
            }
            eptr += nfloats;            /* new end position */
         }
      }
      lseek( f, fptr * sizeof( float ), SEEK_SET );
#ifdef  FPC                             /* floating point conversion */
      if (OS_FLOATING_TYPE != ctid->type) {
         fint8   pleft = ndone;
         fint   pstep = NFLOATS; /* TODO: should this be fint8? */
         fint8   pwrit = nwrit;

         while (pleft) {
            if (pstep > pleft) pstep = pleft;
            spfplf_c( &ctid->type, &data[pwrit], floats, &pstep );
            nw = pstep * sizeof( float );       /* number of bytes */
#if     0
            nd = write( f, floats, nw );/* do the write */
#else
            while ((nd = write( f, floats, nw )) == -1 && errno == EINTR);
#endif
            if (nd == -1) {             /* write error */
               char     *estr;          /* pointer to error string */
               fint     fatal = 4;      /* fatal error */

               estr = strerror( errno );
               error_c( &fatal, tofchar( estr ) );
            } else if (nd != nw) {      /* should not happen */
               fint     fatal = 4;      /* fatal error */

               error_c( &fatal, tofchar( "DATA WRITE ERROR!!" ) );
            }
            pwrit += pstep;
            pleft -= pstep;
         }
      } else {
         nw = ndone * sizeof( float );  /* number of bytes */
#if     0
         nd = write( f, &data[nwrit], nw );     /* do the write */
#else
         while ((nd = write( f, &data[nwrit], nw )) == -1 && errno == EINTR);
#endif
         if (nd == -1) {                /* write error */
            char        *estr;          /* pointer to error string */
            fint        fatal = 4;      /* fatal error */

            estr = strerror( errno );
            error_c( &fatal, tofchar( estr ) );
         } else if (nd != nw) {         /* should not happen */
            fint        fatal = 4;      /* fatal error */
            error_c( &fatal, tofchar( "DATA WRITE ERROR!!" ) );
         }
      }
#else                                   /* CURRENTLY IMPLEMENTED */
      nw = ndone * sizeof( float );     /* number of bytes */
#if     0
      nd = write( f, &data[nwrit], nw );/* do the write */
#else
      while ((nd = write( f, &data[nwrit], nw )) == -1 && errno == EINTR);
#endif
      if (nd == -1) {                   /* write error */
         char   *estr;                  /* pointer to error string */
         fint   fatal = 4;              /* fatal error */

         estr = strerror( errno );
         error_c( &fatal, tofchar( estr ) );
      } else if (nd != nw) {            /* should not happen */
         fint   fatal = 4;              /* fatal error */
         error_c( &fatal, tofchar( "DATA WRITE ERROR!!" ) );
      }
#endif
      nwrit += ndone;                   /* update */
   } while (nleft);                     /* until nothing left to do */
#if     0
   /*
    * because of a sun bug, we have to check here whether the same file is
    * open for read also. If this is true, we have to do a fsync to avoid
    * erroneous reads. This will slow down the process by about a factor 3.
    */
   {
      int       itid;

      for (itid = 0; itid < ntid; itid++) {
         if (tids[itid].qtid == R_TID && tids[itid].ufid == f) break;
      }
      if (itid < ntid) fsync( f );
   }
#endif
   if (!ctid->todo) {                   /* all done */
      ltid = 0;                         /* free transfer id */
      ctid->qtid = E_TID;               /* make it empty */
   }
   if (tobool(presentn_c( done ))) {    /* argument present */
      (*done) = nwrit;                  /* return to caller */
   } else {                             /* argument not present */
      if (nwrit != ntotl) ltid = -30;   /* error code */
   }
   if (tobool(presentn_c( tid ))) {     /* argument present */
      (*tid) = ltid;                    /* return transfer id */
   } else if (ltid) {                   /* not present and no zero */
      ltid = -30;                       /* error code */
      gds___error_c( &ltid );           /* GDS error routine */
   }
}


/*

#>            gdsi_cancel.dc2

Function:     GDSI_CANCEL

Purpose:      Cancels a transfer started with GDSI_READ or GDSI_WRITE.

Category:     DATA, IMAGE-IO

File:         gdsi_read.c

Author:       K.G. Begeman

Use:          CALL GDSI_CANCEL( SET,           Input       CHARACTER*(*)
                                TRANS_ID )     Input       INTEGER

              SET           set name
              TRANS_ID      transfer indentification.
                            This identifier was returned by a previous call
                            GDSI_READ or GDSI_WRITE ( always > 0 ).

Related Docs: gdsi_read.dc2, gdsi_write.dc2.

Updates:      Dec 21, 1989: WZ, migrated to C.
              Jul  5, 1990: KGB, modified document.
              Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file.
              Aug 17, 1991: KGB, new algorithm, replace fopen by open.
              Jan 31, 1994: JPT, attempt to open read-only if read-write fails.
              Mar  1, 2011: JPT, use off_t for file offsets.

#<

Fortran to C interface:

@ subroutine gdsi_cancel( character, integer )

*/

void    gdsi_cancel_c( fchar set ,              /* the set name */
                       fint  *tid )             /* the transfer id */
{
   /*int          f;*/                      /* file descriptor */
   tid_struct   *ctid;                  /* transfer struct */

   if ((*tid) < 0) {                    /* error from previous call */
      gds___error_c( tid );             /* generate standard errors */
   } else if ((*tid) > ntid) {          /* illegal tid */
      (*tid) = -31;                     /* error */
      return;                           /* return to caller */
   }
   if ((*tid) == 0) return;             /* already done */
   ctid = &tids[(*tid)-1];              /* get transfer struct */
   /*f = ctid->ufid;*/                      /* get file descriptor */
   if (ctid->todo) {                    /* not yet finished */
      (*tid) = 0;                       /* reset */
      ctid->todo = 0;                   /* now its finished */
      ctid->qtid = E_TID;               /* make it empty */
   }
}
