/* wminmax.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

*/

#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "gipsyc.h"
#include "anyout.h"
#include "fblank.h"
#include "setfblank.h"
#include "gds_lock.h"
#include "gds_unlock.h"
#include "gdsc_grid.h"
#include "gdsc_intersect.h"
#include "gdsc_ndims.h"
#include "gdsc_range.h"
#include "gdsc_size.h"
#include "gdsd_delete.h"
#include "gdsd_find.h"
#include "gdsd_wint.h"
#include "gdsd_rint.h"
#include "gdsd_wreal.h"
#include "gdsd_rreal.h"
#include "gdsi_read.h"
#include "minmax3.h"

#define MAXDSCNAMLEN      20            /* maximum length of descriptor names */
#define MAXREADBUFLEN   8192           /* maximum size of dynamic read buffer */

typedef struct { char *dsc; fint len; } dsc_struct;
typedef struct { char dsc[MAXDSCNAMLEN]; fint level; } des_struct;

static dsc_struct minmax_key[] = {
   { "DATAMIN ", 8 },
   { "DATAMAX ", 8 },
   { "NBLANK  ", 8 }
};

#define MAXMINMAXKEY (  sizeof( minmax_key ) / sizeof( dsc_struct ) )

#if defined(TESTBED)
static void test( fchar set, des_struct *des, fint num )
{
   char *buf;
   fint  device = 3;
   fint  k;
   fint  n;
   fint  naxis;
   fint  nel = 0;
   fint  zero = 0;

   naxis = gdsc_ndims_c( set, &zero );
   buf = calloc( MAXDSCNAMLEN + 1 + 20 * naxis, sizeof( char ) );
   anyout_c( &device, tofchar( "Deleted the following descriptors:" ) );
   anyout_c( &device, tofchar( "Descriptor          Level" ) );
   for (k = 0; k < num; k++) {
      strncpy( buf, des[k].dsc, MAXDSCNAMLEN );
      buf[MAXDSCNAMLEN] = 0;
      for (n = 0; n < naxis; n++) {
         char s[20];
         fint axnum = n + 1;
         fint cerror = 0;
         fint grid;

         grid = gdsc_grid_c( set, &axnum, &des[k].level, &cerror );
         if (cerror == -19) {
            sprintf( s, "  ..." );
         } else {
            sprintf( s, "%5d", grid );
         }
         strcat( buf, s );
      }
      anyout_c( &device, tofchar( buf ) );
   }
   free( buf );
}
#endif

static fint minmaxkey( fchar dsc )
{
   fint d;
   fint n = 0;

   do {
      d = strncmp( dsc.a, minmax_key[n].dsc, minmax_key[n].len );
   } while (d && ++n < MAXMINMAXKEY);
   if (d) return( 0 ); else return( 1 );
}

static fint intersect( fchar set, fint *subsets, fint nsubs, fint level )
{
   fint n;
   fint r = 0;

   for (n = 0; !r && n < nsubs; n++) {
      fint cerror = 0;

      if (gdsc_intersect_c( set, &subsets[n], &level, &cerror ) != -1) r = 1;
   }
   return( r );
}

static void destruct( fchar set, fint *subsets, fint nsubs )
{
   char        buf[MAXDSCNAMLEN];
   des_struct *des_buf = NULL;
   fchar       dsc;
   fint        derror = 0;
   fint        des_num = 0;
   fint        level = 0;
   fint        n;
   fint        recno = 0;
   fint        err_i = 0;

   dsc.a = buf; dsc.l = MAXDSCNAMLEN;
   gds_lock_c(set, &err_i);
   level = 0;
   gdsd_find_c( dsc, set, NULL, &recno, &level );
   while (recno && level >= 0) {
      if (minmaxkey( dsc )) {
         if (intersect( set, subsets, nsubs, level )) {
            des_buf = realloc( des_buf, sizeof(des_struct) * ( des_num + 1 ) );
            strncpy( des_buf[des_num].dsc, dsc.a, MAXDSCNAMLEN );
            des_buf[des_num++].level = level;
         }
      }
      gdsd_find_c( dsc, set, NULL, &recno, &level );
   }
   err_i = 0;
   gds_unlock_c(set, &err_i);
#if defined(TESTBED)
   test( set, des_buf, des_num );
#endif
   for (n = 0; n < des_num; n++) {
      dsc.a = des_buf[n].dsc; dsc.l = MAXDSCNAMLEN;
      level = des_buf[n].level;
      gdsd_delete_c( set, dsc, &level, &derror );
   }
   free( des_buf );
}

/*
#>            wminmax.dc2

Subroutine:   WMINMAX

Purpose:      Writes (new) minimum and maximum and number of blanks of
              subsets in the descriptor file and optionally deletes
              the MINMAX descriptors at intersecting levels.

Category:     GDS, HEADER

File:         wminmax.c

Author:       K.G. Begeman

Use:          CALL WMINMAX( SET     ,  Input    CHARACTER*(*)
                            SUBSETS ,  Input    INTEGER ARRAY
                            DATAMIN ,  Input    REAL ARRAY
                            DATAMAX ,  Input    REAL ARRAY
                            NBLANK  ,  Input    INTEGER ARRAY
                            NSUBS   ,  Input    INTEGER
                            CHANGE  )  Input    INTEGER

              SET           Name of GDS set.
              SUBSETS       Array containing subset coordinate words
                            of the subsets for which the minimum and
                            maximum were determined.
              DATAMIN       Array containing the minimum value in the
                            subsets.
              DATAMAX       Array containing the maximum value in the
                            subsets.
              NBLANK        Array containing the number of blanks in
                            the subsets.
              NSUBS         Number of subsets.
              CHANGE        0 means that minimum and maximum have not
                            changed, anything else means that minimum
                            and maximum have changed and that the 
                            MINMAX descriptors at intersecting levels
                            will be removed by WMINMAX.

Notes:        1. The so called MINMAX descriptors are: DATAMIN, DATAMAX
              and NBLANK.
              2. When DATAMIN and/or DATAMAX are undefined (BLANK), then
              the DATAMIN and DATAMAX descriptors will be removed and
              only the number of blanks (NBLANK) will be written.

Updates:      Feb 20, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine wminmax( character, integer, real, real, integer, integer,
@                     integer )

*/

void wminmax_c( fchar  set    ,
                fint  *subsets,
                float *datamin,
                float *datamax,
                fint  *nblank ,
                fint  *nsubs  ,
                fint  *change )
{
   fint ns;                                            /* subset loop counter */
   fint subdim;                                       /* dimension of subsets */

   if (*change) {                /* remove descriptors at intersecting levels */
      destruct( set, subsets, *nsubs );
   }
   subdim = gdsc_ndims_c( set, subsets );
   if (subdim) {
      for (ns = 0; ns < *nsubs; ns++) {
         fint gerror = 0;                            /* GDS error return code */

         if (fblank_c( &datamin[ns] )) {                /* minimum is BLANK ? */
            gdsd_delete_c( set, tofchar( "DATAMIN" ), &subsets[ns], &gerror );
            gerror = 0;
         } else {         /* write DATAMIN to descriptor file at subset level */
            gdsd_wreal_c( set, tofchar( "DATAMIN" ), &subsets[ns], &datamin[ns],
                          &gerror );
         }
         if (fblank_c( &datamax[ns] )) {                /* maximum is BLANK ? */
            gdsd_delete_c( set, tofchar( "DATAMAX" ), &subsets[ns], &gerror );
            gerror = 0;
         } else {         /* write DATAMAX to descriptor file at subset level */
            gdsd_wreal_c( set, tofchar( "DATAMAX" ), &subsets[ns], &datamax[ns],
                          &gerror );
         }
         gdsd_wint_c( set, tofchar( "NBLANK" ), &subsets[ns], &nblank[ns],
                      &gerror );
      }
   }
}   

/*
#>            uminmax.dc2

Subroutine:   UMINMAX

Purpose:      Determines and updates the minimum and maximum and the
              number of blanks in subsets.

Category:     GDS, HEADER

File:         wminmax.c

Author:       K.G. Begeman

Use:          CALL UMINMAX( SET     ,  Input    CHARACTER*(*)
                            SUBSETS ,  Input    INTEGER ARRAY
                            NSUBS   ,  Input    INTEGER
                            CHANGE  )  Input    INTEGER

              SET           Name of GDS set.
              SUBSETS       Array containing subset coordinate words
                            of the subsets for which the minimum and
                            maximum are to be determined.
              NSUBS         Number of subsets.
              CHANGE        0 means that minimum and maximum have not
                            changed, anything else means that minimum
                            and maximum have changed and that the 
                            MINMAX descriptors at intersecting levels
                            will be removed by UMINMAX.

Notes:        1. The so called MINMAX descriptors are: DATAMIN, DATAMAX
              and NBLANK.
              2. When no defined DATAMIN and DATAMAX, then BLANK will
              be returned and the DATAMIN and DATAMAX descriptors will
              be removed and only the number of blanks (NBLANK) will be
              written.

Updates:      Feb 21, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine uminmax( character, integer, integer, integer )

*/

void uminmax_c( fchar  set    ,
                fint  *subsets,
                fint  *nsubs  ,
                fint  *change )
{
   fint   buf_len;
   fint   n_pixels = 1;
   fint   n;
   fint   naxis;
   fint  *nblank;
   fint   ns;
   fint   zero = 0;
   float *buffer;
   float *datamin;
   float *datamax;

   naxis = gdsc_ndims_c( set, &zero );
   for (n = 0; n < naxis; n++) {
      fint axnum = n + 1;
      fint cerror = 0;
      fint grid;
      
      grid = gdsc_grid_c( set, &axnum, subsets, &cerror );
      if (cerror == -19) {
         cerror = 0;
         n_pixels *= gdsc_size_c( set, &axnum, &cerror );
      }
   }
   if (n_pixels > MAXREADBUFLEN) {
      buf_len = MAXREADBUFLEN;
   } else {
      buf_len = n_pixels;
   }
   buffer = calloc( buf_len, sizeof( float ) );
   nblank = calloc( *nsubs, sizeof( fint ) );
   datamin = calloc( *nsubs, sizeof( float ) );
   datamax = calloc( *nsubs, sizeof( float ) );
   for (ns = 0; ns < *nsubs; ns++) {
      fint cerror = 0;
      fint count = 0;
      fint cwhi;
      fint cwlo;
      fint t_id = 0;
      
      gdsc_range_c( set, &subsets[ns], &cwlo, &cwhi, &cerror );
      do {
         fint pixels_done;

         gdsi_read_c( set, &cwlo, &cwhi, buffer, &buf_len,
                      &pixels_done, &t_id );
         minmax3_c( buffer, &pixels_done, &datamin[ns], &datamax[ns],
                    &nblank[ns], &count );
      } while (t_id > 0);
   }
   wminmax_c( set, subsets, datamin, datamax, nblank, nsubs, change );
   free( nblank ); free( datamin ); free( datamax );
}

/*
#>            rminmax.dc2

Subroutine:   RMINMAX

Purpose:      Finds the minimum and maximum and the number of blanks
              in subsets by reading the corresponding descriptors or,
              when they are not present, by determining them.

Category:     GDS, HEADER

File:         wminmax.c

Author:       K.G. Begeman

Use:          CALL RMINMAX( SET     ,  Input    CHARACTER*(*)
                            SUBSETS ,  Input    INTEGER ARRAY
                            DATAMIN ,  Output   REAL ARRAY
                            DATAMAX ,  Output   REAL ARRAY
                            NBLANK  ,  Output   INTEGER ARRAY
                            NSUBS   ,  Input    INTEGER
                            CHANGE  )  Input    INTEGER

              SET           Name of GDS set.
              SUBSETS       Array containing subset coordinate words
                            of the subsets for which the minimum and
                            maximum are to be determined.
              DATAMIN       Array containing the minimum value in the
                            subsets.
              DATAMAX       Array containing the maximum value in the
                            subsets.
              NBLANK        Array containing the number of blanks in
                            the subsets.
              NSUBS         Number of subsets.
              CHANGE        0 means that minimum and maximum have not
                            changed, anything else means that minimum
                            and maximum have changed and that the 
                            MINMAX descriptors at intersecting levels
                            will be removed by RMINMAX.

Notes:        1. The so called MINMAX descriptors are: DATAMIN, DATAMAX
              and NBLANK.
              2. When no defined DATAMIN and DATAMAX, then BLANK will
              be returned and the DATAMIN and DATAMAX descriptors will
              be removed and only the number of blanks (NBLANK) will be
              written.

Updates:      Feb 20, 1990: KGB, Document created.

#<

Fortran to C interface:

@ subroutine rminmax( character, integer, real, real, integer, integer,
@                     integer )

*/

void rminmax_c( fchar  set    ,
                fint  *subsets,
                float *datamin,
                float *datamax,
                fint  *nblank ,
                fint  *nsubs  ,
                fint  *change )
{
   fint  nd = 0;                /* number of subsets we still have to work on */
   fint  ns;                                           /* subset loop counter */
   fint *ptrs = NULL;                  /* pointer to subsets in argument list */
   fint  subdim;                                      /* dimension of subsets */
   fint *subs = NULL;   /* list of subsets for which we still have to work on */

   if (*change) {                /* remove descriptors at intersecting levels */
      uminmax_c( set, subsets, nsubs, change );
   }
   subdim = gdsc_ndims_c( set, subsets );
   if (subdim) {
      for (ns = 0; ns < *nsubs; ns++) {
         fint gerror = 0;                            /* GDS error return code */
      
         gdsd_rint_c( set, tofchar( "NBLANK" ), &subsets[ns], &nblank[ns],
                      &gerror );
         if (gerror != subsets[ns]) {    /* we have to determine it ourselves */
            gerror = 0;
            subs = realloc( subs, ( nd + 1 ) * sizeof( fint ) );
            ptrs = realloc( ptrs, ( nd + 1 ) * sizeof( fint ) );
            subs[nd] = subsets[ns];
            ptrs[nd] = ns;
            nd += 1;
         } else {
            gdsd_rreal_c( set, tofchar( "DATAMIN" ), &subsets[ns], &datamin[ns],
                          &gerror );
            if (gerror != subsets[ns]) {
               gerror = 0;
               setfblank_c( &datamin[ns] );
            }
            gdsd_rreal_c( set, tofchar( "DATAMAX" ), &subsets[ns], &datamax[ns],
                          &gerror );
            if (gerror != subsets[ns]) {
               gerror = 0;
               setfblank_c( &datamax[ns] );
            }
         }
      }
      if (nd) {                         /* list of subsets we have to work on */
         uminmax_c( set, subs, &nd, change );
         for (ns = 0; ns < nd; ns++) {
            fint nq = ptrs[ns];
            fint gerror = 0;                         /* GDS error return code */
      
            gdsd_rint_c( set, tofchar( "NBLANK" ), &subsets[nq], &nblank[nq],
                         &gerror );
            gdsd_rreal_c( set, tofchar( "DATAMIN" ), &subsets[nq], &datamin[nq],
                         &gerror );
            if (gerror != subsets[nq]) {
               gerror = 0;
               setfblank_c( &datamin[nq] );
            }
            gdsd_rreal_c( set, tofchar( "DATAMAX" ), &subsets[nq], &datamax[nq],
                          &gerror );
            if (gerror != subsets[nq]) {
               gerror = 0;
               setfblank_c( &datamax[nq] );
            }
         }
         free( subs ); free( ptrs );
      }
   } else {
      for (ns = 0; ns < *nsubs; ns++) {
         fint done;
         fint one = 1;
         fint tid = 0;
         
         gdsi_read_c( set, &subsets[ns], &subsets[ns], &datamin[ns], &one,
                      &done, &tid );
         datamax[ns] = datamin[ns];
         if (fblank_c( &datamax[ns] )) nblank[ns] = 1; else nblank[ns] = 0;
      }
   }
}
