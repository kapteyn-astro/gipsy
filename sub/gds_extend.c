/*============================================================================
                                 gds_extend.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gds_extend.dc2
Subroutine:    GDS_EXTEND

Purpose:       create or extend an axis

Category:      GDS

File:          gds_extend.c

Author:        W. Zwitser

Use:           GDS_EXTEND( SET,                Input           character
                           AXNAME,             Input           character
                           ORIGIN,             Input           double
                           SIZE,               Input           integer
                           ERROR )             In/Out          integer

               SET       name of set                       
          
               AXNAME    name of axis
                         If omitted: last axis.

               ORIGIN    new origin                        
                         If omitted: extend last axis.
                    
               SIZE      new size                          
                         If omitted: 1 is assumed for new axis or
                                     origin shift of an existing one.
                                
               ERROR     0  = successful                     
                        -28 = axis already in use

Description:

               GDS_EXTEND can be used for several purposes:

               1. create a new axis.
                  Each call increments descriptor NAXIS with 1. The new value
                  'n' of NAXIS is used to create 3 new descriptors:

                  - CTYPEn  = the name of the new axis                     
                  - CRPIXn  = the origin/reference point of the new axis   
                  - NAXISn  = the size of the new axis                     
                  
               2. extend the size of the last axis.
                  Now AXNAME and ORIGIN are optional and SIZE will be the
                  new size of the last axis.

               3. shift the origin (reference point) of an axis.
                  AXNAME and ORIGIN must be given and SIZE is not allowed.

Updates:       Dec 21, 1990: WZ, migrated to C
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file
               Mar 23, 1994: JPT, modified to cooperate with GDS server.
               Jul 09, 2008: JPT, fatal error for sets which are too large.
               Mar 02, 2011: JPT, increased set size limit to maximum possible.
#<

@ subroutine gds_extend( character, 
@                        character, 
@                        double precision, 
@                        integer,
@                        integer )

----------------------------------------------------------------------------*/

#include    "gdsparams.h"
#include    "gdserrors.h"
#include    "gdsd_basic.h"
#include    "presentc.h"
#include    "presentn.h"
#include    "gdsd_rint.h"
#include    "gdsd_rchar.h"
#include    "gdsd_wint.h"
#include    "gdsd_wchar.h"
#include    "gdsd_wdble.h"
#include    "dpfplf.h"
#include    "dpfpfl.h"
#include    "userfio.h"
#include    "stdio.h"
#include    "string.h"
#include    <stdint.h>
#include "userfio.h"
#define     NAX_LEN  80
#define     MAXFACT  2147483647

void  gds_extend_c( fchar     set,                          /* name of set  */
                    fchar     axname,                       /* name of axis */
                    double   *origin,                       /* axis origin  */
                    fint     *size,                         /* axis size    */
                    fint     *err )                         /* error code   */
{
   fchar     key, iaxname;
   fint      axnum = 1, iax = 0, level = 0, naxis, size_i, size_old;
   fint      err_i;
   char      axname_s[NAX_LEN], iaxname_s[NAX_LEN], key_s[7];
   int       found, last_ax, ok, orig_shift = 0;
   int       chhed=0;
   gds_coord *set_info;

   gds_lock_c(set, err);
   if (*err < 0) return;
   (void)gds_frhed(set);
   (void)gds_rhed(set, &set_info);
   naxis = set_info->naxis;
   if ( presentn_c( size ) )
      size_i = *size;
   else
      size_i = 1;                                        /* default size: 1 */
   last_ax = !presentc_c( axname );              /* default axis: last axis */
   found   = last_ax;                     
   if ( !found ) {           /* try to find specified axis ( found = true ) */
      (void)gds___char2str( axname, axname_s, sizeof(axname_s) );
      while ( !found && ( iax < naxis ) ) {
         *err = 0;
         axnum = iax + 1;
         sprintf( key_s, "CTYPE%d", axnum );
         key = tofchar( key_s );
         iaxname.a = iaxname_s;
         iaxname.l = NAX_LEN;
         gdsd_rchar_c( set, key, &level, iaxname, err );  /* read axis name */
         (void)gds___char2str( iaxname, iaxname_s ,sizeof(iaxname_s));
         found = !strcmp( iaxname_s, axname_s );            /* axis found ? */
         if ( !found ) iax++;
      }
   }


   *err = 0;
   if ( found ) {                                            /* axis found: */
      orig_shift = presentn_c( (fint*)origin ) && !presentn_c( size ); 
      ok = orig_shift || last_ax || axnum == naxis;       /* - shift origin */
                                           /* - or change size of last axis */
      if (gds___fail(ok, GDS_AXPRESENT, err)) {
         err_i = 0;
         gds_unlock_c(set,&err_i);
         return;
      }
      
   } else {                                              /* axis not found: */
      naxis++;                                         /* - increment #axes */
      key = tofchar( "NAXIS" );
      gdsd_wint_c( set, key, &level, &naxis, err );
      sprintf( key_s, "CTYPE%d", naxis );
      key = tofchar( key_s );
      gdsd_wchar_c( set, key, &level, axname, err );  /* - name of new axis */
      sprintf( key_s, "CRPIX%d", naxis );
      key = tofchar( key_s );
      gdsd_wdble_c( set, key, &level, origin, err );        /* - its origin */
      set_info->naxis = naxis;
      set_info->origin[naxis-1] = *origin;
      chhed = 1;
   }


   if ( orig_shift ) {                  /* shift origin of an existing axis */
      sprintf( key_s, "CRPIX%d", axnum );
      key = tofchar( key_s );
      gdsd_wdble_c( set, key, &level, origin, err );
      set_info->origin[axnum-1] = *origin;
      chhed = 1;
      
   } else {                              /* change size of an existing axis */
      sprintf( key_s, "NAXIS%d", naxis );
      key = tofchar( key_s );
      gdsd_rint_c( set, key, &level, &size_old, err );
      if ( size_old < size_i || *err ) {
         int64_t factor;

         *err = 0;
         gdsd_wint_c( set, key, &level, &size_i, err );
         iax = naxis - 1;
         set_info->size[iax] = size_i;
         factor = (int64_t)set_info->factor[iax] * 
                  (set_info->size[iax] + 1 );
		anyoutf(1, "factor %ld", factor);
         if (factor>MAXFACT) {
            naxis--;
            key = tofchar( "NAXIS" );
            gdsd_wint_c( set, key, &level, &naxis, err );   /* roll back */
         }
         if (gds___fail(factor<MAXFACT, GDS_AXTOOLONG, err)) {
            err_i = 0;
            gds_unlock_c(set,&err_i);
            return;
         }
         set_info->factor[naxis] = factor;
         chhed = 1;
      }
   }
   if (chhed) (void)gds_whed(set,set_info);
   err_i = 0;
   gds_unlock_c(set,&err_i);
}
