/*============================================================================
                                  gdsc_axnum.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsc_axnum.dc2
Function:      GDSC_AXNUM

Purpose:       return axis number of a specified axis

Category:      GDS

File:          gdsc_axnum.c

Author:        J.P. Terlouw

Use:           INTEGER  GDSC_AXNUM( SET,           Input       character
                                    AXNAME,        Input       character
                                    ERROR )        In/Out      integer

               GDSC_AXNUM    number of axis

               SET           set name      

               AXNAME        name of axis (either full or partial)

               ERROR         0  = successful
                            -11 = axis name not unique
                            -14 = axis not present
                            other codes: failed to obtain set info.

Updates:       Dec  5, 1989: WZ, migrated to C
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file
               Mar 24, 1994: JPT, modified to cooperate with GDS server.
#<

@ integer function gdsc_axnum( character, 
@                              character, 
@                              integer )

----------------------------------------------------------------------------*/

#include    "gdsparams.h"
#include    "gdserrors.h"
#include    "gdsd_basic.h"
#include    "gdsc_name.h"
#define     NAX_LEN     80       /* max. length of an axis name (FITS def.) */

fint   gdsc_axnum_c( fchar    set,                          /* set name     */
                     fchar    axname,                       /* name of axis */
                     fint    *err )                         /* error code   */
{
   fchar       iaxname;
   char        axname_s[NAX_LEN], iaxname_s[NAX_LEN];
   char        first_full_match = 0, first_part_match = 0, ok;
   fint        axnum = 0, iax, len_ax, naxis, err_i;
   fint        axfound;
   gds_coord   *setinfo;
      
   err_i = gds_rhed(set, &setinfo);
   if (gds___fail(!err_i, err_i, err)) return 0;
   (void)gds___char2str(axname, axname_s, sizeof(axname));  /* fchar -> char */
   len_ax = strlen( axname_s );
   naxis = setinfo->naxis;
   for ( iax = 0; iax < naxis; iax++ ) {             /* compare names*/
      axnum = iax + 1;
      iaxname.a = iaxname_s;                 /* fill fchar descriptor block */
      iaxname.l = NAX_LEN;   /* (tofchar uses strlen; iaxname_s not filled) */
      gdsc_name_c( iaxname, set, &axnum, err );
      (void)gds___char2str(iaxname, iaxname_s, sizeof(axname) );  
      if ( !strcmp( axname_s, iaxname_s ) ) { /* full match of axis names ? */
         if (gds___fail( !first_full_match, GDS_NOTUNIQ, err )) return 0;
         first_full_match = 1;
         axfound = axnum;
      } else if ( !strncmp( axname_s, iaxname_s, len_ax ) ) {
         if (gds___fail( !( first_full_match || first_part_match ), 
                        GDS_NOTUNIQ, err )) return 0;
         first_part_match = 1;                             /* partial match */
         axfound = axnum;
      }
   }
   ok = first_full_match || first_part_match;           /* axis name matched */
   if (gds___fail( ok, GDS_AXNOTFOUND, err )) return 0; /* axis not present ? */
   return axfound;
}
