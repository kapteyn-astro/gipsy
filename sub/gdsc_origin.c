/*============================================================================
                                 gdsc_origin.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsc_origin.dc2
Function:      GDSC_ORIGIN

Purpose:       return the origin of an axis

Category:      GDS

File:          gdsc_origin.c

Author:        W. Zwitser

Use:           DOUBLE  GDSC_ORIGIN( SET,           Input       character
                                    AXNUM,         Input       integer
                                    ERROR )        In/Out      integer

               GDSC_ORIGIN   origin of the axis

               SET           set name      

               AXNUM         axis number ( 1...naxis )

               ERROR         0  = successful
                            -17 = axis not present

Updates:       Dec  5, 1989: WZ, migrated to C
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file
               Mar 24, 1994: JPT, modified to cooperate with GDS server.
#<

@ double precision function gdsc_origin( character, 
@                                        integer,
@                                        integer )

----------------------------------------------------------------------------*/

#include    "gdsparams.h"
#include    "gdserrors.h"
#include    "gdsd_basic.h"
#include    "dpfpfl.h"

double  gdsc_origin_c( fchar     set,                        /* name of set */
                       fint     *axnum,                      /* axis number */
                       fint     *err )                       /* error code  */
{
   fint        iax, naxis, ftype, one = 1;
   double      r;
   gds_coord   *setinfo;
      
   (void)gds_rhed(set, &setinfo);
   naxis =  setinfo->naxis;
   iax = *axnum - 1;
   if (gds___fail( iax < naxis, GDS_ORAXNOTFOUND, err )) return 0.0;
   return( setinfo->origin[iax] );
}
