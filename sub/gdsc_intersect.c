/*============================================================================
                               gdsc_intersect.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsc_intersect.dc2

Function:      GDSC_INTERSECT

Purpose:       return the intersection coordinate word of two subsets

Category:      GDS

File:          gdsc_intersect.c

Author:        W. Zwitser

Use:           INTEGER  GDSC_INTERSECT( SET,           Input       character
                                        SUBSET_1       Input       integer
                                        SUBSET_2       Input       integer
                                        ERROR )        In/Out      integer

               GDSC_INTERSECT  Returns coordinate word of intersecting
                               sub structure or -1 if sub structures do
                               not intersect.

               SET             set name
               
               SUBSET_1        coordinate word of subset 1 

               SUBSET_2        coordinate word of subset 2 

               ERROR           0 = successful
                              <0 = a GDS error

Updates:       Mar  1, 1990: WZ, migrated to C.
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file.
               Aug  3, 1991: KGB, Returns -1 for no intersection.
               Mar 24, 1994: JPT, modified to cooperate with GDS server.
#<

@ integer function gdsc_intersect( character, 
@                                  integer,
@                                  integer,
@                                  integer )

----------------------------------------------------------------------------*/

#include    "gdsparams.h"
#include    "gdserrors.h"
#include    "gdsd_basic.h"

fint gdsc_intersect_c( fchar     set,             /* set name               */
                       fint     *subset_1,        /* coord.word of subset 1 */
                       fint     *subset_2,        /* coord.word of subset 2 */
                       fint     *err )            /* error code             */
{
   fint        err_up, grid_1, grid_2, iax, new_coord, naxis;
   gds_coord *setinfo;
   
   new_coord = *subset_2;
   (void)gds_rhed(set, &setinfo);
   naxis = setinfo->naxis;
   for ( iax = 0; new_coord >= 0 && iax < naxis; iax++ ) {
      err_up = 0;
      grid_1 = gds___unpack_c( set, subset_1, &iax, &err_up );
      if ( err_up != GDS_COORDUNDEF ) {
         err_up = 0;
         grid_2 = gds___unpack_c( set, subset_2, &iax, &err_up );
         if ( err_up == GDS_COORDUNDEF || grid_1 == grid_2 ) {
            gds___pack_c( set, &new_coord, &grid_1, &iax, err );
         } else {
            new_coord = -1;			/* KGB: no intersection */
         }
      }
   }
   return( new_coord );
}
