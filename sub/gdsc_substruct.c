/*============================================================================
                               gdsc_substruct.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsc_substruct.dc2
Function:      GDSC_SUBSTRUCT

Purpose:       return subset delimited by two coordinate words

Category:      GDS

File:          gdsc_substruct.c

Author:        W. Zwitser

Use:           INTEGER  GDSC_SUBSTRUCT( SET,           Input       character
                                        CLOW,          Input       integer(8)
                                        CUPP,          Input       integer(8)
                                        ERROR )        In/Out      integer

               GDSC_SUBSTRUCT    level of CLOW and CUPP
               
               SET               set name 
               
               CLOW              lower left coordinate word

               CUPP              upper right coordinate word

               ERROR             0  = successful
                                <0  = a GDS error

Updates:       Dec  5, 1989: WZ, migrated to C
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file
               Mar 24, 1994: JPT, modified to cooperate with GDS server.
#<

@ integer function gdsc_substruct( character, 
@                                  integer*8,
@                                  integer*8,
@                                  integer )

----------------------------------------------------------------------------*/

#include    "gdsparams.h"
#include    "gdserrors.h"
#include    "gdsd_basic.h"

fint  gdsc_substruct_c( fchar    set,        /* name of set                 */
                        fint8    *coord_1,    /* lower left coordinate word  */
                        fint8    *coord_2,    /* upper right coordinate word */
                        fint    *err )       /* error code                  */
{
   fint        iax, err_up, grid_1, grid_2, new_coord = 0, naxis;
   gds_coord   *setinfo;
   
   (void)gds_rhed(set, &setinfo);
   naxis = setinfo->naxis;
   if ( *coord_1 || *coord_2 ) {
      for ( iax = 0; iax < naxis; iax++ ) {
         err_up = 0;
         grid_1 = gds___unpack_c( set, coord_1, &iax, &err_up );
         if ( err_up != GDS_COORDUNDEF ) {   /* this axis defined in coord_1 */
            err_up = 0;
            grid_2 = gds___unpack_c( set, coord_2, &iax, &err_up );
            if ( err_up != GDS_COORDUNDEF && grid_1 == grid_2 ) {   
                                                         /* this in coord_2 */
               gds___pack_c( set, &new_coord, &grid_1, &iax, err );
            }             /* pack if grids in coord_1 and coord_2 are equal */
         }
      }
   }
   return( new_coord );
}
