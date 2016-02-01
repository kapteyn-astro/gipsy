/*============================================================================
                                 gdsc_ndims.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsc_ndims.dc2
Function:      GDSC_NDIMS

Purpose:       return the dimensionality of a coordinate word

Category:      GDS

File:          gdsc_ndims.c

Author:        W. Zwitser

Use:           INTEGER  GDSC_NDIMS( SET,           Input       character
                                    CWORD )        Input       integer

               GDSC_NDIMS    dimensionality of CWORD, it means:
                             each undefined axis in CWORD is counted.

               SET           set name      

               CWORD         coordinate word

Updates:       Dec  6, 1989: WZ, migrated to C
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file
               Mar 24, 1994: JPT, modified to cooperate with GDS server.
#<

@ integer function gdsc_ndims( character, 
@                              integer*8 )

----------------------------------------------------------------------------*/

#include    "gdsparams.h"
#include    "gdserrors.h"
#include    "gdsd_basic.h"
#include    "gds___unpack.h"

fint   gdsc_ndims_c( fchar    set,                       /* name of set     */
                     fint8    *coord_word )               /* coordinate word */
{
   fint8		coord;
	fint        err = 0, iax, ndim = 0, naxis;
   gds_coord   *setinfo;
   
   (void)gds_rhed(set, &setinfo);
   naxis =  setinfo->naxis;
   for ( iax = 0; iax < naxis; iax++ ) {
      err = 0;
      coord = gds___unpack_c( set, coord_word, &iax, &err );
      if ( err == GDS_COORDUNDEF ) ndim += 1;    /* count each undefined axis */
   }
   return( ndim );
}
