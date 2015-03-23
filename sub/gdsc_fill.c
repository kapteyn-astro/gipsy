/*============================================================================
                                 gdsc_fill.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsc_fill.dc2
Function:      GDSC_FILL

Purpose:       return coordinate word filled with a grid value for each axis.

Category:      GDS

File:          gdsc_fill.c

Author:        W. Zwitser

Use:           INTEGER  GDSC_FILL( SET,            Input       character
                                   SUBSET,         Input       integer
                                   GRIDS )         Input       integer

               GDSC_FILL     coordinate word filled up; -1 in case of error.
 
               SET           set name      

               SUBSET        coordinate word to be filled.
                             For each undefined axis a grid value is added
                             from array GRIDS.

               GRIDS         array with grid values.
                             This array should contain as many elements as 
                             the number of undefined values in level.

Description:

               This function is very useful inside a loop over all subsets to
               compose the coordinate words of the next subset.
               The subset level is available from GDSINP and the box corners
               (in grids) are returned by BOXINP. Adding the minimum grids to
               the subset level gives the lower left coordinate word and adding
               the maximum grids to it gives the upper right coordinate word of
               the next box.

Updates:       Dec  5, 1989: WZ, migrated to C
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file
               Mar 24, 1994: JPT, modified to cooperate with GDS server.
               Feb 21, 1995: JPT, return -1 in case of error.
#<

@ integer function gdsc_fill( character, 
@                             integer, 
@                             integer )

----------------------------------------------------------------------------*/

#include    "gdsparams.h"
#include    "gdserrors.h"
#include    "gdsd_basic.h"

fint   gdsc_fill_c( fchar     set,               /* set name                */
                    fint     *subset,            /* coord.word to be filled */
                    fint     *grids )            /* array with grid values  */
{
   fint        iax, coord_word, err = 0, grid, nax_in = 0, naxis;
   gds_coord   *setinfo;

   (void)gds_rhed(set, &setinfo);
   coord_word = *subset;
   naxis =  setinfo->naxis;
   for ( iax = 0; iax < naxis; iax++ ) {
      grid = gds___unpack_c( set, &coord_word, &iax, &err );  
      if ( err == GDS_COORDUNDEF ) {       /* undefined axis */
         err = 0;                          /* fill corresponding grid value */
         gds___pack_c( set, &coord_word, grids+nax_in, &iax, &err );
         if (err<0) return -1;
         nax_in += 1;
      }
   }
   return( coord_word );
}
