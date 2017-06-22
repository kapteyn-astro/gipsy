/*============================================================================
                                  gdsc_grid.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsc_grid.dc2
Function:      GDSC_GRID

Purpose:       extract a grid value from a coordinate word

File:          gdsc_grid.c

Category:      GDS

Author:        W. Zwitser

Use:           INTEGER*8  GDSC_GRID( SET,           Input       character
                                   AXNUM,         Input       integer
                                   CWORD,         Input       integer*8
                                   ERROR )        In/Out      integer

               GDSC_GRID     extracted grid value

               SET           set name      

               AXNUM         axis number ( 1...naxis ) of grid in CWORD

               CWORD         coordinate word from which grid is extracted

               ERROR         0  = successful
                            <0  = a GDS error

Updates:       Dec  5, 1989: WZ, migrated to C
               Mar 25, 1994: JPT, modified for GDS server
#<

@ integer function gdsc_grid( character, 
@                             integer,
@                             integer*8,
@                             integer )

----------------------------------------------------------------------------*/

#include    "gipsyc.h"
#include    "gds___unpack.h"

fint8   gdsc_grid_c( fchar     set,                           /* name of set */
                    fint     *axnum,                         /* axis number */
                    fint8     *coord_word,                    /* coord.word  */
                    fint     *err )                          /* error code  */
{
   fint        iax;
   fint8       grid;
   
   iax = *axnum - 1;
   grid = gds___unpack_c( set, coord_word, &iax, err );
   return( grid );
}
