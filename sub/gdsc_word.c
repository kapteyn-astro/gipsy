/*============================================================================
                                 gdsc_word.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsc_word.dc2
Function:      GDSC_WORD

Purpose:       apply a grid to a coordinate word and return the new one

Category:      GDS

File:          gdsc_word.c

Author:        W. Zwitser

Use:           INTEGER  GDSC_WORD( SET,            Input       character
                                   AXNUM,          Input       integer
                                   GRID,           Input       integer
                                   CWORD,          Input       integer
                                   ERROR )         In/Out      integer

               GDSC_WORD     new coordinate word ( CWORD + GRID )

               SET           set name

               AXNUM         axis number ( 1...naxis )

               GRID          grid value to be added in CWORD

               CWORD         coordinate word

               ERR           0  = successful
                            <0  = a GDS error

Updates:       Dec 18, 1989: WZ, migrated to C
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file
               Mar 24, 1994: JPT, modified to cooperate with GDS server.
#<

@ integer*8 function gdsc_word( character,
@                             integer,
@                             integer*8,
@                             integer*8,
@                             integer )

----------------------------------------------------------------------------*/

#include    "gdsparams.h"
#include    "gdserrors.h"
#include    "gdsd_basic.h"

fint8   gdsc_word_c( fchar     set,                /* name of set            */
                    fint     *axnum,              /* axis number            */
                    fint8     *grid,               /* grid value to be added */
                    fint8     *coord_word,         /* coordinate word        */
                    fint     *err )               /* error code             */
{
   fint8       grid_1, new_coord;
   fint        iax, ix, naxis;
   gds_coord   *setinfo;

   (void)gds_rhed(set, &setinfo);
   iax = *axnum - 1;
   naxis = setinfo->naxis;
   grid_1 = gds___unpack_c( set, coord_word, &iax, err ); /* axis defined ? */
   if ( *err != GDS_COORDUNDEF ) {         /* yes: make new coordinate word */
      for ( ix = 0; ix < naxis; ix++ ) {
         if ( ix != iax ) {                      /*from grids on other axes */
            grid_1 = gds___unpack_c( set, coord_word, &ix, err );
            gds___pack_c( set, &new_coord, &grid_1, &ix, err );
         }
      }
   } else {
      new_coord = *coord_word;             /* no: take this coordinate word */
      *err = 0;
   }
   if ( presentn_c( grid ) ) {                       /* add new grid values */
      gds___pack_c( set, &new_coord, grid, &iax, err );
   }
   return( new_coord );
}
