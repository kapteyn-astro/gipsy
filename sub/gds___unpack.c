/*============================================================================
                               gds___unpack.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

*/

#include    "gipsyc.h"
#include    "gdsparams.h"
#include    "gdserrors.h"
#include    "gdsd_basic.h"
#include    "dpfpfl.h"
#include    "math.h"

#define MAX(a,b)        ((a)>(b)?(a):(b))
#define MIN(a,b)        ((a)<(b)?(a):(b))
/* Old definition #define NINT(a) ( (a)<0 ? (fint)((a)-.5) : (fint)((a)+.5) )*/
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )

static   gds_coord   *setinfo;
static   char        ok;

/*============================================================================
                               gds___unpack
------------------------------------------------------------------------------

#> gds___unpack.dc3

Function:      GDS___UNPACK

Purpose:       unpack a grid value from a coordinate word

Category:      GDS

File:          gds___unpack.c

Author:        W. Zwitser

Use:           INTEGER*8 GDS___UNPACK( SET,             Input        character
                                     COORD_WORD,      Input        integer*8
                                     IAX,             Input        integer
                                     ERR )            In/Out       integer

               GDS___UNPACK   grid value

               SET            name of set

               COORD_WORD     coordinate word from which grid is unpacked

               IAX            axis number of grid value
                              ( 0 ... N-1 with N axes )

               ERR            error return code :
                              =  0 : successful
                               -11 : iax < 0 or iax >= number of set axes
                               -19 : axis undefined in coordinate word

Updates:       Dec  5, 1989: WZ,  migrated to C
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file
               Mar 23, 1994: JPT, modified to cooperate with GDS server
               Apr 09, 2009: VOG, Changed definition of NINT to force correct
                                  processing of coordinates when CRPIX ends
                                  on .5 in all functions dealing with coordinates.

#<

@ integer*8 function gds___unpack( character,
@                                integer*8,
@                                integer,
@                                integer )

----------------------------------------------------------------------------*/

fint8   gds___unpack_c( fchar     set,               /* name of set          */
                       fint8     *coord_word,        /* coord.word to unpack */
                       fint     *iax,               /* axis number          */
                       fint     *err )              /* error code           */
{
   double   origin;
   fint     naxis;
   fint8    f_coord, factor;
   fint     err_i;

   err_i = gds_rhed(set,&setinfo);
   if (gds___fail(!err_i,err_i,err)) return 0;
   naxis = setinfo->naxis;
   ok  = *iax >= 0 && *iax < naxis;
   if (gds___fail( ok, GDS_NOTUNIQ, err )) return 0;
   factor = setinfo->factor[naxis];
   if ( *coord_word >= factor && ( *iax + 1 ) == naxis ) {
      factor = setinfo->factor[*iax];
      f_coord = *coord_word / factor;
   } else {
      factor = setinfo->factor[*iax+1];
      f_coord = ( *coord_word % factor ) / setinfo->factor[*iax];
   }
//   anyoutf(1, "unpack: %lld %lld %lld %d\n", *coord_word, f_coord, factor, *iax);
   if ( f_coord == 0 ) {
      f_coord = 1;
      *err = GDS_COORDUNDEF;
   }
   origin = setinfo->origin[*iax];
   return( f_coord - NINT( origin ) );
}

/*============================================================================
                               gds___pack
------------------------------------------------------------------------------

#> gds___pack.dc3

Subroutine:    GDS___PACK

Purpose:       pack a grid value into a coordinate word

Category:      GDS

File:          gds___unpack.c

Author:        W. Zwitser

Use:           CALL GDS___PACK( SET,               Input     character
                                COORD_WORD,        In/Out    integer*8
                                GRID,              Input     integer*8
                                IAX,               Input     integer
                                ERR )              In/Out    integer

               SET          name of set

               COORD_WORD   coordinate word in which grid value is packed

               GRID         grid value

               IAX          axis number of grid value
                            ( 0 ... N-1 with N axes )

               ERR          =  0 : successful
                             -34 : grid < minimum or grid > maximum (limited)
                             -35 : grid > maximum of last axis      (limited)
                                   In this case the axis can be extended.

               Pack a grid coordinate only after a call gds___unpack with
               err=-19 to check whether this axis is undefined.

Updates:       Dec 5,  1989: WZ, migrated to C
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file
               Mar 23, 1994: JPT, modified to cooperate with GDS server

#<

@ subroutine gds___pack( character,
@                        integer*8,
@                        integer*8,
@                        integer,
@                        integer )

----------------------------------------------------------------------------*/

void  gds___pack_c( fchar     set,          /* name of set                  */
                    fint8     *coord_word,   /* coord.word                   */
                    fint8     *coord,        /* grid to pack into coord_word */
                    fint     *iax,          /* axis number of coord         */
                    fint     *err )         /* error code                   */
{
   double   origin;
   fint     naxis, size, err_i;
   fint8    grid, min_g, max_g, factor;
   int      extend, fix;

   err_i = gds_rhed(set,&setinfo);
   if (gds___fail(!err_i,err_i,err)) return;
   naxis = setinfo->naxis;
   ok  = *iax >= 0 && *iax < naxis;
   if (gds___fail( ok, GDS_NOTUNIQ, err ))
      return;                                /* axis number within limits ? */
   origin = setinfo->origin[*iax];
   min_g = 1 - NINT( origin );                         /* min. grid of axis */
   size = setinfo->size[*iax];
   max_g = min_g + size - 1;                           /* max. grid of axis */
   extend = *coord > max_g && ( *iax + 1 ) == naxis;           /* extend ax?*/
   fix    = !extend && ( *coord < min_g || *coord > max_g );
   if ( fix )                              /* fix coordinate to axis length */
      grid = MIN( max_g, MAX( min_g, *coord ) );
   else
      grid = *coord;
   factor = setinfo->factor[*iax];
   *coord_word += ( grid - min_g + 1 ) * factor;
   //anyoutf(1,"pack: %lld %lld %lld %lld %d %d\n", *coord_word, *coord, min_g, max_g, *iax, naxis);
   if (gds___fail( !fix, GDS_COUTRANGE, err )) return;
                                        /* coordinate outside axis limits ? */
   if (gds___fail( !extend, GDS_CTOOBIG, err )) return;
                                                  /* coordinate > maximum ? */
}
