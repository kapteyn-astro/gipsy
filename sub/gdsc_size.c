/*============================================================================
                                  gdsc_size.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsc_size.dc2
Function:      GDSC_SIZE

Purpose:       return the size of an axis

Category:      GDS

File:          gdsc_size.c

Author:        W. Zwitser

Use:           INTEGER  GDSC_SIZE( SET,           Input       character
                                   AXNUM,         Input       integer
                                   ERROR )        In/Out      integer

               GDSC_SIZE     size of axis

               SET           set name      

               AXNUM         axis number  ( 1...naxis )

               ERROR         0  = successful
                            -16 = axis not present

Updates:       Dec  5, 1989: WZ, migrated to C
               Oct 31, 1990: WZ, 'naxis', etc. from setsta -> dsc_file
               Mar 24, 1994: JPT, modified to cooperate with GDS server.
#<

@ integer function gdsc_size( character, 
@                             integer,
@                             integer )

----------------------------------------------------------------------------*/

#include    "gdsparams.h"
#include    "gdserrors.h"
#include    "gdsd_basic.h"

fint   gdsc_size_c( fchar     set,                           /* name of set */
                    fint     *axnum,                         /* axis number */
                    fint     *err )                          /* error code  */
{
   fint        iax, naxis, size;
   gds_coord *setinfo;
      
   (void)gds_rhed(set, &setinfo);
   iax = *axnum - 1;
   naxis = setinfo->naxis;
   if (gds___fail( iax < naxis, GDS_SZAXNOTFOUND, err )) return 0;
   size = setinfo->size[iax];
   return( size );
}
