/*============================================================================
                                  gdsc_name.c
------------------------------------------------------------------------------

                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsc_name.dc2
Function:      GDSC_NAME

Purpose:       return the name of an axis

Category:      GDS

File:          gdsc_name.c

Author:        W. Zwitser

Use:           CHARACTER  GDSC_NAME( SET,           Input       character
                                     AXNUM,         Input       integer
                                     ERROR )        In/Out      integer

               GDSC_NAME     name of axis AXNUM

               SET           set name      

               AXNUM         axis number ( 1...naxis )

               ERROR         0  = successful
                            <0  = a GDS error

Updates:       Dec 5, 1989: WZ, migrated to C
#<

@ character function gdsc_name( character, 
@                               integer, 
@                               integer )

----------------------------------------------------------------------------*/

#include    "gipsyc.h"
#include    "gdsd_rchar.h"
#include    "gdst_abslevel.h"
#include    "gdsd_basic.h"

static   char     key_s[10], dum[80];

void   gdsc_name_c( fchar     axname,                       /* name of axis */
                    fchar     set,                          /* set name     */
                    fint     *axnum,                        /* axis number  */
                    fint     *err )                         /* error code   */
{
   fchar    key;
   fint     level = 0;                         /* axis name is on top level */
   fint     yes = TRUE, no = FALSE;
   
   sprintf( key_s, "CTYPE%d", *axnum );       /* compose key of header item */
   key = tofchar( key_s );
   gdst_abslevel_c( &yes );
   gdsd_rchar_c( set, key, &level, axname, err );     /* read header item */
   gdst_abslevel_c( &no );
   (void)gds___char2str( axname, dum, sizeof(axname) );
   return;
}
