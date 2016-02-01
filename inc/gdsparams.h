/* gdsparams.h
                              COPYRIGHT (c) 1994
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#> gdsparams.dc3
Header:     gdsparams.h

Purpose:    defines parameters and structure for GDS.

File:       gdsparams.h

Author:     J.P. Terlouw

Updates:    Mar 14, 1994: JPT, New document.
            Aug  8, 1995: JPT, GDS_NAMLEN increased from 80 to 256
#<
*/
#if !defined(_gdsparams_h_)
#define _gdsparams_h_
#include "gipsyc.h"


/* ------------------------------- CONSTANTS ----------------------------- */
#define GDS_VERSION     3            /* version number                     */
#define GDS_SUBVERSION  0            /* sub-version number                 */
#define GDS_MAXDIM     20            /* maximum number of dimensions       */
#define GDS_KEYLEN     21            /* descriptor key length (bytes)      */
#define GDS_NAMLEN    256            /* set name length (bytes)            */

/* -------------------------- COORDINATE STRUCTURE ----------------------- */
typedef struct {
   double origin[GDS_MAXDIM];
   fint   size  [GDS_MAXDIM];
   fint8   factor[GDS_MAXDIM+1];
   fint   naxis;
   char   name  [GDS_NAMLEN];
} gds_coord;                            /* used by RHED and WHED functions */

#endif /* _gdsparams_h_ */


