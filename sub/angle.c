/* angle.c

        Copyright (c) Kapteyn Laboratorium Groningen 1993
        All Rights Reserved.

#>            angle.dc2

Function:     ANGLE

Purpose:      Calculate angle in radians between two points given in grids

File:         angle.c

Author:       M. Vogelaar

Use:          DOUBLE ANGLE( SET,       Input      CHARACTER*(*)
                            SUBSET,    Input      INTEGER
                            AXNUM ,    Input      INTEGER
                            GRID1,     Input      DOUBLE PRECISION ARRAY
                            GRID2,     Input      DOUBLE PRECISION ARRAY
                            ERR )      Output     INTEGER

              ANGLE    Spatial distance between GRID1 and GRID2 in radians.
              SET      Name of GDS set.
              SUBSET   Subset in which grids are specified.
              AXNUM    Array containing the axis numbers.
              GRID1    Grid position in subset.
              GRID2    Second grid position in subset. Between these
                       grids, the spatial distance will be calculated.
              ERR      If an error occurred, the error code is returned in ERR and
                       for the angle a double precision blank is substituted. 
                       Positive error numbers correspond to COTRANS errors. The
                       negative values have different origin.
                       ERR = -20  No conversion to DEGREE could be made
                       ERR = -21  Could not find units
                       ERR = -22  No spatial axes in subset.
                       
                       
Description:  ANGLE calculates the spatial distance between two grids. 
              The grids are entered as double precision numbers in GRID1 
              and GRID2. The dimension of the grid arrays is equal to the 
              dimension of the subset. The axis permutation array AXNUM
              is the same axis permutation array as created by GDSINP. If 
              there is at least one spatial axis in the subset, a distance 
              in radians can be calculated. If no coordinate transformation
              is possible, an error number (corresponding to COTRANS error
              numbers) is returned (all > 0). Negative errors are returned
              if something is wrong with the units, or if there is no
              spatial axis in the subset. Then a double precision blank
              is returned in ANGLE.
              
Example:

      maxpos  = 1;
      dfault  = NONE;   
      Key     = tofchar("START=");
      Mes     = tofchar("Give start position (x0,x1,..xn):");
      numpos  = gdspos_c(start, &maxpos, &dfault, Key, Mes, Setin, &subin[0]);
      Key     = tofchar("END=");
      Mes     = tofchar("Give end position (y0,y1,..yn):");
      numpos  = gdspos_c(end, &maxpos, &dfault, Key, Mes, Setin, &subin[0]);
  
      myangle = angle_c( Setin, &subin[0], axnum, start, end, &r1 );
                     
      if (r1 == 0) (void) sprintf( message, 
                                  "Angle = %f Radians %f degree", 
                                   myangle, DEG(myangle)   );
             
              

Updates:      Aug 12, 1993: VOG, Document in system

#<

Fortran to C interface:

@ double precision function angle(   character, 
@                                    integer, 
@                                    integer, 
@                                    double precision, 
@                                    double precision,
@                                    integer )


*/


#include        "stddef.h"              /* <stddef.h> */
#include        "stdio.h"               /* <stdio.h> */
#include        "ctype.h"               /* <ctype.h> */
#include        "string.h"              /* <string.h> */
#include        "stdlib.h"              /* <stdlib.h> */
#include        "math.h"                /* <math.h> */
#include        "gipsyc.h"              /* GIPSY symbols and definitions */

#include        "gdsc_ndims.h"
#include        "ncoords.h"
#include        "gdsd_rchar.h"
#include        "anyout.h"
#include        "cotrans.h"
#include        "fblank.h"
#include        "axtype.h"
#include        "setdblank.h"
#include        "gdsd_rdble.h"
#include        "axcoord.h"
#include        "factor.h"




double angle_c( fchar   Setin, 
                fint   *subin, 
                fint   *axnum, 
                double *start, 
                double *end, 
                fint   *err )
/*---------------------------------------------------------------*/
/* Get spatial distance between two points in a (sub) set. The   */
/* points are in grids.                                          */ 
/*---------------------------------------------------------------*/
{
#define   MAXAXES        10
#define   FTSLEN         20
#define   RAD(a)         ( a * 0.017453292519943295769237 )
#define   DEG(a)         ( a * 57.295779513082320876798155 )
     
#define ERR_NOCONV    -20
#define ERR_NOUNITS   -21
#define ERR_NOSPATIAL -22

   fint   subdim, setdim;           /* Dimensions */
   int    i;                        /* Counter */
   char   ctypebuf[FTSLEN];         /* Character buffer */
   fchar  Ctype;                    /* Type of axis, RA, DEC, etc. */
   char   cunitbuf[FTSLEN];         /* Character buffer */   
   fchar  Cunit;                    /* Units of axis (for conversion to DEGREE) */
   fint   r1;                       /* Return value */
   fint   setlevel = 0;             /* Header on top level */
   fint   colev;                    /* 'COTRANS' level (prim. or sec. axis) */
   fint   grid2phys = 1;            /* 'COTRANS' converts from grids to physicals */
   double physstart[MAXAXES];       /* Start grids converted to physical coordinates */
   double physend[MAXAXES];         /* End grids converted to physical coordinates */
   int    naxis;                    /* Number of axes in set (included hidden axes) */
   double radians;                  /* The result angle in radians */
   double dblank;                   /* The return value if something went wrong */
   fint   axtype, axcoord;          /* Axes properties */
   char   nunitbuf[FTSLEN];         /* Character buffer for dummy units */
   char   sunitbuf[FTSLEN];
   fchar  Nunit, Sunit;             /* Dummy units */
   fint   skysys, prosys, velsys;   /* The systems, returned by axtype routine */
   int    longax, latax;            /* Index for longitude and latitude axis */
   double fact[2];                  /* Conversion factors (convert to DEGREE), long, lat */
   double longval1, latval1;        /* Spatial coodinates in radians */
   double longval2, latval2;         
   

   *err = 0;                                     /* Return code if error detected */
   setdblank_c( &dblank );      
   Ctype.a = ctypebuf; Ctype.l = FTSLEN;
   Cunit.a = cunitbuf; Cunit.l = FTSLEN;
   Nunit.a = nunitbuf; Nunit.l = FTSLEN;
   Sunit.a = sunitbuf; Sunit.l = FTSLEN;   
   subdim  = gdsc_ndims_c( Setin, subin );       /* Dim. of input subset */
   setdim  = gdsc_ndims_c( Setin, &setlevel );   /* Dim. of set */
   naxis   = (int) ncoords_c( Setin );           /* Total number of axes in set */
   longax  = -1;                                 /* Initialize: no long, lat found */
   latax   = -1;
   /*-----------------------------------------------------*/
   /* Start loop over all axes, including the hidden axes */
   /*-----------------------------------------------------*/   
   for (i = 0; i < naxis; i++) {      
      /*------------------------------------------*/               
      /* If this is a hidden axis, the axnum      */
      /* array has to be updated.                 */
      /*------------------------------------------*/ 
      if (i >= setdim) axnum[i] = i + 1;
      /*------------------------------------------*/
      /* Get type and units of this axis. The     */
      /* type is necessary to determine whether   */
      /* the axis is spatial or not. The units    */
      /* are needed to convert to degree.         */
      /*------------------------------------------*/      
      axcoord = axcoord_c( Setin, &axnum[i], Ctype, Cunit, &colev );      
      axtype  = axtype_c( Ctype, Nunit, Sunit, &skysys, &prosys, &velsys );
/*          
      { 
         fint   logdev = 1; char   message[80];               
         (void) sprintf( message, "i=%d, axnum[i]=%d, axcoord=%d, axtype=%d",
                         i, axnum[i], axcoord,axtype );
         anyout_c( &logdev, tofchar(message) );}
      }
*/      
      /*------------------------------------------------*/
      /* The axis was a spatial axis. If units could be */
      /* found, determine the conversion factor.        */
      /*------------------------------------------------*/      
      if ((axtype == 1) || (axtype == 2)) {
         if (axcoord == 0) {                       /* successful return of axis units. */
            r1 = factor_c( Cunit, 
                           tofchar("DEGREE"), 
                           &fact[(int)axtype-1] ); /* [0] for long. & [1] for lat */
            if (r1 != 0) {
               *err = ERR_NOCONV;
               return( dblank );
            }
         } else {
            *err = ERR_NOUNITS;
            return( dblank );
         }
         if (axtype == 1) longax = i;
         else             latax  = i;
      }
   }   
   
   /*-------------------------------------------------------*/
   /* Here all parameters to convert to an angle are known. */
   /*-------------------------------------------------------*/   
   r1 = cotrans_c( Setin, subin, start, physstart, &grid2phys );   
   if (r1 != 0) {
      *err = r1;
      return( dblank );
   } else {
      r1 = cotrans_c( Setin, subin, end, physend, &grid2phys );
   }
  
 
/* 
   {  fint   logdev = 1; char   message[80];               
      for (i = 0; i < naxis; i++) {
         (void) sprintf( message, "Phys %d (axnum=%d) = %g %g", i, 
                         axnum[i], physstart[axnum[i]-1], physend[axnum[i]-1] );
         anyout_c( &logdev, tofchar(message) );
      }
   }
*/


   radians = dblank;   
   /*---------------------------------------------------------------*/
   /* Found two spatial axes and at least one is part of the subset */
   /*---------------------------------------------------------------*/   
   if ( (longax >= 0) && (latax >= 0) && ((longax < subdim) || (latax < subdim)) ) {
      longval1 = RAD(fact[0]*physstart[axnum[longax]-1]);
      latval1  = RAD(fact[1]*physstart[axnum[latax] -1]);
      longval2 = RAD(fact[0]*physend[axnum[longax]-1]);
      latval2  = RAD(fact[1]*physend[axnum[latax] -1]);           
      radians  = acos(sin(latval1)*sin(latval2) +
                      cos(latval1)*cos(latval2)*cos(longval1-longval2));
   } else {
      if ((longax >= 0) && (longax < subdim)) {
         /*--------------------------------------*/
         /* Longitude alone, and part of subset  */
         /*--------------------------------------*/         
         longval1 = RAD(fact[0]*physstart[axnum[longax]-1]);
         longval2 = RAD(fact[0]*physend[axnum[longax]-1]);
         radians  = longval1 - longval2;
      } else if ((latax >= 0) && (latax < subdim)) {
         /*--------------------------------------*/
         /* Latitude alone, and part of subset   */
         /*--------------------------------------*/         
         latval1  = RAD(fact[1]*physstart[axnum[latax] -1]);
         latval2  = RAD(fact[1]*physend[axnum[latax] -1]);
         radians  = latval1 - latval2;
      } else {
         *err = ERR_NOSPATIAL;
         return( dblank );
      } 
   }
   return(fabs(radians));
}


