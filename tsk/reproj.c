/*
                           COPYRIGHT (c) 1995
                      Kapteyn Astronomical Institute
                  University of Groningen, The Netherlands
                           All Rights Reserved.

#>             reproj.dc1

Program:       REPROJ

Purpose:       Re-project a spatial map into another map with a different
               coordinate system.

Category:      COORDINATES, HEADER, MANIPULATION

File:          reproj.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give set (subset(s)) with data to re-project:

               Input set (and subsets) which contain sky maps which
               should be reprojected. Maximum number of subsets is
               2048.


   BOX=        Area which should be reprojected:        [entire structure]

               The input area that you select here is the part
               that will be reprojected. Data outside this box will
               not be transferred.


   DEFSET=     Copy system from reference set, subsets:           [manual input]

               If specified, the defaults for coordinate parameters
               OUTPOS=, CDELT=, CROTA=, EPOCH=, OUTBOX=, SKYSYS=, and
               PROSYS= will be taken from this set.


   ROTATEONLY= Rotation only (i.e.fixed sky-&proj.systems)?          Y/[N]

               If you only want to rotate a map and do not change the
               sky and projection systems (ROTATEONLY=Y) then the keywords
               SKYSYS=, EPOCH=, CROTA= and PROSYS= are skipped.


   OUTPOS=     Give proj. center for output:         [copy from input map]

               Specify for the output map the position of the
               projection center (PC), i.e. a position somewhere in
               grid (0,0). The exact position (given by OUTPOS='PC')
               is derived from the fractional parts of the values
               of CRPIXn in the header in the set.
               This position can be specified either in grid coordinates
               or in world coordinates.

               The PC is the intersection of the line of sight with the
               celestial sphere and as such relates the projection system
               to the grid mesh. A change in PC thus implies a repro-
               jection of the projection system onto the grid and only
               equals a shift in pixels if both, the input and output
               systems are flat (axis name extension FLT).

               Note that the order of input is always x, y, i.e. longitude-
               latitude (e.g. for a RA-DEC map) or latitude-longitude
               (e.g. for a DEC-RA map) depending on the header values
               of 'CTYPE'.


   CDELT=      New grid spacings (Dx,Dy) in ARCSEC: [copy from input map]

               Regrid the input to these new grid spacings. Note that
               the grid spacings are always in seconds of arc, NOT in
               the units of your header (CUNIT).


   CHANGE=     Do you want to change sign of cdelt?                 [Y]/N

               If you entered a positive value for the grid spacing
               in longitude in an equatorial system, then CHANGE=Y
               will change its sign.


   ROTANGLE=   Rotate map over .... degrees:                        [0.0]

               Specify an angle in degrees over which you want to
               rotate the input. The direction is counter clockwise
               for systems with a negative grid spacing in longitude.
               Note that this value will be added to the value of
               'CROTA' (see description at CROTA=) as found in the
               header of the input set (see description!).


               If ROTATEONLY=N then also the keywords SKYSYS=,
               EPOCH= and PROSYS are asked.


   SKYSYS=     Output sky system:                   [copy from input map]

               Change a map in a way that the x- and y axes of the image
               correspond to the principal axes of the new sky system.
               The following sky systems are implemented:

               Skysys  CTYPE_l  CTYPE_m        Meaning
               ===================================================
                 1      RA      DEC      Equatorial (EPOCH 1950.0)
                 2      GLON    GLAT     Galactic
                 3      ELON    ELAT     Ecliptic (EPOCH 1950.0)
                 4      SLON    SLAT     Super galactic
                 5      RA      DEC      Equatorial (EPOCH 2000.0)


   EPOCH=      Give epoch of new sky system:        [copy from input map]

               For equatorial and ecliptic input sky systems it is
               possible to change the epoch.
               The default epoch is copied from the input set. If the
               input set has no epoch keyword in its header, then 2000.0
               is assumed. If the sky system in the header is number 1
               (see table above) and the epoch is 2000.0, then REPROJ
               changes this number automatically into 5.


   PROSYS=     Output projection system:            [copy from input map]

               Change the projection system. Default is the projection
               system of the input map. Projection systems can be
               identified in the axis names by the following postfixes:

               PROSYS  postfix           Meaning
               =================================================

                 1      AIT      Aitoff Equal Area projection
                 2      CYL      Equivalent Cylindrical projection
                 3      FLT      flat projection
                 4      TAN      Gnomonic projection
                 5      SIN      Orthographic projection
                 6      ARC      Rectangular projection
                 7      GLS      Transversal projection
                 8      NCP      North Celestial Pole projection
                 9      STG      Stereographic projection
                10      MER      Mercator projection



   CROTA=      New value for map rotation (deg.):   [copy from input map]

               Header item CROTA stores the map rotation.
               CROTA is the angle in degrees between the +y axis and
               the +m axis (latitude axis) at the position of the PC.
               The angle is counter-clockwise if the grid separation
               (CDELT) in longitude is < 0.0. If you want to rotate
               AND change the sky- and projection system as well,
               use the CROTA= keyword, else use ROTATEONLY=Y and
               ROTANGLE=  (See description for additional information).


   OUTBOX=     New box (in grids):                         [minimum size]

               The box of the input set is transformed using the
               given transformation parameters (OUTPOS=, CDELT=, CROTA=
               SKYSYS= and PROSYS=) to a box in the new system. This
               is the output box that just contains the entire input
               image after transformation and is therefore also the
               default box.


   DIMINISH=   Decrease output dimensionality to 2?                [Y]/N

               If you entered ONE 2-dim subset from a set with 3 or
               more axes, then the default output dimension is not 
               copied from the input set, but is reduced to two.
               This avoids huge and almost empty sets to be created.
               Lost axes become so called hidden axis.


   OUTSET=     Give output set, subset(s):

               This will be the destination of the reprojected set.
               A name is sufficient.


   SPEEDMAT=   Size of 'position' interpolation box:                [1,1]

               Accelerate the calculations by interpolating positions
               instead of transforming them using formulas for coordinate
               transformations. For interpolation boxes with a
               size bigger than 1 x 1 the result is obtained faster
               but less accurate. 


   DATAMODE=   Data acquisition (B)ilinear/(N)earest pix.:          [B]/N

               Obtain output pixel values by (B)ilinear interpolation
               in a 2 x 2 matrix or get value of the (N)earest pixel.
               See description at: Interpolation of data (not positions).



Description:   This program re-projects a spatial input set into another
               set with a different coordinate system. A coordinate
               system is specified by the following parameters.

               -projection center,
               -grid spacing,
               -rotation angle,
               -sky system and epoch.
               -projection system

               New values for these parameters are either given by the
               user or read from the header of the set in DEFSET=


               Projection center:
               ==================

               The projection center (PC) is the intersection point of
               the line of sight with the celestial sphere. The PC in
               physical coordinates is attached to grid coordinate (0,0)
               but not necessarily in the center of that grid.
               Together with the sky & projection systems, the grid-
               spacing and the map rotation, it connects a physical
               coordinate system to a grid mesh.
               A PC can be specified in the standard GIPSY way:


               For spatial axes there are a number of prefixes:

               *        ;  for RA or DEC in resp. HMS and DMS.
               *1950    ;  for RA or DEC in resp. HMS and DMS in EPOCH 1950.0
               *xxxx.x  ;  for RA or DEC in resp. HMS and DMS in EPOCH xxxx.x
               G        ;  Galactic longitude or latitude in degrees
               E        ;  Ecliptic longitude or latitude in degrees
               S        ;  Supergalactic longitude or latitude in degrees

               The prefixes must be repeated for both directions.
               There must be a space between prefix and coordinate
               specification.

               Examples (e.g. an Equatorial map with longitude, latitude):

               OUTPOS=10 5
                            RA at grid 10 of the input map, DEC at grid 5

               OUTPOS=* 10 12 8 * -67 8 9.6

                            RA = 10 hour, 12 min, 8 sec,
                            DEC = -67 deg, 8 min, 9.6 sec.,
                            in a 2-d area and in the epoch as found in the
                            header of the set:

               OUTPOS=*2000.0 3 14 38.02 *2000.0 41 13 54.8

                            Input of RA  3 h 14 m 38.02 s,
                            DEC 41 d 13 m 54.84 s in epoch 2000.0:


               It is also possible to specify the grid center of the input map
               as the new projection center. This can be realized with
               'AC' (axis center e.g. OUTPOS=AC). If the length of axis i is
               NAXISi and the reference pixel is CRPIXi, then the i-th
               coordinate is given by the expression NAXISi / 2 - CRPIXi.

               A projection system is given in the input system. If you transform
               to another sky system, then this projection center is transformed
               to the new system before it is inserted in the output header.
               Now you are sure that the projection center is the same physical
               location in the sky.

               Grid spacing:
               =============

               REPROJ converts only spatial maps. For each axis in a map
               it must be possible to convert a spacing in header units
               to seconds of arc. The sign of the grid spacing in longitude
               determines the direction of rotation. For equatorial maps
               with a negative grid spacing in longitude (RA), rotation
               will be counter-clockwise.


               Rotation angle:
               ===============

               For rotations of maps where sky and projection systems
               are fixed, use ROTATEONLY=YES. Then the angle over
               which you want to rotate will be asked in ROTANGLE=.
               Otherwise the program will prompt you to give the new
               systems and the "header" rotation angle 'CROTA'.
               In both cases a rotation over x degrees implies a change
               in 'CROTA' with +/- x degrees. 'CROTA' is defined for
               the input PC and thus the rotation center is always
               this input center.
               For a rotation around a different center, program REPROJ
               has to run twice. The first time to change the PC, the
               second time to rotate the map.

               However, for maps which do not cover a too large fraction
               of the sky, and/or are not too close to a pole of the
               projection system, the result of a rotation is nearly
               independent of your choice of the rotation center.
               A rotation can then safely be performed around the
               default rotation center which is the original PC.


               Blanks:
               =======

               If a pixel at certain position in the input set is a
               blank, then the corresponding pixel in the output set
               will be set to blank. If a pixel in the output set
               corresponds to a pixel outside the box or frame of the
               input set, this pixel is set to blank also.


               Interpolation of data (not positions):
               ======================================

               For each grid position (integer x, y) in the OUTput
               set a grid position (floating x', y') in the INput
               set is calculated. The position (x', y') has distance
               dx, dy to the nearest pixel m1. If dx and dy were both
               0.0 then the pixel value of m1 is returned. However, in
               most cases the values for dx and dy will not be equal to
               0.0. Then the 3 closest neighbours are involved in an
               interpolation. If the pixel value at (x', y') is a
               blank, then a blank is returned to the output set.
               Else, an interpolation is used to calculate the pixel
               value.

               Given 4 values m1,m2,m3,m4 at positions:

                      m4       m3
                (0,1) o--------o (1,1)
                      |        |
                      |        |
                   dy ^        |
                      | dx     |
                (0,0) o-->-----o (1,0)
                      m1       m2


               and two fractions:  0.0 <= dx <= 0.5 and 0.0 <= dy <= 0.5
               then there are a number of interpolation schemes depending on
               the number of blanks pixels. If all pixels are non blank, a
               bi-linear interpolation is involved. If the start pixel m1 is
               blank then a blank is returned. If one of the other pixels
               is blank, the interpolation is in the plane of the (3) pixels
               that are not blank. For two non blank pixels there is a
               linear interpolation.

               The number of blanks in a map will therefore not increase
               (blot) or decrease (patch). If you change the values for
               the grid spacing, then only intensities will be conserved
               (not the flux!).


               Interpolation of positions (the 'speed matrix'):
               ================================================

               For a 1000x1000 pixels map, a rotation over 45 degrees
               takes 63.3 cpu sec on a certain machine if all pixels
               were transformed. But if a 'speedmatrix' of 1000x1000 is
               used then the process takes 14.4 cpu sec. The less
               linear the projection is, the less accurate the re-projection.


               Re-projecting a map without a coordinate system:
               ===============================================

               It is not possible to shift or rotate an arbitrary
               map without a valid coordinate system. You can fit
               such a coordinate system if you know the physical
               position of some grids in your map by using ASTROM.
               A coordinate system of an existing set can be changed
               by program FIXHED.


               Appendix: Function transform()
               ==============================

               Here we outline the procedure of transforming one
               system to another. The core is function transform().
               Four positions (each two coordinates) of the input box
               are transformed to the corresponding world coordinates.
               These are transformed back to grids for the new system.
               From this new set of grids one can compose a new box
               for the output set. Then for each grid in the output set
               the corresponding grid in the input set is calculated via
               intermediate world coordinates. These grids in the input
               set usually are not integers so an interpolation
               using neighbours is applied (or not if the user wants the
               nearest pixel only).
               This way we sample the output set on its equally spaced
               grids.

               An small but important correction is made for non-integer
               header values of CRPIX. These fractional parts are the
               cause of the fact that the projection center needs not to be at
               the center of grid (0,0). These offsets are processed in
               function transform().

Example:       We want to extract one channel map from a GIPSY data cube
               and rotate that map 30 degrees (towards the East).
               The rotation is around the projection center of the input
               map.
               
               BOX=
               CDELT=
               DATAMODE=
               DEFSET=
               DIMINISH= y
               INSET= aurora velo 50
               OUTBOX=
               OUTPOS=
               OUTSET= aurora_rot30
               ROTANGLE= 30
               ROTATEONLY= y
               SPEEDMAT= 1 1


Updates:       Jul  10, 1990: VOG, Document created
               Apr  10, 1992: VOG, GDSPOS included for IN/OUTPOS=
               May  17, 1994: VOG, Changed integer position conversions
               Oct   4, 1994: VOG, Changed NINT in interpolation positions
                                   to INT and subtracted 1 for negative
                                   pixel positions.
               Sep  21, 1995: VOG, Rewritten in C
               Oct  31, 1995: VOG, Added 'getnewaxisname' function
               Jul  27, 2000: VOG, Changed local 'getipval' to
                                   external function interpol().
               Aug  25, 2000: VOG, Added DIMINISH= keyword to decrease 
                                   output dimensionality for one input
                                   subset from a > 2 dim cube.
               Apr  12, 2009: VOG,-Changed macro NINT to version with floor()
                                  -Grids corrected for offsets due to non integer
                                   values of CRPIX.
                                  -Changed default value of epoch from 1950 to 2000
                                  -Added documentation to core function transform().
               Jul  17, 2009: VOG, An extra transformation was added to transform
                                   CRVAL's (for the projection center) between
                                   two different sky systems.
               Oct  21, 2009: VOG, Removed bug in nearest integer data 
                                   acquisition M[x][y] -> M[y][x]
               Mar  24, 2013: VOG, Output axes names lost hyphens in CTYPE. Repaired
#<
*/

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "math.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"


/* GDS related */

#include "gdsinp.h"
#include "gdsout.h"
#include "gdsbox.h"
#include "gdsasn.h"
#include "gdscss.h"
#include "gdscss.h"
#include "gdscpa.h"
#include "gdspos.h"
#include "gds_errstr.h"
#include "gdsc_ndims.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsc_fill.h"
#include "gdsc_name.h"
#include "gdsc_origin.h"
#include "gdsd_rdble.h"
#include "gdsd_wdble.h"
#include "gdsd_rchar.h"
#include "gdsd_delete.h"
#include "gdsi_read.h"
#include "gdsi_write.h"



/* User input routines */

#include "userint.h"                         /* User input interface routines */
#include "userlog.h"
#include "userreal.h"
#include "userdble.h"
#include "usertext.h"
#include "usercharu.h"
#include "userfio.h"
#include "reject.h"                                      /* Reject user input */
#include "cancel.h"      /* Remove user input from table maintained by HERMES */


/* Axes related */

#include "axtype.h"
#include "axcoord.h"
#include "factor.h"


/* Transformations */

#include "proco.h"
#include "skyco.h"
#include "epoco.h"
#include "eclipco.h"


/* Miscellaneous */

#include "myname.h"
#include "nelc.h"
#include "setfblank.h"     /* fie. to set a data value to the universal blank */
#include "setdblank.h"
#include "dblank.h"
#include "grtoph.h"
#include "timer.h"
#include "minmax3.h"
#include "wminmax.h"
#include "status.h"
#include "getdate.h"    /* Returns the current time and date as a text string */
#include "interpol.h"


#define   VERSION          "3.0"            /* Version number of this program */
#define   SETNAMELEN        256
#define   SUBSMAX           2048
#define   AXESMAX           10
#define   TASKNAMLEN        20    /* Store task name in str. with this length */
#define   MAXBUFLINES       16             /* Height of (small) output buffer */
#define   NONE              0   /* Default values for use in userxxx routines */
#define   REQUEST           1
#define   HIDDEN            2
#define   EXACT             4
#define   FITSLEN           20
#define   LONGITUDE         1
#define   LATITUDE          2
#define   EQUATORIAL        1
#define   GALACTIC          2
#define   ECLIPTIC          3
#define   SUPERGALACTIC     4
#define   EQUATORIAL2000    5
#define   NO                0
#define   YES               1


/* Macro which creates room for a local fchar variable */
#define fmake(fchr,size) { \
                            static char buff[size+1]; \
                            int i; \
                            for (i = 0; i < size; buff[i++] = ' '); \
                            buff[i] = 0; \
                            fchr.a = buff; \
                            fchr.l = size; \
                         }


/* Copy a c-string to an fchar */
#define fcopy( f, c )                   \
        {int k;for(k=0;c[k]&&k<f.l;f.a[k]=c[k],k++);while(k<f.l)f.a[k++]=' ';}
        
       


#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define ISWAP(a,b)     { fint temp=(a);(a)=(b);(b)=temp; }     /* Swap 2 ints */
#define DSWAP(a,b)     { double temp=(a);(a)=(b);(b)=temp; }  /* Swap doubles */
/* Old definition: #define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )*/
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )

#define    KEY_INSET     tofchar("INSET=")
#define    KEY_DEFSET    tofchar("DEFSET=")
#define    KEY_OUTSET    tofchar("OUTSET=")
#define    KEY_SPEEDMAT  tofchar("SPEEDMAT=")


static fint     subin[SUBSMAX];      /* Array for the subset coordinate words */
static fint     subout[SUBSMAX];
static fint     subdef[SUBSMAX];
static fint     setlevel = 0;
static char     taskname[TASKNAMLEN+1];               /* Name of current task */
static float    blank;



static void dms( double degrees, char *convstr, int prec )
/*------------------------------------------------------------*/
/* PURPOSE: Convert degrees to deg/min/sec                    */
/*------------------------------------------------------------*/
{
   double    seconds;
   int       Idegs;
   double    min;
   int       Imin;
   int       negative;
   double    power;


   power = pow( 10.0, (double) prec );
   negative = 0;
   if ( degrees < 0 ) {
      negative = 1;
      degrees = fabs(degrees);
   }
   Idegs   = (int) degrees;
   min     = degrees*60.0 - ((double)Idegs)*60.0;
   Imin    = (int) min;
   seconds = min*60.0 - ((double)Imin*60.0 );
   /* Avoid rounding by formatting */
   seconds = (double) ((int) (seconds * power) ) / power;
   if (negative)
      sprintf( convstr, "-%2dd%2dm%5.*fs", Idegs, Imin, prec, seconds );
   else
      sprintf( convstr,  "%2dd%2dm%5.*fs", Idegs, Imin, prec, seconds );
}



static void hms( double degrees, char *convstr, int prec )
/*------------------------------------------------------------*/
/* PURPOSE: Convert degrees to hours/min/sec                  */
/*------------------------------------------------------------*/
{
   double    seconds;
   double    hours;
   int       Ihours;
   double    min;
   int       Imin;
   int       negative;
   double    power;


   power = pow( 10.0, (double) prec );
   negative = 0;
   if ( degrees < 0 ) {
      negative = 1;
      degrees = -1.0 * degrees;
   }
   hours   = degrees / 15.0;
   Ihours  = (int) hours;
   min     = hours*60.0 - ((double)Ihours)*60.0;
   Imin    = (int) ( min );
   seconds = min*60.0 - ((double)Imin)*60.0;
   seconds = (double) ((int) (seconds * power) ) / power;
   if (negative)
      sprintf( convstr, "-%2dh%2dm%5.*fs", Ihours, Imin, prec, seconds );
   else
      sprintf( convstr,  "%2dh%2dm%5.*fs", Ihours, Imin, prec, seconds );
}



static int strcompare( fchar Str1,
                       fchar Str2 )
/*------------------------------------------------------------*/
/* PURPOSE: 'strcmp' for 'fchar' type strings.                */
/*------------------------------------------------------------*/
{
   fint  l1, l2;
   int   ok;

   l1 = nelc_c( Str1 );
   l2 = nelc_c( Str2 );
   if (l1 != l2)
      return( 0 );
   ok = ( strncmp( Str1.a, Str2.a, l2 ) == 0 );
   return( ok );
}



static int spatialmap( fchar Setin,
                       fint  *subin,
                       fint  *axistype )
/*------------------------------------------------------------*/
/* PURPOSE: Return a value != 0 if a map is spatial.          */
/* A spatial map has one spatial longitude axis and one       */
/* spatial latitude axis.                                     */
/*------------------------------------------------------------*/
{
   fint     subdim;
   int      dev = 16;

   subdim = gdsc_ndims_c( Setin, &subin[0] );
   if (subdim != 2)
   {
      anyoutf( dev, "Not a spatial map because (sub)set dimension != 2" );
      return( 0 );
   }
   if (!(axistype[0] == LONGITUDE || axistype[0] == LATITUDE))
   {
      anyoutf( dev, "Not a spatial map because first axis is not spatial" );
      return( 0 );
   }
   if (!(axistype[1] == LONGITUDE || axistype[1] == LATITUDE))
   {
      anyoutf( dev, "Not a spatial map because second axis is not spatial" );
      return( 0 );
   }
   if (axistype[0] == axistype[1])
   {
      anyoutf( dev, "Not a spatial map both axes are in same spatial direction" );
      return( 0 );
   }
   return( 1 );
}



static double getepoch( fchar Setin,
                        fint  skysys )
/*------------------------------------------------------------*/
/* PURPOSE: Return for the input set an EPOCH.                */
/* For equatorial and ecliptic skysytems, the epoch can be    */
/* used, for others a double blank is returned.               */
/*------------------------------------------------------------*/
{
   fint     r = 0;
   double   epoch = 0.0;

   if (skysys != EQUATORIAL && skysys != ECLIPTIC)
   {
      anyoutf( 16, "Epoch not relevant for this sky system!" );
      setdblank_c( &epoch);
      return( epoch );
   }
   gdsd_rdble_c( Setin, tofchar("EPOCH"), &setlevel, &epoch, &r );
   if (r < 0)
   {
      anyoutf( 1, "Cannot find an EPOCH in the header, 2000.0 assumed." );
      return( 2000.0 );
   }
   anyoutf( 1, "EPOCH:  %f", epoch );
   return( epoch );
}



static void factorerror( fchar  Cunit,
                         char   *tounit,
                         fint   r )
/*------------------------------------------------------------*/
/* PURPOSE: Display error message originating from 'factor'.  */
/*------------------------------------------------------------*/
{
   if (r == 51)
      anyoutf( 1, "FACTOR: Unknown units (%.*s) to convert from",
               nelc_c(Cunit), Cunit.a );
   if (r == 52)
      anyoutf( 1, "FACTOR: Unknown units (%s) to convert to", tounit );
   if (r == 53)
      anyoutf( 1, "FACTOR: Both units (%.*s and %s) are unknown",
               nelc_c(Cunit), Cunit.a, tounit );
   if (r == 54)
      anyoutf( 1, "FACTOR: Incompatible units %.*s and %s",
               nelc_c(Cunit), Cunit.a, tounit );
   if (r == 55)
      anyoutf( 1, "FACTOR: Ambiguous units (%.*s) to convert from",
               nelc_c(Cunit), Cunit.a );
   if (r == 56)
      anyoutf( 1, "FACTOR: Ambiguous units (%s) to convert to", tounit );
}



static bool getspatialdefaults( fchar   Setin,
                                fint    *subin,
                                fint    *axnum,
                                fint    *axistype,
                                double  *crvalXIN,
                                double  *crvalYIN,
                                double  *cdeltXIN,
                                double  *cdeltYIN,
                                double  *crpixXIN,
                                double  *crpixYIN,
                                double  *crotaIN,
                                double  *factorX,
                                double  *factorY,
                                fchar   CunitX,
                                fchar   CunitY,
                                fchar   CtypeX,
                                fchar   CtypeY )
/*------------------------------------------------------------*/
/* PURPOSE: Get defaults for transformation parameters from   */
/* the header of this input set.                              */
/*------------------------------------------------------------*/
{
   char     message[80];
   fint     r;
   int      i;
   int      result = 1;
   double   crota, crval, cdelt, crpix;


   crota = crval = cdelt = crpix = 0.0;
   for (i = 0; i < 2; i++)
   {
      /*-------*/
      /* CROTA */
      /*-------*/
      if (axistype[i] == LATITUDE)                   /* spatial axis latitude */
      {
         sprintf( message, "CROTA%d", axnum[i] );
         r = 0;
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crota, &r );
         if (r < 0)
            crota = 0.0;
      }
      r = 0;
      crpix = gdsc_origin_c( Setin, &axnum[i], &r );
      /*-------*/
      /* CRVAL */
      /*-------*/
      sprintf( message, "CRVAL%d", axnum[i] );
      r = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crval, &r );
      /*-------*/
      /* CDELT */
      /*-------*/
      sprintf( message, "CDELT%d", axnum[i] );
      r = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &cdelt, &r );
      /*-------*/
      /* CUNIT */
      /*-------*/
      if (i == 0)
      {
         *crpixXIN = crpix;
         *crvalXIN = crval;
         *cdeltXIN = cdelt;
         sprintf( message, "CUNIT%d", axnum[i] );
         r = 0;
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, CunitX, &r );
         sprintf( message, "CTYPE%d", axnum[i] );
         r = 0;
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, CtypeX, &r );
      }
      else
      {
         *crpixYIN = crpix;
         *crvalYIN = crval;
         *cdeltYIN = cdelt;
         sprintf( message, "CUNIT%d", axnum[i] );
         r = 0;
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, CunitY, &r );
         sprintf( message, "CTYPE%d", axnum[i] );
         r = 0;
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, CtypeY, &r );
      }
   }
   *crotaIN = crota;
   r = factor_c( CunitX, tofchar("ARCSEC"), factorX );
   if (r != 0)
   {
      *factorX = 1.0;
      factorerror( CunitX, "ARCSEC", r );
      result = 0;
   }
   r = factor_c( CunitY, tofchar("ARCSEC"), factorY );
   if (r != 0)
   {
      *factorY = 1.0;
      factorerror( CunitY, "ARCSEC", r );
      result = 0;
   }
   return( result );
}



void  axinfo( int  typenum,
              int  skynum,
              int  pronum,
              int  velnum,
              char *typestr,
              char *skystr,
              char *prostr,
              char *velstr )
/*------------------------------------------------------------*/
/* PURPOSE: Return a string containing a text that corresponds*/
/* to an axis type, sky/proj./vel. system.                    */
/*------------------------------------------------------------*/
{
   switch ( (int) typenum )
   {
      case 0:
         strcpy( typestr, "unknown type" );
         break;
      case 1:
         strcpy( typestr, "spatial axis longitude" );
         break;
      case 2:
         strcpy( typestr, "spatial axis latitude" );
         break;
      case 3:
         strcpy( typestr, "spectral axis frequency" );
         break;
      case 4:
         strcpy( typestr, "spectral axis velocity" );
         break;
      case 5:
         strcpy( typestr, "spectral axis wavelength" );
         break;
      case 6:
         strcpy( typestr, "spectral axis inverse wavelength" );
         break;
      case 7:
         strcpy( typestr, "spectral axis log(wavelength)" );
         break;
      case 8:
         strcpy( typestr, "time axis" );
         break;
      case 9:
         strcpy( typestr, "polarisation axis" );
         break;
      case 10:
         strcpy( typestr, "parameter axis" );
         break;
      case 11:
         strcpy( typestr, "sample axis of iras data" );
         break;
      case 12:
         strcpy( typestr, "tick axis of iras data" );
         break;
      case 13:
         strcpy( typestr, "detector axis of iras data" );
         break;
      case 14:
         strcpy( typestr, "snip axis of iras data" );
         break;
   }

   skystr[0] = '\0';
   prostr[0] = '\0';
   if ((typenum == 1) || (typenum == 2))
   {
      /* Display projection system */
      switch( (int) skynum )
      {
         case 1:
            strcpy( skystr, "equatorial" );
            break;
         case 2:
            strcpy( skystr, "galactic" );
            break;
         case 3:
            strcpy( skystr, "ecliptic" );
            break;
         case 4:
            strcpy( skystr, "supergalactic" );
            break;
      }

      switch( (int) pronum )
      {
         case 1:
            strcpy( prostr, "AITOFF equal area" );
            break;
         case 2:
            strcpy( prostr, "equivalent cylindrical" );
            break;
         case 3:
            strcpy( prostr, "flat" );
            break;
         case 4:
            strcpy( prostr, "gnomonic" );
            break;
         case 5:
            strcpy( prostr, "orthographic" );
            break;
         case 6:
            strcpy( prostr, "rectangular" );
            break;
         case 7:
            strcpy( prostr, "global sinusoidal" );
            break;
         case 8:
            strcpy( prostr, "north celestial pole (WSRT)" );
            break;
         case 9:
            strcpy( prostr, "stereographic" );
            break;
         case 10:
            strcpy( prostr, "mercator projection" );
            break;
      }
   }

   velstr[0] = '\0';
   if (typenum == 3)
   {
      /* Display projection system */
      switch( (int) skynum )
      {
         case 1:
            strcpy( velstr, "optical" );
            break;
         case 2:
            strcpy( velstr, "radio" );
            break;
      }
   }
   if (typenum == 4)
      strcpy( velstr, "radio" );
}



static int setinfo( fchar Setin,
                    fint  *subin,
                    fint  *axnum,
                    fint  *flo,
                    fint  *fhi,
                    fint  *axistype,
                    fint  nsubs,
                    fint  *systems )
/*------------------------------------------------------------*/
/* PURPOSE: List set characteristics. Only the axis types and */
/* the sky & and projection systems are returned.             */
/*------------------------------------------------------------*/
{
   fchar    Dummy1, Dummy2;
   char     orientation[20];
   char     typetxt[30];
   char     skytxt[30];
   char     protxt[30];
   char     veltxt[30];
   char     message[80];
   fint     r;
   fint     colev;
   fint     skysys, prosys, velsys;
   fint     subdim;
   int      i;
   int      dev = 8;                /* Show info, but not in experienced mode */
   double   crota;
   double   crval, drval;
   double   cdelt, ddelt;
   double   crpix;


   anyoutf( dev, "===== INFO ABOUT SELECTED AXES FROM [%.*s] =====",
            nelc_c( Setin ), Setin.a );

   fmake( Dummy1, 20 );
   fmake( Dummy2, 20 );
   subdim = gdsc_ndims_c( Setin, &subin[0] );

   for (i = 0; i < subdim; i++)
   {
      fchar  Ctype;
      fchar  Cunit;
      fchar  Dunit;
      fmake( Ctype, FITSLEN );
      fmake( Cunit, FITSLEN );
      fmake( Dunit, FITSLEN );

      /*-------*/
      /* CTYPE */
      /*-------*/
      sprintf( message, "CTYPE%d", axnum[i] );
      r = 0;
      gdsd_rchar_c( Setin, tofchar(message), &setlevel, Ctype, &r );
      if (r < 0)
      {
         anyoutf( 1, "Cannot find name (CTYPE) of %dth axis... aborting!", i+1 );
         return( 0 );
      }
      axistype[i] = axtype_c( Ctype,
                              Dummy1,                        /* Natural units */
                              Dummy2,
                              &skysys,
                              &prosys,
                              &velsys );
      if (i == 0)
         strcpy( orientation, "HORIZONTAL" );
      else
         strcpy( orientation, "VERTICAL  " );

      anyoutf( dev,
               "%-15.15s : %s axis from %d to %d",
               "NAME", strtok( Ctype.a, " -" ),
                flo[i], fhi[i] );

      axinfo( axistype[i],
              skysys, prosys, velsys,
              typetxt, skytxt, protxt, veltxt );
      anyoutf( dev,  "%-15.15s : %s", "TYPE", typetxt );
      systems[0] = skysys;
      systems[1] = prosys;
      systems[2] = velsys;

      /*-------*/
      /* CROTA */
      /*-------*/
      crota = 0.0;
      if (axistype[i] == LATITUDE)                   /* spatial axis latitude */
      {
         sprintf( message, "CROTA%d", axnum[i] );
         r = 0;
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crota, &r );
         if (r < 0)
            crota = 0.0;
         anyoutf( dev, "%-15.15s : %g deg.",
                        "MAP-ROTATION", crota );
      }

      if (strlen(skytxt) > 0)
         anyoutf( dev, "%-15.15s : %s", "SKY", skytxt );

      if (strlen(protxt) > 0)
         anyoutf( dev, "%-15.15s : %s", "PROJECTION", protxt );

      if (strlen(veltxt) > 0)
         anyoutf( dev, "%-15.15s : %s", "VELOCITY", veltxt );

      /*-------*/
      /* CRVAL */
      /*-------*/
      sprintf( message, "CRVAL%d", axnum[i] );
      r = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crval, &r );
      if (r < 0)
      {
         anyoutf( 1, "Cannot find physical reference value (CRVAL) of %dth axis... aborting!", i+1 );
         return( 0 );
      }

      /*-------*/
      /* CUNIT */
      /*-------*/
      sprintf( message, "CUNIT%d", axnum[i] );
      r = 0;
      gdsd_rchar_c( Setin, tofchar(message), &setlevel, Cunit, &r );
      if (r < 0)
      {
         anyoutf( 1, "Cannot find units (CUNIT) of %dth axis... aborting!", i+1 );
         return( 0 );
      }

      anyoutf( dev, "%-15.15s : %f (%.*s)",
              "GRID 0", crval, nelc_c(Cunit), Cunit.a );


      /*-------*/
      /* CRPIX */
      /*-------*/
      r = 0;
      crpix = gdsc_origin_c( Setin, &axnum[i], &r );
      if (r < 0)
      {
         anyoutf( 1, "Cannot find index of ref. pixel (CRPIX) of %dth axis... aborting!",
                  i+1 );
         return( 0 );
      }

      /*---------------*/
      /* DRVAL & DUNIT */
      /*---------------*/
      sprintf( message, "DRVAL%d", axnum[i] );
      r = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &drval, &r );
      if (r >= 0)
      {
         (void) sprintf( message, "DUNIT%d", axnum[i] );
         r = 0;
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, Dunit, &r );
         if (r < 0)
         {
            anyoutf( 1, "Cannot find sec. units (DUNIT) of %dth axis... aborting!",
                     i+1 );
            return( 0 );
         }
         anyoutf( dev, "%-15.15s : %f (%.*s)",
                 "(second axis)", drval, nelc_c(Dunit), Dunit.a );
      }

      /*-------*/
      /* CDELT */
      /*-------*/
      sprintf( message, "CDELT%d", axnum[i] );
      r = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &cdelt, &r );
      if (r < 0)
      {
         anyoutf( 1, "Cannot find grid separartion (CDELT) of %dth axis... aborting!",
                  i+1 );
         return( 0 );
      }
      anyoutf( dev, "%-15.15s : %f (%.*s)",
              "GRID SPACING", cdelt, nelc_c(Cunit), Cunit.a );

      /*-------*/
      /* DDELT */
      /*-------*/
      sprintf( message, "DDELT%d", axnum[i] );
      r = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &ddelt, &r );
      if (r >= 0)
      {
         anyoutf( dev, "%-15.15s : %f (%.*s)",
                  "(second axis)", ddelt, nelc_c(Dunit), Dunit.a );
      }

      /*---------------*/
      /* COTRANS LEVEL */
      /*---------------*/
      r = axcoord_c( Setin, &axnum[i], Dummy1, Dummy2, &colev );
      if (r == 0)
      {
         char  trans[20];
         if (colev == 1)
            strcpy( trans, "primary" );
         else
            strcpy( trans, "secondary" );
         anyoutf( dev, "%-15.15s : Transformations to physical coordinates for %s axis",
                 "TRANSFORMATION", trans );
      }
      else
      {
         anyoutf( dev, "%-15.15s : No transformation to physical coordinates possible!",
                 "TRANSFORMATION");
      }
      anyoutf( dev, " " );
   }

   anyoutf( dev, "Number of selected subsets: %d", nsubs );
   return( 1 );
}



static void getnewaxisname( fint   axistype,
                            fint   skysys,
                            fint   prosys,
                            fchar  Ctype )
/*-------------------------------------------------------------*/
/* PURPOSE: */
/*-------------------------------------------------------------*/
{
   char  axstr[FITSLEN+1];
   

   strcpy( axstr, "" );
   if (axistype == LONGITUDE)
   {      
      switch (skysys) 
      {
         case EQUATORIAL:     strcpy( axstr, "RA--" );
         break;
         case GALACTIC:       strcpy( axstr, "GLON" );
         break;
         case ECLIPTIC:       strcpy( axstr, "ELON" );
         break;
         case SUPERGALACTIC:  strcpy( axstr, "SLON" );
         break;     
         case EQUATORIAL2000: strcpy( axstr, "RA--" );
         break;              
      }
   } 
   else
   {
      switch (skysys) 
      {
         case EQUATORIAL:     strcpy( axstr, "DEC-" );
         break;
         case GALACTIC:       strcpy( axstr, "GLAT" );
         break;
         case ECLIPTIC:       strcpy( axstr, "ELAT" );
         break;
         case SUPERGALACTIC:  strcpy( axstr, "SLAT" );
         break;     
         case EQUATORIAL2000: strcpy( axstr, "DEC-" );
         break;              
      }      
   }
   
   switch (prosys)
   {
      case 1  : strcat( axstr, "-AIT" );
      break;
      case 2  : strcat( axstr, "-CYL" );
      break;
      case 3  : strcat( axstr, "-FLT" );
      break;
      case 4  : strcat( axstr, "-TAN" );
      break;
      case 5  : strcat( axstr, "-SIN" );
      break;
      case 6  : strcat( axstr, "-ARC" );
      break;
      case 7  : strcat( axstr, "-GLS" );
      break;
      case 8  : strcat( axstr, "-NCP" );
      break;
      case 9  : strcat( axstr, "-STG" );
      break; 
      case 10 : strcat( axstr, "-MER" );
      break;      
   }  
   fcopy( Ctype, axstr );
}



static void userinput( double   crvalXIN,
                       double   crvalYIN,
                       double   cdeltXIN,
                       double   cdeltYIN,
                       double   crotaIN,
                       fint     skysysIN,
                       fint     prosysIN,
                       double   epochIN,
                       double   factorX,
                       double   factorY,
                       fchar    CunitX,
                       fchar    CunitY,
                       fchar    CtypeX,
                       fchar    CtypeY,
                       fchar    CtypeXOUT,
                       fchar    CtypeYOUT,
                       double   *crvalXOUT,
                       double   *crvalYOUT,
                       double   *cdeltXOUT,
                       double   *cdeltYOUT,
                       double   *crotaOUT,
                       fint     *skysysOUT,
                       fint     *prosysOUT,
                       double   *epochOUT,
                       fint     *axistype,
                       fchar    Setin,
                       fint     *subin,
                       bool     rotateonly )
/*-------------------------------------------------------------*/
/* PURPOSE: Get transformation parameters from user using      */
/* input as defaults.                                          */
/*-------------------------------------------------------------*/
{
   bool      template = NO;
   char      axtype[15];
   char      hmsdmsstr[20];
   char      message[80];
   fint      r;
   fint      dfault;
   fint      nitems;
   int       outdev;
   double    crval[2];
   double    cdelt[2];


   if (!template)
   {
      dfault = REQUEST;
      outdev = 8;
   }
   else
   {
      dfault = HIDDEN;
      outdev = 16;
   }

   /*------------------------------*/
   /* PROJECTION CENTRE            */
   /*------------------------------*/
   if (axistype[0] == LONGITUDE)
   {
      strcpy( axtype, "LONGITUDE" );
      if (skysysIN == EQUATORIAL)
         hms( (crvalXIN*factorX/3600.0), hmsdmsstr, 2 );
      else
         dms( (crvalXIN*factorX/3600.0), hmsdmsstr, 1 );
   }
   else
   {
      strcpy( axtype, "LATITUDE" );
      dms( (crvalXIN*factorX/3600.0), hmsdmsstr, 1 );
   }
   anyoutf( outdev, "Projection centre %.*s (%s) axis: %f (%.*s) = %s",
            nelc_c(CtypeX), CtypeX.a,
            axtype,
            crvalXIN,
            nelc_c(CunitX), CunitX.a,
            hmsdmsstr );
   if (axistype[1] == LONGITUDE)
   {
      strcpy( axtype, "LONGITUDE" );
      if (skysysIN == EQUATORIAL)
         hms( (crvalYIN*factorY/3600.0), hmsdmsstr, 2 );
      else
         dms( (crvalYIN*factorY/3600.0), hmsdmsstr, 1 );
   }
   else
   {
      strcpy( axtype, "LATITUDE" );
      dms( (crvalYIN*factorY/3600.0), hmsdmsstr, 1 );
   }
   anyoutf( outdev, "Projection centre %.*s (%s) axis: %f (%.*s) = %s",
            nelc_c(CtypeY), CtypeY.a,
            axtype,
            crvalYIN,
            nelc_c(CunitY), CunitY.a,
            hmsdmsstr );

   nitems = 1;
   r = gdspos_c( crval,
                 &nitems,
                 &dfault,
                 tofchar("OUTPOS="),
                 tofchar("Give proj. centre in output:    [copy from input map]"),
                 Setin,
                 &subin[0] );
   if (r == 1)
   {
      /* gdspos returns grids --> convert to physical coordinates */
      r = grtoph_c( Setin, &subin[0], crval, crval );
      *crvalXOUT = crval[0];
      *crvalYOUT = crval[1];
   }
   else
   {
      /* User wants copy of input projection centre */
      *crvalXOUT = crvalXIN;
      *crvalYOUT = crvalYIN;
   }
   /*------------------------------*/
   /* GRID SPACINGS                */
   /*------------------------------*/
   cdelt[0] = cdeltXIN * factorX;
   cdelt[1] = cdeltYIN * factorY;
   nitems   = 2;
   sprintf( message, "New grid spacings (x,y) in ARCSEC:  [%g %g]",
            cdelt[0], cdelt[1] );
   r = userdble_c( cdelt, &nitems, &dfault, tofchar("CDELT="),
                   tofchar(message) );
   *cdeltXOUT = cdelt[0] / factorX;                   /* Back to header units */
   *cdeltYOUT = cdelt[1] / factorY;

   if (skysysIN == EQUATORIAL)
   {
      if ((axistype[0] == LONGITUDE && *cdeltXOUT > 0.0) ||
          (axistype[1] == LONGITUDE && *cdeltYOUT > 0.0) )
      {
         bool   change = toflog( YES );
         anyoutf( 1, "%s: You entered a POSITIVE value for the grid spacing in longitude",
                  taskname );
         nitems = 1;
         dfault = REQUEST;
         r = userlog_c( &change, &nitems, &dfault, tofchar("CHANGE="),
                        tofchar("Do you want to change sign of cdelt?    [Y]/N") );
         change = tobool( change );
         if (change)
         {
            if (axistype[0] == LONGITUDE)
               (*cdeltXOUT) *= -1.0;
            else
               (*cdeltYOUT) *= -1.0;
         }
      }
   }

   *skysysOUT = skysysIN;
   if (!dblank_c(&epochIN))
      *epochOUT  = epochIN;
   else
      *epochOUT  = 1950.0;
   *prosysOUT = prosysIN;
   if (!rotateonly)
   {
      /*------------------------------*/
      /* SKY SYSTEM                   */
      /*------------------------------*/
      anyoutf( outdev, " ================= SKY SYSTEMS  ===================" );
      anyoutf( outdev, " 1:  RA/DEC     equatorial (with epoch != 2000.0)" );
      anyoutf( outdev, " 2:  GLON/GLAT  galactic" );
      anyoutf( outdev, " 3:  ELON/ELAT  ecliptic (epoch 1950.0)" );
      anyoutf( outdev, " 4:  SLON/SLAT  supergalactic" );
      anyoutf( outdev, " 5:  RA/DEC     equatorial (epoch 2000.0)" );
      anyoutf( outdev, " Current sky system: [%d].", skysysIN );
      anyoutf( outdev, " " );

      nitems = 1;
      r = userint_c( skysysOUT, &nitems, &dfault, tofchar("SKYSYS="),
                     tofchar("Output sky system:    [copy from input map]") );

      if (*skysysOUT == EQUATORIAL || *skysysOUT == EQUATORIAL2000 ||
          *skysysOUT == ECLIPTIC )
      {
         if (dblank_c(&epochIN))
            sprintf( message, "Give epoch of new sky system:   [%g]", *epochOUT );
         else
            sprintf( message, "Give epoch of new sky system:   [old: %g]", *epochOUT );
         r = userdble_c( epochOUT, &nitems, &dfault, tofchar("EPOCH="),
                         tofchar(message) );
         if (*skysysOUT == EQUATORIAL && *epochOUT == 2000.0)
            *skysysOUT = EQUATORIAL2000;
      }

      /*------------------------------*/
      /* PROJECTION SYSTEM            */
      /*------------------------------*/
      anyoutf( outdev, " ================= PROJECTIONS ===================" );
      anyoutf( outdev, "  1:  AIT      Aitoff Equal Area projection" );
      anyoutf( outdev, "  2:  CYL      Equivalent Cylindrical projection" );
      anyoutf( outdev, "  3:  FLT      flat projection" );
      anyoutf( outdev, "  4:  TAN      Gnomonic projection" );
      anyoutf( outdev, "  5:  SIN      Orthographic projection" );
      anyoutf( outdev, "  6:  ARC      Rectangular projection" );
      anyoutf( outdev, "  7:  GLS      Transversal projection" );
      anyoutf( outdev, "  8:  NCP      North Celestial Pole projection " );
      anyoutf( outdev, "  9:  STG      Stereographic projection" );
      anyoutf( outdev, " 10:  MER      Mercator projection" );
      anyoutf( outdev, " Current projection system: [%d].", prosysIN );
      anyoutf( outdev, " " );

      nitems = 1;
      r = userint_c( prosysOUT, &nitems, &dfault, tofchar("PROSYS="),
                     tofchar("Output projection system:    [copy from input map]") );
   }

   getnewaxisname( axistype[0], *skysysOUT, *prosysOUT, CtypeXOUT );
   getnewaxisname( axistype[1], *skysysOUT, *prosysOUT, CtypeYOUT );   

   if (*skysysOUT != skysysIN)
   {
      /* The projection center is in world coordinates theat belong */
      /* to the sky system of the input map. If the sky system changes */
      /* then one needs to transform the CRVAL's to the new system */
      /* otherwise one gets a completely other projection center. */
      skyco_c( crvalXOUT, crvalYOUT, &skysysIN, crvalXOUT, crvalYOUT, skysysOUT );
   }

   /*------------------------------*/
   /* ROTATION ANGLE               */
   /*------------------------------*/
   dfault = REQUEST;
   nitems = 1;
   if (rotateonly)
   {
      /* Ask angle as an angle over which to rotate */
      *crotaOUT = 0.0;
      r = userdble_c( crotaOUT, &nitems, &dfault, tofchar("ROTANGLE="),
                      tofchar("Rotate map over .... degrees:            [0.0]") );
      *crotaOUT += crotaIN;
   }
   else
   {
      /* Ask angle using CROTA definition */
      *crotaOUT = crotaIN;
      anyoutf( outdev, " Current angle between the +y axis and the +m (latitude) axis is %g degrees.",
               crotaIN );
      anyoutf( outdev, " The angle is counted counter-clockwise if the grid separation (CDELT) in" );
      anyoutf( outdev, " longitude direction is < 0.0." );
      anyoutf( outdev, " " );
      r = userdble_c( crotaOUT, &nitems, &dfault, tofchar("CROTA="),
                      tofchar("New value for map rotation (deg.):   [from input map]") );
   }
}



static void transform( double xin,
                       double yin,
                       fint   skysysI,
                       fint   prosysI,
                       double centrelonI,    /* old projection centre in deg. */
                       double centrelatI,
                       double cdeltlonI,       /* old grid spacing in degrees */
                       double cdeltlatI,
                       double crpixlonI,
                       double crpixlatI,
                       double crotaI,
                       double epochI,
                       double *xout,
                       double *yout,
                       fint   skysysO,
                       fint   prosysO,
                       double centrelonO,
                       double centrelatO,
                       double cdeltlonO,
                       double cdeltlatO,
                       double crotaO,
                       double epochO )
/*-------------------------------------------------------------*/
/* PURPOSE: Transform a coordinate pair in longitude, latitude */
/* (in grids) to a coordinate pair (long, lat) in another      */
/* system. This system can be characterized by different sky   */
/* system, projection system, projection centre, grid spacing, */
/* (both in degrees) rotation angle (degrees) or epoch.        */
/*-------------------------------------------------------------*/
{
   fint   degtogrid = 0;
   fint   gridtodeg = 3;
   double xdeg, ydeg;
   double offX, offY;

   /*--------------------------------------------------*/
   /* Note that the grids entered here are grids that  */
   /* have an integer offset with respect to the       */
   /* 1-based pixel system. For world coordinates we   */
   /* must correct a grid position if the value for    */
   /* CRPIX is not integer. In cotrans.c this          */
   /* correction is done with macro's GETGRID and      */
   /* SETGRID, i.e. for an incoming grid we need to    */
   /* subtract the offset and for an outgoing grid we  */
   /* have to add it. Assume a CRPIX of 3.3 then the   */
   /* offset is 3.4 - floor(3.4+0.5) = 0.4 and assume  */
   /* we have a grid position 0.4, then the grid       */
   /* distance with respect to the projection center   */
   /* is 0.4-0.4 = 0 which meant that our position 0.4 */
   /* was in fact the projection center.               */
   /* The check was made with a 5x5 test image with    */
   /* CRPIX1 = 3.4999 and CRPIX2 = 3.4999 and a        */
   /* rotation with this program of +90 deg. The       */
   /* rotated image should have shifted one pixel to   */
   /* the right (rotate a square on paper around       */
   /* CRPIX1, CRPIX2 to verify this shift). REPROJ     */
   /* was tested with such a test image and the result */
   /* was as expected. Note that this was NOT the case */
   /* with REPROJ versions before 12-04-2009.          */
   /*                                                  */
   /*                                                  */
   /* Note that the output set is made with gdsout and */
   /* the box was modified with gdscss. These routines */
   /* do not alter the fractional part of the new      */
   /* CRPIX. Therefore the offsets in the input set    */
   /* are the same as those in the output set. This    */
   /* explains why there is only one pair of CRPIX     */
   /* values in the argument list of this function.    */
   /*--------------------------------------------------*/
   offX = crpixlonI - floor(crpixlonI+0.5);
   offY = crpixlatI - floor(crpixlatI+0.5);
   xin -= offX;
   yin -= offY;
   /*--------------------------------------------------*/
   /* A long, lat position in degrees is the only      */
   /* parameter that remains constant in a projection  */
   /* transformation, unless there is an epoch or      */
   /* sky system transformation.                       */
   /*--------------------------------------------------*/
   proco_c( &xin, &yin, &xdeg, &ydeg, &centrelonI, &centrelatI,
            &cdeltlonI, &cdeltlatI, &crotaI, &prosysI, &gridtodeg );

   
   /*--------------------------------------------------*/
   /* Now the coordinates are in degrees wrt. the      */
   /* input map. These coordinates can be transformed  */
   /* into another sky system or to another epoch.     */
   /*--------------------------------------------------*/
   if (skysysI == skysysO)         /* Perhaps only an epoch transf. is wanted */
   {
      if (epochI != epochO)
      {
         if (skysysI == EQUATORIAL)
            epoco_c( &xdeg, &ydeg, &epochI, &xdeg, &ydeg, &epochO );
         else if (skysysI == ECLIPTIC)
            eclipco_c( &xdeg, &ydeg, &epochI, &xdeg, &ydeg, &epochO );
      }
   }
   else
   {
      double   epdummy = 1950.0;
      /* A different sky system is wanted */
      if (skysysI == EQUATORIAL && epochI != 1950.0)
         epoco_c( &xdeg, &ydeg, &epochI, &xdeg, &ydeg, &epdummy );
      else if (skysysI == ECLIPTIC && epochI != 1950.0)
         eclipco_c( &xdeg, &ydeg, &epochI, &xdeg, &ydeg, &epdummy );

      /* Convert from original sky system to new skysys. */
      skyco_c( &xdeg, &ydeg, &skysysI, &xdeg, &ydeg, &skysysO );

      /*--------------------------------------------------*/
      /* Output for 'EQUATORIAL' and 'ECLIPTIC' is always */
      /* in 1950.0 coordinates. Perhaps a (second) epoch  */
      /* transformation is needed.                        */
      /*--------------------------------------------------*/
      if (skysysO == EQUATORIAL && epochO != 1950.0)
         epoco_c( &xdeg, &ydeg, &epdummy, &xdeg, &ydeg, &epochO );
      else if (skysysO == ECLIPTIC && epochO != 1950.0)
         eclipco_c( &xdeg, &ydeg, &epdummy, &xdeg, &ydeg, &epochO );
   }

   /*--------------------------------------------------*/
   /* Final stage: Convert (modified) coordinate pair  */
   /* in degrees into grids using the parameters of    */
   /* the output system.                               */
   /*--------------------------------------------------*/
   proco_c( &xdeg, &ydeg, xout, yout, &centrelonO, &centrelatO,
            &cdeltlonO, &cdeltlatO, &crotaO, &prosysO, &degtogrid );
   *xout += offX;
   *yout += offY;
}



static void  getnewbox( fint     *boxLO,
                        fint     *boxHI,
                        fint     *boxLOO,
                        fint     *boxHIO,
                        fint     *axistype,
                        fint     skysysIN,
                        fint     prosysIN,
                        double   crvalXIN,
                        double   crvalYIN,
                        double   cdeltXIN,
                        double   cdeltYIN,     /* old grid spacing in degrees */
                        double   crpixXIN,
                        double   crpixYIN,
                        double   crotaIN,
                        double   epochIN,
                        fint     skysysOUT,
                        fint     prosysOUT,
                        double   crvalXOUT,
                        double   crvalYOUT,
                        double   cdeltXOUT,
                        double   cdeltYOUT,
                        double   crotaOUT,
                        double   epochOUT )
/*-------------------------------------------------------------*/
/* PURPOSE: Get the default box in the new system and prompt   */
/* user to give a box.                                         */
/*-------------------------------------------------------------*/
{
   int      i;
   int      i1 = 0;
   int      i2 = 1;
   fint     corners[4];
   fint     nitems, dfault;
   fint     r;
   bool     swap = (axistype[0] == LATITUDE);
   char     message[80];
   double   boxXI[4];
   double   boxYI[4];
   double   boxXO[4];
   double   boxYO[4];
   double   xmin = 0.0;
   double   xmax = 0.0;
   double   ymin = 0.0;
   double   ymax = 0.0;


   if (swap)
   {
      DSWAP( crvalXIN, crvalYIN );
      DSWAP( cdeltXIN, cdeltYIN );
      DSWAP( crpixXIN, crpixYIN );      
      DSWAP( crvalXOUT, crvalYOUT );
      DSWAP( cdeltXOUT, cdeltYOUT );
      /* swap input box x, y also */
      ISWAP( i1, i2 );
   }
   boxXI[0] = boxLO[i1];
   boxXI[1] = boxHI[i1];
   boxXI[2] = boxLO[i1];
   boxXI[3] = boxHI[i1];

   boxYI[0] = boxLO[i2];
   boxYI[1] = boxLO[i2];
   boxYI[2] = boxHI[i2];
   boxYI[3] = boxHI[i2];


   for (i = 0; i < 4; i++)
   {
      transform( boxXI[i],  boxYI[i],
                 skysysIN,
                 prosysIN,
                 crvalXIN,  crvalYIN,
                 cdeltXIN,  cdeltYIN,
                 crpixXIN, crpixYIN,
                 crotaIN,
                 epochIN,
                 &boxXO[i], &boxYO[i],
                 skysysOUT,
                 prosysOUT,
                 crvalXOUT, crvalYOUT,
                 cdeltXOUT, cdeltYOUT,
                 crotaOUT,
                 epochOUT );
      if (swap)
         DSWAP( boxXO[i], boxYO[i] );

      /* Keep track of min/max coordinates of output map */
      if (i == 0)
      {
         xmax = xmin = boxXO[i];
         ymax = ymin = boxYO[i];
      }
      else
      {
         xmin = MYMIN( xmin, boxXO[i] );
         ymin = MYMIN( ymin, boxYO[i] );
         xmax = MYMAX( xmax, boxXO[i] );
         ymax = MYMAX( ymax, boxYO[i] );
      }
   }

   /* Ask user size of output. Use calculated corners as default. */


   anyoutf( 16, "Min. output box in floats: xmin, ymin, xmax, ymax %g %g %g %g",
            xmin, ymin, xmax, ymax );

   corners[0] = NINT( xmin );
   corners[1] = NINT( ymin );
   corners[2] = NINT( xmax );
   corners[3] = NINT( ymax );

   sprintf( message, "New box (in grids): [%d %d  %d %d]",
            corners[0], corners[1], corners[2], corners[3] );
   nitems = 4;
   dfault = REQUEST;
   r = userint_c( corners, &nitems, &dfault, tofchar("OUTBOX="),
                  tofchar(message) );
   boxLOO[0] = corners[0];
   boxLOO[1] = corners[1];
   boxHIO[0] = corners[2];
   boxHIO[1] = corners[3];
}



float **fmatrix( fint *blo,
                 fint *bhi )
/*-------------------------------------------------------------*/
/* PURPOSE: Create a 2-dim matrix M[y][x] with given size.     */
/* The size is determined by the input box. The values in 'blo'*/
/* set the start indices. The first element of M will be       */
/* M[blo[1]][blo[0]]. Note the order y, x!                     */
/*-------------------------------------------------------------*/
{
   float   **M = NULL;
   int     rows = bhi[1] - blo[1] + 1;
   int     cols = bhi[0] - blo[0] + 1;
   int     i;
   int     xmin = blo[0];
   int     ymin = blo[1];
   int     ymax = bhi[1];


   /* Allocate memory for pointers to rows */
   M = (float **) calloc( rows, sizeof(float *) );
   if (!M)
      return( NULL );
   M -= ymin;                                            /* adjust index in y */

   /* Pointer to first row allocates memory for entire box */
   M[ymin] = (float *) calloc( rows * cols, sizeof(float) );
   if (!M[ymin])
      return( NULL );
   M[ymin] -= xmin;                                      /* adjust index in x */

   /* Set pointers to rows */
   for (i = ymin + 1; i <= ymax ; i++)
      M[i] = M[i-1] + cols;                         /* increase pointer value */

   /* Return pointer to array of pointers to rows */
   return( M );
}



static void freematrix( float **M,
                        fint *blo,
                        fint *bhi )
/*-------------------------------------------------------------*/
/* PURPOSE: Free space allocated for matrix with 'fmatrix'     */
/* Restore pointer offsets before freeing allocated space.     */
/*-------------------------------------------------------------*/
{
   M[blo[1]] += blo[0];
   free( M[blo[1]] );
   M += blo[1];
   free( M );
}



static int getdatafromdisk( fchar Setin,
                            fint  subset,
                            float **M,
                            fint  *blo,
                            fint  *bhi )
/*-------------------------------------------------------------*/
/* PURPOSE: Read data between blo, bhi in M.                   */
/*-------------------------------------------------------------*/
{
   fint   cwlo, cwhi;
   fint   tid = 0;
   fint   pixelsread;
   fint   totpixels = (bhi[0]-blo[0]+1) * (bhi[1]-blo[1]+1);


   cwlo   = gdsc_fill_c( Setin, &subset, blo );
   cwhi   = gdsc_fill_c( Setin, &subset, bhi );
   anyoutf( 16, "cwlo=%d cwhi=%d xlo=%d ylo=%d xhi=%d yhi=%d", cwlo,cwhi,
            blo[0], blo[1], bhi[0], bhi[1]);
   gdsi_read_c( Setin,
                &cwlo, &cwhi,
                &M[blo[1]][blo[0]],
                &totpixels,
                &pixelsread,
                &tid );
   if (tid != 0 || pixelsread != totpixels)
      return( 0 );

   return( 1 );
}



static void tiderror( fint tid )
/*-------------------------------------------------------------*/
/* PURPOSE: Display read/write transfer-id error message.      */
/*-------------------------------------------------------------*/
{
   if (tid == -30)
      anyoutf( 1, "Not all pixels written to disk!" );
   else if (tid == -31)
      anyoutf( 1, "Illegal transfer identifier!" );
   else if (tid == -32)
      anyoutf( 1, "Unable to allocate enough memory!" );
   else if (tid == -36)
      anyoutf( 1, "Cannot open data file!" );
   else if (tid == -38)
      anyoutf( 1, "Maximum open sets exceeded!" );
   else
   {
      fchar   Errstr;
      fmake( Errstr, 80 );
      gds_errstr_c( Errstr, &tid );
      anyoutf( 1, "GDS error: %.*s", nelc_c(Errstr), Errstr.a );
   }
}




static void getposition( double  dx,                /* Fractions in rectangle */
                         double  dy,
                         double  *cx,                /* Corner positions in x */
                         double  *cy,
                         double  *x,
                         double  *y )
/*-------------------------------------------------------------*/
/* PURPOSE: Given 4 corner positions, and two fractions,       */
/*          interpolate a coordinate pair that corresponds to  */
/*          these fractions.                                   */
/* The input are four corner coordinate pairs. Lets number     */
/* them as:                                                    */
/*                                                             */
/*                    (x2,y2)                                  */
/*       cx3,cy3---------*----cx2,cy2                          */
/*         |                     |                             */
/*         |                   |                               */
/* (x3,y3) *           s      * (x1,y1)                        */
/*     dy  |                |                                  */
/*       cx0,cy0------*--cx1,cy1                               */
/*                 (x0,y0)                                     */
/*               dx                                            */
/*                                                             */
/* The shape does not need to be a square.                     */
/* The position of the asterisks are start and end points of   */
/* two lines that intersect at 's'. This routine will calculate*/
/* first the positions of the asterisks using the values of    */
/* the fractions dx and dy. Then the position of 's' is calcu- */
/* lated and returned to the calling environment.              */
/*-------------------------------------------------------------*/
{
   double  x0, x1, x2, x3;
   double  y0, y1, y2, y3;
   double  mu, denom;


   if (dx == 0.0 && dy == 0.0)
   {
      *x = cx[0];
      *y = cy[0];
      return;
   }
   x0 = cx[0] + dx * (cx[1]-cx[0]);
   y0 = cy[0] + dx * (cy[1]-cy[0]);

   x1 = cx[1] + dy * (cx[2]-cx[1]);
   y1 = cy[1] + dy * (cy[2]-cy[1]);

   x2 = cx[3] + dx * (cx[2]-cx[3]);
   y2 = cy[3] + dx * (cy[2]-cy[3]);

   x3 = cx[0] + dy * (cx[3]-cx[0]);
   y3 = cy[0] + dy * (cy[3]-cy[0]);

   if (cx[0] == cx[1] && cx[2] == cx[3])
   {
      *x = cx[0];
      *y = cy[0] + dy * (cy[3]-cy[0]);
      return;
   }
   denom = -(x2-x0)*(y3-y1)+(x3-x1)*(y2-y0);
   if (denom == 0.0)
   {
      *x = x0;
      *y = y0;
   }
   else
   {
      mu = ( (x2-x0)*(y1-y0)-(x1-x0)*(y2-y0) ) / denom;
      *x = x1 + mu * (x3-x1);
      *y = y1 + mu * (y3-y1);
   }
}




static int filloutput( fchar   Setout,
                       fint    subout,
                       fint    subnr,
                       fint    nsubs,
                       fint    *boxLOO,
                       fint    *boxHIO,
                       float   **M,
                       fint    *boxLO,
                       fint    *boxHI,
                       float   **buff,
                       fint    *pospol,
                       double  *crval,
                       double  *cdelt,
                       double  *crpix,                       
                       double  *crota,
                       fint    *prosys,
                       fint    *skysys,
                       double  *epoch,
                       bool    swap,
                       int     interpolate,
                       bool    dataip,
                       float   *minmax,
                       fint    *nblanks )
/*-------------------------------------------------------------*/
/* PURPOSE: For each output subset, calculate for each         */
/* position the physical coordinates wrt the output. Use these */
/* values to calculate grids wrt. the input. These coordinates */
/* correspond to an image value. This value must be placed in  */
/* the corresponding output buffer.                            */
/* Note that crval and cdelt are in long, lat order which is   */
/* not necessary the same as the x, y order (variable 'swap'). */
/*-------------------------------------------------------------*/
{
   fint      tid = 0;
   fint      blo[2], bhi[2];
   fint      cwlo, cwhi;
   fint      mc = 0;
   fint      maxpix, curpix = 0.0;
   int       xlen, ylen;
   int       line;
   int       step;


   if (interpolate)
      step = pospol[1];
   else
      step = MAXBUFLINES;
   xlen     = boxHIO[0] - boxLOO[0] + 1;
   ylen     = boxHIO[1] - boxLOO[1] + 1;
   blo[0]   = boxLOO[0];
   bhi[0]   = boxHIO[0];
   maxpix   = xlen * ylen;

   /* Start loop over all lines in the output box. */
   for (line = boxLOO[1]; line <= boxHIO[1]; line += step)
   {
      fint    done = 0;
      fint    bufsize;
      fint    bx, by;
      fint    bufylen;

      blo[1]  = line;
      bhi[1]  = MYMIN( line+step-1, boxHIO[1] );    /* 'step' lines at a time */
      cwlo    = gdsc_fill_c( Setout, &subout, blo );
      cwhi    = gdsc_fill_c( Setout, &subout, bhi );
      tid     = 0;
      bufylen = bhi[1] - blo[1] + 1;
      bufsize = xlen * bufylen;
      /*--------------------------------------------------*/
      /* Display a message that indicates the progress in */
      /* percents. Display at each percent.               */
      /*--------------------------------------------------*/
      {
         static float    oldfraction = 0.0;       /* Static is essential here */
         float           fraction;                /* A number between 0 and 1 */
         char            message[80];

         curpix  += bufsize;
         fraction = (float) (curpix * (subnr+1)) / (float) (maxpix * nsubs);
         if (fraction > oldfraction + 0.01)
         {
            /* Display progress if progress > 1% */
            sprintf( message, "done: %d %%", (int) (fraction*100.0) );
            status_c( tofchar(message) );
            oldfraction = fraction;
         }
      }

      if (!interpolate)
      /*--------------------------------------------------*/
      /* No interpolation of positions. Transform each    */
      /* grid of the output box to a physical position.   */
      /* Calculate the corresponding grid in the input    */
      /* map. Return a bilinear interpolated image value  */
      /* found at that position.                          */
      /*--------------------------------------------------*/
      {
         /* Start loops over the output buffer */
         for (by = 0; by < bufylen; by++)
         {
            for (bx = 0; bx < xlen; bx++)
            {
               double    x = (double) (boxLOO[0] + bx);
               double    y = (double) (line + by);
               
               if (swap)
                  DSWAP(x, y);
               /* TRANSFORM FROM OUTPUT GRIDS TO INPUT GRIDS */
               transform( x, y,
                          skysys[1],
                          prosys[1],
                          crval[2], crval[3],
                          cdelt[2], cdelt[3],
                          crpix[0], crpix[1],
                          crota[1],
                          epoch[1],
                          &x, &y,
                          skysys[0],
                          prosys[0],
                          crval[0], crval[1],
                          cdelt[0], cdelt[1],
                          crota[0],
                          epoch[0] );
               if (swap)
                  DSWAP(x, y);
               /*--------------------------------------------------*/
               /* Get the (2x2) image value interpolated) data     */
               /* value from the input map.                        */
               /*--------------------------------------------------*/
               if (dataip)
               {
                  buff[by][bx] = interpol( x, y, M, boxLO, boxHI, blank );
               }
               else
               {
               	  x = NINT(x); y = NINT(y);
                  if (x >= boxLO[0] && x <= boxHI[0] &&
                      y >= boxLO[1] && y <= boxHI[1] )
                  {
                     /* buff[by][bx] = M[NINT(x)][NINT(y)]; */
                     buff[by][bx] = M[(int)y][(int)x];
                  }
                  else
                  {                            	
                     buff[by][bx] = blank;
                  }
               }
            }
         }
      }
      else
      /*--------------------------------------------------*/
      /* Interpolate missing positions. For each pospol[0]*/
      /* x pospol[1] matrix, calculate the corner posi-   */
      /* tions in the input map. Get the (data interpola- */
      /* ted) image values at those corners and fill the  */
      /* missing positions by interpolation.              */
      /*--------------------------------------------------*/
      {
         int      c;
         int      xm, ym;
         int      mleny = bufylen;
         double   x, y;
         double   dx, dy;


         /* Start loop in x over the output buffer */

         for (bx = 0; bx < xlen;)
         {
            double    x0 = (double) (boxLOO[0] + bx);
            double    y0 = (double) (line);
            double    xi[4], yi[4];
            double    xo[4], yo[4];
            int       mlenx = (double) MYMIN( xlen-bx, pospol[0] );


            /* Always from output position to input position */
            xo[0] = x0;                     yo[0] = y0;
            xo[1] = x0 + (double)(mlenx-1); yo[1] = y0;
            xo[2] = xo[1];                  yo[2] = y0 + (double)(mleny-1);
            xo[3] = x0;                     yo[3] = yo[2];
            for (c = 0; c < 4; c++)
            {
               x = xo[c];    y = yo[c];
               if (swap)
                  DSWAP(x, y);
               /* TRANSFORM FROM OUTPUT GRIDS TO INPUT GRIDS */
               transform( x, y,
                          skysys[1],
                          prosys[1],
                          crval[2], crval[3],
                          cdelt[2], cdelt[3],
                          crpix[0], crpix[1],
                          crota[1],
                          epoch[1],
                          &xi[c], &yi[c],
                          skysys[0],
                          prosys[0],
                          crval[0], crval[1],
                          cdelt[0], cdelt[1],
                          crota[0],
                          epoch[0] );
               if (swap)
                  DSWAP(xi[c], yi[c]);
            }
            /*--------------------------------------------------*/
            /* Get the (2x2) image value interpolated) data     */
            /* value from the input map.                        */
            /*--------------------------------------------------*/
            for (ym = 0; ym < mleny; ym++)
            {
               if (mleny == 1)
                  dy = 0.0;
               else
                  dy = (double) ym / ((double)mleny-1.0);
               for (xm = 0; xm < mlenx; xm++)
               {
                  if (mlenx == 1)
                     dx = 0.0;
                  else
                     dx = (double) xm / ((double)mlenx-1.0);

                  getposition( dx, dy, xi, yi, &x, &y );
                  if (dataip)
                     buff[ym][bx+xm] = interpol( x, y, M, boxLO, boxHI, blank);
                  else
                     buff[ym][bx+xm] = M[NINT(x)][NINT(y)];
               }
            }
            bx += mlenx;
         }
      }

      /* Update data min. and max. and number of blanks. */
      minmax3_c( &buff[0][0],
                 &bufsize,
                 &minmax[0], &minmax[1],
                 nblanks,
                 &mc );
      /* Write buffer to output set */
      gdsi_write_c( Setout,
                    &cwlo, &cwhi,
                    &buff[0][0],
                    &bufsize,
                    &done,
                    &tid );
      if (tid < 0)
      {
         tiderror( tid );
         return( NO );
      }
   }
   return( YES );
}



static void defseterror( int  axnr,
                         char *str )
/*-------------------------------------------------------------*/
/* PURPOSE: Display error message for 'defset' function.       */
/*-------------------------------------------------------------*/
{
   anyoutf( 1, "                  !!!!!!!!!!!!!!!!!!!!!" );
   anyoutf( 1, "CANNOT USE YOUR DEFSET= SET BECAUSE:" );
   anyoutf( 1, "%s (axis number %d)", str, axnr + 1 );
   anyoutf( 1, "                  !!!!!!!!!!!!!!!!!!!!!" );
}




static int  defset( fchar  Setdef,
                    fint   *axistypeIN,
                    fchar  CunitX,
                    fchar  CunitY,
                    fchar  CtypeXOUT,
                    fchar  CtypeYOUT,
                    double *crvalXOUT,
                    double *crvalYOUT,
                    double *cdeltXOUT,
                    double *cdeltYOUT,
                    double *crotaOUT,
                    fint   *skysysOUT,
                    fint   *prosysOUT,
                    double *epochOUT )
/*-------------------------------------------------------------*/
/* PURPOSE: Get transformation parameters like projection      */
/* centre, grid spacing, rotation, sky system, epoch and pro-  */
/* jection system from a user given 'Setdef'. Return also a    */
/* box size equal to the box size of the default set.          */
/*-------------------------------------------------------------*/
{
   fint      scrnum;
   fint      dfault;
   fint      nsubs;
   fint      maxsubs = SUBSMAX;
   fint      maxaxes = AXESMAX;
   fint      axnum[AXESMAX];                     /* GDSINP axis numbers array */
   fint      axcount[AXESMAX];                   /* GDSINP axis lengths array */
   fint      class = 1;              /* Repeat operation for each subset axis */
   fint      subdim;
   fint      dev = 8;
   fint      axistype[2];
   fint      r;
   int       i;
   char      message[80];


   dfault = REQUEST;
   subdim = 2;                                  /* Subset dimension must be 2 */
   scrnum = 8;                        /* Do not show data in experienced mode */
   nsubs  = gdsinp_c( Setdef,
                      subdef,
                      &maxsubs,
                      &dfault,
                      KEY_DEFSET,
                      tofchar("Copy system from reference set, subsets:    [manual input]"),
                      &scrnum,
                      axnum,
                      axcount,
                      &maxaxes,
                      &class,
                      &subdim );
   if (nsubs == 0)
      return( 0 );

   /*--------------------------------------------------*/
   /* Get the wanted items from the header. If the     */
   /* header is incomplete return to the calling       */
   /* environment.                                     */
   /*--------------------------------------------------*/
   for (i = 0; i < 2; i++)
   {
      fchar  Ctype;
      fchar  Cunit;
      fchar  Dummy1, Dummy2;
      fint   velsysdummy;
      double crval, cdelt;

      fmake( Ctype, FITSLEN );
      fmake( Cunit, FITSLEN );
      fmake( Dummy1, 20 );
      fmake( Dummy2, 20 );

      /*-------*/
      /* CTYPE */
      /*-------*/
      sprintf( message, "CTYPE%d", axnum[i] );
      r = 0;
      gdsd_rchar_c( Setdef, tofchar(message), &setlevel, Ctype, &r );
      if (r < 0)
      {
         defseterror( i, "Cannot find axis name (CTYPE))" );
         return( 0 );
      }
      Ctype.a[nelc_c(Ctype)] = '\0';
      if (i == 0)
      {
         fcopy( CtypeXOUT, Ctype.a );
      }
      else
      {
         fcopy( CtypeYOUT, Ctype.a );
      }

      axistype[i] = axtype_c( Ctype,
                              Dummy1,                        /* Natural units */
                              Dummy2,
                              skysysOUT,
                              prosysOUT,
                              &velsysdummy );
      if (axistype[i] != axistypeIN[i])
      {
         defseterror( i, "Axis type incompatible with input set!" );
         return( 0 );
      }

      /*-------*/
      /* CUNIT */
      /*-------*/
      sprintf( message, "CUNIT%d", axnum[i] );
      r = 0;
      gdsd_rchar_c( Setdef, tofchar(message), &setlevel, Cunit, &r );
      if (r < 0)
      {
         defseterror( i, "Cannot find axis units (CUNIT)!" );
         return( 0 );
      }
      /*--------------------------------------------------*/
      /* Check whether the axis units are the same.       */
      /*--------------------------------------------------*/
      {
         int   ok;

         if (i == 0)
            ok = strcompare( CunitX, Cunit );
         else
            ok = strcompare( CunitY, Cunit );
         if (!ok)
         {
            defseterror( i, "Units of axis are not the same as input units!" );
            return( 0 );
         }
      }


      /*-------*/
      /* CRVAL */
      /*-------*/
      sprintf( message, "CRVAL%d", axnum[i] );
      r = 0;
      gdsd_rdble_c( Setdef, tofchar(message), &setlevel, &crval, &r );
      if (r < 0)
      {
         defseterror( i, "Cannot find physical reference value (CRVAL)!" );
         return( 0 );
      }
      if (i == 0)
         *crvalXOUT = crval;
      else
         *crvalYOUT = crval;

      /*-------*/
      /* CDELT */
      /*-------*/
      sprintf( message, "CDELT%d", axnum[i] );
      r = 0;
      gdsd_rdble_c( Setdef, tofchar(message), &setlevel, &cdelt, &r );
      if (r < 0)
      {
         defseterror( i, "Cannot find grid separartion (CDELT)!" );
         return( 0 );
      }
      if (i == 0)
         *cdeltXOUT = cdelt;
      else
         *cdeltYOUT = cdelt;

      /*-------*/
      /* CROTA */
      /*-------*/
      if (axistype[i] == LATITUDE)                   /* spatial axis latitude */
      {
         sprintf( message, "CROTA%d", axnum[i] );
         r = 0;
         gdsd_rdble_c( Setdef, tofchar(message), &setlevel, crotaOUT, &r );
         if (r < 0)
         {
            *crotaOUT = 0.0;
            anyoutf( dev, "DEFSET: No rotation found in header. CROTA=0 assumed." );
         }
      }

   }
   if (*skysysOUT != EQUATORIAL && *skysysOUT != ECLIPTIC)
      setdblank_c( epochOUT);
   else
   {
      r = 0;
      gdsd_rdble_c( Setdef, tofchar("EPOCH"), &setlevel, epochOUT, &r );
      if (r < 0)
      {
         anyoutf( dev, "Cannot find an EPOCH in the header, 1950.0 assumed." );
         *epochOUT = 1950.0;
      }
   }
   return( 1 );
}



static void report( char    *taskname,
                    fint    nsubs,
                    double  realtime,
                    double  cputime,
                    fchar   Setin,
                    fchar   Setout,
                    fint    *boxLOO,
                    fint    *boxHIO,
                    int     interpol,
                    fint    *pospol,
                    bool    dataip  )
/*-------------------------------------------------------------*/
/* PURPOSE: Report reprojection information etc.               */
/*-------------------------------------------------------------*/
{
   char  message[256];
   fchar Date;
   int   len;

   fmake( Date, 40 );
   getdate_c( Date );
   anyoutf( 1, " " );
   len = sprintf( message, " ================= %s (%.*s) =================",
                  taskname,
                  nelc_c(Date), Date.a );
   anyoutf( 1, message );
   anyoutf( 1, " Input map : [%.*s]", nelc_c(Setin), Setin.a );
   anyoutf( 1, " Output map: [%.*s]", nelc_c(Setout), Setout.a );
   anyoutf( 1, " Output box: [%d %d %d %d]",
            boxLOO[0], boxLOO[1], boxHIO[0], boxHIO[1] );
   if (interpol)
      anyoutf( 1, " Of (each) %d x %d positions only 4 are transformed, others are interpolated.",
               pospol[0], pospol[1] );
   if (dataip)
      anyoutf( 1, " A bilinear interpolation in a 2 x 2 matrix is used to obtain data values.");

   anyoutf( 1, " %s processed %d subset(s) in %.2f sec (%.2f cpu sec)",
               taskname, nsubs, realtime, cputime );
   memset( message, '=', len );
   message[0] = ' '; message[len] = '\0';
   anyoutf( 1, message );
   anyoutf( 1, " " );
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------*/
/* REPROJ main.                                                */
/*-------------------------------------------------------------*/
{
   int       subnr;
   int       agreed;
   int       interpol;
   fint      scrnum;
   fint      dfault;
   fint      nitems;
   fint      nsubs, nsubsO;
   fint      maxsubs = SUBSMAX;
   fint      maxaxes = AXESMAX;
   fint      axnum[AXESMAX];                     /* GDSINP axis numbers array */
   fint      axcount[AXESMAX];                   /* GDSINP axis lengths array */
   fint      class = 1;              /* Repeat operation for each subset axis */
   fint      subdim, setdim;
   fint      cwlo, cwhi;                                  /* Coordinate words */
   fint      frameLO[AXESMAX];                  /* Coordinate words for frame */
   fint      frameHI[AXESMAX];
   fint      boxLO[AXESMAX];                      /* Coordinate words for box */
   fint      boxHI[AXESMAX];
   fint      boxLOO[2];
   fint      boxHIO[2];
   fint      buflo[2], bufhi[2];
   fint      axistype[AXESMAX];             /* Result from call to 'axtype_c' */
   fint      r;
   fint      elapse;
   fint      systems[3];               /* Sky, projection and velocity system */
   fint      pospol[2];
   fint      skysysIN,  skysysOUT;
   fint      prosysIN,  prosysOUT;
   double    cputime,   realtime;                      /* Variables for timer */
   double    epochIN,   epochOUT;
   double    crvalXIN,  crvalXOUT;
   double    crvalYIN,  crvalYOUT;
   double    cdeltXIN,  cdeltXOUT;
   double    cdeltYIN,  cdeltYOUT;
   double    crpixXIN;
   double    crpixYIN;
   double    crotaIN,   crotaOUT;
   double    factorX,   factorY;
   fchar     CunitX,    CunitY;
   fchar     CtypeX,    CtypeY;
   fchar     CtypeXOUT, CtypeYOUT;
   fchar     Setin,     Setout,   Setdef;
   float     **M = NULL;                /* 2-dim array to store entire subset */
   float     **buffO;                                /* (small) output buffer */
   bool      rotateonly = NO;
   bool      dataip = YES;
   bool      diminish = YES;


   init_c();                                                /* Contact Hermes */
   /* Task identification */
   {
      fchar    Task;                                  /* Name of current task */
      fmake( Task, TASKNAMLEN );                       /* Create empty string */
      myname_c( Task );                                      /* Get task name */
      Task.a[nelc_c(Task)] = '\0';      /* Terminate task name with null char */
      strcpy( taskname, Task.a );
      IDENTIFICATION( taskname, VERSION );           /* Show task and version */
   }


   setfblank_c( &blank );
   fmake(Setin, SETNAMELEN);
   dfault = NONE;
   subdim = 2;                                  /* Subset dimension must be 2 */
   scrnum = 16;                         /* terminal, show data in 'test' mode */
   nsubs  = gdsinp_c( Setin,
                      subin,
                      &maxsubs,
                      &dfault,
                      KEY_INSET,
                      tofchar("Give set (subset(s)) with data to reproject:"),
                      &scrnum,
                      axnum,
                      axcount,
                      &maxaxes,
                      &class,
                      &subdim );

   setdim = gdsc_ndims_c( Setin, &setlevel );


   /*-----------------------------------------------------*/
   /* Determine the edges of this its frame ( frameLO/HI) */
   /*-----------------------------------------------------*/
   {
      fint   r1, r2;
      int    m;

      r1 = 0;
      (void) gdsc_range_c( Setin,
                           &setlevel,
                           &cwlo,
                           &cwhi,
                           &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++)
      {
         frameLO[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         frameHI[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
   }


   /*-------------------------------*/
   /* Characteristics of this set:  */
   /*-------------------------------*/
   if ( !setinfo( Setin,
                  subin,
                  axnum,
                  frameLO,
                  frameHI,
                  axistype,
                  nsubs,
                  systems ) )
   {
      anyoutf( 1, "Header not ok!" );
      finis_c();                                               /* Quit Hermes */
      return( EXIT_FAILURE );
   }

   /*------------------------------------------------------------*/
   /* Get epoch of this set. If sky system is equatorial and the */
   /* epoch in the header is equal to 2000.0 then switch sky     */
   /* system to equatorial-2000.0 (type 5).                      */
   /*------------------------------------------------------------*/
   if ( !spatialmap(Setin, subin, axistype) )
   {
      anyoutf( 1, "Aborting %s because input is not a spatial map!",
               taskname );
      finis_c();                                               /* Quit Hermes */
      return( EXIT_FAILURE );
   }
   epochIN = getepoch( Setin, systems[0] );       /* systems[0] == sky system */
   skysysIN = systems[0];
   if (skysysIN == EQUATORIAL && epochIN == 2000.0)
      skysysIN = EQUATORIAL2000;

   /*------------------------------------------------------------*/
   /* Prepare a box for INSET. Default is a box equal to the     */
   /* frame, but (boxopt=0) the box cannot be greater than the   */
   /* frame of the input subset(s).                              */
   /*------------------------------------------------------------*/
   {
      fint   boxopt = 0;
      dfault = REQUEST;
      scrnum = 8;
      (void) gdsbox_c( boxLO,
                       boxHI,
                       Setin,
                       subin,
                       &dfault,
                       tofchar("BOX="),
                       tofchar(" "),
                       &scrnum,
                       &boxopt );
   }

   prosysIN = systems[1];
   fmake( CunitX,    FITSLEN );
   fmake( CunitY,    FITSLEN );
   fmake( CtypeX,    FITSLEN );
   fmake( CtypeY,    FITSLEN );
   fmake( CtypeXOUT, FITSLEN );
   fmake( CtypeYOUT, FITSLEN );
   
   if (!getspatialdefaults( Setin,
                            subin,
                            axnum,
                            axistype,
                            &crvalXIN, &crvalYIN,
                            &cdeltXIN, &cdeltYIN,
                            &crpixXIN, &crpixYIN,
                            &crotaIN,
                            &factorX,
                            &factorY,
                            CunitX,
                            CunitY,
                            CtypeX,
                            CtypeY ) )
   {
      anyoutf( 1, "Aborting %s because cannot obtain necessary header info!",
               taskname );
      finis_c();                                               /* Quit Hermes */
      return( EXIT_FAILURE );
   }

   anyoutf( 16, "Header info from set [%.*s]", nelc_c(Setin), Setin.a );
   anyoutf( 16, " -Proj. centre (x,y): %g %g", crvalXIN, crvalYIN );
   anyoutf( 16, " -Grid spacing (x,y): %g %g", cdeltXIN, cdeltYIN );
   anyoutf( 16, " -Sky sys.: %d,  proj sys.: %d", skysysIN, prosysIN );
   if (skysysIN == EQUATORIAL || skysysIN == ECLIPTIC)   
      anyoutf( 16, " -Epoch: %f", epochIN );
   anyoutf( 16, " -Crota from header: %g", crotaIN );


   fmake(Setdef, SETNAMELEN);
   if ( defset( Setdef,                       /* Output, name of template set */
                axistype,                   /* Input, axis types of input set */
                CunitX,                /* Input, Units of X axis of input set */
                CunitY,
                CtypeXOUT, CtypeYOUT,
                &crvalXOUT, &crvalYOUT,                     /* Output ....... */
                &cdeltXOUT, &cdeltYOUT,
                &crotaOUT,
                &skysysOUT,
                &prosysOUT,
                &epochOUT ) )
   {
      anyoutf( 16, "Header info from set [%.*s]", nelc_c(Setdef), Setdef.a );
      anyoutf( 16, " -Proj. centre (x,y): %g %g", crvalXOUT, crvalYOUT );
      anyoutf( 16, " -Grid spacing (x,y): %g %g", cdeltXOUT, cdeltYOUT );
      anyoutf( 16, " -Sky sys.: %d,  proj sys.: %d", skysysOUT, prosysOUT );
      if (!dblank_c(&epochOUT))
      anyoutf( 16, " -Epoch: %f", epochOUT );
      anyoutf( 16, " -value for crota: %g", crotaOUT );
   }
   else
   {
      rotateonly = toflog( NO );
      dfault = REQUEST;
      nitems = 1;
      r = userlog_c( &rotateonly, &nitems, &dfault, tofchar("ROTATEONLY="),
                     tofchar("Rotation only (i.e. fix sky-&proj.systems)?   Y/[N]") );
      rotateonly = tobool( rotateonly );

      userinput( crvalXIN,   crvalYIN,
                 cdeltXIN,   cdeltYIN,
                 crotaIN,
                 skysysIN,
                 prosysIN,
                 epochIN,
                 factorX,    factorY,
                 CunitX,     CunitY,
                 CtypeX,     CtypeY,
                 CtypeXOUT,  CtypeYOUT, 
                 &crvalXOUT, &crvalYOUT,
                 &cdeltXOUT, &cdeltYOUT,
                 &crotaOUT,
                 &skysysOUT,
                 &prosysOUT,
                 &epochOUT,
                 axistype,
                 Setin,
                 subin,
                 rotateonly );

      anyoutf( 16, "Values entered by the user:" );
      anyoutf( 16, " -Proj. centre (x,y): %g %g", crvalXOUT, crvalYOUT );
      anyoutf( 16, " -Grid spacing (x,y): %g %g", cdeltXOUT, cdeltYOUT );
      anyoutf( 16, " -Sky sys.: %d,  proj sys.: %d", skysysOUT, prosysOUT );
      if (!dblank_c(&epochOUT))
      anyoutf( 16, " -Epoch: %f", epochOUT );
      anyoutf( 16, " -value for crota: %g", crotaOUT );
   }

   getnewbox( boxLO,      boxHI,
              boxLOO,     boxHIO,                                   /* OUTPUT */
              axistype,
              skysysIN,
              prosysIN,
              crvalXIN,   crvalYIN,        /* old proj. cent. in header units */
              cdeltXIN,   cdeltYIN,       /* old grid spacing in header units */
              crpixXIN,   crpixYIN,
              crotaIN,                             /* Map rotation in degrees */
              epochIN,
              skysysOUT,
              prosysOUT,
              crvalXOUT,  crvalYOUT,
              cdeltXOUT,  cdeltYOUT,
              crotaOUT,
              epochOUT );


   /*------------------------------------------------------------*/
   /* If the input is one 2-dim subset from a > 2-dim set, then  */
   /* ask the user to confirm that he wants to decrease the      */
   /* dimensionality of the output set to 2.                     */
   /*------------------------------------------------------------*/
   if (nsubs == 1 && setdim > 2)   
   {
      fint r;
      fint dfault = REQUEST;
      fint nitems = 1;

      diminish = toflog( YES );
      r = userlog_c( &diminish, &nitems, &dfault, tofchar("DIMINISH="),
          tofchar("Decrease output dimensionality to 2?         [Y]/N") );
      diminish = tobool( diminish ); 
   } 


   /*------------------------------------------------------------*/
   /* Create an output set. Before doing so, change properties   */
   /* of the two spatial axes.                                   */
   /*------------------------------------------------------------*/
   {
      fint   m;
      fint   pmask;
      fint   axcountX = boxHIO[0] - boxLOO[0] + 1;
      fint   axcountY = boxHIO[1] - boxLOO[1] + 1;
      double crotaX = 0.0;
      double crotaY = 0.0;

      if (nsubs == 1 && setdim > 2 && diminish)
      {
         /* Decrease output dimension to 2 */
         class = 2;
      }
      /* Assign GDSINP buffer to GDSOUT buffer */
      gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
      /* Modify the size of the output subset(s) */
      gdscss_c( KEY_OUTSET, boxLOO, boxHIO );
      /* Change the coordinate system. crpix is already changed by GDSCSS! */
      pmask = (32 + 16 + 4 + 2 + 1);
      m = 1;
      if (axistype[0] == LATITUDE)
         crotaX = crotaOUT;
      gdscpa_c( KEY_OUTSET,          /* Keyword associated with a GDSOUT call.*/
                &m,             /* The axis number of the axis to be changed. */
                &axcountX,                                /* Size of the axis.*/
                &cdeltXOUT,        /*  Increment in physical units along axis.*/
                &crotaX,                            /* Rotation angle of axis.*/
                &crpixXIN,                         /* Reference pixel of axis.*/
                &crvalXOUT,   /*  Physical reference value at reference pixel.*/
                CtypeXOUT,                                       /* Axis name.*/
                CunitX,                             /* Physical units of axis.*/
                &pmask );   /* Bit mask denoting which of the six above values*/
                                                              /* are defined. */

      m = 2;
      if (axistype[1] == LATITUDE)
         crotaY = crotaOUT;
      gdscpa_c( KEY_OUTSET, 
                &m, 
                &axcountY, 
                &cdeltYOUT, 
                &crotaY, 
                &crpixYIN,
                &crvalYOUT, 
                CtypeYOUT, 
                CunitY, 
                &pmask );

      fmake(Setout, SETNAMELEN);
      do
      {
         fint     axnumO[AXESMAX];
         fint     axcountO[AXESMAX];

         dfault = NONE;
         scrnum = 8;
         /* Max. subsets is the number of input subsets */
         nsubsO = gdsout_c( Setout, subout, &nsubs, &dfault, KEY_OUTSET,
                            tofchar("Give output set, subset(s): "),
                            &scrnum, axnumO, axcountO, &maxaxes );
         agreed = (nsubsO == nsubs);
         if (!agreed)
            reject_c( KEY_OUTSET, tofchar("# Subsets out != in") );
      }
      while (!agreed);
   }

   /* Update Epoch keyword in header */
   r = 0;
   if ( dblank_c(&epochOUT) )
      gdsd_delete_c( Setout, tofchar("EPOCH"), &setlevel, &r );
   else
      gdsd_wdble_c( Setout, tofchar("EPOCH"), &setlevel, &epochOUT, &r );
      



   /*--------------------------------------------------*/
   /* Interpolate positions instead of calculating     */
   /* using projection formulas.                       */
   /*--------------------------------------------------*/
   do
   {
      char    message[80];
      nitems = 2;
      dfault = REQUEST;
      /* if (rotateonly)*/
      /* VOG: Oct 21, 2009. Also for rotations thare are non linear */
      /* effects and position interpolation betwee only the corner  */
      /* positions of the box seems not justified. Speed becomes    */
      /* less important so set defaults to 1,1                      */ 
      if (0)
      {
         pospol[0] = boxHIO[0]-boxLOO[0]+1;
         pospol[1] = boxHIO[1]-boxLOO[1]+1;
      }
      else
      {
         pospol[0] = pospol[1] = 1;
      }
      sprintf( message, "Size of 'position' interpolation box:  [%d %d]",
               pospol[0], pospol[1] );

      r = userint_c( pospol, &nitems, &dfault, KEY_SPEEDMAT,
                     tofchar(message) );
      if (r == 1)
         pospol[1] = pospol[0];
      pospol[0] = MYMIN( pospol[0], boxHIO[0]-boxLOO[0]+1 );
      pospol[1] = MYMIN( pospol[1], boxHIO[1]-boxLOO[1]+1 );
      agreed = (pospol[0] >= 1 && pospol[1] >= 1);
      if (!agreed)
         reject_c( KEY_SPEEDMAT, tofchar("Number(s) must be >= 1") );
   }
   while(!agreed);
   interpol = ((pospol[0] > 1) || (pospol[1] > 1));


   /*--------------------------------------------------*/
   /* Interpolate pixel value using 3 neighbours or    */
   /* get value of nearest pixel.                      */
   /*--------------------------------------------------*/
   {
      fchar Ip;
      fmake( Ip, 1 );
      dataip = YES;
      dfault = REQUEST;
      nitems = 1;
      r = usercharu_c( Ip, &nitems, &dfault, tofchar("DATAMODE="),
                      tofchar("Data acquisition (B)ilinear/(N)earest pix.:  [B]/N") );
      if (r && Ip.a[0] == 'N')
         dataip = NO;
   }

   /*--------------------------------------------------*/
   /* Create a matrix to store input image data of     */
   /* entire (sub)set for fast access and interpola-   */
   /* tion.                                            */
   /*--------------------------------------------------*/
   M = fmatrix( boxLO, boxHI );
   if (!M)
   {
      anyoutf( 1, "Aborting %s because cannot allocate enough memory for input data!",
               taskname );
      finis_c();                                               /* Quit Hermes */
      return( EXIT_FAILURE );
   }


   /*--------------------------------------------------*/
   /* Create a matrix to store part of the output data.*/
   /* The size of this buffer is equal to the x length */
   /* of the new output times the y value given in the */
   /* position interpolation.                          */
   /*--------------------------------------------------*/
   buflo[0] = buflo[1] = 0;
   bufhi[0] = boxHIO[0] - boxLOO[0];
   if (interpol)
      bufhi[1] = pospol[1] - 1;
   else
      bufhi[1] = MAXBUFLINES - 1;

   buffO = fmatrix( buflo, bufhi );
   if (!buffO)
   {
      anyoutf( 1, "Aborting %s because cannot allocate enough memory for output buffer!",
               taskname );
      freematrix( M, boxLO, boxHI );
      finis_c();
      return( EXIT_FAILURE );
   }


   /*--------------------------------------------------*/
   /* Start the loop over all subsets. Store transfor- */
   /* mation parameters in arrays and swap x & y if    */
   /* the first spatial axis is latitude instead of    */
   /* longitude.                                       */
   /*--------------------------------------------------*/
   {
      double    crval[4];
      double    cdelt[4];
      double    crpix[2];
      double    crota[2];
      double    epoch[2];
      float     minmax[2];
      float     minval[SUBSMAX];
      float     maxval[SUBSMAX];
      fint      nblank[SUBSMAX];
      fint      numblanks;
      fint      prosys[2];
      fint      skysys[2];
      bool      swap;

      swap = (axistype[0] == LATITUDE);

      crval[0]  = crvalXIN;   crval[1]  = crvalYIN;
      crval[2]  = crvalXOUT;  crval[3]  = crvalYOUT;
      cdelt[0]  = cdeltXIN;   cdelt[1]  = cdeltYIN;
      cdelt[2]  = cdeltXOUT;  cdelt[3]  = cdeltYOUT;
      crpix[0]  = crpixXIN;   crpix[1]  = crpixYIN;   /* CRPIX needed to calculate grid offsets in trnasform */
      prosys[0] = prosysIN;   prosys[1] = prosysOUT;
      skysys[0] = skysysIN;   skysys[1] = skysysOUT;
      crota[0]  = crotaIN;    crota[1]  = crotaOUT;
      epoch[0]  = epochIN;    epoch[1]  = epochOUT;

      if (swap)
      {
         DSWAP( crval[0], crval[1] );
         DSWAP( cdelt[0], cdelt[1] );
         DSWAP( crpix[0], crpix[1] );
         DSWAP( crval[2], crval[3] );
         DSWAP( cdelt[2], cdelt[3] );
      }

      elapse = 0;
      timer_c( &cputime, &realtime, &elapse );                 /* Reset timer */
      for (subnr = 0; subnr < nsubsO; subnr++)
      {
         if ( !getdatafromdisk(Setin, subin[subnr], M, boxLO, boxHI) )
         {
            anyoutf( 1, "Something wrong while reading data from disk!" );
            freematrix( M, boxLO, boxHI );
            freematrix( buffO, buflo, bufhi );
            finis_c();                                         /* Quit Hermes */
            return( EXIT_FAILURE );
         }
         if (!filloutput( Setout,
                          subout[subnr],
                          subnr,
                          nsubsO,
                          boxLOO, boxHIO,
                          M,                        /* Matrix with input data */
                          boxLO, boxHI,
                          buffO,
                          pospol,
                          crval,
                          cdelt,
                          crpix,
                          crota,
                          prosys,
                          skysys,
                          epoch,
                          swap,                  /* Are lon/lat axes swapped? */
                          interpol,                 /* Interpolate positions? */
                          dataip,                /* Interpolate pixel values? */
                          minmax,                  /* Min/max of output data. */
                          &numblanks ) )
         {
            anyoutf( 1, "Something wrong while writing data from disk!" );
            freematrix( M, boxLO, boxHI );
            freematrix( buffO, buflo, bufhi );
            finis_c();                                         /* Quit Hermes */
            return( EXIT_FAILURE );
         }
         minval[subnr] = minmax[0];
         maxval[subnr] = minmax[1];
         nblank[subnr] = numblanks;
      }
      elapse = 1;
      timer_c( &cputime, &realtime, &elapse );
      /*--------------------------------------------------*/
      /* Update output header, remove MINMAX descriptors  */
      /* at intersecting levels.                          */
      /*--------------------------------------------------*/
      {
         fint remove = 1;      /* Remove in 'WMINMAX' old minmax descriptors */
         wminmax_c( Setout,
                    subout,
                    minval,
                    maxval,
                    nblank,
                    &nsubsO,
                    &remove );
      }
      report( taskname,
              nsubsO,
              realtime, cputime,
              Setin, Setout,
              boxLOO, boxHIO,
              interpol,
              pospol,
              dataip );
   }

   freematrix( M, boxLO, boxHI );
   finis_c();                                                  /* Quit Hermes */
   return( EXIT_SUCCESS );
}
