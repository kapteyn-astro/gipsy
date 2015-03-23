/* irds_rd_detoff.c.c

                            COPYRIGHT (c) 1990
            Kapteyn Astronomical Institute - University of Groningen
                P.O. Box 800, 9700 AV Groningen, The Netherlands

#>            irds_rd_detoff.dc2


Function:     irds_rd_detoff

Purpose:      To extract (projected) sky position offsets from an IRDS

Category:     IR

File:         irds_dpos.c

Author:       Peter Roelfsema

Use:          INTEGER IRDS_RD_DETOFF(  IRDS     ,    Input   character*(*)
                                       SNIP     ,    Input   integer
                                       SDET     ,    Input   integer
                                       TICK     ,    Input   integer
                                       INCOOR   ,    Input   integer
                                       LON      ,    Input   double
                                       LAT      ,    Input   double
			               OUTCOOR  ,    Input   integer
        			       PROJ     ,    Input   integer
                                       LONOFF   ,    Output  double( >=NDATA )
                                       LATOFF   ,    Output  double( >=NDATA )
                                       TWISTOFF ,    Output  double( >=NDATA )
                                       NDATA    ,    In/Out  integer
                                       STATUS   )    Output  integer
                
              IRDS_RD_DETOFF   0  - error condition
                               n  - LON,LAT closest to detector center 
                                    at sample number abs(n)
                              n>0 - LON,LAT inside detector at sample n
                              n<0 - LON,LAT outside detector at sample n
              IRDS            Name of IRDS to read from.
              SNIP            Sequential snip number to read.
              SDET            Sequential detector number to read.
                              SDET = 0 corresponds to boresight,
                              SDET < 0 corresponds to the center of gravity
                              of band number SDET (see IRCC_BANDNR etc.)
              TICK            Sequential tick of first sample to read.
	      INCOOR	      coordinate system of input LON,LAT ( cf irco.dc2 )
              LON             input longitude (radians)
              LAT             input latitude (radians)
	      OUTCOOR	      coordinate system of output LONOFF,LATOFF
			      0 is a valid number => no transformations	
              PROJ	      projection system number ( cf irco_prname.dc2 )
			      0 is a valid number => no projection
	      LONOFF          Array to receive LON(det)-LON offsets (radians)
              LATOFF          Array to receive LAT(det)-LAT offsets (radians)
              TWISTOFF        Array to receive TWIST(det)-TWIST angles 
                              (radadians ccw w.r.t. +LAT)
              NDATA           I - max number of samples to read.
                              O - number of samples actually read.
              STATUS          Error return code:
                               0  - no error.
                              -1  - IRDS does not exist
                              -2  - IRDS is not a legal irds
                              -3  - SNIP not in IRDS
                              -4  - SDET not in IRDS
                              -5  - TICK not in IRDS
                              -6  - gds read error
                              -7  - no coordinate info in header
                              -8  - bad SDET

Description:
	The BPHF data are read for the satcal ticks in the scan. 
	The boresight positions are transformed to the proper detector
	positions. Transformation and projection take place ( when requested 
	with outcoor and proj ) and the resulting positions are
	interpolated to the sample times. Subsequently the offsets of the
	detector postitions w.r.t the input position are calculated. No 
	conscious shortcuts or approximations have been taken in these 
	calculations. 

        The routine also will find out at which of the requested samples the
	the input position (LON,LAT) was closest to the detector center, and 
	whether that position was inside or outside the detector.

	The routine has been optimized for an inner loop over the 
	detectors and an outer loop over the snips.

	The input and returned angles are in radians

Updates:      Jan  9, 1991: PRR, Document created.
              Apr  2, 1991: PRR, Corrected error in return parameter.

#<


@ integer function irds_rd_detoff( 
@                       character        , integer          , integer       ,
@                       integer          , integer          , 
@                       double precision , double precision , 
@                       integer          , integer          ,
@                       double precision , double precision , double precision ,
@                       integer          , integer          )

*/

#include "gipsyc.h"
#include "stdlib.h"
#include "stdio.h"
#include "math.h"
#include "anyout.h"
#include "ctype.h"
#include "gdsd_rint.h"
#include "gdsd_rchar.h"
#include "gdsc_word.h"
#include "irds_rd_detpos.h"
#include "ircc_mask.h"
#include "ircc_bandnr.h"
#include "irco_number.h"
#include "irco_precess.h"
#include "irco_torect.h"
#include "irco_project.h"
#include "irco_transform.h"
#include "irco_tospher.h"

#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.l = len ; }

/* miscellaneous definitions */
#define true              1
#define false             0
static fint	test = 16 ;
#define TEST        (&test)
#define MAXTXTLEN	 80
#define	BADSDET		 -8			/* sdet not in IRDS */
#define	BADIRDS		 -9			/* bad irds */
#define	PI		3.14159265358979
#define RADPERDEG	( PI / 180.0 )		/* nr. of radians per degree */

fint irds_rd_detoff_c( fchar irds     , fint *snip     , fint *sdet       ,
                       fint *tick     , fint *insys    , 
                       double *lon    , double *lat    , 
                       fint *outsys   , fint *prosys   ,
                       double *lonoff , double *latoff , double *twist ,
                       fint *nsamples , fint *status   )
{
   fchar    instr ;
   char     line[250] ;
   fint     one = 1 ;
   fint     level , error = 0 , axnum ;
   fint     naxis1 , sample , nticks , rsdet , detno , closest ; 
   fint     eclsys = 3 ;

   float    yloc , zloc , ysize , zsize ;
   float    distance , mindistance ;
   float    mission = 1983.5 ;

   double   xyzin[ 3 ] , xyzout[ 3 ] ; 
   double   srclon     , srclat      , coslat ;
   double   isoff      , xsoff       ;

   finit( instr , MAXTXTLEN ) ;
   *status = 0 ;				/* think positive */

   level     = 0 ;				/* get nr. samples per tick */
   gdsd_rint_c( irds , tofchar( "NAXIS1" ) , &level , &naxis1 , &error ) ;
   if ( error < 0 ) {				/* something's wrong */
      *status = BADIRDS ;
      return( 0 ) ;				/* exit */
   }
   nticks    = *nsamples / naxis1 ;		/* only intgral nr. ticks */
   *nsamples =    nticks * naxis1 ;

   error = 0 ;
   if ( *sdet != 0 ) {				/* data for one detector */
      axnum = 3 ;				/* sdet is axis 3 */
						/* get detector number */
      level = gdsc_word_c( irds , &axnum , sdet , &level , &error ) ;
      gdsd_rint_c( irds , tofchar( "DETNO" ) , &level , &detno , &error ) ;
      if ( error < 0 ) {			/* detector number not found */
         *status = BADSDET ;
         return( 0 ) ;				/* exit */
      }
      rsdet = *sdet ;				/* use input detector number */
   } else {					/* data for borsight */
						/* get instrument name */
      gdsd_rchar_c( irds , tofchar( "INSTRUME" ) , &level , instr , &error ) ;
      if ( error < 0 ) {			/* instrument name not found */
         *status = BADIRDS ;
         return( 0 ) ;				/* exit */
      }
      detno = -ircc_bandnr_c( instr ) ; 	/* get boresight det. number */
      rsdet = detno ;				/* use borsesight */
   } 

                                                /* precess ecliptic to 1983.5 */
   irco_precess_c( &eclsys , &mission , &eclsys ) ;

						/* get detector positions */
   irds_rd_detpos_c( irds   , snip     , &rsdet  , tick   ,
                     outsys , prosys   , lonoff  , latoff , 
                     twist  , nsamples , status  ) ;
   if ( *status != 0 ) {				/* problems? */
      return( 0 ) ;				/* exit */
   }

						/* transform input coords... */
   irco_torect_c(     lon    , lat     , xyzin   ,          &one ) ;
   irco_transform_c(  xyzin  , insys   , xyzout  , outsys , &one ) ;
   if( *prosys ) irco_project_c(    prosys , xyzout  , &srclon , &srclat, &one ) ;
   else irco_tospher_c(    xyzout , &srclon , &srclat ,          &one ) ;
       						/*    ... to output system */
   coslat = cos( srclat ) ;			/* correction to real deg. */

   mindistance = PI ;				/* maximum on-sky distance */
   closest = -2 * *nsamples ;
   for ( sample = 0 ; sample < *nsamples ; sample++ ) {
						/* get offsets */
      lonoff[ sample ]   = ( srclon - lonoff[ sample ] ) * coslat  ;
      latoff[ sample ]   =   srclat - latoff[ sample ] ;
						/* get distance to source */
      distance = sqrt( lonoff[ sample ] * lonoff[ sample ] + 
                       latoff[ sample ] * latoff[ sample ] ) ;
      if ( distance < mindistance ) {		/* closer than before ? */
         mindistance = distance ;		/*  -> this one is closest */
         closest     = sample   ;
      }
   }

						/* get detector sizes */
   ircc_mask_c( &detno , &yloc , &zloc , &ysize , &zsize ) ;
   ysize    = ysize / 60 * RADPERDEG ;		/* change to radians */
   zsize    = zsize / 60 * RADPERDEG ;
						/* get in/x -scan offsets */
   isoff    = lonoff[ closest ] * cos( twist[ closest ] ) +
              latoff[ closest ] * sin( twist[ closest ] ) ;
   xsoff    = latoff[ closest ] * cos( twist[ closest ] ) -
              lonoff[ closest ] * sin( twist[ closest ] ) ;
   closest  = closest + 1 ;			/* offset => index */
						/* is source inside det.? */
   sprintf( line, "<IRDS_RD_DETOFF> Size of the detector %.2fx%.2f deg", ysize/RADPERDEG, zsize/RADPERDEG ) ;
   anyout_c( TEST, tofchar(line) ) ;
   if ( ( fabs( isoff ) > ( ysize / 2.0 ) ) ||
        ( fabs( xsoff ) > ( zsize / 2.0 ) )  ) closest = -closest ;

   return( closest ) ;				/* exit */
}

