/* irds_samples.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_samples.dc2

Function:     irds_samples

Purpose:      Description of routins acessing sampels in IRDS sets.

Category:     IR

File:         irds_samples.c

Author:       P.R. Roelfsema

Description:    To access the samples in an IRDS a number of routines are
              available. Some of these access only data, some also access
              position information for the samples.
                The following routines are available:

              IRDS_RD_SAMPLES - basic sample read routine.
              IRDS_WR_SAMPLES - basic sample write routine.
              IRDS_RD_BPHF    - reads BPHF data for a series of ticks
              IRDS_WR_BPHF    - writes BPHF data for a series of ticks
              IRDS_RD_SKYPOS  - reads sky coordinates for ticks
              IRDS_WR_SKYPOS  - writes sky coordinates for ticks
              IRDS_RD_SAMPLB  - reads samples and sky positions
              IRDS_RD_SAMPT   - reads samples and timing data
              IRDS_RD_SAMPXY  - reads samples and projected positions

Updates:      Aug 10, 1990: PRR, Creation date
              Apr 15, 1994: JPT, call gdst_abslevel_c instead of gds___abslev_c

#<

*/

#include "gipsyc.h"
#include "irds_exist.h"
#include "ircc_times.h"
#include "gdsc_word.h"
#include "gdsc_size.h"
#include "gdsd_rint.h"
#include "gdsd_rreal.h"
#include "gdsd_rdble.h"
#include "gdsd_rchar.h"
#include "gdsi_read.h"
#include "gdsd_wreal.h"
#include "gdsd_wint.h"
#include "gdsd_wdble.h"
#include "gdsd_wchar.h"
#include "gdsd_grint.h"
#include "gdsi_write.h"
#include "gds_tune.h"

#define INTENDED_BPHF	 1			/* retrun intended posn's */
#define IRDSOK		 0			/* IRDS is OK */
#define NOEXIST		-1			/* IRDS does not exist */
#define NOIRDS          -2			/* not an IRDS */
#define BADSNIP		-3			/* snip not in IRDS */
#define BADSDET		-4			/* detector not in IRDS */
#define BADTICK		-5			/* tick not in IRDS */
#define READERROR	-6			/* gds read error */
#define WRITEERROR	-6			/* gds write error */
#define NOCOORDS	-7			/* no coordinates in header */
#define NOT_YET_IMPLEMENTED	-10		/* not yet implemented ! */

#define false		0
#define true		1
#define PI		3.1415926535897932384	/* PI in 20 decimals */
#define DEGPERRAD	(double) 180/PI		/* DEGrees per RADians */
#define RADPERDEG	(double) PI/180		/* RADians per DEGrees */


/*
   check_irds checks whether the irds does exist and whether the data that
the caller wants are present in that irds.
*/

static fint check_irds( fchar irds , fint *snip , fint *sdet , fint *tick )
{
   fint  error = 0 ;
   fint  level ;
   fint  axis ;

   switch ( irds_exist_c( irds , &error ) ) {	/* does IRDS exist ? */
      case  0 : break ;				/* yes */
      case -1 : return( NOEXIST ) ;		/* no */
                break ;
      default : return( NOIRDS ) ;		/* not an irds */
                break ;
   }

   level = 0 ;
   axis  = 4 ;					/* find level of snips */
   error = 0 ;
   level = gdsc_word_c( irds , &axis , snip , &level ,&error ) ;
   if ( error < 0 ) return( BADSNIP ) ;		/* snip not in irds */

   level = 0 ;
   axis  = 3 ;					/* find level of dets */
   level = gdsc_word_c( irds , &axis , sdet , &level ,&error ) ;
   if ( error < 0 ) return( BADSDET ) ;		/* detector not in irds */

   level = 0 ;
   axis  = 2 ;					/* find level of ticks */
   level = gdsc_word_c( irds , &axis , tick , &level ,&error ) ;
   if ( error < 0 ) return( BADTICK ) ;		/* tick not in irds */

   return( IRDSOK ) ;				/* good irds */
}


/*
   irdsc_word returns the coordinate word corresponding to the grid specified
by snip, sdet and tick.
*/

static fint irdsc_word_c( fchar irds    , fint *snip   , fint *sdet , 
                          fint  *tick   , fint *sample , fint *error ) 
{
   fint level  ;
   fint axis   ;

   level  = 0 ;
   axis   = 4 ;					/* snip level */
   level  = gdsc_word_c( irds , &axis , snip   ,  &level , error ) ;
   axis   = 3 ;					/* detector level */
   level  = gdsc_word_c( irds , &axis , sdet   ,  &level , error ) ;
   axis   = 2 ;					/* tick level */
   level  = gdsc_word_c( irds , &axis , tick   ,  &level , error ) ;
   axis   = 1 ;					/* sample level */
   level  = gdsc_word_c( irds , &axis , sample ,  &level , error ) ;
   if ( !*error ){
      return( level ) ;				/* return good coordinate */
   } else {
      return( 0 ) ;				/* bad coordinate */
   }
}

/*
   Interpol is a simple expander and linear interpolator 
*/
static void interpol( double *data , fint *ndata , fint *decim )
{
   fint n , m ;
   fint nvalues, dec ;
   double prev , next ;

   dec = *decim ;
   nvalues = *ndata / dec ;
   for ( n = nvalues ; n > 0 ; n-- ) 
      data[ n * dec ] = data [ n ] ;

   for ( n = 0 ; n < nvalues ; n++ ) {
      prev = data[ n * dec ] ;
      next = data[ ( n + 1 ) * dec ] ;
      for ( m = 1 ; m < dec ; m++ ){
         data[ m + n * dec ] = prev + m * ( next - prev ) / dec ;
      }
   }
}


/* irds_wr_samples.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_wr_samples.dc2

Function:     irds_wr_samples

Purpose:      To write raw samples to an IRDS

Category:     IR

File:         irds_samples.c

Author:       P.R. Roelfsema

Use:          IRDS_WR_SAMPLES( IRDS   ,    Input   character*(*)
                               SNIP   ,    Input   integer
                               SDET   ,    Input   integer
                               TICK   ,    Input   integer
                               DATA   ,    Output  real( >=NDATA )
                               NDATA  ,    In/Out  integer
                               STATUS )    Output  integer
                                       
              IRDS        Name of IRDS to write to.
              SNIP        Sequential snip number to write to.
              SDET        Sequential detector number to write.
              TICK        Sequential tick of first sample to write.
              DATA        Data array containing samples.
              NDATA       I - max number of samples to write.
                          O - number of samples actually written.
              STATUS      Error return code:
                           0  - no error.
                          -1  - IRDS does not exist
                          -2  - IRDS is not a legal irds
                          -3  - SNIP not in IRDS
                          -4  - SDET not in IRDS
                          -5  - TICK not in IRDS
                          -6  - gds write error

Updates:      Aug 10, 1990: PRR, Creation date
              Sep 10, 1990: PRR, Fixed bug.

#<

Fortran to C interface:

@ subroutine irds_wr_samples( character , integer , integer , 
@                             integer   , real    , integer , integer )

*/

void irds_wr_samples_c( fchar irds  , fint *snip  , fint *sdet , fint *tick 
                      , float *data , fint *ndata , fint *status )
{
   fint error = 0 ;
   fint endcal    ;
   fint tid       ;
   fint reqpts    ;
   fint tickpts   ;
   fint ticks     ;
   fint samplo    ;
   fint samphi    ;
   fint axis      ;
   fint cwlo  = 0 ;
   fint cwhi  = 0 ;

   reqpts = *ndata ;
   *ndata = 0 ;

   if ( ( *status = check_irds( irds , snip , sdet , tick ) ) != 0 ) 
      return ;					/* bad irds -> return */   

   axis      = 1 ;
   tickpts = gdsc_size_c( irds , &axis , &error ) ;	/* nr of samples/tick */
   axis      = 2 ;
   ticks   = gdsc_size_c( irds , &axis , &error ) - *tick + 1 ;	/* nr of ticks */
   reqpts    = ( reqpts / tickpts ) > ticks ? ticks : ( reqpts / tickpts ) ;
   reqpts    = reqpts * tickpts ;			/* reqest nr. of ticks */
   if ( reqpts <= 0 ) return ;				/* < 1 tick -> bad */
   endcal    = *tick + reqpts / tickpts - 1 ;		/* find request end */
   samplo    = 1 ;					/* first sample/tick */
   samphi    = tickpts ;				/* last sample/tick */
   cwlo      = irdsc_word_c( irds, snip, sdet, tick   , &samplo, &error ) ;
   cwhi      = irdsc_word_c( irds, snip, sdet, &endcal, &samphi, &error ) ;
   tid       = 0 ;
   gdsi_write_c( irds , &cwlo , &cwhi , data , &reqpts , ndata , &tid ) ;
   if ( tid != 0 ) *status = WRITEERROR ;		/* trouble writing */

}


/* irds_rd_samples.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_rd_samples.dc2

Function:     irds_rd_samples

Purpose:      To read raw samples from an IRDS

Category:     IR

File:         irds_samples.c

Author:       P.R. Roelfsema

Use:          IRDS_RD_SAMPLES( IRDS   ,    Input   character*(*)
                               SNIP   ,    Input   integer
                               SDET   ,    Input   integer
                               TICK   ,    Input   integer
                               DATA   ,    Output  real( >=NDATA )
                               NDATA  ,    In/Out  integer
                               STATUS )    Output  integer
                                       
              IRDS        Name of IRDS to read from.
              SNIP        Sequential snip number to read from.
              SDET        Sequential detector number to read.
              TICK        Sequential tick of first sample to read.
              DATA        Data array to store samples.
              NDATA       I - max number of samples to read.
                          O - number of samples actually read.
              STATUS      Error return code:
                           0  - no error.
                          -1  - IRDS does not exist
                          -2  - IRDS is not a legal irds
                          -3  - SNIP not in IRDS
                          -4  - SDET not in IRDS
                          -5  - TICK not in IRDS
                          -6  - gds read error

Updates:      Aug 10, 1990: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irds_rd_samples( character , integer , integer , 
@                             integer   , real    , integer , integer )

*/

void irds_rd_samples_c( fchar irds  , fint *snip  , fint *sdet , fint *tick 
                      , float *data , fint *ndata , fint *status )
{
   fint error = 0 ;
   fint endcal    ;
   fint tid       ;
   fint reqpts    ;
   fint tickpts   ;
   fint ticks     ;
   fint samplo    ;
   fint samphi    ;
   fint axis      ;
   fint cwlo  = 0 ;
   fint cwhi  = 0 ;

   reqpts = *ndata ;
   *ndata = 0 ;

   if ( ( *status = check_irds( irds , snip , sdet , tick ) ) != 0 ) return ;
   
   axis      = 1 ;
   tickpts = gdsc_size_c( irds , &axis , &error ) ;	/* nr of samples/tick */
   axis      = 2 ;
   ticks   = gdsc_size_c( irds , &axis , &error ) - *tick + 1 ;	/* nr of ticks */
   reqpts    = ( reqpts / tickpts ) > ticks ? ticks : ( reqpts / tickpts ) ;
   reqpts    = reqpts * tickpts ;			/* reqest nr. of ticks */
   if ( reqpts <= 0 ) return ;				/* < 1 tick -> bad */
   endcal    = *tick + reqpts / tickpts - 1 ;		/* find request end */
   samplo    = 1 ;					/* first sample/tick */
   samphi    = tickpts ;				/* last sample/tick */
   cwlo      = irdsc_word_c( irds, snip, sdet, tick   , &samplo, &error ) ;
   cwhi      = irdsc_word_c( irds, snip, sdet, &endcal, &samphi, &error ) ;
   tid       = 0 ;
   gdsi_read_c( irds , &cwlo , &cwhi , data , &reqpts , ndata , &tid ) ;
   if ( tid != 0 ) *status = READERROR ;		/* trouble reading */

}


/* irds_wr_bphf.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_wr_bphf.dc2

Function:     irds_wr_bphf

Purpose:      To writes BPHF data to an IRDS for each satcal tick

Category:     IR

File:         irds_samples.c

Author:       P.R. Roelfsema

Use:          IRDS_WR_BPHF(    IRDS    ,    Input   character*(*)
                               SNIP    ,    Input   integer
                               TICK    ,    Input   integer
                               SRLON   ,    Input   double( >=NSATS )
                               ESRLON  ,    Input   double( >=NSATS )
                               SRLAT   ,    Input   double( >=NSATS )
                               ESRLAT  ,    Input   double( >=NSATS )
                               TWIST   ,    Input   double( >=NSATS )
                               LNGSUN  ,    Input   double
                               SUNRATE ,    Input   double
                               NSATS   ,    Input   integer
                               STATUS  )    Output  integer
                                       
              IRDS        Name of IRDS to write to.
              SNIP        Sequential snip number to write.
              TICK        Sequential tick of first sample to write.
              SRLON       Array containing SRLON in radians
              ESRLON      Array containing error in SRLON 
                          If ESRLON(1) < 0, no errors are put in the
                          header, if ESRLON(2) < 0 ESRLON(1) is used
                          as average error for the entire snip.
                          In both cases the rest of the array is not accessed.
              SRLAT       Array containing SRLAT in radians
              ESRLAT      Array containing error in SRLAT
                          If ESRLAT(1) < 0, no errors are put in the
                          header, if ESRLAT(2) < 0 ESRLAT(1) is used
                          as average error for the entire snip.
                          In both cases the rest of the array is not accessed.
              TWIST       Array containing TWIST angles in radians 
                          (c.c.w. from north)
              LNGSUN      Solar longitude at TICK in radians
              SUNRATE     Rate of change of solar longitude in radians/tick
              NSATS       Number of satcal ticks to write.
              STATUS      Error return code:
                           0  - no error.
                          -1  - IRDS does not exist
                          -2  - IRDS is not a legal irds
                          -3  - SNIP not in IRDS
                          -5  - TICK or TICK + NSATS not in IRDS


Updates:      Aug 20, 1990: PRR, Creation date
              Sep 21, 1990: PRR, changed PSI/THETA to SRLON/SRLAT

#<

Fortran to C interface:

@ subroutine irds_wr_bphf( character , integer , integer , 
@                          double precision ,  double precision ,  
@                          double precision ,  double precision , 
@                          double precision ,  double precision , 
@                          double precision ,  integer   , integer )

*/

void irds_wr_bphf_c(    fchar  irds     , fint   *snip  , fint   *tick  
                      , double *srlon   , double *esrlon
                      , double *srlat   , double *esrlat
                      , double *twist   , double *lngsun
                      , double *sunrate , fint   *nsats  , fint   *status )
{  
   fint    error = 0 ;
   fint    lonerrs = false , laterrs = false ;
   fint    axis ;
   fint    slevel , sslevel ;
   double  buf = 0 ;
   fint    sattick , sdet = 1 ;
   fint    n ;
   double  scf = DEGPERRAD ;

   if ( ( *status = check_irds( irds , snip , &sdet , tick ) ) != 0 ) return ;

   sattick  = *tick + *nsats - 1 ;		/* last tick to write to */
   if ( ( *status = check_irds( irds , snip , &sdet , &sattick ) ) != 0 ) return ;

   slevel   = 0 ;
   axis     = 4 ;				/* snip level */
   slevel   = gdsc_word_c( irds , &axis , snip   ,  &slevel , &error ) ;
   buf = scf * *lngsun  ;
   gdsd_wdble_c( irds , tofchar( "LNGSUN" )  , &slevel , &buf , &error ) ;
   buf = scf * *sunrate ;
   gdsd_wdble_c( irds , tofchar( "SUNRATE" ) , &slevel , &buf , &error ) ;

   if( esrlon[ 0 ] > 0 ) {
      if( esrlon[ 1 ] < 0 ) {
         buf = scf * esrlon[ 0 ]   ;			/* avg.error in srlon */
         gdsd_wdble_c( irds , tofchar( "SIGSRLON" ) , &slevel , &buf , &error ) ;
      } else {
         lonerrs = true ;
      }
   } else {
      buf = 0 ;						/* no error in srlon */
      gdsd_wdble_c( irds , tofchar( "SIGSRLON" ) , &slevel , &buf , &error ) ;
   }
   if( esrlat[ 0 ] > 0 ) {
      if( esrlat[ 1 ] < 0 ) {
         buf = scf * esrlat[ 0 ]   ;			/* avg.error in srlon */
         gdsd_wdble_c( irds , tofchar( "SIGSRLAT" ) , &slevel , &buf , &error ) ;
      } else {
         laterrs = true ;
      }
   } else {
      buf = 0 ;						/* no error in srlon */
      gdsd_wdble_c( irds , tofchar( "SIGSRLAT" ) , &slevel , &buf , &error ) ;
   }

   axis     = 2 ;					/* snip-tick level */
   for ( n = 0 ; n < *nsats ; n++ ) {			/* loop on ticks */
      error    = 0 ;
      sattick  = *tick + n ;
      sslevel  = gdsc_word_c( irds , &axis , &sattick ,  &slevel , &error ) ;
      buf = scf * srlon[ n ]    ;			/* put srlon */
      gdsd_wdble_c( irds , tofchar( "SRLON" )    , &sslevel , &buf , &error ) ;
      if( lonerrs ) {
         buf = scf * esrlon[ n ]   ;			/* put error in srlon */
         gdsd_wdble_c( irds , tofchar( "SIGSRLON" ) , &sslevel , &buf , &error ) ;
      }
      buf = scf * srlat[ n ]  ;				/* put srlat */
      gdsd_wdble_c( irds , tofchar( "SRLAT" )    , &sslevel , &buf , &error ) ;
      if( laterrs ) {
         buf = scf * esrlat[ n ] ;			/* put error in srlat */
         gdsd_wdble_c( irds , tofchar( "SIGSRLAT" ) , &sslevel , &buf , &error ) ;
      }
      buf = scf * twist[ n ]    ;			/* put twist */
      gdsd_wdble_c( irds , tofchar( "TWIST" )    , &sslevel , &buf , &error ) ;
   }
}



/* irds_rd_bphf.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_rd_bphf.dc2

Function:     irds_rd_bphf

Purpose:      To read BPHF data for ticks from an IRDS

Category:     IR

File:         irds_samples.c

Author:       P.R. Roelfsema

Use:          IRDS_RD_BPHF(    IRDS    ,    Input   character*(*)
                               SNIP    ,    Input   integer
                               TICK    ,    Input   integer
                               SRLON   ,    Output  double( >=NSATS )
                               ESRLON  ,    Output  double( >=NSATS )
                               SRLAT   ,    Output  double( >=NSATS )
                               ESRLAT  ,    Output  double( >=NSATS )
                               TWIST   ,    Output  double( >=NSATS )
                               LNGSUN  ,    Output  double
                               SUNRATE ,    Output  double
                               NSATS   ,    I/O	    integer
                               STATUS  )    Output  integer
                                       
              IRDS        Name of IRDS to read from.
              SNIP        Sequential snip number to read.
              TICK        Sequential tick of first sample to read.
              SRLON       Array to receive SRLON (radians)
              ESRLON      Array to receive error in SRLON (radians)
              SRLAT       Array to receive SRLAT (radians)
              ESRLAT      Array to receive error in SRLAT (radians)
              TWIST       Array to receive TWIST angles (radians)
              LNGSUN      Solar longitude (radians)
              SUNRATE     Rate of change in LNGSUN (radians/tick)
              NSATS       In: Number of satcal ticks to read.
	      		  out: Number of ticks found
              STATUS      Error return code:
                           1  - SRLON/SRLAT are intended positions
                           0  - no error.
                          -1  - IRDS does not exist
                          -2  - IRDS is not a legal irds
                          -3  - SNIP not in IRDS
                          -5  - TICK not in IRDS
                          -6  - gds read error

Note:

              The routine tries to return the best estimate of the position
            parameters. Thus it will first try to find the BPHF information 
            in the irds. If no BPHF data was added to the irds, the intended
            position parameters are returned (STATUS = 1). Since the intended 
            position  parameters have no error estimates the ESRLON and ESRLAT 
            arrays will contain zeros in this case. Also zeros will be 
            returned for TWIST and LNGSUN.

Updates:      Aug 20, 1990: PRR, Creation date
              Sep  5, 1990: PRR, added STATUS = 1.
              Sep 21, 1990: PRR, changed PSI/THETA to SRLON/SRLAT
	      Dec  6, 1990: DK, nsats is upon output the number of ticks found

#<

Fortran to C interface:

@ subroutine irds_rd_bphf( character , integer , integer ,
@                          double precision ,  double precision ,  
@                          double precision ,  double precision , 
@                          double precision ,  double precision , 
@                          double precision ,  integer , integer )

*/

void irds_rd_bphf_c(   fchar irds       , fint  *snip  , fint  *tick 
                      , double *srlon   , double *esrlon
                      , double *srlat   , double *esrlat
                      , double *twist   , double *lngsun
                      , double *sunrate , fint  *nsats  , fint  *status )
{  
   fint    error = 0 ;
   fint    axis ;
   fint    yes = true , no = false ;
   fint    level,slevel ,sslevel ;
   float   psizero = 0 , psirate = 0 , thetazero = 0 ;
   fint    lonerrs = false , laterrs = false ;
   double  scf = RADPERDEG ;
   double  buf = 0 ;
   fint    snipcal = 0 ;
   fint    sattick , sdet = 1 ;
   fint    n ;
   int     bphf ;

   if ( ( *status = check_irds( irds , snip , &sdet , tick ) ) != 0 ) return ;

   axis = 4 ; n = 1 ;
   gdsd_grint_c( irds, tofchar( "SNIPDUR" ), &axis, snip, &n, &sattick, &error );
   sattick += 1 - *tick ;
   if ( sattick <= 0 ) { 
	*status = BADTICK ;
	*nsats = 0 ;
	return ;
	}
   if ( sattick < *nsats ) *nsats = sattick ;

   error    = 0 ;
   level    = 0 ;
   axis     = 4 ;					/* snip level */
   slevel   = gdsc_word_c( irds , &axis , snip ,  &level , &error ) ;
   axis     = 2 ;					/* snip-tick level */
   sslevel  = gdsc_word_c( irds , &axis , tick ,  &slevel , &error ) ;
   gdst_abslevel_c( &yes ) ;				/* search local level */
   gdsd_rdble_c( irds , tofchar( "SRLON" )   , &sslevel , &buf , &error ) ;
   gdst_abslevel_c( &no ) ;				/* enable all levels */
   bphf  = ( error >= 0 ) ;				/* is BPHF present ? */
   error = 0 ;
   if ( bphf ) {					/* use BPHF */
      gdsd_rdble_c( irds , tofchar( "LNGSUN" )  , &slevel , &buf , &error ) ;
      *lngsun  = buf * scf ;				/* get solar longitude */
      gdsd_rdble_c( irds , tofchar( "SUNRATE" ) , &slevel , &buf , &error ) ;
      *sunrate = buf * scf ;				/* get solar longitude */
      gdsd_rdble_c( irds , tofchar( "SIGSRLON" ), &slevel , &buf , &error ) ;
      lonerrs = ( error != slevel ) ;
      error   = 0 ;
      esrlon[ 0 ] = buf * scf ;				/* avg. error in srlon */
      gdsd_rdble_c( irds , tofchar( "SIGSRLAT" ), &slevel , &buf , &error ) ;
      laterrs = ( error != slevel ) ;
      error   = 0 ;
      esrlat[ 0 ] = buf * scf ;				/* avg. error in srlon */
      for ( n = 0 ; n < *nsats ; n++ ) {		/* loop on ticks */
         axis     = 2 ;					/* snip-tick level */
         sattick  = *tick + n ;
         sslevel  = gdsc_word_c( irds , &axis , &sattick ,  &slevel , &error ) ;
         gdsd_rdble_c( irds , tofchar( "SRLON" )    , &sslevel , &buf , &error ) ;
         srlon[ n ]  = buf * scf ;			/* get srlon */
         if( lonerrs ) {
            gdsd_rdble_c( irds , tofchar( "SIGSRLON" ) , &sslevel , &buf , &error ) ;
            esrlon[ n ] = buf * scf ;			/* get error in srlon */
         } else {
            esrlon[ n ] = esrlon[ 0 ] ;
         }
         gdsd_rdble_c( irds , tofchar( "SRLAT" )    , &sslevel , &buf , &error ) ;
         srlat[ n ]  = buf * scf ;			/* get srlat */
         if( laterrs ) {
            gdsd_rdble_c( irds , tofchar( "SIGSRLAT" ) , &sslevel , &buf , &error ) ;
            esrlat[ n ] = buf * scf ;			/* get error in srlat */
         } else {
            esrlat[ n ] = esrlat[ 0 ] ;
         }
         gdsd_rdble_c( irds , tofchar( "TWIST" )    , &sslevel , &buf , &error ) ;
         twist[ n ]  = buf * scf ;			/* get twist */
      }
   } else {						/* use intende positions */
      *status = INTENDED_BPHF ;
      gdsd_rint_c(  irds , tofchar( "SNIPCAL" )  , &slevel  
                  , &snipcal   , &error ) ;		/* get tick of snip */
      gdsd_rreal_c( irds , tofchar( "PSI" )      , &slevel 
                  , &psizero   , &error ) ;		/* get intended psi of snip */
      gdsd_rreal_c( irds , tofchar( "PSIRATE" )  , &slevel 
                  , &psirate   , &error ) ;		/* get intended psirate of snip */
      gdsd_rreal_c( irds , tofchar( "THETA" )    , &slevel 
                  , &thetazero , &error ) ;		/* get  theta of snip */
      psizero = psizero + 				/* offset psi to */ 
              ( snipcal + *tick - 1 ) * psirate ;	/*  desired  tick */
      for ( n = 0 ; n < *nsats ; n++ ) {		/* loop on all samples */
         srlon[ n ]   = ( psizero + n * psirate )*scf;	/* calculate srlon */
         esrlon[  n ] = 0 ;				/* no error in srlon */
         srlat[ n ]   = ( 90 - thetazero ) * scf ;	/* calculate srlat */
         esrlat[ n ]  = 0 ;				/* no error in srlat */
         twist[ n ]   = 0 ;				/* no twist */
      }
      *lngsun  = 0 ;					/* no solar longitude */
      *sunrate = 0 ;
   }
}


/* irds_wr_skypos.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_wr_skypos.dc2

Function:     irds_wr_skypos

Purpose:      To write skypositions for satcal ticks to an IRDS

Category:     IR

File:         irds_samples.c

Author:       P.R. Roelfsema

Use:          IRDS_WR_SKYPOS(  IRDS   ,    Input   character*(*)
                               SNIP   ,    Input   integer
                               TICK   ,    Input   integer
                               LON    ,    Input   double( >=NSATS )
                               LAT    ,    Input   double( >=NSATS )
                               TWIST  ,    Input   double( >=NSATS )
                               NSATS  ,    Input   integer
                               STATUS )    Output  integer
                                       
              IRDS        Name of IRDS to write to.
              SNIP        Sequential snip number to write.
              TICK        Sequential tick of first sample to write.
              LON         Array containing LON coordinates
              LAT         Array containing LAT coordinates
              TWIST       Array containing TWIST angles (ccw w.r.t. +LAT)
              NSATS       Number of satcal ticks  to write.
              STATUS      Error return code:
                           0  - no error.
                          -1  - IRDS does not exist
                          -2  - IRDS is not a legal irds
                          -3  - SNIP not in IRDS
                          -5  - TICK or TICK + NSATS not in IRDS

Updates:      Aug 20, 1990: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irds_wr_skypos( character , integer    ,
@                            integer          , double precision ,
@                            double precision , double precision ,
@                            integer          , integer )

*/

void irds_wr_skypos_c(  fchar irds    , fint  *snip   
                      , fint  *tick   , double *lon   , double *lat
                      , double *twist , fint  *nsats  , fint  *status )
{  
   fint  error = 0 ;
   fint  axis ;
   fint  slevel ,sslevel ;
   double buf = 0 ;
   fint  sattick , sdet = 1 ;
   fint  n ;

   if ( ( *status = check_irds( irds , snip , &sdet , tick ) ) != 0 ) return ;

   sattick  = *tick + *nsats - 1 ;		/* last tick to write to */
   if ( ( *status = check_irds( irds , snip , &sdet , &sattick ) ) != 0 ) return ;

   slevel   = 0 ;
   axis     = 4 ;				/* snip level */
   slevel   = gdsc_word_c( irds , &axis , snip   ,  &slevel   , &error ) ;
   axis     = 2 ;				/* snip-tick level */
   sslevel  = gdsc_word_c( irds , &axis , tick ,  &sslevel , &error ) ;

   for ( n = 0 ; n < *nsats ; n++ ) {		/* loop on ticks */
      sattick = *tick + n ;
      axis    = 2 ;				/* snip-tick level */
      sslevel = gdsc_word_c( irds , &axis , &sattick ,  &sslevel , &error ) ;
      buf = lon[ n ] ;				/* put longitude of tick */
      gdsd_wdble_c( irds , tofchar( "LON" )   , &sslevel , &buf , &error ) ;
      buf = lat[ n ] ;				/* put latitude of tick */
      gdsd_wdble_c( irds , tofchar( "LAT" )   , &sslevel , &buf , &error ) ;
      buf = twist[ n ] ;			/* put twist of tick */
      gdsd_wdble_c( irds , tofchar( "TWIST" ) , &sslevel , &buf , &error ) ;
   }
}


/* irds_rd_skypos.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_rd_skypos.dc2

Function:     irds_rd_skypos

Purpose:      To read skypositions for satcal ticks from an IRDS

Category:     IR

File:         irds_samples.c

Author:       P.R. Roelfsema

Use:          IRDS_RD_SKYPOS(  IRDS   ,    Input   character*(*)
                               SNIP   ,    Input   integer
                               TICK   ,    Input   integer
                               LON    ,    Output  double( >=NSATS )
                               LAT    ,    Output  double( >=NSATS )
                               TWIST  ,    Output  double( >=NSATS )
                               NSATS  ,    Input   integer
                               STATUS )    Output  integer
                                       
              IRDS        Name of IRDS to read from.
              SNIP        Sequential snip number to read.
              TICK        Sequential tick of first sample to read.
              LON         Array to receive LON coordinates
              LAT         Array to receive LAT coordinates
              TWIST       Array to receive TWIST angles (ccw w.r.t. +LAT)
              NSATS       Number of satcal ticks  to read.
              STATUS      Error return code:
                           0  - no error.
                          -1  - IRDS does not exist
                          -2  - IRDS is not a legal irds
                          -3  - SNIP not in IRDS
                          -5  - TICK or TICK + NSATS not in IRDS
                          -7  - no coordinate info in header

Updates:      Aug 20, 1990: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irds_rd_skypos( character , integer   ,
@                            integer          , double precision ,
@                            double precision , double precision ,
@                            integer          , integer )

*/

void irds_rd_skypos_c(  fchar irds    , fint  *snip  
                      , fint  *tick , double *lon    , double *lat
                      , double *twist , fint   *nsats  , fint  *status )
{  
   fint  error = 0 ;
   fint  axis ;
   fint  slevel ,sslevel ;
   double buf = 0 ;
   fint  sattick , sdet = 1 ;
   fint  n ;

   if ( ( *status = check_irds( irds , snip , &sdet , tick ) ) != 0 ) return ;

   sattick  = *tick + *nsats - 1 ;		/* last tick to write to */
   if ( ( *status = check_irds( irds , snip , &sdet , &sattick ) ) != 0 ) return ;

   slevel   = 0 ;
   axis     = 4 ;				/* snip level */
   slevel   = gdsc_word_c( irds , &axis , snip ,  &slevel  , &error ) ;
   axis     = 2 ;				/* snip-tick level */
   sslevel  = gdsc_word_c( irds , &axis , tick ,  &sslevel , &error ) ;

   gdsd_rdble_c( irds , tofchar( "LON" ) , &sslevel , &buf , &error ) ;
   if ( error == sslevel ) {			/* not absolute coordinates */
      *status = NOCOORDS ;			/* bad coordinates -> return */
      return ;
   }

   for ( n = 0 ; n < *nsats ; n++ ) {		/* loop on ticks */
      sattick  = *tick + n ;
      axis     = 2 ;				/* snip-det-tick level */
      sslevel  = gdsc_word_c( irds , &axis , &sattick ,  &sslevel , &error ) ;
      gdsd_rdble_c( irds , tofchar( "LON" )   , &sslevel , &buf , &error ) ;
      lon[ n ] = buf ;				/* get longitude of tick */
      gdsd_rdble_c( irds , tofchar( "LAT" )   , &sslevel , &buf , &error ) ;
      lat[ n ] = buf ;				/* get latitude of tick */
      gdsd_rdble_c( irds , tofchar( "TWIST" ) , &sslevel , &buf , &error ) ;
      twist[ n ] = buf ;			/* get twist of tick */
   }
}


/* irds_rd_sampt.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_rd_sampt.dc2

Function:     irds_rd_sampt

Purpose:      To read raw samples and timing from an IRDS

Category:     IR

File:         irds_samples.c

Author:       P.R. Roelfsema

Use:          IRDS_RD_SAMPT(   IRDS   ,    Input   character*(*)
                               SNIP   ,    Input   integer
                               SDET   ,    Input   integer
                               TICK   ,    Input   integer
                               DATA   ,    Output  real( >=NDATA )
                               TIME   ,    Output  real( >=NDATA )
                               NDATA  ,    In/Out  integer
                               STATUS )    Output  integer
                                       
              IRDS        Name of IRDS to read from.
              SNIP        Sequential snip number to read.
              SDET        Sequential detector number to read.
              TICK        Sequential tick of first sample to read.
              DATA        Data array to recaive samples.
              TIME        Time array to receive sample timing data.
              NDATA       I - max number of samples to write.
                          O - number of samples actually written.
              STATUS      Error return code:
                           0  - no error.
                          -1  - IRDS does not exist
                          -2  - IRDS is not a legal irds
                          -3  - SNIP not in IRDS
                          -4  - SDET not in IRDS
                          -5  - TICK not in IRDS
                          -6  - gds read error

Updates:      Aug 10, 1990: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irds_rd_sampt( character , integer , integer , 
@                           integer   , real    , real    , integer , integer )

*/

void irds_rd_sampt_c( fchar irds    , fint *snip  , fint *sdet  , fint *tick 
                      , float *data , float *time , fint *ndata , fint *status )
{  
   fint  error = 0 ;
   fint  axis ;
   fint  slevel, sdlevel ;
   float timeoff = 0 ;
   fint  detector ;
   fint  nrsamps  ;
   fint  snipcal ;
   fint  n ;

   irds_rd_samples_c( irds  , snip  , sdet , tick , data , ndata , status ) ;
   if ( *status != 0 ) return ;			/* read problem -> return */

   axis     = 1 ;
   nrsamps  = gdsc_size_c( irds , &axis , &error ) ;	/* get samples/tick */
   slevel   = 0 ;
   axis     = 4 ;					/* snip level */
   slevel   = gdsc_word_c( irds , &axis , snip   ,  &slevel  , &error ) ;
   axis     = 3 ;					/* snip-det level */
   sdlevel  = gdsc_word_c( irds , &axis , sdet  ,  &slevel  , &error ) ;

   gdsd_rint_c( irds , tofchar( "SNIPCAL" ) , &slevel  , &snipcal  , &error ) ;
   gdsd_rint_c( irds , tofchar( "DETNO" )   , &sdlevel , &detector , &error ) ;
   timeoff = ircc_times_c( &detector ) ;		/* get readout time */

   for ( n = 0 ; n < *ndata ; n++ ) {			/* calculate times */
      time[ n ] = timeoff + snipcal + n / nrsamps ;
   }

}



/* irds_rd_samplb.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_rd_samplb.dc2

Function:     irds_rd_samplb

Purpose:      To read raw samples and sky positions data from an IRDS

Category:     IR

File:         irds_samples.c

Author:       P.R. Roelfsema

Use:          IRDS_RD_SAMPLB(  IRDS   ,    Input   character*(*)
                               SNIP   ,    Input   integer
                               SDET   ,    Input   integer
                               TICK   ,    Input   integer
                               DATA   ,    Output  real( >=NDATA )
                               LON    ,    Output  double( >=NDATA )
                               LAT    ,    Output  double( >=NDATA )
                               TWIST  ,    Output  double( >=NDATA )
                               NDATA  ,    In/Out  integer
                               STATUS )    Output  integer
                                       
              IRDS        Name of IRDS to read from.
              SNIP        Sequential snip number to read.
              SDET        Sequential detector number to read.
              TICK        Sequential tick of first sample to read.
              DATA        Data array to recaive samples.
              LON         Array to receive LON coordinates
              LAT         Array to receive LAT coordinates
              TWIST       Array to receive TWIST angles (ccw w.r.t. +LAT)
              NDATA       I - max number of samples to write.
                          O - number of samples actually written.
              STATUS      Error return code:
                           0  - no error.
                          -1  - IRDS does not exist
                          -2  - IRDS is not a legal irds
                          -3  - SNIP not in IRDS
                          -4  - SDET not in IRDS
                          -5  - TICK not in IRDS
                          -6  - gds read error
                          -7  - no coordinate info in header

Updates:      Aug 20, 1990: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irds_rd_samplb( character , integer   , integer   ,
@                            integer          , real      , 
@                            double precision , double precision ,
@                            double precision , 
@                            integer          , integer )

*/

void irds_rd_samplb_c(  fchar irds    , fint  *snip   , fint  *sdet 
                      , fint  *tick   , float *data   , double *lon    
                      , double *lat   , double *twist  
                      , fint  *ndata  , fint  *status )
{  
   fint nrsamps ;
   fint ticks ;
   fint axis ;  
   fint error = 0 ;

   irds_rd_samples_c( irds  , snip  , sdet , tick , data , ndata , status ) ;
   if ( *status != 0 ) return ;			/* read problem -> return */

   axis     = 1 ;
   nrsamps  = gdsc_size_c( irds , &axis , &error ) ;	/* nr samples/tick */
   ticks    = *ndata / nrsamps ;			/* nr of ticks */

   irds_rd_skypos_c( irds , snip , tick
                   , lon , lat , twist , &ticks , status ) ;
   if ( *status != 0 ) return ;			/* read problem -> return */

   interpol( lon   , ndata , &nrsamps );	/* interpolate latitudes */
   interpol( lat   , ndata , &nrsamps );	/* interpolate longitudes */
   interpol( twist , ndata , &nrsamps );	/* interpolate twists */
}



/* irds_rd_sampxy.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_rd_sampxy.dc2

Function:     irds_rd_sampxy

Purpose:      To read raw samples and projected positions from an IRDS

Category:     IR

File:         irds_samples.c

Author:       P.R. Roelfsema

Use:          IRDS_RD_SAMPXY(  IRDS   ,    Input   character*(*)
                               SNIP   ,    Input   integer
                               SDET   ,    Input   integer
                               TICK   ,    Input   integer
                               DATA   ,    Output  real( >=NDATA )
                               X      ,    Output  double( >=NDATA )
                               Y      ,    Output  double( >=NDATA )
                               NDATA  ,    In/Out  integer
                               STATUS )    Output  integer
                                       
              IRDS        Name of IRDS to read from.
              SNIP        Sequential snip number to read.
              SDET        Sequential detector number to read.
              TICK        Sequential tick of first sample to read.
              DATA        Data array to receive samples.
              X           Array to receive X coordinates
              Y           Array to receive Y coordinates
              NDATA       I - max number of samples to write.
                          O - number of samples actually written.
              STATUS      Error return code:
                           0  - no error.
                          -1  - IRDS does not exist
                          -2  - IRDS is not a legal irds
                          -3  - SNIP not in IRDS
                          -4  - SDET not in IRDS
                          -5  - TICK not in IRDS
                          -6  - gds read error
                          -7  - no coordinate info in header
                         -10  - not yet implemented

Updates:      Aug 20, 1990: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irds_rd_sampxy( character , integer   , integer   ,
@                            integer          , real      , 
@                            double precision , double precision ,
@                            integer          , integer )

*/

void irds_rd_sampxy_c(  fchar irds    , fint  *snip   , fint  *sdet 
                      , fint  *tick   , float *data   , double *x
                      , double *y     , fint  *ndata  , fint  *status )
{  
   fint nrsamps ;
   fint ticks ;  
   fint axis ;
   fint error = 0 ;

   irds_rd_samples_c( irds  , snip  , sdet , tick , data , ndata , status ) ;
   if ( *status != 0 ) return ;			/* read problem -> return */

   axis     = 1 ;
   nrsamps  = gdsc_size_c( irds , &axis , &error ) ;	/* nr samples/tick */
   ticks    = *ndata / nrsamps ;			/* nr of ticks */

/* here the BPHF must be read and subsequently  a coordinate transform 
should be carried out for each tick.... */

/* since this is not ready => exit with error -10 */
   *status = NOT_YET_IMPLEMENTED ;

   if ( *status != 0 ) return ;			/* read problem -> return */

   interpol( x , ndata , &nrsamps );		/* interpolate X coordinates */
   interpol( y , ndata , &nrsamps );		/* interpolate Y coordinates */
}






