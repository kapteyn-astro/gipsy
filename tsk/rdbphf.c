/* identification */
#define VERSION           "1.3"			/* version number */
#define PROGRAM		"RDBPHF"		/* program name */

/* rdbphf.c

                            COPYRIGHT (c) 1990
            Kapteyn Astronomical Institute - University of Groningen
                P.O. Box 800, 9700 AV Groningen, The Netherlands

#>            rdbphf.dc1

Program:      RDBPHF

Purpose:      Add BPHF information to an existing IRDS

Category:     IRAS

File:         rdbphf.c

Author:       P.R. Roelfsema

Keywords:

    IRSET=    Name of input IR data set [quit]

***   DIR=    Give directory containing BPHF for sop/att xxx/xxx [skip]

              If an alternative set of BPHF files are to be used
              the user can specify a directory (DIR=) where they can 
              be found. In this case the program will ask a file number
              for each file to be read from this directory.

              FILE= Give filenumber for sop/att xxx/xxx [1]

              If a file specified like this does not contain
              the BPHF for the given sop/att, the program will continue
              to the next snip.

***  FULL=    Full error information ? [ N ]


Description:

                RDBPHF reads data from the Super Boresight Pointing
              History Files and adds these data to the IRDS specified
              with IRSET=. For each satcal tick for all sinps in the
              IRDS the positioning information longitude, latitude and
              twist angle between SRLAT north and telescope Z-axis
              (all in the sunreferenced ccordinate system relevant
              for the snip) are added to the header (descriptor items
              SRLAT, SRLON and TWIST).
                For each snip also the longitude (LNGSUN) of the sun at the
              beginning of the snip (at epoch 1983.5) and the rate of change
              of the solar longitude (SUNRATE) are added to the header.
                The error information (errors in SRLON and SRLAT, descriptors
              SIGSRLON and SIGSRLAT respectively) can be added to the IRDS
              header in two ways; only the average errors over each snip
              (FULL=N, the default), or for each tick the appropriate
              error (FULL=Y). Note that using FULL=Y will increase the
              size of the header by approximately 40%.
                 After RDBPHF (with FULL=N) the IRDS header will be very BIG!
              For e.g. band 1,2 and LRS data the header will have the same
              size as the image data.

Updates:      Sep  4, 1990: PRR, Document created.
              Nov 15, 1990: PRR, Version 1.0 in system.
              Mar 19, 1992: HB,  New interface to irds_enquire_snip
              Sep 11, 1992: PRR, Added DIR=/FILE=
#<

*/

#include "gipsyc.h"
#include "cmain.h"
#include "stdio.h"
#include "string.h"
#include "ctype.h"
#include "stdlib.h"
#include "math.h"
#include "nelc.h"
#include "init.h"
#include "finis.h"
#include "anyout.h"
#include "status.h"
#include "cancel.h"
#include "error.h"
#include "irds_exist.h"
#include "irds_enquire.h"
#include "irds_enquire_snip.h"
#include "irds_wr_bphf.h"
#include "gdsd_rchar.h"
#include "userint.h"
#include "userlog.h"
#include "userreal.h"
#include "usertext.h"
#include "irtp_sa2bphf.h"
#include "ftsd_geth.h"
#include "ftsi_geti.h"
#include "ftsd_rreal.h"
#include "ftsd_rdble.h"
#include "ftsd_rint.h"
#include "mtopen.h"
#include "mtfsf.h"
#include "mtclose.h"


/* definitions for error levels */

/*static  fint            error_level_fatal      =   4;
#define FATAL_ERROR     ( &error_level_fatal   )
static  fint            error_level_serious    =   3;
#define SERIOUS_ERROR   ( &error_level_serious )
static  fint            error_level_minor      =   2;
#define MINOR_ERROR     ( &error_level_minor   )*/
static  fint            error_level_warning    =   1;
#define WARNING         ( &error_level_warning )

/* definitions for anyout levels */

static  fint            anyout_level_default   =  0 ;
#define ANYOUT_DEF      ( &anyout_level_default   )
/*static  fint            anyout_level_terminal  =  1 ;
#define ANYOUT_TERM     ( &anyout_level_terminal  )
static  fint            anyout_level_logfile   =  2 ;
#define ANYOUT_LOG      ( &anyout_level_logfile   )
static  fint            anyout_level_dumb_user =  8 ;
#define ANYOUT_NOEXP    ( &anyout_level_dumb_user )*/
static  fint            anyout_level_test      = 16 ;
#define ANYOUT_TST      ( &anyout_level_test      )

/* definitions for default levels */

/*static  fint            default_no_default     =  0 ;
#define DFLT_NONE       ( &default_no_default  )*/
static  fint            default_has_default    =  1 ;
#define DFLT_DEF	( &default_has_default )
static  fint            default_hidden_key     =  2 ;
#define DFLT_HIDD       ( &default_hidden_key  )
/*static  fint            default_exact_number   =  4 ;
#define DFLT_EXACT      ( &default_exact_number)*/


#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.l = len ; }


/* keywords and USER*** message strings */
#define INSET_KEY	tofchar("IRSET=")
#define INSET_MES	tofchar("Give input IR data set [quit]")
#define DIR_KEY		tofchar("DIR=")
#define FILE_KEY	tofchar("FILE=")
#define FULL_KEY	tofchar("FULL=")
#define	FULL_MES	tofchar("Do you want full error information? [ N ]")

/* miscellaneous definitions */
#define true               1
#define false              0
#define MAXTXTLEN        160			/* length of textlines */
#define MAXTICKS	5000			/* max nr of ticks to process */
#define HEDLEN		5000			/* length of BPHF tape header */
#define BUFWIDTH	   4			/* read 4 data items per satcal */
#define SCANLEN		5000			/* max length of scan in satcals */
#define	RDBLOCK		( SCANLEN * BUFWIDTH )	/* size of a block to read */
#define NPARAMS		  13			/* nr of params for BPHF conversion */
#define	PI		3.14159265358979	/* Value of pi	*/



/*
   GetLRSSet asks the user for an IRDS to process. The routine checks whether
the IRDS exists and whether it is an LRS data set. If so control returns to
the caller, if not the user is prompted for a new IRSD name.
If <CR> is given to the keyword the routine returns NULL indicating that
the user wants to terminate the program.
*/

fint GetSet( fchar setname )
{
   char      line[MAXTXTLEN]   ;

   fint      ierr   = 0 ;
   fint      nitems = 0 ;

   int       found  = 0 ;
   int       done   = 0 ;

   anyout_c( ANYOUT_TST , tofchar(" - GetSet") ) ;

   while ( !done && !found ) {
      nitems = usertext_c( setname, DFLT_DEF, INSET_KEY, INSET_MES );/*get set*/
      done = ( nitems == 0 ) ; 				/* did user type CR? */
      if( !done ) {
         found = irds_exist_c( setname , &ierr );	/* does INSET exist? */
         if( found != 0 ) {				/* INSET is not Irds */
            (void) sprintf( line , "IRSET %.*s is not a good IR data set (%d)" ,
                                 (int) nelc_c( setname ) , setname.a , found );
            error_c( WARNING , tofchar( line ) );	/* tell user */
            cancel_c( INSET_KEY );			/* give another chance */
            found = false ;
         } else {  					/* INSET does exist */
            found = true ;
         }
      }
   }
   return( done ) ;
}						/* GetSet */


/*
   The routine GetTape tries to get a tape name for the BPHF tape, and
it subsequently tries to open it. Once opened it will skip to the
fits file on that tape which actually contains the BPHF data.
*/

fint GetTape( fint sop , fint att )
{
   fint  mtid = -1 ;
   fint  fileno = 0 ;
   fint	 nret = 0 ;
   fint  one = 1 ;
   fchar directory ;
   char  line[ MAXTXTLEN ] ;

   finit( directory , MAXTXTLEN ) ;

   for( nret = 0 ; nret < MAXTXTLEN ; nret++ ) {
      directory.a[nret] = ' ' ;
   }
   sprintf( line , "Give directory containing BPHF for sop/att %d/%d [skip]" ,
                    sop , att ) ;
   nret = usertext_c( directory , DFLT_HIDD , DIR_KEY , tofchar( line ) ) ;
   if ( nelc_c( directory ) == 0 ) {
      if ( irtp_sa2bphf_c( &sop , &att , directory , &fileno ) < 0 ) {
         sprintf( line , "Could not find tape for sop/att %d/%d, will skip snip",
                          sop , att );
         error_c( WARNING , tofchar( line ) ) ;
         return( mtid ) ;
      }							/* irtp_sa2bphf */
   } else {						/* nelc( dir ) */
      fileno = 1 ;
      sprintf( line , "Give filenr. in %.*s of BPHF for sop/att %d/%d [1]" ,
                   nelc_c( directory ) , directory.a , sop , att ) ;
      nret = userint_c( &fileno , &one , DFLT_DEF , FILE_KEY , tofchar( line ) ) ;
      cancel_c( FILE_KEY ) ;				/* cancel it */
   } 							/* nret != 1 */
   	
   sprintf( line ,  "Will try to read BPHF from %.*s, file %d"
         , nelc_c( directory ) , directory.a , fileno ) ;
   anyout_c( ANYOUT_TST , tofchar( line ) ) ;
   if ( ( mtid = mtopen_c( directory ) ) >= 0 ) {
      fileno = fileno - 1 ;
      if ( mtfsf_c( &mtid , &fileno ) != fileno ) {
         mtclose_c( &mtid ) ;
         mtid = -1 ;
      }							/* mtfsfs */
      sprintf( line , "Skipped %d files on tape %.*s (mtid %d)" ,
                       fileno , nelc_c( directory ) , directory.a , mtid ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;
   } else {						/* mtopen */
      sprintf( line , "Could not open tape %.*s, will skip snip",
                      nelc_c( directory ) , directory.a );
      error_c( WARNING , tofchar( line ) ) ;
   }							/* mtopen */
   return( mtid ) ;
}


/*
    CnvrtValues converts the values read from tape to srlon, srlat etc..
*/
void CnvrtValues( fchar  bphfhedr , fint   *data   , fint endpix ,
                  double *srlon   , double *esrlon ,
                  double *srlat   , double *esrlat ,
                  double *twist   , double *lngsun , double *sunrate )
{
   fint   error = 0 ;
   fint   i    = 0 ;
   fint   code = 0 ;
   fint   naxis2 = 0 ;
   double bscale = 1 ;

   error = ftsd_rint_c(  bphfhedr , tofchar( "NAXIS2" ) , &naxis2 ) ;
   error = ftsd_rdble_c( bphfhedr , tofchar( "BSCALE" ) , &bscale ) ;
   error = ftsd_rdble_c( bphfhedr , tofchar( "PSI-0" )  , &srlon[ 0 ] ) ;
   srlon[ 0 ] = srlon[ 0 ] / ( bscale * 1000000 ) ;
   error = ftsd_rdble_c( bphfhedr , tofchar( "THETA-0" )  , &srlat[ 0 ] ) ;
   srlat[ 0 ] = ( 90 - srlat[ 0 ] ) / ( bscale * 1000000 ) ;

   twist[ 0 ]  = ( (double) data[ 2 ] ) / 1000000 ;
   code        = data[ 3 ] + 32678 ;
   esrlon[ 0 ] = pow( 10 , ( (float) ( code/256 ) ) / 64 ) / 1000000 ;
   esrlat[ 0 ] = pow( 10 , ( (float) ( code%256 ) ) / 64 ) / 1000000 ;

   for ( i = 1 ; ( ( i <= endpix ) && ( i < naxis2 ) ) ; i++ ) {
      srlon[ i ]  = srlon[ i - 1 ] + ( (double) data[ i * 4 ] ) / 1000000 ;
      srlat[ i ]  = srlat[ i - 1 ] - ( (double) data[ i * 4 + 1 ] ) / 1000000 ;
      twist[ i ]  =                  ( (double) data[ i * 4 + 2 ] ) / 1000000 ;
      code        =                             data[ i * 4 + 3 ] + 32678 ;
      esrlon[ i ] = pow( 10 , ( (float) ( code/256 ) ) / 64 ) / 1000000 ;
      esrlat[ i ] = pow( 10 , ( (float) ( code%256 ) ) / 64 ) / 1000000 ;
   }

   error = ftsd_rdble_c( bphfhedr , tofchar( "SUNRATE" )  , sunrate ) ;
   *sunrate    = *sunrate / ( bscale * 1000000 ) ;
   error = ftsd_rdble_c( bphfhedr , tofchar( "LNGSUN" )  , lngsun ) ;
   *lngsun    = *lngsun / ( bscale * 1000000 ) ;

}


/*
   The routine Prepare reads the BPHF fits-file header and extracts a
number of items from it. Subsequently it uses thes items together with
the first 13 datanumbers on the tape to fill the scaling array p[0..12]

*/
fint Prepare( fint   mtid    , fint sop       , fint att    
            , fint scancal   , fint snipcal   , fint snipdur
            , fchar bphfhedr , fint *startpix , fint *endpix , fint *tid )
{
   fint    result = false , error = 0 ;
   fint	   file_sop = 0 , file_att = 0 ;

   char    line[ MAXTXTLEN ] ;

   double  crval2 = 0 , crpix2 = 0 , snipstart = 0 ;

   *tid = 0 ;
   if ( ( error = ftsd_geth_c( &mtid , bphfhedr , tid ) ) > 0 ) {
      result = true ;
      error = ftsd_rint_c(  bphfhedr , tofchar( "SOP" )     , &file_sop ) ;
      error = ftsd_rint_c(  bphfhedr , tofchar( "ATT" )     , &file_att ) ;
      if ( ( file_sop == sop ) && ( file_att == att ) ) {
         error = ftsd_rdble_c( bphfhedr , tofchar( "CRVAL2" )  , &crval2 ) ;
         error = ftsd_rdble_c( bphfhedr , tofchar( "CRPIX2" )  , &crpix2 ) ;
         snipstart = crval2 - crpix2 + 0.5 ;
         *startpix = scancal + snipcal - snipstart ;
         *endpix   = scancal + snipcal + snipdur - 1 - snipstart ;

         (void) sprintf( line ,
                         "Input is: scancal %d, snipcal %d, snipdur %d" ,
                         scancal , snipcal , snipdur ) ;
         anyout_c( ANYOUT_TST , tofchar( line ) ) ;
         (void) sprintf( line ,
                         "Prepare gives: scanstart %d, startpix %d, endpix %d" ,
                         (int)( snipstart ) , *startpix , *endpix ) ;
         anyout_c( ANYOUT_TST , tofchar( line ) ) ;
      } else {						/* sop==file etc. */
         sprintf( line , "Fits file has wring sop/att (%d,%d), will skip snip" , file_sop , file_att ) ;
         error_c( WARNING , tofchar( line ) ) ;
      }
   } else {						/* ftsd_geth */
      sprintf( line , "Error %d reading fits header on MT %d, will skip snip" , error , mtid ) ;
      error_c( WARNING , tofchar( line ) ) ;
   }							/* ftsd_geth */
   return( result ) ;
}



MAIN_PROGRAM_ENTRY
{
   char      line[MAXTXTLEN] ;
   char      line2[2*MAXTXTLEN] ;

   fchar     setname , instrument , cosys , object ;
   fchar     bphfhedr , scantype ;

   fint      one = 1 , full = false , i = 0 ;
   fint      done  , status , error , nitems , mtid , tid ;
   fint      naxis , axlen[4] , snip ;
   fint      sop , obs , att , scancal , scandur , snipcal , snipdur ;

   double    center[2] , size [2] ;
   float     ipsi , itheta , ipsirate , epoche ;

   fint      startpix , endpix ;

   /*
    * The following arrays are declared static because
    * of a bug in the gnu c compiler v1.39 on sun4 systems.
    *
    * Apr 17, 1991: KGB
    */
   static	fint      data[ RDBLOCK ] ;

   static	double    srlon[ MAXTICKS ] , esrlon[ MAXTICKS ]   ;
   static	double    srlat[ MAXTICKS ] , esrlat[ MAXTICKS ] ;
   static	double    twist[MAXTICKS ]  ;
   double       lngsun , sunrate ;

   init_c( ); 						/* hello HERMES */
   IDENTIFICATION( PROGRAM , VERSION ) ;		/* who are we */

   finit( setname    , MAXTXTLEN ) ;			/* initialise fchars */
   finit( instrument , MAXTXTLEN ) ;
   finit( cosys      , MAXTXTLEN ) ;
   finit( object     , MAXTXTLEN ) ;
   finit( bphfhedr   , HEDLEN ) ;
   finit( scantype   , 20 ) ;

   done = GetSet( setname ) ;				/* get a setname */

   if ( !done ) {
      nitems = userlog_c( &full , &one , DFLT_HIDD , FULL_KEY , FULL_MES ) ;
      irds_enquire_c( setname , object ,instrument , &naxis  , axlen
                    , center  , size   , cosys     , &epoche , &status ) ;
      (void) sprintf( line , "I`ll add BPHF to IR set %.*s (%.*s)" ,
                             (int) nelc_c( setname ) , setname.a ,
                             (int) nelc_c( object )  , object.a );
      anyout_c( ANYOUT_DEF , tofchar( line ) );		/* tell user */

      for ( snip = 1 ; snip <= axlen[3] ; snip++ ){
         irds_enquire_snip_c( setname  , &snip    , &sop     , &obs     ,
                              &att     , scantype , &scancal , &scandur  , 
                              &snipcal , &snipdur , &ipsi    , &ipsirate , 
                              &itheta  , &status ) ;
         (void) sprintf( line2 , "Snip %5d, sop/att %3d/%-3d, satcal %8d to %-8d" ,
                              snip , sop , att , scancal + snipcal ,
                                       scancal + snipcal + snipdur - 1 ) ;
         anyout_c( ANYOUT_DEF , tofchar( line2 ) );		/* tell user */
         (void) sprintf( line , "Working on IR set %.*s , snip %5d" ,
                                (int) nelc_c( setname ) , setname.a , snip ) ;
         status_c( tofchar( line ) );				/* tell user */
         if ( ( mtid = GetTape( sop , att ) ) >= 0 ){
            if ( Prepare( mtid, sop, att, scancal, snipcal, snipdur, bphfhedr , &startpix, &endpix , &tid ) ) {
               nitems = RDBLOCK ;
               nitems = ftsi_geti_c( &mtid , data , &nitems , &tid ) ;
               if ( nitems >= 0 ) {
                  CnvrtValues( bphfhedr , data   , endpix , srlon   , esrlon,
                               srlat    , esrlat , twist  , &lngsun , &sunrate ) ;
                  if ( !full ) {
                     anyout_c( ANYOUT_TST , tofchar( "Calculating mean errors" ) ) ;
                     for ( i = startpix + 1 ; i <= endpix ; i++ ) {
                        esrlon[ startpix ] = esrlon[ startpix ] + esrlon[ i ] ;
                        esrlat[ startpix ] = esrlat[ startpix ] + esrlat[ i ] ;
                     }
                     esrlon[ startpix ]     = esrlon[ startpix ] / snipdur ;
                     esrlat[ startpix ]     = esrlat[ startpix ] / snipdur ;
                     esrlon[ startpix + 1 ] = -1 ;
                     esrlat[ startpix + 1 ] = -1 ;
                  }
                  lngsun = lngsun + startpix * sunrate ;
                  (void) sprintf( line , "Will write SBPHF data for %d satcals to set %*.s" ,
                         snipdur , (int) nelc_c( setname ), setname.a ) ;
                  anyout_c( ANYOUT_TST , tofchar( line ) ) ;
                  irds_wr_bphf_c( setname   , &snip    , &one ,
                                  &srlon[ startpix ]   ,
                                  &esrlon[ startpix ]  ,
                                  &srlat[ startpix ]   ,
                                  &esrlat[ startpix ]  ,
                                  &twist[ startpix ]   ,
                                  &lngsun   , &sunrate ,
                                  &snipdur  , &status  ) ;
               } else {					/* if ( nitems >= 0 ) */
                  sprintf( line , "Error %d while reading fits data, will skip snip" , nitems ) ;
                  error_c( WARNING , tofchar( line ) ) ;
               }					/* if ( nitems >= 0 ) */
            }						/* if ( Prepare ) */
            anyout_c( ANYOUT_TST , tofchar( "Will close tape" ) ) ;
            error = mtclose_c( &mtid ) ;
            sprintf( line , "MTCLOSE returned %d (mtid %d)" , error , mtid ) ;
            anyout_c( ANYOUT_TST , tofchar( line ) ) ;
         }						/* if ( GetTape ) */
      }							/* for ( snips ) */
   }							/* if ( !done ) */
   finis_c( );						/* bye, bye HERMES */
   return(0);
}


