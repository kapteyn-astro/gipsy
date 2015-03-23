/* irset.c

                            COPYRIGHT (c) 1990
            Kapteyn Astronomical Institute - University of Groningen
                P.O. Box 800, 9700 AV Groningen, The Netherlands

#>            irset.dc1

Program:      IRSET

Purpose:      Generate a test IRDS

Category:     IRAS

File:         irset.c

Author:       P.R. Roelfsema

Description:

      IRSET allows a user to construct an Infra Red Data Structure (IRDS)
    within GIPSY for test purposes. This is done by asking the user a series
    of keywords which specify e.g. the IRAS observing mode, coordinate 
    systems etc. etc. Subsequently the structure is created and dummy data
    are put into it.

Keywords:

    IRSETOUT= Name of output IR data set [no output]

    INSTRUME= Instrument name for IR set [ SURVEY B1 ]
              Can be any combination of
              SURVEY, SPLINE, AO or FLASH with
              B1, B2, B3, B4, LRS or BPHF separated
              by a space.

    SKYSYS=   Custom plate coordinate system [ EQUATORIAL ]

    EPOCH=    Epoch of the coordinate system [ 1983.5 ]

    CENTER=   Custom plate center position in degrees [ 0,0 ]

    SIZE=     Custom plate sizes in degrees [ 1,1 ]

    OBJECT=   Name of object in custom plate [ name of IR set ]

    OBSERVER= Name of observer [ ??? ]

    MAXTICKS= Maximum number of satcal ticks [ max number in plate ]

    NSNIPS=   Number of snips wanted in the plate [ 1 ]

           Subsequently for each snip n ( n running from 0 to NSNIPS-1 )
           snip parameters are being asked:

    SOPn=     SOP number for snip n [ n + 30 ]

    ATTn=     ATT number for snip n [ n + 1  ]

    SCANCALn= Start satcal of original scan of snip n [ n * 3000 ]

    SCANDURn= Length in satcals of original scan of snip n [ 3000 ]

    SNIPCALn= Start satcal of snip n [ SCANCALn + 500 ]

    SNIPDURn= Length in satcals of snip n [ min(MAXTICKS,32) ]

    PSIn=     PSI at begin of snip n [ 0.1 * n radians ]

    PSIRATEn= PSIRATE at begin of snip n [ 0.00001 radians/tick ]

    THETAn=   THETA at begin of snip n [ 0.05 * n radians ]


Updates:      Aug 8, 1990: PRR, Document created.
              Feb 19,1992: HB/FL New interface to irds_extend
              Dec  1,1992: VOG  Category added

#<

*/



#include "gipsyc.h"
#include "math.h"
#include "stdio.h"
#include "string.h"
#include "ctype.h"
#include "stdlib.h"
#include "cmain.h"
#include "nelc.h"
#include "init.h"
#include "finis.h"
#include "anyout.h"
#include "status.h"
#include "cancel.h"
#include "error.h"
#include "gds_exist.h"
#include "gdsd_rint.h"
#include "irds_close.h"
#include "irds_create.h"
#include "irds_delete.h"
#include "irds_extend.h"
#include "irds_exist.h"
#include "ircc_rate.h"
#include "ircc_ndets.h"
#include "ircc_bandnr.h"
#include "ircc_obsmode.h"
#include "irds_wr_samples.h"
#include "userint.h"
#include "userlog.h"
#include "userreal.h"
#include "userdble.h"
#include "usertext.h"

#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.l = len ; }


/* definitions for error levels */

static  fint            error_level_fatal      =   4;
#define FATAL_ERROR     ( &error_level_fatal   )
static  fint            error_level_serious    =   3;
#define SERIOUS_ERROR   ( &error_level_serious )
/*static  fint            error_level_minor      =   2;
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

static  fint            default_no_default     =  0 ;
#define DFLT_NONE       ( &default_no_default  )
static  fint            default_has_default    =  1 ;
#define DFLT_DEF	( &default_has_default )
/*static  fint            default_hidden_key     =  2 ;
#define DFLT_HIDD       ( &default_hidden_key  )*/
static  fint            default_exact_number   =  4 ;
#define DFLT_EXACT      ( &default_exact_number)


/* identification */
#define IDENT           tofchar("IRSET  Version 1.1  September 21, 1990")

/* keywords and USER*** message strings */
#define OUTSET_KEY	tofchar("IRSETOUT=")
#define OUTSET_MES	tofchar("Give output IR data set [no output set]")
#define CENTER_KEY	tofchar("CENTER=")
#define CENTER_MES	tofchar("Give custom plate center position (deg) [0,0]")
#define SIZE_KEY	tofchar("SIZE=")
#define SIZE_MES	tofchar("Give custom plate sizes (deg) [1,1]")
#define SKYSYS_KEY	tofchar("SKYSYS=")
#define SKYSYS_MES	tofchar("Give custom plate coordinate system [EQ]")
#define EPOCH_KEY	tofchar("EPOCH=")
#define EPOCH_MES	tofchar("Give epoch of coordinate system [1983.5]")
#define OBJECT_KEY	tofchar("OBJECT=")
#define OBJECT_MES	tofchar("Give name of object [ name of IRDS ]")
#define OBS_KEY		tofchar("OBSERVER=")
#define OBS_MES		tofchar("Give name of observer [ Piet Snot ]")
#define INSTR_KEY	tofchar("INSTRUME=")
#define INSTR_MES	tofchar("Give instrument name for IRDS [ SURVEY B1 ]")
#define MXSATS_KEY	tofchar("MAXTICKS=")
#define NSNIPS_KEY	tofchar("NSNIPS=")

/* miscellaneous definitions */
#define true              1
#define false             0
#define MAXTXTLEN        80			/* length of textlines */
#define MAXSAMPLES    10000			/* maximum nr of samples */



/*
   MakeOutset creates an output set.
*/

int MakeOutset( fchar setname  )
{
   char      line[MAXTXTLEN]  ;
   char      key[MAXTXTLEN]   ;

   fint      ierr   = 0 ;
   fint      nitems = 0 ;
   fint      n ;
   fint      one = 1 , two = 2 ;

   fchar     skysys , object , observer , instrume , scantype;

   int       found  = 0 ;
   int       done   = 0 ;

   double    lonlat[2] ;
   double    sizes[2] ;
   fint      axes[3] ;
   fint      sop, obs, att ;
   fint      scancal, scandur, snipcal, snipdur , maxtck ;
   float     psi, psirate, theta , epoche ;
   fint      nrsnips ;
   


   anyout_c( ANYOUT_TST , tofchar(" - MakeLRSOutset") ) ;
   finit( instrume , MAXTXTLEN ) ;
   finit( skysys   , MAXTXTLEN ) ;
   finit( object   , MAXTXTLEN ) ;
   finit( observer , MAXTXTLEN ) ;
   finit( scantype , 20 ) ;



   nitems = usertext_c( setname, DFLT_NONE, OUTSET_KEY, OUTSET_MES );/*get set*/
   done = ( nitems == 0 ) ; 			/* did user type CR? */
   if( !done ) {
      found = gds_exist_c( setname , &ierr );	/* does INSET exist? */
      if( found ){				/* INSET does exist */
         (void) sprintf( line , "IRSETOUT %.*s already exists, will be deleted " , 
                                 (int) nelc_c( setname ) , setname.a );
         error_c( WARNING , tofchar( line ) );	/* tell user */
         irds_delete_c( setname , &ierr );
         if ( ierr != 0 ){
            sprintf( line , "Tough, IRDS_DELETE error nr. %d !" , ierr ) ;
            error_c( FATAL_ERROR , tofchar( line ) ) ;
         }
      }



/*

   Now we have the output setname which is to be created, so we'll start
asking all the questions to define the base IRDS.

*/

      while( true ) {
         nitems = usertext_c(instrume,      DFLT_DEF  ,INSTR_KEY ,INSTR_MES ) ; 
         if ( nitems <= 0 ) instrume.a= strcpy( instrume.a , "SURVEY B1" )   ;
         if ( ircc_bandnr_c( instrume ) && ircc_obsmode_c( instrume ) ) break ;
         sprintf( line , "I don't know the instrument %.*s, please retry" ,
                                 (int) nelc_c( instrume ) , instrume.a  );
         error_c( WARNING , tofchar( line ) ) ;
         cancel_c( INSTR_KEY ) ;
      }
      nitems = usertext_c(skysys,        DFLT_DEF  ,SKYSYS_KEY,SKYSYS_MES) ;
      if ( nitems <= 0 ) skysys.a  = strcpy( skysys.a   , "EQUATORIAL" ) ;
      nitems = userreal_c(&epoche,&one  ,DFLT_EXACT,EPOCH_KEY ,EPOCH_MES ) ;
      if ( nitems != 1 ) epoche    = 1983.5 ;
      nitems = userdble_c(lonlat,&two   ,DFLT_EXACT,CENTER_KEY,CENTER_MES) ;
      if ( nitems != 2 ) { lonlat[0] = 0 ; lonlat[1] = 0 ; }
      nitems = userdble_c(sizes ,&two   ,DFLT_EXACT,SIZE_KEY  ,SIZE_MES  ) ;
      if ( nitems != 2 ) { sizes[0]  = 1 ; sizes[1]  = 1 ; }
      nitems = usertext_c(object,        DFLT_DEF  ,OBJECT_KEY,OBJECT_MES) ;
      if ( nitems <= 0 ) object.a = strncpy( object.a, setname.a, setname.l ) ;
      nitems = usertext_c(observer,      DFLT_DEF  ,OBS_KEY   ,OBS_MES   ) ;
      if ( nitems <= 0 ) observer.a= strcpy( observer.a , "Piet Snot" )  ;

      maxtck = 1.1 * sqrt( sizes[0] * sizes[0] + sizes[1] * sizes[1] ) * 60 / 3.85 ;
      sprintf( line , "Give maximum number of satcals in %.*s [%d]" ,
                                 (int) nelc_c( setname ) , setname.a , maxtck );
      nitems = userint_c(&maxtck,&nitems,DFLT_DEF,MXSATS_KEY,tofchar(line) ) ;
      if ( maxtck < 0 ) maxtck = 16 ;
      if ( maxtck > MAXSAMPLES ) {
         sprintf( line, "Cannot generate more than %d samples !", MAXSAMPLES ) ;
         error_c( FATAL_ERROR , tofchar( line ) ) ;
      }

      axes[0] = ircc_rate_c( instrume );	/* samples per satcal */
      axes[1] = maxtck ;			/* nr of satcals */
      axes[2] = ircc_ndets_c( instrume );	/* nr of detectors */
/*
   Go ahead and create the set
*/
      irds_create_c( setname ,			/* create setname */
                     instrume ,			/* LRS set */
                     axes ,			/* the axes sizes */
                     lonlat ,			/* the center */
                     sizes , 			/* size of the plate */
                     skysys ,			/* coordinate system */
                     &epoche ,			/* epoche of coordinates */
                     object ,			/* LRS test data */
                     observer,			/* from me */
                     &ierr ) ; 			/* error return */
      if ( ierr != 0 ){
         sprintf( line , "Tough, IRDS_CREATE error nr. %d !" , ierr ) ;
         error_c( FATAL_ERROR , tofchar( line ) ) ;
      }


/*
   The output base IRDS is created, now it must be extended with a series of
of snips (nrsnips to be set by the user). For each snip again a number of 
keywords are asked regarding the properties of that snip.
*/
      nrsnips = 1 ;
      sprintf( line , "Give maximum number of snips in %.*s [%d]" ,
                                 (int) nelc_c( setname ) , setname.a , nrsnips );
      nitems = userint_c(&nrsnips,&one,DFLT_DEF,NSNIPS_KEY,tofchar(line) ) ;
      if ( nitems != 1 ) nrsnips = 1 ;

      for ( n = 0 ; n < nrsnips ; n++ ) {
         sop     = n + 30 ;
         sprintf( key  , "SOP%d=" , n ) ;
         sprintf( line , "Give SOP for snip nr %d [ %d ] " , n , sop ) ;
         nitems  = userint_c(&sop ,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;
         obs = 0 ;
         sprintf( key  , "OBS%d=" , n ) ;
         sprintf( line , "Give OBS for snip nr %d [ %d ] " , n , obs ) ;
         nitems  = userint_c(&obs ,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;
         att     = n + 1  ;
         sprintf( key  , "ATT%d=" , n ) ;
         sprintf( line , "Give ATT for snip nr %d [ %d ] " , n , att ) ;
         nitems  = userint_c(&att ,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;
         
         strncpy( scantype.a, " ", 1 ) ;
         sprintf( key  , "SCANTYPE%d=" , n ) ;
         sprintf( line , "Give SCANTYPE of original scan for snip nr %d [ %.1s ] " , n , scantype.a ) ;
         nitems  = userchar_c(scantype,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;
         scancal = n * 3000 ;
         sprintf( key  , "SCANCAL%d=" , n ) ;
         sprintf( line , "Give start SATCAL of original scan for snip nr %d [ %d ] " , n , scancal ) ;
         nitems  = userint_c(&scancal,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;
         scandur = 3000 ;
         sprintf( key  , "SCANDUR%d=" , n ) ;
         sprintf( line , "Give length of original scan for snip nr %d [ %d ] " , n , scandur ) ;
         nitems  = userint_c(&scandur,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;
         snipcal = scancal + 500 ;
         sprintf( key  , "SNIPCAL%d=" , n ) ;
         sprintf( line , "Give start SATCAL of snip nr %d [ %d ] " , n , snipcal ) ;
         nitems  = userint_c(&snipcal,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;
         snipdur = ( 32 < maxtck ) ? 32 : maxtck ;
         sprintf( key  , "SNIPDUR%d=" , n ) ;
         sprintf( line , "Give length of snip nr %d [ %d ] " , n , snipdur ) ;
         nitems  = userint_c(&snipdur,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;

         psi     =  n ;
         sprintf( key  , "PSI%d=" , n ) ;
         sprintf( line , "Give PSI at begin of snip nr %d [ %f degrees ] " , n , psi ) ;
         nitems  = userreal_c(&psi,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;
         psirate = 3.85/60 ;
         sprintf( key  , "PSIRATE%d=" , n ) ;
         sprintf( line , "Give PSIRATE at begin of snip nr %d [ %f degrees/tick ] " , n , psirate ) ;
         nitems  = userreal_c(&psirate,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;
         theta   = 2 * n ;
         sprintf( key  , "THETA%d=" , n ) ;
         sprintf( line , "Give give THETA at begin of snip nr %d [ %f degrees ] " , n , theta ) ;
         nitems  = userreal_c(&theta,&one,DFLT_DEF,tofchar(key),tofchar(line)) ;
       


/*
   All parameters are known fro this snip => go and add it to the IRDS.
*/
         sprintf( line , "Set %.*s will be extended for snip nr %d" ,
                       (int) nelc_c( setname ) , setname.a , n );
         anyout_c( ANYOUT_DEF , tofchar( line ) ) ;
         irds_extend_c( setname ,		/* add a snip */
                        &sop ,			/* SOP nr. */
			&obs ,			/* OBS nr. */
                        &att ,			/* ATT nr. */
			scantype ,		
                        &scancal , 		/* satcal at begin of scan */
                        &scandur ,		/* nr of satcals in orig scan */
                        &snipcal ,		/* satcal at begin of snip */
                        &snipdur ,		/* nr of satcals in snip */
                        &psi ,			/* intended psi at scancal */
                        &psirate ,		/* intended psirate at scancal */
                        &theta ,		/* intended theta of scan */
                        &ierr ) ;		/* error return */
         if ( ierr != 0 ){
            sprintf( line , "Tough, IRDS_EXTEND error nr. %d !" , ierr ) ;
            error_c( FATAL_ERROR , tofchar( line ) ) ;
         }
      }
   }
   return( done );
}						/* MakeOutset */



/*
   WriteData will write fake data  to the IRDS setname on disk.
*/

void WriteData( fchar setname )
{
   char    line[MAXTXTLEN] ;
   fint	   level , ndets , nsnips , det , n ;
   fint    snip , satcal , nrsamp , status = 0 , error = 0 ;
   float   data[ MAXSAMPLES ] ;

   anyout_c( ANYOUT_TST , tofchar(" - WriteData") ) ;

   satcal = 0 ;
   level  = 0 ;
   gdsd_rint_c( setname , tofchar( "NAXIS3" ) , &level , &ndets  , &error ) ;
   gdsd_rint_c( setname , tofchar( "NAXIS4" ) , &level , &nsnips , &error ) ;
   for ( snip = 0 ; snip < nsnips ; snip++ ) {
      for ( det = 0 ; det < ndets ; det++ ) {
         for ( n = 1 ; n < MAXSAMPLES ; n++ ) {
            data[ n ] = det * MAXSAMPLES / 10 + n ;
         }
         nrsamp = MAXSAMPLES ;
         irds_wr_samples_c( setname , &snip , &det , &satcal ,
                            data , &nrsamp , &status ) ;
         if ( status ){
            sprintf( line , "Error %d while writing detector %d" , status , det );
            error_c( SERIOUS_ERROR , tofchar( line ) ) ;
         }
      }
      sprintf( line , "Wrote data for snip %d to set %.*s (%d detectors, each %d points)" 
                    , snip , (int) nelc_c( setname ) , setname.a , ndets , nrsamp );
      anyout_c( ANYOUT_DEF , tofchar( line ) ) ;
   }
}						/* WriteData */



MAIN_PROGRAM_ENTRY
{
   fint		status	      = 0    ;
   fint 	done ;

   fchar	setname ;

   init_c( ); 					/* get in touch with HERMES */
   anyout_c( ANYOUT_DEF, IDENT ) ;		/* show user who we are */
   finit( setname , MAXTXTLEN ) ;

   done = MakeOutset( setname ) ;		/* ask for output set */
   if ( !done ) {
      WriteData( setname ) ;			/* write the data to disk */
      irds_close_c( setname , &status ) ;
   }

   finis_c( );					/* bye, bye HERMES */
}


