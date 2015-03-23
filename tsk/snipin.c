/* identification */
#define	PROGRAM		"SNIPIN"
#define	VERSION		"1.4"
/* snipin.c

                            COPYRIGHT (c) 1990
            Kapteyn Astronomical Institute - University of Groningen
                P.O. Box 800, 9700 AV Groningen, The Netherlands

#>            snipin.dc1

Program:      SNIPIN

Purpose:      Generate a test IRDS

Category:     IRAS

File:         snipin.c

Author:       P.R. Roelfsema

Description:

      SNIPIN allows a user to construct an Infra Red Data Structure (IRDS)
    within GIPSY for test purposes. This is done by asking the user a tape
    containing snipped (by GEISHA programs) IR data. The user will have to
    specify a number of things not available in the FITS headers (e.g. custom
    plate center, coordinate system etc.).

Keywords:

    TAPE=     Name of FITS tape to read [ exit ]

    IRSETOUT= Name of output IR data set [ exit ]

    SKYSYS=   Custom plate coordinate system [ EQUATORIAL ]

    EPOCH=    Epoch of the coordinate system [ 1983.5 ]

    CENTER=   Custom plate center position in degrees [ 0,0 ]

    SIZE=     Custom plate sizes in degrees [ 1,1 ]

    OBJECT=   Name of object in custom plate [ name of IR set ]

    OBSERVER= Name of observer [ ??? ]

    MAXTICKS= Maximum number of satcal ticks [ max number in plate ]

    SNIPS=    Snip numbers wanted in the plate [ all snips on tape ]

Updates:      Aug 8, 1990: PRR, Document created.
              Mar 19,1992: HB,  New interface to irds_extend
              Dec  1,1992: VOG, Category added

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
#include "gdsd_wchar.h"
#include "irds_close.h"
#include "irds_create.h"
#include "irds_delete.h"
#include "irds_extend.h"
#include "irds_exist.h"
#include "irds_wr_samples.h"
#include "ircc_rate.h"
#include "ircc_ndets.h"
#include "ircc_bandnr.h"
#include "ircc_obsmode.h"
#include "irlrs_dn2mv.h"
#include "userint.h"
#include "userlog.h"
#include "userreal.h"
#include "userdble.h"
#include "usertext.h"
#include "mtopen.h"
#include "mtclose.h"
#include "mtfsf.h"
#include "ftsd_geth.h"
#include "ftsi_getr.h"
#include "fts_skipfil.h"
#include "ftsd_rreal.h"
#include "ftsd_rchar.h"
#include "ftsd_rint.h"

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
#define ANYOUT_NOEXP    ( &anyout_level_dumb_user )
static  fint            anyout_level_test      = 16 ;
#define ANYOUT_TST      ( &anyout_level_test      )*/

/* definitions for default levels */

static  fint            default_no_default     =  0 ;
#define DFLT_NONE       ( &default_no_default  )
static  fint            default_has_default    =  1 ;
#define DFLT_DEF	( &default_has_default )
/*static  fint            default_hidden_key     =  2 ;
#define DFLT_HIDD       ( &default_hidden_key  )*/
static  fint            default_exact_number   =  4 ;
#define DFLT_EXACT      ( &default_exact_number)


/* keywords and USER*** message strings */
#define TAPE_KEY	tofchar("TAPE=")
#define TAPE_MES	tofchar("Give name of tape to read [ exit ]" )
#define OUTSET_KEY	tofchar("IRSETOUT=")
#define OUTSET_MES	tofchar("Give output IR data set [ no output set ]")
#define CENTER_KEY	tofchar("CENTER=")
#define CENTER_MES	tofchar("Give custom plate center position (deg) [ 0 , 0 ]")
#define SIZE_KEY	tofchar("SIZE=")
#define SIZE_MES	tofchar("Give custom plate sizes (deg) [ 1 , 1 ]")
#define SKYSYS_KEY	tofchar("SKYSYS=")
#define SKYSYS_MES	tofchar("Give custom plate coordinate system [ EQ ]")
#define EPOCH_KEY	tofchar("EPOCH=")
#define EPOCH_MES	tofchar("Give epoch of coordinate system [ 1950 ]")
#define OBJECT_KEY	tofchar("OBJECT=")
#define OBJECT_MES	tofchar("Give name of object [ name of IRDS ]")
#define OBS_KEY		tofchar("OBSERVER=")
#define OBS_MES		tofchar("Give name of observer [ Piet Snot ]")
#define MXSATS_KEY	tofchar("MAXTICKS=")
#define SNIPS_KEY	tofchar("SNIPS=")
#define SNIPS_MES	tofchar("Give snip numbers to read [ all ]")

/* miscellaneous definitions */
#define true		    1
#define false		    0
#define HEDLEN		 6000			/* length of fitsheader */
#define MAXTXTLEN	   80			/* length of textlines */
#define MAXPLATES	   25			/* maximum nr of plates */
#define MAXSAMPLES	10000			/* maximum nr of samples */
#define	MAXSNIPS	  250			/* maximum nr. of snips	*/



MAIN_PROGRAM_ENTRY
{
   char      line[MAXTXTLEN]  ;

   fint      ierr   = 0 , eot = false ;
   fint      nitems = 0 , level = 0 ;
   fint      det , snip , satcal ;
   fint      one = 1 , two = 2 ;

   fchar     skysys , object , observer , bunit ;
   fchar     instrume , setname , intape , ftshed , scantype ;

   int       found  = 0 ;
   int       done   = 0 ;

   float     data[ MAXSAMPLES ] ;
   double    lonlat[2] ;
   double    sizes[2] ;
   fint      snips[ MAXSNIPS ] ;
   fint      axes[3] ;
   fint      sop, obs = 0, att ;
   fint      scancal, scandur, snipcal, snipdur , maxtck ;
   float     psi, psirate, theta , epoche , crval1 ;
   fint      nrsnips , ttid , mtid , index , nsamps ;

   init_c( ); 					/* get in touch with HERMES */
   IDENTIFICATION( PROGRAM , VERSION ) ;	/* show user who we are */
   finit( setname  , MAXTXTLEN ) ;		/* initialize fchars */
   finit( instrume , MAXTXTLEN ) ;
   finit( skysys   , MAXTXTLEN ) ;
   finit( object   , MAXTXTLEN ) ;
   finit( bunit    , MAXTXTLEN ) ;
   finit( observer , MAXTXTLEN ) ;
   finit( intape   , MAXTXTLEN ) ;
   finit( ftshed   , HEDLEN    ) ; 
   finit( scantype , 20 ) ;

   nitems = usertext_c( intape , DFLT_NONE, TAPE_KEY, TAPE_MES );/*get tape*/
   done = ( nitems == 0 ) ; 				/* did user type CR? */

   if( !done ) {					/* got a tapename */
      mtid = mtopen_c( intape ) ;			/* get tapedrive */
      if ( mtid < 0 ) {					/* tape drive error */
         sprintf( line , "Tough: error number %d on tape %.*s" ,
               mtid , nelc_c( intape ) , intape.a ) ;
         error_c( FATAL_ERROR , tofchar( line ) ) ;
      }
      ttid = 0 ;
      ierr = ftsd_geth_c( &mtid , ftshed , &ttid ) ;	/* get fits header */
      if ( ierr == -12 ) {				/* was label -> skip */
         ttid = 0 ;
         ierr = ftsd_geth_c( &mtid , ftshed , &ttid ) ;	/* get fits header */
      }
      if ( ierr < 0 ) {					/* read header erro */
         sprintf( line , "Tough: error number %d trying to get header" , ierr ) ;
         error_c( FATAL_ERROR , tofchar( line ) ) ;
      }
   }

   if( !done ) {
      nitems = usertext_c( setname, DFLT_NONE, OUTSET_KEY, OUTSET_MES );/*get set*/
      done = ( nitems == 0 ) ; 	 			/* did user type CR? */
   }
   if( !done ) {
      ierr  = 0 ;
      found = gds_exist_c( setname , &ierr );		/* does INSET exist? */
      if( found ){					/* INSET does exist */
         (void) sprintf( line , "IRSETOUT %.*s already exists, will be deleted " , 
                                 (int) nelc_c( setname ) , setname.a );
         error_c( WARNING , tofchar( line ) );		/* tell user */
         irds_delete_c( setname , &ierr );		/* delete inset */
         if ( ierr != 0 ){				/* error while deleting */
            sprintf( line , "Tough, IRDS_DELETE error nr. %d !" , ierr ) ;
            error_c( FATAL_ERROR , tofchar( line ) ) ;
         }
      }

/*

   Now we have the output setname which is to be created, so we'll start
asking all the questions to define the base IRDS.

*/							/* get instrument */
      index = ftsd_rchar_c( ftshed , tofchar( "INSTRUME" ) , instrume ) ;
      bunit = tofchar( "DN" ) ;
      if ( !strncmp( instrume.a , "LRS" , 3 ) ){
         instrume = tofchar( "SURVEY LRS" ) ;
         bunit    = tofchar( "mV") ;
      } else if ( !strncmp( instrume.a , "SURVEYB1" , 8 ) ) {
         instrume = tofchar( "SURVEY B1" ) ;
      } else if ( !strncmp( instrume.a , "SURVEYB2" , 8 ) ) {
         instrume = tofchar( "SURVEY B2" ) ;
      } else if ( !strncmp( instrume.a , "SURVEYB3" , 8 ) ) {
         instrume = tofchar( "SURVEY B3" ) ;
      } else if ( !strncmp( instrume.a , "SURVEYB4" , 8 ) ) {
         instrume = tofchar( "SURVEY B4" ) ;
      } else {						/* bad instrument */
         sprintf( line , "Tape contains unknown instrument: %.*s" ,
                  nelc_c( instrume ) , instrume.a ) ;
         error_c( FATAL_ERROR , tofchar( line ) ) ;	/* exit */
      }
							/* get coordinates */
      nitems = usertext_c(skysys,        DFLT_DEF  ,SKYSYS_KEY,SKYSYS_MES) ;
      if ( nitems <= 0 ) skysys.a  = strcpy( skysys.a   , "EQUATORIAL" ) ;
      nitems = userreal_c(&epoche,&one  ,DFLT_EXACT,EPOCH_KEY ,EPOCH_MES ) ;
      if ( nitems != 1 ) epoche    = 1950.0 ;
      nitems = userdble_c(lonlat,&two   ,DFLT_EXACT,CENTER_KEY,CENTER_MES) ;
      if ( nitems != 2 ) { lonlat[0] = 0 ; lonlat[1] = 0 ; }
      nitems = userdble_c(sizes ,&two   ,DFLT_EXACT,SIZE_KEY  ,SIZE_MES  ) ;
      if ( nitems != 2 ) { sizes[0]  = 1 ; sizes[1]  = 1 ; }
							/* get object name */
      nitems = usertext_c(object,        DFLT_DEF  ,OBJECT_KEY,OBJECT_MES) ;
      if ( nitems <= 0 ) object.a = strncpy( object.a, setname.a, setname.l ) ;
							/* get observer name */
      nitems = usertext_c(observer,      DFLT_DEF  ,OBS_KEY   ,OBS_MES   ) ;
      if ( nitems <= 0 ) observer.a= strcpy( observer.a , "Piet Snot" )  ;



							/* default nr of ticks */
      maxtck = 2 * sqrt( sizes[0] * sizes[0] + sizes[1] * sizes[1] ) * 60 / 3.85 ;
      sprintf( line , "Give maximum number of satcals in %.*s [%d]" ,
                                 (int) nelc_c( setname ) , setname.a , maxtck );
      nitems = 1 ;
      nitems = userint_c(&maxtck,&nitems,DFLT_DEF,MXSATS_KEY,tofchar(line) ) ;
      if ( maxtck < 0 ) maxtck = 16 ;
      if ( maxtck > MAXSAMPLES ) {			/* too many ticks */
         sprintf( line, "Cannot generate more than %d samples !", MAXSAMPLES ) ;
         error_c( FATAL_ERROR , tofchar( line ) ) ;	/* exit */
      }
      axes[0] = ircc_rate_c( instrume );		/* samples per satcal */
      axes[1] = maxtck ;				/* nr of satcals */
      axes[2] = ircc_ndets_c( instrume );		/* nr of detectors */

/*
   Go ahead and create the set
*/
      ierr = 0 ;
      irds_create_c( setname ,				/* create setname */
                     instrume ,				/* LRS set */
                     axes ,				/* the axes sizes */
                     lonlat ,				/* the center */
                     sizes , 				/* size of the plate */
                     skysys ,				/* coordinate system */
                     &epoche ,				/* epoche of coordinates */
                     object ,				/* object name */
                     observer,				/* observer */
                     &ierr ) ; 				/* error return */
      if ( ierr != 0 ){
         sprintf( line , "Tough, IRDS_CREATE error nr. %d !" , ierr ) ;
         error_c( FATAL_ERROR , tofchar( line ) ) ;
      }
      level = 0 ;
      gdsd_wchar_c( setname , tofchar( "BUNIT" ) , &level , bunit , &ierr ) ;
      
      nrsnips = MAXSNIPS ;
						/* get nr. of snips */
      nitems  = userint_c(snips,&nrsnips,DFLT_DEF,SNIPS_KEY,SNIPS_MES ) ;
      nrsnips = nitems ;
      if ( nitems == 0 ){
         for ( snip = 0 ; snip < MAXSNIPS ; snip++ ) {
            snips[ snip ] = snip + 1 ;
         }
         nrsnips = MAXSNIPS ;
      }



/*
   The output base IRDS is created, now it must be extended with a series of
of snips (nrsnips to be set by the user). For each snip a file is read, and 
the IRDS is extended according to the parameters of that snip. Snips which
are too big are skipped.
*/
      ierr = 0 ;
      snip = 1 ;
      if ( snips[ 0 ] != 1  ) {
         nitems = snips[ 0 ] - 1 ;
         ierr = fts_skipfil_c( &mtid , &nitems ) ;
         eot  = ( ierr == -13 ) || ( ierr < nitems ) ;
         if( !eot ) {					/* not endoftape? */
            ttid = 0 ;					/* try next file */
            ierr = ftsd_geth_c( &mtid , ftshed , &ttid ) ;
            eot  = ( ierr == -13 ) ;			/* endoftape ? */
         }
      }
							/* loop on snips */
      while( ( !eot ) && ( ierr >= 0 ) && ( snip <= nrsnips ) ) {
							/* read header */
         ierr    = ftsd_rchar_c( ftshed , tofchar( "OBJECT" )  , object   ) ;
         sscanf( object.a , "%4d%4d" , &sop , &att ) ;
         ierr    = ftsd_rint_c(  ftshed , tofchar( "SATCAL" )  , &scancal ) ;
         ierr    = ftsd_rint_c(  ftshed , tofchar( "OBSDUR" )  , &scandur ) ;
         ierr    = ftsd_rreal_c( ftshed , tofchar( "CRVAL1" )  , &crval1  ) ;
         snipcal = (int) crval1 ;
         ierr    = ftsd_rint_c(  ftshed , tofchar( "NAXIS1" )  , &snipdur ) ;
         snipdur = snipdur / axes[ 0 ] ;
         ierr    = ftsd_rreal_c( ftshed , tofchar( "PSI" )     , &psi     ) ;
         ierr    = ftsd_rreal_c( ftshed , tofchar( "PSIRATE" ) , &psirate ) ;
         ierr    = ftsd_rreal_c( ftshed , tofchar( "THETA" )   , &theta   ) ;
   
         if( snipdur <= axes[ 1 ] ) {			/* snip not too big */
            sprintf( line , "Set %.*s will be extended for snip nr %d" ,
                          (int) nelc_c( setname ) , setname.a , snips[ snip - 1 ] );
            status_c( tofchar( line ) ) ;		/* tell user */
            ierr = 0 ;
            irds_extend_c( setname ,			/* add a snip */
                           &sop ,			/* SOP nr. */
                           &obs ,                       /* OBS nr. */
                           &att ,			/* ATT nr. */
                           scantype ,
                           &scancal , 			/* satcal at begin of scan */
                           &scandur ,			/* nr of satcals in orig scan */
                           &snipcal ,			/* satcal at begin of snip */
                           &snipdur ,			/* nr of satcals in snip */
                           &psi ,			/* intended psi at scancal */
                           &psirate ,			/* intended psirate at scancal */
                           &theta ,			/* intended theta of scan */
                           &ierr ) ;			/* error return */
            if ( ierr != 0 ){				/* trouble */
               sprintf( line , "Tough, IRDS_EXTEND error nr. %d !" , ierr ) ;
               error_c( FATAL_ERROR , tofchar( line ) ) ;
            }
            satcal = 1 ;				/* add data at grid 1 */
            nsamps = snipdur * axes[ 0 ] ;		/* add entire snip */
            for( det = 1 ; det <= axes[ 2 ] ; det++ ) {	/* loop on detectors */
               ierr = ftsi_getr_c( &mtid  , data  , &nsamps , &ttid   ) ;
               eot  = ( ierr == -13 ) ;			/* got to endoftape? */
               if ( !eot && ( ierr <= 0 ) ){		/* error with read data */
                  sprintf( line , "Tough, det nr %d FTSI_GETR error nr. %d !" ,
                                det , ierr ) ;
                  error_c( SERIOUS_ERROR , tofchar( line ) ) ;	/* skip */
               } else if ( eot ) {			/* end of tape */
                  break ;				/* stop reading */
               } else {
                  if( ircc_bandnr_c( instrume ) == 
                      ircc_bandnr_c( tofchar( "LRS" ) ) ) {/* is this LRS ? */
                     irlrs_dn2mv_c( data , &nsamps ) ;	/* put to linear scale */
                  }
							/* write to IRDS */
                  irds_wr_samples_c( setname , &snip , &det    , &satcal ,
                                               data  , &nsamps , &ierr   ) ;
                  if ( ierr < 0 ){			/* error writing ? */
                     sprintf( line , "Tough, det nr %d IRDS_WR_SAMPLES error nr. %d (%d points)!" , 
                                   det , ierr , nsamps ) ;
                     error_c( SERIOUS_ERROR , tofchar( line ) ) ;
                  }
               }
            }
         } else {					/* snip too big */ 
            sprintf( line , "Snip nr %d too long for set %.*s" , snip ,
                          (int) nelc_c( setname ) , setname.a );
            anyout_c( ANYOUT_DEF , tofchar( line ) ) ;	/* tell user */
         }
         if ( ( snip + 1 ) > nrsnips ) break ;
         nitems = snips[ snip ] - snips[ snip - 1 ] ;
         ierr = fts_skipfil_c( &mtid , &nitems ) ;
         eot  = ( ierr == -13 ) || ( ierr < nitems ) ;
         snip = snip + 1 ;				/* next snip */
         if( !eot ) {					/* not endoftape? */
            ttid = 0 ;					/* try next file */
            ierr = ftsd_geth_c( &mtid , ftshed , &ttid ) ;
            eot  = ( ierr == -13 ) ;			/* endoftape ? */
         }
         if ( !eot && ( ierr < 0 ) ) {			/* header read error */
            sprintf( line , "Tough: error number %d trying to get header" , ierr ) ;
            error_c( SERIOUS_ERROR , tofchar( line ) ) ;
         }
      }
      ierr = mtclose_c( &mtid ) ;			/* close tape */
      irds_close_c( setname , &ierr ) ;			/* close irds */
   }

   finis_c( );						/* bye, bye HERMES */
}


