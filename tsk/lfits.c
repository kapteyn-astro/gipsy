/* identification */
#define VERSION		"1.3"			/* version number 	*/
#define PROGRAM		"LFITS"			/* program name 	*/
#define NDEBUG		0 			/* assert debug mode	*/

/* lfits.c

                            COPYRIGHT (c) 1991
            Kapteyn Astronomical Institute - University of Groningen
                P.O. Box 800, 9700 AV Groningen, The Netherlands

#>            lfits.dc1

Program:      lfits

Purpose:      List FITS files on a tape

Category:     FITS, TAPES, UTILITY

File:         lfits.c

Author:       P.R. Roelfsema (M. Vogelaar)

Keywords:

     TAPE=    Name of input (disk-)tape [ none ]


 ** FILES=    Give file numbers to list [ all files on TAPE ]
    
  ** FULL=    Give listing of entire FITS header? [ N ]
 
** OUTPUT=    Name of file where listings are saved [ none ]

Description:

                LFITS lists files found on a FITS tape. For each file
              it gives a one-line description of its contents.
              
                It is also possible to get the full header (this can give 
              A LOT of output!) by specifying FULL=Y.
 

Updates:      Mar 28, 1991: PRR, Document created.
              Apr  3, 1991: PRR, added OUTPUT= keyword.
              May 15, 1992: VOG, New call to mtopen
#<

*/


#include "gipsyc.h"
#include "assert.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "cmain.h"
#include "init.h"
#include "finis.h"
#include "nelc.h"
#include "status.h"
#include "anyout.h"
#include "error.h"
#include "cancel.h"
#include "usertext.h"
#include "userint.h"
#include "userlog.h"
#include "mtopen.h"
#include "mtclose.h"
#include "mtrew.h"
#include "mtfsf.h"
#include "mtbsf.h"
#include "mtweof.h"
#include "fts_skipfil.h"
#include "ftsd_geth.h"
#include "ftsd_puth.h"
#include "ftsi_geti.h"
#include "ftsi_puti.h"
#include "ftsd_rint.h"
#include "ftsd_rchar.h"
#include "ftsd_find.h"
#include "ircc_rate.h"

#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.l = len ; }


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


/* keywords and USER*** message strings */
#define	TAPE_KEY	tofchar( "TAPE=" )
#define	TAPE_MES	tofchar( "Name of input (disk-)tape [ none ]" )

#define	FILES_KEY	tofchar( "FILES=" )
#define	FILES_MES	tofchar( "Give file numbers to list [ all files on TAPE ]" )

#define	FULL_KEY	tofchar( "FULL=" ) 
#define	FULL_MES	tofchar( "Give listing of entire FITS header? [ N ]" ) 

#define	LIST_KEY	tofchar( "OUTPUT=" ) 
#define	LIST_MES	tofchar( "Name of file where listings are saved [ none ]" ) 

/* miscellaneous definitions */
#define true              1
#define false             0
#define MAXTXTLEN       160			/* length of textlines 	*/

#define	OK		    0			/* OK			*/
#define STOP            -1000                   /* stop code            */
#define OPEN_ERROR      -1001                   /* tape open error code */
#define BAD_SKIP        -1002                   /* bad skip count       */
#define BAD_HEADER      -1003                   /* bad header           */
#define END_OF_TAPE     -1006                   /* found EOT            */
#define HEADER_ERROR    -1007                   /* error in process hed */

#define	FTS_EOT		-13			/* fts_io EOT definition*/
#define TAPE_LABEL	-12			/* fts_io tape label	*/

#define	MAXFILES	 250			/* max. nr. of files	*/
#define	FTSBLOCK	2880			/* fits block size	*/
#define FTS_RECLEN	  80			/* fits record size	*/
#define HEDLEN		30*FTSBLOCK		/* fits header size	*/

/*
      The routine OpenTape tries to open a tape unit 
*/
       fint OpenTape( fchar tapename ,		/* open tapename	*/
                      fint  *deflev ,		/* default level	*/
                      fchar key ,		/* the keyword		*/
                      fchar mess )		/* the message		*/

{
   fint		mtid ;				/* tape id		*/	
   fint		error ;				/* error retrun code	*/
   
   char 	line[ MAXTXTLEN ] ;		/* text line		*/

   anyout_c( ANYOUT_TST , tofchar( "LFITS - OpenTape" ) ) ;
/*   nret = usertext_c( tapename ,*/		/* get tape name	*/
/*                      deflev ,	  */		/* has a default	*/
/*                      key ,	  */		/* keyword		*/
/*                      mess ) ;	  */		/* message		*/
/*   cancel_c( key ) ;		*/		/* cancel the keyword	*/
/*   if ( nret == 0 ) return( STOP ) ; */		/* CR -> exit		*/
/*   mtid = mtopen_c( tapename ) ; */		/* try to open tape	*/
   mtid = mtopen_c( tofchar( "?TAPE=Name of tape device [list of all tape devices]" ) );
   
   if ( mtid < 0 ) {				/* cannot open tape	*/
      sprintf( line , "Cannot open tape %.*s, try another" ,
                       nelc_c( tapename ) ,
                       tapename.a ) ;
      error_c( WARNING , tofchar( line ) ) ;	/* issue warning	*/
      sprintf( line , "MTOPEN error nr %d" , mtid ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;	/* inform user	*/
      return( OPEN_ERROR ) ;			/* return error		*/
   }

   sprintf( line , "Will rewind tape %.*s" ,
                    nelc_c( tapename ) ,
                    tapename.a ) ;
   status_c( tofchar( line ) ) ;		/* inform user		*/
   if ( ( error = mtrew_c( &mtid ) ) < 0 ) {	/* try to rewind	*/
      sprintf( line , "Cannot rewind tape %.*s, try another" ,
                       nelc_c( tapename ) ,
                       tapename.a ) ;
      error_c( WARNING , tofchar( line ) ) ;	/* issue warning	*/
      sprintf( line , "MTREW error nr %d on mtid %d" , error , mtid ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;	/* inform user	*/
      return( OPEN_ERROR ) ;			/* return error		*/
   }

   sprintf( line , "Opened tape %.*s on mtid %d" ,
                    nelc_c( tapename ) ,
                    tapename.a ,
                    mtid ) ;
   anyout_c( ANYOUT_TST , tofchar( line ) ) ;	/* inform user		*/

   return( mtid ) ;				/* give the mtid back 	*/
}						/* end OpenTape		*/

/*
       CloseTape closes a tape unit and issues a warning in case of trouble
*/
void CloseTape( fint  *mtid , 			/* close mtid		*/
                fchar tapename ) 		/* called tapename	*/
{
   char		line[ MAXTXTLEN ] ;		/* text line		*/

   fint		error ;				/* error retrun code	*/
   
   anyout_c( ANYOUT_TST , tofchar( "LFITS - CloseTape" ) ) ;
   sprintf( line , "Will rewind tape %.*s" ,
                    nelc_c( tapename ) ,
                    tapename.a ) ;
   status_c( tofchar( line ) ) ;		/* inform user		*/
   if ( ( error = mtrew_c( mtid ) ) < 0 ) {	/* try to rewind	*/
      sprintf( line , "Cannot rewind tape %.*s" ,
                       nelc_c( tapename ) ,
                       tapename.a ) ;
      error_c( WARNING , tofchar( line ) ) ;	/* issue warning	*/
      sprintf( line , "MTREW error nr %d on mtid %d" , error , *mtid ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;/* inform user		*/
   }

   error = mtclose_c( mtid ) ;			/* try to close mtid	*/
   if( error < 0 ) {
      sprintf( line , "Cannot close tape %.*s, state of tape is unknown" ,
                       nelc_c( tapename ) ,
                       tapename.a ) ;
      error_c( WARNING , tofchar( line ) ) ;	/* issue warning	*/
      sprintf( line , "MTCLOSE error nr %d on mtid %d" , error , *mtid ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;	/* inform user	*/
      return ;
   }

   sprintf( line , "Closed tape %.*s on mtid %d" ,
                    nelc_c( tapename ) ,
                    tapename.a ,
                    *mtid ) ;
   anyout_c( ANYOUT_TST , tofchar( line ) ) ;	/* inform user		*/
}

/*
     MakeHeading opens (on user request an output file, and writes the 
  header for the tape listing on that output file.
*/
static FILE *MakeHeading( fchar tape     ,	/* header for tape	*/
                          fchar listing	 )	/* name of listfile	*/
{
   bool		done = false ;			/* got a filename ?	*/

   FILE		*listfile = NULL ;		/* file handle		*/

   char		line[ MAXTXTLEN ] ;		/* text line		*/
   char		filename[ MAXTXTLEN ] ;		/* filename buffer	*/
   
   fint		nret ;				/* return from usertext	*/
   fint		*deflev ;			/* default level	*/

   if ( !listfile ) {				/* no listfile yet	*/
      deflev = DFLT_HIDD ;			/* start as hidden key	*/
      do {
         nret = usertext_c( listing ,		/* get a listfile name	*/
                            deflev ,		/* default level	*/
                            LIST_KEY ,		/* keyword		*/
                            LIST_MES ) ;	/* message		*/
         if ( nret > 0 ) {			/* user typed something	*/
            (void) strncpy( filename , listing.a , nelc_c( listing ) ) ;
            listfile = fopen( filename , "a" ) ;/* append to file	*/
            done = ( listfile != NULL ) ;	/* done if file is open	*/
            if ( !listfile ) {			/* file open error	*/
               sprintf( line , "Could not open file %.*s, try again" ,
                                nelc_c( listing ) , listing.a ) ;
               error_c( WARNING , tofchar( line ) ) ;	/* warn user	*/
               cancel_c( LIST_KEY ) ;		/* cancel the key	*/
            }
         } else {
            break ;				/* CR -> no file	*/
         }
      } while ( !done ) ;			/* stop until OK	*/
   }

   if ( listfile ) {
      fprintf( listfile , "\n\n  Listing of tape %.*s:\n\n" , nelc_c( tape ) , tape.a ) ;
      fprintf( listfile , "file | object           | instrument |bpix| size(s)  ...   comment\n" ) ;
      fprintf( listfile , "-----|------------------|------------|----|-----------------------------------\n") ;
   } else {
      sprintf( line , "  Listing of tape %.*s:" , nelc_c( tape ) , tape.a ) ;
      anyout_c( ANYOUT_DEF , tofchar( line ) ) ;
      anyout_c( ANYOUT_DEF , tofchar( "" ) ) ;
      sprintf( line , "file | object           | instrument |bpix| size(s)  ...   comment" ) ;
      anyout_c( ANYOUT_DEF , tofchar( line ) ) ;
      sprintf( line , "-----|------------------|------------|----|-----------------------------------") ;
      anyout_c( ANYOUT_DEF , tofchar( line ) ) ;
   }

   return( listfile ) ;				/* give the handle	*/
}						/* MakeHeading		*/

/*
      SkipInFiles skips files on the input tape
*/
       fint SkipInFiles( fint  *mtid ,		/* input tape id	*/
                         fchar tape ,		/* name of input tape	*/
                         fint  *files ,		/* list of files	*/
                         fint  file ,		/* current seq.file nr.	*/
                         bool  all_files ) 	/* do all files ?	*/
{
   char		line[ MAXTXTLEN ] ;		/* text line		*/

   fint		nskip ;				/* nr. of files to skip	*/
   fint		error = 0 ;			/* error return code	*/
         
   anyout_c( ANYOUT_TST , tofchar( "LFITS - SkipInFiles" ) ) ;
   if ( file == 0 ) {				/* first file ?		*/
      nskip = files[ file ] - 1 ;		/* yes-> skip is fileno	*/
   } else {
      nskip = files[ file ] - 
              files[ file - 1 ] - 1 ;		/* no-> skip from last	*/
   }
   sprintf( line , "Sequential file %d is file %d, try to skip %d files" ,
                    file , files[ file ] , nskip ) ;
   anyout_c( ANYOUT_TST , tofchar( line ) ) ;
   error = fts_skipfil_c( mtid  , &nskip ) ;	/* skip forward		*/
   sprintf( line , "FTS_SKIPFIL returns %d trying to skip %d files on tape %.*s" ,
                    error , nskip , nelc_c( tape ) , tape.a ) ;
   anyout_c( ANYOUT_TST , tofchar( line ) ) ;
   if ( error == FTS_EOT )  {			/* got End Of Tape?	*/
      if ( file == 0 ) {			/* first input file ?	*/
         sprintf( line , "Input tape %.*s is empty, try new tape" ,
                          nelc_c( tape ) , tape.a ) ;
         error_c( WARNING , tofchar( line ) ) ;
      } else {					/* EOT, not first file	*/
         if ( !all_files ) {			/* not all? -> warning	*/
            sprintf( line , "Reached end of input tape %.*s before file %d!" ,
                             nelc_c( tape ) , tape.a ,
                             files[ file ] ) ;
            error_c( WARNING , tofchar( line ) ) ;
         }
      }
      return( BAD_SKIP ) ;			/* exit loop on files	*/
   } else if ( error < 0 ) {
      sprintf( line , "Error %d skipping %d files on input tape %.*s, try new tape" ,
                       error , nskip ,
                       nelc_c( tape ) , tape.a ) ;
      error_c( WARNING , tofchar( line ) ) ;
      return( BAD_SKIP ) ;			/* exit loop on files	*/
   }
   
   return( OK ) ;         			/* no problems 		*/
}						/* end SkipInFiles	*/

/*
      ClearHeader sets an entire  header to balnk spaces
*/
       void ClearHeader( char *header , 	/* clear header		*/
                         fint  nel )		/* number of items to do*/
{
   fint		n ;
   
   anyout_c( ANYOUT_TST , tofchar( "LFITS - ClearHeader" ) ) ;
   for ( n = 0 ; n < nel ; n++ ) {		/* loop on header array	*/
      header[ n ] = ' ' ;			/* set element to space	*/
   }
}						/* end ClearHeader	*/

/*
      ProcessHeaders reads the input header and creates an output header
*/
       int ProcessHeaders( fint  *mtid  ,	/* input mtid		*/
                           fchar tape ,		/* input tape		*/
                           fchar header  ,	/* input header		*/
                           fint  file )		/* current file number	*/
{
   char		line[ MAXTXTLEN ] ;		/* text line		*/

   fint		one = 1 ;
   fint		tid  ;				/* tid of input tape	*/
   fint		error ;				/* retrun code		*/
   
   anyout_c( ANYOUT_TST , tofchar( "LFITS - ProcessHeaders" ) ) ;
   ClearHeader( header.a , header.l ) ;	/* fill header with ' '	*/
   error  = TAPE_LABEL ;			
   while ( error == TAPE_LABEL ) {		/* skip label		*/
      tid  = 0 ;				/* initialise tid	*/
      error = ftsd_geth_c( mtid  ,		/* get tape		*/
                           header  ,		/* header of tape	*/
                           &tid  ) ;		/* input tid		*/
      sprintf( line , "Read %d character (max %d) header (tid %d) from mtid %d (tape %.*s)" ,
                       error , header.l , tid  , *mtid  , nelc_c( tape ) , tape.a ) ;
      anyout_c( ANYOUT_TST , tofchar(line ) ) ;
   }
   if ( error == FTS_EOT ) {			/* found end of tape	*/
      sprintf( line , "End of input tape %.*s" ,
                       nelc_c( tape ) , tape.a ) ;
      status_c( tofchar( line ) ) ;		/* tell user	*/
      return( END_OF_TAPE ) ;			/* exit loop on files	*/
   } else if ( error < 0 ) {			/* error reading header	*/
      sprintf( line , "Error %d reading header of file %d on input tape %.*s, try next file" ,
                       error, file , nelc_c( tape ) , tape.a ) ;
      error_c( WARNING , tofchar( line ) ) ;	/* tell user	*/
      sprintf( line , "Error %d reading header of file %d on mtid %d (tape %.*s)" ,
                       error , file , *mtid  ,
                       nelc_c( tape ) , tape.a ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;
      return( HEADER_ERROR ) ;			/* exit loop on files	*/
   }

   error = mtfsf_c( mtid , &one ) ;
   if ( error != one ) {
      sprintf( line , "Error skipping file %d on input tape %.*s, try next file" ,
                       file , nelc_c( tape ) , tape.a ) ;
      error_c( WARNING , tofchar( line ) ) ;	/* tell user	*/
      sprintf( line , "Error %d skipping to next file on mtid %d (tape %.*s)" ,
                       error , file , *mtid  ,
                       nelc_c( tape ) , tape.a ) ;
      return( HEADER_ERROR ) ;			/* exit loop on files	*/
   }

   return( OK ) ;				/* no problems		*/
}						/* end ProcessHeaders	*/

/*
      ListFile lists the current file in the desired format
*/      
static void ListFile( fint file ,		/* current file number	*/
                      fchar header ,  		/* FITS header 		*/
                      FILE *listfile ) 		/* list on list-file	*/
{
   bool		full = false ;			/* list full header?	*/
   
   char		line[ MAXTXTLEN ] ;		/* character line 	*/
   char		line2[ MAXTXTLEN ] ;		/* character line too 	*/
   char		key[ MAXTXTLEN ] ;		/* keyword	 	*/
   
   fint		n , one = 1 ;
   fint		naxis ;				/* nr. of axes		*/
   fint		bitpix ;			/* nr. of bits per pix.	*/ 
   fint		npix ;				/* nr. pixels on axis	*/
   fint		nrecs ;				/* nr. of FITS records	*/
   fint		sop = 0 , att = 0 ;		/* sop/att: IRAS	*/ 
  
   fchar	object ;			/* object name		*/
   fchar	instrument ;			/* instrument name	*/
   fchar	comment ;			/* comment record	*/
   
   anyout_c( ANYOUT_TST , tofchar( "LFITS - ListFile" ) ) ;
   finit( object        , MAXTXTLEN ) ;		/* initialise fchars	*/ 
   finit( instrument    , MAXTXTLEN ) ;
   finit( comment       , MAXTXTLEN ) ;
  
   (void) ftsd_rint_c(  header , 		/* read from header	*/
                        tofchar( "NAXIS" ) , 	/* NAXIS key		*/
                        &naxis ) ;		/* gives naxis		*/
   (void) ftsd_rint_c(  header , 		/* read from header	*/
                        tofchar( "BITPIX" ) , 	/* BITPIX key		*/
                        &bitpix ) ;		/* gives bitpix		*/
   if ( ftsd_rchar_c( header , 			/* read from header	*/
                      tofchar( "OBJECT" ) ,  	/* OBJECT key		*/
                      object ) < 0 ) {		/* gives object		*/
      (void) strncpy( object.a , "object unknown    " , 18 ) ;
      if ( ftsd_rint_c( header ,		/* read from header	*/
                        tofchar( "SOP" ) ,  	/* SOP key		*/
                        &sop ) >= 0 ) {		/* gives sop		*/
         (void) ftsd_rint_c( header ,		/* read from header	*/
                             tofchar( "ATT" ) ,	/* ATT key		*/
                             &att ) ;		/* gives att		*/
         sprintf( object.a , "sop/att %03d/%03d   " , sop ,att ) ;
      }
   }
   if ( ftsd_rchar_c( header , 			/* read from header	*/
                      tofchar( "INSTRUME" ) , 	/* INSTRUME key		*/
                      instrument ) < 0 ) {	/* gives instrument	*/
      if ( sop != 0 ) {				/* IRAS => BPHF		*/
         (void) strncpy( instrument.a , "IRAS BPHF  " , 10 ) ;         
      } else {
         (void) strncpy( instrument.a , "unknown    " , 10 ) ;
      }
   }
   sprintf( line , "%4d | %16.16s | %10.10s | %2d | " ,
                    file ,			/* file number		*/
                    object.a ,			/* object name		*/
                    instrument.a ,		/* instrument name	*/
                    bitpix ) ;			/* nr. of bits per pix.	*/

   for ( n = 1 ; n <= naxis ; n++ ) {		/* loop on axes		*/
      sprintf( key , "NAXIS%d" , n ) ;		/* make NAXIS% key	*/
      ftsd_rint_c(  header , 			/* read from header	*/
                    tofchar( key ) , 		/* NAXIS% key		*/
                    &npix ) ;			/* gives npix		*/
      if ( n < naxis ) {			/* not last axis yet	*/
         sprintf( line2 , "%5d *" , npix ) ;
      } else {					/* last axis 		*/
         sprintf( line2 , "%5d | " , npix ) ;
      }
      (void) strcat( line , line2 ) ;		/* add to line		*/
   }
   if ( ftsd_find_c( header , 			/* read from header	*/
                     tofchar( "COMMENT" ) , 	/* COMMENT key		*/
                     comment ) > 0 ) {		/* gives comment	*/
      (void) strncat( line , &comment.a[ 7 ] , nelc_c( comment ) - 7 ) ;
   }
   if ( listfile ) {				/* listing to file ?	*/
      fprintf( listfile , "%s\n" , line ) ;	/* print to file	*/
   } else {					/* no listfile		*/
      anyout_c( ANYOUT_DEF , tofchar( line ) ) ;/* print to screen	*/
   }
   
   n       = userlog_c( &full ,			/* full header?		*/
                        &one ,			/* only one logical	*/
                        DFLT_HIDD ,		/* hidden keyword	*/
                        FULL_KEY ,		/* keyword		*/
                        FULL_MES ) ;		/* message		*/
   if ( full ) {				/* user wants full	*/
      nrecs = nelc_c( header ) / FTS_RECLEN + 1 ;	/* nr. records	*/
      for ( n = 0 ; n < nrecs ; n++ ) {		/* loop on all records	*/
         sprintf( line , "%.*s" ,		/* put record in buffer	*/
                         FTS_RECLEN ,		/* one FITS record	*/
                         &header.a[ n * FTS_RECLEN ] ) ;
         if ( listfile ) {			/* listing to file ?	*/
            fprintf( listfile , "%s\n" , line ) ;	/* to file	*/
         } else {				/* no listfile		*/
            anyout_c( ANYOUT_DEF , tofchar( line ) ) ;	/* to screen	*/
         }
      }
   }
   

}						/* ListFile		*/

MAIN_PROGRAM_ENTRY
{
   FILE		*listfile = NULL ;		/* list-file handle	*/

   bool		all_files ;			/* all files ?		*/
   bool		done ;				
   
   char 	line[ MAXTXTLEN ] ;		/* text line		*/

   fchar	tape ;				/* name of input tape	*/
   fchar	header  ;			/* input fits header 	*/
   fchar	listing  ;			/* name of list-file 	*/
   
   fint		error = 0 ;			/* error return code	*/
   fint		*files ;			/* filenumbers to read	*/
   fint		nfiles ;			/* nr. of files to read	*/
   fint         maxfiles = MAXFILES ;		/* max. nr. of files	*/
   fint		file ;				/* file number		*/
   fint		mtid  ;				/* input tape id	*/

   init_c( ); 					/* hello HERMES 	*/
   IDENTIFICATION( PROGRAM , VERSION ) ;
   finit( tape       , MAXTXTLEN ) ;		/* initialise fchars	*/
   finit( header     , HEDLEN    ) ;
   finit( listing    , MAXTXTLEN ) ;
   files = (fint *) malloc( MAXFILES * sizeof( fint ) ) ;


   done = false ;
   while( !done ){				/* stop after one tape 	*/
      done = true ;

      mtid   = OpenTape( tape ,			/* get tape name	*/
                         DFLT_DEF ,		/* has a default	*/
                         TAPE_KEY ,		/* keyword		*/
                         TAPE_MES ) ;		/* message		*/
      if( mtid  == OPEN_ERROR ) continue ;	/* open error -> retry	*/
      if( mtid  == STOP ) break ;		/* CR -> exit 		*/
      
      nfiles  = userint_c( files ,		/* ask file numbers	*/
                           &maxfiles ,		/* max. nr. of files	*/
                           DFLT_HIDD ,		/* hidden keyword	*/
                           FILES_KEY ,		/* keyword		*/
                           FILES_MES ) ;	/* message		*/
      all_files = ( nfiles == 0 ) ;		/* read all files ?	*/
      if ( all_files ) {			/* set nfiles for all	*/
         nfiles = 1 ;
      }
            

      listfile = MakeHeading( tape     ,	/* header for tape	*/
                              listing  ) ;	/* name of listfile	*/

      sprintf( line , "Listing tape %s" , tape.a ) ;
      status_c( tofchar( line ) ) ;		/* inform user		*/
      for ( file = 0 ; file < nfiles ; file++ ) {/* loop on the files	*/
         if ( all_files ) {
            if ( nfiles > maxfiles ) {		/* to many files 	*/
               maxfiles = maxfiles + MAXFILES ;	/* add buffer space	*/
               files = (fint *)realloc( (char *)files , maxfiles * sizeof( fint ) ) ;
            }
            files[ file ] = file + 1 ;		/* set files		*/
            nfiles = nfiles + 1 ;		/* add a file		*/
         }

         error = SkipInFiles( &mtid  ,		/* input tape id	*/
                              tape ,		/* name of input tape	*/
                              files ,		/* list of files	*/
                              file ,		/* current seq.file nr.	*/
                              all_files ) ;	/* do all files ?	*/
         if ( error < 0 ) break ;		/* problem -> next tape	*/

         error = ProcessHeaders( &mtid  ,	/* input mtid		*/
                                 tape ,		/* input tape		*/
                                 header  ,	/* input header		*/
                                 files[ file ] ); /* current file nr.	*/
         if ( error == END_OF_TAPE ) {		/* EOT -> done		*/
            break ;
         } else if ( error < 0 ) {		/* problem -> next file	*/
            continue ;
         }
         
         ListFile( files[ file ] ,		/* current file number	*/
                   header , 			/* FITS header 		*/
                   listfile ) ;			/* on list-file		*/
         
      }						/* end loop on files	*/

      CloseTape( &mtid  , tape  ) ;		/* close tape		*/
      
      if ( listfile ) {				/* output file used	*/
         sprintf( line , "Listing of tape %.*s written to file %.*s" ,
                          nelc_c( tape )    , tape.a ,
                          nelc_c( listing ) , listing.a ) ;
         anyout_c( ANYOUT_DEF , tofchar( line ) ) ;	/* tell user 	*/
         (void) fclose( listfile ) ;		/* close listfile	*/
         listfile = NULL ;			/* no listfile		*/
      }
      
                 
   }						/* end endless loop	*/

   finis_c( );					/* bye, bye HERMES 	*/
   return( 0 ) ;
}


