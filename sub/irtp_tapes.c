/* irtp_tapes.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>	      irtp_tapes.dc2
Name:	      IRTP_TAPES

Purpose:      IRTP_TAPES contains routines relevant to identification
	      of physical tapes

Category:     TAPES

File:	      irtp_tapes.c

Author:       P.R. Roelfsema

Description:
	      The following tape identification routines are available:

	      IRTP_SA2BPHF  - returns BPHF tape name for SOP/ATT.

Updates:      Nov 14, 1990: PRR, Creation date
	      Dec  3, 1991: PRR, Added IRAS_ROOT
	      Mar 27, 2003: DK, more chars in directory
#<

*/

#include "gipsyc.h"
#include "string.h"
#include "stdio.h"
#include "stdlib.h"
#include "nelc.h"
#include "anyout.h"
#include "iras_root.h"

#define OK		  0			/* no error		*/
#define BAD_PLATE	 -1			/* plate does not exist */
#define BAD_INSTRUMENT	 -2			/* instrument unknown	*/
#define BAD_TAPE	 -3			/* tape unknown 	*/
#define BAD_SOPATT	 -5			/* sop/att unknown	*/
#define NO_TAPE_DESCRIPTOR	 -6		/* tape descriptor not found */
#define DIR_TOO_SMALL	 -7			/* short directory string */

#define NINSTRUMENTS	  3			/* nr of instruments	*/
#define INSTRUMENTS	 { "survey" , "AO" , "unknown" }

#define MAXTXTLEN	 80			/* max length of string */
#define MINDIRLENGTH	 75			/* min dir.-string length */

#define TAPE_DESCRIPTOR "tape.descriptor"


/*
#>	      irtp_sa2bphf.dc2

Function:     IRTP_SA2BPHF

Purpose:      IRTP_SA2BPHF returns the BPHF tape name containing a SOP/ATT

Category:     TAPE

File:	      ir_tapes.c

Author:       P.R. Roelfsema

Use:	      INTEGER IRTP_SA2BPHF( SOP        ,   Input   integer
				    ATT        ,   Input   integer
				    DIRECTORY  ,   Output  character*(>35)
				    FILENO     )   Output  integer

	      IRTP_SA2BPHF Error return code:
			   >0 - no problem.
			   -5 - SOP/ATT not found.
			   -6 - tape descriptor file not found
			   -7 - string DIRECTORY too small
	      SOP	  SOP of desired BPHF data.
	      ATT	  ATT of desired BPHF data.
	      DIRECTORY   The name of the tape containing BPHF data for
			  the SOP/ATT combination (>35 chars).
	      FILENO	  The sequential file number of the file on
			  TAPENAME containing the BPHF for SOP/ATT.

Updates:      Sep.  5, 1990: PRR, Creation date
	      Jun. 20, 1991: PRR, Patch to use new filesystem
	      Nov.  7, 1991: AdJ, Corrected hard-coded (!) filename
	      Dec.  3, 1991: PRR, Added IRAS_ROOT.
#<

Fortran to C interface:

@ integer function irtp_sa2bphf( integer , integer , character , integer )

*/

fint irtp_sa2bphf_c( fint  *sop      , fint *att    ,
		     fchar directory , fint *fileno )
{
   char 	trydir[ MAXTXTLEN ] , line[ MAXTXTLEN ];
   char 	mess[ MAXTXTLEN ] ;
   char 	instrument[ NINSTRUMENTS ][ 10 ] = INSTRUMENTS ;

   fint 	sixteen = 16 ;
   fint 	filesop , fileatt ;
   fint 	instrumentno ;
   fint 	found = 0 ;

   FILE 	*tapedescriptor = NULL ;	/* tape descriptor file */

   *fileno = 0 ;
   if ( directory.l < MINDIRLENGTH ) {
      return( DIR_TOO_SMALL ) ;
   }

   instrumentno = 0 ;
   while ( !found & ( instrumentno < NINSTRUMENTS ) ) {

      if ( iras_root_c( directory ) < 0 ) {	/* not enough chars.	*/
	 return( DIR_TOO_SMALL ) ;
      }
      sprintf( trydir , "/bphf/%s/sop.%02d_/"
		       , instrument[ instrumentno ] , *sop / 10 ) ;
      strcat( directory.a , trydir ) ;
      sprintf( trydir , "%.*stape.descriptor"
		       , nelc_c( directory ) , directory.a ) ;
      sprintf( mess , "Trying %s" , trydir ) ;
      anyout_c( &sixteen , tofchar( mess ) ) ;
      if ( !( tapedescriptor = fopen( trydir , "r" ) ) ){
	 return( NO_TAPE_DESCRIPTOR ) ;
      }

      *fileno = 0 ;
      while ( !found && !feof( tapedescriptor ) ) {
	 while ( !feof( tapedescriptor ) ) {
	    fgets( line , MAXTXTLEN , tapedescriptor ) ;
	    if ( line[ 0 ] == '@' ) {
	       *fileno += 1 ;
	       if ( strlen( line ) >= 7 ) {
		  sscanf( &line[1] , "%3d%3d" , &filesop , &fileatt ) ;
		  if ( ( *sop == filesop ) && ( *att == fileatt ) ) {
		     found = 1 ;
		     break ;
		  }
		  if ( ( *sop < filesop ) || ( *att < fileatt ) ) {
		     break ;
		  }
	       }
	    }
	 }
      }

      (void) fclose( tapedescriptor ) ;
      instrumentno += 1 ;
   }

   if ( !found ) {
      *fileno = 0 ;
      strncpy( directory.a , " " , 2 ) ;
      return( BAD_SOPATT ) ;
   }


   return( OK ) ;
}

