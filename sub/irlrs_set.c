/* irlrs_set.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irlrs_set.dc2

Function:     irlrs_set

Purpose:      Basic LRS set access routines

Category:     LRS

File:         irlrs_set.c

Author:       P.R. Roelfsema

Description:

                The file irlrs_set.c contains all the basic LRS set
              creation and acess routines.
              The following are available:

              IRLRS_SET_CREATE  - creates an empty LRS set.
              IRLRS_SET_DELETE  - deletes an LRS set.
              IRLRS_SET_EXIST   - checks whether an LRS set exists.
              IRLRS_SET_ADDSNIP - extends an LRS set for new snips.

              IRLRS_SET_ENQUIRE - returns header info for an LRS set.
              IRLRS_SET_WRSNIP  - writes data for a snip to an LRS set.
              IRLRS_SET_RDSNIP  - reads data for a snip from an LRS set.

Updates:      Jan 29, 1991: PRR, Creation date

#<

*/

#include "string.h"
#include "stdlib.h"
#include "gipsyc.h"
#include "irds_exist.h"
#include "ircc_bandnr.h"
#include "gds_exist.h"
#include "gds_create.h"
#include "gds_extend.h"
#include "gds_delete.h"
#include "gdsc_word.h"
#include "gdsc_range.h"
#include "gdsd_rchar.h"
#include "gdsd_rint.h"
#include "gdsd_rreal.h"
#include "gdsd_rdble.h"
#include "gdsd_wchar.h"
#include "gdsd_wint.h"
#include "gdsd_wreal.h"
#include "gdsd_wdble.h"
#include "gdsd_delete.h"
#include "gdsi_write.h"
#include "gdsi_read.h"
#include "setnfblank.h"
#include "ircc_detnr.h"

#define EXISTSET	   1		/* lrsset exists */
#define OK		   0		/* no problems */
#define	BADIRDSIN	  -1		/* bad input irds */
#define NOEXISTSET	  -1		/* non existing set */
#define NOTLRSSET	  -2		/* existing irdsout is not LRS set */
#define OUTISNOTIN	  -3		/* irds in and out do not agree */
#define GDSWRERROR	  -3		/* gds write error */
#define GDSRDERROR	  -3		/* gds read error */
#define GDSPROBLEM	  -4		/* error from a gds call */
#define BADSNIP		  -5		/* snip not in lrsset */
#define BADSDET		  -6		/* sdet not in lrsset */

#define true		   1
#define false		   0
#define MAXTXTLEN	  80
#define MAXBUF		1024


#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.l = len ; }


/* irlrs_set_create.c

#>            irlrs_set_exist.dc2

Function:     irlrs_set_exist

Purpose:      Check wheter a GDS is an LRS set.

Category:     LRS

File:         irlrs_set.c

Author:       P.R. Roelfsema

Use:          IRLRS_SET_EXIST(                         integer
                                LRSSET  )      Input   character*(*)

              IRLRS_SET_EXIST return code:
                           0 - LRSSET exists, but has zero snips (i.e only
                               LRS_SET_CREATE has been used)
                          >0 - number of snips in LRSSET
                          -1 - LRSSET does not exist.
                          -2 - LRSSET exists, is not LRS set.
                          -4 - gds error.
              LRSSET      Name of LRS set.


Updates:      Jan 29, 1991: PRR, Creation date

#<

Fortran to C interface:

@ integer function irlrs_set_exist( character )

*/

fint irlrs_set_exist_c( fchar lrsset )
{
   fint okset , naxis ;
   fint level , error = 0 ;

   fchar cbuf ;

   finit( cbuf , MAXTXTLEN ) ;
   if ( gds_exist_c( lrsset , &error ) ) {
      okset = EXISTSET ;
   } else {
      okset = NOEXISTSET ;
   }

   if ( okset == EXISTSET ) {			/* does the set exist */
      level = 0 ;				/* get ORIGIN keyword */
      gdsd_rchar_c( lrsset, tofchar( "INSTRUME" ), &level, cbuf, &error ) ;
      if ( ( error < 0 )  || ( ircc_bandnr_c( cbuf ) != 5 ) ) 
                                                  return( NOTLRSSET ) ;
      gdsd_rchar_c( lrsset, tofchar( "CTYPE1" ), &level, cbuf, &error ) ;
      if ( ( error < 0 )  || ( strncmp( cbuf.a , "LAMBDA", 5 ) != 0 ) ) 
                                                  return( NOTLRSSET ) ;
      gdsd_rchar_c( lrsset, tofchar( "CTYPE2" ), &level, cbuf, &error ) ;
      if ( ( error < 0 )  || ( strncmp( cbuf.a , "SDET", 4 ) != 0 ) ) 
                                                  return( NOTLRSSET ) ;
      gdsd_rint_c( lrsset , tofchar( "NAXIS2" ) , &level , &naxis , &error ) ;
      if ( ( error < 0 )  || ( naxis != 6 ) ) return( NOTLRSSET ) ;
   }
   error = 0 ;					/* find number of snips */
   gdsd_rint_c( lrsset , tofchar( "NAXIS2" ) , &level , &okset , &error ) ;
   if ( error < 0 ) okset = 0 ;
   return( okset ) ;
}


/*
     Irds_set_compare checks whether an irds and an lrs set do exist, and
     if so checks whether they are compatible. 
*/
static fint irds_set_compare( fchar irds         , fchar lrsset    , 
                              fint  *irds_exists , fint *set_exists ) 
{
   fint   level ;
   fint   error = 0 ;
   double ival = 0 , lval = 0 ;

   fchar  origin ;

   finit( origin , MAXTXTLEN ) ;

   switch ( irds_exist_c( irds , &error ) ) {	/* does the irds exist ? */
      case  0 : *irds_exists = true ;		/* yes the irds does exist */
                break ;
      case -1 : *irds_exists = false ;		/* no, the irds doesn't exist */
                break ;
      default : *irds_exists = false ;		/* yes, but is not an irds */
                return( BADIRDSIN ) ;		/* return immediately */
                break ;
   }
 
   *set_exists = 0 ;
   switch ( irlrs_set_exist_c( lrsset ) ) {
      case NOTLRSSET   : return( NOTLRSSET ) ;
                         break ;
      case NOEXISTSET  : break ;
      case EXISTSET    : *set_exists = true ;
                         break ;
      default          : break ;
   }
   if ( *set_exists && *irds_exists ) {
						/* compare some keywords */
      gdsd_rdble_c( irds  , tofchar( "LONCENTR" ), &level, &ival, &error ) ;
      gdsd_rdble_c( lrsset, tofchar( "LONCENTR" ), &level, &lval, &error ) ;
      if ( ( error != level ) || ( ival != lval ) ) return( OUTISNOTIN ) ;
      gdsd_rdble_c( irds  , tofchar( "LATCENTR" ), &level, &ival, &error ) ;
      gdsd_rdble_c( lrsset, tofchar( "LATCENTR" ), &level, &lval, &error ) ;
      if ( ( error != level ) || ( ival != lval ) ) return( OUTISNOTIN ) ;
   }
   return( OK ) ;				/* OK -> return */
}


/* irlrs_set_create.c

#>            irlrs_set_create.dc2

Function:     irlrs_set_create

Purpose:      Create an LRS set.

Category:     LRS

File:         irlrs_set.c

Author:       P.R. Roelfsema

Use:          IRLRS_SET_CREATE( IRDSIN  ,      Input   character*(*)
                                LMLO    ,      Input   real
                                LMHI    ,      Input   real
                                LMSTEP  ,      Input   real
                                LRSSET  ,      Input   character*(*)
                                STATUS  )      Output  integer

              IRDSIN      Name of input irds to be used as a template
                          for filling in the header. If a non-existing
                          irds is given (e.g. an empty string) an LRS
                          set is created with a limited header (e.g. no
                          SATCAL information).
              LMLO        Begin wavelength of spectra ( micron ).
              LMHI        End wavelength of spectra ( micron ).
              LMSTEP      Step in wavelength between pixels ( micron ).
              LRSSET      Name of output LRS set.
              STATUS      Status return:
                           0 - no problems.
                          -1 - IRDSIN does exist but is not a good irds
                          -2 - LRSSET exists, is not LRS set.
                          -3 - LRSSET exists, does not agree with IRDSIN.
                          -4 - gds error.

Updates:      Jan 29, 1991: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irlrs_set_create( character , real      , real    , 
@                              real      , character , integer )

*/

void irlrs_set_create_c( fchar irdsin  , float *lmlo  , float *lmhi  ,
                         float *lmstep , fchar lrsset , fint *status ) 
{
   fint   level , axis, detnr ;
   fint   error = 0 ;
   fint   in_exist ;
   fint   out_exist ;
   fint   n ;

   double origin ;
   double dval ;
   fint   ival ;
   fchar  cval ;

   finit( cval , MAXTXTLEN ) ;
						/* compare in and out set */
   if ( ( *status = irds_set_compare( irdsin    , lrsset , 
                         &in_exist , &out_exist    ) ) < 0 )  return ;

   if ( !out_exist ) {
      gds_create_c( lrsset , &error ) ;		/* create the output set */
						/* create the WAVE axis */
      origin =   *lmlo ;
      ival   = ( *lmhi - *lmlo ) / *lmstep + 1 ;/* nr of wavelengths */
      gds_extend_c( lrsset, tofchar( "LAMBDA" )   , &origin, &ival, &error ) ;
      level  =      0 ;
      dval   =   *lmlo ;
      gdsd_wdble_c( lrsset, tofchar( "CRVAL1" ) , &level , &dval, &error ) ;
      dval   = *lmstep ;
      gdsd_wdble_c( lrsset, tofchar( "CDELT1" ) , &level , &dval, &error ) ;
						/* create the SDET axis */
      origin = 1.0 ;
      ival   =   6 ;				/* 5 LRS detectors + mean */
      level  =   0 ;
      gds_extend_c( lrsset, tofchar( "SDET" )   , &origin, &ival, &error ) ;
      dval   = 0.0 ;
      gdsd_wdble_c( lrsset, tofchar( "CRVAL2" ) , &level , &dval, &error ) ;
      dval   = 1.0 ;
      gdsd_wdble_c( lrsset, tofchar( "CDELT2" ) , &level , &dval, &error ) ;
      axis  = 2 ;
      level = 0 ;
      n     = 0 ;
      detnr = 0 ;
      level = gdsc_word_c( lrsset , &axis , &n , &level , &error ) ;
      gdsd_wint_c( lrsset, tofchar( "DETNO" ), &level , &detnr , &error ) ;
      for ( n = 1  ; n <= 5 ; n++ ) {
         if ( error ) {
            *status = GDSPROBLEM ;		/* create failed */
            return ;				/*    -> return */
         }
         detnr = ircc_detnr_c( &n , tofchar( "SURVEY LRS" ) ) ;
         level = 0 ;
         level = gdsc_word_c( lrsset , &axis , &n , &level , &error ) ;
         gdsd_wint_c( lrsset, tofchar( "DETNO" ), &level , &detnr , &error ) ;
      }
   }

   if ( in_exist ) {				/* get keys from input set */
      level = 0 ;	
      gdsd_rdble_c( irdsin, tofchar( "EPOCH" )   , &level, &dval, &error ) ;
      gdsd_wdble_c( lrsset, tofchar( "EPOCH" )   , &level, &dval, &error ) ;
      gdsd_rchar_c( irdsin, tofchar( "SKYSYS" )  , &level,  cval, &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "SKYSYS" )  , &level,  cval, &error ) ;
      gdsd_rchar_c( irdsin, tofchar( "OBJECT" )  , &level,  cval, &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "OBJECT" )  , &level,  cval, &error ) ;
      gdsd_rchar_c( irdsin, tofchar( "OBSERVER" ), &level,  cval, &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "OBSERVER" ), &level,  cval, &error ) ;
      gdsd_rchar_c( irdsin, tofchar( "BUNIT"    ), &level,  cval, &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "BUNIT"    ), &level,  cval, &error ) ;
      gdsd_rchar_c( irdsin, tofchar( "INSTRUME" ), &level,  cval, &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "INSTRUME" ), &level,  cval, &error ) ;
      gdsd_rchar_c( irdsin, tofchar( "TELESCOP" ), &level,  cval, &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "TELESCOP" ), &level,  cval, &error ) ;
      gdsd_rdble_c( irdsin, tofchar( "LONCENTR" ), &level, &dval, &error ) ;
      gdsd_wdble_c( lrsset, tofchar( "LONCENTR" ), &level, &dval, &error ) ;
      gdsd_rdble_c( irdsin, tofchar( "LATCENTR" ), &level, &dval, &error ) ;
      gdsd_wdble_c( lrsset, tofchar( "LATCENTR" ), &level, &dval, &error ) ;
      gdsd_rdble_c( irdsin, tofchar( "LONSIZE" ) , &level, &dval, &error ) ;
      gdsd_wdble_c( lrsset, tofchar( "LONSIZE" ) , &level, &dval, &error ) ;
      gdsd_rdble_c( irdsin, tofchar( "LATSIZE" ) , &level, &dval, &error ) ;
      gdsd_wdble_c( lrsset, tofchar( "LATSIZE" ) , &level, &dval, &error ) ;
   } else {					/* make keys from scratch */
      dval = 2000 ;
      gdsd_wdble_c( lrsset, tofchar( "EPOCH" )   , &level, &dval, &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "SKYSYS" )  , &level,
                                         tofchar( "EQUATORIAL" ), &error ) ;
      dval = 0 ;
      gdsd_wdble_c( lrsset, tofchar( "LONCENTR" ), &level, &dval, &error ) ;
      gdsd_wdble_c( lrsset, tofchar( "LATCENTR" ), &level, &dval, &error ) ;
      gdsd_wdble_c( lrsset, tofchar( "LONSIZE" ) , &level, &dval, &error ) ;
      gdsd_wdble_c( lrsset, tofchar( "LATSIZE" ) , &level, &dval, &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "BUNIT" )   , &level,
                                           tofchar( "DUMMY" ) , &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "INSTRUME" ), &level,
                                            tofchar( "LRS" )   , &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "TELESCOP" ), &level,
                                           tofchar( "IRAS" )  , &error ) ;

   }
}


/* irlrs_set_delete.c

#>            irlrs_set_delete.dc2

Function:     irlrs_set_delsnip

Purpose:      Delete an existing LRS set.

Category:     LRS

File:         irlrs_set.c

Author:       P.R. Roelfsema

Use:          IRLRS_SET_DELETE( LRSSET  ,      Input   character*(*)
                                STATUS  )      Output  integer

              LRSSET      Name of LRS set.
              STATUS      Status return:
                             0  - no problems.
                            -1  - LRSSET does not exists.
                            -2  - LRSSET exists, is not LRS set.
                          other - gds error code
                          For STATUS -1 or -2 LRSSET is not deleted.

Updates:      Jan 29, 1991: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irlrs_set_delete( character, integer )

*/

void irlrs_set_delete_c( fchar lrsset , fint *status ) 
{

   switch ( irlrs_set_exist_c( lrsset ) ) {	/* is this an LRS set ? */
      case NOTLRSSET   : *status = NOTLRSSET ;	/* NO, not an LRS set */
                         return ;		/*    -> return */
                         break ;
      case NOEXISTSET  : *status = NOEXISTSET  ;/* lrsset does not exist */
                         return ;		/*    -> return */
                         break ;
      case EXISTSET    : break ;
      default          : break ;
   }

   gds_delete_c( lrsset , status ) ;
}




/* irlrs_set_addsnip.c

#>            irlrs_set_addsnip.dc2

Function:     irlrs_set_addsnip

Purpose:      Add a detector snip to an existing LRS set.

Category:     LRS

File:         irlrs_set.c

Author:       P.R. Roelfsema

Use:          IRLRS_SET_ADDSNIP( LRSSET  ,      Input   character*(*)
                                 SNIP    ,      Input   integer
                                 SOP     ,      Input   integer
                                 ATT     ,      Input   integer
                                 SATCAL  ,      Input   real
                                 STATUS  )      Output  integer

              LRSSET      Name of LRS set.
              SNIP        Sequential snip number to add.
              SOP         SOP of original scan.
              ATT         ATT of original scan.
              SATCAL      satcal of mid point of spectrum.
              STATUS      Status return:
                           0 - no problems.
                          -1 - LRSSET does not exists.
                          -2 - LRSSET exists, is not LRS set.
                          -3 - error when setting snip to undefined.

Updates:      Jan 29, 1991: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irlrs_set_addsnip( character , integer , integer , 
@                               integer   , real    , integer )

*/

void irlrs_set_addsnip_c( fchar lrsset , fint  *snip   , fint *sop    , 
                          fint  *att   , float *satcal , fint *status ) 
{
   fint   level , axis , error = 0 ;
   fint   ival , nr ;
   fint   grid , naxis3 ;
   fint   maxbuf , todo ;
   fint   cwlo , cwhi ;

   float  buffer[ MAXBUF ] ;

   double dval ;
   double origin ;

   *status = OK ;
   switch ( irlrs_set_exist_c( lrsset ) ) {	/* is this an LRS set ? */
      case NOTLRSSET   : *status = NOTLRSSET ;	/* NO, not an LRS set */
                         return ;		/*    -> return */
                         break ;
      case NOEXISTSET  : *status = NOEXISTSET  ;/* lrsset does not exist */
                         return ;		/*    -> return */
                         break ;
      case EXISTSET    : break ;
      default          : break ;
   }

   level = 0 ;
   gdsd_rint_c( lrsset , tofchar( "NAXIS" ) , &level , &ival , &error ) ;
   if ( ival == 2 ) {				/* snip-axis not present */
      naxis3 =   0 ;
      origin = 1.0 ;				/* create snip axis */
      ival   = *snip + 1 ;
      gds_extend_c( lrsset, tofchar( "SNIP" )  , &origin, &ival  , &error ) ;
      dval   = 0.0 ;
      gdsd_wdble_c( lrsset, tofchar( "CRVAL3" ), &level , &dval  , &error ) ;
      dval   = 1.0 ;
      gdsd_wdble_c( lrsset, tofchar( "CDELT3" ), &level , &dval  , &error ) ;
      gdsd_wchar_c( lrsset, tofchar( "CTYPE3" ), &level , 
                                              tofchar( "SNIP" )  , &error ) ;
   } else {
      gdsd_rint_c(  lrsset, tofchar( "NAXIS3" ), &level , &naxis3, &error ) ;
      if ( ( *snip ) > naxis3 - 1 ) {		/* snip is not in lrsset */
         naxis3 = *snip + 1 ;
         origin = 1.0 ;				/* extend snip axis */
         gds_extend_c( lrsset, tofchar( "SNIP" ), &origin, &naxis3, &error ) ;
      }
   }
   level  =   0 ;				/* find snip in lrsset */
   axis   =   3 ;				/* to write sop and att */
   error  =   0 ;
   grid   = *snip ;
   level  = gdsc_word_c( lrsset , &axis , &grid , &level , &error ) ;
   error  =   0 ;
   gdsd_wint_c(  lrsset , tofchar( "SOP"    ) , &level , sop    , &error ) ;
   gdsd_wint_c(  lrsset , tofchar( "ATT"    ) , &level , att    , &error ) ;
   gdsd_wreal_c( lrsset , tofchar( "SATCAL" ) , &level , satcal , &error ) ;
   gdsc_range_c( lrsset , &level , &cwlo , &cwhi , &error ) ;

/* next write blanks to the set in the added snip(s).... */
   level  = 0 ;
   nr     = 1 ;
   gdsd_rint_c( lrsset, tofchar( "NAXIS1" ), &level , &ival, &error ) ;
   nr     = nr * ival ;
   gdsd_rint_c( lrsset, tofchar( "NAXIS2" ), &level , &ival, &error ) ;
   nr     = nr * ival ;				/* nr. pixels per snip */

   maxbuf = MAXBUF ;				/* buffer length */
   setnfblank_c( buffer , &maxbuf ) ;		/* set buffer to undefined */
   todo   = 0 ;					/* nothing done yet */

   for ( ; ; ) {
      gdsi_write_c( lrsset , &cwlo , &cwhi , buffer , &maxbuf , &nr , &todo ) ;
      if ( todo == 0 ) break ;
      if ( todo < 0 ) { 
         *status = GDSWRERROR ;
         return ;
      }
   }
}


/* irlrs_set_wrsnip.c

#>            irlrs_set_wrsnip.dc2

Function:     irlrs_set_wrsnip

Purpose:      Write data for a detector snip to an existing LRS set.

Category:     LRS

File:         irlrs_set.c

Author:       P.R. Roelfsema

Use:          IRLRS_SET_WRSNIP( LRSSET  ,      Input   character*(*)
                                SNIP    ,      Input   integer
                                SDET    ,      Input   integer
                                DATA    ,      Input   real( >NDATA )
                                NDATA   ,    In/Output integer
                                STATUS  )      Output  integer

              LRSSET      Name of LRS set.
              SNIP        Sequential snip number to add.
              SDET        Sequential detector number to add.
                           0  - mean spectrum
                          1-5 - spectra for detectors 71 - 74
              DATA        Array containing the spectrum.
              NDATA       In  - number of elements of DATA to write
                          Out - number of elements of DATA written
              STATUS      Status return:
                           0 - no problems.
                          -1 - LRSSET does not exists.
                          -2 - LRSSET exists, is not LRS set.
                          -3 - gds write error.
                          -5 - SNIP not in LRSSET.
                          -6 - SDET not in LRSSET.

Updates:      Jan 29, 1991: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irlrs_set_wrsnip( character , integer , integer , 
@                              real      , integer , integer )

*/

void irlrs_set_wrsnip_c( fchar lrsset , fint *snip  , fint *sdet   , 
                         float *data  , fint *ndata , fint *status ) 
{
   fint  level , error = 0 , cwlo , cwhi , todo , nr ;
   fint  axis  , naxis ;

   *status = OK ;
   switch ( irlrs_set_exist_c( lrsset ) ) {	/* is this an LRS set ? */
      case NOTLRSSET   : *status = NOTLRSSET ;	/* NO, not an LRS set */
                         return ;		/*    -> return */
                         break ;
      case NOEXISTSET  : *status = NOEXISTSET  ;/* lrsset does not exist */
                         return ;		/*    -> return */
                         break ;
      case EXISTSET    : break ;
      default          : break ;
   }

   level = 0 ;					/* find snip in lrsset */
   axis  = 3 ;
   level = gdsc_word_c( lrsset , &axis , snip , &level , &error ) ;
   if ( error < 0 ) {
      *status = BADSNIP ;
      return ;					/* snip not present -> exit */
   }
   axis  = 2 ;					/* find sdet in lrsset */
   level = gdsc_word_c( lrsset , &axis , sdet , &level , &error ) ;
   if ( error < 0 ) {
      *status = BADSDET ;
      return ;					/* sdet not present -> exit */
   }

   gdsc_range_c( lrsset , &level , &cwlo , &cwhi , &error ) ;
   level = 0 ;
   nr    = 1 ;
   gdsd_rint_c( lrsset , tofchar( "NAXIS1" ) , &level , &naxis , &error ) ;
   nr    = nr * naxis ;
   gdsd_rint_c( lrsset , tofchar( "NAXIS2" ) , &level , &naxis , &error ) ;
   nr    = nr * naxis ;
   if ( nr < *ndata ) *ndata = nr ;
   todo  = 0 ;
   for ( ; ; ) {
      gdsi_write_c( lrsset , &cwlo , &cwhi , data , ndata , &nr , &todo ) ;
      if ( todo == 0 ) break ;
      if ( todo  < 0 ) {
         *status = GDSWRERROR ;
         return ;
      }
   }
}


/* irlrs_set_wrsnip.c

#>            irlrs_set_rdsnip.dc2

Function:     irlrs_set_rdsnip

Purpose:      Read data for a detector snip from an existing LRS set.

Category:     LRS

File:         irlrs_set.c

Author:       P.R. Roelfsema

Use:          IRLRS_SET_RDSNIP( LRSSET  ,      Input   character*(*)
                                SNIP    ,      Input   integer
                                SDET    ,      Input   integer
                                DATA    ,      Input   real( >NDATA )
                                NDATA   ,    In/Output integer
                                STATUS  )      Output  integer

              LRSSET      Name of LRS set.
              SNIP        Sequential snip number to add.
              SDET        Sequential detector number to add.
                           0  - mean spectrum
                          1-5 - spectra for detectors 71 - 74
              DATA        Array receiving the spectrum.
              NDATA       In  - maximum number of elements te read into DATA 
                          Out - number of elements actually read
              STATUS      Status return:
                           0 - no problems.
                          -1 - LRSSET does not exists.
                          -2 - LRSSET exists, is not LRS set.
                          -3 - gds read error.
                          -5 - SNIP not in LRSSET.
                          -6 - SDET not in LRSSET.

Updates:      Jan 29, 1991: PRR, Creation date

#<

Fortran to C interface:

@ subroutine irlrs_set_rdsnip( character , integer , integer , 
@                              real      , integer , integer )

*/

void irlrs_set_rdsnip_c( fchar lrsset , fint *snip  , fint *sdet   , 
                         float *data  , fint *ndata , fint *status ) 
{
   fint  level , error = 0 , cwlo , cwhi , todo , nr ;
   fint  axis  , naxis ;

   *status = OK ;
   switch ( irlrs_set_exist_c( lrsset ) ) {	/* is this an LRS set ? */
      case NOTLRSSET   : *status = NOTLRSSET ;	/* NO, not an LRS set */
                         return ;		/*    -> return */
                         break ;
      case NOEXISTSET  : *status = NOEXISTSET  ;/* lrsset does not exist */
                         return ;		/*    -> return */
                         break ;
      case EXISTSET    : break ;
      default          : break ;
   }

   level = 0 ;					/* find snip in lrsset */
   axis  = 3 ;
   level = gdsc_word_c( lrsset , &axis , snip , &level , &error ) ;
   if ( error < 0 ) {
      *status = BADSNIP ;
      return ;					/* snip not present -> exit */
   }
   axis  = 2 ;					/* find sdet in lrsset */
   level = gdsc_word_c( lrsset , &axis , sdet , &level , &error ) ;
   if ( error < 0 ) {
      *status = BADSDET ;
      return ;					/* sdet not present -> exit */
   }

   gdsc_range_c( lrsset , &level , &cwlo , &cwhi , &error ) ;
   level = 0 ;
   nr    = 0 ;
   gdsd_rint_c( lrsset , tofchar( "NAXIS1" ) , &level , &naxis , &error ) ;
   nr    = nr * naxis ;
   gdsd_rint_c( lrsset , tofchar( "NAXIS2" ) , &level , &naxis , &error ) ;
   nr    = nr * naxis ;
   if ( nr < *ndata ) *ndata = nr ;
   todo  = 0 ;
   for ( ; ; ) {
      gdsi_read_c( lrsset , &cwlo , &cwhi , data , ndata , &nr , &todo ) ;
      if ( todo == 0 ) break ;
      if ( todo  < 0 ) {
         *status = GDSRDERROR ;
         return ;
      }
   }
}

