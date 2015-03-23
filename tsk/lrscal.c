/* identification */
#define VERSION		"2.9" 				/* version number */
#define PROGRAM         "LRSCAL"			/* program name */

/* lrscal.c

                            COPYRIGHT (c) 1990
            Kapteyn Astronomical Institute - University of Groningen
                P.O. Box 800, 9700 AV Groningen, The Netherlands

#>            lrscal.dc1

Program:      LRSCAL

Purpose:      Generate a calibrated LRS spectrum from data in an IRDS

Category:     IRAS

File:         lrscal.c

Author:       P.R. Roelfsema

Keywords:

    IRSET=    Name of input IR data set [quit]

    IRSETOUT= Name of output IR data set [no output]

    POS=      Position at which spectrum is to be generated [ same as before ]
              Giving Q(UIT) at this keyword will stop the program.

    SNIP=     Give snip number of spectrum to process [ all remaining spectra ]
              Possible answers are :
               Q(UIT) - loop the program back to the POS= keyword.
               A(LL)  - (re)process all spectra in the data set.
                 n    - process snip number n.
              If all spectra have been calibrated <CR> has the same effect 
              as typing Q(UIT).

*** ISLIM=    In-Scan offset where spectrum extraction starts in arcsec [ 2000 ]

*** SINGLE=   Use single baseline component ? [ N ]
              The baseline subtraction for LRS is best done using two
              separate baselines; one for those data from before the source
              crossed the aperture and one derived from the data after
              the aperture crossing. This is the standard mode (SINGLE=N).
              In low signal to noise conditions this approach gives rise
              to wildly varying baslines, and thus in that case it is
              better to determine one global baseline level, this can be
              forced by specifying SINGLE=Y.

*** BWIN=     Set default window for ALL snips [  -1000 -250 250 1000 ]

*** BWINn=    Windows (inscan) for baseline of snip n [ BWIN= value]
              This keyword can be used to specify up to two windows in the 
              in-scan direction. Through the data within these windows a
              straight line is fitted which is subsequently subtracted
              from the data. e.g. BWIN3=-300 -200 200 300 (in arcsec).

    UNITS=    Give units of data after calibration [LFL]
              Options are: 
			Jy   - F(lambda) in Jy
			Wm   - F(lambda) in Wm^-2mu^-1
			LFL  - lambda*F(lambda) in Wm^-2
                        L4FL - lambda^4*F(lamba) in Wm^-2mu^3

 GRDEVICE=    Give graphics device for display [ list available devices ]

***COHEN=     Apply Cohen correction factors? [ Yes ]

***MXGAIN=    Specify maximum cross-scan gain to be applied [ 2 ]
              This keyword allows the user to discard data taken with
              the source of interest very close to the edge of a 
              detector. This is done by setting a limit on the
              maximum correction for the cross-scan gain variations.
              The cross-scan gain corrections vary from 1.0 at the
              centre of the detectors upto 5-6 near the edges. 

***ALIGN=     Align spectral halves? [ Yes ]
              If no is specified the short wave and long wave spectral
              halves are not aligned.

***MINMAX=    Min and max levels to be plotted [ min and max of data ].

***PSC=       Point source catalogue fluxes (Jy) at 12 and 25 mu [ none ].

***THICK=     Do you want thick lies in plot? [ Yes ].

Updates:      May 10, 1990: PRR, Document created.
              Dec 17, 1990: PRR, Version 1.0.
              Feb 11, 1991: PRR, Upgrade for PGPLOT.
              Feb 13, 1991: PRR, Major rewrite.
              Jun 12, 1991: PRR, new flux-calibration.
              Aug  9, 1991: PRR, min nr. of ticks implemented
                                 rms noise calculation.
              Oct 10, 1991: WZ,  PGPLOT standard names implemented
              Mar 19, 1992: HB,  New interface to irds_enquire_snip
              Sep 23, 1992: PRR, Changed BWIN default, baseline
                                 subtraction, more UNITS.
              Mar 23, 1993: PRR, GRDEVICE not cancelled anymore
	      Jul 20, 1993: PRR, added Cohen correction factors
              Jul 22, 1993: PRR, added MXGAIN and ALIGN options
              Aug  9, 1993: PRR, solved (?) crashes on HP
	      Aug 11, 1993: PRR, changed BWIN= to BWINn=
              Aug 13, 1993: PRR, added BWIN= to BWINn=
#<

*/


#include "stdio.h"
#include "string.h"
#include "ctype.h"
#include "stdlib.h"
#include "math.h"
#include "gipsyc.h"
#include "cmain.h"
#include "nelc.h"
#include "init.h"
#include "finis.h"
#include "anyout.h"
#include "status.h"
#include "cancel.h"
#include "error.h"
#include "gds_exist.h"
#include "gdsc_word.h"
#include "gdsd_rchar.h"
#include "gdsd_wchar.h"
#include "gdsd_rint.h"
#include "gdsd_rreal.h"
#include "userint.h"
#include "userlog.h"
#include "userreal.h"
#include "userdble.h"
#include "userangle.h"
#include "usertext.h"
#include "sortra.h"
#include "minmax4.h"
#include "setfblank.h"
#include "spline1.h"
#include "irds_exist.h"
#include "irds_enquire.h"
#include "irds_enquire_snip.h"
#include "irds_rd_samples.h"
#include "irds_rd_detoff.h"
#include "ircc_bandnr.h"
#include "ircc_detnr.h"
#include "ircc_mask.h"
#include "ircc_times.h"
#include "irlrs_set_exist.h"
#include "irlrs_set_create.h"
#include "irlrs_set_addsnip.h"
#include "irlrs_set_wrsnip.h"
#include "irlrs_dettype.h"
#include "irlrs_pos2wave.h"
#include "irlrs_jyperpwm2.h"
#include "irlrs_pwm2permv.h"
#include "irlrs_lgain.h"
#include "irlrs_cohen_gain.h"
#include "irlrs_xgain.h"
#include "irlrs_adroop.h"
#include "irco_precess.h"
#include "irco_number.h"
#include "pgbeg.h"
#include "pgqinf.h"
#include "pgswin.h"
#include "pgsvp.h"
#include "pgsch.h"
#include "pgslw.h"
#include "pgsls.h"
#include "pgtext.h"
#include "pgptxt.h"
#include "pgmtxt.h"
#include "pgdraw.h"
#include "pgmove.h"
#include "pgline.h"
#include "pgbox.h"
#include "pglab.h"
#include "pgiden.h"
#include "pgend.h"

/* definitions for error levels */

static  fint            error_level_fatal      =   4;
#define FATAL_ERROR     ( &error_level_fatal   )
/*static  fint            error_level_serious    =   3;
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
#define INSET_KEY	tofchar("IRSET=")
#define INSET_MES	tofchar("Give input LRS data set [quit]")
#define OUTSET_KEY	tofchar("IRSETOUT=")
#define OUTSET_MES	tofchar("Give output LRS data set [no output set]")
#define POS_KEY		tofchar("POS=")
#define	SNIP_KEY	tofchar("SNIP=")
#define	ISLIM_KEY	tofchar("ISLIM=")
#define	ISLIM_MES	tofchar("In-Scan offset where spectrum extraction starts in arcsec [ 500 ]")
#define	THPF_KEY	tofchar("THPF=")
#define	THPF_MES	tofchar("Time constant for high-pass filter in seconds [ 10 ]")
#define	ADLIM_KEY	tofchar("ADLIM=")
#define	ADLIM_MES	tofchar("Anti-droop limits in V [ 0.01 , 0.01 , 0.01 , 0.01 , 0.01 ]")
#define	UNITS_KEY	tofchar("UNITS=")
#define	UNITS_MES	tofchar("Give units of data after calibration [LFL] ")
#define	COHEN_KEY	tofchar("COHEN=")
#define	COHEN_MES	tofchar("Apply Cohen correction factors? [ Yes ]" )
#define	MXGAIN_KEY	tofchar("MXGAIN=")
#define	MXGAIN_MES	tofchar("Specify max. cross-scan gain [ 2 ]" ) 
#define	ALIGN_KEY	tofchar("ALIGN=")
#define	ALIGN_MES	tofchar("Align the spectral halves? [ Yes ]" )
#define MINMAX_KEY	tofchar("MINMAX=")
#define MINMAX_MES	tofchar("Min and max levels to be plotted [ min and max of data ]")
#define	PSC_KEY		tofchar("PSC=")
#define	PSC_MES		tofchar("Point source catalogue fluxes (Jy) at 12 and 25 mu [ none ]")
#define	THICK_KEY	tofchar("THICK=")
#define	THICK_MES	tofchar("Do you want thick lies in plot? [ Yes ]" )

#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            if( !fc.a ) error_c( WARNING , tofchar( "FINIT problem" ) ) ; \
                            fc.a[ len ] = 0 ; \
                            fc.l = len ; }

/* miscellaneous definitions */
#define true               1
#define false              0
#define MAXTXTLEN        160			/* length of textlines */
#define PI		(double) 3.14159265358979 
#define RADPERDEG	( PI / (double) 180.0 )	/* radians per degree */
#define EPS		(double) 1e-9

#define NRDETS		   5			/* number of LRS detectors */
#define SAMPPERSAT 	  32			/* number of samps/satcal */
#define MAXSATS		1000			/* maximum number of satcals */
#define	MAXSNIPS	 100			/* maximum number of snips */
#define NRSAMP		MAXSATS * SAMPPERSAT	/* nr of samples in spectra */
#define SPECLENGTH 	  20			/* rd 20 ticks around source */
#define MINSPECLENGTH	   5			/* min. nr. of ticks */
#define SPECSAMP	SPECLENGTH * SAMPPERSAT	/* nr of samples extracted */
#define ISCANLIM	(float) 2000.0		/* start read 2000" from src */

#define BEGINWAVE	  2.0			/* start wavelength (micron) */
#define	STEPWAVE	  0.1			/* wavelength step (micron) */
#define	NWAVE		250			/* nr. of wavelengths */

#define	LWHIEDGE	 22.5			/* hi wavel. edge of LW det. */
#define	LWLOEDGE	 11.0			/* low wavel. edge of LW det. */
#define	SWHIEDGE	 13.25			/* hi wavel. edge of SW det. */
#define	SWLOEDGE	  8.0			/* low wavel. edge of SW det. */

static fint first_spectrum_printed = false ;	/* is the first sp. printed? */


/* type definitions */

typedef struct {				/* type for set info */
   char			name[ MAXTXTLEN ] ;	/* IRDS name */
   double		lonlat[2] ;		/* position of LRS spectra */
   double               center[2] ;		/* center of IRDS */
   double               size[2] ;		/* on sky size of IRDS */
   fint			nsnips ;		/* number of snips in set */
   fint			cosys ;			/* coordinate system of set */
   char                 coname[ MAXTXTLEN ] ;	/* name of coord. system */
   char                 object[ MAXTXTLEN ] ;	/* name of object */
   float                epoch ;			/* epoch of coord. system */
}  LRSSet ;				

typedef long *NextSpectrumPtr ;			/* pointer to next spectrum */

typedef struct {				/* type for spectrum list */
   fint			snip ;			/* snip nr. of spectrum */
   fint			snipdur ;		/* length of original snip */
   fint			satcal ;		/* satcal of begin of snip */
   fint                 sop ;			/* SOP of spectrum */
   fint                 att ;			/* ATT of spectrum */
   fint			process ;		/* process this spectrum ? */
   fint			done ;			/* is this spectrum done ? */
   NextSpectrumPtr	next ;			/* pointer to next spectrum */
}  Spectrum ;


typedef struct {				/* type: data of 1 detector */
   bool                 onsource ;		/* did detector see source? */
   fint			detno ;			/* detector number */
   fint			ns ;			/* nr. of samples in spectrum */
   float		sigfl ;			/* error in flux */
   float		siglm ;			/* error in lambda */
   float		is[ SPECSAMP ] ;	/* in-scan offsets */
   float		xs[ SPECSAMP ] ;	/* cross-scan offsets */
   float		lm[ SPECSAMP ] ;	/* calibrated wavelengths */
   float		fl[ SPECSAMP ] ;	/* calibrated fluxes */
   float		max ;			/* maximum flux */
   float		min ;			/* minimum flux */
   fint			nspec ;			/* number of spectra averaged */
} DetData ;

typedef struct {				/* type: raw LRS snip data */
   fint			snip ;			/* snip nr. spectrum */
   float		satcal ;		/* satcal of mid of spectrum */
   char			units[ MAXTXTLEN ] ;	/* units of fluxes */
   DetData		det[ NRDETS + 1 ] ;	/* all detectors + total */
   float		wave[ NWAVE ] ;		/* wavelengths for resampling */
}  LRSData ;


/* 
   InitialiseSpectrum is used to initailize one element in the list of spectra
which are to be processed by LRSCAL. The next element will be NULL after
initialization, the spectrum is not processed yet, and should be processed.
The argument of InitialiseSpectrum can be used to identify a sequence of 
spectra.
*/

Spectrum *InitialiseSpectrum( fint snip , fint snipdur , fint satcal ,
                              fint sop  , fint att )
{
   Spectrum *spectrum;

   anyout_c( ANYOUT_TST , tofchar(" - InitialiseSpectrum") ) ;
   spectrum = NULL ;
   spectrum = malloc( sizeof( Spectrum ) ) ;
   if ( spectrum ){
      spectrum->snip    =    snip ;
      spectrum->snipdur = snipdur ;
      spectrum->satcal  =  satcal ;
      spectrum->sop     =     sop ;
      spectrum->att     =     att ;
      spectrum->process =   false ;
      spectrum->done    =   false ;
      spectrum->next    =    NULL ;
      return( spectrum );
   } else {
      error_c( FATAL_ERROR , tofchar( "Cannot get memory for list-element") ) ;
      return( NULL );
   }
}					/* InitialiseSpectrum */


/*
   LastSpectrum finds out whether the current spectrum is the last
to be processed.

*/
bool LastSpectrum( Spectrum *spectrum )
{
   Spectrum *temp ;

   temp = spectrum ;
   while ( temp ) {
      if ( temp->process && !temp->done ) {
         return( false ) ;
      }
      temp = (Spectrum *) temp->next ;
   }
   return( true ) ;
}					/* LastSpectrum */

/*
   InitialiseLRSData is used to Initialise the LRSData struct for further use.
*/

LRSData *InitialiseLRSData( fint snip )
{
   LRSData *raw = NULL ;

   fint    n , sdet ;

   float   blank ;

   anyout_c( ANYOUT_TST , tofchar(" - InitialiseLRSData") ) ;

   setfblank_c( &blank ) ;			/* find blank value */

   raw = malloc( sizeof( LRSData ) ) ;
   if ( !raw )
      error_c( FATAL_ERROR , tofchar( "Cannot get memory to store data" ) ) ;
   raw->snip   = snip ;				/* snip nr. spectrum */
   raw->satcal =  0.0 ;				/* satcal of mid-spectrum */
   sprintf( &raw->units[ 0 ] , "pWm-2" ) ;	/* units of fluxes */

   for ( sdet = 0 ; sdet <= NRDETS ; sdet++ ) {	/* data for all detectors */
      raw->det[ sdet ].onsource = false ;	/* source not (yet) seen */
      raw->det[ sdet ].detno    = 70 + sdet ;	/* set detector number */
      raw->det[ sdet ].ns       =   0 ;		/* nr. of samples in spectrum */
      raw->det[ sdet ].sigfl    = 0.0 ;		/* no errors yet */
      raw->det[ sdet ].siglm    = 0.0 ;		/* no errors yet */
      raw->det[ sdet ].max      = 0.0 ;		/* no data yet */
      raw->det[ sdet ].min      = 0.0 ;		/* no data yet */
      raw->det[ sdet ].nspec    =   0 ;		/* zero spectra averaged */
      for ( n = 0 ; n < SPECSAMP ; n++ ) {
         raw->det[ sdet ].is[ n ] = 0.0 ;	/* no data yet */
         raw->det[ sdet ].xs[ n ] = 0.0 ;	/* no data yet */
         raw->det[ sdet ].lm[ n ] = 0.0 ;	/* no data yet */
         if ( snip == 0 ) {
            raw->det[ sdet ].fl[ n ] = 0.0 ;	/* no average yet */
         } else {
            setfblank_c( &raw->det[ sdet ].fl[ n ] ) ;	/* no data yet */
         }
      }
   }
   raw->det[ 0 ].detno = 0 ;			/* total spectrum in sdet 0 */

   for ( n = 0 ; n < NWAVE ; n++ ) {
     raw->wave[ n ] = BEGINWAVE + n * STEPWAVE ;/* wavelengths for average */
   }

   return( raw );
}						/* InitialiseLRSSet */

/*
   InitialiseLRSSet is used to Initialise the LRSSet struct for further use.
*/

LRSSet *InitialiseLRSSet( fchar setname )
{
   LRSSet *set = NULL ;

   anyout_c( ANYOUT_TST , tofchar(" - InitialiseLRSSet") ) ;
   set = malloc( sizeof(LRSSet) );
   if( !set ) error_c( FATAL_ERROR , tofchar( "Cannot create set-pointer" ) );
   set->name[ 0 ]   = 0 ;
   set->lonlat[0]   = 0 ;
   set->lonlat[1]   = 0 ;
   set->nsnips      = 0 ;
   set->cosys       = 0 ;
   set->coname[ 0 ] = 0 ;
   set->object[ 0 ] = 0 ;
   set->epoch       = 0 ;
   sprintf( set->name , "%.*s" , nelc_c( setname ) , setname.a ) ;
   return( set );
}						/* InitialiseLRSSet */


/*
     BaseSub subtracts a straight line baseline from the data if the
user gives limits in inscan-distance where data should be used to fit
the baseline to .
*/

void BaseSub( float *x , float *y , fint *ndata , fint *snip )
{
   bool  single = false ;

   char  line[ MAXTXTLEN ] , key[ MAXTXTLEN ] ;

   fint  n , as = 0 , bs = 0 , one = 1 , four = 4 ;
 
   float aslope = 0 , ainter = 0 ;
   float bslope = 0 , binter = 0 ;
   float wnds[ 4 ] ;
   float asx = 0.0 , asxx = 0.0 , asy = 0.0 , asxy = 0.0 ;
   float bsx = 0.0 , bsxx = 0.0 , bsy = 0.0 , bsxy = 0.0 ;
   float   blank ;
   float maxX , minX , yloc = 0 , zloc = 0 , ysize = 0 , zsize = 0 ;

   anyout_c( ANYOUT_TST , tofchar(" - BaseSub") ) ;

   setfblank_c( &blank ) ;			/* find blank value */

   n = userlog_c( &single , &one , DFLT_HIDD , tofchar( "SINGLE=" ) , 
                   tofchar( "Use a single baseline? [ N }" ) ) ;

   wnds[ 0 ] = -1000 ;
   wnds[ 1 ] = -250 ;
   wnds[ 2 ] =  250 ;
   wnds[ 3 ] =  1000 ;

   sprintf( key , "BWIN=" ) ;
   sprintf( line , "Default window for all snips [ %6.1f %6.1f %6.1f %6.1f ]" , 
            wnds[ 0 ] , wnds[ 1 ] , wnds[ 2 ] , wnds[ 3 ] ) ;
   n = userreal_c( wnds , &four , DFLT_HIDD , tofchar( key ) , tofchar( line ) ) ;

   sprintf( key , "BWIN%d=" , *snip ) ;
   sprintf( line , "Windows for baseline in snip %d [ %6.1f %6.1f %6.1f %6.1f ]" , 
            *snip , wnds[ 0 ] , wnds[ 1 ] , wnds[ 2 ] , wnds[ 3 ] ) ; 
   n = userreal_c( wnds , &four , DFLT_HIDD , tofchar( key ) , tofchar( line ) ) ;

   if ( fabs( wnds[ 0 ] ) <= 1e-5 ) return ;

   sortra_c( wnds , &n ) ;
   for ( n = 0 ; n < 4 ; n++ ) 
      wnds[ n ] = wnds[ n ] / 3600 ;

   for ( n = 0 ;  n < *ndata ; n++ ) {
      if ( ( x[ n ] > wnds[ 0 ] ) && ( x[ n ] < wnds[ 1 ] ) && 
                                     ( y[ n ] != blank ) ) {
         as   += 1 ;
         asx  += x[ n ] ;
         asxx += x[n ] * x[ n ] ;
         asy  += y[ n ] ;
         asxy += x[ n  ] * y[ n ] ;
      }
      if ( ( x[ n ] > wnds[ 2 ] ) && ( x[ n ] < wnds[ 3 ] ) &&
                                     ( y[ n ] != blank ) ) {
         bs   += 1 ;
         bsx  += x[ n ] ;
         bsxx += x[n ] * x[ n ] ;
         bsy  += y[ n ] ;
         bsxy += x[ n  ] * y[ n ] ;
      }
   }

   if ( as == 0 && bs == 0 ) return ;

   if ( single ) {
      as   = as   + bs   ;
      asx  = asx  + bsx  ;
      asxx = asxx + bsxx ;
      asy  = asy  + bsy  ;
      asxy = asxy + bsxy ;
      ainter = ( asxx * asy  - asx * asxy ) / ( as * asxx - asx * asx ) ;
      aslope = (   as * asxy - asx * asy  ) / ( as * asxx - asx * asx ) ;
      binter = ainter ;
      bslope = aslope ;
   } else {
      if ( as != 0 ) {
         ainter = ( asxx * asy  - asx * asxy ) / ( as * asxx - asx * asx ) ;
         aslope = (   as * asxy - asx * asy  ) / ( as * asxx - asx * asx ) ;
      } 
      if ( bs != 0 ) {
         binter = ( bsxx * bsy  - bsx * bsxy ) / ( bs * bsxx - bsx * bsx ) ;
         bslope = (   bs * bsxy - bsx * bsy  ) / ( bs * bsxx - bsx * bsx ) ;
      }
   }

   n = -5 ;
   (void) ircc_mask_c( &n , &yloc , &zloc , &ysize , &zsize ) ;
   maxX =  ysize / 2 /60 ;
   minX = -ysize / 2 /60 ;
   for ( n = 0 ; n <= *ndata ; n++ ) {
      if ( x[ n ] <= maxX && y[ n ] != blank ) {
         y[ n ] = y[ n ] - ( ainter + aslope * x[ n ] ) ;
      } else if ( y[ n ] != blank ) {
         y[ n ] = y[ n ] - ( binter + bslope * x[ n ] ) ;
      }
   }
}					/* BaseSub */


/*
   GetSpectrumList will create the list of spectra for processing. This
is done by searching the input IRDS coordinate system for positions
corresponding to those given by the user, and subsequently putting
the relevant positioning parameters in the spectrum list. These can than
later be used to extract the data.
*/
   
Spectrum *GetSpectrumList( LRSSet *set , double lonlat[ 2 ] ,
                           Spectrum *spectrumlist ) 
{
   Spectrum *temp ;

   char     line[ MAXTXTLEN ] ;

   fchar    scantype ;

   float    psi , psirate , theta ;

   fint     sop , obs , att , detno , zero = 0 , one = 1 , sunref ;
   fint     scancal , scandur , snipcal , snipdur ;
   fint     snip , sample , status , nsamples ;

   double   inscan[ NRSAMP ] , xscan[ NRSAMP ] , twist[ NRSAMP ] ;
   double   lon , lat ;

   finit( scantype , MAXTXTLEN ) ;
   anyout_c( ANYOUT_TST , tofchar(" - GetSpectrumList") ) ;

   if ( ( spectrumlist == NULL ) ||
        ( fabs( lonlat[ 0 ] - set->lonlat[ 0 ] ) > EPS ) ||
        ( fabs( lonlat[ 1 ] - set->lonlat[ 1 ] ) > EPS ) ) {
      while ( spectrumlist != NULL ){		/* clear the spectrumlist */
         temp = spectrumlist ;
         spectrumlist = (Spectrum *) spectrumlist->next ;
         free( temp ) ;
      }
      sprintf( line , "Searching set %s for snips passing (%7.2f,%7.2f)" ,
                       set->name , set->lonlat[ 0 ] , set->lonlat[ 1 ] ) ;
      status_c( tofchar( line ) ) ;
   
   						/* Initialise top of list */
      spectrumlist  = InitialiseSpectrum( 0 , 0 , 0 , 0 , 0 );
      temp          = spectrumlist ;
      temp->process = false ;			/* top is dummy */
   
      detno = 0 ;					/* boresight */
   
      sunref = 5 ;
      lon = set->lonlat[ 0 ] * RADPERDEG ;
      lat = set->lonlat[ 1 ] * RADPERDEG ;
      for ( snip = 1 ; snip <= set->nsnips ; snip++ ) {
         sprintf( line , "Checking if %s, snip %d is passing (%7.2f,%7.2f)" ,
                          set->name , snip ,
                          set->lonlat[ 0 ] , set->lonlat[ 1 ] ) ;
         status_c( tofchar( line ) ) ;
   						/* read snip-stuff */
         irds_enquire_snip_c( tofchar( set->name ) , &snip     , &sop     , 
			      &obs     , &att      , scantype  , &scancal , 
		              &scandur , &snipcal  , &snipdur  , &psi     , 
		              &psirate , &theta    , &status   ) ;
         sprintf( line , "Considering snip %4d, sop/att (%3d,%3d) (status %d)" , 
                          snip , sop , att , status ) ;
         anyout_c( ANYOUT_TST , tofchar( line ) ) ;
         
         if ( snipdur >= MINSPECLENGTH ) {
            nsamples = snipdur * SAMPPERSAT ;
            sample = irds_rd_detoff_c( tofchar( set->name ) , &snip , &detno , &one ,
                                     &set->cosys , &lon , &lat ,
                                     &sunref , &zero , 
                                     inscan , xscan , twist , &nsamples , &status ) ;
            sprintf( line , "Output of IRDS_RD_DETOFF : closest at sample %d (%10.5f,%10.5f,%10.5f)" ,
                             sample ,
                             inscan[ (int) labs( sample ) ] / RADPERDEG , 
                             xscan[ (int) labs( sample ) ] / RADPERDEG ,
                             twist[ (int) labs( sample ) ] / RADPERDEG ) ;
            anyout_c( ANYOUT_TST , tofchar( line ) ) ;
            if ( status == 0 && sample > 0 ) {
               temp->next   = (NextSpectrumPtr) InitialiseSpectrum( snip ,
                                                  snipdur , scancal + snipcal + 1 ,
                                                  sop , att ) ;
               temp         = (Spectrum *) temp->next ;
               sprintf( line , "Source within aperture at tick %4d" , 
                                    sample / SAMPPERSAT + 1 ) ;
               anyout_c( ANYOUT_TST , tofchar( line ) ) ;
            }
         }
      }
      temp         = spectrumlist ;
      spectrumlist = (Spectrum *) temp->next ;
      temp->next   = NULL ;
      free( temp ) ;
   } else {
      temp = spectrumlist ;
      while ( temp != NULL ){
         temp->process = false ;
         temp->done    = false ;
         temp          = (Spectrum *) temp->next ;
      }
   }

   return( spectrumlist );
}						/* GetSpectrumList */


/*
   NextSpectrum asks the user which spectrum from the spectrum list
he/she wants to process.

*/
   
Spectrum *NextSpectrum( Spectrum *spectrumlist ) 
{
   Spectrum *temp ;

   char     line[ MAXTXTLEN ] , snipmes[ MAXTXTLEN ] ;

   fchar    fbuf ;

   fint     snip[ MAXSNIPS ] , maxsnips = MAXSNIPS ;
   fint     n , nitems = 0 , nsnips = 0 ;

   anyout_c( ANYOUT_TST , tofchar(" - NextSpectrum") ) ;
   finit( fbuf , MAXTXTLEN ) ;
   fbuf.a[ 0 ] = ' ' ;
   fbuf.a[ 1 ] =  0  ;

   temp = spectrumlist ;
   while ( temp ) {
      if ( temp->process && !temp->done ) {
         return( temp ) ;
      }
      temp = (Spectrum *) temp->next ;
   }

   temp   = spectrumlist ;
   nsnips = 0 ;
   while ( temp ) {
      if ( !temp->done ) {
         snip[ nsnips++ ] = temp->snip ;
      }
      temp = (Spectrum *) temp->next ;
   }

   sprintf( snipmes , "Give snip number to process [ all remaining spectra ]" ) ;
   if ( nsnips > 1 ) {
      sprintf( line , "Spectra" ) ;
      for ( n = 0 ; n < nsnips - 1 ; n++ ) {
         sprintf( &line[ nelc_c(tofchar(line)) ] , " %3d," , snip[ n ] ) ;
         if ( nelc_c( tofchar( line ) ) > MAXTXTLEN - 10 ) {
            anyout_c( ANYOUT_DEF , tofchar( line ) ) ;
            line[0] = 0 ;
         }
      }
      sprintf( &line[ nelc_c(tofchar(line)) ] , 
               " and %3d are not yet calibrated." , snip[ nsnips - 1 ] ) ;
   } else if ( nsnips == 1 ) {
      sprintf( line , "Spectrum %3d is not yet calibrated." , snip[ 0 ] ) ;
   } else {
      sprintf( line , "All spectra have been calibrated" ) ;
      sprintf( snipmes , "Give snip number to process [ ask POS= ]" ) ;
      fbuf.a[0] = 'Q' ;
      fbuf.a[1] =  0  ;
   }
   anyout_c( ANYOUT_DEF , tofchar( line ) ) ;

   nitems = usertext_c( fbuf , DFLT_DEF , SNIP_KEY , tofchar( snipmes ) ) ;
   if ( fbuf.a[0] == 'Q' || fbuf.a[0] == 'q' ) {
      cancel_c( SNIP_KEY ) ;
      return( NULL ) ;
   } else if ( fbuf.a[0] == 'A' || fbuf.a[0] == 'a' ) {
      temp   = spectrumlist ;
      nsnips = 0 ;
      while ( temp ) {
         snip[ nsnips++ ] = temp->snip ;
         temp = (Spectrum *) temp->next ;
      }
   } else {
      nitems = userint_c( snip , &maxsnips , DFLT_DEF , SNIP_KEY , tofchar( snipmes ) ) ;
      if( nitems != 0 ) nsnips = nitems ;
   }
   cancel_c( SNIP_KEY ) ;

   temp   = spectrumlist ;
   for ( n = 0 ; n < nsnips ; n++ ) {
      while ( temp ) {
         if ( temp && ( temp->snip == snip[ n ] ) ) {
            temp->process =  true ;
            temp->done    = false ;
            break;
         }
         temp = (Spectrum *) temp->next ;
      }
      temp = spectrumlist ;
   }
   while ( temp && ( temp->done || !temp->process ) ) {
      temp = (Spectrum *) temp->next ;
   }

   return( temp ) ;
}


/*
   GetLRSSet asks the user for an IRDS to process. The routine checks whether
the IRDS exists and whether it is an LRS data set. If so control returns to 
the caller, if not the user is prompted for a new IRSD name. 
If <CR> is given to the keyword the routine returns NULL indicating that
the user wants to terminate the program.
*/

LRSSet *GetLRSSet( )
{
   LRSSet    *set = NULL ;

   char      line[MAXTXTLEN]   ;

   fint      ierr   = 0 ;
   fint      nitems = 0 ;
   fint      status = 0 ;
   fint      refsys ;
   fint      naxes ;
   fint      axes[ 4 ] ;

   fchar     setname ;
   fchar     instrument ;
   fchar     coosys ;
   fchar     object ;

   float     twothou = 2000.0 ;

   int       found  = 0 ;
   int       done   = 0 ;

   anyout_c( ANYOUT_TST , tofchar(" - GetLRSSet") ) ;
   finit( setname    , MAXTXTLEN ) ;		/* Initialise fchars */
   finit( instrument , MAXTXTLEN ) ;
   finit( coosys     , MAXTXTLEN ) ;
   finit( object     , MAXTXTLEN ) ;
   
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
            set   = InitialiseLRSSet( setname ) ;	/* Initialise the set */
            anyout_c( ANYOUT_TST , tofchar( "After IniLRSSet" ) ) ;
            irds_enquire_c( setname  , object  , instrument , 
                            &naxes   , axes , set->center  , set->size  ,
                            coosys   , &set->epoch         , &status ) ;
            anyout_c( ANYOUT_TST , tofchar( "After irds_enquire" ) ) ;
            if ( !strstr( instrument.a , "LRS" ) ){	/* not LRS data set */
               (void) sprintf( line , "IRSET %.*s does not contain LRS data" , 
                                       (int) nelc_c( setname ) , setname.a );
               error_c( WARNING , tofchar( line ) );	/* tell user */
               cancel_c( INSET_KEY );			/* give another chance */
               found = false ;
               free( set ) ;
               set   = NULL ;
            }
         }
      }
   }
   anyout_c( ANYOUT_TST , tofchar( "GetLRSSet got a set" ) ) ;
   if ( found && !done ){
      set->nsnips = axes[ 3 ] ;
      sprintf( set->coname , "%.*s" , nelc_c( coosys ) , coosys.a ) ;
      sprintf( set->object , "%.*s" , nelc_c( object ) , object.a ) ;
      set->lonlat[ 0 ] = set->center[ 0 ] ;
      set->lonlat[ 1 ] = set->center[ 1 ] ;
      refsys      = irco_number_c(        coosys         , &twothou ) ;
      if ( set->epoch == twothou ) {
         set->cosys  = refsys ;
      } else {
         set->cosys  = 0 ;
         irco_precess_c( &refsys , &set->epoch , &set->cosys ) ;
      }
   } else {					/* user wants to stop */
      if ( !set ){
         free( set ) ;				/* free space */
         set = NULL ;				/* set pointer to NULL */
      }
   }

   return( set );
}						/* GetLRSSet */


/*
   MakeLRSOutset creates an output LRS set. It uses information from
the inputset inset to create the output set (outset) header. The parameters
of outset are stored in an LRSSet struct.
*/

LRSSet *MakeLRSOutset( LRSSet *inset )
{
   LRSSet *set = NULL ;

   char      line[MAXTXTLEN]  ;

   fint      ierr   = 0 ;
   fint      nitems = 0 ;

   fchar     setname ;

   int       outset = 0 ;
   int       done   = 0 ;

   float     lmlo   = BEGINWAVE ;
   float     lmhi   = BEGINWAVE + ( NWAVE - 1 ) * STEPWAVE ;
   float     lmstep = STEPWAVE ;

   anyout_c( ANYOUT_TST , tofchar(" - MakeLRSOutset") ) ;
   finit( setname , MAXTXTLEN ) ;		/* Initialise setname */
   while ( !done && !outset ) {
      nitems = usertext_c( setname, DFLT_DEF, OUTSET_KEY, OUTSET_MES );/*get set*/
      done = ( nitems == 0 ) ; 			/* did user type CR? */
      if( !done ) {
         if( irlrs_set_exist_c( setname ) != 0 ) {/* OUTSET does exist */
            (void) sprintf( line , "IRSETOUT %.*s already exists " , 
                                 (int) nelc_c( setname ) , setname.a );
            error_c( WARNING , tofchar( line ) );	/* tell user */
            outset = false ;
            cancel_c( OUTSET_KEY );
         } else {  				/* OUTSET does not exist */
            outset = true ;
            irlrs_set_create_c( tofchar( inset->name ) , 
                                &lmlo , &lmhi , &lmstep , setname , &ierr ) ;
            if ( ierr < 0 ) {
               sprintf( line , "IRLRS_SET_CREATE error nr %d" , ierr );
               error_c( FATAL_ERROR , tofchar( line ) ) ;
            }
         }
      }
   }			/* cancel the keyword */
   if ( outset && !done ){
      set = InitialiseLRSSet( setname ) ;	/* Initialise the set */
   } else {					/* user wants no outset */
      if ( !set ){
         free( set ) ;				/* free space */
         set = NULL ;				/* set pointer to NULL */
      }
   }
   return( set );
}						/* MakeLRSOutset */


/* 
   GetSpectrumPosition will ask the user for a position for which spectra
should be extracted and calibrated. The routine will chek to see whether the
desired position lies within the boundaries of the IRDS. If not the user gets
another chance. When <CR> is entered at the prompt the program will stop.
*/

int GetSpectrumPosition( LRSSet *set )

{
   char      line[MAXTXTLEN]  ;

   fchar     fbuf ;

   fint      nitems ;

   int       done;

   double    pos[ 2 ] ;

   anyout_c( ANYOUT_TST , tofchar(" - GetSpectrumPosition") ) ;
   finit( fbuf       , MAXTXTLEN ) ;           	/* Initialise fchars */

   pos[ 0 ] = set->lonlat[ 0 ] ;
   pos[ 1 ] = set->lonlat[ 1 ] ;
   do {
      sprintf( line , "Give Q(uit) or position (%.*s %6.1f) [%6.2f,%6.2f]" ,
              nelc_c( tofchar( set->coname ) ) , set->coname , set->epoch , 
              pos[ 0 ] , pos[ 1 ] ) ;
      nitems = usertext_c( fbuf, DFLT_DEF, POS_KEY, tofchar( line ) ) ;
      done = ( fbuf.a[0] == 'Q' || 
               fbuf.a[0] == 'q' ) ;		/* did user type Quit? */
      if ( !done ) {
         nitems = 2 ;				/* get 2 numbers */
         nitems = userangle_c( pos , &nitems , DFLT_DEF , 
                                       POS_KEY , tofchar( line ) ) ; 
         if ( nitems == 0 ) nitems = 2 ;	/* CR -> old position */ 
         if ( nitems != 2 ) {			/* NOT two numbers */
            error_c( WARNING , 			/* give a warning */
                    tofchar("You must give TWO numbers or <CR> for a position"));
         } else if ( ( pos[0] < set->center[0] - set->size[0] ) || 
                     ( pos[0] > set->center[0] + set->size[0] ) || 
                     ( pos[1] < set->center[1] - set->size[1] ) || 
                     ( pos[1] > set->center[1] + set->size[1] ) ) { 
            sprintf( line , "(LON,LAT)=(%7.3f, %7.3f ) outside set, try again!", 
                            pos[0] , pos[1] );
            error_c( WARNING , tofchar( line ) );
            nitems = 0 ;
         } else {				/* good input */
            set->lonlat[ 0 ] = pos[ 0 ] ;
            set->lonlat[ 1 ] = pos[ 1 ] ;
            sprintf( line , "Desired position (LON,LAT)= (%7.3f, %7.3f )", 
                            set->lonlat[0] , set->lonlat[1] );
            anyout_c( ANYOUT_TST , tofchar( line ) );
         }
      }
      cancel_c( POS_KEY );			/* cancel the keyword */
   } while( !done && nitems!=2 );
   return( done );
}						/* GetSpectrumPosition */


/*
   ReadSpectrum will read the raw LRS data from the input IRDS into
the LRSData struct raw. These data can then be processed by CalibrateSpectrum.
*/

LRSData *ReadSpectrum( LRSSet *set , Spectrum *spectrum )
{
   char    line[MAXTXTLEN] ;

   fint     sdet , nitems , zero = 0 , one = 1 ;
   fint     error = 0 , status , sunref ;
   fint     sample , samplestart = 0 , readstart , nsamples ;

   float    islim , databuffer[ SPECSAMP ];

   double   inscan[ NRSAMP ] , xscan[ NRSAMP ] , twist[ NRSAMP ] ;
   double   lon , lat ;

   LRSData  *raw = NULL ;

   anyout_c( ANYOUT_TST , tofchar(" - ReadSpectrum") ) ;
   if ( spectrum->process && !spectrum->done ){	/* do this one ? */

      sprintf(line,"Spectrum of snip %d (sop,att = %d,%d) from set %s" , 
                 spectrum->snip, spectrum->sop, spectrum->att, set->name );
      anyout_c( ANYOUT_TST , tofchar( line ) );
      sprintf(line,"Reading snip %d from set %s",  spectrum->snip, set->name );
      status_c( tofchar( line ) );

      raw = InitialiseLRSData( spectrum->snip ) ;

      sunref = 5 ;

      lon = set->lonlat[ 0 ] * RADPERDEG ;
      lat = set->lonlat[ 1 ] * RADPERDEG ;

      sdet = 0 ;
      nsamples = spectrum->snipdur * SAMPPERSAT ;
      sample   = irds_rd_detoff_c( tofchar( set->name ) , &spectrum->snip ,
                    &sdet , &one , &set->cosys ,  &lon , &lat , &sunref ,
                    &zero , inscan , xscan , twist , &nsamples , &status ) ;
      raw->satcal = (float) spectrum->satcal + fabs( sample ) / SAMPPERSAT ;
      if ( status == 0 ) {
         islim  = ISCANLIM ;
         nitems = userreal_c( &islim , &one , DFLT_HIDD , 
                                        ISLIM_KEY , ISLIM_MES ) ;
         islim  = islim / 3600 * RADPERDEG ;
         samplestart = ( sample > 0 ) ? sample : -sample ;
         while( ( samplestart > 0 ) && 
                ( fabs( inscan[ samplestart ] ) < fabs( islim ) ) ) {
               samplestart = samplestart - 1 ; 
         }
      }
      raw->snip   = spectrum->snip ;
      readstart   = samplestart / SAMPPERSAT ;
      samplestart =   readstart * SAMPPERSAT ;

      if ( ( spectrum->snipdur - ( readstart + 1 ) ) > SPECLENGTH ) {
         raw->det[ 1 ].ns  = SPECSAMP ;
      } else {
         raw->det[ 1 ].ns  = ( spectrum->snipdur - ( readstart + 1 ) ) * SAMPPERSAT ;
      }

      readstart   = readstart + 1 ;
      for ( sdet = 1 ; ( sdet < NRDETS + 1 ) ; sdet++ ) {

         raw->det[ sdet ].ns  = raw->det[ 1 ].ns ;

         sample = irds_rd_detoff_c(  tofchar( set->name ) , 
                                    &spectrum->snip ,
                                    &sdet , 
                                    &one , 
                                    &set->cosys ,  
                                    &lon , 
                                    &lat , 
                                    &sunref ,
                                    &zero , 
                                     inscan , 
                                     xscan , 
                                     twist , 
                                    &nsamples , 
                                    &status ) ;

         raw->det[ sdet ].onsource = ( sample > 0 ) ;

         if ( raw->det[ sdet ].onsource ) {
            sprintf( line , "Source in sdet %d, extract %d ticks from tick %d onwards" , 
                       sdet , raw->det[ sdet ].ns / SAMPPERSAT , readstart ) ;
         } else { 
            sprintf( line , "Source not in sdet %d, extract %d ticks from tick %d onwards" , 
                       sdet , raw->det[ sdet ].ns / SAMPPERSAT , readstart ) ;
         }

         anyout_c( ANYOUT_TST , tofchar( line ) );
         irds_rd_samples_c(  tofchar( set->name ) ,
                            &raw->snip            ,
                            &sdet                 ,
                            &readstart            ,
                             databuffer           ,
                            &raw->det[ sdet ].ns  , 
                            &error                 ) ;

         for ( sample = 0 ; sample < raw->det[ sdet ].ns ; sample++ ) {
            if ( databuffer[ sample ] == 0.0 ) 
               setfblank_c( &databuffer[ sample ] ) ;
         }
         irlrs_adroop_c( &raw->det[ sdet ].detno ,
                          databuffer             ,
                         &raw->det[ sdet ].ns    ) ;

         if ( inscan[ 1 ] > inscan[ 0 ] ) {
            anyout_c( ANYOUT_TST , 
                tofchar( "Will invert data-order to get increasing lambda" ) ) ;
            for ( sample = 0 ; sample < raw->det[ sdet ].ns ; sample++ ) {
               raw->det[ sdet ].is[ sample ] = 
                   inscan[ raw->det[ sdet ].ns - 1 - sample + samplestart ] / RADPERDEG ;
               raw->det[ sdet ].xs[ sample ] =
                   xscan[ raw->det[ sdet ].ns - 1 - sample + samplestart ] / RADPERDEG ;
               raw->det[ sdet ].fl[ sample ] =
                    databuffer[ raw->det[ sdet ].ns - 1 - sample ] ;
            }
         } else {
            anyout_c( ANYOUT_TST , 
                tofchar( "Will NOT invert data-order to get increasing lambda" ) ) ;
            for ( sample = 0 ; sample < raw->det[ sdet ].ns ; sample++ ) {
               raw->det[ sdet ].is[ sample ] = 
                   inscan[ sample + samplestart - 1 ] / RADPERDEG ;
               raw->det[ sdet ].xs[ sample ] =
                   xscan[ sample + samplestart - 1 ] / RADPERDEG ;
               raw->det[ sdet ].fl[ sample ] =
                    databuffer[ sample ] ;
            }
         }
      }
   }
   return( raw ) ;
}						/* ReadSpectrum */


/*
   CalibrateSpectrum does the actual calibration of individual spectra. 

*/

void CalibrateSpectrum( Spectrum *spectrum , LRSData *raw )
{
   char    line[MAXTXTLEN] ;

   bool	   cohen = true ;

   fint    one = 1 ;
   fint    n = 0 , sdet ;

   float   factor = 0.0 ;
   float   max_gain = 2 ;
   float   blank ;

   anyout_c( ANYOUT_TST , tofchar(" - CalibrateSpectrum") ) ;

   n = userlog_c( &cohen , &one , DFLT_HIDD , COHEN_KEY , COHEN_MES ) ;
   n = userreal_c( &max_gain , &one , DFLT_HIDD , MXGAIN_KEY , MXGAIN_MES ) ;

   setfblank_c( &blank ) ;
   if ( spectrum->process && !spectrum->done ){	/* do this one ? */
      sprintf( line , "Calibrating spectrum number %d" , spectrum->snip );
      status_c( tofchar( line ) );

      for ( sdet = 1 ; sdet < NRDETS + 1 ; sdet ++ ) {
         BaseSub( raw->det[ sdet ].is ,  raw->det[ sdet ].fl ,
                 &raw->det[ sdet ].ns , &spectrum->snip ) ;
         (void) irlrs_pos2wave_c( &raw->det[ sdet ].detno , 
                                   raw->det[ sdet ].is    , 
                                  &raw->det[ sdet ].ns    , 
                                   raw->det[ sdet ].lm    ) ;
         factor = irlrs_pwm2permv_c( &raw->det[ sdet ].detno ) ;
         for( n = 0 ; n < raw->det[ sdet ].ns ; n++ ) {
            if ( ( raw->det[ sdet ].lm[ n ] != 0 ) &&
                 ( raw->det[ sdet ].fl[ n ] != blank ) ) {
               raw->det[ sdet ].fl[ n ] = raw->det[ sdet ].fl[ n ] 
                                          * raw->det[ sdet ].lm[ n ] * factor ;
            } else {
               setfblank_c( &raw->det[ sdet ].fl[ n ] ) ;
            }
         }
         (void) irlrs_lgain_c( &raw->det[ sdet ].detno , 
                               raw->det[ sdet ].lm     , &raw->det[ sdet ].ns , 
                               raw->det[ sdet ].fl     ) ;
         (void) irlrs_xgain_c( &raw->det[ sdet ].detno , &max_gain ,
                               raw->det[ sdet ].xs     , &raw->det[ sdet ].ns ,
                               raw->det[ sdet ].fl     ) ;
         if( cohen ) {   
            (void) irlrs_cohen_gain_c(
                               raw->det[ sdet ].lm     , &raw->det[ sdet ].ns , 
                               raw->det[ sdet ].fl     ) ;
         }
         if ( raw->units[0] == 'J' || raw->units[0] == 'j' ) {
            irlrs_jyperpwm2_c( raw->det[ sdet ].lm , 
                              raw->det[ sdet ].fl , &raw->det[ sdet ].ns , 
                              raw->det[ sdet ].fl ) ;
         } else if ( raw->units[7] == '-'  ) {	/* F(lambda) wanted */
            for ( n = 0 ; n < raw->det[ sdet ].ns ; n++ ) {
               if( raw->det[ sdet ].fl[ n ] != blank ) 
                  raw->det[ sdet ].fl[ n ] = raw->det[ sdet ].fl[ n ] 
                                             / raw->det[ sdet ].lm[ n ] ;
            }
         } else if ( raw->units[7] == '3'  ) {	/* lambda^4F(lambda) wanted */
            for ( n = 0 ; n < raw->det[ sdet ].ns ; n++ ) {
               if( raw->det[ sdet ].fl[ n ] != blank ) 
                  raw->det[ sdet ].fl[ n ] = raw->det[ sdet ].fl[ n ] 
                                             * raw->det[ sdet ].lm[ n ] 
                                             * raw->det[ sdet ].lm[ n ] 
                                             * raw->det[ sdet ].lm[ n ] ;
            }
         }
      }
      spectrum->done = true ;			/* this one's done */
   }
}						/* CalibrateSpectrum */


/*
   SampleSpectrum will resample the spectra to a regular wavelength grid.
*/

void SampleSpectrum( LRSData *raw )
{
   char    line[ MAXTXTLEN ] ;

   fint    sdet , nwaves , n , error , nblank , count ;
   fint    imin = 0 , imax = 0 ;

   float   flux[ NWAVE ] , blank ;

   anyout_c( ANYOUT_TST , tofchar(" - SampleSpectrum") ) ;

   setfblank_c( &blank ) ;
   for ( sdet = 1 ; sdet < NRDETS + 1 ; sdet++ ) {
      count  = 0 ;
      nblank = 0 ;
      imin   = 0 ;
      imax   = 0 ;
      minmax4_c(  raw->det[ sdet ].fl  , 
                 &raw->det[ sdet ].ns  ,
                 &raw->det[ sdet ].min , 
                 &raw->det[ sdet ].max ,
                 &imin                 , 
                 &imax                 , 
                 &nblank               , 
                 &count                 ) ;
      sprintf( line , "Detector %2d , gave %d points, %4d blanks of %4d points" ,
                 raw->det[ sdet ].detno , raw->det[ sdet ].ns , nblank , count ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;
      sprintf( line , "Found min/max (%f,%f) at %3d/%3d" , 
                 raw->det[ sdet ].min , raw->det[ sdet ].max , imin , imax ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;
      nwaves = NWAVE ;
      for( n = 0 ; 
           ( n < nwaves ) && ( raw->det[ sdet ].lm[ 0 ] > raw->wave[ n ] ) ;
           n++ )
         setfblank_c( &flux[ n ] ) ;
      nwaves = nwaves - n ;

      error = spline1_c(  raw->det[ sdet ].lm ,
                          raw->det[ sdet ].fl , 
                         &raw->det[ sdet ].ns ,
                         &raw->wave[ n ]      , 
                         &flux[ n ]           ,
                         &nwaves               ) ;
      sprintf( line , "SPLINE1 got %4d points, call returned %d" ,
                          raw->det[ sdet ].ns , error ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;
      if ( ( error < 0 ) && ( error != -3 ) ) {
         sprintf( line , "Error %2d in SPLINE1" , error ) ;
         error_c( WARNING , tofchar( line ) ) ;
      }
      for ( n = 0 ; ( n < NWAVE )    && 
                    ( n < SPECSAMP ) &&
                    ( raw->wave[ n ] <
                      raw->det[ sdet ].lm[ raw->det[ sdet ].ns - 1 ] ) 
                                                                  ; n++ ) {
         raw->det[ sdet ].lm[ n ] = raw->wave[ n ] ;
         raw->det[ sdet ].fl[ n ] = flux[ n ] ;
      }
      raw->det[ sdet ].ns = n - 1 ;
      count  = 0 ;
      nblank = 0 ;
      imin   = 0 ;
      imax   = 0 ;
      minmax4_c(  raw->det[ sdet ].fl  , 
                 &raw->det[ sdet ].ns  ,
                 &raw->det[ sdet ].min , 
                 &raw->det[ sdet ].max ,
                 &imin                 , 
                 &imax                 , 
                 &nblank               , 
                 &count                 ) ;
      sprintf( line , "Detector %2d , gave %d points, %4d blanks of %4d points" ,
                 raw->det[ sdet ].detno , raw->det[ sdet ].ns , nblank , count ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;
      sprintf( line , "Found min/max (%f,%f) at %3d/%3d" , 
                 raw->det[ sdet ].min , raw->det[ sdet ].max , imin , imax ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;
   }
}						/* SampleSpectrum */

/* 
   AlignSpectrum aligns the two halves of the spectrum
*/
void AlignSpectrum( LRSData *raw )
{
   bool   align = true ;

   char   line[ MAXTXTLEN ] ;
 
   fint   one = 1 ;
   fint   sdet , n ;
   fint   swcount = 0 , lwcount = 0 ;
   fint   swdet = 0 , lwdet = 0 ;

   float  blank ;
   float  swsum = 0 , lwsum = 0 ;
   float  swgain , lwgain ;

   anyout_c( ANYOUT_TST , tofchar(" - AlignSpectrum") ) ;

   n = userlog_c( &align , &one , DFLT_HIDD , ALIGN_KEY , ALIGN_MES ) ;

   if ( !align ) return ;

   setfblank_c( &blank ) ;
   for ( sdet = 1 ; sdet < NRDETS + 1 ; sdet++ ) {
      if ( raw->det[ sdet ].onsource ) {
         if ( irlrs_dettype_c( &raw->det[ sdet ].detno ) == 1 ) {
            swcount = 0 ;
            swsum   = 0.0 ;
            swdet   = sdet ;
            for ( n = 0 ; n < raw->det[ swdet ].ns ; n++ ) {
               if ( ( raw->det[ swdet ].fl[ n ] != blank    ) &&
                    ( raw->det[ swdet ].lm[ n ] >  LWLOEDGE ) &&
                    ( raw->det[ swdet ].lm[ n ] <  SWHIEDGE )    ) {
                  swsum = swsum + raw->det[ swdet ].fl[ n ] ;
                  swcount += 1 ;
               }
            }
         } else {
            lwcount = 0 ;
            lwsum   = 0.0 ;
            lwdet   = sdet ;
            for ( n = 0 ; n < raw->det[ lwdet ].ns ; n++ ) {
               if ( ( raw->det[ lwdet ].fl[ n ] != blank    ) &&
                    ( raw->det[ lwdet ].lm[ n ] >  LWLOEDGE ) &&
                    ( raw->det[ lwdet ].lm[ n ] <  SWHIEDGE )    ) {
                  lwsum = lwsum + raw->det[ lwdet ].fl[ n ] ;
                  lwcount += 1 ;
               }
            }
         }
      }
   }

   if ( ( swcount > 0 ) && ( lwcount > 0 ) && 
        ( swsum   > 0 ) && ( lwsum   > 0 )   ) {
      swgain = 0.5 * ( 1 + lwsum/lwcount / ( swsum/swcount ) ) ;
      lwgain = 0.5 * ( 1 + swsum/swcount / ( lwsum/lwcount ) ) ;
      for ( n = 0 ; n < raw->det[ swdet ].ns ; n++ ) {
         if ( raw->det[ swdet ].fl[ n ] != blank )
            raw->det[ swdet ].fl[ n ] = swgain * raw->det[ swdet ].fl[ n ] ;
      }
      raw->det[ swdet ].max = swgain * raw->det[ swdet ].max ;
      raw->det[ swdet ].min = swgain * raw->det[ swdet ].min ;
      for ( n = 0 ; n < raw->det[ lwdet ].ns ; n++ ) {
         if ( raw->det[ lwdet ].fl[ n ] != blank )
            raw->det[ lwdet ].fl[ n ] = lwgain * raw->det[ lwdet ].fl[ n ] ;
      }
      raw->det[ lwdet ].max = lwgain * raw->det[ lwdet ].max ;
      raw->det[ lwdet ].min = lwgain * raw->det[ lwdet ].min ;
   } else {
      sprintf( line , "Snip %d cannot be aligned, data not corrected" , 
               raw->snip ) ;
      error_c( WARNING , tofchar( line ) ) ;
   }
}

/*
   SumSpectrum will add the short and long wavelengh parts of the spectrum.
*/

void SumSpectrum( LRSData *raw , LRSData *average )
{
   fint    sdet , n , flcount[ NWAVE ];
   fint    count , nblank , imin = 0 , imax = 0 ;

   float   blank ;

   anyout_c( ANYOUT_TST , tofchar(" - SumSpectrum") ) ;

   for ( n = 0 ; n < NWAVE ; n++ ) {
      raw->det[ 0 ].fl[ n ] = 0.0 ;
      raw->det[ 0 ].lm[ n ] = raw->wave[ n ] ;
      flcount[ n ]          = 0   ;
   }
   raw->det[ 0 ].ns = NWAVE ;

   setfblank_c( &blank ) ;
   for ( sdet = 1 ; sdet < NRDETS + 1 ; sdet++ ) {

      if ( raw->det[ sdet ].onsource ) {
         if ( irlrs_dettype_c( &raw->det[ sdet ].detno ) == 1 ) {
            for ( n = 0 ; n < raw->det[ sdet ].ns ; n++ ) {
               if ( ( raw->det[ sdet ].fl[ n ] != blank    ) &&
                    ( raw->det[ sdet ].lm[ n ] <  SWHIEDGE )    ) {
                  raw->det[ 0 ].fl[ n ] = raw->det[ 0 ].fl[ n ] + 
                                          raw->det[ sdet ].fl[ n ] ;
                  flcount[ n ] += 1 ;
               }
            }
         } else {
            for ( n = 0 ; n < raw->det[ sdet ].ns ; n++ ) {
               if ( ( raw->det[ sdet ].fl[ n ] != blank    ) &&
                    ( raw->det[ sdet ].lm[ n ] >=  SWHIEDGE )    ) {
                  raw->det[ 0 ].fl[ n ] = raw->det[ 0 ].fl[ n ] + 
                                          raw->det[ sdet ].fl[ n ] ;
                  flcount[ n ] += 1 ;
               }
            }
         }

      }

   }

   sprintf( average->units , "%s" , raw->units ) ;
   raw->det[ 0 ].onsource = true ;
   for ( n = 0 ; n < NWAVE ; n++ ) {
      if ( flcount[ n ] == 0 ) {
         setfblank_c( &raw->det[ 0 ].fl[ n ] ) ;
      } else {
         raw->det[ 0 ].fl[ n ] = raw->det[ 0 ].fl[ n ] / flcount[ n ] ;
      }
   }
   
   for ( sdet = 0 ; sdet < NRDETS + 1 ; sdet++ ) {
      if ( raw->det[ sdet ].onsource ) {
         average->det[ sdet ].onsource = true ;
         for ( n = 0 ; n < NWAVE ; n++ ) {
            if ( raw->det[ sdet ].fl[ n ] != blank ) {
               average->det[ sdet ].fl[ n ] =  ( average->det[ sdet ].fl[ n ] *
                                                    average->det[ sdet].nspec +
                                                 raw->det[ sdet ].fl[ n ] ) /
                                              ( average->det[ sdet ].nspec + 1 ) ;
            }
         }
         average->det[ sdet ].nspec = average->det[ sdet ].nspec + 1 ;
         average->det[ sdet ].ns = NWAVE ;
         count  = 0 ;
         nblank = 0 ;
         minmax4_c(  average->det[ sdet ].fl  , 
                    &average->det[ sdet ].ns  ,
                    &average->det[ sdet ].min , 
                    &average->det[ sdet ].max ,
                    &imin                 , 
                    &imax                 , 
                    &nblank               , 
                    &count                 ) ;
      }
   }
}						/* SumSpectrum */


/*
   ReportSpectrum reports on the calibrated spectrum
*/
void ReportSpectrum( Spectrum *spectrum ,
                     LRSData  *raw      )
{
   char		line[ MAXTXTLEN ] ;

   fint		n ;
   fint		sdet , lodet = 0 , hidet = 0 ;
   fint		imin = 0 , imax = 0 , nblank = 0 , count = 0 ;

   float	rms = 0 , sum , sumsq , blank ; 
   float	ratio12mu = 0.0 , ratio25mu = 0.0 ;

   anyout_c( ANYOUT_TST , tofchar(" - ReportSpectrum") ) ;

   setfblank_c( &blank ) ;

   for ( sdet = 1 ; sdet <= NRDETS ; sdet++ ) {
      if ( raw->det[ sdet ].onsource ) {
         if ( sdet < 4 ) {
            hidet = sdet ;
         } else {
            lodet = sdet ;
         }
      }
   }

   minmax4_c(  raw->det[ 0 ].fl  , 
              &raw->det[ 0 ].ns  ,
              &raw->det[ 0 ].min , 
              &raw->det[ 0 ].max ,
              &imin              , 
              &imax              , 
              &nblank            , 
              &count              ) ;

   rms   = 0.0 ;
   sum   = 0.0 ;
   sumsq = 0.0 ;
   count = 0   ;
   for ( n = 0 ; n < raw->det[ 0 ].ns ; n++ ) {
      if ( ( ( raw->det[ 0 ].lm[ n ] < SWLOEDGE ) ||
             ( raw->det[ 0 ].lm[ n ] > LWHIEDGE )   ) &&
                   ( raw->det[ 0 ].fl[ n ] != blank ) ) {
         sum   = sum   + raw->det[ 0 ].fl[ n ] ;
         sumsq = sumsq + raw->det[ 0 ].fl[ n ] * raw->det[ 0 ].fl[ n ] ;
         count += 1 ;
      }
   }
   if ( count > 0 ) 
      rms = sqrt( ( sumsq - sum * sum / count ) / count ) ;

   if ( !first_spectrum_printed ) {
      sprintf( line , " snip | sop/att | LW/SW |      spectrum maximum      |   r.m.s. noise | 12 mu | 25 mu |" ) ;
      anyout_c( ANYOUT_DEF , tofchar( line ) );
      sprintf( line , "------|---------|-------|----------------------------|----------------|-------|-------|" ) ;
      anyout_c( ANYOUT_DEF , tofchar( line ) );
      first_spectrum_printed = true ;
   }

   sprintf( line , " %4d | %3d/%-3d | %2d/%2d | %9.2g %5.5s at %4.1f mu | %8.1g %5.5s | %5.2f | %5.2f |" ,
                     spectrum->snip , spectrum->sop , spectrum->att ,
                     raw->det[ lodet ].detno , raw->det[ hidet ].detno ,
                     (double) raw->det[ 0 ].max , raw->units , 
                     (double) raw->det[ 0 ].lm[ imax ] ,
                     (double) rms , raw->units ,
                     (double) ratio12mu , (double) ratio25mu ) ;
   anyout_c( ANYOUT_DEF , tofchar( line ) ) ;

   if ( LastSpectrum( spectrum ) ) {	/* last spectrum ? */
      sprintf( line , "------|---------|-------|----------------------------|----------------|-------|-------|" ) ;
      anyout_c( ANYOUT_DEF , tofchar( line ) );
      first_spectrum_printed = false ;
   }

   return ;

}

/*
   PlotLine is a help function to plot a series of points while
   skipping blanks.
*/
void PlotLine( fint *npts, float *x , float *y )
{
   fint   m , n ;

   float  blank ;
   float  *xi , *yi ;

   setfblank_c( &blank ) ;

   xi = (float *) malloc( ( *npts ) * sizeof( float ) ) ;
   yi = (float *) malloc( ( *npts ) * sizeof( float ) ) ;

   if ( !xi || !yi )
      error_c( FATAL_ERROR , tofchar( "Cannot get memory for plot arrays" ) ) ;

   m = 0 ;
   for( n = 0 ; n < *npts ; n++ ) {
      if( y[ n ] != blank ){
         xi[ m ] = x[ n ] ;
         yi[ m ] = y[ n ] ;
         m += 1 ;
      }
   }

   if( m > 1 )
      pgline_c( &m , xi , yi ) ;

   free( xi ) ;
   free( yi ) ;

   return ;
}						/* PlotLine	*/

/*
   PlotSpectrum plots the data in LRSData
*/

void PlotSpectrum( LRSSet  *inset , Spectrum *spectrum , 
                   LRSData *raw   , LRSData  *average   )
{
   fint   n , zero = 0 , one = 1 , two = 2 , sign = 1 ;
   fint   error = 0 , dd = 0 , mm = 0 , nitems = 0 ;
   fint   thinlines = 1 , thicklines = 5 ;
   fint   full_line = 1 , dotted_line = 4 ;
   fint   first , last , sdet , swtolw ;

   fchar  pgreply ;

   char   label[ MAXTXTLEN ] ;

   bool   thick = false ;

   float  xmin , xmax , xsize ;
   float  ymin , ymax , ysize ;
   float  ds = 0 ;
   float  minmax[ 2 ] , psc[ 2 ] , edgepos[ 2 ] , edgewav[ 2 ] ;
   float  fzero = 0.0 , x ,y , just , charsize , blank ;
   float  wxmin , wxmax , wymin , wymax ;

   finit( pgreply , MAXTXTLEN ) ;
   setfblank_c( &blank ) ;

   if ( raw->snip != 0 ) {
      sprintf( label , "Plotting snip nr. %d" , spectrum->snip ) ;
      status_c( tofchar( label ) ) ;
   } else {
      status_c( tofchar( "Plotting average spectrum" ) ) ;
   }

   xmin  = 0 ;
   xmax  = 27.5 ;
   xsize = xmax - xmin ;

   ymax = raw->det[ 0 ].max ;
   ymin = raw->det[ 0 ].min ;

   ysize = ymax - ymin ;
   ymin  = ymin - 0.15 * ysize ;
   ymax  = ymax + 0.35 * ysize ;

   n = userreal_c( minmax , &two , DFLT_HIDD , MINMAX_KEY , MINMAX_MES ) ;
   if ( n == 2 ) {
      ymin = minmax[ 0 ] ;
      ymax = minmax[ 1 ] ;
   }
   ysize = ymax - ymin ;

						/* open plot device */
   error = pgbeg_c( &zero , tofchar( "?" ) , &one , &one ) ;
   if ( error != 1 ) {
      error_c( FATAL_ERROR , tofchar( "pgbeg error ... " ) ) ;
   }
   pgqinf_c( tofchar( "TYPE" ) , pgreply , &n ) ;
   if ( !strncmp( pgreply.a , "TEK4010" , 6 ) ) {
      thick = true ;
      nitems = userlog_c( &thick , &one , DFLT_HIDD , THICK_KEY , THICK_MES ) ;
      if ( nitems !=0 && !thick ) {
         thicklines = thinlines ;
      }
   }

   charsize = 1.5 ;
   pgsch_c( &charsize ) ;			/* set charactersize */
   pgslw_c( &thicklines ) ;			/* set linewidth */
   wxmin = 0.20 ;				/* set viewport/window */
   wxmax = 0.95 ;
   wymin = 0.15 ;
   wymax = 0.90 ;
   pgsvp_c(  &wxmin , &wxmax , &wymin , &wymax ) ;
   pgswin_c( &xmin  , &xmax  , &ymin  , &ymax  ) ;
						/* plot frame */
   pgbox_c( tofchar( "BCNTS" )  , &fzero , &zero ,
            tofchar( "BCNTSV" ) , &fzero , &zero ) ;

						/* plot labels */
   pglab_c( tofchar( "Wavelength (\\gm)" ), tofchar( "" ), tofchar( "" ) ) ;
   x    =  0.5  ;				/* add Y-label */
   y    =  5.0  ;
   just =  0.5  ;
   if ( raw->units[0] == 'J' ) {
      pgmtxt_c( tofchar( "L" ) , &y , &x , &just ,
                 tofchar( "Intensity (Jy)" ) ) ;
   } else if ( raw->units[7] == '-' ) {
      pgmtxt_c( tofchar( "L" ) , &y , &x , &just ,
                 tofchar( "F\\d\\gl\\u (10 \\u-12\\d Wm\\u-2\\d\\gm\\u-1\\d)" ) ) ;
   } else if ( raw->units[7] == '3' ) {
      pgmtxt_c( tofchar( "L" ) , &y , &x , &just ,
                 tofchar( "\\gl\\u4\\d F\\d\\gl\\u (10 \\u-12\\d Wm\\u-2\\d\\gm\\u3\\d)" ) ) ;
   } else { 
      pgmtxt_c( tofchar( "L" ) , &y , &x , &just ,
                 tofchar( "\\gl F\\d\\gl\\u (10 \\u-12\\d Wm\\u-2\\d)" ) ) ;
   }


						/* annotate the plot */
   x    =  0.0  ;				/* program ID */
   y    =  0.5  ;
   just =  0.0  ;
   sprintf( label , "LRSCAL v. %s" , VERSION ) ;
   pgmtxt_c( tofchar( "T" ) , &y , &x , &just , tofchar( label ) ) ;

   charsize = 1.2 ;
   pgsch_c( &charsize ) ;			/* set charactersize */

   x    =  1.0  ;				/* set specification */
   y    =  0.5  ;
   just =  1.0  ;
   if ( raw->snip == 0 ) {
      sprintf( label , "set %s, average" , inset->name ) ;
   } else {
      sprintf( label , "set %s, snip nr.%d" , inset->name , raw->snip ) ;
   }
   pgmtxt_c( tofchar( "T" ) , &y , &x , &just , tofchar( label ) ) ;

   just =  1.0 ;
   x = xmin + 0.95 * xsize ;			/* source name */
   y = ymin + 0.82 * ysize ;
   pgptxt_c( &x , &y , &fzero , &just , tofchar( inset->object ) ) ;

   if ( !strncmp( inset->coname , "EQU" , 3 ) ) {
      dd = (int) ( inset->lonlat[0] / 15 ) ;
      mm = (int) ( (float) 60 * ( ( inset->lonlat[0] / 15 ) - dd ) ) ; 
      ds = ( (float) 3600 * ( ( inset->lonlat[0] / 15 ) - dd ) - 60 * mm ) ;
      sprintf( label , "\\ga%4d\\uh\\d%2d\\um\\d%4.1f\\us\\d",dd,mm,(double)ds ) ;
   } else {
      dd = (int) inset->lonlat[0] ;
      mm = (int) ( (float) 60 * ( inset->lonlat[0] - dd ) ) ; 
      ds = ( (float) 3600 * ( inset->lonlat[0] - dd ) - 60 * mm ) ;
      sprintf( label , "\\sl%4d\\ud\\d%2d\\um\\d%4.1f\\us\\d",dd,mm,(double)ds ) ;
   }
   x = xmin + 0.95 * xsize ;			/* source position */
   y = ymin + 0.92 * ysize ;
   pgptxt_c( &x , &y , &fzero , &just , tofchar( label ) ) ;
   sign = inset->lonlat[1] / fabs( inset->lonlat[1] ) ;
   dd = (int) fabs( inset->lonlat[1] ) ;
   mm = (int) ( (float) 60 * ( fabs( inset->lonlat[1] ) - dd ) ) ; 
   ds =       ( (float) 3600 * ( fabs( inset->lonlat[1] ) - dd ) - 60 * mm ) ;
   dd = dd * sign ;
   if ( !strncmp( inset->coname , "EQU" , 3 ) ) {
      sprintf( label , "\\gd%4d\\ud\\d%2d\\um\\d%4.1f\\us\\d",dd,mm,(double)ds ) ;
   } else {
      sprintf( label , "\\sb%4d\\ud\\d%2d\\um\\d%4.1f\\us\\d",dd,mm,(double)ds ) ;
   }
   y = ymin + 0.87 * ysize ;
   pgptxt_c( &x , &y , &fzero , &just , tofchar( label ) ) ;

   if ( raw != average ) {
      x = xmin + 0.05 * xsize ;
      y = ymin + 0.92 * ysize ;
      sprintf( label , "sop/att %d/%d" , spectrum->sop , spectrum->att ) ;  
      pgtext_c( &x , &y , tofchar( label ) ) ;
   }

   n = userreal_c( psc , &two , DFLT_HIDD , PSC_KEY , PSC_MES ) ;
   if ( n > 0 ) {
      if ( ( raw->units[0] != 'J' ) && ( raw->units[0] != 'j' ) ) {
         psc[ 0 ] = psc[ 0 ] * 2.998 / 11.8 ;
         if ( raw->units[7] == '-' ) {
            psc[ 0 ] = psc[ 0 ] / 11.8 ;
         } else if ( raw->units[7] == '3' ) {
            psc[ 0 ] = psc[ 0 ] * 11.8 * 11.8 * 11.8 ;
         }
      }
      charsize = 1.1 ;
      pgsch_c( &charsize ) ;			/* set charactersize */
      pgslw_c( &thinlines ) ;
      pgsls_c( &dotted_line ) ;
      x = 1.0 ;
      pgmove_c( &x , &psc[ 0 ] ) ;
      x = 2.0 ;
      pgdraw_c( &x , &psc[ 0 ] ) ;
      pgslw_c( &thicklines ) ;
      pgtext_c( &x , &psc[ 0 ] , tofchar( "PSC 12\\gm" ) ) ;
      pgslw_c( &thinlines ) ;
      x =  8.6 ;
      pgmove_c( &x , &psc[ 0 ] ) ;
      x = 15.0 ;
      pgdraw_c( &x , &psc[ 0 ] ) ;
   }
   if ( n == 2 ) {
      if ( ( raw->units[0] != 'J' ) && ( raw->units[0] != 'j' ) ) {
         psc[ 1 ] = psc[ 1 ] * 2.998 / 25 ;
         if ( raw->units[7] == '-' ) {
            psc[ 1 ] = psc[ 1 ] / 25 ;
         } else if ( raw->units[7] == '3' ) {
            psc[ 1 ] = psc[ 1 ] * 25 * 25 * 25 ;
         }
      }
      pgslw_c( &thinlines ) ;
      x = 18.5 ;
      pgmove_c( &x , &psc[ 1 ] ) ;
      x = 25.0 ;
      pgdraw_c( &x , &psc[ 1 ] ) ;
      x = 24.0 ;
      pgslw_c( &thicklines ) ;
      pgtext_c( &x , &psc[ 1 ] , tofchar( "PSC 25\\gm" ) ) ;
   }

   pgslw_c( &thinlines ) ;			/* add detector edges */
   pgsls_c( &full_line ) ;
   edgepos[ 0 ] =  0.05 ;
   edgepos[ 1 ] = -0.05 ;
   n = 71 ;
   (void) irlrs_pos2wave_c( &n, edgepos , &two, edgewav ) ;
   y = ymin + 0.05 * ysize ;
   pgmove_c( &edgewav[0] , &y ) ;
   y = ymin + 0.025 * ysize ;
   pgdraw_c( &edgewav[0] , &y ) ;
   pgdraw_c( &edgewav[1] , &y ) ;
   y = ymin + 0.05 * ysize ;
   pgdraw_c( &edgewav[1] , &y ) ;

   n = 74 ;
   (void) irlrs_pos2wave_c( &n, edgepos , &two, edgewav ) ;
   y = ymin + 0.10 * ysize ;
   pgmove_c( &edgewav[0] , &y ) ;
   y = ymin + 0.075 * ysize ;
   pgdraw_c( &edgewav[0] , &y ) ;
   pgdraw_c( &edgewav[1] , &y ) ;
   y = ymin + 0.10 * ysize ;
   pgdraw_c( &edgewav[1] , &y ) ;

   if ( ymin < 0 && ymax > 0 ) {		/* add baseline */
      pgslw_c( &thinlines ) ;
      pgsls_c( &full_line ) ;
      x = xmin ;
      y =  0.0 ;
      pgmove_c( &x , &y ) ;
      x = xmax ;
      pgdraw_c( &x , &y ) ;
   }   

   if ( raw != average ) {
      for( sdet = 1 ; sdet < NRDETS + 1 ; sdet++ ) {
         pgslw_c( &thicklines ) ;		/* add detector numbers */
         if ( raw->det[ sdet ].onsource ) {
            if ( irlrs_dettype_c( &raw->det[ sdet ].detno ) == 1 ) {
               pgsls_c( &full_line ) ;
               x = xmin + 0.05 * xsize ;
               y = ymin + 0.87 * ysize ;
               pgmove_c( &x , &y ) ;
               x = xmin + 0.09 * xsize ;
               pgdraw_c( &x , &y ) ;
               x = xmin + 0.10 * xsize ;
               sprintf( label , "SW det. %2d" , raw->det[ sdet ].detno ) ;
               pgtext_c( &x , &y , tofchar( label ) ) ;
               n = 71 ;
              (void) irlrs_pos2wave_c( &n, edgepos , &two, edgewav ) ;
            } else if ( irlrs_dettype_c( &raw->det[ sdet ].detno ) == 2 ) {
               pgsls_c( &dotted_line ) ;
               x = xmin + 0.05 * xsize ;
               y = ymin + 0.82 * ysize ;
               pgmove_c( &x , &y ) ;
               x = xmin + 0.09 * xsize ;
               pgdraw_c( &x , &y ) ;
               x = xmin + 0.10 * xsize ;
               sprintf( label , "LW det. %2d" , raw->det[ sdet ].detno ) ;
               pgtext_c( &x , &y , tofchar( label ) ) ;
               n = 74 ;
              (void) irlrs_pos2wave_c( &n, edgepos , &two, edgewav ) ;
   	    }      
         
         					/* find end of data */
            first = -1 ;
            last  = raw->det[ sdet ].ns ;
            for ( n = 0 ; n < raw->det[ sdet ].ns ; n++ ) {
               if ( first < 0 ) {
                  if ( raw->det[ sdet ].lm[ n ] > edgewav[ 0 ] ) {
                     first = n ;
                  }
               } else { 
                  if ( raw->det[ sdet ].lm[ n ] > edgewav[ 1 ] ) {
                     last  = n ;
                     break ;
                  }
               }
            }
   
            sprintf( label , "Detector %2d, plot %3d to %3d, in detector %d to %d" ,
                            raw->det[ sdet ].detno, 0 , raw->det[ sdet ].ns , first , last ) ;
            anyout_c( ANYOUT_TST , tofchar( label ) ) ;
            pgslw_c( &thinlines ) ;
            n = first + 1 ;
            PlotLine( &n , &raw->det[ sdet ].lm[ 0 ]     ,
                           &raw->det[ sdet ].fl[ 0 ]     ) ;
            n = last - first + 1 ;
            pgslw_c( &thicklines ) ;
            PlotLine( &n , &raw->det[ sdet ].lm[ first ] ,
                           &raw->det[ sdet ].fl[ first ] ) ;
            n = raw->det[ sdet ].ns - last - 1 ;
            pgslw_c( &thinlines ) ;
            PlotLine( &n , &raw->det[ sdet ].lm[ last ]  , 
                           &raw->det[ sdet ].fl[ last ]  ) ;
         }
      }
   }

   if ( average ) {
      first = -1 ;
      last  = -1 ;
      for ( n = 0 ; n < NWAVE ; n++ ) {
      	 if ( first == -1 ) {
     	    if ( average->wave[ n ] > SWLOEDGE ) {
     	       first = n ;
     	       n = n + ( 10 / STEPWAVE ) ;
     	    }
     	 } else {
            last = n - 1 ;
            if ( average->wave[ n ] > LWHIEDGE ) {
               break ;
            }
         }
      }
      swtolw = (int) ( (float) ( SWHIEDGE - BEGINWAVE ) / STEPWAVE ) + 1 ;
      if ( ( first != -1 ) && ( last != -1 ) ) {
      	 pgsls_c( &full_line ) ;
         pgslw_c( &thinlines ) ;
         n = first + 1 ;
         PlotLine( &n , &average->wave[ 0 ]     ,
                        &average->det[ 0 ].fl[ 0 ]     ) ;
         pgslw_c( &thicklines ) ;
         n = swtolw - first + 1 ;
         PlotLine( &n , &average->wave[ first ] ,
                        &average->det[ 0 ].fl[ first ] ) ;
         n = last - swtolw ;
         PlotLine( &n , &average->wave[ swtolw + 1 ] ,
                        &average->det[ 0 ].fl[ swtolw + 1 ] ) ;
         n = NWAVE - last ;
         pgslw_c( &thinlines ) ;
         PlotLine( &n , &average->wave[ last ]  , 
                        &average->det[ 0 ].fl[ last ]  ) ;
      }
   }

   pgiden_c( ) ;				/* add identification */
   pgend_c( ) ;					/* close plot */
}

/*
   WriteSpectrum will write a table of calibrated LRS data (calib) to the
IRDS outset on disk.
*/

void WriteSpectrum( LRSSet  *outset , Spectrum *spectrum , 
                    LRSData *raw    , LRSData  *average   )
{
   fint    status , sdet = 0 , ndata = 0 ;
   char    line[MAXTXTLEN] ;

   anyout_c( ANYOUT_TST , tofchar(" - WriteSpectrum") ) ;
   if ( outset		 			/* do this one ? */
        && spectrum->done 
        && spectrum->process ){
      sprintf( line , "Writing spectrum of snip %d to set %s" , 
                      raw->snip , outset->name );
      status_c( tofchar( line ) );
      irlrs_set_addsnip_c( tofchar( outset->name ) , &raw->snip     ,
                                    &spectrum->sop , &spectrum->att , 
                                    &raw->satcal   , &status ) ;
      if ( status != 0 ) {
         sprintf( line , "IRLRS_SET_ADDSNIP error number %d" , status ) ;
         error_c( WARNING , tofchar( line ) ) ;
      } else {
         for ( sdet = 0 ; sdet < NRDETS + 1 ; sdet++ ) {
            sprintf( line , "Writing spectrum of snip %d to set %s (detector %2d)" , 
                            raw->snip , outset->name , 
                            raw->det[ sdet ].detno );
            status_c( tofchar( line ) );
            ndata = NWAVE ;
            irlrs_set_wrsnip_c( tofchar( outset->name ) , &raw->snip ,
                                &sdet  , raw->det[ sdet ].fl , 
                                &ndata , &status ) ;
            irlrs_set_wrsnip_c( tofchar( outset->name ) , &average->snip ,
                                &sdet  , average->det[ sdet ].fl , 
                                &ndata , &status ) ;
            if ( status != 0 ) {
               sprintf( line , "IRLRS_SET_WRSNIP error number %d" , status ) ;
               error_c( WARNING , tofchar( line ) ) ;
            }
         }
      }
   }
}						/* WriteSpectrum */



MAIN_PROGRAM_ENTRY
{
   char    line[MAXTXTLEN] ;

   fchar   units ;

   fint      toplevel = 0, status = 0, nitems ;

   int          done;

   LRSSet       *inset        = NULL ;
   LRSSet       *outset       = NULL ;
   Spectrum     *spectrumlist = NULL ;
   Spectrum     *spectrum     = NULL ;
   LRSData      *raw          = NULL ;
   LRSData      *average      = NULL ;
   LRSData      *nullptr      = NULL ;

   double       oldlonlat[ 2 ] = { 0 , 0 } ;

   init_c( ); 					/* get in touch with HERMES */
   IDENTIFICATION( PROGRAM , VERSION ) ;	/* show user who we are */

   finit( units , MAXTXTLEN ) ;

   inset  = GetLRSSet( );			/* ask for an input IRDS */

/* start processing data; outer loop processes different on-sky positions     */
   
   done = ( inset == NULL ) ;			/* stop ? */
   if( !done ) {
      if ( outset != NULL ) free( outset ); 

      outset = MakeLRSOutset( inset ) ;		/* ask for output set */

      sprintf( units.a , "LFL" ) ;
      nitems = usertext_c( units , DFLT_DEF , UNITS_KEY , UNITS_MES ) ;
      if ( units.a[0] == 'W' || units.a[0] == 'w' ) {
         sprintf( units.a , "pWm-2mu-1" ) ;
      } else if ( ( units.a[0] == 'L' || units.a[0] == 'l' ) 
                                      && units.a[1] == '4' ) {
         sprintf( units.a , "pWm-2mu3 " ) ;
      } else if ( units.a[0] == 'J' || units.a[0] == 'j' ) {
         sprintf( units.a , "Jy       " ) ;
      } else {
         sprintf( units.a , "pWm-2    " ) ;
      }
      if( outset ){
	  status = 0 ;
	  gdsd_wchar_c( tofchar(outset->name), tofchar("BUNIT"),
			&toplevel, units, &status ) ;
	  gdsd_wchar_c( tofchar(outset->name), tofchar("CUNIT1"),
			&toplevel, tofchar("mu"), &status ) ;
      }
   }

   while( !done ) {
      done = GetSpectrumPosition( inset );	/* ask for a position */

      if( !done ) {				/* generate a list of spectra */
         spectrumlist = GetSpectrumList( inset , oldlonlat , spectrumlist );
      }
      
      if( !done ) {

         sprintf( line , "Working on set %s at (%f,%f)", inset->name , 
                          inset->lonlat[0], inset->lonlat[1] );
         anyout_c( ANYOUT_DEF , tofchar( line ) );

         spectrum = NextSpectrum( spectrumlist ) ;/* start with the first */

         if ( average ) free( average ) ;
         average = InitialiseLRSData( 0 ) ;	/* initialise average */

         while( spectrum ) { 			/* loop on spectra */
            if ( raw ) free( raw ) ;
						/* read data */
            raw = ReadSpectrum( inset , spectrum ) ;
            sprintf( raw->units, "%s         " , units.a ) ;
						/* calibrate this spectrum */
            CalibrateSpectrum( spectrum , raw ) ; 
						/* put to regular grid */
            SampleSpectrum( raw ) ;
						/* align spectral halves */
            AlignSpectrum( raw ) ;
						/* add detectors together */
            SumSpectrum( raw , average ) ;
                                                /* report to the user	*/
            ReportSpectrum( spectrum , raw ) ;
						/* plot this spectrum */
            PlotSpectrum( inset , spectrum , raw , nullptr ) ;
						/* write the table to disk */
            WriteSpectrum( outset , spectrum , raw , average ) ;	

            if ( LastSpectrum( spectrum ) ) {	/* last spectrum ? */
						/* yes -> plot average */
               PlotSpectrum( inset , spectrum , average , average ) ;
						/*       and delete average */
               if ( average ) free( average ) ;
               average = InitialiseLRSData( 0 ) ;
            }
						/* goto next spectrum */
            spectrum = NextSpectrum( spectrumlist ) ;
         }

      }
      oldlonlat[ 0 ] = inset->lonlat[ 0 ] ;
      oldlonlat[ 1 ] = inset->lonlat[ 1 ] ;

   }
   finis_c( );					/* bye, bye HERMES */
   return( 0 ) ;
}

