/* identification */
#define VERSION		"1.4"		/* version number */
#define PROGRAM		"PLSCAN"	/* program name */

/* plscan.c

                            COPYRIGHT (c) 1990
            Kapteyn Astronomical Institute - University of Groningen
                P.O. Box 800, 9700 AV Groningen, The Netherlands

#>            plscan.dc1

Program:      plscan

Purpose:      Plots raw IRAS scans

Category:     IRAS

File:         plscan.c

Author:       P.R. Roelfsema

Keywords:

    IRSET=    Name of input IR data set [stop]

      POS=    Give a reference position for plot [ none ]
              If a position is entered here which lies within the coordinates
              as defined in the header of the input irds, the data are plotted
              as function of in-scan distance to that position. Default is
              a plot as function of satcal.

    SDETS=    Give begin and end sdet [all detectors]

     SORT=    Plot detectors sorted according to focal plane layout? [ Y ]

    ALIGN=    Plot detectors aligned in time domain ? [ Y ]

    TICKS=    Give begin and end tick [entire snip]

***MINMAX=    Min and max levels to be plotted [ min and max of data ].

 GRDEVICE=    Give graphics device to plot on [ list of available devices ]

              For LRS data the following hiden keywords can be specified:

*** ADROOP=   Do you want anti-droop correction applied ? [ Y ]

*** THPF=     High-pass filter time constants [ all 10 sec. ]

*** ADLIM=    Anti-droop limits [ all 0.1 mV. ]


Updates:      Dec  7, 1990: PRR, Document created.
              Dec 11, 1990: PRR, Version 1.0.
              Dec 17, 1990: PRR, Added LRS anti-droop.
              Jan 11, 1991: PRR, Added POS= keyword.
              Feb 11, 1991: PRR, Upgrade to PGPLOT.
              Oct 10, 1991: WZ,  PGPLOT standard names implemented
              Mar 19, 1992: HB,  New interface to irds_enquire_snip
#<
*/

#include "gipsyc.h"
#include "cmain.h"
#include "ctype.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "math.h"
#include "init.h"
#include "float.h"
#include "usertext.h"
#include "userreal.h"
#include "userint.h"
#include "userangle.h"
#include "userlog.h"
#include "nelc.h"
#include "anyout.h"
#include "status.h"
#include "error.h"
#include "cancel.h"
#include "sortrai.h"
#include "gdsinp.h"
#include "gdsd_rchar.h"
#include "gdsd_rint.h"
#include "gdsc_grid.h"
#include "gdsc_word.h"
#include "minmax1.h"
#include "irds_exist.h"
#include "irds_enquire.h"
#include "irds_enquire_snip.h"
#include "irds_rd_samples.h"
#include "irds_close.h"
#include "irds_rd_detoff.h"
#include "ircc_times.h"
#include "ircc_mask.h"
#include "ircc_bandnr.h"
#include "irlrs_adroop.h"
#include "irco_number.h"
#include "irco_precess.h"
#include "pgbeg.h"
#include "pgqinf.h"
#include "pgsch.h"
#include "pgslw.h"
#include "pgsls.h"
#include "pgsvp.h"
#include "pgswin.h"
#include "pgbox.h"
#include "pglab.h"
#include "pgtext.h"
#include "pgmtxt.h"
#include "pgptxt.h"
#include "pgmove.h"
#include "pgline.h"
#include "pgdraw.h"
#include "pgpt.h"
#include "pgiden.h"
#include "pgend.h"
#include "finis.h"
#include "setfblank.h"

#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.l = len ; }


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
#define INSET_MES	tofchar("Give name of IR data set [stop]")
#define	POS_KEY		tofchar("POS=")
#define	POS_MES		tofchar("Give a reference position for plot [ none ]")
#define PLOT_KEY	tofchar("PLOT=")
#define PLOT_MES	tofchar("Give plot device [ tektronix ]")
#define MINMAX_KEY	tofchar("MINMAX=")
#define MINMAX_MES	tofchar("Give min and max for plot [data min and max]")
#define TICKS_KEY	tofchar("TICKS=")
#define	TICKS_MES	tofchar("Give begin and end tick [entire snip]")
#define SDET_KEY	tofchar("SDETS=")
#define	SDET_MES	tofchar("Give begin and end sdet [all detectors]")
#define SORT_KEY	tofchar("SORT=")
#define	SORT_MES	tofchar("Plot detectors sorted according to focal plane pos. ? [Y]")
#define ALIGN_KEY	tofchar("ALIGN=")
#define	ALIGN_MES	tofchar("Plot detectors aligned in time domain? [Y]")
#define ADROOP_KEY	tofchar("ADROOP=")
#define	ADROOP_MES	tofchar("Do you want anti-droop correction applied ? [Y]")

/* miscellaneous definitions */
#define true                1
#define false               0
#define MAXTXTLEN         256			/* length of textlines */
#define	MAXSUBSETS	 1024			/* max number of subsets */
#define NAXES		    4			/* number of axes in irds */
#define MAXDATA		60000			/* maximum nr. of data points */
#define MAXDETS		   16			/* max number of detectors */
#define PI		3.14159265358979	/* pi */
#define	RADPERDEG	( PI / 180.0 ) 		/* radians per degree */

MAIN_PROGRAM_ENTRY
{
   fint       one = 1 , two = 2 , n , m ;
   fint       full_line = 1 , dotted_line = 4 ;
   fint       dot_symbol = 17 ;
   fint       thinlines = 1 , thicklines = 5 ;

   bool       inpok  = true  ;			/* found good IRDS? */
   bool       posoff = false ;			/* plot position offsets ? */
   bool       sort   = true  ;			/* sort detectors? */
   bool       align  = true  ;			/* align detectors? */
   bool       adroop = true  ;			/* anti-droop LRS? */

   fchar      irds ;				/* input irds */
   fchar      object ;				/* object name */
   fchar      instrument ;			/* instrument name */
   fchar      cosys ;				/* coordinate system name */
   fchar      units ;				/* units of data */
   fchar      pgreply ;				/* answer from pgqinf */
   fchar      scantype ;   
 
   fint       subsets[ MAXSUBSETS ] ;		/* subsets of irds to plot */
   fint       nsubs ;				/* number of subsets */
   fint       subset ;				/* subset number */
   fint       maxsub = MAXSUBSETS ;		/* max nr. of subsets */
   fint       axperm[ NAXES ] ;			/* ax-permutation array */
   fint       axlen[ NAXES ] ;			/* ax lengths */
   fint       axcount[ NAXES ] ;		/* ax lengths spec. by user */
   fint       axnum ;				/* number of axes */
   fint       classdim ; 			/* class dimensionality */
   fint       status = 0 ;			/* status variable */
   fint       error = 0 ;			/* error return codes */
   fint       level = 0 ;			/* subset leve variable */
   fint       snip ; 				/* snip number */
   fint       sdets[ MAXDETS ] ;		/* sdet numbers */
   fint       ndets ;				/* number of dets per plot */
   fint       ticks[ 2 ] ;			/* tick numbers */
   fint       sop , obs, att ; 		 	/* sop/obs/att numbers */
   fint       scancal , scandur ;		/* begin/length of scan */
   fint       snipcal , snipdur ;		/* begin/length of scan */
   fint       ndata , nticks ;			/* nr. of data points, ticks */
   fint       detno ;				/* detector number */
   fint       incosys ;				/* coord. sys. of irds */
   fint       plcosys ;				/* coord. sys. of plot */
   fint       sunsys ;				/* coord. sys. sun-referenced */
   fint       zero = 0 ;
   fint       nitems ;				/* nr itmes returned */

   double     center[ 2 ] ;			/* center of irds */
   double     size[ 2 ] ;			/* size of irds */
   double     pos[ 2 ] ;			/* reference position */
   double     satrate =        1.000054 ;	/* seconds per satcal */
   double     ut0     = 65232005.4 ;		/* UTC at satcal 0 */
   double     ut ;				/* ut at satcal */
   double     inscan[ MAXDATA ] ;		/* inscan offsets */
   double     xscan[ MAXDATA ] ;		/* cross-scan offsets */
   double     twist[ MAXDATA ] ;		/* twist angles */

   real       epoch ;				/* epoch of irds coords. */
   real       twothou = 2000.0 ;		/* epoch of 2000.0 */
   real       fzero = 0.0 ;
   real       psi , psirate , theta ;		/* intended position data */
   real       yloc , zloc[ MAXDETS ] , ydsiz , zdsiz ;
   real       delay = 0 ;			/* time offsets w.r.t. BPHF */
   real       data[ MAXDATA ] ;			/* data read from irds */
   real       xpts[ MAXDATA ], ypts[ MAXDATA ];	/* x and y values for plot */
   real       xoff = 0 , xmin , xmax , xsize ;	/* x-offset, min, max, size */
   real       minmax[ 2 ] , range ;		/* min, max and range of data */
   real       ymin , ymax , ysize ;
   real       wxmin , wymin , wxmax , wymax ;	/* viewport limits */
   real       just , x , y ;			/* plot coordinates */
   real       charsize ;			/* character size */
   real       blank ;

   char       line[ MAXTXTLEN ]	;		/* line for messages etc. */
   char       xlab[ MAXTXTLEN ] ;
   char       ylab[ MAXTXTLEN ] ;
   char       label[ MAXTXTLEN ] ;

   init_c( ); 					/* get in touch with HERMES */
   IDENTIFICATION( PROGRAM , VERSION ) ;	/* program identification */
   setfblank_c( &blank ) ;
   finit( irds       , MAXTXTLEN ) ;		/* initialize fchar's */
   finit( object     , MAXTXTLEN ) ;
   finit( instrument , MAXTXTLEN ) ;
   finit( cosys      , MAXTXTLEN ) ;
   finit( units      , MAXTXTLEN ) ;
   finit( pgreply    , MAXTXTLEN ) ;
   finit( scantype   , 20 ) ;

   do {						/* loop to get irds */
      inpok    = true ;				/* positive thinking */
      classdim = 0 ;				/* user decides subset dim. */
      axnum = NAXES ;				/* ask NAXES axes */
      nsubs = gdsinp_c( irds      , subsets   , &maxsub    , DFLT_DEF  ,
                        INSET_KEY , INSET_MES , ANYOUT_TST , axperm    , 
                        axcount   , &axnum    , &one       , &classdim  ) ; 
      if ( nsubs == 0 ) {			/* CR => */
         break ;				/*          exit */
      }
      if ( irds_exist_c( irds , &status ) != 0 ) {
         inpok = false ; 			/* not an irds */
         sprintf( line , "IRDS %.*s is not a legal irds" , 
                                   irds.a  , nelc_c( irds ) ) ;
      } else if ( classdim < 2 ) {
         inpok = false ;			/* wrong D of subset spec. */
         sprintf( line , "You can ONLY specify SNIP and/or SDET here" ) ;
      } else if ( classdim == 4 || ( classdim == 3 && axperm[ 3 ] != 4 ) ) {
         inpok = false ;			/* no SNIP in 1D subset spec. */
         sprintf( line , "You MUST specify at least SNIP here" ) ;
      } else if ( ( axperm[ 2 ] != 3 ) && ( axperm[ 3 ] != 3 ) ) {
         inpok = false ;			/* no SDET in 2D subset spec. */
         sprintf( line , "You can ONLY specify SNIP and/or SDET here" ) ;
      } else if ( ( axperm[ 2 ] != 4 ) && ( axperm[ 3 ] != 4 ) ) {
         inpok = false ;			/* no SNIP in 2D subset spec. */
         sprintf( line , "You can ONLY specify SNIP and/or SDET here" ) ;
      }
      if ( !inpok ) {				/* bad input => */
         error_c( WARNING , tofchar( line ) ) ;	/* tell user */
         cancel_c( INSET_KEY ) ;		/* and try again */
      }
   } while ( !inpok ) ;				/* found a good irds ? */

   if ( nsubs > 0 ) {				/* user specified a set */
   						/* get general irds info */
      irds_enquire_c( irds , object , instrument , &axnum , axlen ,
                      center , size , cosys , &epoch , &status ) ;
   

						/* set up coordinates */
      incosys  =  irco_number_c(         cosys               , &twothou ) ;
      sunsys   =  irco_number_c( tofchar( "SUN REFERENCED" ) , &fzero   ) ;
      plcosys  = 0 ;
      if ( epoch != twothou ) {
         irco_precess_c( &incosys , &epoch   , &plcosys ) ;
      } else {
         plcosys = incosys ;
      }
      sprintf( line , "Coordinate systems: in %d, plot %d , sunref %d." ,
                         incosys , plcosys , sunsys ) ;
      anyout_c( ANYOUT_TST , tofchar( line ) ) ;
   
      posoff = false ;
      do {					/* get a position from user */
         nitems = userangle_c( pos , &two , DFLT_DEF , POS_KEY , POS_MES ) ;
         if ( nitems > 0 ) {
            if ( nitems != 2 ) {
               sprintf( line , "You must give TWO %.*s coordinates (%6.1f)" ,
                                  nelc_c( cosys ) , cosys.a , epoch ) ;
               error_c( WARNING , tofchar( line ) ) ;
               cancel_c( POS_KEY ) ;
            } else {
               if ( ( pos[ 0 ] < center[ 0 ] - 0.5 * size[ 0 ] ) ||
                    ( pos[ 0 ] > center[ 0 ] + 0.5 * size[ 0 ] ) ||
                    ( pos[ 1 ] < center[ 1 ] - 0.5 * size[ 1 ] ) ||
                    ( pos[ 1 ] > center[ 1 ] + 0.5 * size[ 1 ] ) ) {
                  error_c( WARNING , tofchar( "Position outside of plot" ) ) ;
                  cancel_c( POS_KEY ) ;
               } else {
                  posoff = true ;
                  pos[ 0 ] = pos[ 0 ] * RADPERDEG ;
                  pos[ 1 ] = pos[ 1 ] * RADPERDEG ;
                  break ;
               }
            }
         } else {
            break ;
         }
      } while ( true ) ;


						/* process subsets */
      for ( subset = 0 ; subset < nsubs ; subset++ ) {
         axnum = 4 ;				/* get snip number of subset */
         snip  = gdsc_grid_c( irds , &axnum , &subsets[ subset ] , &error ) ;
   						/* get snip header info */
         irds_enquire_snip_c( irds     , &snip    , &sop     , &obs      ,
			      &att     , scantype , &scancal , &scandur  , 
                              &snipcal , &snipdur , &psi     , &psirate  , 
                              &theta   , &status   ) ;
   
   
   						/* get range in ticks */
         ticks[ 0 ] = 1 ;			/* default ticks : */
         ticks[ 1 ] = snipdur ;			/*      alll!      */
   						/* ask user for ticks */
         n = userint_c( ticks , &two , DFLT_DEF , TICKS_KEY , TICKS_MES ) ;
   						/* check ticks */
         if ( ticks[ 0 ] < 1       ) ticks[ 0 ] =       1 ;
         if ( ticks[ 1 ] > snipdur ) ticks[ 1 ] = snipdur ;
         nticks = ticks[ 1 ] - ticks[ 0 ] + 1 ;	/* total number of ticks */
         ndata  = nticks * axlen[ 0 ] ;		/* total number of samples */
   
   
   						/* get range in sdet */
         if ( classdim == 2 ) {			/* sdet spec. in subsets */
            axnum      = 3 ;			/* get sdet number of subset */
            sdets[ 0 ] = gdsc_grid_c( irds , &axnum , &subsets[ subset ] , &error ) ;
            ndets      = 1 ;			/* only one sdet per plot */
         } else {
   						/* set default sdets */
            for ( n = 0 ; n < axlen[ 2 ] ; n++ ) sdets[ n ] = n + 1 ;
   						/* ask user for sdets */
            ndets = userint_c( sdets , &axlen[ 2 ] , DFLT_DEF , SDET_KEY , SDET_MES ) ;
            if ( ndets == 0 ) ndets = axlen[ 2 ] ; 
         }
         if ( ndets > 1 ) {			/* > one sdet per plot */
    						/* prepare user message */
            sprintf( line , "Set %.*s, snip %d, sdet %d to %d, satcal %d to %d (%d samples)" ,
                  (int) nelc_c( irds ) , irds.a , snip , 
                  sdets[ 0 ] , sdets[ ndets - 1 ] ,
                  scancal + snipcal + ticks[ 0 ] , 
                  scancal + snipcal + ticks[ 0 ] + nticks , ndata ) ;
         } else {				/* one sdet per plot */
    						/* prepare user message */
            sprintf( line , "Set %.*s , snip %3d, sdet %2d, satcal %6d to %6d (%d samples)" ,
                  (int) nelc_c( irds ) , irds.a , snip , sdets[ 0 ] ,
                  scancal + snipcal + ticks[ 0 ] , 
                  scancal + snipcal + ticks[ 0 ] + nticks , ndata ) ;
         }
         anyout_c( ANYOUT_DEF , tofchar( line ) ) ;/* send user message */
         if ( ndets > 1 ) {			/* ask sort and align */
            n = userlog_c( &sort  , &one , DFLT_DEF , SORT_KEY  , SORT_MES  ) ;
            n = userlog_c( &align , &one , DFLT_DEF , ALIGN_KEY , ALIGN_MES ) ;
         } else {
            sort  = false ;
            align = false ;
         }

   						/* determine plot coordinates */
         status_c( tofchar( "Looking for min and max" ) );
   						/* LRS? -> AD-correction */
         n = userlog_c( &adroop  , &one , DFLT_HIDD , ADROOP_KEY , ADROOP_MES ) ;
         if ( adroop && ircc_bandnr_c( instrument ) == 5 ) {
            strncpy( units.a , "mV" , 2 ) ;
         } else {
            level = 0 ;				/* get units from top level */
            gdsd_rchar_c( irds , tofchar("BUNIT") , &level , units , &error ) ;
         }
   
         for ( n = 0 ; n < ndets ; n++ ){	/* loop on all sdets */
            level  = 0 ;			/* start at top level */
            axnum  = 3 ;			/*     at third axis */
   						/* make sdet level */
            level  = gdsc_word_c( irds , &axnum , &sdets[ n ] , &level , &error ) ;
   						/* get DETNO */
            gdsd_rint_c( irds , tofchar( "DETNO" ) , &level , &detno , &error ) ;
   						/* read the samples */
            irds_rd_samples_c( irds , &snip  , &sdets[ n ]  , &ticks[ 0 ] ,
                               data , &ndata , &status ) ;
            if ( status != 0 ) {		/* could not get samples */
               sprintf( line , "Could not read samples: status %d" , status ) ;
               error_c( WARNING , tofchar ( line ) ) ;
            }
            if ( posoff && ( n == 0 ) ) {
               nitems = irds_rd_detoff_c( irds , &snip , &sdets[ n ] , &ticks[ 0 ] ,
                                          &plcosys , &pos[ 0 ] , &pos[ 1 ] ,
                                          &sunsys  , &zero ,
                                          inscan , xscan , twist , 
                                          &ndata , &status ) ;
               if ( inscan[ 0 ] < inscan[ ndata - 1 ] ) {
                  xmin = inscan[ 0 ]         * 3600 / RADPERDEG ;
                  xmax = inscan[ ndata - 1 ] * 3600 / RADPERDEG ;
               } else {
                  xmin = inscan[ ndata - 1 ] * 3600 / RADPERDEG ;
                  xmax = inscan[ 0 ]         * 3600 / RADPERDEG ;
               }
            }
   						/* LRS? -> AD-correction */
            if ( adroop && ircc_bandnr_c( instrument ) == 5 ) {
               irlrs_adroop_c( &detno , data , &ndata ) ;
            }
   						/* get focal plane geometry */
            error  = ircc_mask_c( &detno , &yloc , &zloc[ n ] , &ydsiz , &zdsiz ) ;
            if ( n == 0 ) {			/* first sdet?  */
   						/* get min and max */
               minmax1_c( data , &ndata , &minmax[ 0 ] , &minmax[ 1 ] ) ;
               if( minmax[0] == blank ) minmax[0] = FLT_MAX ;
               if( minmax[1] == blank ) minmax[1] = -FLT_MAX ;
            } else {				/* not first sdet */
   						/* get minmax in buffer */
               minmax1_c( data , &ndata , &ymin , &ymax ) ;
   						/* compare with earlier minmax */
               if ( ymin != blank && ymin < minmax[ 0 ] ) minmax[ 0 ] = ymin ;
               if ( ymax != blank && ymax > minmax[ 1 ] ) minmax[ 1 ] = ymax ;
            }
         }

   						/* tell user minmax */
         sprintf( line , "Data minimum %12.5e %.*s, maximum %12.5e %.*s" ,
                            minmax[ 0 ] , nelc_c( units ) , units.a ,
                            minmax[ 1 ] , nelc_c( units ) , units.a ) ;
         anyout_c( ANYOUT_DEF , tofchar( line ) ) ;
   						/* ask user new minmax */
         n = userreal_c( minmax , &two , DFLT_HIDD , MINMAX_KEY , MINMAX_MES ) ;
   						/* find data range */
         range = 1.05 * ( minmax[ 1 ] - minmax[ 0 ] ) ;
         ymax  = minmax[0] + 1.3  * ndets * range ;/* maximum y in plot */
         ymin  = ymax      - 1.45 * ndets * range ;/* minimum y in plot */
         ysize = ymax - ymin ;			/* range in y in plot */
   
         if ( posoff ) {
            xsize = xmax - xmin ;		/* range in x in plot */
            xmin = xmin - 0.05 * xsize ;
            xmax = xmax + 0.1  * xsize ;
         } else {
            xoff = scancal + snipcal + ticks[ 0 ] ;	/* get satcal offset */
            xoff = xoff - fmod( xoff , 100.0 ) ;/* offset to multiple of 100 */
      						/* make min/max in x in plot */
            xmin = 0.95 * ( (float) ( scancal + snipcal + ticks[ 0 ] ) - xoff ) ;
            xmax = 1.1  * ( (float) ( scancal + snipcal + ticks[ 0 ] ) - xoff 
                              + ( (float) ( ndata - 1 ) ) / (float) axlen[ 0 ] ) ;
         }
         xsize = xmax - xmin ;			/* range in x in plot */
   
   						/* if wanted, sort on zloc */
         if ( sort ) sortrai_c( zloc , sdets , &ndets ) ;
         if ( sort ) sortrai_c( zloc , sdets , &ndets ) ;

						/* make the plot */
         error = pgbeg_c( &zero , tofchar( "?" ) , &one , &one ) ;
         if ( error != 1 ) {
            error_c( FATAL_ERROR , tofchar( "pgbeg error ... " ) ) ;
         }
         pgqinf_c( tofchar( "TYPE" ) , pgreply , &n ) ;
         if ( !strncmp( pgreply.a , "TEK4010" , 6 ) ) {
            thicklines = thinlines ;
         }
         charsize = 1.5 ;			/* set character size */
         pgsch_c( &charsize ) ;
         pgslw_c( &thicklines ) ;		/* set line thickness */
   
         wxmin = 0.15 ;				/* setup viewport/window */
         wxmax = 0.95 ;
         wymin = 0.15 ;
         wymax = 0.90 ;
         pgsvp_c(  &wxmin , &wxmax , &wymin , &wymax ) ;
         pgswin_c( &xmin  , &xmax  , &ymin  , &ymax  ) ;
   
   						/* plot the frame */
         if ( ndets > 1 ) {			/* many detectors? */
            pgbox_c( tofchar( "BCNTS" )  , &fzero , &zero , 
                     tofchar( "BC" )     , &fzero , &zero ) ;
         } else {
            pgbox_c( tofchar( "BCNTS" )  , &fzero , &zero , 
                     tofchar( "BCNTSV" ) , &fzero , &zero ) ;
         }
   
   						/* create frame labels */
         if ( posoff ) {
            sprintf( xlab , "Inscan offset (arcsec)" ) ;
         } else if ( align ) {
            sprintf( xlab , "Boresight satcal (ticks - %.0f)" , xoff ) ;
         } else {
            sprintf( xlab , "Satcal (ticks - %.0f)" , xoff ) ;
         }
         sprintf( ylab , "Intensity (%.*s)" , nelc_c( units ) , units.a ) ;
   						/* plot the frame */
         pglab_c( tofchar( xlab ) , tofchar( ylab ) , tofchar( " " ) ) ;
   
   						/* annotate the plot */
         x    =  0.0  ;				/* program ID */
         y    =  0.5  ;
         just =  0.0  ;
         sprintf( label , "%s v. %s" ,  PROGRAM , VERSION ) ;
         pgmtxt_c( tofchar( "T" ) , &y , &x , &just , tofchar( label ) ) ;
         charsize = 1.2 ;			/* set character size */
         pgsch_c( &charsize ) ;
         x    =  1.0  ;				/* set specification */
         y    =  0.5  ;
         just =  1.0  ;
         if ( ndets == 1 ) {
            sprintf( label , "set %.*s , snip %d, sdet %d, ticks %d-%d" , 
                     nelc_c( irds ) , irds.a , snip , sdets[ 0 ] ,
                     ticks[ 0 ] , ticks[ 1 ] ) ;
         } else {
            sprintf( label , "set %.*s , snip %d, ticks %d-%d" , 
                     nelc_c( irds ) , irds.a , snip , 
                     ticks[ 0 ] , ticks[ 1 ] ) ;
         }
         pgmtxt_c( tofchar( "T" ) , &y , &x , &just , tofchar( label ) ) ;

         x    =  0.05 ;				/* source specification */
         y    = -2.5  ;
         just =  0.0  ;
         sprintf( label , "%.*s with %.*s" , 
                                 nelc_c( object )     , object.a     ,
                                 nelc_c( instrument ) , instrument.a ) ;
         pgmtxt_c( tofchar( "T" ) , &y , &x , &just , tofchar( label ) ) ;
   
         x    =  0.05 ;				/* sop/att */
         y    = -4.0  ;
         just =  0.0  ;
         sprintf( label , "sop/att %d/%d" , sop , att ) ; 
         pgmtxt_c( tofchar( "T" ) , &y , &x , &just , tofchar( label ) ) ;
   
         charsize = 1.0 ;			/* set character size */
         pgsch_c( &charsize ) ;
   
   						/* add alignment marker */
         if ( !posoff && align ) {		/* detectors are aligned */
            pgslw_c( &thinlines ) ;		/* set line thickness */
            pgsls_c( &full_line ) ;		/* plot full line */
            x = scancal + snipcal + ticks[ 0 ] - xoff ;
            y = minmax[ 0 ] ;
            pgmove_c( &x , &y ) ;
            y = minmax[ 0 ] + ndets * range ;
            pgdraw_c( &x , &y ) ;
            pgslw_c( &thicklines ) ;		/* set line thickness */
            pgtext_c( &x , &y , tofchar( "time aligned" ) ) ;
         }
   
   						/* add UT marker */
         if ( !posoff ) {
   						/* plot UTCS at start snip */
            ut = ( (double) ( xmin + xoff ) ) * satrate + ut0 ;
            pgmove_c( &xmin , &minmax[ 0 ] ) ;
            x = xmin + 0.05 * xsize ;
            y = ymin + 0.05 * ysize ;
            pgslw_c( &thinlines ) ;		/* set line thickness */
            pgsls_c( &full_line ) ;		/* plot full line */
            pgdraw_c( &x , &y ) ;
            pgslw_c( &thicklines ) ;		/* set line thickness */
            sprintf( label , "UTCS %9.0f (\\(2233) 10 sec)" , ut ) ;
            pgtext_c( &x , &y , tofchar( label ) ) ;   
            pgpt_c( &one , &xmin , &minmax[ 0 ] , &dot_symbol ) ;
         }

						/* start with the data */
         for ( m = 0 ; m < ndets ; m++ ) {	/* loop on detectors */
            level  = 0 ;			/* get DETNO (again ) */
            axnum  = 3 ;
            level  = gdsc_word_c( irds , &axnum , &sdets[ m ] , &level , &error ) ;
            gdsd_rint_c( irds , tofchar( "DETNO" ) , &level , &detno , &error ) ;
            sprintf( line , "Plotting snip %3d, sdet %2d (detector %2d)" ,
                                      snip , sdets[ m ] , detno ) ;
            status_c( tofchar( line ) ) ;
   						/* read detector data */
            irds_rd_samples_c( irds , &snip  , &sdets[ m ]  , &ticks[ 0 ] ,
                               data , &ndata , &status ) ;
   						/* LRS? -> AD-correction */
            if ( adroop && ircc_bandnr_c( instrument ) == 5 ) {
               irlrs_adroop_c( &detno , data , &ndata ) ;
            }
            if ( posoff ) {
               nitems = irds_rd_detoff_c( irds , &snip , &sdets[ m ] , &ticks[ 0 ] ,
                                          &plcosys , &pos[ 0 ] , &pos[ 1 ] ,
                                          &sunsys  , &zero ,
                                          inscan , xscan , twist , 
                                          &ndata , &status ) ;
            }
   
   						/* plot detector baseline */
            pgslw_c( &thinlines ) ;		/* set line thickness */
            pgsls_c( &dotted_line ) ;		/* plot dotted line */
            y = m * range + minmax[ 0 ] ;
            pgmove_c( &xmin , &y ) ;
            pgdraw_c( &xmax , &y ) ;
            x = xmin + 0.98 * xsize ;
            just = 1.0 ;
            pgslw_c( &thicklines ) ;		/* set line thickness */
            sprintf( label , "det. %2d" , detno ) ;
            pgptxt_c( &x , &y , &fzero , &just , tofchar( label ) ) ;        
   
   						/* get focal plane position */
            error = ircc_mask_c( &detno , &yloc , &zloc[ m ] , &ydsiz , &zdsiz ) ;
   
   						/* add detector edge markers */
            if ( posoff ) {
               pgslw_c( &thinlines ) ;		/* set line thickness */
               pgsls_c( &full_line ) ;		/* plot full line */
               x = -ydsiz * 30 ; 
               y =  m * range + minmax[ 0 ] ;
               pgmove_c( &x , &y ) ;
               y =  m * range + minmax[ 0 ] + 0.5 * range ;
               pgdraw_c( &x , &y ) ;
               x =  ydsiz * 30 ;
               y =  m * range + minmax[ 0 ] ;
               pgmove_c( &x , &y ) ;
               y =  m * range + minmax[ 0 ] + 0.5 * range ;
               pgdraw_c( &x , &y ) ;
            } else if ( align ) {		/* align detectors */
   						/* make delay time */
               delay = ircc_times_c( &detno ) - 
                        yloc / ( sin( theta * 0.01745329 ) * psirate * 60 ) ;
            } else {				/* no alignement */
               delay = 0 ;			/* delay = 0 */
            }


						/* plot the data */
            for ( n = 0 ; n < ndata ; n++ ) {	/* loop on data points */
   						/* determine x-coordinate */
               if ( posoff ) {
                  xpts[ n ] = inscan[ n ] * 3600 / RADPERDEG ;
               } else {
                  xpts[ n ] = (float) ( scancal + snipcal + ticks[ 0 ] ) - xoff 
                                 + ( (float) n ) / (float) axlen[ 0 ] 
                                 - delay ;
               }
   						/* add datapoint to plot */
               ypts[ n ] = data[ n ] + m * range ;
            }
            pgsls_c( &full_line ) ;		/* plot full line */
            pgslw_c( &thicklines ) ;		/* set line thickness */
            pgline_c( &ndata , xpts , ypts ) ;
         }
         pgiden_c( ) ;				/* add identification */
         pgend_c( ) ;				/* quit pgplot */
      }						/* end subset loop */
      irds_close_c( irds , &status ) ;		/* close the irds */
   }
   
   finis_c( );					/* bye bye */
   return( 0 ) ;
}
   
