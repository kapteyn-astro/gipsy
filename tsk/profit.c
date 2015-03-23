/* profit.c
                           COPYRIGHT (c) 1996
                      Kapteyn Astronomical Institute
                  University of Groningen,  P.O. Box 800,
                    9700 AV Groningen, The Netherlands


#>            profit.dc1

Warning:      This program is not maintained anymore. In the near future, 
              it will be removed from the set of GIPSY tasks. An
              improved version with graphical user interface is called 
              XGAUPROF.

Program:      profit

Purpose:      Interactive fitting of one-dimensional profiles

Category:     PROFILES, PLOTTING

File:         profit.c

Author:       P.R. Roelfsema (M. Vogelaar)

Keywords:


   STARTNEW=  Start improved version of this program?           [Y]/N
   
              A new version with graphical user interface is called 
              XGAUPROF. You can start it here.
               
   
 **STOP=      Stop processing?                                     [N]

 **NEXT=      Process next profile?                                [N]

   INSET=     Name of input data set and subsets                [quit]

   BOX=       Give area for processing                    [entire map]

   POS=       Give position for profile to process     [center of map]
              This keyword allows you to go to a new 
              position for fitting. 
              Giving POS=Q will stop the program.

 **GRIDS=     Plot the grids ?                                     [N]

 **ZERO=      Plot the zero level ?                                [N]

   GRDEVICE=  Give plot device                  [list all plotdevices]


   Next loop;

    FIT=      Give type of function to fit            [same as before]
              Here you can specify a function name and, 
              if appropriate the number of components 
              to use; FNAME <n>.

              P(OLY) N  - an N-th order polynomial
              G(AUSS) N - an N-component guassian
              V(OIGT) N - an N-component voigt profile

    GUESS0=   Give a guess for the zero-level                     [0]
              This keyword is asked only when appropriate 
              e.g. for a GAUSS fit.

    GUESSn=   Give guess of ... for FNAME fit                 [ ... ]
              Give values for the different fit parameters 
              as relevant for FNAME. 
              If you specified a parameter to be fixed in
              the fit, the guess will be taken as the value.

    FIX=      Give parameters to fix in fit.
              Specify which parameters of the fit should be kept fixed
              at the value given using the GUESSn= keywords.

              Possibilities:
                POLY  - none
                GAUSS - BASE, AMP1, CENTER1, FWHM1, AMP2, CENTER2 etc.
                VOIGT - BASE, AMP1, CENTER1, FWHM1, LOR1, AMP2, CENTER2 etc.

  **xxxLIM=   Give limits for xxx in fit                        [none]

              e.g.   BASELIM=-10 10 implies that the fitted base level
              should be between -10 and +10. If the baselevel is found
              outside that range, the fit is considered to be BAD. Other
              keywords are e.g. AMP1LIM=, CENTER1LIM= etc.

   MODE=      Which mode? 1-inter, 2-semi, 3-auto                  [1]
              This keyword is asked only once, after that 
              it becomes a hidden keyword, and can be specified 
              at any time during the loop through different positions.

   SN=        Give minimum acceptable signal-to-noise ratio        [3]
              Fits in which any of the fitted parameters 
              have a formal signal to noise ratio less than SN 
              will be considered bad. As a result the output 
              map(s) will contain undefined values for the profile.

   JUDGE=     GOOD/BAD and NEXT profile, profile AGAIN, MORE fits, 
              or SUBTRACT.
              Give a judgement on the fit; is it GOOD or BAD, and tell
              what to do next; go to the NEXT profile, do MORE fits
              or SUBTRACT this fit.
              The default for GOOD/BAD depends e.g. on the SN criterion
              specified above. The default for what next is NEXT.
              Note, for all commands the first character suffices.
              Entering QUIT will cause the program to stop.

   POS=       Give position for profile to process        [next pixel]

   CUTM=      Give cutoff level to use in mask set       [no mask set]

   MASKSET=   Give set to use for mask

 **CONT=      Is this continuation of previous run?                [N]
              If CONT=N the output sets are set to undefined, if
              you want to continue a previous run of PROFIT use CONT=Y

   PARSET=    Give setname to save parameters  [don't save parameters]

   ERRSET=    Give setname to save errors          [don't save errors]

   MODSET=    Give setname to save fitted models          [don't save]

   RESSET=    Give setname to save residuals    [don't save residuals]


 **STOP=      Stop processing?                                     [N]

 **MODE=      Which mode? 1-inter, 2-semi, 3-auto                  [1]

 **TOL=       Give tolerance of fit                            [0.001]

 **LAB=       Give mixing parameter of fit                      [0.01]

 **NITER=     Give maximum nr. of iterations of fit               [25]

 **GRDEVICE=  Give plot device                  [list all plotdevices]

 **FRAMECOL=  Give color for frame                   [8 (i.e. orange)]
              For possible colors see below.

 **DATA=      Give line-type, thickness, symbols and color for plot of data
 **GUES=      Give line-type, thickness, symbols and color for plot of guesses
 **MOD=       Give line-type, thickness, symbols and color for plot of model
 **RES=       Give line-type, thickness, symbols and color for plot of residual

              These four keywords define what your plot looks like. They
              should be answered with three numbers <k>,<l>,<m>,<n> where

              <k> - line type; 0-5
                     0 - no line
                     1 - solid line
                     2 - dashed line
                     3 - dot-dash-dot-dash line
                     4 - dotted line
                     5 - dash-dot-dot-dot line
              <l> - line thickness; 0 - 201 dots/line
              <m> - symbol number; 0 - 31
                     0 - no symbol
                     1 - small dot      2 - plus         3 - star
                     4 - circle         5 - cross        6 - square
                     7 - triangle       8 - circle/plus  9 - cricle/dot
                    10 - 4-star        11 - diamond	12 - open star
                    13 - filled trian. 14 - open plus   15 - jew-star
                    16 - filled square 17 - big dot     18 - filled star
                    19 - large square  20-27 various size circles
                    28 - left arrow    29 - right arrow 30 - up arrow
                    31 - down arrow
               <n> - color number; 0 - 15 for GRDEVICE=GIDS
                     0 - background
                     1 - default        2 - red          3 - green
                     4 - blue           5 - cyan         6 - magenta
                     7 - yellow         8 - orange       9 - green + Yellow
                    10 - green + Cyan  11 - blue + Cyan 12 - blue + Magenta
                    13 - red + Magenta 14 - dark Gray   15 - light Gray

               By specifying e.g. MOD=0 0 0 0 the model will not be drawn
               in the plot.



Updates:      Aug 19, 1991: PRR, Document created.
              Nov  9, 1992: PRR, Finishing it off; added colors,
                                 WINDOW=, FIX=, JUDGE=, MNMX on
                                 output sets,  **LIM=.
              Nov 26, 1992: PRR, Voigt function
              Sep 26, 1993: PRR, More digits in screen output
                                 Larger data arrays
                                 Made xxxLIM hidden keyword
                                 Changed use of MODE=
              Dec 20, 1994: KLO  (Olaf K.) Secondary axis problem
                                 solved. Program recognizes permutated
                                 axes for 'COTRANS'
              Mar 10, 1995: PRR  Fixed problem with FIX= keyword:
                                 program now allows FIXing multiple
                                 parameters.
              Apr 17, 1996: VOG  Several minor bugs removed
              Jun 12, 1996: VOG  Initialize local 'err' arrays, because
                                 PROFIT crashed on alpha machines.
#<

*/

                          /*** include files ***/

#include "gipsyc.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "ctype.h"
#include "cmain.h"
#include "math.h"
#include "float.h"

#include "init.h"
#include "finis.h"

#include "anyout.h"
#include "error.h"
#include "status.h"
#include "stabar.h"
#include "cancel.h"
#include "nelc.h"
#include "setfblank.h"
#include "presetr.h"
#include "minmax2.h"
#include "units.h"
#include "factor.h"
#include "lsqfit.h"
#include "gaussn.h"
#include "dgausn.h"
#include "polyn.h"
#include "dpolyn.h"
#include "voigt.h"
#include "w.h"
#include "dvoigt.h"
#include "sortra.h"
#include "deputy.h"
#include "subst.h"

#include "axtype.h"
#include "axcoord.h"
#include "cotrans.h"

#include "gdsc_name.h"
#include "gdsc_ndims.h"
#include "gdsc_grid.h"
#include "gdsc_fill.h"
#include "gdsc_word.h"
#include "gdsc_range.h"
#include "gdsi_read.h"
#include "gdsi_write.h"
#include "gdsd_rchar.h"
#include "gdsinp.h"
#include "gdsbox.h"
#include "gdspos.h"
#include "gdsout.h"
#include "gdsasn.h"
#include "gdscpa.h"
#include "userfio.h"
#include "userlog.h"
#include "userreal.h"
#include "userchar.h"
#include "usercharu.h"
#include "usertext.h"
#include "userint.h"

#include "pgbeg.h"
#include "pgend.h"
#include "pgiden.h"
#include "pgqinf.h"
#include "pgsvp.h"
#include "pgvsiz.h"
#include "pgqvp.h"
#include "pgswin.h"
#include "pgbox.h"
#include "pglab.h"
#include "pgline.h"
#include "pgpt.h"
#include "pgmtxt.h"
#include "pgdraw.h"
#include "pgmove.h"
#include "pgqls.h"
#include "pgqlw.h"
#include "pgslw.h"
#include "pgsls.h"
#include "pgsch.h"
#include "pgqcol.h"
#include "pgqci.h"
#include "pgsci.h"





/* identification */
#define VERSION		"2.1"			/* version number 	*/
#define PROGRAM		"PROFIT"		/* program name 	*/



/*#define TESTING*/				/* Extensive testing output */
						/* set at compile time 	    */





#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.l = len ; }



              /*** miscellaneous definitions ***/
#define true              1
#define false             0
#define MAXTXTLEN       120			/* length of textlines 	*/
#define	NFSTR		  2			/* nr. of strings FIT=	*/
#define	INCHPERCM	( 1.0 / 2.56 )		/* nr. of inches per cm	*/
#define	MAXDIM		10			/* max dims. of set	*/
#define MAXSUBS		4096			/* max. nr. of subsets	*/
#define	MAXDATA		MAXSUBS			/* maximum size arrays	*/
#define BUFSIZE		2048
#define MAXWINDOWS	10			/* max nr. fit windows	*/
#define	MAXPAR		50			/* max nr. of params.	*/
#define NFIXSTR		MAXPAR			/* max nr. of FIX'es	*/
#define	MAXFITS		20 			/* max number of fits	*/

                   /*** global variables (errlev etc.) ***/

#define DATA		1			/* data			*/
#define RESIDUAL	2			/* residulas		*/
#define	MODEL		3			/* the model		*/
#define	GUESS		4			/* the guesses		*/
#define PARAMETERS	5			/* the parameters	*/
#define	ERRORS		6			/* the errors		*/


static fint	zero        = 0 ;
static fint	one         = 1 ;
static fint	two         = 2 ;
static fint	three	    = 3 ;
static fint	four	    = 4 ;

static float	fzero       = 0.0 ;
static float	fhalf       = 0.5 ;
static float	fone        = 1.0 ;

static fint	test        = 16 ;		/* test level anyout	*/

static fint	hasdefault  = 1 ;		/* keyword has default	*/
static fint	hidden      = 2 ;		/* hidden keyword	*/

static fint	e_warning   = 2 ;		/* warning for error	*/
static fint	e_fatal     = 4 ;		/* fatal error		*/

static fint	thinlines   = 1 ;		/* thin lines		*/
static fint	thicklines  = 5 ;		/* thick lines		*/
static fint	full_line   = 1 ;
static fint	dash_line   = 2 ;
static fint	dot_dash    = 3 ;
static fint	dotted_line = 4 ;
static fint	dash_dot_dot= 5 ;

static fint	no_symbol   = 0 ;
static fint	plus_symbol = 2 ;
static fint	star_symbol = 3 ;
static fint	cross_symbol= 5 ;
static fint	dot_symbol  = 17 ;
static fint	back_ground = 0 ;

static fint	fore_ground = 1 ;
static fint	red         = 2 ;
static fint	green       = 3 ;
static fint	blue        = 4 ;
static fint	yellow      = 7 ;
static fint	orange      = 8 ;
static fint	blue_cyan   = 11 ;
static fint	blue_magenta = 12 ;
static fint	red_magenta = 13 ;

static float	big_char    = 1.5 ;
static float	med_char    = 1.2 ;
static float	small_char  = 1.0 ;

static double	dblzero     = 0.0 ;
static double	dblone      = 1.0 ;

static char     stat_line[ MAXTXTLEN ] ;
                      /*** struct definitions ***/

/*
   The RunMode struct contains all information about the which PROFIT is
supposed to run; run/program moude, automatic/interactive mode etc. etc.

*/
#define	MODE_INIT	{ false , 1 , true ,  false , false }
typedef struct {
   bool		run ;				/* run/program mode	*/
   fint		level ;				/* automatic/interact	*/
   bool		display ;			/* use display ?	*/
   bool		next ;				/* next profile ?	*/
   bool		stop ;				/* stop now ?		*/
} RunMode ;					/* RunMode struct	*/

/*
   The Set struct contains information regarding the input dataset.
Thus it has the definition of which profiles should be analyzed.  Also
it contains the knowledge of whether for each new profile the
x-coordinates must be recalculated.

*/
typedef struct {
   fchar	set ;				/* input set name	*/
   fint		setdim ;			/* dimensions of set	*/
   fint		ss[ MAXSUBS ] ;			/* subset array		*/
   fint		nss ;				/* nr. of subsets 	*/
   fint		axperm[ MAXDIM ] ;		/* axis permutations	*/
   fint		blo[ MAXDIM ] ;			/* lower left of box	*/
   fint		bhi[ MAXDIM ] ;			/* upper right of box	*/
   fint		inc[ MAXDIM ] ;			/* box increment array	*/
   bool		recalc_x ;			/* recalculate y-coord?	*/
} Set ;						/* Set struct		*/

/*
   SetInfo contains information about the set relevant for plotting;
axis names, object name etc.  etc.

*/
typedef struct {
   fint		xtype ;				/* type of x-axis	*/
   fchar	xunit ;				/* units of x-axis	*/
   fchar	xdunit ;			/* secondary x units 	*/
   fint		xskysys ;			/* skysystem x-coord.	*/
   fint		xprosys ;			/* proj.system x-coord.	*/
   fint		xvelsys ;			/* vel. system x-coord.	*/
   fint		ytype ;				/* type of y-axis	*/
   fchar	yunit ;				/* units of y-axis	*/
   fchar	object ;			/* name of object 	*/
   fchar	instrument ;			/* name of obs. instr.	*/
   fchar	axname[ MAXDIM ] ;		/* names of the axes	*/
   fchar	unit[ MAXDIM ] ;		/* units of the axes	*/
   fint         colev[ MAXDIM ] ;               /* secondary axis ???   */
   double	factor ;			/* conversion factor	*/
} SetInfo ;					/* SetInfo struct	*/


/*
   OutSets contains all knowledge about all output sets

*/
typedef struct {
    bool	asked ;				/* were outsets asked?	*/
    Set		par ;				/* parameter set	*/
    Set		err ;				/* error set		*/
    Set		mod ;				/* model set		*/
    Set		res ;				/* residual set		*/
} OutSets ;					/* OutSets struct	*/

/*
   The MaskSet struct contains infromation regarding the set used as a
mask to determine which parts of the input set are to be used.

*/
typedef struct {
   bool		asked ;				/* was maskset asked	*/
   fchar	set ;				/* input set name	*/
   fint		setdim ;			/* dimensions of set	*/
   fint		ss ;				/* subset array		*/
   fint		reppos[ MAXDIM ] ;		/* previous report pos.	*/
   float	cut ;				/* cutoff in mask set	*/
} MaskSet ;					/* MaskSet struct	*/


/*
   The PlotDevice type contains information about the available plot
devices. The PlotSetup struct has switches to determine what the plots
will look like.

*/
#define	PLOT_DEVICE	{ NULL , 0 , false }
typedef struct {
   fchar	name ;
   bool		open ;
} PlotDevice ;				/* PlotDevice types 	*/
#define PLOT_XMARGIN	 3.0
#define PLOT_YMARGIN	 3.0
#define PLOT_XSIZE	15.0
#define PLOT_YSIZE	20.0
#define	PLOT_SETUP	{ PLOT_DEVICE  , true , true  , \
                          PLOT_XMARGIN , PLOT_YMARGIN , \
                          PLOT_XSIZE   , PLOT_YSIZE   , \
                          0.0  ,  0.0  ,  0.0 ,  0.0  , \
                          0.0  ,  0.0  }
typedef struct {
   PlotDevice	device ;			/* plot device		*/
   bool		grids ;				/* plot grids ?		*/
   bool		zero ;				/* plot zero level	*/
   float	xmarg ;				/* margin in x		*/
   float	ymarg ;				/* margin in y		*/
   float	xsize ;				/* size of frame in x	*/
   float	ysize ;				/* size of frame in y	*/
   float	xmin ;				/* lowest x - data	*/
   float	xmax ;				/* highest x - data	*/
   float	ymin ;				/* lowest y - data	*/
   float	ymax ;				/* highest y - data	*/
   float	xsc ;				/* scale in x units/cm	*/
   float	ysc ;				/* scale in y units/cm	*/
} PlotSetup ;					/* PlotSetup struct 	*/


/*
   The Profile struct contains the actual data for a given profile. Also
it has a vector identifying the start position.

*/
typedef struct {
   fint		pos[ MAXDIM ] ;			/* position vector	*/
   float	x[ MAXDATA ] ;			/* x-data (e.g. vel)	*/
   float	y[ MAXDATA ] ;			/* y-data (e.g. flux)	*/
   float	min ;				/* minimum data value	*/
   float	max ;				/* maximum data value	*/
   fint		sym ;				/* symbol plotting	*/
   fint		lwt ;				/* line thickness	*/
   fint		ltyp ;				/* line style		*/
   fint		col ;				/* color used for plot	*/
   fint		n ;				/* nr. of data points	*/
} Profile ;					/* Profile struct	*/

/*
   The F_type is is struct for the definition of functions

*/
typedef struct {
   fint  type ;					/* type number		*/
   fint  npar ; 				/* nr of parameters	*/
   fint  zero ;					/* zero level ?		*/
   fint  nret ;					/* nr returned		*/
   char *name ; 				/* and the name		*/
   char *mes  ;					/* message for FIT=	*/
} F_type ;					/* Function def. struct	*/
#define		NOFUNC	 0 			/* no function		*/
#define 	GAUSS	 1			/* gaussian		*/
#define		POLY	 2			/* polynomial		*/
#define		VOIGT	 3			/* voigt		*/
static F_type FUN[] = {				/* implemented funcs.	*/
   { NOFUNC , 0 , 0 , 1 , "NONE"  , "nothing" } ,		/* dummy*/
   { GAUSS  , 3 , 1 , 2 , "GAUSS" , "Amp., centre, FWHM" } ,	/* gauss*/
   { POLY   , 1 , 0 , 2 , "POLY"  , " " } ,			/* poly	*/
   { VOIGT  , 4 , 1 , 2 , "VOIGT" , "Amp., centre, FWHM, LOR" } } ;
#define MAXFUNCS ( sizeof( FUN )/sizeof( F_type ) )

/*
   The Fix_F_type contains the names of the parameters for each function
*/
typedef struct {
   char *name[ MAXPAR ] ;			/* parameter names	*/
} Fix_F_type ;					/* parameter name struct*/
static Fix_F_type FIX_FUN[ MAXFUNCS ] = {	/* implemented names	*/
   { " "    } ,					/* for NOFUNC		*/
   { "AMP"  , "CENTER" , "FWHM" , " "   } ,	/* for GAUSS		*/
   { " "    } ,					/* for POLY		*/
   { "AMP"  , "CENTER" , "FWHM" , "LOR" } } ;	/* for VOIGT		*/


/*
   FitFunc contains all data relevant for a certain fit; its
type, nr. of components,nr. of parameters, the fitparameters and errors
and a flag telling whether this fit is good or bad.

*/
typedef struct {
   bool		fitok ;				/* good/bad fit		*/
   fint		niter ;				/* nr. of iterations	*/
   float	par[ MAXPAR ] ;			/* fit parameters	*/
   float	err[ MAXPAR ] ;			/* fit errors		*/
   float	rms ;				/* rms. of residual	*/
   float	mean ;				/* mean of residual	*/
} FitFunc ;					/* FitFunction struct	*/

/*
   FitConstrain contains data to constrain a given fit; initial guesses,
limits between which it should lie and parameters which should be fixed.

*/
typedef struct {
   fint		type ;				/* function type	*/
   fint		ncmp ;				/* nr. of components	*/
   fint		npar ;				/* tot. nr. of pars.	*/
   float	sn ;				/* S/N in this fit	*/
   float	wts[ MAXDATA ] ;		/* weights for fit	*/
   float	guess[ MAXPAR ] ;		/* fit guesses		*/
   float	min[ MAXPAR ] ;			/* minimum for a parm.	*/
   float	max[ MAXPAR ] ;			/* maximum for a parm.	*/
   bool		msk[ MAXPAR ] ;			/* mask parm. in fit	*/
} FitConstrain ;				/* FitConstrain struct	*/

/*
   The FitProgram contains info to create a fitting program; a series of
consecutive fits and opcodes to determine what to do after success and
failure.

*/
typedef enum {
   NOOP ,					/* do nothing		*/
   UNDEF ,					/* set to undefine	*/
   NEXT ,					/* goto next fit	*/
   AGAIN ,					/* fit profile again	*/
   MORE ,					/* do more fits		*/
   SUBTRACT					/* subtract fit		*/
} OpCodes ;
typedef struct {
   FitConstrain	fc[ MAXFITS ] ;			/* array of fit types	*/
   fint		nf ;				/* number of fits	*/
   OpCodes	opcode[ MAXFITS ] ;		/* what with result	*/
} FitProgram ;					/* a FitProgram struct	*/

/*
   The ProfileFit struct contains the results of a series of fits for a
given profile. It contains the profile itself and the fit results of
each step. Also it has counters to keep track of what is left and what
is done.

*/
typedef struct {
   Profile	prof ;				/* the data		*/
   Profile	mod ;				/* the model		*/
   Profile	res ;				/* the residual		*/
   FitFunc	fits[ MAXFITS ] ;		/* the fits 		*/
   fint		curr ;				/* current fit		*/
   bool		done ;				/* done with profile ?	*/
} ProfileFit ;					/* ProfileFit struct	*/


                    /*** Function definitions ***/
/*
   The function Send_Status updates the status message

*/
static void Send_Status( char *string , bool add )
{
   static char	stat_mess[ MAXTXTLEN ] ;	/* the status message	*/

   if ( add ) {
      strcat( stat_mess , string ) ;
   } else {
      sprintf( stat_mess , "%s" , string ) ;
   }
   status_c( tofchar( stat_mess ) ) ;
   anyout_c( &test , tofchar( stat_mess ) ) ;

   return ;
}						/* Send_Status		*/

/*
   ListProgram is a utility to list program properties
*/
static void ListProgram( FitProgram *fitprog ,
                         ProfileFit *pro_fit ,
                         char *origin )
{
   char		mess[ MAXTXTLEN ] ;		/* message buffer	*/

   fint		n ;				/* counter		*/

   sprintf( mess , "ListProgram called from %s" , origin ) ;
   anyout_c( &test , tofchar( mess ) ) ;
   sprintf( mess , "  currently at fit nr. %d of %d (done = %d);" ,
                    pro_fit->curr + 1 , fitprog->nf , pro_fit->done ) ;
   anyout_c( &test , tofchar( mess ) ) ;
   sprintf( mess , "    result of %s-fit is %d after %d iterations" ,
                    FUN[ fitprog->fc[ pro_fit->curr ].type ].name ,
                    pro_fit->fits[ pro_fit->curr ].fitok ,
                    pro_fit->fits[ pro_fit->curr ].niter ) ;
   anyout_c( &test , tofchar( mess ) ) ;

   for ( n = 0 ; n < fitprog->nf ; n++ ) {
      sprintf( mess , "  fit %d is %d cmp. %s-fit (%d + %d * %d = %d pars.), opcode %d" ,
                       n + 1 , fitprog->fc[ n ].ncmp ,
                       FUN[ fitprog->fc[ n ].type ].name ,
                       FUN[ fitprog->fc[ n ].type ].zero ,
                       fitprog->fc[ n ].ncmp ,
                       FUN[ fitprog->fc[ n ].type ].npar ,
                       fitprog->fc[ n ].npar ,
                       fitprog->opcode[ n ] ) ;
      anyout_c( &test , tofchar( mess ) ) ;
   }

   anyout_c( &test , tofchar( "End of ListProgram" ) ) ;

   return ;
}						/* ListProgram		*/

/*
   Amp2Int converts GAUSS/VOIGT amplitudes to integrals
*/
static void Amp2Int( fint   type ,
                     float *pars ,
                     float *errs )
{
   float	amp , damp ;			/* orig. amp. en error	*/

   complex	z , dw ;			/* vars for eval. of z()*/

   anyout_c( &test , tofchar( "Function Amp2Int" ) ) ;

   switch( type ) {
      case GAUSS :
         amp  = pars[ 0 ] ;
         damp = errs[ 0 ] ;
         pars[ 0 ] = pars[ 0 ] * pars[ 2 ] / 0.9394373 ;
         if ( ( amp != 0.0 ) && ( pars[ 2 ] != 0.0 ) ) {
            errs[ 0 ] = pars[ 0 ] * sqrt(
               ( damp      * damp      ) / ( amp       * amp       ) +
               ( errs[ 2 ] * errs[ 2 ] ) / ( pars[ 2 ] * pars[ 2 ] ) ) ;
         }
         break ;
      case VOIGT :
         if ( pars[ 2 ] != 0.0 ) {
            amp  = pars[ 0 ] ;
            damp = errs[ 0 ] ;
            z.r = 0 ;
            z.i = 0.83255461 * pars[ 3 ] / pars[ 2 ] ;
            dw = w_c( &z ) ;
            pars[ 0 ] = pars[ 0 ] * pars[ 2 ] / ( dw.r * 0.93943728 ) ;
            if ( amp != 0.0 ) {
               errs[ 0 ] = pars[ 0 ] * sqrt(
                  ( damp      * damp      ) / ( amp       * amp       ) +
                  ( errs[ 2 ] * errs[ 2 ] ) / ( pars[ 2 ] * pars[ 2 ] ) ) ;
            }
         }
         break ;
      default :
         break;
   }
   return ;
}						/* Amp2Int		*/


/*
   Int2Amp converts GAUSS/VOIGT integrals to amplitudes
*/
static void Int2Amp( fint   type ,
                     float *pars ,
                     float *errs )
{
   float	itg , ditg ;			/* orig. int. en error	*/

   complex	z , dw ;			/* vars for eval. of z()*/

   anyout_c( &test , tofchar( "Function Int2Amp" ) ) ;

   switch( type ) {
      case GAUSS :
         itg  = pars[ 0 ] ;
         ditg = errs[ 0 ] ;
         if ( pars[ 2 ] != 0 ) {
            pars[ 0 ] = 0.9394373 * pars[ 0 ] / pars[ 2 ] ;
            if ( itg != 0 ) {
               errs[ 0 ] = pars[ 0 ] * sqrt(
                  ( ditg      * ditg      ) / ( itg       * itg       ) +
                  ( errs[ 2 ] * errs[ 2 ] ) / ( pars[ 2 ] * pars[ 2 ] ) ) ;
            }
         }
         break ;
      case VOIGT :
         if ( pars[ 2 ] != 0 ) {
            itg  = pars[ 0 ] ;
            ditg = errs[ 0 ] ;
            z.r = 0 ;
            z.i = 0.83255461 * pars[ 3 ] / pars[ 2 ] ;
            dw = w_c( &z ) ;
            pars[ 0 ] = ( dw.r * 0.93943728 ) * pars[ 0 ] / pars[ 2 ] ;
            if ( itg != 0 ) {
               errs[ 0 ] = pars[ 0 ] * sqrt(
                  ( ditg      * ditg      ) / ( itg       * itg       ) +
                  ( errs[ 2 ] * errs[ 2 ] ) / ( pars[ 2 ] * pars[ 2 ] ) ) ;
            }
         }
         break ;
      default :
         break;
   }

   return ;
}						/* Int2Amp		*/

/*
   The function GetMode obtains a running mode from the user by asking
the relevant keywords.

*/
static void GetMode( RunMode *mode )		/* ask running mode	*/
{
   fint		nret       = 0 ;		/* user** return	*/

   anyout_c( &test , tofchar( "Function GetMode" ) ) ;

   nret = userlog_c( &mode->stop , 		/* ask if user 		*/
                     &one , &hidden ,		/*     wants to stop	*/
                      tofchar( "STOP=" ) ,
                      tofchar( "Stop the program ? [N]" ) ) ;

   nret = userlog_c( &mode->next , 		/* ask if user 		*/
                     &one , &hidden ,		/*   wants next profile	*/
                      tofchar( "NEXT=" ) ,
                      tofchar( "Next profile ? [N]" ) ) ;

   nret = userint_c( &mode->level , 		/* ask what run-mode	*/
                     &one , &hasdefault ,	/*     the user wants	*/
                      tofchar( "MODE=" ) ,
                      tofchar( "Which mode ? 1-inter, 2-semi, 3-auto [ 1 ]" ) ) ;

   mode->run = ( mode->level != 1 ) ;		/* no interact-> run	*/

   return ;
}						/* GetMode		*/


/*
   The function GetInputSet asks the user for a set/subset and area
specification.

*/
static bool GetInputSet( Set *inset )		/* ask set/area specs.	*/
{
   fint		maxubs     = MAXSUBS ;		/* max. nr. of subsets	*/
   fint		showdev    = 3 ;		/* terminal + logfile	*/
   fint 	maxaxes    = MAXDIM ;		/* maximum nr. of axes	*/
   fint		axgrids[ MAXDIM ] ;		/* nr pix along axes	*/
   fint		classdim = 0 ;			/* class dimensionality	*/
   fint		ssdim = 0 ;			/* subset dimensions	*/

   bool		gotit = true ;			/* got good input ?	*/

   anyout_c( &test , tofchar( "Function GetInputSet" ) ) ;

   finit( inset->set          , 128 ) ;		/* initialise fchars	*/

   do {						/* loop to get good set	*/
      gotit = true ;				/* assume it works 	*/
      classdim = 0 ;				/* a don't care		*/
      inset->nss =
             gdsinp_c( inset->set ,		/* set name		*/
                       inset->ss ,		/* subset numbers	*/
                       &maxubs ,		/* max nr. subsets	*/
                       &hasdefault ,		/* default is stop	*/
                       tofchar( "INSET=" ) ,	/* keyword		*/
                       tofchar( "Give input set and subsets [ quit ]" ) ,
                       &showdev ,		/* output level		*/
                       inset->axperm ,		/* permutation of axes	*/
                       axgrids ,		/* nr. of pixels on ax	*/
                       &maxaxes , 		/* maximum nr. of axes	*/
                       &one ,			/* class one		*/
                       &classdim ) ;

      if ( inset->nss == 0 ) return( true ) ;	/* CR? => exit		*/

      if ( inset->nss == 1 ) {			/* bad nr of subsets	*/
         error_c( &e_warning , 			/* issue warning	*/
                  tofchar( "You MUST specify more than one subset" ) ) ;
         cancel_c( tofchar("INSET=") ) ;	/* cancel the keyword	*/
         gotit = false ;
         continue ;				/* try again		*/
      }

      inset->setdim = gdsc_ndims_c( inset->set , &zero ) ; /* set dims.	*/
      ssdim         = gdsc_ndims_c( inset->set ,
                                    &inset->ss[ 0 ] ) ; /* subset dims.	*/

      if ( inset->setdim != ( ssdim + 1 ) ) {	/* bad subsets		*/
         error_c( &e_warning , 			/* issue warning	*/
                  tofchar( "Subsets MUST be on SAME axis" ) ) ;
         cancel_c( tofchar("INSET=") ) ;	/* cancel the keyword	*/
         gotit = false ;
         continue ;				/* try again		*/
      }

      gdsbox_c(  inset->blo , 			/* lower left corner	*/
                 inset->bhi ,	 		/* upper right corner	*/
                 inset->set , 			/* set name		*/
                &inset->ss[ 0 ] ,		/* subset		*/
                &hasdefault ,			/* default is entire ss	*/
                 tofchar( "" ) ,		/* default key word	*/
                 tofchar( "" ) ,		/* default message	*/
                &showdev ,			/* output level		*/
                &zero ) ;			/* default: all		*/

   } while( !gotit ) ;

   inset->recalc_x = true ;			/* recalculate x-axis	*/

   return( false ) ;
}						/* GetInputSet		*/


/*
   The function GetSetInfo obtains infromation from the input set required
for e.g. plot annotation etc.

*/
static void GetSetInfo( Set      *inset ,
                        SetInfo  *setinfo )	/* get set information	*/
{
   char		key[ MAXTXTLEN ] ;		/* FITS keyword		*/
   char		mess[ MAXTXTLEN ] ;		/* message string	*/

   fchar	xname ;

   fint		error      = 0 ;		/* error codes		*/
   fint		n = 0 , m = 0 ;			/* counter		*/

   anyout_c( &test , tofchar( "Function GetSetInfo" ) ) ;

   finit( setinfo->xunit      ,  80 ) ;		/* initialise fchars	*/
   finit( setinfo->xdunit     ,  80 ) ;
   finit( setinfo->yunit      ,  80 ) ;
   finit( setinfo->object     ,  80 ) ;
   finit( setinfo->instrument ,  80 ) ;
   finit( xname               ,  80 ) ;
   for( n = 0 ; n < MAXDIM ; n++ ) {
      finit( setinfo->axname[ n ] , 80 ) ;
      finit( setinfo->unit[ n ]   , 80 ) ;
   }


   error = 0 ;
   gdsd_rchar_c( inset->set ,			/* read from set	*/
                 tofchar( "BUNIT" ) ,		/* units keyword	*/
                 &zero ,			/* on toplevel		*/
                 setinfo->yunit ,		/* into y-units		*/
                 &error ) ;			/* error return code	*/
   setinfo->ytype = units_c( setinfo->yunit ) ;
   error = 0 ;
   gdsd_rchar_c( inset->set ,			/* read from set	*/
                 tofchar( "OBJECT" ) ,		/* object keyword	*/
                 &zero ,			/* on toplevel		*/
                 setinfo->object ,		/* into object		*/
                 &error ) ;			/* error return code	*/
   error = 0 ;
   gdsd_rchar_c( inset->set ,			/* read from set	*/
                 tofchar( "INSTRUME" ) ,	/* instrument keyword	*/
                 &zero ,			/* on toplevel		*/
                 setinfo->instrument ,		/* into instrument	*/
                 &error ) ;			/* error return code	*/

   for( n = 0 ; n < inset->setdim ; n++ ) {
      error = axcoord_c( inset->set ,		/* read from set	*/
                         &inset->axperm[ n ] ,	/* for n-th axis 	*/
                         setinfo->axname[ n ] ,	/* name of axis 	*/
                         setinfo->unit[ n ] ,	/* units of axis 	*/
		         &setinfo->colev[ n ] ) ;/* secondary units?    */



      sprintf( mess , " Read axis %d: %.*s in %.*s (error %d)" , n ,
               nelc_c( setinfo->axname[ n ] ) , setinfo->axname[ n ].a ,
               nelc_c( setinfo->unit[ n ] ) , setinfo->unit[ n ].a , error ) ;
      anyout_c( &test , tofchar( mess ) ) ;
   }










   error = 0 ;

  error = axcoord_c( inset->set ,           /* read from set        */
              &inset->axperm[ inset->setdim - 1 ] ,  /* for n-th axis */
              xname,         /* name of axis         */
              setinfo->xunit,        /* units of axis        */
              &setinfo->colev [inset->setdim - 1 ]  ) ;/* secondary units ? */

		anyoutf( test , "xname is %.*s xunit= %.*s",
		nelc_c(xname),xname.a,nelc_c(setinfo->xunit),setinfo->xunit.a);

	setinfo->xtype = 0;


   if (setinfo->colev [inset->setdim - 1 ] == 1)
   {

   setinfo->xtype =				/* get axis type	*/
          axtype_c( xname ,			/* for xname		*/
                    setinfo->xunit ,		/* get units		*/
                    setinfo->xdunit ,		/* and secondary units	*/
                    &setinfo->xskysys ,		/* skysystem		*/
                    &setinfo->xprosys ,		/* projection system	*/
                    &setinfo->xvelsys ) ;	/* velocity system	*/
        }
	else
	{
	setinfo->xtype = -units_c( setinfo->xunit );
	}



   anyoutf( test," xtype =%d", setinfo->xtype);




   if ( setinfo->xtype == 0 ) {			/* unknown axis type	*/
      error = 0 ;
      sprintf( key , "CUNIT%d" , inset->axperm[ inset->setdim - 1 ] ) ;
      gdsd_rchar_c( inset->set ,		/* read from set	*/
                    tofchar( key ) ,		/* units keyword	*/
                    &zero ,			/* on toplevel		*/
                    setinfo->xunit ,		/* into x-units		*/
                    &error ) ;			/* error return code	*/
      error = 0 ;
      sprintf( key , "DUNIT%d" , inset->axperm[ inset->setdim - 1 ] ) ;
      gdsd_rchar_c( inset->set ,		/* read from set	*/
                    tofchar( key ) ,		/* dunits keyword	*/
                    &zero ,			/* on toplevel		*/
                    setinfo->xdunit ,		/* into x-dunits	*/
                    &error ) ;			/* error return code	*/
      setinfo->xtype = -units_c( setinfo->xunit ) ;
   }

   anyoutf( test," xtype (=%d) is going to be evaluated", setinfo->xtype);

   switch( setinfo->xtype ) {
      case -1:					/* units is degrees	*/
      case  1:
      case  2: setinfo->factor = 3600 ;		/* change to arcsec	*/
               strcpy( setinfo->xunit.a , "arcsec" ) ;
               break ;
      case -4:					/* units is frequency	*/
      case  3: if ( factor_c( setinfo->xunit ,
                              tofchar( "MHZ" ) ,
                             &setinfo->factor ) == 0 ) {
                  strcpy( setinfo->xunit.a , "MHz" ) ;
               } else {
                  setinfo->factor = 1.0 ;
               }
               break ;
      case -5:					/* units is velocity	*/
      case  4: if ( factor_c( setinfo->xunit ,
                              tofchar( "KM/S" ) ,
                             &setinfo->factor ) == 0 ) {
                  strcpy( setinfo->xunit.a , "Km/s" ) ;
               } else {
                  setinfo->factor = 1.0 ;
               }
               break ;
      default: setinfo->factor = 1.0 ;		/* no conversion	*/
               break ;
   }



   return ;
}						/* GetSetInfo		*/


/*
   The function FirstProfile sets the profile position for the first
profile to be analyzed.

*/
static void FirstProfile( Set        *inset   ,
                          SetInfo    *setinfo ,
                          ProfileFit *pro_fit ,
                          FitProgram *fitprog )	/* get a profile pos.	*/
{
   char		mess[ MAXTXTLEN ] ;		/* message string	*/
   char		txtbuf[ MAXTXTLEN ] ;		/* message string	*/

   fint		n = 0 , m = 0 ;			/* counters		*/
   fint		nret = 0 ;			/* gdspos return	*/

   double	pos[ MAXDIM ] ;			/* position array	*/

   anyout_c( &test , tofchar( "Function FirstProfile" ) ) ;

   for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
						/* set increment array	*/
      if ( inset->bhi[ n ] != inset->blo[ n ] ) {
         inset->inc[ n ]   = ( inset->bhi[ n ] - inset->blo[ n ] ) /
                             abs( inset->bhi[ n ] - inset->blo[ n ] ) ;
      } else {
         inset->inc[ n ] = 0 ;
      }
						/* set default pos.	*/
      pro_fit->prof.pos[ n ] = ( inset->blo[ n ] +inset->bhi[ n ] ) / 2 ;
      pos[ n ]          = (double) pro_fit->prof.pos[ n ] ;
   }
   inset->inc[ inset->setdim - 1 ] = 0 ;

   sprintf( mess , "Give position for profile in (" ) ;
   for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
      m = nelc_c( setinfo->axname[ n ] ) > 3 ?
            3 : nelc_c( setinfo->axname[ n ] ) ;
      sprintf( txtbuf , "%3.*s" , m , setinfo->axname[ n ].a ) ;
      strcat( mess , txtbuf ) ;
      if ( n < inset->setdim - 2 ) strcat( mess , "," ) ;
   }
   strcat( mess , ") [ (" ) ;
   for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
      sprintf( txtbuf , "%d" , pro_fit->prof.pos[ n ] ) ;
      strcat( mess , txtbuf ) ;
      if ( n < inset->setdim - 2 ) strcat( mess , "," ) ;
   }
   strcat( mess , ") ]" ) ;

   nret = gdspos_c(  pos ,			/* get position		*/
                    &one ,			/* only one		*/
                    &hasdefault ,		/* we have a default	*/
		     tofchar( "POS=" ) ,	/* keyword		*/
		     tofchar( mess ) ,		/* message		*/
                     inset->set ,		/* inset name		*/
                    &inset->ss[ 0 ] ) ;		/* first subset		*/
   cancel_c( tofchar( "POS=" ) ) ;		/* cancel the key	*/

   if ( nret == 1 ) {				/* user gave position	*/
      for ( n = 0 ; n < inset->setdim ; n++ ) {
         pro_fit->prof.pos[ n ] = (int) pos[ n ] ;	/* put pos. 	*/
      }
   }

   for ( n = 0 ; n < MAXFITS ; n++ ){
      fitprog->fc[ n ].type =  NOFUNC ;
      fitprog->fc[ n ].sn =  3 ;
      for ( m = 0 ; m < MAXPAR ; m++ ) {
         fitprog->fc[ n ].min[ m ] = -FLT_MAX / 10  ;
         fitprog->fc[ n ].max[ m ] =  FLT_MAX / 10 ;
         fitprog->fc[ n ].msk[ m ] =  1 ;
      }
   }
   pro_fit->curr = 0 ;				/* first of fit series	*/
   fitprog->nf   = 1 ;				/* series has 0 fits	*/

   return ;
}						/* FirstProfile		*/

/*
   The function GetProfile reads data from the input set.

*/
static void GetProfile( Set        *inset ,
                        SetInfo    *setinfo ,
                        ProfileFit *pro_fit ) 	/* read a profile	*/
{
   char		line[ MAXTXTLEN ] ;		/* text line		*/
   char		buff[ MAXTXTLEN ] ;		/* text line		*/

   fint		cwlo = 0 ;			/* read begin c-word	*/
   fint		cwhi = 0 ;			/* read end c-word	*/
   fint		tid ;				/* transfer id		*/
   fint		maxdata ;			/* max. nr. of points	*/
   fint		gridlo = 0 ;			/* first grid coord.	*/
   fint		gridhi = 0 ;			/* last grid coord.	*/
   fint		n , m , j = 0 ;			/* counter		*/
   fint 	level = 0 ;			/* subset level		*/
   fint 	error = 0 ;			/* error level		*/
   fint		nblank = 0 ;			/* nr. of blanks	*/
   fint 	lastindx ;			/* indexnumber		*/
   float	blank = 0 ;			/* blank value		*/
   float	tmp_y = 0 ;

   double	grid[ MAXDIM ] ;		/* grid coordinates	*/
   double	pos[ MAXDIM ] ;			/* cotrans position	*/

   anyout_c( &test , tofchar( "Function GetProfile" ) ) ;

   sprintf( stat_line , "Set %.*s at (" ,
           nelc_c( inset->set ) , inset->set.a ) ;
   for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
      m = nelc_c( setinfo->axname[ n ] ) > 3 ?
                   3 : nelc_c( setinfo->axname[ n ] ) ;
      sprintf( buff , "%.*s\0" , m , setinfo->axname[ n ].a ) ;
      strcat( stat_line , buff ) ;
      if ( n < inset->setdim - 2 ) strcat( stat_line , "," ) ;
   }
   strcat( stat_line , ")=(" ) ;
   for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
      sprintf( buff , "%d\0" , pro_fit->prof.pos[ n ] ) ;
      strcat( stat_line , buff ) ;
      if ( n < inset->setdim - 2 ) strcat( stat_line , "," ) ;
   }
   strcat( stat_line , ")" ) ;
   Send_Status( stat_line , false ) ;
   anyout_c( &test , tofchar( line ) ) ;

   cwlo = gdsc_fill_c(  inset->set, 		/* get cwlo from set	*/
                       &inset->ss[ 0 ] ,	/* for start subset	*/
                        pro_fit->prof.pos ) ;	/* at the profile pos.	*/
   cwhi = gdsc_fill_c(  inset->set, 		/* get cwhi from set	*/
                       &inset->ss[ inset->nss - 1 ] ,	/* end subset	*/
                        pro_fit->prof.pos ) ;	/* at the profile pos.	*/

   tid     = 0 ;				/* set tid		*/
   maxdata = MAXDATA ;				/* read maximum number	*/
   sprintf( line ,
            "Will try to read from set %.*s; cw %d to %d (max %d points)" ,
           nelc_c( inset->set ) , inset->set.a  , cwlo , cwhi , maxdata ) ;
   anyout_c( &test , tofchar( line ) ) ;
   gdsi_read_c( inset->set       ,		/* read from inset	*/
                &cwlo            ,		/* from start cw	*/
                &cwhi            ,		/* to end cw		*/
                pro_fit->prof.y  ,		/* into data array	*/
                &maxdata         ,		/* max nr. points	*/
                &pro_fit->prof.n ,		/* return whats read	*/
                &tid             ) ;		/* get transfer id	*/

   sprintf( line , "Read %d points from set %.*s (%d)" , pro_fit->prof.n ,
                    nelc_c( inset->set ) , inset->set.a  , tid ) ;
   anyout_c( &test , tofchar( line ) ) ;

   if ( tid < 0 ) {
      sprintf( line , "GetProfile read error %d" , tid ) ;
      error_c( &e_fatal , tofchar( line ) ) ;
   }

   pro_fit->prof.max = pro_fit->prof.y[ 0 ] ;
   pro_fit->prof.min = pro_fit->prof.y[ 0 ] ;


   minmax2_c( pro_fit->prof.y    ,		/* for profile of	*/
              &pro_fit->prof.n   ,		/*   prof.n points get	*/
              &pro_fit->prof.min ,		/*   min and 		*/
              &pro_fit->prof.max ,		/*   max and		*/
              &nblank            ) ;		/* nr. of blanks	*/

   sprintf( line , "Read %d points, min %f, max %f, %d blanks" ,
                    pro_fit->prof.n   , pro_fit->prof.min ,
                    pro_fit->prof.max , nblank ) ;
   anyout_c( &test , tofchar( line ) ) ;

   gridlo = gdsc_grid_c( inset->set ,		/* get gridlo of prof.	*/
                         &inset->axperm[ inset->setdim - 1 ] ,
                         &inset->ss[ 0 ] ,
                         &error  ) ;
   gridhi = gdsc_grid_c( inset->set ,		/* get gridhi of prof.	*/
                         &inset->axperm[ inset->setdim - 1 ] ,
                         &inset->ss[ inset->nss - 1 ] ,
                         &error  ) ;
   if ( gridlo > gridhi ) {			/* re-order data	*/
      for ( n = 0 ; n < ( pro_fit->prof.n / 2 ) ; n++ ) { /* on grids	*/
         tmp_y = pro_fit->prof.y[ n ] ;
         pro_fit->prof.y[ n ] = pro_fit->prof.y[ pro_fit->prof.n - n - 1 ] ;
         pro_fit->prof.y[ pro_fit->prof.n - n - 1 ] = tmp_y ;
      }
   }

   if ( inset->recalc_x ) {			/* recalculate x-coord?	*/
      anyout_c( &test , tofchar( "Recalculating x-coordinate" ) ) ;

      error = 0 ;				/* reset error		*/
      for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
         grid[ inset->axperm[n]-1 ] = (double) pro_fit->prof.pos[ n ] ;
      }



	lastindx = inset->axperm[ inset->setdim - 1 ] - 1;
      sprintf( line , "Profile runs from pixel %d to %d on axis %d" ,
               gridlo , gridhi , lastindx ) ;

      anyout_c( &test , tofchar( line ) ) ;
      level = 0 ;
      for ( n = 0 ; n < pro_fit->prof.n ; n++ ) { /* loop on grids	*/
         if ( gridlo < gridhi ) {
            grid[ lastindx ]  = gridlo + n ;
         } else {
            grid[ lastindx ]  = gridlo - n ;
         }
         error = cotrans_c(  inset->set ,
                            &level ,
                             grid ,
                             pos ,
                            &inset->setdim ) ;
         pro_fit->prof.x[ n ] = (float) ( setinfo->factor *
                                          pos[ lastindx ] ) ;

#ifdef TESTING
         for (j = 0; j < inset->setdim; j++)
         {
         	sprintf( line, "grid[%d]=%f pos[%d]=%f", j, grid[j],j, pos[j] ) ;
         	anyout_c( &test , tofchar( line ) ) ;
        }
#endif


      }
      sprintf( line , "Profile runs from %f to %f (%.*s)" ,
               pro_fit->prof.x[ 0 ] ,
               pro_fit->prof.x[ pro_fit->prof.n - 1 ] ,
               nelc_c( setinfo->unit[ inset->setdim - 1 ] ) ,
               setinfo->unit[ inset->setdim - 1 ].a ) ;
      anyout_c( &test , tofchar( line ) ) ;
   }

   if ( nblank > 0 ) {				/* blanks!		*/
      sprintf( line , "%d blanks in profile, will replace with 0" , nblank ) ;
      anyout_c( &one , tofchar( line ) ) ;
      setfblank_c( &blank ) ;			/* get blank value	*/
      for ( n = 0 ; n < pro_fit->prof.n ; n++ ) {
         if ( pro_fit->prof.y[ n ] == blank ) pro_fit->prof.y[ n ] = 0 ;
      }
   }

   return ;
}						/* GetProfile		*/


/*
   The function GetPlotSetup asks the user for a number of details
regarding the way in which the plots are being made.

*/
static void GetPlotSetup( PlotSetup  *plotsetup ,
                          ProfileFit *pro_fit   ,
                          SetInfo    *setinfo   ) /* get plotting setup	*/
{
   fint		nret = 0 ;			/* user I/O return 	*/

   float	range ;
   float	xlo = 0 , xhi = 0 , ylo = 0 , yhi = 0 ;

   anyout_c( &test , tofchar( "Function GetPlotSetup" ) ) ;

   plotsetup->ymin = pro_fit->prof.min ;	/* set plotsizes	*/
   plotsetup->ymax = pro_fit->prof.max ;
   range = plotsetup->ymax - plotsetup->ymin ;
   plotsetup->ymin = plotsetup->ymin - 0.1 * range ;
   plotsetup->ymax = plotsetup->ymax + 0.2 * range ;

   plotsetup->xmin = pro_fit->prof.x[ 0 ] ;
   plotsetup->xmax = pro_fit->prof.x[ pro_fit->prof.n - 1 ] ;
   range = plotsetup->xmax - plotsetup->xmin ;
   plotsetup->xmin = plotsetup->xmin - 0.1 * range ;
   plotsetup->xmax = plotsetup->xmax + 0.1 * range ;

   pgqvp_c( &two , &xlo , &xhi , &ylo , &yhi ) ;	/* get sizes	*/
						/* adjust plot sizes ?	*/
   plotsetup->xsize = PLOT_XSIZE ;		/* set default size	*/
   if ( xhi < 10 * ( plotsetup->xsize + plotsetup->xmarg ) ) 	/* in X	*/
      plotsetup->xsize = xhi / 10 - plotsetup->xmarg ;
   plotsetup->ysize = PLOT_YSIZE ;		/* set default size	*/
   if ( yhi < 10 * ( plotsetup->ysize + plotsetup->ymarg ) )	/* in Y	*/
      plotsetup->ysize = yhi / 10 - plotsetup->ymarg ;

   plotsetup->xsc  = 				/* determine x - scale	*/
           ( plotsetup->xmax - plotsetup->xmin )  / plotsetup->xsize ;
   plotsetup->ysc  = 				/* determine y - scale	*/
           ( plotsetup->ymax - plotsetup->ymin )  / plotsetup->ysize ;

   nret = userlog_c( &plotsetup->grids ,	/* plot grids?		*/
                     &one ,			/* one item		*/
                     &two ,			/* hidden keyword	*/
                      tofchar( "GRIDS=" ) ,	/* keyword		*/
                      tofchar( "Plot the grids? [ Y ]" ) ) ;
   nret = userlog_c( &plotsetup->zero ,		/* plot zero level ?	*/
                     &one ,			/* one item		*/
                     &two ,			/* hidden keyword	*/
                      tofchar( "ZERO=" ) ,	/* keyword		*/
                      tofchar( "Plot the grids? [ Y ]" ) ) ;

   return ;
}						/* GetPlotSetup		*/


/*
   The function GetXAxisLabel makes a label for the x-axis in the plot based
on the axis type and units.

*/
static void GetXAxisLabel( fint  axtype ,	/* axis type		*/
                           fchar units  ,	/* axis units		*/
                           fchar label  )	/* axis label		*/
{
   fint		n = 0 ;

   anyout_c( &test , tofchar( "Function GetXAxisLabel" ) ) ;

   switch ( axtype ) {
      case  1:
      case -1:
      case  2:  sprintf( label.a , "Position (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case -4:
      case  3:  sprintf( label.a , "Frequency (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case -5:
      case  4:  sprintf( label.a , "Velocity (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  5:  sprintf( label.a , "Wavelength (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  6:  sprintf( label.a , "Inverse wavelength (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  7:  sprintf( label.a , "Log(lambda) (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  8:  sprintf( label.a , "Time (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  9:  sprintf( label.a , "Polarization (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case 10:  sprintf( label.a , "Parameter (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case 11:  sprintf( label.a , "IRAS sample (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case 12:  sprintf( label.a , "IRAS ticks (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case 13:  sprintf( label.a , "Sequential IRAS detector (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case 14:  sprintf( label.a , "IRAS snip (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      default:  sprintf( label.a , "(%.*s)",(int) nelc_c( units ) , units.a  ) ;
                break ;
   }
   for ( n = strlen( label.a ) ; n < label.l ; n++ ) {
      label.a[ n ] = ' ' ;
   }

   return ;
}						/* GetXAxisLabel	*/


/*
   The function GetYAxisLabel makes a label for the y-axis in the plot based
on the axis type and units.

*/
static void GetYAxisLabel( fint  axtype ,	/* axis type		*/
                           fchar units  ,	/* axis units		*/
                           fchar label  )	/* axis label		*/
{
   fint		n = 0 ;

   anyout_c( &test , tofchar( "Function GetyAxisLabel" ) ) ;

   switch ( axtype ) {
      case  1:  sprintf( label.a , "Angle (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  2:  sprintf( label.a , "Distance (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  3:  sprintf( label.a , "Time (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  4:  sprintf( label.a , "Frequency (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  5:  sprintf( label.a , "Velocity (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  6:  sprintf( label.a , "Temperature (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  7:  sprintf( label.a , "Flux density (%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
      case  9:  sprintf( label.a , "Optical depth" ) ;
                break ;
      default:  sprintf( label.a , "(%.*s)" ,
                         (int) nelc_c( units ) , units.a ) ;
                break ;
   }
   for ( n = strlen( label.a ) ; n < label.l ; n++ ) {
      label.a[ n ] = ' ' ;
   }

   return ;
}						/* GetYAxisLabel	*/


/*
   The function PlotFrame opens the plot and plots the frame.

*/
static void PlotFrame( ProfileFit *pro_fit   ,
                       PlotSetup  *plotsetup ,
                       Set        *inset ,
                       SetInfo    *setinfo   ,
                       RunMode    *mode      )	/* plot the frame	*/
{
   char		label[ MAXTXTLEN ] ;		/* plot label		*/
   char		txtbuf[ MAXTXTLEN ] ;		/* dummy text string	*/

   fchar	xlabel , ylabel ;		/* x/y axis labels	*/

   fint		nsub ;				/* nr of sub intervals	*/
   fint		n = 0 , m = 0 ;			/* counters		*/
   fint		cola = 0 , colb = 0 ;		/* for pgqcol		*/
   fint		old_col = -1 ;			/* current color	*/
   fint		frame_col = orange ;		/* color of the  frame	*/

   float	xlo , xhi , ylo , yhi ;		/* plotcoordinates	*/
   float 	ticks ;				/* tick interval	*/
   float	xdisp , ydisp ;			/* displacements 	*/

   anyout_c( &test , tofchar( "Function PlotFrame" ) ) ;

   anyout_c( &test , tofchar( "Plotting the frame" ) ) ;

   if ( plotsetup->device.name.a == NULL ) {	/* device name empty	*/
      finit( plotsetup->device.name , MAXTXTLEN ) ;	/* initialise 	*/
   }

   if ( plotsetup->device.open ) {		/* plot device is open	*/
      pgiden_c( ) ;				/* put ID on plot	*/
      pgend_c( ) ;				/* close last plot	*/
      plotsetup->device.open = false ;		/* now it's closed	*/
   }

   plotsetup->device.name.a = 			/* ask user for device	*/
      strcpy( plotsetup->device.name.a , "?" ) ;
   if ( pgbeg_c( &zero ,			/* try to open plot	*/
                  plotsetup->device.name ,	/* get device name	*/
                 &one , &one ) != 1 ) {		/* 1 * 1 mosaic		*/
      error_c( &e_fatal , 			/* tel user, exit	*/
               tofchar( "Serious trouble trying to open plot" ) ) ;
   } else {					/* open was successful	*/
      plotsetup->device.open = true ;		/* plot is open		*/
   }
   pgqinf_c( tofchar( "DEVICE" ) ,		/* get plot device name	*/
             plotsetup->device.name , &n ) ;

   n = usercharu_c(  plotsetup->device.name ,
                    &one,
                    &hidden ,
                     tofchar( "GRDEVICE=" ) ,
                     tofchar( " " ) ) ;

   sprintf( txtbuf , "Plotting on %.*s" ,
                      nelc_c( plotsetup->device.name ) ,
                              plotsetup->device.name.a ) ;
   anyout_c( &test , tofchar( txtbuf ) ) ;
   if ( !strncmp( plotsetup->device.name.a , "NULL" ,
                  nelc_c( plotsetup->device.name ) ) ) {
      return ;					/* no plot -> exit	*/
   }

   finit( xlabel , MAXTXTLEN ) ;		/* initialise fchars	*/
   finit( ylabel , MAXTXTLEN ) ;

   Send_Status( ", plotting" , true ) ;

   GetPlotSetup( plotsetup ,			/* get plotting setup	*/
                 pro_fit   ,			/* use profile data	*/
                 setinfo   ) ;			/* use setinfo		*/

   n = userint_c( &frame_col ,
                  &one,
                  &hidden ,
                   tofchar( "FRAMECOL=" ) ,
                   tofchar( " " ) ) ;
   pgqcol_c( &cola , &colb ) ;			/* get color range	*/
   if ( colb >= frame_col ) {
      pgqci_c( &old_col ) ;			/* get current color	*/
      pgsci_c( &frame_col ) ;			/* set new color	*/
   }
   pgslw_c( &thicklines ) ;			/* draw thick lines	*/
   pgsch_c( &big_char ) ;

   pgsvp_c( &fzero , &fone , &fzero , &fone ) ;	/* use all page	*/

   if ( plotsetup->grids ) {			/* plot the grids	*/
      anyout_c( &test , tofchar( "Plotting the grids" ) ) ;
      pgsch_c( &small_char ) ;
						/* make grid viewport	*/
      xlo = ( plotsetup->xmarg +
              ( pro_fit->prof.x[ 0 ] - plotsetup->xmin ) / plotsetup->xsc )
                                                   * INCHPERCM ;
      ylo =   plotsetup->ymarg * INCHPERCM ;
      xhi = ( plotsetup->xmarg + plotsetup->xsize -
              ( plotsetup->xmax - pro_fit->prof.x[ pro_fit->prof.n - 1 ] )
                                / plotsetup->xsc ) * INCHPERCM ;
      yhi = ( plotsetup->ymarg + plotsetup->ysize ) * INCHPERCM ;
      pgvsiz_c( &xlo , &xhi , &ylo , &yhi ) ;	/* set vieport size	*/
      xlo = 1 ;
      xhi = xlo - 1 + pro_fit->prof.n ;
      ylo = 0 ;
      yhi = 1 ;
      pgswin_c( &xlo , &xhi , &ylo , &yhi ) ;	/* set window in grids	*/
      ticks = 10 ;
      nsub  = 10 ;
						/* frame with grids	*/
      pgbox_c( tofchar( "BCTS" ) , &ticks , &nsub ,
               tofchar( "" )   , &fzero , &zero ) ;
      pgsch_c( &big_char ) ;
   }
   if ( old_col != -1 ) pgsci_c( &old_col ) ;

   xlo =   plotsetup->xmarg * INCHPERCM ;	/* make vieport size 	*/
   ylo =   plotsetup->ymarg * INCHPERCM ;
   xhi = ( plotsetup->xmarg + plotsetup->xsize ) * INCHPERCM ;
   yhi = ( plotsetup->ymarg + plotsetup->ysize ) * INCHPERCM ;
   pgvsiz_c( &xlo , &xhi , &ylo , &yhi ) ;	/* set vieport size	*/

   sprintf( label , "%s v. %s" , PROGRAM , VERSION ) ;
   ydisp  = 1.9 ;
   xdisp  = -0.1 ;
   pgmtxt_c( tofchar( "T" ) , &ydisp , &xdisp , &fzero , tofchar( label ) ) ;

						/* set window in units	*/
   pgswin_c( &plotsetup->xmin ,			/* minimum x coordinate	*/
             &plotsetup->xmax ,			/* maximum x coordinate	*/
             &plotsetup->ymin ,			/* minimum y coordinate	*/
             &plotsetup->ymax ) ;		/* maximum y coordinate	*/

   if ( old_col != -1 ) pgsci_c( &frame_col ) ;
   if ( plotsetup->grids ) {			/* grids plotted ?	*/
      anyout_c( &test , tofchar( "Plotting units outside frame" ) ) ;
						/* frame, units	outside */
      pgbox_c( tofchar( "BCINTS" ) , &fzero , &zero ,
               tofchar( "BCNTSV" ) , &fzero , &zero ) ;
   } else {					/* no grids		*/
      anyout_c( &test , tofchar( "Plotting units inside frame" ) ) ;
						/* frame, units	inside	*/
      pgbox_c( tofchar( "BCNTS" )  , &fzero , &zero ,
               tofchar( "BCNTSV" ) , &fzero , &zero ) ;
   }

						/* label the frame 	*/
   GetXAxisLabel( setinfo->xtype , setinfo->xunit , xlabel ) ;
   GetYAxisLabel( setinfo->ytype , setinfo->yunit , ylabel ) ;
   pglab_c( xlabel , ylabel , tofchar( " " ) ) ;
   if ( old_col != -1 ) pgsci_c( &old_col ) ;

   pgslw_c( &thinlines ) ;
   pgsch_c( &med_char ) ;
   sprintf( label , "set %.*s, (" , nelc_c( inset->set ) , inset->set.a ) ;
   for( n = 0 ; n < inset->setdim - 1 ; n++ ){
      m = nelc_c( setinfo->axname[ n ] ) > 3 ?
             3 : nelc_c( setinfo->axname[ n ] ) ;
      sprintf( txtbuf , "%.*s\0" , m , setinfo->axname[ n ].a ) ;
      strcat( label , txtbuf ) ;
      if ( n < inset->setdim - 2 ) strcat( label , "," ) ;
   }
   strcat( label , ")=(" ) ;
   for( n = 0 ; n < inset->setdim - 1 ; n++ ){
      sprintf( txtbuf , "%d\0" , pro_fit->prof.pos[ n ]) ;
      strcat( label , txtbuf ) ;
      if ( n < inset->setdim - 2 ) strcat( label , "," ) ;
   }
   strcat( label , ")" ) ;
   ydisp = 2.5 ;
   pgmtxt_c( tofchar( "T" ) , &ydisp , &fone , &fone , tofchar( label ) ) ;

   sprintf( label , "Scales: %6.3g %.*s/mm and %6.3g %.*s/mm" ,
            plotsetup->xsc , nelc_c( setinfo->xunit ) , setinfo->xunit.a ,
            plotsetup->ysc , nelc_c( setinfo->yunit ) , setinfo->yunit.a ) ;
   ydisp = 1.0 ;
   pgmtxt_c( tofchar( "T" ) , &ydisp , &fone , &fone , tofchar( label ) ) ;

   pgslw_c( &thicklines ) ;
   if ( old_col != -1 ) pgsci_c( &frame_col ) ;
   sprintf( label , "%.*s" ,
                    nelc_c( setinfo->instrument ) , setinfo->instrument.a ) ;
   ydisp = -2.0 ;
   xdisp =  0.1 ;
   pgmtxt_c( tofchar( "T" ) , &ydisp , &xdisp , &fzero , tofchar( label ) ) ;
   sprintf( label , "%.*s" , nelc_c( setinfo->object ) , setinfo->object.a ) ;
   ydisp = -2.0 ;
   xdisp =  0.9 ;
   pgmtxt_c( tofchar( "T" ) , &ydisp , &xdisp , &fone , tofchar( label ) ) ;
   if ( plotsetup->zero ) {
      anyout_c( &test , tofchar( "Plotting zero level" ) ) ;
      pgslw_c( &thinlines ) ;			/* draw a thin line	*/
      pgmove_c( &plotsetup->xmin , &fzero ) ;	/* goto begin of zero	*/
      pgdraw_c( &plotsetup->xmax , &fzero ) ;	/* draw zero level	*/
      pgslw_c( &thicklines ) ;			/* continu thick lines	*/
   }
   if ( old_col != -1 ) pgsci_c( &old_col ) ;
   free( xlabel.a ) ;
   free( ylabel.a ) ;
   return ;
}						/* PlotFrame		*/


/*
   The function PlotProfile plots a profile

*/
static void PlotProfile( Profile    *prof ,	/* input profile	*/
                         fint        type ,	/* type of the profile	*/
                         RunMode    *mode  )	/* plot the profile	*/
{
   char		mess[ MAXTXTLEN ] ;		/* message buffer	*/
   char		string[ MAXTXTLEN ] ;		/* message buffer	*/
   char		key[ MAXTXTLEN ] ;		/* keyword		*/

   fint		old_line_type = 0 ;		/* old line type	*/
   fint		old_line_width = 0 ;		/* old line width	*/
   fint		nret ;				/* nr. items returned	*/
   fint		ppar[ 4 ] ;			/* plot parameters	*/
   fint		cola = 0 , colb = 0 ;		/* for pgqcol		*/
   fint		old_col = -1 ;			/* current color 	*/

   anyout_c( &test , tofchar( "Function PlotProfile" ) ) ;

   switch ( type ) {				/* different data types	*/
      case DATA : 				/* input data		*/
             strcpy( mess , "Plotting the data" ) ;
             strcpy( key , "DATA=" ) ;
             prof->ltyp = full_line ;
             prof->lwt  = thicklines ;
             prof->sym  = no_symbol ;
             prof->col  = yellow ;
             sprintf( string ,
       "Give line-type, thickness and symbols for plot of data [ %d %d %d ]" ,
       prof->ltyp , prof->lwt, prof->sym ) ;
             break ;
      case RESIDUAL :				/* residuals		*/
             strcpy( mess , "Plotting the residual" ) ;
             strcpy( key , "RES=" ) ;
             prof->ltyp = dotted_line ;
             prof->lwt  = thinlines ;
             prof->sym  = no_symbol ;
             prof->col  = green ;
             sprintf( string ,
       "Give line-type, thickness and symbols for plot of residu [ %d %d %d ]" ,
       prof->ltyp , prof->lwt, prof->sym ) ;
             break ;
      case MODEL :				/* the models		*/
             strcpy( mess , "Plotting the model" ) ;
             strcpy( key , "MOD=" ) ;
             prof->ltyp = full_line ;
             prof->lwt  = thinlines ;
             prof->sym  = no_symbol ;
             prof->col  = blue ;
             sprintf( string ,
       "Give line-type, thickness and symbols for plot of model [ %d %d %d ]" ,
       prof->ltyp , prof->lwt, prof->sym ) ;
             break ;
      case GUESS :				/* the guesses		*/
             strcpy( mess ,  "Plotting the guess" ) ;
             strcpy( key , "GUES=" ) ;
             prof->ltyp = dash_dot_dot ;
             prof->lwt  = thinlines ;
             prof->sym  = no_symbol ;
             prof->col  = red_magenta ;
             sprintf( string ,
       "Give line-type, thickness and symbols for plot of guess [ %d %d %d ]" ,
       prof->ltyp , prof->lwt, prof->sym ) ;
             break ;
      default :					/* everything else	*/
             error_c( &e_warning ,
                      tofchar( "PlotProfile: impossible plot!" ) ) ;
             return ;
             break ;
   }

   nret = userint_c(  ppar ,			/* plots specification	*/
                     &four ,			/* three parameters	*/
                     &two ,			/* hidden keyword	*/
                      tofchar( key ) ,		/* keyword		*/
                      tofchar( string ) ) ;	/* and message		*/
   if ( nret == 4 ) {				/* right nr. items	*/
      if ( ( ppar[ 0 ] > -1 ) && ( ppar[ 0 ] < 6 ) )
         prof->ltyp = ppar[ 0 ] ;		/* set line type	*/
      if ( ( ppar[ 1 ] > -1 ) && ( ppar[ 1 ] < 202 ) )
         prof->lwt  = ppar[ 1 ] ;		/* set line thickness	*/
      if ( ( ppar[ 2 ] > -1 ) && ( ppar[ 2 ] < 32 ) )
         prof->sym  = ppar[ 2 ] ;		/* set symbol		*/
      if ( ( ppar[ 3 ] > -1 ) && ( ppar[ 3 ] < 256 ) )
         prof->col  = ppar[ 3] ;		/* set symbol		*/
   }

   anyout_c( &test , tofchar( mess ) ) ;
   sprintf( mess , "Plotting %d points, ltyp %d, lwt %d, sym %d" ,
                    prof->n , prof->ltyp , prof->lwt , prof->sym ) ;
   anyout_c( &test , tofchar( mess ) ) ;
   sprintf( mess , "Minimum %10.2g, maximum %10.2g" ,
                    prof->min , prof->max ) ;
   anyout_c( &test , tofchar( mess ) ) ;

   pgqcol_c( &cola , &colb ) ;
   if ( colb >= prof->col ) {
      pgqci_c( &old_col ) ;
      pgsci_c( &prof->col ) ;
   }

   if ( ( prof->lwt  != 0 ) &&
        ( prof->ltyp != 0 ) ) {			/* plot a line		*/

      pgqls_c( &old_line_type ) ;		/* save current style	*/
      pgqlw_c( &old_line_width ) ;

      pgsls_c( &prof->ltyp ) ;			/* set new style	*/
      pgslw_c( &prof->lwt ) ;

      pgline_c( &prof->n , prof->x , prof->y ) ;/* plot the graph	*/

      pgsls_c( &old_line_type ) ;		/* reset style		*/
      pgslw_c( &old_line_width ) ;

   }

   if ( prof->sym != no_symbol ) {		/* plot the data points	*/

      pgqlw_c( &old_line_width ) ;		/* save current style	*/
      if ( prof->lwt != 0 ) {
         pgslw_c( &prof->lwt ) ;		/* set new style	*/
      }

      pgpt_c( &prof->n , prof->x , prof->y , &prof->sym ) ;

      pgslw_c( &old_line_width ) ;		/* reset style		*/
   }

   if ( old_col != -1 ) pgsci_c( &old_col ) ;

   return ;
}						/* PlotProfile		*/


/*
   The functions func and derv provide the functions and their derivatives
for the fits.

*/
real func_c( float *x, float *par, fint *npar, fint *type )
{
   float	result = 0 ;			/* result value		*/

   switch( *type ) {				/* all types		*/
      case GAUSS :
         result = gaussn_c( x , par , npar ) ; /* gaussian	*/
         break ;
      case VOIGT :
         result = voigt_c( x , par , npar ) ; /* voigt		*/
         break ;
      case POLY :
         result = polyn_c( x , par , npar ) ; /* polynomial	*/
         break ;
      default :
         error_c( &e_fatal , tofchar( "Func: no fit function" ) ) ;
         break ;
   }

   return( result ) ;
}

void derv_c( float *x, float *par, float *dpar, fint *npar, fint* type )
{
   switch( *type ) {				/* all types	*/
      case GAUSS :
         dgausn_c( x , par , dpar , npar ) ;	/* Gauss	*/
         break ;
      case VOIGT :
         dvoigt_c( x , par , dpar , npar ) ; 	/* Voigt	*/
         break ;
      case POLY :
         dpolyn_c( x , par , dpar , npar ) ; 	/* Poly		*/
         break ;
      default :
         error_c( &e_fatal , tofchar( "Derv: no fit function" ) ) ;
         break ;
   }

   return ;
}


/*
   The function AskWhatFit asks the user to specify what type of fit he
wants.

*/
static void AskWhatFit( RunMode    *mode , 	/* ask fitting params.	*/
                        FitProgram *fitprog ,
                        ProfileFit *pro_fit )
{
   char		mess[ MAXTXTLEN ] ;		/* message string	*/
   char		strbuf[ NFSTR * MAXTXTLEN ] ;	/* buffer for FIT= key	*/

   bool		ok = false ;			/* got good fitfunc.	*/

   fchar	strings[ NFSTR ] ;		/* strings for user i/o	*/

   fint		n ;				/* counter		*/
   fint		nret = 0 ;			/* return value		*/
   fint		nfstr = NFSTR ;			/* nr. strings fro FIT=	*/

   anyout_c( &test , tofchar( "Function AskWhatFit" ) ) ;

   if ( mode->level != 1 ) return ;		/* no interact -> exit	*/

   for ( n = 0 ; n < NFSTR ; n++ ) {
      strings[ n ].a = &strbuf[ n * MAXTXTLEN ] ;
      strings[ n ].l = MAXTXTLEN ;
   }

   if ( !mode->run ) {				/* still programming	*/
      if ( fitprog->fc[ pro_fit->curr ].type == NOFUNC ) {
         fitprog->fc[ pro_fit->curr ].ncmp = 1 ;/* default: 1 comp. 	*/
         fitprog->fc[ pro_fit->curr ].type = GAUSS ;
      }
   }
   if ( fitprog->fc[ pro_fit->curr ].type == POLY ) {
         fitprog->fc[ pro_fit->curr ].ncmp =
            fitprog->fc[ pro_fit->curr ].ncmp - 1 ;
   }

   while ( !ok ) {
      sprintf( strings[ 0 ].a , "%s" ,
              FUN[ fitprog->fc[ pro_fit->curr ].type ].name ) ;
      sprintf( strings[ 1 ].a , "%d" ,
                       fitprog->fc[ pro_fit->curr ].ncmp ) ;
      sprintf( mess , "Give function to fit [ %.*s %.*s ]" ,
                       nelc_c( strings[ 0 ] ) , strings[ 0 ].a ,
                       nelc_c( strings[ 1 ] ) , strings[ 1 ].a ) ;
      nret = usercharu_c(  strings[ 0 ] ,	/* get fit-function	*/
                          &nfstr ,		/* has nfstr words	*/
                          &one ,		/* has a default	*/
                           tofchar( "FIT=" ) ,	/* keyword		*/
                           tofchar( mess ) ) ;	/* message		*/
      sprintf( mess , "FIT= gave nret %d and value %.*s %.*s" ,
                       nret ,
                       nelc_c( strings[ 0 ] ) , strings[ 0 ].a ,
                       nelc_c( strings[ 1 ] ) , strings[ 1 ].a ) ;
      anyout_c( &test , tofchar( mess ) ) ;

      for ( n = 0 ; n < MAXFUNCS ; n++ ) {	/* loop on all funcs.	*/
         if ( !strncmp( FUN[ n ].name , strings[ 0 ].a
                                      , nelc_c( strings[ 0 ] ) ) &&
              ( nret <= FUN[ n ].nret )             ) {
            fitprog->fc[ pro_fit->curr ].type = FUN[ n ].type ;
            ok = true ;
            if ( nret == 2 ) {
               strings[ 1 ].a[ nelc_c( strings[ 1 ] ) ] = 0 ;
               fitprog->fc[ pro_fit->curr ].ncmp =
                            (fint)atoi( strings[ 1 ].a ) ;
            }
            if ( fitprog->fc[ pro_fit->curr ].type == POLY ) {
              fitprog->fc[ pro_fit->curr ].ncmp =
                 fitprog->fc[ pro_fit->curr ].ncmp + 1 ;
            }
            fitprog->fc[ pro_fit->curr ].npar =
                fitprog->fc[ pro_fit->curr ].ncmp *
                   FUN[ fitprog->fc[ pro_fit->curr ].type ].npar +
                      FUN[ fitprog->fc[ pro_fit->curr ].type ].zero ;
            if ( fitprog->fc[ pro_fit->curr ].npar > MAXPAR ) {
               sprintf( mess , "To many parameters in %.*s fit, max %d, try again" ,
                                nelc_c( strings[ 0 ] ) , strings[ 0 ].a ,
                                MAXPAR ) ;
            } else {
               break ;
            }
         } else {
            sprintf( mess , "Unknown function: %.*s %.*s, try again" ,
                             nelc_c( strings[ 0 ] ) , strings[ 0 ].a ,
                             nelc_c( strings[ 1 ] ) , strings[ 1 ].a ) ;
         }
      }
      if ( !ok ) {				/* not good function	*/
         error_c( &e_warning , tofchar( mess ) ) ;
         cancel_c( tofchar( "FIT=" ) ) ;
      }
   }

   return ;
}						/* AskWhatFit		*/



/*
   The function GetGuess ask the user for guesses for the fit

*/
static void GetGuess( RunMode    *mode, 	/* Get guesses for fit	*/
                      FitProgram *fitprog,
                      ProfileFit *pro_fit )
{
   bool		ok = false ;			/* proper GUESS?	*/

   char		mess[ MAXTXTLEN ] ;		/* message string	*/
   char		txtbuf[ MAXTXTLEN ] ;		/* text buffer		*/
   char		key[ MAXTXTLEN ] ;		/* keyword		*/

   int          i;
   fint		n , m ;				/* counter		*/
   fint		nret = 0 ;			/* return value		*/

   float	guess[ MAXPAR ] ;		/* buffer array		*/
   float	err[ MAXPAR ] ;

   anyout_c( &test , tofchar( "Function GetGuess" ) ) ;


   /* Array 'err' was not initialized in a previous version of PROFIT */
   /* This caused crashes on Alpha machines (VOG).   */
   
   for (i = 0; i < MAXPAR; i++)
      err[i] = 0.0;


   if ( fitprog->fc[ pro_fit->curr ].type == POLY ) return ;

   if ( mode->level == 1 ) {			/* inter active mode	*/
      if ( FUN[ fitprog->fc[ pro_fit->curr ].type ].zero == 1 ) {
         sprintf( mess , "Give zero level guess for %s fit [%7.1g]" ,
                      FUN[ fitprog->fc[ pro_fit->curr ].type ].name ,
                      fitprog->fc[ pro_fit->curr ].guess[ 0 ] ) ;
         nret = userreal_c( &fitprog->fc[ pro_fit->curr ].guess[ 0 ] ,
                            &one ,
                            &one ,
                             tofchar( "GUESS0=" ),
                             tofchar( mess ) ) ;
      }

   						/* loop on components	*/
      for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ) {
         sprintf( key , "GUESS%d=" , n + 1 ) ;	/* make keyword		*/
   						/* make message string	*/
         sprintf( mess , "%s for %s cmp. %d [" ,
                      FUN[ fitprog->fc[ pro_fit->curr ].type ].mes ,
                      FUN[ fitprog->fc[ pro_fit->curr ].type ].name , n + 1 ) ;

         for ( m = 0 ; m < FUN[fitprog->fc[pro_fit->curr].type].npar ; m++ ) {
            sprintf( txtbuf , "%5.4g" ,
                      fitprog->fc[ pro_fit->curr ].guess[
                      FUN[ fitprog->fc[ pro_fit->curr ].type ].zero +
                      m + n * FUN[fitprog->fc[pro_fit->curr].type].npar ] ) ;
            if ( m < FUN[ fitprog->fc[ pro_fit->curr ].type ].npar - 1 ) {
               strcat( txtbuf , "," ) ;
            }
            strcat( mess , txtbuf ) ;
         }
         strcat( mess , "]" ) ;			/* message ready	*/
         ok = false ;   			/* ask guesses		*/
         while ( !ok ) {			/* loop to get guesses	*/
            nret = userreal_c( &fitprog->fc[ pro_fit->curr ].guess[
                                FUN[fitprog->fc[pro_fit->curr].type].zero +
                                n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ,
                               &FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ,
                               &one ,
                                tofchar( key ),
                                tofchar( mess ) ) ;
                               
            ok = ( nret == FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ) ||
                 ( nret == 0 ) ;
            if ( !ok ) {			/* not good nr. guesses	*/
               cancel_c( tofchar( key ) ) ;	/* cancel keyword	*/
               sprintf( txtbuf ,
                        "Wrong number of guesses, you must specify %d!" ,
                        FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ) ;
               error_c( &e_warning , tofchar( txtbuf ) ) ;
            }
         }
      }
   }
   sprintf( mess , "Calculating guess for %d comp. %s, in total %d params." ,
                    fitprog->fc[ pro_fit->curr ].ncmp ,
                    FUN[ fitprog->fc[ pro_fit->curr ].type ].name ,
                    fitprog->fc[ pro_fit->curr ].npar ) ;
   anyout_c( &test , tofchar( mess ) ) ;

   for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].npar ; n++ ) {
      guess[ n ] = fitprog->fc[ pro_fit->curr ].guess[ n ] ;
      sprintf( mess , "Parameter %d, guess = %f" , n ,
                       fitprog->fc[ pro_fit->curr ].guess[ n ] ) ;
      anyout_c( &test , tofchar( mess ) ) ;
   }

   for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ){
      Amp2Int( fitprog->fc[ pro_fit->curr ].type  ,
               &guess[ FUN[fitprog->fc[ pro_fit->curr ].type ].zero +
                     n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ,
               &err[ FUN[fitprog->fc[ pro_fit->curr ].type ].zero +
                     n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ) ;
   }
   for ( n = 0 ; n < pro_fit->prof.n ; n++ ) {
      pro_fit->mod.y[ n ] = func_c( &pro_fit->prof.x[ n ] ,
                                     guess ,
                                    &fitprog->fc[ pro_fit->curr ].npar ,
                                    &fitprog->fc[ pro_fit->curr ].type ) ;
                                    
   }


   pro_fit->mod.n = pro_fit->prof.n ;
   minmax2_c(  pro_fit->mod.y   ,		/* for profile of	*/
              &pro_fit->mod.n   ,		/*   mod.n points get	*/
              &pro_fit->mod.min ,		/*   min and 		*/
              &pro_fit->mod.max ,		/*   max and		*/
              &n                 ) ;		/* nr. of blanks	*/

   PlotProfile( &pro_fit->mod , GUESS    , mode ) ;
   return ;
}						/* GetGuess		*/


/*
   The function FixGuess ask the user which guesses should be kept
fixed in the fit

*/
static void FixGuess( RunMode    *mode, 	/* Get fixed guesses 	*/
                      FitProgram *fitprog,
                      ProfileFit *pro_fit )
{
   bool		mask = false ;			/* was a mask used ?	*/

   char		strbuf[ NFIXSTR * MAXTXTLEN ] ;	/* buffer for FIT= key	*/
   char		mess[ MAXTXTLEN ] ;		/* message string	*/
   char		key[ MAXTXTLEN ] ;		/* keyword		*/

   fchar	strings[ NFIXSTR ] ;		/* strings for user i/o	*/

   fint		nfixstr = NFIXSTR ;		/* nr. if FIX strings	*/
   fint		n , m , i ;			/* counter		*/
   fint		nret = 0 ;			/* return value		*/

   anyout_c( &test , tofchar( "Function FixGuess" ) ) ;

   if ( fitprog->fc[ pro_fit->curr ].type == POLY ) return ;

   if ( mode->level == 1 ) {			/* inter active mode	*/

      for ( n = 0 ; n < NFIXSTR ; n++ ) {
         strings[ n ].a = &strbuf[ n * MAXTXTLEN ] ;
         strings[ n ].l = MAXTXTLEN ;
      }

      for ( n = 0 ;
            n < FUN[ fitprog->fc[ pro_fit->curr ].type ].zero +
                FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ; n++ ) {
         if ( fitprog->fc[ pro_fit->curr ].msk[ n ] == 0 ) {
            mask = true ;
            break ;
         }
      }

      if ( mask ) {
         sprintf( mess , "Give parameters to fix for %s fit [ as before ]" ,
                         FUN[ fitprog->fc[ pro_fit->curr ].type ].name ) ;
      } else {
         sprintf( mess , "Give parameters to fix for %s fit [ none ]" ,
                         FUN[ fitprog->fc[ pro_fit->curr ].type ].name ) ;
      }
      nret = usercharu_c(  strings[ 0 ] ,	/* get fix parameters	*/
                          &nfixstr ,		/* has nfixstr words	*/
                          &one ,		/* has a default	*/
                           tofchar( "FIX=" ) ,	/* keyword		*/
                           tofchar( mess ) ) ;	/* message		*/
      for ( n = 0 ; n < nret ; n++ ) {
         sprintf( mess , "FIX= value %d is %.*s" , n ,
                  nelc_c( strings[ n ] ) , strings[ n ].a ) ;
         anyout_c( &test , tofchar( mess ) ) ;
      }
      if ( nret > 0 ) {
         fitprog->fc[ pro_fit->curr ].msk[ 0 ] = 1 ;
         for ( m = 0 ; m < fitprog->fc[ pro_fit->curr ].ncmp ; m++ ) {
            for ( i = 0 ;
                  i < FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ;
                  i++ ) {
               fitprog->fc[ pro_fit->curr ].msk[
                  FUN[ fitprog->fc[ pro_fit->curr ].type ].zero + i +
                  m * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] = 1 ;
            }
         }
         if ( FUN[ fitprog->fc[ pro_fit->curr ].type ].zero == 1 ) {
            for ( n = 0 ; n < nret ; n++ ) {
               if ( !strncmp( strings[ n ].a , "BASE" , 4 ) ) {
                  fitprog->fc[ pro_fit->curr ].msk[ 0 ] = 0 ;
               }
            }
         }
   						/* loop on components	*/
         for ( m = 0 ; m < fitprog->fc[ pro_fit->curr ].ncmp ; m++ ) {
            for ( i = 0 ;
                  i < FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ;
                  i++ ) {
               if ( fitprog->fc[ pro_fit->curr ].ncmp == 1 ) {
                  sprintf( key , "%s" , 	/* make name		*/
                      FIX_FUN[ fitprog->fc[ pro_fit->curr ].type ].name[ i ] ) ;
               } else {
                  sprintf( key , "%s%d" , 	/* make name		*/
                      FIX_FUN[ fitprog->fc[ pro_fit->curr ].type ].name[ i ] ,
                      m + 1 ) ;
               }
               sprintf( mess , "Comparing FIX= names with %s" , key ) ;
               anyout_c( &test , tofchar( mess ) ) ;
               for ( n = 0 ; n < nret ; n++ )
                  if ( !strncmp( strings[ n ].a , key ,
                                 nelc_c( strings[ n ] ) ) ) {
                      fitprog->fc[ pro_fit->curr ].msk[
                        FUN[ fitprog->fc[ pro_fit->curr ].type ].zero + i +
                        m * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] = 0 ;
                  }
            }
         }
      } else if ( !mask ) {
         for ( n = 0 ;
               n < FUN[ fitprog->fc[ pro_fit->curr ].type ].zero +
                   FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ;
               n++  ) {
            fitprog->fc[ pro_fit->curr ].msk[ n ] = 1 ;
         }
      }
   }

   for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].npar ; n++ ) {
      sprintf( mess , "Parameter %d, mask = %d" , n ,
                       fitprog->fc[ pro_fit->curr ].msk[ n ] ) ;
      anyout_c( &test , tofchar( mess ) ) ;
   }

   return ;
}						/* FixGuess		*/


/*
   The function SetLimit ask the user limits on the fitted parameters

*/
static void SetLimit( RunMode    *mode, 	/* Get limits for fit 	*/
                      FitProgram *fitprog,
                      ProfileFit *pro_fit )
{
   char		mess[ MAXTXTLEN ] ;		/* message string	*/
   char		key[ MAXTXTLEN ] ;		/* keyword		*/

   fint		n , m ;				/* counter		*/
   fint		nret = 0 ;			/* return value		*/

   float	values[ 2 ] ;			/* value typed by user	*/

   anyout_c( &test , tofchar( "Function SetLimit" ) ) ;

   if ( fitprog->fc[ pro_fit->curr ].type == POLY ) return ;

   if ( mode->level == 1 ) {			/* inter active mode	*/
      for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ){
         for ( m = 0 ;
               m < FUN[ fitprog->fc[ pro_fit->curr ].type ].npar +
                   FUN[ fitprog->fc[ pro_fit->curr ].type ].zero ;
               m++ ) {
            values[ 0 ] =
               fitprog->fc[ pro_fit->curr ].min[ m +
                   n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ;
            values[ 1 ] =
               fitprog->fc[ pro_fit->curr ].max[ m +
                   n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ;
            if ( ( FUN[ fitprog->fc[ pro_fit->curr ].type ].zero ) &&
                 ( m == 0 ) ) {
               sprintf( key , "BASELIM=" ) ;
               sprintf( mess , "Give limits on baseline fit [ %8.2g,%8.2g ]" ,
                        values[ 0 ] , values[ 1 ] ) ;
            } else {
               sprintf( key , "%s%dLIM=" ,
                        FIX_FUN[ fitprog->fc[ pro_fit->curr ].type ].name[
                        m - FUN[ fitprog->fc[ pro_fit->curr ].type ].zero ] ,
                        n + 1 ) ;
               sprintf( mess , "Give limits on %s fit of comp. %d [ %8.2g,%8.2g ]" ,
                        FIX_FUN[ fitprog->fc[ pro_fit->curr ].type ].name[
                        m - FUN[ fitprog->fc[ pro_fit->curr ].type ].zero ] ,
                        n + 1 , values[ 0 ] , values[ 1 ] ) ;
            }
            nret = userreal_c(  values ,
                               &two    ,
                               &hidden ,
                                tofchar( key ) ,
                                tofchar( mess ) ) ;
            sortra_c( values , &two ) ;
            cancel_c( tofchar( key ) ) ;
            if ( ( nret == 0 ) || ( nret == two ) ) {
               fitprog->fc[ pro_fit->curr ].min[ m +
                   n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] =
                                                         values[ 0 ] ;
               fitprog->fc[ pro_fit->curr ].max[ m +
                   n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] =
                                                         values[ 1 ] ;
            }
         }
      }
   }

   for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].npar ; n++ ) {
      sprintf( mess , "Parameter %d, limits = %f to %f" , n ,
                       fitprog->fc[ pro_fit->curr ].min[ n ] ,
                       fitprog->fc[ pro_fit->curr ].max[ n ] ) ;
      anyout_c( &test , tofchar( mess ) ) ;
   }

   return ;
}						/* SetLimit		*/


/*
   The function GetMask asks the user for widows for the fit

*/
static void GetMask( RunMode    *mode, 		/* Get mask for fit	*/
                     FitProgram *fitprog,
                     ProfileFit *pro_fit )
{
   bool		in_window = false ;		/* inside window ?	*/

   char		mess[ MAXTXTLEN ] ;		/* message buffer	*/

   fint		n , m ;				/* counters		*/
   fint		max_win = 2 * MAXWINDOWS ;	/* max nr. of windows	*/
   fint		nwindows = 0 ;			/* nr of windows	*/

   float	window[ 2 * MAXWINDOWS ] ;	/* the windows		*/
   float	win_lev[ 2 ] = { 0 , 0 } ;	/* level to plot at	*/
   float	all_window[ 2 ] ;		/* the entire range	*/

   anyout_c( &test , tofchar( "Function GetMask" ) ) ;

   win_lev[ 0 ] = pro_fit->prof.min -
                  0.05 * ( pro_fit->prof.max -pro_fit->prof.min );
   win_lev[ 1 ] = pro_fit->prof.min -
                  0.05 * ( pro_fit->prof.max -pro_fit->prof.min );
   all_window[ 0 ] = pro_fit->prof.x[ 0 ] ;
   all_window[ 1 ] = pro_fit->prof.x[ pro_fit->prof.n - 1 ] ;

   for ( n = 0 ; n < pro_fit->prof.n ; n++ ){
      if ( fitprog->fc[ pro_fit->curr ].wts[ n ] == 1 ) {
         if ( !in_window ) {
            window[ 2 * nwindows ] = pro_fit->prof.x[ n ] ;
            in_window = true ;
            sprintf( mess , "Found window begin at pixel %d: %f" ,
                     n , window[ 2 * nwindows ] ) ;
            anyout_c( &test , tofchar( mess ) ) ;
         } else {
            window[ 2 * nwindows + 1 ] = pro_fit->prof.x[ n ] ;
         }
      } else {
         if ( in_window ) {
            in_window = false ;
            sprintf( mess , "Found window end at pixel %d: %f" ,
                     n , window[ 2 * nwindows + 1 ] ) ;
            anyout_c( &test , tofchar( mess ) ) ;
            nwindows = nwindows + 1 ;
         }
      }
      fitprog->fc[ pro_fit->curr ].wts[ n ] = 0 ;
   }
   if ( nwindows == 0 ){
      window[ 0 ] = pro_fit->prof.x[ 0 ] ;
      window[ 1 ] = pro_fit->prof.x[ pro_fit->prof.n - 1 ] ;
      nwindows = 1 ;
   } else if ( in_window ) {
      nwindows = nwindows + 1 ;
   }

   for ( m = 0 ; m < nwindows ; m++ ) {		/* loop all windows	*/
      pgline_c( &two , &window[ 2 * m ] , win_lev ) ;	/* plot it	*/
      sprintf( mess , "Old window %d from %f to %f" , m ,
                       window[ 2 * m ] , window[ 2 * m + 1 ] ) ;
      anyout_c( &test , tofchar( mess ) ) ;
   }

   n = userreal_c(  window ,			/* ask windows		*/
                   &max_win ,			/* max nr. of windows	*/
                   &hasdefault ,		/* default exists	*/
                    tofchar( "WINDOW=" ) ,	/* keyword		*/
                    tofchar( "Give windows for fit [ same as before ]" ) ) ;

   if ( n == 0 ) n = 2 * nwindows ;
   if ( 2 * ( (int) n/2 ) != n  ) n = 2 ;
   nwindows = n / 2 ;
   sortra_c( window , &n ) ;

   pgqci_c( &n ) ;				/* find current color	*/
   pgsci_c( &back_ground ) ;			/* set to background	*/
   pgline_c( &two , all_window , win_lev ) ;	/* erase previous	*/
   pgsci_c( &n ) ;				/* set to current color	*/

   for ( m = 0 ; m < nwindows ; m++ ) {		/* loop all windows	*/
      pgline_c( &two , &window[ 2 * m ] , win_lev ) ;	/* plot it	*/
      for ( n = 0 ; n < pro_fit->prof.n ; n++ ) {/* loop all points	*/
         if ( ( pro_fit->prof.x[ n ] >= window[ 2 * m ] ) &&
              ( pro_fit->prof.x[ n ] <= window[ 2 * m + 1 ] ) ) {
            fitprog->fc[ pro_fit->curr ].wts[ n ] = 1 ;	/* set weight	*/
         }
      }
      sprintf( mess , "New window %d from %f to %f" , m ,
                       window[ 2 * m ] , window[ 2 * m + 1 ] ) ;
      anyout_c( &test , tofchar( mess ) ) ;
   }

   return ;
}						/* GetMask		*/


/*
   The function FitProfile does the actual fitting

*/
static void FitProfile( RunMode    *mode, 	/* fit the profile	*/
                        FitProgram *fitprog,
                        ProfileFit *pro_fit )
{
   fint		maxniter = 25 ;			/* max. nr. of iters.	*/
   fint		nret = 0 ;			/* nr items returned	*/
   fint		n ;				/* counter		*/

   float	tolerance = 0.0001 ;		/* tolerance of fit	*/
   float	lambda = 0.01 ;			/* mixing parameter	*/

   anyout_c( &test , tofchar( "Function FitProfile" ) ) ;

   Send_Status( ", fitting" , true ) ;

   nret = userreal_c( &tolerance ,		/* ask tolerance	*/
                      &one ,			/* one real		*/
                      &two ,			/* hidden keyword	*/
                       tofchar( "TOL=" ) ,	/* keyword TOL=		*/
                       tofchar( "Give tolerance for fit" ) ) ;	/* mess	*/

   nret = userreal_c( &lambda ,			/* ask mix. param	*/
                      &one ,			/* one real		*/
                      &two ,			/* hidden keyword	*/
                       tofchar( "LAB=" ) ,	/* keyword LAB=		*/
                       tofchar( "Give mixing paramter for fit" ) ) ;

   nret = userint_c(  &maxniter,		/* ask nr. iterations	*/
                      &one ,			/* one integer		*/
                      &two ,			/* hidden keyword	*/
                       tofchar( "NITER=" ) ,	/* keyword NITER=	*/
                       tofchar( "Give nr. of iterations for fit" ) ) ;

   for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].npar ; n++ ) {
      pro_fit->fits[ pro_fit->curr ].par[ n ] =
                        fitprog->fc[ pro_fit->curr ].guess[ n ] ;
   }

   if ( fitprog->fc[ pro_fit->curr ].type == NOFUNC ) {	/* bad function	*/
      error_c( &e_fatal , tofchar( "FitProfile: no fit function" ) ) ;
   } else {						/* good function*/
      for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ){
         Amp2Int( fitprog->fc[ pro_fit->curr ].type  ,
                  &pro_fit->fits[ pro_fit->curr ].par[
                     FUN[fitprog->fc[ pro_fit->curr ].type ].zero +
                     n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ,
                  &pro_fit->fits[ pro_fit->curr ].err[
                     FUN[fitprog->fc[ pro_fit->curr ].type ].zero +
                     n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ) ;
                     

      }
     
      pro_fit->fits[ pro_fit->curr ].niter =
            lsqfit_c(  pro_fit->prof.x ,		/* go fit	*/
                       &one ,
                        pro_fit->prof.y ,
                        fitprog->fc[ pro_fit->curr ].wts ,
                       &pro_fit->prof.n ,
                        pro_fit->fits[ pro_fit->curr ].par ,
                        pro_fit->fits[ pro_fit->curr ].err ,
                        fitprog->fc[ pro_fit->curr ].msk ,
                       &fitprog->fc[ pro_fit->curr ].npar ,
                       &tolerance ,
                       &maxniter ,
                       &lambda ,
                       &fitprog->fc[ pro_fit->curr ].type ) ;
                       
                    
      for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ){
         Int2Amp( fitprog->fc[ pro_fit->curr ].type  ,
                  &pro_fit->fits[ pro_fit->curr ].par[
                     FUN[fitprog->fc[ pro_fit->curr ].type ].zero +
                     n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ,
                  &pro_fit->fits[ pro_fit->curr ].err[
                     FUN[fitprog->fc[ pro_fit->curr ].type ].zero +
                     n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ) ;
      }
   }

   return ;
}						/* FitProfile		*/

/*
   The function MakeModel makes the model and residual from the fitted
parameters and data points.

*/
static void MakeModel( RunMode    *mode, 	/* make model profile	*/
                       FitProgram *fitprog,
                       ProfileFit *pro_fit )
{
   int          i;
   fint		n ;				/* counter		*/

   float	sum   = 0 ;			/* statistics numbers	*/
   float	sumsq = 0 ;

   float	mod[ MAXPAR ] ;			/* buffer array		*/
   float	err[ MAXPAR ] ;

   anyout_c( &test , tofchar( "Function MakeModel" ) ) ;

   for (i = 0; i < MAXPAR; i++)
   {
      mod[i] = 0.0;
      err[i] = 0.0;
   }

   for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].npar ; n++ ){
      mod[ n ] = pro_fit->fits[ pro_fit->curr ].par[ n ] ;
   }
   for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ){
      Amp2Int( fitprog->fc[ pro_fit->curr ].type  ,
               &mod[ FUN[fitprog->fc[ pro_fit->curr ].type ].zero +
                     n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ,
               &err[ FUN[fitprog->fc[ pro_fit->curr ].type ].zero +
                     n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ) ;
   }

   for ( n = 0 ; n < pro_fit->prof.n ; n++ ) {
      if ( pro_fit->fits[ pro_fit->curr ].niter > 0 ) {	/* good fit	*/
         pro_fit->mod.y[ n ] = func_c( &pro_fit->prof.x[ n ] ,
                                        mod ,
                                       &fitprog->fc[ pro_fit->curr ].npar ,
                                       &fitprog->fc[ pro_fit->curr ].type ) ;
      } else {					/* bad fit		*/
         pro_fit->mod.y[ n ] = 0 ;
      }
      pro_fit->mod.x[ n ] = pro_fit->prof.x[ n ] ;
      pro_fit->res.y[ n ] = pro_fit->prof.y[ n ] -
                                            pro_fit->mod.y[ n ] ;
      pro_fit->res.x[ n ] = pro_fit->prof.x[ n ] ;
      sum   = sum   + pro_fit->res.y[ n ] ;
      sumsq = sumsq + pro_fit->res.y[ n ] * pro_fit->res.y[ n ] ;
   }
   pro_fit->mod.n = pro_fit->prof.n ;
   pro_fit->res.n = pro_fit->prof.n ;
   pro_fit->fits[  pro_fit->curr ].rms =
      sqrt( ( sumsq - sum * sum / pro_fit->prof.n ) / pro_fit->prof.n ) ;
   pro_fit->fits[  pro_fit->curr ].mean = sum / pro_fit->prof.n ;

   minmax2_c( pro_fit->mod.y    ,		/* for profile of	*/
              &pro_fit->mod.n   ,		/*   mod.n points get	*/
              &pro_fit->mod.min ,		/*   min and 		*/
              &pro_fit->mod.max ,		/*   max and		*/
              &n                 ) ;		/* nr. of blanks	*/
   minmax2_c( pro_fit->res.y    ,		/* for profile of	*/
              &pro_fit->res.n   ,		/*   res.n points get	*/
              &pro_fit->res.min ,		/*   min and 		*/
              &pro_fit->res.max ,		/*   max and		*/
              &n                 ) ;		/* nr. of blanks	*/

   return ;
}						/* MakeModel		*/

/*
   The function ShowResults shows the user the result of the fits.

*/
static void ShowResults( ProfileFit *pro_fit ,
                         FitProgram *fitprog ,
                         SetInfo    *setinfo ,
                         Set        *inset   ,
                         RunMode    *mode    )
{
   char		mess[ MAXTXTLEN ] ;		/* message buffer	*/
   char		txtbuf[ MAXTXTLEN ] ;		/* message buffer	*/
   char		line1[ MAXTXTLEN ] ;		/* message buffer	*/
   char		line2[ MAXTXTLEN ] ;		/* message buffer	*/

   fint		n , m ;				/* counter		*/
   fint 	offset ;			/* offset in par-array	*/

   float	ipar[ MAXPAR ] ;		/* array to calc intgs.	*/
   float	ierr[ MAXPAR ] ;		/* array to calc errs.	*/

   anyout_c( &test , tofchar( "Function ShowResults" ) ) ;

   PlotProfile( &pro_fit->mod , MODEL    , mode ) ;	/* plot model	*/
   PlotProfile( &pro_fit->res , RESIDUAL , mode ) ;	/* plot residu	*/

   if ( pro_fit->fits[ pro_fit->curr ].niter > 0 ) {	/* good fit	*/
      sprintf( mess ,
         "%s-fit for position (" ,
         FUN[ fitprog->fc[ pro_fit->curr ].type ].name ) ;
      for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
         sprintf( txtbuf , "%d" , pro_fit->prof.pos[ n ] ) ;
         strcat( mess , txtbuf ) ;
         if ( n < inset->setdim - 2 ) strcat( mess , "," ) ;
      }
      strcat( mess , ")" ) ;
      anyout_c( &one , tofchar( mess ) ) ;
      sprintf( mess ,
         "Fit took %3d iterations, residual mean %5.2g, rms %5.2g %.*s" ,
         pro_fit->fits[ pro_fit->curr ].niter ,
         pro_fit->fits[ pro_fit->curr ].mean ,
         pro_fit->fits[ pro_fit->curr ].rms ,
         nelc_c( setinfo->yunit ) , setinfo->yunit.a ) ;
      anyout_c( &one , tofchar( mess ) ) ;
      if ( FUN[ fitprog->fc[ pro_fit->curr ].type ].zero != 0 ) {
         if ( fitprog->fc[ pro_fit->curr ].msk[ 0 ] == 1 ) {
            sprintf( mess , "Fitted zero level %8.2g +/- %8.2g %.*s" ,
            pro_fit->fits[ pro_fit->curr ].par[ 0 ] ,
            pro_fit->fits[ pro_fit->curr ].err[ 0 ] ,
            nelc_c( setinfo->yunit ) , setinfo->yunit.a ) ;
         } else {
            sprintf( mess , "Zero level fixed at %8.2g %.*s" ,
            pro_fit->fits[ pro_fit->curr ].par[ 0 ] ,
            nelc_c( setinfo->yunit ) , setinfo->yunit.a ) ;
         }
      }
      anyout_c( &one , tofchar( mess ) ) ;


      for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].npar ; n++ ) {
         ipar[ n ] = pro_fit->fits[ pro_fit->curr ].par[ n ] ;
         ierr[ n ] = pro_fit->fits[ pro_fit->curr ].err[ n ] ;
      }

      for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ){
         Amp2Int( fitprog->fc[ pro_fit->curr ].type  ,
                  &ipar[ FUN[fitprog->fc[ pro_fit->curr ].type ].zero +
                        n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ,
                  &ierr[ FUN[fitprog->fc[ pro_fit->curr ].type ].zero +
                        n * FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ] ) ;
      }

      switch ( fitprog->fc[ pro_fit->curr ].type ) {	/* all types	*/
         case GAUSS :
            anyout_c( &one ,
tofchar( "     | Amplitude  |  Integral  |   Center   |    FWHM    |" ) ) ;
            sprintf( mess , "Comp.| %10.*s | %4.4s*%5.5s | %10.*s | %10.*s |" ,
                 nelc_c( setinfo->yunit ) , setinfo->yunit.a ,
                 setinfo->yunit.a         , setinfo->xunit.a ,
                 nelc_c( setinfo->xunit ) , setinfo->xunit.a ,
                 nelc_c( setinfo->xunit ) , setinfo->xunit.a ) ;
            anyout_c( &one , tofchar( mess ) ) ;
            anyout_c( &one ,
tofchar( "-----|------------|------------|------------|------------|" ) ) ;
            offset = FUN[ fitprog->fc[ pro_fit->curr ].type ].zero ;
            for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ) {
sprintf( mess , "%4d |% #11.4g |% #11.4g |% #11.4g |% #11.4g |",
                 n + 1 ,
                 pro_fit->fits[ pro_fit->curr ].par[ n * 3 + offset + 0 ] ,
                 ipar[ n * 3 + offset + 0 ] ,
                 pro_fit->fits[ pro_fit->curr ].par[ n * 3 + offset + 1 ] ,
                 pro_fit->fits[ pro_fit->curr ].par[ n * 3 + offset + 2 ] ) ;
               anyout_c( &one , tofchar( mess ) ) ;
sprintf( mess , "     | (% #8.2g) | (% #8.2g) | (% #8.2g) | (% #8.2g) |",
                 pro_fit->fits[ pro_fit->curr ].err[ n * 3 + offset + 0 ] ,
                 ierr[ n * 3 + offset + 0 ] ,
                 pro_fit->fits[ pro_fit->curr ].err[ n * 3 + offset + 1 ] ,
                 pro_fit->fits[ pro_fit->curr ].err[ n * 3 + offset + 2 ] ) ;
                 if ( fitprog->fc[ pro_fit->curr ].msk[ n * 3 + offset ] == 0 ) {
                    sprintf( &mess[ 8 ] , "       " ) ;
                    mess[ 8 + 7 ] = ' ' ;
                 }
                 for ( m = 0 ;
                       m < FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ;
                       m++ ) {
                    if ( fitprog->fc[ pro_fit->curr ].msk[ n*3+offset+m ] == 0 ) {
                       sprintf( &mess[ 21 + m * 13 ] , "  fixed" ) ;
                       mess[ 21 + m * 13 + 7 ] = ' ' ;
                    }
                 }
               anyout_c( &one , tofchar( mess ) ) ;
            }
            anyout_c( &one ,
 tofchar( "-----|------------|------------|------------|------------|" ) ) ;
            break ;
         case VOIGT :
            anyout_c( &one ,
tofchar( "     | Amplitude  |  Integral  |   Center   |    FWHM    |    LOR     |" ) ) ;
            sprintf( mess , "Comp.| %10.*s | %4.4s*%5.5s | %10.*s | %10.*s | %10.*s |" ,
                 nelc_c( setinfo->yunit ) , setinfo->yunit.a ,
                 setinfo->yunit.a         , setinfo->xunit.a ,
                 nelc_c( setinfo->xunit ) , setinfo->xunit.a ,
                 nelc_c( setinfo->xunit ) , setinfo->xunit.a ,
                 nelc_c( setinfo->xunit ) , setinfo->xunit.a ) ;
            anyout_c( &one , tofchar( mess ) ) ;
            anyout_c( &one ,
tofchar( "-----|------------|------------|------------|------------|------------|" ) ) ;
            offset = FUN[ fitprog->fc[ pro_fit->curr ].type ].zero ;
            for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ) {
sprintf( mess , "%4d |% #11.4g |% #11.4g |% #11.4g |% #11.4g |% #11.4g |",
                 n + 1 ,
                 pro_fit->fits[ pro_fit->curr ].par[ n * 3 + offset + 0 ] ,
                 ipar[ n * 3 + offset + 0 ] ,
                 pro_fit->fits[ pro_fit->curr ].par[ n * 3 + offset + 1 ] ,
                 pro_fit->fits[ pro_fit->curr ].par[ n * 3 + offset + 2 ] ,
                 pro_fit->fits[ pro_fit->curr ].par[ n * 3 + offset + 3 ] ) ;
               anyout_c( &one , tofchar( mess ) ) ;
sprintf( mess , "     | (% #8.2g) | (% #8.2g) | (% #8.2g) | (% #8.2g) | (% #8.2g) |",
                 pro_fit->fits[ pro_fit->curr ].err[ n * 3 + offset + 0 ] ,
                 ierr[ n * 3 + offset + 0 ] ,
                 pro_fit->fits[ pro_fit->curr ].err[ n * 3 + offset + 1 ] ,
                 pro_fit->fits[ pro_fit->curr ].err[ n * 3 + offset + 2 ] ,
                 pro_fit->fits[ pro_fit->curr ].err[ n * 3 + offset + 3 ] ) ;
                 if ( fitprog->fc[ pro_fit->curr ].msk[ n * 3 + offset ] == 0 ) {
                    sprintf( &mess[ 8 ] , "       " ) ;
                    mess[ 8 + 7 ] = ' ' ;
                 }
                 for ( m = 0 ;
                       m < FUN[ fitprog->fc[ pro_fit->curr ].type ].npar ;
                       m++ ) {
                    if ( fitprog->fc[ pro_fit->curr ].msk[ n*3+offset+m ] == 0 ) {
                       sprintf( &mess[ 21 + m * 13 ] , "  fixed" ) ;
                       mess[ 21 + m * 13 + 7 ] = ' ' ;
                    }
                 }
               anyout_c( &one , tofchar( mess ) ) ;
            }
            anyout_c( &one ,
tofchar( "-----|------------|------------|------------|------------|------------|" ) ) ;
            break ;
         case POLY :
            anyout_c( &one ,
tofchar( "| 0th coeff. | 1st coeff. | 2nd coeff. | 3rd coeff  | 4th coeff  |" ) ) ;
            anyout_c( &one ,
tofchar( "|------------|------------|------------|------------|------------|" ) ) ;
            offset = FUN[ fitprog->fc[ pro_fit->curr ].type ].zero ;
            line1[ 0 ] = 0 ;
            line2[ 0 ] = 0 ;
            for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ) {
sprintf( mess , "|% #11.4g " , pro_fit->fits[ pro_fit->curr ].par[ n ] ) ;
               strcat( line1 , mess ) ;
sprintf( mess , "| (% #8.2g) " , pro_fit->fits[ pro_fit->curr ].err[ n ] ) ;
               strcat( line2 , mess ) ;
               if ( 5 * ( (int) ( n + 1 ) / 5 ) == n + 1 ) {
                  strcat( line1 , "|" ) ;
                  strcat( line2 , "|" ) ;
                  anyout_c( &one , tofchar( line1 ) ) ;
                  anyout_c( &one , tofchar( line2 ) ) ;
                  line1[ 0 ] = 0 ;
                  line2[ 0 ] = 0 ;
               }
            }
            if ( line1[ 0 ] != 0 ) {
               strcat( line1 , "|" ) ;
               strcat( line2 , "|" ) ;
               anyout_c( &one , tofchar( line1 ) ) ;
               anyout_c( &one , tofchar( line2 ) ) ;
            }
            anyout_c( &one ,
 tofchar( "|------------|------------|------------|------------|------------|" ) ) ;
            break ;
         default : 				/* dummy -> NOOP	*/
            error_c( &e_warning ,		/* issue error		*/
                     tofchar( "ShowResults: no proper function used" ) ) ;
            break ;
      }

   } else {
      switch ( pro_fit->fits[ pro_fit->curr ].niter ) {
         case -1 : 				/* too many free pars.	*/
            anyout_c( &one ,
               tofchar( "Too many free parameters => no fit" ) ) ;
            break ;
         case -2 : 				/* No free paramters.	*/
            anyout_c( &one ,
               tofchar( "No free parameters => no fit" ) ) ;
            break ;
         case -3 : 				/* not enough freedom.	*/
            anyout_c( &one ,
               tofchar( "Not enough freedom => change window" ) ) ;
            break ;
         case -4 : 				/* too few iterations.	*/
            anyout_c( &one ,
               tofchar( "Too few iterations=> change TOL= or NITER=" ) ) ;
            break ;
         case -5 : 				/* fit matrix problem.	*/
         case -6 :
            anyout_c( &one ,
               tofchar( "Bad matrix in fit-algorithm => change LAB=" ) ) ;
            break ;
         case -7 : 				/* sqrt( -* ).		*/
            anyout_c( &one ,
               tofchar( "Square root of neg. number in fit => bad fit" ) ) ;
            break ;
         default : 				/* unknown error.	*/
            sprintf( mess , "Unknown fit-error %d, => bad fit" ,
                     pro_fit->fits[ pro_fit->curr ].niter ) ;
            anyout_c( &one , tofchar( mess ) ) ;
            break ;
      }
   }

   return ;
}						/* ShowResults		*/


static void JudgeFit( ProfileFit *pro_fit ,	/* Judge current fit	*/
                      FitProgram *fitprog ,	/* current fitprog	*/
                      RunMode *mode ) 		/* change mode ?	*/
{
   char		mess[ MAXTXTLEN ] ;		/* message string	*/
   char		emess[ MAXTXTLEN ] ;		/* error message	*/
   char		strbuf[ 2 * MAXTXTLEN ] ;	/* buffer for FIT= key	*/
   char		opstring[ MAXTXTLEN ] ;		/* next operation	*/

   bool		ok = false ;			/* got good fitfunc.	*/
   bool		part1 = false , part2 = false ;	/* input parts OK?	*/
   bool		good_bad = true ;		/* is fit good or bad?	*/
   bool		sn_crit = false ;		/* passes S/N criterion	*/
   bool		lim_crit = false ;		/* passes lim criterion	*/

   fint		n ;				/* counter		*/
   fint		nret ;				/* nr. els. returned	*/
   fint		opcode = 0 ;			/* opcode given by user	*/

   float	blank = 0 ;			/* blank value 		*/

   fchar	strings[ 2 ] ;			/* strings for user i/o	*/

   anyout_c( &test , tofchar( "Function JudgeFit" ) ) ;

   ListProgram( fitprog , pro_fit , "JudgeFit at start" ) ;

   for ( n = 0 ; n < 2 ; n++ ) {
      strings[ n ].a = &strbuf[ n * MAXTXTLEN ] ;
      strings[ n ].l = MAXTXTLEN ;
   }

   GetMode( mode ) ;				/* ask running mode	*/

   sprintf( mess ,
            "Give minimum acceptable signal-to-noise ratio [ %.2g ]" ,
            fitprog->fc[ pro_fit->curr ].sn ) ;
   nret = userreal_c( &fitprog->fc[ pro_fit->curr ].sn ,			/* get minimum S/N	*/
                      &one ,			/* only one value	*/
                      &one ,			/* has a default	*/
                       tofchar( "SN=" ) ,	/* keyword		*/
                       tofchar( mess ) ) ;	/* message		*/

   pro_fit->fits[ pro_fit->curr ].fitok = true ;
   if ( pro_fit->fits[ pro_fit->curr ].niter < 0 ) {
      pro_fit->fits[ pro_fit->curr ].fitok = false ;
   }

						/* do S/N test		*/
   if ( ( pro_fit->fits[ pro_fit->curr ].fitok ) &&
        ( fitprog->fc[ pro_fit->curr ].sn > 0 ) ) {
      switch ( fitprog->fc[ pro_fit->curr ].type ) {
         case GAUSS :
            for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ) {
               if ( fitprog->fc[ pro_fit->curr ].msk[ n * 3 + 1 ] != 0 ) {
                  pro_fit->fits[ pro_fit->curr ].fitok =
                     pro_fit->fits[ pro_fit->curr ].fitok &&
                      ( ( pro_fit->fits[ pro_fit->curr ].par[ n * 3 + 1 ] /
                          pro_fit->fits[ pro_fit->curr ].err[ n * 3 + 1 ] )
                                      > fitprog->fc[ pro_fit->curr ].sn ) ;
               }
               if ( fitprog->fc[ pro_fit->curr ].msk[ n * 3 + 3 ] != 0 ) {
                  pro_fit->fits[ pro_fit->curr ].fitok =
                     pro_fit->fits[ pro_fit->curr ].fitok &&
                      ( ( pro_fit->fits[ pro_fit->curr ].par[ n * 3 + 3 ] /
                          pro_fit->fits[ pro_fit->curr ].err[ n * 3 + 3 ] )
                                      > fitprog->fc[ pro_fit->curr ].sn ) ;
               }

            }
            break ;
         case VOIGT :
            for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ) {
               if ( fitprog->fc[ pro_fit->curr ].msk[ n * 4 + 1 ] != 0 ) {
                  pro_fit->fits[ pro_fit->curr ].fitok =
                     pro_fit->fits[ pro_fit->curr ].fitok &&
                      ( ( pro_fit->fits[ pro_fit->curr ].par[ n * 4 + 1 ] /
                          pro_fit->fits[ pro_fit->curr ].err[ n * 4 + 1 ] )
                                      > fitprog->fc[ pro_fit->curr ].sn ) ;
               }
               if ( fitprog->fc[ pro_fit->curr ].msk[ n * 4 + 3 ] != 0 ) {
                  pro_fit->fits[ pro_fit->curr ].fitok =
                     pro_fit->fits[ pro_fit->curr ].fitok &&
                      ( ( pro_fit->fits[ pro_fit->curr ].par[ n * 4 + 3 ] /
                          pro_fit->fits[ pro_fit->curr ].err[ n * 4 + 3 ] )
                                      > fitprog->fc[ pro_fit->curr ].sn ) ;
               }
               if ( fitprog->fc[ pro_fit->curr ].msk[ n * 4 + 4 ] != 0 ) {
                  pro_fit->fits[ pro_fit->curr ].fitok =
                     pro_fit->fits[ pro_fit->curr ].fitok &&
                      ( ( pro_fit->fits[ pro_fit->curr ].par[ n * 4 + 4 ] /
                          pro_fit->fits[ pro_fit->curr ].err[ n * 4 + 4 ] )
                                      > fitprog->fc[ pro_fit->curr ].sn ) ;
               }

            }
            break ;
         case POLY :
            for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].npar ; n++ ) {
                  pro_fit->fits[ pro_fit->curr ].fitok =
                     pro_fit->fits[ pro_fit->curr ].fitok &&
                      ( ( pro_fit->fits[ pro_fit->curr ].par[ n ] /
                          pro_fit->fits[ pro_fit->curr ].err[ n ] )
                                      > fitprog->fc[ pro_fit->curr ].sn ) ;
            }
            break ;
         default :
            break ;
      }
      sn_crit = !pro_fit->fits[ pro_fit->curr ].fitok ;
   }

   if ( pro_fit->fits[ pro_fit->curr ].fitok ) {
      for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].npar ; n++ ) {
         if ( ( pro_fit->fits[ pro_fit->curr ].par[ n ] >
                   fitprog->fc[ pro_fit->curr ].max[ n ] ) ||
              ( pro_fit->fits[ pro_fit->curr ].par[ n ] <
                   fitprog->fc[ pro_fit->curr ].min[ n ] ) ) {
            pro_fit->fits[ pro_fit->curr ].fitok  = false ;
            sprintf( mess , "Parameter %d, value %f not within range %f-%f" ,
                             n , pro_fit->fits[ pro_fit->curr ].par[ n ] ,
                             fitprog->fc[ pro_fit->curr ].min[ n ] ,
                             fitprog->fc[ pro_fit->curr ].max[ n ] ) ;
            anyout_c( &test , tofchar( mess ) ) ;
            break ;
         }
      }
      lim_crit = !pro_fit->fits[ pro_fit->curr ].fitok ;
   }

   if ( !mode->run ) {
      if ( fitprog->opcode[ pro_fit->curr ] == SUBTRACT ) {
         strcpy( opstring , "SUBTRACT" ) ;
      } else if ( fitprog->opcode[ pro_fit->curr ] == MORE ) {
         strcpy( opstring , "MORE" ) ;
      } else {
         fitprog->opcode[ pro_fit->curr ] = NEXT ;
         strcpy( opstring , "NEXT" ) ;
      }
      if ( pro_fit->fits[ pro_fit->curr ].fitok ) {
         anyout_c( &one , tofchar( "The fit looks GOOD, what next ?" ) );
         sprintf( mess ,
	          "GOOD/BAD and NEXT, AGAIN, MORE, SUBTRACT? [ GOOD,%s ]" ,
                   opstring ) ;
      } else {
         sprintf( mess , "" ) ;
         if ( sn_crit ) {
            sprintf( mess ,
              "Fit looks BAD, some parameters have S/N < %.2g, now what?" ,
               fitprog->fc[ pro_fit->curr ].sn ) ;
         }
         if ( lim_crit ) {
            sprintf( mess ,
              "Fit looks BAD, some parameters are out of limits, now what?" ) ;
         }
         anyout_c( &one , tofchar( mess ) );
         sprintf( mess ,
          "GOOD or BAD and NEXT, MORE or SUBTRACT? [ BAD,%s ]" , opstring ) ;
      }
      ok = false ;
      while( !ok ) {
         nret = usercharu_c(  strings[ 0 ] ,
                             &two ,
                             &one ,
                              tofchar( "JUDGE=" ) ,
                              tofchar( mess ) ) ;
         cancel_c( tofchar( "JUDGE=" ) ) ;
         sprintf( mess , "JUDGE returns %d strings:" , nret ) ;
         anyout_c( &test , tofchar( mess ) ) ;
         for ( n = 0 ; n < nret ; n++ ) {
            sprintf( mess , "  string %d is %.*s" , n ,
                            nelc_c( strings[ n ] ) , strings[ n ].a ) ;
            anyout_c( &test , tofchar( mess ) ) ;
         }
         if ( nret == 0 ) {
            ok = true ;
         } else {
            part1 = false ;
            part2 = false ;
            if ( ( nret == 1 ) || ( nret == 2 ) ) {
               if ( strings[ 0 ].a[ 0 ] == 'G' ) {
                  good_bad = true ;
                  part1 = true ;
               } else if ( strings[ 0 ].a[ 0 ] == 'B' ) {
                  good_bad = false ;
                  part1 = true ;
               } else if ( strings[ 0 ].a[ 0 ] == 'Q' ) {
                  mode->stop = true ;
                  return ;
               }
            }
            if ( nret == 2 ) {
               if ( strings[ 1 ].a[ 0 ] == 'N' ) {
                  opcode = NEXT ;
                  part2 = true ;
               } else if ( strings[ 1 ].a[ 0 ] == 'M' ) {
                  opcode = MORE ;
                  part2 = true ;
               } else if ( strings[ 1 ].a[ 0 ] == 'S' ) {
                  opcode = SUBTRACT ;
                  part2 = true ;
               } else if ( strings[ 1 ].a[ 0 ] == 'A' ) {
                  opcode = AGAIN ;
                  part2 = true ;
               }
            }
            if ( ( nret == 2 ) && part1 && part2 ) {
               pro_fit->fits[ pro_fit->curr ].fitok = good_bad ;
               fitprog->opcode[ pro_fit->curr ] = opcode ;
               ok = true ;
            } else if ( ( nret == 1 ) && part1 ) {
               pro_fit->fits[ pro_fit->curr ].fitok = good_bad ;
               fitprog->opcode[ pro_fit->curr ] = NEXT ;
               ok = true ;
            } else {
               if ( nret == 1 ) {
                  strings[ 1 ].a[ 0 ] = 0 ;
               }
               sprintf( emess , "Sorry, I don't understand JUDGE=%.*s,%.*s" ,
                                nelc_c( strings[ 0 ] ) , strings[ 0 ].a ,
                                nelc_c( strings[ 1 ] ) , strings[ 1 ].a ) ;
               error_c( &e_warning , tofchar( emess ) ) ;
            }
         }
      }
   }

   if (  pro_fit->fits[ pro_fit->curr ].fitok ) {
      for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].npar ; n++ ) {
         fitprog->fc[ pro_fit->curr ].guess[ n ] =
            pro_fit->fits[ pro_fit->curr ].par[ n ] ;
      }
      if ( fitprog->opcode[ pro_fit->curr ] == SUBTRACT ) {
         for ( n = 0 ; n < pro_fit->prof.n ; n++ ) {
            pro_fit->prof.y[ n ] = pro_fit->res.y[ n ] ;
         }
         pro_fit->prof.max = pro_fit->res.max ;
         pro_fit->prof.min = pro_fit->res.min ;
         Send_Status( ", subtract fit" , true ) ;
      } else {
         Send_Status( ", good fit" , true ) ;
      }
   } else {
      Send_Status( ", bad fit" , true ) ;
      anyout_c( &one , tofchar( "Bad fit, result set to undefined" ) ) ;
      setfblank_c( &blank ) ;
      for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].npar ; n++ ) {
         pro_fit->fits[ pro_fit->curr ].par[ n ] = blank ;
         pro_fit->fits[ pro_fit->curr ].err[ n ] = blank ;
      }
      for ( n = 0 ; n < pro_fit->prof.n ; n++ ) {
         pro_fit->mod.y[ n ] = 0 ;
         pro_fit->res.y[ n ] = pro_fit->prof.y[ n ] ;
      }
   }

   if ( !mode->run ) { 				/* not running mode yet	*/
      if ( mode->level == 1 ) {			/* interactive	mode	*/
         cancel_c( tofchar( "FIT=" ) ) ;	/* cancel the fit key	*/
         cancel_c( tofchar( "FIX=" ) ) ;	/* cancel FIXed pars.	*/
         cancel_c( tofchar( "WINDOW=" ) ) ;	/* cancel WINDOW key	*/
         if ( FUN[ fitprog->fc[ pro_fit->curr ].type ].zero != 0 ) {
            cancel_c( tofchar( "GUESS0=" ) ) ;	/* cancel zero guess	*/
         }
         for ( n = 0 ; n < fitprog->fc[ pro_fit->curr ].ncmp ; n++ ) {
            sprintf( mess , "GUESS%d=" , n + 1 ) ;
            cancel_c( tofchar( mess ) ) ;	/* cancel the guesses	*/
         }
      }
      if ( fitprog->opcode[ pro_fit->curr ] == NEXT ) {
         anyout_c( &test , tofchar( "Set number of fits to do" ) ) ;
         fitprog->nf   = pro_fit->curr + 1 ;
      } else if ( ( pro_fit->curr + 1 ) == fitprog->nf ) {
         anyout_c( &test , tofchar( "Increase number of fits to do" ) ) ;
         fitprog->nf   = fitprog->nf   + 1 ;
      }
   }

   if ( ( pro_fit->curr + 1 ) < fitprog->nf ) {
      anyout_c( &test , tofchar( "More fits left to do" ) ) ;
      if ( fitprog->opcode[ pro_fit->curr ] != AGAIN ) {
         pro_fit->curr = pro_fit->curr + 1 ;
      }
      mode->next    = false ;
   } else {
      anyout_c( &test , tofchar( "No more fits left to do" ) ) ;
      pro_fit->curr = 0 ;
      mode->next    = true ;
   }

   pro_fit->fits[ pro_fit->curr ].fitok = false ;
   pro_fit->fits[ pro_fit->curr ].niter = 0 ;
   pro_fit->fits[ pro_fit->curr ].rms   = 0 ;
   pro_fit->fits[ pro_fit->curr ].mean  = 0 ;

   ListProgram( fitprog , pro_fit , "JudgeFit at end" ) ;

   return ;
}						/* JudgeFit		*/

/*
   The function AskOutSet asks for an output set

*/
static void AskOutSet( Set        *inset  ,	/* ask output set	*/
                       FitProgram *fitprog,
                       fint	   type   ,
                       char       *key    ,
                       char       *usermes,
                       fint        naxis3 ,
                       Set        *outset   )
{
   bool		cont = false ;			/* continuation profit?	*/

   char		mess[ MAXTXTLEN ] ;		/* message buffer	*/
   char		axis_name[ MAXTXTLEN ] ;	/* axis name 		*/

   fchar	insetstr ;			/* inset string		*/

   fint		n = 0 ;				/* counter		*/
   fint		maxaxes = MAXDIM ;		/* mx nr. of dimensions	*/
   fint		maxsubs = MAXSUBS ;		/* max nr. of subsets	*/
   fint		axgrids[ MAXDIM ] ;		/* grids on axes	*/
   fint		showdev = 3 ;			/* user output device	*/
   fint		cwlo , cwhi ;			/* c-words		*/
   fint		buf_len = BUFSIZE ;		/* nr pix. in buffer	*/
   fint		ndone = 0 ;			/* nr. pix done		*/
   fint		tid ;				/* transfer ID		*/
   fint		error = 0 ;			/* error code		*/
   fint		nret = 0 ;			/* return value		*/

   float	buffer[ BUFSIZE ] ;		/* buffer with blanks	*/
   float	blank ;				/* blank value		*/
   float	fmin, fmax, fcur ;		/* for stabar		*/

   anyout_c( &test , tofchar( "Function AskOutSet" ) ) ;

   finit( outset->set , MAXTXTLEN ) ;		/* initialise setname	*/

   if ( type == MODEL || type == RESIDUAL ) {	/* model or residu	*/
      gdsasn_c(  tofchar( "INSET=" ) ,		/* associate SET	*/
                 tofchar( key ) ,		/*      with key	*/
                &one ) ;			/* class one		*/
   } else {					/* errors or params	*/
      gdsasn_c(  tofchar( "INSET=" ) ,		/* associate SET	*/
                 tofchar( key ) ,		/*      with key	*/
                &two ) ;			/* class two		*/
      if ( type == PARAMETERS ) {
         sprintf( axis_name , "PARAM-FITPAR" ) ;
      } else {
         sprintf( axis_name , "PARAM-FITERR" ) ;
      }
      n = 14 ;
      gdscpa_c(  tofchar( key ) ,		/* add parameter axis	*/
                &inset->setdim ,		/* output dim = input	*/
                &naxis3 ,			/* length in pixels	*/
                &dblone ,			/* CDELT		*/
                &dblzero ,			/* CROTA		*/
                &dblzero ,			/* CRPIX		*/
                &dblzero ,			/* CRVAL		*/
                 tofchar( axis_name ) ,		/* CTYPE		*/
                 tofchar( " " ) ,		/* CUNIT		*/
                &n ) ;
      sprintf( mess , "Did gdscpa for %s, %d pixels on axis %s" ,
                       key , naxis3 , axis_name ) ;
      anyout_c( &test , tofchar( mess ) ) ;
   }

   nret = userchar_c(  outset->set ,		/* get setname		*/
                      &one ,			/* only one set		*/
                      &hasdefault ,		/* default exists	*/
                       tofchar( key ) ,		/* the key		*/
                       tofchar( usermes ) ) ;	/* the message		*/

   if ( nret == 0 ) {				/* no output set	*/
      free( outset->set.a ) ;			/* free memory		*/
      outset->set.a = NULL ;			/* set to NULL		*/
   } else {
      outset->nss =
             gdsout_c(  outset->set ,		/* set name		*/
                        outset->ss ,		/* subset numbers	*/
                       &maxsubs ,		/* max nr. subsets	*/
                       &hasdefault ,		/* default is stop	*/
                        tofchar( key ) ,	/* keyword		*/
                        tofchar( usermes ) ,	/* message		*/
                       &showdev ,		/* output level		*/
                        outset->axperm ,	/* permutation of axes	*/
                        axgrids ,		/* nr. of pixels on ax	*/
                       &maxaxes ) ; 		/* maximum nr. of axes	*/

      sprintf( mess , "Got %d subsets for %s" , outset->nss , key ) ;
      anyout_c( &test , tofchar( mess ) ) ;

      nret = userlog_c( &cont ,
                        &one ,
                        &hidden ,
                         tofchar( "CONT=" ) ,
                         tofchar( "" ) ) ;

      if ( cont ){
         sprintf( mess , "Will add results to %.*s" ,
                          nelc_c( outset->set ) , outset->set.a ) ;
         anyout_c( &one , tofchar( mess ) ) ;
      } else if ( type != RESIDUAL ) {
         sprintf( mess , "Setting set %.*s to undefined" ,
                          nelc_c( outset->set ) , outset->set.a ) ;
         status_c( tofchar( mess ) ) ;
         outset->setdim = gdsc_ndims_c( outset->set , &zero ) ;
         setfblank_c( &blank ) ;		/* set blank value	*/
         presetr_c( &blank ,			/* put blank		*/
	             buffer ,			/* in buffer		*/
	            &buf_len ) ;		/* for all elements	*/
         fmin =    0 ;				/* for stabar		*/
         fmax = outset->nss ;
         for ( n = 0 ; n < outset->nss ; n++ ) {
            gdsc_range_c(  outset->set ,	/* get range of set	*/
                          &outset->ss[ n ] ,	/* for this subset	*/
                          &cwlo ,		/* lower left		*/
                          &cwhi ,		/* upper right		*/
                          &error ) ;		/* error return		*/
            ndone = 0 ;
            tid = 0 ;
            do {
   	       gdsi_write_c(  outset->set ,	/* write to output set	*/
   	      	             &cwlo ,		/* from lower left	*/
   		             &cwhi ,		/*   to upper right	*/
   		   	      buffer ,		/* the data buffer	*/
   		             &buf_len ,		/* of this length	*/
                             &ndone ,		/* this is done		*/
                             &tid ) ;		/* for this tid		*/
               if ( tid < 0 ) {
                  sprintf( mess , "Error %d setting %.*s to undefined" , tid ,
                                nelc_c( outset->set ) , outset->set.a ) ;
               }
            } while( tid > 0 ) ;		/* while not done	*/
            fcur = n ;
            stabar_c( &fmin , &fmax , &fcur ) ;	/* show status		*/
         }
      } else {
         finit( insetstr , MAXTXTLEN ) ;
         nret = usertext_c(  insetstr ,
                            &hidden ,
                             tofchar( "INSET=" ) ,
                             tofchar( "" ) ) ;
         for ( n = 0 ; n < nret ; n++ ) {
            if ( insetstr.a[ n ] == ' ' ) break ;
         }
         error = 0 ;
         sprintf( mess , "COPY OKAY=Y OUTSET=%.*s %.*s" ,
                         nelc_c( outset->set ) , outset->set.a ,
                         nelc_c( insetstr ) - n , &insetstr.a[ n ] ) ;
         anyout_c( &test , tofchar( mess ) ) ;
         deputy_c( tofchar( mess ) , &error ) ;
         if ( error ) {
            sprintf( mess , "DEPUTY COPY for RESSET= gave error %d" , error ) ;
            error_c( &e_warning , tofchar( mess ) ) ;
         }
         free( insetstr.a ) ;
      }
   }

   return ;
}						/* AskOutSet	 	*/

/*
   The function WriteData writes data to an output set

*/
static void WriteData( Set      *outset ,	/* write to output set	*/
                       fint	*pos    ,
                       float	*data   ,
                       fint      ndata    )
{
   char		mess[ MAXTXTLEN ] ;		/* text buffer		*/
   char		txtbuf[ MAXTXTLEN ] ;		/* text buffer		*/

   fint		n = 0 ;				/* counter		*/
   fint		cwlo = 0 , cwhi = 0 ;		/* coord. words		*/
   fint		ndone = 0 ;			/* nr. pixels done	*/
   fint		tid = 0 ;			/* transfer ID		*/

   anyout_c( &test , tofchar( "Function WriteData" ) ) ;

   cwlo = gdsc_fill_c( outset->set , &outset->ss[ 0 ] , pos ) ;
   cwhi = gdsc_fill_c( outset->set , &outset->ss[ outset->nss - 1 ]  , pos ) ;

   sprintf( mess , "Writing %d points to %.*s, at (" ,
                    ndata , nelc_c( outset->set ) , outset->set.a ) ;
   for ( n = 0 ; n < outset->setdim - 1 ; n++ ) {
      sprintf( txtbuf , "%d" , pos[ n ] ) ;
      if ( n < outset->setdim - 2 ) strcat( txtbuf , "," ) ;
      strcat( mess , txtbuf ) ;
   }
   sprintf( txtbuf , ") cw %d to %d" , cwlo , cwhi ) ;
   strcat( mess , txtbuf ) ;
   anyout_c( &test , tofchar( mess ) ) ;

   gdsi_write_c(  outset->set ,			/* write to set		*/
                 &cwlo ,			/* from low		*/
                 &cwhi ,			/*      to hi 		*/
                  data ,			/* the data		*/
                 &ndata ,			/* all of them		*/
                 &ndone ,			/* should be all	*/
                 &tid   ) ;			/* transfer id		*/

   sprintf( mess , "Wrote %d points to %.*s, tid %d" ,
                    ndone , nelc_c( outset->set ) , outset->set.a ,
                    tid ) ;
   anyout_c( &test , tofchar( mess ) ) ;

   return ;
}						/* WriteData	 	*/


/*
   The function WriteResults writes the fit parameters (and errors) to an
output set.

*/
static void WriteResults( RunMode    *mode    ,	/* write results	*/
                          ProfileFit *pro_fit ,
                          FitProgram *fitprog ,
                          Set        *inset   ,
                          OutSets    *outsets   )
{
   fint		n , m ;				/* counters		*/
   fint		npars = 0 ;			/* total nr. of params.	*/

   float	save[ MAXPAR * MAXFITS ] ;	/* buffer array		*/

   anyout_c( &test , tofchar( "Function WriteResults" ) ) ;

/*   if ( !mode->run ) return ;*/			/* still programming	*/

   if ( !outsets->asked ) {
      for ( n = 0 ; n < fitprog->nf ; n++ ) {
         npars = npars + fitprog->fc[ n ].npar ;
      }
      AskOutSet(  inset ,
                  fitprog ,
                  PARAMETERS ,
                  "PARSET="    ,
                  "Give output setname for fit parameters [not saved]" ,
                  npars ,
                 &outsets->par   ) ;
      AskOutSet(  inset ,
                  fitprog ,
                  ERRORS ,
                  "ERRSET="    ,
                  "Give output setname for fit errors [not saved]" ,
                  npars ,
                 &outsets->err   ) ;
      AskOutSet(  inset ,
                  fitprog ,
                  MODEL ,
                  "MODSET="    ,
                  "Give output setname for fit model [not saved]" ,
                  pro_fit->mod.n ,
                 &outsets->mod   ) ;
      AskOutSet(  inset ,
                  fitprog ,
                  RESIDUAL ,
                  "RESSET="      ,
                  "Give output setname for fit residuals [not saved]" ,
                  pro_fit->res.n ,
                 &outsets->res   ) ;
      outsets->asked = true ;			/* now they were asked	*/
   }

   if ( outsets->par.set.a != NULL ) {
      npars = 0 ;
      for ( n = 0 ; n < fitprog->nf ; n++ ) {
         for ( m = 0 ; m < fitprog->fc[ n ].npar ; m++ ) {
            save[ npars++ ] = pro_fit->fits[ n ].par[ m ] ;
         }
      }
      WriteData( &outsets->par , pro_fit->prof.pos , save , npars ) ;
   }

   if ( outsets->err.set.a != NULL ) {
      npars = 0 ;
      for ( n = 0 ; n < fitprog->nf ; n++ ) {
         for ( m = 0 ; m < fitprog->fc[ n ].npar ; m++ ) {
            save[ npars++ ] = pro_fit->fits[ n ].err[ m ] ;
         }
      }
      WriteData( &outsets->err , pro_fit->prof.pos , save , npars ) ;
   }

   if ( outsets->mod.set.a != NULL ) {
      WriteData( &outsets->mod   , pro_fit->prof.pos ,
                  pro_fit->mod.y , pro_fit->mod.n ) ;
   }

   if ( outsets->res.set.a != NULL ) {
      WriteData( &outsets->res   , pro_fit->prof.pos ,
                  pro_fit->res.y , pro_fit->res.n ) ;
   }

   return ;
}						/* WriteResults 	*/



/*
   The function NextGrid increases the pixel position of the current
profile. For that it iteratively tries to increase the grid value
along an axis, if the current axix cannot be increased anymore the
following axis is tried until no more axes are left.

*/
static bool NextGrid( fint *blo   ,		/* begin coordinate	*/
                      fint *bhi   ,		/* end coordinate	*/
                      fint *inc   ,		/* dir. incr. in coords.*/
                      fint *pos   ,		/* current coord.	*/
                      fint level  , 		/* level for change	*/
                      fint maxlev  )		/* maximum level	*/
{
   bool		done = true ;			/* are we done?		*/

   fint		n = 0 ;				/* index for arrays	*/

   if ( level > maxlev ) { 			/* beyond last axis =>	*/
      return( true ) ;				/* done and return	*/
   }

   n = level - 1 ;				/* Coo-array index	*/

   if ( ( inc[ n ] == 0 ) ||			/* no inc. for axis	*/
        ( ( inc[ n ] ==  1 ) && 		/* inc. this axis	*/
          ( pos[ n ] == bhi[ n ] ) ) ||		/*   but and at end	*/
        ( ( inc[ n ] == -1 ) && 		/* decr. this axis	*/
          ( pos[ n ] == blo[ n ] ) )   ) {	/*   but and at end	*/
      inc[ n ] = -inc[ n ] ;			/* change direction	*/
						/* increment next level	*/
      done = NextGrid( blo , bhi , inc , pos , level + 1 , maxlev ) ;
   } else {					/* incr. this axis	*/
      pos[ n ] = pos[ n ] + inc[ n ] ;		/* do the increment	*/
      done = false ;				/* more left to do	*/
   }

   return( done ) ;				/* return result	*/
}						/* NextGrid		*/

/*
   The function InMask determines whether a given position is within the
mask a set in the maskset.

*/
static bool InMask( fint    *pos     ,
                    MaskSet *maskset ,
                    Set     *inset   ,
                    SetInfo *setinfo )
{
   char		mess[ MAXTXTLEN ] ;		/* message buffer	*/
   char		buff[ MAXTXTLEN ] ;		/* text buffer		*/

   bool		inmask = true ;			/* profile in mask?	*/
   bool		ok ;				/* good input set?	*/

   fint		nret ;				/* return nr. items	*/
   fint		axgrids[ MAXDIM ] ;		/* nr pixels along ax	*/
   fint		axperm[ MAXDIM ] ;		/* axis permutations	*/
   fint		naxis ;				/* nr. pixels on axis	*/
   fint		cwd  ;				/* coord. word		*/
   fint		tid = 0 ;			/* transfer id		*/
   fint 	n = 0 , m ; 			/* counters		*/

   float	data ;				/* data buffer		*/

   anyout_c( &test , tofchar( "Function InMask" ) ) ;

   if ( !maskset->asked ) {			/* no mask set asked	*/
      maskset->asked = true ;

      sprintf( mess , "Give cutoff level for mask map [ no mask ]" ) ;
      nret = userreal_c( &maskset->cut ,	/* ask cutoff in mask	*/
                         &one          ,	/* one real only	*/
                         &hasdefault   ,	/* default exists       */
                          tofchar( "CUTM=" ) ,	/* keyword		*/
                          tofchar( mess ) ) ;	/* and message		*/

      if ( nret == 0 ) {			/* nothing given	*/
         maskset->set.a = NULL ;
         return( true ) ;
      }

      finit( maskset->set , MAXTXTLEN ) ;	/* initialise fchar	*/
      do {					/* loop to get maskset	*/
         naxis = MAXDIM ;
         maskset->setdim = inset->setdim - 1 ;
         gdsinp_c(  maskset->set ,		/* get mask set		*/
                   &maskset->ss  ,		/* subset		*/
                   &one          ,		/* one subset		*/
                   &zero         ,		/* no default		*/
                    tofchar( "MASKSET=" ) ,	/* keyword		*/
                    tofchar( "Give set to use as mask" ) ,
                   &three        ,		/* show stuff		*/
                    axperm       ,		/* axis permutations	*/
                    axgrids      ,		/* nr grids		*/
                   &naxis        ,		/* maximum dimensions	*/
                   &one          ,		/* class one 		*/
                   &maskset->setdim ) ;		/* dimension of subset	*/
         ok = maskset->setdim == inset->setdim - 1 ;	/* dim. OK?	*/
         if ( !ok ) {
            error_c( &e_warning ,
                     tofchar( "Wrong mask subset dimensions, try again" ) ) ;
            break ;
         }
         for ( n = 0 ; n < maskset->setdim ; n++ ) {
            maskset->reppos[ n ] = inset->blo[ n ] ;
            ok = ok && ( axperm[ n ] == inset->axperm[ n ] ) ;
            if ( !ok ) {
               sprintf( mess , "Mask axis %d differs from input set" , n ) ;
               error_c( &e_warning , tofchar( mess ) ) ;
               break ;
            }
         }
      } while ( !ok ) ;

   }

   if ( maskset->set.a == NULL ) 		/* no mask set used	*/
      return( true ) ;				/* exit			*/

   cwd = gdsc_fill_c(  maskset->set ,		/* get grid cwd.	*/
                      &maskset->ss  ,		/* for subset		*/
                       pos          ) ;		/* for position		*/
   gdsi_read_c(  maskset->set  ,		/* read from mask set	*/
                &cwd           ,		/* from lower		*/
                &cwd           ,		/*      to upper	*/
                &data          ,		/* into data buffer	*/
                &one	       ,		/* read one  pixel	*/
                &n	       ,		/* this is done		*/
                &tid           ) ;		/* transfer ID		*/

   sprintf( mess , "Give cutoff level for mask set %,*s [ %8.2g ]" ,
            nelc_c( maskset->set ) , maskset->set.a , maskset->cut ) ;
   nret = userreal_c( &maskset->cut ,		/* ask cutoff in mask	*/
                      &one          ,		/* one real only	*/
                      &hasdefault   ,		/* default exists       */
                       tofchar( "CUTM=" ) ,	/* keyword		*/
                       tofchar( mess ) ) ;	/* and message		*/
   inmask = data > maskset->cut ;

   if ( !inmask ) {				/* profile rejected	*/
      ok = false ;
      for ( n = 0 ; n < maskset->setdim ; n++ ) {
         ok = ok || ( fabs( maskset->reppos[ n ] - pos[ n ] ) > 25 ) ;
      }
      if ( ok ) {
         sprintf( mess , "Set %.*s, skipped (" ,
              nelc_c( inset->set ) , inset->set.a ) ;
         for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
            m = nelc_c( setinfo->axname[ n ] ) > 3 ?
                         3 : nelc_c( setinfo->axname[ n ] ) ;
            sprintf( buff , "%.*s\0" , m , setinfo->axname[ n ].a ) ;
            strcat( mess , buff ) ;
            if ( n < inset->setdim - 2 ) strcat( mess , "," ) ;
         }
         strcat( mess , ")=(" ) ;
         for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
            sprintf( buff , "%d\0" , pos[ n ] ) ;
            strcat( mess , buff ) ;
            if ( n < inset->setdim - 2 ) strcat( mess , "," ) ;
            maskset->reppos[ n ] = pos[ n ] ;
         }
         sprintf( buff , "), mask set <%6.3g %.*s" , maskset->cut ,
                          nelc_c( setinfo->yunit ) , setinfo->yunit.a ) ;
         strcat( mess , buff ) ;
         Send_Status( mess , false ) ;
      }
   }

   return( inmask ) ;

} 						/* InMask		*/



/*
   The function NextProfile sets the profile position for the next
profile to be analyzed.

*/
static void NextProfile( Set        *inset   ,
                         SetInfo    *setinfo ,
                         MaskSet    *maskset ,
                         ProfileFit *pro_fit ,
                         RunMode    *mode    )	/* read a profile	*/
{
   char		mess[ MAXTXTLEN ] ;		/* text string		*/
   char		txtbuf[ MAXTXTLEN ] ;		/* text string		*/

   fchar	string ;			/* text string		*/

   fint		deflev ;			/* defualt level 	*/
   fint		n = 0 , m = 0 ;			/* counters		*/
   fint		nret = 0 ;			/* gdspos return	*/
   fint         newpos[ MAXDIM ] ;		/* new position		*/

   double	pos[ MAXDIM ] ;			/* position array	*/

   anyout_c( &test , tofchar( "Function NextProfile" ) ) ;

   finit( string , 80 ) ;			/* initialise fchar	*/

   if ( mode->stop ) return ;			/* stop ? => do nothing	*/

   for ( n = 0 ; n < MAXDIM ; n++ )
      newpos[ n ] = pro_fit->prof.pos[ n ] ;

   do {						/* loop to get new grid	*/
						/* set new grid		*/
      mode->stop = NextGrid( inset->blo     ,	/*   between lo		*/
                             inset->bhi     ,	/*   and hi		*/
                             inset->inc     ,	/*   in inc. direction	*/
                             newpos         ,	/*   from this pos	*/
                             1              ,	/*   from first axis	*/
                             inset->setdim  ) ;	/* to set dimension	*/
      GetMode( mode ) ;
   } while ( !mode->stop && !InMask( newpos, maskset, inset, setinfo ) )  ;

   if ( mode->stop ) return ;			/* quit => exit function*/
   
   sprintf( mess , "Give position for profile in (" ) ;
   for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
      m = nelc_c( setinfo->axname[ n ] ) > 3 ?
             3 : nelc_c( setinfo->axname[ n ] ) ;
      sprintf( txtbuf , "%.*s" , m , setinfo->axname[ n ].a ) ;
      strcat( mess , txtbuf ) ;
      if ( n < inset->setdim - 2 ) strcat( mess , "," ) ;
   }
   strcat( mess , ") [ (" ) ;
   for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
      sprintf( txtbuf , "%d" , newpos[ n ] ) ;
      strcat( mess , txtbuf ) ;
      if ( n < inset->setdim - 2 ) strcat( mess , "," ) ;
   }
   strcat( mess , ") ]" ) ;

   if ( mode->run ) {				/* running mode?	*/
      deflev = hidden ;				/* POS is hidden	*/
   } else {
      deflev = hasdefault ;			/* POS is not hidden	*/
   }

   nret = usertext_c( string ,			/* get string from user	*/
                      &deflev ,			/* default level	*/
                      tofchar( "POS=" ) ,	/* keyword		*/
                      tofchar( mess ) ) ;	/* message		*/
   mode->stop = ( toupper( string.a[ 0 ] ) == 'Q' ) ;
   if ( mode->stop ) {				/* user gave quit 	*/
      cancel_c( tofchar( "POS=" ) ) ;
      return ;
   }

   for ( n = 0 ; n < inset->setdim ; n++ ) {	/* set default pos.	*/
      pos[ n ] = (double) newpos[ n ] ;
   }
   nret = gdspos_c( pos ,			/* get position		*/
                    &one ,			/* only one		*/
                    &deflev ,			/* default level	*/
		    tofchar( "POS=" ) ,		/* keyword		*/
		    tofchar( mess ) ,		/* message		*/
                    inset->set ,		/* inset name		*/
                    &inset->ss[ 0 ] ) ;		/* first subset		*/

   for ( n = 0 ; n < inset->setdim - 1 ; n++ ) {
      pro_fit->prof.pos[ n ] = (int) pos[ n ] ;	/* put pos. in profile	*/
   }

   pro_fit->curr = 0 ;				/* reset fit number	*/
   pro_fit->done = false ;

   cancel_c( tofchar( "POS=" ) ) ;		/* cancel POS=		*/
   free( string.a ) ;

   return ;
}						/* NextProfile		*/




/*
   The function CleaunUp cleans up the remaining mess; close plotfiles,
reset keywords etc. etc.

*/
static void CleanUp( PlotSetup  *plotsetup ,
                     OutSets    *outsets   ,
                     RunMode    *mode      )	/* clean up the mess	*/
{
   char		mess[ MAXTXTLEN ] ;		/* message string	*/

   fint		error = 0 ;			/* error code		*/

   anyout_c( &test , tofchar( "CleanUp" ) ) ;

   status_c( tofchar( "Cleaning up the mess" ) ) ;

   if ( plotsetup->device.open ) {		/* plot device is open	*/
      pgiden_c( ) ;				/* put ID on plot	*/
      pgend_c( ) ;				/* close last plot	*/
      plotsetup->device.open = false ;		/* now it's closed	*/
   }

   if ( outsets->par.set.a != NULL ) {
      error = 0 ;
      subst_c( tofchar( "INSET=PARSET=" ) , &error ) ;
      deputy_c( tofchar( "MNMX" ) , &error ) ;
      if ( error ) {
         sprintf( mess , "DEPUTY MNMX for PARSET= gave error %d" , error ) ;
         error_c( &e_warning , tofchar( mess ) ) ;
      }
   }

   if ( outsets->err.set.a != NULL ) {
      error = 0 ;
      subst_c( tofchar( "INSET=ERRSET=" ) , &error ) ;
      deputy_c( tofchar( "MNMX" ) , &error ) ;
      if ( error ) {
         sprintf( mess , "DEPUTY MNMX for ERRSET= gave error %d" , error ) ;
         error_c( &e_warning , tofchar( mess ) ) ;
      }
   }

   if ( outsets->mod.set.a != NULL ) {
      error = 0 ;
      subst_c( tofchar( "INSET=MODSET=" ) , &error ) ;
      deputy_c( tofchar( "MNMX" ) , &error ) ;
      if ( error ) {
         sprintf( mess , "DEPUTY MNMX for MODSET= gave error %d" , error ) ;
         error_c( &e_warning , tofchar( mess ) ) ;
      }
   }

   if ( outsets->res.set.a != NULL ) {
      error = 0 ;
      subst_c( tofchar( "INSET=RESSET=" ) , &error ) ;
      deputy_c( tofchar( "MNMX" ) , &error ) ;
      if ( error ) {
         sprintf( mess , "DEPUTY MNMX for RESSET= gave error %d" , error ) ;
         error_c( &e_warning , tofchar( mess ) ) ;
      }
   }

   cancel_c( tofchar( "STOP=" ) ) ;		/* cancel stop keyword	*/
   cancel_c( tofchar( "MODE=" ) ) ;		/* cancel mode keyword	*/

   return ;
}						/* CleanUp		*/





MAIN_PROGRAM_ENTRY
{
   RunMode	mode = MODE_INIT ;		/* running mode		*/
   Set		inset ;				/* input dataset	*/
   OutSets	outsets ;			/* uotput data sets	*/
   SetInfo	setinfo ;			/* info input set	*/
   MaskSet	maskset ;			/* set fro masking	*/
   PlotSetup	plotsetup = PLOT_SETUP ;	/* plotting setup 	*/
   ProfileFit	pro_fit ;			/* profile being fitted	*/
   FitProgram	fitprog ;			/* program being done	*/



   init_c( ); 					/* hello HERMES 	*/
   IDENTIFICATION( PROGRAM , VERSION ) ;

   /* Start the new version? */
   {
      fint   r;
      fint   dfault = 1;
      fint   nitems = 1;
      bool   new = toflog( 1 );
      
      r = userlog_c( &new, &nitems, &dfault, tofchar("STARTNEW="), 
          tofchar("Start improved version of this program?           [Y]/N") );
      new = tobool( new );
      if (new)
      {
         fint  dev = 3;
         r = 0;
         anyout_c( &dev, tofchar("PROFIT called XGAUPROF") );
         deputy_c( tofchar("XGAUPROF") , &r );  /* Start new task */
         finis_c();                             /* Quit Hermes */          
      }
   }   


   outsets.asked = false ;
   maskset.asked = false ;

/*   GetMode( &mode ) ;				 ask running mode	*/

   mode.stop = GetInputSet( &inset ) ; 		/* ask set/area specs.	*/
   if ( mode.stop ) {				/* stop	immediately	*/
      CleanUp( &plotsetup , &outsets , &mode ) ;/* cleanup		*/
      finis_c( );				/* bye, bye HERMES 	*/
      return( 0 ) ;
   }

   GetSetInfo( &inset , &setinfo ) ;		/* get set info		*/

   FirstProfile( &inset , 			/* get first prof. pos.	*/
                 &setinfo ,
                 &pro_fit ,
                 &fitprog ) ;

   while( !mode.stop ) {			/* start loop on data	*/
      GetProfile( &inset   , 			/* from inset:		*/
                  &setinfo ,			/* use setinfo		*/
                  &pro_fit ) ;			/* read a profile	*/
      PlotFrame( &pro_fit   ,			/* plot the frame	*/
                 &plotsetup ,			/* use plotsetup	*/
                 &inset     , 			/* from inset		*/
                 &setinfo   ,			/*  and set information	*/
                 &mode      ) ;			/*  and mode		*/
      PlotProfile( &pro_fit.prof ,		/* plot the profile	*/
                    DATA         ,		/* these are data	*/
                   &mode         ) ;		/* use mode		*/

      do {					/* do fit until good 	*/
         GetMask(     &mode    ,		/* ask fit mask.	*/
                      &fitprog ,
                      &pro_fit ) ;
         AskWhatFit(  &mode    ,		/* ask fit function.	*/
                      &fitprog ,
                      &pro_fit ) ;
         GetGuess(    &mode    ,		/* ask fit guesses.	*/
                      &fitprog ,
                      &pro_fit ) ;
         FixGuess(    &mode    ,		/* ask fixed guesses.	*/
                      &fitprog ,
                      &pro_fit ) ;
         SetLimit(    &mode    ,		/* ask limits on fit.	*/
                      &fitprog ,
                      &pro_fit ) ;
         FitProfile(  &mode    ,		/* fit the profile	*/
                      &fitprog ,
                      &pro_fit ) ;
         MakeModel(   &mode    ,		/* make model profile	*/
                      &fitprog ,
                      &pro_fit ) ;
         ShowResults( &pro_fit ,		/* show the results	*/
                      &fitprog ,
                      &setinfo ,
                      &inset   ,
                      &mode    ) ;		/* use the mode		*/
         JudgeFit(    &pro_fit , 		/* Judge the fit	*/
                      &fitprog ,
                      &mode    ) ;

         if ( ( !mode.stop ) &&
              ( pro_fit.curr > 0 ) &&
              ( ( fitprog.opcode[ pro_fit.curr - 1 ] == MORE ) ||
                ( fitprog.opcode[ pro_fit.curr - 1 ] == AGAIN ) ||
                ( fitprog.opcode[ pro_fit.curr - 1 ] == SUBTRACT ) ) ) {
            Send_Status( stat_line , false ) ;
            Send_Status( " residual" , true ) ;
            PlotFrame( &pro_fit   ,		/* plot the frame	*/
                       &plotsetup ,		/* use plotsetup	*/
                       &inset     , 		/* from inset		*/
                       &setinfo   ,		/*  and set information	*/
                       &mode      ) ;		/*  and mode		*/
            PlotProfile( &pro_fit.prof ,	/* plot the profile	*/
                          DATA         ,	/* these are data	*/
                         &mode         ) ;	/* use mode		*/
         }

      } while( !mode.next &&			/* ready with profile ?	*/
               !mode.stop    ) ;		/* ready fitting ?	*/

      WriteResults( &mode    ,			/* write results	*/
                    &pro_fit ,
                    &fitprog ,
                    &inset   ,
                    &outsets ) ;
      NextProfile( &inset   ,			/* for inset:		*/
                   &setinfo ,			/*  use setinfo		*/
                   &maskset ,			/*  and maskset		*/
                   &pro_fit ,			/*  get next prof. pos.	*/
                   &mode    ) ;			/*  use mode		*/
   }						/* end loop on data	*/

   CleanUp( &plotsetup , &outsets , &mode ) ;	/* cleanup		*/
   finis_c( );					/* bye, bye HERMES 	*/
   return( 0 ) ;
}						/* 	DONE!!!		*/
