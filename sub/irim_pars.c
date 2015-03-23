/* irim_pars.c

           Copyright (c) 1992
  Laboratory for Space Research Groningen 
       Kapteyn Laboratory Groningen 
           All Rights Reserved.

**************************************************************************
#>   irim_getpars.dc2
Function:     irim_getpars

Purpose:      read destripe parameters from irds at (snip,sdet)

Category:     IRAS

Author:       Do Kester		

Use:  
int irim_getpars( 
	fchar		irds,	  IN: name of irds
	fint		snip,     IN: sequential snip numbers
	fint		sdet,	  IN: sequential detector number
	float		*cal,     OU: parameters; BASE, DRIFT, GAIN
	fint		nc,       IN: maximum allowable number of params
	float		*scale )  OU: stddev pertaining to this fit

returns: >= 0 : number of destripe parameters found 
	 -1   : no parameters present where they should be.

Comment:      This routine is not callable in Fortran

Updates:      	09 Jun 1992: DK, Creation date
		28 Jan 1993: DK, testoutput also includes gain
                15 Apr 1994: JPT, call gdst_abslevel_c instead of gds___abslev_c
              
#<
*/

#include "gipsyc.h"
#include "userfio.h"
#include "irim_pars.h"
#include "gdsd_grreal.h"
#include "gdsc_word.h"
#include "gdsd_wreal.h"
#include "gdsd_rreal.h"
#include "gdsd_rchar.h"
#include "gdsd_delall.h"
#include "gds_tune.h"
#include "nelc.h"

#define NO_PARAMS	-1
#define MAXCAL		3
#define DEFAULT_SIGMA	1000.0

#define GDSD_GRREAL( a, b, c, d, e, f, g ) \
	gdsd_grreal_c( (a), tofchar(b), (c), (d), &(e), (f), (g) ) 
#define GDSD_WREAL( a, b, c, d, e ) \
	gdsd_wreal_c( (a), tofchar(b), &(c), &(d), (e) ) 
#define GDSD_RREAL( a, b, c, d, e ) \
	gdsd_rreal_c( (a), tofchar(b), &(c),  (d), (e) ) 
#define GDSD_RCHAR( a, b, c, d, e ) \
	gdsd_rchar_c( (a), tofchar(b), &(c),  (d), (e) ) 

static char	*str_key[] = 
	{  "S_SIGMA ", "S_BASE  ", "S_DRIFT ", "S_GAIN  " } ;
static int	new = TRUE, keep = FALSE ;

int irim_getpars( 
	fchar		irds,
	fint		snip,
	fint		sdet,
	float		*cal,
	fint		nc,
	float		*scale )
{
	fint		i, nr, error = 0, axes[2], grid[2] ;
	fint		yes = TRUE, no = FALSE ;

	*scale = 0.0 ;
	for ( i = 0 ; i < nc ; i++ ) cal[i] = 0.0 ;
	i = 0 ;
	if ( ! new ) {
	    gdst_abslevel_c( &yes ) ;		/* search only locally */
	    axes[0] = 4 ; grid[0] = snip ;
	    axes[1] = 3 ; grid[1] = sdet ;
	    nr = 2 ;
	    GDSD_GRREAL( irds, str_key[0], axes, grid, nr, scale, &error ) ;
	    if ( error < 0 ) return( NO_PARAMS ) ;
	    for ( i = 0 ; i < MAXCAL ; i++ ) {
		error = 0 ;
		GDSD_GRREAL( irds, str_key[i+1], axes, grid, nr, 
			&(cal[i]), &error ) ;
		if ( error < 0 ) break ;
	    }
	    gdst_abslevel_c( &no ) ;		/* restore global searching */
	    anyoutf( ANYOUT_TST, 
		"get %2d %2d: nc %d %d  sigma %8.3f  cal %8.3f  %8.3f %8.3f",
		snip, sdet, nc, i, *scale, cal[0], cal[1], cal[2] ) ;
	}
	return( i ) ;
}

/*
**************************************************************************
#>   irim_putpars.dc2
Function:     irim_putpars

Purpose:      write destripe parameters from irds at (snip,sdet)

Category:     IRAS

Author:       Do Kester		

Use:  
int irim_putpars( 
	fchar		irds,	  IN: name of irds
	fint		snip,     IN: sequential snip numbers
	fint		sdet,	  IN: sequential detector number
	float		*cal,     IN: parameters; BASE, DRIFT, GAIN
	fint		nc,       IN: number of params present
	float		scale )   IN: stddev belonging with this fit

returns: >= 0 : number of destripe parameters being written
	 < 0  : error writing

Comment:      This routine is not callable in Fortran

Updates:        09 Jun 1992: DK, Creation date
		28 Jan 1993: DK, delete keys which are not written &&
				 include gain in testoutput.

#<
*/

int irim_putpars( 
	fchar		irds,
	fint		snip,
	fint		sdet,
	float		*cal,
	fint		nc,
	float		scale )
{
	fint	i, axis, error = 0, level = 0 ;

	if ( nc < 0 ) return( 0 ) ;
	axis = 4 ;
	level = gdsc_word_c( irds, &axis, &snip, &level, &error ) ;
	axis = 3 ;
	level = gdsc_word_c( irds, &axis, &sdet, &level, &error ) ;
	GDSD_WREAL( irds, str_key[0], level, scale, &error ) ;
	if ( error < 0 ) return( error ) ;
	for ( i = 1 ; i <= nc ; i++ ) {
		error = 0 ;
		GDSD_WREAL( irds, str_key[i], level, cal[i-1], &error ) ;
		if ( error < 0 ) return( error ) ;
	}
	for ( ; i <= MAXCAL ; i++ ) {
		error = 0 ;
		gdsd_delete_c( irds, tofchar( str_key[i] ), &level, &error ) ;
	}
	anyoutf( ANYOUT_TST, 
		"put %2d %2d: nc %d %d  sigma %8.3f  cal %8.3f  %8.3f %8.3f",
		snip, sdet, nc, i, scale, cal[0], cal[1], cal[2] ) ;
	return( i ) ;
}

/*
**************************************************************************
#>   irim_initpars.dc2
Function:     irim_initpars

Purpose:      Start reading params from the irds

Category:     IRAS

Author:       Do Kester		

Use:  
void irim_initpars( )

Comment:      This routine is not callable in Fortran

Updates:      09 Jun 1992: DK, Creation date

#<
*/

void irim_initpars( )
{
	new = FALSE ;
}

/*
**************************************************************************
#>   irim_userpars.dc2
Function:     irim_userpars

Purpose:      The user is prodded for the status of the destripe params

Category:     IRAS

Author:       Do Kester		

Use:  
void irim_userpars( )

Description:
	The internal variables `new' and `keep' are set according 
	the wishes of the user.
	The hidden keyword PARSTATUS is used which can take two values: 
	one for the initial status from the set [old,new] and 
	one for the final status from the set [keep,delete].
	The meaning is
	  old :   there should be params from a previous session upon which
	  	  should be continued.
	  new :   start a new session irrespective of previous sessions,
		  whose params are silently deleted.
	  keep:   keep the destripe params in the irds for further use.
	  delete: delete the destripe params at all levels.

	The first letter suffices and the order is irrelevant.

Comment:      This routine is not callable in Fortran

Updates:      09 Jun 1992: DK, Creation date

#<
*/

void irim_userpars( )
{
	static char	stat[21]  ;
	static fchar	parstat = { stat, 10 } ;
	int		i, nitem, pnew = TRUE ;

	nitem = userfcharl( parstat, 2, DFLT_HIDD, "PARSTATUS=", 
		"Start and end status of destripe params [new,delete]" ) ;
	i = 0 ;
	while( nitem-- ) {
	    switch ( stat[i] ) {
	    case 'o' : pnew = FALSE ; break ;
	    case 'n' : pnew = TRUE  ; break ;
	    case 'k' : keep = TRUE  ; break ;
	    case 'd' : keep = FALSE ; break ;
	    default  : 
	    	errorf( WARNING, "Unrecognized status: %.10s", &(stat[i]) ) ;
		break ;
	    }
	    i += 10 ;
	}
	anyoutf( ANYOUT_TST, "pnew = %d, keep = %d, new = %d", 
				pnew, keep, new ) ;		    
	new = new && pnew ;
	return ;
}

/*
**************************************************************************
#>   irim_getsigma.dc2
Function:     irim_getsigma

Purpose:      read standard deviation from top level of irds

Category:     IRAS

Author:       Do Kester		

Use:  
int irim_getsigma( 
	fchar		irds,	   IN: name of irds
	float		*sigmap )  IN: overall stddev of stripe fits

Returns: 	>0  : level at which it was found
		<-1 : could not be found where is should be

Description:
	`sigmap' is read from the toplevel.

Comment:      This routine is not callable in Fortran

Updates:      09 Jun 1992: DK, Creation date

#<
*/

int irim_getsigma( 
	fchar	irds,
	float	* sigmap )
{
	static char	bun[21] ;
	static fchar	bunit = { bun, 20 } ;
	fint		i, error = 0, level = 0 ;

/* set default value for sigma; scale it in case of W/m2 */
	*sigmap = DEFAULT_SIGMA ;
	GDSD_RCHAR( irds, "BUNIT", level, bunit, &error ) ;
	i = 0 ;
	while ( bun[i] == ' ' ) i++ ;
	if ( bun[i] == 'W' ) *sigmap *= 1.0e-12 ; 

	if ( ! new ) {
	    GDSD_RREAL( irds, str_key[0], level, sigmap, &error ) ;
	    if ( error < 0 ) {
	        errorf( WARNING, "IRDS %.*s contains no destripe params",
		    	nelc_c( irds ), irds ) ;
	    }
	}
	anyoutf( ANYOUT_TST, "sigma of map is %f", *sigmap ) ;
	return( error ) ;
}

/*
**************************************************************************
#>   irim_putsigma.dc2
Function:     irim_putsigma

Purpose:      write standard deviation at top level of irds

Category:     IRAS

Author:       Do Kester		

Use:  
int irim_putsigma( 
	fchar		irds,	  IN: name of irds
	float		sigmap )  IN: overall stddev of stripe fits

Returns: `error' from gdsd_wreal.

Description:
	`sigmap' is written at the toplevel.

Comment:      This routine is not callable in Fortran

Updates:      09 Jun 1992: DK, Creation date

#<
*/

int irim_putsigma( 
	fchar	irds,
	float	sigmap )
{
	fint	error = 0, level = 0 ;

	GDSD_WREAL( irds, str_key[0], level, sigmap, &error ) ;
	return( error ) ;
}

/*
**************************************************************************
#>   irim_finispars.dc2
Function:     irim_finispars

Purpose:      keep/delete destripe parameters.

Category:     IRAS

Author:       Do Kester		

Use:  
void irim_finispars( 
	fchar		irds )		IN: name of irds

Description:
	Depending on whether the internal variable `keep' is
	TRUE:  all destripe params are kept i.e. nothing is done.
	FALSE: all destripe params are deleted.

Comment:      This routine is not callable in Fortran

Updates:      09 Jun 1992: DK, Creation date

#<
*/

void irim_finispars( 
	fchar		irds )
{
	fint	i, error = 0 ;

	if ( ! keep ) {
	    for ( i = 0 ; i <= MAXCAL ; i++ ) {
		error = 0 ;
		gdsd_delall_c( irds, tofchar( str_key[i] ), &error ) ;
		anyoutf( ANYOUT_TST, 
			"delete from irds %.*s   key  %s   result : %d", 
			nelc_c( irds ), irds.a, str_key[i], error ) ;
	    }
	}
	return ;
}

/*
#>  irim_pars.dc2
File:         irim_pars

Purpose:      connected set of destripe parameter manipulating functions

Category:     IRAS

Author:       Do Kester		

Description:
The file contains the following functions
	irim_getpars
	irim_putpars
	irim_initpars
	irim_getsigma
	irim_putsigma
	irim_userpars
	irim_finispars
See individual .dc2 documents for purpose and use.

Comment:      These routines are not callable in Fortran

Updates:      02 Aug 1992: DK, Creation date

#<

#>  irim_pars.h
int  irim_getpars( fchar, fint, fint, float *, fint, float * ) ;
int  irim_putpars( fchar, fint, fint, float *, fint, float ) ;
void irim_initpars( void ) ;
int  irim_getsigma( fchar, float * ) ;
int  irim_putsigma( fchar, float ) ;
void irim_userpars( void ) ;
void irim_finispars( fchar ) ;
#<
*/
#undef NO_PARAMS
#undef MAXCAL
#undef DEFAULT_SIGMA
#undef GDSD_GRREAL
#undef GDSD_WREAL
#undef GDSD_RREAL
#undef GDSD_RCHAR


