/* irds_basic.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_basic.dc2

Function:     irds_basic

Purpose:      irds_basic.c contains all basic irds access routines

Category:     IRDS

File:         irds_basic.c

Author:       P.R. Roelfsema

Description:    This file contains the basic IRDS routines for creating,
              deleting etc. of IRDSs. The following routines are available:

              IRDS_CLOSE        - close an IRDS.
              IRDS_EXIST        - test whether an IRDS exists.
              IRDS_DELETE       - delete an IRDS.
              IRDS_CREATE       - create an empty IRDS.
              IRDS_ENQUIRE      - gives info about an IRDS.
              IRDS_ENQUIRE_SNIP - gives info about a snip in an IRDS.
              IRDS_EXTEND       - extend IRDS with one snip.

Updates:      Aug 8, 1990: PRR, Creation date
              Nov 4, 1990: PRR, Changed SATCAL   -> TICK axis
                                        DETECTOR -> SDET axis
	      Feb 19,1992: DK, make status/error output parameter only
	      Mar 18,1992: FL, interface to IRDS_ENQUIRE_SNIP and
	                       IRDS_EXTEND changed. 
	      Apr 20, 1994:DK, Adjust hash table in irds_create to expected size
#<

*/


#include "gipsyc.h"
#include "string.h"
#include "error.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "gds_exist.h"
#include "gds_close.h"
#include "gds_delete.h"
#include "gdst_prime.h"		/* sets the size of the hash table */
#include "gdst_initsize.h"	/* sets the initial size of the descr file */
#include "gds_create.h"		/* basic gds function definition */
#include "gds_extend.h"		/* basic gds function definition */
#include "gdsd_rint.h"
#include "gdsd_rchar.h"
#include "gdsd_rreal.h"
#include "gdsd_rdble.h"
#include "gdsd_wchar.h"		/* gds descriptor function definition */
#include "gdsd_wreal.h"		/* gds descriptor function definition */
#include "gdsd_wdble.h"		/* gds descriptor function definition */
#include "gdsd_wint.h"		/* gds descriptor function definition */
#include "gdsc_word.h"
#include "gdsc_ndims.h"
#include "gdsc_name.h"
#include "gdsc_size.h"
#include "gdsc_range.h"
#include "gdsi_write.h"
#include "ircc_detnr.h"
#include "ircc_obsmode.h"
#include "setnfblank.h"
#include "nelc.h"
#include "userfio.h"

#define	FOREVER		for ( ; ; )	/* infinite loop */

#define MINNAXIS	 3		/* minumum number of axes */
#define MAXNAXIS	 4 		/* maximum number of axes */

#define IRDSOK		 0		/* legal IRDS */
#define NOEXIST		-1		/* IRDS does not exist */
#define BADDIMENSION	-2		/* IRDS does not have naxis=4 */
#define NOSAMPLE	-3		/* no SAMPLE axis in IRDS */
#define NOTICK		-4		/* no TICK axis in IRDS */
#define NOSDET		-5		/* no SDET axis in IRDS */
#define NOSNIP		-6		/* no SNIP axis in IRDS */

#define CTBUFLEN	20
#define MAXBUF		1024





/*

#>              irds_close.dc2

Subroutine:     IRDS_CLOSE

Purpose:        closes an existing IRDS

File:   	irds_basic.c

Author:         Do Kester

Use:    CALL IRDS_CLOSE( IRDS,     I     character*(*) 
                         STATUS )  O     integer

        IRDS    name of (existing) irds to close
        STATUS   0 : OK. 
                -2 : error while closing

Description:    

Externals:      gds_close

Updates:        23/02/90 DK, document creation
		19/02/92 DK: make status output parameter only
#<

@ subroutine irds_close( character, integer )

*/

void irds_close_c(
	fchar	irds,
	fint*	status )

{
	*status = 0 ;		/* is output parameter ONLY */
	gds_close_c( irds, status );
}


/* irds_exist.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            irds_exist.dc2

Function:     irds_exist

Purpose:      To test whether a gipsy set is a legal irds.

Category:     IR

File:         irds_basic.c

Author:       P.R. Roelfsema

Use:          INTEGER IRDS_EXIST( IRDS ,       Input   character*(*)
                                  ERROR  )     Output  integer

              IRDS_EXIST  Returns code to indicate what IRDS is"
                           0  - irds is a legal IR data structure.
                          -1  - irds does not exist.
                          -2  - irds exists but has wrong dimension
                          -3  - irds has no sample axis
                          -4  - irds has no tick axis
                          -5  - irds has no detector axis
                          -6  - irds has no snip axis
              IRDS        name of irds.
              ERROR       gds error code.

Updates:      Aug 10, 1990: PRR, Creation date
	      Feb 19, 1992: DK, make ERROR indeed an output param ONLY
#<

Fortran to C interface:

@ integer function irds_exist( character , integer )

*/


fint irds_exist_c( fchar irds , fint *error ) 
{
   fint  level ;
   fint  naxis ;
   fchar ctype ;
   char  ctbuf[ CTBUFLEN + 1 ] ;

   ctype.a = ctbuf ;
   ctype.l = CTBUFLEN ;
   *error = 0 ;				/* is output parameter ONLY */
   if ( !gds_exist_c( irds , error ) ) return( NOEXIST ) ;
   level = 0 ;
   gdsd_rint_c( irds , tofchar( "NAXIS" ) , &level , &naxis , error ) ;
   if ( naxis < MINNAXIS || naxis > MAXNAXIS ) return( BADDIMENSION );
   gdsd_rchar_c( irds , tofchar( "CTYPE1" ) , &level , ctype , error ) ;
   if ( strncmp( ctype.a , "SAMPLE"   , 6 ) ) return( NOSAMPLE );
   gdsd_rchar_c( irds , tofchar( "CTYPE2" ) , &level , ctype , error ) ;
   if ( strncmp( ctype.a , "TICK"     , 4 ) ) return( NOTICK );
   gdsd_rchar_c( irds , tofchar( "CTYPE3" ) , &level , ctype , error ) ;
   if ( strncmp( ctype.a , "SDET"     , 4 ) ) return( NOSDET );
   if ( naxis == 4 ){
      gdsd_rchar_c( irds , tofchar( "CTYPE4" ) , &level , ctype , error ) ;
      if ( strncmp( ctype.a , "SNIP"     , 4 ) ) return( NOSNIP );
   } else {
      return( NOSNIP );
   }
   return( IRDSOK ) ;
}


/*

#>              irds_delete.dc2

Subroutine:     IRDS_DELETE

Purpose:        deletes an existing IRDS

File:   	irds_basic.c

Author:         Do Kester

Use:    CALL IRDS_DELETE( IRDS,     I     character*(*) 
                          STATUS )  O     integer

        IRDS    name of (existing) irds to delete
        STATUS   0 : OK. 
                -2 : error while deleting

Description:    
	When the irds does not exist, IRDS_DELETE does nothing.

Externals:      gds_delete gds_exist

Updates:        23/02/90 DK, document creation
		16/10/90 DK, make no-op
		19/02/92 DK, make status a true output parameter
#<

@ subroutine irds_delete( character, integer )

*/

void irds_delete_c(
        fchar   irds,
        fint    *status )

{
	*status = 0 ;		/* is OUTPUT parameter ONLY */
        if ( gds_exist_c( irds, status ) ) {
		*status = 0 ;
		gds_close_c( irds, status ) ;
		*status = 0 ;
		gds_delete_c( irds, status );
	}
}


/*

#>              irds_create.dc2

Subroutine:     IRDS_CREATE

Purpose:        creates an empty IRDS

File:   	irds_basic.c

Author:         Do Kester

Use:    CALL IRDS_CREATE( IRDS,     I     character*(*) 
                          INSTRU,   I     character*(*)
                          AXES,     I     integer array (3)
                          CENTER,   I     double array (2)
                          SIZE,     I     double array (2)
                          COOR,     I     character*(*)
                          EPOCHE,   I     real
                          OBJECT,   I     character*(*)
                          OBSERV,   I     character*(*)
                          STATUS )  O     integer

        IRDS    name of (existing) irds to open
        INSTRU  instrument identification.
                e.g. 'SURVEY B1', 'SURVEY LRS', 'AO B4' etc
        AXES    expected length of the axes (fast to slow)
        CENTER  longitude & latitude of custum plate center
        SIZE    of custom plate in lon & lat direction
        COOR    coordinate system identification.
                Can be 'EQUATORIAL', 'GALACTIC', 'ECLIPTIC',
                'SUPER GALACTIC', 'SUN REFERENCED'
        EPOCHE  epoche of coordinate system
        OBJECT  name of the object
        OBSERV  name of the observer
        STATUS   0 : OK. 
                -2 : some of the provided parameters are wrong

Description:    

        irds_create creates an empty irds; if an irds with this
        name already exists it will be overwritten.
        The axis lengths are given in axes (the number of 
        samples per tick, the ticks and the number of detectors)
        The number of snips will be set to zero. 
        All obligatory header items will be filled in.


Externals:      

Updates:        23/02/90 DK, document creation
#<

@ subroutine irds_create( character, character, integer, 
@                         double precision, double precision, character,
@                         real, character, character, integer )

*/

#define	NPRIME	20		/* length of the list of primes */

void irds_create_c(
	fchar   irds, 			/* name of set */
	fchar   instru, 		/* instrument identification */
	fint   *axes, 			/* axes of irds */
	double *center, 		/* long & lat of plate  */
	double *size, 			/* size of plate in lon resp. lat */
	fchar   coor, 			/* name of coordinate system */
	float  *epoche, 		/* epoche coordinate system */
	fchar   object, 		/* name of the object */
	fchar   observ, 		/* name of the observer */
	fint   *status )		/* status variable */

{
	fint	grid, i;
	fint	axis, det, level, snips, ticks, dets, items ;
	double	crval, cdelt;
	double	origin;
        double  depoche;		/* epoche in double */
	static fint prime[NPRIME] = 
	  { 311, 599, 997, 1511, 2003, 3001, 5003, 7001, 10007, 14009, 20011,
	  26003, 33013, 41011, 50021, 60013, 71011, 83003, 96001, 170347 } ;

/* calculate the expected number of descriptor items */
	ticks = axes[1] ;
	if ( ircc_obsmode_c( instru ) == 1 ) snips = ticks ; /* approximately */
	else snips = 10 ;		/* for AOs snips = nr of legs */
	dets  = axes[2] ;
/* item = stdhead + nr_dets + snip_info + BPHF data */
	items = 40 + dets + 11 * snips + 5 * snips * ticks ;
/* plus, if not LRS, destripe parameters */
	if ( dets > 5 ) items += 4 * snips * dets ;
	for ( i = 0 ; i < NPRIME && prime[i] < items ; i++ ) ;
	if ( i == NPRIME ) i-- ;
/* set this prime to control the internal hash table of the descriptor */
	gdst_prime_c( &(prime[i]) ) ;
	items = prime[i] * 4 + 40000 ;	/* nr of bytes initially in .descr */
	gdst_initsize_c( &items ) ;
/* create a three axial structure */
	*status = 0 ;
	gds_create_c( irds, status );

	origin = 0.0; *status = 0 ;
	gds_extend_c(irds,tofchar("SAMPLE")  ,&origin,&axes[0]        ,status);
	gds_extend_c(irds,tofchar("TICK")    ,&origin,&axes[1]        ,status);
	gds_extend_c(irds,tofchar("SDET")    ,&origin,&axes[2]        ,status);

/* fill in the obligatory descriptors */
	level = 0;
	gdsd_wchar_c(irds,tofchar("BUNIT")   ,&level,tofchar("DN")    ,status);
	gdsd_wchar_c(irds,tofchar("ORIGIN")  ,&level,tofchar("GEISHA"),status);
	gdsd_wchar_c(irds,tofchar("INSTRUME"),&level,instru           ,status);
	crval = 0.0;
	gdsd_wdble_c(irds,tofchar("CRVAL1")  ,&level,&crval           ,status);
        cdelt = 1.0 / (real) axes[0];
	gdsd_wdble_c(irds,tofchar("CDELT1")  ,&level,&cdelt           ,status);
	gdsd_wchar_c(irds,tofchar("CUNIT1")  ,&level,tofchar("TICK")  ,status);
	gdsd_wdble_c(irds,tofchar("CRVAL2")  ,&level,&crval           ,status);
	cdelt = 1.0;
	gdsd_wdble_c(irds,tofchar("CDELT2")  ,&level,&cdelt           ,status);
	gdsd_wchar_c(irds,tofchar("CUNIT2")  ,&level,tofchar("TICK")  ,status);
	crval = 1.0;
	gdsd_wdble_c(irds,tofchar("CRVAL3")  ,&level,&crval           ,status);
	gdsd_wdble_c(irds,tofchar("CDELT3")  ,&level,&cdelt           ,status);
	gdsd_wchar_c(irds,tofchar("CUNIT3")  ,&level,tofchar(" ")     ,status);
	gdsd_wchar_c(irds,tofchar("TELESCOP"),&level,tofchar("IRAS")  ,status);
        depoche = *epoche ;
	gdsd_wdble_c(irds,tofchar("EPOCH")   ,&level,&depoche         ,status);
	gdsd_wchar_c(irds,tofchar("SKYSYS")  ,&level,coor             ,status);
	gdsd_wdble_c(irds,tofchar("LONCENTR"),&level,&center[0]       ,status);
	gdsd_wdble_c(irds,tofchar("LATCENTR"),&level,&center[1]       ,status);
	gdsd_wdble_c(irds,tofchar("LONSIZE") ,&level,&size[0]         ,status);
	gdsd_wdble_c(irds,tofchar("LATSIZE") ,&level,&size[1]         ,status);
	gdsd_wchar_c(irds,tofchar("OBJECT")  ,&level,object           ,status);
	gdsd_wchar_c(irds,tofchar("OBSERVER"),&level,observ           ,status);

/* write detector numbers at the detector level */
	for ( i = 1, grid = 1; i <= axes[2]; i++, grid++ ) {
		det = ircc_detnr_c( &i, instru );
		level = 0; axis = 3; *status = 0;
		level = gdsc_word_c( irds, &axis, &grid, &level, status );
		gdsd_wint_c( irds, tofchar( "DETNO" ), &level, &det, status );
	}
	return;
}

	

/*

#>              irds_enquire.dc2

Subroutine:     IRDS_ENQUIRE

Purpose:        returns some basic info about an existing IRDS

File:   	irds_basic.c

Author:         Do Kester

Use:    CALL IRDS_ENQUIRE( IRDS,     I     character*(*)
                           OBJECT,   O     character*(*)
                           INSTRU,   O     character*(*)
   	      		   NAXIS,    O	   integer
                           AXES,     O     integer array (naxis)
                           CENTER,   O     double array (2)
                           SIZE,     O     double array (2)
                           COOR,     O     character*(*)
                           EPOCHE,   O     real
                           STATUS )  O     integer

        IRDS    name of (existing) irds to open
	OBJECT	object identification
        INSTRU  instrument identification
	NAXIS	the number of axes of the IRDS
        AXES    length of the axes of the irds (fast to slow)
        CENTER  longitude & latitude of custum plate center
        SIZE    of custom plate in lon & lat direction
        COOR    coordinate system identification
	EPOCHE	the epoche of the coordinate system if applicable
        STATUS   0 : OK. 
                -1 : the irds does not exist
                -2 : not enough axes exist
		-3 : instru is missing
		-4 : center is missing
		-5 : size is missing
		-6 : coor is missing
                -7 : object is missing
                -8 : epoche is missing

Description:    
        irds_enquire tests whether a given irds exists and if so
        it returns information on the instrument, on the length
        of the axes in order fast to slow, on the center and the
        size of the custom plate and on the coordinate system
        these value are given in. 

        Both the instrument and the coordinate system are coded
        strings. (see IRDS_CREATE)

Externals:      gds_exist

Updates:        23/02/90 DK, document creation
		16/10/90 DK, documentation update
#<

@ subroutine irds_enquire( character, character , character, integer, 
@                          integer, double precision, double precision,
@                          character, real, integer )

*/

void irds_enquire_c(
	fchar	 irds,
        fchar    object,
	fchar	 instru,
	fint    *naxis,
	fint    *axes,
	double  *center,
	double  *size,
	fchar 	 coor,
        float   *epoche,
	fint    *status )

{
	fint	i, level, error, axnum;
        double  depoche ;

	error = 0;
	if ( ! ( i = gds_exist_c( irds, &error ) ) ) { *status = -1; return; }

	level = 0;
	error = 0;
	*naxis = gdsc_ndims_c( irds, &level );
	for ( i = 0, axnum = 1; i < *naxis; i++, axnum++ ) {
		axes[i] = gdsc_size_c( irds, &axnum, &error );
		if ( error ) { *status = -2; return; }
	}

	level = 0;
	error = 0;
	gdsd_rdble_c( irds, tofchar( "LONCENTR" ), &level, &center[0], &error );
	if ( error ) { *status = -4; return; }
	gdsd_rdble_c( irds, tofchar( "LATCENTR" ), &level, &center[1], &error );
	if ( error ) { *status = -4; return; }
	gdsd_rchar_c( irds, tofchar( "SKYSYS" )  , &level, coor      , &error ); 
	if ( error ) { *status = -6; return; }
	gdsd_rchar_c( irds, tofchar( "INSTRUME" ), &level, instru    , &error );
	if ( error ) { *status = -3; return; }
	gdsd_rdble_c( irds, tofchar( "LONSIZE" ) , &level, &size[0]  , &error );
	if ( error ) { *status = -5; return; }
	gdsd_rdble_c( irds, tofchar( "LATSIZE" ) , &level, &size[1]  , &error );
	if ( error ) { *status = -5; return; }
	gdsd_rchar_c( irds, tofchar( "OBJECT" )  , &level, object    , &error );
	if ( error ) { *status = -7; return; }
	gdsd_rdble_c( irds, tofchar( "EPOCH" )   , &level, &depoche  , &error );
	if ( error ) { *status = -8; return; }
        *epoche = depoche ;

	return;
}



/*

#>              irds_enquire_snip.dc2
Subroutine:     IRDS_ENQUIRE_SNIP

Purpose:        returns some basic information of a snip

File:   	irds_basic.c

Author:         Do Kester

Use:    CALL IRDS_ENQUIRE_SNIP( IRDS,     I     character*(*)
                                SNIP,     I     integer
                                SOP,      O     integer
                                OBS,      O     integer
                                ATT,      O     integer
                                SCANTYPE, O     character*(*)
                                SCANCAL,  O     integer
                                SCANDUR,  O     integer
                                SNIPCAL,  O     integer
                                SNIPDUR,  O     integer
                                PSI,      O     real
                                PSIRATE,  O     real
                                THETA,    O     real
                                STATUS )  O     integer

        IRDS    name of irds 
        SNIP    sequential snip number for which information is required
        SOP     sop number of the snip
        OBS     observation number of the snip
        ATT     attitude block number of snip
        SCANTYPE
        SCANCAL satcal at the beginning of the original scan
        SCANDUR duration in ticks of the original scan
        SNIPCAL ticks since scancal to begin of snip
        SNIPDUR duration in ticks of the snip
        PSI     intended psi at scancal
        PSIRATE intended rate in psi during scan
        THETA   intended theta of scan
        STATUS   0 : OK. 
                -1 : cannot find irds
                -2 : snip does not exist
                -nn: errors from GDSD_Rxxx

Description:    
        The header items of the specified snip are read and returned
        to the caller.

Externals:      

Updates:        23/02/90 DK, document creation
		16/10/90 DK, change in status return values
		19/12/90 DK, determine error status -2 via gdsc_ndims
		18/03/92 FL, OBS and SCANTYPE added, no error if not
		             present, but zero and blank are returned
		             to also handle old irds data.
		             To add these items in header use FIXSNIP
#<

@ subroutine irds_enquire_snip( character, integer, integer, integer,
@				integer, character, integer, integer,
@				integer, integer, real, real, real, integer )

*/


void irds_enquire_snip_c(
	fchar	irds, 
	fint *	snip,    
	fint *	sop, 
	fint *	obs, 
	fint *	att,      
	fchar 	scantype,
	fint *	scancal,  
	fint *	scandur,  
	fint *	snipcal,  
	fint *	snipdur,  
	float *	psi,      
	float *	psirate,  
	float *	theta,    
	fint *	status )  
{
	fint	error, axis, level, grid, no;

/* check whether the IRDS exists */
	error = 0;
	if ( ! gds_exist_c( irds, &error ) ) { *status = -1; return; }

/* determine number of axes */
	level = 0;
	axis = gdsc_ndims_c( irds, &level );
	if ( axis < 4 ) { *status = -2; return; }
/* read descriptor for this snip */
	grid = *snip ;
	error = 0;
	level = gdsc_word_c( irds, &axis, &grid, &level, &error );
	if ( error ) { *status = -2; return; }
	gdsd_rint_c(  irds, tofchar( "SOP" )    , &level, sop, &error );
	if ( error < 0 ) { *status = error; return; }
	gdsd_rint_c(  irds, tofchar( "OBS" )    , &level, obs, &error );
	if ( error < 0 ) { *obs = 0; error = 0; }
	gdsd_rint_c(  irds, tofchar( "ATT" )    , &level, att, &error );
	if ( error < 0 ) { *status = error; return; }
	gdsd_rint_c(  irds, tofchar( "SNIPCAL" ), &level, snipcal, &error );
	if ( error < 0 ) { *status = error; return; }
	gdsd_rint_c(  irds, tofchar( "SNIPDUR" ), &level, snipdur, &error );
	if ( error < 0 ) { *status = error; return; }
	gdsd_rchar_c(  irds, tofchar( "SCANTYPE" ), &level, scantype, &error );
	if ( error < 0 ) { 
		for( no = 0 ; no < scantype.l ; scantype.a[no++] = ' ' );
		error = 0;
	}
	gdsd_rint_c(  irds, tofchar( "SCANCAL" ), &level, scancal, &error );
	if ( error < 0 ) { *status = error; return; }
	gdsd_rint_c(  irds, tofchar( "SCANDUR" ), &level, scandur, &error );
	if ( error < 0 ) { *status = error; return; }
	gdsd_rreal_c( irds, tofchar( "PSI" )    , &level, psi, &error );
	if ( error < 0 ) { *status = error; return; }
	gdsd_rreal_c( irds, tofchar( "PSIRATE" ), &level, psirate, &error );
	if ( error < 0 ) { *status = error; return; }
	gdsd_rreal_c( irds, tofchar( "THETA" )  , &level, theta, &error );
	if ( error >= 0 ) *status = 0;

	return;
}



/*

#>              irds_extend.dc2
Subroutine:     IRDS_EXTEND

Purpose:        extends an existing IRDS with one snip

File:   	irds_basic.c

Author:         Do Kester

Use:    CALL IRDS_EXTEND( IRDS,     I     character*(*) 
                          SOP,      I     integer
                          OBS,      I     integer
                          ATT,      I     integer
                          SCANTYPE, I     character*(*)
                          SCANCAL,  I     integer
                          SCANDUR,  I     integer
                          SNIPCAL,  I     integer
                          SNIPDUR,  I     integer
                          PSI,      I     real
                          PSIRATE,  I     real
                          THETA,    I     real
                          STATUS )  O     integer

        IRDS    name of (existing) irds to extend
        SOP     sop number of the extending snip
        OBS	observation number of snip
        ATT     attitude block number of snip
        SCANTYPE
        SCANCAL satcal at the beginning of the original scan
        SCANDUR duration in ticks of the original scan
        SNIPCAL ticks since scancal to begin of snip
        SNIPDUR duration in ticks of the snip
        PSI     intended psi at scancal
        PSIRATE intended rate in psi during scan
        THETA   intended theta of scan
        STATUS   0 : OK. 
                -1 : cannot find irds
                -2 : snipdur is too long
                -3 : other error while extending

Description:    
        The existing irds is extended with one snip. The obligatory
        header items are filled in at the snip level. The number of
        snips at the top level is increased by one. 
        The pixels are undefined.

Externals:      

Updates:        23/02/90 DK, document creation
                18/03/92 FL, OBS and SCANTYPE added, PLATNR and NPLATES removed
#<

@ subroutine irds_extend( character, integer, integer, integer, character, 
@	integer, integer, integer, integer, real, real, real, integer )

*/

void irds_extend_c(
	fchar	irds,     
	fint 	*sop,      
	fint	*obs,
	fint 	*att,
	fchar	scantype,      
	fint 	*scancal,  
	fint 	*scandur,  
	fint 	*snipcal,  
	fint 	*snipdur,  
	float 	*psi,      
	float 	*psirate,  
	float 	*theta,    
	fint 	*status )  
{
	fint	error = 0 , naxis, axis, axislength, size[MAXNAXIS], nr;
	fint	i, maxbuf, cwlo, cwhi, todo, grid, level;
	double	crval, cdelt ;
        float	buffer[MAXBUF];
	double	origin;

/* check whether the IRDS exists */
	if ( ! gds_exist_c( irds, &error ) ) { *status = -1; return; }

	level = 0;
        gdsd_rint_c( irds , tofchar( "NAXIS1" ) , &level , &nr , &error ) ;
        size[ 0 ] = nr ;
        gdsd_rint_c( irds , tofchar( "NAXIS2" ) , &level , &i  , &error ) ;
        size[ 1 ] = i ;
        nr = nr * i ;
        gdsd_rint_c( irds , tofchar( "NAXIS3" ) , &level , &i  , &error ) ;
        size[ 2 ] = i ;
        nr = nr * i ;				/* nr pixels per snip */

/* check whether the snip fits in the irds */
	if ( *snipdur > size[ 1 ] ) { *status = -2; return; }

	axis = MAXNAXIS; 
        gdsd_rint_c( irds , tofchar( "NAXIS" ) , &level , &naxis , &error ) ;
	if ( naxis != 4 ) {  		/* the set has no snips: extend */
		axislength = 1;
		origin = 0.0;
		gds_extend_c( irds, tofchar( "SNIP" ), &origin, 
			&axislength, &error );
		crval = cdelt = 1.0;
		gdsd_wdble_c( irds, tofchar( "CRVAL4" ), &level, 
			&crval, &error );
		gdsd_wdble_c( irds, tofchar( "CDELT4" ), &level, 
			&cdelt, &error );
		gdsd_wchar_c( irds, tofchar( "CUNIT4" ), &level, 
			tofchar( " " ), &error );
		size[MAXNAXIS-1] = 1;	
	} else {
		size[MAXNAXIS-1] = gdsc_size_c( irds, &axis, &error ) + 1 ;
		axislength = size[MAXNAXIS-1] ;
		origin = 0.0;
		gds_extend_c( irds, tofchar( "SNIP" ), &origin, 
			&axislength, &error );
	}

/* write all blanks to a new snip */
	maxbuf = MAXBUF;
	setnfblank_c( buffer, &maxbuf );
	todo = 0;
	grid = size[MAXNAXIS - 1] ;		
	error = 0;	
	level = gdsc_word_c( irds, &axis, &grid, &level, &error );
	gdsc_range_c( irds, &level, &cwlo, &cwhi, &error );
	FOREVER {
		gdsi_write_c( irds, &cwlo, &cwhi, buffer, &maxbuf, &nr, &todo );
		if ( todo == 0 ) break;
		if ( todo < 0 ) { *status = -3; return; }
	}

/* write descriptor for this snip to the level */
	gdsd_wint_c( irds, tofchar( "SOP" ), &level, sop, &error );
	gdsd_wint_c( irds, tofchar( "OBS" ), &level, obs, &error );
	gdsd_wint_c( irds, tofchar( "ATT" ), &level, att, &error );
	gdsd_wint_c( irds, tofchar( "SNIPCAL" ), &level, snipcal, &error );
	gdsd_wint_c( irds, tofchar( "SNIPDUR" ), &level, snipdur, &error );
	gdsd_wchar_c( irds, tofchar( "SCANTYPE" ), &level, scantype, &error );
	gdsd_wint_c( irds, tofchar( "SCANCAL" ), &level, scancal, &error );
	gdsd_wint_c( irds, tofchar( "SCANDUR" ), &level, scandur, &error );
	gdsd_wreal_c( irds, tofchar( "PSI" ), &level, psi, &error );
	gdsd_wreal_c( irds, tofchar( "PSIRATE" ), &level, psirate, &error );
	gdsd_wreal_c( irds, tofchar( "THETA" ), &level, theta, &error );
	return;
}
