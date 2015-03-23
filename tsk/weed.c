/*
                           COPYRIGHT (c) 1991
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>            weed.dc1

Program:      WEED

Purpose:      Copies snips to an irds (or part of it), or removes snips.
              It will also remove snips which occur more than once in 
              the irds.

Category:     IRAS

File:         weed.c

Author:       Fred Lahuis

Keywords:

    IRSET=     IRDS data set.
               If given, the (snip)subsets represent the snips to be 
               copied. If noting is entered SNIP is asked.

    OUTSET=    If no output set is given the input set will be    [input set]
               overwritten.
    
    SNIP=      Snips to be copied. It is only asked if            [all snips]
               no snips have been entered by way of subsets.
               
    DELSNIP=   Snips to be deleted.                                    [none]
               Has no effect if snips have already been entered through
               subsets or by way of SNIP and is only asked if this is 
               not the case.

  **LENGTH=    All snips with a length less than or equal to this         [0]
               will be removed.
               
  **POS=       Position of the centre of an area for           ['map' centre]
               which the overlapping snips are to be copied.
               If a position but no radius is entered, all snips passing
               through this point are returned.
               If no position is entered the centre as defined in the 
               header is assumed.
               The position has to be given in the same units as those for
               the area defined in the header.
               
  **RAD=       Radius of the area.                                       [-1]
               The radius has to be given in the same units as used for the
               area defined in the header.
               A radius of zero means the snips going through the position
               given with POS.
               A negative radius means no area is defined.
               
  **SDET=      Detector number used in the selection of                   [0]
               snips in an area.
               SDET = 0 corresponds to the boresight.
               
Description:   A combination of inputs also works, e.g. giving SNIP, LENGTH
               and RAD results in copying all snips inside an area of radius 
               RAD around the centre, not specified in SNIP and longer 
               than LENGTH.
               
               At the end WEED copies all header items common to the selected
               snips. For these it has to search the entire header, which
               unfortunately is not always very fast.
               
               WEED also removes all snips occuring more than once in the
               IRDS, i.e. all snips with the same SOP ATT.
               
Comments:

Updates:       August 15 1991:    FL Document created.
               September 10 1991: FL Adjusted to new copyhead_item (faster
                                     than the old one).
                                     Userinfo altered.
               January 8 1992:    FL Bug in the copying of the header 
                                     items removed.

#<

*/

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"
#include "nelc.h"
#include "anyout.h"
#include "error.h"
#include "math.h"
#include "stabar.h"
#include "status.h"
#include "cancel.h"
#include "wkey.h"
#include "deputy.h"

#include "userint.h"
#include "userreal.h"
#include "userdble.h"
#include "usertext.h"
#include "userangle.h"

#include "gdsinp.h"
#include "gds_close.h"
#include "gds_delete.h"
#include "gds_exist.h"
#include "gds_rename.h"
#include "gdsc_range.h"
#include "gdsc_fill.h"
#include "gdsc_word.h"
#include "gdsc_grid.h"
#include "gdsd_find.h"
#include "gdsd_rint.h"
#include "gdsd_wint.h"
#include "gdsd_rreal.h"
#include "gdsd_wreal.h"
#include "gdsd_rchar.h"
#include "gdsd_wchar.h"
#include "gdsd_rdble.h"

#include "copyhead_item.h"

#include "ircc_rate.h"
#include "irco_number.h"
#include "irco_precess.h"
#include "irco_prname.h"
#include "irco_namepoch.h"
#include "irds_create.h"
#include "irds_extend.h"
#include "irds_rd_bphf.h"
#include "irds_wr_bphf.h"
#include "irds_rd_samples.h"
#include "irds_wr_samples.h"
#include "irds_rd_detpos.h"
#include "irds_rd_detoff.h"
#include "irds_enquire.h"
#include "irds_enquire_snip.h"

#define  VERSION     "1.1"
#define  PROGRAM     "WEED"

#define  poskey      tofchar("POS=")
#define  posmess     tofchar("Position for area:" )
#define  radkey      tofchar("RAD=")
#define  radmess     tofchar("Radius of the area")
#define  inkey       tofchar("IRSET=")
#define  inmess      tofchar("Set and subset to work on")
#define  lengthkey   tofchar("LENGTH=")
#define  lengthmess  tofchar("Minimum length os the snips")
#define  outkey      tofchar("OUTSET=")
#define  outmess     tofchar("Output IRDS. [input irds]")
#define  sdetkey     tofchar("SDET=")
#define  sdetmess    tofchar("Give detector number for selection in area")
#define  snipkey     tofchar("SNIP=")
#define  snipmess    tofchar("Give snips to be copied")
#define  delsnipkey  tofchar("DELSNIP=")
#define  delsnipmess tofchar("Give snips to be deleted")
#define  PI          3.1415926535897932384
#define  RAD         ( 180.0 / PI )
#define  MAXSUB      5000
#define  MAXITEMS    250
#define  EXACT       4				/* default for messages */
#define  HIDDEN      2				/* id. */
#define  REQUEST     1				/* id. */
#define  NONE        0				/* id. */
#define  anyoutC     anyout_c( &device , tofchar(Cstring) )
#define  anyout(a)   anyout_c( &device , tofchar(a) )
#define  mess(a)     (message = tofchar((a)))
#define  sqr(a)      ( (a)*(a) )
#define  max(a,b)    ( ((a)>(b))?(a):(b) )
#define  min(a,b)    ( ((a)<(b))?(a):(b) )
#define  fmake(fchr,size)	{ \
					static char buff[size+1] ; \
					int i ; \
					for( i = 0 ; i < size ; buff[i++] = ' ') ; \
					buff[i] = 0 ; \
					fchr.a = buff ; \
					fchr.l = size ; \
				}

static fint	irds_ss[MAXSUB] , maxsub = MAXSUB ;
static fint	subdim = 0 , naxis = 4 , nsub ;
static fint	class = 1 ;
static fint	k, n , no , snip_no ;
static fint	no_del , no_snips , add_snip ;
static fint	status ;
static fint	axnum[4] , axis[4] , axis_out[4] ;
static fint	zero = 0 , one = 1 , two = 2 , four = 4 ;
static fint	*snip , *del_snip ;
static fint	device = 11 , dfault ;
static fint	grid ;
static fint	min_length  = 0 ;
static fint	new_irds ;

static float	epoch ;
static float	fdummy ;

static double	centre[2] , size[2] ;

static char	Cstring[500] ;

static fchar	irds , irds_out , bunit ;
static fchar	observer , object , instrument , cosys ;
 

void sop_att( fchar irds , fint *snip , fint *no_snips )
{							/** remove equal snips **/
	fint	*sop , *att ;
	fint	n , no , snip_no ;
	fint	status , level ;
	fint	new_snip ;
	
	sop = malloc( *no_snips*sizeof(int) ) ;
	att = malloc( *no_snips*sizeof(int) ) ;
	for( no = snip_no = 0 ; no < *no_snips ; no++ ){
		status = level = 0 ;
		new_snip = 1 ;
		level = gdsc_word_c( irds , &four , &snip[no] , &level , &status ) ;
		if( !status ){
			gdsd_rint_c( irds , tofchar("SOP") , &level , &sop[no] , &status ) ;
			gdsd_rint_c( irds , tofchar("ATT") , &level , &att[no] , &status ) ;
			for( n = 0 ; n < no ; n++ ){
				if( sop[no] == sop[n] && att[no] == att[n] ){
					new_snip = 0 ;
					break ;
				}
			}
			if( new_snip )snip[snip_no++] = snip[no] ;
		}
	}
	*no_snips = snip_no ;
	free( sop ) ;
	free( att ) ;
	return ;
}
	
integer snip_lenght( fchar irds , fint *snip , fint *no_snips , fint *min_length , fint *axis )
{
	fint	snipdur , level , status ;
	fint	no_ticks = 0 , no , snip_no ;

	for( no = snip_no = axis_out[1] = 0 ; no < *no_snips ; no++ ){
		status = snipdur = level = 0 ;
		level = gdsc_word_c( irds , &four , &snip[no] , &level , &status ) ;
		if( !status ){
			gdsd_rint_c( irds , tofchar("SNIPDUR") , &level , &snipdur , &status ) ;
			if( snipdur > *min_length ){				/** remove small snips **/
				no_ticks = max( no_ticks , snipdur ) ;	/** determine longest snip **/
				snip[snip_no++] = snip[no] ;
			}
		}
	}
	*no_snips = snip_no ;
	return no_ticks ;
}

void snips_in_area( fchar irds , fint *snip , fint *no_snips , fchar cosys , float epoch , double *centre , double *size , fint *axis )
{
	fint	coor , coor_out = 0 ;
	fint	n1, no_sdet ;
	fint	ndata , n , no , snip_no ;
	fint	status , sdet[33] ;
	float	twothou = 2000.0 ;
	double	pos[2] , radius = -1.0 , distance ;
	double	*lonoff , *latoff , *twistoff ;

/*	sdet = malloc( axis[2]*sizeof(int) ) ;
*/
	dfault = HIDDEN ;
	pos[0] = centre[0] ;
	pos[1] = centre[1] ;
	no = userangle_c( pos , &two , &dfault , poskey , posmess ) ;
	no = userangle_c( &radius , &one , &dfault , radkey , radmess ) ;
	if( radius < 0.0 )return ;
	pos[0] /= RAD ;
	pos[1] /= RAD ;
	radius /= RAD ;

	ndata = axis[0]*axis[1] ;
	if( (lonoff = malloc(ndata*sizeof(double))) == NULL ){
		anyout("error allocating memory lonoff") ;
		finis_c() ;
	}
	if( (latoff = malloc(ndata*sizeof(double))) == NULL ){
		anyout("error allocating memory latoff") ;
		finis_c() ;
	}
	if( (twistoff = malloc(ndata*sizeof(double))) == NULL ){
		anyout("error allocating memory for twistoff") ;
		finis_c() ;
	}

	coor = abs( irco_number_c( cosys , &twothou ) ) ;
	if( coor == 1 && epoch != 2000.0 ){
		irco_precess_c( &coor , &epoch , &coor_out ) ;
	}
	else coor_out = coor ;
	
	status_c( tofchar("Determining snips in the area.") ) ;

	sdet[0] = 0 ;
	no_sdet = userint_c( sdet , &axis[2] , &dfault , sdetkey , sdetmess ) ;
	if( no_sdet == 0 ) no_sdet = 1 ;
	for( no = snip_no = 0 ; no < *no_snips ; no++ ){
		for( n1 = 0 ; n1 < no_sdet ; n1++ ){
			ndata = axis[0]*axis[1] ;
			n = irds_rd_detoff_c(	irds , &snip[no] , &sdet[n1] , &one ,
					&coor_out , &pos[0] , &pos[1] ,
					&coor_out , &zero , lonoff , latoff ,  twistoff ,
					&ndata , &status ) ;
			if( n != 0 ){
				if(  n > 0 ){
					snip[snip_no++] = snip[no] ;
					n1 = no_sdet ;
				}
				else{
					if( radius != 0.0 ){
						distance = sqrt(sqr(lonoff[-n])
						                +
						                sqr(latoff[-n]) ) ;
						if( distance <= radius ){
							snip[snip_no++] = snip[no] ;
							n1 = no_sdet ;
						}
					}
				}
			}
			else{
				switch( status ){
				case  0:break ;
				case -1:anyout("IRDS does not exist") ;
					finis_c() ;
					break ;
				case -2:anyout("IRDS is not a legal irds") ;
					finis_c() ;
					break ;
				case -3:sprintf( Cstring , "SNIP %d not in IRDS" , snip[no] ) ;
				 	anyoutC ;
					break ;
				case -4:sprintf( Cstring , "SDET %d not in IRDS", sdet[n1] ) ;
					anyoutC ;
					break ;
				case -5:sprintf( Cstring , "TICK not in IRDS" ) ;
					anyoutC ;
					break ;
				case -6:anyout("gds read error") ;
					break ;
				case -7:anyout("no coordinate info in header") ;
					break ;
				case -8:sprintf( Cstring, "bad SDET %d", sdet[n1] ) ;
					anyoutC ;
					break ;
 				default:sprintf( Cstring , "something strange, sdet %d snip %d gds error = %d" , sdet[n1], snip[no], status ) ;
					anyoutC ;
					break ;
				}
			}
		}
	}
	*no_snips = snip_no ;

	free( lonoff ) ;
	free( latoff ) ;
	free( twistoff ) ;

	return ;
}

void copy_snips( fchar irds, fchar instrument, fchar irds_out, fint *snip, fint no_snips, fint *axis )
{
        fchar   scantype ;

	fint	ndata , sdet , no , status ;
	fint	sop , obs , att , snip_out ;
	fint	scancal , scandur , snipcal , snipdur ;
	fint	rate ;

	float	psi , psirate , theta ;
	float	*data ;

        fmake(scantype, 20) ;

	rate = ircc_rate_c( instrument ) ;
	ndata = rate*axis[0]*axis[1] ;
	data  = malloc( ndata*sizeof(float) ) ;
	
	for( no = 0 ; no < no_snips ; no++ ){
		sprintf( Cstring , "Copying SNIP no. %2d" , snip[no] ) ;
		status_c( tofchar(Cstring) ) ;
		snip_out = no+1 ;
		irds_enquire_snip_c( irds     , &snip[no] , &sop     , &obs     ,
                                     &att     , scantype  , &scancal , &scandur , 
                                     &snipcal , &snipdur  , &psi     , &psirate ,
				     &theta   , &status ) ;
		if( status != -1 && status != -2 )
			irds_extend_c( irds_out , &sop , &obs, &att , scantype ,
                                       &scancal , &scandur ,&snipcal , &snipdur , 
                                       &psi , &psirate ,&theta , &status ) ;
		switch(status){
		case 0 :for( sdet = 1 ; sdet <= axis[2] ; sdet++ ){		/** copy data **/
				ndata = rate*axis[0]*axis[1] ;
				irds_rd_samples_c( irds , &snip[no] , &sdet , &one ,
							data , &ndata , &status ) ;
				if( !status )
					irds_wr_samples_c( irds_out , &snip_out , &sdet , &one ,
							data , &ndata , &status ) ;
			}
			break ;
		case -1:anyout("extend: cannot find irds") ;
			break ;
		case -2:anyout("extend: snipdur is too long") ;
			break ;
		case -3:anyout("extend: other error while extending") ;
			break ;
		}
	}
	return ;
}
integer std_item( fchar dsc , fint *level )
{
	if( !strncmp( dsc.a , "SOP     " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "OBS     " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "ATT     " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "SNIPCAL " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "SNIPDUR " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "SCANTYPE" , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "SCANCAL " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "SCANDUR " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "PSI     " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "PSIRATE " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "THETA   " , 8 ) )return(1) ;

	if( !strncmp( dsc.a , "DETNO   " , 8 ) )return(1) ;

	if( *level == 0 ){
		if( !strncmp( dsc.a , "CDELT" , 5 ) )return(1) ;
		if( !strncmp( dsc.a , "CRPIX" , 5 ) )return(1) ;
		if( !strncmp( dsc.a , "CRVAL" , 5 ) )return(1) ;
		if( !strncmp( dsc.a , "CUNIT" , 5 ) )return(1) ;
		if( !strncmp( dsc.a , "CTYPE" , 5 ) )return(1) ;
		if( !strncmp( dsc.a , "NAXIS" , 5 ) )return(1) ;
		if( !strncmp( dsc.a , "EPOCH   " , 8 ) )return(1) ;
		if( !strncmp( dsc.a , "INSTRUME" , 8 ) )return(1) ;
		if( !strncmp( dsc.a , "LATCENTR" , 8 ) )return(1) ;
		if( !strncmp( dsc.a , "LATSIZE " , 8 ) )return(1) ;
		if( !strncmp( dsc.a , "LONCENTR" , 8 ) )return(1) ;
		if( !strncmp( dsc.a , "LONSIZE " , 8 ) )return(1) ;
		if( !strncmp( dsc.a , "OBSERVER" , 8 ) )return(1) ;
		if( !strncmp( dsc.a , "OBJECT  " , 8 ) )return(1) ;
		if( !strncmp( dsc.a , "SKYSYS  " , 8 ) )return(1) ;
		if( !strncmp( dsc.a , "TELESCOP" , 8 ) )return(1) ;
	}
	return(0) ;
}

void copy_header( fchar irds , fchar irds_out , fint *snip , fint no_snips, fint *axis_out )
{
fint	level_out[MAXITEMS] , level_in[MAXITEMS] ;
fint	level = 0 , rec_no = 0 ;
fint	k , n , no , snip_in ;
fint	status ;
fint	axis[] = { 1 , 2 , 3 , 4 } ;
fint	grid[4] ;
fint	nitem =  0 ;
fchar	dsc , items ;

	status_c( tofchar("Copying all relevant header items") ) ;
	fmake( items , (20*MAXITEMS) ) ;
	fmake( dsc , 20 ) ;
	do{
		level = 0 ;
		gdsd_find_c( dsc , irds , NULL , &rec_no , &level ) ;
		if( rec_no && (level >= 0) ){
			status = 0 ;
			snip_in = gdsc_grid_c( irds , &axis[3] , &level , &status ) ;
			if( status == -19 ) snip_in = 0 ;
			for( no = 0 ; no < no_snips ; no++ ){
				if( snip_in == snip[no] || snip_in == 0 ){
					grid[3] = (snip_in) ? no+1 : 0 ;
					if( (std_item(dsc,&level) == 0) ){
						for( n = 0 ; n < 20 ; n++ )items.a[n+nitem*20] = dsc.a[n] ;
						level_out[nitem] = 0 ;
						level_in[nitem] = level ;
						for( k = 0 ; k < 3 ; k++ ){
							status = 0 ;
							grid[k] = gdsc_grid_c( irds , &axis[k] , &level , &status ) ;
							if( !status ){
								status = 0 ;
								level_out[nitem] = gdsc_word_c( irds_out , &axis[k] , &grid[k] ,
											&level_out[nitem] , &status ) ;
							}
						}
						if( grid[3] ){
							status = 0 ;
							level_out[nitem] = gdsc_word_c( irds_out , &axis[3] , &grid[3] , 
										&level_out[nitem] , &status ) ;
						}
						nitem++ ;
						if( nitem == MAXITEMS || rec_no == 0 ){
							copyhead_item_c( items , &nitem , irds , irds_out , level_in , level_out ) ;
							nitem = 0 ;
						}
						break ; 	/* exit for loop on snips */
					}
					else break ;		/* exit for loop on snips */
				}
			}
		}
	}while( rec_no ) ;
	copyhead_item_c( items , &nitem , irds , irds_out , level_in , level_out ) ;
	return ;
}

MAIN_PROGRAM_ENTRY   
{
	init_c() ;
	IDENTIFICATION( PROGRAM , VERSION ) ;
	
	dfault = NONE ;
	fmake( irds , 80 ) ; fmake( irds_out , 30 ) ;
	fmake( cosys , 30 ) ; fmake( object , 30 ) ;
	fmake( observer , 30 ) ; fmake( instrument , 30 ) ;
	fmake( bunit , 30 ) ;

	dfault = NONE ;
	nsub = gdsinp_c( irds	 , irds_ss  , &maxsub , &dfault , inkey , inmess , 
			&device , axnum , axis  , &naxis , &class  , &subdim ) ;
	irds_enquire_c( irds , object , instrument , &naxis , axis , centre ,
			size , cosys , &epoch , &status ) ;
	dfault = REQUEST ;
	new_irds = usertext_c( irds_out , &dfault , outkey , outmess ) ;
	if( new_irds ){
		if( !strncmp( irds.a, irds_out.a, max(nelc_c(irds),nelc_c(irds_out)) ) ) new_irds = 0 ;
	}
	if( !new_irds )
		sprintf( irds_out.a, "%.*s%s", nelc_c(irds), irds.a, "_dummy" ) ;
	status  = 0 ;
	if( gds_exist_c( irds_out , &status ) )gds_delete_c( irds_out , &status ) ;

	if( (snip = malloc(2*axis[3]*sizeof(int))) == NULL ){
		anyout("error allocating memory for snip") ;
		finis_c() ;
	}
	for( no = no_snips = 0 ; no < nsub ; no++ ){		/** determining the snips to be deleted **/
		status = 0 ;					/** which are given as subsets **/
		grid = gdsc_grid_c( irds , &four , &irds_ss[no] , &status ) ;
		if( !status && grid > 0 && grid <= axis[3] ){
			snip[no_snips++] = grid ;
			for( k = 0 ; k < no_snips-1 ; k++ ){
				if( grid == snip[k] ){
					no_snips-- ;
					break ;
				}
			}
		}
	}
	dfault = HIDDEN ;
	no = userint_c( &min_length , &one , &dfault , lengthkey , lengthmess ) ;
	if( no_snips == 0 ){
		dfault = REQUEST ;
		no_snips = userint_c( snip , &axis[3] , &dfault , snipkey , snipmess ) ;
		for( no = snip_no = 0 ; no < no_snips ; no++ ){
			if( snip[no] > 0 && snip_no <= axis[3] )snip[snip_no++] = snip[no] ;
		}
		no_snips = snip_no ;
	}
	if( no_snips == 0 ){
		if( (del_snip = malloc(2*axis[3]*sizeof(int))) == NULL ){
			anyout("error allocating memory for delsnip") ;
			finis_c() ;
		}
		dfault = REQUEST ;					/** snips to be deleted **/
		no_del = userint_c( del_snip , &axis[3] , &dfault , delsnipkey , delsnipmess ) ;
		for( no = 1 ; no < axis[3]+1 ; no++ ){
			add_snip = 1 ;
			for( n = 0 ; n < no_del ; n++ ){
				if( del_snip[n] == no )add_snip = 0 ;
			}
			if( add_snip )snip[no_snips++] = no ;		/** snips to be copied **/
		}
		free( del_snip ) ;
	}
	if( strncmp( instrument.a, "AO", 2 ) ) sop_att( irds , snip , &no_snips ) ;				/** remove equal snips **/
	snips_in_area( irds , snip , &no_snips , cosys , epoch , centre , size , axis ) ;

	axis_out[0] = axis[0] ;
	axis_out[1] = snip_lenght( irds , snip , &no_snips , &min_length , axis ) ;
	axis_out[2] = axis[2] ;
	axis_out[3] = no_snips ;

	anyout(" ") ;
	sprintf( Cstring , "%15s" , "selected snips:" ) ;
	for( no = 0 ; no < no_snips ; no++ ){
		sprintf( Cstring , "%s%4d" , Cstring , snip[no] ) ;
		if( strlen( Cstring ) > 60 ){
			anyoutC ;
			sprintf( Cstring , "%15s" , " " ) ;
		}
	}
	if( strlen( Cstring ) > 15 )anyoutC ;
	anyout(" ") ;
	if( no_snips == 0 ){
		anyout("No snips with chosen selection criteria") ;
		finis_c() ;
	}
	status = 0 ;
	irds_create_c( irds_out , instrument , axis_out , centre , size , cosys ,
			&epoch , object , observer , &status ) ;
	if( status ){
		anyout( "error creating outset" ) ;
		finis_c() ;
	}
	status = 0 ;
	gdsd_rchar_c( irds , tofchar("BUNIT") , &zero , bunit , &status ) ;
	gdsd_wchar_c( irds_out , tofchar("BUNIT") , &zero , bunit , &status ) ;
	copy_snips( irds, instrument, irds_out, snip, no_snips, axis ) ;
	copy_header( irds , irds_out , snip , no_snips, axis_out ) ;

	gdsd_rreal_c( irds , tofchar("DATAMAX") , &zero , &fdummy , &status ) ;
	if( !status || status == -47 ){
		sprintf( Cstring , "INSET=%.*s" , nelc_c(irds_out) , irds_out.a ) ;
		cancel_c( tofchar("INSET=") ) ;
		wkey_c( tofchar(Cstring) ) ;
		deputy_c( tofchar("mnmx") , &status ) ;
	}

	status = 0 ;
	gds_close_c( irds_out , &status ) ;
	if( !new_irds ){
		status = 0 ;
		gds_delete_c( irds , &status ) ;
		(void)gds_rename_c( irds_out , irds ) ;
	}
	finis_c() ;
	return( 0 ) ;
}
