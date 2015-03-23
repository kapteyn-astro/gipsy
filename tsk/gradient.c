/***************************************************************************
****************************************************************************

                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>            gradient.dc1

Program:      GRADIENT

Purpose:      Calculates the gradients or the laplacian of a 
              map, using three by three sized templates.

Category:     CALCULATION

File:         gradient.c

Author:       Fred Lahuis

Keywords:

   INSET=     Set and subset(s) to be used.
              The maximum number of subsets is 50 and the maximun
              number of axis 10 (if desirable these values can be
              altered). The dimension of the subsets has to be 2.

   BOX=       Area to be used.  		        [whole subset]

   GRADIENT=  The kind of output wanted.                           [1]
			1 --> x and y gradient
			2 --> x gradient only
			3 --> y gradient only
			4 --> gradient and gradient-direction phi
                              (in radians anticlockwise from
                              positive x-axis)
			5 --> gradient only
			6 --> laplacian

   ASET=      First output set.
              If no name is entered <insetname>_xxx will be made,
              where xxx is gradx if gradient is 1 or 3, grady if
              gradient is 2, grad if gradient is 4 or 5.

   BSET=      Second output set, if GRAD= 1 or 4.
              If ASET is not entered <insetname>_xxx will be made,
              where xxx is grady if gradient is 1 and phi if gradient 
              is 4, else BSET is asked and a setname must be entered.

**BLANK=      Value used to substitute possible blanks             [0]
              in the map.

**CONSTANT=   Constant used in the gradient templates.       [sqrt(2)]
              Usually a value between 1 and 2 is used.

**FULL=       To determine the size of the output set(s).        [Y]/N
                    Y --> size of the input set
                    N --> size of the box

**SIGMA=      Value below which the gradient is                    [0]
              set to zero if its absolute value is less then this.

Comments:
              The values for BLANK, CONSTANT and SIGMA can be altered
              while the program is running, e.g. to enter a different 
              value for the next subset while the program is working
              on the present subset.

Description: 
              GRADIENT calculates the gradients in a map using three
              by three templates over the set and normalizing the
              result.

		             -1 0 1	                1  c  1
		(x templ.)   -c 0 c	   (y templ.)   0  0  0 
		       	     -1 0 1	               -1 -c -1

              The gradient (GRAD= 4 or 5) is taken as 

			grad = sqrt( sqr(buf_a) + sqr(buf_b) ).

              The laplacian is an approximation to the mathematical 
              laplacian. It is represented by a 'circular' template
              with a central value of -1 and four surrounding values
              of 0.25.

Comments:     Blank values in the map are treated as if being equal
              to zero, unless a value is given by BLANK, which is then
              used instead.

              If an existing set is overwritten the data outside
              the box is not removed.

              GRADIENT also updates the header item NOISE. This value is
              taken as the spread in data points, so to have a first
              guess of the true noise in the map.
              
Updates:      April 8 1991: FL Document created.
              Feb 25 1991:  FL Last update.

#<
****************************************************************************
****************************************************************************/

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "math.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"
#include "error.h"
#include "float.h"
#include "cancel.h"
#include "wkey.h"
#include "nelc.h"
#include "anyout.h"
#include "status.h"
#include "gdsbox.h"
#include "gdsasn.h"
#include "gdscss.h"
#include "gdscpa.h"
#include "gdsout.h"
#include "gdsc_fill.h"
#include "gdsc_grid.h"
#include "gdsc_ndims.h"
#include "gdsc_range.h"
#include "gdsinp.h"
#include "gdsi_read.h"
#include "gdsi_write.h"
#include "gdsd_wdble.h"
#include "setfblank.h"
#include "stabar.h"
#include "userchar.h"
#include "userint.h"
#include "userlog.h"
#include "userreal.h"
#include "wminmax.h"


#define  PROGRAM  "GRADIENT"
#define  VERSION  "1.1"

#define  inkey    tofchar("INSET=")
#define  akey     tofchar("ASET=")
#define  bkey     tofchar("BSET=")
#define  boxkey   tofchar("BOX=")
#define  gradkey  tofchar("GRADIENT=")
#define  ckey     tofchar("CONSTANT=")
#define  sigmakey tofchar("SIGMA=")
#define  blankkey tofchar("BLANK=")
#define  fullkey  tofchar("FULL=")
#define  noisekey tofchar("NOISE=")
#define	 inmess		tofchar("Set and subset to work on:")
#define  boxmess	tofchar( "Give area to work on;" )
#define	 gradmess	tofchar( "Type of output wanted: [1]" )
#define  blankmess	tofchar("Value to replace blanks with [0]")  
#define  cmess		tofchar("Template constant [sqrt 2]")
#define  sigmamess	tofchar("Sigma value [0]")
#define  fullmess	tofchar("Output set same size as input set [Y]/N?")

#define  MAXAX    10
#define  MAXSUB   50
#define  MAXBUF   25600		/** maximum size of the output buffers **/

static fint	hidden = 2 ;
#define  HIDDEN   (&hidden)
static fint	request = 1 ;
#define  REQUEST  (&request)
static fint	none = 0 ;
#define  NONE     (&none)
static fint	stndout = 11 ;
#define  STDOUT   (&stndout)
static fint	test = 16 ;
#define  TEST     (&test)

#define  PI		3.141592653589793
#define  sqr(a)		(a)*(a)
#define  max(a,b)	((a) > (b)) ? (a) : (b)
#define  min(a,b)	((a) < (b)) ? (a) : (b)
#define  fmake(fchr,size)	{ \
					static char buff[size+1] ; \
					int i ; \
					for( i = 0 ; i < size ; buff[i++] = ' ') ; \
					buff[i] = 0 ; \
					fchr.a = buff ; \
					fchr.l = size ; \
				}
static fint *nblanks ;

static float	*datamin_a, *datamax_a, *datamin_b, *datamax_b ;
static float	stamin, stamax, stapos ;

static char	Cstring[250] ; 

void testout( char *string )
{
	anyout_c( TEST, tofchar(string) ) ;
	return ;
}
void status()
{
	stapos++ ; stabar_c( &stamin, &stamax, &stapos ) ;
	return ;
}
void add_noise(	double	*noise,
		double	*noise_sq,
		fint	*noise_np,
		float	value,
		float	blank )
{
	if( value != blank ){
		*noise_np += 1 ;
		*noise = 100*value - 99*value ;
		*noise_sq = 100*sqr(value) - 99*sqr(value) ;
	}
	return ;
}
void minmax(	float	*datamin,
		float	*datamax,
		float	value,
		fint	no,
		float	blank )
{
	if( value != blank ){
		datamin[no] = min(datamin[no],value) ;
		datamax[no] = max(datamax[no],value) ;
	}
	return ;
}
void det_grad(	fint	grad,
		fint	ng,
		fint	no,
		float	clip,
		float	c,
		float	sum_c,
		float	*buf_a,
		float	*buf_b,
		float	*buf1,
		float	*buf2,
		float	*buf3,
		float	blank )
{
float	gx ;

	if( grad != 3 ){				/** x gradient **/
		buf_a[ng] = -buf3[no-1] - c*buf2[no-1] - buf1[no-1] 
			    + buf3[no+1] + c*buf2[no+1] + buf1[no+1] ;
		buf_a[ng] /= sum_c ;
		if( clip && fabs(buf_a[ng]) < clip )buf_a[ng] = 0.0 ;
	}
	else{						/** y gradient only **/
		buf_a[ng]  = buf3[no-1] + c*buf3[no] + buf3[no+1]
			    - buf1[no-1] - c*buf1[no] - buf1[no+1] ;
		buf_a[ng] /= sum_c ;
		if( clip && fabs(buf_a[ng]) < clip )buf_a[ng] = 0.0 ;
	}
	if( grad == 1 || grad == 4 || grad == 5 ){	/** y gradient **/
		buf_b[ng]  = buf3[no-1] + c*buf3[no] + buf3[no+1]
			    - buf1[no-1] - c*buf1[no] - buf1[no+1] ;
		buf_b[ng] /= sum_c ;
		if( clip && fabs(buf_b[ng]) < clip )buf_b[ng] = 0.0 ;
	}
	if( grad == 4 || grad == 5 ){
		gx = buf_a[ng] ;  /** buf_a becomes the gradient **/
		buf_a[ng] = sqrt( sqr(gx) + sqr(buf_b[ng]) ) ;
		if( grad == 4 ){
			if( gx ){	/** buf_b becomes phi **/
				buf_b[ng] = atan( buf_b[ng]/gx ) ;
				if( gx > 0 ) buf_b[ng] += PI ;
				if( buf_b[ng] < 0 ) buf_b[ng] += 2*PI ;
			}
			else{
				if( buf_b[ng] > 0 ) buf_b[ng] = PI/2.0 ;
				if( buf_b[ng] < 0 ) buf_b[ng] = 3*PI/2.0 ;
				if( buf_b[ng] ==0 ) buf_b[ng] = blank ;
			}
		}	
	}
	return ;
}

void dograd(	fint grad,
		fint step,
		fint linesize,
		fchar set,
		fint *subset,
		fchar aset,
		fint *ass,
		fchar bset,
		fint *bss,
		fint ssno,
		fint *flo,
		fint *fhi,
		fint *glo,
		fint *ghi,
		fint low,
		fint high,
		float blank )
{
fint	ng, done, nd, no, error, one = 1, bufsize ;
fint	cwlo, cwhi, cwlo_a, cwhi_a, cwlo_b, cwhi_b ;
fint	tidi, tidi_a, tidi_b ;
fint	noise_np[] = {0,0} ;

float	laplace[] = { 1.0, 0.25 } ;
float	clip = 0.0, c = sqrt(2), sum_c ;
float	subst_blank = 0.0 ;

float	*buf1 = NULL, *buf2 = NULL, *buf3 = NULL ;
float	*buf_a = NULL, *buf_b = NULL ;

double	noise[] = {0.0,0.0}, noise_sq[] = {0.0,0.0} ;

	testout( "allocating memory" ) ;
	bufsize = step*linesize ;
	buf1 = malloc( linesize*sizeof(float) ) ;
	buf2 = malloc( linesize*sizeof(float) ) ;
	buf3 = malloc( linesize*sizeof(float) ) ;
	buf_a = malloc( bufsize*sizeof(float) ) ;
	if( grad == 1 || grad == 4 || grad == 5 )
		buf_b = malloc( bufsize*sizeof(float) ) ;
	for( no = 0 ; no < step ; no++ ){
		buf_a[no*linesize] = buf_a[(no+1)*linesize-1] = blank ;
		if( grad == 1 || grad == 4 || grad == 5 )buf_b[no*linesize] = buf_b[(no+1)*linesize-1] = blank ;
	}

	sprintf( Cstring, "Working on subset %d", ssno+1 ) ;
	status_c( tofchar(Cstring) ) ;
	stabar_c( &stamin, &stamax, &stapos ) ;

	testout( "getting hidden input" ) ;
	no = userreal_c( &subst_blank, &one, HIDDEN, blankkey, blankmess ) ;
	no = userreal_c( &c, &one, HIDDEN, ckey, cmess ) ;
	sum_c = 4.0 + 2*c ;
	clip = 0 ;
	no = userreal_c( &clip, &one, HIDDEN, sigmakey, sigmamess ) ;
	
/**********
	The input data is stored in three buffers of size equal to the width of the set
	(buf1, buf2, and buf3). The input is read line by line in buf3 after having 
	copied buf2 to buf1 and buf3 to buf2. The output buffer has a size of 
	several lines (size = step).
	The template is then shifted over these three buffers and the result is
	stored in the working line of the output buffers, buf_a and /or buf_b
	both with a size of "step" lines.
**********/

	datamax_a[ssno] = -FLT_MAX ; datamin_a[ssno] = FLT_MAX ;
	if( grad == 1 || grad == 4 ){
		datamax_b[ssno] = -FLT_MAX ; datamin_b[ssno] = FLT_MAX ;
	}

/**** the first two lines are read before starting the calculation ***/
	flo[1] = low ; fhi[1] = low ;
	cwlo = gdsc_fill_c( set, &subset[ssno], flo ) ;
	cwhi = gdsc_fill_c( set, &subset[ssno], fhi ) ;
	tidi = 0 ;
	testout( "reading first line" ) ;
	gdsi_read_c( set, &cwlo, &cwhi, buf2, &linesize, &nd, &tidi ) ;
	if( tidi < 0 ) anyout_c( STDOUT, tofchar("error in gdsi_read 1") ) ;
	flo[1]++ ; fhi[1]++ ;
	cwlo = gdsc_fill_c( set, &subset[ssno], flo ) ;
	cwhi = gdsc_fill_c( set, &subset[ssno], fhi ) ;
	tidi = 0 ;
	testout( "reading second line" ) ;
	gdsi_read_c( set, &cwlo, &cwhi, buf3, &linesize, &nd, &tidi ) ;
	if( tidi < 0 ) anyout_c( STDOUT, tofchar("error in gdsi_read 2") ) ;
	for( no = 0 ; no < linesize ; no++ ){
		if( buf2[no] == blank )buf2[no] = subst_blank ;
		if( buf3[no] == blank )buf3[no] = subst_blank ;
	}
	glo[1] = low + 1 ; ghi[1] = high - 1 ;						/** from the second to the forelast line **/
	flo[1] = low + 2 ; fhi[1] = high ;						/** from the third to the last line **/
	cwlo = gdsc_fill_c( set, &subset[ssno], flo ) ;
	cwhi = gdsc_fill_c( set, &subset[ssno], fhi ) ;
	cwlo_a = gdsc_fill_c( aset, &ass[ssno], glo ) ;
	cwhi_a = gdsc_fill_c( aset, &ass[ssno], ghi ) ;
	if( grad == 1 || grad == 4 ){
		cwlo_b = gdsc_fill_c( bset, &bss[ssno], glo ) ;
		cwhi_b = gdsc_fill_c( bset, &bss[ssno], ghi ) ;
	}
	tidi = tidi_a = tidi_b = 0 ;
	testout( "doing the rest" ) ;
	do{
		done = 0 ;
		do{
			for( no = 0 ; no < linesize ; no++ ){
				buf1[no] = buf2[no] ;
				buf2[no] = buf3[no] ;
			}
			gdsi_read_c( set, &cwlo, &cwhi, buf3, &linesize, &nd, &tidi ) ;
			if( tidi < 0 ) anyout_c(  STDOUT, tofchar("error in gdsi_read 3") ) ;
			for( no = 0 ; no < linesize ; no++ )if( buf3[no] == blank )buf3[no] = subst_blank ;
			if( grad != 6 ){
				for( no = 1 ; no < linesize - 1 ; no++ ){
					ng = done*linesize + no ; 			/** number on line in output buffer **/
					det_grad( grad, ng, no, clip, c, sum_c,
						buf_a, buf_b, buf1, buf2, buf3, blank ) ;
					minmax(	datamin_a, datamax_a, buf_a[ng], ssno, blank ) ;
					add_noise( &noise[0], &noise_sq[0], &noise_np[0], buf_a[ng], blank ) ;
					if( grad == 1 || grad == 4 ){
						minmax(	datamin_a, datamax_b, buf_b[ng], ssno, blank ) ;
						add_noise( &noise[1], &noise_sq[1], &noise_np[1], buf_b[ng], blank ) ;
					}
					
				}
			}
			else{
				for( no = 1 ; no < linesize - 1 ; no++ ){
					ng = done*linesize + no ;
					buf_a[ng]  = laplace[1] *
							( buf1[no] + buf2[no-1] + buf2[no+1] + buf3[no] ) 
						      - laplace[0] * buf2[no] ;
					minmax(	datamin_a, datamax_a, buf_a[ng], ssno, blank ) ;
					add_noise( &noise[0], &noise_sq[0], &noise_np[0], buf_a[ng], blank ) ;
				}
			}
			if( !tidi ) break ; 	/** read last line, exit **/
 		}while( ++done != step ) ;

/** write output buffer(s) **/
		gdsi_write_c( aset, &cwlo_a, &cwhi_a, buf_a, &bufsize, &nd, &tidi_a ) ;
		if( tidi_a < 0 ) anyout_c(  STDOUT, tofchar("error in gdsi_write") ) ;
		if( grad == 1 || grad == 4 ){
			gdsi_write_c( bset, &cwlo_b, &cwhi_b, buf_b, &bufsize, &nd, &tidi_b ) ;
			if( tidi_b < 0 ) anyout_c(  STDOUT, tofchar("error in gdsi_write") ) ;
		}
		status() ;
	}while( tidi_a ) ;

/** 
    determine noise and write to header 
    noise == spread in the data points as a rough estimate of the noise
**/
	testout( "writing NOISE" ) ;
	noise[0] = sqrt( noise_sq[0]/(float)noise_np[0] - 
			sqr(noise[0]/(float)noise_np[0]) ) ;
	error = 0 ;
	gdsd_wdble_c( aset, noisekey, &ass[ssno], &noise[0], &error ) ;
	if( error < 0 ) anyout_c( STDOUT, tofchar("error writing buffer 1") ) ;
	if( grad == 1 || grad == 4 ){
		noise[1] = sqrt( noise_sq[1]/(float)noise_np[1] - 
				sqr(noise[1]/(float)noise_np[1]) ) ;
		error = 0 ;
		gdsd_wdble_c( bset, noisekey, &bss[ssno], &noise[1], &error ) ;
		if( error < 0 ) anyout_c( STDOUT, tofchar("error writing buffer 2") ) ;
	}
	free( buf1 ) ;
	free( buf2 ) ;
	free( buf3 ) ;
	free( buf_a ) ;
	free( buf_b ) ;
	return ;
}

MAIN_PROGRAM_ENTRY
{
fint	axnum[MAXAX], axnum_a[MAXAX], axnum_b[MAXAX] ;
fint	axsize[MAXAX], axsize_a[MAXAX], axsize_b[MAXAX] ;
fint	no, error, device = 11 ;
fint	full = 1, grad = 1, toplevel = 0, option = 0 ;
fint	step, one = 1, class = 1 ;
fint	maxaxes = MAXAX, maxsub = MAXSUB ;
fint	nsub, xsub, ysub, setdim, subdim = 2 ;
fint	ssno, subset[MAXSUB], ass[MAXSUB],bss[MAXSUB] ;
fint	lo[2], hi[2], cwlo, cwhi ;
fint	flo[MAXAX], fhi[MAXAX], glo[MAXAX], ghi[MAXAX], low, high ;
fint	size, linesize ;

float	blank ;

fchar	message ;
fchar	set, aset, bset ;

	init_c() ;
	IDENTIFICATION( PROGRAM, VERSION ) ;
	setfblank_c( &blank ) ;

	fmake( set, 80 ) ;
	fmake( aset, 80 ) ;
	fmake( bset, 80 ) ;
	
	nsub = gdsinp_c( set , subset , &maxsub, NONE, inkey, inmess,
			&device, axnum, axsize , &maxaxes, &class , &subdim ) ;
	setdim = gdsc_ndims_c( set, &toplevel ) ;
	error = 0 ; gdsc_range_c( set, &toplevel, &cwlo, &cwhi, &error ) ;
	for( no = 0 ; no < setdim ; no++ ){
		error = 0 ; flo[no] = gdsc_grid_c( set, &axnum[no], &cwlo, &error ) ;
		error = 0 ; fhi[no] = gdsc_grid_c( set, &axnum[no], &cwhi, &error ) ;
	}
	lo[0] = flo[0] ; lo[1] = flo[1] ; hi[0] = fhi[0] ; hi[1] = fhi[1] ;

	gdsbox_c(flo, fhi, set, subset, REQUEST, boxkey, boxmess, &device, &option ) ;
	for( no = 0 ; no < setdim ; no++ ){ glo[no] = flo[no] ; ghi[no] = fhi[no] ; }
	low = flo[1] ; high = fhi[1] ;

	no = userint_c( &grad, &one, REQUEST, gradkey, gradmess ) ;
/************************************
	grad = 1 > x- and y-gradient
	grad = 2 > x-gradient
	grad = 3 > y-gradient
	grad = 4 > gradient and phi
	grad = 5 > gradient
	grad = 6 > laplace operator
************************************/
	no = userlog_c( &full, &one, HIDDEN, fullkey, fullmess ) ;
							/*** true  --> output size will be the same as the size of the input set 
							     false -->  ,,   ,,  ,,  have the size of the box
							***/
	if( full ){
		glo[0] = lo[0] ; glo[1] = lo[1] ;
		ghi[0] = hi[0] ; ghi[1] = hi[1] ;
	}
	gdsasn_c( inkey, akey, &class ) ;
	gdscss_c( akey, glo, ghi ) ;
	if( full ){
		glo[0] = flo[0] ; glo[1] = flo[1] ;
		ghi[0] = fhi[0] ; ghi[1] = fhi[1] ;
	}
       	if( grad == 1 || grad == 2 )message = tofchar( "Give set for the x gradient: " ) ;
       	if( grad == 3 )message = tofchar( "Give set for the y gradient: " ) ;
       	if( grad == 4 || grad == 5 )message = tofchar( "Give set for the gradient: " ) ;
       	if( grad == 6 )message = tofchar( "Give set for the laplacian: " ) ;
	no = userchar_c( aset, &one, REQUEST, akey, message ) ;
	if( no == 0 ){
		if( grad == 1 || grad == 2 )sprintf( aset.a, "%.*s_gradx", nelc_c(set), set.a ) ;
		if( grad == 3 )sprintf( aset.a, "%.*s_grady", nelc_c(set), set.a ) ;
		if( grad == 4 || grad == 5 )sprintf( aset.a, "%.*s_grad", nelc_c(set), set.a ) ;
		if( grad == 6 )sprintf( aset.a, "%.*s_laplace", nelc_c(set), set.a ) ;
		cancel_c( akey ) ;
		sprintf( Cstring, "ASET=%.*s", nelc_c( aset ), aset.a ) ;
		wkey_c( tofchar(Cstring) ) ;
	}
	xsub = gdsout_c(aset, ass, &nsub, HIDDEN, akey, message, &device, 
			axnum_a, axsize_a, &maxaxes ) ;
/** 
	second output set if necessary
**/
	if( grad == 1 || grad == 4 ){
		if( no == 0 ){
			if( grad == 1 )sprintf( bset.a, "%.*s_grady", nelc_c(set), set.a ) ;
			if( grad == 4 )sprintf( bset.a, "%.*s_phi", nelc_c(set), set.a ) ;
			cancel_c( bkey ) ;
			sprintf( Cstring, "BSET=%.*s", nelc_c( bset ), bset.a ) ;
			wkey_c( tofchar(Cstring) ) ;
		}
		message = ( grad == 4 ) ? tofchar( "Give set for phi: " ) :
					  tofchar( "Give set for the y gradient: " ) ;
		if( full ){
			glo[0] = lo[0] ; glo[1] = lo[1] ;
			ghi[0] = hi[0] ; ghi[1] = hi[1] ;
		}
		gdsasn_c( inkey, bkey, &class ) ;
		gdscss_c( bkey, glo, ghi ) ;
		if( full ){
			glo[0] = flo[0] ; glo[1] = flo[1] ;
			ghi[0] = fhi[0] ; ghi[1] = fhi[1] ;
		}
		if( no == 0 ) ysub = gdsout_c(bset, bss, &nsub, HIDDEN, bkey, message,
				&device, axnum_b, axsize_b, &maxaxes ) ;
		else ysub = gdsout_c(bset, bss, &nsub, NONE, bkey, message,
				&device, axnum_b, axsize_b, &maxaxes ) ;
	}
	linesize = fhi[0] - flo[0] + 1 ;
	step = min( (int)MAXBUF/linesize, (fhi[1]-flo[1]-1) ) ;	
/** 
    number of lines in the output buffers, never being larger than the total
    number of lines minus two (because the first and last line are undefined.
**/
	size = nsub*sizeof(float) ;
		datamin_a = malloc( size ) ; datamax_a = malloc( size ) ;
		if( grad == 1 || grad == 4 ){
			datamin_b = malloc( size ) ; datamax_b = malloc( size ) ;
		}
	nblanks = malloc( nsub*sizeof(int) ) ;

	stamin = 0.0 ; stamax = (float)(fhi[1]-flo[1]-1)*nsub/(float)step ; stapos = 0.0 ;
	for( ssno = 0 ; ssno < nsub ; ssno++ )
		dograd( grad, step, linesize,
			set, subset, aset, ass, bset, bss, ssno,
			flo, fhi, glo, ghi, low, high, blank ) ;

	wminmax_c( aset, ass, datamin_a,  datamax_a, nblanks, &nsub, &one ) ;
	if( grad == 1 || grad == 4 )wminmax_c( bset, bss, datamin_b,  datamax_b, nblanks, &nsub, &one ) ;	
								/** header update **/
	finis_c() ;
	return(0) ;
}
