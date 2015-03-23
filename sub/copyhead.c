/* copyhead.c
                              COPYRIGHT (c) 1991
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>            copyhead.dc3

Document:     copyhead

Purpose:      Routines to copy header items from one set to another.

Category:     HEADER

File:         copyhead.c

Author:       Fred Lahuis

              COPYHEAD_ITEM       Routine to copy a number of given 
                                  header item from a given level in 
                                  the input set to a given level in 
                                  the output set.
              COPYHEAD_LEVEL      Routine to copy all header items
                                  at a given level in the input set
                                  to a given level in the output set.

Updates:      August 23 1991:    FL, Creation date.
              September 10 1991: FL, Copying of a maximum number of
                                     items at once added.
#<

*/
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "gipsyc.h"
#include "nelc.h"
#include "anyout.h"
#include "gdsd_delete.h"
#include "gdsd_find.h"
#include "gdsd_length.h"
#include "gdsd_read.h"
#include "gdsd_write.h"
#include "gdsd_rint.h"
#include "gdsd_rdble.h"

static int reserved_item( fchar dsc , fint *naxis )
{
	if( !strncmp( dsc.a , "NAXIS" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "CDELT" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "CRPIX" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "CRVAL" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "CTYPE" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "CUNIT" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "CROTA" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "DDELT" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "DROTA" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "DRPIX" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "DRVAL" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "DTYPE" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "DUNIT" , 5 ) )return(1) ;
	if( !strncmp( dsc.a , "EPOCH   " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "FREQ0   " , 8 ) )return(1) ;
	if( !strncmp( dsc.a , "INSTRUME" , 8 ) )return(1) ;
	return(0) ;
}

#define	 DSCBUFLEN  1024
#define  MAXITEMS   250
#define	 fmake(fchr,size)	{ \
				static char buff[size+1] ; \
				int i ; \
				for( i = 0 ; i < size ; buff[i++] = ' ') ; \
				buff[i] = 0 ; \
				fchr.a = buff ; \
				fchr.l = size ; \
			}

/* copyhead.c
                              COPYRIGHT (c) 1991
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>            copyhead_item.dc2

Subroutine:   COPYHEAD_ITEM

Purpose:      Copies header items from given levels in the input set to
              the output set.

Category:     HEADER

File:         copyhead.c

Author:       Fred Lahuis

Use:          CALL COPYHEAD_ITEM( ITEMS       INPUT char*(*)
                                  NITEMS      INPUT integer
                                  INSET       INPUT char*(*)
                                  OUTSET      INPUT CHAR*(*)
                                  INLEVEL     INPUT integer array
                                  OUTLEVEL    INPUT integer array
                                 )

              ITEMS     descriptor items to be copied.
                        The size of ITEMS must be NITEMS*20
                        and the seperate items must be stored 
                        in groups of 20 characters.
              NITEMS    number of items to be copied
              INSET     input set
              OUTSET    output set
              INLEVEL   coordinate words of the levels from which
                        the items are to be read
              OUTLEVEL  coordinate word of the levels to which the
                        items should be written.

Updates:      August 23 1991:    FL, Creation date.
              September 10 1991: FL, Copying of a maximum number of
                                     items at once (NITEMS) added.
#<

Fortran to C interface:

@ subroutine copyhead_item(	character,
@				integer,
@				character,
@				character,
@				integer,
@				integer )

*/

void copyhead_item_c(	fchar items,
			fint *nitems,
			fchar inset,
			fchar outset,
			fint *inlevel,
			fint *outlevel )
{
fint	no , n ;
fint	status ;
fint	bytes_left ;
fint	bytes_done_r , bytes_done_w ;
fint	position_r , position_w ;

fchar	item ;
char	buffer[DSCBUFLEN] ;

fmake( item , 20 ) ;
for( no = 0 ; no < *nitems ; no++ ){
	for( n = 0 ; n < 20 ; n++ ) item.a[n] = items.a[no*20+n] ;
	position_r = position_w = 1 ;
	status = 0 ;
	gdsd_delete_c( outset , item , &outlevel[no] , &status ) ;
	status = 0 ;
	bytes_left = gdsd_length_c( inset , item , &inlevel[no] , &status ) ;
	while( bytes_left ){
		if( bytes_left < DSCBUFLEN) bytes_done_r = bytes_left ;
		else bytes_done_r = DSCBUFLEN ;
		gdsd_read_c(	inset, item, &inlevel[no], (fint *)buffer,
				&bytes_done_r, &position_r, &bytes_done_r,
				&status ) ;
		position_r += bytes_done_r ;
		bytes_done_w = bytes_done_r ;
		gdsd_write_c(	outset, item, &outlevel[no], (fint *)buffer,
				&bytes_done_w, &position_w, &bytes_done_w,
				&status ) ;
		position_w += bytes_done_w ;
		bytes_left -= bytes_done_r ;
	}
}
return ;
}

/* copyhead.c
                              COPYRIGHT (c) 1991
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>            copyhead_level.dc2

Subroutine:   COPYHEAD_LEVEL

Purpose:      Copies all header items which are located at a given
              level in the input set, to a given level in the
              output set, except for reserved keywords.

Category:     HEADER

File:         copyhead.c

Author:       Fred Lahuis

Use:          CALL COPYHEAD_LEVEL( INSET       INPUT char*(*)
                                   OUTSET      INPUT CHAR*(*)
                                   INLEVEL     INPUT integer
                                   OUTLEVEL    INPUT integer
                                  )

              INSET     input set
              OUTSET    output set
              INLEVEL   coordinate word of the level from which
                        the items are to be read
              OUTLEVEL  coordinate word of the level to which the
                        items are to be written.

Comments:     Reserved keywords are not copied.
              
Updates:      August 23 1991:    FL, Creation date.
              September 10 1991: FL, Copying of a maximum number of
                                     items at once added.
              September 19 1991: FL, Check on reserved header items 
                                     added.
              August 25 1993:    FL, Minor bug removed.
#<

Fortran to C interface:

@ subroutine copyhead_level( character , character , integer , integer )

*/

void copyhead_level_c( fchar inset , fchar outset , fint *inlevel , fint *outlevel )
{
fint	recno = 0 , no , naxis ;
fint	level = 0 , nitems = 0 ;
fint	status ;
fint	level_out[MAXITEMS] , level_in[MAXITEMS] ;
fchar	dsc , items ;

	fmake( dsc , 20 ) ;
	fmake( items , 20*MAXITEMS ) ;
	status = 0 ;
	gdsd_rint_c( outset , tofchar("NAXIS") , &level , &naxis , &status ) ;
	do{
		level = 0 ;
		gdsd_find_c( dsc , inset , inlevel , &recno , &level ) ;
		if( level == *inlevel && !reserved_item(dsc,&naxis) ){
			for( no = 0 ; no < 20 ; no++ )items.a[nitems*20+no] = dsc.a[no] ;
			level_in[nitems] = *inlevel ;
			level_out[nitems] = *outlevel ;
			nitems++ ;
		}
		if( nitems == MAXITEMS || recno == 0 ){
			copyhead_item_c( items , &nitems , inset , outset , level_in , level_out ) ;
			nitems = 0 ;
		}
	}while( recno ) ;
	return ;
}



