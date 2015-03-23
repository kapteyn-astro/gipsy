/* gdsd_grxxx.c

#>            gdsd_grxxx.dc3

Document:     GDSD_GRXXX

Purpose:      Contains descriptor read routines.

Category:     HEADER

File:         gdsd_grxxx.c

Author:       D. Kester

Description:  Refer to separate documents. Current members are the
              modules gdsd_grint, gdsd_grreal, gdsd_grchar,
              gdsd_grlog and gdsd_grdble.

Updates:      Jul 15, 1991: KGB, Document created.

#<
*/

/*
@ subroutine gdsd_grint( character, character, integer, integer, 
@			 integer, integer, integer )

#>              gdsd_grint.dc2

subroutine:     GDSD_GRINT

purpose:        read a descriptor from a specified level

category :	header

file:   gdsd_grxxx.c

author: Do Kester	do@guspace.rug.nl

use:    call gdsd_grint( 
             set_id,    I     	character*(*)
             key,       I     	character*(*)
	     axes,	I     	integer(n)
	     grid,	I	integer(n)
	     n,		I	integer
	     value,	O	integer
	     error )	O	integer

        set_id 		name to search the key in
        key		gds_descriptor to search for
	axes		list of defined axes 
			axes which are not mentioned are undefined
	grid		list of grid positions on the specified axes
	n		number of defined axes and grids
	value		returned value; 
			if not found value remains as it was upon input
        error		if succesful: level where the item was found
			if not: error code as in GDSD_Rxxx

description:    

externals:      GDSD_Rxxx 

updates:        15/11/90 DK, code
#< 
*/

#include "gipsyc.h"
#include "gdsd_rchar.h"
#include "gdsd_rint.h"
#include "gdsd_rreal.h"
#include "gdsd_rchar.h"
#include "gdsd_rlog.h"
#include "gdsd_rdble.h"
#include "gdsc_word.h"

			
void gdsd_grint_c( 
	fchar	set_id, 
	fchar	key, 
	fint	*axes,
	fint	*grid, 
	fint	*n, 
	fint	*value,
	fint	*error )

{
	fint		k, level;

	level = 0;
	for ( k = 0; k < *n; k++, axes++, grid++ ) {
		level = gdsc_word_c( set_id, axes, grid, &level, error );
		if ( *error ) return;
		}
	gdsd_rint_c( set_id, key, &level, value, error );
	return;
}

/*
@ subroutine gdsd_grreal( character, character, integer, integer, 
@			 integer, real, integer )

#>              gdsd_grreal.dc2

subroutine:     GDSD_GRreal

purpose:        read a descriptor from a specified level

category :	header

file:   gdsd_grxxx.c

author: Do Kester	do@guspace.rug.nl

use:    call gdsd_grreal( 
             set_id,    I     	character*(*)
             key,       I     	character*(*)
	     axes,	I     	integer(n)
	     grid,	I	integer(n)
	     n,		I	integer
	     value,	O	real
	     error )	O	integer

        set_id 		name to search the key in
        key		gds_descriptor to search for
	axes		list of defined axes 
			axes which are not mentioned are undefined
	grid		list of grid positions on the specified axes
	n		number of defined axes and grids
	value		returned value; 
			if not found value remains as it was upon input
        error		if succesful: level where the item was found
			if not: error code as in GDSD_Rxxx

description:    

externals:      GDSD_Rxxx 

updates:        15/11/90 DK, code
#< 
*/

void gdsd_grreal_c( 
	fchar	set_id, 
	fchar	key, 
	fint	*axes,
	fint	*grid, 
	fint	*n, 
	float	*value,
	integer *error )

{
	fint		k, level;

	level = 0;
	for ( k = 0; k < *n; k++, axes++, grid++ ) {
		level = gdsc_word_c( set_id, axes, grid, &level, error );
		if ( *error ) return;
		}
	gdsd_rreal_c( set_id, key, &level, value, error );
	return;
}


/*
@ subroutine gdsd_grchar( character, character, integer, integer, 
@			 integer, character, integer )

#>              gdsd_grchar.dc2

subroutine:     GDSD_GRchar

purpose:        read a descriptor from a specified level

category :	header

file:   gdsd_grxxx.c

author: Do Kester	do@guspace.rug.nl

use:    call gdsd_grchar( 
             set_id,    I     	character*(*)
             key,       I     	character*(*)
	     axes,	I     	integer(n)
	     grid,	I	integer(n)
	     n,		I	integer
	     value,	O	character*(*)
	     error )	O	integer

        set_id 		name to search the key in
        key		gds_descriptor to search for
	axes		list of defined axes 
			axes which are not mentioned are undefined
	grid		list of grid positions on the specified axes
	n		number of defined axes and grids
	value		returned value; 
			if not found value remains as it was upon input
        error		if succesful: level where the item was found
			if not: error code as in GDSD_Rxxx

description:    

externals:      GDSD_Rxxx 

updates:        15/11/90 DK, code
#< 
*/

void gdsd_grchar_c( 
	fchar	set_id, 
	fchar	key, 
	fint	*axes,
	fint	*grid, 
	fint	*n, 
	fchar	value,
	fint	*error )

{
	fint		k, level;

	level = 0;
	for ( k = 0; k < *n; k++, axes++, grid++ ) {
		level = gdsc_word_c( set_id, axes, grid, &level, error );
		if ( *error ) return;
		}
	gdsd_rchar_c( set_id, key, &level, value, error );
	return;
}


/*
@ subroutine gdsd_grlog( character, character, integer, integer, 
@			 integer, logical, integer )

#>              gdsd_grlog.dc2

subroutine:     GDSD_GRlog

purpose:        read a descriptor from a specified level

category :	header

file:   gdsd_grxxx.c

author: Do Kester	do@guspace.rug.nl

use:    call gdsd_grlog( 
             set_id,    I     	character*(*)
             key,       I     	character*(*)
	     axes,	I     	integer(n)
	     grid,	I	integer(n)
	     n,		I	integer
	     value,	O	logical
	     error )	O	integer

        set_id 		name to search the key in
        key		gds_descriptor to search for
	axes		list of defined axes 
			axes which are not mentioned are undefined
	grid		list of grid positions on the specified axes
	n		number of defined axes and grids
	value		returned value; 
			if not found value remains as it was upon input
        error 		if succesful: level where the item was found
			if not: error code as in GDSD_Rxxx

description:    

externals:      GDSD_Rxxx 

updates:        15/11/90 DK, code
#< 
*/

void gdsd_grlog_c( 
	fchar	set_id, 
	fchar	key, 
	fint	*axes,
	fint	*grid, 
	fint	*n, 
	bool	*value,
	fint	*error )

{
	fint		k, level;

	level = 0;
	for ( k = 0; k < *n; k++, axes++, grid++ ) {
		level = gdsc_word_c( set_id, axes, grid, &level, error );
		if ( *error ) return;
		}
	gdsd_rlog_c( set_id, key, &level, value, error );
	return;
}


/*
@ subroutine gdsd_grdble( character, character, integer, integer, 
@			 integer, double precision, integer )

#>              gdsd_grdble.dc2

subroutine:     GDSD_GRDBLE

purpose:        read a descriptor from a specified level

category :	header

file:   gdsd_grxxx.c

author: Do Kester	do@guspace.rug.nl

use:    call gdsd_grdble( 
             set_id,    I     	character*(*)
             key,       I     	character*(*)
	     axes,	I     	integer(n)
	     grid,	I	integer(n)
	     n,		I	integer
	     value,	O	double precision
	     error )	O	integer

        set_id 		name to search the key in
        key		gds_descriptor to search for
	axes		list of defined axes 
			axes which are not mentioned are undefined
	grid		list of grid positions on the specified axes
	n		number of defined axes and grids
	value		returned value; 
			if not found value remains as it was upon input
        error		if succesful: level where the item was found
			if not: error code as in GDSD_Rxxx

description:    

externals:      GDSD_Rxxx 

updates:        15/11/90 DK, code
#< 
*/

void gdsd_grdble_c( 
	fchar	set_id, 
	fchar	key, 
	fint	*axes,
	fint	*grid, 
	fint	*n, 
	double  *value,
	fint	*error )

{
	fint		k, level;

	level = 0;
	for ( k = 0; k < *n; k++, axes++, grid++ ) {
		level = gdsc_word_c( set_id, axes, grid, &level, error );
		if ( *error ) return;
		}
	gdsd_rdble_c( set_id, key, &level, value, error );
	return;
}


