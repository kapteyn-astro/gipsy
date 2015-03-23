/* iras_root.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>	      iras_root.dc3

Function:     iras_root

Purpose:      provide a string containing the basename of the IRAS server

Category:     IRAS

File:	      iras_root.c

Author:       Do Kester

Use:  integer iras_root_c(
	root )		O	character*(*)

	root	the name of the IRAS root (enclosed in backslashes)

	returns the number of characters in root
		-1 : not enough characters in root

Dependent:    System dependent name; now : /net/hermes/SRONproj/iras/server/

Updates:      29 Nov 1991: DK, Creation date
	      27 Mar 2003: DK, move to another storage system
	      02 Feb 2011: JPT, yet another storage system
#<

Fortran to C interface:

@ integer function iras_root( character )

*/

#include	"gipsyc.h"
#include	"string.h"

/* iras_root should be to /net/ouranos/Data/iras/server/
   problems with the dns server prevent this
   use ip number in stead
*/
#define 	IRASROOT	"/Software/users/irasadm/server/"
#define 	ROOTLENG	31		/* number of char in root */
#define 	TOOSHORT	-1		/* not enough chars in root */

fint iras_root_c(
	fchar	root )
{
	if ( root.l <= ROOTLENG ) return( TOOSHORT ) ;
	strcpy( root.a, IRASROOT ) ;
	return( ROOTLENG ) ;
}

