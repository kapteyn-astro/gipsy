/* irlist.h

           Copyright (c) 1992
  Laboratory for Space Research Groningen 
       Kapteyn Laboratory Groningen 
           All Rights Reserved.

#> irlist.dc2
Include:      irlist.h

Purpose:      typdefs and declarations for lists.

Category:     IRAS

Author:       Do Kester

Description:
	A list is a number of snip.det combinations. The items are floats,
	whose integral part is the sequential snip number and the 
	fractional part is the `true' detector number divided by 100.
	The purpose of the list is either to perform some processing 
	for the items in the list only or for the complement of the list.

	Utilities are present to ask a list from the user, sort it,
	select items which are not in the present band, disjunct or merge
	2 lists, write the contents of a list to the screen/logfile,
	select detectors and decide whether a detector snip should
	be processed.

	irlist.h contains the typedef of struct list_type and the
	declarations of the pertaining utility functions.

Updates:      	04 Jun 1992: DK, Creation of this document
		03 Sep 1992: DK, EPSIL added
#<
*/

#define	LIST_TYPE	1	/* signal that list_type is defined */
#define EPSIL		0.001	/* to distinguish between detectors */

/* define a list_type */

typedef struct {
  float         *snip_det ;     /* snip.det combination */
  int           nl ;            /* number of snip_det's in list */
  int           complement;     /* FALSE: process list itself;
                                   TRUE : process complement of list */
} list_type ;

#define LIST_INIT( list) \
        list = (list_type*)malloc( sizeof( list_type ) ) ;\
        assert( list ) ; list->snip_det = 0

/* declarations of the utility functions for list_types */

int irim_userlist(			/* nr of items */
		list_type *,		/* any list */
		int,			/* default level */
		char*,			/* keyword */
		char* );		/* message */
void irim_cutlist(
		list_type *,		/* any list */
		int ) ;			/* length to cut to */
void irim_seldets( 	
		list_type *,		/* exclude	*/
		fint ) ;		/* band 	*/
void irim_detinband( 	
		list_type *, 		/* any list	*/
		int );			/* band number  */    
void irim_sortuniq( 	
		list_type * );		/* any list	*/
void irim_disjunct( 	
		list_type *, 		/* any list	*/
		list_type * );		/* any list	*/
void irim_merge( 
		list_type *,		/* list to be merged	*/
		list_type * );		/* receiving list	*/
void irim_wrlist( 
		int,			/* logfile */
		char *,			/* message	*/
		list_type * );		/* any list	*/
int irim_active( 			/* active ? 	*/
		int,			/* scan number 	*/
		list_type *,		/* any list	*/
		int *,			/* position in list	*/
		int *,			/* detector list	*/
		int * );		/* length of det list	*/

