/* dcdsetdef.h

	Copyright (c) Kapteyn Laboratorium Groningen 1993
	All Rights Reserved.

#>            dcdsetdef.dc3

Header:       dcdsetdef

Purpose:      Defines the type dcdset_struct which is returned by a call
              to dcdset.

Category:     USER IO

File:         dcdsetdef.h

Author:       K.G. Begeman

Description:  The dcdset_struct is defined as follows:

              typedef struct {
                 char setname[FILENAME_MAX+1]; // name of set
                 fint *subsets;                // decoded subset levels
                 fint status;                  // return status
              } dcdset_struct;

Related Docs: dcdset.dc2

Updates:      Nov 28, 1993: KGB, Document created.

#<

*/

typedef	struct {				/* set information */
   char	setname[FILENAME_MAX+1];		/* name of set */
   fint	*subsets;				/* subset levels */
   fint	status;					/* status */
} dcdset_struct;				/* the struct */

extern	dcdset_struct	*dcdset( char * );
