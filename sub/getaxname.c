/* getaxname.c
                           COPYRIGHT (c) 1990
                            Kapteyn Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>            getaxname.dc2

Subroutine:   GETAXNAME

Purpose:      Get the axis name for an axis and clean up the name if 
              appropriate and wanted.

Category:     COORDINATES

File:         getaxname.c

Author:       M.G.R. Vogelaar

Use:          Fortran/Sheltran:
              
              CALL GETAXNAME( setname    ,   Input   CHARACTER*(*) 
                              axnum      ,   Input   INTEGER
                              chop       ,   Input   INTEGER
                              axname     ,   Output  CHARACTER*(*) 
                            )
 
              C:

              getaxname( setname,      Input  fchar  
                         axnum,        Input  fint   
                         chop,         Input  fint   
                         axname,       Output fchar  
                       )

              setname: Name of set from which to extract axis name
              axnum:   Axis number as returned by gdsinp
              chop:    If <> 0 allow to remove characters after dash
                       in name. Parameter type axis names are returned
                       without chopping.
              axname:  The processed axis name.


Description:  Axis names are stored in FITS item CTYPEn where n is the
              number of the axis. Usually these names contain a dash
              followed by projection information which is removed
              before displaying the name. However since the use of
              parameter type axes, the information after the dash 
              is important to distinguish for example the two axes 
              PAR-SLICE and PAR-OFFSET in one set. This routine 
              removes the dash and information that follows for 
              axes that are not of parameter type. You allow this 
              behaviour by entering a value for 'chop' unequal to 0.


Updates:      Sep 25, 1999: VOG, Document created.

#<      
Fortran to C interface:
@ subroutine getaxname( character, integer, integer, character )
*/

#include    "stddef.h"
#include    "stdio.h"
#include    "string.h"
#include    "gipsyc.h" 
#include    "gdsc_name.h"
#include    "axtype.h"
#include    "nelc.h"


void getaxname_c( fchar  Setin,
                  fint  *axnum,
                  fint  *chop,                  
                  fchar  Name )
/*------------------------------------------------------------*/
/* PURPOSE: Return axis name. If allowed chop the name to     */
/* avoid having the projection information. For parameter     */
/* type axes, return the complete name.                       */
/*------------------------------------------------------------*/
{
   fchar   Ctype, Cunit, Dunit;
   char    b1[21], b2[21], b3[21];
   fint    r;
   fint    typenr;
   fint    skysys, prosys, velsys;
   
   Ctype.a = b1;
   Ctype.l = 20;
   Cunit.a = b2;
   Cunit.l = 20;
   Dunit.a = b3;
   Dunit.l = 20;   
   r = 0;
   gdsc_name_c( Ctype, Setin, axnum, &r );
   if (r < 0)
   {
      strcpy( Name.a, "" );
      return;
   }
   Ctype.a[nelc_c(Ctype)] = '\0';
   Ctype.l = strlen(Ctype.a);
   typenr = axtype_c( Ctype, Cunit, Dunit, &skysys, &prosys, &velsys );

   if (typenr != 10 && *chop != 0)  /* Parameter axis */
   {
      strcpy( Name.a, strtok( Ctype.a, " -" ) ); 
   }
   else
   {
      strcpy( Name.a, Ctype.a );
   }
}

