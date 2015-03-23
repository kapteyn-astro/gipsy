/* axprops.c

                            COPYRIGHT (c) 1999
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.
*/

/*
#>            axprops.dc2

Function:     AXPROPS

Purpose:      Return structure with axis properties for a set.

Category:     FILES, IMAGE-IO,  USER-INTERFACE

File:         axprops.c 

Author:       M.G.R. Vogelaar

Use:          #include    "axprops.h"

              int   r;
              fchar Setin;
              atype axis[10];
              char  errmes[80];
              
              r = axprops( Setin, axis, errmes );
              anyoutf( 0, "name: %s", axis[0].name );              
             
              The struct 'atype' has members:

              int   blo;
              int   bhi;
              char  name[20];

Description:  If the input set exists, return the number of axes in 
              the set. Else return 0 and a message which contains the 
              error description.
              
Updates:      Nov 5, 1999: VOG, Document created.

#<   



#>             axprops.h

typedef struct
{
   int   blo;
   int   bhi;
   char  name[20];
}
atype;

int axprops( fchar, atype *, char * );               
#<
*/



#include    "stdio.h"
#include    "gipsyc.h"       /* Defines the ANSI-F77 types for F to C intf. */ 
#include    "axprops.h"
#include    "gds_exist.h" 
#include    "nelc.h"
#include    "gdsc_ndims.h"
#include    "gdsc_range.h"
#include    "gdsc_grid.h"
#include    "gds_errstr.h"
#include    "getaxname.h" 


int axprops( fchar Setin, atype *props, char *errmes )
/*------------------------------------------------------------*/
/* PURPOSE: Return axis properties of this set in a structure */
/*          of type 'atype' as defined in axprops.h.          */
/*------------------------------------------------------------*/
{
   fint   r = 0;
   fchar  Errstr;
   char   errstr[256];
   int    result;
   fint   setlevel = 0;
   fchar  Axname; 
   fint   chop = 1; 


   Errstr.a = errstr;
   Errstr.l = 255; 
   Axname.l = 20;                                    /* Max. axis name length */
     
   strcpy( errmes, "" );
   if (gds_exist_c(Setin, &r))
   {
      fint   setdim  = gdsc_ndims_c( Setin, &setlevel );
      /*-------------------------------*/
      /* Determine edges of this frame */
      /*-------------------------------*/
      {
         fint cwlo, cwhi;                          /* Local coordinate words */
         int  m;
         fint r2, r1 = 0;
         gdsc_range_c( Setin, &setlevel, &cwlo, &cwhi, &r1 );
         r1 = r2 = 0;
         for (m = 0; m < (int) setdim; m++)
         {
            fint   axnum = m + 1;
            Axname.a = props[m].name;            
            props[m].blo = gdsc_grid_c( Setin, &axnum, &cwlo, &r1 );
            props[m].bhi = gdsc_grid_c( Setin, &axnum, &cwhi, &r2 );
            getaxname_c( Setin, &axnum, &chop, Axname );
            Axname.a[nelc_c(Axname)] = '\0';
         }
      }
      result = setdim;
   }
   else
   {
      gds_errstr_c( Errstr, &r );   
      Errstr.a[nelc_c(Errstr)] = '\0';
      if (r == 0 )
         sprintf( errmes, "Set does not exist!" );
      else
         sprintf( errmes, "Set does not exist (%s)", Errstr.a );
      result = 0;
   }
   return( result );
}
