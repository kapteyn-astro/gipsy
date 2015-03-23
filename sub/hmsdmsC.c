/* hmsdmsC.c
                           COPYRIGHT (c) 2000
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             hmsdmsC.h
#if !defined(_hmsdmsC_h_)
#define _hmsdmsC_h_
void hmsC( double degrees, char *convstr, int prec );
void dmsC( double degrees, char *convstr, int prec );
#endif
#<                          
*/

/*----------------------------------------------------------------------*/
/*
#>             hmsdmsC.dc2

Document:      HMSDMSC

Purpose:       Convert degrees to a string containing the hours,
               minutes and seconds and deg, min, sec.
                              
Category:      COORDINATES

File:          hmsdmsC.c

Author:        M. Vogelaar 

Description:   hms       Convert degrees to hours, minutes and seconds
               dms       Convert degrees to degrees, minutes and seconds
               
Comment:       The routines are NOT callable from FORTRAN.
               They are simple alternatives for hms_c/dms_c
               which are Fortran callable.


Updates:       28 Jul,  2000: VOG, Document created. 
#<
*/

/*----------------------------------------------------------------------*/

/*
#>             hmsC.dc2

Function:      hmsC

Purpose:       Convert degrees to hours, minutes and seconds
               
Category:      COORDINATES

File:          hmsdmsC.c 

Author:        M. Vogelaar 

Use:           hmsC( degrees, string, precision );

               degrees  : Input of position in degrees.
               string   : Output of character string containing hms.
               precision: Number of decimals in seconds.
                
Example:       char  hmsstr[80];

               hmsC( 87.6376, hmsstr, 2 );
               anyoutf( 1, "position RA: %s", hmsstr );

Comment:       This routine is NOT callable from FORTRAN.

Notes:

Updates:       28 Jul,  2000: VOG, Document created. 
#<
*/

/*----------------------------------------------------------------------*/

/*
#>             dmsC.dc2

Function:      dmsC

Purpose:       Convert degrees to hours, minutes and seconds
               
Category:      COORDINATES

File:          hmsdmsC.c 

Author:        M. Vogelaar 

Use:           dmsC( degrees, string, precision );

               degrees  : Input of position in degrees.
               string   : Output of character string containing dms.
               precision: Number of decimals in seconds.
                
Example:       char  dmsstr[80];

               dmsC( 87.6376, dmsstr, 1 );
               anyoutf( 1, "position DEC: %s", hmsstr );

Comment:       This routine is NOT callable from FORTRAN.

Notes:

Updates:       28 Jul,  2000: VOG, Document created. 
#<
*/


/*----------------------------------------------------------------------*/


#include "stdio.h"
#include "math.h"




void dmsC( double degrees, char *convstr, int prec )
/*------------------------------------------------------------------*/
/* PURPOSE: Convert degrees to deg/min/sec                          */
/*------------------------------------------------------------------*/ 
{
   double    seconds;
   int       Idegs;
   double    min;
   int       Imin;
   int       negative;
   double    power;
   int       seclen = 2 + 1 + prec; /* ss.sss */

   power = pow( 10.0, (double) prec );
   negative = 0;
   if ( degrees < 0 ) {
      negative = 1;
      degrees = fabs(degrees);
   }
   Idegs   = (int) degrees;
   min     = degrees*60.0 - ((double)Idegs)*60.0;
   Imin    = (int) min;
   seconds = min*60.0 - ((double)Imin*60.0 );
   /* Avoid rounding by formatting */
   seconds = (double) ((int) (seconds * power) ) / power;
   if (negative)
   sprintf( convstr, "-%2dd%2dm%*.*fs", Idegs, Imin, seclen, prec, seconds );
   else
   sprintf( convstr,  "%2dd%2dm%*.*fs", Idegs, Imin, seclen, prec, seconds );
}



void hmsC( double degrees, char *convstr, int prec )
/*------------------------------------------------------------------*/
/* PURPOSE: Convert degrees to hours/min/sec                        */
/*------------------------------------------------------------------*/ 
{
   double    seconds;
   double    hours;
   int       Ihours;
   double    min;
   int       Imin;
   double    power;
   int       seclen = 2 + 1 + prec; /* ss.sss */   


   power   = pow( 10.0, (double) prec );
   degrees = fmod( (degrees + 360.0), 360.0 );
   hours   = degrees / 15.0;
   Ihours  = (int) hours;
   min     = hours*60.0 - ((double)Ihours)*60.0;
   Imin    = (int) ( min );
   seconds = min*60.0 - ((double)Imin)*60.0;
   seconds = (double) ((int) (seconds * power) ) / power;
   sprintf( convstr,  "%2dh%2dm%*.*fs", Ihours, Imin, seclen, prec, seconds );
}

