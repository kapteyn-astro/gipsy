/* julianday.c

                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.
        
#>            julianday.dc2       

Function:     JULIANDAY

Purpose:      Convert a date in the Julian or Gregorian calendars into the
              corresponding Julian Day number.

Category:     COORDINATES

File:         julianday.c

Author:       M.G.R. Vogelaar

Use:          DOUBLE JULIANDAY( DAY,         Input      DOUBLE PRECISION
                                MONTH,       Input      DOUBLE PRECISION
                                YEAR,        Input      DOUBLE PRECISION 
                              )

              DAY     Day (with decimals) of input data
              MONTH   Month of input data
              YEAR    Year of input data. If year is entered with 
                      decimals, take DAY=0.0 and MONTH=0.0
                      
Example:      DOUBLE PRECISION   JD
              JD = JULIANDAY( 0.0, 0.0, 1950.0 )
              JD = JULIANDAY( 22.5, 9, 2000.0 )
              
             
Notes:        The Julian Day begins at Greenwich mean noon that is at 12h
              Universal time (or 12h Ephemeris Time).                     
              After 15 October 1582, the Gregorian Calendar is used.

Updates:      Nov 6, 1992; VOG, Document created.

#<

Fortran to C interface:

@double precision function julianday( double precision, double precision, double precision )

*/

#include   "math.h"


double julianday_c( double *varday, double *varmonth, double *varyear )
/*----------------------------------------------------------------------*/
/* Purpose: Convert a date in the Julian or Gregorian calendars into the*/
/*          corresponding Julian Day number (JD).                       */
/*          The Julian Day begins at Greenwich mean noon that is at 12h */
/*          Universal time (or 12h Ephemeris Time).                     */
/*          After 15 October 1582, the Gregorian Calendar is used.      */
/*----------------------------------------------------------------------*/
{
   double  A, B;
   double  JD;
   double  Gregorian = 1582.0 + 10.0/12.0 + 15.0/365.25;
   double  day, month, year;
   

   day   = *varday;
   month = *varmonth;
   year  = *varyear;
   if (month < 3.0) { 
      year  -= 1.0; 
      month += 12.0; 
   }
   if ( (year + month/12.0 + day/365.25) >= Gregorian )  {		
      A = (double) ( (int)(year/100.0) );
      B = 2 - A + (double) ( (int)(A/4.0) );
   } else {
      B = 0;
   }
   if (year >= 0.0) {
      JD = (double) ( (int)( 365.25 * year ) ) + 
           (double) ( (int)( 30.6001 * (month + 1.0) ) ) +
           day + 1720994.5 + B;
   } else {
      JD = (double) ( (int)( 365.25 * year - 0.75 ) ) + 
           (double) ( (int)( 30.6001 * (month + 1.0) ) ) +
           day + 1720994.5 + B;                       
   }
   return( JD );
}
