/* ircc_const.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            ircc_const.dc2

Purpose:      IRAS instrument parameters.

Category:     IR

File:         ircc_const.c

Author:       P.R. Roelfsema

Description:  In this file the following routines are present:

	      IRCC_ALIVE     - returns 1: alive; 0: dead; -1: nonexistent
	      IRCC_BANDET    - returns band number given a detector
              IRCC_BANDNR    - returns band number for instrument names.
              IRCC_DETNR     - returns detector numbers.
              IRCC_INSTRNAME - returns the IRAS instrument names.
              IRCC_MASK      - returns IRAS detector layout parameters.
              IRCC_NDETS     - returns the number of detectors.
              IRCC_OBSMODE   - returns observing mode for instrument names.
              IRCC_RATE      - returns the IRAS scan rates.
	      IRCC_SOLID     - returns effective solid angle of a detector
              IRCC_TIMES     - returns detector read-out time offsets.
              IRCC_SDET      - returns SDET nr for detector number

Updates:      Aug  8, 1990: PRR, Creation date
              Sep  6, 1990: PRR, various changes 
              Sep 10, 1990: PRR, changed device/bands to modes/bands.
	      Oct 31, 1990: DK,  added IRCC_BANDET IRCC_SOLID
	      Sep 24, 1992: DK,  added UNKNOWN to obsmode
              Aug 13, 1993: PRR, added IRCC_SDET
	      Dec 02, 1993: DK, strstr changed in strnstr because of
				problems with fortran strings
	      May 01, 2007: JPT, conflicting strnstr declaration changed
				
#<

*/

#include "gipsyc.h"
#include "string.h"
#include "stdlib.h"
#include "userfio.h"
#include "nelc.h"


#define BADDETNO	-1			/* illegal detector number */

#define MAXDETNO	75			/* higest IRAS detector nr. */
#define	MAXDETS		16			/* max number of detectors */
#define	MAXMODES	 5			/* max nuber of obs modess */
#define	SURVEY		 1			/* observing mode numbers */
#define	SPLINE		 2
#define AO		 3
#define FLASH		 4
#define UNKNOWN		 5
#define	MAXBAND		 6			/* max number of bands */
#define	B1		 1			/* band numbers */
#define	B2		 2
#define B3		 3
#define B4		 4
#define LRS		 5
#define BPHF		 6

/* static char *strnstr( char*, fint, char * ) ;  */
/* conflicsts with header definitiion. Rename */
static char *strzstr( char*, fint, char * ) ; 

						/* detector alive or dead? */
static char alive[ MAXDETNO + 1 ] = {
/*  0 */  -1, 1, 1, 1, 1, 1, 1, 1, 1, 1  , 1, 1, 1, 1, 1, 1, 1, 0, 1, 1 ,
/* 20 */   0, 1, 1, 1, 1, 1, 1, 1, 1, 1  , 1, 1, 1, 1, 1, 1, 0, 1, 1, 1 ,
/* 40 */   1, 1, 1, 1, 1, 1, 1, 1, 1, 1  , 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 ,
/* 60 */   1, 1, 1,-1,-1,-1,-1,-1,-1,-1  ,-1, 1, 1, 1, 1, 1 } ;

						/* survey detector numbers */
static fint detnrs[ MAXBAND + 1 ][ MAXDETS ] = {
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
      23, 24, 25, 26, 27, 28, 29, 30, 47, 48, 49, 50, 51, 52, 53, 54,
      16, 17, 18, 19, 20, 21, 22, 39, 40, 41, 42, 43, 44, 45, 46, -1,
       8,  9, 10, 11, 12, 13, 14, 15, 31, 32, 33, 34, 35, 36, 37, 38,  
       1,  2,  3,  4,  5,  6,  7, 55, 56, 57, 58, 59, 60, 61, 62, -1,
      71, 72, 73, 74, 75, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 } ;

						/* nr. of detectors per band */
static fint ndets[ MAXBAND + 1 ] = { 0 , 16 , 15 , 16, 15 , 5 , 0 } ;

						/* detector -> band  */
static char bandet[ MAXDETNO + 1 ] = {
/*  0 */  -1, 4, 4, 4, 4, 4, 4, 4, 3, 3,   3, 3, 3, 3, 3, 3, 2, 2, 2, 2,
/* 20 */   2, 2, 2, 1, 1, 1, 1, 1, 1, 1,   1, 3, 3, 3, 3, 3, 3, 3, 3, 2,
/* 40 */   2, 2, 2, 2, 2, 2, 2, 1, 1, 1,   1, 1, 1, 1, 1, 4, 4, 4, 4, 4,
/* 60 */   4, 4, 4,-1,-1,-1,-1,-1,-1,-1  ,-1, 5, 5, 5, 5, 5 } ;

						/* mode names */
static char obsmodes[ MAXMODES + 1 ][ 10 ] = { 
       "   " , "SURVEY" , "SPLINE" , "AO" , "FLASH", "UNKN" } ;

						/* sampling rates */
static fint rates[ MAXBAND + 1 ][ MAXMODES + 1 ] = {
         -1  ,   -1     ,   -1     , -1   ,   -1  ,   -1 ,
         -1  ,   16     ,    1     , 16   ,   16  ,   16 ,
         -1  ,   16     ,    1     , 16   ,   16  ,   16 ,
         -1  ,    8     ,    1     ,  8   ,    8  ,    8 ,
         -1  ,    4     ,    1     ,  4   ,    4  ,    4 ,
         -1  ,   32     ,   32     , 32   ,   32  ,   32 ,
         -1  ,    1     ,    1     ,  1   ,    1  ,    1 } ;

						/* band names */
static char bands[ MAXBAND + 1 ][ 6 ] = {
       " " , "B1" , "B2" , "B3" , "B4" , "LRS" , "BPHF" } ;
                                     

/*
      Numbers in table 'times' are those from SAT/Ground ICD table
      3.4.tab.1, converted to satcal units:
      times-value = ( 2 * icd-value + 1 ) / 2048
*/
static float times[ MAXDETNO ] = {
		/* detector 1-7 band IV */
   0.04150, 0.10205, 0.10400, 0.16455, 0.16650, 0.22705, 0.22900,
		/* detector  8-15 band III */
   0.03271, 0.03467, 0.03662, 0.03857, 0.09521, 0.09717, 0.09912, 0.10107,
		/* detector 16-22 band II */
   0.00244, 0.00439, 0.00635, 0.00830, 0.01025, 0.01221, 0.01416,
		/* detector 23-30 band I */
   0.01709, 0.01904, 0.02100, 0.02295, 0.02490, 0.02686, 0.02881, 0.03076,
		/* detector 31-38 band III */
   0.03174, 0.03369, 0.03564, 0.03760, 0.09424, 0.09619, 0.09814, 0.10010,
		/* detector 39-46 band II */
   0.00146, 0.00342, 0.00537, 0.00732, 0.00928, 0.01123, 0.01318, 0.01514,
		/* detector 47-54 band I */
   0.01611, 0.01807, 0.02002, 0.02197, 0.02393, 0.02588, 0.02783, 0.02979,
		/* detector 55-62 band IV */
   0.04053, 0.04248, 0.10303, 0.10498, 0.16553, 0.16748, 0.22803, 0.22998,
		/* detector 71-73 band V; 74-75 band VI; */
   0 , 0 , 0  , 0 , 0 , 0 , 0 , 0 , 0.052, 0.048, 0.044, 0.036, 0.032 } ;

/*
      Numbers in table 'edelay' are based on the timing information
      in the Faint Source Survey explanatory supplement. When compared
      with the timing information of the IRAS explan. supp. it was
      found that for each band there is a constant timing offset due
      to electronic delay:
                band I   :  36.9 +/- 0.1 msec.
                band II  :  33.3 +/- 0.1 msec.
                band III :  80.4 +/- 0.3 msec.
                band IV  : 147.8 +/- 0.3 msec.
                band V   :
                band VI  :  26.5  msec. (LRS)
      To compensate for electronic delay these times are subtracted
      from the readout times.
*/
static float edelay[ MAXDETNO ] = {
		/* detector 1-7 band IV */
     0.1478 ,0.1478 ,0.1478 ,0.1478 ,0.1478 ,0.1478 ,0.1478 ,
		/* detector  8-15 band III */
     0.0804 ,0.0804 ,0.0804 ,0.0804 ,0.0804 ,0.0804 ,0.0804 ,0.0804 ,
		/* detector 16-22 band II */
     0.0333 ,0.0333 ,0.0333 ,0.0333 ,0.0333 ,0.0333 ,0.0333 ,
		/* detector 23-30 band I */
     0.0369 ,0.0369 ,0.0369 ,0.0369 ,0.0369 ,0.0369 ,0.0369 ,0.0369 ,
		/* detector 31-38 band III */
     0.0804 ,0.0804 ,0.0804 ,0.0804 ,0.0804 ,0.0804 ,0.0804 ,0.0804 ,
		/* detector 39-46 band II */
     0.0333 ,0.0333 ,0.0333 ,0.0333 ,0.0333 ,0.0333 ,0.0333 ,0.0333 ,
		/* detector 47-54 band I */
     0.0369 ,0.0369 ,0.0369 ,0.0369 ,0.0369 ,0.0369 ,0.0369 ,0.0369 ,
		/* detector 55-62 band IV */
     0.1478 ,0.1478 ,0.1478 ,0.1478 ,0.1478 ,0.1478 ,0.1478 ,0.1478 ,
		/* detector 71-73 band V; 74-75 band VI; */
     0 ,0 ,0 ,0 ,0 ,0 ,0 ,0 , 0.0265 , 0.0265 , 0.0265 , 0.0265 , 0.0265 , } ;


/* 
      The tables yloc, zloc contain respectively the Y and Z
      positions of the detector centroids w.r.t. the boresight (arcmin). 
      The numbers for the survey detectors  are taken from the
      Faint Source Survey expl. supp. table Appendix II.2
*/

static float yloc[ MAXDETNO ] = {
         28.00,  28.01,  28.01,  23.97,  23.97,  23.98,  23.98,
         19.66,  19.67,  19.67,  19.67,  17.14,  17.15,  17.15,  17.15,
         14.03,  14.03,  14.03,  12.26,  12.26,  12.27,  12.27,
          9.47,   9.48,   9.49,   9.49,   7.71,   7.71,   7.72,   7.72,
          4.54,   4.53,   4.53,   4.53,   2.02,   2.01,   2.01,   2.01,
         -1.17,  -1.17,  -1.17,  -1.17,  -2.93,  -2.93,  -2.93,  -2.93,
         -5.68,  -5.68,  -5.67,  -5.67,  -7.44,  -7.44,  -7.43,  -7.43,
        -11.33, -11.33, -11.33, -11.32, -15.36, -15.36, -15.36, -15.36,
         99999,  99999,  99999,  99999,  99999,  99999,  99999,  99999,
        -26.35, -26.40, -26.40, -26.50, -26.45  } ;

static float zloc[ MAXDETNO ] = {
          8.71,   0.04,  -8.62,  12.86,   4.37,  -4.29, -12.77,
          9.80,   1.14,  -7.53, -14.46,  13.49,   5.47,  -3.20, -11.86,
          8.71,   0.04,  -8.62,  12.96,   4.37,  -4.29, -12.88,
          9.81,   1.14,  -7.52, -14.50,  13.55,   5.47,  -3.19, -11.86,
         14.55,   7.61,  -1.06,  -9.73,  11.94,   3.27,  -5.40, -13.41,
         14.05,   6.55,  -2.12, -10.78,  10.88,   2.22,  -6.45, -13.95,
         14.64,   7.65,  -1.02,  -9.68,  11.98,   3.32,  -5.35, -13.41,
         13.95,   6.55,  -2.12, -10.79,  10.88,   2.21,  -6.46, -13.85,
         99999,  99999,  99999,  99999,  99999,  99999,  99999,  99999,
          8.15,   3.00,  -1.85,   7.35,  -0.15 } ;

/* 
      The tables ysize, zsize contain respectively the Y and Z
      sizes of the detectors (arcmin).
      The numbers for the survey detectors  are taken from the
      Faint Source Survey expl. supp. table Appendix II.2
*/
static float ysize[ MAXDETNO ] = {
          3.03,   3.03,   3.03,   3.03,   3.03,   3.03,   3.03,
          1.51,   1.51,   1.51,   1.51,   1.51,   1.51,   1.51,   1.51,
          0.76,   0.76,   0.76,   0.76,   0.76,   0.76,   0.76,
          0.76,   0.76,   0.76,   0.76,   0.76,   0.76,   0.76,   0.76,
          1.51,   1.51,   1.51,   1.51,   1.51,   1.51,   1.51,   1.51,
          0.76,   0.76,   0.76,   0.76,   0.76,   0.76,   0.76,   0.76,
          0.76,   0.76,   0.76,   0.76,   0.76,   0.76,   0.76,   0.76,
          3.03,   3.03,   3.03,   3.03,   3.03,   3.03,   3.03,   3.03,
          0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,
          6.0 ,   6.0 ,   6.0 ,   6.0 ,   6.0  } ;

static float zsize[ MAXDETNO ] = {  
          5.05,   5.05,   5.05,   4.68,   5.05,   5.05,   4.70,
          4.75,   4.76,   4.76,   1.30,   3.45,   4.76,   4.75,   4.75,
          4.65,   4.65,   4.65,   4.48,   4.65,   4.65,   4.48,
          4.55,   4.55,   4.45,   1.20,   3.33,   4.54,   4.54,   4.54,
          1.28,   4.75,   4.75,   4.75,   4.75,   4.76,   4.75,   3.47,
          2.33,   4.65,   4.65,   4.65,   4.65,   4.65,   4.65,   2.33,
          1.18,   4.55,   4.55,   4.55,   4.55,   4.55,   4.55,   3.36,
          2.52,   5.05,   5.05,   5.05,   5.06,   5.05,   5.05,   2.53,
          0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,
          5.3 ,   5.0 ,   4.7 ,   7.5 ,   7.5  } ;

static float solid[ MAXDETNO + 1 ] = { 0.0,
/* B4 */ 14.1,   13.7,   12.9,   13.1,   13.2,   13.2,   13.6,
/* B3 */  6.10,   6.04,   6.07,   1.92,   4.85,   6.10,   6.19,   6.56,
/* B2 */  3.39,   0.00,   3.46,   3.40,   0.00,   3.48,   3.38,
/* B1 */  3.21,   3.22,   3.17,   0.92,   2.41,   2.95,   3.25,   3.25,
/* B3 */  1.97,   6.40,   6.32,   6.25,   6.36,   0.00,   6.37,   4.84,
/* B2 */  1.82,   3.50,   3.44,   3.48,   3.50,   3.45,   3.46,   1.78,
/* B1 */  0.89,   3.31,   3.22,   3.29,   3.25,   3.19,   3.24,   2.45,
/* B4 */  7.9,   13.3,   14.0,   12.9,   14.0,   13.8,   14.2,    6.8 ,
          0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0 ,
/*LRS */  0.0 ,   0.0 ,   0.0 ,   0.0 ,   0.0  } ;



/* 
      The tables ylband, zlband contain respectively the Y and Z
      positions of the band centroids w.r.t. the boresight (arcmin). 
*/


static float ylband[ MAXBAND + 1 ] = {
          0.0 ,   1.02,   5.55,  10.84,   6.32, -26.3 ,   0.0  } ;



static float zlband[ MAXBAND + 1 ] = {
          0.0 ,   0.07,   0.04,   0.05,   0.04,   3.6 ,   0.0  } ;


/* 
      The tables ysband, zsband contain respectively the Y and Z
      total sizes of the focal plain for a given band (arcmin).
*/

static float ysband[ MAXDETNO ] = {
         59.1 ,  17.68,  16.2 ,  19.2 ,  46.40,   6.0 ,   0.0  } ;


static float zsband[ MAXBAND + 1 ] = {
         30.4 ,  30.32,  30.32,  30.32,  30.32,  15.0 ,   0.0  } ;




/*
#>            ircc_bandnr.dc2

Function:     ircc_bandnr

Purpose:      Returns IRAS band numbers

Category:     IR

File:         ircc_const.c

Author:       P.R. Roelfsema

Use:          INTEGER IRCC_BANDNR( INSTRUMENT )  Input character*(*)

              IRCC_BANDNR Returns the band number for a given instrument; 
                          If the instrument does not have a band associated 
                          with it 0 is returned.
              INSTRUMENT  Instrument name; this can be a combination 
                          of a mode and band name e.g.:
                          SURVEY B1, SURVEY B2, SPLINE B3, AO B4, 
                          AO LRS, SURVEY BPHF etc.
                          Note B1, B2, LRS etc alone will also give correct
                          bandnumbers.
                          ( see also IRCC_INSTRNAME )
Description:
	The band numbers are:
	  1	 12 micron
	  2	 25 micron
	  3	 60 micron
	  4	100 micron
	  5	LRS
	  6	BPHF

Updates:      Sep  6, 1990: PRR, Creation date

#<

Fortran to C interface:

@ integer function ircc_bandnr( character )

*/

fint ircc_bandnr_c( fchar instrument )
{
   fint  n  = 0 ;

   for ( n = 1 ; n <= MAXBAND ; n++ ) 
      if ( strzstr( instrument.a, nelc_c(instrument), bands[ n ] ) != 0 ) {
	return( n ) ;
   }
   return( 0 ) ;
}



/*
#>            ircc_obsmode.dc2

Function:     ircc_obsmode

Purpose:      Returns IRAS observation mode

Category:     IR

File:         ircc_const.c

Author:       P.R. Roelfsema

Use:          CHARACTER IRCC_OBSMODE( INSTRUMENT )  Input character*(*)

              IRCC_OBSMODE  Returns the observing mode number for a given 
                            instrument; 
                            If the instrument does not exist 0 is returned.
              INSTRUMENT    Instrument name; this can be a combination 
                            of a mode and band name e.g.:
                            SURVEY B1, SURVEY B2, SPLINE B3, AO B4, 
                            AO LRS etc.
                            Note SURVEY, AO etc alone will also give correct
                            observing mode numbers.
                            ( see also IRCC_INSTRNAME )

Description:
	The observation modes are:
	  1	Survey
	  2	Splines
	  3	AO
	  4	Flashes
	  5	Unknown

Updates:      Sep  6, 1990: PRR, Creation date

#<

Fortran to C interface:

@ integer function ircc_obsmode( character )

*/

fint ircc_obsmode_c( fchar instrument )
{
   fint  n  = 0 ;

   for ( n  = 1 ; n <= MAXMODES ; n++ )
      if ( strzstr( instrument.a, nelc_c(instrument), obsmodes[ n ] ) ) 
		return( n ) ;
   return( 0 ) ;
}


/*
#>            ircc_instrname.dc2

Function:     ircc_instrname

Purpose:      Returns IRAS instrument names.

Category:     IR

File:         ircc_const.c

Author:       P.R. Roelfsema

Use:          CHARACTER IRCC_INSTRNAME( OBSMODE ,     I   integer
                                        band      )   I   integer

              IRCC_INSTRNAME   
                          Returns the name of the instrument corresponding
                          to the observing mode and band numbers.
              OBSMODE     Observing mode number, can be:
                            1   - SURVEY
                            2   - SPLINE
                            3   - AO
                            4   - FLASH
			    5   - UNKNOWN
                          other - no observing mode
              BAND        Band number, can be:
                          1 - 4 - bands 1 - 4
                            5   - LRS
                            6   - BPHF
                          other - no band 

Description:
	The returned instrument names are a combination of observation name
	followed by band name, separated by one space. e.g.
		SURVEY B1
		SURVEY LRS
		AO BPHF
		AO B4     etc.

Updates:      Aug  8, 1990: PRR, Creation date
              Sep 10, 1990: PRR, Changed device to observing mode.
              Aug 19, 1991: PRR, Chang to make OBSMODE or BAND is 'other'
                                 work properly.

#<

Fortran to C interface:

@ character function ircc_instrname( integer , integer )

*/

void ircc_instrname_c( fchar instrument , fint *obsmod , fint *band )
{
   int n ;

   instrument.a[ 0 ] = 0 ;
   if ( *obsmod > 0 && *obsmod <= MAXMODES 
     && strlen( obsmodes[ *obsmod ] ) < instrument.l ){
      instrument.a = strcpy( instrument.a , obsmodes[ *obsmod ] ) ;
   }
   if ( *band > 0 && *band <= MAXBAND 
     && ( strlen( instrument.a ) + strlen( bands[ *band ] ) ) < instrument.l ) {
      if ( strlen( instrument.a ) > 0  
        && ( strlen( instrument.a ) + 1 ) < instrument.l ){
         instrument.a = strcat( instrument.a , " " ) ;
      }
      instrument.a = strcat( instrument.a , bands[ *band ] ) ;
   }

   for( n = strlen( instrument.a ) ; n < instrument.l ; n++ ) {
      instrument.a[ n ] = ' ' ;
   }
}


/*
#>            ircc_rate.dc2

Function:     ircc_rate

Purpose:      Returns IRAS scanning rates.

Category:     IR

File:         ircc_const.c

Author:       P.R. Roelfsema

Use:          INTEGER IRCC_RATE( instrument )  I  character*(*)

              IRCC_RATE   > 0 - number of samples taken per second.
                           -1 - unknown instrument
              INSTRUMENT  Instrument name; this can be a combination 
                          of a mode and band name e.g.:
                          SURVEY B1, SURVEY B2, SPLINE B3, AO B4, 
                          SURVEY LRS etc.
                          ( see also IRCC_INSTRNAME )

Updates:      Aug 8, 1990: PRR, Creation date

#<

Fortran to C interface:

@ integer function ircc_rate( character )

*/


fint ircc_rate_c( fchar instrument )
{
   fint  r = -1 ;

   r = rates[ ircc_bandnr_c( instrument ) ]
              [ ircc_obsmode_c( instrument ) ] ;
   return( r ) ;
}


/*
#>            ircc_ndets.dc2

Function:     ircc_ndets

Purpose:      Returns IRAS the number of detectors.

Category:     IR

File:         ircc_const.c

Author:       P.R. Roelfsema

Use:          INTEGER IRCC_NDETS( instrument )  I  character*(*)

              IRCC_NDETS  > 0 - number of detectors for instrument.
                           -1 - unknown instrument
              INSTRUMENT  Instrument name; this can be a combination 
                          of a mode and band name e.g.:
                          SURVEY B1, SURVEY B2, SPLINE B3, AO B4, 
                          SURVEY LRS etc.
                          ( see also IRCC_INSTRNAME )

Updates:      Aug 8, 1990: PRR, Creation date

#<

Fortran to C interface:

@ integer function ircc_ndets( character )

*/


fint ircc_ndets_c( fchar instrument )
{
   fint  r = -1 ;

   r = ndets[ ircc_bandnr_c( instrument ) ] ;
   return( r ) ;
}


/*
#>            ircc_detnr.dc2

Function:     ircc_detnr

Purpose:      Returns IRAS detector numbers.

Category:     IR

File:         ircc_const.c

Author:       P.R. Roelfsema

Use:          INTEGER IRCC_DETNR(  det        ,  I  integer
                                   instrument )  I  character*(*)

              IRCC_DETNR  > 0 - detector number of detnr for instrument.
                           -1 - unknown instrument
              DET         Sequential detector number; 1 - IRCC_NDETS .
              INSTRUMENT  Instrument name; this can be a combination 
                          of a mode and band name e.g.:
                          SURVEY B1, SURVEY B2, SPLINEB 3, AO B4, 
                          SURVEY LRS etc.
                          ( see also IRCC_INSTRNAME )

Updates:      Aug  8, 1990: PRR, Creation date
              Nov 26, 1990: PRR, Rearranged LRS detector numbers.

#<

Fortran to C interface:

@ integer function ircc_detnr( integer , character )

*/


fint ircc_detnr_c( fint *det , fchar instrument )
{
   fint  r   = -1 ;
   fint  mod, band ;

   mod = ircc_obsmode_c( instrument ) ;
   if ( mod != 0 && 
        *det > 0 && *det <= ircc_ndets_c( instrument) ){
      band = ircc_bandnr_c( instrument ) ;
      r = detnrs[ band ][ *det - 1 ] ;
   }
   return( r ) ;
}



/*

#>            ircc_times.dc2

Function:     ircc_times

Purpose:      Give exact time of sampling of individual IRAS detectors

Category:     IR

File:         ircc_times.c

Author:       P.R. Roelfsema

Use:          REAL IRCC_TIMES( DETNO )     Input   integer

              IRCC_TIMES  Returns the time offset w.r.t. satcal ticks
                          at which data for a given detector were taken.
                          For non existing detector numbers zero is returned.
              DETNO       Detector number as given in IRAS Epl. Suppl.

Description:

      For example, take detector 9 (band III), at satcal 123456.
      CCTIMS(9) will return -0.04573
      meaning that the first sample of detector 9 in this 'second'
      is not taken at satcal 123456.0000 but at satcal 123455.95427;
      the next samples being at the proper equidistant
      interval of 1/8 tick as appropriate for band III.

      See IRAS SAT/Ground Interface Control Document section 3.4.3

      It is not clear what the LRS timing is derived from.

      The values fro the electronic delay times were obtained by com-
      paring the times given in the IRAS SAT/GICD sec. 3.4.3 with
      times as measured by the FSS group (FSS expl. supp., table
      Appendix II.2).
      The LRS delay times were calculated from the theoretical
      behaviour of the 12Hz low-pass filters. 


Updates:      Aug 18, 1990: PRR, Adapted from GEISHA
              Nov 26, 1990: PRR, Added LRS delay times

#<

Fortran to C interface:

@ real function ircc_times( integer )

*/

float ircc_times_c( fint *detno )
{
   float r = 0 ;
   fint  det ;

   det = *detno - 1 ;
   if ( det >= 0 && det < MAXDETNO ) 
      r = times[ det ] - edelay[ det ] ;
   return( r ) ;
}

/*

#>            ircc_mask.dc2

Function:     ircc_mask

Purpose:      Give IRAS detector layout parameters

Category:     IR

File:         ircc_mask.c

Author:       P.R. Roelfsema

Use:          INTEGER IRCC_MASK( DETNO ,     Input   integer
                                 YLOC  ,     Output  real
                                 ZLOC  ,     Output  real
                                 YSIZE ,     Output  real
                                 ZSIZE )     Output  real

              IRCC_MASK   Returns error code:
                           0 - no problem.
                          -1 - not a legal IRAS detector number.
              DETNO       Detector number as given in IRAS Epl. Suppl.
                          DETNO=0 returns the location of the boresight 
                          (0,0) and (roughly) the total size of the IRAS 
                          focal plane.
                          DETNO=-IRCC_BANDNR( INSTRUMENT ) returns the 
                          centroid position and total extent for all detectors
                          in the band defined by INSTRUMENT. (INSTRUMENT=BPHF
                          i.e. boresight returns 0 for all).
              YLOC        Y position of DETNO centroid w.r.t. boresight.
              ZLOC        Z position of DETNO centroid w.r.t. boresight.
              YSIZE       Y size DETNO.
              ZSIZE       Z size DETNO.

Description:

      YLOC and ZLOC are the positions in arcmin of the centers of the 
      detectors relative to the boresight. the detectors in the focal 
      plane have an extent (YLOC-0.5*YSIZE,YLOC+0.5*YSIZE), and 
      (ZLOC-0.5*ZSIZE,ZLOC+0.5*ZSIZE).
      In survey mode images move in the +Y direction over the focal
      plane or, the satellite moves in the -PSI direction on the sky.

      To first order, the nominal detector centre is at:
        THETA(detector) = THETA(boresight) - Z
        PSI  (detector) = PSI  (boresight) + Y/sin(THETA(detector))
      (disregarding proper angular units).

      For higher accuracy (10 arcsec), use ccpsth and IRCC_TIMES.

      The focal plane layout parameters of the survey detectors are
      taken from the Faint Source Survey expl. supp. table
      Appendix II.2


Updates:      Aug 22, 1990: PRR, Adapted from GEISHA
              Nov 26, 1990: PRR, Updated LRS parameters.
              Jan 21, 1991: PRR, Updated LRS parameters.

#<

Fortran to C interface:

@ real function ircc_mask( integer , real , real , real , real )

*/

float ircc_mask_c( fint *detno , float *yl , float *zl , 
                                 float *ys , float *zs )
{
   float r = 0 ;
   fint  det ;

   det = *detno - 1 ;
   if ( det >= 0 && det < MAXDETNO )  {
      *yl = yloc[ det ]  ;
      *zl = zloc[ det ]  ;
      *ys = ysize[ det ] ;
      *zs = zsize[ det ] ;
   } else if ( -*detno <= MAXBAND && det < MAXDETNO ) {
      *yl = ylband[ -*detno ] ;
      *zl = zlband[ -*detno ] ;
      *ys = ysband[ -*detno ] ;
      *zs = zsband[ -*detno ] ;
   } else {
      r = BADDETNO ;
   }
   return( r ) ;
}


/*
#>            ircc_alive.dc2

Function:     ircc_alive

Purpose:      Tells whether a detector is alive or dead.

Category:     IR

File:         ircc_const.c

Author:       P.R. Roelfsema

Use:          INTEGER IRCC_ALIVE(  DETECTOR )  I  integer

              IRCC_ALIVE  1 - detector is alive.
                          0 - detector is dead
                         -1 - non existent detector.
              DETECTOR   Detector number as given in IRAS Epl. Suppl.

Updates:      Sep 27, 1990: PRR, Creation date

#<

Fortran to C interface:

@ integer function ircc_alive( integer )

*/


fint ircc_alive_c( fint *detno )
{
   fint  r   = -1 ;

   if ( ( *detno > 0 ) && ( *detno <= MAXDETNO ) ){
      r = alive[ *detno ] ;
   }
   return( r ) ;
}


/*
#>            ircc_bandet.dc2

Function:     ircc_bandet

Purpose:      band number given a detector number

Category:     IR

File:         ircc_const.c

Author:       Do Kester

Use:          INTEGER IRCC_BANDET(  DETECTOR )  I  integer

              IRCC_BANDET band number as in IRCC_BANDNR
              DETECTOR    Detector number as given in IRAS Epl. Suppl.

Updates:      Oct 31, 1990: DK, Creation

#<

Fortran to C interface:

@ integer function ircc_bandet( integer )

*/


fint ircc_bandet_c( fint *detno )
{
   fint  r   = -1 ;

   if ( ( *detno > 0 ) && ( *detno <= MAXDETNO ) ){
      r = bandet[ *detno ] ;
   }
   return( r ) ;
}


/*
#>              ircc_solid.dc2

Function:	ircc_solid

Purpose:	returns effective solid angle of a detector

Category:	IR

File:		ircc_const.c

Author:		Do Kester

Use:	real function ircc_solid(
	detector )	I	integer

	ircc_solid	effective solid angle of the detector
	detector	detector number

Description:
       It returns the effective solid angle of a given detector
       as listed in the Explanatory Supplement to the IRAS Faint
       Source Survey, Table number II.D.1 page II-50.
       The solid angles are in steradians.
       The dead detectors have solid angle == 0.0
 
       No provisions (yet) for LRS detectors. 
       They are also == 0.0.

Updates:	1 Nov 1990 Do Kester, Creation

#<

@ real function ircc_solid( integer )

*/

float ircc_solid_c( fint *detno )
{
   float  r   = 0.0 ;

   if ( ( *detno > 0 ) && ( *detno <= MAXDETNO ) ){
      r = solid[ *detno ] * 1.0e-7 ;
   }
   return( r ) ;
}


/*
#>            ircc_sdet.dc2

Function:     ircc_sdet

Purpose:      Returns SDET number in IRDS for IRAS-ES detector number.

Category:     IR

File:         ircc_const.c

Author:       P.R. Roelfsema

Use:          INTEGER IRCC_SDET(  DETNR      ,  I  integer
                                  INSTRUMENT )  I  character*(*)

              IRCC_SDET   > 0 - SDET of detetector DETNR for instrument.
                           -1 - DETNR not valid for INSTRUMENT
              DETNR       IRAS-ES detector number; 1 - 75
              INSTRUMENT  Instrument name; this can be a combination 
                          of a mode and band name e.g.:
                          SURVEY B1, SURVEY B2, SPLINEB 3, AO B4, 
                          SURVEY LRS etc.
                          ( see also IRCC_INSTRNAME )

Updates:      Aug 12, 1993: PRR, Creation date

#<

Fortran to C interface:

@ integer function ircc_sdet( integer , character )

*/


fint ircc_sdet_c( fint *det , fchar instrument )
{
   fint  sdet   = -1 ;
   fint  band , n ;

   band = ircc_bandnr_c( instrument ) ;

   if ( *det > 0 && *det <= MAXDETNO ) {
      for ( n = 0 ; n < MAXDETS ; n++ ) {
          if ( *det == detnrs[ band ][ n ] ) {
             sdet = n + 1 ;
             break ;
          } 
      }
   }
   return( sdet ) ;
}

static char *strzstr( char *cs, fint n, char *ct ) 
{
    int		i, k ;

    for ( i = 0 ; i < n && cs[i] != '\0' ; i++ ) {
	for ( k = 0 ; k < n - i && cs[k+i] == ct[k] && ct[k] != '\0' ; k++ ) ;
	if ( ct[k] == '\0' ) return &(cs[i]) ;
    }
    return 0 ;
}

