/* stabar.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            stabar.dc2

Function:     STABAR

Purpose:      Displays a progress bar in the status area.

File:         stabar.c

Author:       K.G. Begeman

Use:          CALL STABAR( MIN ,    Input    REAL
                           MAX ,    Input    REAL
                           CUR )    Input    REAL

              MIN      Lower range (i.e. start value).
              MAX      Upper range (i.e. end value).
              CUR      Current value .

Example:      loop through a number (NMAP) maps

              C     no maps done
                    CALL STABAR( 0.0, float( nmap ), 0.0 )
                    FOR N = 1, NMAPS
                             .
                             .
                             .
              C        progress report (N maps done)
                       CALL STABAR( 0.0, float( nmap ), float( n ) )
                    CFOR

Updates:      Apr  1, 1987: KGB, Creation date.
              Feb 21, 1990: KGB, Migrated to GPS.

#<

Fortran to C interface:

@ subroutine stabar( real, real, real )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"string.h"		/* <string.h> */
#include 	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"status.h"		/* declare status_c */

#define	MAXLEN	57			/* maximum length of character string */

static	fint	n_dot = -1;		/* save this value */

void stabar_c( float *min, float *max, float *cur )
{
   char line[MAXLEN+1];
   fint i;
   fint p;
   
   p = ((float) ( MAXLEN - 11 )) * ( *cur - *min ) / ( *max - *min ) + 0.5;
   if (p > (MAXLEN - 11)) p = MAXLEN - 11;
   if (p != n_dot) {					/* change since last call */
      n_dot = p;					/* save p */
      strcpy( line, "Working [" );			/* text */
      for (i = 0; i < p; i++) line[i+9] = '-';		/* dashes */
      i += 9;						/* increment */
      line[i++] = '>';					/* arrow */
      while (i < (MAXLEN - 1)) line[i++] = ' ';		/* blanks */
      line[i++] = ']';					/* end mark */
      line[i] = 0;					/* closing zero */
      status_c( tofchar( line ) );			/* status line */
   }
}

#if defined(TESTBED)

#include "init.h"
#include "finis.h"
#include "cmain.h"

MAIN_PROGRAM_ENTRY
{
   float min =    0.0;
   float max = 1000.0;
   float cur;
   fint  n;
   
   init_c( );
   for (n = 0; n <= 2000; n++) {
      cur = (float) n;
      stabar_c( &min, &max, &cur );
   }
   finis_c( );
}
#endif
