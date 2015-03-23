/* factor.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            factor.dc2

Function:     factor

Purpose:      Factor returns the conversion factor between two different
              units (i.e. M en KM)

Category:     PHYSICAL COORDINATES

File:         factor.c

Author:       K.G. Begeman

Use:          INTEGER FACTOR( CUNIT1 ,     Input    CHARACTER*(*)
                              CUNIT2 ,     Input    CHARACTER*(*)
                              CFACT  )     Output   DOUBLE PRECISION

              CUNIT1     Type of unit to convert from.
                         The units can be abbreviated, i.e enough
                         characters must be given to find a unique
                         match in a list of known units.
              CUNIT2     Type of unit to convert to.
              CFACT      Factor to convert from CUNIT1 to CUNIT2.
              FACTOR     Returns:
                          0: successful conversion
                         51: unknown units to convert from
                         52: unknown units to convert to
                         53: both units are unknown
                         54: incompatible units
                         55: ambiguous units to convert from
                         56: ambiguous units to convert to


Notes:        The following type of units are implemented:
              DEGREE      ARCSEC      ARCMIN      RADIAN
              CIRCLE      DMSSEC      DMSMIN      DMSDEG
              HMSSEC      HMSMIN      HMSHOUR     
              METER       ANGSTROM    NM          MICRON      
              MM          CM          INCH        FOOT        
              YARD        M           KM          MILE        
              PC          KPC         MPC
              AU          LYR
              TICK        SECOND      MINUTE      HOUR
              DAY         YEAR        YR      
              HZ          KHZ         MHZ         GHZ         
              M/S         MM/S        CM/S        KM/S        
              K           MK
              W/M2/HZ     JY          MJY         
              TAU
              JOULE       J           EV          ERG         RY                       

Updates:      Dec 23, 1989: KGB, document created.
              May  9, 1995: VOG, minimal match.
                                 DMSSEC/MIN/DEG,
                                 HMSSEC/MIN/HOUR
              Mar 25, 2009: JPT, improved minimal match
              Mar 29, 2009: VOG, units for energy added
                                 (WCSLIB) and updated value for pc.
                                 Also added au and lyr
#<

F2CVV:

@integer function factor( character, character, double precision )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"ctype.h"		/* <ctype.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"nelc.h"		/* defines nelc_c */
#include        "wmatch.h"              /* match 2 strings (wildcards allowed) */

typedef	struct { char *unit; char *match; fint type; double fact; } f_struct;

static f_struct TABLE[] = {
   { "DEGREE",     "DE*",      1,                        1.0000000000000000000 },
   { "ARCSEC",     "ARCS*",    1,                        0.0002777777777777778 },
   { "ARCMIN",     "ARCM*",    1,                        0.0166666666666666667 },
   { "RADIAN",     "RAD*",     1,                       57.2957795130823208767 },
   { "CIRCLE",     "CI*",      1,                      360.0000000000000000000 },
   { "DMSSEC",     "DMSS*",    1,                        0.0002777777777777778 },
   { "DMSMIN",     "DMSM*",    1,                        0.0166666666666666667 },
   { "DMSDEG",     "DMSD*",    1,                        1.0000000000000000000 },
   { "HMSSEC",     "HMSS*",    1,                   15.0*0.0002777777777777778 },
   { "HMSMIN",     "HMSM*",    1,                   15.0*0.0166666666666666667 },
   { "HMSHOUR",    "HMSH*",    1,                       15.0000000000000000000 },
   { "METER",      "ME*",      2,                        1.0000000000000000000 },
   { "ANGSTROM",   "AN*",      2,                        0.0000000001000000000 },
   { "NM",         "NM",       2,                        0.0000000010000000000 },
   { "MICRON",     "MIC*",     2,                        0.0000010000000000000 },
   { "MM",         "MM",       2,                        0.0010000000000000000 },
   { "CM",         "CM",       2,                        0.0100000000000000000 },
   { "INCH",       "IN*",      2,                        0.0254000000000000000 },
   { "FOOT",       "F*",       2,                        0.3048000000000000000 },
   { "YARD",       "YA*",      2,                        0.9144000000000000000 },
   { "M",          "M",        2,                        1.0000000000000000000 },
   { "KM",         "KM",       2,                     1000.0000000000000000000 },
   { "MILE",       "MIL*",     2,                     1609.3440000000000000000 },
   { "PC",         "PC",       2,        30856802500000000.0000000000000000000 },
   { "KPC",        "KPC",      2,     30856802500000000000.0000000000000000000 },
   { "MPC",        "MPC",      2,  30856802500000000000000.0000000000000000000 },
   { "AU",         "AU",       2,                        1.4959787e11          },
   { "LYR",        "LY*",      2,                        9.4607304725808e15    },
   { "TICK",       "TI*",      3,                        1.0000500000000000000 },
   { "SECOND",     "SEC*",     3,                        1.0000000000000000000 },
   { "MINUTE",     "MIN*",     3,                       60.0000000000000000000 },
   { "HOUR",       "HO*",      3,                     3600.0000000000000000000 },
   { "DAY",        "DA*",      3,                    86400.0000000000000000000 },
   { "YEAR",       "YE*",      3,                 31557600.0000000000000000000 },
   { "YR",         "YR",       3,                 31557600.0000000000000000000 },   
   { "HZ",         "HZ*",      4,                        1.0000000000000000000 },
   { "KHZ",        "KH*",      4,                     1000.0000000000000000000 },
   { "MHZ",        "MH*",      4,                  1000000.0000000000000000000 },
   { "GHZ",        "GH*",      4,               1000000000.0000000000000000000 },
   { "M/S",        "M/S*",     5,                        1.0000000000000000000 },
   { "MM/S",       "MM/S*",    5,                        0.0010000000000000000 },
   { "CM/S",       "CM/S*",    5,                        0.0100000000000000000 },
   { "KM/S",       "KM/S*",    5,                     1000.0000000000000000000 },
   { "K",          "K",        6,                        1.0000000000000000000 },
   { "MK",         "MK*",      6,                        0.0010000000000000000 },
   { "W/M2/HZ",    "W/M2/HZ",  7,                        1.0                   },   
   { "JY",         "JY",       7,                        1e-26                 },
   { "MJY",        "MJ*",      7,                        1e-29                 },
   { "TAU",        "TA*",      9,                        1.0000000000000000000 },
   { "J",          "J",       10,                        1.0                   },
   { "JOULE",      "JOU*",    10,                        1.0                   },   
   { "EV",         "EV",      10,                        1.60217733e-19        },
   { "ERG",        "ERG",     10,                        1.0e-7                },
   { "RY",         "RY",      10,                        2.179872e-18          }
};

#define MAXTABLEN (sizeof(TABLE)/sizeof(f_struct))
#define MAXBUFLEN 80

fint factor_c( fchar cunit1, fchar cunit2, double *cfact )
{
   fint   r = 0;
   fint   n;
   fint   u1;
   fint   u2;
   fint   found1 = 0;
   fint   found2 = 0;
   fint   casesensitive = 0;
   char   cunit1w[MAXBUFLEN], cunit2w[MAXBUFLEN];       /* asterisk appended */
   char   cunit1b[MAXBUFLEN], cunit2b[MAXBUFLEN];       /* "bare" string    */

   strncpy(cunit1w, cunit1.a, cunit1.l);
   cunit1w[cunit1.l] = '\0';
   for (n=0; ; n++) {
      cunit1w[n] = toupper(cunit1w[n]);
      if (!cunit1w[n] || cunit1w[n]==' ') {
         cunit1w[n] = '\0';
         strcpy(cunit1b, cunit1w);
         cunit1w[n]   = '*';
         cunit1w[n+1] = '\0';
         break;
      }
   }
   cunit1 = tofchar(cunit1w);

   strncpy(cunit2w, cunit2.a, cunit2.l);
   cunit2w[cunit2.l] = '\0';
   for (n=0; ; n++) {
      cunit2w[n] = toupper(cunit2w[n]);
      if (!cunit2w[n] || cunit2w[n]==' ') {
         cunit2w[n] = '\0';
         strcpy(cunit2b, cunit2w);
         cunit2w[n]   = '*';
         cunit2w[n+1] = '\0';
         break;
      }
   }
   cunit2 = tofchar(cunit2w);

   *cfact = 0.0;                                                     /* reset */
   u1 = u2 = -1;
   for (n = 0; n < MAXTABLEN; n++) {
      if (!strcmp(cunit1b, TABLE[n].unit)) {
         found1 = 1;
         u1 = n;
         break;
      }
      if (wmatch_c(tofchar(TABLE[n].unit), cunit1, tofchar("*"), &casesensitive )) {
         found1++;
         u1 = n;
      }
   }
   for (n = 0; n < MAXTABLEN; n++) {
      if (!strcmp(cunit2b, TABLE[n].unit)) {
         found2 = 1;
         u2 = n;
         break;
      }
      if (wmatch_c(tofchar(TABLE[n].unit), cunit2, tofchar("*"), &casesensitive )) {
         found2++;
         u2 = n;
      }
   }

   if (!found1 && !found2) return( 53 );
   if (!found1) return( 51 );
   if (!found2) return( 52 );
   if (found1 > 1) return( 55 );
   if (found2 > 1) return( 56 );
   if (TABLE[u1].type == TABLE[u2].type) {
      *cfact = (TABLE[u1].fact / TABLE[u2].fact);
   } else {
      r = 54;
   }
   return( r );
}


/*
#>            units.dc2

Function:     units

Purpose:      Units returns the type of units.

Category:     PHYSICAL COORDINATES

File:         factor.c

Author:       P.R. Roelfsema

Use:          INTEGER UNITS( CUNIT )     Input    CHARACTER*(*)

              UNITS      Returns:
                          0: unknown units.
                          1: angle units (degrees, seconds etc.)
                          2: length units (kilometers, Angstrom etc.)
                          3: time units (seconds, hours days etc.
                          4: frequency units (Hz, MHz etc.)
                          5: speed units (m/s, km/s etc. )
                          6: teperature units (kelvin, millikelvin )
                          7: fluxdensity units (Jy, mJy)
                          9: optical depth (tau)
                         10: Energy (J, eV, erg, Ry)
              CUNIT      Name of unit.

Notes:        The type of units which are implemented are listed in
              factor.dc2.

Updates:      Aug 28, 1991: PRR, document created.

#<

F2CVV:

@integer function units( character )

*/
fint units_c( fchar cunit )
{
   char		unit[ MAXBUFLEN + 1 ] ;		/* internal buffer	*/

   fint		r = 0 ;				/* return result	*/
   fint		n ;				/* counter		*/
   fint		l = nelc_c( cunit ) ;		/* length of input	*/

   n = 0;
   while ((n < MAXBUFLEN) && (n < l )) {	/* set input uppercase	*/
      unit[n] = toupper( cunit.a[n] );
      n++;
   }
   unit[n]  = 0 ;

   for (n = 0; n < MAXTABLEN; n++) {		/* loop to find units	*/
      if ( !strcmp( unit, TABLE[n].unit ) ) {	/* is this it ?		*/
         r = TABLE[n].type ;			/* yes => set result	*/
         break ;				/* exit search loop	*/
      }
   }						/* end search loop	*/

   return( r ) ;
}

#if defined(TESTBED)

#include	"cmain.h"

int cmain( int argc, char *argv[] )
{
   fchar cunit1;
   fchar cunit2;
   char  a1[MAXBUFLEN+1];
   char  a2[MAXBUFLEN+1];

   if (argc > 2) {
      double f;
      fint   r;
      strncpy( a1, argv[1], MAXBUFLEN ); a1[MAXBUFLEN] = 0;
      cunit1.a = a1; cunit1.l = strlen( cunit1.a );
      strncpy( a2, argv[2], MAXBUFLEN ); a2[MAXBUFLEN] = 0;
      cunit2.a = a2; cunit2.l = strlen( cunit2.a );
      r = factor_c( cunit1, cunit2, &f );
      printf( "factor(%2ld), %g\n", r, f );
   }
   return( 0 );
}
#endif
