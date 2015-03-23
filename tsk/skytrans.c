/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             skytrans.dc1

Program:       SKYTRANS

Purpose:       Transformation between equatorial, galactic, ecliptic and
               supergalactic coordinates.

Category:      COORDINATES, UTILITY

File:          skytrans.c

Author:        M.G.R. Vogelaar

Keywords:


   FILENAME=   Write output to file with name:   [skip writing]
               Write output to a file on disk. Output coordinates
               are written in degrees in two column format.
               You are not allowed to write to a file that already
               exists.               

   SKYSYSIN=   Give sky system of input *, G, E or S:  [*(=Equatorial)]
               * == equatorial system.
               G == Galactic system.
               E == Ecliptic system.
               S == Supergalactic system.

   EPOCHIN=    Give epoch of input:                            [1950.0]
               Precession correction for the year EPOCHIN= is applied
               for the Equatorial and Ecliptic system.

   SKYSYSOUT=  Give sky system of output *, G, E or S:       [Galactic]

   EPOCHOUT=   Give epoch of output:                           [1950.0]
               See remark at EPOCHIN=

   COORD=      Position RA,DEC as d d  or hms dms:               [stop]
               or:
               Position l,b as d d or dms dms:                   [stop]
              
               Positions can be given as coordinate pairs in
               degrees or in hms/dms notation (6 numbers needed).
               If you enter a negative declination in dms notation,
               only the first field needs the minus sign.

   QUIT=       Quit?                                             [Y]/N
               Quit program or enter new sky systems.


Description:   SKYTRANS is an coordinate transformation utility that
               performs transformations between equatorial, galactic,
               ecliptic and supergalactic coordinates. Input coordinate
               system is defined by SKYSYSIN= Known sky systems are:

               * == equatorial system.
               G == Galactic system.
               E == Ecliptic system.
               S == Supergalactic system.

               If the equatorial system is selected, an epoch is needed
               also. This number is in years after 0.0. The same
               remarks can be made for the output sky system.
               Positions can be given entered as:

               COORD=123.4 34.0        | Two numbers in degrees (all systems)
               COORD=4 23 23.3 5 34 0  | hms 4h23m23.3s dms 5d34m0s
                                         (equatorial)
               COORD=4 23 23.3 5 34 0  | dms 4d23m23.3s dms 5d34m0s
                                         (not equatorial)
               COORD=4 23 23.3 -5 34 0 | hms 4d23m23.3s dms -5d34m0s

Notes:

Example:       <USER> SKYTRANS
               SKYTRANS  Version 1.0  (Nov  4 1992)
               <USER> SKYTRANS SKYSYSIN=
               <USER> SKYTRANS EPOCHIN=
               <USER> SKYTRANS SKYSYSOUT=S

                      RA DEC (1950.0)       ==>    Super gal. l b
               ===============================================================
               <USER> SKYTRANS COORD=2 10 0 45 0 0
                2h10m 0.00s , 45d 0m  0.0s  ==>  344d50m 35.0s , - 2d 0m 19.3s
               ( 32.500000,  45.000000 deg. ==> 344.843068,  -2.005373 deg.)
               <USER> SKYTRANS COORD=
               <USER> SKYTRANS QUIT=
               <STATUS>  SKYTRANS   +++ FINISHED +++


Updates:       Nov 3,  1992: VOG, Document created.
               Sep 15, 1995: VOG, FILENAME= added.

#<
*/

/*  skytrans.c: include files     */

#include    "stdio.h"        /* Defines ANSI C input and output utilities */
#include    "stdlib.h"       /* Defines the ANSI C functions for number */
                             /* conversion, storage allocation, and similar tasks.*/
#include    "string.h"       /* Declares the ANSI C string functions*/
                             /* like:strcpy, strcat etc.*/
#include    "math.h"         /* Declares the mathematical functions and macros.*/
#include    "cmain.h"        /* Defines the main body of a C program with */
                             /* MAIN_PROGRAM_ENTRY and IDENTIFICATION */
#include    "gipsyc.h"       /* Defines the ANSI-F77 types for Fortran to C intface */
                             /* including def. of char2str,str2char,tofchar,zadd */
                             /* and macros tobool and toflog */
#include    "float.h"        /* Definition of FLT_MAX etc.*/
#include    "ctype.h"        /* Declares ANSI C functions for testing characters */
                             /* like: isalpha, isdigit etc. also tolower, toupper.*/

/* Common includes */

#include    "init.h"         /* Declare task running to HERMES and initialize.*/
#include    "finis.h"        /* Informs HERMES that servant quits and cleans up the mess.*/
#include    "anyout.h"       /* General character output routine for GIPSY programs.*/
#include    "setfblank.h"    /* Subroutine to set a data value to the universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/

/* User input routines */

#include    "userlog.h"
#include    "userdble.h"
#include    "userchar.h"
#include    "usercharu.h"
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/

/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/

#include    "dcddble.h"
#include    "epoco.h"
#include    "skyco.h"
#include    "eclipco.h"



/* DEFINITIONS: */

/* Initialize Fortran compatible string with macro 'fmake' */

#define fmake(fchr,size) { \
                           static char buff[size+1]; \
                           int i; \
                           for (i = 0; i < size; buff[i++] = ' '); \
                           buff[i] = 0; \
                           fchr.a = buff; \
                           fchr.l = size; \
                         }

/* Malloc version of 'fmake'  */
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )
#define PI             3.141592653589793
#define RAD(a)         ( a * 0.017453292519943295769237 )
#define DEG(a)         ( a * 57.295779513082320876798155 )

#define RELEASE        "1.0"           /* Version number */
#define MAXAXES        10              /* Max. axes in a set */
#define MAXSUBSETS        1            /* Max. allowed subsets */
#define MAXBUF         4096            /* Buffer size for I/O */
#define STRLEN         80              /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define MAXFILENAMELEN 256
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0
#define FIELDS         6
#define FIELDLEN       20
#define SHRT           20
#define EQUATORIAL     1
#define GALACTIC       2
#define ECLIPTIC       3
#define SUPERGALACTIC  4



/* Miscellaneous */

static fchar    Key, Mes;
static float    blank;              /* Global value for BLANK. */
static char     message[120];       /* All purpose character buffer. */
static bool     agreed;             /* Loop guard. */



void anyoutC( int dev, char *anyCstr )
/*------------------------------------------------------------------*/
/* The C version of 'anyout_c' needs two parameters:                */
/* an integer and a C-type string. The integer determines           */
/* the destination of the output which is:                          */
/*    0  use default [set by HERMES to 3 but can be changed by user]*/
/*    1  terminal                                                   */
/*    2  LOG file                                                   */
/*    8  terminal, suppressed in "experienced mode"                 */
/*   16  terminal, only when in "test mode"                         */
/*------------------------------------------------------------------*/
{
   fint ldev = (fint) dev;
   anyout_c( &ldev, tofchar( anyCstr ) );
}



static void dms( double degrees, char *convstr, int prec )
/*-----------------------------------------------------------------------------
 * Convert degrees to deg/min/sec
 *-----------------------------------------------------------------------------
 */
{
   double    seconds;
   int       Idegs;
   double    min;
   int       Imin;
   int       negative;
   double    power;


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
   sprintf( convstr, "-%2dd%2dm%5.*fs", Idegs, Imin, prec, seconds );
   else
   sprintf( convstr,  "%2dd%2dm%5.*fs", Idegs, Imin, prec, seconds );
}


static void hms( double degrees, char *convstr, int prec )
/*-----------------------------------------------------------------------------
 * Convert degrees to hours/min/sec
 *-----------------------------------------------------------------------------
 */
{
   double    seconds;
   double    hours;
   int       Ihours;
   double    min;
   int       Imin;
   int       negative;
   double    power;


   power = pow( 10.0, (double) prec );
   negative = 0;
   if ( degrees < 0 ) {
      negative = 1;
      degrees = -1.0 * degrees;
   }
   hours   = degrees / 15.0;
   Ihours  = (int) hours;
   min     = hours*60.0 - ((double)Ihours)*60.0;
   Imin    = (int) ( min );
   seconds = min*60.0 - ((double)Imin)*60.0;
   seconds = (double) ((int) (seconds * power) ) / power;
   if (negative)
   sprintf( convstr, "-%2dh%2dm%5.*fs", Ihours, Imin, prec, seconds );
   else
   sprintf( convstr,  "%2dh%2dm%5.*fs", Ihours, Imin, prec, seconds );
}



static void convpos( double epochIN, fint skysysIN,
                     double epochOUT, fint skysysOUT,
                     FILE *fp )
/*------------------------------------------------------------*/
/* Convert position from (sky1, epoch1) to (sky2, epoch2)     */
/*------------------------------------------------------------*/

{
   fint     nitems;
   fint     dfault;
   fint     r1, r2, r3;
   fchar    Postxt;
   char     field[FIELDS][FIELDLEN];
   double   posI[FIELDS];
   double   posO[FIELDS];
   fint     one = 1;
   bool     quit;
   bool     degrees;
   char     str1[SHRT], str2[SHRT], str3[SHRT], str4[SHRT];
   bool     agreed;
   char     mes1[40], mes2[40];
   double   epochdum;
   int      i;


   fmake( Postxt, FIELDS * FIELDLEN );
   Postxt.l = FIELDLEN;

   anyoutC( 3, " " );
   if (skysysIN == EQUATORIAL) {
      sprintf( mes1, "       RA DEC (%6.1f)        ==>", epochIN );
   }
   if (skysysIN == ECLIPTIC) {
      sprintf( mes1, "ecliptic long. lat.(%6.1f)   ==>", epochIN );
   }
   if (skysysIN == GALACTIC) {
      sprintf( mes1, "Galactic l b                ==>" );
   }
   if (skysysIN == SUPERGALACTIC) {
      sprintf( mes1, "Super gal. l b              ==>" );
   }
   if (skysysOUT == EQUATORIAL) {
      sprintf( mes2, "       RA DEC (%6.1f)", epochOUT );
   }
   if (skysysOUT == ECLIPTIC) {
      sprintf( mes2, "  ecliptic long. lat.(%6.1f)", epochOUT );
   }
   if (skysysOUT == GALACTIC) {
      sprintf( mes2, "     Galactic l b" );
   }
   if (skysysOUT == SUPERGALACTIC) {
      sprintf( mes2, "    Super gal. l b" );
   }
   sprintf( message, "%s%s", mes1, mes2 );
   anyoutC( 3, message );
   memset( message, '=', 63 );
   anyoutC( 3, message );

   do{
      do {
         do {
            nitems    = FIELDS;
            dfault    = REQUEST;
            Key       = tofchar("COORD=");
            if (skysysIN == 1) {
               Mes = tofchar("Position RA,DEC as d d or hms dms:   [stop]");
            } else {
               Mes = tofchar("Position l,b as d d or dms dms:      [stop]");
            }
            r1        = userchar_c( Postxt, &nitems, &dfault, Key, Mes );
            if (r1 == 2) degrees = YES; else degrees = NO;
            agreed    = ((r1 == 0) || (r1 == FIELDS) || (r1 == 2));
            if (!agreed) reject_c( Key, tofchar("Wrong number of coords.") );
         } while (!agreed);
         quit = (r1 == 0);
         if (!quit) {
            for (i = 0; i < r1; i++) {
               char   ch;
               int    k;
               for (k = 0; k < STRLEN; k++) {
                  ch = Postxt.a[i*FIELDLEN+k];
                  if (ch != ' ') {
                     field[i][k] = ch;
                  } else {
                     break;
                  }
               }
               field[i][k] = '\0';
            }
            for (i = 0; i < r1; i++) {
               r2 = dcddble_c( tofchar(field[i]), &posI[i], &one, &r3 );
               agreed = (r3 == 0);
               if (!agreed) {
                  reject_c( Key, tofchar("Cannot convert!") );
                  break;
               }
            }
         }
      } while (!agreed);
      if (!quit) {
         if (!degrees) {
            int negative = (posI[0] < 0.0);
            posI[0] = (fabs(posI[0]) + fabs(posI[1])/60.0 + fabs(posI[2])/3600.0 );
            if (skysysIN == 1) {
               posI[0] *= 15.0;
            }
            if (negative)
               posI[0] *= -1.0;

            negative = (posI[3] < 0.0);
            posI[1] = (fabs(posI[3]) + fabs(posI[4])/60.0 + fabs(posI[5])/3600.0 );
            if (negative)
               posI[1] *= -1.0;
         }
         posO[0] = posI[0];
         posO[1] = posI[1];
         if (skysysIN == EQUATORIAL) {
            epochdum = 1950.0;
            epoco_c( &posI[0], &posI[1], &epochIN, &posO[0], &posO[1], &epochdum );
         }
         if (skysysIN == ECLIPTIC) {
            epochdum = 1950.0;
            eclipco_c( &posI[0], &posI[1], &epochIN, &posO[0], &posO[1], &epochdum );
         }
         r3 = skyco_c( &posO[0], &posO[1], &skysysIN, &posO[0], &posO[1], &skysysOUT );
         if (skysysOUT == EQUATORIAL) {
            epochdum = 1950.0;
            epoco_c( &posO[0], &posO[1], &epochdum, &posO[0], &posO[1], &epochOUT );
         }
         if (skysysOUT == ECLIPTIC) {
            epochdum = 1950.0;
            eclipco_c( &posO[0], &posO[1], &epochdum, &posO[0], &posO[1], &epochOUT );
         }
         if (skysysIN == EQUATORIAL) {
            hms(posI[0], str1, 2);
            dms(posI[1], str2, 1);
            if (skysysOUT == EQUATORIAL) {
               hms(posO[0], str3, 2);
               dms(posO[1], str4, 1);
            }
            else {
               dms(posO[0], str3, 1);
               dms(posO[1], str4, 1);
            }
         } else {
            /* Input NOT equatorial */
            dms(posI[0], str1, 1);
            dms(posI[1], str2, 1);
            if (skysysOUT == EQUATORIAL) {
               hms(posO[0], str3, 2);
               dms(posO[1], str4, 1);
            } else {
               dms(posO[0], str3, 1);
               dms(posO[1], str4, 1);
            }
         }
         sprintf( message, "%s , %s  ==>  %s , %s", str1, str2, str3, str4 );
         anyoutC( 3, message );
         sprintf( message,
                 "(%10f, %10f deg. ==> %10f, %10f deg.)",
                  posI[0], posI[1], posO[0], posO[1] );
         if (fp)
            fprintf( fp, "%15f %15f\n", posO[0], posO[1] );
         anyoutC( 3, message );
       }
       cancel_c( Key );
   } while (!quit);
}




MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint     nitems;
   fint     dfault;
   fint     r1;
   fint     skysysOUT = 0;
   fint     skysysIN  = 0;
   double   epochIN  = 1950.0;
   double   epochOUT = 1950.0;
   bool     quit;
   fchar    Skysys;
   fchar    Filename;
   FILE     *fp = NULL;



   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      (void) myname_c( Task );             /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );
   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   fmake( Skysys, FIELDLEN );
   
   nitems = 1;
   dfault = REQUEST;
   fmake( Filename, MAXFILENAMELEN );
   r1 = userchar_c( Filename, &nitems, &dfault, 
                    tofchar("FILENAME="), 
                    tofchar("Write output to file with name:   [skip writing]" ) );
                    
   if (r1)
   {      
      char   longmes[300];   
      Filename.a[nelc_c(Filename)] = '\0';      
      fp = fopen( Filename.a, "r" );
      if (fp != NULL)
      {
         sprintf( longmes, "Cannot open file because file [%s] exists!", 
                  Filename.a );
         anyoutC( 1, longmes );
         fp = NULL;
      }
      else
      {
         fp = fopen( Filename.a, "w" );
         if (fp == NULL)
         {
            sprintf( longmes, "Cannot open [%s]", Filename.a );
            anyoutC( 1, longmes );
         }
      }
   }
   
   do {
      do {
         nitems      = 1;
         dfault      = REQUEST;
         Key         = tofchar("SKYSYSIN=");
         Mes         = tofchar("Give sky system of input */G/E/S:    [*(=equatorial)]");
         Skysys.a[0] = '*';
         r1          = usercharu_c( Skysys, &nitems, &dfault, Key, Mes );
         if (strchr(Skysys.a, '*') != NULL) skysysIN = EQUATORIAL;
         if (strchr(Skysys.a, 'G') != NULL) skysysIN = GALACTIC;
         if (strchr(Skysys.a, 'E') != NULL) skysysIN = ECLIPTIC;
         if (strchr(Skysys.a, 'S') != NULL) skysysIN = SUPERGALACTIC;
         agreed = ((skysysIN >=1) && (skysysIN <= 4));
         if (!agreed) reject_c( Key, tofchar("Wrong sky system!") );
      } while (!agreed);
      cancel_c( Key );

      if ((skysysIN == EQUATORIAL) || (skysysIN == ECLIPTIC)) {
         nitems    = 1;
         Key       = tofchar("EPOCHIN=");
         if (skysysIN == EQUATORIAL) {
            Mes     = tofchar("Give epoch of input:              [1950.0]");
            epochIN = 1950.0;
         }
         if (skysysIN == ECLIPTIC) {
            Mes     = tofchar("Give epoch of input:              [1983.5]");
            epochIN = 1983.5;
         }
         r1        = userdble_c( &epochIN, &nitems, &dfault, Key, Mes );
         cancel_c( Key );
      }

      do {
         nitems      = 1;
         dfault      = REQUEST;
         Key         = tofchar("SKYSYSOUT=");
         Mes         = tofchar("Give sky system of output */G/E/S:    [G(alactic)]");
         Skysys.a[0] = 'G';
         r1          = usercharu_c( Skysys, &nitems, &dfault, Key, Mes );
         if (strchr(Skysys.a, '*') != NULL) skysysOUT = EQUATORIAL;
         if (strchr(Skysys.a, 'G') != NULL) skysysOUT = GALACTIC;
         if (strchr(Skysys.a, 'E') != NULL) skysysOUT = ECLIPTIC;
         if (strchr(Skysys.a, 'S') != NULL) skysysOUT = SUPERGALACTIC;
         agreed = ((skysysOUT >=1) && (skysysOUT <= 4));
         if (!agreed) reject_c( Key, tofchar("Wrong sky system!") );
      } while (!agreed);
      cancel_c( Key );

      if ((skysysOUT == EQUATORIAL) || (skysysOUT == ECLIPTIC)) {
         nitems    = 1;
         Key       = tofchar("EPOCHOUT=");
         if (skysysOUT == EQUATORIAL) {
            Mes      = tofchar("Give epoch of output:              [1950.0]");
            epochOUT = 1950.0;
         }
         if (skysysOUT == ECLIPTIC) {
            Mes      = tofchar("Give epoch of output:              [1983.5]");
            epochOUT = 1983.5;
         }
         r1        = userdble_c( &epochOUT, &nitems, &dfault, Key, Mes );
         cancel_c( Key );
      }

      convpos( epochIN, skysysIN, epochOUT, skysysOUT, fp );
      nitems = 1;
      quit   = toflog(YES);
      Key    = tofchar("QUIT=");
      Mes    = tofchar("Quit?               [Y]/N");
      r1     = userlog_c( &quit, &nitems, &dfault, Key, Mes );
      quit   = tobool( quit );
      cancel_c( Key );
   } while (!quit);
   if (fp)
      fclose( fp );
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
