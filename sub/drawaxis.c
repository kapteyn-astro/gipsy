/* drawaxis.c

          Copyright (c) Kapteyn Laboratorium Groningen 1993
          All Rights Reserved.
*/


#include    "stdio.h"        /* Defines ANSI C input and output utilities */
#include    "stdlib.h"       /* Defines the ANSI C functions for number */
                             /* conversion, storage allocation, and similar tasks.*/
#include    "string.h"       /* Declares the ANSI C string functions*/
                             /* like:strcpy, strcat etc.*/
#include    "math.h"         /* Declares the mathematical functions and macros.*/
#include    "gipsyc.h"       /* Defines the ANSI-F77 types for Fortran to C intface */
                             /* including def. of char2str,str2char,tofchar,zadd */
                             /* and macros tobool and toflog */
#include    "float.h"        /* Definition of FLT_MAX etc.*/
#include    "ctype.h"        /* Declares ANSI C functions for testing characters */
                             /* like: isalpha, isdigit etc. also tolower, toupper.*/


#include    "setdblank.h"    /* Set a double to blank */
#include    "axtype.h"       /* Type, units, projection */
#include    "proco.h"        /* Conversion grid to sky coordinates vv. */
#include    "cotrans.h"      /* General conversion from grids to phys. coords. */
#include    "axcoord.h"      /* Coordinate type and units as returned by cotrans */
#include    "factor.h"       /* conversion factor between two different units */
#include    "printusing.h"   /* Convert numbers to formatted strings */
#include    "nelc.h"         /* Num. of chars. in Fortran string */
#include    "gdsd_rchar.h"   /* Obtain string from set header */
#include    "gdsd_rdble.h"   /* Obtain double from set header */
#include    "gdsc_grid.h"    /* Extract a grid value from a coordinate word */
#include    "gdsc_ndims.h"   /* Determine dimension of (sub)set */
#include    "gdsc_word.h"    /* Apply a grid to a coordinate word and return the new one */
#include    "anyout.h"       /* General character output routine */
#include    "dcddble.h"      /* Decodes double precision value */
#include    "dcdpos.h"       /* Decodes position */
#include    "sortda.h"       /* Sort the axlogs array in Ascending order */
#include    "skyrot.h"       /* Returns the rotation angle of the sky in a set */

/* PGplot includes */

#include     "pgmove.h"
#include     "pgdraw.h"
#include     "pgptxt.h"      /* Text plotting */
#include     "pgqvp.h"       /* Inquire viewport to get sizes in mm of current device */
#include     "pgqch.h"       /* Get current character height */
#include     "pgrnd.h"       /* Find the smallest "round" number greater than x */
#include     "pgbbuf.h"      /* Start graphics buffer */
#include     "pgebuf.h"      /* Flush graphics buffer */
#include     "pglen.h"       /* Length of a string in graphics mode */

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



#define YES           1         /* C versions of .TRUE. and .FALSE. */
#define NO            0
#define MYMAX(a,b)    ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)    ( (a) > (b) ? (b) : (a) )
#define ABS(a)        ( (a) < 0 ? (-(a)) : (a) )
/* Pre Apr 2009 def.: #define NINT(a)       ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) ) */
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )


#define TITLEOFFSET   2.0 * charheight

#define PLMOVE( x, y ) {\
                         float a, b;\
                         a = (float) (x); b = (float) (y);\
                         pgmove_c( &a, &b );\
                        }

#define PLDRAW( x, y ) {\
                         float a, b;\
                         a = (float) (x); b = (float) (y);\
                         pgdraw_c( &a, &b );\
                        }

#define PLTEXT( x, y, angle, just, Mes ) \
                       {\
                         float a, b, c, d;\
                         a = (float) (x); b = (float) (y);\
                         c = (angle);\
                         d = (just);\
                         pgptxt_c( &a, &b, &c, &d, Mes );\
                       }



static int hmsdms( double  *degrees,
                   fchar   Label,
                   fint    *init,
                   double  *step,
                   fchar   Axformat )
/*-------------------------------------------------------------------*/
/* Convert a number in degrees, to hms, or dms and return formatted  */
/* number in characters string 'Label'. The return value of the      */
/* function is the length of the formatted string WITH control       */
/* characters.                                                       */
/* The step size must be in degree also. If init is 1                */
/* the routine is initialized and the label has max. information.    */
/* Else, the position is compared with the previous position and     */
/* some information is made hidden.                                  */
/*-------------------------------------------------------------------*/
{
   int           Ihour, Imin, Ideg;
   double        Dhour, Dmin;
   double        sec;
   double        deg;
   static int    oldhour, oldmin, olddeg;
   int           ndigit = 0;
   int           n = 0;
   double        prec;
   char          postxt[80];
   bool          first;
   bool          hms;
   bool          dms;
   bool          prdeg, prhour, prmin, prsec;  /* Flags for incl. d,h,m,s */
   bool          precgiven;
   int           i;                            /* Counter */
   int           slen;
   int           neg;
   static int    oldneg;                       /* Value changed sign? */


   neg = (*degrees < 0.0);
   prdeg = prhour = prmin = prsec = NO;
   Ihour = Imin = Ideg = 0;
   Dmin  = 0.0;
   first = (bool) (*init);
   postxt[0] = '\0';
   hms = NO; dms = NO;
   precgiven = NO;
   slen = (int) nelc_c( Axformat );
   for (i = 0; i < slen; i++)
   {
      if (Axformat.a[i] == 'H')
      {
         hms    = YES;
         prhour = YES;
      }
      if (Axformat.a[i] == 'h')
         hms = YES;
      if (Axformat.a[i] == 'D')
      {
         dms   = YES;
         prdeg = YES;
      }
      if (Axformat.a[i] == 'd')
         dms   = YES;
      if (Axformat.a[i] == 'M')
         prmin = YES;
      if (Axformat.a[i] == 'S')
         prsec = YES;
      if (Axformat.a[i] == '.')
      {
         precgiven = YES;
         ndigit = slen - 1 - i;
      }
   }
   if ((!hms) && (!dms))
      return( 0 );            /* Wrong mode, return! */


   /*--------------------------------------------------------*/
   /* Calculate the integer values for the hours and minutes */
   /*--------------------------------------------------------*/
   deg   = (*degrees);
   if (hms)
   {
      /* HMS mode, note that the type casting has a function here. */
      /* It prevents strings like 45o59m60.0s                      */
      deg   = fmod( (deg + 360.0), 360.0 ) ;
      Dhour = (float)deg / 15.0;
      Ihour = (int) (Dhour);
      Dmin  = (float) fabs(Dhour * 60.0 - ((double)Ihour) * 60.0);
   }
   if (dms)
   {
      if (deg < 0.0)
      {
         strcpy( postxt, "-" );
         deg = fabs( deg );
      }
      else
      {
         postxt[0] = '\0';
      }
      Ideg = (int) ( (float) deg );
      Dmin = (float) fabs(deg * 60.0 - ((double)Ideg) * 60.0);
   }
   Imin  = (int) Dmin;
   sec   = (float) fabs(Dmin  * 60.0 - ((double)Imin) * 60.0);

   if (first)
   {
      if (hms) oldhour = Ihour;
      if (dms) olddeg  = Ideg;
      oldmin  = Imin;
   }

   /*-----------------------------------------------------------------*/
   /* Determine precision for seconds if user did not give it himself */
   /*-----------------------------------------------------------------*/
   if (!precgiven)
   {
      double stepsize = 0.0;
      /* Precision must be calculated */
      if (hms)
         stepsize = 240.0 * (*step);
      else
         stepsize = 3600.0 * (*step);

      for (ndigit = 0, prec = fabs(stepsize); prec < 1.0; prec *= 10.0, ndigit++);
   }

   /*---------------------------------*/
   /* Do the formatting of the string */
   /*---------------------------------*/
   if (hms)    /* Format: 16h23m18.342s */
   {
      if (first)
      {
         if (!precgiven)
         /*-------------------------------------------------------------*/
         /* Precision for seconds is not known, do not print seconds if */
         /* number of seconds is (near) zero.                           */
         /*-------------------------------------------------------------*/
         {
            if (fabs(sec) >= pow(10.0, (double) -ndigit))
               prsec = YES;
         }
         else
            prsec = YES;
         if (prsec)
            n = sprintf( postxt, "%2d\\uh\\d%2d\\um\\d%.*f\\us\\d" ,
                                 Ihour, Imin, ndigit, sec );
         else
            n = sprintf( postxt, "%2d\\uh\\d%2d\\um\\d" ,
                                 Ihour, Imin );
      }
      else
      {
         if (!prhour)
            prhour = (Ihour != oldhour);           /* The hour needs not to be repeated */
         if (!prhour)
         {
            if (!prmin)
               prmin = (Imin != oldmin);
            if (!prmin)
               prsec = YES;
            else if (fabs(sec) > pow(10.0, (double) -ndigit))
               prsec = YES;
         }
         else
         {
            prmin = YES;
            if (fabs(sec) >= pow(10.0, (double) -ndigit))
               prsec = YES;
         }
         (void) sprintf( postxt, "" ); /* empty */
         if (prhour)
         {
            n = sprintf( postxt, "%.*s%d\\uh\\d", strlen(postxt), postxt, Ihour );
            oldhour = Ihour;
         }
         if (prmin)
         {
            n = sprintf( postxt, "%.*s%2d\\um\\d",strlen(postxt), postxt, Imin );
            oldmin = Imin;
         }
         if (prsec)
         {
            n = sprintf( postxt, "%.*s%.*f\\us\\d",strlen(postxt), postxt,
                         ndigit, sec );
         }
      }
   }

   if (dms)      /* Format: -311o23'18.342" */
   {
      if (first)
      {
         if (!precgiven)
         /*-------------------------------------------------------------*/
         /* Precision for seconds is not known, do not print seconds if */
         /* number of seconds is (near) zero.                           */
         /*-------------------------------------------------------------*/
         {
            if (fabs(sec) > pow(10.0, (double) -ndigit))
               prsec = YES;
         }
         else
            prsec = YES;
         if (prsec)
         {
            n = sprintf( postxt, "%.*s%d\\uo\\d%2d\\u'\\d%.*f\\u\"\\d",
                         strlen(postxt), postxt,
                         Ideg , Imin, ndigit, sec );
         }
         else
         {
            n = sprintf( postxt, "%.*s%d\\uo\\d%2d\\u'\\d",
                         strlen(postxt), postxt,
                         Ideg , Imin );
         }
      }
      else
      {
         if (!prdeg)
            prdeg = (Ideg != olddeg) ||           /* The deg needs not to be repeated */
                    (neg != oldneg);
         if (!prdeg)
         {
            if (!prmin)
               prmin = (Imin != oldmin);
            if (!prmin)
            {
               prsec = YES;
            }
            else if (fabs(sec) > pow(10.0, (double) -ndigit))
               prsec = YES;
         }
         else
         {
            prmin = YES;
            if (fabs(sec) > pow(10.0, (double) -ndigit)) prsec = YES;
         }
         if (!prdeg)
            (void) sprintf( postxt, "" ); /* empty */
         if (prdeg)
         {
            n = sprintf( postxt, "%.*s%d\\uo\\d", strlen(postxt), postxt, Ideg );
            olddeg = Ideg;
         }
         if (prmin)
         {
            n = sprintf( postxt, "%.*s%2d\\u'\\d",strlen(postxt), postxt, Imin );
            oldmin = Imin;
         }
         if (prsec)
         {
            n = sprintf( postxt, "%.*s%.*f\\u\"\\d",strlen(postxt), postxt,
                         ndigit, sec );
         }
      }
   }
   /*----------------------------------------------------------------------*/
   /* There is a problem about the length here, because escape characters  */
   /* are counted also. Therefore we must count the number of \d en \u and */
   /* subtract 2 in length for each.                                       */
   /*----------------------------------------------------------------------*/

   if (hms)
      oldhour = Ihour;
   if (dms)
      olddeg  = Ideg;
   oldmin  = Imin;
   oldneg  = neg;

   strcpy( Label.a, postxt );
   return( n );
}




static void procoerr( char *mes, fint err )
/*-----------------------------------------------------------*/
/* Generate a message that belongs to a 'proco' error        */
/*-----------------------------------------------------------*/
{
   if      (err == 1)  strcpy( mes, "PROCO unknown projection" );
   else if (err == 2)  strcpy( mes, "PROCO unknown mode" );
   else if (err == 3)  strcpy( mes, "PROCO CROTA2 = 90.0 for mode 1 and 2" );
   else if (err == 4)  strcpy( mes, "PROCO CDELT1 or CDELT2 equal to zero" );
   else                strcpy( mes, "PROCO Unknown error" );
}



static void cotranserr( char *mes, fint err )
/*-----------------------------------------------------------*/
/* Generate a message that belongs to a cotrans' error       */
/*-----------------------------------------------------------*/
{
   if      (err == 1)  strcpy( mes, "COTRANS unknown projection" );
   else if (err == 2)  strcpy( mes, "COTRANS unknown mode" );
   else if (err == 3)  strcpy( mes, "COTRANS CROTA2 = 90.0 for mode 1 and 2" );
   else if (err == 4)  strcpy( mes, "COTRANS CDELT1 or CDELT2 equal to zero" );
   else if (err == 5)  strcpy( mes, "COTRANS input sky system unknown" );
   else if (err == 6)  strcpy( mes, "COTRANS output sky system unknown" );
   else if (err == 7)  strcpy( mes, "COTRANS input and output sky system unknown" );
   else if (err == 8)  strcpy( mes, "COTRANS skypro error" );
   else if (err == 9)  strcpy( mes, "COTRANS unknown velocity system" );
   else if (err == 10) strcpy( mes, "COTRANS rest frequency less than or equal to zero" );
   else if (err == 11) strcpy( mes, "COTRANS crval equal to zero" );
   else if (err == 12) strcpy( mes, "COTRANS cdelt equal to zero" );
   else if (err == 13) strcpy( mes, "COTRANS no matching axis pair found" );
   else if (err == 14) strcpy( mes, "COTRANS incompatible sky systems" );
   else if (err == 15) strcpy( mes, "COTRANS cannot do epoch transformations" );
   else                strcpy( mes, "COTRANS Unknown error" );
}

   

static void poserr( char *mes, fint err )
/*-----------------------------------------------------------*/
/* Generate a message that belongs to a dcdpos error         */
/*-----------------------------------------------------------*/
{   
   if      (err == -1)  strcpy( mes, "DCDPOS illegal use of 'PC', 'AC' or 'D'");
   else if (err == -2)  strcpy( mes, "DCDPOS prefix incompatible with axis");
   else if (err == -3)  strcpy( mes, "DCDPOS position incomplete");
   else if (err == -4)  strcpy( mes, "DCDPOS error reading descriptor info");
   else if (err == -5)  strcpy( mes, "DCDPOS 'D' not allowed for positions");
   else if (err == -6)  strcpy( mes, "DCDPOS No grid separation defined for units");
   else if (err == -7)  strcpy( mes, "DCDPOS Too many positions");
   else if (err == -8)  strcpy( mes, "DCDPOS Cannot obtain header information");
   else if (err == -9)  strcpy( mes, "DCDPOS No mixed epochs allowed");
   else if (err == -10) strcpy( mes, "DCDPOS General decode error (detected by dcddble)" );
   else if (err == -11) strcpy( mes, "DCDPOS BLANKS decoded" );
   else                 strcpy( mes, "DCDPOS Unknown error" );
}



static void dcderr( char *mes, fint err )
/*-----------------------------------------------------------*/
/* Generate a message that belongs to a dcd conversion error */
/*-----------------------------------------------------------*/
{
   if      (err == -11) strcpy( mes, "bad call" );
   else if (err == -12) strcpy( mes, "unknown function" );
   else if (err == -13) strcpy( mes, "syntax error" );
   else if (err == -14) strcpy( mes, "illegal character" );
   else if (err == -15) strcpy( mes, "wrong repeat argument" );
   else if (err == -16) strcpy( mes, "wrong number of arguments" );
   else if (err == -17) strcpy( mes, "arithmetic error" );
   else if (err == -18) strcpy( mes, "not enough internal memory" );
   else if (err == -19) strcpy( mes, "conversion error" );
   else if (err == -20) strcpy( mes, "unequal list length" );
   else if (err == -21) strcpy( mes, "empty list" );
   else if (err == -22) strcpy( mes, "nested lists" );
   else if (err == -23) strcpy( mes, "output buffer overflow" );
   else if (err == -24) strcpy( mes, "floating overflow/underflow in conversion" );
   else                 strcpy( mes, "DCD... Unknown error" );
}


static void convtype( int typ, int sky, int vel, char *typestr, fchar Pardefstr )
/*---------------------------------------------------------------*/
/* Convert a given axis type into a string.                      */
/* If the type is a parameter axis, use a default from header    */
/* (can be 'X' or 'Y' etc.)                                      */
/*---------------------------------------------------------------*/
{
   switch ( typ )
   {
      case 0:                        /* unknown type */
         strcpy( typestr, " " );
         break;
      case 1:                        /* spatial axis longitude */
         if (sky == 1) strcpy( typestr, "R.A." );
         if (sky == 2) strcpy( typestr, "Gal. long." );
         if (sky == 3) strcpy( typestr, "Ecl. long." );
         if (sky == 4) strcpy( typestr, "SGal. long." );
         break;
      case 2:                        /* spatial axis latitude */
         if (sky == 1) strcpy( typestr, "Dec." );
         if (sky == 2) strcpy( typestr, "Gal. lat." );
         if (sky == 3) strcpy( typestr, "Ecl. lat." );
         if (sky == 4) strcpy( typestr, "SGal. lat." );
         break;
      case 3:                        /* spectral axis frequency */
         strcpy( typestr, "Frequency" );
         break;
      case 4:                        /* spectral axis velocity */
         strcpy( typestr, "Velocity" );
         break;
      case 5:                        /* spectral axis wavelength */
         strcpy( typestr, "Wavelength" );
         break;
      case 6:                        /* spectral axis inverse wavelength */
         strcpy( typestr, "Inverse Wavelength" );
         break;
      case 7:                        /* spectral axis log(wavelength) */
         strcpy( typestr, "log(wavelength)" );
         break;
      case 8:                        /* time axis */
         strcpy( typestr, "Time" );
         break;
      case 9:                        /* polarisation axis */
         strcpy( typestr, "Polarisation" );
         break;
      case 10:                       /* parameter axis, take name from header */      
         sprintf( typestr, "%.*s", nelc_c(Pardefstr), Pardefstr.a );
         break;
      case 11:                       /* sample axis of iras data */
         strcpy( typestr, "Sample" );
         break;
      case 12:                       /* tick axis of iras data */
         strcpy( typestr, "Tick" );
         break;
      case 13:                       /* detector axis of iras data */
         strcpy( typestr, "Detector" );
         break;
      case 14:                       /* snip axis of iras data */
         strcpy( typestr, "Snip" );
         break;
   }
}


/* static double proco_correct( double x ) */
/*------------------------------------------------------------*/
/* The function 'proco' doesn't know about 'crpix' values.    */
/* The calculated positions must be corrected for the non-    */
/* integer 'crpix'. This correction is included in the        */
/* function 'cotrans', so only 'proco' needs to be corrected. */
/*------------------------------------------------------------*/
/* INCORRECT VERSION. See documentation of cotrans.c
{
   double offset = x - floor(x);
   
   if (offset > 0.5)
      offset -= 1.0;
   return( offset );
}
*/


static double proco_correct( double x )
/*------------------------------------------------------------*/
/* The function 'proco' doesn't know about 'crpix' values.    */
/* The calculated positions must be corrected for the non-    */
/* integer 'crpix'. This correction is included in the        */
/* function 'cotrans', so only 'proco' needs to be corrected. */
/* Follow the same correction system as cotrans does in       */
/* cotrans.c                                                  */
/*------------------------------------------------------------*/
{
   return( x - NINT(x) );  
}



static int getminors( double deltaph, 
                      bool   dmsform, 
                      bool   hmsform )
/*------------------------------------------------------------*/
/* For a given interval, calculate a suitable number for the  */
/* amount of subdivisions.                                    */
/*------------------------------------------------------------*/
{
   float   step = (float) fabs(deltaph);
   fint    nsubint = 0;
      
     
   for (; step < 10.0; step *= 10.0 );
   if (!hmsform && !dmsform)
   {
      int k = (int) step;
      if      (k%7 == 0)
         nsubint = 7;
      else if (k%3 == 0) 
         nsubint = 3;
      else if (k%2 == 0) 
         nsubint = 2;         
      else if (k%5 == 0) 
         nsubint = 5;         
      else
          (void) pgrnd_c( &step, &nsubint );
   }
   else 
   {
      double x;
      if (hmsform)
         /* The input is a step size in degrees. 1 RAsec = 15 arcsec */
         deltaph *= 240.0;
      else
         deltaph *= 3600.0;

      x = deltaph;
      while ( fmod(x, 60.0) == 0.0 )
         x /= 60.0;

      if ( (x - (int) x) == 0.0 )
      {
         /* Could be divided by 60.0 */
         int k = (int) x;
         if      (k%7 == 0)
            nsubint = 7;
         else if (k%4 == 0) 
            nsubint = 4;
         else if (k%5 == 0) 
            nsubint = 5;
         else if (k%3 == 0) 
            nsubint = 3;
         else if (k%2 == 0) 
            nsubint = 2;
         else
            nsubint = 2;         
      }
      else 
         (void) pgrnd_c( &step, &nsubint );
   }        
   return( nsubint );
}




/*
#>            drawaxis.dc2

Function:     DRAWAXIS

Purpose:      Label an axis with (transformed) coordinates

Category:     PHYSICAL COORDINATES, PLOTTING

File:         drawaxis.c

Author:       M.G.R. Vogelaar

Updates:      Aug 10, 1993: VOG, Document created.
              Oct 10, 1994: VOG, Replaced 'X' option by new 'I' and
                                 'A' options (decouple tick marks and
                                 labels )
              Dec 16, 1994: VOG, Implemented 'e' option for a coordinate
                                 grid overlay. Some minor changes.            
              Mar 30, 1995: VOG, New function 'getminors'.
              Mar 6,  1996: VOG, Removed bug in 'getminors' if step < 1
              Aug 12, 1997: VOG, Adapted for AITOFF projection
              Aug 12, 1999: VOG, Changed default center for offset axis
                                 to grid center (instead of PC at grid 0). 
              Sep  7, 2003: VOG, Try to get axis name from header if axis
                                 is of parameter type.
              Jun 27, 2005: VOG, Change correction for crpix in function 
                                 proco_correct. Definition now the same as
                                 in cotrans and gdsbox (NINT, not floor).
              Apr 14, 2009: VOG, -Changed definition of macro NINT so that is
                                 uses floor(). Change is necessary for
                                 consistency with other routines that proces
                                 coordinates.
                                 -Removed bug. procoXoff, procoYoff were set to
                                 0.0 in loop. So always one of them was 0.0.
                                 Now initialization is done outside loop.
                                 Mouse positions now comparable to axis labels
                                 for non-integer CRPIX.


Use:          INTEGER DRAWAXIS ( OPTIONS,    INPUT    CHARACTER*(*)
                                 MARGIN,     INPUT    DOUBLE PRECISION ARRAY (2)
                                 SCALE,      INPUT    DOUBLE PRECISION ARRAY (2)
                                 ORIGMM,     INPUT    DOUBLE PRECISION ARRAY (2)
                                 BOX,        INPUT    DOUBLE PRECISION ARRAY (2)
                                 SETEXIST,   INPUT    LOGICAL
                                 SETIN,      INPUT    CHARACTER*(*)
                                 SUBIN,      INPUT    INTEGER
                                 LDEV,       INPUT    INTEGER
                                 TICKSIZE,   INPUT    DOUBLE PRECISION
                                 START,      INPUT    CHARACTER*(*)
                                 DELTA,      INPUT    CHARACTER*(*)
                                 MINORS,     INPUT    INTEGER
                                 AXFORMAT,   INPUT    CHARACTER*(*)
                                 AXLOGS,     INPUT    DOUBLE ARRAY
                                 AXLOGLEN    INPUT    INTEGER
                                 CGSTEP,     INPUT    DOUBLE PRECISION
                                 AXTITLE,    OUTPUT   CHARACTER*(*)
                                 TITLEXY,    OUTPUT   DOUBLE PRECISION ARRAY (8)
                                 TITLE_ID )  OUTPUT   CHARACTER*(*)


              DRAWAXIS   One axis will be plotted and labeled with
                         coordinates If successful, the routine returns the
                         length of a default title created by the routine.
                         Otherwise it returns:
                         -1  Something wrong with your box
                         -2  No axis was specified
                         -3  The conversion to hour, min, sec is not possible
                         -4  No transformation possible for this spatial map
                         -5  Your start position is not within range of axis
                         -6  Axis has no length.
                         -7  Subset dimension not 1 or 2.
                         -8  Cannot convert START.
                         -9  Cannot convert DELTA.

                         -55 Unknown DELTAUNITS to convert from
                         -56 Unknown (axis) units to convert to
                         -57 Both DELTAUNITS and axis units are unknown
                         -58 DELTAUNITS and header are incompatible.
              OPTIONS    String with one or more characters of:
                         B = Plot Bottom axis
                         R = Plot Right axis
                         L = Plot Left axis
                         T = Plot Top axis

                         W = Label current axis with untransformed
                             World coordinates
                         P = Label current axis with transformed Physical
                             coordinates, SETIN and SUBIN must be
                             part of the input. If axis are spatial and
                             rotated, plot offsets only.
                         O = Plot Offsets. Label START with zero. Plot labels
                             at separation DELTA
                         F = Calculate coordinates in FITS way, i.e.
                             Xphys = CRVAL + Xgrid * CDELT
                         I00 or I10, I01, I11
                             Plot/do not plot tIcks in(out)side frame
                             on == 1, off == 0
                             inside == 1, outside == 0
                         A00 or A10, A01, A11
                             Plot/do not plot lAbels in(out)side frame
                             on == 1, off == 0
                             inside == 1, outside == 0
                         Z   Plot labels as 10**(World coordinate).
              MARGIN     Space in mm in x and y direction for labels.
              SCALE      Scale in x and y direction in grids/mm
              ORIGMM     Origin x, y of the plot in mm. This is the location
                         of the first position in BOX.
                         (!) Note that the same SCALE and ORIGIN applies to
                         all axes.
              BOX        Box in world coordinates (if the axis is part
                         a set, the world coordinates are grids). The
                         axis range of the current axis will be extracted
                         from these numbers.

                         If you want coordinate transformations:

              SETEXIST   Set/subset is given
              SETIN      Name of set (set must exist) or an empty string.
              SUBIN      Coordinate word of the subset for which an axis
                         should be plotted. Subset dimension can be 1 or 2.
              LDEV       Device to which output is directed.
                         (See also anyout.dc2)

                         About the labels:

              TICKSIZE   Size of the label ticks in mm. If TICKSIZE is smaller
                         than 0, a default size of 1/3 of the character height
                         is used.
              START      Start writing labels at this point. The string is
                         converted to a position (Gipsy syntax for positions).
                         If the string is an empty string, a suitable default
                         will be calculated.
              DELTA      Plot major ticks with separation DELTA, start
                         at START. If the input string is empty, a suitable
                         default will be calculated. If SETIN is specified, the
                         value of DELTA can be given in (some) units.
              MINORS     Number of minor tick intervals(!). If the input value
                         is negative, a default value will be calculated.
              AXFORMAT   AXFORMAT is a string used as format template. Also the
                         formats hms/dms can be used. In these formats, a
                         capital has a special meaning.
              AXLOGS     Array containing numbers to label logarithmic values.
                         e.g. 1 3 6 will plot the labels
                         ...
                            0.1 at position log(0.1)
                            0.3  "    "     log(0.3)
                         ...
                            60   "    "     log(60)
                         ...
              AXLOGLEN   Length of the AXLOGS array. If length equal to zero
                         only logarithmic labels 10^n are plotted (n integer)

              CGSTEP     Step size in grids between sample positions used to
                         plot a coordinate grid. 

                         Output:

              AXTITLE    A default title. If there was no set specified, this
                         title is an empty string. The maximum length of a
                         default string is 256.
              TITLEXY    Suggested x,y of title. The title must be plotted in the 
                         the calling environment after correction for the 
                         character height of the title. The array has 8 elements.
                         Element 1&2 for the bottom axis, 3&4 for the right axis,
                         5&6 for the top axis and 7&8 for the left axis.
              TITLE_ID   One of the characters B,R,T,L for axis identification.


Description:  The routine DRAWAXIS plots one axis. The range of an axis is
              given in BOX. BOX has four numbers: x,y of lower left position and
              x,y of upper right position. The numbers are the world coordinates
              of the axis. If a set is given, this box is the range in grids to
              be plotted (and automatically also the range in physical
              coordinates). If the routine was successful, it will return a
              default title AXTITLE if a set was given. In TITLEXY a default
              position in mm of this title is returned in a special position
              in the TITLEXY array. The TITLEXY array has 8 elements.
              Element 1&2 for the bottom axis, 3&4 for the right axis,
              5&6 for the top axis and 7&8 for the left axis. If there is not an
              input set, the title will be an empty string, but a default 
              position is available. This is a position just outside the range 
              of the plotted labels and it must be corrected in the calling 
              environment for the character height of the title. The title 
              always belongs to an axis, therefore also an identification, 
              TITLE_ID (one of the characters B,T,R,L), is returned.
              If the routine encounters an error it will return with a negative 
              number in DRAWAXIS. The list of errors is given above. The values 
              for the default title position will be blank.
              The axis that must be plotted is specified in OPTIONS. This is a 
              string with at least one character (one of B(ottom), R(ight), 
              L(eft) or T(op)).
              If you want to plot world (grid) coordinates along an axis,
              include W(orld coordinate labels and if you want to plot
              transformed physical coordinates, include P(hysical axis).
              If you want physical coordinates that are not transformed with
              build in coordinate transformation but are calculated with the
              FITS formula:
                                phys  = CRVAL + grid * CDELT

              use option F(its). Note that CRVAL must be a position INSIDE your 
              box, otherwise you will not get any labels.
              Axis labels (World or Physical positions) are plotted outside the
              box, unless you included the character I(nside box labeling). The
              characters can be in upper or lower case. Only one axis is plotted
              in one call. If you want no ticks and labels but only the axis
              itself to be plotted, give one of the axis characters B,T,R,L, but
              not a 'P' or 'W'. If coordinates in offset are wanted, include the
              character O(ffset). If rotation is detected, and physical
              coordinates are wanted then the offset mode is selected
              automatically. The offset then is in units of DELTAUNITS. If the
              start position for an offset is not given, the physical value of
              the reference pixel (from the set header) is taken if option 'P'
              is used, else, the world coordinate of the center (boxlo+boxhi)/2 is 
              used as starting point.
              If the starting point is not within the axis range, no labels
              are plotted and an error code is returned.
              If an inset is given, coordinate transformation
              (between grids and physical coordinates) is possible with option
              'P', else the option 'P' will have the same result as option 'W'.
              For the (internal) transformation of world coordinates to mm, a
              scale and origin is needed. SCALE has two values: first value is
              a scale in grids/mm in x direction, second value is a scale in
              grids/mm in y direction. ORIGMM has also two values indicating
              x,y in mm of the origin of your plot. To create more space between
              plot and axes, use MARGIN. MARGIN has two values indicating
              a margin in mm in x and y direction. This margin if often used as
              space for labels if labels has to be plotted inside the given
              box.

              For the transformation of grids to physical coordinates, a set
              must be part of the input. The information that is needed can
              be obtained by a call to GDSINP. SETIN is the name of set.
              SUBIN is the coordinate word of the subset for which an axis
              should be plotted. If you do not want or need these
              transformation, supply an empty string for SETIN. Then the
              value for SUBIN can be a dummy.


              There is a lot of flexibility in plotting the ticks and labels.
              TICKSIZE is the size of the label ticks in mm. If TICKSIZE is
              blank, a default size of 1/3 of the character height is used.
              The size of the minor ticks are 0.5 times the size major ticks.
              The routine starts writing a label at START. If on input the
              value of start is equal to blank (a floating or a double blank)
              a suitable default will be calculated. If SETIN is specified,
              the value of START can be given in some units. These
              units are compared to the units of the current axis found in the
              header. Conversion is possible if the units are compatible. A
              list of units can be found in $gip_sub/factor.c
              Major ticks have separation DELTA. If DELTA is equal to blank
              (a floating or a double blank) a suitable default will be
              calculated. If SETIN is specified, the value of DELTA can be
              given in units of DELTAUNITS. The number of minor tick
              intervals (!) is controlled by MINORS. If the value is a
              blank, a default value will be calculated. The labels can be
              formatted with AXFORMAT. AXFORMAT is a string used as format
              template. If the string is an empty string, a general format will
              be used for numerical labels and if a set was given, The hms
              format will be used for a longitude axis (if equatorial) and the
              dms format will be used for a latitude axis. The hms/dms formats
              may include a precision in seconds for example hms.ss will give
              the seconds with two digits accuracy. If no dot was included
              the routine calculates the precision itself. An uppercase
              character in the format results in fixing the corresponding
              digits, i.e. 'axformat hMS' always displays the minutes and
              seconds. For more information about numerical formats, see
              example and documentation $gip_sub/printusing.dc2.

              Other options:

              The 'O' option plots Offsets. It labels the position START
              with zero, and plots labels at separation DELTA.
              'F' is used to do all position calculations according
              to FITS with the simple transformation:

                            Xphys = CRVAL + Xgrid * CDELT

              Also the FITS transformation starts at the user given 
              position to plot its first label. The default start position
              for FITS offsets is the center of your map.
              The options 'I' and 'A' are for tIckmarks and lAbels.
              The letter is followed by one or two digits. The first digit
              is 0 if tick mark or label must NOT be plotted and unequal to
              0 if it must be plotted. The second digit is 0 if the tick mark
              or label is to be plotted outside the frame and 1 if it is
              to be plotted inside the frame. The default is I11A10 i.e.
              the tick marks are plotted inside the frame and the labels
              are plotted outside the frame.

              The 'Z' option plots labels as 10**(World/physical coordinate).
              The array AXLOGS contains the labels. The drawaxis routine
              calculates the corresponding positions and plots the labels.

              If the length of the array is 0, the default labels
              ... 0.01  0.1  1  10  100 ... are plotted. If you want more
              labels between these, specify an AXLOGS array with values
              >=1 and < 10. E.g. if AXLOGS contains 1 2 5 7, you will
              get labels:
              ... 0.01  0.02  0.05  0.07  0.1  0.2  0.5  0.7  1  2  5  7 ...
              
              The 'E' option extends the ticks until a border of the box
              is reached. With this option it is possible to plot a
              coordinate grid overlay.


Examples:     AXFORMAT examples:
              Note: The underscore character indicates a space.

    string        | number     | result      | remark
    ==============|============|=============|===============================
     +eeeeee.eeee | 43345.5436 | +4.3346e+04 | exp. format, signed conversion
     gggg.ggggg   | 34.43      | _____34.430 | field width is 10
     +ffff.ff     | 23.456     | __+23.46    | signed conversion
     -ffff        | 345.87     | 346         | left justified
     -+ffff.fff   | 34.43      | +34.430     | left justified signed conv.
     eee.eeee     | 234.43     | ________*   | field width too small
     ffff.ff      | blank      | _______b    | input was a blank
     hms          | 45         | 3h0m0s      | program determines precision
     hms.ss       | 45         | 3h0m0.00s   | 2 digits in precision
     hms.         | 45         | 3h0m0s      | 0 digits in precision
     dms          | 45         | 45o0'       | program determines precision


Note:         To be able to use the entire plot device and address positions
              in millimeter, use something like the next C code:

              mm = 2;
              nx1 = 0.0, nx2 = 1.0, ny1 = 0.0, ny2 = 1.0;
              pgsvp_c( &nx1, &nx2, &ny1, &ny2 );
              pgqvp_c( &mm, x1mm, x2mm, y1mm, y2mm );
              pgswin_c( x1mm, x2mm, y1mm, y2mm );

#<

Fortran to C interface:

@ integer function drawaxis( character,
@                            double precision,
@                            double precision,
@                            double precision,
@                            double precision,
@                            logical,
@                            character,
@                            integer,
@                            integer,
@                            double precision,
@                            character,
@                            character,
@                            integer,
@                            character,
@                            double precision,
@                            integer,
@                            double precision,
@                            character,
@                            double precision,
@                            character )

*/


fint drawaxis_c( fchar   Options,
                 double  *gridmargin,
                 double  *scale,
                 double  *origmm,
                 double  *box,
                 bool    *setexist,
                 fchar   Setin,
                 fint    *subin,
                 fint    *ldev,
                 double  *ticksize,
                 fchar   Start,
                 fchar   Delta,
                 fint    *numminors,
                 fchar   Axformat,
                 double  *axlogs,
                 fint    *axloglen,
                 double  *cgstep,
                 fchar   Axtitle,
                 double  *titleXY,
                 fchar   Title_id )
/*---------------------------------------------------------------------------*/
/* Options is a string containing one of the characters B(ottom), R(ight),   */
/* L(eft) or T(op), one or more characters of P(hysical axis), G(rid axis),  */
/* I(nside box labeling).                                                    */
/*                                                                           */
/* Axformat is a string which holds the format of the labels. The formats    */
/* that can be used are the formats recognized by 'PRINTUSING' for numbers   */
/* and the formats hms.ss, dms.ss                                            */
/* If hms is selected, the labels are plotted in hour, min,                  */
/* sec. The wanted precision in seconds is given by the number of charac-    */
/* ters after the dot in the format string.                                  */
/* If 'Start', or 'Delta' are empty strings, or 'numminors' or 'ticksize'    */
/* have negative values,  default values are calculated.                     */
/* For an offset axis, the offset is given in units of 'Deltaunits'. If no   */
/* units were defined, the units from the header are used.                   */
/* For the current axis, default title, position and angle are created and   */
/* returned.                                                                 */
/*---------------------------------------------------------------------------*/
#ifdef gdggddgd
#define Xgrid2mm( a )  (box[0] < box[2] ?\
                       ((double) (origmm[0] + ((a)-box[0])/scale[0])):\
                       ((double) (origmm[0] + (box[0]-(a))/scale[0])))

#define Ygrid2mm( a )  (box[1] < box[3] ?\
                       ((double) (origmm[1] + ((a)-box[1])/scale[1])):\
                       ((double) (origmm[1] + (box[1]-(a))/scale[1])))
#endif

#define Xgrid2mm( a ) ((double) (origmm[0] + ((a)-box[0])/scale[0]))
#define Ygrid2mm( a ) ((double) (origmm[1] + ((a)-box[1])/scale[1]))

#define MAXSTRLEN   128
#define TITLELEN    256
#define MAXLEN      20
#define MAXAXES     10
#define anyoutD( dev, anytxt ) {\
                         fint fdev;\
                         char ltxt[MAXSTRLEN];\
                         fdev = (fint) (dev);\
                         strcpy(ltxt, "<DRAWAXIS> ");\
                         strcat(ltxt, anytxt);\
                         anyout_c( &fdev, tofchar(ltxt) );\
                       }

#define ERR_NOBOX    -1
#define ERR_NOAXIS   -2
#define ERR_NOHMS    -3
#define ERR_NOPROCO  -4
#define ERR_OUTSIDE  -5
#define ERR_NOLEN    -6
#define ERR_NOSUBSET -7
#define ERR_NOSTART  -8
#define ERR_NODELT   -9
#define ERR_NOLOGS   -10


{
   char     message[MAXSTRLEN+1];         /* All purpose character buffer */
   char     status[MAXLEN+1];             /* Copy the fchar Options to this array in uppercase */
   char     axisformat[MAXSTRLEN+1];      /* Copy of input label format */
   char     axtitle[TITLELEN+1];          /* Create a default title special to this axis */
   char     axname[MAXLEN+1];             /* Convert Ctype to a name */

   fchar    Ctype[2], Cunit[2];           /* Type, unit of primary axis */
   fchar    Dtype[2], Dunit[2];           /* Same for secondary axis */
   fchar    Labelstr;                     /* The label value converted to a string */
   fchar    Deltaunits;

   fint     mm_mode = 2;                  /* Flag for the mm mode */
   fint     maxlen;                       /* Maximum num. characters in Options */
   fint     r1;                           /* Results of user input routines */
   fint     nminors;                      /* Number of minor tick INTERVALS */
   fint     skysys[2];                    /* Info sky  system */
   fint     prosys[2];                    /* Info projection system */
   fint     velsys[2];                    /* Info velocity system */
   fint     axistype[2];                  /* Spatial or other types */
   fint     setlevel = 0;                 /* Get axis parameters (CDELT etc.) from top level */
   fint     procomode;                    /* I/O mode for 'PROCO' coordinate transf. */
   fint     colev[2];                     /* Primary or secondary axis used by 'COTRANS' (1/2) */
   fint     first;                        /* Is first label plotted? */
   fint     axnum[2];                     /* Axis permutation array (for 2 subset axes) */
   fint     coordword;                    /* Coordinate word for 1 dim structures */
   fint     grid2phys = 1;                /* 'COTRANS' converts from grids to physics now */
   fint     phys2grid = 0;

   int      logdev;                       /* Output status */
   int      i;                            /* Counter */
   int      minorcount;                   /* Count the minor tick marks, start from 'startph' */
   int      sign = 1;                     /* Reverse direction of plotting labels */
   int      slen;                         /* Length of a string */
   int      axn;                          /* Array index number of current axis */
   int      subdim = 0;                   /* Dimension of input subset if set is available */
   int      al = 0;                       /* axlogs array counter */
   int      titlenr= 0;                   /* Title id as an integer BRTL==0123 */

   double   xinc, yinc;                   /* Increments in x- or y direction */
   double   startph;                      /* Copy of input start position */
   double   maxph = 0.0;                  /* Highest axis value */
   double   deltaph;                      /* Copy of step size. Can be converted in program */
   double   deltastored;                  /* Copy of step size. Is NOT converted in program */
   double   Xin, Yin, Xout, Yout;         /* Running label positions */
   double   Xinstart, Yinstart;           /* Store start positions of Xin, Yin */
   double   crval[2], cdelt[2];           /* Coordinate parameters primary axis */
   double   drval[2], ddelt[2];           /* Coordinate parameters secondary axis */
   double   crpix[2];
   double   procoXoff = 0.0;              /* Offsets for 'proco' */
   double   procoYoff = 0.0;
   double   offset;                       /* The offset from 0 if an offset axis is wanted */
   double   labelval;                     /* The value to be plotted as a label */
   double   stoX, stoY;                   /* Variable for storage */
   double   cfactD;                       /* Conversion factors for alternative units */
   double   skyangle;                     /* Rotation of map found in header */
   double   logfact = 0.0;                /* Factor for logarithmic conversion */

   double   xlabmm, ylabmm;               /* Position in x and y for label */
   double   Xofftitmm, Yofftitmm;         /* Offsets in x and y for titles */
   double   lablenmm = 0.0;               /* Graphics length of string */
   double   tickmm;                       /* Size of the ticks in mm */
   double   digit_h, digit_w;             /* The value of the current char. height */
   double   charheight;                   /* Scaled character height */
   double   devsize[2];                   /* Char. height is 1/40 of min. of dev. size in x,y */
   double   xoffsmm, yoffsmm;             /* Offset in the labels in mm (e.g.gridmargin) */
   double   xtick, ytick;                 /* Size of the tick in mm (tickmm) in x and y */
   double   Xmm, Ymm;                     /* Label positions in mm */
   double   xg, yg;                       /* Grid positions */
   double   xmm, ymm;                     /* Positions in mm */
   double   x1mm, x2mm, y1mm, y2mm;       /* Box positions in mm */
   double   dblank;                       /* double precision blank */

   float    fjust;                        /* Justification of labels along the axes */

   bool     axisT, axisB, axisL, axisR;   /* Which axis is in use? */
   bool     drawgrids;                    /* If 'W' in 'status', then draw the grids */
   bool     spatialmap;                   /* Both axes are spatial */
   bool     extendticks;                  /* Draw grid overlay? */   
   bool     physical;                     /* Necessary coord. par. could be found in header */
   bool     rotated[2];                   /* Has this axis a rotation angle? */
   bool     fitsway;                      /* 'COTRANS' could not be used, use FITS instead */
   bool     hmsform, dmsform, numform;    /* Kind of formats for labels */
   bool     defform, offsform;            /* Other possibilities */
   bool     inside;                       /* Is label position within axis range? */
   bool     nostart, nodelta;             /* No default values as input */
   bool     convS, convD;                 /* Are there units to convert? */
   bool     inset;                        /* Is a set given? */
   bool     logarit;                      /* Logarithmic axis? */
   bool     ticksinside;                  /* Tick marks in/outside frame? */
   bool     labelsinside;                 /* Labels in/outside frame? */
   bool     plotticks;                    /* Plot any tick marks? */
   bool     plotlabels;                   /* Plot any labels? */


   logdev = *ldev;
   /*---------------------------------------------------*/
   /* Is a set involved?                                */
   /*---------------------------------------------------*/   
   inset = *setexist;

   if (inset)
   {
      anyoutD( 16, "Debug: An input set is detected" );         
      subdim = gdsc_ndims_c( Setin, subin );
      if ((subdim != 1) && (subdim != 2))
      {
         /* The subset must be an axis or a plane */
         anyoutD( logdev, "Dimension of subset not 1 or 2!");
         return(ERR_NOSUBSET);
      }
   }

   /*---------------------------------------------------*/
   /* Check the box, if not correct, return immediately */
   /*---------------------------------------------------*/
   if (box[0] == box[2]) 
   {
      anyoutD( logdev, "blo == bhi in x-dir!" );
      return(ERR_NOBOX);
   }
   if (box[1] == box[3])
   {
      anyoutD( logdev, "blo == bhi in y-dir!" ); 
      return(ERR_NOBOX);
   }
   
   /*-------------------------------------------------------*/
   /* Copy the options to 'status' in upper case. Number of */
   /* options can be 0                                      */
   /*-------------------------------------------------------*/
   maxlen = nelc_c(Options);   
   if (maxlen == 0)
   {
      anyoutD( logdev, "Axis command needs parameters" );
      return(ERR_NOAXIS);
   }
   if (maxlen > MAXLEN)
      maxlen = MAXLEN;
   for (i = 0; i < maxlen; i++)
      status[i] = toupper(Options.a[i]);

   status[i] = '\0';
   if (strlen(status) == 0)
   {
      anyoutD( logdev, "Axis command needs parameters" );
      return(ERR_NOAXIS);
   }

   /*------------------------------------------------------------------------*/
   /* Set the blank values for floats and doubles and (pre)set some variables*/
   /*------------------------------------------------------------------------*/
   setdblank_c( &dblank );   
   xmm        = ymm        = 0.0;
   xinc       = yinc       = 0.0;
   xlabmm     = ylabmm     = 0.0;
   xoffsmm    = yoffsmm    = 0.0;
   Xofftitmm  = Yofftitmm  = 0.0;
   xtick      = ytick      = 0.0;
   colev[0]   = colev[1]   = 1;
   extendticks= NO;
   spatialmap = NO;
   physical   = NO;
   fitsway    = NO;
   drawgrids  = NO;
   convD      = NO;
   convS      = NO;
   fjust      = 0.0;
   slen       = 0;                         /* Length of created default title */
   axtitle[0] = '\0';                      /* Initialize the default title */

   /*----------------------------------------------------------------*/
   /* Determine which axis is in use and return if no axis was found */
   /*----------------------------------------------------------------*/
   axisT = axisB = axisL = axisR = NO;
   if (strchr(status, 'B') != NULL)                /* Bottom axis */
   {
      titleXY[0] = titleXY[1] = dblank;
      axisB= YES;
      axn = 0;
   }
   else if (strchr(status, 'R') != NULL)           /* Right axis */
   {
      titleXY[2] = titleXY[3] = dblank;      
      axisR = YES;
      axn = 1;
   }
   else if (strchr(status, 'T') != NULL)           /* Top axis */
   {
      titleXY[4] = titleXY[5] = dblank;      
      axisT = YES;
      axn = 0;
   }
   else if (strchr(status, 'L') != NULL)           /* Left axis */
   {
      titleXY[6] = titleXY[7] = dblank;      
      axisL = YES;
      axn = 1;
   }
   else
   {
      anyoutD( logdev, "Axis T(op), B(ottom), L(eft) or R(ight)" );
      return(ERR_NOAXIS);
   }

   pgbbuf_c();
   /*-------------------------------------------------------*/
   /* Move to starting point of this axis and draw the line */
   /*-------------------------------------------------------*/
   if (axisB)
   {
      xmm = Xgrid2mm(box[0]) - gridmargin[0];
      ymm = Ygrid2mm(box[1]) - gridmargin[1];
   }
   else if (axisR)
   {
      xmm = Xgrid2mm(box[2]) + gridmargin[0];
      ymm = Ygrid2mm(box[1]) - gridmargin[1];
   }
   else if (axisT)
   {
      xmm = Xgrid2mm(box[0]) - gridmargin[0];
      ymm = Ygrid2mm(box[3]) + gridmargin[1];
   }
   else if (axisL)
   {
      xmm = Xgrid2mm(box[0]) - gridmargin[0];
      ymm = Ygrid2mm(box[1]) - gridmargin[1];
   }
   PLMOVE( xmm, ymm );

   /* Draw to end point */
   if (axisB)
   {
      xmm = Xgrid2mm(box[2]) + gridmargin[0];
      ymm = Ygrid2mm(box[1]) - gridmargin[1];
   }
   else if (axisR)
   {
      xmm = Xgrid2mm(box[2]) + gridmargin[0];
      ymm = Ygrid2mm(box[3]) + gridmargin[1];
   }
   else if (axisT)
   {
      xmm = Xgrid2mm(box[2]) + gridmargin[0];
      ymm = Ygrid2mm(box[3]) + gridmargin[1];
   }
   else if (axisL)
   {
      xmm = Xgrid2mm(box[0]) - gridmargin[0];
      ymm = Ygrid2mm(box[3]) + gridmargin[1];
   }
   PLDRAW( xmm, ymm );


   /*------------------------------------------*/
   /* What are the actions to be taken? Is     */
   /* a set is involved, the world coordinates */
   /* are the grid coordinates.                */
   /*------------------------------------------*/
   if (strchr(status, 'W') != NULL)
   {
      drawgrids = YES;
      anyoutD( 16, "Debug: Drawing grids" );
   }
   /*-------------------------------------------------------------*/
   /* If there is no set specified, physical mode and grid mode   */
   /* are the same.                                               */
   /*-------------------------------------------------------------*/   
   if (!drawgrids && (strchr(status, 'P') != NULL))
   {
      physical = YES;
      anyoutD( 16, "Debug: Set is known... drawing physical coordinates" );
   }
   if (physical && !inset)               /* No set, switch to grid mode */
   {
      physical  = NO;
      drawgrids = YES;
      anyoutD( 16, "Debug: No set, draw grids only" );
   }
   /* User wants offsets, or specifies coordinate transforms in FITS way */
   if (physical && ((strchr(status, 'F') != NULL) || (strchr(status, 'O') != NULL)) )
   {
      fitsway = YES;
      anyoutD( 16, "Debug: User selected FITS transformations" );
   }
   logarit = (strchr(status, 'Z') != NULL);
   extendticks = (strchr(status, 'E') != NULL);

   /*-------------------------------------------------------*/
   /* Define the positions and offsets etc. for the labels  */
   /* Distinguish 'inside' and 'outside' labels in 'status' */
   /* First get the character width and height in mm.       */
   /*-------------------------------------------------------*/
   {
      float x1, x2, y1, y2;
      pgqvp_c( &mm_mode, &x1, &x2, &y1, &y2 );
      x1mm = (double) x1;  x2mm = (double) x2;
      y1mm = (double) y1;  y2mm = (double) y2;
   }
   devsize[0] = (float) fabs(x2mm - x1mm);
   devsize[1] = (float) fabs(y2mm - y1mm);
   {
      float ch;
      pgqch_c( &ch );
      charheight = (double) ch;
   }
   digit_h = charheight * MYMIN(devsize[1], devsize[0]) / 40.0;
   digit_w = 0.75 * digit_h;

   if ((*ticksize) < 0.0)
   {
      /* Default tick size is 1/3 of character height */
      tickmm = digit_h / 3.0;
   }
   else
      tickmm = fabs( *ticksize );


   {
      /* Get arguments for 'I' and 'A' options (tIcks & lAbels) */
      char   *p;
      ticksinside = YES;
      plotticks = YES;
      p = strchr(status, 'I');
      if (p)
      {
         /* Look for digits as options for 'I' */
         int i = 0;
         while (isdigit(p[++i]))
         {
            if (i == 1 && p[i] == '0' )
               plotticks = NO;
            if (i == 2 && p[i] == '0' )
               ticksinside = NO;
         }
      }
      labelsinside = NO;
      plotlabels = YES;
      p = strchr(status, 'A');
      if (p)
      {
         /* Look for digits as options for 'A' */
         int i = 0;
         while (isdigit(p[++i]))
         {
            if (i == 1 && p[i] == '0' )
               plotlabels = NO;
            if (i == 2 && p[i] != '0' )
               labelsinside = YES;
         }
      }
   }


   if (axisB)
   {
      Title_id.a[0] = 'B';
      titlenr    =  0;
      fjust      =  0.5;
      xoffsmm    =  0.0;            
      yoffsmm    = -gridmargin[1];
      Xofftitmm  =  0.0;            
      Yofftitmm  =  0.0;
      /* Default tick and label positions ticks inside, labels outside: */
      xtick   =  0.0;
      if (ticksinside)
         ytick =  tickmm;
      else
         ytick = -tickmm;
      xlabmm = 0.0;
      if (labelsinside)
         if (ticksinside)
            ylabmm =  tickmm + TITLEOFFSET;
         else
            ylabmm = TITLEOFFSET;
      else
         if (ticksinside)
            ylabmm = -digit_h - TITLEOFFSET;
         else
            ylabmm = -tickmm - digit_h - TITLEOFFSET;
   }
   else if (axisR)
   {
      Title_id.a[0] = 'R';
      titlenr    =  1;
      xoffsmm    =  gridmargin[0];  
      yoffsmm    =  0.0;
      Xofftitmm  =  digit_w;        
      Yofftitmm  =  0.0;
      fjust      =  0.0;
      ytick      =  0.0;
      if (labelsinside)
         fjust   =  1.0;
      if (ticksinside)
         xtick   = -tickmm;
      else
         xtick   =  tickmm;
      ylabmm  =  0.0 - digit_h/3.0;
      if (labelsinside)
         if (ticksinside)
            xlabmm = -tickmm - TITLEOFFSET;
         else
            xlabmm = - TITLEOFFSET;
      else
         if (ticksinside)
            xlabmm = + TITLEOFFSET;
         else
            xlabmm = tickmm + TITLEOFFSET;
   }
   else if (axisT)
   {
      Title_id.a[0] = 'T';
      titlenr    =  2;
      fjust      =  0.5;
      xoffsmm    =  0.0;            
      yoffsmm    =  gridmargin[1];
      Xofftitmm  =  0.0;            
      Yofftitmm  =  0.0;
      xtick      =  0.0;
      if (ticksinside)
         ytick   = -tickmm;
      else
         ytick   =  tickmm;
      xlabmm  =  0.0;
      if (labelsinside)
         if (ticksinside)
            ylabmm  = -tickmm - digit_h;
         else
            ylabmm  = -digit_h;
      else
         if (ticksinside)
            ylabmm  =  + TITLEOFFSET;
         else
            ylabmm  =  tickmm + TITLEOFFSET;
   }
   else if (axisL)
   {
      Title_id.a[0] = 'L';
      titlenr    =  3;
      xoffsmm    = -gridmargin[0];  
      yoffsmm    =  0.0;
      Xofftitmm  = -digit_w;        
      Yofftitmm  =  0.0;
      fjust      =  1.0;
      if (labelsinside)
         fjust   =  0.0;
      ytick   =  0.0;
      if (ticksinside)
         xtick   =  tickmm;
      else
         xtick   = -tickmm;
      ylabmm  =  0.0 - digit_h/3.0;
      if (labelsinside)
         if (ticksinside)
            xlabmm  =  tickmm;
         else
            xlabmm  = 0.0;
      else
         if (ticksinside)
            xlabmm  =  - TITLEOFFSET;
         else
            xlabmm  = -tickmm - TITLEOFFSET;
   }

   /*-------------------------------------------------------------*/
   /* Part of the calculation of a default position for the title */
   /* that belongs to this axis. The calculation involves the     */
   /* position along an axis including a small offset that scales */
   /* with the character height and and grid margin offset. The   */
   /* correction of the length of the labels is calculated later. */
   /*-------------------------------------------------------------*/
   if (axisB || axisT)
   {
      double  offset = TITLEOFFSET;
      xg  = (box[2] + box[0]) / 2.0;
      xmm = Xgrid2mm( xg );
      if (axisB)
         ymm = Ygrid2mm( box[1] ) - offset;
      else
         ymm = Ygrid2mm( box[3] ) + offset;
   }
   else
   {
      double  offset = TITLEOFFSET;
      yg  = (box[3] + box[1]) / 2.0;
      ymm = Ygrid2mm( yg );
      if (axisL)
         xmm = Xgrid2mm( box[0] ) - offset;
      else
         xmm = Xgrid2mm( box[2] ) + offset;
   }
   titleXY[titlenr*2]   = xmm + xoffsmm;
   titleXY[titlenr*2+1] = ymm + yoffsmm;  
  

   /*------------------------------------------------*/
   /* Nothing else to do, give title pos. and return */
   /*------------------------------------------------*/
   if (!drawgrids && !physical)
   {
      pgebuf_c();
      return(0);
   }

   if (inset)
   /*---------------------------------------------------------------------*/
   /* If physical coordinates are wanted, we need parameters that make up */
   /* the coordinate system of the input set.                             */
   /*---------------------------------------------------------------------*/
   {
      fchar    Dummy1, Dummy2;
      fint     setdim;
      fint     iax;

      fmake(Dummy1, MAXLEN);
      fmake(Dummy2, MAXLEN);
      setdim = gdsc_ndims_c( Setin, &setlevel );
      /*-----------------------------*/
      /* Where are the subset axes?  */
      /*-----------------------------*/
      for (i = 0, iax = 1; iax <= setdim; iax++)
      {
         r1 = 0;
         (void) gdsc_grid_c( Setin, &iax, subin, &r1 );
         if (r1 < 0)
            axnum[i++] = iax;             /* Undefined axis, must be a subset axis */
      }
      /*----------------------------------------------*/
      /* If the subset dimension was 1, copy the axis */
      /* number for the 'dummy' second axis.          */
      /*----------------------------------------------*/
      if (i == 1)
      {
         axnum[1] = axnum[0];
         anyoutD( 16, "Debug: Subset dimension == 1, copy dummy axis");
      }


      /*-----------------------------------------------------------*/
      /* Determine the coordinate word that belongs to this axis.  */
      /* Distinguish one and two dimensional subsets. For the 2dim */
      /* subsets, determine a coordinate word to specify the axis. */
      /*-----------------------------------------------------------*/

      if (subdim == 1)
         coordword = *subin;
      if (subdim == 2)
      {
         fint grid;
         fint axn;
         if      (axisB) grid = (int) box[1];
         else if (axisT) grid = (int) box[3];
         else if (axisL) grid = (int) box[0];
         else if (axisR) grid = (int) box[2];
         /* The new coordinate word is determined by specifying */
         /* a grid on the axis that is orthogonal to the axis   */
         /* you want. */
         if (axisB || axisT)
            axn = axnum[1];
         else
            axn = axnum[0];
         r1 = 0;
         coordword = gdsc_word_c( Setin, &axn, &grid, subin, &r1 );
      }
      /*-------------------------------------*/
      /* Get the axis characteristics. If    */
      /* the subset was 1 dim. repeat action */
      /* for the same axis. This way you can */
      /* plot the same axis at 4 positions.  */
      /*-------------------------------------*/
      procoXoff = 0.0;
      procoYoff = 0.0;
      for (i = 0; i < 2; i++)
      {
         finit( Ctype[i], MAXLEN );       /* finit instead of fmake because Ctype is an array */
         finit( Dtype[i], MAXLEN );
         finit( Cunit[i], MAXLEN );
         finit( Dunit[i], MAXLEN );

         r1 = axcoord_c( Setin, &axnum[i], Dummy1, Dummy2, &colev[i] );
         if (r1 != 0)
         {
            colev[i] = 1;
            physical = NO;
            (void) sprintf( message, "Debug: Error [%d] in axcoord routine", (int) r1 );
            anyoutD( 16, message );
         }

         r1 = 0;
         (void) sprintf( message, "CTYPE%d", axnum[i] );
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, Ctype[i], &r1 );
         if (r1 < 0)
         {
            strcpy( Ctype[i].a, "PIXELS" );
            axistype[i] = 0;
         }
         else
         {
            if (colev[i] == 1)
               axistype[i] = axtype_c( Ctype[i],
                                       Cunit[i],    /* Natural units */
                                       Dunit[i],
                                       &skysys[i],
                                       &prosys[i],
                                       &velsys[i] );
         }


         /* we do not want the 'natural units, but the real units! */
         r1 = 0;
         (void) sprintf( message, "CUNIT%d", axnum[i] );
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, Cunit[i], &r1 );
         if (r1 < 0) strcpy( Cunit[i].a, "?" );


         (void) sprintf( message, "CDELT%d", axnum[i] );
         r1 = 0;
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &cdelt[i], &r1 );
         if (r1 < 0)
         {
            anyoutD( logdev, "No grid spacing CDELT found in header." );
            if (physical)
            {
               physical = NO;
               anyoutD( logdev, "Switched to plotting WORLD coordinates" );
            }
         }


         (void) sprintf( message, "CRVAL%d", axnum[i] );
         r1 = 0;
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crval[i], &r1 );
         if (r1 < 0)
         {
            anyoutD( logdev, "No reference value CRVAL found in header." );
            if (physical)
            {
               physical = NO;
               anyoutD( logdev, "Switched to plotting WORLD coordinates" );
            }
         }


         (void) sprintf( message, "CRPIX%d", axnum[i] );
         r1 = 0;
         gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crpix[i], &r1 );
         if (r1 < 0)
         {
            anyoutD( logdev, "No reference pixel CRPIX found in header." );
         }
         else
         {
            if (i == 0)
               procoXoff = proco_correct( crpix[0] );
            else
               procoYoff = proco_correct( crpix[1] );
         }

         if (colev[i] == 2)
         {
            (void) sprintf( message, "DDELT%d", axnum[i] );
            r1 = 0;
            gdsd_rdble_c( Setin, tofchar(message), &setlevel, &ddelt[i], &r1 );
            if (r1 < 0)
            {
               anyoutD( 16, "No grid spacing DDELT found in header." );
               if (fitsway)            /* Change, do not use secondary axis */
                  colev[i] = 1;
            }

            (void) sprintf( message, "DRVAL%d", axnum[i] );
            r1 = 0;
            gdsd_rdble_c( Setin, tofchar(message), &setlevel, &drval[i], &r1 );
            if (r1 < 0)
            {
               anyoutD( logdev, "No reference value DRVAL found in header." );
               colev[i] = 1;
            }

            r1 = 0;
            (void) sprintf( message, "DTYPE%d", axnum[i] );
            gdsd_rchar_c( Setin, tofchar(message), &setlevel, Dtype[i], &r1 );
            if (r1 < 0)
            {
               strcpy( Dtype[i].a, "?" );
               axistype[i] = 0;
            }
            else
            {
               if (colev[i] == 2)
                  axistype[i] = axtype_c( Dtype[i],
                                          Dummy1,    /* Natural units */
                                          Dummy2,
                                          &skysys[i],
                                          &prosys[i],
                                          &velsys[i] );
               else   /* colev could be changed */
                  axistype[i] = axtype_c( Ctype[i],
                                          Dummy1,    /* Natural units */
                                          Dummy2,
                                          &skysys[i],
                                          &prosys[i],
                                          &velsys[i] );
            }
            
            r1 = 0;
            (void) sprintf( message, "DUNIT%d", axnum[i] );
            gdsd_rchar_c( Setin, tofchar(message), &setlevel, Dunit[i], &r1 );
            if (r1 < 0)
               strcpy( Dunit[i].a, "?" );               
         }
      } /* End for both axes */

      /*---------------------------------------------------------------*/
      /* Special care for rotation. The rotation of a map is given by  */
      /* the CROTA belonging to the latitude axis. But this axis could */
      /* be a hidden axis. Function "skyrot" will find out.            */
      /*---------------------------------------------------------------*/
      skyangle = 0.0;      
      r1 = skyrot_c( Setin, &skyangle );
      if (r1)
      {
         skyangle = 0.0;
         if (r1 == -1)
         {
            (void) sprintf( message, "Debug: No sky coordinates found in set" );
            anyoutD( 16, message );            
         }
      }
      else
      {
         (void) sprintf( message, "Debug: Found sky rotation angle %f", 
                         skyangle );
         anyoutD( 16, message );
      }

      for (i = 0; i < 2; i++)
      {
         rotated[i] = NO;                   /* Initialize */
         if (skyangle != 0.0 && ((axistype[i] == 1) || (axistype[i] == 2)))
         {
             rotated[i] = YES;
             (void) sprintf( message, "Debug: Spatial axis [%d] is rotated", i);
             anyoutD( 16, message );
         }
      }
   }


   /*---------------------------------------------------*/
   /* Determine the format of the numbers in the labels */
   /*---------------------------------------------------*/
   fmake( Labelstr, 80 );
   hmsform   = NO;          /* Labels in hours, minuts, seconds */
   dmsform   = NO;          /* Labels in degrees, (arc)minuts, (arc)seconds */
   numform   = NO;          /* Some numerical format */
   defform   = NO;          /* Use the general format %g */
   offsform  = NO;          /* Label the offsets only */


   if (strchr(status, 'O') != NULL)
   {
      offsform = YES;
      anyoutD( 16, "Debug: Offset wanted" );
   }
   slen = MYMIN( nelc_c(Axformat), MAXSTRLEN );
   if (slen)
   {
      for (i = 0; i < slen; i++)
      {
         /* Copy the format string (do not alter the original) and */
         /* convert to lower case. */
         axisformat[i] = tolower(Axformat.a[i]);
      }
      axisformat[i] = '\0';
      if (strstr(axisformat, "hms") != NULL)
      {
         hmsform = YES;
         if ((axistype[axn] != 1) || (skysys[axn] != 1))
         {
            /* No hms format for others than equatorial longitude */
            anyoutD( logdev, "No HMS format possible for this axis" );
            pgebuf_c();
            return(ERR_NOHMS);
         }
      }
      if (strstr(axisformat, "dms") != NULL)
         dmsform = YES;
      if (!hmsform && !dmsform)
         numform = YES;
   }
   else
   {
      anyoutD( 16, "Debug: No format given. Use a general format. If option" );
      anyoutD( 16, "Debug: includes 'P', and the axis is spatial and not" );
      anyoutD( 16, "Debug: rotated, the default format is hms/dms.");
      /*-----------------------------------------------------------*/
      /* No format given, use general format. If option includes   */
      /* 'P', and the axis is spatial and not rotated, the default */
      /* format is hms/dms.                                        */
      /*-----------------------------------------------------------*/
      defform = YES;
      if (physical)
      {
         if (axistype[axn] == 1)                    /* Spatial axis longitude. */
         {
            anyoutD( 16, "Debug: Spatial axis longitude" );
            if (!rotated[axn] && !offsform)
            {
               anyoutD( 16, "Debug: Not rotated, no offsets" );
               if (skysys[axn] == 1)                /* Sky system is equatorial */
               {
                  anyoutD( 16, "Debug: Equatorial system --> use HMS" );
                  hmsform = YES;
               } else {
                  anyoutD( 16, "Debug: Not an Equatorial system --> use DMS" );
                  dmsform = YES;
               }
               defform = NO;
            }
            else
               offsform = YES;
         }
         if (axistype[axn] == 2)                    /* Spatial axis latitude. */
         {
            anyoutD( 16, "Debug: Spatial axis latitude" );
            if (!rotated[axn] && !offsform)
            {
               dmsform = YES;
               defform = NO;
            }
            else
               offsform = YES;
         }
      } /* end if physical */
   } /* end no axformat */

   if (offsform)
      fitsway = YES;
      
   /*------------------------------------------------------------*/
   /* Are values specified for the start position and the delta? */
   /* If so, translate the string to a number, else calculate a  */
   /* default value.                                             */
   /*------------------------------------------------------------*/

   nostart = (logarit || nelc_c(Start) == 0 || Start.a[0] == 0);
   if (!nostart)     /* i.e. a start value was specified */
   {
      fint   npos;
      fint   err;
      fint   one = 1;

      if (physical)
      {
         fint   maxpos = 1;
         double startgr;
         fint   slen;
         int    k = 0;

         /*-------------------------------------------------------------------*/
         /* If first non blank character is a 'U', then these are physical    */
         /* units without postfix, so the number has the right units already. */
         /*-------------------------------------------------------------------*/
         slen = nelc_c(Start);
         anyoutD( 16, "Debug: A start pos. is given" );
         while ((k < slen) && (Start.a[k] == ' ')) k++;  /* Skip spaces */
         if (toupper(Start.a[k]) == 'U')
         {
            if ((k+2) < slen)
            {

               int  j;
               k += 2;
               for (j = 0; k < slen; j++,k++)
                  message[j] = Start.a[k];
               message[j] = '\0';
               r1 = dcddble_c( tofchar(message), &startph, &one, &err );
               if (r1 != 1)
               {
                  anyoutD( logdev, "Error converting start position" );
                  pgebuf_c();
                  return(ERR_NOSTART);
               }
               else
               {
                  (void) sprintf( message, "Debug: Start position= %f", startph );
                  anyoutD( 16, message );
               }
            }
            else
            {
               anyoutD( logdev, "Error converting start position" );
               pgebuf_c();
               return(ERR_NOSTART);
            }
         }
         else
         /*------------------------------------------------------------------*/
         /* The position did not start with a 'U' so try to convert it with  */
         /* DCDPOS. The result however is a grid that has to be converted to */
         /* a physical coordinate. The coordinate word for the subset in     */
         /* DCDPOS is always one dimensional.                                */
         /*------------------------------------------------------------------*/
         {
            npos = dcdpos_c( Setin, &coordword, Start, &startgr, &maxpos );
            if (npos < 0)
            {
               poserr( message, npos );
               anyoutD( logdev, message );
               pgebuf_c();
               return(ERR_NOSTART);
            }
            else
            /*----------------------------------------------*/
            /* Transform the grids to physical coordinates. */
            /*----------------------------------------------*/
            {
               double   dumphys[MAXAXES];
               int      indx;
               r1 = cotrans_c( Setin, &coordword, &startgr, dumphys, &grid2phys );
               if (r1 == 0)
               {
                  anyoutD( 16, "Debug: Grids to phys. coords. 'cotrans way'" );
                  if (axisB || axisT)
                     indx = axnum[0]-1;
                  else
                     indx = axnum[1]-1;
                  startph = dumphys[indx];
               }
               else
               {
                  cotranserr( message, r1 );
                  anyoutD( logdev, message );
                  anyoutD( 16, "Debug: Grids to phys. coords. 'FITS way'" );
                  /* FITS way */
                  if (axisB || axisT)
                     startph = crval[0] + startgr * cdelt[0];
                  else
                     startph = crval[1] + startgr * cdelt[1];
               }
            }
         }
      }
      else
      /*---------------------------------------------------*/
      /* Not a physical position, it is a world coordinate */
      /*---------------------------------------------------*/
      {
         npos = dcddble_c( Start, &startph, &one, &err );
         if ((npos != 1) || (err < 0))
         {
            if (err < 0)
            {
               dcderr( message, err );
               anyoutD( logdev, message );
            }
            pgebuf_c();
            return(ERR_NOSTART);
         }
      }
   }


   fmake( Deltaunits, 30 );
   nodelta = (logarit || nelc_c(Delta) == 0 || Delta.a[0] == 0);
   if (!nodelta)
   {
      /* A delta was given. This can be a number and a unit. */
      int    len;
      char   buf[120];
      fint   one = 1;
      fint   err;
      int    k, m;

      len = nelc_c(Delta);
      k = 0;
      while( (k < len) && (Delta.a[k] == ' ') )
         k++;   /* Skip spaces */
      while( (k < len) && (Delta.a[k] != ' ') && (Delta.a[k] != ',') )
      {
         buf[k] = Delta.a[k];
         k++;
      }
      buf[k] = '\0';
      r1 = dcddble_c( tofchar(buf), &deltaph, &one, &err );
      if ((r1 != 1) || (err < 0))
      {
         if (err < 0)
         {
            dcderr( message, err );
            anyoutD( logdev, message );
         }
         pgebuf_c();
         return(ERR_NODELT);
      }
      m = 0;
      while( (k < len) && ((Delta.a[k] == ' ') || (Delta.a[k] == ',')) )
         k++;
      while( (k < len) &&  (Delta.a[k] != ' ') && (Delta.a[k] != ',') )
      {
         Deltaunits.a[m] = Delta.a[k];
         m++;
         k++;
      }
      Deltaunits.l = m;
   }



   /*------------------------------------------------------------*/
   /* Value equals blank, start calculating a reasonable default */
   /* for delta or start.                                        */
   /*------------------------------------------------------------*/
   if (nostart || nodelta)
   {
      double   Xs, Ys, Xe, Ye;                 /* The physical coordinates */
      double   xs, ys, xe, ye;                 /* The grids */
      double   x1, x2;                         /* Physical coordinates of begin and end */
      double   delta, nsteps;                  /* Determine default distance between majors */
      int      sign = 1;                       /* Sign of delta */
      int      ndigit;                         /* To bring delta in range 1..10 */
      int      k;                              /* Help variable */

      Xs = Ys = Xe = Ye = 0.0;
      xs = ys = xe = ye = 0.0;
      if (axisB)
      {
         xs = box[0];          ys = box[1];
         xe = box[2];          ye = ys;
      }
      else if (axisR)
      {
         xs = box[2]; ys = box[1];
         xe = xs;              ye = box[3];
      }
      else if (axisT)
      {
         xs = box[0];          ys = box[3];
         xe = box[2];          ye = ys;
      }
      else if (axisL)
      {
         xs = box[0];          ys = box[1];
         xe = xs;              ye = box[3];
      }
      if (physical)
      {
         double   dumphys[MAXAXES], dumgrid[2];         /* Dummy I/O arrays for 'COTRANS' */
         if (!fitsway)
         {
            /* cotrans seems to change the values of dumphys, so initialize in the loop */
            dumgrid[0] = xs;
            dumgrid[1] = ys;
            if ((subdim == 1) && (axisL || axisR))
               dumgrid[0] = dumgrid[1];
            r1 = cotrans_c( Setin, subin, dumgrid, dumphys, &grid2phys );
            if (r1 == 0)
            {
               Xs = dumphys[axnum[0]-1];
               Ys = dumphys[axnum[1]-1];
               dumgrid[0] = xe;
               dumgrid[1] = ye;
               if ((subdim == 1) && (axisL || axisR))
                  dumgrid[0] = dumgrid[1];
               r1 = cotrans_c( Setin, subin, dumgrid, dumphys, &grid2phys );
               if (r1 == 0)
               {
                  Xe = dumphys[axnum[0]-1];
                  Ye = dumphys[axnum[1]-1];
               }
               else
                  fitsway = YES;
            }
            else
               fitsway = YES;
         }
         if (fitsway)  /* Do not change in else here, because 'fitsway' could have changed */
         {
            if (axisB || axisT)
            {
               if (colev[axn] == 1)
               {
                  Xs = crval[axn] + xs * cdelt[axn];
                  Xe = crval[axn] + xe * cdelt[axn];
               }
               if (colev[axn] == 2)
               {
                  Xs = drval[axn] + xs * ddelt[axn];
                  Xe = drval[axn] + xe * ddelt[axn];
               }
            }
            else
            {
               if (colev[axn] == 1)
               {
                  Ys = crval[axn] + ys * cdelt[axn];
                  Ye = crval[axn] + ye * cdelt[axn];
               }
               if (colev[axn] == 2)
               {
                  Ys = drval[axn] + ys * ddelt[axn];
                  Ye = drval[axn] + ye * ddelt[axn];
               }
            }
         }
      }
      else    /* Only the grids */
      {   
         Xe = xe; Xs = xs;
         Ye = ye; Ys = ys;
      }

      /*-------------------------------------------------------*/
      /* Now calculate a default value for 'start' and 'delta' */
      /* with the converted Xs, Ys, Xe, Ye.                    */
      /*-------------------------------------------------------*/
      if (axisB || axisT)
      {
         x1 = Xs; x2 = Xe;
      }
      else
      {
         x1 = Ys; x2 = Ye;
      }
      delta = (x2 - x1);
      if (delta == 0.0)
      {
         anyoutD( logdev, "Axis has no length" );
         pgebuf_c();
         return(ERR_NOLEN);
      }
      if (delta < 0.0)
         sign = -1;
      delta = fabs(delta);
      if (hmsform)
         delta *= 240.0;       /* 'time' and 'spatial' delta's */
      if (dmsform)
         delta *= 3600.0;
      /*-----------------------------------------------------------------*/
      /* Multiply or divide 'delta' by 10.0 until it is a number between */
      /* 1 and 10. Take the integer part of that number. For each number */
      /* there is a pre defined number of steps. With this number a new  */
      /* delta is calculated.                                            */
      /*-----------------------------------------------------------------*/
      if (delta > 10.0)
         for (ndigit = 0; delta > 10.0; delta /= 10.0, ndigit++);
      else
         for (ndigit = 0; delta <= 1.0;  delta *= 10.0, ndigit--);
      delta = floor( delta );
      k = (int) delta;
      if        ((k % 7) == 0) {
         nsteps = 7.0;
      } else if ((k % 4) == 0) {
         nsteps = 4.0;
      } else if ((k % 5) == 0) {
         nsteps = 5.0;
      } else if ((k % 3) == 0) {
         nsteps = 3.0;
      } else if ((k % 2) == 0) {
         nsteps = 2.0;
      }
      else
         nsteps = 2.0;
      delta *= pow(10.0, (double)ndigit) / ((double)sign*nsteps);
      delta  = fabs(delta);
      if (hmsform)
      /*-----------------------------------------------------------*/
      /* The value of 'delta' was multiplied by 240.0 to calculate */
      /* in time seconds. Now transform to old value but before    */
      /* transforming, try to find a 'nice' number for 'delta'     */
      /*-----------------------------------------------------------*/
      {
         int    j;
         int    m = 0;
         double nicenumber[] = {2400.0, 240.0, 60.0};
         do
         {
            j = (int) (delta/nicenumber[m]);
            if (fabs(j) >= 1.0) 
               delta = (double) j * nicenumber[m];
            m++;
         }
         while ((fabs(j) < 1.0) && (m < 3));
         delta /= 240.0;
      }

      if (dmsform)
      {
         int    j;
         int    m = 0;
         double nicenumber[] = {36000.0, 3600.0, 600.0, 60.0};
         do
         {
            j = (int) (delta/nicenumber[m]);
            if (fabs(j) >= 1.0) delta = (double) j * nicenumber[m];
            m++;
         }
         while ((fabs(j) < 1.0) && (m < 4));
         delta /= 3600.0;
      }

      if (nostart)
      {
         /*-----------------------------------------------------------*/
         /* A map could be rotated and we must plot offsets.          */
         /* There are two situations. If the map is not spatial       */
         /* then the FITS way is used to calculate coordinates        */ 
         /* and the delta's are constants wherever you are in the     */
         /* map. If the map is spatial, we have 'proco' to calculate  */
         /* the right positions for constant delta's. These           */
         /* transformations are aware of the projection center        */
         /* and one could take the projection center as offset center */
         /* However sometimes this PC is outside the box and          */
         /* nothing will be plotted. If we choose the grid center     */
         /* then there are always labels to plot and the no harm      */
         /* is done to the projections ==> Default is grid center.    */
         /*-----------------------------------------------------------*/
         if (offsform)
         {
            /* Just the middle of the box as a default for the start position */
            startph = (x1 + x2) / 2.0;
         }
         else
         {
            if (logarit)
            {
               startph  = MYMIN( x1, x2 );
               maxph    = MYMAX( x1, x2 );
            }
            else
            {
               startph = floor( ((x1+x2)/2.0) / delta ) * delta;
            }
         }
         (void) sprintf( message, "START=%g", startph );
         anyoutD( 16, message );         
      }

      if (nodelta)
      {
         deltaph = delta;
         (void) sprintf( message, "DELTA=%g", deltaph );
         anyoutD( 16, message );
      }
   }

   /*--------------------------------------------------------------*/
   /* Now a delta must be known or calculated. Store this value in */
   /* original units before converting.                            */
   /*--------------------------------------------------------------*/
   deltastored = deltaph;

   if ((*numminors) < 0)
   {
      nminors = getminors( deltaph, dmsform, hmsform );
   }
   else
      nminors = ABS(*numminors);


   /*-------------------------------------------------------------*/
   /* If physical coordinates are wanted, it is possible that the */
   /* values of the starting position and/or delta are in units   */
   /* that have to be converted first to the units of the axis.   */
   /*-------------------------------------------------------------*/
   if (physical)
   {
      convD = nelc_c(Deltaunits);                     /* Conversion of delta units? */
      if (convD)
      {
         fchar    Axunits;
         fmake( Axunits, MAXLEN );

         /* Convert units to upper case */
         for (i = 0; i < (int) convD; i++)
            Deltaunits.a[i] = toupper(Deltaunits.a[i]);

         r1 = 0;
         /* There are units defined, try to convert */
         if (colev[axn] == 1)
            Axunits.l = sprintf( Axunits.a, "%.*s", nelc_c(Cunit[axn]), Cunit[axn].a );
         else
            Axunits.l = sprintf( Axunits.a, "%.*s", nelc_c(Dunit[axn]), Dunit[axn].a );
         if (convD)
         {
            r1 = factor_c( Deltaunits, Axunits, &cfactD );
            if (r1 == 0)
               deltaph *= cfactD;
            else
            {
               pgebuf_c();
               return(-r1);  /* error numbers can be 51,52,53 & 54 */
            }
         }
      } /* End of conversion of delta */
   } /* End if physical */


   if (logarit)
   {
      al = 0;  /* Initialize a counter for axlogs array */
      /* Function floor: floor(1.02)=1.0,  floor(-1.02)=-2.0 */
      logfact = pow( 10.0, floor(startph) );
      startph = log( logfact );
      /*-----------------------------------------------------------*/
      /* 'logfact' is a variable that sets the logarithmic factor  */
      /* of the interval where 'startph' resides. The start value  */
      /* is adjusted so that it is always smaller than the smallest*/
      /* axis value. In a loop the 'logfact' variable is multiplied*/
      /* with elements from the 'axlogs' array. If the last element*/
      /* of this array is used, a counter is reset and the first   */
      /* element of the array is used again, but then 'logfact' is */
      /* multiplied by 10. The loop ends when the log(labelvalue)  */
      /* exceeds 'maxph'.                                          */
      /*-----------------------------------------------------------*/
      if (*axloglen != 0)
         sortda_c( axlogs, axloglen );   /* Sort the array in ascending order */
   }


   /*-----------------------------------------------------------------*/
   /* Distinguish spatial axis pair (converted with proco) and a      */
   /* non-spatial pair. Proco needs a physical coordinate and a grid, */
   /* cotrans needs two physical coordinates (for this we use stoX    */
   /* and stoY). If only world coordinates (grids) have to be plotted,*/
   /* do not transform to grids.                                      */
   /*-----------------------------------------------------------------*/
   stoX = 0.0; stoY = 0.0;
   Xin = Yin = 0.0;                           /* Initialize */
   if (axisB || axisT)
   {
      procomode = 2;
      Xin  = startph;
      xinc = deltaph;
      if (nminors)
         xinc /= (double) nminors;
      yinc = 0.0;
      if (axisB)
         Yin = box[1];
      else
         Yin = box[3];
      (void) sprintf( message, "Debug: Start: Xphys=%f, Ygrid=%f", Xin, Yin );
      anyoutD( 16, message );
      stoX = Xin;
      /* if cotrans uses secondary axis */
      if (colev[1] == 2)
         stoY = drval[1];
      else
         stoY = crval[1];
   }
   if (axisL || axisR)
   {
      procomode = 1;
      Yin  = startph;
      xinc = 0.0;
      yinc = deltaph;
      if (nminors) yinc /= (double) nminors;
      if (axisL)
         Xin = box[0];
      else
         Xin = box[2];
      (void) sprintf( message, "Debug: Start: Xgrid=%f, Yphys=%f", Xin, Yin );
      anyoutD( 16, message );
      stoY = Yin;
      if (colev[0] == 2)
         stoX = drval[0];
      else
         stoX = crval[0];
   }

   /*-----------------------------------------------------------------*/
   /* Start a loop over all physical coordinates inside the range of  */
   /* the axis. Start at 'startph' and step in each direction with    */
   /* step size 'deltaph'. If only grids has to be plotted then the   */
   /* physical coordinates are in fact grids.                         */
   /*-----------------------------------------------------------------*/
   Xinstart   = Xin;
   Yinstart   = Yin;
   sign       = 1;
   minorcount = 0;
   offset     = 0.0;
   first      = YES;
   fitsway    = NO;


   if (physical &&                                       /* Re-initialize */
      ((strchr(status, 'F') != NULL) || (strchr(status, 'O') != NULL) || offsform) )
      fitsway = YES;


   /* Draw all labels inside range */
   do
   {
      integer   procooutside = NO;
      if (physical)
      /*-----------------------------------------*/
      /* Transform physical coordinates to grids */
      /*-----------------------------------------*/
      {
         spatialmap = ( ((axistype[0] == 1) && (axistype[1] == 2)) ||
                        ((axistype[0] == 2) && (axistype[1] == 1)) );
         if (!fitsway)
         {
            if (spatialmap)
            {
               double proX, proY;
               proX = Xin;  proY = Yin;
               if (prosys[axn] == 1)
               {
                  if (axisL || axisR)
                  {
                     proX = 0.0;                              
                  }
                  else
                  {
                     proY = 0.0;
                  }
               }
               if (procomode == 1)
               {
                  proX += procoXoff;
               }
               else
               {
                  proY += procoYoff;
               }
               anyoutD( 16, "Debug: Use 'proco', because this is a spatial map");
               /*--------------------------------------*/
               /* MODE       XIN     YIN   XOUT   YOUT */
               /*   1          X     LAT    LON      Y */
               /*   2        LON       Y      X    LAT */
               /*--------------------------------------*/               
               r1 = proco_c( &proX, &proY,
                             &Xout,&Yout,
                             &crval[0], &crval[1],
                             &cdelt[0], &cdelt[1],
                             &skyangle,
                             &prosys[axn],
                             &procomode );
               if (procomode == 2)
                  Xout += procoXoff;
               else
                  Yout += procoYoff;
               (void) sprintf( message, "Debug: 'proco': Xin=%f, Yin=%f", proX, proY );
               anyoutD( 16, message );
               (void) sprintf( message, "Debug: 'proco': Xout=%f, Yout=%f", Xout, Yout );
               anyoutD( 16, message );
               if (r1 != 0 && r1 != 5)
               {
                  procoerr( message, r1 );
                  anyoutD( logdev, message );
                  anyoutD( logdev, "No coordinate transformation possible, using FITS def." );
                  fitsway = YES;
               }
               if (r1 == 5)
               {
                  procooutside = YES;
               }
               else 
                  procooutside = NO;
            }
            else   /* Not a spatial map */
            {
               if (rotated[axn])
               {
                  anyoutD( logdev, "Rotated axis in NON spatial map --> use FITS trans.");
                  fitsway = YES;
               }
               else
               {
                  double   dumphys[2], dumgrid[MAXAXES];
                  /* cotrans seems to change the values of dumphys, so initialize in the loop */
                  if ((axisB || axisT) && Xin != stoX)
                     stoX = Xin;
                  if ((axisL || axisR) && Yin != stoY)
                     stoY = Yin;
                  dumphys[0] = stoX;
                  dumphys[1] = stoY;
                  if ((subdim == 1) && (axisL || axisR))
                     dumphys[0] = dumphys[1];
                  r1 = cotrans_c( Setin, subin, dumphys, dumgrid, &phys2grid );
                  if (r1 == 0)
                  {
                     Xout = dumgrid[axnum[0]-1];
                     Yout = dumgrid[axnum[1]-1];
                     (void) sprintf( message, "Debug: Cotrans: Xin=%f Yin=%f", Xin, Yin );
                     anyoutD( 16, message );
                     (void) sprintf( message, "Debug: Cotrans: Xout=%f Yout=%f", Xout, Yout );
                     anyoutD( 16, message );
                  }
                  else
                  {
                    fitsway = YES;
                    anyoutD( logdev, "Trying the FITS way..." );
                  }
               } /* End if non rotated non spatial map */
            } /* End if non spatial map */
         } /* End if !fitsway */

         if (fitsway)      /* Do not use else here! */
         {
            anyoutD( 16, "Debug: Calculate coordinates in FITS way" );
            (void) sprintf( message, "Debug: Colev[%d]=%d, cdelt=%f, crval=%f",
                            axn, colev[axn], cdelt[axn], crval[axn] );
            anyoutD( 16, message );
            {
               double start = 0.0, delt = 0.0;
              
               if (colev[axn] == 1) { start = crval[axn]; delt = cdelt[axn]; }
               if (colev[axn] == 2) { start = drval[axn]; delt = ddelt[axn]; }

               if (axisB || axisT)
                  Xout = ( Xin - start ) / delt;
               else
                  Yout = ( Yin - start ) / delt; 
            }
         }
      }        /* End if physical */
      else     /* In case only the grids have to be plotted: */
      {
         Xout = Xin;
         Yout = Yin;
      }


      /*--------------------------------------------------------------------*/
      /* Test whether the physical starting point is within the axis range. */
      /*--------------------------------------------------------------------*/
      if (axisB || axisT)
      {
         (void) sprintf( message, "Debug: x, box[0], box[2] = %f %f %f",
                         Xout, box[0], box[2] );
         anyoutD( 16, message );
         if ((Xout > MYMAX(box[0],box[2])) || (Xout < MYMIN(box[0],box[2])))
            inside = NO;
         else
         {
            inside   = YES;
            Yout     = Yin;     /* Yin was the grid position */
            labelval = Xin;
         }
      }
      else
      {
         (void) sprintf( message, "Debug: y, box[1], box[3] = %f %f %f",
                         Yout, box[1], box[3] );
         anyoutD( 16, message );
         if ((Yout > MYMAX(box[1],box[3])) || (Yout < MYMIN(box[1],box[3])))
            inside = NO;
         else
         {
            Xout     = Xin;
            inside   = YES;
            labelval = Yin;
         }
      }
      if (procooutside)      
         inside = NO;
      if (inside)
      {
         bool  drawminor = NO;
         
         Xmm = Xgrid2mm(Xout);
         Ymm = Ygrid2mm(Yout);
         PLMOVE( Xmm + xoffsmm, Ymm + yoffsmm );
         if (nminors != 0)
         {
            if (minorcount%nminors)                     /* The minor tick marks */
               drawminor = YES;
         }

         if (logarit)
         {
            drawminor = NO;
            labelval = pow( 10.0, labelval );
         }

         if (drawminor)
         {
            if (plotticks)
               PLDRAW( Xmm + xoffsmm + xtick/2.0, Ymm + yoffsmm + ytick/2.0 );
         }
         else
         {
            /* Major tick marks and labels */
            if (plotticks)
               PLDRAW( Xmm + xoffsmm + xtick, Ymm + yoffsmm + ytick );
            if (extendticks && (!spatialmap || fitsway) && minorcount == 0 && sign == 1)
            {
               anyoutD( logdev, "Cannot create a coordinate grid because axes are not" );
               anyoutD( logdev, "spatial or coordinates are calculated in FITS.");
            }
            if (extendticks && spatialmap && !fitsway)
            /*------------------------------------------------------------*/
            /* Extend the tick marks to create a coordinate grid. This is */
            /* only possible for maps with two spatial axes. Then 'proco' */
            /* can be used with one fixed physical coordinate and a       */
            /* running grid. The grid value is increased or decreased     */
            /* by 'cgstep'.                                               */
            /*------------------------------------------------------------*/
            {
               int      cont = YES;
               double   Xin_c = Xin;
               double   Yin_c = Yin;
               double   Xgr, Ygr;
               int      domove = YES;
               int      firstproco = YES;            
               
               pgebuf_c();
               while (cont)
               {
                  if (axisL)
                     { Xin_c += *cgstep; cont = (Xin_c < box[2]); }
                  if (axisR)
                     { Xin_c -= *cgstep; cont = (Xin_c > box[0]); }
                  if (axisB)
                     { Yin_c += *cgstep; cont = (Yin_c < box[3]); }
                  if (axisT)
                     { Yin_c -= *cgstep; cont = (Yin_c > box[1]); }
                  r1 = proco_c( &Xin_c, &Yin_c,
                                &Xout, &Yout,
                                &crval[0], &crval[1],
                                &cdelt[0], &cdelt[1],
                                &skyangle,
                                &prosys[axn],
                                &procomode );
                  if (r1 != 0 || firstproco)
                  {
                     /* An error, do not draw only move */
                     domove = YES;                     
                  }
                  if (axisL || axisR)
                  {
                     Xgr  = Xin_c;   
                     Ygr  = Yout + procoYoff;   /* Correct proco for crpix */
                     cont = (cont && Ygr >= box[1] && Ygr <= box[3]);
                  }
                  else
                  {
                     Xgr  = Xout + procoXoff;   /* Correct proco for crpix */
                     Ygr  = Yin_c;
                     cont = (cont && Xgr >= box[0] && Xgr <= box[2]);
                  }
                  if (domove)
                  {
                     PLMOVE( Xgrid2mm(Xgr), Ygrid2mm(Ygr) );
                     domove = NO;
                  }
                  else if (cont)
                  {
                     PLDRAW( Xgrid2mm(Xgr), Ygrid2mm(Ygr) );
                  }
                  if (r1 == 0)
                     firstproco = NO;
               }               
            }
            if (plotlabels)
            {
               if (offsform)
               {
                  if (defform)
                  {
                     if (offset == 0.0)
                        Labelstr.l = sprintf( Labelstr.a, "%g", (float) offset );
                     else
                        Labelstr.l = sprintf( Labelstr.a, "%+g",(float) offset );
                  }
                  else
                     Labelstr.l = printusing_c( tofchar(axisformat), &offset, Labelstr );
                  offset += (double) sign * deltastored;
               }
               else
               {
                  if (numform)
                     Labelstr.l = printusing_c( tofchar(axisformat), &labelval, Labelstr );
                  if (defform)
                  {
                     if (fabs(labelval) < 10e-16)
                        labelval = 0.0;
                     Labelstr.l = sprintf( Labelstr.a, "%g", labelval );
                  }
                  if (hmsform)
                  {
                     if (nelc_c(Axformat) == 0)
                        Labelstr.l = hmsdms( &labelval, Labelstr, &first, &deltaph,
                                             tofchar("hms") );
                     else
                        Labelstr.l = hmsdms( &labelval, Labelstr, &first, &deltaph,
                                             Axformat );
                  }
                  if (dmsform)
                  {
                     if (nelc_c(Axformat) == 0)
                        Labelstr.l = hmsdms( &labelval, Labelstr, &first, &deltaph,
                                             tofchar("dms") );
                     else
                        Labelstr.l = hmsdms( &labelval, Labelstr, &first, &deltaph,
                                             Axformat );
                  }
               }
               first = NO;
               PLTEXT( Xmm+xoffsmm+xlabmm, Ymm+yoffsmm+ylabmm, 0.0,
                       fjust, Labelstr );
               {
                  fint units = 2;     /* mm */
                  float xl, yl;
                  pglen_c( &units, Labelstr, &xl, &yl );
                  lablenmm = MYMAX( lablenmm, xl );
               }
            }
         }
      }
      else
      {
         /* The first value for the power scaling (logarit) is always */
         /* outside range. In that case, continue... */
         if (!logarit && minorcount == 0)
         {
            anyoutD( logdev, "Start is outside range of axis" );
            pgebuf_c();
            return(ERR_OUTSIDE);
         }
         Xin = Xinstart;
         Yin = Yinstart;
         if (sign == 1)
         {
            minorcount = 0;               /* Skip drawing label of starting point 2nd time */
            inside = YES;
            sign   = -1;
            if (offsform)
               offset = -deltastored;     /* Increase offset in units of original 'delta' */
         }
      }
      minorcount++;
      if (logarit)
      {
         double val = 0.0;
         if (*axloglen > 0)
            val = axlogs[al] * logfact;
         else                             /* No array elements defined */
            val = logfact;
         al++;
         if (al >= *axloglen)
         {
            logfact *= 10.0;
            al = 0;
         }
         if (xinc != 0.0)
            Xin = log10( val );
         else
            Yin = log10( val );
         inside = ( log10( val ) < maxph );
      }
      else
      {
         Xin += ((double) sign) * xinc;
         Yin += ((double) sign) * yinc;
      }
   }
   while( inside );

   /*-----------------------------------------------------------------------*/
   /* Create default title. Return also the calculated position and axis id */
   /*-----------------------------------------------------------------------*/
   if (inset)
   {      
      convtype( axistype[axn], skysys[axn], velsys[axn], axname, Ctype[axn] );
      if (drawgrids)
      {
         if (offsform)
            slen = sprintf( axtitle, "Offset %s (GRIDS)", axname );
         else
            slen = sprintf( axtitle, "%s (GRIDS)", axname );
      }
      else
      {
         /* The physicals */
         if (offsform)
         {
            /*-----------------------------------------------------*/
            /* The offset format: give axis name, units of 'delta' */
            /*-----------------------------------------------------*/
            if (colev[axn] == 1)
               (void) sprintf( axtitle, "Offset %s ", axname );
            else
            {
               char *cptr = strtok( Dtype[axn].a, "- " );
               if (cptr != NULL)
                  (void) sprintf( axtitle, "Offset %s ", cptr );
               else
                  (void) sprintf( axtitle, "Offset %s ", "?" );
            }
            if (convD)
               slen = sprintf( axtitle, "%.*s (%.*s)", strlen(axtitle), axtitle,
                               nelc_c(Deltaunits), Deltaunits.a );
            else
            {
               if (colev[axn] == 1)
                  slen = sprintf( axtitle, "%.*s (%.*s)", strlen(axtitle), axtitle,
                                  nelc_c(Cunit[axn]), Cunit[axn].a );
               else
                  slen = sprintf( axtitle, "%.*s (%.*s)", strlen(axtitle), axtitle,
                                  nelc_c(Dunit[axn]), Dunit[axn].a );
            }
         }
         else if ((skysys[axn] == 1) || (skysys[axn] == 3))
         {
            double    epoch;
            /*-----------------------------------------------------*/
            /* For equatorial and ecliptic coords. give axis name  */
            /* and, if found in the header, also the epoch.        */
            /*-----------------------------------------------------*/
            slen = sprintf( axtitle, "%s", axname );
            r1 = 0;
            gdsd_rdble_c( Setin, tofchar("EPOCH"), &setlevel, &epoch, &r1 );
            if (r1 >= 0)
               slen = sprintf( axtitle, "%.*s (%.1f)", strlen(axtitle), axtitle, epoch );
         }
         else
         {
            /*-----------------------------------------------------*/
            /* All others: give axis name and header units         */
            /*-----------------------------------------------------*/
            if (colev[axn] == 1)
               slen = sprintf( axtitle, "%s (%.*s)", axname,
                               nelc_c(Cunit[axn]), Cunit[axn].a );
            else
            {
               /*
               char *cptr = strtok( Dtype[axn].a, "- " );
               if (cptr != NULL)
                  slen = sprintf( axtitle, "%s (%.*s)", cptr,
                                  nelc_c(Dunit[axn]), Dunit[axn].a );
               else
                  slen = sprintf( axtitle, "%s (%.*s)", "?",
                                  nelc_c(Dunit[axn]), Dunit[axn].a );
               */
               if (axname)
                  slen = sprintf( axtitle, "%s (%.*s)", axname,
                                  nelc_c(Dunit[axn]), Dunit[axn].a );
               else
                  slen = sprintf( axtitle, "%s (%.*s)", "?",
                                  nelc_c(Dunit[axn]), Dunit[axn].a );
            }
         }
      } /* end else if physical */
   } /* end if inset */

   /*------------------------------------------------------------*/
   /* Return title properties. Do not exceed max. length, i.e do */
   /* not copy more characters than the length of 'Axtitle'.     */
   /* Correct position for left and right axis for the max.      */
   /* of the labels. Add extra offsets to title position.        */
   /*------------------------------------------------------------*/

   /* Correct the position in x direction for the title along y */

   if (!labelsinside)
   {
      if (axisL)
         Xofftitmm = -lablenmm;
      if (axisR)
         Xofftitmm = lablenmm;
   }

   /*--------------------------------------------------*/
   /* Store final title positions in 'titleXY' array.  */
   /*--------------------------------------------------*/   
   titleXY[titlenr*2]   += xlabmm + Xofftitmm;
   titleXY[titlenr*2+1] += ylabmm + Yofftitmm;
    
   slen = MYMIN( slen, Axtitle.l );
   for (i = 0; i < slen; i++)
      Axtitle.a[i] = axtitle[i];
   for (i = slen; i < (int) Axtitle.l; i++) 
      Axtitle.a[i] = ' ';

   pgebuf_c();

   if (inset)
   {
      for (i = 0; i < 2; i++)
      {
         free( Ctype[i].a );
         free( Dtype[i].a );
         free( Cunit[i].a );
         free( Dunit[i].a );
      }
   }

   return(slen);
}
