
/*
                            COPYRIGHT (c) 1995
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             regrid.dc1

Program:       REGRID

Purpose:       Create an output map with different grid along one axis.

Category:      COORDINATES, MANIPULATION, UTILITY

File:          regrid.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give NAME of set:
    
               There is no need to specify subsets. The 'regrid axis'
               is entered with AXNAME=


   AXNAME=     Enter name of axis to regrid:                    [Abort]
   
               After INSET= a list is displayed with the names of
               all axes in INSET=. One of these axes can be selected
               to become the 'regrid axis'. For this axis you will
               be prompted to enter a new grid spacing. Abbreviation
               of axis names is allowed.


   CDELT=      Enter new grid spac. for ... axis in ...:       [header]
   
               The new grid spacing sets the regridding. It can be a 
               number greater than or less than the original value.
               Its sign will be converted to the sign of the 
               original grid spacing. In the prompt the axis name
               and the corresponding units are listed. The default
               spacing is read from the header. The units are also
               read from the header, but if these units are DEGREE,
               then they are converted to seconds of arc.
  
  
   BOX=        Give BOX in ....                         [entire subset]
   
               Resize the INPUT by entering a box. The default is 
               the entire set as selected by INSET= and the box 
               values cannot exceed the limits of INSET=
               Note that the output set will show axis limits as in
               BOX= except for the 'regrid axis' which will get
               new values because of the regridding.
   
  
   OUTSET=     Give output set:

               This will be the name of the regridded INSET=
               It has sizes as defined by BOX= The size of the
               regridded axis is also defined by CDELT=


   IPOL=       Enter interpolation method:                   [1=linear]
    
               See also 'description'.
               A list with interpolation methods will be presented.
               After selecting a method, the program starts to regrid.


   WIDTH=      Enter width of regridding function:                [1.0]
   
               Only asked if IPOL=2.
               Give the position of the first zero of the sinc 
               in old gridunits.
               
              
             

Description:   Regrid data in INSET= in one direction. The direction
               is one of the axis names in a set. Before the prompt
               AXNAME= you get a list with all set axes. The regrid 
               direction is selected by entering one of the axis names.
               If you want to regrid more than one axis, please repeat 
               the process onto intermediate results (sets).
               The sign of the grid spacing cannot be altered. If you
               want to change this sign then use a program like AXSWAP
               to do this. The new limits of the 'regrid axis' LO and
               HI are:
               
               LOnew = LOold * ABS(CDELTold/CDELTnew)
               HInew = HIold * ABS(CDELTold/CDELTnew)               



               INTERPOLATIONS:
               ==============
               
              
               1) Linear interpolation
               
               Standard linear interpolation between two pixels in the 
               input image.


               2) Sinc interpolation
               
               Interpolation method is SINC.                    
               This gives a weight to an old pixel dependent on 
               its distance to the new pixel position and on    
               the ratio of old and new gridspacings.           
               The default regridding function is the following:
                                                                
                           sin pi*x              pi      2      
                  R(x)  =  --------  exp (  4*ln(--)  * x  )    
                             pi*x                 4             
                                                                
               with  a = signed distance from the central pixel 
               in x or y. This regridding function has the      
               following properties:                            
               R(0) = 1;  R(+/-0.5) = 0.5;  R(+/-1) = 0         
               
              
               3) Spline interpolation
                
               Standard 1-dim. cubic spline interpolation.
               
               4) Average
               
               Replace output by average of a number of input pixels.
               This number of input pixels is set by the ratio of
               input pixels to output pixels. The averaging will only
               work if this ratio is integer and greater than 1.
               
               
              
               BLANKS:
               ======
               
               All interpolation methods take care of blank image data.
               1) If the LINEAR interpolation is used, all input values
                  that are blank, set the corresponding output values
                  to blank.
               2) If the SINC method is used, then the weight of an 
                  'input' blank is set to 0. If the sum of the weights 
                  is 0 then a blank is returned for the output.
               3) The SPLINE method interpolates over blanks.
               
               

Notes:         .......

Example:       .......

Updates:       Dec 07, 1995: VOG, Document created.
               Nov 30, 1998: VOG, Average option inserted.
               Apr 14, 2009: VOG, Replaced macro NINT by a version that uses
                                  floor() which is consistent with other 
                                  routines which process coordinates.

#<
*/

/*  regrid.c: include files     */

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
#include    "wmatch.h"       /* Matching of strings with wildcard. */
#include    "factor.h"       /* Conversion factor between two different units. */
#include    "spline1.h"      /* 1D cubic spline interpolation. */
#include    "status.h"       /* Display text in the "RUNNING" status display. */
#include    "timer.h"        /* Returns the cpu time and real time. */

/* User input routines */

#include    "userfio.h"      /* Easy-C companions for user interface routines.*/
#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"      
#include    "userreal.h"     
#include    "userdble.h"     
#include    "usertext.h"     
#include    "usercharu.h"    
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/


/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsd_rchar.h"
#include    "gdsd_rdble.h"   /* Read double from descriptor */
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "minmax3.h"      /* Find min, max and #blanks in subset. */
#include    "wminmax.h"      /* Writes (new) minimum and maximum and number */
                             /* of blanks of subsets in the descriptor file */
                             /* and optionally deletes the MINMAX descriptors */
                             /* at intersecting levels. */


/* Output set related */

#include    "gdsasn.h"       /* GDSASN copies the coordinate system of a */
                             /* previously opened input set obtained with */
                             /* GDSINP to the output set to be obtained */
                             /* with GDSOUT. */
#include    "gdscss.h"
#include    "gdscpa.h"
#include    "gdsout.h"       /* GDSOUT prompts the user to enter the */
                             /* name of an output set and the subsets, */
                             /* and returns the number of subsets entered. */
#include    "gdsi_write.h"   /* Writes data to (part of) an set. */



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

/* Malloc version of 'fmake. Strings allocated with'  */
/* finit, must be freed with free( fc.a ) */
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }  

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
/* Pre Apr 2009 def.: #define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) ) */
#define NINT(a)        ( (int) floor( (double) (a) + 0.5 ) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )
#define PI             3.141592653589793
#define RAD(a)         ( a * 0.017453292519943295769237 )
#define DEG(a)         ( a * 57.295779513082320876798155 )

#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     1          /* Max. allowed subsets */
#define MAXBUF         4096       /* Buffer size for I/O */
#define STRLEN         256        /* Max length of strings */
#define FILENAMELEN    256        /* Max length of file names */
#define FITSLEN        20         /* Max length of header items etc.*/
#define TASKNAMLEN     20         /* Store task name in str. with this length */
#define UPDATES        20         /* Number of status updates. */
#define NONE           0          /* Default levels in userxxx routines */
#define REQUEST        1          
#define HIDDEN         2          
#define EXACT          4          
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0          
#define LINEAR         1
#define SINC           2
#define SPLINE         3
#define AVERAGE        4


/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give NAME of set:")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set: ")
#define KEY_AXNAME     tofchar("AXNAME=")
#define KEY_IPOL       tofchar("IPOL=")

/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered ccording to the */
                                    /* specification by the user. */
static fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
                                    /* number of grids along an axes as */
                                    /* specified by the user. The first elements */
                                    /* (upto the dimension of the subset) contain */
                                    /* the length of the subset axes, the other */
                                    /* ones contain the the number of grids along */
                                    /* an axes outside the subset. */
                                    /* the operation for each subset, Class 2 */
                                    /* is for applications for which the operation */
                                    /* requires an interaction between the different */
                                    /* subsets. */
static fint     subdim;             /* Dimensionality of the subsets for class 1 applications */
static fint     setdim;             /* Dimension of set. */


/* Box and frame related */

static fint     flo[MAXAXES];       /* Low  edge of frame in grids */
static fint     fhi[MAXAXES];       /* High edge of frame in grids */
static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */
static fint     bloO[MAXAXES];      /* Low  edge of box in grids */
static fint     bhiO[MAXAXES];      /* High edge of box in grids */


/* OUTSET related variables */

static fchar    Setout;
static fint     subout[MAXSUBSETS]; /* Output subset coordinate words */
static fint     nsubsout;
static fint     axnumout[MAXAXES];
static fint     axcountout[MAXAXES];


/* Miscellaneous */

static fint     setlevel = 0;
static float    blank;              /* Global value for BLANK. */
static char     taskname[TASKNAMLEN+1];  /* Name of current task */





static void errorC( int  level,
                    char *str )
/*-----------------------------------------------------------*/
/* PURPOSE: User error handling routine.                     */
/* The C version of 'error'.                                 */
/* 1 = Warning, 2 = Minor error, 3 = Serious error,          */
/* 4 = Fatal error                                           */
/*-----------------------------------------------------------*/
{
   fint flev = (fint) level;
   error_c( &flev, tofchar( str ) ); 
}



static void getaxnames( char axnames[][FITSLEN], 
                        fchar Setin )
/*-----------------------------------------------------------*/
/* PURPOSE: Fill character array with names of all axes in   */
/*          current set.                                     */
/*-----------------------------------------------------------*/
{
   fint    setdim;
   int     i;
   char    message[80];
   fchar   Ctype;
   
  
   fmake( Ctype, FITSLEN );   
   setdim = gdsc_ndims_c( Setin, &setlevel );
   for (i = 0; i < setdim; i++)
   {
      fint r = 0;
      sprintf( message, "CTYPE%d", i+1 );
      gdsd_rchar_c( Setin,                   /* Read from header */
                    tofchar(message), 
                    &setlevel, 
                    Ctype, 
                    &r );
      /* Copy and store in 2-dim 'axnames' array. */
      sprintf( axnames[i], "%.*s", nelc_c(Ctype), Ctype.a );
   }
}



static int getprofilenr( fchar Setin,
                         char axnames[][FITSLEN] )
/*-----------------------------------------------------------*/
/* PURPOSE: Given an array with axis names ('axnames'), ask  */
/*          user to select the regrid axis.                  */
/* Axis names can be abbreviated.                            */
/*-----------------------------------------------------------*/
{
   int    unique;
   int    abort;
   int    numaxes = 0;      
   char   wildcard = '*';
   fchar  Wildcard;
   fchar  Axisname;   
   fint   r;
   fint   dfault = REQUEST;
   fint   setdim;
   fint   axflag = -1;
   
     
   fmake( Axisname, FITSLEN+1 );    
   setdim  = gdsc_ndims_c( Setin, &setlevel );
   fmake( Wildcard, 1 );
   Wildcard.a[0] = wildcard;
   do
   {
      int   ok = YES;
      r = usertext_c( Axisname, 
                      &dfault, 
                      KEY_AXNAME,
                      tofchar("Enter name of axis to regrid:   [Abort]") );
      abort = (r == 0);
      if (!abort)
      {
         int    k;
         int    len;
         int    num = 0;
            
         /* Add wildcard character */
         len = nelc_c(Axisname);
         Axisname.a[len] = wildcard;
         unique = 0;
         /*----------------------------------------*/
         /* Is the name entered by the user a      */
         /* valid axis name?                       */
         /*----------------------------------------*/            
         for (k = 0; k < setdim; k++)
         {
            fint    caseinsensitive = 0;
            if ( wmatch_c(tofchar(axnames[k]), 
                          Axisname,                             
                          Wildcard,
                          &caseinsensitive) )
            {
               unique++;
               num = k;
            }
         }
         ok = (unique == 1);
         if (unique == 0)
            reject_c( KEY_AXNAME, tofchar("Entry is not a valid axis name!") );
         if (unique > 1)
            reject_c( KEY_AXNAME, tofchar("Entry is not a unique axis name!") );
         if (ok)
         {
            axflag = num;
            numaxes++;
            abort = YES;
         }
         else
            abort = NO;
      }
   }  
   while (!abort);
   return( axflag );
}



static float sinc( float x )
/*-----------------------------------------------------------*/
/* PURPOSE: Calculate sinc(x), see documentation at          */
/*         'interpolate'.                                    */ 
/*-----------------------------------------------------------*/
{
   double    result;
   double    xx = (double) x;
   
   if (ABS(x) < 1e-8)
      result = 1.0;
   else
   {
      /* log(x) : NATURAL logarithm of x */
      result = (sin(PI*xx)/(PI*xx)) * exp(4.0*log(PI/4.0) * xx*xx);
   }
  
   return( (float) result );
}




static void interpolate( float   *imageI,
                         fint    buflenI,
                         float   *imageO,
                         fint    buflenO,
                         int     option,
                         float   width,
                         double  cdeltold,
                         double  cdeltnew,
                         fint    xlo, 
                         fint    xhi,
                         fint    xloO,
                         fint    xhiO,
                         float   *XI,
                         float   *XO )
/*-----------------------------------------------------------*/
/* PURPOSE: Interpolate data in array.                       */
/* Given an input array 'imageI' filled with 'buflenI' data- */
/* points. The array 'imageO' is smaller or bigger, depending*/
/* on the new grid spacing for the regrid axis. The data in  */
/* this output array has to be interpolated. The interpola-  */
/* tion is selected by 'option'.                             */
/*-----------------------------------------------------------*/
{
   int          i, j;
   fint         r;
   static int   blankflag = NO;
   static int   first = YES;   
  
 


   if (first)
   /*--------------------------------------------------*/
   /* Create the X arrays. Note that the contents does */
   /* not change for all profiles. For the spline      */
   /* function, the x-data must be in increasing order */
   /* (ABS(CDELT)).                                    */   
   /*--------------------------------------------------*/
   {
      j = 0;
      for (i = xlo; i <= xhi; i++)
         XI[j++] = ((float) i) * (float) fabs(cdeltold);
       
      j = 0;
      for (i = xloO; i <= xhiO; i++)
         XO[j++] = ((float) i) * (float) fabs(cdeltnew);
      
      first = NO;
   }
  
   if (option == SPLINE)
   /*--------------------------------------------------*/
   /* Interpolation method is SPLINE.                  */
   /*--------------------------------------------------*/
   { 
      r = spline1_c( XI, imageI, &buflenI, XO, imageO, &buflenO );
      if (r != 0)
      {
         if (r > 0 && !blankflag)
         {
            anyoutf( 1, "There were input profiles containing blanks!" );

            blankflag = YES;
         }
         if (r == -1)
            errorC( 4, "Not enough memory to create internal tables in spline fie.!" );
         if (r == -2)
            errorC( 4, "X range problems in interpolation!" );
      }
   }

   else if (option == SINC)
   /*--------------------------------------------------*/
   /* Interpolation method is SINC.                    */
   /* This gives a weight to an old pixel dependent on */
   /* its distance to the new pixel position and on    */
   /* the ratio of old and new gridspacings.           */
   /* The default regridding function is the following:*/
   /*                                                  */
   /*             sin pi*x              pi      2      */
   /*    R(x)  =  --------  exp (  4*ln(--)  * x  )    */
   /*               pi*x                 4             */
   /*                                                  */
   /* with  x = signed distance from the central pixel */
   /* in x. This regridding function has the           */
   /* following properties:                            */
   /* R(0) = 1;  R(+/-0.5) = 0.5;  R(+/-1) = 0         */
   /*--------------------------------------------------*/
   {
      int     i, j;
      float   sum;
      float   sumweights;
      float   fwidth = ABS( width*cdeltold );
      float   sincval;

      for (j = 0; j < buflenO; j++)              /* For all output positions: */
      {
         sum = sumweights = 0.0;
         for (i = 0; i < buflenI; i++)
         {
            float    dx = XO[j] - XI[i];
            if (ABS(dx) < fwidth)
            {
               sincval = sinc( dx/fwidth );
               /* Input value contributes only if it is not a blank */
               if (imageI[i] != blank)
               {
                  sumweights += sincval;
                  sum += sincval * imageI[i];
               }
            }
         }
         if (sumweights == 0.0)
            imageO[j] = blank;
         else
            imageO[j] = sum / sumweights;      
      }
   }


   else if (option == AVERAGE)
   {
      /* Replace an output pixel by the average of the contained input */
      /* pixels. This works only if the regrid factor is integer and   */
      /* greater than 1. */
      
      int m = (int) fabs(cdeltnew/cdeltold);


      for (j = 0; j < buflenO; j++)              /* For all output positions: */
      {
         float    sum = 0.0;
         int      n = 0;
         int      indxlo, indxhi;
         
         indxlo = m*j;
         indxhi = m*(j+1);

         for (i = indxlo; i < MYMIN(indxhi,buflenI); i++)
         {
            if (imageI[i] != blank)
            {               
               sum += imageI[i];
               n++;
            }
         }
         if (n != 0)
            imageO[j] = sum / (float) n;
         else
            imageO[j] = blank;
      }
   }
   
  
   else if (option == LINEAR)
   {
      int  j1, j2, indx;

      for (i = xloO; i <= xhiO; i++)             /* For all output positions: */
      {
         float  x1, x2, y1, y2;
         float  dx;

         dx = (float) i * (float) fabs(cdeltnew/cdeltold);
         if (dx < 0.0)
            j1 = (int) dx - xlo - 1;
         else
            j1 = (int) dx - xlo;
         j2 = j1 + 1;
         j1 = MYMAX( j1, 0 );
         j2 = MYMIN( j2, buflenI-1 );
         x1 = XI[j1];
         y1 = imageI[j1];
         x2 = XI[j2];
         indx = i - xloO;
         if (x2 == x1)
            imageO[indx] = y1;
         else
         {
            y2 = imageI[j2];
            if (y1 == blank || y2 == blank)
               imageO[indx] = blank;
            else
            {
               /* Linear interpolation between y1,y2 at XO[indx] */
               imageO[indx] = (y1-y2) * ((XO[indx]-x1)/(x1-x2)) + y1;
            }
         }
      }
   }
}



static void process( fchar   Setin, 
                     fchar   Setout,
                     fint    *flo,
                     fint    *fhi,
                     fint    *floO,
                     fint    *fhiO, 
                     float   *imageI, 
                     fint    buflenI,
                     float   *imageO,
                     fint    buflenO, 
                     int     axflag,
                     double  cdeltold,
                     double  cdeltnew,
                     int     ipoloption,
                     float   width )
/*-----------------------------------------------------------*/
/* PURPOSE: Process all profiles in the regrid direction.    */
/*-----------------------------------------------------------*/
{
   int      i;
   int      numprofiles;
   int      step;
   fint     xlo[MAXAXES], xhi[MAXAXES];
   fint     xloO[MAXAXES], xhiO[MAXAXES];
   float    *XI = NULL;                              /* Work arrays for spline */
   float    *XO = NULL;
   char     message[120];
   

   XI = (float *) calloc( (int) buflenI, sizeof(float) );
   if (XI == NULL) 
      errorC( 4, "Cannot allocate memory for work input buffer!" );

   XO = (float *) calloc( (int) buflenO, sizeof(float) );
   if (XO == NULL) 
      errorC( 4, "Cannot allocate memory for work output buffer!" );   
   

   numprofiles = 1;
   /*--------------------------------------------------*/
   /* Count number of profiles that has to be processed*/
   /* This number is the product of all axis lengths,  */
   /* except the lenghth of the regrid axis.           */
   /*--------------------------------------------------*/
   for (i = 0; i < setdim; i++)
   {
      if (i != axflag)
      {
         numprofiles *= (fhi[i]-flo[i]+1);
      }
      /* Create vectors equal to the starting point of the data cube */
      xlo[i]  = flo[i];
      xloO[i] = floO[i];
   }  
   
   /*--------------------------------------------------*/
   /* Increase a pixelcounter along the first set axis */
   /* which is not the regrid axis. If the pixel       */
   /* counter exceeds the upper boundary ('fhi') then  */
   /* increase the pixel counter for the next axis.    */
   /* This way we process all profiles in fastest I/O  */
   /* order.                                           */
   /*--------------------------------------------------*/

   step = numprofiles / UPDATES;   
   for (i = 0; i < numprofiles; i++)
   {
      int j, k;            

      if (!(i%step))           /* Update message ~'UPDATES' times */
      {
         (void) sprintf( message, "progress: %d%%", i*100/numprofiles );
         status_c( tofchar(message) );
      }
      for (j = 0; j < setdim;)
      {         
         if (xlo[j] > fhi[j])
         {
            xlo[j] = flo[j];
            j++;
            /* Skip increasing pixel counter if axis is the 'regrid' axis. */
            if (j == axflag && j < setdim)
               j++;
            xlo[j]++;
         }
         else
           j++;
      }         
      for (k = 0; k < setdim; k++)
      {
         if (k == axflag)
         {         
            xhi[axflag]  = fhi[axflag];
            xhiO[axflag] = fhiO[axflag];
         }
         else
         {
            xhiO[k] = xloO[k] = xhi[k] = xlo[k];
         }
      }            
      /*--------------------------------------------------*/
      /* The n-dim vectors are calculated. Next find the  */
      /* the corresponding coordinate words and fill a    */
      /* profile.                                         */
      /*--------------------------------------------------*/ 
      {
         fint  cwlo, cwhi;            /* Coordinate words */
         fint  cwloO, cwhiO;
         fint  tid = 0;               /* Transfer id's */   
         fint  pixelsdone;
         fint  tidO = 0;

         cwlo   = gdsc_fill_c( Setin, &subin[0], xlo );
         cwhi   = gdsc_fill_c( Setin, &subin[0], xhi );
         cwloO  = gdsc_fill_c( Setout, &subout[0], xloO );
         cwhiO  = gdsc_fill_c( Setout, &subout[0], xhiO );
         
         gdsi_read_c( Setin,
                      &cwlo, &cwhi,
                      imageI,
                      &buflenI,
                      &pixelsdone,
                      &tid );

         interpolate( imageI, buflenI, 
                      imageO, buflenO, 
                      ipoloption,
                      width,
                      cdeltold, cdeltnew,
                      xlo[axflag], xhi[axflag],
                      xloO[axflag], xhiO[axflag],
                      XI, XO );

         gdsi_write_c( Setout,
                       &cwloO, &cwhiO,
                       imageO,
                       &buflenO,
                       &pixelsdone,
                       &tidO );    
      }      
      if (axflag == 0 && setdim > 1)
         xlo[1]++;
      else
         xlo[0]++;            
   }
   status_c( tofchar("progress: 100%") );   
   free( XO );
   free( XI );
}


static int menu( void )
/*-----------------------------------------------------------*/
/* PURPOSE: Select the interpolation method.                 */
/* The return value is an argument of the 'interpolate'      */
/* function. It sets the type of interpolation that can be   */
/* used.                                                     */
/*-----------------------------------------------------------*/
{
   fint    r;
   fint    nitems, dfault;
   fint    opt;
   int     ok;
   int     numoptions = 4;                /* Change this if you add an option */
   
  
   anyoutf( 1, " " );
   anyoutf( 1, "      INTERPOLATIONS" );
   anyoutf( 1, "      ==============" );
   anyoutf( 1, " 1) Linear (default) " );
   anyoutf( 1, " 2) Sinc " );
   anyoutf( 1, " 3) Spline (for artificial data etc.)" );
   anyoutf( 1, " 4) Average " );   
   anyoutf( 1, " " );
   do
   {
      nitems = 1;
      dfault = REQUEST;
      opt    = 1;
      r      = userint_c( &opt, 
                          &nitems, 
                          &dfault, 
                          KEY_IPOL,
                          tofchar("Enter interpolation method:     [1]") );
      ok = (opt >= 1 && opt <= numoptions);
      if (!ok) 
         reject_c( KEY_IPOL, tofchar("Invalid number!") );
   }
   while (!ok);

   return( (int) opt );
}


 
static int getnewcdelt( fchar   Setin, 
                        char    axnames[][FITSLEN],
                        int     axflag,
                        double  *cdeltold,
                        double  *cdeltnew )
/*-----------------------------------------------------------*/
/* PURPOSE: Prompt user with grid spacing keyword.           */
/* Use header value as default. Take care of sign.           */
/* If header units are in degrees then convert DEGREE to     */
/* ARCSEC.                                                   */
/*-----------------------------------------------------------*/
{
   fchar   Cunit;
   char    message[160];
   char    name[FITSLEN];   
   double  cdelt;
   double  factor;
   fint    r;
   fint    nitems, dfault;
   int     convert;
   
  
   sprintf( message, "CDELT%d", axflag+1 );
   r = 0; 
   gdsd_rdble_c( Setin, tofchar(message), &setlevel, &cdelt, &r );
   if (r < 0) 
   {
      anyoutf( 1, "Cannot find %s in the header!", message );
      return( NO );
   }
   fmake( Cunit, FITSLEN );
   sprintf( message, "CUNIT%d", axflag+1 ); 
   r = 0;
   gdsd_rchar_c( Setin,                   /* Read from header */
                 tofchar(message),
                 &setlevel,
                 Cunit,
                 &r );
   if (r < 0)
      strcpy( Cunit.a, "?" );
   nitems = 1;
   dfault = REQUEST;   
   strcpy( name, axnames[axflag] );
   strtok( name, " -" );
  
   convert = ( factor_c(Cunit, tofchar("ARCSEC"), &factor) == 0 );   

   if (convert)
   {
      sprintf( message, 
              "Enter new grid spac. for %s axis in ARCSEC: [%g]",
               name, 
               cdelt*factor );
   }
   else
   {
      sprintf( message, 
              "Enter new grid spac. for %s axis in %.*s: [%g]", 
               name,
               nelc_c(Cunit), Cunit.a,
               cdelt );
   }
   *cdeltnew = cdelt;
   r = userdble_c( cdeltnew, &nitems, &dfault, tofchar("CDELT="),
                   tofchar(message) );
   if (r && convert)
   {
      *cdeltnew /= factor;                  /* Convert back to original units */
      *cdeltnew = fabs(*cdeltnew);           /* Sign must be same as original */
      if (cdelt < 0.0)
         *cdeltnew *= -1.0;
   }
   *cdeltold = cdelt;
   return( YES );
}



static float getsincwidth( int option )
/*-----------------------------------------------------------*/
/* PURPOSE: Get a width in pixels for a regridding function. */
/*-----------------------------------------------------------*/
{
   float    width  = 1.0;
   fint     dfault = REQUEST;
   fint     nitems = 1; 
  
   if (option == SPLINE)
      width = 1.0;                                             /* dummy value */
   if (option == SINC)
   {
      fint  r;
      r = userreal_c( &width, &nitems, &dfault, tofchar("WIDTH="),
                      tofchar("Enter width of regridding function:    [1.0]") );
      width = ABS( width );
   }
   return( width );
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint     maxsubs = MAXSUBSETS;
   fint     maxaxes = MAXAXES;             /* Max num. of axes the program can deal with.*/
   fint     class   = 1;                   /* Class 1 is for applications which repeat */
   fint     showdev = 1;
   fint     nsubs;                         /* Number of input subsets */
   fint     dfault;                        /* Default option for input etc */
   int      i;
   int      axflag;
   int      agreed = NO;
   int      ipoloption;
   fint     buflenI, buflenO;
   fint     elapse;                        /* CPU timer */
   char     axnames[MAXAXES][FITSLEN];
   double   cdeltold;      
   double   cdeltnew;
   double   cputime, realtime;             /* Variables for timer */
   float    *imageI = NULL;                /* Buffers for read/write. */
   float    *imageO = NULL;
   float    width;                         /* Width of regridding function */
   
     
     
   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      fchar    Task;                       /* Name of current task */
      fmake( Task, TASKNAMLEN );           /* Create empty string */      
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char. */
      strcpy( taskname, Task.a );
      IDENTIFICATION( taskname, RELEASE ); /* Show task and version */
   }
   setfblank_c( &blank );

   /*--------------------------------------------------*/
   /* Get the input set. Documentation can be found in */
   /* $gip_sub/gdsinp.dc2                              */
   /*--------------------------------------------------*/
   {
      fmake( Setin, STRLEN );
      dfault = NONE;
      subdim = 0;                    /* Allow all dim. structures */
      nsubs  = gdsinp_c( Setin,      /* Name of input set. */
                         subin,      /* Array containing subsets coordinate words. */
                         &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                         &dfault,    /* Default code as is USERxxx. */
                         KEY_INSET,  /* Keyword prompt. */
                         MES_INSET,  /* Keyword message for the user. */
                         &showdev,   /* Device number (as in ANYOUT). */
                         axnum,      /* Array of size 'maxaxes' containing the axes numbers. */
                                     /* The first elements (upto the dimension of the subset) */
                                     /* contain the axes numbers of the subset, */
                                     /* the other ones contain the axes numbers */
                                     /* outside the subset ordered according to the */
                                     /* specification by the user. */
                         axcount,    /* Number of grids on axes in 'axnum' */
                         &maxaxes,   /* Max. number of axes. */
                                     /* the operation for each subset. */
                         &class,     /* Class 1 is for applications which repeat */
                         &subdim );  /* Dimensionality of the subsets for class 1 */
   } 
   setdim  = gdsc_ndims_c( Setin, &setlevel );
   

   getaxnames( axnames, Setin );   
   anyoutf( 1, "Axis names of set [%.*s]:", nelc_c(Setin), Setin.a );   
   anyoutf( 1, "===============================" );

   /*--------------------------------------------------*/
   /* Display all axis names, but do not display the   */
   /* projection information. However keep long names  */
   /* unaltered for comparisons where this information */
   /* can be used (e.g. PARAM-1, PARAM-2 axes in a     */
   /* set).                                            */
   /*--------------------------------------------------*/
   for (i = 0; i < setdim; i++)
   {      
      char  str[FITSLEN];
      strcpy( str, axnames[i] );
      anyoutf( 1, "%d) %s", i+1, strtok(str, " -") );
   }
   anyoutf( 1, "===============================" );


   dfault = REQUEST;
   /*--------------------------------------------------*/
   /* Use a minimal match to find the keywords. The    */
   /* program sets a flag for each axis that the user  */
   /* wants to regrid ('axflag'). This information will*/
   /* be used later to regrid the data.                */
   /*--------------------------------------------------*/       
   axflag = getprofilenr( Setin, axnames );
   if (axflag < 0)
   {
      /* User was not interested in regridding, abort program */      
      anyoutf( 1, "Nothing to regrid, aborting!" );
      finis_c();
      return(EXIT_FAILURE);
   }

   if ( !getnewcdelt(Setin, axnames, axflag, &cdeltold, &cdeltnew) )
   {
      anyoutf( 1, "Program aborted because header was not complete!" );
      finis_c();
      return(EXIT_FAILURE);      
   }

   /*-------------------------------*/
   /* Determine edges of this frame */
   /*-------------------------------*/
   {
      fint  cwlo, cwhi;                             /* Local coordinate words */
      fint  r1, r2;
      int   m;
      
      r1 = 0;
      gdsc_range_c( Setin, &setlevel, &cwlo, &cwhi, &r1 );
      for (m = 0; m < (int) setdim; m++)
      {
         r1 = r2 = 0;         
         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
   }
   /*-------------------------------*/
   /* Prepare grid ranges for INSET */
   /*-------------------------------*/
   {
      fint     boxopt = 0;         /* The different options are: */

      dfault = REQUEST;
      gdsbox_c( blo, bhi, Setin, subin, &dfault, 
                KEY_BOX, MES_BOX, &showdev, &boxopt );
   }

   for (i = 0; i < setdim; i++)
   {
      if (i != axflag)
      {
         bloO[i] = blo[i];
         bhiO[i] = bhi[i];         
      }
      else
      {
         double    f = fabs(cdeltold)/fabs(cdeltnew);
         bloO[i] = NINT( (double) blo[i] * f );
         bhiO[i] = NINT( (double) bhi[i] * f );
      }     
   }

   
      
   /*--------------------------------------------------------------*/
   /* Assign 'gdsinp' buffer to 'gdsout'. Output set will get same */
   /* coordinate system as input INSET=.  GDSOUT is a function     */
   /* which prompts the user to enter the name of a set and        */
   /* (optionally) subset(s) and returns the number of subsets     */
   /* entered.                                                     */
   /*--------------------------------------------------------------*/
   gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
   gdscss_c( KEY_OUTSET, bloO, bhiO );
   {
      fint     pmask = ( 32 + 0 + 0 + 0 + 0 + 0 );
      fint     m = axflag + 1;
      fint     axcount = bhiO[axflag] - bloO[axflag] + 1;
      double   dummy;
      fchar    Dummy;
           
      fmake( Dummy, 1 );
      gdscpa_c( KEY_OUTSET,          /* Keyword associated with a GDSOUT call.*/
                &m,                  /* The axis number of the axis to be changed. */
                &axcount,            /* Size of the axis.*/
                &cdeltnew,           /* Increment in physical units along axis. */
                &dummy,
                &dummy,
                &dummy,
                Dummy,
                Dummy,
                &pmask );                      
   }
   dfault  = NONE;
   fmake( Setout, STRLEN );
   do 
   {
      nsubsout = gdsout_c( Setout,        /* Name of the output set. */
                           subout,        /* Output array with subsets coordinate words.*/
                           &nsubs,        /* Maximum number of subsets in subout. */
                           &dfault,       /* Default code as in USERxxx. */
                           KEY_OUTSET,    /* User keyword prompt. */
                           MES_OUTSET,    /* Message for the user. */
                           &showdev,      /* Device number (as in ANYOUT). */
                           axnumout,      /* Array of size 'maxaxes' containing the axes numbers. */
                           axcountout,    /* Array with the axis sizes. */
                           &maxaxes );    /* Max axes the program can deal with. */
      agreed = (nsubsout == nsubs);
      if (!agreed)
         reject_c( KEY_OUTSET, tofchar("#out != #in") );
   }
   while (!agreed);

   buflenI = bhi[axflag] - blo[axflag] + 1;
   imageI  = (float *) calloc( (int) buflenI, sizeof(float) );
   if (imageI == NULL) 
      errorC( 4, "Cannot allocate memory for input buffer!" );

   buflenO = bhiO[axflag] - bloO[axflag] + 1;
   imageO  = (float *) calloc( (int) buflenO, sizeof(float) );
   if (imageO == NULL) 
      errorC( 4, "Cannot allocate memory for output buffer!" );   

   ipoloption = menu();
   width = getsincwidth( ipoloption );

   elapse = 0;
   timer_c( &cputime, &realtime, &elapse );   /* Reset timer */
   process( Setin, Setout, 
            blo, bhi, 
            bloO, bhiO, 
            imageI, buflenI, 
            imageO, buflenO, 
            axflag,
            cdeltold,
            cdeltnew,
            ipoloption,
            width );  
   elapse = 1;
   timer_c( &cputime, &realtime, &elapse );   /* Get cpu seconds */
   
   anyoutf( 3, "-Task '%s' did a regrid along %s in %.2f sec (%.2f cpu sec)",
            taskname, axnames[axflag], realtime, cputime );

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/
   
   free( imageO );
   free( imageI );
   finis_c();
   return(EXIT_SUCCESS);                                      /* Dummy return */
}
