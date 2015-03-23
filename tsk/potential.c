/*
                            COPYRIGHT (c) 2001
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             potential.dc1
Program:       POTENTIAL

Purpose:       Program to calculate the 2-dim gravitational potential 
               map of a 2-dim image. This is evaluating the integral 
               given as Eq. 2-3 in the book Galactic Dynamics by 
               Binney and Tremaine. 

Category:      ANALYSIS, CALCULATION

File:          potential.c

Author:        M.G.R. Vogelaar
               Roelof Bottema (algorithm and documentation).

Keywords:

   INSET=      Give set, subsets:
              
               Maximum number of subsets is 1024.
               Input map for which you want to calculate the potential.


   BOX1=       Region for potential field:               [entire subset]
   
               Region in the input map for which you want to calculate
               the potential.
               

   BOX2=       Region to include in calculation:         [entire subset]
   
               The region of the input map which you want to include in
               the calculation of the potential.
               
              
   OUTSET=     Give output set (, subsets):
               
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input 
               subsets.
               Also, the size and attributes (header items)
               of the output map are equal to that of the input map.
       


Description:   Program to calculate the 2-dim gravitational potential map 
               of a 2-dim image. This is evaluating the integral given 
               as Eq. 2-3 Galactic Dynamics by Binney and Tremaine. The 
               integral is replaced by a sum over the pixel values in 
               the image of INSET=


                                     rho(p)
               Pot(P) = -G * SUM   ---------- dx dy dz
                                    | p - P |


               P, and p are vector positions, rho(p) is the space density 
               at position p. For the present application, rho(p) * dz 
               is replaced by the (surface density) value at pixel p. 
               dx and dy are pixel sizes which are taken to be equal to 
               1.0 and the distance |p-P| is measured in pixels. 
               Furthermore the constant -G is replaced with 1.0. 

               To calculate the physical value for the potential in 
               some units, you have to multiply the value of Pot 
               with a certain constant. Then you have to know your 
               pixel size in length units and actual value of the surface 
               density. In addition the gravitational constant G has to 
               be included. Since everybody uses his or her own units
               we leave this calculation to the user........ 

               Blanks or undefined values in the input map are replaced 
               with zeros. Positions outside BOX1 in the output map are 
               filled up with blanks.


Notes:         For large regions the calculation may take a while, 
               which gives you the opportunity to have a cup of coffee.


Example:       Calculate the 2-dim potential which would result for a
               photometric image of a galaxy. You first have to do a 
               proper sky subtraction and interpolation over any 
               irregularities or foreground stars. Project the image 
               to face on. Then use POTENTIAL. 


Updates:       Jun 14, 2001: VOG, Document created.


#<
*/

/*  potential.c: include files     */

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
#include    "stabar.h"       /* Status bar */

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
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )
#define PI             3.141592653589793
#define RAD(a)         ( (a) * 0.017453292519943295769237 )
#define DEG(a)         ( (a) * 57.295779513082320876798155 )

#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     1024       /* Max. allowed subsets */
#define MAXBUF         4096       /* Buffer size for I/O */
#define STRLEN         256        /* Max length of strings */
#define FILENAMELEN    256        /* Max length of file names */
#define FITSLEN        20         /* Max length of header items etc.*/
#define NONE           0          /* Default levels in userxxx routines */
#define REQUEST        1          
#define HIDDEN         2          
#define EXACT          4          
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0          

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX1       tofchar("BOX1=")
#define MES_BOX1       tofchar(" ")
#define KEY_BOX2       tofchar("BOX2=")
#define MES_BOX2       tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set (subset(s)): ")


/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered according to the */
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
static fint     blo1[MAXAXES];      /* Low  edge of box in grids */
static fint     bhi1[MAXAXES];      /* High edge of box in grids */
static fint     blo2[MAXAXES];  
static fint     bhi2[MAXAXES];
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */


/* Reading data */

static fint     maxIObuf = MAXBUF;  /* Maximum size of read buffer. */
static fint     subnr;              /* Counter for subset loop. */


/* OUTSET related variables */

static fchar    Setout;
static fint     subout[MAXSUBSETS];  /* Output subset coordinate words */
static fint     nsubsout;
static fint     axnumout[MAXAXES];
static fint     axcountout[MAXAXES];


/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[STRLEN];    /* All purpose character buffer. */
static int      i;                  /* Various counters. */
static bool     agreed = NO;        /* Loop guard. */
static float    minval[MAXSUBSETS]; /* Min. value of data for each subset. */
static float    maxval[MAXSUBSETS]; /* Max. value of data for each subset. */
static fint     nblanks[MAXSUBSETS];/* Number of blanks in each subset. */
static fint     change;             /* Used in WMINMAX. change!=0 means */
                                    /* minimum and maximum have changed and */
                                    /* that the MINMAX descriptors at */
                                    /* intersecting levels will be removed. */



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



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint     maxsubs = MAXSUBSETS;
   fint     maxaxes = MAXAXES;           /* Max num. of axes the program can deal with.*/
   fint     class   = 1;                 /* Class 1 is for applications which repeat */
   fint     showdev = 1;
   fint     mcount  = 0;                 /* Initialize MINMAX3 */
   fint     nsubs;                       /* Number of input subsets */
   fint     dfault;                      /* Default option for input etc */
   float    *imageI, *imageO;
   fint     imsize;
   float    stabarv[3];                  /* start-, end- and current value for progress bar */   
  
   init_c();                             /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;              /* Name of current task */
      fmake( Task, 20 );                 /* Macro 'fmake' must be available */
      myname_c( Task );                  /* Get task name */
      Task.a[nelc_c(Task)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, RELEASE ); /* Show task and version */
   }
   setfblank_c( &blank );

   /*--------------------------------------------------*/
   /* Get the input set. Documentation can be found in */
   /* $gip_sub/gdsinp.dc2                              */
   /*--------------------------------------------------*/
   {
      fmake( Setin, STRLEN );
      dfault  = NONE;
      subdim  = 2;                  /* Allow only 2-dim structures */
      nsubs = gdsinp_c( Setin,      /* Name of input set. */
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

   /*-------------------------------*/
   /* Determine edges of this frame */
   /*-------------------------------*/
   {
      fint cwlo, cwhi;                          /* Local coordinate words */
      int  m;
      r1 = 0;
      gdsc_range_c( Setin, &setlevel, &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++)
      {
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
      gdsbox_c( blo1, bhi1, Setin, subin, &dfault, 
                KEY_BOX1, MES_BOX1, &showdev, &boxopt );
   }

   /*----------------------------------*/
   /* Region to include in calculation */
   /*----------------------------------*/
   {
      fint     boxopt = 0;         /* The different options are: */

      dfault = REQUEST;
      gdsbox_c( blo2, bhi2, Setin, subin, &dfault, 
                KEY_BOX2, MES_BOX2, &showdev, &boxopt );
   }

   
   /*--------------------------------------------------------------*/
   /* Assign 'gdsinp' buffer to 'gdsout'. Output set will get same */
   /* coordinate system as input INSET=.  GDSOUT is a function     */
   /* which prompts the user to enter the name of a set and        */
   /* (optionally) subset(s) and returns the number of subsets     */
   /* entered.                                                     */
   /*--------------------------------------------------------------*/
   gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
   dfault  = NONE;
   fmake( Setout, STRLEN );
   do {
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

   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/


   imsize = (fhi[0]-flo[0]+1)*(fhi[1]-flo[1]+1);
   imageI = (float *) calloc( imsize, sizeof(float) );
   imageO = (float *) calloc( imsize, sizeof(float) );   
   stabarv[0] = stabarv[2] = 0.0;
   stabarv[1] = (float) (imsize*nsubs);    
   stabar_c( &stabarv[0], &stabarv[1], &stabarv[2] );      
   for(subnr = 0; subnr < nsubs; subnr++)
   {
      fint  tid = 0;               /* Transfer id's */
      fint  cwlo, cwhi;            /* Coordinate words */
      fint  pixelsread;            /* Number of pixels read by read routine. */
      fint  tidO = 0;
      fint  cwloO, cwhiO;
      fint  pixelswrite;           /* Number of pixels to write to output. */
      int   i;
      int   xlen = fhi[0]-flo[0]+1;
      int   row = 0;

      cwlo   = gdsc_fill_c( Setin, &subin[subnr], flo );
      cwhi   = gdsc_fill_c( Setin, &subin[subnr], fhi );
      /* Use input grid coordinates, but connect to output subsets */
      cwloO  = gdsc_fill_c( Setout, &subout[subnr], flo );
      cwhiO  = gdsc_fill_c( Setout, &subout[subnr], fhi );
      tid    = 0;

      /* Read imsize' values in 'image'. */
      gdsi_read_c( Setin,
                   &cwlo, &cwhi,
                   imageI,
                   &imsize,
                   &pixelsread,
                   &tid );
      for (i = 0; i < pixelsread; i++)
      {
         imageO[i] = blank; 
         /* Replace blanks by 0.0 */
         if (imageI[i] == blank)
         {
            imageI[i] = 0.0;
         }
      }
      /* Calculation loops */
      {
         int   i1, i2, j1, j2, k1, k2, l1, l2;
         int   i, j, k, l;
         
         i1 = blo1[0]; i2 = bhi1[0];
         j1 = blo1[1]; j2 = bhi1[1];      
         k1 = blo2[0]; k2 = bhi2[0];
         l1 = blo2[1]; l2 = bhi2[1];
      

         for (i = i1; i <= i2; i++)
         {
            for (j = j1; j <= j2; j++) 
            {
               double   sum = 0.0;
               for (k = k1; k <= k2; k++)
               {
                  for (l = l1; l <= l2; l++)  
                  {
                     double val = imageI[(l-flo[1])*xlen+(k-flo[0])];
                     double add;
                     if (k == i && l == j)
                     {
                        add = 3.54*val;
                     }
                     else
                     {
                        int d = (i-k)*(i-k) + (j-l)*(j-l);
                        add = val / (sqrt((double)d));
                     }
                     sum += add;
                  }
               }
               imageO[ (j-flo[1])*xlen+(i-flo[0]) ] = (float) sum;  
            }
            row++;
            stabarv[2] = (float) (subnr*imsize + row*xlen);
            /* Show progress to user */
            stabar_c( &stabarv[0], &stabarv[1], &stabarv[2] );            
         }
      }
      

      /* Calculate running min, max & blanks of output */
      minmax3_c( imageO,
                 &pixelsread,
                 &minval[subnr], &maxval[subnr],
                 &nblanks[subnr],
                 &mcount );
      pixelswrite = pixelsread;
      /* Write 'pixelswrite' values from 'imageI' to output. */
      gdsi_write_c( Setout,
                    &cwloO, &cwhiO,
                    imageO, 
                    &imsize,
                    &pixelswrite,
                    &tidO );
   }
   /* Update OUTSET= descriptor with new values */
   change = YES;
   wminmax_c( Setout, subout,
              minval, maxval, nblanks, 
              &nsubsout,
              &change );
   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   free( imageI );
   free( imageO );
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
