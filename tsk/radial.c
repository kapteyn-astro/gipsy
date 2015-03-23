/*
                            COPYRIGHT (c) 1999
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             radial.dc1

Program:       RADIAL

Purpose:       Derive radial surface density distribution from a 2-dim 
               total HI map using the 'Lucy method'.

Category:      ANALYSIS, FITTING, PROFILES, RADIO

File:          radial.c

Author:        M.G.R. Vogelaar
               (radprof.f by Rob Swaters)

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 1.

   POSANG=     Enter position angle major axis:                   [0.0]

   OUTPOS=     Enter rotation center in grids:                [0.0 0.0] 
   
   BOX=        Give box in .....                        [entire subset]
               Enter a box for the rotated set 
               (see description)

   TABLEFILE=  Enter output filename for densities:        [RADIAL.DAT]
   
   GUESS=      Enter initial estimate:                              [L]

               Enter one of: L = Linear Regression
                             E = Exponential Decreasing
                             G = Gaussian Distribution
                             F = Flat Distribution

   SCF=        Give scale factor for density:                       [1]


   SIGMA=      Give noise in your data points:                      [1]

   ITMAX=      Maximum number of iterations:                       [10]

   BEAM=       Half power beam width (arcsec):                    [0.0]
  
   GRDEVICE=   Plot device:                           [List of devices]
               Destination of plot, Screen or Hardcopy.


Description:   For edge on or highly inclined galaxies a radial surface 
               density distribution can be obtained by this program. it uses
               the method described in Warmels, R.H., 1988b, A&AS 72, 427
               The core of the method is based on the 'Lucy method'
               (The Astronomical Journal, Vol. 79, Number 6, June 1974)
               and it is written as a separate task called RADPROF which
               is called by RADIAL as if it is a subroutine. 
               You can use RADPROF independently but then you need
               to do the extra work which RADIAL does for you.

               RADIAL starts with 2-dim total HI set (INSET=) and uses data
               within user given limits (BOX=).
               It asks to enter a value for the angle (deg.) of the 
               major axis wrt. the north in POSANG=.
               RADIAL then rotates your set so that its major axis is 
               parallel to the direction of the east by calling task
               REPROJ. The rotation center is entered in OUTPOS=.
               The rotated output is stored in 'tempREPROJsetxxxxx' 
               and displayed in GIDS. Now it is rotated, its data can be 
               summed in the y-direction and the results of this process
               are written in two ASCII files on disk called radialEAST.dat
               and radialWEST.dat for both the east and west parts of the 
               (rotated) galaxy. These files are the basic input files for 
               the iterative Lucy method.
               
               Now RADPROF is called (deputied). It asks you to 
               enter a file name to store its results (strip distributions
               and surface densities as function of radius as a result
               of Lucy"s iterative solution scheme for a radial HI
               distribution). The method needs an initial estimate for
               the distribution. This is one of the following:
               L = Linear Regression
               E = Exponential Decreasing
               G = Gaussian Distribution
               F = Flat Distribution  
               The abbreviation is used in keyword GUESS=
               Further a scale factor is asked in SCF=,
               the noise in the data points in SIGMA=
               and the maximum number of iterations in ITMAX=
               Also the Full Width at Half Maximum is needed (BEAM=).
               This is a value in seconds of arc.
               Then the iteration starts and the results are written to 
               file and to a plot on the device that you selected in
               GRDEVICE=


Related doc:   radprof.dc1, radprof.f        


Notes:         RADIAL can only work together with RADPROF.

Example:       Example of used keywords and filenames:

               INSET= rob param 0
               POSANG= 57
               OUTPOS= -4.22 -4.96
               BOX= -41 -25 39 22                
               TABLEFILE=               
               GUESS=
               SCF= 
               SIGMA= 3
               ITMAX=
               BEAM= 3
               GRDEVICE= x11             
              
Updates:       Jul 30, 1999: VOG, Document created.

#<
*/

/*  radial.c: include files     */

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
#include    "minmax1.h"


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
#include    "gdsd_rchar.h"

/* Others */

#include    "wkey.h"         /* Write keywords to task's own parameter list */
#include    "deputy.h"       /* Start a task which temporarily assumes the */
                             /* role of the calling task. */
#include    "gds_delete.h"
#include    "subst.h"
#include    "listctrl.h"
#include    "matrix.h"       /* M[ylo..yhi][xlo..xhi] */


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
#define RAD(a)         ( a * 0.017453292519943295769237 )
#define DEG(a)         ( a * 57.295779513082320876798155 )

#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     1          /* Max. allowed subsets */
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
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")


#define TSK_REPROJ       tofchar("REPROJ")
#define TSK_RADPROF      tofchar("RADPROF")
#define TMPSET           "tempREPROJsetxxxxx"

/* Variables for input */

static fchar    Setin;              /* Name of input set */
static fint     subin[MAXSUBSETS];  /* Subset coordinate words */
static fint     TMPsubin[MAXSUBSETS]; 
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
 

/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[STRLEN];    /* All purpose character buffer. */
static int      i;                  /* Various counters. */
static bool     agreed = NO;        /* Loop guard. */


   

static void clearstr( fchar Fstr )
/*---------------------------------------------*/
/* Blank a Fortran string up to Len characters */
/*---------------------------------------------*/
{
   int    i;
   fint   len = Fstr.l;


   for (i = 0; i < (int) len; i++)
   {
      Fstr.a[i] = '\0';
   }
}



static void printstatus( fchar Tsk, fint status )
/*------------------------------------------------------------*/
/* PURPOSE: Display error for deputy call.                    */
/*------------------------------------------------------------*/
{
   if (status == -6)
      anyoutf( 1, "Called task (%.*s) not present", nelc_c(Tsk), Tsk.a );
   if (status == -7)
      anyoutf( 1, "Max. number of tasks already active" );
}


static void subsetcw2str( fchar   Setin,
                          fint    subset,
                          fint    *axnum,
                          char    *str )
/*------------------------------------------------------------*/
/* PURPOSE: Find expression to specify 2-dim subset.          */
/*------------------------------------------------------------*/
{
   int       i;
   fint      setdim;
   char      ctype[FITSLEN];
   fchar     Ctype;  
   fint      setlevel = 0; 
   char      mes1[STRLEN];
   char      mes2[STRLEN];   
   
   
   setdim = gdsc_ndims_c( Setin, &setlevel );
   mes1[0] = '\0';
   for (i = 2; i < setdim; i++)
   {
      char  *cptr;      
      fint  r1;
      fint  grid;

      Ctype.a = ctype; Ctype.l = FITSLEN; clearstr( Ctype );
      (void) sprintf( mes2, "CTYPE%d", axnum[i] );
      r1 = 0;      
      gdsd_rchar_c( Setin, tofchar(mes2), &setlevel, Ctype, &r1 );
      Ctype.a[nelc_c(Ctype)] = '\0';      
      cptr = strtok( Ctype.a, " -" );
      if (cptr != NULL) 
      {
         (void) sprintf( ctype, "%s", cptr );
      }
      r1 = 0;
      grid = gdsc_grid_c( Setin, &axnum[i], &subset, &r1 );
      sprintf( mes2, "%s %d ", ctype, grid );
      strcat( mes1, mes2 );
   }   
   strcpy( str, mes1 );
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
   fint     nitems;
   fint     r;
   fint     tid;
   fint     imagesize;
   fint     pixelsread;
   fint     cwlo, cwhi;
   float    pa;
   fint     row, col;
   float    **image = NULL;              /* 2-dim matrix with floats */
   fint     numsums = 0;
   fint     sumnr;
   float    *sums;                       /* Sum of image values. */   
   float    minval, maxval;
   float    *radii;
   fchar    Tmpset;
   char     viewmessage[STRLEN];            


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
 
   /* The data must be summed along the minor axis direction. */
   /* Therefore we have to rotate the image. Use the position angle */
   /* of the major axis to calculate the rotation angle for REPROJ. */
   
   pa = 0.0;
   nitems = 1;
   dfault = REQUEST;
   sprintf( message, "Enter position angle major axis (deg):   [%g]", pa );
   r = userreal_c( &pa, &nitems, &dfault, tofchar("POSANG="), tofchar(message) );
   anyoutf(1, "angle=%f", pa );
   
   /* Ask user for rotation center */
   {
      float  x[2];
      x[0] = x[1] = 0.0;
      nitems = 2;
      dfault = REQUEST;
      sprintf( message, "Enter rotation center in grids:   [%g %g]", x[0], x[1] );
      r = userreal_c( x, &nitems, &dfault, tofchar("OUTPOS="), tofchar(message) );
   }


   /* Prepare for call to REPROJ */
   {
      fint   status;
      fint   r = 0;
      
      r = 0; subst_c( tofchar("BOX=R_BOX="), &r );
      wkey_c( tofchar("R_BOX=") );
      wkey_c( tofchar("DEFSET=") );
      wkey_c( tofchar("ROTATEONLY=Y") );
/*      wkey_c( tofchar("OUTPOS=") );*/
      wkey_c( tofchar("CDELT=") );
      wkey_c( tofchar("OUTBOX=") );
      wkey_c( tofchar("SPEEDMAT=") );
      wkey_c( tofchar("DATAMODE=") );
      sprintf( message, "ROTANGLE=%f", 90.0-pa );
      wkey_c( tofchar(message) );
      r = 0; gds_delete_c ( tofchar(TMPSET), &r );
      sprintf( message, "OUTSET=%s", TMPSET );
      wkey_c( tofchar(message) );
      deputy_c( TSK_REPROJ, &status );
      if (status != 1)   
         printstatus( TSK_REPROJ, status );
   }


   /* Display the TMPSET so that the user can enter a box */
   {
      int       i;
      char      mes[STRLEN];
      fint      status;
      fint      r = 0;
      fint      hermesold, hermesnew;

      sprintf( viewmessage, "V_INSET=%s ", TMPSET );
      subsetcw2str( Setin, subin[0], axnum, mes ); 
      strcat( viewmessage, mes );
      wkey_c( tofchar(viewmessage) );   
               
      r = 0; subst_c( tofchar("CLIP=V_CLIP="), &r );
      r = 0; subst_c( tofchar("INSET=V_INSET="), &r );
      r = 0; subst_c( tofchar("BOX=V_BOX="), &r );     
      wkey_c( tofchar("V_BOX=") );
      wkey_c( tofchar("V_CLIP=") );
      hermesnew = 0;
      hermesold = listctrl_c( &hermesnew );
      deputy_c( tofchar("VIEW"), &status );
      (void) listctrl_c( &hermesold );

      /* Store tmp name and subset specification for use in gdsbox */
      fmake( Tmpset, STRLEN );      
      sprintf( Tmpset.a, "%s %s", TMPSET, mes );
   }
   
   /* Find subset coordinate word for subset of tmp set */
   /* (gdsbox needs subset coordinate word(s) */
   {
      fint     TMPaxnum[MAXAXES];    
      fint     TMPaxcount[MAXAXES];
      fint     TMPsubdim;
      
      dfault     = HIDDEN;
      TMPsubdim  = 2;            
      class      = 1;      

      nsubs = gdsinp_c( Tmpset,
                        TMPsubin,   
                        &maxsubs,   
                        &dfault,    
                        tofchar("TMPSET="),
                        tofchar("Enter temp. set: " ),
                        &showdev, 
                        TMPaxnum,
                        TMPaxcount,
                        &maxaxes,  
                        &class, 
                        &TMPsubdim );
      /*-------------------------------*/
      /* Prepare grid ranges for INSET */
      /*-------------------------------*/
      {
         fint     boxopt = 0;         /* The different options are: */

         dfault = REQUEST;
         gdsbox_c( blo, bhi, tofchar(TMPSET), &TMPsubin[0], &dfault, 
                   KEY_BOX, MES_BOX, &showdev, &boxopt );
      }
   }
   

   /* Allocate memory */
   numsums = (bhi[0] - blo[0] + 1);
   sums = (float *) calloc( numsums, sizeof(float) );
   if (!sums)
      errorf( 4, "Cannot allocate memory for sums array" );
   radii = (float *) calloc( numsums, sizeof(float) );
   if (!radii)
      errorf( 4, "Cannot allocate memory for radii array" ); 

   image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );
   if (!image)
      errorf( 4, "Cannot allocate memory for image" );
   imagesize = (bhi[0] - blo[0] + 1) * (bhi[1] - blo[1] + 1);   


   /* Read image data */
   cwlo = gdsc_fill_c( Tmpset, &TMPsubin[0], blo );
   cwhi = gdsc_fill_c( Tmpset, &TMPsubin[0], bhi );
   tid  = 0;
   gdsi_read_c( Tmpset,
                &cwlo, &cwhi,
                &image[blo[1]][blo[0]],
                &imagesize,
                &pixelsread,
                &tid );

   /* Integrate along minor axis (== y axis after REPROJ) */
   sumnr = 0;
   for (col = blo[0]; col <= bhi[0]; col++)
   {
      float   val;
      
      radii[sumnr] = (float) col;
      sums[sumnr] = 0.0;      
      for (row = blo[1]; row <= bhi[1]; row++)
      {
         val  = image[row][col];               
         if (val != blank)
         {
            sums[sumnr] += val;
         }                                                
      }      
      sumnr++;
   }
   minmax1_c( sums, &numsums, &minval, &maxval );

   /* Open */
   {
      FILE    *east, *west;
      fint    start;
      int     k;
      
      start = ABS(blo[0]); 

      east = fopen("radialEAST.dat", "w");
      if (east == NULL)
         errorf( 4, "Cannot open data file 'radialEAST.dat'" );
      west = fopen("radialWEST.dat", "w");
      if (west == NULL)
         errorf( 4, "Cannot open data file 'radialWEST.dat'" );
      for (k = start; k >= 0; k--)
         fprintf( east, "%f\n", sums[k] );
      for (k = start; k < numsums; k++)
         fprintf( west, "%f\n", sums[k] );         
      fclose( west );
      fclose( east );      
   }

                           
   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen */
   /* are closed, allocated memory is released,  and HERMES */
   /* is instructed to stop.                                */
   /*-------------------------------------------------------*/
   
   freefmatrix( image, blo[0], blo[1] );
   free( sums );
   free( radii );
   
   {
      fint   status;  
      int    num = MYMIN(ABS(blo[0]), ABS(bhi[0]) );
      
      wkey_c( tofchar("PA=0.0") );
      wkey_c( tofchar("INCL=0.0") );
      sprintf( message, "POS=0:%d;", num );      
      wkey_c( tofchar(message) );
      sprintf( message, "EAST=file(radialEAST.dat,1,1:%d)", num );
      wkey_c( tofchar(message) );
      sprintf( message, "WEST=file(radialWEST.dat,1,1:%d)", num );
      wkey_c( tofchar(message) );
      deputy_c( TSK_RADPROF, &status );
      if (status != 1)
         printstatus( TSK_RADPROF, status );
    }

   r = 0;
   gds_delete_c ( tofchar(TMPSET), &r );
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
