/*
                            COPYRIGHT (c) 1996
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             condit.dc1

Program:       CONDIT

Purpose:       Transfer values if test value within certain range.

Category:      MANIPULATION, TRANSFER

File:          condit.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give input set(,subsets) to copy:
   
               Maximum number of subsets is 2048. Values of INSET= 
               are copied to OUTSET= under conditions applied to
               data in MASKSET..


   BOX=        Give box in .....                        [entire subset]
   
               Limit copy actions to this region of the INSET=
               subset.
               

   MASKSET=    Give mask set (, subsets):
   
               CONDIT transfers values in two different modes:
               A: One MASKSET= subset works one all INSET= subsets
               B: All given MASKSET= subsets work pairwise on all
                  subsets of INSET=  For this option you need to enter 
                  as much subsets as you did with INSET=
                                                            
     
 **MASKBOX=    Give box in ....                          [previous box]
 
               The axis sizes of the subset box of MASKSET= have to be
               equal to the axis sizes of the subset box of INSET=
               If using comparable subsets (i.e. same axis names,
               same grids and origins), then this keyword is hidden 
               else unhidden.
                                            
  
   OUTSET=     Give output set (, subsets):

               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets. Usually the name of the set will suffice.


   RGMODE=     Mask range must Block or Transfer inset data?     [T]/B
   
               Transfer data from INSET= to OUTSET= if values in 
               MASKSET= are in range of RANGE= and RGMODE=T
               Block data from INSET= if values in MASKSET= are 
               in range of RANGE= and RGMODE=B
               
   
   RANGE=      Enter 'transfer' range in mask:             [All values]
               (if RGMODE=T)
               or:
               Enter 'block' range in mask: 
               (if RGMODE=B)
  
               Enter a range of values for MASKSET=  The first value
               must be less than the second value. If you enter one 
               value, then the second is copied from the first. 
               A value is within a range if it is greater than or equal 
               to the first value in RANGE= and less than or equal to 
               the second value in RANGE=
 

   BLMODE=     Blanks in mask will Transfer or Block data?       T/[B]
   
               Set an action if a blank in the mask is encountered. 
               If BLMODE=T then the INSET= value will be copied to
               the output set. If BLMODE=B then the INSET= value
               is blocked and a value equal to BLOCKVAL= is written 
               to the output set.
               

   BLOCKVAL=   Enter value to replace blocked values:          [blank]
   
               Values that are blocked are replaced by this in the
               output set. Note that blanks in the input set are 
               copied as blanks to the output. BLOCKVAL=  does not
               apply to this situation.



Description:   Copy values in INSET= to OUTSET= under certain 
               conditions. These conditions apply to data in a mask
               set called MASKSET=  First, you have to select whether 
               you want to use the RANGE= keyword to transfer data
               (RGMODE=T) or to block data (RGMODE=B) while copying
               INSET= 
               Then you enter the range with RANGE= 
               A value in MASKSET= that is greater than  or equal to
               the first value and smaller than or equal to
               the second value in RANGE= causes a transfer or a block
               (depending on RGMODE=) of a value in INSET=
               If MASKSET= contains blank values, then transfer
               or blocking is set by a separate keyword BLMODE=
               If (BLMODE=T), data in INSET- will be transferred if
               the value in MASKSET= is a blank else it will be blocked.
               At positions where blocking was applied, usually a 
               blank is copied. However, you can replace this value 
               

Notes:         

Example:       

Updates:       Mar 21, 1996: VOG, Document created.

#<
*/

/*  condit.c: include files     */

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
#include    "stabar.h"       /* Progress bar */


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
#define RAD(a)         ( a * 0.017453292519943295769237 )
#define DEG(a)         ( a * 57.295779513082320876798155 )

#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     2048       /* Max. allowed subsets */
#define MAXBUF         16*4096    /* Buffer size for I/O */
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
#define MES_INSET      tofchar("Give input set(,subsets) to copy:")
#define KEY_MASKSET    tofchar("MASKSET=")
#define MES_MASKSET    tofchar("Give mask set (, subsets):")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")
#define KEY_MASKBOX    tofchar("MASKBOX=")
#define MES_MASKBOX    tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set (subset(s)): ")
#define KEY_RGMODE     tofchar("RGMODE=")
#define MES_RGMODE     tofchar("Mask range must Block or Transfer inset data?    [T]/B")
#define KEY_RANGE      tofchar("RANGE=")
#define KEY_BLMODE     tofchar("BLMODE=")
#define MES_BLMODE     tofchar("Blanks in mask will Transfer or Block data?  T/[B]")
#define KEY_BLOCKVAL   tofchar("BLOCKVAL=")
#define MES_BLOCKVAL   tofchar("Enter value to replace blocked values:  [blank]" )


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
static fint     setdimX;
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
static float    image[MAXBUF];      /* Buffer for read routine. */
static float    imageX[MAXBUF];     /* Buffer for mask data */
static fint     subnr;              /* Counter for subset loop. */


/* OUTSET related variables */

static fchar    Setout;
static fint     subout[MAXSUBSETS];  /* Output subset coordinate words */
static fint     nsubsout;
static fint     axnumout[MAXAXES];
static fint     axcountout[MAXAXES];


/* XSET related variables */

static fchar    SetX;
static fint     subinX[MAXSUBSETS];  /* Output subset coordinate words */
static fint     nsubsX;
static fint     axnumX[MAXAXES];
static fint     axcountX[MAXAXES];
static fint     floX[MAXAXES];
static fint     fhiX[MAXAXES];
static fint     bloX[MAXAXES];
static fint     bhiX[MAXAXES];



/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static fint     r1, r2;             /* Result values for different routines. */
static char     message[STRLEN];    /* All purpose character buffer. */
static bool     agreed = NO;        /* Loop guard. */
static float    minval[MAXSUBSETS]; /* Min. value of data for each subset. */
static float    maxval[MAXSUBSETS]; /* Max. value of data for each subset. */
static fint     nblanks[MAXSUBSETS];/* Number of blanks in each subset. */
static fint     change;             /* Used in WMINMAX. change!=0 means */
                                    /* minimum and maximum have changed and */
                                    /* that the MINMAX descriptors at */
                                    /* intersecting levels will be removed. */




static float transfer( float  val,
                       float  valx,
                       char   blmod, 
                       char   rgmod, 
                       float  *range,
                       float  blockval )
/*------------------------------------------------------------*/
/* PURPOSE: Transfer the value 'val' or replace it by         */
/* 'blockval' if the mask 'valx' does not match the range     */
/* criteria                                                   */
/*------------------------------------------------------------*/
{
   if (val == blank)
      return( val );
   if (valx == blank)
   {
      if (blmod == 'B')
         return( blockval );
      else
         return( val );
   }
   if (rgmod == 'T')         /* Transfer mode */
   {
      if (valx >= range[0] && valx <= range[1])
         return( val );
      else 
         return( blockval );
   }
   else
   {
      if (valx >= range[0] && valx <= range[1])
         return( blockval );
   }
   return( val );
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
   fint     boxopt = 0;                  /* The different options are: */   
   fint     r;
   fint     totpixels;
   int      ok;
   int      equalgrids;
   int      i, m;                        /* Various counters. */
   fchar    RGmode;
   char     rgmode;
   fchar    BLmode;
   char     blmode;
   float    blockval; 
   float    range[2];
   float    stabarv[3];                  /* start-, end- and current */
                                         /* value for progress bar */



   init_c();                             /* contact Hermes */
   /* Task identification */
   {
      fchar    Task;                     /* Name of current task */
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
      fmake( Setin, FILENAMELEN );
      dfault  = NONE;
      subdim  = 0;                  /* Allow all dimensions. */
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
      dfault = REQUEST;
      gdsbox_c( blo, bhi, 
                Setin, subin, &dfault, 
                KEY_BOX, 
                MES_BOX, 
                &showdev, 
                &boxopt );
   }

   /* Prepare for GDSINP for second set */
   do
   {
      class  = 1;      
      fmake( SetX, FILENAMELEN );
      dfault  = NONE;
      nsubsX = gdsinp_c( SetX,       /* Name of input set. */
                         subinX,     /* Array containing subsets coordinate words. */
                         &nsubs,     /* Maximum number of subsets in 'subin'.*/
                         &dfault,    /* Default code as is USERxxx. */
                         KEY_MASKSET,/* Keyword prompt. */
                         MES_MASKSET,/* Keyword message for the user. */
                         &showdev,   /* Device number (as in ANYOUT). */
                         axnumX,     /* Array of size 'maxaxes' containing the axes numbers. */
                                     /* The first elements (upto the dimension of the subset) */
                                     /* contain the axes numbers of the subset, */
                                     /* the other ones contain the axes numbers */
                                     /* outside the subset ordered according to the */
                                     /* specification by the user. */
                         axcountX,   /* Number of grids on axes in 'axnum' */
                         &maxaxes,   /* Max. number of axes. */
                                     /* the operation for each subset. */
                         &class,     /* Class 1 is for applications which repeat */
                         &subdim );  /* Dimensionality must be same as input subset */
                         
      ok = (nsubsX == 1 || nsubsX == nsubs);
      if (!ok)
      {
         sprintf( message, "#subsets must be 1 or %d", nsubs );
         reject_c( KEY_MASKSET, tofchar(message) );
      }
   }
   while (!ok);
   setdimX  = gdsc_ndims_c( SetX, &setlevel );

   /*-------------------------------*/
   /* Determine edges of 'mask'     */
   /*-------------------------------*/
   {
      fint cwlo, cwhi;                          /* Local coordinate words */
      int  m;
      r1 = 0;
      gdsc_range_c( SetX, &setlevel, &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++)
      {
         floX[m] = gdsc_grid_c( SetX, &axnumX[m], &cwlo, &r1 );
         fhiX[m] = gdsc_grid_c( SetX, &axnumX[m], &cwhi, &r2 );
      }
   }


   /* Are input- and mask subsets comparable? */
   {
      int   m;
      equalgrids = YES;
   
      for (m = 0; m < subdim; m++)
         equalgrids = (equalgrids && floX[m] == flo[m] && fhiX[m] == fhi[m]);
   }
   
  
   /* Ask box for 'mask' */
   if (equalgrids)
   {
      for (m = 0; m < subdim; m++)
      {
         bloX[m] = blo[m]; 
         bhiX[m] = bhi[m];          
      }
      dfault = HIDDEN;
      boxopt = 6;      
   }
   else
   {
      /* Fill arrays with SIZES for GDSBOX */      
      for (m = 0; m < subdim; m++)
      {
         bhiX[m] = bhi[m] - blo[m] + 1;
      }
      dfault = REQUEST;
      /* GDSBOX has a special option for sizes */
      boxopt = (8 + 4);
   }


   /*-------------------------------*/
   /* Prepare grid ranges for mask  */
   /*-------------------------------*/
   gdsbox_c( bloX, bhiX, 
             SetX, subinX, &dfault, 
             KEY_MASKBOX, 
             MES_MASKBOX, 
             &showdev, 
             &boxopt );
   
 
   
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

   nitems      = 1;
   dfault      = REQUEST;
   rgmode      = 'T';       /* Default include when in mask range */ 
   fmake( RGmode, 1 );
   RGmode.a[0] = rgmode;
   RGmode.l    = 1;
   r = usercharu_c( RGmode, &nitems, &dfault, KEY_RGMODE, MES_RGMODE );
   rgmode = RGmode.a[0];
   
   nitems = 2;
   if (rgmode == 'T')
   {
      strcpy( message, "Enter 'transfer' range in mask:   [All values]" );
      dfault = REQUEST;      
   }
   else
   {
      strcpy( message, "Enter 'block' range in mask:" );
      dfault = EXACT;
   }      
   r = userreal_c( range, &nitems, &dfault, KEY_RANGE, tofchar(message) );
   if (rgmode == 'T')
   {
      if (r == 1)
         range[1] = range[0];
      if (r == 0)
      {
         range[0] = -1.0*FLT_MAX;
         range[1] =      FLT_MAX;
      }
   }   

   nitems      = 1;
   dfault      = REQUEST;
   blmode      = 'B';       /* Default block data if mask value is blank */
   fmake( BLmode, 1 );
   BLmode.a[0] = blmode;
   BLmode.l    = 1;
   r = usercharu_c( BLmode, &nitems, &dfault, KEY_BLMODE, MES_BLMODE );
   blmode = BLmode.a[0];

   nitems   = 1;
   dfault   = REQUEST;
   blockval = blank;
   r = userreal_c( &blockval, &nitems, &dfault, KEY_BLOCKVAL, MES_BLOCKVAL );   

   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/

   totpixels = 1;
   for(i = 0; i < subdim; i++) 
      totpixels *= (bhi[i] - blo[i] + 1);
   totpixels *= nsubs;

   stabarv[0] = 0.0;
   stabarv[1] = (float) totpixels;
   stabarv[2] = 0.0;
   stabar_c( &stabarv[0], &stabarv[1], &stabarv[2] );

   for(subnr = 0; subnr < nsubs; subnr++)
   {
      fint  tid = 0;               /* Transfer id's */
      fint  cwlo, cwhi;            /* Coordinate words */
      fint  pixelsread;            /* Number of pixels read by read routine. */
      fint  tidO = 0;
      fint  cwloO, cwhiO;
      fint  pixelswrite;           /* Number of pixels to write to output. */
      fint  tidX = 0;
      fint  cwloX, cwhiX;
      
          
      cwlo   = gdsc_fill_c( Setin, &subin[subnr], blo );
      cwhi   = gdsc_fill_c( Setin, &subin[subnr], bhi );
      /* Use input grid coordinates, but connect to output subsets */
      cwloO  = gdsc_fill_c( Setout, &subout[subnr], blo );
      cwhiO  = gdsc_fill_c( Setout, &subout[subnr], bhi );
            
      /* Data for mask set */
      if (nsubsX == 1)
         m = 0;
      else
         m = subnr;
      cwloX  = gdsc_fill_c( SetX, &subinX[m], bloX );
      cwhiX  = gdsc_fill_c( SetX, &subinX[m], bhiX );     
          
      tid = tidX = 0;
      do
      {
         /* Read 'maxIObuf' values in 'image'. */
         gdsi_read_c( Setin,
                      &cwlo, &cwhi,
                      image,
                      &maxIObuf,
                      &pixelsread,
                      &tid );
         /* Read mask values */
         gdsi_read_c( SetX,
                      &cwloX, &cwhiX,
                      imageX,
                      &maxIObuf,
                      &pixelsread,
                      &tidX );                      
                     
         for (i = 0; i < pixelsread; i++)
         {
            image[i] = transfer( image[i], 
                                 imageX[i], 
                                 blmode, 
                                 rgmode, 
                                 range, 
                                 blockval );
         } 
         /* Calculate running min, max & blanks of output */
         minmax3_c( image,
                    &pixelsread,
                    &minval[subnr], &maxval[subnr],
                    &nblanks[subnr],
                    &mcount );
         pixelswrite = pixelsread;
         /* Write 'pixelswrite' values from 'image' to output. */
         gdsi_write_c( Setout,
                       &cwloO, &cwhiO,
                       image, 
                       &maxIObuf,
                       &pixelswrite,
                       &tidO );
         stabarv[2] += (float) pixelsread;
         /* Show progress to user */
         stabar_c( &stabarv[0], &stabarv[1], &stabarv[2] );
      }
      while (tid != 0);      
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

   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
