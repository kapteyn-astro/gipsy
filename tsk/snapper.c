/*
                            COPYRIGHT (c) 1996
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             snapper.dc1

Program:       SNAPPER

Purpose:       Replace values by a box with other values e.g.
               for removing a bad perimeter of a velocity field.

Category:      MANIPULATION, UTILITY, VELOCITY-FIELDS

File:          snapper.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give input set (,subsets) to process:
   
               Maximum number of subsets is 2048.
               This set will be transformed to OUTSET= after replacing
               certain values by other values.


   BOX=        Give box in .....                        [entire subset]
   
               Restrict operations to data within these limits.


   OUTSET=     Give output set (, subsets):
   
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets. 


   OLDVAL=     Which map value do you want to replace?          [blank]

               All values OLDVAL= in the input set will be replaced 
               by NEWVAL= Also its neighbours in a box with size 
               REPSIZE= get the new value.


   NEWVAL=     What is the new value?                           [blank]
   
               This is the new value in OUTSET= which replaces
               OLDVAL= in the input set.

   
   REPSIZE=    Size of replace box:                               [3 3]
   
               Not only the pixel with OLDVAL= is replaced but also 
               its neighbours in a box with sizes in x and y direction
               as defined in this keyword. If one selects REPSIZE=1 1 
               then you have the functionality of program CBLANK.

                          
Notes:         .......

Example:       .......

Description:   Suppose you have a velocity field with a perimeter defined 
               by a blank and a non-blank pixel and the values in your 
               perimeter are results of a poor fit. To remove such values 
               by hand can take a long time.
               This program searches for values given in OLDVAL= and replaces 
               them by values entered in NEWVAL= in a box in the output set
               with sizes entered in REPSIZE= and centered at the position
               of the corresponding value in the input set. 
               In the default configuration,
               this program replaces one blank by 9 blanks in a box with 
               size 3 x 3 pixels, thereby effectively 'eating' the blank/
               non-blank perimeter of any object in the input map.


Updates:       Sep 01, 1998: VOG, Document created.

#<
*/

/*  snapper.c: include files     */

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
#include    "matrix.h"


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
#define SWAP(a,b)      { double temp=(a);(a)=(b);(b)=temp; } /* Swap 2 numbers */


#define RELEASE        "1.0"      /* Version number */
#define MAXAXES        10         /* Max. axes in a set */
#define MAXSUBSETS     2048       /* Max. allowed subsets */
#define MAXBUF         4096       /* Buffer size for I/O */
#define OPERATIONS     8
#define MAXOPERATIONS  32
#define STRLEN         256        /* Max length of strings */
#define FILENAMELEN    256        /* Max length of file names */
#define FITSLEN        20         /* Max length of header items etc.*/
#define NONE           0          /* Default levels in userxxx routines */
#define REQUEST        1          
#define HIDDEN         2          
#define EXACT          4          
#define YES            1          /* C versions of .TRUE. and .FALSE. */
#define NO             0          

#define TRANS          1
#define ROTATION       2
#define SCALING        3
#define REFLECTX       4
#define REFLECTY       5
#define REFLECTCEN     6
#define XSHEAR         7
#define YSHEAR         8

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (,subsets) to process:")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set (subset(s)): ")


typedef struct 
{
   fchar        Name;                /* Set name */
   fint         setdim;              /* Dimensions of set */
   fint         subdim;              /* Dimensions of subset(s) */
   fint         subset[MAXSUBSETS];  /* Subset array */
   fint         nsubs;               /* Number of subsets */
   fint         axnum[MAXAXES];      /* Array of size MAXAXES containing the */
                                     /* axes numbers. The first elements */
                                     /* (upto the dimension of the subset) */ 
                                     /* contain the axes numbers of the */
                                     /* subset, the other ones ontain the */
                                     /* axes numbers outside the */
                                     /* the subset ordered according to the */
                                     /* specification by the user. */
   fint         axcount[MAXAXES];    /* Array of size MAXAXES containing the */
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
   fint         blo[MAXAXES];        /* Lower left of box */
   fint         bhi[MAXAXES ];       /* Upper right of box */
   fint         class;               /* Usually 1 (repeat operations for subsets */
   float      **image;               /* Data buffer for this set */
} set;                                        
                             


/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */





replacepixelbybox( float    **Mi,
                   float    **Mo,
                   fint      *blo,
                   fint      *bhi,
                   float      oldvalue,
                   float      newvalue,
                   int        Xsize,
                   int        Ysize ) 
/*------------------------------------------------------------------*/
/* PURPOSE: Replace in matrix Mi all 'oldvalue' by Xsize x Ysize    */
/*          'newvalue'.                                             */
/*------------------------------------------------------------------*/
{
   int   x, y;


   /* Copy first */
   for (y = blo[1]; y <= bhi[1]; y++)
   {
      for (x = blo[0]; x <= bhi[0]; x++)
      {
         Mo[y][x] = Mi[y][x];
      }
   }
            
              
   for (y = blo[1]; y <= bhi[1]; y++)
   {
      for (x = blo[0]; x <= bhi[0]; x++)
      {
         if (Mi[y][x] == oldvalue)
         {
            int ix, iy;
            for (iy = -Ysize/2; iy <= Ysize/2; iy++)
            {
               for (ix = -Xsize/2; ix <= Xsize/2; ix++)
               {
                  int xx = x + ix;
                  int yy = y + iy;
                  /* Whitin box? */
                  if (xx >= blo[0] && xx <= bhi[0] &&
                      yy >= blo[1] && yy <= bhi[1] )
                     Mo[yy][xx] = newvalue;
               }
            }
         }
      }
   }
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
   fint     showdev = 1;
   fint     dfault;                      /* Default option for input etc */
   fint     nitems;
   fint     blo[MAXAXES];                /* Low  edge of box in grids */
   fint     bhi[MAXAXES];                /* High edge of box in grids */       
   fint     imagesize;                   /* Counters */
   fint     subnr;                       /* Counter for subset loop. */      
   fint     r1;
   set      inset, outset;
   float    oldval, newval;
   int      matrixxsize, matrixysize;
   int      i, agreed;
   

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

   fmake( inset.Name, STRLEN );
   dfault        = NONE;
   inset.subdim  = 2;                    /* Allow only 2-dim structures */
   inset.class   = 1;
   inset.nsubs   = gdsinp_c( 
                      inset.Name,        /* Name of input set. */
                      inset.subset,      /* Array containing subsets coordinate words. */
                      &maxsubs,          /* Maximum number of subsets in 'subin'.*/
                      &dfault,           /* Default code as is USERxxx. */
                      KEY_INSET,         /* Keyword prompt. */
                      MES_INSET,         /* Keyword message for the user. */
                      &showdev,          /* Device number (as in ANYOUT). */
                      inset.axnum,       /* Array of size 'maxaxes' containing the axes numbers. */
                                         /* The first elements (upto the dimension of the subset) */
                                         /* contain the axes numbers of the subset, */
                                         /* the other ones contain the axes numbers */
                                         /* outside the subset ordered according to the */
                                         /* specification by the user. */
                      inset.axcount,     /* Number of grids on axes in 'axnum' */
                      &maxaxes,          /* Max. number of axes. */
                                         /* the operation for each subset. */
                      &(inset.class),    /* Class 1 is for applications which repeat */
                      &(inset.subdim) ); /* Dimensionality of the subsets for class 1 */

   inset.setdim  = gdsc_ndims_c( inset.Name, &setlevel );


   /*-------------------------------*/
   /* Prepare grid ranges for INSET */
   /*-------------------------------*/
   {
      fint     boxopt = 0;          /* The different options are: */
                                    /*  1 box may exceed subset size */
                                    /*  2 default is in BLO */
                                    /*  4 default is in BHI */
                                    /*  8 box restricted to size defined in BHI*/
                                    /*  These codes work additive.*/
                                    /*  When boxopt is 0 or 1, the default is the */
                                    /*  is the entire subset. */      

      dfault = REQUEST;
      gdsbox_c( inset.blo, inset.bhi, 
                inset.Name, 
                inset.subset,
                &dfault, 
                KEY_BOX, MES_BOX, 
                &showdev, 
                &boxopt );
   }
   
   for (i = 0; i < inset.subdim; i++)
   {
      blo[i] = inset.blo[i];
      bhi[i] = inset.bhi[i];
   }
   
   /*--------------------------------------------------------------*/
   /* Assign 'gdsinp' buffer to 'gdsout'. Output set will get same */
   /* coordinate system as input INSET=.  GDSOUT is a function     */
   /* which prompts the user to enter the name of a set and        */
   /* (optionally) subset(s) and returns the number of subsets     */
   /* entered.                                                     */
   /*--------------------------------------------------------------*/
   gdsasn_c( KEY_INSET, KEY_OUTSET, &(inset.class) );
   dfault  = NONE;
   fmake( outset.Name, STRLEN );
   do 
   {
      outset.nsubs = gdsout_c( 
                        outset.Name,    /* Name of the output set. */
                        outset.subset,  /* Output array with subsets coordinate words.*/
                        &(inset.nsubs), /* Maximum number of subsets in subout. */
                        &dfault,        /* Default code as in USERxxx. */
                        KEY_OUTSET,     /* User keyword prompt. */
                        MES_OUTSET,     /* Message for the user. */
                        &showdev,       /* Device number (as in ANYOUT). */
                        outset.axnum,   /* Array of size 'maxaxes' containing the axes numbers. */
                        outset.axcount, /* Array with the axis sizes. */
                        &maxaxes );     /* Max axes the program can deal with. */
      agreed = (outset.nsubs == inset.nsubs);
      if (!agreed)
         reject_c( KEY_OUTSET, tofchar("#out != #in") );
   }
   while (!agreed);


   /* Ask user which value he wants to replace by which (other) value */
   nitems = 1;
   dfault = REQUEST;
   oldval = blank;
   r1 = userreal_c( &oldval, &nitems, &dfault, tofchar("OLDVAL="),
                    tofchar("Which map value do you want to replace? [blank]") );
   newval = blank;
   r1 = userreal_c( &newval, &nitems, &dfault, tofchar("NEWVAL="),
                    tofchar("What is the new value?    [blank]") );
   
   {
      fint  size[2];
      nitems = 2;
      size[1] = size[0] = 3;
      r1 = userint_c( size, &nitems, &dfault, tofchar("REPSIZE="),
                      tofchar("Size of replace box:    [3 3]") );
      matrixxsize = size[0];
      matrixysize = size[1];
   }
                      
   /* Prepare the data buffers for input and output data */
    
   inset.image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );
   if (!inset.image)
      errorf( 4, "Cannot allocate memory for input image buffer!" );
   imagesize = (bhi[0] - blo[0] + 1) * (bhi[1] - blo[1] + 1);
 
   outset.image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );
   if (!outset.image)
      errorf( 4, "Cannot allocate memory for output image buffer!" );


   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/
   for(subnr = 0; subnr < inset.nsubs; subnr++)
   {
      fint  tid = 0;               /* Transfer id's */
      fint  cwlo, cwhi;            /* Coordinate words */
      fint  pixelsread;            /* Number of pixels read by read routine. */
      fint  tidO = 0;
      fint  cwloO, cwhiO;
      fint  pixelswrite;           /* Number of pixels to write to output. */

      cwlo   = gdsc_fill_c( inset.Name, &(inset.subset[subnr]), blo );
      cwhi   = gdsc_fill_c( inset.Name, &(inset.subset[subnr]), bhi );
      
     /* Use input grid coordinates, but connect to output subsets */
           
      cwloO  = gdsc_fill_c( outset.Name, &(outset.subset[subnr]), blo );
      cwhiO  = gdsc_fill_c( outset.Name, &(outset.subset[subnr]), bhi );
      tid    = 0;

      /* Read all subset values in 'image'. */
      gdsi_read_c( inset.Name,
                   &cwlo, &cwhi,
                   &(inset.image[blo[1]][blo[0]]),
                   &imagesize,
                   &pixelsread,
                   &tid );
                   
      replacepixelbybox( inset.image, outset.image, 
                         blo, bhi, 
                         oldval, newval,
                         matrixxsize, matrixysize );

      pixelswrite = pixelsread;

      /* Write 'pixelswrite' values from 'image' to output. */
      gdsi_write_c( outset.Name,
                    &cwloO, &cwhiO,
                    &(outset.image[blo[1]][blo[0]]), 
                    &imagesize,
                    &pixelswrite,
                    &tidO );
   }

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   freefmatrix( inset.image, blo[0], blo[1] );
   freefmatrix( outset.image, blo[0], blo[1] );
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
