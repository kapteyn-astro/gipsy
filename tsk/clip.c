/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                          All Rights Reserved.


#>             clip.dc1

Program:       CLIP

Purpose:       Blank values of input if they are outside range.

Category:      MANIPULATION

File:          clip.c

Author:        M. Vogelaar

Keywords:

    INSET=     Input set (and subsets). Maximum number of subsets
               is 2048.
              
    BOX=       Frame for input subsets.            [entire subset]

    OUTSET=    Output set and subset(s) for the result.            
               The number of output subsets is the same as the
               number of input subsets.
               
    RANGE=     Give range of levels to TRANSFER:
               Input consists of two values. The first value may 
               be greater than the second. See the description 
               for the correct use of this keyword.
               
**  CVAL=      Specify value to replace clipped data       [blank]

**  BVAL=      Give value to replace values outside box    [blank]


Description:   Transfer pixels of the input set to an output set
               if their amplitude fall in a user given range. 
               Replace other values by a blank or some other 
               selected value.
               
               Examples of the use of the RANGE= keyword:

               RANGE=2, 5
               (IF 2<value<5 THEN transfer ELSE value==>blank)

               RANGE=5, 2
               (IF 2<value<5 THEN value==>blank ELSE transfer)
                             
               At the RANGE= keyword, the values -INF and INF can 
               be input also. These values represent the minimum
               and maximum values of the current system.
               
               RANGE=5, INF
               (IF value>5 THEN transfer ELSE value==>blank)



Updates:       Mar 2,  1990: MV, Document created.
               Mar 21, 1991: MV, Rewritten in C.

#<

*/


#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "math.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"
#include "float.h"
#include "gdsinp.h"
#include "gdsc_ndims.h"
#include "setfblank.h"
#include "myname.h"
#include "anyout.h"
#include "nelc.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsbox.h"
#include "gdsc_fill.h"
#include "gdsi_read.h"
#include "reject.h"
#include "userreal.h"
#include "usercharu.h"
#include "cancel.h"
#include "gdsasn.h"
#include "error.h"
#include "gdsout.h"
#include "gdsi_write.h"
#include "stabar.h"
#include "wkey.h"
#include "minmax3.h"
#include "wminmax.h"
#include "getrange.h"
#include "initptr.h"
#include "insideptr.h"
#include "outsideptr.h"
#include "clipper.h"



#define AXESMAX    10
#define SUBSMAX    2048
#define MAXBUF     4096          /* Buffer size for I/O */
#define VERSION    "1.0"
#define NONE       0
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define BIGSTORE   80
#define false      0
#define true       1


/* Keywords and messages */

#define KEY_INSET         tofchar("INSET=")
#define MES_INSET         tofchar("Give set (, subsets) to clip: " )
#define KEY_BOX           tofchar("BOX=")
#define MES_BOX           tofchar(" ")
#define KEY_OUTSET        tofchar("OUTSET=")
#define MES_OUTSET        tofchar("Give output set (,subsets) ")      
#define KEY_RANGE         tofchar("RANGE=")
#define MES_RANGE         tofchar("Give range of levels to TRANSFER:")
#define KEY_CVAL          tofchar("CVAL=")
#define MES_CVAL          tofchar("Give value to replace clipped data")
#define KEY_BVAL          tofchar("BVAL=")
#define MES_BVAL          tofchar("Give value to replace values outside box")


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
                                                        
                                                                               
#define MYMAX(a,b) ((a) > (b) ? (a) : (b))                          
#define MYMIN(a,b) ((a) > (b) ? (b) : (a))



/* Input of set, subsets: */  

static fchar    Fsetin;                /* Name of the set */
static fint     Fsubin[SUBSMAX];       /* Array for the subset coordinate words */
static fint     FnsubsI;               /* Number of input subsets */
static fint     Fdfault;               /* Default option for input etc */
static fint     Faxnum[AXESMAX];       /* GDSINP axis numbers array */
static fint     Faxcount[AXESMAX];     /* GDSINP axis lengths array */
static fint     Fclass = 1;            /* Repeat operation for each subset */
static fint     Fsetdim;               /* Dimension of the set */
static fint     Fsubdim;               /* Dimension of the subset */
static fint     Fscrnum = 11;          /* Destination of log output */
static fint     Fmaxaxes  = AXESMAX;   /* Convert parameters to variables */
static fint     Fmaxsubs  = SUBSMAX;
static fint     FmaxIObuf = MAXBUF;
static int      subnr;                 /* Index of current subset */
static int      i, m;                  /* Counters */
static fint     Fwholeset = 0;         /* Indicate set level */


/* Output set related */

static fchar    Fsetout;               /* Name of the clipped output set */
static fint     Fsubout[SUBSMAX];      /* Array for the subset coordinate words */
static fint     FnsubsO;               /* Number of input subsets */
static fint     FaxnumO[AXESMAX];      /* GDSINP axis numbers array */
static fint     FaxcountO[AXESMAX];    /* GDSINP axis lengths array */
   

/* Input of area etc.:*/

static fint     Fcwlo, Fcwhi;          /* Coordinate words */
static fint     Bcwlo, Bcwhi;
static fint     FcwloO, FcwhiO;
static fint     FgridLO[AXESMAX];      /* Coordinate words for frame */
static fint     FgridHI[AXESMAX];
static fint     FgridLOO[AXESMAX];     /* Coordinate words for output frame */
static fint     FgridHIO[AXESMAX];
static fint     BgridLO[AXESMAX];      /* Coordinate words for box */
static fint     BgridHI[AXESMAX];
static fint     Fboxopt;               /* Input option for 'gdsbox' */


/* Data transfer: */

static fint     totpixels;            /* Total number of pixels in a subset */   
static fint     FtidIN, FtidOUT;       /* Transfer id's */
static float    imageIN[MAXBUF];       /* Contains data to be changed */   
static float    imageOUT[MAXBUF];      /* Contains changed data */   
static fint     Fmcount;               /* Initialize 'minmax3' function */
static float    datamin[SUBSMAX];      /* min, max in a subset */
static float    datamax[SUBSMAX];
static fint     Fnblanks[SUBSMAX];     /* Number of blanks in a subset */


/* 'stabar' related: */

static float    STBstart;              /* Start value = 0 */
static float    STBend;                /* Max number of pixels to examin */
static float    STBcurrent;            /* Index of current pixel */


/* Miscellaneous: */

static fint     Fnumitems;             /* Max. number of input items in userxxx routines */
static fint     Fr1, Fr2;              /* Results of userxxx routines */
static float    blank;                 /* Value of system blank */
static float    range[2];              /* User given data in/exclude range */
static float    clipreplace;           /* Replace exclude pixel by this rvalue */
static float    outboxreplace;         /* Replace pixels outside box by this value */
static fint     Fremove;               /* Remove inters. levs in 'wminmax'? */


void anyoutC( char *anystr )
/*
 *------------------------------------------------------------------------------
 * The C version of 'anyout' needs a C type string as argument only. The value
 * of Fscrnum is global.
 *------------------------------------------------------------------------------
 */
{
   anyout_c( &Fscrnum, tofchar( anystr ) );
}



MAIN_PROGRAM_ENTRY
{
 
   init_c();                               /* contact Hermes */
   /* Task identification */
   {        
      static fchar    Ftask;               /* Name of current task */
      fmake( Ftask, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Ftask );                   /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }
   
   setfblank_c( &blank );      
   fmake(Fsetin, BIGSTORE);
  /*
   *----------------------------------------------------------------------------
   * Because Fortran passes all arguments by reference, all C functions with
   * a Fortran equivalent must do this also (GIPSY programmers guide, 
   * Chapter 9).
   * If the subset dimension 'Fsubdim' is set to 0 before the call to 'gdsinp',
   * the input routine accepts subsets of arbitrary dimensions smaller than 
   * the dimension of the set, and it returns the dimension of the user created 
   * subset.
   *----------------------------------------------------------------------------
   */
   Fdfault = NONE;
   Fsubdim = 0;                     
   Fscrnum = 11;
   FnsubsI  = gdsinp_c( Fsetin, Fsubin, &Fmaxsubs, &Fdfault, KEY_INSET,
                       MES_INSET, &Fscrnum, Faxnum, Faxcount, &Fmaxaxes, 
                       &Fclass, &Fsubdim );
   Fsetdim  = gdsc_ndims_c( Fsetin, &Fwholeset );
  /*   
   *----------------------------------------------------------------------------
   * Determine the edges of this its frame (FgridLO/HI)
   *----------------------------------------------------------------------------
   */
   Fr1 = 0;
   (void) gdsc_range_c( Fsetin, &Fwholeset, &Fcwlo, &Fcwhi, &Fr1 );
   Fr1 = Fr2 = 0;
   for (m = 0; m < (int) Fsetdim; m++) {
      FgridLO[m] = gdsc_grid_c( Fsetin, &Faxnum[m], &Fcwlo, &Fr1 );
      FgridHI[m] = gdsc_grid_c( Fsetin, &Faxnum[m], &Fcwhi, &Fr2 );
   }

  /*
   *----------------------------------------------------------------------------
   * Prepare a box for INSET. Default is the entire subset.
   *----------------------------------------------------------------------------
   */
   Fdfault = REQUEST;
   Fboxopt = 0;
   for (i = 0; i < (int) Fsubdim; i++) BgridHI[i] = 1;            
   Fscrnum = 11;
   (void) gdsbox_c( BgridLO, BgridHI, Fsetin, Fsubin,
                    &Fdfault, KEY_BOX, MES_BOX, &Fscrnum, &Fboxopt );
   /* Count number of pixels in this substructure */
   totpixels = 1;
   /* For one subset */   
   for(m = 0; m < (int) Fsubdim; m++) totpixels *= (int) Faxcount[m];
  

   (void) gdsasn_c( KEY_INSET, KEY_OUTSET, &Fclass );
   fmake(Fsetout, BIGSTORE);
   Fdfault = NONE;
   do {
      FnsubsO = gdsout_c( Fsetout, Fsubout, &FnsubsI, &Fdfault, 
                          KEY_OUTSET, MES_OUTSET,
                          &Fscrnum, FaxnumO, FaxcountO, &Fmaxaxes );
      if (FnsubsO != FnsubsI) {
         (void) reject_c( KEY_OUTSET, tofchar("Subset(s) error") );
      }
   } while (FnsubsO != FnsubsI);   



   Fr1 = 0;
   (void) gdsc_range_c( Fsetout, &Fwholeset, &Fcwlo, &Fcwhi, &Fr1 );
   Fr1 = Fr2 = 0;
   for (m = 0; m < (int) Fsetdim; m++) {
      FgridLOO[m] = gdsc_grid_c( Fsetout, &FaxnumO[m], &Fcwlo, &Fr1 );
      FgridHIO[m] = gdsc_grid_c( Fsetout, &FaxnumO[m], &Fcwhi, &Fr2 );
   }
      
   Fdfault = NONE;
   getrange_c( range, &Fdfault, KEY_RANGE, MES_RANGE );


   Fdfault = HIDDEN;
   Fnumitems = 1;
   clipreplace = blank;
   Fr1 = userreal_c( &clipreplace, &Fnumitems, &Fdfault, 
                     KEY_CVAL, MES_CVAL );
                     
   Fdfault = HIDDEN;
   Fnumitems = 1;
   outboxreplace = blank;
   Fr1 = userreal_c( &outboxreplace, &Fnumitems, &Fdfault, 
                     KEY_BVAL, MES_BVAL );                    

    
   /* Initialize stabar */

   STBstart   = 0.0;
   STBend     = (float) FnsubsI * totpixels;
   STBcurrent = 0.0;
   (void) stabar_c( &STBstart, &STBend, &STBcurrent );
  
  /*
   *----------------------------------------------------------------------------
   * Loop over all specified subsets. 
   *----------------------------------------------------------------------------
   */   
   for(subnr = 0; subnr < (int) FnsubsI; subnr++) {
           
      static int   stilltowrite;
      static fint  Fptrbuf;
      static fint  Fptrcount;
      static fint  Fbufptr;
      static fint  Fnumout, Fnumin;
      static fint  Foldleft, Fnewleft;
      static fint  Findatptr;
      static fint  Fnuminreadbuf;

           
      
      /* Make coordinate words for these corners */   	
      Fcwlo   = gdsc_fill_c( Fsetin, &Fsubin[subnr], FgridLO );
      Fcwhi   = gdsc_fill_c( Fsetin, &Fsubin[subnr], FgridHI );
      Bcwlo   = gdsc_fill_c( Fsetin, &Fsubin[subnr], BgridLO );
      Bcwhi   = gdsc_fill_c( Fsetin, &Fsubin[subnr], BgridHI );
     /*
      *-----------------------------------------------------------------------
      * It is possible to clip an arbitrary subset in the data set
      * to just one output subset. The coordinate words will not 
      * correspond in this case, therefore we have to calculate 
      * the coordinate words of the output set seperately.
      *------------------------------------------------------------------------
      */
      FcwloO  = gdsc_fill_c( Fsetout, &Fsubout[subnr], FgridLOO );
      FcwhiO  = gdsc_fill_c( Fsetout, &Fsubout[subnr], FgridHIO );
      
      FtidIN  = FtidOUT = 0;      
      Fmcount   = 0;                                     /* Used in 'minmax3' */
      Fptrcount = 0;
      Findatptr = 0;
      Fnuminreadbuf = 0;
      stilltowrite = totpixels;  /* Number of pixels to examine for one subset */
      do {         
         /* Don't exceed the max. buffer storage and the number still to write */
         Fptrbuf = (fint) MYMIN( FmaxIObuf, stilltowrite );
         /* On exit ptrcount contains number of points in pointer buffer */
         (void) initptr_c( FgridLO, FgridHI, BgridLO, BgridHI,
                           &Fsubdim, &Fptrbuf, &Fptrcount );


         while ( outsideptr_c(&Fbufptr, &Fnumout) ) {
            /* Every pixel outside the box gets replace value */
            for (i = 0; i < (int) Fnumout; i++) {
               imageOUT[Fbufptr+i] = outboxreplace;
            }
         }

         while ( insideptr_c(&Fbufptr, &Fnumin) ) {
            if ((Findatptr+Fnumin) <= Fnuminreadbuf) {
               /* All the pixels to examine are already in the read buffer */
               Foldleft = Fnumin;
               Fnewleft = 0;
            }
            else {
              /* There are more pixels in the pointer buffer than the */
              /* read buffer, so split the action in pixels that can be */
              /* clipped directly and pixels that has to be read first and */
              /* than be clipped */
              
              Foldleft = Fnuminreadbuf - Findatptr;
              Fnewleft = Fnumin - Foldleft;
            }
            /* Do the clipping */
            
            (void) clipper_c( &range[1], &range[0], 
                              &imageIN[ Findatptr ], 
                              &imageIN[ Findatptr ], 
                              &imageOUT[ Fbufptr ], 
                              &Foldleft, &clipreplace );

            Findatptr += Foldleft;
            if (Fnewleft > 0) {
               (void) gdsi_read_c( Fsetin, &Bcwlo, &Bcwhi, imageIN,
                                   &FmaxIObuf, &Fnuminreadbuf, &FtidIN );
               Findatptr = 0;
               (void) clipper_c( &range[1], &range[0],
                                 &imageIN[ Findatptr ], 
                                 &imageIN[ Findatptr ], 
                                 &imageOUT[ Fbufptr+Foldleft ], 
                                 &Fnewleft, &clipreplace );
               Findatptr = Fnewleft; /* Reset input data buffer to this value */
            }
         }
         (void) minmax3_c( imageOUT, 
                           &Fptrbuf, 
                           &datamin[subnr], &datamax[subnr], 
                           &Fnblanks[subnr], 
                           &Fmcount );
                           
         STBcurrent += (float) Fptrbuf;         
         (void) stabar_c( &STBstart, 
                          &STBend, 
                          &STBcurrent );            /* Show progress to user */
                                                   
         (void) gdsi_write_c( Fsetout, 
                              &FcwloO, &FcwhiO, 
                              imageOUT,
                              &Fptrbuf, 
                              &Fptrbuf, 
                              &FtidOUT );         
         stilltowrite -= (int) Fptrbuf;                             
      } while (stilltowrite > 0);
   }                                                    /* All subsets done? */
   
   /* Remove in 'WMINMAX' old minmax descriptors because program */
   /* changes values! */
   Fremove = true;   
   (void) wminmax_c( Fsetout, Fsubout, datamin, datamax, Fnblanks,
                     &FnsubsO, &Fremove );
   finis_c();                                                  /* Quit Hermes */
}
            

