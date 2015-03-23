/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                          All Rights Reserved.


#>             enhance.dc1

Program:       ENHANCE

Purpose:       Enhance image  (Local enhancement method)

Category:      MANIPULATION

File:          enhance.c

Author:        M. Vogelaar

Keywords:

    INSET=     Input set (and subsets). Maximum number of subsets
               is 2048. The subsets must be 2 dimensional.
              
    BOX=       Frame for input subsets.            [entire subset]

    SETOUT=    Output set and subset(s) for the result.            
               The number of output subsets is the same as the
               number of input subsets.
               
    RANGE=     Give range of levels to work on.       [All levels]
               Outside range data will be transferred.
               INF and -INF are accepted as input.
               
    EBOX=      Give sizes of local enhancement box:          [3,3]
               Determine local two dimensional array in which the
               local average and standard deviation is calculated.
               
    WEIGHTS=   Use calculated weights?                         [N]              
               To determine the average in a small box, data can
               be weighted by a simple weighting function.
               If WEIGHTS=N all weights are equal to 1.
                              
    REPLACE=   Replace values outside box?                   Y/[N]
               If selected box is smaller than the subset, you can
               choose what to do with the values outside the box.
               If REPLACE=N the original data will be substituted, 
               else the keyword BVAL= will be asked.

    BVAL=      Value to replace values outside box:        [blank]
    
    GRANGE=    Give range for local          [all values possible]
               gain factor:
               The first value must be less than or equal to the 
               second one. INF and -INF are accepted as input.
               
    AVERAGE=   Give average value of selected box:    [calculated]
               If an average is specified, its value will be used
               as global mean in all subsets. If carriage return
               is pressed, then for each subset the average will 
               be calculated.


Description:   With keyword 'EBOX=' a n x m neighborhood is 
               defined and the center of this area is moved from 
               valid pixel to valid pixel in your input. 
               A valid pixel is a pixel with a non-blank value
               within a user given range (RANGE=).
               The range in EBOX sizes is limited. The values
               must be odd, greater than or equal to 1 and 
               their product must be less than 400. The height of
               the EBOX is limited to the maximum number of 
               complete lines that can be read in the buffer at 
               once.
               
               For each point g(x,y) in a new set the following 
               transformation is applied:

               g(x,y) = A(x,y)*[ f(x,y) - m(x,y) ] + m(x,y)
               
               g(x,y)   = New value at position x, y
               A(x,y)   = M / sig(x,y)
               M        = Global mean of f(x,y) calculated in 
                          selected box.
               sig(x,y) = Standard deviation of data in small 
                          box centered around x,y.
               f(x,y)   = Old value at x,y.
               m(x,y)   = Average of data in small box around
                          x,y.

               The local average can be a weighted or an un-
               weighted average (WEIGHTS=N).
               The global mean can be determined by the 
               application, but an user given value (AVERAGE=)
               is accepted also.
               
                          
               The local gain factor A(x,y) amplifies local 
               variations. Since it is inversely proportional
               to the standard deviation of the intensity, areas
               with low contrast receive larger gain ( Gonzalez &
               Wintz, Digital image processing, section 4.2.4, 
               2nd ed.). To restrict variations in the local gain 
               A(x,y), it is possible (GRANGE=) to select a range
               in order to balance large areas of intensity 
               in isolated regions. The minimum and maximum value
               of the gain can be obtained by running the program 
               without specifying GRANGE=

Example:       
               <USER >enhance
               <USER >INSET=spiral
               Set SPIRAL has 2 axes
               RA                 from  -207 to   207
               DEC                from  -236 to   236
               <USER >BOX=
               BOX range for set SPIRAL :
               RA                 from  -207 to   207
               DEC                from  -236 to   236
               <USER >OUTSET=spiralout
               Set SPIRALOUT has 2 axes
               RA                 from  -207 to   207
               DEC                from  -236 to   236
               <USER >RANGE=
               <USER >EBOX=
               <USER >WEIGHTS=y
               <USER >AVERAGE=

               Local enhancement in 3 x 3 neighborhood:
               Subset index            = 0
               Name of input set       = SPIRAL
               Name of output set      = SPIRALOUT
               Average in selected box = 0.675860
               Min. value new data     = -0.248401
               Max. value new data     = 1.685447
               Min. value gain         = 1.564082
               Max. value gain         = 14.561221

               <STATUS> enhance -  +++ FINISHED +++


                                         
Updates:       Apr 20,  1990: MV, Document created.

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
#include "userlog.h"
#include "userint.h"
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
#include "mover.h"
#include "status.h"


#define AXESMAX    10
#define SUBSMAX    2048
#define MAXBUF     8*4096          /* Buffer size for I/O */
#define MAXLOCAL   20*20           /* Max size local matrix */
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
#define MES_INSET         tofchar("Give set (, subsets) to enhance: " )
#define KEY_BOX           tofchar("BOX=")
#define MES_BOX           tofchar(" ")
#define KEY_OUTSET        tofchar("OUTSET=")
#define MES_OUTSET        tofchar("Give output set (,subsets) ")      
#define KEY_RANGE         tofchar("RANGE=")
#define MES_RANGE         tofchar("Give range of levels for enhancing:  [all]")
#define KEY_BVAL          tofchar("BVAL=")
#define MES_BVAL          tofchar("Value to replace values outside box [blank]")
#define KEY_REPLACE       tofchar("REPLACE=")
#define MES_REPLACE       tofchar("Replace values outside box?  Y/[N]")
#define KEY_EBOX          tofchar("EBOX=")
#define MES_EBOX          tofchar("Give sizes of local enhancement box:    [3,3]")
#define KEY_WEIGHTS       tofchar("WEIGHTS=")
#define MES_WEIGHTS       tofchar("Use calculated weights?         [N]")
#define KEY_AVERAGE       tofchar("AVERAGE=")
#define MES_AVERAGE       tofchar("Give average in selected box:  [calculated]")
#define KEY_GRANGE        tofchar("GRANGE=")
#define MES_GRANGE        tofchar("Range for local gain factor:  [all values possible]")


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

static fint     totpixels;             /* Total number of pixels in a subset */   
static fint     FtidIN, FtidOUT;       /* Transfer id's */
static float    imageIN[MAXBUF];       /* Contains data to be changed */   
static float    imageOUT[MAXBUF];      /* Contains changed data */   
static fint     Fmcount;               /* Initialize 'minmax3' function */
static float    datamin[SUBSMAX];      /* min, max in a subset */
static float    datamax[SUBSMAX];
static fint     Fnblanks[SUBSMAX];     /* Number of blanks in a subset */


/* 'stabar' related: */

static float    STBstart;              /* Start value = 0 */
static float    STBend;                /* Max number of pixels to examine */
static float    STBcurrent;            /* Index of current pixel */


/* Miscellaneous: */

static fint     Fnumitems;             /* Max. number of input items in userxxx routines */
static fint     Fr1, Fr2;              /* Results of userxxx routines */
static float    blank;                 /* Value of system blank */
static float    range[2];              /* User given data in/exclude range */
static float    outboxreplace;         /* Replace pixels outside box by this value */
static fint     Fremove;               /* Remove inters. levs in 'wminmax'? */
static fint     Freplace;              /* Replace values outside box by a constant? */
static char     messbuf[BIGSTORE];     /* Common buffer for messages */
static int      agreed;                /* Loop guard */
static fint     Febox[2];              /* Size of box defining local neighborhood */
static fint     Fweighting;            /* Must weighting be applied? */
static int      maxYlines;             /* Max. height of enhancement box */
static float    boxaverage;            /* Global mean M from described formula */
static fint     Fenhanced;             /* Number of enhanced input pixels */
static float    gainrange[2];          /* Limit variations in the gain factor */
static float    gainminmax[2];         /* Min. and max. gain factor used */
static int      usermean;              /* Specify or calculate global mean(s) */


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



int insidebox( int x, int y )
/*
 *------------------------------------------------------------------------------
 * Is position inside or outside specified box? 
 *------------------------------------------------------------------------------
 */
{
   if ( x >= BgridLO[0] && x <= BgridHI[0] && 
        y >= BgridLO[1] && y <= BgridHI[1] ) return true;
   else return false;
}



int inrange( float value, float *range )
/*
 *------------------------------------------------------------------------------
 * Check whether value of pixel is within user given range. 
 *------------------------------------------------------------------------------
 */
{
   if (range[0] < range[1]) {
      if (value >= range[0] && value <= range[1]) return true;
      else return false;
   }
   else {
      if (value < range[1] || value > range[0]) return true;
      else return false;
   }
}



float average( fint Bcwlo, fint Bcwhi, int *usermean )
/*
 *------------------------------------------------------------------------------
 * User can give an average here or press carriage return to calculate the
 * average in the selected box.
 *------------------------------------------------------------------------------
 */
{
   static fint   FtidIN;
   static fint   Fnuminreadbuf;
   static int    validpixels = 0;
   static float  blank;
   static float  totalsum = 0.0; 
   static fint   Fdfault;
   static fint   Fnumitems;
   static float  average;


   Fdfault = REQUEST;
   Fnumitems = 1;
   Fr1 = userreal_c( &average, &Fnumitems, &Fdfault,
                     KEY_AVERAGE, MES_AVERAGE );    
   cancel_c( KEY_AVERAGE );
   if (Fr1 != 0) {
      *usermean = true;
      return average;
   }
   else {               
      *usermean = false;      
      setfblank_c( &blank );         
      status_c( tofchar("Calculating average in box") );
      do {
         (void) gdsi_read_c( Fsetin, 
                             &Bcwlo, &Bcwhi, 
                             imageIN,
                             &FmaxIObuf, 
                             &Fnuminreadbuf, 
                             &FtidIN );
                          
         for (i = 0; i < (int) Fnuminreadbuf; i++) {
            if (imageIN[i] != blank) {
               totalsum += imageIN[i];
               validpixels++;
            }
         }
      } while ( FtidIN != 0 ); 
      average =( totalsum / (float) validpixels );
      return average;  
   }
}


void to_logfile( int   subnr, 
                 float datamin,
                 float datamax,
                 float *gainminmax,
                 fint  *Febox,
                 float boxaverage )
/*
 *------------------------------------------------------------------------------
 * Output some data to log file 
 *------------------------------------------------------------------------------
 */                 
{
   sprintf( messbuf, "\nLocal enhancement in %d x %d neighborhood:", Febox[0], Febox[1] );
   anyoutC( messbuf );
   sprintf( messbuf, "Subset index            = %d", subnr );
   anyoutC( messbuf );
   sprintf( messbuf, "Name of input set       = %.*s", nelc_c(Fsetin), Fsetin.a );    
   anyoutC( messbuf );
   sprintf( messbuf, "Name of output set      = %.*s", nelc_c(Fsetout), Fsetout.a );    
   anyoutC( messbuf );
   sprintf( messbuf, "Average in selected box = %f", boxaverage );
   anyoutC( messbuf );
   sprintf( messbuf, "Min. value new data     = %f", datamin );
   anyoutC( messbuf );
   sprintf( messbuf, "Max. value new data     = %f", datamax );
   anyoutC( messbuf );
   sprintf( messbuf, "Min. value gain         = %f", gainminmax[0] );
   anyoutC( messbuf );
   sprintf( messbuf, "Max. value gain         = %f\n", gainminmax[1] );
   anyoutC( messbuf );
}   


int convert_21( int x, 
                int y, 
                int Xpointsinline) 
/*
 *------------------------------------------------------------------------------
 * Convert a two dimensional to an one dimensional position. Starting point
 * is (0,0) == (0)
 *------------------------------------------------------------------------------
 */
{
   return (y * Xpointsinline + x);
}



int local_enhance( int YlineLO, 
                   int YlineHI, 
                   int Xpointsinline, 
                   float boxaverage, 
                   float *gainrange, 
                   float *gainminmax )
/*
 *------------------------------------------------------------------------------
 * Construct a buffer with partially old and new values en do the local 
 * enhancement. 
 *------------------------------------------------------------------------------
 */
{
   static  int    oldstorage_len;
   static  int    Ylines;
   static  fint   ndat;
   static  int    bottomline, topline;
   static  int    linestokeep;
   static  fint   pixelstokeep;
   static  float  imageSTORE[2*MAXBUF];
   static  int    x, y;
   static  int    outpos, storepos;
   static  int    needno_exam;
   static  float  value;
   static  int    imin, imax;
   static  int    jmin, jmax;   
  

    
   Ylines = YlineHI - YlineLO + 1;                  /* Total number of lines in imageIN */
   ndat   = Ylines * Xpointsinline;                 /* That is 'ndat' pixels */
   needno_exam = (Febox[1] / 2);
   if (YlineLO == (int) FgridLO[1]) {
      /* First time data was read */
      /* Move read buffer to storage */
      mover_c( imageIN, imageSTORE, &ndat );
      oldstorage_len = ndat;
      bottomline = 0;
      topline    = YlineHI - YlineLO - needno_exam;
   }
   else {
      linestokeep = Febox[1] - 1;
      pixelstokeep = linestokeep * Xpointsinline;
      /* Move last lines of storage to start of storage */
      mover_c( &imageSTORE[oldstorage_len - pixelstokeep], 
               imageSTORE, &pixelstokeep );
      /* Append read buffer to storage */
      mover_c( imageIN, &imageSTORE[pixelstokeep], &ndat );
      oldstorage_len = pixelstokeep + ndat;
      bottomline = Febox[1] / 2;
      topline    = YlineHI - YlineLO + linestokeep - needno_exam;
   }
   if (YlineHI == (int) FgridHI[1]) {
      /* The last line is included */
      if (YlineLO == (int) FgridLO[1]) {
         bottomline = 0;
      }
      else {         
        bottomline = Febox[1] / 2;
      }
      topline    = YlineHI - YlineLO + linestokeep;
   }
   
   outpos = 0;                                      /* Pos. in output array */
   for (y = bottomline; y <= topline; y++) {
      for (x = 0; x < Xpointsinline; x++) { 
         storepos = convert_21(x, y, Xpointsinline);
         value = imageSTORE[storepos];
         if (value == blank) {
            imageOUT[outpos++] = blank;             /* Check on blanks */
         }
         else {
            if (!insidebox(FgridLO[0] + x, YlineLO + y)) {
               /* 'Freplace' and 'outboxreplace' are global! */
               if (Freplace) {
                  imageOUT[outpos++] = outboxreplace;/* user value */
               }
               else {
                  imageOUT[outpos++] = value;       /* original value */
               }
            }
            else {    
               /* Not blank and inside box */        
               imax = Febox[0] / 2; imin = -1 * imax;
               jmax = Febox[1] / 2; jmin = -1 * jmax;
               { /* Begin calculation part */
                  static float dev, sumdevs;
                  static float localsum;
                  static float localaverage;
                  static float localsigma;
                  static float localgain;
                  static float weightsum, weight;
                  static int   i, j, k;
                         int   Xoff = FgridLO[0];
                         int   Yoff = YlineLO;
                  static float subval;
                  static int   validpixels;
                  static float localstore[MAXLOCAL];
         
                  if (!inrange(value, range)) {
                     imageOUT[outpos++] = value;
                  }
                  else {
                     localsum = 0.0;
                     weightsum = 0.0;
                     validpixels = 0;
                     for (i = imin; i <= imax; i++) {
                        for (j = jmin; j <= jmax; j++) {
                           if (insidebox(Xoff+x+i,Yoff+y+j)) {
                              if (Fweighting) {
                                 if (i==0 && j==0) {
                                    /* Initial weight for central pixel */
                                    weight = 2.0; 
                                 }
                                 else {
                                    /* Weight with distance */ 
                                    weight = sqrt(i*i+j*j);
                                 }
                              }
                              else {
                                weight = 1.0;
                              }                              
                              subval = imageSTORE[convert_21(x+i, y+j, Xpointsinline)];
                              if (subval != blank) {
                                 localstore[validpixels++] = subval;
                                 localsum += (subval / weight);
                                 weightsum += weight;
                              }
                           }
                        }
                     }            
                     localaverage = localsum / weightsum;
                     
                     /* Calculate the standard deviation */
                     sumdevs = 0.0;
                     for (k = 0; k < validpixels; k++) {
                        dev = (localstore[k] - localaverage);
                        dev *= dev;
                        sumdevs += dev; 
                     }                     
                     if (validpixels !=0 && sumdevs != 0.0) {
                        localsigma = sqrt( sumdevs / (float) validpixels ); 
                        localgain = (boxaverage/localsigma);
/*                        sprintf(messbuf, "Range: %f %f", gainrange[0], gainrange[1] ); anyoutC(messbuf);
                        sprintf(messbuf, "gain original %f", localgain ); anyoutC(messbuf);*/
                        localgain = MYMAX( gainrange[0], localgain );
/*                        sprintf(messbuf, "gain after rangelo %f", localgain ); anyoutC(messbuf);*/
                        localgain = MYMIN( gainrange[1], localgain ); 
/*                        sprintf(messbuf, "gain after rangehi %f", localgain ); anyoutC(messbuf);*/
                        gainminmax[0] = MYMIN( gainminmax[0], localgain );
/*                        sprintf(messbuf, "gain min %f", gainminmax[0] ); anyoutC(messbuf);*/
                        gainminmax[1] = MYMAX( gainminmax[1], localgain );
/*                        sprintf(messbuf, "gain max %f", gainminmax[1] ); anyoutC(messbuf);*/
                        /* Do the transformation */
                        imageOUT[outpos++] = localgain *
                                             (value - localaverage) + 
                                             localaverage;
                     }
                     else {
                       imageOUT[outpos++] = value;
                     }
                  } /* if in range */
               } /* end local calculation */ 
            } /* inside box and not blank */
         } /* if not blank */
      } /* Endfor in x-direction */
   } /* Endfor in y-direction */
   return outpos;
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
   Fsubdim = 2;  /* Subset dimension must be 2! */                     
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
      
   range[0] = -1.0*FLT_MAX;            /* Defined in float.h */
   range[1] = FLT_MAX;
   Fdfault  = REQUEST;
   getrange_c( range, &Fdfault, KEY_RANGE, MES_RANGE );


   maxYlines    = FmaxIObuf / (int) Faxcount[0];
   Fnumitems    = 2;
   Fdfault      = REQUEST;
   do {
      Febox[0] = 3;
      Febox[1] = 3;   
      agreed = true;
      Fr1 = userint_c( Febox, &Fnumitems, &Fdfault,
                       KEY_EBOX, MES_EBOX );
      if ( Febox[0] < 1 || Febox[1] < 1 ) {
         (void) reject_c( KEY_EBOX, tofchar("Values >= 1") );
         agreed = false;
      }
      if (agreed && (Febox[0]%2 == 0 || Febox[1]%2 == 0)) {
         (void) reject_c( KEY_EBOX, tofchar("Only odd values!") );
         agreed = false;
      }
      if (agreed && (Febox[0] > maxYlines || Febox[1] > maxYlines)) {
         (void) reject_c( KEY_EBOX, tofchar("Size(s) too big!") );
         agreed = false;
      }   
      if (agreed && (Febox[0] * Febox[1] > MAXLOCAL)) {
         sprintf( messbuf, "> %d pixels", MAXLOCAL );
         (void) reject_c( KEY_EBOX, tofchar(messbuf) );
         agreed = false;
      }   
   } while (!agreed);
      
   Fnumitems = 1;
   Fdfault = REQUEST;
   Fweighting = toflog( false );
   Fr1 = userlog_c( &Fweighting, &Fnumitems, &Fdfault,
                    KEY_WEIGHTS, MES_WEIGHTS );


   Freplace  = toflog( false ); 

   if  (  (FgridLO[0] != BgridLO[0]) ||
          (FgridLO[1] != BgridLO[1]) ||
          (FgridHI[0] != BgridHI[0]) ||
          (FgridHI[1] != BgridHI[1]) ) {            
      /* There is a box smaller than the subset */    
      Fdfault   = REQUEST;
      Fnumitems = 1;      
      Fr1 = userlog_c( &Freplace, &Fnumitems, &Fdfault,
                       KEY_REPLACE, MES_REPLACE );
   }

   if (Freplace) {
      /* User wants to replace values outside box with a constant */
      Fdfault = REQUEST;
      Fnumitems = 1;
      outboxreplace = blank;
      Fr1 = userreal_c( &outboxreplace, &Fnumitems, &Fdfault, 
                        KEY_BVAL, MES_BVAL );                    
   }


   Fnumitems = 2;
   Fdfault   = HIDDEN;
   do {
      gainrange[0] = -1.0 * FLT_MAX;
      gainrange[1] = FLT_MAX; 
      getrange_c( gainrange, &Fdfault, KEY_GRANGE, MES_GRANGE );
      agreed = (gainrange[0] <= gainrange[1]);
      if (!agreed) {         
         Fdfault = REQUEST;
         (void) reject_c( KEY_GRANGE, tofchar("first > second") );         
      }
   } while (!agreed);
   
   gainminmax[1] = -1.0*FLT_MAX;
   gainminmax[0] = FLT_MAX;
   
   usermean = false;
      
  /*
   *----------------------------------------------------------------------------
   * Loop over all specified subsets. 
   *----------------------------------------------------------------------------
   */   
   for(subnr = 0; subnr < (int) FnsubsI; subnr++) {

           
      static fint  Fnuminreadbuf;
      static fint  Fnumtodisk;
      static int   totalYlines;
      static int   Xpointsinline;
      static int   Ylinesleft;
      static int   YlineLO, YlineHI;
      static int   Ylinesread;   
    

           
      
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
      
      FtidIN        = FtidOUT = 0;      
      Fmcount       = 0;                              /* Used in 'minmax3' */
      totalYlines   = Faxcount[1];
      Xpointsinline = Faxcount[0];
      Ylinesleft    = totalYlines;
      YlineLO       = FgridLO[1];
      YlineHI       = YlineLO;

      /* Check whether an average is specified or has to be calculated */
      if (!usermean) boxaverage = average( Bcwlo, Bcwhi, &usermean );
            
      if (subnr == 0) {
         /* Initialize stabar */
         STBstart   = 0.0;
         STBend     = (float) FnsubsI * totpixels;     /* stabar at read and write */
         STBcurrent = 0.0;
         (void) stabar_c( &STBstart, &STBend, &STBcurrent );
      }
     
         
      do {         
         Ylinesread = (FmaxIObuf / Xpointsinline);     /* Maximum lines to read */
         Ylinesread = MYMIN( Ylinesleft, Ylinesread ); /* But no more than lines left */
         Fnuminreadbuf = Ylinesread * Xpointsinline;   /* Total number of values to read */
         Ylinesleft -= Ylinesread;
         YlineHI     = YlineLO + Ylinesread - 1; 
         
         (void) gdsi_read_c( Fsetin,                   /* Read pixels in 'imageIN' */
                             &Fcwlo, &Fcwhi, 
                             imageIN,
                             &Fnuminreadbuf, 
                             &Fnuminreadbuf, 
                             &FtidIN );
                             
         Fenhanced = local_enhance( YlineLO,           /* Do the enhancement */
                                    YlineHI,
                                    Xpointsinline,
                                    boxaverage, 
                                    gainrange,
                                    gainminmax );
                           
         (void) gdsi_write_c( Fsetout,                 /* Write new pixel values */
                              &FcwloO, &FcwhiO, 
                              imageOUT,
                              &Fenhanced, 
                              &Fnumtodisk, 
                              &FtidOUT );          

         (void) minmax3_c( imageOUT,                   /* Administration */
                           &Fnumtodisk, 
                           &datamin[subnr], 
                           &datamax[subnr], 
                           &Fnblanks[subnr], 
                           &Fmcount );
                           
         STBcurrent += (float) Fnumtodisk;             /* Show progress to user */
         (void) stabar_c( &STBstart, 
                          &STBend, 
                          &STBcurrent );            
                            
                             
        YlineLO = YlineHI + 1;         
      } while (Ylinesleft > 0);
      (void) to_logfile( subnr, 
                         datamin[subnr], 
                         datamax[subnr], 
                         gainminmax, 
                         Febox,
                         boxaverage );
   } /* For all subsets? */
   
   /* Remove in 'WMINMAX' old minmax descriptors because program */
   /* changes values! */
   Fremove = true;   
   (void) wminmax_c( Fsetout, Fsubout, datamin, datamax, Fnblanks,
                     &FnsubsO, &Fremove );
   finis_c();                                                  /* Quit Hermes */
}

