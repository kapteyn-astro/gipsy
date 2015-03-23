/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             decim.dc1

Program:       DECIM

Purpose:       Decrease size of a set in any direction with an
               integer factor

Category:      MANIPULATION, CALCULATION

File:          decim.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give set (, subsets) to decimate:

               Maximum number of subsets is 2048.


   BOX=        Frame for input subsets.                 [entire subset]


   DECIM=      Give decimation:				        [1,...]

               The user can specify a decimation factor for each axis
               in a subset. The default value for each axis is 1.
               Negative values are not allowed.


** SHIFT=      Shift in start of decimation:                        [0]

               Specify which of the 'decim'  number of pixels
               will be transferred to the new created output pixel.
               If for example a decimation factor 3 is specified, the
               pixels 0, 1, 2 create a new output pixel containing the
               value of the original pixel 0 by default. However, if
               the user wants to transfer one of the other pixels,
               he has to specify a shift. For the decimation factor 3
               the shifts 1 and 2 are allowed. Of negative shifts, the
               absolute values are taken and of shift values greater
               than (decimation - 1) the modulus is calculated.


   OUTSET=     Give output set (, subsets):

               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets.


Description:   Decimate data in each specified direction of a subset.
               The decimation factor must be an integer number.
               In the output, pixel 0 will always be retained.
               If for example the user specified the decimation factor
               3, then the pixels -9, -6, -3, 0, 3, 6, 9 etc. will be
               in the output. If he also specified for this axis a
               shift of 1, the pixels  -5, -2, 1, 4, 7 etc. will be
               transferred. A possible use of the shift could be the
               creation of sets made with different shifts to combine
               them to a new set with flux that can be compared to
               the flux of the original (not decimated) set.


Example:

Updates:       ... ..,  1990: VOG, Document created.
               jun 11,  1996: VOG, Bug in output size removed.
               Oct  1,  1996: VOG, DECIM could not handle subsets:
                                   out=0 initialisation was move from
                                   outside to inside subset loop.
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
#include "userint.h"
#include "userfio.h"
#include "cancel.h"
#include "gdsasn.h"
#include "gdscss.h"
#include "gdscpa.h"
#include "gdsd_rdble.h"
#include "error.h"
#include "gdsout.h"
#include "gdsi_write.h"
#include "minmax3.h"
#include "stabar.h"
#include "wminmax.h"
#include "gdsc_name.h"

#define AXESMAX    10
#define SUBSMAX    2048
#define MAXBUF     16*4096                             /* Buffer size for I/O */
#define VERSION    "1.1"
#define NONE       0
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define fmake(fchr,size) { \
                            static char buff[size+1]; \
                            int i; \
                            for (i = 0; i < size; buff[i++] = ' '); \
                            buff[i] = 0; \
                            fchr.a = buff; \
                            fchr.l = size; \
                         }


#define MYMAX(a,b)     ((a) > (b) ? (a) : (b))
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )



/* Input of set, subsets: */

static fchar    setin;
static fint     subin[SUBSMAX];
static fint     nsubsI;                     /* Number of input subsets */
static fint     dfault;                     /* Default option for input etc */
static fchar    keyword, message;
static fint     axnum[AXESMAX];
static fint     axcount[AXESMAX];
static fint     Daxcount[AXESMAX];          /* axis lengths of decimated box */
static fint     class = 1;
static fint     subdim, setdim;
static fint     scrnum;                     /* Destination of log output */
static fint     Ires;
static fint     maxaxes  = AXESMAX;
static fint     maxsubs  = SUBSMAX;
static fint     maxIObuf = MAXBUF;
static fint     subnr;
static fchar    axname;
static fint     i, m, out;                   /* Counters */
static float    blank;
static fint     wholeset = 0;
static char     messbuf[80];
static char     messstr[80];

/* Output stuff */

static fchar    setout;
static fint     nsubsO;
static fint     subout[SUBSMAX];
static fint     axnumO[AXESMAX];
static fint     axcountO[AXESMAX];

/* Input of area etc.:*/

static fint     r1, r2;
static fint     cwloI, cwhiI;              /* Coordinate words */
static fint     cwloO, cwhiO;
static fint     FgridLO[AXESMAX], FgridHI[AXESMAX];
static fint     BgridLO[AXESMAX], BgridHI[AXESMAX];
static fint     DgridLO[AXESMAX], DgridHI[AXESMAX];  /* Decimated frame */
static fint     modvec[AXESMAX];           /* Number of pixels in each dim */
static fint     curvec[AXESMAX];           /* subdim dim. equivalent of 1 dim. pos */
static fint     shiftvec[AXESMAX];         /* shift curvec if axis doesn't contain 0 */
static fint     boxopt;
static fint     totpixels;

/* Data transfer: */

static fint     pixelsdone;
static fint     pixelcount;
static fint     writepixels;
static fint     tidI, tidO;                /* Transfer id's */
static float    image[MAXBUF];
static float    imageO[MAXBUF];            /* Buffer for output data */

/* Miscellaneous: */

static fint     decim[AXESMAX];
static fint     pos, curpos;
static double   cdelt[AXESMAX], Dcdelt[AXESMAX];
static double   dummyD;
static fint     pmask;                     /* Action mask in gdscpa routine */
static fint     mcount;                    /* Counter in minmax3 */
static float    minval[SUBSMAX], maxval[SUBSMAX];
static fint     nblanks[SUBSMAX];
static float    stabarv[3];      /* start-, end- and current value for progress bar */
static fint     removit = 1;     /* Remove in 'WMINMAX' old minmax descriptors */
static int      charpos;
static int      agreed;


MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{

   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      fchar    Ftask;                      /* Name of current task */
      fmake( Ftask, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Ftask );                   /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';       /* Terminate task name with null char. */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }

   setfblank_c( &blank );
   fmake(setin, 80);
   keyword = tofchar("INSET=");
   message = tofchar("Give set (, subsets) to decimate: " );
   dfault  = NONE;
   subdim  = 0;
   scrnum  = 1;
   nsubsI  = gdsinp_c( setin,
                       subin,
                       &maxsubs,
                       &dfault,
                       keyword,
                       message,
                       &scrnum,
                       axnum,
                       axcount,
                       &maxaxes,
                       &class,
                       &subdim );
   setdim  = gdsc_ndims_c( setin, &wholeset );
  /*
   *----------------------------------------------------------------------------
   * Determine the edges of this its frame (FgridLO/HI)
   *----------------------------------------------------------------------------
   */
   r1 = 0;
   gdsc_range_c( setin, &wholeset, &cwloI, &cwhiI, &r1 );
   for (m = 0; m < setdim; m++)
   {
      r1 = r2 = 0;
      FgridLO[m] = gdsc_grid_c( setin, &axnum[m], &cwloI, &r1 );
      FgridHI[m] = gdsc_grid_c( setin, &axnum[m], &cwhiI, &r2 );
   }
   /* Get grid spacings from header */
   for (i = 0; i < subdim; i++)
   {
      Ires = sprintf( messbuf, "%s%-2d", "CDELT", axnum[i] );
      r1 = 0;
      /* Get the pixel separation of the axes */
      gdsd_rdble_c( setin, tofchar(messbuf), &wholeset, &cdelt[i], &r1 );
      if (r1 < 0)
         errorf( 4, "DECIM: No grid spacings in header of this set!" );
   }
  /*
   *----------------------------------------------------------------------------
   * Prepare a box for INSET. Default is entire subset
   *----------------------------------------------------------------------------
   */
   dfault  = REQUEST;
   keyword = tofchar("BOX=");
   message = tofchar(" ");
   boxopt  = 0;
   scrnum  = 3;
   gdsbox_c( BgridLO,
             BgridHI,
             setin,
             subin,
             &dfault,
             keyword,
             message,
             &scrnum,
             &boxopt );

   /* Count number of pixels in this substructure */
   totpixels = 1;
   /* For one subset */
   for(m = 0; m < subdim; m++) totpixels *= (BgridHI[m] - BgridLO[m] + 1);
   totpixels *= nsubsI;
  /*
   *----------------------------------------------------------------------------
   * Ask user to give decimation values. If he gives less than subdim
   * integers, make rest equal to 1.
   *----------------------------------------------------------------------------
   */
   fmake(axname, 18);
   dfault  = REQUEST;
   keyword = tofchar("DECIM=");
   /* First part of message */
   sprintf( messstr, "%s", "Give decimation in {" );
   for (i = 0; i < subdim; i++)
   {
      decim[i] = 1;
      r1 = 0;
      gdsc_name_c( axname, setin, &axnum[i], &r1 );
      sprintf( messbuf, "%s", axname.a );
      charpos = 0;
      /* Strip the string with the axis name */

      while(messbuf[charpos] != '-' &&
            messbuf[charpos] != ' ' &&
            messbuf[charpos] != '\0'   )
        charpos++;

      sprintf( messbuf, "%.*s", charpos, messbuf );
      /* Append axis name to message */
      sprintf( messstr, "%.*s %s", strlen(messstr), messstr, messbuf );
   }

   /* Axis names placed, append colon and default now */
   sprintf( messstr, "%.*s%s", strlen(messstr), messstr, " }:    [1,... ]" );
   message = tofchar(messstr);
   do
   {
      Ires = userint_c( decim, &subdim, &dfault, keyword, message );
      agreed = 1;
      for (i = 0; i < subdim; i++)
      {
         if (!(decim[i] >= 1))
            agreed = 0;
      }
      if (!agreed)
      {
         cancel_c( keyword );
         anyoutf( 1, "Decimation not allowed, try again" );
         for (i = 0; i < subdim; decim[i++] = 1);          /* Restore default */
      }
   }
   while (!agreed);

  /*
   *----------------------------------------------------------------------------
   * If for example a decimation factor 3 is specified, the pixels 0, 1, 2
   * create a new output pixel containing the value of the original pixel 0
   * by default. If however the user wants to transfer one of the other pixels,
   * he has to specify a shift. For the decimation factor 3 the shifts 1 and 2
   * are allowed. Of negative shifts the absolute values are taken and of
   * values greater than (decimation - 1) the modulus is calculated.
   * Ask user to give the shift values specified with the hidden keyword SHIFT=
   * If the user specified less than 'subdim' integers, make rest of the shift
   * values equal to 1.
   *----------------------------------------------------------------------------
   */
   dfault  = HIDDEN;
   keyword = tofchar("SHIFT=");
   message = tofchar(" ");
   /* Preset default values for the shift vector */
   for (i = 0; i < subdim; shiftvec[i++] = 0);
   Ires = userint_c( shiftvec, &subdim, &dfault, keyword, message );
   for (i = 0; i < subdim; i++)
      shiftvec[i] = ABS(shiftvec[i]) % decim[i];
  /*
   *----------------------------------------------------------------------------
   * Assign 'gdsinp' buffer to 'gdsout'. Output set will get same coordinate
   * system as input INSET.
   *----------------------------------------------------------------------------
   */
   keyword = tofchar("OUTSET=");
   gdsasn_c( tofchar("INSET="), keyword, &class );
  /*
   *----------------------------------------------------------------------------
   * Construct the new grids by dividing the position of the pixel on the low
   * end of the axis and the position of the pixel on the high end of the axis
   * by the corresponding decimation factor.
   * The division is an integer division so: -7/2=-3 and  7/2=3.
   *----------------------------------------------------------------------------
   */
   for (i = 0; i < subdim; i++)
   {
      int   len = 0, k;
      for ( k = BgridLO[i]; k <= BgridHI[i]; k++)
      {
          if ( (k+shiftvec[i])%decim[i] == 0 )
            len ++;
      }
      DgridLO[i] = BgridLO[i] / decim[i];  /* Just a start value */
      DgridHI[i] = DgridLO[i] + len -1;    /* Add len for highest grid */
   }
   for (i = subdim; i < setdim; i++)
   {
      DgridHI[i]  =  FgridHI[i];
      DgridLO[i]  =  FgridLO[i];
   }
   gdscss_c( keyword, DgridLO, DgridHI );        /* Change size of output set */
   dummyD = 0.0;
   pmask = 32;                                           /* Only change cdelt */
   for (i = 0; i < subdim; i++)
   {
      fint   axis = i + 1;

      /* Determine new axis length */
      Daxcount[i] = DgridHI[i] - DgridLO[i] + 1;
      /* New pixel separation in double precision */
      Dcdelt[i] =  cdelt[i] * (double) decim[i];
     /*
      *-------------------------------------------------------------------------
      * Change properties of an axis. Beware: the axis numbers are 1, 2, 3 etc.
      * now, so don't confuse this with the sequence in axnum here. Only the
      * length of an axis and its cdelt is changed.
      *-------------------------------------------------------------------------
      */
      gdscpa_c( keyword,
                &axis,
                &Daxcount[i],
                &Dcdelt[i],
                &dummyD,
                &dummyD,
                &dummyD,
                tofchar(" "),
                tofchar(" "),
                &pmask );
   }

  /*
   *----------------------------------------------------------------------------
   * Ask for output set, check on number of subsets:
   *----------------------------------------------------------------------------
   */
   dfault  = NONE;
   message = tofchar("Give output set (, subsets):");
   scrnum  = 3;
   fmake(setout, 80);
   message = tofchar("Give output set (and subsets) " );
   do
   {
      nsubsO = gdsout_c( setout,
                         subout,
                         &nsubsI,
                         &dfault,
                         keyword,
                         message,
                         &scrnum,
                         axnumO,
                         axcountO,
                         &maxaxes );
      if (nsubsO != nsubsI)
      {
         cancel_c( keyword );
         anyout_c( &scrnum, tofchar("DECIM: Wrong number of subsets:") );
      }
   }
   while (nsubsO != nsubsI);

  /*
   *----------------------------------------------------------------------------
   * Calculate number of pixels of underlying substructures of 1, 2, ...
   * (subdim - 1) dimensions. Put these numbers in the vector 'modvec'. The
   * numbers are used in the function 'postovec' to convert a one dimensional
   * position to a 'subdim' dimensional position.
   *----------------------------------------------------------------------------
   */
   modvec[0] = 1;
   for (i = 1; i < subdim; i++)
      modvec[i] = modvec[i-1] * (BgridHI[i-1] - BgridLO[i-1] + 1);
  /*
   *----------------------------------------------------------------------------
   * Loop over all subsets and read data. Do the decimation and fill
   * output with decimated data.
   *----------------------------------------------------------------------------
   */
   pixelcount = 0;
   curpos = pos = 0;
   stabarv[0] = 0.0;
   stabarv[1] = (float) totpixels;
   for(subnr = 0; subnr < nsubsI; subnr++)
   {
      /* Make coordinate words for these corners */
      cwloI = gdsc_fill_c( setin, &subin[subnr], BgridLO );
      cwhiI = gdsc_fill_c( setin, &subin[subnr], BgridHI );
      /* Use decimated grids output */
      cwloO = gdsc_fill_c( setout, &subout[subnr], DgridLO );
      cwhiO = gdsc_fill_c( setout, &subout[subnr], DgridHI );
      tidI = tidO = 0;
      mcount = 0;
      out = 0;
      do
      {
      	 gdsi_read_c( setin,
      	              &cwloI, &cwhiI,
      	              image,
                      &maxIObuf,
                      &pixelsdone,
                      &tidI );
         for (i = 0; i < pixelsdone; i++)
         {
            pos = pixelcount + i;
            curpos = pos;
           /*
            *-------------------------------------------------------------------
            * Convert one dimensional position 'curpos' to a 'subdim'
            * dimensional position. The result is 'curvec'. Shift a position in
            * 'curvec' if a corresponding shift is given.
            * After this, decimate by calculating 'pixel' MOD 'decim'.
            * If the result is 0 then transfer to the output buffer.
            *-------------------------------------------------------------------
            */
            {
 	       fint k,j;

 	       agreed = 1;
               for (k = subdim - 1; k >= 0; k--)
               {
                  j = curpos / modvec[k];
	          curvec[k] = BgridLO[k] + j + shiftvec[k];
 	          if ( (ABS(curvec[k]) % decim[k]) != 0 )
 	          {
 	             agreed = 0;
 	             /* Abort loop if pixel need not to be transferred */
 	             break;
 	          }
 	          curpos -= j * modvec[k];
 	       }
 	    }
           /*
            *-------------------------------------------------------------------
            * If pixel falls on decimated grid, transfer it to the
            * output buffer. Write filled buffer to disk. Before that,
            * update min. & max and number of blanks.
            *-------------------------------------------------------------------
            */
            if (agreed)
            {
            	imageO[out++] = image[i];
                if (out == maxIObuf)
                {
                   /* Calculate running min, max & blanks of output */
                   minmax3_c( imageO,
                              &out,
                              &minval[subnr],
                              &maxval[subnr],
                              &nblanks[subnr],
                              &mcount );
                   gdsi_write_c( setout,
                                 &cwloO, &cwhiO,
                                 imageO,
                                 &maxIObuf,
                                 &writepixels,
                                 &tidO );
                   out = 0;
                }
            }
         }
         pixelcount += pixelsdone;
         stabarv[2] = (float) pixelcount;
         /* Show progress to user */
         stabar_c( &stabarv[0], &stabarv[1], &stabarv[2] );
      }
      while (tidI != 0);

      if (out != 0)
      {
         /* Some pixels left in buffer */
         minmax3_c( imageO,
                    &out,
                    &minval[subnr],
                    &maxval[subnr],
                    &nblanks[subnr],
                    &mcount );
         gdsi_write_c( setout,
                       &cwloO,
                       &cwhiO,
                       imageO,
                       &maxIObuf,
                       &writepixels,
                       &tidO );
      }
   }
  /*
   *----------------------------------------------------------------------------
   * Update output header, remove MINMAX descriptors at intersecting levels.
   *----------------------------------------------------------------------------
   */
   wminmax_c( setout,
              subout,
              minval,
              maxval,
              nblanks,
              &nsubsO,
              &removit );
   finis_c();                                                  /* Quit Hermes */
   return(EXIT_SUCCESS);                                      /* Dummy return */
}
