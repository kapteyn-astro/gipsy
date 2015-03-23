/*
			    COPYRIGHT (c) 1992
		      Kapteyn Astronomical Institute
		University of Groningen, The Netherlands
			   All Rights Reserved.


#>	       scanaid.dc1

Program:       SCANAID

Purpose:       Add IRAS data with respect to a reference point (ADDSCAN)

Category:      IRAS

File:	       scanaid.c

Author:        T. Prusti

Keywords:

   INIRDS=     Input IRDS

   OUTSET=     Output set		    [Scans not stored in a set]
	       Set to contain the selected detector scans.

*  OVERWRITE=  Output set exists, ok to overwrite?		  [Y]/N
	       If OVERWRITE=N you will be prompted for another OUTSET=

   FILENAME=   Name of ASCII file:		    [No output to file]
	       If a name is specified, an ASCII file is created to
	       store data. If you press carriage return, there will
	       be no output to an ASCII file. If a given name already
	       exists, APPEND= must be specified.

*  APPEND=     File exists, ok to append?			  [Y]/N
	       The file specified in FILENAME= already exists.
	       You can append to this file with APPEND=Y. If APPEND=N
	       you will be prompted for another filename.

   POS=        Reference position		   [Center of the IRDS]

   OBJECT=     Name of the object in the reference position [IRDS name]

** UNITS=      Output units					   [Jy]

** ACDC=       ACDC conversion				      [default]
	       IRAS band dependent conversion factor to change
	       calibration applicable for point sources. Default
	       values are 1.282, 1.2195, 1.087 and 1.0 for 12, 25,
	       60 and 100 micron respectively. See Explanatory
	       Supplement page IV-9 for more information.

** INCFAC=     Include factor in half detector units.		  [1.0]
	       Include scans crossing at a distance INCFAC * detector
	       half width. By default INCFAC is 1.0 which will select
	       only scans which had the reference position within the
	       nominal detector size.

   SHOW=       Control plotting of scans	    [Plot final result]
	       N(one), A(ll), O(ne by one) plotting can be chosen in
	       addition to the default to show the ADDSCAN result only.

*  GRDEVICE=   Plot device:			      [List of devices]
	       Destination of plot, Screen or Hardcopy.

** LENGTH=     Length of the scan to be plotted (arcmin)    [15 arcmin]
	       Length is the minimum length to be plotted and valid
	       only for the individual scans (not to ADDSCAN).

** AROUND=     The minimum number of samples around the closest     [5]
	       This value is used to select only those scans which
	       have at least AROUND samples before and after the
	       sample closest to the reference point. It is also
	       used to define the window for minimum and maximum
	       determination for baseline removal and plot limits.

** MINMAX=     Fixed minimum and maximum value for plots   [calculated]
	       Plot height will be fixed to contain at least the min
	       and max given by the user. Otherwise the plot y-scale
	       will vary from scan to scan. Units as from UNITS=.

** PGMOSAIC=   View surface sub divisions in x,y:		  [1,1]
	       View surface can contain a number of plots in
	       in X and Y direction (mosaic). Default is 1 plot in
	       both X- and Y direction.

** PGPAPER=    Give width(cm), aspect ratio:		   [calc, calc]
	       Aspect ratio is height/width.

** PGVIEW=     Viewport on paper:		     [0.2 0.95 0.1 0.9]
	       It is possible to change the viewport on paper.
	       The parameters are x-left, x-right, y-bottom
	       y-top.

** PGCOLOR=    Give color 1..15:				    [1]
	       See description for the available colors.

** PGWIDTH=    Give line width 1..21:				    [2]

** PGHEIGHT=   Give character height:				  [1.0]

** PGFONT=     Give font 1..4:					    [2]


Description:   From the given input IRDS all data crossing the given
	       reference point will be extracted and put in the given
	       output set after regridding the data into a finer grid.
	       The output set has axes for OFFSET (with respect to the
	       reference point) and SCANNR. SCANNR zero contains the
	       average of all scans (equivalent to ADDSCAN if the
	       default INCFAC=1.0 is kept).

	       For every detector scan selected a plot (if chosen) and
	       a line of information is given. In the plot the crosses
	       are the original data and the smooth line represents
	       the splined data. In the final ADDSCAN result the average
	       of the splined data is shown. The printed information
	       contains a running number (equal to SCANNR above), sop &
	       att numbers identifying the scan, detector number, in-scan
	       offset of the closest sample to the normal from the
	       detector scan to the reference point, in-scan step as
	       deduced from the immediate surroundings of the reference
	       point, twist angle of the scan, cross-scan offset between
	       the center of the detector and the detector scan, half
	       cross-scan size of the detector in question, modified
	       Julian Date (JD-2440000 e.g. 0 UT Jan 1, 1983 is 5335.5)
	       and IRDS info. The IRDS info contains SNIP and SDET
	       coordinates of the scan in the original input IRDS set.
	       Also the number of the closest sample in the IRDS is listed.

	       Color indices:

	       0      Background
	       1      Default (Black if background is white)
	       2      Red
	       3      Green
	       4      Blue
	       5      Cyan
	       6      Magenta
	       7      Yellow
	       8      Orange
	       9      Green + Yellow
	       10     Green + Cyan
	       11     Blue + Cyan
	       12     Blue + Magenta
	       13     Red + Magenta
	       14     Dark Gray
	       15     Light Gray
	       16-255 Undefined

	       Available fonts:

	       1  single stroke "normal" font
	       2  roman font
	       3  italic font
	       4  script font

Notes:	       The program only selects scans. It is left up to the user
	       to extract the final result either from the final plot
	       or from the outset. The baseline removal is primitive and
	       should be controlled for high precision work.

	       For the pointed observations it may be necessary to define
	       ACDC and AROUND (see above) to get correct results.

Hints:	       Use keywords PGMOSAIC= and MINMAX= to plot several scans
	       in one scale on one page.

Updates:       Aug 5,  1992: TP, Document created and test version released.
	       May 26, 1993: TP, Version 1.0 released.
	       Jun 1,  1993: TP, Handling of units simplified.
	       Jun 2,  1993: TP, MINMAX= handling fixed.
	       Jun 3,  1993: TP, Modifications made to improve PO handling.
	       Nov 11, 1993: TP, Allow change of viewport.
	       Apr 12, 1995: VOG/ZAAL Implemented CDELT1 for OFFSET axis
			     'Ytitle' position default (PGLAB)
	       Dec 12, 2001: DK, provide more precision on JD.
#<
*/

/*  scanaid.c: include files	 */

#include    "stdio.h"        /* Defines ANSI C input and output utilities */
#include    "stdlib.h"       /* Defines the ANSI C functions for number */
			     /* conversion, storage allocation, etc.*/
#include    "string.h"       /* Declares the ANSI C string functions*/
			     /* like:strcpy, strcat etc.*/
#include    "math.h"         /* Declares the math functions and macros.*/
#include    "cmain.h"        /* Defines the main body of a C program with */
			     /* MAIN_PROGRAM_ENTRY and IDENTIFICATION */
#include    "gipsyc.h"       /* Defines the ANSI-F77 types for Fortran */
			     /* to C interface including definition of */
			     /* char2str,str2char,tofchar,zadd */
			     /* and macros tobool and toflog */
#include    "float.h"        /* Definition of FLT_MAX etc.*/
#include    "ctype.h"        /* Declares ANSI C functions for testing chars */
			     /* like: isalpha, isdigit etc. */
			     /* also tolower, toupper.*/

/* Common includes */

#include    "init.h"         /* Declare task running to HERMES and initialize*/
#include    "finis.h"        /* Informs HERMES that servant quits and cleans */
			     /* up the mess.*/
#include    "anyout.h"       /* General character output routine for GIPSY */
			     /* programs.*/
#include    "setfblank.h"    /* Subroutine to set a data value to the */
			     /* universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "status.h"       /* Status line on HERMES */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is */
			     /* being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing */
			     /* blanks.*/

/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "usercharu.h"
#include    "userangle.h"
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by */
			     /* HERMES.*/

/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gdsc_range.h"   /* Return lower left and upper right corner */
			     /* of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate */
			     /* word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
			     /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "gdsd_rchar.h"   /* Reads fits header items.*/
#include    "minmax1.h"      /* Find min and max in an array. */
#include    "minmax3.h"      /* Find min, max and #blanks in subset. */
#include    "wminmax.h"      /* Writes (new) minimum and maximum and number */
			     /* of blanks of subsets in the descriptor file */
			     /* and optionally deletes the MINMAX descriptors*/
			     /* at intersecting levels. */
#include    "spline1.h"      /* Spline an array to another grid */
#include    "hms.h"          /* Convert number to a printable string */
#include    "dms.h"          /* Convert number to a printable string */

/* IRDS and IRAS related includes */

#include    "irds_enquire.h"
#include    "irds_enquire_snip.h"
#include    "irds_rd_detoff.h"
#include    "irds_rd_detpos.h"
#include    "irds_rd_samples.h"
#include    "irco.h"
#include    "ircc_detnr.h"
#include    "ircc_mask.h"
#include    "irc_chunit.h"

/* Output set related includes */

#include    "gds_create.h"
#include    "gds_extend.h"
#include    "gds_exist.h"
#include    "gds_delete.h"
#include    "gdsi_write.h"

/* PGPLOT includes */

#include    "pgbeg.h"        /* Begin PGPLOT, open output device. */
#include    "pgend.h"        /* Terminate PGPLOT. */
#include    "pgask.h"        /* Control new page prompting. */
#include    "pgpap.h"        /* Change the size of the view surface.*/
#include    "pgslw.h"        /* Set line width.*/
#include    "pgsvp.h"        /* Set viewport (normalized device coordinates).*/
#include    "pgsci.h"        /* Set color index. */
#include    "pgsch.h"        /* Set character height. */
#include    "pgpage.h"       /* Advance to new page. */
#include    "pgswin.h"       /* Set window. */
#include    "pgbox.h"        /* Draws a labeled frame around viewport. */
#include    "pgscf.h"        /* Set character font.*/
#include    "pglab.h"        /* Write labels for x-, y-axis, and top of plot.*/
#include    "pgmtxt.h"       /* Write text at position relative to viewport.*/
#include    "pgdraw.h"       /* Draw a line from the current pen position to */
			     /*  a point.*/
#include    "pgmove.h"       /* Move pen (change current pen position).*/
#include    "pgpt.h"         /* Plot one or more points.*/
#include    "pgline.h"       /* Plot one or more lines.*/

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

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)	       ( (a) < 0 ? (-(a)) : (a) )
#define PI	       3.141592653589793
#define RAD(a)	       ( a * 0.017453292519943295769237 )
#define DEG(a)	       ( a * 57.295779513082320876798155 )

#define RELEASE        "1.4"           /* Version number */
#define MAXAXES        4	       /* Max. axes in an irds set */
#define MAXSUBSETS     1024	       /* Max. allowed subsets */
#define SCAN_LEN       512	       /* Number of offset points in Setout */
#define ORIGIN	       SCAN_LEN / 2    /* Center of offset axis in Setout */
#define STRLEN	       80	       /* Max length of strings */
#define KEYLEN	       20	       /* Max length of keywords */
#define NONE	       0	       /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN	       2
#define EXACT	       4
#define YES	       1	       /* C versions of .TRUE. and .FALSE. */
#define NO	       0
#define NORMAL	       0	       /* anyoutC values */
#define NOVICE	       8
#define TEST	       16
#define WARNING        1	       /* error values */
#define MINOR	       2
#define SERIOUS        3
#define FATAL	       4

/* Defines for in/output routines etc.*/

#define KEY_INIRDS   tofchar("INIRDS=")
#define MES_INIRDS   tofchar("Give input irds:")
#define KEY_INSET    tofchar("INSET=")
#define MES_INSET    tofchar("Give input set:")
#define KEY_POS      tofchar("POS=")
#define MES_POS      tofchar("Give reference position [IRDS center]:")
#define KEY_OUTSET   tofchar("OUTSET=")
#define MES_OUTSET   tofchar("Give output set [no output to set]:")
#define KEY_OVERW    tofchar("OVERWRITE=")
#define MES_OVERW    tofchar("OUTSET exists. Ok to overwrite [Y]/N:")
#define KEY_OBJECT   tofchar("OBJECT=")
#define MES_OBJECT   tofchar("Give name of the object [IRDS object]:")
#define KEY_UNITS    tofchar("UNITS=")
#define MES_UNITS    tofchar("Give units for the output data [Jy]:")
#define KEY_ACDC     tofchar("ACDC=")
#define MES_ACDC     tofchar("Give the ACDC conversion factor [default]:")
#define KEY_INCFAC   tofchar("INCFAC=")
#define MAS_INCFAC   tofchar("Relative X-scan distance for selection [1.0]")
#define KEY_SHOW     tofchar("SHOW=")
#define MES_SHOW     tofchar("Show selected scans N, O, A, or [final result]")
#define MES_CONTINUE tofchar("Show selected scans N, A, F, or [next scan]")
#define KEY_LENGTH   tofchar("LENGTH=")
#define MES_LENGTH   tofchar("Length of plotted scan (arcmin): [15 arcmin]")
#define KEY_MINMAX   tofchar("MINMAX=")
#define MES_MINMAX   tofchar("Fixed min and max for y axis: [var. & calc.]")
#define KEY_AROUND   tofchar("AROUND=")
#define MES_AROUND   tofchar("Min number of samples around the closest: [5]")

#define AXNAM1	       tofchar("OFFSET")
#define AXNAM2	       tofchar("SCANNR")

/* Variables for input */

static fchar	Setin;		    /* Name of input set */
static fint	subin[MAXSUBSETS];  /* Subset coordinate words */
static fint	nsubs;		    /* Number of input subsets */
static fint	dfault; 	    /* Default option for input etc */
static fint	axnum[MAXAXES];     /* Array of size MAXAXES containing the */
				    /* axes numbers.  The first elements */
				    /* (upto the dimension of the subset) */
				    /* contain the axes numbers of the */
				    /* subset, the other contain the axes */
				    /* numbers outside the the subset */
				    /* ordered ccording to the specification */
				    /* by the user. */
static fint	showdev;	    /* Device number (as in ANYOUT) for info */
static fint	axcount[MAXAXES];   /* Array of size MAXAXES containing the */
				    /* number of grids along an axes as */
				    /* specified by the user. The first */
				    /* elements (upto the dimension of the */
				    /* subset) contain the length of the */
				    /* subset axes, the other ones contain */
				    /* the the number of grids along */
				    /* an axes outside the subset. */
static fint	maxsubs = MAXSUBSETS;
static fint	maxaxes = MAXAXES;  /* Max num. of axes the program can deal */
				    /* with.*/
static fint	class = 1;	    /* Class 1 is for applications which */
				    /* repeat the operation for each subset, */
				    /* Class 2 is for applications for which */
				    /* the operation requires an interaction */
				    /* between the different subsets. */
static fint	subdim; 	    /* Dimensionality of the subsets for */
				    /* class 1 applications */
static fint	setdim; 	    /* Dimension of set. */

/* OUTSET related variables */

static fchar	Setout; 	    /* Output set */
static fint	subout; 	    /* Output subset coordinate words */
static fint	nsubsout;
static fint	cwloO = SCAN_LEN+2; /* Output Coordinate words. */
static fint	cwhiO = 2*SCAN_LEN+1;
static fint	tidO;		    /* Transfer id for write function. */
static fint	pixelswrite;	    /* Number of pixels to write to output. */
static fint	outlen; 	    /* Set name length. */

/* PGPLOT variables */

const  fint  background  =  0;	    /* Color definitions for PGPLOT. */
const  fint  foreground  =  1;	    /* Black if background is white. */
const  fint  red	 =  2;
const  fint  green	 =  3;
const  fint  blue	 =  4;
const  fint  cyan	 =  5;
const  fint  magenta	 =  6;
const  fint  yellow	 =  7;
const  fint  orange	 =  8;
const  fint  greenyellow =  9;
const  fint  greencyan	 = 10;
const  fint  bluecyan	 = 11;
const  fint  bluemagenta = 12;
const  fint  redmagenta  = 13;
const  fint  darkgray	 = 14;
const  fint  lightgray	 = 15;
static fint  symbol	 =  2;	    /* Take a plus as plot symbol, */
				    /* see PGPLOT MANUAL */
static float xmin;		    /* Plot corners in user units */
static float xmax;
static float ymin;
static float ymax;

/* Miscellaneous */

static fchar	Key, Mes;	    /* USERXXX routine parameters */
static fint	setlevel = 0;	    /* To get header items at set level. */
static float	blank;		    /* Global value for BLANK. */
static char	message[120];	    /* All purpose character buffers. */
static char	unitin[6],unitout[6];
static fint	i,j,ii; 	    /* Various counters. */
static float	minval; 	    /* Min. value of data for each subset. */
static float	maxval; 	    /* Max. value of data for each subset. */
static fint	nblanks;	    /* Number of blanks in each subset. */
static fint	mcount; 	    /* Initialize MINMAX3. */
static fint	change; 	    /* Used in WMINMAX. change!=0 means */
				    /* minimum and maximum have changed and */
				    /* that the MINMAX descriptors at */
				    /* intersecting levels will be removed. */
FILE		*asciifile;	    /* File pointer to ASCII file */
static fint	inerr,outerr,error; /* Error codes */
static fchar	Axname; 	    /* Axis name for Setout */
static double	origin; 	    /* Origin of Setout */
static fint	scanlen;	    /* Scanlength in Setout */
static fint	scannr; 	    /* Scan number in Setout */
static fint	zero=0; 	    /* Pre-defined parameter values */
static fint	one=1;
static fint	two=2;
static fchar	Object,Instru,Coor; /* irds info */
static fint	naxis;
static fint	axes[4];
static double	pos[2],center[2],size[2];
static float	epoche;
static float	year = 1983.5;
static fint	eclsys = 3;
static fint	n, entered, ref, newref = 0, posref = 0;
static fchar	Scantype;	    /* snip info and detector info */
static fint	detno, sop, obs, att, scancal, scandur, snipcal, snipdur;
static float	psi, psirate, theta, yloc, zloc, ysize, zsize;
static double	*lon,*lat,*twist;
static float	*offset,*doffset;   /* scan search and regridding variables */
static float	grid[SCAN_LEN],dgrid[SCAN_LEN];
static float	*data;
static float	splined[SCAN_LEN];
static fint	ndata,nsplined;
static float	incfac;
static double	distance, isoff, xsoff, step, gridstep = -1.0;
static double	mindis;
static fint	closest;
static double	sum[SCAN_LEN];	    /* ADDSCAN variables */
static float	sumreal[SCAN_LEN];
static fint	sumcount = 0;
static fchar	Refobject;
static bool	answer; 	    /* User's answer */
static fchar	Show;		    /* What is plotted */
static float	length=15.0;	    /* Scan length in plot (arcmin)*/
static double	longit, latitu;     /* Longitude and latitude of object */
static fchar	Long, Lati;
static fchar	Unit,Titlea,Titleb; /* Plot labels */
static fchar	Units;		    /* Output units */
static fchar	Unitin,Unitout;     /* Units in upper case */
static double	cdelt;
static double	crval;
static float	acdc=-1.0;	    /* ACDC conversion factor */
static float	minmax[2];	    /* fixed y-axis for plots */
static fint	around=5;	    /* min number of samples around source */



void anyoutC( int dev, char *anyCstr )
/*------------------------------------------------------------------*/
/* The C version of 'anyout_c' needs two parameters:                */
/* an integer and a C-type string. The integer determines	    */
/* the destination of the output which is:			    */
/*    0  use default [set by HERMES to 3 but can be changed by user]*/
/*    1  terminal						    */
/*    2  LOG file						    */
/*    8  terminal, suppressed in "experienced mode"                 */
/*   16  terminal, only when in "test mode"                         */
/*------------------------------------------------------------------*/
{
   fint ldev = (fint) dev;
   anyout_c( &ldev, tofchar( anyCstr ) );
}


FILE *openfile( char *filename )
/*-----------------------------------------------------*/
/* Open file for writing. Ask filename in GIPSY way    */
/* Check file for existence. Return file pointer       */
/* and the name of the given file.		       */
/* The function introduces the keywords FILENAME= and  */
/* APPEND=. The macro 'fmake' and the definitions for  */
/* YES and NO must be available.		       */
/*-----------------------------------------------------*/
{
#include    "stdio.h"
#include    "usertext.h"
#include    "userlog.h"
#include    "cancel.h"
#include    "reject.h"

#define    NAMELEN    80
#define    KEYLEN     20

   fchar     Filename;
   bool      append;
   fint      request = 1;
   fint      dfault;
   fint      nitems;
   fint      agreed;
   fint      exist;
   fint      n;
   fchar     Key, Mes;
   FILE     *fp;

   dfault = request;
   fmake( Filename, NAMELEN );
   fmake( Key, KEYLEN );
   fmake( Mes, NAMELEN );
   do {
      append = toflog(YES);		     /* Default APPEND=Y */
      Key    = tofchar("FILENAME=");
      Mes    = tofchar("Name of ASCII file:     [No output to file]");
      n      = usertext_c( Filename,
			   &dfault,
			   Key,
			   Mes );
      if (n == 0) return NULL;
      strcpy( filename, strtok(Filename.a, " ") );     /* Delete after space */
      fp = fopen(filename, "r");
      exist = (fp != NULL);
      if (exist) {
	 nitems = 1;
	 Key = tofchar("APPEND=");
	 Mes = tofchar("File exists, ok to append?    [Y]/N");
	 n   = userlog_c( &append,
			  &nitems,
			  &dfault,
			  Key,
			  Mes );
	 append = tobool( append );
	 fclose( fp );
	 cancel_c( Key );
      }
      Key = tofchar("FILENAME=");
      if (!append) {
	 cancel_c( Key );
	 agreed = NO;
      } else {
	 fp = fopen(filename, "a");
	 agreed = (fp != NULL);
	 if (!agreed) {
	    reject_c( Key, tofchar("Cannot open, try another!") );
	 } else {
	    if (exist) {
	       fprintf( fp, "\f\n" );
	    } else {
	       fprintf( fp, "\n" );
	    }
	 }
      }
   } while (!agreed);
   return( fp );		/* Return the file pointer */
}


void initplot( void )
/*------------------------------------------------------------------*/
/* Initialize plot software. Set viewport and output dimensions.    */
/* If output device is a printer, ask user for line width.	    */
/*------------------------------------------------------------------*/
{
   fint   unit; 	   /* Ignored by pgbeg, use unit=0. */
   fchar  Devspec;	   /* Device specification. */
   fint   nxysub[2];	   /* Number of subdivisions on 1 page. */
   float  width;	   /* Width of output on paper */
   float  aspect;	   /* Aspect ratio of output on paper */
   float  viewbox[4];	   /* Corners of viewbox. */
   fint   nitems, dfault;
   fint   r1;
   fint   errlev = 4;	   /* Set error level to fatal. */
   bool   pageoff;	   /* Disable PGPLOT's NEXTPAGE keyword. */
   float  paper[2];
   float  xl, xr, yb, yt;  /* Edges of the viewport. */


   /* Begin PGPLOT, open output device. A return value of 1 indicates */
   /* successful completion. There are 4 arguments for PGBEG:	      */
   /* UNIT, this argument is ignored by PGBEG (use zero).	      */
   /* FILE, If this argument is a question mark PGBEG will prompt the */
   /*	    user to supply a string.				      */
   /* NXSUB,the number of subdivisions of the view surface in X.      */
   /* NYSUB,the number of subdivisions of the view surface in Y.      */

   nxysub[1] = nxysub[0] = 1;		/* Default no subdivisions in plot.*/
   nitems = 2;
   dfault = HIDDEN;
   r1 = userint_c( nxysub,
		   &nitems,
		   &dfault,
		   tofchar("PGMOSAIC="),
		   tofchar("View surface sub divisions in x,y:   [1,1]") );

   unit = 0;
   Devspec = tofchar("?");
   r1 = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );
   if (r1 != 1) error_c( &errlev, tofchar("Cannot open output device") );

   /* No PGPLOT's NEXTPAGE= keyword */
   pageoff = toflog( 0 );
   pgask_c( &pageoff );

   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */
   nitems = 2; dfault = HIDDEN;
   paper[0] = 0.0; paper[1] = 1.0;
   r1 = userreal_c( paper,
		    &nitems,
		    &dfault,
		    tofchar("PGPAPER="),
		    tofchar("Give width(cm), aspect ratio: [calculated]") );
   if (r1 > 0) {
      /* If width = 0.0 then the program will select the largest view surface*/
      width  = paper[0] / 2.54;      /* Convert width to inches. */
      aspect = paper[1];
      pgpap_c( &width, &aspect );
   }

   /* Set viewport */
   xl = 0.2; xr = 0.95;
   yb = 0.1; yt = 0.9;
   viewbox[0] = xl; viewbox[1] = xr;	  /* Get size from user input */
   viewbox[2] = yb; viewbox[3] = yt;
   nitems = 4; dfault = HIDDEN;
   sprintf( message,
	  "Viewport of plot:  [%f,%f,%f,%f]", xl,xr,yb,yt );
   r1 = userreal_c( viewbox,
		    &nitems,
		    &dfault,
		    tofchar("PGVIEW="),
		    tofchar( message ) );
   xl = viewbox[0]; xr = viewbox[1];
   yb = viewbox[2]; yt = viewbox[3];
   pgsvp_c( &xl, &xr, &yb, &yt );
}

void drawbox( float Xmin, float Ymin, float Xmax, float Ymax,
	      fchar Unit, fchar Titlea, fchar Titleb )
/*------------------------------------------------------------------*/
/* Draw box and labels. Take special care for the y labels and	    */
/* title. Colors are defined globally. Xmin etc are the corners of  */
/* the box in world coordinates.				    */
/*------------------------------------------------------------------*/
{
   float  charsize = 1.0;
   float  delta;
   fint   lwidth;
   fint   r1;
   fint   nitems;
   fint   dfault;
   fint   color;
   fint   font;
   fint   nxsub, nysub;
   float  xtick, ytick;
   fchar  Xtitle, Ytitle, Topa, Topb;
   float  disp, coord, fjust;		      /* Adjust Y title in pgmtxt. */
   fint   len1, len2;
   char   message[80];


   pgpage_c();			       /* Advance to new page. */

   /* Increase the size of the box a little */
   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Xmin -= delta; Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Ymin -= delta; Ymax += delta;
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );   /* Set the window */

   color = 1; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &color,
		   &nitems,
		   &dfault,
		   tofchar("PGCOLOR="),
		   tofchar("Give color 1..15:        [1]") );
   if (color > 15) color = 15;
   if (color < 1 ) color =  1;
   pgsci_c( &color );

   lwidth = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &lwidth,
		   &nitems,
		   &dfault,
		   tofchar("PGWIDTH="),
		   tofchar("Give line width 1..21:        [2]") );
   if (lwidth > 21) lwidth = 21;
   if (lwidth < 1 ) lwidth =  1;
   pgslw_c( &lwidth );			/* Set line width. */

   charsize = 1.0; nitems = 1; dfault = HIDDEN;
   r1 = userreal_c( &charsize,
		    &nitems,
		    &dfault,
		    tofchar("PGHEIGHT="),
		    tofchar("Give character height:     [1.0]") );
   pgsch_c( &charsize );	       /* Character height. */

   font = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &font,
		   &nitems,
		   &dfault,
		   tofchar("PGFONT="),
		   tofchar("Give font 1..4:        [2]") );
   if (font > 4) font = 4;
   if (font < 1) font = 1;
   pgscf_c( &font );		       /* Set font. */

   /* xtick is world coordinate interval between major tick marks    */
   /* on X axis. If xtick=0.0, the interval is chosen by PGBOX, so   */
   /* that there will be at least 3 major tick marks along the axis. */
   /* nxsub is the number of subintervals to divide the major	     */
   /* coordinate interval into. If xtick=0.0 or nxsub=0, the number  */
   /* is chosen by PGBOX.					     */
   /* BCNSTV :							     */
   /* B: draw bottom (X) or left (Y) edge of frame.		     */
   /* C: draw top (X) or right (Y) edge of frame.		     */
   /* N: write Numeric labels in the conventional location below     */
   /*	 the viewport (X) or to the left of the viewport (Y).	     */
   /* S: draw minor tick marks (Subticks).			     */
   /* T: draw major Tick marks at the major coordinate interval.     */
   /* V: orient numeric labels Vertically. This is only applicable   */
   /*	 to Y.							     */
   xtick = ytick = 0.0;
   nxsub = nysub = 0;
   pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
		   tofchar("BCNSTV"), &ytick, &nysub );

   /* Create titles */

   fmake( Xtitle, 80 ); fmake( Ytitle, 80 );
   fmake( Topa, 80); fmake( Topb, 80);
   Xtitle = tofchar("Offset [arcmin]");
   pglab_c( Xtitle, Unit, tofchar(" ") );

   coord = 0.0; fjust = 0.0; disp = 0.3;
   Topa = Titlea;
   pgmtxt_c( tofchar("T"), &disp, &coord, &fjust, Topa );
   coord = 1.0; fjust = 1.0; disp = 0.3;
   Topb = Titleb;
   pgmtxt_c( tofchar("T"), &disp, &coord, &fjust, Topb );
}


MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.							   */
/*-------------------------------------------------------------------------*/
{
   init_c();				   /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;		   /* Name of current task */
      fmake( Task, 20 );		   /* Macro 'fmake' must be available*/
      myname_c( Task ); 		   /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null */
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );
   fmake( Setin, STRLEN );
   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   dfault  = NONE;
   subdim  = 0;
   showdev = 3;
   Key	   = KEY_INIRDS;
   Mes	   = MES_INIRDS;
   nsubs   = gdsinp_c( Setin,	   /* Name of input set. */
		       subin,	   /* Array containing subsets coord words. */
		       &maxsubs,   /* Maximum number of subsets in 'subin'.*/
		       &dfault,    /* Default code as is USERxxx. */
		       Key,	   /* Keyword prompt. */
		       Mes,	   /* Keyword message for the user. */
		       &showdev,   /* Device number (as in ANYOUT). */
		       axnum,	   /* Array of size 4 containing the axes */
				   /* numbers. The first elements (upto the */
				   /* dimension of the subset) contain the */
				   /* axes numbers of the subset, the other */
				   /* ones contain the axes numbers outside */
				   /* the subset ordered according to the */
				   /* specification by the user. */
		       axcount,    /* Number of grids on axes in 'axnum' */
		       &maxaxes,   /* Max. number of axes. */
				   /* the operation for each subset. */
		       &class,	   /* Class 1 is for applications to repeat */
		       &subdim );  /* Dimensionality of the subsets for */
				   /* class 1 */
   setdim  = gdsc_ndims_c( Setin, &setlevel );
   fmake( Scantype, STRLEN );
   fmake( Object, STRLEN );
   fmake( Instru, STRLEN );
   fmake( Titlea, KEYLEN );
   fmake( Titleb, STRLEN );
   fmake( Unit, KEYLEN );
   fmake( Units, KEYLEN );
   fmake( Unitin, KEYLEN );
   fmake( Unitout, KEYLEN );
   fmake( Coor, KEYLEN );
   irds_enquire_c( Setin, Object, Instru,
		   &naxis, axes, center, size, Coor, &epoche, &inerr );
   if ( inerr < 0 ) {
      error = FATAL;
      error_c( &error, tofchar("Set is not an IRDS.") );
   }
   inerr = 0;
   gdsd_rchar_c( Setin, tofchar("BUNIT"), &setlevel, Unit, &inerr );
   if ( inerr < 0 ) {
      error = WARNING;
      error_c( &error, tofchar("Unit of IRDS data is not understood.") );
      inerr = 0;
      Unit = tofchar("Data");
   }
   sprintf( unitin, "%.*s", nelc_c(Unit), Unit.a );
   for ( ii = 0; ii < nelc_c(Unit); ii++ ) {
       unitin[ii] = toupper(unitin[ii]);
   }
   Unitin = tofchar(unitin);
   ref	   = irco_number_c( Coor, &epoche );
   if (ref < 0) {
      ref = -ref;
      irco_precess_c( &ref, &epoche, &newref );
   } else {
      newref = ref;
   }
   irco_precess_c( &eclsys, &year, &eclsys );
   lon = malloc( axes[0] * axes[1] * sizeof( double ) );
   lat = malloc( axes[0] * axes[1] * sizeof( double ) );
   twist = malloc( axes[0] * axes[1] * sizeof( double ) );
   offset = malloc( axes[0] * axes[1] * sizeof( double ) );
   doffset = malloc( axes[0] * axes[1] * sizeof( double ) );
   data = malloc( axes[0] * axes[1] * sizeof( double ) );
   if ( ( lon == NULL ) || ( lat == NULL ) || ( twist == NULL ) ||
	( offset == NULL ) || ( doffset == NULL ) || ( data == NULL ) ) {
      error = FATAL;
      error_c( &error, tofchar("Failed to allocate working memory!") );
   }

   /*-------------------------------*/
   /* Outset			    */
   /*-------------------------------*/

   fmake( Setout, STRLEN );
   do {
      answer  = TRUE;
      dfault  = REQUEST;
      Key     = KEY_OUTSET;
      Mes     = MES_OUTSET;
      outlen  = usertext_c( Setout, &dfault, Key, Mes );
      outerr = 0;
      if ( gds_exist_c( Setout, &outerr ) ) {
	 dfault = REQUEST;
	 Key	= KEY_OVERW;
	 Mes	= MES_OVERW;
	 n	= userlog_c( &answer, &one, &dfault, Key, Mes );
	 if (answer) {
	    outerr = 0;
	    gds_delete_c( Setout, &outerr );
	 } else {
	    cancel_c( KEY_OUTSET );
	 }
	 cancel_c( KEY_OVERW );
      }
   }
   while (!answer);
   scannr  = 1;
   if (outlen>0) {
      outerr = 0;
      gds_create_c( Setout, &outerr );
      Axname  = AXNAM1;
      origin  = ORIGIN;
      scanlen = SCAN_LEN;
      outerr = 0;
      gds_extend_c( Setout, Axname, &origin, &scanlen, &outerr );
      Axname  = AXNAM2;
      origin  = 1.0;
      outerr = 0;
      gds_extend_c( Setout, Axname, &origin, &scannr, &outerr );
   }
   for ( ii = 0; ii < SCAN_LEN; ii++ ) {
       sum[ii] = 0.0;
   }
   Key = KEY_UNITS;
   Mes = MES_UNITS;
   dfault = HIDDEN;
   outerr = usertext_c( Units, &dfault, Key, Mes );
   if (outerr == 0) Units = tofchar("Jy");
   sprintf( unitout, "%.*s", nelc_c(Units), Units.a );
   for ( ii = 0; ii < nelc_c(Units); ii++ ) {
       unitout[ii] = toupper(unitout[ii]);
   }
   Unitout = tofchar(unitout);

   inerr = 0;
   gdsd_wchar_c( Setout, tofchar("BUNIT"), &setlevel, Unitout, &inerr );


   Key = KEY_ACDC;
   Mes = MES_ACDC;
   dfault = HIDDEN;
   entered = userreal_c( &acdc, &one, &dfault, Key, Mes );
   if (entered == 0) {
      sprintf( message, "%.*s", nelc_c(Instru), Instru.a );
      entered = 9;
      if (strncmp(message,"SURVEY B1",entered)==0) acdc = 1.0/0.78;
      if (strncmp(message,"SURVEY B2",entered)==0) acdc = 1.0/0.82;
      if (strncmp(message,"SURVEY B3",entered)==0) acdc = 1.0/0.92;
      if (strncmp(message,"SURVEY B4",entered)==0) acdc = 1.0;
      if (acdc<0.0) {
	 error = WARNING;
	 error_c( &error, tofchar("No known ACDC default; will use 1.0.") );
	 acdc = 1.0;
      }
   }

   /*--------------------*/
   /* Reference position */
   /*--------------------*/

   dfault  = REQUEST;
   Key	   = KEY_POS;
   Mes	   = MES_POS;
   entered = userangle_c( pos, &two, &dfault, Key, Mes );
   if ( entered < two ) {
      pos[0] = center[0];
      pos[1] = center[1];
   }
   pos[0] = RAD(pos[0]);
   pos[1] = RAD(pos[1]);
/*   irco_torect_c( &pos[0], &pos[1], xyz, &one );
   irco_transform_c( xyz, &newref, xyz, &newref, &one );
   irco_tospher_c( xyz, &pos[0], &pos[1], &one );*/
   irco_plate_c( &newref, pos, Object, &posref );
   fmake( Refobject, STRLEN );
   Key = KEY_OBJECT;
   Mes = MES_OBJECT;
   entered = usertext_c( Refobject, &dfault, Key, Mes );
   if ( entered == 0 ) Refobject = Object;
   Key = KEY_INCFAC;
   Mes = MAS_INCFAC;
   dfault = HIDDEN;
   entered = userreal_c( &incfac, &one, &dfault, Key, Mes );
   if ( entered == 0 ) incfac = 1.0;

   /*------*/
   /* plot */
   /*------*/

   fmake( Show, 2 );
   dfault = REQUEST;
   Key = KEY_SHOW;
   Mes = MES_SHOW;
   sprintf( message, "Show selected scans:" );
   anyoutC( NOVICE, message );
   sprintf( message, "N(one at all)" );
   anyoutC( NOVICE, message );
   sprintf( message, "A(ll without stopping in between)" );
   anyoutC( NOVICE, message );
   sprintf( message, "O(ne by one pausing after each plot)" );
   anyoutC( NOVICE, message );
   sprintf( message, "F(inal ADDSCAN result only) [default]" );
   anyoutC( NOVICE, message );
   entered = usercharu_c( Show, &one, &dfault, Key, Mes );
   Show.a[1] = '\0';
   if ( strcmp( Show.a, "N" ) != 0 ) initplot();
   dfault = HIDDEN;
   Key = KEY_LENGTH;
   Mes = MES_LENGTH;
   entered = userreal_c( &length, &one, &dfault, Key, Mes );
   dfault = HIDDEN;
   Key = KEY_MINMAX;
   Mes = MES_MINMAX;
   entered = userreal_c( minmax, &two, &dfault, Key, Mes );
   if ( entered < two ) {
      minmax[0] = -1e30;
      minmax[1] = 1e30;
   }
   dfault = HIDDEN;
   Key = KEY_AROUND;
   Mes = MES_AROUND;
   entered = userint_c( &around, &one, &dfault, Key, Mes );

   /*-------*/
   /* print */
   /*-------*/

   asciifile = openfile( message );
   sprintf( message, "Object: %.*s", nelc_c( Refobject ), Refobject.a );
   anyoutC( NORMAL, message );
   if (asciifile != NULL) {
      fprintf( asciifile, strcat( message, "\n" ) );
   }
   sprintf( message, "Instrument: %.*s", nelc_c( Instru ), Instru.a );
   anyoutC( NORMAL, message );
   if (asciifile != NULL) {
      fprintf( asciifile, strcat( message, "\n" ) );
   }
   longit = DEG( pos[0] );
   latitu = DEG( pos[1] );
   fmake( Long, KEYLEN );
   fmake( Lati, KEYLEN );
   if ( ref == 1 ) {
      hms_c( &longit, Long, NULL, &one, &zero );
      dms_c( &latitu, Lati, NULL, &zero, &zero );
      sprintf( message, "Position: %.*s %.*s in system %.*s %7.2f",
	       nelc_c( Long ), Long.a, nelc_c( Lati ), Lati.a,
	       nelc_c( Coor ), Coor.a, epoche );
   } else {
      if ( ref == 3 ) {
	 sprintf( message, "Position: %7.2fd %7.2fd in system %.*s %7.2f",
		  longit, latitu, nelc_c( Coor ), Coor.a, epoche );
      } else {
	 sprintf( message, "Position: %7.2fd %7.2fd in system %.*s",
		  longit, latitu, nelc_c( Coor ), Coor.a );
      }
   }
   anyoutC( NORMAL, message );
   if (asciifile != NULL) {
      fprintf( asciifile, strcat( message, "\n" ) );
   }
   sprintf( message,
   "Sc. sop att det In-off  Step   Twist X-off X-d/2 mod. JD IRDS");
   anyoutC( NORMAL, message );
   if (asciifile != NULL) {
      fprintf( asciifile, strcat( message, "\n" ) );
   }
   sprintf( message,
   "                  ['']  ['']   [deg]   [']   [']   [day] Snip Sdet Clo.");
   anyoutC( NORMAL, message );
   if (asciifile != NULL) {
      fprintf( asciifile, strcat( message, "\n" ) );
   }

   /*----------------------------------------------------------*/
   /* Start the main loop over all snips.		       */
   /*----------------------------------------------------------*/

   for ( i = 1; i <= axes[3]; i++ ) {
       sprintf( message, "Working on scan %d out of %d", i, axes[3] );
       status_c( tofchar(message) );
       for ( j = 1; j<= axes[2]; j++ ) {
	   ndata = axes[0] * axes[1];
	   irds_rd_detpos_c( Setin, &i, &j, &one, &posref,
			     &zero, lon, lat, twist, &ndata, &outerr );
	   if ( outerr != 0 ) {
	      sprintf( message, "Error %d at Scan-Det %d %d",
		       outerr, i, j );
	      anyoutC( TEST, message );
	   } else {
	      mindis = PI*PI;
	      for ( n = 0 ; n < ndata ; n++ ) {
		  distance = lon[ n ] * lon[ n ] + lat[ n ] * lat[ n ];
		  if ( distance < mindis ) {
		     mindis = distance;
		     closest = n;
		  }
	      }
	      distance = sqrt( mindis );
	      irds_enquire_snip_c( Setin, &i, &sop, &obs, &att,
				   Scantype, &scancal, &scandur,
				   &snipcal, &snipdur,
				   &psi, &psirate, &theta, &outerr );
	      detno = ircc_detnr_c( &j, Instru );
	      outerr = ircc_mask_c( &detno, &yloc, &zloc,
				    &ysize, &zsize );
	      isoff = lon[ closest ] * cos( twist[ closest ] ) +
		      lat[ closest ] * sin( twist[ closest ] );
	      xsoff = lat[ closest ] * cos( twist[ closest ] ) -
		      lon[ closest ] * sin( twist[ closest ] );
	      if ( ( ABS( isoff ) < RAD( ysize / 60.0 ) / 2.0 ) &&
		 ( ABS( xsoff ) < incfac * RAD( zsize / 60.0 ) / 2.0 ) &&
		 (closest >= around) && ( closest < ndata-around ) ) {
		 sprintf( message, "Scan-Det: %d %d", i, j );
		 anyoutC( TEST, message );
		 step = sqrt( ( lon[ closest+1 ] - lon[ closest-1 ] ) *
			      ( lon[ closest+1 ] - lon[ closest-1 ] ) +
			      ( lat[ closest+1 ] - lat[ closest-1 ] ) *
			      ( lat[ closest+1 ] - lat[ closest-1 ] ) ) / 2.0 ;
		 irds_rd_samples_c( Setin, &i, &j, &one,
				    data, &ndata, &outerr );
		 ii = irc_chunit_c( &detno, data, &ndata, Unitin, Unitout );
		 if (ii<0) Units = tofchar(unitin);
		 for ( ii=0 ; ii<ndata ; ii++ ) {
		     offset[ii] = step * ( ii - closest ) - isoff ;
		     doffset[ii] = DEG( offset[ii] ) * 60.0 ;
		 }
		 if ( gridstep < 0.0 ) gridstep = step / 10.0;
		 for ( ii=0 ; ii<SCAN_LEN ; ii++ ) {
		     grid[ii] = ( ii - SCAN_LEN / 2 ) * gridstep ;
		     dgrid[ii] = DEG( grid[ii] ) * 60.0 ;
		 }
		 minmax1_c( data, &ndata, &ymin, &ymax );
		 /* the minmax1 call above is done only because spline1
		    below has sometimes problems with blank only scans
		    (doesn't detect blanks) */
		 if ( ( ymin != blank ) && ( ymax != blank ) ) {
		    ymin = 1e30 ;
		    for ( ii=closest-around ; ii<=closest+around ; ii++ ) {
		       if ( data[ii]<ymin ) ymin = data[ii];
		    }
		    for ( ii=0 ; ii<ndata ; ii++ ) {
		       data[ii] = acdc * ( data[ii] - ymin ) ;
		    }
		    nsplined = SCAN_LEN ;
		    n = spline1_c( offset, data, &ndata,
				   grid, splined, &nsplined ) ;
		    sprintf( message, "spline1: %d %d", n, nsplined );
		    anyoutC( TEST, message );
		 }
		 if ( ( ymin != blank ) && ( ymax != blank ) &&
		      ( n >= 0 ) && ( n < nsplined ) &&
		      ( detno != 17 ) && ( detno != 20 ) &&
		      ( detno != 25 ) && ( detno != 26 ) &&
		      ( detno != 28 ) && ( detno != 36 ) &&
		      ( detno != 42 ) ) {
		    sprintf( message,
	    "%3d %3d %3d %3d %6.2f %5.2f %7.2f %5.2f %5.2f %8.3f %4d %4d %4d",
		      scannr, sop, att, detno, DEG( isoff ) * 3600.0,
		      DEG( step ) * 3600.0 , DEG( twist[ closest ] ),
		      DEG( xsoff ) * 60.0, zsize/2.0,
		      (scancal+snipcal+(closest+1)/axes[0])/86395.3+5360.5,
		      i, j, closest+1 );
		    anyoutC( NORMAL, message );
		    if (asciifile != NULL) {
		       fprintf( asciifile, strcat( message, "\n" ) );
		       fflush( asciifile );
		    }
		    if ( ( strcmp( Show.a, "O" ) == 0 ) ||
			 ( strcmp( Show.a, "A" ) == 0 ) ) {
		       xmin = -length/2.0 ;
		       xmax =  length/2.0 ;
		       ymin = 0.0 ;
		       ymax = -1e30 ;
		       for ( ii=closest-around ; ii<=closest+around ; ii++ ) {
			  if ( data[ii]>ymax ) ymax = data[ii];
		       }
		       if (minmax[0]>-5e29) ymin = minmax[0];
		       if (minmax[1]<5e29) ymax = minmax[1];
		       Titlea = Refobject;
		       sprintf( message, "sop=%3d att=%3d det=%3d",
				sop, att, detno );
		       Titleb = tofchar(message);
		       drawbox( xmin, ymin, xmax, ymax, Units, Titlea, Titleb );
		       pgpt_c( &ndata, doffset, data, &symbol );
		       pgline_c( &nsplined, dgrid, splined );
		       if ( strcmp( Show.a, "O" ) == 0 ) {
			  dfault = REQUEST;
			  Key = KEY_SHOW;
			  Mes = MES_CONTINUE;
			  cancel_c(Key);
			  n = usercharu_c( Show, &one, &dfault, Key, Mes );
			  if (n>0) Show.a[1] = '\0';
			  sprintf( message, "Working on scan %d out of %d",
				   i, axes[3] );
			  status_c( tofchar(message) );
		       }
		    }
		    scannr += 1;
		    sumcount += 1;
		    for ( ii = 0; ii < SCAN_LEN; ii++ ) {
			sum[ii] += splined[ii];
		    }
		    if (outlen>0) {
		       outerr = 0;
		       gds_extend_c( Setout, Axname, &origin,
				     &scannr, &outerr );
		       tidO = 0 ;
		       cwloO += SCAN_LEN+1 ;
		       cwhiO += SCAN_LEN+1 ;
		       do {
			  pixelswrite = nsplined ;
			  gdsi_write_c( Setout, &cwloO, &cwhiO, splined,
					&nsplined, &pixelswrite, &tidO );
		       }
		       while ( tidO > 0 ) ;
		       minmax3_c( splined, &nsplined,
				  &minval, &maxval,
				  &nblanks, &mcount );
		    }
		 }
	      }
	   }
       }
   }

/*---------*/
/* ADDSCAN */
/*---------*/

   cdelt = DEG( gridstep ) * 60.0;
   inerr = 0;
   gdsd_wdble_c( Setout, tofchar("CDELT1"), &setlevel, &cdelt, &inerr );
   inerr = 0;
   crval = 0.0;
   gdsd_wdble_c( Setout, tofchar("CRVAL1"), &setlevel, &crval, &inerr );
   inerr = 0;
   gdsd_wchar_c( Setout, tofchar("CUNIT1"), &setlevel, tofchar("ARCMIN"), &inerr );


   sprintf( message, "Calculating final ADDSCAN result" );
   status_c( tofchar(message) );
   if ( sumcount>0) {
      n = SCAN_LEN ;
      for ( ii = 0; ii < n; ii++ ) {
	  sumreal[ii] = sum[ii] / sumcount;
      }
      if (outlen>0) {
	 minmax3_c( sumreal, &n,
		    &minval, &maxval,
		    &nblanks, &mcount );
	 tidO = 0 ;
	 cwloO = SCAN_LEN+2 ;
	 cwhiO = 2*SCAN_LEN+1 ;
	 do {
	    pixelswrite = SCAN_LEN ;
	    gdsi_write_c( Setout, &cwloO, &cwhiO, sumreal,
			  &n, &pixelswrite, &tidO );
	 }
	 while ( tidO > 0 ) ;
	 /* Update OUTSET= descriptor with new values */
	 subout = 0;
	 nsubsout = 1;
	 change = YES;
	 wminmax_c( Setout, &subout,
		    &minval, &maxval, &nblanks,
		    &nsubsout, &change );
      }
      if ( strcmp( Show.a, "N" ) != 0 ) {
	 minmax1_c( dgrid, &n, &xmin, &xmax );
	 minmax1_c( sumreal, &n, &ymin, &ymax );
	 if ( incfac != 1.0 ) {
	    sprintf( message, "Modified (incfac =%6.2f) ADDSCAN", incfac );
	 } else {
	    sprintf( message, "ADDSCAN" );
	 }
	 Titleb = tofchar(message);
	 if (minmax[0]>-5e29) ymin = minmax[0];
	 if (minmax[1]<5e29) ymax = minmax[1];
	 drawbox( xmin, ymin, xmax, ymax, Units, Titlea, Titleb );
	 pgline_c( &n, dgrid, sumreal );
      }
   } else {
      error = WARNING;
      error_c( &error, tofchar("No scans selected.") );
      if (outlen>0) {
	 outerr = 0;
	 gds_delete_c( Setout, &outerr );
      }
   }

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opend with fopen  */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.		    */
   /*-------------------------------------------------------*/

   pgend_c();
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
