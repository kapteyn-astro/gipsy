/*
                            COPYRIGHT (c) 1998
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             renzogram.dc1

Program:       RENZOGRAM

Purpose:       Build and execute a GPLOT input file that plots 
               contours per level for different subsets. The contours
               can be plotted on a set displayed in GIDS.

Category:      DYNAMICS, PLOTTING, UTILITY

File:          renzogram.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set and subsets for the colour contours.
               Maximum number of subsets is 1024.
              
               From these subsets, the contours will be plotted in one
               plot. The levels are entered in LEVEL1= etc.
              
             
   BACKSET=    Give set or subset for a background colour map: [no map]
       
               Draw the contours on this image which becomes your
               background.
               Input must be 2-dimensional and the box at BOX= is 
               applied to THIS set. Then the contour map is scaled 
               so that it fits this map. If nothing is entered (default),
               then BOX= applies to INSET=


   BOX=        Give box in .....                        [entire subset]
   
               If BACKSET= has a set name then BOX= applies to this set.
               Else, BOX= applies to the subsets in INSET=


** FILENAME=   Name of GPLOT ascii file:           [GPLOTinputfile.scr]

               This program generates an input file for GPLOT.
               After finishing, it is not erased from disk, so you can
               use it again, or edit it before re-use in GPLOT.
               If you want a hardcopy (COPY=Y) then a second file
               is created with the same name and extra extension '.dump'. 
               This file contains plot commands for a printer or a 
               PostScript file.


    LEVEL1=    Enter contour level:
    LEVEL2=    Enter contour level:                              [skip]
    LEVEL3=    Enter contour level:                              [skip]
    LEVEL4=    Enter contour level:                              [skip]
    LEVEL5=    Enter contour level:                              [skip]
                     
               Enter per LEVELx= keyword ONE contour level in units 
               of the input data at INSET=
               Each level gets its own line style which must be 
               specified with the LTYPE= keyword.
               
               Data is read from each subset and the contour is plotted 
               in a colour depending on the subset number. 
               However, after starting GIDS there are only 16 colours 
               available for the the contours, so some neighbouring 
               subsets get the same colour. The colours are selected
               according the 'Rainbow' colour lookup table. For
               hard copy devices there are 112 colours available.
               
              
    LTYPE=     Enter a line style per contour level:          [1 2 ...]
               The styles are:

               1. full line 
               2. long dashes
               3. dash-dot-dash-dot
               4. dotted
               5. dash-dot-dot-dot
                            
              
** LWIDTH=     Give the line width of the contours:                 [2] 

               This controls how thick your contours will be plotted.
               You can select a number between 1 and 21. 
               

   COPY=       Copy output to hardcopy device?                 Yes/[No]
   
               Send the output to printer or PostScript file.
               Adjust the colours in GIDS if you have a background 
               image. Then use COPY=Y to create a hard copy.
               If COPY=Y , the GRDEVICE= keyword is prompted 
               to specify the device.
  
       
   READY=      You can change the colours for the background set now!
 
               At this point you can adjust the colours of the 
               background set in GIDS. After pressing enter, the
               colours are fixed. If you want to use the generated
               GPLOT file from within GPLOT, then you need to 
               start GIDS also because the GPLOT macro will always
               try to contact GIDS for the colour information.
               
                
   GRDEVICE=   Graphics device           [list of all graphics devices]
              
               Select the device to which you want to send the output.
               This can be a (colour) printer or a (colour) PostScript 
               file. Note that this keyword is also set in hidden mode
               to start VIEW, so for a batch job you have to specify 
               a second device e.g.: GRDEVICE=X11;X11



Description:   RENZOGRAM is a program which displays kinematic and spatial 
               information in a single colour plot. It overlays one 
               (or more) contours per subset over an image (such as an 
               optical image), plotting the contours of each subset with a
               different colour. In this way, the velocity information is 
               color coded and easy to see together with the extension of 
               the features. It is a useful tool to observe changes in 
               inclination of galactic disks (warps). Its most common use is
               to plot a low contour (e.g. 3 sigma), and perhaps a high 
               contour (to see the peak of each channel). The program is an 
               implementation of an old idea of prof. Renzo Sancisi.

Notes:         


Example:       renzogram
               INSET=AURORA freq
               BACKSET=AURORA freq 0 
               BOX=-64 -64 64 64
               CLIP=
               LEVEL1=3
               COPY=y
               GRDEVICE=PCPSFILE/myfile.ps

Updates:       Mar 31, 1998: VOG, Document created.
               Oct 19, 2000: VOG, Add colour coded table with physical 
                                  coordinates.
               Nov 24, 2000: VOG, 'box' in GPLOT file and change of format
                                  in velocity legend.

#<
*/

/*  renzogram.c: include files     */

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


#include    "time.h"
#include    "deputy.h"
#include    "wkey.h"
#include    "subst.h"
#include    "getaxname.h"
#include    "axunit.h"   
#include    "cotrans.h"

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


/* Colors */

#define BACKGROUND    0
#define WHITE         1
#define RED           2
#define GREEN         3
#define BLUE          4
#define CYAN          5
#define MAGENTA       6
#define YELLOW        7
#define ORANGE        8
#define GREENYELLOW   9
#define GREENCYAN    10
#define BLUECYAN     11
#define BLUEMAGENTA  12
#define REDMAGENTA   13
#define DARKGRAY     14
#define REDMAGENTA   13
#define DARKGRAY     14
#define LIGHTGRAY    15

#define MAXCOL       15
#define MAXLEVELS     5

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")


typedef	struct color_struct {
   float	red;
   float	green;
   float	blue;
} Color_struct;


static	Color_struct	rainbow[] = {
   { 0.00000, 0.00000, 0.01176 },
   { 0.00000, 0.00000, 0.02745 },
   { 0.00000, 0.00000, 0.04314 },
   { 0.00000, 0.00000, 0.05882 },
   { 0.00000, 0.00000, 0.07451 },
   { 0.00000, 0.00000, 0.09020 },
   { 0.00000, 0.00000, 0.10588 },
   { 0.00000, 0.00000, 0.12157 },
   { 0.00000, 0.00000, 0.13725 },
   { 0.00000, 0.00000, 0.15294 },
   { 0.00000, 0.00000, 0.16863 },
   { 0.00000, 0.00000, 0.18431 },
   { 0.00000, 0.00000, 0.20000 },
   { 0.00000, 0.00000, 0.21176 },
   { 0.00000, 0.00000, 0.22745 },
   { 0.00000, 0.00000, 0.24314 },
   { 0.00000, 0.00000, 0.25882 },
   { 0.00000, 0.00000, 0.27451 },
   { 0.00000, 0.00000, 0.29020 },
   { 0.00000, 0.00000, 0.30588 },
   { 0.00000, 0.00000, 0.32157 },
   { 0.00000, 0.00000, 0.33725 },
   { 0.00000, 0.00000, 0.35294 },
   { 0.00000, 0.00000, 0.36863 },
   { 0.00000, 0.00000, 0.38431 },
   { 0.00000, 0.00000, 0.40000 },
   { 0.00000, 0.00000, 0.41176 },
   { 0.00000, 0.00000, 0.42745 },
   { 0.00000, 0.00000, 0.44314 },
   { 0.00000, 0.00000, 0.45882 },
   { 0.00000, 0.00000, 0.47451 },
   { 0.00000, 0.00000, 0.49020 },
   { 0.00000, 0.00000, 0.50588 },
   { 0.00000, 0.00000, 0.52157 },
   { 0.00000, 0.00000, 0.53725 },
   { 0.00000, 0.00000, 0.55294 },
   { 0.00000, 0.00000, 0.56863 },
   { 0.00000, 0.00000, 0.58431 },
   { 0.00000, 0.00000, 0.60000 },
   { 0.00000, 0.00000, 0.61176 },
   { 0.00000, 0.00000, 0.62745 },
   { 0.00000, 0.00000, 0.64314 },
   { 0.00000, 0.00000, 0.65882 },
   { 0.00000, 0.00000, 0.67451 },
   { 0.00000, 0.00000, 0.69020 },
   { 0.00000, 0.00000, 0.70588 },
   { 0.00000, 0.00000, 0.72157 },
   { 0.00000, 0.00000, 0.73725 },
   { 0.00000, 0.00000, 0.75294 },
   { 0.00000, 0.00000, 0.76863 },
   { 0.00000, 0.00000, 0.78431 },
   { 0.00000, 0.00000, 0.80000 },
   { 0.00000, 0.00000, 0.81176 },
   { 0.00000, 0.00000, 0.82745 },
   { 0.00000, 0.00000, 0.84314 },
   { 0.00000, 0.00000, 0.85882 },
   { 0.00000, 0.00000, 0.87451 },
   { 0.00000, 0.00000, 0.89020 },
   { 0.00000, 0.00000, 0.90588 },
   { 0.00000, 0.00000, 0.92157 },
   { 0.00000, 0.00000, 0.93725 },
   { 0.00000, 0.00000, 0.95294 },
   { 0.00000, 0.00000, 0.96863 },
   { 0.00000, 0.00000, 0.98431 },
   { 0.00000, 0.00000, 1.00000 },
   { 0.00000, 0.03529, 1.00000 },
   { 0.00000, 0.07059, 1.00000 },
   { 0.00000, 0.10980, 1.00000 },
   { 0.00000, 0.14510, 1.00000 },
   { 0.00000, 0.18039, 1.00000 },
   { 0.00000, 0.21961, 1.00000 },
   { 0.00000, 0.25490, 1.00000 },
   { 0.00000, 0.29412, 1.00000 },
   { 0.00000, 0.32941, 1.00000 },
   { 0.00000, 0.36471, 1.00000 },
   { 0.00000, 0.40392, 1.00000 },
   { 0.00000, 0.43922, 1.00000 },
   { 0.00000, 0.47843, 1.00000 },
   { 0.00000, 0.50196, 1.00000 },
   { 0.00000, 0.52549, 1.00000 },
   { 0.00000, 0.54902, 1.00000 },
   { 0.00000, 0.57255, 1.00000 },
   { 0.00000, 0.59608, 1.00000 },
   { 0.00000, 0.61961, 1.00000 },
   { 0.00000, 0.64314, 1.00000 },
   { 0.00000, 0.66667, 1.00000 },
   { 0.00000, 0.69020, 1.00000 },
   { 0.00000, 0.71373, 1.00000 },
   { 0.00000, 0.73725, 1.00000 },
   { 0.00000, 0.76078, 1.00000 },
   { 0.00000, 0.78431, 1.00000 },
   { 0.00000, 0.80000, 1.00000 },
   { 0.00000, 0.81569, 1.00000 },
   { 0.00000, 0.83137, 1.00000 },
   { 0.00000, 0.84706, 1.00000 },
   { 0.00000, 0.86667, 1.00000 },
   { 0.00000, 0.88235, 1.00000 },
   { 0.00000, 0.89804, 1.00000 },
   { 0.00000, 0.91373, 1.00000 },
   { 0.00000, 0.93333, 1.00000 },
   { 0.00000, 0.94902, 1.00000 },
   { 0.00000, 0.96471, 1.00000 },
   { 0.00000, 0.98039, 1.00000 },
   { 0.00000, 1.00000, 1.00000 },
   { 0.00000, 1.00000, 0.97647 },
   { 0.00000, 1.00000, 0.95294 },
   { 0.00000, 1.00000, 0.92941 },
   { 0.00000, 1.00000, 0.90588 },
   { 0.00000, 1.00000, 0.88627 },
   { 0.00000, 1.00000, 0.86275 },
   { 0.00000, 1.00000, 0.83922 },
   { 0.00000, 1.00000, 0.81569 },
   { 0.00000, 1.00000, 0.79608 },
   { 0.00000, 1.00000, 0.77255 },
   { 0.00000, 1.00000, 0.74902 },
   { 0.00000, 1.00000, 0.72549 },
   { 0.00000, 1.00000, 0.70588 },
   { 0.00000, 1.00000, 0.65098 },
   { 0.00000, 1.00000, 0.59608 },
   { 0.00000, 1.00000, 0.54118 },
   { 0.00000, 1.00000, 0.48627 },
   { 0.00000, 1.00000, 0.43137 },
   { 0.00000, 1.00000, 0.37647 },
   { 0.00000, 1.00000, 0.32549 },
   { 0.00000, 1.00000, 0.27059 },
   { 0.00000, 1.00000, 0.21569 },
   { 0.00000, 1.00000, 0.16078 },
   { 0.00000, 1.00000, 0.10588 },
   { 0.00000, 1.00000, 0.05098 },
   { 0.00000, 1.00000, 0.00000 },
   { 0.05098, 1.00000, 0.00000 },
   { 0.10588, 1.00000, 0.00000 },
   { 0.16078, 1.00000, 0.00000 },
   { 0.21569, 1.00000, 0.00000 },
   { 0.27059, 1.00000, 0.00000 },
   { 0.32549, 1.00000, 0.00000 },
   { 0.37647, 1.00000, 0.00000 },
   { 0.43137, 1.00000, 0.00000 },
   { 0.48627, 1.00000, 0.00000 },
   { 0.54118, 1.00000, 0.00000 },
   { 0.59608, 1.00000, 0.00000 },
   { 0.65098, 1.00000, 0.00000 },
   { 0.70588, 1.00000, 0.00000 },
   { 0.72549, 1.00000, 0.00000 },
   { 0.74902, 1.00000, 0.00000 },
   { 0.77255, 1.00000, 0.00000 },
   { 0.79608, 1.00000, 0.00000 },
   { 0.81569, 1.00000, 0.00000 },
   { 0.83922, 1.00000, 0.00000 },
   { 0.86275, 1.00000, 0.00000 },
   { 0.88627, 1.00000, 0.00000 },
   { 0.90588, 1.00000, 0.00000 },
   { 0.92941, 1.00000, 0.00000 },
   { 0.95294, 1.00000, 0.00000 },
   { 0.97647, 1.00000, 0.00000 },
   { 1.00000, 1.00000, 0.00000 },
   { 0.99608, 0.97647, 0.00000 },
   { 0.99608, 0.95686, 0.00000 },
   { 0.99608, 0.93333, 0.00000 },
   { 0.99608, 0.91373, 0.00000 },
   { 0.99216, 0.89412, 0.00000 },
   { 0.99216, 0.87059, 0.00000 },
   { 0.99216, 0.85098, 0.00000 },
   { 0.99216, 0.82745, 0.00000 },
   { 0.98824, 0.80784, 0.00000 },
   { 0.98824, 0.78824, 0.00000 },
   { 0.98824, 0.76471, 0.00000 },
   { 0.98824, 0.74510, 0.00000 },
   { 0.98824, 0.72549, 0.00000 },
   { 0.98824, 0.70588, 0.00000 },
   { 0.98824, 0.68627, 0.00000 },
   { 0.98824, 0.66667, 0.00000 },
   { 0.98824, 0.64706, 0.00000 },
   { 0.99216, 0.62745, 0.00000 },
   { 0.99216, 0.60784, 0.00000 },
   { 0.99216, 0.58824, 0.00000 },
   { 0.99216, 0.56863, 0.00000 },
   { 0.99608, 0.54902, 0.00000 },
   { 0.99608, 0.52941, 0.00000 },
   { 0.99608, 0.50980, 0.00000 },
   { 0.99608, 0.49020, 0.00000 },
   { 1.00000, 0.47059, 0.00000 },
   { 1.00000, 0.43137, 0.00000 },
   { 1.00000, 0.39608, 0.00000 },
   { 1.00000, 0.36078, 0.00000 },
   { 1.00000, 0.32549, 0.00000 },
   { 1.00000, 0.28627, 0.00000 },
   { 1.00000, 0.25098, 0.00000 },
   { 1.00000, 0.21569, 0.00000 },
   { 1.00000, 0.18039, 0.00000 },
   { 1.00000, 0.14118, 0.00000 },
   { 1.00000, 0.10588, 0.00000 },
   { 1.00000, 0.07059, 0.00000 },
   { 1.00000, 0.03529, 0.00000 },
   { 1.00000, 0.00000, 0.00000 },
   { 1.00000, 0.00000, 0.00000 },
   { 1.00000, 0.00000, 0.00000 },
   { 1.00000, 0.00000, 0.05098 },
   { 1.00000, 0.00000, 0.10588 },
   { 1.00000, 0.00000, 0.16078 },
   { 1.00000, 0.00000, 0.21569 },
   { 1.00000, 0.00000, 0.27059 },
   { 1.00000, 0.00000, 0.32549 },
   { 1.00000, 0.00000, 0.37647 },
   { 1.00000, 0.00000, 0.43137 },
   { 1.00000, 0.00000, 0.48627 },
   { 1.00000, 0.00000, 0.54118 },
   { 1.00000, 0.00000, 0.59608 },
   { 1.00000, 0.00000, 0.65098 },
   { 1.00000, 0.00000, 0.70588 },
   { 1.00000, 0.00000, 0.72549 },
   { 1.00000, 0.00000, 0.74902 },
   { 1.00000, 0.00000, 0.77255 },
   { 1.00000, 0.00000, 0.79608 },
   { 1.00000, 0.00000, 0.81569 },
   { 1.00000, 0.00000, 0.83922 },
   { 1.00000, 0.00000, 0.86275 },
   { 1.00000, 0.00000, 0.88627 },
   { 1.00000, 0.00000, 0.90588 },
   { 1.00000, 0.00000, 0.92941 },
   { 1.00000, 0.00000, 0.95294 },
   { 1.00000, 0.00000, 0.97647 },
   { 1.00000, 0.00000, 1.00000 },
   { 1.00000, 0.03529, 1.00000 },
   { 1.00000, 0.07059, 1.00000 },
   { 1.00000, 0.10588, 1.00000 },
   { 1.00000, 0.14118, 1.00000 },
   { 1.00000, 0.18039, 1.00000 },
   { 1.00000, 0.21569, 1.00000 },
   { 1.00000, 0.25098, 1.00000 },
   { 1.00000, 0.28627, 1.00000 },
   { 1.00000, 0.32549, 1.00000 },
   { 1.00000, 0.36078, 1.00000 },
   { 1.00000, 0.39608, 1.00000 },
   { 1.00000, 0.43137, 1.00000 },
   { 1.00000, 0.47059, 1.00000 },
   { 1.00000, 0.48627, 1.00000 },
   { 1.00000, 0.50588, 1.00000 },
   { 1.00000, 0.52157, 1.00000 },
   { 1.00000, 0.54118, 1.00000 },
   { 1.00000, 0.56078, 1.00000 },
   { 1.00000, 0.57647, 1.00000 },
   { 1.00000, 0.59608, 1.00000 },
   { 1.00000, 0.61176, 1.00000 },
   { 1.00000, 0.63137, 1.00000 },
   { 1.00000, 0.65098, 1.00000 },
   { 1.00000, 0.66667, 1.00000 },
   { 1.00000, 0.68627, 1.00000 },
   { 1.00000, 0.70588, 1.00000 },
   { 1.00000, 0.74510, 1.00000 },
   { 1.00000, 0.78824, 1.00000 },
   { 1.00000, 0.83137, 1.00000 },
   { 1.00000, 0.87059, 1.00000 },
   { 1.00000, 0.91373, 1.00000 },
   { 1.00000, 0.95686, 1.00000 },
   { 1.00000, 1.00000, 1.00000 },
};


typedef struct
{
   fchar    name;                   /* Name of input set */
   char     buf[STRLEN];
   fint     subin[MAXSUBSETS];      /* Subset coordinate words */
   fint     axnum[MAXAXES];         /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered according to the */
                                    /* specification by the user. */
   fint     axcount[MAXAXES];       /* Array of size MAXAXES containing the */
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
   fint     subdim;                 /* Dimensionality of the subsets for class 1 applications */
   fint     setdim;                 /* Dimension of set. */
   fint     flo[MAXAXES];           /* Low  edge of frame in grids */
   fint     fhi[MAXAXES];           /* High edge of frame in grids */
   fint     blo[MAXAXES];           /* Low  edge of box in grids */
   fint     bhi[MAXAXES];           /* High edge of box in grids */   
   fint     nsubs;                  /* Number of subsets entered */
} settype;


/* Miscellaneous globals */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static char     message[STRLEN];    /* All purpose character buffer. */




static void createaxisheader( fchar  Set,
                              fint   subset,
                              fint  *axnum,
                              char  *label )
/*------------------------------------------------------------*/
/* PURPOSE: Create a text containing the physical coordinate  */
/*          units and axis names outside a subset.            */
/*------------------------------------------------------------*/
{
   fint  setlevel = 0;
   fint  setdim, subdim;
   int   i;
   fint  r;
   fchar Axname;
   char  axname[20+1];
   fchar Axunit;
   char  axunit[20+1];
    
   
   setdim = gdsc_ndims_c( Set, &setlevel );
   subdim = gdsc_ndims_c( Set, &subset ); 
   strcpy( label, "" );   
   
   if (setdim == subdim)                            /* Must be some exception */
   {
      return;
   }
 
   Axname.l = 20;
   Axname.a = axname;
   Axunit.l = 20;
   Axunit.a = axunit;   
   
   for (i = subdim; i < setdim; i++)
   {
      fint   chop = 1;
      getaxname_c( Set, &axnum[i], &chop, Axname ); 
      Axname.a[nelc_c(Axname)] = '\0';
      r = axunit_c( Set, &axnum[i], Axunit ); 
      Axunit.a[nelc_c(Axunit)] = '\0';
      strcat( label, axname );
      strcat( label, " (" );
      strcat( label, axunit );
      strcat( label, ")" );        
      if ( i < setdim - 1 )
         strcat( label, ", " );
   } 
}



static void getset( settype  *inset, 
                    fint     dfault,
                    char     *key,
                    char     *mes,
                    int      maxsubsets )
/*------------------------------------------------------------*/
/* PURPOSE: */
/*------------------------------------------------------------*/
{
   fint     maxsubs = maxsubsets;
   fint     maxaxes = MAXAXES;           /* Max num. of axes the program can deal with.*/
   fint     class   = 1;                 /* Class 1 is for applications which repeat */
   fint     showdev = 1;
   
     
   inset->name.a = inset->buf;
   inset->name.l = STRLEN-1;
   inset->subdim = 2;                    /* Allow only 2-dim structures */
   inset->nsubs = 
           gdsinp_c( inset->name,        /* Name of input set. */
                     inset->subin,       /* Array containing subsets coordinate words. */
                     &maxsubs,           /* Maximum number of subsets in 'subin'.*/
                     &dfault,            /* Default code as is USERxxx. */
                     tofchar(key),       /* Keyword prompt. */
                     tofchar(mes),       /* Keyword message for the user. */
                     &showdev,           /* Device number (as in ANYOUT). */
                     inset->axnum,       /* Array of size 'maxaxes' containing the axes numbers. */
                                         /* The first elements (upto the dimension of the subset) */
                                         /* contain the axes numbers of the subset, */
                                         /* the other ones contain the axes numbers */
                                         /* outside the subset ordered according to the */
                                         /* specification by the user. */
                     inset->axcount,     /* Number of grids on axes in 'axnum' */
                     &maxaxes,           /* Max. number of axes. */
                                         /* the operation for each subset. */
                     &class,             /* Class 1 is for applications which repeat */
                     &(inset->subdim) ); /* Dimensionality of the subsets for class 1 */

   if (inset->nsubs <= 0)
   {
      /* No set specified */
      return;
   }
   inset->setdim  = gdsc_ndims_c( inset->name, &setlevel );

   /*-------------------------------*/
   /* Determine edges of this frame */
   /*-------------------------------*/
   {
      fint cwlo, cwhi;                          /* Local coordinate words */
      int  m;
      fint r2, r1 = 0;
      gdsc_range_c( inset->name, &setlevel, &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) inset->setdim; m++)
      {
         inset->flo[m] = gdsc_grid_c( inset->name, &(inset->axnum[m]), &cwlo, &r1 );
         inset->fhi[m] = gdsc_grid_c( inset->name, &(inset->axnum[m]), &cwhi, &r2 );
      }
   }
   
}




static void drawcontours( fint   nsubs, 
                          FILE   *fp, 
                          float  *levels, 
                          fint   numlev,
                          fint   *ltypes,
                          fint   linewidth,                          
                          int    indxmin,
                          int    indxmax )
/*------------------------------------------------------------*/
/* PURPOSE: Draw contour levels for all subsets in one plot.  */
/*------------------------------------------------------------*/
{
   int   i, j;
            
   fprintf( fp, "lwidth %d\n", linewidth );           /* Set GPLOT line width */
   for (i = 0; i < nsubs; i++)                       /* Loop over all subsets */
   {
      /*--------------------------------------------------*/
      /* Calculate a colour index between 'indxmin' and   */
      /* 'indxmax' and calculate an array index between   */
      /* 0 and 255 for the colour lut Rainbow.            */
      /*--------------------------------------------------*/
      int  col = i*(indxmax-indxmin)/(nsubs-1) + indxmin;
      int  indx = (int) ( ((float)i/(float)nsubs) * 255.0);
      fprintf( fp, "colrep %d %f %f %f\n", col, 
               rainbow[indx].red,
               rainbow[indx].green, 
               rainbow[indx].blue );
      fprintf( fp, "subset %d\n", i );      
      fprintf( fp, "colour %d\n", col );
      for (j = 0; j < numlev; j++)
      {
         fprintf( fp, "lstyle %d\n", ltypes[j] );
         fprintf( fp, "levels %f\n", levels[j] );      
         fprintf( fp, "contours\n" );
      }
   }
   fprintf( fp, "lwidth 2\n"  );   
}



static void physicalcoords( fchar  Setin, 
                            fint   subset, 
                            fint   *axnum, 
                            char   *label )
/*------------------------------------------------------------*/
/* PURPOSE: Get the physical values of the axes outside the   */
/*          current subset.                                   */
/*------------------------------------------------------------*/
{
   fint   setlevel = 0;
   fint   setdim, subdim;
   int    i;
   fint   r;
   double grid[MAXAXES];
   double phys[MAXAXES];
   fint   dir = 1;              /* grid to phys */
    
   
   setdim = gdsc_ndims_c( Setin, &setlevel );
   subdim = gdsc_ndims_c( Setin, &subset ); 
   strcpy( label, "" );   
   
   if (setdim == subdim)                            /* Must be some exception */
   {
      return;
   }
   for (i = 0; i < subdim; i++) 
   {
      grid[i] = 0.0;
   }
   for (i = subdim; i < setdim; i++)    
   {
      fint   r2 = 0;

      grid[i] = (double) gdsc_grid_c( Setin, &axnum[i], &subset, &r2 );       
   }
   r = cotrans_c( Setin, &subset, grid, phys, &dir );
   for (i = subdim; i < setdim; i++)
   {
      char    number[128];
      sprintf( number, "%.5g", phys[axnum[i]-1] );
      strcat( label, number );
      if ( i < setdim - 1 )
         strcat( label, ", " ); 
   }
}




static void drawlegend( fchar  Setin,
                        fint   *subset,
                        fint   *axnum,
                        fint   nsubs,
                        fint   *blo,
                        fint   *bhi,
                        FILE   *fp, 
                        fint   linewidth,                          
                        int    indxmin,
                        int    indxmax )
/*------------------------------------------------------------*/
/* PURPOSE: Draw coloured legend.                             */
/*------------------------------------------------------------*/
{
   int   i, line;
   char  label[256];         
   float deltaY = (bhi[1] - blo[1] + 1) / 35.0;
   float deltaX = (bhi[0] - blo[0] + 1) / 50.0;   
   float x , y;
   int   deltaS;
   
           
   fprintf( fp, "lwidth %d\n", linewidth );           /* Set GPLOT line width */
   
   x = (float) bhi[0] + deltaX;
   y = (float) bhi[1];   
   fprintf( fp, "move %f %f\n", x , y+deltaY );
   fprintf( fp, "colour %d\n", 1 );                   /* In foreground colour */
   createaxisheader( Setin, subset[0], axnum, label );   
   fprintf( fp, "charhei 3.0\n" );
   fprintf( fp, "text %s\n", label );
   
   deltaS = 1 + nsubs / 35;
   

   for (i = 0, line = 0; i < nsubs; i++)                       /* Loop over all subsets */
   {
      if (!(i%deltaS))
      {
         /*--------------------------------------------------*/
         /* Calculate a colour index between 'indxmin' and   */
         /* 'indxmax' and calculate an array index between   */
         /* 0 and 255 for the colour lut Rainbow.            */
         /*--------------------------------------------------*/
         int  col = i*(indxmax-indxmin)/(nsubs-1) + indxmin;
         int  indx = (int) ( ((float)i/(float)nsubs) * 255.0);
         fprintf( fp, "colrep %d %f %f %f\n", col, 
                  rainbow[indx].red,
                  rainbow[indx].green, 
                  rainbow[indx].blue );
         fprintf( fp, "colour %d\n", col );
         fprintf( fp, "move %f %f\n", x , y-((float)(line+1)*deltaY)+deltaY/3.0 );
         fprintf( fp, "draw %f %f\n", x-3.0*deltaX , y-((float)(line+1)*deltaY)+deltaY/3.0 );
         fprintf( fp, "move %f %f\n", x , y-((float)(line+1)*deltaY) );      
         physicalcoords( Setin, subset[i], axnum, label );      
      fprintf( fp, "colour foreground\n" );
         fprintf( fp, "text %s\n", label );
         line++;
      }
   }
   fprintf( fp, "lwidth 2\n"  );   
}




static void stamp( FILE *fp )
/*------------------------------------------------------------*/
/* PURPOSE: Create an id in the header of the GPLOT file.     */
/*------------------------------------------------------------*/
{
   char    mes[STRLEN];
   
   /* First line in GPLOT input file is a comment */
   if (fp != NULL)
   {
      struct tm   *ptr;
      time_t      lt;
            
      lt    = time(NULL);                   /* Get the coded calendar time */
      ptr   = localtime(&lt);
      strftime( mes, STRLEN, "%d-%b-%Y (%H:%M:%S)", ptr );
      fprintf( fp, "!----------------------------------------------------------\n" );
      fprintf( fp, "! %s GPLOT input file generated by GIPSY program RENZOGRAM\n", mes );
      fprintf( fp, "!----------------------------------------------------------\n" );      
   }
}




MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint     dfault;                      /* Default option for input etc */
   fint     nitems;
   FILE     *asciifile;                  /* File pointer to ASCII file */
   char     writefile[STRLEN+1];
   char     dumpfile[STRLEN+1];   
   fint     status;
   fchar    Insetstr;
   fchar    Backsetstr;   
   settype  cset;                        /* The contour set/subsets */
   settype  bset;                        /* The background image */
   int      backset;
   fint     r;
   float    levels[MAXLEVELS];   
   fint     ltypes[MAXLEVELS];
   fint     numlev;
   bool     hardcopy;
   fint     linewidth;
   fint     *boxlo, *boxhi;
   
  
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


   getset( &cset, 
           NONE, 
          "INSET=", "Enter set/subsets for contours",
           MAXSUBSETS );

   getset( &bset, 
           REQUEST, 
          "BACKSET=", "Enter one subset as background:    [none]",   
           1 );

   /*-------------------------------*/
   /* Prepare grid ranges for INSET */
   /*-------------------------------*/
   {
      fint     boxopt = 0;
      fint     showdev = 1;
      settype  *dumset;
      
      if (bset.nsubs != 0)
      {
         dumset = &(bset);
         backset = YES;
         boxlo = bset.blo;
         boxhi = bset.bhi;         
      }
      else
      {
         dumset = &(cset);
         backset = NO;
         boxlo = cset.blo;
         boxhi = cset.bhi;
      }

      dfault = REQUEST;
      gdsbox_c( dumset->blo, 
                dumset->bhi, 
                dumset->name, 
                dumset->subin, 
                &dfault, 
                KEY_BOX, MES_BOX, 
                &showdev, 
                &boxopt );
   }

   /*--------------------------------------------------*/
   /* At this point we activate GIDS as plot device.   */
   /* There are two possibilities now: first we have a */
   /* background set. This set must be displayed and   */
   /* gids must be opened by GPLOT in append mode.     */
   /*--------------------------------------------------*/
   if (backset)
   {
      fint    r = 0;
      subst_c( tofchar( "INSET=BACKSET=" ) , &r );
      
      /* BOX= for VIEW is copied from gdsbox call. */
      /* CLIP= is asked by program VIEW. */
      deputy_c( tofchar("VIEW"), &status );      
   }



   /*--------------------------------------------------*/
   /* Open the GPLOT input file                        */
   /*--------------------------------------------------*/
   {
      char   mes[STRLEN];
      fchar  Filename;
      int    agreed = YES;
 
      Filename.a = writefile;
      Filename.l = STRLEN;
      strcpy( writefile, "GPLOTinputfile.scr" );
      sprintf( mes, "Name of GPLOT ascii file:           [%s]", writefile );

      dfault = HIDDEN;
      nitems = 1;
      do
      {         
         agreed = YES;
         r = usertext_c( Filename, &dfault, tofchar("FILENAME="), tofchar(mes) );      
         Filename.a[nelc_c(Filename)] = '\0';
         asciifile = fopen( writefile, "w" );
         if (asciifile == NULL)
         {
            reject_c( tofchar("FILENAME="), 
                      tofchar("Cannot open for writing, try another!") );
            agreed = NO;
         }
      }
      while (!agreed);

      if (asciifile != NULL)
      {
         stamp( asciifile );
      }      
   }
   dfault = HIDDEN;
   fmake( Insetstr, STRLEN );
   fmake( Backsetstr, STRLEN );   
   (void) usertext_c( Insetstr, &dfault, tofchar("INSET="), tofchar(" ") );
   (void) usertext_c( Backsetstr, &dfault, tofchar("BACKSET="), tofchar(" ") );
   if (!backset)
   {      
      fprintf( asciifile, "device gids\n" );      
      fprintf( asciifile, "inset %.*s\n", nelc_c(Insetstr), Insetstr.a );
      fprintf( asciifile, "box %d %d %d %d\n",
                           boxlo[0], boxlo[1], boxhi[0], boxhi[1] ); 
      fprintf( asciifile, "autoscale\n" );
      fprintf( asciifile, "frame\n" );      
   }
    
   if (backset)
   {
      /* GIDS is running and is displaying the background set */
      fprintf( asciifile, "device gids//append\n" );
      fprintf( asciifile, "gids\n" );
      fprintf( asciifile, "frame\n" );
      fprintf( asciifile, "inset %.*s\n", nelc_c(Insetstr), Insetstr.a );
      fprintf( asciifile, "overbox\n" );
      fprintf( asciifile, "overlay on\n" );
   }
   
   /*--------------------------------------------------*/
   /* At this point we opened GIDS as device. A box    */
   /* and scale is set and we can start to plot        */
   /* contours from the subsets. Ask level(s) first.   */
   /*--------------------------------------------------*/
      
   nitems = 1;
   dfault = NONE;
   numlev = userreal_c( &levels[0], &nitems, &dfault, tofchar("LEVEL1="),
                        tofchar("Enter first contour level:") );

   do
   {
      char    key[FITSLEN];
      dfault = REQUEST;
      sprintf( key, "LEVEL%d=", numlev+1 );
      sprintf( message, "Enter %dth contour level:    [skip]", numlev+1 );
      r = userreal_c( &levels[numlev], &nitems, &dfault, tofchar(key), tofchar(message) );
      numlev += r;
   }
   while (r && numlev < MAXLEVELS);

   /* Ask the line styles */
   {
      int  i;
      char meslong[MAXLEVELS*5];
      strcpy( meslong, "[1" );
      ltypes[0] = 1;
      for (i = 1; i < numlev; i++)
      {
         ltypes[i] = i+1;
         sprintf( message, " %d", i+1 );
         strcat( meslong, message );
      }
      strcat( meslong, "]" );
      nitems = numlev;
      r = userint_c( ltypes, &nitems, &dfault, tofchar("LTYPE="),
                     tofchar(meslong) );
   }


   nitems = 1;
   dfault = HIDDEN;
   linewidth = 2;
   r = userint_c( &linewidth, &nitems, &dfault, tofchar("LWIDTH="),
                  tofchar("Give the line width of the contours:        [2]") );


   drawcontours( cset.nsubs, asciifile, levels, numlev, ltypes, linewidth,
                 RED, MAXCOL );
   drawlegend( cset.name, cset.subin, cset.axnum,
               cset.nsubs, boxlo, boxhi,
               asciifile, linewidth, RED, MAXCOL ); 

   fclose( asciifile );
   sprintf( message, "COMMAND=input %s ; quit", writefile );  
   wkey_c( tofchar(message) );
   deputy_c( tofchar("GPLOT"), &status );

   nitems = 1;
   dfault = REQUEST;
   hardcopy = toflog( NO );   
   r = userlog_c( &hardcopy, &nitems, &dfault, 
                  tofchar("COPY="), 
                  tofchar("Copy output to hardcopy device?     Yes/[No]") );
   hardcopy = tobool( hardcopy );
   if (hardcopy)
   {
      sprintf( dumpfile, "%s.dump", writefile );
      asciifile = fopen( dumpfile, "w" );      
      stamp( asciifile );
      fprintf( asciifile, "device\n" );            /* Ask device */            
      if (!backset)
      {      
         fprintf( asciifile, "inset %.*s\n", nelc_c(Insetstr), Insetstr.a );
         fprintf( asciifile, "box %d %d %d %d\n", 
                  boxlo[0], boxlo[1], boxhi[0], boxhi[1] );         
         fprintf( asciifile, "autoscale\n" );         
         fprintf( asciifile, "frame\n" );      
      }    
      else
      {
         bool ready = toflog( YES );
         nitems = 1;
         dfault = REQUEST;
         r = userlog_c( &ready, &nitems, &dfault, tofchar("READY="), 
                        tofchar("You can change the colours for the background set now!") );
         cancel_c( tofchar("READY=") );
         /* GIDS is running and displaying the background set */
         fprintf( asciifile, "inset %.*s\n", nelc_c(Backsetstr), Backsetstr.a );
         fprintf( asciifile, "getlut\n" );
         fprintf( asciifile, "box %d %d %d %d\n", 
                  boxlo[0], boxlo[1], boxhi[0], boxhi[1] );
         fprintf( asciifile, "xmargin 2.0\n" );
         fprintf( asciifile, "ymargin 2.0\n" );         
         fprintf( asciifile, "autoscale\n" );      /* Get background set from GIDS */
         fprintf( asciifile, "colplot\n" );
         fprintf( asciifile, "frame\n" );
         fprintf( asciifile, "inset %.*s\n", nelc_c(Insetstr), Insetstr.a );
         fprintf( asciifile, "overbox\n" );
         fprintf( asciifile, "overlay on\n" );
      }      
      /*--------------------------------------------------*/
      /* For a colour device, the maximum colour index is */
      /* 127 (see pgdriv.src). Let's not change the       */
      /* definitions of the first 16 indices.             */
      /*--------------------------------------------------*/
      drawcontours( cset.nsubs, asciifile, levels, numlev, ltypes, linewidth,
                    16, 127 );      
      drawlegend( cset.name, cset.subin, cset.axnum, 
                  cset.nsubs, boxlo, boxhi, 
                  asciifile, linewidth, 16, 127 ); 
      fclose( asciifile );
      sprintf( message, "COMMAND=input %s ; quit", dumpfile );  
      wkey_c( tofchar(message) );
      deputy_c( tofchar("GPLOT"), &status );
   }     
 
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
