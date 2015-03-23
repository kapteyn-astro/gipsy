/*
                            COPYRIGHT (c) 1995
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             ellprof.dc1

Program:       ELLPROF

Purpose:       Create an output set with profiles extracted from an
               input set at positions in subsets that are either
               1) defined by ellipses, 2) entered manually or
               3) entered with the graphics cursor.


Category:      ANALYSIS, PROFILES, VELOCITY-FIELDS, PLOTTING

File:          ellprof.c

Author:        M.G.R. Vogelaar

Keywords:

 **TABSET=     Name of set with table to display:                [SKIP]

               You can display a table with ellipse parameters
               generated in a previous run of ELLPROF. The table is
               stored in the header of the output set that was created.


   INSET=      Give input set (, subsets):

               Maximum number of subsets is 2048.
               Dimension of subset(s) must be 2.
               Dimension of set must be at least 3.


   PROFAXIS=   Give name of profile axis:         [Next available axis]

               This keyword is only asked if the dimension of the
               set is greater than 3 (the ambiguous case). If the
               set dimension is 3 then the profile axis is always the
               only axis that does not belong to the input subsets.


   BOX=        Give box in .....                        [entire subset]


   OVERLAY=    Overlay positions in GIDS?                         [Y]/N

               With OVERLAY=Y it is possible to overlay ellipses
               and/or sample positions in GIDS. The overlay option
               must also be set if you want to generate positions
               with the graphics cursor.

               If GIDS is not running or it is running with an
               incompatible image then OVERLAY=Y  will spawn task VIEW.
               The INSET= keyword is not cancelled so the program VIEW
               starts GIDS and displays what's in INSET= (you can also
               expect VIEWs' keywords CLIP= and NEXT=).


   SAMPLES=    1) Ellipse(s),  2) Manual,  3) Cursor:           [1]/2/3
               or, if GIDS is not available:
               1) Ellipse(s),  2) Manual:                         [1]/2

               (SAMPLES=1) Generate sample positions on an ellipse
               and specify keywords CENTRE=, UNITS=, MAJOR=, INCL=,
               PA=, and RANGE= to define the ellipse.
               (SAMPLES=2) Get the positions manually (for example with
               recall files).
               (SAMPLES=3). Get the positions from GIDS with the
               graphics cursor.
               For options 2 and 3, it is possible to generate
               positions (INTERPOS=Y) between input positions, so that
               the generated positions have distance DELTA= in units
               given by UNITS=. Image values are interpolated for non-
               integer grid positions. Positions outside BOX= generate
               blanks.



               The ellipse keywords:
               =====================

   CENTRE=     Give central position of ellipses:                 [x,y]

               Input is a position in grids or physical coordinates.
               If a transformation from physical values to
               grids is possible then x,y is the projection centre
               (0,0) in grids (corresponding to the transformed CRVAL
               header values).
               Else, x,y is the centre of the map.

   UNITS=      Give units for axis length:               [header units]

               The units must be the header units or units that the
               program can convert to these header units. The units
               can be abbreviated as long as the input is unambiguous.
               Valid input is also GRIDS or PIXELS. Then the lengths
               are all given in grids. If a conversion is possible,


   MAJOR=      (Rad.#) Give length of MAJOR axes in "units": [end loop]

               # is the number of the current radius.
               The keywords MAJOR=, INCL= and PA= are all
               asked in a loop which is aborted if carriage return is
               pressed for MAJOR=.
               The max. number of radii that can be specified in ONE(!)
               MAJOR= keyword is restricted (max 128). This construct-
               ion enables you to specify the axes, inclinations and
               position angles in one keyword, or to enter them using
               a Hermes recall file. For each radius, one output subset
               is created.


   INCL=       Give inclinations (deg) of these ellipses:
                  or, if number of entered major axes is 1:
               Give inclination (deg) of this ellipse:

               The inclination in degrees determines the length of the
               minor axis: minor = major*cos(INCL=).
               If you give less inclinations than radii in MAJOR=, the
               missing inclinations are copied from the last one.


   PA=         Give P.A. (deg) of these ellipses:
                  or, if number of entered major axes is 1:
               Give P.A. (deg) of this ellipse:

               If possible (both subset axes are spatial), the program
               automatically corrects this angle if the y-axis of your
               map does not align with the direction of the north (i.e.
               header says: CROTA <> 0).
               It is assumed that the spatial latitude axis carries the
               rotation information. If nothing is found in the header,
               0 degrees is assumed.
               If you give less angles than radii in MAJOR=, the missing
               angles are copied from the last one.


   RANGE=      Give sample angles 'start,end,step' (deg):     [0 360 2]
               
               Define the sample positions on the ellipse. The
               start and end angles are wrt. the position of the major
               axis. The end angle is NOT included.
               The third value is the step size in degrees.
               The step is a constant angle along the ellipse (not along
               the enclosing circle).

   IPOLSIZE=   Enter (odd) size in x & y of interpolation matrix: [7 7] 
               
               Usually positions are non integer. Therefore an 
               interpolation must be applied using neighbouring pixels.
               The interpolation is a spline interpolation. First 
               the rows in the matrix are interpolated for the non integer 
               x position and with the results a spline interpolation is 
               applied for the non integer y of a position.
               The sizes must be odd numbers and cannot exceed
               the size of your map. The interpolation function can
               deal with blank values.
               

               Manual or cursor input:
               ======================

   INTERPOS=   Interpolate between input positions?               Y/[N]

               INTERPOS=N: The positions entered manually or with
                           the cursor are the sample positions.
               INTERPOS=Y: Use the positions entered manually or with
                           the cursor as begin and end points of lines
                           along which positions are interpolated
                           so that the separations between these
                           generated positions equals DELTA=

   DELTA=      Give step size for interpolation in units:

               Give separation between positions along a line between
               two input positions in units of UNITS=. If the entered
               value is less than or equal to 0, there is no inter-
               polation of positions.


 **COLOUR=     Marker colour (number or abbrv. name):             [red]   
   
               The positions pointed by the graphics cursor will
               be marked in this colour. The colour is entered as
               a number between 1 and 15 (see notes) or as a string
               which represent a colour. The colours are listed in 
               the description. Colour strings can be abbreviated.
               Example: Set marker colour to yellow:
               COLOUR=7
               COLOUR=Yellow
               COLOUR=yel


   XY=         Give Position x, y:                           [end loop]

               In manual mode (SAMPLE=2), you enter positions in either
               grids or physical coordinates. This keyword can be used
               together with a Hermes recall file. The keyword is asked
               in a loop which is aborted by pressing carriage return.

               Examples:
               XY=10 20                  Position in GRIDS.
               XY=* 10 12 8 * -67 8 9.6  Physical coordinates for spatial
                                         map in HMS DMS format in epoch
                                         given by the header.


   MORE=       Continue with another subset with samples?         Y/[N]
               You can create another output subset with profiles
               if you use MORE=Y. Then you re-enter the loop with XY=.
               The number of XY= positions is now limited by the
               first loop. If less are entered, profiles with blanks
               are generated.


   FILENAME=   Give name of ASCII file:                       [No file]

               Write positions generated with SAMPLES=2 or SAMPLES=3
               to a file on disk. If a transformation to physical
               coordinates is possible, write these coordinates also.


   OVERWRITE=  File exists, ok to overwrite?                      [Y]/N

               Only prompted if FILENAME= is an existing file.



               The output set:
               ==============

   OUTSET=     Give name of output set:

               The output set has at least 3 axes. The first is a new
               axis called PARAM-THETA with length equal to the
               number of selected positions. The second axis is the
               profile axis of the input set and the third axis is
               a new axis PARAM-RADIUS with length equal to the
               number of radii for the ellipse option or equal to
               the number of sample sessions (MORE=) for the manual or
               cursor options. Other (non subset) axes are copied
               from the input set. The axis names of the output set
               are displayed in the log-file.


   DELETE=     Delete this set?                                    [NO]
               If OUTSET= already exists, you delete this set with
               DELETE=Y. With DELETE=NO, you are prompted with
               OUTSET= again.



Description:   INTRODUCTION
               ============

               Program ELLPROF has two functions. First: It creates
               an output set with profiles with starting points at
               (SAMPLES=1) positions on a user given ellipse, at
               (SAMPLES=2) positions entered by the user or (SAMPLES=3)
               at positions entered with the graphics cursor.
               Second, ELLPROF reads a GDS table from the header of
               a set previously made by ELLPROF with option SAMPLES=1.
               The ellipse characteristics are displayed if you started
               ELLPROF with TABSET=<name of set>.


               CONSTRUCTION OF THE OUTPUT SET
               ==============================

               The set from which you want to extract profiles is INSET=
               and must be at least 3-dimensional.
               This set must be given as a number of two-dimensional
               subsets. Examples: 1) Set AURORA has axis RA, DEC, FREQ
               and you want to define ellipses in the RA-DEC plane, then
               INSET=AURORA FREQ
               2) Set NGCX has axes RA, DEC, FREQ, PARAM and you want to
               define ellipses in the RA-DEC plane, then
               INSET=NGCX FREQ PARAM. This defines RA-DEC subsets in both
               FREQ and PARAM directions.

               In this last example it is not clear whether you want
               profiles in the FREQ or PARAM direction. In such cases you
               are prompted with keyword PROFAXIS=  If you enter
               PROFAXIS=FREQ, then the profiles are taken in the FREQ
               direction. The subsets are repeated in the PARAM direction.

               The output set given with OUTSET= is a new set with axes:
               1) PARAM-THETA
               2) The profile axis
               3) PARAM-RADIUS
               4) A copy of all remaining axes in INSET= which are not
                  part of the subsets and are not defined as the profile
                  axis.

               For the second example an output set gets the axes:
               PARAM-THETA   FREQ   PARAM-RADIUS   PARAM.

               The meaning of THETA and RADIUS is different for the
               options SAMPLES=1 and SAMPLES=2,3. The range of the
               PARAM-THETA axis is 1..n where n is the number of sample
               positions. The range of the PARAM-RADIUS axis is 1..m
               where m is the number of defined ellipses for SAMPLES=1
               or the number of sample sessions (keyword MORE=) for
               SAMPLES=2 or 3 (manual or cursor input).

               It is possible to write only a part of the profile
               to the output set, e.g. INSET=NGCX FREQ 2:60 PARAM
               will write frequencies 2 to 60 to the output. However
               the length of the profile axis will remain the same as
               in the input set.


               GENERATION OF SAMPLE POSITIONS
               ==============================

               1) Ellipse(s):

               There are 3 options for the position input. If SAMPLES=1
               the positions are generated on an ellipse with centre
               CENTRE=. Input can be either in grids or in physical
               coordinates. Before giving the lengths of the major axes
               you see keyword UNITS=. This keyword has a default equal
               to the header units if the units of both subset axes are
               equal. Otherwise the default units are GRIDS. If you want
               to work in grids whatever the default is, use UNITS=GRIDS
               or UNITS=PIXELS. The units that you enter can be
               abbreviated as long as it is unambiguous. For example:
               if header units is DEGREE then the following units are
               allowed:
               UNITS=               (degrees)
               UNITS=DE             (degrees)
               UNITS=ARCM           (minutes of arc)
               UNITS=ARCS           (seconds of arc)

               After valid input of units unequal to grids, the screen
               displays the conversion between units and grids vv.
               The major axis (MAJOR=) is given in units of UNITS=.
               The number of radii given with MAJOR= sets the number
               of output subset. There is one subset for each radius.
               The inclination (INCL=) in degrees determines the length
               of the minor axis.
               The position angle (PA= in degrees) of the major axis of
               a galaxy is defined as the angle taken in anti-clockwise
               direction between the north direction in the sky and the
               major axis of the receding half of that galaxy (Rots 1975)
               astron, astrophys 45, 43.

               The keywords MAJOR=, INCL= and PA= are prompted in a loop
               to enable the use of a 'recall' file. However, per
               MAJOR= keyword it is possible to give a maximum of 128
               radii at once. But the other loop keywords INCL= and PA=
               expect as many numbers as radii in MAJOR=. Missing
               numbers will be copied from the last one.
               The upper limit for the total number of radii is restricted
               by memory only.

               Finally the keyword RANGE= sets the ellipse sampling.
               It RANGE= accepts 3 numbers. First and second  numbers
               are angles in degrees with respect to the position angle
               of the major axis. Sampling starts at the first angle
               and ends at the last. The last angle is not included.
               The third number is a step size in degrees.


               2) manual input:

               For the manual or cursor input of positions, there are
               two options. The first option (INTERPOS=N) uses the
               positions entered by the user. If INTERPOS=Y, the
               entered positions are used to generate positions each
               separated DELTA= units (as in UNITS=) on an imaginary
               line through all the entered points, so the entered
               points need not to be part of the generated positions.

               For SAMPLES=2, the manual input, positions are entered
               with XY= (one position x, y per keyword). The keyword is
               prompted in a loop, so a recall file with positions can
               be used. The positions can be entered as grids or
               physical coordinates using prefixes as:

               U        Numbers in header units
               *        for RA or DEC in resp. HMS and DMS in EPOCH of set
               *1950    for RA or DEC in resp. HMS and DMS in EPOCH 1950.0
               *xxxx.x  for RA or DEC in resp. HMS and DMS in EPOCH xxxx.x
               G        Galactic longitude or latitude in degrees
               E        Ecliptic longitude or latitude in degrees
               S        Supergalactic longitude or latitude in degrees

               Physical coordinates are transformed for the first input
               subset only.
               For the manual input the loop is aborted if carriage return
               is pressed.
               
               NOTE: Positions outside the box in BOX= do not participate!


               3) cursor:

               With the cursor it is possible to enter positions with
               the left button or any keyboard key except 'Q'/'q'. The
               input loop is aborted if the right button is pressed or
               keyboard key 'Q' or 'q'.

               You can continue with entering more positions with MORE=Y.
               The first set of profiles is put into the first subset
               of the output, the second set in the second subset, etc.
               The maximum number of entered or interpolated positions
               in the first run is limited by memory only. In later
               runs it is limited by the number of positions of the first
               run.

               For both manual and cursor input, there is a possibility to
               write the used positions to an ASCII file on disk. You are
               prompted with FILENAME= which expects a file name or
               carriage return if you do not want to write to disk.
               For each output subset a header is written with the subset
               number. Then 2 or 4 columns with data follow.
               The first column in the file is a grid position
               in X direction, the second is a grid position in Y direction.
               If a transformation to header units is possible, then also
               these transformed physical coordinates are written where
               both columns are prefixed with a 'U' to indicate that the
               units are header units (in most cases: DEGREE).

               NOTE: Positions outside the box in BOX= do not participate!
               

               USING GIDS FOR OVERLAYS
               =======================

               It is possible to overlay the defined ellipses and/or to
               plot the input sample positions on a subset displayed in
               GIDS (OVERLAY=Y). If OVERLAY=Y and and GIDS is not
               started or GIDS displays an incompatible set then the task
               VIEW is spawned with set specification equal to INSET=
               (keyword is not cancelled by ELLPROF). VIEW can ask
               keywords CLIP= and NEXT=. Keyword NEXT= allows you to
               browse through the subsets. In an overlay, a frame is
               plotted for the box as found in GIDS. Also two lines with 
               length as given in BOX= are plotted through the centre of 
               the map. If you cannot read the labels, use the zoom
               option in GIDS to rescale and rerun ELLPROF. For option
               SAMPLES=1, also the direction of the north is plotted.
               (the North arrow is an indication whether your map is
               rotated or not, the rotation is given by CROTA in the header)
               and all the specified ellipses are plotted including the
               major axis. If the pixel aspect ratio is not equal to 1
               (CDELTs in header are unequal), you get a warning that
               the plotted angles are not factuous because you are
               introducing a compression in x or y direction. The position
               of the major axis does not coincide with the major
               axis of the (contorted) plotted ellipse.


               INTERPOLATION OF POSITIONS FOR IMAGE DATA
               =========================================

               Ellipse sample positions or positions entered manually or
               with the cursor, usually do not coincide with the centre of
               a grid. Therefore an interpolation of image data is
               performed. Image data is taken from the input set at the four
               integer neighbouring positions of an input position.
               If all positions are within the input box (BOX=) and
               all image data is not blank then a bilinear interpolation
               is applied. For three valid neighbours, a plane is
               constructed to do the interpolation and for two positions,
               the interpolation is linear.


               MINIMUM/MAXIMUM UPDATE OF THE OUTPUT
               ====================================

               The header values DATAMIN, DATAMAX, and BLANKS are
               updated in the output set after a call to task MNMX.
               The log file will show you the calculated values.


               GIPSY LOG FILE USE OF HERMES OUTPUT LEVEL
               =========================================

               The information that ELLPROF generates can be controlled
               by setting the Hermes output level. If the level is NORMAL
               (usually the default) information is written to terminal
               and log file, warnings are written to the terminal and
               debug information is not displayed. If the level is set to
               EXPERT, all kind of information and warnings will not
               be displayed. With level TEST, also debug information
               is written to the terminal.


               UNITS RECOGNIZED BY GIPSY
               =========================

               DEGREE      ARCSEC      ARCMIN      RADIAN
               CIRCLE      METER       ANGSTROM    NM
               MICRON      MM          CM          INCH
               FOOT        YARD        M           KM
               MILE        PC          KPC         MPC
               TICK        SECOND      MINUTE      HOUR
               DAY         YEAR        HZ          KHZ
               MHZ         GHZ         M/S         MM/S
               CM/S        KM/S        K           MK
               JY          MJY         TAU


               INPUT FOR COLOUR KEYWORD
               ========================

               index  name
               =============================================
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
               
               
Notes:         If you started GIDS on another machine than the one on 
               which you run CURSOR, then cursor actions can be very slow!

Example:       .......

Updates:       Jan 10,  1995: VOG, Document created.
               Jul 07,  1999: VOG, Improved interpolation in data by using
                                   a 2d-spline interpolation in 'getipval'.

#<
*/

/*  ellprof.c: include files     */

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
                             /* 0 use default [set by HERMES to 3] */
                             /* 1  terminal                        */
                             /* 2  LOG file                        */
                             /* 8  terminal, suppressed in "experienced mode" */
                             /* 16  terminal, only when in "test mode". */
#include    "status.h"       /* Display info in the "RUNNING" status display */
#include    "userfio.h"      /* Easy-C companions for GIPSY user interface routines */
#include    "setfblank.h"    /* Function to set a data value to the universal BLANK.*/
#include    "setdblank.h"    /* Set data value to a double type blank.*/
#include    "dblank.h"       /* Is a value a double type blank? */
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "getpath.h"      /* Gets full pathname of a file. It tries to resolve links.*/

/* User input routines */

#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "userchar.h"
#include    "usercharu.h"
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/
#include    "dcdint.h"       /* Decodes a string of characters into integers. */


/* Input of sets */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gds_exist.h"    /* Test whether set exists. */
#include    "gds_delete.h"   /* Delete set. */
#include    "gds_create.h"   /* Create a new set. */
#include    "gds_extend.h"   /* Create or extend an axis. */
#include    "gds_close.h"    /* Close set. */
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_axnum.h"   /* Return axis number of a specified axis */
#include    "axtype.h"       /* Return the type of axis, the natural etc. */
#include    "gdsc_name.h"    /* Return the name of an axis. */
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid. */
                             /* value for each axis. */
#include    "gdsd_rdble.h"   /* Read and write routines for set header. */
#include    "gdsd_rint.h"
#include    "gdsd_rchar.h"
#include    "gdsd_wdble.h"
#include    "gdsd_wint.h"
#include    "gdsd_wchar.h"


/* Data IO */

#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "gdsi_write.h"   /* Writes data to (part of) an set. */


/* Gids related */

#include    "gdi_iinfo.h"    /* Obtains info about GDS image loaded in DISPLAY SERVER. */
#include    "gdi_open.h"     /* Opens a display. */
#include    "gdi_open2.h"    /* Opens a d. only if the d. server is already running. */
#include    "gdi_frame.h"    /* Obtains info about frame currently on display. */
#include    "gdi_close.h"    /* Closes an opened display device. */


/* PGPLOT includes */

#include    "pgplot.h"


/* Table related: */

#include    "gdsa_crecol.h"
#include    "gdsa_wcint.h"
#include    "gdsa_wcdble.h"
#include    "gdsa_wcchar.h"
#include    "gdsa_rcint.h"
#include    "gdsa_rcdble.h"
#include    "gdsa_rcchar.h"
#include    "gdsa_colinq.h"
#include    "gdsa_tabinq.h"


/* Miscellaneous */

#include    "minmax3.h"      /* Find min, max and #blanks in subset. */
#include    "wminmax.h"      /* Writes (new) minimum and maximum and number */
                             /* of blanks of subsets in the descriptor file */
                             /* and optionally deletes the MINMAX descriptors */
                             /* at intersecting levels. */
#include    "cotrans.h"      /* Transformation from grid coordinates to */
                             /* physical coordinates and vice versa. */
#include    "factor.h"       /* Factor returns the conversion factor between */
                             /* two different units (i.e. M en KM). */
#include    "wkey.h"         /* Write keywords to task's own parameter list */
#include    "deputy.h"       /* Start a task which temporarily assumes the */
                             /* role of the calling task. */
#include    "getusernam.h"   /* Returns the user name of the current user. */
#include    "getdate.h"      /* Returns the current time and date as a text string. */
#include    "wmatch.h"       /* Matches a test string with mask which */
                             /* contains wildcards. */
#include    "spline1.h"      /* 1D cubic spline interpolation. */
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

/* Malloc version of 'fmake', but now space must be freed */
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }


/* Miscellaneous defines */

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )
#define PI             3.141592653589793
#define RAD(a)         ( (a) * 0.017453292519943295769237 )
#define DEG(a)         ( (a) * 57.295779513082320876798155 )

#define RELEASE        "1.0"           /* Version number */
#define MAXAXES        10              /* Max. axes in a set */
#define MAXSUBSETS     2048            /* Max. allowed subsets */
#define MAXRADS        512             /* Max number of radii to read in one time */
#define STRLEN         120             /* Max length of strings */
#define KEYLEN         20              /* Max length of keywords */
#define FITSLEN        20
#define ITEMLEN        8
#define VARLEN         132
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0


/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_FILENAME   tofchar("FILENAME=")
#define KEY_OVERWRITE  tofchar("OVERWRITE=")
#define KEY_XY         tofchar("XY=")
#define KEY_BOX        tofchar("BOX=")

/* Defines for columns in a GDS table. */

#define COL_RADNUM     tofchar("RADNUM")
#define COL_RADIUS     tofchar("RADIUS")
#define COL_PA         tofchar("PA")
#define COL_INCL       tofchar("INCL")
#define COL_SRANGE     tofchar("SRANGE")
#define COL_ORIGIN     tofchar("ORIGIN")
#define COL_CONFACT    tofchar("CONFACT")
#define COL_CROTA      tofchar("CROTA")
#define COL_SETNAME    tofchar("SETNAME")
#define COL_IDSTR      tofchar("IDSTR")
#define COL_DATESTR    tofchar("DATESTR")

#define TABNAME        tofchar("ELLIPSES")


/* Defines for deputy tasks */

#define TSK_VIEW       tofchar("view")
#define TSK_MNMX       tofchar("mnmx")


/* Colours: */

#define   BACKGROUND    0      /* Color definitions for PGPLOT. */
#define   FOREGROUND    1      /* Black if background is white. */
#define   RED           2
#define   GREEN         3
#define   BLUE          4
#define   CYAN          5
#define   MAGENTA       6
#define   YELLOW        7
#define   ORANGE        8
#define   GREENYELLOW   9
#define   GREENCYAN    10
#define   BLUECYAN     11
#define   BLUEMAGENTA  12
#define   REDMAGENTA   13
#define   DARKGRAY     14
#define   LIGHTGRAY    15


/* Variables for input */

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
static fint     maxsubs = MAXSUBSETS;
static fint     maxaxes = MAXAXES;  /* Max num. of axes the program can deal with.*/


/* Box and frame related */

static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */


/* Reading data */

static float    **image = NULL;     /* 2-dim matrix with floats */
static float     *Xin = NULL;
static float     *Yin = NULL;
static float     *Xinp = NULL;
static float     *Yinp = NULL;
static fint       IPxsize, IPysize;     /* Data interpolation vector sizes */


/* PGPLOT variables */

static int   pgopen = NO;


/* Miscellaneous */

static fchar    Key, Mes;
static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static char     message[STRLEN];    /* All purpose character buffer. */



static char *strip( fchar Str, char stripch )
/*------------------------------------------------------------------*/
/* Return C string copy of fchar stripped at 'stripch'.             */
/*------------------------------------------------------------------*/
{
   fint   len = nelc_c( Str );
   int    i = 0;

   while (i < len && Str.a[i] != stripch)
     i++;
   i = MYMIN( i, Str.l - 1 );
   Str.a[i] = '\0';
   return( Str.a );
}



static void errorC( int level,
                    char *str )
/*------------------------------------------------------------------*/
/* The C version of 'error'.                                        */
/*------------------------------------------------------------------*/
{
   fint   flev = (fint) level;
   error_c( &flev, tofchar( str ) );
}



static void setcol( fint color )
/*------------------------------------------------------------------*/
/* The C version of pgsci_c.                                        */
/*------------------------------------------------------------------*/
{
   pgsci_c( &color );
}



static void setmarkers( fint  len,
                        float *Xarray,
                        float *Yarray,
                        fint  color,
                        fint  symbol )
/*------------------------------------------------------------------*/
/* The C version of pgpt_c.  Restore old colour after call.         */
/*------------------------------------------------------------------*/
{
   pgsci_c( &color );
   pgpt_c( &len, Xarray, Yarray, &symbol );
}



static float getimval( int  xi,
                       int  yi,
                       fint *blo,
                       fint *bhi )
/*------------------------------------------------------------------*/
/* PURPOSE: Get a value from 'image'. Function can only be used     */
/*          after a call to gdsi_read. The global value of 'blank'  */
/*          must be set.                                            */
/*------------------------------------------------------------------*/
{
   if (xi >= blo[0] && xi <= bhi[0] &&
       yi >= blo[1] && yi <= bhi[1] )
      return( image[yi][xi] );
   else
      return( blank );
}



static int wmatchC( char *test, 
                    char *mask )
/*------------------------------------------------------------*/
/* PURPOSE: Matches a test string with the mask string which  */
/* contains wildcards. wmatchC returns non-zero if matched,   */
/* zero if not. It is set to case insensitive and the wild-   */
/* card character is '*'.                                     */
/*------------------------------------------------------------*/
{
      fint  casesensitive = NO;
         
      return( wmatch_c(tofchar(test),    /* String to test agains MASK. */
                       tofchar(mask),    /* String which contains the massk */
                       tofchar("*"),     /* Character which represents wildcard */
                       &casesensitive) );/* If zero, matching is case insensitive */
}



static fint askcolor( void )
/*------------------------------------------------------------------*/
/* PURPOSE: Get colour from user. COLOUR= can be either a number    */
/*          between 1 and 15 or a (abbreviated) string.             */
/*------------------------------------------------------------------*/
{
   char   *mess = NULL;
   int    len;
   int    i;
   fint   one = 1;
   fint   err;
   fint   dfault, nitems;
   fint   r1;
   fint   color;
   fchar  Color;
  
 

   fmake( Color, 16 );
   dfault = HIDDEN;
   nitems = 1;
   r1 = usercharu_c( Color, &nitems, &dfault, 
                     tofchar("COLOUR="),
                     tofchar("Marker colour (number or abbrv. name):    [red]") );
                     
   if (!r1)                              /* The default */
      return( RED );


   /* Try to convert input */
   
   r1 = dcdint_c( Color, &color, &one, &err );
   if (r1 == 1)
   {
      if (color > 15)
         color = 1;
      if (color < 0)
         color = 0;
      return( color );
   }


   /* No default, no number, perhaps a string? */
   
   len  = nelc_c( Color );
   mess = malloc( len + 2 );          
   for (i = 0; i < len; i++)           /* Copy */
     mess[i] = Color.a[i];
   mess[len] = '*';                    /* Add wildcard */
   mess[len+1] = '\0';

   color = 1;                          /* Initialize */
   if      (wmatchC("background",  mess) )
      color = 0;
   else if (wmatchC("black",       mess) )
      color = 0;      
   else if (wmatchC("default",     mess) )
      color = 1;
   else if (wmatchC("red",         mess) )
      color = 2;
   else if (wmatchC("green",       mess) )
      color = 3;
   else if (wmatchC("blue",        mess) )
      color = 4;
   else if (wmatchC("cyan",        mess) )
      color = 5;
   else if (wmatchC("magenta",     mess) )
      color = 6;
   else if (wmatchC("yellow",      mess) )
      color = 7;
   else if (wmatchC("orange",      mess) )
      color = 8;
   else if (wmatchC("greenyellow", mess) )
      color = 9;
   else if (wmatchC("greencyan",   mess) )
      color = 10;
   else if (wmatchC("bluecyan",    mess) )
      color = 11;
   else if (wmatchC("bluemagenta", mess) )
      color = 12;
   else if (wmatchC("redmagenta",  mess) )
      color = 13;
   else if (wmatchC("darkgray",    mess) )
      color = 14;
   else if (wmatchC("LightGray",   mess) )
      color = 15;
   
   free( mess );
   return( color );
}



static int initplot( fint  *GIDSblo,
                     fint  *GIDSbhi,
                     float *GIDSflo,
                     float *GIDSfhi )
/*------------------------------------------------------------------*/
/* PURPOSE: Initialize plot software. Set viewport and output       */
/*          dimensions.                                             */
/* INPUT:   GIDSblo, GIDSbhi, GIDSflo, GIDSfhi                      */
/* Start PGPLOT, open output device. A return value of 1 indicates  */
/* successful completion. There are 4 arguments for PGBEG:          */
/* UNIT, this argument is ignored by PGBEG (use zero).              */
/* FILE, If this argument is a question mark PGBEG will prompt the  */
/*       user to supply a string.                                   */
/* NXSUB,the number of subdivisions of the view surface in X.       */
/* NYSUB,the number of subdivisions of the view surface in Y.       */
/*------------------------------------------------------------------*/
{
   fint   unit;                  /* Ignored by pgbeg, use unit=0. */
   fchar  Devspec;               /* Device specification. */
   fint   nxysub[2];             /* Number of subdivisions on 1 page. */
   fint   r1;
   int    i;
   bool   pageoff;               /* Disable PGPLOT's NEXTPAGE keyword. */
   float  xl, xr, yb, yt;        /* Edges of the viewport. */
   float  Gblo[2], Gbhi[2];


   nxysub[1] = nxysub[0] = 1;    /* Default no subdivisions in plot.*/
   unit      = 0;
   Devspec   = tofchar("gids//append");
   r1        = pgbeg_c( &unit, Devspec, &nxysub[0], &nxysub[1] );
   if (r1 != 1)
   {
      anyoutf( 1, "Cannot open output device" );
      return( NO );
   }

   /* No PGPLOT's NEXTPAGE= keyword */
   pageoff = toflog( NO );
   pgask_c( &pageoff );

   /*--------------------------------------------------------------*/
   /* If a displayed set is zoomed, the box will keep              */
   /* its original values and the frame is adjusted. To keep a box */
   /* within the frame, adjust the box values.                     */
   /*--------------------------------------------------------------*/
   for (i = 0; i < 2; i++)
   {
      Gblo[i] = (float) GIDSblo[i];
      Gbhi[i] = (float) GIDSbhi[i];


      if (Gblo[i] < GIDSflo[i])
         Gblo[i] = (float) ( (int) GIDSflo[i] );
      if (Gbhi[i] > GIDSfhi[i])
         Gbhi[i] = (float) ( (int) GIDSfhi[i] );
   }

   xl = (Gblo[0] - GIDSflo[0]) / (GIDSfhi[0] - GIDSflo[0]);
   xr = (Gbhi[0] - GIDSflo[0]) / (GIDSfhi[0] - GIDSflo[0]);
   yb = (Gblo[1] - GIDSflo[1]) / (GIDSfhi[1] - GIDSflo[1]);
   yt = (Gbhi[1] - GIDSflo[1]) / (GIDSfhi[1] - GIDSflo[1]);
   pgsvp_c( &xl, &xr, &yb, &yt );                       /* Set viewport */
   pgswin_c( &Gblo[0], &Gbhi[0], &Gblo[1], &Gbhi[1]);   /* Set the window */
   return( YES );
}


static void getoutsetname( fchar Setout )
/*------------------------------------------------------------*/
/* PURPOSE: Get a name for the output set.                    */
/*------------------------------------------------------------*/
{
   fint   r1;
   fint   nitems, dfault;
   fint   err;
   int    nameok;
   bool   del;

   do
   {
      nameok = NO;
      nitems = 1;
      dfault = NONE;
      r1 = userchar_c( Setout, &nitems, &dfault, tofchar("OUTSET="),
                       tofchar("Name of output set?") );
      err = 0;
      if ( gds_exist_c(Setout, &err) )
      {
         del = toflog( NO );
         nitems = 1;
         dfault = REQUEST;
         r1 = userlog_c( &del, &nitems, &dfault, tofchar("DELETE="),
                         tofchar("Set already exists. Delete this set?   Y/[N]") );
         del = tobool( del );
         if (del)
         {
            err = 0;
            gds_delete_c( Setout, &err );
            nameok = YES;
         }
         else
         {
            cancel_c( tofchar("OUTSET=") );
            cancel_c( tofchar("DELETE=") );
            nameok = NO;
         }
      }
      else
         nameok = YES;
   }
   while (!nameok);
}



static void copyaxis( fchar Setin,
                      int   inpaxisnum,
                      fchar Setout,
                      int   outaxisnum )
/*------------------------------------------------------------*/
/* PURPOSE: Copy CDELT, DDELT, DTYPE from source axis         */
/*          (inpaxisnum) to destination (outaxisnum).         */
/*------------------------------------------------------------*/
{
   fchar     Ctype, Cunit;
   fchar     Dtype, Dunit;
   double    cdelt, ddelt;
   double    crpix;
   double    crval, drval;
   fint      naxis;
   fint      r1;


   (void) sprintf( message, "CRPIX%d", inpaxisnum );
   r1 = 0;
   gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crpix, &r1 );

   (void) sprintf( message, "NAXIS%d", inpaxisnum );
   r1 = 0;
   gdsd_rint_c( Setin, tofchar(message), &setlevel, &naxis, &r1 );

   fmake( Ctype, FITSLEN );
   (void) sprintf( message, "CTYPE%d", inpaxisnum );
   r1 = 0;
   gdsd_rchar_c( Setin, tofchar(message), &setlevel, Ctype, &r1 );

   r1 = 0;
   gds_extend_c( Setout, Ctype, &crpix, &naxis, &r1 );

   fmake( Cunit, FITSLEN );
   (void) sprintf( message, "CUNIT%d", inpaxisnum );
   r1 = 0;
   gdsd_rchar_c( Setin, tofchar(message), &setlevel, Cunit, &r1 );
   if (r1 == 0)
   {
      (void) sprintf( message, "CUNIT%d", outaxisnum );
      gdsd_wchar_c( Setout, tofchar(message), &setlevel, Cunit, &r1 );
   }

   (void) sprintf( message, "CRVAL%d", inpaxisnum );
   r1 = 0;
   gdsd_rdble_c( Setin, tofchar(message), &setlevel, &crval, &r1 );
   if (r1 == 0)
   {
      (void) sprintf( message, "CRVAL%d", outaxisnum );
      gdsd_wdble_c( Setout, tofchar(message), &setlevel, &crval, &r1 );
   }


   (void) sprintf( message, "CDELT%d", inpaxisnum );
   r1 = 0;
   gdsd_rdble_c( Setin, tofchar(message), &setlevel, &cdelt, &r1 );
   if (r1 == 0)
   {
      (void) sprintf( message, "CDELT%d", outaxisnum );
      gdsd_wdble_c( Setout, tofchar(message), &setlevel, &cdelt, &r1 );
   }

   /* Secondary info for second axis */

   fmake( Dtype, FITSLEN );
   (void) sprintf( message, "DTYPE%d", inpaxisnum );
   r1 = 0;
   gdsd_rchar_c( Setin, tofchar(message), &setlevel, Dtype, &r1 );
   if (r1 == 0)
   {
      (void) sprintf( message, "DTYPE%d", outaxisnum );
      gdsd_wchar_c( Setout, tofchar(message), &setlevel, Dtype, &r1 );
   }
   
   r1 = 0;
   fmake( Dunit, FITSLEN );
   (void) sprintf( message, "DUNIT%d", inpaxisnum );
   gdsd_rchar_c( Setin, tofchar(message), &setlevel, Dunit, &r1 );
   if (r1 == 0)
   {
      (void) sprintf( message, "DUNIT%d", outaxisnum );
      gdsd_wchar_c( Setout, tofchar(message), &setlevel, Dunit, &r1 );
   }

   (void) sprintf( message, "DRVAL%d", inpaxisnum );
   r1 = 0;
   gdsd_rdble_c( Setin, tofchar(message), &setlevel, &drval, &r1 );
   if (r1 == 0)
   {
      (void) sprintf( message, "DRVAL%d", outaxisnum );
      gdsd_wdble_c( Setout, tofchar(message), &setlevel, &drval, &r1 );
   }
   
   (void) sprintf( message, "DDELT%d", inpaxisnum );
   r1 = 0;
   gdsd_rdble_c( Setin, tofchar(message), &setlevel, &ddelt, &r1 );
   if (r1 == 0)
   {
      (void) sprintf( message, "DDELT%d", outaxisnum );
      gdsd_wdble_c( Setout, tofchar(message), &setlevel, &ddelt, &r1 );
   }
}



static double getaspectratio( fchar Setin,
                              fint  subset,
                              fint  *axnum )
/*------------------------------------------------------------*/
/* PURPOSE: Get the aspect ratio of the pixels in this subset.*/
/*------------------------------------------------------------*/
{
   int      i;
   double   cdelt[2];
   fint     err;

   for (i = 0; i < 2; i++)
   {
      (void) sprintf( message, "CDELT%d", axnum[i] );
      err = 0;
      gdsd_rdble_c( Setin,                   /* Get rotation angle */
                    tofchar(message),
                    &setlevel,
                    &cdelt[i],
                    &err );
      if (err < 0)
         return( -1.0 );
   }
   return( ABS( (float)(cdelt[0]/cdelt[1])) );
}



static bool mapPA( fchar    Setin,
                   double  *crota )
/*------------------------------------------------------------*/
/* PURPOSE: Get the rotation angle in the set header.         */
/*          CROTAi holds the rotation angle in degrees of the */
/*          actual axis from the stated coordinate type in    */
/*          CTYPEi. This descriptor makes only sense for      */
/*          spatial axes. In GIPSY it is assumed that the     */
/*          spatial latitude axis carries the rotation infor- */
/*          mation.                                           */
/*------------------------------------------------------------*/
{
   fchar    Ctype;
   fchar    Dummy;
   char     mbuff[STRLEN];
   bool     found = NO;
   fint     r, s, err;
   fint     setdim;
   fint     toplevel = 0;
   int      i;


   setdim = gdsc_ndims_c( Setin, &toplevel );
   fmake( Ctype, FITSLEN );
   fmake( Dummy, FITSLEN );
   *crota = 0.0;
   for (i = 0; i < setdim; i++)                    /* Check all axes */
   {
      (void) sprintf( mbuff, "CTYPE%d", i + 1 );
      err = 0;
      gdsd_rchar_c( Setin,                         /* Read CTYPE */
                    tofchar(mbuff),
                    &toplevel,
                    Ctype,
                    &err );
      if (err >= 0)
      {
         r = axtype_c( Ctype,                      /* Get axis type */
                       Dummy, Dummy,
                       &s, &s, &s );
         if (r == 2)                               /* Spatial axis latitude */
         {
            (void) sprintf( mbuff, "CROTA%d", i + 1 );
            err = 0;
            gdsd_rdble_c( Setin,                   /* Get rotation angle */
                          tofchar(mbuff),
                          &setlevel,
                          crota,                   /* Input was by reference */
                          &err );
            if (err < 0)                           /* No value found */
            {
               anyoutf( 8, "Cannot find (map) position angle in header, 0.0 assumed!");
               *crota = 0.0;
            }
            found = YES;
         }
      }
   }
   return( found );
}



static bool spatialsubset( fchar Setin,
                           fint  subset,
                           fint  *axnum )
/*------------------------------------------------------------*/
/* PURPOSE: Examine whether a subset is spatial.              */
/*          A subset is a spatial map if its dimension is 2,  */
/*          the first axis is a spatial longitude axis and the*/
/*          seconds is a spatial latitude axis.               */
/*------------------------------------------------------------*/
{
   fchar    Ctype;
   fchar    Dummy;
   char     mbuff[STRLEN];
   fint     r, s, err;
   fint     subdim;
   fint     toplevel = 0;
   int      i;


   subdim = gdsc_ndims_c( Setin, &subset );
   if (subdim != 2)
      return( NO );

   fmake( Ctype, FITSLEN );
   fmake( Dummy, FITSLEN );
   for (i = 0; i < subdim; i++)           /* Check all axes */
   {
      (void) sprintf( mbuff,
                     "CTYPE%d",
                      axnum[i] );
      err = 0;
      gdsd_rchar_c( Setin,                /* Read CTYPE */
                    tofchar(mbuff),
                    &toplevel,
                    Ctype,
                    &err );
      if (err >= 0)
      {
         r = axtype_c( Ctype,             /* Get axis type */
                       Dummy, Dummy,
                       &s, &s, &s );
         if (i == 0 && r != 1)            /* Not spatial axis longitude */
            return( NO );
         if (i == 1 && r != 2)            /* Not spatial axis latitude */
            return( NO );
      }
      else
         return( NO );                    /* Could not get CTYPE from header */
   }
   return( YES );
}



static void outsetinfo( fchar Setout,
                        fint  dev )
/*------------------------------------------------------------*/
/* PURPOSE: Display output set info.                          */
/*------------------------------------------------------------*/
{
   int      i;
   fint     numaxis;
   fint     r1, r2;
   fint     cwlo, cwhi;
   fint     flo[MAXAXES];
   fint     fhi[MAXAXES];
   fint     ax;
   fchar    Axname;
   fchar    Task;


   numaxis = gdsc_ndims_c( Setout, &setlevel );
   anyoutf( dev, " " );
   fmake( Task, 20 );
   myname_c( Task );
   (void) sprintf( message, "Program %.*s created OUTPUT set: %.*s",
                   nelc_c( Task ),
                   Task.a,
                   nelc_c( Setout ),
                   Setout.a );
   anyoutf( dev, message );

   anyoutf( dev, "----------------------------------------------" );
   anyoutf( dev, "Axis        Name          start  end    length" );
   anyoutf( dev, "----------------------------------------------" );

   r1 = 0;
   gdsc_range_c( Setout, &setlevel, &cwlo, &cwhi, &r1 );
   r1 = r2 = 0;
   for (i = 0; i < numaxis; i++)
   {
      ax = i + 1;
      flo[i] = gdsc_grid_c( Setout, &ax, &cwlo, &r1 );
      fhi[i] = gdsc_grid_c( Setout, &ax, &cwhi, &r2 );
   }

   fmake( Axname, FITSLEN );
   for (i = 0; i < numaxis; i++)
   {
      r1 = r2 = 0;
      ax = i + 1;
      gdsc_name_c( Axname, Setout, &ax, &r1 );
      (void) sprintf( message,
                     "%-4d %-20.20s %6d %6d %6d",
                      ax,
                      Axname.a,
                      flo[i], fhi[i],
                      fhi[i] - flo[i] + 1 );
      anyoutf( dev, message );
   }
   anyoutf( dev, " " );
}



static int createoutset( fchar Setin,
                         fchar Setout,
                         fint  numang,
                         fint  numrad,
                         fint  *axnum,
                         fint  profaxis )
/*------------------------------------------------------------*/
/* PURPOSE: Create the output set.                            */
/*                                                            */
/* The first axis in the output set is an (new)               */
/* angle axis. The second is the profile axis of the          */
/* input set. The third axis is a (new) radius axis.          */
/* If the dimension of the input set is greater than          */
/* 3, there are some axes left to copy. These are the         */
/* remaining input set axes except the subset axes.           */
/* Example:                                                   */
/*   Input is: set 4DIM with axes RA DEC FREQ POL and         */
/*   subset specification INSET=4DIM ra 3:6 pol (gives        */
/*   subsets in DEC,FREQ) and a selected profile direc-       */
/*   tion of RA, results in an output set with axes:          */
/*   PARAM-THETA  RA  PARAM-RADIUS  POL.                      */
/*------------------------------------------------------------*/
{
   fint      err;
   fint      r1;
   fint      setlevel = 0;
   fint      naxis;
   int       numaxis;
   int       i;
   double    crpix, cdelt, crval;
   fchar     Bunit;


   gds_create_c( Setout, &err );
   if (err < 0)
   {
      anyoutf( 1, "Error  while creating set" );
      return( NO );
   }

   numaxis = gdsc_ndims_c( Setin, &setlevel );

   /* First axis new set */
   r1 = 0;
   crpix = 0;
   naxis = numang;
   gds_extend_c( Setout, tofchar("PARAM-THETA"), &crpix, &naxis, &r1 );
   r1 = 0;
   gdsd_wchar_c( Setout, tofchar("CUNIT1"), &setlevel,
                 tofchar("ANGLENUM"), &r1 );
   cdelt = 1.0;
   r1 = 0;
   gdsd_wdble_c( Setout, tofchar("CDELT1"), &setlevel, &cdelt, &r1 );
   crval = 0.0;
   r1 = 0;
   gdsd_wdble_c( Setout, tofchar("CRVAL1"), &setlevel, &crval, &r1 );


   /* Second axis new set */
   copyaxis( Setin, profaxis, Setout, 2 );


   /* Third axis new set */
   crpix = 0;
   naxis = numrad;
   gds_extend_c( Setout, tofchar("PARAM-RADIUS"), &crpix, &naxis, &r1 );
   r1 = 0;
   gdsd_wchar_c( Setout, tofchar("CUNIT3"), &setlevel,
                 tofchar("RADNUM"), &r1 );
   cdelt = 1.0;
   r1 = 0;
   gdsd_wdble_c( Setout, tofchar("CDELT3"), &setlevel, &cdelt, &r1 );
   crval = 0.0;
   r1 = 0;
   gdsd_wdble_c( Setout, tofchar("CRVAL3"), &setlevel, &crval, &r1 );


   /* Get units of the data */
   r1 = 0;
   fmake( Bunit, FITSLEN );
   gdsd_rchar_c( Setin, tofchar("BUNIT"), &setlevel, Bunit, &r1 );
   if (r1 == 0)
      gdsd_wchar_c( Setout, tofchar("BUNIT"), &setlevel, Bunit, &r1 );


   /* Perhaps there are remaining axes. Check and add */
   if (numaxis > 3)
   {
      int  newindx = 4;
      for (i = 2; i < numaxis; i++)
      {
         if (axnum[i] != profaxis)
            copyaxis( Setin, axnum[i], Setout, newindx++ );
      }
   }

   return( YES );
}



static void getunits( fchar   Setin,
                      fint    *axnum,
                      char    *units,
                      double  *confact )
/*------------------------------------------------------------*/
/* PURPOSE: Get header compatible units from the user.        */
/* IN:      Setin, axnum                                      */
/* OUT:     units, confact                                    */
/*                                                            */
/* Given header units and user units, calculate conversion    */
/* factors so that units*conversion=pixels.                   */
/* If the units of the subset axes are not the same, the input*/
/* is in pixels. A default unit can be overruled with         */
/* UNITS=PIXELS or UNITS=GRIDS. Two conversion factors are    */
/* calculated using the CDELTS from the header.               */
/*------------------------------------------------------------*/
{
   fint      nitems, dfault;
   fint      r;
   int       agreed;
   int       i;
   double    cdelt[2];
   double    factor;
   fchar     Cunit[2];
   fchar     Units;
   int       len1, len2;
   int       convert = YES;


   /* Calculate the conversion factors for the units given by the user */

   fmake( Units, FITSLEN );
   for (i = 0; i < 2; i++)
   {
      finit( Cunit[i], FITSLEN );
      (void) sprintf( message, "CUNIT%d", axnum[i] );
      r = 0;
      gdsd_rchar_c( Setin, tofchar(message), &setlevel, Cunit[i], &r );
      if (r != 0)
      {
         anyoutf( 8, "Could not obtain header value for %s", message );
         convert = NO;
         break;
      }
      else                           /* Units are found, now look for cdelt's */
      {
         (void) sprintf( message, "CDELT%d", axnum[i] );
         r = 0;
         gdsd_rdble_c( Setin, tofchar(message),
                       &setlevel, &cdelt[i], &r );
         if (r != 0)
         {
            anyoutf( 8, "Could not obtain value for %s in header", message );
            convert = NO;
            break;
         }
      }
   }

   /* Conversion only possible if both header units are the same */
   if (convert)
   {
      len1 = nelc_c( Cunit[0] );
      len2 = nelc_c( Cunit[1] );
      convert = (len1 == len2);
      if (convert)
         convert = !strncmp( Cunit[0].a, Cunit[1].a, len1 );
      if (!convert)
         anyoutf( 1, "Unequal units for axes ==> switching to grids!" );
   }


   if (convert)
   {
      do
      {
         nitems = 1;
         dfault = REQUEST;
         (void) sprintf( message,
                        "Give units for axis length:       [%.*s]",
                         nelc_c(Cunit[0]), Cunit[0].a );
         r = usercharu_c( Units, &nitems, &dfault,
                          tofchar("UNITS="),
                          tofchar( message ) );
         if (r == 0)
            (void) sprintf( Units.a, "%.*s", nelc_c(Cunit[0]), Cunit[0].a );
         Units.a[nelc_c(Units)] = '\0';
         if ( strstr(Units.a, "GRID") || strstr(Units.a, "PIX") )
         {
            convert = NO;
            agreed = YES;
         }
         else
         {
            agreed = YES;
            r = factor_c( Cunit[0], Units, &factor );
            if (r != 0)
            {
               anyoutf( 8, "Cannot convert from %.*s to %.*s",
                        nelc_c(Units), Units.a,
                        nelc_c(Cunit[0]), Cunit[0].a );
               agreed = NO;
            }
         }
         if (!agreed)
            cancel_c( tofchar("UNITS=") );
      }
      while (!agreed);
   }

   /* Do not use else here */
   if (!convert)
      Units.l = sprintf( Units.a, "GRIDS" );

   for (i = 0; i < 2; i++)
   {
      if (convert)
         confact[i] = 1.0 / ( factor*ABS(cdelt[i]) );
      else
         confact[i] = 1.0;
   }

   /* Copy units to C string */
   (void) sprintf( units, "%.*s", nelc_c(Units), Units.a );

   /*-----------------------------------------*/
   /* Tell user how the pixels convert if the */
   /* units are not grids. The conversion is: */
   /* units * conversion = pixels.            */
   /*-----------------------------------------*/
   if (convert)
   {
      anyoutf( 8, " " );
      anyoutf( 8, "Units conversion for set [%.*s]:", nelc_c(Setin), Setin.a );
      anyoutf( 8, "1 x 1 grids == %g x %g %s",
                1.0/confact[0], 1.0/confact[1], units );
      anyoutf( 8, "1 x 1 %s == %g x %g grids",
                units, confact[0], confact[1] );
   }

   free( Cunit[1].a );
   free( Cunit[0].a );
}



static int getellipse( fchar  Setin,
                       fint   *axnum,
                       double **radii,
                       double **incl,
                       double **angles,
                       double *samplerange,
                       char   *units )
/*------------------------------------------------------------*/
/* PURPOSE: Get ellipse parameters MAJOR=, INCL=, PA= and     */
/*          RANGE= from user.                                 */
/* INPUT:   Setin, axnum, units                               */
/* OUTPUT:  radii, incl, angles, samplerange                  */
/*                                                            */
/* Note that radii, incl, angles are pointers to rows.        */
/* Allocate space for angles and radii arrays and get values  */
/* in a loop. The angles are the PA's of the ellipses. The    */
/* radii are given in the UNITS= units and represent the minor*/
/* and major axes of an ellipse. There are as many angles as  */
/* radii pairs. Finally, get the sampling. This is a starting */
/* angle (wrt. PA) and an end. The third parameter is the step*/
/* size. All angles are in degree.                            */
/*------------------------------------------------------------*/
{
   fint      nitems, dfault;
   fint      r, r1;
   int       i;
   double    rads[MAXRADS];
   int       numrads;


   /* Get the radii. */

   numrads = 0;
   do
   {
      if (numrads)
         dfault = REQUEST;
      else
         dfault = NONE;
      nitems = MAXRADS;
      (void) sprintf( message,
                     "(Rad.%d) Give length of MAJOR axes in %s: ",
                      numrads + 1, units );

      r = userdble_c( rads, &nitems, &dfault, tofchar("MAJOR="),
                      tofchar( message ) );
      cancel_c( tofchar("MAJOR=") );

      if (r != 0)  /* Nothing needs to be added, end this loop */
      {
         if (!numrads)
         {
            *radii  = (double *) calloc( r, sizeof(double) );
            *incl   = (double *) calloc( r, sizeof(double) );
            *angles = (double *) calloc( r, sizeof(double) );
         }
         else
         {
            *radii  = (double *) realloc( (double *) *radii,
                                          (numrads+r)*sizeof(double) );

            *incl   = (double *) realloc( (double *) *incl,
                                          (numrads+r)*sizeof(double) );

            *angles = (double *) realloc( (double *) *angles,
                                          (numrads+r)*sizeof(double) );
         }
         if (*radii == NULL)
            errorC( 4, "Cannot allocate memory anymore for radius array!" );
         if (*incl == NULL)
            errorC( 4, "Cannot allocate memory anymore for inclinations array!" );
         if (*angles == NULL)
            errorC( 4, "Cannot allocate memory anymore for position angle array!" );

         /* Copy the radii to the (reallocated) array.     */
         /* Remember that radii is a pointer to a pointer. */
         for (i = 0; i < r; i++)
            radii[0][numrads+i] = ABS(rads[i]);   /* Do not allow negative values */

         /* cancel_c( tofchar("MAJOR=") ); */

         /* For each radius there must be an inclination */
         dfault = NONE;
         nitems = r;
         if (r == 1)
            (void) sprintf( message, "Give inclination (deg) of this ellipse:" );
         else
            (void) sprintf( message, "Give inclinations (deg) of these ellipses:" );

         r1 = userdble_c( &incl[0][numrads], &nitems, &dfault, tofchar("INCL="),
                          tofchar( message ) );
         cancel_c( tofchar("INCL=") );

         /* At least one is given here. Copy missing inclinations from last. */
         for (i = r1; i < r; i++)
            incl[0][numrads+i] = incl[0][numrads+(i-1)];

         if (r == 1)
            (void) sprintf( message, "Give P.A. (deg) of this ellipse:" );
         else
            (void) sprintf( message, "Give P.A. (deg) of these ellipses:" );
         r1 =userdble_c( &angles[0][numrads], &nitems, &dfault, tofchar("PA="),
                         tofchar( message ) );
         cancel_c( tofchar("PA=") );

         /* Copy missing angles: */
         for (i = r1; i < r; i++)
            angles[0][numrads+i] = angles[0][numrads+(i-1)];

         numrads += r;
      }                                       /* End if */
   }                                          /* end do */
   while (r);


   dfault = REQUEST;
   nitems = 3;
   samplerange[0] = 0.0;
   samplerange[1] = 360.0;
   samplerange[2] = 2.0;
   (void) sprintf( message,
                  "Give sample angles 'start,end,step' (deg): [%g %g %g]",
                   samplerange[0], samplerange[1], samplerange[2] );
   r = userdble_c( samplerange, &nitems, &dfault, tofchar("RANGE="),
                   tofchar( message ) );
   return( numrads );
}



static void getorigin( fchar  Setin,
                       fint   *axnum,
                       fint   *subsets,
                       fint   *blo,
                       fint   *bhi,
                       double *origin )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate a default for the origin of the ellipses*/
/*          and prompt user with centre keyword.              */
/*------------------------------------------------------------*/
{
   fint     dfault;
   fint     maxpos;
   fint     r[2];
   fint     phys2grid = 0;
   fint     r1;
   int      i;
   double   crval[2];
   double   phys[2];
   double   grid[MAXAXES];



   for (i = 0; i < 2; i++)
   {
      r[i] = 0;
      (void) sprintf( message, "CRVAL%d", axnum[i] );
      gdsd_rdble_c( Setin, tofchar(message) , &setlevel, &crval[i], &r[i] );
   }
   if (r[0] != 0 || r[1] != 0)
   {
      origin[0] = (double)  ((bhi[0] + blo[0]) / 2);
      origin[1] = (double)  ((bhi[1] + blo[1]) / 2);
   }
   else
   {
      phys[0] = crval[0];
      phys[1] = crval[1];
      r1 = cotrans_c( Setin, &subsets[0], phys, grid, &phys2grid );
      origin[0] = grid[axnum[0]-1];
      origin[1] = grid[axnum[1]-1];
   }
   maxpos = 1;
   dfault = REQUEST;
   (void) sprintf( message, "Give central position of ellipses:  [%.2f %.2f]",
                   origin[0], origin[1]);

   r1 = gdspos_c( origin, &maxpos, &dfault,
                  tofchar("CENTRE="),
                  tofchar( message ),
                  Setin,
                  &subsets[0] );
}



static int equalset( fchar Set1,
                     fchar Set2 )
/*------------------------------------------------------------*/
/* PURPOSE: Check whether two set names are equal.            */
/* Include path names in comparison. But before including a   */
/* path, copy the set names so that input is not changed!     */
/*------------------------------------------------------------*/
{
   int    i;
   fint   l1, l2;
   fchar  S1, S2;


   fmake( S1, STRLEN );         /* Set1/2 also have length 'STRLEN' */
   fmake( S2, STRLEN );

   for (i = 0; i < nelc_c(Set1) && i < S1.l; i++)  /* Copy all characters in src */
      S1.a[i] = Set1.a[i];
   while (i < S1.l)                                /* Pad with blanks */
      S1.a[i++] = ' ';

   for (i = 0; i < nelc_c(Set2) && i < S2.l; i++)
      S2.a[i] = Set2.a[i];
   while (i < S2.l)
      S2.a[i++] = ' ';

   l1 = getpath_c( S1 );
   l2 = getpath_c( S2 );
   if (l1 != 0 || l2 != 0)
   {
      anyoutf( 8, "Cannot get full pathname for set name" );
      if (l1 == -1 || l2 == -1)
         anyoutf( 8, "Cannot get entry from password file.");
      if (l1 == -2 || l2 == -2)
         anyoutf( 8, "Full pathname too long for PATH" );
      return( 0 );
   }

   l1 = nelc_c( S1 );
   l2 = nelc_c( S2 );

   if (l1 != l2)
      return( 0 );
   return( strncmp( S1.a, S2.a, l1 ) == 0 );
}



static int getGIDSset( fchar Setin,
                       fint  *GIDSblo,
                       fint  *GIDSbhi,
                       float *GIDSflo,
                       float *GIDSfhi )
/*------------------------------------------------------------*/
/* INPUT:   Setin                                             */
/* OUTPUT:  GIDSblo, GIDSbhi, GIDSflo, GIDSfhi                */
/* PURPOSE: Check whether an overlay in GIDS can be made. I.e.*/
/*          1) Gids must be started, 2) An image must be load-*/
/*          ed and 3) The name of the displayed set must match*/
/*          'Setin'. If so, return 1 and the sizes of the     */
/*          displayed box and the GIDS frame, Otherwise return*/
/*          0 as result of the function.                      */
/*------------------------------------------------------------*/
{
   fint         display_stat;                   /* display operation status */
   fint         setdim;
   fint         iax;
   fint         r1, r2;
   fint         grid;
   fchar        Axisname;
   fint         GIDSdisplay_id = -1;            /* id of display */
   fchar        GIDSset;
   fint         GIDSsubset;


   /* If not available, do NOT start GIDS */
   GIDSdisplay_id = gdi_open2_c( tofchar(" ") );

   if (GIDSdisplay_id < 0)                      /* error opening display */
   {
      anyoutf( 1, "GIDS not started!" );
      return( 0 );
   }
   fmake( GIDSset, 128 );
   display_stat = gdi_iinfo_c( &GIDSdisplay_id, /* id of display */
                               GIDSset,         /* name of set */
                               &GIDSsubset,     /* subset level */
                               GIDSblo,         /* lower left frame boundary */
                               GIDSbhi );       /* upper right frame boundary */

   if (display_stat < 0)                        /* error obtaining info */
   {
      anyoutf( 1,  "No image loaded! Use application VIEW!");
      return( 0 );
   }

   if (gdsc_ndims_c( GIDSset, &GIDSsubset ) != 2)
   {
      anyoutf( 1,  "Wrong dimension of set in GIDS!");
      return( 0 );
   }

   r1 = gdi_frame_c( &GIDSdisplay_id ,          /* id of display */
                     GIDSflo,                   /* lower left frame boundary */
                     GIDSfhi );                 /* .. in (floating) grids */

   if (r1 != 0)
   {
      (void) sprintf( message,
                     "Cannot obtain info about frame currently on display! (err=%d)", r1 );
      anyoutf( 1,  message );
      return( 0 );
   }
   display_stat = gdi_close_c( &GIDSdisplay_id );   /* close display */

   /*--------------------------------------------------*/
   /* Are input set name and displayed set name equal? */
   /*--------------------------------------------------*/
   if ( !equalset(Setin, GIDSset) )
   {
      anyoutf( 1,  "Input set name and displayed set name are NOT equal!" );
      return( 0 );
   }

   /* Some information about displayed set: */

   anyoutf( 16, "========== GIDS =========" );
   (void) sprintf( message, "%.*s", nelc_c(GIDSset), GIDSset.a );
   setdim = gdsc_ndims_c( GIDSset, &setlevel );
   fmake( Axisname, FITSLEN );

   /* Append non subset axis names to set name */
   for (iax = 1; iax <= setdim; iax++)
   {
      r1 = 0;
      grid = gdsc_grid_c( GIDSset, &iax, &GIDSsubset, &r1 );
      if (r1 >= 0)
      {
         r2 = 0;
         gdsc_name_c( Axisname, GIDSset, &iax, &r2 );
         (void) sprintf( message, "%.*s %s %d",
                         strlen(message), message,
                         strtok( Axisname.a , " -" ), grid );
      }
   }
   anyoutf( 16,  message );
   (void) sprintf( message,
                  "Displayed set has box: [%d %d %d %d]",
                   GIDSblo[0], GIDSblo[1], GIDSbhi[0], GIDSbhi[1] );
   anyoutf( 16,  message );
   (void) sprintf( message,
                  "Gids frame: [%g %g %g %g]",
                   GIDSflo[0], GIDSflo[1], GIDSfhi[0], GIDSfhi[1] );
   anyoutf( 16,  message );
   return( 1 );
}



static void scaletrans( double xin,
                        double yin,
                        double *conv,
                        double cx,
                        double cy,
                        float  *xout,
                        float  *yout )
/*--------------------------------------------------------------------*/
/* PURPOSE: Convert input position to pixels (pixels-->pixels:        */
/*          conv = 1). Correct this position for the central position */
/*          and convert to floats (plot positions are in floats).     */
/*--------------------------------------------------------------------*/
{
   *xout = (float) ( conv[0]*xin + cx );
   *yout = (float) ( conv[1]*yin + cy );
}



static void rotate( double x,
                    double y,
                    double angle,
                    double *xrot,
                    double *yrot )
/*--------------------------------------------------------------------*/
/* PURPOSE: Rotate over 'angle' RADIANS anti clockwise wrt. pos.      */
/*          x axis.                                                   */
/*--------------------------------------------------------------------*/
{
   double    CosP, SinP;


   CosP = cos( angle );                  /* Calculate angles */
   SinP = sin( angle );

   *xrot = x * CosP - y * SinP;          /* Do the rotation */
   *yrot = x * SinP + y * CosP;
}



#ifdef THETACIRCLE
static void ellfie( double major,
                    double minor,
                    double alpha,
                    double *xp,
                    double *yp )
/*------------------------------------------------------------------------*/
/* Calculate for given angle the coordinates of a standard ellipse        */
/* b**2.x**2 + a**2.y**2 = a**2.b**2 in Polar coordinates and transform   */
/* to Rectangular coordinates (a = major axis).                           */
/*------------------------------------------------------------------------*/
{
   double   p1, p2, p3;
   double   r;
   double   denom;
   double   sinA = sin(RAD(alpha));
   double   cosA = cos(RAD(alpha));

   p1 = minor * cosA;
   p2 = major * sinA;
   p3 = minor * major;
   denom = (p1*p1 + p2*p2);
   if (denom == 0.0)
      r = 0;
   else
      r = sqrt( minor*major * minor*major / denom );
   *xp = r * cosA;
   *yp = r * sinA;
}
#endif



static void drawframe( fint   *blo,
                       fint   *bhi,
                       double originX,
                       double originY,
                       double crota,
                       bool   rotated,
                       double *confact )
/*------------------------------------------------------------------*/
/* PURPOSE: Draw a simple labeled box around viewport and draw      */
/*          coordinate axes. If necessary, plot the direction of    */
/*          the north with an arrow.                                */
/*------------------------------------------------------------------*/
{
   fint    nxsub, nysub;
   float   xtick, ytick;
   float   x, y;
   float   just;
   float   charsize;
   float   oldsize;
   fint    oldcolor;
   double  xx, yy;
   double  xr, yr;


   xtick = ytick = 0.0;
   setcol( FOREGROUND );
   nxsub = nysub = 0;
   pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
            tofchar("BCNSTV"), &ytick, &nysub );
   setcol( RED );
   x = (float) originX;
   y = (float) blo[1];
   pgmove_c( &x, &y );
   y = (float) bhi[1];
   pgdraw_c( &x, &y );
   x = (float) blo[0];
   y = (float) originY;
   pgmove_c( &x, &y );
   x = (float) bhi[0];
   pgdraw_c( &x, &y );

   /* Direction of the north */

   if (rotated)
   {
      float   angle;
      double  arrowin[4];
      float   arrowout[4];

      pgqci_c( &oldcolor );
      setcol( BLUE );
      x = (float) originX;
      y = (float) originY;
      pgmove_c( &x, &y );

      xx = ( (double)(bhi[0] - blo[0]) ) * 0.40 / confact[0];
      yy = ( (double)(bhi[1] - blo[1]) ) * 0.40 / confact[1];   /* Part of Y axis In user units */
      xx = MYMIN( xx, yy );
      yy = 0.0;

      /* Imaginary arrow aligned with positive x-axis */
      arrowin[0] = 0.9  * xx;
      arrowin[1] = 0.02 * xx;
      rotate( arrowin[0], arrowin[1], RAD(90.0+crota), &xr, &yr );
      scaletrans( xr, yr, confact, originX, originY, &arrowout[0], &arrowout[1] );
      arrowin[2] =  0.9  * xx;
      arrowin[3] = -0.02 * xx;
      rotate( arrowin[2], arrowin[3], RAD(90.0+crota), &xr, &yr );
      scaletrans( xr, yr, confact, originX, originY, &arrowout[2], &arrowout[3] );

      rotate( xx, yy, RAD(90.0+crota), &xr, &yr );
      scaletrans( xr, yr, confact, originX, originY, &x, &y );
      pgdraw_c( &x, &y );
      pgdraw_c( &arrowout[0], &arrowout[1] );
      pgmove_c( &x, &y );
      pgdraw_c( &arrowout[2], &arrowout[3] );

      just = 0.5;
      pgqch_c( &oldsize );
      charsize = 2.0;
      pgsch_c( &charsize );
      angle = 0.0;
      xx *= 1.1;
      rotate( xx, yy, RAD(90.0+crota), &xr, &yr );
      scaletrans( xr, yr, confact, originX, originY, &x, &y );
      pgptxt_c( &x, &y, &angle, &just, tofchar("N") );
      pgsch_c( &oldsize );
      setcol( oldcolor );
   }
}



static int getnumsamples( double *samplerange )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate the number of samples, given a start    */
/*          and end value for theta and a step (all in        */
/*          degrees).                                         */
/*------------------------------------------------------------*/
{
   int      i = 0;
   double   theta;


   for (theta = samplerange[0]; theta < samplerange[1];
        theta += samplerange[2], i++);
   return( i );
}



static void getsamplepos( double centreX,
                          double centreY,
                          double major,
                          double incl,
                          double pa,
                          double crota,
                          double *samrange,
                          double *confact,
                          float  *Xarray,
                          float  *Yarray )
/*------------------------------------------------------------*/
/* INPUT:   centreX, centreY, major, incl, pa, crota, samrange*/
/*          confact.                                          */
/* OUTPUT:  Xarray, Yarray                                    */
/* PURPOSE: Given the ellipse parameters, calculate the sample*/
/*          positions.                                        */
/*------------------------------------------------------------*/
{
   double   CosP, SinP;
   double   cosTh, sinTh;
   double   theta;
   double   x, y;
   double   xr, yr;
   double   minor = major * cos( RAD(incl) );
   fint     i;


   pa = RAD(pa + 90.0 + crota);
   CosP = cos( pa );
   SinP = sin( pa );

   i = 0;
   for (theta = samrange[0]; theta < samrange[1]; theta += samrange[2])
   {
      cosTh = cos( RAD(theta) );
      sinTh = sin( RAD(theta) );
      x = major * cosTh;
      y = minor * sinTh;
      rotate( x, y, pa, &xr, &yr );
      scaletrans( xr, yr,
                  confact,
                  centreX, centreY,
                  &Xarray[i], &Yarray[i] );
      i++;
   }
}



static void drawellipse( double centreX,
                         double centreY,
                         double major,
                         double incl,
                         double pa,
                         double crota,
                         double *confact,
                         fint   color_pa,
                         fint   color_ell )
/*------------------------------------------------------------*/
/* PURPOSE: Plot an ellipse with user given parameters.       */
/*                                                            */
/* Draw an ellipse with origin at 'centreX', 'centreY'. Major */
/* axis is 'major' and the minor axis is 'major' * COS(incl). */
/* The inclination is in degrees. The position angle is 'pa'  */
/* also in degrees. The position angle is increased with 90   */
/* because 'pa' is defined wrt. the North. 'crota' is the     */
/* map rotation. The ellipse is sampled at 1 deg in the       */
/* inclined plane.                                            */
/*------------------------------------------------------------*/
{
   double   CosP, SinP;
   double   cosTh, sinTh;
   double   theta;
   double   x, y;
   double   xr, yr;
   double   minor = major * cos( RAD(incl) );
   double   start, end, step;
   float    *Xarray = NULL;
   float    *Yarray = NULL;
   fint     i;
   fint     oldcolor;
   int      len;


   pa    = RAD(pa + 90.0 + crota);
   CosP  = cos( pa );
   SinP  = sin( pa );
   start = 1.0;
   end   = 360.0;
   step  = 1.0;
   len   = 0;
   for (theta = start; theta <= end; theta += step)
      len++;
   Xarray = (float *) calloc( len, sizeof(float) );
   Yarray = (float *) calloc( len, sizeof(float) );
   if (Xarray == NULL || Yarray == NULL)
      errorC( 4, "Cannot allocate memory for ellipse arrays!" );


   pgqci_c( &oldcolor );
   setcol( color_pa );
   {
      float xf = (float) centreX;
      float yf = (float) centreY;
      pgmove_c( &xf, &yf );
      theta = 0.0;
      cosTh = cos( RAD(theta) );
      sinTh = sin( RAD(theta) );
      x = major * cosTh;
      y = minor * sinTh;
      rotate( x, y, pa, &xr, &yr );
      scaletrans( xr, yr, confact, centreX, centreY, &xf, &yf );
      pgdraw_c( &xf, &yf );
   }

   i = 0;
   for (theta = start; theta <= end; theta += step)
   {
      cosTh = cos( RAD(theta) );
      sinTh = sin( RAD(theta) );
      x = major * cosTh;
      y = minor * sinTh;
      rotate( x, y, pa, &xr, &yr );
      scaletrans( xr, yr, confact, centreX, centreY, &Xarray[i], &Yarray[i] );
      i++;
   }
   setcol( color_ell );
   pgline_c( &i, Xarray, Yarray );
   setcol( oldcolor );                              /* Restore colour */

   free( Xarray );
   free( Yarray );
}



static void ellipsetable( int    dev,
                          fchar  Setname,
                          double crota,
                          bool   rotated,
                          int    numrad,
                          double *origin,
                          double *confact,
                          char   *units,
                          double *radii,
                          double *angles,
                          double *incl,
                          double *samplerange,
                          int    samples,
                          fchar  Idstr,
                          fchar  Datestr )
/*------------------------------------------------------------*/
/* PURPOSE: Show the ellipse parameters in a table.           */
/*------------------------------------------------------------*/
{
   int i;

   anyoutf( dev, " " );
   anyoutf( dev, "=============== Ellipse Parameters ===============" );
   anyoutf( dev, "Original set          : %.*s", nelc_c(Setname), Setname.a );
   anyoutf( dev, "User                  : %.*s", nelc_c(Idstr), Idstr.a );
   anyoutf( dev, "Date of creation      : %.*s", nelc_c(Datestr), Datestr.a );
   anyoutf( dev, "Origin of all ellipses: %.3f, %.3f (grids)",
                  origin[0], origin[1] );
   if (rotated)
      anyoutf( dev, "Map rotation          : %.2f (degrees)", crota );
   anyoutf( dev, "1 %s in X == %g grids", units, confact[0] );
   anyoutf( dev, "1 %s in Y == %g grids", units, confact[1] );
   anyoutf( dev, " " );

   i = sprintf( message,
               "Ellipse #| Rad.(%-8.8s) | Pos.angle (deg) | Inclin.(deg)",
                units );
   anyoutf( dev, message );
   memset( message, '=', i );
   message[i] = '\0';
   anyoutf( dev, message );

   for (i = 0; i < numrad; i++)
      anyoutf( dev, "%-9d| %14.3f | %15.3f | %11.3f",
               i+1, radii[i], angles[i], incl[i] );

   anyoutf( dev, message );
   anyoutf( dev, "Positions are sampled from %.2f (deg) wrt. the position angle",
            samplerange[0] );
   anyoutf( dev, "to (not included) %.2f (deg) wrt. the position angle.",
            samplerange[1] );
   anyoutf( dev, "There are %d sample positions with separations %.2f (deg)",
            samples, samplerange[2] );
   anyoutf( dev, "in the plane of the ellipse." );
   anyoutf( dev, " " );
}




static float getipval( float x,
                       float y,
                       fint *blo,
                       fint *bhi )
/*------------------------------------------------------------*/
/* PURPOSE: Get 2d-interpolated image value using spline      */
/*          interpolation                                     */
/*------------------------------------------------------------*/
{
   int     xi = (int) x;
   int     yi = (int) y; 
   int     row, col;
   fint    one = 1;
   float   result;
   int     j;
   
   /* GLOBAL VARIABLES: Xin, Yin, Xinp, Yinp, IPxsize, IPysize */
 
   /* Position outside box? */
   if (x < blo[0] || x > bhi[0] || y < blo[1] || y > bhi[1])
      return( blank );

   /*--------------------------------------------------*/
   /* Get all data in a row. Do a spline and store     */
   /* result. Repeat this for all 'IPysize' rows.      */
   /* Store the 'IPxsize' results and apply a spline   */
   /* interpolation again.                             */
   /*--------------------------------------------------*/ 
   j = 0;
   for (row = yi - IPysize/2; row <= yi + IPysize/2; row++)
   {
      int   i = 0;
      for (col = xi - IPxsize/2; col <= xi + IPxsize/2; col++)
      {
         Xin[i] = (float) col;
         Yin[i] = getimval( col, row, blo, bhi );    
         i++;
      }
      spline1_c( Xin, Yin, &IPxsize, &x, &Yinp[j], &one );
      Xinp[j] = (float) row;
      j++;
   }
   /* Now we filled a column with data and do a spline again */
   spline1_c( Xinp, Yinp, &IPysize, &y, &result, &one );
   return( result );
}





static FILE *getfileptr( void )
/*------------------------------------------------------------*/
/* PURPOSE: Ask user name of an ASCII file.                   */
/*                                                            */
/* Check whether the file already exists. If so, Ask for      */
/* permission to overwrite. Return a file pointer. This file  */
/* pointer is NULL if no file is wanted.                      */
/*------------------------------------------------------------*/
{
   FILE   *fp = NULL;
   fint   dfault, nitems;
   fint   r1;
   int    agreed;
   bool   overwrite;
   fchar  Filename;


   fmake( Filename, 512 );
   do
   {
      dfault    = HIDDEN;
      overwrite = toflog(YES);
      nitems    = 1;
      r1 = userchar_c( Filename,
                       &dfault,
                       &nitems,
                       KEY_FILENAME,
                       tofchar("Give name of ASCII file:     [No file]") );

      if (!r1)
      {
         fp = NULL;
         agreed = YES;
      }
      else
      {
         Filename.a[nelc_c(Filename)] = '\0';
         fp = fopen( Filename.a, "r" );
         if (fp != NULL)                       /* The file exists */
         {
            nitems = 1;
            dfault = REQUEST;
            r1 = userlog_c( &overwrite,
                            &nitems,
                            &dfault,
                            KEY_OVERWRITE,
                            tofchar("File exists, ok to overwrite?    [Y]/N") );

            fclose( fp );
            cancel_c( KEY_OVERWRITE );
         }
         if (!overwrite)
         {
            cancel_c( KEY_FILENAME );
            agreed = NO;
         }
         else
         {
            fp = fopen( Filename.a, "w" );
            agreed = (fp != NULL);
            if (!agreed)
               reject_c( KEY_FILENAME, tofchar("Cannot open, try another!") );
         }
      }
   }
   while (!agreed);
   return( fp );
}



static void writetodisk( FILE   *fp,
                         fchar  Setin,
                         fint   *blo,
                         fint   *bhi,
                         int    numrad,
                         int    samples,
                         float  **Xarray,
                         float  **Yarray,
                         fint   subset,
                         bool   interpos,
                         float  delta,
                         char   *units  )
/*------------------------------------------------------------*/
/* PURPOSE: Write contents of Xarray and Yarray (grids) to    */
/*          file. If possible, write also the physical        */
/*          coordinates.                                      */
/*------------------------------------------------------------*/
{
   double   phys[MAXAXES];
   double   grid[2];
   fint     grid2phys = 1;
   int      r, s;
   bool     trans;
   double   xlo = (double) blo[0];   
   double   xhi = (double) bhi[0];
   double   ylo = (double) blo[1];
   double   yhi = (double) bhi[1];


   grid[0] = grid[1] = 0.0;
   trans = ( cotrans_c( Setin, &subset, grid, phys, &grid2phys ) == 0 );
   if (interpos)
      (void) fprintf( fp, "Positions are interpolated with separation %g %s\n",
                      delta, units );
   for (r = 0; r < numrad; r++)
   {
      (void) fprintf( fp, "Positions for output subset %d:\n", r+1 );
      for (s = 0; s < samples; s++)
      {
         bool     inside;
         grid[0] = (double) Xarray[r][s];
         grid[1] = (double) Yarray[r][s];
         inside = (grid[0] >= xlo && grid[0] <= xhi && 
                   grid[1] >= ylo && grid[1] <= yhi);
         if (inside)
         {
            (void) fprintf( fp, "%+10.2f %+10.2f   ", grid[0], grid[1] );
            if (trans)
            {
               (void) cotrans_c( Setin, &subset, grid, phys, &grid2phys );
               (void) fprintf( fp, "U %12f U %12f\n",
                               phys[axnum[0]-1],
                               phys[axnum[1]-1] );
            }
            else
              (void) fprintf( fp, "\n" );
         }
         else
            (void) fprintf( fp, "%10.10s %10.10s\n", "blank", "blank" );
      }
   }
}



static int  createGDStable( fchar   Setout,
                            fchar   Setin,
                            fchar   Tabname,
                            fint    setlevel,
                            fint    numrad,
                            double  *radii,
                            double  *incl,
                            double  *angles,
                            double  *samplerange,
                            double  *origin,
                            double  *confact,
                            bool    rotated,
                            double  crota,
                            char    *units,
                            fchar   Idstr,
                            fchar   Datestr )
/*------------------------------------------------------------*/
/* PURPOSE: Create a GDS table in the header of 'Setout'.     */
/*------------------------------------------------------------*/
{
   fint    r1;
   fint    *radnums = NULL;
   fint    one = 1;
   fint    n;
   int     i;


   radnums = (fint *) calloc( numrad, sizeof(fint) );
   if (radnums == NULL)
   {
      anyoutf( 1, "Cannot allocate memory for table array." );
      return( 0 );
   }
   for (i = 0; i < numrad; i++)
      radnums[i] = i + 1;

   (void) sprintf( message, "Angle range:[%.2f,%.2f> deg. Step:%.2f",
                   samplerange[0], samplerange[1], samplerange[2] );

   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_RADNUM,
                  tofchar("INT"),
                  tofchar(message),
                  tofchar("#"),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   r1 = 0;
   gdsa_wcint_c( Setout,
                 &setlevel,
                 Tabname,
                 COL_RADNUM,
                 radnums,
                 &one,
                 &numrad,
                 &r1 );

   free( radnums );
   if (r1 < 0)
      return( 0 );

   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_RADIUS,
                  tofchar("DBLE"),
                  tofchar("Radius"),
                  tofchar(units),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   r1 = 0;
   gdsa_wcdble_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_RADIUS,
                  radii,
                  &one,
                  &numrad,
                  &r1 );
   if (r1 < 0)
      return( 0 );

   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_PA,
                  tofchar("DBLE"),
                  tofchar("Position angle maj.axis"),
                  tofchar("degrees"),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   r1 = 0;
   gdsa_wcdble_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_PA,
                  angles,
                  &one,
                  &numrad,
                  &r1 );
   if (r1 < 0)
      return( 0 );

   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_INCL,
                  tofchar("DBLE"),
                  tofchar("Inclination"),
                  tofchar("degree"),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   r1 = 0;
   gdsa_wcdble_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_INCL,
                  incl,
                  &one,
                  &numrad,
                  &r1 );
   if (r1 < 0)
      return( 0 );

   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_SRANGE,
                  tofchar("DBLE"),
                  tofchar("Sample angles start, end, step (deg.)"),
                  tofchar("degree"),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   r1 = 0;
   n  = 3;
   gdsa_wcdble_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_SRANGE,
                  samplerange,
                  &one,
                  &n,
                  &r1 );
   if (r1 < 0)
      return( 0 );

   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_ORIGIN,
                  tofchar("DBLE"),
                  tofchar("Origin in grids."),
                  tofchar("grids"),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   r1 = 0;
   n  = 2;
   gdsa_wcdble_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_ORIGIN,
                  origin,
                  &one,
                  &n,
                  &r1 );
   if (r1 < 0)
      return( 0 );

   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_CONFACT,
                  tofchar("DBLE"),
                  tofchar("Conversion factors grid <-> units."),
                  tofchar("degree"),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   r1 = 0;
   n  = 2;
   gdsa_wcdble_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_CONFACT,
                  confact,
                  &one,
                  &n,
                  &r1 );
   if (r1 < 0)
      return( 0 );

   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_CROTA,
                  tofchar("DBLE"),
                  tofchar("Map rotation in degrees."),
                  tofchar("degree"),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   if (!rotated)                           /* Map not rotated */
      setdblank_c( &crota );                 /* Store crota as a blank */

   r1 = 0;
   n  = 1;
   gdsa_wcdble_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_CROTA,
                  &crota,
                  &one,
                  &n,
                  &r1 );
   if (r1 < 0)
      return( 0 );

   /* Store original set name */
   (void) sprintf( message, "CHAR%d", nelc_c(Setin) );
   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_SETNAME,
                  tofchar(message),
                  tofchar("Name of original set."),
                  tofchar(" "),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   r1 = 0;
   n  = 1;
   gdsa_wcchar_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_SETNAME,
                  Setin,
                  &one,
                  &n,
                  &r1 );
   if (r1 < 0)
      return( 0 );

   /* Store user name */
   (void) sprintf( message, "CHAR%d", nelc_c(Idstr) );
   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_IDSTR,
                  tofchar(message),
                  tofchar("User at time of creation."),
                  tofchar(" "),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   r1 = 0;
   n  = 1;
   gdsa_wcchar_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_IDSTR,
                  Idstr,
                  &one,
                  &n,
                  &r1 );
   if (r1 < 0)
      return( 0 );

   /* Store creation date */
   (void) sprintf( message, "CHAR%d", nelc_c(Datestr) );
   gdsa_crecol_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_DATESTR,
                  tofchar(message),
                  tofchar("Creation time."),
                  tofchar(" "),
                  &r1 );
   if (r1 < 0)
      return( 0 );

   r1 = 0;
   n  = 1;
   gdsa_wcchar_c( Setout,
                  &setlevel,
                  Tabname,
                  COL_DATESTR,
                  Datestr,
                  &one,
                  &n,
                  &r1 );
   if (r1 < 0)
      return( 0 );

   return( 1 );
}



static bool tabexist(fchar Setin, fchar Tname)
/*-------------------------------------------------------------*/
/* PURPOSE: Does table exist on set level?                     */
/*-------------------------------------------------------------*/
{
   fint    nitems = 100;               /* An arbitrary high number of columns */
   fint    r1;
   fint    nfound;
   fchar   Cnames;

   Cnames.a = malloc(1);
   Cnames.l = 0;
   r1 = 0;
   gdsa_tabinq_c( Setin, &setlevel, Tname, Cnames, &nitems, &nfound, &r1 );
   free( Cnames.a );
   return (r1 >= 0 && nfound > 0);
}



static void displayGDStable( fchar Tabset,
                             fchar Tabname )
/*------------------------------------------------------------*/
/* PURPOSE: Display a table in a set created by ELLPROF in a  */
/*          previous  run.                                    */
/*------------------------------------------------------------*/
{
   if ( !tabexist(Tabset, Tabname) )
   {
      anyoutf( 1, "Cannot find table [%.*s] in set [%.*s]",
               nelc_c(Tabname), Tabname.a, nelc_c(Tabset), Tabset.a );
      return;
   }
   else
   {
      fchar    Cunits, Ctype, Ccom;
      fchar    Setname;
      fchar    Idstr, Datestr;
      fint     err;
      fint     numrad;
      fint     one = 1;
      fint     n;
      fint     *radnr = NULL;
      double   *radii = NULL;
      double   *pa    = NULL;
      double   *incl  = NULL;
      double   samplerange[3];
      double   origin[2];
      double   confact[2];
      double   crota;
      bool     rotated;
      char     units[ITEMLEN+1];
      int      samples;

      fmake( Cunits,  ITEMLEN );
      fmake( Ccom,    1024 );
      fmake( Ctype,   ITEMLEN );
      fmake( Setname, VARLEN );
      fmake( Idstr,   VARLEN );
      fmake( Datestr, VARLEN );
      gdsa_colinq_c( Tabset, &setlevel, Tabname, COL_RADIUS, Ctype,
                     Ccom, Cunits, &numrad, &err );
      (void) sprintf( units, "%.*s", nelc_c(Cunits), Cunits.a ); /* Copy str */

      if (err != 0)
      {
         anyoutf( 1, "Error (nr %d) reading column info!", err );
         return;
      }
      (void) sprintf( message,
                     "Reading table data from %.*s, col. length = %d",
                      nelc_c(Tabname), Tabname.a, numrad );
      status_c( tofchar(message) );

      radnr = (fint *)   calloc( numrad,  sizeof(fint)   );
      radii = (double *) calloc( numrad,  sizeof(double) );
      pa    = (double *) calloc( numrad,  sizeof(double) );
      incl  = (double *) calloc( numrad,  sizeof(double) );
      if (!radnr || !radii || !pa || !incl )
      {
         anyoutf( 1, "Could not allocate memory for 'table' arrays!" );
         return;
      }
      gdsa_rcint_c( Tabset, &setlevel, Tabname, COL_RADNUM,
                    radnr, &one, &numrad, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from number column!", err );
         return;
      }
      gdsa_rcdble_c( Tabset, &setlevel, Tabname, COL_RADIUS,
                     radii, &one, &numrad, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from radius column!", err );
         return;
      }
      gdsa_rcdble_c( Tabset, &setlevel, Tabname, COL_PA,
                     pa, &one, &numrad, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from position angle column!", err );
         return;
      }
      gdsa_rcdble_c( Tabset, &setlevel, Tabname, COL_INCL,
                     incl, &one, &numrad, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from inclinations column!", err );
         return;
      }
      n = 3;
      gdsa_rcdble_c( Tabset, &setlevel, Tabname, COL_SRANGE,
                     samplerange, &one, &n, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from sample range column!", err );
         return;
      }
      n = 2;
      gdsa_rcdble_c( Tabset, &setlevel, Tabname, COL_ORIGIN,
                     origin, &one, &n, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from origin column!", err );
         return;
      }
      n = 2;
      gdsa_rcdble_c( Tabset, &setlevel, Tabname, COL_CONFACT,
                     confact, &one, &n, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from con. factor column!", err );
         return;
      }
      n = 1;
      gdsa_rcdble_c( Tabset, &setlevel, Tabname, COL_CROTA,
                     &crota, &one, &n, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from crota column!", err );
         return;
      }
      rotated = !tobool( dblank_c(&crota) );

      n = 1;
      gdsa_rcchar_c( Tabset, &setlevel, Tabname, COL_SETNAME,
                     Setname, &one, &n, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from crota column!", err );
         return;
      }

       n = 1;
      gdsa_rcchar_c( Tabset, &setlevel, Tabname, COL_IDSTR,
                     Idstr, &one, &n, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from Username column!", err );
         Idstr.a[0] = '\0';
         Idstr.l = 1;
      }

      n = 1;
      gdsa_rcchar_c( Tabset, &setlevel, Tabname, COL_DATESTR,
                     Datestr, &one, &n, &err );
      if (err < 0)
      {
         anyoutf( 1, "Error (nr %d) reading data from Date column!", err );
         Datestr.a[0] = '\0';
         Datestr.l = 1;
      }

      /* All ok? Display the contents */
      samples = getnumsamples( samplerange );
      ellipsetable( 3,
                    Setname,
                    crota,
                    rotated,
                    numrad,
                    origin,
                    confact,
                    units,
                    radii,
                    pa,
                    incl,
                    samplerange,
                    samples,
                    Idstr,
                    Datestr );
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



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   double    *angles = NULL;               /* PA angles */
   double    *radii  = NULL;               /* Ellipse radii */
   double    *incl = NULL;                 /* Inclinations */
   double    origin[2];                    /* Ellipse origin */
   double    confact[2];                   /* Conversion to pixels */
   double    samplerange[3];               /* Range of sample angles and step */
   double    crota = 0.0;                  /* Rotation angle of map */
   fint      numrad = 0;                   /* Number of radii, angles */
   fint      nitems, dfault;               /* Used in userxxx_c functions */
   fint      dev;                          /* Anyout destination. */
   fint      profaxis;                     /* Axis number (1..n) of profile */
   float     *buff = NULL;                 /* Output data. One point for each angle */
   float     **Xarray = NULL;              /* The sample x,y positions */
   float     **Yarray = NULL;
   float     GIDSflo[2], GIDSfhi[2];       /* GIDS frame */
   fint      GIDSblo[2], GIDSbhi[2];       /* GIDS box */
   fint      option;                       /* Data from ellipse, manual, cursor */
   fint      samples = 0;                  /* Number of sample positions */
   fint      imagesize;                    /* Size of input subsets */
   fint      r1, r2;                       /* Function results */
   fint      subdim;                       /* Dimensionality of the subsets for class 1 applications */
   fint      setdim;                       /* Dimension of set */
   fint      nsubs;                        /* Number of input subsets */
   fint      subnr;                        /* Counter for subset loop. */
   fint      markcol;                      /* Colour of graphics markers */
   int       i;                            /* Counter */
   bool      opengids;                     /* Is GIDS available? */
   bool      rotated;                      /* Is there a map rotation? */
   char      units[FITSLEN+1];             /* Distance units in map */
   fchar     Idstr, Datestr;
   fchar     Setin, Setout;




   init_c();                               /* Contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );


   /*--------------------------------------------------*/
   /* Is there a GDS table to display?                 */
   /*--------------------------------------------------*/
   {
      fchar   Tabset;

      fmake( Tabset, STRLEN );
      dfault = HIDDEN;
      nitems = 1;
      r1 = userchar_c( Tabset, &nitems, &dfault, tofchar("TABSET="),
                       tofchar("Name of set with table to display:") );
      if (r1)
      {
         displayGDStable( Tabset, TABNAME );
         finis_c();
      }
   }


   /*--------------------------------------------------*/
   /* Prepare for an input set. This is a class 1      */
   /* program. For sets with dim > 3 a profile direc-  */
   /* tion is asked. Class 1 is for applications which */
   /* repeat the operation for each subset.            */
   /*--------------------------------------------------*/
   {
      fint     class = 1;

      fmake( Setin, STRLEN );
      fmake( Key,   KEYLEN );
      fmake( Mes,   STRLEN );
      dfault  = NONE;
      dev     = 3;
      Key     = KEY_INSET;
      Mes     = MES_INSET;
      subdim  = 2;                    /* Subset dimension MUST be 2! */

      nsubs   = gdsinp_c( Setin,      /* Name of input set. */
                          subin,      /* Array containing subsets coordinate words. */
                          &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                          &dfault,    /* Default code as is USERxxx. */
                          Key,        /* Keyword prompt. */
                          Mes,        /* Keyword message for the user. */
                          &dev,       /* Device number (as in ANYOUT). */
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
                          &subdim );  /* Dimensionality of subset(s). */
      setdim  = gdsc_ndims_c( Setin, &setlevel );
   }


   if (setdim <= 2)
   {
      anyoutf( 1, "Dimension of the input set must be at least 3");
      finis_c();
   }

   profaxis = axnum[2];               /* Profile axis for 3-dim sets */
   if (setdim > 3)
   /*-------------------------------*/
   /* Special case where we have to */
   /* ask which axis is the profile */
   /* axis.                         */
   /*-------------------------------*/
   {
      fchar Ctype;
      int   agreed;

      fmake( Ctype, FITSLEN );
      do
      {
         nitems = 1;
         dfault = REQUEST;
         /* Get a default for the profile axis */
         (void) sprintf( message, "CTYPE%d", axnum[2] );
         r1 = 0;
         gdsd_rchar_c( Setin, tofchar(message), &setlevel, Ctype, &r1 );
         (void) sprintf( message,
                        "Give name of profile axis:   [%.*s]",
                         nelc_c( Ctype ), Ctype.a );

         /* The axis name must be in capitals */
         (void) usercharu_c( Ctype, &nitems, &dfault,
                             tofchar("PROFAXIS="),
                             tofchar( message ) );
         r1 = 0;
         profaxis = gdsc_axnum_c( Setin, Ctype, &r1 );
         agreed = (r1 == 0);
         if (!agreed)
            reject_c( tofchar("PROFAXIS="), tofchar("Unknown axis!") );
         else
         {
            agreed = (profaxis != axnum[0] && profaxis != axnum[1]);
            if (!agreed)
               reject_c( tofchar("PROFAXIS="), tofchar("Is a subset axis!") );
         }
      }
      while( !agreed );
   }


   /*------------------------------------------------------*/
   /* Prepare a box for INSET. Variable 'boxopt' sets      */
   /* the input in 'gdsbox'.                               */
   /*  1 box may exceed subset size                        */
   /*  2 default is in BLO                                 */
   /*  4 default is in BHI                                 */
   /*  8 box restricted to size defined in BHI             */
   /*  These codes work additive.                          */
   /*  When boxopt is 0 or 1, the default is the is the    */
   /*  entire subset.                                      */
   /*------------------------------------------------------*/
   {
      fint     boxopt = 0;

      dev     = 1;
      dfault  = REQUEST;
      Key     = KEY_BOX;
      Mes     = tofchar(" ");
      gdsbox_c( blo, bhi, Setin, subin, &dfault,
                Key, Mes, &dev, &boxopt );
   }

   image = fmatrix( blo[0], blo[1], bhi[0], bhi[1] );
   if (!image)
      errorC( 4, "Cannot allocate memory for image" );
   imagesize = (bhi[0] - blo[0] + 1) * (bhi[1] - blo[1] + 1);


   /* Prepare for the data interpolation matrix */
   nitems   = 2;
   dfault   = REQUEST;
   {
      fint xysize[2];
      fint xlen = (bhi[0] - blo[0] + 1);
      fint ylen = (bhi[1] - blo[1] + 1);      
      fint r;
      
      r = userint_c( xysize, &nitems, &dfault, tofchar("IPOLSIZE="),
                     tofchar("Enter (odd) size in x & y of interpolation matrix: [7 7]" ) ); 
      if (r == 0)
      {
         IPxsize = MYMIN( xlen-1, 7 );
         IPysize = MYMIN( ylen-1, 7 );
      }
      if (r == 1)
         IPxsize = IPysize = ABS(xysize[0]);

      if (r == 2)
      {
         IPxsize = ABS(xysize[0]);
         IPysize = ABS(xysize[1]);
      }
      IPxsize = MYMIN( xlen-1, IPxsize );
      IPysize = MYMIN( ylen-1, IPysize );      
      if (!(IPxsize%2)) IPxsize++;         /* Must be odd */
      if (!(IPysize%2)) IPysize++;
   }
   anyoutf( 16, "Size data interpolation matrix: %d x %d", IPxsize, IPysize );
   
   Xin = (float *) calloc( IPxsize,  sizeof(float) );  
   Yin = (float *) calloc( IPxsize,  sizeof(float) );  
   Xinp = (float *) calloc( IPysize,  sizeof(float) );  
   Yinp = (float *) calloc( IPysize,  sizeof(float) );  

   if (!Xin || !Yin || !Xinp || !Yinp)
      errorC( 4, "Cannot allocate memory for data interpolation arrays" );
   
   /* Does user want to mark the positions in the subset in GIDS? */

   opengids = toflog( YES );
   nitems   = 1;
   dfault   = REQUEST;
   (void) userlog_c( &opengids, &nitems, &dfault, tofchar("OVERLAY="),
                     tofchar("Overlay positions in GIDS?   [Y]/N") );
   opengids = tobool( opengids );

   if (!opengids)
      pgopen = NO;
   else
   {
      if ( getGIDSset(Setin, GIDSblo, GIDSbhi, GIDSflo, GIDSfhi) )
         pgopen = initplot( GIDSblo, GIDSbhi, GIDSflo, GIDSfhi );

      if (!pgopen)
      /*---------------------------------------------------*/
      /* Start VIEW. The keyword INSET= is not cancelled,  */
      /* so VIEW starts with displaying all subsets until  */
      /* NEXT=NO is entered.                               */
      /*---------------------------------------------------*/
      {
         fint status;
         deputy_c( TSK_VIEW, &status );
         if (status != 1)
            printstatus( TSK_VIEW, status );
         else
            if ( getGIDSset(Setin, GIDSblo, GIDSbhi, GIDSflo, GIDSfhi) )
               pgopen = initplot( GIDSblo, GIDSbhi, GIDSflo, GIDSfhi );
       }
   }

   rotated = NO;
   if ( spatialsubset(Setin, subin[0], axnum) )
      rotated = mapPA( Setin, &crota );

   option = 1;
   nitems = 1;
   dfault = REQUEST;
   if (pgopen)
      strcpy( message, "1) Ellipse(s),  2) Manual,  3) Cursor:    [1]/2/3" );
   else
      strcpy( message, "1) Ellipse(s),  2) Manual:    [1]/2" );
   (void) userint_c( &option, &nitems, &dfault,
                     tofchar("SAMPLES="),
                     tofchar(message) );
   if (option < 1)
      option = 1;
   if (option > 3)
      option = 3;

   if (!pgopen && option == 3)
      option = 2;

   markcol = askcolor();

   if (option == 1)
   /*------------------------------------------------------*/
   /* The ellipse option. Sample positions on an ellipse.  */
   /*------------------------------------------------------*/
   {
      getorigin( Setin,                      /* Get origin of ellipses */
                 axnum,
                 subin,
                 blo, bhi,
                 origin );


      /* 'confact' are the XY conversions units*confact=grids. */
      getunits( Setin, axnum, units, confact );

      /* Get ellipse parameters */
      numrad = getellipse( Setin,
                           axnum,
                           &radii,           /* Minor, major axes */
                           &incl,            /* Inclinations */
                           &angles,          /* PA's of the ellipses */
                           samplerange,      /* Sample angle start,end,step */
                           units );          /* User units of major axis */

      if (pgopen)
      {

         if ( (float) getaspectratio( Setin, subin[0], axnum ) != 1.0 )
            anyoutf( 8, "Warning: Pixel aspect ratio is not 1. Plotted angles are NOT factual!");

         drawframe( blo, bhi,
                    origin[0], origin[1],
                    crota,
                    rotated,
                    confact );

         for (i = 0; i < numrad; i++)
            drawellipse( origin[0], origin[1],
                         radii[i],
                         incl[i],
                         angles[i],
                         crota,
                         confact,
                         BLUE,
                         YELLOW );
      }
      samples = getnumsamples( samplerange );
      Xarray = (float **) malloc( numrad * sizeof(float *) );
      Yarray = (float **) malloc( numrad * sizeof(float *) );
      if (Xarray == NULL || Yarray == NULL)
         errorC( 4, "Cannot allocate memory for sample positions" );

      for (i = 0; i < numrad; i++)
      /*-----------------------------------------------------------*/
      /* Fill Xarray[i], Yarray[i] (pointers to rows) with sample  */
      /* positions on ellipse with given parameters.               */
      /*-----------------------------------------------------------*/
      {
         Xarray[i] = (float *) malloc( samples * sizeof(float) );
         Yarray[i] = (float *) malloc( samples * sizeof(float) );
         if (Xarray[i] == NULL || Yarray[i] == NULL)
            errorC( 4, "Cannot allocate memory for sample positions" );
         getsamplepos( origin[0], origin[1],
                       radii[i],
                       incl[i],
                       angles[i],
                       crota,
                       samplerange,
                       confact,
                       Xarray[i], Yarray[i] );

         if (pgopen)
            setmarkers( samples, Xarray[i], Yarray[i], markcol, 2 );
      }

      fmake( Idstr, VARLEN );
      fmake( Datestr, VARLEN );
      getusernam_c( Idstr );
      getdate_c( Datestr );
      ellipsetable( 8,                   /* Display ellipse parameters */
                    Setin,
                    crota,
                    rotated,
                    numrad,
                    origin,
                    confact,
                    units,
                    radii,
                    angles,
                    incl,
                    samplerange,
                    samples,
                    Idstr,
                    Datestr );

   } /* End of ellipse option */


   if (option == 2 || option == 3)
   /*--------------------------------------*/
   /* The manual and cursor options.       */
   /*--------------------------------------*/
   {
      bool        more;             /* Another subset with profiles? */
      bool        interpos;         /* Generate positions by interpolation? */
      float       delta;            /* Separation between interpolated pos. */
      fint        r;
      fchar       Ch;
      char        ch;
      int         maxsamples = 0;
      FILE        *fp;              /* File pointer */


      rotated  = NO;                /* Rotation of map not in use */
      interpos = toflog( NO );
      nitems   = 1;
      dfault   = REQUEST;
      r = userlog_c( &interpos, &nitems, &dfault, tofchar("INTERPOS="),
                     tofchar("Interpolate between input positions?  Y/[N]") );
      interpos = tobool( interpos );
      if (interpos)
      {
         getunits( Setin, axnum, units, confact );
         dfault = NONE;
         (void) sprintf( message, "Give step size for interpolation in %s:",
                         units );
         r = userreal_c( &delta, &nitems, &dfault, tofchar("DELTA="),
                         tofchar(message) );
         delta = ABS( delta );
         if (delta == 0.0)
            interpos = NO;
      }
      r = 0;
      numrad = 0;
      if (option == 3)
      {
         fmake( Ch, 1 );      /* Used in 'pgcurs' */
         drawframe( blo, bhi, 0.0, 0.0, crota, rotated, confact );
      }
      do   /* Get sample positions for 1 subset */
      {
         float   whatsleft;
         float   prevx = blank;
         float   prevy = blank;
         float   xy[2];


         xy[0] = blo[0] + (bhi[0]-blo[0])/3.0; /* Here is the graphics cursor */
         xy[1] = blo[1] + (bhi[1]-blo[1])/3.0;

         if (numrad == 0)
         {
            Xarray = (float **) malloc( 1 * sizeof(float *) );
            Yarray = (float **) malloc( 1 * sizeof(float *) );
            Xarray[numrad] = (float *) malloc( 1 * sizeof(float) );
            Yarray[numrad] = (float *) malloc( 1 * sizeof(float) );
         }
         else
         {
            Xarray = (float **) realloc( (float **)Xarray, (numrad+1)*sizeof(float *) );
            Yarray = (float **) realloc( (float **)Yarray, (numrad+1)*sizeof(float *) );
            Xarray[numrad] = (float *) malloc( maxsamples * sizeof(float) );
            Yarray[numrad] = (float *) malloc( maxsamples * sizeof(float) );
         }
         samples = 0;
         whatsleft = 0.0;
         do
         {
            int     inside = YES;
            float   xlo = (float) blo[0];   
            float   xhi = (float) bhi[0];
            float   ylo = (float) blo[1];
            float   yhi = (float) bhi[1];
            
            dfault = REQUEST;
            if (numrad != 0 && samples >= maxsamples)
               r = 0;
            else
            {
               if (option == 3)
               {
                  status_c( tofchar( "LEFT button or key to mark, RIGHT bt. or Q to Quit" ) );
                  r = pgcurs_c( &xy[0], &xy[1], Ch );
                  if (!r)
                     anyoutf( 1, "Device has no cursor, or other error" );
                  ch = toupper(Ch.a[0]);
                  if (ch == 'Q' || ch == '3')
                     r = 0;
               }
               else    /* User input */
               {
                  double xxyy[2];
                  nitems = 1;
                  r = gdspos_c( xxyy, &nitems, &dfault,
                                KEY_XY,
                                tofchar("Give Position x, y:        [end loop]"),
                                Setin,
                                &subin[0] );
                  if (r)
                  {
                     xy[0] = (float) xxyy[0];
                     xy[1] = (float) xxyy[1];
                  }
               }
               cancel_c( KEY_XY );
               inside = (xy[0] >= xlo && xy[0] <= xhi && xy[1] >= ylo && xy[1] <= yhi);
            }
            if (r && inside)
            {
               /* Selected position is plotted */

               if (interpos)
               {
                  float  lambda, lambda0;
                  float  dist;

                  setmarkers( 1, &xy[0], &xy[1], BLUE, 2 );
                  /*-----------------------------------------------------*/
                  /* If there is a previously stored position, calculate */
                  /* the distance between this position and the new one  */
                  /* and convert it to user given units with the formula */
                  /* units = pixels / conversion. 'lambda' is a variable */
                  /* that sets a position on a line. If lambda=0 we are  */
                  /* at the start position and if lambda=1, we are at    */
                  /* the last entered position. 'lambda' is partitioned  */
                  /* using the fraction of 'delta' in units and 'dist' in*/
                  /* units. In most cases, the loop does not exactly end */
                  /* with lambda=1. The difference is stored and subtrac */
                  /* ted from lambda=0 in the next loop. This way one    */
                  /* generates a series of equidistant (in units) posit- */
                  /* ions.                                               */
                  /*-----------------------------------------------------*/
                  if (prevx != blank)
                  {
                     dist = sqrt( (xy[0]-prevx)*(xy[0]-prevx)
                                  /confact[0]/confact[0] +
                                  (xy[1]-prevy)*(xy[1]-prevy)
                                  /confact[1]/confact[1] );

                     lambda0 = whatsleft / dist;
                     for (lambda = -lambda0; lambda < 1.0; lambda += delta/dist)
                     {
                        float x, y;
                        if (lambda >= 0.0)
                        {
                           x = prevx + lambda * (xy[0]-prevx);
                           y = prevy + lambda * (xy[1]-prevy);
                           if (numrad == 0 && samples > 0)
                           {
                              Xarray[numrad] = (float *) realloc( (float *) Xarray[numrad],
                                               (samples+1)*sizeof(float) );
                              Yarray[numrad] = (float *) realloc( (float *) Yarray[numrad],
                                               (samples+1)*sizeof(float) );
                           }
                           if (numrad == 0 || (numrad != 0 && samples < maxsamples))
                           {
                              Xarray[numrad][samples] = x;
                              Yarray[numrad][samples] = y;
                              if (pgopen)
                                 setmarkers( 1, &x, &y, markcol, 2 );
                              samples++;
                           }
                        }
                        if (lambda >= 0.0)
                           whatsleft = (1.0 - lambda) * dist;
                        else
                           whatsleft += dist;
                     }
                  }
               }
               else
               {
                  setmarkers( 1, &xy[0], &xy[1], markcol, 2 );
                  /* Store the input position */
                  if (numrad == 0 && samples > 0)
                  {
                     Xarray[numrad] = (float *) realloc( (float *) Xarray[numrad],
                                                (samples+1)*sizeof(float) );
                     Yarray[numrad] = (float *) realloc( (float *) Yarray[numrad],
                                                (samples+1)*sizeof(float) );
                  }
                  Xarray[numrad][samples] = xy[0];
                  Yarray[numrad][samples] = xy[1];
                  samples++;
               }
               prevx = xy[0];
               prevy = xy[1];
            }
         }
         while (r);

         maxsamples = MYMAX( maxsamples, samples );
         if (numrad != 0)
         {
            for (i = samples; i < maxsamples; i++)
            {
               /* Fill up (if necessary) the array with positions outside box */
               Xarray[numrad][i] = blo[0] - 1;
               Yarray[numrad][i] = blo[1] - 1;
            }
         }

         numrad++;
         nitems = 1;
         dfault = REQUEST;
         more   = toflog( NO );
         r = userlog_c( &more, &nitems, &dfault,
                        tofchar("MORE="),
                        tofchar("Continue with another subset with samples?  Y/[N]") );
         cancel_c( tofchar("MORE=") );
         more = tobool( more );
      }
      while (more);
      samples = maxsamples;

      fp = getfileptr();
      if (fp)
         writetodisk( fp,              /* Pointer to ASCII file on disk */
                      Setin,           /* Name of input set */
                      blo,             /* Current box */
                      bhi,
                      numrad,          /* Number of smple sessions */
                      samples,         /* Number of samples per session */
                      Xarray,          /* 2-d matrix with positions */
                      Yarray,
                      subin[0],        /* Subset level for coord. transf. */
                      interpos,        /* Position interpolation flag */
                      delta,           /* Step size for interpolation */
                      units );         /* Distance units given by user */

   }


   /* Create an output set */

   fmake( Setout, STRLEN );
   getoutsetname( Setout );

   createoutset( Setin, Setout, samples, numrad, axnum, profaxis );

   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/

   buff = (float *) calloc( samples, sizeof(float) );
   if (buff == NULL)
   {
      anyoutf( 1, "Cannot allocate space for output data buffer!" );

      for (i = 0; i < numrad; i++)
      {
         free( Xarray[i] );
         free( Yarray[i] );
      }
      free( Xarray );
      free( Yarray );
      freefmatrix( image, blo[0], blo[1] );
      free( angles );
      free( radii );
      free( incl );
      pgend_c();
      finis_c();
   }



   for(subnr = 0; subnr < nsubs; subnr++)
   /*--------------------------------------------------------------*/
   /* This is the loop where data is written to the output set.    */
   /* For each input subset all profile values at sample positions */
   /* are assembled. This action is repeated for the number of     */
   /* ellipse radii (or sample sessions for manual and cursor      */
   /* options). For each loop two n-dim grid vectors are filled.   */
   /* These vectors are used to calculate output set coordinate    */
   /* words. An input subset is read in one buffer.                */
   /*--------------------------------------------------------------*/
   {
      int    m;
      fint   hi, lo;
      fint   lovec[MAXAXES];
      fint   hivec[MAXAXES];
      fint   cwlo, cwhi;           /* Coordinate words. */
      int    radius;
      fint   tid = 0;              /* Transfer id for read function. */
      fint   pixelsread;           /* Number of pixels read by read routine. */


      cwlo = gdsc_fill_c( Setin, &subin[subnr], blo );
      cwhi = gdsc_fill_c( Setin, &subin[subnr], bhi );
      gdsi_read_c( Setin,
                   &cwlo, &cwhi,
                   &image[blo[1]][blo[0]],
                   &imagesize,
                   &pixelsread,
                   &tid );


      lovec[0] = 1;                               /* Angle axis */
      hivec[0] = samples;

                                                  /* Profile axis */
      lovec[1] = hivec[1] = gdsc_grid_c( Setin,
                                         &profaxis,
                                         &cwlo,
                                         &r1 );

      for (m = 3; m < (int) setdim; m++)          /* Other axes (if any) */
      {
         lo = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         hi = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
         hivec[m] = lovec[m] = lo;
      }

      for (radius = 1; radius <= numrad; radius++)
      {
         fint   tidO;
         fint   cwloO, cwhiO;
         fint   pixelsdone;
         hivec[2] = lovec[2] = radius;            /* Radius axis */
         for (i = 0; i < samples; i++)
            buff[i] = getipval( Xarray[radius-1][i],
                                Yarray[radius-1][i],
                                blo, bhi );

         cwloO  = gdsc_fill_c( Setout, &setlevel, lovec );
         cwhiO  = gdsc_fill_c( Setout, &setlevel, hivec );
         tidO = 0;
         gdsi_write_c( Setout,
                       &cwloO, &cwhiO,
                       buff,
                       &samples,
                       &pixelsdone,
                       &tidO );
      }
   }
   outsetinfo( Setout, 8 ) ;

   if (option == 1)
   /*----------------------------------------------------*/
   /* Create a GDS table in the header of the output set */
   /*----------------------------------------------------*/
   {
      int res;
      res = createGDStable( Setout,
                            Setin,
                            TABNAME,
                            setlevel,
                            numrad,
                            radii,
                            incl,
                            angles,
                            samplerange,
                            origin,
                            confact,
                            rotated,
                            crota,
                            units,
                            Idstr,
                            Datestr );
      if (!res)
         anyoutf( 1, "Could not write a correct GDS table!" );
      else
         anyoutf( 8,  "Table [ELLIPSES] in set [%.*s] at top level has columns:",
                       nelc_c(Setout), Setout.a );
         anyoutf( 8 , "RADNUM RADIUS PA INCL SRANGE ORIGIN CONFACT CROTA SETNAME IDSTR DATESTR" );
         anyoutf( 16, "RADNUM  Radius number is equal to PARAM-RADIUS grid" );
         anyoutf( 16, "RADIUS  Length of major axes ");
         anyoutf( 16, "PA      Position angle major axes in degrees" );
         anyoutf( 16, "INCL    Inclination in degrees" );
         anyoutf( 16, "SRANGE  Sample angles start, end, step in degrees" );
         anyoutf( 16, "ORIGIN  Selected origin in grids" );
         anyoutf( 16, "CONFACT Conversion factors grids <-> units" );
         anyoutf( 16, "CROTA   Map rotation angle of original set in degrees" );
         anyoutf( 16, "SETNAME Name of the original data set" );
         anyoutf( 16, "IDSTR   Id of user at creation time" );
         anyoutf( 16, "DATESTR Creation time and date" );
         anyoutf( 16, " " );
         anyoutf( 8,  "Columns can be displayed with: ELLPROF TABSET=%.*s",
                       nelc_c(Setout), Setout.a );
         anyoutf( 8,  " " );
   }

   gds_close_c( Setout, &r1 );
   /*-------------------------------------------------------------*/
   /* The new output set has no min/max descriptors in its header */
   /* Use 'MNMX' to update header. Input is a newly created INSET=*/
   /* keyword consisting of the name of the output set and the    */
   /* name(s) of its non-subset axes.                             */
   /*-------------------------------------------------------------*/
   {
      fint    status;
      fint    err;
      fchar   Axisname;

      cancel_c( KEY_BOX );
      fmake( Axisname, FITSLEN );
      (void) sprintf( message, "INSET=%.*s PARAM-RADIUS",
                      nelc_c(Setout), Setout.a );
      for (i = 3; i < setdim; i++)
      {
         gdsc_name_c( Axisname, Setin, &axnum[i], &err );
         if (!err)
         {
            strcat( message, " " );
            strcat( message, strip(Axisname, '-') );
            anyoutf( 1, message );
         }
      }
      wkey_c( tofchar(message) );
      deputy_c( TSK_MNMX, &status );
      if (status != 1)
         printstatus( TSK_MNMX, status );
      cancel_c( tofchar("INSET=") );
   }

   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   free( buff );
   for (i = 0; i < numrad; i++)
   {
      free( Yarray[i] );
      free( Xarray[i] );
   }
   free( Yarray );
   free( Xarray );
   free( Yinp );
   free( Xinp );
   free( Yin );
   free( Xin );         
   freefmatrix( image, blo[0], blo[1] );
   free( angles );
   free( radii );
   free( incl );
   pgend_c();
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}

