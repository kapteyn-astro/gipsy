/*
                            COPYRIGHT (c) 1992
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             objects.dc1

Program:       OBJECTS

Purpose:       Find and analyze isolated (extended) structures in
               an image.

Category:      ANALYSIS

File:          objects.c

Author:        M.G.R. Vogelaar
               R. Slagter

Keywords:

   INSET=      Give set, subsets:
               Maximum number of subsets is 2048.

   BOX=        Give box in .....                        [entire subset]
               When you use a box be sure that the output set is empty
               around it. The program doesn't empty the output set
               around the defined box.

** OBJNAME=    Enter a name to identify field:                 [header]
               Tables on screen and on disk are identified by
               a name. The default is a name stored in the header 
               of INSET= 
               You can change this id by entering text here.
 

   OUTSET=     Give output set (, subsets):
               Output set and subset(s) for the result. The number of
               output subsets is the same as the number of input sub-
               sets.

   RANGE=      Give range of levels:                             [ ,->]
               At least one value is needed. Range selects all
               pixels in the analysis.
               To EXCLUDE a range of values, take the first value
               greater than the second. Examples:
               RANGE=3       Include all values >= 3
               RANGE=3 5     Include values >= 3 and <= 5
               RANGE=5 3     Include values <= 3 and >= 5

   MAXCUT=     Enter a cutoff for the max. in an object:         [none]
               Do not include objects that have a maximum intensity
               less than this value. The intensities are in units 
               of the map.

   SIZE=       Give range of object sizes (number of pixels):   [10,->]
               The size of an object in your image must be in the range
               as defined with SIZE= before it is counted as an object.
               To EXCLUDE a range of object sizes, take the first value
               greater than the second. Examples:
               SIZE=3       Include all sizes >= 3
               SIZE=3 5     Include sizes >= 3 and <= 5
               SIZE=5 3     Include sizes <= 3 and >= 5

   BRIDGE=     Give min. thickness allowed for bridges            [1 1]
               If objects are connected by 'bridges', it is
               possible to disconnect them by defining the width
               or height of the bridge. BRIDGE= takes two values. The
               first value is the width in Y for a bridge in the X
               direction and the second is the width in X for a
               bridge in the Y direction.
               If [0 0] is given the program switches from checking
               4-connectivity to 8-connectivity when selecting pixels of
               an object.

   TOSCREEN=   Display table on screen?                           [Y]/N
               If the program examined all objects, it prints a
               table on screen. If you do not want this table, use
               TOSCREEN=N

   TOPLOT=     Make Area-Perimeter plot of objects?               [Y]/N

   MAXBORDER=  Give max. pixels allowed on border of box:           [2]
               This keyword is only prompted when TOPLOT=Y.
               Objects with pixels on a border are still isolated.
               This keyword allows you to eliminate objects, with
               pixels on the border, from the area-perimeter
               calculation. With this keyword you can give the minimum
               number of pixels that are allowed on the borders. The
               borders are given with BOX= . A pixel in the corner of
               the box is counted twice.

   GRDEVICE=   Plot device:                           [List of devices]
               If TOPLOT=Y, specify the destination of the plot.

   VOLUMES=    Connected clouds in subset direction?              [Y]/N
               (Default depends on number of input subsets)
               Using more than one subset in your input, it
               could be interesting to examine connections of objects
               in three dimensions.

   OBJVOLUME=  Give range of volumes:              [as given with size]
               This keyword is only prompted when VOLUMES=Y.
               An object in your image must have a number of pixels in
               the range specified, before it is counted as a volume.

   DEPTH=      Give range in depths of objects:                  [2,->]
               This keyword is only prompted when VOLUMES=Y.
               An object in your image must have a certain number of
               overlapping two dimensional objects in the third
               dimension before it is counted as a volume.
               You can include volumes if the first depth is smaller
               than the second. You can exclude volumes if the first
               depth is greater than the second.

   OVERLAP=    Give the range in overlap:                        [1,->]
               This keyword is only prompted when VOLUMES=Y.
               An two-dimensional object is considered connected to
               another two-dimensional object, if they have a number of
               pixels in the range given by overlap, in common. It is
               also possible to select on the percentage overlap
               between two 2-dimensional objects (See the program
               description for the definition of the overlap percentage).
               In that case; end your range definition with the "%" sign.
               Note: it is only possible to include a range!

   TOSCREEN=   Display table on screen?                           [Y]/N
               This keyword is only prompted when VOLUMES=Y.
               If the program examined all volumes, it prints a
               table on screen. If you do not want this table, use
               TOSCREEN=N

   EXCLUDE=    Give index of volumes to exclude:                 [NONE]
               This keyword is only prompted when VOLUMES=Y.
               Before you make an output set is it possible to exclude
               objects. You can do this by giving their index numbers.
               The program will re-arrange the indices of the allowed
               objects. Because of gipsy limitations, it is only possible
               to use exclude 1024 volumes at once. Therefore the keyword
               EXCLUDE will reappear when your selection contains more
               than 1024 volumes. This loop will terminate if you respond
               with EXCLUDE=NONE. It's also possible to exclude volumes
               later, by using the program "CONDIT" (see program
               description).

   FILENAME=   Name of ASCII file:                  [No output to file]
               If a name is specified, an ASCII file is created to
               store data. If you press carriage return, there will
               be no output to an ASCII file. If a given name already
               exists, APPEND= must be specified.

   APPEND=     File exists, ok to append?                        [Y]/N
               The file specified in FILENAME= already exists.
               You can append to this file with APPEND=Y. If APPEND=N
               you will be prompted for another filename.

   GRDEVICE=   Plot device:                           [List of devices]
               Destination of plot, Screen or Hardcopy.

   AGREED=     Agreed with current selection?                     Y/[N]
               Selection of volumes can be repeated. The keywords
               DEPTH=, OBJVOLUME= and OVERLAP= are asked again.

   AGREED=     Agreed with current output?                        [Y]/N
               Each selection can be stored in a set. This action
               can be repeated for different selections.

               ==== PLOTTING ====

** PGMOSAIC=   View surface sub divisions in x,y:                 [1,1]
               View surface can contain a number of plots in
               in X and Y direction (mosaic). Default is 1 plot in
               both X- and Y direction.

** PGPAPER=    Give width(cm), aspect ratio:               [calc, calc]
               Aspect ratio is height/width.

** PGBOX=      Corners of box Xl,Yl,Xh,Yh:     [default by application]
               It is possible to overrule the calculated
               PGPLOT box size with PGBOX=. The coordinates (x,y) of
               the lower point are given first.

** PGCOLOR=    Give color 1..15:                                    [1]
               See description for the available colors.

** PGWIDTH=    Give line width 1..21:                               [2]

** PGHEIGHT=   Give character height:                             [1.0]

** PGFONT=     Give font 1..4:                                      [2]

** MINBOXSIZ=  Give min. box size for volume:                     [1,1]
               It is possible to set a minimum on the size of
               the minimum box around the volume. The first number
               indicates the minimum length of the box in the x-
               direction. The second number the minimum length in the
               y-direction. This keyword should be set when the other
               volume criteria are asked.

** MAXBOXSIZ=  Give max. box size for volume:                     [ALL]
               It is possible to set a maximum on the size of
               the maximum box around the volume. The first number
               indicates the maximum length of the box in the x-
               direction. The second number the maximum length in the
               y-direction. This keyword should be set when the other
               volume criteria are asked.


Description:   Program description:

               The program "OBJECTS" finds and analyses isolated
               structures (called objects in the program), in two and
               three dimensions, automatically. In two dimensions a
               definition of an object is given with keywords as
               described above. The objects are isolated and stored
               in an output set and the analysis is written to screen
               or ASCII file. Also a plot of area against perimeter
               can be created. The analysis can be extended to the
               third dimension. The objects are then isolated volumes.

               "OBJECTS" starts always with finding objects in two
               dimensions. After the two-dimensional analysis, the user
               can directly continue with a three-dimensional analysis by
               responding with VOLUMES=Y.

               // In the near future it will
               also be possible to leave the program after the two-
               dimensional analysis and do the three-dimensional analysis
               later. Then it will be possible to rerun the three
               dimensional analysis without rerunning the two-dimensional
               analysis again. The user has to respond then to INSET= with
               the set created by the two-dimensional analysis and he will
               be prompted immediately with the keyword VOLUMES= for
               the three-dimensional analysis. //

               The two-dimensional analysis starts with the specification
               of the input with INSET=  The input must be 2-dimensional.
               For a two-dimensional analysis you can manage with only one
               subset. For the three-dimensional case however, you need at
               least two subsets.

               In each subset the program searches for pixels which are in
               the range of image values (as given with the keyword RANGE=).
               You can also exclude data by specifying two values, the
               first value greater than the second one. If you give only
               one value, all pixels with an image value greater than
               this value will be part of an object. Pixels belong to
               the same object if they share at least one side (4-connec-
               tivity). (By setting the keyword BRIDGE=0 0, it is possible
               to change the 4-connectivity mode to 8-connectivity)

               The second criterion for an object is its size in pixels.
               A valid object must contain a number of pixels in the range of
               SIZE= before it is used for further inspection. An object can
               contain so called 'bridges'. These are very small connections.
               Sometimes it is useful to separate such objects. This can
               be achieved with BRIDGE= The default values are BRIDGE=1 1
               This allows bridges of one pixel in size in each direction
               (See description at BRIDGE= for the directions).

               According to Mandelbrot, fractals are objects that exhibit a
               self similar geometry. That is, the appearance of a fractal
               object is the same even when subject to arbitrary magnification.
               The contortion and complexity of the perimeter of such object
               is given by the fractal dimension of the perimeter (D).
               If A is the area of the object and P is the perimeter, the
               fractal dimension is given by the relation:

                                     P ~ A ^ (D/2)

               (smooth appearance, e.g. squares and circles: D = 1 ,
               distorted perimeter: 1 < D <= 2   )
               The keyword TOPLOT=Y allows you to view this relation on a
               graphics device for all the objects selected. The perimeters
               in are summed in X- and Y- direction and multiplied
               with the grid spacing for that direction. The grid spacing
               is read from the header. Also the area is converted to 
               header units. The plot that is showed is a LOG10-LOG10 
               plot. The fractal dimension is equal to twice the average 
               slope.
               
               With the keyword
               MAXBORDER=, all the objects which have more than a certain
               number of pixels on the border of the box are excluded from
               the Area-Perimeter calculation. These objects however are still
               stored in the output set.

               Each valid object found in the search for isolated objects
               is stored in an output set (OUTSET=). Each object gets
               a unique number. The numbers start at 0 and for each new object
               the number is increased by 1 (independent of subset number).

               You can use the results in the output sets to blot out objects
               in your input set. The easiest way to do this is by using
               the program "CONDIT". Give your input set also as input set
               (INSET= ) for "CONDIT". Use one of the results output sets for
               the set which should operate on the input (XSET= ).
               Use the keyword RANGE= for selecting the objects which should
               be blotted out and the way in which they should be blotted out
               ( transfer blanks or transfer the original value or a replace
               value (REPLACE= ) ).

               The program updates a list of all the important information about
               the objects found in the analysis. Implemented are the starting
               position x, y of the objects, the subset, the area, the
               perimeters in x- and y direction, the flux, the flux per pixel,
               the maximum value and the number of pixels on the border of
               the box (see keyword BOX=).
               With the keywords TOSCREEN= and FILENAME= you can get this list
               on screen and file respectively. The Area-Perimeter results
               can be viewed in a plot with TOPLOT=Y. A graphics device
               can be selected with GRDEVICE=.

               The three-dimensional analysis is based on the results of the
               (previous) two-dimensional analysis (so don't change the output
               set).  Each object left from the two-dimensional analysis is
               examined to have overlapping pixels with objects in the
               previous or next subset (connections in the third dimension).
               If two objects have overlapping pixels in the range as
               given with OVERLAP=, then they are connected and a new 3d-object
               (volume) is found. It is also possible to select on the overlap
               percentage. In that case, end your input with "%".

               The user can put further restraints on the volume in pixels
               (keyword OBJVOLUME=) and the range in depths (keyword DEPTH= ,
               the minimum number of planes in which the object was found).
               With the hidden keywords MINBOXSIZ= and MAXBOXSIZ= you can
               also select on the size of the minimum box around a volume.

               The selected volumes and all important information about them,
               are updated in a list. Implemented are the index number, the
               starting position x,y of the volumes, the lowest and highest
               pixel position and subset they are found in, the depth of the
               volume, the volume in pixels, the flux per pixel, the maximum
               value and the overlap-percentage.
               The overlap-percentage of a volume is the average overlap-
               percentage of all overlapping 2-dimensional objects forming the
               volume. The overlap-percentage of two connected 2-dimensional
               objects is 100% if the smallest object is completely overlapped
               by the biggest.

               After inspection of the list of selected volumes the user
               can exclude volumes with EXCLUDE=  The keyword accepts
               the index numbers of volumes in the generated list. Because of
               gipsy limitations, you can only use exclude 1024 volumes at
               once. This has to be no problem because the program will repeat
               prompting you the EXCLUDE= keyword.
               You can also do the excluding later by using "CONDIT".
               If you want to blot out with "CONDIT", the excluding
               with "CONDIT" can be done at the same moment, which will
               save you (for large data cubes) a lot of time.

               If the user is not satisfied with the volume-selection or
               the user wants to create several output sets with different
               selections, he can specify new output sets with keyword
               OUTSETn= where n is an integer number. If the user wants to
               make more output sets he may not use the previous OUTSET= of
               the two-dimensional analysis for OUTSETn=  The loop over
               selections and output sets is controlled by AGREED=

               Be aware of the correct clip levels when viewing the
               resulting output sets. If the clip levels are wrong, iso-
               lated clouds could seem to have similar colors.
               When the maps contain a lot of objects, the value given
               in "GIDS" may be wrong. Use for the correct values the
               program "PRINT". If you want to view just one volume, use
               the box parameters in the table to select a nice box for
               "VIEW".


               OUTPUT TABLE:
               
               Example:
               
    INDX|===X|===Y|SUB|===AREA|===XPER|===YPER|===FLUX|FLUX/PIX|====MAX|BORD|
    -------------------------------------------------------------------------
       1|  -5| -15|  0|    613|    108|     90|10768.5| 17.5669|78.2993|   0|
       2|  10| -12|  0|     32|     20|     16|120.502| 3.76568|7.07311|   0|
       3|  12|   4|  0|     10|     12|      4|40.1944| 4.01944|5.81718|   0|
       4| -22|   8|  0|     12|      8|     12|48.9353| 4.07795|6.70393|   0|
       5| -14| -20|  1|    936|    100|    120| 397945| 425.155|543.007|   0|
                                       
               INDX      Each object gets a unique number, starting with 1
               X         X-position of first pixel encountered that is 
                         part of current object.
               Y         Y-position of first pixel encountered that is 
                         part of current object.
               SUB       The subset number to which the object belongs.
               AREA      The number of pixel in this object.
               XPER      The perimeter in pixels in X-direction.
               YPER      The perimeter in pixels in Y-direction.
               FLUX      Sum of the pixel values in this object.
               FLUX/PIX  Average flux per pixel in this object.
               MAX       The maximum pixel value in this object
               BORD      The number of pixels of this object that belongs
                         to the border of the current box.
                         
                        
                       
               Each table with more than one entry is followed by a table
               with averages and standard deviations.
               
               Example:
               
               Average perimeter in DEGREE = 0.210573 +- 0.251443
               Average perimeter in X in pixels = 38.363636 +- 46.396709
               Average perimeter in Y in pixels = 37.454544 +- 44.750214
               Average area in DEGREE x DEGREE  = 0.001595 +- 0.002571
               Average area in pixels = 206.818176 +- 333.314819
               Average flux in map units = 40646.238281 +- 118619.796875
               Average flux per pixel in map units = 161.308823 +- 212.934387
               Average maximum value in map units = 185.303741 +- 230.783920


               Table if VOLUMES=Y

==NR|===X|===Y|LOWX|LOWY|LOSUB|HIGX|HIGY|HISUB|DEPTH|VOLUME|=FLUX/PIX|======MAX|
--------------------------------------------------------------------------------
   1|  -5| -15| -22| -20|    0|  19|  23|    2|    3|  2167|  191.057|  543.007|
   2|  12|   4|  12|   4|    0|  17|   7|    1|    2|    30|  291.273|  445.613|
   3| -22|   8| -22|   8|    0| -17|  17|    2|    3|    54|  268.066|  478.646|


               NR        Each object gets a unique number, starting with 1.
               X         X-position of first pixel encountered that is 
                         part of current object.
               Y         Y-position of first pixel encountered that is 
                         part of current object.
               LOWX      Lowest X coordinate of this volume.
               LOWY      Lowest Y coordinate of this volume.
               LOSUB     First subset with a pixel that belongs to this volume.
               HIGX      Highest X coordinate of this volume.
               HIGY      Highest Y coordinate of this volume.
               HISUB     Last subset with a pixel that belongs to this volume.
               DEPTH     number of subsets that contributed to this volume
               FLUX      Sum of the pixel values in this volume.
               FLUX/PIX  Average flux per pixel in this volume.
               MAX       The maximum pixel value in this volume.

                         
                                                              
               Each table with more than one entry is followed by a table
               with averages and standard deviations.
               
               Example:

               AVERAGES AND STANDARD DEVIATIONS USING 3 VOLUMES
               ===================================================
               Average volume in pixels = 750.333313 +- 1226.927979
               Average depth in pixels = 2.666667 +- 0.577350
               Average flux in map units = 145744.765625 +- 232351.328125
               Average flux per pixel in map units = 250.132126 +- 52.459873
               Average maximum value in map units = 489.088715 +- 49.529644

               

               COLOR INDICES:

               0      Background
               1      Default (Black if background is white)
               2      Red
               3      Green
               4      Blue
               5      Cyan
               6      Magenta
               7      Yellow
               8      Orange
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

Notes:         Future developments:
               If the input set is the previous output set of the two
               dimensional analysis then the two dimensional analysis
               will be skipped.

Example:

<USER> objects
OBJECTS  Version 1.1  (Aug 12 1993)
<USER> OBJECTS INSET=set_van_tape 1:10
Set set_van_tape has 3 axes
RA                 from  -127 to   128
DEC                from  -127 to   128
FREQ               from     1 to    59
<USER> OBJECTS BOX=-100 -100 100 100
BOX range for set set_van_tape :
RA                 from  -100 to   100
DEC                from  -100 to   100
Be sure your output set is empty around the defined box !!!
<USER> OBJECTS OUTSET=output
Set output has 3 axes
RA                 from  -127 to   128
DEC                from  -127 to   128
FREQ               from     1 to    10
<USER> OBJECTS FILENAME=ASCII_data
<USER> OBJECTS APPEND=y
<USER> OBJECTS RANGE=5.0 10.0
<USER> OBJECTS SIZE=20 4
<USER> OBJECTS BRIDGE=0 0
Mode changed to 8-connectivity
111 objects found
<USER> OBJECTS TOSCREEN=y
<USER> OBJECTS TOPLOT=
<USER> OBJECTS MAXBORDER=0
User: R.Slagter, date: Thu Aug 12 13:39:16 1993
range of data values: [5, 10]
Range of sizes: [20,4]
All bridges allowed
INDX|===X|===Y|SUB|===AREA|===XPER|===YPER|===FLUX|FLUX/PIX|====MAX|BORD|
-------------------------------------------------------------------------
   1|  47|-100|  1|      1|      2|      2|5.36739| 5.36739|5.36739|   1|
   2| -52| -98|  1|      1|      2|      2|5.21503| 5.21503|5.21503|   0|
   3|  86| -92|  1|      1|      2|      2|5.24646| 5.24646|5.24646|   0|
   4|  57| -75|  1|      2|      4|      2|11.3251| 5.66257|6.24437|   0|

CORRELATION AND REGRESSION
==========================
Data points diagram  : n = 108
(1) Regression Y on X: Y = +0.610541 (+/-0.003152) X +1.095081 (+/-0.013920)
(2) Regression X on Y: X = +1.633277 (+/-0.008433) Y -1.801008 (+/-0.013506)
Linear corr. coeff.  : r = 0.998590
Average slope        : M = 0.611403 (+/-0.002232)
Probability that
Parent Distribution
is uncorrelated      : Pc(r,n) < 1.0e-18 ( Very highly significant )
<USER> OBJECTS VOLUMES=
Found 11 connections between objects in successive subsets
<USER> OBJECTS OBJVOLUME=5
<USER> OBJECTS DEPTH=3 7
<USER> OBJECTS OVERLAP=90%
Selected 0 volumes : found in 0.00 sec (0.00 cpu sec)
<USER> OBJECTS TOSCREEN=n
<USER> OBJECTS AGREED=
<USER> OBJECTS OBJVOLUME=
<USER> OBJECTS DEPTH=1
<USER> OBJECTS OVERLAP=1
Selected 1 volumes : found in 0.00 sec (0.00 cpu sec)
<USER> OBJECTS TOSCREEN=y

Analysis of volumes:
minimum volume: 5
range of data values: [5, 10]
overlap: [1, ->]
depth of volumes: [1, ->]

==NR|===X|===Y|LOWX|LOWY|LOSUB|HIGX|HIGY|HISUB|DEPTH|VOLUME|=FLUX/PIX|======MAX|OVERLAP|
----------------------------------------------------------------------------------------
   0|  10| -28|  10| -28|    3|  11| -26|   10|    8|    18|  6.76017|  9.47997|   100%|
<USER> OBJECTS AGREED=y
<USER> OBJECTS EXCLUDE=
<USER> OBJECTS OUTSET1=output
<USER> OBJECTS OKAY=
Set output has 3 axes
RA                 from  -127 to   128
DEC                from  -127 to   128
FREQ               from     1 to    10
<USER> OBJECTS AGREED=y
<STATUS>  OBJECTS   +++ FINISHED +++

Updates:       Jun 25,  1993: VOG, Document created.
               Jul 29,  1997: VOG, Added averages for objects in table
               Oct 29,  1997: VOG, Added OBJNAME= keyword
               Feb  1,  2000: JPT, Increased number of subsets.
#<
*/

                          /*-------------------------------*/
                          /*  OBJECTS.C: INCLUDE FILES     */
                          /*-------------------------------*/


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

/*-----------------*/
/* Common includes */
/*-----------------*/
#include    "init.h"         /* Declare task running to HERMES and initialize.*/
#include    "finis.h"        /* Informs HERMES that servant quits and cleans up the mess.*/
#include    "anyout.h"       /* General character output routine for GIPSY programs.*/
#include    "setfblank.h"    /* Subroutine to set a data value to the universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "stabar.h"       /* Shows in a bar the current status of a proces. */
#include    "getusernam.h"   /* Returns the user name of the current user.*/
#include    "getdate.h"      /* Returns the current time and date as a text string */
#include    "status.h"
#include    "timer.h"        /* Used for timing the calculations. */
#include    "gdsd_rchar.h"   /* Read an item, defined by characters, from the header */
#include    "gdsd_rdble.h"   /* Read an item, defined by doubles, from the header */
#include    "dcdreal.h"      /* Convert a string into reals */

/*---------------------*/
/* User input routines */
/*---------------------*/
#include    "userint.h"      /* User input interface routines.*/
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "usercharu.h"
#include    "reject.h"       /* Reject user input.*/
#include    "cancel.h"       /* Remove user input from table maintained by HERMES.*/

/*---------------*/
/* Input of sets */
/*---------------*/
#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdspos.h"       /* Define a position in a subset.*/
#include    "gdsbox.h"       /* Define a box inside/around a subset.*/
#include    "gds_close.h"
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid */
                             /* value for each axis.*/
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/
#include    "gdsd_rreal.h"
#include    "minmax3.h"      /* Find min, max and #blanks in subset. */
#include    "wminmax.h"      /* Writes (new) minimum and maximum and number */
                             /* of blanks of subsets in the descriptor file */
                             /* and optionally deletes the MINMAX descriptors */
                             /* at intersecting levels. */

/*-----------------------------*/
/* Output set related includes */
/*-----------------------------*/
#include    "gdsasn.h"       /* GDSASN copies the coordinate system of a */
                             /* previously opened input set obtained with */
                             /* GDSINP to the output set to be obtained */
                             /* with GDSOUT. */
#include    "gdsout.h"       /* GDSOUT prompts the user to enter the */
                             /* name of an output set and the subsets, */
                             /* and returns the number of subsets entered. */
#include    "gdsi_write.h"   /* Writes data to (part of) an set. */

/*-----------------*/
/* PGPLOT includes */
/*-----------------*/
#include    "pgplot.h"



                          /*------------------*/
                          /*   DEFINITIONS:   */
                          /*------------------*/


/*---------------------------------------------------------*/
/* Initialize Fortran compatible string with macro 'fmake' */
/*---------------------------------------------------------*/
#define fmake(fchr,size) { \
                           static char buff[size+1]; \
                           int i; \
                           for (i = 0; i < size; buff[i++] = ' '); \
                           buff[i] = 0; \
                           fchr.a = buff; \
                           fchr.l = size; \
                         }
/*----------------------------*/
/* Malloc version of 'fmake'  */
/*----------------------------*/
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

#define RELEASE        "1.1"               /* Version number */
#define MAXAXES        10                  /* Max. axes in a set */
#define MAXSUBSETS     2048                /* Max. allowed subsets */
#define MAXBUF         4096                /* Buffer size for I/O */
#define MAXOBJECTS     4096                /* Max. number of objects to find */
#define MAXCONNECT     256                 /* Max. connections before reallocation is needed */
#define MAXITEMS       1024                /* Max. items allowed in user input routines */
#define STRLEN         80                  /* Max length of strings */
#define KEYLEN         20                  /* Max length of keywords */
#define NONE           0                   /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1                   /* C versions of .TRUE.  */
#define NO             0                   /*           and .FALSE. */

/*------------------------------------*/
/* Defines for in/output routines etc.*/
/*------------------------------------*/
#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_BOX        tofchar("BOX=")
#define MES_BOX        tofchar(" ")
#define KEY_OUTSET     tofchar("OUTSET=")
#define MES_OUTSET     tofchar("Give output set (subset(s)): ")
#define KEY_OUTSET2    tofchar("OUTSET2=")
#define MES_OUTSET2    tofchar("Give output set (subset(s)):    [Previous outset] ")

/*---------------------*/
/* Variables for input */
/*---------------------*/
static fchar    Setin;                  /* Name of input set */
static fint     subin[MAXSUBSETS];      /* Subset coordinate words */
static fint     nsubs;                  /* Number of input subsets */
static fint     dfault;                 /* Default option for input etc */
static fint     axnum[MAXAXES];         /* Array of size MAXAXES containing the */
                                        /* axes numbers.  The first elements (upto */
                                        /* the dimension of the subset) contain the */
                                        /* axes numbers of the subset, the other */
                                        /* ones ontain the axes numbers outside the */
                                        /* the subset ordered ccording to the */
                                        /* specification by the user. */
static fint     showdev;                /* Device number (as in ANYOUT) for info */
static fint     axcount[MAXAXES];       /* Array of size MAXAXES containing the */
                                        /* number of grids along an axes as */
                                        /* specified by the user. The first elements */
                                        /* (upto the dimension of the subset) contain */
                                        /* the length of the subset axes, the other */
                                        /* ones contain the the number of grids along */
                                        /* an axes outside the subset. */
static fint     maxsubs = MAXSUBSETS;
static fint     maxaxes = MAXAXES;      /* Max num. of axes the program can deal with.*/
static fint     class = 1;              /* Class 1 is for applications which repeat */
                                        /* the operation for each subset, Class 2 */
                                        /* is for applications for which the operation */
                                        /* requires an interaction between the different */
                                        /* subsets. */
static fint     subdim;                 /* Dimensionality of the subsets for class 1 applications */
static fint     setdim;                 /* Dimension of set. */
static fint     subsetgrid[MAXSUBSETS]; /* Translate local subset numbers in real subset numbers */

/*-----------------------*/
/* Box and frame related */
/*-----------------------*/
static fint     flo[MAXAXES];           /* Low  edge of frame in grids */
static fint     fhi[MAXAXES];           /* High edge of frame in grids */
static fint     blo[MAXAXES];           /* Low  edge of box in grids */
static fint     bhi[MAXAXES];           /* High edge of box in grids */
static fint     boxopt;                 /* The different options are: */
                                        /*  1 box may exceed subset size */
                                        /*  2 default is in BLO */
                                        /*  4 default is in BHI */
                                        /*  8 box restricted to size defined in BHI*/
                                        /*  These codes work additive.*/
                                        /*  When boxopt is 0 or 1, the default is the */
                                        /*  is the entire subset. */

/*--------------*/
/* Reading data */
/*--------------*/
static fint     maxIObuf = MAXBUF;      /* Maximum size of read buffer. */
static fint     pixelsread;             /* Number of pixels read by read routine. */
static fint     pixelswrite;            /* Number of pixels to write to output. */
static float    *image = NULL;          /* Buffer for read routine. */
static float    *imageout = NULL;       /* Buffer for read routine. */
static int      *posx = NULL;           /* Buffer for read routine. */
static int      *posy = NULL;           /* Buffer for read routine. */
static fint     subnr;                  /* Counter for subset loop. */

/*--------------------------*/
/* OUTSET related variables */
/*--------------------------*/
static fchar    Setout;
static fchar    Setout2;
static fint     subout[MAXSUBSETS];     /* Output subset coordinate words */
static fint     subout2[MAXSUBSETS];    /* Output subset coordinate words */
static fint     nsubsout;
static fint     axnumout[MAXAXES];
static fint     axcountout[MAXAXES];
static fint     cwloO, cwhiO;           /* Output Coordinate words. */
static fint     tidO;                   /* Transfer id for write function. */

/*------------------*/
/* PGPLOT variables */
/*------------------*/
const  fint  background  =  0;          /* Color definitions for PGPLOT. */
const  fint  foreground  =  1;          /* Black if background is white. */
const  fint  red         =  2;
const  fint  green       =  3;
const  fint  blue        =  4;
const  fint  cyan        =  5;
const  fint  magenta     =  6;
const  fint  yellow      =  7;
const  fint  orange      =  8;
const  fint  greenyellow =  9;
const  fint  greencyan   = 10;
const  fint  bluecyan    = 11;
const  fint  bluemagenta = 12;
const  fint  redmagenta  = 13;
const  fint  darkgray    = 14;
const  fint  lightgray   = 15;
static fint  symbol      =  2;          /* Take a plus as plot symbol, see PGPLOT MANUAL */

/*-------------------*/
/* 'stabar' related: */
/*-------------------*/
static float    STBstart;               /* Start value = 0 */
static float    STBend;                 /* Max number of pixels to examin */
static float    STBcurrent;             /* Index of current pixel */

/*---------------*/
/* Miscellaneous */
/*---------------*/
static fchar    Key, Mes;
static fchar    Cunit[2];
static char     cu[2][20];
static double   cdelt[2];
static fint     setlevel = 0;           /* To get header items at set level. */
static float    blank;                  /* Global value for BLANK. */
static fint     r1, r2;                 /* Result values for different routines. */
static char     message[120];           /* All purpose character buffer. */
static int      i;                      /* Various counters. */
static bool     agreed;                 /* Loop guard. */
static bool     stop_selections = NO;   /* Break volumes selection if 2 dim results set is overwritten */
static bool     box_defined = NO;       /* Checks if box is defined */
static bool     save_results;           /* If TOSCREEN= or ASCIIFILE= */
static float    minval[MAXSUBSETS];     /* Min. value of data for each subset. */
static float    maxval[MAXSUBSETS];     /* Max. value of data for each subset. */
static fint     nblanks[MAXSUBSETS];    /* Number of blanks in each subset. */
static fint     mcount;                 /* Initialize MINMAX3. */
static fint     change;                 /* Used in WMINMAX. change!=0 means */
                                        /* minimum and maximum have changed and */
                                        /* that the MINMAX descriptors at */
                                        /* intersecting levels will be removed. */
FILE            *asciifile;             /* File pointer to ASCII file */
static char     writefile[80];          /* Dummy file name for ASCII file */
static int      lx, ly;                 /* Size of the image to be read */
static fint     nitems;                 /* Used in keywords settings */
static float    clip[2];                /* The range of the pixels */
static float    maxcut;
static int      area;                   /* The area of an object in pixels */
static int      xperim, yperim;         /* Perimeters of an object in x,y directions */
static int      bordercount;            /* Number of pixels on a border */
static fint     objsize[2];             /* Range of object sizes in pixels for an object to be counted */
static fint     maxborder;              /* Max. number of pixels allowed on the border of the box */
static float    curval;                 /* Value of all pix. in isolated object */
static int      currentobject = 0;      /* Index of isolated output object */
static float    flux;                   /* Flux of a object */
static float    maxvalue;               /* Max. value of the object */
static bool     volumes;                /* If YES then do the 3-dimensional part */
static float    *Xarray = NULL;         /* Plot position X for the Area-Perimeter relation */
static float    *Yarray = NULL;         /* Plot position Y for the Area-Perimeter relation */
static bool     toscreen;               /* If YES make a table */
static bool     toplot;                 /* If YES make a Area-Perimeter plot */
static int      device;                 /* The number of the device for writing results */
static fint     objvolume[2];           /* 3d objects related */
static fint     planes[2];              /* 3d objects related */
static int      subsetend[MAXSUBSETS];  /* The last connectionnumber of a subset */
static int      volume1;                /* Volume of a volume */
static float    maxval1;                /* The max. value of a volume */
static float    flux1;                  /* The flux of a volume */
static int      depth1;                 /* The depth of a volume */
static int      connections;            /* Counter for the number of connections in forming a volume */
static float    overlapfactor;          /* The factor of overlap to the area of smallest overlapping object */
static int      minconnect;             /* The lowest connectionnumber of a volume */
static int      maxconnect;             /* The highest connectionnumber of a volume */
static fint     bridge[2];              /* The bridges allowed for bridge connected objects */
static bool     eightconnectivity = NO; /* Switch from 4-connectivity to 8-connectivity */
static bool     check_bridge = YES;     /* Is a check on bridges necessary? */
static int      framebox[2][2];         /* Contains the coordinates of the smallest box */
                                        /* around an object.                            */
                                        /* framebox[0][] = the lowest coordinate        */
                                        /* framebox[1][] = the highest coordinate       */
                                        /* framebox[][0] = x coordinate                 */
                                        /* framebox[][1] = y coordinate                 */
static fint     minboxsize[2];          /* minimum size allowed for the box of a volume */
static fint     maxboxsize[2];          /* maximum size allowed for the box of a volume */
static fchar    Percstr;                /* String containing percentages */
static float    overlap[2];             /* The range of overlap */
static bool     percent = NO;           /* If YES the user wants percentages */
static fint     err;                    /* errorlevel */
static fchar    Headerobjname;
static char     headerobjname[KEYLEN+1];



/*-------------------------------------------------------------*/
/* In the 3-dim case, an array of connections must be updated. */
/* A connection can only exist in two successive subsets.      */
/*-------------------------------------------------------------*/
typedef struct {
   int    previous;
   int    current;
   int    subset;
   int    free;
   int    overlap;
   float  overlapfactor;
} connect;

static connect  *connection = NULL;
static int      maxconnections = MAXCONNECT;

/*-----------------------------------------*/
/* Structure listing the information about */
/* 2-dimensional objects                   */
/*-----------------------------------------*/
typedef struct {
   int      index;
   fint     subset;
   int      Xpos;
   int      Ypos;
   float    area;
   float    Xperimeter;
   float    Yperimeter;
   int      bordercount;
   float    flux;
   int      pixels;
   float    maxval;
   int      free;
   int      framebox[2][2];
} isolatedobject;

static isolatedobject   *object = NULL;
static int              maxobjects = MAXOBJECTS;

/*-----------------------------------------*/
/* Structure listing the information about */
/* 3-dimensional objects (volumes)         */
/*-----------------------------------------*/
typedef struct {
   int      index;
   int      startX;
   int      startY;
   int      losub;
   int      hisub;
   int      volume;
   float    overlapfactor;
   float    flux;
   float    maxval;
   int      framebox[2][2];
} isolatedvolume;

static isolatedvolume   *object3d = NULL;



                          /*-----------------*/
                          /*    FUNCTIONS    */
                          /*-----------------*/


static void anyoutC( int dev, char *anyCstr )
/*------------------------------------------------------------------*/
/* The C version of 'anyout_c' needs two parameters:                */
/* an integer and a C-type string. The integer determines           */
/* the destination of the output which is:                          */
/*    0  use default [set by HERMES to 3 but can be changed by user]*/
/*    1  terminal                                                   */
/*    2  LOG file                                                   */
/*    8  terminal, suppressed in "experienced mode"                 */
/*   16  terminal, only when in "test mode"                         */
/*------------------------------------------------------------------*/
{
   fint ldev = (fint) dev;
   anyout_c( &ldev, tofchar( anyCstr ) );
}


static void display( int dev, FILE *fp, char *anyCstr )
/*------------------------------------------------------------------*/
/* A C version of anyout, extended with the possibility to write to */
/* file. If writing to file, a newline character is appended.       */
/*------------------------------------------------------------------*/
{
   if (dev >= 0) {
      fint ldev = (fint) dev;
      anyout_c( &ldev, tofchar( anyCstr ) );
   }
   if (fp != NULL) {
      if (anyCstr)
         fprintf( fp, "%s\n", anyCstr );        /* Write to ASCII file */
      else
         fprintf( fp, "\n" );
   }
}


FILE *openfile( fchar Key, fchar Mes, fint dfault, char *filename,
                char mode )
/*-------------------------------------------------------------*/
/* Key       Keyword                                           */
/* Mes       Message                                           */
/* dfault    Default for user                                  */
/* filename  Default filename, if length equals 0 then the     */
/*           default means no file pointer returned            */
/* mode      character r or w for read/write in fopen C        */
/*           function                                          */
/*                                                             */
/* Open file for writing/reading. Ask filename in GIPSY way    */
/* Check file for existence. Return file pointer and the name  */
/* of the given file. The function introduces the keyword      */
/* APPEND= for 'write' files that already exist.               */
/* If APPEND=N the existing file will be overwritten.          */
/*-------------------------------------------------------------*/
{
#include    "stdio.h"
#include    "ctype.h"
#include    "gipsyc.h"
#include    "usertext.h"
#include    "userlog.h"
#include    "cancel.h"
#include    "reject.h"

   bool      append;
   fint      nitems = 1;
   fint      agreed;
   fint      n;
   FILE     *fp;
   bool      readmode, writemode;
   int       yes = 1, no = 0;
   char      filebuf[132];
   fchar     Filename;
   bool      nodeffile;


   Filename.a = filebuf;
   Filename.l = 132;
   readmode  = ('R' == toupper(mode) );
   writemode = ('W' == toupper(mode) );
   nodeffile = (strlen(filename) == 0);
   if (readmode) {
      do {
         n = usertext_c( Filename, &dfault, Key, Mes );
         if (n == 0) {
            if (nodeffile) return(NULL);
         } else {
            strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */
         }
         fp = fopen(filename, "r");
         if (fp == NULL) {
            reject_c( Key, tofchar("Cannot read file") );
            if (dfault >= 2) dfault = 1;
            nodeffile = yes;
            Mes = tofchar( "Try another file name:" );
         }
      } while (fp == NULL);
      return( fp );
   }
   if (writemode) {
      do {
         n = usertext_c( Filename, &dfault, Key, Mes );
         if (n == 0) {
            if (nodeffile) return(NULL);
         } else {
            strcpy( filename, strtok(Filename.a, " ") );      /* Delete after space */
         }
         fp = fopen(filename, "r");
         /* cancel_c( Key ); */
         if (fp != NULL) {       /* File exists */
            append = toflog( no );
            n   = userlog_c( &append,
                             &nitems,
                             &dfault,
                             tofchar("APPEND="),
                             tofchar("File exists, append?   Y=append/[N=overwrite]") );
            append = tobool( append );
            fclose( fp );
            cancel_c( tofchar("APPEND=") );
            if (append) {
               fp = fopen(filename, "a");
               agreed = (fp != NULL);
               if (!agreed) {
                  reject_c( Key, tofchar("Cannot open for appending, try another!") );
               } else {
                  return( fp );
               }
            } else {
               fp = fopen(filename, "w");
               agreed = (fp != NULL);
               if (!agreed) {
                  reject_c( Key, tofchar("Cannot open for writing, try another!") );
               } else {
                  return( fp );
               }
            }
         } else {
            /* File does not exist */
            fp = fopen(filename, "w");
            agreed = (fp != NULL);
            if (!agreed) {
               reject_c( Key, tofchar("Cannot open for writing, try another!") );
            } else {
               return( fp );
            }
         }
      } while (!agreed);
   }
   return( NULL );                /* Return NULL if not write or read mode */
}


void initplot( void )
/*------------------------------------------------------------------*/
/* Initialize plot software. Set viewport and output dimensions.    */
/* If output device is a printer, ask user for line width.          */
/*------------------------------------------------------------------*/
{
   fint   unit;            /* Ignored by pgbeg, use unit=0. */
   fchar  Devspec;         /* Device specification. */
   fint   nxysub[2];       /* Number of subdivisions on 1 page. */
   float  width;           /* Width of output on paper */
   float  aspect;          /* Aspect ratio of output on paper */
   fint   nitems, dfault;
   fint   r1;
   fint   errlev = 4;      /* Set error level to fatal. */
   bool   pageoff;         /* Disable PGPLOT's NEXTPAGE keyword. */
   float  paper[2];
   float  xl, xr, yb, yt;  /* Edges of the viewport. */


   /* Begin PGPLOT, open output device. A return value of 1 indicates */
   /* successful completion. There are 4 arguments for PGBEG:         */
   /* UNIT, this argument is ignored by PGBEG (use zero).             */
   /* FILE, If this argument is a question mark PGBEG will prompt the */
   /*       user to supply a string.                                  */
   /* NXSUB,the number of subdivisions of the view surface in X.      */
   /* NYSUB,the number of subdivisions of the view surface in Y.      */

   nxysub[1] = nxysub[0] = 1;           /* Default no subdivisions in plot.*/
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
      /* If width = 0.0 then the program will select the largest view surface */
      width  = paper[0] / 2.54;      /* Convert width to inches. */
      aspect = paper[1];
      pgpap_c( &width, &aspect );
   }

   /* Set viewport */
   xl = 0.2; xr = 0.9;
   yb = 0.3; yt = 0.9;
   pgsvp_c( &xl, &xr, &yb, &yt );
}


void drawbox( float Xmin, float Ymin, float Xmax, float Ymax )
/*------------------------------------------------------------------*/
/* Draw box and labels. Take special care for the y labels and      */
/* title. Colors are defined globally. Xmin etc are the corners of  */
/* the box in world coordinates.                                    */
/*------------------------------------------------------------------*/
{
   float  charsize = 1.0;
   float  delta;
   fint   lwidth;
   fint   r1;
   fint   nitems;
   fint   dfault;
   float  pg_box[4];                          /* Corners of draw box. */
   fint   color;
   fint   font;
   fint   nxsub, nysub;
   float  xtick, ytick;
   fchar  Xtitle, Ytitle, Toptitle;
   char   message[80];


   pgpage_c();                                /* Advance to new page. */

   /* Increase the size of the box a little */
   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Xmin -= delta; Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0) delta = 1.0;
   Ymin -= delta; Ymax += delta;
   pg_box[0] = Xmin; pg_box[1] = Ymin;        /* Get size from user input */
   pg_box[2] = Xmax; pg_box[3] = Ymax;
   nitems = 4; dfault = HIDDEN;
   sprintf( message, "Corners of box Xl,Yl, Xh,Yh:  [%f,%f,%f,%f]", Xmin,Ymin,Xmax,Ymax );
   r1 = userreal_c( pg_box,
                    &nitems,
                    &dfault,
                    tofchar("PGBOX="),
                    tofchar( message ) );
   Xmin = pg_box[0]; Ymin = pg_box[1];
   Xmax = pg_box[2]; Ymax = pg_box[3];
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );    /* Set the window */

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
   pgslw_c( &lwidth );                        /* Set line width. */

   charsize = 1.0; nitems = 1; dfault = HIDDEN;
   r1 = userreal_c( &charsize,
                    &nitems,
                    &dfault,
                    tofchar("PGHEIGHT="),
                    tofchar("Give character height:     [1.0]") );
   pgsch_c( &charsize );                      /* Character height. */

   font = 2; nitems = 1; dfault = HIDDEN;
   r1 = userint_c( &font,
                   &nitems,
                   &dfault,
                   tofchar("PGFONT="),
                   tofchar("Give font 1..4:        [2]") );
   if (font > 4) font = 4;
   if (font < 1) font = 1;
   pgscf_c( &font );                          /* Set font. */

   /* xtick is world coordinate interval between major tick marks    */
   /* on X axis. If xtick=0.0, the interval is chosen by PGBOX, so   */
   /* that there will be at least 3 major tick marks along the axis. */
   /* nxsub is the number of subintervals to divide the major        */
   /* coordinate interval into. If xtick=0.0 or nxsub=0, the number  */
   /* is chosen by PGBOX.                                            */
   /* BCNSTV :                                                       */
   /* B: draw bottom (X) or left (Y) edge of frame.                  */
   /* C: draw top (X) or right (Y) edge of frame.                    */
   /* N: write Numeric labels in the conventional location below     */
   /*    the viewport (X) or to the left of the viewport (Y).        */
   /* S: draw minor tick marks (Subticks).                           */
   /* T: draw major Tick marks at the major coordinate interval.     */
   /* V: orient numeric labels Vertically. This is only applicable   */
   /*    to Y.                                                       */
   xtick = ytick = 0.0;
   nxsub = nysub = 0;
   pgbox_c( tofchar("BCNST" ), &xtick, &nxsub,
            tofchar("BCNSTV"), &ytick, &nysub );

   /* Create titles */

   fmake( Xtitle, 80 ); fmake( Ytitle, 80 ); fmake( Toptitle, 80);
   Xtitle = tofchar("Log area"); Ytitle = tofchar("Log perimeter");
   {  /* Create to title with set input in it */
      fint r1, dfault = HIDDEN;
      r1 = usertext_c( Toptitle, &dfault, tofchar("INSET="), tofchar(" ") );
   }
   pglab_c( Xtitle, Ytitle, Toptitle );
}


static int inrange( float value )
/*------------------------------------------------------------*/
/* Determine if a value is in range as specified by the user  */
/*------------------------------------------------------------*/
{
   if (value == blank) return(0);        /* A blank is out of range */

   if (clip[0] <= clip[1]) {
      /*-----------------------------------*/
      /* Include values between cliplevels */
      /*-----------------------------------*/
      if ((value >= clip[0]) && (value <= clip[1])) {
         return(1);
      } else {
         return(0);
      }
   } else {
      /*-----------------------------------*/
      /* Exclude values between cliplevels */
      /*-----------------------------------*/
      if ((value <= clip[0]) || (value >= clip[1])) {
         return(1);
      } else {
         return(0);
      }
   }
}


static void SETIM( int x, int y, float value )
/*------------------------------------------------------------*/
/* The x, y coordinates are in the range of blo, bhi. The     */
/* coordinates are converted to one dimensional positions and */
/* the image value is returned.                               */
/*------------------------------------------------------------*/
{
   if ((x > bhi[0]) || (x < blo[0])) return;
   if ((y > bhi[1]) || (y < blo[1])) return;
   image[ ((y-blo[1]) * lx) + (x-blo[0]) ] = value;
}


static void SETOUTIM( int x, int y, float value )
/*------------------------------------------------------------*/
/* The x, y coordinates are in the range of blo, bhi. The     */
/* coordinates are converted to one dimensional positions and */
/* the image value is returned.                               */
/*------------------------------------------------------------*/
{
   if ((x > bhi[0]) || (x < blo[0])) return;
   if ((y > bhi[1]) || (y < blo[1])) return;
   imageout[ ((y-blo[1]) * lx) + (x-blo[0]) ] = value;
}


static float GETOUTIM( int x, int y )
/*------------------------------------------------------------*/
/* The x, y coordinates are in the range of blo, bhi. The     */
/* coordinates are converted to one dimensional positions and */
/* the image value is returned.                               */
/*------------------------------------------------------------*/
{
   if ((x > bhi[0]) || (x < blo[0])) return( blank );
   if ((y > bhi[1]) || (y < blo[1])) return( blank );
   return imageout[ ((y-blo[1]) * lx) + (x-blo[0]) ];
}


static float GETIM( int x, int y )
/*------------------------------------------------------------*/
/* The x, y coordinates are in the range of blo, bhi. The     */
/* coordinates are converted to one dimensional positions and */
/* the image value is returned.                               */
/*------------------------------------------------------------*/
{
   if ((x > bhi[0]) || (x < blo[0])) return( blank );
   if ((y > bhi[1]) || (y < blo[1])) return( blank );
   return image[ ((y-blo[1]) * lx) + (x-blo[0]) ];
}


static int fillimage( fchar Setin, fint subset, fint *blo, fint *bhi,
                      fint imagenr )
/*--------------------------------------------------------*/
/* Read data from a subset in one of the two data buffers */
/*--------------------------------------------------------*/
{
   fint   cwlo, cwhi;
   fint   tid = 0;

   cwlo   = gdsc_fill_c( Setin, &subset, blo );
   cwhi   = gdsc_fill_c( Setin, &subset, bhi );
   pixelsread = 0;
   /* Read 'maxIObuf' values in 'image'. */
   if (imagenr == 1) {
      gdsi_read_c( Setin, &cwlo, &cwhi, image, &maxIObuf, &pixelsread, &tid );
   } else {
      gdsi_read_c( Setin, &cwlo, &cwhi, imageout, &maxIObuf, &pixelsread, &tid );
   }
   return(pixelsread);
}


static int inbox(int px , int py)
/*-------------------------------------------------------*/
/* Checks if the pixelposition is within the allowed box */
/*-------------------------------------------------------*/
{
   if (px < blo[0]) {                   /* x position left  of box */
      return(0);
   } else {
      if (px > bhi[0]) {                /* x position right of box */
         return(0);
      } else {
         if (py < blo[1]) {             /* y position under box    */
            return(0);
         } else {
            if (py > bhi[1]) return(0); /* y position above box    */
         }
      }
   }
   return(1);
}


static int inobject(int px, int py)
/*-------------------------------*/
/* Check if pixel(px,py) belongs */
/* to the current object.        */
/*-------------------------------*/
{
    float value;
    if (inbox(px,py)) {                     /* position has to be in the defined box */
       value = GETIM(px,py);
       if (inrange(value)) return(1);
       /*----------------------------------------------------*/
       /* if pixel is in range then it belongs to the object */
       /* if not look at output to find out if position is   */
       /* already examined and belongs to the current object */
       /*----------------------------------------------------*/
       value = GETOUTIM(px,py);
       if (value != blank) {
       	  if (curval == value) {
       	     return(1);
       	  }
       }
    }
    return(0);
}


static int found_bridge( int px, int py )
/*------------------------------------------------------------*/
/* Check if pixel(px,py) is a bridge. If the thickness of the */
/* bridge is smaller than defined, the pixel is considered    */
/* a bridgepixel that should be eliminated from the object    */
/*------------------------------------------------------------*/
{
   if (check_bridge) {                                 /* Check only if a check on bridges is wanted */
          int thickness,store;

          /*---------------------------------------*/
          /* Check if pixel is a horizontal bridge */
          /*---------------------------------------*/
   	  if ( inobject(px-1,py) && inobject(px+1,py) ) {
   	     thickness = 1;    /* pixel is a bridge and should be counted */

             /*-------------------------------------------------------------*/
   	     /* Determine the thickness of the brige above the bridge pixel */
   	     /*-------------------------------------------------------------*/
             while ( (inobject(px,py+thickness)) && (thickness < bridge[0]) ) thickness++;
             store = thickness-1;

             /*-------------------------------------------------------------*/
   	     /* Determine the thickness of the brige under the bridge pixel */
   	     /*-------------------------------------------------------------*/
             while ( (inobject(px,py-thickness+store)) && (thickness < bridge[0]) ) thickness++;

             if ( thickness < bridge[0] ) return(1) ;  /* Thickness of the bridge is smaller than defined */
          }

          /*---------------------------------------*/
          /* Check if pixel is a vertical bridge   */
          /*---------------------------------------*/
     	  if ( inobject(px,py-1) && inobject(px,py+1) ) {
   	     thickness = 1;

             /*-------------------------------------------------------------*/
   	     /* Determine the thickness of the brige to the right           */
   	     /*-------------------------------------------------------------*/
             while ( (inobject(px+thickness,py)) && (thickness < bridge[1]) ) thickness++;
             store = thickness-1;

             /*-------------------------------------------------------------*/
   	     /* Determine the thickness of the brige to the left            */
   	     /*-------------------------------------------------------------*/
             while ( (inobject(px-thickness+store,py)) && (thickness < bridge[1]) ) thickness++;

             if ( thickness < bridge[1] ) return(1) ;  /* Thickness of the bridge is smaller than defined */
          }
   }
   return(0);
}


static int insize( int size)
/*-----------------------------------------------------*/
/* Check if the size of an object is within the range  */
/* as defined by the user in objsize[0] and objsize[1] */
/*-----------------------------------------------------*/
{
   if (objsize[1] == 1) {
      if ( size < objsize[0] ) return(0);
   } else {
   	 if (objsize[0] > objsize[1]) {
   	    if ( (size < objsize[0]) && (size > objsize[1]) ) return(0);
   	 } else {
            if ( (size < objsize[0]) || (size > objsize[1]) ) return(0);
   	 }
   }
   return(1);
}

static void checkneighbours( int i, int j )
/*---------------------------------------------------------------*/
/* Check neighbours recursively. Neighbour pixels share one side */
/* (so called four-connectivity). Count the number of pixels in  */
/* the object and count the perimeters in x- and y directions.   */
/* A pixel is part of an object if its image value is within a   */
/* user given range of values. The object is counted if it's     */
/* objectsize is in the range defined by the user. Because we    */
/* only know afterwards if a object has the required range, it's */
/* necessary to store at least a number of pixels equal to the   */
/* maximum of the two rangvalues as given by the user. If the    */
/* is in the objects size rangeobject, all its pixels get the    */
/* same value in the output. All examined pixels get the blank   */
/* value to indicate that they are counted. The number of pixels */
/* in an object that are part of the border of an image are      */
/* counted in 'bordercount'.                                     */
/*---------------------------------------------------------------*/
{
   float   val;

   /*-------------------------------------------------------------*/
   /* Check if pixel is in range and not forms a too small bridge */
   /* In that case pixel(i,j) belongs to current object           */
   /*-------------------------------------------------------------*/
   if ( !inrange(GETIM(i,j)) ) return;
   if ( found_bridge(i,j) ) return;

   /*---------------------------------------------------------------*/
   /* Start giving the object a colour, 'curval' is the current     */
   /* colour of the object, OUTIM is the array with the output      */
   /* pixel values. Count the area, flux.                           */
   /*---------------------------------------------------------------*/
   SETOUTIM( i, j, curval );
   area += 1;
   val   = GETIM(i, j);
   flux += val;

   /*----------------------------------------*/
   /* Determine the max. value in the object */
   /*----------------------------------------*/
   if (maxvalue == blank) {
      maxvalue = val;
   } else {
      if (val > maxvalue) maxvalue = val;
   }

   /*----------------------------------------------*/
   /* Determine the smallest box around the object */
   /*----------------------------------------------*/
   if ( (i < framebox[0][0]) ) framebox[0][0] = i;
   if ( (j < framebox[0][1]) ) framebox[0][1] = j;
   if ( (i > framebox[1][0]) ) framebox[1][0] = i;
   if ( (j > framebox[1][1]) ) framebox[1][1] = j;

   SETIM( i, j, blank );          /* This pixel need not to be examined anymore */

   /*---------------------------------------------------------------*/
   /* For all objects > 2 pixels the first pixel is set to blank    */
   /* and the next pixel sees a perimeter where the first pixel was */
   /* because there is a blank now ( and therefore the first pixel  */
   /* is not 'inrange' anymore. So compensate for this perimeter.   */
   /*---------------------------------------------------------------*/
   if ( !inobject( i-1,j ) || found_bridge( i-1,j ) ) yperim += 1;
   if ( !inobject( i,j-1 ) || found_bridge( i,j-1 ) ) xperim += 1;
   if ( !inobject( i+1,j ) || found_bridge( i+1,j ) ) yperim += 1;
   if ( !inobject( i,j+1 ) || found_bridge( i,j+1 ) ) xperim += 1;

   /*-------------------------------------------------------------------*/
   /* If a neighbour is found on one of the borders, its perimeter is   */
   /* already increased, but the border counter is still to be updated. */
   /* Take into account pixels in corners (add to the border counter    */
   /* twice).                                                           */
   /*-------------------------------------------------------------------*/
   if ( ((i-1) < blo[0]) || ((i+1) > bhi[0]) ) {
      bordercount += 1;
   }
   if ( ((j-1) < blo[1]) || ((j+1) > bhi[1]) ) {
      bordercount += 1;
   }

   /*-----------------------------------------------------------------*/
   /* Start the recursive search for neighbours. Made suitable for so */
   /* called 4-connectivity. Note that 8-connectivity is included at  */
   /* this point if the user changed the mode.                        */
   /*-----------------------------------------------------------------*/
   if (eightconnectivity) {
      checkneighbours( i-1,j-1 );
      checkneighbours( i-1,j+1 );
      checkneighbours( i+1,j-1 );
      checkneighbours( i+1,j+1 );
   }
   checkneighbours( i-1,j );
   checkneighbours( i,j-1 );
   checkneighbours( i+1,j );
   checkneighbours( i,j+1 );
   return;
}


static void getstamp( char *message )
/*-----------------------------------------------*/
/* Get user id and date and create a string like */
/* G. Grsnuzkyy, Fri Jul 16 11:16:51 1993        */
/*-----------------------------------------------*/
{
   fchar   Idstr;
   fmake( Idstr, STRLEN );
   getusernam_c( Idstr );
   (void) sprintf( message, "User: %.*s", nelc_c( Idstr ), Idstr.a );
   getdate_c( Idstr );
   (void) sprintf( message, "%.*s, date: %.*s", strlen(message), message,
                   nelc_c( Idstr ), Idstr.a );
}


static double GAMMLN( double XX )
/*-------------------------------------------------------*/
/* Returns the value LN[gamma(xx)] for xx>0              */
/*-------------------------------------------------------*/
{
   double COF[6] = { 76.18009173,
                    -86.50532033,
                     24.01409822,
                    -1.231739516,
                     0.120858003e-2,
                    -0.536382e-5 };

   double STP = 2.50662827465;
   double HALF = 0.5, ONE = 1.0 , FPF = 5.5;
   double X, TMP, SER;
   int j;


   X = XX-ONE;
   TMP = X + FPF;
   TMP = ( X + HALF ) * log(TMP) - TMP;
   SER = ONE;
   for (j = 0; j < 6; j++) {
      X = X + ONE;
      SER = SER + COF[j] / X;
   }
   return ( TMP + log( STP * SER ) );
}


static double probability( float Rlin, fint N )
/*----------------------------------------------------------------------*/
/* Calculate the probability Pc(r,N) of exceeding r in a random         */
/* sample of observations taken from an uncorrelated parent             */
/* population (rho=0).                                                  */
/* The statistic t = r * sqrt( n-2 / 1-r**2 ) is distributed in the     */
/* case of the null-hypothesis (of no correlation) like Student's t-    */
/* distribution with v = n-2 degrees of freedom. The two sided signi-   */
/* ficance level is given by 1 - A(t|v). For large n it is not          */
/* neccessary to assume a binormal distribution for the variables x, y  */
/* in the sample. r is the linear correlation coefficient for pairs of  */
/* quantities (Xi, Yi). The value of r lies between -1 and 1 inclusive. */
/* The function returns the significance level.                         */
/* If x and y are uncorrelated and we can assume that r is distributed  */
/* normally, this function returns the significance of the correlation, */
/* i.e. the probability that |r| should be larger than its observed     */
/* value in the null hypothesis. The significance level alpha is        */
/* classified as:                                                       */
/*                                                                      */
/* 5%   or less ( ttest < 0.05  )        ... significant                */
/* 1%   or less ( ttest < 0.01  )        ... highly significant         */
/* 0.1% or less ( ttest < 0.001 )        ... very highly significant    */
/*----------------------------------------------------------------------*/
{
   double freedom, free;
   double r, R2;
   int    i, imax;
   double term;
   double fi, fnum, sum, denom, pcorre;
   int    uneven;


   /*-------------------------------------------------------------------------*/
   /* Algorithm: Bevington, Philip R., 1969, Data Reduction and Error         */
   /*            Analysis for the Physical Sciences (New York: McGraw-Hill),  */
   /*            Chapter 6.                                                   */
   /*-------------------------------------------------------------------------*/

   r = (double) Rlin;
   R2 = r * r;
   freedom = (double) N - 2.0;
   if (freedom  < 0.0) return 0.0;
   if ((1.0 - R2) <= 0.0) return 0.0;
   uneven = fmod( freedom, 2.0 );
   if (!uneven) {
      imax = (int) (freedom -2.0) / 2.0;
      free = freedom;
      term = fabs( r );
      sum = term;
      if (imax < 0) return 0.0;
      if (imax == 0) return (float) (1.0 - term);
      for (i = 1; i <= imax; i++) {
         fi = (double) i;
         fnum = (double) (imax - i + 1.0);
         denom = 2.0 * i + 1;
         term = -term * R2 * fnum/fi;
         sum = sum + term/denom;
      }

      pcorre = 1.128379167 * exp( GAMMLN((free+1.0)/2.0)) /
                             exp( GAMMLN( free / 2.0 ));
      pcorre = 1.0 - pcorre * sum;
   } else {
      imax = (int) (freedom - 3) / 2;
      term = fabs( r ) * sqrt( 1.0 - R2 );
      sum = atan( R2/term );
      if (imax < 0) {
         return (float) (1.0 - 0.6366197724*sum);
      }
      if (imax == 0) {
         sum += term;
         return (float) (1.0 - 0.6366197724*sum);
      }
      sum  += term;
      for (i = 1; i <= imax; i++) {
         fnum  = 2.0 * (double) i;
         denom = 2.0 * (double) i + 1.0;
         term = term * (1.0-R2) * fnum/denom;
         sum = sum + term;
      }
      pcorre = 1.0 - 0.6366197724*sum;
   }
   if (pcorre < 0.0) pcorre = 0.0;
   return (pcorre);
}


static fint sums( float *x, float *y, fint ndata,
                  float *fsumX, float *fsumY, float *fsumXX,
                  float *fsumXY, float *fsumYY, bool *first )
/*--------------------------------------------------------*/
/* Determine different sum, used in the linear regression */
/* Returned is the number of valid data pairs, i.e.       */
/* x nor y is a blank.                                    */
/*--------------------------------------------------------*/
{
   static double  sumX, sumY, sumXX, sumYY, sumXY;
   static fint    ntot;
   double         X, Y;
   int            i;


   if (*first) {
      ntot = 0;
      sumX = sumY = 0.0;
      sumXX = sumYY = sumXY = 0.0;
   }
   for (i = 0; i < ndata; i++) {
      if ( (x[i] != blank) && (y[i] != blank) ) {
         ntot += 1;
         X = (double) x[i];
         Y = (double) y[i];
         sumX += X;
         sumY += Y;
         sumXX += X * X;
         sumXY += X * Y;
         sumYY += Y * Y;
      }
   }
   *fsumX  = (float) sumX;
   *fsumY  = (float) sumY;
   *fsumXX = (float) sumXX;
   *fsumXY = (float) sumXY;
   *fsumYY = (float) sumYY;
   *first  = 0;
   return ntot;
}


static fint linreg( fint ndata, float *fsumX, float *fsumY, float *fsumXX,
                    float *fsumXY, float *fsumYY, float *m, float *b,
                    float *corrcoeff, float *sigM, float *sigB,
                    float *chi2 )
/*-----------------------------------------------------------------------*/
/* INPUT:   fsumX etc, ndata                                             */
/* OUTPUT:  m, b, corrcoeff, sigM, sigB, corrcoeff, chi2                 */
/* PURPOSE: Input is sumX/Y/XX/XY/YY, and ndata, the number of data-     */
/*          points. In y = mx + b the parameters m and b are determined  */
/*          with a method called linear regression. Also a correlation-  */
/*          coefficient is determined. If there is not enough data, the  */
/*          values m = 0 and b = 0 are returned. The function itself     */
/*          then returns 0;                                              */
/*          The independent quantities are stored in x, the dependent    */
/*          data points are stored in y. sigM and sigB are the           */
/*          uncertainties in m and b (Bev. 6.21, 6.22).                  */
/*          In this routine there is no weighting ==>                    */
/*          chi2 = (ndata-2) * variance                                  */
/*          Bevington, Philip R., 1969, Data Reduction and Error         */
/*          Analysis for the Physical Sciences (New York: McGraw-Hill),  */
/*          Chapter 6.                                                   */
/*-----------------------------------------------------------------------*/
{
   double  sumX, sumY, sumXX, sumYY, sumXY;
   double  N;
   double  freedom;
   double  delta;
   double  variance;
   double  A, B, sigmA, sigmB;                              /* Y = A + BX !!! */
   double  Rlin;                            /* Linear correlation coefficient */


   N = (double) ndata;
   sumX  = (double) *fsumX;
   sumY  = (double) *fsumY;
   sumXX = (double) *fsumXX;
   sumXY = (double) *fsumXY;
   sumYY = (double) *fsumYY;

   if (ndata < 3) {
      anyoutC( 1, "Not enough data pairs" );
      *m = *b = 0.0;
      *sigM = *sigB = 0.0;
      return(0);
   }

   delta      = N*sumXX - sumX*sumX;
   A          = (sumXX*sumY - sumX*sumXY) / delta;
   B          = (sumXY*N    - sumX*sumY ) / delta;
   freedom    = N - 2.0;
   variance   = (sumYY + A*A*ndata + B*B*sumXX -
                 2.0*(A*sumY+B*sumXY - A*B*sumX)) / freedom;
   sigmA      = sqrt( variance*sumXX / delta );
   sigmB      = sqrt( variance*N     / delta );
   Rlin       = (N*sumXY - sumX*sumY) / sqrt(delta*(N*sumYY - sumY*sumY));
   *m         = (float) B;
   *b         = (float) A;
   *sigM      = (float) sigmB;
   *sigB      = (float) sigmA;
   *corrcoeff = (float) Rlin;
   *chi2      = (float) freedom * variance;
   return(1);
}


void dostat( float *Xarray, float *Yarray, 
             fint   size, 
             float  Xmin, float  Ymin, float Xmax, float Ymax,
             bool   save_results, 
             int    device, 
             FILE  *asciifile )
/*--------------------------------------------------------------*/
/* Do the regression and display results.                       */
/*--------------------------------------------------------------*/
{
   float    sumX, sumY, sumXX, sumXY, sumYY;
   float    M1, B1, sigM1, sigB1;
   float    M2, B2, sigM2, sigB2;
   float    Mav, sigMav;
   float    Rlin, chi2;
   float    xl, yl;
   double   prob;
   bool     first = YES;
   fint     n1;
   fint     r1, r2;
   float    fjust;
   float    angle;
   float    Xpl, Ypl;
   float    newheight, oldheight;
   float    deltaY;


   n1 = sums( Xarray, Yarray, size,
              &sumX, &sumY, &sumXX, &sumXY,&sumYY, &first );

   /*-----------------*/
   /* Display results */
   /*-----------------*/
   Xpl    = Xmin;
   deltaY = fabs(Ymax-Ymin) / 20.0;  /* Difference in height between two text lines */
   Ypl    = Ymin - 7.0 * deltaY;     /* Below here you can start writing the text */
   angle  = 0.0;
   fjust  = 0.0;
   pgqch_c( &oldheight );
   newheight = oldheight/1.6;
   pgsch_c( &newheight );

   /*---------------------------------*/
   /* Print regression header and the */
   /* number of objects on screen     */
   /*---------------------------------*/
   display( device, asciifile, " " );
   display( device, asciifile, "CORRELATION AND REGRESSION" );
   display( device, asciifile, "==========================" );
   (void) sprintf( message, "Data points diagram  : n = %d", n1 );
   display( device, asciifile, message );

   /*-----------------------------------------*/
   /* Print on plot the number of objects and */
   /* the range of the data values            */
   /*-----------------------------------------*/
   if (clip[1] == FLT_MAX) {
      (void) sprintf( message, "Objects in diagram  : n = %d      Range of data values: [%g, ->]", n1, clip[0] );
   } else {
      (void) sprintf( message, "Objects in diagram  : n = %d      Range of data values: [%g, %g]", n1, clip[0], clip[1] );
   }
   pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

   /*-----------------------------------*/
   /* Print on plot the allowed bridges */
   /* and the range of allowed sizes    */
   /*-----------------------------------*/
   if ( !check_bridge ) {
      if (objsize[1] == 1) {
         (void) sprintf( message, "All bridges allowed                  Range of sizes: [%d, ->]", objsize[0] );
      } else {
         (void) sprintf( message, "All bridges allowed                  Range of sizes: [%d, %d]", objsize[0], objsize[1] );
      }
   } else {
      if (objsize[1] == 1) {
         (void) sprintf( message, "Bridge = [%d %d]                       Range of sizes: [%d, ->]", bridge[0], bridge[1], objsize[0] );
      } else {
         (void) sprintf( message, "Bridge = [%d %d]                       Range of sizes: [%d, %d]", bridge[0], bridge[1], objsize[0], objsize[1] );
      }
   }
   Ypl -= deltaY;
   pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

   /*---------------------------------------------------*/
   /* Dummy to avoid crash on HP's with optimized code: */
   /*---------------------------------------------------*/
   (void) sprintf( message,"xpl=%f Ypl=%f ang=%f just=%f", Xpl,Ypl,angle,fjust);

   /*---------------------------------------------------------------------*/
   /* Do the lineair regression Y on X and display the corresponding line */
   /*---------------------------------------------------------------------*/
   r1 = linreg( n1, &sumX, &sumY, &sumXX, &sumXY,&sumYY,
               &M1, &B1, &Rlin, &sigM1, &sigB1, &chi2 );
   if (r1) {
      (void) sprintf( message, "(1) Regression Y on X: Y = %+f (+/-%f) X %+f (+/-%f)",
                      M1, fabs(sigM1), B1, fabs(sigB1) );
      display( device, asciifile, message );

      /*----------------*/
      /* Draw this line */
      /*----------------*/
      xl = Xmin;
      yl = M1*Xmin + B1;
      if (yl > Ymax) {
         yl = Ymax;
         xl = (yl - B1) / M1;
      }
      if (yl < Ymin) {
         yl = Ymin;
         xl = (yl - B1) / M1;
      }
      pgmove_c( &xl, &yl );
      xl = Xmax;
      yl = M1*Xmax + B1;
      if (yl > Ymax) {
         yl = Ymax;
         xl = (yl - B1) / M1;
      }
      if (yl < Ymin) {
         yl = Ymin;
         xl = (yl - B1) / M1;
      }
      pgdraw_c( &xl, &yl );
      pgptxt_c( &xl, &yl, &angle, &fjust, tofchar("(1)") );
      Ypl -= deltaY;
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );
   }

   /*---------------------------------------------------------------------*/
   /* Do the lineair regression X on Y and display the corresponding line */
   /*---------------------------------------------------------------------*/
   r2 = linreg( n1, &sumY, &sumX, &sumYY, &sumXY, &sumXX,
                &M2, &B2, &Rlin, &sigM2, &sigB2, &chi2 );
   if (r2) {
      (void) sprintf( message, "(2) Regression X on Y: X = %+f (+/-%f) Y %+f (+/-%f)",
                      M2, fabs(sigM2), B2, fabs(sigB2) );
      display( device, asciifile, message );
      /*----------------*/
      /* Draw this line */
      /*----------------*/
      yl = Ymin;
      xl = M2 * Ymin + B2;
      if (xl > Xmax) {
        xl = Xmax;
        yl = (xl - B2) / M2;
      }
      if (xl < Xmin) {
         xl = Xmin;
         yl = (xl - B2) / M2;
      }
      pgmove_c( &xl, &yl );
      yl = Ymax;
      xl = M2 * Ymax + B2;
      if (xl > Xmax) {
        xl = Xmax;
        yl = (xl - B2) / M2;
      }
      if (xl < Xmin) {
         xl = Xmin;
         yl = (xl - B2) / M2;
      }
      pgdraw_c( &xl, &yl );
      pgptxt_c( &xl, &yl, &angle, &fjust, tofchar("(2)") );
      Ypl -= deltaY;
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );
   }

   /*----------------------------------------*/
   /* If both regressions are successful then*/
   /* print the results of the regression    */
   /*----------------------------------------*/
   if (r1 && r2) {
      /*-------------------------------------------------*/
      /* Print the linear corr.coeff. on screen and plot */
      /*-------------------------------------------------*/
      (void) sprintf( message, "Linear corr. coeff.  : r = %f", Rlin );
      display( device, asciifile, message );
      Ypl -= deltaY;
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

      /*----------------------------------------------------------*/
      /* Calculate and print the average slope on screen and plot */
      /*----------------------------------------------------------*/
      Mav = ( M1 + (1.0/M2) ) / 2.0;
      sigMav = 0.5 * sqrt( sigM1*sigM1 + pow( (sigM2/(M2*M2)), 2.0 ) );
      (void) sprintf( message, "Average slope        : M = %f (+/-%f)",
                      Mav, sigMav );
      display( device, asciifile, message );
      Ypl -= deltaY;
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

      /*------------------------------------------------------*/
      /* Calculate and print the probability that the parent  */
      /* distribution is uncorrelated. If this probability is */
      /* too small, then give his upper limit.                */
      /*------------------------------------------------------*/
      prob = 100.0 * probability( Rlin, r1);

      /*-----------------*/
      /* print on screen */
      /*-----------------*/
      display( device, asciifile, "Probability that     " );
      display( device, asciifile, "Parent Distribution  " );
      if ( prob > 1.0e-18 ) {
         (void) sprintf( message, "is uncorrelated      : Pc(r,n) = %e(%)", prob );
      } else {
         (void) sprintf( message, "is uncorrelated      : Pc(r,n) < 1.0e-18");
      }
      if (prob >= 5.0) strcat( message, " ( Not significant )" );
      else if (prob <= 0.1) strcat( message, " ( Very highly significant )" );
      else if (prob <= 1.0) strcat( message, " ( Highly significant )" );
      else if (prob <= 5.0) strcat( message, " ( Significant )" );

      /*---------------*/
      /* print on plot */
      /*---------------*/
      display( device, asciifile, message );
      if ( prob > 1.0e-18 ) {
         (void) sprintf( message, "P uncorrelated parent distribution: Pc(r,n) = %e(%)", prob );
      } else {
         (void) sprintf( message, "P uncorrelated parent distribution: Pc(r,n) < 1.0e-18");
      }
      Ypl -= deltaY;
      pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );
   }

   /*---------------*/
   /* Print user ID */
   /*---------------*/
   getstamp( message );
   Ypl -= deltaY;
   pgptxt_c( &Xpl, &Ypl, &angle, &fjust, tofchar(message) );

}


static void updatevolume( int connectionindex, int newindex )
/*------------------------------------------------------*/
/* This function is invoked in the recursive search for */
/* isolated volumes by using connections between the    */
/* objects found earlier in the 2-dimensional analysis. */
/* After each recursive call, the information about the */
/* current volume should be stored. Furthermore, the    */
/* corresponding connection and the 2d-objects involved */
/* should not be investigated anymore.                  */
/*------------------------------------------------------*/
{
   int   objindcur, objindprv;
   float dummymax;

   objindcur = connection[connectionindex].current;  /* the number of the current object   */
   objindprv = connection[connectionindex].previous; /* the number of the connected object */
   connections++;                                    /* the number of connections should be increased */

   /*--------------------------------------------*/
   /* determine the overlap to size ratio of the */
   /* smallest of the two overlapping objects    */
   /*--------------------------------------------*/
   overlapfactor += connection[connectionindex].overlapfactor;

   /*-----------------------------------------*/
   /* if the object is not yet include in the */
   /* volume- and flux update, then do so     */
   /*-----------------------------------------*/
   if (object[objindcur].free) {
       volume1 += object[objindcur].pixels;
       flux1   += object[objindcur].flux;
   }
   if (object[objindprv].free) {
      volume1 += object[objindprv].pixels;
      flux1   += object[objindprv].flux;
   }

   /*--------------------------------*/
   /* Determine max. value in volume */
   /*--------------------------------*/
   dummymax = MYMAX( object[objindcur].maxval, object[objindprv].maxval );
   maxval1  = MYMAX( maxval1, dummymax );

   /*--------------------------------------------*/
   /* Determine the min. and max. index number   */
   /* of the connections belonging to the volume */
   /*--------------------------------------------*/
   if (connection[connectionindex].subset > connection[maxconnect].subset) {
      maxconnect = connectionindex;
   }
   if (connection[connectionindex].subset < connection[minconnect].subset) {
      minconnect = connectionindex;
   }

   /*-------------------------*/
   /* Determine the min. box  */
   /* around the volume       */
   /*-------------------------*/
   if (object[objindcur].framebox[0][0] < framebox[0][0]) framebox[0][0] = object[objindcur].framebox[0][0];
   if (object[objindcur].framebox[0][1] < framebox[0][1]) framebox[0][1] = object[objindcur].framebox[0][1];
   if (object[objindcur].framebox[1][0] > framebox[1][0]) framebox[1][0] = object[objindcur].framebox[1][0];
   if (object[objindcur].framebox[1][1] > framebox[1][1]) framebox[1][1] = object[objindcur].framebox[1][1];
   if (object[objindprv].framebox[0][0] < framebox[0][0]) framebox[0][0] = object[objindprv].framebox[0][0];
   if (object[objindprv].framebox[0][1] < framebox[0][1]) framebox[0][1] = object[objindprv].framebox[0][1];
   if (object[objindprv].framebox[1][0] > framebox[1][0]) framebox[1][0] = object[objindprv].framebox[1][0];
   if (object[objindprv].framebox[1][1] > framebox[1][1]) framebox[1][1] = object[objindprv].framebox[1][1];

   /*--------------------------------*/
   /* Give the objects of the volume */
   /* the index of the volume        */
   /*--------------------------------*/
   object[objindcur].index = newindex;
   object[objindprv].index = newindex;

   /*----------------------------------*/
   /* Make sure connection and objects */
   /* are not investigated anymore     */
   /*----------------------------------*/
   object[objindcur].free = NO;
   object[objindprv].free = NO;
   connection[connectionindex].free = NO;
}


static void subsetrange( int subset, int *start, int *end )
/*--------------------------------------------------------------*/
/* Minimize the search for connections in the connection array  */
/* by finding two array indices as start and end. These indices */
/* are stored in the array 'subsetend' which is updated in the  */
/* 'volume' loop. The routine is called in 'findnext'.          */
/*--------------------------------------------------------------*/
{
   if ((subset < 1) || (subset > (nsubs-1))) {
      *start = 0;
      *end = 0;
   } else {
      *end = subsetend[subset];
      *start = subsetend[subset-1];
   }
}


static void findnext( int connectionnumber, int newindex )
/*--------------------------------------------------------------*/
/* Recursive search in the arrays connection[].current and      */
/* connection[].previous for a connection. The arrays are       */
/* examined in a certain range were connections can be expected */
/* (call to 'subsetrange'). If numbers match, (in current and   */
/* previous plane, update volume etc.                           */
/* Example:                                                     */
/* connection subset | connection current | connection previous */
/*         1                    4                    1          */
/*         1                    5                    2          */
/*         1                    5                    3          */
/*         2                    6                    4          */
/*         2                    6                    5          */
/*         2                    7                    5          */
/*         3                    9                    7          */
/* Results in the sum of pixels of object: 4,6,5,2,3,7,9,1      */
/*--------------------------------------------------------------*/
{
   int  i = connectionnumber;
   int  subset;
   int  currentcolor;
   int  j;
   int  start, end;


   updatevolume(i, newindex);

   currentcolor = connection[i].current;
   subset = connection[i].subset;
   subsetrange( subset, &start, &end );
   for (j = start; ((j <= end) && (connection[j].current <= currentcolor)); j++) {
      if (connection[j].free) {
         if (connection[j].current == currentcolor) {   /* found another connection */
            findnext( j, newindex );
         }
      }
   }
   subsetrange( subset+1, &start, &end );
   for (j = start; j <= end; j++) {
      if (connection[j].free) {
         if (connection[j].previous == currentcolor) {   /* found another connection */
            findnext( j, newindex );
         }
      }
   }
   currentcolor = connection[i].previous;
   subsetrange( subset, &start, &end );
   for (j = start; j <= end; j++) {
      if (connection[j].free) {
         if (connection[j].previous == currentcolor) {   /* found another connection */
            findnext( j, newindex );
         }
      }
   }
   subsetrange( subset-1, &start, &end );
   for (j = start; ((j <= end) && (connection[j].current <= currentcolor)); j++) {
      if (connection[j].free) {
         if (connection[j].current == currentcolor) {   /* found another connection */
            findnext( j, newindex );
         }
      }
   }
}



static void object_statistics( isolatedobject  *object, 
                               int              numobjects, 
                               int              device, 
                               FILE            *asciifile )
/*--------------------------------------------------------------*/
/* PURPOSE: Do some statistics over all the objects.            */
/*--------------------------------------------------------------*/
{
   int     i;
   float   av_Xperimeter = 0.0;
   float   dev_Xperimeter = 0.0;
   float   av_Yperimeter = 0.0;
   float   dev_Yperimeter = 0.0;
   float   av_perimeterunits = 0.0;
   float   dev_perimeterunits = 0.0;
   float   av_flux = 0.0;
   float   dev_flux = 0.0;
   float   av_fluxperpixel = 0.0;
   float   dev_fluxperpixel = 0.0;
   float   av_areapixels = 0.0;
   float   dev_areapixels = 0.0;
   float   av_areaunits = 0.0;
   float   dev_areaunits = 0.0;
   float   av_maxval = 0.0;
   float   dev_maxval = 0.0;
   float   N = (float) numobjects;
   float   M = N - 1.0;
  


   display( device, asciifile, " " );
   sprintf( message, "AVERAGES AND STANDARD DEVIATIONS USING %d OBJECTS", numobjects );
   display( device, asciifile, message );
   sprintf( message, "===================================================" );
   display( device, asciifile, message ); 
   if (numobjects < 2)
   {
      anyoutC( 3, "Not enough objects to do statistics over totals!" );
      return;
   }
   for (i = 0; i < numobjects; i++)
   {
      av_Xperimeter     += object[i].Xperimeter;
      av_Yperimeter     += object[i].Yperimeter;
      av_perimeterunits += object[i].Xperimeter * cdelt[0] + object[i].Yperimeter * cdelt[1];
      av_areapixels     += (float) object[i].pixels;
      av_areaunits      += ((float) object[i].pixels) * ((float)(cdelt[0]*cdelt[1]));
      av_flux           += object[i].flux;
      av_fluxperpixel   += object[i].flux/(float)object[i].pixels;
      av_maxval         += object[i].maxval;
   }
   av_Xperimeter     /= N;
   av_Yperimeter     /= N;
   av_perimeterunits /= N;
   av_areapixels     /= N;
   av_areaunits      /= N;
   av_flux           /= N;
   av_fluxperpixel   /= N;
   av_maxval         /= N;
   
   for (i = 0; i < numobjects; i++)
   {
      float  xx;
   
      xx = object[i].Xperimeter - av_Xperimeter;
      dev_Xperimeter     +=  xx * xx;
      xx = object[i].Yperimeter - av_Xperimeter;
      dev_Yperimeter     +=  xx * xx;
      xx = (object[i].Xperimeter * cdelt[0] + object[i].Yperimeter * cdelt[1]) - av_perimeterunits;
      dev_perimeterunits +=  xx * xx;
      xx = ((float) object[i].pixels) - av_areapixels;
      dev_areapixels     +=  xx * xx;
      xx = (((float) object[i].pixels) * ((float)(cdelt[0]*cdelt[1]))) - av_areaunits;
      dev_areaunits      +=  xx * xx;
      xx = object[i].flux - av_flux;
      dev_flux           +=  xx * xx;
      xx = (object[i].flux/(float)object[i].pixels) - av_fluxperpixel;
      dev_fluxperpixel   +=  xx * xx;
      xx = object[i].maxval - av_maxval;
      dev_maxval         +=  xx * xx;
   }
   dev_Xperimeter     = sqrt(  dev_Xperimeter / M  );
   dev_Yperimeter     = sqrt(  dev_Yperimeter / M  );
   dev_perimeterunits = sqrt(  dev_perimeterunits / M  );
   dev_areapixels     = sqrt(  dev_areapixels / M  );
   dev_areaunits      = sqrt(  dev_areaunits / M  );
   dev_flux           = sqrt(  dev_flux / M  );
   dev_fluxperpixel   = sqrt(  dev_fluxperpixel / M  );
   dev_maxval         = sqrt(  dev_maxval / M  );

   sprintf( message, "Average perimeter in %.*s = %f +- %f", nelc_c(Cunit[0]), Cunit[0].a, av_perimeterunits, dev_perimeterunits );   
   display( device, asciifile, message );
   sprintf( message, "Average perimeter in X in pixels = %f +- %f", av_Xperimeter, dev_Xperimeter );
   display( device, asciifile, message );
   sprintf( message, "Average perimeter in Y in pixels = %f +- %f", av_Yperimeter, dev_Yperimeter );
   display( device, asciifile, message );
   sprintf( message, "Average area in %.*s x %.*s  = %f +- %f", nelc_c(Cunit[0]), Cunit[0].a,
            nelc_c(Cunit[1]), Cunit[1].a, av_areaunits, dev_areaunits );
   display( device, asciifile, message );
   sprintf( message, "Average area in pixels = %f +- %f", av_areapixels, dev_areapixels );
   display( device, asciifile, message );
   sprintf( message, "Average flux in map units = %f +- %f", av_flux, dev_flux );
   display( device, asciifile, message );
   sprintf( message, "Average flux per pixel in map units = %f +- %f", av_fluxperpixel, dev_fluxperpixel );
   display( device, asciifile, message ); 
   sprintf( message, "Average maximum value in map units = %f +- %f", av_maxval, dev_maxval );
   display( device, asciifile, message );         
   display( device, asciifile, " " );
}




static void volume_statistics( isolatedvolume  *object3d,
                               int              numvolumes,
                               int              device,
                               FILE            *asciifile )
/*--------------------------------------------------------------*/
/* PURPOSE: Do some statistics over all the VOLUMES.            */
/*--------------------------------------------------------------*/
{
   int     i;
   float   av_flux = 0.0;
   float   dev_flux = 0.0;
   float   av_fluxperpixel = 0.0;
   float   dev_fluxperpixel = 0.0;
   float   av_volume = 0.0;
   float   dev_volume = 0.0;
   float   av_depth = 0.0;
   float   dev_depth = 0.0;
   float   av_maxval = 0.0;
   float   dev_maxval = 0.0;
   float   N, M;
   int     n = 0;
  


   display( device, asciifile, " " );
   sprintf( message, "AVERAGES AND STANDARD DEVIATIONS USING %d VOLUMES", numvolumes );
   display( device, asciifile, message );
   sprintf( message, "===================================================" );
   display( device, asciifile, message ); 
   if (numvolumes < 2)
   {
      anyoutC( 3, "Not enough objects to do statistics over totals!" );
      return;
   }
   for (i = 0; i < numvolumes; i++)
   {
      if (object3d[i].index >= 0)     /* Only the selected volumes */
      {
         av_volume         += object3d[i].volume;
         av_depth          += (float) (object3d[i].hisub - object3d[i].losub + 1);
         av_flux           += object3d[i].flux;
         av_fluxperpixel   += object3d[i].flux/(float)object3d[i].volume;
         av_maxval         += object3d[i].maxval;
         n++;
      }
   }
   if (n < 2)
   {
      anyoutC( 3, "Not enough objects to do statistics over totals!" );
      return;
   }
   N = (float) n;
   av_volume       /= N;
   av_depth        /= N;
   av_flux         /= N;
   av_fluxperpixel /= N;
   av_maxval       /= N;
   
   for (i = 0; i < numvolumes; i++)
   {
      float  xx;
   
      if (object3d[i].index >= 0)     /* Only the selected volumes */
      {
         xx = object3d[i].volume - av_volume;
         dev_volume       +=  xx * xx;
         xx = (float) (object3d[i].hisub - object3d[i].losub + 1) - av_depth;
         dev_depth        +=  xx * xx;
         xx = object3d[i].flux - av_flux;
         dev_flux         +=  xx * xx;
         xx = (object3d[i].flux/(float)object3d[i].volume) - av_fluxperpixel;
         dev_fluxperpixel +=  xx * xx;
         xx = object3d[i].maxval - av_maxval;
         dev_maxval       +=  xx * xx;         
      }
   }
   
   M = N - 1.0;
   dev_volume       = sqrt(  dev_volume / M  );
   dev_depth        = sqrt(  dev_depth / M  );
   dev_flux         = sqrt(  dev_flux / M  );
   dev_fluxperpixel = sqrt(  dev_fluxperpixel / M  );
   dev_maxval       = sqrt(  dev_maxval / M  );

   sprintf( message, "Average volume in pixels = %f +- %f", av_volume, dev_volume );
   display( device, asciifile, message );
   sprintf( message, "Average depth in pixels = %f +- %f", av_depth, dev_depth );
   display( device, asciifile, message );
   sprintf( message, "Average flux in map units = %f +- %f", av_flux, dev_flux );
   display( device, asciifile, message );
   sprintf( message, "Average flux per pixel in map units = %f +- %f", av_fluxperpixel, dev_fluxperpixel );
   display( device, asciifile, message ); 
   sprintf( message, "Average maximum value in map units = %f +- %f", av_maxval, dev_maxval );
   display( device, asciifile, message );         
   display( device, asciifile, " " );
}





MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   int     x, y;
   float   minarea = 0.0;
   float   maxarea = 0.0;
   float   minperim = 0.0;
   float   maxperim = 0.0;
   fint    numobjects = 0;
   fint    imagenr;
   int     connectcount = 0;
   int     keynum;


   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );
   fmake( Setin, STRLEN );
   fmake( Key, KEYLEN );
   fmake( Mes, STRLEN );
   dfault  = NONE;
   subdim  = 2;                    /* Input must be 2-dim. */
   showdev = 3;
   Key     = KEY_INSET;
   Mes     = MES_INSET;
   nsubs   = gdsinp_c( Setin,      /* Name of input set. */
                       subin,      /* Array containing subsets coordinate words. */
                       &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                       &dfault,    /* Default code as is USERxxx. */
                       Key,        /* Keyword prompt. */
                       Mes,        /* Keyword message for the user. */
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
   setdim  = gdsc_ndims_c( Setin, &setlevel );

   /*--------------------------------------*/
   /* Fill an array with real subset grids */
   /*--------------------------------------*/
   for (subnr = 0; subnr < (int) nsubs; subnr++) {
      if (setdim <= 2) {
         subsetgrid[subnr] = subnr;
      } else {
         /* Store subset grid of third axis only! */
         r1 = 0;
         subsetgrid[subnr] = gdsc_grid_c( Setin, &axnum[2], &subin[subnr], &r1 );
      }
   }


   /*-------------------------------*/
   /* Determine edges of this frame */
   /*-------------------------------*/
   {
      fint cwlo, cwhi;                          /* Local coordinate words */
      int  m;
      r1 = 0;
      gdsc_range_c( Setin, &setlevel, &cwlo, &cwhi, &r1 );
      r1 = r2 = 0;
      for (m = 0; m < (int) setdim; m++) {
         flo[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
         fhi[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
      }
   }

   /*------------------------*/
   /* Determine gridspacings */
   /*------------------------*/
   for (i = 0; i < 2; i++) {
      /* finit( Cunit[i], 40 );*/
      Cunit[i].a = cu[0]; Cunit[i].l = 20;
      (void) sprintf( message, "CUNIT%d", axnum[i] );
      r1 = 0;
      gdsd_rchar_c( Setin, tofchar(message), &setlevel, Cunit[i], &r1 );
      if (r1 < 0) {
            strcpy( Cunit[i].a, "?" );
      }
      (void) sprintf( message, "CDELT%d", axnum[i] );
      r1 = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &cdelt[i], &r1 );
      if (r1 < 0) {
         cdelt[i] = 1.0; /* Then in pixels */
      }
      cdelt[i] = fabs(cdelt[i]);
   }


   /*-------------------------------*/
   /* Prepare a box for INSET       */
   /*-------------------------------*/
   boxopt  = 0;
   showdev = 3;
   dfault  = REQUEST;
   Key     = KEY_BOX;
   Mes     = MES_BOX;
   gdsbox_c( blo, bhi, Setin, subin, &dfault,
             Key, Mes, &showdev, &boxopt );

   /*---------------------------------*/
   /* Check if box is defined by user */
   /*---------------------------------*/
   for (i = 0; i < 2 ; i++) {
       if (flo[i] < blo[i]) box_defined = YES;
       if (fhi[i] > bhi[i]) box_defined = YES;
   }
   if (box_defined) anyoutC(1 , "Be sure your output set is empty around the defined box !!!" );



   r1 = 0;
   fmake( Headerobjname, KEYLEN );
   Headerobjname.l = KEYLEN;   
   Headerobjname.a = headerobjname;
   gdsd_rchar_c( Setin, tofchar("OBJECT"), &setlevel, Headerobjname, &r1 );
   if (r1 < 0) {
      anyoutC(16, "Could not find an OBJECTS field in the header" );
      strcpy( headerobjname, "?" );
   }
   else {
      /* Take care of trailing zero for string */
      (void) sprintf( headerobjname, "%.*s", nelc_c( Headerobjname ), Headerobjname.a );
   }
   dfault = HIDDEN;
   r1 = usertext_c( Headerobjname, &dfault, tofchar("OBJNAME="), 
                    tofchar("Enter a name to identify field: [header]") );
                                 
   /*--------------------------------------------------------------*/
   /* Assign 'gdsinp' buffer to 'gdsout'. Output set will get same */
   /* coordinate system as input INSET=.  GDSOUT is a function     */
   /* which prompts the user to enter the name of a set and        */
   /* (optionally) subset(s) and returns the number of subsets     */
   /* entered.                                                     */
   /*--------------------------------------------------------------*/

   gdsasn_c( KEY_INSET, KEY_OUTSET, &class );
   dfault  = NONE;
   showdev = 3;
   Key     = KEY_OUTSET;
   Mes     = MES_OUTSET;
   fmake( Setout, STRLEN );
   do {
      nsubsout = gdsout_c( Setout,        /* Name of the output set. */
                           subout,        /* Output array with subsets coordinate words.*/
                           &nsubs,        /* Maximum number of subsets in subout. */
                           &dfault,       /* Default code as in USERxxx. */
                           Key,           /* User keyword prompt. */
                           Mes,           /* Message for the user. */
                           &showdev,      /* Device number (as in ANYOUT). */
                           axnumout,      /* Array of size 'maxaxes' containing the axes numbers. */
                           axcountout,    /* Array with the axis sizes. */
                           &maxaxes );    /* Max axes the program can deal with. */
      agreed = (nsubsout == nsubs);
      if (!agreed) reject_c( KEY_OUTSET, tofchar("#out != #in") );
   } while (!agreed);

   writefile[0] = '\0';     /* <CR> in 'openfile' results in asciifile=NULL */
   dfault       = REQUEST;
   asciifile    = openfile( tofchar("FILENAME="),                  /* Keyword */
                            tofchar("Name of ASCII file:                  [No output to file]"),  /* Message */
                            dfault,                                /* default */
                            writefile,                             /* name of file */
                           'w' );                                  /* write mode */

   /*-------------------------------------------------------------------*/
   /* The user has to define the keywords of the 2-dimensional analyses */
   /*-------------------------------------------------------------------*/

   nitems  = 2;
   dfault  = NONE;
   clip[1] = FLT_MAX;
   Key     = tofchar("RANGE=");
   Mes     = tofchar("Give range of levels:         ... , [->]");
   r1      = userreal_c( clip, &nitems, &dfault, Key, Mes);


   nitems  = 2;
   dfault  = REQUEST;
   maxcut  = FLT_MIN;
   Key     = tofchar("MAXCUT=");
   Mes     = tofchar("Enter a cutoff for the max. in an object:  [none]" );
   r1      = userreal_c( &maxcut, &nitems, &dfault, Key, Mes);

   do {
      nitems     = 2;
      dfault     = REQUEST;
      objsize[0] = 10;
      objsize[1] = 1;
      Key        = tofchar("SIZE=");
      Mes        = tofchar("Give range of object sizes (number of pixels):  [10,->]");
      r1         = userint_c( objsize, &nitems, &dfault, Key, Mes );

      /*----------------------*/
      /* Check on wrong input */
      /*----------------------*/
      if ( (objsize[0] < 1) || (objsize[1] < 1) ) {
      	anyoutC(1, "object should be at least one pixel in size");
        cancel_c( Key );
      } else {
        if ( (objsize[0]-objsize[1]) == 1 ) {
           anyoutC(1, "you won't select any objects this way!");
           cancel_c( Key );
        }
      }

   } while ( (objsize[0] < 1) || (objsize[1] < 1) || ( (objsize[0]-objsize[1]) == 1 ) );

   nitems = 2;
   dfault = REQUEST;
   bridge[0] = 1;
   bridge[1] = 1;
   Key    = tofchar("BRIDGE=");
   Mes    = tofchar("Give min. thickness allowed for bridges   [1 1]" );
   r1     = userint_c( bridge, &nitems, &dfault, Key, Mes );

   /*------------------------------------------------------------------*/
   /* If the user responds with bridge=0 0 then the mode should be     */
   /* changed to 8-connectivity.                                       */
   /*------------------------------------------------------------------*/

   if ( (bridge[0] == 0) && (bridge[1] == 0) ) {
      eightconnectivity = YES;
      anyoutC( 1, "Mode changed to 8-connectivity" );
   }

   /*--------------------------------------------------------------*/
   /* If bridge is defined smaller than default there should be no */
   /* checking of bridges.                                         */
   /*--------------------------------------------------------------*/

   if (bridge[0] <= 1) bridge[0] = 1;
   if (bridge[1] <= 1) bridge[1] = 1;
   if ( (bridge[0] == 1) && (bridge[1] == 1) ) check_bridge = NO;


   /*------------------------------------------------------------*/
   /* Start the main loop over all subsets. Calculate for each   */
   /* subset new coordinate words and reset the transfer id's    */
   /*------------------------------------------------------------*/


   lx = (bhi[0] - blo[0] + 1);
   ly = (bhi[1] - blo[1] + 1);
   image = (float *) calloc( (int) (lx*ly), sizeof(float) );
   if (image == NULL) {
      anyoutC( 1, "Could not allocate enough memory for this box" );
      return( 0 );
   }
   imageout = (float *) calloc( (int) (lx*ly), sizeof(float) );
   if (imageout == NULL) {
      anyoutC( 1, "Could not allocate enough memory for this output" );
      return( 0 );
   }
   maxIObuf = lx * ly;

   /*-------------------*/
   /* Initialize stabar */
   /*-------------------*/
   STBstart   = 0.0;
   STBend     = (float) (nsubs * maxIObuf);
   STBcurrent = 0.0;
   stabar_c( &STBstart, &STBend, &STBcurrent );

   /*----------------------------------------*/
   /* Create space for the object structures */
   /*----------------------------------------*/
   object = (isolatedobject *) realloc( (isolatedobject *) object,
                               maxobjects*sizeof(isolatedobject) );
   if (object == NULL ) {
      anyoutC( 1, "Too many objects to fit in memory!" );
      return( 0 );
   }

   /*-------------------------------------------------------------*/
   /* Start the recursive two dimensional search for objects.     */
   /* Store all the information about the objects in a structure. */
   /*-------------------------------------------------------------*/
   for(subnr = 0; subnr < nsubs; subnr++) {

      /*-----------------------------------------------------------*/
      /* Use input grid coordinates, but connect to output subsets */
      /*-----------------------------------------------------------*/
      cwloO  = gdsc_fill_c( Setout, &subout[subnr], blo );
      cwhiO  = gdsc_fill_c( Setout, &subout[subnr], bhi );

      /*-----------------*/
      /* Reset variables */
      /*-----------------*/
      mcount = 0;
      tidO   = 0;
      imagenr = 1;
      pixelsread = fillimage( Setin, subin[subnr], blo, bhi, imagenr );

      /*-----------------------------------*/
      /* Initialize output buffer to blank */
      /*-----------------------------------*/
      for (i = 0; i < pixelsread; i++) imageout[i] = blank;

      /*-------------------------------------------*/
      /* Loop over all the pixels in the subsetmap */
      /*-------------------------------------------*/
      for (y = blo[1]; y <= bhi[1]; y++) {
         for (x = blo[0]; x <= bhi[0]; x++) {
            STBcurrent += 1.0;
            if (inrange(GETIM(x,y))) {                    /* pixel is part of a object */

               /*-----------------------*/
               /* A new object is found */
               /* Reset all information */
               /*-----------------------*/
               area   = 0;
               yperim = 0;
               xperim = 0;
               flux   = 0.0;
               maxvalue = blank;
               bordercount = 0;
               curval = (float) currentobject;            /* New objects gets this color */
               framebox[0][0] = x;
               framebox[0][1] = y;
               framebox[1][0] = x;
               framebox[1][1] = y;

               /*----------------------*/
               /* The recursive search */
               /*----------------------*/
               checkneighbours( x, y);

               /*----------------------------------------*/
               /* Check if object has correct size       */
               /*----------------------------------------*/
               if ( !insize(area) || maxvalue < maxcut ) {
                  int k;
                  int m;
                  float value;
                  for (k = framebox[0][0]; k <= framebox[1][0]; k++) {
                      for (m = framebox[0][1]; m <= framebox[1][1]; m++) {
                          value = GETOUTIM(k,m);
                          if (value != blank) {
                             if (curval == value) {
                                SETOUTIM(k, m, blank );       /* Object is wiped out */
                             }
                          }
                      }
                  }
               } else {
                  /*------------------------------------*/
                  /* The object is selected and stored. */
                  /* If more space needs to be created: */
                  /*------------------------------------*/
                  if (currentobject == maxobjects) {
                     maxobjects += MAXOBJECTS;
                     object = (isolatedobject *) realloc( (isolatedobject *) object,
                                                 maxobjects * sizeof(isolatedobject) );
                     if (object == NULL ) {
                        anyoutC( 1, "Too many objects to fit in memory!" );
                        return( 0 );
                     }
                  }
                  /*------------------------------------*/
                  /* Store the results in a structure   */
                  /*------------------------------------*/
                  object[currentobject].index = currentobject;
                  object[currentobject].subset = subnr;
                  object[currentobject].Xpos = x;
                  object[currentobject].Ypos = y;
                  object[currentobject].area = (float) (cdelt[0] * cdelt[1]) * (float) area;
                  object[currentobject].pixels = area;
                  object[currentobject].Xperimeter = (float) xperim;
                  object[currentobject].Yperimeter = (float) yperim;
                  object[currentobject].flux   = flux;
                  object[currentobject].maxval = maxvalue;
                  object[currentobject].bordercount = bordercount;
                  object[currentobject].free = YES ;
                  object[currentobject].framebox[0][0] = framebox[0][0];
                  object[currentobject].framebox[0][1] = framebox[0][1];
                  object[currentobject].framebox[1][0] = framebox[1][0];
                  object[currentobject].framebox[1][1] = framebox[1][1];
                  currentobject++;
               }
            }
         }
         numobjects = currentobject;

         stabar_c( &STBstart, &STBend, &STBcurrent );
      }

      /*-----------------------------------------------*/
      /* Calculate running min, max & blanks of output */
      /*-----------------------------------------------*/
      minmax3_c( imageout,
                 &pixelsread,
                 &minval[subnr], &maxval[subnr],
                 &nblanks[subnr],
                 &mcount );
      pixelswrite = pixelsread;

      /*---------------------------------*/
      /* Write results to the output set */
      /*---------------------------------*/
      gdsi_write_c( Setout,
                    &cwloO, &cwhiO,
                    imageout,
                    &maxIObuf,
                    &pixelswrite,
                    &tidO );
   }

   /*-------------------------------------------*/
   /* Update OUTSET= descriptor with new values */
   /*-------------------------------------------*/
   change = YES;
   wminmax_c( Setout, subout,
              minval, maxval, nblanks,
              &nsubsout,
              &change );

   /*----------------------*/
   /* close the output set */
   /*----------------------*/
   r1 = 0;
   gds_close_c( Setout, &r1 );


   /*-------------------------------------------------*/
   /* Create header for the 'screen' or 'ascii' table */
   /* if user responds positively to the following    */
   /* keywords TOSCREEN= and TOPLOT=                  */
   /*-------------------------------------------------*/

   (void) sprintf( message,
                   "%d objects found", numobjects );
   anyoutC( 1, message );

   nitems   = 1;
   dfault   = REQUEST;
   toscreen = toflog( YES );
   Key      = tofchar("TOSCREEN=");
   Mes      = tofchar("Display table on screen?         [Y]/N" );
   r1       = userlog_c( &toscreen, &nitems, &dfault, Key, Mes );
   cancel_c( Key );

   save_results = (toscreen || asciifile);  /* Results can be written to the asciifile only */

   nitems = 1;
   dfault = REQUEST;
   toplot = toflog( YES );
   Key    = tofchar("TOPLOT=");
   Mes    = tofchar("Make Area-Perimeter plot of objects?    [Y]/N" );
   r1     = userlog_c( &toplot, &nitems, &dfault, Key, Mes );
   toplot = tobool( toplot );

   /*---------------------------------------------*/
   /* If the user wants a Area-Perimeter relation */
   /* he should be able to select on borderpixels */
   /*---------------------------------------------*/
   if (toplot) {
      nitems    = 1;
      dfault    = REQUEST;
      maxborder = 2;
      Key       = tofchar("MAXBORDER=");
      Mes       = tofchar("Give max. pixels allowed on border of box:      [2]");
      r1        = userint_c( &maxborder, &nitems, &dfault, Key, Mes );
   }

   device = 3;   /* Make sure that default the results are written to the logfile */

   if (save_results || toplot) {
      int   mlen;                                      /* message length */
      fint  points = 0;                                /* counter for the number of Area-Perimeter points */

      if (!toscreen) device = -1;                      /* write results only to asciifile */

      /*-----------------------------------------*/
      /* Relocate memory for the positions array */
      /* necessary for the Area-Perimeter part   */
      /*-----------------------------------------*/
      Xarray = (float *) calloc( currentobject, sizeof(float) );
      Yarray = (float *) calloc( currentobject, sizeof(float) );
      if ((Xarray == NULL) || (Yarray == NULL)) {
         anyoutC( 1, "Cannot allocate space for plot arrays"  );
         return(0);
      }

      if (save_results) {
      	 /*------------------------------*/
         /* Make the header of the table */
         /*------------------------------*/
         getstamp( message );                             /* Get user id */
         display( device, asciifile, message );           /* write user ID */

         /*------------------------------*/
         /* Write the field id.          */
         /*------------------------------*/
         sprintf( message, "Id. of this field: %s", headerobjname );
         display( device, asciifile, message );

         {
            fchar Insettxt;
            fmake( Insettxt, 512 );
            dfault = HIDDEN;
            (void) usertext_c( Insettxt, &dfault, KEY_INSET, tofchar("") );
            Insettxt.a[nelc_c(Insettxt)] = '\0';
            sprintf( message, "Input set: [%s]", Insettxt.a );
            display( device, asciifile, message );
         }
         /*------------------------------------------*/
         /* Print on device the range of data values */
         /*------------------------------------------*/
         if (clip[1] == FLT_MAX) {
            (void) sprintf( message, "range of data values: [%g, ->]", clip[0] );
         } else {
            (void) sprintf( message, "range of data values: [%g, %g]", clip[0], clip[1] );
         }
         display( device, asciifile, message );

         /*------------------------------------*/
         /* Print on device the range of sizes */
         /*------------------------------------*/
         if (objsize[1] == 1) {
            (void) sprintf( message, "Range of sizes: [%d, ->]", objsize[0] );
         } else {
            (void) sprintf( message, "Range of sizes: [%d, %d]", objsize[0], objsize[1] );
         }
         display( device, asciifile, message );

         /*-------------------------------------*/
         /* Print on device the allowed bridges */
         /*-------------------------------------*/
         if ( !check_bridge ) {
            (void) sprintf( message, "All bridges allowed");
         } else {
            (void) sprintf( message, "Bridge = [%d %d]", bridge[0], bridge[1] );
         }
         display( device, asciifile, message );

         /*------------------------------------*/
         /* Print on device the table settings */
         /*------------------------------------*/
         display( device, asciifile, " " );
         mlen = sprintf( message,
         "INDX|===X|===Y|SUB|===AREA|===XPER|===YPER|===FLUX|FLUX/PIX|====MAX|BORD|" );
         display( device, asciifile, message );
         memset( message, '-', mlen );
         display( device, asciifile, message );
      }

      for (i = 0; i < numobjects; i++) {        /* Loop over all objects */
         float  xp, yp;

         if (save_results) {
            /*-----------------------------------*/
            /* Write object information in table */
            /*-----------------------------------*/
            (void) sprintf( message, "%4d|%4d|%4d|%3d|%7d|%7g|%7g|%7g|%8g|%7g|%4d|",
                            object[i].index + 1,            /* Start index at 1 */
                            object[i].Xpos,
                            object[i].Ypos,
                            subsetgrid[object[i].subset],   /* Translate in real subsetnumbers */
                            object[i].pixels,
                            object[i].Xperimeter,
                            object[i].Yperimeter,
                            object[i].flux,
                            object[i].flux/(float)object[i].pixels,
                            object[i].maxval,
                            object[i].bordercount );
            display( device, asciifile, message );
         }

         if (toplot && (object[i].bordercount <= maxborder) ) {
             /*-------------------------------------------*/
             /* Store results for Area-Perimeter relation */
             /*-------------------------------------------*/
             xp = (float) log10((double) object[i].area);
             yp = (float) log10(  ( cdelt[0] * ((double) object[i].Xperimeter) )
                                + ( cdelt[1] * ((double) object[i].Yperimeter) ) );
             Xarray[points] = xp; Yarray[points] = yp;
             if (points == 0) {
                minarea  = xp; maxarea = xp;
                minperim = xp; maxperim = yp;
             } else {
                if (xp > maxarea)  maxarea = xp;
                if (xp < minarea)  minarea = xp;
                if (yp > maxperim) maxperim = yp;
                if (yp < minperim) minperim = yp;
             }
             points++;
         }
      }
      
      if (save_results) {         
         object_statistics( object, numobjects, device, asciifile );
      }
     

      /*----------------------------------------------------------------------*/
      /* If the user wants a Area-Perimeter relation, his maxborder selection */
      /* has to leave at least one data point for the plot and at least three */
      /* data points for the calculation                                      */
      /*----------------------------------------------------------------------*/
      if (toplot) {
         if (points <= 0) {
            anyoutC( 1, "OBJECTS: There are no points to plot!"  );
            if (numobjects == 0)
               anyoutC( 1, "         because number of objects found was 0." );
            else 
               anyoutC( 1, "         perhaps because too many pixels belong to a border." );
         }
         else {                  
            initplot();
            drawbox( minarea, minperim, maxarea, maxperim );
            pgpt_c( &points, Xarray, Yarray, &symbol );
            /* Do some statistics, (regression, correlation) */
            dostat( Xarray, Yarray, points, minarea, minperim,
                    maxarea, maxperim, save_results, device, asciifile );
         }
      }
   }

   device = 3;   /* Make sure that default the results are written to the logfile */

   /*----------------------------------------------------------------------*/
   /* If the input contained more subsets, it must be possible to assemble */
   /* connected clouds in subset direction                                 */
   /*----------------------------------------------------------------------*/
   volumes = NO;
   if (nsubs > 1) {
      volumes = toflog(YES);
      nitems  = 1;
      dfault  = REQUEST;
      Key     = tofchar("VOLUMES=");
      Mes     = tofchar("Connected clouds in subset direction? [Y]/N");
      r1      = userlog_c( &volumes, &nitems, &dfault, Key, Mes );
      volumes = tobool( volumes );
   }

   /*----------------------------------------------------------------*/
   /* If the user wants to connection clouds in the subset direction */
   /* do the 3d-dimensional analysis                                 */
   /*----------------------------------------------------------------*/
   if (volumes) {
      fint     sub = 0;            /* dummy */
      int      areacount;
      double   cputime, realtime;  /* Variables for timer */
      fint     elapse = 0;
      int      newindex;
      fint     *exclude = NULL;
      int      neworder = 0;
      int      t;

      r1 = 0;
      gds_close_c( Setout, &r1 );
      maxconnections = 2 * numobjects;
      connection = (connect *) realloc( (connect *) connection, maxconnections*sizeof(connect) );
      if (connection == NULL ) {
         anyoutC( 1, "Too many connections to fit in memory!" );
         return( 0 );
      }
      /*-------------------------------------------------------------*/
      /* Find two successive non empty subsets (from the output set) */
      /* The counter 'sub' is not the subset grid but just an index  */
      /* that starts at 0.                                           */
      /* i is the object counter with range [0..numobjects-1]. Start */
      /* i at 1, because object 0 cannot have a connection in a      */
      /* previous subset.                                            */
      /*-------------------------------------------------------------*/
      for (i = 0; i < nsubs; i++) {
         subsetend[i] = 0;               /* ranges for subset */
      }

      i = 1;
      do {
      	 /*--------------------------------------------------*/
         /* If the current object is in a new plane then the */
         /* program should read new planes                   */
         /*--------------------------------------------------*/
         if ((sub != object[i].subset) || (i == 1)) {
            /*--------------------------------------------------------------*/
            /* if the difference in subsetnumber between the current object */
            /* and the previous object is equal to 1, read planes.          */
            /*--------------------------------------------------------------*/
            while ( ((object[i].subset - object[i-1].subset) != 1) && (i < numobjects) ) i++;
            sub = object[i-1].subset;
            imagenr = 1;                                                         /* Previous plane */
            pixelsread = fillimage( Setout, subout[sub], blo, bhi, imagenr );
            sub = object[i].subset;                                   /* 'sub' has value of current subset */
            imagenr = 2;                                                         /* Current plane  */
            pixelsread = fillimage( Setout, subout[sub], blo, bhi, imagenr );

            (void) sprintf( message, "Read plane %d and %d.",
                            object[i-1].subset, object[i].subset  );
            status_c( tofchar( message ) );  /* Updates the current status of the reading proces */
         }

         /*-------------------------------------------------------------*/
         /* process the clouds, the start position of an object is      */
         /* always the lowest, leftmost pixel. Create an struct with    */
         /* an entry for the current index and the index of the         */
         /* connection in the previous subset. The resulting array      */
         /* contains only unique connections.                           */
         /*-------------------------------------------------------------*/
         y = object[i].Ypos;                                    /* First line where object is to be found */
         areacount = 0;                                         /* Size of current object in pixels */
         do {                                                   /* Loop over all y positions */
            float   previouscolor;
            float   currentcolor;
            for (x = blo[0]; x <= bhi[0]; x++) {                /* Loop over all x positions */
               currentcolor = GETOUTIM( x, y );                 /* Read pixels of 2d-output set */
               if (currentcolor != blank) {                     /* Part of an object */
                  if (i == (int) currentcolor) {                /* Part of the object we selected */
                     areacount++;                               /* Update end condition */
                     previouscolor = GETIM( x, y );             /* Read same position in previous subset */
                     if (previouscolor != blank) {              /* Found a 2d-object in previous subset */
                        int indx = object[(int) previouscolor].index;

                        /*------------------------------------*/
                        /* Found connection !                 */
                        /* Check if connection already exists */
                        /*------------------------------------*/
                        if (indx == i) {
                           /*------------------------------------------------------------------*/
                           /* Found old connection, search for corresponding connection number */
                           /* Overlap is incremented, connectcount isn't                       */
                           /*------------------------------------------------------------------*/
                           t = connectcount - 1;
                           while ( (connection[t].previous != (int) previouscolor) ) t--;
                           connection[t].overlap++;
                        } else {
                           /*-----------------------------*/
                           /* Reallocate memory if needed */
                           /*-----------------------------*/
                           if (connectcount == maxconnections) {
                              maxconnections += MAXCONNECT;
                              connection = (connect *) realloc( (connect *) connection, maxconnections*sizeof(connect) );
                              if (connection == NULL ) {
                                 anyoutC( 1, "Too many connections to fit in memory!" );
                                 return( 0 );
                              }
                           }

                           /*-----------------------------------------------------------*/
                           /* Found new connection, initialize connection and increment */
                           /* connection counter                                        */
                           /*-----------------------------------------------------------*/
                           connection[connectcount].current  = i;
                           connection[connectcount].previous = (int) previouscolor;
                           connection[connectcount].subset   = object[i].subset;
                           connection[connectcount].overlap  = 1;
                           connectcount++;
                           object[(int) previouscolor].index = i;    /* To insure unique connections */
                        }
                     }
                  }
               }
            }
            y++;
         } while (areacount < object[i].pixels);

         subsetend[sub] = connectcount-1;
         i++;
      } while (i < numobjects);

      (void) sprintf( message,
                     "Found %d connections between objects in successive subsets", connectcount );
      anyoutC( 1, message );


      /*------------------------------------------------------------------------------------------------*/
      /* With the connections found, the user can put restraints on the volumes that should be selected */
      /* The user can define several selections and write them to an output set                         */
      /* Reallocate memory for the volumes (object3d)                                                   */
      /*------------------------------------------------------------------------------------------------*/

      object3d = (isolatedvolume *) realloc( (isolatedvolume *) object3d,
                                    connectcount*sizeof(isolatedvolume) );
      if (object3d == NULL ) {
         anyoutC( 1, "Too many volumes to fit in memory!" );
         return( 0 );
      }

      /*-------------------------------------------------------*/
      /* Initialize the default of the keyword variables here  */
      /* to refresh them with the previous selection if the    */
      /* user is pompted for another selection                 */
      /*-------------------------------------------------------*/
      (void) memcpy(objvolume,objsize,sizeof(objvolume)) ;
      planes[0] = 2 ;
      planes[1] = 1 ;
      overlap[0] = 1.0;
      overlap[1] = ( bhi[0] - blo[0] + 1)*(bhi[1] - blo[1] + 1);  /* max. overlap possible */


      device = 3;
      keynum = 1;
      do {                          /* Loop over output updates */
         do {                       /* Loop over volume selections */

            /*-------------------------------------------------------------------*/
            /* User can define selection criteria for the 3-dimensional analysis */
            /*-------------------------------------------------------------------*/
            int   mlen;
            nitems    = 2;
            dfault    = REQUEST;
            if ( objvolume[1] == 1) {
                (void) sprintf( message, "Give range of volumes:        [%d,->]", objvolume[0] );
            } else {
                (void) sprintf( message, "Give range of volumes:        [%d,%d]", objvolume[0], objvolume[1] );
            }
            Key       = tofchar("OBJVOLUME=");
            Mes       = tofchar( message );
            r1        = userint_c( objvolume, &nitems, &dfault, Key, Mes );
            if ( r1 == 1 ) objvolume[1] = 1;
            cancel_c( Key );

            nitems = 2;
            dfault = REQUEST;
            if ( planes[1] == 1) {
            	(void) sprintf( message, "Give range in depths of objects:        [%d,->]", planes[0] );
            } else {
            	(void) sprintf( message, "Give range in depths of objects:        [%d,%d]", planes[0], planes[1] );
            }
            Key    = tofchar("DEPTH=");
            Mes    = tofchar( message );
            r1     = userint_c( planes, &nitems, &dfault, Key, Mes );
            if ( r1 == 1 ) planes[1] = 1;
            cancel_c( Key );

            /*----------------------------------------------------------*/
            /* User is allowed to use percentages for the overlap range */
            /*----------------------------------------------------------*/
            fmake( Percstr, 120 );        /* use a string for the input */
            do {
               err   = 0;               /* the errorlevel of the conversion routine: */
                                        /* err=-21 means empty list*/
               nitems     = 1;
               dfault     = REQUEST;
               Percstr.l  = 120;          /* maximum input is 120 characters */
               if ( percent ) {
                  (void) sprintf( message, "Give the range in overlap:        [%d%%,%d%%]", (int) overlap[0],
                                                                                             (int) overlap[1] );
               } else {
                  if ( overlap[1] >= ( bhi[0] - blo[0] + 1)*(bhi[1] - blo[1] + 1) ) {
                     (void) sprintf( message, "Give the range in overlap:        [%d,->]", (int) overlap[0] );
                  } else {
                     (void) sprintf( message, "Give the range in overlap:        [%d,%d]", (int) overlap[0],
                                                                                           (int) overlap[1] );
                  }
               }
               Key        = tofchar("OVERLAP=");
               Mes        = tofchar( message );
               r1         = usertext_c( Percstr, &dfault, Key, Mes );
               cancel_c( Key );

               /*---------------------------------*/
               /* Check if user wants percentages */
               /*---------------------------------*/
               i = 0;
               while( (i < r1) && (Percstr.a[i] != '%')) i++;
               if (Percstr.a[i] == '%') {
	          percent = YES;
	          Percstr.l = i;
	          overlap[0] = 0.0;
	          overlap[1] = 100.0;
	       }

	       /*-------------------------------------*/
	       /* Convert the input string into reals */
	       /*-------------------------------------*/
               nitems = 2;
	       r1 = dcdreal_c( Percstr, overlap, &nitems, &err );
               if ( (r1 > 0) && ( Percstr.a[i] != '%') ) percent = NO;
               if ( r1 == 1 ) {
               	  if ( percent ) overlap[1] = 100.0; else overlap[1] = ( bhi[0] - blo[0] + 1)*(bhi[1] - blo[1] + 1);
	       }

	       /*--------------------*/
	       /* Handle wrong input */
	       /*--------------------*/
               if ( (err != 0) && (err != -21) ) anyoutC(1, "wrong input, try again");
               if ( overlap[0] > overlap[1] ) anyoutC(1, "you can only include overlaps");

            } while( ((err != 0) && (err != -21)) || (overlap[0] > overlap[1]) );

            /*-----------------------------------------------*/
            /* The user can define some hidden keywords to   */
            /* control the size of the box around the volume */
            /*-----------------------------------------------*/
            nitems        = 2;
            dfault        = HIDDEN;
            minboxsize[0] = 1;
            minboxsize[1] = 1;
            Key           = tofchar("MINBOXSIZ=");
            Mes           = tofchar("Give min. box size for volume:       [1,1]");
            r1            = userint_c( minboxsize, &nitems, &dfault, Key, Mes );
            cancel_c( Key );

            nitems        = 2;
            dfault        = HIDDEN;
            maxboxsize[0] = bhi[0] - blo[0] + 1;
            maxboxsize[1] = bhi[1] - blo[1] + 1;
            (void) sprintf( message, "Give max. box size for volume:      [%d,%d]", maxboxsize[0]
                                                                                  , maxboxsize[1] );
            Key           = tofchar("MAXBOXSIZ=");
            Mes           = tofchar( message );
            r1            = userint_c( maxboxsize, &nitems, &dfault, Key, Mes );
            cancel_c( Key );


            /*-----------------------------------------------------*/
            /* Exclude objects with overlap smaller than specified */
            /*-----------------------------------------------------*/
            for (i = 0; i < connectcount; i++) {
               connection[i].free  = YES;                                    /* reset connection access */

               /*--------------------------------------------*/
               /* determine the overlap to size ratio of the */
               /* smallest of the two overlapping objects    */
               /*--------------------------------------------*/
               if ( object[connection[i].current].pixels > object[connection[i].previous].pixels ) {
                  connection[i].overlapfactor = ( (float) connection[i].overlap )
                                                /object[connection[i].previous].pixels;
               } else {
                  connection[i].overlapfactor = ( (float) connection[i].overlap )
                                                /object[connection[i].current].pixels;
               }

               /*----------------------------------------------------*/
               /* Handle percentage range different than pixel range */
               /*----------------------------------------------------*/
               if ( percent ) {
                  if ( ((connection[i].overlapfactor*100.0) < overlap[0]) ||
                       ((connection[i].overlapfactor*100.0) > overlap[1]) )  {
                     connection[i].free = NO;           /* no access to connection */
                  }
               } else {
                  if ( (connection[i].overlap < (int) overlap[0]) ||
                       (connection[i].overlap > (int) overlap[1]) )  {
                       connection[i].free = NO;           /* no access to connection */
                  }
               }
            }

            /*--------------------------------------------------*/
            /* Reset all objects and include some later again   */
            /*--------------------------------------------------*/
            for (i = 0; i < numobjects; i++) {
               object[i].index = -1;
               object[i].free = YES;
            }

            /*---------------------------------------------*/
            /* Find connected objects, construct object3ds */
            /*---------------------------------------------*/
            timer_c( &cputime, &realtime, &elapse );
            i = 0;
            newindex = 0;
            while( i < connectcount ) {
               int   losub, hisub;
               int   indexfirstobj;
               while( (i < connectcount) && !(connection[i].free) ) i++;  /* a connection must be free */
               if (i < connectcount) {

               	  /*----------------------------------*/
                  /* Initialization of the new volume */
                  /*----------------------------------*/
                  connections = 0;
                  overlapfactor = 0.0;
                  volume1 = 0;
                  flux1   = 0.0;
                  maxval1 = MYMAX( object[connection[i].current].maxval,
                                   object[connection[i].previous].maxval );
                  maxconnect = minconnect = i;                /* Initialize lowest and highest plane */
                  framebox[0][0] = object[connection[i].current].framebox[0][0];
                  framebox[0][1] = object[connection[i].current].framebox[0][1];
                  framebox[1][0] = object[connection[i].current].framebox[1][0];
                  framebox[1][1] = object[connection[i].current].framebox[1][1];
                  currentobject++;

                  /*------------------*/
                  /* Recursive search */
                  /*------------------*/
                  findnext( i, newindex );

                  /*-----------------------*/
                  /* Adjusting the results */
                  /*-----------------------*/
                  losub  = connection[minconnect].subset - 1; /* Connection always in previous plane */
                  hisub  = connection[maxconnect].subset;
                  depth1 = hisub - losub;
                  indexfirstobj = connection[minconnect].previous;

                  /*------------------------------------*/
                  /* Store the results in a structure   */
                  /*------------------------------------*/
                  object3d[newindex].startX = object[indexfirstobj].Xpos;
                  object3d[newindex].startY = object[indexfirstobj].Ypos;
                  object3d[newindex].volume = volume1;
                  object3d[newindex].flux   = flux1;
                  object3d[newindex].losub  = losub;
                  object3d[newindex].hisub  = hisub;
                  object3d[newindex].maxval = maxval1;
                  object3d[newindex].overlapfactor = overlapfactor/( (float) connections);
                  object3d[newindex].framebox[0][0] = framebox[0][0];
                  object3d[newindex].framebox[0][1] = framebox[0][1];
                  object3d[newindex].framebox[1][0] = framebox[1][0];
                  object3d[newindex].framebox[1][1] = framebox[1][1];
                  newindex++;
               }
            }


            /*----------------*/
            /* Select volumes */
            /*----------------*/
            for (i = 0, neworder = 0; i < newindex; i++) {
               /*----------------------------------------------------------------------------*/
               /* Check if depth is in range as given by the user in planes[0] and planes[1] */
               /* and if the volume is sufficient and box size allowed                       */
               /*----------------------------------------------------------------------------*/
               int  depth = object3d[i].hisub - object3d[i].losub + 1;
               int  xlength = object3d[i].framebox[1][0] - object3d[i].framebox[0][0] + 1;
               int  ylength = object3d[i].framebox[1][1] - object3d[i].framebox[0][1] + 1;
               bool indepthrange = YES;
               bool allowedbox = YES;
               bool involumerange = YES;

               /*-------------*/
               /* Depth check */
               /*-------------*/
               if (planes[1] == 1) {
                  if ( depth < planes[0] ) indepthrange = NO;
               } else {
               	 if (planes[0] > planes[1]) {
               	    if ( (depth > planes[1]) && (depth < planes[0]) ) indepthrange = NO;
               	 } else {
                    if ( (depth < planes[0]) || (depth > planes[1]) ) indepthrange = NO;
               	 }
               }

               /*-----------*/
               /* Box check */
               /*-----------*/
               if ( (xlength < minboxsize[0]) || (xlength > maxboxsize[0]) ||
                    (ylength < minboxsize[1]) || (ylength > maxboxsize[1])    ) allowedbox = NO;

               /*--------------*/
               /* Volume check */
               /*--------------*/
               if (objvolume[1] == 1) {
                  if ( object3d[i].volume < objvolume[0] ) involumerange = NO;
               } else {
                 if (objvolume[0] > objvolume[1]) {
                    if ( (object3d[i].volume > objvolume[1]) && (object3d[i].volume < objvolume[0]) ) involumerange = NO;
                 } else {
                    if ( (object3d[i].volume < objvolume[0]) || (object3d[i].volume > objvolume[1]) ) involumerange = NO;
                 }
               }

               /*-------------------------------------------------*/
               /* Check if volume satisfies the selectioncriteria */
               /*-------------------------------------------------*/
               if ( involumerange && indepthrange && allowedbox ) {
                  /*---------------------*/
                  /* New selected volume */
                  /*---------------------*/
                  object3d[i].index = neworder;
                  neworder++;
               } else {
               	  /*---------------------*/
               	  /* Selected out volume */
               	  /*---------------------*/
                  object3d[i].index = -1;
               }
            }
            /*----------------------------------------------------------------*/
            /* Show user calculation times and the number of selected volumes */
            /*----------------------------------------------------------------*/
            elapse = 1;
            timer_c( &cputime, &realtime, &elapse );
            (void) sprintf( message,
                           "Selected %d volumes : found in %.2f sec (%.2f cpu sec)", neworder, realtime, cputime );
            anyoutC( 1, message );

            nitems   = 1;
            dfault   = REQUEST;
            toscreen = toflog( YES );
            Key      = tofchar("TOSCREEN=");
            Mes      = tofchar("Display table on screen?         [Y]/N" );
            r1       = userlog_c( &toscreen, &nitems, &dfault, Key, Mes );
            cancel_c( Key );

            if (toscreen) 
            {
               /*-----------------------*/
               /* Write header of table */
               /*-----------------------*/
               display( device, asciifile, " " );
               (void) sprintf( message, "Analysis of volumes:" );
               display( device, asciifile, message );
               if (objvolume[1] == 1) {
                  (void) sprintf( message, "volume range: [%d, ->]", objvolume[0] );
               } else {
                  (void) sprintf( message, "volume range: [%d, %d]", objvolume[0], objvolume[1] );
               }
               display( device, asciifile, message );
               if (clip[1] == FLT_MAX) {
                  (void) sprintf( message, "range of data values: [%g, ->]", clip[0] );
               } else {
                  (void) sprintf( message, "range of data values: [%g, %g]", clip[0], clip[1] );
               }
               display( device, asciifile, message );

               if (percent) {
                  if (overlap[1] >= 100.0 ) {
                     (void) sprintf( message, "overlap: [%d%%, 100%]", (int) overlap[0] );
                  } else {
                     (void) sprintf( message, "overlap: [%d%%, %d%%]", (int) overlap[0], (int) overlap[1] );
                  }
                  display( device, asciifile, message );
               } else {
                  if (overlap[1] >= ( bhi[0] - blo[0] + 1)*(bhi[1] - blo[1] + 1) ) {
                     (void) sprintf( message, "overlap: [%d, ->]", (int) overlap[0] );
                  } else {
                     (void) sprintf( message, "overlap: [%d, %d]", (int) overlap[0], (int) overlap[1] );
                  }
                  display( device, asciifile, message );
               }

               if (planes[1] == 1) {
                  (void) sprintf( message, "depth of volumes: [%d, ->]", planes[0] );
               } else {
                  (void) sprintf( message, "depth of data volumes: [%d, %d]", planes[0], planes[1] );
               }
               display( device, asciifile, message );
               display( device, asciifile, " " );

               mlen = sprintf( message, "==NR|===X|===Y|LOWX|LOWY|LOSUB|HIGX|HIGY|HISUB|DEPTH|VOLUME|=FLUX/PIX|======MAX|OVERLAP|");
               display( device, asciifile, message );
               memset( message, '-', mlen );
               display( device, asciifile, message );

               /*----------------------------*/
               /* Write all selected volumes */
               /*----------------------------*/
               for (i = 0; i < newindex; i++) {
                  if ( object3d[i].index >= 0 ) {
                     (void) sprintf( message, "%4d|%4d|%4d|%4d|%4d|%5d|%4d|%4d|%5d|%5d|%6d|%9g|%9g|   %3d%%|",
                                     object3d[i].index + 1,
                                     object3d[i].startX,
                                     object3d[i].startY,
                                     object3d[i].framebox[0][0],
                                     object3d[i].framebox[0][1],
                                     subsetgrid[object3d[i].losub],                 /* Translate in real subsetnumbers */
                                     object3d[i].framebox[1][0],
                                     object3d[i].framebox[1][1],
                                     subsetgrid[object3d[i].hisub],                 /* Translate in real subsetnumbers */
                                     object3d[i].hisub - object3d[i].losub + 1,     /* depth */
                                     object3d[i].volume,
                                     object3d[i].flux/object3d[i].volume,           /* flux per pixel */
                                     object3d[i].maxval,
                                     (int) (object3d[i].overlapfactor*100.0)  );    /* overlap-percentage */
                     display( device, asciifile, message );
                  }
               }
               volume_statistics( object3d, newindex, device, asciifile );
            }

            agreed  = toflog(NO);
            nitems  = 1;
            dfault  = REQUEST;
            Key     = tofchar("AGREED=");
            Mes     = tofchar("Agreed with current selection?      Y/[N]");
            r1      = userlog_c( &agreed, &nitems, &dfault, Key, Mes );
            agreed  = tobool( agreed );
            cancel_c( Key );

         } while (!agreed);


         {
            /*---------------------------------------------*/
            /* Exclude some volumes, by their indexnumbers */
            /*---------------------------------------------*/

            int   j, k;
            bool to_much;

            if (exclude != NULL) free( exclude );
            exclude = (fint *) calloc( neworder, sizeof(fint) );
            if (exclude == NULL) {
               anyoutC( 1, "Too many volumes to fit in memory!" );
               return( 0 );
            }

            do {
               /*----------------------------*/
               /* handle the loop conditions */
               /*----------------------------*/
               if (neworder > MAXITEMS) to_much = YES; else to_much = NO;

               nitems = MYMIN(neworder,MAXITEMS);
               Key    = tofchar("EXCLUDE=");
               Mes    = tofchar("Give index of volumes to exclude:        [NONE]" );
               r1     = userint_c( exclude, &nitems, &dfault, Key, Mes );
               cancel_c( Key );

               /*---------------------------------------------------------*/
               /* Volumes are exluded by setting their indexnumbers to -1 */
               /*---------------------------------------------------------*/
               for (j = 0; j < r1; j++) {
                  for (k = 0; k < newindex; k++) {
                     if (object3d[k].index == exclude[j]) {
                        object3d[k].index = -1;
                        break;
                     }
                  }
               }
               if (r1) {
               	  /*------------------------*/
                  /* re-arrange the indices */
                  /*------------------------*/
                  for (j = 0, neworder = 0; j < newindex; j++) {
                     if (object3d[j].index != -1) {
                        object3d[j].index = neworder;
                        neworder++;
                     }
                  }
               }
            } while ( to_much && (r1 > 0) );
         }

         /*---------------------------------------------------------------*/
         /* Update the output. The default for the output is the previous */
         /* output set.                                                   */
         /*---------------------------------------------------------------*/

         (void) sprintf( message, "OUTSET%d=", keynum++ );
         gdsasn_c( KEY_INSET, tofchar(message), &class );
         dfault  = REQUEST;
         showdev = 3;
         Key     = tofchar(message);
         Mes     = MES_OUTSET2;
         fmake( Setout2, STRLEN );
         strncpy( Setout2.a, Setout.a, nelc_c(Setout) );
         do {
            nsubsout = gdsout_c( Setout2,       /* Name of the output set. */
                                 subout2,       /* Output array with subsets coordinate words.*/
                                 &nsubs,        /* Maximum number of subsets in subout. */
                                 &dfault,       /* Default code as in USERxxx. */
                                 Key,           /* User keyword prompt. */
                                 Mes,           /* Message for the user. */
                                 &showdev,      /* Device number (as in ANYOUT). */
                                 axnumout,      /* Array of size 'maxaxes' containing the axes numbers. */
                                 axcountout,    /* Array with the axis sizes. */
                                 &maxaxes );    /* Max axes the program can deal with. */
            agreed = (nsubsout == nsubs);
            if (!agreed) {
            	anyoutC( 1, "#out != #in" );
            } else {
               int l1, l2;
               l1 = nelc_c(Setout2);
               l2 = nelc_c(Setout);
               if (l1 == l2) {
                  if (strncmp(Setout2.a, Setout.a, l1) == 0) {
                     agreed  = toflog(NO);
                     nitems  = 1;
                     dfault  = REQUEST;
                     r1      = userlog_c( &agreed, &nitems, &dfault, tofchar("AGREED="),
                                          tofchar("You are changing your 2-dim results set, ok?      Y/[N]") );
                     agreed  = tobool( agreed );
                     cancel_c( tofchar("AGREED=") );
                     stop_selections = NO;
                     if (agreed) stop_selections = YES;
                  }
               }
            }
            cancel_c( Key );
         } while (!agreed);

         /*-------------------*/
         /* Initialize stabar */
         /*-------------------*/
         STBstart   = 0.0;
         STBend     = (float) (nsubsout * maxIObuf);
         STBcurrent = 0.0;
         stabar_c( &STBstart, &STBend, &STBcurrent );

         /*----------------------------------------------*/
         /* Write the selected volumes in the output set */
         /*----------------------------------------------*/

         for(subnr = 0; subnr < nsubsout; subnr++) {
            cwloO   = gdsc_fill_c( Setout2, &subout2[subnr], blo );
            cwhiO   = gdsc_fill_c( Setout2, &subout2[subnr], bhi );
            imagenr = 1;
            mcount  = 0;
            tidO    = 0;
            pixelsread = fillimage( Setout, subout[subnr], blo, bhi, imagenr );
            /* Update imageout */
            for (y = blo[1]; y <= bhi[1]; y++) {                    /* Loop over all y positions in box   */
               for (x = blo[0]; x <= bhi[0]; x++) {                 /* Loop over all x positions in box   */
                  float   pixelind;
                  STBcurrent++;
                  pixelind = GETIM( x, y );                         /* Read a pixel in the 2d-output set  */
                  if (pixelind != blank) {                          /* A pixel of a 2d-object is found    */
                     pixelind = object[(int) pixelind].index;       /* The indexnumber of the 2d-object   */
                     if ((int) pixelind != -1) {                    /* The 2d-object belongs to volume    */
                        pixelind = object3d[(int) pixelind].index;  /* The indexnumber of the volume      */
                     }
                     if ((int) pixelind == -1) {                    /* Volume is selected out             */
                        pixelind = blank;
                     }
                  }
                  SETOUTIM( x, y, pixelind );                       /* If volume write indexnr else blank */
               }
               stabar_c( &STBstart, &STBend, &STBcurrent );         /* Increase the statusbar             */
            }
            minmax3_c( imageout,
                       &pixelsread,
                       &minval[subnr], &maxval[subnr],
                       &nblanks[subnr],
                       &mcount );
            pixelswrite = pixelsread;
            gdsi_write_c( Setout2,
                          &cwloO, &cwhiO,
                          imageout,
                          &maxIObuf,
                          &pixelswrite,
                          &tidO );
         }
         status_c( tofchar("Header update ...") );

         /*-------------------------------------------*/
         /* Update OUTSET= descriptor with new values */
         /*-------------------------------------------*/

         change = YES;
         wminmax_c( Setout2, subout2,
                    minval, maxval, nblanks,
                    &nsubsout,
                    &change );
         r1 = 0;
         gds_close_c( Setout2, &r1 );

         /*--------------------------------------------------------------*/
         /* If 2d-set still exists check if user wants another selection */
         /*--------------------------------------------------------------*/
         if (stop_selections) {
            agreed = YES;
         } else {
           agreed  = toflog(YES);
           nitems  = 1;
           dfault  = REQUEST;
           Key     = tofchar("AGREED=");
           Mes     = tofchar("Agreed with current output?      [Y]/N");
           r1      = userlog_c( &agreed, &nitems, &dfault, Key, Mes );
           agreed  = tobool( agreed );
           cancel_c( Key );
         }

                         /*------------------------------*/
      } while (!agreed); /* Ready with volumes selection */
                         /*------------------------------*/

     /*-----------------------------*/
   } /* Ready with volumes analysis */
     /*-----------------------------*/


   /*-------------------------------------------------------*/
   /* To end the program, make sure files opened with fopen */
   /* are closed, allocated memory is released, PGPLOT is   */
   /* closed and HERMES is instructed to stop.              */
   /*-------------------------------------------------------*/

   free( image );
   free( imageout );
   if (asciifile) fclose(asciifile);
   if (posx       != NULL ) free( posx );
   if (posy       != NULL ) free( posy );
   if (Xarray     != NULL ) free( Xarray);
   if (Yarray     != NULL ) free( Yarray );
   if (object     != NULL ) free( object );
   if (object3d   != NULL ) free( object3d );
   if (connection != NULL ) free( connection );
   if (toplot) pgend_c();
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
