/*
                           COPYRIGHT (c) 1992
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             cplot.dc1

Name:          CPLOT

Purpose:       Create contour-, gray scale- and colour plots.
               Provide basic drawing routines.

Category:      PLOTTING

File:          cplot.c

Author:        M. Vogelaar

Keywords:

   INSET=      Set and subset(s) to work on. Maximum number of subsets
               is 256. The dimension of the input must be 2.

   BOX=        Frame of subset.                          [whole subset]
               Decrease area to be plotted.

   GRDEVICE=   Plot device:                           [List of devices]
               Destination of plot, Screen or Hardcopy.

   MOSAIC=     Number of ROWS in a mosaic:                  [No mosaic]
               Keyword is prompted if number of subsets > 1.
               This keyword determines the layout of the mosaic.

   NEXT=       Give <CR> for next plot.
               If no mosaic is selected but the number of input subsets
               is greater than 1, than each subset has its own plot.
               <CR> will start the next plot.

** CALCMNMX=   Calculate min. & max. in box (Y/N):                  [N]
               If CALCMNMX=N the program displays header values
               for the minimum and maximum values in the box. If
               header values cannot be obtained or CALCMNMX=Y, the
               values are calculated.

   SCALE=      Scale in x & y (units/mm or arcs/mm)        [Calculated]
               Defaults and units are shown in the message and are
               taken to fit the plot on GRDEVICE=.


               ===== CONTOUR CONTROL =====

** PERCENT=    Contours (and gray scales) in % (Y/N):               [N]
               If PERCENT=Y the contours (and grayscales) are percen-
               tages instead of discrete values.

   CONTOUR=    Give contours (min=..., max=... ):                [None]
               The contours are given in units of your image,
               or percentages.
               Maximum number of contours is 128.

** COLIND=     Color indices for contours:                 [Calculated]
               If you give more than 0 indices but less than the number
               of contours, the remaining indices are copied from the
               last one specified.

** CSTYLE=     All cont.(S)olid, dash (O)dd or (N)egative           [N]
               Distinguish contours by dashing the odd or negative
               contours.

** CWIDTH=     Linewidth for contours [1..21]:     [same as LINEWIDTH=]


               ===== GRAY SCALES =====

   GRAYSC=     Gray scales:                         [No gray scale map]
               Draw a gray-scale map of the image values.
               The gray scales are given in units of your image or
               percentages. For each interval a shade is calculated.
               Minimum number of scales is 1, maximum is 128.


               ===== CREATING A COLOUR PLOT =====

   COLPLOT=    Do you want a colour plot?                         Y/[N]
               A colour plot is made in interaction with GIDS.
               The GIDS colour Look Up Table and levels are used
               by the program to create a colour plot. GIDS must be
               available and set to your own preferences.
               Your graphics device must be a colour device
               (e.g. GRDEVICE=LCPSFILE). See description.


               ===== CONTROL PHYSICAL COORDINATES =====


** TICK=       separation between major ticks in x,y       [calculated]
               Units are physical units of the axes, except when the
               units are 'DEGREE' then the input is in seconds of arc.
               If an axis is labeled 'RA' and is unrotated, then the
               units are in seconds of time.

** NMINOR=     Number of minor ticks between major ticks   [Calculated]

** EXTEND=     Extend major tick lines in map?                    Y/[N]

** LINESTEPS=  Number of steps in tick lines:                      [40]
               More steps takes more time but result in
               smoother lines. Keyword in only asked if EXTEND=Y


               ===== LAYOUT OF PLOT =====


** MEAGRE=     Plot without units and labels:                      [N]Y
               If a plot is wanted without something along the axes,
               use MEAGRE=Y

** GRIDMARGIN= Gridmargins in mm in x, y:                         [0 0]
               Grid margin in x, y-direction. If one value is given,
               the second is copied from the first one.

** TITLEMARGIN=Title margins in mm in x, y:                [Calculated]
               Title margin in x, y direction. The defaults
               are 10 mm if XTITLE= and YTITLE= are specified.
               Otherwise they are 0 mm. If one value is given, the
               second is copied from the first one.

** LABELMARGIN=Label margins in mm in x, y:                [Calculated]
               Label margins for the physical coordinates
               along x- and y axis.

** INFOMARGIN= Info margin in mm (x-dir):          [Default by program]
               Default width for info is 50 mm.

** LINEWIDTH=  Line width in plot [1..21]:                          [1]

** XTITLE=     Title along the X axis:          [Axis name from header]

** XTPOS=      X-title position x,y (mm):                  [Calculated]

** YTITLE=     Title along the Y axis:          [Axis name from header]

** YTPOS=      Y-title position x,y (mm):                  [Calculated]

** AXMASK=     Mask for ticks & units for axes:     [Y Y Y Y Y N N Y y]
               Enable(Y)/Disable(N) plotting tick marks (4 axes) and
               physical coordinates (4 axes) along lower, right, upper
               and left axis. Default is draw tick marks along all axes
               and the physical units along the lower and left axis only.
               Axes are counted anti clockwise, starting with the
               lower x-axis. The last value indicates plotting of ticks
               along axes shared by two plots in a mosaic.

   COMPOS=     Give position for text:
               Positions can be given either in grid or physical
               coordinates. Default is the previous position.

   COMMENT=    Comment in plot:                         [Stop comments]

               Before plotting text there are some attributes to set
               (defaults are given by the program). Specification is
               requested for the axis text and comments. After the
               first comment, the keywords are hidden.


               ===== TEXT ATTRIBUTES =====


** LABELATT=   Hght,Wdth,Fnt,Jstf,Era,Angl,Col             [calculated]

** COMMATT*=   Hght,Wdth,Fnt,Jstf,Era,Angl,Col             [calculated]
               * is a wildcard for a comment number:
               COMMATT1= : attributes for first comment in a (sub)plot
               COMMATT2= :  "    "     "  second ......
               If COMMATT= (without comment number), the effect of all
               COMMATT*= keywords is overruled. This keyword is
               canceled, so it can be used in a recall file.

** SUBATT=     Hght,Wdth,Fnt,Jstf,Era,Angl,Col             [calculated]

** GRIDSATT=   Hght,Wdth,Fnt,Jstf,Era,Angl,Col             [calculated]

** TITLEATT=   Hght,Wdth,Fnt,Jstf,Era,Angl,Col             [calculated]

** INFOATT=    Hght,Wdth,Fnt,Jstf,Era,Angl,Col             [calculated]



           ===== INCLUDING A PHYSICAL SUBSET COORDINATE IN A MOSAIC ====


** PLSUB=      Plot phys. coord. of subset in plot?                 [N]
               Keyword can be set if dimension of the set is
               greater than 2. (See also SUBNUM= keyword)

** SUBNUM=     Plotnumber(s) to exclude plotting phys.coord's:   [none]
               Exclude plotting the physical coordinate label
               in panels given by SUBNUM=
               The upper left panel is panel 1.

** DECIMALS=   Decimals in phys. coord. of subset:                  [2]

** SUBPOS=     Position of physical sub. coord.:           [Calculated]
             
               Enter ONE alternative position for the labels that 
               mark a subset.


               ===== PLOT INFORMATION =====


** PLOTINFO=   Information in plot? Y/N:            [Depends on device]
               If the device is a hardcopy device the
               default is Y(es) otherwise it is N(o). The text contains
               info about contours, grayscales etc. and is located
               at the upper right of the plot.

** INFOCOM=    Comment in plot info:                       [No comment]
               Plot info can contain user supplied comment.
               Keyword is asked in a loop. The loop is aborted with
               carriage return. The keyword is hidden only the first
               time it requests input.


               ===== ADDITIONAL FEATURES =====


   GRID=       Frame with grid labeling Y/N:                        [Y]
               In a mosaic only isolated axes have grid labeling.

   MARKPOS=    Give marker position(s):                    [Stop input]
               marker. There is a loop over this keyword
               (so a recall file can be used here). Exit loop with
               carriage return. Maximum number of positions is 512.

   MARKER=     Graphics markers:                                [All 2]
               You can give as much different markers as there
               are specified (valid) positions after the use of
               MARKPOS=

** BEAMPOS=    Central position of ellipse corresponding to the beam
               you want to plot. If you specify a position, the
               beam characteristics from the header will be used as
               defaults for the next keywords. If the map is not a
               spatial map the beam is replaced by error bars.

** BEAM=       Beam major and minor axis FWHM in arcsec: [Header values]
               If values cannot be found in the header, the
               physical value of the grid spacings are taken as
               defaults.

** BEAMPA=     Position angle of beam in degrees:        [Header value]

** BEAMATT=    Erase, Separation (mm), Slope (deg), shape:   [0,1,45,1]
               argument 1) 0 Do not erase box around beam
                           1 Erase box around beam.
               argument 2) Shade lines have a default separation
                           of 1 mm
               argument 3) Default slope of the shade lines is 45 deg.
                           wrt. major axis.
               argument 4) 1 Plot elliptical beam.
                           2 Plot rectangular beam (IRAS survey,
                             long slit spectroscopy)

** BEAMNUM=    Plot number(s) to plot a beam:               [last plot]


               ===== DRAWING STRAIGHT LINES IN PLOT =====

** PLLINE=     Draw line to/from x1,y1 (to x2,y2):           [No line]
               If two pairs are given, than a line from x1,y1
               to x2,y2 is plotted. If only one pair is given, a
               line is plotted from the previous end point to this
               point.
               As soon as one line is defined, the PLLINE keyword
               is prompted in a loop and is not hidden anymore.
               The loop is aborted with carriage return.

** PLATT=      Width, Style, Color:                       [Calculated]
               Attributes for lines:
               Width: 1..21
               Style: 1..5
               Color: 0..15


               ===== DRAWING ELLIPSE(S) =====

** ELLPOS=     Central pos. for ellipse:                  [abort loop]

** ELLATT=     Width, Style, Color:                       [Calculated]
               Attributes for ellipse:
               Width: 1..21
               Style: 1..5
               Color: 0..15

** ELLAXESPA=  Beam maj, min (arcs) & PA (deg):                [0 0 0]
               For maps where one or both axes is not spatial, the
               major and minor axes are in pixels.
               The rotation angle is in degrees. 0 deg. means that the
               major axis is aligned with the declination axis, there
               is no correction for the angle in rotated maps.


               ===== OVERLAY OVER EXISTING IMAGE IN GIDS=====

** OVERLAY=    Overlay over existing image in GIDS?               Y[N]
               Only possible if a set is already displayed in
               GIDS. Default for the overlay set is the set name and
               subset of the image already displayed in GIDS.


               ===== OVERLAY OF NEW (SUB)SET(S) =====


** INSET2=     See above

   BOX2=         "

   CONTOUR2=     "

   COLIND2=      "

** CSTYLE2=      "

   GRAYSC2=      "

   COLPLOT2=    Do you want a colour plot?                         Y/[N]


               ===== CURSOR INTERACTION =====
               ( Only working properly in GIDS )

** INTERACTIVE=Purpose is to call an interactive cursor routine to be
               able to mark positions in the plot or to get and convert
               positions and put the values in the log-file.
               P <carriage return>   : Plot marker at this position
               Q <carriage return>   : Quit interactive cursor mode





Description:  Among standard ways of presenting astronomical data are
              contour and grayscale plots. If a map is represented by
              f(x,y), contours are lines of constant f. This requires
              the data you want to present with this program must be 2
              dimensional. Therefore sets with more than 2 axes need a
              subset specification at INSET=. Information about the
              syntax can be found in the GIPSY users manual. The names
              of the axes can be found with the programs HEADER or DISK.
              If only a part of the data need to be displayed, use BOX=
              with four arguments to decrease the size. Input can be a
              lower and an upper coordinate pair, a coordinate pair and
              combination with physical coordinates. See the GIPSY user
              manual for the use of physical coordinates. If you want to
              display more than one subset of a set (INSET=AURORA F 1:10)
              you get each subset on a blank screen or paper (advancing
              to a new page or clearing the screen is controlled by the
              NEXT=<CR> keyword) unless you want to display all the data
              in a mosaic. With MOSAIC= you can give the number of rows
              in the mosaic. The program calculates the number of columns.
              The first plot will be in the upper left corner of the
              screen or paper. Each subset in a mosaic will now be
              called 'subplot'.


              Contours

              The contours at CONTOUR= are a series of floating point
              numbers given in units of your map. This keyword obeys the
              syntax for reals as described in the GIPSY user manual.
              This includes the use of expressions also. An example
              could be CONTOUR=LOG([10:1000:50]) which is evaluated to
              1, 1.77815, 2.04139, 2.20412, ...., 2.98227
              Contours will be plotted with the same line width as the
              axes of your plot. The blanks in your data are ignored,
              making gaps in the contour map. The contour levels can be
              given in percentages also. To put input in percentage
              mode, select PERCENT=Y. The conversion from percentages to
              absolute levels is done with the formula:
              level[i] = min + perc[i]/100 * [max-min]
              so that 0% results in the level 'min' and 100% results in
              the level 'max'.
              The values min, max are the minimum and maximum values of
              the data as recorded in the header of the current
              (sub)set. If you don't trust these values or if your box is
              smaller than the entire (sub)set, use CALCMNMX=Y to
              calculate these values. The min and max are also
              calculated if PERCENT=Y and no header items DATAMIN and
              DATAMAX are found in the header. If plotting more than one
              contour, you can select a contour style to distinguish negative
              from positive contours, (default CSTYLE=N), or odd from even
              contours (CSTYLE=O). If you want all contours to be solid,
              use CSTYLE=S. Contours are associated with a color index.
              This index is an integer number that corresponds to a
              device dependent color. For a color plot you can use in
              COLIND= the color indices:

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

              The number of color indices must be equal to the number of
              contours. If COLIND=<CR> the indices are calculated by the
              program. If at least one value is given but the number of
              indices is less than the number of contours, the remaining
              indices are copied from the last one specified.
              For a black and white hardcopy you can use 0 for a white
              contour and 1 for a black contour. This combination is
              sometimes used to emphasize the contours in dark regions
              of a gray scale plot. If the device is the screen, the
              color indices will not be very useful. The width of the
              contours is controlled by CWIDTH=. Together with LINEWIDTH=
              and PLATT=, these keywords control the width of the lines to
              be plotted. The smallest number is 1 the largest possible
              number is 21. The CWIDTH= keyword is asked hidden just
              before plotting a contour, making it possible to draw
              different contours in different widths. The default values
              of all the keywords containing widths is 1 if the plotting
              device is the screen or 2 if it is a hardcopy device.
              Example: CWIDTH=4;3;2;1
              The lowest contour has width 4, the highest has width 1.
              The semi colons are important here because CWIDTH= accepts
              only one number at a time.

              ** Trick:
              If the minimum value 1 is too thick on paper, there is a
              possibility to set 'setlinewidth' in your postscript file
              to a smaller value

              Devices

              The destination of the plot is selected with GRDEVICE=
              If you give <CR> only, you get a list of devices that can
              contain for example:

              X          : Tektronix window on screen
              L1LASER    : QMS laser printer 1 in landscape mode
              P1LASER    : QMS laser printer 1 in portrait mode
              LCOLOUR    : Colour printer in landscape mode
              PCOLOUR    : Colour printer in portrait mode
              NULL       : null device (no plot output)

              etc...

              On some printers it is possible to plot on a transparency
              instead of paper. Instructions are obtained at local
              system management.


              Gray scales

              Instead or along with drawing contours, it is also
              possible to fill your plot with shades representing the
              image values. The shades are given in GRAYSC=. The maximum
              number of shades is 128. Again, input values are floating
              point numbers and expressions are allowed. The scales are
              in units of the map and must be in ascending order.
              Example: GRAYSC=2 3.5 8
              Image values smaller than 2 are white in the plot. Values
              equal to or greater than 2, but smaller than 3.5 get the first
              shade of gray, values 3.5 <= f(x,y) < 8 get the second
              shade of gray and values >= 8 are black in the plot. The
              available number of different shades depend on the
              selected device. If PERCENT=Y, the gray scale values are
              percentages.


              ** Hint

              If you want to fill contours smoothly with gray scales,
              (contours can intersect the 'gray scale pixels')
              simulate more pixels by regridding the map. Use REPROJ
              with changed grid spacings to achieve this effect.



              Color plots

              Color plots can be made if you specified color indices for
              text or contours and send the plot to a color printer. As
              soon as the program knows that you want to use a color
              printer, you are also able to print colored pixels instead of
              gray pixels. The image data is translated to colors according
              to ..........


              Layout

              A plot consists of a title along the lower x-axis and left
              y-axis, a space for labels containing positions in
              physical coordinates, a grid margin to reserve space for
              plotting positions in pixels, the plot itself and an area
              where important information about the plot is displayed.


              Titles

              The default titles along x and y axes are the names of
              these axes as found in the header. Titles can be altered
              with XTITLE= or YTITLE= The keywords accept text strings.
              A string can contain escape sequences. These are character-
              sequences that are not plotted, but are interpreted as
              instructions. All escape sequences start with a backslash
              character (\).

              \u  Start a superscript or end a subscript;
              \d  Start a subscript or end a superscript;
                  (\u and \d must always be used in pairs)
              \b  Backspace, i.e. do not advance text pointer after
                  plotting the previous character;
              \\  Backslash character;
              \A  Angstrom symbol;
              \gx Greek letter corresponding to roman letter x;
                  (Lower case and upper case x give different greek
                  characters )
              \fn Switch to Normal font
              \fr Switch to Roman font
              \fi Switch to Italic font
              \fs Switch to Script font

              Like all 'plot' text, the text can contain so called
              PGPLOT symbols. Available symbols (~1000) are arranged
              according to Hersey's numerical sequence and are listed in
              Table B.1 of the PGPLOT manual. Any character can be
              inserted in a text string using an escape sequence of the
              form \(n), where n is the 4-digit Hersey number.
              The positions of the titles are calculated by the program
              but can be altered with XTPOS= and YTPOS=. Each keyword
              accepts a coordinate pair in mm. The numbers are absolute
              positions in the plot. If for some reason there is not
              enough space along the axes to place the title, increase
              the default values (10 mm in x and y direction) at
              TITLEMARGIN=  If only one number is given, the y value is
              automatically copied from this value.


              Position in pixels

              Positions along the axes can be in pixels and physical
              values. If GRID=Y, each 10th pixel position will be
              marked inside the plot box. Default there is no room
              reserved in x and y direction so the numbers can spoil
              the layout of the plot if contours reach the borders.
              But extra space for the grids can be reserved.
              This space is controlled by the keyword
              GRIDMARGIN= which accepts two numbers for space in x and y
              direction. If only one number is given, the y value is
              copied from this value. If GRID=N (default) there are
              no grid margins.


              Physical values

              The labels along the axes indicate the physical positions
              i.e. if transformation from pixels to physical coordinates
              is possible. If transformation is not possible, the
              possible reason is mentioned in your log-file. There the
              program puts the result of transformation of pixel 0 to a
              physical position. Conversion always take place to the
              primary axis units as found in the header (CUNIT#). If
              there are also secondary units, the program will convert
              the pixels to these units. The default separations between
              two positions is calculated by the program. A position
              label is plotted together with a marker called a Tick. Space
              between two are controlled by TICK=. It accepts values for
              the x and y direction overruling the calculated default
              values. Negative numbers will be converted to positive
              numbers. The units are the axis units except if the header
              unit was 'DEGREE'. The values are in seconds of arc then.
              If an axis is the RA axis of an unrotated map, the TICK=
              units are in time seconds. Between two major tick marks
              there are minor tick marks. The default number is
              calculated by the program. The numbers of minor ticks in x
              and y direction can be changed with NMINOR= If you don't
              want minor tick marks, use NMINOR=0 0

              It is possible to plot labels for all axes, but in most
              plots only the lower x axis and the left y axis do have
              labels, while the others have ticks only. For a single
              plot this is the default labeling. In a mosaic, lines
              connecting two 'subplots' have tick marks only. The
              decision to plot ticks or labels is made after AXMASK=
              The keywords accepts 8+1 entries. An entry is Y or N. The
              first four values indicate the plotting of ticks and the
              second four indicate the plotting of labels. Default is
              AXMASK=Y Y Y Y Y N N Y y
              Counting of the axes is anti-clockwise and starts with
              the lower x-axis. The last value indicates plotting of
              ticks along axes shared by two plots in a mosaic. The
              default is Y. If you change this to N, there will be no
              ticks on the 'inside' axes of a mosaic.
              Space for the labels along both axes is calculated, but
              it is not known to the program beforehand how large the
              labels along the y axis are going to be. Therefore
              sometimes the space between these labels and the y title
              is to big. The spacing is controlled with LABELMARGIN=
              which accepts two values. The first is the space in mm
              in the x direction (for the y-labels) and the second is
              the space in the y direction.


              Rotated images

              Along spatial rotated axes, only offsets are plotted. An
              offset is wrt. a point in your map which is pixel 0 if
              this pixel is inside the box or the central pixel if
              pixel 0 is not in the box.


              Scales

              The plot is scaled in some way to fit on paper or screen.
              First the physical size in mm of the device plot area is
              determined. From this number the margins for grids, labels
              titles and info is subtracted. This number is divided by
              the number of subplots in one of the directions and is
              scaled onto the number of pixels in that direction .
              After a correction for the grid margin we end up with a
              scale in grids/mm. Changing these values can be done with
              SCALE= but then the scales are converted to units/mm.
              These units are the physical units as found in the header,
              only if the units are 'DEGREE', the scale input must be in
              ARCSEC/mm then. If both axes have the same units, then the
              scales are made equal in both directions.


              Plot information

              Information about plot characteristics is placed in the
              log file and some is also placed in the plot. The keyword
              that can be used here is PLOTINFO= which is N for the
              screen and Y for other devices. Note that this parameter
              also influences the scaling in your plot. Info in the plot
              contains items like the object name, the set name, used
              box, units, scales etc. It can be extended with a user
              supplied comment in INFOCOM= which is asked in a loop that
              is aborted with carriage return. The info is closed with a
              time stamp which is also placed in the log file so that
              you're always able to find the corresponding information
              in the log-file (GIPSY.LOG).


              FEATURES

              Marking of positions

              Positions in a plot can be marked. The positions are taken
              from MARKPOS= The keyword is asked in a loop and canceled
              after each specification. In this way it made suitable for
              using so called recall files.   Example: MARKPOS=<mypos

              and mypos.rcl  contains:

              * 29 30 45 * 4 10 40
              * 29 30 30 * 4 10  0
              ......

              Each physical position in the recall file will be marked
              as long as it is contained in BOX= Maximum number of
              positions is 512. Each position is marked with a symbol as
              selected with MARKER= Such a symbol is called a Graph
              Marker and is associated with a symbol number. The symbol
              number can be: -1 to draw a dot of the smallest possible
              size; 0-31 to draw any one of the symbols in figure 4.1 of
              the PGPLOT manual; or 33-127 to draw the corresponding
              ASCII character. Examples of symbols are:

                  1    dot
                  2    plus
                  3    star
                  4    circle
                  5    cross
                  6    square
                  ..   .....
                 17    small circle filled up
                 28    Arrow Left
                 29    Arrow Right
                 30    Arrow Up
                 31    Arrow Down

              Markpos accepts at most the same number of symbol numbers as
              there are positions given within the box (this number is
              displayed in the log file). The keyword is not asked in a
              loop, so no recall file can be used. For different symbols
              you can use the repeat facility like MARKER=2::100 8::50 10
              (first 100 positions in symbol 2, next 50 in 8 and what is
              left all in symbol 10).


              Beam

              The process of drawing a beam in your plot is started with
              the keyword BEAMPOS= which accepts a coordinate pair
              either in pixels or in physical units. Default, the
              program reads the FWHM's and position angle from the header
              and calculates the minor and major axis of the beam by
              dividing the header values (degrees) by 3600 to get a
              value in seconds of arc. The FWHM is read from header
              items BMMAJ and BMMIN and the angle (in degrees wrt the
              north) from BMPA. However if the items are not found in
              the descriptor, you are prompted to give the values
              yourself. If you want to overrule the defaults use BEAM=
              for major and minor sizes in seconds of arc, and BEAMPA= for
              the position angle in degrees. The beam is shaded with
              straight lines with default 1 mm separation and with an
              angle of 30 degrees wrt the major axis. Keyword BEAMATT=
              accepts four values: first value is the 'erase' variable.
              Default it is set to 0, and nothing happens with the beam,
              but when it is set to 1, the area in a box containing the
              beam will be erased with the background color. On screen
              however you will see the outline only. The second value
              is the line separation in mm and the third value is the
              angle in degrees. You can get a rectangular shaped non-shaded
              beam (IRAS survey, long slit spectroscopy) if the fourth
              parameter is 2 (default 1 = elliptical).

              Default, the beam in a mosaic is plotted in the last subplot,
              but this can be altered with BEAMNUM= which accepts at most
              the maximum of subplots integer numbers. Subplot 1 is the
              upper left plot.

              If the map is not a spatial map, the beam is replaced by
              error bars. The default lengths will be the grid spacing
              in units as found in the header.


              Plotting the physical value of the subset

              If the set has more than two axes, and a coordinate
              transformation is possible, you can print the physical
              value of a subset in the plot. For this you use PLSUB=Y
              The number of decimals to be printed is controlled by
              DECIMALS= which has default 2. The default position is
              somewhere in the upper left corner, but if you want a
              different position, use SUBPOS= with a position in pixels
              or physical coordinates. The position of the text applies
              to all the subsets in a mosaic. The units are the units as
              found in the header. If there is a secondary axis defined
              in the header, the units that correspond to that axis are
              used. The units are returned to your log-file.


              Comments in plot

              With keyword COMMENT= you can put strings in your plot.
              The keyword is prompted only if previously COMPOS= was
              defined. As with other positions, COMPOS= expect you to
              give a coordinate pair either in pixels or in physical
              coordinates. A loop is started now. For each specification
              of COMPOS= there is a COMMENT= prompt. The keyword accepts
              the so called Hersey symbols also. The loop is aborted
              with COMMENT=<CR>. Each comment can be in different
              height, width font etc. These so called text attributes
              are connected with the text attributes keywords.
              The text string can also contain escape sequences. These
              are character-sequences that are not plotted, but are
              interpreted as instructions.


              Text attributes keywords

              Input are 7 floating point numbers that control the
              appearance of the text to be plotted. The input syntax is:

              XXXXXATT=Hght,Wdth,Fnt,Jstf,Era,Angl,Col   [x1,..,x7]

              More specific:

              Hght:      Character height
                         It's a normalized value corresponding to about
                         1/40 of the height of the plot device.
              Wdth:      Thickness of chars & lines (1..21):
              Fnt:       Font type 1,2,3, 4:
                         1  single stroke "normal" font
                         2  roman font
                         3  italic font
                         4  script font
              Jstf:      Justification of text: 0=left, 0.5=middle, 1=right
                         of a given coordinate.
              Era:       Erase text box first before plotting text.
                         0 = Do not erase background.
                         1 = Erase background for the number of characters
                             found in the text.
                         n = Erase background for the n characters.
              Angl:      Angle of text, 0 is horizontal.
              Col:       Character color, for indices see color indices
                         above.


              Not all text obey these attributes. For example labeling
              has its own justification and has no erase option.
              Defaults are all determined by the program.

              Defaults:
              LABELATT=1.0, currentlinewidth, 2, 1, 0, 0.0, currentcolor
              COMMATT*=1.0, currentlinewidth, 2, 1, 0, 0.0, currentcolor
              TITLEATT=1.0, currentlinewidth, 2, 1, 0, 0.0, currentcolor
              INFOATT= 0.5, 1,                2, 1, 0, 0.0, currentcolor
              SUBATT=  0.6, currentlinewidth, 2, 1, 0, 0.0, currentcolor
              GRIDSATT=0.5, currentlinewidth, 2, 1, 0, 0.0, currentcolor

              * is a number from 1 to the number of comments in one (sub)plot
              If the number is omitted, each COMMATT= keyword overrules
              the current COMMATT*= specification. COMMATT= is canceled
              and therefore can be used in a recall file. This option is
              used if the number of keywords is greater than allowed for
              a default file. Then the  attributes can be stored in a recall
              file.


              Extended ticks

              The tick marks can be extended so that they are connected
              by curves following the current projection (EXTEND=Y).
              The number of points calculated in a curve is determined
              with LINESTEPS= Default is 40 points, for a quick look
              you can do with less points, but for a quality plot 40
              will probably not be sufficient. The extensions of ticks
              in other sky systems is not yet implemented.


              Drawing lines in plot

              To add some free style plotting use PLLINE= The keyword is
              hidden but after specification it will be asked unhidden
              in a loop that is aborted by PLLINE=<CR> The keyword
              accepts at most 4 floating point numbers i.e. 2 coordinate
              pairs (in pixels or physical coordinates). The action that
              follows is the plotting of a line between the two points.
              If the keyword appears again and you specify only one
              coordinate the line is plotted from the previous last
              coordinate to the specified coordinate. An application for
              these lines is the indication of zero levels for instance.
              The line width, style and color are controlled by PLATT=
              The range in width is [1..21], the range in colors is
              [0..15] and line style is one from:

              1   Full line
              2   Long dashes
              3   Dash-dot-dash-dot
              4   Dotted
              5   Dash-dot-dot-dot


              Plotting ellipses

              ELLPOS= is a position in grids or physical coordinates.
              If the keyword is not empty, you start a loop in which
              a beam and angle is asked in ELLAXESPA= First value of
              this keyword is the major axis, second the minor axis
              and the third is the angle in degrees wrt the positive
              y axis. The units of the major and minor axes are in
              seconds of arc if both axes in the plot are spatial
              axes, else, the units are grids. The appearance is
              controlled by ELLATT= (see remarks at PLATT=).


              Overlays

              CPLOT distinguishes two overlays. The first is an overlay
              over an existing image displayed in GIDS (for example
              displayed with the program VIEW). The hidden keyword
              OVERLAY= must be set to YES. CPLOT then, calculates the
              defaults needed to fit the overlay. The default for INSET=
              will be name and subset of the set that is already
              displayed in GIDS. The default for BOX= will be calculated.
              The program tries to create a box that is equal (in
              physical coordinates) to the box of the currently displayed
              set in GIDS. If the overlay box is smaller, everything
              outside that box will be erased before the labels are
              written. A default scale is calculated so that the
              overlay will fit. The rule for a new scale is:

                                           OldGridSpacing
                     NewScale = OldScale * --------------
                                           NewGridSpacing


              There is not a prompt for GRDEVICE= (because the specifi-
              cation MUST be GRDEVICE=gids//append). The gridmargins
              are both set to zero.

              The second type
              is an overlay of contours over the existing ones. These
              overlays are made possible by asking in the current plot
              for a new (sub)set with INSET2= The keyword is hidden so
              it must be specified beforehand. If no more overlays are
              needed use INSET2=<CR> to abort the overlay loop. For each
              subset it is possible to create overlays. The program
              calculates a box for the new (sub)set so that it is
              contained in the original box in physical coordinates.
              With the prompted keyword BOX2= you can rearrange these
              values. Pixel 0,0 in the original set is transformed to
              physical coordinates which are converted to a pixel position
              in the new set.
              From this point on all new pixels are scaled with new
              scales. A new scale is calculated as described above.

              There is no check on projection. You have to decide
              yourself whether the overlay makes sense or you have to
              use the program REPROJ first. The keywords CONTOUR2=,
              COLIND2=, and GRAYSC2= are described under CONTOUR= etc.
              If you select no original contours and gray scales in a
              mosaic, but use INSET2= for a second (sub)set, it is
              possible to use different data origins in your mosaic or
              use different contours per subset etc.


              Colour plots

              To create a colour plot, it is not necessary anymore to
              use OVERLAY=Y and make a hardcopy of the result in GIDS.
              Now you can select a colour option with the keyword
              COLPLOT=Y. For the graphics device you have to select
              a colour device ( e.g.: GRDEVICE= LCPSFILE (or PCPSFILE,
              LCOLOUR, PCOLOUR) ). Now the program wants to know how
              to translate the data values of your (sub)set to colours.
              You have to use GIDS for this. In GIDS you can view
              your (sub)set and set levels and colours to your own
              preferences. If you are satisfied with what you see, run
              CPLOT COLPLOT=Y and the colour plot can be made. It is
              convenient to send the plot first to a PostScript file
              (GRDEVICE=PCPSFILE or LCPSFILE) and view the result
              with a viewer like 'gs' (Ghostscript) (Tip: do not leave
              GIDS while viewing with 'gs'). Together with the INSET2=
              keyword, the keyword COLPLOT2= can be used to make colour
              plots with different colour characteristics. See also
              the remarks about plotting grayscales. If you want to
              use your own colour look up table, use the program COLOUR.
              It loads a COLOUR Look Up Table in GIDS. The LUT can be
              activated in GIDS with the USER button in the COLOR menu.



Notes:        WARNING: If you want to use a grayscale map in your overlays,
              make sure that this is your first (sub)set. Otherwise a
              grayscale map will obscure all previous plotted contours
              in a hardcopy.


Examples:     Examples of default files:
              1) Create contour plot of subset containing
                'short observation beam' (data from A. Broeils).
                Note the special use of the comment keyword.

              linewidth=2
              BEAMPOS=
              BOX=-80 -80 80 80
              CONTOUR= -0.1 -0.05 -0.025 0.025 0.05 0.1 0.2 0.4 0.8
              GRAYSC=
              GRDEVICE=x
              GRID= n
              INSET= app 1
              SCALE=10 10
              tick=360 360
              nminor=1 1
              markpos=
              compos=10 51.8;-31 45;-10 29;-43 49;-70 65;
              comment=\(2522);\(2522);fan beam;res. axis;(b)
              commatt1=1 2 2 0.5 0 126;
              commatt2=1 2 2 0.5 0 -144;
              commatt3=1.2 2 2 0 0 36;
              commatt4=1.2 2 2 0 0 -54;
              commatt5=1.2 2 2 0 1;
              xtitle=R.A. - offset (arcmin)
              ytitle=Dec. - offset (arcmin)
              titleatt=1.5 2
              plline=-20 30 10 51.8; -31 45 -20 30;
              platt=3;3
              labelatt=1.3 2
              plotinfo=n

              The use of a Hersey symbol is demonstrated in:
              comment=\(2522); ....


              2) Example of mosaic and use of recall files. There are
                 five rows in the mosaic.

              INSET=clean 1:29:2
              BOX=-50 -65 60 35
              GRDEVICE=x
              PLOTINFO=
              MOSAIC=5
              LINEWIDTH=1
              PLSUB=y
              SUBATT=0.8 1 2 0 1
              DECIMALS=0
              SUBPOS=
              PERCENT=n
              CONTOUR=-0.48 -0.24 0.24 0.48 0.96 1.44
              CSTYLE=
              GRAYSC=
              GRIDMARGIN=
              labelmargin=25 9.1
              SCALE=15 15
              GRID=
              MARKPOS=<opt2460;
              MARKER=
              TICK=60 360
              NMINOR=1 1
              XTITLE=R.A. (1950)
              YTITLE=Dec. (1950)
              AXMASK=
              BEAMPOS=-48 -63
              BEAMATT=
              BEAMNUM=2
              COMMENT=
              COMPOS=;
              EXTENDTICKS=
              LINESTEPS=
              LABELATT=0.8 2
              TITLEATT=1.0 2


              The file 'opt2460.rcl' contains:

              * 7 52 35.8 * 60 29 01
              * 7 52 36   * 60 24 00
              * 7 51 57.4 * 60 26 12
              * 7 51 56.8 * 60 26 17


              3) Example use of overlays. De primary set is called
                '30' and the overlay is the subset 'dtotal.new 1'
                 (data: A. Szomoru).


              axmask=
              markpos=;
              BEAMPOS=45 -65
              beampa=-98.50
              beam=74.79 58.58
              BOX= 30 -75 100 -25
              BOX2= 30 -35 50 -22
              CONTOUR2=0.005 0.01:0.06:0.01
              contour=
              GRAYSC=2 5:20:5
              graysc2=
              GRDEVICE=x
              GRID=n
              INSET=30
              inset2=dtotal.new 1 ;
              LINEWIDTH=3
              MARKER=
              PERCENT=
              SCALE=3 3
              CSTYLE=
              titleatt=1.5,3
              labelatt=1,3
              xtitle=R.A. (1950)
              ytitle=Dec. (1950)
              xtpos=109.208405 10
              titlemargin=20 20
              markpos=;
              compos=;


              Plotting more overlays is achieved with more (sub)sets in
              INSET2=. For instance use '1005' as primary data and
              'warpleft 1' and 'warpright 1' as overlay subsets.

              ........
              BOX= -280 -190 230 210
              BOX2= 30 -35 50 -22;30 -35 50 -22;
              CONTOUR=
              contour2=0.002;0.002;
              GRAYSC=6.69 17.49 43.94 110.4
              graysc2=; ; ;
              INSET=/dt1/arpad/bootes/1005
              inset2=warpleft 1 ;warpright 1;
              ........

              Note the use of semi colons in the file.


              4) Example of using the color indices. The first 5 contours
                 (in the dark regions of the gray scale plot) are white
                 and the remaining contours are black.

              INSET=velo 1
              BOX=-60 -50 50 30
              GRDEVICE=x
              MOSAIC=
              PLSUB=
              DECIMALS=
              SUBPOS=
              PERCENT=n
              LINEWIDTH=2
              CONTOUR=3650:4100:50
              COLIND=1::5 0
              CSTYLE=
              GRAYSC=3620:4100:10
              graysc=
              PLOTINFO=
              GRIDMARGIN=
              SCALE=5 5
              GRID=
              MARKPOS=<opt5533;
              MARKER=
              TICK=20 360
              NMINOR=1 2
              XTITLE=R.A. (1950)
              YTITLE=Dec. (1950)
              AXMASK=
              xBEAMPOS=-55 -45
              BEAMPOS=-170 -140
              BEAMSHADE=
              COMMENT=NGC 5533
              COMPOS=-170 80;
              TEXTATT=;1.3;1.5 2 2 0;1.5
              INSET2=;


              5) Example offset axis and the use of PLLINE= to draw
                 offset axes in the plot. The contents of the used
                 recall files is displayed also.


              AXMASK=
              BEAMPOS=
              BOX= -30 14 30 40
              CONTOUR= -1 -0.5 0.5:2:0.5
              GRAYSC=
              GRDEVICE= x
              GRID=n
              INSET= 19slice dec 0
              LINEWIDTH=3
              gridmargin=0 0
              PERCENT=
              SCALE=
              CSTYLE=
              box=-30 14 30 40
              markpos=<xvrot
              marker=17;
              titleatt=1.5,3
              labelatt=1,3
              markpos=;
              compos=<compos
              commatt1=2,3,1,0.5,0,0,1
              commatt2=2,3,1,0.5,0,0,1
              comment=<comment
              xtitle=Offset along major axis (arcsec)
              ytitle=Heliocentric velocity (km/s)
              titlemargin=20 20
              plline=<plline
              platt=<platt


              Contents of the used recall files:

              xvrot.rcl: (positions of markers)
              -11.3636 33.74362
              -10.2272 33.68168
              -9.09090 33.58433
              -7.95454 33.47814
              -6.81818    33.31
              .....
              11.36363 22.98256
              ;

              compos.rcl: (positions of comments)
              -4.59 14
              4.59 14
              ;

              comment.rcl: (Hersey symbols as comments)
              \(2262)
              \(2262)

              plline.rcl: (physical- and grid  coordinates are mixed)

              0 14 0 40
              u 229.286 u 14234.2 u 229.248 u 14234.2
              14 17 18.5455 17
              16.273 16 16.273 18

              platt.rcl: (attributes for the lines, width, style, color,
              two lines in long dashes, two lines in full)
              1,2,1
              1,2,1
              3,1,1
              3,1,1





Updates:      Feb 12, 1992; VOG, new release (3.0), physical units,
                                 rotated images.
              Mar 18, 1992; VOG, new release (3.1), more extensions,
                                 documentation.
              Jan 04, 1994; KGB, bug in plotting overlays repaired.
              May 19, 1994; VEH, SUBNUM= keyword added.
              Feb 14, 1997; VOG, Bug SUBNUM= removed
              Mar 05, 1998; VOG, Bug in call to new beam routine 
                                 removed. 
              Nov 19, 1998; VOG, PGCOLIM replaced by PGIMAG (PGPLOT 5.2.0)
              Nov 20, 1998; VOG, Replaced pggray2_c by local pggray2 
                                 routine that calls standard PGGRAY
              Apr 12, 2009; VOG, Replaced NINT macro by one that used floor().
                                 Now all the coordinate transformations
                                 are compatible with other transform routines
                                 (e.g. gds___unpack.c).
                                 Also corrected grids for offsets caused
                                 by non-integer values of CRPIX before
                                 and after a call to proco().



#<
*/



/* cplot:   include files     */


#include    "stdio.h"
#include    "stdlib.h"
#include    "string.h"
#include    "math.h"
#include    "cmain.h"          /* Defines the main body for GIPSY application */
#include    "gipsyc.h"         /* Defines the ANSI-F77 types for F to C intf. */
#include    "float.h"                                /* Definition of FLT_MAX */
#include    "time.h"
#include    "ctype.h"    /* Declares ANSI C functions for testing characters. */

#include    "axtype.h"
#include    "cancel.h"
#include    "cotrans.h"
#include    "ncoords.h"
#include    "proco.h"
#include    "skyco.h"
#include    "axcoord.h"
#include    "error.h"
#include    "reject.h"
#include    "finis.h"
#include    "init.h"
#include    "minmax1.h"
#include    "anyout.h"
#include    "setfblank.h"
#include    "showsub1.h"
#include    "myname.h"
#include    "nelc.h"
#include    "axunit.h"
#include    "hms.h"
#include    "dms.h"

#include    "userint.h"
#include    "userlog.h"
#include    "userreal.h"
#include    "userdble.h"
#include    "usertext.h"
#include    "usercharu.h"

#include    "gds_exist.h"

#include    "gdsc_fill.h"
#include    "gdsc_grid.h"
#include    "gdsc_name.h"
#include    "gdsc_ndims.h"
#include    "gdsc_range.h"
#include    "gdsd_rchar.h"
#include    "gdsd_rdble.h"
#include    "gdsd_rreal.h"
#include    "gdsi_read.h"

#include    "gdi_iinfo.h"
#include    "gdi_iinfo2.h"
#include    "gdi_cinfo.h"
#include    "gdi_open.h"
#include    "gdi_open2.h"
#include    "gdi_close.h"
#include    "gdi_frame.h"
#include    "gdi_getlut.h"

#include    "gdsinp.h"
#include    "gdspos.h"
#include    "gdsbox.h"

#include    "pgplot.h"
#include    "pgellipse.h"
#include    "beam.h"

/* cplot:   definitions    */

/* Initialize string with macro */
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


#define MYMAX(a,b)       ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)       ( (a) > (b) ? (b) : (a) )
/* Pre April 2009 definition: #define NINT(a)          ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) ) */
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )
#define BETWEEN(a,b,c)   ( (a) < (b) ? (b) : ((a) > (c) ? (c) : (a) ) )
#define ABS(a)           ( (a) < 0 ? (-(a)) : (a) )
#define PI               3.141592653589793
#define FSWAP(a,b)       { float temp=(a);(a)=(b);(b)=temp; } /* Swap 2 numbers */


#define     AXISLOWER      0
#define     AXISRIGHT      1
#define     AXISUPPER      2
#define     AXISLEFT       3
#define     BOX            2
#define     MAXAXES        10
#define     MAXCONT        256
#define     MAXDATA        640 * 640
#define     MAXTITLE       80
#define     MAXMES         128
#define     MAXOPT         10
#define     MAXPOS         8192
#define     MAXSETNAMLEN   80
#define     MAXSUB         256
#define     GMARGIN        0.0
#define     IMARGIN        50.0
#define     RELEASE       "3.2"
#define     TITLE_WIDTH    10
#define     NONE           0    /* Default values for use in userxxx routines */
#define     REQUEST        1
#define     HIDDEN         2
#define     EXACT          4
#define     YES            1
#define     NO             0


#define  KEY_BEAM        tofchar("BEAM=")
#define  KEY_BEAMPOS     tofchar("BEAMPOS=")
#define  KEY_BEAMNUM     tofchar("BEAMNUM=")
#define  KEY_BEAMPA      tofchar("BEAMPA=")
#define  KEY_BEAMATT     tofchar("BEAMATT=")
#define  KEY_ELLPOS      tofchar("ELLPOS=")
#define  KEY_ELLATT      tofchar("ELLATT=")
#define  KEY_ELLAXESPA   tofchar("ELLAXESPA=")
#define  KEY_BOX         tofchar("BOX=")
#define  KEY_BOX2        tofchar("BOX2=")
#define  KEY_CONTOUR     tofchar("CONTOUR=")
#define  KEY_COLIND      tofchar("COLIND=")
#define  KEY_CONTOUR2    tofchar("CONTOUR2=")
#define  KEY_COLIND2     tofchar("COLIND2=")
#define  KEY_FONT        tofchar("FONT=")
#define  KEY_GRAYSC      tofchar("GRAYSC=")
#define  KEY_GRAYSC2     tofchar("GRAYSC2=")
#define  KEY_GRID        tofchar("GRID=")
#define  KEY_GRIDMARGIN  tofchar("GRIDMARGIN=")
#define  KEY_TITLEMARGIN tofchar("TITLEMARGIN=")
#define  KEY_LABELMARGIN tofchar("LABELMARGIN=")
#define  KEY_INFOMARGIN  tofchar("INFOMARGIN=")
#define  KEY_INSET       tofchar("INSET=")
#define  KEY_INSET2      tofchar("INSET2=")
#define  KEY_MARKER      tofchar("MARKER=")
#define  KEY_MOSAIC      tofchar("MOSAIC=")
#define  KEY_CALCMNMX    tofchar("CALCMNMX=")
#define  KEY_NEXT        tofchar("NEXT=")
#define  KEY_NMINOR      tofchar("NMINOR=")
#define  KEY_CSTYLE      tofchar("CSTYLE=")
#define  KEY_CSTYLE2     tofchar("CSTYLE2=")
#define  KEY_PERCENT     tofchar("PERCENT=")
#define  KEY_MARKPOS     tofchar("MARKPOS=")
#define  KEY_AXMASK      tofchar("AXMASK=")
#define  KEY_PLOTINFO    tofchar("PLOTINFO=")
#define  KEY_SCALE       tofchar("SCALE=")
#define  KEY_TICK        tofchar("TICK=")
#define  KEY_XTITLE      tofchar("XTITLE=")
#define  KEY_XTPOS       tofchar("XTPOS=")
#define  KEY_YTITLE      tofchar("YTITLE=")
#define  KEY_YTPOS       tofchar("YTPOS=")
#define  KEY_INTERACTIVE tofchar("INTERACTIVE=")
#define  KEY_TEXTATT     tofchar("TEXTATT=")
#define  KEY_COMMENT     tofchar("COMMENT=")
#define  KEY_COMPOS      tofchar("COMPOS=")
#define  KEY_LINEWIDTH   tofchar("LINEWIDTH=" )
#define  KEY_CWIDTH      tofchar("CWIDTH=")
#define  KEY_PLSUB       tofchar("PLSUB=")
#define  KEY_SUBNUM      tofchar("SUBNUM=")
#define  KEY_PLLINE      tofchar("PLLINE")
#define  KEY_PLATT       tofchar("PLATT=")
#define  KEY_DECIMALS    tofchar("DECIMALS=")
#define  KEY_SUBPOS      tofchar("SUBPOS=")
#define  KEY_EXTEND      tofchar("EXTEND=")
#define  KEY_LINESTEPS   tofchar("LINESTEPS=")
#define  KEY_INFOCOM     tofchar("INFOCOM=")
#define  KEY_OVERLAY     tofchar("OVERLAY=")


#define  MES_BEAM        tofchar( message )
#define  MES_BEAMPOS     tofchar("Give position for beam  [no beam]")
#define  MES_BEAMNUM     tofchar( message )
#define  MES_BEAMPA      tofchar( message )
#define  MES_BEAMATT     tofchar("Erase, Separation(mm),Slope(deg), shape:  [0,1,45,1]")
#define  MES_ELLPOS      tofchar( message)
#define  MES_ELLATT      tofchar( message )
#define  MES_ELLAXESPA   tofchar( message )
#define  MES_BOX         tofchar(" ")
#define  MES_BOX2        tofchar(" ")
#define  MES_CONTOUR     tofchar( message )
#define  MES_COLIND      tofchar("Color indices for contours:  [Calculated]")
#define  MES_CONTOUR2    tofchar( message )
#define  MES_COLIND2     tofchar("Color indices for overlay contours:  [Calculated]")
#define  MES_FONT        tofchar("Font type 1,2,3,4  [2]")
#define  MES_GRAYSC      tofchar("Gray scales:      [no gray scale map]")
#define  MES_GRAYSC2     tofchar("Gray scales:      [no gray scale map]")
#define  MES_GRID        tofchar("Frame with grid labeling?  [n]")
#define  MES_GRIDMARGIN  tofchar( message )
#define  MES_TITLEMARGIN tofchar( message )
#define  MES_LABELMARGIN tofchar( message )
#define  MES_INFOMARGIN  tofchar( message )
#define  MES_INSET       tofchar(" ")
#define  MES_INSET2      tofchar("Give (sub)set for overlay:    [no overlay]")
#define  MES_MARKER      tofchar("Graphics marker(s):                    [2]")
#define  MES_MOSAIC      tofchar("Number of rows in mosaic  [no mosaic]")
#define  MES_CALCMNMX    tofchar("Calculate min. & max. in box (Y/N):    [N]")
#define  MES_NEXT        tofchar("Give <CR> for next plot")
#define  MES_NMINOR      tofchar("Number of minor ticks between major tickmarks  [calculated]")
#define  MES_CSTYLE      tofchar("All cont.(S)olid, dash (O)dd or (N)egative  [N]")
#define  MES_PERCENT     tofchar("Contours (and grayscales) in %? (Y/N):   [N]")
#define  MES_MARKPOS     tofchar("Give marker position(s):        [stop input]")
#define  MES_AXMASK      tofchar("Mask for ticks & units for axes: [Y Y Y Y Y N N Y y]")
#define  MES_PLOTINFO    tofchar( message )
#define  MES_SCALE       tofchar( message )
#define  MES_TICK        tofchar("Separation between maj. tickmarks:     [calculated]")
#define  MES_XTITLE      tofchar( message )
#define  MES_XTPOS       tofchar( message )
#define  MES_YTITLE      tofchar( message )
#define  MES_YTPOS       tofchar( message )
#define  MES_INTERACTIVE tofchar("Interactive cursor mode?     [N]")
#define  MES_COMMENT     tofchar("Comment in plot: ")
#define  MES_COMPOS      tofchar( message )
#define  MES_LINEWIDTH   tofchar( message )
#define  MES_CWIDTH      tofchar( message )
#define  MES_PLSUB       tofchar("Plot phys. coord. subset in plot?      [N]")
#define  MES_SUBNUM      tofchar( message )
#define  MES_PLLINE      tofchar("Draw line to/from x1,y1 (to x2,y2):    [No line]")
#define  MES_PLATT       tofchar( message )
#define  MES_DECIMALS    tofchar("Decimals in phys. coord. of subset:    [2]")
#define  MES_SUBPOS      tofchar("Position of physical sub. coord.:   [Calculated]")
#define  MES_EXTEND      tofchar("Extend major tick lines in map?        Y/[N]")
#define  MES_LINESTEPS   tofchar("Number of steps in tick lines:         [40]")
#define  MES_INFOCOM     tofchar("Comment in plot info:    [No comment]")
#define  MES_OVERLAY     tofchar("Overlay over existing image in GIDS?   Y[N]")


/* variables for input set */

static   fint     Axperm[MAXAXES], Axsize[MAXAXES];
static   fint     Blo[MAXAXES], Bhi[MAXAXES];
static   fint     Subset[MAXSUB];
static   char     message[MAXMES];
static   fchar    Setname;
static   fint     Setdim;


/* variables for plot */

static   fint     ngrid[2];
static   float    inch = 25.4;
static   float    devsize[2];
static   float    labelmargin[2] = { 25., 10. };
static   float    grmnmx[2];                         /* Minmax for the gray scales */
static   float    gridmargin[2] = { 0.0, 0.0 };
static   float    mnmx[2];
static   float    plotsize[2];
static   float    sc_g[2];
static   float    titlemargin[2] = { 0.0, 0.0 };
static   float    infomargin;
static   float    x1_mm, x2_mm, y1_mm, y2_mm;


/* Gids related */

static   bool     Gidsoverlay;
static   fint     Gidsblo[2], Gidsbhi[2];
static   float    Gidsoffset[2];
static   double   GidsCdelt[2];
static   fint     Gidsaxis[2];
static   fchar    Gidsset;
static   fint     Gidssubset;
static   float    Gidsflo[2], Gidsfhi[2];        /* Corners of total GIDS area in grids */
static   fint     Gidsdisplay_id = -1;           /* id of display */
static   float    Gidsbscale;
static   float    Gidsbzero;
static   fint     Gidsmincol;
static   fint     Gidsmaxcol;
static   fint     Gidsncolors;
static   fint     Gidsblank;


/* global variables */

static   bool     graysc = FALSE, grids = TRUE;
static   bool     colsc;
static   bool     plotinfo;
static   bool     Toscreen;
static   char     style_s[20];
static   fchar    style = { style_s, 20 };
static   char     style2_s[20];
static   fchar    style2 = { style2_s, 20 };
static   double   pos[MAXPOS * 2];
static   fint     marker[MAXPOS];
static   char     xtitle_s[MAXTITLE];
static   char     ytitle_s[MAXTITLE];
static   fchar    xtitle = { xtitle_s, MAXTITLE };
static   fchar    ytitle = { ytitle_s, MAXTITLE };
static   fint     lxtitle, lytitle;
static   fint     linew = 1;
static   fint     mosaic = 0;
static   fint     ncont, ngray, npos, nreject, Nsub;
static   fint     nminor[2] = { 0, 0 };
static   fint     nplot[2] = { 1, 1 };
static   float    Blank;
static   float    cont[MAXCONT];
static   float    Cont2[MAXCONT];
static   fint     Cindex[MAXCONT];
static   fint     Cindex2[MAXCONT];
static   float    grayscale[MAXCONT];
static   float    Grayscale2[MAXCONT];
static   float    data[MAXDATA];
static   float    tick[2];
static   fint     Scrnum;
static   fint     Dfault;
static   fint     Setlevel = 0;
static   fchar    Cunit[MAXAXES];
static   fchar    Dunit[MAXAXES];            /* Units of physical coordinates */
static   fchar    Ctype[MAXAXES];            /* Complete axis name fi. RA-NCP */
static   fchar    Dtype[MAXAXES];            /* Complete axis name fi. RA-NCP */
static   char     messbuf[80];
static   double   Crota[MAXAXES];
static   double   physicl[MAXAXES];
static   double   Grid[2];
static   bool     CotransOk;                        /* Is cotrans successful? */
static   fint     grids2units = 1;
static   bool     Interactive;
static   double   Cdelt[2], Ddelt[2];
static   double   Crpix[2];
static   double   Crval[2];
static   fint     Colind;
static   fint     Width, Font, Color;
static   float    Height, Angle, Justification;
static   int      Spatialmap;
static   bool     calcmnmx;
static   bool     percentage;
static   float    label_len = 0.0;
static   float    label_height = 0.0;
static   double   bmpos[2];
static   fint     Beam[MAXSUB];
static   bool     plsub;
static   fint     sbnum[MAXSUB];
static   double   RotationAngle;
static   bool     Axmask[9];
static   bool     Extendticks;
static   bool     Meagre;
static   fint     Linesteps;
static   int      agreed;
static   fint     Subsetdecimals[MAXAXES];
static   double   Subpos[2];
static   fint     Erase;
static   fint     Colev[2];
static   fchar    Mes;
static	 float    lutr[256];
static	 float    lutg[256];
static	 float    lutb[256];
static   fint     lutlen;
static   fchar    Devtxt;

struct   axesdef {
   fint     StartXgrid;
   fint     StartYgrid;
   fint     EndXgrid;
   fint     EndYgrid;
   fint     Gridlength;
   bool     Togrids;
   fint     Gridstep;
   double   StartXphys;
   double   StartYphys;
   double   EndXphys;
   double   EndYphys;
   bool     Tophys;
   bool     Todelta;
   bool     rotated;
   double   Angle;
   fint     Nminor;
   fint     Ticksize;
   fint     Font;
   fint     Fontsize;
   fint     Fontcolor;
   fint     Linecolor;
   fint     Linethickness;
   float    StartXmm;
   float    StartYmm;
   float    EndXmm;
   float    EndYmm;
   fint     Type;
   fint     Skysystem;
   fint     Prosystem;
   fint     Velsystem;
} Axno[4];






void anyoutC( int scrnum, char *anystr )
/*
 *------------------------------------------------------------------------------
 * The C version of 'anyoutC' needs a C type string as argument only. and
 * and integer to determine the destination of the output:
 *   0  use default [set by HERMES to 3 but can be changed by user]
 *   1  terminal
 *   2  LOG file
 *   8  terminal, suppressed in "experienced mode"
 *  16  terminal, only when in "test mode
 *------------------------------------------------------------------------------
 */
{
   fint Scrnum = (fint) scrnum;
   anyout_c( &Scrnum, tofchar( anystr ) );
}



static void clearstr( fchar str, fint l )
/*----------------------------------------------*/
/* Clear string, i.e. fill string with l spaces */
/*----------------------------------------------*/
{
   int i;
   for (i = 0; i < (int) l; str.a[i++] = ' ' );
}


static int prec( double Step )
/*------------------------------------------------------------------*/
/* Step is for instance the stepsize between two tickmarks. This    */
/* size determines the precision of the numbers along the tickmarks */
/* and returns the integer indicating the number of precision       */
/* digits.                                                          */
/*------------------------------------------------------------------*/
{
   int     ndigit = 0;
   double  Prec = fabs(Step);

   if (Prec < 1.0) {
      do {
      Prec *= 10.0;
         ndigit++;
      } while (Prec < 1.0);
   }
   return( ndigit );
}


void Attributes( char *mode, int subsetnr )
/*--------------------------------------------------------------------------*/
/* This routine sets some of the plot text attributes like character height */
/* character width, font, justification, color and angle.                   */
/*--------------------------------------------------------------------------*/
{
   fchar           Keyword;
   static int      first = YES;
   static int      oldsubsetnr = 0;                  /* Static essential here */
   static int      indx = 0;                         /* Static essential here */
   float           Dummyreal[7];
   fint            Nitems;
   fint            Dfault;
   fint            R1;
   char            message[120];
   static float    OldHeight;
   static float    OldJustification;
   static float    OldAngle;
   static fint     OldWidth, OldFont, OldColor, OldErase;


   if (first) {
       pgqch_c( &OldHeight );
       pgqlw_c( &OldWidth  );
       pgqcf_c( &OldFont   );
       pgqci_c( &OldColor  );
       OldAngle = 0.0;
       OldJustification = 0.0;
       OldErase = 0;
       first = NO;
   }

   fmake( Keyword, 20 );
   if (strcmp( mode, "LABEL" ) == 0) {
      Keyword = tofchar( "LABELATT=" );
      Font = 2; Height = 1.0; Width = linew; Color = OldColor;
      Justification = 0.0; Angle = 0.0; Erase = NO;
   }
   if (strcmp( mode, "COMMENT" ) == 0) {
      if (subsetnr == oldsubsetnr) indx++;
      else {
         indx = 1;
         oldsubsetnr = subsetnr;
      }
      (void) sprintf( Keyword.a, "COMMATT%d=", indx );
      Font = 2; Height = 1.0; Width = linew; Color = OldColor;
      Justification = 0.0; Angle = 0.0; Erase = NO;
   }
   if (strcmp( mode, "TITLE" ) == 0) {
      Keyword = tofchar( "TITLEATT=" );
      Font = 2; Height = 1.0; Width = linew; Color = OldColor;
      Justification = 0.0; Angle = 0.0; Erase = NO;
   }
   if (strcmp( mode, "INFO" ) == 0) {
      Keyword = tofchar( "INFOATT=" );
      Font = 2; Height = 0.5; Width = 1.0; Color = OldColor;
      Justification = 0.0; Angle = 0.0; Erase = NO;
   }
   if (strcmp( mode, "SUBSET" ) == 0) {
      Keyword = tofchar( "SUBATT=" );
      Font = 2; Height = 0.6; Width = linew; Color = OldColor;
      Justification = 0.0; Angle = 0.0; Erase = NO;
   }
   if (strcmp( mode, "GRIDS" ) == 0) {
      Keyword = tofchar( "GRIDSATT=" );
      Font = 2; Height = 0.5; Width = linew; Color = OldColor;
      Justification = 0.0; Angle = 0.0; Erase = NO;
   }

   (void) sprintf( message,
           "Hght,Wdth,Fnt,Jstf,Era,Angl,Col: [%.1f,%d,%d,%.1f,%d,%.2f,%d]",
            Height,
            Width,
            Font,
            Justification,
            Erase,
            Angle,
            Color );

   Nitems = 7;
   Dfault = HIDDEN;
   Dummyreal[0]  = Height;
   Dummyreal[1]  = (float) Width;
   Dummyreal[2]  = (float) Font;
   Dummyreal[3]  = Justification;
   Dummyreal[4]  = (float) Erase;
   Dummyreal[5]  = Angle;
   Dummyreal[6]  = (float) Color;
   R1 = userreal_c( Dummyreal, &Nitems, &Dfault, Keyword, tofchar(message) );
   if (strstr( Keyword.a, "COMMATT" ) != NULL) {
      /* It is possible to overrule the COMMATT*= keywords with COMMATT= */
      /* Also COMMATT= is canceled, so can be used with recall files */
      R1 = userreal_c( Dummyreal, &Nitems, &Dfault,
                       tofchar("COMMATT="), tofchar("Change COMMATT*=") );
      cancel_c( tofchar("COMMATT=") );
   }

   Height        =        Dummyreal[0];
   Width         = (fint) Dummyreal[1];
   Font          = (fint) Dummyreal[2];
   Justification =        Dummyreal[3];
   Erase         = (fint) Dummyreal[4];
   Angle         =        Dummyreal[5];
   Color         = (fint) Dummyreal[6];
   pgsch_c( &Height );
   pgslw_c( &Width  );
   pgscf_c( &Font   );
   pgsci_c( &Color  );
}


static   void  plmove( float x, float y )
/*---------------------------------------*/
/* Alternative pgmove                    */
/*---------------------------------------*/
{
    pgmove_c( &x, &y );
}



static   void  pldraw( float x, float y )
/*---------------------------------------*/
/* Alternative pgdraw                    */
/*---------------------------------------*/
{
    pgdraw_c( &x, &y );
}



static void pltext( float x, float y, float angle,
                    float fjust, fchar text )
/*---------------------------------------*/
/* Alternative pgtxt                     */
/*---------------------------------------*/
{
    pgptxt_c( &x, &y, &angle, &fjust, text );
}



static  void grid2mm( fint ns, float Xgrid, float Ygrid,
                      float *Xmm, float *Ymm )
/*-------------------------------------------------------------------*/
/* Convert a position in grids to a position in mm taking the subset */
/* position in a mosaic into account.                                */
/*-------------------------------------------------------------------*/
{
   int      ix, iy;
   float    start[2];


   if (Gidsoverlay) {
      start[0] = Gidsoffset[0];
      start[1] = Gidsoffset[1];
   } else {
      if ( nplot[0] == 1 && nplot[1] == 1 ) {           /* no mosaic: simple.... */
         start[0] = titlemargin[0] + labelmargin[0];
         start[1] = titlemargin[1] + labelmargin[1];
      } else {
         ix = ns % nplot[0];
         iy = ns / nplot[0];
         start[0] = titlemargin[0] + labelmargin[0] + ( plotsize[0] ) * ix;
         start[1] = titlemargin[1] + labelmargin[1] +
                    ( plotsize[1] ) * ( nplot[1] - iy - 1 ) ;
      }
   }
   *Xmm = ( Xgrid - ( float )Blo[0] ) / sc_g[0] + start[0] + gridmargin[0];
   *Ymm = ( Ygrid - ( float )Blo[1] ) / sc_g[1] + start[1] + gridmargin[1];
}



static  void phys2grid( double Px, double Py,
                        double *X, double *Y, int subnr )
/*----------------------------------------------------------*/
/* Convert physical coordinate (Phys1, Phys2) to grid (X,Y) */
/*----------------------------------------------------------*/
{
   fint            Phys2grid = 0;
   static double   DumGrid[MAXAXES];
   static double   DumPhys[2];
   fint            R1;

   if (CotransOk) {
      DumPhys[0] = Px;
      DumPhys[1] = Py;
      R1 = cotrans_c( Setname, &Subset[subnr], DumPhys, DumGrid, &Phys2grid );
      if  (R1 == 0) {
         *X = DumGrid[Axperm[0]-1];
         *Y = DumGrid[Axperm[1]-1];
      }
      else {
         *X = Px;
         *Y = Py;
      }
   }
   else {
      *X = Px;
      *Y = Py;
   }
}



static void grid2phys( double X, double Y,
                       double *Px, double *Py, int subnr )
/*--------------------------------------------------------------------*/
/* Convert grid (X,Y) to physical coordinate (Phys1, Phys2)           */
/*--------------------------------------------------------------------*/
{
   fint            Grid2phys = 1;
   static double   DumGrid[2];
   static double   DumPhys[MAXAXES];
   fint            R1;

   if (CotransOk) {
      DumGrid[0] = X;
      DumGrid[1] = Y;
      R1 = cotrans_c( Setname, &Subset[subnr], DumGrid,  DumPhys, &Grid2phys);
      if  (R1 == 0) {
         *Px = DumPhys[Axperm[0]-1];
         *Py = DumPhys[Axperm[1]-1];
      }
      else {
         *Px = X;
         *Py = Y;
      }
   }
   else {
      *Px = X;
      *Py = Y;
   }
}


static  void getstartXY( fint ns, float start[] )
/*-------------------------------------------------------------------*/
/* Determine starting point of plot in mm. The position changes for  */
/* every subset in a mosaic.                                         */
/*-------------------------------------------------------------------*/
{
   if (Gidsoverlay) {
      start[0] = Gidsoffset[0];
      start[1] = Gidsoffset[1];
   } else {
      if ( nplot[0] == 1 && nplot[1] == 1 ) {           /* no mosaic: simple.... */
         start[0] = titlemargin[0] + labelmargin[0];
         start[1] = titlemargin[1] + labelmargin[1];
      } else {
         int      ix, iy;

         ix = ns % nplot[0];
         iy = ns / nplot[0];
         start[0] = titlemargin[0] + labelmargin[0] + ( plotsize[0] ) * ix;
         start[1] = titlemargin[1] + labelmargin[1] +
                    ( plotsize[1] ) * ( nplot[1] - iy - 1 ) ;
      }
   }
}


void boxpltext( float Xpos_mm, float Ypos_mm, float Angle,
                float Justification, char *comment, bool Toscreen,
                fint Erase )
/*--------------------------------------------------------------------*/
/* Draw box around text and erase interior.                           */
/*--------------------------------------------------------------------*/
{
   fint   Len;
   float  digit_h, digit_w;
   float  Xsize, Ysize;
   float  X[4], Y[4];
   float  XR[4], YR[4];
   float  ShiftX, ShiftY;
   double Rads;
   fint   Solid;
   fint   Oldcolor;
   fint   Background = 0;
   fint   Nitems;
   int    i;


   pgqci_c( &Oldcolor );
   Len = strlen( comment );
   if (Erase != 1) Len = MYMAX(1, Erase);
    pgqch_c( &digit_h );
   digit_h *= MYMIN(devsize[1], devsize[0]) / 40.0;
   digit_w = 0.8 * digit_h;                                /* Character width */
   Xsize = digit_w * (float) Len;
   Ysize = digit_h;

   /* Define a box fitting the given text */

   X[0] = Xpos_mm;       Y[0] = Ypos_mm;
   X[1] = X[0] + Xsize;  Y[1] = Y[0];
   X[2] = X[1];          Y[2] = Y[0] + Ysize;
   X[3] = X[0];          Y[3] = Y[2];

   /* Now correct for the justification */

   ShiftX = Justification * Xsize;
   ShiftY = 0.3 * Ysize;
   for (i = 0; i < 4; i++) {
       X[i] -= ShiftX;
   }
   Y[0] -= ShiftY; Y[1] -= ShiftY;

   /* Increase box to fit comment on paper */

   X[0] -= 0.3 * digit_w;       Y[0] -= 0.08 * digit_h;
   X[1] += 0.3 * digit_w;       Y[1] -= 0.08 * digit_h;
   X[2] += 0.3 * digit_w;       Y[2] += 0.15 * digit_h;
   X[3] -= 0.3 * digit_w;       Y[3] += 0.15 * digit_h;

   /* Rotate box over given angle */

   Rads = (double) Angle * PI / 180.0;
   for (i = 0; i < 4; i++) {
       X[i]  -= Xpos_mm;         Y[i] -= Ypos_mm;
       XR[i]  = X[i]*cos(Rads) - Y[i]*sin(Rads);
       YR[i]  = X[i]*sin(Rads) + Y[i]*cos(Rads);
       XR[i] += Xpos_mm;         YR[i] += Ypos_mm;
   }

   if (Toscreen) {
      Solid = 2;
   } else {
      Solid = 1;
      pgsci_c( &Background );
   }
   Nitems = 4;                                 /* Number of points in polygon */
   pgsfs_c(  &Solid );                         /* Set fill area style */
   pgpoly_c( &Nitems, XR, YR );                /* Draw polygon */
   pgsci_c( &Oldcolor );                       /* Back to current color */
   pltext( Xpos_mm, Ypos_mm, Angle,
           Justification, tofchar(comment) );
}







static void setCwidth( void )
/*-------------------------------------*/
/* Ask (hidden) linewidth for contours */
/*-------------------------------------*/
{
   fint Clinew;
   fint Nitems = 1;
   fint Dfault = HIDDEN;
   fint R1;

   (void) sprintf( message, "Contour line width [1..21]:   [%d]", (int) linew );
   Clinew = linew;
   R1 = userint_c( &Clinew,
                   &Nitems,
                   &Dfault,
                   KEY_CWIDTH,
                   tofchar(message) );

   cancel_c( KEY_CWIDTH );

   Clinew = MYMIN( MYMAX( 1, Clinew ), 21 );
   pgslw_c( &Clinew );
}




static float maxlablen( char *txt )
/*----------------------------------------------------------*/
/* Determine length of string without '\' characters and    */
/* return the length in mm. 'devsize' is global.            */
/* The default character height is 1/40 of the height or    */
/* width of the view surface (whichever is less). The width */
/* is appr. 0.8 times the height.                           */
/*----------------------------------------------------------*/
{
   int           k, len=0;
   float         Charwidth;
   static float  Maxlen = 0.0;                    /* Static is essential here */

   for (k = 0; k < strlen(txt); k++) {
      if ( (txt[k] != '\\') &&
           (txt[k] != 'u') &&
           (txt[k] != 'd') ) len++;
   }
   pgqch_c( &Charwidth );
   Charwidth *= 0.8 * (float) len * MYMIN(devsize[1], devsize[0]) / 40.0;
   Maxlen = MYMAX( Maxlen, Charwidth );
   return( Maxlen );
}



static float maxlabheight( void )
/*------------------------------------------*/
/* Determine Maximum height of current text */
/*------------------------------------------*/
{
   float  Charheight;
   static float  Maxlen = 0.0;                    /* Static is essential here */

   pgqch_c( &Charheight );
   Charheight *= MYMIN(devsize[1], devsize[0]) / 40.0;
   Maxlen = MYMAX( Maxlen, Charheight );
   return( Maxlen );
}



static  void initframe( fint ns, fint nplot[] )
/*---------------------------------------------------------------*/
/* If this is the first subset in a mosaic or it is an arbitrary */
/* subset not in a mosaic, set the PGPLOT environment.           */
/*---------------------------------------------------------------*/
{
   if ( ( ns == 0 ) || ( nplot[0] == 1 && nplot[1] == 1 ) ) {
      float    x1_win, x2_win, y1_win, y2_win;
      float    x1_inch, x2_inch, y1_inch, y2_inch;
      bool     false = FALSE;

      pgpage_c( );
      x1_win = 0.0;                              /* define PGPLOT environment */
      y1_win = 0.0;
      x2_win = devsize[0];
      y2_win = devsize[1];
      x1_inch = x1_win / inch;
      y1_inch = y1_win / inch;
      x2_inch = x2_win / inch;
      y2_inch = y2_win / inch;
      pgvsiz_c( &x1_inch, &x2_inch, &y1_inch, &y2_inch );
      pgswin_c( &x1_win, &x2_win, &y1_win, &y2_win );
      pgask_c( &false );
   }
}



static bool visual( int ax, int ns )
/*-----------------------------------------------------------------*/
/* Not all axes in a mosaic need tickmarks or labels. This routine */
/* determines which axes.                                          */
/*-----------------------------------------------------------------*/
{
   int    ix, iy;

   if (nplot[0] == 0 && nplot[1] == 0) return( YES );
   ix = ns % nplot[0];
   iy = ns / nplot[0];
   if  ((ax == AXISUPPER) && (iy == 0))          return( YES );
   if  ((ax == AXISLOWER) && (iy == nplot[1]-1)) return( YES );
   if  ((ax == AXISLEFT ) && (ix == 0))          return( YES );
   if  ( (ax == AXISRIGHT) && ((ix == nplot[0]-1) || ((ns+1) == Nsub))) return( YES );
   return( NO );
}



static void drawgrids( fint ns )
/*-------------------------------------------------*/
/* Draw grids for this subset                      */
/*-------------------------------------------------*/
{

   float    X1mm, Y1mm;                        /* Coordinates in mm */
   int      ax;                                /* Axis index */
   int      ndigit;                            /* number of digits in a label */
   float    digit_h, digit_w;                  /* height & width of a digit */
   float    fjust=0.0;                         /* label justification */
   float    tick_mm;                           /* size of tickmark */
   float    x, y;
   float    xlabel=0.0, ylabel=0.0;            /* plot position of label */
   float    xtick=0.0, ytick=0.0;              /* direction of tickmark */
   int      igrid;
   char     text[80];
   int      gridl=0, gridu=0;
   float    xinc=0.0, yinc=0.0;
   int      horizontal, vertical;



   pgqch_c( &digit_h );
   digit_h *= MYMIN(devsize[1], devsize[0]) / 40.0;
   digit_w = 0.8 * digit_h;                     /* Character width  */
   tick_mm = 0.25 * digit_h;

   for (ax = 0; ax < 4; ax++) {
      grid2mm( ns,
               Axno[ax].StartXgrid,
               Axno[ax].StartYgrid,
               &X1mm, &Y1mm );                  /* Start position for the axis */
      x = X1mm;
      y = Y1mm;


      if ( ax == AXISLEFT ) {
         gridu  =  Axno[ax].EndYgrid;
         gridl  =  Axno[ax].StartYgrid;
         xlabel =  tick_mm / 2.0;                              /* label right */
         ylabel =  0.0;
         fjust  =  0.0;                               /* label left justified */
         xtick  =  tick_mm;                                 /* tickmark right */
         ytick  =  0.0;
         xinc   =  0.0;
         yinc   =  1.0 / sc_g[1];
         x     -=  gridmargin[0];
      } else if ( ax == AXISRIGHT ) {
         gridu  =  Axno[ax].EndYgrid;
         gridl  =  Axno[ax].StartYgrid;
         xlabel = -1.0;                                         /* label left */
         ylabel =  0.0;
         fjust  =  1.0;                              /* label right justified */
         xtick  = -tick_mm;                                  /* tickmark left */
         ytick  =  0.0;
         xinc   =  0.0;
         yinc   =  1.0 / sc_g[1];
         x     +=  gridmargin[0];
      } else if ( ax == AXISLOWER ) {
         gridu  =  Axno[ax].EndXgrid;
         gridl  =  Axno[ax].StartXgrid;
         ylabel =  2.0 * tick_mm;                                 /* label up */
         xlabel =  0.0;
         ytick  =  tick_mm;                                    /* tickmark up */
         xtick  =  0.0;
         fjust  =  0.5;                                     /* label centered */
         xinc   =  1.0 / sc_g[0];
         yinc   =  0.0;
         y     -=  gridmargin[1];
      } else if ( ax == AXISUPPER ) {
         gridu  =  Axno[ax].EndXgrid;
         gridl  =  Axno[ax].StartXgrid;
         ylabel = -tick_mm - digit_h;                           /* label down */
         xlabel =  0.0;
         ytick  = -tick_mm;                                  /* tickmark down */
         xtick  =  0.0;
         fjust  =  0.5;                                     /* label centered */
         xinc   =  1.0 / sc_g[0];
         yinc   =  0.0;
         y     +=  gridmargin[1];
      }

      {
         int n1, n2;
         n1 = sprintf( message, "%d", gridl );       /* Max length for format */
         n2 = sprintf( message, "%d", gridu );
         ndigit = MYMAX( n1, n2 );
      }

      horizontal = ((ax == 0) || (ax == 2));
      vertical   = ((ax == 1) || (ax == 3));

      if (visual( ax, ns )) {
         plmove( x, y );
         /*-----------------------------------------------*/
         /* Loop over all grids and label each tenth grid */
         /*-----------------------------------------------*/
         for( igrid = gridl; igrid <= gridu; igrid++ ) {
            if ( igrid % 10 == 0 ) {

               (void) sprintf( text, "%*d", ndigit, igrid );
               pldraw( x + xtick, y + ytick );
               pltext( x + xlabel, y + ylabel, 0.0, fjust, tofchar( text ) );
            } else {
               if ( vertical ) {                             /* plot tickmark */
                  pldraw( x + ( xtick / 2 ), y + ytick );
               } else {
                  pldraw( x + xtick, y + ( ytick / 2 ) );
               }
            }
            x += xinc;
            y += yinc;
            plmove( x, y );
         }
      }
   }
/*   pgsch_c( &orig_size ); */
}



static void hms1( double Seconds, char *postxt, int first, double Step )
/*-----------------------------------------------------------------*/
/* Convert a number that is already in time seconds, in h,m,s, and */
/* return formatted number in string 'postxt'                      */
/*-----------------------------------------------------------------*/
{
   int           hour, min, sec;
   static int    oldhour, oldmin, oldsec;
   int           ndigit;
   int           n;

   hour = (int) Seconds / 3600.0;
   Seconds -= (double) hour*3600.0;
   min = (int) Seconds / 60.0;
   Seconds -= (double) min*60.0;
   sec = (int) Seconds;


   if (first) {
      oldhour = hour;
      oldmin = min;
      oldsec = sec;
   }
   ndigit = prec(Step);

   /* Format: 16h23m18.342s */
   if (first) {
      n = sprintf( postxt, "%2d\\uh\\d%2d\\um\\d%.*f\\us\\d" ,
                            hour , min, ndigit, Seconds );
   } else {
      bool   prhour, prmin, prsec;

      prhour = (hour != oldhour);
      if (!prhour) {
         prmin  = (min != oldmin);
         prsec = YES;
      } else {
         prmin  = YES;
         if (sec == 0.0) {
            prsec = NO;
         } else {
            prsec = YES;
         }
      }
      (void) sprintf( postxt, "" ); /* empty */
      if (prhour) {
         (void) sprintf( postxt, "%.*s%d\\uh\\d", strlen(postxt), postxt, hour );
         oldhour = hour;
      }
      if (prmin) {
         (void) sprintf( postxt, "%.*s%2d\\um\\d",strlen(postxt), postxt, min );
         oldmin = min;
      }
      if (prsec) {
      (void) sprintf( postxt, "%.*s%.*f\\us\\d",strlen(postxt), postxt,
               ndigit, Seconds );
      }
   }
}


static void dms1(  double Seconds, char *postxt, int first, double Step )
/*-----------------------------------------------------------------*/
/* Convert a number that is already in arc seconds, in d,m,s, and  */
/* return formatted number in string 'postxt'                      */
/*-----------------------------------------------------------------*/
{
   int     deg, min, sec;
   static  int   olddeg, oldmin, oldsec, oldsign;
   int     ndigit = 0;
   int     negative = NO;
   int     n;
   int     arti, verschil;

   if( Seconds < 0.0 ){ negative = YES ; Seconds = -Seconds ; }
   deg = (int) Seconds / 3600.0;
   Seconds -= (double) deg*3600.0;
   min = (int) Seconds / 60.0;
   Seconds -= (double) min*60.0;
   sec = (int) Seconds;


   if (first) {
      olddeg  = deg;
      oldmin  = min;
      oldsec  = sec;
      oldsign = negative;
   }

   Step = fabs(Step);
   ndigit = prec(Step);

   if (negative) {
      (void) sprintf( postxt, "-" );
   } else {
      (void) sprintf( postxt, "" );
   }

   /* Format: -311o23'18.342'' */
   if (first || (oldsign != negative)) {
      if (Step < 60.0) {
         n = sprintf( postxt, "%.*s%d\\uo\\d%2d\\u'\\d%.*f\\u\"\\d",
                      strlen(postxt), postxt,
                      deg , min, ndigit, Seconds );
      } else {
         if (Step < 3600.0) {
            n = sprintf( postxt, "%.*s%d\\uo\\d%2d\\u'\\d",
                         strlen(postxt), postxt,
                         deg , min );
         } else {
            n = sprintf( postxt, "%.*s%d\\uo\\d",
                         strlen(postxt), postxt, deg );
         }
      }
      if (!first) {
         olddeg  = deg;
         oldmin  = min;
         oldsec  = sec;
         oldsign = negative;
      }
   } else {
      if (deg != olddeg) {
         (void) sprintf( postxt, "%.*s%d\\uo\\d", strlen(postxt), postxt, deg );
         olddeg = deg;
      }
      else
      {
         if (negative == oldsign)
            (void) sprintf( postxt, "" );  /* Get rid of "-" sign */
      }
      if (min != oldmin) {
         (void) sprintf( postxt, "%.*s%2d\\u'\\d",strlen(postxt), postxt, min );
         oldmin = min;
      }

      arti = (int) (Step / 60.0);
      verschil = Step - (double) arti * 60.0;

      if ( (Step < 60.0) || (fabs(verschil) != 0.0) ) {
         (void) sprintf( postxt, "%.*s%.*f\\u\"\\d",
                                   strlen(postxt),
                                   postxt,
                                   ndigit, Seconds );
      }
   }
   oldsign = negative;
}


static void dmsoffset( double Seconds, char *postxt, double Step )
/*-----------------------------------------------------------------*/
/* Convert a number that is already in arc seconds, in d,m,s, and  */
/* return formatted number in string 'postxt'                      */
/*-----------------------------------------------------------------*/
{
   int     ndigit = 0;

   ndigit = prec(Step);
   Step   = fabs(Step);

   if  ( ((int)Step % 60) == 0) {
      /* Minutes instead of seconds */
      (void) sprintf( postxt, "%+d\\u'\\d",
               (int) (Seconds / 60.0)  );
   } else {
      (void) sprintf( postxt, "%+.*f\\u\"\\d",
               ndigit, Seconds );
   }
}


static void drawbeam( fint ns )
/*---------------------------------------------------------------*/
/* Plot an ellipse in the plot, representing the a beam given in */
/* seconds of arc.                                               */
/*---------------------------------------------------------------*/
{
   double   bmpa;
   double   bmmin, bmmaj ;
   float    delta, slope;
   fint     Nitems = 1;
   fint     r1, r2, r3;
   float    x, y;
   float    major, minor, angle;
   fint     setlevel = 0;
   double   cdeltvert, cdelthorz;
   double   PAmap;
   double   majmin[2];
   int      beamerase;
   fint     beamshape;
   float    X1mm, X2mm, Y1mm, Y2mm;



   Dfault = HIDDEN;
   bmmin = 0.0;
   r1 = 0;
   gdsd_rdble_c( Setname,
                 tofchar( "BMMIN" ),
                 &Subset[ns],
                 &bmmin,
                 &r1 );

   bmmaj = 0.0;
   r2 = 0;
   gdsd_rdble_c( Setname,
                 tofchar( "BMMAJ" ),
                 &Subset[ns],
                 &bmmaj,
                 &r2 );
   r3 = 0;
   bmpa = 0.0;
   gdsd_rdble_c( Setname,
                 tofchar( "BMPA" ),
                 &Subset[ns],
                 &bmpa,
                 &r3 );

   if ( (r1 < 0) || (r2 < 0) || (!Spatialmap) ) {
      Dfault = REQUEST;
   } else {
      Dfault = HIDDEN;
   }
   majmin[0] = bmmaj;
   majmin[1] = bmmin;
   if (Spatialmap) {
      (void) sprintf( message,
              "Beam major and minor axis FWHM: [%f,%f] arcsec.",
               bmmaj,
               bmmin );
   } else {
      char unitsmaj[20], unitsmin[20];
      majmin[0] = fabs(Cdelt[1]);
      majmin[1] = fabs(Cdelt[0]);
      if (strstr( Cunit[0].a, "DEGREE" )) {
         strcpy( unitsmin, "ARCSEC" );
         majmin[1] *= 3600.0;
      } else {
         (void) sprintf( unitsmin, "%.*s", nelc_c( Cunit[0] ), Cunit[0].a );
      }
      if (strstr( Cunit[1].a, "DEGREE" )) {
         strcpy( unitsmaj, "ARCSEC" );
         majmin[0] *= 3600.0;
         strcpy( unitsmaj, "ARCSEC" );
      } else {
         (void) sprintf( unitsmaj, "%.*s", nelc_c( Cunit[1] ), Cunit[1].a );
      }

      (void) sprintf( message,
              "Maj and min bars:  [%f,%f] %s, %s",
               majmin[0],
               majmin[1],
               unitsmaj, unitsmin );
      if (strstr( Cunit[0].a, "DEGREE" )) majmin[1] /= 3600.0;
      if (strstr( Cunit[1].a, "DEGREE" )) majmin[0] /= 3600.0;
   }
   Nitems = 2;
   r1 = userdble_c( majmin,
                    &Nitems,
                    &Dfault,
                    KEY_BEAM,
                    MES_BEAM
                  );
   bmmaj = majmin[0];
   bmmin = majmin[1];
   if ( (r3 < 0) || (!Spatialmap) ) {
      Dfault = REQUEST;
   } else {
      Dfault = HIDDEN;
   }
   if (Spatialmap) {
      (void) sprintf( message,
              "Position angle of beam:   [%f] degrees",
               bmpa );
   } else {
      (void) sprintf( message,
              "Position angle of bars:    [%f] degrees",
               bmpa );
   }
   Nitems = 1;
   r1 = userdble_c( &bmpa,
                    &Nitems,
                    &Dfault,
                    KEY_BEAMPA,
                    MES_BEAMPA
                  );

   delta = 1.0;
   slope = 30.0;
   beamerase = NO;
   beamshape = 1;
   {  float atts[4];
      Nitems = 4;
      atts[0] =  (float) beamerase;
      atts[1] =  delta;
      atts[2] =  slope;
      atts[3] =  beamshape;
      r1 = userreal_c( atts,
                       &Nitems,
                       &Dfault,
                       KEY_BEAMATT,
                       MES_BEAMATT
                      );
      beamerase = (int)  atts[0];
      delta     =        atts[1];
      slope     =        atts[2];
      beamshape = (fint) atts[3];
   }
   (void) sprintf( message, "CROTA%d", Axperm[1] );
   r1 = 0;
   gdsd_rdble_c( Setname, tofchar(message), &setlevel, &PAmap, &r1 );
   if (r1 < 0) {
      /* No crota in header, assume 0.0 */
      PAmap = 0.0;
   }


   angle = (float) bmpa - PAmap;                         /* Correct for crota */
   angle += 90.0;                                            /* Now wrt North */
   (void) sprintf( message, "CDELT%d", Axperm[0] );
   r1 = 0;
   gdsd_rdble_c( Setname, tofchar(message), &setlevel, &cdelthorz, &r1 );
   (void) sprintf( message, "CDELT%d", Axperm[1] );
   r1 = 0;
   gdsd_rdble_c( Setname, tofchar(message), &setlevel, &cdeltvert, &r1 );

   /*------------------------------------------------------------*/
   /* The major and minor axes of the beam are read from the     */
   /* header and are in seconds of arc. They indicate the FWHM   */
   /* so to convert to major and minor axis divide by 2. To      */
   /* convert to degrees, divide by 3600. The conversion to      */
   /* grids is done with cdelt from the header. This value is    */
   /* in degrees, so the ratio is in grids. Finally there is     */
   /* a conversion to world coordinates with sc_g.               */
   /*------------------------------------------------------------*/

   if (Spatialmap) {
      major = (float) fabs( ((bmmaj/7200.0) / cdeltvert ) / sc_g[1] );
      minor = (float) fabs( ((bmmin/7200.0) / cdelthorz ) / sc_g[0] );
   } else {
      major = (float) fabs( ((bmmaj/2.0) / cdeltvert) / sc_g[1] );
      minor = (float) fabs( ((bmmin/2.0) / cdelthorz) / sc_g[0] );
   }

   grid2mm( ns, bmpos[0], bmpos[1], &x, &y );

   X1mm = x - MYMAX(major, minor) - 1.0;
   X2mm = x + MYMAX(major, minor) + 1.0;
   Y1mm = y - MYMAX(major, minor) - 1.0;
   Y2mm = y + MYMAX(major, minor) + 1.0;
   if (beamerase) {
      fint   Oldc, Newc = 0;     /* Background */
      fint   Solid = 1;

      if (Toscreen) {
          Solid = 2;
      } else {
          Solid = 1;
           pgqci_c( &Oldc );
          pgsci_c( &Newc );
      }
      pgsfs_c( &Solid );                        /* Set fill area style */
      pgrect_c( &X1mm, &X2mm, &Y1mm, &Y2mm );
      pgsci_c( &Oldc );                                            /* Restore */
   }

   if (Spatialmap)
   {
      /*------------------------------------------------------------*/
      /* Note that this part of the code previously consisted of    */
      /* the pgbeam routine only. However this routine did not      */
      /* suffice in situations with grid aspect ratio's unequal to  */
      /* zero. Therefore it was replaced by the 'beam' routine.     */
      /* This routine needs a screen window in grids and we have to */
      /* transform from mm to grids first.                          */
      /*------------------------------------------------------------*/      
      fint   r;
      fint   Spatial = 1;
      double Cdelt[2];
      double currentXY[2];
      double Slope = (double) slope;
      fint   lines = (fint) ( ABS(Y2mm-Y1mm) / delta);
      float  x1_win, x2_win, y1_win, y2_win;
      float  x1mm, x2mm, y1mm, y2mm;
      float  x1vp, x2vp, y1vp, y2vp;      
      double mina, maja;
     
      Cdelt[0] = cdelthorz;
      Cdelt[1] = cdeltvert;
      currentXY[0] = bmpos[0];
      currentXY[1] = bmpos[1];
      
      /*------------------------------------------------------------*/
      /* Find the positions in mm that correspond to the current    */
      /* box. Set viewport on this part of the device and set a     */
      /* window (equal to the box) on this viewport. Then call the  */
      /* beam routine and set the viewport back to the entire       */
      /* device.                                                    */
      /*------------------------------------------------------------*/
      x1_win = Blo[0]; x2_win = Bhi[0];
      y1_win = Blo[1]; y2_win = Bhi[1];
      grid2mm( ns, Blo[0], Blo[1], &x1mm, &y1mm );
      grid2mm( ns, Bhi[0], Bhi[1], &x2mm, &y2mm );
      x1vp = x1mm / devsize[0];
      x2vp = x2mm / devsize[0];
      y1vp = y1mm / devsize[1];
      y2vp = y2mm / devsize[1];
      pgsvp_c( &x1vp, &x2vp, &y1vp, &y2vp );
      pgswin_c( &x1_win, &x2_win, &y1_win, &y2_win );
      mina = bmmin / 3600.0;
      maja = bmmaj / 3600.0;
      r = beam_c( Setname, &Subset[ns],
                  &Spatial,
                  Cdelt,
                  currentXY,
                  &maja, &mina,
                  &bmpa,
                  &lines,
                  &Slope,
                  &beamshape );
      
      /* Reset the viewport and window to entire device in mm */
      x1vp = 0.0;
      x2vp = 1.0;
      y1vp = 0.0;
      y2vp = 1.0;
      pgsvp_c( &x1vp, &x2vp, &y1vp, &y2vp );     
      x1_win = 0.0;                              /* define PGPLOT environment */
      y1_win = 0.0;
      x2_win = devsize[0];
      y2_win = devsize[1];      
      pgswin_c( &x1_win, &x2_win, &y1_win, &y2_win );
            
/*      pgbeam_c( &x, &y, &major, &minor, &angle, &delta, &slope, &beamshape );*/
   }
   else
   {
      float   xr, yr;
      double  Rads;

      Rads = (double) angle * PI / 180.0;

      xr = -major*cos(Rads) - 0.0*sin(Rads);
      yr = -major*sin(Rads) + 0.0*cos(Rads);
      plmove( xr + x, yr + y );
      xr =  major*cos(Rads) - 0.0*sin(Rads);
      yr =  major*sin(Rads) + 0.0*cos(Rads);
      pldraw( xr + x, yr + y );
      xr = 0.0*cos(Rads) - (-minor)*sin(Rads);
      yr = 0.0*sin(Rads) + (-minor)*cos(Rads);
      plmove( xr + x, yr + y );
      xr = 0.0*cos(Rads) - (minor)*sin(Rads);
      yr = 0.0*sin(Rads) + (minor)*cos(Rads);
      pldraw( xr + x, yr + y );
   }

   (void) sprintf( message, "%f %f mm, x0=%f, y0=%f", major, minor,x,y );
}





static void drawunits( int ns )
/*----------------------------------------------------------*/
/* Draw major and minor tickmarks and label major tickmarks */
/* Special care if one of the CROTA's <> 0                  */
/*----------------------------------------------------------*/
{

#define INSIDE  (int) ( ( (horizontal) &&                          \
                    (Xgrid >= (double) Axno[ax].StartXgrid) &&     \
                    (Xgrid <= (double) Axno[ax].EndXgrid) )        \
                  ||                                               \
                  ( (vertical) &&                                  \
                    (Ygrid >= (double) Axno[ax].StartYgrid) &&     \
                    (Ygrid <= (double) Axno[ax].EndYgrid) ) )

   int        ax;
   double     Start, End;
   int        Totime, Toarcs;
   double     Delta[4], Step[4], Dist;
   int        n;
   double     Tickstart;
   double     Xphys, Yphys;
   double     Xphys2, Yphys2;
   double     Xgrid, Ygrid;
   float      flStep;
   float      divs;
   float      parts;
   fint       Minor;
   int        horizontal, vertical;
   float      Xmm, Ymm;
   double     TheseUnits;
   char       postxt[80];                  /* Text with position of maj. tick */
   float      digit_h, digit_w;                  /* height & width of a digit */
   float      xlabel=0.0, ylabel=0.0;               /* plot position of label */
   float      xtick=0.0, ytick=0.0;                  /* direction of tickmark */
   float      fjust=0.0;                               /* label justification */
   float      xoff= 0.0, yoff = 0.0;
   double     Minordist = 0.0;
   double     MinorstartX, MinorstartY;
   int        inbox;
   int        first;
   double     Xcent, Ycent;
   int        ndigit;
   double     StartXgrid = 0.0, StartYgrid = 0.0;
   fint       Mode;
   fint       R1;
   float      Offset, Sign;
   bool       Exit;
   int        newsky;
   fint       Skysys = 2;
   int        plotunits;
   int        plotgrids;
   int        plotticks;
   float      tick_mm;



   pgqch_c( &digit_h );
   digit_h *= MYMIN(devsize[1], devsize[0]) / 40.0;
   digit_w = 0.8 * digit_h;                               /* Character width  */
   tick_mm = 0.5 * digit_h;


   for (ax = 0; ax < 4; ax++) {                           /* Loop over 4 axes */
      if (Axmask[ax]) {
         if (Axmask[8]) {
            plotticks = YES;
         } else {
            if (visual(ax, ns)) plotticks = YES; else plotticks = NO;
         }
      } else {
         plotticks = NO;
      }
      plotunits = (   Axmask[ax+4]
                   && visual(ax, ns)
                   && (Axno[ax].Tophys)
                   && plotticks);
      plotgrids = (grids && visual(ax,ns));

      /*---------------------------------------------------*/
      /* Plot units according to Axmask[ax+4] and 'visual' */
      /* Plot tick marks according 'visual' or Axmask[ax]  */
      /* and Axmask[8].                                    */
      /*---------------------------------------------------*/
      if ( plotticks ) {
         if ( ax == AXISLEFT ) {
            if ( !plotgrids ) {
               xtick  = tick_mm;                            /* tickmark right */
            } else {
               xtick  = -tick_mm;                            /* tickmark left */
            }
            ytick = 0.0;
            xlabel = -tick_mm;                                  /* label left */
            ylabel = -.5 * digit_h;
            fjust = 1.0;                             /* label right justified */
            xoff = -gridmargin[0];                     /* Plot things on axes */
            yoff = 0.0;
         } else if ( ax == AXISRIGHT ) {
            if ( !plotgrids ) {
               xtick  = -tick_mm;                            /* tickmark left */
            } else {
               xtick  = tick_mm;                            /* tickmark right */
            }
            ytick = 0.0;
            xlabel = tick_mm;                                  /* label right */
            ylabel = -.5 * digit_h;
            fjust = 0.0;                              /* label left justified */
            xoff = +gridmargin[0];                     /* Plot things on axes */
            yoff = 0.0;
         } else if ( ax == AXISLOWER ) {
            if ( !plotgrids ) {
               ytick  = tick_mm;
            } else {
               ytick  = -tick_mm;
            }
            xtick = 0.0;
            xlabel = 0;                                         /* label down */
            ylabel = -tick_mm - digit_h;
            fjust = 0.5;                                    /* label centered */
            xoff = 0.0;
            yoff = -gridmargin[1];
         } else if ( ax == AXISUPPER ) {
            if ( !plotgrids ) {
               ytick  = -tick_mm;
            } else {
               ytick  = tick_mm;
            }
            xtick = 0.0;
            xlabel = 0;                                           /* label up */
            ylabel = tick_mm;
            fjust = 0.5;                                    /* label centered */
            xoff = 0.0;
            yoff = +gridmargin[1];
         }


         horizontal = ((ax == 0) || (ax == 2));
         vertical   = ((ax == 1) || (ax == 3));
         newsky = (Skysys != Axno[ax].Skysystem);
         newsky = NO;

         if (horizontal && (tick[0] == 0.0)) return;
         if (vertical && (tick[1] == 0.0)) return;

         Totime = ((Axno[ax].Type == 1) && ((Axno[ax].Skysystem) == 1) &&
                   !Axno[ax].rotated);
         Toarcs = ((Axno[ax].Type == 2) && ((Axno[ax].Skysystem) == 1) &&
                   !Axno[ax].rotated);
         if (!Totime) {
            Toarcs = (Toarcs ||
                      (horizontal && (strstr( Cunit[0].a, "DEGREE" ))) ||
                      (vertical   && (strstr( Cunit[1].a, "DEGREE" ))) );
         }

         if  (horizontal) {
              Start = Axno[ax].StartXphys; End = Axno[ax].EndXphys;
              if (newsky) {
                 double   Dummy;
                 Totime = Toarcs = NO;
                 (void) sprintf( message, "in:start=%f end=%f", Start, End );
                 anyoutC( 3, message );
                 R1 = skyco_c( &Start, &Axno[ax].StartYphys,
                               &Axno[ax].Skysystem,
                               &Start, &Dummy,
                               &Skysys );
                 R1 = skyco_c( &End, &Axno[ax].EndYphys,
                               &Axno[ax].Skysystem,
                               &End, &Dummy,
                               &Skysys );
                 (void) sprintf( message, "out: start=%f end=%f", Start, End );
                 anyoutC( 3, message );
              }


         } else { /* i.e. vertical */
            Start = Axno[ax].StartYphys; End = Axno[ax].EndYphys;
         }
         if (Start == End) Start += Start / 10.0;
         /* This axis could be a RA-axis, convert Delta to time seconds then */
         if (Totime) {
            /* spatial longitude in equatorial system */
            Start *= 240.0; End *= 240.0;
         } else if (Toarcs) {
            /* spatial latitude in equatorial system or header units in DEGREE */
            Start *= 3600.0; End *= 3600.0;
         }

         Delta[ax] = End - Start;
         if (Axno[ax].rotated) {
            if (horizontal) Delta[ax] = fabs((Bhi[0]-Blo[0]+1)*Cdelt[0]*3600.0);
            if (vertical)   Delta[ax] = fabs((Bhi[1]-Blo[1]+1)*Cdelt[1]*3600.0);
         }

         /* Calculate a reasonable tick mark sep. for the major tickmarks */

         if ( (horizontal && (tick[0] == Blank)) ||
              (vertical   && (tick[1] == Blank)) ) {
            if (Axno[ax].rotated) parts = 4.0; else parts = 2.0;
            divs = 2.0;
            do {
               Step[ax]   = Delta[ax] / divs;
               flStep = (float) Step[ax];
               Step[ax]   = (double) pgrnd_c( &flStep, &Minor );
               if (Toarcs || Totime || Axno[ax].rotated) {
                  int k;
                  k = (int) Step[ax]/3600.0;
                  if (fabs(k) >= 1) {
                     Step[ax] = (double) k * 3600.0;
                  } else {
                     k = (int) Step[ax]/60.0;
                     if (fabs(k) >= 1) Step[ax] = (double) k * 60.0;
                  }
               }
               divs  += 1.0;
            } while ( fabs(Delta[ax]/Step[ax]) <= parts );
         } else {                                       /* User defined steps */
            if (horizontal) Step[ax] = (double) tick[0];
            if (vertical)   Step[ax] = (double) tick[1];
            if (Start > End) Step[ax] *= -1.0;
            if (Axno[ax].rotated) Step[ax] = fabs(Step[ax]);
            flStep = (float) Step[ax];
             pgrnd_c( &flStep, &Minor );
         }

         /* Adjust Minors for spatials */

         if (Totime || Toarcs || Axno[ax].rotated) {
            int   k;
            float Newstep = fabs(Step[ax]);
            if (((int) Newstep % 60) == 0 ) {
               Newstep /= 60.0;
               if (((int) Newstep % 60) == 0 ) {
                  Newstep /= 60.0;
               }
            }
            k = (fint) Newstep;
            if (k == 1) k = 2;
            Minor = k;
            if ((k % 4) == 0) {
               Minor = 4;
            } else if ((k % 5) == 0) {
               Minor = 5;
            } else if ((k % 3) == 0) {
               Minor = 3;
            } else if ((k % 2) == 0) {
               Minor = 2;
            }
            if (Minor > 23) Minor = 0;
         }



         /*-----------------*/
         /* Minor tickmarks */
         /*-----------------*/

         if (horizontal) {
            if (nminor[0] <  0 ) Minordist = Step[ax] / (double) (Minor); /* Default, calculate values */
            if (nminor[0] == 0 ) Minordist = 0.0;
            if (nminor[0] >  0 ) Minordist = Step[ax] / (double) (nminor[0]+1);
         } else {
            if (nminor[1] <  0 ) Minordist = Step[ax] / (double) (Minor); /* Default, calculate values */
            if (nminor[1] == 0 ) Minordist = 0.0;
            if (nminor[1] >  0 ) Minordist = Step[ax] / (double) (nminor[1]+1);
         }


         /*-----------------------------------------------------------*/
         /* Special care for spatial axes (type 1&2) that came from a */
         /* rotated image                                             */
         /*-----------------------------------------------------------*/

         if (Axno[ax].rotated) {
            /* Get reasonable values for projection centre in grids */
            /* Is 0 inside axis range? */
            if ((Bhi[0] > 0) && (Blo[0] < 0)) Xcent = 0.0;
            else {
               Xcent = ( (double)Bhi[0] + (double)Blo[0] + 1.0 ) / 2.0;
            }
            if ((Bhi[1] > 0) && (Blo[1] < 0)) Ycent = 0.0;
            else {
               Ycent = ( (double)Bhi[1] + (double)Blo[1] + 1.0 ) / 2.0;
            }

            /* Put these offsets along the axes */
            if (ax==0) { Xgrid = Xcent;  Ygrid = Blo[1];}
            if (ax==1) { Xgrid = Bhi[0]; Ygrid = Ycent;}
            if (ax==2) { Xgrid = Xcent;  Ygrid = Bhi[1];}
            if (ax==3) { Xgrid = Blo[0]; Ygrid = Ycent;}

            grid2mm( ns, Xgrid, Ygrid, &Xmm, &Ymm );
            /* For this axis we have a central position on that axis */
            /* in mm with a physical value. Plot along the axis the  */
            /* relevant value */
            Xmm += xoff; Ymm += yoff;
            plmove( Xmm, Ymm );
            pldraw( Xmm + xtick, Ymm + ytick );            /* draw major tick */
            ndigit = prec(Step[ax]);

            (void) sprintf( postxt, "0" );

            if (plotunits) {
               dmsoffset( 0.0, postxt, Step[ax] );
               /* Write label only if mask is set to Y */
               pltext( Xmm+xlabel, Ymm+ylabel, 0.0, fjust, tofchar(postxt) );
            }
            /* Other major tickmarks are 'Step' physical units away */

            if (horizontal) {
               StartXgrid = Xgrid;
            } else {
               StartYgrid = Ygrid;
            }

            Dist   = 0.0;
            Sign   = 1.0;
            Offset = 0.0;
            first  = YES;
            while ( Dist <= fabs(Delta[ax]+Step[ax]) ) {
               if (horizontal) {
                  Ygrid = Ycent;
                  Xgrid += Sign * Step[ax] / (Cdelt[0]*3600.0);
               } else {
                  Xgrid = Xcent;
                  Ygrid += Sign * Step[ax] / (Cdelt[1]*3600.0);
               }

               if (INSIDE) {
                  /* Put these offsets along the axes */
                  if (ax==0) { Ygrid = Blo[1];}
                  if (ax==1) { Xgrid = Bhi[0];}
                  if (ax==2) { Ygrid = Bhi[1];}
                  if (ax==3) { Xgrid = Blo[0];}

                  grid2mm( ns, Xgrid, Ygrid, &Xmm, &Ymm );
                  Xmm += xoff; Ymm += yoff;
                  plmove( Xmm, Ymm );
                  pldraw( Xmm + xtick, Ymm + ytick );
                  Offset += Sign * Step[ax];
                  if (plotunits) {
                     dmsoffset( Offset, postxt, Step[ax] );
                     pltext( Xmm+xlabel, Ymm+ylabel, 0.0, fjust,
                             tofchar( postxt ) );
                     if (ax == 3) {
                        label_len = maxlablen( postxt );
                     }
                     if (ax == 0) {
                        label_height = maxlabheight();
                     }
                     first = NO;
                  }
               }
               else {
                  Sign = -1.0;
                  Offset = 0.0;
                  if (horizontal) {
                     Xgrid = StartXgrid;
                  } else {
                     Ygrid = StartYgrid;
                  }
               }
               Dist += fabs(Step[ax]);
            }

            if (Minordist != 0.0) {
               /* Minors */
               Dist     = 0.0;
               Sign     = 1.0;
               while ( Dist <= fabs(Delta[ax]+Step[ax]) ) {
                  if (horizontal) {
                     Ygrid = Ycent;
                     Xgrid += Sign * Minordist / (Cdelt[0]*3600.0);
                  } else {
                     Xgrid = Xcent;
                     Ygrid += Sign * Minordist / (Cdelt[1]*3600.0);
                  }
                  if (INSIDE) {
                     /* Put these offsets along the axes */
                     if (ax==0) { Ygrid = Blo[1];}
                     if (ax==1) { Xgrid = Bhi[0];}
                     if (ax==2) { Ygrid = Bhi[1];}
                     if (ax==3) { Xgrid = Blo[0];}

                     grid2mm( ns, Xgrid, Ygrid, &Xmm, &Ymm );
                     Xmm += xoff; Ymm += yoff;
                     plmove( Xmm, Ymm );
                     pldraw( Xmm + xtick/2, Ymm + ytick/2 );
                  }
                  else {
                     Sign = -1.0;
                     if (horizontal) Xgrid = StartXgrid;
                     else            Ygrid = StartYgrid;
                  }
                  Dist += fabs(Minordist);
               }
            }

         } else {
            /*--------------------------------------------------------*/
            /* Unrotated image. If the system is spatial, use 'proco' */
            /* instead of 'cotrans'.                                  */
            /*--------------------------------------------------------*/
            /* Calculate the start position of the major ticks */
            n = (int) (Start/Step[ax]);
            Tickstart = n * Step[ax];
            if (horizontal) {
               Xphys = Tickstart;
               Yphys = Axno[ax].StartYphys;
            } else {
               Yphys = Tickstart;
               Xphys = Axno[ax].StartXphys;
            }

            if ((ax == 2) && (visual(0, ns))) {
               Step[ax]  = MYMAX(Step[ax], Step[0]);
            }
            if ((ax == 3) && (visual(1, ns))) {
               Step[ax]  = MYMAX(Step[ax], Step[1]);
            }

            MinorstartX = Xphys;
            MinorstartY = Yphys;

            Dist = 0.0;
            first = YES;
            while ( Dist <= fabs(Delta[ax]+Step[ax]) ) {
               Xphys2 = Xphys;
               Yphys2 = Yphys;
               if (Totime) {
                  if (horizontal) Xphys2 /= 240.0;
                  if (vertical)   Yphys2 /= 240.0;
               }
               if (Toarcs) {
                  if (horizontal) Xphys2 /= 3600.0;
                  if (vertical)   Yphys2 /= 3600.0;
               }
               if (Spatialmap) {
                  double    Zeroangle = 0.0;
                  double  Xoff = Crpix[0] - NINT(Crpix[0]);
                  double  Yoff = Crpix[1] - NINT(Crpix[1]);
                  /* Put these offsets along the axes */
                  if (ax==0) { Ygrid = Blo[1]; }
                  if (ax==1) { Xgrid = Bhi[0]; }
                  if (ax==2) { Ygrid = Bhi[1]; }
                  if (ax==3) { Xgrid = Blo[0]; }
                  if (horizontal) {
                     Mode = 2;
                     Ygrid -=  Yoff;
                     R1 = proco_c( &Xphys2, &Ygrid,
                                   &Xgrid,  &Yphys2,
                                   &Crval[0], &Crval[1],
                                   &Cdelt[0], &Cdelt[1],
                                   &Zeroangle,
                                   &Axno[ax].Prosystem,
                                   &Mode );
                     Xgrid += Xoff;
                     Ygrid += Yoff;
                  } else {
                     Mode = 1;
                     Xgrid -=  Xoff;
                     R1 = proco_c( &Xgrid, &Yphys2,
                                   &Xphys2, &Ygrid,
                                   &Crval[0], &Crval[1],
                                   &Cdelt[0], &Cdelt[1],
                                   &Zeroangle,
                                   &Axno[ax].Prosystem,
                                   &Mode );
                     Xgrid += Xoff;
                     Ygrid += Yoff;
                  }
               } else {
                   phys2grid( Xphys2, Yphys2, &Xgrid, &Ygrid, ns );
               }
               /* Is this position within axis range? */
               if ( INSIDE ) {
                  if (first) {
                     MinorstartX = Xphys;
                     MinorstartY = Yphys;
                  }
                  /* Small correction */
                  if (ax==0) { Ygrid = Blo[1]; }
                  if (ax==1) { Xgrid = Bhi[0]; }
                  if (ax==2) { Ygrid = Bhi[1]; }
                  if (ax==3) { Xgrid = Blo[0]; }
                  grid2mm( ns, Xgrid, Ygrid, &Xmm, &Ymm );
                  Xmm += xoff; Ymm += yoff;
                  plmove( Xmm, Ymm );
                  pldraw( Xmm + xtick, Ymm + ytick );      /* draw major tick */
                  if (horizontal) TheseUnits = Xphys; else TheseUnits = Yphys;
                  if (Totime) {
                      hms1( TheseUnits, postxt, first, Step[ax] );
                  } else if (Toarcs) {
                      dms1( TheseUnits, postxt, first, Step[ax] );
                  } else {
                     ndigit = prec(Step[ax]);
                     (void) sprintf( postxt, "%.*f", ndigit,  TheseUnits );
                  }
                  if (plotunits) {
                     /* Write label only if mask is set to Y */
                     pltext( Xmm + xlabel, Ymm + ylabel, 0.0, fjust,
                             tofchar( postxt ) );

                     if (ax == 3) {
                        label_len = maxlablen( postxt );
                     }
                     if (ax == 0) {
                        label_height = maxlabheight();
                     }
                  }
                  first = NO;

                  /*-----------------------------*/
                  /* Here is a starting point to */
                  /* draw a grid overlay         */
                  /*-----------------------------*/
                  if (Extendticks && ((ax == 2) || (ax == 3))) {
                     double Step = 0.0;
                     double Xphys3, Yphys3;
                     bool   inside;
                     Xphys3 = Xphys2;
                     Yphys3 = Yphys2;
                     phys2grid( Xphys2, Yphys2, &Xgrid, &Ygrid, ns );
                     grid2mm( ns, Xgrid, Ygrid, &Xmm, &Ymm );
                     plmove( Xmm, Ymm );
                     if (horizontal)
                        Step = (Axno[1].EndYphys - Axno[1].StartYphys) / (double) Linesteps;
                     if (vertical)
                        Step = (Axno[0].EndXphys - Axno[0].StartXphys) / (double) Linesteps;
                     Sign = 1.0;
                     Exit = NO;
                     do {
                        if (horizontal) Yphys3 += Sign * Step;
                        if (vertical)   Xphys3 += Sign * Step;
                        phys2grid( Xphys3, Yphys3, &Xgrid, &Ygrid, ns );
                        inside = ((Xgrid >= Blo[0]) && (Xgrid <= Bhi[0]) &&
                                  (Ygrid >= Blo[1]) && (Ygrid <= Bhi[1]));
                        if (inside) {
                           grid2mm( ns, Xgrid, Ygrid, &Xmm, &Ymm );
                           pldraw( Xmm, Ymm );
                        } else {
                            if (Sign > 0) {
                               Sign = -1.0;
                               Xphys3 = Xphys2;
                               Yphys3 = Yphys2;
                               phys2grid( Xphys2, Yphys2, &Xgrid, &Ygrid, ns );
                               grid2mm( ns, Xgrid, Ygrid, &Xmm, &Ymm );
                               plmove( Xmm, Ymm );
                            } else {
                               Exit = YES;
                            }
                        }
                     } while ( !Exit );
                  }


               }
               Dist += fabs(Step[ax]);
               if (horizontal) Xphys += Step[ax];
               if (vertical)   Yphys += Step[ax];
            } /* No more major tickmarks for this axis */




            /*-----------------*/
            /* Minor tickmarks */
            /*-----------------*/

            if (horizontal) {
                /* Default, calculate values */
               if (nminor[0] <  0 ) Minordist = Step[ax] / (double) (Minor);
               if (nminor[0] == 0 ) Minordist = 0.0;
               if (nminor[0] >  0 ) Minordist = Step[ax] / (double) (nminor[0]+1);
            } else {
               if (nminor[1] <  0 ) Minordist = Step[ax] / (double) (Minor);
               if (nminor[1] == 0 ) Minordist = 0.0;
               if (nminor[1] >  0 ) Minordist = Step[ax] / (double) (nminor[1]+1);
            }

            if (Minordist != 0.0) {
               /* Draw minors in one direction */
               Xphys = MinorstartX; Yphys = MinorstartY;
               Sign = 1.0;
               Exit = NO;
               do {
                  if (horizontal) Xphys += Sign * Minordist;
                  if (vertical)   Yphys += Sign * Minordist;
                  Xphys2 = Xphys;
                  Yphys2 = Yphys;
                  if (Totime) {
                     if (horizontal) Xphys2 /= 240.0;
                     if (vertical)   Yphys2 /= 240.0;
                  }
                  if (Toarcs) {
                     if (horizontal) Xphys2 /= 3600.0;
                     if (vertical)   Yphys2 /= 3600.0;
                  }
                  if (Spatialmap) {
                     double    Zeroangle = 0.0;
                     double  Xoff = Crpix[0] - NINT(Crpix[0]);                     
                     double  Yoff = Crpix[1] - NINT(Crpix[1]);
                     /* Put these offsets along the axes */
                     if (ax==0) { Ygrid = Blo[1]; }
                     if (ax==1) { Xgrid = Bhi[0]; }
                     if (ax==2) { Ygrid = Bhi[1]; }
                     if (ax==3) { Xgrid = Blo[0]; }
                     if (horizontal) {
                        Ygrid -=  Yoff;
                        Mode = 2;
                        R1 = proco_c( &Xphys2, &Ygrid,
                                      &Xgrid,  &Yphys2,
                                      &Crval[0], &Crval[1],
                                      &Cdelt[0], &Cdelt[1],
                                      &Zeroangle,
                                      &Axno[ax].Prosystem,
                                      &Mode );
                        Xgrid += Xoff;
                        Ygrid += Yoff;
                     } else {
                     	Xgrid -= Xoff;
                        Mode = 1;
                        R1 = proco_c( &Xgrid, &Yphys2,
                                      &Xphys2, &Ygrid,
                                      &Crval[0], &Crval[1],
                                      &Cdelt[0], &Cdelt[1],
                                      &Zeroangle,
                                      &Axno[ax].Prosystem,
                                      &Mode );
                        Xgrid += Xoff;
                        Ygrid += Yoff;
                     }
                  } else {
                     phys2grid( Xphys2, Yphys2, &Xgrid, &Ygrid, ns );
                     if (ax==0) { Ygrid = Blo[1]; }
                     if (ax==1) { Xgrid = Bhi[0]; }
                     if (ax==2) { Ygrid = Bhi[1]; }
                     if (ax==3) { Xgrid = Blo[0]; }
                  }

                  /* Is this position within axis range? */
                  inbox = INSIDE;
                  if (inbox) {
                     grid2mm( ns, Xgrid, Ygrid, &Xmm, &Ymm );
                     Xmm += xoff; Ymm += yoff;
                     plmove( Xmm, Ymm );
                     pldraw( Xmm + xtick/2.0, Ymm + ytick/2.0 );
                  } else {
                     if (Sign > 0) {
                        Sign = -1.0;
                        Xphys = MinorstartX; Yphys = MinorstartY;
                     }
                     else {
                        Exit = YES;
                     }
                  }
               } while ( !Exit );
            }                                              /* End draw minors */
         }                                      /* End Rotated else Unrotated */
      }                                           /* End if visual and tophys */
   }                                  /* End for loop, this was the last axis */
}



static   void  plinfo(  float    x, float    y  )
/*-----------------------------------------------*/
/* Create a info column at the right of the plot */
/*-----------------------------------------------*/
{
#define     INFOLEN   255

   char     info_s[INFOLEN];
   fchar    info;
   fint     hidden = 2, request = 1;
   fint     level = 0, n;
   float    tabx, taby;
   int      i, j;
   fchar    Unitstr;
   fint     R1;
   char     UnitsX[40];
   char     UnitsY[40];
   float    Charheight;
   fint     Dfault;



   pgqch_c( &Charheight );
   Charheight *= MYMIN(devsize[1], devsize[0]) / 40.0;
   tabx = 8.0 * 0.6 * Charheight;
   taby = 1.3 * Charheight;
   info.a = info_s;
   info.l = INFOLEN;
   clearstr( info, INFOLEN );
   R1 = 0;
   gdsd_rchar_c( Setname,
                 tofchar( "OBJECT" ),
                 &level,
                 info,
                 &R1  );
   if (R1 < 0) {
      strcpy( info.a, "Object name unknown" );
   }
   info.l = nelc_c( info );
   pltext( x, y, 0.0, 0.0, info );
   R1 = 0;
   fmake( Unitstr, 10);
   gdsd_rchar_c( Setname, tofchar("BUNIT"), &Setlevel, Unitstr, &R1 );
   if (R1 < 0) strcpy( Unitstr.a, "?" );

   y -= 4.0 * taby;

   info.l = INFOLEN;
   clearstr( info, INFOLEN );
   n = usertext_c( info,
                   &hidden,
                   KEY_INSET,
                   MES_INSET );
   info.l = nelc_c( info );
   pltext( x,        y, 0.0, 0.0, tofchar( "Set:" ) );
   pltext( x + tabx, y, 0.0, 0.0, info );
   y -= taby;
   info.l = INFOLEN;
   clearstr( info, INFOLEN );
   n = usertext_c( info,
                   &hidden,
                   KEY_BOX,
                   MES_BOX );
   if ( n == 0 ) {
      (void) sprintf( info.a, "%d %d %d %d", Blo[0], Blo[1], Bhi[0], Bhi[1] );
   }
   info.l = nelc_c( info );
   for ( i = 0; i < info.l; i++ ) if ( info.a[i] == '\0' ) info.a[i] = ' ';
   pltext( x,        y, 0.0, 0.0, tofchar( "Box" ) );
   (void) sprintf( message, "%.*s (grids)",
                   nelc_c( info ),
                   info.a );
   pltext( x + tabx, y, 0.0, 0.0, tofchar(message) );
   pltext( x + tabx, y, 0.0, 0.0, info );
   y -= taby;
   if (ncont > 0) {
      info.l = INFOLEN;
      clearstr( info, INFOLEN );
      n = usertext_c( info,
                      &hidden,
                      KEY_CONTOUR,
                      MES_CONTOUR );
      info.l = nelc_c( info );
      for ( i = 0; i < info.l; i++ ) if ( info.a[i] == '\0' ) info.a[i] = ' ';
      pltext( x,        y, 0.0, 0.0, tofchar( "Cont." ) );
      if (percentage) {
         (void) sprintf( message, "%.*s (%)",
                            nelc_c( info ), info.a );
      } else {
         (void) sprintf( message, "%.*s (%.*s)",
                            nelc_c( info ), info.a,
                            nelc_c( Unitstr ), Unitstr.a );
      }
      pltext( x + tabx, y, 0.0, 0.0, tofchar(message) );
   } else {
      pltext( x,        y, 0.0, 0.0, tofchar( "Cont." ) );
      pltext( x + tabx, y, 0.0, 0.0, tofchar( "None" ) );
   }
   if ( graysc ) {
      y -= taby;
      info.l = INFOLEN;
      clearstr( info, INFOLEN );
      n = usertext_c( info,
                      &hidden,
                      KEY_GRAYSC,
                      MES_GRAYSC );
      info.l = nelc_c( info );
      for ( i = 0; i < info.l; i++ ) if ( info.a[i] == '\0' ) info.a[i] = ' ';
      pltext( x,        y, 0.0, 0.0, tofchar( "Gray." ) );
      if (percentage) {
         (void) sprintf( message, "%.*s (%)",
                            nelc_c( info ), info.a );
      } else {
         (void) sprintf( message, "%.*s (%.*s)",
                            nelc_c( info ), info.a,
                            nelc_c( Unitstr ), Unitstr.a );
      }
      pltext( x + tabx, y, 0.0, 0.0, tofchar(message) );
   }

   y -= taby;
   info.l = INFOLEN;
   clearstr( info, INFOLEN );
   if (Spatialmap) {
      (void) sprintf( info.a, "%.2f %.2f %s",
               sc_g[0]*ABS(Cdelt[0])*3600.0,
               sc_g[1]*ABS(Cdelt[1])*3600.0,
               "arcs/mm" );
   } else {
      sc_g[0] *= ABS(Cdelt[0]);
      sc_g[1] *= ABS(Cdelt[1]);
      if (strstr( Cunit[0].a, "DEGREE" )) {
         sc_g[0] *= 3600.0;
         strcpy( UnitsX, "ARCSEC" );
      } else {
         int  l;
         l = nelc_c(Cunit[0]);
         strncpy( UnitsX, Cunit[0].a, l ); UnitsX[l] = '\0';
      }
      if (strstr( Cunit[1].a, "DEGREE" )) {
         sc_g[1] *= 3600.0;
         strcpy( UnitsY, "ARCSEC" );
      } else {
         int  l;
         l = nelc_c(Cunit[1]);
         strncpy( UnitsY, Cunit[1].a, l ); UnitsY[l] = '\0';
      }


      (void) sprintf( info.a, "%gx%g %s/mm, %s/mm",
                      sc_g[0], sc_g[1],
                      UnitsX, UnitsY );
   }
   info.l = nelc_c( info );
   pltext( x,        y, 0.0, 0.0, tofchar( "Scale" ) );
   pltext( x + tabx, y, 0.0, 0.0, info );
   y -= taby;
   info.l = INFOLEN;
   clearstr( info, INFOLEN );
   (void) sprintf( info.a, "%.2f", mnmx[0] );
   info.l = nelc_c( info );
   for ( i = 0; i < info.l; i++ ) if ( info.a[i] == '\0' ) info.a[i] = ' ';
   pltext( x,        y, 0.0, 0.0, tofchar( "Min" ) );
   (void) sprintf( message, "%.*s (%.*s)",
                   nelc_c( info ), info.a,
                   nelc_c( Unitstr ), Unitstr.a );
   pltext( x + tabx, y, 0.0, 0.0, tofchar(message) );

   y -= taby;
   info.l = INFOLEN;
   clearstr( info, INFOLEN );
   (void) sprintf( info.a, "%.2f", mnmx[1] );
   info.l = nelc_c( info );
   for ( i = 0; i < info.l; i++ ) if ( info.a[i] == '\0' ) info.a[i] = ' ';
   pltext( x,        y, 0.0, 0.0, tofchar( "Max" ) );
   (void) sprintf( message, "%.*s (%.*s)",
                      nelc_c( info ), info.a,
                      nelc_c( Unitstr ), Unitstr.a );
   pltext( x + tabx, y, 0.0, 0.0, tofchar(message) );


   y -= taby;
   pltext( x,        y, 0.0, 0.0, tofchar( "(0,0):" ) );

   for (j = 0; j < 2; j++) {
      fint     mode = 0;
      fchar    Convstr;
      double   dummy[3];
      bool     displ = NO;
      fint     prec;

      fmake( Convstr, 20 );
      if ( (Axno[j].Type == 1) && (Axno[j].Skysystem == 1) ) {
         /* to hms */
         displ = YES;
         prec = 2;
         hms_c( &physicl[ Axperm[j] - 1 ],
                Convstr,
                dummy,
                &prec,
                &mode );
      }
      if ( ( (Axno[j].Type == 1) && (Axno[j].Skysystem != 1) )
          || (Axno[j].Type == 2) ) {
         displ = YES;
         prec = 1;
         dms_c( &physicl[ Axperm[j] - 1 ],
                Convstr,
                dummy,
                &prec,
                &mode );
      }
      if (displ) {
         (void) sprintf( message, "%s: %.*s", strtok( Ctype[j].a, " -" ),
                         nelc_c( Convstr ), Convstr.a );
         pltext( x + tabx, y, 0.0, 0.0, tofchar(message) );
         y -= taby;
      }
   }

   Dfault = hidden;
   do {
      info.l = INFOLEN;
      clearstr( info, INFOLEN );
      n = usertext_c( info,
                      &Dfault,
                      KEY_INFOCOM,
                      MES_INFOCOM );

      cancel_c( KEY_INFOCOM );
      info.l = nelc_c( info );
      if (info.l > 0) {
         y -= taby;
         for ( i = 0; i < info.l; i++ ) if ( info.a[i] == '\0' ) info.a[i] = ' ';
         if (Dfault == hidden) {
            pltext( x,        y, 0.0, 0.0, tofchar( "Comm." ) );
         }
         pltext( x + tabx, y, 0.0, 0.0, info );
      }
      Dfault = request;
   } while (n != 0);


   y -= 4.0 * taby;
   info.l = INFOLEN;
   clearstr( info, INFOLEN );
   pgqinf_c( tofchar( "NOW" ), info, &info.l );
   for ( i = 0; i < info.l; i++ ) if ( info.a[i] == '\0' ) info.a[i] = ' ';
   pltext( x, y, 0.0, 0.0, info );
   (void) sprintf( message, "TIME         : %.*s",
                   nelc_c( info ), info.a );
   anyoutC( 3, message );
}





static void drawframe( fint ns )
/*-------------------------------------------------*/
/* Draw a box for this subset, i.e. draw each axis */
/*-------------------------------------------------*/
{
   float  X1mm, Y1mm, X2mm, Y2mm;
   fint   Hollow = 2, Solid = 1;
   float  x1, x2, y1, y2;
   fint   Oldcolor, Background = 0;


   grid2mm( ns,
            Axno[0].StartXgrid,
            Axno[0].StartYgrid,
            &X1mm, &Y1mm );
   grid2mm( ns,
            Axno[1].EndXgrid,
            Axno[1].EndYgrid,
            &X2mm, &Y2mm );

   X1mm -= gridmargin[0];
   X2mm += gridmargin[0];
   Y1mm -= gridmargin[1];
   Y2mm += gridmargin[1];
   /* sprintf( message, "x,y x,y: %d %d %d %d", Axno[0].StartXgrid,
            Axno[0].StartYgrid,Axno[1].EndXgrid,Axno[1].EndYgrid);
   anyoutC( 3, message );
   (void) sprintf( message, "x,y x,y (mm): %f %f %f %f", X1mm,Y1mm,X2mm,Y2mm);
   anyoutC( 3, message );
   (void) sprintf( message, "devsize: %f %f", devsize[0], devsize[1] );
   anyoutC( 3, message );
   */
   pgsfs_c( &Solid );
   pgqci_c( &Oldcolor );
   pgsci_c( &Background );
   if (Gidsoverlay) {
      /* Clear everything outside rectangle */
      x1 = 0.0; x2 = devsize[0];
      y1 = 0.0; y2 = Y1mm;
      pgrect_c( &x1, &x2, &y1, &y2 );
      x1 = 0.0; x2 = devsize[0];
      y1 = Y2mm; y2 = devsize[1];
      pgrect_c( &x1, &x2, &y1, &y2 );
      x1 = 0.0; x2 = X1mm;
      y1 = 0.0; y2 = devsize[1];
      pgrect_c( &x1, &x2, &y1, &y2 );
      x1 = X2mm; x2 = devsize[0];
      y1 = 0.0; y2 = devsize[1];
      pgrect_c( &x1, &x2, &y1, &y2 );
   }
   pgsci_c( &Oldcolor );
   pgsfs_c( &Hollow );                          /* Set fill area style */
   pgrect_c( &X1mm, &X2mm, &Y1mm, &Y2mm );
}


static  void getminmaxfromdata( fchar Setname, fint Subset,
                                fint *Blo, fint *Bhi,
                                float *mnmx, int *status )
/*---------------------------------------------------------------------*/
/* Try the header items on this level first, otherwise read data and   */
/* determine min and max. Output 'status' is 0 for items from header   */
/* and 1 for calculated items.                                         */
/*---------------------------------------------------------------------*/
{
   fint     cwlo, cwhi;
   fint     maxdata = MAXDATA, nread, tid = 0;
   float    datamin, datamax;
   fint     R1;


   *status = 0;
   R1 = 0;
   gdsd_rreal_c(  Setname,
                  tofchar( "DATAMIN" ),
                  &Subset,
                  &mnmx[0],
                  &R1  );
   if ( R1 >= 0 ) {
      R1 = 0;
      gdsd_rreal_c(  Setname,
                     tofchar( "DATAMAX" ),
                     &Subset,
                     &mnmx[1],
                     &R1  );
   }



   if ( (R1 < 0) || calcmnmx ) {
      cwlo = gdsc_fill_c( Setname, &Subset, Blo );
      cwhi = gdsc_fill_c( Setname, &Subset, Bhi );
      mnmx[0] =  FLT_MAX;                            /* As defined in float.h */
      mnmx[1] = -1.0*FLT_MAX;
      do {
         gdsi_read_c(   Setname,
                        &cwlo,
                        &cwhi,
                        data,                                /* Global buffer */
                        &maxdata,
                        &nread,
                        &tid  );
         minmax1_c( data, &nread, &datamin, &datamax ); /* Take care of blanks */
         mnmx[0] = MYMIN( datamin, mnmx[0] );
         mnmx[1] = MYMAX( datamax, mnmx[1] );
      } while ( tid > 0 );
      *status = 1;
   }
}


static  void plbeg( fchar Devtxt )
/*----------------------------------------*/
/* Open device. Is it a hardcopy device?? */
/*----------------------------------------*/
{
   fint     err, mm = 2;
   fint     fatal = 4;
   fint     len;
   fint     unit = 0, xplot = 1, yplot = 1;
   float    x1_world = 0.0, x2_world = 1.0, y1_world = 0.0, y2_world = 1.0;
   char     device[2] = "?";
   fchar    inqtxt;


   /* 'pgbeg' repeats keyword until a valid device is given */
   if (Gidsoverlay) {
      err = pgbeg_c( &unit, tofchar( "gids//append" ), &xplot, &yplot );
   } else {
      err = pgbeg_c( &unit, tofchar( device ), &xplot, &yplot );
   }
   if (err != 1) {
      /* Device is valid but will not open; give message and abort task */
      error_c( &fatal, tofchar( "no access to plot device" ) );
   }

   pgsvp_c( &x1_world, &x2_world, &y1_world, &y2_world );     /* Set viewport */
   pgqvp_c( &mm, &x1_mm, &x2_mm, &y1_mm, &y2_mm );          /* This is in mm: */

   len = 20;
   fmake( inqtxt, 20 );
   pgqinf_c( tofchar( "HARDCOPY" ), inqtxt, &len );
   plotinfo = ( len == 3 );                           /* If 'YES' is returned */
   if (len == 3) Toscreen = 0; else Toscreen = 1;
   len = 20;
   pgqinf_c( tofchar( "TYPE" ), inqtxt, &len );
   strcpy( Devtxt.a, inqtxt.a );
}



static  void cotranserror( char *txt, fint Error )
/*-----------------------------------------------------------*/
/* Give more information about reason that 'cotrans' failed. */
/*-----------------------------------------------------------*/
{
   switch ( (int) Error ) {
      case 1:
         strcpy( txt, "unknown projection" );
         break;
      case 2:
         strcpy( txt, "unknown COTRANS mode" );
         break;
      case 3:
         strcpy( txt, "CROTA2 = 90.0 for COTRANS mode 1 and 2" );
         break;
      case 4:
         strcpy( txt, "CDELT1 and/or CDELT2 equal to zero" );
         break;
      case 5:
         strcpy( txt, "input sky system unknown" );
         break;
      case 6:
         strcpy( txt, "output sky system unknown" );
         break;
      case 7:
         strcpy( txt, "input and output sky system unknown" );
         break;
      case 8:
         strcpy( txt, "skypro error" );
         break;
      case 9:
         strcpy( txt, "unknown velocity system" );
         break;
      case 10:
         strcpy( txt, "rest frequency less than or equal to zero" );
         break;
      case 11:
         strcpy( txt, "crval equal to zero" );
         break;
      case 12:
         strcpy( txt, "cdelt equal to zero" );
         break;
      case 13:
         strcpy( txt, "no matching axis pair found" );
         break;
      case 14:
         strcpy( txt, "incompatible sky systems" );
         break;
      case 15:
         strcpy( txt, "cannot do epoch transformations" );
         break;
   }
}


static void  axinfo( int typenum, int skynum, int pronum, int velnum,
                     char *typestr, char *skystr, char *prostr, char *velstr )
/*----------------------------------------------------------------------*/
/* What kind of axis is this? The 'typenum' corresponds to a text       */
/*----------------------------------------------------------------------*/
{
   switch ( (int) typenum ) {
      case 0:
         strcpy( typestr, "unknown type" );
         break;
      case 1:
         strcpy( typestr, "spatial axis longitude" );
         break;
      case 2:
         strcpy( typestr, "spatial axis latitude" );
         break;
      case 3:
         strcpy( typestr, "spectral axis frequency" );
         break;
      case 4:
         strcpy( typestr, "spectral axis velocity" );
         break;
      case 5:
         strcpy( typestr, "spectral axis wavelength" );
         break;
      case 6:
         strcpy( typestr, "spectral axis inverse wavelength" );
         break;
      case 7:
         strcpy( typestr, "spectral axis log(wavelength)" );
         break;
      case 8:
         strcpy( typestr, "time axis" );
         break;
      case 9:
         strcpy( typestr, "polarisation axis" );
         break;
      case 10:
         strcpy( typestr, "parameter axis" );
         break;
      case 11:
         strcpy( typestr, "sample axis of iras data" );
         break;
      case 12:
         strcpy( typestr, "tick axis of iras data" );
         break;
      case 13:
         strcpy( typestr, "detector axis of iras data" );
         break;
      case 14:
         strcpy( typestr, "snip axis of iras data" );
         break;
   }

   skystr[0] = '\0';
   prostr[0] = '\0';
   if ((typenum == 1) || (typenum == 2)) {
      /* Display projection system */
      switch( (int) skynum ) {
         case 1:
            strcpy( skystr, "equatorial" );
            break;
         case 2:
            strcpy( skystr, "galactic" );
            break;
         case 3:
            strcpy( skystr, "ecliptic" );
            break;
         case 4:
            strcpy( skystr, "supergalactic" );
            break;
      }

      switch( (int) pronum ) {
         case 1:
            strcpy( prostr, "AITOFF equal area" );
            break;
         case 2:
            strcpy( prostr, "equivalent cylindrical" );
            break;
         case 3:
            strcpy( prostr, "flat" );
            break;
         case 4:
            strcpy( prostr, "gnomonic" );
            break;
         case 5:
            strcpy( prostr, "orthographic" );
            break;
         case 6:
            strcpy( prostr, "rectangular" );
            break;
         case 7:
            strcpy( prostr, "global sinusoidal" );
            break;
         case 8:
            strcpy( prostr, "north celestial pole (WSRT)" );
            break;
         case 9:
            strcpy( prostr, "stereographic" );
            break;
         case 10:
            strcpy( prostr, "mercator projection" );
            break;
      }
   }

   velstr[0] = '\0';
   if (typenum == 3) {
      /* Display projection system */
      switch( (int) skynum ) {
         case 1:
            strcpy( velstr, "optical" );
            break;
         case 2:
            strcpy( velstr, "radio" );
            break;
      }
   }
   if (typenum == 4)  strcpy( velstr, "radio" );
}



static  int drawlines( int ns )
/*----------------------------------------------------*/
/* Draw lines in current plot. User stops drawing     */
/* if <CR> is pressed. If one position is specified   */
/* the second position is the previous last position. */
/* As soon as a line is defined, the PLLINE keyword   */
/* is prompted and not hidden anymore.                */
/*----------------------------------------------------*/
{
   fint             Nitems;
   float            Xpos_mm, Ypos_mm;
   double           Line[4];
   static fint      Dfault = HIDDEN;
   fint             Dfault2;
   fint             More;
   static  fint     Oldsubset = -1;
   static  double   OldX, OldY;
   fint             R1;
   fint             Style, Oldstyle, Oldcolor, Oldwidth;
   fint             Llinew;
   fint             Platt[3];


   if (ns != Oldsubset) {
      Oldsubset = ns;
      OldX = 0.0;
      OldY = 0.0;
   }
   Nitems = 2;
   More = gdspos_c( Line,
                    &Nitems,
                    &Dfault,
                    KEY_PLLINE,
                    MES_PLLINE,
                    Setname,
                    &Subset[ns] );
   cancel_c( KEY_PLLINE );

   if (!More) return( NO );
   if (More == 1) {
      Line[2] = Line[0];
      Line[3] = Line[1];
      Line[0] = OldX;
      Line[1] = OldY;
      OldX = Line[2];
      OldY = Line[3];
   }
   Dfault = REQUEST;

   Dfault = REQUEST;
   Platt[0] = linew;
   Platt[1] = 1;
   pgqci_c( &Oldcolor );
   Platt[2] = Oldcolor;
   Nitems = 3;
   Dfault2 = HIDDEN;
   (void) sprintf( message, "Width, Style, Color:  [%d %d %d]",
                   Platt[0], Platt[1], Platt[2] );
   R1 = userint_c( Platt,
                   &Nitems,
                   &Dfault2,
                   KEY_PLATT,
                   MES_PLATT );
   cancel_c( KEY_PLATT );

   Llinew = MYMIN( MYMAX( 1, Platt[0] ), 21 );
   pgqlw_c( &Oldwidth );
   pgslw_c( &Llinew );

   Style = MYMIN( MYMAX( 1, Platt[1] ), 5 );
   pgqls_c( &Oldstyle );
   pgsls_c( &Style );
   pgsci_c( &Platt[2] );

   grid2mm( ns, Line[0], Line[1], &Xpos_mm, &Ypos_mm );
   plmove( Xpos_mm, Ypos_mm );
   grid2mm( ns, Line[2], Line[3], &Xpos_mm, &Ypos_mm );
   pldraw( Xpos_mm, Ypos_mm );
   pgsls_c( &Oldstyle );        /* Restore */
   pgslw_c( &linew );
   return( YES );
}



static  int drawellipse( int ns )
/*----------------------------------------------------*/
/* Draw ellipse in current plot. User stops drawing   */
/* if <CR> is pressed.                                */
/*----------------------------------------------------*/
{
   fint             Nitems;
   float            Xpos_mm, Ypos_mm;
   double           Cpos[2];
   static fint      Dfault = HIDDEN;
   fint             Dfault2;
   fint             More;
   static  fint     Oldsubset = -1;
   fint             R1;
   fint             Style, Oldstyle;
   fint             Oldcolor, Oldwidth;
   fint             Elinew;
   float            Beamang[5];
   float            angle;
   float            delta = 1.0;
   float            major, minor;
   fint             Ellatt[3];


   if (ns != Oldsubset) {
      Oldsubset = ns;
   }
   (void) sprintf( message, "Central pos. for ellipse:  [abort loop]" );
   Nitems = 1;
   More = gdspos_c( Cpos,
                    &Nitems,
                    &Dfault,
                    KEY_ELLPOS,
                    MES_ELLPOS,
                    Setname,
                    &Subset[ns] );
    cancel_c( KEY_ELLPOS );

   if (!More) return( NO );

   Dfault = REQUEST;
   Ellatt[0] = linew;
   Ellatt[1] = 1;
   pgqci_c( &Oldcolor );
   Ellatt[2] = Oldcolor;
   Nitems = 3;
   Dfault2 = HIDDEN;
   (void) sprintf( message, "Width, Style, Color:  [%d %d %d]",
                   Ellatt[0], Ellatt[1], Ellatt[2] );
   R1 = userint_c( Ellatt,
                   &Nitems,
                   &Dfault2,
                   KEY_ELLATT,
                   MES_ELLATT );
   cancel_c( KEY_ELLATT );

   Elinew = MYMIN( MYMAX( 1, Ellatt[0] ), 21 );
   pgqlw_c( &Oldwidth );
   pgslw_c( &Elinew );

   Style = MYMIN( MYMAX( 1, Ellatt[1] ), 5 );
   pgqls_c( &Oldstyle );
   pgsls_c( &Style );

   pgsci_c( &Ellatt[2] );

   grid2mm( ns, Cpos[0], Cpos[1], &Xpos_mm, &Ypos_mm );

   if (Spatialmap) {
      (void) sprintf( message, "Beam maj, min (arcs), PA, a1, a2 (deg) :  [0 0 0 0 360]" );
   } else {
      (void) sprintf( message, "Beam maj, min (units), PA, a1, a2 (deg) :  [0 0 0 0 360]" );
   }

   Dfault2 = REQUEST;
   Nitems = 5;
   Beamang[0] = 0.0;
   Beamang[1] = 0.0;
   Beamang[2] = 0.0;
   Beamang[3] = 0.0;
   Beamang[4] = 360.0;
   R1 = userreal_c( Beamang,
                    &Nitems,
                    &Dfault2,
                    KEY_ELLAXESPA,
                    MES_ELLAXESPA );
   cancel_c( KEY_ELLAXESPA );
   major = Beamang[0];
   minor = Beamang[1];
   angle = Beamang[2] + 90.0;                         /* 0 deg = along y axis */

   if (Spatialmap) {
      /* Convert to degrees and grids, note index order */
      major = (float) fabs( ((major/7200.0) / Cdelt[1] ) / sc_g[1] );
      minor = (float) fabs( ((minor/7200.0) / Cdelt[0] ) / sc_g[0] );
   } else {
      major = (float) fabs( (major/2) / sc_g[1] );
      minor = (float) fabs( (minor/2) / sc_g[0] );
   }

   pgellipse_c( &Xpos_mm, &Ypos_mm, &major, &minor, &angle, &Beamang[3], &Beamang[4], &delta );

   pgsls_c( &Oldstyle );                                    /* Restore */
   pgslw_c( &Oldwidth );
   pgsci_c( &Oldcolor );
   return( YES );
}


int  getoverlaybox( fchar Originalset, fint origsubset,
                    fchar Overlayset,  fint oversubset,
                    fint *origblo, fint *origbhi,
                    fint *overblo, fint *overbhi )
/*-----------------------------------------------------------------*/
/* Create default box for overlays.                                */
/*-----------------------------------------------------------------*/
{
   fint    Phys2grid = 0;
   fint    Grid2phys = 1;
   fint    R1, R2;
   fint    overcwlo, overcwhi;
   double  GridXY[MAXAXES];
   double  PhysXY[MAXAXES];
   double  DumXY[MAXAXES];
   fint    overflo[2], overfhi[2];
   int     i;
   fint    iax;
   fint    grid;
   fint    origindex[2];
   fint    overindex[2];
   fint    origaxis[2];
   fint    overaxis[2];
   int     agreed;
   fint    setdim;


   setdim = gdsc_ndims_c( Originalset, &Setlevel );
   i = 0;
   for (iax = 1; iax <= setdim; iax++) {
      R1 = 0;
      grid = gdsc_grid_c( Originalset, &iax, &origsubset, &R1 );
      if (R1 < 0) {
         origaxis[i] = iax;                        /* Undefined axis, must be a subset axis */
         origindex[i++] = iax-1;
      }
   }
   setdim = gdsc_ndims_c( Overlayset, &Setlevel );
   i = 0;
   for (iax = 1; iax <= setdim; iax++) {
      R1 = 0;
      grid = gdsc_grid_c( Overlayset, &iax, &oversubset, &R1 );
      if (R1 < 0) {
         overaxis[i] = iax;                        /* Undefined axis, must be a subset axis */
         overindex[i++] = iax-1;
      }
   }

   GridXY[0] = (double) origblo[0];
   GridXY[1] = (double) origblo[1];
   R1 = cotrans_c( Originalset, &origsubset, GridXY, DumXY, &Grid2phys);
   if (R1 != 0) return( NO );
   PhysXY[0] = DumXY[origindex[0]];
   PhysXY[1] = DumXY[origindex[1]];
   R1 = cotrans_c( Overlayset, &oversubset, PhysXY, GridXY, &Phys2grid );
   if (R1 != 0) return( NO );
   overblo[0] = (fint) NINT( GridXY[overindex[0]]);
   overblo[1] = (fint) NINT( GridXY[overindex[1]]);
   GridXY[0] = (double) origbhi[0];
   GridXY[1] = (double) origbhi[1];
   R1 = cotrans_c( Originalset, &origsubset, GridXY, DumXY, &Grid2phys);
   if (R1 != 0) return( NO );
   PhysXY[0] = DumXY[origindex[0]];
   PhysXY[1] = DumXY[origindex[1]];
   R1 = cotrans_c( Overlayset, &oversubset, PhysXY, GridXY, &Phys2grid );
   if (R1 != 0) return( NO );
   overbhi[0] = (fint) NINT( GridXY[overindex[0]]);
   overbhi[1] = (fint) NINT( GridXY[overindex[1]]);

   R1 = 0;
   gdsc_range_c( Overlayset, &Setlevel, &overcwlo, &overcwhi, &R1 );
   for (i = 0; i < 2; i++) {
      R1 = R2 = 0;
      overflo[i] = gdsc_grid_c( Overlayset, &overaxis[i], &overcwlo, &R1 );
      overfhi[i] = gdsc_grid_c( Overlayset, &overaxis[i], &overcwhi, &R2 );
   }

   overbhi[0] = MYMIN( overbhi[0], overfhi[0] );
   overbhi[1] = MYMIN( overbhi[1], overfhi[1] );
   overblo[0] = MYMAX( overblo[0], overflo[0] );
   overblo[1] = MYMAX( overblo[1], overflo[1] );
   agreed = (  (overbhi[0] >= overblo[0]) && (overbhi[1] >= overblo[1]) );
   if (!agreed) {
      anyoutC(3, "No part of overlay is contained in original box");
      return( NO );
   }
   return( YES );
}



static bool getlut( fint *Gidsdisplay_id, fint *lutlen, fchar Device )
/*----------------------------------------------*/
/* Temp. routine to read a colour look up table */
/*----------------------------------------------*/
{
   fint   r1;
   fint   display_id;
   bool   opened = NO;
   fint   display_stat;


   display_id = *Gidsdisplay_id;
   if (strstr( Device.a, "PS") == NULL) {
      anyoutC( 3, "Selected device is not a PostScript device!" );
      anyoutC( 3, "Rerun CPLOT with GRDEVICE=<colour device>" );
      return( NO );
   }

   display_id = gdi_open2_c( tofchar(" ") );     /* open display device */

   if (display_id < 0) {
      anyoutC( 3, "Cannot connect to GIDS for colour properties" );
      anyoutC( 3, "Use VIEW/INIDISPLAY first to get GIDS" );
      return( NO );
   } else {
      opened = YES;
   }

   r1 = gdi_getlut_c( &display_id, lutr, lutg, lutb, lutlen );
   if (r1 != 0) {
      anyoutC( 3, "Cannot get colour look up table from GIDS" );
      if (opened) {
         r1 = gdi_close_c( &display_id );  /* close display */
         opened = NO;
      }
      return( NO );
   }

   /* Get BSCALE and BZERO from GIDS to scale image data to integers */
   fmake( Gidsset, 80 );
   display_stat = gdi_iinfo2_c( &display_id ,    /* id of display */
                                Gidsset ,        /* name of set */
                                &Gidssubset ,    /* subset level */
                                Gidsblo,         /* lower left frame boundary */
                                Gidsbhi,         /* upper right frame boundary */
                                &Gidsbscale,
                                &Gidsbzero );
   if (display_stat < 0) {                       /* error obtaining info */
      anyoutC( 3, "No image loaded! Use VIEW!" );
      return( NO );
   }

   display_stat = gdi_cinfo_c( &display_id ,    /* id of display */
                               &Gidsmincol,     /* Minimum display value for which a color can be assigned. */
                               &Gidsmaxcol,
                               &Gidsncolors,    /* Number of colors (MAXCOL - MINCOL + 1).*/
                               &Gidsblank );    /* Display value reserved for undefined data. */


   if (display_stat < 0) {                      /* error obtaining info */
      anyoutC( 3, "Cannot obtain the min.& max. colour from GIDS!" );
      return( NO );
   }

   if (opened) {
      r1 = gdi_close_c( &display_id );  /* close display */
   }
   *Gidsdisplay_id = display_id;
   (void) sprintf( message, "GIDS         : allocated colours=%d  bscale=%g  bzero=%g",
                  *lutlen, Gidsbscale, Gidsbzero );
   anyoutC( 3, message );
   (void) sprintf( message, "             : colour indices: [%d,%d], blank index=%d",
                   Gidsmincol, Gidsmaxcol, Gidsblank );
   anyoutC( 3, message );

   return( YES );
}




static void setcolplot( fint  GIDSmincol, fint GIDSmaxcol,
                        float GIDSbzero, float GIDSbscale, 
                        float *lutr, float *lutg, float *lutb,
                        float *mindataval, float *maxdataval, float *blankdataval )
/*----------------------------------------------------------------------*/
/* Purpose: Setup for a colour plot.                                    */
/*----------------------------------------------------------------------*/
{
   fint  c;
   
   *blankdataval = GIDSbscale * (float)  GIDSmincol    + GIDSbzero;
   *mindataval   = GIDSbscale * (float) (GIDSmincol+1) + GIDSbzero;
   *maxdataval   = GIDSbscale * (float)  GIDSmaxcol    + GIDSbzero;
   
   for (c = GIDSmincol; c <= GIDSmaxcol; c++)
   {
      int k = c - GIDSmincol;      /* Index 0 is the definition of a blank!!! */
      pgscr_c( &c,
               &lutr[k],
               &lutg[k],
               &lutb[k] );
   }
   pgscir_c( &GIDSmincol, &GIDSmaxcol );         /* Set the color index range */   
}            



static void makecolplot( float *image, fint len,
                         fint  idim, fint jdim,
                         fint  i1, fint i2, fint j1, fint j2, 
                         float *trans, 
                         float minval, float maxval,
                         float blankmin )
/*----------------------------------------------------------------------*/
/* Purpose: Do the actual colour plotting. Take care of blanks first.   */
/*----------------------------------------------------------------------*/
{
   int  i;
   float blank;
   
    
   setfblank_c( &blank );
 
   for (i = 0; i < len; i++)
   {
      if (image[i] == blank)
        image[i] = blankmin;
      else
        if (image[i] < minval)
           image[i] = minval;                                 
   }
   

   pgimag_c(    image,            /* Colour indices to be plotted */
                &idim,            /* First dimension of 'intdata' */  
                &jdim,            /* Second dimension of 'intdata' */
                &i1,              /* Start of range of first index */
                &i2,              /* End of range of first index */
                &j1,              /* Start of range of second index */
                &j2,              /* End of range of second index */
                &blankmin,
                &maxval, 
                trans );
}




static void pggray2( float *image, 
                     int   ndat,
                     fint  idim,
                     fint  jdim,
                     fint  i1,
                     fint  i2,
                     fint  j1,
                     fint  j2,
                     float immax,
                     float immin,
                     float *trans,
                     float *levels,
                     fint  ngray )
/*----------------------------------------------------------------------*/
/* PURPOSE: Actual call to pggray. Allow blanks and non linear          */
/* subdivision of levels. Allow also immin > immax.                     */
/*----------------------------------------------------------------------*/
{
   int i;
   {
      for (i = 0; i < ndat; i++)
      {         
         int   lev;
         int   found = NO;                     

         if (image[i] != Blank)
         {
            for (lev = 0; lev < ngray-1; lev++)
            {
               if (immax >= immin)
               {
                  /* Standard sorted ascending order */
                  if (image[i] > levels[lev] && image[i] <= levels[lev+1])
                  {
                     image[i] = (levels[lev] + levels[lev+1]) / 2.0;
                     found = YES;
                     break;
                  }
               }
               else
               {
                  /* Descending order */
                  if (image[i] <= levels[lev] && image[i] > levels[lev+1])
                  {
                     image[i] = (levels[ngray-lev-1] + levels[ngray-lev-2]) / 2.0;                     
                     found = YES;
                     break;
                  }
               }
            }
         }
         if (!found)
         {
            /* Blanks */
            if (image[i] == Blank)
            {
               if (immax >= immin)
                  image[i] = immin - 1.0;
               else
                  image[i] = immax - 1.0;
            }
            else if (immax < immin)
            /* Values outside ranges and immax < immin ! */  
            {
               if (image[i] >= immin)
                  image[i] = immax - 1.0;
              else if (image[i] <= immax)
                  image[i] = immin + 1.0;
            }
         }
      }     
      if (immin > immax)
         FSWAP( immin, immax );

      pggray_c(  image,            /* Array to be plotted */
                 &idim,            /* First dimension of 'image' */
                 &jdim,            /* Second dimension of 'image' */
                 &i1,              /* Start of range of first index */
                 &i2,              /* End of range of first index */
                 &j1,              /* Start of range of second index */
                 &j2,              /* End of range of second index */
                 &immax,           /* The array value which is to appear with shade 1 ("foreground") */
                 &immin,           /* Shade 0 ("background") */
                 trans             /* Transformation matrix between array grid and world coordinates */
              );
   }
}




static  int drawoverlay( int ns )
/*-------------------------------*/
/* Overlay a user given (sub)set */
/*-------------------------------*/
{
   fchar   OverlaySetname;
   fint    Subdim = 2;
   fint    Maxsub = 1;
   fint    OverlaySubset[1];
   fint    Axperm[MAXAXES], Axsize[MAXAXES];
   fint    Class = 1;
   fint    Oblo[2], Obhi[2];
   fint    Phys2grid = 0;
   fint    Grid2phys = 1;
   fint    R1;
   double  GridXY[MAXAXES];
   double  PhysXY[MAXAXES];
   double  DumXY[MAXAXES];
   fint    Maxaxes = MAXAXES;
   fint    Option;
   double  OCdelt[2];
   double  OCrpix[2];
   float   Xmm, Ymm;
   fint    ncont, ngray;
   bool    graysc;
   bool    colsc;
   float   mnmx2[2];
   float   grmnmx[2];
   float   trans[6] = { 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 };
   fint    ly, nx, ny;
   fint    cwlo, cwhi;
   fint    tid = 0;
   int     i, j;
   float   sc_o[2];
   fint    Maxcont = MAXCONT;
   fint    Nsub2;
   int     calculated;
   fint    Nitems;
   static  fint insetdfault = HIDDEN;
   float   mindataval, maxdataval, blankdataval;
   int     first = YES;


   /* Get one 2-dim subset */
   Scrnum = 8;
   Subdim = 2;                               /* Dimension of subset must be 2 */
   fmake( OverlaySetname, 80 );
   Nsub2 = gdsinp_c(  OverlaySetname,
                      OverlaySubset,
                      &Maxsub,
                      &insetdfault,
                      KEY_INSET2,
                      MES_INSET2,
                      &Scrnum,
                      Axperm,
                      Axsize,
                      &Maxaxes,
                      &Class,
                      &Subdim   );

   cancel_c(  KEY_INSET2 );
   if (Nsub2 == 0) return( NO );          /* User does not want overlay, so quit */
   insetdfault = REQUEST;

   /* 2) Check the projection */

   /* 3) Get default values for box */
   Option = 0;
   R1 = getoverlaybox( Setname, Subset[ns], OverlaySetname, OverlaySubset[0],
                       Blo, Bhi, Oblo, Obhi );
   if (R1) Option = 6;                         /* Default in Oblo, Obhi */

   /* 4) ask user for box */
   Scrnum = 16;
   Dfault = REQUEST;
   gdsbox_c(   Oblo,
               Obhi,
               OverlaySetname,
               OverlaySubset,
               &Dfault,
               KEY_BOX2,
               MES_BOX2,
               &Scrnum,
               &Option  );
   cancel_c(  KEY_BOX2 );


   /* 6) Determine new scales */

   for (i = 0; i < 2; i++) {
      (void) sprintf( message, "CDELT%d", Axperm[i] );
      R1 = 0;
      gdsd_rdble_c( OverlaySetname, tofchar(message),
                    &Setlevel, &OCdelt[i], &R1 );
      if (R1 < 0) {
         anyoutC( 3, "CPLOT: No grid spacing found in overlay header." );
         return( NO );
      }
      (void) sprintf( message, "CRPIX%d", Axperm[i] );
      R1 = 0;
      gdsd_rdble_c( OverlaySetname, tofchar(message),
                    &Setlevel, &OCrpix[i], &R1 );
      if (R1 < 0) {
         anyoutC( 3, "CPLOT: No reference pixel found in overlay header." );
         return( NO );
      }
   }
   sc_o[0] = (float) ( (double)sc_g[0] * Cdelt[0]/OCdelt[0] );
   sc_o[1] = (float) ( (double)sc_g[1] * Cdelt[1]/OCdelt[1] );

   /* 7) determine starting point in mm */

   /* Starting point in grids in overlay */
   GridXY[0] = 0.0; GridXY[1] = 0.0;
   grid2mm( ns, (float) GridXY[0], (float) GridXY[1], &Xmm, &Ymm );
   R1 = cotrans_c( Setname, &Subset[ns], GridXY, PhysXY, &Grid2phys );

   DumXY[0] = PhysXY[Axperm[0]-1];
   DumXY[1] = PhysXY[Axperm[1]-1];
   R1 = cotrans_c( OverlaySetname, &OverlaySubset[0],
		   DumXY, GridXY, &Phys2grid );

   Xmm -= (float) ((GridXY[Axperm[0]-1] - (double)Oblo[0]) / (double)sc_o[0]);
   Ymm -= (float) ((GridXY[Axperm[1]-1] - (double)Oblo[1]) / (double)sc_o[1]);

   /* 8) Ask contours and grayscales */


   getminmaxfromdata( OverlaySetname, OverlaySubset[0], Obhi, Oblo,
                      mnmx2, &calculated );



   Dfault = REQUEST;
   {
      fchar Cmess;
      fmake( Cmess, 80 );
      if (!percentage) {
         (void) sprintf( message, "Give Ovl.contours (min=%.2f, max=%.2f): [none]",
                         mnmx2[0], mnmx2[1] );
      } else {
         (void) sprintf( message, "Give Ovl.contours 0%==min, 100%==max:  [none]");
      }

      Cmess = tofchar( message ); Cmess.l = strlen( message );
      ncont = userreal_c(  Cont2,
                           &Maxcont,
                           &Dfault,
                           KEY_CONTOUR2,
                           Cmess );
   }

   cancel_c(  KEY_CONTOUR2 );
   if ((percentage) && (ncont > 0)) {
      for (j = 0; j < ncont; j++) {
         Cont2[j] = mnmx2[0] + Cont2[j]/100.0 * (mnmx2[1]-mnmx2[0]);
      }
   }


   for (j = 0; j < (int) ncont; j++) {
      Cindex2[j] = 1;
   }
   Dfault = REQUEST;
   R1 = userint_c( Cindex2,
                   &ncont,
                   &Dfault,
                   KEY_COLIND2,
                   MES_COLIND2 );
   cancel_c(  KEY_COLIND2 );
   if (R1 >= 1) {
      /* Copy last element into remainder */
      for (j = R1; j < ncont; j++) Cindex2[j] = Cindex2[j-1];
   }

   if ( ncont > 1 ) {
      Nitems = 1;
      style2.a[0] = 'N';
      Dfault = HIDDEN;
      (void) usercharu_c( style2,
                          &Nitems,
                          &Dfault,
                          KEY_CSTYLE2,
                          MES_CSTYLE );
   }

   if (!percentage) {
      (void) sprintf( message,
              "Gray scales (min= %.2f, max= %.2f ): [no grsc map]",
               mnmx2[0], mnmx2[1] );
   } else {
      (void) sprintf( message, "Gray scales (0%==min, 100%==max):  [none]");
   }

   Dfault = REQUEST;
   ngray = userreal_c( Grayscale2,
                       &Maxcont,
                       &Dfault,
                       KEY_GRAYSC2,
                       tofchar( message ) );

   if (ngray == 0) graysc = NO; else graysc = YES;
   if ( graysc ) {
      if (ngray == 1) {
         grmnmx[0] = Grayscale2[0];
         grmnmx[1] = mnmx2[1];
      } else {
         grmnmx[0] = Grayscale2[0];
         grmnmx[1] = Grayscale2[ngray-1];
      }
   }
   cancel_c(  KEY_GRAYSC2 );


   /* and colours */

   colsc  = toflog( NO );
   Nitems = 1;
   Dfault = REQUEST;
   R1 = userlog_c( &colsc,
                   &Nitems,
                   &Dfault,
                   tofchar("COLPLOT2="),
                   tofchar("Do you want a colour plot?   Y/[N]") );
   colsc = tobool( colsc );
   if (colsc) colsc = getlut( &Gidsdisplay_id, &lutlen, Devtxt );
   cancel_c( tofchar("COLPLOT2=") ),


   /* 9) Adjust transformation matrix, determine coordinate words etc. */

   trans[0] = ( Xmm - ( 1. / sc_o[0] ) );
   trans[1] = 1. / sc_o[0];
   trans[2] = 0.0;
   trans[3] = ( Ymm - ( 1. / sc_o[1] ) );
   trans[4] = 0.0;
   trans[5] = 1. / sc_o[1];

   nx = Obhi[0] - Oblo[0] + 1;
   ny = Obhi[1] - Oblo[1] + 1;
   ly = MYMIN( MAXDATA / nx, ny );
   cwlo = gdsc_fill_c( OverlaySetname,
                       &OverlaySubset[0],
                       Oblo );
   cwhi = gdsc_fill_c( OverlaySetname,
                       &OverlaySubset[0],
                       Obhi );

   pgslw_c( &linew );


   /* 10) Draw contours and grayscales */

   do {
      fint     icont;
      fint     maxread = nx * ly;
      fint     nl;
      fint     nread;
      fint     one = 1;
      fint     solid = 1, dashed = 2;


      while (maxread > MAXDATA) maxread -= nx;
      gdsi_read_c( OverlaySetname,
                   &cwlo,
                   &cwhi,
                   data,
                   &maxread,
                   &nread,
                   &tid );

      nl = MYMIN( nread / nx, ly );
      if ( graysc ) {
         pggray2( data,
                  nread,
                  nx,
                  nl,
                  one,
                  nx,
                  one,
                  nl,
                  grmnmx[1],    /* NOTE: The array value which is */
                                /* to appear with shade 1 ("foreground"). */
                  grmnmx[0],
                  trans,
                  Grayscale2,
                  ngray  );


      }
      if (colsc) {
         if (first)
         {
            setcolplot( Gidsmincol, Gidsmaxcol, 
                        Gidsbzero, Gidsbscale,
                        lutr, lutg, lutb,
                        &mindataval,
                        &maxdataval,
                        &blankdataval );
                              
            first = NO;
         }
         makecolplot( data, 
                    nread, 
                    nx,
                    nl,
                    one,
                    nx,
                    one,
                    nl,
                    trans,
                    mindataval,
                    maxdataval,
                    blankdataval );         
      }
      Colind = 0;
      for ( icont = 0; icont < ncont; icont++ ) {
         if ( ( (style2.a[0]=='O') && icont % 2 == 1 ) ||
              ( (style2.a[0]=='N') && Cont2[icont] < 0 ) ) {
             pgsls_c( &dashed );
         } else {
             pgsls_c( &solid );
         }
         pgsci_c(&Cindex2[Colind]);
         Colind++;
         setCwidth();
         pgconb_c( data,
                   &nx,
                   &nl,
                   &one,
                   &nx,
                   &one,
                   &nl,
                   &Cont2[icont],
                   &one,
                   trans,
                   &Blank );
      }
      Colind = 1;   /* Reset */
      pgsci_c(&Colind);
      pgsls_c( &one );
      /* Adjust the offset in the Y-direction in mm */
      trans[3] += nl / sc_o[1];
   } while ( tid > 0 );
   pgslw_c( &linew );                /* Reset line width to original value */
   return( YES );
}


static void preparegids( fchar Setname )
/*-------------------------------------------------------*/
/* Check whether a GIDS window is open and a set is      */
/* displayed. Get info about size and position of the    */
/* displayed image. Calculate the offset (from lower     */
/* left corner) in mm                                    */
/*-------------------------------------------------------*/
{
   fint         display_stat;                   /* display operation status */
   fint         error_level = 4;
   fint         gerror = 0;
   fint         setdim;
   fint         iax;
   fint         R1, R2;
   int          i;
   fint         grid;
   char         messbuf[MAXMES];
   fchar        axisname;


   Gidsdisplay_id = gdi_open_c( tofchar(" ") );     /* open display device */
   if (Gidsdisplay_id < 0) {                        /* error opening display */
      error_c( &error_level ,                       /* level of error */
               tofchar( "Could not open display!" ) );
   }
   fmake( Gidsset, 80 );
   display_stat = gdi_iinfo_c( &Gidsdisplay_id ,    /* id of display */
                               Gidsset ,        /* name of set */
                               &Gidssubset ,    /* subset level */
                               Gidsblo,         /* lower left frame boundary */
                               Gidsbhi );       /* upper right frame boundary */

   if (display_stat < 0) {                      /* error obtaining info */
      error_c( &error_level ,                   /* level of error */
      tofchar( "No image loaded! Use VIEW!" ) );
   }
   if (!tobool( gds_exist_c( Gidsset, &gerror ) ) ) {
      error_c( &error_level ,                   /* level of error */
               tofchar( "Set not present!" ) );
   }
   if (gdsc_ndims_c( Gidsset, &Gidssubset ) != 2) {
      error_c( &error_level, tofchar( "Wrong dimension!" ) );
   }
   (void) sprintf( message, "Displayed set [%.*s] has box [%d %d %d %d]",
                   nelc_c(Gidsset), Gidsset.a,
                   Gidsblo[0], Gidsblo[1], Gidsbhi[0], Gidsbhi[1] );
   anyoutC( 3, message );

   /* For the proper scaling, we also want to know what the cdelts */
   /* are of the displayed (sub)set.                               */

   (void) sprintf( messbuf, "%.*s", nelc_c(Gidsset), Gidsset.a);
   setdim = gdsc_ndims_c( Gidsset, &Setlevel );
   i = 0;
   finit( axisname, 80 );
   for (iax = 1; iax <= setdim; iax++) {
      R1 = 0;
      grid = gdsc_grid_c( Gidsset, &iax, &Gidssubset, &R1 );
      if (R1 < 0) {
         /* Undefined axis, must be a subset axis */
         Gidsaxis[i] = iax;
         R2 = 0;
         (void) sprintf( message, "CDELT%d", (int) iax );
         gdsd_rdble_c( Gidsset, tofchar(message),
                       &Setlevel, &GidsCdelt[i++], &R2 );
      } else {
         R2 = 0;
         gdsc_name_c( axisname, Gidsset, &iax, &R2 );
         (void) sprintf( messbuf, "%.*s %s %d", strlen(messbuf), messbuf,
                         strtok( axisname.a , " -" ), grid );
      }
   }
   str2char( messbuf, Setname );

   R1 = gdi_frame_c( &Gidsdisplay_id ,    /* id of display */
                     Gidsflo,         /* lower left frame boundary of total GIDS area in grids */
                     Gidsfhi );

   if (R1 != 0) {
      (void) sprintf( message,
                     "Cannot obtain info about frame currently on display! (err=%d)", R1 );
      anyoutC( 3, message );
      Gidsflo[0] = Gidsblo[0];
      Gidsfhi[0] = Gidsbhi[0];
      Gidsflo[1] = Gidsblo[1];
      Gidsfhi[1] = Gidsbhi[1];
   }
   display_stat = gdi_close_c( &Gidsdisplay_id );   /* close display */
}



static void grid2Gidsmm( float GridX, float GridY, float *Xmm, float *Ymm )
/*-------------------------------------------------------------------------*/
/* Interpolate a position in mm in GIDS (given a gridposition).            */
/*-------------------------------------------------------------------------*/
{
   *Xmm = ((GridX - Gidsflo[0]) / (Gidsfhi[0] - Gidsflo[0])) * devsize[0];
   *Ymm = ((GridY - Gidsflo[1]) / (Gidsfhi[1] - Gidsflo[1])) * devsize[1];
}



static void gidsoffsets( void )
/*------------------------------------------------*/
/* Determine starting point in mm                 */
/*------------------------------------------------*/
{
   fint    Phys2grid = 0;
   fint    Grid2phys = 1;
   fint    R1;
   double  GridXY[MAXAXES];
   double  PhysXY[MAXAXES];
   double  DumXY[MAXAXES];
   float   Xmm, Ymm;


   /* Starting point in grids in overlay */
   GridXY[0] = 0.0; GridXY[1] = 0.0;
   grid2Gidsmm( (float) GridXY[0], (float) GridXY[1], &Xmm, &Ymm );
   R1 = cotrans_c( Gidsset, &Gidssubset, GridXY, PhysXY, &Grid2phys );

   DumXY[0] = PhysXY[Gidsaxis[0]-1];
   DumXY[1] = PhysXY[Gidsaxis[1]-1];

   R1 = cotrans_c( Setname, &Subset[0], DumXY, GridXY, &Phys2grid );

   Xmm -= (float) ((GridXY[Axperm[0]-1] - (double)Blo[0]) / (double)sc_g[0]);
   Ymm -= (float) ((GridXY[Axperm[1]-1] - (double)Blo[1]) / (double)sc_g[1]);
   Gidsoffset[0] = Xmm;
   Gidsoffset[1] = Ymm;
}




static void userinput( void )
/*--------------------------*/
/* General input routine.   */
/*--------------------------*/
{
   fint     Class = 1;
   fint     Maxaxes = MAXAXES, Maxcont = MAXCONT, Maxsub = MAXSUB;
   fint     Maxpos  = MAXPOS;
   fint     Nitems, Option;
   float    sc_store[2];
   fint     dumnpos;
   float    papermm[2];
   int      i,j;
   fint     Subdim = 2;
   fint     R1, R2;
   fint     Axistype;
   fint     Skysys, Prosys, Velsys;
   int      calculated;
   int      ncoord;
   char     UnitsX[40];
   char     UnitsY[40];

   char     typetxt[30];
   char     skytxt[30];
   char     protxt[30];
   char     veltxt[30];


   /*-------------------------------------------------------------------*/
   /* To be able to overlay frame or contours over an image in GIDS,    */
   /* the keyword OVERLAY= must be set to Y. The program tries to find  */
   /* out whether GIDS is already opened and if so, if there is alreay  */
   /* data displayed. Try to find then all the set info that is needed  */
   /* to use in CPLOT and use this as default for 'gdsinp'.             */
   /*-------------------------------------------------------------------*/

   Nitems      = 1;
   Dfault      = HIDDEN;
   Gidsoverlay = toflog( NO );
   R1 = userlog_c( &Gidsoverlay,
                   &Nitems,
                   &Dfault,
                   KEY_OVERLAY,
                   MES_OVERLAY );
   Gidsoverlay = tobool( Gidsoverlay );

   finit( Setname, 80 );
   finit( Mes, 80 );
   if (Gidsoverlay) {
      preparegids( Setname );
      Dfault = REQUEST;
      (void) sprintf( message, "Set (and subset(s)):   [%.*s]",
                      nelc_c(Setname), Setname.a );
      Mes = tofchar( message );
   } else {
      Dfault = NONE;
      Mes = tofchar(" ");
   }
   Scrnum = 8;
   Subdim = 2;                                    /* Dimension of subset must be 2 */
   Nsub = gdsinp_c(  Setname,
                     Subset,
                     &Maxsub,
                     &Dfault,
                     KEY_INSET,
                     Mes,
                     &Scrnum,
                     Axperm,
                     Axsize,
                     &Maxaxes,
                     &Class,
                     &Subdim   );
   Setdim  = gdsc_ndims_c( Setname, &Setlevel );

   /*-------------------------------------------------------------------*/
   /* The default size for the box is the entire frame of the input set */
   /* except if there is already a (different) set to overlay. Then the */
   /* default box is calculated.                                        */
   /*-------------------------------------------------------------------*/

   Option = 0;
   if (Gidsoverlay) {
      R1 = getoverlaybox( Gidsset, Gidssubset, Setname, Subset[0],
                          Gidsblo, Gidsbhi, Blo, Bhi );
      if (R1) Option = 6;                         /* Default in Blo, Bhi */
   }

   Scrnum = 16;                                   /* terminal, only when in "test mode" */
   Dfault = REQUEST;
   gdsbox_c(   Blo,
               Bhi,
               Setname,
               Subset,
               &Dfault,
               KEY_BOX,
               MES_BOX,
               &Scrnum,
               &Option  );


   /* Axes are labeled counter clockwise */

   Axno[0].StartXgrid = Blo[0];
   Axno[0].StartYgrid = Blo[1];
   Axno[0].EndXgrid   = Bhi[0];
   Axno[0].EndYgrid   = Blo[1];

   Axno[1].StartXgrid = Bhi[0];
   Axno[1].StartYgrid = Blo[1];
   Axno[1].EndXgrid   = Bhi[0];
   Axno[1].EndYgrid   = Bhi[1];

   Axno[2].StartXgrid = Blo[0];
   Axno[2].StartYgrid = Bhi[1];
   Axno[2].EndXgrid   = Bhi[0];
   Axno[2].EndYgrid   = Bhi[1];

   Axno[3].StartXgrid = Blo[0];
   Axno[3].StartYgrid = Blo[1];
   Axno[3].EndXgrid   = Blo[0];
   Axno[3].EndYgrid   = Bhi[1];



   /***** INFORMATION ABOUT DISPLAY AXES *****/

   Scrnum = 3;                                      /* To log file and screen */
   (void) sprintf( messbuf,
           "================ CPLOT LOG FOR SET: %.*s ====================",
            nelc_c( Setname ),
            Setname.a );
   anyoutC( 3, messbuf );


   for (i = 0; i < 2; i++) {
      anyoutC( 3, " " );
      finit( Ctype[i], 40 );
      finit( Dtype[i], 40 );
      finit( Cunit[i], 40 );
      finit( Dunit[i], 40 );
      R1 = 0;
      (void) sprintf( messbuf, "CTYPE%d", Axperm[i] );
      gdsd_rchar_c( Setname, tofchar(messbuf), &Setlevel, Ctype[i], &R1 );
      if (R1 < 0) {
         strcpy( Ctype[i].a, "PIXELS" );
         Axistype = 0;
      }
      else {
         Axistype = axtype_c( Ctype[i],
                              Cunit[i],    /* Natural units */
                              Dunit[i],
                              &Skysys,
                              &Prosys,
                              &Velsys );
      }
      R1 = 0;
      (void) sprintf( messbuf, "DTYPE%d", Axperm[i] );
      gdsd_rchar_c( Setname, tofchar(messbuf), &Setlevel, Dtype[i], &R1 );
      if (R1 < 0) strcpy( Dtype[i].a, "?" );
      if (i == 0) {
         (void) sprintf( messbuf,
                 "HORIZONTAL   : %s axis from %d to %d",
                  strtok( Ctype[i].a, " -" ),
                  Blo[i], Bhi[i]
                );
      }
      else {
         (void) sprintf( messbuf,
                 "VERTICAL     : %s axis from %d to %d",
                  strtok( Ctype[i].a, " -" ),
                  Blo[i], Bhi[i]
                );
      }
      anyoutC( 3, messbuf );
      if (R1 >= 0) {
         (void) sprintf( messbuf,
                        "(Sec. type)    %s", strtok( Dtype[i].a, " -" ) );
         anyoutC( 3, messbuf );
      }


      (void) axinfo( Axistype,
                     Skysys,
                     Prosys,
                     Velsys,
                     typetxt,
                     skytxt,
                     protxt,
                     veltxt );
      (void) sprintf( messbuf,
                     "TYPE         : %s",
                      typetxt );
      anyoutC( 3,  messbuf );

      if (strlen(skytxt) > 0) {
          (void) sprintf( messbuf,
                         "SKY          : %s",
                          skytxt );
         anyoutC( 3, messbuf );
      }

      if (strlen(protxt) > 0) {
          (void) sprintf( messbuf,
                         "PROJECTION   : %s",
                          protxt );
         anyoutC( 3, messbuf );
      }

      if (strlen(veltxt) > 0) {
          (void) sprintf( messbuf,
                         "VELOCITY     : %s",
                          veltxt );
         anyoutC( 3, messbuf );
      }

      (void) sprintf( messbuf, "CUNIT%d", Axperm[i] );
      R1 = 0;
      gdsd_rchar_c( Setname, tofchar(messbuf), &Setlevel, Cunit[i], &R1 );
      if (R1 < 0) strcpy( Cunit[i].a, "(?)" );

      (void) sprintf( messbuf, "CDELT%d", Axperm[i] );
      R1 = 0;
      gdsd_rdble_c( Setname, tofchar(messbuf), &Setlevel, &Cdelt[i], &R1 );
      if (R1 < 0) {
         anyoutC( 3, "CPLOT: No grid spacing found in header, 1.0 assumed." );
         Cdelt[i] = 1.0;
      }

      (void) sprintf( messbuf, "CRPIX%d", Axperm[i] );
      R1 = 0;
      gdsd_rdble_c( Setname, tofchar(messbuf), &Setlevel, &Crpix[i], &R1 );
      if (R1 < 0) {
         anyoutC( 3, "CPLOT: No reference pixel found in header, 0 assumed." );
         Crpix[i] = 0.0;
      }


      (void) sprintf( messbuf, "CRVAL%d", Axperm[i] );
      R1 = 0;
      gdsd_rdble_c( Setname, tofchar(messbuf), &Setlevel, &Crval[i], &R1 );
      if (R1 < 0) {
         anyoutC( 3, "CPLOT: No CRVAL in header." );
         Crval[i] = 0.0;
      }

      (void) sprintf( messbuf, "DUNIT%d", Axperm[i] );
      R1 = 0;
      gdsd_rchar_c( Setname, tofchar(messbuf), &Setlevel, Dunit[i], &R1 );

      (void) sprintf( messbuf, "DDELT%d", Axperm[i] );
      R1 = 0;
      gdsd_rdble_c( Setname, tofchar(messbuf), &Setlevel, &Ddelt[i], &R1 );
      if (R1 < 0) {
         Ddelt[i] = 1.0;
      }

      if (R1 < 0) {
         strcpy( Dunit[i].a, "(?)" );
         (void) sprintf( messbuf,
                 "GRID SPACING : %f %.*s",
                  Cdelt[i],
                  nelc_c( Cunit[i] ),
                  Cunit[i].a );
      }
      else {
         (void) sprintf( messbuf,
                 "GRID SPACING : %f %.*s, (%f %.*s)",
                  Cdelt[i],
                  nelc_c( Cunit[i] ),
                  Cunit[i].a,
                  Ddelt[i],
                  nelc_c( Dunit[i] ),
                  Dunit[i].a );
      }
      anyoutC( 3, messbuf );



      Grid[0] = 0.0; Grid[1] = 0.0;
      R1 = cotrans_c( Setname, &Subset[0], Grid, physicl, &grids2units );
      if (R1 == 0) {
         /* Cotrans successful */
         fchar Axtype;
         fchar Axunit;
         fmake( Axtype, 80 );
         fmake( Axunit, 80 );
         /* Try to find which units cotrans uses */
         R2 = axcoord_c( Setname,
                         &Axperm[i],
                         Axtype,
                         Axunit,
                         &Colev[i] );
         if (R2 != 0) strcpy( Axunit.a, "(?)" );

         CotransOk = tobool( TRUE );
         Axno[i].Tophys   = tobool( TRUE );
         Axno[i+2].Tophys = tobool( TRUE );

         (void) sprintf( messbuf,
                 "REFERENCE    : pixel [0] == [%f] %.*s",
                  physicl[ Axperm[i] - 1 ],
                  nelc_c( Axunit ),
                  Axunit.a );
         anyoutC( 3, messbuf );


         /* Update axis struct: */

          grid2phys( Axno[i].StartXgrid, Axno[i].StartYgrid,
                           &Axno[i].StartXphys, &Axno[i].StartYphys,0 );
          grid2phys( Axno[i].EndXgrid, Axno[i].EndYgrid,
                           &Axno[i].EndXphys, &Axno[i].EndYphys,0 );
          grid2phys( Axno[i+2].StartXgrid, Axno[i+2].StartYgrid,
                           &Axno[i+2].StartXphys, &Axno[i+2].StartYphys,0 );
          grid2phys( Axno[i+2].EndXgrid, Axno[i+2].EndYgrid,
                           &Axno[i+2].EndXphys, &Axno[i+2].EndYphys,0 );
      }
      else {
         CotransOk = tobool( FALSE );
         cotranserror( messbuf, R1 );
         (void) sprintf( messbuf,
                 "REFERENCE    : Cannot convert (reason: %.*s)",
                  strlen( messbuf ),
                  messbuf );
         anyoutC( 3, messbuf );

      }
      Axno[i].Type = Axistype;    Axno[i+2].Type = Axistype;
      Axno[i].Skysystem = Skysys; Axno[i+2].Skysystem = Skysys;
      Axno[i].Prosystem = Prosys; Axno[i+2].Prosystem = Prosys;
      Axno[i].Velsystem = Velsys; Axno[i+2].Velsystem = Velsys;
   } /* End loop over both axes */

   if ( (Axno[0].Type == 1) && (Axno[1].Type == 2) ) {
      /* spatial axis longitude, spat. latitude, in equatorial system */
      Spatialmap = YES;
   } else {
      Spatialmap = NO;
   }


   /* Additional info for other axes */


   /*----------------------------------------------------------------*/
   /* Special attention for rotated axes. A map is rotated if one of */
   /* the CROTA's is unequal to zero.                                */
   /*----------------------------------------------------------------*/


   ncoord = MYMAX( Setdim, ncoords_c( Setname ) );
   for (i = 0; i < ncoord; i++) {
      (void) sprintf( messbuf, "CROTA%d", i+1 );
      R1 = 0;
      gdsd_rdble_c( Setname, tofchar(messbuf), &Setlevel, &Crota[i], &R1 );
      if (R1 < 0) {
         if (R1 == -46) {
            (void) sprintf( messbuf,
                           "HEADER       : Crota%d item was stored as a real",
                            i+1 );
            anyoutC(3, messbuf );
            Crota[i] = 0.0;
         } else {
            (void) sprintf( messbuf,
                           "HEADER       : Crota%d not found, 0.0 assumed",
                            i+1 );
            anyoutC(3, messbuf );
            Crota[i] = 0.0;
         }
      } else {
         if (Crota[i] != 0.0) {
            RotationAngle = Crota[i];
         }
      }
   }

   for (i = 0; i < 2; i++) {
      static int first = YES;
      Axno[i].rotated = NO;
      Axno[i+2].rotated = NO;
      if ( ((Axno[i].Type == 1) || (Axno[i].Type == 2)) &&    /* Spatial axis */
           (RotationAngle != 0.0) ){
         Axno[i].rotated = YES;
         Axno[i+2].rotated = YES;
         if (first) {
            (void) sprintf( messbuf,
                           "MAP P.A.     : %f deg.",
                            RotationAngle );
            anyoutC( 3, messbuf );
            first = NO;
         }
      }
   }


   anyoutC( 3, " " );
   (void) sprintf( messbuf, "BOX (grid)   : from %d %d to %d %d" ,
            Axno[0].StartXgrid,
            Axno[0].StartYgrid,
            Axno[2].EndXgrid,
            Axno[2].EndYgrid );
   anyoutC( 3, messbuf);
   (void) sprintf( messbuf, "BOX (phys.)  : from %g %g to %g %g (%.*s, %.*s)" ,
            Axno[0].StartXphys,
            Axno[0].StartYphys,
            Axno[2].EndXphys,
            Axno[2].EndYphys,
            nelc_c( Cunit[0] ), Cunit[0].a,
            nelc_c( Cunit[1] ), Cunit[1].a );
   anyoutC( 3, messbuf);


   /* Select device and display device name */
   fmake( Devtxt, 20 );
   plbeg( Devtxt  );
   (void) sprintf( messbuf, "DEVICE       : Plot to device: %.*s", nelc_c( Devtxt ), Devtxt.a );
   anyoutC( 3, messbuf );

   devsize[0] = x2_mm - x1_mm;
   devsize[1] = y2_mm - y1_mm;

   if ( Nsub > 1 ) {
      Nitems = 1;
      mosaic = 0;
      Dfault = REQUEST;
      R1 = userint_c( &mosaic,
                      &Nitems,
                      &Dfault,
                      KEY_MOSAIC,
                      MES_MOSAIC  );
      if ( mosaic != 0 ) {
         nplot[1] = mosaic;                       /* Number of rows in mosaic */
         /* Then the maximum number of columns is: */
         nplot[0] = (int) ceil( ((double)Nsub / (double)nplot[1]) );
      }
   }

   /*------------------------------------------------------------*/
   /* Plot physical coordinates of subset in plot. The number of */
   /* coordinates depends on the number of non-subset axes. The  */
   /* accuracy depends on the number given in DECIMALS= and the  */
   /* position depends on SUBPOS=.                               */
   /*------------------------------------------------------------*/

   plsub = NO;
   if (Setdim > 2) {
      Nitems   = 1;
      Dfault   = HIDDEN;
      plsub    = toflog( NO );
      R1 = userlog_c( &plsub,
                      &Nitems,
                      &Dfault,
                      KEY_PLSUB,
                      MES_PLSUB );
      plsub = tobool( plsub );
   }

   if (plsub) {
      do {
         int  j;
         for (j = Subdim; j < Setdim; j++) Subsetdecimals[j-Subdim] = 2;
         Nitems = Setdim - Subdim;
         Dfault = HIDDEN;
         R1 = userint_c( Subsetdecimals,
                         &Nitems,
                         &Dfault,
                         KEY_DECIMALS,
                         MES_DECIMALS );
         agreed = YES;
         for (j = 0; j < R1; j++) {
            agreed = ( agreed && (Subsetdecimals[j] >= 0) );
         }
         if (!agreed) {
             reject_c( KEY_DECIMALS, tofchar("Must be >= 0") );
             Dfault = REQUEST;
         }
      } while (!agreed);

      Maxpos = 2;                                              /* One position */
      Subpos[0] = Blank;
      Subpos[1] = Blank;
      R1 = gdspos_c(  Subpos,
                      &Maxpos,
                      &Dfault,
                      KEY_SUBPOS,
                      MES_SUBPOS,
                      Setname,
                      Subset   );
   }


   /* plot phys. coord's labels in each mosaic */
   for (i = 0; i < (int) Nsub; i++) 
      sbnum[i] = YES;  

   if (R1 > 0) 
   {
      if (Nsub == 1) 
      {
         sbnum[0] = YES;
      } 
      else 
      {
         fint   Index[MAXSUB];

         sprintf( message, 
                 "Plotnumber(s) to exclude plotting phys.coord's:  [NONE]" ); 

         Dfault = HIDDEN;
         R1 = userint_c( Index,
                         &Nsub,
                         &Dfault,
                         KEY_SUBNUM,
                         MES_SUBNUM );
                         
         /* Plot numbers start with 1, but indices start with 0 */
         for (i = 0; i < R1; i++) 
         {
            int j = Index[i] - 1;
            if (j >= 0 && j < Nsub)                /* Subset number must exist! */
               sbnum[j] = NO;
         }
      }
   }



   /*----------------------------------------------------------------*/
   /* If user wants to get the min & max in the box, set 'calcmnmx'  */
   /* to true. Otherwise the program tries to get these values from  */
   /* the header first.                                              */
   /*----------------------------------------------------------------*/
   Nitems   = 1;
   Dfault   = HIDDEN;
   calcmnmx = toflog( NO );
   R1 = userlog_c( &calcmnmx,
                   &Nitems,
                   &Dfault,
                   KEY_CALCMNMX,
                   MES_CALCMNMX );
   calcmnmx = tobool( calcmnmx );
   getminmaxfromdata( Setname, Subset[0], Bhi, Blo, mnmx, &calculated );
   {  char   substr[40];
      fchar  Unitstr;
      fint   R1;
      (void) sprintf( message,
              "MINMAX       : %g, %g",
               mnmx[0],
               mnmx[1] );

      R1 = 0;
      fmake(Unitstr, 10);
      gdsd_rchar_c( Setname, tofchar("BUNIT"), &Setlevel, Unitstr, &R1 );
      if (R1 < 0) Unitstr = tofchar( "Units ??" );

      if (!calcmnmx) {
         if (calculated) {
            (void) sprintf( substr, "(Calculated, not in header)" );
         } else {
            (void) sprintf( substr, "(Data from header)");
         }
      } else {
         (void) sprintf( substr, "(Calculated)" );
      }

      (void) sprintf( message, "%.*s %.*s %s",
                      strlen(message), message,
                      nelc_c(Unitstr), Unitstr.a,
                      substr );
      anyoutC( 3, message );
   }



   /*----------------------------------------------------------------*/
   /* Input of contours and gray scales can also be percentages.     */
   /* Level[i] = min + (Level[i] in %) * (max - min)                 */
   /*----------------------------------------------------------------*/
   Nitems     = 1;
   Dfault     = HIDDEN;
   percentage = toflog( NO );
   R1 = userlog_c( &percentage,
                   &Nitems,
                   &Dfault,
                   KEY_PERCENT,
                   MES_PERCENT );
   percentage = tobool( percentage );


   if (!percentage) {
      (void) sprintf( message, "Give contours (min= %.2f, max= %.2f ): [none]",
                      mnmx[0], mnmx[1] );
   } else {
      (void) sprintf( message, "Give contours 0%==min, 100%==max:  [none]");
   }

   Dfault = REQUEST;
   ncont  = userreal_c(  cont,
                         &Maxcont,
                         &Dfault,
                         KEY_CONTOUR,
                         MES_CONTOUR );

   if ((percentage) && (ncont > 0)) {
      for (j = 0; j < ncont; j++) {
         cont[j] = mnmx[0] + cont[j]/100.0 * (mnmx[1]-mnmx[0]);
      }
   }


   /* Write contour levels to log file & screen */

   for (j = 0; j < ncont; j++) {
      if (j == 0) {
         (void) sprintf(message, "CONTOURS     : %g", cont[j]);
      } else {
         (void) sprintf(message, "%.*s, %g", strlen(message), message, cont[j]);
      }
      if ((strlen(message) > (80-14) ) || (j == ncont-1)) {
         anyoutC( 3, message );
         (void) sprintf(message, "             :");
      }
   }

   for (j = 0; j < (int) ncont; j++) {
      Cindex[j] = 1;
   }
   Dfault = HIDDEN;
   R1 = userint_c( Cindex,
                   &ncont,
                   &Dfault,
                   KEY_COLIND,
                   MES_COLIND );
   if (R1 >= 1) {
      /* Copy last element into remainder */
      for (j = R1; j < ncont; j++) Cindex[j] = Cindex[j-1];
   }

   if ( ncont > 1 ) {
      Nitems = 1;
      style.a[0] = 'N';
      Dfault = HIDDEN;
      (void) usercharu_c( style,
                          &Nitems,
                          &Dfault,
                          KEY_CSTYLE,
                          MES_CSTYLE );
   }

   if (!percentage) {
      (void) sprintf( message,
                     "Gray scales (min= %.2f, max= %.2f ): [no grsc map]",
                      mnmx[0], mnmx[1] );
   } else {
      (void) sprintf( message, "Gray scales (0%==min, 100%==max):  [none]");
   }


   /*--------------------------------------------------------------*/
   /* Get gray scales. Scales are also in units of the map and can */
   /* be given as values or percentages, depending on 'percentage' */
   /*--------------------------------------------------------------*/
   Dfault = REQUEST;
   ngray  = userreal_c( grayscale,
                        &Maxcont,
                        &Dfault,
                        KEY_GRAYSC,
                        tofchar(message) );



   if ((percentage) && (ngray > 0)) {
      for (j = 0; j < ngray; j++) {
         grayscale[j] = mnmx[0] + grayscale[j]/100.0 * (mnmx[1]-mnmx[0]);
      }
   }

   /* Write the scales to log file */

   for (j = 0; j < ngray; j++) {
      if (j == 0) {
         (void) sprintf(message, "GRAYSCALES   : %g", grayscale[j]);
      } else {
         (void) sprintf(message, "%.*s, %g", strlen(message), message, grayscale[j]);
      }
      if ((strlen(message) > (80-14) ) || (j == ngray-1)) {
         anyoutC( 3, message );
         (void) sprintf(message, "             :");
      }
   }



   if (ngray == 0) graysc = NO; else graysc = YES;
   if ( graysc ) {
      if (ngray == 1) {
         grmnmx[0] = grayscale[0];
         grmnmx[1] = mnmx[1];
      } else {
         grmnmx[0] = grayscale[0];
         grmnmx[1] = grayscale[ngray-1];
      }
   }


   /* Colour plots can be made in PostScript if a Colour Look Up Table is */
   /* supplied. */

   colsc  = toflog( NO );
   Nitems = 1;
   Dfault = REQUEST;
   R1 = userlog_c( &colsc,
                   &Nitems,
                   &Dfault,
                   tofchar("COLPLOT="),
                   tofchar("Do you want a colour plot?   Y/[N]") );
   colsc = tobool( colsc );
   if (colsc) colsc = getlut( &Gidsdisplay_id, &lutlen, Devtxt );

   linew = 1;
   (void) sprintf( message, "Line width in plot:    [%d]", (int) linew );
   Nitems = 1;
   Dfault = HIDDEN;
   R1 = userint_c( &linew,
                   &Nitems,
                   &Dfault,
                   KEY_LINEWIDTH,
                   tofchar(message) );
   linew = MYMIN( MYMAX( 1, linew ), 21 );
   pgslw_c( &linew );

   /* 'linew' must be known to the program before setting any attribute */



   Nitems      = 1;
   Dfault      = HIDDEN;
   Meagre      =  toflog( NO );
   R1 = userlog_c( &Meagre,
                   &Nitems,
                   &Dfault,
                   tofchar("MEAGRE="),
                   tofchar("Plot without units and labels:          Y/[N]") );
   Meagre = tobool( Meagre );

   if (Colev[0] == 2) {
        (void) sprintf( xtitle.a, "%s", strtok( Dtype[0].a, " -" ) );
   } else {
      (void) sprintf( xtitle.a, "%s", strtok( Ctype[0].a, " -" ) );
   }
   if (Meagre) {
      clearstr( xtitle, MAXTITLE );
   }
   (void) sprintf( message, "X-Title:  [%.*s]", nelc_c(xtitle), xtitle.a );
   Dfault = HIDDEN;
   (void) usertext_c( xtitle,
                      &Dfault,
                      KEY_XTITLE,
                      MES_XTITLE );
   lxtitle = nelc_c(xtitle);
   xtitle.l = lxtitle;

   if (Colev[1] == 2) {
      (void) sprintf( ytitle.a, "%s", strtok( Dtype[1].a, " -" ) );
   } else {
      (void) sprintf( ytitle.a, "%s", strtok( Ctype[1].a, " -" ) );
   }
   if (Meagre) {
      clearstr( ytitle, MAXTITLE );
   }
   (void) sprintf( message, "Y-Title:  [%.*s]", nelc_c(ytitle), ytitle.a );
   Dfault = HIDDEN;
   lytitle = usertext_c( ytitle,
                         &Dfault,
                         KEY_YTITLE,
                         MES_YTITLE );
   lytitle = nelc_c(ytitle);
   ytitle.l = lytitle;


   {
      float Charheight;
      Attributes( "TITLE", 0 );
      pgqch_c( &Charheight );
      Charheight *= MYMIN(devsize[1], devsize[0]) / 40.0;
      if (lxtitle) titlemargin[1] = 1.1 * Charheight;
      if (lytitle) titlemargin[0] = 1.1 * Charheight;
   }

   Nitems = 2;
   (void) sprintf( message, "Title margins in mm in x, y:    [%f %f]", titlemargin[0], titlemargin[1] );
   Dfault = HIDDEN;
   R1 = userreal_c( titlemargin,
                    &Nitems,
                    &Dfault,
                    KEY_TITLEMARGIN,
                    MES_TITLEMARGIN );
   if (R1 == 1) titlemargin[1] = titlemargin[0];



   /* In 'axmask' you determine whether a physical axis must be */
   /* plotted or not */
   {
      int  i;

      Nitems   = 9;
      Dfault   = HIDDEN;
      Axmask[0] = toflog( YES );
      Axmask[1] = toflog( YES );
      Axmask[2] = toflog( YES );
      Axmask[3] = toflog( YES );
      Axmask[4] = toflog( YES );
      Axmask[5] = toflog( NO  );
      Axmask[6] = toflog( NO  );
      Axmask[7] = toflog( YES );
      Axmask[8] = toflog( YES );
      R1 = userlog_c( Axmask,
                      &Nitems,
                      &Dfault,
                      KEY_AXMASK,
                      MES_AXMASK );
      for (i = 0; i < Nitems; i++) {
         Axmask[i] = tobool( Axmask[i] );
      }
   }

   if (Toscreen) plotinfo = toflog(NO); else plotinfo = toflog(YES);
   Dfault = HIDDEN;
   Nitems = 1;
   R1 = userlog_c( &plotinfo,
                   &Nitems,
                   &Dfault,
                   KEY_PLOTINFO,
                   MES_PLOTINFO );
   plotinfo = tobool(plotinfo);


   if (plotinfo) infomargin = IMARGIN; else infomargin = 0.0;
   Nitems = 1;
   (void) sprintf( message, "Info margin in mm in x:    [%f]", infomargin );
   Dfault = HIDDEN;
   R1 = userreal_c( &infomargin,
                    &Nitems,
                    &Dfault,
                    KEY_INFOMARGIN,
                    MES_INFOMARGIN );
   infomargin = fabs( infomargin );



   Nitems = 1;
   grids = toflog(NO);
   Dfault = REQUEST;
   (void) userlog_c( &grids,
                     &Nitems,
                     &Dfault,
                     KEY_GRID,
                     MES_GRID   );
   grids = tobool( grids );


   Nitems = 2;
   Dfault = HIDDEN;
   do {
      if (grids) {
         gridmargin[0] = GMARGIN;
         gridmargin[1] = GMARGIN;
      } else {
         gridmargin[0] = 0.0;
         gridmargin[1] = 0.0;
      }
      if (Gidsoverlay) gridmargin[0] = gridmargin[1] = 0.0;
      (void) sprintf( message, "Gridmargin in mm in x, y:    [%f, %f]",
                      gridmargin[0], gridmargin[1] );
      R1 = userreal_c(   gridmargin,
                         &Nitems,
                         &Dfault,
                         KEY_GRIDMARGIN,
                         MES_GRIDMARGIN );

      if (R1 == 1) gridmargin[1] = gridmargin[0];
      agreed = ( (gridmargin[0] >= 0.0) && (gridmargin[0] < devsize[0]) &&
                 (gridmargin[1] >= 0.0) && (gridmargin[1] < devsize[1]) );
      if (!agreed) {
         reject_c( KEY_GRIDMARGIN, tofchar("incorrect size!") );
         Dfault = REQUEST;
      }
   } while (!agreed);


   /* Scale the plot in units/mm. All DEGREEs are converted to  */
   /* Seconds of arc */

   ngrid[0] = Bhi[0] - Blo[0] + 1;
   ngrid[1] = Bhi[1] - Blo[1] + 1;


   {
      float Charheight;
      Attributes( "LABEL", 0 );
      pgqch_c( &Charheight );
      Charheight *= MYMIN(devsize[1], devsize[0]) / 40.0;
      labelmargin[1] = 3.0 * Charheight;
      labelmargin[0] = 10.0 * Charheight;      /* Width is at most 10 characters */
   }

   (void) sprintf( message, "Label margin in mm in x, y:    [%f, %f]",
                   labelmargin[0], labelmargin[1] );
   Dfault = HIDDEN;
   Nitems = 2;
   R1 = userreal_c( labelmargin,
                    &Nitems,
                    &Dfault,
                    KEY_LABELMARGIN,
                    MES_LABELMARGIN );


   if (Gidsoverlay) {
      sc_g[0] =  ( (float) (Gidsfhi[0]-Gidsflo[0]) ) / devsize[0];
      sc_g[1] =  ( (float) (Gidsfhi[1]-Gidsflo[1]) ) / devsize[1];
      /* Adapt scale for different pixel sizes of 'viewed' image */
      sc_g[0] = (float) ( (double)sc_g[0] * GidsCdelt[0]/Cdelt[0] );
      sc_g[1] = (float) ( (double)sc_g[1] * GidsCdelt[1]/Cdelt[1] );
   } else {
      sc_g[0] = ngrid[0] /
         ( ( devsize[0]-titlemargin[0]-2*labelmargin[0]-infomargin ) / nplot[0] - 2.0*gridmargin[0]);
      sc_g[1] = ngrid[1] /
         ( ( devsize[1]-titlemargin[1]-2*labelmargin[1]           ) / nplot[1]  - 2.0*gridmargin[1] );
   }


   /* Message for SCALE= keyword */
   if (Gidsoverlay) {
      (void) sprintf( message, "scale in x & y (grids/mm): [%.2f, %.2f]",
               sc_g[0], sc_g[1] );
   } else {
      if (Spatialmap) {
         sc_g[0] *= ABS(Cdelt[0])*3600.0;
         sc_g[1] *= ABS(Cdelt[1])*3600.0;
         /* Equal scale for spatial system */
         if (sc_g[0] > sc_g[1]) sc_g[1] = sc_g[0]; else sc_g[0] = sc_g[1];
         (void) sprintf( message,
                        "scale in x & y (arcs/mm): [%.2f, %.2f]",
                         sc_g[0],
                         sc_g[1] );
      } else {
         int len;
         sc_g[0] *= ABS(Cdelt[0]);
         sc_g[1] *= ABS(Cdelt[1]);

         len = nelc_c(Cunit[0]);
         if (strncmp( Cunit[0].a, Cunit[1].a, len ) == 0) {
            /* This is not a RA-DEC map but the units of  */
            /* the axes are the same */
            if (sc_g[0] > sc_g[1]) {
               sc_g[1] = sc_g[0];
            } else {
               sc_g[0] = sc_g[1];
            }
         }
         if (strstr( Cunit[0].a, "DEGREE" )) {
            sc_g[0] *= 3600.0;
            strcpy( UnitsX, "ARCSEC" );
         } else {
            int  l;
            l = nelc_c(Cunit[0]);
            strncpy( UnitsX, Cunit[0].a, l ); UnitsX[l] = '\0';
         }
         if (strstr( Cunit[1].a, "DEGREE" )) {
            sc_g[1] *= 3600.0;
            strcpy( UnitsY, "ARCSEC" );
         } else {
            int  l;
            l = nelc_c(Cunit[1]);
            strncpy( UnitsY, Cunit[1].a, l ); UnitsY[l] = '\0';
         }

         (void) sprintf( message,
                        "scale in x & y: [%f, %f] (%s/mm, %s/mm)",
                         sc_g[0], sc_g[1],
                         UnitsX, UnitsY );
      }
   }

   sc_store[0] = sc_g[0];
   sc_store[1] = sc_g[1];
   Nitems = 2;
   Dfault = REQUEST;
   R1 = userreal_c(   sc_g,
                      &Nitems,
                      &Dfault,
                      KEY_SCALE,
                      MES_SCALE );

   if (Gidsoverlay) {
      /* Do nothing */
   } else {
      if (Spatialmap) {
         sc_g[0] /= ABS(Cdelt[0])*3600.0;
         sc_g[1] /= ABS(Cdelt[1])*3600.0;
         sc_store[0] /= ABS(Cdelt[0])*3600.0;
         sc_store[1] /= ABS(Cdelt[1])*3600.0;
      } else {
         sc_g[0] /= ABS(Cdelt[0]);
         sc_g[1] /= ABS(Cdelt[1]);
         sc_store[0] /= ABS(Cdelt[0]);
         sc_store[1] /= ABS(Cdelt[1]);
         /* If conversion from DEGREE to ARCSEC took place: */
         if (strstr( Cunit[0].a, "DEGREE" )) {
            sc_g[0] /= 3600.0;
            sc_store[0] /= 3600.0;
         }
         if (strstr( Cunit[1].a, "DEGREE" )) {
            sc_g[1] /= 3600.0;
            sc_store[1] /= 3600.0;
         }

      }
   }


   /* Give user information about the size of the plot on paper */
   /* Scale is always in grids/mm now */

   papermm[0] = (nplot[0] * ngrid[0] / sc_g[0])
              + titlemargin[0] + 2 * labelmargin[0] + nplot[0]*gridmargin[0] + infomargin;
   papermm[1] = (nplot[1] * ngrid[1] / sc_g[1])
              + titlemargin[1] + nplot[1]*gridmargin[1] +2*labelmargin[1];

   if (Spatialmap) {
      (void) sprintf( message,
              "SCALE        : %.2fx%.2f (arcs/mm) (On paper: %.1fx%.1f mm)",
               sc_g[0]*ABS(Cdelt[0])*3600.0,
               sc_g[1]*ABS(Cdelt[1])*3600.0,
               papermm[0],
               papermm[1] );
   } else {
      float scX, scY;
      scX = sc_g[0]*ABS(Cdelt[0]);
      scY = sc_g[1]*ABS(Cdelt[1]);
      if (strstr( Cunit[0].a, "DEGREE" )) scX *= 3600.0;
      if (strstr( Cunit[1].a, "DEGREE" )) scY *= 3600.0;
      (void) sprintf( message,
              "SCALE        : %fx%f (%s/mm, %s/mm) (On paper: %.1fx%.1f mm)",
               scX, scY,
               UnitsX, UnitsY,
               papermm[0],
               papermm[1] );
   }
   anyoutC( 3, message );


   if (Toscreen && !Gidsoverlay) {
      float rat1, rat2, shrink;
      /* If the plot is on screen, try to draw the plot completely */
      /* in the TEK-window by rescaling the scales. */

      rat1 = sc_store[0] / sc_g[0];
      rat2 = sc_store[1] / sc_g[1];
      shrink = MYMAX( rat1, rat2 );
      sc_g[0] *= shrink;
      sc_g[1] *= shrink;
   }

   /* Now the scale is known: */
   if (Gidsoverlay) gidsoffsets( );

   /* Loop over the POS= keyword to be able to read a number of positions */
   /* from a recall file. */

   npos    = 0;
   nreject = 0;
   Dfault  = REQUEST;
   do {
      dumnpos = gdspos_c(  &pos[npos*2],
                           &Maxpos,
                           &Dfault,
                           KEY_MARKPOS,
                           MES_MARKPOS,
                           Setname,
                           Subset   );
      npos += dumnpos;
      cancel_c( KEY_MARKPOS );
   } while ( dumnpos  > 0 );

   dumnpos = 0;
   for (i = 0; i < npos; i++) {
      if ( pos[i*2]    >= Blo[0] &&
           pos[i*2]    <= Bhi[0] &&
           pos[i*2+1]  >= Blo[1] &&
           pos[i*2+1]  <= Bhi[1] ) {
         /* Store positions inside box */
         pos[dumnpos*2]   = pos[i*2];
         pos[dumnpos*2+1] = pos[i*2+1];
         dumnpos++;
      } else {
         nreject++;
      }
   }
   npos = dumnpos;
   if ( (npos > 0) || (nreject > 0) ) {
      (void) sprintf( message,
              "MARKERS      : Valid positions within box: %d, rejected: %d",
               npos,
               nreject );
      anyoutC( 3, message );
   }

   if ( npos ) {
      Nitems = npos;
      for (i = 0; i < npos; i++) marker[i] = 2;
      Dfault = REQUEST;
      R1 = userint_c( marker,
                      &Nitems,
                      &Dfault,
                      KEY_MARKER,
                      MES_MARKER  );
      if (R1 > 0) {
         /* Fill array with copy of last value */
         for (i = (int) R1; i < npos; i++) marker[i] = marker[i-1];
      }
   }



   /* Major tickmarks, only positive values */
   Nitems   = 2;
   tick[0]  = Blank;
   tick[1]  = Blank;
   Dfault   = HIDDEN;
   R1 = userreal_c(   tick,
                      &Nitems,
                      &Dfault,
                      KEY_TICK,
                      MES_TICK );
   if (tick[0] != Blank) tick[0] =  fabs(tick[0]);
   if (tick[1] != Blank) tick[1] =  fabs(tick[1]);



   Nitems    = 2;
   nminor[0] = -1;
   nminor[1] = -1;
   Dfault    = HIDDEN;
   (void) userint_c(  nminor,
                      &Nitems,
                      &Dfault,
                      KEY_NMINOR,
                      MES_NMINOR );



   for (i = 0; i < (int) Nsub; i++) Beam[i] = NO;               /* No beam(s) */
   Nitems = 2;
   Dfault = REQUEST;
   R1 = gdspos_c(  bmpos,
                   &Nitems,
                   &Dfault,
                   KEY_BEAMPOS,
                   MES_BEAMPOS,
                   Setname,
                   &Subset[0] );


   if (R1 > 0) {
      if (Nsub == 1) {
         Beam[0] = YES;
      } else {
         fint   Index[MAXSUB];

         Index[0] = Nsub;
         (void) sprintf( message, "Plot number(s) to plot a beam:  [%d]", Nsub );
         Dfault = REQUEST;
         R1 = userint_c( Index,
                         &Nsub,
                         &Dfault,
                         KEY_BEAMNUM,
                         MES_BEAMNUM );
         if (R1 == 0) R1 = 1;
         for (i = 0; i < (int) R1; i++) {
            int ind;
            ind = (int) Index[i]-1;              /* Subset number must exist! */
            if ((ind >= 0) && (ind < (int) Nsub)) Beam[ind] = YES;
         }
      }
   }



   Nitems      = 1;
   Dfault      = HIDDEN;
   Extendticks =  toflog( NO );
   R1 = userlog_c( &Extendticks,
                   &Nitems,
                   &Dfault,
                   KEY_EXTEND,
                   MES_EXTEND );
   Extendticks = tobool( Extendticks );

   if (Extendticks) {
      do {
         Nitems    = 1;
         Dfault    = HIDDEN;
         Linesteps = 40;
         R1 = userint_c( &Linesteps,
                         &Nitems,
                         &Dfault,
                         KEY_LINESTEPS,
                         MES_LINESTEPS );
         agreed = (Linesteps > 3);
         if (!agreed) reject_c( KEY_LINESTEPS, tofchar("Must be > 2") );
      } while  (!agreed);
   }


   Nitems      = 1;
   Interactive = FALSE;
   Dfault      = HIDDEN;
   (void) userlog_c( &Interactive,
                     &Nitems,
                     &Dfault,
                     KEY_INTERACTIVE,
                     MES_INTERACTIVE );
   Interactive = tobool( Interactive );

}



static  void plotsubset( int subsetnr, fint Subsetdecimals[],
                         double Subpos[] )
/*-------------------------------------------------------------*/
/* Plot the physical value of the subset at certain position   */
/* The definition for MAXAXES must be available.               */
/*-------------------------------------------------------------*/
{
   int    n;
   fchar  Fcunit;
   float  Xmm, Ymm;
   fint   Ferr = 0;
   fint   R1;
   fint   Fgrid;
   char   dummystr[80];
   char   messbuf[80];
   fint   Subdim = 2;      /* Must be, always */
   static fint first = YES;
   float  grid[2];
   float  Charheight;


   /* Coordinate transformation */

   fint     Direct;              /* grid coord. -> physical coord. */
   double   Coordin[MAXAXES];    /* Grids before transformation */
   double   Coordout[MAXAXES];   /* Physical coordinates after transformation */



   messbuf[0] = '\0';
   for (n = 0; n < Setdim; n++ ) {
      if (n >= Subdim) {
         Fgrid = gdsc_grid_c( Setname, &Axperm[n], &Subset[subsetnr], &Ferr );
         Coordin[ (int) Axperm[n]-1 ] = (double) Fgrid;
      }
      else {
         Coordin[ (int) Axperm[n]-1 ] = 0.0;
      }
   }
   Direct = 1;                             /* grid coord. -> physical coord. */
   R1 = cotrans_c( Setname, &Subset[subsetnr], Coordin, Coordout, &Direct );
   if (R1 != 0) {
      anyoutC( 3, "No subset info" );
      return;
   }
   for (n = Subdim; n < Setdim; n++ ) {
      if (first) {
         fmake( Fcunit, 80 );
         R1 = axunit_c( Setname, &Axperm[n], Fcunit );
         (void) sprintf( dummystr,
                        "SUBSET UNITS: %.*s",
                         (int) nelc_c( Fcunit ),
                          Fcunit.a );
          anyoutC( 3, dummystr );
         first = NO;
      }
      if ( (n + 1) == Setdim ) {
         (void) sprintf( messbuf, "%.*s%.*f", strlen(messbuf), messbuf,
                  Subsetdecimals[n-Subdim], Coordout[ (int) Axperm[n]-1 ] );
      }
      else { /* Add comma */
         (void) sprintf( messbuf, "%.*s%.*f,", strlen(messbuf), messbuf,
                  Subsetdecimals[n-Subdim], Coordout[ (int) Axperm[n]-1 ] );
      }
   }

   if (Subpos[0] == Blank) {
      grid[0] = (float) Axno[3].StartXgrid;
   } else {
      grid[0] = (float) Subpos[0];
   }
   if (Subpos[1] == Blank) {
      grid[1] = (float) Axno[3].EndYgrid;
   } else {
      grid[1] = (float) Subpos[1];
   }

   grid2mm( subsetnr,
            grid[0],
            grid[1],
            &Xmm, &Ymm );

   Attributes( "SUBSET", subsetnr );

   /* Offset from upper left corner */
   pgqch_c( &Charheight );
   Charheight *= MYMIN(devsize[1], devsize[0]) / 40.0;
   if (Subpos[0] == Blank) Xmm += 1.1*Charheight;
   if (Subpos[1] == Blank) Ymm -= 1.7*Charheight;

   if (Erase) {
       boxpltext( Xmm, Ymm, 0.0, Justification, messbuf, Toscreen, Erase );
   } else {
       pltext( Xmm, Ymm, 0.0, Justification, tofchar(messbuf) );
   }
}


static void plotcomment( int subsetnr )
/*----------------------------------------------------------------------*/
/* Plot comments on position x, y in plot. There are several keywords:  */
/* COMMENT= holds the string to be printed. COMPOS= the position        */
/* in the plot in pixels or physical units. Others select the           */
/* attributes like: Fontsize, Width, Font, Justification, Color, Angle  */
/*----------------------------------------------------------------------*/
{

   fchar   Comment;
   fint    Dfault;
   static  double Compos[2];                        /* Static essential here! */
   float   Xpos_mm, Ypos_mm;
   fint    More;
   fint    Nitems;


   do {  /* Repeat Asking comments until user pressed carriage return */
      (void) sprintf( message, "Give position for comment:  [no position]" );
      if (subsetnr == 0) Dfault = REQUEST; else Dfault = HIDDEN;
      Nitems = 2;
      More = gdspos_c( Compos,
                       &Nitems,
                       &Dfault,
                       KEY_COMPOS,
                       MES_COMPOS,
                       Setname,
                       &Subset[subsetnr] );

      cancel_c( KEY_COMPOS );


      if (More) {
         fmake( Comment, 80 );
         Dfault = REQUEST;
         More = usertext_c( Comment,
                            &Dfault,
                            KEY_COMMENT,
                            MES_COMMENT );
          cancel_c( KEY_COMMENT );
         { fint Len;
           Len = nelc_c(Comment);
           Comment.a[Len] = '\0';
           Comment.l = Len;
         }

         Attributes( "COMMENT", subsetnr );

         grid2mm( subsetnr, Compos[0], Compos[1], &Xpos_mm, &Ypos_mm );

         if (Erase) {
             boxpltext( Xpos_mm, Ypos_mm, Angle, Justification, Comment.a, Toscreen, Erase );
         } else {
             pltext( Xpos_mm, Ypos_mm, Angle, Justification, Comment );
         }
      }
   } while (More);
}


MAIN_PROGRAM_ENTRY
/*
 *----------------------------------------------------------------------------
 * Because Fortran passes all arguments by reference, all C functions with
 * a Fortran equivalent must do this also (GIPSY programmers guide,
 * Chapter 9). Variables that can be interchanged between Fortran and C
 * all start with a capital.
 *----------------------------------------------------------------------------
 */
{

   fint     cwlo, cwhi;
   fint     ly, nx, ny;
   fint     n = 0;
   fint     ns;
   float    plotmm[2];
   float    start[2];
   float    trans[6] = { 0.0, 1.0, 0.0, 0.0, 0.0, 1.0 };
   int      ipos;
   fint     Nitems;
   fint     R1;
   float    mindataval, maxdataval, blankdataval;
   int      first = YES;


   init_c();                              /* contact Hermes */
   /* Task identification */
   {
      static fchar    Task;               /* Name of current task */
      fmake( Task, 20 );                  /* Macro 'fmake' must be available */
      myname_c( Task );                   /* Get task name */
      Task.a[nelc_c(Task)] = '\0';        /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, RELEASE );  /* Show task and version */
   }

   anyoutC(3, "==============================================");
   anyoutC(3, "Start GIDS overlays with OVERLAY=Y"            );
   anyoutC(3, "==============================================");
   setfblank_c( &Blank );


   userinput( );
   nx = Bhi[0] - Blo[0] + 1;
   ny = Bhi[1] - Blo[1] + 1;
   ly = MYMIN( MAXDATA / nx, ny );
   plotmm[0] = ( nx - 1 ) / sc_g[0];
   plotmm[1] = ( ny - 1 ) / sc_g[1];
   plotsize[0] = plotmm[0] + 2 * gridmargin[0];
   plotsize[1] = plotmm[1] + 2 * gridmargin[1];
   for ( ns = 0; ns < Nsub; ns++) {
      fint     tid = 0;

      showsub1_c( Setname,
                  &Subset[ns],
                  Axperm );
      cwlo = gdsc_fill_c( Setname,
                          &Subset[ns],
                          Blo );
      cwhi = gdsc_fill_c( Setname,
                          &Subset[ns],
                          Bhi );

      initframe( ns, nplot );
      getstartXY( ns, start );
      if (Gidsoverlay) {
         trans[0] = Gidsoffset[0];
         trans[3] = Gidsoffset[1];
      } else {
         trans[0] = ( start[0] + gridmargin[0] - ( 1. / sc_g[0] ) );
         trans[3] = ( start[1] + gridmargin[1] - ( 1. / sc_g[1] ) );
      }
      trans[1] = 1. / sc_g[0];
      trans[5] = 1. / sc_g[1];

      pgslw_c( &linew );                /* Reset line width to original value */
      do {
         fint     icont;
         fint     maxread = nx * ly;
         fint     nl;
         fint     nread;
         fint     one = 1;
         fint     solid = 1, dashed = 2;


         while (maxread > MAXDATA) maxread -= nx;
         gdsi_read_c( Setname,
                      &cwlo,
                      &cwhi,
                      data,
                      &maxread,
                      &nread,
                      &tid );
         nl = MYMIN( nread / nx, ly );
         if ( graysc ) {
             pggray2( data,
                      nread,
                      nx,
                      nl,
                      one,
                      nx,
                      one,
                      nl,
                      grmnmx[1],    /* NOTE: The array value which is */
                                    /* to appear with shade 1 ("foreground"). */
                      grmnmx[0],
                      trans,
                      grayscale,
                      ngray  );
          }
          if (colsc) {
             if (first)
             {
               setcolplot( Gidsmincol, Gidsmaxcol, 
                        Gidsbzero, Gidsbscale,
                        lutr, lutg, lutb,
                        &mindataval,
                        &maxdataval,
                        &blankdataval );
                              
               first = NO;
             }
             makecolplot( data, 
                     nread, 
                     nx,
                     nl,
                     one,
                     nx,
                     one,
                     nl,
                     trans,
                     mindataval,
                     maxdataval,
                     blankdataval );
          }

          Colind = 0;
          for ( icont = 0; icont < ncont; icont++ ) {
            if ( ( (style.a[0]=='O') && icont % 2 == 1 ) ||
                 ( (style.a[0]=='N') && cont[icont] < 0 ) ) {
                pgsls_c( &dashed );
            } else {
                pgsls_c( &solid );
            }
            pgsci_c(&Cindex[Colind]);
            Colind++;
            setCwidth();
            pgconb_c( data,
                      &nx,
                      &nl,
                      &one,
                      &nx,
                      &one,
                      &nl,
                      &cont[icont],
                      &one,
                      trans,
                      &Blank );
         }
         Colind = 1;
         pgsci_c(&Colind);
         pgsls_c( &one );                                /* Line style */
         /* Adjust the offset in the Y-direction in mm */
         trans[3] += nl / sc_g[1];
      } while ( tid > 0 );
      pgslw_c( &linew );                /* Reset line width to original value */

      /*--------------------------------------------------------------*/
      /* All data of this subset of the first set is plotted. The box */
      /* has to be drawn still, but this must be done after all gray- */
      /* scale plotting in this subset is finished. Therefore, if an  */
      /* overlay is wanted, it must be plotted at this position. The  */
      /* corners of the new subset are determined and the scales are  */
      /* adjusted.                                                    */
      /*--------------------------------------------------------------*/

      do{} while (drawoverlay( ns ));


      drawframe( ns );
      if (!Meagre) {
         if (grids) {
            Attributes( "GRIDS", ns );
             drawgrids( ns );
         }
         Attributes( "LABEL", ns );
         drawunits( ns );
      }

      for ( ipos = 0; ipos < npos; ipos++ ) {
         int      ip = ipos * 2;
         fint     one = 1;
         float    xpos, ypos;

         xpos = ( pos[ip]   - ( float )Blo[0] ) / sc_g[0] + start[0] + gridmargin[0];
         ypos = ( pos[ip+1] - ( float )Blo[1] ) / sc_g[1] + start[1] + gridmargin[1];
         pgpt_c( &one, &xpos, &ypos, &marker[ipos] );
      }


      /* A beam is plotted here. */

      if (Beam[ ns ])
         drawbeam( ns );

      /* Plot one or more comments in this subset plot */
      plotcomment(ns);

      /* Plot physical subset coordinate */
      if (plsub) {
         if (sbnum[ ns ]){
           plotsubset( ns, Subsetdecimals, Subpos );
         }
      }


      /* Draw one or more lines in the plot */

      do{} while (drawlines( ns ));

      do{} while (drawellipse( ns ));

      if ( ( Nsub - ns ) > 1 && !mosaic ) {
         bool     next;

         Nitems = 1;
         Dfault = REQUEST;
         (void) userlog_c( &next,
                           &Nitems,
                           &Dfault,
                           KEY_NEXT,
                           MES_NEXT );
         cancel_c( KEY_NEXT );
      }
   }

   Attributes( "TITLE", 0 );
   if ( lxtitle ) {
      float    x, y;
      float    XYpos[2];
      float    Charheight;


      pgqch_c( &Charheight );
      Charheight *= MYMIN(devsize[1], devsize[0]) / 40.0;
      x = titlemargin[0] + labelmargin[0] + ( nplot[0] * plotsize[0] / 2.0 );
      /* Justification applies in horizontal direction only */
      y = titlemargin[1] + labelmargin[1] - Charheight - label_height - 5.0;
      if (Gidsoverlay) {
         grid2mm( 0, 0.5*(Bhi[0]+Blo[0]), Blo[1], &x, &y );
         y -= Charheight + label_height + 5.0;
      }


      Nitems   = 2;
      Dfault   = HIDDEN;
      XYpos[0] = x;
      XYpos[1] = y;
      (void) sprintf( message, "X-title position x,y (mm):      [%f %f]", x, y );
      R1 = userreal_c( XYpos,
                       &Nitems,
                       &Dfault,
                       KEY_XTPOS,
                       MES_XTPOS );
      x = XYpos[0];
      y = XYpos[1];
      (void) sprintf( message, "XTITLE at    : %f, %f mm", x, y );
      anyoutC(3, message );
      pltext( x, y, 0.0, 0.5, xtitle );
   }
   if ( lytitle ) {
      float    x, y;
      float    XYpos[2];

      x = titlemargin[0] + labelmargin[0] - label_len - 2.0;
      y = titlemargin[1] + labelmargin[1] + ( nplot[1] * plotsize[1] / 2.0 );
      if (Gidsoverlay) {
         grid2mm( 0, Blo[0], 0.5*(Bhi[1]+Blo[1]), &x, &y );
         x -= label_len + 2.0;
      }


      XYpos[0] = x;
      XYpos[1] = y;
      Nitems   = 2;
      Dfault   = HIDDEN;
      (void) sprintf( message, "Y-title position x,y (mm):      [%f %f]", x, y );
      R1 = userreal_c( XYpos,
                       &Nitems,
                       &Dfault,
                       KEY_YTPOS,
                       MES_YTPOS );
      x = XYpos[0];
      y = XYpos[1];
      (void) sprintf( message, "YTITLE at    : %f, %f mm", x, y );
      anyoutC(3, message );
      pltext( x, y, 90.0, 0.5, ytitle );
   }

   if ( plotinfo ) {
      float    x, y;
      int      labels = 1;
      Attributes( "INFO", 0 );
      /* if there is a label at the right y axis, take extra space */
      if (Axmask[5]) labels = 2;
      x = titlemargin[0] + labels * labelmargin[0] + nplot[0] * plotsize[0];
      y = titlemargin[1] +          labelmargin[1] + nplot[1] * plotsize[1];
      x += 10.0;                                        /* small offset for x */
      y = MYMIN( y, devsize[0] - 10. );
      plinfo( x, y );
   }

   if (Interactive) {
      /* Go in interactive mode to find positions */
      /* and leave mode with q <return>   */

      float  posx = 0.0;
      float  posy = 0.0;
      static char   ipkar[1];
      static fchar  kar = { ipkar, 1 };
      fint   res;
      int    exit;
      double gridxy[2];
      fint   npos = 1;
      double physxy[MAXAXES];
      fint   grids2units = 1;

      anyoutC( 8, "========== INTERACTIVE MODE =========" );
      anyoutC( 8, "P=Plot current cursor position" );
      anyoutC( 8, "Q=Quit interactive mode" );
      anyoutC( 8, "=====================================" );
      anyoutC( 8, "Grid X    Grid Y      Phys X    PhysY" );
      do {
         res = pgcurs_c( &posx, &posy, kar );
         exit = ( toupper(kar.a[0]) == 'Q' );
         if (res != 1) exit = YES;
         if ( (int) kar.a[0] == 0 ) {
            anyoutC( 1, "error occurs" );
         }
         exit = (toupper(kar.a[0]) == 'Q');
         if (!exit) {
            gridxy[0] = (double) ( sc_g[0] * ( ( posx - titlemargin[0] - labelmargin[0] ) / nplot[0] - gridmargin[0] ) );
            gridxy[1] = (double) ( sc_g[1] * ( ( posy - titlemargin[1] - labelmargin[1] ) / nplot[1] - gridmargin[1] ) );

            gridxy[0] += (double) Blo[0];
            gridxy[1] += (double) Blo[1];
            n = cotrans_c( Setname, &Subset[0], gridxy, physxy, &grids2units );
            (void) sprintf( message,
                     "%7.2f  %7.2f   %9g %9g",
                     gridxy[0],
                     gridxy[1],
                     physxy[ Axperm[0] - 1 ],
                     physxy[ Axperm[1] - 1 ] );
            anyoutC( 3, message );
            if (toupper(kar.a[0]) == 'P') pgpt_c( &npos, &posx, &posy, &marker[0] );
         }
     } while (!exit);
     anyoutC( 8, "QUIT INTERACTIVE MODE" );
   }
   pgend_c( );
   finis_c( );
   return( 0 );
}


