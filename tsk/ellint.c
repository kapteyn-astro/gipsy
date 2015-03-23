/*
                           COPYRIGHT (c) 1995
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             ellint.dc1

Program:       ELLINT

Purpose:       Integrate map in elliptical rings. Write statistics
               to log file, Ascii file or GDS table.


Category:      ANALYSIS, TABLES

File:          ellint.c

Author:        M.G.R. Vogelaar

Keywords:

   INSET=      Give set, subsets:

               Maximum number of subsets is 2048.
               Dimension of the subsets must be 2.


   OPTION=     Give OPTION:                                         [1]

               OPTION=1  Calculate SUM, MEAN, MEDIAN, AREA etc. in
                         rings.
               OPTION=2  Calculate projected and face-on surface
                         brightness in mapunits/area for all rings.
               OPTION=3  Calculate mass surface density in Msun/pc^2
                         given the total mass (MASS=) in Msun and
                         distance (DISTANCE=) in Mpc. The total area
                         under the density distribution will be
                         scaled to the total mass.


   MASS=       Give mass in Msun:

               Total mass of your object in solar masses. This value
               will be used (together with DISTANCE=) to scale the
               results to mass surface densities.


   DISTANCE=   Give distance in Mpc:

               See description at MASS=


** PIXELS=     Calculations and plotting in pixels?                 [N]

               Do calculations and plotting in pixels, even when the
               conversion to seconds of arc is possible. If PIXELS=Y
               subsequent keywords (RADII=, WIDTH= etc) are given in
               pixels. There is a status message in the log file about
               this conversion.


   RADII=      Give radii in seconds of arc:

               If PIXELS=Y or no conversion to arcsec could be made
               then the prompt is changed to:

               Give radii in pixels:

               The radius is measured in seconds of arc (or pixels)
               from the centre (POS=) of the inner ellipse to the
               centre of the ring, along the major axis. Maximum
               number of radii is 512.


   WIDTH=      Width of ring (arcsec or pixels):

               There can be as many widths as there are radii.
               If the number of widths is less, the last value is
               copied until the number of widths is equal to the
               number of radii. A ring (radius R, Width W) is defined 
               by the inner ellipse with radius R-(W/2) and an outer
               ellipse with radius R+(W/2)


   PA=         Position angle major axis (deg):

               Measured as per astronomical convention from north
               to east. For the number of position angles see remarks
               at WIDTH=


   SEGMENTS=   Segments (angles wrt major axis):                [0,360]

               For options 1 and 2 it is also possible to calculate
               statistics in a part of a ring, a so called segment.
               Default segment is the entire ring (0,360). Required
               to define a valid segment are two angles. The maximum
               number of segments is 360. The first pair of segment
               angles MUST be 0,360.
               See description.


   INCL=       Inclination (deg):

               If i is the inclination in degrees then
               minor / major = cos(i).
               For the number of inclinations see remarks at WIDTH=


   POS=        Position of center of ellipses:                    [0,0]

               (Enter position in grids or physical coordinates)
               This position applies to ALL ellipses.


   RANGE=      Range of intensities to include:            [all levels]

               Input consists of two values. The first value may
               be greater than the second. See the description
               for the correct use of this keyword.


** OVERLAP=    Weight data in overlapping rings:                 [Y]/N

               Data can be weighted in overlapping rings. If at certain
               position a pixel with image value X is encountered in 
               N rings, its image value decreases to X/N if OVERLAP=Y


   SUBPIX=     Give number of 'sub'pixels in x and y per pixel:   [2 2]

               Make a better approximation of the true area in a ring
               by dividing a pixel into SUBPIX= 'sub'pixels. The
               first number is the subdivision in x. The second is the
               subdivision in y. If only x is entered, then y is copied
               from the value of x.


** ORDER=      Table order 1)seg/rad, 2)rad/seg:                  [1]/2

               The generated statistics table can display all
               segments for one ring (ORDER=1) or display all radii
               for one segment (ORDER=2).


   MEDIAN=     Do you want to calculate the MEDIAN also!          Y/[N]

               For OPTION=1 it is possible to calculate the median
               of the (sub)pixels in a ring or segment.


   OVERLAY=    Overlay ellipse(s) in GIDS?                        [N]/Y

               If GIDS displays a set with the same name as INSET=
               then you are prompted with this keyword. It is asked
               hidden if the names do not match and the keyword is
               skipped if GIDS is not in use.


   ELLIPSE=    Give index of ellipse to plot:                 [no plot]

               Keyword is unhidden if OVERLAY=Y else hidden.
               Syntax: ELLIPSE=n m

               Indices correspond to the n-th radius and m-th segment.
               If m is omitted, m is set to 1 (first segment).
               The first number corresponds to a radius index,
               the second to a segment index. Indices start at 1.
               Plot the selected ellipse and fill the LAST selected
               ring & segment with dots on positions where a pixel is
               found that is include in the ring. The default ends the
               plotting loop.
               This plot illustrates the definition of the angles
               and the sampling of a ring or segment.


** FILENAME=   Write statistics to:                           [No file]

               Results are always written to a GDS table, but
               is also possible to write data to an Ascii file. The
               name of this file is given in FILENAME=
               If the file already exists, the keyword OVERWRITE=
               is prompted. The output is formatted according to
               FORMAT=


   OVERWRITE=  File exists, ok to overwrite?                      [Y]/N

               Only asked if FILENAME= is a name of an existing
               file on disk.


   FORMAT=     Enter format for number output:              [ffffff.ff]

               See description for syntax and examples.



               Keywords to initialize plots:


** PGBOX=      Corners of box Xl,Yl, Xh,Yh:                [calculated]

               (In the same units as the radius)
               If a plot is made of one or more ellipses, then
               the program needs to know what the plot boundaries are.
               Default are the corners the same as the box (in pixels)
               returned by GIDS, or if GIDS is not used, the default
               corners are set by the largest ellipse.

               PGBOX= is also prompted (hidden) after keyword PLOTOPT=
               to set the limits of the box in which profiles are
               plotted.


** PGMOSAIC=   View surface subdivisions x,y:                     [1,1]

               It is possible to have more plots on the view surface
               (f.i. the plot window).
               The number of plots can vary in both directions x and y.


   GRDEVICE=   Give graphics device to      [list of available devices]
               plot on:


** PGPAPER=    Give width (cm), aspect ratio:                [0.0, 1.0]

               Change the size of the (output) view surface.
               The aspect ratio is defined as height/width.
               The default is a calculated size and aspect ratio 1.


** PGWIDTH=    Give line width (1-21):                              [2]

               Only required if a hardcopy device is selected.


               Note that the plot keywords are asked before plotting
               ellipses and are asked again before plotting profiles.



               Keywords related to GDS tables:


** GDSTABLE=   Store results in table header of INSET=            Y/[N]

               If GDSTABLE=Y a GDS table is created in the header
               of INSET= and most of the results are written to this 
               table (so that it can extracted by porgram TABLE or
               by your private COLA program). However, writing to such
               a table can slow down the output. Therefore the 
               default will NOT create a GDS table.


** TABNAME=    Give name of table to store results:            [ELLINT]

               Columns are created at set level. See 'description'
               to see what the names and contents of the columns
               is. Tables can be listed/edited/plotted with program
               TABLE.


** TABAPPEND=  Append to existing columns?                        Y/[N]

               If a table already exists, it is possible to append
               to this table if you enter TABAPPEND=Y
               The default always creates a new table.


   PLOTOPT=    OPTION=1 MEDIAN=Y:
               1=sum 2=mean 3=rms 4=min 5=max 6=A 7=A-bl 8=med:  [quit]

               OPTION=1 MEDIAN=N:
               1=sum 2=mean 3=rms 4=min 5=max 6=A 7=A-bl:        [quit]

               OPTION=2:
               1=sd 2=sd-t 3=fo-sd 4=fo-sd-t 5=sum 6=A 7=A-bl:   [quit]

               OPTION=3
               1=Msd-t 2=M-t 3=Cu.M 4=Msd 5=M 6=Cu.M 7=S 8=A 9=A-bl:[q]


               Plot calculated statistics as function of radius.
               At this moment one can alter the default limits
               of a plot with PGBOX=  (is a hidden keyword).
               Example: PLOTOPT=1 PGBOX=0 0 1200 10



Description:   GEOMETRY
               ========
               
              
               ELLINT can be used to find the radial intensity
               distributions in galaxies or, for example, to
               find the mean intensity of instrumental rings
               in maps. The source of the data is specified by INSET=
               The output is a table where the sum, mean, median and
               rms, are given in map units (OPTION=1), or as (a sort
               of) surface brightness (OPTION=2: map units/arcsec^2
               usually for optical work) or, as mass surface density
               (OPTION=3: Mo/pc^2). The last option is a scaling and
               requires the total mass of your object (galaxy) in
               solar masses (MASS=) and the distance in Mpc (DISTANCE=)
               (a negative sum will result in a negative mass).

               The maximum number of subsets is 2048 and the subsets
               must be two dimensional. The program gets the axis
               information from the header (can be displayed when
               Hermes set to TEST mode) and calculates a factor to 
               convert from the header axis units to seconds of arc. 
               However, if you want that all calculations and plotting 
               is done in pixels, use PIXELS=Y. The program informs you
               if it uses the conversion to seconds of arc or not.
               At the RADII= prompt,  give radii measured in seconds of
               arc (or pixels) from the center along the major
               axis.
               The keyword POS= is used to set the position
               of the center of all the ellipses.
               For example, if you want the new central position in
               input pixel coordinates (-10.680130, -8.700096) and
               position (0,0) in your input corresponds to the physical
               coordinates 198.468100, 46.002610 deg. you specify:

               POS=-10.680130 -8.700096              (pixels)   or:
               POS= U 198.493667  U 45.983306        (deg, deg) or:
               POS= * 13 13 58.48  * 45 58 59.90     (hms, dms)

               The position angle of the major axis of a galaxy is
               defined as the angle taken in anti-clockwise direction
               between the north direction in the sky and the major axis
               of the receding half of that galaxy (Rots 1975) astron,
               astrophys 45, 43. This angle is given in PA=.
               If possible, the program automatically corrects for the
               rotation angle of the actual axis from the stated header
               coordinate.  It is assumed that the spatial latitude axis
               carries the rotation information. If nothing is found
               in the header, 0 degrees is assumed. If you give less
               angles than there are radii, the missing angles are copied
               from the last one.

               A ring is defined after the keyword WIDTH= by an
               inner ellipse with radius R-(width/2) and an outer
               ellipse with radius R+(width/2). The width is given in
               the same units as the radii. For each radius you can
               define a width. If you give less widths than there are,
               radii, the program copies the last value of
               the width until there are as much widths as there are
               radii. Note that the width of the defined segments is
               not constant in the defined ellipse but is only
               constant in its (de)projection to a circle.

               Integration in circles is simply accomplished by setting
               INCL=0.0 else you integrate in an ellipse with
               minor/major = COS(INCL=).
               If you want to integrate a complete circle with radius
               R, then use:
               INCL=0.0 , RADII=R/2 , WIDTH=R

               For each radius an inclination can be given, but if
               you give less inclinations than there are radii, the
               rest of the inclinations are copied from the last one
               until there are as much inclinations as there are radii.
               You can give an expression instead of the angles.
               If, for example, you want to give two ratio's instead of
               two angles, try an input like:

               INCL=DEG(ACOS([0.3 0.5]))

               Between the brackets are the arguments 0.3 and 0.5.
               They are converted to the angles 72.5424 and 60.0 deg.
               by the input routines.

               For all defined rings, the calculations are carried
               out for one or more segments (SEGMENTS=).
               Two numbers mark one segment. The numbers
               [0,360 270,90 90,270] for example, define three
               segments. The first one is the complete ring, the second
               runs from the minor axis to the opposite minor axis
               through the major axis (from -90 deg. incl. to 90 deg. 
               excl. in the direction of the east) and the third segment 
               runs from the opposite minor axis to the the minor axis 
               through the opposite major axis (from 90 deg. incl. to
               -90 deg. excl.)                             
               The angles can be negative but then they are converted
               to values in the range [0,360> degrees.
               If option 3 was selected the first segment must be
               0,360, otherwise the scaling would not be correct.

               You can visualize the way the program interprets
               the axes and the angles with keyword ELLIPSE=
               The arguments are the index of a radius and a segment.
               ELLIPSE=2,3 plots the third segment of your second
               ellipse. The default skips plotting.
               The plot-data is always extracted from the first subset.
               You can overlay such a plot on an image displayed in GIDS.
               The image can be a zoomed image. If it is possible
               to apply an overlay then you are prompted with 
               OVERLAY=

 

               STATISTICS
               ==========

               ELLINT computes in a given elliptical ring with
               certain major-, minor axis, width, angle, inclination
               and in each defined segment:


               OPTION=1 : the sum, mean and rms of these values.
                          If the values are represented by Xi and there 
                          are Ni valid values, then:
                          
                             sum  = SUM(Xi)
                             mean = sum / Ni
                             var  = (SUM(Xi^2) - Ni*mean^2) / (Ni-1)
                             rms  = SQRT(var)

                          A valid value is not a blank and is within 
                          a range set by RANGE=  Values outside this range 
                          are treated as blanks. A 'subpixel' (there are 
                          SUBPIX= 'subpixels' in a pixel in x and y) is 
                          treated like a pixel.

                          Examples of the use of the RANGE= keyword:

                          RANGE=2, 5
                          (IF 2<value<5 THEN include ELSE value==>blank)

                          RANGE=5, 2
                          (IF 2<value<5 THEN value==>blank ELSE include)

                          At the RANGE= keyword, the values -INF and INF can
                          be input also. These values represent the minimum
                          and maximum values of the current system.

                          RANGE=5, INF
                          (IF value>5 THEN include ELSE value==>blank)

 
                          If regions do overlap in rings, it is possible
                          to integrate them separate or weight the common data 
                          over the overlapping regions. The keyword OVERLAP= 
                          sets the action. If OVERLAP=Y a value is weighted by 
                          the number of times the corresponding position is 
                          present in a ring.


               OPTION=2: -Mean surface brightness projected on the sky,
                          averaged over all non blank area.
                         -Mean surface brightness projected on the sky,
                          averaged over total area.
                         -Mean face-on surface brightness averaged over
                          all non blank area.
                         -Mean face-on surface brightness averaged over
                          total area.

                          The mean surface brightnesses are calculated by
                          summing and averaging the surface densities per
                          pixel. This is equivalent to the sum divided by the 
                          area. Here we distinguish two areas. The first
                          area is equal to the area of all pixels that are 
                          not blank.
                          The second area is the area that is equal to 
                          to the area of all pixels.
                          It depends on what you think of what a blank
                          actually represents whether you use the one
                          surface brightness or the other.

                          The face-on areas are increased by a factor
                          1/COS(inclination) 
 
                          sb = Xi / [Area of one pixel / COS(inclination)]

                          The units are map units per second of arc
                          squared.
                          

               OPTION=3:  First for each ring the mean face-on surface
                          density (fo-sd) (derived in the same way as the 
                          mean face-on surface brightness of OPTION=2) 
                          is multiplied by PI*(R(n+1)^2-Rn^2), the area between 
                          two annuli, with index n and n+1, in the segment 
                          0,360 deg. (this is the reason that for OPTION=3 
                          the first segment in a sequence must always be 
                          0,360 degrees).
                          The values are summed for all rings and the result
                          is stored ('total sum').

                          If M is the total mass of the object in solar
                          masses (MASS+), D its distance (DISTANCE=) in Mpc 
                          and the conversion of an area in seconds of arc to
                          an area in pc is given by:
 
                                x(pc) = 4.848 * D(Mpc) * a(arcsec)

                          then the mass surface density sigM per ring is

                            sigM = (face-on sd/total sum)*M/(4.848*D)^2

                          We distinguish two values for the mass surface 
                          density. This is because the mass surface density 
                          is derived from the surface density (see OPTION=2)
                          and that value depends on the area that was used 
                          (total area or non-blank area). Program ELLINT
                          lists both values in a table.
                          
                         

               TABLES
               ======

               Examples of output table in log file:

               OPTION=1:

               radius   cum.sum       sum      mean    median       rms
               arcsec   JY/BEAM   JY/BEAM   JY/BEAM   JY/BEAM   JY/BEAM
               ========================================================= ...
                 0.00      1.49      1.49      0.37      0.37      0.01
                30.00      9.19      7.71      0.39      0.39      0.01


               subpix unique      area   area-bl  segLO  segHI  width
                    # pixels    pixels    pixels   deg.   deg. arcsec
               ====================================================== ...
                    4      4      4.00      0.00   0.00 360.00  30.00
                   20     20     20.00      0.00   0.00 360.00  30.00


                   pa   incl
                  deg.   deg.
               ==============   
               336.00  30.00
               336.00  30.00 


               cum.sum: The cumulative sum, i.e. the 'running' sum of the
                        values in the next column 'sum'.

               sum:     Add the image values represented by each 'subpixel'
                        in the ring.
                        The image value of a 'subpixel' is equal to the
                        image value of the pixel to which it belongs
                        divided by the number of 'subpixels' in a pixel.

               mean:    Mean image value in the ring.

               median:  Only listed if MEDIAN=Y. It the number of
                        'subpixels' is equal to N, then the median element
                        of the data is the (N+1)/2 th element of the
                        sorted data if N is odd.  If N is even, its the
                        arithmetic mean of the N/2 th and N/2+1 th element.

               rms:     Measure of data's width around its central value.

               subpix:  Number of 'subpixels' that was found in a ring.
                        This number depends on the value of SUBPIX=
                        It will increase if SUBPIX= is increased.

               unique:  Number of different pixels that contributed to the
                        ring. If SUBPIX= is increased then this number
                        will converge to a fixed number. This is because
                        a larger number for SUBPIX= means a better
                        approximation of the area of a ring.

               area:    Area of ring covered by non blank image values, counted
                        in 'subpixels' and scaled to pixels.

               area-bl: Same as area, but now only the blank 'subpixels'
                        are counted.

               segLO:   All the statistics described above can also be
                        applied to segments. If a ring is divided into
                        segments (SEGMENTS=) then the generated table displays
                        all statistics for all segments per ring (ORDER=1) or
                        it displays statistics for all radii per segment
                        (ORDER=2). This column lists the start angles of the
                        segments.

               segHI:   End angles of segments.

               width:   Width of current ring.

               pa:      Position angle of major axis of current ring.

               incl:    Inclination.



               OPTION=2:
               
               radius         sb       sb-t      fo-sb    fo-sb-t        sum 
               arcsec JY/BEAM/'' JY/BEAM/'' JY/BEAM/'' JY/BEAM/''    JY/BEAM
               =============================================================
                 0.00     0.0017     0.0017     0.0014     0.0014     1.4866  
                30.00     0.0017     0.0017     0.0015     0.0015     7.7064            
                 
                
               sb:       Mean surface brightness projected on the sky,
                         averaged over all non blank area.
               sb-t:     Mean surface brightness projected on the sky,
                         averaged over total area (i.e. blank pixel area 
                         included).
               fo-sb:    Mean face-on surface brightness averaged over
                         all non blank area.
               fo-sb-t:  Mean face-on surface brightness averaged over
                         total area.

               
              
               OPTION=3: 
            
               radius radius    Msd-t  Mass-t  Cum.Mass      Msd     Mass Cum.Mass
               arcsec    Kpc  Mo/pc^2  10^9 M0  10^9 M0  Mo/pc^2  10^9 M0  10^9 M0
               ===================================================================
                 0.00   0.00     5.05     0.00     0.00     4.67     0.00     0.00
                30.00   0.99     5.24     0.03     0.04     4.84     0.03     0.03              

               radius (Kpc): Radius converted from arcsec to kpc:
                             R(Kpc) = 4.848 * D(Mpc)/1000 * Radius('');
               Msd-t:       Mass surface density derived from mean face-on 
                             surface density (as in OPTION=2) averaged over
                             total area.
               Mass-t:      Scaled MASS= proportional to surface density
                             times area of a ring. The surface density
                             is the same as used in 'Msd-t'.
               Cum.Mass:     The cumulative 'Mass-t'. The value in the 
                             last row of this column, is equal to MASS=
               Msd:          Mass surface density derived from mean face-on
                             surface density (as in OPTION=2) averaged over
                             area in which there are no blank pixels.
               Mass:         Scaled MASS= proportional to surface density
                             times area of a ring. The surface density
                             is the same as used in 'Msd'.
               Cum.Mass:     The cumulative 'Mass'. The value in the    
                             last row of this column, is equal to MASS=

               The result is written to disk in an Ascii file with 
               name set by FILENAME=
               If this file already exists, the user is warned
               with OVERWRITE= 
               Note that the first lines in the file are meant as
               comments and are prefixed with an exclamation mark (!).
               If you want more precision in the output, use the
               FORMAT= keyword.


               Results are always stored in a table. The name of the table 
               is 'ELLINT' by default. You can change this with TABNAME=
               The column names are:

               CUMSUM,          Cumulative sum
               SUM,             Sum in ring
               MEAN,            Mean in ring
               RMS,             Rms in ring
               MINVAL,          Minimum of data
               MAXVAL,          Maximum of data
               NPTS,            Non blank pixels in ring
               NBLANKS,         Blanks in ring
               SEGMLO,          Start angle of segment
               SEGMHI,          End angle of segment
               WIDTH,           Width of ring
               PAMAJ,           Pos. angle Major axis
               INCL,            Inclination of ring
               SUBGRIDn         Subset grid of n-th non subset axis
               FREQ etc.        Physical coordinates of non subset axis.
               LONGGRID         Centre position in grids (longitude)
               LATGRID          Centre position in grids (latitude)
               LONGPHYS         Physical coord. of CP (longitude)
               LATPHYS          Physical coord. of CP (latitude)

               Data for all subsets, radii and segments are stored in one
               table. If you want to append to an existing table, use
               TABAPPEND=Y  The application TABLE is used to do the
               formatting of numbers and the plotting of data.


               Number output (FORMAT=):

               FORMAT= is used to print floating point numbers
               in an user specified format. Input is a format image
               consisting of characters representing the
               wanted output format for 'NUMBER'. The formatted number
               is returned in 'RESULT'.
               The syntax for 'FORMAT'is:

               flag(s):

               Zero or more flags, in any order, which modify the
               meaning of the conversion specification.  The flag
               characters and their meanings are:

               -       The result of the conversion is left-
                       justified within the field.

               +       The result of a signed conversion always
                        begins with a sign, "+" or "-".


              string:
 
              Characters, some with special meaning.
              If the string (f.i. FFFFF.FF or gggg.gg or wwwww)
              contains no dot, the number of characters specify
              a minimum field width.  For an output field, if the
              converted value has fewer characters than the field
              width, it is padded on the left (or right, if the
              left-adjustment flag, - has been given) to the field
              width.
              If the string contains a dot, the total number of
              characters including the dot is the minimum field width
              and the number of characters after the dot is the
              precision.

              The characters are used to determine the conversion
              type. If the string contains an:

              'e' or 'E'
                      The floating-point-number argument is
                      printed in the style [-]drddde+dd,
                      where there is one digit before the
                      radix character, and the number of
                      digits after it is equal to the
                      precision. The E conversion character
                      produces a number with E introducing
                      the exponent instead of e. The
                      exponent always contains at least two
                      digits.  However, if the value to be
                      printed requires an exponent greater
                      than two digits, additional exponent
                      digits are printed as necessary.

              'g' or 'G'

                      The floating-point-number argument is
                      printed in style f or e (or int style E
                      n the case of a G conversion
                      character), with the precision
                      specifying the number of significant
                      digits.  The style used depends on the
                      value converted; style e is used only
                      if the exponent resulting from the
                      conversion is less than -4 or greater
                      than or equal to the precision.

              others
                      Strings without 'e', 'E', 'g' and 'G'
                      indicate a floating point conversion.
                      The floating point number argument is
                      printed in decimal notation in the
                      style [-]dddrddd, where the number of
                      digits after the radix character, r, is
                      equal to the precision specification.

              If the result of a conversion is longer than the
              field width, an asterisk is returned. If the
              input number is a blank, a 'b' is returned.



Examples:             FORMAT= +eeeeee.eeee
                             Number: 43345.5436
                             Result:  +4.3346e+04
                             Remark: exponential format
                                     signed conversion
                                     field width: 12
                                     precision:    4

                      FORMAT= gggg.ggggg
                             Number: 34.43
                             Result:     34.430
                             Remark: Field width is 10
                                     Number of significant digits is 5

                      FORMAT= +ffff.ff
                             Number: 23.456
                             Result:   +23.46
                             Remark: signed conversion

                      FORMAT= -ffff
                             Number: 345.87
                             Result: 346
                             Remark: left justified


                      FORMAT= -+ffff.fff
                             Number: 34.43
                             Result: +34.430
                             Remark: left justified
                                     signed conversion

                      FORMAT= eee.eeee
                             Number: 234.43
                             Result:        *
                             Remark: Field width too small
                                     for conversion

                      FORMAT= ffff.ff
                             Number: blank
                             Result:       b
                             Remark: input was a blank



Example:
               <USER >ellint
               ELLINT  Version 1.0  (Oct 31 1991)
               <USER >ellint ellipse=3 1
               ELLINT  Version 1.0  (Oct 31 1991)
               <USER >INSET="../M8320" f 30
               Set ../M8320 has 3 axes
               RA-NCP             from   -50 to    40
               DEC-NCP            from   -50 to    20
               FREQ-OHEL          from     1 to    59
               Choose on of the following options:
               OPTION=1  Calculate SUM, MEAN, #POINTS along ellipse,
                         in map units
               OPTION=2  Calculate surface brightness in map units
               OPTION=3  Calculate surface brightness in Msun/pc^2
               <USER >OPTION=1
               Calculations in seconds of arc
               <USER >RADII=100:200:50
               <USER >WIDTH=50
               <USER >PA=60
               <USER >INCL=deg(acos(0.5))
               <USER >SEGMENTS=
               <USER >POS=* 13 14 00 * 46 0 0
               <USER >GRDEVICE=x11
               Calculations in seconds of arc
               <USER >FILENAME=

                         ====== ELLINT RESULTS from set ../M8320 (31-OCT-1991) =====
               etc., etc.



Notes:

Updates:       Oct 14, 1991: VOG,  Document created.
               Feb 17, 1994: JPT,  Header lines of file output preceded by "!".
               Mar  8, 1995: VOG,  Removed bug where pixels at 360 deg are not
                                   counted in segment 0-360 deg.
               Jan 24, 2002: VOG,  Increased number of segments to 360



#<
*/


/* Includes */

#include "stdio.h"
#include "stdlib.h"
#include "string.h"
#include "math.h"
#include "cmain.h"
#include "gipsyc.h"
#include "init.h"
#include "finis.h"
#include "float.h"
#include "time.h"
#include "gdsinp.h"
#include "gdsasn.h"
#include "gdsout.h"
#include "setfblank.h"
#include "myname.h"
#include "anyout.h"
#include "userfio.h"
#include "nelc.h"
#include "presetr.h"
#include "gdsbox.h"
#include "gds_exist.h"
#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsc_fill.h"
#include "gdsc_name.h"
#include "gdsi_read.h"
#include "gdsi_write.h"
#include "gdsc_ndims.h"
#include "gdsd_rdble.h"
#include "gdsd_rchar.h"
#include "userchar.h"
#include "userreal.h"
#include "userint.h"
#include "userlog.h"
#include "usertext.h"
#include "cancel.h"
#include "gdspos.h"
#include "error.h"
#include "factor.h"
#include "reject.h"
#include "getrange.h"
#include "minmax3.h"
#include "wminmax.h"
#include "status.h"
#include "axunit.h"
#include "cotrans.h"
#include "getpath.h"
#include "skyrot.h"
#include "timer.h"        /* Returns the cpu time and real time. */
#include "getusernam.h"   /* Returns the user name of the current user.*/
#include "getdate.h"      /* Returns the current time and date as a text string.*/
#include "matrix.h"       /* M[ylo..yhi][xlo..xhi] */


/* GIDS overlays */

#include    "gdi_iinfo.h"
#include    "gdi_open2.h"
#include    "gdi_close.h"
#include    "gdi_frame.h"


/* PGplot includes */

#include "pgplot.h"


/* Related to tables */

#include    "gdsa_crecol.h"  /* Creates a column in a GDS descriptor file.*/
#include    "gdsa_delcol.h"  /* Deletes a column in a GDS table.*/
#include    "gdsa_colinq.h"  /* Give information about columns in a GDS table.*/
#include    "gdsa_wcreal.h"  /* Write real items to a column in a GDS table.*/
#include    "gdsa_wcdble.h"  /* Double.*/
#include    "gdsa_wcint.h"   /* Integer. */


#define AXESMAX    10               /* Max. allowed number of axes in a set */
#define SUBSMAX    2048             /* Max. number of substructures to be specified */
#define MAXRAD     4069             /* Max. # radii allowed */
#define MAXSEGS    360              /* Max # segments to divide ring in */
#define STRLEN     132              /* String length for common strings */
#define LONGSTR    256              /* String length for strings used in ASCII tables */
#define MEDIANBUF  64               /* Start array length of median array per ring per segment */
#define NSTATS     50               /* # times a status is generated */
#define VERSION    "1.0"            /* Version number of this program */
#define NONE       0                /* Default values for use in userxxx routines */
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define FULL_LINE  1                /* PGPLOT related */
#define DASHED     2
#define DOTTED     4
#define YES        1
#define NO         0
#define PI         3.141592653589793
#define ITEMLEN    8                /* Table, column name length */
#define FITSLEN    20               /* Max length of FITS keywords contents from header */
#define VARLEN     132              /* Char. size in tables */
#define COLWIDTH   6                /* Fixed width for some columns in the ASCII output */
#define COLPREC    2                /* Fixed precision .... */
#define SPACE      " "
#define DEFFORMAT1  "ffffff.ff"     /* Default format (in 'printusing') for ASCII columns */
#define DEFFORMAT2  "fffff.ffff"
#define DEFFORMAT3  "fffff.ff"


/* Keywords and messages */

#define KEY_INSET         tofchar("INSET=")
#define MES_INSET         tofchar("Give set (, subsets): ")
#define KEY_OPTION        tofchar("OPTION=")
#define MES_OPTION        tofchar("Give OPTION:      [1]/2/3")
#define KEY_MASS          tofchar("MASS=")
#define MES_MASS          tofchar("Give mass in Msun ")
#define KEY_DISTANCE      tofchar("DISTANCE=")
#define MES_DISTANCE      tofchar("Give distance in Mpc: ")
#define KEY_RADIUS        tofchar("RADII=")
#define KEY_WIDTH         tofchar("WIDTH=")
#define KEY_ANGLE         tofchar("PA=")
#define MES_ANGLE         tofchar("Position angle of ellipse major axis (deg):")
#define KEY_INCLINATION   tofchar("INCL=")
#define MES_INCLINATION   tofchar("Inclination (deg):")
#define KEY_POSITION      tofchar("POS=")
#define KEY_RANGE         tofchar("RANGE=")
#define MES_RANGE         tofchar("Range of intensities to include:  [all levels]")
#define KEY_SEGMENTS      tofchar("SEGMENTS=")
#define MES_SEGMENTS      tofchar("Segments (angles wrt major axis): [0,360]")
#define KEY_ORDER         tofchar("ORDER=")
#define MES_ORDER         tofchar("Table order 1)seg/rad, 2)rad/seg: [1]/2")
#define KEY_ELLIPSE       tofchar("ELLIPSE=")
#define MES_ELLIPSE       tofchar("Give index of ellipse to plot:  [no plot]")
#define KEY_OVERLAP       tofchar("OVERLAP=")
#define MES_OVERLAP       tofchar("Weight data in overlapping regions:    [Y]/N")
#define KEY_PIXELS        tofchar("PIXELS=")
#define MES_PIXELS        tofchar("Calculations and plotting in pixels?    Y/[N]")
#define KEY_FILENAME      tofchar("FILENAME=")
#define MES_FILENAME      tofchar("Write statistics to:       [No file]")
#define KEY_OVERWRITE     tofchar("OVERWRITE=")
#define MES_OVERWRITE     tofchar("File exists, ok to overwrite?    [Y]/N")
#define KEY_OVERLAY       tofchar("OVERLAY=")
#define MES_OVERLAY       tofchar("Overlay ellipse(s) in GIDS?   [N]/Y")
#define KEY_PGBOX         tofchar("PGBOX=")
#define KEY_PGMOSAIC      tofchar("PGMOSAIC=")
#define MES_PGMOSAIC      tofchar("View surface subdivisions x,y:   [1,1]")
#define KEY_GDSTABLE      tofchar("GDSTABLE=")
#define MES_GDSTABLE      tofchar("Store results in table header of INSET=      Y/[N]")
#define KEY_TABNAME       tofchar("TABNAME=")
#define KEY_TABAPPEND     tofchar("TABAPPEND=")
#define KEY_PLOTOPT       tofchar("PLOTOPT=")
#define KEY_SUBPIX        tofchar("SUBPIX=")
#define KEY_FORMAT        tofchar("FORMAT=")
#define MES_MEDIAN        tofchar("Do you want to calculate the MEDIAN also!   Y/[N]")
#define KEY_MEDIAN        tofchar("MEDIAN=")


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


#define MYMAX(a,b) ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b) ( (a) > (b) ? (b) : (a) )
#define NINT(a)    ( (a)<0 ? (int)((a)-.5) : (int)((a)+.5) )
#define RAD(a)     ( (a) * 0.017453292519943295769237 )
#define DEG(a)     ( (a) * 57.295779513082320876798155 )
#define FSWAP(a,b) { float temp=(a);(a)=(b);(b)=temp; } /* Swap 2 floats */




/* Input of set, subsets: */

static fchar    Setin;                 /* Name of the set */
static fint     Subin[SUBSMAX];        /* Array for the subset coordinate words */
static fint     Axnum[AXESMAX];        /* GDSINP axis numbers array */
static fint     Axcount[AXESMAX];      /* GDSINP axis lengths array */
static fint     Setdim;                /* Dimension of the set */
static fint     Subdim;                /* Dimension of the subset */
static fint     Maxaxes  = AXESMAX;    /* Convert parameters to variables */
static fint     Maxsubs  = SUBSMAX;    /* Max. input subsets */
static fint     Setlevel = 0;          /* Indicate set level */


/* Input of area etc.:*/

static fint     GridloI[AXESMAX];      /* Coordinates for input-frame */
static fint     GridhiI[AXESMAX];
static fint     BgridloI[AXESMAX];     /* Coordinates for input-box */
static fint     BgridhiI[AXESMAX];


/* Axis characteristics */

static double   Cdelt[AXESMAX];        /* Grid spacings */
static fchar    Cunit[AXESMAX];        /* Units along axis */
static fchar    Ctype[AXESMAX];        /* Name of axis */
static double   Crpix[AXESMAX];        /* Reference pixel */
static double   Crval[AXESMAX];        /* Values at ref. pixel */


/* ELLINT specific */

static float    Radius[MAXRAD];
static float    Width[MAXRAD];
static float    Phi[MAXRAD];           /* Pos. angle of major axis */
static float    Inc[MAXRAD];           /* Inclination of object */
static float    Annuli[MAXRAD][2];     /* Inner and outer radius of ring */
static float    Annuli_sqr[MAXRAD][2]; /* Radii squared */
static double   Position[2];           /* Central position of all ellipses */
static double   PhysposST[2];          /* Central position in physical coordinates */
static fint     Ellindx[2];            /* Two indices indicating plotted ellipse */
static fint     Option;                /* Ellint option */
static bool     Overlap;               /* Weight data in overlapping regions? */
static float    Sinphi[MAXRAD];        /* Table with angles */
static float    Cosphi[MAXRAD];
static float    Cosinc[MAXRAD];
static float    Segments[MAXSEGS+1];
static fint     Numsegm;
static float    Sumtotgeo;             /* Sum of all complete rings */
static float    Sumtotgeo_bl;
static float    Masstot;
static float    Masstot_bl;
static float    Sum[MAXRAD][MAXSEGS];
static float    Cumsum;                /* Cumulative sum */
static float    Mass;                  /* Mass of galaxy in Msun */
static float    Distance;              /* Distance to galaxy in Mpc */


/* ELLINT ring and segment arrays  */

static float    Sumsqr[MAXRAD][MAXSEGS];       /* Sum of the squares to calculate the variance */
static fint     Num[MAXRAD][MAXSEGS];          /* Number of 'subpixel' hits in a ring/segment */
static fint     Numblanks[MAXRAD][MAXSEGS];    /* Number of 'subpixel' BLANK hits */
static fint     Contrib[MAXRAD][MAXSEGS];      /* Number of different pixels in a ring/segment */
static int      Contribflag[MAXRAD][MAXSEGS];  /* Did a pixel already contribute to ring? */
static float    Datamin[MAXRAD][MAXSEGS];      /* Minimum value  per ring per segment */
static float    Datamax[MAXRAD][MAXSEGS];
static float    Var[MAXRAD][MAXSEGS];          /* Store the variances */
static float    Median[MAXRAD][MAXSEGS];       /* Store the medians */
static float    *medianarray[MAXRAD][MAXSEGS]; /* Data per ring/segment for which median is wanted */




/* Miscellaneous: */

static float    blank;                /* Value of system blank */
static char     messbuf[STRLEN];      /* Buffer for text message */
static char     tablehead[LONGSTR];
static char     tableunits[LONGSTR];
static char     border[LONGSTR];
static int      agreed;               /* Loop control */
static float    Range[2];             /* User given data in/exclude range */
static float    Rmax;                 /* Max radius of all ellipses */
static fint     Nrad;                 /* Number of radii entered by the user */
static fint     Maxpos;               /* Max num. of central positions (1) */
static float    Dx, Dy;               /* Real grid spacings from header */
static float    Absdx, Absdy;         /* Absolute values of spacings */
static double   Convfact;             /* Conversion factor */
static int      conversion;           /* Is conversion possible ? */
static bool     calcmedian;
FILE            *fpOUT;               /* File for table data */
static fchar    Dataunits;            /* Units of data in the set */
static char     axunits[FITSLEN];
static char     datunits[FITSLEN];
static char     surfdunits[FITSLEN];
static char     areaunits[FITSLEN*FITSLEN+1];
static char     posunitx[FITSLEN];
static char     posunity[FITSLEN];
static fint     Tableopt;             /* Order to display table (rad/segm) */
static int      imax, mmax;           /* Loop guards */
static int      standard_table;       /* User used default segments, so */
                                      /* a standard table is given. */
static fint     subsetgrid[AXESMAX];
static double   subsetphys[AXESMAX];



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
#define LIGHTGRAY    15



static void pgmove( float x, float y )
/*------------------------------------------------------------*/
/* PURPOSE: Alternative for pgmove_c                          */
/*------------------------------------------------------------*/
{
   pgmove_c( &x, &y );
}



static void pgdraw( float x, float y )
/*------------------------------------------------------------*/
/* PURPOSE: Alternative for pgdraw_c                          */
/*------------------------------------------------------------*/
{
   pgdraw_c( &x, &y );
}



static void pgrect( float xmin,
                    float ymin,
                    float xmax,
                    float ymax )
/*-------------------------------------------------------------*/
/* PURPOSE: Alternative for pgrect_c. Order of arguments       */
/* is changed!                                                 */
/*-------------------------------------------------------------*/
{
   pgrect_c( &xmin, &xmax, &ymin, &ymax );
}



static void setcolor( fint col )
/*-------------------------------------------------------------*/
/* PURPOSE: Set color to 'col'.                                */
/* Alternative pgsci.                                          */
/*-------------------------------------------------------------*/
{
      pgsci_c( &col );
}




static int inrange( float value,
                    float *Range )
/*-------------------------------------------------------------*/
/* PURPOSE: Check whether value of pixel is within user given  */
/*          range.                                             */
/*-------------------------------------------------------------*/
{
   if (value == blank)
      return( NO );

   if (Range[0] < Range[1])
   {
     if (value >= Range[0] && value <= Range[1])
        return( YES );
   }
   else
   {
     if (value < Range[1] || value > Range[0])
        return( YES );
   }
   return( NO );
}



static void putid( void )
/*-------------------------------------------------------------*/
/* PURPOSE: Put user identification in plot.                   */
/* Create string with user name and date and plot it at the    */
/* right side of the (last) plot.                              */
/*-------------------------------------------------------------*/
{
   fchar     Idstr;
   float     disp, coord, fjust;
   float     newheight, oldheight;
   char      message[512];
   char      append[256];
   fint      oldcolor;


   pgqci_c( &oldcolor  );
   setcolor( WHITE );
   pgqch_c( &oldheight );
   newheight = oldheight/1.4;
   pgsch_c( &newheight );
   fmake( Idstr, 160 );
   getusernam_c( Idstr );
   sprintf( message, "\\fi GIPSY:\\fn: %.*s", nelc_c( Idstr ), Idstr.a );
   getdate_c( Idstr );
   sprintf( append, " %.*s", nelc_c( Idstr ), Idstr.a );
   strcat( message, append );
   disp  = +2.0;
   coord = 0.5;
   fjust = 0.5;
   pgmtxt_c( tofchar("R"), &disp, &coord, &fjust, tofchar(message) );
   pgsch_c( &oldheight );
   setcolor( oldcolor );
}




static int printusing( char     *formatstr,
                       float    number,
                       char     *resultstr )
/*------------------------------------------------------------*/
/* PURPOSE: Print numbers in an user specified format.        */
/* Substitute 'b' for blanks. Do not print a number if the    */
/* length exceeds the fieldlen.                               */
/*------------------------------------------------------------*/
{
   int    len;
   int    before, after;
   char   format[80];
   int    clen;
   char   *instr;
   char   mode;
   int    i;


   len    = strlen( formatstr );
   before = strcspn( formatstr, "." );
   if (before > len)
      before = len;
   if (before!=len)
      after = len - before - 1;
   else
      after = 0;
   strcpy( format, "%" );
   for (i = 0; i < 2; i++)
   {
     mode = formatstr[i];
     if (mode == '+')
        strcat( format, "+" );
     if (mode == '-')
        strcat( format, "-" );
   }
   instr = strpbrk( formatstr, "eEgG" );
   if (instr == NULL)
      mode = 'f';
   else
   /*--------------------------------------------------------------------*/
   /* For e, E, f, g and G conversions, the result shall always contain  */
   /* a radix character, even if no digits follow the radix character.   */
   /* For g and G conversions, trailing zeroes shall not be removed from */
   /* the result as they usually are.                                    */
   /*--------------------------------------------------------------------*/
   {
      mode = *instr;
      strcat( format, "#" );
   }

   sprintf( format, "%.*s%d.%d%c", strlen(format), format, len, after, mode );
   if (number == blank)
      sprintf( resultstr, "%*s", len, "b" );
   else
   {
      clen = sprintf( resultstr, format, number );
      if (clen > len)
         sprintf( resultstr, "%*s", len, "*" );
   }
   return( len );
}




static int insegment( float Angle,
                      float Segm1,
                      float Segm2 )
/*------------------------------------------------------------*/
/* PURPOSE: Check whether Angle of pixel is within user given */
/*          segment.                                          */
/* Return true if range >= 360 degrees.                       */
/*------------------------------------------------------------*/
{
   if ( fabs(Segm2 - Segm1) >= 360.0 )
      return( YES );
   if (Segm1 < Segm2)
   {
     if (Angle >= Segm1 && Angle < Segm2)
        return( YES );
     else
        return( NO );
   }
   if (Segm1 > Segm2)
   {
     if (Angle >= Segm1 || Angle < Segm2)
        return( YES );
     else
        return( NO );
   }

   if (Segm1 == Segm2)
   {
     if (Angle == Segm1)
        return( YES );
     else
        return( NO );
   }

   return( NO );                                      /* Dummy */
}



static float toangle( float Angle )
/*------------------------------------------------------------*/
/* PURPOSE: Return angle between 0 and < 360.0                */
/*------------------------------------------------------------*/
{
   while (Angle < 0.0)
      Angle +=360.0;
   while (Angle > 360.0)
      Angle -=360.0;
   return Angle;
}



static float gettheta( float X,
                       float Y,
                       float Phi,
                       float Crota )
/*------------------------------------------------------------*/
/* Convert angle in XY plane to angle wrt major axis of map.  */
/* 'atan2': Returns in radians the arc tangent of two real    */
/* numbers. The arguments must not both be 0.0. If number-2   */
/* is 0.0, the absolute value of the result is pi/2. If       */
/* number-1 is 0.0, the result is 0.0 if number-2 is positive */
/* and pi if number-2 is negative. Otherwise, the result is   */
/* in the range -pi, exclusive, through +pi, inclusive, and   */
/* is calculated as follows: arctan (argument_1/argument_2)   */
/* If number-1 is positive, the result is positive; if        */
/* number-1 is negative, the result is negative.              */
/*------------------------------------------------------------*/
{
   float     theta;


   if ( X == 0.0 && Y == 0.0 )
      theta = 0.0;
   else
      theta = DEG( atan2( Y, X ) );         /* Angle in deg. wrt pos x axis */
                                            /* Counted anti-clockwise */

   /* Now correct for angle offsets: */
   /* 360 = (90-theta) + Crota + phi */

   theta = theta + 270.0  - Phi - Crota;
   theta = toangle( theta );                /* Convert to [0,360] */
   return( theta );
}



void initplot( bool  Gidsoverlay,
               fint  xsub,
               fint  ysub )
/*------------------------------------------------------------*/
/* PURPOSE: Initialize plot software. Set viewport and output */
/*          dimensions.                                       */
/* If output device is a printer, ask user for line width.    */
/*------------------------------------------------------------*/
{
   fint    pgunit;                 /* Ignored by 'pgbeg', use 0 */
   fchar   Ffile;                  /* Device specification */
   fint    nxysub[2];              /* Number of subdivisions */
   float   width;                  /* Width of output on paper */
   float   aspect;                 /* Aspect ratio of output on paper */
   float   uservals[2];            /* Array version of above */
   fint    nitems;                 /* Use in userxxx routines */
   fint    dfault;                 /* Use in userxxx routines */
   fint    r1;                     /* Return value or level */
   fint    linewidth;              /* Width of lines on output device */
   fint    agreed;                 /* Loop guard */
   fint    off;


   pgunit = 0;                            /* Ignored by 'pgbeg' */
   fmake( Ffile, 10 );
   Ffile = tofchar( "?" );                /* 'pgbeg' will prompt the user */
                                          /* to supply a string. */
   nxysub[0] = xsub;                      /* Default no subdivisions in plot */
   nxysub[1] = ysub;


   /* Set window and viewport */

   if (Gidsoverlay)
      r1 = pgbeg_c( &pgunit,
                    tofchar( "gids//append" ),
                    &nxysub[0],
                    &nxysub[1] );
   else
      r1 = pgbeg_c( &pgunit,
                    Ffile,
                    &nxysub[0],
                    &nxysub[1] );

   if (r1 != 1)
      errorf( 4, "Cannot open output device" );

   /* No NEXTPAGE= keyword */
   off = tobool( 0 );
   pgask_c( &off );

   /* Change size of the view surface to a specified width */
   /* and aspect ratio (=height/width) */

   nitems = 2;
   dfault = HIDDEN;
   uservals[0] = 0.0;
   uservals[1] = 1.0;
   r1 = userreal_c( uservals,
                    &nitems,
                    &dfault,
                    tofchar("PGPAPER="),
                    tofchar("Give width(cm), aspect ratio: [0.0,1.0]") );
   if (r1 > 0)
   {
      /* If width = 0.0 then the program will select the largest */
      /* view surface */
      width  = uservals[0];
      /* Convert from cm to inches */
      width /= 2.54;
      aspect = uservals[1];
      pgpap_c( &width, &aspect );
   }

   /* Get device-type code name of the current PGPLOT device */
   /* If the destination is a printer (=every destination  */
   /* except the Tektronix device), use thick lines in the plot */


   nitems = 1;
   dfault = HIDDEN;
   do
   {
      linewidth = 2;
      r1 = userint_c( &linewidth,
                      &nitems,
                      &dfault,
                      tofchar("PGWIDTH="),
                      tofchar("Give line width (1-21):  [2]") );
      agreed = (linewidth >= 1 && linewidth <= 21);
      if (!agreed)
         reject_c( tofchar("PGWIDTH="), tofchar("Invalid number") );
   }
   while (!agreed);
   pgslw_c( &linewidth );

   /* Set viewport */
   {
     float Xl, Xr, Yb, Yt;
     Xl = 0.15;
     Xr = 0.9;
     Yb = 0.15;
     Yt = 0.9;
     pgsvp_c(&Xl, &Xr, &Yb, &Yt);      /* Set viewport */
   }
   cancel_c( tofchar("PGPAPER=") );
   cancel_c( tofchar("PGWIDTH=") );
   cancel_c( tofchar("GRDEVICE=") );
}



void drawbox( float   *VXmin,
              float   *VXmax,
              float   *VYmin,
              float   *VYmax,
              float    Dx,
              float    Dy,
              fint    *Gidsblo,
              fint    *Gidsbhi,
              float   *Gidsflo,
              float   *Gidsfhi,
              bool     conversion,
              bool     Gidsoverlay,
              fchar    Toptitle )
/*------------------------------------------------------------*/
/* PURPOSE: Draw box and labels etc. for ellipse overview.    */
/* If an overlay on GIDS is wanted, return 'Gidsblo/bhi'.     */
/*------------------------------------------------------------*/
{
   float   charsize;
   float   Xtick, Ytick;           /* Tick values for plot box */
   fint    nxsub, nysub;           /* Subintervals of major coordinate interval */
   float   delta;
   fchar   FXlabel, FYlabel;
   float   XYminmax[4];
   fint    dfault;
   fint    nitems;
   fint    font;
   fint    r1;
   float   Xmin, Xmax, Ymin, Ymax;


   Xmin = *VXmin; Xmax = *VXmax;
   Ymin = *VYmin; Ymax = *VYmax;
   setcolor( WHITE );
   charsize = 1.0;                         /* Set character height */
   pgsch_c( &charsize );
   pgpage_c();                             /* Start with new page */

   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0)
      delta = 1.0;
   Xmin -= delta;
   Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0)
      delta = 1.0;
   Ymin -= delta;
   Ymax += delta;

   if (Gidsoverlay)
   {
      XYminmax[0] = ((float) Gidsblo[0]) * Dx;
      XYminmax[1] = ((float) Gidsblo[1]) * Dy;
      XYminmax[2] = ((float) Gidsbhi[0]) * Dx;
      XYminmax[3] = ((float) Gidsbhi[1]) * Dy;
   }
   else
   {
      XYminmax[0] = Xmin;
      XYminmax[1] = Ymin;
      XYminmax[2] = Xmax;
      XYminmax[3] = Ymax;
   }
   nitems = 4;
   dfault = HIDDEN;
   (void) sprintf( messbuf,
                  "Corners of box Xl,Yl, Xh,Yh:  [%g,%g,%g,%g]",
                   XYminmax[0],
                   XYminmax[1],
                   XYminmax[2],
                   XYminmax[3] );
   r1 = userreal_c( XYminmax,
                    &nitems,
                    &dfault,
                    KEY_PGBOX,
                    tofchar(messbuf)  );
   cancel_c( KEY_PGBOX );

   Xmin = XYminmax[0];
   Ymin = XYminmax[1];
   Xmax = XYminmax[2];
   Ymax = XYminmax[3];

   if (Gidsoverlay)
   /*----------------------------------------*/
   /* Adjust window for GIDS overlays.       */
   /*----------------------------------------*/
   {
      float     Xl, Xr, Yb, Yt;
      float     lx, ly;

      Xl = 0.0+0.1; Xr = 1.0-0.1; Yb = 0.0+0.1; Yt = 1.0-0.1;
      pgsvp_c(&Xl, &Xr, &Yb, &Yt);
      Xmin = Dx * Gidsflo[0];
      Xmax = Dx * Gidsfhi[0];
      Ymin = Dy * Gidsflo[1];
      Ymax = Dy * Gidsfhi[1];
      lx = 0.1 * (Xmax - Xmin);
      ly = 0.1 * (Ymax - Ymin);
      Xmin += lx;
      Xmax -= lx;
      Ymin += ly;
      Ymax -= ly;
   }
   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );   /* Set window to draw box with physical coords */
   font   = 1;
   pgscf_c( &font );                         /* Normal font */
   Xtick  = 0.0;
   Ytick  = 0.0;                             /* PGPLOT selects tick intervals */
   nxsub = nysub = 0;                        /* Default subintervals */
   if (Gidsoverlay)
      pgbox_c( tofchar("BCINST"),
               &Xtick, &nxsub,
               tofchar("BCINSTV"),
               &Ytick, &nysub );
   else
      pgbox_c( tofchar("BCNST"),
               &Xtick, &nxsub,
               tofchar("BCNSTV"),
               &Ytick, &nysub );

   *VXmin = Xmin;
   *VXmax = Xmax;
   *VYmin = Ymin;
   *VYmax = Ymax;

   /* Labels: */
   fmake( FXlabel,    STRLEN );
   fmake( FYlabel,    STRLEN );
   if (conversion) {
      FYlabel = tofchar("Y arcsec" );
      FXlabel = tofchar("X arcsec" );
   }
   else {
      FYlabel = tofchar("Y pixels" );
      FXlabel = tofchar("X pixels" );
   }
   pglab_c( FXlabel, FYlabel, Toptitle );
}



static void rotate( float  X,
                    float  Y,
                    float *Cpos,
                    float  Angle,
                    float *Xrot,
                    float *Yrot )
/*------------------------------------------------------------*/
/* PURPOSE: Rotate over 'Angle' degrees anti clockwise wrt.   */
/*          central position given in Cpos.                   */
/*------------------------------------------------------------*/
{
   float    Xx, Yy;
   float    Xr, Yr;
   float    CosP, SinP;


   CosP = (float)cos( RAD(Angle) );
   SinP = (float)sin( RAD(Angle) );
   Xr = X;  Yr = Y;       /* Correct for central position */

   Xx = Xr * CosP - Yr * SinP;
   Yy = Xr * SinP + Yr * CosP;

   *Xrot = Xx + Cpos[0];
   *Yrot = Yy + Cpos[1];
   return;
}



void ellfie( float Major,
             float Minor,
             float Alpha,
             float *xp,
             float *yp )
/*------------------------------------------------------------*/
/* PURPOSE: For given ellipse, return x,y on ellipse for given*/
/*          angle.                                            */
/* Calculate for given angle the coordinates of a standard    */
/* ellipse b**2.x**2 + a**2.y**2 = a**2.b**2 in Polar         */
/* coordinates and transform to Rectangular coordinates.      */
/*------------------------------------------------------------*/
{
   float   p1, p2, p3;
   float   R;
   float   denom;

   p1 = Minor * (float)cos(RAD(Alpha));
   p2 = Major * (float)sin(RAD(Alpha));
   p3 = Minor * Major;
   denom = (p1*p1 + p2*p2);
   if (denom == 0.0)
      R = 0;
   else R =
      sqrt( p3*p3 / denom );
   *xp = R * (float)cos(RAD(Alpha));
   *yp = R * (float)sin(RAD(Alpha));
   return;
}



void drawellipse( float    Radius,
                  float    Phi,
                  float    Inclination,
                  float    Width,
                  float    SegmentL,
                  float    SegmentH,
                  double  *Position,
                  float    Dx,
                  float    Dy,
                  fint    *Gidsblo,
                  fint    *Gidsbhi,
                  float   *Gidsflo,
                  float   *Gidsfhi,
                  float    Crota,
                  bool     conversion,
                  bool     Gidsoverlay )
/*------------------------------------------------------------*/
/* PURPOSE: Draw box &  ellipse.                              */
/* The function parameters 1 to 6 are ellipse characteristics.*/
/* The other parameters are properties of the field. Note that*/
/* all calculations are done in arcsec. If conversion is not  */
/* possible the grid spacing is Dx=Dy=1                       */
/*------------------------------------------------------------*/
{
   float   Major, Minor;
   float   xp, yp;
   float   xr, yr;
   float   MapPA = 0.0;
   float   Cpxy[2];                        /* Central position in floats */
   float   RotPA;
   float   TxtPA;                          /* Text angle in 'pgptxt' */
   float   Alpha;
   float   Txtjust;                        /* Text justification in 'pgptxt' */
   fint    Linestyle;
   float   charsize;
   float   X[361], Y[361];
   fint    i;
   fchar   FTOPlabel;

   static  int     first = YES;            /* Static is essentail here */
   static  float   Xmin, Xmax, Ymin, Ymax; /* Static is essentail here */



   Major = Radius;                                /* In arcsecs or pixels */
   Minor = Major * (float) cos(RAD(Inclination));
   Cpxy[0] = (float) Position[0] * Dx;
   Cpxy[1] = (float) Position[1] * Dy;


   if (first)
   {
      initplot( Gidsoverlay, 1, 1 );                 /* Initialize graphics */
      Xmin = ((float) Gidsblo[0]) * Dx;
      Xmax = ((float) Gidsbhi[0]) * Dx;
      Ymin = ((float) Gidsblo[1]) * Dy;
      Ymax = ((float) Gidsbhi[1]) * Dy;

      if (Xmin > Xmax)
         FSWAP( Xmin, Xmax );
      if (Ymin > Ymax)
         FSWAP( Ymin, Ymax );

      /* Create label at top of plot. Used in function 'drawbox' */
      fmake( FTOPlabel,  STRLEN );
      FTOPlabel.l = sprintf( FTOPlabel.a,
              "ELLINT: a=%g b=%g \\gf\\dwrt N\\u=%g\\uo\\d, i=%g\\uo\\d, PA\\dmap\\u=%g\\uo\\d",
               Major,
               Minor,
               Phi,
               Inclination,
               Crota );


      drawbox( &Xmin,            /* A box in PHYSICAL coordinates! */
               &Xmax,
               &Ymin,
               &Ymax,
               Dx,
               Dy,
               Gidsblo,
               Gidsbhi,
               Gidsflo,
               Gidsfhi,
               conversion,
               Gidsoverlay,
               FTOPlabel );

      /*--------------------------------------------------*/
      /* Plot dotted axes through central position        */
      /*--------------------------------------------------*/
      setcolor( CYAN );
      xp = Cpxy[0]; yp = Ymin;
      pgmove( xp, yp );
      yp =  Ymax;
      pgdraw( xp, yp );
      xp = Xmin; yp = Cpxy[1];
      pgmove( xp, yp );
      xp = Xmax;
      pgdraw( xp, yp );


      /*--------------------------------------------------*/
      /* Draw rotated axis for pos. angle of map (crota2) */
      /* indicating the direction of North.               */
      /*--------------------------------------------------*/
      setcolor( RED );
      MapPA = Crota + 90.0;
      Txtjust = 0.5;                                /* Center text in 'pgptxt' */
      Linestyle = FULL_LINE;
      pgsls_c( &Linestyle );
      pgmove( Cpxy[0], Cpxy[1] );
      xp = 0.45 * (Xmax - Xmin);                    /* Arbitrary length for North axis */
      yp = 0.0;
      /* All in arcsec */
      rotate( xp, yp, Cpxy, MapPA, &xr, &yr );
      pgdraw( xr, yr );
      /* Plot a sign at end of line. Number is a Hershey number */
      (void) sprintf( messbuf, "\\(0852)" );
      charsize = 0.6;
      pgsch_c( &charsize );
      pgptxt_c( &xr, &yr, &Crota, &Txtjust, tofchar(messbuf) );
      charsize = 1.0;
      pgsch_c( &charsize );
      xp = 0.50 * (Xmax - Xmin);                    /* Little shift */
      yp = 0.0;
      rotate( xp, yp, Cpxy, MapPA, &xr, &yr );
      TxtPA = 0.0;
      pgptxt_c( &xr, &yr, &TxtPA, &Txtjust, tofchar("N") );

      first = NO;
   }


   /*--------------------------------------------------*/
   /* Draw ellipse axes and mark with a for major and  */
   /* b for minor.                                     */
   /*--------------------------------------------------*/
   TxtPA   = 0.0;
   Txtjust = 0.5;                                  /* Center text in 'pgptxt' */
   MapPA   = Crota + 90.0;

   setcolor( CYAN );

   RotPA = Phi + 90.0 + Crota;
   xp = -1.0*Major; yp = 0.0;
   rotate( xp, yp, Cpxy, RotPA, &xr, &yr );
   pgmove( xr, yr );
   xp =  Major;
   rotate( xp, yp, Cpxy, RotPA, &xr, &yr );
   pgdraw( xr, yr );
   pgptxt_c( &xr, &yr, &TxtPA, &Txtjust, tofchar("a") );
   xp = 0.0; yp = -1.0 * Minor;
   rotate( xp, yp, Cpxy, RotPA, &xr, &yr );
   pgmove( xr, yr );
   yp = Minor;
   rotate( xp, yp, Cpxy, RotPA, &xr, &yr );
   pgdraw( xr, yr );
   pgptxt_c( &xr, &yr, &TxtPA, &Txtjust, tofchar("b") );

   /*--------------------------------------------------*/
   /* Draw part of circle indicating the angle 'phi'.  */
   /*--------------------------------------------------*/
   if (Phi > 0.0)
   {
      float   Delta;
      float   R;

      R = 0.25 * Major;
      Delta = Phi / 60.0;
      for (Alpha = 0.0, i = 0; Alpha < Phi; Alpha += Delta) {
         ellfie( R, R, Alpha, &xp, &yp );
         rotate( xp, yp, Cpxy, MapPA, &xr, &yr );
         X[i] = xr;
         Y[i] = yr;
         i++;
      }
      pgline_c( &i, X, Y );

      /* Plot an arrow at end of circle part. Number is a Hershey number */
      TxtPA = MapPA + Phi;
      charsize = 0.6;
      pgsch_c( &charsize );
      pgptxt_c( &xr, &yr, &TxtPA, &Txtjust, tofchar("\\(0852)") );
      charsize = 1.0;
      pgsch_c( &charsize );
      Alpha = Phi / 2.0;
      R = 0.30 * Major;
      ellfie( R, R, Alpha, &xp, &yp );
      rotate( xp, yp, Cpxy, MapPA, &xr, &yr );
      TxtPA = 0.0;
      pgptxt_c( &xr, &yr, &TxtPA, &Txtjust, tofchar("\\gf") ); /* Greek, phi */
   }

   /*-----------------------------------------------------------*/
   /* Switch to Polar coordinates: x = r cos(), y = r sin()     */
   /* Substitute in b**2.x**2 + a**2.y**2 = a**2.b**2 and solve */
   /* for r for a sequence of angles. Substitute back to find   */
   /* x and y. Rotate and plot.                                 */
   /*-----------------------------------------------------------*/
   setcolor( YELLOW );

   /* Smallest ellipse */

   Major = MYMAX( (Radius - 0.5 * Width), 0.0);
   Minor = Major * (float)cos(RAD(Inclination));
   for (Alpha = 0.0, i = 0; Alpha <= 360.0; Alpha += 1.0) {
      ellfie( Major, Minor, Alpha, &xp, &yp );
      rotate( xp, yp, Cpxy, RotPA, &xr, &yr );
      X[i] = xr;
      Y[i] = yr;
      i++;
   }
   pgline_c( &i, X, Y );

   /* Biggest ellipse: */

   Major = (Radius + 0.5 * Width);
   Minor =  Major * (float)cos(RAD(Inclination));
   for (Alpha = 0.0, i = 0; Alpha <= 360.0; Alpha += 1.0) {
      ellfie( Major, Minor, Alpha, &xp, &yp );
      rotate( xp, yp, Cpxy, RotPA, &xr, &yr );
      X[i] = xr;
      Y[i] = yr;
      i++;
   }
   pgline_c( &i, X, Y );

   /*--------------------------------------------------*/
   /* Plot the delimeter lines for this segment.       */
   /*--------------------------------------------------*/
   charsize = 0.65;                 /* Use smaller font for display of angles */
   pgsch_c( &charsize );
   Alpha = SegmentL;
   ellfie( Major, Minor, Alpha, &xp, &yp );
   rotate( xp, yp, Cpxy, RotPA, &xr, &yr );
   pgmove( Cpxy[0], Cpxy[1]);
   pgdraw( xr, yr );
   (void) sprintf( messbuf, "%g" , SegmentL );
   TxtPA = 0.0;
   pgptxt_c( &xr, &yr, &TxtPA, &Txtjust, tofchar(messbuf) );

   Alpha = SegmentH;
   ellfie( Major, Minor, Alpha, &xp, &yp );
   rotate( xp, yp, Cpxy, RotPA, &xr, &yr );
   pgmove( Cpxy[0], Cpxy[1] );
   pgdraw( xr, yr );
   (void) sprintf( messbuf, "%g" , SegmentH );
   pgptxt_c( &xr, &yr, &TxtPA, &Txtjust, tofchar(messbuf) );

   charsize = 1.0;                                /* Reset */
   setcolor( WHITE );
   pgsch_c( &charsize );

   return;
}



static char *makedate( char *buffer )
/*------------------------------------------------------------*/
/* PURPOSE: Return date in format : 29-NOV-1990               */
/*------------------------------------------------------------*/
{
   struct tm   *ptr;
   time_t      lt;


   lt    = time(NULL);                         /* Get the coded calendar time */
   ptr   = localtime(&lt);
   strftime( buffer, STRLEN, "%d-%b-%Y", ptr );
   return( buffer );
}



void plotpoint( float xp, float yp )
/*------------------------------------------------------------*/
/* PURPOSE:  Plot this one point as a dot.                    */
/*------------------------------------------------------------*/
{
   fint    Numpt = 1;
   fint    Symbol = 1;                          /* A dot but not the smallest */

   pgpt_c( &Numpt, &xp, &yp, &Symbol );
}



static void dms( double degrees,
                 int    prec,
                 char   *convstr,
                 double *DMS )
/*------------------------------------------------------------*/
/* PURPOSE: Convert degrees to deg/min/sec                    */
/*------------------------------------------------------------*/
{
   double    seconds;
   int       Idegs;
   double    min;
   int       Imin;
   int       negative;
   double    power;


   degrees = fmod( degrees, 360.0 );   /* -360.0 < deg < 360.0 */
   power = pow( 10.0, (double) prec );
   negative = 0;
   if (degrees < 0)
   {
      negative = 1;
      degrees = fabs(degrees);
   }
   Idegs   = (int) degrees;
   min     = degrees*60.0 - ((double)Idegs)*60.0;
   Imin    = (int) min;
   seconds = min*60.0 - ((double)Imin*60.0 );
   /* Avoid rounding by formatting */
   seconds = (double) ((int) (seconds * power) ) / power;

   if (negative)
   {
      /* Numbers between -90 and 90 */
      (void) sprintf( convstr, "-%2dd%2dm%5.*fs", Idegs, Imin, prec, seconds );
      Idegs *= -1;
   }
   else
      (void) sprintf( convstr, "%2dd%2dm%5.*fs", Idegs, Imin, prec, seconds );

   DMS[0] = (double) Idegs;
   DMS[1] = (double) Imin;
   DMS[2] = seconds;
}



static void hms( double  degrees,
                 int     prec,
                 char    *convstr,
                 double  *HMS )
/*------------------------------------------------------------*/
/* PURPOSE: Convert degrees to hours/min/sec                  */
/*------------------------------------------------------------*/
{
   double    hours, min, seconds;
   int       Ihours, Imin;
   double    power;


   /* Input must be >= 0.0 degrees and < 360.0 degrees*/
   degrees = fmod( degrees, 360.0 );
   while (degrees < 0.0)
      degrees += 360.0;
   power = pow( 10.0, (double) prec );
   hours   = degrees / 15.0;
   Ihours  = (int) hours;
   min     = hours*60.0 - ((double)Ihours)*60.0;
   Imin    = (int) ( min );
   seconds = min*60.0 - ((double)Imin)*60.0;

   /* Avoid format problems in sprintf */
   seconds = (double) ((int) (seconds * power) ) / power;
   (void) sprintf( convstr, "%2dh%2dm%5.*fs", Ihours, Imin, prec, seconds );

   HMS[0] = (double) Ihours;
   HMS[1] = (double) Imin;
   HMS[2] = seconds;
}



void tableheader( fint   Subset,
                  int    toarcsec,
                  fint   *subpix )
/*------------------------------------------------------------*/
/* PURPOSE: Create header for (SCREEN) table in Log file or   */
/*          disk file. Display grids and level.               */
/*------------------------------------------------------------*/
{
   fint      R1;
   fint      Setlevel;
   fchar     Objname;
   char      strbuf1[STRLEN];
   char      strbuf2[STRLEN];
   char      convstr1[FITSLEN];               /* Convert degrees */
   char      convstr2[FITSLEN];               /* Convert degrees */
   fint      Conversion;
   double    Coordin[AXESMAX];
   double    Coordout[AXESMAX];
   fint      Direction;
   fchar     Axunits1, Axunits2;
   double    Cfact;
   int       i, j;
   double    Physpos[2];
   double    dummy[3];



   /* Line 1:  ELLINT results from set: setname (date)*/

   Setlevel = 0;
   anyoutf(3, " " );
   (void) sprintf( strbuf2, "          ====== ELLINT RESULTS from set %.*s (%s) =====",
                   nelc_c(Setin),
                   Setin.a,
                   makedate(messbuf) );
   anyoutf( 3, strbuf2 );
   anyoutf( 3, " " );
   if (fpOUT != NULL)
      (void) fprintf( fpOUT, "!%s\n!\n", strbuf2 );


   /* Line 2: (Object: NGC...)   */

   fmake( Objname, STRLEN );
   R1 = 0;
   gdsd_rchar_c( Setin, tofchar("OBJECT"), &Setlevel, Objname, &R1 );
   if (R1 >= 0)
   {
      (void) sprintf( strbuf1,
                     "(Object: %.*s)",
                      (int) nelc_c( Objname ),
                      Objname.a );
      anyoutf( 3, strbuf1 );
      if (fpOUT != NULL)
         (void) fprintf( fpOUT, "!%s\n", strbuf1 );
   }


   /* Line 3: Central Position =  (... , ...) grids */

   (void) sprintf( strbuf1,
                  "Central position = (%f, %f) grid units" ,
                   Position[0],
                   Position[1] );
   anyoutf( 3, strbuf1 );
   if (fpOUT != NULL)
      (void) fprintf( fpOUT, "!%s\n", strbuf1 );


   /* Line 4:                  ....   .... degrees */

   Coordin[0] = Position[0];
   Coordin[1] = Position[1];
   Direction = 1;                           /* grid coord. -> physical coord. */

   /*------------------------------------------------------------*/
   /* Input coordinates are the coordinates in the subset        */
   /* Cotrans translates the subset coordinate word to internal  */
   /* coordinates. After transformation, these coordinates are   */
   /* in original set order and can be obtained via the Axnum    */
   /* numbers.                                                   */
   /*------------------------------------------------------------*/

   R1 = cotrans_c( Setin, &Subset, Coordin, Coordout, &Direction );
   Physpos[0] = Coordout[Axnum[0]-1];
   Physpos[1] = Coordout[Axnum[1]-1];
   PhysposST[0] = Physpos[0];
   PhysposST[1] = Physpos[1];
   fmake( Axunits1, FITSLEN );
   fmake( Axunits2, FITSLEN );
   R1 = axunit_c( Setin, &Axnum[0], Axunits1 );
   R1 = axunit_c( Setin, &Axnum[1], Axunits2 );
   (void) sprintf( strbuf1,
                  "                 =  %f (%.*s), %f (%.*s)" ,
                   Physpos[0],
                   nelc_c( Axunits1 ),
                   Axunits1.a,
                   Physpos[1],
                   nelc_c( Axunits2 ),
                   Axunits2.a );
   anyoutf( 3, strbuf1 );
   if (fpOUT != NULL)
      (void) fprintf( fpOUT, "!%s\n", strbuf1 );


   /* Line 5:  = ..h..m..s   ..d..m..s   */

   /* Check on spatial axes and try to read from header */
   if ( strstr( Ctype[Axnum[0]-1].a, "RA" )  &&
        strstr( Ctype[Axnum[1]-1].a, "DEC" ) )
   {
      Conversion = factor_c( Cunit[Axnum[0]-1], tofchar("DEGREE"), &Cfact );
      if (Conversion == 0)
      {
         Physpos[0] *= Cfact;
         Conversion = factor_c( Cunit[Axnum[1]-1], tofchar("DEGREE"), &Cfact );
         if (Conversion == 0)
         {
            Physpos[1] *= Cfact;
         }
      }
      if (Conversion == 0)
      {
         /* RA, DEC axis and units converted to degrees */

         hms( Physpos[0], 2, convstr1, dummy );         /* Now convert to hms */
         dms( Physpos[1], 1, convstr2, dummy );         /* Convert to deg/m/s */
         (void) sprintf( strbuf1,
                         "                 =  %s  %s , %s  %s" ,
                         strtok( Ctype[Axnum[0]-1].a, " -" ),
                         convstr1,
                         strtok( Ctype[Axnum[1]-1].a, " -" ),
                         convstr2 );
         anyoutf( 3, strbuf1 );
         if (fpOUT != NULL)
            (void) fprintf( fpOUT, "!%s\n", strbuf1 );
      }
   }


   /* Line 6: something like: Subset: FREQ = 1.4e9 Hz */

   if (Subdim == Setdim)
   {
      strcpy( strbuf1, "Level: Set level" );
   }
   else
   {
      strcpy( strbuf1, "Subset: " );
      for (i = Subdim; i < (int) Setdim; i++)
      {
         (void) sprintf( strbuf1,
                        "%.*s %s ",
                         strlen( strbuf1 ),
                         strbuf1,
                         strtok( Ctype[Axnum[i]-1].a, " -" ) );
      }
      (void) sprintf( strbuf1,
                     "%.*s = ",
                      strlen( strbuf1 ),
                      strbuf1 );
      for ( i = Subdim, j = 0; i < Setdim; i++, j++ )
      {
         fmake( Axunits1, FITSLEN );
         R1 = axunit_c( Setin, &Axnum[i], Axunits1 );
         (void) sprintf( strbuf1,
                         "%.*s %f (%.*s) ",
                         strlen( strbuf1 ),
                         strbuf1,
                         Coordout[Axnum[i]-1],
                         nelc_c( Axunits1 ),
                         Axunits1.a );
         subsetgrid[j] = gdsc_grid_c( Setin, &Axnum[i], &Subset, &R1 );
         subsetphys[j] = Coordout[Axnum[i]-1];
      }
   }
   anyoutf( 3, strbuf1 );
   if (fpOUT != NULL)
      (void) fprintf( fpOUT, "!%s\n", strbuf1 );
   (void) sprintf( strbuf1, "Biggest ellipse uses box: [%d %d, %d %d]",
                   BgridloI[0],
                   BgridhiI[0],
                   BgridloI[1],
                   BgridhiI[1] );
   anyoutf( 3, strbuf1 );
   if (fpOUT != NULL)
      fprintf( fpOUT, "!%s\n!\n", strbuf1 );

   if (toarcsec)
   {
      sprintf( strbuf1, "Area 1 pixel dx, dy = %f, %f (arcsec^2)", Dx, Dy );
      anyoutf( 3, strbuf1 );
      if (fpOUT != NULL)
          fprintf( fpOUT, "!%s\n!", strbuf1 );
   }
   if (subpix[0] != 1 && subpix[1] != 1)
   {
      sprintf( strbuf1, "One pixel is divided in %dx%d 'subpixels' in x and y",
               subpix[0], subpix[1] );
   }
   else
      strcpy( strbuf1, "No 'subsampling' (SUBPIX=1 1)" );
   anyoutf( 3, strbuf1 );
   if (fpOUT != NULL)
   {
      fprintf( fpOUT, "!%s\n", strbuf1 );
      fprintf( fpOUT, "!\n");
   }
   anyoutf( 3, " " );
}



static bool preparegids( fchar  Gidsset,
                         fint   Gidssubset,
                         fint  *Gidsblo,
                         fint  *Gidsbhi,
                         float *Gidsflo,
                         float *Gidsfhi )
/*------------------------------------------------------------*/
/* PURPOSE: Check whether a GIDS window is open and a set is  */
/*          displayed.                                        */
/* Get info about size and position of the displayed image.   */
/* Calculate the offset (from lower left corner) in mm        */
/*------------------------------------------------------------*/
{
   fint         display_id;                     /* id of display */
   fint         display_stat;                   /* display operation status */
   fint         gerror = 0;
   fint         R1;


   display_id = gdi_open2_c( tofchar(" ") );    /* open display device */
   if (display_id < 0)                          /* error opening display */
   {
      anyoutf( 16, "Cannot interact with GIDS: No display server running!" );
      return( NO );
   }
   display_stat = gdi_iinfo_c( &display_id ,    /* id of display */
                               Gidsset ,        /* name of set */
                               &Gidssubset ,    /* subset level */
                               Gidsblo,         /* lower left frame boundary */
                               Gidsbhi );       /* upper right frame boundary */

   if (display_stat < 0)                        /* error obtaining info */
   {
      anyoutf( 1, "No image loaded! Use VIEW if you want to overlay ellipses!" );
      return( NO );
   }
   if (!tobool( gds_exist_c( Gidsset, &gerror ) ) )
   {
      anyoutf( 1, "Set not present!" );
      return( NO );
   }
   if (gdsc_ndims_c( Gidsset, &Gidssubset ) != 2)
   {
      anyoutf( 1, "Wrong dimension!" );
      return( NO );
   }
   (void) sprintf( messbuf,
                  "Displayed set [%.*s] has box [%d %d %d %d]",
                   nelc_c(Gidsset),
                   Gidsset.a,
                   Gidsblo[0],
                   Gidsblo[1],
                   Gidsbhi[0],
                   Gidsbhi[1] );
   anyoutf( 8, messbuf );


   R1 = gdi_frame_c( &display_id ,    /* id of display */
                     Gidsflo,         /* lower left frame boundary of total GIDS area in grids */
                     Gidsfhi );

   if (R1 != 0)
   {
      anyoutf( 1,
               "Cannot obtain info about frame currently on display! (err=%d)",
               R1 );
      Gidsflo[0] = (float) Gidsblo[0];
      Gidsfhi[0] = (float) Gidsbhi[0];
      Gidsflo[1] = (float) Gidsblo[1];
      Gidsfhi[1] = (float) Gidsbhi[1];
   }

   (void) sprintf( messbuf, "Displayed set [%.*s] has frame [%f %f %f %f]",
                   nelc_c(Gidsset), Gidsset.a,
                   Gidsflo[0], Gidsflo[1], Gidsfhi[0], Gidsfhi[1] );
   anyoutf( 16, messbuf );

   display_stat = gdi_close_c( &display_id );   /* close display */
   return( YES );
}



static void checkcol( fchar Tname,
                      fint  tablev,
                      char *colname,
                      char *coltype,
                      char *colunit,
                      char *comments,
                      bool  append,
                      fint *collen )
/*------------------------------------------------------------*/
/* PURPOSE: Check whether this column already exist.          */
/* If so, delete it if you do not want to append.             */
/*------------------------------------------------------------*/
{
   fchar   Cname;
   fchar   Units;
   fchar   Comment;
   fchar   Type;
   fint    r1, r2;
   fint    nrows = 0;
   bool    exist;


   fmake( Cname,   ITEMLEN );
   fmake( Units,   VARLEN );
   fmake( Comment, VARLEN );
   fmake( Type,    VARLEN );
   Cname.l   = str2char( colname, Cname );
   r1 = 0;
   gdsa_colinq_c( Setin,
                  &tablev,
                  Tname,
                  Cname,
                  Type,
                  Comment,
                  Units,
                  &nrows,
                  &r1 );
   exist = (r1 >= 0);
   if (exist && !append)
   {
      r2 = 0;
      gdsa_delcol_c( Setin, &tablev, Tname, Cname, &r2 );
      exist = NO;
   }
   if (!exist)
   {
      Type.l    = str2char( coltype,  Type );
      Comment.l = str2char( comments, Comment);
      Units.l   = str2char( colunit,  Units );
      r1 = 0;
      gdsa_crecol_c( Setin,
                     &tablev,
                     Tname,
                     Cname,
                     Type,
                     Comment,
                     Units,
                     &r1 );
   }
   *collen = nrows;
}



static void createsubsetcols( fchar Setin,
                              fint  tablev,
                              fint  *axnum,
                              fint  subdim,
                              fchar Tname,
                              bool  append,
                              fint  nrows )
/*------------------------------------------------------------*/
/* PURPOSE: Create columns containing subset data.            */
/* Use name and units of non subset axes to create new columns*/
/* Ctype[] and Cunit[] are global                             */
/*------------------------------------------------------------*/
{
   fint   zero = 0;
   char   cbuf[STRLEN];
   int    i, j;
   fint   setdim;


   setdim = gdsc_ndims_c( Setin, &zero );                   /* dimension of set */
   for (i = subdim, j = 1; i < setdim; i++, j++)
   {
      (void) sprintf( cbuf, "SUBGRID%d", j);
      checkcol( Tname,
                tablev,
                cbuf,
               "INT",
               "GRIDS",
               "Subset in grid coordinates",
                append,
                &nrows );
      (void) sprintf( cbuf, "%.*s",
                      nelc_c( Ctype[axnum[i]-1] ),
                      Ctype[axnum[i]-1].a );
      checkcol( Tname,
                tablev,
                cbuf,
               "DBLE",
                Cunit[axnum[i]-1].a,
               "Subset in physical coordinates",
                append,
                &nrows );
   }
}



static void inittable( fchar Setin,
                       fint  tablev,
                       fint  subdim,
                       fint  *axnum,
                       fchar Tname,
                       int   option,
                       char  *axunits,
                       char  *datunits,
                       char  *posunitx,
                       char  *posunity,
                       bool  append,
                       fint  *collen )
/*-----------------------------------------------------------*/
/* PURPOSE: Create GDS columns.                              */
/* If column already exist, delete before (re)creation.      */
/*-----------------------------------------------------------*/
{
   fint   nrows;


   if (strlen( datunits ) == 0)
      strcpy( datunits, "?" );
   if (strlen( axunits ) == 0)
      strcpy( axunits,  "?" );

   checkcol( Tname, tablev, "RADIUS",    "REAL", axunits, "Radius", append, &nrows );
   if (option == 3)
      checkcol( Tname, tablev, "RADKPC",    "REAL", "Kpc", "Radius in kilo parsec", append, &nrows );

   checkcol( Tname, tablev, "CUMSUM",  "REAL", datunits, "Cumulative sum", append, &nrows );
   checkcol( Tname, tablev, "SUM",     "REAL", datunits, "Sum in ring", append, &nrows );
   checkcol( Tname, tablev, "MEAN",    "REAL", datunits, "Mean in ring", append, &nrows );
   checkcol( Tname, tablev, "RMS",     "REAL", datunits, "Rms in ring", append, &nrows );
   checkcol( Tname, tablev, "MINVAL",  "REAL", datunits, "Minimum of data", append, &nrows );
   checkcol( Tname, tablev, "MAXVAL",  "REAL", datunits, "Maximum of data", append, &nrows );
   checkcol( Tname, tablev, "NPTS",    "INT", "#",       "Non blank pixels in ring", append, &nrows );
   checkcol( Tname, tablev, "NBLANKS", "INT", "#",       "Blanks in ring", append, &nrows );
   checkcol( Tname, tablev, "SEGMLO",  "REAL","degree",  "Start angle of segment", append, &nrows );
   checkcol( Tname, tablev, "SEGMHI",  "REAL","degree",  "End angle of segment", append, &nrows );
   checkcol( Tname, tablev, "WIDTH",   "REAL", axunits,  "Width of ring", append, &nrows );
   checkcol( Tname, tablev, "PAMAJ",   "REAL","degree",  "Pos. angle Major axis", append, &nrows );
   checkcol( Tname, tablev, "INCL",    "REAL","degree",  "Inclination of ring", append, &nrows );
   checkcol( Tname, tablev, "LONGGRID","DBLE","grids",   "Position longitude (grids)", append, &nrows );
   checkcol( Tname, tablev, "LATGRID", "DBLE","grids",   "Position latitude (grids)", append, &nrows );
   checkcol( Tname, tablev, "LONGPHYS","DBLE", posunitx, "Position longitude (phys)", append, &nrows );
   checkcol( Tname, tablev, "LATPHYS", "DBLE", posunity, "Position latitude (phys)", append, &nrows );

   if (append) *collen = nrows;

   /*------------------------------------------------------------*/
   /* Create columns that contain grid and physical coordinates  */
   /* of subset axes.                                            */
   /*------------------------------------------------------------*/
   createsubsetcols( Setin, tablev, axnum, subdim, Tname, append, nrows );
}



static void filltab(  fchar   Setin,
                      fint    tablev,
                      fint    subdim,
                      fint   *axnum,
                      fchar   Tname,
                      fint    count,
                      int     option,
                      float   radius,
                      float   radkpc,
                      float   cumsum,
                      float   sum,
                      float   mean,
                      float   rms,
                      fint    npts,
                      fint    nblanks,
                      float   minval,
                      float   maxval,
                      float   segmlo,
                      float   segmhi,
                      float   width,
                      float   phi,
                      float   incl,
                      double *position,
                      double *physpos,
                      fint   *subsetgrid,
                      double *subsetphys )
/*------------------------------------------------------------*/
/* PURPOSE: Fill GDS table with these values.                 */
/*------------------------------------------------------------*/
{
   fint    r1;
   fint    zero = 0;
   fint    one = 1;
   int     i, j;
   char    cbuf[STRLEN];
   fint    setdim;


   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("RADIUS"), &radius, &count, &one, &r1 );
   if (option == 3)
   {
      r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                             tofchar("RADKPC"), &radkpc, &count, &one, &r1 );
   }
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("CUMSUM"), &cumsum, &count, &one, &r1 );
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("SUM"), &sum, &count, &one, &r1 );
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("MEAN"), &mean, &count, &one, &r1 );
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("RMS"), &rms, &count, &one, &r1 );
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("MINVAL"), &minval, &count, &one, &r1 );
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("MAXVAL"), &maxval, &count, &one, &r1 );
   r1 = 0; gdsa_wcint_c(  Setin, &tablev, Tname,
                          tofchar("NPTS"), &npts, &count, &one, &r1 );
   r1 = 0; gdsa_wcint_c(  Setin, &tablev, Tname,
                          tofchar("NBLANKS"), &nblanks, &count, &one, &r1 );
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("SEGMLO"), &segmlo, &count, &one, &r1 );
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("SEGMHI"), &segmhi, &count, &one, &r1 );
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("WIDTH"), &width, &count, &one, &r1 );
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("PAMAJ"), &phi, &count, &one, &r1 );
   r1 = 0; gdsa_wcreal_c( Setin, &tablev, Tname,
                          tofchar("INCL"), &incl, &count, &one, &r1 );
   r1 = 0; gdsa_wcdble_c( Setin, &tablev, Tname,
                          tofchar("LONGGRID"), &position[0], &count, &one, &r1 );
   r1 = 0; gdsa_wcdble_c( Setin, &tablev, Tname,
                          tofchar("LATGRID"), &position[1], &count, &one, &r1 );
   r1 = 0; gdsa_wcdble_c( Setin, &tablev, Tname,
                          tofchar("LONGPHYS"), &physpos[0], &count, &one, &r1 );
   r1 = 0; gdsa_wcdble_c( Setin, &tablev, Tname,
                          tofchar("LATPHYS"), &physpos[1], &count, &one, &r1 );

   setdim = gdsc_ndims_c( Setin, &zero );
   for (i = subdim, j = 0; i < setdim; i++, j++)
   {
      sprintf( cbuf, "SUBGRID%d", j+1);
      r1 = 0; gdsa_wcint_c(  Setin, &tablev, Tname, tofchar(cbuf),
                             &subsetgrid[j], &count, &one, &r1 );
      (void) sprintf( cbuf, "%.*s", nelc_c( Ctype[axnum[i]-1] ), Ctype[axnum[i]-1].a );
      r1 = 0; gdsa_wcdble_c( Setin, &tablev, Tname, tofchar(cbuf),
                             &subsetphys[j], &count, &one, &r1 );
   }
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



static float variance( float sumsqr,
                       float mean,
                       int   n )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate variance using familiar variance formula*/
/*------------------------------------------------------------*/
{
   if (n <= 1)
      return( 0.0 );
   return( (sumsqr - ((float)n)*mean*mean) / (float) (n - 1) );
}



static int inring( float  Xr,
                   float  Yr,
                   int    radnr )
/*------------------------------------------------------------*/
/* PURPOSE: Return 1 if position Xr Yr is inside a ring       */
/*------------------------------------------------------------*/
{
   float    Xx, Yy;
   float    R_sqr;


   Xx =  Xr * Cosphi[radnr] - Yr * Sinphi[radnr];
   Yy = (Xr * Sinphi[radnr] + Yr * Cosphi[radnr]) / Cosinc[radnr];
   R_sqr = Xx*Xx + Yy*Yy;
   return( (Annuli_sqr[radnr][0] <= R_sqr && R_sqr < Annuli_sqr[radnr][1]) );
}



static void plotprofile( fint  Option,
                         fint  plotopt,
                         fint  Nrad,
                         fint  Nsegs,
                         float subpixtot )
/*------------------------------------------------------------*/
/* PURPOSE: Plot measured statistical parameters per ring.    */
/* The plot action is repeated for all segments.              */
/*------------------------------------------------------------*/
{
   int     i, s;
   float   Xmin, Xmax;
   float   Ymin, Ymax;
   float   delta;
   float   Xtick, Ytick;
   fint    font;
   fint    nxsub, nysub;
   float   xl, xr, yb, yt;  /* Edges of the viewport. */
   char    Xtitle[128];
   char    Ytitle[128];
   char    Ttitle[128];
   char    areaunits[10];
   float   area = 0.0;
   float   area_bl = 0.0;
   float   Area, Blankarea;
   float   Mean;
   float   Masstot, Masstot_bl;
   float   surfdens, face_on_surfdens;
   float   surfdens_bl, face_on_surfdens_bl;



   surfdens = face_on_surfdens = 0.0;
   surfdens_bl = face_on_surfdens_bl = 0.0;

   Xmin = Xmax = Radius[0];
   for (i = 1; i < Nrad; i++)
   {
      float x = Radius[i];
      if (x < Xmin)
         Xmin = x;
      if (x > Xmax)
         Xmax = x;
   }


   if (Option == 1)
   {
      for (s = 0; s < Nsegs; s++)
      {
         for (i = 0; i < Nrad; i++)
         {
            float y = 0.0;

            Area = Num[i][s] / subpixtot;
            Blankarea = Numblanks[i][s] / subpixtot;

            if (Area == 0.0)
               Mean = 0.0;
            else
               Mean = Sum[i][s] / Area;

            if (plotopt == 1)
               y = Sum[i][s];
            if (plotopt == 2)
               y = Mean;
            if (plotopt == 3)
               y = sqrt( Var[i][s] );
            if (plotopt == 4)
               y = Datamin[i][s];
            if (plotopt == 5)
               y = Datamax[i][s];
            if (plotopt == 6)
               y = Area;
            if (plotopt == 7)
               y = Blankarea;
            if (plotopt == 8)
               y = Median[i][s];
            if (i == 0)
               Ymin = Ymax = y;
            else
            {
               if (y < Ymin)
                  Ymin = y;
               if (y > Ymax)
                  Ymax = y;
            }
         }
      }
   }

   if (Option == 2)
   {
      for (s = 0; s < Nsegs; s++)
      {
         for (i = 0; i < Nrad; i++)
         {
            float y = 0.0;

            Area = Num[i][s] / subpixtot;
            Blankarea = Numblanks[i][s] / subpixtot;

            area = Area * fabs(Dx*Dy);
            if (area == 0.0)
               surfdens = 0.0;
            else
              surfdens = Sum[i][s] / area;
            face_on_surfdens = Cosinc[i] * surfdens;

            area_bl = Blankarea * fabs(Dx*Dy);
            if (area + area_bl == 0.0)
               surfdens_bl = 0.0;
            else
               surfdens_bl = Sum[i][s] / (area+area_bl);
            face_on_surfdens_bl = Cosinc[i] * surfdens_bl;

            if (plotopt == 1)
               y = surfdens;
            if (plotopt == 2)
               y = surfdens_bl;
            if (plotopt == 3)
               y = face_on_surfdens;
            if (plotopt == 4)
               y = face_on_surfdens_bl;
            if (plotopt == 5)
               y = Sum[i][s];
            if (plotopt == 6)
               y = Area;
            if (plotopt == 7)
               y = Blankarea;
            if (i == 0)
               Ymin = Ymax = y;
            else
            {
               if (y < Ymin)
                  Ymin = y;
               if (y > Ymax)
                  Ymax = y;
            }
         }
      }
   }


   if (Option == 3)
   {
      for (s = 0; s < Nsegs; s++)
      {
         for (i = 0; i < Nrad; i++)
         {
            float      PCsqr = ((4.848 * Distance)*(4.848 * Distance));
            float      ringmassdens, ringmassdens_bl;
            float      geometricalarea;
            float      Realmass, Realmass_bl;
            float      y = 0.0;


            Area      = Num[i][s] / subpixtot;
            Blankarea = Numblanks[i][s] / subpixtot;

            area = Area * fabs(Dx*Dy);
            if (area == 0.0)
               surfdens = 0.0;
            else
              surfdens = Sum[i][s] / area;
            face_on_surfdens = Cosinc[i] * surfdens;

            area_bl = Blankarea * fabs(Dx*Dy);
            if (area + area_bl == 0.0)
               surfdens_bl = 0.0;
            else
               surfdens_bl = Sum[i][s] / (area+area_bl);
            face_on_surfdens_bl = Cosinc[i] * surfdens_bl;

            if (Sumtotgeo == 0.0)
               ringmassdens = 0.0;
            else
               ringmassdens = Mass * face_on_surfdens  / (Sumtotgeo * PCsqr);

            if (Sumtotgeo_bl == 0.0)
               ringmassdens_bl = 0.0;
            else
               ringmassdens_bl = Mass * face_on_surfdens_bl  / (Sumtotgeo_bl * PCsqr);


#ifdef TESTING
            if (Sumtotpix == 0.0)
               ringmasspix = 0.0;
            else
               ringmasspix = Mass * face_on_surfdens  / (Sumtotpix * PCsqr);

            if (Sumtotpix_bl == 0.0)
               ringmasspix_bl  = 0.0;
            else
               ringmasspix_bl = Mass * face_on_surfdens_bl  / (Sumtotpix_bl * PCsqr);
#endif

            geometricalarea = PI * (Annuli[i][1]*Annuli[i][1] - Annuli[i][0]*Annuli[i][0]);
            Realmass    = (Mass*1.0e-9) * (face_on_surfdens*geometricalarea)/Sumtotgeo;
            Realmass_bl = (Mass*1.0e-9) * (face_on_surfdens_bl*geometricalarea)/Sumtotgeo_bl;

                 if (plotopt == 1) y = ringmassdens_bl;
            else if (plotopt == 2) y = Realmass_bl;
            else if (plotopt == 3) y = 0.0;
            else if (plotopt == 4) y = ringmassdens;
            else if (plotopt == 5) y = Realmass;
            else if (plotopt == 6) y = 0.0;
            else if (plotopt == 7) y = Sum[i][s];
            else if (plotopt == 8) y = Area;                                   
            else if (plotopt == 9) y = Blankarea;
            if (i == 0)
               Ymin = Ymax = y;
            else
            {
               if (y < Ymin)
                  Ymin = y;
               if (y > Ymax)
                  Ymax = y;
            }
         }
      }
      /* Min, max for cumulative mass */
      if (plotopt == 3 || plotopt == 6)
      {
         Ymin = 0.0;
         Ymax = Mass * 1.0e-9;
      }
   }


   delta = fabs( Xmax - Xmin ) / 10.0;
   if (delta == 0.0)
      delta = 1.0;
   Xmin -= delta;
   Xmax += delta;
   delta = fabs( Ymax - Ymin ) / 10.0;
   if (delta == 0.0)
      delta = 1.0;
   Ymin -= delta;
   Ymax += delta;

   /*----------------------------------------*/
   /* For each profile, one can change the   */
   /* default box                            */
   /*----------------------------------------*/
   {
      fint    nitems = 4;
      fint    dfault = HIDDEN;
      fint    r1;
      float   XYminmax[4];
      char    messbuf[128];

      XYminmax[0] = Xmin;
      XYminmax[1] = Ymin;
      XYminmax[2] = Xmax;
      XYminmax[3] = Ymax;

      sprintf( messbuf,
              "Corners of box Xl,Yl, Xh,Yh:  [%g,%g,%g,%g]",
               XYminmax[0],
               XYminmax[1],
               XYminmax[2],
               XYminmax[3] );
      r1 = userreal_c( XYminmax,
                       &nitems,
                       &dfault,
                       KEY_PGBOX,
                       tofchar(messbuf) );
      Xmin = XYminmax[0];
      Ymin = XYminmax[1];
      Xmax = XYminmax[2];
      Ymax = XYminmax[3];
   }


   pgpage_c();                               /* Advance to new (plot) page */
   xl = 0.15; xr = 0.9;
   yb = 0.1;  yt = 0.9;
   pgsvp_c( &xl, &xr, &yb, &yt );            /* Set the viewport */

   pgswin_c( &Xmin, &Xmax, &Ymin, &Ymax );   /* Set window to draw box with physical coordinates */
   font   = 1;
   pgscf_c( &font );                         /* Normal font */
   Xtick  = 0.0;
   Ytick  = 0.0;                             /* PGPLOT selects tick intervals */
   nxsub = nysub = 0;                        /* Default subintervals */
   pgbox_c( tofchar("BCNST"),
            &Xtick, &nxsub,
            tofchar("BCNSTV"),
            &Ytick, &nysub );

   pgmove( Xmin, 0.0 );
   pgdraw( Xmax, 0.0 );


   Masstot = Masstot_bl = 0.0;

   pgbbuf_c();
   for (s = 0; s < Nsegs; s++)
   {
      for (i = 0; i < Nrad; i++)
      {
         fint       Numpt = 1;
         fint       Symbol = 2 + s;
         float      value;


         Area      = Num[i][s] / subpixtot;
         Blankarea = Numblanks[i][s] / subpixtot;

         if (Option == 1)
         {
            if (plotopt == 1)  value = Sum[i][s];
            else if (plotopt == 2)
            {
               if (Area == 0.0)
                  Mean = 0.0;
               else
                  Mean = Sum[i][s] / Area;
               value = Mean;
            }
            else if (plotopt == 3)
            {
               float rms = sqrt( Var[i][s] );
               value = rms;
            }
            else if (plotopt == 4) value = Datamin[i][s];
            else if (plotopt == 5) value = Datamax[i][s];
            else if (plotopt == 6) value = Area;
            else if (plotopt == 7) value = Blankarea;
            else if (plotopt == 8) value = Median[i][s];
            pgpt_c( &Numpt, &Radius[i], &value, &Symbol );
         }
         if (Option == 2 || Option == 3)
         {
            area = Area * fabs(Dx*Dy);
            if (area == 0.0)
               surfdens = 0.0;
            else
               surfdens = Sum[i][s] / area;
            face_on_surfdens = Cosinc[i] * surfdens;

            area_bl = Blankarea * fabs(Dx*Dy);
            if (area + area_bl == 0.0)
               surfdens_bl = 0.0;
            else
               surfdens_bl = Sum[i][s] / (area+area_bl);
            face_on_surfdens_bl = Cosinc[i] * surfdens_bl;
         }
         if (Option == 2)
         {
                 if (plotopt == 1) value = surfdens;
            else if (plotopt == 2) value = surfdens_bl;
            else if (plotopt == 3) value = face_on_surfdens;
            else if (plotopt == 4) value = face_on_surfdens_bl;
            else if (plotopt == 5) value = Sum[i][s];
            else if (plotopt == 6) value = Area;
            else if (plotopt == 7) value = Blankarea;
            pgpt_c( &Numpt, &Radius[i], &value, &Symbol );
         }
         if (Option == 3)
         {
            float      PCsqr = ((4.848 * Distance)*(4.848 * Distance));
            float      ringmassdens, ringmassdens_bl;
            float      geometricalarea;
            float      Realmass, Realmass_bl;

            if (Sumtotgeo == 0.0)
               ringmassdens = 0.0;
            else
               ringmassdens = Mass * face_on_surfdens  / (Sumtotgeo * PCsqr);

            if (Sumtotgeo_bl == 0.0)
               ringmassdens_bl = 0.0;
            else
               ringmassdens_bl = Mass * face_on_surfdens_bl  / (Sumtotgeo_bl * PCsqr);

#ifdef TESTING
            if (Sumtotpix == 0.0)
               ringmasspix = 0.0;
            else
               ringmasspix = Mass * face_on_surfdens  / (Sumtotpix * PCsqr);

            if (Sumtotpix_bl == 0.0)
               ringmasspix_bl = 0.0;
            else
               ringmasspix_bl = Mass * face_on_surfdens_bl  / (Sumtotpix_bl * PCsqr);
#endif

            geometricalarea = PI * (Annuli[i][1]*Annuli[i][1] - Annuli[i][0]*Annuli[i][0]);
            Realmass    = (Mass * 1.0e-9) * (face_on_surfdens*geometricalarea)/Sumtotgeo;
            Realmass_bl = (Mass * 1.0e-9) * (face_on_surfdens_bl*geometricalarea)/Sumtotgeo_bl;
            
            Masstot += Realmass;
            Masstot_bl += Realmass_bl;
            
                 if (plotopt == 1) value = ringmassdens_bl;
            else if (plotopt == 2) value = Realmass_bl;
            else if (plotopt == 3) value = Masstot_bl;
            else if (plotopt == 4) value = ringmassdens;
            else if (plotopt == 5) value = Realmass;
            else if (plotopt == 6) value = Masstot;
            else if (plotopt == 7) value = Sum[i][s];
            else if (plotopt == 8) value = Area;
            else if (plotopt == 9) value = Blankarea;
            pgpt_c( &Numpt, &Radius[i], &value, &Symbol );
         }
      }
   }
   pgebuf_c();   

   if (conversion)
   {
      strcpy( Xtitle, "Radius in arcsec" );
      strcpy( areaunits, "''\\u2\\d" );
   }
   else
   {
      strcpy( Xtitle, "Radius in pixels" );
      strcpy( areaunits, "pix\\u2\\d" );
   }

   if (Option == 1)
   {
      if (plotopt == 1)
      {
         sprintf( Ytitle, "Sum (%.*s)", nelc_c(Dataunits), Dataunits.a );
         strcpy( Ttitle, "Sum image values per seg/ring" );
      }
      if (plotopt == 2)
      {
         sprintf( Ytitle, "Mean (%.*s)", nelc_c(Dataunits), Dataunits.a );
         strcpy( Ttitle, "Mean image value per seg/ring" );
      }
      if (plotopt == 3)
      {
         sprintf( Ytitle, "Rms (%.*s)", nelc_c(Dataunits), Dataunits.a );
         strcpy( Ttitle, "Rms image value per seg/ring" );
      }
      if (plotopt == 4)
      {
         sprintf( Ytitle, "Minimum value (%.*s)", nelc_c(Dataunits), Dataunits.a );
         strcpy( Ttitle, "Minimum image value in a seg/ring" );
      }
      if (plotopt == 5)
      {
         sprintf( Ytitle, "Maximum value (%.*s)", nelc_c(Dataunits), Dataunits.a );
         strcpy( Ttitle, "Maximum image value in a seg/ring" );
      }
      if (plotopt == 6)
      {
         sprintf( Ytitle, "Area without blanks (%s)", areaunits );
         strcpy( Ttitle, "Area per seg/ring" );
      }
      if (plotopt == 7)
      {
         sprintf( Ytitle, "Total area (containing blanks) (%s)", areaunits );
         strcpy( Ttitle, "Blank area per seg/ring" );
      }
      if (plotopt == 8)
      {
         sprintf( Ytitle, "Median (%.*s)", nelc_c(Dataunits), Dataunits.a );
         strcpy( Ttitle, "Median per seg/ring" );
      }
      pglab_c( tofchar(Xtitle), tofchar(Ytitle), tofchar(Ttitle) );
   }
   if (Option == 2)
   {
      if (plotopt == 1)
      {
         sprintf( Ytitle, "Surface density (%.*s/*s)",
                  nelc_c(Dataunits), Dataunits.a, areaunits );
         strcpy( Ttitle, "Surface density (non blank area) per seg/ring" );
      }
      if (plotopt == 2)
      {
         sprintf( Ytitle, "Surface density (%.*s/*s)",
                  nelc_c(Dataunits), Dataunits.a, areaunits );
         strcpy( Ttitle, "Surface density (total area) per seg/ring" );
      }
      if (plotopt == 3)
      {
         sprintf( Ytitle, "Face-on surface density (%.*s/*s)",
                  nelc_c(Dataunits), Dataunits.a, areaunits );
         strcpy( Ttitle, "Face-on surface density (non blank area) per seg/ring" );
      }
      if (plotopt == 4)
      {
         sprintf( Ytitle, "Face-on surface density (%.*s/*s)",
                  nelc_c(Dataunits), Dataunits.a, areaunits );
         strcpy( Ttitle, "Face-on surface density (total area) per seg/ring" );
      }
      if (plotopt == 5)
      {
         sprintf( Ytitle, "Sum (%.*s)", nelc_c(Dataunits), Dataunits.a );
         strcpy( Ttitle, "Sum image values per seg/ring" );
      }
      if (plotopt == 6)
      {
         sprintf( Ytitle, "Area without blanks (%s)", areaunits );
         strcpy( Ttitle, "Area per seg/ring" );
      }
      if (plotopt == 7)
      {
         sprintf( Ytitle, "Total area (containing blanks) (%s)", areaunits );
         strcpy( Ttitle, "Blank area per seg/ring" );
      }
      pglab_c( tofchar(Xtitle), tofchar(Ytitle), tofchar(Ttitle) );
   }
   if (Option == 3)
   {
      if (plotopt == 1)
      {
         strcpy( Ytitle, "Mass surface density (M\\d\\(2281)\\u/pc\\u2\\d)" );
         strcpy( Ttitle, "Msd derived from 'total-area' mean surface density" );
      }
      if (plotopt == 2)
      {
         strcpy( Ytitle, "Mass (10\\u9\\d M\\d\\(2281)\\u)" );         
         strcpy( Ttitle, "Mass derived from 'total-area' mean surface density" );
      }
      if (plotopt == 3)
      {
         strcpy( Ytitle, "Cumulative mass (10\\u9\\d M\\d\\(2281)\\u)" );
         strcpy( Ttitle, "Cum. mass derived from 'total-area' mean surface density" );
      }      
      if (plotopt == 4)
      {
         strcpy( Ytitle, "Mass surface density (M\\d\\(2281)\\u/pc\\u2\\d)" );
         strcpy( Ttitle, "Msd derived from 'non-blank-area' mean surface density" );
      }
      if (plotopt == 5)
      {
         strcpy( Ytitle, "Mass (10\\u9\\d M\\d\\(2281)\\u)" );
         strcpy( Ttitle, "Mass derived from 'non-blank-area' mean surface density" );
      }
      if (plotopt == 6)
      {
         strcpy( Ytitle, "Cumulative mass (10\\u9\\d M\\d\\(2281)\\u)" );
         strcpy( Ttitle, "Cum. mass derived from 'non-blank-area' mean surface density" );
      }      
      if (plotopt == 7)
      {
         sprintf( Ytitle, "Sum (%.*s)", nelc_c(Dataunits), Dataunits.a );
         strcpy( Ttitle, "Sum image values per ring" );
      }
      if (plotopt == 8)
      {
         sprintf( Ytitle, "Area without blanks (%s)", areaunits );
         strcpy( Ttitle, "Area per ring" );
      }
      if (plotopt == 9)
      {
         sprintf( Ytitle, "Total area (containing blanks) (%s)", areaunits );
         strcpy( Ttitle, "Blank area per ring" );
      }
      pglab_c( tofchar(Xtitle), tofchar(Ytitle), tofchar(Ttitle) );
   }
}



static int compare( float *f1,
                    float *f2 )
/*-----------------------------------------------------------*/
/* PURPOSE: (float) compare function for quick sort          */
/*-----------------------------------------------------------*/
{
   if (*f1 < *f2)
      return( -1 );
   else if (*f1 > *f2)
      return( 1 );
   return( 0 );
}



static float getmedian( float *data,
                        int   n )
/*-----------------------------------------------------------*/
/* PURPOSE: Return the median of numbers in 'data'.          */
/*-----------------------------------------------------------*/
{
   qsort( data, n, sizeof(float), (int(*)())compare );
   if (n%2)                                /* 'n' odd */
      return( data[(n+1)/2-1] );
   return( 0.5 * (data[n/2-1] + data[n/2]) );
}



static void storevalue( int   rad,
                        int   seg,
                        float imval )
/*------------------------------------------------------------*/
/* PURPOSE: Store data in array and if necessary reallocate   */
/*          memory for an array.                              */
/*------------------------------------------------------------*/
{
   int    indx = Num[rad][seg] - 1;

   medianarray[rad][seg][indx] = imval;
   if ( !(Num[rad][seg] % MEDIANBUF) )
   {
      int    newlen;

      newlen = Num[rad][seg] / MEDIANBUF;
      medianarray[rad][seg] = (float *) realloc(
                                        (float *) medianarray[rad][seg],
                                        (newlen+1)*MEDIANBUF*sizeof(float) );
      if (medianarray[rad][seg] == NULL)
          errorf( 4, "Cannot (re)allocate enough memory for median arrays!" );
   }
}



static void processpixel( fint     x,
                          fint     y,
                          float    *stepxy,
                          float    absdx,
                          float    absdy,
                          double   *cpos,
                          double   maprotation,
                          float    *range,
                          fint     nrad,
                          fint     nseg,
                          float    imval,
                          bool     overlap,
                          bool     plot_exist )
/*------------------------------------------------------------*/
/* PURPOSE: Given the central position of a pixel, generate   */
/*          positions in that pixel and check whether the are */
/*          inside or outside a ring/segment.                 */
/*                                                            */
/* Example of subdivision of a pixel in y direction.          */
/*                                                            */
/*       -                                                    */
/*  |    |  +   <- Last subpixel in y                         */
/*  A    -                                                    */
/*  b    |  +                                                 */
/*  s    -         <== position of Yr                         */
/*  d    |  +                                                 */
/*  y    -                                                    */
/*  |    |  +   <- First subpixel in y                        */
/*       -                                                    */
/*                                                            */
/* Note that if the subdivision is one pixel, the position    */
/* that will be examined is the central position (Xr, Yr) of  */
/* that pixel.                                                */
/*------------------------------------------------------------*/
{
   float   Xr, Yr;
   float   posX, posY;
   int     rad, seg;
   int     inside[MAXRAD];
   int     validpixel;


   validpixel = inrange(imval, range);

   for (rad = 0; rad < nrad; rad++)
   {
      for (seg = 0; seg < (int) nseg; seg++)
      {
         Contribflag[rad][seg] = NO;
      }
   }

   /* The pixel position converted to arcsec wrt central position */
   Xr = absdx * ( (float) x - (float) cpos[0] );
   Yr = absdy * ( (float) y - (float) cpos[1] );

   for (posX = Xr + 0.5*(stepxy[0]-absdx); posX < Xr + 0.5*absdx; posX += stepxy[0])
   {
      for (posY = Yr + 0.5*(stepxy[1]-absdy); posY < Yr + 0.5*absdy; posY += stepxy[1])
      {
         int     overcount = 0;
         float   imvaloverlap;
         int     color = YELLOW;

         for (rad = 0; rad < nrad; rad++)
         {
            if ( inring(posX, posY, rad) )
            {
               inside[rad] = YES;
               overcount++;
            }
            else
               inside[rad] = NO;
         }
         imvaloverlap = imval;
         if (overlap)
         {
            if (overcount != 0 && imval != blank)
            {
               imvaloverlap = imval / (float) overcount;
               color = RED;
            }
         }

         for (rad = 0; rad < nrad; rad++)
         {
            if (inside[rad])
            {
               float  theta;
               theta = gettheta( posX, posY, Phi[rad], maprotation );
               for (seg = 0; seg < (int) nseg; seg++)
               {
                  if ( insegment(theta, Segments[2*seg], Segments[2*seg+1]) )
                  /*----------------------------------------*/
                  /* Now we have a pixel (in a ring, in a   */
                  /* segment) that is either blank or not   */
                  /* blank. If it is not a blank, but its   */
                  /* image value is not within the wanted   */
                  /* range of values, it will be treated as */
                  /* a blank.                               */
                  /*----------------------------------------*/
                  {
                     if (validpixel)
                     {
                        Sum[rad][seg] += imvaloverlap;
                        Sumsqr[rad][seg] += imvaloverlap * imvaloverlap;
                        Num[rad][seg] += 1;
                        Contribflag[rad][seg] = YES;

                        /*----------------------------------------*/
                        /* Overlapping or not, the data min, max  */
                        /* are the real data min and max. not of  */
                        /* the weighted values.                   */
                        /*----------------------------------------*/
                        if (imval > Datamax[rad][seg])
                           Datamax[rad][seg] = imval;
                        if (imval < Datamin[rad][seg])
                           Datamin[rad][seg] = imval;

                        if (calcmedian)
                           storevalue( rad, seg, imvaloverlap );
                     }
                     else
                     {
                        Numblanks[rad][seg] += 1.0;
                        color = MAGENTA;
                     }

                     if (plot_exist)
                     {
                        if (rad == Ellindx[0] && seg == Ellindx[1])
                        {
                           setcolor( color );
                           plotpoint( posX+absdx*cpos[0], posY+absdy*cpos[1] );
                        }
                     }
                  }
               }
            }
         }
      }
   }

   for (rad = 0; rad < nrad; rad++)
   {
      for (seg = 0; seg < (int) nseg; seg++)
      {
         if (Contribflag[rad][seg])
            Contrib[rad][seg] += 1;
      }
   }
}



static int  outsideallellipses( float  x,
                                float  y )
/*------------------------------------------------------------*/
/* PURPOSE: (not yet implemented) Function to determine       */
/*          whether a pixel is completely outside the biggest */
/*          ellipse.                                          */
/*------------------------------------------------------------*/
{
   return( NO );
}



MAIN_PROGRAM_ENTRY
/*------------------------------------------------------------*/
/* Because Fortran passes all arguments by reference, all C   */
/* functions with a Fortran equivalent must do this also      */
/* (GIPSY programmers guide).                                 */
/*------------------------------------------------------------*/
{
   bool     quit;
   fint     rowcount = 1;
   fchar    Tname;
   bool     append;
   bool     plot_exist;           /* Program user PGPLOT routines */
   bool     gdstable;
   fint     nrows;
   int      i, n, m;              /* Counters */
   fint     nitems;
   fint     dfault;               /* Default option for input etc */
   fint     r1, r2;               /* Results of userxxx routines */
   fint     class = 1;            /* Axis is operation axis */
   fint     cwloI;                /* Coordinate words */
   fint     cwhiI;
   fint     pixelsread;           /* Read buffer administration */
   fint     nsubsI;               /* Number of input subsets */
   int      subnr;                /* Index of current input subset */
   int      ax1ind, ax2ind;       /* Indices */
   float    charsize;
   float    oldsize;
   fint     subpix[2];            /* Divide pixel in 'subpix' smaller pixels */
   float    stepxy[2];
   double   maprotation;
   char     formatstr[STRLEN];
   int      fieldlen;
   float    subpixtot = 1;
   float    Area, Blankarea;
   float    Mean;
   fint     dev;
   int      segm;



   /* GIDS related */

   bool     Gidsoverlay;
   fint     Gidsblo[2], Gidsbhi[2];
   fint     Gidssubset = 0;
   float    Gidsflo[2], Gidsfhi[2]; /* Corners of total GIDS area in grids */
   fchar    Gidsset;



   /* Data transfer: */

   fint     TidI;                  /* Tranfer id for input */
   float    **image = NULL;        /* Multiple buffer for all subsets */
   fint     imagesize;


   /*----------------------------------------*/
   /* Contact Hermes                         */
   /*----------------------------------------*/

   init_c();                               /* contact Hermes */
   /* Task identification */
   {
      fchar  Task;                         /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char. */
      IDENTIFICATION( Task.a, VERSION );   /* Show task and version */
   }


   /*----------------------------------------*/
   /* Get set from user                      */
   /*----------------------------------------*/
   setfblank_c( &blank );
   fmake( Gidsset, STRLEN );
   fmake( Setin, STRLEN );
   dfault  = NONE;
   dev     = 8;                            /* Output device, 8 is experienced mode */
   Subdim  = 2;                            /* Dimension of subsets must be 2 */
   class   = 1;
   nsubsI = gdsinp_c( Setin,
                      Subin,
                      &Maxsubs,
                      &dfault,
                      KEY_INSET,
                      MES_INSET,
                      &dev,
                      Axnum,
                      Axcount,
                      &Maxaxes,
                      &class,
                      &Subdim );

   Setdim = gdsc_ndims_c( Setin, &Setlevel );


   /*----------------------------------------*/
   /* Determine the edges of this its frame  */
   /* (GridloI/hiI)                          */
   /*----------------------------------------*/

   r1 = 0;
   gdsc_range_c( Setin, &Setlevel, &cwloI, &cwhiI, &r1 );
   for (m = 0; m < (int) Setdim; m++)
   {
      r1 = r2 = 0;
      GridloI[m] = gdsc_grid_c( Setin, &Axnum[m], &cwloI, &r1 );
      GridhiI[m] = gdsc_grid_c( Setin, &Axnum[m], &cwhiI, &r2 );
   }


   /*----------------------------------------*/
   /* Find axes properties in header of the  */
   /* set                                    */
   /*----------------------------------------*/

   /* Axis numbers in the original set are numbered from 1 to n */
   for (m = 0; m < Setdim; m++)
   {
      (void) sprintf( messbuf, "CDELT%d", m + 1 );
      r1 = 0;
      gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, &Cdelt[m], &r1 );
      anyoutf( 16, "Grid spacing: %f", Cdelt[m] );

      (void) sprintf( messbuf, "CRPIX%d", m + 1 );
      r1 = 0;
      gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, &Crpix[m], &r1 );
      anyoutf( 16, "Reference pixel: %f", Crpix[m] );

      (void) sprintf( messbuf, "CRVAL%d", m + 1 );
      r1 = 0;
      gdsd_rdble_c( Setin, tofchar(messbuf), &Setlevel, &Crval[m], &r1 );
      anyoutf( 16, "Value at reference pixel: %f", Crval[m] );

      finit( Cunit[m], FITSLEN );
      (void) sprintf( messbuf, "CUNIT%d", m + 1 );
      r1 = 0;
      gdsd_rchar_c( Setin, tofchar(messbuf), &Setlevel, Cunit[m], &r1 );
      anyoutf( 16, "Units in header: %.*s",
               nelc_c( Cunit[m] ), Cunit[m].a );

      finit( Ctype[m], FITSLEN );
      (void) sprintf( messbuf, "CTYPE%d", m + 1 );
      r1 = 0;
      gdsd_rchar_c( Setin, tofchar(messbuf), &Setlevel, Ctype[m], &r1 );
      anyoutf( 16, "Name of axis: %.*s",
               nelc_c( Ctype[m] ), Ctype[m].a );
   }

   r1 = skyrot_c( Setin, &maprotation );
   if (r1)
      maprotation = 0.0;
   anyoutf( 16, "Rotation of map: %g degrees", maprotation );

   ax1ind = (int) Axnum[0] - 1;      /* -1 because of C index starting with 0 */
   ax2ind = (int) Axnum[1] - 1;

   /*----------------------------------------*/
   /* Find units of the data                 */
   /*----------------------------------------*/
   fmake( Dataunits, STRLEN );
   r1 = 0;
   gdsd_rchar_c( Setin, tofchar("BUNIT"), &Setlevel, Dataunits, &r1 );
   if (r1 < 0)
      strcpy( Dataunits.a, "?" );
   else
      Dataunits.a[nelc_c(Dataunits)] = '\0';

   /*----------------------------------------*/
   /* Ask for option to be used.             */
   /*----------------------------------------*/
   dev = 8;
   anyoutf( dev, "Choose on of the following options:");
   anyoutf( dev, "OPTION=1  Calculate SUM, MEAN, #POINTS along ellipse,in map units");
   anyoutf( dev, "OPTION=2  Calculate surface brightness in mapunits/arcsec^2");
   anyoutf( dev, "OPTION=3  Calculate mass surface density in Msun/pc^2");

   Option = 1;
   dfault = REQUEST;
   nitems = 1;
   do
   {
      r1 = userint_c( &Option,
                      &nitems,
                      &dfault,
                      KEY_OPTION,
                      MES_OPTION );
      agreed = ((Option > 0) && (Option < 4));
      if (!agreed)
         reject_c( KEY_OPTION, tofchar("Wrong option!") );
   }
   while (!agreed);


   /*----------------------------------------*/
   /* If the data is HI data, the flux       */
   /* represents the mass in the ring. If    */
   /* mass and distance of an object are     */
   /* given, the results are converted to    */
   /* sun-masses/pc^2                        */
   /*----------------------------------------*/
   if (Option == 3)
   {
      Mass   = 0.0;
      dfault = NONE;
      nitems = 1;
      do
      {
         r1 = userreal_c( &Mass,
                          &nitems,
                          &dfault,
                          KEY_MASS,
                          MES_MASS );
         agreed = (Mass > 0.0);
         if (!agreed)
            reject_c( KEY_MASS, tofchar("Must be > 0!") );
      }
      while (!agreed);

      /* Distance */
      Distance = 0.0;
      dfault   = NONE;
      nitems   = 1;
      do
      {
         r1 = userreal_c( &Distance,
                          &nitems,
                          &dfault,
                          KEY_DISTANCE,
                          MES_DISTANCE );
         agreed = (Distance > 0.0);
         if (!agreed)
            reject_c( KEY_DISTANCE, tofchar("Must be > 0!") );
      }
      while (!agreed);
   }

   /*----------------------------------------*/
   /* Find out whether conversion can be     */
   /* made to arcsec (i.e. both axes must be */
   /* in DEGREE).                            */
   /*----------------------------------------*/
   {
      double cf1, cf2;
      conversion = NO;
      r1 = factor_c( Cunit[ax1ind], tofchar("ARCSEC"), &cf1 );
      r2 = factor_c( Cunit[ax2ind], tofchar("ARCSEC"), &cf2 );
      conversion = ( (cf1 == cf2) && (r1 == 0) && (r2 == 0) );
      if (conversion)
         Convfact = cf1;
   }


   /*----------------------------------------*/
   /* User can always decide to work in      */
   /* pixels instead of arcsec.              */
   /*----------------------------------------*/
   {
      fint       Inpixels;
      Inpixels = toflog( NO );
      dfault   = HIDDEN;
      nitems   = 1;
      r1 = userlog_c( &Inpixels,
                      &nitems,
                      &dfault,
                      KEY_PIXELS,
                      MES_PIXELS );
      Inpixels = tobool( Inpixels );
      if (Inpixels)
         conversion = NO;     /* User decides to work in pixels */
   }
   if (conversion)
      anyoutf( 3, "Calculations in seconds of arc");
   else
   {
      anyoutf( 3, "No conversion to sec of arc, calculations in pixels!");
      if (Option == 3)
      {
         anyoutf( 3, "Combination pixels/option=3 not possible");
         finis_c();                                            /* Quit Hermes */
      }
   }


   /*----------------------------------------*/
   /* Get the radii from the user            */
   /*----------------------------------------*/
   dfault = NONE;
   nitems = MAXRAD;
   if (conversion)
   {
      int  l;
      for (l = 0; l < 2; l++)
      {
         anyoutf( 3, "Grid spacing %.*s = %f arcsec",
                  nelc_c( Ctype[l] ), Ctype[l].a, Cdelt[l]*Convfact );
      }
      sprintf( messbuf, "Give radii in arcsec: ");
   }
   else
      sprintf( messbuf, "Give radii in pixels: ");

   Nrad = userreal_c( Radius,
                      &nitems,
                      &dfault,
                      KEY_RADIUS,
                      tofchar(messbuf)  );


   /*----------------------------------------*/
   /* Get the widths from the user           */
   /*----------------------------------------*/
   dfault = NONE;
   nitems = Nrad;
   if (conversion)
      (void) sprintf( messbuf, "Width of ring (arcsec): ");
   else
      (void) sprintf( messbuf, "Width of ring (pixels): ");

   r1 = userreal_c( Width,
                    &nitems,
                    &dfault,
                    KEY_WIDTH,
                    tofchar(messbuf)  );

   for (i = (int) r1; i < (int) Nrad; i++)
   {
      /* fill array with last width if #widths is less than #radii */
      Width[i] = Width[r1-1];
   }


   /*----------------------------------------*/
   /* Get the position angles of major axis  */
   /* wrt north from the user.               */
   /*----------------------------------------*/
   for (i = 0; i < (int) Nrad; i++)
   {
      /* fill array with zero's */
      Phi[i] = 0.0;
   }
   dfault = NONE;
   nitems = Nrad;
   r1 = userreal_c( Phi,
                    &nitems,
                    &dfault,
                    KEY_ANGLE,
                    MES_ANGLE );

   for (i = (int) r1; i < (int) Nrad; i++)
   {
      /* fill array with last angle if #angles is less than #radii */
      Phi[i] = Phi[r1-1];
   }
   for (i = 0; i < (int) Nrad; i++)
   {
      /* Convert to proper range */
      Phi[i] = toangle( Phi[i] );

      /* Setup angle arrays */

      /*---------------------------------------------------------------*/
      /* The values are needed to rotate back so invert sign of angle  */
      /* The angle is Phi, corrected for the map PA and wrt. hor. axis */
      /*---------------------------------------------------------------*/
      Cosphi[i] = (float) cos( RAD( -1.0*(Phi[i]+maprotation+90.0)) );
      Sinphi[i] = (float) sin( RAD( -1.0*(Phi[i]+maprotation+90.0)) );
   }


   /*----------------------------------------*/
   /* Get the inclinations from the user.    */
   /* Abort action if one of the             */
   /* COS(inc) == 0                          */
   /*----------------------------------------*/
   do
   {
      for (i = 0; i < (int) Nrad; i++)
      {
         /* fill array with zero's */
         Inc[i] = 0.0;
      }
      dfault = NONE;
      nitems = Nrad;
      r1 = userreal_c( Inc,
                       &nitems,
                       &dfault,
                       KEY_INCLINATION,
                       MES_INCLINATION );
      for (i = (int) r1; i < (int) Nrad; i++)
      {
         /* fill array with last width if #widths is less than #radii */
         Inc[i] = Inc[r1-1];
      }
      agreed = YES;
      for (i = 0; i < (int) Nrad; i++)
      {
         Cosinc[i] = (float) cos( RAD(Inc[i]) );
         if (fabs(Cosinc[i]) <= 0.0001)
         {
            agreed = NO;
            anyoutf( 1, "%dth inclination (=%f) is illegal! (COS(inc) = 0)", i+1, Inc[i] );
         }
      }
      if (!agreed)
         reject_c( KEY_INCLINATION, tofchar("Illegal entry!") );
   }
   while (!agreed);


   /*-----------------------------------------------------------*/
   /* It is possible to integrate over a segment in the ring.   */
   /* The user enters pairs of angles, a start- and end angle.  */
   /* These angles are counted from the Major axis in the       */
   /* direction of the East. The segments apply to each defined */
   /* ring. The second radius marks the end of the first seg-   */
   /* ment and the start of the second segment (if any).        */
   /*-----------------------------------------------------------*/

   dfault = REQUEST;
   nitems = 2*(MAXSEGS-1);
   do
   {
      Segments[0] =   0.0;
      Segments[1] = 360.0;                        /* Complete ring */
      Numsegm = userreal_c( Segments,
                            &nitems,
                            &dfault,
                            KEY_SEGMENTS,
                            MES_SEGMENTS );
      /*------------------------------------------------------------*/
      /* If user pressed carriage return, the number of segments    */
      /* we want is 1 ==> Numsegm = 2*1=2. In this case a standard  */
      /* table is generated.                                        */
      /*------------------------------------------------------------*/
      if (Numsegm == 0)
      {
         Numsegm = 2;
         standard_table = YES;
      }
      else
      {
         standard_table = NO;
      }
      agreed = ((int)Numsegm%2 == 0);             /* Must be even number */
      anyoutf(1,"numseg, mod=%f %d", Numsegm, (int)Numsegm%2);
      if (!agreed)
         reject_c( KEY_SEGMENTS, tofchar("Number must be even!") );
      if ( (agreed) && (Option == 3) )
      {
         agreed = ( (Segments[0] == 0.0) && (Segments[1] == 360.0) );
         if (!agreed)
            reject_c( KEY_SEGMENTS, tofchar("First must be 0,360") );
      }
      Numsegm /= 2;
   }
   while (!agreed);

   for (i = 0; i < (int) Numsegm; i++)
   {
      /* Convert to proper range */
      Segments[2*i]   = toangle( Segments[2*i] );
      Segments[2*i+1] = toangle( Segments[2*i+1] );
   }


   /*----------------------------------------*/
   /* Determine range of levels to include   */
   /* in the count. The default values are   */
   /* max. and min. floats of the system, so */
   /* that all levels are included if the    */
   /* user pressed carriage return.          */
   /*----------------------------------------*/

   dfault   = HIDDEN;
   Range[0] = -1.0*FLT_MAX;                       /* Defined in float.h */
   Range[1] = FLT_MAX;

   getrange_c( Range, &dfault, KEY_RANGE, MES_RANGE );


   /*----------------------------------------*/
   /* Determine the annuli from the radii.   */
   /* Square the values for the annuli to    */
   /* avoid the need to take the sqrt of the */
   /* calculated radius in the algorithms.   */
   /*----------------------------------------*/
   Rmax = 0.0;
   for (i = 0; i < (int) Nrad; i++)
   {
      /* First index is the ring number, the second is */
      /* the inner- or outer radius. */
      Annuli[i][0] = MYMAX( Radius[i] - 0.5 * Width[i], 0.0 );
      Annuli[i][1] = MYMAX( Radius[i] + 0.5 * Width[i], 0.0 );
      Annuli_sqr[i][0] = Annuli[i][0] * Annuli[i][0];
      Annuli_sqr[i][1] = Annuli[i][1] * Annuli[i][1];
      Rmax = MYMAX( Rmax, Annuli[i][1] );
   }


   /*----------------------------------------*/
   /* Ask for the center of the galaxy       */
   /*----------------------------------------*/
   dfault = REQUEST;
   Maxpos = 1;
   do
   {
      fint    Numpos;
      Position[0] = (double)GridloI[0] + Crpix[ax1ind] - 1.0;
      Position[1] = (double)GridloI[1] + Crpix[ax2ind] - 1.0;
      sprintf( messbuf,
               "Position of center of ellipses:  [%d,%d]",
               (int)Position[0],
               (int)Position[1] );
      Numpos = gdspos_c( Position,                /* One position occupies 'Subdim' items. */
                         &Maxpos,                 /* Maximum number of items to enter */
                         &dfault,
                         KEY_POSITION,
                         tofchar(messbuf),
                         Setin,
                         &Subin[0] );

      agreed = (((fint)Position[0] <= GridhiI[0]) && ((fint)Position[0] >= GridloI[0]) &&
                ((fint)Position[1] <= GridhiI[1]) && ((fint)Position[1] >= GridloI[1]) );
      if (!agreed)
         reject_c( KEY_POSITION, tofchar("Pos. outside input") );
   }
   while(!agreed);


   /*----------------------------------------*/
   /* Find area to be read as input          */
   /*----------------------------------------*/
   if (conversion)
   {
      Dx = (float) (Cdelt[ax1ind] * Convfact);
      Dy = (float) (Cdelt[ax2ind] * Convfact);
      if ((Dx == 0.0) || (Dy == 0.0))
      {
         if ((Cdelt[ax1ind] == 0.0) || (Cdelt[ax2ind] == 0.0))
         {
           anyoutf( 1, "One (or both) grid spacing(s) == 0, check header ");
           anyoutf( 1, "with HEADER or FIXHED ITEM=HEAD" );
         }
         errorf( 4, "Grid spacing or conversion factor == 0" );
      }
   }
   else
      Dx = Dy = 1.0;

   Absdx = fabs(Dx);
   Absdy = fabs(Dy);


   /* Boundaries in pixels. Note the absolute values of the increments Dx, Dy */
   BgridloI[0] = MYMAX( GridloI[0], (int) ((float)Position[0] - Rmax/fabs(Dx)) - 1 ); /*Xmin */
   BgridhiI[0] = MYMIN( GridhiI[0], (int) ((float)Position[0] + Rmax/fabs(Dx)) + 1 ); /*Xmax */
   BgridloI[1] = MYMAX( GridloI[1], (int) ((float)Position[1] - Rmax/fabs(Dy)) - 1 ); /*Ymin */
   BgridhiI[1] = MYMIN( GridhiI[1], (int) ((float)Position[1] + Rmax/fabs(Dy)) + 1 ); /*Ymax */


   anyoutf( 16, "Origin in pixels: %g %g", Position[0], Position[1] );
   anyoutf( 16, "Rmax_x, Rmax_y in pixels: %g %g", Rmax/fabs(Dx), Rmax/fabs(Dy) );
   anyoutf( 16, "Box entire subset: %d %d %d %d",
                 GridloI[0], GridhiI[0], GridloI[1], GridhiI[1] );
   anyoutf( 16, "Min. box that enclose biggest ellipse: %d %d %d %d",
                 BgridloI[0], BgridhiI[0], BgridloI[1], BgridhiI[1] );



   /*----------------------------------------*/
   /* For better approximation of the real   */
   /* area in a ring, divide a pixel in sub- */
   /* pixels.                                */
   /*----------------------------------------*/
   subpix[0] = subpix[1]= 2;
   dfault   = REQUEST;
   nitems   = 2;
   sprintf( messbuf,
           "Give number of 'sub'pixels in x and y per pixel: [%d (%d)]",
            subpix[0], subpix[1] );
   do
   {
      r1 = userint_c( subpix,
                      &nitems,
                      &dfault,
                      KEY_SUBPIX,
                      tofchar(messbuf) );
      if (r1 == 1)
         subpix[1] = subpix[0];                 /* Only one entered, copy second */
      agreed = ( (subpix[0] >= 1) && (subpix[1] >= 1) );
      if (!agreed)
         reject_c( KEY_SUBPIX, tofchar("Number(s) must be > 0") );
   }
   while (!agreed);

   stepxy[0] = Absdx / (float) subpix[0];
   stepxy[1] = Absdy / (float) subpix[1];


   /*----------------------------------------*/
   /* There is a choice in order to generate */
   /* your table. The first is per radius all*/
   /* segments. The second is per segment all*/
   /* radii.                                 */
   /*----------------------------------------*/
   Tableopt = 1;
   dfault   = HIDDEN;
   nitems   = 1;
   do
   {
      r1 = userint_c( &Tableopt,
                      &nitems,
                      &dfault,
                      KEY_ORDER,
                      MES_ORDER );
      agreed = ( (Tableopt >= 1) && (Tableopt <= 2) );
      if (!agreed)
      {
         reject_c( KEY_ORDER, tofchar("Wrong option!") );
         dfault = REQUEST;
      }
   }
   while (!agreed);


   if (Option != 1)
   {
      calcmedian = NO;
   }
   else
   {
      /*----------------------------------------*/
      /* User wants to include the calculation  */
      /* of the median. Then we must keep a log */
      /* of all data per ring and per segment.  */
      /*----------------------------------------*/
      dfault   = REQUEST;
      nitems   = 1;
      calcmedian = toflog( NO );
      r1 = userlog_c( &calcmedian,
                      &nitems,
                      &dfault,
                      KEY_MEDIAN,
                      MES_MEDIAN );
      calcmedian = tobool( calcmedian ) && (Option == 1);
      if (calcmedian)
      {
         for (n = 0; n < Nrad; n++)
         {
            for (segm = 0; segm < (int) Numsegm; segm++)
            {
               medianarray[n][segm] = NULL;
               medianarray[n][segm] = (float *) realloc( (float *) medianarray[n][segm],
                                                         MEDIANBUF * sizeof(float) );
               if (medianarray[n][segm] == NULL)
                  errorf( 4, "Cannot allocate enough memory for median arrays!" );
            }
         }
      }
   }


   /*----------------------------------------*/
   /* Check whether there is a GIDS window   */
   /* open. If so, get the properties of the */
   /* displayed set. If it is possible to    */
   /* overlay, ask user if he wants to       */
   /* overlay one or more ellipses. The      */
   /* keyword is asked hidden if the         */
   /* displayed set is not the same as the   */
   /* ELLINT set entered with gdsinp.        */
   /*----------------------------------------*/
   Gidsoverlay = preparegids( Gidsset, Gidssubset,
                              Gidsblo, Gidsbhi,
                              Gidsflo, Gidsfhi );
   if (Gidsoverlay)
   {
      int equal;

      equal = equalset( Setin, Gidsset );
      if (equal)
      {
         dfault = REQUEST;
         sprintf( messbuf, "Overlay ellipse(s) in Gids?   [Y]/N" );
      }
      else
      {
         Gidsoverlay = NO;
         dfault = HIDDEN;
         sprintf( messbuf, "Overlay ellipse(s) in Gids?   Y/[N]");
      }
      nitems = 1;
      r1 = userlog_c( &Gidsoverlay,
                      &nitems,
                      &dfault,
                      KEY_OVERLAY,
                      tofchar(messbuf) );
      Gidsoverlay = tobool( Gidsoverlay );
   }
   else
   {
      /* Not GIDS, but the biggest ellipse defines the 'plot' box */
      Gidsblo[0] = BgridloI[0];
      Gidsblo[1] = BgridloI[1];
      Gidsbhi[0] = BgridhiI[0];
      Gidsbhi[1] = BgridhiI[1];
   }


   /*----------------------------------------*/
   /* User can select an ellipse to plot.    */
   /* A negative index indicates no plotting */
   /* of the ellipse. Indices start at 1     */
   /*----------------------------------------*/

   Ellindx[0] = -1;
   Ellindx[1] =  1;
   if (Gidsoverlay)
      dfault = REQUEST;
   else
      dfault = HIDDEN;

   plot_exist = NO;
   do
   {
      nitems = 2;
      r1 = userint_c( Ellindx,
                      &nitems,
                      &dfault,
                      KEY_ELLIPSE,
                      MES_ELLIPSE );
      cancel_c( KEY_ELLIPSE );
      quit = (r1 == 0);
      if (r1 == 1) Ellindx[1] = 1;
      if (!quit)
      {
         /* Indices start with 1, so subtract 1 for the C-index */
         Ellindx[0] -= 1;
         Ellindx[1] -= 1;
         agreed = ( (Ellindx[0] <  Nrad) &&
                    (Ellindx[0] >= 0)    &&
                    (Ellindx[1] >= 0)    &&
                    (Ellindx[1] <  Numsegm) );
         if (!agreed)
         {
            reject_c( KEY_ELLIPSE, tofchar("Unknown ellipse/segment") );
            dfault = REQUEST;
         }
         else
         {
            plot_exist = YES;
            drawellipse(  Radius[Ellindx[0]], Phi[Ellindx[0]], Inc[Ellindx[0]],
                          Width[Ellindx[0]], Segments[2*Ellindx[1]],
                          Segments[2*Ellindx[1]+1],
                          Position, Absdx, Absdy,
                          Gidsblo, Gidsbhi,
                          Gidsflo, Gidsfhi,
                          maprotation,
                          conversion,
                          Gidsoverlay );
         }
      }
   }
   while (!quit);


   /*----------------------------------------*/
   /* If regions do overlap, it is possible  */
   /* to integrate them separate or weight   */
   /* the common data over the overlapping   */
   /* regions. The keyword OVERLAP= sets the */
   /* action.                                */
   /*----------------------------------------*/
   dfault  = HIDDEN;
   nitems  = 1;
   Overlap = toflog( YES );
   r1 = userlog_c( &Overlap,
                   &nitems,
                   &dfault,
                   KEY_OVERLAP,
                   MES_OVERLAP );
   Overlap = tobool( Overlap );

   /*----------------------------------------*/
   /* Open ASCII file to write obtained      */
   /* statistics                             */
   /*----------------------------------------*/
   {
      fchar    Dummystr;
      char     filenameOUT[STRLEN];
      fint     Overwrite;


      fmake( Dummystr, STRLEN );
      do
      {
         dfault = HIDDEN;
         Overwrite = toflog( YES );
         r1 = usertext_c( Dummystr,
                          &dfault,
                          KEY_FILENAME,
                          MES_FILENAME );
         if (r1 == 0)
         {
            fpOUT = NULL;
            agreed = YES;
         }
         else
         {
            strcpy( filenameOUT, strtok(Dummystr.a, " ") ); /* Delete spaces in name */
            fpOUT = fopen(filenameOUT, "r");
            if (fpOUT != NULL)
            {                                   /* The file exists */
               nitems = 1;
               dfault = REQUEST;
               r1 = userlog_c( &Overwrite,
                               &nitems,
                               &dfault,
                               KEY_OVERWRITE,
                               MES_OVERWRITE );
               fclose( fpOUT );
               cancel_c( KEY_OVERWRITE );
            }
            if (!Overwrite)
            {
               cancel_c( KEY_FILENAME );
               agreed = NO;
            }
            else
            {
               fpOUT = fopen(filenameOUT, "w");
               agreed = (fpOUT != NULL);
               if (!agreed)
                  reject_c( KEY_FILENAME, tofchar("Cannot open, try another!") );
            }
         }
      }
      while (!agreed);
   }

   /*----------------------------------------*/
   /* What is the format of the output       */
   /* numbers in file/tables                 */
   /*----------------------------------------*/
   {
      fchar   Formstr;
      fmake( Formstr, STRLEN );
      nitems = 1;
      dfault = REQUEST;
      if (Option == 1)
         strcpy( formatstr, DEFFORMAT1 );
      if (Option == 2)
         strcpy( formatstr, DEFFORMAT2 );
      if (Option == 3)
         strcpy( formatstr, DEFFORMAT3 );
      sprintf( messbuf, "Enter format for number output:  [%s]", formatstr );
      r1 = userchar_c( Formstr, &nitems, &dfault, KEY_FORMAT,
                       tofchar(messbuf) );
      if (r1)
      {
         Formstr.a[nelc_c(Formstr)] = '\0';
         strcpy( formatstr, Formstr.a );
      }
   }

   /*----------------------------------------*/
   /* Ask name of GDS table to store the     */
   /* results.                               */
   /*----------------------------------------*/
   dfault   = HIDDEN;
   nitems   = 1;
   gdstable = toflog( NO );
   r1       = userlog_c( &gdstable,
                         &nitems,
                         &dfault,
                         KEY_GDSTABLE,
                         MES_GDSTABLE );
   gdstable = tobool( gdstable );
  
   fmake( Tname, ITEMLEN );
   sprintf( messbuf, "Give name of table to store results:    [ELLINT]" );
   str2char("ELLINT", Tname);
   dfault  = HIDDEN;
   nitems  = 1;
   r1      = userchar_c( Tname,
                         &nitems,
                         &dfault,
                         KEY_TABNAME,
                         tofchar(messbuf) );

   (void) sprintf( messbuf, "Append to existing columns?    Y/[N]");
   dfault  = HIDDEN;
   nitems = 1;
   append  = toflog( NO );
   r1      = userlog_c( &append,
                        &nitems,
                        &dfault,
                        KEY_TABAPPEND,
                        tofchar(messbuf) );
   append  = tobool( append );



   /*----------------------------------------*/
   /* Create a 2-dim array with indices      */
   /* given by the minimum box. Use array as */
   /* data buffer.                           */
   /*----------------------------------------*/
   image = fmatrix( BgridloI[0], BgridloI[1], BgridhiI[0], BgridhiI[1] );
   imagesize = (BgridhiI[0] - BgridloI[0] + 1) * (BgridhiI[1] - BgridloI[1] + 1);
   if (!image)
      errorf( 4, "Cannot allocate %d bytes for image data!",
                  imagesize*sizeof(float) );

   /*--------------------------------------------------*/
   /* Start the loop over subsets.                     */
   /*--------------------------------------------------*/
   for (subnr = 0; subnr < nsubsI; subnr++)
   {
      int      x, y;
      fint     elapse;
      double   cputime, realtime;                /* Variables for timer */
      int      count = 0;
      int      totcount = nsubsI * imagesize;
      int      countstep = totcount / NSTATS;    /*'NSTATS' times a status message */
      int      slen;
      char     mbuf[20];


      tableheader( Subin[subnr], conversion, subpix ); /* List some info on screen */

      elapse = 0;
      timer_c( &cputime, &realtime, &elapse );   /* Reset timer */

      cwloI = gdsc_fill_c( Setin, &Subin[subnr], BgridloI );
      cwhiI = gdsc_fill_c( Setin, &Subin[subnr], BgridhiI );
      TidI  = 0;

      plot_exist = (plot_exist && subnr == 0);
      if (plot_exist)
      {
         fint   hollow = 2;
         pgqch_c( &oldsize );
         charsize = 4.0;
         pgsch_c( &charsize );
         pgsfs_c( &hollow );
         setcolor( BLUE );
         pgrect( BgridloI[0]*Absdx,
                 BgridloI[1]*Absdy,
                 BgridhiI[0]*Absdx,
                 BgridhiI[1]*Absdy );
         setcolor( YELLOW );
      }
      for (i = 0; i < (int) Nrad; i++)
      {
         for (segm = 0; segm < (int) Numsegm; segm++)
         {
            Sum[i][segm]       = 0.0;             /* Reset the statistics variables */
            Num[i][segm]       = 0;
            Numblanks[i][segm] = 0;
            Contrib[i][segm]   = 0;
            Datamin[i][segm]   = FLT_MAX;
            Datamax[i][segm]   = -FLT_MAX;
            Var[i][segm]       = 0.0;
         }
      }

      /* Read entire subset in memory */
      gdsi_read_c( Setin,
                   &cwloI,
                   &cwhiI,
                   &image[BgridloI[1]][BgridloI[0]],
                   &imagesize,
                   &pixelsread,
                   &TidI );

      if (TidI != 0)
         errorf( 4, "Something wrong while reading data from disk!" );

      /* Start looping over all pixels in this subset */

      for (x = BgridloI[0]; x <= BgridhiI[0]; x++)
      {
         for (y = BgridloI[1]; y <= BgridhiI[1]; y++)
         {
            if ( !outsideallellipses(x, y) )
            {
               processpixel( x, y,
                             stepxy,
                             Absdx, Absdy,
                             Position,
                             maprotation,
                             Range,
                             Nrad, Numsegm,
                             image[y][x],
                             Overlap,
                             plot_exist );
            }
            /* Show progress */
            count++;
            if (!(count%countstep))
            {
               statusf( "Examined %d%% of all pixels",
                         count*100 / totcount );
            }
         }
      }

      statusf( "Examined 100%% of all pixels" );
      elapse = 1;
      timer_c( &cputime, &realtime, &elapse );   /* Get cpu seconds */
      anyoutf( 3, " " );
      anyoutf( 3, "-ELLINT integrated %d rings in %.2f sec (%.2f cpu sec)",
               Nrad, realtime, cputime );
      anyoutf( 3, " " );
      statusf( "Writing tables" );



      Sumtotgeo    = 0.0;
      Sumtotgeo_bl = 0.0;
      Masstot      = 0.0;
      Masstot_bl   = 0.0;
      if (Option == 3)
      {
         for (n = 0; n < Nrad; n++)
         {
            /*-------------------------------------------------------*/
            /* If option = 3 we need the sum of of the contributions */
            /* for each ring. Each contribution is multiplied by the */
            /* area between two radii. The contributions are the     */
            /* corrected means.                                      */
            /*-------------------------------------------------------*/
            {
               float   face_on_av_surfdens, mean;
               float   face_on_av_surfdens_bl, meanbl;
               float   geometricalarea;

               mean   = Sum[n][0] / Num[n][0];
               meanbl = Sum[n][0] / (Num[n][0]+Numblanks[n][0]);
               face_on_av_surfdens = mean * Cosinc[n]/(fabs(Dx*Dy));
               face_on_av_surfdens_bl = meanbl * Cosinc[n]/(fabs(Dx*Dy));
               geometricalarea = PI * (Annuli[n][1]*Annuli[n][1] - Annuli[n][0]*Annuli[n][0]);
               Sumtotgeo    += face_on_av_surfdens * geometricalarea;
               Sumtotgeo_bl += face_on_av_surfdens_bl * geometricalarea;

#ifdef TESTING
               pixelarea  = (fabs(Dx*Dy) / Cosinc[n]) * Num[n][0] /(float) (subpix[0]*subpix[1]);
               Sumtotpix    += face_on_av_surfdens * pixelarea;
               Sumtotpix_bl += face_on_av_surfdens_bl * pixelarea;
               anyoutf( 3, "area ring %d = %f", n, geometricalarea );
               anyoutf( 3, "area ring %d = %f", n, pixelarea );
#endif
            }
         }
      }

      /*--------------------------------------------------*/
      /* Display the statistics and write to ASCII file   */
      /* specified in FILENAME=                           */
      /*--------------------------------------------------*/

      fieldlen = printusing( formatstr, 1.0, tablehead );

      if (conversion)
         strcpy( axunits, "arcsec" );
      else
         strcpy( axunits, "pixels" );
      strcpy( datunits, Dataunits.a );

      tablehead[0] = tableunits[0] = '\0';      /* Initialize long table strings */

      /* Fixed field for radius, inc, pa etc. Field length radius is */
      /* one longer. */
      sprintf( mbuf, "%*s ", COLWIDTH+1, "radius" );  strcat( tablehead,  mbuf );
      sprintf( mbuf, "%*s ", COLWIDTH+1, axunits );     strcat( tableunits, mbuf );

      if (Option == 1)
      {
         sprintf( mbuf, "%*s ", fieldlen, "cum.sum" ); strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, datunits );  strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "sum" );     strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, datunits );  strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "mean" );    strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, datunits );  strcat( tableunits, mbuf );

         if (calcmedian)
         {
            sprintf( mbuf, "%*s ", fieldlen, "median" ); strcat( tablehead,  mbuf );
            sprintf( mbuf, "%*s ", fieldlen, datunits ); strcat( tableunits, mbuf );
         }

         sprintf( mbuf, "%*s ", fieldlen, "rms" );     strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, datunits );  strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", COLWIDTH, "subpix" );  strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", COLWIDTH, "#" );       strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", COLWIDTH, "unique" );  strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", COLWIDTH, "pixels" );  strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "area" );    strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, "pixels" );  strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "area-bl" ); strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, "pixels" );  strcat( tableunits, mbuf );
      }

      if (Option == 2)
      {
         if (conversion)
            strcpy( areaunits, "''^2");
         else
            strcpy( areaunits, "pix^2");

         /* Map units per sec of arc */
         sprintf( surfdunits, "%.*s/''", nelc_c( Dataunits ), Dataunits.a );

         sprintf( mbuf, "%*s ", fieldlen, "sb" );       strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, surfdunits ); strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "sb-t" );     strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, surfdunits ); strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "fo-sb" );    strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, surfdunits ); strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "fo-sb-t" );  strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, surfdunits ); strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "sum" );      strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, datunits );   strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", COLWIDTH, "subpix" );   strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", COLWIDTH, "#" );        strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "area" );     strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, "pixels" );   strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "area-bl" );  strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, "pixels" );   strcat( tableunits, mbuf );
      }
      if (Option == 3)
      {
         /* Solar mass per square pc */
         strcpy( surfdunits, "Mo/pc^2" );

         sprintf( mbuf, "%*s ", COLWIDTH, "radius" );   strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", COLWIDTH, "Kpc" );      strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "Msd-t" );    strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, surfdunits ); strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "Mass-t" );   strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, "10^9 M0" );  strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "Cum.Mass" ); strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, "10^9 M0" );  strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "Msd" );      strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, surfdunits ); strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "Mass" );     strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, "10^9 M0" );  strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "Cum.Mass" ); strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, "10^9 M0" );  strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "sum" );      strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, datunits );   strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", COLWIDTH, "subpix" );   strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", COLWIDTH, "#" );        strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "area" );     strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, "pixels" );   strcat( tableunits, mbuf );

         sprintf( mbuf, "%*s ", fieldlen, "area-bl" );  strcat( tablehead,  mbuf );
         sprintf( mbuf, "%*s ", fieldlen, "pixels" );   strcat( tableunits, mbuf );
      }

      sprintf( mbuf, "%*s ", COLWIDTH, "segLO" );    strcat( tablehead,  mbuf );
      sprintf( mbuf, "%*s ", COLWIDTH, "deg." );     strcat( tableunits, mbuf );

      sprintf( mbuf, "%*s ", COLWIDTH, "segHI" );    strcat( tablehead,  mbuf );
      sprintf( mbuf, "%*s ", COLWIDTH, "deg." );     strcat( tableunits, mbuf );

      sprintf( mbuf, "%*s ", COLWIDTH, "width" );    strcat( tablehead,  mbuf );
      sprintf( mbuf, "%*s ", COLWIDTH, axunits );    strcat( tableunits, mbuf );

      sprintf( mbuf, "%*s ", COLWIDTH, "pa" );       strcat( tablehead,  mbuf );
      sprintf( mbuf, "%*s ", COLWIDTH, "deg." );     strcat( tableunits, mbuf );

      sprintf( mbuf, "%*s", COLWIDTH, "incl" );      strcat( tablehead,  mbuf );
      sprintf( mbuf, "%*s", COLWIDTH, "deg." );      strcat( tableunits, mbuf );

      anyoutf( 3, tablehead );
      anyoutf( 3, tableunits );

      if (fpOUT != NULL)
      {
         fprintf( fpOUT, "!%s\n", tablehead );
         fprintf( fpOUT, "!%s\n", tableunits );
      }

      /* Print a table border */
      slen = strlen( tablehead );
      memset( border, '=', slen );
      border[slen] = '\0';

      anyoutf( 3, border );
      if (fpOUT != NULL)
         fprintf( fpOUT, "!%s\n", border );


      /* Initialize a GDS table */

      sprintf( posunitx, "%.*s", (int) nelc_c(Cunit[0]), Cunit[0].a );
      sprintf( posunity, "%.*s", (int) nelc_c(Cunit[1]), Cunit[1].a );

      if (gdstable && subnr == 0)
      {
         inittable( Setin,
                    Setlevel,
                    Subdim,
                    Axnum,
                    Tname,
                    Option,
                    axunits,
                    datunits,
                    posunitx, posunity,
                    append,
                    &nrows );
         if (append)
            rowcount = nrows + 1;
         else
            rowcount = 1;
      }

      /*--------------------------------------------------*/
      /* For all rings and segments the sum is calculated.*/
      /* Do some simple statistics using this sum and the */
      /* number of pixels involved. Note that the loop    */
      /* order depends on 'Tableopt': All segments for one*/
      /* ring (ORDER=1) or display all radii for one      */
      /* segment (ORDER=2).                               */
      /*--------------------------------------------------*/
      if (Tableopt == 1)
      {
         imax = (int) Nrad;
         mmax = (int) Numsegm;
      }
      else
      {
         imax = (int) Numsegm;
         mmax = (int) Nrad;
      }
      Cumsum = 0.0;
      subpixtot = (float) (subpix[0]*subpix[1]);
      for (i = 0; i < imax; i++)
      {
         float      Radkpc = 0.0;

         if (mmax > 1)
            Cumsum = 0.0;
         for (m = 0; m < mmax; m++)
         {
            int     Ri, Si;
            float   surfdens = 0.0;
            float   face_on_surfdens = 0.0;
            float   surfdens_bl = 0.0;
            float   face_on_surfdens_bl = 0.0;
            float   area, area_bl;

            if (Tableopt == 1)
            {
               Ri = i;
               Si = m;
            }
            else
            {
               Ri = m;
               Si = i;
            }

            Sum[Ri][Si] /= subpixtot;
            if (Num[Ri][Si] == 0)              /* Initialize */
            {
               Mean = 0.0;
               Var[Ri][Si]  = 0.0;
               Datamin[Ri][Si] = 0.0;
               Datamax[Ri][Si] = 0.0;
               Area = 0.0;
            }
            /*----------------------------------------*/
            /* The 'processpixel' function calculated */
            /* too much FLUX. Each intensity has to   */
            /* be divided by the number of 'subpixels'*/
            /* in a pixel. In order to get the AREA   */
            /* expressed in pixels, it will be divided*/
            /* by the same number.                    */
            /*----------------------------------------*/
            else
            {
               Area = Num[Ri][Si] / subpixtot;
               Mean = Sum[Ri][Si] / Area;
               Var[Ri][Si] = variance( Sumsqr[Ri][Si], Mean, Num[Ri][Si] );
            }

            Blankarea = Numblanks[Ri][Si] / subpixtot;

            Cumsum += Sum[Ri][Si];
            tablehead[0] = '\0';

            sprintf( mbuf, "%*.*f ", COLWIDTH+1, COLPREC, Radius[Ri] );
            strcat( tablehead, mbuf );

            if (Option == 1)
            {
               printusing( formatstr, Cumsum, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Sum[Ri][Si], mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Mean, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               if (calcmedian)
               {
                  Median[Ri][Si]  = getmedian( medianarray[Ri][Si], Num[Ri][Si] );
                  printusing( formatstr, Median[Ri][Si], mbuf );
                  strcat( tablehead, mbuf ); strcat( tablehead, SPACE );
               }
               printusing( formatstr, sqrt(fabs(Var[Ri][Si])), mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               sprintf( mbuf, "%*d ", COLWIDTH, Num[Ri][Si] );
               strcat( tablehead, mbuf );

               sprintf( mbuf, "%*d ", COLWIDTH, Contrib[Ri][Si] );
               strcat( tablehead, mbuf );

               printusing( formatstr, Area, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Blankarea, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );
            }

            if (Option == 2 || Option == 3)
            {
               area = Area * fabs(Dx*Dy);
               if (area == 0.0)
                  surfdens = 0.0;
               else
                  surfdens = Sum[Ri][Si] / area;
               face_on_surfdens = Cosinc[Ri] * surfdens;

               area_bl = Blankarea * fabs(Dx*Dy);
               if (area + area_bl == 0.0)
                  surfdens_bl = 0.0;
               else
                  surfdens_bl = Sum[Ri][Si] / (area+area_bl);
               face_on_surfdens_bl = Cosinc[Ri] * surfdens_bl;
            }

            if (Option == 2)
            {
               printusing( formatstr, surfdens, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, surfdens_bl, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, face_on_surfdens, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, face_on_surfdens_bl, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Sum[Ri][Si], mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               sprintf( mbuf, "%*d ", COLWIDTH, Num[Ri][Si] );
               strcat( tablehead, mbuf );

               printusing( formatstr, Area, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Blankarea, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );
            }

            if (Option == 3)
            /*--------------------------------------------------*/
            /* x = physical size at distance D                  */
            /* a = angle                                        */
            /*                                                  */
            /* x(pc)          = Distance(pc) * tan(a)           */
            /*                =~ D(pc) * a(rad)                 */
            /*                = D(Mpc)*10^6 * a(")*4.848.10^-6  */
            /*                = 4.848 * D(Mpc) * a(")           */
            /*--------------------------------------------------*/
            {
               float      PCsqr = ((4.848 * Distance)*(4.848 * Distance));
               float      ringmassdens, ringmassdens_bl;
               float      geometricalarea;
               float      Realmass, Realmass_bl;


               if (Sumtotgeo == 0.0)
                  ringmassdens = 0.0;
               else
                  ringmassdens = Mass * face_on_surfdens  / (Sumtotgeo * PCsqr);

               if (Sumtotgeo_bl == 0.0)
                  ringmassdens_bl = 0.0;
               else
                  ringmassdens_bl = Mass * face_on_surfdens_bl  / (Sumtotgeo_bl * PCsqr);

#ifdef TESTING
               if (Sumtotpix == 0.0)
                  ringmasspix = 0.0;
               else
                  ringmasspix = Mass * face_on_surfdens  / (Sumtotpix * PCsqr);

               if (Sumtotpix_bl == 0.0)
                  ringmasspix_bl = 0.0;
               else
                  ringmasspix_bl = Mass * face_on_surfdens_bl  / (Sumtotpix_bl * PCsqr);
#endif

               geometricalarea = PI * (Annuli[Ri][1]*Annuli[Ri][1] - Annuli[Ri][0]*Annuli[Ri][0]);
               Realmass    = Mass * (face_on_surfdens*geometricalarea)/Sumtotgeo;
               Realmass_bl = Mass * (face_on_surfdens_bl*geometricalarea)/Sumtotgeo_bl;
               Masstot    += Realmass;
               Masstot_bl += Realmass_bl;

               Radkpc = 4.848 * Distance * Radius[Ri];
               Radkpc /= 1000.0;

               sprintf( mbuf, "%*.*f ", COLWIDTH, COLPREC, Radkpc );
               strcat( tablehead, mbuf );

               printusing( formatstr, ringmassdens_bl, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Realmass_bl*1.0e-9, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Masstot_bl*1.0e-9, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, ringmassdens, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Realmass*1.0e-9, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Masstot*1.0e-9, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Sum[Ri][Si], mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               sprintf( mbuf, "%*d ", COLWIDTH, Num[Ri][Si] );
               strcat( tablehead, mbuf );

               printusing( formatstr, Area, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );

               printusing( formatstr, Blankarea, mbuf );
               strcat( tablehead, mbuf ); strcat( tablehead, SPACE );
            }

            sprintf( mbuf, "%*.*f ", COLWIDTH, COLPREC, Segments[2*Si] );
            strcat( tablehead, mbuf );

            sprintf( mbuf, "%*.*f ", COLWIDTH, COLPREC, Segments[2*Si+1] );
            strcat( tablehead, mbuf );

            sprintf( mbuf, "%*.*f ", COLWIDTH, COLPREC, Width[Ri] );
            strcat( tablehead, mbuf );

            sprintf( mbuf, "%*.*f ", COLWIDTH, COLPREC, Phi[Ri] );
            strcat( tablehead, mbuf );

            sprintf( mbuf, "%*.*f", COLWIDTH, COLPREC, Inc[Ri] );
            strcat( tablehead, mbuf );

            anyoutf( 3, tablehead );
            if (fpOUT != NULL)
               (void) fprintf( fpOUT, " %s\n", tablehead );

            if (gdstable)
            {
               filltab( Setin, Setlevel, Subdim, Axnum, Tname, rowcount, Option,
                        Radius[Ri],
                        Radkpc,
                        Cumsum,
                        Sum[Ri][Si],
                        Mean,
                        sqrt(fabs(Var[Ri][Si])),       /* Rms = SQRT(Variance) */
                        Num[Ri][Si],
                        Numblanks[Ri][Si],
                        Datamin[Ri][Si],
                        Datamax[Ri][Si],
                        Segments[2*Si],
                        Segments[2*Si+1],
                        Width[Ri],
                        Phi[Ri],
                        Inc[Ri],
                        Position,
                        PhysposST,
                        subsetgrid,
                        subsetphys );
               rowcount++;
            }
         } /* For all segments */
      } /* For all radii */

      anyoutf( 3, border );
      if (fpOUT != NULL)
         fprintf( fpOUT, "!%s\n", border );
      anyoutf( 3, " " );

   } /* End of all subsets */



   /*----------------------------------------*/
   /* The results are known and displayed    */
   /* and/or stored. At this stage the user  */
   /* can create plots of any of the         */
   /* measured quantities against radius.    */
   /*----------------------------------------*/
   if (plot_exist)
      pgend_c();
   plot_exist = NO;
   do
   {
      int     maxitems = 0;
      fint    plot;
      if (Option == 1)
      {
         if (calcmedian)
         {
            strcpy( messbuf, "1=sum 2=mean 3=rms 4=min 5=max 6=A 7=A-bl 8=med: [quit]" );
            maxitems = 8;
         }
         else
         {
            strcpy( messbuf, "1=sum 2=mean 3=rms 4=min 5=max 6=A 7=A-bl: [quit]" );
            maxitems = 7;
         }
      }
      if (Option == 2)
      {
         strcpy( messbuf, "1=sd 2=sd-t 3=fo-sd 4=fo-sd-t 5=sum 6=A 7=A-bl: [quit]" );
         maxitems = 7;
      }
      if (Option == 3)
      {
         strcpy( messbuf, "1=Msd-t 2=M-t 3=Cu.M 4=Msd 5=M 6=Cu.M 7=S 8=A 9=A-bl: [q]" );
         maxitems = 9;
      }
      nitems = 1;
      dfault = REQUEST;
      r1 = userint_c( &plot, &nitems, &dfault, KEY_PLOTOPT,
                      tofchar(messbuf) );
      quit = (r1 == 0);
      if (!quit)
      {
         /* Open (new) plot device for 'radii' plots */
         if (!plot_exist)
         {
            fint  nxysub[2];
            nxysub[0] = nxysub[1] = 1;
            nitems = 2;
            dfault = HIDDEN;
            r1 = userint_c( nxysub,
                            &nitems,
                            &dfault,
                            KEY_PGMOSAIC,
                            MES_PGMOSAIC );
            initplot( NO, nxysub[0], nxysub[1] );
            plot_exist = YES;                     /* Open new device only once */
         }
         plot = MYMAX( plot, 1 );
         plot = MYMIN( plot, maxitems );
         plotprofile( Option, plot, Nrad, Numsegm, subpixtot );
      }
      cancel_c( KEY_PLOTOPT );
   }
   while(!quit);


   if (plot_exist)
   {
      putid();                                     /* Plot username and date */
      pgend_c();                                   /* Close plots (and send) */
   }
   freefmatrix( image, BgridloI[0], BgridloI[1] ); /* Free Image data buffer */

   for (m = 0; m < Setdim; m++)
   {
      free( Ctype[m].a );
      free( Cunit[m].a );
   }

   /*----------------------------------------*/
   /* Free memory allocated for the median   */
   /* arrays.                                */
   /*----------------------------------------*/
   if (calcmedian)
   {
      for (n = 0; n < Nrad; n++)
      {
         for (segm = 0; segm < (int) Numsegm; segm++)
            free( medianarray[n][segm] );
      }
   }

   if (fpOUT != NULL)
      fclose( fpOUT );
   finis_c();                                      /* Quit Hermes */
   return( EXIT_SUCCESS );
}
