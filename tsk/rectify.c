/*
                           COPYRIGHT (c) 1994
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>             rectify.dc1

Program:       RECTIFY

Purpose:       Review the geometrical component of a model galaxy
               by fitting surface densities in (user supplied) tilted 
               rings.
               
Category:      ANALYSIS, MODELS

File:          rectify.c

Author:        M. Vogelaar

Keywords:

   INSET=      Give (sub)set with data to fit:

               Maximum number of subsets is 1. The dimension of the
               input structure is 2.
               This set contains to data used to fit the rings.
               It must be possible to convert the units of its subset
               axes to seconds of arc.


   BOX=        Give BOX in ..., ....                     [entire subset]

               Limit the size of the input data from INSET=
               The dots correspond to the axis names of the input
               (sub)set. A box bigger than the frame of the subset
               is not allowed.


   OUTSET=     Give output set (,subset):

               With the fitted model parameters, a set is created
               with the same structure as the input set. Only the
               part defined with BOX= is written to the output set.
               Data outside any rings will be blank in the output
               (unless the output set already existed and contained
               non blank values at these positions).


   PROSET=     Give projected output set (,subset):            [no set]

               The fitted model can be projected onto the plane
               of the central ring. The name of this projection
               is given with PROSET= The grid spacings of this set
               are equal in both directions. The position angle
               of the central ring is aligned with the y-axis.



               KEYWORDS RELATED TO THE MODEL:

   POSITION=   Central position (of galaxy):

               All positions in the model will be calculated wrt.
               this position so it will be the center of all rings.
               The position is entered as grids or physical
               coordinates.
               A two dimensional subset with axes RA and DEC:

               POSITION=10 5            Grid (RA,DEC)=(10,5)
               POSITION=U 45.8 U 20.02  Phys. coord. at RA=45.8,
                                        DEC=20.02 in
                                        units of the RA, DEC axes.
               POSITION=PC              RA, DEC of projection centre
               POSITION=AC              RA, DEC of axis centre
               POSITION=* 10 12 8 * -67 8 9.6
                                        (RA=10h12m8s, DEC=-67d8m9.6s
                                        in epoch of set)

   RADII=      Give (outer) radius of rings (arcsec):

               Maximum number of rings is 2048, but this maximum is
               also limited by the available memory. 


   SEGMENTS=   Give number of segments per ring for n rings:   [calc.]

               If you assume axial symmetry for your model, there is
               no need to define segments in a ring: enter SEGMENTS=1.
               Else, give for each defined ring the number of segments
               that you want to use. If you give less than 'n' values
               the remaining number of segments will be copied from
               the last one entered. The default for ring k with
               k=1..n is: segments = int((k-1)/4)*4


   POSANG=     Give n position angles (deg. N-->E):

               (See description)
               n is the number of rings defined with RADII=
               If you give less than 'n' angles, the remaining angles
               all get the value of the last specified angle.
               Values for the position angles and inclinations could
               (for example) be obtained with program ROTCUR.


   INCLINATION= Give n inclinations (degree):

               (See description) If less than n items are entered, then
               the missing items are copied from the last one entered.


   Z0=         Give n scale heights (arcsec):

               (See description) If less than n items are entered, then
               the missing items are copied from the last one entered.


   PROFILE=    Give dens.prof. perpend. to plane of the rings:   [list]

               Complete tilted ring model with a vertical structure
               for the HI layer.
               The options are:
                      1 gaussian layer ( dispersion = 1*Z0(ring) )
                      2 sech2 layer.
                      3 exponential layer.
                      4 Lorentzian layer.
                      5 box layer.



   ISEED=      Seed (should be negative):                          [-1]

               Different seeds generate different random number
               sequences.


   MAXCLOUDS=  Max. number of Monte Carlo clouds:              [100000]

               Use MAXCLOUDS= random positions in a ring for
               the Monte Carlo simulation.
               See also description at EQDENS=


   EQDENS=     Equal density of M.C. points in all rings?         Y/[N]

               The number in MAXCLOUDS= is used to set the number of
               Monte Carlo points that cover the largest ring area.
               If EQDENS=Y and if the area of a ring is smaller
               than the area of the largest ring, then scale the number
               of generated M.C. points so that equal areas in different
               rings will approximately contain the same number of
               clouds.



               KEYWORDS RELATED TO PLOTTING
               ============================

   GRDEVICE=   Plot device:                           [List of devices]

               Destination of plot, Screen or Hardcopy.


** MOSAIC=     View surface subdivisions x,y:                     [1,1]

               View surface can contain a number of plot pages in
               in X and Y direction (mosaic). Default is 1 plot in
               both X- and Y direction. With this keyword, you can
               force to display more than one perspective view of
               your model on one physical device page.


** LINEWIDTH=  Give line width (1-21):                              [1]

               The width of lines etc. that are plotted, can be
               controlled by this keyword. For a hardcopy, the default
               is 2.


   RADPLOT=    Plot radius vs. pos.ang/inclination?               Y/[N]

               Plot input radius vs. input position angle and
               radius vs. inclination in separate plots.


   PLOTMODEL=  Draw rings in perspective view?                    Y/[N[

               Display model in a perspective view, using
               parameters defined in DISTANCE= RHO=, PHI= & THETA=


   MCPLOT=     Monte Carlo plot?                                  Y/[N]

               Create a 2d-plot with positions generated by the
               random number generator.


   TRPLOT=     Tilted ring overview?                              Y/[N]

               For an overview of the tilted ring, 1) plot the
               components of the spin vectors in the xy plane of the
               central disk (first ring) and 2) plot the intersection
               of all rings with the central disk. Just before
               plotting, keyword GRDEVICE= is asked, so you are able
               to change the output destination for this plot.


   DRPLOT=     Plot surface density vs radius?                    Y/[N]

               Plot the fitted parameters agains radius. Draw a
               horizontal line from inner radius to outer radius and
               mark outer radius. Just before plotting, keyword
               GRDEVICE= is asked, so you are able to change the
               output destination for this plot.


   COMPVALS=   Values to include in plot:                        [None]

               If you plotted the fitted ring parameter vs. radius
               (DRPLOT=Y) it can be handy to include some data
               for comparison. The data is given as a list of
               reals (entered manually or via a default file or a
               recall file). The maximum number of values that can
               be specified  is equal to the maximum number of rings.


               VIEW RESULTS OBTAINED IN PREVIOUS RUN
               =====================================

               To view previously obtained results stored in a GDS
               table in a GIPSY descriptor file, use the following
               keywords:

** TABSET=     View table results from set:                 [skip plot]

               Name (no subset level) of set containing
               table data. This must be a set previously made 
               by RECTIFY.


   TABNAME=    Give name of GDS table:                           [stop]

               The name of the table (not a column name) can be 
               obtained by running the task TABLE. In most cases the 
               table name will be RECTIFY, but if you used a default 
               file for RECTIFY, it will be the name of that default 
               file. After TABSET= and TABNAME= a plot will appear 
               with the fitted values (densities) as function
               of radius and the keyword PLOTSEG= will be prompted.


   PLOTSEG=    Ring number, max. segments:                       [stop]

               This keyword is asked in a loop that can be aborted
               by pressing carriage return. The first number is the
               ring number. The second is the maximum number of
               segments that you want to display in your plot.
               The default for the second number is the total number
               of segments that was defined for this ring.


               KEYWORDS RELATED CREATING GDS TABLES
               ====================================

   TABNAME=    Give name of GDS table to store results:  [Name of task]

               A table will be stored in the output set OUTSET=
               The maximum length of the table name is less than 9
               characters. Columns in a table are created on set level.
               The tables can be viewed and manipulated with
               program TABLE. These tables are not the tables that
               are written to your screen and GIPSY log file but have
               similar contents.


   OVERTAB=    Overwrite table?                                   [Y]/N

               If a table exists, ask user permission to overwrite.
               If OVERTAB=N, a new name is asked.



               KEYWORDS RELATED TO THE VIEWING TRANSFORMATION
               ==============================================

               It is possible to view your rings in a 3-plot. The
               viewing transformation is controlled by the foll

   DISTANCE=   Distance of the eye from the screen:        [calculated]

               Control amount of perspective with this
               unitless number.


** RHO=        Distance between viewpoint and origin.      [calculated]


   THETA=      Angle view vector wrt. pos. x-axis (deg):           [30]

               Angle between view vector and positive x-axis wrt.
               the positive x-axis (degrees).
               x-axis is equivalent to the first subset axis.


   PHI=        Angle view vector wrt. pos. z-axis (deg):           [60]

               Angle between view vector and positive z-axis wrt.
               the positive z-axis (degrees).
               PHI= ranges from 0 to 180 deg.
               z-axis is equivalent to the image value axis.


   REPEAT=     Repeat perspective view?                           [Y}/N

               Replot same subset with new values for DISTANCE=
               RHO=, PHI=, THETA=. The defaults are the previous values
               of these keywords.



Description:   RECTIFY


               Introduction
               ============

               RECTIFY is used as part of a series of programs to
               create a model galaxy which is a 'best fit' to an
               existing HI data cube. You can review the geometrical 
               component of your model galaxy after RECTIFY fitted 
               the surface densities in the tilted (and perhaps
               segmented) rings that are part of that model.
               
               So RECTIFY determines fitted surface densities in a
               tilted ring model as function of radius and azimuth
               (SEGMENTS=). The program uses data from an existing 
               HI map (INSET=) to fit the parameters of the model,
               which is a set of rings with radius, position angle, 
               inclination, scale height (Z0=) and a definition for the 
               assumed vertical structure for the HI layer (PROFILE=). 
               RECTIFY fits the surface densities per ring or per segment 
               and transforms the model into a set (OUTSET=) so that it 
               will be possible to compare the model with the original 
               set. The fitted segments can also be projected onto the 
               plane of the central disk, the name of this set is given 
               in PROSET=


               Model parameters
               ================

               A (simplified) tilted ring model consists of a number
               of concentric rings with arbitrary spatial orientations.
               A ring is characterized by an inner and outer radius.
               The outer radii are given by RADII=. The inner radii
               are calculated. The first disk has inner radius equal
               to 0 and outer radius equal to the first number in RADII=.
               The orientation of a ring is determined by two angles.
               1) POSANG=. The position angle of the major
                  axis of a galaxy is defined as the angle taken in
                  anti-clockwise direction between the north direction
                  in the sky and the major axis of the receding half
                  of that galaxy (Rots 1975) astron, astrophys 45, 43.
               2) INCLINATION= The inclination of the ring with respect
                  to the plane of the sky.

               The tilted ring model is completed with a choice
               of the width of the HI layer (Z0=) for each ring and
               a vertical structure (PROFILE=) for all rings. The
               options for this vertical structure are:

               1 gaussian layer ( dispersion = 1*Z0(ring) )
               2 sech2 layer.
               3 exponential layer.
               4 Lorentzian layer.
               5 box layer.

               Each of these distributions can be plotted and tested
               with the GIPSY program RANDOM.
               If all ring parameters are given, a table is written in
               the Log file.


               (Non) axial symmetry
               ====================

               If you assume axial symmetry, there is no need to define
               segments in a ring. You must tell to program to ignore
               the existence of segments with SEGMENTS=0
               However, for non axial symmetry, segments can be defined
               for each ring. The last value that you enter with
               SEGMENTS= is copied for all remaining radii that did not
               get a number of segments with SEGMENTS=  There is a
               suitable default for this keyword. Assuming that the
               width of the rings is one bundle and the index number
               of a ring is k (k=1..nrings) then the default number of
               segments for that ring is: s = int((k-1)/4)*4 (if k = 1
               then s = 1)
               resulting in a series s = 1 1 1 1 4 4 4 4 8 8 8...32
               with a maximum of 32.
               Examples: (You entered: RADII= 1 2 3 4 5 6 7 8)
               SEGMENTS= 2 2 4 6 8  ==> 2 2 4 6 8 8 8 8 .... 8
               SEGMENTS= 1          ==> 1 1 1 1 1 1 1 1 .... 1
               SEGMENTS=            ==> 1 1 1 1 4 4 4 4 .... 32

               In practice the total number of segments will be limited
               by the available memory. This amount will be different on 
               different machines.


               Method
               ======

               The surface densities that we want to fit are calculated
               using a Monte Carlo simulation. A certain number of
               'clouds' given in MAXCLOUDS= are randomly distributed
               over a ring. Each position corresponds to a position
               of a pixel in the input set. If we count the number of
               hits in each pixel and divide that number by the number
               of generated clouds in the ring, it is possible to use
               this value as a matrix element containing weights in a
               system with linear equations:

               A(1,1)m(1)  + A(1,2)m(2)  + ... + A(1,NR)m(NR)  = u(1)
               A(2,1)m(1)  + A(2,2)m(2)  + ... + A(2,NR)m(NR)  = u(2)
               ......................................................
               A(i,1)m(1)  + A(i,2)m(2)  + ... + A(i,NR)m(NR)  = u(i)
               ......................................................
               A(NP,1)m(1) + A(NP,2)m(2) + ... + A(NP,NR)m(NR) = u(NP)

               NP    = Number of pixels in the input box (BOX=)
               NR    = Number of rings
               A     = Matrix element containing weights
               m     = Wanted surface densities
               u(i)  = Pixel value of pixel from input with index i

               A cloud has a random position determined by a random
               radius between the inner radius (included) and the
               outer radius (excluded) and a random azimuth between 0
               and PI. But if the radius increases, also the area in-
               creases, so we have to correct the uniform random radius:
               To fill a circle uniformly, the correction would be:
               (rnd is an uniform random number between 0 and 1)

                             Radius = Rhi*sqrt( rnd )

               For a ring with inner radius Rlo and outer radius Rhi
               we find the correction formula:

                    Radius = sqrt( (Rhi^2-Rlo^2)*rnd + Rlo^2 )

               A second correction can be applied for the decreasing
               density of points for increasing areas. The correction
               is initiated with EQDENS=Y. The number in MAXCLOUDS=
               is used to cover the largest area. For other areas
               the number of random clouds is smaller. If EQDENS=N,
               the number in MAXCLOUDS= will be used for all rings.

               Radius and angle correspond to a position in the
               xy plane (in the xyz system of a ring). The value of
               z is randomly selected from the distribution given
               in PROFILE=. The characteristic width of these
               distributions is equal to the value of Z0 of the ring
               that is examined.

               Because NP >> NR, the system is 'overdetermined' and
               we use a least squares method to solve m(i). The method
               is described in Numerical Recipes in C (Press cs., 2nd ed.)
               $15.4, page 674. The Matrix solution is obtained with
               a Gauss-Jordan elimination, Num. Rec. $2.1 pag 39.

               Segments are treated as rings. The number NR has to be
               interpreted as the number of segments in your model and
               not as the number of rings anymore.

               
               Output
               ======
               
               The output set OUTSET= is a set made with the fitted 
               parameters, i.e. the matrix elements m(j) are known 
               after the least squares fit, the normalized matrix A(i,j) 
               is left unchanged and therefore the values of u[i] can 
               be calculated. This vector u[i] is in fact the output 
               set in OUTSET=
               
               A set which contains an image of the input set as seen
               from a position perpendicular to the plane of the central 
               disk (ring with first input radius, position angle and
               inclination) is created with PROSET=  Random positions
               in the rings are converted to positions in the plane 
               of the central ring and so these rings are sampled in
               that plane. The matrix A(i,j) stores the hits at all
               possible positions and after normalization of 'A' a new
               vector 'u' is calculated using the fitted parameters 'm'.
               The number of generated random positions is equal related
               to the number in MAXCLOUDS=
               
               
               Log file
               ========
               
               Results are written in the GIPSY log file. A table shows
               for each segment the fitted parameters (surface densities)
               and their estimated errors and the sum off all pixels in
               a segment (and the errors). 
               The minimized Chi square, the reduced Chi square and an 
               estimated measurement error per pixel are also listed. 
               
               
               Tables
               ======

               The results of the fitting are saved in a GDS table
               with the name of the current task as default name.
               This name is changed with TABNAME= An existing table can
               be overwritten but you are always prompted with OVERTAB=
               before you can do so. The table has the following column
               names:

               LORAD     -Inner radius in arcsec
               HIRAD     -Outer radius in arcsec
               WIDTH     -Width in arcsec
               POSANG    -Position angle of ring in degrees
               INCLIN    -Inclination of ring in degrees
               AREA      -Area of ring in arcsec^2
               SURFDENS  -Fitted surface density in map units of INSET=
               SURFDERR  -Errors in covariance matrix corresponding to
                         -fitted densities
               Z0        -Scale height of ring in arcsec

               Contents of the columns can be viewed with program TABLE.
               But RECTIFY also has a possibility to view previously
               stored data. To enable this feature start RECTIFY with
               TABSET= and TABNAME= You get a plot of all stored
               surface densities. In a loop it is possible to plot
               the values of the segments in a ring. The keyword
               PLOTSEG= accepts a ring index number and a second number
               that specifies the maximum number of segments that you
               want in your plot. The loop (and the program) is aborted
               by pressing carriage return.


               Plots
               =====

               1) Position angles and inclinations

               Keyword RADPLOT=Y shows two plots on one page. The
               first is a plot of radius versus position angle and
               the second plot shows radius versus inclinations.
               The data used in this plot is written in the log file.


               2) The model

               A view of the defined model (set of circles) from an
               arbitrary angle can be obtained with PLOTMODEL=Y.
               First a viewpoint has to be specified (RHO=, THETA=,
               PHI= & DISTANCE=, angles in degrees, distance in grids)
               The 'viewpoint' transformation
               converts positions in world coordinates (== ring coordinates)
               into 'eye' coordinates expressed in a coordinate system
               centered at the viewpoint. A perspective transfor-
               mation produces the actual 2-dim. 'screen' coordinates.
               It has a single vanishing point and the screen axes
               are parallel to the 'eye' coordinates.
               If you want to view the projection of the model on the
               sky, use THETA=90 and PHI=180.
               In the plot, a coordinate system that is aligned with the
               sky (XY) and with the line of sight (Z) is displayed.
               The plot also shows the xyz coordinate system
               of the central disk and labels it with p0, q0, and s0.
               The vector s is the spin vector of a ring. It is aligned
               with the rotation axis of the rings. The vector p
               is a unit vector aligned with the major axis of the
               (projected) ring. The unit vector q is perpendicular
               to p and s. For all rings the vectors p and s are plotted.


               3) Overview tilted ring

               With TRPLOT=Y, a 2d-plot of the tilted ring model is
               created. The first plot shows the end points of the
               spin vectors of each ring with respect to the central
               spin vector (positioned at 0,0 ). A second plot shows
               the intersections of the rings with the central disk.


               4) Monte Carlo plot

               If you want to view the random process of selecting
               positions in a ring, use MCPLOT=Y. You will see how
               the rings are build up. The plotting of positions is
               slow so take a small number for MAXCLOUDS= if you
               want to use this feature.


               5) Surface density vs. radius

               DRPLOT=Y will plot the fitted densities vs. radius.
               A line is drawn between the inner radius to the
               outer radius along the X-axis at height equal to the
               fitted density.
               If you want to compare the plotted data with an
               array of numbers of another source (maybe some input
               densities for another program), use COMPVALS=
               to read this data. Then also these values will be plotted.
               Make sure that the densities are in the same units.


Example:       An example of a COLA file where a set of rings was fitted to 
               data in NGC3198 p 7 (subset param 7 == total HI map)

               ! The COLA file can also be used to start nhermes (or ngipsy 
               ! if plots are created) as a batch.
               ! The syntax is: nhermes -lmylog batch
               ! The file 'mylog' is the log file for the RECTIFY output.
               !
               !===========================================================
               "RECTIFY 
               INSET=NGC3198 7
               OUTSET=rect_fit
               PROSET=rect_depro
               BOX=-101 -87 102 106
               POSITION=-7.8359895314883 8.8999911586812
               SEGMENTS=1::4 4::4 8::4 12::4 16::4 20::4 24
               PROFILE=1
               ISEED=-10
               MAXCLOUDS=
               EQDENS=
               GRDEVICE=
               MOSAIC=
               LINEWIDTH=
               RADPLOT=
               PLOTMODEL=
               MCPLOT=
               TRPLOT=
               DRPLOT=
               COMPVALS=
               TABSET=
               TABNAME=
               PLOTSEG=
               TABNAME=
               OVERTAB=
               DISTANCE=
               RHO=
               THETA=
               PHI=
               REPEAT=
               
               POSANG=      2.092830e+02 2.119370e+02 2.136150e+02 2.149110e+02
               2.162340e+02 2.169150e+02 2.172880e+02 2.175830e+02 2.176790e+02
               2.176830e+02 2.176370e+02 2.173810e+02 2.170590e+02 2.166300e+02
               2.162810e+02 2.159940e+02 2.158050e+02 2.157010e+02 2.156520e+02
               2.155100e+02 2.153090e+02 2.151090e+02 2.149150e+02 2.146560e+02
               2.143530e+02 2.140620e+02 2.138230e+02 2.136580e+02 2.136440e+02
               2.136890e+02 2.137610e+02 2.138460e+02 2.140110e+02 2.142470e+02
               2.146760e+02
               RADII=       1.800000e+01 3.600000e+01 5.400000e+01 7.200000e+01
               9.000000e+01 1.080000e+02 1.260000e+02 1.440000e+02 1.620000e+02
               1.800000e+02 1.980000e+02 2.160000e+02 2.340000e+02 2.520000e+02
               2.700000e+02 2.880000e+02 3.060000e+02 3.240000e+02 3.420000e+02
               3.600000e+02 3.780000e+02 3.960000e+02 4.140000e+02 4.320000e+02
               4.500000e+02 4.680000e+02 4.860000e+02 5.040000e+02 5.220000e+02
               5.400000e+02 5.580000e+02 5.760000e+02 5.940000e+02 6.120000e+02
               6.300000e+02  
               INCLINATION= 7.645950e+01 7.349020e+01 7.204080e+01 7.132620e+01
               7.107400e+01 7.086810e+01 7.073370e+01 7.054340e+01 7.044120e+01
               7.031490e+01 7.027940e+01 7.033710e+01 7.035760e+01 7.025710e+01
               7.005100e+01 6.993850e+01 6.990560e+01 6.992830e+01 7.004700e+01
               7.015080e+01 7.015080e+01 7.042400e+01 7.064860e+01 7.103810e+01
               7.149680e+01 7.163160e+01 7.202490e+01 7.224920e+01 7.247960e+01
               7.268960e+01 7.286270e+01 7.295460e+01 7.305270e+01 7.327160e+01
               7.393910e+01
               Z0=          1.000000e+01 1.000000e+01 1.000000e+01 1.000000e+01 
               1.000000e+01 1.000000e+01 1.000000e+01 1.000000e+01 1.000000e+01
               1.000000e+01 1.000000e+01 1.000000e+01 1.000000e+01 1.000000e+01
               1.000000e+01 1.000000e+01 1.000000e+01 1.000000e+01 1.000000e+01
               1.000000e+01 1.000000e+01 1.100000e+01 1.200000e+01 1.400000e+01
               1.600000e+01 1.800000e+01 2.000000e+01 2.200000e+01 2.400000e+01
               2.600000e+01 2.800000e+01 3.000000e+01 3.200000e+01 3.400000e+01
               3.600000E+01
               
               COMPVALS= 
               3.443390e+20/(1.7114975189653e19*cos(RAD(7.645950e+01)))
               4.024280e+20/(1.7114975189653e19*cos(RAD(7.349020e+01)))
               4.690980e+20/(1.7114975189653e19*cos(RAD(7.204080e+01)))
               5.445430e+20/(1.7114975189653e19*cos(RAD(7.132620e+01)))
               6.381640e+20/(1.7114975189653e19*cos(RAD(7.107400e+01)))
               6.785450e+20/(1.7114975189653e19*cos(RAD(7.086810e+01)))
               6.926840e+20/(1.7114975189653e19*cos(RAD(7.073370e+01)))
               6.770860e+20/(1.7114975189653e19*cos(RAD(7.054340e+01)))
               6.459870e+20/(1.7114975189653e19*cos(RAD(7.044120e+01)))
               5.997140e+20/(1.7114975189653e19*cos(RAD(7.031490e+01)))
               5.454890e+20/(1.7114975189653e19*cos(RAD(7.027940e+01)))
               4.962500e+20/(1.7114975189653e19*cos(RAD(7.033710e+01)))
               4.592390e+20/(1.7114975189653e19*cos(RAD(7.035760e+01)))
               4.217650e+20/(1.7114975189653e19*cos(RAD(7.025710e+01)))
               4.217650e+20/(1.7114975189653e19*cos(RAD(7.005100e+01)))
               4.217650e+20/(1.7114975189653e19*cos(RAD(6.993850e+01)))
               4.217650e+20/(1.7114975189653e19*cos(RAD(6.990560e+01)))
               3.233590e+20/(1.7114975189653e19*cos(RAD(6.992830e+01)))
               3.045750e+20/(1.7114975189653e19*cos(RAD(7.004700e+01)))
               2.760480e+20/(1.7114975189653e19*cos(RAD(7.015080e+01)))
               2.448930e+20/(1.7114975189653e19*cos(RAD(7.015080e+01)))
               2.171780e+20/(1.7114975189653e19*cos(RAD(7.042400e+01)))
               1.903990e+20/(1.7114975189653e19*cos(RAD(7.064860e+01)))
               1.610130e+20/(1.7114975189653e19*cos(RAD(7.103810e+01)))
               1.344120e+20/(1.7114975189653e19*cos(RAD(7.149680e+01)))
               1.148650e+20/(1.7114975189653e19*cos(RAD(7.163160e+01)))
               1.044350e+20/(1.7114975189653e19*cos(RAD(7.202490e+01)))
               9.934400e+19/(1.7114975189653e19*cos(RAD(7.224920e+01)))
               9.245870e+19/(1.7114975189653e19*cos(RAD(7.247960e+01)))
               8.053860e+19/(1.7114975189653e19*cos(RAD(7.268960e+01)))
               6.675810e+19/(1.7114975189653e19*cos(RAD(7.286270e+01)))
               5.385430e+19/(1.7114975189653e19*cos(RAD(7.295460e+01)))
               4.159810e+19/(1.7114975189653e19*cos(RAD(7.305270e+01)))
               3.017970e+19/(1.7114975189653e19*cos(RAD(7.327160e+01)))
               2.336700e+19/(1.7114975189653e19*cos(RAD(7.393910e+01)))
               "
               ! NOTE LAST QUOTE


Updates:       Jul 14, 1994: MV, Document created.


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

#include "gdsc_range.h"
#include "gdsc_grid.h"
#include "gdsc_fill.h"
#include "gdsc_name.h"
#include "gdsc_ndims.h"
#include "gdsbox.h"
#include "gdsasn.h"
#include "gdscss.h"
#include "gdsinp.h"
#include "gdsout.h"
#include "gdspos.h"           /* Define a position in a subset.*/
#include "gdsi_read.h"
#include "gdsi_write.h"
#include "gdsd_rreal.h"
#include "gdsd_rdble.h"
#include "gdsd_rchar.h"
#include "gdsd_wdble.h"


/* User input */

#include "userfio.h"          /* Easy-C companions for GIPSY interface routines */
#include "userint.h"
#include "usertext.h"
#include "userreal.h"
#include "userdble.h"
#include "userlog.h"
#include "userchar.h"
#include "userint.h"


/* Related to tables */

#include "gdsa_crecol.h"      /* Creates a column in a GDS descriptor file.*/
#include "gdsa_delcol.h"      /* Deletes a column in a GDS table.*/
#include "gdsa_colinq.h"      /* Give information about columns in a GDS table.*/
#include "gdsa_tabinq.h"      /* Table info */
#include "gdsa_deltab.h"      /* Delete table */
#include "gdsa_wcreal.h"      /* Write real items to a column in a GDS table.*/
#include "gdsa_wcdble.h"      /* Double.*/
#include "gdsa_wcint.h"       /* Integer. */
#include "gdsa_rcdble.h"      /* Read array of doubles from table */
#include "gdsa_rcint.h"


/* Miscellaneous */

#include "setfblank.h"
#include "setdblank.h"
#include "myname.h"
#include "anyout.h"
#include "nelc.h"
#include "cancel.h"
#include "status.h"
#include "cancel.h"
#include "error.h"
#include "stabar.h"
#include "reject.h"
#include "pgplot.h"           /* Include all pgplot includes */
#include "factor.h"
#include "minmax1.h"
#include "minmax3.h"
#include "wminmax.h"
#include "axcoord.h"
#include "getusernam.h"
#include "getdate.h"
#include "presetd.h"
#include "randev.h"
#include "iran.h"             /* Include for the random number generator */
#include "timer.h"


#define AXESMAX    10         /* Max. allowed number of axes in a set */
#define SUBSMAX    1          /* Max. number of substructures to be specified */
#define MAXBUF     8192       /* Buffer size for I/O */
#define BIGSTORE   160        /* Length of a string */
#define VERSION    "1.0"      /* Version number of this program */
#define TASKNAMLEN 20         /* Store task name in str. with this length */
#define ITEMLEN    8          /* Table, column name length */
#define VARLEN     132        /* Char. size in tables */
#define NONE       0          /* Default values for use in userxxx routines */
#define REQUEST    1
#define HIDDEN     2
#define EXACT      4
#define FITSLEN    20         /* Length of a fits item from header */
#define RANSCALE   16777216.0 /* Rescale 'iran' number to [0, 1> */
#define MAXRINGS   2048       /* Max. number of rings (only in user.fies)*/
#define NO         0          /* Booleans */
#define YES        1


/* Define colours */

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


/* Keywords and messages */

#define KEY_INSET         tofchar("INSET=")
#define KEY_OUTSET        tofchar("OUTSET=")
#define KEY_PROSET        tofchar("PROSET=")
#define KEY_TABSET        tofchar("TABSET=")
#define KEY_BOX           tofchar("BOX=")
#define KEY_CPOS          tofchar("POSITION=")
#define KEY_RHO           tofchar("RHO=")
#define KEY_THETA         tofchar("THETA=")
#define KEY_PHI           tofchar("PHI=")
#define KEY_EYE           tofchar("DISTANCE=")
#define KEY_WINDOW        tofchar("WINDOW=")
#define KEY_MOSAIC        tofchar("MOSAIC==")
#define KEY_RADII         tofchar("RADII=")
#define KEY_POSANG        tofchar("POSANG=")
#define KEY_INCLINATION   tofchar("INCLINATION=")
#define KEY_Z0            tofchar("Z0=")
#define KEY_SEGMENTS      tofchar("SEGMENTS=")
#define KEY_CONT          tofchar("REPEAT=")
#define KEY_RADPLOT       tofchar("RADPLOT=")
#define KEY_SKYPLOT       tofchar("SKYPLOT=")
#define KEY_TRPLOT        tofchar("TRPLOT=")
#define KEY_MCPLOT        tofchar("MCPLOT=")
#define KEY_DRPLOT        tofchar("DRPLOT=")
#define KEY_PLOTMODEL     tofchar("PLOTMODEL=")
#define KEY_PLOTSEG       tofchar("PLOTSEG=")
#define KEY_LINEWIDTH     tofchar("LINEWIDTH=")
#define KEY_TABNAME       tofchar("TABNAME=")
#define KEY_OVERTAB       tofchar("OVERTAB=")
#define KEY_PROFILE       tofchar("PROFILE=")
#define KEY_ISEED         tofchar("ISEED=")
#define KEY_MAXCLOUDS     tofchar("MAXCLOUDS=")
#define KEY_EQDENS        tofchar("EQDENS=")
#define KEY_COMPVALS      tofchar("COMPVALS=")


#define MES_INSET         tofchar("Give (sub)set with data to fit:")
#define MES_OUTSET        tofchar("Give output set (,subset): ")
#define MES_PROSET        tofchar("Give projected output set (,subset):   [no set]")
#define MES_TABSET        tofchar("View table results from set:     [skip plot]")
#define MES_BOX           tofchar(" ")
#define MES_CPOS          tofchar("Central position (of galaxy):")
#define MES_MOSAIC        tofchar("View surface subdivisions x,y:   [1,1]")
#define MES_WINDOW        tofchar("Xmin, Ymin, Xmax, Ymax:          [calculated]")


/* Column names (tables) */

#define COL_SEGMENTS      tofchar("SEGMENTS")
#define COL_SEGNR         tofchar("SEGNR")
#define COL_RINGNR        tofchar("RINGNR")
#define COL_LORAD         tofchar("LORAD")
#define COL_HIRAD         tofchar("HIRAD")
#define COL_WIDTH         tofchar("WIDTH")
#define COL_POSANG        tofchar("POSANG")
#define COL_INCLIN        tofchar("INCLIN")
#define COL_Z0            tofchar("Z0")
#define COL_SURFDENS      tofchar("SURFDENS")
#define COL_SURFDERR      tofchar("SURFDERR")
#define COL_AREA          tofchar("AREA")



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


#define MYMAX(a,b) ( (a) > (b) ? (a) : (b) )    /* Max. of two numbers */
#define MYMIN(a,b) ( (a) > (b) ? (b) : (a) )    /* Min. of two numbers */
#define NINT(a)    ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
                                                /* Nearest integer*/
#define TORAD(a)   ( (a)*atan(1.0L)/45.0L )     /* Convert degrees to radians */
#define TODEG(a)   ( (a)*45.0L/atan(1.0L) )     /* Convert radians to degrees */
#define ABS(a)     ( (a) < 0 ? (-(a)) : (a) )   /* Absolute value of a */
#define SIGN(a)    ( (a) < 0 ? (-1) : (1) )     /* negative/ positive number? */
#define SWAP(a,b)  { double temp=(a);(a)=(b);(b)=temp; } /* Swap 2 numbers */
#define PI         3.141592653589793115997963468544


/* Input of set, subsets: */

static fchar    Setin;                    /* Name of the set */
static fchar    Setout;                   /* Name of the model set */
static fchar    Setpro;                   /* Deprojected model */
static fchar    Settab;                   /* Set containing table results */
static fint     subin[SUBSMAX];           /* Array for the subset coordinate words */
static fint     subout[SUBSMAX];          /* Array for the subset cw's for output */
static fint     subpro[SUBSMAX];          /* Subset in deprojection */
static fint     nsubs;                    /* Number of input subsets */
static fint     nsubsO;                   /* Number of model output subsets */
static fint     nsubsP;                   /* Number of model output subsets */
static fint     axnum[AXESMAX];           /* GDSINP axis numbers array */
static fint     axcount[AXESMAX];         /* GDSINP axis lengths array */
static fint     axnumO[AXESMAX];          /* Version for GDSOUT */
static fint     axnumP[AXESMAX];          /* Version for GDSOUT */
static fint     axcountO[AXESMAX];        /* Version for GDSOUT */
static fint     axcountP[AXESMAX];        /* Version for GDSOUT */
static fint     setdim;                   /* Dimension of the set */
static fint     subdim;                   /* Dimension of the subset */
static fint     maxaxes  = AXESMAX;       /* Convert parameters to variables */
static fint     maxsubs  = SUBSMAX;       /* Maximum number of subsets */
static fint     setlevel;                 /* Indicate set level */


/* Input of area etc.:*/

static fint     cwlo, cwhi;               /* Coordinate words */
static fint     frameLO[AXESMAX];         /* Coordinate words for frame */
static fint     frameHI[AXESMAX];
static fint     boxLO[AXESMAX];           /* Coordinate words for box */
static fint     boxHI[AXESMAX];


/* Variables related to perspective transform */

static double    v11, v12, v13, v14;      /* Global matrix elements for view transform! */
static double    v21, v22, v23, v24;
static double    v31, v32, v33, v34;
static double    v41, v42, v43, v44;
static double    rho, theta, phi;         /* Specify the viewpoint */
static double    d_eye;                   /* Distance of the eye from screen */


/* PGPLOT related */

static bool      plotopen;                /* Is plot device open? */
static char      xtitle[FITSLEN];
static char      ytitle[FITSLEN];
static char      mapunits[FITSLEN];


/* Miscellaneous */

static char      taskname[TASKNAMLEN+1];  /* Name of current task */
static char      messbuf[BIGSTORE];       /* Buffer for text message */
static float     xs_min, ys_min;          /* Screen coordinates */
static float     xs_max, ys_max;
static float     xwmin, xwmax;
static float     ywmin, ywmax;
static float     zwmin, zwmax;
static float     image[MAXBUF];           /* Read buffer for 'mu' array */
static double    dblank;                  /* System double blank */
static float     fblank;                  /* System float blank */

/* Characteristics of rings */

static fint      nrings;                  /* Total number of rings */
static fint      nsegs;                   /* Total number of segments */
static double    *radii = NULL;           /* Temporary help array */
static double    *lorad = NULL;           /* Inner radius */
static double    *hirad = NULL;           /* Outer radius */
static double    *inclinations = NULL;    /* Inclination of ring(s) */
static double    *posangs = NULL;         /* Position angles */
static double    *Z0 = NULL;              /* Scale heights */
static double    *surfdens = NULL;        /* Surface density in a ring */
static double    *surfderr = NULL;        /* Error in fitted density */
static fint      *segments = NULL;        /* Mumber of segments per ring */




static void initglobals( void )
/*-------------------------------------------------------------*/
/* PURPOSE: Initialize global variables.                       */
/*-------------------------------------------------------------*/
{
   setlevel = 0;
   plotopen = NO;
   setfblank_c( &fblank );
   setdblank_c( &dblank );

}



void anyoutC( int  scrnum,
              char *anystr )
/*-------------------------------------------------------------*/
/* PURPOSE: Write a message to the screen/log file             */
/* The C version of 'anyout'.                                  */
/*-------------------------------------------------------------*/
{
   fint Fscrnum = (fint) scrnum;
   anyout_c( &Fscrnum, tofchar( anystr ) );
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



void anyoutT( int  scrnum,
              char *anystr )
/*-------------------------------------------------------------*/
/* PURPOSE: Write a message prefixed by the taskname.          */
/*-------------------------------------------------------------*/
{
   int    m;
   char   *newstr;

   m = strlen( taskname ) + strlen( anystr ) + 4;
   newstr = malloc( m );
   if (!newstr)
   {
      anyoutC( 1, "Memory allocation problems in 'anyout'" );
      return;
   }
   (void) sprintf( newstr, "-%s: %s", taskname, anystr );
   anyoutC( scrnum, newstr );
   free( newstr );
}



static bool usercont( fchar Keyword,
                      char  *message,
                      bool  def,
                      fint  dfault  )
/*-------------------------------------------------------------*/
/* PURPOSE: Ask user confirmation to continue.                 */
/*-------------------------------------------------------------*/
{
   bool    cont;
   fint    nitems;
   fint    r1;


   cont   = toflog( def );
   nitems = 1;
   r1     = userlog_c( &cont, &nitems, &dfault, Keyword,
                       tofchar( message ) );
   cancel_c( Keyword );
   cont   = tobool( cont );
   return( cont );
}



static void movexy( float x, float y )
/*-------------------------------------------------------------*/
/* PURPOSE: Move pgplot cursor to x,y                          */
/* Alternative pgmove                                          */
/*-------------------------------------------------------------*/
{
   pgmove_c( &x, &y );
}



static void drawxy( float x, float y )
/*-------------------------------------------------------------*/
/* PURPOSE: Draw a line to x,y                                 */
/* Alternative pgdraw                                          */
/*-------------------------------------------------------------*/
{
   pgdraw_c( &x, &y );
}



static void setlabel( char  *xtitle,
                      char  *ytitle,
                      char  *toptitle )
/*-------------------------------------------------------------*/
/* PURPOSE: Put labels along plot axes.                        */
/*-------------------------------------------------------------*/
{
   pglab_c( tofchar(xtitle), tofchar(ytitle), tofchar(toptitle) );
}



static void setcolor( fint col )
/*-------------------------------------------------------------*/
/* PURPOSE: Set color to 'col'.                                */
/* Alternative pgsci.                                          */
/*-------------------------------------------------------------*/
{
   pgsci_c( &col );
}



static void setmarker( float x, float y, fint sym )
/*-------------------------------------------------------------*/
/* PURPOSE: Set marker op x, y.                                */
/*-------------------------------------------------------------*/
{
   fint one = 1;
   pgpt_c( &one, &x, &y, &sym );
}



void initviewtransform( float rho,
                        float theta,
                        float phi )
/*-------------------------------------------------------------*/
/* PURPOSE: Initialize the matrix that is used for a viewing   */
/*          transform.                                         */
/* For a given rho, theta and phi, this routine need to be     */
/* called only once.                                           */
/*-------------------------------------------------------------*/
{
   double  sinth, costh, sinph, cosph;

   theta *= PI/180.0;
   phi   *= PI/180.0;

   costh = cos(theta);
   sinth = sin(theta);
   cosph = cos(phi);
   sinph = sin(phi);

   v11 = -sinth;    v12 = -costh*cosph;    v13 = -costh*sinph;   v14 = 0.0;
   v21 =  costh;    v22 = -sinth*cosph;    v23 = -sinth*sinph;   v24 = 0.0;
   v31 =  0.0;      v32 =  sinph;          v33 = -cosph;         v34 = 0.0;
   v41 =  0.0;      v42 =  0.0;            v43 =  rho;           v44 = 1.0;
}



void getviewpoint( double *rho,
                   double *theta,
                   double *phi,
                   double *d_eye )
/*-------------------------------------------------------------*/
/* PURPOSE: Get parameters to initialize viewing transform     */
/* rho is the distance from origin to viewpoint. theta is angle*/
/* of viewvector with respect to positive x-axis and Phi is    */
/* angle of viewvector wrt. positive y-axis                    */
/*-------------------------------------------------------------*/
{
   fint   nitems = 1;
   fint   dfault;
   fint   r1;
   int    agreed;
   double dummy;


   dfault = HIDDEN;
   (void) sprintf( messbuf,
                  "Distance of the eye from a screen:  [%.2f]",
                   *d_eye );
   r1 = userdble_c( d_eye,
                    &nitems,
                    &dfault,
                    KEY_EYE,
                    tofchar(messbuf) );
   if (*d_eye <= 0.0)
      *d_eye = 1.0;

   dfault = HIDDEN;
   do
   {
      (void) sprintf( messbuf,
                     "Distance to viewpoint:   [%.2f]",
                     *rho );
      dummy = *rho;
      r1 = userdble_c( &dummy,
                       &nitems,
                       &dfault,
                       KEY_RHO,
                       tofchar(messbuf) );
      agreed = (dummy > 0.0);
      if (!agreed)
         reject_c( KEY_RHO, tofchar("Must be > 0.0!") );
   }
   while (!agreed);
   *rho = dummy;

   dfault = REQUEST;
   sprintf( messbuf,
            "Angle view vector wrt. pos. x-axis (deg):  [%.2f]",
            *theta );
   dummy = *theta;
   r1 = userdble_c( &dummy,
                    &nitems,
                    &dfault,
                    KEY_THETA,
                    tofchar(messbuf) );
   *theta = dummy;

   do
   {
      (void) sprintf( messbuf,
                     "Angle view vector wrt. pos. z-axis (deg):   [%.2f]",
                     *phi );
      dummy = *phi;
      r1 = userdble_c( &dummy,
                       &nitems,
                       &dfault,
                       KEY_PHI,
                       tofchar(messbuf) );
      agreed = (dummy >= 0.0 && dummy <= 180.0);
      if (!agreed)
         reject_c( KEY_PHI, tofchar("0.0<=phi<=180") );
   }
   while (!agreed);
   *phi = dummy;

   cancel_c( KEY_RHO );
   cancel_c( KEY_THETA );
   cancel_c( KEY_PHI );
   return;
}



void coord2screen( double  xx,
                   double  yy,
                   double  zz,
                   float  *px,
                   float  *py )
/*-------------------------------------------------------------*/
/* PURPOSE: Transform a world coordinate to a screen coordin-  */
/*          nate.                                              */
/* Transform the three dimensional point x,y,z to the screen   */
/* coordinates px, py. The conversion consists of a viewing    */
/* and a perspective transformation.                           */
/*-------------------------------------------------------------*/
{
   double    xe, ye, ze;

   /* Eye coordinates */
   xe = v11*xx + v21*yy;
   ye = v12*xx + v22*yy + v32*zz;
   ze = v13*xx + v23*yy + v33*zz + v43;

   /* Screen coordinates */
   *px = (float) (d_eye * xe/ze);
   *py = (float) (d_eye * ye/ze);
}



void findscreenminmax( float *xs_min, float *xs_max,
                       float *ys_min, float *ys_max,
                       float xwmin, float xwmax,
                       float ywmin, float ywmax,
                       float zwmin, float zwmax )
/*-------------------------------------------------------------*/
/* PURPOSE: Given the min, max in world coordinates, determine */
/*          the min, max in screen coordinates for a given     */
/*          viewing transformation.                            */
/* The axis lengths are known and the range of the data values */
/* is known. Construct the volume cube and determine the mini- */
/* mum and maximum values in screen coordinates of this volume.*/
/*-------------------------------------------------------------*/
{
   float xs[8], ys[8];
   fint  ndat = 8;


   coord2screen( xwmin, ywmin, zwmax, &xs[0], &ys[0] );
   coord2screen( xwmax, ywmin, zwmax, &xs[1], &ys[1] );
   coord2screen( xwmax, ywmax, zwmax, &xs[2], &ys[2] );
   coord2screen( xwmin, ywmax, zwmax, &xs[3], &ys[3] );
   coord2screen( xwmin, ywmin, zwmin, &xs[4], &ys[4] );
   coord2screen( xwmax, ywmin, zwmin, &xs[5], &ys[5] );
   coord2screen( xwmax, ywmax, zwmin, &xs[6], &ys[6] );
   coord2screen( xwmin, ywmax, zwmin, &xs[7], &ys[7] );

   minmax1_c( xs, &ndat, xs_min, xs_max );
   minmax1_c( ys, &ndat, ys_min, ys_max );
}



static void setwindow( float xsmin,
                       float xsmax,
                       float ysmin,
                       float ysmax,
                       float aspectratio )
/*-------------------------------------------------------------*/
/* PURPOSE: Setup a PGPLOT window.                             */
/* Set up a pgplot window. Correct window for aspect ratio in  */
/* plot coordinates and device coordinates. The window can be  */
/* adjusted with the keyword WINDOW=                           */
/*-------------------------------------------------------------*/
{
   float   window_minmax[4];
   fint    nitems, dfault;
   fint    r1;
   float   x1inch, x2inch, y1inch, y2inch;
   float   xlen, ylen;
   float   scx, scy, scale;
   float   nx1 = 0.0, nx2 = 1.0, ny1 = 0.0, ny2 = 1.0;   /* Normalized dev.co.*/
   fint    units = 1;


   (void) sprintf( messbuf, "Window Xlo Ylo Xhi Yhi: [%.2f %.2f %.2f %.2f]",
                   xsmin, ysmin, xsmax, ysmax );

   window_minmax[0] = xsmin;
   window_minmax[1] = ysmin;
   window_minmax[2] = xsmax;
   window_minmax[3] = ysmax;
   nitems = 4;
   dfault = HIDDEN;
   r1     = userreal_c( window_minmax,
                        &nitems,
                        &dfault,
                        KEY_WINDOW,
                        tofchar( messbuf ) );
   cancel_c( KEY_WINDOW );
   xsmin = window_minmax[0];
   ysmin = window_minmax[1];
   xsmax = window_minmax[2];
   ysmax = window_minmax[3];

   pgsvp_c( &nx1, &nx2, &ny1, &ny2 );           /* Set viewport to entire device */
   pgqvp_c( &units,                             /* Get sizes in inches */
            &x1inch,
            &x2inch,
            &y1inch,
            &y2inch );

   xlen   = 0.7*(x2inch - x1inch);              /* Decrease sizes of viewport */
   ylen   = 0.7*(y2inch - y1inch);              /* to create space for labels */
   scx    = (xsmax - xsmin) / xlen;             /* Determine a max. scale factor to */
   scy    = aspectratio*(ysmax - ysmin) / ylen; /* scale world co. to screen co. */
   scale  = MYMAX( scx, scy );
   xlen   = xsmax - xsmin;                      /* Rescale the lengths in world co. */
   ylen   = aspectratio*(ysmax - ysmin);        /* and adjust for aspect ratio. */
   xlen  /= scale;
   ylen  /= scale;
   x1inch = (x2inch - x1inch)/2.0 - 0.5*xlen;   /* Centre the plot */
   y1inch = (y2inch - y1inch)/2.0 - 0.5*ylen;   /* Determine pos. of origin */
   x2inch = x1inch + xlen;
   y2inch = y1inch + ylen;
   pgvsiz_c( &x1inch,                           /* Set viewport in INCHES */
             &x2inch,
             &y1inch,
             &y2inch );

   /*-----------------------------------------------*/
   /* Set the window in world coordinate space      */
   /* that is to be mapped on to the viewport.      */
   /*-----------------------------------------------*/
   pgswin_c( &xsmin, &xsmax, &ysmin, &ysmax );
}



void initplot( fint xmosaic,
               fint ymosaic,
               fint askpage )
/*-------------------------------------------------------------*/
/* PURPOSE: Initialize plot software.                          */
/* Set viewport and output dimensions. If output device is a   */
/* printer, ask user for linewidth.                            */
/*-------------------------------------------------------------*/
{
   fchar   Device;                 /* Device specification */
   fchar   devtype;                /* Device specified in 'pgbeg' */
   fint    Funit;                  /* Ignored by 'pgbeg', use 0 */
   fint    nitems;                 /* Use in userxxx routines */
   fint    dfault;                 /* Use in userxxx routines */
   fint    r1;                     /* Return value or level */
   fint    len;                    /* Length of a string */
   fint    linewidth;              /* Width of lines on output device */
   fint    agreed;                 /* Loop guard */
   fint    XYsubdiv[2];            /* Number of subdivisions */


   Funit = 0;                      /* Ignored by 'pgbeg' */
   fmake( Device, 10 );
   Device = tofchar( "?" );        /*'pgbeg' will prompt the user
                                      to supply a string. */
   do
   {
      XYsubdiv[0] = xmosaic;       /* Default subdivisions in plot */
      XYsubdiv[1] = ymosaic;
      nitems = 2;
      dfault = HIDDEN;
      r1 = userint_c(  XYsubdiv,
                       &nitems,
                       &dfault,
                       KEY_MOSAIC,
                       MES_MOSAIC );
      agreed = (XYsubdiv[0] >= 1 && XYsubdiv[1] >= 1);
      if (!agreed)
         reject_c( KEY_MOSAIC, tofchar("Must both be >= 1!") );
   } while (!agreed);


   if (plotopen)                   /* Close if device was already opened.*/
      pgend_c();

   /* Set window and viewport */
   r1 = pgbeg_c( &Funit, Device, &XYsubdiv[0], &XYsubdiv[1] );
   if (r1 != 1)
      errorC( 4, "Cannot open output device" );

   plotopen = YES;

   /* NEXTPAGE= keyword */
   askpage = toflog( askpage );
   pgask_c( &askpage );

   /* Get device-type code name of the current PGPLOT device */
   /* If the destination is a printer (=every destination  */
   /* except the Tektronix device), use thick lines in the plot */

   len = 20;
   fmake(devtype, 20);
   pgqinf_c( tofchar("HARDCOPY"), devtype, &len );
   do
   {
      if (len == 3)
         /* A hardcopy */
         linewidth = 2;
      else
         linewidth = 1;

      (void) sprintf( messbuf, "Give line width (1-21):     [%d]",
                      linewidth );
      nitems = 1;
      dfault = HIDDEN;
      r1     = userint_c( &linewidth,
                          &nitems, &dfault,
                          KEY_LINEWIDTH,
                          tofchar(messbuf) );
      agreed = ((linewidth >= 1) && (linewidth <= 21));
      if (!agreed)
         reject_c( KEY_LINEWIDTH,
                   tofchar("Invalid number") );
   } while  (!agreed);
   pgslw_c( &linewidth );
}



double **dmatrix( int rows, int columns )
/*-------------------------------------------------------------*/
/* PURPOSE: Create matrix with input rows and columns.         */
/* Create pointer to array of pointers to doubles. If          */
/* allocation is impossible, free as much space as             */
/* possible. The indices of the array start with 1. Ex.:       */
/* double **A; A = dmatrix(n, m) has first element A[1][1]     */
/* and last element A[n][m]. After allocation, all elements    */
/* are set to zero. Note that the allocated space is           */
/* released by the function 'dfree'.                           */
/*-------------------------------------------------------------*/
{
   double **m;
   int    i;


   /* Allocate memory for pointers to rows */
   m = (double **) calloc( rows, sizeof(double *) );
   if (!m)
      return( NULL );
   m--;                                      /* Subscript starts at 1 */

   /* Pointer to first row allocates memory for box */
   m[1] = (double *) calloc( rows * columns, sizeof(double) );
   if (!m[1])
      return( NULL );
   m[1]--;

   /* Set pointers to rows */
   for (i = 2; i <= rows; i++)
       m[i] = m[i-1] + columns;

   /* Return pointer to array of pointers to rows */
   return( m );
}



static void dmfree( double **m, int rows )
/*-------------------------------------------------------------*/
/* PURPOSE: Free space allocated for matrix with 'dmatrix'     */
/* Restore pointer offsets before freeing allocated space.     */
/*-------------------------------------------------------------*/
{
   m[1]++;
   free( m[1] );
   m++;
   free( m );
}



double *dvector( int rows )
/*-------------------------------------------------------------*/
/* PURPOSE: Create vector with 'rows' rows.                    */
/* Create pointer to array of doubles. The indices of the      */
/* array start with 1. Ex.:                                    */
/* double *V; V = dvector(n) has first element V[1]            */
/* and last element V[n]. After allocation, all elements       */
/* are set to zero. Note that the allocated space is           */
/* released by the function 'dvfree'.                          */
/*-------------------------------------------------------------*/
{
   double *m;


   m = (double *) calloc( rows, sizeof(double) );
   if (!m)
      return( NULL );
   m--;
   return( m );
}



static void dvfree( double *m )
/*-------------------------------------------------------------*/
/* PURPOSE: Free space allocated for vector with 'dvector'     */
/* Restore pointer offsets before freeing allocated space.     */
/*-------------------------------------------------------------*/
{
   m++;                                    /* Restore pointer */
   free( m );
}



static int gaussj( double **a,
                   int      n,
                   double **b,
                   int      m )
/*-------------------------------------------------------------*/
/* PURPOSE: Linear equation solution by Gauss-Jordan elimi-    */
/* nation (numerical Recipes in C (2nd ed.) $2.1.              */
/* a[1..n][1..n] is the input matrix. b[1..n][1..m] is input   */
/* containing the m(=1) right-hand side vectors. On output a   */
/* is replaced by its matrix inverse, and b is replaced by the */
/* corresponding set of solution vectors.                      */
/* Error return codes:                                         */
/* gaussj = 0        Success                                   */
/* gaussj = -1       Memory allocation problems.               */
/* gaussj = -2, -3   Singular Matrix.                          */
/*-------------------------------------------------------------*/
{
   int       *indxc, *indxr, *ipiv;
   int       *idum = NULL;
   int       i, j, k, l, ll;
   int       icol = 0;
   int       irow = 0;
   double    big, dum, pivinv;


   /*----------------------------------------------------------*/
   /* Allocate memory for bookkeeping arrays. Decrease pointer */
   /* so that first element has index 1.                       */
   /*----------------------------------------------------------*/
   idum = (int *) calloc( n, sizeof(int) );
   if (idum)
      indxc = idum - 1;
   else
      return( -1 );
   idum = (int *) calloc( n, sizeof(int) );
   if (idum)
      indxr = idum - 1;
   else
   {
      indxc++;
      free( indxc );
      return( -1 );
   }
   idum = (int *) calloc( n, sizeof(int) );
   if (idum)
      ipiv = idum - 1;
   else
   {
      indxc++;
      free( indxc );
      indxr++;
      free( indxr );
      return( -1 );                 /* Memory allocation error */
   }
   for (j = 1; j <= n; j++)
      ipiv[j] = 0;
   for (i = 1; i <= n; i++)
   {
      big = 0.0;
      for (j = 1; j <= n; j++)
      {
         if (ipiv[j] != 1)
         {
            for (k = 1; k <= n; k++)
            {
               if (ipiv[k] == 0)
               {
                  if (fabs(a[j][k]) >= big)
                  {
                     big = fabs(a[j][k]);
                     irow = j;
                     icol = k;
                  }
               }
               else if (ipiv[k] > 1)
                     return( -2 );                       /* Singular Matrix-1 */
            }
         }
      }
      ++(ipiv[icol]);
      if (irow != icol)
      {
         for (l = 1; l <= n; l++)
            SWAP( a[irow][l], a[icol][l] );
         for (l = 1; l <= m; l++)
            SWAP( b[irow][l], b[icol][l] );
      }
      indxr[i] = irow;
      indxc[i] = icol;
      if (a[icol][icol] == 0.0)
         return( -3 );                                   /* Singular Matrix-2 */
      pivinv = 1.0 / a[icol][icol];
      a[icol][icol] = 1.0;
      for (l = 1; l <= n; l++)
         a[icol][l] *= pivinv;
      for (l = 1; l <= m; l++)
         b[icol][l] *= pivinv;
      for (ll = 1; ll <= n; ll++)
      {
         if (ll != icol)
         {
            dum = a[ll][icol];
            a[ll][icol] = 0.0;
            for (l = 1; l <= n; l++)
               a[ll][l] -= a[icol][l] * dum;
            for (l = 1; l <= m; l++)
               b[ll][l] -= b[icol][l] * dum;
         }
      }
   }
   for (l = n; l >= 1; l--)
   {
      if (indxr[l] != indxc[l])
         for (k = 1; k <= n; k++)
             SWAP( a[k][indxr[l]], a[k][indxc[l]] );
   }
   /* Free space but do not forget to raise pointer first */
   ipiv++;  free( ipiv );
   indxr++; free( indxr );
   indxc++; free( indxc );

   return( 0 );
}



static int lfit( double **M,
                 double  *y,
                 int      ndat,
                 int      nvar,
                 double  *a,
                 double  *err,
                 double  *chi2,
                 double  *redchi2 )
/*-------------------------------------------------------------*/
/* PURPOSE: Solve (M)a = y for 'a' with a least-squares method.*/
/* Input matrix M has 'ndat' rows and 'nvar' columns. The input*/
/* vector 'y' has 'ndat' elements. The output vectors 'a' and  */
/* 'err' have length 'nvar'. The vector 'err' contain the      */
/* uncertainties of the fitted values in 'a'. The routine uses */
/* chi2 minimization to fit the parameters. Numerical Recipes  */
/* in C (2nd ed.) $15.4.                                       */
/* Return error codes:                                         */
/* lfit =  0      Success.                                     */
/* lfit = -1      Memory allocation problems.                  */
/* lfit = -2, -3  Could not obtain a solution for normal       */
/*                equations.                                   */
/*-------------------------------------------------------------*/
{
   int      i, j, k, l, m;
   int      mfit;
   int      ret;
   int      step = ndat / 20;
   double   **beta;
   double   **covar;
   double   ym, wt;


   covar = dmatrix( nvar, nvar );
   if (!covar)
   {
      anyoutT( 1, "Allocation problems in lfit" );
      return(-1);
   }
   beta = dmatrix( nvar, 1 );
   if (!beta)
   {
      anyoutT( 1, "Allocation problems in lfit" );
      return(-1);
   }

   /*-------------------------------------*/
   /* Initialize the (symmetrical) matrix */
   /*-------------------------------------*/
   mfit = nvar;
   for (j = 1; j <= mfit; j++)
   {
      for(k = 1; k <= mfit; k++)
         covar[j][k] = 0.0;
      beta[j][1] = 0.0;
   }
   /*-------------------------------------------------------------------*/
   /* Loop over data to accumulate coefficients of the normal equations */
   /*-------------------------------------------------------------------*/
   for (i = 1; i <= ndat; i++)
   {
      if (!(i%step))           /* Update message ~20 times */
      {
         (void) sprintf( messbuf, 
                        "Accumulating matrix coefficients: %d%%", 
                         i*100/ndat );
         status_c( tofchar(messbuf) );
      }
      ym = y[i];      
      for (j = 0, l = 1; l <= nvar; l++)
      {
         wt = M[i][l];
         for (j++, k = 0, m = 1; m <= l; m++)
            covar[j][++k] += wt * M[i][m];
         beta[j][1] += ym * wt;
      }
   }
   /*------------------------------------------*/
   /* Fill in above the diagonal from symmetry */
   /*------------------------------------------*/
   for (j = 2; j <= mfit; j++)
   {
      for (k = 1; k < j; k++)
         covar[k][j] = covar[j][k];
   }
   /*-------------------------*/
   /* Get the matrix solution */
   /*-------------------------*/
   ret = gaussj( covar, mfit, beta, 1 );
   if (ret != 0)                                               /* No solution */
      return( ret );
   /*------------------------------------------------*/
   /* Partition solution to appropriate coefficients */
   /*------------------------------------------------*/
   for (j = 1, l = 1; l <= nvar; j++,l++)
      a[l] = beta[j][1];

   /*----------------*/
   /* Calculate chi2 */
   /*----------------*/
   *chi2 = 0.0;
   for (i = 1; i <= ndat; i++)
   {
      double   sum;
      double   diff;
      if (!(i%step))
      {
         (void) sprintf( messbuf, 
                        "Calculating Chi^2: %d%%", 
                         i*100/ndat );
         status_c( tofchar(messbuf) );
      }      
      for (sum = 0.0, j = 1; j <= nvar; j++)
         sum += M[i][j] * a[j];
      diff = y[i] - sum;
      (*chi2) += diff * diff;
   }
   *redchi2 = *chi2 / (ndat - nvar);

   /*---------------------------------------------------------*/
   /* Spread covariances back in covariance matrix in proper  */
   /* rows and columns.                                       */
   /*---------------------------------------------------------*/
/* k = mfit;
   for (j = nvar; j >= 1; j--)
   {
      for (i = 1; i <= nvar; i++) SWAP( covar[i][k], covar[i][j] );
      for (i = 1; i <= nvar; i++) SWAP( covar[k][i], covar[j][i] );
      k--;
   }
*/
   /*---------------------*/
   /* Update error vector */
   /*---------------------*/
   for (i = 1; i <= nvar; i++)
      err[i] = sqrt( covar[i][i]/(*redchi2) );

   dmfree( covar, nvar );
   dmfree( beta, nvar );
   return( 0 );
}



static void dminmaxf( double *X,
                      int     n,
                      float  *xmin,
                      float  *xmax )
/*-------------------------------------------------------------*/
/* PURPOSE: minimum and maximum value in array of doubles.     */
/* The arrays do NOT contain any blanks. Note that the return  */
/* values are of type float.                                   */
/*-------------------------------------------------------------*/
{
   int i;

   *xmax = *xmin = (float) X[0];
   for  (i = 1; i < n; i++)
   {
      if (X[i] > (double) *xmax) *xmax = (float) X[i];
      if (X[i] < (double) *xmin) *xmin = (float) X[i];
   }
}



static void getaxnames( fchar Setin,
                        char *xtitle,
                        char *ytitle,
                        char *mapunits )
/*-------------------------------------------------------------*/
/* PURPOSE: Get name of the two subset axes and units.         */
/*-------------------------------------------------------------*/
{
   fchar    Ztitle;
   fchar    Ctype, Cunit;
   fint     colev;
   fint     r1;
   int      m;


   Ztitle.a = mapunits;
   Ztitle.l = FITSLEN;
   r1 = 0;
   gdsd_rchar_c( Setin, tofchar("BUNIT"), &setlevel, Ztitle, &r1 );
   if (r1 < 0)
      strcpy( mapunits, "Z" );
   else
      mapunits[nelc_c(Ztitle)] = '\0';


   /* Get subset axis names */
   for (m = 0; m < (int) subdim; m++)
   {
      fmake( Ctype, FITSLEN );
      fmake( Cunit, FITSLEN );

      r1 = axcoord_c( Setin, &axnum[m], Ctype, Cunit, &colev );
      if (m == 0)
      {
         if (r1 != 0)
            strcpy( xtitle, "X" );
         else
            strcpy( xtitle, strtok( Ctype.a, " -" ) );
      }
      else
      {
         if (r1 != 0)
            strcpy( ytitle, "Y" );
         else
            strcpy( ytitle, strtok( Ctype.a, " -" ) );
      }
   }
}



static void putsetname( void )
/*-------------------------------------------------------------*/
/* PURPOSE: Put name of input set in plot.                     */
/* Put name of set somewhere on screen.                        */
/* The position is relative to viewport.                       */
/*-------------------------------------------------------------*/
{
   fchar   Setstr;
   fint    hidden = 2;
   float   disp, coord, fjust;
   fint    oldcolor;
   float   newheight, oldheight;

   pgqci_c( &oldcolor  );
   setcolor( WHITE );
   pgqch_c( &oldheight );
   newheight = oldheight * 2.0;
   pgsch_c( &newheight );
   fmake( Setstr, BIGSTORE );
   Setstr.l = usertext_c( Setstr, &hidden, KEY_INSET, MES_INSET );
   disp  = -1.0;
   coord = 0.5;
   fjust = 0.5;
   pgmtxt_c( tofchar("T"), &disp, &coord, &fjust, Setstr );
   pgsch_c( &oldheight );
   setcolor( oldcolor );
}



static void putid( void )
/*-------------------------------------------------------------*/
/* PURPOSE: Put user identification in plot.                   */
/* Create string with user name and date and plot it at the    */
/* left side of the (last) plot.                               */
/*-------------------------------------------------------------*/
{
   fchar     Idstr;
   float     disp, coord, fjust;
   float     newheight, oldheight;
   char      message[1024];
   fint      oldcolor;


   pgqci_c( &oldcolor  );
   setcolor( WHITE );
   pgqch_c( &oldheight );
   newheight = oldheight/1.4;
   pgsch_c( &newheight );
   fmake( Idstr, 160 );
   getusernam_c( Idstr );
   sprintf( message, "%.*s", nelc_c( Idstr ), Idstr.a );
   getdate_c( Idstr );
   sprintf( message, "%.*s %.*s", strlen(message), message,
            nelc_c( Idstr ), Idstr.a );
   disp  = -2.0;
   coord = 0.5;
   fjust = 0.5;
   pgmtxt_c( tofchar("B"), &disp, &coord, &fjust, tofchar(message) );
   pgsch_c( &oldheight );
   setcolor( oldcolor );
}


static void getvector_p( double *p,
                         double alpha,
                         double inc )
/*-------------------------------------------------------------*/
/* PURPOSE: Return normalized world coordinates of p vector.   */
/* p is a vector along the receding major axis of a galaxy.    */
/* The position angle of the major axis of a galaxy is         */
/* defined as the angle taken in anti-clockwise direction      */
/* between the north direction in the sky and the major axis   */
/* of the receding half of that galaxy (Rots 1975) astron,     */
/* astrophys 45, 43. Note that alpha = PI/2 - posang.          */
/*-------------------------------------------------------------*/
{
   p[0] =  cos(alpha);
   p[1] =  sin(alpha);
   p[2] =  0.0;
}



static void getvector_q( double *q,
                         double alpha,
                         double inc )
/*-------------------------------------------------------------*/
/* PURPOSE: Return normalized world coordinates of q vector.   */
/* q is a vector perpendicular to s and p so that q = s x p    */
/*-------------------------------------------------------------*/
{
   q[0] = -sin(alpha) * cos(inc);
   q[1] =  cos(alpha) * cos(inc);
   q[2] =               sin(inc);
}



static void getvector_s( double *s,
                         double alpha,
                         double inc )
/*-------------------------------------------------------------*/
/* PURPOSE: Return normalized world coordinates of s vector.   */
/* s is the spin vector of a ring.                             */
/*-------------------------------------------------------------*/
{
   s[0] =  sin(alpha) * sin(inc);
   s[1] = -cos(alpha) * sin(inc);
   s[2] =               cos(inc);
}



static double dotprod( double *v1,
                       double *v2 )
/*-------------------------------------------------------------*/
/* PURPOSE: Calculate the dot product of two vectors v1, v2    */
/*-------------------------------------------------------------*/
{
   return( v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2] );
}



static float textangle( float x2,
                        float y2,
                        float x1,
                        float y1,
                        float *fjust )
/*-------------------------------------------------------------*/
/* PURPOSE: Determine angle for text along an axis.            */
/* Adjust angles for the axis titles along the x- &y axis.     */
/*-------------------------------------------------------------*/
{
   double  angle;

   angle = TODEG(atan2( y2-y1, x2-x1 ));
   *fjust = 1.0;
   if (angle > 90.0)
   {
      angle -= 180.0;
      *fjust = 0.0;
   }
   else if (angle < -90.0)
   {
      angle += 180.0;
      *fjust = 0.0;
   }
   return( (float) angle );
}



static void drawaxes( float xsmin,
                      float xsmax,
                      float ysmin,
                      float ysmax,
                      float xwmin,
                      float xwmax,
                      float ywmin,
                      float ywmax,
                      float zwmin,
                      float zwmax,
                      char *xtitle,
                      char *ytitle,
                      char *mapunits )
/*-------------------------------------------------------------*/
/* PURPOSE: Draw a coordinate frame using a viewing            */
/*          transformation.                                    */
/*-------------------------------------------------------------*/
{
   float   xend, yend, xor, yor;
   fint    linewidth;
   fint    oldcolor;
   float   angle;
   float   fjust;
   fint    LWINC = 3;

   /*-----------------------------------------------*/
   /* Advance plotter to a new (sub-)page,          */
   /* clearing the screen if necessary.             */
   /*-----------------------------------------------*/
   pgpage_c();

   setwindow( xsmin, xsmax, ysmin, ysmax, 1.0 );
   pgqci_c( &oldcolor );
   setcolor( YELLOW );

   /* Plot axes */
   pgqlw_c( &linewidth );                      /* Get original linewidth */
   linewidth += LWINC;                         /* Make it bigger for the axes */
   pgslw_c( &linewidth );

   /* Origin */
   coord2screen( 0.0, 0.0, 0.0, &xor, &yor );

   /* X-axis */
   coord2screen( xwmin, 0.0, 0.0, &xend, &yend );
   movexy( xend, yend );
   coord2screen( xwmax, 0.0, 0.0, &xend, &yend );
   drawxy( xend, yend );
   angle = textangle( xend, yend, xor, yor, &fjust );
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar(xtitle) );

   /* Y-axis */
   coord2screen( 0.0, ywmin, 0.0, &xend, &yend );
   movexy( xend, yend );
   coord2screen( 0.0, ywmax, 0.0, &xend, &yend );
   drawxy( xend, yend );
   angle = textangle( xend, yend, xor, yor, &fjust );
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar(ytitle) );

   /* Z-axis */
   coord2screen( 0.0, 0.0, zwmin, &xend, &yend );
   movexy( xend, yend );
   coord2screen( 0.0, 0.0, zwmax, &xend, &yend );
   drawxy( xend, yend );
   angle = 0.0;
   fjust = 0.5;
   yend = yend + (yend-yor)/10.0;
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar(mapunits) );

   /* Write characteristics in log */
   anyoutC( 1, " ");
   anyoutC( 1, "         ----- Viewing parameters -------" );
   anyoutf( 1, " theta (xy plane) = %.2f phi (z) = %.2f rho = %.2f",
                 theta, phi, rho );
   anyoutf( 1, " Distance eye to screen = %.2f", d_eye );

   linewidth -= LWINC;                       /* Reset width to original value */
   pgslw_c( &linewidth );
   setcolor( oldcolor );
}



static void plotpqs( double alp,
                     double inc )
/*-------------------------------------------------------------*/
/* PURPOSE: Plot spin vectors s,p,q of ring.                   */
/*-------------------------------------------------------------*/
{
   double  sXYZ[3];
   double  pXYZ[3];
   double  qXYZ[3];
   float   xor, yor, xend, yend;
   float   angle, fjust;
   fint    linewidth;
   fint    LWINC = 3;


   pgqlw_c( &linewidth );                      /* Get original linewidth */
   linewidth += LWINC;                         /* Make it bigger for the axes */
   pgslw_c( &linewidth );

   getvector_s( sXYZ, alp, inc );

   setcolor( RED );

   /* Origin */
   coord2screen( 0.0, 0.0, 0.0, &xor, &yor );

   movexy( xor, yor );
   coord2screen( sXYZ[0], sXYZ[1], sXYZ[2], &xend, &yend );
   drawxy( xend, yend );
   angle = 0.0;
   fjust = 0.5;
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar("s0") );

   getvector_p( pXYZ, alp, inc );

   movexy( xor, yor );
   coord2screen( pXYZ[0], pXYZ[1], pXYZ[2], &xend, &yend );
   drawxy( xend, yend );
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar("p0") );

   getvector_q( qXYZ, alp, inc );

   movexy( xor, yor );
   coord2screen( qXYZ[0], qXYZ[1], qXYZ[2], &xend, &yend );
   drawxy( xend, yend );
   pgptxt_c( &xend, &yend, &angle, &fjust, tofchar("q0") );

   linewidth -= LWINC;                         /* Restore linewidth */
   pgslw_c( &linewidth );
}


#ifdef qqqqq
static void XYZ2xyz( double X, double Y, double Z,
                     double alpha, double inclination,
                     double *x, double *y, double *z )
/*-------------------------------------------------------------*/
/* PURPOSE: Transform coordinate in world system to a          */
/*          coordinate in the ring system (with a,i).          */
/* The goniometric functions are recalculated if one of the    */
/* angles changes.                                             */
/*-------------------------------------------------------------*/
{
   static double  alp = 0.0;    /* static is essential here */
   static double  inc = 0.0;
   static double  cosA = 1.0;
   static double  sinA = 0.0;
   static double  cosI = 1.0;
   static double  sinI = 0.0;


   if (alpha != alp)
   {
      alp = alpha;
      cosA = cos( alp );
      sinA = sin( alp );
   }

   if (inclination != inc)
   {
      inc = inclination;
      cosI = cos( inc );
      sinI = sin( inc );
   }

   *x =  X*cosA      + Y*sinA;
   *y = -X*sinA*cosI + Y*cosA*cosI + Z*sinI;
   *z =  X*sinA*sinI - Y*cosA*sinI + Z*cosI;
}
#endif


static void xyz2XYZ( double x,
                     double y,
                     double z,
                     double alpha,
                     double inclination,
                     double *X,
                     double *Y,
                     double *Z )
/*-------------------------------------------------------------*/
/* PURPOSE: Transform coordinate in ring system xyz with a,i   */
/*          to a coordinate in the world system XYZ.           */
/* Alpha and inclination are both in radians!                  */
/* The goniometric functions are recalculated if one of the    */
/* angles changes.                                             */
/*-------------------------------------------------------------*/
{
   static double  alp = 0.0;               /* static is essential here */
   static double  inc = 0.0;
   static double  cosA = 1.0;
   static double  sinA = 0.0;
   static double  cosI = 1.0;
   static double  sinI = 0.0;


   if (alpha != alp)
   {
      alp = alpha;
      cosA = cos( alp );
      sinA = sin( alp );
   }

   if (inclination != inc)
   {
      inc = inclination;
      cosI = cos( inc );
      sinI = sin( inc );
   }

   *X = x*cosA - y*sinA*cosI + z*sinA*sinI;
   *Y = x*sinA + y*cosA*cosI - z*cosA*sinI;
   *Z =          y*sinI      + z*cosI;
}



static void xyz2XY( double x,
                    double y,
                    double z,
                    double alpha,
                    double inclination,
                    double *X,
                    double *Y )
/*-------------------------------------------------------------*/
/* PURPOSE: Transform coordinate in ring system xyz with a,i   */
/*          to a coordinate in the XY plane (sky).             */
/* Alpha and inclination are both in radians!                  */
/* The goniometric functions are recalculated if one of the    */
/* angles changes.                                             */
/*-------------------------------------------------------------*/
{
   static double  alp = 0.0;
   static double  inc = 0.0;
   static double  cosA = 1.0;
   static double  sinA = 0.0;
   static double  cosI = 1.0;
   static double  sinI = 0.0;


   if (alpha != alp)
   {
      alp = alpha;
      cosA = cos( alp );
      sinA = sin( alp );
   }

   if (inclination != inc)
   {
      inc = inclination;
      cosI = cos( inc );
      sinI = sin( inc );
   }

   *X = x*cosA - y*sinA*cosI + z*sinA*sinI;
   *Y = x*sinA + y*cosA*cosI - z*cosA*sinI;
}



static int ringpos2pixindex( int     ring,
                             fint   *blo,
                             fint   *bhi,
                             double *gridspacing,
                             double *cpos,
                             double  x,
                             double  y,
                             double  z )
/*-------------------------------------------------------------*/
/* PURPOSE: Convert a position in a ring to an array index.    */
/* A ring is characterized by radius r, inclination i and      */
/* position angle a. Given a position xyz in the plane of      */
/* ring with index 'ring', determine a 2-dim position in the   */
/* XY plane (sky) and convert this position to a pixel index.  */
/* The pixel index is 0 for the lower left pixel (blo) and     */
/* Yp-blo[1]) * Xlen + Xp-blo[0] for a pixel at Xp, Yp.        */
/*-------------------------------------------------------------*/
{
   double      alpha = posangs[ring];
   double      inclination = inclinations[ring];
   double      X, Y, X2, Y2;                       /* The sky position */
   int         Xp, Yp;                             /* The pixel position */
   int         Xlen = bhi[0] - blo[0] + 1;         /* Length of X axis in box */


   xyz2XY( x, y, z, alpha, inclination, &X, &Y );  /* Position in the sky */

   X2 = X/gridspacing[0] + cpos[0];                /* Position in pixels corrected */
   Y2 = Y/gridspacing[1] + cpos[1];                /* for shift in origin */

   if (X2 < 0.0)
      Xp = (int) (X2 - 0.5);
   else
      Xp = (int) (X2 + 0.5);
   if (Y2 < 0.0)
      Yp = (int) (Y2 - 0.5);
   else
      Yp = (int) (Y2 + 0.5);
   return( (Yp-blo[1]) * Xlen + Xp-blo[0] );       /* Return pixel index */
}



static void drawring( double lorad,
                      double hirad,
                      double alpha,
                      double inc )
/*-------------------------------------------------------------*/
/* PURPOSE: Plot a ring with given a,i in perspective view.    */
/* Calculate coordinates of a circle in the xyz system of the  */
/* ring and convert the coordinates to XYZ world coordinates.  */
/*-------------------------------------------------------------*/
{
   double   i;
   double   x, y, z;
   double   X, Y, Z;
   double   ang;
   double   radius;
   fint     j = 0;
   int      m;
   float    cxup[182], cyup[182];
   float    xs, ys;
   fint     color = GREEN;


   /*----------------------------------------------------------*/
   /* Note that rings are defined in the xy plane so z = 0.    */
   /*----------------------------------------------------------*/
   for (m = 0; m < 2; m++)
   {
      if (m == 0)
      {
         radius = lorad;
         color = GREEN;
      }
      else
      {
         radius = hirad;
         color = CYAN;
      }
      setcolor( color );
      z = 0.00;
      for (i = 0.0, j = 0; i <= 360.0; i += 2.0, j++)
      {
         ang = TORAD(i);
         x = radius * cos( ang );
         y = radius * sin( ang );
         xyz2XYZ( x, y, z, alpha, inc, &X, &Y, &Z );
         coord2screen( X, Y, Z, &xs, &ys );
         cxup[j] = xs;
         cyup[j] = ys;
      }
      pgline_c( &j, cxup, cyup );
   }
}



static void plotvector( double alpha,
                        double inclination,
                        char   vector,
                        fint   color )
/*-------------------------------------------------------------*/
/* PURPOSE: Plot a unit vector in perpective view.             */
/* Input of vector is only one of the characters 'p', 'q',     */
/* or 's'.                                                     */
/*-------------------------------------------------------------*/
{
   double   v[3];
   float    xor, yor;
   float    xs, ys;
   fint     linewidth;
   fint     LWINC = 1;


   pgqlw_c( &linewidth );                      /* Get original linewidth */
   linewidth += LWINC;                         /* Make it bigger for the axes */
   pgslw_c( &linewidth );

   coord2screen( 0.0, 0.0, 0.0, &xor, &yor );  /* Origin */
   setcolor( color );
   if (vector == 'p')
      getvector_p( v, alpha, inclination );
   if (vector == 'q')
      getvector_q( v, alpha, inclination );
   if (vector == 's')
      getvector_s( v, alpha, inclination );
   movexy( xor, yor );
   coord2screen( v[0], v[1], v[2], &xs, &ys );
   drawxy( xs, ys );

   linewidth -= LWINC;                         /* Restore linewidth */
   pgslw_c( &linewidth );
}



static void plotSxSy( fint   nrings,
                      double *alpha,
                      double *inclination )
/*-------------------------------------------------------------*/
/* PURPOSE: Plot orientation of spin vector.                   */
/* For an overview of the tilted ring, we plot sx, sy of       */
/* vector k in the xy system of the vector with k=0.           */
/*-------------------------------------------------------------*/
{
   int      k;
   double   a0 = alpha[0];
   double   i0 = inclination[0];
   float    x0 = 0.0, y0 = 0.0;
   float    *sx = NULL;
   float    *sy = NULL;
   float    tick = 0.0;
   fint     nsub = 0;
   float    fjust, angle;
   float    xs, ys;
   fint     ndat;
   float    xmin, xmax, ymin, ymax;
   float    oldheight, newheight;
   fint     linewidth;


   sx = (float *) calloc( nrings, sizeof(float) );
   sy = (float *) calloc( nrings, sizeof(float) );
   if (!sx || !sy)
   {
      anyoutC( 1, "Memory allocation problems!" );
      return;
   }

   for (k = 1; k < nrings; k++)               /* Exclude vector k = 0 */
   {
      double    ak = alpha[k];
      double    ik = inclination[k];

      /* Formula (7) in draft of manual */
      sx[k] = (float) (sin(ik)*(sin(ak)*cos(a0) - cos(ak)*sin(a0)));
      sy[k] = (float) (-sin(ik)*cos(i0)*(sin(ak)*sin(a0) + cos(ak)*cos(a0))
                                       + cos(ik)*sin(i0));
   }
   ndat = nrings - 1;

   minmax1_c( &sx[1], &ndat, &xmin, &xmax );  /* Exclude index 0 */
   minmax1_c( &sy[1], &ndat, &ymin, &ymax );

   setcolor( WHITE );
   pgpage_c();
   pgqch_c( &oldheight );
   newheight = 1.5;
   pgsch_c( &newheight );
   xmax = MYMAX( ABS(xmax), ABS(xmin) );
   ymax = MYMAX( ABS(ymax), ABS(ymin) );
   xmax = MYMAX( xmax, ymax );
   if (xmax == 0.0)
      xmax  = 1.0;
   else
      xmax *= 1.1;                            /* Increase size for better layout */
   setwindow( -xmax, xmax, -xmax, xmax, 1.0 );
   pgbox_c( tofchar("BCNST"), &tick, &nsub,
            tofchar("BCNSTV"), &tick, &nsub );
   setlabel( "X (k=0)", "Y (k=0)",
             "ORIENTATION OF SPINVECTORS(\\ga,i) WRT. CENTRAL DISK" );
   setcolor( WHITE );
   xs = -xmax; ys = 0.0;
   movexy( xs, ys );
   xs = xmax;
   drawxy( xs, ys );
   fjust = 1.0;
   angle = 0.0;
   setcolor( YELLOW );
   pgptxt_c( &xs, &ys, &angle, &fjust, tofchar("Sx") );

   setcolor( WHITE );
   xs = 0.0; ys = -xmax;
   movexy( xs, ys );
   ys = xmax;
   drawxy( xs, ys );
   fjust = 0.5;
   setcolor( YELLOW );
   pgptxt_c( &xs, &ys, &angle, &fjust, tofchar("Sy") );

   setcolor( YELLOW );
   for (k = 1; k < nrings; k++)
   {
      movexy( x0, y0 );
      drawxy( sx[k], sy[k] );
   }
   pgqlw_c( &linewidth );                  /* Get original linewidth */
   linewidth += 4;                         /* Make it bigger for the connections */
   pgslw_c( &linewidth );
   setcolor( RED );
   pgline_c( &ndat, &sx[1], &sy[1] );
   pgsch_c( &oldheight );                  /* Restore char. height */
   linewidth -= 2;
   pgslw_c( &linewidth );                  /* Restore line width */

   free( sx );
   free( sy );
}



static void plotintersections( fint   nrings,
                               double *alpha,
                               double *inclination )
/*-------------------------------------------------------------*/
/* PURPOSE: Plot intersection of rings with central ring.      */
/* For an overview of the tilted ring, we plot the intersection*/
/* of ring k with the central disk.                            */
/*-------------------------------------------------------------*/
{
   fint     k;
   float    tick = 0.0;
   fint     nsub = 0;
   float    fjust, angle;
   float    xs, ys;
   float    xmin, xmax, ymin, ymax;
   double   sXYZ[3];
   double   pXYZ[3];
   double   qXYZ[3];
   double   ii;
   float    xc[181], yc[181];
   float    oldheight, newheight;


   setcolor( WHITE );
   pgpage_c();

   xmin = -1.3; xmax = 1.3;
   ymin = -1.3; ymax = 1.3;
   setwindow( -xmax, xmax, -xmax, xmax, 1.0 );
   pgqch_c( &oldheight );
   newheight = 1.5;
   pgsch_c( &newheight );
   pgbox_c( tofchar("BCNST"), &tick, &nsub,
            tofchar("BCNSTV"), &tick, &nsub );
   setlabel( "X (k=0)", "Y (k=0)", "INTERSECTIONS WITH CENTRAL DISK" );
   setcolor( WHITE );
   xs = -xmax; ys = 0.0;
   movexy( xs, ys );
   xs = xmax;
   drawxy( xs, ys );
   fjust = 0.0;
   angle = 0.0;
   setcolor( YELLOW );
   pgptxt_c( &xs, &ys, &angle, &fjust, tofchar("p0") );

   setcolor( WHITE );
   xs = 0.0; ys = -xmax;
   movexy( xs, ys );
   ys = xmax;
   drawxy( xs, ys );
   fjust = 0.5;
   setcolor( YELLOW );
   pgptxt_c( &xs, &ys, &angle, &fjust, tofchar("q0") );

   setcolor( RED );
   for (ii = 0.0, k = 0; ii <= 360.0; ii += 2.0, k++)
   {
      double ang;

      ang = TORAD(ii);
      xc[k] = (float) cos( ang );
      yc[k] = (float) sin( ang );
   }
   pgline_c( &k, xc, yc );


   setcolor( YELLOW );
   getvector_p( pXYZ, alpha[0], inclination[0] );
   getvector_q( qXYZ, alpha[0], inclination[0] );

   for (k = 1; k < nrings; k++)
   {
      double    prodps, prodqs;
      double    beta;
      double    x, y;

      /* Formula explained in draft of manual */
      getvector_s( sXYZ, alpha[k], inclination[k] );
      prodps = dotprod( pXYZ, sXYZ );
      prodqs = dotprod( qXYZ, sXYZ );
      if (prodps == 0.0 && prodqs == 0.0)
         beta = 0.0;
      else if (prodqs == 0.0)
         beta = PI / 2.0;
      else
         beta = atan2( -prodps, prodqs );

      x = (float) cos( beta );
      y = (float) sin( beta );

      movexy( -x, -y );
      drawxy( x, y );
   }
   pgsch_c( &oldheight );
}



static void plotradvsposang( double *lorad,
                             double *hirad,
                             double *posangs,
                             double *inclinations,
                             fint   nrings )
/*-------------------------------------------------------------*/
/* PURPOSE: Plot radius vs. position angle and radius vs.      */
/*          inclination.                                       */
/*-------------------------------------------------------------*/
{
   float    xsmin;
   float    xsmax;
   float    ysmin;
   float    ysmax;
   float    asprat;
   float    tick = 0.0;
   float    oldheight, newheight;
   fint     nsub = 0;
   int      i;
   double   delta;


   dminmaxf( hirad, nrings, &xsmin, &xsmax );
   xsmin  = 0.0;                                 /* Always start at 0 */
   xsmax *= 1.1;                                 /* Increase max. for space */
   dminmaxf( posangs, nrings, &ysmin, &ysmax );
   delta = (ysmax - ysmin) / 10.0;
   if (delta == 0.0)
   {
      ysmin -= 1.0;
      ysmax += 1.0;
   }
   else
   {
      ysmin -= delta;
      ysmax += delta;
   }
   pgqch_c( &oldheight );
   newheight = 1.8;
   pgsch_c( &newheight );
   pgpage_c();
   asprat = 0.5 * (xsmax - xsmin)/(ysmax - ysmin);
   setwindow( xsmin, xsmax, ysmin, ysmax, asprat );
   setcolor( 1 );
   pgbox_c( tofchar("BCNST"), &tick, &nsub,
            tofchar("BCNSTV"), &tick, &nsub );
   setlabel( "Radius (arcsec)", "Position angle (degrees)",
             "POSITION ANGLE VS. RADIUS" );

   /*---------------------------------------------------*/
   /* Plot a line from lorad to hirad and put marker on */
   /* position (hirad, surfdens).                       */
   /*---------------------------------------------------*/
   for (i = 0; i < nrings; i++)
   {
       float  xx, yy;
       xx = lorad[i];
       yy = posangs[i];
       movexy( xx, yy );
       xx = hirad[i];
       setcolor( YELLOW );
       drawxy( xx, yy );
       setcolor( RED );
       setmarker( xx, yy, 4 );
   }
   dminmaxf( inclinations, nrings, &ysmin, &ysmax );
   delta = (ysmax - ysmin) / 10.0;
   if (delta == 0.0)
   {
      ysmin -= 1.0;
      ysmax += 1.0;
   }
   else
   {
      ysmin -= delta;
      ysmax += delta;
   }
   pgpage_c();
   asprat = 0.5 * (xsmax - xsmin)/(ysmax - ysmin);
   setwindow( xsmin, xsmax, ysmin, ysmax, asprat );
   setcolor( 1 );
   pgbox_c( tofchar("BCNST"), &tick, &nsub,
            tofchar("BCNSTV"), &tick, &nsub );
   setlabel( "Radius (arcsec)", "Inclination (degrees)",
             "INCLINATION VS. RADIUS" );

   /*---------------------------------------------------*/
   /* Plot a line from lorad to hirad and put marker on */
   /* position (hirad, surfdens).                       */
   /*---------------------------------------------------*/
   for (i = 0; i < nrings; i++)
   {
      float  xx, yy;
      xx = lorad[i];
      yy = inclinations[i];
      movexy( xx, yy );
      xx = hirad[i];
      setcolor( YELLOW );
      drawxy( xx, yy );
      setcolor( RED );
      setmarker( xx, yy, 4 );
   }
   pgsch_c( &oldheight );
}



static void plotdensvsradius( double *lorad,
                              double *hirad,
                              double *surfdens,
                              fint    nrings,
                              fint    nsegs,
                              char   *mapunits )
/*-------------------------------------------------------------*/
/* PURPOSE: Plot the fitted densities against radius.          */
/*-------------------------------------------------------------*/
{
   float    xsmin;
   float    xsmax;
   float    ysmin;
   float    ysmax;
   float    asprat;
   float    tick = 0.0;
   float    *compvals = NULL;
   fint     nsub = 0;
   fint     k;
   fint     dfault;
   int      i, j;
   int      segbase;
   double   delta;


   /*------------------------------------------------------*/
   /* Note that only the surfdens arrays are 'nsegs' long. */
   /* Others have length 'nrings'.                         */
   /*------------------------------------------------------*/
   dminmaxf( hirad, nrings, &xsmin, &xsmax );
   xsmin  = 0.0;                                 /* Always start at 0 */
   xsmax *= 1.1;                                 /* Increase max. for space */
   dminmaxf( surfdens, nsegs, &ysmin, &ysmax );
   delta = (ysmax - ysmin) / 10.0;
   if (delta == 0.0)
   {
      ysmin -= 1.0;
      ysmax += 1.0;
   }
   else
   {
      ysmin -= delta;
      ysmax += delta;
   }
   asprat = (xsmax - xsmin)/(ysmax - ysmin);
   setwindow( xsmin, xsmax, ysmin, ysmax, asprat );
   pgbox_c( tofchar("BCNST"), &tick, &nsub,
            tofchar("BCNSTV"), &tick, &nsub );
   (void) sprintf( messbuf, "Fitted ring parameter in %s", mapunits );
   setlabel( "Radius (arcsec)", messbuf, "RESULTS OF THE FIT" );

   /*---------------------------------------------------*/
   /* Plot a line from lorad to hirad and put marker on */
   /* position (hirad, surfdens).                       */
   /*---------------------------------------------------*/
   segbase = 0;
   for (i = 0; i < nrings; i++)
   {
      for (j = 0; j < segments[i]; j++)
      {
         float  xx, yy;
         xx = lorad[i];
         yy = surfdens[segbase+j];
         movexy( xx, yy );
         xx = hirad[i];
         setcolor( YELLOW );
         drawxy( xx, yy );
         setcolor( RED );
         setmarker( xx, yy, 4 );
      }
      segbase += segments[i];
   }
   /*---------------------------------------------------------*/
   /* If user wants to compare this plot with some input data */
   /* get this data with COMPVALS= keyword and scale it with  */
   /* SCALE=. Plot the points in a different colour.          */
   /*---------------------------------------------------------*/
   compvals = (float *) calloc( nrings, sizeof(float) );
   if (!compvals)
   {
      anyoutC( 1, "Memory allocation problems!" );
      return;
   }
   dfault = REQUEST;
   k = userreal_c( compvals,
                   &nrings,
                   &dfault,
                   KEY_COMPVALS,
                   tofchar("Values to include in plot:     [None]") );

   for (i = 0; i < k; i++)
   {
      float  xx, yy;
      xx = lorad[i];
      yy = compvals[i];
      if (i == 0)
         movexy( xx, yy );
      else
         drawxy( xx, yy );
      xx = hirad[i];
      setcolor( GREEN );
      drawxy( xx, yy );
      setcolor( GREEN );
      setmarker( xx, yy, 3 );
   }
   free( compvals );
}



static void plottable( fchar Settab, fchar Tname )
/*------------------------------------------------------------*/
/* PURPOSE: Display table info obtained in previous run.      */
/*------------------------------------------------------------*/
{
   fchar    Cunits, Ctype, Ccom;
   fint     err;
   fint     nrows;
   fint     one = 1;
   fint     nitems, dfault;
   fint     r1;
   float    xsmin;
   float    xsmax;
   float    ysmin;
   float    ysmax;
   float    asprat;
   float    tick = 0.0;
   fint     nsub = 0;
   int      j;
   fint     *ringnr = NULL;
   fint     *segnr  = NULL;
   fint     *segments = NULL;
   fint     plotring[2];
   double   delta;


   fmake( Cunits, ITEMLEN );
   fmake( Ccom,   1024 );
   fmake( Ctype,  ITEMLEN );
   gdsa_colinq_c( Settab,             /* INFO  about columns in a GDS table. */
                  &setlevel,
                  Tname,
                  COL_SURFDENS,
                  Ctype,
                  Ccom,
                  Cunits,
                  &nrows,
                  &err );

   /* Any non-negative number indicates a successful */
   /* completion of the operation. Negative values   */
   /* indicate that an error has occured. Colums are */
   /* written on top level so the return value must  */
   /* be 0.                                          */

   if (err != 0)
   {
      (void) sprintf( messbuf, "Error (nr %d) reading column info!", err );
      anyoutT( 1, messbuf );
      return;
   }
   (void) sprintf( messbuf, "Reading table data from %.*s",
                   nelc_c(Tname), Tname.a );
   anyoutT( 3, messbuf );
   (void) sprintf( messbuf, "Column length: %d. Units: %.*s",
                   nrows, nelc_c(Cunits), Cunits.a );
   anyoutT( 3, messbuf );
   surfdens = (double *) calloc( nrows,  sizeof(double) );
   hirad    = (double *) calloc( nrows,  sizeof(double) );
   lorad    = (double *) calloc( nrows,  sizeof(double) );
   ringnr   = (fint *)   calloc( nrows,  sizeof(fint)   );
   segnr    = (fint *)   calloc( nrows,  sizeof(fint)   );
   segments = (fint *)   calloc( nrows,  sizeof(fint)   );
   if (!hirad || !lorad || !surfdens || !ringnr || !segnr || !segments)
   {
      anyoutT( 1, "Could not allocate memory for 'table' arrays!" );
      return;
   }
   gdsa_rcdble_c( Settab, &setlevel, Tname, COL_SURFDENS,
                  surfdens, &one, &nrows, &err );
   if (err < 0)
   {
      (void) sprintf( messbuf,
                     "Error (nr %d) reading data from density column!",
                      err );
      anyoutT( 1, messbuf );
      return;
   }
   gdsa_rcdble_c( Settab, &setlevel, Tname, COL_HIRAD,
                  hirad, &one, &nrows, &err );
   if (err < 0)
   {
      (void) sprintf( messbuf,
                     "Error (nr %d) reading data from 'hirad' column!",
                      err );
      anyoutT( 1, messbuf );
      return;
   }
   gdsa_rcdble_c( Settab, &setlevel, Tname, COL_LORAD,
                  lorad, &one, &nrows, &err );
   if (err < 0)
   {
      (void) sprintf( messbuf,
                     "Error (nr %d) reading data from 'lorad' column!",
                      err );
      anyoutT( 1, messbuf );
      return;
   }
   gdsa_rcint_c( Settab, &setlevel, Tname, COL_RINGNR,
                 ringnr, &one, &nrows, &err );
   if (err < 0)
   {
      (void) sprintf( messbuf,
                     "Error (nr %d) reading data from 'ringnr' column!",
                      err );
      anyoutT( 1, messbuf );
      return;
   }
   gdsa_rcint_c( Settab, &setlevel, Tname, COL_SEGNR,
                 segnr, &one, &nrows, &err );
   if (err < 0)
   {
      (void) sprintf( messbuf,
                     "Error (nr %d) reading data from 'segnr' column!",
                      err );
      anyoutT( 1, messbuf );
      return;
   }
   gdsa_rcint_c( Settab, &setlevel, Tname, COL_SEGMENTS,
                 segments, &one, &nrows, &err );
   if (err < 0)
   {
      (void) sprintf( messbuf,
                     "Error (nr %d) reading data from 'segments' column!",
                      err );
      anyoutT( 1, messbuf );
      return;
   }

   initplot( 1, 1, NO );
   dminmaxf( hirad, nrows, &xsmin, &xsmax );
   xsmin  = 0.0;                                 /* Always start at 0 */
   xsmax *= 1.1;                                 /* Increase max. for space */
   dminmaxf( surfdens, nrows, &ysmin, &ysmax );
   delta = (ysmax - ysmin) / 10.0;
   if (delta == 0.0)
   {
      ysmin -= 1.0;
      ysmax += 1.0;
   }
   else
   {
      ysmin -= delta;
      ysmax += delta;
   }
   asprat = (xsmax - xsmin)/(ysmax - ysmin);
   setwindow( xsmin, xsmax, ysmin, ysmax, asprat );
   pgbox_c( tofchar("BCNST"), &tick, &nsub,
            tofchar("BCNSTV"), &tick, &nsub );
   (void) sprintf( messbuf, "Fitted ring parameter in %.*s",
                   nelc_c(Cunits), Cunits.a );
   setlabel( "Radius (arcsec)",
              messbuf,
             "RESULTS OF THE FIT, AS FUNCTION OF RADIUS" );
   for (j = 0; j < nrows; j++)
   {
      float  xx, yy;
      xx = lorad[j];
      yy = surfdens[j];
      movexy( xx, yy );
      xx = hirad[j];
      setcolor( YELLOW );
      drawxy( xx, yy );
      setcolor( RED );
      setmarker( xx, yy, 4 );
   }
   nitems = 2;
   dfault = REQUEST;
   do
   {
      (void) sprintf( messbuf,
                     "Ring number, max.segments:      [stop]" );
      r1 = userint_c( plotring, &nitems, &dfault,
                      KEY_PLOTSEG,
                      tofchar( messbuf ) );
      cancel_c( KEY_PLOTSEG );

      if (r1)
      {
         pgpage_c();
         xsmin = 1.0;
         if (r1 == 1)
         {
            /* What is the max. num. of segments in this ring? */
            for (j = 0; j < nrows; j++)
               if (ringnr[j] == plotring[0])
               {
                  plotring[1] = segments[j];
                  break;
               }
         }
         plotring[1] = MYMAX( plotring[1], 2.0);
         xsmax = (float) plotring[1];
         asprat = (xsmax - xsmin)/(ysmax - ysmin);
         setwindow( xsmin, xsmax, ysmin, ysmax, asprat );
         setcolor( WHITE );
         pgbox_c( tofchar("BCNST"), &tick, &nsub,
                  tofchar("BCNSTV"), &tick, &nsub );
         {
            char   mess2[BIGSTORE];
            char   mess1[BIGSTORE];
            (void) sprintf( mess1,
                           "Fitted ring parameter in %.*s",
                            nelc_c(Cunits), Cunits.a );
            (void) sprintf( mess2,
                           "RESULTS FOR %d SEGMENTS IN RING %d",
                            plotring[1], plotring[0] );
            setlabel( "Segment number",
                       mess1, mess2 );
         }
         for (j = 0; j < nrows; j++)
         {
            float  xx, yy;
            if (ringnr[j] == plotring[0])
            {
               xx = segnr[j];
               yy = surfdens[j];
               movexy( xx, yy );
               setcolor( YELLOW );
               setmarker( xx, yy, 4 );
            }
         }
      }
   } while (r1);

   if (surfdens)  free( surfdens );
   if (hirad)     free( hirad );
   if (lorad)     free( lorad );
   if (ringnr)    free( ringnr );
   if (segnr)     free( segnr );
   if (segments)  free( segments );
}



static void logtable0( fint   nrings,
                       fint   nsegs,
                       double *cpos )
/*------------------------------------------------------------*/
/* PURPOSE: Write a table with common characteristics.        */
/* Central position, number of rings.                         */
/*------------------------------------------------------------*/
{
   int     dev = 3;


   anyoutC( dev, " RING INFO:" );
   anyoutf( dev, "-Selected central position (grids): %g %g",
            cpos[0], cpos[1] );
   anyoutf( dev, "-Number of rings: %d", nrings );
   anyoutf( dev, "-Total number of segments: %d", nsegs );
   anyoutC( dev, " " );
}



static void logtable1( fint   nrings,
                       fint   *segments,
                       double *lorad,
                       double *hirad,
                       double *posangs,
                       double *inclinations,
                       double *Z0 )
/*-------------------------------------------------------------*/
/* PURPOSE: Write a table with ring characteristics.           */
/* Characteristics must be known before the fit.               */
/*-------------------------------------------------------------*/
{
   int     i;
   int     len;
   int     dev = 3;
   char    *border;


   len = sprintf( messbuf,
                  " %4s | %4s | %8s | %8s | %8s | %6s | %6s | %6s |",
                  "ring", "segm", "Rlo", "Rhi", "width", "posang", "inclin", "Z0" );

   border = malloc( len+1 );
   if (!border)
   {
      anyoutT( 1, "Not enough memory to produce a table!" );
      return;
   }

   memset( border, '=', len );
   border[len] = '\0';
   anyoutC( dev, border );
   anyoutC( dev, messbuf );
   anyoutC( dev, border );
   for (i = 0; i < nrings; i++)
   {
      (void) sprintf( messbuf,
                     " %4d | %4d | %8.2f | %8.2f | %8.2f | %6.2f | %6.2f | %6.2f |",
                      i+1,
                      segments[i],
                      lorad[i],
                      hirad[i],
                      hirad[i]-lorad[i],
                      posangs[i],
                      inclinations[i],
                      Z0[i] );
      anyoutC( dev, messbuf );
   }
   anyoutC( dev, border );
   free( border );
}



static int fieldformat( double *x,
                        double *ex,
                        int     n,
                        int    *field,
                        int    *prec )
/*------------------------------------------------------------*/
/* PURPOSE: Determine print format field width and precision. */
/* The input array ex is the array of errors in the elements  */
/* of the array x that we want to print. Both arrays do not   */
/* contain blanks.                                            */
/*------------------------------------------------------------*/
{
   int       i;
   double    xmax;
   double    exmin;


   *field = *prec = 0;          /* Initialize */
   xmax = fabs( x[0] );         /* Determine biggest of abs values in x */
   for  (i = 1; i < n; i++)
   {
      double   val = fabs( x[i] );
      if (val > xmax)
         xmax = val;
   }
   if (xmax == 0.0)
      return( NO );
      
   /* If maximum < 1.0 we do not add to the field width. */
   *field = MYMAX( (int)log10( xmax )+1, 0 );   

   i = 0;
   do                           /* Determine smallest of abs values in errors */
   {
      exmin = fabs( ex[i++] );
   }
   while (exmin == 0.0 && i < n);
   
   for  ( ; i < n; i++)
   {
      double val = fabs( ex[i] );
      if (val < exmin) 
         exmin = val;
   }
   if (exmin == 0.0)
      return( NO );
          
  
   /* If xmin < 1.0, the log10 value is a negative value and its   */
   /* absolute value (+1) is the wanted precision. Else, the log10 */
   /* value is positive and no precision (0) is needed.            */
   
   *prec = ABS( MYMIN( (int)log10( exmin )-1, 0 ) ); 
   *field += (*prec) + 1;     /* Add one for radix character in format */
   return( YES );
}



static void logtable2( fint    nrings,        /* Tot.num of rings in model */
                       fint    nsegs,         /* Tot.num of segments (all rings) */
                       fint    totpixels,     /* Total num. of pixels in box */
                       char   *mapunits,      /* Units of INSET= data */
                       double *hirad,         /* Higher radii of rings */
                       double *lorad,         /* Lower radii of rings */
                       double *surfdens,      /* Array with surface densities */
                       double *surfderr,      /* Array with errors on sf.dens.*/
                       double  chi2,          /* Chi square */
                       double  redchi2,       /* Rediced chi square */
                       double *gridspacing,
                       double  realtime,      /* Program time in sec. */
                       double  cputime,       /* Real processor time */
                       fchar   Setin,         /* The input set */
                       fchar   Setout,        /* Output made with fit */
                       fchar   Setpro,        /* Set with deprj. model.*/
                       bool    writedeproj,   /* Write map with deproj. model */
                       fchar   Tname )        /* GDS Table name */
/*-------------------------------------------------------------*/
/* PURPOSE: Write a table with surface densities and sum.      */
/* Characteristics known after the fit.                        */
/*-------------------------------------------------------------*/
{
   int       m, k;
   double    pixarea;
   int       segbase = 0;
   char      *border;
   char      formatstr[256];
   fint      dev = 3;
   int       field, prec;
   int       ok;
   double    area;
   double    numpixels;
   

   pixarea = fabs( gridspacing[0]*gridspacing[1] );
   
   ok = fieldformat(surfdens, surfderr, nsegs, &field, &prec);
   if (!ok || field > 9)
      strcpy( formatstr, "%4d | %3d | %10.3e | %10.3e | %10.3e | %10.3e | %10.2f |" );      
   else
   {
      int   sumprec;
      
      area = PI * (hirad[0]*hirad[0] - lorad[0]*lorad[0]);
      numpixels = area/segments[0]/pixarea;
      if (numpixels <= 0.0)
         sumprec = prec;
      else
         sumprec = prec - (int) log10(numpixels);
      (void) sprintf( formatstr,
           "%%4d | %%3d | %%10.%df | %%10.%df | %%10.%df | %%10.%df | %%10.2f |",
                          prec,      prec,      sumprec,   sumprec );         
   }

   m = sprintf( messbuf, "%4s | %3s | %10s | %10s | %10s | %10s | %10s |",
               "ring", "seg", "surfdens", "+/-", "sum", "+/-", "pixels" );

   border = malloc( m+1 );
   if (!border)
   {
      anyoutT( 1, "Not enough memory to produce a table!" );
      return;
   }
               
   memset( border, '=', m );
   border[m] = '\0';
   anyoutC( dev, border );
   anyoutC( dev, messbuf );
   (void) sprintf( messbuf, "%4s | %3s | %10s | %10s | %10s | %10s | %10s |",
                   " ", " ", mapunits, mapunits, mapunits, mapunits, " " );
   anyoutC( dev, messbuf );   
   anyoutC( dev, border );

   segbase = 0;
   for (m = 0; m < nrings; m++)
   {
      area = PI * (hirad[m]*hirad[m] - lorad[m]*lorad[m]);
      numpixels = area/segments[m]/pixarea;       
      for (k = 0; k < segments[m]; k++)
      {
         int     mk = segbase + k;
         anyoutf( dev, formatstr,
                       m + 1,
                       k + 1,
                       surfdens[mk],
                       surfderr[mk],
                       surfdens[mk] * numpixels,
                       surfderr[mk] * numpixels,
                       numpixels );
      }
      segbase += segments[m];
   }
   anyoutC( dev, border );
   anyoutC( dev, " " );
   {
      fchar     Userstr, Datestr;
      fmake( Userstr, 256 );
      fmake( Datestr, 256 );
      getusernam_c( Userstr );
      getdate_c( Datestr );
      anyoutf( dev, " RESULTS (%.*s) OBTAINED BY USER %.*s:",
                      nelc_c( Datestr ), Datestr.a,
                      nelc_c( Userstr ), Userstr.a );
      anyoutC( dev, " " );
   }
   anyoutf( dev, "-Rings were fitted using data from set '%.*s'",
                   nelc_c( Setin ), Setin.a );
   anyoutf( dev, "-A map with the fitted model is written to set '%.*s'",
                   nelc_c( Setout ), Setout.a );
   anyoutf( dev, "-The set '%.*s' also contains the ring parameter table '%.*s'",
                   nelc_c( Setout ), Setout.a,
                   nelc_c( Tname ), Tname.a );
   anyoutC( dev, " at set level!" );

   if (writedeproj)
   {
      anyoutC( dev, "-A map with the fitted model projected on the plane of" );
      anyoutf( dev, " the central disk is written to set '%.*s'",
                      nelc_c( Setpro ), Setpro.a );
   }
   /*------------------------------------------------*/
   /* The box and segment keywords are not cancelled */
   /* so we can use them again.                      */
   /*------------------------------------------------*/
   {
      fchar  Info1, Info2;
      fint   opt = HIDDEN;

      fmake( Info1, BIGSTORE );
      fmake( Info2, BIGSTORE );      
      (void) usertext_c( Info1, &opt, KEY_BOX, tofchar(" ") );
      anyoutf( dev, "-You selected the box: %.*s containing %d pixels",
                      nelc_c( Info1 ), Info1.a, totpixels );
      (void) usertext_c( Info2, &opt, KEY_SEGMENTS, tofchar(" ") );
      anyoutf( dev, "-You entered the segment expression: %.*s",
                      nelc_c( Info2 ), Info2.a );
   }
   anyoutf( dev, "-Total number of rings: %d. Total number of segments: %d",
                   nrings, nsegs );
   anyoutf( dev, "-Task '%s' fitted %d segments in %.2f sec (%.2f cpu sec)",
                   taskname, nsegs, realtime, cputime );
   anyoutf( dev, "-Minimized Chi^2 fitting all pixels: %g", chi2 );
   anyoutf( dev, "-Reduced Chi^2 [=Chi^2/(pixels-segments)]:  %g",
                   redchi2 );
   /*---------------------------------------------------------*/
   /* The uncertainties are not known in advance. Assume that */
   /* all measurements have the same standard deviation sigma */
   /* and assume that you have a good fit, then you can use   */
   /* chi2 to compute an estimated error.                     */
   /*---------------------------------------------------------*/
   {
      double sigma;

      sigma = sqrt( chi2 / (double) (totpixels-nsegs) );
      anyoutf( dev, "-Measurement error (per pixel): %f", sigma );
   }
   anyoutC( dev, border );
   free( border );

   anyoutC( dev," ");
}



static void convertarrays( fint    nrings,
                           double *posangs,
                           double *inclinations )
/*-------------------------------------------------------------*/
/* PURPOSE: Convert user input angles in degrees to radians.   */
/* The derived formulae for the geometry all use a position    */
/* angle wrt. the X-axis: alpha = 90 - posang.                 */
/*-------------------------------------------------------------*/
{
   int  i;

   for (i = 0; i < nrings; i++)
   {
      inclinations[i] = TORAD( inclinations[i] );
      posangs[i] = TORAD(90.0 - posangs[i]);
   }
}



static void getdatafromdisk( double *mu,
                             fint   *blo,
                             fint   *bhi,
                             fchar  Setin,
                             fint   subset )
/*-------------------------------------------------------------*/
/* PURPOSE: Fill array with pixel values from set.             */
/*-------------------------------------------------------------*/
{
   fint   cwlo, cwhi;
   fint   tid = 0;
   fint   totpixels = 0;
   fint   pixelsread, numpixels = 0;
   fint   maxIObuf = MAXBUF;
   int    i, k = 1;                  /* Note that index of 'mu' starts with 1 */


   totpixels = (bhi[0]-blo[0]+1) * (bhi[1]-blo[1]+1);
   cwlo   = gdsc_fill_c( Setin, &subset, blo );
   cwhi   = gdsc_fill_c( Setin, &subset, bhi );

   do
   {
      gdsi_read_c( Setin,
                   &cwlo, &cwhi,
                   image,
                   &maxIObuf,
                   &pixelsread,
                   &tid );
      numpixels += pixelsread;
      for (i = 0; i < pixelsread; i++)
      {
         if (image[i] == fblank)
            mu[k] = dblank;
         else
            mu[k] = (double) image[i];         /* Promote to double */
         k++;
      }
   } while (tid != 0);

   if (numpixels != totpixels)
      anyoutT( 1, "Did not read all pixels in box!" );
}



static int getsurfacedensities( fint    nrings,
                                fint    nsegs,
                                fint    iseed,
                                fint    maxclouds,
                                bool    eqdens,
                                fint    profileoption,
                                fint   *blo,
                                fint   *bhi,
                                double *surfdens,
                                double *surfderr,
                                double *chi2,
                                double *redchi2,
                                bool    makeplot,
                                double *gridspacing,
                                double *cpos,
                                double **M,
                                double *mu )
/*-------------------------------------------------------------*/
/* PURPOSE: Generate positions in a ring, transform position to*/
/*          a (pixel) array index, build matrix and return     */
/*          solution of:  Matrix*pars = mu.                    */
/* 'M'  is a matrix with as many rows as there are pixels in   */
/* the user defined box. The number of columns is equal to the */
/* number of rings. 'mu' is the column with pixel values from  */
/* disk. 'pars' are the surface densities we want to fit. It is*/
/* a column with length 'nrings'.                              */
/* The matrix 'M' is returned to be used in creating an output */
/* set with the fitted parameters.                             */
/*-------------------------------------------------------------*/
{
   double     x, y, z;                         /* Ring coordinates */
   double     radius, angle;                   /* Random radius and angle */
   double     fraction;
   int        ring, cloud;                     /* Counters */
   int        pixelindx;
   int        maxindx;
   int        ret;
   int        outrange = 0;
   int        segbase;                         /* Create index from ring and segment */
   int        totpixels;
   double     numclouds = maxclouds;
   double     weight = 1.0;
   double     *pars;
   double     *parerr;
   double     maxarea = -1.0;
   fint       color = 1;


   pars = surfdens;
   pars--;                                     /* First index = 1 */
   parerr = surfderr;
   parerr--;
   totpixels = (bhi[0]-blo[0]+1) * (bhi[1]-blo[1]+1);
   maxindx   = totpixels - 1;

   if (makeplot)
   {
      fint     nsub = 0;
      float    xsmin, xsmax;
      float    tick = 0.0;

      initplot( 1, 1, NO );

      dminmaxf( hirad, nrings, &xsmin, &xsmax );
      setwindow( -xsmax, xsmax, -xsmax, xsmax, 1.0 );
      pgbox_c( tofchar("BCNST"), &tick, &nsub,
               tofchar("BCNSTV"), &tick, &nsub );
   }

   /* Find the area of the biggest ring */
   for (ring = 0; ring < nrings; ring++)
   {
      double   Rlo2, Rhi2;
      double   area;

      Rlo2 = lorad[ring] * lorad[ring];
      Rhi2 = hirad[ring] * hirad[ring];
      area = PI * (Rhi2 - Rlo2);
      if (area > maxarea)
         maxarea = area;
   }

   /*---------------------------------------------------------*/
   /* Generate for all rings 'maxclouds' random clouds of     */
   /* weight 'weight' and determine their pixel position in   */
   /* the plane of the sky. Add weight to matrix element 'M'. */
   /*---------------------------------------------------------*/
   segbase = 0;
   for (ring = 0; ring < nrings; ring++)
   {
      double   Rlo2, Rhi2;
      double   Rhi2minRlo2;
      double   area;

      if (makeplot)
      {
         color++;
         setcolor( color );
      }
      (void) sprintf( messbuf, "Generating clouds for ring %d", ring+1 );
      status_c( tofchar(messbuf) );
      Rlo2 = lorad[ring] * lorad[ring];
      Rhi2 = hirad[ring] * hirad[ring];
      Rhi2minRlo2 = Rhi2 - Rlo2;
      area = PI * (Rhi2 - Rlo2);

      /* Correct for decreasing areas wrt. the largest area. */
      if (eqdens)
      {
         numclouds = (int) ( (double) maxclouds*area/maxarea);
         weight = 1.0;/* /(double) numclouds;*/
      }
      for (cloud = 0; cloud < numclouds; cloud++)
      {
         int   segnr;

         /* Get random radius */
         fraction = iran_c( &iseed ) / RANSCALE;   /* Number in range [0,1> */
         /*--------------------------------------------------*/
         /* Correct linear distribution for increasing area. */
         /* To fill a circle, the correction would be:       */
         /* Radius = Radius *sqrt( rnd ) and  0 <= rnd < 1   */
         /* For a ring:                                      */
         /* Radius = sqrt( (Rhi^2-Rlo^2)*rnd + Rlo^2 )       */
         /*--------------------------------------------------*/
         radius = sqrt( Rhi2minRlo2 * fraction + Rlo2);

         /* Get random angle [0,2PI> */
         fraction = iran_c( &iseed ) / RANSCALE;
         angle = fraction * 2.0 * PI;
         x = radius * cos( angle );
         y = radius * sin( angle );

         /* Get random height */
         z = randev_c( &profileoption, &iseed ) * Z0[ring];
         pixelindx = ringpos2pixindex( ring,
                                       blo, bhi,
                                       gridspacing,
                                       cpos,
                                       x, y, z );
         if (pixelindx < 0 || pixelindx >= totpixels)
            outrange++;
         else
         {
            segnr = (int) ( fraction * (double) segments[ring] );
            M[pixelindx+1][segbase+segnr+1] += weight;
         }
         if (makeplot)
            setmarker( x, y, -1 );
      }
      segbase += segments[ring];
   }   

   if (outrange)
   {
      (void) sprintf( messbuf, "There are %d clouds outside box!", outrange );
      anyoutT( 1, messbuf );
   }

   /*---------------------------------------*/
   /* Read data from disk. Fill vector mu.  */
   /*---------------------------------------*/
   (void) sprintf( messbuf, "Reading data from set %.*s",
                   nelc_c(Setin), Setin.a );
   status_c( tofchar(messbuf) );
   getdatafromdisk( mu, blo, bhi, Setin, subin[0] );

   /*---------------------------------------------------------------*/
   /* Correct matrix elements for pixels which are not completely   */
   /* covered by a ring (outer ring). Skip equations for pixels     */
   /* outside all rings (0.pars=mu) and pixels that are blank.      */
   /*---------------------------------------------------------------*/
   {
      int col, row;
      int step = totpixels/20;
      
      status_c( tofchar("Prepare matrix...") );      
      for (row = 1; row <= totpixels; row++)
      {
         double sum = 0.0;
         if (!(row%step))       /* Update message ~20 times */
         {
            (void) sprintf( messbuf,
                           "Normalize matrix: %d%%",
                            row*100/totpixels );
            status_c( tofchar(messbuf) );
         }
         for (col = 1; col <= nsegs; col++)
            sum += M[row][col];
         if (sum != 0.0 && mu[row] != dblank)
         {
            for (col = 1; col <= nsegs; col++)
               M[row][col] /= sum;
         }
         else
         {
            if (sum != 0.0)
            {
               for (col = 1; col <= nsegs; col++)
                  M[row][col] = 0.0;
            }
            mu[row] = 0.0;
         }
      }
   }
   /*---------------------------------------*/
   /* The Matrix 'M' is known. The vector   */
   /* 'mu' is filled. Solve 'pars'.         */
   /*---------------------------------------*/
   ret = lfit( M, mu, totpixels, nsegs, pars, parerr, chi2, redchi2 );

   if (ret != 0)
   {
      (void) sprintf( messbuf, "Could not obtain a solution. err=%d", ret );
      anyoutT( 1, messbuf );
      return( NO );
   }
   return( YES );
}



static void refillM( fint     nrings,
                     fint     nsegs,
                     fint     iseed,
                     fint     profileoption,
                     fint     maxclouds,
                     bool     eqdens,
                     fint    *blo,
                     fint    *bhi,
                     double  *gridspacing,
                     double  *cpos,
                     double **M )
/*-------------------------------------------------------------*/
/* PURPOSE: Fill matrix that represents a deprojected fit.     */
/* Fill the matrix 'M' with weights. Use the system of the     */
/* central disk to determine x,y positions.                    */
/* Note that the grid spacings are equal in x and y direction  */
/* and are both positive values.                               */
/*-------------------------------------------------------------*/
{
   double     x, y;                            /* Ring coordinates */
   double     radius, angle;                   /* Random radius and angle */
   double     fraction;
   int        ring, cloud;                     /* Counters */
   int        pixelindx;
   int        maxindx;
   int        outrange = 0;
   int        segbase;                         /* Create index from ring and segment */
   int        totpixels;
   int        col, row;
   fint       Xlen;
   double     numclouds = maxclouds;
   double     weight = 1.0;
   double     maxarea = -1.0;
   double     a0, ak, i0, ik;
   double     cosak, sinak, cosa0, sina0;
   double     cosik, sinik, cosi0, sini0;


   totpixels = (bhi[0]-blo[0]+1) * (bhi[1]-blo[1]+1);
   maxindx   = totpixels - 1;
   Xlen      = bhi[0] - blo[0] + 1;             /* Length of X axis in output */

   a0 = posangs[0];                             /* pos. ang. of central disk */
   i0 = inclinations[0];

   for (row = 1; row <= totpixels; row++)       /* Reset the matrix */
      for (col = 1; col <= nsegs; col++)
          M[row][col] = 0.0;

   for (ring = 0; ring < nrings; ring++)        /* Determine max. area */
   {
      double   Rlo2, Rhi2;
      double   area;

      Rlo2 = lorad[ring] * lorad[ring];
      Rhi2 = hirad[ring] * hirad[ring];
      area = PI * (Rhi2 - Rlo2);
      if (area > maxarea)
         maxarea = area;
   }

   cosa0 = cos(a0);
   sina0 = sin(a0);
   cosi0 = cos(i0);
   sini0 = sin(i0);
   /*---------------------------------------------------------*/
   /* Generate for all rings 'maxclouds' random clouds of     */
   /* weight 'weight' and determine their pixel position in   */
   /* the plane of the sky. Add weight to matrix element 'M'. */
   /*---------------------------------------------------------*/
   segbase = 0;
   for (ring = 0; ring < nrings; ring++)
   {
      double   Rlo2, Rhi2;
      double   Rhi2minRlo2;
      double   area;

      (void) sprintf( messbuf, "Generating deprojected ring %d", ring+1 );
      status_c( tofchar(messbuf) );
      Rlo2 = lorad[ring] * lorad[ring];
      Rhi2 = hirad[ring] * hirad[ring];
      Rhi2minRlo2 = Rhi2 - Rlo2;
      area = PI * (Rhi2 - Rlo2);

      ak = posangs[ring];
      ik = inclinations[ring];
      cosak = cos(ak);
      sinak = sin(ak);
      cosik = cos(ik);
      sinik = sin(ik);

      /* Correct for decreasing areas wrt. the largest area. */
      if (eqdens)
      {
         numclouds = (int) ( (double) maxclouds*area/maxarea);
         weight = 1.0;/* /(double) numclouds;*/
      }
      for (cloud = 0; cloud < numclouds; cloud++)
      {
         int   segnr;
         fint  IX, IY;

         /* Get random radius */
         fraction = iran_c( &iseed ) / RANSCALE;   /* Number in range [0,1> */
         /*--------------------------------------------------*/
         /* Correct linear distribution for increasing area. */
         /* To fill a circle, the correction would be:       */
         /* Radius = Radius *sqrt( rnd ) and  0 <= rnd < 1   */
         /* For a ring:                                      */
         /* Radius = sqrt( (Rhi^2-Rlo^2)*rnd + Rlo^2 )       */
         /*--------------------------------------------------*/
         radius = sqrt(Rhi2minRlo2 * fraction + Rlo2);

         /* Get random angle [0,2PI> */
         fraction = iran_c( &iseed ) / RANSCALE;
         angle = fraction * 2.0 * PI;
         x = radius * cos( angle );
         y = radius * sin( angle );

         /*--------------------------------------------------------*/
         /* For all rings other than the central disk, transform   */
         /* the x,y, coordinates of those rings to x,y coordinates */
         /* of the central disk.                                   */
         /*--------------------------------------------------------*/
         if (ring != 0)
         {
            double X, Y, Z;
            X = x*cosak - y*sinak*cosik;
            Y = x*sinak + y*cosak*cosik;
            Z =           y*sinik;

            x =  cosa0*X       + sina0*Y;
            y = -sina0*cosi0*X + cosa0*cosi0*Y + sini0*Z;
         }

         /*--------------------------------------------------------*/
         /* Now calculate the corresponding pixel position. Note   */
         /* that the output is again centered around cpos, but the */
         /* grid spacings are equal in both directions.            */
         /*--------------------------------------------------------*/
         x = x/gridspacing[0] + cpos[0];      /* Position in pixels corrected */
         y = y/gridspacing[1] + cpos[1];      /*   for shift in origin */

         if (x < 0.0)
            IX = (int) (x - 0.5);
         else
            IX = (int) (x + 0.5);
         if (y < 0.0)
            IY = (int) (y - 0.5);
         else
            IY = (int) (y + 0.5);

         pixelindx = (IY-blo[1]) * Xlen + IX-blo[0];

         if (pixelindx < 0 || pixelindx >= totpixels)
            outrange++;
         else
         {
            segnr = (int) ( fraction * (double) segments[ring] );
            M[pixelindx+1][segbase+segnr+1] += weight;
         }
      }
      segbase += segments[ring];
   }

   if (outrange)
   {
      (void) sprintf( messbuf, "There are %d clouds outside box!", outrange );
      anyoutT( 1, messbuf );
   }
   /*---------------------------------------------------------------*/
   /* Correct matrix elements for pixels which are not completely   */
   /* covered by a ring (outer ring).                               */
   /*---------------------------------------------------------------*/
   for (row = 1; row <= totpixels; row++)
   {
      double sum = 0.0;
      for (col = 1; col <= nsegs; col++)     /* Sum column values in this row */
         sum += M[row][col];
      if (sum != 0.0)
      {
         for (col = 1; col <= nsegs; col++)  /* Scale each column element */
            M[row][col] /= sum;
      }
      else
      {
         for (col = 1; col <= nsegs; col++)
            M[row][col] = 0.0;
      }
   }
}



static void writetoset( fchar    Setname,
                        fint     subset,
                        fint    *blo,
                        fint    *bhi,
                        fint     nsegs,
                        double **M,
                        double  *surfdens )
/*-------------------------------------------------------------*/
/* PURPOSE: Using the matrix 'M' and vector 'pars', create     */
/* output map and write to disk.                               */
/*-------------------------------------------------------------*/
{
   fint       len = 0;
   fint       tid = 0;
   fint       cwlo, cwhi;
   fint       pixelsdone;
   int        col, row;
   int        totpixels;
   double     *pars;


   pars = surfdens;              /* 'pars' is pointer to 'surfdens' array */
   pars--;                       /* Raise to get first index == 1 */

   totpixels = (bhi[0]-blo[0]+1) * (bhi[1]-blo[1]+1);
   cwlo = gdsc_fill_c( Setname, &subset, blo );
   cwhi = gdsc_fill_c( Setname, &subset, bhi );
   for (len = 0, row = 1; row <= totpixels; row++)
   {
      double sum  = 0.0;
      double wsum = 0.0;
      double element;

      for (col = 1; col <= nsegs; col++)
      {
         element = M[row][col];
         wsum += element;
         sum  += element * pars[col];
      }
      if (wsum == 0.0)
         image[len] = fblank;
      else
         image[len] = (float) sum;

      len++;
      if (len == MAXBUF || row == totpixels)
      {
         gdsi_write_c( Setname,
                       &cwlo, &cwhi,
                       image,
                       &len,
                       &pixelsdone,
                       &tid );
         len = 0;
      }
   }
}



static void getparameters( fint  *iseed,
                           fint  *maxclouds,
                           bool  *eqdens,
                           fint  *densprof )
/*-------------------------------------------------------------*/
/* PURPOSE: Get some additional model parameters from user.    */
/* Get a value for the seed, number of clouds and profile.     */
/*-------------------------------------------------------------*/
{
   int       agreed;
   fint      nitems, dfault;
   char      message[BIGSTORE];
   fint      r1;

   /* Get a seed value for the Random Number Generator */
   nitems = 1;
   dfault = REQUEST;
   *iseed = -1;
   (void) sprintf( message, "Seed (should be negative):      [%d]", *iseed );
   r1 = userint_c( iseed, &nitems, &dfault,
                   KEY_ISEED,
                   tofchar( message ) );
   *iseed = -1 * ABS(*iseed);


   /* Get the max. number of Monte Carlo clouds */
   nitems = 1;
   dfault = REQUEST;
   *maxclouds = 100000;
   (void) sprintf( message, "Max. number of Monte Carlo clouds:     [%d]",
                   *maxclouds );
   r1 = userint_c( maxclouds, &nitems, &dfault,
                   KEY_MAXCLOUDS,
                   tofchar( message ) );

   *eqdens = toflog( NO );
   nitems  = 1;
   r1      = userlog_c( eqdens, &nitems, &dfault,
                        KEY_EQDENS,
                        tofchar( "Equal density of M.C. points in all rings?  Y/[N]") );
   *eqdens = tobool( *eqdens );

   nitems = 1;
   dfault = REQUEST;
   (void) sprintf( message,
                  "Give dens.prof. perpend. to plane of the rings: [list]");
   do
   {
      r1 = userint_c( densprof, &nitems, &dfault,
                      KEY_PROFILE,
                      tofchar(message) );
      agreed = (r1 != 0 && *densprof >= 1 && *densprof <= 5);
      if (!agreed)
      {
         anyoutC( 1, " ");
         anyoutC( 1, "   OPTIONS:");
         anyoutC( 1, "       1 -- Gaussian layer.");
         anyoutC( 1, "       2 -- Sech2 layer.");
         anyoutC( 1, "       3 -- Exponential layer.");
         anyoutC( 1, "       4 -- Lorentzian layer.");
         anyoutC( 1, "       5 -- Box layer.");
         anyoutC( 1, " ");
         cancel_c( tofchar("PROFILE=") );
      }
   } while (!agreed);
}



static int getrings( fchar   Setin,
                     fint   *subin,
                     double *gridspacing )
/*-------------------------------------------------------------*/
/* PURPOSE: Specify values for radius, pos. angle and incli-   */
/*          nation. Return number of input rings.              */
/* Get characteristics of model rings. The rings all have equal*/
/* width. The number of rings is determined by the radius of   */
/* the last ring. (the ring with the largest radius).          */
/* The return value is the number of input rings or 0 if an    */
/* error occurred. The grid spacings are returned in arcsec.   */
/*-------------------------------------------------------------*/
{
   fint    nrings = 0;              /* Function result */
   fint    dfault;
   fint    r1;
   fint    stored_as_real = -46;    /* Real in header read as double */
   fint    nitems;
   char    message[BIGSTORE];
   int     i;
   int     agreed;
   double  bmminor, bmmajor;        /* Minor,major axis of bundle from header */
   fint    subdim;
   int     m;
   fchar   Cunit;
   char    xunit[FITSLEN+1];
   char    yunit[FITSLEN+1];
   double  cdelt[2];
   double  conx, cony;
   int     dev = 3;


   anyoutC( dev, " " );
   anyoutC( dev, " INFO FROM HEADER:" );
   r1 = 0;
   gdsd_rdble_c( Setin, tofchar("BMMIN"), &subin[0], &bmminor, &r1 );
   if (r1 >= 0 || r1 == stored_as_real)
   {
      anyoutf( dev, "-Found minor axis length of beam: %f ARCSEC", bmminor );
      bmminor = fabs( bmminor );                           /* Just to be sure */
      r1 = 0;
      gdsd_rdble_c( Setin, tofchar("BMMAJ"), &subin[0], &bmmajor, &r1 );
      if (r1 >= 0 || r1 == stored_as_real)
      {
         anyoutf( dev, "-Found major axis length of beam: %f ARCSEC", bmminor );
         bmmajor = fabs( bmmajor );
      }
   }
   else
      anyoutT( 1, "Cannot find any beam properties in header!" );


   /* Get the grid spacings and convert to seconds of arc. */

   subdim = gdsc_ndims_c( Setin, &subin[0] );
   strcpy( xunit, "?" );
   strcpy( yunit, "?" );

   for (m = 0; m < (int) subdim; m++)
   {
      (void) sprintf( message, "CDELT%d", axnum[m] );
      r1 = 0;
      gdsd_rdble_c( Setin, tofchar(message), &setlevel, &cdelt[m], &r1 );
      fmake( Cunit, FITSLEN );
      (void) sprintf( message, "CUNIT%d", axnum[m] );
      r1 = 0;
      gdsd_rchar_c( Setin, tofchar(message), &setlevel, Cunit, &r1 );
      Cunit.a[nelc_c(Cunit)] = '\0';
      if (r1 == 0)
      {
         if (m == 0)
            strcpy( xunit, Cunit.a );
         else
            strcpy( yunit, Cunit.a );
      }
   }

   /*-----------------------------------------------------*/
   /* Grid spacings are returned in units seconds of arc. */
   /*-----------------------------------------------------*/
   r1 = factor_c( tofchar(xunit), tofchar("ARCSEC"), &conx );
   if (r1 != 0)
   {
      (void) sprintf( message, "Could not convert %s to seconds of arc!",
                      xunit );
      anyoutT( dev, message );
      return( 0 );
   }
   r1 = factor_c( tofchar(yunit), tofchar("ARCSEC"), &cony );
   if (r1 != 0)
   {
      (void) sprintf( message, "Could not convert %s to seconds of arc!",
                      yunit );
      anyoutT( dev, message );
      return( 0 );
   }
   gridspacing[0] = conx * cdelt[0];
   gridspacing[1] = cony * cdelt[1];

   anyoutf( dev, "-Grid spacings: %g x %g ARCSEC",
            gridspacing[0], gridspacing[1] );

   nitems = 1;
   (void) sprintf( message, "Give (outer) radius of rings (arcsec):" );
   nitems = MAXRINGS;
   radii = (double *) calloc( nitems, sizeof(double) );
   if (!radii)
   {
      anyoutT( 1, "Memory allocation problems for radii!" );
      return( 0 );
   }
   dfault = NONE;
   do
   {
      int  k;
      r1 = userdble_c( radii,
                       &nitems,
                       &dfault,
                       KEY_RADII,
                       tofchar(message) );
      agreed = YES;
      if (r1 == 0)
      {
         anyoutT( 1, "No radii specified!" );
         agreed = NO;
      }
      if (agreed)
      {
         for (k = 0; k < r1; k++)
         {
            if (radii[k] < 0.0)
               {
               anyoutT( 1, "Negative radius detected!" );
               agreed = NO;
               break;
            }
            if (k > 0 && radii[k] < radii[k-1])
            {
               (void) sprintf( message, "Radius %d <= radius %d !", k, k-1);
               anyoutT( 1, message );
               agreed = NO;
               break;
            }
         }
      }
      if (!agreed)
         reject_c( KEY_RADII, tofchar("Illegal radii!") );
   } while (!agreed);

   nrings = r1;

   /*-------------------------------------------------------*/
   /* Non axial symmetry: Each ring can have a number of    */
   /* segments. If the user wants axial symmetry, use       */
   /* SEGMENTS=0. Else, give the number of required segments*/
   /* for each ring. Note that if you give less numbers than*/
   /* that there are radii, the last number will be copied  */
   /* for the remaining radii. The default is calculated    */
   /* with Ns = INT(k/4)*4.                                 */
   /*-------------------------------------------------------*/
   {
      int  k;

      segments = (fint *) calloc( nrings, sizeof(fint) );
      if (!segments)
      {
         anyoutT( 1, "Could not allocate memory for 'segments' array!" );
         finis_c();
      }
      /* Calculate the defaults, do not exceed 32 segments per ring */
      for (k = 0; k < nrings; k++)
      {
         segments[k] = MYMIN((k/4)*4, 32);
         if (segments[k] == 0)
            segments[k] = 1;
      }

      (void) sprintf( message,
                     "Give number of segments per ring for %d rings:   [calc.]",
                      nrings );
      dfault = REQUEST;
      r1 = userint_c( segments,
                      &nrings,
                      &dfault,
                      KEY_SEGMENTS,
                      tofchar(message) );
      nsegs = 0;
      for (k = 0; k < nrings; k++)
      {
         if (segments[k] < 0)
         {
            segments[k] *= -1;
            anyoutT( 1, "Negative number detected and converted to positive.");
         }
         if (k < r1 && segments[k] == 0)
         {
            segments[k] = 1;
            anyoutT( 1, "Substituted 1 segment for 0" );
         }
         if (r1 > 0 && k >= r1)
            segments[k] = segments[k-1];
         nsegs += segments[k];
         /*---------------------------------------------------------*/
         /* At this point, 'nsegs' is at least 'nrings' and at most */
         /* the total number of segments in all rings.              */
         /*---------------------------------------------------------*/
      }
   }

   /*-------------------------------------------------------*/
   /* Create space for the arrays with ring characteristics */
   /*-------------------------------------------------------*/

   lorad        = (double *) calloc( nrings, sizeof(double) );
   hirad        = (double *) calloc( nrings, sizeof(double) );
   inclinations = (double *) calloc( nrings, sizeof(double) );
   posangs      = (double *) calloc( nrings, sizeof(double) );
   Z0           = (double *) calloc( nrings, sizeof(double) );
   surfdens     = (double *) calloc( nsegs,  sizeof(double) );
   surfderr     = (double *) calloc( nsegs,  sizeof(double) );


   if (!lorad || !hirad || !inclinations || !posangs || !Z0 ||
       !surfdens || !surfderr )
   {
      anyoutT( 1, "Could not allocate memory for 'ring' arrays!" );
      finis_c();
   }

   /*-----------------------------------------------------------------------*/
   /* Determine the number of rings. 'lastrad' > 0, so there is always      */
   /* one ring starting in 0, with width 'width'. Increase inner radius     */
   /* of rings until its outer radius is not longer smaller than 'lastrad'. */
   /*-----------------------------------------------------------------------*/

   lorad[0] = 0.0;
   hirad[0] = radii[0];
   for (i = 1; i < nrings; i++)
   {
      lorad[i] = radii[i-1];
      hirad[i] = radii[i];
   }
   /* Now we don't need the radius array anymore */
   free( radii );

   /*-----------------------------------*/
   /* Ask inclinations of the rings:    */
   /*-----------------------------------*/
   (void) sprintf( message, "Give %d inclinations (degree): ", nrings );
   dfault = NONE;
   r1 = userdble_c( inclinations,
                    &nrings, &dfault,
                    KEY_INCLINATION,
                    tofchar(message) );

   /* If less than 'nrings' are specified, fill up to 'nrings' */
   if (r1 > 0)
   {
      for (i = r1; i < nrings; i++)
         inclinations[i] = inclinations[i-1];
   }

   /*-----------------------------------*/
   /* Ask position angles of the rings: */
   /*-----------------------------------*/

   (void) sprintf( message, "Give %d position angles (deg. N-->E): ", nrings );
   dfault = NONE;
   r1 = userdble_c( posangs,
                    &nrings, &dfault,
                    KEY_POSANG,
                    tofchar(message) );

   /* If less than 'nrings' are specified, fill up to 'nrings' */
   if (r1 > 0)
   {
      for (i = r1; i < nrings; i++)
         posangs[i] = posangs[i-1];
   }

   /*-----------------------------------*/
   /* Ask scale heights of the rings:   */
   /*-----------------------------------*/
   (void) sprintf( message, "Give %d scale heights (arcsec): ", nrings );
   dfault = NONE;
   r1 = userdble_c( Z0, &nrings, &dfault,
                    KEY_Z0, tofchar(message) );

   /* If less than 'nrings' are specified, fill up to 'nrings' */
   if (r1 > 0)
   {
      for (i = r1; i < nrings; i++)
         Z0[i] = Z0[i-1];
   }

   return( nrings );
}




static bool tabexist( fchar Setin,
                      fchar Tname )
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
   gdsa_tabinq_c( Setin,
                  &setlevel,
                  Tname,
                  Cnames,
                  &nitems,
                  &nfound,
                  &r1 );
   free( Cnames.a );
   return (r1 >= 0 && nfound > 0);
}



static void gettabname( fchar Setin, fchar Tname )
/*-------------------------------------------------------------*/
/* PURPOSE: Get name of GDS table from user.                   */
/*-------------------------------------------------------------*/
{
   int     agreed;
   fint    nitems;
   fint    dfault;
   fint    r1;
   bool    overwrite;


   do
   {
      agreed  = YES;
      str2char(taskname, Tname);                     /* Tname < 8 chars! */
      dfault  = REQUEST;
      nitems  = 1;
      (void) sprintf( messbuf, "Give name of GDS table to store results:  [%.*s]",
                      nelc_c(Tname), Tname.a );
      r1  = userchar_c( Tname, &nitems, &dfault, KEY_TABNAME,
                        tofchar(messbuf) );

      if (tabexist(Setin, Tname))
      {
         dfault = REQUEST;
         nitems = 1;
         overwrite = toflog( YES );
         r1 = userlog_c( &overwrite,
                         &nitems,
                         &dfault,
                         KEY_OVERTAB,
                         tofchar("Overwrite table?            [Y]/N") );
         overwrite = tobool( overwrite );
         if (overwrite)                              /* Delete this table */
            gdsa_deltab_c( Setin,
                           &setlevel,
                           Tname,
                           &r1 );
         else
            agreed = NO;
      }
      cancel_c( KEY_TABNAME );
      cancel_c( KEY_OVERTAB );
   } while (!agreed);
}



static void inittable( fchar Setin,
                       fchar Tname,
                       char  *mapunits )
/*-------------------------------------------------------------*/
/* PURPOSE: Create columns in table.                           */
/*-------------------------------------------------------------*/
{
   fint   tablev = setlevel;
   fint   r1;


   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_SEGMENTS,
                  tofchar("INT"),
                  tofchar("Number of segments in this ring"),
                  tofchar("#"), &r1 );
   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_RINGNR,
                  tofchar("INT"),
                  tofchar("Index number of ring"),
                  tofchar("#"), &r1 );
   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_SEGNR,
                  tofchar("INT"),
                  tofchar("Segment nr 1..n"),
                  tofchar("#"), &r1 );

   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_LORAD,
                  tofchar("DBLE"),
                  tofchar("Inner radius of ring"),
                  tofchar("ARCSEC"), &r1 );
   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_HIRAD,
                  tofchar("DBLE"),
                  tofchar("Outer radius of ring"),
                  tofchar("ARCSEC"), &r1 );
   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_WIDTH,
                  tofchar("DBLE"),
                  tofchar("Width of ring"),
                  tofchar("ARCSEC"), &r1 );
   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_POSANG,
                  tofchar("DBLE"),
                  tofchar("Position angle of this ring"),
                  tofchar("DEGREE"), &r1 );
   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_INCLIN,
                  tofchar("DBLE"),
                  tofchar("Inclination of this ring"),
                  tofchar("DEGREE"), &r1 );
   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_Z0,
                  tofchar("DBLE"),
                  tofchar("Scale height of this ring"),
                  tofchar("ARCSEC"), &r1 );
   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_SURFDENS,
                  tofchar("DBLE"),
                  tofchar("Surface density of ring"),
                  tofchar(mapunits), &r1 );
   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_SURFDERR,
                  tofchar("DBLE"),
                  tofchar("Error in surface density of ring"),
                  tofchar(mapunits), &r1 );
   r1 = 0;
   gdsa_crecol_c( Setin, &tablev, Tname,
                  COL_AREA,
                  tofchar("DBLE"),
                  tofchar("Area of this ring"),
                  tofchar("ARCSEC^2"), &r1 );
}



static void filltab( fchar   Setin,
                     fchar   Tname,
                     fint    nrings,
                     double *lorad,
                     double *hirad,
                     double *posang,
                     double *inclinations,
                     double *Z0,
                     double *surfdens,
                     double *surfderr )
/*-------------------------------------------------------------*/
/* PURPOSE: Write arrays to a GDS table.                       */
/* This routine is not optimized, i.e. the elements are        */
/* written to the table element by element and not as an array.*/
/* This is done because we have arrays with different lengths  */
/* and have to loop over all the segments per ring for all     */
/* rings.                                                      */
/*-------------------------------------------------------------*/
{
   fint    r1;
   fint    tablev = setlevel;
   fint    one = 1;
   fint    rowstart = 1;
   int     i, s;
   int     segbase;


   segbase = 0;
   for (i = 0; i < nrings; i++)
   {
      for (s = 0; s < segments[i]; s++)
      {
         double   value;
         double   Rlo2, Rhi2;
         fint     segnr = (int) s + 1;
         fint     ringnr = i + 1;

         r1 = 0;
         gdsa_wcdble_c( Setin, &tablev, Tname, COL_LORAD,
                        &lorad[i], &rowstart, &one, &r1 );
         r1 = 0;
         gdsa_wcdble_c( Setin, &tablev, Tname, COL_HIRAD,
                        &hirad[i], &rowstart, &one, &r1 );
         r1 = 0;
         gdsa_wcint_c(  Setin, &tablev, Tname, COL_SEGMENTS,
                        &segments[i], &rowstart, &one, &r1 );
         r1 = 0;
         value = hirad[i] - lorad[i];
         gdsa_wcdble_c( Setin, &tablev, Tname, COL_WIDTH,
                        &value, &rowstart, &one, &r1 );
         r1 = 0;
         value = 90.0 - TODEG(posangs[i]);
         gdsa_wcdble_c( Setin, &tablev, Tname, COL_POSANG,
                        &value, &rowstart, &one, &r1 );
         r1 = 0;
         value = TODEG(inclinations[i]);
         gdsa_wcdble_c( Setin, &tablev, Tname, COL_INCLIN,
                        &value, &rowstart, &one, &r1 );
         Rlo2 = lorad[i] * lorad[i];
         Rhi2 = hirad[i] * hirad[i];
         value = PI * (Rhi2 - Rlo2);
         r1 = 0;
         gdsa_wcdble_c( Setin, &tablev, Tname, COL_AREA,
                        &value, &rowstart, &one, &r1 );
         r1 = 0;
         gdsa_wcdble_c( Setin, &tablev, Tname, COL_Z0,
                        &Z0[i], &rowstart, &one, &r1 );
         r1 = 0;
         gdsa_wcint_c(  Setin, &tablev, Tname, COL_SEGNR,
                        &segnr, &rowstart, &one, &r1 );
         r1 = 0;
         gdsa_wcint_c(  Setin, &tablev, Tname, COL_RINGNR,
                        &ringnr, &rowstart, &one, &r1 );
         r1 = 0;
         gdsa_wcdble_c( Setin, &tablev, Tname, COL_SURFDENS,
                        &surfdens[segbase+s], &rowstart, &one, &r1 );
         r1 = 0;
         gdsa_wcdble_c( Setin, &tablev, Tname, COL_SURFDERR,
                        &surfderr[segbase+s], &rowstart, &one, &r1 );
         rowstart++;
      }
      segbase += segments[i];
   }
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------*/
/* RECTIFY main.                                               */
/*-------------------------------------------------------------*/
{
   bool    plotmodel;
   bool    montecarloplot;
   bool    eqdens;
   bool    writedeproj = YES;
   fint    iseed;
   fint    maxclouds;
   fint    densprof;
   fint    r1, r2;
   fint    nitems, dfault;
   fint    scrnum;
   fint    class = 1;                      /* Repeat operation for each subset */
   fint    boxopt;                         /* Input option for 'gdsbox' */
   int     m;
   int     success = NO;
   int     totpixels;
   double  chi2, redchi2;
   double  gridspacing[2];
   double  cpos[2];
   double  cputime, realtime;              /* Variables for timer */
   fint    elapse = 1;
   fchar   Tname;                          /* Name of table */
   double  **M;
   double  *mu;



   init_c();                               /* Contact Hermes */
   /* Task identification */
   {
      fchar    Task;                       /* Name of current task */
      fmake( Task, TASKNAMLEN );           /* Create empty string */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char. */
      strcpy( taskname, Task.a );
      IDENTIFICATION( taskname, VERSION ); /* Show task and version */
   }

   initglobals();

   /*-------------------------------------------------------------------------*/
   /* Because Fortran passes all arguments by reference, all C functions with */
   /* a Fortran equivalent must do this also (GIPSY programmers guide,        */
   /* Chapter 9).                                                             */
   /*-------------------------------------------------------------------------*/

   fmake( Tname, ITEMLEN );
   fmake( Settab, BIGSTORE );
   dfault = HIDDEN;
   r1 = usertext_c( Settab,
                    &dfault,
                    KEY_TABSET,
                    MES_TABSET );
   if (r1)
   {
      fchar   Tablename;
      fmake(Tablename, ITEMLEN+1)
      dfault = REQUEST;
      nitems = 1;
      (void) sprintf( messbuf, "Give name of GDS TABLE (case sens.):        [stop]" );
      r1 = userchar_c( Tname,
                       &nitems,
                       &dfault,
                       KEY_TABNAME,
                       tofchar(messbuf) );
      cancel_c( KEY_TABNAME );
      if ( tabexist(Settab, Tname) )
         plottable( Settab, Tname );
      else
      {
         anyoutf( 1, "Could not find a table with this name. Note that input" );
         anyoutf( 1, "is case sensitive. Table names can be displayed with" );
         anyoutf( 1, "program TABLE INSET=<your set with GDS table>" );
      }
      if (plotopen)
         pgend_c();
      finis_c();                                     /* Quit Hermes */
      return( 0 );
   }


   /*------------------------*/
   /* Get the (sub)set(s)    */
   /*------------------------*/
   fmake(Setin, BIGSTORE);
   dfault = NONE;
   subdim = 2;              /* Subset must be two dimensional */
   scrnum = 8;              /* terminal, suppressed in "experienced mode" */
   nsubs  = gdsinp_c( Setin,
                      subin,
                      &maxsubs,
                      &dfault,
                      KEY_INSET,
                      MES_INSET,
                      &scrnum,
                      axnum,
                      axcount,
                      &maxaxes,
                      &class,
                      &subdim );

   setdim = gdsc_ndims_c( Setin, &setlevel );

   /*-----------------------------------------------------*/
   /* Determine the edges of this its frame ( frameLO/HI) */
   /*-----------------------------------------------------*/

   r1 = 0;
   (void) gdsc_range_c( Setin,
                        &setlevel,
                        &cwlo,
                        &cwhi,
                        &r1 );
   r1 = r2 = 0;
   for (m = 0; m < (int) setdim; m++) {
      frameLO[m] = gdsc_grid_c( Setin, &axnum[m], &cwlo, &r1 );
      frameHI[m] = gdsc_grid_c( Setin, &axnum[m], &cwhi, &r2 );
   }

   /*------------------------------------------------------------*/
   /* Prepare a box for INSET. Default is a box equal to the     */
   /* frame, but (boxopt=0) the box cannot be greater than the   */
   /* frame of the input subset(s).                              */
   /*------------------------------------------------------------*/
   dfault = REQUEST;
   boxopt = 0;
   scrnum = 8;
   (void) gdsbox_c( boxLO,
                    boxHI,
                    Setin,
                    subin,
                    &dfault,
                    KEY_BOX,
                    MES_BOX,
                    &scrnum,
                    &boxopt );


   nitems = 1;                                    /* One position (2 numbers) */
   dfault = NONE;
   r1 = gdspos_c( cpos,
                  &nitems, &dfault,
                  KEY_CPOS, MES_CPOS,
                  Setin, &subin[0] );

   /*------------------------------------------------------------*/
   /* Get output set. GDSASN copies the coordinate system of a   */
   /* previously opened input set obtained with GDSINP to the    */
   /* output set to be obtained with GDSOUT.                     */
   /*------------------------------------------------------------*/

   gdsasn_c( KEY_INSET,
             KEY_OUTSET,
             &class );

   /*------------------------------------------------------------*/
   /* Create set for deprojection. Default is no set             */
   /*------------------------------------------------------------*/

   gdsasn_c( KEY_INSET,
             KEY_PROSET,
             &class );

   /*------------------------------------------------------------*/
   /* GDSCSS changes the size of the subsets of the output set.  */
   /*------------------------------------------------------------*/

/*   gdscss_c( KEY_OUTSET,
             boxLO, boxHI );*/

   /*----------------------------------------------------------------*/
   /* GDSOUT prompts the user to enter the name of an output set and */
   /* the subsets, and returns the number of subsets entered.        */
   /*----------------------------------------------------------------*/

   fmake( Setout, BIGSTORE );
   dfault = NONE;
   do
   {
      nsubsO = gdsout_c( Setout,
                         subout,
                         &nsubs,
                         &dfault,
                         KEY_OUTSET,
                         MES_OUTSET,
                         &scrnum,
                         axnumO,
                         axcountO,
                         &maxaxes );
      if (nsubsO != nsubs)
        reject_c( KEY_OUTSET, tofchar("Subset(s) error") );
   } while (nsubsO != nsubs);


   fmake( Setpro, BIGSTORE );
   dfault = REQUEST;
   nsubsP = gdsout_c( Setpro,
                      subpro,
                      &nsubs,
                      &dfault,
                      KEY_PROSET,
                      MES_PROSET,
                      &scrnum,
                      axnumP,
                      axcountP,
                      &maxaxes );

   if (nsubsP)
      writedeproj = YES;
   else
      writedeproj = NO;


   getaxnames( Setin,
               xtitle,
               ytitle,
               mapunits );                /* Axes names and units */

   /*--------------------------------------------------------*/
   /* Get (global) pointers to arrays lorad, hirad, posangs, */
   /* inclinations & Z0 (scale heights) and segments.        */
   /* Create space for the arrays and get user input.        */
   /*--------------------------------------------------------*/
   nrings = getrings( Setin,
                      subin,
                      gridspacing );
   if (nrings == 0)
   {
      /* Mem. allocation-or unit conversion problems */
      if (plotopen)
         pgend_c();
      finis_c();
   }

   if (usercont( KEY_RADPLOT,
                "Plot radius vs. pos.ang/inclination?         Y/[N]",
                 NO,
                 REQUEST ))
   {
      initplot( 1, 2, YES );
      plotradvsposang( lorad,
                       hirad,
                       posangs,
                       inclinations,
                       nrings );
   }


   gettabname( Setin, Tname );

   logtable0( nrings, nsegs, cpos );
   logtable1( nrings,                       /* ring props. */
              segments,
              lorad,
              hirad,
              posangs,
              inclinations,
              Z0 );
   convertarrays( nrings,
                  posangs,
                  inclinations );           /* Convert to RADIANS */
   getparameters( &iseed,
                  &maxclouds,
                  &eqdens,
                  &densprof );              /* Other parameters */

   /*------------------------------------------*/
   /* Related to plot of model. 'xwmin' etc.   */
   /* determine the length of the plot axes.   */
   /* Note that the plotted rings are scaled   */
   /* so that the biggest ring has radius == 1 */
   /*------------------------------------------*/
   xwmin = ywmin = zwmin = -1.5;
   xwmax = ywmax = zwmax =  1.5;
   {
      fint   xlen = xwmax - xwmin;
      fint   ylen = ywmax - ywmin;

      theta = 30.0;
      phi   = 60.0;
      d_eye = 8.0 * MYMAX( xlen, ylen );
      rho   = d_eye / 2.0;
   }


   plotmodel = usercont( KEY_PLOTMODEL,
                        "Draw rings in perspective view?        Y/[N]",
                         NO,
                         REQUEST );

   if (plotmodel)
   /*---------------------------------------------------------*/
   /* Get a perspective view of the model. Loop over viewing  */
   /* angles.                                                 */
   /*---------------------------------------------------------*/
   {
      initplot( 1, 1, NO );
      do
      {
         int   ring;

         getviewpoint( &rho,
                       &theta,
                       &phi,
                       &d_eye );
         initviewtransform( rho,
                            theta,
                            phi );
         findscreenminmax( &xs_min, &xs_max,
                           &ys_min, &ys_max,
                           -1.0, 1.0,
                           -1.0, 1.0,
                           -1.0, 1.0 );

         drawaxes( xs_min, xs_max,
                   ys_min, ys_max,
                   xwmin, xwmax,
                   ywmin, ywmax,
                   zwmin, zwmax,
                   xtitle, ytitle,
                   "Z" );

         for (ring = 0; ring < nrings; ring++ )
         {
            double    scale = hirad[nrings-1];
            drawring( lorad[ring]/scale,        /* scale rings */
                      hirad[ring]/scale,
                      posangs[ring],
                      inclinations[ring] );
         }
         /*-------------------------------------------------------*/
         /* Plot vectors after rings, so that they are not hidden */
         /* by the rings.                                         */
         /*-------------------------------------------------------*/
         plotpqs( posangs[0], inclinations[0] );
         for (ring = 0; ring < nrings; ring++)
         {
            plotvector( posangs[ring], inclinations[ring], 'p', RED );
            plotvector( posangs[ring], inclinations[ring], 's', BLUE );
         }
      } while (usercont( KEY_CONT,
                        "Repeat perspective view?         [Y}/N",
                         YES,
                         REQUEST ));

      putid();                                       /* User id at bottom */
      putsetname( );                                 /* Input set name at top */
   }


   /*---------------------------------------------------------------*/
   /* Generate the 'clouds', fit and return the surface densities   */
   /*---------------------------------------------------------------*/
   montecarloplot = toflog( NO );
   nitems = 1;
   dfault = REQUEST;
   r1     = userlog_c( &montecarloplot,
                       &nitems,
                       &dfault,
                       KEY_MCPLOT,
                       tofchar( "Monte Carlo plot?    Y/[N]") );
   montecarloplot = tobool( montecarloplot );

   /*---------------------------------------------------------------*/
   /* Fit the ring parameters (surface density) and fill the output */
   /* set with values calculated with these fitted parameters.      */
   /*---------------------------------------------------------------*/


   /*--------------------------------------------------------------*/
   /* We need a Matrix with 'totpixels' rows and 'nsegs' columns.  */
   /* Also needed is a vector (mu), containing the pixel values    */
   /* read from disk.                                              */
   /*--------------------------------------------------------------*/
   totpixels = (boxHI[0]-boxLO[0]+1) * (boxHI[1]-boxLO[1]+1);
   (void) sprintf( messbuf,
                  "Trying to allocate %d Mb for %dx%d matrix",
                   sizeof(double)*totpixels*nsegs/1024/1024,
                   totpixels,
                   nsegs );
   status_c( tofchar(messbuf) );
   success = YES;
   M = dmatrix( totpixels, nsegs );
   status_c( tofchar(" ") );   
   if (!M)
   {
      (void) sprintf( messbuf,
                     "Memory allocation problems for %d x %d matrix",
                      totpixels, nsegs );
      anyoutT( 3, messbuf );
      success = NO ;
   }
   mu = dvector( totpixels );
   if (!mu)
   {
      (void) sprintf( messbuf,
                     "Memory allocation problems for length %d vector",
                      totpixels );
      anyoutT( 3, messbuf );
      dmfree( M, totpixels );
      success = NO;
   }

   if (success)
   {
      (void) sprintf( messbuf,
                     "Successful allocation of %d Mb for %dx%d matrix",
                      sizeof(double)*totpixels*(nsegs+1)/1024/1024,
                      totpixels,
                      nsegs );
      anyoutT( 3, messbuf );
      anyoutf( 3, "%*.*s   and vector of length %d.",
                   strlen(taskname), strlen(taskname), " ",
                   totpixels );

      timer_c( &cputime, &realtime, &elapse );   /* Reset timer */
      success = getsurfacedensities(
                        nrings,           /* Number of rings */
                        nsegs,            /* Total number of segments */
                        iseed,            /* Seed for Randon Num. Generator */
                        maxclouds,        /* Monte Carlo 'Clouds' */
                        eqdens,           /* Correct for increasing area (radius) */
                        densprof,         /* Vertical structure of galaxy */
                        boxLO, boxHI,     /* Work inside this box only */
                        surfdens,         /* Fitted output parameters */
                        surfderr,         /* Errors in covariance matrix */
                        &chi2,            /* Chi square in lsq. fit */
                        &redchi2,         /* Reduced chi2. */
                        montecarloplot,   /* Plot random clouds (slow) */
                        gridspacing,      /* Size of pixel in arcsec */
                        cpos,             /* Central position of galaxy */
                        M,                /* Matrix with weights */
                        mu );             /* Work space for input map */

      if (success)
      {
         (void) sprintf( messbuf, "Creating model output in: %.*s",
                         nelc_c(Setout), Setout.a );
         status_c( tofchar(messbuf) );
         writetoset( Setout, subout[0], boxLO, boxHI, nsegs, M, surfdens );
         status_c( tofchar(" ") );         
         if (writedeproj)
         {
            double  dp_gridspacing[2];
            dp_gridspacing[1] = dp_gridspacing[0]
                              = MYMAX( fabs(gridspacing[0]),
                                       fabs(gridspacing[1]) );
            if (gridspacing[0] < 0.0)
               dp_gridspacing[0] *= -1.0;  /* Restore original directions */
            if (gridspacing[1] < 0.0)
               dp_gridspacing[1] *= -1.0;
            refillM( nrings,  
                     nsegs,
                     iseed,
                     densprof,
                     maxclouds,
                     eqdens,
                     boxLO, boxHI,
                     dp_gridspacing,       /* In seconds of arc */
                     cpos,
                     M );
            (void) sprintf( messbuf,
                           "Creating deprojected model output in : %.*s",
                            nelc_c(Setpro), Setpro.a );
            status_c( tofchar(messbuf) );
            writetoset( Setpro, subpro[0], boxLO, boxHI, nsegs, M, surfdens );
            status_c( tofchar(" ") );            
            {
               fchar   Cdeltstr;
               fmake( Cdeltstr, FITSLEN );
               (void) sprintf( messbuf, "CDELT%d", axnumP[0] );
               /* Convert to degrees */
               dp_gridspacing[0] /= 3600.0;
               gdsd_wdble_c( Setpro, tofchar(messbuf),
                             &setlevel, &dp_gridspacing[0], &r1 );
               dp_gridspacing[1] /= 3600.0;
               (void) sprintf( messbuf, "CDELT%d", axnumP[1] );
               gdsd_wdble_c( Setpro, tofchar(messbuf),
                             &setlevel, &dp_gridspacing[1], &r1 );
            }
         }
      }

      dmfree( M, totpixels );
      dvfree( mu );
   }

   if (success)
   {
      timer_c( &cputime, &realtime, &elapse );
      logtable2( nrings,
                 nsegs,
                 totpixels,
                 mapunits,
                 hirad, lorad,
                 surfdens, surfderr,
                 chi2, redchi2,
                 gridspacing,
                 realtime, cputime,
                 Setin, Setout, Setpro,
                 writedeproj,
                 Tname );


      /*-----------------------------------------------------------*/
      /* Initialize table and write arrays to table in OUTPUT set! */
      /*-----------------------------------------------------------*/
      inittable( Setout, Tname, mapunits );
      filltab( Setout,       /* Set where table is stored */
               Tname,        /* Table name < 8 characters */
               nrings,       /* Number of rings (number of fitted densities) */
               lorad,        /* Inner radius of rings */
               hirad,        /* Outer radius of rings */
               posangs,      /* In filltab converted to degrees and corrected 90 deg */
               inclinations, /* In filltab converted to degrees */
               Z0,           /* Scale heights */
               surfdens,     /* Column with fitted surface densities */
               surfderr );   /* Errors from cavariance matrix */

      if (usercont( KEY_TRPLOT,
                   "Tilted ring overview?         Y/[N]",
                    NO,
                    REQUEST ))
      {
         initplot( 2, 1, YES );
         plotSxSy( nrings, posangs, inclinations );
         plotintersections( nrings, posangs, inclinations );
      }

      if (usercont( KEY_DRPLOT,
                   "Plot surface density vs radius?         Y/[N]",
                    NO,
                    REQUEST ))
      {
         initplot( 1, 1, YES );
         plotdensvsradius( lorad, hirad, surfdens, nrings, nsegs, mapunits );
      }
   }


   /* Release memory */

   if (segments)     free( segments );
   if (surfdens)     free( surfdens );
   if (surfderr)     free( surfderr );
   if (hirad)        free( hirad );
   if (lorad)        free( lorad );
   if (inclinations) free( inclinations );
   if (posangs)      free( posangs );
   if (Z0)           free( Z0 );

   if (plotopen)
      pgend_c();
   finis_c();                                     /* Quit Hermes */
   return( 0 );
}
