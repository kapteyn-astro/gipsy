C#>            pgdemo.dc1
CProgram:      PGDEMO
C
CPurpose:      Demonstrates PGPLOT.
C
CCategory:     DEMO
C
CFile:         pgdemo.f
C
CAuthor:       T.J. Pearson
C
CKeywords:
C
C   GRDEVICE=  Name of graphics device. Carriage Return gives a list
C              of all possible devices.
C
C** DEMOS=     Enter the number of demo routines to execute [1,2,3,4,5,6,7,
C              8,9,10,11,17]. Other numbers available: 13,14,15 and 18.
C
CUpdates:      Jun 29, 1991: KGB, Document created.
C              Oct 28, 1998: JPT, Modified for PGPLOT version 5.2.0.
C
C#<
C
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. The main program opens the output
C device and calls a series of subroutines, one for each sample plot.
C-----------------------------------------------------------------------
C
C Call PGBEG to initiate PGPLOT and open the output device; PGBEG
C will prompt the user to supply the device name and type.
C
      PROGRAM PGDEMO
      INTEGER DEMOS(18)
      INTEGER NDEMO, N
      INTEGER USERINT
C
      DATA DEMOS/1,2,3,4,5,6,7,8,9,10,11,17,0,0,0,0,0,0/
C
C Call the demonstration subroutines.
C
      CALL INIT
      CALL PGBEG(0, '?', 1, 1)
      NDEMO = USERINT( DEMOS, 18, 2, 'DEMOS=', 'Enter demo numbers' )
      IF (NDEMO.EQ.0) NDEMO = 12
      DO 10 N = 1, NDEMO
         IF (DEMOS(N).EQ.1) CALL PGEX1
         IF (DEMOS(N).EQ.2) CALL PGEX2
         IF (DEMOS(N).EQ.3) CALL PGEX3
         IF (DEMOS(N).EQ.4) CALL PGEX4
         IF (DEMOS(N).EQ.5) CALL PGEX5
         IF (DEMOS(N).EQ.6) CALL PGEX6
         IF (DEMOS(N).EQ.7) CALL PGEX7
         IF (DEMOS(N).EQ.8) CALL PGEX8
         IF (DEMOS(N).EQ.9) CALL PGEX9
         IF (DEMOS(N).EQ.10) CALL PGEX10
         IF (DEMOS(N).EQ.11) CALL PGEX11
         IF (DEMOS(N).EQ.13) CALL PGEX13
         IF (DEMOS(N).EQ.14) CALL PGEX14
         IF (DEMOS(N).EQ.15) CALL PGEX15
         IF (DEMOS(N).EQ.17) CALL PGEX17
         IF (DEMOS(N).EQ.18) CALL PGEX18
   10 CONTINUE
C
C Finally, call PGEND to terminate things properly.
C
C-----------------------------------------------------------------------
      CALL PGEND
      CALL FINIS
      END
C-----------------------------------------------------------------------
C This example illustrates the use of PGENV, PGLAB, PGPT, PGLINE.
C-----------------------------------------------------------------------
      SUBROUTINE PGEX1()
      INTEGER I
      REAL XS(5), YS(5), XR(100), YR(100)
C
C Call PGENV to specify the range of the axes and to draw a box, and
C PGLAB to label it. The x-axis runs from 0 to 10, and y from 0 to 20.
C
      DATA XS / 1., 2., 3., 4., 5. /
      DATA YS / 1., 4., 9., 16., 25. /
      CALL PGENV(0., 10., 0., 20., 0, 1)
C
C Mark five points (coordinates in arrays XS and YS), using symbol
C number 9.
C
      CALL PGLAB('(x)', '(y)', 'PGPLOT Example 1 - y = x'//CHAR(92)//
     #'u2')
C
C Compute the function at 60 points, and use PGLINE to draw it.
C
      CALL PGPT(5, XS, YS, 9)
      DO 10 I = 1, 60
      XR(I) = 0.1 * I
      YR(I) = XR(I) ** 2
   10 CONTINUE
C-----------------------------------------------------------------------
      CALL PGLINE(60, XR, YR)
      END
C-----------------------------------------------------------------------
C Repeat the process for another graph. This one is a graph of the
C sinc (sin x over x) function.
C-----------------------------------------------------------------------
      SUBROUTINE PGEX2()
      INTEGER I
C
      REAL XR(100), YR(100)
      CALL PGENV(-2., 10., -0.4, 1.2, 0, 1)
      CALL PGLAB('(x)', 'sin(x)/x',
     #'PGPLOT Example 2 - Sinc Function')
      DO 20 I = 1, 100
      XR(I) = (I - 20) / 6.
      YR(I) = 1.0
      IF (XR(I) .NE. 0.0) YR(I) = SIN(XR(I)) / XR(I)
   20 CONTINUE
C-----------------------------------------------------------------------
      CALL PGLINE(100, XR, YR)
      END
C----------------------------------------------------------------------
C This example illustrates the use of PGBOX and attribute routines to
C mix colors and line-styles.
C----------------------------------------------------------------------
      SUBROUTINE PGEX3()
      REAL PI
      PARAMETER (PI = 3.14159265)
      INTEGER I
      REAL XR(360), YR(360)
C
C Call PGENV to initialize the viewport and window; the
C AXIS argument is -2, so no frame or labels will be drawn.
C
      REAL ARG
C
C Set the color index for the axes and grid (index 5 = cyan).
C Call PGBOX to draw first a grid at low brightness, and then a
C frame and axes at full brightness. Note that as the x-axis is
C to represent an angle in degrees, we request an explicit tick 
C interval of 90 deg with subdivisions at 30 deg, as multiples of
C 3 are a more natural division than the default.
C
      CALL PGENV(0., 720., -2.0, 2.0, 0, -2)
      CALL PGSCI(14)
      CALL PGBOX('G', 30.0, 0, 'G', 0.2, 0)
      CALL PGSCI(5)
C
C Call PGLAB to label the graph in a different color (3=green).
C
      CALL PGBOX('ABCTSN', 90.0, 3, 'ABCTSNV', 0.0, 0)
      CALL PGSCI(3)
C
C Compute the function to be plotted: a trig function of an
C angle in degrees, computed every 2 degrees.
C
      CALL PGLAB('x (degrees)', 'f(x)', 'PGPLOT Example 3')
      DO 20 I = 1, 360
      XR(I) = 2.0 * I
      ARG = (XR(I) / 180.0) * PI
      YR(I) = (SIN(ARG) + (0.5 * COS(2.0 * ARG))) + (0.5 * SIN((1.5 * 
     #ARG) + (PI / 3.0)))
C
C Change the color (6=magenta), line-style (2=dashed), and line
C width and draw the function.
C
   20 CONTINUE
      CALL PGSCI(6)
      CALL PGSLS(2)
      CALL PGSLW(3)
C
C Restore attributes to defaults.
C
      CALL PGLINE(360, XR, YR)
      CALL PGSLS(1)
      CALL PGSCI(1)
C-----------------------------------------------------------------------
      CALL PGSLW(1)
      END
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT: draw a histogram.
C-----------------------------------------------------------------------
      SUBROUTINE PGEX4()
      INTEGER I, ISEED
      REAL DATA(1000), X(620), Y(620)
C
C Call RNGAUS to obtain 1000 samples from a normal distribution.
C
      REAL RNGAUS
      ISEED = 5678921
      DO 10 I = 1, 1000
      DATA(I) = RNGAUS(ISEED)
C
C Draw a histogram of these values.
C
   10 CONTINUE
      CALL PGHIST(1000, DATA, -3.1, 3.1, 31, 0)
C
C Superimpose the theoretical distribution.
C
      CALL PGLAB('Variate', ' ', 
     #'PGPLOT Example 4 - Histogram (Gaussian)')
      DO 20 I = 1, 620
      X(I) = (-3.1) + (0.01 * (I - 1))
      Y(I) = ((0.2 * 1000.) / SQRT(2. * 3.14159265)) * EXP(- ((0.5 * X(I
     #)) * X(I)))
   20 CONTINUE
C-----------------------------------------------------------------------
      CALL PGLINE(620, X, Y)
      END
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates how to draw a log-log plot.
C PGPLOT subroutines demonstrated:
C    PGENV, PGERRY, PGLAB, PGLINE, PGPT, PGSCI.
C----------------------------------------------------------------------
      SUBROUTINE PGEX5()
      INTEGER RED, GREEN, CYAN
      PARAMETER (RED = 2)
      PARAMETER (GREEN = 3)
      PARAMETER (CYAN = 5)
      INTEGER I
      REAL X, YLO, YHI
      REAL FREQ(15), FLUX(15), XP(100), YP(100), ERR(15)
C
C Call PGENV to initialize the viewport and window; the AXIS argument 
C is 30 so both axes will be logarithmic. The X-axis (frequency) runs 
C from 0.01 to 100 GHz, the Y-axis (flux density) runs from 0.3 to 300
C Jy. Note that it is necessary to specify the logarithms of these
C quantities in the call to PGENV. We request equal scales in x and y
C so that slopes will be correct.  Use PGLAB to label the graph.
C
      DATA FREQ / 26., 38., 80., 160., 178., 318., 365., 408., 750., 
     #1400., 2695., 2700., 5000., 10695., 14900. /
      DATA FLUX / 38.0, 66.4, 89.0, 69.8, 55.9, 37.4, 46.8, 42.4, 27.0, 
     #15.8, 9.09, 9.17, 5.35, 2.56, 1.73 /
      DATA ERR / 2*6.0, 13.0, 9.1, 2.9, 1.4, 2.7, 3.0, 0.34, 0.8, 0.2, 
     #0.46, 0.15, 0.08, 0.01 /
      CALL PGSCI(CYAN)
      CALL PGENV(-2.0, 2.0, -0.5, 2.5, 1, 30)
C
C Draw a fit to the spectrum (don't ask how this was chosen). This 
C curve is drawn before the data points, so that the data will write 
C over the curve, rather than vice versa.
C
      CALL PGLAB('Frequency, '//CHAR(92)//'gn (GHz)', 
     #'Flux Density, S'//CHAR(92)//'d'//CHAR(92)//'gn'//
     #CHAR(92)//'u (Jy)', 'PGPLOT Example 5 - Log-Log plot')
      DO 10 I = 1, 100
      X = 1.3 + (I * 0.03)
      XP(I) = X - 3.0
      YP(I) = (5.18 - (1.15 * X)) - (7.72 * EXP(- X))
   10 CONTINUE
      CALL PGSCI(RED)
C
C Plot the measured flux densities: here the data are installed with a
C DATA statement; in a more general program, they might be read from a
C file. We first have to take logarithms (the -3.0 converts MHz to GHz).
C
      CALL PGLINE(100, XP, YP)
      DO 20 I = 1, 15
      XP(I) = ALOG10(FREQ(I)) - 3.0
      YP(I) = ALOG10(FLUX(I))
   20 CONTINUE
      CALL PGSCI(GREEN)
C
C Draw +/- 2 sigma error bars: take logs of both limits.
C
      CALL PGPT(15, XP, YP, 17)
      DO 30 I = 1, 15
      YHI = ALOG10(FLUX(I) + (2. * ERR(I)))
      YLO = ALOG10(FLUX(I) - (2. * ERR(I)))
      CALL PGERRY(1, XP(I), YLO, YHI, 1.0)
C-----------------------------------------------------------------------
   30 CONTINUE
      END
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates the use of PGPOLY using SOLID and HOLLOW fill-area
C attributes.
C----------------------------------------------------------------------
      SUBROUTINE PGEX6()
      REAL TWOPI
      PARAMETER (TWOPI = 6.28319)
      INTEGER I, J
C
C Call PGENV to initialize the viewport and window; the
C AXIS argument is -2, so no frame or labels will be drawn.
C
      REAL X(10), Y(10)
C
C Call PGLAB to label the graph.
C
      CALL PGENV(0., 8., 0., 8.0, 1, -2)
      CALL PGSCI(3)
C
C Draw assorted regular convex polygons (solid).
C
      CALL PGLAB(' ', ' ', 'PGPLOT Example 6 - PGPOLY')
      CALL PGSFS(1)
      DO 20 I = 3, 9
      CALL PGSCI(I - 2)
      DO 10 J = 1, I
      X(J) = (I - 2) + (0.5 * COS((TWOPI * (J - 1)) / I))
      Y(J) = 6 + (0.5 * SIN((TWOPI * (J - 1)) / I))
   10 CONTINUE
      CALL PGPOLY(I, X, Y)
C
C Draw assorted regular convex polygons (hollow).
C
   20 CONTINUE
      CALL PGSFS(2)
      DO 40 I = 3, 9
      CALL PGSCI(I - 2)
      DO 30 J = 1, I
      X(J) = (I - 2) + (0.5 * COS((TWOPI * (J - 1)) / I))
      Y(J) = 3 + (0.5 * SIN((TWOPI * (J - 1)) / I))
   30 CONTINUE
      CALL PGPOLY(I, X, Y)
   40 CONTINUE
C-----------------------------------------------------------------------
      CALL PGSFS(1)
      END
C-----------------------------------------------------------------------
C Example program for PGPLOT. This program generates an Aitoff equal-
C area projection of the whole sky, centered on (lat=0, long=180).
C-----------------------------------------------------------------------
      SUBROUTINE PGEX7()
      REAL RPDEG
      PARAMETER (RPDEG = 0.0174533)
      INTEGER I, J
C
      REAL B, L, XC(361), YC(361)
C
C Call PGENV to create a rectangular window of 4 x 2 units. This is 
C the bounding rectangle of the Aitoff plot. The JUST argument is 1
C to get equal scales in x and y.  Setting the character height to
C zero eliminates the margin that PGENV normally leaves for labels.
C
      CALL PGBBUF
      CALL PGSCH(0.0)
C
C Draw 7 lines of constant longitude at longitude 0, 60, 120, ..., 
C 360 degrees. Each line is made up of 180 straight-line segments.
C
      CALL PGENV(-2.0, 2.0, -1.0, 1.0, 1, -2)
      DO 20 J = 1, 7
      L = ((-180.) + ((J - 1) * 60.)) * RPDEG
      DO 10 I = 1, 181
      B = (I - 91) * RPDEG
      CALL AITOFF(B, L, XC(I), YC(I))
   10 CONTINUE
      CALL PGLINE(181, XC, YC)
C
C Draw 5 lines of constant latitude at latitudes -60, -30, 0, 30, 
C 60 degrees. Each line is made up of 360 straight-line segments.
C
   20 CONTINUE
      DO 40 J = 1, 5
      B = ((-60.) + ((J - 1) * 30.)) * RPDEG
      DO 30 I = 1, 361
      L = FLOAT(I - 181) * RPDEG
      CALL AITOFF(B, L, XC(I), YC(I))
   30 CONTINUE
      CALL PGLINE(361, XC, YC)
C
C Having drawn the lines of latitude and longitude, one can now mark 
C points, etc.  To do this, call subroutine AITOFF to convert 
C latitude and longitude to (x,y) coordinates.  This is outside the 
C scope of this example program.  
C
   40 CONTINUE
      CALL PGEBUF
      CALL PGSCH(1.0)
      END
C-----------------------------------------------------------------------
C Aitoff projection .
C
C       Input: latitude and longitude (B,L) in radians
C       Output: cartesian (X,Y) in range +/-2, +/-1
C-----------------------------------------------------------------------
      SUBROUTINE AITOFF(B, L, X, Y)
C
      REAL L, B, X, Y, L2, DEN
      L2 = L / 2.0
      DEN = SQRT(1.0 + (COS(B) * COS(L2)))
      X = ((2.0 * COS(B)) * SIN(L2)) / DEN
      Y = SIN(B) / DEN
      END
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. This program shows some of the
C possibilities for overlapping windows and viewports.
C T. J. Pearson  1986 Nov 28
C-----------------------------------------------------------------------
      SUBROUTINE PGEX8()
      INTEGER I
C-----------------------------------------------------------------------
C Color index:
      REAL XR(720), YR(720)
      INTEGER BLACK, WHITE, RED, GREEN, BLUE, CYAN, MAGENT, YELLOW
      PARAMETER (BLACK = 0)
      PARAMETER (WHITE = 1)
      PARAMETER (RED = 2)
      PARAMETER (GREEN = 3)
      PARAMETER (BLUE = 4)
      PARAMETER (CYAN = 5)
      PARAMETER (MAGENT = 6)
C Line style:
      PARAMETER (YELLOW = 7)
      INTEGER FULL, DASHED, DOTDSH, DOTTED, FANCY
      PARAMETER (FULL = 1)
      PARAMETER (DASHED = 2)
      PARAMETER (DOTDSH = 3)
      PARAMETER (DOTTED = 4)
C Character font:
      PARAMETER (FANCY = 5)
      INTEGER NORMAL, ROMAN, ITALIC, SCRIPT
      PARAMETER (NORMAL = 1)
      PARAMETER (ROMAN = 2)
      PARAMETER (ITALIC = 3)
C Fill-area style:
      PARAMETER (SCRIPT = 4)
      INTEGER SOLID, HOLLOW
      PARAMETER (SOLID = 1)
C-----------------------------------------------------------------------
C
      PARAMETER (HOLLOW = 2)
C
C Define the Viewport
C
      CALL PGPAGE
C
C Define the Window
C
      CALL PGSVP(0.1, 0.6, 0.1, 0.6)
C
C Draw a box
C
      CALL PGSWIN(0.0, 630.0, -2.0, 2.0)
      CALL PGSCI(CYAN)
C
C Draw labels
C
      CALL PGBOX('ABCTS', 90.0, 3, 'ABCTSV', 0.0, 0)
      CALL PGSCI(RED)
C
C Draw SIN line
C
      CALL PGBOX('N', 90.0, 3, 'VN', 0.0, 0)
      DO 10 I = 1, 360
      XR(I) = 2.0 * I
      YR(I) = SIN(3.14159*XR(I)/180.0)
   10 CONTINUE
      CALL PGSCI(MAGENT)
      CALL PGSLS(DASHED)
C
C Draw COS line by redefining the window
C
      CALL PGLINE(360, XR, YR)
      CALL PGSWIN(90.0, 720.0, -2.0, 2.0)
      CALL PGSCI(YELLOW)
      CALL PGSLS(DOTTED)
      CALL PGLINE(360, XR, YR)
C
C Re-Define the Viewport
C
      CALL PGSLS(FULL)
C
C Define the Window, and erase it
C
      CALL PGSVP(0.45, 0.85, 0.45, 0.85)
      CALL PGSWIN(0.0, 180.0, -2.0, 2.0)
      CALL PGSCI(0)
C
C Draw a box
C
      CALL PGRECT(0.0, 180., -2.0, 2.0)
      CALL PGSCI(BLUE)
C
C Draw SIN line
C
      CALL PGBOX('ABCTSM', 60.0, 3, 'VABCTSM', 1.0, 2)
      CALL PGSCI(WHITE)
      CALL PGSLS(DASHED)
      CALL PGLINE(360, XR, YR)
C-----------------------------------------------------------------------
      CALL PGSLS(SOLID)
      END
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates curve drawing with PGFUNT; the parametric curve drawn is
C a simple Lissajous figure.
C                              T. J. Pearson  1983 Oct 5
C----------------------------------------------------------------------
      SUBROUTINE PGEX9()
      EXTERNAL PGFX, PGFY
C
C Call PGFUNT to draw the function (autoscaling).
C
      CALL PGSCI(5)
C
C Call PGLAB to label the graph in a different color.
C
      CALL PGFUNT(PGFX, PGFY, 360, 0.0, 2.0 * 3.14159265, 0)
      CALL PGSCI(3)
      CALL PGLAB('x', 'y', 'PGPLOT Example 9 - routine PGFUNT')
C
      CALL PGSCI(1)
      END

      REAL FUNCTION PGFX(T)
      REAL    T
      PGFX = SIN(T * 5.0)
      RETURN 
      END

      REAL FUNCTION PGFY(T)
      REAL    T
      PGFY = SIN(T * 4.0)
      RETURN 
      END
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  This example
C illustrates curve drawing with PGFUNX.
C                              T. J. Pearson  1983 Oct 5
C----------------------------------------------------------------------
C The following define mnemonic names for the color indices and
C linestyle codes.
      SUBROUTINE PGEX10()
      INTEGER BLACK, WHITE, RED, GREEN, BLUE, CYAN, MAGENT, YELLOW
      EXTERNAL BESJ0, BESJ1
      PARAMETER (BLACK = 0)
      PARAMETER (WHITE = 1)
      PARAMETER (RED = 2)
      PARAMETER (GREEN = 3)
      PARAMETER (BLUE = 4)
      PARAMETER (CYAN = 5)
      PARAMETER (MAGENT = 6)
      PARAMETER (YELLOW = 7)
      INTEGER FULL, DASH, DOTD
      PARAMETER (FULL = 1)
      PARAMETER (DASH = 2)
C
C The Fortran functions to be plotted must be declared EXTERNAL.
C
      PARAMETER (DOTD = 3)
C
C Call PGFUNX twice to draw two functions (autoscaling the first time).
C
      CALL PGSCI(YELLOW)
      CALL PGFUNX(BESJ0, 500, 0.0, 10.0 * 3.14159265, 0)
      CALL PGSCI(RED)
      CALL PGSLS(DASH)
C
C Call PGLAB to label the graph in a different color. Note the
C use of "\f" to change font.  Use PGMTXT to write an additional
C legend inside the viewport.
C
      CALL PGFUNX(BESJ1, 500, 0.0, 10.0 * 3.14159265, 1)
      CALL PGSCI(GREEN)
      CALL PGLAB(CHAR(92)//'fix', CHAR(92)//'fiy',
     #CHAR(92)//'frPGPLOT Example 10')
      CALL PGMTXT('T', -4.0, 0.5, 0.5, 
     #CHAR(92)//'fiy = J'//CHAR(92)//'d0'//CHAR(92)//'u(x)'//
     #CHAR(92)//'fr (solid),'//CHAR(92)//'fi y = J'//
     #CHAR(92)//'d1'//CHAR(92)//'u(x)'//CHAR(92)//'fr (dashed)')
C-----------------------------------------------------------------------
      CALL PGSCI(1)
      END
C-----------------------------------------------------------------------
C Test routine for PGPLOT: draws a skeletal dodecahedron.
C-----------------------------------------------------------------------
      SUBROUTINE PGEX11()
      INTEGER NVERT
      REAL T, T1, T2, T3
      PARAMETER (NVERT = 20)
      PARAMETER (T = 1.618)
      PARAMETER (T1 = 1.0 + T)
      PARAMETER (T2 = - (1.0 * T))
      PARAMETER (T3 = - (1.0 * T1))
      INTEGER I, J, K
      REAL VERT(3, NVERT), R, ZZ
C
C Cartesian coordinates of the 20 vertices.
C
      REAL X(2), Y(2)
C
C Initialize the plot (no labels).
C
      DATA VERT / 5*T, T2, T, T2, 2*T, 3*T2, 2*T, T2, T, 3*T2, T, 3*T2, 
     &T1, 1.0, 0.0, T1, -1.0, 0.0, T3, 1.0, 0.0, T3, -1.0, 2*0.0, T1, 
     &1.0, 0.0, T1, -1.0, 0.0, T3, 1.0, 0.0, T3, -1.0, 1.0, 0.0, T1, 
     &-1.0, 0.0, T1, 1.0, 0.0, T3, -1.0, 0.0, T3 /
      CALL PGENV(-4., 4., -4., 4., 1, -2)
      CALL PGSCI(2)
      CALL PGSLS(1)
C
C Write a heading.
C
      CALL PGSLW(1)
C
C Mark the vertices.
C
      CALL PGLAB(' ', ' ', 'PGPLOT Example 11 - Dodecahedron')
      DO 2 I = 1, NVERT
      ZZ = VERT(3,I)
      CALL PGPT(1, VERT(1,I) + (0.2 * ZZ), VERT(2,I) + (0.3 * ZZ), 9)
C
C Draw the edges - test all vertex pairs to find the edges of the 
C correct length.
C
    2 CONTINUE
      CALL PGSLW(3)
      DO 20 I = 2, NVERT
      DO 10 J = 1, I - 1
      R = 0.
      DO 5 K = 1, 3
      R = R + ((VERT(K,I) - VERT(K,J)) ** 2)
    5 CONTINUE
      R = SQRT(R)
      IF (ABS(R - 2.0) .GT. 0.1) GOTO 10
      ZZ = VERT(3,I)
      X(1) = VERT(1,I) + (0.2 * ZZ)
      Y(1) = VERT(2,I) + (0.3 * ZZ)
      ZZ = VERT(3,J)
      X(2) = VERT(1,J) + (0.2 * ZZ)
      Y(2) = VERT(2,J) + (0.3 * ZZ)
      CALL PGLINE(2, X, Y)
   10 CONTINUE
   20 CONTINUE
      CALL PGSLW(1)
C-----------------------------------------------------------------------
      CALL PGSCI(1)
      END

      REAL FUNCTION BESJ0(XX)
C     Bessel Function J0(xx)
C     Revised Sept, 1971  -  transfered to vax July 1979.
C     J0(-xx) = J0(xx)
C-----------------------------------------------------------------------
      REAL XX
C
      REAL X, XO3, T, F0, THETA0
      X = ABS(XX)
      IF (X .LE. 3.0) THEN
      XO3 = X / 3.
      T = XO3 * XO3
      BESJ0 = 1. + (T * ((-2.2499997) + (T * (1.2656208 + (T * ((
     #-.3163866) + (T * (.0444479 + (T * ((-.003944) + (T * .0002100))))
     #)))))))
      ELSE
      T = 3. / X
      F0 = .79788456 + (T * ((-.00000077) + (T * ((-.00552740) + (T * ((
     #-.00009512) + (T * (.00137237 + (T * ((-.00072805) + (T * 
     #.00014476)))))))))))
      THETA0 = (X - .78539816) + (T * ((-.04166397) + (T * ((-.00003954)
     # + (T * (.00262573 + (T * ((-.00054125) + (T * ((-.00029333) + (T
     # * .00013558)))))))))))
      BESJ0 = (F0 * COS(THETA0)) / SQRT(X)
      END IF
      RETURN 
      END
      REAL FUNCTION BESJ1(XX)
C     Bessel Function J1(xx)
C     Revised Sept,1971
C     J1(-xx)=-J1(xx)
C     Transfered to VAX    July 1979
C-----------------------------------------------------------------------
      REAL XX
C
      REAL X, XO3, T, F1, THETA1
      X = ABS(XX)
      IF (X .LE. 3.0) THEN
      XO3 = X / 3.
      T = XO3 * XO3
      BESJ1 = .5 + (T * ((-.56249985) + (T * (.21093573 + (T * ((
     #-.03954289) + (T * (.00443319 + (T * ((-.00031761) + (T * 
     #.00001109)))))))))))
      BESJ1 = BESJ1 * XX
      ELSE
      T = 3. / X
      F1 = .79788456 + (T * (.00000156 + (T * (.01659667 + (T * (
     #.00017105 + (T * ((-.00249511) + (T * (.00113653 - (T * .00020033)
     #))))))))))
      THETA1 = (X - 2.35619449) + (T * (.12499612 + (T * (.00005650 + (T
     # * ((-.00637879) + (T * (.00074348 + (T * (.00079824 - (T * 
     #.00029166)))))))))))
      BESJ1 = (F1 * COS(THETA1)) / SQRT(X)
      END IF
      IF (XX .LT. 0.0) BESJ1 = - BESJ1
      RETURN 
C*RNGAUS -- random number from Gaussian (normal) distribution
C+
      END
      REAL FUNCTION RNGAUS(ISEED)
C
C Returns a normally distributed deviate with zero mean and unit 
C variance. The routine uses the Box-Muller transformation of uniform
C deviates. Reference: Press et al., Numerical Recipes, Sec. 7.2.
C
C Arguments:
C  ISEED  (in/out) : seed used for RAN random-number generator.
C
C Subroutines required:
C  RAN -- return a uniform random deviate between 0 and 1.
C
C History:
C  1987 Nov 13 - TJP.
C-----------------------------------------------------------------------
      INTEGER ISEED
      INTEGER ISET
      REAL R, V1, V2, FAC, GSET
      REAL RAN
C
      SAVE GSET, ISET
      DATA ISET / 0 /
      IF (ISET .EQ. 0) THEN
   10 V1 = (2. * RAN(ISEED)) - 1.
      V2 = (2. * RAN(ISEED)) - 1.
      R = (V1 ** 2) + (V2 ** 2)
      IF (R .GE. 1.) GOTO 10
      FAC = SQRT(- ((2. * LOG(R)) / R))
      GSET = V1 * FAC
      RNGAUS = V2 * FAC
      ISET = 1
      ELSE
      RNGAUS = GSET
      ISET = 0
C
      END IF
      END
      SUBROUTINE PGEX13( )
C-----------------------------------------------------------------------
C      Test program for PGPLOT plotting package
C      T. J. Pearson  1982 July 1
C-----------------------------------------------------------------------
      INTEGER I
      REAL XR(720),YR(720)
C-----------------------------------------------------------------------
C Color index:
      INTEGER BLACK, WHITE, RED, GREEN, BLUE, CYAN, MAGENT, YELLOW
      PARAMETER (BLACK=0)
      PARAMETER (WHITE=1)
      PARAMETER (RED=2)
      PARAMETER (GREEN=3)
      PARAMETER (BLUE=4)
      PARAMETER (CYAN=5)
      PARAMETER (MAGENT=6)
      PARAMETER (YELLOW=7)
C Line style:
      INTEGER FULL, DASHED, DOTDSH, DOTTED, FANCY
      PARAMETER (FULL=1)
      PARAMETER (DASHED=2)
      PARAMETER (DOTDSH=3)
      PARAMETER (DOTTED=4)
      PARAMETER (FANCY=5)
C Character font:
      INTEGER NORMAL, ROMAN, ITALIC, SCRIPT
      PARAMETER (NORMAL=1)
      PARAMETER (ROMAN=2)
      PARAMETER (ITALIC=3)
      PARAMETER (SCRIPT=4)
C Fill-area style:
      INTEGER SOLID, HOLLOW
      PARAMETER (SOLID=1)
      PARAMETER (HOLLOW=2)
C-----------------------------------------------------------------------
      CALL PGSCI(CYAN)
      CALL PGENV(0.,720.,-2.0,2.0,0,1)
      CALL PGSCI(GREEN)
      CALL PGLAB('x (degrees)',' ','TRIGONOMETRIC FUNCTIONS')
      DO 10 I=1,360
          XR(I) = 2.0*I
          YR(I) = SIN(XR(I)/57.29577951)
   10 CONTINUE
      CALL PGSCI(MAGENT)
      CALL PGSLS(DASHED)
      CALL PGLAB(' ','- sin(x)         -',' ')
      CALL PGLINE(360,XR,YR)
      DO 20 I=1,360
          XR(I) = 2.0*I
          YR(I) = COS(XR(I)/57.29577951)
   20 CONTINUE
      CALL PGSCI(YELLOW)
      CALL PGLAB(' ','-         cos(x) -',' ')
      CALL PGSLS(DOTTED)
      CALL PGLINE(360,XR,YR)
      RETURN
      END
      SUBROUTINE PGEX14()
C----------------------------------------------------------------------
C Demonstration program for the PGPLOT plotting package.  
C This is a line-drawing test; it draws a regular n-gon joining
C each vertex to every other vertex. It is not optimized for pen
C plotters.
C                              T. J. Pearson  1984 Dec 26
C----------------------------------------------------------------------
      INTEGER I, J, NV, USERINT
      REAL A, D, X(100), Y(100)
C
C Call PGBEG to select the output device. PGPLOT is initialized so 
C that it buffers output, does not prompt when starting a new page, 
C and uses zero character size (so that there is no margin around the
C viewport). The color indices are changed to something more
C interesting than black on white.
C
      CALL PGSCH(0.0)
      CALL PGASK(.FALSE.)
      CALL PGSCR(0,0.2,0.3,0.3)
      CALL PGSCR(1,1.0,0.5,0.2)
      CALL PGSCR(2,0.2,0.5,1.0)
C
C Ask the user to specify the number of vertices, and compute the 
C coordinates for unit circumradius.
C
   10 I = USERINT(NV,1,0,'VERTICES=',
     1       'Number of vertices (2-100, 0 to exit)')
      CALL CANCEL('VERTICES=')
      IF (NV.GT.100.OR. NV.LT.2) GOTO 50
      D = 360.0/NV
      A = -D
      DO 20 I=1,NV
          A = A+D
          X(I) = COS(A/57.29577951)
          Y(I) = SIN(A/57.29577951)
   20 CONTINUE
C
C Select a square viewport.
C
      CALL PGBBUF
      CALL PGSCI(2)
      CALL PGENV(-1.05,1.05,-1.05,1.05,1,-1)
      CALL PGSCI(1)
C
C Draw the polygon.
C
      DO 40 I=1,NV-1
          DO 30 J=I+1,NV
            CALL PGMOVE(X(I),Y(I))
            CALL PGDRAW(X(J),Y(J))
   30     CONTINUE
   40 CONTINUE
C
C Flush the buffer and request a new number of vertices.
C
      CALL PGEBUF
      CALL PGUPDT
      GOTO 10
C
C End of program.
C
   50 RETURN
      END
      SUBROUTINE PGEX15( )
C-----------------------------------------------------------------------
C Test program for PGPLOT: test of Cursor
C-----------------------------------------------------------------------
      CHARACTER*1 CH
      CHARACTER*80 TEXT
      REAL X,Y
C
C Open device for graphics.
C
C
C Clear the screen. Draw a frame at the physical extremities of the
C plot, using full-screen viewport and standard window.
C
      CALL PGPAGE
      CALL PGSVP(0.0,1.0,0.0,1.0)
      CALL PGSWIN(0.0,1.0,0.0,1.0)
      CALL PGBOX('bcts',0.1,5,'bcts',0.1,5)
C
C Loop to read and display cursor position. Initial position for cursor
C is center of viewport. 
C
      X = 0.5
      Y = 0.5
   10 CONTINUE
          CALL PGCURS(X,Y,CH)
          WRITE (TEXT, '(2F8.3,I4)') X,Y,ICHAR(CH)
          CALL ANYOUT(0,TEXT)
      IF (CH.NE.'/'.AND. CH.NE.CHAR(0)) GOTO 10
C
C Close the device and exit.
C
      RETURN
      END
      SUBROUTINE PGEX17( )
C-----------------------------------------------------------------------
C Test program for PGPLOT. 
C Author: T. J. Pearson.
C Last modified: 9-Feb-1988
C-----------------------------------------------------------------------
      CHARACTER*128 DEVICE
      CHARACTER*80 TEXT
      CHARACTER*80 GTYPE, GVER
      INTEGER I, J, L, L1, L2
      REAL X, X1, X2, Y, Y1, Y2, R, XI, XP, YP
      REAL PX(43), PY(43)
      DATA PX / 0.0,2.0,4.0,6.0,8.0,10.0,12.0,14.0,16.4,17.0,17.3,
     1          17.8, 18.5, 20.0, 22.0, 24.0, 26.0, 28.0, 29.0,
     2          28.8,27.2,25.0,23.0,21.5,21.1,21.5,22.8, 24.1, 25.1,
     3          25.2, 24.2, 22.1, 20.0, 18.0, 16.0, 14.0, 12.0,
     4          10.0,  8.0,  6.1,  4.2,  3.0,  1.3 /
      DATA PY / 8.8, 7.6, 7.1, 7.4, 8.0, 8.9, 9.6, 9.9, 9.4,
     1          9.7, 12.0, 14.0, 16.1, 17.0, 17.0, 16.0, 13.9,
     2          13.1, 13.2, 12.3, 11.5, 11.5, 11.5, 11.2, 10.5,
     3          9.0, 8.0, 7.0, 5.1, 3.6, 1.9, 1.1, 0.9, 0.7,
     4          0.8, 1.0, 1.0, 1.2, 1.8, 2.1, 2.9, 4.1, 6.0 /
C
C Open device for graphics
C
Cvms  CALL LIB$INIT_TIMER
      CALL PGQINF('DEV/TYPE', DEVICE, L)
      CALL PGQINF('VERSION', GVER, L1)
      CALL PGQINF('TYPE', GTYPE, L2)
      CALL PGBBUF
C
C Clear the screen; set background color.
C
      CALL PGPAGE
      CALL PGSCR(0,0.0,0.0,0.35)
C
C Draw a frame at the physical extremities of the plot.
C Dimensions are X by Y (inches).
C
      CALL PGSVP(0.0, 1.0, 0.0, 1.0)
      CALL PGQVP(1, X1, X2, Y1, Y2)
      X = X2-X1
      Y = Y2-Y1
      CALL PGSWIN(0.0, X, 0.0, Y)
      CALL PGSFS(2)
      CALL PGRECT(0.0, X, 0.0, Y)
      CALL PGMOVE(0.5*X, 0.0)
      CALL PGDRAW(0.5*X, Y)
      CALL PGMOVE(0.0, 0.5*Y)
      CALL PGDRAW(X, 0.5*Y)
C
C Draw a circle of diameter 0.5 x min(x,y)
C
      R = 0.25*MIN(X,Y)
      CALL PGCIRC(X*0.5, Y*0.5, R)
C
C Draw some more circles with different line-styles; this tests
C the dashing algorithm on curved lines.
C
      CALL PGSLS(2)
      CALL PGCIRC(X*0.5, Y*0.5, R*1.1)
      CALL PGSLS(3)
      CALL PGCIRC(X*0.5, Y*0.5, R*1.2)
      CALL PGSLS(2)
      CALL PGSLW(3)
      CALL PGCIRC(X*0.5, Y*0.5, R*1.3)
      CALL PGSLS(1)
      CALL PGSLW(1)      
C
C Demonstrate different line-styles
C
      DO 10 I=1,5
          CALL PGSLS(I)
          CALL PGMOVE(I*(X/20.0),0.0)
          CALL PGDRAW(I*(X/20.0),Y)
   10 CONTINUE
      CALL PGSLS(1)
C
C Demonstrate different line-widths
C
      DO 20 I=1,5
          CALL PGSLW(I)
          CALL PGMOVE(0.0, I*(Y/20.0))
          CALL PGDRAW(X, I*(Y/20.0))
   20 CONTINUE
      CALL PGSLW(1)
C
C Draw dots in different thicknesses.
C
      DO 30 I=1,21
          XP = 6*Y/20.0
          YP = I*Y/22.0
          CALL PGSLW(I)
          CALL PGPT(1,XP,YP,-1)
   30 CONTINUE
C
C Demonstrate different line-colors
C
      CALL PGSLW(4)
      DO 40 I=0,15
          CALL PGSCI(I)
          XI = (I+20)*(X/40.0)
          CALL PGMOVE(XI,0.0)
          CALL PGDRAW(XI,Y)
   40 CONTINUE
      CALL PGSCI(1)
      CALL PGSLW(1)
C
C Demonstrate fill area
C
      DO 50 J=1,43
         PX(J) = (PX(J)+50.0)/100.0*X
         PY(J) = (PY(J)+75.0)/100.0*Y
   50 CONTINUE
      DO 70 I=0,3
          CALL PGSCI(I)
          CALL PGSFS(1)
          CALL PGPOLY(43,PX,PY)
          CALL PGSCI(1)
          CALL PGSFS(2)
          CALL PGPOLY(43,PX,PY)
          DO 60 J=1,43
             PY(J) = PY(J)-0.25*Y
   60     CONTINUE
   70 CONTINUE
C
C Write the device type on the plot.
C
      CALL PGSWIN(0.0, 1.0, 0.0, 1.0)
      CALL PGSFS(1)
      CALL PGSCI(0)
      CALL PGRECT(0.31, 1.0-0.31, 0.85, 0.97)
      CALL PGSCI(1)
      CALL PGSFS(2)
      CALL PGRECT(0.31, 1.0-0.31, 0.85, 0.97)
      CALL PGPTXT(0.5, 0.91, 0.0, 0.5, 'PGPLOT '//GVER(1:L1))
      CALL PGPTXT(0.5, 0.87, 0.0, 0.5, 'Device '//GTYPE(1:L2))
C
C Report dimensions.
C
      WRITE(TEXT,'('' Device                   : '',A)') DEVICE(1:L)
      CALL ANYOUT(0,TEXT)
      WRITE(TEXT,'('' Plot dimensions  (inches): '',2F9.2)') X, Y
      CALL ANYOUT(0,TEXT)
      WRITE(TEXT,'('' Radius of circle (inches): '',F9.2)') R
      CALL ANYOUT(0,TEXT)
Cvms  CALL LIB$SHOW_TIMER
C
C and exit.
C
      RETURN
      END

      SUBROUTINE PGCIRC(X0, Y0, R)
      REAL X0, Y0, R
C
C Draw a circle of radius R, center (X0, Y0) [world coordinates].
C
      REAL A, XI, YI

      CALL PGMOVE(X0+R,Y0)
      DO 10 A=0.,360.,1.0
         XI = X0 + R*COS(A/57.29577951)
         YI = Y0 + R*SIN(A/57.29577951)
         CALL PGDRAW(XI,YI)
   10 CONTINUE
      END
      SUBROUTINE PGEX18( )
C-----------------------------------------------------------------------
C Demonstration program for PGPLOT. This programs shows the use of
C routine PGOLIN to allow the user to draw polygons on the screen.
C As each polygon is completed, it is filled in using routine PGPOLY.
C The user positions the cursor to define the vertices of the polygon.
C He types 'A' to add a vertex at the current cursor position, 'D' to
C delete the nearest vertex, or 'X' to signal that the polygon is 
C complete. Two 'X's in succession terminates the program.
C-----------------------------------------------------------------------
      INTEGER MAXPT, NPT, COL, USERINT
      PARAMETER (MAXPT=50)
      REAL X(MAXPT),Y(MAXPT)
      INTEGER WHICH
C
      CALL ANYOUT(0,'PGEX18 - demonstration of PGPLOT cursor routines')
      CALL ANYOUT(0,'PGLCUR and PGOLIN.')
      CALL ANYOUT(0,'These routines allow you to draw polygons on the')
      CALL ANYOUT(0,'screen, using the cursor to mark the vertices.')
   10 CALL ANYOUT(0,'PGLCUR outlines the polygon as you draw it,')
      CALL ANYOUT(0,'PGOLIN just marks the vertices.')
      CALL ANYOUT(0,'Which routine do you want to use?')
      CALL ANYOUT(0,'Type 1 for PGLCUR, 2 for PGOLIN:')
      I = USERINT(WHICH,1,0,'ROUTINE=','1=PGLCUR,2=PGOLIN')
      CALL CANCEL('ROUTINE=')
      IF (WHICH.LT.1 .OR. WHICH.GT.2) GOTO 10
C
C Clear the screen. Draw a frame at the physical extremities of the
C view surface using full-screen viewport and standard window.
C
      CALL PGPAGE
      CALL PGSVP(0.0,1.0,0.0,1.0)
      CALL PGSWIN(0.0,1.0,0.0,1.0)
      CALL PGBOX('BC',0.1,5,'BC',0.1,5)
      COL = 0
      CALL ANYOUT(0,'Use the cursor to choose the vertices of'//
     1   ' the polygon')
      CALL ANYOUT(0,'Type A to add a vertex at the cursor position')
      CALL ANYOUT(0,'Type D to delete the last vertex')
      CALL ANYOUT(0,'Type X to close the polygon and shade it')
      CALL ANYOUT(0,'(Type X again to exit from the program)')
C
C Increment the color index and then call PGOLIN to allow the user
C to draw a polygon.
C
   20 COL = COL+1
      CALL PGSCI(COL)
      NPT = 0
      IF (WHICH.EQ.1) CALL PGLCUR(MAXPT,NPT,X,Y)
      IF (WHICH.EQ.2) CALL PGOLIN(MAXPT,NPT,X,Y,-1)
C
C Fill the interior of the polygon in the current color. If less
C than three vertices were supplied, that is a signal to terminate
C the program.  Otherwise, go back and draw another polygon.
C
      IF (NPT.GE.3) THEN
           CALL PGPOLY(NPT,X,Y)
           GOTO 20
       END IF 
C
C Close the device and exit.
C
      RETURN
      END
