/*
                            COPYRIGHT (c) 1995
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             pplot.dc1

Program:       pplot.c

Purpose:       Plot the intensity along a line through a set.

Category:      UTILITY

File:          pplot.c

Author:        M.G.R. Vogelaar

Keywords:


   GRDEVICE=   Graphics device              [list of all graphics devices]


   INSET=      Give a (sub)set to extract a profile:

               Examples:
               You want a profile in frequency (velocity) direction:
               The set n3198h has axes RA, DEC, FREQ. Then the input
               can be:

               1) INSET=n3198h
                  and you are prompted to give:
                  Profile from (RA1,DEC1,FREQ1) to (RA2,DEC2,FREQ2):
                  (with RA1=RA2, DEC1=DEC2)
               2) INSET=n3198h f 10:100
                  Because you entered a profile direction you are prompted:
                  Give position in (RA,DEC):
               3) INSET=n3198h ra 0 dec 0
                  You specified a profile in frequency direction:
                  Profile from (FREQ1) to (FREQ2):


    CHARHEIGHT= Give character height in mm:                        [4.0]



               The following keywords are asked in a loop which is
               aborted after you pressed carriage return at the PROFILE=
               prompt.


    PROFILE=   Profile from (...) to (...):                        [none]
               or:
               Give position in (....):                            [none]

               The prompt that you get depends on the INSET= keyword.
               If you specified a profile direction (e.g. INSET=n3198h f)
               then you are asked to enter a position to locate the
               profile with: Give position in (....): where the text
               between parentheses shows which grids need to be
               specified. Else, the profile needs to be entered as
               two vectors in a subset. The prompt is:
               Profile from (...) to (...): where the text between
               parentheses gives the input order.
               Pressing carriage return will abort the loop in which
               keyword PROFILE= is asked.


    PROFINT=   Integration sizes in (....):              [no integration]

               Integrate profile along the subset axes. The text
               between the parentheses will show in which directions
               you can integrate. The numbers that you enter are sizes
               of a box centered around a profile position according to
               the formulas: gridlo = cpos - PROFINT/2 and
               gridhi = cpos + PROFINT/2
               The integration is an average in the direction of a
               (sub)set axis.

    XRANGE=    Give grid range in x:                         [calculated]


    YRANGE=    Give grid range in y:                         [calculated]


    XSIZE=     Size of plot in x (mm):                            [120.0]


    YSIZE=     Size of plot in y (mm):                             [80.0]


    LOCATION=  Lower left corner of plot (mm,mm):                 [30,35]


    FILENAME=  Write profile data to file:                      [No file]

               If a file is given, then the data is written to
               an Ascii file on disk with name as in FILENAME=
               The default skips this process. The keyword is not
               cancelled in the loop.
               The file will contain 2 columns. The first contains
               the positions in the profile in grids (or, if possible,
               in physical coordinates). The second column contains
               the profile data in 'BUNIT' units.
               If the profile was not aligned to one of the set axes,
               then the first column will contain index numbers only.
               If the profile positions are physical coordinates,
               then the values are in units as found in the header
               of the set (CUNITn or DUNITn).


               After you pressed carriage return at the PROFILE=
               prompt you get a choice to end the program or to
               enter an interactive plot mode:

     GPLOT=    Go to General Plot Command mode?                     Y/[N]




Description:   PPLOT plots the intensity along a line through a set.
               The line must have a starting point and an end point (to
               be able to plot profiles in any direction through your set)
               or a starting point and a direction aligned with one of the
               axes of the input set. Therefore the profile direction is
               given by a combination of the keywords INSET= and PROFILE=
               For example: A set n3198h has axes RA, DEC, FREQ.
               then:
            1) INSET=n3198h
               prompts with: Profile from (RA1,DEC1,FREQ1) to (RA2,DEC2,FREQ2):
               enter:
               a) PROFILE=0 0 3 0 0 76     (RA1=RA2, DEC1=DEC2)
                  is a profile from freq 3 to freq 76
               b) PROFILE=0 0 400 km/s 0 0 700 km/s
                  same as a)
               c) PROFILE=0 -100 3 0 100 76
                  is a profile through the cube and is not aligned with one
                  of the set axes. Instead of labels with physical coordinates,
                  only grids will be plotted.

            2) INSET=n3198h f 3:76 80
               Because you entered a profile direction you are prompted:
               Give position in (RA,DEC):
               enter:
               POSITION=0 0
               and a profile in the frequency direction is sampled at
               grids 3 to 76 and 80! The RA,DEC position is 0,0 for all
               frequency grids. Note that this combination of keywords
               allows you to non-contiguous sampling.

            3) INSET=n3198h ra 0 dec 0
               You specified a profile in frequency direction at position
               RA,DEC = 0,0. You can expect the prompt:
               Profile from (FREQ1) to (FREQ2):
               enter:
               PROFILE=3 76
               or:
               PROFILE=400 km/s 700 km/s
               Note that for this INSET= specification, that the range in
               frequencies must be contiguous.

            4) INSET=n3198h f 4
               This is a situation in which you specified just ONE subset.
               The program recognizes this and will prompt with:
               Profile from (RA1,DEC1) to (RA2,DEC2):

               The  PROFINT= keyword is used to average profiles. The inte-
               gration is always in the direction of your subset axes. These
               axes are listed in the PROFINT= prompt. Instead of using one
               pixel per profile sample, all pixels in a subset box are summed
               and an average is returned.

Notes:         .......

Example:       Example of a default file which creates a panel with 4
               profiles and an integrated profile in the upper left corner:

               GRDEVICE=x11
               CHARHEIGHT=3
               INSET=n3198h f
               XRANGE=;;;;;
               YRANGE=;;;;;
               PROFINT=; ; ; ;3 3;
               POSITION=-1 -1;0 0;1 -1;-1 1;0 0;
               LOCATION=30 35;115 35;30 110;115 110;30 185
               XSIZE=50;;;;
               YSIZE=30;;;;
               FILENAME=
               ERASE=n;n;n;n;n
               GPLOT=


Updates:       Mar 21,  1995: VOG, Document created.

#<
*/

/*  pplot.c: include files     */

#include    "stdio.h"        /* Defines ANSI C input and output utilities */
#include    "stdlib.h"       /* Defines the ANSI C functions for number */
                             /* conversion, storage allocation, and similar tasks */
                             /* Defines EXIT_FAILURE and EXIT_SUCCESS */
#include    "cmain.h"        /* Defines the main body of a C program with */
                             /* MAIN_PROGRAM_ENTRY and IDENTIFICATION */
#include    "gipsyc.h"       /* Defines the ANSI-F77 types for Fortran to C interface */
                             /* including def. of char2str,str2char,tofchar,zadd */
                             /* and macros tobool and toflog */

/* Common includes */

#include    "init.h"         /* Declare task running to HERMES and initialize.*/
#include    "finis.h"        /* Informs HERMES that servant quits and cleans up the mess.*/
#include    "deputy.h"
#include    "wkey.h"
#include    "userfio.h"


MAIN_PROGRAM_ENTRY
{
   fint    status;

   init_c();                               /* contact Hermes */
   wkey_c( tofchar("COMMAND=pplot") );
   deputy_c( tofchar("GPLOT"), &status );
   if (status ==  5)
      anyoutf( 1, "called task disconnected");
   if (status == -6)
      anyoutf( 1, "task not present");
   if (status == -7)
      anyoutf( 1, "max number of tasks already active");
   if (status !=  1)
   {
      finis_c();
      return( EXIT_FAILURE );
   }

   finis_c();
   return( EXIT_SUCCESS );
}
