/* showsub.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            showsub.dc2

Document:     SHOWSUB

Purpose:      Describes the available routines which display
              Set and subset information in the Task Status Area.

File:         showsub.c

Author:       K.G. Begeman

Description:  The routines which display set and subset info in the
              Task Status Area are:
              SUBROUTINE SHOWSUB1    displays the name of the input
                                     set and the subset coordinates
              SUBROUTINE SHOWSUB2    displays tne name of input and
                                     output set and the input and
                                     output subset coordinates.

Updates:      Feb 20, 1990: KGB, Document created.

#<
*/

#include "stdio.h"
#include "string.h"
#include "gipsyc.h"
#include "nelc.h"
#include "status.h"
#include "gdsc_grid.h"
#include "gdsc_name.h"
#include "gdsc_ndims.h"

#define MAXCHR      100     /* maximum number of characters in display buffer */
#define MAXAXNAMLEN  18 /* maximum number of characters an axis name can have */

static void put_subset_coordinates( char  *message,
                                    fchar  set    ,
                                    fint  *subset ,
                                    fint  *axperm )
{
   char b[MAXCHR+1];
   fint l;
   fint n;
   fint setdim;
   fint subdim;
   fint zero = 0;

   l = strlen( message );
   setdim = gdsc_ndims_c( set, &zero );                   /* dimension of set */
   subdim = gdsc_ndims_c( set, subset );               /* dimension of subset */
   for (n = subdim; n < setdim; n++) {
      char   axis_b[MAXAXNAMLEN+1];
      char   *ptr;
      fchar  ctype;
      fint   cerror = 0;
      fint   grid;

      ctype.a = axis_b; ctype.l = MAXAXNAMLEN; axis_b[MAXAXNAMLEN] = 0;
      gdsc_name_c( ctype, set, &axperm[n], &cerror );
      grid = gdsc_grid_c( set, &axperm[n], subset, &cerror );
      ptr = strtok( axis_b, "- " );
      if (( n + 1 ) == setdim) {
         sprintf( b, "%s=%d ", axis_b, grid );
      } else {
         sprintf( b, "%s=%d,", axis_b, grid );
      }
      l += strlen( b );
      if (l < MAXCHR) strcat( message, b );
   }
}

/*
#>            showsub1.dc2

Subroutine:   SHOWSUB1

Purpose:      Shows the user (in status line) which subset the
              program is working on.

File:         showsub.c

Author:       K.G. Begeman

Use:          CALL SHOWSUB1( SET    ,  Input   CHARACTER*(*)
                             SUBSET ,  Input   INTEGER
                             AXPERM )  Input   INTEGER ARRAY

              SET            Name of GDS set.
              SUBSET         Subset coordinate word.
              AXPERM         Array containing axis sequence
                             numbers (returned by GDSINP).

Example:      FOR N = 1, NSUB
                 CALL SHOWSUB1( SET, SUBSET(N), AXPERM )
                 REPEAT
                    CALL GDSI_READ( SET, CW1, CW2, .. , ERR )
                    ............
                 UNTIL (ERR .EQ. 0)
              CFOR

Updates:      Oct 25, 1988: KGB, Document created.

#<

Fortran to C interface:

@ subroutine showsub1( character, integer, integer )

*/

void showsub1_c( fchar set, fint *subset, fint *axperm )
{
   char  b[MAXCHR+1];
   char  format[20];
   char  message[MAXCHR+1];
   fint  l;                                     /* length of character string */
   fint  lset = nelc_c( set );            /* number of characters in set name */

   strcpy( message, "Working on set " );
   l = strlen( message );
   sprintf( format, "%%.%ds ", ( lset > MAXCHR - l ? MAXCHR - l : lset ) );
   sprintf( b, format, set.a );
   strcat( message, b );
   put_subset_coordinates( message, set, subset, axperm );
   status_c( tofchar( message ) );
}

/*
#>            showsub2.dc2

Subroutine:   SHOWSUB2

Purpose:      Shows the user (in status line) the input subset
              and output subset the program is working on.

File:         showsub.c

Author:       K.G. Begeman

Use:          CALL SHOWSUB2( SET1    ,  Input   CHARACTER*(*)
                             SUBSET1 ,  Input   INTEGER
                             AXPERM1 ,  Input   INTEGER ARRAY
                             SET2    ,  Input   CHARACTER*(*)
                             SUBSET2 ,  Input   INTEGER
                             AXPERM2 )  Input   INTEGER ARRAY

              SET1           Name of GDS input set.
              SUBSET1        Input subset coordinate word.
              AXPERM1        Array with axis sequence numbers returned
                             by GDSINP.
              SET2           Name of GDS output set.
              SUBSET2        Output subset coordinate word.
              AXPERM2        Array with axis sequence numbers returned
                             by GDSOUT.

Example:      FOR N = 1, NSUB
                 CALL SHOWSUB2( SET1, SUBSET1(N), AXPERM1,
                                SET2, SUBSET2(N), AXPERM2 )
                 REPEAT
                    CALL GDSI_READ( SET1, CW1, CW2, .. , ERR )
                    ............
                    CALL GDSI_WRITE( SET2, CW1, CW2, .., ERR )
                 UNTIL (ERR .EQ. 0)
              CFOR

Updates:      Feb 20, 1988: KGB, Document created.

#<

Fortran to C interface:

@ subroutine showsub2( character, integer, integer,
@                      character, integer, integer )

*/

void showsub2_c( fchar  set1    ,
                 fint  *subset1 ,
                 fint  *axperm1 ,
                 fchar  set2    ,
                 fint  *subset2 ,
                 fint  *axperm2 )
{
   char  b[MAXCHR+1];
   char  format[20];
   char  message[MAXCHR+1];
   fint  l;                                     /* length of character string */
   fint  lset1 = nelc_c( set1 );          /* number of characters in set name */
   fint  lset2 = nelc_c( set2 );          /* number of characters in set name */

   strcpy( message, "Set " );
   l = strlen( message );
   sprintf( format, "%%.%ds ", ( lset1 > MAXCHR - l ? MAXCHR - l : lset1 ) );
   sprintf( b, format, set1.a );
   strcat( message, b );
   put_subset_coordinates( message, set1, subset1, axperm1 );
   l = strlen( message );
   strcpy( b, "to Set " );
   l += strlen( b );
   if (l < MAXCHR) {
      strcat( message, b );
      sprintf( format, "%%.%ds ", ( lset2 > MAXCHR - l ? MAXCHR - l : lset2 ) );
      sprintf( b, format, set2.a );
      strcat( message, b );
   }
   put_subset_coordinates( message, set2, subset2, axperm2 );
   status_c( tofchar( message ) );
}


