/* minmax.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.


#>            minmax.dc2

Document:     MINMAX

Purpose:      Describes the available routines which find the minimum
              and maximum in real arrays.

File:         minmax.c

Author:       K.G. Begeman

Description:  The available routines are the following:
              MINMAX1           Determines minimum and maximum in a
                                real array.
              MINMAX2           Same as MINMAX1, but also counts
                                number of BLANKS.
              MINMAX3           Same as MINMAX2, but determines the
                                running min. and max. and number of
                                BLANKS.
              MINMAX4           Same as MINMAX3, but also determines
                                position of min. and. max.

Updates:      Jul 22, 1989: KGB, Document created.
                                
#<

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"setfblank.h"		/* define setfblank_c */

/*
#>            minmax1.dc2

Subroutine:   MINMAX1

Purpose:      Finds the minimum and maximum value in a data
              array of reals taking care of BLANK values.

File:         minmax.c

Author:       K.G. Begeman

Use:          CALL MINMAX1( DATA ,     Input     REAL ARRAY
                            NDAT ,     Input     INTEGER
                            AMIN ,     Output    REAL
                            AMAX )     Output    REAL

              DATA          Data array to operate on.
              NDAT          Number of data points in DATA.
              AMIN          Minimum value in DATA array.
              AMAX          Maximum value in DATA array.

Note:         If all values in the data array have the BLANK value,
              the minimum and maximum will also be set to BLANK.

Updates:      Jul 22, 1989: KGB, Document created
              AUg  8, 1990: KGB, Bug in loop counter fixed

#<

@ subroutine minmax1( real, integer, real, real )

*/

void minmax1_c( float *data, fint *n, float *amin, float *amax )
{
   fint   def = 0;			/* define mode or not */
   fint8   l;				/* loop counter */
   float  blank;			/* BLANK value */

   setfblank_c( amin );			/* set to blank, as initial value */
   setfblank_c( amax );			/* set to blank, as initial value */
   setfblank_c( &blank );		/* set to blank for comparison */
   for (l = 0; l < *n; l++, data++) {	/* loop */
      if (*data != blank) {		/* no BLANK value */
         if (def) {			/* min/max already defined */
            if (*amin > *data) {	/* new minimum ? */
               *amin = *data;		/* save new minimum */
            } else if (*amax < *data) {	/* new maximum ? */
               *amax = *data;		/* save new maximum */
            }
         } else {			/* define min and max */
            *amin = *amax = *data;	/* save new minimum and maximum */
            def = 1;			/* turn on define mode */
         }
      }
   }
}

/*
#>            minmax2.dc2

Subroutine:   MINMAX2

Purpose:      Finds the minimum and maximum value in a data
              array of reals, counting the BLANK values.

File:         minmax.c

Author:       K.G. Begeman

Use:          CALL MINMAX2( DATA   ,  Input      REAL ARRAY
                            NDAT   ,  Input      INTEGER
                            AMIN   ,  Output     REAL
                            AMAX   ,  Output     REAL
                            NBLANK )  Output     INTEGER

              DATA          Data array to operate on.
              NDAT          Number of data points in DATA array.
              AMIN          Minimum value in DATA array.
              AMAX          Maximum value in DATA array.
              NBLANK        Number of blanks in DATA array.

Note:         If all values in the data array have the BLANK value,
              the minimum and maximum will also be set to BLANK.

Updates:      Jul 22, 1989: KGB, Document created.
              AUg  8, 1990: KGB, Bug in loop counter fixed

#<

@ subroutine minmax2( real, integer, real, real, integer )

*/

void minmax2_c( float *data, fint *n, float *amin, float *amax, fint *nblank )
{
   fint   count = 0;			/* BLANK count */
   fint   def = 0;			/* define mode or not */
   fint   l;				/* loop counter */
   float  blank;			/* BLANK value */

   setfblank_c( amin );			/* initialize to BLANK */
   setfblank_c( amax );			/* initialize to BLANK */
   setfblank_c( &blank );		/* local BLANK value */
   for (l = 0; l++ < *n; data++) {	/* loop */
      if (blank == *data) {		/* data is BLANK */
         count++;			/* increase BLANK counter */
      } else if (def) {			/* min and amx already defined */
         if (*amin > *data) {		/* new minimum */
            *amin = *data;		/* save new minimum */
         } else if (*amax < *data) {	/* new maximum */
            *amax = *data;		/* save new maximum */
         }
      } else {				/* min and max not yet defined */
         *amin = *amax = *data;		/* save new minimum and maximum */
         def = 1;			/* turn on defined mode */
      }
   }
   *nblank = count;			/* return BLANK count */
}

/*
#>            minmax3.dc2

Subroutine:   MINMAX3

Purpose:      Finds the minimum and maximum value in a data
              array of reals, counting the BLANK values. Especially
              for finding the minimum and maximum in a subset.

File:         minmax.c

Author:       K.G. Begeman

Use:          CALL MINMAX3( DATA  ,    Input       REAL ARRAY
                            NDAT  ,    Input       INTEGER
                            AMIN  ,  In/Output     REAL
                            AMAX  ,  In/Output     REAL
                            NBLANK,  In/Output     INTEGER
                            COUNT )  In/Output     INTEGER

              DATA          Data array to operate on.
              NDAT          Number of data points in DATA.
              AMIN          Minimum value.
              AMAX          maximum value.
              NBLANK        Total number of blanks.
              COUNT         On input, the number of data points
                            checked so far, on output the number
                            of data points checked.

Example:      COUNT = 0
              REPEAT
                 CALL GDSI_READ(SET,C1,C2,A,SIZE,N,I_ERR)
                 CALL MINMAX3(A,N,AMIN,AMAX,NBLANK,COUNT)
              UNTIL (I_ERR .EQ. 0)
              WRITE(*,*) ' The number of blanks is:',NBLANK
              etc.

Note:         If all values in the data array have the BLANK value,
              the minimum and maximum will also be set to BLANK.

Updates:      Jul 22, 1989: KGB, Document created.

#<

@ subroutine minmax3( real, integer, real, real, integer, integer )

*/

void minmax3_c( float *data, fint *n, float *amin, float *amax,
                fint *nblank, fint *count )
{
   fint   def;				/* define mode or not */
   fint   k = 0;			/* partial BLANK counter */
   fint   l;				/* loop counter */
   float  blank;			/* local BLANK value */

   setfblank_c( &blank );		/* obtain BLANK value */
   if (*count == 0) {			/* initialize */
      *amax = *amin = blank;		/* initialize minimum and maximum */
      *nblank = 0;			/* and the number of BLANKS */
   }
   def = (blank != *amin);		/* define mode ? */
   for (l = 0; l++ < *n ; data++) {	/* loop */
      if (blank == *data) {		/* data is BLANK */
         k++;				/* count BLANKS */
      } else if (def) {			/* min and max already defined */
         if (*amin > *data) {		/* new minimum */
            *amin = *data;		/* save new minimum */
         } else if (*amax < *data) {	/* new maximum */
            *amax = *data;		/* save new maximum */
         }
      } else {				/* min and max not yet defined */
         *amax = *amin = *data;		/* save new minimum and maximum */
         def = 1;			/* turn on defined mode */
      }
   }
   *count += *n;			/* return counter */
   *nblank += k;			/* and  number of blanks */
}

/*
#>            minmax4.dc2

Subroutine:   MINMAX4

Purpose:      Finds the minimum and maximum value and their
              positions in a data array of reals, counting the BLANK
              values. Especially for finding the minimum and maximum
              in a subset.

File:         minmax.c

Author:       K.G. Begeman

Use:          CALL MINMAX4( DATA  ,    Input       REAL ARRAY
                            NDAT  ,    Input       INTEGER
                            AMIN  ,  In/Output     REAL
                            AMAX  ,  In/Output     REAL
                            IMIN  ,  In/Output     INTEGER
                            IMAX  ,  In/Output     INTEGER
                            NBLANK,  In/Output     INTEGER
                            COUNT )  In/Output     INTEGER

              DATA          Data array to operate on.
              NDAT          Number of data points in DATA.
              AMIN          Minimum value.
              AMAX          maximum value.
              IMIN          Offset of minimum.
              IMAX          Offset of maximum.
              NBLANK        Total number of blanks.
              COUNT         On input, the number of data points
                            checked so far, on output the number
                            of data points checked.

Example:      COUNT = 0
              REPEAT
                 CALL GDSI_READ(SET,C1,C2,A,SIZE,N,I_ERR)
                 CALL MINMAX4(A,N,AMIN,AMAX,IMIN,IMAX,NBLANK,COUNT)
              UNTIL (I_ERR .EQ. 0)
              WRITE(*,*) ' The number of blanks is:',NBLANK
              etc.

Note:         If all values in the data array have the BLANK value,
              the minimum and maximum will also be set to BLANK.

Updates:      Jul 22, 1989: KGB, Document created.

#<

@ subroutine minmax4( real, integer, real, real,
@                     integer*8, integer*8, integer, integer*8 )

*/

void minmax4_c( float *data, fint *n, float *amin, float *amax,
                fint8 *imin, fint8 *imax, fint *nblank, fint8 *count )
{
   fint   def;				/* define mode or not */
   fint   k = 0;			/* local BLANK counter */
   fint   l;				/* loop counter */
   float  blank;			/* local BLANK value */

   setfblank_c( &blank );		/* obtain local BLANK value */
   if (*count == 0) {			/* initialize */
      *amax = *amin = blank;		/* initial value */
      *nblank = 0;			/* reset */
   }
   def = (blank != *amin);		/* define mode ? */
   for (l = 0; l++ < *n; data++) {	/* loop */
      if (blank == *data) {		/* data is BLANK */
         k++;				/* increase BLANK counter */
      } else if (def) {			/* min and max are defined */
         if (*amin > *data) {		/* new ninimum */
            *amin = *data;		/* save new minimum */
            *imin = *count + l - 1;	/* and its position */
         } else if (*amax < *data) {	/* new maximum */
            *amax = *data;		/* save new maximum */
            *imax = *count + l - 1;	/* and its position */
         }
      } else {
         *amax = *amin = *data;		/* save new minimum and maximum */
         *imin = *imax = *count + l - 1;/* and their positions */
         def = 1;			/* turn on define mode */
      }
   }
   *count += *n;			/* return counter */
   *nblank += k;			/* and number of blanks */
}

#if defined(TESTBED)
main()
{
   float data[20];
   fint  n1 = 20, n2 = 20, n3 = 10, n4 = 10;
   float amin1, amax1, amin2, amax2, amin3, amax3, amin4, amax4;
   fint  nblank2, nblank3, nblank4;
   fint  count3 = 0, count4 = 0, imin4, imax4;
   fint  i;
   for (i = 0; i < 20; i++) {
      data[i] = (float) i;
   }
   setfblank_c( &data[13] );
   minmax1_c( data, &n1, &amin1, &amax1 );
   minmax2_c( data, &n2, &amin2, &amax2, &nblank2 );
   minmax3_c( data, &n3, &amin3, &amax3, &nblank3, &count3 );
   minmax3_c( &data[n3], &n3, &amin3, &amax3, &nblank3, &count3 );
   minmax4_c( data, &n4, &amin4, &amax4, &imin4, &imax4, &nblank4, &count4 );
   minmax4_c( &data[n4], &n4, &amin4, &amax4, &imin4, &imax4, &nblank4, &count4 );
   printf("routine minimum maximum nblank count imin imax\n\n");
   printf("MINMAX1 %7.3f %7.3f ------ ----- ---- ----\n",amin1,amax1);
   printf("MINMAX2 %7.3f %7.3f %6ld ----- ---- ----\n",amin2,amax2,nblank2);
   printf("MINMAX3 %7.3f %7.3f %6ld %5ld ---- ----\n",amin3,amax3,nblank3,count3);
   printf("MINMAX4 %7.3f %7.3f %6ld %5ld %4ld %4ld\n",amin4,amax4,nblank4,count4,imin4,imax4);
}
#endif
