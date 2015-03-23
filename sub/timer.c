/* timer.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            timer.dc2

Function:     TIMER

Purpose:      Returns the cpu time and real time.

Category:     UTILITY

File:         timer.c

Author:       K.G. Begeman

Use:          CALL TIMER( CT ,       Input/Output   DOUBLE PRECISION
                          RT ,       Input/Output   DOUBLE PRECISION
                          MODE )        Input       INTEGER

              CT          Cpu time (in seconds) used since start of
                          programme (MODE=0) or CPU time used since
                          last call to TIMER (MODE<>0).
              RT          Real time (in seconds) since Jan 1 1970
                          (MODE=0) or real time used since last call
                          to TIMER (MODE<>0).
              MODE        If MODE=0 the current cpu time and real time
                          are returned otherwize the elapsed times are
                          returned.

Example:      PROGRAM TEST
              CHARACTER*80     MES
              DOUBLE PRECISION CT, RT
              ......
              CALL INIT
              CALL TIMER(CT,RT,0)
              ......
              ......
              CALL TIMER(CT,RT,1)
              WRITE(MES,'(''CPU TIME  :'',F10.4,'' SEC'')') CT
              CALL ANYOUT(3,MES)
              WRITE(MES,'(''REAL TIME :'',F10.4,'' SEC'')') RT
              CALL ANYOUT(3,MES)
              CALL FINIS
              STOP
              END

Updates:      Aug 17, 1991: KGB, Document created.
              Jul  7, 1999: JPT, Use CLOCKS_PER_SEC instead of CLK_TCK.
              May  4, 2007: JPT, Include conditional code for Apple Mac.

#<

Fortran to C interface:

@ subroutine timer( double precision, double precision, integer )

*/

#include	"stdio.h"			/* <stdio.h> */
#include	"stddef.h"			/* <stddef.h> */
#include	"time.h"			/* <time.h> */
#include	"gipsyc.h"			/* GIPSY definitions */

#if	defined(__sysv__) | defined(__APPLE__)

#include	<sys/time.h>

#elif	defined(__bsd__)

#define		ftime	FTIME

#include	<sys/timeb.h>			/* from system */

#undef		ftime

extern	void	ftime( struct timeb * );	/* this is the funtion */

#else

#endif

void	timer_c( double *cpu_time ,		/* cpu timer */
                 double *real_time ,		/* real timer */
                 fint  *mode )			/* the mode */
{
   clock_t	tc;				/* clock time */
   double	ct;				/* cpu time in seconds */
   double	rt;				/* real time in seconds */
#if	defined(__sysv__) | defined(__APPLE__)
   struct timeval 	Tp;
   struct timezone	Tzp;
#elif	defined(__bsd__)
   struct timeb tr;				/* struct from ftime */
#else
#endif
   tc = clock( );				/* get clock time */
   ct = (double)(tc) / (double)CLOCKS_PER_SEC;	/* to seconds */
#if	defined(__sysv__) | defined(__APPLE__)
   gettimeofday( &Tp, &Tzp );			/* get timeofday */
   rt = (double) Tp.tv_sec + 0.000001 * (double) Tp.tv_usec;
#elif	defined(__bsd__)
   ftime( &tr );				/* get real time */
   rt = (double) tr.time + 0.001 * (double) tr.millitm;	/* in seconds */
#else
#endif
   if (*mode) {					/* calculate difference */
      (*cpu_time)  = ct - (*cpu_time);		/* cpu time */
      (*real_time) = rt - (*real_time);		/* real time */
   } else {
      (*cpu_time)  = ct;			/* set cpu time */
      (*real_time) = rt;			/* set real time */
   }
}


#if	defined(TESTBED)

extern	int	sleep( int );

#include	"cmain.h"			/* main C */
#include        "math.h"

int	cmain( int argc, char **argv )
{
   fint		mode;
   double	cpu_time;
   double	real_time;
   double       ca=0.7;
   int          i;

   mode = 0;
   timer_c( &cpu_time, &real_time, &mode );
   for (i=0; i<10000000; i++) ca=cos(ca);
   mode = 1;
   timer_c( &cpu_time, &real_time, &mode );
   printf( "cpu_time  = %10f seconds\n", cpu_time );
   printf( "real_time = %10f seconds\n", real_time );
   return( 0 );
}

#endif
