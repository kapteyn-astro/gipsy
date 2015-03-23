/* iotest.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.

#>            iotest.dc1

Program:      IOTEST

Purpose:      Tests the speed of disk io.

Category:     DEMO

File:         iotest.c

Author:       K.G. Begeman

Description:  The programme has three options: 1 = read data, 2 is read
              data and write it back to disk and 3 read data, write back to
              disk and read again to check whether the data was corrupted.
              At the end the programme will list the number of bytes
              transferred and the cpu and real time it took.

Keywords:

  INSET=      Enter set (and subsets) to read/write.
              The data should NOT be damaged by this programme if the
              disk is o.k.

  BOX=        Enter BOX [whole subset].

  OPTION=     Which option? (1=read,2=read/write,3=check)

  BUFFERSIZE= Enter size of read/write buffer [4096].

Updates:      Dec 10, 1991: KGB Document created.

#<

*/

#include	"stdio.h"
#include	"stdlib.h"
#include	"time.h"
#include	"gipsyc.h"
#include	"cmain.h"
#include	"anyout.h"
#include	"error.h"
#include	"finis.h"
#include	"gdsbox.h"
#include	"gdsc_fill.h"
#include	"gdsinp.h"
#include	"gdsi_read.h"
#include	"gdsi_write.h"
#include	"gds_close.h"
#include	"init.h"
#include	"timer.h"
#include	"userint.h"

#define	MAXAXES		10
#define	MAXSETNAMLEN	80
#define	MAXSUBSETS	1000

static	char	setb[MAXSETNAMLEN];
static	fchar	set = { setb, MAXSETNAMLEN };
static	fint	subsets[MAXSUBSETS];
static	fint	nsub;
static	fint	blo[MAXAXES];
static	fint	bup[MAXAXES];

MAIN_PROGRAM_ENTRY
{
   double	cpu_time = 0.0;
   double	real_time = 0.0;
   fint		maxdata;
   fint		npixels = 0;
   fint		n;
   fint		option;
   float	*data = NULL;
   float	*buff = NULL;

   init_c( );
   {
      fint	axperm[MAXAXES];
      fint	axsize[MAXAXES];
      fint	class = 1;
      fint	classdim = 0;
      fint	input_level = 0;
      fint	maxaxes = MAXAXES;
      fint	maxsub = MAXSUBSETS;
      fint	output_level = 0;

      nsub = gdsinp_c( set ,
                       subsets ,
                       &maxsub ,
                       &input_level ,
                       tofchar( "INSET=" ) ,
                       tofchar( "Enter set/subsets to read/write" ) ,
                       &output_level ,
                       axperm ,
                       axsize ,
                       &maxaxes ,
                       &class ,
                       &classdim );
   }
   {
      fint	input_level = 1;
      fint	mode = 0;
      fint	output_level = 11;

      gdsbox_c( blo ,
                bup ,
                set ,
                subsets ,
                &input_level ,
                tofchar( "BOX=" ) ,
                tofchar( "Enter BOX [whole subset]" ) ,
                &output_level ,
                &mode );
   }
   {
      fint	input_level = 0;
      fint	one = 1;

      userint_c( &option ,
                 &one ,
                 &input_level ,
                 tofchar( "OPTION=" ) ,
                 tofchar( "Option (1=read,2=read/write,3=check)" ) );
      if (option < 1 || option > 3) {
         fint	error_level = 4;

         error_c( &error_level, tofchar( "Illegal option!" ) );
      }
   }
   {
      fint	input_level = 1;
      fint	one = 1;

      if (!userint_c( &maxdata ,
                      &one ,
                      &input_level ,
                      tofchar( "BUFFERSIZE=" ) ,
                      tofchar( "Enter size of read buffer [4096]" ) )) {
         maxdata = 4096;
      }
      data = calloc( maxdata, sizeof( float ) );
      if (data == NULL) {
         fint	error_level = 4;

         error_c( &error_level, tofchar( "Memory allocation problems" ) );
      }
      if (option == 3) {
         buff = calloc( maxdata, sizeof( float ) );
         if (buff == NULL) {
            fint	error_level = 4;

            error_c( &error_level, tofchar( "Memory allocation problems" ) );
         }
      }
   }
   {
      fint	mode = 0;

      timer_c( &cpu_time, &real_time, &mode );
   }
   for (n = 0; n < nsub; n++) {
      fint	block = 0;
      fint	cwlo = gdsc_fill_c( set, &subsets[n], blo );
      fint	cwup = gdsc_fill_c( set, &subsets[n], bup );
      fint	i;
      fint	ndone;
      fint	nread = maxdata;
      fint	ntotal = 0;
      fint	tid1 = 0;
      fint	tid2 = 0;
      fint	tid3 = 0;

      do {
         gdsi_read_c( set ,
                      &cwlo ,
                      &cwup ,
                      data ,
                      &nread ,
                      &ndone ,
                      &tid1 );
         if (option != 1) {
            gdsi_write_c( set ,
                          &cwlo ,
                          &cwup ,
                          data ,
                          &nread ,
                          &ndone ,
                          &tid2 );
         }
         if (option == 3) {
            fint	d = 0;

            gdsi_read_c( set ,
                         &cwlo ,
                         &cwup ,
                         buff ,
                         &nread ,
                         &ndone ,
                         &tid3 );
            for (i = 0; i < ndone; i++) {
               if (buff[i] != data[i]) d++;
            }
            if (d) {
               char	message[80];
               fint	output_level = 3;

               sprintf( message, "Subset %5d Block %5d Errors %5d", n+1, block, d );
               anyout_c( &output_level, tofchar( message ) );
            }
         }
         block += 1;
         ntotal += ndone;
      } while (tid1 || tid2 || tid3);
      npixels += ntotal;
   }
   {
      char	message[80];
      fint	mode = 1;
      fint	output_level = 3;

      timer_c( &cpu_time, &real_time, &mode );
      sprintf( message, "Pixels     (number) : %10d", npixels );
      anyout_c( &output_level, tofchar( message ) );
      sprintf( message, "CPU time  (seconds) : %10f", cpu_time );
      anyout_c( &output_level, tofchar( message ) );
      sprintf( message, "Real time (seconds) : %10f", real_time );
      anyout_c( &output_level, tofchar( message ) );
   }
#if	0
   {
      fint	gerror = 0;

      gds_close_c( set, &gerror );
   }
#endif
   finis_c( );
   return( EXIT_SUCCESS );
}
