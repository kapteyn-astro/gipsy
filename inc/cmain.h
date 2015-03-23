/* cmain.h

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.


#>            cmain.dc3

Header:       cmain

Purpose:      Defines the main body of a C program.

File:         cmain.h

Author:       K.G. Begeman

Use:          #include "cmain.h"

Description:  cmain.h defines the main body for GIPSY applications
              written in C. C programmers have to use the
              MAIN_PROGRAM_ENTRY statement to declare the main.
              If they want to access the command line arguments
              they have to declare int cmain( int argc, char **argv )
              where argc and argv have the same meaning as in the
              normal main.
              This header file also defines the macro:
              IDENTIFICATION(char *myname, char *version)
              which prints the name of the program, its version
              number and the date of compilation. 

Example:      .....
              #include "cmain.h"
              .....

              MAIN_PROGRAM_ENTRY
              {
                 init_c( );
                 IDENTIFICATION( "CLEAN", "1.2" );
                 .....
                 .....
                 finis_c( );
              }

              or
              
              #include "cmain.h"
              .....
              int cmain( int argc, char **argv )
              {
                 init_c( );
                 IDENTIFICATION( "CLEAN", "1.2" );
                 .....
                 .....
                 finis_c( );
              }

Updates:      Jun  2, 1990: KGB Document created.
              Nov 26, 1990: KGB IDENTIFICATION macro added.
              Dec 19, 2008: JPT gfortran code section added.
              Apr 21, 2010: JPT gfortran main section for MacOS X Leopard.
              May 10, 2010: JPT command line arguments for MacOS X fixed.
              Jan 17, 2011: JPT always define main() regardless of necessity.

#<

*/

#if     !defined(_CMAIN_H)
#define _CMAIN_H

#include	"stdarg.h"			/* <stdarg.h> */
#include	"stdlib.h"
#include	"string.h"
#include	"osdef.h"			/* get __'machine'__ */
#include	"gipsyc.h"			/* GIPSY definitions */

/*
 * First we define the main. The main stores the command lne arguments
 * in gipsy_argc and gipsy_argv and then calls cmain. The programmer
 * has to define cmain.
 */

extern	char	**gipsy_argv;			/* declared in getcla_c */
extern	int	gipsy_argc;			/* declared in getcla_c */

#ifdef	__g77__

#if	__g77__ == 2
extern	int	f__xargc;
extern	char	**f__xargv;

int	MAIN__(void)
{
   int	cmain();
   gipsy_argc = f__xargc;
   gipsy_argv = f__xargv;
   return( cmain( f__xargc, f__xargv ) );
   
}
#else
extern	int	xargc;
extern	char	**xargv;

int	MAIN__(void)
{
   int	cmain();
   gipsy_argc = xargc;
   gipsy_argv = xargv;
   return( cmain( xargc, xargv ) );
   
}
#endif
#elif defined(__GFORTRAN__) && !defined(__APPLE__)
#define CURARGSIZE 256                /* hope this is large enough */

extern int  _gfortran_iargc(void);
extern void _gfortran_getarg_i4(int *, char *, int);

static void trim(char* s, int l)
{
   int i, len=0;
   char c;
   for (i=0; i<l; i++) {
      c = s[i];
      if (c) {
         if (c!=' ') len = i+1;
      } else {
         break;
      }
   }
   s[len] = '\0';
}

int     MAIN__(void)
{
   int  cmain();
   int  i;
   fint fi;
   char curarg[CURARGSIZE];  
   gipsy_argc = _gfortran_iargc()+1;
   gipsy_argv = calloc(gipsy_argc, sizeof(char*));
   for (i=0; i<gipsy_argc; i++) {
      fi = i;
      _gfortran_getarg_i4(&fi, curarg, CURARGSIZE-1);
      trim(curarg, CURARGSIZE);
      gipsy_argv[i] = strdup(curarg);
   }
   return( cmain( gipsy_argc, gipsy_argv ) );
}
#endif
int	main( int argc, char **argv )
{
   int	cmain();
   gipsy_argc = argc;
   gipsy_argv = argv;
   return( cmain( argc, argv ) );
}

/*
 * Here we define MAIN_PROGRAM_ENTRY for application programmers who do
 * not need the command line arguments.
 */

#if	!defined(MAIN_PROGRAM_ENTRY)
#define	MAIN_PROGRAM_ENTRY	int cmain(void)
#endif

/*
 * Next we define the IDENTIFICATION macro (if it is not already defined).
 */

#if	!defined(IDENTIFICATION)
#define IDENTIFICATION(myname,version) \
{ \
   char  b[80]; \
   fchar fb; \
   fint  l = 8; \
   void  anyout_c(fint*,fchar); \
   fb.a = b; \
   fb.l = sprintf( b, "%s  Version %s  (%s)", myname, version, __DATE__ ); \
   anyout_c( &l, fb ); \
}
#endif

#endif
