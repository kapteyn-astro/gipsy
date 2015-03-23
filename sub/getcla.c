/* getcla.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991
	All Rights Reserved.


#>            getcla.dc3

Function:     GETCLA

Purpose:      Obtains a command line argument.

Category:     UTILITY

File:         getcla.src

Author:       K.G. Begeman

Use:          INTEGER GETCLA( ARGC ,       Input     INTEGER
                              ARGV )       Output    CHARACTER*(*)

              GETCLA    Returns number of command line arguments (excluding
                        the command name) or -1 on error.
              ARGC      The number of the desired command line argument;
                        the command name is argument number 0; the
                        remaining arguments are numbered beginning with 1.
              ARGV      The text of the specified command line argument.

Warning:      System dependent! Currently implemented for ALLIANT, ALPHA,
              CONVEX, CRAY, IBM AIX, SILICON GRAPHICS, SUN, MIPS, Linux
              and HP 9000.

Updates:      Jul  1, 1991: KGB, Document created.
              Jul  2, 1999: JPT, Added __g77__ option 3.
              Dec 19, 2008: JPT, Included gfortran code section.

#<

Fortran to C interface:

@ integer function getcla( integer, character )

*/

#include	"osdef.h"		/* define __'machine'__ */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */

extern	char	**gipsy_argv;		/* export the arguments */
extern	int	gipsy_argc;		/* export the argument counter */

#ifdef	__sgi__
char	**f77argv;
int	f77argc;
#endif

char	**gipsy_argv;			/* define the arguments */
int	gipsy_argc = -1;		/* define the argument counter */

fint	getcla_c( fint	*narg ,		/* which argument do you want ? */
                  fchar	argv  )		/* return it here */
{
   fint	r = 0;				/* return value */

/*
 * Since we initialize gipsy_argc to one here, and the only place this external
 * can be changed is in cmain.h or here, we can figure out whether the programme
 * has a Fortran main (gipsy_argc not changed) or a C main (gipsy_argc set to
 * the correct value).
 */
   if (gipsy_argc == -1) {		/* probably Fortran Main Programme */
/*
 * Here comes the system dependent part. If the programme has a Fortran Main
 * we will have to get the arguments from that. Usuallly the Fortran main
 * defines two externals, one which holds the argument counter and one
 * which holds the actual arguments. Unfortunately, the names of these
 * externals are system dependent.
 */
#if   defined(__GFORTRAN__)
      extern int  _gfortran_iargc(void);
      extern void _gfortran_getarg_i4(int *, char *, int);
       
      _gfortran_getarg_i4(narg, argv.a, argv.l);
      r = _gfortran_iargc();

#elif defined(__F2C__)
#if	__g77__ == 3
      extern    char    **__libc_argv;  /* Defined in the Fortran Main */
      extern    int     __libc_argc;    /* Defined in the Fortran Main */

      gipsy_argc = __libc_argc;         /* get argument counter */
      gipsy_argv = __libc_argv;         /* get the arguments */
#elif   __g77__ == 2
      extern	char	**f__xargv;	/* Defined in the Fortran Main */
      extern	int	f__xargc;	/* Defined in the Fortran Main */

      gipsy_argc = f__xargc;		/* get argument counter */
      gipsy_argv = f__xargv;		/* get the arguments */
#else
      extern	char	**xargv;	/* Defined in the Fortran Main */
      extern	int	xargc;		/* Defined in the Fortran Main */

      gipsy_argc = xargc;		/* get argument counter */
      gipsy_argv = xargv;		/* get the arguments */
#endif
#elif	defined(__aix__)
      extern	char	**p_xargv;	/* Defined in the Fortran Main */
      extern	int	p_xargc;	/* Defined in the Fortran Main */

      gipsy_argc = p_xargc;		/* get argument counter */
      gipsy_argv = p_xargv;		/* get the arguments */
#elif	defined(__cray__)
      extern	int	IARGC(void);
      extern	int	GETARG(int *,_fcd);
      int		n;

      r = IARGC( );
      if ((*narg) >= 0 && (*narg) <= r) {
      	 int	argn = (*narg);
         n = GETARG(&argn,_cptofcd(argv.a,argv.l));
      } else {
      	 n = 0;
      }
      for (; n < argv.l; argv.a[n++] = ' ');
#elif	defined(__hp9000s300__)
      extern	char	**__argv_value;	/* Defined in the Fortran Main */
      extern	int	__argc_value;	/* Defined in the Fortran Main */

      gipsy_argc = __argc_value;	/* get argument counter */
      gipsy_argv = __argv_value;	/* get the arguments */
#elif	defined(__hp9000s700__)
/*
 * On the HP 700 Series computers we have to call the Fortran routines
 * to get to the arguments. We don't change gipsy_argc and gipsy_argv here.
 * For each argument we call IARGC and GETARG.
 */
      extern	int	IARGC( void );
      extern	void	GETARG( int *, char *, int );

      r = IARGC( ) - 1;
      if ((*narg) >= 0 && (*narg) <= r) {
      	 int	argn = (*narg) + 1;
         GETARG( &argn, argv.a, argv.l );
      } else {
         int	n;

         for (n = 0; n < argv.l; argv.a[n++] = ' ');
      }
#elif	defined(__hp9000s800__)
      extern	char	**__argv_value;	/* Defined in the Fortran Main */
      extern	int	__argc_value;	/* Defined in the Fortran Main */

      gipsy_argc = __argc_value;	/* get argument counter */
      gipsy_argv = __argv_value;	/* get the arguments */
#elif	defined(__alpha__) | defined(__mips__) | defined(__sgi__) | defined(__sun__)
/*
 * On the mips computers we have to call the Fortran routines
 * to get to the arguments. We don't change gipsy_argc and gipsy_argv here.
 * For each argument we call iargc and getarg.
 */
      extern	int	iargc_( void );
      extern	void	getarg_( int *, char *, int );

      r = iargc_( );
      if ((*narg) >= 0 && (*narg) <= r) {
         getarg_( (int*)narg, argv.a, argv.l );
      } else {
         int	n;

         for (n = 0; n < argv.l; argv.a[n++] = ' ');
      }
#else
/*
 * Most machines (alliant, convex) use the same externals in the
 * Fortran main. So we made this default. This could be dangerous and has
 * to be checked on each new machine.
 */
      extern	char	**xargv;	/* Defined in the Fortran Main */
      extern	int	xargc;		/* Defined in the Fortran Main */

      gipsy_argc = xargc;		/* get argument counter */
      gipsy_argv = xargv;		/* get the arguments */
#endif
   }
   if (gipsy_argc > -1) {		/* we made the assignment */
      int	n = 0;			/* loop counter */

      r = gipsy_argc - 1;		/* one less than argument counter */
      if ((*narg) >= 0 && (*narg) <= r) {
         while (n < argv.l && (argv.a[n] = gipsy_argv[*narg][n])) n++;
      }
      while (n < argv.l) argv.a[n++] = ' ';
   }
   return( r );				/* return to caller */
}
