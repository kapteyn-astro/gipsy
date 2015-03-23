/* floating.c

	Copyright (c) Kapteyn Laboratorium Groningen 1991, 1992, 1993
	All Rights Reserved.

#>            floating.dc2

Document:     floating

Purpose:      Describes the routines which handle floating point
              conversion.

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Description:  The routines which deal with single and double precision
              floating point types are:

              Routine      Function
              SPFPFL       convert from foreign single precision
                           representation to local
              SPFPLF       convert from local single precision
                           representation to foreign
              DPFPFL       convert from foreign double precision
                           representation to local
              DPFPLF       convert from local double precision
                           representation to foreign
              FBLANK       tests whether argument is a (local)
                           universal single precision blank
              SETFBLANK    sets argument to (local) universal
                           single precision blank
              SETNFBLANK   sets an array to (local) universal
                           single precision blank
              DBLANK       tests whether argument is a (local)
                           universal double precision blank
              SETDBLANK    sets argument to (local) universal
                           double precision blank
              SETNDBLANK   sets an array to (local) universal
                           double precision blank

              The local type of floating point must be defined in "osdef.h"
              via the OS_FLOATING_TYPE macro. The following types are
              recognized:

              OS_FLOATING_TYPE          Type of floating point
                     0                  IEEE High_Endian (BLANK=-Inf)
                     1                  IEEE Low_Endian (BLANK=-Inf)
                     2                  CONVEX Native
                     3                  VMS D_Floating
                     4                  VMS G_Floating
                     5                  IEEE High_Endian (BLANK=-FLT_MAX)
                     6                  IEEE Low_Endian (BLANK=-FLT_MAX)

              The routines mentioned above are separately documented.

Updates:      Dec 10, 1991: Document created.

#<

*/

#include	"osdef.h"		/* os definition */
#include	"float.h"		/* <float.h> */
#include	"math.h"		/* <math.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"string.h"		/* <string.h> */
#include	"f2cvvdefs.h"		/* Fortran to C definitions */

#ifndef	OS_FLOATING_TYPE		/* not defined, take default */
#define	OS_FLOATING_TYPE	0	/* IEEE high_Endian */
#endif

#ifndef	OS_INTEGER_TYPE			/* not defined, take default */
#define	OS_INTEGER_TYPE		0	/* IEEE High_Endian */
#endif

#if	OS_INTEGER_TYPE == 1
#define	SWAP				/* we have to swap bytes */
#endif

/*
 * The following types define unions of floats and doubles with unsigned
 * characters and ints.
 */

typedef union {				/* for single precision floats */
   unsigned int		l;
   float		f;
   unsigned char	b[sizeof(float)];
} F_UN;					/* the type */

typedef union {				/* for double precision floats */
   unsigned int		l[sizeof(double)/sizeof(unsigned int)];
   double		d;
   unsigned char	b[sizeof(double)];
} D_UN;					/* the type */

/*
 * The following table holds all known blank values as they are represented
 * in the different floating point types.
 */

static	unsigned int	FBLANK_TAB[7] = {
   0xff800000,				/* IEEE High_Endian (-Inf) */
   0xff800000,				/* IEEE Low_Endian (-Inf) */
   0xffffffff,				/* CONVEX Native */
   0xffffffff,				/* VMS F_floating */
   0xffffffff,				/* VMS F_floating */
   0xff7fffff,				/* IEEE High_Endian (-FLT_MAX) */
   0xff7fffff				/* IEEE Low_Endian (-FLT_MAX) */
};

#if	!defined(__vms__)

static	D_UN		DBLANK_TAB[7] = {
   { { 0xfff00000, 0x00000000 } },	/* IEEE High_Endian */
   { { 0x00000000, 0xfff00000 } },	/* IEEE Low_Endian */
   { { 0xffffffff, 0xffffffff } },	/* CONVEX Native */
   { { 0xffffffff, 0xffffffff } },	/* VMS D_Floating */
   { { 0xffffffff, 0xffffffff } },	/* VMS F_Floating */
   { { 0xffefffff, 0xffffffff } },	/* IEEE High_Endian (-DBL_MAX) */
   { { 0xffffffff, 0xffefffff } }	/* IEEE Low_Endian (-DBL_MAX) */
};

#else

static	D_UN		DBLANK_TAB[7];
static	fint		INIT = 0;

static	init_blank( void )
{
   if (!INIT) {
      INIT = 1;
      DBLANK_TAB[0].l[0] = 0xfff00000; DBLANK_TAB[0].l[1] = 0x00000000;
      DBLANK_TAB[1].l[0] = 0x00000000; DBLANK_TAB[1].l[1] = 0xfff00000;
      DBLANK_TAB[2].l[0] = 0xffffffff; DBLANK_TAB[2].l[1] = 0xffffffff;
      DBLANK_TAB[3].l[0] = 0xffffffff; DBLANK_TAB[3].l[1] = 0xffffffff;
      DBLANK_TAB[4].l[0] = 0xffffffff; DBLANK_TAB[4].l[1] = 0xffffffff;
      DBLANK_TAB[5].l[0] = 0xffefffff; DBLANK_TAB[5].l[1] = 0xffffffff;
      DBLANK_TAB[6].l[0] = 0xffffffff; DBLANK_TAB[6].l[1] = 0xffefffff;
   }
}

#endif

/*
 * Here we define blank as a short-cut to the local representation of
 * blank.
 */

#define	FBLANK FBLANK_TAB[OS_FLOATING_TYPE]
#define	DBLANK DBLANK_TAB[OS_FLOATING_TYPE]


/*
#>            fblank.dc2

Function:     FBLANK

Purpose:      Logical function returning whether argument is a universal
              BLANK (undefined).

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          LOGICAL FBLANK( DATA )     Input   REAL

              DATA        Data value to check upon.

Notes:        - Each application routine which reads in data must check
                before doing its operation whether a datum has the
                blank value.
              - It is faster to do a direct compare in your application
                routine. The universal BLANK can be obtained with
                subroutine SETFBLANK.

Example:      REAL  BLANK
              CALL SETFBLANK( BLANK )
              FOR I = 1, N
                 IF (BLANK .EQ. DATA(I))
                 THEN
                    CALL SETFBLANK(RESULT(I))
                 ELSE
                    RESULT(I) = 2.0 * DATA(I) + 3.1456
                 CIF
              CFOR

Warning:      This routine is system dependent!

Updates:      Oct 24, 1988: KGB, Document created.
              Apr 23, 1991: KGB, Implemented for HP 9000.
              May 16, 1991: KGB, Incorporated in floating.c.

#<

Fortran to C interface:

@ logical function fblank( real )

*/

bool	fblank_c( unsigned int *datum )
{
   return( toflog( *datum == FBLANK ) );	/* return to caller */
}


/*
#>            setfblank.dc2

Subroutine:   SETFBLANK

Purpose:      Subroutine to set a data value to the universal BLANK.

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          CALL SETFBLANK( DATA )     Output    REAL

              DATA       Data will contain the value for BLANK on return.

Example:      REAL  BLANK
              CALL SETFBLANK( BLANK )
              FOR I = 1, N
                 IF (BLANK .EQ. DATA(I))
                 THEN
                    CALL SETFBLANK(RESULT(I))
                 ELSE
                    RESULT(I) = 2.0 * DATA(I) + 3.1456
                 CIF
              CFOR

Warning:      This routine is system dependent!

Updates:      Oct 24, 1988: KGB, Document created.
              Apr 23, 1991: KGB, Implemented for HP 9000.
              May 16, 1991: KGB, Incorprated in floating.c.

#<

Fortran to C interface:

@ subroutine setfblank( real )

*/

void	setfblank_c( unsigned int *datum )
{
   *datum = FBLANK;				/* this is it */
}


/*
#>            setnfblank.dc2

Subroutine:   SETNFBLANK

Purpose:      Subroutine to set a number of data values to the
              universal BLANK.

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          CALL SETNFBLANK( DATA  ,    Output       real array
                               NDATA )    Input        integer

              DATA      This array will contain the NDATA BLANK values
                        on return.
              NDATA     Number of elements in DATA.

Warning:      This routine is system dependent!

Updates:      Feb 24, 1989: KGB, Document created.
              Apr 23, 1991: KGB, Implemented for HP 9000.
              May 16, 1991: KGB, Incorporated in floating.c.

#<

Fortran to C interface:

@ subroutine setnfblank( real, integer )

*/


void	setnfblank_c( unsigned int *data , fint *ndata )
{
   fint  i;

   for (i = 0; i++ < (*ndata); *data++ = FBLANK);	/* fill with blanks */
}


/*
#>            dblank.dc2

Function:     DBLANK

Purpose:      Logical function returning whether argument is a universal
              BLANK (undefined).

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          LOGICAL DBLANK( DATA )     Input   DOUBLE PRECISION

              DATA        Data value to check upon.

Notes:        - Each application routine which reads in data must check
                before doing its operation whether a datum has the
                blank value.
              - It is faster to do a direct compare in your application
                routine. The universal BLANK can be obtained with
                subroutine SETDBLANK.

Example:      DOUBLE PRECISION  BLANK
              CALL SETDBLANK( BLANK )
              FOR I = 1, N
                 IF (BLANK .EQ. DATA(I))
                 THEN
                    CALL SETDBLANK(RESULT(I))
                 ELSE
                    RESULT(I) = 2.0D0 * DATA(I) + 3.1456D0
                 CIF
              CFOR

Warning:      This routine is system dependent!

Updates:      May 18, 1988: KGB, Document created.

#<

Fortran to C interface:

@ logical function dblank( double precision )

*/

bool	dblank_c( D_UN *datum )
{
#if	defined(__vms__)
   init_blank( );
#endif
   return( toflog( datum->l[0] == DBLANK_TAB[OS_FLOATING_TYPE].l[0] &&
                   datum->l[1] == DBLANK_TAB[OS_FLOATING_TYPE].l[1] ) );
}


/*
#>            setdblank.dc2

Subroutine:   SETDBLANK

Purpose:      Subroutine to set a data value to the universal BLANK.

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          CALL SETDBLANK( DATA )     Output    DOUBLE PRECISION

              DATA       Data will contain the value for BLANK on return.

Example:      DOUBLE PRECISION  BLANK
              CALL SETDBLANK( BLANK )
              FOR I = 1, N
                 IF (BLANK .EQ. DATA(I))
                 THEN
                    CALL SETDBLANK(RESULT(I))
                 ELSE
                    RESULT(I) = 2.0D0 * DATA(I) + 3.1456D0
                 CIF
              CFOR

Warning:      This routine is system dependent!

Updates:      May 18, 1991: KGB, Document created.

#<

Fortran to C interface:

@ subroutine setdblank( double precision )

*/

void	setdblank_c( D_UN *datum )
{
#if	defined(__vms__)
   init_blank( );
#endif
   *datum = DBLANK;				/* this is it */
}


/*
#>            setndblank.dc2

Subroutine:   SETNDBLANK

Purpose:      Subroutine to set a number of data values to the
              universal BLANK.

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          CALL SETNDBLANK( DATA  ,    Output   DOUBLE PRECISION ARRAY
                               NDATA )    Input    INTEGER

              DATA      This array will contain the NDATA BLANK values
                        on return.
              NDATA     Number of elements in DATA.

Warning:      This routine is system dependent!

Updates:      May 18, 1991: KGB, Document created.

#<

Fortran to C interface:

@ subroutine setndblank( double precision, integer )

*/


void	setndblank_c( D_UN *data , fint *ndata )
{
   fint  i;

#if	defined(__vms__)
   init_blank( );
#endif
   for (i = 0; i++ < (*ndata); *data++ = DBLANK);	/* fill with blanks */
}


/*
 * swap2 swaps bytes in two's.
 */

static unsigned int	swap2( unsigned int value )
{
   union {
      unsigned char	b[sizeof(unsigned int)];
      unsigned int	l;
   } u_arg, u_ret;

   u_arg.l = value;
   u_ret.b[1] = u_arg.b[0];
   u_ret.b[0] = u_arg.b[1];
   u_ret.b[3] = u_arg.b[2];
   u_ret.b[2] = u_arg.b[3];
   return( u_ret.l );
}


/*
 * swap4 swaps bytes in four's.
 */

static	unsigned int	swap4( unsigned int value )
{
   union {
      unsigned char	b[sizeof(unsigned int)];
      unsigned int	l;
   } u_arg, u_ret;

   u_arg.l = value;
   u_ret.b[3] = u_arg.b[0];
   u_ret.b[2] = u_arg.b[1];
   u_ret.b[1] = u_arg.b[2];
   u_ret.b[0] = u_arg.b[3];
   return( u_ret.l );
}


/*

Function:     spfp_conversion

Purpose:      spfp_conversion does the back and forth conversion of single
              precision reals (floats). It is not guaranteed that conversion
              from one foreign format to another foreign format works.

Category:     DATA

File:         floating.h

Author:       K.G. Begeman

Use:          fint spfp_conversion( fint  intype ,
                                    float infloat[] ,
                                    fint  outtype ,
                                    float outfloat[] ,
                                    fint  nf )

              spfp_conversion    Return -1 when intype or outtype are
                                 not known, else the number of blank
                                 substitutions because of representation
                                 errors.
              intype             type of input floating point format.
              infloat            array of input floating point numbers.
              outtype            type of output floating point format.
              outfloat           array of output floating point numbers.
              nf                 number of floats to convert.

Updates:      May 17, 1991: KGB Document created.

*/

static fint	spfp_conversion( fint	intype ,
                                 float	infloat[] ,
                                 fint	outtype ,
                                 float	outfloat[] ,
                                 fint	nf )
{
   F_UN		bi, bo;				/* the union for blanks */
   F_UN		fp;				/* the union */
   fint		n;				/* loop counter */
   fint		r = 0;				/* return value */
   unsigned int	e;				/* exponent */
   unsigned int	f;				/* fraction */
   unsigned int	s;				/* sign */
   unsigned int	*fin;				/* converted input */
   unsigned int	*fout;				/* converted output */

   fin = (unsigned int *) infloat;		/* convert input */
   fout = (unsigned int *) outfloat;		/* convert output */
   if ( intype == outtype ) {			/* quick solution */
      if (fin != fout) {			/* we need to copy */
         for (n = 0; n < nf; n++) {		/* conversion loop */
            fout[n] = fin[n];			/* do the conversion */
         }
      }
      return( r );				/* we're done */
   }
   switch( intype ) {				/* which input type ? */
      case 0:					/* IEEE High Endian (1) -> */
      case 5: {					/* IEEE High Endian (2) -> */
         switch( outtype ) {			/* which output type ? */
            case 0:				/* -> IEEE High Endian (1) */
            case 5: {				/* -> IEEE High Endian (2) */
#ifdef	SWAP
               bi.l = swap4( FBLANK_TAB[intype] );
               bo.l = swap4( FBLANK_TAB[outtype] );
#else
               bi.l = FBLANK_TAB[intype];
               bo.l = FBLANK_TAB[outtype];
#endif
               for (n = 0; n < nf; n++) {
                  if (fin[n] == bi.l) {
                     fout[n] = bo.l;
                  } else {
                     fout[n] = fin[n];
                  }
               }
               break;
            }
            case 1:				/* -> IEEE Low Endian (1) */
            case 6: {				/* -> IEEE Low Endian (2) */
               for (n = 0; n < nf; n++) {	/* conversion loop */
#ifdef	SWAP
                  fp.l = swap4( fin[n] );	/* swap bytes */
#else
                  fp.l = fin[n];		/* just copy */
#endif
                  if (fp.l == FBLANK_TAB[intype]) {
                     fp.l = FBLANK_TAB[outtype];
                  }
#ifdef	SWAP
                  fout[n] = fp.l;		/* just copy */
#else
                  fout[n] = swap4( fp.l );	/* swap bytes */
#endif
               }
               break;				/* we're done */
            }
            case 2: {				/* -> CONVEX Native */
               for (n = 0; n < nf; n++) {	/* conversion loop */
#ifdef	SWAP
                  fp.l = swap4( fin[n] );	/* swap input float */
#else
                  fp.l = fin[n];		/* get input float */
#endif
                  if (fp.l == FBLANK_TAB[intype]) {
                     fp.l = FBLANK_TAB[outtype];
                  } else {			/* try conversion */
                     e = ( fp.l & 0x7f800000 ) >> 23;
                     f = ( fp.l & 0x007fffff );
                     s = ( fp.l & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        fp.l = f;		/* result is zero */
                     } else if (e < 0x000000fe) {
                        e += 2;			/* multiply by 4 */
                        fp.l = s | (e << 23) | f;
                     } else {			/* conversion error */
                        fp.l = FBLANK_TAB[outtype];
                        r += 1;			/* we produced a BLANK */
                     }
                  }
#ifdef	SWAP
                  fout[n] = swap4( fp.l );	/* swap bytes */
#else
                  fout[n] = fp.l;		/* output converted float */
#endif
               }
               break;
            }
            /* NOTE: VMS D_floating and G_floating are the same for
             * single precision reals.
             */
            case 3:				/* -> VMS D_floating */
            case 4: {				/* -> VMS G_floating */
               for (n = 0; n < nf; n++) {	/* conversion loop */
#ifdef	SWAP
                  fp.l = swap4( fin[n] );	/* swap bytes */
#else
                  fp.l = fin[n];		/* get input float */
#endif
                  if (fp.l == FBLANK_TAB[intype]) {
                     fp.l = FBLANK_TAB[outtype];
                  } else {			/* try conversion */
                     e = ( fp.l & 0x7f800000 ) >> 23;
                     f = ( fp.l & 0x007fffff );
                     s = ( fp.l & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        fp.l = f;		/* result is zero */
                     } else if (e < 0x000000fe) {
                        e += 2;			/* multiply by 4 */
                        fp.l = s | (e << 23) | f;
                     } else {			/* conversion error */
                        fp.l = FBLANK_TAB[outtype];
                        r += 1;			/* we produced a BLANK */
                     }
                  }
                  fp.l = swap2( fp.l );	/* swap 2 bytes */
#ifdef	SWAP
                  fout[n] = swap4( fp.l );	/* swap bytes */
#else
                  fout[n] = fp.l;		/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            default: {				/* unknown type */
               r = -1;
               break;
            }
         }
         break;					/* we're done */
      }
      case 1:					/* IEEE Low Endian (1) -> */
      case 6: {					/* IEEE Low Endian (2) -> */
         switch( outtype ) {			/* which output type ? */
            case 0:				/* -> IEEE High Endian (1) */
            case 5: {				/* -> IEEE High Endian (2) */
               for (n = 0; n < nf; n++) {	/* conversion loop */
#ifdef	SWAP
                  fp.l = fin[n];		/* just copy */
#else
                  fp.l = swap4( fin[n] );	/* swap bytes */
#endif
                  if (fp.l == FBLANK_TAB[intype]) {
                     fp.l = FBLANK_TAB[outtype];
                  }
#ifdef	SWAP
                  fout[n] = swap4( fp.l );	/* swap bytes */
#else
                  fout[n] = fp.l;		/* just copy */
#endif
               }
               break;				/* we're done */
            }
            case 1:				/* -> IEEE Low Endian (1) */
            case 6: {				/* -> IEEE Low Endian (2) */
#ifdef	SWAP
               bi.l = FBLANK_TAB[intype];
               bo.l = FBLANK_TAB[outtype];
#else
               bi.l = swap4( FBLANK_TAB[intype] );
               bo.l = swap4( FBLANK_TAB[outtype] );
#endif
               for (n = 0; n < nf; n++) {	/* conversion loop */
                  if (fin[n] == bi.l) {
                     fout[n] = bo.l;
                  } else {
                     fout[n] = fin[n];		/* do the conversion */
                  }
               }
               break;				/* we're done */
            }
            case 2: {				/* -> CONVEX Native */
               for (n = 0; n < nf; n++) {	/* conversion loop */
#ifdef	SWAP
                  fp.l = fin[n];		/* get input float */
#else
                  fp.l = swap4( fin[n] );	/* swap bytes */
#endif
                  if (fp.l == FBLANK_TAB[intype]) {
                     fp.l = FBLANK_TAB[outtype];
                  } else {			/* try conversion */
                     e = ( fp.l & 0x7f800000 ) >> 23;
                     f = ( fp.l & 0x007fffff );
                     s = ( fp.l & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        fp.l = f;		/* result is zero */
                     } else if (e < 0x000000fe) {
                        e += 2;			/* multiply by 4 */
                        fp.l = s | (e << 23) | f;
                     } else {			/* conversion error */
                        fp.l = FBLANK_TAB[outtype];
                        r += 1;			/* we produced a BLANK */
                     }
                  }
#ifdef	SWAP
                  fout[n] = swap4( fp.l );	/* swap bytes */
#else
                  fout[n] = fp.l;		/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            /* NOTE: VMS D_floating and G_floating are the same for
             * single precision reals.
             */
            case 3:				/* -> VMS D_floating */
            case 4: {				/* -> VMS G_floating */
               for (n = 0; n < nf; n++) {	/* conversion loop */
#ifdef	SWAP
                  fp.l = fin[n];		/* get input float */
#else
                  fp.l = swap4( fin[n] );	/* swap bytes */
#endif
                  if (fp.l == FBLANK_TAB[intype]) {
                     fp.l = FBLANK_TAB[outtype];
                  } else {			/* try conversion */
                     e = ( fp.l & 0x7f800000 ) >> 23;
                     f = ( fp.l & 0x007fffff );
                     s = ( fp.l & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        fp.l = f;		/* result is zero */
                     } else if (e < 0x000000fe) {
                        e += 2;			/* multiply by 4 */
                        fp.l = s | (e << 23) | f;
                     } else {			/* conversion error */
                        fp.l = FBLANK_TAB[outtype];
                        r += 1;			/* we produced a BLANK */
                     }
                  }
                  fp.l = swap2( fp.l );		/* swap in two's */
#ifdef	SWAP
                  fout[n] = swap4( fp.l );	/* swap bytes */
#else
                  fout[n] = fp.l;		/* output converted float */
#endif
               }
               break;				/* we're done */
            }
            default: {				/* unknown type */
               r = -1;
               break;
            }
         }
         break;					/* we're done */
      }
      case 2: {					/* CONVEX Native -> */
         switch( outtype ) {			/* which output type ? */
            case 0:				/* -> IEEE High Endian (1) */
            case 5: {				/* -> IEEE High Endian (2) */
               for (n = 0; n < nf; n++) {	/* conversion loop */
#ifdef	SWAP
                  fp.l = swap4( fin[n] );	/* swap bytes */
#else
                  fp.l = fin[n];		/* get input float */
#endif
                  if (fp.l == FBLANK_TAB[intype]) {
                     fp.l = FBLANK_TAB[outtype];
                  } else {			/* try conversion */
                     e = ( fp.l & 0x7f800000 ) >> 23;
                     f = ( fp.l & 0x007fffff );
                     s = ( fp.l & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        fp.l = 0;		/* result is zero */
                     } else if (e > 0x0000002) {
                        e -= 2;			/* multiply by 4 */
                        fp.l = s | (e << 23) | f;
                     } else {
                        fp.l = 0;		/* underflow */
                     }
                  }
#ifdef	SWAP
                  fout[n] = swap4( fp.l );	/* swap bytes */
#else
                  fout[n] = fp.l;		/* output converted float */
#endif
               }
               break;				/* we're done */
            }
            case 1:				/* -> IEEE Low Endian (1) */
            case 6: {				/* -> IEEE Low Endian (2) */
               for (n = 0; n < nf; n++) {	/* conversion loop */
#ifdef	SWAP
                  fp.l = swap4( fin[n] );	/* swap bytes */
#else
                  fp.l = fin[n];		/* get input float */
#endif
                  if (fp.l == FBLANK_TAB[intype]) {
                     fp.l = FBLANK_TAB[outtype];
                  } else {			/* try conversion */
                     e = ( fp.l & 0x7f800000 ) >> 23;
                     f = ( fp.l & 0x007fffff );
                     s = ( fp.l & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        fp.l = 0;		/* result is zero */
                     } else if (e > 0x00000002) {
                        e -= 2;			/* multiply by 4 */
                        fp.l = s | (e << 23) | f;
                     } else {
                        fp.l = 0;		/* underflow */
                     }
                  }
#ifdef	SWAP
                  fout[n] = fp.l;		/* conversion done */
#else
                  fout[n] = swap4( fp.l );	/* swap bytes */
#endif
               }
               break;				/* we're done */
            }
            /* NOTE: VMS D_floating and G_floating are the same for
             * single precision reals.
             */
            case 3:				/* -> VMS D_floating */
            case 4: {				/* -> VMS G_floating */
               for (n = 0; n < nf; n++) {	/* conversion loop */
                  fout[n] = swap2( fin[n] );	/* swap bytes in two's*/
               }
               break;				/* ciao */
            }
            default: {				/* unknown type */
               r = -1;
               break;
            }
         }
         break;					/* we're done with it */
      }
      /* NOTE: VMS D_floating and G_floating are the same for
       * single precision reals.
       */
      case 3:					/* VMS D_floating -> */
      case 4: {					/* VMS G_floating -> */
         switch( outtype ) {			/* which output type ? */
            case 0:				/* -> IEEE High Endian (1) */
            case 5: {				/* -> IEEE High Endian (2) */
               for (n = 0; n < nf; n++) {	/* conversion loop */
#ifdef	SWAP
                  fp.l = swap4( fin[n] );	/* swap bytes */
#else
                  fp.l = fin[n];		/* get input float */
#endif
                  fp.l = swap2( fp.l );	/* swap in two's */
                  if (fp.l == FBLANK_TAB[intype]) {
                     fp.l = FBLANK_TAB[outtype];
                  } else {			/* try conversion */
                     e = ( fp.l & 0x7f800000 ) >> 23;
                     f = ( fp.l & 0x007fffff );
                     s = ( fp.l & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        fp.l = 0;		/* result is zero */
                     } else if (e > 0x00000002) {
                        e -= 2;			/* multiply by 4 */
                        fp.l = s | (e << 23) | f;
                     } else {			/* underflow */
                        fp.l = 0;		/* set to blank */
                     }
                  }
#ifdef	SWAP
                  fout[n] = swap4( fp.l );	/* swap bytes */
#else
                  fout[n] = fp.l;		/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            case 1:				/* -> IEEE Low Endian (1) */
            case 6: {				/* -> IEEE Low Endian (2) */
               for (n = 0; n < nf; n++) {	/* conversion loop */
#ifdef	SWAP
                  fp.l = swap4( fin[n] );	/* swap bytes */
#else
                  fp.l = fin[n];		/* get input float */
#endif
                  fp.l = swap2( fp.l );	/* swap in two's */
                  if (fp.l == FBLANK_TAB[intype]) {
                     fp.l = FBLANK_TAB[outtype];
                  } else {			/* try conversion */
                     e = ( fp.l & 0x7f800000 ) >> 23;
                     f = ( fp.l & 0x007fffff );
                     s = ( fp.l & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        fp.l = 0;		/* result is zero */
                     } else if (e > 0x0000002) {
                        e -= 2;			/* multiply by 4 */
                        fp.l = s | (e << 23) | f;
                     } else {			/* underflow */
                        fp.l = 0;		/* set to blank */
                     }
                  }
#ifdef	SWAP
                  fout[n] = fp.l;		/* output converted float */
#else
                  fout[n] = swap4( fp.l );	/* swap bytes */
#endif
               }
               break;				/* we're done */
            }
            case 2: {				/* -> CONVEX Native */
               for (n = 0; n < nf; n++) {	/* conversion loop */
                  fout[n] = swap2( fin[n] );	/* swap bytes in two's*/
               }
               break;				/* ciao */
            }
            case 3:				/* -> VMS F_FLOATING */
            case 4: {				/* -> VMS D_FLOATING */
               if (fin != fout) {		/* we need to copy */
                  for (n = 0; n < nf; n++) {	/* conversion loop */
                     fout[n] = fin[n];		/* do the conversion */
                  }
               }
               break;
            }
            default: {				/* unknown format */
               r = -1;
               break;
            }
         }
         break;					/* we're done */
      }
      default: {				/* unknown format */
         r = -1;
         break;
      }
   }
   return( r );					/* return to caller */
}


/*
#>            spfpfl.dc2

Function:     SPFPFL

Purpose:      Converts Single Precision Floating Point numbers from Foreign
              format to Local format.

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          INTEGER SPFPFL( FTYPE ,       Input     INTEGER
                              FIN ,         Input     REAL ARRAY
                              FOUT ,        Output    REAL ARRAY
                              NF )          Input     INTEGER

              SPFPFL        Returns the number of BLANKS inserted because
                            input reals cannot be represented in local
                            reals, or  -1 if FTYPE is unknown.
              FTYPE         Type of foreign format:
                            0: IEEE High Endian format (BLANK=-Inf)
                            1: IEEE Low Endian format (BLANK=-Inf)
                            2: CONVEX native format
                            3: VMS D_floating format
                            4: VMS F_floating format
                            5: IEEE High Endian format (BLANK=-FLT_MAX)
                            6: IEEE Low Endian format (BLANK=-FLT_MAX)
              FIN           Input real array containing the
                            reals to be converted.
              FOUT          Output real array containing the
                            converted reals in local format.
              NF            Number of reals in FIN and FOUT.

Updates:      May 16, 1991: KGB, Document created.
              Mar 30, 1993: KGB, Types 5 and 6 implemented.

#<

Fortran to C interface:

@ integer function spfpfl( integer, real, real, integer )

*/

fint	spfpfl_c( fint	*ftype ,
                  float	*fin ,
                  float	*fout ,
                  fint	*nf )
{
   return( spfp_conversion( (*ftype), fin, OS_FLOATING_TYPE, fout, (*nf) ) );
}


/*
#>            spfplf.dc2

Function:     SPFPLF

Purpose:      Converts Single Precision Floating Point numbers from Local
              format to Foreign format.

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          INTEGER SPFPLF( FTYPE ,       Input     INTEGER
                              FIN ,         Input     REAL ARRAY
                              FOUT ,        Output    REAL ARRAY
                              NF )          Input     INTEGER

              SPFPLF        Returns the number of BLANKS inserted because
                            input reals cannot be represented in local
                            reals, or  -1 if FTYPE is unknown.
              FTYPE         Type of foreign format:
                            0: IEEE High Endian format (BLANK=-Inf)
                            1: IEEE Low Endian format (Blank=-Inf)
                            2: CONVEX native format
                            3: VMS D_floating format
                            4: VMS F_floating format
                            5: IEEE High Endian format (BLANK=-FLT_MAX)
                            6: IEEE Low Endian format (BLANK=-FLT_MAX)
              FIN           Input real array containing the
                            reals to be converted.
              FOUT          Output real array containing the
                            converted reals in foreign format.
              NF            Number of reals in FIN and FOUT.

Updates:      May 16, 1991: KGB, Document created.
              Mar 30, 1993: KGB, Types 5 and 6 implemented.

#<

Fortran to C interface:

@ integer function spfplf( integer, real, real, integer )

*/

fint	spfplf_c( fint	*ftype ,
                  float	*fin ,
                  float	*fout ,
                  fint	*nf )
{
   return( spfp_conversion( OS_FLOATING_TYPE, fin, (*ftype), fout, (*nf) ) );
}


/*

Function:     dpfp_conversion

Purpose:      dpfp_conversion does the back and forth conversion of double
              precision reals (doubless). It is not guaranteed that conversion
              from one foreign format to another forteign format works.

Category:     DATA

File:         floating.h

Author:       K.G. Begeman

Use:          fint dpfp_conversion( fint   intype ,
                                    double indouble[] ,
                                    fint   outtype ,
                                    double outdouble[] ,
                                    fint   nd )

              dpfp_conversion    Return -1 when intype or outtype are
                                 not known, else the number of zero
                                 substitutions because of representation
                                 errors.
              intype             type of input floating point format.
              indouble           array of input floating point numbers.
              outtype            type of output floating point format.
              outdouble          array of output floating point numbers.
              nd                 number of doubles to convert.

Updates:      May 17, 1991: KGB Document created.
              Mar 30, 1993: KGB Types 5 and 6 added.
              Apr  6, 1994: KGB Repaired inplace conversion.

*/

static	fint	dpfp_conversion( fint	intype ,
                                 double indouble[] ,
                                 fint	outtype ,
                                 double	outdouble[] ,
                                 fint	nd )
{
   D_UN		bi, bo;				/* the blanks */
   D_UN		dp;				/* the unions */
   D_UN		*din;				/* input */
   D_UN		*dout;				/* output */
   fint		n;				/* loop counter */
   fint		r = 0;				/* return value */
   unsigned int	e;				/* the exponent */
   unsigned int	f;				/* fraction part */
   unsigned int	s;				/* sign */

#if	defined(__vms__)
   init_blank( );
#endif
   din = (D_UN *) indouble;			/* input pointer */
   dout = (D_UN *) outdouble;			/* output pointer */
   if (intype == outtype) {			/* quick solution */
      if (din != dout) {			/* we need to copy */
         for (n = 0; n < nd; n++) {		/* conversion loop */
            dout[n] = din[n];			/* do the conversion */
         }
      }
      return( r );				/* we're done */
   }
   switch( intype ) {				/* which input type */
      case 0:					/* IEEE High Endian (1) -> */
      case 5: {					/* IEEE High Endian (2) -> */
         switch( outtype ) {			/* which output type */
            case 0:				/* -> IEEE High Endian (1) */
            case 5: {				/* -> IEEE High Endian (2) */
#ifdef	SWAP
               bi.l[1] = swap4( DBLANK_TAB[intype].l[0] );
               bi.l[0] = swap4( DBLANK_TAB[intype].l[1] );
               bo.l[1] = swap4( DBLANK_TAB[outtype].l[0] );
               bo.l[0] = swap4( DBLANK_TAB[outtype].l[1] );
#else
               bi = DBLANK_TAB[intype];
               bo = DBLANK_TAB[outtype];
#endif
               for (n = 0; n < nd; n++) {
                  if ((din[n].l[0] == bi.l[0]) && (din[n].l[1] == bi.l[1])) {
                     dout[n] = bo;
                  } else {
                     dout[n] = din[n];
                  }
               }
               break;				/* we're done */
            }
            case 1:				/* -> IEEE Low Endian (1) */
            case 6: {				/* -> IEEE Low Endian (2) */
#ifdef	SWAP
               bi.l[0] = swap4( DBLANK_TAB[intype].l[0] );
               bi.l[1] = swap4( DBLANK_TAB[intype].l[1] );
               bo = DBLANK_TAB[outtype];
#else
               bi = DBLANK_TAB[intype];
               bo.l[0] = swap4( DBLANK_TAB[outtype].l[0] );
               bo.l[1] = swap4( DBLANK_TAB[outtype].l[1] );
#endif
               for (n = 0; n < nd; n++) {	/* conversion loop */
                  if ((din[n].l[0] == bi.l[0]) && (din[n].l[1] == bi.l[1])) {
                     dout[n] = bo;
                  } else {
                     dp = din[n];
                     dout[n].l[1] = swap4( dp.l[0] );
                     dout[n].l[0] = swap4( dp.l[1] );
                  }
               }
               break;				/* we're done */
            }
            case 2: {				/* -> CONVEX Native */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = swap4( din[n].l[0] );
                  dp.l[1] = swap4( din[n].l[1] );
#else
                  dp = din[n];			/* input number */
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     f = ( dp.l[0] & 0x000fffff );
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        dp.l[0] = f;		/* result is zero */
                     } else if (e < 0x000007fe ) {
                        e += 2;			/* multiply by 4 */
                        dp.l[0] = s | (e << 20) | f;
                     } else {			/* conversion error */
                        dp = DBLANK_TAB[outtype];
                        r += 1;			/* we make a blank */
                     }
                  }
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            case 3: {				/* -> VMS D_Floating */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = swap4( din[n].l[0] );
                  dp.l[1] = swap4( din[n].l[1] );
#else
                  dp = din[n];
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     f = ( dp.l[0] & 0x000fffff );
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        dp.l[0] = 0;		/* result is zero */
                     } else if (e > 894 ) {	/* it fits in */
                        e -= 894;		/* decrease exponent */
                        dp.l[0] = s | (e << 23) | (f << 3) | (dp.l[1] >> 29);
                        dp.l[1] = (dp.l[1] << 3 );
                     } else {			/* conversion error */
                        dp = DBLANK_TAB[outtype];
                        r += 1;			/* we make a blank */
                     }
                  }
                  dp.l[0] = swap2( dp.l[0] );
                  dp.l[1] = swap2( dp.l[1] );
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            case 4: {				/* -> VMS G_Floating */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = swap4( din[n].l[0] );
                  dp.l[1] = swap4( din[n].l[1] );
#else
                  dp = din[n];
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     f = ( dp.l[0] & 0x000fffff );
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        dp.l[0] = f;		/* result is zero */
                     } else if (e < 0x000007fe ) {
                        e += 2;			/* multiply by 4 */
                        dp.l[0] = s | (e << 20) | f;
                     } else {			/* conversion error */
                        dp = DBLANK_TAB[outtype];
                        r += 1;			/* we make a blank */
                     }
                  }
                  dp.l[0] = swap2( dp.l[0] );
                  dp.l[1] = swap2( dp.l[1] );
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            default: {				/* unknown type */
               r = -1;
               break;
            }
         }
         break;					/* we're done */
      }
      case 1:					/* IEEE Low Endian (1) -> */
      case 6: {					/* IEEE Low Endian (2) -> */
         switch( outtype ) {			/* which output type */
            case 0:				/* -> IEEE High Endian (1) */
            case 5: {				/* -> IEEE High Endian (2) */
#ifdef	SWAP
               bi = DBLANK_TAB[intype];
               bo.l[0] = swap4( DBLANK_TAB[outtype].l[0] );
               bo.l[1] = swap4( DBLANK_TAB[outtype].l[1] );
#else
               bi.l[0] = swap4( DBLANK_TAB[intype].l[0] );
               bi.l[1] = swap4( DBLANK_TAB[intype].l[1] );
               bo = DBLANK_TAB[outtype];
#endif
               for (n = 0; n < nd; n++) {	/* conversion loop */
                  if ((din[n].l[0] == bi.l[0]) && (din[n].l[1] == bi.l[1])) {
                     dout[n] = bo;
                  } else {
                     dp = din[n];
                     dout[n].l[1] = swap4( dp.l[0] );
                     dout[n].l[0] = swap4( dp.l[1] );
                  }
               }
               break;				/* we're done */
            }
            case 1:				/* -> IEEE Low Endian (1) */
            case 6: {				/* -> IEEE Low Endian (2) */
#ifdef	SWAP
               bi = DBLANK_TAB[intype];
               bo = DBLANK_TAB[outtype];
#else
               bi.l[1] = swap4( DBLANK_TAB[intype].l[0] );
               bi.l[0] = swap4( DBLANK_TAB[intype].l[1] );
               bo.l[1] = swap4( DBLANK_TAB[outtype].l[0] );
               bo.l[0] = swap4( DBLANK_TAB[outtype].l[1] );
#endif
               for (n = 0; n < nd; n++) {	/* conversion loop */
                  if ((din[n].l[0] == bi.l[0]) && (din[n].l[1] == bi.l[1])) {
                     dout[n] = bo;
                  } else {
                     dout[n] = din[n];		/* do the conversion */
                  }
               }
               break;				/* we're done */
            }
            case 2: {				/* -> CONVEX Native */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = din[n].l[1];
                  dp.l[1] = din[n].l[0];
#else
                  dp.l[0] = swap4( din[n].l[1] );
                  dp.l[1] = swap4( din[n].l[0] );
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[1] &&
                      dp.l[1] == DBLANK_TAB[intype].l[0]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     f = ( dp.l[0] & 0x000fffff );
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        dp.l[0] = f;		/* result is zero */
                     } else if (e < 0x000007fe ) {
                        e += 2;			/* multiply by 4 */
                        dp.l[0] = s | (e << 20) | f;
                     } else {			/* conversion error */
                        dp = DBLANK_TAB[outtype];
                        r += 1;			/* we make a blank */
                     }
                  }
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            case 3: {				/* -> VMS D_Floating */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = din[n].l[1];
                  dp.l[1] = din[n].l[0];
#else
                  dp.l[0] = swap4( din[n].l[1] );
                  dp.l[1] = swap4( din[n].l[0] );
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[1] &&
                      dp.l[1] == DBLANK_TAB[intype].l[0]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     f = ( dp.l[0] & 0x000fffff );
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        dp.l[0] = 0;		/* result is zero */
                     } else if (e > 894 ) {	/* it fits in */
                        e -= 894;		/* decrease exponent */
                        dp.l[0] = s | (e << 23) | (f << 3) | (dp.l[1] >> 29);
                        dp.l[1] = (dp.l[1] << 3 );
                     } else {			/* conversion error */
                        dp = DBLANK_TAB[outtype];
                        r += 1;			/* we make a blank */
                     }
                  }
                  dp.l[0] = swap2( dp.l[0] );
                  dp.l[1] = swap2( dp.l[1] );
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            case 4: {				/* -> VMS G_Floating */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = din[n].l[1];
                  dp.l[1] = din[n].l[0];
#else
                  dp.l[0] = swap4( din[n].l[1] );
                  dp.l[1] = swap4( din[n].l[0] );
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[1] &&
                      dp.l[1] == DBLANK_TAB[intype].l[0]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     f = ( dp.l[0] & 0x000fffff );
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        dp.l[0] = f;		/* result is zero */
                     } else if (e < 0x000007fe ) {
                        e += 2;			/* multiply by 4 */
                        dp.l[0] = s | (e << 20) | f;
                     } else {			/* conversion error */
                        dp = DBLANK_TAB[outtype];
                        r += 1;			/* we make a blank */
                     }
                  }
                  dp.l[0] = swap2( dp.l[0] );
                  dp.l[1] = swap2( dp.l[1] );
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            default: {				/* unknown type */
               r = -1;
               break;
            }
         }
         break;					/* we're done */
      }
      case 2: {					/* CONVEX Native -> */
         switch( outtype ) {			/* which output type */
            case 0:				/* -> IEEE High Endian (1) */
            case 5: {				/* -> IEEE High Endian (2) */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = swap4( din[n].l[0] );
                  dp.l[1] = swap4( din[n].l[1] );
#else
                  dp = din[n];			/* input number */
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     f = ( dp.l[0] & 0x000fffff );
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        dp.l[0] = f;		/* result is zero */
                     } else if (e > 0x00000002 ) {
                        e -= 2;			/* multiply by 4 */
                        dp.l[0] = s | (e << 20) | f;
                     } else {			/* underflow */
                        dp.l[0] = f;		/* result is zero */
                     }
                  }
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            case 1:				/* -> IEEE Low Endian (1) */
            case 6: {				/* -> IEEE Low Endian (2) */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = swap4( din[n].l[0] );
                  dp.l[1] = swap4( din[n].l[1] );
#else
                  dp = din[n];			/* input number */
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp.l[0] = DBLANK_TAB[outtype].l[1];
                     dp.l[1] = DBLANK_TAB[outtype].l[0];
                  } else {
                     f = ( dp.l[0] & 0x000fffff );
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        dp.l[0] = f;		/* result is zero */
                     } else if (e > 0x00000002 ) {
                        e -= 2;			/* multiply by 4 */
                        dp.l[0] = s | (e << 20) | f;
                     } else {			/* underflow */
                        dp.l[0] = f;		/* result is zero */
                     }
                  }
#ifdef	SWAP
                  dout[n].l[0] = dp.l[1];
                  dout[n].l[1] = dp.l[0];
#else
                  dout[n].l[0] = swap4( dp.l[1] );
                  dout[n].l[1] = swap4( dp.l[0] );
#endif
               }
               break;				/* we're done */
            }
            case 3: {				/* -> VMS D_Floating */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = swap4( din[n].l[0] );
                  dp.l[1] = swap4( din[n].l[1] );
#else
                  dp = din[n];
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     f = ( dp.l[0] & 0x000fffff );
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        dp.l[0] = 0;		/* result is zero */
                     } else if (e > 896 ) {	/* it fits in */
                        e -= 896;		/* decrease exponent */
                        dp.l[0] = s | (e << 23) | (f << 3) | (dp.l[1] >> 29);
                        dp.l[1] = (dp.l[1] << 3 );
                     } else {			/* conversion error */
                        dp = DBLANK_TAB[outtype];
                        r += 1;			/* we make a blank */
                     }
                  }
                  dp.l[0] = swap2( dp.l[0] );
                  dp.l[1] = swap2( dp.l[1] );
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            case 4: {				/* -> VMS G_Floating */
               for (n = 0; n < nd; n++) {	/* conversion loop */
                  dout[n].l[0] = swap2( din[n].l[0] );
                  dout[n].l[1] = swap2( din[n].l[1] );
               }
               break;				/* we're done */
            }
            default: {				/* unknown type */
               r = -1;
               break;
            }
         }
         break;					/* we're done */
      }
      case 3: {					/* VMS D_Floating -> */
         switch( outtype ) {			/* which output type */
            case 0:				/* -> IEEE High Endian (1) */
            case 5: {				/* -> IEEE High Endian (2) */
               for (n = 0; n < nd; n++) {	/* conversion loop */
                  dp.l[0] = swap2( din[n].l[0] );
                  dp.l[1] = swap2( din[n].l[1] );
#ifdef	SWAP
                  dp.l[0] = swap4( dp.l[0] );
                  dp.l[1] = swap4( dp.l[1] );
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     e = ( dp.l[0] & 0x7f800000 ) >> 23;
                     f = ( dp.l[0] & 0x007fffff );
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        dp.l[0] = 0;		/* result is zero */
                     } else if (s && !e) {	/* reserved operand */
                        dp = DBLANK_TAB[outtype];
                        r += 1;
                     } else {			/* always fits in */
                        e += 894;		/* increase exponent */
                        dp.l[0] = s | (e << 20) | (f >> 3);
                        dp.l[1] = ((dp.l[1] >> 3 ) & 0x1fffffff) | ((f & 0x7) << 29);
                     }
                  }
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            case 1:				/* -> IEEE Low Endian (1) */
            case 6: {				/* -> IEEE Low Endian (2) */
               for (n = 0; n < nd; n++) {	/* conversion loop */
                  dp.l[0] = swap2( din[n].l[0] );
                  dp.l[1] = swap2( din[n].l[1] );
#ifdef	SWAP
                  dp.l[0] = swap4( dp.l[0] );
                  dp.l[1] = swap4( dp.l[1] );
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp.l[0] = DBLANK_TAB[outtype].l[1];
                     dp.l[1] = DBLANK_TAB[outtype].l[0];
                  } else {
                     e = ( dp.l[0] & 0x7f800000 ) >> 23;
                     f = ( dp.l[0] & 0x007fffff );
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        dp.l[0] = 0;		/* result is zero */
                     } else if (s && !e) {	/* reserved operand */
                        dp.l[0] = DBLANK_TAB[outtype].l[1];
                        dp.l[1] = DBLANK_TAB[outtype].l[0];
                        r += 1;
                     } else {			/* always fits in */
                        e += 894;		/* increase exponent */
                        dp.l[0] = s | (e << 20) | (f >> 3);
                        dp.l[1] = ((dp.l[1] >> 3 ) & 0x1fffffff) | ((f & 0x7) << 29);
                     }
                  }
#ifdef	SWAP
                  dout[n].l[0] = dp.l[1];
                  dout[n].l[1] = dp.l[0];
#else
                  dout[n].l[0] = swap4( dp.l[1] );
                  dout[n].l[1] = swap4( dp.l[0] );
#endif
               }
               break;				/* we're done */
            }
            case 2: {				/* -> CONVEX Native */
               for (n = 0; n < nd; n++) {	/* conversion loop */
                  dp.l[0] = swap2( din[n].l[0] );
                  dp.l[1] = swap2( din[n].l[1] );
#ifdef	SWAP
                  dp.l[0] = swap4( dp.l[0] );
                  dp.l[1] = swap4( dp.l[1] );
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     e = ( dp.l[0] & 0x7f800000 ) >> 23;
                     f = ( dp.l[0] & 0x007fffff );
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        dp.l[0] = 0;		/* result is zero */
                     } else if (s && !e) {	/* reserved operand */
                        dp = DBLANK_TAB[outtype];
                        r += 1;
                     } else {			/* always fits in */
                        e += 896;		/* increase exponent */
                        dp.l[0] = s | (e << 20) | (f >> 3);
                        dp.l[1] = ((dp.l[1] >> 3 ) & 0x1fffffff) | ((f & 0x7) << 29);
                     }
                  }
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            case 4: {				/* -> VMS G_Floating */
               for (n = 0; n < nd; n++) {	/* conversion loop */
                  dp.l[0] = swap2( din[n].l[0] );
                  dp.l[1] = swap2( din[n].l[1] );
#ifdef	SWAP
                  dp.l[0] = swap4( dp.l[0] );
                  dp.l[1] = swap4( dp.l[1] );
#else
                  dp = din[n];
#endif
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     e = ( dp.l[0] & 0x7f800000 ) >> 23;
                     f = ( dp.l[0] & 0x007fffff );
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        dp.l[0] = 0;		/* result is zero */
                     } else if (s && !e) {	/* reserved operand */
                        dp = DBLANK_TAB[outtype];
                        r += 1;
                     } else {			/* always fits in */
                        e += 896;		/* increase exponent */
                        dp.l[0] = s | (e << 20) | (f >> 3);
                        dp.l[1] = ((dp.l[1] >> 3 ) & 0x1fffffff) | ((f & 0x7) << 29);
                     }
                  }
                  dp.l[0] = swap2( dp.l[0] );
                  dp.l[1] = swap2( dp.l[1] );
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            default: {				/* unknown type */
               r = -1;
               break;
            }
         }
         break;					/* we're done */
      }
      case 4: {					/* VMS G_Floating -> */
         switch( outtype ) {			/* which output type */
            case 0:				/* -> IEEE High Endian (1) */
            case 5: {				/* -> IEEE High Endian (2) */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = swap4( din[n].l[0] );
                  dp.l[1] = swap4( din[n].l[1] );
#else
                  dp = din[n];			/* input number */
#endif
                  dp.l[0] = swap2( dp.l[0] );
                  dp.l[1] = swap2( dp.l[1] );
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     f = ( dp.l[0] & 0x000fffff );
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        dp.l[0] = f;		/* result is zero */
                     } else if (e > 0x00000002 ) {
                        e -= 2;			/* multiply by 4 */
                        dp.l[0] = s | (e << 20) | f;
                     } else {			/* underflow */
                        dp.l[0] = f;		/* result is zero */
                     }
                  }
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            case 1:				/* -> IEEE Low Endian (1) */
            case 6: {				/* -> IEEE Low Endian (2) */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = swap4( din[n].l[0] );
                  dp.l[1] = swap4( din[n].l[1] );
#else
                  dp = din[n];			/* input number */
#endif
                  dp.l[0] = swap2( dp.l[0] );
                  dp.l[1] = swap2( dp.l[1] );
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp.l[0] = DBLANK_TAB[outtype].l[1];
                     dp.l[1] = DBLANK_TAB[outtype].l[0];
                  } else {
                     f = ( dp.l[0] & 0x000fffff );
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !s) {		/* floating zero ? */
                        dp.l[0] = f;		/* result is zero */
                     } else if (e > 0x00000002 ) {
                        e -= 2;			/* multiply by 4 */
                        dp.l[0] = s | (e << 20) | f;
                     } else {			/* underflow */
                        dp.l[0] = f;		/* result is zero */
                     }
                  }
#ifdef	SWAP
                  dout[n].l[0] = dp.l[1];
                  dout[n].l[1] = dp.l[0];
#else
                  dout[n].l[0] = swap4( dp.l[1] );
                  dout[n].l[1] = swap4( dp.l[0] );
#endif
               }
               break;				/* we're done */
            }
            case 2: {				/* -> CONVEX Native */
               for (n = 0; n < nd; n++) {	/* conversion loop */
                  dout[n].l[0] = swap2( din[n].l[0] );
                  dout[n].l[1] = swap2( din[n].l[1] );
               }
               break;				/* we're done */
            }
            case 3: {				/* -> VMS D_Floating */
               for (n = 0; n < nd; n++) {	/* conversion loop */
#ifdef	SWAP
                  dp.l[0] = swap4( din[n].l[0] );
                  dp.l[1] = swap4( din[n].l[1] );
#else
                  dp = din[n];
#endif
                  dp.l[0] = swap2( dp.l[0] );
                  dp.l[1] = swap2( dp.l[1] );
                  if (dp.l[0] == DBLANK_TAB[intype].l[0] &&
                      dp.l[1] == DBLANK_TAB[intype].l[1]) {
                     dp = DBLANK_TAB[outtype];
                  } else {
                     e = ( dp.l[0] & 0x7ff00000 ) >> 20;
                     f = ( dp.l[0] & 0x000fffff );
                     s = ( dp.l[0] & 0x80000000 );
                     if (!e && !f) {		/* floating zero ? */
                        dp.l[0] = 0;		/* result is zero */
                     } else if (e > 896 ) {	/* it fits in */
                        e -= 896;		/* decrease exponent */
                        dp.l[0] = s | (e << 23) | (f << 3) | (dp.l[1] >> 29);
                        dp.l[1] = (dp.l[1] << 3 );
                     } else {			/* conversion error */
                        dp = DBLANK_TAB[outtype];
                        r += 1;			/* we make a blank */
                     }
                  }
                  dp.l[0] = swap2( dp.l[0] );
                  dp.l[1] = swap2( dp.l[1] );
#ifdef	SWAP
                  dout[n].l[0] = swap4( dp.l[0] );
                  dout[n].l[1] = swap4( dp.l[1] );
#else
                  dout[n] = dp;			/* conversion done */
#endif
               }
               break;				/* we're done */
            }
            default: {				/* unknown type */
               r = -1;
               break;
            }
         }
         break;					/* we're done */
      }
      default: {				/* unknown type */
         r= -1;
         break;
      }
   }
   return( r );					/* return to caller */
}


/*
#>            dpfpfl.dc2

Function:     DPFPFL

Purpose:      Converts Double Precision Floating Point numbers from Foreign
              format to Local format.

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          INTEGER DPFPFL( DTYPE ,       Input     INTEGER
                              DIN ,         Input     DOUBLE PRECISION ARRAY
                              DOUT ,        Output    DOUBLE PRECISION ARRAY
                              ND )          Input     INTEGER

              DPFPFL        Returns the number of zeroes inserted because
                            input doubles cannot be represented in local
                            doubles, or  -1 if DTYPE is unknown.
              DTYPE         Type of foreign format:
                            0: IEEE High Endian format (BLANK=-Inf)
                            1: IEEE Low Endian format (BLANK=-Inf)
                            2: CONVEX native format
                            3: VMS D_floating format
                            4: VMS F_floating format
                            5: IEEE High Endian format (BLANK=-DBL_MAX)
                            6: IEEE Low Endian format (BLANK=-DBL_MAX)
              DIN           Input  array containing the
                            doubles to be converted.
              DOUT          Output double array containing the
                            converted doubles in local format.
              ND            Number of doubles in DIN and DOUT.

Updates:      May 16, 1991: KGB, Document created.
              Mar 29, 1993: KGB, Types 5 and 6 added.

#<

Fortran to C interface:

@ integer function dpfpfl( integer, double precision, double precision, integer )

*/

fint	dpfpfl_c( fint		*dtype ,
                  double	*din ,
                  double	*dout ,
                  fint		*nd )
{
   return( dpfp_conversion( (*dtype), din, OS_FLOATING_TYPE, dout, (*nd) ) );
}


/*
#>            dpfplf.dc2

Function:     DPFPLF

Purpose:      Converts Double Precision Floating Point numbers from Local
              format to Foreign format.

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          INTEGER DPFPLF( DTYPE ,       Input     INTEGER
                              DIN ,         Input     DOUBLE PRECISION ARRAY
                              DOUT ,        Output    DOUBLE PRECISION ARRAY
                              ND )          Input     INTEGER

              DPFPLF        Returns the number of zeroes inserted because
                            input doubles cannot be represented in foreign
                            doubles, or  -1 if DTYPE is unknown.
              DTYPE         Type of foreign format:
                            0: IEEE High Endian format (BLANK=-Inf)
                            1: IEEE Low Endian format (BLANK=-Inf)
                            2: CONVEX native format
                            3: VMS D_floating format
                            4: VMS F_floating format
                            5: IEEE High Endian format (BLANK=-DBL_MAX)
                            6: IEEE Low Endian format (BLANK=-DBL_MAX)
              DIN           Input  array containing the
                            doubles to be converted.
              DOUT          Output double array containing the
                            converted doubles in foreign format.
              ND            Number of doubles in DIN and DOUT.

Updates:      May 16, 1991: KGB, Document created.
              Mar 29, 1993: KGB, Types 5 and 6 added.

#<

Fortran to C interface:

@ integer function dpfplf( integer, double precision, double precision, integer )

*/

fint	dpfplf_c( fint		*dtype ,
                  double	*din ,
                  double	*dout ,
                  fint		*nd )
{
   return( dpfp_conversion( OS_FLOATING_TYPE, din, (*dtype), dout, (*nd) ) );
}

/*
#>            clspfp.dc2

Function:     CLSPFP

Purpose:      Checks local single precion floats whether they are legal
              floating point numbers. If not, they are replaced by BLANKs.

Category:     DATA

File:         floating.c

Author:       K.G. Begeman

Use:          INTEGER CLSPFP( FIN ,    Input/Output    REAL ARRAY
                              NF )        Input        INTEGER

              CLSPFP        Returns the number of BLANKS inserted because
                            input reals cannot be represented.
              FIN           Input/output  array containing the
                            reals to be checked.
              NF            Number of reals in FIN.

Updates:      Oct 26, 1993: KGB, Document created.

#<

Fortran to C interface:

@ integer function clspfp( real, integer )

*/

fint	clspfp_c( float	*fin,
                  fint	*nf )
{
   fint	r = 0;					/* return value */

   switch( OS_FLOATING_TYPE ) {
      case 0:					/* IEEE High Endian */
      case 1:					/* IEEE Low Endian */
      case 5:					/* IEEE High Endian (BLANK2) */
      case 6: {					/* IEEE Low Endian (BLANK2) */
         fint		n;			/* loop counter */
         unsigned int	e;			/* exponent */
         unsigned int	*fl;			/* floats */

         fl = (unsigned int *) fin;		/* floats */
         for ( n = 0; n < (*nf); n++ ) {
            if (fl[n] != FBLANK_TAB[OS_FLOATING_TYPE]) {
               e = ( fl[n] & 0x7f800000 ) >> 23;
               if (e >= 0x000000fe) {
                  r += 1;
                  fl[n] = FBLANK_TAB[OS_FLOATING_TYPE];
               }
            }
         }
         break;
      }
      case 2:					/* CONVEX native */
      case 3:					/* VMS D_floating */
      case 4: {					/* VMS G_floating */
         break;
      }
      default: {
         break;
      }
   }
   return( r );
}

#if	defined(TESTBED)

int main( )
{
   int	ecount = 0;
   printf( "Local floating type = %d\n", OS_FLOATING_TYPE );
   {
      unsigned char	fbuf[7][4*sizeof(float)] = {
         { 0100, 0111, 0017, 0333, 0377, 0200, 0000, 0000,
           0375, 0024, 0233, 0010, 0000, 0000, 0000, 0000 },
         { 0333, 0017, 0111, 0100, 0000, 0000, 0200, 0377,
           0010, 0233, 0024, 0375, 0000, 0000, 0000, 0000 },
         { 0101, 0111, 0017, 0333, 0377, 0377, 0377, 0377,
           0376, 0024, 0233, 0010, 0000, 0000, 0000, 0000 },
         { 0111, 0101, 0333, 0017, 0377, 0377, 0377, 0377,
           0024, 0376, 0010, 0233, 0000, 0000, 0000, 0000 },
         { 0111, 0101, 0333, 0017, 0377, 0377, 0377, 0377,
           0024, 0376, 0010, 0233, 0000, 0000, 0000, 0000 },
         { 0100, 0111, 0017, 0333, 0377, 0177, 0377, 0377,
           0375, 0024, 0233, 0010, 0000, 0000, 0000, 0000 },
         { 0333, 0017, 0111, 0100, 0377, 0377, 0177, 0377,
           0010, 0233, 0024, 0375, 0000, 0000, 0000, 0000 }
      };
      fint		ftype;
      fint		four = 4;
      float		fos[4];

      memcpy( fos, fbuf[OS_FLOATING_TYPE], sizeof(float) * four );
      {
         fint	m;

         printf( "local floats  : " );
         for (m = 0; m < four; m++) {
            if (fblank_c( (unsigned int *)&fos[m] )) {
               printf( "BLANK          " );
            } else {
               printf( "%#14.8g ", (double) fos[m] );
            }
         }
         printf( "\n" );
      }
      for (ftype = 0; ftype < 7; ftype++) {
         fint	m;
         fint	r;
         float	fin[4];
         float	fout[4];

         fout[0] = fout[1] = fout[2] = fout[3] = 0.0;
         memcpy( fin, fbuf[ftype], sizeof(float) * four );
         r = spfpfl_c( &ftype, fin, fout, &four );
         if (r) {
            printf( "spfpfl = %d ftype = %d\n", r, ftype );
         }
         for (m = 0; m < four; m++) {
            if (memcmp( &fout[m], &fos[m], sizeof(float) )) {
               printf( "(%d -> %d) error at %d\n", ftype, OS_FLOATING_TYPE, m );
               ecount++;
            }
         }
      }
      for (ftype = 0; ftype < 7; ftype++) {
         fint	m;
         fint	r;
         float	fout[4];

         fout[0] = fout[1] = fout[2] = fout[3] = 0.0;
         r = spfplf_c( &ftype, fos, fout, &four );
         if (r) {
            printf( "spfplf = %d ftype = %d\n", r, ftype );
         }
         for (m = 0; m < four; m++) {
            if (memcmp( &fout[m], &fbuf[ftype][m*sizeof(float)], sizeof(float) )) {
               printf( "(%d -> %d) error at %d\n", OS_FLOATING_TYPE, ftype, m );
               ecount++;
            }
         }
      }
   }
   {
      unsigned char	dbuf[7][sizeof(double)*4] = {
         { 0100, 0011, 0041, 0373, 0132, 0176, 0321, 0227,
           0377, 0360, 0000, 0000, 0000, 0000, 0000, 0000,
           0307, 0242, 0223, 0141, 0015, 0055, 0043, 0023,
           0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000 },
         { 0227, 0321, 0176, 0132, 0373, 0041, 0011, 0100,
           0000, 0000, 0000, 0000, 0000, 0000, 0360, 0377,
           0023, 0043, 0055, 0015, 0141, 0223, 0242, 0307,
           0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000 },
         { 0100, 0051, 0041, 0373, 0132, 0176, 0321, 0227,
           0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
           0307, 0302, 0223, 0141, 0015, 0055, 0043, 0023,
           0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000 },
         { 0111, 0101, 0332, 0017, 0366, 0323, 0270, 0214,
           0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
           0024, 0376, 0010, 0233, 0151, 0151, 0230, 0030,
           0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000 },
         { 0051, 0100, 0373, 0041, 0176, 0132, 0227, 0321,
           0377, 0377, 0377, 0377, 0377, 0377, 0377, 0377,
           0302, 0307, 0141, 0223, 0055, 0015, 0023, 0043,
           0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000 },
         { 0100, 0011, 0041, 0373, 0132, 0176, 0321, 0227,
           0377, 0357, 0377, 0377, 0377, 0377, 0377, 0377,
           0307, 0242, 0223, 0141, 0015, 0055, 0043, 0023,
           0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000 },
         { 0227, 0321, 0176, 0132, 0373, 0041, 0011, 0100,
           0377, 0377, 0377, 0377, 0377, 0377, 0357, 0377,
           0023, 0043, 0055, 0015, 0141, 0223, 0242, 0307,
           0000, 0000, 0000, 0000, 0000, 0000, 0000, 0000 }
      };
      double		dos[4];
      fint		dtype;
      fint		four = 4;

      memcpy( dos, dbuf[OS_FLOATING_TYPE], sizeof(double) * four );
      {
         fint	m;

         printf( "local doubles : " );
         for (m = 0; m < four; m++) {
            if (dblank_c( (D_UN *) &dos[m] )) {
               printf( "BLANK          " );
            } else {
               printf( "%#14.8g ", dos[m] );
            }
         }
         printf( "\n" );
      }
      for (dtype = 0; dtype < 7; dtype++) {
         fint	m;
         fint	r;
         double	din[4];
         double	dout[4];

         dout[0] = dout[1] = dout[2] = dout[3] = 0.0;
         memcpy( din, dbuf[dtype], sizeof(double) * four );
         r = dpfpfl_c( &dtype, din, dout, &four );
         if (r) {
            printf( "dpfpfl = %d dtype = %d\n", r, dtype );
         }
         for ( m = 0; m < four; m++ ) {
            if (memcmp( &dout[m], &dos[m], sizeof(double))) {
               printf( "(%d -> %d) error at %d\n", dtype, OS_FLOATING_TYPE, m );
               ecount++;
            }
         }
      }
      for (dtype = 0; dtype < 7; dtype++) {
         fint	m;
         fint	r;
         double	dout[4];

         dout[0] = dout[1] = dout[2] = dout[3] = 0.0;
         r = dpfplf_c( &dtype, dos, dout, &four );
         if (r) {
            printf( "dpfplf = %d dtype = %d\n", r, dtype );
         }
         for (m = 0; m < four; m++) {
            if (memcmp( &dout[m], &dbuf[dtype][m*sizeof(double)], sizeof(double) )) {
               printf( "(%d -> %d) error at %d\n", OS_FLOATING_TYPE, dtype, m );
               ecount++;
            }
         }
      }
   }
   printf( "%d errors\n", ecount );
   return( 0 );
}
#endif
