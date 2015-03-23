/* userfio.h

           Copyright (c) 1991
  Laboratory for Space Research Groningen 
       Kapteyn Laboratory Groningen 
           All Rights Reserved.

#>            userfio.dc2
Function:     userfio.h

Purpose:      Easy-C companions for GIPSY userinterface routines

Category:     General C; Userinterface

Author:       Do Kester

Description:
	This header file contains the declarations for 
	    anyoutf		see anyoutf.dc2
	    strarf		see strarf.dc2
	    errorf		see errorf.dc2
	    statusf 		see statusf.dc2
	    wkeyf               see wkeyf.dc2
	    rejectf             see rejectf.dc2
	    xeqcontf            see xeqcont.dc2
	and for the userinterface routines; 
	see userfxxx.dc2 for common features in all userfxxx.
	    userfint		see userfint.dc2
	    userfchar		etc.
	    userfcharl
	    userfcharu
	    userftext
	    userfreal
	    userfdble
	    userfangle
	    userflog.
	There is an alias for cancel_c:
	    #define cancel( key )	cancel_c( tofchar( key ) )
	Also constants are defined for 
	    anyout-devices 	ANYOUT_DEF, ANYOUT_TERM, ANYOUT_LOG,
				ANYOUT_NOEXP, ANYOUT_TST, ANYOUT_DEBUG
	    error-levels 	FATAL, SERIOUS, MINOR, WARNING
	    default-levels	DFLT_NONE, DFLT_DEF, DFLT_HIDD, DFLT_NOREAD,
				DFLT_EXACT, DFLT_DEFEXACT, DFLT_HIDEXACT
				DFLT_DONE

Updates:      06 Aug 1991: DK,  Creation date
              04 Jun 1992: DK,  into the system.
              25 Oct 1995: DK,  add strarf and ANYOUT_DEBUG
              21 Apr 1998: JPT, add wkeyf and rejectf
              17 Jan 2001: JPT, add xeqcontf
              04 May 2001: JPT, DFLT_NOREAD and DFLT_DONE default levels
#<
*/

#include "cancel.h"

/* declarations of the companions to the userfxxx_c functions */

int userfchar(  fchar,    int, int, char*, char*, ... ) ;
int userfcharl( fchar,    int, int, char*, char*, ... ) ;
int userfcharu( fchar,    int, int, char*, char*, ... ) ;
int userftext(  fchar,         int, char*, char*, ... ) ;
int userfint(   fint *,   int, int, char*, char*, ... ) ;
int userfreal(  float *,  int, int, char*, char*, ... ) ;
int userfdble(  double *, int, int, char*, char*, ... ) ;
int userfangle( double *, int, int, char*, char*, ... ) ;
int userflog(   bool *,   int, int, char*, char*, ... ) ;

/* a macro suffices for cancel */
#define cancel( b ) 	cancel_c( tofchar(b) )

/* declarations of variable argument equivalents of .. */

void anyoutf( int, char*, ... ) ;	/* .. anyout  */
void errorf( int, char*, ... ) ;	/* .. error   */
void statusf( char*, ... ) ;		/* .. status  */
void wkeyf( char*, ... ) ;		/* .. wkey    */
void rejectf( char*, char*, ... ) ;	/* .. reject  */
int  xeqcontf(char*, char*, ... ) ;     /* .. xeqcont */

/* write array into string */
char* strarf( char*, char*, void*, long, long ) ;

/* definitions for error levels */

#define	FATAL		4	/* error level fatal   */
#define	SERIOUS		3	/* error level serious */
#define	MINOR		2	/* error level minor   */
#define	WARNING 	1	/* error level warning */

/* definitions for anyout levels */

#define	ANYOUT_DEF  	0	/* anyout_level_default   */
#define	ANYOUT_TERM 	1	/* anyout_level_terminal  */
#define	ANYOUT_LOG  	2	/* anyout_level_logfile   */
#define	ANYOUT_NOEXP	8	/* anyout_level_dumb_user */
#define	ANYOUT_TST  	16	/* anyout_level_test      */
#ifndef ANYOUT_DEBUG
#define ANYOUT_DEBUG	128	/* works only with anyoutf */
#endif

/* definitions for default levels */

#define	DFLT_NONE	0	/* default_no_default  */
#define	DFLT_DEF 	1	/* default_has_default */
#define	DFLT_HIDD       2	/* default_hidden_key  */
#define DFLT_NOREAD     3       /* access input but do not return data */
#define	DFLT_EXACT      4	/* exact_number */
#define	DFLT_DEFEXACT   5	/* default and exact */
#define	DFLT_HIDEXACT	6	/* hidden and exact */
#define DFLT_DONE       8       /* indicate KEYCHANGE processing finished */
