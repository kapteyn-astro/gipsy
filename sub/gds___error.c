/* gds___error.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            gds___error.dc3

Function:     GDS___ERROR

Purpose:      Central error handler for GDS.

Category:     GDS

Files:        gds___error.c

Author:       J.P. Terlouw

Use:          CALL GDS___ERROR( CODE )       Input    INTEGER

              CODE       Error code (<0). The error is converted to an
                         error message. If code<1000, then an extra
                         message precedes the final message. This feature
                         is used when a subroutine is called with the
                         error code already <0, i.e. an error has occurred
                         in a previous call and was not cleared.

Notes:        In most cases GDS___ERROR is called indirectly via
              GDS___CHECK. GDS___ERROR must be called directly when a
              routine is called with a wrong number of arguments.

Updates:     Apr  2, 1987: JPT, original document.
             Dec 21, 1989: JPT, GPS version.
             Jun 28, 1990: KGB, converted to C because of SUN f77.
             Mar  2, 1994: JPT, modified for GDS server.
             Apr  5, 2011: JPT, added code 72.

#<

Fortran to C interface:

@ subroutine gds___error( integer )
*/

#include	"stdio.h"		/* <stdio.h> */
#include	"string.h"		/* <string.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"error.h"		/* declare error_c */

/*
 * Here follows the list of known GDS error codes. To add a new code, simply
 * put the associated message at the end of the following initialization and
 * update the file gdserrors.h.
 */

static char *messages[] = {		/* these are the messages */
   " ", 		                /* success code */
   "GDS -- fatal error", 				/*  1 */
   "GDSD_WRITE -- bad writing position", 		/*  2 */
   "GDSD_READ -- item not found", 			/*  3 */
   "GDSD_READ -- more data requested than present", 	/*  4 */
   "GDS -- cannot open set",		 		/*  5 */
   "GDSD_LENGTH -- item not found", 			/*  6 */
   "GDSD_REWIND -- item not found", 			/*  7 */
   "GDSD_CREATE -- cannot create set", 			/*  8 */
   "GDSD_DELETE -- cannot delete set", 			/*  9 */
   "GDS -- wrong number of arguments", 			/* 10 */
   "GDSC_GRID -- axis name not unique", 		/* 11 */
   "GDSC_WORD -- axis name not unique", 		/* 12 */
   "GDSC_RANGE -- more than one dimension unspecified", /* 13 */
   "GDSC_GRID -- axis not present", 			/* 14 */
   "GDSC_WORD -- axis not present", 			/* 15 */
   "GDSC_SIZE -- axis not present", 			/* 16 */
   "GDSC_ORIGIN -- axis not present", 			/* 17 */
   "GDSD_RFITS -- item not FITS type", 			/* 18 */
   "GDSC_GRID -- coordinate not defined", 		/* 19 */
   "GDSC_RANGE -- no undefined coordinates", 		/* 20 */
   "GDSD_R/Wxxx -- format error", 			/* 21 */
   "GDSD_Rxxx -- incompatible FITS type", 		/* 22 */
   "GDSC_SIZE -- axis name not unique", 		/* 23 */
   "GDSC_ORIGIN -- axis name not unique", 		/* 24 */
   "GDSD_RVAR -- record length exceeds buffer space", 	/* 25 */
   "GDSD_RVAR -- invalid record", 			/* 26 */
   "GDSD_WVAR -- record too big", 			/* 27 */
   "GDS_EXTEND -- axis already present", 		/* 28 */
   "GDSI_READ -- function call has buffer length <> 1", /* 29 */
   "GDSI_READ -- pixels done <> buffer length", 	/* 30 */
   "GDSI_XXXX -- illegal transfer identifier",		/* 31 */
   "GDSI_READ/WRITE -- Cannot allocate enough memory",	/* 32 */
   "GDSD_WRITE -- cannot extend file", 			/* 33 */
   "GDSC_WORD -- coordinate outside range: fixed", 	/* 34 */
   "GDSC_WORD -- coordinate > maximum", 		/* 35 */
   "GDSI_READ/WRITE -- cannot open data file", 		/* 36 */
   "GDS -- corrupt descriptor file (truncated)",	/* 37 */
   "GDS -- corrupt descriptor file (bad link)",		/* 38 */
   "GDSINP -- no such set", 				/* 39 */
   "GDSINP -- set already present", 			/* 40 */
   "NSUB -- insufficient header information", 		/* 41 */
   "NSUB -- dimensionality not in structure", 		/* 42 */
   "NXTSUB -- no (more) substructures", 		/* 43 */
   "GDSD_TYPE -- item not FITS type", 			/* 44 */
   "GDSD_RFITS -- item type is INT", 			/* 45 */
   "GDSD_RFITS -- item type is REAL", 			/* 46 */
   "GDSD_RFITS -- item type is DBLE", 			/* 47 */
   "GDS -- incompatible file version",			/* 48 */
   "GDS_EXIST -- bad descriptor file",                  /* 49 */
   "GDS_EXIST -- incompatible byte order",              /* 50 */
   "GDS -- insufficient privilege for operation",       /* 51 */
   "GDS -- set is currently locked",                    /* 52 */
   "GDS_UNLOCK -- set is not locked",                   /* 53 */
   "GDS -- undefined function",				/* 54 */
   "GDS -- set is not open",                            /* 55 */
   "GDS -- no server contact (check disk space)",       /* 56 */
   "GDS -- invalid set handle",                         /* 57 */
   "GDS -- memory allocation failure",                  /* 58 */
   "GDS -- failed to rename set",                       /* 59 */
   "GDS -- bad descriptor header",                      /* 60 */
   "GDS -- cannot access directory",                    /* 61 */
   "GDS -- error code 62",
   "GDS -- error code 63",
   "GDS -- error code 64",
   "GDS -- error code 65",
   "GDSA_TABLE -- descriptor file not present",         /* 66 */
   "GDSA_TABLE -- illegal data type",                   /* 67 */
   "GDSA_RCxxx -- reading past end of information",     /* 68 */
   "GDSA_WCxxx -- attempt to skip rows in writing",     /* 69 */
   "GDSA_RDCOM -- end of information",                  /* 70 */
   "GDSA_TABLE -- number of items in list too small",   /* 71 */
   "GDS_EXTEND -- axis too long"			/* 72 */
   
   /* PUT HERE YOUR NEW ERROR MESSAGE(S) */
};

/* MAXMESNUM is the first unknown error code */
#define	MAXMESNUM	(sizeof( messages ) / sizeof( char * ))

extern void gds___error_c( fint *code )
{
   fint  kode;				/* local code */
   fint  errlev;			/* error level */

   kode = *code;			/* copy error code */
   if (kode < -1000) {
      errlev = 2;			/* set error level */
      error_c( &errlev, tofchar( "Routine called with error code present" ) );
      kode += 1000;			/* normalize error code */
   }
   kode = -kode;			/* negate error code */
   if (kode > 0 && kode < MAXMESNUM) {	/* known error code */
      errlev = 4;			/* set error level */
      error_c( &errlev, tofchar( messages[kode] ) );
   } else {
      char errmesbuf[40];		/* buffer for message */
      
      errlev = 4;			/* set error level */
      (void) sprintf( errmesbuf, "GDS error - code = %d", *code );
      error_c( &errlev, tofchar( errmesbuf ) );
   }
}
/*
#>gds_errstr.dc2
Function:      GDS_ERRSTR

Purpose:       Obtain the message string associated with a GDS error code.

Category:      GDS

File:          gds___error.c

Author:        J.P. Terlouw

Use:           CHARACTER*(*) GDS_ERRSTR( CODE )    Input   INTEGER

Updates:       Mar 30, 1994: JPT, Document created.
#<
Fortran to C interface:
@ character function gds_errstr( integer )
*/
extern void gds_errstr_c( fchar chr, fint *code )
{
   fint  kode;
    
   kode = *code;
   if (kode>0) kode = 0;
   if (kode < -1000) {
      kode += 1000;
   }
   kode = -kode;
   if (kode > 0 && kode < MAXMESNUM) {
      (void)str2char(messages[kode],chr);
   } else {
      (void)str2char("Undefined GDS error code",chr);
   }
}
