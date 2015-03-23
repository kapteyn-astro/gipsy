/* rfits.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            rfits.dc1

Program:      RFITS

Purpose:      Load FITS files from tape into a GIPSY set.

Category:     FITS, TAPES, UTILITY

File:         rfits.c

Author:       K.G. Begeman

Description:  The program RFITS is designed to load FITS files into
              GIPSY sets. Since the F in FITS stands for flexible,
              it is not that simple to comprehend the data structure
              which is to be loaded. Therefore RFITS needs a lot of
              interaction with the user, and the user must know what
              is on tape and where it is on tape.
              Since this program must also be flexible, it will always
              be in devellopment, which means that there will be
              a constant interaction between the user and the programmer.
              So don't hesitate to bring forward suggestions for
              improvement, or even criticism.

Keywords:

*** AUTO=     Try to run in automatice mode [Y].
              In automatice mode the program will do its best to
              comprehend the FITS structure on tape. FITS items for
              which the program has some reasonable defaults will
              not be asked. In non-automatic mode all items will be
              asked.

*** NODATA=   Create only set headers [N].
              With NODATA=Y no data is read, only the set headers
              are created.

*** HISTORY=  Copy history information from FITS file to Set header [Y].

*** COMMENT=  Copy comments from FITS file to Set Header [Y].

    FITSFILE= Name of file with FITS data [read from tape device].
              If a file name is entered, the keywords INTAPE= and
              INFILES= will be skipped. This keyword, and all the
              following keywords are repeated. After the first filename
              is read, the default will change to [RFITS quits].

    INTAPE=   Tape device to load from [list of all tape devices].

    INFILES=  File numbers on tape to load data from [RFITS quits].
              Maximum number of files is 200. The first time this
              keyword is asked, the current file on tape has file
              number 0, the previous file (if present) has file number
              -1 and the next file has file number +1. All numbers will
              be relative to this initial position. It is also important
              to know that the program reads the tape in forward direction,
              meaning that it does not care about the order of file numbers
              entered by the user.
              This keyword, and all the following keywords are repeated.

    OUTSET=   Set to load data into. The following keywords depend
              on whether the set already exists or not.

*** BLANK=    Redefine the FITS BLANK value as specified in the FITS
              header [value from FITS header]. This option is useful
              if the program which created the FITS file made a
              mistake.

              --------------------------------------------------------
              The following keywords deal with the FITS structure on
              tape. If the program comprehends this structure, the
              keywords will be hidden. You can force all keywords to
              be prompted with AUTO=N.
              The asterix in the keyword will be replaced by the
              corresponding axis number.
              --------------------------------------------------------

    CTYPE*=   Enter the corresponding axis name. Default is chosen as
              smart as possible. If the set does not exist, the user may
              type any other legal axis name. If the axis name found in
              the FITS file is NOT a correct axis name, the user must
              supply a legal axis name.
              It is also possible to SKIP an axis or to HIDE an axis.
              The default will again be chosen as smart as possible.
              NOTE: Frequency axes to which a velocity system is
              attached must be indicated by FREQ-OHEL, FREQ-OLSR,
              FREQ-RHEL or FREQ-RLSR.
              NOTE: This keyword is also asked (hidden) if the set
              does not yet exist. It will ask for extra axes names
              to be added to the new set, all with length 1.

    CUNIT*=   Enter the units of the axis on the FITS tape. This
              keyword only appears if the set does not yet exist
              and the corresponding FITS descriptor is not present.
              Default depends on CTYPE and is normally as prescribed
              by the FITS standard.

    CRVAL*=   Enter the reference value on the axis. This keyword only
              appears if the set does not yet exist.
              Default will be chosen as best we can.

    CDELT*=   Enter the pixel separation along the axis. This keyword only
              appears if the set does not yet exist.
              Note: WSRT data might have the wrong sign for CDELT3.

*** LIMITS*=  Enter the range in grids along the axis [whole axis].
              If the set does already exist, this keyword will not be asked.
              Note that GIPSY does not allow to extend axes in any
              direction. Only the last axis in a GIPSY set may be
              extended, so with this keyword you can make room for
              data which should be loaded at lower grid positions.

    GRID*=    Enter the first grid position along the axis [first on axis].
              This keyword is only asked if the set does not yet exist.
              Default will be chosen as best we can.

    FREQ0=    Rest frequency of observation in Hz. This keyword only
              appears if FITS descriptor not found in FITS file and
              the axis is a frequency axis with velocity information.

    VEL=      Velocity of first grid on axis. This keywords is only asked
              for old FITS files which have been written by VMS GIPSY.

Example:      <USER  > RFITS,AUTO=N
              <USER  > RFITS,INTAPE=IMPORT
              <USER  > RFITS,INFILES=31
              <USER  > RFITS,OUTSET=NGC4214
              <USER  > RFITS,CTYPE1=RA-NCP
              <USER  > RFITS,CUNIT1=
              <USER  > RFITS,LIMITS1=
              <USER  > RFITS,GRID1=
              <USER  > RFITS,CTYPE2=DEC-NCP
              <USER  > RFITS,CUNIT2=
              <USER  > RFITS,LIMITS2=
              <USER  > RFITS,GRID2=
              <USER  > RFITS,CTYPE3=FREQ-OHEL
              <USER  > RFITS,CUNIT3=MHZ
              <USER  > RFITS,CDELT3=-2.5/64
              <USER  > RFITS,LIMITS3=1 32
              <USER  > RFITS,GRID3=32
              <USER  > RFITS,FREQ0=
              <USER  > RFITS,CTYPE4=
              <USER  > RFITS,CTYPE5=
              <USER  > RFITS,INFILES=

Updates:      May 24, 1990: KGB, Document created.
              Nov 26, 1991: KGB, Automatic mode implemented.
              Feb 26, 1993: KGB, Modification for NEWSTAR FITS.
              Mar 19, 1993: KGB, Extending the dimensions of new set.
              Apr 23, 1993: KGB, Bug repaired, keyword NODATA added.
              Apr 14, 1994: KGB, Bug when reading multiple NMAP cubes repaired.
              Feb  5, 1996: KGB, Keyword FITSFILE= implemented.
              Dec 14, 1999: VOG, Allow axis names X1...Xn as default for 
                                 missing CTYPEs in FITS header
              Mar  3, 2005: JPT, Implemented 64-bit float (BITPIX = -64)
              Apr 12, 2009: VOG, Replaced definition of function nint with
                                 one that uses function floor(). This is to
                                 enforce compatibility with other coordinate
                                 routines and to avoid problems with values
                                 of CRPIX that end on .5
#<

*/

/*
 * include files:
 */

#include	"ctype.h"		/* <ctype.h> */
#include	"float.h"		/* <float.h> */
#include	"limits.h"		/* <limits.h> */
#include	"math.h"		/* <math.h> */
#include        "stdio.h"               /* <stdio.h> */
#include        "stdlib.h"              /* <stdlib.h> */
#include        "string.h"              /* <string.h> */
#include        "gipsyc.h"              /* GIPSY definitions and symbols */
#include	"cmain.h"		/* C MAIN PROGRAM */
#include        "anyout.h"              /* define anyout_c */
#include	"axtype.h"		/* define axtype_c */
#include        "cancel.h"              /* define cancel_c */
#include	"clspfp.h"		/* define clspfp_c */
#include        "dcdint.h"              /* define dcdint_c */
#include        "error.h"               /* define error_c */
#include	"factor.h"		/* define factor_c */
#include        "finis.h"               /* define finis_c */
#include	"ftsd_find.h"		/* define ftsd_find_c */
#include	"ftsd_rchar.h"		/* define ftsd_rchar_c */
#include	"ftsd_rdble.h"		/* define ftsd_rdble_c */
#include	"ftsd_rint.h"		/* define ftsd_rint_c */
#include	"ftsd_rlog.h"		/* define ftsd_rlog_c */
#include	"gds_close.h"		/* define gds_close_c */
#include	"gds_create.h"		/* define gds_create_c */
#include	"gds_exist.h"		/* define gds_exist_c */
#include	"gds_extend.h"		/* define gds_extend_c */
#include	"gdsc_grid.h"		/* define gdsc_grid_c */
#include	"gdsc_substruct.h"	/* define gdsc_substruct_c */
#include	"gdsc_word.h"		/* define gdsc_word_c */
#include	"gdsd_length.h"		/* define gdsd_length_c */
#include	"gdsd_rchar.h"		/* define gdsd_rchar_c */
#include	"gdsd_rdble.h"		/* define gdsd_rdble_c */
#include	"gdsd_rint.h"		/* define gdsd_rint_c */
#include	"gdsd_wchar.h"		/* define gdsd_wchar_c */
#include	"gdsd_wdble.h"		/* define gdsd_wdble_c */
#include	"gdsd_wfits.h"		/* define gdsd_wfits_c */
#include	"gdsi_write.h"		/* define gdsi_write_c */
#include	"gdsd_wvar.h"		/* define gdsd_wvar_c */
#include        "init.h"                /* define init_c */
#include	"initptr.h"		/* define initptr_c */
#include	"insideptr.h"		/* define insideptr_c */
#include	"mtbsf.h"		/* define mtbsf_c */
#include	"mtbsr.h"		/* define mtbsr_c */
#include        "mtclose.h"             /* define mtclose_c */
#include	"mtfsf.h"		/* define mtfsf_c */
#include	"mtname.h"		/* define mtname_c */
#include        "mtopen.h"              /* define mtopen_c */
#include	"mtreadc.h"		/* define mtreadc_c */
#include	"mtrew.h"		/* define mtrew_c */
#include	"nelc.h"		/* define nelc_c */
#include	"reject.h"		/* define reject_c */
#include	"setfblank.h"		/* define setfblank_c */
#include        "sortia.h"              /* define sortia_c */
#include	"spfpfl.h"		/* define spfpfl_c */
#include	"dpfpfl.h"		/* define dpfpfl_c */
#include	"status.h"		/* define status_c */
#include	"usercharu.h"		/* define usercharu_c */
#include	"userdble.h"		/* define userdble_c */
#include	"userint.h"		/* define userint_c */
#include	"userlog.h"		/* define userlog_c */
#include        "usertext.h"            /* define usertext_c */
#include	"velpro.h"		/* define velpro_c */


/*
 * Define the keywords and associated messages.
 */

#define	VERSION		"1.3"		/* change version number on this line */

#define	AUTO_KEY	tofchar("AUTO=")
#define	AUTO_MES	tofchar("Automatic mode [Y]")

#define	COMMENT_KEY	tofchar("COMMENT=")
#define	COMMENT_MES	tofchar("Copy FITS comments? [Y]")

#define	FITSFILE_KEY	tofchar("FITSFILE=")
#define	FITSFILE_MES1	tofchar("Name of FITS file [read from tape device]")
#define	FITSFILE_MES2	tofchar("Name of FITS file [program quits]")

#define	FMAKE_ERR	tofchar("Cannot allocate enough memory!")

#define	HISTORY_KEY	tofchar("HISTORY=")
#define	HISTORY_MES	tofchar("Copy FITS history? [Y]")

#define INTAPE_DEV      tofchar("?INTAPE=Tape device to load from [list of all tape devices]")
#define INTAPE_ERR1     tofchar("No such tape device!")

#define INFILES_KEY     tofchar("INFILES=")
#define INFILES_MES     tofchar("Give file numbers on tape [program quits]")

#define	MAIN_ERR1	tofchar("Program expects 8 bit bytes!")
#define	MAIN_ERR2	tofchar("Program expects 1 byte chars!")
#define	MAIN_ERR3	tofchar("Program expects 2 byte shorts!")
#define	MAIN_ERR4	tofchar("Program expects 4 byte ints!")
#define	MAIN_ERR5	tofchar("Program expects 4 byte floats!")

#define	NODATA_KEY	tofchar("NODATA=")
#define	NODATA_MES	tofchar("Skip reading in the image part? [N]")

#define	OKAY_KEY	tofchar("OKAY=")
#define	OKAY_MES	tofchar("Set already exists. Overwrite? [Y]/N")

#define OUTSET_KEY      tofchar("OUTSET=")
#define OUTSET_MES      tofchar("Set to load data into")

#define	POSITION_ERR	tofchar("error positioning tape!")

#define	READHEADER_ERR1	tofchar("End Of Information encountered!")
#define READHEADER_ERR2	tofchar("Wrong block size!")
#define	READHEADER_ERR3	tofchar("Tape error ?!?")
#define	READHEADER_ERR4	tofchar("Error reading FITS header!")
#define	DECODEHEAD_ERR1	tofchar("FITS descriptor SIMPLE not present!")
#define	DECODEHEAD_ERR2	tofchar("SIMPLE = FALSE (will try anyway!)")
#define	DECODEHEAD_ERR3	tofchar("FITS descriptor BITPIX nor present!")
#define	DECODEHEAD_ERR4	tofchar("BITPIX not compatible with this machine!")
#define	DECODEHEAD_ERR5	tofchar("Descriptor NAXIS not present!")

#define	GAMBLE1_ERR1	tofchar("Cannot hide this axis! Try again!")
#define	GAMBLE1_ERR2	tofchar("Cannot skip this axis! Try again!")
#define	GAMBLE1_ERR3	tofchar("Unknown axis! Try again!")
#define	GAMBLE1_ERR4	tofchar("Illegal limits! Try again!")
#define	GAMBLE1_ERR5	tofchar("No data in range! Try again!")

#define	GAMBLE2_ERR1	tofchar("Axis not in set!")
#define	GAMBLE2_ERR2	tofchar("GRID outside range!")
#define	GAMBLE2_ERR3	tofchar("Error extending set!")
#define	GAMBLE2_ERR4	tofchar("Illegal limits! Try again!")
#define	GAMBLE2_ERR5	tofchar("No data in range! Try again!")

#define	COPYDATA_ERR1	tofchar("GRID outside range!" )
#define	COPYDATA_ERR2	tofchar("Error extending set!")
#define	COPYDATA_ERR3	tofchar("BITPIX not compatible with this machine!")
#define	COPYDATA_ERR4	tofchar("End Of Information encountered!")
#define COPYDATA_ERR5	tofchar("Wrong block size!")
#define	COPYDATA_ERR6	tofchar("Tape error ?!?")
#define	COPYDATA_ERR7	tofchar("Error skipping to next file!")


/*
 * define some parameters and functions:
 */

#define	MUNIT		1		/* mask for CUNIT/DUNIT */
#define	MTYPE		2		/* mask for CTYPE/DTYPE */
#define	MRVAL		4		/* mask for CRVAL/DRVAL */
#define	MRPIX		8		/* mask for CRPIX/DRPIX */
#define MROTA		16		/* mask for CROTA/DROTA */
#define	MDELT		32		/* mask for CDELT/DDELT */

#define	DSC_SKIP	1		/* skip this descriptor */
#define	DSC_SUB		2		/* descriptor at subset level */
#define	DSC_CHECK	4		/* dont't overwrite old descriptor */
#define	DSC_COMMENT	8		/* comment */
#define	DSC_HISTORY	16		/* history */

#define	MAXAXES		16		/* maximum number of axes possible */
					/* size of descriptor list */
#define	DSCLISTLEN	(sizeof(dsc_buf)/sizeof(dsc_struct))
#define	MAXDEVNAMLEN	(FILENAME_MAX)	/* maximum length of tape name */
#define MAXFTSDSCLEN    8		/* length of FITS descriptor */
#define MAXFTSNAMLEN    18              /* length of FITS character value */
#define	MAXFTSCHRLEN	78		/* .. */
#define MAXSETNAMLEN    80		/* maximum length of set name */
#define	MAXSYNONYMS	100		/* maximum numver of synonyms */
#define MAXTEXTLEN      128             /* maximum length of text strings */
#define MAXFILES        200		/* maximum number of files */
#define FITSRECLEN	80		/* length of a FITS record on tape */
#define FITSBLOCKLEN    2880		/* length of FITS blocks on tape */
#define FITSBLOCKFACTOR 1		/* FITS blocking factor (STANDARD) */


/*
 * some macros:
 */

#define	READ( r, b )					\
if ( fid != NULL ) {					\
   tst = fread( b.a, sizeof( char ), b.l, fid );	\
} else {						\
   tst = mtreadc_c( &mtid, b );				\
}
#define	PANIC( s )					\
{							\
   fint	elev = 4;					\
   if ( fid == NULL ) {					\
      (void) mtrew_c( &mtid );				\
      (void) mtclose_c( &mtid );			\
   } else {						\
      (void) fclose( fid );				\
   }							\
   error_c( &elev, s );					\
}


/*
 * define the structures:
 */

typedef struct {			/* structure for axis info */
   fint   naxis;                        /* length of an axis */
   fint   pmask;                        /* mask for primary axis */
   double cdelt;                        /* increment in units along axis */
   double crota;                        /* rotation angle of axis */
   double crpix;                        /* reference pixel of axis */
   double crval;                        /* reference value at reference pixel */
   char   ctypeb[MAXFTSNAMLEN];		/* axis name */
   char   cunitb[MAXFTSNAMLEN];         /* units of axis */
   fchar  ctype;			/* f character points to ctypeb */
   fchar  cunit;			/* f character points to cunitb */
   fint   smask;                        /* mask for secondary axis */
   double ddelt;                        /* increment along secondary axis */
   double drota;                        /* secondary rotation angle of axis */
   double drpix;                        /* secondary reference pixel of axis */
   double drval;                        /* units at secondary reference pixel */
   char   dtypeb[MAXFTSNAMLEN];         /* secondary axis name */
   char   dunitb[MAXFTSNAMLEN];         /* secondary units of axis */
   fchar  dtype;			/* f character points to dtypeb */
   fchar  dunit;			/* f character points to dunitb */
   fint   matchax;			/* matching axis */
   fint   axtype;			/* type of axis */
   fint   skysys;			/* sky system code */
   fint   prosys;			/* projection system code */
   fint   velsys;			/* velocity system code */
   fint   low;                          /* lower grid value on axis */
   fint   upp;                          /* upper grid value on axis */
   fint   min;				/* wanted lower grid value on axis */
   fint   max;				/* wanted upper grid value on axis */
   fint   grid;				/* grid position */
} ax_struct;

typedef struct {			/* set info structure */
   ax_struct ax[MAXAXES];		/* contains axis info */
   char      setb[MAXSETNAMLEN];	/* name of set */
   char      instrumeb[MAXFTSNAMLEN];	/* name of instrument */
   fchar     set;			/* f character points to setb */
   fchar     instrume;			/* f character points to instrumeb */
   fint      subset;			/* level of subset */
   double    epoch;			/* epoch of set */
   double    freq0;			/* rest frequency of set */
   fint      exist;			/* does set exist */
   fint      naxis;			/* number of real axes */
   fint      maxis;			/* total number of axes */
   float     blank;			/* blank value */
} set_struct;

typedef struct {			/* fits info structure */
   ax_struct ax[MAXAXES];		/* contains axis info */
   char      devb[MAXDEVNAMLEN+1];	/* buffer for tape device name */
   fchar     dev;			/* f character points to devb */
   char      instrumeb[MAXFTSNAMLEN];	/* name of instrument */
   fchar     instrume;			/* f character points to instrumeb */
   double    bscal;			/* data scaling factor */
   double    bzero;			/* data offset factor */
   double    epoch;			/* epoch of set */
   double    freq0;			/* rest frequency of set */
   fint      axused;			/* number of axes in used already */
   fint      nbytes;			/* number of bytes / pixel */
   fint      bitpix;			/* number of bits / pixel */
   fint      naxis;			/* number of real axes */
   fint      maxis;			/* total number of axes */
   fint      blank;			/* blank value on tape */
} fts_struct;

typedef struct {			/* synonym structure */
   char  cnameb[MAXFTSNAMLEN];		/* buffer for synonym */
   fchar cname;				/* points to cnameb */
   char  ctypeb[MAXFTSNAMLEN];		/* buffer for ctype */
   fchar ctype;				/* points to ctypeb */
} syn_struct;				/* THE STRUCT */

typedef struct {			/* descriptor structure */
   char *dsc;				/* descriptor name */
   char	*rep;				/* replace by ... */
   int	 code;				/* code */
} dsc_struct;				/* THE STRUCT */

typedef	unsigned char	byte;		/* make'em unsigned */


/*
 * define the global variables:
 */

static	fts_struct	fts_buf;		/* contains FITS info */
static	set_struct	set_buf;		/* contains GDS info */
static	syn_struct	syn_buf[MAXSYNONYMS];	/* contains synonyms */
static	dsc_struct	dsc_buf[] = {
   { "APLAB"   , NULL      , DSC_SUB },		/* tape label of Antenna Pattern */
   { "APSET"   , NULL      , DSC_SKIP },	/* set of AP */
   { "APSSET"  , NULL      , DSC_SKIP },	/* subset of AP */
   { "APVSN"   , NULL      , DSC_SUB },		/* tape volume of Antenna Pattern */
   { "BANDW"   , NULL      , DSC_CHECK },	/* total bandwidth of observation */
   { "BITPIX"  , NULL      , DSC_SKIP },	/* for encoding only */
   { "BLANK"   , NULL      , DSC_SKIP },	/* blank value */
   { "BLGRAD"  , NULL      , DSC_CHECK },	/* baseline grading function */
   { "BLOCKED" , NULL      , DSC_SKIP },	/* only for tapes */
   { "BMMAJ"   , NULL      , DSC_CHECK },	/* major axis of beam */
   { "BMMIN"   , NULL      , DSC_CHECK },	/* minor axis of beam */
   { "BMPA"    , NULL      , DSC_CHECK },	/* PA major axis beam */
   { "BSCALE"  , NULL      , DSC_SKIP },	/* scaling factor */
   { "BUNIT"   , NULL      , DSC_CHECK },	/* data units   (WU,MJY/SR,...) */
   { "BZERO"   , NULL      , DSC_SKIP },	/* zero level */
   { "CDELT"   , NULL      , DSC_SKIP },	/* primary grid separation */
   { "COMMENT" , NULL      , DSC_COMMENT | DSC_SUB },	/* Comment records */
   { "CROTA"   , NULL      , DSC_SKIP },	/* primary rotation angle */
   { "CRPIX"   , NULL      , DSC_SKIP },	/* primary reference pixel */
   { "CRVAL"   , NULL      , DSC_SKIP },	/* primary reference value */
   { "CTYPE"   , NULL      , DSC_SKIP },	/* primary axis name */
   { "CUNIT"   , NULL      , DSC_SKIP },	/* primary axis units */
   { "DATAMAX" , NULL      , DSC_SUB },		/* maximum in FITS file */
   { "DATAMIN" , NULL      , DSC_SUB },		/* minimum in FITS file */
   { "DATE"    , NULL      , DSC_CHECK },	/* tape writing date */
   { "DATE-OBS", NULL      , DSC_CHECK },	/* date of observation */
   { "DDELT"   , NULL      , DSC_SKIP },	/* secondary grid separation */
   { "DROTA"   , NULL      , DSC_SKIP },	/* secondary rotation angle */
   { "DRPIX"   , NULL      , DSC_SKIP },	/* secondary reference pixel */
   { "DRVAL"   , NULL      , DSC_SKIP },	/* secondary referenc value */
   { "DTYPE"   , NULL      , DSC_SKIP },	/* secondary axis name */
   { "DUNIT"   , NULL      , DSC_SKIP },	/* secondary axis units */
   { "EPOCH"   , NULL      , DSC_SKIP },	/* epoche of observation (years) */
   { "EQUINOX" , NULL      , DSC_SKIP },	/* equinox */
   { "EXTEND"  , NULL      , DSC_SKIP },	/* only for tapes */
   { "FILEID"  , NULL      , DSC_SKIP },	/* identification of file */
   { "FREQ0"   , NULL      , DSC_SKIP },	/* Rest frequency of spectral line (Hz) */
   { "FSCDEC"  , NULL      , DSC_CHECK },	/* DEC fringe stopping center (degrees) */
   { "FSCRA"   , NULL      , DSC_CHECK },	/* RA fringe stopping center  (degrees) */
   { "GRIDTYPE", NULL      , DSC_CHECK },	/* type of grid */
   { "HISTORY" , NULL      , DSC_HISTORY | DSC_SUB },	/* History records */
   { "INSTRUME", NULL      , DSC_SKIP },	/* name of instrument */
   { "LMAX"    , NULL      , DSC_SKIP },	/* ????? */
   { "LMIN"    , NULL      , DSC_SKIP },	/* ????? */
   { "MAPCODE" , NULL      , DSC_SKIP },	/* LINEMAP map code */
   { "MAPLAB"  , NULL      , DSC_SUB },		/* tape label of map archive */
   { "MAPVSN"  , NULL      , DSC_SUB },		/* tape volume of map archive */
   { "MAXBASE" , NULL      , DSC_CHECK },	/* maximum baseline */
   { "MAXINT"  , NULL      , DSC_SKIP },	/* ????? */
   { "MINBASE" , NULL      , DSC_CHECK },	/* minimum baseline */
   { "MININT"  , NULL      , DSC_SKIP },	/* ????? */
   { "MMAX"    , NULL      , DSC_SKIP },	/* ????? */
   { "MMIN"    , NULL      , DSC_SKIP },	/* ????? */
   { "NAXIS"   , NULL      , DSC_SKIP },	/* number of axes */
   { "NBLANK"  , NULL      , DSC_SUB },		/* number of undefined values in FITS file */
   { "NINTF"   , NULL      , DSC_CHECK },	/* number of interferometers used */
   { "NOISE"   , NULL      , DSC_SUB },		/* r.m.s. noise in FITS file */
   { "NORM"    , NULL      , DSC_SUB },		/* normalizing factor in FFT */
   { "NPOL"    , NULL      , DSC_CHECK },	/* number of polarizations used */
   { "NRFREQ"  , NULL      , DSC_CHECK },	/* number of frequency points used */
   { "OBJECT"  , NULL      , DSC_CHECK },	/* name of object */
   { "OBSDEC"  , "PCDEC"   , DSC_CHECK },	/* replace with PCDEC */
   { "OBSERVER", NULL      , DSC_CHECK },	/* name of observer */
   { "OBSRA"   , "PCRA"    , DSC_CHECK },	/* replace with PCRA */
   { "OBSTIME" , NULL      , DSC_CHECK },	/* observation time */
   { "OBSTYPE" , NULL      , DSC_CHECK },	/* type of observation (LINE,CONT) */
   { "ORIGIN"  , NULL      , DSC_CHECK },	/* tape writing institute */
   { "PCDEC"   , NULL      , DSC_CHECK },	/* pointing centre DEC */
   { "PCRA"    , NULL      , DSC_CHECK },	/* pointing centre RA */
   { "REDCODE" , NULL      , DSC_SKIP },	/* LINEMAP reduction code */
   { "RESOL"   , NULL      , DSC_SKIP },	/* spectral resolution */
   { "RESTFREQ", NULL      , DSC_SKIP },	/* is in freq0 */
   { "SIMPLE"  , NULL      , DSC_SKIP },	/* simple must be true */
   { "TAPER"   , NULL      , DSC_CHECK },	/* frequency taper */
   { "TELESCOP", NULL      , DSC_SKIP },	/* name of telescope */
   { "UVBANDW" , NULL      , DSC_SKIP },	/* UV bandwidth */
   { "UVFREQ"  , NULL      , DSC_SKIP },	/* UV frequency */
   { "UVGRID"  , NULL      , DSC_CHECK },	/* convolving function code */
   { "VELCODE" , NULL      , DSC_SKIP },	/* velocity system */
   { "VELREF"  , NULL      , DSC_SKIP },	/* aips velref */
};

static	FILE	*fid = NULL;		/* file id */
static	bool	amode = TRUE;		/* automatic mode */
static	bool	comment = TRUE;		/* save comment */
static	bool	history = TRUE;		/* save history */
static	bool	nodata = FALSE;		/* load image data */
static  fchar   fts_head = { NULL, 0 };	/* dynamic memory for FITS header */
static  fchar	fts_data = { NULL, 0 };	/* dynamic memory for FITS image */
static	fchar	fts_tape = { NULL, 0 };	/* dynamic memory for FITS tape io */
static	fint	currentfile = 0;	/* current file number */
static  fint    files[MAXFILES];        /* file numbers */
static	fint	indata;			/* number of bytes in data buffer */
static	fint	inhead;			/* number of bytes in header buffer */
static  fint    mtid;                   /* id of tape device */
static  fint    nfile;                  /* number of files to load from tape */
static	fint	nsyns = 0;		/* number of synonyms in syn_buf */


static	fint	nint( double x )
{
   /* Pre Apr 2009 def.: return(x > 0.0 ? (fint) ( x + 0.5 ) : (fint) ( x - 0.5 )); */
   return( (fint) floor(x + 0.5) );
}

static	void	fcopy( fchar dest, fchar source)
/*
 * fcopy copies a f character from source to dest, filling dest with
 * trailing blanks if dest longer than source, or truncating dest if
 * dest shorter than source.
 */
{
   int n;					/* counter */

						/* copy loop */
   for (n = 0; n < dest.l && n < source.l; n++) {
      dest.a[n] = source.a[n];			/* copy character */
   }
   while (n < dest.l) dest.a[n++] = ' ';	/* trailing blanks */
}


static int	fcomp( fchar a, fchar b)
/*
 * fcomp compares f characters similar to strcmp.
 */
{
   return( strncmp( a.a, b.a, a.l > b.l ? b.l : a.l ) );
}


static	fchar	finit( char *b, int len )
/*
 * finit returns a f character which points to the character buffer
 * b, which contains len bytes.
 * The buffer b is filled with blanks.
 */
{
   fchar r;					/* return value */
   int   i;					/* counter */

   r.a = b;					/* assign buffer */
   r.l = len;					/* length of buffer */
   for (i = 0; i < r.l; r.a[i++] = ' ');	/* fill with blanks */
   return( r );					/* retrun f character */
}


static	fchar	fmake( fchar f, fint len )
/*
 * fmake dynamically allocates len bytes of memory for the fortran
 * character f. This is only done if len greater than the current size
 * of f.
 */
{
   fchar r;					/* return value */

   if (f.l < len) {				/* enlarge */
      r.a = realloc( f.a, len );		/* allocate len bytes */
      if (r.a == NULL) {
         fint elev = 4;				/* error code (FATAL) */

         error_c( &elev, FMAKE_ERR );		/* error message */
      }
      r.l = len;				/* new length of f */
   } else {
      r.a = f.a; r.l = f.l;			/* use old f character */
   }
   return( r );					/* return to caller */
}


static void fixup( fchar fc )
/*
 * fixup scans the string fc for non_printable characters and leading
 * blanks.
 */
{
   int	k;

   for ( k = 0; k < fc.l; k++ ) {
      if (!isprint( fc.a[k] )) fc.a[k] = ' ';
   }
   for ( k = 0; k < fc.l && fc.a[k] == ' '; k++ );
   if (k) {
      int	m = 0;

      while ( k < fc.l ) fc.a[m++] = fc.a[k++];
      while ( m < fc.l ) fc.a[m++] = ' ';
   }
}


static fchar descr( char *name, fint axnum )
/*
 * descr creates a fits descriptor consisting of name plus the axis
 * number attached. The address of the f character points to a static
 * area which is overwritten each call. descr does NOT check whether
 * the resulting descriptor name will overflow the static buffer area!
 */
{
   static char dscbuf[MAXFTSDSCLEN+1];		/* buffer for descriptor */
   fchar       r;				/* return value */
   fint        l;				/* counter */

   l = sprintf( dscbuf, "%s%d", name, axnum );	/* encode descriptor */
   while (l < MAXFTSDSCLEN) dscbuf[l++] = ' ';	/* blank fill */
   r.a = dscbuf; r.l = l;			/* initialize return value */
   return( r );					/* return to caller */
}


static	fint	matchaxis( fchar ctype )
/*
 * matchaxis compares ctype with the axis names in the existing set.
 * For hidden axis, matchaxis returns -2, for other matching axes
 * the axtype code is returned, If no matching axis, zero is returned.
 */
{
   char  cunitb[MAXFTSNAMLEN];			/* buffer for cunit */
   char  dunitb[MAXFTSNAMLEN];			/* buffer for dunit */
   fchar cunit;					/* points to cunitb */
   fchar dunit;					/* points to dunitb */
   fint  axmatch = 0;				/* reset matching axis number */
   fint  axtype;				/* axis type of FITS axis */
   fint  n;					/* loop counter */
   fint  prosys;				/* projection system */
   fint  skysys;				/* sky system */
   fint  velsys;				/* velocity system */

   cunit = finit( cunitb, MAXFTSNAMLEN );	/* initialize cunit */
   dunit = finit( dunitb, MAXFTSNAMLEN );	/* initialize dunit */
						/* obtain axis type */
   axtype = axtype_c( ctype, cunit, dunit, &skysys, &prosys, &velsys );
						/* search loop */
   for (n = 0; n < set_buf.maxis && !axmatch; n++) {
      if (axtype == set_buf.ax[n].axtype) {	/* matching axis type */
						/* does rest match ? */
         if (skysys == set_buf.ax[n].skysys && prosys == set_buf.ax[n].prosys && velsys == set_buf.ax[n].velsys) {
            axmatch = n + 1;			/* matching axis number */
         }
      }
   }
   return( axmatch );				/* return to caller */
}


static	void	addsynonym( fchar cname, fchar ctype )
/*
 * addsynonym adds a synonym to syn_buf.
 */
{
   fint n;					/* loop counter */

   for (n = 0; n < cname.l && isprint(cname.a[n]); n++);
   if (n != cname.l || cname.a[0] == ' ') return;
						/* search loop */
   for (n = 0; n < nsyns && fcomp( syn_buf[n].cname, cname ); n++);
   if (n == nsyns) {				/* increase storage space */
      if (nsyns < MAXSYNONYMS) {		/* buffer not yet full */
         nsyns += 1;				/* increase synonym counter */
      }
   }
   if (n < nsyns) {				/* can we copy */
      fcopy( syn_buf[n].cname, cname );		/* copy synonym */
      fcopy( syn_buf[n].ctype, ctype );		/* copy ctype */
   }
}


static	void	getsynonym( fchar cname )
/*
 * getsynonym searches the synonym buffer for a synonym for cname.
 * If found, it returns the axis name (in cname).
 */
{
   fint  n;					/* loop counter */

						/* search loop */
   for (n = 0; n < nsyns && fcomp( syn_buf[n].cname, cname ); n++);
   if (n < nsyns) {				/* found */
      fcopy( cname, syn_buf[n].ctype );		/* return synonym */
   }
}


static	int	getdsccode( char *rec )
/*
 * getdsccode returns a code which determines what should be done with it.
 *  0 means unknown descriptor.
 * -1 not a FITS descriptor.
 */
{
   int  r = 0;					/* return value */

   if (*rec == ' ') return( -1 );		/* simple solution */
						/* search loop */
   while ((r < DSCLISTLEN) && strncmp( rec, dsc_buf[r].dsc, strlen( dsc_buf[r].dsc ) ) ) r++;
   if (r < DSCLISTLEN) {			/* descriptor in list */
      if (dsc_buf[r].rep != NULL) {
         int	k = 0;

         while (k < MAXFTSDSCLEN && dsc_buf[r].rep[k]) {
            rec[k] = dsc_buf[r].rep[k]; k++;
         }
         while (k < MAXFTSDSCLEN) rec[k++] = ' ';
      }
      r = dsc_buf[r].code;			/* return code */
   } else if (rec[MAXFTSDSCLEN] == '=') {
      r = 0;					/* record contains descriptor */
   } else {					/* no descriptor */
      r = -1;					/* return code */
   }
   return( r );					/* return to caller */
}


static  void    getintape( void )
/*
 * getintape prompts the user for the name of the input tape device.
 * Next it is checked whether the tape device exists by opening it with
 * mtopen. If it could be opened getintape is satisfied and returns to
 * the caller, otherwise the keyword is cancelled and the user is
 * prompted again for the name of the input tape device.
 */
{
   fint   elev = 4;				/* error level (fatal) */

   mtid = mtopen_c( INTAPE_DEV );		/* open tape device */
   if (mtid < 0) {
      error_c( &elev, INTAPE_ERR1 );		/* unknown tape device */
   }
   (void) mtname_c( &mtid, fts_buf.dev );	/* get name of tape device */
}


static	void	getinfile( void )
/*
 * getinfile prompts the user for the name of the inputfile.
 */
{
   int	first;

   if ( fid == NULL ) {
      first = 1;
   } else {
      first = 0;
      (void ) fclose( fid );
   }
   do {
      fint	n;
      fint	dlev = 1;

      if ( first ) {
         n = usertext_c( fts_buf.dev, &dlev, FITSFILE_KEY, FITSFILE_MES1 );
      } else {
         n = usertext_c( fts_buf.dev, &dlev, FITSFILE_KEY, FITSFILE_MES2 );
      }
      if ( n ) {
         char	name[FILENAME_MAX+1];

         strncpy( name, fts_buf.dev.a, n );
         name[n] = 0;
         fid = fopen( name, "rb" );
         if ( fid == NULL ) {
            reject_c( FITSFILE_KEY, tofchar( "File not found!" ) );
         } else {
            nfile = 1;
            break;
         }
      } else {
         nfile = 0;
         break;
      }
   } while ( 1 );
   cancel_c( FITSFILE_KEY );
}


static	void	getinfiles( void )
/*
 * getinfiles prompts the user for the file numbers on tape. The default
 * action is that rfits quits. Negative file numbers mean previous files,
 * positive numbers next files and zero the current file. The program is
 * setup so that after each load the tape will be positioned after a
 * tapemark, so the next read gives us the first header block.
 * Only nfile (= number of files) and files[] (= array containing file
 * numbers) are exported. The file numbers are sorted in ascending order.
 * The keyword is cancelled each time for the repeat loop in the main program.
 */
{
   fchar  key = INFILES_KEY;			/* keyword */
   fchar  mes = INFILES_MES;			/* message */
   fint   dlev = 1;                             /* default level */
   fint   maxfiles = MAXFILES;			/* maximum number of files */

   if ( fid != NULL ) return;			/* ready already */
   nfile = userint_c( files, &maxfiles, &dlev, key, mes );
   cancel_c( key );			/* cancel keyword */
   if (nfile) {					/* do not quit yet */
      sortia_c( files, &nfile );		/* sort in ascending order */
   }
}


static	void	getoutset( void )
/*
 * getoutset prompts the user for the set name. If the set does exist,
 * the significant descriptor items of the set are read and stored in
 * a structure.
 */
{
   fint   dlev = 0;                             /* default level (no default) */
   fint   gerror = 0;				/* for GDS errors */

   do {
      fchar	key = OUTSET_KEY;
      fchar	mes = OUTSET_MES;

      (void) usertext_c( set_buf.set, &dlev, key, mes );
      cancel_c( key );				/* cancel keyword */
      set_buf.exist = tobool( gds_exist_c( set_buf.set, &gerror ) );
      gerror = 0;
      if ( set_buf.exist ) {
         bool	okay = TRUE;
         fchar	key = OKAY_KEY;
         fchar	mes = OKAY_MES;
         fint	dlev = 1;
         fint	nlog = 1;

         (void) userlog_c( &okay, &nlog, &dlev, key, mes );
         cancel_c( key );
         if ( tobool( okay ) ) break;
      } else break;
   } while (1);
   setfblank_c( &set_buf.blank );		/* set blank value */
   if (set_buf.exist) {				/* set already exists */
      fint level = 0;				/* top level */
      fint n = 0;				/* loop counter */
      fint pmask, smask;			/* bit masks */

      gdsd_rint_c( set_buf.set, tofchar( "NAXIS" ), &level, &set_buf.naxis, &gerror );
      if (gerror) gerror = 0;
      gdsd_rdble_c( set_buf.set, tofchar( "EPOCH" ), &level, &set_buf.epoch, &gerror );
      if (gerror) set_buf.epoch = 0.0; gerror = 0;
      gdsd_rdble_c( set_buf.set, tofchar( "FREQ0" ), &level, &set_buf.freq0, &gerror );
      if (gerror) set_buf.freq0 = 0.0; gerror = 0;
      gdsd_rchar_c( set_buf.set, tofchar( "INSTRUME" ), &level, set_buf.instrume, &gerror );
      if (gerror) gerror = 0;
      do {					/* loop to get axes info */
         char   ctypeb[MAXFTSNAMLEN], cunitb[MAXFTSNAMLEN];
         char   dtypeb[MAXFTSNAMLEN], dunitb[MAXFTSNAMLEN];
         double cdelt, crota, crpix, crval;
         double ddelt, drota, drpix, drval;
         fchar  ctype, cunit;
         fchar  dtype, dunit;
         fint   naxis;
         fint   skysys, prosys, velsys;

         pmask = smask = 0;
         ctype = finit( ctypeb, MAXFTSNAMLEN );
         cunit = finit( cunitb, MAXFTSNAMLEN );
         dtype = finit( dtypeb, MAXFTSNAMLEN );
         dunit = finit( dunitb, MAXFTSNAMLEN );
         gdsd_rint_c( set_buf.set, descr( "NAXIS", n + 1 ), &level, &naxis, &gerror );
         if (gerror) naxis = gerror = 0;
         gdsd_rdble_c( set_buf.set, descr( "CDELT", n + 1 ), &level, &cdelt, &gerror );
         if (gerror) gerror = 0; else pmask += MDELT;
         gdsd_rdble_c( set_buf.set, descr( "CROTA", n + 1 ), &level, &crota, &gerror );
         if (gerror) gerror = 0; else pmask += MROTA;
         gdsd_rdble_c( set_buf.set, descr( "CRPIX", n + 1 ), &level, &crpix, &gerror );
         if (gerror) gerror = 0; else pmask += MRPIX;
         gdsd_rdble_c( set_buf.set, descr( "CRVAL", n + 1 ), &level, &crval, &gerror );
         if (gerror) gerror = 0; else pmask += MRVAL;
         gdsd_rchar_c( set_buf.set, descr( "CTYPE", n + 1 ),  &level, ctype, &gerror );
         if (gerror) gerror = 0; else pmask += MTYPE;
         gdsd_rchar_c( set_buf.set, descr( "CUNIT", n + 1 ),  &level, cunit, &gerror );
         if (gerror) gerror = 0; else pmask += MUNIT;
         gdsd_rdble_c( set_buf.set, descr( "DDELT", n + 1 ), &level, &ddelt, &gerror );
         if (gerror) gerror = 0; else smask += MDELT;
         gdsd_rdble_c( set_buf.set, descr( "DROTA", n + 1 ), &level, &drota, &gerror );
         if (gerror) gerror = 0; else smask += MROTA;
         gdsd_rdble_c( set_buf.set, descr( "DRPIX", n + 1 ), &level, &drpix, &gerror );
         if (gerror) gerror = 0; else smask += MRPIX;
         gdsd_rdble_c( set_buf.set, descr( "DRVAL", n + 1 ), &level, &drval, &gerror );
         if (gerror) gerror = 0; else smask += MRVAL;
         gdsd_rchar_c( set_buf.set, descr( "DTYPE", n + 1 ),  &level, dtype, &gerror );
         if (gerror) gerror = 0; else smask += MTYPE;
         gdsd_rchar_c( set_buf.set, descr( "DUNIT", n + 1 ),  &level, dunit, &gerror );
         if (gerror) gerror = 0; else smask += MUNIT;
         if (smask || pmask) {
            set_buf.ax[n].naxis = naxis;
            set_buf.ax[n].pmask = pmask;
            set_buf.ax[n].cdelt = cdelt;
            set_buf.ax[n].crota = crota;
            set_buf.ax[n].crpix = crpix;
            set_buf.ax[n].crval = crval;
            fcopy( set_buf.ax[n].ctype, ctype );
            fcopy( set_buf.ax[n].cunit, cunit );
            set_buf.ax[n].smask = smask;
            set_buf.ax[n].ddelt = ddelt;
            set_buf.ax[n].drota = drota;
            set_buf.ax[n].drpix = drpix;
            set_buf.ax[n].drval = drval;
            fcopy( set_buf.ax[n].dtype, dtype );
            fcopy( set_buf.ax[n].dunit, dunit );
            set_buf.ax[n].low = 1 - nint( crpix );
            set_buf.ax[n].upp = naxis - nint( crpix );
            set_buf.ax[n].min = set_buf.ax[n].low;
            set_buf.ax[n].max = set_buf.ax[n].upp;
            set_buf.ax[n].axtype = axtype_c( ctype, cunit, dunit, &skysys, &prosys, &velsys );
            set_buf.ax[n].skysys = skysys;	/* sky system code */
            set_buf.ax[n].prosys = prosys;	/* projection system code */
            set_buf.ax[n].velsys = velsys;	/* velocity system code */
            n += 1;				/* increase number of axes */
         }
      } while (smask || pmask);			/* until no information found */
      set_buf.maxis = n;			/* total number of axis */
   }
}


static	void 	position( fint nextfile )
/*
 * position positions the tape at the beginning of the file
 * denoted by nextfile. Counting is done relative to the initial
 * position, which is 0. Negative numbers indicate previous files,
 * positive numbers files further on. The static variable currentfile
 * holds the current file number. This is the only exported variable.
 * It is assumed that the tape is positioned at the first record of a
 * file, that is, just after a tapemark or BOT.
 */
{
   fint elev = 4;				/* error level (FATAL) */

   if ( fid != NULL ) return;			/* quick exit */
   if (nextfile > currentfile) {		/* do a forward skip */
      fint fsf = nextfile - currentfile;	/* number of files to skip */
      fint tst;					/* tape status from mtfsf */

      tst = mtfsf_c( &mtid, &fsf );		/* do the skip */
      if (tst != fsf) {				/* error skipping files */
         error_c( &elev, POSITION_ERR );	/* display error message */
      }
      currentfile = nextfile;			/* save new tape position */
   } else if (nextfile < currentfile) {		/* do a backward skip */
      fint bsf = currentfile - nextfile + 1;	/* number of files to skip */
      fint tst;					/* tape status from mtbsf */

      tst = mtbsf_c( &mtid, &bsf );		/* do the skip */
      if (tst == bsf) {				/* no problem */
         fint fsf = 1;				/* skip one file forward */

         tst = mtfsf_c( &mtid, &fsf );		/* do the forward skip */
         if (tst == 1) currentfile = nextfile;	/* save new tape position */
      } else if (tst == ( bsf - 1 )) {		/* BOT encountered (?) */
         currentfile = nextfile;		/* save new tape position */
      }
      if (tst < 1) {				/* error positioning tape */
         error_c( &elev, POSITION_ERR );	/* error message */
      }
   }
}


static	void	readheader( void )
/*
 * readheader reads the complete image description from tape.
 * It fills the structure fts_tape with the essential axis information.
 */
{
   char  recordb[FITSRECLEN];			/* bufer for FITS record */
   fchar record;				/* points to recordb */
   fint  nrec;					/* FITS record number */
   fint  tst;					/* tape status */

   indata = inhead = 0;				/* reset buffer counters */
   record = finit( recordb, FITSRECLEN );	/* initialize f character */
					   	/* allocate memory */
   fts_tape = fmake( fts_tape, FITSBLOCKLEN * FITSBLOCKFACTOR );
   READ( tst, fts_tape );			/* read first block */
   if (tst <= 0) {				/* End of Information */
      PANIC( READHEADER_ERR1 );			/* error message */
   }
   if (tst > 0 && tst%FITSBLOCKLEN) {		/* wrong block length */
      PANIC( READHEADER_ERR2 );			/* error message */
   }
   if (fts_tape.l < tst ) {			/* buffer not large enough */
      fint bsr = 1;				/* skip back one record */

      fts_tape = fmake( fts_tape, tst );	/* enlarge */
      tst = mtbsr_c( &mtid, &bsr );		/* skip back */
      if (tst == bsr) {				/* skip okay */
         READ( tst, fts_tape );			/* reread this block */
      }
   }
   if (tst < 0) {				/* tape error */
      PANIC( READHEADER_ERR3 );			/* error message */
   }
   do {						/* loop to read complete header */
      fchar buff;				/* points to data in buffer */
      fint  extb;				/* number of bytes for header */
      fint  m, n;				/* loop counters */

      buff.a = fts_tape.a; buff.l = tst;	/* initialize f character */
      nrec = ftsd_find_c( buff, tofchar( "END" ), record );
      if (nrec < 0) {				/* descriptor not found */
         extb = tst;				/* transfer whole buffer */
      } else {					/* descriptor found */
         extb = ( nrec * FITSRECLEN / FITSBLOCKLEN + 1 ) * FITSBLOCKLEN;
      }
						/* extend buffer */
      fts_head = fmake( fts_head, inhead + extb );
      for (m = 0, n = inhead; m < extb; fts_head.a[n++] = buff.a[m++]);
      inhead += extb;				/* new number of bytes in header */
      if (tst != extb) {			/* some data left in buffer */
         indata = tst - extb;			/* number of data bytes left */
         fts_data = fmake( fts_data, indata );	/* enlarge data buffer */
         for (m = extb, n = 0; m < tst; fts_data.a[n++] = buff.a[m++]);
      }
      if (nrec < 0) {				/* read next buffer from tape */
         READ( tst, fts_tape );			/* read next header block */
         if (tst <= 0 || tst%FITSBLOCKLEN || fts_tape.l < tst) {
            PANIC( READHEADER_ERR4 );		/* error message */
         }
      }
   } while (nrec < 0);				/* until header complete */
}


static	void	decodehead( void )
/*
 * decodehead decodes the FITS header read in with readheader. It
 * load items similar to outset into a fts_struct. Later on we try to
 * fit it with a GDS structure.
 */
{
   bool  lval;					/* logical */
   fchar buff;					/* points to FITS header */
   fint  blank;					/* blank value */
   fint  elev;					/* error level */
   fint  n;					/* loop counter */
   fint  nbyt;					/* number of bytes / pixel */
   fint  nrec;					/* FITS record number */

   fts_buf.axused = 0;				/* reset */
   buff.a = fts_head.a; buff.l = inhead;	/* initialize f character */
   nrec = ftsd_rlog_c( buff, tofchar( "SIMPLE" ), &lval );
   if (nrec < 0) {
      elev = 4;					/* fatal error */
      error_c( &elev, DECODEHEAD_ERR1 );	/* error message */
   } else if (!tobool( lval )) {		/* SIMPLE = F */
      elev = 1;					/* warning */
      error_c( &elev, DECODEHEAD_ERR2 );	/* error message */
   }
   nrec = ftsd_rdble_c( buff, tofchar( "BSCALE" ), &fts_buf.bscal );
   if (nrec < 0) fts_buf.bscal = 1.0;		/* default */
   nrec = ftsd_rdble_c( buff, tofchar( "BZERO" ), &fts_buf.bzero );
   if (nrec < 0) fts_buf.bzero = 0.0;		/* default */
   nrec = ftsd_rint_c( buff, tofchar( "BITPIX" ), &fts_buf.bitpix );
   if (nrec < 0) {				/* BITPIX not found */
      elev = 4;					/* fatal error */
      error_c( &elev, DECODEHEAD_ERR3 );	/* error message */
   }
   nbyt = abs(fts_buf.bitpix) / CHAR_BIT;	/* number of bytes / pixel */
   if (nbyt != 1 && nbyt != 2 && nbyt != 4 && fts_buf.bitpix != -64) {	/* cannot do it on this machine */
      elev = 4;					/* fatal error */
      error_c( &elev, DECODEHEAD_ERR4 );	/* error message */
   }
   fts_buf.nbytes = nbyt;			/* save number of bytes */
   nrec = ftsd_rint_c( buff, tofchar( "BLANK" ), &blank );
   if (nrec < 0) blank = INT_MIN;
   {
      fint	input_level = 2;
      fint	one = 1;

      userint_c( &blank, &one, &input_level, tofchar( "BLANK=" ), tofchar( "Give value for FITS BLANK [from FITS header]" ) );
   }
   fts_buf.blank = blank;
   nrec = ftsd_rdble_c( buff, tofchar( "EPOCH" ), &fts_buf.epoch );
   if (nrec < 0) {				/* try EQUINOX */
      nrec = ftsd_rdble_c( buff, tofchar( "EQUINOX" ), &fts_buf.epoch );
   }
   nrec = ftsd_rdble_c( buff, tofchar( "FREQ0" ), &fts_buf.freq0 );
   if (nrec < 0) {
      nrec =ftsd_rdble_c( buff, tofchar( "RESTFREQ" ), &fts_buf.freq0 );
   }
   nrec = ftsd_rchar_c( buff, tofchar( "INSTRUME" ), fts_buf.instrume );
   if (nrec < 0) {
      nrec = ftsd_rchar_c( buff, tofchar( "TELESCOP" ), fts_buf.instrume );
      fixup( fts_buf.instrume );
   }
   nrec = ftsd_rint_c( buff, tofchar( "NAXIS" ), &fts_buf.naxis );
   if (nrec < 0) {
      elev = 4;					/* fatal error */
      error_c( &elev, DECODEHEAD_ERR5 );	/* error message */
   }
   for (n = 0; n < fts_buf.naxis; n++) {	/* loop over all axes */
      fts_buf.ax[n].pmask = 0;			/* reset primary mask */
      fts_buf.ax[n].smask = 0;			/* reset secondary mask */
      nrec = ftsd_rint_c( buff, descr( "NAXIS", n + 1 ), &fts_buf.ax[n].naxis );
      nrec = ftsd_rdble_c( buff, descr( "CDELT", n + 1 ), &fts_buf.ax[n].cdelt );
      if (nrec >= 0) {
         fts_buf.ax[n].pmask += MDELT;		/* cdelt found */
      }
      nrec = ftsd_rdble_c( buff, descr( "CROTA", n + 1 ), &fts_buf.ax[n].crota );
      if (nrec >= 0) {
         fts_buf.ax[n].pmask += MROTA;		/* crota found */
      }
      nrec = ftsd_rdble_c( buff, descr( "CRPIX", n + 1 ), &fts_buf.ax[n].crpix );
      if (nrec >= 0) {
         fts_buf.ax[n].pmask += MRPIX;		/* crpix found */
      } else {
         fts_buf.ax[n].crpix = 0.0;		/* default for old GIPSY */
      }
      nrec = ftsd_rdble_c( buff, descr( "CRVAL", n + 1 ), &fts_buf.ax[n].crval );
      if (nrec >= 0) {
         fts_buf.ax[n].pmask += MRVAL;		/* crval found */
      }
      nrec = ftsd_rchar_c( buff, descr( "CTYPE", n + 1 ), fts_buf.ax[n].ctype );
      if (nrec >= 0) {
         fts_buf.ax[n].pmask += MTYPE;		/* ctype found */
         fixup( fts_buf.ax[n].ctype );		/* fix it */
      }
      nrec = ftsd_rchar_c( buff, descr( "CUNIT", n + 1 ), fts_buf.ax[n].cunit );
      if (nrec >= 0) {
         fts_buf.ax[n].pmask += MUNIT;		/* cunit found */
         fixup( fts_buf.ax[n].cunit );		/* fix it */
      }
      nrec = ftsd_rdble_c( buff, descr( "DDELT", n + 1 ), &fts_buf.ax[n].ddelt );
      if (nrec >= 0) {
         fts_buf.ax[n].smask += MDELT;		/* ddelt found */
      }
      nrec = ftsd_rdble_c( buff, descr( "DROTA", n + 1 ), &fts_buf.ax[n].drota );
      if (nrec >= 0) {
         fts_buf.ax[n].smask += MROTA;		/* drota found */
      }
      nrec = ftsd_rdble_c( buff, descr( "DRPIX", n + 1 ), &fts_buf.ax[n].drpix );
      if (nrec >= 0) {
         fts_buf.ax[n].smask += MRPIX;		/* drpix found */
      }
      nrec = ftsd_rdble_c( buff, descr( "DRVAL", n + 1 ), &fts_buf.ax[n].drval );
      if (nrec >= 0) {
         fts_buf.ax[n].smask += MRVAL;		/* drval found */
      }
      nrec = ftsd_rchar_c( buff, descr( "DTYPE", n + 1 ), fts_buf.ax[n].dtype );
      if (nrec >= 0) {
         fts_buf.ax[n].smask += MTYPE;		/* dtype found */
         fixup( fts_buf.ax[n].dtype );
      }
      nrec = ftsd_rchar_c( buff, descr( "DUNIT", n + 1 ), fts_buf.ax[n].dunit );
      if (nrec >= 0) {
         fts_buf.ax[n].smask += MUNIT;		/* dunit found */
         fixup( fts_buf.ax[n].dunit );
      }
      fts_buf.ax[n].low = 1 - nint( fts_buf.ax[n].crpix );
      fts_buf.ax[n].upp = fts_buf.ax[n].naxis - nint( fts_buf.ax[n].crpix );
   }
}


static	void	gamble1( fchar buff, int version, int n )
/*
 * If the set does not exists, we have to be sure that we
 * get the correct axis names. Therefore we will ask the user
 * for each axis what to do with it.
*/
{
   char  ctypeb[MAXFTSNAMLEN];			/* buffer for CTYPE */
   char  cunitb[MAXFTSNAMLEN];			/* buffer for CUNIT */
   char  dunitb[MAXFTSNAMLEN];			/* buffer for DUNIT */
   char  keyword[MAXTEXTLEN];			/* buffer for keywords */
   char  message[MAXTEXTLEN];			/* buffer for messages */
   char  message1[MAXTEXTLEN];			/* buffer for message (first part) */
   fchar ctype;					/* name of axis */
   fchar cunit;					/* units of axis */
   fchar dunit;					/* secondary units */
   fint  save_synonym = 1;			/* save synonym ? */
   fint  k;					/* loop counter */
   fint  zero = 0;				/* this is zero */
   fint  one = 1;				/* this is one */
   fint  two = 2;				/* this is two */
   fint  axtype;				/* code axis type */
   fint  skysys;				/* sky system of exis */
   fint  prosys;				/* projection system */
   fint  velsys;				/* velocity system */
   fint  lugrid[2];				/* buffer for range */
   fint  okay = 0;				/* controls loop flow */
   fint  oxtype;				/* old axis type */
   fint  nmap = 0;				/* origin == nmap */

   fts_buf.ax[n].matchax = 0;			/* reset */
   ctype = finit( ctypeb, MAXFTSNAMLEN );	/* initialize f character */
   cunit = finit( cunitb, MAXFTSNAMLEN );	/* initialize f character */
   dunit = finit( dunitb, MAXFTSNAMLEN );	/* initialize f character */
   {
      char	originb[MAXFTSCHRLEN+1];
      fchar	origin;
      fint	nrec;

      origin = finit( originb, MAXFTSCHRLEN );
      originb[MAXFTSCHRLEN] = 0;
      nrec = ftsd_rchar_c( buff, tofchar( "ORIGIN" ), origin );
      if (nrec > 0 && strstr( originb, "PGM=NMAP" ) != NULL ) nmap = 1;
   }
   for (k = 0; k < MAXFTSNAMLEN; k++) {		/* clean loop */
      if (!isprint( fts_buf.ax[n].ctype.a[k] )) {
         fts_buf.ax[n].ctype.a[k] = '?';	/* replace by '?' */
         save_synonym = 0;			/* don't save synonym */
      }
   }
   fcopy( ctype, fts_buf.ax[n].ctype );		/* copy f character */
   (void) sprintf( keyword, "CTYPE%d=", n + 1 );/* make keyword */
   (void) sprintf( message1, "Enter synonym for %.*s", (int) nelc_c( ctype ), ctype.a );
   getsynonym( ctype );				/* is there a synonym */
   axtype = axtype_c( ctype, cunit, dunit, &skysys, &prosys, &velsys );
   /*
    * If the axis type is FREQ and there is no velocity system attached,
    * try to find the velocity system from VELCODE or from VELREF.
    */
   if (axtype == 3 && velsys == 0) {
      char	velcodeb[MAXFTSNAMLEN];
      fchar	velcode;
      fint	nrec;

      velcode = finit( velcodeb, sizeof(velcodeb) );
      nrec = ftsd_rchar_c( buff, tofchar( "VELCODE" ), velcode );
      if (nrec > 0 && nelc_c( velcode )) {
         fint	k = 0;
         fint	l = nelc_c( ctype );

         ctype.a[l++] = '-';
         while (l < MAXFTSNAMLEN && velcode.a[k] != ' ') {
            ctype.a[l++] = velcode.a[k++];
         }
         axtype = axtype_c( ctype, cunit, dunit, &skysys, &prosys, &velsys );
      }
      if (velsys == 0) {			/* try velref */
         char	code[6];
         fint	k = 0;
         fint	l = nelc_c( ctype );
         fint	velref;

         nrec = ftsd_rint_c( buff, tofchar( "VELREF" ), &velref );
         if (nrec > 0) {
            code[0] = '-';
            if (velref > 256) {
               velref -= 256;
               velsys = 2;
               code[1] = 'R';
            } else {
               velsys = 1;
               code[1] = 'O';
            }
            switch( velref ) {
               case 1: {
                  strcpy( &code[2], "LSR" );
                  break;
               }
               case 2: {
                  strcpy( &code[2], "HEL" );
                  break;
               }
               case 3: {
                  strcpy( &code[2], "OBS" );
                  break;
               }
               default: {
                  break;
               }
            }
            while (l < MAXFTSNAMLEN && code[k]) {
               ctype.a[l++] = code[k++];
            }
            axtype = axtype_c( ctype, cunit, dunit, &skysys, &prosys, &velsys );
         }
      }
   }
   /*
    * If axis type is FREQ we may have to detect the channel spacing in
    * frequency.
    */
   if (axtype == 3 && !(fts_buf.ax[n].pmask & MDELT)) {
      double	dspec = 0.0;
      fint	nrec;

      nrec = ftsd_rdble_c( buff, tofchar( "DSPEC" ), &dspec );
      if (nrec < 0) {
         double	bandw = 0.0;
         fint	nfreq = 0;

         nrec = ftsd_rdble_c( buff, tofchar( "BANDW" ), &bandw );
         if (nrec >= 0) {
            nrec = ftsd_rint_c( buff, tofchar( "NFREQ" ), &nfreq );
            if (nrec >= 0 && nfreq != 0) {
               dspec = bandw / nfreq;
            }
         }
      }
      if (dspec != 0.0) {
         if (dspec > 0.0) dspec = -dspec;
         fts_buf.ax[n].pmask |= MDELT;
         fts_buf.ax[n].cdelt = dspec;
      }
   }
   /*
    * If we deal with a FREQ axis, get the units here.
    */
   if (axtype == 3 && !(fts_buf.ax[n].pmask & MUNIT)) {
      if (fts_buf.ax[n].pmask & MRVAL) {
         if (fts_buf.ax[n].crval < 1.0e6) {
            fcopy( fts_buf.ax[n].cunit, tofchar( "MHZ" ) );
         } else {
            fcopy( fts_buf.ax[n].cunit, tofchar( "HZ" ) );
         }
         fts_buf.ax[n].pmask |= MUNIT;
      }
   }
   oxtype = axtype;				/* save old axis type */
   axtype = 0;					/* reset */
   /*
    * In the next loop we check whether the user agrees with the
    * axis names. Each axis, even if it is a correct axis name
    * (according to the GIPSY standard of course), can be changed
    * by the user.
    */
   do {						/* loop */
      char  inputb[MAXFTSNAMLEN];		/* buffer for input */
      fchar input;				/* points to inputb */
      fint  dlev;				/* default level */
						/* initialize f character */
      input = finit( inputb, MAXFTSNAMLEN );
      if (oxtype == 0 && fts_buf.ax[n].naxis == 1) {
         dlev = 2;				/* default possible */
         (void) sprintf( message, "%s [SKIP]", message1 );
         fcopy( input, tofchar( "SKIP" ) );
#if	0
      } else if (oxtype != 0 && fts_buf.ax[n].naxis == 1) {
         dlev = 1;				/* default possible */
         (void) sprintf( message, "%s [HIDE]", message1 );
         fcopy( input, tofchar( "HIDE" ) );
      } else if (oxtype != 0 && fts_buf.ax[n].naxis != 1) {
         dlev = 1;				/* default possible */
         (void) sprintf( message, "%s [%.*s]", message1, (int) nelc_c( ctype ), ctype.a );
         fcopy( input, ctype );
#else
      } else if (oxtype != 0) {
         dlev = 2;				/* default possible (hidden) */
         (void) sprintf( message, "%s [%.*s]", message1, (int) nelc_c( ctype ), ctype.a );
         fcopy( input, ctype );
#endif
      } else {
         dlev = 0;				/* no default */
         (void) sprintf( message, "%s", message1 );
      }
      if (!amode && dlev == 2) dlev = 1;
      if (dlev == 0)
      {
         (void) sprintf( message, "Enter name for axis %d:  [X%d]", n+1, n+1 );
         (void) sprintf( inputb, "X%d", n+1 );
         dlev = 1;
      }    
    
      (void) usercharu_c( input, &one, &dlev, tofchar( keyword ), tofchar( message ) );
      if (!fcomp( input, tofchar( "HIDE" ) )) {
         if (oxtype == 0) {			/* this is wrong ! */
            error_c( &one, GAMBLE1_ERR1 );	/* error message */
         } else {
            axtype = -2;			/* hidden axis code */
         }
      } else if (!fcomp( input, tofchar( "SKIP" ) )) {
         if (fts_buf.ax[n].naxis != 1) {
            error_c( &one, GAMBLE1_ERR2 );	/* error message */
         } else {
            axtype = -1;			/* skip axis code */
         }
      } else {
         axtype = axtype_c( input, cunit, dunit, &skysys, &prosys, &velsys );
         if (axtype) {				/* correct axis */
            fcopy( ctype, input );		/* save axis name */
         } else {				/* wrong axis */
            axtype = 0;				/* set error code */
            error_c( &one, GAMBLE1_ERR3 );	/* error message */
         }
      }
      cancel_c( tofchar( keyword ) );		/* cancel keyword */
   } while (!axtype);				/* until axis is known */
   /*
    * When an axis is wanted, we have to be sure that certain
    * essential descriptors are present. Here is where we prompt
    * the user for missing descriptors.
    */
   if (axtype > 0) {
      fint	dlev;				/* default level */

      if (!(fts_buf.ax[n].pmask & MUNIT)) {
         if (axtype == 3 && nmap) {
            fcopy( fts_buf.ax[n].cunit, tofchar( "HZ" ) );
            dlev = 1;
         } else if (axtype == 3 && !version) {
            fcopy( fts_buf.ax[n].cunit, tofchar( "MHZ" ) );
            dlev = 1;
         } else {
            fcopy( fts_buf.ax[n].cunit, cunit );
            if (axtype == 1 || axtype == 2) {
               dlev = 2;
            } else {
               dlev = 1;
            }
         }
      } else {
         dlev = 2;
      }
      (void) sprintf( keyword, "CUNIT%d=", n +  1 );
      (void) sprintf( message, "Enter units of axis [%.*s]", (int) nelc_c( cunit ), cunit.a );
      if (!amode && dlev == 2) dlev = 1;
      (void) usercharu_c( fts_buf.ax[n].cunit, &one, &dlev, tofchar( keyword ), tofchar( message ) );
      cancel_c( tofchar( keyword ) );		/* cancel keyword */
      fts_buf.ax[n].pmask |= MUNIT;
      if (!(fts_buf.ax[n].pmask & MRVAL)) {
         dlev = 0;
      } else {
         dlev = 2;
      }
      (void) sprintf( keyword, "CRVAL%d=", n + 1 );
      (void) sprintf( message, "Enter reference value in %.*s", (int) nelc_c( fts_buf.ax[n].cunit ), fts_buf.ax[n].cunit.a );
      if (!amode && dlev == 2) dlev = 1;
      if (dlev == 0)
      {
         fts_buf.ax[n].crval = 0.0;
         dlev = 1;
         (void) sprintf( message, "Enter reference value in %.*s:   [0.0]", 
                         (int) nelc_c( fts_buf.ax[n].cunit ), 
                         fts_buf.ax[n].cunit.a );
      }
      (void) userdble_c( &fts_buf.ax[n].crval, &one, &dlev,  tofchar( keyword ), tofchar( message ) );
      cancel_c( tofchar( keyword ) );		/* cancel keyword */
      fts_buf.ax[n].pmask |= MRVAL;
      if (!(fts_buf.ax[n].pmask & MDELT)) {
         dlev = 0;
      } else {
         dlev = 2;
      }
      (void) sprintf( keyword, "CDELT%d=", n + 1 );
      (void) sprintf( message, "Enter pixel separation in %.*s", (int) nelc_c( fts_buf.ax[n].cunit ), fts_buf.ax[n].cunit.a );
      if (!amode && dlev == 2) dlev = 1;
      if (dlev == 0)
      {
         fts_buf.ax[n].cdelt = 1.0;
         dlev = 1;
         (void) sprintf( message, "Enter pixel separation in %.*s:   [1.0]", (int) nelc_c( fts_buf.ax[n].cunit ), fts_buf.ax[n].cunit.a );         
      }            
      (void) userdble_c( &fts_buf.ax[n].cdelt, &one, &dlev,  tofchar( keyword ), tofchar( message ) );
      cancel_c( tofchar( keyword ) );		/* cancel keyword */
      fts_buf.ax[n].pmask |= MDELT;		/* KGB */
   }
   /*
    * Now we do some special things for NMAP maps.
    */
   if (axtype == 3 && nmap && velsys) {
      double	freqr;

      if (ftsd_rdble_c( buff, tofchar( "FREQR" ), &freqr ) >= 0) {
         double	g;
         fint	grid;

         g = ( fts_buf.ax[n].crval - freqr ) / fts_buf.ax[n].cdelt;
         grid = nint( g );
         fts_buf.ax[n].low = fts_buf.ax[n].grid = grid;
         fts_buf.ax[n].upp = fts_buf.ax[n].naxis + grid - 1;
         fts_buf.ax[n].crpix = 1.0 - g;
      }
   }
   /*
    * Now we know the axis name which we should use for the
    * new set. Next we determine the range along this axis which
    * is wanted by the user. It is possible to specify a larger range
    * than necessary to allow space for other FITS images.
    * Also we need to know the position of the first grid on
    * the axis.
    */
   if (axtype > 0) {				/* get limits */
      fint diff;				/* shift in crpix */
      fint dlev;				/* default level */
      fint ngrid;				/* new position of first grid */
      fint ogrid;				/* old position of first grid */

      dlev = 6;					/* exact number wanted */
      (void) sprintf( keyword, "LIMITS%d=", n + 1 );
      lugrid[0] = fts_buf.ax[n].low;		/* lower grid */
      lugrid[1] = fts_buf.ax[n].upp;		/* upper grid */
      (void) sprintf( message, "Enter limits along %.*s [%d,%d]", (int) nelc_c( ctype ), ctype.a, lugrid[0], lugrid[1] );
      do {					/* loop */
         if (!amode) dlev = 5;
         if (userint_c( lugrid, &two, &dlev, tofchar( keyword ), tofchar( message ) )) {
            if (lugrid[0] <= lugrid[1]) {	/* input okay */
               fts_buf.ax[n].min = lugrid[0];
               fts_buf.ax[n].max = lugrid[1];
               okay = 1;			/* quit loop */
            } else {				/* error message */
               error_c( &one, GAMBLE1_ERR4 );
               okay = 0;
            }
         } else {
            fts_buf.ax[n].min = lugrid[0];	/* default */
            fts_buf.ax[n].max = lugrid[1];	/* default */
            okay = 1;				/* quit loop */
         }
         cancel_c( tofchar( keyword ) );	/* cancel keyword */
      } while (!okay);				/* quit when everything ok */
      diff = lugrid[0] - fts_buf.ax[n].low;	/* shift in crpix */
      fts_buf.ax[n].crpix -= diff;		/* new crpix */
      do {					/* loop to get first grid */
         (void) sprintf( keyword, "GRID%d=", n + 1 );
         (void) sprintf( message1, "Enter first grid along %.*s", (int) nelc_c( ctype ), ctype.a );
         ogrid = fts_buf.ax[n].low;		/* first grid (default) */
         if (ogrid < fts_buf.ax[n].min) {	/* this is somewhat nicer */
            ogrid = fts_buf.ax[n].min;		/* ?? */
         }
         if (ogrid > fts_buf.ax[n].max || (ogrid + fts_buf.ax[n].naxis - 1) < fts_buf.ax[n].min) {
            dlev = 0;				/* no default */
            (void) sprintf( message, "%s", message1 );
         } else {
            dlev = 2;				/* default */
            (void) sprintf( message, "%s [%d]", message1, ogrid );
         }
         ngrid = ogrid;
         if (!amode && dlev == 2) dlev = 1;
         (void) userint_c( &ngrid, &one, &dlev, tofchar( keyword ), tofchar( message ) );
         if (ngrid > fts_buf.ax[n].max || (ngrid + fts_buf.ax[n].naxis - 1) < fts_buf.ax[n].min) {
            error_c( &one, GAMBLE1_ERR5 );	/* error message */
            okay = 0;				/* not okay */
         } else {
            okay = 1;				/* okay */
         }
         cancel_c( tofchar( keyword ) );	/* cancel keyword */
      } while (!okay);				/* until okay */
      diff = ngrid - ogrid;
      fts_buf.ax[n].grid = ngrid;		/* first grid */
      fts_buf.ax[n].low = ngrid;
      fts_buf.ax[n].upp = fts_buf.ax[n].naxis + ngrid - 1;
      fts_buf.ax[n].crval -= fts_buf.ax[n].cdelt * diff;
   }
   /*
    * Now we have to find the position where to put the
    * data from tape.
    */
   switch( axtype ) {				/* which type of axis */
      case 1: {					/* longitude axis */
         double crval;				/* for reference value */

         if (!version) {			/* old GIPSY */
            if (ftsd_rdble_c( buff, tofchar( "LON0" ), &crval ) >= 0) {
               fts_buf.ax[n].crval = crval;	/* replace by LON0 */
            }
            if (fts_buf.ax[n].cdelt > 0.0) {	/* positive CDELT */
               fts_buf.ax[n].cdelt *= -1.0;	/* make it negative */
            }
            fts_buf.ax[n].crota = 0.0;		/* no rotation for LON axis */
         }
         break;
      }
      case 2: {					/* latitude axis */
         double crota;				/* rotation axis of sky */
         double crval;				/* for reference value */

         if (!version) {			/* old GIPSY */
            if (ftsd_rdble_c( buff, tofchar( "LAT0" ), &crval ) >= 0) {
               fts_buf.ax[n].crval = crval;	/* replace by LAT0 */
            }
            if (ftsd_rdble_c( buff, tofchar( "POSANG" ), &crota ) >= 0) {
               fts_buf.ax[n].crota = crota;	/* rotation angle */
            }
         }
         break;
      }
      case 3: {					/* frequency axis */
         double crval = fts_buf.ax[n].crval;	/* reference frequency */
         double drval = 0.0;			/* reference velocity */
         double fact = 1.0;			/* frequency scaling factor */
         double freq = 0.0;			/* frequency of first grid */
         double freq0 = 0.0;			/* rest frequency */
         double vel;				/* velocity */
         fint   nrec;				/* FITS record number */

         if (nmap && velsys) {
            double	freqr;
            double	velr;

            fts_buf.freq0 = freq0;
            ftsd_rdble_c( buff, tofchar( "FREQR" ), &freqr );
            ftsd_rdble_c( buff, tofchar( "VELR" ), &velr );
            velr /= 1000.0;			/* to km/s */
            fts_buf.ax[n].crval = freqr;	/* change crval */
            fts_buf.ax[n].drval = velr;		/* change drval */
            fcopy( fts_buf.ax[n].dunit, tofchar( "KM/S" ) );
            fcopy( fts_buf.ax[n].dtype, tofchar( "VELO" ) );
            fts_buf.ax[n].smask |= (MRVAL + MTYPE + MUNIT);
            break;
         }
         if (version) {				/* NEW GIPSY */
            freq = crval + fts_buf.ax[n].cdelt * fts_buf.ax[n].grid;
         } else {				/* OLD GIPSY */
            if (crval == 0.0) {			/* not filled in by Marco */
               if (ftsd_rdble_c( buff, tofchar( "UVFREQ" ), &crval ) >= 0) {
                  fts_buf.ax[n].crval = crval;
               }
            }
            if (fts_buf.ax[n].cdelt == 1.0) {	/* not filled in by Marco */
               double	cdelt;

               if (ftsd_rdble_c( buff, tofchar( "DSPEC" ), &cdelt ) >= 0) {
                  fts_buf.ax[n].cdelt = -cdelt;
               }
            }
            freq = crval + fts_buf.ax[n].cdelt * fts_buf.ax[n].grid;
         }
         if (freq < 1.0e6) {			/* probably in MHz */
            fact = 1.0e6;			/* scaling factor */
            freq *= fact;		        /* convert to Hz */
         }
         freq0 = fts_buf.freq0;			/* get rest frequency */
         if (freq0 < 1.0e6) {			/* probably in MHZ */

            fts_buf.freq0 = freq0 *= 1.0e6;	/* now in HZ */
            {
               fint	output_level = 16;

               anyout_c( &output_level, tofchar( "FREQ0 in MHZ!") );
            }
         }
         if (freq0 == 0.0 && velsys) {		/* gamble for rest frequency */
            double df = DBL_MAX;		/* maximum difference */
            double dummy;			/* dummy difference */

            if ((dummy = fabs(freq - 1420405752.0)) < df) {
               freq0 = 1420405752.0;		/* HI line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 1424734000.0)) < df) {
               freq0 = 1424734000.0;		/* H166a line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 1425445000.0)) < df) {
               freq0 = 1425445000.0;		/* CII166a line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 1612231000.0)) < df) {
               freq0 = 1612231000.0;		/* OH line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 1665401800.0)) < df) {
               freq0 = 1665401800.0;		/* OH line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 1667359000.0)) < df) {
               freq0 = 1667359000.0;		/* OH line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 1720530000.0)) < df) {
               freq0 = 1720530000.0;		/* OH line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 4829659600.0)) < df) {
               freq0 = 4829659600.0;		/* H2CO line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 4874157000.0)) < df) {
               freq0 = 4874157000.0;		/* H110a line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 4876144000.0)) < df) {
               freq0 = 4876144000.0;		/* He110a line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 4897779000.0)) < df) {
               freq0 = 4897779000.0;		/* H138b line */
               df = dummy;
            }
            if ((dummy = fabs(freq - 4899774000.0)) < df) {
               freq0 = 4899774000.0;		/* He138b line */
               df = dummy;
            }
            (void) sprintf( message, "Rest frequency in Hz [%f]", freq0 );
            (void) userdble_c( &freq0, &one, &one, tofchar( "FREQ0=" ), tofchar( message ) );
            cancel_c( tofchar( "FREQ0=" ) );	/* cancel keyword */
            fts_buf.freq0 = freq0;		/* save rest frequency */
         }
         if (velsys && !version) {		/* old GIPSY */
            freq /= fact;			/* rescale */
            nrec = ftsd_rdble_c( buff, tofchar( "VEL" ), &vel );
            if (nrec < 0) {			/* get it from user */
               fint	velref;

               nrec = ftsd_rint_c( buff, tofchar( "VELREF" ), &velref );
               if (nrec < 0) {
                  (void) sprintf( keyword, "VEL=" );
                  (void) sprintf( message, "Velocity at grid (in km/s)" );
                  (void) userdble_c( &vel, &one, &zero, tofchar( "VEL=" ), tofchar( message ) );
                  cancel_c( tofchar( keyword ) );
               } else {
                  double	fof;

                  nrec = ftsd_rdble_c( buff, tofchar( "ALTRVAL" ), &vel );
                  vel /= 1000.0;
                  nrec = ftsd_rdble_c( buff, tofchar( "ALTRPIX" ), &fof );
                  freq = crval + ( fof - fts_buf.ax[n].crpix ) * fts_buf.ax[n].cdelt;
               }
            }
            if (freq < 1.0e6) freq *= 1.0e6;	/* to Hz */
            if (crval < 1.0e6) crval *= 1.0e6;	/* to Hz */
            if (velsys == 1) {			/* optical definition */
               drval = vel + 299792.480 * freq0 * ( freq - crval ) / freq / crval;
            } else if (velsys == 2) {		/* radio definition */
               drval = vel + 299792.480 / freq0 * ( freq - crval );
            }
            fts_buf.ax[n].drval = drval;	/* change drval */
            fcopy( fts_buf.ax[n].dunit, tofchar( "KM/S" ) );
            fcopy( fts_buf.ax[n].dtype, tofchar( "VELO" ) );
            fts_buf.ax[n].smask |= (MRVAL + MTYPE + MUNIT);
         }
         break;
      }
      case 4:			  		/* velocity axis */
      case 5:					/* wavelength */
      case 6:					/* inverse wavelength */
      case 7:					/* log(wavelength) */
      case 8:					/* time */
      case 9:					/* polarisation */
      case 10: {				/* parameter */
         break;
      }
      default: {
         break;
      }
   }
   fts_buf.ax[n].axtype = axtype;		/* save axis type code */
   fts_buf.ax[n].skysys = skysys;		/* save sky system */
   fts_buf.ax[n].prosys = prosys;		/* save projection system */
   fts_buf.ax[n].velsys = velsys;		/* save velocity system */
   if (save_synonym) {				/* should we save synonym */
      addsynonym( fts_buf.ax[n].ctype, ctype );	/* add synonym */
   }
   if (axtype != -1) {				/* save new axis name */
      fcopy( fts_buf.ax[n].ctype, ctype );
   }
}


static	void	gamble2( fchar buff, int version, int n )
/*
 * If the set exists, we have to compare each axis on the FITS tape
 * with the axis names of the set.
 * setdim counts the number of axes used from the FITS structure.
 */
{
   char  ctypeb[MAXFTSNAMLEN];			/* buffer for CTYPE */
   char  cunitb[MAXFTSNAMLEN];			/* buffer for CUNIT */
   char  dunitb[MAXFTSNAMLEN];			/* buffer for DUNIT */
   char  keyword[MAXTEXTLEN];			/* buffer for keywords */
   char  message[MAXTEXTLEN];			/* buffer for messages */
   fchar ctype;					/* name of axis */
   fchar cunit;					/* units of axis */
   fchar dunit;					/* secondary units */
   fint  save_synonym = 1;			/* save synonym */
   fint  k;					/* loop counter */
   fint  axmatch = 0;				/* matching axis number */
   fint  zero = 0;				/* this is zero */
   fint  one = 1;				/* this is one */
   fint  four = 4;				/* this is four */
   fint  axtype = 0;				/* code axis type */

   fts_buf.ax[n].matchax = 0;			/* reset */
   ctype = finit( ctypeb, MAXFTSNAMLEN );	/* initialize f character */
   cunit = finit( cunitb, MAXFTSNAMLEN );	/* initialize f character */
   dunit = finit( dunitb, MAXFTSNAMLEN );	/* initialize f character */
   for (k = 0; k < MAXFTSNAMLEN; k++) {		/* clean loop */
      if (!isprint( fts_buf.ax[n].ctype.a[k] )) {
         fts_buf.ax[n].ctype.a[k] = '?';	/* replace by '?' */
         save_synonym = 0;			/* don't save synonym */
      }
   }
   fcopy( ctype, fts_buf.ax[n].ctype );		/* copy f character */
   if (fts_buf.axused == set_buf.naxis) {	/* enough axes defined */
      axtype = -1;				/* skip this axis */
   } else {
      getsynonym( ctype );			/* get synonym */
      axmatch = matchaxis( ctype );		/* get matching axis number */
      if (!axmatch) {				/* prompt user */
         (void) sprintf( keyword, "CTYPE%d=", n + 1 );
         (void) sprintf( message, "Axis %.*s becomes ?", (int) nelc_c( ctype ), ctype.a );
         do {
            char  inputb[MAXFTSNAMLEN];		/* buffer for user input */
            fchar input;			/* points to inputb */

						/* initialize f character */
            input = finit( inputb, MAXFTSNAMLEN );
						/* get it from the user */
            (void) usercharu_c( input, &one, &zero, tofchar( keyword ), tofchar( message ) );
            cancel_c( tofchar( keyword ) );	/* cancel keyword */
            if (!fcomp( input, tofchar("SKIP") )) {
               axtype = -1;			/* skip this axis */
               break;				/* leave loop */
            }
            axmatch = matchaxis( input );	/* does axis match ? */
            if (!axmatch) {			/* no! */
               error_c( &one, GAMBLE2_ERR1 );	/* error message */
            }
         } while (!axmatch);			/* until a match found */
      }
      if (axmatch) {				/* use this axis */
         if (axmatch > set_buf.naxis) {		/* we have plenty already */
            axtype = -2;			/* hidden axis */
         } else {				/* we still have room for it */
            axtype = set_buf.ax[axmatch-1].axtype;
            fts_buf.ax[n].matchax = axmatch;
         }
      }
   }
   if (axtype > 0) fts_buf.axused += 1;		/* update number of axes used */
   if (axtype > 0) {				/* set matching axis in set_buf */
      set_buf.ax[axmatch-1].matchax = n + 1;
   }
   /*
    * Now we know what to do with this axis.
    * In case of an axis which should be skipped, we have to check
    * whether the size of the axis is one. If not, we prompt the user
    * for the grid position to use.
    */
   if (axtype < 0 && fts_buf.ax[n].naxis > 1) {
      fint grid;

      (void) sprintf( keyword, "GRID%d=", n +  1  );
      (void) sprintf( message, "Enter grid along %.*s", (int) nelc_c( ctype ), ctype.a );
      do {
         (void) userint_c( &grid, &one, &zero, tofchar( keyword ), tofchar( message ) );
         cancel_c( tofchar(keyword ) );		/* cancel keyword */
         if (grid < fts_buf.ax[n].low || grid > fts_buf.ax[n].upp) {
            error_c( &one, GAMBLE2_ERR2 );	/* error message */
         } else {
            fts_buf.ax[n].grid = grid;
            break;				/* leave loop */
         }
      } while (1);				/* infinite loop */
   }
   switch( axtype ) {				/* which type of axis */
      case 1:					/* longitude axis */
      case 2: {					/* latitude axis */
         if (fts_buf.ax[n].low < set_buf.ax[axmatch-1].low) {
            set_buf.ax[axmatch-1].min = set_buf.ax[axmatch-1].low;
         } else {
            set_buf.ax[axmatch-1].min = fts_buf.ax[n].low;
         }
         if (fts_buf.ax[n].upp > set_buf.ax[axmatch-1].upp) {
            set_buf.ax[axmatch-1].max = set_buf.ax[axmatch-1].upp;
         } else {
            set_buf.ax[axmatch-1].max = fts_buf.ax[n].upp;
         }
         break;
      }
      case 3: {					/* velocity axis */
         char	originb[MAXFTSCHRLEN+1];
         fchar	origin;
         fint	nrec;

         origin = finit( originb, MAXFTSCHRLEN );
         originb[MAXFTSCHRLEN] = 0;
         nrec = ftsd_rchar_c( buff, tofchar( "ORIGIN" ), origin );
         if (nrec > 0 && strstr( originb, "PGM=NMAP" ) != NULL ) {
            double	vel;
            double	grid;

            nrec = ftsd_rdble_c( buff, tofchar( "VEL" ), &vel );
            do {
               double crval, drval, cdelt, f;

               if (nrec < 0) {			/* get it from the user */
                  (void) sprintf( keyword, "VEL=" );
                  (void) sprintf( message, "Velocity at grid (in km/sec)" );
                  (void) userdble_c( &vel, &one, &zero, tofchar( keyword ), tofchar( message ) );
                  cancel_c( tofchar( keyword ) );
                  vel *= 1000.0;		/* to m/s */
               }
               (void) factor_c( set_buf.ax[axmatch-1].cunit, tofchar( "HZ" ), &f );
               crval = set_buf.ax[axmatch-1].crval * f;
               cdelt = set_buf.ax[axmatch-1].cdelt * f;
               (void) factor_c( set_buf.ax[axmatch-1].dunit, tofchar( "M/S" ), &f );
               drval = set_buf.ax[axmatch-1].drval * f;
               nrec = velpro_c( &vel, &grid, &crval, &cdelt, &drval, &set_buf.freq0, &set_buf.ax[axmatch-1].velsys, &zero );
               nrec *= -1;
            } while (nrec < 0);
            fts_buf.ax[n].grid = nint(grid);
            fts_buf.ax[n].low = fts_buf.ax[n].grid;
            fts_buf.ax[n].upp = fts_buf.ax[n].naxis + fts_buf.ax[n].low - 1;
            set_buf.ax[axmatch-1].min = fts_buf.ax[n].grid;
            set_buf.ax[axmatch-1].max = fts_buf.ax[n].grid;
         } else if (set_buf.ax[axmatch-1].velsys && !version) {
            double vel;
            double grid;

            nrec = ftsd_rdble_c( buff, tofchar( "VEL" ), &vel );
            do {
               double crval, drval, cdelt, f;

               if (nrec < 0) {			/* get it from the user */
                  (void) sprintf( keyword, "VEL=" );
                  (void) sprintf( message, "Velocity at grid (in km/sec)" );
                  (void) userdble_c( &vel, &one, &zero, tofchar( keyword ), tofchar( message ) );
                  cancel_c( tofchar( keyword ) );
               }
               vel *= 1000.0;			/* to m/s */
               (void) factor_c( set_buf.ax[axmatch-1].cunit, tofchar( "HZ" ), &f );
               crval = set_buf.ax[axmatch-1].crval * f;
               cdelt = set_buf.ax[axmatch-1].cdelt * f;
               (void) factor_c( set_buf.ax[axmatch-1].dunit, tofchar( "M/S" ), &f );
               drval = set_buf.ax[axmatch-1].drval * f;
               nrec = velpro_c( &vel, &grid, &crval, &cdelt, &drval, &set_buf.freq0, &set_buf.ax[axmatch-1].velsys, &zero );
               nrec *= -1;
            } while (nrec < 0);
            fts_buf.ax[n].grid = (fint) (grid+0.5);
            fts_buf.ax[n].low = fts_buf.ax[n].grid;
            fts_buf.ax[n].upp = fts_buf.ax[n].naxis + fts_buf.ax[n].low - 1;
            set_buf.ax[axmatch-1].min = fts_buf.ax[n].grid;
            set_buf.ax[axmatch-1].max = fts_buf.ax[n].grid;
         } else if (!set_buf.ax[axmatch-1].velsys) {
            fts_buf.ax[n].grid = ( fts_buf.ax[n].crval - set_buf.ax[axmatch-1].crval ) / set_buf.ax[axmatch-1].cdelt + 0.5;
            fts_buf.ax[n].low  = fts_buf.ax[n].grid;
            fts_buf.ax[n].upp = fts_buf.ax[n].naxis + fts_buf.ax[n].low - 1;
            set_buf.ax[axmatch-1].min = fts_buf.ax[n].grid;
            set_buf.ax[axmatch-1].max = fts_buf.ax[n].grid;
         } else if (version) {
            double      grid;

            grid = 1.0 - fts_buf.ax[n].crpix;
            fts_buf.ax[n].grid = (fint) (grid+0.5);
            fts_buf.ax[n].low = fts_buf.ax[n].grid;
            fts_buf.ax[n].upp = fts_buf.ax[n].naxis + fts_buf.ax[n].low - 1;
            set_buf.ax[axmatch-1].min = fts_buf.ax[n].low;
            set_buf.ax[axmatch-1].max = fts_buf.ax[n].upp;
         }
         break;
      }
      case 4:					/* velocity axis */
      case 5:					/* wavelength */
      case 6:					/* inverse wavelength */
      case 7:					/* log(wavelength) */
      case 8:					/* time */
      case 9:					/* polarisation */
      case 10: {				/* parameter */
         if (fts_buf.ax[n].low < set_buf.ax[axmatch-1].low) {
            set_buf.ax[axmatch-1].min = set_buf.ax[axmatch-1].low;
         } else {
            set_buf.ax[axmatch-1].min = fts_buf.ax[n].low;
         }
         if (fts_buf.ax[n].upp > set_buf.ax[axmatch-1].upp) {
            set_buf.ax[axmatch-1].max = set_buf.ax[axmatch-1].upp;
         } else {
            set_buf.ax[axmatch-1].max = fts_buf.ax[n].upp;
         }
         break;
      }
      default: {
         break;
      }
   }
   /*
    * Here we check whether we have to extend the last axis of the set.
    */
   if (axtype > 0 && axmatch == set_buf.naxis) {
      if (set_buf.ax[axmatch-1].max > set_buf.ax[axmatch-1].upp) {
         fint gerror = 0;			/* GDS error return */
         fint naxis;				/* new size of axis */

         naxis = set_buf.ax[axmatch-1].naxis - set_buf.ax[axmatch-1].upp + set_buf.ax[axmatch-1].max;
         gds_extend_c( set_buf.set, set_buf.ax[axmatch-1].ctype, &set_buf.ax[axmatch-1].crpix, &naxis, &gerror );
         if (gerror < 0) {
            error_c( &four, GAMBLE2_ERR3 );	/* error message */
         }
         set_buf.ax[axmatch-1].naxis = naxis;	/* new size */
         set_buf.ax[axmatch-1].upp = set_buf.ax[axmatch-1].max;
      }
   }
   if (axtype > 0 && axmatch && save_synonym) {	/* add synonym */
      addsynonym( fts_buf.ax[n].ctype, set_buf.ax[axmatch-1].ctype );
   }
}


static	void	create_set( void )
/*
 * Here we create the set.
 */
{
   fint gerror = 0;				/* GDS error codes */
   fint level = 0;				/* top level of set */
   fint max = 0;				/* total number of axes */
   fint n;					/* loop counter */
   fint nax = 0;				/* real number of axes */

   set_buf.naxis = 0;				/* reset number of axis */
   set_buf.exist = 1;				/* set to true */
   gds_create_c( set_buf.set, &gerror );	/* create set */
   set_buf.freq0 = fts_buf.freq0;		/* save rest frequency */
   if (fts_buf.freq0 != 0.0) {			/* save rest frequency */
      gdsd_wdble_c( set_buf.set, tofchar( "FREQ0" ), &level, &fts_buf.freq0, &gerror );
   }
   set_buf.epoch = fts_buf.epoch;		/* save epoch */
   if (fts_buf.epoch != 0.0) {			/* save epoch */
      gdsd_wdble_c( set_buf.set, tofchar( "EPOCH" ), &level, &fts_buf.epoch, &gerror );
   }
   fcopy( set_buf.instrume, fts_buf.instrume );
   if (fts_buf.instrume.a[0] != ' ') {		/* save instrument name */
      gdsd_wchar_c( set_buf.set, tofchar( "INSTRUME" ), &level, fts_buf.instrume, &gerror );
   }
   /*
    * First loop to add axes and extend the set.
    */
   for (n = 0; n < fts_buf.naxis; n++) {	/* first loop */
      if (fts_buf.ax[n].axtype > 0) {		/* real axis */
         fts_buf.ax[n].matchax = nax + 1;	/* matching axis */
         set_buf.ax[nax].matchax = n + 1;	/* matching axis number */
         set_buf.ax[nax].naxis = fts_buf.ax[n].max - fts_buf.ax[n].min + 1;
         set_buf.ax[nax].low = fts_buf.ax[n].min;
         set_buf.ax[nax].upp = fts_buf.ax[n].max;
         set_buf.ax[nax].axtype = fts_buf.ax[n].axtype;
         set_buf.ax[nax].skysys = fts_buf.ax[n].skysys;
         set_buf.ax[nax].prosys = fts_buf.ax[n].prosys;
         set_buf.ax[nax].velsys = fts_buf.ax[n].velsys;
         if (fts_buf.ax[n].grid < fts_buf.ax[n].min) {
            set_buf.ax[nax].min = fts_buf.ax[n].min;
         } else {
            set_buf.ax[nax].min = fts_buf.ax[n].grid;
         }
         if ((fts_buf.ax[n].grid + fts_buf.ax[n].naxis - 1) > fts_buf.ax[n].max) {
            set_buf.ax[nax].max = fts_buf.ax[n].max;
         } else {
            set_buf.ax[nax].max = fts_buf.ax[n].grid + fts_buf.ax[n].naxis - 1;
         }
         set_buf.ax[nax].pmask = fts_buf.ax[n].pmask;
         set_buf.ax[nax].crpix = fts_buf.ax[n].crpix;
         fcopy( set_buf.ax[nax].ctype, fts_buf.ax[n].ctype );
         gds_extend_c( set_buf.set, set_buf.ax[nax].ctype, &set_buf.ax[nax].crpix, &set_buf.ax[nax].naxis, &gerror );
         set_buf.ax[nax].crval = fts_buf.ax[n].crval;
         if (set_buf.ax[nax].pmask & MRVAL) {
            gdsd_wdble_c( set_buf.set, descr( "CRVAL", nax + 1 ), &level, &set_buf.ax[nax].crval, &gerror );
         }
         set_buf.ax[nax].cdelt = fts_buf.ax[n].cdelt;
         if (set_buf.ax[nax].pmask & MDELT) {
            gdsd_wdble_c( set_buf.set, descr( "CDELT", nax + 1 ), &level, &set_buf.ax[nax].cdelt, &gerror );
         }
         set_buf.ax[nax].crota = fts_buf.ax[n].crota;
         if (set_buf.ax[nax].pmask & MROTA) {
            gdsd_wdble_c( set_buf.set, descr( "CROTA", nax + 1 ), &level, &set_buf.ax[nax].crota, &gerror );
         }
         fcopy( set_buf.ax[nax].cunit, fts_buf.ax[n].cunit );
         if (set_buf.ax[nax].pmask & MUNIT) {
            gdsd_wchar_c( set_buf.set, descr( "CUNIT", nax + 1 ), &level, set_buf.ax[nax].cunit, &gerror );
         }
         set_buf.ax[nax].smask = fts_buf.ax[n].smask;
         set_buf.ax[nax].ddelt = fts_buf.ax[n].ddelt;
         if (set_buf.ax[nax].smask & MDELT) {
            gdsd_wdble_c( set_buf.set, descr( "DDELT", nax + 1 ), &level, &set_buf.ax[nax].ddelt, &gerror );
         }
         set_buf.ax[nax].drota = fts_buf.ax[n].drota;
         if (set_buf.ax[nax].smask & MROTA) {
            gdsd_wdble_c( set_buf.set, descr( "DROTA", nax + 1 ), &level, &set_buf.ax[nax].drota, &gerror );
         }
         set_buf.ax[nax].drpix = fts_buf.ax[n].drpix;
         if (set_buf.ax[nax].smask & MRPIX) {
            gdsd_wdble_c( set_buf.set, descr( "DRPIX", nax + 1 ), &level, &set_buf.ax[nax].drpix, &gerror );
         }
         set_buf.ax[nax].drval = fts_buf.ax[n].drval;
         if (set_buf.ax[nax].smask & MRVAL) {
            gdsd_wdble_c( set_buf.set, descr( "DRVAL", nax + 1 ), &level, &set_buf.ax[nax].drval, &gerror );
         }
         fcopy( set_buf.ax[nax].dtype, fts_buf.ax[n].dtype );
         if (set_buf.ax[nax].smask & MTYPE) {
            gdsd_wchar_c( set_buf.set, descr( "DTYPE", nax + 1 ), &level, set_buf.ax[nax].dtype, &gerror );
         }
         fcopy( set_buf.ax[nax].dunit, fts_buf.ax[n].dunit );
         if (set_buf.ax[nax].smask & MUNIT) {
            gdsd_wchar_c( set_buf.set, descr( "DUNIT", nax + 1 ), &level, set_buf.ax[nax].dunit, &gerror );
         }
         set_buf.naxis = ++nax;				/* increase axis count */
      } else {
         fts_buf.ax[n].matchax = 0;			/* no matching axis */
      }
   }
   /*
    * second loop to add hidden axes
    */
   set_buf.maxis = set_buf.naxis;
   for (max = nax, n = 0; n < fts_buf.naxis; n++) {	/* second loop */
      if (fts_buf.ax[n].axtype == -2) {
         set_buf.ax[nax].matchax = n + 1;		/* matching axis number */
         set_buf.ax[max].pmask = fts_buf.ax[n].pmask;
         set_buf.ax[max].cdelt = fts_buf.ax[n].cdelt;
         if (set_buf.ax[max].pmask & MDELT) {
            gdsd_wdble_c( set_buf.set, descr( "CDELT", max + 1 ), &level, &set_buf.ax[max].cdelt, &gerror );
         }
         set_buf.ax[max].crota = fts_buf.ax[n].crota;
         if (set_buf.ax[max].pmask & MROTA) {
            gdsd_wdble_c( set_buf.set, descr( "CROTA", max + 1 ), &level, &set_buf.ax[max].crota, &gerror );
         }
         set_buf.ax[max].crpix = fts_buf.ax[n].crpix;
         if (set_buf.ax[max].pmask & MRPIX) {
            gdsd_wdble_c( set_buf.set, descr( "CRPIX", max + 1 ), &level, &set_buf.ax[max].crpix, &gerror );
         }
         set_buf.ax[max].crval = fts_buf.ax[n].crval;
         if (set_buf.ax[max].pmask & MRVAL) {
            gdsd_wdble_c( set_buf.set, descr( "CRVAL", max + 1 ), &level, &set_buf.ax[max].crval, &gerror );
         }
         fcopy( set_buf.ax[max].ctype, fts_buf.ax[n].ctype );
         if (set_buf.ax[max].pmask & MTYPE) {
            gdsd_wchar_c( set_buf.set, descr( "CTYPE", max + 1 ), &level, set_buf.ax[max].ctype, &gerror );
         }
         fcopy( set_buf.ax[max].cunit, fts_buf.ax[n].cunit );
         if (set_buf.ax[max].pmask & MUNIT) {
            gdsd_wchar_c( set_buf.set, descr( "CUNIT", max + 1 ), &level, set_buf.ax[max].cunit, &gerror );
         }
         set_buf.ax[max].smask = fts_buf.ax[n].smask;
         set_buf.ax[max].ddelt = fts_buf.ax[n].ddelt;
         if (set_buf.ax[max].smask & MDELT) {
            gdsd_wdble_c( set_buf.set, descr( "DDELT", max + 1 ), &level, &set_buf.ax[max].ddelt, &gerror );
         }
         set_buf.ax[max].drota = fts_buf.ax[n].drota;
         if (set_buf.ax[max].smask & MROTA) {
            gdsd_wdble_c( set_buf.set, descr( "DROTA", max + 1 ), &level, &set_buf.ax[max].drota, &gerror );
         }
         set_buf.ax[max].drpix = fts_buf.ax[n].drpix;
         if (set_buf.ax[max].smask & MRPIX) {
            gdsd_wdble_c( set_buf.set, descr( "DRPIX", max + 1 ), &level, &set_buf.ax[max].drpix, &gerror );
         }
         set_buf.ax[max].drval = fts_buf.ax[n].drval;
         if (set_buf.ax[max].smask & MRVAL) {
            gdsd_wdble_c( set_buf.set, descr( "DRVAL", max + 1 ), &level, &set_buf.ax[max].drval, &gerror );
         }
         fcopy( set_buf.ax[max].dtype, fts_buf.ax[n].dtype );
         if (set_buf.ax[max].smask & MTYPE) {
            gdsd_wchar_c( set_buf.set, descr( "DTYPE", max + 1 ), &level, set_buf.ax[max].dtype, &gerror );
         }
         fcopy( set_buf.ax[max].dunit, fts_buf.ax[n].dunit );
         if (set_buf.ax[max].smask & MUNIT) {
            gdsd_wchar_c( set_buf.set, descr( "DUNIT", max + 1 ), &level, set_buf.ax[max].dunit, &gerror );
         }
         set_buf.maxis = ++max;				/* increase axis count */
      }
   }
}


static	void	gamble( void )
/*
 * gamble tries to decide from the FITS header and the set header (if
 * it exists) where to put the data from tape. If gamble cannot decide
 * where to put it, it will prompt the user for help. This is the most
 * tricky part of the whole operation since it is not always obvious
 * where to put the data. If the output set already exists, defaults
 * are taken (if possible) from this existing set.
 */
{
   fchar buff;					/* pointer to FITS header */
   fint  n;					/* loop counter */
   fint  version = 0;				/* version of wfits */

   buff.a = fts_head.a; buff.l = inhead;	/* initialize f character */
   for (n = 0; n < fts_buf.naxis; n++) {	/* loop to get version */
      if ((fts_buf.ax[n].pmask & MUNIT) || fts_buf.ax[n].smask) version = 1;
   }
   if (set_buf.exist) {				/* set exists, reset */
      for (n = 0; n < set_buf.maxis; n++) {	/* loop over all axis */
         set_buf.ax[n].matchax = 0;		/* reset */
      }
   }
   for (n = 0; n < fts_buf.naxis; n++) {	/* loop over axis */
      if (!set_buf.exist) {
         gamble1( buff, version, n );
      } else {
         gamble2( buff, version, n );
      }
   }
   /*
    * A possibility to extend the dimensions of the new set.
    */
   if (!set_buf.exist) {
      char	keyword[MAXFTSNAMLEN+1];
      fint	one = 1, two = 2;

      sprintf( keyword, "CTYPE%d=", n + 1 );
      while (usercharu_c( fts_buf.ax[n].ctype, &one, &two, tofchar( keyword ), tofchar( "Extra axis?" ) ) && n < MAXAXES) {
         fts_buf.naxis += 1;
         fts_buf.ax[n].naxis = 1;
         fts_buf.ax[n].pmask = 0;
         fts_buf.ax[n].smask = 0;
         fts_buf.ax[n].crpix = 1.0;
         gamble1( buff, version, n++ );
      }
   }
   /*
    * Here we create the set if it does not yet exist.
    */
   if (!set_buf.exist) create_set( );		/* create the set */
}


static	void	copydata( void )
/*
 * copydata gets the data off the tape, transforms the data into int
 * integers, scales the data and writes the data to disk. At this time
 * the set exists and is adapted to fit the new data in.
 */
{
   char   message[MAXTEXTLEN];			/* buffer for message */
   fint   bmax[MAXAXES];			/* upper box coordinate */
   fint   bmin[MAXAXES];			/* lower box coordinate */
   fint   bsr = 1;				/* back skip a record */
   fint   cwlo = 0;				/* lower coordinate word */
   fint   cwhi = 0;				/* upper coordinate word */
   fint   elev = 4;				/* error level (FATAL) */
   fint   smax[MAXAXES];			/* upper frame coordinate */
   fint   smin[MAXAXES];			/* lower frame coordinate */
   fint   gerror = 0;				/* GDS error code */
   fint   n;					/* loop counter */
   fint   nc = 0;				/* out count */
   fint   nd;					/* number of pixels written */
   fint   nr;					/* in buffer counter */
   fint   nt = 0;				/* number of pixels done */
   fint   ntotal = 1;				/* total number of pixels */
   fint   ndim;					/* dimension of FITS data */
   fint   ndata = 0;				/* size of float and buffer */
   fint   nddata = 0;				/* size of double buffer */
   fint   tid = 0;				/* transfer id */
   fint   tst;					/* tape status */

   /*
    * We create arrays which hold the grid positions of the total
    * frame of the FITS image and the grid positions of the subframe,
    * i.e. that part of the FITS image we want to load into the set.
    */
   ndim = fts_buf.naxis;			/* FITS dimensions */
   /*
    * Now we calculate the frames and the coordinate words.
    */
   for (n = 0; n < ndim; n++) {			/* loop to get coordinates */
      fint matchax = fts_buf.ax[n].matchax;	/* matching sequence number */

      bmin[n] = fts_buf.ax[n].low;		/* lower frame coordinate */
      bmax[n] = fts_buf.ax[n].upp;		/* upper frame coordinate */
      if (matchax) {
         smin[n] = set_buf.ax[matchax-1].min;	/* lower subframe coordinate */
         smax[n] = set_buf.ax[matchax-1].max;	/* upper subframe coordinate */
      } else {
         smin[n] = bmin[n];			/* lower subframe coordinate */
         smax[n] = bmax[n];			/* upper subframe coordinate */
      }
      if (matchax) {				/* add to coordinate word */
         cwlo = gdsc_word_c( set_buf.set, &matchax, &smin[n], &cwlo, &gerror );
         cwhi = gdsc_word_c( set_buf.set, &matchax, &smax[n], &cwhi, &gerror );
      }
      ntotal *= ( bmax[n] - bmin[n] + 1 );	/* total number of pixels */
   }
   for (n = 0; n < set_buf.naxis; n++) {	/* loop over all axis */
      if (!set_buf.ax[n].matchax) {		/* not matched */
         fint axnum = n + 1;			/* axis number */
         fint grid;				/* grid position */

         if (axnum < set_buf.naxis) {		/* cannot extend */
            if (set_buf.ax[n].naxis == 1) {	/* only one grid possible */
               grid = set_buf.ax[n].low;	/* THIS grid */
            } else do {
               char keyword[MAXTEXTLEN];	/* buffer for keyword */
               char message[MAXTEXTLEN];	/* buffer for message */
               fint dlev = 0;			/* no default */
               fint one = 1;			/* this is one */

               (void) sprintf( keyword, "GRID%d=", n + 1 );
               (void) sprintf( message, "Grid along %.*s", MAXFTSNAMLEN, set_buf.ax[n].ctype.a );
               (void) userint_c( &grid, &one, &dlev, tofchar( keyword ), tofchar( message ) );
               cancel_c( tofchar( keyword ) );
               if ((grid < set_buf.ax[n].low) || (grid > set_buf.ax[n].upp)) {
                  error_c( &one, COPYDATA_ERR1 );
               } else {
                  break;			/* leave loop */
               }
            } while ( 1 );			/* infinite loop */
         } else {
            do {
               char keyword[MAXTEXTLEN];	/* buffer for keyword */
               char message[MAXTEXTLEN];	/* buffer for message */
               fint dlev = 0;			/* no default */
               fint one = 1;			/* this is one */

               (void) sprintf( keyword, "GRID%d=", n + 1 );
               (void) sprintf( message, "Grid along %.*s", MAXFTSNAMLEN, set_buf.ax[n].ctype.a );
               (void) userint_c( &grid, &one, &dlev, tofchar( keyword ), tofchar( message ) );
               cancel_c( tofchar( keyword ) );
               if (grid < set_buf.ax[n].low) {	/* error */
                  error_c( &one, COPYDATA_ERR1 );
               } else {
                  break;			/* leave loop */
               }
            } while (1);			/* infinite loop */
            if (grid > set_buf.ax[n].upp) {	/* axis must be extended */
               fint gerror = 0;			/* GDS error return */
               fint naxis;			/* new size of axis */

               naxis = set_buf.ax[n].naxis - set_buf.ax[n].upp + grid;
               gds_extend_c( set_buf.set, set_buf.ax[n].ctype, &set_buf.ax[n].crpix, &naxis, &gerror );
               if (gerror < 0) {		/* error message */
                  fint  four = 4;		/* this is four */

                  error_c( &four, COPYDATA_ERR2 );
               }
               set_buf.ax[n].naxis = naxis;	/* new size */
               set_buf.ax[n].upp = grid;	/* new upper limit */
            }
         }
         cwlo = gdsc_word_c( set_buf.set, &axnum, &grid, &cwlo, &gerror );
         cwhi = gdsc_word_c( set_buf.set, &axnum, &grid, &cwhi, &gerror );
      }
   }
   /*
    * message for user
    */
   {
      int	n;
      fint	output_level = 3;
 						/* determine subset level */
      set_buf.subset = gdsc_substruct_c( set_buf.set, &cwlo, &cwhi, &gerror );
      if ( fid == NULL ) {
         (void) sprintf( message, "File #%d from %.*s into %.*s", currentfile, (int) nelc_c( fts_buf.dev ), fts_buf.dev.a, (int) nelc_c( set_buf.set ), set_buf.set.a );
      } else {
         (void) sprintf( message, "File %.*s into %.*s", (int) nelc_c( fts_buf.dev ), fts_buf.dev.a, (int) nelc_c( set_buf.set ), set_buf.set.a );
      }
      strcat( message, " (" );
      for (n = 0; n < set_buf.naxis; n++) {
         char	string[80];
         fint	axnum = n + 1;
         fint	grid;

         gerror = 0;
         grid = gdsc_grid_c( set_buf.set, &axnum, &set_buf.subset, &gerror );
         if (!gerror) {
            sprintf( string, "%d", grid );
         } else {
            strcpy( string, "-" );
         }
         strcat( message, string );
         if (axnum < set_buf.naxis) {
            strcat( message, "," );
         }
      }
      strcat( message, ")" );
      status_c( tofchar( message ) );		/* message for user */
      anyout_c( &output_level, tofchar( message ) );
   }
   /*
    * Loop to get data off tape. Only data which are in the sub frame
    * will be scaled to floating point numbers.
    */
   if (!nodata) {				/* load images */
      float	*fdata = NULL;			/* float pointer */
      double	*ddata = NULL;			/* double pointer */

      do {					/* read loop */
         fint ip;				/* pointer */
         fint np;				/* counter */

         if (!indata) {				/* load data into buffer */
            fts_data = fmake( fts_data, FITSBLOCKLEN * FITSBLOCKFACTOR );
            READ( tst, fts_data );		/* read next block */
            if (tst <= 0) {			/* end of file mark */
               PANIC( COPYDATA_ERR4 );		/* error message */
            }
            if (tst > 0 && tst%FITSBLOCKLEN) {	/* wrong block length */
               PANIC( COPYDATA_ERR5 );		/* error message */
            }
            if (fts_data.l < tst) {		/* buffer not large enough */
               fts_data = fmake( fts_data, tst );	/* increase read buffer */
               tst = mtbsr_c( &mtid, &bsr );	/* back skip a record */
               if (tst == bsr) {		/* skip okay */
                  READ( tst, fts_data );
               }
            }
            if (tst < 0) {			/* tape error */
               PANIC( COPYDATA_ERR6 );		/* error message */
            }
            indata = tst;			/* number of bytes in buffer */
         }
         nr = indata / fts_buf.nbytes;		/* number of pixels */
         if ((nc + nr) > ntotal) nr = ntotal - nc;
         nc += nr;				/* our count */
         initptr_c( bmin, bmax, smin, smax, &ndim, &nr, &nt );
         while (tobool(insideptr_c( &ip, &np ))) {
            char *ptr = &fts_data.a[ip*fts_buf.nbytes];
            fint  k;				/* loop counter */
            int   l;				/* int integer */

            if (ndata < np) {			/* not enough memory */
               fdata = realloc( fdata, sizeof( float ) * np );
               ndata = np;			/* new size of fdata */
            }
            switch( fts_buf.nbytes ) {		/* type of FITS data */
               case 1: {			/* char */
                  union {
                     char *tptr;		/* pointer to tape data */
                     byte *dptr;		/* pointer to local data */
                  } u;

                  u.tptr = ptr;			/* assign char pointer */
                  for (k = 0; k < np; k++) {	/* scaling loop */
                     l = u.dptr[k];		/* convert to int */
                     if (l == fts_buf.blank) {	/* is blank */
                        fdata[k] = set_buf.blank;
                     } else {			/* is data */
                        fdata[k] = l * fts_buf.bscal + fts_buf.bzero;
                     }
                  }
                  break;
               }
               case 2: {			/* short */
                  union {
                     char  *tptr;		/* pointer to tape data */
                     short *dptr;		/* pointer to local data */
                  } u;

                  u.tptr = ptr;			/* assign short pointer */
#if	( OS_INTEGER_TYPE == 1 )
                  {
                     union {
                        byte	b[2];
                        short	s;
                     } ui, uo;

                     for ( k = 0; k < np; k++ ) {
                        ui.s = u.dptr[k];
                        uo.b[0] = ui.b[1];
                        uo.b[1] = ui.b[0];
                        u.dptr[k] = uo.s;
                     }
                  }
#endif
                  for (k = 0; k < np; k++) {	/* scaling loop */
                     l = u.dptr[k];		/* convert to int */
                     if (l == fts_buf.blank) {	/* is blank */
                        fdata[k] = set_buf.blank;
                     } else {			/* is data */
                        fdata[k] = l * fts_buf.bscal + fts_buf.bzero;
                     }
                  }
                  break;
               }
               case 4: {			/* int or float */
                  if (fts_buf.bitpix > 0) {
                     union {
                        char *tptr;		/* pointer to tape data */
                        int  *dptr;		/* pointer to local data */
                     } u;

                     u.tptr = ptr;		/* assign char pointer */
#if	( OS_INTEGER_TYPE == 1 )
                     {
                        union {
                           byte	b[4];
                           int	l;
                        } ui, uo;

                        for ( k = 0; k < np; k++ ) {
                           ui.l = u.dptr[k];
                           uo.b[0] = ui.b[3];
                           uo.b[1] = ui.b[2];
                           uo.b[2] = ui.b[1];
                           uo.b[3] = ui.b[0];
                           u.dptr[k] = uo.l;
                        }
                     }
#endif
                     for (k = 0; k < np; k++) {	/* scaling loop */
                        l = u.dptr[k];		/* convert to int */
                        if (l == fts_buf.blank) {	/* is blank */
                           fdata[k] = set_buf.blank;
                        } else {		/* is data */
                           fdata[k] = l * fts_buf.bscal + fts_buf.bzero;
                        }
                     }
                  } else {
                     fint	ftype = 0;
                     union {
                        char	*tptr;
                        float	*fptr;
                     } u;

                     u.tptr = ptr;
                     spfpfl_c( &ftype, u.fptr, fdata, &np );
                     clspfp_c( fdata, &np );
                  }
                  break;
               }
               case 8: {			/* int or float */
                  if (fts_buf.bitpix > 0) {
                     error_c( &elev, COPYDATA_ERR3 );
                     break;
                  } else {
                     fint	ftype = 0;
                     int	idbl;
                     union {
                        char	*tptr;
                        double	*dptr;
                     } u;

                     if (nddata < np) {			/* not enough memory */
                        ddata = realloc( fdata, sizeof( double ) * np );
                        nddata = np;			/* new size of ddata */
                     }

                     u.tptr = ptr;
                     dpfpfl_c( &ftype, u.dptr, ddata, &np );
                     for (idbl=0; idbl<np; idbl++) fdata[idbl] = ddata[idbl];
                  }
                  break;
               }
               default: {
                  error_c( &elev, COPYDATA_ERR3 );
                  break;
               }
            }
            gdsi_write_c( set_buf.set, &cwlo, &cwhi, fdata, &np, &nd, &tid );
         }
         indata = 0;				/* reset number of bytes */
      } while (nt < ntotal);			/* until all pixels read */
      if (fdata) free( fdata );			/* free float buffer */
   }
   /*
    * Now skip over next tape mark.
    */
   if ( fid == NULL ) {
      fint	one = 1;

      if (mtfsf_c( &mtid, &one ) == 1) {
         currentfile++;
      } else {
         error_c( &elev, COPYDATA_ERR7 );
      }
   }
}


static	void	copyheader( void )
/*
 * copyheader copies the relevant FITS descriptors to the GDS descriptor
 * file.
 */
{
   char *ptr = fts_head.a;			/* pointer to FITS header */
   int   r;					/* record counter */

   for (r = 0; r < inhead; r += FITSRECLEN ) {
      int code;					/* code from getdsccode */

      code = getdsccode( ptr );			/* get code */
      switch( code ) {				/* what ? */
         case -1: {				/* no descriptor */
            break;				/* do nothing */
         }
         case  0: {				/* descriptor */
            fchar dsc;				/* FITS descriptor */
            fchar rec;				/* FITS record */
            fint  gerror = 0;			/* GDS return code */
            fint  level = set_buf.subset;	/* always on subset level */

            dsc.a = ptr; dsc.l = MAXFTSDSCLEN;	/* initialize f character */
            rec.a = ptr; rec.l = FITSRECLEN;	/* initialize f character */
						/* write full record */
            gdsd_wfits_c( set_buf.set, dsc, &level, rec, &gerror );
            break;
         }
         default: {				/* action depends on code */
            fchar dsc;				/* FITS descriptor */
            fchar rec;				/* FITS record */
            fint  gerror = 0;			/* GDS return code */
            fint  level;			/* level */
            int   putrec = 1;			/* write FITS record */
            int   putvar = 0;			/* write variable record */


            if (code & DSC_SKIP) break;		/* simple action */
            if (code & DSC_COMMENT) {
               if (!comment) break;
               putrec = 0; putvar = 1;
            }
            if (code & DSC_HISTORY) {
               if (!history) break;
               putrec = 0; putvar = 1;
            }
            if (code & DSC_SUB) {		/* on which level */
               level = set_buf.subset;		/* subset level */
            } else {
               level = 0;			/* on top level */
            }
            dsc.a = ptr; dsc.l = MAXFTSDSCLEN;	/* initialize f character */
            if (putrec) {
               rec.a = ptr; rec.l = FITSRECLEN;	/* initialize f character */
            } else {
               rec.a = ptr + MAXFTSDSCLEN;
               rec.l = FITSRECLEN - MAXFTSDSCLEN;
            }
            if (code & DSC_CHECK) {		/* check first */
               fint gerror = 0;			/* GDS error return */
               fint length;			/* length of descriptor item */

						/* get length to check existence */
               length = gdsd_length_c( set_buf.set, dsc, &level, &gerror );
               if (gerror >= 0) putrec = 0;	/* already exists */
            }
            if (putrec) {			/* do the write */
						/* write full record */
               gdsd_wfits_c( set_buf.set, dsc, &level, rec, &gerror );
            }
            if (putvar) {
               gdsd_wvar_c( set_buf.set, dsc, &level, rec, &gerror );
            }
            break;
         }
      }
      ptr += FITSRECLEN;			/* next record */
   }
}


static	void	getfitsdata( void )
/*
 * getfitsdata decodes the current FITS file and stores it in the
 * selected set. It tries to find out by itself where to put (i.e.
 * it tries to find out in which subset it should be put). This part
 * is actually done by the procedure gamble. Other routines called are
 * readheader (which reads the FITS header from tape), decodehead
 * (which unscrambles the FITS header) and copydata (which transfers
 * the data from tape to the chosen subset).
 */
{
   readheader( );				/* read the FITS header */
   decodehead( );				/* decode FITS header */
   gamble( );					/* guess where to put it */
   copydata( );					/* put data in (sub)set */
   copyheader( );				/* put descriptors in (sub)set */
}


static	void	init_rfits( void )
/*
 * Initializes f characters.
 */
{
   int n;					/* loop counter */

						/* initialize syn_buf */
   for (n = 0; n < MAXSYNONYMS; n++) {		/* loop over all synonyms */
      syn_buf[n].cname = finit( syn_buf[n].cnameb, MAXFTSNAMLEN );
      syn_buf[n].ctype = finit( syn_buf[n].ctypeb, MAXFTSNAMLEN );
   }
						/* initialize fts_buf */
   fts_buf.dev = finit( fts_buf.devb, MAXDEVNAMLEN );
   fts_buf.instrume = finit( fts_buf.instrumeb, MAXFTSNAMLEN );
   for (n = 0; n < MAXAXES; n++) {		/* loop over all axes */
      fts_buf.ax[n].ctype = finit( fts_buf.ax[n].ctypeb, MAXFTSNAMLEN );
      fts_buf.ax[n].cunit = finit( fts_buf.ax[n].cunitb, MAXFTSNAMLEN );
      fts_buf.ax[n].dtype = finit( fts_buf.ax[n].dtypeb, MAXFTSNAMLEN );
      fts_buf.ax[n].dunit = finit( fts_buf.ax[n].dunitb, MAXFTSNAMLEN );
   }
						/* initialize set_buf */
   set_buf.set = finit( set_buf.setb, MAXSETNAMLEN );
   set_buf.instrume = finit( set_buf.instrumeb, MAXFTSNAMLEN );
   for (n = 0; n < MAXAXES; n++) {		/* loop over all axes */
      set_buf.ax[n].ctype = finit( set_buf.ax[n].ctypeb, MAXFTSNAMLEN );
      set_buf.ax[n].cunit = finit( set_buf.ax[n].cunitb, MAXFTSNAMLEN );
      set_buf.ax[n].dtype = finit( set_buf.ax[n].dtypeb, MAXFTSNAMLEN );
      set_buf.ax[n].dunit = finit( set_buf.ax[n].dunitb, MAXFTSNAMLEN );
   }
}


static	void	close_set( void )
/*
 * close_set close the set into which the FITS files are copied.
 */
{
   fint gerror = 0;				/* GDS error */

   gds_close_c( set_buf.set, &gerror );		/* close the set */
}


MAIN_PROGRAM_ENTRY                              /* program entry */
/*
 * The main program gets in touch with the user via the Master Control
 * Program. Furthermore it repeats the operations per set, that is,
 * from the same tape device different (or similar) files can be
 * read into different sets.
 */
{
   fint n;					/* loop counter */

   init_c( );                                   /* enter servant mode */
   {
      fint	error_level = 4;		/* FATAL errors */

      if (CHAR_BIT != 8) {
         error_c( &error_level, MAIN_ERR1 );
      }
      if (sizeof( char ) != 1) {
         error_c( &error_level, MAIN_ERR2 );
      }
      if (sizeof( short ) != 2) {
         error_c( &error_level, MAIN_ERR3 );
      }
      if (sizeof( int ) != 4) {
         error_c( &error_level, MAIN_ERR4 );
      }
      if (sizeof( float ) != 4) {
         error_c( &error_level, MAIN_ERR5 );
      }
   }
   {
      fchar	key = AUTO_KEY;
      fchar	mes = AUTO_MES;
      fint	input_level = 2;		/* hidden keyword */
      fint	one = 1;

      (void) userlog_c( &amode, &one, &input_level, key, mes );
      amode = tobool( amode );
   }
   {
      fchar	key = NODATA_KEY;
      fchar	mes = NODATA_MES;
      fint	input_level = 2;		/* hidden keyword */
      fint	one = 1;

      (void) userlog_c( &nodata, &one, &input_level, key, mes );
      nodata = tobool( nodata );
   }
   {
      fchar	key = COMMENT_KEY;
      fchar	mes = COMMENT_MES;
      fint	input_level = 2;		/* hidden keyword */
      fint	one = 1;

      (void) userlog_c( &comment, &one, &input_level, key, mes );
      comment = tobool( comment );
   }
   {
      fchar	key = HISTORY_KEY;
      fchar	mes = HISTORY_MES;
      fint	input_level = 2;		/* hidden keyword */
      fint	one = 1;

      (void) userlog_c( &history, &one, &input_level, key, mes );
      history = tobool( history );
   }
   init_rfits( );				/* initialisation */
   addsynonym( tofchar( "LL-50" ), tofchar( "RA-NCP" ) );
   addsynonym( tofchar( "MM-50" ), tofchar( "DEC-NCP" ) );
   IDENTIFICATION( "RFITS", VERSION );		/* show who we are */
   getinfile( );                                /* get input fits files */
   if ( fid == NULL ) getintape( );		/* get input tape device */
   do {						/* main loop */
      getinfiles( );				/* get the file numbers */
      if (!nfile) break;			/* leave main loop */
      getoutset( );				/* get set to load data into */
      for (n = 0;  n < nfile; n++) {		/* loop over files */
         position( files[n] );			/* position tape */
         getfitsdata( );			/* get data off tape */
      }
      close_set( );				/* close the set */
      if ( fid != NULL ) getinfile( );		/* loop only once */
   } while ( nfile );				/* infinite loop */
   if ( fid == NULL ) (void) mtclose_c( &mtid );/* close tape device */
   finis_c( );                                  /* exit servant mode */
   return( EXIT_SUCCESS );			/* exit with status */
}
