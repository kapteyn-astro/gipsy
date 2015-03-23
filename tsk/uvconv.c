/*
                           COPYRIGHT (c) 1990
                     Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.

#>            uvconv.dc1

Program:      UVCONV

Purpose:      Load UV data from tape into a GIPSY set.

Category:     UV, TAPES

File:         uvconv.c

Author:       M.G.R. Vogelaar
              (written by Hans de Haan)

Keywords:

  INTAPE=     Tape device to load from [list of all tape devices].

  OUTSET=     Set to load data into. The following keywords depend
              on whether the set already exists or not.

  SAMPLETIME= Sampletime where all datapoints are scaled to [Greatest
              Common Divisor]. The sampletime given by the user must
              be a multiple factor of the Greatest Common Divisor.

  AXIS=       Choose which axis, the complex (COS/SIN) axis or
              Amplitude/Phase axis.

Description:  UVCONV loads a uv-data tape and converts it to a GDS
              set.
              UVCONV creates a descriptor file from the headers given
              on the uv-data tape.
              The user will be asked to what sampletime the data has
              to be scaled, before this the program will show which
              sampletimes have been used, default the GCD(Greatest
              Common Divisor) will be used as sampletime.

              Blank values in the uv-data are recognized by UVCONV.

              The destination of the uv-data tape is OUTSET= . There
              will be created a descriptor file and a image file.



Updates:      June 28, 1992: Hans, document created.

#<

*/

#include "stdio.h"
#include "ctype.h"
#include "string.h"
#include "math.h"
#include "stdlib.h"
#include "gipsyc.h"
#include "cmain.h"
#include "anyout.h"
#include "myname.h"
#include "error.h"
#include "finis.h"
#include "init.h"
#include "mtopen.h"
#include "mtrew.h"
#include "mtstat.h"
#include "mtfsf.h"
#include "userint.h"
#include "nelc.h"
#include "gds_extend.h"
#include "gds_exist.h"
#include "gds_delete.h"
#include "gds_create.h"
#include "gdsa_crecol.h"
#include "gdsa_wcint.h"
#include "gdsa_istable.h"
#include "gdsc_fill.h"
#include "gdsc_word.h"
#include "gdsd_wchar.h"
#include "gdsd_wdble.h"
#include "gdsd_wreal.h"
#include "gdsd_wint.h"
#include "gdsi_write.h"
#include "setfblank.h"
#include "axtype.h"
#include "userchar.h"
#include "userlog.h"
#include "cancel.h"


extern   void  cnvrtc_c( fint *, unsigned char *, unsigned char *, fint * );
extern   void  cnvrth_c( fint *, char *, short *, fint * );
extern   void  cnvrtf_c( fint *, char *, int *, fint * );
extern   void  cnvrte_c( fint *, char *, float *, fint * );
extern   void  cnvrtd_c( fint *, char *, double *, fint * );

extern   fint  mtread_c( fint *, char *, fint *);


/* Some definitions */
#define VERSION     "1.0"

static  fint        error_level_fatal = 4;
#define FATAL       (&error_level_fatal)                  /* Fatal errorlevel */

static  fint        anyout_level_default = 0;
#define ANYOUT_DEF  (&anyout_level_default)     /* default level for anyout_c */

static  fint        anyout_level_test = 16;
#define ANYOUT_TEST (&anyout_level_test)        /* test level for anyout_c */

#define INSAMPLE_KEY tofchar("SAMPLETIME=")
#define INSAMPLE_MES tofchar("Give sampletime [biggest common divisor]")

/* See for more information, about the defined parameters, the Westerbork
   tape format documents */
#define MAXPOL      4                      /* maximum number of polarisations */
#define EMPTY       -32768                       /* number equals empty field */
#define LAST_GRP    -2147450880                   /* number equals last group */

#define BLKLEN      3840                       /* size of tape block in bytes */
#define RECLEN      128                                /* size of tape record */
#define LABLEN      240                             /* size of label in bytes */
#define MAXLEN      80                                /* Max. textline length */
#define TEXTLEN     100
#define VOLLEN      23                                  /* Max. volume length */
#define SCLINFLEN   10                             /* Max. scale info records */
#define AMPL        0                                            /* Amplitude */
#define PHASE       1                                                /* Phase */

#define INTAPE_DEV  tofchar("?INTAPE=Tape device to load from [list of all tape devices]")
#define MNTERR      tofchar("Cannot mount the specified unit!")

#define BANDNR      -10

/* Macro to create static storage for Fortran type string */
#define fmake(fchr,size) { \
                            static char buff[size+1]; \
                            int i; \
                            for (i = 0; i < size; buff[i++] = ' '); \
                            buff[i] = 0; \
                            fchr.a = buff; \
                            fchr.l = size; \
                         }


/* Calloc version of 'fmake'  */
#define finit( fc , len ) { fc = calloc( ( len + 1 ),  sizeof( char ) ) ;  \
                            fc[ len ] = '\0' ; \
                           }

/* CONTENTS OF FD HEADER */
typedef struct {
   short  udbf;        /* Unequal data block flag */
   int    nfd;         /* Record number of this record */
   int    nfde;
   short  sday;        /* Local UT day number */
   short  stim;        /* Start UT time in Units of 10 sec */
   short  etim;        /* End   UT tome in Units of 10 sec */
   short  empty;       /* Empty (delete character) */
   short  fvers;       /* Tape format version */
   short  lrcrd;       /* Record length in bytes */
   short  olsys;       /* Online program system number */
   int    noh;         /* First record number of first OH group */
   int    moh;         /* Number of OH groups */
   short  lsh;
   int    nsh;         /* First record number of first SH group */
   int    msh;         /* record number of SH group */
   int    nih;         /* First record number of first IH group */
   int    ndb;         /* First record number of first DB group */
   int    mdb;         /* total number of DB records */
   int    nbl;         /* total number of DB records */
   char   volume[6];
   char   label[4];
   int    reddate;     /* Civil date on which the dataset was made YYDDD */
   short  redtime;     /* Civil time in minutes on which the dataset was made */
   short  inftb[384];  /* Interferometernumber table. */
} FD;

/* CONTENTS OF OH HEADER */
typedef struct {
   short  udbf;        /* Unequal data block flag */
   char   oh[2];       /* Type identification: Observation header (OH) */
   int    noh;         /* Record number of this record */
   int    nohn;        /* Record number of this record */
   int    loh;         /* Number of OH records in this group */
   int    nohe;        /* First OH record number at end of dataset */
   short  sday;        /* Local UT day number */
   short  stim;        /* Start UT time in units of 10 sec */
   short  etim;        /* End UT time in units of 10 sec */
   short  empty;
   char   field[12];   /* Fieldname */
   int    volgnr;      /* Observation number (YYNNNNN) */
   short  stuurc;      /* Online peripherals control bits */
   float  band;
   short  ntot;        /* Total number of channels of the backend */
   short  nrfeq;       /* Total number of frequency points or bands */
   short  sfreq;       /* Spacing frequency points (DLB), bands in use (DCB) */
   short  nrpol;       /* Number of polarization channels */
   short  nrint;       /* Number of interferometers */
   short  confnr;
   char   be_code[4];
   double ra0;         /* Apparent right ascension field centre (circles) */
   double dec0;        /* Apparent declination field centre (circles) */
   double freq;        /* Obs. Freq. (DLB) Prim. Fringe Stopping Freq. (DCB) */
   double hast;        /* Hour angle middle of first 10 sec */
   double haend;       /* Hour angle middle of last 10 sec */
   float  vlcty;       /* Velocity in system given by velc */
   short  velc;        /* Velocity reference system code */
   short  taper;       /* Code of weighting function */
   short  mspat;       /* Mosaicking pattern number (=0 if no mosaicking */
   short  mposn;       /* Position number within the mosaicking pattern */
   short  nrsts;       /* Total number of sets in database */
   short  nrfrq;       /* Total number of frequency points in database */
   short  lent;        /* Number of bytes per set in table */
} OH;

/* CONTENTS OF OH TABLE */
typedef struct {
   int    bfreq;       /* Set observing frequency (2**-16 MHz) */
   char   datyp[2];    /* Data type (IF, XX, XY, YX or YY) */
   short  bandnr;      /* Band number (0 = continuum, 1, 2, ..) */
   int    nsh;         /* First record number of current set */
} OHTAB;

/* CONTENTS OF SC HEADER */
typedef struct {
   char   pnchi[320]; /* general information fields */
} SC;

/* CONTENTS OF SC TABLE */
typedef struct {
   int ra0;           /* RA epoch position i in 2**(-32) circles */
   int dec0;          /* DEC epoch position i in 2**(-32) circles */
   int ra1;           /* RA apparent position i in 2**(-32) circles */
   int dec1;          /* DEC apparent position i in 2**(-32) circles */
   int sdec;          /* SIN(DEC1) in units of 2**(-31) */
   int cdec;          /* COS(DEC1) in units of 2**(-31) */
   int vnpa;          /* Apparent position vector position i in 2**(-31) */
   int dvnpa;         /* Differential apparent position vector position i
                         in 2**(-31) */
} SCTAB;

/* CONTENTS OF SC HEADER */
typedef struct {
   short  udbf;       /* Unequal data block flag */
   int    nsh;        /* Record number of this record */
   int    lsh;
   int    mhlnk;
   short  stim;       /* Start U.T. time in units of 10 sec */
   short  bandnr;     /* Frequentie bandnr. */
   char   datyp[2];   /* Data type (IF,XX,XY,YX or YY) */
   short  polc;       /* Dipole position code */
   short  nrint;      /* Number of interferometers in the set */
   int    pts;        /* Total number of observed point in the set */
   short  nent;
   short  lent;
   int    nih;
} SH;

/* CONTENTS OF SH TABLE */
typedef struct {
   short infnr;
   short wtel;
   short otel;
   short rbas;
} SHTAB;

/* CONTENTS OF IH HEADER */
typedef struct {
   short  udbf;       /* Unequal data block flag */
   int    nih;        /* Record number of this record */
   int    lih;
   int    ihlnk;
   short  stim;       /* Start U.T. time in units of 10 sec */
   short  bandnr;     /* Frequentie bandnr. */
   short  infnr;      /* Interferometer number of this interferometer */
   short  wtel;       /* West telescope indicator: 0-d is 0..13 */
   short  otel;       /* East telescope indicator: 0-d is 0..13 */
   int    bfreq;      /* Set frequency (2**(-16) Mhz) */
   float  drt;        /* Baseline of the interferometer (M) */
   float  hab;        /* Start hrangle, middle of the first integr. tm UT sec */
   float  hae;        /* End hrangle, middle of the last integr. tm UT sec*/
   float  dha;        /* Hrangle increment (Integration time in U.T. sec) */
   short  ld;
   short  ndatp;      /* Number of datapoints, inclusive NDELP */
   short  ndelp;
   float  acos;
   float  asin;
   short  nexp;       /* Common exponential scaling factor */
   short  fscal;      /* Common multiplication scaling factor */
   short  offs;       /* Common offset scaling factor */
   short  inct;       /* Increment time in U.T.sec */
   short  dwelt;      /* Time per mos. pat. pos. per radial in sec. */
   int    volgnr;     /* Observation number */
   float  intt;       /* Integration time in U.T. sec */
   short  dradt;      /* Time betw. 2 succ. radials of the same mos. pattern                             pos. in sec. (=INCT if no mosaicking)  */
   int    lnrihdb[6];
   int    nentar[6];
} IH;

/* The paramaters for every axis in the GDS cube */
typedef struct {
   fchar  nunit;
   fchar  sunit;
   fchar  ctype;
   double crpix;
   fint   naxis;
   double crval;
   double crota;
   double cdelt;
   fchar  cunit;
} AXDESCR;

/* Structure to buffer the datapoints coming from tape */
typedef struct {
   float  *cos;
   float  *sin;
   float  *ampl;
   float  *fase;
} DATAPOINTS;

/* Information used for scaling the datapoints */
typedef struct {
   int    nrdpnts;
   int    sampt;
   int    index;
} SCALEDATA;


/* Define some global variables                                               */
FD                   fd;                /* WSRT tape file descriptor */
OH                   oh;                /* Observation header */
SC                   sc;                /* System Calibration header */
SH                   sh;                /* Set Header */
IH                   ih;                /* Interferometer header */
OHTAB                *ohtab = NULL;     /* OH set table */
SCTAB                *sctab = NULL;     /* SC set table (only version == 6) */
SHTAB                *shtab = NULL;     /* SH set table */
fint                 unit;              /* tape unit number */
fint                 tform;             /* tape format (VMS or IBM) */
fint                 np=1;              /* Number of param's to be converted */
fchar                fdblk;             /* FD block */
fchar                ohblk;             /* OH block */
fchar                scblk;             /* SC block */
fchar                scblk5_6;          /* SC block 5 and 6 only ver.= 6 and                                               olsys >= 62 */
fchar                shblk;             /* SH block */
fchar                buf;               /* Buffer used in readblock() */
int                  bufoffset = 0;     /* Offset used in readblock() */
fint                 nread = 0, nreq = 3840;
static int           rnfd = 0, rnoh = 0, rnsh = 0, rnih = 0;
float                blank = 0;
static DATAPOINTS    *datapnts;         /* Datapoints buffer */
static SCALEDATA     *sclinf;           /* Scale infomation buffer */
static char          text[TEXTLEN];
bool                 first = TRUE;      /* Used in uvload_ih_db() */
static fint          usamplt[1];        /* Buffer with user sampletime */
static fint          nusamplt;
static fint          samplt;
static int           ihnrs[200];        /* Used in uvload_ih() for checking
                                           number ih, filled in uvload_sh */
bool                 scale_2_gcd = TRUE; /* Used in uvload_ih_db() if data has                                       to be scaled to Greatest Common Divisor */
bool                 had_not_all_diff_pols; /* Used in uvload_sh(), initialized
                                               in uvload_oh() */
bool                 amphas = FALSE;    /* Used in uvload_ih_db() initialized
                                           in uvload_ini */


                                          /* calculates corrected frequencies */
double frqcor( double frqb, double frq0, double frqv, float v, short velc )
{
   static double c = 299792.480;
   if ((velc == 0) || (velc == 1)) {
      return( frq0 * ( 1 - v / c ) - frqv + frqb );
   } else if ((velc == 2) || (velc == 3)) {
      return( 1 / ( 1 / frqb - 1 / frqv + ( 1 + v / c ) / frq0 ) );
   } else {
      return( frq0 );
   }
}

void readerror(int errnr)
/* If error occured during reading, then routine gives back reason of error */
{

      switch( errnr ){
       case -1 :sprintf(text, "System error !");
                break;
       case -2 :sprintf(text, "End of tape encountered ! ");
                break;
       case -4 :sprintf(text, "Tape device not open !");
                break;
       case -9 :sprintf(text, "Call error !");
                break;
       default :if(errnr < 0) anyout_c(ANYOUT_DEF, tofchar(text));
                error_c(FATAL, tofchar("Error reading block"));
                break;
      }
}

void readblk(char *blk1)
/* Readblk reads a block (3840 bytes) from tape. If the block length is
   less then 3840 bytes then still 3840 bytes are read. But then an offset
   and a buffer is used. It works like this: if blocklength is less, then
   two blocks of this length are read, copy from these two blocks 3840 bytes
   to the block that had to be read and for the following block start at the
   offset an copy the already read bytes.

   OUTPUT:

   blk1   :  Block that has to be read.
*/
{
  int diff, ch2cpy;

  if(bufoffset == 0){
    nread = mtread_c( &unit, blk1, &nreq );
  /* if blocklength not equal to 3840 then equal nreq to the read blocklength */
    if(nreq != nread) nreq = nread;
    if(nread < 0) readerror(nread);
    /* bloklength read smaller then default blocklength ?? */
    if(nread < BLKLEN) {
      diff = BLKLEN - nread;
      nread = mtread_c( &unit, buf.a, &nreq );
      if(nread < 0) readerror(nread);
      /* copy bytes to block that had to be read until default block length is
         reached */
      memcpy(&blk1[nread], &buf.a[0], diff);
      bufoffset = diff;
    }
  }
  else{
    ch2cpy = nread - bufoffset;
    memcpy(&blk1[0], &buf.a[bufoffset], ch2cpy);
    nread = mtread_c( &unit, buf.a, &nreq );
    if(nread < 0) readerror(nread);
    diff = BLKLEN - ch2cpy;
    bufoffset = diff;
    if(bufoffset == nread) bufoffset = ch2cpy;
    memcpy(&blk1[bufoffset], &buf.a[0], diff);
    if(diff == nread) bufoffset = 0;
  }
}



void create_descr(fchar setname, int maxdtpnts, int samplt, int nrdifst)
/* Create_descr creates the descriptor file. A complete descriptor file
   can be created from the headers on tape, no input from the user is
   needed.

   INPUT:

   setname     :  Name of the set which has to be created.
   maxdtpnts   :  Number of points on the time axis.
   samplt      :  which sampletime is used.
   nrdifst     :  Number of different sampletimes used.
*/
{
         int     i, j;
         fint    grid, axis, errcode, level, bfreq;
         fint    naxis = 0;
  static fint    derror, gerror;
  static fint    toplevel = 0;
  static fchar   msg;
  static char    msg1[MAXLEN];
  static fint    skysys, prosys, velsys;
  static fint    nintf, npol, nfreq;
  static fchar   nunit, sunit;
  static fchar   instrume;
  static fint    typ, bandnr, cw;
         fchar   datyp;
         AXDESCR ax[15];
         int     offset;
         fint    ra0, ra1, dec0, dec1;
         fint    sdec, cdec, vnpa, dvnpa;
         fchar   line, cont;
         fchar   vol, lab;


  fmake(msg,  MAXLEN);
  /* Is it a line observation ? */
  if((strncmp(oh.be_code, "DLB", 3) == 0) ||
     (strncmp(oh.be_code, "DXB", 3) == 0)   ){
    msg = tofchar("OBSTYPE");
    fmake(line, MAXLEN);
    line = tofchar("LINE");
    gdsd_wchar_c(setname, msg,  &toplevel, line, &derror);
    msg = tofchar("BANDW");
    gdsd_wreal_c(setname, msg,  &toplevel, &oh.band, &derror);

  }
  /* Or is it a continuum observation ? */
  else{
    msg = tofchar("OBSTYPE");
    fmake(cont, MAXLEN);
    cont = tofchar("CONT");
    gdsd_wchar_c(setname, msg,  &toplevel, cont, &derror);

  }
  /* Number of interferometers */
  nintf = oh.nrint;
  msg = tofchar("NINTF");
  gdsd_wint_c(setname, msg,  &toplevel, &nintf, &derror);
  /* Number of polarisations */
  npol = oh.nrpol;
  msg = tofchar("NPOL");
  gdsd_wint_c(setname, msg,  &toplevel, &npol, &derror);
  /* Number of frequencies */
  nfreq = oh.nrfrq;
  msg = tofchar("NFREQ");
  gdsd_wint_c(setname, msg,  &toplevel, &nfreq, &derror);
  /* Tape volume */
  msg = tofchar("MAPVSN");
  fmake(vol, MAXLEN);
  vol = tofchar(fd.volume);
  gdsd_wchar_c(setname, msg,  &toplevel, vol, &derror);
  /* Tape label */
  msg = tofchar("MAPLAB");
  fmake(lab, MAXLEN);
  lab = tofchar(fd.label);
  gdsd_wchar_c(setname, msg,  &toplevel, lab, &derror);

  for(i=0;i<5;i++){
    fmake(ax[i].nunit, 20);
    fmake(ax[i].sunit, 20);
    fmake(ax[i].ctype, 20);
  }
  /* Fill in the parameters for every axis */
  ax[0].ctype = tofchar("TIME");   /* Time axis */
  ax[0].nunit = tofchar("SECOND");
  ax[0].sunit = tofchar(" ");
  ax[0].crpix = 1.0;
  ax[0].naxis = maxdtpnts;
  ax[0].crval = fd.stim;
  ax[0].crota = 0.0;
  ax[0].cdelt = samplt;
  ax[0].cunit = tofchar("SECOND");

  ax[1].ctype = tofchar("INTFM"); /* Interferometer axis */
  ax[1].nunit = tofchar(" ");
  ax[1].sunit = tofchar(" ");
  ax[1].crpix = 1.0;
  ax[1].naxis = sh.nrint;
  ax[1].crval = 0.0;
  ax[1].crota = 0.0;
  ax[1].cdelt = 1.0;
  ax[1].cunit = tofchar(" ");

  ax[2].ctype = tofchar("POL"); /* Polarization  axis */
  ax[2].nunit = tofchar(" ");
  ax[2].sunit = tofchar(" ");
  ax[2].crpix = 1.0;
  ax[2].naxis = oh.nrpol;
  ax[2].crval = 0.0;
  ax[2].crota = 0.0;
  ax[2].cdelt = 1.0;
  ax[2].cunit = tofchar(" ");;

  ax[3].ctype = tofchar("FREQ"); /* Frequency point or band axis */
  ax[3].nunit = tofchar(" ");
  ax[3].sunit = tofchar(" ");
  ax[3].crpix = 1.0;
  ax[3].naxis = oh.nrfrq;
  ax[3].crval = 0.0;
  ax[3].crota = 0.0;
  ax[3].cdelt = 1.0;
  ax[3].cunit = tofchar(" ");

  fmake(instrume, MAXLEN);
  /* Tape version 6 then the fifth axis is the Complex/Amplitude-Phase axis */
  if(fd.fvers == 6){
    instrume = tofchar("WSRT");
    gdsd_wchar_c(setname, tofchar("INSTRUME"), &toplevel, instrume, &derror);
    if(amphas)
      ax[4].ctype = tofchar("AMPPHAS"); /* Amplitude and Phase axis */
    else
      ax[4].ctype = tofchar("COMPLEX"); /* "Complex axis" */
    ax[4].nunit = tofchar(" ");
    ax[4].sunit = tofchar(" ");
    ax[4].crpix = 1.0;
    ax[4].naxis = 2;
    ax[4].crval = 0.0;
    ax[4].crota = 0.0;
    ax[4].cdelt = 1.0;
    ax[4].cunit = tofchar(" ");
    naxis = 5;
  }
  else
  /* Tape version 7 then the fifth axis is the position axis and the sixth axis
     is the Complex/Amplitude-Phase axis */
   if(fd.fvers == 7){
     instrume = tofchar("WSRT-Mosaicking");
     gdsd_wchar_c(setname, tofchar("INSTRUME"), &toplevel, instrume, &derror);
     ax[4].ctype = tofchar("POSITION"); /* Position axis */
     ax[4].nunit = tofchar(" ");
     ax[4].sunit = tofchar(" ");
     ax[4].crpix = 1.0;
     ax[4].naxis = fd.moh;
     ax[4].crval = 0.0;
     ax[4].crota = 0.0;
     ax[4].cdelt = 1.0;
     ax[4].cunit = tofchar(" ");

     if(amphas)
       ax[5].ctype = tofchar("AMPPHAS"); /* Amplitude and Phase axis */
     else
       ax[5].ctype = tofchar("COMPLEX"); /* "Complex axis" */
     ax[5].nunit = tofchar(" ");
     ax[5].sunit = tofchar(" ");
     ax[5].crpix = 1.0;
     ax[5].naxis = 2;
     ax[5].crval = 0.0;
     ax[5].crota = 0.0;
     ax[5].cdelt = 1.0;
     ax[5].cunit = tofchar(" ");
     naxis = 6;
   }

  /* Loop over all axes */
  for(i=1;i <= naxis;i++){
     typ = axtype_c(ax[i-1].ctype, nunit, sunit, &skysys, &prosys, &velsys );

     gds_extend_c(setname, ax[i-1].ctype,
                  &ax[i-1].crpix, &ax[i-1].naxis,
                  &gerror);
     if(gerror == -28){
       fint errorlev = 1;
       error_c( &errorlev, tofchar("axis already in use") );
     }

     /* Write CRVALn, CROTAn, CDELTn and CUNITn in header at top level */
     sprintf( msg1, "CRVAL%d", i );
     msg = tofchar(msg1);
     gdsd_wdble_c(setname, msg,  &toplevel, &ax[i-1].crval, &derror);
     sprintf( msg1, "CROTA%d", i );
     msg = tofchar(msg1);
     gdsd_wdble_c(setname, msg,  &toplevel, &ax[i-1].crota, &derror);
     sprintf( msg1, "CDELT%d", i );
     msg = tofchar(msg1);
     gdsd_wdble_c(setname, msg,  &toplevel, &ax[i-1].cdelt, &derror);
     sprintf( msg1, "CUNIT%d", i );
     msg = tofchar(msg1);
     gdsd_wchar_c(setname, msg,  &toplevel, ax[i-1].cunit, &derror);
  }
  fmake(datyp, 8);
  /* write frequencies at the freq level */
  for(i=0, grid=0, offset=1;i < oh.nrfrq;i++, grid++, offset += oh.nrpol){
    level = 0; axis = 4; errcode = 0;
    cw    = gdsc_word_c(setname, &axis, &grid, &level, &errcode);
    for(j=0;j < oh.nrpol;j++){
      bfreq = ohtab[j+offset].bfreq;
      sprintf(msg1, "BFREQ_%s" ,ohtab[offset+j].datyp);
      msg = tofchar(msg1);
      gdsd_wint_c(setname, msg, &cw, &bfreq, &errcode);
      bandnr = ohtab[j+offset].bandnr;
      sprintf(msg1, "BNDNR_%s", ohtab[offset+j].datyp);
      msg = tofchar(msg1);
      gdsd_wint_c(setname, msg, &cw, &bandnr, &errcode);
    }
  }
/* PAY ATTENTION: The condition (See tape format7 document SC-5/6 BLOCK)
   for which this should work is NOT CORRECT */
/*
  if((fd.fvers == 6) && (fd.olsys >= 60)){
    for(i=1, grid=0;i < fd.moh;i++, grid++){
      level = 0; axis = 5; errcode = 0;
      cw    = gdsc_word_c(setname, &axis, &grid, &level, &errcode);
      ra0 = sctab[i-1].ra0;
      msg = tofchar("RA0");
      gdsd_wint_c(setname, msg, &cw, &ra0, &errcode);
      dec0 = sctab[i-1].dec0;
      msg = tofchar("DEC0");
      gdsd_wint_c(setname, msg, &cw, &dec0, &errcode);
      ra1 = sctab[i-1].ra1;
      msg = tofchar("RA1");
      gdsd_wint_c(setname, msg, &cw, &ra1, &errcode);
      dec1 = sctab[i-1].dec1;
      msg = tofchar("DEC1");
      gdsd_wint_c(setname, msg, &cw, &dec1, &errcode);
      sdec = sctab[i-1].sdec;
      msg = tofchar("SDEC");
      gdsd_wint_c(setname, msg, &cw, &sdec, &errcode);
      cdec = sctab[i-1].cdec;
      msg = tofchar("CDEC");
      gdsd_wint_c(setname, msg, &cw, &cdec, &errcode);
      vnpa = sctab[i-1].vnpa;
      msg = tofchar("VNPA");
      gdsd_wint_c(setname, msg, &cw, &vnpa, &errcode);
      dvnpa = sctab[i-1].dvnpa;
      msg = tofchar("DVNPA");
      gdsd_wint_c(setname, msg, &cw, &dvnpa, &errcode);
    }
  }
*/
}


void  write_info_at_intfm(fchar setname)
/* Write_info_at_intfm writes the information like: Interferometer number,
   West and East telescope indicator and the Baseline, at the inter-
   ferometer level.
*/
{
          int     i;
          fint    grid, level, axis, errcode, cw;
   static fchar   msg;
   static char    msg1[MAXLEN];
          fint    infnr;
          fint    wtel, otel;
          fint    rbas;


  fmake(msg,  MAXLEN);
  /* Write interferometer info at the intf level */
  for(i=0, grid=0;i < sh.nent;i++, grid++){
    level = 0; axis = 2; errcode = 0;
    cw    = gdsc_word_c(setname, &axis, &grid, &level, &errcode);
    infnr = shtab[i].infnr;
    sprintf(msg1,"INFNR_%s",sh.datyp);
    msg = tofchar(msg1);
    gdsd_wint_c(setname, msg, &cw, &infnr, &errcode);
    wtel  = shtab[i].wtel;
    sprintf(msg1,"WTEL_%s",sh.datyp);
    msg = tofchar(msg1);
    gdsd_wint_c(setname, msg, &cw, &wtel, &errcode);
    otel  = shtab[i].otel;
    sprintf(msg1,"OTEL_%s",sh.datyp);
    msg = tofchar(msg1);
    gdsd_wint_c(setname, msg, &cw, &otel, &errcode);
    rbas  = shtab[i].rbas;
    sprintf(msg1,"RBAS_%s",sh.datyp);
    msg = tofchar(msg1);
    gdsd_wint_c(setname, msg, &cw, &rbas, &errcode);
  }
  had_not_all_diff_pols -= 1;
}


void write_2_image(fchar setname, int intfm,
                   int  nrdtpnts, int freq,
                   int  pol,      int pos   )
/* Write_2_image writes the datapoints to the image file.

   INPUT :

   setname  :    Name of the image file.
   intfm    :    From which interferometer are the datapoints.
   nrdtpnts :    Number of datapoints to write.
   freq     :    At which frequency.
   pol      :    Which polarization.
   pos      :    If mosaicking, which position.
*/
{
   fint   size, nd, tid = 0;
   fint   clo, chi;
   fint   blo[6], bhi[6];
   fint   subset;


   size   = nrdtpnts;
   subset = 0;
   blo[0] = 0;
   bhi[0] = size - 1;
   blo[1] = bhi[1] = intfm;
   blo[2] = bhi[2] = pol;
   blo[3] = bhi[3] = freq;

   if((fd.fvers == 7) && (fd.olsys >= 62)){
     blo[4] = bhi[4] = pos;
     blo[5] = bhi[5] = AMPL;
   }
   else
     blo[4] = bhi[4] = AMPL;

   /* write COS/AMPLITUDE */
   clo = gdsc_fill_c(setname, &subset, blo);
   chi = gdsc_fill_c(setname, &subset, bhi);
   if(amphas)
     gdsi_write_c(setname, &clo, &chi, datapnts[intfm].ampl, &size, &nd, &tid);
   else
     gdsi_write_c(setname, &clo, &chi, datapnts[intfm].cos, &size, &nd, &tid);


   if((fd.fvers == 7) && (fd.olsys >= 62))
     blo[5] = bhi[5] = PHASE;
   else
     blo[4] = bhi[4] = PHASE;

   /* write SIN/PHASE */
   clo = gdsc_fill_c(setname, &subset, blo);
   chi = gdsc_fill_c(setname, &subset, bhi);
   if(amphas)
     gdsi_write_c(setname, &clo, &chi, datapnts[intfm].fase, &size, &nd, &tid);
   else
     gdsi_write_c(setname, &clo, &chi, datapnts[intfm].sin, &size, &nd, &tid);
}


void uvload_fd()
/* Uvload_fd reads FD block only the necessery information is converted. */
{

   int   i;
   /* read fd block */
   readblk(fdblk.a);

   if(!strncmp(&fdblk.a[2], "FD", 2))                /* determine tape format */
     tform = 1;                                     /* is ASCII, so VMS tape */
   else
     tform = 3;                                    /* not ASCII, so IBM tape */

   /* conversion of some parameters           */
   cnvrth_c(&tform, &fdblk.a[0],  &fd.udbf,  &np);
   cnvrtf_c(&tform, &fdblk.a[4],  &fd.nfd,   &np);
   cnvrtf_c(&tform, &fdblk.a[12], &fd.nfde,  &np);
   cnvrth_c(&tform, &fdblk.a[18], &fd.stim,  &np);
   cnvrth_c(&tform, &fdblk.a[20], &fd.etim,  &np);
   cnvrth_c(&tform, &fdblk.a[22], &fd.empty, &np);
   cnvrth_c(&tform, &fdblk.a[24], &fd.fvers, &np);
   cnvrth_c(&tform, &fdblk.a[26], &fd.lrcrd, &np);
   cnvrth_c(&tform, &fdblk.a[32], &fd.olsys, &np);
   cnvrtf_c(&tform, &fdblk.a[40], &fd.noh,   &np);
   cnvrtf_c(&tform, &fdblk.a[44], &fd.moh,   &np);
   cnvrth_c(&tform, &fdblk.a[62], &fd.lsh,   &np);
   cnvrtf_c(&tform, &fdblk.a[64], &fd.nsh,   &np);
   cnvrtf_c(&tform, &fdblk.a[68], &fd.msh,   &np);
   cnvrtf_c(&tform, &fdblk.a[76], &fd.nih,   &np);
   cnvrtf_c(&tform, &fdblk.a[88], &fd.ndb,   &np);
   cnvrtf_c(&tform, &fdblk.a[92], &fd.mdb,   &np);
   cnvrtf_c(&tform, &fdblk.a[100], &fd.nbl,   &np);
   strncpy(fd.volume, &fdblk.a[104], 6);
   strncpy(fd.label,  &fdblk.a[110], 4);
   for(i=0;i < 384;i++)
     cnvrth_c(&tform, &fdblk.a[128+(i*2)], &fd.inftb[i], &np);

   if((fd.udbf != 32767) || strncmp( "FD", &fdblk.a[2], 2))    /* final check */
     error_c(FATAL, tofchar("FD not found"));

   if((fd.nfd != rnfd))                                 /* record number ok ? */
     error_c(FATAL, tofchar("Record organisation error on tape"));

   sprintf(text,"Tape version nr. is : %d",fd.fvers);
   anyout_c(ANYOUT_DEF, tofchar(text));

   sprintf(text,"Tape online system nr. is : %d",fd.olsys);
   anyout_c(ANYOUT_DEF, tofchar(text));

   sprintf(text,"Total number blocks in data: %d",fd.nbl);
   anyout_c(ANYOUT_TEST, tofchar(text));

   sprintf(text,"Total number of positions : %d",fd.moh);
   anyout_c(ANYOUT_TEST, tofchar(text));

   rnfd = fd.nfde;                            /* record number first fd block */
   rnoh = fd.noh;                             /* record number first oh block */
   rnsh = fd.nsh;                             /* record number first sh block */
   rnih = fd.nih;                             /* record number first ih block */
}


void uvload_oh()
/* Uvload_oh reads the two OH blocks */
{
   int i;


   readblk(ohblk.a);
   readblk(&ohblk.a[BLKLEN]);

   /* conversion of some parameters           */
   cnvrth_c(&tform, &ohblk.a[0],   &oh.udbf,   &np);
   cnvrtf_c(&tform, &ohblk.a[4],   &oh.noh,    &np);
   cnvrtf_c(&tform, &ohblk.a[12],  &oh.nohn,   &np);
   cnvrth_c(&tform, &ohblk.a[18],  &oh.stim,   &np);
   cnvrth_c(&tform, &ohblk.a[20],  &oh.etim,   &np);
   cnvrth_c(&tform, &ohblk.a[22],  &oh.empty,  &np);
   strncpy(oh.field, &ohblk.a[28], 12);
   cnvrth_c(&tform, &ohblk.a[44],  &oh.stuurc, &np);
   cnvrte_c(&tform, &ohblk.a[150], &oh.band,   &np);
   cnvrth_c(&tform, &ohblk.a[152], &oh.ntot,   &np);
   cnvrth_c(&tform, &ohblk.a[154], &oh.nrfeq,  &np);
   cnvrth_c(&tform, &ohblk.a[156], &oh.sfreq,  &np);
   cnvrth_c(&tform, &ohblk.a[158], &oh.nrpol,  &np);
   cnvrth_c(&tform, &ohblk.a[160], &oh.nrint,  &np);
   cnvrth_c(&tform, &ohblk.a[168], &oh.confnr, &np);
   strncpy(oh.be_code, &ohblk.a[172], 3);
   cnvrtd_c(&tform, &ohblk.a[176], &oh.ra0,    &np);
   cnvrtd_c(&tform, &ohblk.a[184], &oh.dec0,   &np);
   cnvrtd_c(&tform, &ohblk.a[192], &oh.freq,   &np);
   cnvrtd_c(&tform, &ohblk.a[200], &oh.hast,   &np);
   cnvrte_c(&tform, &ohblk.a[244], &oh.vlcty,  &np);
   cnvrth_c(&tform, &ohblk.a[248], &oh.velc,   &np);
   cnvrth_c(&tform, &ohblk.a[356], &oh.taper,  &np);
   cnvrth_c(&tform, &ohblk.a[370], &oh.mspat,  &np);
   cnvrth_c(&tform, &ohblk.a[372], &oh.mposn,  &np);
   cnvrth_c(&tform, &ohblk.a[666], &oh.nrsts,  &np);
   cnvrth_c(&tform, &ohblk.a[668], &oh.nrfrq,  &np);

   sprintf(text,"Total nbr. of freq. pts. : %d", oh.nrfeq);
   anyout_c(ANYOUT_TEST, tofchar(text));

   sprintf(text,"Nbr of polar. channels : %d", oh.nrpol);
   anyout_c(ANYOUT_TEST, tofchar(text));

   sprintf(text,"Nbr of interferometers : %d", oh.nrint);
   anyout_c(ANYOUT_TEST, tofchar(text));

   sprintf(text,"Be-Code is: %s", oh.be_code);
   anyout_c(ANYOUT_TEST, tofchar(text));

   sprintf(text,"tot number of sets %d",oh.nrsts);
   anyout_c(ANYOUT_TEST,tofchar(text));

   sprintf(text,"tot number of freqs %d",oh.nrfrq);
   anyout_c(ANYOUT_TEST,tofchar(text));

   sprintf(text,"Mos pattern %d",oh.mspat);
   anyout_c(ANYOUT_TEST,tofchar(text));

   if((oh.udbf != 32767) || strncmp("OH", &ohblk.a[2], 2))       /*final check*/
     error_c(FATAL, tofchar("OH not found"));

   if(oh.noh != rnoh)                                    /* record number ok? */
     error_c(FATAL, tofchar("Record organisation error on tape"));

   rnoh = oh.nohn;                            /* record number next oh block */

   if(first)
     had_not_all_diff_pols = oh.nrpol;

   if (ohtab != NULL) {                    /* remove allocation for set table */
     free( ohtab );
   }
   if((ohtab = malloc(oh.nrsts*sizeof(OHTAB))) == NULL)
     error_c(FATAL, tofchar("Cannot allocate memory for set table"));

   for(i=0;i < oh.nrsts;i++){                               /* fill set table */
     cnvrtf_c(&tform, &ohblk.a[672+(12*i)], &ohtab[i].bfreq,  &np);
     strncpy(ohtab[i].datyp, &ohblk.a[676+(12*i)], 2);
     cnvrth_c(&tform, &ohblk.a[678+(12*i)], &ohtab[i].bandnr, &np);
     cnvrtf_c(&tform, &ohblk.a[680+(12*i)], &ohtab[i].nsh,    &np);
   }
}


void uvload_sc()
/* Uvload_sc reads four SC blocks (at the moment read means skip). If
   tapeversion 6 and online system number >= 62 then SC blocks 5 and 6
   are also read and the table is converted.
*/
{
   int  i;


   sprintf(text,"The SC-BLOCK is read.");
   anyout_c(ANYOUT_TEST,tofchar(text));

   for(i=0;i < 4;i++) readblk(&scblk.a[i*BLKLEN]);
/* PAY ATTENTION: The condition (See tape format7 document SC-5/6 BLOCK)
   for which this should work is NOT CORRECT */
/*
   if((fd.fvers == 6) && (fd.olsys >= 60)){
     fmake(scblk5_6, 2*BLKLEN);
     for(i=0;i < 2;i++) readblk(&scblk5_6.a[i*BLKLEN]);
     if(sctab != NULL) free( sctab );       remove allocation for set table */
/*     if((sctab = (SCTAB *) malloc(fd.moh*sizeof(SCTAB))) == NULL)
       error_c(FATAL, tofchar("Cannot allocate memory for set table"));

     for(i=0;i < fd.moh;i++){ */                            /* fill set table */
/*       cnvrtf_c(&tform, &scblk5_6.a[0+(i*64)],  &sctab[i].ra0,   &np);
       cnvrtf_c(&tform, &scblk5_6.a[4+(i*64)],  &sctab[i].dec0,  &np);
       cnvrtf_c(&tform, &scblk5_6.a[8+(i*64)],  &sctab[i].ra1,   &np);
       cnvrtf_c(&tform, &scblk5_6.a[12+(i*64)], &sctab[i].dec1,  &np);
       cnvrtf_c(&tform, &scblk5_6.a[16+(i*64)], &sctab[i].sdec,  &np);
       cnvrtf_c(&tform, &scblk5_6.a[20+(i*64)], &sctab[i].cdec,  &np);
       cnvrtf_c(&tform, &scblk5_6.a[24+(i*64)], &sctab[i].vnpa,  &np);
       cnvrtf_c(&tform, &scblk5_6.a[28+(i*64)], &sctab[i].dvnpa, &np);
     }
   }
*/
}


void uvload_sh(fchar setname)
/* Uvload_sh reads the SH block.

   INPUT :

   setname   :  Name of GDS set to be created.
*/
{
   int    i;


   sprintf(text,"The SH-BLOCK is read");
   anyout_c(ANYOUT_TEST,tofchar(text));

   readblk(shblk.a);

   cnvrth_c(&tform, &shblk.a[0],   &sh.udbf,   &np);
   cnvrtf_c(&tform, &shblk.a[4],   &sh.nsh,    &np);
   cnvrtf_c(&tform, &shblk.a[8],   &sh.lsh,    &np);
   cnvrtf_c(&tform, &shblk.a[12],  &sh.mhlnk,  &np);
   cnvrth_c(&tform, &shblk.a[18],  &sh.stim,   &np);
   cnvrth_c(&tform, &shblk.a[20],  &sh.bandnr, &np);
   strncpy(sh.datyp, &shblk.a[30], 2);
   cnvrth_c(&tform, &shblk.a[32],  &sh.polc,   &np);
   cnvrth_c(&tform, &shblk.a[34],  &sh.nrint,  &np);
   cnvrtf_c(&tform, &shblk.a[36],  &sh.pts,    &np);
   cnvrth_c(&tform, &shblk.a[156], &sh.nent,   &np);
   cnvrth_c(&tform, &shblk.a[158], &sh.lent,   &np);
   cnvrtf_c(&tform, &shblk.a[180], &sh.nih,    &np);

   if((sh.udbf != 32767) || strncmp("SH", &shblk.a[2], 2))       /*final check*/
     error_c(FATAL, tofchar("SH not found"));
/*
   if(sh.nsh != rnsh) */                                 /* record number ok? */
/*     error_c(FATAL, tofchar("Record organisation error on tape"));
*/
   rnsh = sh.mhlnk;                            /* record number next sh block */

   if(strncmp("IF", &sh.datyp[0], 2) == 0) return;            /* skip IF data */


   if(had_not_all_diff_pols){
     if(shtab != NULL) free( shtab );    /* remove allocation for set table */
     if((shtab = malloc(sh.nent*sizeof(SHTAB))) == NULL)
       error_c(FATAL, tofchar("Cannot allocate memory for set table"));

     for(i=0;i < sh.nent;i++){                              /* fill set table */
       cnvrth_c(&tform, &shblk.a[160+(i*12)], &shtab[i].infnr, &np);
       cnvrth_c(&tform, &shblk.a[162+(i*12)], &shtab[i].wtel,  &np);
       cnvrth_c(&tform, &shblk.a[164+(i*12)], &shtab[i].otel,  &np);
       cnvrth_c(&tform, &shblk.a[166+(i*12)], &shtab[i].rbas,  &np);
       cnvrtf_c(&tform, &shblk.a[168+(i*12)], &ihnrs[i],       &np);
     }
   }
   else
     for(i=0;i < sh.nent;i++)
       cnvrtf_c(&tform, &shblk.a[168+(i*12)], &ihnrs[i], &np);
}


int getsmts(int nrdifts, int smts)
/* Getsmts finds from different sampletimes the Greatest Common Divisor and
   gives it back. This sampletime is used for scaling if the user doesn't
   in a sampletime.

   INPUT :

   nrdifts  :  Number of different sampletimes.
   smts     :  The smallest sampletime uptil now.
*/
{
   static int   i;
   int          remainder;
   int          OK = TRUE;
   int          samplt;

   /* Loop over number of different sampletimes, and check if smallest sample
      time uptil now is equal to Greatest Common Divisor */
   for(i=0;i<nrdifts;i++){
     remainder = sclinf[i].sampt % smts;
     if(remainder != 0){
       OK = FALSE;
       break;
     }
   }
   /* Smallest sampletime not equal Greatest Common Divisor, calculate GCD */
   if(!OK){
     do{
       smts -= 10;
       OK = TRUE;
       /* Loop over number of different sampletimes */
       for(i=0;i<nrdifts;i++){
         remainder = sclinf[i].sampt % smts;
         if(remainder != 0){
           OK = FALSE;
           break;
         }
       }
     } while(!OK);
   }
   samplt = smts;
   /* Return GCD */
   return(samplt);
}


void scale_datapnts_2_GCD(int samplt, int sclindex,
                          int intfindex)
/* Scale_datapnts_2_GCD will scale the datapoints to the Greatest Common
   Divisor if user didn't fill in a sampletime.

   INPUT :

   samplt    :  Sampletime where to scale to ( == Greatest Common Divisor).
   sclindex  :  Which scale information is to be used.
   intfindex :  For which interferometer.
*/
{
   static float *cbuf = NULL;
   static float *sbuf = NULL;
   static int   i, j, k;
   static int   quotient;
   int          offset;


   i = sclindex;
   quotient = sclinf[i].sampt / samplt;
   if(quotient > 1){
     /* Allocate memory for cos/sin buffer */
     cbuf = malloc(quotient*sclinf[i].nrdpnts*sizeof(float));
     sbuf = malloc(quotient*sclinf[i].nrdpnts*sizeof(float));
     if((cbuf == NULL) || (sbuf == NULL))
       error_c(FATAL, tofchar("Cannot allocate memory for scalebuffer"));
     offset = 0;
     /* Loop over all datapoints */
     for(j=0;j<sclinf[i].nrdpnts;j++){
       for(k=0;k<quotient;k++){
         cbuf[offset+k] = datapnts[intfindex].cos[j];
         sbuf[offset+k] = datapnts[intfindex].sin[j];
       }
       offset += quotient;
     }
     free(datapnts[intfindex].cos);
     free(datapnts[intfindex].sin);
     datapnts[intfindex].cos = cbuf;
     datapnts[intfindex].sin = sbuf;
   }
}


void scale_datapnts_2_usamplt(fint *usamplt, int sclindex, int intfindex)
/* Scale_datapnts_2_usamplt scales all datapoints to a sampletime given
   by the user.

   INPUT :

   usamplt   :  Sampletime given by the user.
   sclindex  :  Which scale information is to be used.
   intfindex :  For which interferometer.
*/
{
   static float *cbuf = NULL;
   static float *sbuf = NULL;
   static float fcos, fsin;
   static int   i, j, k, l;
   static int   quotient, nrpnts;


   i = sclindex;
   quotient = usamplt[0] / sclinf[i].sampt;
   if(quotient > 1){
     /* Allocate memory for cos/sin buffer */
     cbuf = malloc((sclinf[i].nrdpnts / quotient)*sizeof(float));
     sbuf = malloc((sclinf[i].nrdpnts / quotient)*sizeof(float));
     if((cbuf == NULL) || (sbuf == NULL))
       error_c(FATAL, tofchar("Cannot allocate memory for scalebuffer"));
     j = 0;
     l = 0;
     /* Loop over all datapoints */
     while(j<sclinf[i].nrdpnts){
       nrpnts = quotient;
       for(k=0;k<quotient;k++){
         if(datapnts[intfindex].cos[j] == blank ||
            datapnts[intfindex].sin[j] == blank   ){
           nrpnts -= 1;
           j += 1;
         }
         else{
           fcos += datapnts[intfindex].cos[j];
           fsin += datapnts[intfindex].sin[j++];
         }
       }
       /* This means that quotient*points are blank */
       if(nrpnts == 0){
         cbuf[l]  = blank;
         sbuf[l++]= blank;
       }
       else{
         cbuf[l]  = fcos / nrpnts;
         sbuf[l++]= fsin / nrpnts;
       }
     }
     free(datapnts[intfindex].cos);
     free(datapnts[intfindex].sin);
     datapnts[intfindex].cos = cbuf;
     datapnts[intfindex].sin = sbuf;
   }
}

void getusamplet(int samplt)
/* Getusamplet gets the sampletime the user wants to scale to.

   OUTPUT :

   samplt  :  Sampletime where all data has to be scaled to.
*/
{
   fint   dlev = 1;                             /* default level */
   fint   maxsamplt = 1;         /* maximum number of sampletimes */
   fchar  msg;
   char   msg1[MAXLEN];


   fmake(msg,  MAXLEN);
   sprintf(msg1, "Give sampletime [%d]", samplt);
   msg = tofchar(msg1);
   nusamplt = userint_c(usamplt, &maxsamplt, &dlev, INSAMPLE_KEY, msg);
}


void calc_ampl_phase(int nrdpnts, int intfm)
/* Calc_ampl_phase calculates from cos/sin the amplitude and the phase.

   INPUT :

   nrdpnts  :  Maximum number of datapoints (length of time axis).
   intfm    :  Datapoints from which interferometer.
*/
{
  static int      i;
  static float    *amplbuf  = NULL;
  static float    *phasebuf = NULL;
  static double   cos, sin;


  amplbuf = malloc(nrdpnts*sizeof(float));
  phasebuf = malloc(nrdpnts*sizeof(float));
  if((amplbuf == NULL) || (phasebuf == NULL))
    error_c(FATAL, tofchar("Cannot allocate memory for ampl_phasebuf"));
  /* Loop over all datapoints */
  for(i=0;i<nrdpnts;i++){
    cos = datapnts[intfm].cos[i];
    sin = datapnts[intfm].sin[i];
    if((cos == blank) || (sin == blank)){
      amplbuf[i] = blank;
      phasebuf[i] = blank;
    }
    /* Calculate amplitude and phase */
    else{
      amplbuf[i] = (float) sqrt((cos*cos)+(sin*sin));
      if(cos == 0)
        phasebuf[i] = 90;
      else
        phasebuf[i] = (float) atan((sin/cos));
    }
  }
  free(datapnts[intfm].cos);
  free(datapnts[intfm].sin);
  datapnts[intfm].ampl = NULL;
  datapnts[intfm].ampl = amplbuf;
  datapnts[intfm].fase = NULL;
  datapnts[intfm].fase = phasebuf;
}


void uvload_ih_db(fchar setname, int freq, int pol, int pos)
/* Uvload_ih_db reads the IH+DB blocks. Information from the IH header
   that is to be needed is converted.

   INPUT :

   setname  :  Name of set to be created.
   freq     :  Which frequency.
   pol      :  Which polarization.
   pos      :  If mosaicking which position.
*/
{
   static int     i, j;
   short          sin, cos;
   float          fsin, fcos;
   static char    *buffer;
   static int     offset;
   static int     nrihdb;
   static int     smts;
   static int     curst;
   static int     nrdifst;
   static int     sclindex;
   static int     maxdtpnts;


   /* If last group is reached than the calculation for the number of blocks
      that has to be read is different */
   if(sh.mhlnk == LAST_GRP){
     for(i=0;i < oh.nrpol;i++)
       if(sh.nent == ih.nentar[i])
         nrihdb = ih.lnrihdb[i];
     anyout_c(ANYOUT_TEST, tofchar("Last Group"));
   }
   else{
     /* Calculate the number of datablocks that has to be read */
     nrihdb = (((sh.mhlnk - sh.nsh)* fd.lrcrd)/BLKLEN) - 1;
     ih.nentar[pol] = sh.nent;
     ih.lnrihdb[pol] = nrihdb;
   }
   sprintf(text,"nrihdb : %d", nrihdb);
   anyout_c(ANYOUT_TEST,tofchar(text));

   buffer = malloc(nrihdb*BLKLEN*sizeof(char));
   if(buffer == NULL)
     error_c(FATAL, tofchar("Cannot allocate memory for buffer"));
   for(i=0;i < nrihdb;i++) readblk(&buffer[0+(i*3840)]);

   if(strncmp("IF", &sh.datyp[0], 2) == 0)                   /* skip IF data */
     return;

   /* Allocate memory for the datapoints */
   datapnts = malloc(sh.nent*sizeof(DATAPOINTS));
   if(datapnts == NULL)
     error_c(FATAL, tofchar("Cannot allocate memory for datapnts"));

   /* Allocate memory for the scale information */
   sclinf = malloc(SCLINFLEN*sizeof(SCALEDATA));
   if(sclinf == NULL)
     error_c(FATAL, tofchar("Cannot allocate memory for scaledata"));

   offset = 0;
   smts = 1000;
   curst = 0;
   nrdifst = 0;

   cnvrtf_c(&tform, &buffer[4], &rnih, &np);
   /* Loop over the number of entries. The number of entries equals the
      number of sets. A set equals an IH+n*DB block, but these IH+DB blocks
      are already read in a buffer */
   for(i=0;i < sh.nent;i++){
     if(strncmp( "IH", &buffer[2+offset], 2) != 0)
       error_c(FATAL, tofchar("IH not found"));

     cnvrth_c(&tform, &buffer[0+offset],   &ih.udbf,   &np);
     cnvrtf_c(&tform, &buffer[4+offset],   &ih.nih,    &np);
     cnvrtf_c(&tform, &buffer[8+offset],   &ih.lih,    &np);
     cnvrtf_c(&tform, &buffer[12+offset],  &ih.ihlnk,  &np);
     cnvrth_c(&tform, &buffer[18+offset],  &ih.stim,   &np);
     cnvrth_c(&tform, &buffer[20+offset],  &ih.bandnr, &np);
     cnvrth_c(&tform, &buffer[22+offset],  &ih.infnr,  &np);
     cnvrth_c(&tform, &buffer[24+offset],  &ih.wtel,   &np);
     cnvrth_c(&tform, &buffer[26+offset],  &ih.otel,   &np);
     cnvrtf_c(&tform, &buffer[28+offset],  &ih.bfreq,  &np);
     cnvrte_c(&tform, &buffer[32+offset],  &ih.drt,    &np);
     cnvrte_c(&tform, &buffer[36+offset],  &ih.hab,    &np);
     cnvrte_c(&tform, &buffer[40+offset],  &ih.hae,    &np);
     cnvrte_c(&tform, &buffer[44+offset],  &ih.dha,    &np);
     cnvrth_c(&tform, &buffer[48+offset],  &ih.ld,     &np);
     cnvrth_c(&tform, &buffer[50+offset],  &ih.ndatp,  &np);
     cnvrth_c(&tform, &buffer[52+offset],  &ih.ndelp,  &np);
     cnvrth_c(&tform, &buffer[54+offset],  &ih.nexp,   &np);
     cnvrth_c(&tform, &buffer[56+offset],  &ih.fscal,  &np);
     cnvrth_c(&tform, &buffer[58+offset],  &ih.offs,   &np);
     cnvrte_c(&tform, &buffer[64+offset],  &ih.acos,   &np);
     cnvrte_c(&tform, &buffer[76+offset],  &ih.asin,   &np);
     cnvrth_c(&tform, &buffer[108+offset], &ih.inct,   &np);
     cnvrth_c(&tform, &buffer[110+offset], &ih.dwelt,  &np);
     cnvrtf_c(&tform, &buffer[112+offset], &ih.volgnr, &np);
     cnvrte_c(&tform, &buffer[116+offset], &ih.intt,   &np);
     cnvrth_c(&tform, &buffer[120+offset], &ih.dradt,  &np);

     if(ih.nih != ihnrs[i])                          /* record number ok? */
       error_c(FATAL, tofchar("Record organisation error on tape at IH"));

     /* Is the smallest sampletime uptil now bigger than the new found sample-
        time, than smallest sampletime equals new found sampletime. Add a new
        record to the scale information */
     if(ih.inct < smts){
       smts = ih.inct;
       sclinf[++nrdifst-1].nrdpnts = ih.ndatp;
       sclinf[nrdifst-1].sampt = ih.inct;
       sclinf[nrdifst-1].index = i;
     }
     else
       /* If found a different sampletime than add an new record to the scale
          information */
       if((ih.inct != smts) && (ih.inct != curst)){
         sclinf[++nrdifst-1].nrdpnts = ih.ndatp;
         sclinf[nrdifst-1].sampt = ih.inct;
         sclinf[nrdifst-1].index = i;
         curst = ih.inct;
       }
     datapnts[i].cos = malloc(ih.ndatp*sizeof(float));
     datapnts[i].sin = malloc(ih.ndatp*sizeof(float));
     if((datapnts[i].cos == NULL) || (datapnts[i].sin == NULL))
       error_c(FATAL, tofchar("Cannot allocate memory for cos/sinbuffer"));

     /* load data points */
     for(j=0;j<ih.ndatp;j++){
       cnvrth_c(&tform, &buffer[128+offset+(j*4)], &cos, &np);
       cnvrth_c(&tform, &buffer[130+offset+(j*4)], &sin, &np);
       fcos = (float)cos;
       fsin = (float)sin;
       if(fcos == EMPTY) fcos = blank;
       if(fsin == EMPTY) fsin = blank;
       datapnts[i].cos[j] = fcos;
       datapnts[i].sin[j] = fsin;
     }
     rnih = ih.ihlnk;
     offset += ((ih.ld+1)*fd.lrcrd);
   }
   sclinf[nrdifst].nrdpnts = 0;
   sclinf[nrdifst].sampt = 0;
   sclinf[nrdifst].index = sh.nent;

   /* Descriptor file has to be made and only once, and the first time this
      procedure called there is all th einformation needed */
   if(first){
     sprintf(text,"Sampletime(s) found is (are) : ");
     anyout_c(ANYOUT_DEF,tofchar(text));
     for(i=0;i < nrdifst;i++){
       sprintf(text,"Sampletime %d is : %d seconds", i+1, sclinf[i].sampt);
       anyout_c(ANYOUT_DEF,tofchar(text));
     }
     samplt = getsmts(nrdifst, smts);
     maxdtpnts = smts/samplt*sclinf[0].nrdpnts;
     getusamplet(samplt);  /* get sampletime from user */
     if(nusamplt){
       anyout_c(ANYOUT_TEST,tofchar("NOT SCALE_2_GCD"));
       scale_2_gcd = FALSE;
       samplt = usamplt[0];
       maxdtpnts = sclinf[0].nrdpnts/(samplt/sclinf[0].sampt);
       sprintf(text,"maxdtpnts : %d", maxdtpnts);
       anyout_c(ANYOUT_TEST,tofchar(text));
     }
     /* Create descriptor file */
     create_descr(setname, maxdtpnts, samplt, nrdifst);
     sprintf(text,"Data is scaled to a sampletime of %d seconds", samplt);
     anyout_c(ANYOUT_DEF,tofchar(text));
     first = FALSE;
   }

   if(had_not_all_diff_pols)
     write_info_at_intfm(setname);

   if(scale_2_gcd){   /* Greatest Common Divisor */
     /* Loop over number of different sampletimes */
     for(sclindex=0;sclindex<nrdifst;sclindex++){
       /* Loop over number of interferometers found with this sampletime */
       for(j=sclinf[sclindex].index;j<sclinf[sclindex+1].index;j++){
         /* Scale datapoints to GCD */
         scale_datapnts_2_GCD(samplt, sclindex, j);
         if(amphas){
           /* Calculate the amplitude and the phase */
           calc_ampl_phase(maxdtpnts, j);
           /* Write data to the image file */
           write_2_image(setname, j, maxdtpnts, freq, pol, pos);
           free(datapnts[j].ampl);
           free(datapnts[j].fase);
         }
         else{
           /* Write data to the image file */
           write_2_image(setname, j, maxdtpnts, freq, pol, pos);
           free(datapnts[j].cos);
           free(datapnts[j].sin);
         }
       }
     }
   }
   else{   /* Scale datapoints to sampletime of user */
     /* Loop over number of different sampletimes */
     for(sclindex=0;sclindex<nrdifst;sclindex++){
       /* Loop over number of interferometers found with this sampletime */
       for(j=sclinf[sclindex].index;j<sclinf[sclindex+1].index;j++){
         /* Scale datapoints to sampletime given by user */
         scale_datapnts_2_usamplt(usamplt, sclindex, j);
         if(amphas){
           /* Calculate the amplitude and the phase */
           calc_ampl_phase(maxdtpnts, j);
           /* Write data to the image file */
           write_2_image(setname, j, maxdtpnts, freq, pol, pos);
           free(datapnts[j].ampl);
           free(datapnts[j].fase);
         }
         else{
           /* Write data to the image file */
           write_2_image(setname, j, maxdtpnts, freq, pol, pos);
           free(datapnts[j].cos);
           free(datapnts[j].sin);
         }
       }
     }
   }
   free(datapnts);
   free(buffer);
   free(sclinf);
}


void tape_ini()
/* Tape_ini opens the tape unit, sets it at the beginning of the tape,
   reads the label and tells the user which volume is mounted. */
{
   fchar  label;
   char   volume[VOLLEN];
   fint   reclen = 240;
   fint   nread;
   int    stat;
   fint   taperr;
   fint   nf=1;
   int    i;


   fmake(label, LABLEN);
   unit = mtopen_c(INTAPE_DEV);
   if(unit < 0)                                           /* open tape unit */
     error_c(FATAL, MNTERR);

   stat = mtstat_c(&unit);
   if(!(stat & 1)) taperr = mtrew_c(&unit);            /* not at BOT, rewind */
   nread = mtread_c(&unit, label.a, &reclen);
   /* Check if whole label has been read (240 bytes) */
   if(nread < reclen) {
     for(i=1;i < (reclen/nread);i++)
       mtread_c(&unit, &label.a[i*nread], &reclen);
   }
   else if(nread < 0) readerror(nread);


   if(strncmp(label.a, "VOL1", 4) != 0)
     error_c(FATAL, tofchar("Not a redundancy tape"));

   strncpy(volume, &label.a[4], 6);                        /* copy tape label */
	volume[6] = '\0';
   sprintf(text, "Mounted Volume is: %s ", volume);                /* message */
   anyout_c(ANYOUT_DEF, tofchar(text));
   if((taperr = mtfsf_c(&unit, &nf)) != nf)
     error_c(FATAL, tofchar("Cannot skip tape file forward"));
}


void uvload_ini(fchar setname)
/* Uvload_ini initializes the blocks and asks the user for the setname

   OUTPUT :

   setname  :  Name of the set to be created.
 */
{
   fint   nitems, dfault, gerror = 0;
   bool   del;
   fint   result;
   fchar  msg;
   char   msg1[MAXLEN];
   fint   axis = 2;


   setfblank_c(&blank);
   fmake(buf,BLKLEN);
   fmake(fdblk, BLKLEN);
   fmake(ohblk, 2*BLKLEN);
   fmake(scblk, 4*BLKLEN);
   fmake(shblk, BLKLEN);

   do{  /* get name of set to create */
     nitems = 1;
     dfault = 0; /* No default */
     result = userchar_c(setname, &nitems, &dfault,
                         tofchar("OUTSET="),
                         tofchar("Set to create ?"));


     /* Does it exist already? */

     if(gds_exist_c(setname, &gerror)){
       anyout_c(ANYOUT_DEF, tofchar("Set already exists!"));
       nitems = 1, dfault = 1; del = toflog(0);
       result = userlog_c(&del, &nitems, &dfault,
                          tofchar("DELETE="),
                          tofchar("Delete this set ? [NO]"));
       del = tobool(del);
       if(del){
         gds_delete_c(setname, &gerror);
         break;
       }
       else{
         cancel_c(tofchar("OUTSET="));
         cancel_c(tofchar("DELETE="));
       }
     }
     else
       break;
   } while (1);

   /* Get choice between Complex and Amplitude/Phase axis from user */
   nitems = 1;
   dfault = 1; /* Default is Amplitude/Phase axis */
   sprintf(msg1, "1: Complex axis  2: Amplitude/Phase axis  [%d]", 2);
   fmake(msg, MAXLEN);
   msg = tofchar(msg1);
   result = userint_c(&axis, &nitems, &dfault, tofchar("AXIS="), msg);

   switch(axis){
     case 1:  amphas = FALSE; break;
     case 2:  amphas = TRUE;  break;
     default: amphas = TRUE;
   }
}


void create_uvset(fchar setname)
/* Create_uvset is the main loop for the program it is actually the body.

   INPUT :

   setname  :  Name of set to be created.
*/
{
   static int   i, j, k;
   fint  gerror = 0;


   gds_create_c(setname, &gerror); /* Create set. */
   /* Is it possible to create this set, i.e. is gerror >= 0 ? */
   if(gerror < 0)
     error_c(FATAL, tofchar("Error while creating set"));

   uvload_fd();
   if(fd.fvers == 6){                            /* Tape format 6, see manual */
     uvload_oh();
     uvload_sc();
     /* Is it a LINE observation ? */
     if((strncmp(oh.be_code, "DLB", 3) == 0) ||
        (strncmp(oh.be_code, "DXB", 3) == 0)   ){
       uvload_sh(setname);
       uvload_ih_db(setname, 0, 0, 0);
       /* Loop over number of frequencies */
       for(i=0;i < oh.nrfrq;i++){
         /* Loop over number of polarizations */
         for(j=0;j < oh.nrpol;j++){
           sprintf(text,"FREQ : %d POL : %d",i,j);
           anyout_c(ANYOUT_TEST,tofchar(text));
           uvload_sh(setname);
           uvload_ih_db(setname, i, j, 0);
         }
       }
     }
     /* Or is it a CONTINUUM observation ? */
     else{ /* DCB DATASET */
       /* Loop over number of frequencies */
       for(i=0;i < oh.nrfrq;i++){
         uvload_sh(setname);
         uvload_ih_db(setname, 0, 0, 0);
         /* Loop over number of polarizations */
         for(j=0;j < oh.nrpol;j++){
           sprintf(text,"FREQ : %d POL : %d",i,j);
           anyout_c(ANYOUT_TEST,tofchar(text));
           uvload_sh(setname);
           uvload_ih_db(setname, i, j, 0);
         }
       }
     }
     uvload_fd();
   }
   else{
     /* Tape format 7, see manual for description */
     if((fd.fvers == 7) && (fd.olsys >= 62)){
       /* Loop over all positions */
       for(i=0;i < fd.moh;i++){
         uvload_oh();
         uvload_sc();
         /* Is it a LINE observation ? */
         if((strncmp(oh.be_code, "DLB", 3) == 0) ||
            (strncmp(oh.be_code, "DXB", 3) == 0)   ){
           uvload_sh(setname);
           uvload_ih_db(setname, 0, 0, 0);
           /* Loop over number of frequencies */
           for(j=0;j < oh.nrfrq;j++){
             /* Loop over number of polarizations */
             for(k=0;k < oh.nrpol;k++){
               sprintf(text,"POS : %d FREQ : %d POL : %d",i,j,k);
               anyout_c(ANYOUT_TEST,tofchar(text));
               uvload_sh(setname);
               uvload_ih_db(setname, j, k, i);
             }
           }
         }
         else{ /* DCB DATASET */
           /* Loop over number of frequencies */
           for(j=0;j < oh.nrfrq;j++){
             uvload_sh(setname);
             uvload_ih_db(setname, 0, 0, 0);
             /* Loop over number of polarizations */
             for(k=0;k < oh.nrpol;k++){
               sprintf(text,"POS : %d FREQ : %d POL : %d",i,j,k);
               anyout_c(ANYOUT_TEST,tofchar(text));
               uvload_sh(setname);
               uvload_ih_db(setname, j, k, i);
             }
           }
         }
       }
     }
   }
}

MAIN_PROGRAM_ENTRY
{
   static fchar	setname;


   init_c();                                      /* Get in touch with HERMES */
   /* Task identification */
   {
      static fchar    Ftask;            /* Name of current task */
      fmake( Ftask, 20 );               /* Macro 'fmake' must be available */
      myname_c( Ftask );                /* Get task name */
      Ftask.a[nelc_c(Ftask)] = '\0';    /* Terminate task name with null char */
      IDENTIFICATION( Ftask.a, VERSION );  /* Show task and version */
   }

   tape_ini();                                    /* Mount tape               */
   fmake(setname, MAXLEN);
   uvload_ini(setname);                           /* Initialize               */

   create_uvset(setname);                         /* Create dataset           */

   finis_c();                                     /* Quit link with HERMES    */
   return( EXIT_SUCCESS );
}
