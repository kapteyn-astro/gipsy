/* skytool.c -XT
                              COPYRIGHT (c) 1997
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

Author: J.P. Terlouw         


#> skytool.dc1
Program:       SKYTOOL

Purpose:       Interactive coordinate transformation utility.

Category:      COORDINATES, UTILITY

File:          skytool.c

Author:        J.P. Terlouw

Keywords:

   **EPOCH1=   Epoch of the first equatorial and ecliptic coordinates.  [1950.0]
   **EPOCH2=   Epoch of the second equatorial and ecliptic coordinates. [1983.5]

               Though there are many other keywords, only the above keywords
               should be set directly from the command line. All others are
               set by typing into input fields, pressing buttons or selecting
               menu entries in the X-window, or are set internally by the
               program itself. 

#begin section description

Description:   SKYTOOL is an interactive coordinate transformation utility
               based on the GIPSY graphical interface routines (Ggi).
               It is intended to be self-explanatory.

               When it is started, an X-window appears with input/output
               fields for the following values:
                 - Epoch 1
                 - RA and Dec for the first epoch.
                 - Ecliptic l and b for the first epoch.
                 - Epoch 2
                 - RA and Dec for the second epoch.
                 - Ecliptic l and b for the second epoch.
                 - Galactic l and b.
                 - Supergalactic l and b.
               These fields represent only one and the same position.
               If any of the fields is changed, the other fields are
               changed accordingly.
               For every coordinate field there are two versions:
               one in degrees and one in hms,dms or dms,dms
               
               When the "LOG" button is activated, all transactions are
               written to the GIPSY log file. When an output file is
               specified (via the "FILE" menu), the resulting coordinates
               in degrees are written to this file. The output sky system
               can be specified with the "Output Sky System" menu.
               Where applicable, Epoch 2 is used for output coordinates.
               Writing to the file can be stopped by either pressing
               "CANCEL" in the file specification window, or by specifying
               an other filename. In the latter case writing will then
               continue to the newly specified file.

               When an input file is specified, the "GO" button will become
               available. Pressing this button causes the input file to
               be processed by the task SKYTRANS. The input sky system for
               this option can be specified with the "Input Sky System" menu.
               Where applicable, Epoch 1 is used for input coordinates.
               When an output file is specified, this file will be
               _overwritten_ by the results from SKYTRANS.
               Please note that SKYTOOL will be unresponsive until SKYTRANS
               is finished. Any communication due to errors in the input
               file will be done via Hermes.

#end section description

Updates:       Jul 25, 1997: JPT, Document created.
               Aug  1, 1997: JPT, Changed layout.
               Aug  8, 1997: JPT, "Show Document" button added.
               Jan  5, 1998: JPT, Upgraded to latest Ggi version
               Apr 21, 1998: JPT, Version 1.0
               Feb 11, 2000: JPT, Increased precision of seconds output.
               Mar  8, 2000: JPT, Still more precision of declination seconds.
               Jun 28, 2000: JPT, Redesigned GUI.
#<

*/

#define RELEASE "2.0"
#define DEFAULT_EQU_EPOCH 1950.0
#define DEFAULT_ECL_EPOCH 1983.5

#define STANDARD_EPOCH 1950.0

#define LABELWIDTH 120
#define PANELWIDTH 615

#include "stddef.h"
#include <sys/file.h>
#ifndef O_RDONLY
#include <fcntl.h>
#endif
#ifdef  __sysv__
#include <unistd.h>
#endif
#include "stdlib.h"
#include "stdio.h"
#include "math.h"
#include "gipsyc.h"
#include "init.h"
#include "userfio.h"
#include "finis.h"  
#include "cmain.h"
#include "epoco.h"
#include "eclipco.h"
#include "skyco.h"
#include "subst.h"
#include "deputy.h"
#include "keyevents.h"
#include "ggi.h"


#define New(type) ((type *)calloc(1,sizeof(type)))
#define NNew(n,type) ((type *)calloc(n,sizeof(type)))
#define Delete(x)  {free(x); x=NULL;}
#define MINSIZE(x,y) GgiSetPosition(GgiCanvas("canvas",1,1), x, NULL, y, NULL)

/*
 *  Coordinate representations
 */
typedef enum {DEGREES, HMSDMS, DMSDMS} coordrepr;
/*
 *  Sky systems
 */
typedef enum {EQUATORIAL, GALACTIC, SUPERGALACTIC, ECLIPTIC} skysys;

/*
 *  position structure
 */
typedef struct Position {   
   double x;
   double y;
} Position;

/*
 *  conversion to 'standard' position, e.g. RA, Dec (1950.0)
 */
typedef Position (*asStd)(/* struct _Coordinate* */);

/*
 *  conversion to 'own' position
 */
typedef void (*fromStd)(/* struct _Coordinate*, Position */);

/*
 *  update keyword
 */
typedef void (*updproc)(/* struct _Coordinate* */);

/*
 *  'general' coordinate structure
 */
typedef struct _Coordinate {
   struct _Coordinate *next;  /* next object in linked list */
   skysys      system;        /* sky system */
   double      epoch;         /* epoch (not relevant for all systems) */
   Position    pos;           /* current position */
   bool        expect_change; /* expect keywrod change due to update action */
   char        *key;          /* unique user input keyword for coordinate */
   char        *epoch_key;    /* user input keyword for epoch */
   ident       changepos;     /* position change registration */
   ident       changeepoch;   /* epoch change registration */
   ident       textfield;     /* Ggi input text field */
   asStd       tostd;         /* converter function to 'standard' position */
   fromStd     setpos;        /* function to change coordinate position */
   updproc     update;        /* function to update keyword */
} _Coordinate, *Coordinate;


static fint fint1=1, fint2=2, fint3=3, fint4=4;
static double stdepoch=STANDARD_EPOCH;
static ident  browser, gobut, errlab, statlab;

char *filelabels[]={
   "Batch input file",
   "Output file",
   "_LINE",
   "Exit",
   NULL
};
typedef enum {
   FILE_INPUT,
   FILE_OUTPUT,
   FILE_LINE,
   FILE_EXIT
} filechoices;

/*
 *  Coordinate input- and output routines.
 *  One for each coordinate representation.
 */
static void ReadDegrees(ident id, char *key, int mask, void *arg);
static void ReadHmsDms(ident id, char *key, int mask, void *arg);
static void ReadDmsDms(ident id, char *key, int mask, void *arg);
static KeyeventProc readprocs[]={ReadDegrees, ReadHmsDms, ReadDmsDms};

static void WriteDegrees(Coordinate coord);
static void WriteHmsDms(Coordinate coord);
static void WriteDmsDms(Coordinate coord);
static updproc writeprocs[]={WriteDegrees, WriteHmsDms, WriteDmsDms};

static Coordinate coordlist=NULL; /* linked list of all coordinate objects */
static FILE *outptr=NULL;         /* output file */


/* -------------------------------------------------------------------------- */
/*                                 StrDup                                     */
/* -------------------------------------------------------------------------- */
/*  String duplication function.
 */
static char *StrDup(char *text)
{
   char *result=NULL;

   if (text) {
      result = NNew(strlen(text)+1,char);
      strcpy(result,text);
   }
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 changed                                    */
/* -------------------------------------------------------------------------- */
/*  changed() broadcasts the most recently changed position (in standard form)
 *  to all coordinate objects.
 *  Then, if the batch/log utility is active, the coordinates indicated by
 *  the INSYS and OUTSYS menus are written to the log file.
 */
static void changed(Coordinate coord)
{
   static char *outkeys[]={"EQU2=","GAL=","SUP=","ECL2="};
   static char *sysnames[]={"RA,Dec","Gal.l,b","Supergal.l,b","Ecl.l,b"};
   bool logging=FALSE;
   
   Coordinate current;
   Position   stdpos=coord->tostd(coord);
    
/*     Broadcasting phase:
 */
   for (current=coordlist; current; current=current->next) {
      if (current != coord) current->setpos(current, stdpos);
   }

/*     Logging phase:
 */
   (void)userflog(&logging, 1, 2, "LOG=", " ");
   if (outptr || logging) {
      char iname[20], oname[20];
      fint   isys, osys;
      double inpos[2], outpos[2];
      double epoch1, epoch2;
     
      isys = (int)coord->system;
      (void)userfint(&osys, 1, 4, "OUTSYS=", " ");
      inpos[0] = coord->pos.x;
      inpos[1] = coord->pos.y;
      epoch1   = coord->epoch;
      (void)userfdble(outpos, 2, 4, outkeys[osys], " ");
      (void)userfdble(&epoch2, 1, 4, "EPOCH2=", " ");
      if (isys==0 || isys==3) {
         sprintf(iname, "%s(%6.1f)", sysnames[isys], epoch1);
      } else {
         strcpy(iname, sysnames[isys]);
      }
      if (osys==0 || osys==3) {
         sprintf(oname, "%s(%6.1f)", sysnames[osys], epoch2);
      } else {
         strcpy(oname, sysnames[osys]);
      }
      if (logging) anyoutf(0, "%s %f, %f ==> %s %f, %f",
                      iname, inpos[0], inpos[1], oname, outpos[0], outpos[1]);
      if (outptr) fprintf(outptr,"%f %f\n", outpos[0], outpos[1]);
   }
}

/* -------------------------------------------------------------------------- */
/*                                 ReadDegrees                                */
/* -------------------------------------------------------------------------- */
/*  Key event procedure. It reads a position in degrees and stores it in
 *  the associated coordinate structure. If the change is expected, i.e.,
 *  it is caused by a wkeyf() call from the program, then it returns
 *  immediately. If it is not expected, i.e., caused by the user,
 *  it calls changed() which broadcasts the changed position.
 */
static void ReadDegrees(ident id, char *key, int mask, void *arg)
{
   Coordinate coord=(Coordinate)arg;
   double values[2];   

   if (userfdble(values, 2, 4, key, "Position")) {
      coord->pos.x = values[0];
      coord->pos.y = values[1];
      if (coord->expect_change) {
      coord->expect_change = FALSE;
      } else {
         changed(coord);
      }
   }
}

/* -------------------------------------------------------------------------- */
/*                                 ReadHmsDms                                 */
/* -------------------------------------------------------------------------- */
/*  Key event procedure. It reads a position in hours, minutes, seconds,
 *  degrees, minutes, seconds and stores it in
 *  the associated coordinate structure. If the change is expected, i.e.,
 *  it is caused by a wkeyf() call from the program, then it returns.
 *  If it is not expected, i.e., caused by the user, it calls changed()
 *  which broadcasts the changed position.
 */
static void ReadHmsDms(ident id, char *key, int mask, void *arg)
{
   Coordinate coord=(Coordinate)arg;
   double values[6], negx, negy;

   if (!userfdble(values, 6, 4, key, "Position"))
      return;
   negx = values[0]<0?-1.0:1.0;
   negy = values[3]<0?-1.0:1.0;

   coord->pos.x = 15.0*values[0] + negx*values[1]/4.0 + negx*values[2]/240.0;
   coord->pos.y = values[3]      + negy*values[4]/60  + negy*values[5]/3600;

   if (coord->expect_change) {
      coord->expect_change = FALSE;
   } else {
      changed(coord);
   }
}

/* -------------------------------------------------------------------------- */
/*                                 ReadDmsDms                                 */
/* -------------------------------------------------------------------------- */
/*  Key event procedure. It reads a position in degrees, minutes,
 *  seconds, degrees, minutes, seconds and stores it in
 *  the associated coordinate structure. If the change is expected, i.e.,
 *  it is caused by a wkeyf() call from the program, then it returns.
 *  If it is not expected, i.e., caused by the user, it calls changed()
 *  which broadcasts the changed position.
 */
static void ReadDmsDms(ident id, char *key, int mask, void *arg)
{
   Coordinate coord=(Coordinate)arg;
   double values[6], negx, negy;

   if (!userfdble(values, 6, 4, key, "Position"))
      return;
   negx = values[0]<0?-1.0:1.0;
   negy = values[3]<0?-1.0:1.0;

   coord->pos.x = values[0] + negx*values[1]/60.0 + negx*values[2]/3600.0;
   coord->pos.y = values[3] + negy*values[4]/60.0 + negy*values[5]/3600.0;

   if (coord->expect_change) {
      coord->expect_change = FALSE;
   } else {
      changed(coord);
   }
}

/* -------------------------------------------------------------------------- */
/*                                 epoch_equ                                  */
/* -------------------------------------------------------------------------- */
/*  Key event procedure. It reads an epoch and applies it to the associated
 *  equatorial coordinate.
 */
static void epoch_equ(ident id, char *key, int mask, void *arg)
{
   Coordinate coord=(Coordinate)arg;
   double     epoch;
   char       label[80];
    
   if (userfdble(&epoch, 1, 4, key, "Epoch")) {
      epoco_c(&coord->pos.x, &coord->pos.y, &coord->epoch,
              &coord->pos.x, &coord->pos.y, &epoch);
      coord->epoch = epoch;
      coord->update(coord);
      sprintf(label,"RA, Dec (%4.1f)", epoch);
      GgiSetLabel(coord->textfield, label, LABELWIDTH);
   }
}

/* -------------------------------------------------------------------------- */
/*                                 epoch_ecl                                  */
/* -------------------------------------------------------------------------- */
/*  Key event procedure. It reads an epoch and applies it to the associated
 *  ecliptic coordinate.
 */
static void epoch_ecl(ident id, char *key, int mask, void *arg)
{
   Coordinate coord=(Coordinate)arg;
   double     epoch;
   char       label[80];
    
   if (userfdble(&epoch, 1, 4, key, "Epoch")) {
      eclipco_c(&coord->pos.x, &coord->pos.y, &coord->epoch,
                &coord->pos.x, &coord->pos.y, &epoch);
      coord->epoch = epoch;
      coord->update(coord);
      sprintf(label,"Ecl. L, B (%4.1f)", epoch);
      GgiSetLabel(coord->textfield, label, LABELWIDTH);
   }
}

/* -------------------------------------------------------------------------- */
/*                                 WriteDegrees                               */
/* -------------------------------------------------------------------------- */
/*  WriteDegrees() writes a coordinate position in degrees to the 
 *  associated user input keyword.
 */
static void WriteDegrees(Coordinate coord)
{
   wkeyf("%s%f %f", coord->key, coord->pos.x, coord->pos.y);
   coord->expect_change = TRUE;
}

/* -------------------------------------------------------------------------- */
/*                                 WriteHmsDms                                */
/* -------------------------------------------------------------------------- */
/*  WriteHmsDms() writes a coordinate position in HMS,DMS to the 
 *  associated user input keyword.
 */
static void WriteHmsDms(Coordinate coord)
{
   double x=coord->pos.x, y=coord->pos.y;
   char negx, negy;
   int ixh, ixm, iyd, iym;
   double xh, xm, ym;
   double xs, ys;
    
   negx = x<0?'-':' ';   x = fabs(x);
   negy = y<0?'-':' ';   y = fabs(y);

   xh  = x/15.0;
   ixh = (int)xh;
   xm  = xh*60.0 - ((double)ixh)*60.0;
   ixm = (int)xm;
   xs  = ((double)xm)*60.0 - ((double)ixm)*60.0;
   xs  = (double)((int)(xs*1000.0))/1000.0;
   
   iyd = (int)y;
   ym  = y*60.0 - ((double)iyd)*60.0;
   iym = (int)ym;
   ys  = ((double)ym)*60.0 - ((double)iym)*60.0;
   ys  = (double)((int)(ys*1000.0))/1000.0;
    
   coord->expect_change = TRUE;
   wkeyf("%s%c%d %d %.3f %c%d %d %.3f", coord->key,
           negx, ixh, ixm, xs, negy, iyd, iym, ys);
}

/* -------------------------------------------------------------------------- */
/*                                 WriteDmsDms                                */
/* -------------------------------------------------------------------------- */
/*  WriteDmsDms() writes a coordinate position in DMS,DMS to the 
 *  associated user input keyword.
 */
static void WriteDmsDms(Coordinate coord)
{
   double x=coord->pos.x, y=coord->pos.y;
   char negx, negy;
   int ixd, ixm, iyd, iym;
   double xm, ym;
   double xs, ys;
    
   negx = x<0?'-':' ';   x = fabs(x);
   negy = y<0?'-':' ';   y = fabs(y);

   ixd = (int)x;
   xm  = x*60.0 - ((double)ixd)*60.0;
   ixm = (int)xm;
   xs  = ((double)xm)*60.0 - ((double)ixm)*60.0;
   xs  = (double)((int)(xs*100.0))/100.0;
   
   iyd = (int)y;
   ym  = y*60.0 - ((double)iyd)*60.0;
   iym = (int)ym;
   ys  = ((double)ym)*60.0 - ((double)iym)*60.0;
   ys  = (double)((int)(ys*100.0))/100.0;
    
   coord->expect_change = TRUE;
   wkeyf("%s%c%d %d %.2f %c%d %d %.2f", coord->key,
          negx, ixd, ixm, xs, negy, iyd, iym, ys);
}

/* -------------------------------------------------------------------------- */
/*                                 Equ2Std                                    */
/* -------------------------------------------------------------------------- */
/*  Equ2Std() returns a position in 'standard' form given an equatorial
 *  coordinate.
 */
static Position Equ2Std(Coordinate coord)
{
   Position result;
   
   epoco_c(&coord->pos.x, &coord->pos.y, &coord->epoch,
           &result.x, &result.y, &stdepoch);
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 Std2Equ                                    */
/* -------------------------------------------------------------------------- */
/*  Std2Ecl() applies a coordinate in 'standard' form to
 *  an equatorial coordinate.
 */
static void Std2Equ(Coordinate coord, Position pos)
{
   epoco_c(&pos.x, &pos.y, &stdepoch,
           &coord->pos.x, &coord->pos.y, &coord->epoch);
   coord->update(coord);
}

/* -------------------------------------------------------------------------- */
/*                                 Ecl2Std                                    */
/* -------------------------------------------------------------------------- */
/*  Ecl2Std() returns a position in 'standard' form given an ecliptic
 *  coordinate.
 */
static Position Ecl2Std(Coordinate coord)
{
   Position result;
   
   eclipco_c(&coord->pos.x, &coord->pos.y, &coord->epoch,
             &result.x, &result.y, &stdepoch);
   skyco_c(&result.x, &result.y, &fint3, &result.x, &result.y, &fint1);
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 Std2Ecl                                    */
/* -------------------------------------------------------------------------- */
/*  Std2Ecl() applies a coordinate in 'standard' form to
 *  an ecliptic coordinate.
 */
static void Std2Ecl(Coordinate coord, Position pos)
{
   Position tmp;
    
   skyco_c(&pos.x, &pos.y, &fint1, &tmp.x, &tmp.y, &fint3);
   eclipco_c(&tmp.x, &tmp.y, &stdepoch,
             &coord->pos.x, &coord->pos.y, &coord->epoch);
   coord->update(coord);
}

/* -------------------------------------------------------------------------- */
/*                                 Gal2Std                                    */
/* -------------------------------------------------------------------------- */
/*  Gal2Std() returns a position in 'standard' form given a galactic
 *  coordinate.
 */
static Position Gal2Std(Coordinate coord)
{
   Position result;
   
   skyco_c(&coord->pos.x, &coord->pos.y, &fint2, &result.x, &result.y, &fint1);
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 Std2Gal                                    */
/* -------------------------------------------------------------------------- */
/*  Std2Gal() applies a coordinate in 'standard' form to
 *  a galactic coordinate.
 */
static void Std2Gal(Coordinate coord, Position pos)
{
   skyco_c(&pos.x, &pos.y, &fint1, &coord->pos.x, &coord->pos.y, &fint2);
   coord->update(coord);
}

/* -------------------------------------------------------------------------- */
/*                                 Sup2Std                                    */
/* -------------------------------------------------------------------------- */
/*  Sup2Std() returns a position in 'standard' form given a supergalactic
 *  coordinate.
 */
static Position Sup2Std(Coordinate coord)
{
   Position result;
   
   skyco_c(&coord->pos.x, &coord->pos.y, &fint4, &result.x, &result.y, &fint1);
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 Std2Sup                                    */
/* -------------------------------------------------------------------------- */
/*  Std2Sup() applies a coordinate in 'standard' form to
 *  a supergalactic coordinate.
 */
static void Std2Sup(Coordinate coord, Position pos)
{
   skyco_c(&pos.x, &pos.y, &fint1, &coord->pos.x, &coord->pos.y, &fint4);
   coord->update(coord);
}

/* -------------------------------------------------------------------------- */
/*                                 NewEqu                                     */
/* -------------------------------------------------------------------------- */
/*  NewEqu() creates a new equatorial coordinate.
 */
static Coordinate NewEqu(char *key, coordrepr repr, ident xin, char *epoch_key)
{
   Coordinate result=New(_Coordinate);
   char label[80];

   result->next= coordlist;
   coordlist   = result;
   result->system = EQUATORIAL;
   result->key = StrDup(key);
   result->changepos =
                     ScheduleKeyevent(readprocs[repr], key, KEYCHANGE, result);
   result->epoch = DEFAULT_EQU_EPOCH;
   (void)userfdble(&result->epoch, 1, 6, epoch_key, "Epoch");
   result->tostd = Equ2Std;
   result->setpos= Std2Equ;
   result->epoch_key = StrDup(epoch_key);
   result->changeepoch =
                    ScheduleKeyevent(epoch_equ, epoch_key, KEYCHANGE, result);
   result->update = writeprocs[repr];
   result->textfield = xin;
   sprintf(label,"RA, Dec (%4.1f)",result->epoch);
   GgiSetLabel(result->textfield, label, LABELWIDTH);

   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 NewEcl                                     */
/* -------------------------------------------------------------------------- */
/*  NewEcl() creates a new ecliptic coordinate.
 */
static Coordinate NewEcl(char *key, coordrepr repr, ident xin, char *epoch_key)
{
   Coordinate result=New(_Coordinate);
   char label[80];
    
   result->next= coordlist;
   coordlist   = result;
   result->system = ECLIPTIC;
   result->key = StrDup(key);
   result->changepos =
                     ScheduleKeyevent(readprocs[repr], key, KEYCHANGE, result);
   result->epoch = DEFAULT_ECL_EPOCH;
   (void)userfdble(&result->epoch, 1, 6, epoch_key, "Epoch");
   result->tostd = Ecl2Std;
   result->setpos= Std2Ecl;
   result->epoch_key = StrDup(epoch_key);
   result->changeepoch =
                    ScheduleKeyevent(epoch_ecl, epoch_key, KEYCHANGE, result);
   result->update = writeprocs[repr];
   result->textfield = xin;
   sprintf(label,"Ecl. L, B (%4.1f)",result->epoch);
   GgiSetLabel(result->textfield, label, LABELWIDTH);

   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 NewGal                                     */
/* -------------------------------------------------------------------------- */
/*  NewGal() creates a new galactic coordinate.
 */
static Coordinate NewGal(char *key, coordrepr repr, ident xin)
{
   Coordinate result=New(_Coordinate);
    
   result->next= coordlist;
   coordlist   = result;
   result->system = GALACTIC;
   result->key = StrDup(key);
   result->changepos =
                     ScheduleKeyevent(readprocs[repr], key, KEYCHANGE, result);
   result->tostd = Gal2Std;
   result->setpos= Std2Gal;
   result->update = writeprocs[repr];
   result->textfield = xin;
   GgiSetLabel(result->textfield, "Galactic L, B", LABELWIDTH);
    
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 NewSup                                     */
/* -------------------------------------------------------------------------- */
/*  NewSup() creates a new supergalactic coordinate.
 */
static Coordinate NewSup(char *key, coordrepr repr, ident xin)
{
   Coordinate result=New(_Coordinate);
    
   result->next= coordlist;
   coordlist   = result;
   result->system = SUPERGALACTIC;
   result->key = StrDup(key);
   result->changepos =
                     ScheduleKeyevent(readprocs[repr], key, KEYCHANGE, result);
   result->tostd = Sup2Std;
   result->setpos= Std2Sup;
   result->update = writeprocs[repr];
   result->textfield = xin;
   GgiSetLabel(result->textfield, "Supergalactic L, B", LABELWIDTH);
    
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 go_batch                                   */
/* -------------------------------------------------------------------------- */
/*  Key handler for GO button.
 *  It prepares keyword input for the task SKYTRANS, which will then be
 *  executed, after which some clean-up is done.
 */
static void go_batch(ident id, char *key, int mask, void *arg)
{
   static char syscodes[]={'*','G','S','E'};
   fint   irc;
   fint   skycode;
   double epoch;
   bool   button=FALSE;
   
   (void)userflog(&button, 1, 2, key, " ");
   if (button) {
      fchar  iofile;
      char iofilec[80];
      fint nchars;

      iofile.a = iofilec;
      iofile.l = 80;
      
      nchars = userftext(iofile, 2, "INFILE=", " ");
      iofilec[nchars] = '\0';
      (void)remove("skytool_tmp.rcl");
      (void)symlink(iofilec,"skytool_tmp.rcl");
      if (outptr) {
         fclose(outptr);
         outptr = NULL;
         nchars = userftext(iofile, 2, "OUTFILE=", " ");
         iofilec[nchars] = '\0';
         (void)remove(iofilec);
      }
      wkeyf("COORD=<skytool_tmp ;");
      (void)userfdble(&epoch, 1, 2, "EPOCH1=", " ");
      wkeyf("EPOCHIN=%f", epoch);                 /* write EPOCHIN= keyword */
      (void)userfdble(&epoch, 1, 2, "EPOCH2=", " ");
      wkeyf("EPOCHOUT=%f", epoch);                /* write EPOCHOUT= keyword */
      (void)userfint(&skycode, 1, 2, "INSYS=", " ");
      wkeyf("SKYSYSIN=%c",syscodes[skycode]);     /* write SKYSYSIN= keyword */
      (void)userfint(&skycode, 1, 2, "OUTSYS=", " ");
      wkeyf("SKYSYSOUT=%c",syscodes[skycode]);    /* write SKYSYSOUT= keyword */
      subst_c(tofchar("QUIT=BATCHQUIT="), &irc);         /* avoid confusion */
      subst_c(tofchar("FILENAME=OUTFILE="), &irc);
      wkeyf("BATCHQUIT=y");                       /* exit after one set */
      deputy_c(tofchar("skytrans"), &irc);        /* run SKYTRANS */
      (void)remove("skytool_tmp.rcl");
      wkeyf(key);                                 /* reset GO button */
      wkeyf("OUTFILE=");
      GgiSetLabel(statlab, "Output file closed", 0);
      if (irc<0) GgiSetLabel(errlab, "Failed to run SKYTRANS", 0);
   }
}

/* -------------------------------------------------------------------------- */
/*                                 handle_infile                              */
/* -------------------------------------------------------------------------- */
/*  Keyword handler for the output file.
 */
static void handle_infile(ident id, char* key, int code, void *arg)
{
   fchar  infile;
   char   infilec[80];
   fint   nchars;
   int    fd;
    
   infile.a = infilec;
   infile.l = 80;
   nchars = userftext(infile, 2, key, " ");
   infilec[nchars] = '\0';
   if ((fd = open(infilec, O_RDONLY, NULL)) < 0) {
      rejectf(key, "Cannot open input file");
      GgiDeactivate(gobut);
   } else {
      close(fd);
      GgiActivate(gobut);
      (void)GgiFileBrowser(ggiDelete, browser);
   }
}

/* -------------------------------------------------------------------------- */
/*                                 handle_outfile                             */
/* -------------------------------------------------------------------------- */
/*  Keyword handler for the output file.
 */
static void handle_outfile(ident id, char* key, int code, void *arg)
{
   static int phase=0;
   fchar  outfile;
   char   outfilec[80];
   fint   nchars;
     
   outfile.a = outfilec;
   outfile.l = 80;
   nchars = userftext(outfile, 2, key, " ");
   
   if (nchars==0) {
      if (outptr) {
         fclose(outptr);
         outptr = NULL;
         GgiSetLabel(statlab, "Output file closed", 0);
      }
      phase = 0;
      GgiPrompter("ACCEPT", NULL);
      return;
   } else {
      outfilec[nchars] = '\0';
      if (outptr) fclose(outptr);
      if (phase==0) {
         outptr = fopen(outfilec,"r");
         if (outptr) {
            GgiPrompter("AGAIN", "File exists.\nOK to append/overwrite?");
            phase = 1;
            return;
         }
      }
      phase = 0;
      outptr = fopen(outfilec,"a+");
      if (outptr) {
         char message[80];
         GgiPrompter("ACCEPT", NULL);
         sprintf(message, "Output file: %s", outfilec);
         GgiSetLabel(statlab, message, 0);
      } else {
         GgiPrompter("AGAIN", "Cannot write to file.");
      }
      
   }
}
   

/* -------------------------------------------------------------------------- */
/*                                 handle_file                                */
/* -------------------------------------------------------------------------- */
/*  Keyword handler for the file menu.
 */
static void handle_file(ident id, char* key, int code, void *arg)
{
   fint entry=0;  
          
   if (userfint(&entry, 1, 2, key, " ")==1) {
      wkeyf(key);
      switch (entry) {
         case FILE_INPUT: {
            browser = GgiFileBrowser(ggiIdent, "INFILE=");
            if (!browser) {
               browser = GgiFileBrowser(ggiCreate, "INFILE=",
                            "Input file te be processed by SKYTRANS", NULL);
            }
            break;
         }
         case FILE_OUTPUT: {
            GgiPrompter("OUTFILE=", "Output file for results");
            break;
         }
         case FILE_EXIT: {
            if (outptr) fclose(outptr);
            finis_c();                            /* terminate */
         }
      }
   }
}

/* -------------------------------------------------------------------------- */
/*                                 messages                                   */
/* -------------------------------------------------------------------------- */
/*  Create messages form
 */
static ident messages(void)
{
   ident logo;
   ident result=GgiForm(" ", 1);
   ident prev=GgiUseShell(result);

   errlab  = GgiSetLabel(GgiLabel("error"),  " ", 0);
   statlab = GgiSetLabel(GgiLabel("status"), "No output file", 0);
   logo    = GgiLogo(NULL, 0, 0);
   GgiSetPosition(errlab,  0, NULL, 0, NULL);
   GgiSetPosition(statlab, 0, NULL, 0, errlab);
   GgiSetPosition(logo, PANELWIDTH-GgiWidth(logo)+1, NULL, 0, NULL);
   (void)GgiUseShell(prev);
   return result;
}
   
MAIN_PROGRAM_ENTRY
{
   char *helplabels[]={
      "Program description",
      "_LINE",
      "User Interface",
      NULL
   };
   char *helpdocs[]={
      "skytool.dc1#description",
      NULL,
      "ggi.doc"
   };
    
   char *syslabels[]={
      "Equatorial",
      "Galactic",
      "Supergalactic",
      "Ecliptic",
      NULL
   };
    
   static char *my_resources[] = {
      "*error.foreground: red",
      NULL
   };
   
   ident  topbar, eq1frame, eq2frame, galframe, mesframe;
   ident  filemenu, logbut, inpsysmenu, outsysmenu, helpmenu;
   ident  equ1, equ1x, ecl1, ecl1x;
   ident  equ2, equ2x, ecl2, ecl2x;
   ident  gal, galx, sup, supx;
   ident  ep1, ep2;
   
   init_c();
   IDENTIFICATION("SKYTOOL", RELEASE);
   statusf("Change any field - the other fields will be updated");

   if (!TriggerKey("EPOCH1=")) wkeyf("EPOCH1=%6.1f", DEFAULT_EQU_EPOCH);
   if (!TriggerKey("EPOCH2=")) wkeyf("EPOCH2=%6.1f", DEFAULT_ECL_EPOCH);
   if (!TriggerKey("INSYS="))  wkeyf("INSYS=0");
   if (!TriggerKey("OUTSYS=")) wkeyf("OUTSYS=0");

   GgiAutoLayout(FALSE);
   GgiPostponeRealize(TRUE);
   GgiSetResources(my_resources);

   topbar   = GgiForm(" ", 1);
   eq1frame = GgiForm(" ", 1);
   eq2frame = GgiForm(" ", 1);
   galframe = GgiForm(" ", 1);
   mesframe = messages();
   GgiSetPosition(topbar,   0, NULL, 0, NULL);
   GgiSetPosition(eq1frame, 0, NULL, 0, topbar);
   GgiSetPosition(eq2frame, 0, NULL, 0, eq1frame);
   GgiSetPosition(galframe, 0, NULL, 0, eq2frame);
   GgiSetPosition(mesframe, 0, NULL, 0, galframe);
   
   (void)GgiUseShell(topbar);
   filemenu = GgiMenu("FILE=", NULL, filelabels);
   (void)ScheduleKeyevent(handle_file, "FILE=", KEYCHANGE, NULL);
   logbut = GgiButton("LOG=", "Writing to GIPSY log file ON/OFF");
   gobut  = GgiButton("GO=", "Start batch processing");
   GgiDeactivate(gobut);
   inpsysmenu = GgiMenu("INSYS=",
                           "Sky system for batch input", syslabels);
   GgiSetLabel(inpsysmenu,"Input Sky System",0);
   outsysmenu = GgiMenu("OUTSYS=",
                         "Sky system for log file and batch output", syslabels);
   GgiSetLabel(outsysmenu,"Output Sky System",0);
   helpmenu = GgiHelpMenu("HELP=", NULL, NULL, helplabels, helpdocs);
   GgiSetPosition(filemenu,    0, NULL,       0, NULL);
   GgiSetPosition(logbut,      0, filemenu,   0, NULL);
   GgiSetPosition(gobut,      20, logbut,     0, NULL);
   GgiSetPosition(inpsysmenu, 20, gobut,      0, NULL);
   GgiSetPosition(outsysmenu,  0, inpsysmenu, 0, NULL);
   GgiSetPosition(helpmenu, PANELWIDTH-GgiWidth(helpmenu)+1, NULL, 0, NULL);
   
   (void)GgiUseShell(eq1frame);
   MINSIZE(PANELWIDTH, 0);
   ep1 = GgiSetLabel(
         GgiTextField("EPOCH1=",
         "Epoch of first set of equatorial and ecliptic coordinates",6),
         "Epoch 1",LABELWIDTH/2+5);
   equ1x = GgiTextField("EQU1X=","Right Ascension, Declination (hms,dms)",26);
   equ1  = GgiTextField("EQU1=","Right Ascension, Declination (degrees)",22);
   ecl1x = GgiTextField("ECL1X=","Ecliptic longitude, latidute (dms,dms)",26);
   ecl1  = GgiTextField("ECL1=","Ecliptic longitude, latidute (degrees)",22);
   GgiSetPosition(ep1,   0, NULL,  0, NULL);
   GgiSetPosition(equ1x, 0, NULL,  0, ep1);
   GgiSetPosition(equ1,  0, equ1x, 0, ep1);
   GgiSetPosition(ecl1x, 0, NULL,  0, equ1x);
   GgiSetPosition(ecl1,  0, ecl1x, 0, equ1x);
   
   (void)GgiUseShell(eq2frame);
   MINSIZE(PANELWIDTH, 0);
   ep2 = GgiSetLabel(
         GgiTextField("EPOCH2=",
         "Epoch of second set of equatorial and ecliptic coordinates",6),
         "Epoch 2",LABELWIDTH/2+5);
   equ2x = GgiTextField("EQU2X=","Right Ascension, Declination (hms,dms)",26);
   equ2  = GgiTextField("EQU2=","Right Ascension, Declination (degrees)",22);
   ecl2x = GgiTextField("ECL2X=","Ecliptic longitude, latidute (dms,dms)",26);
   ecl2  = GgiTextField("ECL2=","Ecliptic longitude, latidute (degrees)",22);
   GgiSetPosition(ep2,   0, NULL,  0, NULL);
   GgiSetPosition(equ2x, 0, NULL,  0, ep2);
   GgiSetPosition(equ2,  0, equ2x, 0, ep2);
   GgiSetPosition(ecl2x, 0, NULL,  0, equ2x);
   GgiSetPosition(ecl2,  0, ecl2x, 0, equ2x);
   
   (void)GgiUseShell(galframe);
   MINSIZE(PANELWIDTH, 0);
   galx  = GgiTextField("GALX=","Galactic longitude, latidute (dms,dms)",26);
   gal   = GgiTextField("GAL=","Galactic longitude, latidute",22);
   supx  = GgiTextField("SUPX=","Supergalactic longitude, latidute (dms,dms)",
                         26);
   sup   = GgiTextField("SUP=","Supergalactic longitude, latidute (degrees)",
                         22);
   GgiSetPosition(galx,  0, NULL, 0, NULL);
   GgiSetPosition(gal,   0, galx, 0, NULL);
   GgiSetPosition(supx,  0, NULL, 0, galx);
   GgiSetPosition(sup,   0, supx, 0, galx);

   (void)NewEqu("EQU1=", DEGREES, equ1,  "EPOCH1=");
   (void)NewEqu("EQU1X=", HMSDMS, equ1x, "EPOCH1=");
   (void)NewEcl("ECL1=", DEGREES, ecl1,  "EPOCH1=");
   (void)NewEcl("ECL1X=", DMSDMS, ecl1x, "EPOCH1=");
   (void)NewEqu("EQU2=", DEGREES, equ2,  "EPOCH2=");
   (void)NewEqu("EQU2X=", HMSDMS, equ2x, "EPOCH2=");
   (void)NewEcl("ECL2=", DEGREES, ecl2,  "EPOCH2=");
   (void)NewEcl("ECL2X=", DMSDMS, ecl2x, "EPOCH2=");
   (void)NewGal("GAL=",  DEGREES, gal);
   (void)NewGal("GALX=",  DMSDMS, galx);
   (void)NewSup("SUP=",  DEGREES, sup);
   (void)NewSup("SUPX=",  DMSDMS, supx);
   GgiRealize();
   (void)ScheduleKeyevent(handle_outfile, "OUTFILE=", KEYCHANGE, NULL);
   (void)ScheduleKeyevent(handle_infile,  "INFILE=",  KEYCHANGE, NULL);
   (void)ScheduleKeyevent(go_batch,       "GO=",      KEYCHANGE, NULL);
   wkeyf("EQU1=0.000000 0.000000");                /* initial position */
   MainLoop();
}
