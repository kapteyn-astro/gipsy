/* hercom.h
                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>            hercom.dc3

Include:    hercom

Purpose:    Private. Definitions and declarations to be used in 
            hermes communication routines.

File:       hercom.h

Author:     J.P. Terlouw

Updates:    25-Feb-90  --  original document
            11-Oct-90  --  changed to allow choice between commun. methods
#<

*/

#if !defined(_hercom_h_)
#define _hercom_h_

#include "taskcom.h"
 
/* ========================================================================= */
/*                   Various dimensions and constants                        */
/* ------------------------------------------------------------------------- */
#define MAXACT      1  /* maximum number of active servant processes */
#define NAMLEN     31  /* servant name length */
#define TEMPLATE   -1  /* pidstatus argument causing template to be returned */
#define SCREEN      1  /* terminal screen device mask */
#define LOGFILE     2  /* log file device mask */
#define NOEXPERT    8  /* terminal screen, suppressed in "expert mode" */
#define SCREENTEST 16  /* test mode terminal screen device mask */
#define DEFMASK    (SCREEN|LOGFILE|NOEXPERT) /* default device mask */
#define KEYPROMPT   1  /* "prompted" context for prompter and satisfy */
#define FREE        2  /* "free" context for prompter and satisfy */

#define PAREXT "_parm" /* parameter file suffix to process name */
#define CTRLCMAX    3  /* maximum number of CTRL-C's before exiting */

/* ========================================================================= */
/*                      task status block structure                          */
/* ------------------------------------------------------------------------- */
typedef struct _TaskStatus {
   int         pid;       /* process identification */
   int         taskno;    /* task ordinal number */
   int         errlev;    /* error level */
   int         meslev;    /* message level */
   int         devmask;   /* anyout device mask */
   char        name[NAMLEN+1]; /* task name */
   char        parname[NAMLEN+1]; /* parameter file name */
   enum { WAITRUN, RUNNING, WAITINP, PAUSING } state; /* task state */
   enum { EXPERIENCED, NORMAL, TEST } output_mode;
   enum { NOMSG, PLUSTERM, PLUSINP, PLUSPAUSE, ALL } status_level;
   struct _UserinpRequest userinp;  /* outstanding userinp request */
} *TaskStatus;

/* ========================================================================= */
/*                         function declarations                             */
/* ------------------------------------------------------------------------- */

extern bool       satisfy    (TaskStatus,UserinpRequest,int);

extern TaskStatus pidstatus  (int);
extern TaskStatus namstatus  (char*);
extern void       initstatus (void);

extern int        prompter(char*,int);

extern void       reply_clear(void);
extern bool       reply_ready(void);
extern void       reply_send (TaskStatus,int);
extern void       reply_put  (char*,int);

extern void       put_coa    (TaskStatus,char*);
extern void       put_log    (TaskStatus,char*);
extern void       put_psa    (TaskStatus,char*,int);
extern void       put_uca    (char*);
extern void       err_uca    (char*);

extern void       init_par   (TaskStatus);
extern void       use_par    (TaskStatus);
extern void       insert_par (TaskStatus,char*,char*);
extern void       delete_par (TaskStatus,char*);
extern int        find_par   (TaskStatus,char*,char*,int);

#endif  /* DO NOT ADD ANYTHING AFTER THIS LINE */
