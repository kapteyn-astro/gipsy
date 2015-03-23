/* taskcom.h
                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>          taskcom.dc3
Include:    taskcom

Purpose:    Private. Definitions and declarations to be used in 
            hermes communication routines.

File:       taskcom.h

Author:     J.P. Terlouw

Updates:    25-Feb-90  --  original document
            11-Oct-90  --  changed to allow choice between commun. methods
             4-Mar-91  --  some symbols added
            29-Nov-91  --  SIGABORT symbol added  
            11-Dec-91  --  Reject message length increased
             9-Nov-92  --  EDITFILE function added
            18-Mar-93  --  CMDLEN increased to 4000
            18-Oct-93  --  bind_hermes buffer size limitation removed
             4-Jan-94  --  LISTCTRL function added
             4-Feb-94  --  CMDLEN increased to 8000
            16-Feb-94  --  CMDLEN increased to 15000
            24-Feb-94  --  removed RAWPAR, defined ALLPAR
            21-Jul-94  --  defined DECODE
             8-Dec-94  --  defined DCDERR
             8-Apr-97  --  event codes and structures added
            10-Jun-98  --  added deputy return codes for event-driven tasks
             1-Jul-98  --  SAVEPAR function added
            17-Jan-01  --  XEQCONT function added
            10-Mar-09  --  increased MSGLEN
            15-Apr-11  --  ABORT function added
#<

*/
#if !defined(_taskcom_h_)
#define _taskcom_h_

#include "gipsyc.h"

#define VERSION    3       /* protocol version */
#define N_ATFINIS 16       /* max. number of proc's registered by atfinis()  */
#define SIGABORT SIGINT    /* signal to be used for user abort               */
/* ========================================================================= */
/*                          Various dimensions                               */
/* ------------------------------------------------------------------------- */
#define KEYLEN    20       /* Userinp keyword      */
#define MSGLEN   160       /* status message       */
#define TSKLEN    20       /* task name            */
#define CMDLEN 15000       /* hermes command       */
#define LINLEN   132       /* anyout line          */
#define TYPLEN   150       /* type buffer          */
#define REJLEN    75       /* reject message       */
#define SUBLEN   128       /* substitute spec      */
#define EDTLEN    75       /* file name length     */
#define DCDLEN  8192       /* decode buffer length */

/* ========================================================================= */
/*                         Major function codes                              */
/* ------------------------------------------------------------------------- */
#define INIT      1
#define FINIS     2
#define CANCEL    3
#define PAUSE     4
#define USERINP   5
#define DEPUTY    6
#define XEQ       7
#define SUBST     8
#define ANYOUT    9
#define CANALL   10
#define MYNAME   11
#define RAWPAR   12  /* obsolete */
#define ERROR    13
#define STATUS   14
#define WKEY     15
#define XEQXIT   16
#define TYPE     17
#define REJECT   18
#define EDITFILE 19
#define LISTCTRL 20
#define ALLPAR   21
#define DECODE   22
#define DCDERR   23
#define NOTIFY   24
#define SAVEPAR  25
#define XEQCONT  26
#define ABORT    27

#define LAST   -1 /* sent by the Hermes SIGCHLD handler on behalf of servant */

/* ========================================================================= */
/*                            USERINP subcodes                               */
/* ------------------------------------------------------------------------- */
#define USERINT   1
#define USERLOG   2
#define USERREAL  3
#define USERDBLE  4
#define USERCHAR  5
#define USERCHARU 6
#define USERCHARL 7
#define USERTEXT  8

/* ========================================================================= */
/*                 status codes returned by Hermes                           */
/* ------------------------------------------------------------------------- */
#define ERRNOTIMP  -666                     /* function not implemented */
#define ERRLEVEXC    -1                     /* error level exceeded     */
#define BADVERSION   -2                     /* incompatible protocol    */
#define SUBSTODD     -1                     /* odd number of keywords   */
#define SUBSTFULL    -3                     /* substitution table full  */
#define DEPUTYSUC     1                     /* successful deputy exec.  */
#define NOTPRESENT   -6                     /* task not present         */
#define MAXDEPEXC    -7                     /* nr. of deputies exceeded */
#define DEPFATAL     -8                     /* deputy fatal error       */
#define DEPABORT     -9                     /* deputy aborted           */
#define DEPCRASH    -10                     /* deputy crashed           */
#define DEPNODEF    -11                     /* deputy no-default usrinp */

/* ========================================================================= */
/*                       Hermes request structures                           */
/* ------------------------------------------------------------------------- */

typedef struct _Request {
   int    code;                                /* request code */
} *Request,  /* "generic" request */
  *FinisRequest, 
  *CanallRequest, 
  *MynameRequest, 
  *RawparRequest;

typedef struct _InitRequest {
   int    code;
   int    version;
} *InitRequest;

typedef struct _CancelRequest {
   int    code;
   char   key[KEYLEN];                         /* keyword to be cancelled */
} *CancelRequest;

typedef struct _PauseRequest {
   int     code;
   char    message[MSGLEN];                    /* status message */
} *PauseRequest;

typedef struct _UserinpRequest {
   int     code;
   int     type;                               /* subcode */
   int     level;                              /* default level */
   int     number;                             /* number of items requested */
   int     width;                              /* width for CHAR% subcodes */
   char    key[KEYLEN];                        /* keyword */
   char    message[MSGLEN];                    /* status message */
} *UserinpRequest;

typedef struct _DeputyRequest {
   int     code;
   char    taskname[TSKLEN];                   /* name of task to be started */
} *DeputyRequest;

typedef struct _XeqRequest {
   int     code;
   char    command[CMDLEN];                   /* Hermes command to be xeq'ted */
} *XeqRequest, *XeqxitRequest;

typedef struct _XeqcontRequest {
   int     code;
   char    key[KEYLEN];                       /* Status return keyword */
   char    command[CMDLEN];                   /* Hermes command to be xeq'ted */
} *XeqcontRequest;

typedef struct _SubstRequest {
   int     code;
   char    string[SUBLEN];                    /* string with substitutions */
} *SubstRequest;

typedef struct _AnyoutRequest {
   int     code;
   int     devmask;                           /* device mask */
   char    line[LINLEN];                      /* output line */
} *AnyoutRequest;

typedef struct _StatusRequest {
   int     code;
   char    message[MSGLEN];                   /* status message */
} *StatusRequest;

typedef struct _ErrorRequest {
   int     code;
   int     level;                             /* error level */
   char    message[MSGLEN];                   /* status message */
} *ErrorRequest;

typedef struct _WkeyRequest {
   int     code;
   char    command[CMDLEN];                   /* string with key/value pairs */
} *WkeyRequest;

typedef struct _TypeRequest {
   int     code;
   char    string[TYPLEN];                    /* string to be "typed" in UCA */
} *TypeRequest;

typedef struct _RejectRequest {
   int     code;
   char    key[KEYLEN];                       /* key to be rejected   */
   char    message[REJLEN];                   /* message for the user */
} *RejectRequest;

typedef struct _EditfileRequest {
   int     code;
   char    filename[EDTLEN];                  /* name of file to be edited */
   char    message[REJLEN];                   /* message for the user */
} *EditfileRequest;

typedef struct _ListctrlRequest {
   int     code;
   int     mask;                              /* new 'device mask'         */
} *ListctrlRequest;

typedef struct _AllparRequest {
   int     code;
   int     mode;                              /* 0=new sequence; 1=continue */
   int     length;
} *AllparRequest;

typedef struct _DecodeRequest {
   int     code;                             
   int     type;                              /* subcode */
   int     number;                            /* number of items requested */
   char    expr[DCDLEN];
} *DecodeRequest;

typedef struct _DcderrRequest {
   int     code;
   int     error;
} *DcderrRequest;

typedef struct _NotifyRequest {
   int     code;
   int     mode;                              /* 1 = "ON", 0 = "OFF"       */
} *NotifyRequest;

typedef struct _SaveparRequest {
   int     code;
   char    filename[EDTLEN];
} *SaveparRequest;

typedef struct _AbortRequest {
   int     code;
   char    taskname[TSKLEN];
} *AbortRequest;

typedef struct _LastRequest {
   int     code;
   int     status;
} *LastRequest;

/* ------------------------------------------------------------------------- */
/*             Union containing all structures defined above                 */
/* ------------------------------------------------------------------------- */
typedef union {
   struct _Request         request;
   struct _InitRequest     init;
   struct _CancelRequest   cancel;
   struct _PauseRequest    pause;
   struct _UserinpRequest  userinp;
   struct _DeputyRequest   deputy;
   struct _XeqRequest      xeq;
   struct _XeqcontRequest  xeqcont;
   struct _SubstRequest    subst;
   struct _AnyoutRequest   anyout;
   struct _ErrorRequest    error;
   struct _StatusRequest   status;
   struct _WkeyRequest     wkey;
   struct _TypeRequest     type;
   struct _RejectRequest   reject;
   struct _EditfileRequest editfile;
   struct _ListctrlRequest listctrl;
   struct _AllparRequest   allpar;
   struct _DecodeRequest   decode;
   struct _DcderrRequest   dcderr;
   struct _NotifyRequest   notify;
   struct _SaveparRequest  savepar;
   struct _AbortRequest    abort;
} UnitedRequest;

/* ========================================================================= */
/*                   Hermes request message structure                        */
/* ------------------------------------------------------------------------- */
typedef struct {
   int              reqlen;            /* length of the request (bytes) */
   int              pid;               /* process id of the requestor */
   UnitedRequest    *request;
} RequestMessage;

/* ========================================================================= */
/*                            derived constants                              */
/* ------------------------------------------------------------------------- */

#define REQLEN (sizeof(UnitedRequest *))
#define MSGHED (sizeof(RequestMessage)-REQLEN)

/* ========================================================================= */
/*                           Hermes Event Codes                              */
/* ------------------------------------------------------------------------- */
#define KEYCHANGE   1                   /* user input keyword change       */
#define KEYCANCEL   2                   /* user input keyword cancel       */
#define KEYREJECT   4                   /* user input keyword reject       */
#define RESEVENT    8                   /* resume after suspend            */

/* ========================================================================= */
/*                          Hermes Event Structures                          */
/* ------------------------------------------------------------------------- */
typedef struct _Event {
   int     code;
} _Event, *Event;

typedef struct _KeyEvent {
   int     code;              /* one of KEYCHANGE, KEYCANCEL or KEYREJECT */
   char    key[KEYLEN];       /* Keyword involved                         */
} _KeyEvent, *KeyEvent;

typedef struct _ResEvent {
   int     code;              /* RESEVENT */
} _ResEvent, *ResEvent;

typedef union {
   struct _KeyEvent key;
   struct _KeyEvent res;
} UnitedEvent;

/* ========================================================================= */
/*                           Procedure pointers                              */
/* ------------------------------------------------------------------------- */

typedef void (*send_proc)(RequestMessage *);      /* send to Hermes          */
typedef int  (*recv_proc)(char *, int);           /* receive from Hermes     */ 
typedef void (*CleanProc)(void*);         /* procedure registered by atfinis */ 

/* ========================================================================= */
/*                           FINIS callback entry                            */
/* ------------------------------------------------------------------------- */

typedef struct _CleanBlock {
   CleanProc   proc;
   void        *data;
   void        *next;
} CleanBlock;

/* ========================================================================= */
/*                          Declaration                                      */
/* ------------------------------------------------------------------------- */

extern int       bind_hermes( send_proc*, recv_proc*);


/* ------------------------------------------------------------------------- */
#endif  /* DO NOT ADD ANYTHING AFTER THIS LINE */
