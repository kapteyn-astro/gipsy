/* srvreq.c
                           COPYRIGHT (c) 1993-2011
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>            srvreq.dc3
Document:     srvreq

Purpose:      user interface module

Category:     SYSTEM, USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Description:  The routines available to applications programmers which
              communicate with HERMES are the following (Each routine
              is described in more detail in its own DC2 document):
              
              Name        Type               Action
              ----------- ------------------ ----------------------------------
              INIT        Subroutine         Open communication with HERMES
              FINIS       Subroutine         Close communication with HERMES
              USERINT     integer function   Get integers from user
              USERLOG     integer function   Get logicals from user
              USERREAL    integer function   Get reals from user
              USERDBLE    integer function   Get doubles from user
              USERCHAR    integer function   Get char. strings from user
              USERCHARU   integer function   As USERCHAR, but now upperc.
              USERCHARL   integer function   As USERCHAR, but now lowerc.
              USERTEXT    integer function   Get text from user
              ANYOUT      subroutine         Send text to screen/logfile
              ERROR       subroutine         Report an error to user
              ABORT       integer function   Abort an other task.
              PAUSE       subroutine         Put task in pause state
              CANCEL      subroutine         Remove keyword from list
              REJECT      subroutine         Reject keyword
              XEQ         subroutine         Execute another task
              XEQCONT     integer function   Start task and continue
              TYPE        subroutine         Type something in UTA
              STATUS      subroutine         Show status message
              WKEY        subroutine         Put keyword in list
              DEPUTY      subroutine         Start another task
              SUBST       subroutine         Keyword substitution
              CANALL      subroutine         Cancel all keywords in list
              XEQXIT      subroutine         Execute task and stop
              MYNAME      character function Return name of task
              ALLPAR      integer function   Obtain all USERxxx parameters
              SAVEPAR     integer function   Save USERxxx parameters in file
              EDITFILE    integer function   Edit a text file
              LISTCTRL    integer function   Control ANYOUT device mask
              DECODEINT   integer function   Decode integers
              DECODEREAL  integer function   Decode reals
              DECODEDBLE  integer function   Decode doubles
              DCDERRSTR   character function Return DECODExxx error message
              GIPSYTASK   logical function   Test whether program is GIPSY task
              atfinis     int function       register callback with FINIS
              atfinisrm   void function      unregister callback with FINIS
              atabort     void function      register callback for user aborts
              notify      int function       connect to notification socket
              keystatus   int function       check status of last user input
              status_cb   void function      register status messages callback
              wkey_cb     void function      register wkey callback (for Ggi)
              
              For C programmers all prototype declarations can be made available
              by including srvreq.h. 

Limitations:  Though the code of this routine is complete, not all
              functions may actually work. This is depends on 
              which functions are actually provided by the specific 
              kind and version of Hermes.

Updates:      Jul 13, 1989: KGB, Document created.
              Feb 15, 1990: JPT, Rewritten for new Hermes
              Mar 16, 1990: JPT, Trim trailing blanks from Userinp keywords
              Sep 17, 1990: JPT, Bug removed from USERTEXT part
              Oct 11, 1990: JPT, INIT can bind to different commun. methods
              Nov  7, 1990: JPT, atfinis() and atfinisrm() added
              Dec  3, 1990: JPT, message length bugs in a.o. deputy_c fixed
              Dec 17, 1990: JPT, subroutine REJECT added
              Apr 17, 1991: JPT, reply buffer check added for Userinp.
              Jun 29, 1991: KGB, correct alignment for all variable types.
              Jul  3, 1991: PRR, exit status 1 for ERROR and DEPUTY etc.
              Nov 29, 1991: JPT, atabort() added.
              Dec  2, 1991: JPT, XEQ return codes revised.
              Sep 07, 1992: JPT, change directory performed in init_c().
              Nov 09, 1992: JPT, implemented EDITFILE
              Oct 15, 1993: JPT, removed buffer size limitations
              Oct 19, 1993: JPT, added logging of process starts and stops
              Jan  4, 1994: JPT, implemented LISTCTRL
              Feb 23, 1994: JPT, removed RAWPAR; implemented ALLPAR
              Jul 21, 1994: JPT, implemented DECODExxx
              Jul 22, 1994: JPT, implemented GIPSYTASK
              Dec  8, 1994: JPT, implemented DCDERRSTR
              Apr  8, 1997: JPT, implemented notify()
              Nov 24, 1997: JPT, implemented keystatus()
              Feb  9, 1998: JPT, implemented status_cb()
              Jun 10, 1998: JPT, updated doc. for USERTEXT and DEPUTY
              Jul  1, 1998: JPT, implemented SAVEPAR
              Jan 17, 2001: JPT, implemented XEQCONT
              Jun  2, 2004: JPT, use errno.h instead of explicit declaration
              Jan 15, 2009: JPT, GIPSYTASK returns TRUE after successful INIT
#<
*/
#include "stddef.h"
#ifdef  __linux__   
#define __LINUX_UIO_H
#endif
#include "signal.h"
#include "taskcom.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "errno.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <fcntl.h>

#define  ISSUE(r) { int result; \
                    if ((result=send_req((UnitedRequest*)r,sizeof(*r)))) \
                    fail((Request)r,result); }
#define  UISSUE(r) { int result; ufailed = 0; \
                    if ((result=send_req((UnitedRequest*)r,sizeof(*r)))) \
                    ufail((UserinpRequest)r,result); if (ufailed) return 0;}
#define  GETREPLY(l) lenrcv = (*recv_msg)(replybuf,(l));

int         getpid( );

static void NotInitialized(RequestMessage *);

static int  key2str  ( fchar, char*, int );
static void fail     ( Request, int );
static void ufail    ( UserinpRequest, int );
static int  send_req ( UnitedRequest*, int);
static void InitCleanup (void);
static void Cleanup  (void);
static void intsigh  (int);
static void hupsigh  (int);

static send_proc       send_msg = NotInitialized;
static recv_proc       recv_msg;
static void (*status_callback)(char *)=NULL;
static int (*wkey_callback)(void)=NULL;
static UnitedRequest   requests;
static Request         request=(Request)&requests;
static InitRequest     init=(InitRequest)&requests;
static UserinpRequest  userinp=(UserinpRequest)&requests;

static int             lenrcv;            /* length received   */
static int             ufailed=0;         /* user input failed */  

static CleanBlock  clnblk[N_ATFINIS];
static CleanBlock *clnfree=NULL;
static CleanBlock *clnregd=NULL;
static CleanBlock *abtblk=NULL;


/*
#>            gipsytask.dc2
Function:     GIPSYTASK

Purpose:      GIPSYTASK tests whether the calling program is a GIPSY task.

Category:     SYSTEM, USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Use:          GIPSYTASK()    LOGICAL function

              returns .TRUE. when INIT has been called successfully,
              .FALSE. otherwise.

Updates:      Jul 22, 1994: JPT, Original document.
              Jan 15, 2009: JPT, Now depends on INIT call.
#<

@logical function gipsytask ( )

*/

bool gipsytask_c(void)
{
   return send_msg!=NotInitialized;
}

/*
#>            init.dc2
Subroutine:   INIT

Purpose:      INIT declares the task running to HERMES and does the
              necessary initializations.

Category:     SYSTEM, USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL INIT

Updates:      Jul 10, 1979: JPT, Original document.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Oct 11, 1990: JPT, 1) call to bind_hermes to get communication
                                 method. 2) sends protocol version number.
              Sep 07, 1992: JPT, change directory performed in this module.
#<

@ subroutine init( )

*/

void init_c( void )
{
   char replybuf[81];    /* +++ 81 = DIRLEN +1 from hermescom.h in hermes.src */
   
   InitCleanup();                                 /* initialize callback list */
   bind_hermes(&send_msg,&recv_msg);           /* choose communication method */
   init->code = INIT;
   init->version = VERSION;                        /* to be checked in Hermes */
   ISSUE(init)
   GETREPLY(80)
   replybuf[lenrcv] = '\0';
   if (lenrcv) (void)chdir(replybuf);              /* set working directory   */
   signal(SIGHUP,hupsigh);                         /* for 'orphan' sessions   */
   signal(SIGTSTP,hupsigh);                        /* for 'orphan' sessions   */
}

/*
#>            finis.dc2
Subroutine:   FINIS

Purpose:      Informs HERMES that task quits and cleans up the mess.

Category:     SYSTEM, USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL FINIS

Updates:      Jul 10, 1979: original document
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
      

#<

@ subroutine finis( )

*/

void finis_c( void )
{
   request->code = FINIS;
   ISSUE(request)
   Cleanup();
   exit(0);
}

/*
#>            atfinis.dc2
Function:     atfinis

Purpose:      Register procedure to be called by FINIS.

Category:     SYSTEM

File:         srvreq.c

Author:       J.P. Terlouw

Use:          result = atfinis( proc, data);
                      result : result code; type int.
                               = 1 - procedure successfully registered;
                               = 0 - insufficient callback entries.
                      proc    : procedure to be called; type void(*)(void*).
                      data    : data to be passed to proc; type void*.

              If atfinis is called more than once, FINIS will call the most 
              recently registered procedures first.

Limitations:  This procedure is only available from C. The maximum number of
              callback entries is determined by the constant N_ATFINIS
              defined in taskcom.h.

Updates:      Nov  7, 1990: JPT, Document created.
#<
*/

int atfinis(CleanProc proc, void *data)
{
   CleanBlock *current=clnfree;
   
   if (!current) return 0;                      /* no more space ?            */
   clnfree = current->next;                     /* remove from free list      */
   current->proc = proc;                        /* assign procedure pointer   */
   current->data = data;                        /* assign data pointer        */
   current->next = clnregd;                     /* link into registered list  */
   clnregd = current;                           /*            ,,              */
   return 1;
}

/*
#>            atfinisrm.dc2
Function:     atfinisrm

Purpose:      Unregister procedure previously registered by atfinis.

Category:     SYSTEM

File:         srvreq.c

Author:       J.P. Terlouw

Use:          atfinisrm( proc, data);
                      proc    : registered procedure; type void(*)(void*).
                      data    : registered data pointer; type void*.

Limitations:  This procedure is only available from C.

Updates:      Nov  7, 1990: JPT, Document created.
#<
*/

void atfinisrm(CleanProc proc, void *data)
{
   CleanBlock **current=&clnregd,*tmp;
    
   while (*current) {
                             /* procedure pointer AND data pointer must match */
      if ((*current)->proc == proc && (*current)->data == data) {
         tmp = *current;                    /* unlink from registered list    */
         *current = tmp->next;              /* link into free list            */
         tmp->next = clnfree;
         clnfree = tmp;
         break;
      } else {
         current = (CleanBlock **)&(*current)->next;   /* no match; try next  */
      }
   }
}

/*
#>            atabort.dc2
Function:     atabort

Purpose:      Register procedure to be called whenever a user abort occurs.

Category:     SYSTEM

File:         srvreq.c

Author:       J.P. Terlouw

Use:          atabort( proc, data);
                      proc    : procedure to be called; type void(*)(void*).
                                if a null pointer is specified, any previous
                                registration is removed.
                      data    : data to be passed to proc; type void*.

              If atabort is called more than once, only the most
              recently registered procedure will be called, i.e. subsequent
              calls supersede previous ones.
              As user aborts are asynchronous events, special care is needed
              to avoid damaging things which are in an indeterminate state.
              To protect critical sections of code, the routines enterc_c and
              leavec_c can be used.

Limitations:  This procedure is only available from C.

Related documents:
              enterc.dc2 leavec.dc2 atfinis.dc2

Updates:      Nov 29, 1991: JPT, Document created.
#<
*/

void atabort(CleanProc proc, void *data)
{
   if (proc) {
      if (!abtblk) abtblk=calloc(1,sizeof(struct _CleanBlock));
      abtblk->proc = proc;
      abtblk->data = data;
   } else {
      if (abtblk) {
         free(abtblk);
         abtblk = NULL;
      }
   }
}      

/*
#>            notify.dc3
Function:     notify

Purpose:      Connect to Hermes' notification socket

Category:     SYSTEM
  
File:         srvreq.c
 
Author:       J.P. Terlouw
 
Use:          fd = notify(mode);
                       int fd   - file descriptor on socket or zero if mode==0
                       int mode - 1: connect; 0: disconnect

Description:  When a task is connected to the notification socket, Hermes
              will send event messages over this socket. These messages
              are defined in taskcom.h and include messages regarding
              changes on user input keywords. The task must be prepared
              to read these messages.
              This function is not normally called directly from
              application code, but rather through ScheduleHerevent or
              utility routines which call the latter routine.
              
Updates:      Apr  8, 1997: JPT, document created.
#<
#>            notify.h
#if !defined(_notify_h_)
#define _notify_h_
extern int notify(int);
#endif
#<
*/

extern int notify(int mode)
{
   char replybuf[128];
   int  fd, status;
   struct sockaddr_un addr;
    
   NotifyRequest notify=(NotifyRequest)request;
   notify->code = NOTIFY;
   notify->mode = mode;
   ISSUE(notify);
   if (mode) {
      GETREPLY(128)
      replybuf[lenrcv] = '\0';
      fd = socket( AF_UNIX, SOCK_STREAM, 0 );
      if (fd<0) return fd;
      addr.sun_family = AF_UNIX;
      strcpy( addr.sun_path, replybuf);
      if ((status=connect(fd,(struct sockaddr *)&addr, sizeof(addr))) < 0 ) {
         close(fd);
         return -errno;
      }
   } else fd = 0;
   return fd;
}

/*
#>            keystatus.dc2
Function:     keystatus

Purpose:      Obtain user input error code.

Category:     USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Use:          #include "srvreq.h"
              int result;
              
              result = keystatus();

              result =  0 :  no error
                       -2 :  last user input request failed.

Description:  keystatus() is intended to be used in event-driven
              tasks. In this case when a user input failure occurs, e.g.
              bad input, no default allowed, etc., the user input
              function (e.g. userreal_c) returns zero after which
              keystatus() can be called to find out whether 
              a problem has occured.
              In "normal", not event-driven, tasks the user input
              function does not return until any error condition has
              been removed by Hermes.

Updates:      Nov 24, 1997: JPT, Original document
#<
*/
                       

extern int keystatus(void)
{
   return ufailed;
}

/*
#>            status_cb.dc3
Function:     status_cb

Purpose:      Register callback to be called by status_c().

Category:     SYSTEM

File:         srvreq.c

Author:       J.P. Terlouw

Use:          #include "srvreq.h"
              void (*callback)(char*);
               
              status_cb(callback);
              
              callback  : pointer to function receiving the status message,
                          if NULL, the callback is de-registered.

Description:  The primary purpose for this registration is to allow Ggi
              to display the status messages generated by the task.
              Only one callback can be active at the same time.
              The argument to callback will be a string consisting of
              the task name followed by the status message.

Updates:      Feb 9, 1998: JPT, Original document
#<
*/

extern void status_cb(void (*callback)(char*))
{
   status_callback = callback;
}

/*
#>            wkey_cb.dc3
Function:     wkey_cb

Purpose:      Register callback to be called by wkey_c().

Category:     SYSTEM

File:         srvreq.c

Author:       J.P. Terlouw

Use:          #include "srvreq.h"
              int (*callback)(void);
               
              wkey_cb(callback);
              
              callback  : pointer to function to be called after wkey_c
                          has sent its request to Hermes.
                          if NULL, the callback is de-registered.

Description:  The only purpose for this registration is to allow Ggi
              to immediately handle the keyword event caused by wkey's
              action. The function to be registered is currently always
              GgiHandleEvents().

Updates:      Jun 30, 1999: JPT, Original document
#<
*/

extern void wkey_cb(int (*callback)(void))
{
   wkey_callback = callback;
}

/*
#>            allpar.dc2
Function:     ALLPAR

Purpose:      Obtain all HERMES user input parameters for issuing task.

Category:     USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER ALLPAR( MODE,         Input-Output  integer
                              TEXT)         Output        character*(*)
              
              MODE   : 0 fresh start; 1 continue.
                       Upon return it is set to 1.
              TEXT   : Receives the information.
              ALLPAR : >0 number of characters read into TEXT;
                       =0 no more characters available;
                       <0 error.

Description:  This function enables a program to obtain all actual user input
              parameters. This is normally done in a sequence of calls.
              The first call in this sequence is done with MODE=0; subsequent
              calls are done with MODE not equal to 0. Note that MODE is also
              an output argument.

Updates:      Feb 23, 1994: JPT, Original document
#<

Fortran to C interface:

@ integer function allpar ( integer character )

*/
fint allpar_c (fint *mode, fchar params)
{
   AllparRequest allpar = (AllparRequest)request;
   
   char *replybuf = params.a;
   fint  result;
   int i;
   
   allpar->code = ALLPAR;
   allpar->mode = *mode;
   allpar->length = params.l;
   result = send_req((UnitedRequest*)allpar,sizeof(*allpar));
   if (result>0)  {
      GETREPLY(result)
      for (i=lenrcv; i<params.l; i++) params.a[i]=' ';
   }
   *mode = 1;
   return result;
}

/*
#>            savepar.dc2
Function:     SAVEPAR

Purpose:      Save to file all HERMES user input parameters for issuing task.

Category:     USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER SAVEPAR(
                                FILENAME          Input        character*(*)
                             )
              
Description:  This function enables a program to save all actual user input
              parameters to a file.

Updates:      Jul  1, 1998: JPT, Original document
#<

Fortran to C interface:

@ integer function savepar ( character )

*/
fint savepar_c (fchar filename)
{
   SaveparRequest savepar = (SaveparRequest)request;
   
   savepar->code = SAVEPAR;

   char2str(filename,savepar->filename,EDTLEN);
   return send_req((UnitedRequest*)savepar,sizeof(*savepar));
}

/*
#>            userint.dc2
Function:     USERINT

Purpose:      User input interface routine for integers.

Category:     USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Use:         INTEGER USERINT( ARRAY,        Output      integer array
                              NMAX,         Input       integer
                              DEFAULT,      Input       integer
                              KEY,          Input       character
                              MES )         Input       character

              USERINT   Number of integers entered by the user.
              ARRAY     array, contains output.
              NMAX      maximum number of integers to return.
              DEFAULT   default code ( 0: no default, 1: default,
                        2: hidden, 4: exact number).
              KEY       keyword prompt.
              MES       message for user.
              
Updates:      Jul 25, 1988: KGB, Creation date.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Apr 17, 1991: JPT, reply buffer check added.
              Oct 15, 1993: JPT, removed reply buffer limitation.
#<

@ integer function userint( integer,
@                           integer,
@                           integer,
@                           character,
@                           character )

*/

fint userint_c( fint *array,     /* array with return values */
                fint *nmax,      /* maximum number of return values */
                fint *dflt,      /* HERMES default code */
                fchar key,       /* keyword */
                fchar mes )      /* message */
{
   char *replybuf=(char*)array;
  
   userinp->code   = USERINP;
   userinp->type   = USERINT;
   userinp->level  = *dflt;
   userinp->number = *nmax;
   (void)key2str(key,userinp->key,KEYLEN);
   (void)char2str(mes,userinp->message,MSGLEN);
   UISSUE(userinp)
   GETREPLY((*nmax)*sizeof(fint))
   return lenrcv/sizeof(fint);
}
/*
#>            userreal.dc2
Function:     USERREAL

Purpose:      User input interface routine for reals.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER USERREAL( ARRAY,        Output      real array
                                NMAX,         Input       integer
                                DEFAULT,      Input       integer
                                KEY,          Input       character
                                MES )         Input       character

              USERREAL  Number of reals entered by the user.
              ARRAY     array, contains output.
              NMAX      maximum number of reals to return.
              DEFAULT   default code ( 0: no default, 1: default,
                        2: hidden, 4: exact number).
              KEY       keyword prompt.
              MES       message for user.


Updates:      Jul 25, 1988: KGB, Creation date.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Apr 17, 1991: JPT, reply buffer check added
              Oct 15, 1993: JPT, removed reply buffer limitation.
#<

@ integer function userreal( real,
@                            integer,
@                            integer,
@                            character,
@                            character )

*/

fint userreal_c( float *array,   /* array with return values */
                 fint *nmax,     /* maximum number of return values */
                 fint *dflt,     /* HERMES default code */
                 fchar key,      /* keyword */
                 fchar mes )     /* message */
{
   char *replybuf=(char*)array;
  
   userinp->code   = USERINP;
   userinp->type   = USERREAL;
   userinp->level  = *dflt;
   userinp->number = *nmax;
   (void)key2str(key,userinp->key,KEYLEN);
   (void)char2str(mes,userinp->message,MSGLEN);
   UISSUE(userinp)
   GETREPLY((*nmax)*sizeof(float))
   return lenrcv/sizeof(float);
}

/*
#>            userdble.dc2
Function:     USERDBLE

Purpose:      User input interface routine for doubles.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER USERDBLE( ARRAY,        Output      array of doubles
                                NMAX,         Input       integer
                                DEFAULT,      Input       integer
                                KEY,          Input       character
                                MES )         Input       character

              USERDBLE  Number of doubles entered by the user.
              ARRAY     array, contains output.
              NMAX      maximum number of doubles to return.
              DEFAULT   default code ( 0: no default, 1: default,
                        2: hidden, 4: exact number).
              KEY       keyword prompt.
              MES       message for user.

Updates:      Jul 25, 1988: KGB, Creation date.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Apr 17, 1991: JPT, reply buffer check added
              Oct 15, 1993: JPT, removed reply buffer limitation.
#<

@ integer function userdble( double precision,
@                            integer,
@                            integer,
@                            character,
@                            character )

*/

fint userdble_c( double *array,  /* array with return values */
                 fint *nmax,     /* maximum number of return values */
                 fint *dflt,     /* HERMES default code */
                 fchar key,      /* keyword */
                 fchar mes )     /* message */
{
   char *replybuf=(char*)array;
   
   userinp->code   = USERINP;
   userinp->type   = USERDBLE;
   userinp->level  = *dflt;
   userinp->number = *nmax;
   (void)key2str(key,userinp->key,KEYLEN);
   (void)char2str(mes,userinp->message,MSGLEN);
   UISSUE(userinp)
   GETREPLY((*nmax)*sizeof(double))
   return lenrcv/sizeof(double);
}

/*
#>            userlog.dc2
Function:     USERLOG

Purpose:      User input interface routine for logicals.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER USERLOG( ARRAY,        Output      logical array
                               NMAX,         Input       integer
                               DEFAULT,      Input       integer
                               KEY,          Input       character
                               MES )         Input       character

              USERLOG   number of logicals entered by the user.
              ARRAY     array, contains output.
              NMAX      maximum number of logicals to return.
              DEFAULT   default code ( 0: no default, 1: default,
                        2: hidden, 4: exact number).
              KEY       keyword prompt.
              MES       message for user.


Updates:      Jul 25, 1988: KGB, Creation date.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Apr 17, 1991: JPT, reply buffer check added
              Oct 15, 1993: JPT, removed reply buffer limitation.
#<

@ integer function userlog( logical,
@                           integer,
@                           integer,
@                           character,
@                           character )

*/

fint userlog_c( bool *array,    /* array with return values */
                fint *nmax,     /* maximum number of return values */
                fint *dflt,     /* HERMES default code */
                fchar key,      /* keyword */
                fchar mes )     /* message */
{
   char *replybuf=(char*)array;
   
   userinp->code   = USERINP;
   userinp->type   = USERLOG;
   userinp->level  = *dflt;
   userinp->number = *nmax;
   (void)key2str(key,userinp->key,KEYLEN);
   (void)char2str(mes,userinp->message,MSGLEN);
   UISSUE(userinp)
   GETREPLY((*nmax)*sizeof(bool))
   return lenrcv/sizeof(bool);
}

/*
#>            userchar.dc2
Function:     USERCHAR

Purpose:      User input interface routine for character strings.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER USERCHAR( ARRAY,        Output      array of characters
                                NMAX,         Input       integer
                                DEFAULT,      Input       integer
                                KEY,          Input       character
                                MES )         Input       character

              USERCHAR  number of character strings entered by the user.
              ARRAY     array, contains output.
              NMAX      maximum number of strings to return.
              DEFAULT   default code ( 0: no default, 1: default,
                        2: hidden, 4: exact number).
              KEY       keyword prompt.
              MES       message for user.

Updates:      Jul 25, 1988: KGB, Creation date.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Apr 17, 1991: JPT, reply buffer check added
              Oct 15, 1993: JPT, removed reply buffer limitation.
#<

@ integer function userchar( character,
@                            integer,
@                            integer,
@                            character,
@                            character )

*/

fint userchar_c( fchar array,    /* array with return values */
                 fint *nmax,     /* maximum number of return values */
                 fint *dflt,     /* HERMES default code */
                 fchar key,      /* keyword */
                 fchar mes )     /* message */
{
   char *replybuf=array.a;
   
   userinp->code   = USERINP;
   userinp->type   = USERCHAR;
   userinp->level  = *dflt;
   userinp->number = *nmax;
   userinp->width  = array.l;
   (void)key2str(key,userinp->key,KEYLEN);
   (void)char2str(mes,userinp->message,MSGLEN);
   UISSUE(userinp)
   GETREPLY((*nmax)*array.l)
   return lenrcv/array.l;
}

/*
#>            usercharu.dc2
Function:     USERCHARU

Purpose:      User input interface routine for character strings. All
              alphabetic characters are in uppercase.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER USERCHARU( ARRAY,        Output      array of characters
                                 NMAX,         Input       integer
                                 DEFAULT,      Input       integer
                                 KEY,          Input       character
                                 MES )         Input       character

              USERCHARU number of character strings entered by the user.
              ARRAY     array, contains output.
              NMAX      maximum number of strings to return.
              DEFAULT   default code ( 0: no default, 1: default,
                        2: hidden, 4: exact number).
              KEY       keyword prompt.
              MES       message for user.

Updates:      Jul 31, 1989: KGB, Creation date.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Apr 17, 1991: JPT, reply buffer check added
              Oct 15, 1993: JPT, removed reply buffer limitation.
#<

@ integer function usercharu( character,
@                             integer,
@                             integer,
@                             character,
@                             character )

*/

fint usercharu_c( fchar array,    /* array with return values */
                  fint *nmax,     /* maximum number of return values */
                  fint *dflt,     /* HERMES default code */
                  fchar key,      /* keyword */
                  fchar mes )     /* message */
{
   char *replybuf=array.a;
   
   userinp->code   = USERINP;
   userinp->type   = USERCHARU;
   userinp->level  = *dflt;
   userinp->number = *nmax;
   userinp->width  = array.l;
   (void)key2str(key,userinp->key,KEYLEN);
   (void)char2str(mes,userinp->message,MSGLEN);
   UISSUE(userinp)
   GETREPLY((*nmax)*array.l)
   return lenrcv/array.l;
}

/*
#>            usercharl.dc2
Function:     USERCHARL

Purpose:      User input interface routine for character strings. All
              alphabetic characters are in lower case.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER USERCHARL( ARRAY,        Output      array of characters
                                 NMAX,         Input       integer
                                 DEFAULT,      Input       integer
                                 KEY,          Input       character
                                 MES )         Input       character

              USERCHARL number of character strings entered by the user.
              ARRAY     array, contains output.
              NMAX      maximum number of strings to return.
              DEFAULT   default code ( 0: no default, 1: default,
                        2: hidden, 4: exact number).
              KEY       keyword prompt.
              MES       message for user.

Updates:      Jul 31, 1989: KGB, Creation date.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Apr 17, 1991: JPT, reply buffer check added
              Oct 15, 1993: JPT, removed reply buffer limitation.
#<

@ integer function usercharl( character,
@                             integer,
@                             integer,
@                             character,
@                             character )

*/

fint usercharl_c( fchar array,    /* array with return values */
                  fint *nmax,     /* maximum number of return values */
                  fint *dflt,     /* HERMES default code */
                  fchar key,      /* keyword */
                  fchar mes )     /* message */
{
   char *replybuf=array.a;
   
   userinp->code   = USERINP;
   userinp->type   = USERCHARL;
   userinp->level  = *dflt;
   userinp->number = *nmax;
   userinp->width  = array.l;
   (void)key2str(key,userinp->key,KEYLEN);
   (void)char2str(mes,userinp->message,MSGLEN);
   UISSUE(userinp)
   GETREPLY((*nmax)*array.l)
   return lenrcv/array.l;
}

/*
#>            usertext.dc2
Function:     USERTEXT

Purpose:      User input interface routine for characters.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Declaration:  INTEGER USERTEXT

Use:          INTEGER USERTEXT( ARRAY,        Output      character array
                                DEFAULT,      Input       integer
                                KEY,          Input       character
                                MES )         Input       character

              USERTEXT  number of characters entered by the user.
              ARRAY     array, contains output.
              DEFAULT   default code ( 0: no default, 1: default,
                        2: hidden, 4: exact number, -1: special, see below).
              KEY       keyword prompt.
              MES       message for user.

Special:      Event-driven tasks can call USERTEXT with DEFAULT=-1.
              In this case the following information can be obtained
              from Hermes, depending on the supplied keyword:

              UCAMESSAGE= obtains the last User Command Area error message,
                          usually a message associated with a rejected keyword;
              REJECTED=   the input text associated with a rejected keyword.
              LSTMESSAGE= the last status message issued by a deputy task.
               
              In order to obtain reliable information from these keywords,
              the call must take place immediately after the detection of
              the problem at hand.

Updates:      Jul 25, 1988: KGB, Creation date.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Sep 17, 1990: JPT, Bug in default handling repaired.
              Apr 17, 1991: JPT, reply buffer check added
              Oct 15, 1993: JPT, removed reply buffer limitation.
              Nov 25, 1997: JPT, documented special operation.
              Jun 10, 1998: JPT, documented LSTMESSAGE=.
#<

@ integer function usertext( character,
@                            integer,
@                            character,
@                            character )

*/

fint usertext_c( fchar array,   /* array with return values */
                 fint *dflt,    /* HERMES default code */
                 fchar key,     /* keyword */
                 fchar mes )    /* message */
{
   char *replybuf=array.a;
   int  i;
   
   userinp->code   = USERINP;
   userinp->type   = USERTEXT;
   userinp->level  = *dflt;
   userinp->number = array.l;
   (void)key2str(key,userinp->key,KEYLEN);
   (void)char2str(mes,userinp->message,MSGLEN);
   UISSUE(userinp)
   GETREPLY(array.l)
   if (lenrcv>0) for (i=lenrcv; i<array.l; i++) array.a[i] = ' ';
   return lenrcv;
}

/*
#>            anyout.dc2
Subroutine:   ANYOUT

Purpose:      General character output routine for GIPSY programs.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL ANYOUT( LDEV,        Input      integer
                           TEXT )       Output     character

              LDEV   Device to which output is directed.
                     The following devices are possible:
                      0  use default [set by HERMES to 3
                         but can be changed by user]
                      1  terminal
                      2  LOG file (=line printer)
                      8  terminal, suppressed in "experienced mode"
                     16  terminal, only when in "test mode"
                     These devices can be combined by adding the codes.
                     For terminal device the max. line length is 80
                     characters, for LOG file the max. is 130 characters.
              TEXT   This character string contains the text which
                     will be sent to the output device.
                     The first character in the string is used for
                     printer(terminal) control in the normal way
                     (eg 1 for a new page)

Examples:     - Put text on terminal
                CALL ANYOUT( 1, ' THIS IS TEXT' )
              - Both to terminal and LOG FILE
                CALL ANYOUT( 3, ' TWO FILES OUTPUT' )
              - Put text on default device and start a new page
                CALL ANYOUT( 0, '1THIS IS A NEW PAGE')
              - Prepare the array TEXT before calling
                CHARACTER*40 TEXT
                      .
                      .
                      .
                WRITE( TEXT, '('' Values for I and J are:'',2I3)') I, J
                CALL ANYOUT( 0, LINE )

Updates:      ~1979: original document
              Mar  6, 1989: KGB, New document.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
#<

@ subroutine anyout( integer, character )

*/

void anyout_c( fint *device, fchar string )
{
   AnyoutRequest anyout = (AnyoutRequest)request;

   anyout->code = ANYOUT;
   anyout->devmask = *device;
   (void)char2str(string,anyout->line,LINLEN);
   ISSUE(anyout)
}

/*
#>           listctrl.dc2
Function:    LISTCTRL

Purpose:     Change and report Hermes' character output device state

Category:    SYSTEM, USER-INTERFACE 

File:        srvreq.c

Author:      J.P. Terlouw

Use:         INTEGER LISTCTRL ( NEW )

             LISTCTRL : device state prior to the call.
             NEW      : device state after the call. (INTEGER, input).
             
             The following device states are possible:
                0  both terminal and log file off;
                1  terminal on;
                2  log file on;
                3  both terminal and log file on.
             If an invalid (e.g. negative) device state is given,
             the function only reports the current device state.
             
             The NEW state is propagated to the tasks which activated the
             calling task (if any).
             
Examples:    Because the state is propagated to any caller task, this function 
             can be used to control output in a COLA script.
             This function can also be used to switch off unwanted terminal
             output which could be generated by a called subroutine over which
             the programmer has no control. 

Updates:     Jan  3, 1994: original document.
#<

@ integer function listctrl ( integer )

*/
              
fint listctrl_c(fint *newdev)
{
   ListctrlRequest listctrl = (ListctrlRequest) request;
    
   listctrl->code = LISTCTRL;
   listctrl->mask = *newdev;
   return (fint)send_req((UnitedRequest*)listctrl,sizeof(*listctrl));
}

/*
#>            error.dc2
Subroutine:   ERROR

Purpose:      User error handling routine.

Category:     SYSTEM, USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL ERROR( IER,          Input        integer
                          MESSAGE )     Input        character

              IER       Severity of error.   1 = Warning
                                             2 = Minor error
                                             3 = Serious error
                                             4 = Fatal error
              MESSAGE   Character string containing message for the
                        user.

Description:  The user is allowed to set the value of the MESSAGELEVEL
              and the ERRORLEVEL. The action taken depends on the
              severity of the error in relation to these levels.
              If IER >= ERRORLEVEL then the error is fatal and the
              task is aborted.
              If IER >= MESSAGELEVEL then the message is given to the
              user.
              Default levels: ERRORLEVEL=4, MESSAGELEVEL=1.

Examples:     With default levels IER=1,2,3 gives message but no abort.
              IER=4 gives message and aborts task
              By setting MESSAGELEVEL=2  The user or applications
              programer could suppress the error messages for IER=1
              conditions

Updates:      1978: original document
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
#<

@ subroutine error( integer, character )

*/

void error_c( fint *level, fchar string )
{
   ErrorRequest error = (ErrorRequest)request;
   
   error->code  = ERROR;
   error->level = *level;
   char2str(string,error->message,MSGLEN);
   if (send_req((UnitedRequest*)error,sizeof(*error))) {
      Cleanup();
      exit(1);
   }
}

/*
#>            aborttask.dc2
Function:     ABORTTASK

Purpose:      Abort a task

Category:     SYSTEM, USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER ABORTTASK( TASKNAME )     Input        character

              ABORTTASK   0 = Success
                         -1 = Error, i.e. task not known

              TASKNAME   name of the task to be aborted.

Description:  This function aborts a running task, given its taskname.
              Its main purpose is to allow be a task that has started another
              task with XEQCONT, to abort that other task. In this case the
              first task can obtain the taskname via a prefixed keyword
              provided by XEQCONT.

Related document:
              xeqcont.dc2

Updates:      Apr 15, 2011: original document
#<

@ integer function aborttask( character )

*/

fint aborttask_c( fchar string )
{
   AbortRequest abort = (AbortRequest)request;
   
   abort->code  = ABORT;
   char2str(string,abort->taskname,TSKLEN);
   return send_req((UnitedRequest*)abort,sizeof(*abort));
}

/*
#>            pause.dc2
Subroutine:   PAUSE

Purpose:      To suspend a GIPSY task.

Category:     SYSTEM, USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL PAUSE( TEXT )     Input     character

              TEXT     TEXT will be displayed in the task in the
                       status area on the terminal screen.

Example:      In this example three values are obtained by the
              servant task of which the combination must follow
              certain restrictions.

              .
              .
              .
              WHILE (.TRUE.)                    infinite loop
                 A = USERINT( .... )
                 B = USERINT( .... )
                 C = USERINT( .... )
                 IF combination of A, B, and C is valid
                 THEN
                    XWHILE
                 ELSE
                    CALL PAUSE('INVALID COMB. OF A, B AND C')
                    at this stage the user can correct any
                    input. Other inputs are kept. Then the user
                    can resume the task by typing CTRL-G.
                 CIF
              CWHILE
              .
              .
              .

Updates:      Jul 10, 1979: original document
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.      

#<

@ subroutine pause( character )

*/
void pause_c(fchar message)
{
   PauseRequest pause = (PauseRequest)request;
   
   pause->code = PAUSE;
   (void)char2str(message,pause->message,MSGLEN);
   ISSUE(pause)
}

/*
#>            cancel.dc2
Subroutine:   CANCEL

Purpose:      Remove user input from table maintained by HERMES.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL CANCEL( KEY )        Input      character

              KEY      An ASCII string containing the keyword
                       (including = sign) of which the associated
                       values will be removed.

Examples:     Here the servant task checks on the validity of the
              input and repeats the call to obtain the input if it
              was invalid.

              .
              WHILE (.TRUE.)                      infinite loop
                 NEL = USRREAL( VALUE,..., 'V=', 'TYPE V' )
                 IF (VALUE is valid)
                 THEN
                    XWHILE            leave loop
                 ELSE
                    CALL CANCEL( 'V=' )
                 CIF
              CWHILE
              .

              In the next example the servant task performs an
              infinite loop and asks the user whether it should go
              through this loop another time.

              LOGICAL AGAIN
              .
              WHILE .TRUE.                  infinite loop
                 .
                 .
                 NEL = USERLOG( AGAIN, ..., 'AGAIN=', ...)
                 IF (AGAIN)
                 THEN
                    CALL CANCEL( 'AGAIN=' )
                 ELSE
                    XWHILE
                 CIF
              CWHILE
              .
              .

Updates:      Mar  9, 1979: original document
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.      

#<

@ subroutine cancel( character )

*/

void cancel_c( fchar key )
{
   CancelRequest cancel = (CancelRequest)request;
   
   cancel->code = CANCEL;
   key2str(key,cancel->key,KEYLEN);
   ISSUE(cancel)
}

/*
#>            reject.dc2
Subroutine:   REJECT

Purpose:      Reject user input.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL REJECT( KEY, MESSAGE)

              KEY     The userinp keyword of which the values are to
                      be rejected. Type CHARACTER.

              MESSAGE informs the user about the rejection. (CHARACTER)
              
Example:      Here the servant task checks on the validity of the
              input and repeats the call to obtain the input if it
              was invalid.
              
              .
              WHILE (.TRUE.)                      infinite loop
                 NEL = USRREAL( VALUE,..., 'V=', 'TYPE V' )
                 IF (VALUE is valid)
                 THEN
                    XWHILE            leave loop
                 ELSE
                    CALL REJECT( 'V=', 'Invalid value' )
                 CIF
              CWHILE
              .
              
Updates:      Dec 17, 1990: JPT, document created.
#<

@ subroutine reject( character, character )

*/

void reject_c(fchar key, fchar message)
{
   RejectRequest reject = (RejectRequest)request;
    
   reject->code = REJECT;
   key2str(key,reject->key,KEYLEN);
   char2str(message,reject->message,REJLEN);
   ISSUE(reject)
}

/*
#>            xeq.dc2
Subroutine:   XEQ

Purpose:      Send a task start command to HERMES.

Category:     SYSTEM, USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL XEQ( STRING,         Input     character
                        IRC )           Output    integer

              STRING    Character variable containing the HERMES task
                        start command.
                        Example:  CLEAR,MEM=1
              IRC       Return code containing information about the
                        fate of the command.
                          1      successful execution
                         -1      Hermes refused to execute command because
                                 of sytax error, too many tasks active, etc.
                         -2      task exited before calling INIT
                         -3      fatal execution error (i.e. CALL ERROR with
                                 a level at or above current error level)
                         -4      task crashed
                         -5      user abort

Related docs: DEPUTY.DC2,  XEQXIT.DC2

Updates:      Aug  8, 1979: JPT, Original document.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Dec  2, 1991: JPT, Return codes revised.
#<

@ subroutine xeq( character, integer )

*/

void xeq_c( fchar string, fint *errorcode )
{
   XeqRequest xeq = (XeqRequest)request;
   
   xeq->code = XEQ;
   char2str(string,xeq->command,CMDLEN);
   *errorcode = send_req((UnitedRequest*)xeq,sizeof(*xeq));
}

/*
#>            xeqcont.dc2
Function:     XEQCONT

Purpose:      Send a task start command to HERMES and continue.

Category:     SYSTEM, USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER XEQCONT( KEY,              Input     character
                               COMMAND )         Input     character


              XEQCONT   Status return:
                          0      command executed
                         -1      Hermes refused to execute command because
                                 of sytax error, too many tasks active, etc.

              KEY       Keyword to which integer task exit status is written:
                          1      successful execution
                         -2      task exited before calling INIT
                         -3      fatal execution error (i.e. CALL ERROR with
                                 a level at or above current error level)
                         -4      task crashed
                         -5      user abort
                        If keyword is specified as "*" (asterisk), the called
                        task will run independently and no status will be
                        written to the calling task.
                         
              COMMAND   Character variable containing the Hermes task
                        start command.

Description:  XEQCONT allows a task to initiate another task without waiting
              for its completion. When the second task has exited, Hermes
              will write the task exit status to the keyword, which then can
              be read by the calling task. From this keyword also other
              keywords are derived which are used to report about the second
              task's status.
              
              N_<key> : provides the second task's taskname.
              
              S_<key> : provides any status message generated by or on
                        behalf of the second task.
            
              When the second task needs input to a keyword, keywords with
              the following prefixes will be set for the calling task:

              K_<key> : the requested keyword
              M_<key> : the message associated with the input request
              T_<key> : the type of the requested input:
                        1 - USERINT
                        2 - USERLOG
                        3 - USERREAL
                        4 - USERDBLE
                        5 - USERCHAR
                        6 - USERCHAR
                        7 - USERCHARL
                        8 - USERTEXT
              R_<key> : the rejection message for a keyword that was rejected
              
              If the calling task exits before the called task, the latter
              will be aborted, except when it has been started as an
              independent task using "*" as the keyword argument.

Related docs: deputy.dc2,  xeq.dc2

Updates:      Jan 17, 2001: JPT, Original document.
              Apr 16, 2011: JPT, Described prefixed keywords.
              May 18, 2012: JPT, Documented independent start-up.
#<

@ integer function xeqcont( character, character)

*/

fint xeqcont_c( fchar key, fchar command)
{
   XeqcontRequest xeqcont = (XeqcontRequest)request;
    
   xeqcont->code = XEQCONT;
   char2str(key,     xeqcont->key,     KEYLEN);
   char2str(command, xeqcont->command, CMDLEN);
   return send_req((UnitedRequest*)xeqcont,sizeof(*xeqcont));
}

/*
#>            type.dc2
Subroutine:   TYPE

Purpose:      To "type" into the HERMES User Type-in Area.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL TYPE( TEXT )           Input      character

              TEXT      Character string containing text to be typed.

Updates:      Aug 10, 1979: JPT, Document created.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.

#<

@ subroutine type( character )

*/

void type_c( fchar string )
{
   TypeRequest type = (TypeRequest)request;
   
   type->code = TYPE;
   char2str(string,type->string,TYPLEN);
   ISSUE(type)   
}

/*
#>            status.dc2
Subroutine:   STATUS

Purpose:      To display additional information in the "RUNNING" status
              display.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL STATUS( TEXT )       Input       integer

              TEXT         TEXT will be displayed in the task
                           status area on the terminal screen.

Updates:      Aug 20, 1980: JPT, Document created.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.

#<

@ subroutine status( character )

*/

void status_c( fchar string )
{
   StatusRequest status = (StatusRequest)request;
   static char name[TSKLEN+1]="";
   
   status->code = STATUS;
   char2str(string,status->message,MSGLEN);
   ISSUE(status)
   if (status_callback) {
      char message[TSKLEN+MSGLEN+3+1];
      if (!(*name)) {
         char *replybuf=name;
         request->code = MYNAME;
         ISSUE(request)
         GETREPLY(TSKLEN)
         name[lenrcv] = '\0';
      }
      strcpy(message, name);
      strcat(message, " - ");
      char2str(string, message+strlen(message), MSGLEN);
      status_callback(message);
   }
}

/*
#>            wkey.dc2
Subroutine:   WKEY

Purpose:      Write keywords to a task parameter list

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL WKEY( TEXT )    Input,   character

              TEXT     A character string containing keywords and
                       values, optionally prefixed by a taskname

Description:  This routine writes keyword-value pairs to the parameter
              list of the calling task or, if the name of a different
              active task is specified, to the parameter list of that task.

              Writing to the own parameter list is the most common.
              It can be used to pass parameter information back and forth
              between members of a chain of tasks initiated via calls to
              DEPUTY. It is also used in event-driven programs in
              connection the GIPSY's graphical user interface (Ggi).

              Writing to an other task's parameter list can be useful when
              one task needs to influence an other task. For example a
              processing task could cause a display task to display
              the processed data. If the addressed task is event-driven,
              the calling task will wait until the addressed task has read
              all keywords written with this call or until the addressed
              task exits.
              
              In case of error, no action is performed and WKEY just returns.

Examples:     a) "bare" call:
                 CALL WKEY('POS=PC, SET=0,1 PRINT=YES')

              b) Suppose TASK1 computes the value of a parameter PARM1
                 which is needed as input to TASK2 which is called via
                 DEPUTY. Then in TASK1 the programmer codes:

                 PROGRAM TASK1
                 CHARACTER*20 STRING
                 .
                 .
                 PARM1=...
                 WRITE(STRING,'(''KEY1='',F10.4)') PARM1
                 CALL WKEY(STRING)
                 CALL DEPUTY('TASK2',IER)
                 .
                 .
                 END

                 The task receiving the keyword could be coded as
                 follows:

                 PROGRAM TASK2
                 .
                 .
                 N = USERREAL(VALUE,1,0,'KEY1=','Value, please')
                 .
                 .
                 .
                 END

                 Note that TASK2 calls USRINP in a standard way and
                 that it is unaware of the fact that the keyword has
                 been prepared by its caller.

              c) In a similar way it is also possible that a spawned
                 task prepares keywords for its caller:

                 PROGRAM TASK1
                 .
                 .
                 CALL DEPUTY('TASK2',IER)
                 N = USERINT(...,'KEY=',...)
                 .
                 .
                 .
                 END

                 PROGRAM TASK2
                 .
                 .
                 CALL WKEY('KEY=...')
                 .
                 .
                 END

              d) Write to other task:
                 CALL WKEY('SLICEVIEW READSUB=YES')

Related docs: DEPUTY.DC2, SUBST.DC2

Updates:      Sep  4, 1981: JPT, Original document.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Jan 16, 2001: JPT, Implement writing to other tasks.
              Apr 14, 2011: JPT, Document changed behaviour w.r.t.
                                 event-driven tasks.
#<

@ subroutine wkey( character )

*/

void wkey_c( fchar string )
{
   WkeyRequest wkey = (WkeyRequest)request;
   
   wkey->code = WKEY;
   char2str(string,wkey->command,CMDLEN);
   ISSUE(wkey)
   if (wkey_callback) (void)wkey_callback();
}

/*
#>            subst.dc2
Subroutine:   SUBST

Purpose:      Specify keyword substitution for a task which will be
              spawned via a call to DEPUTY.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL SUBST( STRING,     Input     character
                          IRC )       Output    integer

              STRING    Character string containing keyword pairs.
                        The first member of the pair is the keyword as
                        referenced by the spawned task; the second is
                        the keyword as referenced by the calling task.
                        Keywords in the string must not be separated by
                        any separator (i.e. the string must only contain
                        keywords).
              IRC       error return code
                         1      no error
                        -1      odd number of keywords in string
                        -2      syntax error in keyword
                        -3      insufficient room for string

Notes:        If more than one keyword substitution is needed, they
              can be specified in one call or in a sequence of calls
              (see example b.)
              The mechanism is quite general and can also  be used
              in tasks which have been spawned themselves.
              If necessary, substitutions will be chained.
              After the call to DEPUTY, the substitution specification
              is removed.

Examples:     a) Suppose TASK1 processes a set and produces an output
              set of which the setnumbers are obtained from the
              keywords SET= and SETOUT= respectively. Also suppose that
              TASK1 requires the service of TASK2 to obtain information
              about the output set and that TASK2 uses the keyword SET=
              to obtain the setnumber. TASK1 and TASK2 then could be
              coded as follows:

              PROGRAM TASK1
              .
              .
              N = USERINT(...,'SET=',...)
              N = USERINT(...,'SETOUT=',...)
              .
              .
              CALL SUBST('SET=SETOUT=',IRC1)
              CALL DEPUTY('TASK2',IRC2)
              .
              .
              END

              PROGRAM TASK2
              .
              .
              N = USERINT(...,'SET=',...)
              .
              .
              END

              The call to USRINP from TASK2 obtains the setnumber
              associated with SETOUT=.  If the keyword isn't present
              in the keyword list, this call will prompt with
              "TASK1,SETOUT=".

              b) More than one substitution is allowed:
              CALL SUBST('SET=SETOUT=BOX=AREA=',IRC)
              This is equivalent to the following sequence:
              CALL SUBST('SET=SETOUT=',IRC)
              CALL SUBST('BOX=AREA=',IRC)

Related docs: DEPUTY.DC2  WKEY.DC2

Updates:      Oct 12, 1981: JPT Original document.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
#<

@ subroutine subst( character, integer )

*/

void subst_c( fchar string, fint *errorcode )
{
   SubstRequest subst = (SubstRequest)request;
   
   subst->code = SUBST;
   char2str(string,subst->string,SUBLEN);
   *errorcode = send_req((UnitedRequest*)subst,sizeof(*subst));
}

/*
#>            canall.dc2
Subroutine:   CANALL

Purpose:      Remove all user input from table maintained by HERMES.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL CANALL

Updates:      Nov  9, 1987: JPT, original document.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
#<

@ subroutine canall()

*/

void canall_c( void )
{
   request->code = CANALL;
   ISSUE(request)
}

/*
#>            xeqxit.dc2
Subroutine:   XEQXIT

Purpose:      Send a task start command to HERMES and exit.

Category:     USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL XEQXIT( STRING )    Input    character

              STRING     Character string containing the HERMES task
                         start command.

Example:      CALL XEQXIT( 'CLEAR, MEM=1' )

Related docs: XEQ.DC2

Updates:      Jun 16, 1980: JPT, Original document.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.

#<

@ subroutine xeqxit( character )

*/

void xeqxit_c( fchar string )
{
   XeqRequest xeqxit = (XeqRequest)request;
   
   xeqxit->code = XEQXIT;
   char2str(string,xeqxit->command,CMDLEN);
   ISSUE(xeqxit)
}

/*
#>            deputy.dc2
Subroutine:   DEPUTY

Purpose:      Start a task which temporarily assumes the role of the
              calling task.

Category:     SYSTEM, USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CALL DEPUTY( STRING,   Input    character
                           IRC )     Output   integer

              STRING  character string containing a servant task name.
              IRC     return code containing information about the
                      fate of the command.
                        1      successful execution
                       -6      task not present
                       -7      max number of tasks already active

                      User-initiated aborts, crashes and fatal calls
                      to ERROR cause a non event-driven task
                      to exit. In the latter case the caller
                      never regains control.
                      Event-driven tasks always regain control.
                      For these the following codes are also defined:
                       -8      fatal error
                       -9      user abort
                      -10      crash
                      -11      aborted due to no-default user input request

Description:  After a call to DEPUTY the calling task is suspended
              until the called task is finished.
              The same set of keywords applies to all tasks spawned
              from successive calls to deputy. The keyword table is
              common to all tasks in the chain. Both calling and
              called tasks can obtain information from the
              keyword list using USRINP and provide and/or modify
              keywords using CANCEL or WKEY.

Limitation:   Keywords cannot be specified in the call to DEPUTY.

Example:      CALL DEPUTY('KIJK',IRC)

Related docs: WKEY.DC2  SUBST.DC2  XEQ.DC2

Updates:      Aug 10, 1981: JPT, Original document.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.
              Jun 10, 1998: JPT, Added event-driven task codes.
#<

@ subroutine deputy( character, integer )

*/

void deputy_c( fchar string, fint *errorcode )
{
   DeputyRequest deputy = (DeputyRequest)request;
   
   deputy->code = DEPUTY;
   char2str(string,deputy->taskname,TSKLEN);
   *errorcode = send_req((UnitedRequest*)deputy,sizeof(*deputy));
}

/*
#>            myname.dc2
Function:     MYNAME

Purpose:      Obtain the name under which a GIPSY task is being run.

Category:     SYSTEM, USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Declaration:  CHARACTER*n MYNAME

Returns:      Name of running task.
 
Use:          MYNAME()

Example:      CHARACTER*9 MYNAME,NAME
              .
              .
              NAME = MYNAME()
              IF (NAME .EQ. 'ADD')
              THEN
                 .
                 .

Updates:      Feb 26, 1986: JPT, original document.
              Feb 15, 1990: JPT, Portable version, rewritten in ANSI C.

#<

@ character function myname()

*/
void myname_c(fchar name)
{
   char *replybuf=name.a;
   int  i;
   
   request->code = MYNAME;
   ISSUE(request)
   GETREPLY(name.l)
   for (i=lenrcv; i<name.l; i++) name.a[i] = ' ';
}
/*
#>            editfile.dc2
Function:     EDITFILE

Purpose:      User action interface routine to edit a text file.

Category:     SYSTEM, USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER EDITFILE( FILENAME,   Input       character
                                MESSAGE )   Input       character

              EDITFILE   success/error return code:
                         0: success;
                        -1: user cancelled the edit;
                        -2: cannot start editor;
                        -3: editor process exited with error status.
                      -666: this version of Hermes does not support EDITFILE

              FILENAME   name of file to be edited.

              MESSAGE    message for user.

Description:  EDITFILE requests Hermes to run an editor to edit the specified
              file. Hermes then displays the message in the task status area
              and prompts the user to start the editor. At this moment the
              user has the option to start the edit operation or to cancel or
              postpone it.
              
Updates:      Nov  8, 1992: JPT, document created.
#<

@ integer function editfile( character, character )

*/

fint editfile_c(fchar filename,    /* file to be edited */
                fchar message )    /* message */
{
   EditfileRequest editfile = (EditfileRequest)request;
    
   editfile->code = EDITFILE;
   char2str(filename,editfile->filename,EDTLEN);
   char2str(message,editfile->message,MSGLEN);
   return send_req((UnitedRequest*)editfile,sizeof(*editfile));
}

/*
#>            decodeint.dc2
Function:     DECODEINT

Purpose:      Hermes interface routine for decoding integers.

Category:     SYSTEM, USER-INTERFACE 

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER DECODEINT ( EXPR,       Input    CHARACTER*(*)
                                  OUTV,       Output   INTEGER ARRAY
                                  NOUT )      Input    INTEGER
                                  
              DECODEINT   Returns number of integers actually decoded or
                          a negative error code.
              EXPR        Character string containing the expressions to
                          be evaluated.
              OUTV        Array of integers obtaining the decoded values.

Notes:        - The syntax of the input for this routine is described
                in document input.dc2.
              - Variables defined in Hermes are also available through
                this function.

Updates:      Jul 21, 1994: JPT, document created.
#<

@ integer function decodeint( character, integer, integer )

*/

fint decodeint_c( fchar expr, fint *outv, fint *nout)
{
   DecodeRequest decode = (DecodeRequest)request;
   char *replybuf=(char*)outv;
   fint result;
   int  i;
    
   if (expr.l >= DCDLEN) return -999;
   decode->code =   DECODE;
   decode->type =   USERINT;
   decode->number = *nout;
   (void)char2str(expr,decode->expr,DCDLEN);
   for (i=strlen(decode->expr)-1; i; i--) {
      if (decode->expr[i]==' ') decode->expr[i] = '\0'; else break;
   }
   result = send_req((UnitedRequest*)decode,sizeof(*decode));
   if (result>0) {
      GETREPLY((result*sizeof(fint)))
   }
   return result;
}
/*
#>            decodereal.dc2
Function:     DECODEREAL

Purpose:      Hermes interface routine for decoding reals.

Category:     SYSTEM, USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER DECODEREAL( EXPR,       Input    CHARACTER*(*)
                                  OUTV,       Output   REAL ARRAY
                                  NOUT )      Input    INTEGER
                                  
              DECODEREAL  Returns number of reals actually decoded or
                          a negative error code.
              EXPR        Character string containing the expressions to
                          be evaluated.
              OUTV        Array of reals obtaining the decoded values.

Notes:        - The syntax of the input for this routine is described
                in document input.dc2.
              - Variables defined in Hermes are also available through
                this function.

Updates:      Jul 21, 1994: JPT, document created.
#<

@ integer function decodereal( character, real, integer )

*/

fint decodereal_c( fchar expr, float *outv, fint *nout)
{
   DecodeRequest decode = (DecodeRequest)request;
   char *replybuf=(char*)outv;
   fint result;
   int  i;
    
   if (expr.l >= DCDLEN) return -999;
   decode->code =   DECODE;
   decode->type =   USERREAL;
   decode->number = *nout;
   (void)char2str(expr,decode->expr,DCDLEN);
   for (i=strlen(decode->expr)-1; i; i--) {
      if (decode->expr[i]==' ') decode->expr[i] = '\0'; else break;
   }
   result = send_req((UnitedRequest*)decode,sizeof(*decode));
   if (result>0) {
      GETREPLY((result*sizeof(float)))
   }
   return result;
}

/*
#>            decodedble.dc2
Function:     DECODEDBLE

Purpose:      Hermes interface routine for decoding doubles.

Category:     SYSTEM, USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Use:          INTEGER DECODEDBLE( EXPR,       Input    CHARACTER*(*)
                                  OUTV,       Output   DOUBLE ARRAY
                                  NOUT )      Input    INTEGER
                                  
              DECODEDBLE  Returns number of doubles actually decoded or
                          a negative error code.
              EXPR        Character string containing the expressions to
                          be evaluated.
              OUTV        Array of doubles obtaining the decoded values.

Notes:        - The syntax of the input for this routine is described
                in document input.dc2.
              - Variables defined in Hermes are also available through
                this function.

Updates:      Jul 21, 1994: JPT, document created.
#<

@ integer function decodedble( character, double precision, integer )

*/

fint decodedble_c( fchar expr, float *outv, fint *nout)
{
   DecodeRequest decode = (DecodeRequest)request;
   char *replybuf=(char*)outv;
   fint result;
   int  i;
    
   if (expr.l >= DCDLEN) return -999;
   decode->code =   DECODE;
   decode->type =   USERDBLE;
   decode->number = *nout;
   (void)char2str(expr,decode->expr,DCDLEN);
   for (i=strlen(decode->expr)-1; i; i--) {
      if (decode->expr[i]==' ') decode->expr[i] = '\0'; else break;
   }
   result = send_req((UnitedRequest*)decode,sizeof(*decode));
   if (result>0) {
      GETREPLY((result*sizeof(double)))
   }
   return result;
}

/*
#>            dcderrstr.dc2
Function:     DCDERRSTR

Purpose:      Obtain an error message, given a DECODExxx error code.

Category:     SYSTEM, USER-INTERFACE

File:         srvreq.c

Author:       J.P. Terlouw

Use:          CHARACTER DCDERRSTR( 
                                   CODE )     Input INTEGER

              DCDERRSTR  Returns the error message
              CODE       The error code as returned by DECODEDBLE ,DECODEINT
                         or DECODEREAL.

Updates:      Dec  8, 1994: JPT, document created.
#<

@ character function dcderrstr(integer)

*/

void dcderrstr_c(fchar message, fint *code)
{
   DcderrRequest dcderr = (DcderrRequest)request;
   char *replybuf=message.a;
   int  i; 
    
   dcderr->code  = DCDERR;
   dcderr->error = *code;
   ISSUE(dcderr);
   GETREPLY(message.l)
   for (i=lenrcv; i<message.l; i++) message.a[i] = ' ';
}

/* ---------------------------------------------------------------------------

                             KEY2STR

Name:     key2str

Purpose:  copy a fchar keyword to a char[]

Use:      int key2str(fchar c, char *s, int ls)
                       c    input fchar object
                       s    output string
                       ls   length of s
                       
             The tranfer stops after the first equals sign (=).
             If a blank is encountered before an equals sign, key2str
             inserts the equals sign itself.
             If the length of c exceeds the length of s, only the first ls-1
             elements are transferred.
             The output string is closed with a zero byte.
             The function value is the number of characters transferred.
             
--------------------------------------------------------------------------- */
static int key2str(fchar c, char *s, int ls)
{  
   int nc;

   for(nc = 1 ; nc<=c.l && nc<ls-1 && *c.a != ' ' && *c.a != '=' ; nc++)  {
      *(s++) = *(c.a++);
   }
   *(s++) = '=';
   *s = 0;
   return nc+1;
}

/* ------------------------------------------------------------------------- */
static void fail (Request request, int error)
{
   printf("\nHermes service failed: \n"
          "  request code: %d\n"
          "    error code: %d\n", request->code, error);
   exit(1);
}
/* ------------------------------------------------------------------------- */
static void ufail (UserinpRequest request, int error)
{
   fint  errlev=4;
   char message[80];
   fchar errmes;
    
   switch (error) {
      case -1: {
         sprintf(message,"Userinp(%s): too many items requested",request->key);
         errmes.a = message;
         errmes.l = strlen(message);
         error_c(&errlev,errmes);
         break;
      }
      default: {
         ufailed = error; /* non-fatal failure (bad input, reject, etc.) */
         break;
      }
   }
}
/* ------------------------------------------------------------------------- */
static int send_req(UnitedRequest *request, int size)
{
   RequestMessage  message;
   int             status;
   static int      pid=0;
    
   if (!pid) pid = getpid();
   message.pid     = pid;
   message.reqlen  = size;
   message.request = request;
   (*send_msg)(&message);
   (void)(*recv_msg)((char*)&status,sizeof(int));
   return status;
}

static void NotInitialized(RequestMessage *request)
{
   fprintf(stderr,"Hermes not bound. Call INIT first.\n");
   exit(1);
}

static void InitCleanup(void)
/* Initialize, i.e. build the free list. */
{
   int i;
    
   for (i=0; i<N_ATFINIS; i++) {
      clnblk[i].next = clnfree;
      clnfree = &clnblk[i];
   }
   signal(SIGABORT,intsigh);
}
/*
    Cleanup is called by FINIS and ERROR when they decide to exit.
    It calls any callback procedures registered by atfinis.
    It should allow callback procedures to remove the registration
    itself. For that reason the code seems to be more complicated than
    needed.
*/
static void Cleanup(void)
{
   CleanBlock *current=clnregd, *next;
   
   while (current) {
      next = current->next;
      (*current->proc)(current->data);
      current = next;
   }
}

/*
 *  intsigh() handles INT signals. If an abort procedure was registered, it
 *  will be called and processing continues, otherwise it simply exits with 
 *  status code 1.
 */
static void intsigh(int dummy)
{
   if (abtblk) (*abtblk->proc)(abtblk->data); else exit(1);
}

/*
 *  hupsigh handles SIGHUP signals and translates them to SIGSTOP signals.
 *  This is done because the SIGTSTP signal which is used for pausing a task
 *  will be translated by the system to a SIGHUP if the task process is in
 *  an orphaned task group. This happens when GIPSY is started in a dedicated
 *  xterm, e.g. by the command "xterm -e ${gip_sys}/gipsy.csh &".
 */
static void hupsigh(int dummy)
{
   signal(SIGHUP, hupsigh);
   signal(SIGTSTP, hupsigh);
   kill(getpid(),SIGSTOP);
}

/* ------------------------------------------------------------------------- */
/*
#>srvreq.h
#if !defined(_srvreq_h_)
#define _srvreq_h_
#include "init.h"
#include "finis.h"
#include "allpar.h"
#include "userint.h"
#include "userreal.h"
#include "userdble.h"
#include "userlog.h"
#include "userchar.h"
#include "usercharu.h"
#include "usercharl.h"
#include "usertext.h"
#include "anyout.h"
#include "error.h"
#include "pause.h"
#include "cancel.h"
#include "reject.h"
#include "xeq.h"
#include "type.h"
#include "status.h"
#include "wkey.h"
#include "subst.h"
#include "canall.h"
#include "xeqxit.h"
#include "deputy.h"
#include "myname.h"
#include "editfile.h"
#include "listctrl.h"
#include "xeqcont.h"
#include "gipsytask.h"
#include "aborttask.h"


extern int  atfinis(void(*)(void*),void*);
extern void atfinisrm(void(*)(void*),void*);
extern void atabort(void(*)(void*),void*);
extern void status_cb(void(*)(char*));
extern void wkey_cb(int(*)(void));
extern int  notify(int mode);
extern int  keystatus(void);
#endif
#<
*/
/* ------------------------------------------------------------------------- */

