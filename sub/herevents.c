/* herevents.c
                              COPYRIGHT (c) 1997
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

Author: J.P. Terlouw
*/

#include "stddef.h"

#include <unistd.h>
#include <sys/types.h>
#if     defined(__aix__)
#include <sys/select.h>
#endif
#include <sys/time.h>

#include "stdlib.h"
#include "taskcom.h"
#include "notify.h"
#include "herevents.h"

#define New(type) ((type *)calloc(1,sizeof(type)))
#define Delete(x)  {free(x); x=NULL;}

typedef struct _Callback {
   struct _Callback *next;                /* link to next block        */
   HereventProc     proc;                 /* callback function pointer */
   int              mask;                 /* event mask                */
   void             *arg;                 /* client data               */
} _Callback, *Callback;

static Callback list=NULL;
static int      n_active=0;
static int      fd=0;                     /* fd for external event loop */

/* -------------------------------------------------------------------------- */
/*                                 readevent                                  */
/* -------------------------------------------------------------------------- */
static void readevent(ident id, int fd, void *arg)
{
   UnitedEvent uevent;
   Event event=(Event)&uevent;
   Callback cb, *cbp, *next;
   fd_set readfds;
   struct timeval timeout;
   
   timeout.tv_sec  = 0;
   timeout.tv_usec = 0;
   
   FD_ZERO(&readfds);
   FD_SET(fd, &readfds);

   while (select(fd+1, &readfds, NULL, NULL, &timeout)>0) {
      if (read(fd, &uevent, sizeof(uevent))>0) {
         for (cbp=&list; *cbp; cbp=next) {
            cb = *cbp;
            next = &cb->next;
            if (cb->mask) {
               if (event->code & cb->mask) {
                  cb->proc((ident)cb, event, cb->arg);
               }
            } else {
               next = cbp;
               *cbp = cb->next;
               Delete(cb);
            }
         }
      }
   }
}

/* -------------------------------------------------------------------------- */
/*                                 active                                     */
/* -------------------------------------------------------------------------- */
static void active(int mode)
{
   static ident id;
   static int fd;
    
   if (mode) {
      fd = notify(1);
      id = ScheduleRead(readevent,fd,NULL);
   } else {
      (void)notify(0);
      close(fd);
      Deschedule(&id);
   }
}

/* ========================================================================== */
/*                                 ScheduleHerevent                           */
/* -------------------------------------------------------------------------- */
ident ScheduleHerevent(HereventProc proc, int mask, void *arg)
{
   Callback cb=New(_Callback);
    
   if (!n_active++) active(1);
   
   cb->next = list;
   list     = cb;
   cb->proc = proc;
   cb->mask = mask; 
   cb->arg  = arg;
   
   return (ident)cb;
}

/* ========================================================================== */
/*                                 DescheduleHerevent                         */
/* -------------------------------------------------------------------------- */
extern void DescheduleHerevent(ident *id)
{
   Callback cb=(Callback)*id;
    
   if (!*id) return;
   *id = NULL;
   cb->mask = 0;
   if (!--n_active) active(0);
}

/* ========================================================================== */
/*                                 Herevents                                  */
/* -------------------------------------------------------------------------- */
extern void Herevents(bool state)
{
   if (n_active) active(state?1:0);
}

/* ========================================================================== */
/*                                 HerConnect                                 */
/* -------------------------------------------------------------------------- */
extern int HerConnect(void)
{
   fd = notify(1);
   n_active = 2;                          /* prevents active() to be called */ 
   return fd;
}

/* ========================================================================== */
/*                                 HerSignal                                  */
/* -------------------------------------------------------------------------- */
extern void HerSignal()
{
   readevent(0, fd, NULL);
}
         
/*
#> herevents.h
#if !defined(_herevents_h_)
#define _herevents_h_
#include "taskcom.h"
#include "events.h"
typedef void (*HereventProc)(ident, Event, void*);
extern  ident ScheduleHerevent(HereventProc proc, int mask, void *arg);
extern void DescheduleHerevent(ident *id);
extern void Herevents(bool state);
extern int  HerConnect(void);
extern void HerSignal(void);
#endif
#<

#> ScheduleHerevent.dc3
Function:     ScheduleHerevent

Purpose:      Register a function to be called whenever Hermes generates an
              event meeting a specified mask.

Category:     SYSTEM

File:         herevents.c

Author:       J.P. Terlouw

Use:          #include "herevents.h"
              ident id;
              id = ScheduleHerevent(proc, mask, arg);
              
              HereventProc proc  -  pointer to function
              int          mask  -  event mask: the bitwise OR of any of
                                    the events defined in taskcom.h
              void         *arg  -  'client' data

Description:  ScheduleHerevent registers a function to be called by MainLoop
              (or by another event loop to wich event handling has been
              delegated) whenever Hermes generates an event which meets the
              specified mask.
              In this call, 'arg' will be passed to 'proc'.
              The return value is a unique identification code corresponding
              with the registration, which will also be passed as an
              argument to 'proc'.
               
              The prototype of 'proc' is:
                  void proc(ident id, Event event, void *arg);

              The argument 'event' has the generic Hermes event type.
              Within 'proc' this should be cast to one or more specific
              event types.

Example:      The following program illustrates how the function can be used.

          #include "stdio.h"
          #include "cmain.h"
          #include "gipsyc.h"
          #include "init.h"
          #include "finis.h"
          #include "anyout.h"
          #include "herevents.h"
          
          static char message[80];
          static fint zero=0;
          
          static void handle_key(ident id, Event event, void *arg)
          {
             KeyEvent keyevent=(KeyEvent)event;
             sprintf(message,"%s: %s", (char*)arg, keyevent->key);
             anyout_c(&zero,tofchar(message));
          }
          
          static void handle_resume(ident id, Event event, void *arg)
          {
             ResEvent resevent=(ResEvent)event;
             anyout_c(&zero,tofchar("Resume after pause"));
          }
          
          static void handle_any(ident id, Event event, void *arg)
          {
             anyout_c(&zero,tofchar("Keychange or resume (only one message)"));
             DescheduleHerevent(&id);
          }
             
          MAIN_PROGRAM_ENTRY
          {
             init_c();
             (void)ScheduleHerevent(handle_key, KEYCHANGE, "Keyword changed");
             (void)ScheduleHerevent(handle_resume,RESEVENT,NULL);
             (void)ScheduleHerevent(handle_any,KEYCHANGE|RESEVENT,NULL);
             MainLoop();
             finis_c();
             return 0;
          }
          
Related Docs: DescheduleHerevent.dc3, events.dc3

Updates:      Apr  8, 1997: JPT, Document created.
#<

#>  DescheduleHerevent.dc3
Function:     DescheduleHerevent

Purpose:      De-register a function registered previously by ScheduleHerevent.

Category:     SYSTEM

File:         herevents.c

Author:       J.P. Terlouw

Use:          DescheduleHerevent(&id);
              ident id  -  registration identification code.

Description:  The function associated with 'id' is descheduled and 'id' is
              set to zero. (Note that the argument is a pointer to 'id'.)

Related Doc:  ScheduleHerevent.dc3

Updates:      Apr  8, 1997: JPT, Document created.
#<

#>  herevents.dc3
Document:     herevents

Purpose:      Describes routines for handling Hermes events

Category:     SYSTEM
              
File:         herevents.c
             
Author:       J.P. Terlouw
            
Description:  The routines in this source allow a task to specify ("schedule")
              one or more functions to be called when Hermes generates a
              specified event. A Hermes event can be one of the following:
              - user input keyword change;
              - user input keyword cancel;
              - user input keyword reject;
              - task resume after suspend.
              These events and associated messages are defined in taskcom.h.
              
              The following routines are available:

              ScheduleHerevent     - schedule function for receiving events
              DescheduleHerevent   - deschedule function
              Herevents            - control Hermes' event generation
              HerConnect           - connect from alternative event loop
              HerSignal            - activation by alternative event loop

Related doc:  MainLoop.dc2

Updates:      Feb 22, 2010: JPT, readevent() now processes all pending events.
              Sep 03, 2010: JPT, leave select() loop on negative status.
#<

#>  Herevents.dc2
Function:     Herevents

Purpose:      Control Hermes' event generation.

Category:     SYSTEM
              
File:         herevents.c
             
Author:       J.P. Terlouw

Use:          Herevents(state);
              bool state - determines whether events are generated or not.
            
Description:  This function can be used in event-driven tasks to temporarily
              prevent Hermes from generating events such as keyword events.
              This can be useful when the current interaction is
              exclusively controlled by an other event source, e.g. a
              a plotfield cursor. In such cases the responsiveness
              of the application may improve.

Updates:      Jun 26, 2000: JPT, Document created.
#<

#>  HerConnect.dc3
Function:     HerConnect

Purpose:      Obtain a file descriptor to be used by an external event loop.

Category:     SYSTEM

File:         herevents.c

Author:       J.P. Terlouw

Use:          #include "herevents.h"
              int fd;
              fd = HerConnect()

Description:  HerConnect() returns the file descriptor on which Hermes events
              are reported and disables the normal MainLoop processing.
              This file descriptor can be used to delegate event handling
              to an external event loop, e.g., from a GUI toolkit such as Qt.

Related doc:  HerSignal.dc3 

Updates:      Mar 5, 2009: JPT, Document created.
#<

#>  HerSignal.dc3
Function:     HerSignal

Purpose:      Process a Hermes event.

Category:     SYSTEM

File:         herevents.c

Author:       J.P. Terlouw

Use:          HerSignal()

Description:  HerSignal() causes any pending Hermes events to be processed.
              This can be done by an external event loop to wich
              event handling has been delegated, e.g., from a GUI toolkit
              such as Qt.
              It may also be called by a program which wishes
              to have any Hermes events it has caused to be processed
              before other actions are performed.

Related doc:  HerConnect.dc3 

Updates:      Mar  5, 2009: JPT, Document created.
              Feb 22, 2010: JPT, Prevent blocking in absence of pending events.
#<
*/

