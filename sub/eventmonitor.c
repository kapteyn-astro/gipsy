/* eventmonitor.c
                              COPYRIGHT (c) 1999
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands


Author: J.P. Terlouw
*/

#include "stdio.h"
#include "stdlib.h"
#include "gipsyc.h"
#include "userfio.h"
#include "keyevents.h"
#include "eventmonitor.h"

static void handle_event(ident id, Event event, void *arg)
{
   switch (event->code) {
      case KEYCHANGE: {
         KeyEvent ke=(KeyEvent)event;
         int nc;
         char  ctext[21];
         fchar text;
         text.a = ctext; text.l = 20;
         nc = userftext(text, 2, ke->key, " ");
         ctext[nc]= '\0';
         anyoutf(0, "Event: keyword %s changed: %s", ke->key, ctext);
         break;
      }
      case KEYCANCEL: {
         KeyEvent ke=(KeyEvent)event;
         anyoutf(0, "Event: keyword %s cancelled", ke->key);
         break;
      }
      case KEYREJECT: {
         KeyEvent ke=(KeyEvent)event;
         anyoutf(0, "Event: keyword %s rejected", ke->key);
         break;
      }
      case RESEVENT: {
         anyoutf(0,"Event: resume after pause");
         break;
      }
      default: {
         anyoutf(0,"Event: undefined event code=%d",event->code);
      }
   }
}

/* -------------------------------------------------------------------------- */
/*                                 monitor                                    */
/* -------------------------------------------------------------------------- */
/*  Keyword handler for switching the event monitor.
 */
static void monitor(ident id, char *key, int code, void *arg)
{
   bool state=toflog(FALSE);
   (void)userflog(&state, 1, 2, key, " ");
   (void)eventmonitor(NULL, tobool(state));
}

/* ========================================================================== */
/*                                 eventmonitor                               */
/* -------------------------------------------------------------------------- */
bool eventmonitor(char *key, bool newstate)
{
   static ident id=NULL;
   static ident kid=NULL;
   static bool oldstate=FALSE;
   
   if (key) {
      if (newstate && !kid) {
         kid = ScheduleKeyevent(monitor, key, KEYCHANGE, NULL);
      } else if (kid && !newstate) {
         DescheduleKeyevent(&id);
      }
   } else {
      if (newstate && !oldstate) {
         id = ScheduleHerevent(handle_event, -1, NULL);
      } else if (oldstate & !newstate) {
         DescheduleHerevent(&id);
      }
      oldstate = newstate;
   }
    
   return oldstate;
}

/*

#> eventmonitor.dc3
Function:     eventmonitor

Purpose:      Monitoring utility for debugging event-driven tasks.

Category:     SYSTEM

File:         eventmonitor.c

Author:       J.P. Terlouw

Use:          #include "eventmonitor.h"
              bool state, newstate;
              char *key;

              state = eventmonitor(key, newstate);

Description:  eventmonitor() allows the programmer to switch the monitoring
              utility on and off, or specify a keyword which can be used
              to switch the utility interactively.

              If switched on, eventmonitor() outputs all Hermes-generated
              events to the screen and the logfile.

              There are two modes of operation:

              1. key is a user input keyword.
                 Associate or disassociate keyword with the monitor.
                 newstate == TRUE: associate;
                 newstate == FALSE: disassociate.
                 When a keyword is associated with the monitor, monitoring
                 can be switched by the user. "YES" switches on, "NO"
                 switches off.

              2. key == NULL.
                 In this case the monitor is switched directly:
                 newstate == TRUE switches on;
                 newstate == FALSE switches off.

              In both cases the current monitoring state is returned.

Related Docs: events.dc3, ScheduleRead.dc3, ScheduleWrite.dc3,
              ScheduleTimer.dc3, DescheduleAll.dc3, MainLoop.dc2, AttachXt.dc2.

Updates:      Mar 17, 1999: JPT, Document created.
#<

#> eventmonitor.h
#if !defined(_eventmonitor_h_)
#define _eventmonitor_h_
bool eventmonitor(char *key, bool newstate);
#endif
#<

*/

#if defined(TESTBED)
#include "init.h"
#include "finis.h"
#include "cmain.h"  

MAIN_PROGRAM_ENTRY
{
   init_c();
   (void)eventmonitor("MTR=", TRUE);
   MainLoop();
}
#endif
