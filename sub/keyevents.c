/* keyevents.c
                              COPYRIGHT (c) 1997
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands
             
Author: J.P. Terlouw
*/

#define  HTABSIZ 311                      /* hash table size (prime)   */

#include "stddef.h"
#include "stdlib.h"
#include "string.h"
#include "gipsyc.h"
#include "userfio.h"
#include "usertext.h"
#include "wkey.h"
#include "keyevents.h"

#define New(type) ((type *)calloc(1,sizeof(type)))
#define NNew(n,type) ((type *)calloc(n,sizeof(type)))
#define Delete(x)  {free(x); x=NULL;}
               
typedef struct _Callback {
   struct _Callback *next;                /* link to next block        */
   KeyeventProc     proc;                 /* callback function pointer */
   char             *key;                 /* user input keyword        */
   int              mask;                 /* event mask                */
   void             *arg;                 /* client data               */
   int              skip;                 /* suppress count            */
} _Callback, *Callback;
       
static Callback lists[HTABSIZ];
static int      n_active=0;

/* -------------------------------------------------------------------------- */
/*                                 hash                                       */
/* -------------------------------------------------------------------------- */
static int hash(char *key)
{
   int  h=*key;

   while (*key != '=') h = (2*h)^(int)*(key++);
   return h%HTABSIZ;
}

/* -------------------------------------------------------------------------- */
/*                                 handle_event                               */
/* -------------------------------------------------------------------------- */
static void handle_event(ident id, Event event, void *arg)
{
   KeyEvent keyevent=(KeyEvent)event;
   Callback *list=&lists[hash(keyevent->key)];
   Callback cb, *cbp, *next;

   for (cbp=list; *cbp; cbp=next) {
      cb = *cbp;
      next = &cb->next;
      if (cb->key) {
         if ((!strcmp(keyevent->key,cb->key)) && (event->code & cb->mask)) {
            if (cb->skip) {
               cb->skip--;
            } else {
               cb->proc((ident)cb, keyevent->key, keyevent->code, cb->arg);
            }
         }
      } else {
         next = cbp;
         *cbp = cb->next;
         Delete(cb);
      }
   }
   if (keyevent->code==KEYCHANGE) {
      /* allow task responsible for event to continue */
      userfint(NULL, 0, DFLT_NOREAD|DFLT_DONE, keyevent->key, " ");
   }
}

/* -------------------------------------------------------------------------- */
/*                                 active                                     */
/* -------------------------------------------------------------------------- */
static void active(int mode)
{
   static ident id;

   if (mode) {
      id = ScheduleHerevent(handle_event, KEYCHANGE|KEYCANCEL|KEYREJECT ,NULL);
   } else {
      DescheduleHerevent(&id);
   }
}

#if defined(HASHTEST)  
/* -------------------------------------------------------------------------- */
/*                                 dumptable                                  */
/* -------------------------------------------------------------------------- */
/*  dumptable() prints the distribution of keywords over the hash table.
 *  For test purposes only.
*/
extern void dumptable(void)
{
   int i;
   Callback current;

   for (i=0; i<HTABSIZ; i++) {
      if (lists[i]) {
         anyoutf(0, "Bucket %d", i);
         for (current=lists[i]; current; current=current->next) {
            if (current->key) {
               anyoutf(0, ".         %s", current->key);
            }
         }
      }
   }
}
#endif

/* ========================================================================== */
/*                                 ScheduleKeyevent                           */
/* -------------------------------------------------------------------------- */
ident ScheduleKeyevent(KeyeventProc proc, char *key, int mask, void *arg)
{
   Callback cb=New(_Callback);
   Callback *list=&lists[hash(key)];
   
   if (!n_active++) active(1);

   cb->next = *list;
   *list    = cb;
   cb->proc = proc;
   cb->key  = NNew(strlen(key)+1,char); strcpy(cb->key, key);
   cb->mask = mask;
   cb->arg  = arg;

   return (ident)cb;
}

/* ========================================================================== */
/*                                 DescheduleKeyevent                         */
/* -------------------------------------------------------------------------- */
extern void DescheduleKeyevent(ident *id)
{
   Callback cb=(Callback)*id;

   if (!*id) return;
   *id = NULL;
   Delete(cb->key);
   if (!--n_active) active(0);
}

/* ========================================================================== */
/*                                SuppressKeyevent                            */
/* -------------------------------------------------------------------------- */
void SuppressKeyevent(KeyeventProc proc, char *key)
{
   Callback list=lists[hash(key)];
   Callback current;
   bool     ok=FALSE;
    
   for (current=list; current; current=current->next) {
      if (current->proc==proc && current->key && !strcmp(current->key, key)) {
         current->skip++;
         ok = TRUE;
      }
   }
   if (!ok) errorf(2, "SuppressKeyevent: %s not found", key);
}
   
/* ========================================================================== */
/*                                 TriggerKey                                 */
/* -------------------------------------------------------------------------- */
bool TriggerKey(char *key)
{
   fchar value;
   char  value_c[512];
   char  buffer[532];
   fint  nread, fint2=2;
   bool  result=FALSE;

   value.l = 512; value.a = value_c;
   nread = usertext_c(value, &fint2, tofchar(key), tofchar(" "));
   if (nread && (nread<512)) {
      value_c[nread] = '\0';
      strcpy(buffer, key);
      strcat(buffer, value_c);
      wkey_c(tofchar(buffer));
      result = TRUE;
   }
   return result;
}

/*
#> keyevents.h
#if !defined(_keyevents_h_)  
#define _keyevents_h_
#include "herevents.h"
typedef void (*KeyeventProc)(ident, char*, int, void*);
extern  ident ScheduleKeyevent(KeyeventProc proc, char* key, int mask, void *arg);
extern void DescheduleKeyevent(ident *id);
extern void SuppressKeyevent(KeyeventProc proc, char *key);
bool TriggerKey(char *key);
#endif
#<

#> keyevents.dc2
Document:     keyevents

Purpose:      Describes routines for handling user input keyword events

Category:     SYSTEM, USER-INTERFACE
              
File:         keyevents.c
             
Author:       J.P. Terlouw
            
Description:  The routines in this source allow a task to specify ("schedule")
              one or more functions to be called when Hermes generates an
              event associated with a user input keyword.
              Such an event can be one of the following:
              - user input keyword change;
              - user input keyword cancel;
              - user input keyword reject;
              
              The following routines are available:

              ScheduleKeyevent     - schedule function for receiving events
              DescheduleKeyevent   - deschedule function           
              SuppressKeyevent     - suppres call to function

Related doc:  MainLoop.dc2

Updates:      Apr 18, 1997: JPT, Document created.
              Jun 28, 1998: JPT, Implemented hash table.
              Oct  7, 1999: JPT, Implemented SuppressKeyevent.
              Nov 11, 1999: JPT, SuppressKeyevent bug fix.
              May  4, 2001: JPT, Synchronization with other task's wkey calls.
#<

#> ScheduleKeyevent.dc2
Function:     ScheduleKeyevent

Purpose:      Register a function to be called whenever Hermes generates an
              event associated with the specified user input keyword.

Category:     SYSTEM, USER-INTERFACE

File:         keyevents.c

Author:       J.P. Terlouw

Use:          #include "keyevents.h"
              ident id;
              id = ScheduleKeyevent(proc, key, mask, arg);
              
              KeyeventProc proc  -  pointer to function
              char         *key  -  user input keyword
              int          mask  -  event mask: the bitwise OR of any of
                                    the key events defined in taskcom.h
              void         *arg  -  'client' data

Description:  ScheduleKeyevent registers a function to be called by MainLoop
              whenever Hermes generates an event which meets the specified
              keyword and mask.
              In this call, 'arg' will be passed to 'proc'.
              The return value is a unique identification code corresponding
              with the registration, which will also be passed as an
              argument to 'proc'.
               
              The prototype of 'proc' is:
                  void proc(ident id, char *key, int code, void *arg);

              The argument 'key' is the keyword for which the event was
              generated. This argument can be used to discriminate between
              keywords when the same function is registered for multiple
              keywords. The argument 'code' is the actual event code.
              (KEYCHANGE, KEYCANCEL or KEYREJECT as defined in taskcom.h)

Example:      The following program illustrates how the function can be used.
              It waits for (re)definition of any of the keywords STOP=, TEST=
              or CONT=. When such an event occurs, the function 'handle_key'
              is called, which prints the argument specified when the function
              was scheduled, reads the value of the input, prints it,
              and finally deschedules itself for this keyword.
              When all three keywords have been specified, everything has
              been descheduled causing MainLoop to return.

         #include "cmain.h"
         #include "gipsyc.h"
         #include "init.h"
         #include "finis.h"
         #include "usertext.h"
         #include "anyout.h"
         #include "keyevents.h"
         
         static fint zero=0;
         
         static void handle_key(ident id, char *key, int code, void *arg)
         {
            fchar value;
            char  valstr[40];
             
            value.a = valstr;
            value.l = 40;
             
            anyout_c(&zero, tofchar((char*)arg));
            (void)usertext_c(value, &zero, tofchar(key), tofchar(" "));
            anyout_c(&zero, value);
            DescheduleKeyevent(&id);
         }
         
         MAIN_PROGRAM_ENTRY
         {
            init_c();
            (void)ScheduleKeyevent(handle_key, "STOP=", KEYCHANGE, "Stop");
            (void)ScheduleKeyevent(handle_key, "TEST=", KEYCHANGE, "Test");
            (void)ScheduleKeyevent(handle_key, "CONT=", KEYCHANGE, "Cont");
            MainLoop();
            finis_c();
         }

Related Docs: DescheduleKeyevent.dc2, events.dc3

Updates:      Apr 18, 1997: JPT, Document created.
#<

#> DescheduleKeyevent.dc2
Function:     DescheduleKeyevent

Purpose:      De-register a function registered previously by ScheduleKeyevent.

Category:     USER-INTERFACE, SYSTEM

File:         keyevents.c

Author:       J.P. Terlouw

Use:          DescheduleKeyevent(&id);
              ident id  -  registration identification code.

Description:  The function associated with 'id' is descheduled and 'id' is
              set to zero. (Note that the argument is a pointer to 'id'.)

Related Doc:  ScheduleKeyevent.dc2

Updates:      Apr 18, 1997: JPT, Document created.
#<

#> SuppressKeyevent.dc2
Function:     SuppressKeyevent

Purpose:      Suppress call to keyword event handler function.

Category:     USER-INTERFACE, SYSTEM

File:         keyevents.c

Author:       J.P. Terlouw

Use:          SuppressKeyevent(proc, key);
              KeyeventProc proc  -  pointer to function
              char         *key  -  user input keyword

Description:  SuppressKeyevent prevents function 'proc' (which should have
              been registered by ScheduleKeyevent) to be called at the
              occasion of the next event on 'key'. The action is cumulative:
              for each call to SuppressKeyevent one event will be suppressed.

Related Doc:  ScheduleKeyevent.dc2

Updates:      Oct  7, 1999: JPT, Document created.
              Nov 11, 1999: JPT, Fixed problem with deactivated callbacks.

#<

#> TriggerKey.dc2
Function:      TriggerKey

Purpose:       Re-send keyword to Hermes, causing a KEYCHANGE event

Category:      USER-INTERFACE, SYSTEM

File:          keyevents.c

Author:        J.P. Terlouw

Use:           #include "keyevents.h"
               char *key;
               bool result;

               result = TriggerKey(key);

                  result - TRUE if keyword was actually triggered,
                           i.e. if it was present. Otherwise FALSE.
                  key    - user input keyword

Description:   TriggerKey attempts to read the value associated with the
               user input keyword. If this succeeds, the keyword is re-sent
               to Hermes. In event-driven programs this will result in a
               KEYCHANGE event for the given keyword.
               This routine can be useful e.g. in the initialization phase
               of event-driven programs.

Note:          An other approach in implementing this functionality would
               be calling the event handler(s) directly from this function
               instead of letting Hermes generate the event. This may be
               more efficient, but the implementation chosen is simpler.

Updates:       Mar  6, 1998: JPT, Document created.
#<


*/
