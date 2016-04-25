/* help.c
                              COPYRIGHT (c) 1992
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

This module is private to Hermes. It handles the tHermes context-sensitive
help display.

Author: J.P. Terlouw.
*/

#include "stddef.h"
#include "stdlib.h"
#include "ctype.h"
#include "string.h"
#include "stdio.h"
#include "thermescom.h"
#include "windows.h"
#include "prompter.h"
#include "keyboard.h"
#include "keysym.h"
#include "ucamgr.h"
#include "taskmgr.h"
#include "findstring.h"
#include "helpkey.h"
#include "webhelp.h"
#include "help.h"

#define NALLOC  100  /* memory allocation increment (in lines) */
#define OVERLAP   2  /* number of lines overlap between pages  */

static Window win=NULL,top=NULL;
static int    xsize, ysize;
static int    textsize, nlines, startline;
static long   kid=0, uid=0, tid=0;
static int    keywindow=0,keyhelpmode=1;
static char   help_name[NAMLEN+1];
static char   help_key[KEYLEN+1];
static char   seastr[40];
static char   *text=NULL;

static int  search(long,int,char*,void*);
static void deactivate(void);
static int  getchars(long,int,void*);
static void followuca(long,int,void*);
static void tasknotify(long,TaskStatus,void*);
static void updatewin(void);
static int  findtext(char*);
static char *findhelpname(void);
static void initkeyhelp(void);

extern int NTASK;
extern int LINES, COLS;                      /* screen dimensions from Curses */

/* ========================================================================== */
/*                               InitHelp                                     */
/* -------------------------------------------------------------------------- */
/*  InitHelp initializes the context-sensitive help display.
 *  It displays the .dc1 document of the task of which the name is present
 *  in the User Command Area. If a user input keyword is present, the
 *  document is positioned at the first occurrence of the keyword.
 */
extern void InitHelp()
{
   Window errwin;

   int i, nchars;
   char topline[80];
   char filename[80];
   char dc;
   FILE *stream;

   if (kid) return;                                /* allow only one instance */

   xsize = COLS;
   ysize = LINES-NUCA-NTASK-1;
   strcpy(help_name,findhelpname());
   strcpy(filename,getenv("gip_tsk"));
   strcat(filename,"/");
   strcat(filename,help_name);
   strcat(filename,".dc1");
   if ((stream=fopen(filename,"r"))) {
      if (!WebHelp(filename)) {
         (void)fclose(stream);
         return;                                  /* delegated to WWW browser */
      }
      text = (char*)Malloc(NALLOC*xsize+1);
      textsize = NALLOC;
      i = NALLOC;
      nlines = 0;
      nchars = 0;
      while (!feof(stream)) {
         if (i==0) {
            textsize += NALLOC;
            i = NALLOC;
            text = (char*)Realloc(text,textsize*xsize+1);
         }
         dc = fgetc(stream);
         if (dc==EOF || dc=='\n') {
            while ((nchars)<xsize) {
               *(text+nlines*xsize+nchars) = ' ';
               nchars++;
            }
            nlines++;
            i--;
            nchars = 0;
         } else if (nchars<xsize) {
            *(text+nlines*xsize+nchars++) = dc;
         }
      }
      *(text+nlines*xsize+nchars) = 0;
      (void)fclose(stream);
   } else {
      errwin = CreateWindow(4,25);
      FrameWindow(errwin);
      PutLine(errwin,1," Document not found:");
      PutString(errwin,2,3,help_name);
      PutString(errwin,2,3+strlen(help_name),".dc1");
      MapWindow(errwin,0,0);
      InsertTimerProc(TimeDelWin,3,errwin);
      errwin = NULL;
      Beep();
      return;
   }
   sprintf(topline,
   "  Document: %s.dc1  ---  Remove: TAB  ---  Hyper Help: ESC X  ",help_name);
   if (!win) {
      /* windows are re-used; not deleted */
      top = CreateWindow(1,xsize);
      top->framed = TRUE;
      win = CreateWindow(1,xsize);
      free(win->backup);                /* maintain own backup */
      win->private = FALSE;
   }
   for (i=0; i<xsize; i++) PutChar(top,0,i,'=');
   PutString(top,0,(xsize-strlen(topline))/2,topline);
   MapWindow(top,0,0);
   win->ny = ysize<nlines-1?ysize:nlines-1;               /* resize window */
   startline = 0;
   findtext(KeyFromUca());
   updatewin();
   MapWindow(win,1,0);
   kid = InsertKeyboardProc(getchars,NULL);
   uid = InsertUcaProc(followuca,NULL);
   tid = InsertTaskProc(tasknotify,NULL);
   initkeyhelp();
}

/* ========================================================================== */
/*                               SetKeyHelp                                   */
/* -------------------------------------------------------------------------- */
/*  SetKeyHelp() disables or enables the keyword help overlay.
 *  mode = 1 enables the overlay
 *  mode = 0 disables the overlay.
 */
extern void SetKeyHelp(int mode)
{
   keyhelpmode = mode;
}

/* -------------------------------------------------------------------------- */
/*                               getchars                                     */
/* -------------------------------------------------------------------------- */
/*  Keyboard procedure getchars() allows the user to page through the document.
 */
static int  getchars(long id, int c, void* arg)
{
   static int sdir;

   if (c==QRESCHED) return c;  /* allow rescheduling but do not participate */

   if ((c==CTRL_I)||(c==CTRL_P)) {
      deactivate();
      return 0;
   }

   if (win->ny>=ysize) {
      sdir = 0;
      switch (c) {
         case ESCBIT|'<': startline = 0; break;

         case ESCBIT|'>': startline = nlines-ysize; break;
          
         case ESCBIT|TAB: SetKeyHelp(0);
                          if (keywindow) {
                             StopHelpKey(); keywindow=0; break;
                          } else return c;

         case CTRL_Z:  startline = startline - ysize + OVERLAP;
                       break;

         case CTRL_V:  startline = startline + ysize - OVERLAP;
                       break;

         case CTRL_R:  sdir = 1;              /* switch to backward search */
         case CTRL_S:
                       Prompter("Enter search string:",seastr,
                       search, (void*)sdir, LINES-6,40);
                       break;

         default:      return c;                /* propagate              */
      }
   } else return c;
   updatewin();
   return 0;
}

/* -------------------------------------------------------------------------- */
/*                             followuca                                      */
/* -------------------------------------------------------------------------- */
/*  UCA callback procedure followuca repositions the document at any keyword
 *  present in the User Command Area.
 */
static void followuca(long id, int reason, void *arg)
{
   if (   reason==UCA_PROMPT
       && !strcmp(help_name,findhelpname())
       && !findtext(KeyFromUca())
       && nlines>ysize ) updatewin();
   initkeyhelp();
}

/* -------------------------------------------------------------------------- */
/*                             tasknotify                                     */
/* -------------------------------------------------------------------------- */
/*  tasknotify() deactivates the help screen if the task asscociated with it
 *  is terminated
 */
static void tasknotify(long id, TaskStatus task, void *arg)
{
   char taskname[NAMLEN+1];

   if (!task->proc) {
      strcpy(taskname,task->name);
      StringLow(taskname);
      if (!strcmp(taskname,help_name)) deactivate();
   }
}

/* -------------------------------------------------------------------------- */
/*                                 search                                     */
/* -------------------------------------------------------------------------- */
/*   search() performs a text search in the help display.
 */
static int search(long id, int status, char *result, void *arg)
{
   char *matchpoint;
   int sdir=(int)arg;

   if (status==PROMPT_CANCEL) return 0;
   strcpy(seastr,result);
   matchpoint = findstring(text, text+(startline+1-sdir)*xsize, seastr, sdir);
   if (matchpoint) {
      startline = (matchpoint-text)/xsize;
      updatewin();
      return 0;
   } else {
      SetPrompterError(id,"- text not found");
      return -1;
   }
}

/* -------------------------------------------------------------------------- */
/*                           findhelpname                                     */
/* -------------------------------------------------------------------------- */
/*   findhelpname() determines the name of the appropriate .dc1 document
 */
static char *findhelpname(void)
{
   char *hn;
   TaskStatus task=NULL;
   static char name[NAMLEN+1];

   hn = NameFromUca();
   if (hn && *hn) strcpy(name,hn); else strcpy(name,"thelp");
   StringUp(name);
   task = NameToTask(name);
   if (task) {
      ProcStatus proc;
      for (proc=task->proc; proc->next; proc=proc->next) {;}
      strcpy(name,proc->name);
   }
   StringLow(name);
   return name;
}

/* -------------------------------------------------------------------------- */
/*                             deactivate                                     */
/* -------------------------------------------------------------------------- */
/*  deactivate() removes the help display and cleans up everything.
 */
static void deactivate(void)
{
   UnmapWindow(top);
   UnmapWindow(win);
   free(text);
   help_key[0] = 0;
   RemoveKeyboardProc(kid); kid = 0;
   RemoveUcaProc(uid); uid = 0;
   RemoveTaskProc(tid); tid = 0;
   if (keywindow) StopHelpKey();
   keywindow = 0;
}

/* -------------------------------------------------------------------------- */
/*                             updatewin                                      */
/* -------------------------------------------------------------------------- */
/*  updatewin() maps the correct part of the memory representation of
 *  the document to the help window.
 */
static void updatewin(void)
{
   if (startline>(nlines-ysize)) startline = nlines-ysize;
   if (startline<0) startline = 0;
   win->backup = text+startline*xsize;
   RedrawWindow(win);
}

/* -------------------------------------------------------------------------- */
/*                              findtext                                      */
/* -------------------------------------------------------------------------- */
/*  findtext() searches for the text specified in the argument and adjusts
 *  the point where the memory is to be mapped to the help window.
 */
static int findtext(char *word)
{
   char *matchpoint;

   if (nlines>ysize && strcmp(help_key,word)) {
      matchpoint = strstr(text,word);
      if (matchpoint) {
         startline = (matchpoint-text)/xsize;
         strcpy(help_key,word);
         return 0;        /* found and startline adjusted */
      } else return -1;   /* string not found */
   } else return -1;      /* fits on one page or found on a previous occasion */
}

/* -------------------------------------------------------------------------- */
/*                             initkeyhelp                                    */
/* -------------------------------------------------------------------------- */
static void initkeyhelp(void)
{
   if (keyhelpmode && !keywindow && *KeyFromUca()) {
      StringUp(help_name);
      HelpKey(help_name);
      StringLow(help_name);
      keywindow = 1;
   }
}
