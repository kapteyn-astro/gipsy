#if !defined(_ggi_h_)
#define _ggi_h_
#include "keyevents.h"

#define ggiButn1         1                        /* mouse button 1     */
#define ggiButn2         2                        /* mouse button 2     */
#define ggiButn3         4                        /* mouse button 3     */
#define ggiShift         8                        /* shift key          */
#define ggiCtrl         16                        /* ctrl key           */
#define ggiKeybd        32                        /* other keyboard key */
#define ggiLeftArrow  0xD1                        /* left arrow         */
#define ggiUpArrow    0xD2                        /* up arrow           */
#define ggiRightArrow 0xD3                        /* right arrow        */
#define ggiDownArrow  0xD4                        /* down arrow         */

#define ggiCreate     1
#define ggiColind     2
#define ggiLimits     3
#define ggiUnits      4
#define ggiApply      5
#define ggiDelete     6
#define ggiIdent      7
#define ggiNdims      8
#define ggiNowait     9
#define ggiCloseOnOK 10
#define ggiFileInfo  11
#define ggiGetMap    12
#define ggiStart     13
#define ggiNext      14
#define ggiStop      15

#define ggiReadOnly 0
#define ggiEdit     1
#define ggiAppend   2

#define ggiImmediateKeyevent(x) wkey_cb(x?GgiHandleEvents:NULL)

typedef enum {ggiLeft, ggiCenter, ggiRight} ggiAlign;   /* alignments */

typedef struct {
   ident plotfield;       /* Ggi element id */
   char  *name;           /* PGPLOT device name */
   float x, y;            /* cursor position in world coordinates */
   float width, height;   /* size of box (may be negative) */
   int   button;          /* button or key which caused the event */
   int   state;           /* mask of all active buttons and modifiers */
   char  key;             /* keyboard key */
} _GgiPlotInfo, *GgiPlotInfo;

typedef void (*GgiCursorProc)(ident, GgiPlotInfo, void*);
typedef char *(*GgiNameconverterProc)(char*);
typedef void (*GgiCleanupProc)(void*);

ident GgiTextField(char *keyword, char *message, int size);
ident GgiButton(char *keyword, char *message);
ident GgiMenu(char *keyword, char *message, char **labels);
ident GgiMenuLabels(ident menu, char **labels);
ident GgiList(char *keyword, int defcol, char **labels);
ident GgiListLabels(ident id, char **labels);
ident GgiListColumns(ident id, int columns);
ident GgiEditor(char *keyword, int width, int height, int mode,
                char *contents, int size);
ident GgiEditorSetSource(ident id, char *source, int size);
char *GgiEditorSource(ident id);
ident GgiEditorSave(ident id);
ident GgiEditorSaveAs(ident id, char *name);
ident GgiEditorSearch(ident id, int direction);
ident GgiGauge(char *keyword, char *message, int length, float minval,
               float maxval);
ident GgiDial(char *keyword, char *message, int radius, int minang, int maxang,
              int intervals, float arrw, float inarrl, float outarrl,
              float minval, float maxval);
ident GgiLabel(char *text);
ident GgiLogo(unsigned char bits[], int width, int height);
ident GgiCanvas(char *name, int width, int height);
ident GgiProgress(char *keyword, char *message, int length);
ident GgiPlotField(char *name, int width, int height);
int   GgiPlotColors(ident id, int newcolors);
ident GgiShell(char *name);
ident GgiDialog(char *name);
ident GgiForm(char *name, int border);
ident GgiViewport(char *name, int xsize, int ysize);
ident GgiUseShell(ident id);
void  GgiShowShell(ident id, bool show);
bool  GgiPlotXor(bool mode);
int   GgiPlotFrames(ident id, int newframes);
void  GgiPlotRecord(ident id, int frame);
void  GgiPlotShow(ident id, int frame);
void  GgiPlotMapColors(ident id, ... );
void  GgiPlotExport(ident id, char *name);
ident ScheduleGgiPlotCursor(GgiCursorProc, ident plotfield, void *arg);
void  DescheduleGgiPlotCursor(ident *id);
ident GgiActivate(ident id);
ident GgiDeactivate(ident id);
ident GgiFollowKey(ident id, bool follow);
ident GgiHelpText(ident id, char *message);
ident GgiSetLabel(ident id, char *label, int width);
ident GgiSetBitmap(ident id, int width, int height, unsigned char *bits);
ident GgiSetRange(ident id, float min, float max);
ident GgiSetFormat(ident id, char *format);
ident GgiSetCircular(ident id, bool circular);
ident GgiSetKeyword(ident id, char *key);
ident GgiSetCleanup(ident id, GgiCleanupProc proc, void *arg);
ident GgiAlignLabel(ident id, ggiAlign al);
ident GgiSetPosition(ident id, int xfrom, ident xid, int yfrom, ident yid);
void  GgiDelete(ident *id);
int   GgiWidth(ident id);
int   GgiHeight(ident id);
bool  GgiIsRealized(ident id);
void  GgiConfigure(char *appclass, int width, int menuheight, int textheight,
                   int buttonheight);
void  GgiAutoLayout(bool a);
void  GgiPostponeRealize(bool later);
void  GgiRealize(void);
void  GgiSetHeight(int height);
void  GgiSetWidth(int width);
void  GgiReLayout(void);
void  GgiShowDoc(char *docname);
void  GgiSetResources(char **resources);
int   GgiHandleEvents(void);
void  GgiPrompter(char *key, char *message);
void  GgiPlotPrompter(char *filekey, char *devkey, char *message);
ident GgiPlotColorEditor(int opcode, ... );
ident GgiInset(int opcode, ... );
ident GgiFileBrowser(int opcode, ... );
bool  GgiVerify(char *question, char *truelabel, char *falselabel);
ident GgiHelpMenu(char *key, char *buttontext, char *message,
                  char** labels, char **documents);
ident GgiTextMenu(char *keyword, char *message, char **labels);
ident GgiColorMenu(char *keyword, char *message);
ident GgiHeaderButton(char *key, char *buttontext, char *message, char *setkey);
int   GgiKeyIdent(char *key);
void  GgiSetDisplay(char *displayname);
void  GgiOptionsShell(char *key);
ident GgiOptionsButton(void);
ident GgiSetBtnKey(ident id, char *key);
int   GgiMpeg(ident id, int opcode, ...);
void  GgiPlotMovie(ident plotfield, int opcode, ...);
void GgiPlotCopy(ident id, int src, int dst);
void GgiPlotDest(ident id, int index);
#endif
