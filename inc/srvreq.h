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
