#if !defined(_eventsxt_h_)
#define _eventsxt_h_
#include "events.h"
#include <X11/Intrinsic.h>
extern void AttachXt(XtAppContext *app_context);
extern void ScheduleX(void(*)());
#endif
