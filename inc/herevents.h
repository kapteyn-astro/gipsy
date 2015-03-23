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
