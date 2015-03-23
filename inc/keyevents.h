#if !defined(_keyevents_h_)  
#define _keyevents_h_
#include "herevents.h"
typedef void (*KeyeventProc)(ident, char*, int, void*);
extern  ident ScheduleKeyevent(KeyeventProc proc, char* key, int mask, void *arg);
extern void DescheduleKeyevent(ident *id);
extern void SuppressKeyevent(KeyeventProc proc, char *key);
bool TriggerKey(char *key);
#endif
