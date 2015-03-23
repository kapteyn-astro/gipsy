#if !defined(_events_h_)
#define _events_h_

#define MS        *1
#define SECONDS   *1000 MS
#define MINUTES   *60 SECONDS
#define HOURS     *60 MINUTES
#define DAYS      *24 HOURS
#define WEEKS     *7  DAYS  
#define SECOND SECONDS
#define MINUTE MINUTES
#define HOUR   HOURS
#define DAY    DAYS
#define WEEK   WEEKS

typedef void *ident;
extern void  MainLoop(void);
extern ident ScheduleRead(void(*)(),int,void*);
extern ident ScheduleWrite(void(*)(),int,void*);
extern ident ScheduleTimer(void(*)(),int,void*);
extern void  Deschedule(ident*);
extern void  DescheduleAll(void);
extern void  BreakMainLoop(void);
#endif
