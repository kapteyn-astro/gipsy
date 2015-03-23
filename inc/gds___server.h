#if !defined(_gds___server_h_)
#define _gds___server_h_
typedef void* gdsserver;
extern gdsserver gds___server(char *name);
extern int       gds___srvreq(gdsserver, void*, int, int*);
extern int       gds___srvsnd(gdsserver id, void* data, int nbytes);
extern int       gds___srvrcv(gdsserver id, void* data, int nbytes);
extern char     *gds___srvnam(gdsserver id, char *fullname);
extern void      gds___srvcls(void);
extern int       gds___srverr(void);
#endif
