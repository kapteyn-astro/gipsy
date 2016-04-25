/* gds___server.c
                              COPYRIGHT (c) 1995
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands
*/

#define IDENTFILE ".gds_ident"
#define DESCR     ".descr"
#define LNBLEN    256

#include "stddef.h"

#ifdef	__linux__
#define	__LINUX_UIO_H
#endif
#include <sys/types.h>
#include <sys/uio.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/stat.h>


#if defined(__linux__) | defined(__APPLE__)
#include "sockio.h"
#endif

#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "xscanf.h"
#include "netaddr.h"
#include "gdscom.h"
#include "userfio.h"
#include "gds___server.h"

#include <sys/stat.h>
#include <fcntl.h>


typedef struct _server {   
   struct _server *next;    /* link to next block */
   char      *dir;          /* directory name */
   char      *id;           /* server id */
   int        fd;           /* communication socket */
   fint       result;       /* last operation's result code */
   fint       nbytes;       /* number of bytes to be read or written */
} _server, *server;    /* ------------------------------ server status block */

#define New(type) ((type *)calloc(1,sizeof(type)))
#define NNew(n,type) ((type *)calloc(n,sizeof(type)))
#define Delete(x)  {free(x); x=NULL;}

static char *StrDup(char*);
static char *RealName(char *name);
static char *StrDup(char*);
static int ServerConnect(server srv, char* type);
static char *gds___dirid(char *dirname);
static server srvlist=NULL;
static int srverr;
static int setname_is64bit(char *name);


/* ========================================================================== */
/*                             gds___server                                   */
/* -------------------------------------------------------------------------- */
/*
#>gds___server.dc3
Function:     gds___server

Purpose:      Obtain a GDS server status block pointer, given a set name.

Category:     GDS, SYSTEM

File:         gds___server.c

Author:       J.P. Terlouw

Use:          #include "gds___server.h"
              gdsserver gds___server(char *setname);

Description:  gds___server() obtains a pointer to a structure associated with
              a gds server process. This pointer can subsequently be used in
              calls to gds___srvreq(), gds___srvrcv(), and gds___srvsnd().
              In case of failure NULL is returned. In this case gds___srverr()
              can be called to obtain the error code.

Related docs: gds___srvreq.dc3, gds___srvsnd.dc3, gds___srvrcv.dc3, 
              gds___srvcls.dc3, gds___srvnam.dc3, gds___srverr.dc3.

Updates:      Mar 28, 1995: JPT, Document created.
              May 10, 1995: JPT, Reduced number of I/O's in gds___srvreq.
              Sep  1, 1995: JPT, Fixed inaccessible directory bug.
              Sep 28, 1995: JPT, Translate symbolic links.
              Sep 29, 1995: JPT, Fixed absolute symbolic link bug.
              Sep  2, 1996: JPT, Small code change by KGB.
              Feb 16, 1998: JPT, Forcefully close all sets when fd table full.
              Nov 19, 1998: JPT, Retry in case of bad sockets file.
              Sep 19, 2000: JPT, Check GDS server startup failure.
              May  1, 2007: JPT, Conditional code for Apple Mac included.
              Mar  8, 2011: JPT, Fixed problem with uninitialized st_dev.
#<
*/

extern gdsserver gds___server(char *name)
{
   server current;
   char   *dir, *srvid, *dirid;
   int    i;
    
   dir = RealName(name);
   for (i=strlen(dir)-1; i>0; i--) {
      if (dir[i-1]=='/') {
         dir[i] = '\0';
         break;
      }
   }

/*
 * Check whether directory name is already served.
 */
   for (current=srvlist; current; current=current->next) {
      if (!strcmp(dir,current->dir)) {
         Delete(dir);
         return (gdsserver)current;
      }
   }
   
/*
 * Check whether directory is served under a different name.
 */
   dirid = gds___dirid(dir);
   if (!dirid) {
      srverr = GDS_BADDIR;
      return (gdsserver)NULL;
   }
   srvid = StrDup(dirid);
   for (current=srvlist; current; current=current->next) {
      if (!strcmp(srvid,current->id)) {
         Delete(current->dir);                      /* delete oldest name */
         current->dir = dir;                        /* keep newest name   */
         Delete(srvid);
         return (gdsserver)current;
      }
   }
   
   
/*
 * No appropriate server known: find server.
 */
   current      = New(_server);
   current->dir = dir;
   current->id  = srvid;
   char* type = "";
   if(setname_is64bit(name)) {
	   type = "64";
   }
   printf("connect: type: %s\n", type);
   fflush(stdout);
   if (!ServerConnect(current, type)) {
      current->next = srvlist;
      srvlist       = current;
   } else {
      Delete(current->dir);
      Delete(current->id);
      Delete(current);
      srverr = GDS_CONNFAIL;
   }
   return (gdsserver)current;
}

/* ========================================================================== */
/*                                 gds___srvreq                               */
/* -------------------------------------------------------------------------- */
/*
#>gds___srvreq.dc3
Function:     gds___srvreq

Purpose:      Issue a request to a GDS server.

Category:     GDS, SYSTEM

File:         gds___server.c

Author:       J.P. Terlouw

Use:          #include "gds___server.h"
              int gds___srvreq(gdsserver id,
                               void *request,
                               int size,
                               int *nbytes)
   
              id         server 'object', obtained by calling gds___server().
              request    request to server. Requests are defined in gdscom.h.
              size       size of request (bytes).
              nbytes     output argument. If not NULL, it will receive the
                         number of bytes to be read or written as a result
                         of this request.
               
              return value: zero on success; non-zero in case of errors.
              
              A set name in a request must be specified relative to the
              directory in which the set resides.

Related docs: gds___server.dc3, gds___srvsnd.dc3, gds___srvrcv.dc3, 
              gds___srvcls.dc3, gds___srvnam.dc3.

Updates:      Mar 28, 1995: JPT, Document created.
              May 10, 1995: JPT, Reduced number of I/O's.
#<
*/
extern int gds___srvreq(gdsserver id, void *request, int size, int *nbytes)
{
   server current=(server)id;
   fint rsize=(fint)size;
   struct {
      fint       size;
      _uRequest  request;
   } reqmsg;
   
   _gdsReply replymsg;

   reqmsg.size = rsize;
   memcpy(&reqmsg.request, request, size);
   write(current->fd, &reqmsg, size+sizeof(reqmsg.size));

   read(current->fd, &replymsg, sizeof(replymsg));
   current->result = replymsg.result;
   current->nbytes = replymsg.length;
   if (nbytes) *nbytes = current->nbytes;
   return current->result;
}

/* ========================================================================== */
/*                                 gds___srvsnd                               */
/* -------------------------------------------------------------------------- */
/*
#>gds___srvsnd.dc3
Function:     gds___srvsnd

Purpose:      send data associated with the current request to a GDS server.

Category:     GDS, SYSTEM

File:         gds___server.c

Author:       J.P. Terlouw

Use:          #include "gds___server.h" 
              int gds___srvsnd( gdsserver id, void* data, int nbytes)
              
              id         server 'object', obtained by calling gds___server().
              data       data to be sent
              nbytes     number of bytes to be sent.
              
              return value:
              success:   number of bytes still to be transferred;
              error:     -1

Related docs: gds___server.dc3 gds_srvreq.dc3, gds___srvrcv.dc3,
              gds___srvcls.dc3, gds___srvnam.dc3.

Updates:      Mar 28, 1995: JPT, Document created.
#<
*/
extern int gds___srvsnd(gdsserver id, void* data, int nbytes)
{
   server current=(server)id;
   int    result;
   
   if ((nbytes>=0) && (nbytes <= current->nbytes)) {
      if (nbytes) {
#if defined(__linux__) | defined(__APPLE__)
         (void)sock_write(current->fd,data,nbytes);
#else
         (void)write(current->fd,data,nbytes);
#endif
      }
      current->nbytes -= nbytes;
      result = current->nbytes;
   } else result = -1;
   return result;
}
   
/* ========================================================================== */
/*                                 gds___srvrcv                               */
/* -------------------------------------------------------------------------- */
/*
#>gds___srvrcv.dc3
Function:     gds___srvrcv

Purpose:      receive data associated with the current request from GDS server.

Category:     GDS, SYSTEM

File:         gds___server.c

Author:       J.P. Terlouw

Use:          #include "gds___server.h" 
              int gds___srvrcv( gdsserver id, void* data, int nbytes)
              
              id         server 'object', obtained by calling gds___server().
              data       data to be received
              nbytes     number of bytes to be received.
              
              return value:
              success:   number of bytes still to be transferred;
              error:     -1

Related docs: gds___server.dc3 gds_srvreq.dc3, gds___srvsnd.dc3,
              gds___srvcls.dc3, gds___srvnam.dc3.

Updates:      Mar 28, 1995: JPT, Document created.
#<
*/
extern int gds___srvrcv(gdsserver id, void* vdata, int nbytes)
{
   server current=(server)id;
   char   *data=(char*)vdata;
   int    result;
   int    nread=0;
   
   if (nbytes>=0) {
      while (nbytes>0) {
         nread = read(current->fd,data,nbytes);
         if (nread>=0) {
            nbytes -= nread;
            current->nbytes -= nread;
            data   += nread;
            if (current->nbytes <= 0) break;
         } else {
            return -1;
         }
      }
      result = current->nbytes;
   } else result = -1;
   return result;
}

/* ========================================================================== */
/*                                 gds___srvcls                               */
/* -------------------------------------------------------------------------- */
/*
#>gds___srvcls.dc3
Function:     gds___srvcls

Purpose:      Close all GDS server connections

Category:     GDS, SYSTEM

File:         gds___server.c

Author:       J.P. Terlouw

Use:          #include "gds___server.h"
              void gds___srvreq( void )

Related docs: gds___server.dc3, gds___srvsnd.dc3, gds___srvrcv.dc3, 
              gds___srvreq.dc3, gds___srvnam.dc3.

Updates:      Mar 28, 1995: JPT, Document created.
#<
*/
extern void gds___srvcls(void)
{
   server current;
    
   while (current=srvlist) {
      srvlist = current->next;
      close(current->fd);
      Delete(current->dir);
      Delete(current->id);
      Delete(current);
   }
}

/* ========================================================================== */
/*                                 gds___srvnam                               */
/* -------------------------------------------------------------------------- */
/*
#>gds___srvnam.dc3
Function:     gds___srvnam

Purpose:      Obtain GDS server-relative set name, given the full set path.

Category:     GDS, SYSTEM

File:         gds___server.c

Author:       J.P. Terlouw

Use:          #include "gds___server.h"
              char *gds___srvnam(gdsserver id, char *name )

              id         server 'object', obtained by calling gds___server().
              name       full-path set name.
              
              return value:
              pointer to set name as expected by the GDS server

Related docs: gds___server.dc3, gds___srvsnd.dc3, gds___srvrcv.dc3, 
              gds___srvreq.dc3, gds___srvcls.dc3.

Updates:      Mar 28, 1995: JPT, Document created.
              Sep 28, 1995: JPT, Translate symbolic links
              Sep 29, 1995: JPT, Fixed absolute symbolic link bug.
#<
*/
extern char *gds___srvnam(gdsserver id, char *name)
{
   static char result[GDS_NAMLEN+1];
   char        *cp;
   char        *fullname;
    
   fullname = RealName(name);
   for(cp=fullname+strlen(fullname)-1; cp>fullname; cp--) {
      if (*(cp-1)=='/') break;
   }
   strcpy(result,cp);
   Delete(fullname);
   return result;
}

/* ========================================================================== */
/*                                 gds___srverr                               */
/* -------------------------------------------------------------------------- */
/*
#>gds___srverr.dc3
Function:     gds___srverr

Purpose:      Obtain GDS error code associated with server connect failure

Category:     GDS, SYSTEM

File:         gds___server.c

Author:       J.P. Terlouw

Use:          #include "gds___server.h"
              int gds___srverr(void)

              return value:
              GDS error code; only valid after a failed call to gds___server().

Related docs: gds___server.dc3

Updates:      Sep  1, 1995: JPT, Document created.
#<
 */
extern int gds___srverr(void)
{
   return srverr;
}
/* -------------------------------------------------------------------------- */
/*                                 ServerConnect                              */
/* -------------------------------------------------------------------------- */
static int ServerConnect(server current, char* type)
{

   int    result, nbytes, status;
   int    fd, fd2, pwd, nf;
   int    started=0;
   FILE   *f;
   char   filename[SNLEN+1];
   char   inet_name[SNLEN+1];
   char   unix_name[SNLEN+1];
   char   host[SNLEN+1];
   
   union {
      fint l;
      char b[sizeof(fint)];
   } u;
   
   _gdsConnect request;
    
   fd  = dup(0);
   fd2 = dup(0);
   if (fd2<0) {
      fint one=1;
      error_c(&one,tofchar("gds___server: temporary close of all sets"));
      gds_closeall_c(); /* try to obtain sufficient file descriptors */
   }
   close(fd);
   close(fd2);

   sprintf(filename,"%s/.gds%s_sockets%s",getenv("HOME"),type,current->id);
   if (!(f = fopen(filename,"r"))) {
      /* no sockets file: start server */
      int pid=fork();
      if (!pid) {
                                  /* ---- child (server) process branch ---- */
         char execpath[200];
         char execname[100];
         if (chdir(current->dir)) {
            printf("chdir failed\n");
            exit(1);
         }
		 sprintf(execname, "gdsserver%s", type);
         sprintf(execpath, "%s/%s", getenv("gip_exe"), execname);
         //strcpy(execname,getenv("gip_exe"));
         //strcat(execname,"/gdsserver");
		 printf("starting %s as %s\n", execpath, execname);
         if (execl(execpath,execname,current->id,NULL)) exit(1);
		 //if (execl(execname,"gdsserver",current->id,NULL)) exit(1);
      } else {
                                 /* ---- parent (client) process branch ---- */
         waitpid(pid, &status, 0);             /* wait until server is ready */
         if (status) errorf(4, "GDS server failure - cannot write in $HOME");
         started = 1;
         if (!(f = fopen(filename,"r"))) return -1;  /* sockets file problem */
      }
   }
   nf = xscanf( f, "%*s %*s %*s", 
               SNLEN, host, SNLEN, unix_name, SNLEN, inet_name);
   fclose(f);
   if (nf<2) {
      remove(filename);                                  /* bad sockets file */
      return ServerConnect(current,type);
   }
   
   if (!strcmp(host,netaddr())) {
                                     /* server on this host: use UNIX socket */
      struct sockaddr_un  uxaddr;              /* unix socket address struct */
       
      fd = socket( AF_UNIX, SOCK_STREAM, 0 );               /* create socket */
      if (fd<0) return 0;
      uxaddr.sun_family = AF_UNIX;
      strcpy( uxaddr.sun_path, unix_name );
      if (connect(fd,(struct sockaddr *)&uxaddr, sizeof(uxaddr)) < 0 ) {
      /* failed to connect - if existing sockets file: try again, else fail */
         close(fd);
         remove(filename);
         return started ? -1 : ServerConnect(current,type);
      }
   } else {
                                    /* 'foreign' server: use Internet socket */
      short               port;              /* inet port number */
      struct hostent      *hp;               /* host entry struct */
      struct hostent      *gethostbyname( ); /* get host info */
      struct sockaddr_in  inaddr;            /* inet socket address struct */
          
      fd = socket( AF_INET, SOCK_STREAM, 0);              /* create socket */
      if (fd<0) return 0;
      port = atoi(inet_name);
      inaddr.sin_addr.s_addr = inet_addr(host);
      inaddr.sin_family = AF_INET;
      inaddr.sin_port   = htons(port);
      if (connect(fd,(struct sockaddr *)&inaddr, sizeof(inaddr)) < 0 ) {
      /* failed to connect - if existing sockets file: try again, else fail  */
         close(fd);
         remove(filename);
         return started ? -1 : ServerConnect(current, type);
      }
   }
   /*
    *  Try to get write permission, or else read permission.
    */
   sprintf(filename,"%s/.gds%s_write",getenv("HOME"), type);
   f = fopen(filename,"r");
   if (!f) {
      sprintf(filename,"%s/.gds%s_read",getenv("HOME"), type);
      f = fopen(filename,"r");
   }
   if (f) {
      fscanf(f, "%d", &pwd);
      fclose(f);
   }
   current->fd = fd;
   request.head.code  = GDS_CONNECT;
   u.b[0]             = OS_INTEGER_TYPE;
   u.b[1]             = OS_FLOATING_TYPE;
   request.head.id    = u.l;
   request.pwd        = pwd;
   if(strcmp(type, "64") == 0) {
	request.version    = VERSION;
	request.subversion = SUBVERSION;
   } else {
	request.version    = VERSION_32;
	request.subversion = SUBVERSION_32;
   }
   result = gds___srvreq((gdsserver)current, (void*)&request,
                          sizeof(request), &nbytes);
   if (result) close(fd);
   return result;
}


/* -------------------------------------------------------------------------- */
/*                                 gds___dirid                                */
/* -------------------------------------------------------------------------- */
/*  gds___dirid() returns a unique identification string associated with
 *  the directory specified in the argument.
 */
static char *gds___dirid(char *dirname)
{
   int    status, nf;
   long   device=0, inode=0; 
   struct stat statbuf;
   FILE   *f;
   static char result[50];
   char   filename[128];
    
   strcpy(filename,dirname);
   strcat(filename,IDENTFILE);
   status = stat(dirname, &statbuf);         /* check directory accessibility */
   if (status) return NULL;
   
/* Try to read directory identification file.
 * If the inode number is this file is consistent with inode found by stat(),
 * assume that this is a correct ident file.
 */
   f = fopen(filename,"r");
   if (f) {
      nf = xscanf(f, "%d %d",&device, &inode);
      fclose(f);
      if ((nf == 2) && (statbuf.st_ino == inode)) {
         sprintf(result,"%ld_%ld",device,inode);
         return result;
      }
   }

/* Ident file non-(existent|consistent): use information from stat() and
 * try to re-write ident file.
 */
   device = statbuf.st_dev;
   inode  = statbuf.st_ino;
   f = fopen(filename,"w");
   if (f) {
      fprintf(f,"%ld:%ld", device, inode);
      fclose(f);
   }
   sprintf(result,"%ld_%ld",device,inode);
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 RealName                                   */
/* -------------------------------------------------------------------------- */
/* RealName() resolves any symbolic links in the argument name and returns
 * the resulting name.
 */
static char *RealName(char *name)
{
   int    i;
   char   linkbuffer[LNBLEN]; 
   int    linklen;
   char  *result;
   
   result = NNew(strlen(name)+strlen(DESCR)+1,char);
   strcpy(result,name); strcat(result,DESCR);
   while ((linklen = readlink(result,linkbuffer,LNBLEN)) >0) {
      if (linkbuffer[0]=='/') {
         result[0] = '\0';                    /* link is absolute path */
      } else {
         for (i=strlen(result)-1; i>0; i--) {
            if (result[i-1]=='/') {
               result[i] = '\0';
               break;
            }
         }                                    /* strip name of symbolic link  */
      }
      result = realloc(result,strlen(result)+linklen+1);
      linkbuffer[linklen] = '\0';          /* append name pointed to       */
      strcat(result,linkbuffer);
   }
   for (i=strlen(result)-1; i>0; i--) {
      if (result[i]=='.') {
         result[i] = '\0';
         break;
      }
   }                                       /* strip off file name extension */
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 StrDup                                     */
/* -------------------------------------------------------------------------- */
/* StrDup() makes a duplicate if the argument string.
 */
static char *StrDup (char *orig)
{
   char *result=NNew(strlen(orig)+1,char);
   
   strcpy(result,orig);
   return result;
}
/*
typedef struct {
   fint   version;
   fint   subversion;
} _header, *header;
*/
static int setname_is64bit(char *name) {
//return 1; }
	//*
	int fd;
	fint version;
	char desc_name[200];
	strcpy(desc_name, name);
	strcat(desc_name, DESCR);
   fd = open(desc_name,O_RDONLY,NULL);
   if (fd<0) {
	   /* if file does not exist yet, we'll make it a 64 bit file */
	   anyoutf(1, "file %s does not exist yet, we will use the 64 bit server", desc_name);
	   return 1;
   }
   read(fd, &version, sizeof(fint));
   anyoutf(1, "file %s is %s", desc_name, (version >= 3) ? "64 bit" : "32 bit");
   return (version >= 3);
}/**/



/* -------------------------------------------------------------------------- */
/*                                 gds___server.h                             */
/* -------------------------------------------------------------------------- */
/*  Declarations and prototypes.

#>gds___server.h
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
#<
*/
