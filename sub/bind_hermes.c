/* bind_hermes.c
                              COPYRIGHT (c) 1990
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>       bind_hermes.dc3
Function:    bind_hermes

Purpose:     select a communication method between an application
             program and the user control process (Hermes).

Category:    USER-INTERFACE, SYSTEM

File:        bind_hermes.c
             
Author:      J.P. Terlouw
             
Use:         int bind_hermes(&sendproc, &receiveproc);
                               ONLY called from init_c in srvreq.c !
                          bind_hermes  -- 0: success; 1: error
                          sendproc     -- pointer to sender procedure
                          receiveproc  -- pointer to receiver procedure
                          
Description: bind_hermes used to assign values to the pointers 
             on the basis of a two-letter argument to the
             program (either on the command line or via exec).
             Now a fixed method of communication is used.
             The methods are normally implemented as static procedures
             in this module. Exceptions were the stand-alone Hermes
             routines (nohermes). These routines are not normally called by
             this module. To include them, this module should be compiled
             with -DNOHERMES.

Updates:     15-Oct-1990  --  original document
              4-Dec-1990  --  bug in socket pair method fixed
             11-Mar-1991  --  fixed infinite loop after Hermes crash
             27-May-1991  --  pipe methods added
             16-Oct-1991  --  code added to handle interrupted read() calls
              6-Jan-1993  --  suppress inclusion of nohermes routines
             13-Oct-1993  --  eliminate internal I/O buffer
              7-Feb-1995  --  return status
             20-Mar-1995  --  replace writev by sock_writev for Linux
             24-Nov-2008  --  socket pair method is now "hardwired"
#<
*/

#include "errno.h"
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "gipsyc.h"
#include "hercom.h"

struct iovec {
	char	*iov_base;
	int	iov_len;
};
int write  (int, char*, int);
int writev (int, struct iovec*,int);
int read   (int, char*, int);
 
#if defined(NOHERMES)
extern void SA_send_msg(RequestMessage *);              /* stand alone hermes */
extern int  SA_recv_msg(char*,int);
#else
static void SA_send_msg(RequestMessage *);              /* stand alone hermes */
static int  SA_recv_msg(char*,int);
#endif

static int Read(int fd, char *buffer, int n);   /* checking read() substitute */

static void SP_send_msg(RequestMessage *);              /* socket pair IPC    */
static int  SP_recv_msg(char *, int);
static void PI_send_msg(RequestMessage *);              /* pipe        IPC    */
static int  PI_recv_msg(char *, int);
static int  SP_flag=0;
static int  PI_flag=0;

extern int bind_hermes(send_proc *send_msg, recv_proc *recv_msg)
{
   char   arg[3];
   fchar farg;
   fint    one =1;
   char *names[]       = { "SA",       "SP",        "PI",        0 };
   send_proc senders[] = { SA_send_msg, SP_send_msg, PI_send_msg };
   recv_proc recvers[] = { SA_recv_msg, SP_recv_msg, PI_recv_msg };
   int i;
    
   farg.a = arg; farg.l = sizeof(arg);
   /* Here we used to call getcla() in order to determine the inter-process
    * communication method. This method is now "hardwired" as "SP", which
    * has been the sole method for many years anyway.
    */
   strcpy(farg.a, "SP");
   arg[2] = 0;
   for (i=0; names[i]; i++) {
      if (!strcmp(arg,names[i])) {
         *send_msg = senders[i];
         *recv_msg = recvers[i];
         break;
      }
   }
   if (!names[i]) {    /* name not found - use stand-alone Hermes as default */
      *send_msg = SA_send_msg;
      *recv_msg = SA_recv_msg;
   }
#if defined(NOHERMES)
   return 0;
#else
   return *send_msg == SA_send_msg;
#endif
}

/*
   Socket pair methods. These methods communicate through a bi-directional
   socket, which has been dup'ed to file descriptor 3. In this way 
   descriptors 1 and 2 remain available for standard output and standard
   error, which should normally be redirected via a pipe or socket.
   The socket pair methods should be suitable for almost any kind of
   master control process, provided it runs in the same machine as the
   servant.
*/
   
static void SP_send_msg(RequestMessage *request)
{
   struct iovec iov[2];
   
   iov[0].iov_base = (char*)request;
   iov[0].iov_len  = MSGHED;
   iov[1].iov_base = (char*)request->request;
   iov[1].iov_len  = request->reqlen;
#if defined(__APPLE__) || defined(__linux__)
   if (sock_writev(3,iov,2)) {
#else
   if (writev(3,iov,2)!=(MSGHED + request->reqlen)) {
#endif
      fprintf(stderr, "SP_send_msg -- failed to send request.\n");
      exit(1);
   }
   SP_flag = 1;
}

static int SP_recv_msg(char* bufout, int size)
{
   static int  out=1, in=0, done=0;
   int    num, part, nbytes;
   char dumbuf[4096];
    
   if (SP_flag) {                                   /* new reply message ? */
      SP_flag = 0;
      while (done<in) {
         part = Read(3,dumbuf,in-done);
         if (part<=0) {
            fprintf(stderr,"SP_recv_msg -- failure while flushing\n");
            exit(1);
         }
         done += part;
      }

      Read(3,(char*)&in,sizeof(in));                /* get the next one.   */
      if (in<1) {
         fprintf(stderr,"SP_recv_msg -- bad length code: %d\n",in);
         exit(1);
      }
      done = 0;
   }

   nbytes = (in-done)<size?(in-done):size;
   for( num=0; num<nbytes; num += part) {
      part = Read(3,bufout+num,nbytes-num);
      if (part<=0) {
         fprintf(stderr,"SP_recv_msg -- failed to read reply.\n");
         exit(1);
      }
   }
   done += num;
   return nbytes;
}

/*
   Pipe methods. These methods communicate through two pipes; one for
   sending and one for receiving. The have been dup'ed to file descriptors
   3 and 4.
   Descriptors 1 and 2 remain available for standard output and standard
   error, which should normally be redirected via a pipe or socket.
*/

static void PI_send_msg(RequestMessage *request)
{
   struct iovec iov[2];
   
   iov[0].iov_base = (char*)request;
   iov[0].iov_len  = MSGHED;
   iov[1].iov_base = (char*)request->request;
   iov[1].iov_len  = request->reqlen;
#if  defined(__linux__)
   if (sock_writev(3,iov,2)) {
#else
   if (writev(3,iov,2)!=(MSGHED + request->reqlen)) {
#endif
      fprintf(stderr, "PI_send_msg -- failed to send request.\n");
      exit(1);
   }
   PI_flag = 1;
}

static int PI_recv_msg(char* bufout, int size)
{
   static int  out=1, in=0, done=0;
   int    num, part, nbytes;
   char dumbuf[4096];
    
   if (PI_flag) {                                   /* new reply message ? */
      PI_flag = 0;
      while (done<in) {
         part = Read(4,dumbuf,in-done);
         if (part<=0) {
            fprintf(stderr,"PI_recv_msg -- failure while flushing\n");
            exit(1);
         }
         done += part;
      }

      Read(4,(char*)&in,sizeof(in));                /* get the next one.   */
      if (in<1) {
         fprintf(stderr,"PI_recv_msg -- bad length code: %d\n",in);
         exit(1);
      }
      done = 0;
   }

   nbytes = (in-done)<size?(in-done):size;
   for( num=0; num<nbytes; num += part) {
      part = Read(4,bufout+num,nbytes-num);
      if (part<=0) {
         fprintf(stderr,"PI_recv_msg -- failed to read reply.\n");
         exit(1);
      }
   }
   done += num;
   return nbytes;
}

#if !defined(NOHERMES)
/*
 *  Replacement routines for obsolete (obsolescent?) nohermes communication
 *  routines.
 */
static void SA_send_msg(RequestMessage *request)
{
   printf("\n --- Cannot run without Hermes.\n");
   exit(1);
}
   
static int SA_recv_msg(char* bufout, int size)
{
   printf("\n --- Cannot run without Hermes.\n");
   exit(1);
   return 0;
}
#endif

/*
 *  Read() is a substitute for read() system call. If the latter is interrupted
 *  by a signal, it is called again.
 */
static int Read(int fd, char *buffer, int n)
{
   int status;
    
   for (;;) {
      status = read(fd,buffer,n);
      if (status>=0 || (status<0 && errno!=EINTR)) break;
   }
   return status;
}
