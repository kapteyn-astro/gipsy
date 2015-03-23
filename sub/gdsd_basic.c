/*  gdsd_basic.c
                              COPYRIGHT (c) 1994
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

#>gdsd_basic.dc2
Document:     GDSD_BASIC

Purpose:      Describes the basic GDS system.

Category:     GDS

File:         gdsd_basic.c

Author:       J.P. Terlouw

Description:  

Updates:      Mar 24, 1994: JPT document created.
              Mar 17, 1995: JPT corrected incorrect execl() call.
              Mar 20, 1995: JPT replace write by sock_write for Linux.
              Mar 30, 1995: JPT revised to cooperate with gds___server.
              May  8, 1995: JPT implemented function gds_recover.
              Jun 26, 1995: JPT repaired gds_delete.
              Sep  1, 1995: JPT improved error reporting
              Jul 21, 2000: JPT improved set handles;
                                fixed working directory problem.
#<
*/

#include "stdlib.h"
#include "fcntl.h"
#include "string.h"
#include "stdio.h"
#include "stdlib.h"
#include "stddef.h"
#include "gdscom.h"
#include "gds_tune.h"
#include "presentn.h"
#include "gds___error.h"
#include "gds___server.h"
#include "gdsd_basic.h"

#define ISSUE(s,r)   gds___srvreq(s,(void*)&r,sizeof(r),NULL)
#define PUTDATA(s,d) (void)gds___srvsnd(s,(void*)&d,sizeof(d));
#define GETDATA(s,d) (void)gds___srvrcv(s,(void*)&d,sizeof(d));

/* Maximum and minimum of two items                                          */
#if	defined(MAX)
#undef	MAX
#endif
#define	MAX(a,b)	((a)>(b)?(a):(b))
#if	defined(MIN)
#undef	MIN
#endif
#define MIN(a,b)	((a)<(b)?(a):(b))

/* shorthand for fchar to char conversion and vice versa                     */
#define CTS(a,b) (void)gds___char2str(a,b,sizeof(b))
#define STC(a,b) (void)gds___str2char(a,b)

/* 'object' allocation and de-allocation macro's                             */
#define New(type) ((type *)calloc(1,sizeof(type)))
#define Delete(x)  {free(x); x=NULL;}

typedef struct _setinf {
   struct _setinf *current; /* pointer to this block (for checking) */
   struct _setinf *next;    /* link to next block */
   fint       id;           /* set id */
   int        fi;           /* file descriptor of image component */
   int        lockdepth;    /* locking depth */
   gds_coord  *coords;      /* chache for origins, sizes, factors, #axes */
   void       (*iclose)();  /* image closing routine */
   gdsserver  server;       /* server for this set */
   char       name[NAMLEN]; /* name of the set */
} _setinf, *setinf; /* ------------------------------- set information block */

int gds___abslevel(void);
char *getcwd(char *buf, size_t size);
int close(int fd);

static setinf SetOpen(char *name, int incr, fint *err);
static void   absname(char *name, char *sname);
static fint   SetClose(setinf handle);
static setinf SetHandle(fchar name, int noopen, fint *err);

static setinf  setlist=NULL;

/* ========================================================================== */
/*                                 GDS_HANDLE                                 */
/* -------------------------------------------------------------------------- */
/*
@character function gds_handle( character, integer )
#>gds_handle.dc2
Function:     GDS_HANDLE

Purpose:      Obtain a GDS set handle.

Category:     GDS

File:         gdsd_basic.c

Author:       J.P. Terlouw

Use:          CHARACTER GDS_HANDLE (            Return    CHARACTER*(*)
                                     SET,       Input     CHARACTER*(*)
                                     ERR )      Input     INTEGER

              GDS_HANDLE  Returns the set handle.            character*(*)
              SET         Name of set for which the handle is wanted.
              ERR         Error return code.
              
Description:  GDS_HANDLE translates a setname into a 'set handle'.
              A set handle is a special kind of set identifier which
              can be used instead of a set name in every GDS call as long
              as the set is open. The advantage of using a set handle
              instead of a set name is that set handles are more efficient
              because a number of string comparisons can be bypassed.

Updates:      Mar  2, 1994: JPT, Document created.
#<
------------------------------------------------------------------------------*/
extern void gds_handle_c(fchar funval, fchar set, fint *err)
{
   setinf handle;
    
   handle = SetHandle(set, 0, err);
   if (!handle) return;
   sprintf(funval.a, "#%p", handle);
}

/* ========================================================================== */
/*                                 GDS_CREATE                                 */
/* -------------------------------------------------------------------------- */
/*
@subroutine gds_create( character, integer )
#>gds_create.dc2
Subbroutine:   GDS_CREATE

Purpose:       Create a new set.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDS_CREATE ( SET,          Input   CHARACTER*(*)
                                 ERROR )       Output  INTEGER

                  SET      Name of set to be created.
                  ERROR    Error return code.

               The structure created by this call is initially 
               zero-dimensional. Its shape can be determined later by
               calling GDS_EXTEND one or more times.

Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
               8-Mar-94   rewritten for GDS server
#<
-----------------------------------------------------------------------------*/
extern void gds_create_c (fchar set, fint *err)
{
   gdsserver    server;
   _setCreate request;
   char       set_i[NAMLEN], set_a[NAMLEN];
   int        result;
    
   CTS(set,set_i);
   absname(set_i,set_a);
   server = gds___server(set_a);
   if (gds___fail((int)server,gds___srverr(),err)) return;
   request.head.code = SET_CREATE;
   request.head.id   = 0;
   request.head.imm  = 0;
   request.size      = gds___initsize();
   request.incr      = gds___extendsize();
   request.prime     = gds___prime();
   strcpy(request.name, gds___srvnam(server,set_a));
   result = ISSUE(server,request);
   if (gds___fail(!result, result, err)) return;
}

/* ========================================================================== */
/*                                 GDS_CLOSE                                  */
/* -------------------------------------------------------------------------- */
/*
@subroutine gds_close(character,integer)
#>gds_close.dc2
Subroutine:    GDS_CLOSE

Purpose:       Close set.

Category:      GDS  

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDS_CLOSE ( SET,            Input   CHARACTER*(*)
                                ERROR )         Output  INTEGER

               SET         Name or handle of set.
               ERROR       Error return code.

Description:   Both the descriptor and the image part are closed.
               A task should use this routine when it is finished with a
               set.

See also:      gds_closeall.dc2

Updates:      25-May-87   original document
              15-Dec-89   rewritten in C
               3-Mar-94   rewritten for GDS server
#<
-----------------------------------------------------------------------------*/
extern void gds_close_c(fchar set, fint *err)
{
   setinf    handle;
   fint      result;
    
   handle = SetHandle(set, 0, err);
   if (!handle) return;
   result = SetClose(handle);
   if (gds___fail(!result, result, err)) return;
}

/* ========================================================================== */
/*                                 GDS_CLOSEALL                               */
/* -------------------------------------------------------------------------- */
/*
@subroutine gds_closeall ( )
#>gds_closeall.dc2
Subroutine:    GDS_CLOSEALL

Purpose:       Close all sets.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDS_CLOSEALL

Description:   All open sets are closed and the connection to the GDS server
               is shut down.
               A task can call this routine when it is finished with all
               sets it has open. After this call sets still can be opened.
               In this case a new connection to the GDS server will be
               established.
               
See also:      gds_close.dc2

Updates:       Apr 21, 1994: JPT, Document created.
#<
------------------------------------------------------------------------------*/
extern void gds_closeall_c(void)
{
   (void)SetClose(NULL);
}

/* ========================================================================== */
/*                                 GDS_FTYPE                                  */
/* -------------------------------------------------------------------------- */
/*
#>            gds_ftype.dc2
Function:     GDS_FTYPE

Purpose:      Returns the Floating point type of a set.

Category:     GDS

File:         gdsd_basic.c

Author:       J.P. Terlouw

Use:          INTEGER GDS_FTYPE( SET ,    Input    CHARACTER*(*)
                                 ERROR )  Output   INTEGER

              GDS_FTYPE     Returns the type of floating point used in
                            the data set. Can be one of:
                            0 = IEEE High Endian (BLANK=-Inf)
                            1 = IEEE Low Endian (BLANK=-Inf)
                            2 = CONVEX Native format
                            3 = VAX D Floating
                            4 = VAX G Floating
                            5 = IEEE High Endian (BLANK=-FLT_MAX)
                            6 = IEEE Low Endian (BLANK=-FLT_MAX)
              SET           Name or handle of set.
              ERROR         Error return code.

Updates:      Jun  5, 1992: KGB, document created.
              Mar  9, 1994: JPT, rewritten for GDS server.
#<

Fortran to C interface:
@ integer function gds_ftype( character, integer )
------------------------------------------------------------------------------*/
extern fint	gds_ftype_c( fchar set, fint *err )
{
   setinf    handle;
   _setFtype request;
   fint      result;
   
   handle = SetHandle(set, 0,  err);
   if (!handle) return -1;
   request.head.code = SET_FTYPE;
   request.head.id   = handle->id;
   request.head.imm  = 0;
   result = ISSUE(handle->server,request);
   if (gds___fail(!result, result, err)) return -1;
   GETDATA(handle->server,result)
   return result;
}

/* ========================================================================== */
/*                                 GDS_ITYPE                                  */
/* -------------------------------------------------------------------------- */
/*
#>            gds_itype.dc2
Function:     GDS_ITYPE

Purpose:      Returns the Integer type of a set.

Category:     GDS

File:         gdsd_basic.c

Author:       J.P. Terlouw

Use:          INTEGER GDS_ITYPE( SET ,    Input    CHARACTER*(*)
                                 ERROR )  Output   INTEGER

              GDS_ITYPE     Returns the type of integer used in
                            the data set. Can be one of:
                            0 = High Endian
                            1 = Little Endian
              SET           Name or handle of set.
              ERROR         Error return code.

Updates:      Mar  9, 1994: JPT, document created.
#<

Fortran to C interface:
@ integer function gds_itype( character, integer )
------------------------------------------------------------------------------*/
extern fint	gds_itype_c( fchar set, fint *err )
{
   setinf    handle;
   _setItype request;
   fint      result;
   
   handle = SetHandle(set, 0, err);
   if (!handle) return -1;
   request.head.code = SET_ITYPE;
   request.head.id   = handle->id;
   request.head.imm  = 0;
   result = ISSUE(handle->server,request);
   if (gds___fail(!result, result, err)) return -1;
   GETDATA(handle->server,result)
   return result;
}

/* ========================================================================== */
/*                                 GDS_PRIME                                  */
/* -------------------------------------------------------------------------- */
/*
#>            gds_prime.dc2
Function:     GDS_PRIME

Purpose:      Returns the size of the set's hash table.

Category:     GDS

File:         gdsd_basic.c

Author:       J.P. Terlouw

Use:          INTEGER GDS_PRIME( SET ,    Input    CHARACTER*(*)
                                 ERROR )  Output   INTEGER

              GDS_PRIME     Returns the size of the set's hash table.
              SET           Name or handle of set.
              ERROR         Error return code.
              
Description:  The obtained size of the hash table can be used to determine
              the size of the hash table of a set to be created.
              The size of a set's hash table influences the efficiency with
              which descriptor items can be accessed. It should be of the
              same order of magnitude as the number of descriptor items
              (to be) stored.

See also:     gds_nitems.dc2, gdst_prime.dc2.

Updates:      Apr 27, 1994: JPT, document created.
#<

Fortran to C interface:
@ integer function gds_prime( character, integer )
------------------------------------------------------------------------------*/
extern fint	gds_prime_c( fchar set, fint *err )
{
   setinf    handle;
   _setPrime request;
   fint      result;
   
   handle = SetHandle(set, 0, err);
   if (!handle) return -1;
   request.head.code = SET_PRIME;
   request.head.id   = handle->id;
   request.head.imm  = 0;
   result = ISSUE(handle->server,request);
   if (gds___fail(!result, result, err)) return -1;
   GETDATA(handle->server,result)
   return result;
}

/* ========================================================================== */
/*                                 GDS_NITEMS                                 */
/* -------------------------------------------------------------------------- */
/*
#>            gds_nitems.dc2
Function:     GDS_NITEMS

Purpose:      Returns the number of descriptor items present in a set.

Category:     GDS

File:         gdsd_basic.c

Author:       J.P. Terlouw

Use:          INTEGER GDS_NITEMS( SET ,    Input    CHARACTER*(*)
                                  ERROR )  Output   INTEGER

              GDS_NITEMS    Returns the number of descriptor items.
              SET           Name or handle of set.
              ERROR         Error return code.
              
Description:  The obtained number of descriptor items can be used to determine
              the size of the hash table of a set to be created.
              The size of a set's hash table influences the efficiency with
              which descriptor items can be accessed. It should be of the
              same order of magnitude as the number of descriptor items
              (to be) stored.

See also:     gds_prime.dc2, gdst_prime.dc2.

Updates:      Apr 27, 1994: JPT, document created.
#<

Fortran to C interface:
@ integer function gds_nitems( character, integer )
------------------------------------------------------------------------------*/
extern fint	gds_nitems_c( fchar set, fint *err )
{
   setinf    handle;
   _setNitems request;
   fint      result;
   
   handle = SetHandle(set, 0, err);
   if (!handle) return -1;
   request.head.code = SET_NITEMS;
   request.head.id   = handle->id;
   request.head.imm  = 0;
   result = ISSUE(handle->server,request);
   if (gds___fail(!result, result, err)) return -1;
   GETDATA(handle->server,result)
   return result;
}

/* ========================================================================== */
/*                                 GDS_RENAME                                 */
/* -------------------------------------------------------------------------- */
/*
#>gds_rename.dc2
Function:     GDS_RENAME

Purpose:      Rename an existing GDS set

Category:     GDS

File:         gdsd_basic.c

Author:       J.P. Terlouw


Use:          GDS_RENAME(                 Return    INTEGER
                          OLDNAME ,       Input     CHARACTER*(*)
                          NEWNAME )       Input     CHARACTER*(*)

              GDS_RENAME  Returns a GDS result code: 0 if OK, <0 if error.
              OLDNAME     Old set name.
              NEWNAME     New set name.

Description:  Both the .image and the .descr files are renamed to the new
              name. The set handle is still valid, so tasks which opened the
              set under the old name can still access it.
              This function obsoletes the function of the same name and
              functionality written by Do Kester. The most important difference
              is the result code which is returned.
              
Limitation:   This routine cannot rename between different directories.

Updates:      Apr  7, 1994: JPT, Document created.
              Mar 28, 1995: JPT, Limitation added.
#<

Fortran to C interface:
@ integer function gds_rename( character, character )
------------------------------------------------------------------------------*/
extern fint gds_rename_c( fchar old, fchar new )
{
   gdsserver  server;
   _setRename request;
   char       old_i[NAMLEN], new_i[NAMLEN], old_a[NAMLEN], new_a[NAMLEN];
   int        i, dflag=0;
   setinf     set;
   int        result;
    
   CTS(old,old_i);
   CTS(new,new_i);
   absname(old_i,old_a);
   absname(new_i,new_a);
   strcpy(new_i,request.newname);
   
                                          /* check for same directory */
   for (i=0; old_a[i] && new_a[i]; i++) {
      if (old_a[i] != new_a[i])  dflag++;
      if (dflag && (old_a[i]=='/' || new_a[i]=='/')) return GDS_RENAMEFAIL;
   }
                                          /* close any open set with new name */
   for (set=setlist; set; set=set->next) {
      if (!strcmp(new_a,set->name)) {
         SetClose(set);
         break;
      }
   }
   server = gds___server(old_a);
   if (!server) return gds___srverr();
   request.head.code = SET_RENAME;
   request.head.id   = 0;
   request.head.imm  = 0;
   strcpy(request.oldname,gds___srvnam(server,old_a));
   strcpy(request.newname,gds___srvnam(server,new_a));
   result = ISSUE(server,request);
                                        /* establish new name in client table */
   if (result==GDS_SUCCESS) {
      for (set=setlist; set; set=set->next) {
         if (!strcmp(old_a,set->name)) {
            strcpy(set->name,new_a);
            break;
         }
      }
   }
   return result;
}
		

/* ========================================================================== */
/*                                 GDS_RECOVER                                */
/* -------------------------------------------------------------------------- */
/*
#>gds_recover.dc3
Function:      gds_recover

Purpose:       Recover corrupt descriptor file

Category:      BASIC-GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           GDS_RECOVER(                     Return  INTEGER
                            SET )               Input   CHARACTER*(*)
                            
               GDS_RECOVER    Error return code
               SET            Name of the set
               
Description:   This function attempts to recover a corrupt descriptor file.
               If recovery is possible, zero (0) is returned. Otherwise
               a negative error code will report that recovery is not possible.
               
Warning:       This function may result in loss of data in the descriptor file.
               It should only be used as a last resort when it is impossible
               to retrieve a correct version of the descriptor file, e.g. from
               a backup tape.
               
Updates:       May  8, 1995: JPT, Document created.
#<

Fortran to C interface:   
@ integer function gds_recover( character )
*/

extern fint gds_recover_c(fchar name)
{
   gdsserver  server;
   _setRecover request;
   char        name_i[NAMLEN], name_a[NAMLEN];
    
   CTS(name,name_i);
   absname(name_i, name_a);
   server = gds___server(name_a);
   if (!server) return gds___srverr();
   request.head.code = SET_RECOVER;
   strcpy(request.name,gds___srvnam(server,name_a));
   return ISSUE(server,request);
}

/* ========================================================================== */
/*                                 gds_rhed                                   */
/* -------------------------------------------------------------------------- */
/*
#>gds_rhed.dc3
Function:      gds_rhed

Purpose:       Obtain coordinate-related header information.

Category:      BASIC-GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           fint gds_rhed ( fchar set, gds_coord **cinfo);

               set      = Name or handle of set.
               cinfo    = structure receiving coordinate information from set.
               gds_rhed = GDS result return code.
               
Description:   This function is used by the other GDS routines, such as the
               coordinate routines. For efficiency the coordinate information
               is cached. If it is necessary to have the latest version of
               the information, gds_frhed can be called to clear the cache.

Related documents:
               gds_whed.dc3, gds_frhed.dc3, gdsparams.dc3
               
Updates:       Mar 14, 1994: JPT, Document created.
#<
------------------------------------------------------------------------------*/
extern fint gds_rhed( fchar set, gds_coord **cinfo)
{
   setinf   handle;
   _setRhed request;
   fint result=0;
    
   handle = SetHandle(set, 0, &result);
   if (!handle) return result;
   if (handle->coords) {
      *cinfo = handle->coords;
      return GDS_SUCCESS;
   }
   handle->coords = New(gds_coord);
   request.head.code = SET_RHED;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   result = ISSUE(handle->server,request);
   if (result) return result;
   (void)gds___srvrcv(handle->server,handle->coords,sizeof(gds_coord));
   strcpy(handle->coords->name, handle->name); /* use client version of name */
   *cinfo = handle->coords;
   return GDS_SUCCESS;
}

/* ========================================================================== */
/*                                 gds_whed                                   */
/* -------------------------------------------------------------------------- */
/*
#>gds_whed.dc3
Function:      gds_whed

Purpose:       Change coordinate-related header information.

Category:      BASIC-GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           fint gds_whed ( fchar set, gds_coord *cinfo);

               set      = Name or handle of set.
               cinfo    = structure containing coordinate information for set.
               gds_whed = GDS result return code.
               
Related documents: 
               gds_rhed.dc3, gdsparams.dc3
               
Updates:       Mar 14, 1994: JPT, Document created.
#<
------------------------------------------------------------------------------*/
extern fint gds_whed( fchar set, gds_coord *cinfo)
{
   setinf   handle;
   _setWhed request;
   fint     result=0;
    
   handle = SetHandle(set, 0, &result);
   if (!handle) return result;
   request.head.code = SET_WHED;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   result = ISSUE(handle->server,request);
   if (result) return result;
   (void)gds___srvsnd(handle->server,cinfo,sizeof(gds_coord));
   if (!handle->coords) handle->coords = New(gds_coord);
   *handle->coords = *cinfo;
   return GDS_SUCCESS;
}

/* ========================================================================== */
/*                                 gds_frhed                                  */
/* -------------------------------------------------------------------------- */
/*
#>gds_frhed.dc3
Function:      gds_frhed

Purpose:       Clear coordinate-related header information cache.
   
Category:      BASIC-GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw
  
Use:           fint gds_frhed ( fchar set );

               set        = Name or handle of set.
               gds_frhed  = GDS result return code
               
Description:   This function is to be called whenever a subsequent call to
               gds_rhed() should deliver the most recent information from
               the set.

Related documents:
               gds_rhed.dc3, gds_whed.dc3, gdsparams.dc3

Updates:       Mar 31, 1994: JPT, Document created.
#<
------------------------------------------------------------------------------*/

extern fint gds_frhed( fchar set)
{
   setinf   handle;
   fint     result=0;
    
   handle = SetHandle(set, 0, &result);
   if (!handle) return result;
   if (handle->coords) Delete(handle->coords);
   return GDS_SUCCESS;
}

/* ========================================================================== */
/*                                 GDS_DELETE                                 */
/* -------------------------------------------------------------------------- */
/*
@subroutine gds_delete( character, integer )
#>gds_delete.dc2
Subroutine:    GDS_DELETE

Purpose:       Delete set.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDS_DELETE ( SETNAME,        Input   CHARACTER*(*)
                                 ERROR )         Output  INTEGER
                
               SETNAME     Name of set.
               ERROR       Error return code.

               Both the descriptor and the image part are deleted.
               Any attempts to access the set by tasks which had
               it open when it was deleted, will fail with the error code
               -57 (GDS_BADHANDLE).

Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
               3-Mar-94   rewritten for GDS server
              26-Jun-95   close set before deleting it
#<
------------------------------------------------------------------------------*/
extern void gds_delete_c (fchar set, fint *err)
{
   setinf     handle;
   gdsserver  server;
   _setDelete request;
   char       set_i[NAMLEN], set_a[NAMLEN];
   fint       result;
    
   handle = SetHandle(set, 1, err);
   if (handle) (void)SetClose(handle);
   CTS(set,set_i);
   absname(set_i,set_a);
   server = gds___server(set_a);
   strcpy(request.name,gds___srvnam(server,set_a));
   if (gds___fail((int)server,gds___srverr(),err)) return;
   request.head.code = SET_DELETE;
   request.head.id   = 0;
   request.head.imm  = 0;
   result = ISSUE(server,request);
   if (gds___fail(!result, result, err)) return;
}

/* ========================================================================== */
/*                                 GDS_EXIST                                  */
/* -------------------------------------------------------------------------- */
/*
@logical function gds_exist( character , integer)
#>gds_exist.dc2
Function:      GDS_EXIST

Purpose:       Test whether set exists.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           GDS_EXIST (                 Return value, LOGICAL
                           SETNAME,        Input, CHARACTER
                           ERROR)          Output, INTEGER

                  SETNAME   Name of set.
                  ERROR     Error return code.

Description:   .TRUE. is returned if the set exists; .FALSE. otherwise.
               This routine can return .FALSE. though the descriptor file
               does exist. This can happen when the descriptor file is 
               corrupt or has an incompatible version number. Such a condition
               is reported through the ERROR argument.

Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
              05-Dec-91   check on file contents added
              09-Mar-94   rewritten for GDS server
#<
-----------------------------------------------------------------------------*/
bool gds_exist_c ( fchar set, fint *err )
{
   gdsserver server;
   _setExist request;
   bool      exist;
   char      set_i[NAMLEN], set_a[NAMLEN];
   fint      result;
   
   CTS(set,set_i);
   absname(set_i,set_a);
   server = gds___server(set_a);
   strcpy(request.name,gds___srvnam(server,set_a));
   if (gds___fail((int)server,gds___srverr(),err)) return FALSE;
   request.head.code = SET_EXIST;
   request.head.id   = 0;
   request.head.imm  = 0;
   result = ISSUE(server,request);
   exist = (result==GDS_SUCCESS);
   if (result==GDS_OPENFAIL) result = GDS_SUCCESS;
   if (gds___fail(!result, result, err)) return exist;
   return exist;
}

/* ========================================================================== */
/*                                 GDS_OPTIMIZE                               */
/* -------------------------------------------------------------------------- */
/*
@subroutine gds_optimize( character, integer)
#>gds_optimize.dc2
Subroutine:    GDS_OPTIMIZE
 
Purpose:       Modify lay-out of GDS descriptor file for efficient access.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDS_OPTIMIZE ( SET ,      Input    CHARACTER*(*)
                                   ERROR )    Output   INTEGER
                                   
               SET       Name or handle of set.
               ERROR     Error return code.

Description:   This routine rearranges all descriptor items so that they
               become contiguous and adjusts the hash table size.
               It also eliminates empty space.

Limitation:    Cannot be called when the calling task has a lock on the set.

Updates:       Apr 12, 1994: JPT Document created.
#<
*/

extern void gds_optimize_c( fchar set, fint *err)
{
   setinf       handle;
   _setOptimize request;
   fint         result;
    
   handle = SetHandle(set, 0, err);
   if (!handle) return;
   request.head.code = SET_OPTIMIZE;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   result = ISSUE(handle->server,request);
   if (gds___fail(!result, result, err)) return;
}
   
/* ========================================================================== */
/*                                 GDS_SYNC                                   */
/* -------------------------------------------------------------------------- */
/*
@subroutine gds_sync( character, integer )
#>gds_sync.dc2
Subroutine:    GDS_SYNC

Purpose:       Update descriptor file.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDS_SYNC ( SET ,          Input    CHARACTER*(*)
                               ERROR )        Output   INTEGER
                               
               SET       Name or handle of set.
               ERROR     Error return code.
                
Description:   This routine causes the GDS server to update the descriptor
               file from its internal buffer. Normally it is not necessary to
               call GDS_SYNC, because the server itself also writes out
               changes periodically.

Updates:       Apr 21, 1994: JPT document created.
#<
-----------------------------------------------------------------------------*/
extern void gds_sync_c (fchar set, fint *err)
{
   setinf   handle;
   _setSync request;
   fint     result;

   handle = SetHandle(set, 0, err);
   if (!handle) return;
   request.head.code = SET_SYNC;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   result = ISSUE(handle->server,request);
   (void)gds___fail(!result, result, err);
}

/* ========================================================================== */
/*                                 GDSD_READ                                  */
/* -------------------------------------------------------------------------- */
/*
@subroutine gdsd_read( character, character, integer, integer,
@                       integer, integer, integer, integer)
#>gdsd_read.dc2
Subroutine:    GDSD_READ

Purpose:       Read GDS descriptor item.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDSD_READ( SET ,          Input    CHARACTER*(*)
                               KEY ,          Input    CHARACTER*20
                               LEVEL ,        Input    INTEGER
                               BUFFER ,       Output   any type except CHARACTER
                               NBYTES ,       Input    INTEGER
                               POSITION ,     Input    INTEGER
                               DONE ,         Output,  INTEGER
                               ERROR )        Output,  INTEGER
               
               SET      =  Name or handle of set.
               KEY      =  Name of descriptor item.
               LEVEL    =  Coordinate word specifying the substructure from
                           which the descriptor item is to be obtained. 
                           If the item is not present at this level, higher 
                           levels are inspected until the item is found or
                           proven to be not present.
               BUFFER   =  Variable or array of receiving the data to be read.
               NBYTES   =  Number of bytes to be read. Optional argument. If it
                           is omitted, the current length of the descriptor
                           item is used.
               POSITION =  Relative position, starting at 1, from which will
                           be read. If zero (0) is specified, reading is done
                           from the current reading position. Optional argument.
                           If it is omitted, the default 1 is assumed.
               DONE     =  The number of bytes which have actually been read.
                           Optional argument. If it is omitted and the number 
                           of bytes which have been transferred is not equal
                           to the requested length of the transfer, an error
                           condition exists.
               ERROR    =  Error return code. In case of success, it will 
                           contain the level where the item has been found.
                           Optional argument.

Note:         This routine does not allow buffers of the type CHARACTER.
              Buffers of this type can be handled by the special routine
              GDSD_READC.

Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
               1-Mar-94   rewritten for GDS server
#<
------------------------------------------------------------------------------*/
extern void gdsd_read_c(fchar set, fchar key, fint *level, char buf[],
              fint *nb, fint *pos, fint *done, fint *err)
{
   fint     level_i;
   int      done_i;
   fint     nb_i,pos_i;
   setinf   handle;
   _dscRead request;
   fint     result;

   handle = SetHandle(set, 0, err);
   if (!handle) return;
   if (tobool(presentn_c(level))) level_i = *level; else level_i =0;
   if (tobool(presentn_c(pos))) pos_i = MAX(0,*pos); else pos_i =1;
   nb_i = tobool(presentn_c(nb))? MAX(1,*nb) : 0;

   request.head.code = DSC_READ;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   request.level     = level_i;
   request.nelems    = nb_i;
   request.position  = pos_i;
   request.abslev    = gds___abslevel();
   request.type      = 0;
   CTS(key,request.name);
   result = gds___srvreq(handle->server, &request, sizeof(request), &done_i);


   if (gds___fail(!result, result, err)) return;
   done_i -= sizeof(level_i);
   GETDATA(handle->server,level_i);
   (void)gds___srvrcv(handle->server, buf, done_i);
   if (tobool(presentn_c(done))) {
      *done = done_i;
   } else {
      if (gds___fail(done_i==nb_i || nb_i==0,GDS_INCOMPLETE,err)) return;
   }
   if (tobool(presentn_c(err))) *err = level_i;
}

/* ========================================================================== */
/*                                 GDSD_WRITE                                 */
/* -------------------------------------------------------------------------- */
/*
@subroutine gdsd_write( character, character, integer, integer,
@                        integer, integer, integer, integer )
#>gdsd_write.dc2
Subroutine:    GDSD_WRITE

Purpose:       Write descriptor item.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDSD_WRITE( SET ,         Input    CHARACTER*(*)
                                KEY ,         Input    CHARACTER*20
                                LEVEL ,       Input    INTEGER
                                BUFFER ,      Output   any type except CHARACTER
                                NBYTES ,      Input    INTEGER
                                POSITION ,    Input    INTEGER
                                DONE ,        Output,  INTEGER
                                ERROR )       Output,  INTEGER
               
               SET      =  Name or handle of set.
               KEY      =  Name of descriptor item.
               LEVEL    =  Coordinate word specifying the substructure to
                           which the descriptor belongs.
               BUFFER   =  Variable or array of containing the data to be
                           written.
               NBYTES   =  Number of bytes to be written. Optional argument.
                           If it is omitted, the current length of the
                           descriptor item is used.
               POSITION =  Relative position, starting at 1, from which will
                           be read. If zero (0) is specified, writing is done
                           at the end of the descriptor item. Optional argument.
                           If it is omitted, the default 1 is assumed.
               DONE     =  The number of bytes which have actually been written.
                           Optional argument. If it is omitted and the number 
                           of bytes which have been transferred is not equal
                           to the requested length of the transfer, an error
                           condition exists.
               ERROR    =  Error return code. In case of success, it will 
                           contain the level where the item has been found.
                           Optional argument.

Note:         This routine does not allow buffers of the type CHARACTER.
              Buffers of this type can be handled by the special routine
              GDSD_WRITEC.

Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
               3-Mar-94   rewritten for GDS server
#<
------------------------------------------------------------------------------*/
extern void gdsd_write_c(fchar set, fchar key, fint *level, char buf[],
                         fint *nb, fint *pos, fint *done, fint *err)
{
   fint      level_i;
   fint      nb_i, pos_i;
   int       done_i;
   setinf    handle;
   _dscWrite request;
   fint      result;


   handle = SetHandle(set, 0, err);
   if (!handle) return;
   if (tobool(presentn_c(level))) level_i = *level; else level_i = 0;
   if (tobool(presentn_c(pos)))   pos_i = MAX(0,*pos); else pos_i = 1;
   if (tobool(presentn_c(nb)))    nb_i = MAX(1,*nb); else nb_i = 0;

   request.head.code = DSC_WRITE;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   request.level     = level_i;
   request.nelems    = nb_i;
   request.position  = pos_i;
   request.type      = 0;
   CTS(key,request.name);
   result = gds___srvreq(handle->server, &request, sizeof(request), &done_i);

   if (gds___fail(!result, result, err)) return;
   gds___srvsnd(handle->server, buf, done_i);

   if (tobool(presentn_c(done))) {
      *done = done_i;
   } else {
      if (gds___fail(done_i==nb_i || nb_i==0,GDS_INCOMPLETE,err)) return;
   }
}

/* ========================================================================== */
/*                                 GDSD_DELETE                                */
/* -------------------------------------------------------------------------- */
/*
@subroutine gdsd_delete( character, character, integer, integer )
#>gdsd_delete.dc2
Subroutine:    GDSD_DELETE

Purpose:       Delete descriptor item.
             
Category:      GDS
                                          
File:          gdsd_basic.c
                                         
Author:        J.P. Terlouw

Use:           CALL GDSD_DELETE ( SET,            Input   CHARACTER*(*)
                                  NAME,           Input   CHARACTER*20
                                  LEVEL,          Input   INTEGER
                                  ERROR )         Output  INTEGER
                                  
               SET         Name or handle of set.
               NAME        Name of descriptor item.
               LEVEL       Coordinate word specifying the substructure level
                           where the descriptor item is to be found.
               ERROR       Error return code.
               

Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
              11-Mar-94   rewritten for GDS server
#<
-----------------------------------------------------------------------------*/
void gdsd_delete_c (fchar set, fchar key, fint *level, fint *err)
{
   setinf     handle;
   _dscDelete request;
   fint       result;

   handle = SetHandle(set, 0, err);
   if (!handle) return;
   request.head.code = DSC_DELETE;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   request.level     = *level;
   CTS(key,request.name);
   result = ISSUE(handle->server,request);
   if (gds___fail(!result, result, err)) return;
}

/* ========================================================================== */
/*                                 GDSD_DELALL                                */
/* -------------------------------------------------------------------------- */
/*
@subroutine gdsd_delall( character, character, integer)
#>gdsd_delall.dc2
Subroutine:    GDSD_DELALL

Purpose:       Delete descriptor item at all levels where it exists.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDSD_DELALL ( SET,            Input   CHARACTER*(*)
                                  NAME,           Input   CHARACTER*20
                                  ERROR )         Output  INTEGER

               SET         Name or handle of set.
               NAME        Name of descriptor item.
               ERROR       Error return code.

Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
              11-Mar-94   rewritten for GDS server
#<
---------------------------------------------------------------------------- */
extern void gdsd_delall_c(fchar set, fchar key, fint *err)
{
   setinf     handle;
   _dscDelall request;
   fint       result;

   handle = SetHandle(set, 0, err);
   if (!handle) return;
   request.head.code = DSC_DELALL;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   CTS(key,request.name);
   result = ISSUE(handle->server,request);
   if (gds___fail(!result, result, err)) return;
}

/* ========================================================================== */
/*                                 GDSD_FIND                                  */
/* -------------------------------------------------------------------------- */
/*
@character function gdsd_find( character, integer, integer, integer)
#>gdsd_find.dc2
Function:      GDSD_FIND

Purpose:       Find next descriptor name.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           GDSD_FIND (                                 CHARACTER*(*)
                           SET,            Input           CHARACTER*(*)
                           LEVEL,          Input           INTEGER
                           INDEX,          Input, Output   INTEGER
                           ERROR )         Output          INTEGER
                
               SET         Name or handle of set.
               LEVEL       If specified, the routine only finds items of this
                           and of higher levels; if not specified, all levels
                           are found.
               INDEX       internal index where the search starts.
                           In the first call of a sequence zero should be
                           specified. Upon return, it will contain the next
                           index. If an item cannot be found, zero is returned.
               ERROR       Error return code. If call has completed
                           successfully, and an item was found, the level where
                           the item was found is returned.

              By using this routine, one is capable of generating all
              descriptor item names. This is useful when transferring data
              to a different set or to tape.

Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
              11-Mar-94   rewritten for GDS server
#<
---------------------------------------------------------------------------- */
void gdsd_find_c(fchar funval, fchar set, fint *level,
                 fint *index, fint *err)
{
   setinf    handle;
   _dscFind  request;
   char      key[NAMLEN];
   fint      level_i;
   fint      result;
    
   handle = SetHandle(set, 0, err);
   if (!handle) return;
   request.head.code = DSC_FIND;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   request.level     = tobool(presentn_c(level))?*level:0x7FFFFFFF;
   request.index     = *index;
   result = ISSUE(handle->server,request);
   *index = -1;
   if (result==GDS_NOTFOUND) {
      *index = 0;
      result = GDS_SUCCESS;
   }
   if (gds___fail(!result, result, err)) return;
   if (*index) {
      GETDATA(handle->server,level_i);
      if (tobool(presentn_c(err))) *err = level_i;
      (void)gds___srvrcv(handle->server,index,sizeof(fint));
      (void)gds___srvrcv(handle->server,key,NAMLEN);
      STC(key,funval);
   }
}
/* ========================================================================== */
/*                                 GDSD_LENGTH                                */
/* -------------------------------------------------------------------------- */
/*
@integer function gdsd_length( character, character, integer, integer)
#>gdsd_length.dc2
Function:      GDSD_LENGTH

Purpose:       Obtain length of descriptor item

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           GDSD_LENGTH(                         INTEGER
                            SET,            Input   CHARACTER*(*)
                            NAME,           Input   CHARACTER*20
                            LEVEL,          Input   INTEGER
                            ERROR )         Output  INTEGER
                
               SET         Name or handle of set.
               NAME        Name of descriptor item.
               LEVEL       Coordinate word specifying the substructure level
                           where the descriptor item is to be found.
               ERROR       Error return code.
               
Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
              11-Mar-94   rewritten for GDS server
#<
-----------------------------------------------------------------------------*/
fint gdsd_length_c (fchar set, fchar key, fint *level, fint *err)
{
   setinf     handle;
   _dscLength request;
   fint       result;
    
   handle = SetHandle(set, 0, err);
   if (!handle) return -1;
   request.head.code = DSC_LENGTH;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   request.level     = *level;
   CTS(key,request.name);
   result = ISSUE(handle->server,request);
   if (gds___fail(!result, result, err)) return -1;
   GETDATA(handle->server,result)
   return result;
}

/* ========================================================================== */
/*                                 GDSD_REWIND                                */
/* -------------------------------------------------------------------------- */
/*
@subroutine gdsd_rewind( character, character, integer, integer )
#>gdsd_rewind.dc2
Subroutine:    GDSD_REWIND

Purpose:       Set current read position at beginning of descriptor item.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDSD_REWIND ( SET,            Input   CHARACTER*(*)
                                  NAME,           Input   CHARACTER*20
                                  LEVEL,          Input   INTEGER
                                  ERROR )         Output  INTEGER
                                  
               SET         Name or handle of set.
               NAME        Name of descriptor item.
               LEVEL       Coordinate word specifying the substructure level
                           where the descriptor item is to be found.
               ERROR       Error return code.

Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
              11-Mar-94   rewritten for GDS server
#<
-----------------------------------------------------------------------------*/
void gdsd_rewind_c(fchar set, fchar key, fint *level, fint *err)
{
   setinf     handle;
   _dscRewind request;
   fint       result;
    
   handle = SetHandle(set, 0, err);
   if (!handle) return;
   request.head.code = DSC_REWIND;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   request.level     = *level;
   CTS(key,request.name);
   result = ISSUE(handle->server,request);
   if (gds___fail(!result, result, err)) return;
}

/* ========================================================================== */
/*                                 GDS_LOCK                                   */
/* -------------------------------------------------------------------------- */
/*
@subroutine gds_lock( character, integer )
#>gds_lock.dc2
Subroutine:    GDS_LOCK

Purpose:       Obtain exclusive access to a set.

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDS_LOCK( SET,           Input     CHARACTER*(*)
                              ERROR )        Output    INTEGER
                              
               SET         Name or handle of set.
               ERROR       Error return code.

Description:   GDS_LOCK gives a task exclusive access to a set.
               The task should have write permission on the set.
               This routine should be used when a task makes a number of
               related modifications to a set which should all be completed 
               before an other task may access that set.
               When the task is finished with these modifications, it should
               release the lock by calling GDS_UNLOCK.
                              
               It is possible to 'nest' calls to GDS_LOCK and GDS_UNLOCK.
               In this case the set remains locked until all calls to GDS_LOCK
               have been cancelled by corresponding calls to GDS_UNLOCK.

               In order to prevent deadlocks, the GDS server allows only one
               client at a time to have locks. If there is already such a
               client, any requests for a lock will be delayed until all
               locks are released.

               If a task does not want to wait for any lock, it can call
               GDST_IMMEDIATE to allow the locked status to be reported.

Related documents: gds_unlock.dc2, gdst_immediate.dc2.

Updates:       Mar  3, 1994: JPT Document created.
#<
------------------------------------------------------------------------------*/
extern void gds_lock_c( fchar set, fint *err)
{
   setinf   handle;
   _setLock request;
   fint     result;
    
   handle = SetHandle(set, 0, err);
   if (!handle) return;
   if (handle->lockdepth++) return;                        /* already locked? */
   request.head.code = SET_LOCK;
   request.head.id   = handle->id;
   request.head.imm  = gds___immediate();
   result = ISSUE(handle->server,request);
   if (gds___fail(!result, result, err)) handle->lockdepth--;
}

/* ========================================================================== */
/*                                 GDS_UNLOCK                                 */
/* -------------------------------------------------------------------------- */
/*
@subroutine gds_unlock( character, integer )
#>gds_unlock.dc2
Subroutine:    GDS_UNLOCK

Purpose:       Abandon exclusive access to a set.

Category:      GDS
  
File:          gdsd_basic.c
   
Author:        J.P. Terlouw
    
Use:           CALL GDS_UNLOCK( SET,         Input     CHARACTER*(*)
                                ERROR )      Output    INTEGER

               SET         Name or handle of set.
               ERROR       Error return code.

Updates:       Mar  3, 1994: JPT Document created.
#<
------------------------------------------------------------------------------*/
extern void gds_unlock_c( fchar set, fint *err)
{
   setinf     handle;
   _setUnlock request;
   fint       result;
    
   handle = SetHandle(set, 0, err);
   if (!handle) return;
   if (gds___fail(handle->lockdepth>0, GDS_NOTLOCKED, err)) return;
   if (--handle->lockdepth) return;                          /* still locked? */
   request.head.code = SET_UNLOCK;
   request.head.id   = handle->id;
   request.head.imm  = 0;
   result = ISSUE(handle->server,request);
   (void)gds___fail(!result, result, err);
}
               
/* ========================================================================== */
/*                                 gds___image                                */
/* -------------------------------------------------------------------------- */
/*
#>gds___image.dc3
Function:   gds___image

Purpose:    obtain file descriptor of the image component of a set.

Author:     J.P. Terlouw

Category:   BASIC-GDS

File:       gdsd_basic.c

Use:        int gds___image( fchar set, void (*proc)() )

            The function returns the file descriptor of the image file.
            If necessary, the set and/or the image file is opened.
            The function specified in the second argument will be called
            when the set is closed. The prototype of this function is:
              void proc(int fd);
            The argument 'fd' is the image file descriptor.

Updates:    Mar  4, 1994: JPT, document created.
#<
------------------------------------------------------------------------------*/
extern int gds___image( fchar name, void(*proc)() )
{
   setinf   handle;
   char     name_i[NAMLEN+6];
   int      fd;
   fint     result;

   handle = SetHandle(name, 0, &result);
   if (!handle) return -1;
   if (!handle->fi) {
      gds_coord *setinfo;
      CTS(name,name_i);
      (void)gds_frhed(name);
      (void)gds_rhed(name,&setinfo);
      strcpy(name_i,setinfo->name);
      strcat(name_i,".image");
      fd = open( name_i, O_CREAT | O_RDWR, 420 );
      if (fd == -1) fd = open( name_i, O_RDONLY, 420 );
      if (fd<0) return -1;
      handle->fi = fd;
      if (proc) handle->iclose = proc;
   }
   return handle->fi;
}

/* ========================================================================== */
/*                                 gds___fail                                 */
/* -------------------------------------------------------------------------- */
/*
#>gds___fail.dc3
Function:   gds___fail

Purpose:    verify condition and handle error when condition not met.

Author:     J.P. Terlouw
 
Category:   BASIC-GDS

File:       gdsd_basic.c

Use:        int gds___fail(int verify, fint erri, fint *erro)
            
            It is similar to GDS___CHECK.
            
            If "error" is present and is negative due to a previous
            error condition, 1000 is subtracted from it to indicate
            failure in a previous call and gds___error_c is called
            with this value. This causes the program to abort with
            an error message.
            
            If "verify" is true, gds___fail returns false.
            
            If "verify" is false, then the presence of "erro" is checked.
            If it is present, then "erri" is copied to "erro" and true is
            returned.
            If it is not present, then gds___error_c is called with "erri"
            as argument.

Updates:   21-Dec-89   --  original document
#<
------------------------------------------------------------------------------*/
extern int gds___fail(int verify, fint erri, fint *erro)
{
  if (tobool(presentn_c(erro)))
    if (*erro<0) {
      *erro -= 1000;
      gds___error_c(erro);
    }
  if (!verify)
    if(tobool(presentn_c(erro))) {
      *erro = erri;
      return (1);
    } else {
      gds___error_c(&erri);
    }
  return (0);
}

/* ========================================================================== */
/*                                 gds___char2str                             */
/* -------------------------------------------------------------------------- */
/*
#>gds___char2str.dc3
Function: gds___char2str

Purpose:  copy a fchar value to a char[]

Author:   J.P. Terlouw
 
Category: BASIC-GDS

File:     gdsd_basic.c

Use:      int gds___char2str(fchar c, char *s, int ls)
                       c    input fchar object
                       s    output string
                       ls   length of s
                       
             The tranfer stops at the end of c or at the first blank 
             encountered.
             If the length of c exceeds the length of s, only the first ls-1
             elements are transferred.
             The output string is closed with a zero byte.
             The function value is the number of characters transferred.
             
Updates:     21-Dec-89   --   original document
#<
------------------------------------------------------------------------------*/
extern int gds___char2str(fchar c, char *s, int ls)
{  
   int nc;

   for(nc = 1 ;  nc<=c.l && nc<ls ;nc++)  *(s++)=*(c.a++);
   *s = 0;
   while (*(--s)==' ') {
      *s = 0; nc--;
   }
   return nc;
}

/* ========================================================================== */
/*                                 gds___str2char                             */
/* -------------------------------------------------------------------------- */
/*
#>gds___str2char.dc3
Function:     gds___str2char

Purpose:      copy a zero-terminated character string to a fchar.

Category:     GDS

File:         gdsd_basic.c

Author:       J.P. Terlouw

Use:          int gds___str2char(char *s, fchar c)

                           s    input character string
                           c    output fchar
                           
              If c is longer than s, than the result is padded with blanks;
              if it is shorter, than the result is truncated.
              The funcion value returned is the actual number of characters
              transferred form s to c.
              
Updates:      21-Dec-89   --   original document
#<
----------------------------------------------------------------------------- */
extern int gds___str2char(char *s, fchar c)
{  
   int nc;

   for(nc = 0 ; *s != 0 && nc<c.l ; nc++) *(c.a++)=*(s++);  /* Copy         */
   while(c.l>nc){
     *(c.a++) = ' ';                                        /* Blank extend */
     c.l--;
   }
   return nc;
}


/* ========================================================================== */
/*                                 GDSD_WRITEC                                */
/* -------------------------------------------------------------------------- */
/*
@subroutine gdsd_writec( character, character, integer, character,
@                        integer, integer, integer, integer )
#>gdsd_writec.dc2
Subroutine:    GDSD_WRITEC

Purpose:       Write CHARACTER-typed descriptor item

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDSD_WRITEC( SET ,         Input    CHARACTER*(*)
                                 KEY ,         Input    CHARACTER*20
                                 LEVEL ,       Input    INTEGER
                                 BUFFER ,      Output   CHARACTER*(*)
                                 NBYTES ,      Input    INTEGER
                                 POSITION ,    Input    INTEGER
                                 DONE ,        Output,  INTEGER
                                 ERROR )       Output,  INTEGER
               
               SET      =  Name or handle of set.
               KEY      =  Name of descriptor item.
               LEVEL    =  Coordinate word specifying the substructure to
                           which the descriptor belongs.
               BUFFER   =  Variable or array of containing the data to be
                           written.
               NBYTES   =  Number of bytes to be written.
               POSITION =  Relative position, starting at 1, to which will
                           be written. If zero (0) is specified, writing is done
                           at the end of the descriptor item. Optional argument.
                           If it is omitted, the default 1 is assumed.
               DONE     =  The number of bytes which have actually been written.
                           Optional argument. If it is omitted and the number 
                           of bytes which have been transferred is not equal
                           to the requested length of the transfer, an error
                           condition exists.
               ERROR    =  Error return code. In case of success, it will 
                           contain the level where the item has been found.
                           Optional argument.

Updates:       14-Dec-89   --  original document
               24-Mar-94   --  modified for GDS server
#<
-----------------------------------------------------------------------------*/
void      gdsd_writec_c  (fchar set, fchar key, fint *level,
                          fchar buffer, fint *nbytes, fint *position,
                          fint *done, fint *error)
{
   fint newbytes;
   
   newbytes = MIN(MAX(1,*nbytes),buffer.l);
   gdsd_write_c( set, key, level, buffer.a, &newbytes, position, done, error);
}


/* ========================================================================== */
/*                                 GDSD_READC                                 */
/* -------------------------------------------------------------------------- */
/*
@subroutine gdsd_readc( character, character, integer, character,
@                        integer, integer, integer, integer )
#>gdsd_readc.dc2
Subroutine:    GDSD_READC

Purpose:       Read CHARACTER-typed descriptor item

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           CALL GDSD_READC( SET ,          Input    CHARACTER*(*)
                                KEY ,          Input    CHARACTER*20
                                LEVEL ,        Input    INTEGER
                                BUFFER ,       Output   CHARACTER*(*)
                                NBYTES ,       Input    INTEGER
                                POSITION ,     Input    INTEGER
                                DONE ,         Output,  INTEGER
                                ERROR )        Output,  INTEGER
               
               SET      =  Name or handle of set.
               KEY      =  Name of descriptor item.
               LEVEL    =  Coordinate word specifying the substructure from
                           which the descriptor item is to be obtained. 
                           If the item is not present at this level, higher 
                           levels are inspected until the item is found or
                           proven to be not present.
               BUFFER   =  Variable or array of receiving the data to be read.
               NBYTES   =  Number of bytes to be read.
               POSITION =  Relative position, starting at 1, from which will
                           be read. If zero (0) is specified, reading is done
                           from the current reading position. Optional argument.
                           If it is omitted, the default 1 is assumed.
               DONE     =  The number of bytes which have actually been read.
                           Optional argument. If it is omitted and the number 
                           of bytes which have been transferred is not equal
                           to the requested length of the transfer, an error
                           condition exists.
               ERROR    =  Error return code. In case of success, it will 
                           contain the level where the item has been found.
                           Optional argument.

Updates:       14-Dec-89   --  original document
               24-Mar-94   --  modified for GDS server
#<
------------------------------------------------------------------------------*/
void      gdsd_readc_c   (fchar set, fchar key, fint *level,
                          fchar buffer, fint *nbytes, fint *position,
                          fint *done, fint *error)
{
   fint newbytes;
   
   newbytes = MIN(MAX(1,*nbytes),buffer.l);
   gdsd_read_c( set, key, level, buffer.a, &newbytes, position, done, error);
}

/* ========================================================================== */
/*                                 GDSD_TYPE                                  */
/* -------------------------------------------------------------------------- */
/*
@character function gdsd_type( character, character,  integer, integer)
#>gdsd_type.dc2
Function:      GDSD_TYPE

Purpose:       obtain FITS item type

Category:      GDS

File:          gdsd_basic.c

Author:        J.P. Terlouw

Use:           GDSD_TYPE (                 Return  CHARACTER*(*)
                           SET   ,         Input   CHARACTER*(*)
                           NAME  ,         Input   CHARACTER*8
                           LEVEL ,         Input   INTEGER
                           ERROR )         Output  INTEGER

               SET      name or handle of set.
               NAME     name of FITS item.
               LEVEL    coordinate word specifying the substructure from 
                        which the FITS item is to be obtained. If it is not
                        present at this level, higher levels are inspected
                        until the item is found or proven to be not present.
               ERROR    error return code, or level found when successful.

Updates:      25-May-87   original document
              14-Nov-89   rewritten in C
              24-Mar-94   modified for GDS server
#<
---------------------------------------------------------------------------- */
void gdsd_type_c(fchar funval, fchar set, fchar key, 
                 fint *level, fint *error)
{
   char frec_i[11];
   fint error_i,nd,c10=10,c1=1;

   error_i = 0;
   key.l = 8;                 /* FITS keywords are limited to 8 characters */
   gdsd_read_c(set,key,level,frec_i,&c10,&c1,&nd,&error_i);
   if (gds___fail(error_i>=0||error_i==GDS_INCOMPLETE, error_i, error)) return;
   if (gds___fail(strncmp(frec_i,"FITS ",5)==0,GDS_TYPNOTFITS,error)) return;
   frec_i[10]=0;
   gds___str2char(&frec_i[5],funval);
}

/* -------------------------------------------------------------------------- */
/*                                 SetHandle                                  */
/* -------------------------------------------------------------------------- */
static setinf SetHandle(fchar name, int noopen, fint *err)
{
   setinf result;
   setinf set;
   char   cname[NAMLEN], iname[NAMLEN];
   fint   ierr=0;
    
                                       /* symbolic handle - only decode.     */
   if (*name.a=='#') {
      if (gds___fail((sscanf(name.a+1,"%p",&result)==1),GDS_BADHANDLE,err))
         return NULL;
      if (gds___fail(result==result->current,GDS_BADHANDLE,err)) return NULL;
      return result;
   }

   CTS(name,cname);   
   absname(cname,iname);               /* convert to 'standard' name         */
                                       /* normal name - find if already open */
   for (set=setlist; set; set=set->next)
      if (!strcmp(iname,set->name)) return set;
   if (noopen) return NULL;
                                       /* finally attempt to open set        */
   result = SetOpen(iname, gds___extendsize(), &ierr);
   (void)gds___fail((int)result,ierr,err);
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                  SetOpen                                   */
/* -------------------------------------------------------------------------- */
/*  SetOpen() attempts to open a set.
 */
static setinf SetOpen(char *name, int incr, fint *err)
{
   setinf    result=NULL;
   gdsserver server;
   fint      srvres;
    
   server = gds___server(name);
   if (!gds___fail((int)server,gds___srverr(),err)) {
      _setOpen request;
      request.head.code = SET_OPEN;
      request.head.id   = 0;
      request.head.imm  = 0;
      request.incr      = incr;          /* if zero, server will use default */
      strcpy(request.name,gds___srvnam(server,name));
      srvres = ISSUE(server,request);
      if (!gds___fail(!srvres,srvres,err)) {
         setinf setstat=New(_setinf);
         
         setstat->current = setstat;
         setstat->server = server;
         GETDATA(server,setstat->id)
         strcpy(setstat->name,name);
         setstat->next = setlist;
         setlist = setstat;
         result = setstat;
      }
   }
   return result;
}

/* -------------------------------------------------------------------------- */
/*                                 SetClose                                   */
/* -------------------------------------------------------------------------- */
/*  SetClose() closes a set, given its handle. Both descriptor and image
 *  are closed.
 *  If 'handle' is zero, all sets are closed and the connection to the
 *  server is also closed.
 *  It always returns GDS_SUCCESS.
 */
static fint SetClose(setinf handle)
{
   gdsserver server=NULL;
   setinf set, *setp, *next;
   int    fi;
   void (*iclose)();
    
   for (setp=&setlist; *setp; setp=next) {
      set  = *setp;
      next = &set->next;
      if (!handle || set==handle) {
         if (set->server) {
            server = set->server;
            if (handle) {
               _setClose request;
               request.head.code = SET_CLOSE;
               request.head.id   = handle->id;
               request.head.imm  = 0;
               (void)ISSUE(server,request);
            }
            fi = set->fi;
            iclose = set->iclose;
            if (fi>0) {
               if (iclose) (*iclose)(fi);
               close(fi);
            }
         }
         if (set->coords) Delete(set->coords);
         next = setp;
         *setp = set->next;
         free(set);
         if (handle) return GDS_SUCCESS;
      }
   }
   if (!handle) gds___srvcls();
   return GDS_SUCCESS;
}

/* -------------------------------------------------------------------------- */
/*                                 absname                                    */
/* -------------------------------------------------------------------------- */
/*  absname() translates a set name into an absolute name.
 */
static void absname(char *name, char *sname)
{
   static char cwd[NAMLEN]="";
   if (*name=='/') {
      strcpy(sname,name);
      return;
   }
   (void)getcwd(cwd,NAMLEN-strlen(name)-2);
   strcpy(sname,cwd);
   strcat(sname,"/");
   strcat(sname,name);
   /* ... this scheme can be improved. E.g. allow environment variables,
    * ... going back in the tree with "..", etc. We should also consider to
    * ... use one standard case for set names.
    */
}

/* -------------------------------------------------------------------------- */
/*                                 gdsd_basic.h                               */
/* -------------------------------------------------------------------------- */
/*  Prototype declarations of external functions.

#>gdsd_basic.h
#if !defined(_gdsd_basic_h_)
#define _gdsd_basic_h_
#include "gipsyc.h"
#include "gdsparams.h"
extern void gds_handle_c(fchar funval, fchar set, fint *err);
extern void gds_create_c (fchar set, fint *err);
extern void gds_close_c(fchar set, fint *err);
extern void gds_closeall_c(void);
extern fint gds_rhed( fchar set, gds_coord **cinfo);
extern fint gds_whed( fchar set, gds_coord *cinfo);
extern fint gds_frhed( fchar set);
extern fint gds_ftype_c( fchar set, fint *err );
extern fint gds_itype_c( fchar set, fint *err );
extern fint gds_prime_c(fchar set, fint *err);
extern fint gds_nitems_c(fchar set, fint *err);
extern fint gds_rename_c( fchar old,fchar new );
extern bool gds_exist_c ( fchar set, fint *err );
extern void gds_delete_c (fchar set, fint *err);
extern void gds_optimize_c (fchar set, fint *err);
extern void gds_sync_c (fchar set, fint *err);
extern fint gds_recover_c (fchar set);

extern void gdsd_read_c(fchar set, fchar key, fint *level, char buf[],
                        fint *nb, fint *pos, fint *done, fint *err);
extern void gdsd_write_c(fchar set, fchar key, fint *level, char buf[],
                         fint *nb, fint *pos, fint *done, fint *err);
void gdsd_delete_c (fchar set, fchar key, fint *level, fint *err);
extern void gdsd_delall_c(fchar set, fchar key, fint *err);
void gdsd_find_c(fchar funval, fchar set, fint *level,
                 fint *record, fint *err);
fint gdsd_length_c (fchar set, fchar key, fint *level, fint *err);
void gdsd_rewind_c(fchar set, fchar key, fint *level, fint *err);
extern void gds_lock_c( fchar set, fint *err);
extern void gds_unlock_c( fchar set, fint *err);
extern int gds___image( fchar name, void (*proc)() );
extern int gds___fail(int verify, fint erri, fint *erro);
extern int gds___char2str(fchar c, char *s, int ls);
extern int gds___str2char(char *s, fchar c);
#endif
#<
*/

