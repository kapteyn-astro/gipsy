/*  gdscom.h
                              COPYRIGHT (c) 1994
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands
#>gdscom.dc3

Header:    gdscom.h

Purpose:   Define GDS client-server protocol.

File:      gdscom.h

Author:    J.P. Terlouw

Updates:   Mar  3, 1994: JPT, Document created.
#<
*/

#if !defined(_gdscom_h_)
#define _gdscom_h_
#include "gipsyc.h"
#include "gdserrors.h"
#include "gdsparams.h"

/* ========================================================================== */
/*                           Parameters                                       */
/* -------------------------------------------------------------------------- */

#define VERSION    GDS_VERSION         /* version number                     */
#define SUBVERSION GDS_SUBVERSION      /* sub-version number                 */
#define KEY_LEN    GDS_KEYLEN          /* descriptor key length (bytes)      */
#define NAMLEN     GDS_NAMLEN          /* descriptor file name length (bytes)*/
#define MAXDIM     GDS_MAXDIM          /* maximum number of dimensions       */

#define SNLEN      80                  /* name strings length                */

/* ========================================================================== */
/*                           Server opcodes                                   */
/* -------------------------------------------------------------------------- */
#define GDS_CONNECT   0    /* connect to server, must be zero */

#define SET_OPEN      1    /* open set */
#define SET_CLOSE     2    /* close set */
#define SET_CREATE    3    /* create set */
#define SET_DELETE    4    /* delete set */
#define SET_EXIST     5    /* test set existence */
#define SET_LOCK      6    /* lock set for other tasks and for file updates */
#define SET_UNLOCK    7    /* unlock set */
#define SET_SYNC      8    /* update file (write out changes) */
#define SET_RHED      9    /* obtain selected fields from set header */
#define SET_WHED     10    /* update selected fields in set header */
#define SET_PRIME    11    /* obtain set's hash table size */
#define SET_FTYPE    12    /* obtain set's floating point type */
#define SET_ITYPE    13    /* obtain set's integer type */
#define SET_RENAME   14    /* rename a set */
#define SET_OPTIMIZE 15    /* optimize descriptor lay-out and reclaim space */
#define SET_NITEMS   16    /* obtain number of descriptor items in set */
#define SET_RECOVER  17    /* attempt to recover corrupt descriptor file */

#define DSC_DELALL  101    /* delete descriptor at all levels */
#define DSC_DELETE  102    /* delete descriptor */
#define DSC_FIND    103    /* find next descriptor */
#define DSC_LENGTH  104    /* get descriptor length */
#define DSC_READ    105    /* read descriptor data */
#define DSC_REWIND  106    /* position descriptor at beginning */
#define DSC_TYPE    107    /* get descriptor type */
#define DSC_WRITE   108    /* write descriptor data */


/* ========================================================================== */
/*                          Server request structures                         */
/* -------------------------------------------------------------------------- */
typedef struct {
   fint       code;    /* function code */  
   fint       id;      /* set id */
   fint       imm;     /* immediate report */
} _gdsReqHead, *gdsReqHead;

typedef struct {
   _gdsReqHead head;
} _gdsRequest, *gdsRequest,
  _setClose, *setClose,
  _setFtype, *setFtype,
  _setItype, *setItype,
  _setLock, *setLock,
  _setUnlock, *setUnlock,
  _setRhed, *setRhed,
  _setWhed,*setWhed,
  _setOptimize,*setOptimize,
  _setSync,*setSync,
  _setPrime,*setPrime,
  _setNitems,*setNitems;
  
typedef struct {
  _gdsReqHead head;
   fint  pwd;          /* password */
   fint  version;      /* GDS version */
   fint  subversion;   /* GDS sub-version */
} _gdsConnect, *gdsConnect;

typedef struct {
   _gdsReqHead head;
   fint  size;         /* initial size of the descriptor file (bytes) */ 
   fint  incr;         /* increment for descriptor file extends (bytes) */
   fint  prime;        /* initial size of hash table (entries) */
   char  name[NAMLEN]; /* name of the set */
} _setCreate, *setCreate;

typedef struct {
   _gdsReqHead head;   
   char  name[NAMLEN]; /* name of the set */
} _setDelete, *setDelete,
  _setExist, *setExist;
  
typedef struct {
   _gdsReqHead head;
   char  oldname[NAMLEN]; /* old name of the set */
   char  newname[NAMLEN]; /* new name of the set */
} _setRename, *setRename;

typedef struct {
   _gdsReqHead head;
   fint  incr;         /* increment for descriptor file extends (bytes) */   
   char  name[NAMLEN]; /* name of the set */
} _setOpen, *setOpen;

typedef struct {
   _gdsReqHead head;
   char  name[NAMLEN]; /* name of the set */
} _setRecover, *setRecover;

typedef struct {
   _gdsReqHead head;
   fint  level;        /* coordinate 'level' */
   fint  nelems;       /* number of elements */
   fint  position;     /* starting position  */
   fint  curr;         /* server sets this   */
   fint  length;       /* server sets this   */
   fint  akrec;        /* server sets this   */
   fint  rkrec;        /* server sets this   */
   char  type;         /* descriptor type    */
   char  name[KEY_LEN];/* descriptor name    */
} _dscWrite, *dscWrite;

typedef struct {
   _gdsReqHead head;
   fint  level;        /* coordinate 'level' */
   fint  nelems;       /* number of elements */
   fint  position;     /* starting position  */
   fint  abslev;       /* absolute level flag */
   char  type;         /* descriptor type    */
   char  name[KEY_LEN];/* descriptor name    */
} _dscRead, *dscRead;

typedef struct {
   _gdsReqHead head;
   fint  level;        /* coordinate 'level' */
   char  name[KEY_LEN];/* descriptor name    */
} _dscDelete, *dscDelete,
  _dscRewind, *dscRewind;

typedef struct {
   _gdsReqHead head;
   fint  level;        /* coordinate 'level' */
   char  name[KEY_LEN];/* descriptor name    */
} _dscDelall, *dscDelall; 

typedef struct {
   _gdsReqHead head;
   fint  level;        /* coordinate 'level' */
   char  name[KEY_LEN];/* descriptor name    */
} _dscLength, *dscLength,
  _dscType, *dscType;

typedef struct {
   _gdsReqHead head;
   fint  level;        /* coordinate 'level' */
   fint  index;        /* starting index     */
} _dscFind, *dscFind;

typedef union {
   _gdsRequest  gdsrequest;
   _gdsConnect  srvconnect;
   _setClose    setclose;
   _setFtype    setftype;
   _setItype    setitype;
   _setRename   setrename;
   _setLock     setlock;
   _setUnlock   setunlock;
   _setCreate   setcreate;
   _setDelete   setdelete;
   _setExist    setexist;
   _setOpen     setopen;
   _setRhed     setrhed;
   _setWhed     setwhed;
   _setOptimize setoptimize;
   _setSync     setsync;
   _setPrime    setprime;
   _setNitems   setnitems;
   _setRecover  setrecover;
   _dscWrite    dscwrite;
   _dscRead     dscread;
   _dscDelall   dscdelall;
   _dscLength   dsclength;
   _dscRewind   dscrewind;
   _dscType     dsctype;
   _dscFind     dscfind;
} _uRequest;

/* ========================================================================== */
/*                         Server reply structure                             */
/* -------------------------------------------------------------------------- */

typedef struct _gdsReply {
   fint result;
   fint length;
} _gdsReply, *gdsReply;

#endif /* _gdscom_h_ */
