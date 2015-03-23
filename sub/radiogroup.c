/* radiogroup.c

                              COPYRIGHT (c) 1999
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands

Author: J.P. Terlouw
*/

/*
  ========================= Documentation ===================================

#> radiogroup.dc2
Document:     radiogroup

Purpose:      Set of routines for coordinating groups of keywords.

Category:     USER INTERFACE

File:         radiogroup.c

Author:       J.P. Terlouw

Description:  These routines allow sets of user input keywords of type
              LOGICAL to be coordinated in groups. With every group
              a user input keyword is associated. This keyword is specified
              when the group is created with the function RadioGroup().

              Member keywords can be added to a group by calling RadioAddKey().
              In this call a value is specified which will be associated
              with the keyword.

              Finally a group can be deleted by calling RadioDeleteGroup().
              It is currently not possible to remove members from a group.

              When a group exists, it behaves as follows:
              - when one of the keywords is set to TRUE, all other keywords
                are set to FALSE. The keyword associated with the group
                will be set to the value associated with the keyword which
                was set to TRUE.
              - When the keyword with value TRUE is set to false,
                the keyword associated with the group will be set to an
                empty value.

Related docs: RadioGroup.dc2, RadioAddKey, RadioDeleteGroup.

See also:     Example program in TESTBED section of source code.

Updates:      May 12, 1999: JPT, Document created.
#<
#> RadioAddKey.dc2
Function:     RadioAddKey

Purpose:      Ad a keyword to a group of associated keywords.

Category:     USER-INTERFACE

File:         radiogroup.c

Author:       J.P. Terlouw

Use:          #include "radiogroup.h"
              ident group;
              char  *key, *value;
              RadioAddKey(group, key, value);

              group  -  group to which 'key' is to be added.
              key    -  LOGICAL user input keyword to be added to the group.
              value  -  associated value to be assigned to the group keyword
                        when 'key' becomes TRUE.

Related docs: radiogroup.dc2 RadioGroup.dc2, RadioDeleteGroup.dc2.

Updates:      May 12, 1999: JPT, Document created.
#<
#> RadioDeleteGroup.dc2
Function:     RadioDeleteGroup

Purpose:      Delete a group of associated keywords.

Category:     USER-INTERFACE

File:         radiogroup.c

Author:       J.P. Terlouw

Use:          #include "radiogroup.h"
              ident group;
              RadioDeleteGroup(&group);

              group  -  group to be deleted. Please note that the argument
                        to the call is a pointer to 'group'.

Related docs: radiogroup.dc2 RadioGroup.dc2, RadioAddKey.dc2.

Updates:      May 12, 1999: JPT, Document created.
#<
#> RadioGroup.dc2
Function:     RadioGroup

Purpose:      Create a group of associated keywords.

Category:     USER-INTERFACE

File:         radiogroup.c

Author:       J.P. Terlouw

Use:          #include "radiogroup.h"
              ident group;
              char  *key;
              group = RadioGroup(key);

              group  -  'handle' by which the group can be referenced.
              key    -  user input keyword associated with group.

Related docs: radiogroup.dc2 RadioAddKey.dc2, RadioDeleteGroup.dc2.

Updates:      May 12, 1999: JPT, Document created.
#<

   ============================ Interface ===================================

#> radiogroup.h
#if !defined (_radiogroup_h_)
#define _radiogroup_h_
ident RadioGroup(char *key);
void RadioAddKey(ident gr, char *key, char *value);
void RadioDeleteGroup(ident *gr);
#endif
#<

*/

#include "stdlib.h"
#include "gipsyc.h"
#include "userfio.h"
#include "keyevents.h"
#include "radiogroup.h"

#define New(type) ((type *)calloc(1,sizeof(type)))
#define NNew(n,type) ((type *)calloc(n,sizeof(type)))
#define Delete(x)  {free(x); x=NULL;}

typedef struct Member {
   struct Member *next;
   void   *group;
   char   *value;
   ident  handler;
} _Member, *Member;

typedef struct {
   char   key[KEYLEN];
   char   prv[KEYLEN];
   bool   expected;
   Member first;
} _Group, *Group;

/* -------------------------------------------------------------------------- */
/*                                 StrDup                                     */
/* -------------------------------------------------------------------------- */
static char *StrDup (char *orig)
{
   char *result=NNew(strlen(orig)+1,char);
   strcpy(result,orig);
   return result;
}
         
/* -------------------------------------------------------------------------- */
/*                                 changed                                    */
/* -------------------------------------------------------------------------- */
static void changed(ident id, char *key, int code, void *arg)
{
   Member member=(Member)arg;
   Group group=(Group)member->group;
   Member *current;
   bool on=toflog(FALSE);

   (void)userflog(&on, 1, 2, key, " ");
   if (tobool(on)) {
      if (*group->prv) {
         wkeyf("%sNO", group->prv);
         group->expected = TRUE;
      }
      wkeyf("%s%s", group->key, member->value);
      strcpy(group->prv, key);
   } else {
      if (!group->expected) {
         wkeyf("%s", group->key);
         *group->prv = '\0';
      } else {
         group->expected = FALSE;
      }
   }
}

/* ========================================================================== */
/*                                 RadioGroup                                 */
/* -------------------------------------------------------------------------- */
ident RadioGroup(char *key)
{
   Group result=New(_Group);
    
   strcpy(result->key, key);
   return (ident)result;
}

/* ========================================================================== */
/*                                 RadioAddKey                                */
/* -------------------------------------------------------------------------- */
void RadioAddKey(ident gr, char *key, char *value)
{
   Member member=New(_Member);
   Group  group=(Group)gr;
    
   member->next = group->first;                          /* link into list */
   group->first = member;
   
   member->group = group;                                /* back link */
   member->value = StrDup(value);
   member->handler = ScheduleKeyevent(changed, key, KEYCHANGE, member);
}

/* ========================================================================== */
/*                                 RadioDeleteGroup                           */
/* -------------------------------------------------------------------------- */
void RadioDeleteGroup(ident *gr)
{
   Group  group=(Group)*gr;
   Member next=group->first;
   Member current;
    
   while (next) {
      current = next;
      next = current->next;
      DescheduleKeyevent(&current->handler);
      Delete(current->value);
      Delete(current);
   }
   Delete(group);
   *gr = NULL;
}

/* ========================================================================== */
#if defined(TESTBED)
#define NG 3
#define NM 4
#include "init.h"
#include "finis.h"
#include "cmain.h"
#include "eventmonitor.h"

MAIN_PROGRAM_ENTRY
{
   int   ig, im;
   ident groups[NG];
   char  keyname[KEYLEN];
   char  value[10];
   
   init_c();
   for (ig=0; ig<NG; ig++) {
      sprintf(keyname, "GROUP_%d=", ig);
      groups[ig] = RadioGroup(keyname);
      for (im=0; im<NM; im++) {
         sprintf(keyname, "MEMBER_%d_%d=", ig, im);
         sprintf(value, "%d", im);
         RadioAddKey(groups[ig], keyname, value);
      }
   }
   RadioDeleteGroup(&groups[1]);
   eventmonitor(NULL, TRUE);
   MainLoop();
}
#endif
