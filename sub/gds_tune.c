/* gds_tune.c

#>            gds_tune.dc3

Document:     gds_tune

Purpose:      Contains GDS tuning routines.

Category:     GDS

File:         gds_tune.c

Author:       J.P. Terlouw

Description:  Refer to separate doucments. Current members are the
              modules gdst_incr, gdst_prime, gdst_initsize and
              gdst_immediate.

Updates:      Jul 15, 1991: KGB, Document created.
              Mar  1, 1994: JPT, Modified for GDS server.

#<
#>gds_tune.h
#if !defined(_gds_tune_h_)
#define _gds_tune_h_
int gds___initsize( void );
int gds___extendsize( void );
int gds___prime( void );
int gds___immediate( void );
#endif
#<

*/

#include "gipsyc.h"
#include "gds_tune.h"

static fint Init_size=0;            /* descriptor file initial size (bytes)*/
static fint Extend_size=0;          /* decriptor file extend size (bytes)  */
static fint Prime=0;                /* default hash table size (entries)   */
static bool Abslevel=0;             /* suppress search at higher levels    */
static bool Immediate=0;            /* immediate reporting for locked sets */
/* -------------------------------------------------------------------------

                                GDST_INCR

@subroutine gdst_incr( integer )
---------------------------------------------------------------------------- */

/*
#>gdst_incr.dc2

Subroutine:   GDST_INCR

Purpose:      Change increment for descriptor file extends.

Author:       J.P. Terlouw

Category:     GDS

File:         gds_tune.c

Use:          CALL GDST_INCR ( 
                               INCR )         Input   INTEGER

              INCR  =  increment by which file is extended (bytes).
               
Updates:      29-Sep-87   original document
              20-Sep-89   implementation in C
               1-Mar-94    modified for GDS server
#<
*/

void gdst_incr_c ( fint *nincr)
{
	Extend_size = *nincr;
}

/* -------------------------------------------------------------------------

                                GDST_PRIME

@subroutine gdst_prime( integer )
---------------------------------------------------------------------------- */

/*
*#>gdst_prime.dc2
*
*Subroutine:   GDST_PRIME
*
*Purpose:      Change hash table size for sets to be created.
*
*Author:       J.P. Terlouw
*
*Category:     GDS
*
*File:         gds_tune.c
*
*Use:          CALL GDST_PRIME ( 
*                                SIZE )     Input     INTEGER
*
*                 SIZE  =  approximate size of hash table in sets which 
*                          are created after this call. 
*
*Description:  This routine can be used if the descriptor file is
*              expected to contain a large number of descriptor items.
*              Increasing the hash table makes retrieval of descriptor items
*              more efficient. (The size of the hash table does NOT restrict
*              the possible number of items in any way.)
*
*              As a rule of thumb, the hash table size should be of the
*              same order of magnitude as the number of expected descriptor
*              items. The default hash table size is defined by
*              a constant in the GDS server code (now 311). This default can be
*              re-established by calling  GDST_PRIME with size=0.
*
*              The size of the hash table can later be optimized by calling
*              GDS_OPTIMIZE. Setting the size before opening is however
*              more efficient.
*
*Updates:      26-May-87   original document
*              20-Sep-89   implementation in C
*               1-Mar-94   modified for GDS server
*#<
*/

void gdst_prime_c ( fint *nprime)
{
	Prime = *nprime;
}

/* -------------------------------------------------------------------------

                                GDST_INITSIZE

@subroutine gdst_initsize( integer )
---------------------------------------------------------------------------- */

/*
*#>gdst_initsize.dc2
*
*Subroutine:   GDST_INITSIZE
*
*Purpose:      Change initial file size for sets to be created.
*
*Author:       J.P. Terlouw
*
*Category:     GDS
*
*File:         gds_tune.c
*
*Use:          CALL GDST_INITSIZE ( 
*                                   SIZE )        Input   INTEGER
*
*              SIZE  =  initial size in bytes of sets which 
*                       are created after this call.
*
*Updates:      26-May-87   original document
*              20-Sep-89   implementation in C
*              1-Mar-94    modified for GDS server
*#<
*/

void gdst_initsize_c ( fint *ninit)
{
	Init_size = *ninit;
}

/* -------------------------------------------------------------------------

                                GDST_IMMEDIATE

@subroutine gdst_immediate( logical )
---------------------------------------------------------------------------- */

/*
*#>gdst_immediate.dc2
*
*Subroutine:   GDST_IMMEDIATE
*
*Purpose:      Set "immediate reporting" for locked sets.
*
*Author:       J.P. Terlouw
*
*Category:     GDS
*
*File:         gds_tune.c
*
*Use:          CALL GDST_IMMEDIATE (
*                                    IMM )      Input    LOGICAL
*
*                 IMM   =  If .TRUE., any request for a locked
*                          set returns immediately with an error code.
*                          If .FALSE., the request is suspended until the
*                          set is unlocked. By default GDS will wait until
*                          the set is unlocked.
*
*Related documents: gds_lock.dc2, gds_unlock.dc2.
*
*Updates:      Mar  1, 1994: JPT, original document.
*#<
*/

void gdst_immediate_c ( bool *imm)
{
	Immediate = *imm;
}

/* -------------------------------------------------------------------------

                                GDST_ABSLEVEL

@subroutine gdst_abslevel( logical )
---------------------------------------------------------------------------- */

/*
*#>gdst_abslevel.dc2
*
*Subroutine:   GDST_ABSLEVEL
*
*Purpose:      Disable/enable search at higher levels.
*
*Author:       J.P. Terlouw
*
*Category:     GDS
*
*File:         gds_tune.c
*
*Use:          CALL GDST_ABSLEVEL ( 
*                                   ABSLEV )    Input    LOGICAL
*
*                 ABSLEV   If .TRUE., GDS does not search at higher
*                          higher levels if an item is not present at
*                          the specified level; it will read on
*                          "absolute levels". By default GDS attempts
*                          to read at the specified level and, if this
*                          fails, at levels above the specified level.
*
*Updates:      Mar  2, 1994: JPT, original document.
*#<
*/

void gdst_abslevel_c ( bool *abslev)
{
	Abslevel = *abslev;
}

/*

#>            gds___initsize.dc3

Function:     gds___initsize

Purpose:      Returns initial size set by GDST_INITSIZE.

Category:     BASIC-GDS

File:         gds_tune.c

Author:       J.P. Terlouw

Use:          int gds___initsize( )

Warning:      Not callable from Fortran.

Updates:      Aug 24, 1991: KGB, Document created.
              Mar  1, 1994: JPT, Modified for GDS server.
              
#<

*/

int gds___initsize(void)
{
   return Init_size;
}

/*

#>            gds___extendsize.dc3

Function:     gds___extendsize

Purpose:      Returns extendsize set by GDST_INCR.

Category:     BASIC-GDS

File:         gds_tune.c

Author:       J.P. Terlouw

Use:          int gds___extendsize( )

Warning:      Not callable from Fortran.

Updates:      Aug 24, 1991: KGB, Document created.
              Mar  1, 1994: JPT, Modified for GDS server.
              
#<

*/

int gds___extendsize(void)
{
   return Extend_size;
}

/*

#>            gds___prime.dc3

Function:     gds___prime

Purpose:      Returns prime set by GDST_PRIME.

Category:     BASIC-GDS

File:         gds_tune.c

Author:       J.P. Terlouw

Use:          int gds___prime( )

Warning:      Not callable from Fortran.

Updates:      Aug 24, 1991: KGB, Document created.
              Mar  1, 1994: JPT, Modified for GDS server.
              
#<

*/

int gds___prime(void)
{
   return Prime;
}

/*

#>            gds___immediate.dc3

Function:     gds___immediate

Purpose:      Returns value set by GDST_IMMEDIATE.

Category:     BASIC-GDS

File:         gds_tune.c

Author:       J.P. Terlouw

Use:          int gds___immediate( )

Warning:      Not callable from Fortran.

Updates:      Mar  1, 1994: JPT, Original document.
              
#<

*/

int gds___immediate(void)
{
   return Immediate;
}
/*

#>            gds___abslevel.dc3

Function:     gds___abslevel

Purpose:      Returns value set by GDST_ABSLEVEL.

Category:     BASIC-GDS

File:         gds_tune.c

Author:       J.P. Terlouw

Use:          int gds___abslevel( )

Warning:      Not callable from Fortran.

Updates:      Mar  2, 1994: JPT, Original document.
              
#<

*/

int gds___abslevel(void)
{
   return Abslevel;
}

