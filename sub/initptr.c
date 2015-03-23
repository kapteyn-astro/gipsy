/* initptr.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

*/

#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"gipsyc.h"			/* GIPSY symbols */

static	fint	*fp = NULL;
static	fint	*b1p = NULL, *b2p = NULL;
static	fint	*gip = NULL, *gop = NULL;
static	fint	*gis = NULL, *gos = NULL;
static	fint	*p1p = NULL, *p2p = NULL;
static	fint	*mp = NULL;
static	fint	inc;
static	fint	dim;
static	fint	nic, noc;

static	void	reseti( void )
{
   fint	n;

   nic = 0;
   for (n = 0; n < dim; n++) gip[n] = gis[n];
}

static	void	reseto( void )
{
   fint	n;

   noc = 0;
   for (n = 0; n < dim; n++) gop[n] = gos[n];
}

static	bool	inside( fint g[] )
{
   bool	ins = 1;
   fint	n = dim - 1;

   do {
      ins = ((g[n] >= b1p[n]) && (g[n] <= b2p[n]));
   } while ((ins) && (n-- > 0));
   return(ins);
}

static	void	next( fint g[] )
{
   fint	n = 0;
   fint	l;

   while (g[n] > fp[n]) {
      l = 0;
      while (g[n] > fp[n]) {
         g[n] -= fp[n];
         l++;
      }
      if (++n == dim) break;
      g[n] += l;
   }
}

/*
#>            initptr.dc2

Subroutine:   INITPTR

Purpose:      Pointer handling within parts of subset.

File:         initptr.c

Author:       K.G. Begeman

Use:          CALL INITPTR( RFLO,     	Input      INTEGER ARRAY
                            RFHI,       Input      INTEGER ARRAY
                            SFLO,       Input      INTEGER ARRAY
                            SFHI,       Input      INTEGER ARRAY
                            NDIM,       Input      INTEGER
                            NR,         Input      INTEGER
                            NT )       In/Output   INTEGER

              RFLO     Array with the lower coordinates of readframe.
              RFHI     Array with the upper coordinates of readframe.
              SFLO     Array with the lower coordinates of subframe.
              SFHI     Array with the upper coordinates of subframe.
              NDIM     Dimension of subset (length of RFLO,RFHI, etc.).
              NR       Number of data values currently in readbuffer.
              NT       On input, number of points done sofar, on output
                       number of points done. Initial value must be 0!

Description:  The subroutine INITPTR is used in combination with
              GDSI_READ. GDSI_READ returns the number of data values
              actually read, which is input to INITPTR in order to
              initialize the logical functions INSIDEPTR and
              OUTSIDEPTR. Further input to INITPTR are the dimension
              of the subset, the readframe (part of the subset which
              is to be read by GDSI_READ) and a subframe (which is
              contained in the readframe). After each call to GDSI_READ
              and INITPTR the functions INSIDEPTR and OUTSIDEPTR
              return pointers to datavalues (in the buffer filled by
              GDSI_READ) which are inside or outside the subframe.
              INSIDEPTR and OUTSIDEPTR return .FALSE. if all
              positions in the readbuffer have been scanned. These
              routines provide the applications programmer with a
              simple tool to handle data in subframes (i.e. frames
              returned from BOXINP) in a different way than the data
              outside the subframe. See the example below.

Example:      INTEGER SIZE
              PARAMETER (SIZE=4096)
              INTEGER MAXAX
              PARAMETER (MAXAX=5)
              CHARACTER*80 SET
              INTEGER C1,C2,F1(MAXAX),F2(MAXAX),B1(MAXAX),B2(MAXAX)
              INTEGER IP,NP,NR,NT,NW,IERR,IERW
              REAL RBUF(SIZE)
              LOGICAL INSIDEPTR, OUTSIDEPTR
              NT=0
              REPEAT
                CALL GDSI_READ(SET,C1,C2,RBUF,SIZE,NR,IERR)
                CALL INITPTR(F1,F2,B1,B2,NDIM,NR,NT)
                WHILE (INSIDEPTR(IP,NP))
                  CALL SCALE1(RBUF(IP+1),NP,2.0,0.0)
                CWHILE
                WHILE (OUTSIDEPTR(IP,NP))
                  CALL SCALE1(RBUF(IP+1),NP,0.5,0.0)
                CWHILE
                CALL GDSI_WRITE(SET,C1,C2,RBUF,NR,NW,IERW)
              UNTIL (IERR .EQ. 0)

              COPY and SCALE are example programs which
              demonstrate the use of INITPTR.

Related Docs: insideptr.dc2, outsideptr.dc2

Updates:      Nov 15, 1988: KGB, Creation date.
              Jan  3, 1992: KGB, Bug removed.

#<

@ subroutine initptr( integer, integer, integer, integer,
@                     integer, integer, integer )

*/

void	initptr_c( fint *f1 ,
                   fint *f2 ,
                   fint *b1 ,
                   fint *b2 ,
                   fint *ndim ,
                   fint *nd ,
                   fint *ntot )
{
   fint	i, j;
   fint	ipos;

   if (*ntot == 0) {
      int	s = sizeof(fint);
      int	d = dim = *ndim;

      if (dim) {
         if (fp != NULL) {
            free(fp);
            free(b1p);
            free(b2p);
            free(gip);
            free(gop);
            free(gis);
            free(gos);
            free(p1p);
            free(p2p);
            free(mp);
         }
         fp = calloc(d,s);
         b1p = calloc(d,s);
         b2p = calloc(d,s);
         gip = calloc(d,s);
         gop = calloc(d,s);
         gis = calloc(d,s);
         gos = calloc(d,s);
         p1p = calloc(d,s);
         p2p = calloc(d,s);
         mp = calloc(d,s);
         for (j = 1, i = 0; i < dim; i++) {
            fp[i] = f2[i] - f1[i] + 1;
            b1p[i] = b1[i] - f1[i] + 1;
            b2p[i] = b2[i] - f1[i] + 1;
            p2p[i] = 1;
            mp[i] = j;
            j = j * fp[i];
         }
      }
   }
   ipos = *ntot += inc = *nd;
   ipos++;
   for (i = dim; i-- > 0; ) {
      fint	g = (ipos - 1)/ mp[i];

      ipos -= g * mp[i];
      gis[i] = gos[i] = gip[i] = gop[i] = p1p[i] = p2p[i];
      p2p[i] = 1 + g;
   }
   nic = noc = 0;
}

/*
#>            insideptr.dc2

Function:     INSIDEPTR

Purpose:      Indicates whether inside or outside subframe defined by
              INITPTR.

File:         initptr.c

Author:       K.G. Begeman

Use:          LOGICAL INSIDEPTR( IP,    Output   INTEGER
                                 NP )   Output   INTEGER

              INSIDEPTR  Returns .TRUE. when still some data inside
                         subframe in readbuffer, else .FALSE.
              IP         If INSIDEPTR .eq. .TRUE., offset pointer from
                         start of readbuffer to where first data value
                         insiderame can be found.
              NP         If INSIDEPTR .eq. .TRUE., number of data values
                         inside subframe in readbuffer starting at IP.

Notes:        When INSIDEPTR returns .FALSE. it reinitializes itself
              to the state just after the call to INITPTR. So it can
              be used again to do a different operation on data inside
              the sub frame.

Related Docs: initptr.dc2, outsideptr.dc2

Updates:      Nov 15, 1988: KGB, Creation date.
              Feb 27, 1990: KGB, Reinitialization implemented.

#<

@ logical function insideptr( integer, integer )

*/

bool	insideptr_c( fint *ip ,
                     fint *np )
{
   fint	n;

   if (nic >= inc) {
      reseti();              /* reset to initial status after call to initptr */
      return(FALSE);
   }
   if (!dim) {
      *ip = 0;
      *np = 1;
      nic += inc;
      return(TRUE);
   }
   *ip = nic;
   *np = 0;
   if (!dim) return(TRUE);
   while (!inside(gip)) {
      if (b1p[0] > gip[0]) {
         nic += n = b1p[0] - gip[0];
      } else {
         nic += n = b1p[0] - gip[0] + fp[0];
      }
      if (nic >= inc) {
         reseti();          /* reset to initial status after call to initptr */
         return(FALSE);
      }
      gip[0] += n;
      *ip += n;
      next(gip);
   }
   while (inside(gip)) {
      nic += n = b2p[0] - gip[0] + 1;
      if (nic >= inc) {
         *np = inc - *ip;
         return(TRUE);
      }
      gip[0] += n;
      *np += n;
      next(gip);
   }
   return(TRUE);
}

/*
#>            outsideptr.dc2

Function:     OUTSIDEPTR

Purpose:      Indicates whether inside or outside subframe defined by
              INITPTR.

File:         initptr.c

Author:       K.G. Begeman

Use:          LOGICAL OUTSIDEPTR( IP,   Output   INTEGER
                                  NP )  Output   INTEGER

              OUTSIDEPTR  Returns .TRUE. when still some data outside
                          subframe in readbuffer, else .FALSE.
              IP          If OUTSIDEPTR .eq. .TRUE., offset pointer from
                          start of readbuffer to where first data value
                          outside subframe can be found.
              NP          If OUTSIDEPTR .eq. .TRUE., number of data values
                          outside subframe in readbuffer starting at IP.

Notes:        When OUTSIDEPTR returns .FALSE. it reinitializes itself
              to the state just after the call to INITPTR. So it can
              be used again to do a different operation on data outside
              the sub frame.

Related Docs: initptr.dc2, insideptr.dc2

Updates:      Nov 15, 1988: KGB, Creation date.
              Feb 27, 1990: KGB, Reinitialization implemented.

#<

@ logical function outsideptr( integer, integer )

*/

bool outsideptr_c( fint *ip, fint *np )
{
   fint n;
   if (noc >= inc) {
      reseto();              /* reset to initial status after call to initptr */
      return(FALSE);
   }
   if (!dim) {
      noc += inc;
      return(FALSE);
   }
   *ip = noc;
   *np = 0;
   while (inside(gop)) {
      noc += n = b2p[0] - gop[0] + 1;
      if (noc >= inc) {
         reseto();           /* reset to initial status after call to initptr */
         return(FALSE);
      }
      gop[0] += n;
      *ip += n;
      next(gop);
   }
   while (!inside(gop)) {
      if (b1p[0] > gop[0]) {
         noc += n = b1p[0] - gop[0];
      } else {
         noc += n = b1p[0] - gop[0] + fp[0];
      }
      if (noc >= inc) {
         *np = inc - *ip;
         return(TRUE);
      }
      gop[0] += n;
      *np += n;
      next(gop);
   }
   return(TRUE);
}

#if defined(TESTBED)
main(argc,argv)
int  argc;
char *argv[];
{
   bool	bingo = 1;
   fint area[] = {
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
      0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
      0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
      0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
      0, 0, 1, 1, 1, 1, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0
   };
   fint save[sizeof(area)/sizeof(fint*)];
   fint frame2[] = { 10, 10 };
   fint frame1[] = { 1, 1 };
   fint boxlo[] = {  3,  3 };
   fint boxhi[] = {  6,  7 };
   fint d = 2;
   fint ntot = 0;
   fint nd = 2;
   fint ip, np;
   fint nt = 0;
   if (argc > 1) {
      sscanf(argv[1],"%ld",&nd);
   } else {
      nd = 2;
   }
   if (nd <= 0) nd = 7;
   printf("INITPTR, Step:%5ld\n",nd);
   printf("Initial:\n");
   for (ip = 0; ip < 10; ip++) {
      for (np = 0; np < 10; np++) {
         save[ip*10+np] = area[ip*10+np];
         printf("%5ld",area[ip*10+np]);
      }
      printf("\n");
   }
   do {
      if ((ntot + nd) > 100) nd = 100 - ntot;
      initptr_c(frame1,frame2,boxlo,boxhi,&d,&nd,&ntot);
      while (tobool(insideptr_c(&ip,&np))) {
         fint i;
         for (i = 0; i < np; i++) area[nt+ip+i] += 2;
      }
      while (tobool(outsideptr_c(&ip,&np))) {
         fint i;
         for (i = 0; i < np; i++) area[nt+ip+i] += 4;
      }
      while (tobool(insideptr_c(&ip,&np))) {
         fint i;
         for (i = 0; i < np; i++) area[nt+ip+i] += 2;
      }
      while (tobool(outsideptr_c(&ip,&np))) {
         fint i;
         for (i = 0; i < np; i++) area[nt+ip+i] += 4;
      }
      nt = ntot;
   } while (ntot < 100);
   printf("Result:\n");
   for (ip = 0; ip < 10; ip++) {
      for (np = 0; np < 10; np++) {
         printf("%5ld",area[ip*10+np]);
         if (area[ip*10+np] == 8 && save[ip*10+np] != 0) bingo = 0;
         if (area[ip*10+np] == 5 && save[ip*10+np] != 1) bingo = 0;
      }
      printf("\n");
   }
   if (bingo) {
      printf( "bingo\n" );
   } else {
      printf( "not bingo\n" );
   }
}
#endif
