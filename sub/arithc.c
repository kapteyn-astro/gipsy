/* arithc.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            arithc.dc2

Subroutine:   ARITHC

Purpose:      Do an arithmetic operation on a real constant and a real
              array and store the result in a second one. This routine
              checks for BLANK values.

Category:     ARRAY

File:         arithc.c

Author:       K.G. Begeman

Use:          CALL ARITHC( CONSTANT,      input    real
                           OP,            input    character
                           ARRAY1,        input    real array
                           ARRAY2,        output   real array
                           NUMBER )       input    integer

              CONSTANT  constant.
              OP        character  operand ('+','-','*','/').
              ARRAY1    input array.
              ARRAY2    output array.
              NUMBER    number of operations.

Description:  For every element of the arrays 1 and 2 the
              following operation is performed:
              ARRAY2 = CONSTANT 'OP' ARRAY1
              The arrays need not be different. OP can be '+', '-', '*'
              or '/'. When it is not one of these four, ARRAY2 is
              filled with BLANKs. Other errors (e.g. zero divide) also
              result in a BLANK value.

Example:      Add 10.0 to 100 elements of array A and store the result
              in B.
              CALL ARITH(10.0,'+',A,B,100)

Updates:      Jul 21, 1989: KGB, original document.

#<

@ subroutine arithc( real, character, real, real, integer )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"setfblank.h"		/* define setfblank_c */
#include	"setnfblank.h"		/* define setnfblank_c */

void arithc_c( float *c, fchar opr, float *array1, float *array2, fint *number )
{
   fint   i;
   float *a1 = array1;
   float *a2 = array2;
   float  blank;
   float  v1;

   setfblank_c( &blank );                         /* get system defined BLANK */
   if (blank == *c ) {
      setnfblank_c( a2, number );                        /* make it all BLANK */
   } else {
      switch(opr.a[0]) {                                      /* add to array */
         case '+': {
            for (i = 0; i < *number; i++) {
               v1 = *a1++;
               if (blank == v1) {
                  *a2++ = blank;                                     /* BLANK */
               } else {
                  *a2++ = (*c) + v1;                                   /* add */
               }
            }
            break;
         }
         case '-': {                                        /* subtract array */
            for (i = 0; i < *number; i++) {
               v1 = *a1++;
               if (blank == v1) {
                  *a2++ = blank;                                     /* BLANK */
               } else {
                  *a2++ = (*c) - v1;                              /* subtract */
               }
            }
            break;
         }
         case '*': {                                        /* multiply array */
            for (i = 0; i < *number; i++) {
               v1 = *a1++;
               if (blank == v1) {
                  *a2++ = blank;                                     /* BLANK */
               } else {
                  *a2++ = (*c) * v1;                              /* multiply */
               }
            }
            break;
         }
         case '/': {                                       /* divide by array */
            for (i = 0; i < *number; i++) {
               v1 = *a1++;
               if (blank == v1) {
                  *a2++ = blank;                                     /* BLANK */
               } else if (v1 == 0.0) {
                  *a2++ = blank;                                     /* BLANK */
               } else {
                  *a2++ = (*c) / v1;                                /* divide */
               }
            }
            break;
         }
         default: {                                                      /* ? */
            setnfblank_c( a2, number );
            break;
         }
      }
   }
}

#if defined(TESTBED)
void main()
{
   float c = 10.0, a1[10], a[10], s[10], m[10], d[10];
   fchar opr;
   fint  n = 10;
   fint  i;
   for (i = 0; i < n; i++) {
      a1[i] = (float) i;
   }
   setfblank_c( &a1[4] ); setfblank_c( &a1[8] );
   opr.l = 1; opr.a = "+"; arithc_c( &c, opr, a1, a, &n );
   opr.l = 1; opr.a = "-"; arithc_c( &c, opr, a1, s, &n );
   opr.l = 1; opr.a = "*"; arithc_c( &c, opr, a1, m, &n );
   opr.l = 1; opr.a = "/"; arithc_c( &c, opr, a1, d, &n );
   printf("    A1        ADD        SUB        MUL        DIV\n");
   for (i = 0; i < n; i++) {
      if (fblank_c(&a1[i])) printf("     BLANK "); else printf("%10f ",a1[i]);
      if (fblank_c(&a[i])) printf("     BLANK "); else printf("%10f ",a[i]);
      if (fblank_c(&s[i])) printf("     BLANK "); else printf("%10f ",s[i]);
      if (fblank_c(&m[i])) printf("     BLANK "); else printf("%10f ",m[i]);
      if (fblank_c(&d[i])) printf("     BLANK\n"); else printf("%10f\n",d[i]);
   }
}
#endif
