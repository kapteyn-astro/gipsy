/* arith.c
	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            arith.dc2

Subroutine:   ARITH

Purpose:      Do an arithmetic operation on two real arrays and store
              the result in a third one. This routine checks for BLANK
              values.

Category:     ARRAY

File:         arith.c

Author:       K.G. Begeman

Use:          CALL ARITH( ARRAY1,    input        real array
                          OP,        input        character
                          ARRAY2,    input        real array
                          ARRAY3,    output       real array
                          NUMBER )   input        integer

              ARRAY1    input array.
              OP        character  operand ('+','-','*','/').
              ARRAY2    input array.
              ARRAY3    output array.
              NUMBER    number of operations.

Description:  For every element of the arrays 1, 2 and 3 the
              following operation is performed:
              ARRAY3 = ARRAY1 'OP' ARRAY2
              The arrays need not be different. OP can be '+', '-', '*'
              or '/'. When it is not one of these four, ARRAY3 is
              filled with BLANKs. Other errors (e.g. zero divide) also
              result in a BLANK value.

Example:      Add 100 elements of array A to array B and store the
              result in B.
              CALL ARITH(A,'+',B,B,100)

Updates:      Jul 21, 1989: KGB, original document.

#<

@ subroutine arith( real, character, real, real, integer )

*/

#include	"stdio.h"		/* <stdio.h> */
#include	"gipsyc.h"		/* GIPSY symbols and definitions */
#include	"setfblank.h"		/* define setfblank_c */
#include	"setnfblank.h"		/* define setnfblank_c */

void arith_c( float *arr1, fchar opr, float *arr2, float *arr3, fint *number )
{
   fint   i;
   float *a1 = arr1;
   float *a2 = arr2;
   float *a3 = arr3;
   float  blank;
   float  v1;
   float  v2;

   setfblank_c( &blank );                   /* get system defined blank value */
   switch(opr.a[0]) {
      case '+': {                                               /* add arrays */
         for (i = 0; i < *number; i++) {
            v1 = *a1++;
            v2 = *a2++;
            if (blank == v1 || blank == v2) {
               *a3++ = blank;                                        /* BLANK */
            } else {
               *a3++ = v1 + v2;                                        /* add */
            }
         }
         break;
      }
      case '-': {                                          /* subtract arrays */
         for (i = 0; i < *number; i++) {
            v1 = *a1++;
            v2 = *a2++;
            if (blank == v1 || blank == v2) {
               *a3++ = blank;                                        /* BLANK */
            } else {
               *a3++ = v1 - v2;                                   /* subtract */
            }
         }
         break;
      }
      case '*': {                                          /* multiply arrays */
         for (i = 0; i < *number; i++) {
            v1 = *a1++;
            v2 = *a2++;
            if (blank == v1 || blank == v2) {
               *a3++ = blank;                                        /* BLANK */
            } else {
               *a3++ = v1 * v2;                                   /* multiply */
            }
         }
         break;
      }
      case '/': {                                            /* divide arrays */
         for (i = 0; i < *number; i++) {
            v1 = *a1++;
            v2 = *a2++;
            if (blank == v1 || blank == v2) {
               *a3++ = blank;                                        /* BLANK */
            } else if (v2 == 0.0) {
               *a3++ = blank;                                        /* BLANK */
            } else {
               *a3++ = v1 / v2;                                     /* divide */
            }
         }
         break;
      }
      default: {                                                         /* ? */
         setnfblank_c( a3, number );                   /* blank the whole lot */
         break;
      }
   }
}

#if defined(TESTBED)
void main()
{
   float a1[10], a2[10], a[10], s[10], m[10], d[10];
   fchar opr;
   fint  n = 10;
   fint  i;
   for (i = 0; i < n; i++) {
      a1[i] = a2[i] = (float) i;
   }
   setfblank_c( &a1[4] ); setfblank_c( &a2[8] );
   opr.l = 1; opr.a = "+"; arith_c( a1, opr, a2, a, &n );
   opr.l = 1; opr.a = "-"; arith_c( a1, opr, a2, s, &n );
   opr.l = 1; opr.a = "*"; arith_c( a1, opr, a2, m, &n );
   opr.l = 1; opr.a = "/"; arith_c( a1, opr, a2, d, &n );
   printf("    A1         A2        ADD        SUB        MUL        DIV\n");
   for (i = 0; i < n; i++) {
      if (fblank_c( &a1[i] )) printf("     BLANK "); else printf("%10f ",a1[i]);
      if (fblank_c( &a2[i] )) printf("     BLANK "); else printf("%10f ",a2[i]);
      if (fblank_c( &a[i] )) printf("     BLANK "); else printf("%10f ",a[i]);
      if (fblank_c( &s[i] )) printf("     BLANK "); else printf("%10f ",s[i]);
      if (fblank_c( &m[i] )) printf("     BLANK "); else printf("%10f ",m[i]);
      if (fblank_c( &d[i] )) printf("     BLANK\n"); else printf("%10f\n",d[i]);
   }
}
#endif
