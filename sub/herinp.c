/* herinp.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.


name:      HERINP

author:    K.G. Begeman

errors:     #    explanation
           -11   bad call
           -12   unknown function
           -13   syntax error
           -14   illegal character
           -15   wrong repeat argument
           -16   wrong number of arguments
           -17   arithmetic error
           -18   not enough internal memory
           -19   conversion error
           -20   unequal list length
           -21   empty list
           -22   nested lists
           -23   output buffer overflow
           -24   floating overflow/underflow in conversion

updates:   16/dec/87 : Type C implemented (KGB)
           23/mar/88 : Argument NCHR added (KGB)
           14/apr/88 : Error if after , no values (KGB)
           13/oct/88 : Implemented ANSI F77 calls (KGB)
            1/mar/89 : Bug in decoding logicals removed (KGB)
           10/aug/93 : Finally fixed bug in loop when start = end (KGB)
           25/jul/94 : Delegate decoding to Hermes for GIPSY tasks (JPT)
*/

#include "stdio.h"
#include "string.h"
#include "stdlib.h"
#include "ctype.h"
#include "math.h"
#include "f2cvvdefs.h"

#include "gipsytask.h"
#include "decodeint.h"
#include "decodereal.h"
#include "decodedble.h"

#if defined(TESTBED)
/* To make this run without GIPSY library, define fblank_c and setfblank_c */
static bool fblank_c( val )
float *val;
{
   union {
      int   i;
      float f;
   } u;
   u.f = *val;
   if (u.i == 0xffffffff) return( 1 ); else return( 0 );
}

static void setfblank_c( val )
float *val;
{
   union {
      int   i;
      float f;
   } u;
   u.i = 0xffffffff;
   *val = u.f;
}
#else
extern bool fblank_c();
extern void setfblank_c();
#endif

#define byte    char
#define DEFAULT 1

static void   dcd_inifblank();
static int    dcd_round();
static void   dcd_error();
static void   dcd_gencode();
static void   dcd_genconst();
static void   dcd_inilist();
static void   dcd_beginlist();
static void   dcd_endlist();
static void   dcd_putlist();
static void   dcd_nextch();
static void   dcd_nextsym();
static void   dcd_nextwr();
static void   dcd_movenum();
static void   dcd_loop();
static void   dcd_expression();
static void   dcd_term();
static void   dcd_factor();
static void   dcd_function();
static void   dcd_list();
#if defined(TESTBED)
static void   dcd_dump();
#endif
static void   dcd_push();
static double dcd_pop();
static double dcd_add();
static double dcd_sub();
static double dcd_mul();
static double dcd_div();
static double dcd_neg();
static double dcd_pwr();
static double dcd_sin();
static double dcd_asin();
static double dcd_sinh();
static double dcd_cos();
static double dcd_acos();
static double dcd_cosh();
static double dcd_tan();
static double dcd_atan();
static double dcd_tanh();
static double dcd_atan2();
static double dcd_rad();
static double dcd_deg();
static double dcd_pi();
static double dcd_exp();
static double dcd_ln();
static double dcd_log();
static double dcd_sqrt();
static double dcd_abs();
static double dcd_sinc();
static double dcd_max();
static double dcd_min();
static double dcd_erf();
static double dcd_erfc();
static double dcd_mod();
static double dcd_int();
static double dcd_nint();
static double dcd_sign();
static double dcd_ifgt();
static double dcd_iflt();
static double dcd_ifge();
static double dcd_ifle();
static double dcd_ifeq();
static double dcd_ifne();
static double dcd_ran();
static double dcd_ranu();
static double dcd_rang();
static double dcd_ranp();
static void   dcd_evaluate();

/*  definitions/declarations for the function code  */

#define maxfiecode 512

#define bif     2       /* number of floats in a double real                 */
#define bii     2       /* number of integers in a double real               */
#define bis     4       /* number of shorts in a double real                 */
#define bid     8       /* number of bytes in a double real                  */

#define err    -1       /* an error occurred, too bad!                       */
#define hlt     0
#define add     1
#define sub     2
#define mul     3
#define dvd     4
#define neg     5
#define pwr     6
#define ldc     7
#define lst     8
#define fie     9       /* function call (i) has opcode  fie + i             */

#if defined(TESTBED)
static char *mnem[] = {
   "HLT","ADD","SUB","MUL","DIV","NEG","PWR","LDC","LST","FIE"
};
#endif

static union {
   byte opcode[bid];
   double c;
} fiecode[maxfiecode], lstfiecode[maxfiecode];

static int codeptr;
static int opcodeptr;
static int lstcodeptr;
static int lstopcodeptr;

/*  functions we know  */

#define maxfuncts  51
#define maxfunlen  10
#define maxarg      4
#define maxbools    6
#define maxboollen  6

static char *functs[] = {
   "SIN"   , "ASIN"  , "SINH"  , "COS"   , "ACOS"  , "COSH"  ,
   "TAN"   , "ATAN"  , "TANH"  , "ATAN2" , "RAD"   , "DEG"   ,
   "PI"    , "EXP"   , "LN"    , "LOG"   , "SQRT"  , "ABS"   ,
   "SINC"  , "C"     , "G"     , "M"     , "ERF"   , "ERFC"  ,
   "K"     , "H"     , "P"     , "S"     , "MAX"   , "MIN"   ,
   "MOD"   , "INT"   , "NINT"  , "SIGN"  , "BLANK" , "IFGT"  ,
   "IFLT"  , "IFGE"  , "IFLE"  , "IFEQ"  , "IFNE"  , "RANU"  ,
   "RANG"  , "RANP"  , "SIND"  , "ASIND" , "COSD"  , "ACOSD" ,
   "TAND"  , "ATAND" , "ATAND2"
};

static int nargs[]    = {
   1   ,    1   ,    1   ,    1   ,    1   ,    1   ,
   1   ,    1   ,    1   ,    2   ,    1   ,    1   ,
   0   ,    1   ,    1   ,    1   ,    1   ,    1   ,
   1   ,    0   ,    0   ,    0   ,    1   ,    1   ,
   0   ,    0   ,    0   ,    0   ,    2   ,    2   ,
   2   ,    1   ,    1   ,    1   ,    0   ,    4   ,
   4   ,    4   ,    4   ,    4   ,    4   ,    2   ,
   2   ,    1   ,    1   ,    1   ,    1   ,    1   ,
   1   ,    1   ,    2
};

static char *bools[] = {
   "TRUE", "FALSE", "YES", "NO", "JA", "NEE"
};

static bool boolv[] = {
   TRUE, FALSE, TRUE, FALSE, TRUE, FALSE
};

/*  definitions/declarations for the scanner and parser  */

#define end      0
#define plus     1
#define minus    2
#define times    3
#define divide   4
#define constant 5
#define funct    6
#define lpar     7
#define rpar     8
#define comma    9
#define power   10
#define blank   11
#define colon   12
#define lbrac   13
#define rbrac   14

static int    listlen[2];
static int    lists;
static bool   list;
static int    pos, mpos, errorpos, errornum;
static char   ch;
static int    sym;
static int    curfun, npar;
static double curconst;
static int    oddran;
static char   *cptr, *dptr;
static char   ctyp;
static int    ilen, nmax;
static double dval;
static int    nval;

static union {
   byte    b[bid];
   short   s;
   int     i;
   float   f;
   double  d;
} unie;

/* define internal blank value */
static double DCDBLANK;

static union {
   byte   bb[sizeof(double)];
   double dd;
} dcd_blank;

#define MAXFLOAT        1.2e+37
#define MINFLOAT        0.8e-37
#define MAXSHORT        32767.5
#define MINSHORT       -32768.5
#define MAXINT     2147483647.5
#define MININT    -2147483648.5
#define MAXLOG             38.0
#define MINLOG            -38.0

static void dcd_inifblank()
{
   int i = 0;
#if defined(__vms__)
   for (i = 0; i < sizeof(double); i++) dcd_blank.bb[i] = 0xff;
#else
   for (i = 0; i < sizeof(double); i++) dcd_blank.bb[i] = 0x77;
#endif
   DCDBLANK = dcd_blank.dd;
}

static int dcd_round(arg)
double arg;
{
   int val;
   if (arg > 0.0) {
      val = (int) (arg + 0.5);
   } else {
      val = (int) (arg - 0.5);
   }
   return(val);
}

static void dcd_error(errnum)
int errnum;
{
   if (errornum == 0) {
      if (errorpos == 0) errorpos = pos;
      sym = err;
      errornum = errnum;
   }
}

static void dcd_gencode(opc)
int opc;
{
   if (errornum != 0) return;
   if (list) {
      lstfiecode[lstcodeptr].opcode[lstopcodeptr++] = opc;
      if (lstopcodeptr == bid) {
         lstcodeptr++;
         lstopcodeptr = 0;
      }
      if (lstcodeptr == maxfiecode) dcd_error(-18);  /* buffer overflow */
   } else {
      fiecode[codeptr].opcode[opcodeptr++] = opc;
      if (opcodeptr == bid) {
         codeptr++;
         opcodeptr = 0;
      }
      if (codeptr == maxfiecode) dcd_error(-18);     /* buffer overflow */
   }
}

static void dcd_genconst(cst)
double cst;
{
   dcd_gencode(ldc);
   if (errornum != 0) return;
   if (list) {
      if (lstopcodeptr != 0) lstcodeptr++;
      if (lstcodeptr == maxfiecode) {
         dcd_error(-18);
         return;
      }
      lstfiecode[lstcodeptr++].c = cst;
      lstopcodeptr = 0;
   } else {
      if (opcodeptr != 0) codeptr++;
      if (codeptr == maxfiecode) {
         dcd_error(-18);
         return;
      }
      fiecode[codeptr++].c = cst;
      opcodeptr = 0;
   }
}

static void dcd_inilist()
{
   lists = 0;
   list = 0;
}

static void dcd_beginlist()
{
   int l;
   if (errornum != 0) return;
   dcd_gencode(lst);
   lstcodeptr = 0;
   lstopcodeptr = 0;
   list = 1;
   if (lists++ > 0) {
      l = 1;
   } else {
      l = 0;
   }
   listlen[l] = 0;
}

static void dcd_endlist()
{
   int l;
   list = 0;
   if (lists > 1) l = 1; else l = 0;
   if (listlen[l] == 0) dcd_error(-21);          /* empty list */
   if (listlen[0] != listlen[l]) dcd_error(-20); /* unequal list length */
}

static void dcd_putlist()
{
   int l;
   if (errornum != 0) return;
   if (opcodeptr != 0) {
      codeptr++;
      opcodeptr = 0;
   }
   if (codeptr == maxfiecode) {
      dcd_error(-18);
      return;
   }
   fiecode[codeptr++].c = dval;
   if (codeptr == maxfiecode) {
      dcd_error(-18);
      return;
   }
   if (lists > 1) l = 1; else l = 0;
   listlen[l]++;
}

static void dcd_nextch()
{
   if (pos++ < mpos) ch = *cptr++; else ch = '\0';
}

static void dcd_nextsym()
{
   double f, frac;
   int    tenp;
   int    i;
   char   fun[maxfunlen];
   if (sym == err) return;
   if (isdigit(ch)||(ch == '.')) {
      curconst = 0.0;
      while (isdigit(ch)) {
         if (errornum == 0) curconst = 10.0 * curconst + ch - '0';
         if (curconst > MAXFLOAT) dcd_error(-24);        /* floating overflow */
         dcd_nextch();
      };
      if (ch == '.') {
         dcd_nextch();
         f = 1;
         frac = 0;
         while (isdigit(ch)) {
            if (errornum == 0) {
               frac = 10 * frac + (ch - '0');
               f = 10 * f;
            }
            if ((frac > MAXFLOAT)||(f > MAXFLOAT)) dcd_error(-24);
            dcd_nextch();
         };
         if (errornum == 0) curconst = curconst + frac/f;
      };
      if ((ch == 'E')||(ch == 'e')||(ch == 'D')||(ch == 'd')) {
         dcd_nextch();
         tenp = 1;
         frac = 0;
         if (ch == '+') {
            dcd_nextch();
         }
         else if (ch == '-') {
            tenp = -tenp;
            dcd_nextch();
         }
         while (isdigit(ch)) {
            if (errornum == 0) frac = 10 * frac + (ch - '0');
            if (frac > MAXLOG) dcd_error(-24);
            dcd_nextch();
         }
         if (errornum == 0) {
            double sumlog = 0.0;
            if (curconst <= 0.0) sumlog = log10(curconst);
            frac = frac * tenp;
            if ((MINLOG < frac) && (frac < MAXLOG)) {
               sumlog += frac;
               if ((MINLOG < sumlog)&&(sumlog < MAXLOG)) {
                  curconst = curconst * pow(10.0,(double) frac);
               } else dcd_error(-24);
            } else dcd_error(-24);
         }
      }
      sym = constant;
   } else if (isalpha(ch)) {
      i = 0;
      while ((isalpha(ch)||isdigit(ch)) && (i < maxfunlen)) {
         fun[i++] = toupper(ch);
         dcd_nextch();
      };
      fun[i] = 0;
      curfun = 0;
      while ((curfun < maxfuncts) && strcmp(fun,functs[curfun])) {
         curfun++;
      };
      sym = funct;
      if (curfun == maxfuncts) dcd_error(-12);  /* unknown function */
   } else switch(ch) {
      case '\0' : sym = end; dcd_nextch(); break;
      case '+'  : sym = plus; dcd_nextch(); break;
      case '-'  : sym = minus; dcd_nextch(); break;
      case '*'  : {
         sym = times; dcd_nextch();
         if (ch == '*') {
            sym = power; dcd_nextch();
         };
         break;
      }
      case '/'  : sym = divide; dcd_nextch(); break;
      case '('  : {
         sym = lpar;
         do {
            dcd_nextch();
         } while (ch == ' ');
         break;
      }
      case ')'  : sym = rpar; dcd_nextch(); break;
      case ','  : {
         sym = comma;
         do {
            dcd_nextch();
         } while (ch == ' ');
         if (ch == '\0') dcd_error(-13);     /* syntax error */
         break;
      }
      case ' '  : {
         sym = blank;
         while (ch == ' ') dcd_nextch();
         if (ch == ')') {
            sym = rpar;
            break;
         }
         if (ch == ',') {
            sym = comma;
            do {
               dcd_nextch();
            } while (ch == ' ');
            if (ch == '\0') dcd_error(-13);     /* syntax error */
            break;
         }
         if (list&&(ch == ']')) {
            sym = rbrac;
            dcd_nextch();
            break;
         }
         if (ch != ':') break;
      }
      case ':'  : {
         sym = colon;
         do dcd_nextch(); while (ch == ' ');
         if (ch == '[') dcd_error(-22);       /* nested lists */
         break;
      }
      case '['  : {
         sym = lbrac;
         do dcd_nextch(); while (ch == ' ');
         if (list) dcd_error(-22);            /* nested lists */
         break;
      }
      case ']'  : {
         sym = rbrac;
         dcd_nextch();
         if (!list) dcd_error(-13);           /* syntax error */
         break;
      }
      default   : {
         dcd_error(-14);                      /* illegal character */
         dcd_nextch();
         break;
      }
   }
}

static void dcd_nextwr()                   /* put ilen bytes in output buffer */
{
   int i;
   if (nval++ < nmax) {
      for ( i=0 ; i<ilen ; *dptr++ = unie.b[i++]);
   }
}

static void dcd_movenum()                  /* send next item to output buffer */
{
   if (sym == err) return;
   if (list) dcd_putlist(); else {
      if ((ctyp == 'I')&&(ilen == 2)) {
         if (dval == DCDBLANK) dval = 0.0;
         if ((dval > MINSHORT)&&(dval < MAXSHORT)) unie.s = dcd_round(dval);
         else dcd_error(-19);                             /* conversion error */
      } else if ((ctyp == 'I')&&(ilen == 4)) {
         if (dval == DCDBLANK) dval = 0.0;
         if ((dval > MININT)&&(dval < MAXINT)) unie.i = dcd_round(dval);
         else dcd_error(-19);                             /* conversion error */
      } else if ((ctyp == 'F')&&(ilen == 4)) {
         if (dval == DCDBLANK) setfblank_c(&unie.f); else unie.f = (float) dval;
      }
      else if ((ctyp == 'F')&&(ilen == 8)) {
         if (dval == DCDBLANK) setfblank_c(&unie.f); else unie.d = dval;
      }
      if (errorpos == 0) dcd_nextwr();
   }
}

static void dcd_loop()
{
   double a, b, c;
   int    p, q = 0, r;

   if (sym == err) return;
   dcd_expression();
   if (sym == colon) {
      dcd_gencode(hlt);
      dcd_evaluate(q);
      a = dval;
      dcd_nextsym();
      if (sym == colon) {
         dcd_nextsym();
         dcd_expression();
         dcd_gencode(hlt);
         dcd_evaluate(q);
         if ((dval > 0.5)&&(dval < MAXSHORT)) {
            r = dcd_round(dval);
            dval = a;
            for ( p=0; p<r; p++ ) { dcd_movenum();}
         } else dcd_error(-15);               /* wrong repeat/loop argument */
      } else {
         dcd_expression();
         dcd_gencode(hlt);
         dcd_evaluate(q);
         b = dval;
         if (sym == colon) {
            dcd_nextsym();
            dcd_expression();
            dcd_gencode(hlt);
            dcd_evaluate(q);
            c = dval;
         } else { c = 1.0; }
         if (c != 0.0) {
            int    n,i;
            double d;
            d = (b-a)/c;
            if ((d >= 0.0)&&(d < MAXSHORT)) {
               n = (int) (d+0.00001);
               for ( i=0; i<=n; i++) {
                  dval = a+i*c;
                  dcd_movenum();
               }
            } else dcd_error(-15);
/*
         if (c > 0) {
            for ( dval = a; dval <= b; dval = dval + c) dcd_movenum();
         } else if ( c < 0) {
            for ( dval = a; dval >= b; dval = dval + c) dcd_movenum();
*/
         } else dcd_error(-15);               /* wrong repeat/loop argument */
      }
   } else {
      int i;
      dcd_gencode(hlt);
      if ((lists > 0) && (!list)) {
         for ( i=0; i<listlen[0]; i++ ) {
            dcd_evaluate(i);
            dcd_movenum();
         };
      } else {
         dcd_evaluate(q);
         dcd_movenum();
      }
   }
}

static void dcd_expression()
{
   int s;

   if (sym == err) return;
   dcd_term();
   while ((sym == plus) || (sym == minus)) {
      s = sym;
      dcd_nextsym();
      dcd_term();
      if (s == plus) dcd_gencode(add); else dcd_gencode(sub);
   };
}

static void dcd_term()
{
   int s;

   if (sym == err) return;
   dcd_factor();
   while ((sym == times) || (sym == divide)) {
      s = sym;
      dcd_nextsym();
      dcd_factor();
      if (s == times) dcd_gencode(mul); else dcd_gencode(dvd);
   };
}

static void dcd_factor()
{
   if (sym == err) return;
   switch (sym) {
      case lpar  : {
         dcd_nextsym(); dcd_expression();
         if (sym == rpar) dcd_nextsym(); else dcd_error(-13);
         break;
      }
      case minus    : dcd_nextsym(); dcd_factor(); dcd_gencode(neg); break;
      case plus     : dcd_nextsym(); dcd_factor(); break;
      case constant : dcd_genconst(curconst); dcd_nextsym(); break;
      case funct    : dcd_function(); break;
      case lbrac    : {
         dcd_beginlist();
         dcd_list();
         if (sym == rbrac) {
            dcd_endlist();
            dcd_nextsym();
         } else dcd_error(-13);
         break;
      }
      default    : dcd_error(-13); break;
   };
   if (sym == power) {
      dcd_nextsym();
      dcd_factor();
      dcd_gencode(pwr);
   };
}

static void dcd_function()
{
   int f = curfun;
   int n = nargs[curfun];

   if (sym == err) return;
   dcd_nextsym();
   if (n > 0) {
      if (sym == lpar) dcd_nextsym(); else dcd_error(-16);
      while (n > 0) {
         dcd_expression();
         if (--n > 0) {
            if (sym == comma) dcd_nextsym(); else dcd_error(-16);
         };
      };
      if (sym == rpar) dcd_nextsym(); else dcd_error(-16);
   };
   dcd_gencode( fie + f );
}

static void dcd_list()
{
   if (sym == err) return;
   do {
      while (ch == ' ') dcd_nextch();
      dcd_nextsym();
      dcd_loop();
   } while ((sym == blank)||(sym == comma));
}

#if defined(TESTBED)
static void dcd_dump()
{
   int    c, o, opc, op;

   if (sym == err) return;
   c = o = 0;
   do {
      if (list) op = lstfiecode[c].opcode[o++];
      else      op = fiecode[c].opcode[o++];
      if (o == bid) { c++ ; o = 0; };
      opc = op>fie ? fie : op;
      printf("     %s",mnem[opc]);
      if (opc == fie) {
         printf("   %s",functs[op-opc]);
      } else if (opc == ldc) {
         if (o != 0) c++;
         if (list) printf("   %f",lstfiecode[c++].c);
         else      printf("   %f",fiecode[c++].c);
         o = 0;
      } else if (opc == lst) {
         int i;
         if (o != 0) c++;
         printf("   %d",listlen[0]);
         for ( i=0; i<listlen[0]; i++) {
            printf("\n");
            printf("           %f",fiecode[c++].c);
         };
         o = 0;
      };
      printf("\n");
   } while ((opc != hlt)&&(c < maxfiecode));
}
#endif

#define stackmax 20

static double stack[stackmax];
static int sp;

static void dcd_push(r)
double r;
{
   stack[++sp] = r;
}

static double dcd_pop()
{
   return(stack[sp--]);
}

static double dcd_add(arg1,arg2)
double arg1, arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else {
      return(arg1+arg2);
   }
}

static double dcd_sub(arg1,arg2)
double arg1, arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else {
      return(arg1-arg2);
   }
}

static double dcd_mul(arg1,arg2)
double arg1, arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else if ((arg1 == 0.0) || (arg2 == 0.0)) {
      return(0.0);
   } else {
      double intsum = (log10(fabs(arg1))+log10(fabs(arg2)));
      if ((MINLOG < intsum) && (intsum < MAXLOG)) {
         return(arg1*arg2);
      } else {
         dcd_error(-17);
         return(DCDBLANK);
      }
   }
}

static double dcd_div(arg1,arg2)
double arg1, arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else if (arg2 == 0.0) {
      dcd_error(-17);
      return(DCDBLANK);
   } else if (arg1 == 0.0) {
      return(0.0);
   } else {
      double intsum = (log10(fabs(arg2))-log10(fabs(arg1)));
      if ((MINLOG < intsum)&&(intsum < MAXLOG)) {
         return(arg1/arg2);
      } else {
         dcd_error(-17);
         return(DCDBLANK);
      }
   }
}

static double dcd_neg(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
      return(-arg1);
   }
}

static double dcd_pwr(arg1,arg2)
double arg1, arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else if (arg1 >= 0.0) {
      return(pow(arg1,arg2));
   } else {
      int p = arg2, t;
      double epsilon = 0.000001;
      if (fabs(arg2 - p) <= epsilon) {
         t = (p % 2 == 0) ? 1 : -1;
         return(t * pow(fabs(arg1),arg2));
      } else {
         dcd_error(-17);
         return(DCDBLANK);
      }
   }
}

static double dcd_sin(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
      return(sin(arg1));
   }
}

static double dcd_asin(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (fabs(arg1) > 1) {
      dcd_error(-17);
      return(DCDBLANK);
   } else {
      return(asin(arg1));
   }
}

static double dcd_sinh(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (fabs(arg1) > 70) {
      dcd_error(-17);
      return(DCDBLANK);
   } else {
      return(sinh(arg1));
   }
}

static double dcd_cos(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
      return(cos(arg1));
   }
}

static double dcd_acos(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (fabs(arg1) > 1) {
      dcd_error(-17);
      return(DCDBLANK);
   } else {
      return(acos(arg1));
   }
}

static double dcd_cosh(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (fabs(arg1) > 70) {
      dcd_error(-17);
      return(DCDBLANK);
   } else {
      return(cosh(arg1));
   }
}

static double dcd_tan(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
      return(tan(arg1));
   }
}

static double dcd_atan(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
      return(atan(arg1));
   }
}
static double dcd_tanh(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (fabs(arg1) > 70) {
      dcd_error(-17);
      return(DCDBLANK);
   } else {
      return(tanh(arg1));
   }
}

static double dcd_atan2(arg1,arg2)
double arg1, arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else {
      return(atan2(arg1,arg2));
   }
}

static double dcd_rad(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
      return(arg1 * 0.017453292519943295769237);
   }
}

static double dcd_deg(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
      return(arg1 * 57.295779513082320876798155);
   }
}

static double dcd_pi()
{
   double val;
   val = (double) 3.141592653589793238462643;
   return(val);
}

static double dcd_exp(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (fabs(arg1) > 70) {
      dcd_error(-17);
      return(DCDBLANK);
   } else {
      return(exp(arg1));
   }
}

static double dcd_ln(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (arg1 > 0) {
      return(log(arg1));
   } else {
      dcd_error(-17);
      return(DCDBLANK);
   }
}

static double dcd_log(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (arg1 > 0) {
      return(log10(arg1));
   } else {
      dcd_error(-17);
      return(DCDBLANK);
   }
}

static double dcd_sqrt(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (arg1 < 0) {
      dcd_error(-17);
      return(DCDBLANK);
   } else {
      return(sqrt(arg1));
   }
}

static double dcd_abs(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
      return(fabs(arg1));
   }
}

static double dcd_sinc(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (fabs(arg1) < 1.0e-30) {
      return(1.0);
   } else {
      return(sin(arg1)/arg1);
   }
}

static double dcd_max(arg1,arg2)
double arg1,arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else if (arg1 > arg2) {
      return(arg1);
   } else {
      return(arg2);
   }
}

static double dcd_min(arg1,arg2)
double arg1,arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else if (arg1 < arg2) {
      return(arg1);
   } else {
      return(arg2);
   }
}

static double dcd_erf(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
      double p  =  0.327591100;
      double a1 =  0.254829592;
      double a2 = -0.284496736;
      double a3 =  1.421413741;
      double a4 = -1.453152027;
      double a5 =  1.061405429;
      double t1 = 1.0 / ( 1.0 + p * fabs(arg1));
      double t2 = t1*t1, t3 = t1*t2, t4 = t1*t3, t5 = t4*t1;
      if (arg1 > 0.0) {
         return(1.0 - (a1*t1+a2*t2+a3*t3+a4*t4+a5*t5)*exp(-arg1*arg1));
      } else {
         return((a1*t1+a2*t2+a3*t3+a4*t4+a5*t5)*exp(-arg1*arg1) - 1.0);
      }
   }
}

static double dcd_erfc(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
      return(1.0 - dcd_erf(arg1));
   }
}

static double dcd_mod(arg1,arg2)
double arg1,arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else if (arg1 == 0.0) {
      dcd_error(-17);
      return(DCDBLANK);
   } else {
     int   xxx = arg1/arg2;
     return(arg1 - xxx * arg2);
   }
}

static double dcd_int(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
     int xxx = arg1;  /* this could be dangerous */
     return((double) xxx);
   }
}

static double dcd_nint(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else {
     int xxx = (arg1 + 0.5);  /* this could be dangerous */
     return((double) xxx);
   }
}

static double dcd_sign(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (arg1 == 0.0) {
      return(0.0);
   } else if (arg1 > 0.0) {
      return (1.0);
   } else {
      return(-1.0);
   }
}

static double dcd_ifgt(arg1,arg2,arg3,arg4)
double arg1, arg2, arg3, arg4;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else if (arg1 > arg2) {
      return(arg3);
   } else {
      return(arg4);
   }
}

static double dcd_iflt(arg1,arg2,arg3,arg4)
double arg1, arg2, arg3, arg4;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else if (arg1 < arg2) {
      return(arg3);
   } else {
      return(arg4);
   }
}

static double dcd_ifge(arg1,arg2,arg3,arg4)
double arg1, arg2, arg3, arg4;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else if (arg1 >= arg2) {
      return(arg3);
   } else {
      return(arg4);
   }
}

static double dcd_ifle(arg1,arg2,arg3,arg4)
double arg1, arg2, arg3, arg4;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else if (arg1 <= arg2) {
      return(arg3);
   } else {
      return(arg4);
   }
}

static double dcd_ifeq(arg1,arg2,arg3,arg4)
double arg1, arg2, arg3, arg4;
{
   if (arg1 == arg2) {
      return(arg3);
   } else {
      return(arg4);
   }
}

static double dcd_ifne(arg1,arg2,arg3,arg4)
double arg1, arg2, arg3, arg4;
{
   if (arg1 != arg2) {
      return(arg3);
   } else {
      return(arg4);
   }
}

static double dcd_ran()
{
   double xxx = rand();
   return((xxx+1.0) / ((double) RAND_MAX + 1.0));
}

static double dcd_ranu(arg1,arg2)
double arg1,arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else {
      return(arg1 + dcd_ran() * (arg2 - arg1));
   }
}

static double dcd_rang(arg1,arg2)
double arg1,arg2;
{
   if ((arg1 == DCDBLANK) || (arg2 == DCDBLANK)) {
      return(DCDBLANK);
   } else {
     double val, r1, r2;
     r1 = dcd_ran();
     r2 = dcd_ran();
     if (oddran == 0) {
        val = sqrt(-2*log(r1))*cos(6.283185307179586476925286*r2); oddran = 1;
     } else {
        val = sqrt(-2*log(r1))*cos(6.283185307179586476925286*r2); oddran = 0;
     }
     val = arg1 + fabs(arg2) * val;
     return(val);
   }
}

static double dcd_ranp(arg1)
double arg1;
{
   if (arg1 == DCDBLANK) {
      return(DCDBLANK);
   } else if (arg1 < 0) {
      dcd_error(-17);
      return(DCDBLANK);
   } else {
      double val, cum, p, f;
      if (arg1 < 40) {
         int xxx = dcd_rang(arg1,sqrt(arg1))+0.5;
         val = xxx;
      } else {
         cum = exp(-arg1);
         p = cum;
         val = 0.0;
         f = dcd_ran();
         while ( f >= cum) {
            val = val + 1.0;
            p = p * arg1 / val;
            cum = cum + p;
         }
      }
     return(val);
   }
}

static void dcd_evaluate(q)
int q;
{
   int c, o, opc;
   double arg[maxarg];

   if (sym == err) return;
   c = o = sp = 0;
   do {
      if (list) {
         opc = lstfiecode[c].opcode[o++];
      } else {
         opc = fiecode[c].opcode[o++];
      }
      if (o == bid) { c++ ; o = 0; };
      if (opc >= fie) {
         int narg = nargs[opc-fie], n;
         for (n = 1 ; n<=narg ; n++ ) arg[narg-n] = dcd_pop();
      };
      switch (opc) {
         case hlt: break;
/* add */case add: {
            double a2 = dcd_pop();
            double a1 = dcd_pop();
            dcd_push(dcd_add(a1,a2));
            break;
         }
/* sub */case sub: {
            double a2 = dcd_pop();
            double a1 = dcd_pop();
            dcd_push(dcd_sub(a1,a2));
            break;
         }
/* mul */case mul: {
            double a2 = dcd_pop();
            double a1 = dcd_pop();
            dcd_push(dcd_mul(a1,a2));
            break;
         }
/* div */case dvd: {
            double a2 = dcd_pop();
            double a1 = dcd_pop();
            dcd_push(dcd_div(a1,a2));
            break;
         }
/* neg */case neg: dcd_push(dcd_neg(dcd_pop())); break;
/* pow */case pwr: {
            double a2 = dcd_pop();
            double a1 = dcd_pop();
            dcd_push(dcd_pwr(a1,a2));
            break;
         }
         case ldc: {
            if (o != 0) c++; if (list) {
               dcd_push(lstfiecode[c++].c);
            } else {
               dcd_push(fiecode[c++].c);
            }
            o = 0; break;
         }
         case lst: {
            if (o != 0) c++; o = 0; if (list) {
               dcd_push(lstfiecode[q+c].c);
            } else {
               dcd_push(fiecode[q+c].c);
            }
            c = c + listlen[0]; break;
         }
         default:  switch(opc-fie) {
/* sin   */ case  0: dcd_push(dcd_sin(arg[0])); break;
/* asin  */ case  1: dcd_push(dcd_asin(arg[0])); break;
/* sinh  */ case  2: dcd_push(dcd_sinh(arg[0])); break;
/* cos   */ case  3: dcd_push(dcd_cos(arg[0])); break;
/* acos  */ case  4: dcd_push(dcd_acos(arg[0])); break;
/* cosh  */ case  5: dcd_push(dcd_cosh(arg[0])); break;
/* tan   */ case  6: dcd_push(dcd_tan(arg[0])); break;
/* atan  */ case  7: dcd_push(dcd_atan(arg[0])); break;
/* tanh  */ case  8: dcd_push(dcd_tanh(arg[0])); break;
/* atan2 */ case  9: dcd_push(dcd_atan2(arg[0],arg[1])); break;
/* rad   */ case 10: dcd_push(dcd_rad(arg[0])); break;
/* deg   */ case 11: dcd_push(dcd_deg(arg[0])); break;
/* pi    */ case 12: dcd_push(dcd_pi()); break;
/* exp   */ case 13: dcd_push(dcd_exp(arg[0])); break;
/* ln    */ case 14: dcd_push(dcd_ln(arg[0])); break;
/* log   */ case 15: dcd_push(dcd_log(arg[0])); break;
/* sqrt  */ case 16: dcd_push(dcd_sqrt(arg[0])); break;
/* abs   */ case 17: dcd_push(dcd_abs(arg[0])); break;
/* sinc  */ case 18: dcd_push(dcd_sinc(arg[0])); break;
/* c     */ case 19: dcd_push( 2.997925e+8 ); break;
/* g     */ case 20: dcd_push( 6.6732e-11 ); break;
/* m     */ case 21: dcd_push( 1.99e30 ); break;
/* erf   */ case 22: dcd_push(dcd_erf(arg[0])); break;
/* erfc  */ case 23: dcd_push(dcd_erfc(arg[0])); break;
/* k     */ case 24: dcd_push( 1.380622e-23 ); break;
/* h     */ case 25: dcd_push( 6.6256196e-34 ); break;
/* p     */ case 26: dcd_push( 3.086e16 ); break;
/* s     */ case 27: dcd_push( 5.66961e-8 ); break;
/* max   */ case 28: dcd_push(dcd_max(arg[0],arg[1])); break;
/* min   */ case 29: dcd_push(dcd_min(arg[0],arg[1])); break;
/* mod   */ case 30: dcd_push(dcd_mod(arg[0],arg[1])); break;
/* int   */ case 31: dcd_push(dcd_int(arg[0])); break;
/* nint  */ case 32: dcd_push(dcd_nint(arg[0])); break;
/* sign  */ case 33: dcd_push(dcd_sign(arg[0])); break;
/* undef */ case 34: dcd_push(DCDBLANK); break;
/* ifgt  */ case 35: dcd_push(dcd_ifgt(arg[0],arg[1],arg[2],arg[3])); break;
/* iflt  */ case 36: dcd_push(dcd_iflt(arg[0],arg[1],arg[2],arg[3])); break;
/* ifge  */ case 37: dcd_push(dcd_ifge(arg[0],arg[1],arg[2],arg[3])); break;
/* ifle  */ case 38: dcd_push(dcd_ifle(arg[0],arg[1],arg[2],arg[3])); break;
/* ifeq  */ case 39: dcd_push(dcd_ifeq(arg[0],arg[1],arg[2],arg[3])); break;
/* ifne  */ case 40: dcd_push(dcd_ifne(arg[0],arg[1],arg[2],arg[3])); break;
/* ranu  */ case 41: dcd_push(dcd_ranu(arg[0],arg[1])); break;
/* rang  */ case 42: dcd_push(dcd_rang(arg[0],arg[1])); break;
/* ranp  */ case 43: dcd_push(dcd_ranp(arg[0])); break;
/* sind  */ case 44: dcd_push(sin(dcd_rad(arg[0]))); break;
/* asind */ case 45: dcd_push(dcd_deg(dcd_asin(arg[0]))); break;
/* cosd  */ case 46: dcd_push(cos(dcd_rad(arg[0]))); break;
/* acosd */ case 47: dcd_push(dcd_deg(dcd_acos(arg[0]))); break;
/* tand  */ case 48: dcd_push(tan(dcd_rad(arg[0]))); break;
/* atand */ case 49: dcd_push(dcd_deg(dcd_atan(arg[0]))); break;
/* atand2*/ case 50: dcd_push(dcd_deg(dcd_atan2(arg[0],arg[1]))); break;
            default: opc = err; break;
         }; break;
      };
   } while ((opc != hlt) && (opc != err) && (errornum == 0));
   if (opc == err) {
      dcd_error(-17);
   }
   if (errornum != 0) {
      dval = DCDBLANK;
   } else {
      dval = dcd_pop();
   }
   if (list) {
      lstcodeptr = 0;
      lstopcodeptr = 0;
   } else {
      codeptr = 0;
      opcodeptr = 0;
   }
}

/*
#>            herinp.dc2

Function:     HERINP

Purpose:      Decodes a string of characters into reals, integers,
              logicals, characters or bytes.

Category:     MATH, UTILITY, USER-INTERFACE

File:         herinp.c

Author:       K.G. Begeman

Use:          CALL HERINP( EXPR ,    Input   CHARACTER*(*)
                           TYPE ,    Input   CHARACTER*(*)
                           LENT ,    Input   INTEGER
                           OUTV ,    Output  ARRAY of BYTES
                           NOUT ,    Input   INTEGER
                           NRET ,    Output  INTEGER
                           IERH )    Output  INTEGER

              EXPR  Character string containing the expressions to be
                    evaluated.
              TYPE  Character denoting type of conversion wanted. See
                    the notes for a list of possible types.
              LENT  Length of type in bytes. See the notes for a list
                    of possibilities.
              OUTV  Array of bytes containing the decoded values.
              NOUT  Maximum number of values of type TYPE to be
                    returned.
              NRET  Number of values actual decoded.
              IERH  Error return code. A list of error codes is given
                    in the notes.

Notes:        1. The syntax of the input for this routine is described
                 in document INPUT.DC2
              2. List of possible types and allowed lengths:
                 TYPE         LENT     type
                  'F'         4,8      reals
                  'I'          2,4      integers
                  'L'         1,2,4    logicals
                  'A'         any      plain bytes
                  'C'         any      bytes with separator recognition
              3. List of error returns:
                 IERH  explanation
                   0   no error
                 -11   bad call
                 -12   unknown function
                 -13   syntax error
                 -14   illegal character
                 -15   wrong repeat argument
                 -16   wrong number of arguments
                 -17   arithmetic error
                 -18   not enough internal memory
                 -19   conversion error
                 -20   unequal list length
                 -21   empty list
                 -22   nested lists
                 -23   output buffer overflow (not for 'A' type of input)
                 -24   floating overflow/underflow in conversion

Updates:      Dec 16, 1987: KGB, Type C implemented .
              Jan 25, 1988: KGB, Document created.
              Feb 16, 1988: KGB, In GENLIB.
              Mar 23, 1988: KGB, Added argument NCHR.
              Apr 11, 1988: KGB, Disabled check of buffer overflow
                                 for type 'A'
              Apr 14, 1988: KGB, Error if no value after a ','
              May  5, 1988: KGB, Small but important change in
                          document
              Oct 13, 1988: KGB, Implemented ANSI F77 calls to DCDCHAR,
                                 DCDINT, DCDLOG, DCDREAL, DCDDBLE
              Nov 23, 1988: KGB, Handles now BLANKS
              Dec  5, 1988: KGB, Bug in UNIX version removed
              Mar  1, 1989: KGB, Bug in decoding logicals removed
              Jul 27, 1989: KGB, FtoC interface implemented.
              Jul 22, 1994: JPT, Call Hermes for decoding numbers.

#<

@ subroutine herinp( character, character, integer, integer,
@                    integer, integer, integer )

*/

void herinp_c( fchar  expr  ,
               fchar  type  ,
               fint  *length,
               char  *outv  ,
               fint  *nout  ,
               fint  *nret  ,
               fint  *ierd  )
{
   int    i;
   cptr = expr.a;
   mpos = expr.l;
   dptr = outv;
   ilen = *length;
   nmax = *nout;
   ctyp = toupper(type.a[0]);
   nval = 0;
   oddran = 0;
   errornum = 0;
   errorpos = 0;
   pos = 0;
   npar = 0;
   codeptr = 0;
   opcodeptr = 0;
   ch = ' ';
   dcd_inifblank();
   switch(ctyp) {
      case 'A': {
         dcd_nextch();
         if (ch == '\0') break;
         do {
            for ( i=0 ; i<ilen; i++ ) {
               *dptr++ = ch;
               dcd_nextch();
            };
            nval++;
         } while ((ch != '\0') && (nval < nmax));
/*       if ((nval == nmax) && (ch != '\0')) dcd_error(-23);  check disabled */
         /* fill with end-of-string */
         if (errornum == 0) {
            int n;
            for ( n = nval; n < nmax; n++) {
               for ( i = 0; i < ilen; i++) *dptr++ = '\0';
            }
         }
         break;
      }
      case 'C': {
         dcd_nextch();
         if (ch == '\0') break;
         do {
            while (ch == ' ') dcd_nextch();
            if (ch == ',') dcd_error(-13);
            else {
               for ( i=0 ; i<ilen; i++ ) {
                  if ((ch == '\0') || (ch == ',') || (ch == ' ')) {
                     *dptr++ = ' ';
                  } else {
                     *dptr++ = ch;
                     dcd_nextch();
                  }
               }
               if ((ch != '\0') && (ch != ',') && (ch != ' ')) dcd_error(-13);
               else {
                  nval++;
                  while (ch == ' ') dcd_nextch();
                  if (ch == ',') {
                     do {
                        dcd_nextch();
                     } while (ch == ' ');
                     if (ch == '\0') dcd_error(-13);
                  }
               }
           };
         } while ((ch != '\0') && (errornum == 0) && (nval < nmax));
         if ((nval == nmax) && ( ch != '\0')) dcd_error(-23);
         /* fill with end-of-string */
         if (errornum == 0) {
            int n;
            for ( n = nval; n < nmax; n++) {
               for ( i = 0; i < ilen; i++) {*dptr++ = ' ';}
            }
         }
         break;
      }
      case 'L': {
         char logc[maxboollen];
         int  i, curlog;
         dcd_nextch();
         if (ch == '\0') break;
         do {
            while (ch == ' ') dcd_nextch();
            if (isalpha(ch)) {
               i = 0;
               while ((isalpha(ch)) && (i < maxboollen)) {
                  logc[i++] = toupper(ch);
                  dcd_nextch();
               };
               curlog = 0;
               while (( curlog < maxbools) && strncmp(logc,bools[curlog],i)) {
                  curlog++;
               };
               if (curlog == maxbools) dcd_error(-13); /* syntax error */
               else {
                  switch(ilen) {
                     case 1 : unie.b[0] = boolv[curlog]; break;
                     case 2 : unie.s = boolv[curlog]; break;
                     case 4 : unie.i = boolv[curlog]; break;
                     default: break;
                  };
                  dcd_nextwr();
                  while (ch == ' ') dcd_nextch();
                  if (ch == ',') {
                     do {
                        dcd_nextch();
                     } while (ch == ' ');
                     if (ch == '\0') dcd_error(-13);
                  }
               }
            } else dcd_error(-13);
         } while ((ch != '\0') && (errornum == 0) && (nval < nmax));
         if ((nval == nmax) && (ch != '\0')) dcd_error(-23);
         break;
      }
      case 'I':
      case 'F': {
         dcd_nextch();
         if (ch == '\0') break;
         sym = blank;
         do {
            dcd_inilist();
            while (ch == ' ') dcd_nextch();
            if (ch != '\0') {
               dcd_nextsym();
               dcd_loop();
            } else sym = end;
         } while ((errornum == 0)&&((sym == blank)||(sym == comma)));
         if ((errornum == 0) && (ch != '\0')) dcd_error(-13);
         if ((errornum == 0) && (nval > nmax)) dcd_error(-23);
         break;
      }
      default : errornum = -11;                  /* bad call */
   }
   *nret = nval;
   *ierd = errornum;
}

/*
#>            dcdint.dc2

Function:     DCDINT

Purpose:      Decodes a string of characters into integers.

Category:     MATH, UTILITY, USER-INTERFACE

File:         herinp.c

Author:       K.G. Begeman

Use:          INTEGER DCDINT( EXPR,     Input    CHARACTER*(*)
                              OUTV,     Output   INTEGER ARRAY
                              NOUT,     Input    INTEGER
                              IERH )    Input    INTEGER

              DCDINT Returns number of integers decoded.
              EXPR   Character string containing the expressions to
                     be evaluated.
              OUTV   Array of integers containing the decoded values.
              NOUT   Dimension of OUTV.
              IERH   Error return code. A list of error codes is given
                     in HERINP.DC2.

Notes:        1. The syntax of the input for this routine is described
                 in document INPUT.DC2

Updates:      Oct 13, 1988: KGB, Document created.
              Jul 25, 1994: JPT, Delegate decoding to decodeint_c for
                                 GIPSY tasks.

#<

@ integer function dcdint( character, integer, integer, integer )

*/

fint dcdint_c( fchar expr, fint *outv, fint *nout, fint *ierd )
{
   static fint nret, length = sizeof(int);
   static fchar type = { "I", 1 };
   if (gipsytask_c()) {
      nret = decodeint_c(expr, outv, nout);
      if (nret<0) *ierd = nret; else *ierd = 0;
   } else herinp_c(expr,type,&length,(char *) outv,nout,&nret,ierd);
   return(nret);
}

/*
#>            dcdlog.dc2

Function:     DCDLOG

Purpose:      Decodes a string of characters into logicals.

Category:     UTILITY, USER-INTERFACE

File:         herinp.c

Author:       K.G. Begeman

Use:          INTEGER DCDLOG( EXPR,     Input    CHARACTER*(*)
                              OUTV,     Output   LOGICAL ARRAY
                              NOUT,     Input    INTEGER
                              IERH )    Input    INTEGER

              DCDLOG Returns number of logicals decoded.
              EXPR   Character string containing the expressions to
                     be evaluated.
              OUTV   Array of logicals containing the decoded values.
              NOUT   Dimension of OUTV.
              IERH   Error return code. A list of error codes is given
                     in HERINP.DC2.

Notes:        1. The syntax of the input for this routine is described
                 in document INPUT.DC2

Updates:      Oct 13, 1988: KGB, Document created.

#<

@ integer function dcdlog( character, logical, integer integer )

*/

fint dcdlog_c( fchar expr, fint *outv, fint *nout, fint *ierd )
{
   static fint nret, length = sizeof(bool);
   static fchar type = { "L", 1 };
   herinp_c(expr,type,&length,(char *)outv,nout,&nret,ierd);
   return(nret);
}

/*
#>            dcdreal.dc2

Function:     DCDREAL

Purpose:      Decodes a string of characters into reals.

Category:     MATH, UTILITY, USER-INTERFACE

File:         herinp.c

Author:       K.G. Begeman

Use:          INTEGER DCDREAL( EXPR,     Input    CHARACTER*(*)
                               OUTV,     Output   REAL ARRAY
                               NOUT,     Input    INTEGER
                               IERH )    Input    INTEGER

              DCDREAL Returns number of reals actually decoded.
              EXPR    Character string containing the expressions to
                      be evaluated.
              OUTV    Array of reals containing the decoded values.
              NOUT    Dimension of OUTV.
              IERH    Error return code. A list of error codes is given
                      in HERINP.DC2.

Notes:        1. The syntax of the input for this routine is described
                 in document INPUT.DC2

Updates:      Oct 13, 1988: KGB, Document created.
              Jul 25, 1994: JPT, Delegate decoding to decodereal_c for
                                 GIPSY tasks.

#<

@ integer function dcdreal( character, real, integer integer )

*/

fint dcdreal_c( fchar expr, fint *outv, fint *nout, fint *ierd )
{
   static fint nret, length = sizeof(float);
   static fchar type = { "F", 1 };
   if (gipsytask_c()) {
      nret = decodereal_c(expr, (float*)outv, nout);
      if (nret<0) *ierd = nret; else *ierd = 0;
   } else herinp_c(expr,type,&length,(char *)outv,nout,&nret,ierd);
   return(nret);
}

/*
#>            dcddble.dc2

Function:     DCDDBLE

Purpose:      Decodes a string of characters into doubles.

Category:     MATH, UTILITY, USER-INTERFACE

File:         herinp.c

Author:       K.G. Begeman

Use:          INTEGER DCDDBLE( EXPR,     Input    CHARACTER*(*)
                               OUTV,     Output   DOUBLE PRESICION ARRAY
                               NOUT,     Input    INTEGER
                               IERH )    Input    INTEGER

              DCDDBLE Returns number of doubles actually decoded.
              EXPR    Character string containing the expressions to
                      be evaluated.
              OUTV    Array of doubles containing the decoded values.
              NOUT    Dimension of OUTV.
              IERH    Error return code. A list of error codes is given
                      in HERINP.DC2.

Notes:        1. The syntax of the input for this routine is described
                 in document INPUT.DC2

Updates:      Oct 13, 1988: KGB, Document created.
              Jul 25, 1994: JPT, Delegate decoding to decodedble_c for
                                 GIPSY tasks.

#<

@ integer function dcddble( character, double precision, integer integer )

*/

fint dcddble_c( fchar expr, fint *outv, fint *nout, fint *ierd )
{
   static fint nret, length = sizeof(double);
   static fchar type = { "F", 1 };
   if (gipsytask_c()) {
      nret = decodedble_c(expr, (double*)outv, nout);
      if (nret<0) *ierd = nret; else *ierd = 0;
   } else herinp_c(expr,type,&length,(char *)outv,nout,&nret,ierd);
   return(nret);
}

/*
#>            dcdchar.dc2

Function:     DCDCHAR

Purpose:      Decodes a string of characters into character strings.

Category:     UTILITY, USER-INTERFACE

File:         herinp.c

Author:       K.G. Begeman

Use:          INTEGER DCDCHAR( EXPR,     Input   CHARACTER*(*)
                               OUTV,     Output  CHARACTER*(*) ARRAY
                               NOUT,     Input   INTEGER
                               IERH )    Output  INTEGER

              DCDCHAR Returns number of character strings actually
                      decoded.
              EXPR    Character string containing the expressions to
                      be evaluated.
              OUTV    Array of character strings containing the
                      decoded values.
              NOUT    Dimension of OUTV.
              IERH    Error return code. A list of error is given
                      in HERINP.DC2.

Notes:        1. The syntax of the input for this routine is described
                 in document INPUT.DC2

Updates:      Oct 13, 1988: KGB, Document created.

#<

@ integer function dcdchar( character, character, integer, integer )

*/

fint dcdchar_c( fchar expr, fchar outv, fint *nout, fint *ierd )
{
   static fint nret, length;
   static fchar type = { "C", 1 };
   length = outv.l;
   herinp_c(expr,type,&length,outv.a,nout,&nret,ierd);
   return(nret);
}

/*
#>            dcdcharu.dc2

Function:     DCDCHARU

Purpose:      Decodes a string of characters into character strings.
              All alphabetic characters will be in uppercase.

Category:     UTILITY, USER-INTERFACE

File:         herinp.c

Author:       K.G. Begeman

Use:          INTEGER DCDCHARU( EXPR,    Input   CHARACTER*(*)
                                OUTV,    Output  CHARACTER*(*) ARRAY
                                NOUT,    Input   INTEGER
                                IERH )   Output  INTEGER

              DCDCHARU Returns number of character strings actually
                       decoded.
              EXPR     Character string containing the expressions to
                       be evaluated.
              OUTV     Array of character strings containing the
                       decoded values.
              NOUT     Dimension of OUTV.
              IERH     Error return code. A list of error is given
                       in HERINP.DC2.

Notes:        1. The syntax of the input for this routine is described
                 in document INPUT.DC2

Updates:      Jul 31, 1989: KGB, Document created.
              May  8, 1992: FXV, Bug removed.

#<

@ integer function dcdcharu( character, character, integer, integer )

*/

fint dcdcharu_c( fchar expr, fchar outv, fint *nout, fint *ierd )
{
   static fint n, nret, length;
   static fchar type = { "C", 1 };
   length = outv.l;
   herinp_c(expr,type,&length,outv.a,nout,&nret,ierd);
   length *= nret;

   for (n = 0; n < length; n++) {
     outv.a[n] = toupper(outv.a[n]);
   }
   return(nret);
}


/*
#>            dcdcharl.dc2

Function:     DCDCHARL

Purpose:      Decodes a string of characters into character strings.
              All alphabetic characters will be in lowercase.

Category:     UTILITY, USER-INTERFACE

File:         herinp.c

Author:       K.G. Begeman

Use:          INTEGER DCDCHARL( EXPR,    Input   CHARACTER*(*)
                                OUTV,    Output  CHARACTER*(*) ARRAY
                                NOUT,    Input   INTEGER
                                IERH )   Output  INTEGER

              DCDCHARL Returns number of character strings actually
                       decoded.
              EXPR     Character string containing the expressions to
                       be evaluated.
              OUTV     Array of character strings containing the
                       decoded values.
              NOUT     Dimension of OUTV.
              IERH     Error return code. A list of error is given
                       in HERINP.DC2.

Notes:        1. The syntax of the input for this routine is described
                 in document INPUT.DC2

Updates:      Jul 31, 1989: KGB, Document created.
              May  8, 1992: FXV, Bug removed.

#<

@ integer function dcdcharl( character, character, integer, integer )

*/

fint dcdcharl_c( expr, outv, nout, ierd )
fchar expr, outv;
fint  *nout, *ierd;
{
   static fint n, nret, length;
   static fchar type = { "C", 1 };
   length = outv.l;
   herinp_c(expr,type,&length,outv.a,nout,&nret,ierd);
   length *= nret;

   for (n = 0; n < length; n++) {
     outv.a[n] = tolower(outv.a[n]);
   }
   return(nret);
}

#if defined(TESTBED)
main( argc, argv )
char *argv[];
int   argc;
{
   double outv[256];
   char   expr[256];
   int    carg;
   int    nout = 256, nret, ierd;
   fchar  ex;
   strcpy(expr,"");
   for (carg = 1; carg < argc; carg++) {
      strcat(expr,argv[carg]);
      strcat(expr," ");
   }
   ex.a = expr; ex.l = strlen(expr);
   if (ex.l == 0) {
      printf("Usage: %s <expression> [<expression> [ ... ] ]\n", argv[0] );
      exit(0);
   }
   nret = dcddble_c(ex,outv,&nout,&ierd);
   switch(ierd) {
      case -11: printf("bad call\n"); break;
      case -12: printf("unknown function\n"); break;
      case -13: printf("syntax error\n"); break;
      case -14: printf("illegal character\n"); break;
      case -15: printf("wrong repeat argument\n"); break;
      case -16: printf("wrong number of arguments\n"); break;
      case -17: printf("arithmetic error\n"); break;
      case -18: printf("not enough internal memory\n"); break;
      case -19: printf("conversion error\n"); break;
      case -20: printf("unequal list length\n"); break;
      case -21: printf("empty list\n"); break;
      case -22: printf("nested lists\n"); break;
      case -23: printf("output buffer overflow\n"); break;
      case -24: printf("floating overflow/underflow in conversion\n"); break;
      default: {
         int n;
         int p;
         for (n = 0; n < nret; n++) {
            if (fblank_c(&outv[n])) printf("BLANK\n"); else {
               if (outv[n] != 0.0) {
                  p = log10(fabs(outv[n]));
                  outv[n] = outv[n]*pow(10.0,(double) -p);
                  if (p > 9) printf("  %.10fE+%d\n",outv[n],p);
                  else if (p >=0) printf("  %.10fE+0%d\n",outv[n],p);
                  else if (p > -9) printf("  %.10fE-0%d\n",outv[n],-p);
                  else printf("  %.10fE-%d\n",outv[n],-p);
               } else printf("  0.0000000000E+00\n");
            }
         }
      }
      break;
   }
}
#endif
