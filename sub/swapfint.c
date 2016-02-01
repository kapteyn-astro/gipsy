/* swapfint.c
#> swapfint.dc3
Function:   swapfint

Purpose:    Swap the bytes of a fint

Category:   SYSTEM

File:       swapfint.c

Author:     J.P. Terlouw

Use:        fint swapfint(fint arg)
 
            swapfint   returns the arg, with its bytes reversed in order.

Updates:    Mar  9, 1994: JPT, Document created
#<
#> swapfint8.dc3
Function:   swapfint8

Purpose:    Swap the bytes of a fint8

Category:   SYSTEM

File:       swapfint8.c

Author:     J.P. Terlouw

Use:        fint8 swapfint8(fint8 arg)
 
            swapfint8   returns the arg, with its bytes reversed in order.

Updates:    Mar  9, 1994: JPT, Document created
#<
#> swapfint.h
#if !defined(_swapfint_h_)
#define _swapfint_h_
#include "gipsyc.h"
extern fint swapfint(fint arg);
extern fint8 swapfint8(fint8 arg);
#endif
#<
*/

#include "swapfint.h"
extern fint swapfint(fint arg)
{
   int i,j;
   
   union {
      fint l;
      char b[sizeof(fint)];
   } in, out;
    
   in.l = arg;

   i=0;
   j=sizeof(fint);
   do out.b[--j] = in.b[i++]; while(j);
   return out.l;
}
extern fint8 swapfint8(fint8 arg)
{
   int i,j;
   
   union {
      fint8 l;
      char b[sizeof(fint8)];
   } in, out;
    
   in.l = arg;

   i=0;
   j=sizeof(fint8);
   do out.b[--j] = in.b[i++]; while(j);
   return out.l;
}
