#include "gipsyc.h"
/*
@integer function nbits(integer)
*/
fint nbits_c(fint *bits)
{
/*
#if (sizeof(fint) != 4)
#error This function can only take 32-bit arguments
#endif
*/

/* 
#>nbits.dc3
Name:        NBITS

Purpose:     count the number of bits in a 32-bit word which are set

Author:      J.P. Terlouw

Use:         NBITS ( word )                [ INTEGER function]

               word  =  32-bit value

Warning:     A routine with the same name and a similar function exists 
             for the VAX. That routine currently takes a 16-bit argument.
             
Updates:     15-Sep-89  original document

#<
*/
   unsigned int word;

   word = (unsigned int)(*bits);

   word = ((word & 0xAAAAAAAA)>> 1) + (word & 0x55555555);
   word = ((word & 0xCCCCCCCC)>> 2) + (word & 0x33333333);
   word = ((word & 0xF0F0F0F0)>> 4) + (word & 0x0F0F0F0F);
   word = ((word & 0xFF00FF00)>> 8) + (word & 0x00FF00FF);
   word = ((word & 0xFFFF0000)>>16) + (word & 0x0000FFFF);

   return (fint)word;
}
