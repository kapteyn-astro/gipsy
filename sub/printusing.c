/* printusing.c

        Copyright (c) Kapteyn Laboratorium Groningen 1990
        All Rights Reserved.

#>            printusing.dc2

Function:     PRINTUSING

Purpose:      Print numbers using a format image

File:         printusing.c

Author:       M. Vogelaar

Use:          INTEGER PRINTUSING( FORMAT  ,  input      CHARACTER ARRAY
                                  NUMBER  ,  input      DOUBLE PRECISION
                                  RESULT )   output     CHARACTER ARRAY

              PRINTUSING Field width in format
              FORMAT     String containing format image
              NUMBER     Number to be formatted
              RESULT     String containing formatted number 


Description:  PRINTUSING is used to print double precision numbers
              in an user specified format. Input is a format image 
              'FORMAT' consisting of characters representing the   
              wanted output format for 'NUMBER'. The formatted number 
              is returned in 'RESULT'.
              The syntax is for 'FORMAT'is:
 
 flag(s)      Zero or more flags, in any order, which modify the
              meaning of the conversion specification.  The flag
              characters and their meanings are:

      -       The result of the conversion is left-
              justified within the field.

      +       The result of a signed conversion always
              begins with a sign, "+" or "-".
                                                                            
 string       Characters, some with special meaning. 
              If the string (f.i. FFFFF.FF or gggg.gg or wwwww)
              contains no dot, the number of characters specify
              a minimum field width.  For an output field, if the
              converted value has fewer characters than the field
              width, it is padded on the left (or right, if the
              left-adjustment flag, - has been given) to the field
              width.
              If the string contains a dot, the total number of 
              characters including the dot is the minimum field width
              and the number of characters after the dot is the 
              precision.  

              The characters are used to determine the conversion
              type. If the string contains an:

              'e' or 'E' 
                      The floating-point-number argument is
                      printed in the style [-]drddde+dd,
                      where there is one digit before the
                      radix character, and the number of
                      digits after it is equal to the
                      precision. The E conversion character
                      produces a number with E introducing
                      the exponent instead of e. The
                      exponent always contains at least two 
                      digits.  However, if the value to be
                      printed requires an exponent greater
                      than two digits, additional exponent
                      digits are printed as necessary.
            
              'g' or 'G'

                      The floating-point-number argument is
                      printed in style f or e (or int style E
                      n the case of a G conversion
                      character), with the precision
                      specifying the number of significant
                      digits.  The style used depends on the
                      value converted; style e is used only
                      if the exponent resulting from the
                      conversion is less than -4 or greater
                      than or equal to the precision.
                      
              others
                      Strings without 'e', 'E', 'g' and 'G'
                      indicate a floating point conversion.
                      The floating point number argument is
                      printed in decimal notation in the
                      style [-]dddrddd, where the number of
                      digits after the radix character, r, is
                      equal to the precision specification.

              If the result of a conversion is longer than the 
              field width, an asterisk is returned. If the 
              input number is a blank, a 'b' is returned.
         


Examples:             Format string: +eeeeee.eeee     
                             Number: 43345.5436
                             Result:  +4.3346e+04
                             Remark: exponential format
                                     signed conversion
                                     field width: 12
                                     precision:    4

                      Format string: gggg.ggggg
                             Number: 34.43
                             Result:     34.430
                             Remark: Field width is 10
                                     Number of significant digits is 5

                      Format string: +ffff.ff
                             Number: 23.456
                             Result:   +23.46
                             Remark: signed conversion

                      Format string: -ffff
                             Number: 345.87
                             Result: 346
                             Remark: left justified

 
                      Format string: -+ffff.fff
                             Number: 34.43
                             Result: +34.430
                             Remark: left justified
                                     signed conversion

                      Format string: eee.eeee
                             Number: 234.43
                             Result:        *
                             Remark: Field width too small 
                                     for conversion

                      Format string: ffff.ff
                             Number: blank
                             Result:       b
                             Remark: input was a blank


Updates:      Aug 10, 1992: MGV, original document.
#<

@ integer function printusing( character, double precision, character )


*/

#include "string.h"       /* defines 'strcspn', 'strcpy', 'strcat', 'strpbrk' */
#include "stdio.h"        /* defines 'sprintf' */
#include "gipsyc.h"       /* defines the ANSI-F77 types for the Fortran to C */
                          /* interface, defines str2char */
#include "nelc.h"         /* number of  characters in in a fortran character */
                          /* string discarding trailing blanks */
#include "dblank.h"       /* logical function returning whether argument is a */
                          /* universal BLANK (undefined) */

fint printusing_c( fchar Formatstr, double *dnumber, fchar Resultstr )
/*--------------------------------------------------------------------------*/ 
/* Decode 'Formatstr' to a C-type format and return formatted               */
/* 'number' in 'Resultstr'. Examples of possible format strings can         */
/* be found in the description. Return the field width.                     */
/*--------------------------------------------------------------------------*/
{
   int    len;
   int    before, after;
   char   format[80];
   int    clen;
   char   *instr;
   char   mode;
   int    i;
   char   result[120];
   double number = *dnumber;
   fint   scr = 3;
   
   
   len    = (int) nelc_c( Formatstr );
   before = strcspn( Formatstr.a, "." );
   if (before > len) before = len;
   if (before!=len) after = len - before - 1; else after = 0;
   strcpy( format, "%" );
   for (i=0; i<2; i++) {
     mode = Formatstr.a[i];
     if (mode == '+') strcat( format, "+" );
     if (mode == '-') strcat( format, "-" );
   }   
   instr = strpbrk( Formatstr.a, "eEgG" );
   if (instr == NULL) {
      mode = 'f';
   } else {      
      /*--------------------------------------------------------------------*/
      /* For e, E, f, g and G conversions, the result shall always contain  */
      /* a radix character, even if no digits follow the radix character.   */
      /* For g and G conversions, trailing zeroes shall not be removed from */ 
      /* the result as they usually are.                                    */
      /*--------------------------------------------------------------------*/      
      mode = *instr;
      strcat( format, "#" );                           
   }

   sprintf( format, "%.*s%d.%d%c", strlen(format), format, len, after, mode );
   if (dblank_c(&number)) {
      sprintf( result, "%*s", len, "b" );
   } else {            
      clen = sprintf( result, format, number );
      if (clen > len) {
         sprintf( result, "%*s", len, "*" );      
      }
   }   
   /* Copy a zero-terminated character string to a fchar. */
   (void) str2char( result, Resultstr );
   return(len);
}

