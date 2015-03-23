/* getrange.c
                              COPYRIGHT (c) 1991
                        Kapteyn Astronomical Institute
         University of Groningen  -  9700 AV Groningen, The Netherlands
                             All Rights Reserved.
 
#>            getrange.dc2

Subroutine:   GETRANGE

Purpose:      User input interface routine for two reals 

Category:     USER IO

Files:        getrange.c

Author:       M. Vogelaar

Use:          CALL GETRANGE(RANGE  ,    Output  REAL ARRAY
                            DEFAULT,    Input   INTEGER
                            KEY,        Input   CHARACTER
                            MES         Input   CHARACTER
                            )

              RANGE         Two reals indicating a range
              DEFAULT       Default code ( 0: no default, 1: default,
                                           2: hidden, 4: exact number).
              KEY           Keyword prompt.
              MES           Message for user.

Description:  Interface routine based on function 'USERREAL', but
              limited to two reals and extended with -INF and INF
              which stands for the biggest negative and the biggest
              positive value of the current system.
                                          
Updates:      Apr 4, 1990: MV, Document created.

#<

Fortran to C interface:

@ subroutine getrange( real, integer, character, character )

*/



#include "stdio.h"
#include "gipsyc.h"
#include "dcdreal.h"
#include "float.h"                       /* Definition of FLT_MAX             */
#include "usercharu.h"
#include "reject.h"
#include "string.h"                      /* Functions 'strstr' and 'strcpy'   */



void getrange_c( float *range, fint *Fdfault, fchar Fkeyword, fchar Fmessage  )
/*
 *------------------------------------------------------------------------------
 * Extend user input with +/-INF. Make sure input always consists of two
 * values.
 *------------------------------------------------------------------------------
 */
{


#define   SMALLSTORE   80
#define   true          1


   static fchar  Finputstr;              /* Complete buffer for 'usercharu'   */
   static fint   Fnumitems;              /* Max. number of items to return    */
   static fint   Freturned;              /* Number of items returned          */
   static int    i;                      /* Counter                           */
   static int    agreed, ok;             /* Loop guards                       */
   static char   string[SMALLSTORE+1];   /* Get sub string from string buffer */
   static fint   Ferrorcode;             /* Error in real conversion          */
   static char   buff[2*SMALLSTORE+1];   /* Character space for Fortran str.  */
   static char   convbuf[100];           /* Convert from string to float      */
  
 
   /* Create space for two strings */
   for (i = 0; i < 2*SMALLSTORE; buff[i++] = ' ')
   buff[i] = '\0'; 
   Finputstr.a = buff;
   Finputstr.a[SMALLSTORE] = '\0';     

   if (*Fdfault > 0) {
      /* create default string with default values */
      sprintf( convbuf, "%30g", range[0] );
      strcpy( Finputstr.a, convbuf );
      sprintf( convbuf, "%30g", range[1] );
      strcpy( Finputstr.a + SMALLSTORE + 1, convbuf );
   }

   Finputstr.l = SMALLSTORE+1;
   do { /* Until two values could be converted */
      Fnumitems = 2;
      do { /* Until user input was two strings */
         Freturned = usercharu_c( Finputstr, 
                                  &Fnumitems, 
                                  Fdfault,       
                                  Fkeyword, 
                                  Fmessage );
         if (Freturned == 0) return;                                  
         agreed = (Freturned == 2);                                  
         if (!agreed) {
            reject_c( Fkeyword, tofchar("2 values required") );
            *Fdfault = 0;  /* request */
         }
      } while (!agreed);
      
      Finputstr.a[SMALLSTORE] = '\0'; 
      Finputstr.a[2*SMALLSTORE+1] = '\0'; 
      
         
        
      /* Decode the two strings and check whether they contain     */
      /* one of -INF or INF. For these strings, the min and max.   */
      /* in the floating point range is substituted.               */
      
      for (i = 0; i <= 1; i++) { 
         if (i == 0) strcpy(string, Finputstr.a);
         else strcpy(string, Finputstr.a + SMALLSTORE + 1);
         if (strstr(string, "-INF") != NULL) {
            range[i] = -1.0*FLT_MAX;
            ok = true;
         }
         else {
            if (strstr(string, "INF") != NULL) {
                range[i] = FLT_MAX;
                ok = true;
            }
            else {
               Fnumitems = 1;
               Freturned = dcdreal_c( tofchar(string), &range[i], 
                                      &Fnumitems, &Ferrorcode );
               ok = (Ferrorcode == 0);
               if (!ok) {
                  reject_c( Fkeyword, tofchar("Wrong input!") );
                  *Fdfault = 0;
                  break;
               }
            }
         }
      }
   } while (!ok);
}


