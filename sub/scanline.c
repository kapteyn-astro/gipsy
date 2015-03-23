/* scanline.c

     Copyright (c) Kapteyn Laboratorium Groningen 2004
     All Rights Reserved.
*/
                    
                   
/*
#>            scanline.dc2

Function:     scanline

Purpose:      Set a mask for each point in a box that is inside a polygon
                            
Category:     MATH
                           
File:         scanline.c
                          
Author:       M.G.R. Vogelaar
                        
Description:  Ordered edge list algorithm from 'Procedural
              elements for computer graphics', David F. Rogers

Use:          void scanline( int   *mask, 
                             int    blo[0], 
                             int    blo[1], 
                             int    bhi[0], 
                             int    bhi[1], 
                             float *polyX, 
                             float *polyY, 
                             int    ndat );

              A polygon is defined by a number of vertices with X coordinates
              in 'polX' and Y coordinates in 'polY'. There are 'ndat' vertices
              in a polygon. The polygon may be or may not be closed. 
              A mask is an integer array with as many elements as contained in the
              box given by blo and bhi (usually from the BOX= keyword).
              This mask represents the data in a subset in a box with limits blo
              and bhi. Initially it should be set to zero for each element in the mask.
              The function loops over all grid positions in the box and if such a 
              grid is inside the polygon, then the corresponding position in the
              mask is flagged (i.e. one is added to the array element).
            
Remarks:      
              
Updates:      Jul 26, 2004: VOG, Document created.
              Apr 15, 2009: VOG, Replaced NINT macro with one that uses floor()
                                 for the calculation of positions.


#<
*/



/*
#> scanline.h
void scanline( int *, int, int, int, int, float *, float *, int);
#<
*/


#include  "stdlib.h"
#include  "gipsyc.h"
#include  "math.h"
#include  "userfio.h"


/* Pre Apr 2009 def.: #define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) ) */
#define NINT(a) ( (int) floor( (double) (a) + 0.5 ) )

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) ) 
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) ) 

void scanline( int   *mask, 
               int    xblo, int yblo, int xbhi, int ybhi, 
               float *Xpos, float *Ypos,
               int    ndat )
/*------------------------------------------------------------*/
/* PURPOSE: Set masker for grids inside/outside polygon       */
/* The mask is set by the vertices of the plotted spline.     */
/*------------------------------------------------------------*/
{
   typedef struct 
   {
      float Xi[10];
      float Yi[10];
      int   nis;
   } intersect;

   intersect   ins[10000];
      

   int     n = ndat;
   int     i, j;
   int     xi, yi;
   float   y;
   float   Y1, Y2, X1, X2; 
   int     nsect;
   float   *vertX;
   float   *vertY;
   int     scanline;
   int     numscanlines;
   int     lx = (xbhi - xblo) + 1;
   float   xlo, ylo, xhi, yhi;

   if (n < 3)
   {
      anyoutf( 1, "Not enough points to make a closed polygon!" );
      return;            
   }

   /* Allocate memory for vertices and possible closing position */
   vertX = (float*) malloc( (n+1) * sizeof(float) );
   vertY = (float*) malloc( (n+1) * sizeof(float) );
   
   /* Copy the data */
   for (i = 0; i < n; i++)
   {
      vertX[i] = Xpos[i];
      vertY[i] = Ypos[i];      
   }
  
   /* If polygon is not closed then close first */
   if (Xpos[0] != Xpos[n-1] ||
      (Ypos[0] != Ypos[n-1])  )
   {
       vertX[n] = Xpos[0];      
       vertY[n] = Ypos[0];
       n++;
   }
   
   
   /* Find minimum sized box that encloses these positions */
   /* Initialize */
   xlo = xhi = vertX[0];
   ylo = yhi = vertY[0];
   for (i = 1; i < n; i++)
   {
      float xf = vertX[i], yf = vertY[i];
      
      if (xf <  xlo) 
         xlo =  xf;
      if (xf > xhi) 
          xhi = xf;
      if (yf < ylo) 
         ylo =  yf;
      if (yf > yhi) 
         yhi = yf;          
   }
   
   xlo = NINT( xlo ); 
   xlo = MYMIN( xlo, xbhi );
   xlo = MYMAX( xlo, xblo );   

   xhi = NINT( xhi ); 
   xhi = MYMIN( xhi, xbhi );
   xhi = MYMAX( xhi, xblo );   

   ylo = NINT( ylo ); 
   ylo = MYMIN( ylo, ybhi );
   ylo = MYMAX( ylo, yblo );   

   yhi = NINT( yhi ); 
   yhi = MYMIN( yhi, ybhi );
   yhi = MYMAX( yhi, yblo );   
                  

   /*--------------------------------------------------*/
   /* Ordered edge list algorithm from 'Procedural     */
   /* elements for computer graphics', David F. Rogers */
   /*--------------------------------------------------*/      
   for (scanline = 0, y = ylo; y <= yhi; y += 1.0, scanline++)
   {
      nsect = 0;
      /* For each line determine intersections */
      for (i = 1; i < n; i++) 
      {
         X1 = vertX[i-1]; X2 = vertX[i];
         Y1 = vertY[i-1]; Y2 = vertY[i];
         /* Does current scan line intersect this vertices connection line? */
         if (  (y >= Y1 && y <= Y2) ||
               (y <= Y1 && y >= Y2)  )
         {
            /* Calculate the intersection */
            if (Y1 != Y2)
            {
               /* Ignore Horizontal line */
               float Y = y;
               float Xisect = ( X1 + (X2-X1) * (Y-Y1) / (Y2-Y1) );             
               int   areequal = 0;

               /* Special cases are scanlines crossing vertices */
               if (Y == Y2)
               {
                  areequal = 1;                  
                  if (Y < Y1)
                  {
                     ins[scanline].Xi[nsect] = Xisect;
                     ins[scanline].Yi[nsect] = Y;
                     nsect++;
                  }
               }
               if (Y == Y1)
               {
                  areequal = 1;                                   
                  if (Y < Y2)
                  {
                     ins[scanline].Xi[nsect] = Xisect;
                     ins[scanline].Yi[nsect] = Y;                     
                     nsect++;    
                  }
               }
               if (!areequal)
               {
                  ins[scanline].Xi[nsect] = Xisect;
                  ins[scanline].Yi[nsect] = Y;
                  nsect++;
               }
            }
         }
      } /* These were all the vertice lines */       
      ins[scanline].nis = nsect;
      /* Sort these intersections to increasing x on the scan line */      
      for (i = 1; i < nsect; i++) 
      {
         for (j = 0; j <= i; j++) 
         {
            float temp;
            if (ins[scanline].Xi[j] > ins[scanline].Xi[i]) 
            {
               temp  = ins[scanline].Xi[j];
               ins[scanline].Xi[j] = ins[scanline].Xi[i];
               ins[scanline].Xi[i] = temp;
               /* No need to sort the Y values because these are the */
               /* same for each scan line */
            }
         }
      }      
   } /* These were all the scan lines */
   numscanlines = scanline;   
   

#if (0)  
   for (i = 0; i < scanline; i++)
   {
      for (j = 0; j < ins[i].nis; j++) 
      {
         anyoutf( 1, "scanline=%d int#=%d %f %f", i, j, ins[i].Xi[j],
                              ins[i].Yi[j] );
      }
   }
#endif


   for (scanline = 0, yi = ylo; yi <= yhi; yi++, scanline++)
   {
      for (xi = xlo; xi <= xhi; xi++)
      {
         int  Xi, Yi, indx;
         /* Mark the points inside the contour */
         Xi = xi - xblo;
         Yi = yi - yblo;            
         indx = Yi * lx + Xi;         
         for (i = 0; i < ins[scanline].nis/2; i++)
         {
            float x1 = ins[scanline].Xi[i*2+0];
            float x2 = ins[scanline].Xi[i*2+1];
            float X = (float) xi;
            if (X >= x1 && X <= x2)
            {               
               mask[indx] += 1;
               break; /* No need to examine more intervals */
            }
         }
      }
   }

   free( vertY );
   free( vertX );    
}


