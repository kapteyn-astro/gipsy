/*
                            COPYRIGHT (c) 1995
                      Kapteyn Astronomical Institute
                University of Groningen, The Netherlands
                           All Rights Reserved.


#>             minbox.dc1

Program:       MINBOX

Purpose:       MINBOX calculates the smallest box in a (sub)set,
               outside which only blanks exist and creates an output
               set with the calculated box size. It stores the box
               in physical coordinates in the header of the set.          

Category:      UTILITY

File:          minbox.c

Author:        M.G.R. Vogelaar

Keywords:


   INSET=      Give input set (, subsets):

               Maximum number of subsets is 2048.


   OUTSET=     Set and subset(s) where to copy to:     [do not copy]


** MARGIN=     Give n (subset dimension) margin values: [no margins]


** SQUARE=     Adjust (2dim) box sizes to form a square?       Y/[N]
               
               If the dimension of a box is greater than or equal to 
               2 then it is possible for the first two box axes
               to adjust their sizes so that a square box is the
               resulting shape. If SQUARE=Y then only the smallest
               axis size will increase. The maximum however is the
               size of the frame of the set. Note that if you enter
               a box at MINBOX= and this entry is not a square box
               then SQUARE=Y will transform this box into a square 
               box as described above by increasing the size of
               the smallest axis. If it encounters a border it will
               increase only in the other direction. If both
               boundaries are crossed the result is possibly not a
               square.
               This keyword can be handy 
               if cursor input is given to define a region around
               an object, but you want a square version of this
               box as output box.


   MINBOX=     Change default values for box:           [calculated]

               Program MINBOX prompts with the calculated default 
               values of a box so that you can use these values, 
               or alter them before MINBOX continues.
               NOTE, there is no (boundary) check of the box that 
               is altered by the user.
               Read also the remarks in the description of the 
               hidden keyword SQUARE=
               
              


Description:   MINBOX calculates the smallest box in the given
               (sub)set(s), outside which only blanks exist.
               Then program COPY is called with the calculated
               box parameters to actually copy the data into a new
               set. Therefore COPY prompts with the OUTSET=
               keyword. The box calculated by MINBOX can be changed
               by using the MARGIN= keyword. This keyword accepts
               at most n integer values where n equals the dimension
               of the input subset(s). If a margin value is positive,
               it will increase the size of the 'minbox'.

               The box that was found (or entered) in grids by MINBOX 
               is converted to a box in physical coordinates and stored  
               as a character string in a table in the header of the 
               input set. The name of the table is MBOX, the column
               is MINBOX and the level is the subset level.
               If possible the units are converted to degrees and 
               then this unit is abbreviated to DEG. If you want to run 
               MINBOX on set AURORA freq 2 then type 

               MINBOX INSET=AURORA FREQ 2
               
               Now inspect the contents with:
               
               TABLE
               INSET=AURORA FREQ 2
               OPTION=5
               TABCOL1=1
               FORMAT1=
               COLNAME1=
               COLUNITS1=
               TABCOL2=
               DESTINATION=


               Result will look something like this:
               
                                                                  MINBOX
                                                                    NONE
               =========================================================
                 44.985860 DEG 44.989999 DEG 45.014145 DEG 45.009999 DEG


Notes:

Example:       Minimize set AURORA with axes RA, DEC and FREQ
               in RA and DEC directions only:
               INSET=AURORA FREQ
               OUTSET=AURORA_SMALL1


               Minimize same set AURORA in all directions:
               INSET=AURORA
               OUTSET=AURORA_SMALL2

               Minimize same set AURORA in RA and DEC direction
               but copy only frequency planes 4 to 60:
               INSET=AURORA FREQ 4:60
               OUTSET=AURORA_SMALL3


Updates:       feb 14,  1995: VOG, Document created.
               mar 10,  1995: VOG, MARGIN= keyword included.
               oct 20,  1995: VOG, SQUARE= keyword added.
                                   MINBOX string in header
               may 10,  2000: VOG, Force SQUARE=Y to square any manual
                                   box input

#<
*/

/*  cursor.c: include files     */

#include    "stdio.h"        /* Defines ANSI C input and output utilities */
#include    "stdlib.h"       /* Defines the ANSI C functions for number */
                             /* conversion, storage allocation, and similar tasks.*/
#include    "string.h"       /* Declares the ANSI C string functions*/
                             /* like:strcpy, strcat etc.*/
#include    "math.h"         /* Declares the mathematical functions and macros.*/
#include    "cmain.h"        /* Defines the main body of a C program with */
                             /* MAIN_PROGRAM_ENTRY and IDENTIFICATION */
#include    "gipsyc.h"       /* Defines the ANSI-F77 types for Fortran to C intface */
                             /* including def. of char2str,str2char,tofchar,zadd */
                             /* and macros tobool and toflog */
#include    "float.h"        /* Definition of FLT_MAX etc.*/
#include    "ctype.h"        /* Declares ANSI C functions for testing characters */
                             /* like: isalpha, isdigit etc. also tolower, toupper.*/

/* Common includes */

#include    "init.h"         /* Declare task running to HERMES and initialize.*/
#include    "finis.h"        /* Informs HERMES that servant quits and cleans up the mess.*/
#include    "anyout.h"       /* General character output routine for GIPSY programs.*/
                             /* 0 use default [set by HERMES to 3] */
                             /* 1  terminal                        */
                             /* 2  LOG file                        */
                             /* 8  terminal, suppressed in "experienced mode" */
                             /* 16  terminal, only when in "test mode". */
#include    "status.h"       /* Display info in the "RUNNING" status display */
#include    "userfio.h"      /* Easy-C companions for GIPSY user interface routines */
#include    "userint.h"
#include    "usertext.h"
#include    "setfblank.h"    /* Function to set a data value to the universal BLANK.*/
#include    "error.h"        /* User error handling routine. */
#include    "myname.h"       /* Obtain the name under which a GIPSY task is being run.*/
#include    "nelc.h"         /* Characters in F-string discarding trailing blanks.*/
#include    "grtoph.h"
#include    "axunit.h"
#include    "userlog.h"

/* GDS stuff */

#include    "gdsinp.h"       /* Input of set, subsets, return # subsets.*/
#include    "gdsbox.h"       /* Define a frame */
#include    "gdsc_ndims.h"   /* Return the dimensionality of a coordinate word. */
#include    "gdsc_range.h"   /* Return lower left and upper right corner of a subset.*/
#include    "gdsc_grid.h"    /* Extract grid value.*/
#include    "gdsc_fill.h"    /* return coordinate word filled with a grid. */
                             /* value for each axis. */
#include    "gdsi_read.h"    /* Reads data from (part of) a set.*/


/* Spawn a task */

#include    "wkey.h"         /* Write keywords to task's own parameter list */
#include    "deputy.h"       /* Start a task which temporarily assumes the */
                             /* role of the calling task. */



#include    "gdsa_colinq.h"
#include    "gdsa_delcol.h"
#include    "gdsa_crecol.h"
#include    "gdsa_wcchar.h"

/* DEFINITIONS: */

/* Initialize Fortran compatible string with macro 'fmake' */

#define fmake(fchr,size) { \
                           static char buff[size+1]; \
                           int i; \
                           for (i = 0; i < size; buff[i++] = ' '); \
                           buff[i] = 0; \
                           fchr.a = buff; \
                           fchr.l = size; \
                         }


/* Copy a c-string to an fchar */
#define fcopy( f, c )                   \
        {int k;for(k=0;c[k]&&k<f.l;f.a[k]=c[k],k++);while(k<f.l)f.a[k++]=' ';}


/* Malloc version of 'fmake', but now space must be freed */
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }


/* Miscellaneous defines */

#define MYMAX(a,b)     ( (a) > (b) ? (a) : (b) )
#define MYMIN(a,b)     ( (a) > (b) ? (b) : (a) )
#define NINT(a)        ( (a) < 0 ? (int)((a)-.5) : (int)((a)+.5) )
#define ABS(a)         ( (a) < 0 ? (-(a)) : (a) )
#define PI             3.141592653589793
#define RAD(a)         ( (a) * 0.017453292519943295769237 )
#define DEG(a)         ( (a) * 57.295779513082320876798155 )

#define RELEASE        "1.0"           /* Version number */
#define MAXAXES        10              /* Max. axes in a set */
#define MAXSUBSETS     2048            /* Max. allowed subsets */
#define STRLEN         256             /* Max length of strings */
#define MAXBUF         256*256
#define FITSLEN        20
#define NONE           0               /* Default levels in userxxx routines */
#define REQUEST        1
#define HIDDEN         2
#define EXACT          4
#define YES            1               /* C versions of .TRUE. and .FALSE. */
#define NO             0


#define ITEMLEN 8
#define VARRECLEN  132

/* Defines for in/output routines etc.*/

#define KEY_INSET      tofchar("INSET=")
#define MES_INSET      tofchar("Give input set (, subsets):")
#define KEY_MARGIN     tofchar("MARGIN=")
#define KEY_MINBOX     tofchar("MINBOX=")
#define TSK_COPY       tofchar("copy")



/* Variables for input */


static fint     subin[MAXSUBSETS];  /* Subset coordinate words */

static fint     axnum[MAXAXES];     /* Array of size MAXAXES containing the */
                                    /* axes numbers.  The first elements (upto */
                                    /* the dimension of the subset) contain the */
                                    /* axes numbers of the subset, the other */
                                    /* ones ontain the axes numbers outside the */
                                    /* the subset ordered ccording to the */
                                    /* specification by the user. */
static fint     axcount[MAXAXES];   /* Array of size MAXAXES containing the */
                                    /* number of grids along an axes as */
                                    /* specified by the user. The first elements */
                                    /* (upto the dimension of the subset) contain */
                                    /* the length of the subset axes, the other */
                                    /* ones contain the the number of grids along */
                                    /* an axes outside the subset. */



/* Box and frame related */

static fint     flo[MAXAXES];       /* Low  edge of frame in grids */
static fint     fhi[MAXAXES];       /* High edge of frame in grids */
static fint     blo[MAXAXES];       /* Low  edge of box in grids */
static fint     bhi[MAXAXES];       /* High edge of box in grids */
static fint     vlo[MAXAXES];
static fint     vhi[MAXAXES];
static fint     vect[MAXAXES];


/* Reading data */

static float    image[MAXBUF];


/* Miscellaneous */

static fint     setlevel = 0;       /* To get header items at set level. */
static float    blank;              /* Global value for BLANK. */
static char     message[STRLEN];    /* All purpose character buffer. */





static void printstatus( fchar Tsk, fint status )
/*------------------------------------------------------------*/
/* PURPOSE: Display error for deputy call.                    */
/*------------------------------------------------------------*/
{
   if (status == -6)
      anyoutf( 1, "Called task (%.*s) not present", nelc_c(Tsk), Tsk.a );
   if (status == -7)
      anyoutf( 1, "Max. number of tasks already active" );
}



static void makesquare( fint *vlo,
                        fint *vhi,
                        fint *flo,
                        fint *fhi,
                        fint subdim )
/*------------------------------------------------------------*/
/* PURPOSE: Make a square box with the first two axes in      */
/*          vlo, vhi.                                         */
/*------------------------------------------------------------*/
{
   int  maxlen, minlen;
   int  difflen;
   int  extrahi, extralo;
   int  lenx = vhi[0] - vlo[0] + 1;
   int  leny = vhi[1] - vlo[1] + 1;
   int  i;
   

   if (subdim < 2)
   {
      anyoutf( 1, "Dimension must be >= 2");
      return;
   }

   maxlen = MYMAX( lenx, leny );
   minlen = MYMIN( lenx, leny );
   difflen = maxlen - minlen;
   extrahi = difflen / 2;                            /* Extension to the right */
   extralo = difflen - extrahi;                        /* Extension to the left */


   /* Adjust but do not exceed boundaries */

   i = -1;
   if (lenx > leny)
   {      
      i = 1;
   }
   if (leny > lenx)
   {
      i = 0;
   }
   if (i != -1)
   {
      fint   xlo, xhi, delta;          
      xlo = vlo[i] - extralo;
      delta = flo[i] - xlo;
      /* Does this box crosses the bottom (or left) border then increase */
      /* to upper (right) */
      if (delta > 0)
      {
         extralo -= ABS( delta );
         extrahi += ABS( delta );
      }
      xhi = vhi[i] + extrahi;
      delta = fhi[i] - xhi;
      /* Does this box crosses the top (or right) border then increase */
      /* to bottom (left) */
      if (delta < 0) 
      {
         extrahi -= ABS( delta ); 
         extralo += ABS( delta );
      }
      vlo[i] -= extralo;
      vhi[i] += extrahi;
   }
   /* Final check on boundaries */
   vlo[0] = MYMAX( vlo[0], flo[0] );
   vhi[0] = MYMIN( vhi[0], fhi[0] );      
   vlo[1] = MYMAX( vlo[1], flo[1] );
   vhi[1] = MYMIN( vhi[1], fhi[1] );     
}


static int colexist( fchar Setin,
                     fint  subset,
                     fchar Tname,
                     fchar Cname )
/*------------------------------------------------------------*/
/* PURPOSE: Does a column in this table in this set exists?   */
/*------------------------------------------------------------*/
{
   fint    r1 = 0;
   fint    nrows;
   fchar   Units;
   fchar   Comment;
   fchar   Type;
   
   fmake( Units,   VARRECLEN );
   fmake( Comment, VARRECLEN );
   fmake( Type,    VARRECLEN );

   gdsa_colinq_c( Setin, &subset, Tname, Cname, Type, Comment, Units, &nrows, &r1 );
   if (r1 < 0)
   {
      anyoutf( 16, "Cannot get info from column [%.*s] in set [%.*s]",
               nelc_c(Cname), Cname.a, nelc_c(Setin), Setin.a );
      return( 0 );
   }
   return( 1 );
}


static void physbox( fint  *vlo,
                     fint  *vhi,
                     fchar Setin,
                     fint  subset,
                     fint  *axnum )
/*------------------------------------------------------------*/
/* PURPOSE: Convert the box in grids to a box in physical     */
/*          coordinates. It uses DEG for units that can be    */
/*          converted to degrees.                             */
/*------------------------------------------------------------*/
{
   int       i;
   fint      subdim;
   fint      r;
   char      message[512];
   double    coordin[MAXAXES], coordout[MAXAXES];


   subdim = gdsc_ndims_c( Setin, &subset );

   for (i = 0; i < subdim; i++)
      coordin[i] = (double) vlo[i];

   grtoph_c( Setin, &subset, coordin, coordout );
   message[0] = '\0';
   for (i = 0; i < subdim; i++)
   {
      fchar  Unit;
      char   cbuf[FITSLEN];

      fmake( Unit, FITSLEN );
      r = axunit_c( Setin, &axnum[i], Unit );
      if (r != 0)
         str2char( "?", Unit );
      /* Abbreviate if units are degrees */
      if ( strncmp("DEGREE", Unit.a, MYMIN(6,nelc_c(Unit))) == 0 )
         Unit.l = sprintf( Unit.a, "DEG" );         
      sprintf( cbuf, "%f %.*s ", coordout[i], nelc_c(Unit), Unit.a );
      strcat( message, cbuf );
   }

   for (i = 0; i < subdim; i++)
      coordin[i] = (double) vhi[i];

   grtoph_c( Setin, &subset, coordin, coordout );
   for (i = 0; i < subdim; i++)
   {
      fchar  Unit;
      char   cbuf[FITSLEN];

      fmake( Unit, FITSLEN );
      r = axunit_c( Setin, &axnum[i], Unit );
      if (r != 0)
         str2char( "?", Unit );
      
      /* Abbreviate if units are degrees */
      if ( strncmp("DEGREE", Unit.a, MYMIN(6,nelc_c(Unit))) == 0 )
         Unit.l = sprintf( Unit.a, "DEG" );
      sprintf( cbuf, "%f %.*s ", coordout[i], nelc_c(Unit), Unit.a );
      strcat( message, cbuf );
   }


   {
      /* Store string < VARRECLEN characters in a table/column */
      fchar   Cname;
      fchar   Tname;
      fchar   Units;
      fchar   Comment;
      fchar   Type;
      char    typebuf[8];
      fint    r1;
      fint    one = 1;

      fmake( Tname,   ITEMLEN );
      fmake( Cname,   ITEMLEN );
      fmake( Units,   VARRECLEN );
      fmake( Comment, VARRECLEN );
      fmake( Type,    VARRECLEN );
   
      fcopy( Tname, "MBOX" );
      fcopy( Cname, "MINBOX" );
      fcopy( Units, "NONE" );
      if ( colexist( Setin, subset, Tname, Cname) )
      {
         r1 = 0;
         gdsa_delcol_c( Setin, &subset, Tname, Cname, &r1 );
         if (r1 < 0)
            anyoutf( 1, "gdsa_delcol error nr %d", r1 );
      }
      sprintf( typebuf, "CHAR%d", MYMIN(132, strlen(message)+1) );
      fcopy( Type, typebuf );
      fcopy( Comment, "Box result of MINBOX" );
      r1 = 0;      
      gdsa_crecol_c( Setin, &subset, Tname, Cname, Type, Comment, Units, &r1 );
      if (r1 < 0)
         anyoutf( 1, "gdsa_crecol error nr %d", r1 );
      r1 = 0;
      gdsa_wcchar_c( Setin, &subset, Tname, Cname, tofchar(message), &one, &one, &r1 );
      if (r1 < 0)
         anyoutf( 1, "gdsa_wcchar error nr %d", r1 );
      anyoutf( 3, "MINBOX wrote in the header of set %.*s, table MBOX, column MINBOX:", 
               nelc_c(Setin), Setin.a );      
      anyoutf( 3, "%s", message );            
   }   
}



MAIN_PROGRAM_ENTRY
/*-------------------------------------------------------------------------*/
/* The macro MAIN_PROGRAM_ENTRY replaces the C-call main() to start the    */
/* main body of your GIPSY application. Variables defined as 'fchar' start */
/* with a capital.                                                         */
/*-------------------------------------------------------------------------*/
{
   fint    r1, r2;
   fint    subnr;
   fint    setdim, subdim;
   bool    allblanks;
   bool    square;
   fint    dfault;
   fint    nitems;
   fint    nsubs;
   fint    status;
   fchar   Setin;
   fchar   Setout;
   fint    margin[MAXAXES];
   int     i;


   init_c();                               /* Contact Hermes */
   /* Task identification */
   {
      static fchar    Task;                /* Name of current task */
      fmake( Task, 20 );                   /* Macro 'fmake' must be available */
      myname_c( Task );                    /* Get task name */
      Task.a[nelc_c(Task)] = '\0';         /* Terminate task name with null char*/
      IDENTIFICATION( Task.a, RELEASE );   /* Show task and version */
   }
   setfblank_c( &blank );

   /*---------------*/
   /* Ask input set */
   /*---------------*/
   {
      fint     class = 1;
      fint     dev = 3;
      fint     maxsubs = MAXSUBSETS;
      fint     maxaxes = MAXAXES;     /* Max num. of axes the program can deal with.*/

      fmake( Setin, STRLEN );
      dfault  = NONE;
      subdim  = 0;

      nsubs   = gdsinp_c( Setin,      /* Name of input set. */
                          subin,      /* Array containing subsets coordinate words. */
                          &maxsubs,   /* Maximum number of subsets in 'subin'.*/
                          &dfault,    /* Default code as is USERxxx. */
                          KEY_INSET,  /* Keyword prompt. */
                          MES_INSET,  /* Keyword message for the user. */
                          &dev,       /* Device number (as in ANYOUT). */
                          axnum,      /* Array of size 'maxaxes' containing the axes numbers. */
                                      /* The first elements (upto the dimension of the subset) */
                                      /* contain the axes numbers of the subset, */
                                      /* the other ones contain the axes numbers */
                                      /* outside the subset ordered according to the */
                                      /* specification by the user. */
                          axcount,    /* Number of grids on axes in 'axnum' */
                          &maxaxes,   /* Max. number of axes. */
                                      /* the operation for each subset. */
                          &class,     /* Class 1 is for applications which repeat */
                          &subdim );  /* Dimensionality of subset(s). */
      setdim  = gdsc_ndims_c( Setin, &setlevel );
   }


   dfault = HIDDEN;

   for (i = 0; i < subdim; i++)
      margin[i] = 0;
   (void) sprintf( message, "Give %d margin values:     [no margins]",
                   subdim );
   r1 = userint_c( margin,
                   &subdim,
                   &dfault,
                   KEY_MARGIN,
                   tofchar(message) );
   /*------------------------------------------------------*/
   /* Determine the edges of this its frame.               */
   /*------------------------------------------------------*/
   {
      fint   cwlo, cwhi;
      int    m;
      r1 = 0;
      gdsc_range_c( Setin,
                    &setlevel,
                    &cwlo,
                    &cwhi,
                    &r1 );
      for (m = 0; m < (int) setdim; m++)
      {
         r1 = r2 = 0;
         flo[m] = gdsc_grid_c( Setin,
                               &axnum[m],
                               &cwlo,
                               &r1 );
         fhi[m] = gdsc_grid_c( Setin,
                               &axnum[m],
                               &cwhi,
                               &r2 );
         blo[m] = flo[m];
         bhi[m] = fhi[m];
      }
   }


   nitems = 1;
   dfault = HIDDEN;
   square = toflog( NO );
   r1 = userlog_c( &square, &nitems, &dfault, tofchar("SQUARE="),
                   tofchar("Adjust (2dim) box sizes to a square?            Y/[N]") );
   square = tobool( square );


   allblanks = YES;
   /*---------------------------------------*/
   /* Determine smallest box with real data.*/
   /* Do it in subsets to be able to keep   */
   /* the number of subsets constant (if    */
   /* user want is that way.                */
   /*---------------------------------------*/
   for (subnr = 0; subnr < nsubs; subnr++)
   {
      fint   tid = 0;
      fint   cwlo, cwhi;
      fint   totcount = 0;
      int    i;

      cwlo = gdsc_fill_c( Setin, &subin[subnr], flo );
      cwhi = gdsc_fill_c( Setin, &subin[subnr], fhi );

      for (i = 0; i < subdim; i++)
         vect[i] = flo[i];
      do                                          /* Loop until read all */
      {
         fint   maxIObuf = MAXBUF;
         fint   pixelsdone;
         int    m;

         /* Read 'maxIObuf' values in 'image'. */
         gdsi_read_c( Setin,
                      &cwlo, &cwhi,
                      image,
                      &maxIObuf,
                      &pixelsdone,
                      &tid );

         for (i = 0; i < pixelsdone; i++)         /* Loop over all pixels */
         {                                        /* in the buffer.       */
            if (image[i] != blank)
            {
               if (allblanks)                     /* Set box at first non-blank */
               {
                  for (m = 0; m < subdim; m++)
                     blo[m] = bhi[m] = vect[m];
               }
               else
               {
                  for (m = 0; m < subdim; m++)    /* Compare value with min.box */
                  {
                     if (vect[m] < blo[m])
                        blo[m] = vect[m];
                     if (vect[m] > bhi[m])
                        bhi[m] = vect[m];
                  }
               }
               allblanks = NO;
            }
            totcount++;
            /*-------------------------------------------------------*/
            /* Increase the vector value. Start at vector value with */
            /* the lowest index. If this value exceeds the highest   */
            /* of the input frame, reset its value and increase      */
            /* vector value for next index (next axis). Repeat this  */
            /* for all subset axes.                                  */
            /*-------------------------------------------------------*/
            vect[0]++;
            for (m = 1; m < subdim; m++)
            {
               if (vect[m-1] > fhi[m-1])
               {
                  vect[m-1] = flo[m-1];
                  vect[m]++;
               }
            }
         }
      }
      while (tid != 0);
   }

   if (allblanks)
   {
      anyoutf( 1, " " );
      anyoutf( 1, "WARNING: All pixels in the input were blank" );
      anyoutf( 1, "         Could not find a box for COPY!" );
      anyoutf( 1, " " );
   }
   else
   {
      char appstr[STRLEN];
      strcpy( message, "BOX=" );
      {
         int     i;
         fint    boxopt;
         fint    dev = 8;
         char    buff[STRLEN];
         fchar   Mesbox;

         for (i = 0; i < subdim; i++)
         {
            vlo[i] = MYMAX(blo[i]-margin[i], flo[i]);
            vhi[i] = MYMIN(bhi[i]+margin[i], fhi[i]);
         }
         if (square && subdim > 1)
         {
            anyoutf( 1, "Creating a square box because SQUARE=Y" );
            makesquare( vlo, vhi, flo, fhi, subdim );
         }

         for (i = 0; i < subdim; i++)
         {
            (void) sprintf( appstr, "%d ", vlo[i] );
            strcat( message, appstr );
         }
         for (i = 0; i < subdim; i++)
         {
            (void) sprintf( appstr, "%d ", vhi[i] );
            strcat( message, appstr );
         }
         anyoutf( 1, "  MINBOX found %s", message );
         fmake( Mesbox, STRLEN );
         dfault = REQUEST;
         sprintf( buff, "Change default values for box:  [%s]", 
                  &message[strlen("BOX=")] );
                  
         /* Fill the grid vectors. Use the input string for 'usertext'. */
         
         boxopt = 6;  /* The defaults are in vlo, vhi */
         gdsbox_c( vlo, vhi, 
                   Setin, subin, 
                   &dfault, 
                   KEY_MINBOX, 
                   tofchar(buff),
                   &dev, 
                   &boxopt );

         r1 = usertext_c( Mesbox, &dfault, KEY_MINBOX, tofchar(buff) );
         /* User entered his own box but it MUST be square */ 
         if (r1)
         {
            if (square && subdim > 1)
            {
               anyoutf( 1, "Ok, a new box is entered but we want a square version of it" );
               makesquare( vlo, vhi, flo, fhi, subdim );
            }
            strcpy( message, "BOX=" );
            for (i = 0; i < subdim; i++)
            {
               (void) sprintf( appstr, "%d ", vlo[i] );
               strcat( message, appstr );
            }
            for (i = 0; i < subdim; i++)
            {
               (void) sprintf( appstr, "%d ", vhi[i] );
               strcat( message, appstr );
            }
         }
         cancel_c( KEY_MINBOX );
         anyoutf( 8, "Final box in pixels is: %s", &message[strlen("BOX=")] );
/*         sprintf( message, "BOX=%s", &message[strlen("BOX=")] );*/
            
         fmake( Setout, STRLEN );
         dfault = REQUEST;
         r1 = usertext_c( Setout, &dfault, tofchar("OUTSET="),
                          tofchar("Set (and subset(s)) where to copy to:     [do not copy]" ) );
          
         physbox( vlo, vhi, Setin, subin[0], axnum );

         if (r1)
         {
            anyoutf( 8, "Task COPY is called with keyword %s", message );
            wkey_c( tofchar(message) );               /* Substitute keyword in par. list */
            deputy_c( TSK_COPY, &status );            /* Spawn COPY task */
            if (status != 1)                          /* Something wrong? */
               printstatus( TSK_COPY, status );
         }
      }
   }
   finis_c();
   return(EXIT_SUCCESS);   /* Dummy return */
}
