/* xfile.c

	Copyright (c) Kapteyn Laboratorium Groningen 1990
	All Rights Reserved.

#>            xfile.doc

Program:      xfile

Purpose:      General file extractor.

Category:     SYSTEM MANAGEMENT

File:         xfile.c

Author:       J.P. Terlouw


Use:          xfile <fn> [-d<sym>] [-u<sym>] [-p<path>]  [-n<name>] [-w<wc>]
              <fn>        input file from which to extract files
              <path>      path to where extracted files will be written
              <sym>       symbol to be (un)defined
              <name>      only file names matching name will be extracted.
                          Wildcards are allowed. Default all files will
                          be extracted.
              <wc>        set the wildcard in <name>. Default is *.

Description:  Xfile extracts files from its input file. Files to be
              extracted are indicated by:  #>name  or %#>name at the
              beginning of an input line. The % in the second form can be
              any character. This character is then used as a flag
              character which must be the first character in subsequent
              lines belonging to this file. It is not copied to the
              output file, i.e. it is stripped off. Lines without the
              flag character are simply discarded. The end of a file to be
              extracted is indicated by #< or %#< .
              The input file can contain any number of files to be
              extracted which can also be nested. The names of the
              extracted files are written to standard output and are
              exactly (!) the same as stated in the input file.
              Conditional file extraction is also possible. A line
              containing a conditional statement is indicated by:
              #!<statement> or %#!<statement> at the beginning of an input
              line. The % in the second form can be any character. This
              character is NOT a flag character! The conditional statements
              allowed are: IF   <symbol> [<symbol> [...] ]
                           ELIF <symbol> [<symbol> [...] ]
                           ELSE
                           ENDIF

              A symbol can be any name (not containing white space) which
              can be defined by the system (ALLIANT, CONVEX, SUN etc.) or
              by the user on the command line.

Example:      File to be processed by xfile:

              0a
              #>    xfile.ou1
              1a
              *#>   xfile.ou2
              *2a
              *2b
              2b'
              *2c
              #<
              1b
              1c
              #<
              ##>   xfile.ou3
              #1A
              #1B
              #1C
              1C'
              #1D
              ##<
              0b
              0c

              Resulting files:

              (xfile.ou1:)
              1a
              1b
              1c

              (xfile.ou2:)
              2a
              2b
              2c

              (xfile.ou3:)
              1A
              1B
              1C
              1D

Updates:      Sep 23, 1989: JPT, original document.
              Oct 11, 1989: KGB, option -d and -t implemented.
              Apr 13, 1991: KGB, conditional file extraction implemented.
              Sep 16, 1991: KGB, unrecognized directives are skipped.
              Jan 14, 1993: KGB, Bug in -n switch repaired.
              Jan 14, 1998: JPT, Bug in checkLine repaired.
              Jun 20, 2007: JPT, Drastic increase in maximum line length.
              Feb 26, 2010: JPT, Maximum line length increased to 262143.

#<

*/

#include	"ctype.h"		/* <ctype.h> */
#include	"stddef.h"		/* <stddef.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"osdef.h"		/* defines the system */


#define	MAXSYMLEN	16		/* length of symbol */
#define MAXLINE		262143		/* length of lines */
#define	MAXNEST		16		/* nesting level */

#define	NEXTLINE	1		/* code: read next line */
#define	ENDFILE		2		/* code: close file */
#define	NEXTFILE	3		/* code: get next file */

typedef	struct { char s[MAXSYMLEN+1]; int d; } def_struct;

static	def_struct	*defs = NULL;	/* store the defines here */
static	int		ndefs = 0;	/* number of defines */
static	FILE		*infile = NULL;	/* input file descriptor */
static	int		status = EXIT_SUCCESS;
static	char		line[MAXLINE];
static	char		*name;
static	int		code;
static	int		lineno = 0;	/* line counter */
static	int		nchar;
static	char		wild = '*';	/* wild card */
static	char		mask[MAXLINE] = { "*" };
static	char		dir[MAXLINE] = { "" };
static	int		if_curl = 0;
static	long		if_code = 1;
static	long		if_mask = 1;
static	long		if_done = 0;
static	long		if_else = 0;


static	void	inidef( void )
/*
 * inidef sets up the system dependent symbols.
 */
{
   int	n = 0;

   defs = (def_struct *) realloc( defs, (ndefs + 1) * sizeof(def_struct) );
   defs[ndefs].d = 1;
   strcpy( defs[ndefs].s, OS_ARCHITECTURE );
   while (defs[ndefs].s[n]) {
      defs[ndefs].s[n] = toupper( defs[ndefs].s[n] );
      n += 1;
   }
   ndefs += 1;
#if	defined(__bsd__)
   defs = (def_struct *) realloc( defs, (ndefs + 1) * sizeof(def_struct) );
   defs[ndefs].d = 1;
   strcpy( defs[ndefs++].s, "BSD" );
#endif
#if	defined(__hpux__)
   defs = (def_struct *) realloc( defs, (ndefs + 1) * sizeof(def_struct) );
   defs[ndefs].d = 1;
   strcpy( defs[ndefs++].s, "HPUX" );
#endif
#if	defined(__sparc__)
   defs = (def_struct *) realloc( defs, (ndefs + 1) * sizeof(def_struct) );
   defs[ndefs].d = 1;
   strcpy( defs[ndefs++].s, "SPARC" );
#endif
#if	defined(__sun__)
   defs = (def_struct *) realloc( defs, (ndefs + 1) * sizeof(def_struct) );
   defs[ndefs].d = 1;
   strcpy( defs[ndefs++].s, "SUN" );
#endif
#if	defined(__sysv__)
   defs = (def_struct *) realloc( defs, (ndefs + 1) * sizeof(def_struct) );
   defs[ndefs].d = 1;
   strcpy( defs[ndefs++].s, "SYSV" );
#endif
#if	defined(__unix__)
   defs = (def_struct *) realloc( defs, (ndefs + 1) * sizeof(def_struct) );
   defs[ndefs].d = 1;
   strcpy( defs[ndefs++].s, "UNIX" );
#endif
#if	defined(__F2C__)
   defs = (def_struct *) realloc( defs, (ndefs + 1) * sizeof(def_struct) );
   defs[ndefs].d = 1;
   strcpy( defs[ndefs++].s, "F2C" );
#endif
}


static	void	adddef( char *s, int d )
/*
 * adddef adds the user defined or undefined symbols.
 */
{
   char	symbol[MAXSYMLEN+1];
   int	n;

   for (n = 0; *s && n < MAXSYMLEN; symbol[n++] = toupper(*s++));
   symbol[n] = 0;
   n = 0;
   while (n < ndefs && strcmp( symbol, defs[n].s )) n++;
   if (n == ndefs) {
      defs = (def_struct *) realloc( defs, (++ndefs) * sizeof(def_struct) );
      strcpy( defs[n].s, symbol );
   }
   defs[n].d = d;
}


static	int	ifdef( char *line )
/*
 * ifdef determines whether there are any defined symbols
 */
{
   char	symbol[MAXSYMLEN+1];
   int	m;
   int	n;
   int	nsym = 0;
   int	r = 0;

   while (!r && *line) {
      while (*line && isspace(*line)) line++;
      if (*line) {
         nsym++;
         n = 0;
         while (*line && !isspace(*line) && n < MAXSYMLEN) {
            symbol[n++] = toupper(*line++);
         }
         symbol[n] = 0;
         m = 0;
         while (m < ndefs && strcmp( defs[m].s, symbol )) m++;
         if (m < ndefs && defs[m].d) r = 1;
      }
   }
   if (!nsym) {
      fprintf( stderr, "Xfile -- no symbols on line #%d\n", lineno );
      status = EXIT_FAILURE;
   }
   return( r );
}


static	int	match( char *text )
/*
 * match compare filename with the userdefined mask
 */
{
   int	m = 0;
   int	r = 0;
   int	t = 0;

   while (mask[m]) {
      int	mi = m;
      int	ti = t;
      int	om = 0;

      r = 0;
      while (mask[m+om] == wild) om++;
      if (om && !mask[m+om]) { r = 1; break; }
      m += om;
      if (!m && !t) {
         while (mask[m] == text[t]) {
            t += 1; m += 1;
            if (!mask[m-1]) { r = 1; break; }
         }
         if (mask[m] == wild) r = 1;
      } else {
         int	c = 1;

         while (c) {
            while (mask[m] != text[t++]) {
               if (!text[t]) { c = 0; break; }
            }
            if (!c) break;
            mi = m; m += 1; ti = t;
            while (mask[m] == text[t]) {
               if (!mask[m]) { c = 0; break; }
               m += 1; t += 1;
            }
            if (mask[m] == wild) { c = 0; }
            if (!c) { r = 1; break; }
            m = mi; t = ti;
         }
      }
      if (!r) break;
   }
   return(r);
}


static	char	*strstrX( char super[], char sub[] )
/*
 * strstrX compares super and sub (with shift).
 */
{
   int	ixa,ixb;

   for (ixa=0; ;ixa++) {
      for (ixb=0; ;ixb++) {
         if (sub[ixb]==0) return (char *)&super[ixa];
         if (super[ixb+ixa]==0) return NULL;
         if (sub[ixb]!=super[ixb+ixa]) break;
      }
   }
}


static	int	readLine( char line[] )
/*
 *  Function to read a line from the input file.
 *  The contents of the line are passed to the argument in a zero-terminated
 *  string; The length of the line is returned as the function value.
 */
{
   int	index = -1;

   if (!feof(infile)) {
      lineno += 1;
      for ( index=0; index<MAXLINE; index++) {
        line[index] = getc(infile);
        if (line[index]=='\n' || feof(infile)) break;
      }
   }
   if ( index > -1 ) {
      line[index] = 0;
   } else {
      line[0] = 0;
   }
   return (index);
}


static	int	getLine( char line[] )
/*
 * getLine obtains next line and decides whether it passes the
 * symbols tests.
 */
{
   char	command[6];
   char *statement;
   int	m;
   int  nc;
   int	not;

   do {
      char	head[4];

      not = 0;
      nc = readLine( line );
      if (nc < 0) break;
      strncpy( head, line, 3 ); head[3] = 0;
      if (strstrX( head, "#!" ) != NULL) {
         statement = strstrX( line, "#!" ) + 2;
         m = 0;
         while (*statement && !isspace(*statement) && m < 5) {
            command[m++] = toupper(*statement++);
         }
         command[m] = 0;
         if (!strcmp( command, "IF" )) {
            int	r = ifdef( statement );

            if (if_curl > MAXNEST) {
               fprintf( stderr, "Xfile -- exceeded nesting level at line #%d\n", lineno );
               status = EXIT_FAILURE;
            } else {
               if_curl += 1;
               if_mask |= (1 << if_curl);
               if (r) {
                  if_code |= (1 << if_curl);
               }
            }
            not = 1;
         } else if (!strcmp( command, "ELIF" )) {
            int	r = ifdef( statement );

            if (!if_curl) {
               fprintf( stderr, "Xfile -- IF statement missing at LINE #%d\n", lineno );
               status = EXIT_FAILURE;
            } else {
               if (if_code & (1 << if_curl)) {
                  if_done |= (1 << if_curl);
                  if_code -= (1 << if_curl);
               }
               if (!(if_done & (1 << if_curl))) {
                  if (r) if_code |= (1 << if_curl);
               }
            }
            not = 1;
         } else if (!strcmp( command, "ELSE" )) {
            if (!if_curl) {
               fprintf( stderr, "Xfile -- IF statement missing at LINE #%d\n", lineno );
               status = EXIT_FAILURE;
            } else {
               if (if_else & (1 << if_curl)) {
                  fprintf( stderr, "Xfile -- ELSE misplaced at line #%d\n", lineno );
                  status = EXIT_FAILURE;
                  break;
               } else {
                  if_else |= (1 << if_curl);
               }
               if (if_code & (1 << if_curl)) {
                  if_done |= (1 << if_curl);
                  if_code -= (1 << if_curl);
               }
               if (!(if_done & (1 << if_curl))) {
                  if_code |= (1 << if_curl);
               }
            }
            not = 1;
         } else if (!strcmp( command, "ENDIF" )) {
            if (!if_curl) {
               fprintf( stderr, "Xfile -- IF statement missing at LINE #%d\n", lineno );
               status = EXIT_FAILURE;
            } else {
               if (if_code & (1 << if_curl)) {
                  if_done |= (1 << if_curl);
                  if_code -= (1 << if_curl);
               }
               if_else = (if_else | (1 << if_curl)) - (1 << if_curl);
               if_done = (if_done | (1 << if_curl)) - (1 << if_curl);
               if_mask -= 1 << if_curl;
               if_curl -= 1;
            }
            not = 1;
#if	0
         /*
          * Sep 16, 1991: KGB, skip unknown directives.
          */
         } else {
            fprintf( stderr, "Xfile -- unknown statement on line #%d\n", lineno );
            status = EXIT_FAILURE;
#endif
         }
      }
      if (status == EXIT_FAILURE) break;
   } while (not || (if_curl && if_code != if_mask));
   return( nc );
}


static	int	checkLine( char line[], int nchar, char *outflag, char **name )
/*
 *  Function to classify an input line.
 *  The function value indicates the line class.
 *
 *  Input arguments:
 *    line []    --   pointer to the input line
 *    nchar      --   length of the input line
 *
 *  Output arguments:
 *    outflag    --   flag character
 *    name       --   pointer to the new file name
 */
{
   char	head[4];
   char	*filename;

   if (nchar<0) return(ENDFILE);               /* physical end of input file */
   if (nchar<2) return(NEXTLINE);           /* short lines cannot be special */
   (void)strncpy(head,line,3); head[3] = '\0';
   if (strstrX(head,"#<")) return(ENDFILE);           /* internal end of file */
   if (strstrX(head,"#>")) {                      /* start of new output file */
      filename = strstrX(line,"#>");
      if (filename==line)
	 *outflag = 0;
      else
	 *outflag = line[0];
      *name = filename+2;
      return(NEXTFILE);
   } else {
      return(NEXTLINE);
   }
}


static	FILE	*openFile( char *name )
{
   FILE *file = NULL;
   char filename[MAXLINE];
   int  s, f, n = 0;

   strcpy(filename,dir);                     /* put in the directory first */
   s = f = strlen(filename);                   /* length of directory name */
   while (name[n] == ' ' || name[n] == 9) n++;       /* skip leading chars */
   while ((name[n]) && (name[n] != ' ')) filename[f++] = name[n++];
   filename[f++] = 0;
   if (!match(&filename[s])) return(file);                     /* no match */
   file = fopen(filename,"w");
   if (file == NULL) {
      fprintf(stderr,"Xfile -- cannot open %s\n",filename);
      status = EXIT_FAILURE;
   } else printf("%s\n",filename);
   return(file);
}


static	void	writeLine( FILE *file, char flag, char line[] )
{
   int start = -1;

   if (file) {
      if (flag) {
         if (line[0]==flag) {
            start = 1;
         }
      } else start = 0;
      if (start>=0) fprintf(file,"%s\n",&line[start]);
   }
}


static	void	closeFile( FILE *file )
{
   if (file) (void) fclose(file);
}


static	void	extract( FILE *arg_file, char arg_flag )
/*
 *  Recursive procedure to allow for nested files.
 */
{
   FILE *next_file;
   char next_flag;

   while (status == EXIT_SUCCESS) {
      nchar = getLine(line);                           /* obtain input line */
      code = checkLine(line,nchar,&next_flag,&name);        /* classify line */
      switch (code) {
         case NEXTLINE:
            writeLine(arg_file,arg_flag,line);       /* line in current file */
            break;
         case ENDFILE:
            closeFile(arg_file);                      /* end of current file */
            return;
         case NEXTFILE:
            next_file = openFile(name);                          /* new file */
            extract(next_file,next_flag);                  /* recursive call */
            break;
         default:
            fprintf(stderr,"Xfile -- internal error\n");
            status = EXIT_FAILURE;
      }
   }
}


int main( int argc, char **argv )
{
   inidef( );
   if (argc == 1) {
      int	n;

      printf("Usage: %s <fn> -d<sym> -u<sym> -p<path> -n<name> -w<wc>\n",argv[0]);
      printf("       <fn>       file from which to extract embedded files\n");
      printf("       <sym>      symbol to (un)define\n");
      printf("       <path>     path where to put extracted files\n");
      printf("       <name>     only file names matching name will be extracted\n");
      printf("                  Wildcards are allowed. Default extract all files\n");
      printf("       <wc>       sets wildcard in <name>. Default is *\n");
      printf("\n");
      printf("                  Defined are:");
      for (n = 0; n < ndefs; n++) {
         if (n) {
            printf(", %s", defs[n].s );
         } else {
            printf(" %s", defs[n].s );
         }
      }
      printf("\n");
      exit(EXIT_SUCCESS);
   } else {
      int	na;

      for (na = 1; na < argc; na++) {
         if (argv[na][0] == '-') switch(argv[na][1]) {
            case 'd':
            case 'D': {
               adddef( &argv[na][2], 1 );
               break;
            }
            case 'n':
            case 'N': {
               int a = 2, t = 0;
               while ((mask[t++] = argv[na][a++]));
               break;
            }
            case 'p':
            case 'P': {
               int a = 2, d = 0;
               while ((dir[d++] = argv[na][a++]));
               break;
            }
            case 'u':
            case 'U': {
               adddef( &argv[na][2], 0 );
               break;
            }
            case 'w':
            case 'W': {
               wild = argv[na][2];
               break;
            }
            default:
               fprintf(stderr,"Xfile -- unknown switch %s\n",argv[na]);
               exit(EXIT_FAILURE);
         } else if (infile) {
            fprintf(stderr,"Xfile -- can only Xtract from ONE file\n");
            exit(EXIT_FAILURE);
         } else if (!(infile = fopen(argv[na],"r"))) {
            fprintf(stderr,"Xfile -- cannot open %s\n",argv[na]);
            exit(EXIT_FAILURE);
         }
      }
   }
   if (infile == NULL) {
      fprintf( stderr, "Xfile -- No input file given!\n" );
      status = EXIT_FAILURE;
   } else {
      extract(NULL,0);
   }
   if (if_curl && status == EXIT_SUCCESS) {
      fprintf( stderr, "Xfile -- %d ENDIF(s) missing at line %d\n", if_curl, lineno );
      status = EXIT_FAILURE;
   }
   if (infile != NULL) {
      fclose(infile);
   }
   exit(status);
   return(status);
}
