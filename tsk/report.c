/* report.c

        Copyright (c) Kapteyn Laboratorium Groningen 1991, 1997, 2000
        All Rights Reserved.

#>            report.dc1

Document:     report

Purpose:      reports a bug or a wish

Category:     UTILITY

File:         report.c

Author:       W. Zwitser

Keywords:


   SUBJECT=   name of a task (usually)
              A report may also be given about:
              - "general", if it concerns a general Gipsy item
              - a subroutine or document.


   SUMMARY=   a summary of the report ( max. 50 characters )             [quit]

   TYPE=      type of report                                              [BUG]
              BUG : if i.e. the program crashes or gives wrong results
              WISH: if it concerns i.e. a new feature

** FILE=      name of a prepared bug report

** EDIT=      Y: edit the prepared bug report                              [N]
              N: bypass the edit session
              (only if FILE=<name> was specified)


Use:          After the mentioned keywords the report is created in
              an edit session. The environment variable EDITOR is
              used to determine the editor. Just before the editor is
              started it gives the name of a file on /tmp which is used
              to create the report. This is not meant to change that name
              but to have the opportunity to abort with ^C.
              An error message about the editor generally means that
              you did not define the environment variable EDITOR.

Updates:      May  3, 1992: WZ  test version installed.
              May  4, 1992: WZ  version+date of subject added to report.
              Aug 24, 1993: JPT bitmap logic for fixes corrected.
              Nov 19, 1993: KGB bug in clearstr repaired.
              Apr 12, 1994: KGB address of GIPSY server now from server.mgr.
              May 12, 1997: KGB default address changed to new server.
              Jan 16, 1999: JPT modified for Linux 2.0.36
              Jun  7, 2000: JPT simplified; disabled bug listings
              Mar  9, 2001: JPT changed e-mail address
              Feb 11, 2004: JPT included subject in mail
              May  4, 2007: JPT included conditional code for Apple Mac.
#<
*/

/* report:     include files     */

#define  SIMPLIFIED 1

#include    "cmain.h"
#include    "ctype.h"
#include    "stdio.h"
#include    "stdlib.h"
#include    "stddef.h"
#if defined(__linux__) | defined(__APPLE__)
#include    <unistd.h>
#endif
#include    "string.h"
#include    "time.h"
#include    "xscanf.h"                                     /* code for xscanf */

#include    "anyout.h"
#include    "cancel.h"
#include    "editfile.h"
#include    "error.h"
#include    "gipsyc.h"
#include    "init.h"
#include    "finis.h"
#include    "flist.h"
#include    "nelc.h"
#include    "sortiai.h"
#include    "userchar.h"
#include    "userint.h"
#include    "userlog.h"
#include    "usertext.h"

#define     getpwuid       GETPWUID
#include    <pwd.h>
#undef      getpwuid
/* Malloc version of 'fmake'  */
#define finit( fc , len ) { fc.a = malloc( ( len + 1 ) * sizeof( char ) ) ;  \
                            fc.a[ len ] = '\0' ; \
                            fc.l = len ; }

/* report:     definitions    */

#define     KEY_LEN     20
#define     MAX_REPS    512
#define     MAX_CMD     255
#define     MAX_DATE    40
#define     MAX_LINE    255
#define     MAX_NAME    80
#define     MAX_SUBJECT 40
#define     MAX_SUMMARY 50
#define     MAX_TYPE    5
#define     MES_LEN     100
#define     RELEASE     "1.0"
#define     TEXT_LEN    255

#define     FATAL       0xffffffff                             /* fatal error */
#define     F_OK        0                           /* check presence of file */

#define  KEY_EDIT    tofchar("EDIT=")
#define  KEY_FIX     tofchar("FIX=")
#define  KEY_FIXNR   tofchar("FIXNR=")
#define  KEY_FILE    tofchar("FILE=")
#define  KEY_MODIFY  tofchar("MODIFY=")
#define  KEY_MODNR   tofchar("MODNR=")
#define  KEY_READNR  tofchar("READNR=")
#define  KEY_SUBJECT tofchar("SUBJECT=")
#define  KEY_SUMMARY tofchar("SUMMARY=")
#define  KEY_TYPE    tofchar("TYPE=")

#define  MES_EDIT    tofchar(" ")
#define  MES_FIX     tofchar(" ")
#define  MES_FILE    tofchar(" ")
#define  MES_FIXNR   tofchar("give number of report which has been fixed  [none]")
#define  MES_MODIFY  tofchar(" ")
#define  MES_MODNR   tofchar("give number of report to modify [none]")
#define  MES_READNR  tofchar("give number of report to read [none] ")
#define  MES_SUBJECT_BUG   tofchar("name of a task (usually)")
#define  MES_SUBJECT_FIX   tofchar("name of a task for which a bug has been fixed  [list all]")
#define  MES_SUBJECT_BUG_STOP   tofchar("name of a task (usually)  [quit]")
#define  MES_SUBJECT_FIX_STOP   tofchar("name of a task for which a bug has been fixed  [quit]")
#define  MES_SUMMARY       tofchar("give a summary of the problem (max.50 characters) [quit]")
#define  MES_TYPE    tofchar("type of report [BUG]")

#if !defined(__linux__) & !defined(__APPLE__)
extern   int   access( char *path, int amode );
#endif

/* report:   variables   */

static   char     date[MAX_DATE];
static   char     *extension = ".rep";
static   char     *fix_begin = "Gipsy_Fix_Report_Begin";
static   char     *fix_end   = "Gipsy_Fix_Report_End";
static   char     *mod_begin = "Gipsy_Mod_Report_Begin";
static   char     *rep_begin = "Gipsy_Report_Begin";
static   char     *rep_end   = "Gipsy_Report_End";
static   char     gip_doc[FILENAME_MAX];
static   char     gip_mis[FILENAME_MAX];
static   char     gip_sub[FILENAME_MAX];
static   char     gip_tsk[FILENAME_MAX];
static   char     edit_file[FILENAME_MAX];
static   fchar    edit_file_f = { edit_file, FILENAME_MAX };
static   fchar    Key, Mes;
static   char     inst_date[MAX_DATE];
static   char     mail_file[FILENAME_MAX];
static   char     subject[MAX_SUBJECT];
static   fchar    subject_f = { subject, MAX_SUBJECT };
static   char     *subject_label = "Subject#";
static   char     *summary_label = "Summary#";
static   char     text[MES_LEN];
static   fchar    text_f = { text, MES_LEN };
static   char     *version_label = "Version#";
static   fint     dev = 0;
static   fint     fatal = 4;
static   fint     deflt = 1;
static   fint     hidden = 2;
static   fint     lsubject = 0;
static   fint     nreps = 0;
static   int      level;
static   int      version;

#if !defined(__linux__) & !defined(__APPLE__)
extern  uid_t           getuid( void );
#endif

typedef  struct   {
   char        subject[MAX_SUBJECT];
   int         level;
   int         nreports;
   int         fixed[10];      /* a maximum of 320 reports/subject possible */
} rep_struct;

static   rep_struct     *rep = NULL;

/*
 *   clearstr fills a string with l blanks
 */

static   void  clearstr(   fchar str,
                           fint  l  )
{
   int i;

   for (i = 0; i < (int) l; str.a[i++] = ' ' );
   str.a[l-1] = '\0';
}

/*
 * getuser obtains the name of the user.
 */

static  void    getuser( char *username )
{
   char *getlogin( );                           /* gets login name */
   char *uname;                                 /* points to login name */

   uname = getlogin( );                         /* obtain login name */
   if (uname == NULL) {                         /* no success */
      struct passwd     *pw, *getpwuid( );      /* from /etc/passwd */

      pw = getpwuid( getuid( ) );               /* get pw struct */
      uname = pw->pw_name;                      /* get login name */
   }
   strcpy( username, uname );                   /* copy user name */
}


/*
 * compare sorts reports at 2 "columns":
 * - level
 * - within level at subject
 */

static   int   compare( const void *a1, const void *a2 )
{
   const rep_struct  *b1 = a1;
   const rep_struct  *b2 = a2;
   int   level = b1->level - b2->level;
   return( level == 0 ) ? strcmp( b1->subject, b2->subject ) : level;
}



/*
 * docmd does the unix command. It returns FATAL when the command could
 * not be executed, otherwize the status of the command is returned.
 */

static  int     docmd( char *cmd )
{
   int  r;                                      /* return from call to system */

   r = system( cmd );                           /* system call */
   return( r );                                 /* return to caller */
}


/*
 *  mail_report sends the report file to the "gipsy" mailbox
 */

static   int  mail_report( )
{
   char     cmd[MAX_CMD];
   char     gipsy_root[MAX_NAME];
   char     gipsy_mail[MAX_NAME];
   char     serv_file[FILENAME_MAX];
   char     gipsy_serv[MAX_NAME];
   FILE     *fp_serv;
   int      r = 0;

   sprintf( serv_file, "%s/server", getenv( "gip_loc" ) );
   fp_serv = fopen( serv_file, "r" );
   if ( fp_serv == NULL ) {
      sprintf( serv_file, "%s/server.mgr", getenv( "gip_sys" ) );
      fp_serv = fopen( serv_file, "r" );
   }
   if ( fp_serv != NULL ) {
      r = xscanf( fp_serv, "%s %s %s", gipsy_serv, gipsy_mail, gipsy_root );
   }
   if ( r != 3 ) {
      strcpy( gipsy_mail, "gipsy@astro.rug.nl" );
   }
   if ( fp_serv != NULL ) fclose( fp_serv );
   sprintf( cmd, "mail -s \"GIPSY BUG REPORT\" %s < %s", gipsy_mail, mail_file );
   return( docmd( cmd ) );
}

/*
 *   rep_scanf scans the first line of a bug report
 */

static   void  rep_scanf(  char  *line,
                           int   *level,
                           int   *nr,
                           char  *type,
                           char  *date_send,
                           char  *date_fix,
                           char  *user_mail_addr,
                           char  *resp_mail_addr   )
{
   char  string[5];
   int   it = 0, iline = 0;
   int   item = 0;

   date_fix[0] = '\0';
   while( line[iline] != '\n' ) {
      if ( line[iline] == '#' ) {
         int   l = iline - it;

         switch( item ) {
            case 0: {                            /* label: Gipsy_Report_Begin */
               break;
            }
            case 1: {                                  /* documentation level */
               strncpy( string, line+it, l );
               string[l] = '\0';
               sscanf( string, "%d", level );
               break;
            }
            case 2: {                                        /* report number */
               strncpy( string, line+it, l );
               string[l] = '\0';
               sscanf( string, "%d", nr );
               break;
            }
            case 3: {                                                 /* type */
               strncpy( type, line+it, l );
               type[l] = '\0';
               break;
            }
            case 4: {                                            /* date send */
               strncpy( date_send, line+it, l );
               date_send[l] = '\0';
               break;
            }
            case 5: {                                           /* date fixed */
               strncpy( date_fix, line+it, l );
               date_fix[l] = '\0';
               break;
            }
            case 6: {                                    /* user mail address */
               strncpy( user_mail_addr, line+it, l );
               user_mail_addr[l] = '\0';
               break;
            }
            case 7: {                                   /* responsible person */
               strncpy( resp_mail_addr, line+it, l );
               resp_mail_addr[l] = '\0';
               break;
            }
         }
         it = iline + 1;
         item++;
      }
      iline++;
   }
}

/*
 *  read_report reads a report from a ".rep"-file and prints the header
 *              information in the screen/log file. If contents == 1 the
 *              report itself is printed too.
 *              It returns 1 if a report was found, otherwise 0.
 */

static   int   read_report(   FILE  *fp_rep,
                              int   irep,
                              fint  report,
                              int   contents )
{
   char     date_fix[MAX_DATE];
   char     date_send[MAX_DATE];
   char     resp_mail_addr[MAX_NAME];
   char     user_mail_addr[MAX_NAME];
   char     summary[MAX_SUMMARY];
   char     line[MAX_LINE];
   char     type[MAX_TYPE];
   int      level, nr, ok = 0;


#if SIMPLIFIED
   return 0;
#endif
   while( fgets( line, MAX_LINE, fp_rep ) ) {
      if( strstr( line, rep_begin ) != NULL ) {         /* header information */
         rep_scanf( line, &level, &nr, type, date_send, date_fix, user_mail_addr, resp_mail_addr );
         if( nr == report || report == 0 ) ok = 1;
         if( ok ) {
            sprintf( text, "%4d %4s %s %-50.50s %13s\n", nr, type, "from", user_mail_addr, date_send );
            anyout_c( &dev,text_f );
            if( strlen( date_fix ) ) {
               int   nbits = sizeof( rep[irep].fixed[0] ) * 8;
               int   ilong = (report-1) / nbits;
               int   ibit  = (report-1) % nbits;

               rep[irep].fixed[ilong] |= (1<<ibit);   /* set fixed bit in rep */
               sprintf( text, "%14s %-50.50s %13s\n", "fixed by", resp_mail_addr, date_fix );
               anyout_c( &dev, text_f );
            } else {
               sprintf( text, "%14s %-50.50s %13s\n", "sent to", resp_mail_addr, "not yet fixed" );
               anyout_c( &dev, text_f );
            }
         }
      } else if( ( strstr( line, rep_end ) != NULL ) && ok ) {  /* end report */
         break;
      } else if( ( strstr( line, summary_label ) != NULL ) && ok ) {
         strcpy( summary, line + strlen( summary_label ) );
         summary[strlen( summary ) - 1] = '\0';
         if( contents ) {
            sprintf( text, "\"%s\"\n", summary );
         } else {
            sprintf( text, "%14s\"%s\"\n", " ", summary );
         }
         anyout_c( &dev, text_f );
      } else if( ( strstr( line, version_label ) != NULL ) && contents && ok ) {
         strcpy( text, line + strlen( version_label ) );
         anyout_c( &dev, text_f );
      } else if( contents && ok ) {                        /* report contents */
         anyout_c( &dev, tofchar( line ) );
      }
   }
   return( ok );
}

/*
 * known_reps gives a listing of known reports.
 * It is called once (with subject) or twice (a listing of all bugs first):
 * - allocate a struct for each subject
 * - sort all structs
 * - print header information + summary of each subject
 */

static   void  known_reps( int   all_listed )
{
   char     line[MAX_LINE];
   char     rep_file[FILENAME_MAX];
   fchar    rep_file_f;
   FILE     *fp_rep;
   fchar    line_f;
   fint     nitems = 1;
   int      contents = 1;
   int      i, irep;

#if SIMPLIFIED
   return;
#endif
   line_f.a = line;
   line_f.l = MAX_LINE;
   if( !all_listed ) {
      while( flist_c( tofchar( gip_mis ), line_f ) == 0 && nreps < MAX_REPS ) {
         char  *k = strstr( line, extension );

         if( k != NULL ) {
            int   l_ext  = strlen( extension );
            int   l_line = nelc_c( line_f );
            int   l_name = k - line;  /* length of rep name without extension */

            if( ( l_line - l_name ) == l_ext && ( ( strstr( line, subject ) == line ) || lsubject == 0 ) ) {
               char     doc_level[2];
                                       /* store subject + level in rep struct */
               rep = realloc( rep, sizeof( rep_struct ) * ( nreps + 1 ) );
               strncpy( rep[nreps].subject, line, l_name );
               rep[nreps].subject[l_name] = '\0';
               sprintf( rep_file, "%s/%s%s", gip_mis, rep[nreps].subject, extension );
               fp_rep = fopen( rep_file, "r" );
               while( ( fgets( line, MAX_LINE, fp_rep ) != NULL ) && ( strstr( line, rep_begin ) != line ) ) {
                  int   lbegin = strlen( rep_begin );

                  doc_level[0] = line[lbegin + 1];
                  doc_level[1] = '\0';
                  sscanf( doc_level, "%d", &rep[nreps].level );
               }
               rep[nreps].nreports = 0;
               for( i = 0; i < 10; i++ ) rep[nreps].fixed[i] = 0;
               fclose( fp_rep );
               nreps++;
            }
         }
      }
   }

   if( nreps ) {
      rep_file_f.a = rep_file;
      rep_file_f.l = FILENAME_MAX;
      if( !all_listed ) {
         int   n;

         qsort( rep, nreps, sizeof( rep_struct ), compare );
         anyout_c( &dev, tofchar( "subject/nr" ) );
         anyout_c( &dev, tofchar( "----------" ) );
         for( irep = 0; irep < nreps; irep++ ) {
            fint  report = 0;

            anyout_c( &dev, tofchar( rep[irep].subject ) );
            sprintf( rep_file, "%s/%s%s", gip_mis, rep[irep].subject, extension );
            fp_rep = fopen( rep_file, "r" );
            while( n = read_report( fp_rep, irep, ++report, !contents ) ) {
               rep[irep].nreports++;
            }
            fclose( fp_rep );
         }
      }

      if( lsubject ) {                             /* read reports one by one */
         fint  report;

         for( irep = 0; irep < nreps; irep++ ) {
            if( !strcmp( rep[irep].subject, subject ) ) {
               sprintf( rep_file, "%s/%s%s", gip_mis, subject, extension );
               fp_rep = fopen( rep_file, "r" );
               Key = KEY_READNR;
               Mes = MES_READNR;
               while( userint_c( &report,
                                 &nitems,
                                 &deflt,
                                 Key,
                                 Mes  ) ) {
                  cancel_c( Key );
                  fseek( fp_rep, 0, SEEK_SET );
                  if( !read_report( fp_rep, irep, report, contents ) ) {
                     sprintf( text, "report nr %d for \"%s\" not found", report, subject );
                     anyout_c( &dev, text_f );
                  }
               }
               cancel_c( Key );
               fclose( fp_rep );
            }
         }
      }
   } else if( !all_listed ) {
      if( lsubject ) {
         sprintf( text, "no reports known about \"%s\"", subject );
         anyout_c( &dev, text_f );
      } else {
         anyout_c( &dev, tofchar( "no reports received at the moment" ) );
      }
   }
}

/*
 * new_rep mails a new bug report to the Gipsy manager
 */

static   void  new_rep( )
{
   char     summary[MAX_SUMMARY];
   fchar    summary_f;
   char     type[MAX_TYPE];
   fchar    type_f;
   FILE     *fp_mail, *fp_edit = NULL;
   fint     ledit = 0, lsummary, ltype;
   int      i, r;

   summary_f.a = summary;
   summary_f.l = MAX_SUMMARY;
   clearstr( summary_f, MAX_SUMMARY );
   Key = KEY_SUMMARY;
   Mes = MES_SUMMARY;
   if( lsummary = usertext_c( summary_f,
                              &deflt,
                              Key,
                              Mes ) ) {
      bool     edit = TRUE;
      fint     edit_ok;
      fint     nitems = 1;

      type_f.a = type;
      type_f.l = MAX_TYPE;
      strcpy( type, "BUG" );
      Key = KEY_TYPE;
      Mes = MES_TYPE;
      if( ltype = usertext_c( type_f,
                              &deflt,
                              Key,
                              Mes   ) ) {
         type[ltype] = '\0';
         for( i = 0; i < ltype; i++ ) type[i] = toupper( type[i] );
      }
      Key = KEY_FILE;
      Mes = MES_FILE;
      if( ledit = usertext_c( edit_file_f,
                              &hidden,
                              Key,
                              Mes   ) ) {
         edit_file[ledit] = '\0';
         Key = KEY_EDIT;
         Mes = MES_EDIT;
         userlog_c(  &edit,
                     &nitems,
                     &hidden,
                     Key,
                     Mes );
      }
      if( tobool( edit ) ) {
         sprintf( text, "abort with ^C  [ edit %s ]", edit_file );
         edit_ok = editfile_c( tofchar( edit_file ),
                               text_f );
         switch( edit_ok ) {
            case  0: {
               break;
            }
            case -1: {
               error_c( &fatal, tofchar( "edit cancelled" ) );
            }
            case -2: {
               error_c( &fatal, tofchar( "cannot start editor" ) );
            }
            case -3: {
               error_c( &fatal, tofchar( "editor process exited with error status" ) );
            }
            default: {
               error_c( &fatal, tofchar( "this is an old version of Hermes probably" ) );
            }
         }
      }
      fp_mail = fopen( mail_file, "w" );
      fp_edit = fopen( edit_file, "r" );
      if( fp_edit ) {
         char     line[MAX_LINE];

         if ( fp_mail == NULL ) {
            error_c( &fatal, tofchar( "could not open a report on /tmp" ) );
         }
         fprintf( fp_mail, "%s#%d#0#%s#%s####\n", rep_begin, level, type, date );
         fprintf( fp_mail, "%s%.*s\n", subject_label, lsubject, subject );
         if( version >= 0 ) {
            char     SUBJECT[MAX_SUBJECT];

            for( i = 0; i < lsubject; i++ ) SUBJECT[i] = toupper( subject[i] );
            SUBJECT[lsubject] = '\0';
            fprintf( fp_mail, "%s(%s Version %d, installed at %s)\n", version_label, SUBJECT, version, inst_date );
         }
         fprintf( fp_mail, "%s%.*s\n", summary_label, lsummary, summary );
         while( fgets( line, MAX_LINE, fp_edit ) != NULL ) fputs( line, fp_mail );
         fputs( rep_end, fp_mail );
         fclose( fp_mail );
         fclose( fp_edit );
         r = mail_report( );
      }
   }
   if( !access( mail_file, F_OK )           ) r = remove( mail_file );
   if( !access( edit_file, F_OK ) && !ledit ) r = remove( edit_file );
}

/*
 * mod_rep modifies an existing report
 */

static   void  mod_rep( )
{
   char     rep_file[FILENAME_MAX];
   FILE     *fp_mail, *fp_edit = NULL, *fp_rep;
   fint     report;
   fint     nitems = 1;
   int      irep;
   int      known_report = 0, r;

   for( irep = 0; irep < nreps; irep++ ) {
      if( !strcmp( rep[irep].subject, subject ) ) {
         Key = KEY_MODNR;
         Mes = MES_MODNR;
         while( !known_report ) {
            r = userint_c( &report,
                           &nitems,
                           &deflt,
                           Key,
                           Mes  );
            cancel_c( Key );
            if( r ) {
               if( report > rep[irep].nreports || report < 1 ) {
                  sprintf( text, "report nr %d for \"%.*s\" not found\n", report, lsubject, subject );
                  anyout_c( &dev, text_f );
               } else {
                  known_report = report;
               }
            } else {
               break;
            }
         }
      }
   }

   if( known_report ) {
      char     date_fix[MAX_DATE];
      char     date_send[MAX_DATE];
      char     resp_mail_addr[MAX_NAME];
      char     user_mail_addr[MAX_NAME];
      char     type[MAX_TYPE];
      char     line[MAX_LINE];
      fint     edit_ok;
      int      level, nr, ok = 0;

      sprintf( rep_file, "%s/%s%s", gip_mis, subject, extension );
      fp_rep  = fopen( rep_file, "r" );
      fp_edit = fopen( edit_file, "w" );
      while( fgets( line, MAX_LINE, fp_rep ) ) {
         if( strstr( line, rep_begin ) != NULL ) {
            rep_scanf( line, &level, &nr, type, date_send, date_fix, user_mail_addr, resp_mail_addr );
            if( nr == report ) {
               ok = 1;
               fprintf( fp_edit, "%s#%d#%d#%s#%s#%s#%s#%s#\n", mod_begin, level, nr, type, date_send, date_fix, user_mail_addr, resp_mail_addr );
            }
         } else if( ( strstr( line, rep_end ) != NULL ) && ok ) {
            fputs( line, fp_edit );
            break;
         } else if( ok ) {
            fputs( line, fp_edit );
         }
      }
      fclose( fp_edit );
      sprintf( text, "abort with ^C  [ edit %s ]", edit_file );
      edit_ok = editfile_c( tofchar( edit_file ),
                               text_f );
      fp_mail = fopen( mail_file, "w" );
      fp_edit = fopen( edit_file, "r" );
      if( fp_edit ) {
         char     line[MAX_LINE];

         if ( fp_mail == NULL ) {
            error_c( &fatal, tofchar( "could not open a report on /tmp" ) );
         }
         fgets( line, MAX_LINE, fp_edit );
         fputs( line, fp_mail );
         fprintf( fp_mail, "%s%s\n", subject_label, subject );
         while( fgets( line, MAX_LINE, fp_edit ) != NULL ) fputs( line, fp_mail );
         fclose( fp_mail );
         fclose( fp_edit );
         r = mail_report( );
      }
   }
   if( !access( mail_file, F_OK ) ) r = remove( mail_file );
   if( !access( edit_file, F_OK ) ) r = remove( edit_file );
}

/*
 * fix_rep mails the solution to the Gipsy manager
 */

static   void  fix_rep( )
{
   FILE     *fp_mail, *fp_edit = NULL;
   fint     report;
   fint     ledit = 0, nitems = 1;
   int      irep;
   int      known_report = 0, r;

   for( irep = 0; irep < nreps; irep++ ) {
      if( !strcmp( rep[irep].subject, subject ) ) {
         Key = KEY_FIXNR;
         Mes = MES_FIXNR;
         while( !known_report ) {
            r = userint_c( &report,
                           &nitems,
                           &deflt,
                           Key,
                           Mes  );
            cancel_c( Key );
            if( r ) {
               int   nbits = sizeof( rep[irep].fixed[0] ) * 8;
               int   ilong = (report-1) / nbits;
               int   ibit  = (report-1) % nbits;

               if( report > rep[irep].nreports || report < 1 ) {
                  sprintf( text, "report nr %d for \"%.*s\" not found\n", report, lsubject, subject );
                  anyout_c( &dev, text_f );
               } else {
                  if( rep[irep].fixed[ilong] & (1<<ibit) ) {
                     sprintf( text, "report nr %d for \"%.*s\" has already been fixed\n", report, lsubject, subject );
                     anyout_c( &dev, text_f );
                  } else {
                     known_report = report;
                  }
               }
            } else {
               break;
            }
         }
      }
   }

   if( known_report ) {
      bool	edit = TRUE;
      fint	edit_ok;

      Key = KEY_FILE;
      Mes = MES_FILE;
      if( ledit = usertext_c( edit_file_f,
                              &hidden,
                              Key,
                              Mes   ) ) {
         edit_file[ledit] = '\0';
         Key = KEY_EDIT;
         Mes = MES_EDIT;
         userlog_c(  &edit,
                     &nitems,
                     &hidden,
                     Key,
                     Mes );
      }
      if( tobool( edit ) ) {
         sprintf( text, "abort with ^C  [ edit %s ]", edit_file );
         edit_ok = editfile_c( tofchar( edit_file ),
                                        text_f );
      }
      fp_mail = fopen( mail_file, "w" );
      fp_edit = fopen( edit_file, "r" );
      if( fp_edit ) {
         char     line[MAX_LINE];

         if ( fp_mail == NULL ) {
            error_c( &fatal, tofchar( "could not open a report on /tmp" ) );
         }
         fprintf( fp_mail, "%s#0#%d#%s#####\n", fix_begin, report, date );
         fprintf( fp_mail, "%s%.*s\n", subject_label, lsubject, subject );
         while( fgets( line, MAX_LINE, fp_edit ) != NULL ) fputs( line, fp_mail );
         fputs( fix_end, fp_mail );
         fclose( fp_mail );
         fclose( fp_edit );
         r = mail_report( );
      }
   }
   if( !access( mail_file, F_OK )           ) r = remove( mail_file );
   if( !access( edit_file, F_OK ) && !ledit ) r = remove( edit_file );
}

/*
 * subject_ok checks whether the subject has a document or is "gipsy"
 */

static   int   subject_ok( )
{
   char     doc_file[FILENAME_MAX];
   char     *dir[4];
   char     *doc[]   = { ".doc",  ".dc1",  ".dc2",  ".dc3"  };
   char     line[MAX_LINE];
   char     source_file[FILENAME_MAX];
   fchar    line_f;
   FILE     *fp_doc = NULL;
   int      i, ok = 0;
   int      lline;

   dir[0] = gip_doc;
   dir[1] = gip_tsk;
   dir[2] = gip_sub;
   dir[3] = gip_sub;
   level   = -1;
   version = -1;
   line_f.a = line;
   line_f.l = MAX_LINE;
   if( lsubject ) {
      for( i = 0; i < 4; i++ ) {
         sprintf( doc_file, "%s/%.*s%s", dir[i], lsubject, subject, doc[i] );
         fp_doc = fopen( doc_file, "r" );
         if ( fp_doc ) {                                    /* document found */
            ok    = 1;
            level = i;                                        /* return level */
            while( flist_c( tofchar( dir[i] ), line_f ) == 0 ) {
               char  *dot = strstr( line, "." );
               if( dot != NULL ) {
                  int   lname = dot - line;
                  if( strstr( line, subject ) == line && strstr( line, doc[i] ) == NULL && lsubject == lname ) {
                     strcpy( source_file, line );
                     lline = nelc_c( line_f );
                     source_file[lline] = '\0';
                  }
               }
            }
         }
      }
      if( ok ) {
         char     his_date[MAX_NAME];
         char     his_file[FILENAME_MAX];
         char     his_name[MAX_NAME];
         char     his_owner[MAX_NAME];
         char     his_type[MAX_NAME];
         char     his_user[MAX_NAME];
         FILE     *fp_his = NULL;
         int      nf;
         int      size;
         int      vers;

         sprintf( his_file, "%s/history", getenv( "gip_sys" ) );
         fp_his = fopen( his_file, "r" );
         if( fp_his ) {
            while( ( nf = xscanf( fp_his, "%s %s %d %s %s %s %d", his_name, his_type, &vers, his_date, his_user, his_owner, &size ) ) == 7 ) {
               if( strstr( his_name, source_file ) != NULL && vers > version ) {
                  version = vers;
                  strcpy( inst_date, his_date );
               }
            }
            fclose( fp_his );
         }
      } else {
         ok = ( !strcmp( subject, "general" ) );
      }
      if( !ok ) {
         sprintf( text, "%s is unknown within Gipsy; report only about a documented feature", subject );
         anyout_c( &dev, text_f );
      }
   }
   return( ok );
}

MAIN_PROGRAM_ENTRY
{
   bool     fix, modify;
   fint     nitems = 1;
   int      i, all_listed;
   time_t   now = time( NULL );

   init_c( );
   IDENTIFICATION( "REPORT", RELEASE );
   strcpy( gip_doc, getenv( "gip_doc" ) );
   strcpy( gip_mis, getenv( "gip_mis" ) );
   strcpy( gip_sub, getenv( "gip_sub" ) );
   strcpy( gip_tsk, getenv( "gip_tsk" ) );
   sprintf( mail_file, "/tmp/report.%d", (int) now );
   sprintf( edit_file, "/tmp/edit.%d", (int) now );
   strftime( date, MAX_DATE, "%b %d, %Y", localtime( &now ) );

   fix = FALSE;
   modify = FALSE;
#if !SIMPLIFIED
   finit( Key, KEY_LEN );
   finit( Mes, MES_LEN );
   Key = KEY_FIX;
   Mes = MES_FIX;
   userlog_c(  &fix,
               &nitems,
               &hidden,
               Key,
               Mes );
   Key = KEY_MODIFY;
   Mes = MES_MODIFY;
   userlog_c(  &modify,
               &nitems,
               &hidden,
               Key,
               Mes );
#endif
   if( tobool( modify ) ) {
      char  *manager[] = { "kgb", "terlouw", "zwitser" };
      char  username[20];
      int   ok = 0;

      getuser( username );
      for( i = 0; i < 3; i++ ) if( !strcmp( username, manager[i] ) ) ok = 1;
      if( !ok ) {
         error_c( &fatal, tofchar( "you are not allowed to modify a report" ) );
      }
   }
   for( all_listed = 0; all_listed < 2 && !lsubject; all_listed++ ) {
      Key = KEY_SUBJECT;
      if( tobool(fix) ) {
         if( all_listed ) {
            Mes = MES_SUBJECT_FIX_STOP;
         } else {
            Mes = MES_SUBJECT_FIX;
         }
      } else {
         if( all_listed ) {
            Mes = MES_SUBJECT_BUG_STOP;
         } else {
            Mes = MES_SUBJECT_BUG;
         }
      }
      do {
         fint	nitems = 1;

         clearstr(subject_f, MAX_SUBJECT );
         lsubject = userchar_c(  subject_f,
                                 &nitems,
                                 &deflt,
                                 Key,
                                 Mes );
         if (lsubject) lsubject = nelc_c( subject_f );
         cancel_c( Key );
         for( i = 0; i < lsubject; i++ ) {
            subject[i] = tolower( subject[i] );
         }
         subject[lsubject] = '\0';
      } while ( !subject_ok( ) && lsubject );
#if SIMPLIFIED
      known_reps( all_listed );                      /* summary of known bugs */
#endif
   }
   if ( lsubject ) {
      if( tobool( modify ) ) {
         mod_rep( );                                       /* modify a report */
      } else if( tobool( fix ) ) {
         fix_rep( );                                          /* a fix report */
      } else {
         new_rep( );                                          /* a new report */
      }
   }
   finis_c( );
   return( 0 );
}
