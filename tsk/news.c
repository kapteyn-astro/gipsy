/* news.c
 
        Copyright (c) Kapteyn Laboratorium Groningen 1994
                All Rights Reserved.
                 
#>            news.dc1
 
Programme:    NEWS
 
Purpose:      Displays the GIPSY news items.
 
Category:     UTILITY
 
File:         news.c
 
Author:       K.G. Begeman
 
Keywords:
 

** OPTION=    Enter option (one of: [NEW] LIST READ).
              Option NEW lists all unread news items and prompts with
              keyword ITEM= to allow the user to select an unread news item.
              If no unread items are present, nothing happens.
              Option LIST lists all unread and read items and prompts with
              keyword ITEM=. If there are no items, nothing happens.
              Option READ does the same as list, but without a listing of
              news items.

   ITEM=      You can enter the item by name or number [First item]. This
              keyword is repeated until an unknown item is entered.

Updates:      May 30, 1992: KGB Document created.
              Dec 22, 1994: KGB New version.
              Feb 20, 2009: JPT Now also runs Python package/module check.
 
#<
 
*/

#include	"stddef.h"		/* <stddef.h> */
#include	"stdio.h"		/* <stdio.h> */
#include	"stdlib.h"		/* <stdlib.h> */
#include	"string.h"		/* <string.h> */
#include	"xscanf.h"		/* scans files */
#include	"gipsyc.h"		/* GIPSY definitions */
#include	"cmain.h"		/* main program in C */
#include	"cancel.h"		/* defines cancel_c */
#include	"finis.h"		/* defines finis_c */
#include	"init.h"		/* defines init_c */
#include	"match.h"		/* defines match_c */
#include	"nelc.h"		/* defines nelc_c */
#include	"userfio.h"		/* user I/O stuff */
#include	"deputy.h"		/* deputy task */

#define	ALLOC_INC	100		/* increase size by this amount */
#define	M_DEL		4		/* has been deleted */
#define	M_NUL		1		/* has not been delete, or read */
#define	M_READ		2		/* has been read */

typedef	struct {
   char	item[FILENAME_MAX];		/* item name */
   int	vers;				/* version number */
   int	mode;				/* what ? */
   int	done;				/* alread shown in this session */
} NEWS;

static	NEWS	*list = NULL;		/* our list */

static	char	news_file[FILENAME_MAX+1];	/* user file */
static	int	change = 0;		/* modify user file */
static	int	listsize = 0;		/* size of list */
static	int	nlist = 0;		/* number of items in list */

/*
 * compar is used by qsort. Sorting order: unread, read, deleted. Same
 * mode, then alphabetic.
 */

static	int	compar( const void *a1, const void *a2 )
{
   const NEWS	*b1 = a1;
   const NEWS	*b2 = a2;

   if ( b1->mode == b2->mode ) {
      return( strcmp( b1->item, b2->item ) );
   } else {
      return( b1->mode - b2->mode );
   }
}

/*
 * sort_list sorts the list of news items. See compar.
 */

static	void	sort_list( void )
{
   if ( nlist == 0 ) return;
   qsort( list, nlist, sizeof( NEWS ), compar );
}

/*
 * make_list extracts the list from $gip_sys/history.
 */

static	void	make_list( void )
{
   FILE	*hf;
   char	*gip_sys;
   char	history_file[FILENAME_MAX];
   char	h_date[FILENAME_MAX], h_name[FILENAME_MAX], h_ownr[FILENAME_MAX];
   char	h_type[FILENAME_MAX], h_user[FILENAME_MAX];
   int	h_size, h_sumc, h_vers;

   gip_sys = getenv( "gip_sys" );
   if ( gip_sys == NULL ) errorf( FATAL, "gip_sys not defined" );
   sprintf( history_file, "%s/history", gip_sys );
   hf = fopen( history_file, "r" );
   if ( hf == NULL ) errorf( FATAL, "cannot open %s!", history_file );
   while ( xscanf( hf, "%s %s %d %s %s %s %d %d", h_name, h_type,
         &h_vers, h_date, h_user, h_ownr, &h_size, &h_sumc ) >= 6 ) {
      if ( !strcmp( h_type, "mis" ) ) {
         char	*ext = strrchr( h_name, '.' );

         if ( ( ext != NULL ) && !strcmp( ext, ".nws" ) ) {
            int	n = 0;

            *ext = 0;
            while ( ( n < nlist ) && strcmp( h_name, list[n].item ) ) n++;
            if ( n == nlist ) {
               if ( nlist == listsize ) {
                  listsize += ALLOC_INC;
                  list = realloc( list, listsize * sizeof( NEWS ) );
                  if ( list == NULL ) {
                     errorf( FATAL, "cannot allocate enough memory" );
                  }
               }
               strcpy( list[n].item, h_name );
               list[n].vers = h_vers;
               if ( strcmp( h_ownr, "WASTEBASKET" ) ) {
                  list[n].mode = M_NUL;
               } else {
                  list[n].mode = M_DEL;
               }
               list[n].done = 0;
               nlist += 1;
            } else {
               list[n].vers = h_vers;
               if ( strcmp( h_ownr, "WASTEBASKET" ) ) {
                  list[n].mode = M_NUL;
               } else {
                  list[n].mode = M_DEL;
               }
            }
         }
      }
   }
   fclose( hf );
}

/*
 * init_news sets up the news item list and compares it with the user list.
 */

static	void	init_news( void )
{
   FILE	*nf;
   char	*dir;
   char	*num;

   make_list( );
   dir = getenv( "usr_root" );
   if ( dir == NULL ) dir = getenv( "HOME" );
   if ( dir == NULL ) dir = getenv( "home" );
   if ( dir == NULL ) dir = ".";
   num = getenv( "SESSION_ID" );
   if ( num == NULL ) num = "1";
   sprintf( news_file, "%s/.news.%s", dir, num );
   nf = fopen( news_file, "r" );
   if ( nf != NULL ) {
      char	n_item[FILENAME_MAX];
      int	n_vers;

      while ( xscanf( nf, "%s %d", n_item, &n_vers ) == 2 ) {
         int	i = 0;

         while ( ( i < nlist ) && strcmp( list[i].item, n_item ) ) i++;
         if ( i < nlist ) {
            if ( list[i].mode != M_DEL && list[i].vers == n_vers ) {
               list[i].mode = M_READ;
            }
         }
      }
      fclose( nf );
   }
   sort_list( );
   while ( nlist && ( list[nlist-1].mode == M_DEL ) ) nlist--;
}

/*
 * show_item displays a news item.
 */

static	void	show_item( int n )
{
   FILE	*nf;
   char	filename[FILENAME_MAX];
   char	*gip_mis;
   char	line[1024];
   
   if ( ( n < 0 ) || ( n >= nlist ) || ( list[n].mode == M_DEL ) ) {
      return;
   }
   gip_mis = getenv( "gip_mis" );
   if ( gip_mis == NULL ) errorf( FATAL, "gip_mis not defined!" );
   sprintf( filename, "%s/%s.nws", gip_mis, list[n].item );
   nf = fopen( filename, "r" );
   if ( nf == NULL ) {
      errorf( WARNING, "Cannot open %s!", filename );
      return;
   }
   anyoutf( 1, "++++++++++ NEWS item %s ++++++++++", list[n].item );
   while ( fgets( line, sizeof( line ), nf ) != NULL ) {
      int	l = strlen( line );

      if ( line[l-1] == '\n' ) line[--l] = 0;
      anyoutf( 1, line );
   }
   anyoutf( 1, "---------- NEWS item %s ----------", list[n].item );
   fclose( nf );
   list[n].done += 1;
   if ( list[n].mode == M_NUL ) {
      list[n].mode = M_READ;
      change += 1;
   }
}

/*
 * getnum decodes ascii digits into a number.
 */

static	int	getnum( char *s )
{
   int	r = 0;

   while ( isdigit( *s ) ) r = 10 * r + (*s++) - '0';
   return( *s ? 0 : r );
}

/*
 * do_item prompts the user for a news item.
 */

static	void	do_item( int total )
{
   int	done = 0;
   int	f = 0;
   int	n;

   while ( done < total ) {
      char	userb[21];
      fchar	user;

      user.a = userb; user.l = sizeof( userb ) - 1;
      while ( list[f].done ) f++;
      sprintf( userb, "%-*.*s", (int) user.l, (int) user.l, list[f].item ); 
      (void) userftext( user, 1, "ITEM=",
         "Enter item name/number, anything else to quit [%s]", list[f].item );
      cancel_c( tofchar( "ITEM=" ) );
      userb[nelc_c(user)] = 0;
      if ( !( n = getnum( userb ) ) ) {
         n = 0;
         while ( ( n < nlist ) && strcmp( list[n].item, userb ) ) n++;
      } else {
         n -= 1;
      }
      if ( ( n > -1 ) && ( n < nlist ) ) {
         show_item( n );
         if ( ( n < total ) && list[n].done ) done++;
      } else {
         return;
      }
   }
}

/*
 * new_news lists the unread news items. If there are any, it will prompt
 * the user for an item.
 */

static	void	new_news( void )
{
   int	n;
   int	u = 0;

   for ( n = 0; n < nlist; n++ ) {
      if ( list[n].mode == M_NUL ) {
         u++;
         anyoutf( 1, "N %3d  %s", n + 1, list[n].item );
      }
   }
   do_item( u );
}

/*
 * list_news lists all news items and prompts for one to show.
 */

static	void	list_news( void )
{
   int	n;
   int	t = 0;

   for ( n = 0; n < nlist; n++ ) {
      switch( list[n].mode ) {
         case M_NUL: {
            anyoutf( 1, "N %3d  %s", n + 1, list[n].item );
            t += 1;
            break;
         }
         case M_READ: {
            anyoutf( 1, "  %3d  %s", n + 1, list[n].item );
            t += 1;
            break;
         }
      }
   }
   do_item( t );
}

/*
 * read_news always prompts for a news item to display.
 */

static	void	read_news( void )
{
   do_item( nlist );
}

#define	OPT_LEN	5			/* max. chars. in option */

typedef struct {
   char	*name;				/* text */
   void	(*func)(void);			/* function */
} OPTS;

static	OPTS	options[] = {		/* the list of options */
   { "NEW" , new_news },
   { "LIST", list_news },
   { "READ", read_news },
};

#define	OPT_MAX	(sizeof( options ) / sizeof( OPTS ) )

/*
 * do_news gets the wanted option from the user.
 */

static	void	do_news( void )
{
   char		optsb[OPT_LEN*OPT_MAX];
   char		userb[OPT_LEN];
   fchar	opts;
   fchar	user;
   fint		maxopts = OPT_MAX;
   fint		option;
   int		n;

   opts.a = optsb; opts.l = OPT_LEN;
   user.a = userb; user.l = OPT_LEN;
   for ( n = 0; n < OPT_MAX; n++ ) {
      int	l = strlen( options[n].name );
      int	o = n * OPT_LEN;

      strncpy( &optsb[o], options[n].name, l > OPT_LEN ? OPT_LEN : l );
      if ( !n ) strncpy( userb, options[n].name, l > OPT_LEN ? OPT_LEN : l );
      while ( l < OPT_LEN ) {
         if ( !n ) userb[l] = ' ';
         optsb[o+l++] = ' ';
      }
   }
   (void) userftext( user, 2, "OPTION=", "Enter option (one of: [%s] %s %s)",
      options[0].name, options[1].name, options[2].name );
   option =  match_c( opts, &maxopts, user );
   if ( !option ) {
      anyoutf( 1, "UNKNOWN OPTION!" );
      return;
   }
   options[option-1].func( );
}

/*
 * exit_news write the modified user file (if modified).
 */

static	void	exit_news( void )
{
   if ( change ) {
      FILE	*n;
      int	i;

      n = fopen( news_file, "w" );
      if ( n == NULL ) errorf( FATAL, "Cannot update %s!", news_file );
      for ( i = 0; i < nlist; i++ ) {
         if ( list[i].mode == M_READ ) {
            fprintf( n, "%s:%d\n", list[i].item, list[i].vers );
         }
      }
      fclose( n );
   }
}

static void check_python( void )
{
   fint irc;
   deputy_c(tofchar("pycheck"), &irc);
   if (irc != 1) errorf(1, "Cannot run Python module/package check");
}

/*
 * THE PROGRAM
 */
         
MAIN_PROGRAM_ENTRY
{
   init_c( );
   check_python( );
   init_news( );
   do_news( );
   exit_news( );
   finis_c( );
   return( EXIT_SUCCESS );
}
