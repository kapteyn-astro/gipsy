#!/bin/csh -f
# archreset.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992, 1993
#       All Rights Reserved.
#
#
##>            archreset.doc
#
#Script:       archreset
#
#Purpose:      Reset a specified architecture in the bookkeeper file
#              so that all sources will be rebuild.
#
#Category:     SYSTEM MANAGEMENT
#
#File:         archreset.csh
#
#Author:       K.G. Begeman
#
#Use:          $gip_sys/archreset.csh architecture ( for csh )
#              $gip_sys/archreset.sh  architecture ( for sh )
#
#Description:  A modified bookkeeper file (bookkeeper.new) will be
#              created.
#
#Updates:      Jun 20, 1992: KGB Document created.
#              Mar 23, 1993: KGB also for sh.
#
##<
#
if ( ${#argv} == 0 || ${#argv} > 1 ) then
	echo "usage: archreset.csh architecture"
	exit 0
endif
\rm -f archreset.awk
\cat > archreset.awk << @EOF
BEGIN{
   s = 0;
   t = 0;
}
{
   if ( substr(\$0,1,1) == "#" ) {
      printf( "%s\n", \$0 );
   } else if ( s == 0 ) {
      printf( "%s\n", \$0 );
      s = 1;
      for ( i = 1; i <= NF; i++) {
         if ( \$i == "$argv[1]" ) {
            t = s;
         }
         s = 2 * s;
      }
   } else if ( t ) {
      o = \$3;
      v = \$3;
      b = t + t;
      while ( v >= b ) {
         v -= b;
      }
      if ( v >= t ) {
         o -= t;
      }
      printf( "%s:%s:%d:%d:%d\n", \$1, \$2, o, \$4, \$5 );
   } else {
      printf( "%s\n", \$0 );
   }
}
@EOF
\rm -f bookkeeper.new
awk -F: -f archreset.awk $gip_sys/bookkeeper > bookkeeper.new
\rm -f archreset.awk
echo "bookkeeper.new created"
##>            archreset.sh
## archreset.sh
##
##       Copyright (c) Kapteyn Laboratorium Groningen 1993
##       All Rights Reserved.
##
#if [ "$1" = "" ] ; then
#	echo "usage: archreset.sh architecture"
#	exit 0
#fi
#if [ "$2" != "" ] ; then
#	echo "$0 architecture"
#	exit 0
#fi
#\rm -f archreset.awk
#\cat > archreset.awk << @EOF
#BEGIN{
#   s = 0;
#   t = 0;
#}
#{
#   if ( substr(\$0,1,1) == "#" ) {
#      printf( "%s\n", \$0 );
#   } else if ( s == 0 ) {
#      printf( "%s\n", \$0 );
#      s = 1;
#      for ( i = 1; i <= NF; i++) {
#         if ( \$i == "$1" ) {
#            t = s;
#         }
#         s = 2 * s;
#      }
#   } else if ( t ) {
#      o = \$3;
#      v = \$3;
#      b = t + t;
#      while ( v >= b ) {
#         v -= b;
#      }
#      if ( v >= t ) {
#         o -= t;
#      }
#      printf( "%s:%s:%d:%d:%d\n", \$1, \$2, o, \$4, \$5 );
#   } else {
#      printf( "%s\n", \$0 );
#   }
#}
#@EOF
#\rm -f bookkeeper.new
#awk -F: -f archreset.awk $gip_sys/bookkeeper > bookkeeper.new
#\rm -f archreset.awk
#echo "bookkeeper.new created"
##<
