#!/bin/sh
# findx.sh
#
#   Copyright (c) Kapteyn Laboratorium Groningen 1994, 1997
#   All Rights Reserved.
#
##>            findx.doc
#
#Script:       findx
#
#Purpose:      Searches for paths to the X11 include files and the
#              X11 library and when found produces the appropriate
#              compiler switches.
#
#Category:     SYSTEM
#
#File:         findx.sh
#
#Author:       K.G. Begeman
#
#Use:          findx.sh includes    produces the compiler switches to
#                                   include the X11 include files.
#              findx.sh library     produces the compiler switches to
#                                   link with the X11 library.
#              findx.sh libraries   produces the compiler switches to
#                                   link with X-Toolkit libraries.
#
#Updates:      Dec  5, 1994: KGB, Document created.
#              Dec  7, 1994: KGB, Revision for openwin.
#              Jul 29, 1997: KGB, search for X-Toolkit libraries.
#
##<
#
if [ "$1" = "includes" ] ; then
   xinc="
      $gip_lib
      /usr/X11R6.3/include
      /usr/X11R6.1/include
      /usr/X11R6/include
      /usr/X11R5/include
      /usr/lpp/X11/include
      /usr/include/X11R6
      /usr/include/X11R5
      /usr/include/X11R4
      /usr/openwin/include
      /usr/include
   "
   for i in $xinc; do
      if [ -r "$i/X11/X.h" ] ; then
         echo "-I$i -I$i/X11"
         exit
      fi
   done
elif [ "$1" = "library" ] ; then
   xlib="
      $gip_lib/libX11
      /usr/X11R6.3/lib/libX11
      /usr/X11R6.1/lib/libX11
      /usr/X11/lib/libX11
      /usr/X11R6/lib/libX11
      /usr/X11R5/lib/libX11
      /usr/lpp/X11/lib/R6/libX11
      /usr/lpp/X11/lib/R5/libX11
      /usr/lpp/X11/libX11
      /usr/lib/libX11-mit
      /usr/lib/X11R6/libX11
      /usr/lib/X11R5/libX11
      /usr/lib/X11R4/libX11
      /usr/openwin/lib/libX11
      /usr/lib/X11/libX11
      /usr/lib/libX11
   "
   for i in $xlib; do
      for j in $i.a $i.so* $i.sl; do
         if [ -r $j ] ; then
            d=`echo $i | awk -F/ '{for(i=1;i<NF;i++)printf("%s/",$i);}'`
            l=`echo $i | awk -F/ '{if(substr($NF,1,3)=="lib"){printf("%s",substr($NF,4,length($NF)-3))}}'`
            if [ "$d" = "/usr/lib/" ] ; then
               echo "-l$l"
            else
               echo "-L$d -l$l"
            fi
            exit
         fi
      done
   done
elif [ "$1" = "libraries" ] ; then
   xlib="
      $gip_lib
      /usr/X11R6.3/lib
      /usr/X11R6.1/lib
      /usr/X11/lib
      /usr/X11R6/lib
      /usr/X11R5/lib
      /usr/lpp/X11/lib/R6
      /usr/lpp/X11/lib/R5
      /usr/lpp/X11
      /usr/lib/X11R6
      /usr/lib/X11R5
      /usr/lib/X11R4
      /usr/openwin/lib
      /usr/lib/X11
      /usr/lib
   "
   for i in $xlib; do
      str=""
      c1="0"
      for j in $i/libXaw $i/libXmu $i/libXt $i/libXext $i/libSM $i/libICE $i/libX11; do
         c2="0"
         for k in $j.a $j.so* $j.sl; do
            if [ -r $k -a "$c2" = "0" ] ; then
               d=`echo $i`
               l=`echo $j | awk -F/ '{if(substr($NF,1,3)=="lib"){printf("%s",substr($NF,4,length($NF)-3))}}'`
               if [ "$c1" = "0" ] ; then
                  if [ "$d" = "/usr/lib" ] ; then
                     str="$str -l$l"
                  else
                     str="$str -L$d -l$l"
                  fi
               else
                  str="$str -l$l"
               fi
               c1=`echo $c1 | awk '{a=$1;print ++a}'`
               c2=`echo $c2 | awk '{a=$1;print ++a}'`
            fi
         done
      done
      if [ "$c1" = "5" -o "$c1" = "7" ] ; then
         echo $str
         exit
      fi
   done
else
   echo "Usage: $0 libraries | library | includes"
fi
