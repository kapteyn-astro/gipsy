# archreset.sh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1993
#       All Rights Reserved.
#
if [ "$1" = "" ] ; then
	echo "usage: archreset.sh architecture"
	exit 0
fi
if [ "$2" != "" ] ; then
	echo "$0 architecture"
	exit 0
fi
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
         if ( \$i == "$1" ) {
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
