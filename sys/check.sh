# check.sh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992, 1993
#       All Rights Reserved.
#
#
# First check the functions
#
\rm -f check.awk
cat > check.awk << @EOF
BEGIN{
   char a[32];
   p[32];
   h = 0;
   i = 0;
   FS = ":";
}
{
   if ( i == 0 && NF > 4 ) {
      j = 1;
      for ( i = 0; i++ < NF; ) {
         a[i] = \$i;
         p[i] = j;
         j *= 2 ;
      }
   } else { 
      if( i && NF == 5 && \$5 != 0 && \$2 == "sub" ) {
         if ( h == 0 ) {
            h = 1;
            printf( "Function              Architectures\n" );
         }
         m = \$5;
         printf( "%-20s", \$1 );
         for ( j=i; j ; j-- ){
            if ( m >= p[j] ) { 
               printf( " %s", a[j] );
               m -= p[j];
            }
         }
         printf( "\n" );
      }
   }
}
@EOF
awk -f check.awk ${gip_sys}/bookkeeper
\rm -f check.awk
#
# Second check the applications
#
cat > check.awk << @EOF
BEGIN{
   char a[32];
   p[32];
   h = 0;
   i = 0;
   FS = ":";
}
{
   if ( i == 0 && NF > 4 ) {
      j = 1;
      for ( i = 0 ; i++ < NF; ) {
         a[i] = \$i;
         p[i] = j;
         j *= 2;
      }
   } else {
      if ( i && NF == 5 && \$5 != 0 && \$2 == "tsk" ) {
         if ( h == 0 ) {
            h = 1;
            printf( "Application           Architectures\n" );
         }
         m = \$5;
         printf( "%-20s", \$1 );
         for ( j = i; j ; j-- ) {
            if ( m >= p[j] ) {
               printf( " %s", a[j] );
               m -= p[j];
            }
         }
         printf( "\n" );
      }
   }
}
@EOF
awk -f check.awk ${gip_sys}/bookkeeper
\rm -f check.awk
