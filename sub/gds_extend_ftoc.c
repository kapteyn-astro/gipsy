/* ANSI F2C interface generated for F2C by program f2cvv */

#include <stdint.h>
#if     defined(__g77__) | defined(__F2C__) | defined(__linux__) | defined(__FreeBSD__) | defined(__APPLE__)
#if     defined(__APPLE__) && !defined(__x86_64__)
typedef long fint;
typedef long bool;
#else
typedef int fint;
typedef int bool;
#endif
typedef struct { char *a; fint  l; } fchar;
typedef struct { float r; float i; } complex;
typedef int64_t fint8;
#define TRUE            ( 1 )
#define FALSE           ( 0 )
#define tobool(l)       ( l ? 1 : 0 )
#define toflog(l)       ( l ? 1 : 0 )
#else
#endif

void gds_extend__(char *a1,
                  char *a2,
                  double *a3,
                  fint *a4,
                  fint *a5,
                  fint l1,
                  fint l2)
{
   void gds_extend_c(fchar,fchar,double *,fint *,fint *);
   fchar b1;
   fchar b2;
   b1.a = a1;
   b1.l = l1;
   b2.a = a2;
   b2.l = l2;
   gds_extend_c(b1,b2,a3,a4,a5);
}
