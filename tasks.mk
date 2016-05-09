#AM_LDFLAGS=-ggdb2 $(top_srcdir)/sub/libgipsy.a
#LINKER=gfortran
#AM_LINK=$(LINKER) $(CFLAGS) $(LDFLAGS) -o $@

# ugly way to do defaults, but AM_LDFLAGS doesnt work
CCLD=gfortran
LDFLAGS=-ggdb2 $(top_srcdir)/sub/libgipsy.a

LDADD = $(top_srcdir)/sub/libgipsy.a
