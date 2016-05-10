include $(top_srcdir)/common.mk
SUBDIRS=hermes gdsserver
bin_PROGRAMS = create print 
create_SOURCES = create.f
print_SOURCES = print.c


#AM_CFLAGS=-Wall -ggdb2 -iquote$(top_srcdir)/inc
#AM_LDFLAGS=-ggdb2 $(top_srcdir)/sub/libgipsy.a
LINKER=f77
AM_LINK=bla $(LINKER) $(CFLAGS) $(LDFLAGS) -o $@
#AM_FFLAGS= -fno-backslash -fsecond-underscore

#LDFLAGS=-ggdb3 $(top_srcdir)/sub/libgipsy.a
LDADD=$(top_srcdir)/sub/libgipsy.a