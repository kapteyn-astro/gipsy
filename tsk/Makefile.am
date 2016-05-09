include $(top_srcdir)/common.mk
SUBDIRS=hermes gdsserver
bin_PROGRAMS = create
create_SOURCES = create.f


#AM_CFLAGS=-Wall -ggdb2 -iquote$(top_srcdir)/inc
#AM_LDFLAGS=-ggdb2 $(top_srcdir)/sub/libgipsy.a
#LINKER=f77
#AM_LINK=$(LINKER) $(CFLAGS) $(LDFLAGS) -o $@
#AM_FFLAGS= -fno-backslash -fsecond-underscore

#LDFLAGS=-ggdb3 $(top_srcdir)/sub/libgipsy.a
LDADD=$(top_srcdir)/sub/libgipsy.a