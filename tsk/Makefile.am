SUBDIRS=hermes gdsserver
bin_PROGRAMS = create
create_SOURCES = create.f


AM_CFLAGS=-Wall -ggdb2 -iquote$(top_srcdir)/inc
AM_LDFLAGS=-ggdb2 $(top_srcdir)/sub/libgipsy.a
LINKER=gfortran
AM_LINK=$(LINKER) $(CFLAGS) $(LDFLAGS) -o $@
AM_FFLAGS= -fno-backslash -fsecond-underscore
