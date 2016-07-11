include $(top_srcdir)/common.mk
include $(top_srcdir)/tasks.mk
SUBDIRS=hermes gdsserver gdsserver64
bin_PROGRAMS = create print
create_SOURCES = create.f
print_SOURCES = dummy.f print.c

#LINKER=f77
#AM_LINK=bla $(LINKER) $(CFLAGS) $(LDFLAGS) -o $@


create.f: create.shl


#AM_CFLAGS=-Wall -ggdb2 -iquote$(top_srcdir)/inc
#AM_LDFLAGS=-ggdb2 $(top_srcdir)/sub/libgipsy.a
#AM_FFLAGS= -fno-backslash -fsecond-underscore

#LDFLAGS=-ggdb3 $(top_srcdir)/sub/libgipsy.a
#LDADD=$(top_srcdir)/sub/libgipsy.a
