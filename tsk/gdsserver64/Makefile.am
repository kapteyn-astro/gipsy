include $(top_srcdir)/common.mk
include $(top_srcdir)/tasks.mk


bin_PROGRAMS = gdsserver64
gdsserver64_SOURCES= acceptor.c alloc.c clientmgr.c file.c inetsocket.c \
          linkedlist.c mainloop.c primes.c request.c server.c setmgr.c \
          unixsocket.c descr.c

$(gdsserver64_SOURCES): ../gdsserver64.src
	$(top_srcdir)/exe/$(SYSTEM)/xfile $<
