include $(top_srcdir)/common.mk
include $(top_srcdir)/tasks.mk


bin_PROGRAMS = gdsserver
gdsserver_SOURCES= acceptor.c alloc.c clientmgr.c file.c inetsocket.c \
          linkedlist.c mainloop.c primes.c request.c server.c setmgr.c \
          unixsocket.c descr.c

$(gdsserver_SOURCES): ../gdsserver.src
	$(top_srcdir)/exe/$(SYSTEM)/xfile $<
