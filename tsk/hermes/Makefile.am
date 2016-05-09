include $(top_srcdir)/common.mk
include $(top_srcdir)/tasks.mk
#AM_LDFLAGS=-ggdb2 $(top_srcdir)/sub/libgipsy.a
#LINKER=gcc
#AM_LINK=bla $(LINKER) $(CFLAGS) $(LDFLAGS) -o $@

bin_PROGRAMS = thermes
thermes_SOURCES=thermes.c mainloop.c keyboard.c screen.c taskmgr.c substi.c \
          parmgr.c prompter.c coamgr.c do_request.c getdtablesize.c \
          hermesipc.c put_log.c alloc.c parse.c altkeysock.c altkeyboard.c \
          execpath.c rdefaults.c changecase.c ushell.c bintree.c \
          contextmenu.c showkeys.c dcdrange.c printmgr.c \
          filecontents.c readcontext.c ucamgr.c help.c oshell.c findstring.c \
          tekwindow.c terminal.c minimatch.c aiduca.c monopoly.c editkeys.c \
          cmdstack.c editfile.c search.c helpkey.c khnodemgr.c keytext.c \
          parsekeyhelp.c taskoutput.c \
          dcderrstr.c dcdfile.c dcdgds.c dcdparse.c dcdscan.c stubs.c varmgr.c \
          showexpr.c calculator.c callback.c absname.c webhelp.c notifier.c

$(thermes_SOURCES): ../hermes.src
	$(top_srcdir)/exe/$(SYSTEM)/xfile $<
thermes_LDFLAGS=-lcurses
#LDFLAGS=-ggdb2 $(top_srcdir)/sub/libgipsy.a
#LDADD=$(top_srcdir)/sub/libgipsy.a
