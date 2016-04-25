%.f: %.shl
	$(top_srcdir)/exe/apple_i64/sheltran $<

%_ctof.c: %.shl
	touch $@ # just in case it does not get created
	$(top_srcdir)/exe/apple_i64/f2cvv $<

%_ftoc.c: %.c
	touch $@ # just in case it does not get created
	$(top_srcdir)/exe/apple_i64/f2cvv $<



AM_CFLAGS=-Wall -ggdb2 -iquote$(top_srcdir)/inc
AM_FFLAGS= -fno-backslash -fsecond-underscore
