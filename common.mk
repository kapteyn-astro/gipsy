

%.f: %.shl
	$(top_srcdir)/exe/$(SYSTEM)/sheltran $<

%_ctof.c: %.shl
	touch $@ # just in case it does not get created
	$(top_srcdir)/exe/$(SYSTEM)/f2cvv $<

%_ftoc.c: %.c
	touch $@ # just in case it does not get created
	$(top_srcdir)/exe/$(SYSTEM)/f2cvv $<


AM_CFLAGS=-Wall -ggdb3 -iquote$(top_srcdir)/inc  -D__GFORTRAN__
AM_FFLAGS= -fno-backslash -fsecond-underscore  -ggdb3 -D__GFORTRAN__
