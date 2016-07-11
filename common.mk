

%.f: %.shl
	$(top_srcdir)/exe/$(SYSTEM)/sheltran $<

%_ctof.c: %.shl
	touch $@ # just in case it does not get created
	$(top_srcdir)/exe/$(SYSTEM)/f2cvv $<

%_ftoc.c: %.c
	touch $@ # just in case it does not get created
	$(top_srcdir)/exe/$(SYSTEM)/f2cvv $<


AM_CFLAGS=-Wall -O0 -ggdb3 -iquote$(top_srcdir)/inc  -D__GFORTRAN__
AM_FFLAGS=-Wall -Wpedantic -Wno-unused-label -O0 -fno-backslash -fsecond-underscore  -ggdb3 -D__GFORTRAN__
FFLAGS=-O0
CFLAGS=-O0
