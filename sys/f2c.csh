#!/bin/csh -f
# f2c.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1994
#       All Rights Reserved.
#
##>            f2c.doc
#
#Script:       f2c
#
#Purpose:      Compiles Fortran sources with the Fortran to C (f2c) compiler.
#
#Category:     MANAGEMENT
#
#File:         f2c.csh
#
#Author:       K.G. Begeman
#
#Use:          This shell script starts the Fortran to C compiler. The
#              generated C code is compiled into objects or executables.
#              It recognizes the following options:
#
#              -f2c             indicates that the following options until the
#                               next occurrence of this option are for the
#                               Fortran to C compiler.
#              -f2cC <arg>      <arg> is name of C compiler to use after
#                               conversion from Fortran to C [cc].
#              -f2cE <arg>      <arg> is the name of the Fortran to C compiler
#                               which should be used [f2c].
#              -f2cI <arg>      <arg> is the path to the directory where
#                               include files needed for the compilation of
#                               the C sources created by the Fortran to C
#                               compiler reside.
#              -f2cK            do not remove the generated C code.
#
#              All other switches (except those in between two -f2c options)
#              are passed to the C compiler.
#
#Updates:      Mar 28, 1994: KGB, Document created.
#
##<
#
@ f2c_rm = 1					# removal of generated C code
@ f2c_sw = 0					# options for C compiler
set f2c_fopt = ( )				# Fortran to C options
set f2c_incs = ( )				# includes for generated C code
set f2c_path = ( f2c )				# name for Fortran to C compiler
set f2c_fnam = ( )				# fortran sources
set f2c_cnam = ( )				# generated C sources
set f2c_copt = ( )				# C compiler options
set f2c_ccmd = ( cc )				# C compiler
@ iarg = 0					# loop counter
while ( ${iarg} < ${#argv} )			# loop over options
	@ iarg++				# next options
	switch( ${argv[${iarg}]} )		# which option ?
	case '-f2c':				# switch change
		if ( ${f2c_sw} ) then		# back to C options
			@ f2c_sw--
		else				# next options for Fortran to C
			@ f2c_sw++
		endif
		breaksw
	case '-cc':
	case '-f2cC':				# name of C compiler
		@ iarg++
		set f2c_ccmd = ( ${argv[${iarg}]} )
		breaksw
	case '-f2cE':				# path to Fortran to C compiler
		@ iarg++
		set f2c_path = ( ${argv[${iarg}]} )
		breaksw
	case '-f2cI':				# includes for generated C code
		@ iarg++
		set f2c_incs = ( -I${argv[${iarg}]} )
		breaksw
	case '-f2cK':				# want to keep generated C code
		@ f2c_rm = 0
		breaksw
	case '+*':				# pass them on to ?
	case '-*':
		if ( ${f2c_sw} ) then
			set f2c_fopt = ( ${f2c_fopt} ${argv[${iarg}]} )
		else
			set f2c_copt = ( ${f2c_copt} ${argv[${iarg}]} )
		endif
		breaksw
	default:				# check for Fortran source
		set f = ${argv[${iarg}]}
		if ( -e ${f} && ${f:e} == 'f' ) then
			set f2c_fnam = ( ${f2c_fnam} ${f} )
			set r = ${f:t}
			set c = "${r:r}.c"
			set f2c_cnam = ( ${f2c_cnam} ${c} )
		else if ( ${f2c_sw} ) then
			set f2c_fopt = ( ${f2c_fopt} ${f} )
		else
			set f2c_copt = ( ${f2c_copt} ${f} )
		endif
		breaksw
	endsw
end
if ( ${#f2c_fnam} ) then			# we have some Fortran sources
	${f2c_path} ${f2c_fopt} ${f2c_fnam}	# Fortran -> C
	@ fcs = ${status}
	if ( ${fcs} && ${#f2c_cnam} && ${f2c_rm} ) then
		\rm -f ${f2c_cnam}
		exit( ${fcs} )
	endif
endif
${f2c_ccmd} ${f2c_incs} ${f2c_cnam} ${f2c_copt}	# do the compilation of C code
@ ccs = ${status}
if ( ${#f2c_cnam} && ${f2c_rm} ) then
	\rm -f ${f2c_cnam}
endif
exit( ${ccs} )
