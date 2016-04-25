#!/bin/csh -f
# getfile.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1993
#       All Rights Reserved.
#
##>            getfile.doc
#
#Script:       getfile
#
#Purpose:      Retrieves a file from the GIPSY source server.
#
#Category:     MANAGEMENT
#
#File:         getfile.csh
#
#Author:       K.G. Begeman
#
#Use:          getfile.csh filename
#
#Description:  You have to follow the following 3 steps to obtain
#              a file from the GIPSY source server.
#              1. Goto the directory in the GIPSY tree where the file
#              resides (i.e. cd $gip_tsk for compile.c and cd
#              $gip_exe for compile).
#              2. Type $gip_sys/getfile.csh filename to retrieve
#              filename in the current directory. If the file already
#              exists, it will be renamed to filename.old
#              3. Check whether the file arrived. The script cannot
#              check this. If the file has extension csh or it is a 
#              binary executable, it will be made executable.
#
#Example:      To obtain $gip_exe/stat
#              cd $gip_exe
#              $gip_sys/getfile.csh stat
#
#Updates:      Jan 25, 1993: KGB, Document created.
#
##<
#
# Here we do our job!
#
if ( ${#argv} != 1 ) then			# arguments ?
	echo "Use: getfile.csh filename"	# message
	exit 1					# exit with status
endif						# }
set	file = ${argv[1]:t}			# the file
if ( -e ${gip_loc}/server ) then		# try local first
	set	fields = ( `awk -F: '{if(substr($0,1,1)!="#"&&NF==3){printf("%s\n%s\n",$1,$3);}}' ${gip_loc}/server` )
else if ( -e ${gip_sys}/server.mgr ) then	# try system
	set	fields = ( `awk -F: '{if(substr($0,1,1)!="#"&&NF==3){printf("%s\n%s\n",$1,$3);}}' ${gip_sys}/server.mgr` )
else						# use default
	set	fields = (  '129.125.6.204' '/dj3/users/gipsy' )
endif						# }
set	curdir = `pwd`				# current directory
@	makexe = 0				# reset
switch( ${curdir} )				# test
case */dat/*:					# in gip_dat/*
	set	tstdir = ${curdir}
	set	datdir = ""
	while ( ${tstdir:t} != 'dat' )
		set	datdir = ${tstdir:t}/${datdir}
		set	tstdir = ${tstdir:h}
	end
	set	gspath = ${fields[2]}/dat/${datdir}
	@	binary = 0
	breaksw
case */sys:					# in gip_sys
	if ( ${file:e} == 'csh' ) then		# a script
		@ makeexe = 1			# make executable
	else if ( ${file:e} == 'sh' ) then	# also a script
		@ makeexe = 1			# make executable
	endif					# }
case */dat:					# in gip_dat
case */doc:					# in gip_doc
case */inc:					# in gip_inc
case */loc:					# in gip_loc
case */mis:					# in gip_mis
case */old:					# in gip_old
case */sub:					# in gip_sub
case */tsk:					# in gip_tsk
	set	gspath = ${fields[2]}/${curdir:t}
	@	binary = 0			# ascii
	breaksw					# quit
default:					# other
	set	bindir	= ${curdir:h}		# strip architecture
	switch( ${bindir} )			# where are we ?
	case */exe:				# in gip_exe
		@ makexe = 1;			# make executable
	case */lib:				# in gip_lib
	case */tmp:				# in gip_tmp
		set	gspath = ${fields[2]}/${bindir:t}/${curdir:t}
		@	binary = 1;		# binary
		breaksw				# break
	default:				# error
		echo "Not in a GIPSY directory"	# message
		exit 1				# exit with status
	endsw					# }
endsw						# }
if ( -e ${file} ) then				# old file present
	echo "renaming old file to ${file}.old"
	mv ${file} ${file}.old			# rename it
endif						# }
if ( -x ${gip_exe}/gftp ) then
	${gip_exe}/gftp get ${gspath}/${file} ${file}
	@ s = ${status}
	if ( ${s} ) then
		\rm -f ${file}
	endif
else
	if ( ${binary} ) then			# binary files
		tftp ${fields[1]} >& .errors << @EOF
binary
timeout 60
get ${gspath}/${file}
quit
@EOF
	else					# ascii files
		tftp ${fields[1]} >& .errors << @EOF
ascii
timeout 60
get ${gspath}/${file}
quit
@EOF
	endif					# }
	if ( -e .errors ) then			# present
		grep Received .errors > /dev/null	# search for errors
		@ s = ${status}			# save status
		if ( ${s} ) then		# errors occurred
			rm -f ${file}		# remove it
		endif				# }
		rm -f .errors			# remove it
	endif					# }
endif						# }
if ( -e ${file} == 0 ) then			# we got it
	echo "Error retrieving ${file}"		# message
	exit 1					# exit with status
else if ( ${makexe} ) then			# executable
	chmod +x ${file}			# do it
endif						# }
