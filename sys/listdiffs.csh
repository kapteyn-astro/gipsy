#!/bin/csh -f
# listdiff.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1993
#       All Rights Reserved.
#
##>            listdiffs.doc
#
#Script:       listdiffs
#
#Purpose:      List different files in two directories.
#
#Category:     UTILITY
#
#File:         listdiffs.csh
#
#Author:       K.G. Begeman
#
#Use:          listdiffs.csh newdir olddir
#
#Description:  This script lists the files which are modified and the
#              files which are only present in newdir.
#
#Updates:      Feb  4, 1993: KGB, Document created.
#
##<
#
# Here we start.
#
if ( ${#argv} != 2 ) then			# wrong number of args
	echo 'Usage: listdiffs newdir olddir'	# message
	exit( 1 )				# quit
endif						# cif
set	dirn =	${argv[1]}			# newdir
set	diro =	${argv[2]}			# olddir
if ( -d ${dirn} == 0 ) then			# not present
	echo "${dirn} not found"		# message
	exit( 1 )				# quit
endif						# cif
if ( -d ${diro} == 0 ) then			# not present
	echo "${diro} not found"		# message
	exit( 1 )				# quit
endif						# cif
ls ${dirn} > /tmp/.dirlist			# file list
@ nlist	= `cat /tmp/.dirlist | awk 'END{print NR}'`
@ i = 0						# reset
while ( ${i} < ${nlist} )			# while
	@ i++					# next
	set	fn	= `cat /tmp/.dirlist | awk '{if(NR=='${i}')print $1}'`
	set	fo	= ${diro}/${fn:t}	# old file
	if ( -e ${fo} == 0 ) then		# not present
		echo "New     : ${fn:t}"	# message
	else if ( -e ${dirn}/${fn} ) then	# else if
		diff ${dirn}/${fn} ${fo} > /dev/null
		@ s	= ${status}		# get status
		if ( ${s} ) then		# not zero
			echo "Modified: ${fn:t}"# message
		endif				# cif
	endif					# cif
end						# cwhile
\rm /tmp/.dirlist				# remove tmp
