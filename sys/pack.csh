#!/bin/csh
# pack.csh
#
#	Copyright (c) Kapteyn Laboratorium Groningen 1992, 2011
#	All Rights Reserved.
#
##>            pack.doc
#
#Script:       pack
#
#Purpose:      Packs files so that they can be unpacked by xfile
#
#Category:     UTILTIY
#
#File:         pack.csh
#
#Author:       K.G. Begeman
#
#Use:          $gip_sys/pack.csh pname [ iname [ iname [ iname ... ] ] ]
#
#Description:  The file 'pname' will be created and the rest of the
#              filenames on the command line will be packed into this file.
#
#Updates:      Aug  7, 1991: KGB, Document created.
#              Mar  6, 2011: JPT, Changed copyright notice.
#
##<
#
# Check the number of arguments
#
@ argc = ${#argv}					# argument count
if (${argc} < 1) then					# no arguments
	echo "Usage: pack pname [ iname [ iname [ iname ... ] ] ]"
	exit						# quit
endif							# }
#
# Check whether file pname already exists. If so, we rename the old
# file to 'pname'.bak
#
if ( -e ${argv[1]} ) then				# file exists
	if ( -e ${argv[1]}.bak ) then			# backup also exists
		echo "Backup ${argv[1]}.bak already exists\!"
		exit					# quit
	endif
	echo "Renaming ${argv[1]} to ${argv[1]}.bak\!"
	\mv ${argv[1]} ${argv[1]}.bak			# rename
endif
#
#
# Make heading
#
echo ${argv[1]}                     | cat >  ${argv[1]}	# line 1
echo ""                             | cat >> ${argv[1]}	# line 2
date | awk '{printf("        Copyright (c) Kapteyn Astronomical Institute, Groningen %s\n",$NF);}' >> ${argv[1]}
echo "        All Rights Reserved." | cat >> ${argv[1]}	# line 4
echo ""                             | cat >> ${argv[1]}	# line 5
echo "Name:         ${argv[1]}"     | cat >> ${argv[1]}	# line 6
echo "Creator:      `whoami`"       | cat >> ${argv[1]}	# line 7
echo "Host:         `hostname`"     | cat >> ${argv[1]}	# line 8
date | awk '{printf("Date:         %s %s, %s\n",$2,$3,$NF);}' >> ${argv[1]}
set line = "Contents:    "				# line 10
@ iarg = 1						# argument counter
@ count = 0						# item counter
@ error = 0						# reset
while (${iarg} < ${argc})				# loop
	@ iarg++					# increase
	@ count++					# increase
	if ( -e ${argv[${iarg}]} == 0 ) then		# file does not exist
		@ error++				# increase
		echo "${argv[${iarg}]} not found\!"	# message
	endif						# }
	set line = "${line} ${argv[${iarg}]}"		# next file
	if (${iarg} == ${argc} || ${count} == 5) then	# ship it out
		@ count = 0				# reset
		echo "${line}"      | cat >> ${argv[1]}	# next line
		set line = "             "		# reset
	endif						# }
end							# endwhile
#
# Check for errors
#
if (${error} != 0) then					# error
	rm -f ${argv[1]}				# remove packed file
	exit						# and quit
endif							# }
#
# Loop
#
@ iarg = 1						# argument counter
echo -n "Packing"					# message for user
while (${iarg} < ${argc})				# loop
	@ iarg++;					# increase
	if (${iarg} == ${argc}) then			# last file
		echo " ${argv[${iarg}]}"		# do <cr>
	else						# no last file
		echo -n " ${argv[${iarg}]}"		# don't <cr>
	endif						# }
	echo ""                     | cat >> ${argv[1]}	# empty line
	echo "#>            ${argv[${iarg}]}"  | cat >> ${argv[1]}
	cat ${argv[${iarg}]} >> ${argv[1]}		# the file
	echo "#<"                   | cat >> ${argv[1]}	# closing part
end							# endwhile
