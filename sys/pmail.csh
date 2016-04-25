#!/bin/csh
# pmail.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992
#       All Rights Reserved.
#
##>            pmail.doc
#
#Script:       pmail
#
#Purpose:      pmails sends a mail message (file) to all GIPSY programmers.
#
#Category:     MANAGEMENT
#
#File:         pmail.csh
#
#Author:       K.G. Begeman
#
#Use:          $gip_sys/pmail.csh message-file
#
#Warning:      This script runs only on (some) UNIX systems.
#
#Updates:      Apr 29, 1992: KGB, original document.
#
##<
#
# Here we go!!
#
if ( ${#argv} != 1 ) then
	echo "script sends mail to all GIPSY programmers"
	echo "usage: pmail <filename>"
else if ( -e ${argv[1]} ) then
	set pees = ( `awk -F: '{if (substr($0,1,1) != "#" && NF == 3) printf( "%s %s %s ;\n", $1, $2, $3 ) }' $gip_sys/programmers.mgr` )
	@ i = 0
	while ( ${i} < ${#pees} )
		@ i++
		@ i++
		mail ${pees[${i}]} < ${argv[1]}
		@ i++
		echo -n "mail to : ${pees[${i}]}"
		@ i++
		while ( ${pees[${i}]} != ";" )
			echo -n " ${pees[${i}]}"
			@ i++
		end
		echo ""
	end
else
	echo "file (${argv[1]}} not found"
endif
