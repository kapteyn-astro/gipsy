#!/bin/csh
# unlock.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992
#       All Rights Reserved.
#
#
##>            unlock.doc
#
#Script:       unlock
#
#Purpose:      Unlocks compile by GIPSY manager.
#
#Category:     MANAGEMENT
#
#File:         unlock.csh
#
#Author:       K.G. Begeman
#
#Use:          $gip_sys/unlock.csh
#
#Updates:      Dec  9, 1991: KGB, document created.
#
##<
#
#
foreach f ( ${gip_root}/tmp/* )
	if ( -d ${f} ) then
		if ( -e ${f}/update.lock ) then
			set locker = `cat ${f}/update.lock`
			if ( ${locker} != "MANAGER" ) then
				echo "Locked by ${locker}"
			else
				rm -f ${f}/update.lock
				echo "Unlocked ${f:t}"
			endif
		else
			echo "${f:t} not locked"
		endif
	endif
end
