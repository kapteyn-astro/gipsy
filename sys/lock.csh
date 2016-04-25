#!/bin/csh
# lock.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1991
#       All Rights Reserved.
#
#
##>            lock.doc
#
#Script:       lock
#
#Purpose:      Locks compile for GIPSY manager.
#
#Category:     SYSTEM MANAGEMENT
#
#File:         lock.csh
#
#Author:       K.G. Begeman
#
#Use:          $gip_sys/lock.csh
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
			if ( ${locker} == "MANAGER" ) then
				echo "${f:t} alread locked"
			else
				while ( -e ${f}/update.lock )
					echo "Locked by ${locker}"
					sleep 60
				end
			endif
		endif
		if ( -e ${f}/update.lock == 0 ) then
			echo "MANAGER" > ${f}/update.lock
			echo locked ${f:t}
		endif
	endif
end
