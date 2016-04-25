#!/bin/csh
# atqueue.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992
#       All Rights Reserved.
#
##>            atqueue.doc
#
#Script:       atqueue
#
#Purpose:      Schedule compile to update GIPSY libraries and applications.
#
#Category:     MANAGEMENT
#
#File:         atqueue.csh
#
#Author:       K.G. Begeman
#
#Use:          In principle atqueue.csh has to be started once manually.
#              It will submit itself to run at 1.00 am the next morning by
#              default, or at the time entered on the command line.
#              This script has to be run from the user which is owner
#              of the gipsy sources, and that user must have priviledge
#              to use at.
#
#Example:      atqueue.csh 2
#
#Updates:      May 26, 1992: KGB, Document created.
#
##<
#
tty -s						# batch mode ?
@ batch = ${status}				# obtain status
if (${batch}) then				# in batch
	${gip_sys}/update.csh			# to the update
	sleep 3600				# wait one hour
else						# just queueing
	if (${#argv}) then			# argument on command line
		setenv AT_TIME "$argv"		# set environment
	else					# use default
		setenv AT_TIME 1		# 1:00 am
	endif					# }
	if (${?gip_sys} == 0) then		# no gipsy environment
		echo "Gipsy environment not present\!"
		exit 1				# quit with error
	endif					# }
	echo "Queuing Job at ${AT_TIME}"	# message
endif						# }
#
# Now queue the job for the next time.
#
unset batch
echo "/bin/csh ${gip_sys}/atqueue.csh" | at ${AT_TIME}
