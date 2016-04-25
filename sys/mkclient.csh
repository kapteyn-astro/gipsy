#!/bin/csh -f
# mkclient.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1993
#       All Rights Reserved.
#
##>            mkclient.doc
#
#Script:       mkclient
#
#Purpose:      Make current host a GIPSY client.
#
#Category:     MANAGEMENT
#
#File:         mkclient.doc
#
#Author:       K.G. Begeman
#
#Use:          mkclient.csh mode
#
#Description:  This script creates a new clients file (clients.new) in
#              current directory with the client flags as specified on the
#              command line.
#
#Updates:      Jan 11, 1993: KGB, Document created.
#              Sep 22, 2000: JPT, Add copy .template files to $gip_loc
#
##<
#
# Creating clients file
#
if ( ${#argv} == 1 ) then			# flags on command line
	@ sflag = ${argv[1]}			# the flags
else						# default flags
	@ sflag = 0				# no options
endif						# }
set client = `hostname`				# name of client
if ( ${?gip_root} == 0 ) then			# check whether defined or not
	echo '$gip_root NOT defined\!'		# message
	exit 1					# quit
endif						# } 
setenv gip_loc ${gip_root}/loc			# define it
if ( -d ${gip_loc} == 0 ) then			# check whether present or not
	echo "Creating ${gip_loc}"		# message
	mkdir ${gip_loc}			# make it
        cp ${gip_root}/sys/grdevices.template ${gip_loc}/grdevices
        cp ${gip_root}/sys/lpdevices.template ${gip_loc}/lpdevices
        cp ${gip_root}/sys/mtdevices.template ${gip_loc}/mtdevices
        cp ${gip_root}/sys/tvdevices.template ${gip_loc}/tvdevices
endif						# }
if ( -e ${gip_loc}/clients == 0 ) then		# check whether present or not
	if ( -e ${gip_root}/doc/clients.doc ) then	# get it
		awk '{printf("#%s\n",$0)}' ${gip_root}/doc/clients.doc > clients.new
	else					# }{
		touch clients.new		# create it
	endif					# }
else						# copy without current host
	awk -F: '{if(substr($0,length($0),1)=="\\"){printf("%s",substr($0,1,length($0)-1))}else{printf("%s\n",$0)}}' $gip_loc/clients | awk -F: '{if ($1!="'${client}'") printf("%s\n",$0)}' > clients.new
endif						# }
#
# Now get the architecture
#
set archi = `${gip_root}/sys/getarch.csh`		# get architecture
if ( ${archi} == 'unknown' ) then		# unknown architecture
	echo "Cannot determine architecture of host ${client}."
	echo -n "Please enter it here > "	# prompt
	set archi = $<				# get it
endif						# }
setenv gip_exe ${gip_root}/exe/${archi}		# set
setenv gip_lib ${gip_root}/lib/${archi}		# set
setenv gip_tmp ${gip_root}/tmp/${archi}		# set
echo "${client}:${archi}:${sflag}:${gip_exe}:${gip_lib}:${gip_tmp}" >> clients.new
echo "clients.new created"			# text
