# gipenv.sh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1993
#       All Rights Reserved.
#
if [ "${gip_root}" = "" ] ; then			# gip_root not defined
	echo "gip_root not defined\!\!"			# error message
elif [ ! -d ${gip_root} ] ; then			# gip_root non existent
	echo "${gip_root} not found\!\!"		# error message
elif [ -f "${gip_root}/loc/gipenv.sh" ] ; then		# local setup
	. ${gip_root}/loc/gipenv.sh			# setup locally
else							# okay
	client="`hostname`"				# get client name
	line=`awk -F: '{if(substr($0,1,1)!="#")if(substr($0,length($0),1)=="\\\\"){printf("%s",substr($0,1,length($0)-1));}else{printf("%s\n",$0);}}' ${gip_root}/loc/clients | grep ${client}`
	if [ `echo ${line} | awk -F: '{print NF}'` -eq 6 ] ; then
		gip_arch=`echo ${line} | awk -F: '{print $2}'`	# gip_arch
		export gip_arch				# architecture
		gip_dat=${gip_root}/dat			# gip_dat
		export gip_dat				# the data directory
		gip_doc=${gip_root}/doc			# gip_doc
		export 	gip_doc				# documents
		gip_exe=`echo ${line} | awk -F: '{print $4}'`	# gip_exe
		export gip_exe				# executables
		gip_host=`echo ${line} | awk -F: '{print $1}'`	# gip_host
		export gip_host				# the client
		gip_htm=${gip_root}/htm			# gip_htm
		export gip_htm				# html's
		gip_inc=${gip_root}/inc			# gip_inc
		export gip_inc				# includes
		gip_lib=`echo ${line} | awk -F: '{print $5}'`	# gip_lib
		export gip_lib				# libraries
		gip_loc=${gip_root}/loc			# gip_loc
		export gip_loc				# local setup
		gip_mis=${gip_root}/mis			# gip_mis
		export gip_mis				# the rest
		gip_mode=`echo ${line} | awk -F: '{print $3}'`	# gip_mode
		export gip_mode				# the mode
		gip_old=${gip_root}/old			# gip_old
		export gip_old				# old sources
		gip_sub=${gip_root}/sub			# gip_sub
		export gip_sub				# functions
		gip_sys=${gip_root}/sys			# gip_sys
		export gip_sys				# system files
		gip_tmp=`echo ${line} | awk -F: '{print $6}'`	# gip_tmp
		export gip_tmp				# temporary
		gip_tsk=${gip_root}/tsk			# gip_tsk
		export gip_tsk				# applications
#	else						# unknown client
#		echo "GIPSY client ${client} unknown\!\!"	# error message (suppressed)
	fi						# }
	unset	client					# remove
	unset	line					# remove
fi
