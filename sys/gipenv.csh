# gipenv.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992, 1993, 1996
#       All Rights Reserved.
#
##>            gipenv.doc
#
#Script:       gipenv
#
#Purpose:      Set some GIPSY environment variables in .cshrc or .profile
#              for process wide use.
#
#Category:     MANAGEMENT
#
#File:         gipenv.csh
#
#Author:       K.G. Begeman
#
#Use:          when running csh, execute this C shell script in .cshrc by
#              the following statement:
#
#              source $gip_root/sys/gipenv.csh
#
#              when running sh, execute gipenv.sh in .profile by the
#              following statement:
#
#              . $gip_root/sys/gipenv.sh
#
#Notes:        1) gip_root must have been defined before this statement
#              and is system dependent. For example for the Kapteyn alliant
#              gip_root must be defined as follows in .cshrc:
#              setenv gip_root /dj3/users/gipsy
#              and in .profile:
#              gip_root=/dj3/users/gipsy; export gip_root
#              2) if a local setup file exists ($gip_root/loc/gipenv.csh
#              or $gip_root/loc/gipenv.sh), then the local file will be
#              executed.
#
#Updates:      Jun 30, 1990: KGB, Document created.
#              Mar 22, 1993: KGB, also for sh.
#              Apr  4, 1996: KGB, allowed local setup.
#              Feb 12, 2003: JPT, suppress client error message.
#              Nov 15, 2004: JPT, introduced generic per-architecture client.
#              Jun 16, 2009: JPT, 64-bit generic clients added. x86_64 only.
#              Mar 22, 2011: JPT, added command-line invokable task aliases.
#
##<
#
if (${?gip_root} == 0) then				# gip_root not defined
	echo "gip_root not defined\!\!"			# error message
else if ( -d ${gip_root} == 0 ) then			# gip_root non existent
	echo "${gip_root} not found\!\!"		# error message
else if ( -e ${gip_root}/loc/gipenv.csh ) then		# local setup
	source ${gip_root}/loc/gipenv.csh		# setup locally
else							# okay.
	set client = `hostname`				# get client name
	set fields = `awk -F: '{{if(NR==1)nf=0;}{if((nf==0&&($1=="'${client}'"))||(nf>0&&nf<6)){if($NF=="\\"){iend=NF;}else{iend=NF+1;}for(i=1;i<iend&&nf<7;i++){printf("%s",$i);nf++;if(nf!=6)printf(" ");}}}}' ${gip_root}/loc/clients`
	@ nf = ${#fields}				# number of fields
        if (${nf} != 6) then
		set sysinfo = 'Generic-'`uname -s`	# generic
		if (`uname -m` == "x86_64") then
			set sysinfo = ${sysinfo}64	# 64-bit generic
		endif
		set fields = `awk -F: '{{if(NR==1)nf=0;}{if((nf==0&&($1=="'${sysinfo}'"))||(nf>0&&nf<6)){if($NF=="\\"){iend=NF;}else{iend=NF+1;}for(i=1;i<iend&&nf<7;i++){printf("%s",$i);nf++;if(nf!=6)printf(" ");}}}}' ${gip_root}/loc/clients`
		@ nf = ${#fields}
	endif
	if (${nf} == 6) then				# okay
		setenv	gip_arch	${fields[2]}	# architecture
		setenv	gip_dat	${gip_root}/dat		# GIPSY data
		setenv	gip_doc	${gip_root}/doc		# documents
		setenv	gip_exe	${fields[4]}		# executables
		setenv	gip_host ${client}		# GIPSY client
		setenv	gip_htm ${gip_root}/htm		# html's
		setenv	gip_inc	${gip_root}/inc		# includes
		setenv	gip_lib	${fields[5]}		# libraries
		setenv	gip_loc	${gip_root}/loc		# local setup files
		setenv	gip_mis	${gip_root}/mis		# miscellaneous files
		setenv	gip_mode	${fields[3]}	# mode
		setenv	gip_old	${gip_root}/old		# old sources
		setenv	gip_sub	${gip_root}/sub		# sources of procedures
		setenv	gip_sys	${gip_root}/sys		# system files
		setenv	gip_tsk	${gip_root}/tsk		# sources of applications
		setenv	gip_tmp	${fields[6]}		# temporary files
		alias	check	${gip_sys}/check.csh	# checks for compilation errors
		alias	gipsy	${gip_sys}/gipsy.csh	# starts default GIPSY
		alias	p	${gip_exe}/compile	# does compilations
		alias	pack	${gip_sys}/pack.csh	# packs files
		alias	s77	${gip_exe}/sheltran	# sheltran compiler
		alias	xgipsy	${gip_sys}/xgipsy.csh	# starts X GIPSY
                alias   ngipsy  ${gip_sys}/ngipsy.csh   # set env. for N HERMES
		alias	nhermes ${gip_exe}/nhermes	# starts N HERMES

                alias   skycalc ${gip_exe}/skycalc      # command-line...
                alias   skycalq ${gip_exe}/skycalq      # ...invokable tasks
                alias   visions ${gip_exe}/visions      #


#	else						# } else {
#		echo "GIPSY client ${client} unknown\!\!"	# error message (suppressed)
	endif						# }
	unset	client					# remove
	unset	fields					# remove
	unset	nf					# remove
endif							# }
##>            gipenv.sh
## gipenv.sh
##
##       Copyright (c) Kapteyn Laboratorium Groningen 1993
##       All Rights Reserved.
##
#if [ "${gip_root}" = "" ] ; then			# gip_root not defined
#	echo "gip_root not defined\!\!"			# error message
#elif [ ! -d ${gip_root} ] ; then			# gip_root non existent
#	echo "${gip_root} not found\!\!"		# error message
#elif [ -f "${gip_root}/loc/gipenv.sh" ] ; then		# local setup
#	. ${gip_root}/loc/gipenv.sh			# setup locally
#else							# okay
#	client="`hostname`"				# get client name
#	line=`awk -F: '{if(substr($0,1,1)!="#")if(substr($0,length($0),1)=="\\\\"){printf("%s",substr($0,1,length($0)-1));}else{printf("%s\n",$0);}}' ${gip_root}/loc/clients | grep ${client}`
#	if [ `echo ${line} | awk -F: '{print NF}'` -eq 6 ] ; then
#		gip_arch=`echo ${line} | awk -F: '{print $2}'`	# gip_arch
#		export gip_arch				# architecture
#		gip_dat=${gip_root}/dat			# gip_dat
#		export gip_dat				# the data directory
#		gip_doc=${gip_root}/doc			# gip_doc
#		export 	gip_doc				# documents
#		gip_exe=`echo ${line} | awk -F: '{print $4}'`	# gip_exe
#		export gip_exe				# executables
#		gip_host=`echo ${line} | awk -F: '{print $1}'`	# gip_host
#		export gip_host				# the client
#		gip_htm=${gip_root}/htm			# gip_htm
#		export gip_htm				# html's
#		gip_inc=${gip_root}/inc			# gip_inc
#		export gip_inc				# includes
#		gip_lib=`echo ${line} | awk -F: '{print $5}'`	# gip_lib
#		export gip_lib				# libraries
#		gip_loc=${gip_root}/loc			# gip_loc
#		export gip_loc				# local setup
#		gip_mis=${gip_root}/mis			# gip_mis
#		export gip_mis				# the rest
#		gip_mode=`echo ${line} | awk -F: '{print $3}'`	# gip_mode
#		export gip_mode				# the mode
#		gip_old=${gip_root}/old			# gip_old
#		export gip_old				# old sources
#		gip_sub=${gip_root}/sub			# gip_sub
#		export gip_sub				# functions
#		gip_sys=${gip_root}/sys			# gip_sys
#		export gip_sys				# system files
#		gip_tmp=`echo ${line} | awk -F: '{print $6}'`	# gip_tmp
#		export gip_tmp				# temporary
#		gip_tsk=${gip_root}/tsk			# gip_tsk
#		export gip_tsk				# applications
##	else						# unknown client
##		echo "GIPSY client ${client} unknown\!\!"	# error message (suppressed)
#	fi						# }
#	unset	client					# remove
#	unset	line					# remove
#fi
##<
