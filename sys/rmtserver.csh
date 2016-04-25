#!/bin/csh
#
# Script for starting the rmtserver via cron.
#
# crontab entry: 0 * * * * /bin/csh <gip_sys>/rmtserver.csh
#
if ( $0 != '' ) then					# command line argv[0]
	set tmp = $0					# command line
	if ( -d ${tmp:h} ) then 			# directory ?
		set mywd = `\pwd`			# cwd
		cd ${tmp:h}				# change wd
		set tmp = `\pwd`			# get wd
		cd ${mywd}				# back
		unset mywd				# unset
	else						# else
		set tmp = `\pwd`			# cwd
	endif						# }
	set tmp = ${tmp:h}				# path to gip_root
	if ( -e ${tmp}/sys/cshrc.csh ) then		# present
		setenv gip_root ${tmp}			# make gip_root
		source ${gip_root}/sys/cshrc.csh	# execute
	endif						# }
	unset tmp					# unset
endif							# }
if ( ${?gip_root} && -x ${gip_exe}/rmtserver && -r ${gip_loc}/mtdevices ) then
	\rm -f ${gip_loc}/rmtserver.`hostname`
	${gip_exe}/rmtserver -daemon ${gip_loc} >& ${gip_loc}/rmtserver.`hostname`
else
	mail `whoami` << EOF
The Remote Tape Server (rmtserver) was not started on `hostname`

                                rmtserver.csh
EOF
endif
