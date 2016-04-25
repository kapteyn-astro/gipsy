#!/bin/csh
# update.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992, 1993 - 2008
#       All Rights Reserved.
#
##>            update.doc
#
#Script:       update
#
#Purpose:      Updates the GIPSY libraries and applications.
#
#Category:     MANAGEMENT
#
#File:         update.csh
#
#Author:       K.G. Begeman
#
#Use:          This shell script is executed by the atqueue or by crontab
#              script. When this script has completed the update, a message
#              is send to the GIPSY manager (see $gip_loc/manager), or if
#              there is no GIPSY manager found for this machine, to the
#              initiating user (usually gipsy).
#              The complete logfile is stored in $gip_loc/update.`hostname`.
#              Old executables older than 1 week are removed from $gip_exe.
#
#Updates:      Jul 23, 1991: KGB, Document created.
#              Mar 15, 1993: KGB, Logfile store in $gip_loc.
#              Jul 29, 1993: KGB, Check for existing log files.
#              Nov 10, 1998: JPT, Version number upgraded to 101.
#              May 28, 2008: JPT, For MacOS: changed "Compile" into "uCompile".
#
##<
#
unalias cd
if ( $0 != '' ) then					# command line argv[0]
	set tmp = $0					# command line
	if ( -d ${tmp:h} ) then				# directory ?
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
if ( ${?gip_loc} == 0 ) then				# error
	mail `whoami` << @EOF
GIPSY environment not correct on `hostname`

                  update.csh
@EOF
	exit						# exit
endif							# }
set noglob						# no filename completion
set host = `hostname`					# name of host
if ( -e ${gip_loc}/manager ) then			# {
	set awkfile = /tmp/update.awk.${host}		# name of awk file
	\rm -f ${awkfile}				# just in case
	\cat << @EOF > ${awkfile}
BEGIN {
   mailto = "`whoami`";
}
{
   if (NF == 2 && \$1 == "${host}" ) {
      mailto = \$2;
   }
}
END {
   printf("%s\n",mailto);
}
@EOF
	set mngr = `\awk -F: -f ${awkfile} ${gip_loc}/manager`
	\rm -f ${awkfile}				# remove it
	unset awkfile					# un set
else							# } else {
	set mngr = `whoami`				# default
endif							# }
#
# Remove old executables which are older than a week.
#
\find ${gip_exe} -name \*.old -atime +7 -exec rm \{\} \;
#
# Check for version 101 or older
#
set versions = ( `\cat ${gip_sys}/history | \awk '{if(substr($0,1,1)!="#"){if(substr($0,length($0),1)=="\\"){printf("%s",substr($0,1,length($0)-1))}else{printf("%s\n",$0)}}}' | \awk -F: '{if($1=="compile.c"){printf("%s\n",$3)}}'` )
if ( $#versions ) then
	if ( $versions[$#versions] < 101 ) then
		\mv ${gip_tsk}/compile.c ${gip_tsk}/compile.c.original
		${gip_exe}/gftp get /tha3/users/gipsy/tsk/compile.c ${gip_tsk}/compile.c
		@ s = ${status}
		if ( ${s} ) then
			${gip_exe}/gftp get /tha3/users/gipsy/tsk/compile.c ${gip_exe}/compile.c
			@ s = ${status}
		endif
		if ( ! ${s} ) then
			\cp ${gip_exe}/compile ${gip_exe}/uCompile	# start this one
			${gip_exe}/uCompile -rebuild compile.c >& /dev/null
			\rm -f ${gip_exe}/uCompile			# remove it
		endif
	endif
endif
unset versions
#
# Now do the update.
#
set ufile = update
if ( -e ${gip_tmp}/${ufile}.log ) then
	@ i = 1;
	while ( -e ${gip_tmp}/update${i}.log )
		@ i++;
	end
	set ufile = update${i}
	unset i
endif
set s_time = `date`					# start time
\cp ${gip_exe}/compile ${gip_exe}/uCompile		# start this one
${gip_exe}/uCompile -update >& ${gip_tmp}/${ufile}.log	# update everything
\rm -f ${gip_exe}/uCompile				# remove it
set e_time = `date`					# end time
\rm -f ${gip_tmp}/${ufile}.head				# remove
\cat > ${gip_tmp}/${ufile}.head << @EOF
START of Update  --  ${s_time}  on  ${host}
@EOF
\rm -f ${gip_tmp}/${ufile}.tail				# remove
\cat > ${gip_tmp}/${ufile}.tail << @EOF
END of Update    --  ${e_time}  on  ${host}
@EOF
#
# We copy the complete logfile to $gip_loc.
#
\rm -f ${gip_loc}/${ufile}.${host}			# remove
\cat ${gip_tmp}/${ufile}.head ${gip_tmp}/${ufile}.log ${gip_tmp}/${ufile}.tail > ${gip_loc}/${ufile}.${host}
#
# Now we search for the status only and copy these lines to update.mail.
#
\rm -f ${gip_tmp}/${ufile}.mail				# remove
\touch ${gip_tmp}/${ufile}.mail				# create it
\cat ${gip_tmp}/${ufile}.head >> ${gip_tmp}/${ufile}.mail	# append
\cat ${gip_tmp}/${ufile}.log | \awk '{if($1=="uCompile"&&$2=="--")printf("%s\n",substr($0,1,length($0)-1));}' >> ${gip_tmp}/${ufile}.mail
\cat ${gip_tmp}/${ufile}.tail >> ${gip_tmp}/${ufile}.mail	# append
\rm -f ${gip_tmp}/${ufile}.head				# remove
\rm -f ${gip_tmp}/${ufile}.log				# remove
\rm -f ${gip_tmp}/${ufile}.tail				# remove
#
# Now mail the damned thing.
#
\cat ${gip_tmp}/${ufile}.mail | mail ${mngr}		# to manager
\rm -f ${gip_tmp}/${ufile}.mail				# remove
#
# Reset and remove leftovers.
#
unset noglob						# un set
unset host						# un set
unset mngr						# un set
unset ufile						# un set
