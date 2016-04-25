#!/bin/csh -f
# install.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1993, 1997
#       All Rights Reserved.
#
##>            install.doc
#
#Script:       install
#
#Purpose:      Installs or reinstalls the GIPSY essentials. It also
#              creates the directory tree.
#
#Category:     MANAGEMENT
#
#File:         install.csh
#
#Author:       K.G. Begeman
#
#Use:          install should be run by the GIPSY manager to (re)install
#              the GIPSY basics on a certain type of machine. The GIPSY
#              environment variables must have been defined (gipenv.csh) and
#              the $gip_sys/setup.mgr file adapted for the host machine.
#
#Description:  install installs the compile program, the Fortran to C
#              interface program, the file extracter, the gipsy file
#              transporter, the gipsy lock server, the Sheltran compiler
#              and the PGPLOT fonts.
#              It creates the system dependent directories for the
#              GIPSY libraries, executables and temporary files (if
#              they do not yet exist!).
#              install also checks whether it can find the X11 libraries
#              and include files.
#
#Warning:      This script runs only on (some) UNIX systems.
#
#Updates:      Apr 24, 1991: KGB, original document.
#              Feb 11, 1993: KGB, Version 3.0.
#              Mar 16, 1993: KGB, Version 3.1.
#              Aug  1, 1993: KGB, Version 3.3.
#              Oct  1, 1993: KGB, Version 3.4.
#              Mar 28, 1994: KGB, Version 3.5.
#              Mar  3, 1995: KGB, Version 3.6.
#              Sep 22, 2000: JPT, Add copy .template files to $gip_loc
#              Dec  8, 2000: JPT, Silently create missing htm directory.
#              Dec 11, 2008, JPT, Removed redundant -I${gip_inc} option.
#
##<
#
# Here we go!!
#
echo "INSTALL  Version 3.6"				# message
if ( ${?gip_root} == 0 ) then				# fatal
	echo "gip_root not defined\!"			# message
	exit 1						# quit now
endif							# }
echo "Checking for essential utilities ..."		# message
set need = ( awk chmod cp find grep make more mv rm sed strip )	# utilities we need
@ ecount = 0						# reset
@ in = 0						# reset
while ( ${in} < ${#need} )				# loop
	@ in++						# next
	@ ip = 0					# reset
	@ found = 0					# reset
	while ( ${ip} < ${#path} )			# loop
		@ ip++					# next
		if ( -x ${path[${ip}]}/${need[${in}]} ) then
			@ found++			# bingo
		endif					# }
	end						# }
	if ( ${found} == 0 ) then			# okay ?
		echo "${need[${in}]} not in path"	# message
		@ ecount++				# increase error counter
	endif						# }
end							# }
if ( ${ecount} != 0 ) then				# error
	echo "Some essential utilities are missing"	# message
	echo "Suggestion: modify your path"		# message
	echo "INSTALL quits\!"				# message
	exit( 1 )					# quit
endif							# endif
@ ecount = 0						# reset
set client = `hostname`					# name of client
set cwd    = `pwd`					# current dir.
if ( ${?gip_doc} ) then					# default
	unsetenv	gip_doc				# remove it
endif							# }
setenv	gip_doc	${gip_root}/doc				# define gip_doc
if ( -d ${gip_doc} == 0 ) then				# error
	mkdir ${gip_doc}				# create directory
	@ ecount += 1					# increase ecount
endif							# }
if ( ${?gip_htm} ) then					# default
	unsetenv	gip_htm				# remove it
endif							# }
setenv	gip_htm	${gip_root}/htm				# define gip_htm
if ( -d ${gip_htm} == 0 ) then				# error
	mkdir ${gip_htm}				# create directory
endif							# }
if ( ${?gip_inc} ) then					# default
	unsetenv	gip_inc				# remove it
endif							# endif
setenv	gip_inc	${gip_root}/inc				# define gip_inc
if ( -d ${gip_inc} == 0 ) then				# error
	mkdir ${gip_inc}				# create directory
	@ ecount += 1					# increase ecount
endif							# }
if ( ${?gip_loc} ) then					# default
	unsetenv	gip_loc				# remove it
endif							# }
setenv	gip_loc	${gip_root}/loc				# define gip_loc
if ( -d ${gip_loc} == 0 ) then				# error
	mkdir ${gip_loc}				# create directory
        cp ${gip_root}/sys/grdevices.template ${gip_loc}/grdevices
        cp ${gip_root}/sys/lpdevices.template ${gip_loc}/lpdevices
        cp ${gip_root}/sys/mtdevices.template ${gip_loc}/mtdevices
        cp ${gip_root}/sys/tvdevices.template ${gip_loc}/tvdevices
endif							# }
if ( ${?gip_mis} ) then					# default
	unsetenv	gip_mis				# remove it
endif							# }
setenv	gip_mis	${gip_root}/mis				# define gip_mis
if ( -d ${gip_mis} == 0 ) then				# error
	mkdir ${gip_mis}				# create directory
	@ ecount += 1					# increase ecount
endif							# }
if ( ${?gip_old} ) then					# default
	unsetenv	gip_old				# remove it
endif							# }
setenv	gip_old	${gip_root}/old				# define gip_old
if ( -d ${gip_old} == 0 ) then				# error
	mkdir ${gip_old}				# create directory
endif							# }
if ( ${?gip_sub} ) then					# default
	unsetenv	gip_sub				# remove it
endif							# }
setenv	gip_sub	${gip_root}/sub				# define gip_sub
if ( -d ${gip_sub} == 0 ) then				# error
	mkdir ${gip_sub}				# create directory
	@ ecount += 1					# increase ecount
endif							# }
if ( ${?gip_sys} ) then					# default
	unsetenv	gip_sys				# remove it
endif							# }
setenv	gip_sys	${gip_root}/sys				# define gip_sys
if ( -d ${gip_sys} == 0 ) then				# error
	@ ecount += 1					# increase ecount
endif							# }
if ( ${?gip_tsk} ) then					# default
	unsetenv	gip_tsk				# remove it
endif							# }
setenv	gip_tsk	${gip_root}/tsk				# define gip_tsk
if ( -d ${gip_tsk} == 0 ) then				# error
	mkdir ${gip_tsk}				# create directory
	@ ecount += 1					# increase ecount
endif							# }
if ( ${ecount} != 0 ) then				# errors occurred
	echo "Some essential GIPSY directories are missing\!"
	echo "INSTALL quits\!"				# message
	exit( 1 )					# quit
endif							# }
if ( -e ${gip_loc}/clients == 0 ) then			# error
	echo "${gip_loc}/clients not found\!"		# message
	exit( 1 )					# quit
endif							# }
set fields = `cat ${gip_loc}/clients | awk '{if(substr($0,1,1)!="#"){if(substr($0,length($0),1)=="\\"){printf("%s",$0)}else{printf("%s\n",$0)}}}' | awk -F: '{if(NF>=6&&$1=="'${client}'"){for(i=0;i++<6;){printf(" %s",$i)}}}'`
if ( ${#fields} != 6 ) then				# error
	echo "${client} not registered in ${gip_loc}/clients\!"
	exit( 1 )					# quit
endif							# }
if ( ${?gip_arch} ) then				# from clients
	unsetenv	gip_arch			# remove it
endif							# }
setenv	gip_arch	${fields[2]}			# architecture
if ( ${?gip_exe} ) then					# from clients
	unsetenv	gip_exe				# remove it
endif							# }
@ nosuid = `echo ${fields[3]} 8 | awk '{v=$1;t=$2;b=t+t;while(v>=b)v-=b;printf("%d\n",v/t);}'`
setenv	gip_exe	${fields[4]}				# set it
if ( -d ${gip_exe} == 0 ) then				# does not exist
	set dir = ${gip_exe}				# create directory
	while ( -d $dir == 0 )				# loop
	        set dirf = ${dir}			# original name
	        set dirs = ${dirf:h}			# one up
	        while ( -d ${dirs} == 0 )		# loop
	                set dirf = ${dirs}		# save
	                set dirs = ${dirf:h}		# one up
	        end					# end loop
	        mkdir ${dirf}				# create directory
	end						# end loop
	echo "${gip_exe} created\!"			# message
endif							# }
if ( -d ${gip_root}/exe == 0 ) then			# need this one
	mkdir ${gip_root}/exe				# make it
endif							# }
if ( ${gip_exe} != ${gip_root}/exe/${gip_arch} ) then	# need a link
	if ( -e ${gip_root}/exe/${gip_arch} == 0 ) then	# not yet present
		echo "ln -s ${gip_exe} ${gip_root}/exe/${gip_arch}"
		ln -s ${gip_exe} ${gip_root}/exe/${gip_arch}
	endif						# }
endif							# }
if ( ${?gip_lib} ) then					# from clients
	unsetenv	gip_lib				# remove it
endif							# }
setenv	gip_lib	${fields[5]}				# set it
if ( -d ${gip_lib} == 0 ) then				# does not exist
	set dir = ${gip_lib}				# create directory
	while ( -d ${dir} == 0 )			# loop
	        set dirf = ${dir}			# original name
	        set dirs = ${dirf:h}			# one up
	        while ( -d ${dirs} == 0 )		# loop
	                set dirf = ${dirs}		# save
	                set dirs = ${dirf:h}		# one up
	        end					# end loop
	        mkdir ${dirf}				# create directory
	end						# end loop
	echo "${gip_lib} created\!"			# message
endif							# }
if ( -d ${gip_root}/lib == 0 ) then			# need this one
	mkdir ${gip_root}/lib				# make it
endif							# }
if ( ${gip_lib} != ${gip_root}/lib/${gip_arch} ) then	# need a link
	if ( -e ${gip_root}/lib/${gip_arch} == 0 ) then	# not yet present
		echo "ln -s ${gip_lib} ${gip_root}/lib/${gip_arch}"
		ln -s ${gip_lib} ${gip_root}/lib/${gip_arch}
	endif						# }
endif							# }
if ( ${?gip_tmp} ) then					# from clients
	unsetenv	gip_tmp				# remove it
endif							# }
setenv	gip_tmp	${fields[6]}				# set it
if ( -d ${gip_tmp} == 0 ) then				# does not exist
	set dir = ${gip_tmp}				# create directory
	while ( -d ${dir} == 0 )			# loop
	        set dirf = ${dir}			# original name
	        set dirs = ${dirf:h}			# one up
	        while ( -d ${dirs} == 0 )		# loop
	                set dirf = ${dirs}		# save
	                set dirs = ${dirf:h}		# one up
	        end					# end loop
	        mkdir ${dirf}				# create directory
	end						# end loop
	echo "${gip_tmp} created\!"			# message
endif							# }
if ( -d ${gip_root}/tmp == 0 ) then			# need this one
	mkdir ${gip_root}/tmp				# make it
endif							# }
if ( ${gip_tmp} != ${gip_root}/tmp/${gip_arch} ) then	# need a link
	if ( -e ${gip_root}/tmp/${gip_arch} == 0 ) then	# not yet present
		echo "ln -s ${gip_tmp} ${gip_root}/tmp/${gip_arch}"
		ln -s ${gip_tmp} ${gip_root}/tmp/${gip_arch}
	endif						# }
endif							# }
#
# Now check the htm directory
#
set lnks = ( dat doc exe htm inc lib loc mis old sub sys tmp tsk )
foreach d ( ${lnks} )					# loop
	if ( -e ${gip_htm}/${d} == 0 ) then		# not (yet) present
		echo "ln -s ${gip_root}/${d} ${gip_htm}/${d}"
		ln -s ${gip_root}/${d} ${gip_htm}/${d}	# make it
	endif						# }
end							# }
if ( -e ${gip_sys}/setup.mgr == 0 ) then		# error
	echo "${gip_sys}/setup.mgr not found\!"		# message
	exit( 1 )					# quit
endif							# }
unset fields						# remove
if ( -e ${gip_loc}/setup.${client} ) then		# try client setup
	set fields = ( `cat ${gip_loc}/setup.${client} | awk '{if(substr($0,1,1)!="#"){if(substr($0,length($0),1)=="\\"){printf("%s",substr($0,1,length($0)-1))}else{printf("%s\n",$0)}}}' | awk -F: '{if(NF>=14&&$1=="'${gip_arch}'"){for(i=1;i<NF;i++){printf("%s:",$i)}printf("%s",$NF)}}'` )
	if ( ${#fields} == 0 ) then			# not found
		unset	fields				# remove it
	else						# use local setup
		echo "--  Using client setup  --"	# message
	endif						# }
endif							# }
if ( ${?fields} == 0 && -e ${gip_loc}/setup ) then	# try local setup
	set fields = ( `cat ${gip_loc}/setup | awk '{if(substr($0,1,1)!="#"){if(substr($0,length($0),1)=="\\"){printf("%s",substr($0,1,length($0)-1))}else{printf("%s\n",$0)}}}' | awk -F: '{if(NF>=14&&$1=="'${gip_arch}'"){for(i=1;i<NF;i++){printf("%s:",$i)}printf("%s",$NF)}}'` )
	if ( ${#fields} == 0 ) then			# not found
		unset	fields				# remove it
	else						# use local setup
		echo "--  Using local setup  --"	# message
	endif						# }
endif							# }
if ( ${?fields} == 0 ) then				# no local setup
	echo "You have to create ${gip_loc}/setup or create an entry for this machine"
	exit( 1 )
endif							# }
@ nfields = `echo ${fields} | awk -F':' '{ printf NF; }'`
if ( ${nfields} < 14 || ${nfields} > 18 ) then		# error
	echo "Someting wrong in setup file"		# message
	exit( 1 )					# quit
endif
#
# Now we substitute some gipsy symbols
#
set fields = ( `echo ${fields} | sed s+\$gip_inc+${gip_inc}+g | sed s+\$gip_lib+${gip_lib}+g | sed s+\$gip_sys+${gip_sys}+g ` )
#
set CC_COMP = `echo ${fields} | awk -F':' '{ printf $2; }'`
set CC_OPTS = `echo ${fields} | awk -F':' '{ printf $3; }'`
set CC_LIBS = `echo ${fields} | awk -F':' '{ printf $5; }'`
set FC_COMP = `echo ${fields} | awk -F':' '{ printf $6; }'`
set FC_OPTS = `echo ${fields} | awk -F':' '{ printf $7; }'`
set FC_LIBS = `echo ${fields} | awk -F':' '{ printf $9; }'`
#
# Now we have to modify the libraries to get rid of the (non existing)
# giplib.a references
#
set CC_LIBT = ( `echo ${CC_LIBS}` )			# save
set CC_LIBS = ( )					# empty
@ l = 0							# reset counter
while ( ${l} < ${#CC_LIBT} )				# loop
	@ l++						# increment
	switch( ${CC_LIBT[${l}]} )			# switch
	case *giplib.a*:				# got you
		breaksw
	default:					# go on
		set CC_LIBS = ( ${CC_LIBS} ${CC_LIBT[${l}]} )
		breaksw
	endsw
end
set FC_LIBT = ( `echo ${FC_LIBS}` )			# save
set FC_LIBS = ( )					# empty
@ l = 0							# reset counter
while ( ${l} < ${#FC_LIBT} )				# loop
	@ l++						# increment
	switch( ${FC_LIBT[${l}]} )			# switch
	case *giplib.a*:				# got you
		breaksw
	default:					# go on
		set FC_LIBS = ( ${FC_LIBS} ${FC_LIBT[${l}]} )
		breaksw
	endsw
end
echo "cd ${gip_tmp}"					# message
cd ${gip_tmp}						# change dir
#
# Registration
#
if ( -x ${gip_sys}/register.sh ) then
	${gip_sys}/register.sh install
endif
#
# Installing compile
#
if ( -e ${gip_tsk}/compile.c == 0 ) then		# error
	echo "${gip_tsk}/compile.c not found\!"		# message
	exit( 1 )					# quit
endif
if ( `find ${gip_exe} -name compile -newer ${gip_tsk}/compile.c -print | awk 'END{print NR}'` == 0 ) then
	echo "Installing compile ..."			# message
	echo "cp ${gip_tsk}/compile.c ."		# message
	cp ${gip_tsk}/compile.c .			# copy
	echo "${CC_COMP} ${CC_OPTS} -o compile compile.c ${gip_sub}/xclib.c ${gip_sub}/gip_lock.c ${CC_LIBS}"
	${CC_COMP} ${CC_OPTS} -o compile compile.c ${gip_sub}/xclib.c ${gip_sub}/gip_lock.c ${CC_LIBS}
	@ stat = ${status}				# get status
	if ( ${stat} != 0 ) then			# error
		exit( 1 )				# quit
	else if ( -x ${gip_tmp}/compile == 0 ) then	# something wrong
		exit( 1 )				# quit
	endif						# }
	echo "cp compile ${gip_exe}/compile"		# message
	cp compile ${gip_exe}/compile			# install it
endif							# }
if ( -x ${gip_exe}/compile != 0 ) then			# exists
	if ( ${nosuid} == 0 ) then			# set uid
		echo "chmod ug+s ${gip_exe}/compile"	# message
		chmod ug+s ${gip_exe}/compile		# do it
	endif						# }
endif							# }
#
# Installing f2cvv
#
if ( -e ${gip_tsk}/f2cvv.c == 0 ) then			# error
	echo "${gip_tsk}/f2cvv.c not found\!"		# message
	exit( 1 )					# quit
endif
if ( `find ${gip_exe} -name f2cvv -newer ${gip_tsk}/f2cvv.c -print | awk 'END{print NR}'` == 0 ) then
	echo "Installing f2cvv ..."			# message
	echo "cp ${gip_tsk}/f2cvv.c ."			# message
	cp ${gip_tsk}/f2cvv.c .				# copy
	echo "${CC_COMP} ${CC_OPTS} -o f2cvv f2cvv.c ${gip_sub}/xclib.c ${CC_LIBS}"
	${CC_COMP} ${CC_OPTS} -o f2cvv f2cvv.c ${gip_sub}/xclib.c ${CC_LIBS}
	@ stat = ${status}				# get status
	if ( ${stat} != 0 ) then			# error
		exit( 1 )				# quit
	else if ( -x ${gip_tmp}/f2cvv == 0 ) then	# something wrong
		exit( 1 )				# quit
	endif						# }
	echo "f2cvv f2cvvdefs.h"			# message
	f2cvv f2cvvdefs.h				# run it
	echo "cp f2cvvdefs.h ${gip_inc}/f2cvvdefs.h"	# message
	cp f2cvvdefs.h ${gip_inc}/f2cvvdefs.h		# copy it
	echo "cp f2cvv ${gip_exe}/f2cvv"		# message
	cp f2cvv ${gip_exe}/f2cvv			# install it
endif							# }
#
# Installing xfile
#
if ( -e ${gip_tsk}/xfile.c == 0 ) then			# error
	echo "${gip_tsk}/xfile.c not found\!"		# message
	exit( 1 )					# quit
endif
if ( `find ${gip_exe} -name xfile -newer ${gip_tsk}/xfile.c -print | awk 'END{print NR}'` == 0 ) then
	echo "Installing xfile ..."			# message
	echo "cp ${gip_tsk}/xfile.c ."			# message
	cp ${gip_tsk}/xfile.c .				# copy it
	echo "${CC_COMP} ${CC_OPTS} -o xfile xfile.c ${gip_sub}/xclib.c ${CC_LIBS}"
	${CC_COMP} ${CC_OPTS} -o xfile xfile.c ${gip_sub}/xclib.c ${CC_LIBS}
	@ stat = ${status}				# get status
	if ( ${stat} != 0 ) then			# error
		exit( 1 )				# quit
	else if ( -x ${gip_tmp}/xfile == 0 ) then	# something wrong
		exit( 1 )				# quit
	endif						# }
	echo "cp xfile ${gip_exe}/xfile"		# message
	cp xfile ${gip_exe}/xfile			# install it
endif							# }
#
# Installing lckserver
#
if ( -e ${gip_tsk}/lckserver.c == 0 ) then		# error
	echo "${gip_tsk}/lckserver.c not found\!"	# message
	exit( 1 )					# quit
endif
if ( `find ${gip_exe} -name lckserver -newer ${gip_tsk}/lckserver.c -print | awk 'END{print NR}'` == 0 ) then
	echo "Installing lckserver ..."			# message
	echo "cp ${gip_tsk}/lckserver.c ."		# message
	cp ${gip_tsk}/lckserver.c .			# copy it
	echo "${CC_COMP} ${CC_OPTS} -o lckserver lckserver.c ${gip_sub}/xclib.c ${CC_LIBS}"
	${CC_COMP} ${CC_OPTS} -o lckserver lckserver.c ${gip_sub}/xclib.c ${CC_LIBS}
	@ stat = ${status}				# get status
	if ( ${stat} != 0 ) then			# error
		exit( 1 )				# quit
	else if ( -x ${gip_tmp}/lckserver == 0 ) then	# something wrong
		exit( 1 )				# quit
	endif						# }
	echo "cp lckserver ${gip_exe}/lckserver"	# message
	cp lckserver ${gip_exe}/lckserver		# install it
endif							# }
#
# Installing gftp
#
if ( -e ${gip_tsk}/gftp.c == 0 ) then			# error
	echo "${gip_tsk}/gftp.c not found\!"		# message
	exit( 1 )					# quit
endif
if ( `find ${gip_exe} -name gftp -newer ${gip_tsk}/gftp.c -print | awk 'END{print NR}'` == 0 ) then
	echo "Installing gftp ..."			# message
	echo "cp ${gip_tsk}/gftp.c ."			# message
	cp ${gip_tsk}/gftp.c .				# copy
	echo "${CC_COMP} ${CC_OPTS} -o gftp gftp.c ${gip_sub}/xclib.c ${CC_LIBS}"
	${CC_COMP} ${CC_OPTS} -o gftp gftp.c ${gip_sub}/xclib.c ${CC_LIBS}
	@ stat = ${status}				# get status
	if ( ${stat} != 0 ) then			# error
		exit( 1 )				# quit
	else if ( -x ${gip_tmp}/gftp == 0 ) then	# something wrong
		exit( 1 )				# quit
	endif						# }
	echo "cp gftp ${gip_exe}/gftp"			# message
	cp gftp ${gip_exe}/gftp				# install it
endif							# }
#
# Testing connection with GIPSY source server.
#
if ( -e server.new ) then				# present
	\rm -f server.new				# remove it
endif							# }
${gip_exe}/gftp tst					# test gftp
@ stat = ${status}					# get status
if ( ${stat} != 0 ) then				# error
	echo "Warning, cannot contact GIPSY source server"
else if ( -e server.new ) then				# created by gftp
	if ( -e ${gip_loc}/server ) then		# move
		mv ${gip_loc}/server ${gip_loc}/server.old
	endif						# }
	echo "cp server.new ${gip_loc}/server"		# message
	cp server.new ${gip_loc}/server			# copy
endif							# }
#
# Installing sheltran compiler
#
if ( -e ${gip_tsk}/sheltran.src == 0 ) then		# error
	echo "${gip_tsk}/sheltran.src not found\!"	# message
	exit( 1 )					# quit
endif
if ( -e ${gip_sub}/xflib.c == 0 ) then			# error
	echo "${gip_sub}/xflib.c not found\!"		# message
	exit( 1 )					# quit
endif
if ( `find ${gip_exe} -name sheltran -newer ${gip_tsk}/sheltran.src -print | awk 'END{print NR}'` == 0 ) then
	echo "Installing sheltran ..."			# message
	echo "cp ${gip_sub}/xflib.c ."			# copy this
	cp ${gip_sub}/xflib.c .				# do it
	echo "${gip_exe}/f2cvv xflib.c"			# run f2cvv
	${gip_exe}/f2cvv xflib.c			# do it
	echo "${CC_COMP} -c ${CC_OPTS} xflib.c xflib_ftoc.c"
	${CC_COMP} -c ${CC_OPTS} xflib.c xflib_ftoc.c	# compile it
	@ stat = ${status}				# get status
	if ( ${stat} != 0 ) then			# error
		exit( 1 )				# quit
	endif
	echo "cp ${gip_tsk}/sheltran.src ."		# message
	cp ${gip_tsk}/sheltran.src .			# copy it
	echo "${gip_exe}/xfile sheltran.src"		# message
	${gip_exe}/xfile sheltran.src			# run xfile
	echo "${FC_COMP} -o sheltran sheltran.f xflib.o xflib_ftoc.o ${FC_OPTS} ${FC_LIBS}"
	${FC_COMP} -o sheltran sheltran.f xflib.o xflib_ftoc.o ${FC_OPTS} ${FC_LIBS}
	@ stat = ${status}				# get status
	if ( ${stat} != 0 ) then			# error
		exit( 1 )				# quit
	else if ( -x ${gip_tmp}/sheltran == 0 ) then	# something wrong
		exit( 1 )				# quit
	endif						# }
	echo "cp sheltran ${gip_exe}/sheltran"		# message
	cp sheltran ${gip_exe}/sheltran			# install it
endif							# }
if ( -e ${gip_sys}/pgpack.f == 0 ) then			# error
	echo "${gip_sys}/pgpack.f not found\!"		# message
	exit( 1 )					# quit
else if ( -e ${gip_lib}/grfont.dat == 0 ) then		# compile
	echo "Installing pgplot fonts ..."		# message
	echo "cp ${gip_sys}/pgpack.f ."			# message
	cp ${gip_sys}/pgpack.f .			# copy it
	echo "cp ${gip_sys}/grfont.txt ."		# message
	cp ${gip_sys}/grfont.txt .			# copy it
	echo "${FC_COMP} -o pgpack pgpack.f ${FC_OPTS} ${FC_LIBS}"
	${FC_COMP} -o pgpack pgpack.f ${FC_OPTS} ${FC_LIBS}
	@ stat = ${status}				# get status
	if ( ${stat} != 0 ) then			# error
		exit( 1 )				# quit
	else if ( -x ${gip_tmp}/pgpack == 0 ) then	# something wrong
		exit( 1 )				# quit
	endif						# }
	echo "pgpack"					# message
	./pgpack					# run it
	if ( -e grfont.dat == 0 ) then			# something wrong
		echo "NO grfont.dat created"		# message
		exit( 1 )				# quit
	endif						# }
	echo "cp grfont.dat ${gip_lib}/grfont.dat"	# message
	cp grfont.dat ${gip_lib}/grfont.dat		# install it
endif							# }
\rm -f ${gip_tmp}/* >& /dev/null			# remove all tmp files
#
# Check for existence of findx.sh
#
if ( -e ${gip_sys}/findx.sh == 0 ) then			# not present
	echo "${gip_sys}/findx.sh missing"		# message
	exit 1						# quit
else if ( -x ${gip_sys}/findx.sh == 0 ) then		# not executable
	chmod +x ${gip_sys}/findx.sh			# make it so
endif							# }
#
# Now check the X11 compiler options
#
echo 'Checking for X11 compiler options ...'
if ( ${nfields} > 15 ) then
	set xinc = `echo ${fields} | awk -F':' '{ printf $16; }'`
else
	set xinc = ( `${gip_sys}/findx.sh includes` )	# get switches
endif
if ( ${#xinc} == 0 ) then				# nothing found
	echo 'Default X11 compiler options assumed.'
else
	echo "... will use for X11 compiler options : ${xinc}"
endif							# }
#
# Now check the X11 linker options
#
echo 'Checking for X11 linker options ...'			# message
if ( ${nfields} > 16 ) then
	set xlib = `echo ${fields} | awk -F':' '{ printf $17; }'`
else
	set xlib = ( `${gip_sys}/findx.sh library` )	# get switches
endif
if ( ${#xlib} == 0 ) then				# nothing found
	echo 'No X11 linker options found\!\!'
	echo 'You must ask your system manager what they are or find out yourself'
	echo 'and put them into the 17th field of the setup file, i.e.'
	echo '-L/usr/openwin/lib/ -lX11'
	exit 1						# exit with status
endif							# }
echo "... will use for X11 linker options   : ${xlib}"
#
# Now check the XToolkit linker options
#
echo 'Checking the X-Toolkit linker options ...'		# message
if ( ${nfields} > 17 ) then
	set xtlib = `echo ${fields} | awk -F':' '{ printf $18; }'`
else
	set xtlib = ( `${gip_sys}/findx.sh libraries` )	# get switches
endif
if ( ${#xtlib} == 0 ) then				# nothing found
	echo 'No X11 toolkit options found\!\!'
	echo 'You must ask your system manager what they are or find out yourself'
	echo 'and put them into the 18th field of the setup file, i.e.'
	echo '-L/usr/openwin/lib/ -lXaw -lXmu -lXt -lXext -lX11'
	exit 1						# exit with status
endif							# }
echo "... will use for X-Toolkit options    : ${xtlib}"
#
echo "INSTALL finished\!"				# message
