#!/bin/csh -f
# sysgen.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1993
#       All Rights Reserved.
#
##>            sysgen.doc
#
#Script:       sysgen
#
#Purpose:      Generates the GIPSY system.
#
#Category:     MANAGEMENT
#
#File:         sysgen.csh
#
#Author:       K.G. Begeman
#
#Use:          sysgen.csh
#
#Description:  This script obtains the necessary files to compile and run
#              GIPSY. The script should be run in the GIPSY ROOT directory,
#              i.e. the directory where all the GIPSY sub directories will
#              be created. It tries to obtain all necessary files via
#              gftp from the GIPSY server at the Kapteyn Astronomical
#              Institute.
#              You should obtain the executable of gftp from the anonymous
#              ftp directory at kapteyn.astro.rug.nl and copy
#              it (in binary mode) from gipsy/<machine-architecture>/gftp to
#              the directory where you want to run this script.
#              Then type: chmod +x gftp to make it executable.
#              The procedure to install GIPSY via this script is as
#              follows:
#
#               1) Goto the directory where you want to have all the
#                  GIPSY sub directories  (the so called GIPSY ROOT).
#                  Also the gftp executable should be here.
#               2) type: sysgen.csh
#                  This command should get the necessary files from the
#                  GIPSY source server. If the script complains that some
#                  files could not be retrieved, run it again until it
#                  does not complain anymore.
#               3) type: setenv gip_root `pwd`
#                  This defines the GIPSY ROOT directory
#               4) type: cd sys
#                  Goto the directory where all the GIPSY scripts are.
#               5) type: mkclient.csh 199
#                  This command makes the current host a GIPSY client,
#                  i.e. it creates an entry in the clients file. The modified
#                  files is in the current directory and is called clients.new.
#                  You should check this file and if necessary, modify it.
#                  Usually the fields which are modified are the paths to the
#                  GIPSY exe, lib and tmp directory. A description of the
#                  clients file and the mkclient.csh script can be found in
#                  ../doc/clients.doc and ../doc/mkclient.doc resp.
#               6) type: mv clients.new ../loc/clients
#               7) type: source gipenv.csh
#                  This command sets the GIPSY enviroment variables. A
#                  description of the gipenv.csh script can be found in
#                  ../doc/gipenv.doc.
#               8) The next step is to create the setup file for the compilers
#                  in the $gip_loc directory. You can check the
#                  $gip_sys/setup.mgr file for entries which match your local
#                  situation, and copy the relevant part to ${gip_loc}/setup.
#                  You also might want to create a manager file. A desription
#                  can be found in $gip_doc/manager.doc.
#               9) type: install.csh
#                  This script compiles the basic GIPSY programmes necessary
#                  to create the GIPSY library and applications.
#                  A description can be found in $gip_doc/install.doc.
#              10) Read $gip_doc/server.doc and check whether your machine
#                  can resolve the address of the GIPSY source server.
#                  If not, you must create a file server in $gip_loc with
#                  the correct address of the GIPSY source server.
#              11) type: p -sysgen >& p.log &
#                  This command will generate the GIPSY library and the
#                  GIPSY applications. It will retrieve the sources from
#                  the GIPSY source server and compile them.
#              12) If compilation has been successfull, you should put
#                  in your .cshrc the following lines:
#                  setenv gip_root <path to GIPSY ROOT>
#                  source ${gip_root}/sys/gipenv.csh
#                  If you want to receive regular updates of GIPSY
#                  applications, you should read $gip_loc/update.doc
#                  In order to get a working system, you should read
#                  in $gip_doc grdevices.doc, lpdevices.doc, mtdevices.doc
#                  and tvdevices.doc.  
#
#Updates:      Jan 11, 1993: KGB, Document created
#              Sep 22, 2000: JPT, Add copy .template files to $gip_loc
#
##<
#
# Here we start the real job
#
# Set the lists of files we need
#
set spc = ( history offspring )
set doc = ( clients.doc compile.doc cshrc.doc f2cvv.doc getarch.doc gipenv.doc grdevices.doc install.doc lckserver.doc lpdevices.doc manager.doc mkclient.doc mtdevices.doc offspring.doc options.doc server.doc setup.doc sysgen.doc tvdevices.doc update.doc )
set sub = ( gip_lock.c int16.c int32.c sockio.c xclib.c xflib.c )
set sys = ( COPYRIGHT README cshrc.csh f2c.csh findx.sh getarch.csh gipenv.csh grfont.txt install.csh manager.mgr mkclient.csh options.mgr pgpack.f register.sh server.mgr setup.mgr sysgen.csh )
set tsk = ( compile.c f2cvv.c gftp.c lckserver.c sheltran.src xfile.c )
#
# Some variables
#
setenv gip_root `\pwd`				# set it
echo "Using ${gip_root} for GIPSY ROOT"		# message
setenv gip_dat ${gip_root}/dat			# set it
if ( -e ${gip_dat} == 0 ) then			# not present
	echo "Creating ${gip_dat}"		# message
	\mkdir ${gip_dat}			# make it
endif						# }
setenv gip_doc ${gip_root}/doc			# set it
if ( -e ${gip_doc} == 0 ) then			# not present
	echo "Creating ${gip_doc}"		# message
	\mkdir ${gip_doc}			# make it
endif						# }
setenv gip_htm ${gip_root}/htm			# set it
if ( -e ${gip_htm} == 0 ) then			# not present
	echo "Creating ${gip_htm}"		# message
	\mkdir ${gip_htm}			# make it
endif						# }
setenv gip_inc ${gip_root}/inc			# set it
if ( -e ${gip_inc} == 0 ) then			# not present
	echo "Creating ${gip_inc}"		# message
	\mkdir ${gip_inc}			# make it
endif						# }
setenv gip_loc ${gip_root}/loc			# set it
if ( -e ${gip_loc} == 0 ) then			# not present
	echo "Creating ${gip_loc}"		# message
	\mkdir ${gip_loc}			# make it
        cp ${gip_root}/sys/grdevices.template ${gip_loc}/grdevices
        cp ${gip_root}/sys/lpdevices.template ${gip_loc}/lpdevices
        cp ${gip_root}/sys/mtdevices.template ${gip_loc}/mtdevices
        cp ${gip_root}/sys/tvdevices.template ${gip_loc}/tvdevices
endif						# }
setenv gip_mis ${gip_root}/mis			# set it
if ( -e ${gip_mis} == 0 ) then			# not present
	echo "Creating ${gip_mis}"		# message
	\mkdir ${gip_mis}			# make it
endif						# }
setenv gip_old ${gip_root}/old			# set it
if ( -e ${gip_old} == 0 ) then			# not present
	echo "Creating ${gip_old}"		# message
	\mkdir ${gip_old}			# make it
endif						# }
setenv gip_sub ${gip_root}/sub			# set it
if ( -e ${gip_sub} == 0 ) then			# not present
	echo "Creating ${gip_sub}"		# message
	\mkdir ${gip_sub}			# make it
endif						# }
setenv gip_sys ${gip_root}/sys			# set it
if ( -e ${gip_sys} == 0 ) then			# not present
	echo "Creating ${gip_sys}"		# message
	\mkdir ${gip_sys}			# make it
endif						# }
setenv gip_tsk ${gip_root}/tsk			# set it
if ( -e ${gip_tsk} == 0 ) then			# not present
	echo "Creating ${gip_tsk}"		# message
	\mkdir ${gip_tsk}			# make it
endif						# }
#
# Check whether gftp has been fetched.
#
if ( -e ${gip_root}/gftp ) then			# it's here
	if ( -x ${gip_root}/gftp == 0 ) then	# not executable
		echo "chmod +x ${gip_root}/gftp"
		\chmod +x ${gip_root}/gftp
	endif
	echo "Using gftp to get the files"
#
# Testing connection with GIPSY source server.
#
	if ( -e server.new ) then			# present
		\rm -f server.new			# remove it
	endif						# }
	${gip_root}/gftp tst				# test gftp
	@ stat = ${status}				# get status
	if ( ${stat} != 0 ) then			# error
		echo "FATAL, cannot contact GIPSY source server"
		exit 1
	else if ( -e server.new ) then			# created by gftp
		if ( -e ${gip_loc}/server ) then	# move
			\rm -f ${gip_loc}/server
		endif					# }
		echo "cp server.new ${gip_loc}/server"	# message
		\cp server.new ${gip_loc}/server	# copy
	endif						# }
else 
	echo "${gip_root}/gftp not found"
	echo "... Maybe you forgot to fetch it ..."
	exit 1
endif
#
# Get gipsy root at site of GIPSY source server
#
if ( -e ${gip_loc}/server ) then
	setenv rem_root `\awk -F: '{if(NF==3)printf("%s\n",$3)}' ${gip_loc}/server`
else
	setenv rem_root /tha3/users/gipsy
endif
#
# Set error counter
#
@ ecount = 0					# reset
#
# First get the special files
#
foreach f ( ${spc} )
	echo -n "Obtaining ${f} ..."
	${gip_root}/gftp get ${rem_root}/sys/.${f}size .${f}size
	if ( -e .${f}size == 0 ) then
		echo " FATAL"
		exit 1
	else					# }{
		set dummy = `\wc -c .${f}size`
		@ size = ${dummy[1]}
		if ( ${size} != 10 ) then
			echo " FATAL"
			\rm -f .${f}size
			exit 1
		else
			@ fsize = `cat .${f}size`
			\rm -f .${f}size
			echo -n " ${fsize} bytes ..."
		endif
	endif					# }
	${gip_root}/gftp get ${rem_root}/sys/${f} ${f}
	if ( -e ${f} == 0 ) then
		echo " FATAL"
		exit 1
	else					# }{
		set dummy = `\wc -c ${f}`
		@ size = ${dummy[1]}
		if ( ${size} != ${fsize} ) then
			echo " FATAL"
			\rm -f ${f}
			exit 1
		else
			echo " done"
		endif
	endif					# }
end
#
# Get the necessary documents.
#
cd ${gip_doc}					# change directory
echo -n "Checking files in ${gip_doc} ..."	# message
foreach f ( ${doc} )				# loop through list
	set dummy = ( `\awk -F: '{if($1=="'${f}'"&&NF>=9)print $9}' ${gip_root}/history` )
	if ( ${#dummy} ) then
		@ size = ${dummy[${#dummy}]}
	else
		@ size = 0
	endif
	if ( ${size} && -e ${f} ) then
		set dummy = ( `\wc -c ${f}` )
		@ fsize = ${dummy[1]}
		if ( ${size} != ${fsize} ) then
			\rm -f ${f}
		endif
	endif
	if ( -e ${f} == 0 ) then		# not present
		${gip_root}/gftp get ${rem_root}/doc/${f} ${f}
		if ( -e ${f} == 0 ) then
			@ ecount++		# increase
			echo -n " -${f}"	# message
		else if ( ${size} ) then	# }{
			set dummy = ( `\wc -c ${f}` )
			@ fsize = ${dummy[1]}
			if ( ${size} != ${fsize} ) then
				echo -n " --${f}"# message
  			else
				echo -n " ++${f}"# message
	 		endif
		else
			echo -n " +${f}"	# message
		endif				# }
	endif					# }
end						# }
echo " ... done"				# message
#
# Get the necessary subroutines
#
cd ${gip_sub}					# change directory
echo -n "Checking files in ${gip_sub} ..."	# message
foreach f ( ${sub} )				# loop through list
	set dummy = ( `\awk -F: '{if($1=="'${f}'"&&NF>=9)print $9}' ${gip_root}/history` )
	if ( ${#dummy} ) then
		@ size = ${dummy[${#dummy}]}
	else
		@ size = 0
	endif
	if ( ${size} && -e ${f} ) then
		set dummy = ( `\wc -c ${f}` )
		@ fsize = ${dummy[1]}
		if ( ${size} != ${fsize} ) then
			\rm -f ${f}
		endif
	endif
	if ( -e ${f} == 0 ) then	# not present
		${gip_root}/gftp get ${rem_root}/sub/${f} ${f}
		if ( -e ${f} == 0 ) then
			@ ecount++		# increase
			echo -n " -${f}"	# message
		else if ( ${size} ) then	# }{
			set dummy = ( `\wc -c ${f}` )
			@ fsize = ${dummy[1]}
			if ( ${size} != ${fsize} ) then
				echo -n " --${f}"# message
  			else
				echo -n " ++${f}"# message
	 		endif
		else
			echo -n " +${f}"	# message
		endif				# }
	endif					# }
end						# }
echo " ... done"				# message
#
# Get the necessary system files
#
cd ${gip_sys}					# change directory
echo -n "Checking files in ${gip_sys} ..."	# message
foreach f ( ${sys} )				# loop through list
	set dummy = ( `\awk -F: '{if($1=="'${f}'"&&NF>=9)print $9}' ${gip_root}/history` )
	if ( ${#dummy} ) then
		@ size = ${dummy[${#dummy}]}
	else
		@ size = 0
	endif
	if ( ${size} && -e ${f} ) then
		set dummy = ( `\wc -c ${f}` )
		@ fsize = ${dummy[1]}
		if ( ${size} != ${fsize} ) then
			\rm -f ${f}
		endif
	endif
	if ( -e ${f} == 0 ) then		# not present
		${gip_root}/gftp get ${rem_root}/sys/${f} ${f}
		if ( -e ${f} == 0 ) then
			@ ecount++		# increase
			echo -n " -${f}"	# message
		else if ( ${size} ) then	# }{
			set dummy = ( `\wc -c ${f}` )
			@ fsize = ${dummy[1]}
			if ( ${size} != ${fsize} ) then
				echo -n " --${f}"# message
  			else
				echo -n " ++${f}"# message
				if ( ${f:e} == 'csh' ) then
					chmod +x ${f}	# make executable
				else if ( ${f:e} == 'sh' ) then
					chmod +x ${f}	# make executable
				endif			# }
	 		endif
		else
			echo -n " +${f}"	# message
			if ( ${f:e} == 'csh' ) then
				chmod +x ${f}	# make executable
			else if ( ${f:e} == 'sh' ) then
				chmod +x ${f}	# make executable
			endif			# }
		endif				# }
	endif					# }
end						# }
echo " ... done"				# message
#
# Get the necessary tasks
#
cd ${gip_tsk}					# change directory
echo -n "Checking files in ${gip_tsk} ..."	# message
foreach f ( ${tsk} )				# loop through list
	set dummy = ( `\awk -F: '{if($1=="'${f}'"&&NF>=9)print $9}' ${gip_root}/history` )
	if ( ${#dummy} ) then
		@ size = ${dummy[${#dummy}]}
	else
		@ size = 0
	endif
	if ( ${size} && -e ${f} ) then
		set dummy = ( `\wc -c ${f}` )
		@ fsize = ${dummy[1]}
		if ( ${size} != ${fsize} ) then
			\rm -f ${f}
		endif
	endif
	if ( -e ${f} == 0 ) then		# not present
		${gip_root}/gftp get ${rem_root}/tsk/${f} ${f}
		if ( -e ${f} == 0 ) then
			@ ecount++		# increase
			echo -n " -${f}"	# message
		else if ( ${size} ) then	# }{
			set dummy = ( `\wc -c ${f}` )
			@ fsize = ${dummy[1]}
			if ( ${size} != ${fsize} ) then
				echo -n " --${f}"# message
  			else
				echo -n " ++${f}"# message
	 		endif
		else
			echo -n " +${f}"	# message
		endif				# }
	endif					# }
end						# }
echo " ... done"				# message
#
# Get the standard includes
#
set inc = ( `\awk -F: '{if(NF>=2&&substr($1,length($1)-1,2)==".h")print $1}' ${gip_root}/offspring` )
cd ${gip_inc}					# change directory
echo -n "Checking files in ${gip_inc} ..."	# message
foreach f ( ${inc} )				# loop through list
	set dummy = ( `\awk -F: '{if($1=="'${f}'"&&NF>=9)print $9}' ${gip_root}/history` )
	if ( ${#dummy} ) then
		@ size = ${dummy[${#dummy}]}
	else
		@ size = 0
	endif
	if ( ${size} && -e ${f} ) then
		set dummy = ( `\wc -c ${f}` )
		@ fsize = ${dummy[1]}
		if ( ${size} != ${fsize} ) then
			\rm -f ${f}
		endif
	endif
	if ( -e ${f} == 0 ) then		# not present
		${gip_root}/gftp get ${rem_root}/inc/${f} ${f}
		if ( -e ${f} == 0 ) then
			@ ecount++		# increase
			echo -n " -${f}"	# message
		else if ( ${size} ) then	# }{
			set dummy = ( `\wc -c ${f}` )
			@ fsize = ${dummy[1]}
			if ( ${size} != ${fsize} ) then
				echo -n " --${f}"# message
  			else
				echo -n " ++${f}"# message
	 		endif
		else
			echo -n " +${f}"	# message
		endif				# }
	endif					# }
end						# }
echo " ... done"				# message
#
# the offspring includes
#
set inc = ( `\awk -F: '{if(NF>=2&&substr($2,length($2)-1,2)==".h")print $2}' ${gip_root}/offspring` )
cd ${gip_inc}					# change directory
echo -n "Checking files in ${gip_inc} ..."	# message
foreach f ( ${inc} )				# loop through list
	if ( -e ${gip_inc}/${f} == 0 ) then	# not present
		${gip_root}/gftp get ${rem_root}/inc/${f} ${f}
		if ( -e ${gip_inc}/${f} == 0 ) then
			@ ecount++		# increase
			echo -n " -${f}"	# message
		else				# }{
			echo -n " +${f}"	# message
		endif				# }
	endif					# }
end						# }
echo " ... done"				# message
#
# Get the necessary data files
#
cd ${gip_dat}					# change directory
${gip_root}/gftp get ${rem_root}/dat/dat.dir dat.dir
if ( -e dat.dir ) then				# file is present
	set dat = ( `cat dat.dir` )		# make list
	echo -n "Checking files in ${gip_dat} ..."
	foreach f ( ${dat} )			# loop through list
		set lf = ${gip_dat}/${f}	# local file name
		set dir = ${lf:h}		# the path
		while ( -d ${dir} == 0 )	# directory not present
			set dirf = ${dir}	# save
			set dirs = ${dir:h}	# strip
			while ( -d ${dirs} == 0 )
				set dirf = ${dirs}
				set dirs = ${dirf:h}
			end			# }
			\mkdir ${dirf}		# make it
		end				# }
		if ( -e ${lf} == 0 ) then	# not present
			${gip_root}/gftp get ${rem_root}/dat/${f} ${lf}
			if ( -e ${lf} == 0 ) then
				@ ecount++	# increase
				echo -n " -${f}"
			else			# }{
				echo -n " +${f}"
			endif			# }
		endif				# }
	end					# }
	echo " ... done"			# message
endif						# }
#
# Check for some specific files
#
if ( -e ${gip_sys}/bookkeeper == 0 ) then	# not present
	echo "Creating ${gip_sys}/bookkeeper"	# message
	echo 'aix:alliant:alpha:convex:cray:hp9000s300:hp9000s700:linux:mips:sgi:sol4:sun4:sun386i' > ${gip_sys}/bookkeeper
endif						# }
if ( -e ${gip_sys}/history == 0 ) then		# not present
	echo "Creating ${gip_sys}/history"	# message
	\touch ${gip_sys}/history		# make it
endif						# }
if ( -e ${gip_sys}/offspring == 0 ) then	# not present
	echo "Creating ${gip_sys}/offspring"	# message
	\touch ${gip_sys}/offspring		# make it
endif						# }
#
# Remove the special files
#
cd ${gip_root}
foreach f ( $spc)
	\rm -f ${f}
end
#
# Check for errors
#
if ( ${ecount} > 0 ) then			# errors occurred
	echo "${ecount} file(s) could not be retrieved"
	exit 1					# fatal
endif						# }
