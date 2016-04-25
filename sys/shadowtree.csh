#!/bin/csh -f
# shadowtree.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1993
#       All Rights Reserved.
#
##>            shadowtree.doc
#
#Script:       shadowtree
#
#Purpose:      Creates a shadowtree (i.e. creates an new branch with
#              linked leaves).
#
#Category:     SYSTEM MANAGEMENT
#
#File:         shadowtree.csh
#
#Author:       K.G. Begeman
#
#Use:          shadowtree fromdir [todir]
#
#              fromdir    branch with real leaves
#              todir      branch with linked leaves [current directory]
#
#Updates:      Aug 22, 1990: KGB, Document created.
#
##<
#
@ nargs = ${#argv}				# number of args
if ( "${nargs}" < 1 ) then			# not enough
   echo "Usage: shadowtree fromdir [todir]"	# message
   exit( 0 );					# quit
endif						# cif
set pwd = `pwd`					# here we are
cd "${argv[1]}"					# DIRFROM
set DIRFROM = `pwd`				# ..
cd "${pwd}"					# back to start
if ( "${nargs}" > 1 ) then			# DIRTO from command line
	cd "${argv[2]}"				# goto DIRTO
	set DIRTO = `pwd`			# set it
	cd "${pwd}"				# back to start
else						# else
	set DIRTO = `pwd`			# here
endif						# cif
if ( ! -d "${DIRTO}" ) then
	echo "shadowtree: ${DIRTO} is not a directory"
	exit( 2 );
endif
if ( ! -d "${DIRFROM}" ) then
	echo "shadowtree: ${DIRFROM} is not a directory"
	exit( 2 );
endif
if ( "${DIRFROM}" == "${DIRTO}" ) then
	echo "${DIRFROM}: FROM and TO are identical!"
	exit( 1 )
endif
foreach file (`ls ${DIRFROM}`)
	if ( ! -d "${DIRFROM}/${file}" ) then
		ln -s "${DIRFROM}/${file}" "${DIRTO}/${file}"
	else
		echo "${file}"':'
		if ( ! -d "${DIRTO}/${file}" ) then
			mkdir "${DIRTO}/${file}"
			${gip_sys}/shadowtree.csh "${DIRFROM}/${file}" "${DIRTO}/${file}"
		endif
	endif
end
