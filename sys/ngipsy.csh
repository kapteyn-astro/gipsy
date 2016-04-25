#!/bin/csh -f
# ngipsy.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992
#       All Rights Reserved.
#
##>            ngipsy.doc
#
#Script:       ngipsy
#
#Purpose:      Starts a GIPSY session with nhermes.
#
#Category:     MANAGEMENT
#
#File:         ngipsy.csh
#
#Author:       K. Begeman
#
#Use:          ngipsy [ -l<log file name> ] [ -t<minutes> ] 
#              { <user command> }
#
#Description:  The ngipsy script will start a GIPSY session. The gipsy
#              environment must have been defined. The script looks for
#              $HOME/.gipsyrc, and if this file is present it will source it.
#
#Directory:    $gip_sys/
#
#
#Related Docs: xgipsy.doc, gipsy.doc
#
#Updates:      Oct  7, 1991: KGB, script create.
#              Apr  2, 2002: JPT, improved command line argument passing.
#              May 30, 2001: JPT, undefined Ggi-incompatibe environment vars.
#              Nov 15, 2004: JPT, removed client exclusion test.
#              Nov  1, 2006: JPT, Python support added
#              Aug 20, 2007: JPT, changed PYTHONPATH for new Python module.
#              May 27, 2008: JPT, merge PYTHONPATH with existing PYTHONPATH
#              Mar 22, 2011: JPT, reversed PYTHONPATH merge order.
#              Jan 21, 2014: JPT, enforce C number format.
#
##<
# .............................  initialisation  .............................
#
setenv LC_NUMERIC "C"                           # C-format numbers
set client = `hostname`				# name of gipsy client
if (${?gip_root} == 0 ) then			# no GIPSY environment
	if ($0 != '') then			# command line argv[0]
		set tmp = $0			# command line
		setenv gip_sys ${tmp:h}		# path to gip_sys
		if ( -e ${gip_sys}/gipenv.csh ) then
			set tmp = ${gip_sys}
			setenv gip_root ${tmp:h}
			source ${gip_root}/sys/gipenv.csh
		endif
	endif
endif
if (${?gip_root} == 0 ) then			# no GIPSY environment
	echo -n 
	echo ''
	echo 'No GIPSY environment defined'
	echo ''
	echo 'Edit your .cshrc file to contain the following lines:'
	echo 'setenv gip_root <path to gipsy root> (ask GIPSY manager)'
	echo 'source ${gip_root}/sys/gipenv.csh'
	echo ''
	exit					# quit
endif
#
# Check for session id.
#
@ narg = ${#argv}				# number of arguments
@ session_id = 0
setenv SESSION_ID ${session_id}			# set session id
#
# Execute $HOME/.gipsyrc if present
#
if ( -e "${HOME}/.gipsyrc" ) then		# user setup wanted
	source "${HOME}/.gipsyrc"		# execute .gipsyrc
endif						# go on
#
# Set the rest of the environment
#
if (${?usr_root} == 0) then			# no usr_root
	setenv	usr_root	${HOME}		# default
endif
if (${?usr_exe} == 0) then			# no usr_exe
	setenv	usr_exe		${usr_root}/exe/${gip_arch}
endif
if (${?ALT_KEYBOARD} == 0) then			# make it
	setenv ALT_KEYBOARD "${usr_root}/.hermes_sockets.${SESSION_ID}"
endif
if (${?DEFAULT_DISPLAY} == 0) then		# make it
	setenv DEFAULT_DISPLAY "${usr_root}/.gids_sockets.${SESSION_ID}"
endif
if (${?THERTEK} == 0) then			# make it
	setenv THERTEK "${usr_root}/.thertek.${SESSION_ID}"
endif
if (${?EXPORT} == 0) then			# make it
	setenv EXPORT `pwd`/export		# disk tape for export
endif
if (${?IMPORT} == 0) then			# make it
	setenv IMPORT `pwd`/import		# disk tape for import
endif
if (${?PGPLOT_FONT} == 0) then			# make it
	setenv PGPLOT_FONT "${gip_lib}/grfont.dat"# the PGPLOT fonts
endif
if (${?TEK_DEVICE} == 0) then			# make it
	setenv TEK_DEVICE /dev/tty
endif
switch( ${gip_arch} )				# machine dependent settings
case alpha:
	limit stacksize unlimited
	breaksw
case convex:
	limit stacksize unlimited
	breaksw
case mips:
	limit stacksize unlimited
	breaksw
default:
	breaksw
endsw
if (${?PYTHONPATH} == 0) then
        setenv PYTHONPATH ${gip_exe}
else
        setenv PYTHONPATH ${PYTHONPATH}:${gip_exe}
endif 
if ( -x ${gip_exe}/nhermes ) then		# we start batch HERMES
        unsetenv PGPLOT_BACKGROUND              # incompatible with Ggi
        unsetenv PGPLOT_FOREGROUND              # incompatible with Ggi
        @ nargs=${#argv}
        @ current=0
        set arguments = ""
        while ($current < $nargs)
           @ current++
           set curarg = (${argv[$current]})
           @ alen = $#curarg
           if ($alen>1) then
              set curarg[1]     =  \'$curarg[1]
              set curarg[$alen] =  $curarg[$alen]\'
           endif
           set arguments = "$arguments $curarg"
        end
	eval ${gip_exe}/nhermes ${arguments}
	if ( -e ${ALT_KEYBOARD} ) then		# it still exists
		rm -f ${ALT_KEYBOARD}		# not anymore (I hope)
	endif
else
	echo "No HERMES found"
endif
