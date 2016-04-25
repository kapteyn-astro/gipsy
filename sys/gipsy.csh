#!/bin/csh -f
# gipsy.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992, 1993, 1995, 2002
#       All Rights Reserved.
#
##>            gipsy.doc
#
#Script:       gipsy
#
#Purpose:      Starts a GIPSY session with thermes.
#
#Category:     MANAGEMENT
#
#File:         gipsy.csh
#
#Author:       K. Begeman
#
#Use:          gipsy
#
#Description:  The gipsy script will start a GIPSY session. The gipsy
#              environment must have been defined.
#
#Use:          $gip_sys/gipsy.csh [session_number] ( when running csh )
#              $gip_sys/gipsy.sh  [session_number] ( when running sh )
#
#              session_number       Gipsy session id [1]. If you want to
#                                   run two gipsy sessions separately on
#                                   the same user account, you need to
#                                   specify different session numbers.
#                                   If you want to run more than one
#                                   display server (gids), you also should
#                                   specify different sessions numbers.
#                                    
#Notes:        The gipsy.csh script looks for $HOME/.gipsyrc, and the
#              gipsy.sh script looks for $HOME/.gipsysc. If present,
#              the script will executae it.
#
#Related Docs: xgipsy.doc
#
#Updates:      Oct  7, 1991: KGB, script create.
#              Mar 23, 1993: KGB, also for sh.
#              Jul  3, 1995: JPT, define WEB_BROWSER.
#              Apr  9, 1996: JPT, search for editor.
#              May 30, 2001: JPT, undefined Ggi-incompatibe environment vars.
#              Aug 22, 2003: JPT, set Python path.
#              Nov 15, 2004: JPT, removed client exclusion tests.
#              Apr 10, 2006: JPT, interfaced Firefox browser.
#              Oct 16, 2006: JPT, also support system-installed Python.
#              Oct 30, 2006: JPT, system Python tried before GIPSY-private.
#              Jul 12, 2007: JPT, changed PYTHONPATH for new Python module.
#              May 27, 2008: JPT, merge PYTHONPATH with existing PYTHONPATH.
#              Aug 23, 2010: JPT, reversed PYTHONPATH merge order.
#              May 31, 2011: JPT, removed command line mode.
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

mkdir -p $HOME/.gipsy/ggidevices                # for Ggi plot server sockets

#
# Check for session id.
#
@ narg = ${#argv}				# number of arguments
if (${narg} == 1) then				# session id entered
	set session_id = ${argv[1]}
	if (${?gids_windowname} == 0) then
		setenv	gids_windowname	GIDS_${session_id}
	endif
else
	set session_id = 1
endif
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
if (${?WEB_BROWSER} == 0) then                  # find WWW browser
	set browsers = ( firefox xmosaic Mosaic netscape arena )
else
	if ( !(-x "${WEB_BROWSER}") ) then
		set browsers = ( ${WEB_BROWSER} )
		unsetenv WEB_BROWSER
	endif
endif
if (${?WEB_BROWSER} == 0) then
	foreach browser ( ${browsers} )
		foreach prefix (${path})
			if ( -x ${prefix}/${browser} ) then
				setenv WEB_BROWSER ${prefix}/${browser}
				break
			endif
		end
		if (${?WEB_BROWSER} != 0) then
			break
		endif
	end
endif
if (${?EDITOR} == 0) then                       # find editor
   set editors = ( mem vi emacs )
else
   if ( !(-x "${EDITOR}") ) then
      set editors = ( ${EDITOR} )
      unsetenv EDITOR
   endif
endif
if (${?EDITOR} == 0) then
   foreach editor ( ${editors} )
      foreach prefix (${path})
         if ( -x ${prefix}/${editor} ) then
            setenv EDITOR ${prefix}/${editor}
            break
          endif
      end
      if (${?EDITOR} != 0) then
         break
      endif
   end
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
#
# Registration
#
if ( -x ${gip_sys}/register.sh ) then
	${gip_sys}/register.sh client
endif
if (${?PYTHONPATH} == 0) then
        setenv PYTHONPATH ${gip_exe}
else
        setenv PYTHONPATH ${PYTHONPATH}:${gip_exe}
endif
if ( -x ${gip_exe}/thermes ) then		# we start terminal HERMES
        unsetenv PGPLOT_BACKGROUND              # incompatible with Ggi
        unsetenv PGPLOT_FOREGROUND              #     ,,        ,,  ,,
	${gip_exe}/thermes
	if ( -e ${ALT_KEYBOARD} ) then		# it still exists
		rm -f ${ALT_KEYBOARD}		# not anymore (I hope)
	endif
else
	echo "Cannot start GIPSY, please check installation log for errors."
endif
##>            gipsy.sh
## gipsy.sh
##
##       Copyright (c) Kapteyn Laboratorium Groningen 1993
##       All Rights Reserved.
##
#if [ "${gip_mode}" = "" ] ; then		# no GIPSY environment
#	echo 
#	echo 'No GIPSY environment defined'
#	echo ''
#	echo 'Edit your .profile file to contain the following lines:'
#	echo 'gip_root= <path to gipsy root> (ask GIPSY manager)'
#	echo 'export gip_root'
#	echo '. $gip_root/sys/gipenv.sh'
#	echo ''
#	exit					# quit
#fi
##
## Check for session id.
##
#if [ "$1" = "" ] ; then
#	session_id=1
#else
#	session_id=$1
#	if [ "${gids_windowname}" = "" ] ; then
#		gids_windowname="GIDS_${session_id}"
#		export gids_windowname
#	fi
#fi
#SESSION_ID=${session_id}
#export SESSION_ID				# set session id
##
## Execute $HOME/.gipsyrs if present
##
#if [ -r "${HOME}/.gipsyrs" ] ; then		# user setup wanted
#	. ${HOME}/.gipsyrs			# execute .gipsyrs
#fi						# go on
##
## Set the rest of the environment
##
#if [ "${usr_root}" = "" ] ; then		# no usr_root
#	usr_root=${HOME}
#	export usr_root
#fi
#if [ "${usr_exe}" = "" ] ; then			# no usr_exe
#	usr_exe=${usr_root}/exe/${gip_arch}
#	export usr_exe
#fi
#if [ "${ALT_KEYBOARD}" = "" ] ; then		# make it
#	ALT_KEYBOARD="${usr_root}/.hermes_sockets.${SESSION_ID}"
#	export ALT_KEYBOARD
#fi
#if [ "${DEFAULT_DISPLAY}" = "" ] ; then		# make it
#	DEFAULT_DISPLAY="${usr_root}/.gids_sockets.${SESSION_ID}"
#	export DEFAULT_DISPLAY
#fi
#if [ "${THERTEK}" = "" ] ; then			# make it
#	THERTEK="${usr_root}/.thertek.${SESSION_ID}"
#	export THERTEK
#fi
#if [ "${EXPORT}" = "" ] ; then			# make it
#	EXPORT=`pwd`/export			# disk tape for export
#	export EXPORT
#fi
#if [ "${IMPORT}" = "" ] ; then			# make it
#	IMPORT=`pwd`/import			# disk tape for import
#	export IMPORT
#fi
#if [ "${PGPLOT_FONT}" = "" ] ; then		# make it
#	PGPLOT_FONT="${gip_lib}/grfont.dat"	# the PGPLOT fonts
#	export PGPLOT_FONT
#fi
#if [ "${TEK_DEVICE}" = "" ] ; then		# make it
#	TEK_DEVICE=/dev/tty
#	export TEK_DEVICE
#fi
##case ${gip_arch} in				# machine dependent settings
##alpha)
##	limit stacksize unlimited
##	;;
##convex)
##	limit stacksize unlimited
##	;;
##mips)
##	limit stacksize unlimited
##	;;
##esac
#if [ -x "$gip_sys/register.sh" ] ; then
#	${gip_sys}/register.sh client
#fi
#if [ -r "$gip_exe/thermes" ] ; then		# we start terminal HERMES
#	${gip_exe}/thermes
#	if [ -r ${ALT_KEYBOARD} ] ; then	# it still exists
#		rm -f ${ALT_KEYBOARD}		# not anymore (I hope)
#	fi
#else
#	echo "No HERMES found"
#fi
##<
