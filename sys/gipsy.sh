# gipsy.sh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1993
#       All Rights Reserved.
#
if [ "${gip_mode}" = "" ] ; then		# no GIPSY environment
	echo 
	echo 'No GIPSY environment defined'
	echo ''
	echo 'Edit your .profile file to contain the following lines:'
	echo 'gip_root= <path to gipsy root> (ask GIPSY manager)'
	echo 'export gip_root'
	echo '. $gip_root/sys/gipenv.sh'
	echo ''
	exit					# quit
fi
#
# Check for session id.
#
if [ "$1" = "" ] ; then
	session_id=1
else
	session_id=$1
	if [ "${gids_windowname}" = "" ] ; then
		gids_windowname="GIDS_${session_id}"
		export gids_windowname
	fi
fi
SESSION_ID=${session_id}
export SESSION_ID				# set session id
#
# Execute $HOME/.gipsyrs if present
#
if [ -r "${HOME}/.gipsyrs" ] ; then		# user setup wanted
	. ${HOME}/.gipsyrs			# execute .gipsyrs
fi						# go on
#
# Set the rest of the environment
#
if [ "${usr_root}" = "" ] ; then		# no usr_root
	usr_root=${HOME}
	export usr_root
fi
if [ "${usr_exe}" = "" ] ; then			# no usr_exe
	usr_exe=${usr_root}/exe/${gip_arch}
	export usr_exe
fi
if [ "${ALT_KEYBOARD}" = "" ] ; then		# make it
	ALT_KEYBOARD="${usr_root}/.hermes_sockets.${SESSION_ID}"
	export ALT_KEYBOARD
fi
if [ "${DEFAULT_DISPLAY}" = "" ] ; then		# make it
	DEFAULT_DISPLAY="${usr_root}/.gids_sockets.${SESSION_ID}"
	export DEFAULT_DISPLAY
fi
if [ "${THERTEK}" = "" ] ; then			# make it
	THERTEK="${usr_root}/.thertek.${SESSION_ID}"
	export THERTEK
fi
if [ "${EXPORT}" = "" ] ; then			# make it
	EXPORT=`pwd`/export			# disk tape for export
	export EXPORT
fi
if [ "${IMPORT}" = "" ] ; then			# make it
	IMPORT=`pwd`/import			# disk tape for import
	export IMPORT
fi
if [ "${PGPLOT_FONT}" = "" ] ; then		# make it
	PGPLOT_FONT="${gip_lib}/grfont.dat"	# the PGPLOT fonts
	export PGPLOT_FONT
fi
if [ "${TEK_DEVICE}" = "" ] ; then		# make it
	TEK_DEVICE=/dev/tty
	export TEK_DEVICE
fi
#case ${gip_arch} in				# machine dependent settings
#alpha)
#	limit stacksize unlimited
#	;;
#convex)
#	limit stacksize unlimited
#	;;
#mips)
#	limit stacksize unlimited
#	;;
#esac
if [ -x "$gip_sys/register.sh" ] ; then
	${gip_sys}/register.sh client
fi
if [ -r "$gip_exe/thermes" ] ; then		# we start terminal HERMES
	${gip_exe}/thermes
	if [ -r ${ALT_KEYBOARD} ] ; then	# it still exists
		rm -f ${ALT_KEYBOARD}		# not anymore (I hope)
	fi
else
	echo "No HERMES found"
fi
