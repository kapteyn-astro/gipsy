#!/bin/sh
# register.sh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1994
#       All Rights Reserved.
#
##>            register.doc
#
#Script:       register
#
#Purpose:      Registers a GIPSY installation or a new GIPSY user.
#
#Category:     MANAGEMENT
#
#File:         register.sh
#
#Author:       K.G. Begeman
#
#Use:          Only from some special GIPSY scripts.
#
#Description:  The first time a user starts a GIPSY session or each time
#              the GIPSY install script is run, this script sends a mail
#              to the GIPSY server.
#
#Updates:      Apr  7, 1994: KGB, Document created
#
##<
#
if [ "${gip_root}" = "" ] ; then
	current_path=`\pwd`
	cd `echo $0 | sed s+register.sh+.+`
	cd ..
	gip_root=`\pwd`
	cd ${current_path}
	export gip_root
	if [ -r "${gip_root}/sys/gipenv.sh" ] ; then
		. ${gip_root}/sys/gipenv.sh
	else
		exit
	fi
fi
if [ ! -r "${HOME}/.register" -o "$1" = "install" ] ; then
	if [ -r "${gip_loc}/server" ] ; then
		server_file=${gip_loc}/server
	elif [ -r "${gip_sys}/server.mgr" ] ; then
		server_file=${gip_sys}/server.mgr
	fi
	if [ "${server_file}" = "" ] ; then
		server_address='gipsy@astro.rug.nl'
	else
		server_address=`awk -F: '{if (substr($0,1,1)!="#"&&NF==3)print $2}' ${server_file}`
	fi
	\cat << @EOF | \mail ${server_address} > /dev/null 2>&1
Gipsy_Registration_Begin
`hostname`:`whoami`:${gip_arch}:${1}
Gipsy_Registration_End
@EOF
	if [ "$1" != "install" ] ; then
		touch ${HOME}/.register
	fi
fi
