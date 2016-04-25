# cshrc.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992
#       All Rights Reserved.
#
##>            cshrc.doc
#
#Script:       cshrc
#
#Purpose:      Sets the environment for the GIPSY directory. The user gipsy
#              must run the csh or tcsh shell.
#
#Category:     MANAGEMENT
#
#File:         cshrc.csh
#
#Author:       K.G. Begeman
#
#Use:          Define gip_root in your .cshrc and add the command:
#              source ${gip_root}/sys/cshrc.csh
#
#Updates:      Aug 17, 1991: KGB, Document created.
#              May  1, 2007: JPT, Added missing {} braces to gipenv call.
#
##<
#
# Setup the GIPSY environment. If gip_root is not defined, it is assumed
# that the root directory is the home directory. If not, edit your .cshrc
#
if ( ${?gip_root} == 0 ) then		# not defined, try default
	setenv gip_root ${HOME}		# the gipsy root directory
endif
#
# Initialize the GIPSY environment
#
source ${gip_root}/sys/gipenv.csh		# setup the gipsy environment
#
# Now we put some machine dependent instructions to get it going
#
switch( ${gip_arch} )
case alpha:				# This is a alpha and does not ..
	limit stacksize unlimited	# .. want to run a GIPSY application ..
	breaksw				# .. unless the stack is unlimited.
case convex:				# This is a convex and does not ..
	limit stacksize unlimited	# .. want to run a GIPSY application ..
	breaksw				# .. unless the stack is unlimited.
case mips:				# This is a dec station and does not ..
	limit stacksize unlimited	# .. want to run a GIPSY application ..
	breaksw				# .. unless the stack is unlimited.
default:
	breaksw
endsw
