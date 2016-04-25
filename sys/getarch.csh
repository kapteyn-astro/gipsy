#!/bin/csh -f
# getarch.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1993
#       All Rights Reserved.
#
##>            getarch.doc
#
#Script:       getarch
#
#Purpose:      Determines architecture of host.
#
#Category:     MANAGEMENT
#
#File:         getarch.csh
#
#Author:       K.G. Begeman
#
#Use:          getarch.csh
#
#Description:  getarch.csh tries to determine the architecture of
#              the host and displays the result.
#
#Updates:      Jan  8, 1993: KGB Document created.
#              Apr 30, 1994: KGB New version for LINUX.
#              Apr 18, 1996: KGB New version for FreeBSD.
#              May  1, 2007: JPT Added Apple Mac.
#              Aug 27, 2007: JPT Fixed Mac Power PC bug.
#              May 29, 2009: JPT Added 64-bit Linux.
#
##<
#
# Now determine the architecture
#
setenv archi unknown				# default
if ( -e /bin/uname ) then
	setenv uname	'/bin/uname'
else if ( -e /usr/bin/uname ) then
	setenv uname	'/usr/bin/uname'
endif
if ( ${?uname} ) then				# on SYSV systems this works
	switch ( `${uname}` )			# switch
	case AIX:				# IBM/AIX
		setenv archi aix		# set to 'aix'
		breaksw				# break
	case ConvexOS:				# CONVEX
		setenv archi convex		# set to 'convex'
		breaksw				# break
        case Darwin:                            # Apple Mac
                switch( "`${uname} -m`" )       # machine type
                case i386:                      # Intel
                        setenv archi apple_i
                        breaksw
                case x86_64:
                        setenv archi apple_i64  # Intel 64-bit
                        breaksw
                default:                        # assume Motorola
                        setenv archi apple_m
                        breaksw
                endsw
                breaksw
	case FreeBSD:				# FREEBSD
		setenv archi freebsd		# set to 'freebsd'
		breaksw
	case IRIX:				# SGI/IRIX
		setenv archi sgi		# set to 'sgi'
		breaksw				# break
	case Linux:				# LINUX
                switch( "`${uname} -m`" )       # machine type
                case x86_64:                    # 64 bit linux
                        setenv archi linux64
                        breaksw
                default:                        # assume i386
                        setenv archi linux
                        breaksw
                endsw
                breaksw
	case OSF1:				# OSF
		setenv archi `${uname} -m`	# machine type
		breaksw				# break
	case SunOS:				# SUN
		switch( `${uname} -m` )		# machine type
		case i86pc:			# PC running solaris
			setenv archi sun386i	# set to 'sun386i'
			breaksw
		case sun3*:			# old sun
			setenv archi sun3	# set to 'sun3'
			breaksw			# break
		case sun4*:			# sparcs etc.
			setenv archi sun4	# set to 'sun4'
			breaksw			# break
		default:			# don't know
			if ( -e /bin/mach ) then
				switch( `/bin/mach` )
				case sparc:
					setenv archi sun4
					breaksw
				default:
					setenv archi `/bin/mach`
					breaksw
				endsw
			else			#
				setenv archi `${uname}`
			endif			# }
			breaksw			# break
		endsw				# end
		switch( `${uname} -r` )
		case 4.*:
			breaksw;
		case 5.*:
			switch( ${archi} )
			case sun3:
				setenv archi 'sol3'
				breaksw
			case sun4:
				setenv archi 'sol4'
				breaksw
			default:
				breaksw
			endsw
			breaksw
		default:
			breaksw
		endsw
		breaksw				# break
	case HP-UX:				# HP
		switch ( `${uname} -m` )	# machine type
		case 9000/3*:			# 9000 300 series
			setenv archi hp9000s300	# set to 'hp9000s300'
			breaksw			# break
		case 9000/4*:			# 9000 400 series
			setenv archi hp9000s300	# set to 'hp9000s300'
			breaksw			# break
		case 9000/7*:			# 9000 700 series
			setenv archi hp9000s700	# set to 'hp9000s300'
			breaksw			# break
		default:			# don't know
			breaksw			# break
		endsw				# end
		breaksw				# break
	case ULTRIX:				# DECSTATION
		setenv archi mips		# set to 'mips'
		breaksw				# break
	default:				# don't know
		breaksw				# break
	endsw					# end
endif						# }
if ( ${archi} == "unknown" && -d /usr/convex ) then	# try this
	setenv archi convex			# set to 'convex'
endif						# }
if ( ${archi} == "unknown" && -d /usr/alliant ) then	# try this
	setenv archi alliant			# set to 'alliant'
endif						# }
echo "${archi}"					# echo
unsetenv archi					# remove it
