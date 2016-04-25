#!/bin/csh
# export.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992
#       All Rights Reserved.
#
##>            export.doc
#
#Script:       export
#
#Purpose:      Creates an compressed exportable tar file of the GIPSY
#              sources and/or GIPSY binaries for all architectures for
#              export.
#
#Category:     MANAGEMENT
#
#File:         export.csh
#
#Author:       K.G. Begeman
#
#Use:          export.csh [-b] [-s] [-t tapedevice] [archs]
#
#              -b                export binaries.
#              -s                export sources.
#              -t tapedevice     export to tapedevice. Default export
#                                to GIPSY export directory.
#              archs             architectures to export. Default all
#                                available architectures will be
#                                exported
#
#Description:  The sources are stored in $gip_root/export/src/gipsy-src.???
#              in files of 0.5 Megabyte each. The binaries are stored in
#              $gip_root/export/"arch"/gipsy-bin.???, also in files
#              of 0.5 Megabyte each. The $gip_root/export directory should
#              be a link to the gipsy directory of the anonymous ftp
#              directory. The gipsy account must have write priviledge to
#              the gipsy directory of the anonymous ftp account.
#
#Updates:      Oct 29, 1991: Document created.
#              Aug 10, 1999: JPT, also create gzip'ed tar file for sources.
#              May  4, 2007: JPT, compress utility replaced by gzip.
#
##<
#
@ narg = ${#argv}
if ( ${narg} == 0 ) then
	echo ''
	echo 'Use: [-b] [-s] [-t tapedevice] [archs]'
	echo ''
else
	@ bin  = 0
	@ src  = 0
        @ iarg = 0
	@ narch = 0
	@ tap = 0
	set archs = ( )
	while ( ${iarg} < ${narg} )
		@ iarg++
		switch( ${argv[${iarg}]} )
		case -b:
			@ bin = 1
			breaksw
		case -s:
			@ src = 1
			breaksw
		case -t:
			@ tap = 1
			if ( ${iarg} < ${narg} ) then
				@ iarg++
				set tar = ( tar cvf ${argv[${iarg}]} )
			else
				echo "No tape device specified"
				exit 0
			endif
			breaksw
		default:
			set archs = ( ${archs} ${argv[${iarg}]} )
			@ narch++
			breaksw
		endsw
	end
	if ( ${src} ) then
		if ( ${tap} ) then
			if ( -d ${gip_root}/export/src == 0 ) then
				echo "${gip_root}/export/src not present"
				exit
			endif
			set tar = ( $tar src )
		else
			echo "cp ${gip_sys}/README ${gip_root}/export"
			cp ${gip_sys}/README ${gip_root}/export
			echo "cp ${gip_sys}/COPYRIGHT ${gip_root}/export"
			cp ${gip_sys}/COPYRIGHT ${gip_root}/export
			echo "cp ${gip_sys}/sysgen.csh ${gip_root}/export"
			cp ${gip_sys}/sysgen.csh ${gip_root}/export
			echo "( cd ${gip_root} ; tar chf - dat doc mis inc sub sys tsk ) | gzip > ${gip_root}/export/src/gipsy_src.tar.gz"
			( cd ${gip_root} ; tar chf - dat doc mis inc sub sys tsk ) | gzip > ${gip_root}/export/src/gipsy_src.tar.gz
			echo "( cd ${gip_root} ; tar chf - dat doc mis inc sub sys tsk ) | gzip > ${gip_root}/gipsy-src"
			( cd ${gip_root} ; tar chf - dat doc mis inc sub sys tsk ) | gzip > ${gip_root}/gipsy-src
			if ( -d ${gip_root}/export/src == 0 ) then
				echo "( cd ${gip_root}/export ; mkdir src )"
				( cd ${gip_root}/export ; mkdir src )
			endif
			if ( -d ${gip_root}/export/src == 0 ) then
				echo "Cannot create ${gip_root}/export/src"
				exit
			endif
			if ( -e ${gip_root}/export/src/gipsy-src.001 ) then
				echo "Clearing ${gip_root}/export/src"
				rm -f ${gip_root}/export/src/gipsy-src.*
			endif
			set lim = ( `ls -ol ${gip_root}/gipsy-src | awk '{t=$4}END{d=0;n=0;while(d<t){n++;s=d+524288;printf("%3.3d %d\n",n,n-1);d=s;}}'` )
			@ nlim = ${#lim}
			@ ilim = 0
			while ( ${ilim} < ${nlim} )
				@ ilim++
				@ n1 = ${ilim}
				@ ilim++
				@ n2 = ${ilim}
				echo "dd if=${gip_root}/gipsy-src of=${gip_root}/export/src/gipsy-src.${lim[${n1}]} bs=524288 skip=${lim[${n2}]} count=1"
				dd if=${gip_root}/gipsy-src of=${gip_root}/export/src/gipsy-src.${lim[${n1}]} bs=524288 skip=${lim[${n2}]} count=1
			end
			echo "rm ${gip_root}/gipsy-src"
			rm ${gip_root}/gipsy-src
		endif
	endif
	if ( ${bin} ) then
		foreach dir ( ${gip_root}/exe/* )
			set arch =  ${dir:t}
			if ( ${narch} ) then
				@ skip = 1
				@ iarch = 0
				while ( ${iarch} < ${narch} )
					@ iarch++
					if ( ${archs[${iarch}]} == ${arch} ) then
						@ skip = 0
					endif
				end
			else
				@ skip = 0
			endif
			if ( ${tap} ) then
				if ( ${skip} ) then
					echo "skipping ${arch}"
				else if ( -d ${gip_root}/export/${arch} == 0 ) then
					echo "${gip_root}/export/${arch} not present"
				else
					set tar = ( ${tar} ${arch} )
				endif
			else
				if ( ${skip} ) then
					echo "skipping ${arch}"
				else if ( -d ${gip_root}/exe/${arch} ) then
					if ( -d ${gip_root}/export/${arch} == 0 ) then
						echo "( cd ${gip_root}/export ; mkdir ${arch} )"
						( cd ${gip_root}/export ; mkdir ${arch} )
					endif
					if ( -d ${gip_root}/export/${arch} == 0 ) then
						echo "Cannot create ${gip_root}/export/${arch}"
						exit
					endif
					if ( -e ${gip_root}/export/${arch}/gipsy-bin.001 ) then
						echo "Clearing ${gip_root}/export/${arch}"
						rm -f ${gip_root}/export/${arch}/gipsy-bin.*
					endif
                                        if ( -x ${gip_root}/exe/${arch}/compile ) then
						echo "( cd ${gip_root} ; tar chf - exe/${arch} lib/${arch} tmp/${arch} ) | gzip > ${gip_root}/gipsy-bin"
						( cd ${gip_root} ; tar chf - exe/${arch} lib/${arch} tmp/${arch} ) | gzip > ${gip_root}/gipsy-bin
						set lim = ( `ls -ol ${gip_root}/gipsy-bin | awk '{t=$4}END{d=0;n=0;while(d<t){n++;s=d+524288;printf("%3.3d %d\n",n,n-1);d=s;}}'` )
						@ nlim = ${#lim}
						@ ilim = 0
						while ( ${ilim} < ${nlim} )
							@ ilim++
							@ n1 = ${ilim}
							@ ilim++
							@ n2 = ${ilim}
							echo "dd if=${gip_root}/gipsy-bin of=${gip_root}/export/${arch}/gipsy-bin.${lim[${n1}]} bs=524288 skip=${lim[${n2}]} count=1"
							dd if=${gip_root}/gipsy-bin of=${gip_root}/export/${arch}/gipsy-bin.${lim[${n1}]} bs=524288 skip=${lim[${n2}]} count=1
						end
						echo "rm ${gip_root}/gipsy-bin"
						rm ${gip_root}/gipsy-bin
					else
						echo "Skipping ${arch}"
					endif
					if ( -x ${gip_root}/exe/${arch}/gftp ) then
						echo "cp ${gip_root}/exe/${arch}/gftp ${gip_root}/export/${arch}/gftp"
						cp ${gip_root}/exe/${arch}/gftp ${gip_root}/export/${arch}/gftp
					endif
				endif
			endif
		end
	endif
	if ( ${tap} ) then
		echo "( cd ${gip_root}/export; ${tar} )"
		( cd ${gip_root}/export; ${tar} )
	endif
endif
