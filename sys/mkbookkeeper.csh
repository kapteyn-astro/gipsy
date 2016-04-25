#!/bin/csh -f
# mkbookkeeper.csh
#
#       Copyright (c) Kapteyn Laboratorium Groningen 1992
#       All Rights Reserved.
#
#
##>            mkbookkeeper.doc
#
#Script:       mkbookkeeper
#
#Purpose:      Creates bookkeeper file from scratch.
#
#Category:     MANAGEMENT
#
#File:         mkbookkeeper.csh
#
#Author:       K.G. Begeman
#
#Use:          mkbookkeeper creates file bookkeeper.new with all
#              sources out of date.
#
#Updates:      Sep 18, 1991: KGB, document created.
#
##<
#
# List all sources in sub directory:
#
\ls ${gip_sub}/*.c ${gip_sub}/*.f ${gip_sub}/*.shl ${gip_sub}/*.src > bookkeeper.sub
\awk -F/ '{printf("%s:sub:0:0:0\n",$NF)}' bookkeeper.sub > bookkeeper.tmp0
#
# List all sources in tsk directory:
#
\ls ${gip_tsk}/*.c ${gip_tsk}/*.f ${gip_tsk}/*.shl ${gip_tsk}/*.src > bookkeeper.tsk
\awk -F/ '{printf("%s:tsk:0:0:0\n",$NF)}' bookkeeper.tsk >> bookkeeper.tmp0
#
# Create the header
#
set cdate = `date`
\rm -f bookkeeper.new
\cat << @EOF > bookkeeper.new
# bookkeeper
#
#       Copyright (c) Kapteyn Laboratorium Groningen ${cdate[${#cdate}]}
#       All Rights Reserved.
#
@EOF
\awk '{printf( "#%s\n", $0 )}' ${gip_doc}/bookkeeper.doc >> bookkeeper.new
#
# Now put in the known architectures
#
echo 'aix:alliant:alpha:apple_i:apple_i64:apple_m:convex:cray:hp9000s300:hp9000s700:linux:linux64:mips:sgi:sol4:sun4:sun386i' >> bookkeeper.new
#
# Sort the file
#
\sort bookkeeper.tmp0 >> bookkeeper.new
#
# Remove intermediate files:
#
\rm -f bookkeeper.tmp bookkeeper.tmp0 bookkeeper.sub bookkeeper.tsk
#
echo "bookkeeper.new created"
