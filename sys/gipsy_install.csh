#!/bin/csh

#-----------------------------------------------------------------------------
#  Check for absolute requirements.
#-----------------------------------------------------------------------------
set fail = 0

echo ""
which wget >& /dev/null
if (${status} != 0) then
   echo "Non-interactive network downloader 'wget' not found!"
   set fail = 1
endif

which gfortran >& /dev/null
if (${status} != 0) then
   echo "GNU Fortran 95 compiler 'gfortran' not found!"
   set fail = 1
endif

if (${fail} != 0) then
   echo ""
   echo "--- Cannot install GIPSY ---"
   exit(1)
endif

set pyfail = 0

#-----------------------------------------------------------------------------
#  Check for Python requirements.
#-----------------------------------------------------------------------------
python -c "" >& /dev/null
if (${status} != 0) then
   echo ""
   echo "No Python interpreter found."
   echo "GIPSY tasks written in Python will not be available."
   set pyfail = 1
endif

if (${pyfail} == 0) then
   python -c "import numpy" >& /dev/null
   if (${status} != 0) then
      echo ""
      echo "Python package 'numpy' is missing."
      echo "GIPSY tasks written in Python will not be available."
      set pyfail = 1
   endif
endif

if (${pyfail} == 0) then

   python -c "import matplotlib" >& /dev/null
   if (${status} != 0) then
      echo ""
      echo "Python package 'matplotlib' is missing."
      set pyfail = 1
   endif

   python -c "import vtk" >& /dev/null
   if (${status} != 0) then
      echo ""
      echo "Python package 'vtk' is missing."
      echo "This currently only affects GIPSY task VTKVOLUME."
      set pyfail = 1
   endif

   python -c "import PyQt4" >& /dev/null
   if (${status} != 0) then
      echo ""
      echo "Python package 'PyQt4' is missing."
      set pyfail = 1
   endif

   python -c "import pyfits" >& /dev/null
   if (${status} != 0) then
      echo ""
      echo "Python package 'pyfits' is missing."
      set pyfail = 1
   endif
endif

if (${pyfail} != 0) then
   echo ""
   echo "Not all Python-related requirements are met."
   echo "You may proceed, but some or all Python tasks will be unavailable."
   echo "In principle, this can be corrected later."
   echo ""
endif

#-----------------------------------------------------------------------------
#  Ask user for GIPSY's location.
#-----------------------------------------------------------------------------
echo -n "Enter directory where to install GIPSY: "
set install_root = "$<"

mkdir -p ${install_root} >& /dev/null
cd ${install_root}
set install_root = `pwd`
if (${status} != 0) then
   echo ""
   echo ""
   echo "Cannot install in ${install_root}!"
   exit(1)
endif

touch test.dum >& /dev/null
if (${status} != 0) then
   echo ""
   echo ""
   echo "Cannot install GIPSY in ${install_root}!"
   exit(1)
endif
\rm test.dum

echo -n "OK to install GIPSY in ${install_root} ? [Y/N] "
set proceed = "$<"
if ((${proceed} != 'Y') && (${proceed} != 'y')) then
   echo ""
   echo ""
   echo "Installation cancelled."
   exit(1)
endif

#-----------------------------------------------------------------------------
#  Start installation procedure.
#-----------------------------------------------------------------------------
set logfile = ${install_root}/install.log
echo ""
echo "Log file will be written to ${logfile}."
echo "In case of problems, please check this file first and"
echo "attach it to your mail when you have to ask for help."
echo ""

setenv gip_root `\pwd`
cd ${gip_root}
mkdir import >& /dev/null
cd import
mkdir src >& /dev/null
cd src
echo "Fetching GIPSY distribution ..."
wget "ftp://ftp.astro.rug.nl/gipsy/src/gipsy_src.tar.gz" -o ${logfile}
if (${status} != 0) then
   echo ""
   echo ""
   echo "Failed to fetch GIPSY distribution!"
   exit(1)
endif

cd ../..
tar xfz import/src/gipsy_src.tar.gz >>& ${logfile}
cd ${gip_root}/sys
./mkclient.csh 103 >>& ${logfile}
\mv clients.new $gip_root/loc/clients
source cshrc.csh
cd ${gip_loc}
\cp ${gip_sys}/setup.mgr setup
cd ${gip_sys}
./install.csh >>& ${logfile}

if (${status} != 0) then
   echo ""
   echo ""
   echo "Compiler setup failed!"
   exit(1)
endif

cd ${gip_sys}
./mkbookkeeper.csh >>& ${logfile}
\mv bookkeeper.new bookkeeper

echo "Start building GIPSY - wait ..."
p -update >> & ${logfile}
p -rebuild ggivtk.src >> & ${logfile}
./mkclient.csh 231 >>& ${logfile}
\mv clients.new $gip_root/loc/clients
echo "+++ FINISHED +++"
