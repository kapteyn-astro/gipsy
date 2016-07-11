#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.63])
AC_INIT([gipsy], [1.0], [gipsy@astro.rug.nl])
AC_CONFIG_SRCDIR([inc/aborttask.h])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE
AC_CONFIG_FILES([Makefile sub/Makefile tsk/Makefile
	tsk/hermes/Makefile
	tsk/gdsserver/Makefile
	tsk/gdsserver64/Makefile
	])

# Checks for programs.
AC_PROG_AWK
AC_PROG_CC
AC_PROG_F77
#AC_PROG_INSTALL

# older versions of automake need this
AM_PROG_CC_C_O

# Checks for libraries.

# Checks for header files.
AC_PATH_X
AC_HEADER_MAJOR
AC_CHECK_HEADERS([fcntl.h float.h limits.h netdb.h netinet/in.h stddef.h stdint.h stdlib.h string.h sys/file.h sys/ioctl.h sys/socket.h sys/time.h sys/timeb.h unistd.h])

AC_PROG_RANLIB

# Checks for typedefs, structures, and compiler characteristics.
#AC_CHECK_HEADER_STDBOOL
#AC_TYPE_INT64_T
#AC_TYPE_OFF_T
#AC_TYPE_PID_T
#AC_TYPE_SIZE_T
#AC_CHECK_MEMBERS([struct stat.st_blksize])
#AC_CHECK_MEMBERS([struct stat.st_rdev])
#AC_STRUCT_TIMEZONE
#AC_TYPE_UID_T
#AC_CHECK_TYPES([ptrdiff_t])

# Checks for library functions.
#AC_FUNC_FORK
#AC_FUNC_LSTAT_FOLLOWS_SLASHED_SYMLINK
#AC_FUNC_MALLOC
#AC_FUNC_MKTIME
#AC_FUNC_REALLOC
#AC_FUNC_STRTOD
#AC_FUNC_WAIT3
#AC_CHECK_FUNCS([atexit floor ftime ftruncate getcwd gethostbyname gethostname gettimeofday inet_ntoa memchr memmove memset mkdir modf pow select socket sqrt strchr strcspn strdup strerror strpbrk strrchr strspn strstr strtol strtoul uname])
#AM_INIT_AUTOMAKE

AM_CONDITIONAL([SYSTEM], [test $(uname) -eq "Darwin"])
AC_CANONICAL_HOST

case $host_os in
  darwin* )
        SYSTEM="apple_i64"
        ;;
  linux*)
        SYSTEM="linux64"
        ;;
    *)
        #Default Case
        AC_MSG_ERROR([Your platform is not currently supported])
        ;;
esac

AC_SUBST(SYSTEM)
AC_OUTPUT



