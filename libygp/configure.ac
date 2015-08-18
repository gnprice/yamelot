# Run `./bootstrap` to generate the "configure" script.

# Define the package version numbers and the bug reporting link.
m4_define([YGP_MAJOR], 0)
m4_define([YGP_MINOR], 1)
m4_define([YGP_PATCH], 0)
m4_define([YGP_BUGS], [https://github.com/gnprice/yamelot/issues])

# Define the libtool version numbers; check the Autobook, Section 11.4.
# Bump the libtool version numbers using the following algorithm:
#   if (the current interface has not been changed):
#       YGP_REVISION += 1
#   else:
#       YGP_REVISION = 0
#       YGP_CURRENT += 1
#       if (this release is backward compatible with the previous release):
#           YGP_AGE += 1
#       else:
#           YGP_AGE = 0
m4_define([YGP_RELEASE], 0)
m4_define([YGP_CURRENT], 0)
m4_define([YGP_REVISION], 1)
m4_define([YGP_AGE], 0)

# Initialize autoconf & automake.
AC_PREREQ(2.59)
AC_INIT([ygp], [YGP_MAJOR.YGP_MINOR.YGP_PATCH], [YGP_BUGS])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_HEADERS([config.h])
AM_INIT_AUTOMAKE([1.9 foreign])

# Define macro variables for the package version numbers.
AC_DEFINE(YGP_VERSION_MAJOR, YGP_MAJOR, [Define the major version number.])
AC_DEFINE(YGP_VERSION_MINOR, YGP_MINOR, [Define the minor version number.])
AC_DEFINE(YGP_VERSION_PATCH, YGP_PATCH, [Define the patch version number.])
AC_DEFINE(YGP_VERSION_STRING, "YGP_MAJOR.YGP_MINOR.YGP_PATCH", [Define the version string.])

# Define substitutions for the libtool version numbers.
YGP_LT_RELEASE=YGP_RELEASE
YGP_LT_CURRENT=YGP_CURRENT
YGP_LT_REVISION=YGP_REVISION
YGP_LT_AGE=YGP_AGE
AC_SUBST(YGP_LT_RELEASE)
AC_SUBST(YGP_LT_CURRENT)
AC_SUBST(YGP_LT_REVISION)
AC_SUBST(YGP_LT_AGE)

# Note: in order to update checks, run `autoscan` and look through "configure.scan".

# Checks for programs.
AC_PROG_CC
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_LIBTOOL

AC_CHECK_PROG(DOXYGEN, [doxygen], [true], [false])
AM_CONDITIONAL(DOXYGEN, [test "$DOXYGEN" = true])

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h])

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T

# Define Makefiles.
AC_CONFIG_FILES([ygp-0.1.pc include/Makefile src/Makefile Makefile tests/Makefile win32/Makefile])

# Generate the "configure" script.
AC_OUTPUT
