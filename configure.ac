#
# configure.ac
#
# Copyright (C) 2024 Victor Santos <victor_santos@fisica.ufc.br>
#
# This file is part of guile-hello.
#
# guile-hello is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# guile-hello is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with guile-hello. If not, see https://www.gnu.org/licenses/.
#

AC_INIT([guile-hello], [1.0.0], [victor_santos@fisica.ufc.br])
AC_CONFIG_MACRO_DIRS([m4])
AC_CONFIG_SRCDIR(hello.scm)
AC_CONFIG_AUX_DIR([build-aux])
AM_INIT_AUTOMAKE([color-tests -Wall -Wno-portability foreign])
AM_SILENT_RULES([yes])

dnl We require pkg.m4 (from pkg-config) and guile.m4.
dnl Make sure they are available.
m4_pattern_forbid([PKG_CHECK_MODULES])
m4_pattern_forbid([^GUILE_PKG])

dnl Check for Guile 3.x.
GUILE_PKG([3.0])
GUILE_PROGS
GUILE_SITE_DIR

dnl Guile prefix and libdir.
GUILE_PREFIX=`$PKG_CONFIG --print-errors --variable=prefix guile-$GUILE_EFFECTIVE_VERSION`
GUILE_LIBDIR=`$PKG_CONFIG --print-errors --variable=libdir guile-$GUILE_EFFECTIVE_VERSION`
AC_SUBST(GUILE_PREFIX)
AC_SUBST(GUILE_LIBDIR)

if test "$cross_compiling" != no; then
   GUILE_TARGET="--target=$host_alias"
   AC_SUBST([GUILE_TARGET])
fi

AC_CONFIG_FILES([Makefile hello/Makefile tests/Makefile])
AC_CONFIG_FILES([env], [chmod +x env])
AC_CONFIG_FILES([hello/info.scm])

AC_OUTPUT

dnl This is just for printing $libdir below.
LIBDIR=`eval echo $libdir`
LIBDIR=`eval echo $LIBDIR`
AC_SUBST([LIBDIR])

echo
echo "*** $PACKAGE $VERSION has been successfully configured ***"
echo
echo "$PACKAGE is using:"
echo
echo "    --prefix=$prefix --libdir=$LIBDIR"
echo
echo "If you want to install in Guile system's directory re-run with:"
echo
echo "    --prefix=$GUILE_PREFIX --libdir=$GUILE_LIBDIR"
echo

# configure.ac ends here
