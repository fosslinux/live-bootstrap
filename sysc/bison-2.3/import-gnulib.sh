#!/bin/sh

# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

set -e

gnulib_modules='
argmatch
dirname
error
exitfail
extensions
getopt
gettext
hard-locale
hash
malloc
mbswidth
obstack
quote
quotearg
stdbool
stdio-safer
strerror
strtoul
strverscmp
unistd-safer
unlocked-io
verify
xalloc
xalloc-die
xstrndup
'

# Copy over needed files
for i in ${gnulib_modules}; do
    for f in $(../gnulib-b28236b/gnulib-tool --extract-filelist "${i}") \
        lib/wcwidth.h; do
        cp -pf "../gnulib-b28236b/${f}" "./${f}"
    done
done

# Generate the things
(echo '# This file is generated automatically by "bootstrap".' &&
 echo 'AC_DEFUN([GNULIB_AUTOCONF_SNIPPET],[' &&
 ../gnulib-b28236b/gnulib-tool --extract-autoconf-snippet $gnulib_modules &&
 echo '])'
) > m4/gnulib.m4

(echo '# This file is generated automatically by "bootstrap".' &&
 ../gnulib-b28236b/gnulib-tool --extract-automake-snippet $gnulib_modules |
 sed 's/^[	 ]*AM_CPPFLAGS[	 ]*+=/# (commented out by bootstrap) &/'
) > lib/gnulib.mk
