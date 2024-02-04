#!/bin/sh
#
# SPDX-FileCopyrightText: 2024 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

../gnulib-5651802/gnulib-tool --import --local-dir=gl \
 --lib=libgzip \
 --source-base=lib \
 --m4-base=m4 \
 --doc-base=doc \
 --tests-base=tests \
 --aux-dir=build-aux \
 --makefile-name=gnulib.mk \
 --no-conditional-dependencies \
 --no-libtool \
 --macro-prefix=gl \
 --avoid=getline \
 --avoid=rpmatch \
 announce-gen \
 calloc-gnu \
 close \
 dirname-lgpl \
 fclose \
 fcntl \
 fcntl-safer \
 fdatasync \
 fdopendir \
 filename \
 fprintf-posix \
 fsync \
 getopt-gnu \
 git-version-gen \
 gitlog-to-changelog \
 gnu-make \
 gnu-web-doc-update \
 gnumakefile \
 gnupload \
 ignore-value \
 intprops \
 largefile \
 lib-ignore \
 lstat \
 maintainer-makefile \
 malloc-gnu \
 manywarnings \
 openat-safer \
 printf-posix \
 readme-release \
 realloc-gnu \
 savedir \
 sigaction \
 stat-time \
 strerror \
 sys_stat \
 time \
 unistd-safer \
 unlinkat \
 update-copyright \
 utimens \
 verify \
 xalloc \
 year2038 \
 yesno
