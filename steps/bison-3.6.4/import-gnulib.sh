#!/bin/bash

# SPDX-FileCopyrightText: 2021 Samuel Tyler <samuel@samuelt.me>
#
# SPDX-License-Identifier: GPL-3.0-or-later

../gnulib-ffbb0ce/gnulib-tool --import --local-dir=gl \
 --lib=libbison \
 --source-base=lib \
 --m4-base=m4 \
 --po-base=gnulib-po \
 --doc-base=doc \
 --tests-base=tests \
 --aux-dir=build-aux \
 --makefile-name=gnulib.mk \
 --conditional-dependencies \
 --no-libtool \
 --macro-prefix=gl \
 --po-domain=bison \
 argmatch \
 array-list \
 assert \
 assure \
 bitsetv \
 c-strcase \
 calloc-posix \
 close \
 closeout \
 config-h \
 configmake \
 dirname \
 error \
 extensions \
 fdl \
 fopen-safer \
 fprintf-posix \
 fstrcmp \
 getopt-gnu \
 gettext-h \
 git-version-gen \
 gitlog-to-changelog \
 gpl-3.0 \
 intprops \
 inttypes \
 isnan \
 javacomp-script \
 javaexec-script \
 ldexpl \
 libtextstyle-optional \
 malloc-gnu \
 mbfile \
 mbswidth \
 non-recursive-gnulib-prefix-hack \
 obstack \
 obstack-printf \
 perror \
 printf-posix \
 progname \
 quote \
 quotearg \
 readline \
 readme-release \
 realloc-posix \
 relocatable-prog \
 relocatable-script \
 rename \
 snprintf-posix \
 spawn-pipe \
 sprintf-posix \
 stdbool \
 stpcpy \
 strdup-posix \
 strerror \
 strverscmp \
 timevar \
 unistd \
 unistd-safer \
 unlink \
 unlocked-io \
 unsetenv \
 update-copyright \
 verify \
 vsnprintf-posix \
 vsprintf-posix \
 warnings \
 winsz-ioctl \
 winsz-termios \
 xalloc \
 xalloc-die \
 xconcat-filename \
 xhash \
 xlist \
 xmemdup0 \
 xstrndup

build-aux/prefix-gnulib-mk --lib-name=libbison lib/gnulib.mk
