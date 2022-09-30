#!/bin/sh

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

../gnulib-d279bc/gnulib-tool --import --local-dir=gl \
  --lib=libsed \
  --source-base=lib \
  --m4-base=m4 \
  --doc-base=doc \
  --tests-base=gnulib-tests \
  --aux-dir=build-aux \
  --with-tests \
  --makefile-name=gnulib.mk \
  --no-conditional-dependencies \
  --no-libtool \
  --macro-prefix=gl \
  --avoid=lock-tests \
  acl \
  alloca \
  binary-io \
  btowc \
  c-ctype \
  closeout \
  dfa \
  extensions \
  fdl \
  fwriting \
  getdelim \
  getopt \
  gettext-h \
  git-version-gen \
  gitlog-to-changelog \
  ignore-value \
  localcharset \
  manywarnings \
  mbrlen \
  mbrtowc \
  mbsinit \
  memchr \
  memrchr \
  mkostemp \
  non-recursive-gnulib-prefix-hack \
  obstack \
  perl \
  progname \
  readme-release \
  regex \
  rename \
  selinux-h \
  ssize_t \
  stat-macros \
  stdalign \
  stdbool \
  strerror \
  strverscmp \
  unlocked-io \
  update-copyright \
  verify \
  version-etc-fsf \
  wcrtomb \
  wctob \
  xalloc

./build-aux/prefix-gnulib-mk --lib-name=libsed lib/gnulib.mk
