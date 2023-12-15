#!/bin/sh

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

../gnulib-b81ec69/gnulib-tool --import \
    --lib=libgnu \
    --source-base=gnulib/lib \
    --m4-base=gnulib/m4 \
    --doc-base=doc \
    --tests-base=tests \
    --aux-dir=build-aux \
    --conditional-dependencies \
    --no-libtool \
    --macro-prefix=gl \
    --no-vc-files \
    argz \
    getopt-gnu \
    gettext-h \
    iconv \
    mbchar \
    mbiter \
    mbscasecmp \
    mbschr \
    mbslen \
    mbsncasecmp \
    mbsstr \
    mbswidth \
    memrchr \
    regex \
    stdarg \
    strcasestr \
    strdup-posix \
    strerror \
    vasprintf \
    xalloc
