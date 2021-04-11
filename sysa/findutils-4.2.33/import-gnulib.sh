#!/bin/sh

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

../gnulib-8e128e/gnulib-tool \
    --import \
    --dir=. \
    --lib=libgnulib \
    --source-base=gnulib/lib \
    --m4-base=gnulib/m4 \
    --doc-base=doc \
    --aux-dir=. \
    --with-tests \
    --no-libtool \
    --macro-prefix=gl \
    alloca \
    argmatch \
    canonicalize \
    closein \
    closeout \
    dirname \
    error \
    fdl \
    fileblocks \
    filemode \
    fnmatch-gnu \
    fopen-safer \
    getline \
    getopt \
    gettext \
    human \
    idcache \
    lstat \
    malloc \
    memcmp \
    memset \
    mktime \
    modechange \
    mountlist \
    pathmax \
    quotearg \
    realloc \
    regex \
    rpmatch \
    savedir \
    stat-macros \
    stpcpy \
    strdup \
    strftime \
    strstr \
    strtol \
    strtoul \
    strtoull \
    strtoumax \
    xalloc \
    xalloc-die \
    xgetcwd \
    xstrtol \
    xstrtoumax \
    yesno
