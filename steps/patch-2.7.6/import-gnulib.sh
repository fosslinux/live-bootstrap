#!/bin/sh

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

../gnulib-e017871/gnulib-tool --import \
    --local-dir=gl \
    --lib=libpatch \
    --source-base=lib \
    --m4-base=m4 \
    --doc-base=doc \
    --tests-base=tests \
    --aux-dir=build-aux \
    --makefile-name=gnulib.mk \
    --no-conditional-dependencies \
    --no-libtool \
    --macro-prefix=gl \
    argmatch \
    backupfile \
    clock-time \
    diffseq \
    dirname \
    dup2 \
    errno \
    exitfail \
    extensions \
    faccessat \
    fchmodat \
    fchownat \
    fcntl-h \
    fstatat \
    full-write \
    getdate \
    getopt-gnu \
    gettime \
    git-version-gen \
    gitlog-to-changelog \
    gnupload \
    hash \
    ignore-value \
    intprops \
    largefile \
    linked-list \
    maintainer-makefile \
    malloc \
    manywarnings \
    memchr \
    minmax \
    mkdirat \
    nstrftime \
    openat \
    progname \
    quotearg \
    readlinkat \
    realloc \
    renameat \
    setenv \
    signal \
    size_max \
    ssize_t \
    stat-time \
    stdbool \
    stdlib \
    symlinkat \
    sys_stat \
    tempname \
    time \
    unistd \
    unlinkat \
    update-copyright \
    utimensat \
    verror \
    xalloc \
    xlist \
    xmemdup0
