#!/bin/sh

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

../gnulib-30820c/gnulib-tool --import --local-dir=gl \
    --lib=libgnu \
    --source-base=gnu \
    --m4-base=m4 \
    --doc-base=doc \
    --tests-base=tests \
    --aux-dir=build-aux \
    --no-conditional-dependencies \
    --no-libtool \
    --macro-prefix=gl \
    --avoid=lock \
    alloca \
    areadlinkat-with-size \
    argmatch \
    argp \
    argp-version-etc \
    backupfile \
    closeout \
    configmake \
    dirname \
    error \
    exclude \
    exitfail \
    extern-inline \
    faccessat \
    fchmodat \
    fchownat \
    fcntl-h \
    fdopendir \
    fdutimensat \
    file-has-acl \
    fileblocks \
    fnmatch-gnu \
    fprintftime \
    fseeko \
    fstatat \
    full-write \
    futimens \
    getline \
    getopt-gnu \
    getpagesize \
    gettext \
    gettime \
    gitlog-to-changelog \
    hash \
    human \
    inttostr \
    inttypes \
    lchown \
    linkat \
    localcharset \
    manywarnings \
    mkdirat \
    mkdtemp \
    mkfifoat \
    modechange \
    obstack \
    openat \
    parse-datetime \
    priv-set \
    progname \
    quote \
    quotearg \
    readlinkat \
    renameat \
    root-uid \
    rpmatch \
    safe-read \
    savedir \
    selinux-at \
    setenv \
    snprintf \
    stat-time \
    stdbool \
    stdint \
    stdopen \
    stpcpy \
    strdup-posix \
    strerror \
    strnlen \
    strtoimax \
    strtol \
    strtoul \
    strtoumax \
    symlinkat \
    timespec \
    timespec-sub \
    unlinkat \
    unlinkdir \
    unlocked-io \
    utimensat \
    version-etc-fsf \
    xalloc \
    xalloc-die \
    xgetcwd \
    xstrtoumax \
    xvasprintf
