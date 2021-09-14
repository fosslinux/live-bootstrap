#!/bin/sh

# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

../gnulib-672663a/gnulib-tool --import --local-dir=gl \
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
    getopt-gnu \
    gettext-h \
    git-version-gen \
    gitlog-to-changelog \
    gpl-3.0 \
    inttypes \
    isnan \
    javacomp-script \
    javaexec-script \
    ldexpl \
    libtextstyle-optional \
    mbswidth \
    non-recursive-gnulib-prefix-hack \
    obstack \
    obstack-printf \
    perror \
    printf-posix \
    progname \
    quote \
    quotearg \
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
    xalloc \
    xalloc-die \
    xconcat-filename \
    xhash \
    xlist \
    xmemdup0 \
    xstrndup

../gnulib-672663a/build-aux/prefix-gnulib-mk \
    --lib-name=libbison \
    lib/gnulib.mk
