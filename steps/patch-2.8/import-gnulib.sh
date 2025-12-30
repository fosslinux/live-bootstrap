#!/bin/sh

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Regenerate unicode files
pushd ../gnulib-9829f0a/lib
gcc -Iunictype -o gen-uni-tables gen-uni-tables.c
mv ../../*.txt .
./gen-uni-tables UnicodeData-16.0.0.txt \
    PropList-16.0.0.txt \
    DerivedCoreProperties-16.0.0.txt \
    ArabicShaping-16.0.0.txt \
    Scripts-16.0.0.txt \
    Blocks-16.0.0.txt \
    PropList-3.0.1.txt \
    EastAsianWidth-16.0.0.txt \
    LineBreak-16.0.0.txt \
    WordBreakProperty-16.0.0.txt \
    GraphemeBreakProperty-16.0.0.txt \
    CompositionExclusions-16.0.0.txt \
    SpecialCasing-16.0.0.txt \
    CaseFolding-16.0.0.txt \
    16.0.0
popd

../gnulib-9829f0a/gnulib-tool --import \
 --local-dir=gl \
 --lib=libpatch \
 --source-base=lib \
 --m4-base=m4 \
 --doc-base=doc \
 --tests-base=tests \
 --aux-dir=build-aux \
 --no-conditional-dependencies \
 --no-libtool \
 --macro-prefix=gl \
 announce-gen \
 argmatch \
 assert-h \
 attribute \
 backupfile \
 basename-lgpl \
 bool \
 c-ctype \
 closeout \
 diffseq \
 dup2 \
 errno-h \
 exitfail \
 extensions \
 faccessat \
 fchmodat \
 fchownat \
 fcntl-h \
 filename \
 fseeko \
 fstatat \
 ftello \
 futimens \
 getopt-gnu \
 gettime \
 git-version-gen \
 gitlog-to-changelog \
 gnupload \
 hash \
 idx \
 ignore-value \
 intprops \
 inttypes-h \
 largefile \
 maintainer-makefile \
 malloc-gnu \
 manywarnings \
 memchr \
 mempcpy \
 minmax \
 mkdirat \
 nullptr \
 openat \
 parse-datetime \
 progname \
 quotearg \
 raise \
 readlinkat \
 realloc-posix \
 renameat \
 setenv \
 signal-h \
 ssize_t \
 stat-time \
 stdckdint-h \
 stdlib-h \
 stpcpy \
 symlinkat \
 sys_stat-h \
 tempname \
 test-xfail \
 unistd-h \
 unlinkat \
 update-copyright \
 utimensat \
 verror \
 xalloc \
 xstdopen \
 year2038-recommended
