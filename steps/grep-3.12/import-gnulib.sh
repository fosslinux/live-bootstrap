#!/bin/sh

# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

rm lib/unictype/ctype_*.h \
 lib/unicase/to{lower,upper}.h

local GNULIB_DIR="$(realpath ../gnulib-3773db6)"

pushd "$GNULIB_DIR"
rm lib/unictype/ctype_*.h \
 lib/unicase/to{lower,upper}.h

pushd lib
cat > config.h <<EOF
#include <stdbool.h>
EOF
gcc -Iunictype -o gen-uni-tables str_startswith.c str_endswith.c gen-uni-tables.c
rm config.h
mv ../../*-16.0.0.txt ../../PropList-3.0.1.txt .
./gen-uni-tables UnicodeData-16.0.0.txt \
    PropList-16.0.0.txt \
    DerivedCoreProperties-16.0.0.txt \
    emoji-data-16.0.0.txt \
    ArabicShaping-16.0.0.txt \
    Scripts-16.0.0.txt \
    Blocks-16.0.0.txt \
    PropList-3.0.1.txt \
    BidiMirroring-16.0.0.txt \
    EastAsianWidth-16.0.0.txt \
    LineBreak-16.0.0.txt \
    WordBreakProperty-16.0.0.txt \
    GraphemeBreakProperty-16.0.0.txt \
    CompositionExclusions-16.0.0.txt \
    SpecialCasing-16.0.0.txt \
    CaseFolding-16.0.0.txt \
    16.0.0
popd

popd

"$GNULIB_DIR"/gnulib-tool --import --local-dir=gl \
 --lib=libgreputils \
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
 --avoid=mbuiter \
 --avoid=mbuiterf \
 --avoid=mbrlen-tests \
 --avoid=mbrtowc-tests \
 --avoid=update-copyright-tests \
 announce-gen \
 argmatch \
 assert-h \
 c-ctype \
 c-stack \
 c-strcasecmp \
 c32isalnum \
 c32rtomb \
 closeout \
 configmake \
 dfa \
 dirname-lgpl \
 do-release-commit-and-tag \
 error \
 exclude \
 fcntl-h \
 fnmatch \
 fstatat \
 fts \
 getopt-gnu \
 getpagesize \
 getprogname \
 gettext-h \
 git-version-gen \
 gitlog-to-changelog \
 gnu-web-doc-update \
 gnupload \
 hash \
 idx \
 ignore-value \
 intprops \
 inttypes-h \
 isatty \
 isblank \
 largefile \
 locale-h \
 lseek \
 maintainer-makefile \
 malloc-gnu \
 manywarnings \
 mbrlen \
 mbrtoc32-regular \
 mbszero \
 mcel-prefer \
 memchr \
 memchr2 \
 mempcpy \
 minmax \
 nullptr \
 obstack \
 openat-safer \
 perl \
 rawmemchr \
 readme-release \
 realloc-posix \
 regex \
 safe-read \
 same-inode \
 ssize_t \
 stdckdint-h \
 stddef-h \
 stdlib-h \
 stpcpy \
 strerror \
 string-h \
 strstr \
 sys_stat-h \
 threadlib \
 unistd-h \
 unlocked-io \
 update-copyright \
 useless-if-before-free \
 verify \
 version-etc-fsf \
 wchar-single \
 windows-stat-inodes \
 xalloc \
 xbinary-io \
 xstrtoimax \
 year2038
