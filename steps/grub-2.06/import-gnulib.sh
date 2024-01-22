#!/bin/sh

# SPDX-FileCopyrightText: 2021 Melg Eight <public.melg8@gmail.com>
# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2024 Gábor Stefanik <netrolller.3d@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

# Regenerate unicode files
pushd ../gnulib-d271f86/lib
gcc -Iunictype -o gen-uni-tables gen-uni-tables.c
mv ../../*.txt .
./gen-uni-tables UnicodeData-9.0.0.txt PropList-9.0.0.txt DerivedCoreProperties-9.0.0.txt ArabicShaping-9.0.0.txt Scripts-9.0.0.txt Blocks-9.0.0.txt PropList-3.0.1.txt EastAsianWidth-9.0.0.txt LineBreak-9.0.0.txt WordBreakProperty-9.0.0.txt GraphemeBreakProperty-9.0.0.txt CompositionExclusions-9.0.0.txt SpecialCasing-9.0.0.txt CaseFolding-9.0.0.txt 9.0
popd

../gnulib-d271f86/gnulib-tool --import --local-dir=gl \
  --lib=libgnu \
  --source-base=grub-core/lib/gnulib \
  --m4-base=m4 \
  --doc-base=doc \
  --tests-base=tests \
  --aux-dir=build-aux \
  --no-conditional-dependencies \
  --no-libtool \
  --macro-prefix=gl \
  --no-vc-files \
  argp \
  base64 \
  error \
  fnmatch \
  getdelim \
  getline \
  gettext-h \
  gitlog-to-changelog \
  mbswidth \
  progname \
  realloc-gnu \
  regex \
  save-cwd
