#!/bin/sh

# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

rm gnulib/lib/unictype/ctype_*.h \
 gnulib/lib/uniwidth/width*.h \
 gnulib/lib/unicase/tolower.h \
 tp/Texinfo/XS/gnulib/lib/unicase/{tolower,cased,ignorable,toupper}.h \
 tp/Texinfo/XS/gnulib/lib/unicase/special-casing-table.gperf \
 tp/Texinfo/XS/gnulib/lib/uninorm/composition-table* \
 tp/Texinfo/XS/gnulib/lib/uninorm/decomposition-table{1,2}.h \
 tp/Texinfo/XS/gnulib/lib/uniwidth/width*.h \
 tp/Texinfo/XS/gnulib/lib/unictype/{combiningclass,ctype_upper}.h \
 tp/Texinfo/XS/gnulib/lib/unictype/{pr_,categ_}*.h

GNULIB_DIR=$(realpath ../gnulib-d82702e)

pushd "$GNULIB_DIR"
rm lib/unictype/ctype_*.h \
 lib/unictype/combiningclass.h \
 lib/unictype/{pr_,categ_}*.h \
 lib/unicase/{tolower,cased,ignorable,toupper}.h \
 lib/unicase/special-casing-table.gperf \
 lib/uninorm/composition-table* \
 lib/uninorm/decomposition-table{1,2}.h \
 lib/uniwidth/width*.h

pushd lib
gcc -Iunictype -o gen-uni-tables gen-uni-tables.c
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

"$GNULIB_DIR"/gnulib-tool --import \
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
 mkstemp \
 regex \
 stdarg \
 strcasestr \
 strdup-posix \
 strerror \
 vasprintf \
 xalloc

pushd tp/Texinfo/XS
"$GNULIB_DIR"/gnulib-tool --import \
 --lib=libgnu \
 --source-base=gnulib/lib \
 --m4-base=gnulib/m4 \
 --doc-base=doc \
 --tests-base=tests \
 --aux-dir=build-aux \
 --no-conditional-dependencies \
 --libtool \
 --macro-prefix=gl \
 euidaccess \
 getline \
 gettext-h \
 iconv \
 libunistring \
 locale \
 setenv \
 strchrnul \
 strndup \
 uchar \
 unicase/u8-tolower \
 unicase/u8-toupper \
 unictype/category-L \
 unictype/category-M \
 unictype/category-Mn \
 unictype/category-Nd \
 unictype/category-test \
 unictype/ctype-upper \
 unictype/property-alphabetic \
 unictype/property-join-control \
 unictype/property-test \
 uninorm/nfc \
 uninorm/nfkd \
 uninorm/u8-normalize \
 unistr/u32-next \
 unistr/u8-mbsnlen \
 unistr/u8-mbtouc \
 unistr/u8-next \
 unistr/u8-strlen \
 unistr/u8-strmbtouc \
 unistr/u8-uctomb \
 uniwidth/u8-strwidth \
 uniwidth/u8-width \
 unsetenv \
 vasprintf
popd
