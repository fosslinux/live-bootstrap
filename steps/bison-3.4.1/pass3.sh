# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    mv lib/textstyle.in.h lib/textstyle.h

    # Remove pre-generated flex/bison files
    rm src/parse-gram.c src/parse-gram.h
    rm src/scan-code.c
    rm src/scan-gram.c
    rm src/scan-skel.c

    cp ../../mk/lib.mk lib/Makefile
    cp ../../mk/src.mk src/Makefile
}

src_compile() {
    make -j1 -f Makefile PREFIX="${PREFIX}"
}
