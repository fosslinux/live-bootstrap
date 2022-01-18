# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=1c4358bb5c67eb8a194800d3b705cccf51224c9ef6aa2d847463495c740b7d69

src_prepare() {
    default

    mv lib/textstyle.in.h lib/textstyle.h

    # Remove pre-generated flex/bison files
    rm src/parse-gram.c src/parse-gram.h
    rm src/scan-code.c
    rm src/scan-gram.c
    rm src/scan-skel.c

    # Simplified bison grammar
    mv parse-gram.y src/

    cp ../../mk/lib.mk lib/Makefile
    cp ../../mk/src.mk src/Makefile
}
