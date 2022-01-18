# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=37156bd81143981d986a9e728e1dbdd2ead130b454be6cf46315b80d9c04f5aa

src_prepare() {
    default

    mv lib/textstyle.in.h lib/textstyle.h

    # Remove pre-generated flex/bison files
    rm src/parse-gram.c src/parse-gram.h
    rm src/scan-code.c
    rm src/scan-gram.c
    rm src/scan-skel.c

    # Handwritten bison parser
    mv parse-gram.c parse-gram.h src/

    cp ../../mk/lib.mk lib/Makefile
    cp ../../mk/src.mk src/Makefile
}
