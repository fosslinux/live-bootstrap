# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=d0a309628ecc5a9f331a8e4275412689b1c27509f7ebf255a8df2324a591381c

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
