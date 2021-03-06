# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove pre-generated flex/bison files
    rm src/parse-gram.c src/parse-gram.h
    rm src/scan-code.c
    rm src/scan-gram.c
    rm src/scan-skel.c

    # Remove pregenerated info files
    rm doc/bison.info

    ../../import-gnulib.sh

    AUTOPOINT=true autoreconf -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}" --disable-nls
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
