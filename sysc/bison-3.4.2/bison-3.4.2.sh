# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=1963bc0ea0d2865eded29158fb31e9573897aef041ee04fddc800187381cfeda

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
    ./configure --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-nls
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
