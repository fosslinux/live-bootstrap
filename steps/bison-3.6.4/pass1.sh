# SPDX-FileCopyrightText: 2021-22 Samuel Tyler <samuel@samuelt.me>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # GRAM_error was added after Bison 3.4, and doesn't add anything
    # other than some sanity checks.
    sed -i '/GRAM_error/d' src/scan-gram.l

    # Remove pre-generated flex/bison files
    rm src/parse-gram.c src/parse-gram.h
    rm src/scan-code.c
    rm src/scan-gram.c
    rm src/scan-skel.c

    # Remove pregenerated info files
    rm doc/bison.info*

    # Remove gettext files
    rm runtime-po/*.gmo

    ../../import-gnulib.sh

    # pregenerated gperf files
    for f in lib/iconv_open-*.gperf; do
        touch "$(basename "$f" .gperf).h"
    done

    AUTOPOINT=true AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --disable-nls
}

src_compile() {
    make -j1 MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
