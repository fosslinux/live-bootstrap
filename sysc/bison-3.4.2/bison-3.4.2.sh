# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="http://mirrors.kernel.org/gnu/bison/bison-3.4.2.tar.xz
 http://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-672663a.tar.gz"

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

    AUTOPOINT=true autoreconf-2.69 -fi
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
