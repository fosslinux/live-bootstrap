# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    AUTOCONF=autoconf-2.13 AUTOHEADER=autoheader-2.13 ACLOCAL=aclocal-1.4 AUTOMAKE=automake-1.4 ./bootstrap

    rm doc/libtool.info*
}

src_configure() {
    CC=tcc LD=true AR="tcc -ar" ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-shared \
        --host=i386-unknown-linux \
        --target=i386-unknown-linux \
        --build=i386-unknown-linux
}

src_compile() {
    AR="tcc -ar" AR_FLAGS="cr" make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
