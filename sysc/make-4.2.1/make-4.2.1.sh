# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later


src_prepare() {
    default

    rm doc/make.info
    touch doc/make.info

    # We don't have autopoint from gettext yet
    AUTOPOINT=true AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15  autoreconf-2.69 -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --build=i386-unknown-linux-musl \
        --disable-nls
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO="true"
}

src_install() {
    make install MAKEINFO="true" DESTDIR="${DESTDIR}"
}
