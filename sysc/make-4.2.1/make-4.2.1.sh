# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=6278b4b3f7ff4c1b04e24229b17e3cb67c282d9d9d74ca0f936e31ed70b3a1f4

src_prepare() {
    default

    rm doc/make.info
    touch doc/make.info

    # We don't have autopoint from gettext yet
    AUTOPOINT=true autoreconf -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --build=i386-unknown-linux-musl \
        --disable-nls
}

src_compile() {
    make MAKEINFO="true"
}

src_install() {
    make install MAKEINFO="true" DESTDIR="${DESTDIR}"
}
