# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    find . -name '*.info' -delete
    autoreconf-2.69 -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --target=i386-unknown-linux-gnu \
        --host=i386-unknown-linux-gnu \
        --build=i386-unknown-linux-gnu \
        --disable-shared
}

src_compile() {
    make MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
