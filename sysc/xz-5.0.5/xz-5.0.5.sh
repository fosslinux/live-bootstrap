# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    autoreconf-2.64 -f
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --disable-shared \
        --build=i386-unknown-linux-musl \
        --libdir="${PREFIX}/lib/musl"
}
