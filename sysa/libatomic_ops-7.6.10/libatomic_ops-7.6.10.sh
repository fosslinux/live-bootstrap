# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    autoreconf-2.69 -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --build=i386-unknown-linux-gnu \
        --host=i386-unknown-linux-gnu \
        --target=i386-unknown-linux-gnu \
        --libdir="${PREFIX}/lib/musl" \
        --disable-shared
}
