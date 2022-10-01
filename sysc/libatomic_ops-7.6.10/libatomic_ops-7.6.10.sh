# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    autoreconf-2.71 -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --enable-shared
}
