# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=3a9d7649afe75a76583305283c8606e717cbab01b098f094f806e8e37045a658

src_prepare() {
    autoreconf-2.71 -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-shared
}
