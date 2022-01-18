# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=245dffa71bf97be04ff78959987178c51e984d4f957f4346308dfa0d0bacdb7a

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
