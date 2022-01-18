# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=19bde5b058c188e976f4cfcea35d9f0d897daf593479a7db00eb6a6eabefd131

src_prepare() {
    find . -name '*.info*' -delete

    # libunistring does not specify which gnulib snapshot was used,
    # pick a random one that works
    GNULIB_TOOL=../gnulib-52a06cb3/gnulib-tool ./autogen.sh

    # autogen.sh does not regenerate libtool files
    autoreconf-2.69 -fi
}

src_configure() {
    ./configure \
	--prefix="${PREFIX}" \
	--libdir="${PREFIX}/lib/musl" \
	--disable-shared
}
