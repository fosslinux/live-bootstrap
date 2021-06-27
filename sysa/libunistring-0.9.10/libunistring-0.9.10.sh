# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    find . -name '*.info*' -delete

    # libunistring does not specify which gnulib snapshot was used,
    # pick a random one that works
    GNULIB_TOOL=../gnulib-52a06cb3a849df0bbce38b69dd7ae58cc1de5f68/gnulib-tool ./autogen.sh

    # autogen.sh does not regenerate libtool files
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
