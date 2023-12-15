# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

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
        --libdir="${LIBDIR}" \
        --disable-shared
}
