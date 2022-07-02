# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="http://ixpeering.dl.sourceforge.net/project/lzmautils/xz-5.0.5.tar.bz2"

src_prepare() {
    default

    AUTOCONF=autoconf-2.64 AUTOM4TE=autom4te-2.64 autoreconf-2.64 -f
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --disable-shared \
        --build=i386-unknown-linux-musl \
        --libdir="${PREFIX}/lib/musl"
}
