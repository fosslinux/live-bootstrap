# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

SRCS="musl-1.2.2.tar.gz"

src_configure() {
    CC=gcc ./configure \
        --host=i386-unknown-linux-musl \
        --disable-shared \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --includedir="${PREFIX}/include/"

    # configure script creates this file
    if test -f /dev/null; then
        rm /dev/null
        mknod -m 666 /dev/null c 1 3
    fi
}

src_compile() {
    make CROSS_COMPILE=
}
