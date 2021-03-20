# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_configure() {
    CC=gcc ./configure \
        --host=i386 \
        --disable-shared \
        --prefix=/after \
        --libdir=/after/lib/musl \
        --includedir=/after/include/

    # configure script creates this file
    test -f /dev/null && rm /dev/null && mknod -m 666 /dev/null c 1 3
}

src_compile() {
    make CROSS_COMPILE=
}
