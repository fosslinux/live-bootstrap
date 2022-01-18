# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=f2f82ffefc2dbbe72ccc5ae532d207778af2f56537abc0cc756d080eb8a2b11e

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
