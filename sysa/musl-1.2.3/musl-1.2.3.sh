# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_get() {
    # Before the last pass the tar file is moved before
    # the linux build removes all distfiles to save space.
    if [ -e "../${pkg}.tar.gz" ]; then
        mkdir "${DISTFILES}"
        mv "../${pkg}.tar.gz" "${DISTFILES}"
    else
        default
    fi
}

src_configure() {
    CC=gcc ./configure \
        --host=i386-unknown-linux-musl \
        --disable-shared \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
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
