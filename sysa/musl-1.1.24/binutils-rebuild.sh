# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # tcc does not support complex types
    rm -rf src/complex
}

src_configure() {
    CC=tcc ./configure \
      --host=i386 \
      --disable-shared \
      --prefix=/after \
      --libdir=/after/lib/musl \
      --includedir=/after/include/

    # configure script creates this file
    if test -f /dev/null; then
        rm /dev/null
    fi
}

src_compile() {
    make CROSS_COMPILE= CFLAGS="-DSYSCALL_NO_TLS" AS_CMD='as -o $@ $<'
}

src_install() {
    rm -rf /after/include
    make install
}
