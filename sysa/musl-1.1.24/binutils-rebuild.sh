# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=17b4d6779e1b2f61fe120546553cc3c72201bd4d639f2e6f8b65e98df1b24025

src_prepare() {
    default

    # tcc does not support complex types
    rm -rf src/complex
}

src_configure() {
    CC=tcc ./configure \
      --host=i386 \
      --disable-shared \
      --prefix="${PREFIX}" \
      --libdir="${PREFIX}/lib/musl" \
      --includedir="${PREFIX}/include/"

    # configure script creates this file
    if test -f /dev/null; then
        rm /dev/null
    fi
}

src_compile() {
    make CROSS_COMPILE= CFLAGS="-DSYSCALL_NO_TLS" AS_CMD='as -o $@ $<'
}

src_install() {
    rm -rf "${PREFIX}/include"
    make install
}
