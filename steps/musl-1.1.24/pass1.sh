# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # meslibc is insufficient to regenerate src/ctype or src/iconv
    # disable everything using a generated header
    patch -Np1 -i ../../files/disable_ctype_headers.patch
    rm src/ctype/iswalpha.c src/ctype/iswalnum.c src/ctype/iswctype.c \
        src/ctype/towctrans.c
    rm include/iconv.h src/locale/iconv.c src/locale/iconv_close.c

    # tcc does not support complex types
    rm -rf src/complex

    # Configure fails without this
    mkdir -p /dev
}

src_configure() {
    CC=tcc ./configure \
      --host=i386 \
      --disable-shared \
      --prefix="${PREFIX}" \
      --libdir="${LIBDIR}" \
      --includedir="${PREFIX}/include/"

    # configure script creates this file
    if test -f /dev/null; then
        rm /dev/null
    fi
}

src_compile() {
    make "${MAKEJOBS}" CROSS_COMPILE= AR="tcc -ar" RANLIB=true CFLAGS="-DSYSCALL_NO_TLS"
}
