# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # regenerate headers in src/ctype and src/iconv
    cd ../musl-chartable-tools-44d780e03e78efcb3168ceab068170206dc92e85
    patch -Np1 -i ../../files/cut.patch
    cd ctype
    CC=tcc make
    cp alpha.h punct.h nonspacing.h wide.h ../../musl-1.1.24/src/ctype/
    cd ../iconv
    CC=tcc make
    cp legacychars.h codepages.h jis0208.h gb18030.h hkscs.h ksc.h revjis.h \
        ../../musl-1.1.24/src/locale/
    cd ../../musl-1.1.24

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
