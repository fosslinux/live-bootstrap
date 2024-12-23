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

src_prepare() {
    default

    # regenerate headers in src/ctype and src/iconv
    cd ../musl-chartable-tools-78b213a868553b1154ee9627c96ff1f14a9a3b1b
    cd ctype
    CC=gcc make
    cp alpha.h punct.h nonspacing.h wide.h ../../musl-1.2.5/src/ctype/
    cd ../iconv
    CC=gcc make
    cp legacychars.h codepages.h jis0208.h gb18030.h hkscs.h ksc.h revjis.h \
        ../../musl-1.2.5/src/locale/
    cd ../../musl-1.2.5
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
    make "${MAKEJOBS}" CROSS_COMPILE=
}
