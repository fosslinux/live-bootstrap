# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    . ../../import-gnulib.sh

    # We don't have autopoint from gettext yet
    AUTOPOINT=true autoreconf -fi

    # Remove bison pregenerated file
    rm gnu/parse-datetime.c
}

src_configure() {
    FORCE_UNSAFE_CONFIGURE=1 ./configure \
        --prefix="${PREFIX}" \
        --disable-nls \
        --target=i386-unknown-linux-gnu \
        --host=i386-unknown-linux-gnu \
        --build=i386-unknown-linux-gnu
}

src_compile() {
    make PREFIX="${PREFIX}" MAKEINFO="true"
}

src_install() {
    make install PREFIX="${PREFIX}" MAKEINFO="true" DESTDIR="${DESTDIR}"
}
