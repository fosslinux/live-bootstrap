# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm configure
    find . -name 'Makefile.in' -delete

    ../../import-gnulib.sh

    autoreconf-2.69 -fi
}

src_configure() {
    GL_GENERATE_ALLOCA_H_TRUE=0 LDFLAGS="-static" ./configure --prefix="${PREFIX}"
}

src_install() {
    default
}
