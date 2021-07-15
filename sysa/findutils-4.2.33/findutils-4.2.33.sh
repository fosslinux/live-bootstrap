# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    . ../../import-gnulib.sh

    autoreconf-2.61 -f

    # Pre-built texinfo files
    rm doc/find.info
}

src_configure() {
    # Musl is not recognized, pretend to be uClibc
    CC=tcc ./configure --prefix="${PREFIX}" CPPFLAGS="-D__UCLIBC__"
}

src_compile() {
    make MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
