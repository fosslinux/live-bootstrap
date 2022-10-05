# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="https://carlowood.github.io/which/which-2.21.tar.gz"

src_prepare() {
    rm configure Makefile.in aclocal.m4 which.1
    touch ChangeLog which.1
    sed -i '/@ACLOCAL_CWFLAGS@/d' Makefile.am
    autoreconf-2.69 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_install() {
    default

    rm "${DESTDIR}/${PREFIX}/share/man/man1/which.1"
}
