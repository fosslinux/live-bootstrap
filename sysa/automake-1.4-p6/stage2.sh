# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm configure Makefile.in */Makefile.in
    autoconf-2.52
    automake-1.4
}

src_configure() {
    ./configure --prefix=/after
}

src_install() {
    # cleanup old manual install
    rm ${PREFIX}/bin/automake-1.4
    rm -rf ${PREFIX}/share/automake-1.4

    default_src_install
}
