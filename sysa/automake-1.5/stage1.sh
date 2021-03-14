# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default_src_prepare

    rm configure Makefile.in */Makefile.in */*/Makefile.in
    autoconf-2.52

    aclocal-1.4
    automake-1.4
}

src_configure() {
    ./configure --prefix=/after
}

src_install() {
    install automake "${PREFIX}/bin/automake-1.5"
    mkdir -p "${PREFIX}/share/automake/Automake"
    install -m644 lib/Automake/Struct.pm "${PREFIX}/share/automake/Automake/"
    mkdir -p "${PREFIX}/share/automake/am"
    cp lib/am/*.am "${PREFIX}/share/automake/am/"
}
