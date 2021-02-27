# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    sed -i 's#m4/Makefile tests/Makefile##; s/Makefile //' configure.in

    rm configure Makefile.in */Makefile.in
    autoconf-2.52
}

src_configure() {
    ./configure --prefix=/after
}

src_compile() {
    :
}

src_install() {
    install automake ${PREFIX}/bin/automake-1.4
    mkdir -p ${PREFIX}/share/automake-1.4
    cp -r *.am ${PREFIX}/share/automake-1.4/
}
