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
    cp m4/amversion.in m4/amversion.m4
    sed -i 's/@VERSION@/1.4-p6/' m4/amversion.m4
    sed -i 's/@APIVERSION@/1.4/' m4/amversion.m4
}

src_install() {
    install automake "${PREFIX}"/bin/automake-1.4
    mkdir -p "${PREFIX}"/share/automake-1.4
    cp -r *.am "${PREFIX}"/share/automake-1.4/

    install aclocal "${PREFIX}"/bin/aclocal-1.4
    mkdir -p "${PREFIX}"/share/aclocal-1.4
    cp -r m4/*.m4 "${PREFIX}"/share/aclocal-1.4/
}
