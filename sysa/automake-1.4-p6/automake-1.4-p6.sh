# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm configure Makefile.in */Makefile.in aclocal.m4 automake.info*
    aclocal-1.6
    autoconf-2.52
    # When building with newer automake we get the following error
    # Makefile.am:59: ETAGS_ARGS multiply defined in condition TRUE
    sed -i '/ETAGS_ARGS/,+1d' Makefile.am
    automake-1.6
}

src_configure() {
    ./configure --prefix=/after
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
