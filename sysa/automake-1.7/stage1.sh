# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm configure Makefile.in */Makefile.in */*/Makefile.in aclocal.m4
    sed -i -e 's/2.54/2.53/' -e '/AC_PROG_EGREP/d' -e '/AC_PROG_FGREP/d' configure.in
    aclocal-1.6
    autoconf-2.53
    automake-1.6
}

src_configure() {
    ./configure --prefix=/after
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true
}

