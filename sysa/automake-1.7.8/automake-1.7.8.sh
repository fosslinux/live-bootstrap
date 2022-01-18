# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=57de0e448b3b334ea728cf1eac5f3a4d75953ec09b0f41c76ada93ef8a1621c0

src_prepare() {
    rm configure Makefile.in */Makefile.in */*/Makefile.in aclocal.m4 automake.info*

    autoreconf-2.55
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
