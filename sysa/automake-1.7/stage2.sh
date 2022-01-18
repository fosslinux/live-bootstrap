# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=a4f8b44f15dd79202fd1900e670b2b789aa2b27a632b6b174300c244986b9d0d

src_prepare() {
    rm configure Makefile.in */Makefile.in */*/Makefile.in aclocal.m4 automake.info*

    autoreconf-2.54
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
