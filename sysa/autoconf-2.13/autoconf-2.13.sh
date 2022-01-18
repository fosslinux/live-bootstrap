# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=ef579e5f53689bd5facdd93fd9573785b4778fd18a739932fb0fd91c1bddde18

src_prepare() {
    rm configure standards.info autoconf.info
    touch autoconf.info
    autoconf-2.52

    sed -i '/^acdatadir/s:$:-2.13:' Makefile.in
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.13
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
