# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm configure standards.info autoconf.info
    touch autoconf.info
    autoconf-2.52

    sed -i '/^acdatadir/s:$:-2.12:' Makefile.in
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.12
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
