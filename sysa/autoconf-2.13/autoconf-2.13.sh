# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

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
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
