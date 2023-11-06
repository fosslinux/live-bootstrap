# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm doc/automake.info*

    AUTOMAKE=automake-1.7 ACLOCAL=aclocal-1.7 AUTOCONF=autoconf-2.59 autoreconf-2.59 -f
}

src_configure() {
    AUTOCONF=autoconf-2.59 ./configure --prefix="${PREFIX}"
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    rm "${DESTDIR}/usr/bin/automake" "${DESTDIR}/usr/bin/aclocal"
}
