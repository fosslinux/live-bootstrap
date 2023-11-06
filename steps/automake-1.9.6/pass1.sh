# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm doc/automake.info*

    sed -i 's/1.8a/1.8.5/; s/ filename-length-max=99//' configure.ac
    AUTOMAKE=automake-1.8 ACLOCAL=aclocal-1.8 AUTOM4TE=autom4te-2.61 AUTOCONF=autoconf-2.61 autoreconf-2.61 -f
}

src_configure() {
    AUTOCONF=autoconf-2.61 ./configure --prefix="${PREFIX}"
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    rm "${DESTDIR}/usr/bin/automake" "${DESTDIR}/usr/bin/aclocal"
}
