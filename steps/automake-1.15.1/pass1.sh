# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm doc/amhello-1.0.tar.gz

    # Building doc often causes race conditions, skip it
    sed -i '/doc\/Makefile.inc/d' Makefile.am
    sed -i '/t\/Makefile.inc/d' Makefile.am

    AUTOCONF="autoconf-2.69 -f" AUTOM4TE=autom4te-2.69 ./bootstrap

    rm doc/automake-history.info doc/automake.info*
}

src_configure() {
    AUTORECONF=autoreconf-2.69 AUTOM4TE=autom4te-2.69 AUTOHEADER=autoheader-2.69 AUTOCONF="autoconf-2.69 -f" ./configure --prefix="${PREFIX}"
}

src_compile() {
    AUTORECONF=autoreconf-2.69 AUTOM4TE=autom4te-2.69 AUTOHEADER=autoheader-2.69 AUTOCONF="autoconf-2.69 -f" make -j1 MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    rm "${DESTDIR}/usr/bin/automake" "${DESTDIR}/usr/bin/aclocal"
}
