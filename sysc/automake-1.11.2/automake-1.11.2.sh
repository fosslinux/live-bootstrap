# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="http://mirrors.kernel.org/gnu/automake/automake-1.11.2.tar.bz2"

src_prepare() {
    default

    rm -f doc/amhello-1.0.tar.gz doc/automake.info* doc/aclocal-1.11.1 doc/automake-1.11.1

    AUTOCONF=autoconf-2.64 AUTOM4TE=autom4te-2.64 ./bootstrap
}

src_configure() {
    AUTORECONF=autoreconf-2.64 AUTOM4TE=autom4te-2.64 AUTOHEADER=autoheader-2.64 AUTOCONF=autoconf-2.64 ./configure --prefix="${PREFIX}"
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    rm "${DESTDIR}/usr/bin/automake" "${DESTDIR}/usr/bin/aclocal"
    rm "${DESTDIR}${PREFIX}/share/doc/automake/amhello-1.0.tar.gz"
}
