# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="http://mirrors.kernel.org/gnu/automake/automake-1.15.1.tar.xz"

src_prepare() {
    default

    rm doc/amhello-1.0.tar.gz

    AUTOCONF=autoconf-2.69 AUTOM4TE=autom4te-2.69 ./bootstrap

    rm doc/automake-history.info doc/automake.info*

    cp "${PREFIX}/bin/help2man" doc/
}

src_configure() {
    AUTORECONF=autoreconf-2.69 AUTOM4TE=autom4te-2.69 AUTOHEADER=autoheader-2.69 AUTOCONF=autoconf-2.69 ./configure --prefix="${PREFIX}"
}

src_compile() {
    AUTORECONF=autoreconf-2.69 AUTOM4TE=autom4te-2.69 AUTOHEADER=autoheader-2.69 AUTOCONF=autoconf-2.69 make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    rm "${DESTDIR}/usr/bin/automake" "${DESTDIR}/usr/bin/aclocal"
    rm "${DESTDIR}${PREFIX}/share/doc/automake/amhello-1.0.tar.gz"
}
