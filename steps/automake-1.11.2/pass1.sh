# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm -f doc/amhello-1.0.tar.gz doc/automake.info* doc/aclocal-1.11.1 doc/automake-1.11.1

    # Building doc often causes race conditions, skip it
    awk '/SUBDIRS/{sub("doc ", "", $0)} {print}' Makefile.am > Makefile.am.tmp
    mv Makefile.am.tmp Makefile.am

    AUTOCONF=autoconf-2.64 AUTOM4TE=autom4te-2.64 ./bootstrap
}

src_configure() {
    AUTORECONF=autoreconf-2.64 AUTOM4TE=autom4te-2.64 AUTOHEADER=autoheader-2.64 AUTOCONF=autoconf-2.64 ./configure --prefix="${PREFIX}"
}

src_compile() {
    AUTORECONF=autoreconf-2.64 AUTOM4TE=autom4te-2.64 AUTOHEADER=autoheader-2.64 AUTOCONF=autoconf-2.64 make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    rm "${DESTDIR}/usr/bin/automake"
}
