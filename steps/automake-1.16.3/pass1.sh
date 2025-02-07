# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2025 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    # Disable amhello, it is not reproducible
    rm doc/amhello-1.0.tar.gz
    sed -i "/^dist_doc_DATA =/d" doc/local.mk

    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 AUTOCONF=autoconf-2.69 AUTOM4TE=autom4te-2.69 ./bootstrap

    rm doc/automake-history.info doc/automake.info*
    grep "DO NOT EDIT BY HAND" -r t -l | while read f; do
        rm "$f"
    done

    cp "${PREFIX}/bin/help2man" doc/

    perl ./gen-testsuite-part --srcdir . > t/testsuite-part.am
    touch Makefile.in # timestamps for make rules
}

src_configure() {
    AUTOCONF=autoconf-2.69 ./configure --prefix="${PREFIX}"
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
