# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

EXTRA_DISTFILES="gnulib-8e128e.tar.gz"

src_prepare() {
    . ../../import-gnulib.sh

    default

    AUTOMAKE=automake-1.9 ACLOCAL=aclocal-1.9 AUTOM4TE=autom4te-2.61 AUTOCONF=autoconf-2.61 autoreconf-2.61 -f

    # Pre-built texinfo files
    rm doc/find.info
}

src_configure() {
    # Musl is not recognized, pretend to be uClibc
    # Must use --host for config.charset reproducibility
    CC=tcc ./configure --prefix="${PREFIX}" \
        --host=i386-unknown-linux-gnu \
        CPPFLAGS="-D__UCLIBC__"
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
