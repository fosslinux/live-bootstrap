# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm doc/standards.info
    autoreconf-2.69 -fi

    # Install autoconf data files into versioned directory
    sed -i '/^pkgdatadir/s:$:-@VERSION@:' Makefile.in
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.71
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"

    ln -s autoconf-2.71 "${DESTDIR}${PREFIX}/bin/autoconf"
    ln -s autoheader-2.71 "${DESTDIR}${PREFIX}/bin/autoheader"
    ln -s autom4te-2.71 "${DESTDIR}${PREFIX}/bin/autom4te"
    ln -s autoreconf-2.71 "${DESTDIR}${PREFIX}/bin/autoreconf"
}
