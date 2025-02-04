# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm doc/standards.info man/*.1
    sed -i -e '/AC_PROG_GREP/d' -e '/AC_PROG_SED/d' configure.ac

    AUTOMAKE=automake-1.8 ACLOCAL=aclocal-1.8 AUTOM4TE=autom4te-2.59 AUTOCONF=autoconf-2.59 autoreconf-2.59 -f

    # Install autoconf data files into versioned directory
    for file in */*/Makefile.in */Makefile.in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' "$file"
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.61
}

src_compile() {
    make -j1 MAKEINFO=true PREFIX="${PREFIX}"
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    ln -s autoconf-2.61 "${DESTDIR}${PREFIX}/bin/autoconf"
    ln -s autoheader-2.61 "${DESTDIR}${PREFIX}/bin/autoheader"
    ln -s autom4te-2.61 "${DESTDIR}${PREFIX}/bin/autom4te"
    ln -s autoreconf-2.61 "${DESTDIR}${PREFIX}/bin/autoreconf"
}
