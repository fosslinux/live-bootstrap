# SPDX-FileCopyrightText: 2021-2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    rm -- Makefile.in */Makefile.in */*/Makefile.in aclocal.m4 configure
    rm doc/standards.info doc/autoconf.info

    # Do not use pregenerated manpages
    sed -i '/SUBDIRS/s/ man//' Makefile.am

    aclocal-1.6
    cat config/m4.m4 >> aclocal.m4
    autoconf-2.52
    automake-1.6

    # Not supported by autoconf-2.52
    sed -i "s#@abs_top_builddir@#$PWD#" tests/wrappl.in
    sed -i "s#@abs_top_srcdir@#$PWD#" tests/wrappl.in

    # Install autoconf data files into versioned directory
    for file in */*/Makefile.in */Makefile.in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' "$file"
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.53
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    ln -s autoconf-2.53 "${DESTDIR}${PREFIX}/bin/autoconf"
    ln -s autoheader-2.53 "${DESTDIR}${PREFIX}/bin/autoheader"
    ln -s autom4te-2.53 "${DESTDIR}${PREFIX}/bin/autom4te"
    ln -s autoreconf-2.53 "${DESTDIR}${PREFIX}/bin/autoreconf"
}
