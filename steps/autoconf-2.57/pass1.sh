# SPDX-FileCopyrightText: 2021-2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm bin/autoconf.in
    rm doc/*.info
    rm man/*.1
    rm tests/*.at

    # Do not use pregenerated manpages
    sed -i '/SUBDIRS/s/ man//' Makefile.am

    AUTOMAKE=automake-1.7 ACLOCAL=aclocal-1.7 AUTOCONF=autoconf-2.55 autoreconf-2.55 -f

    # Install autoconf data files into versioned directory
    for file in */*/Makefile.in */Makefile.in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' "$file"
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.57
}

src_compile() {
    # Workaround for racy make dependencies
    make -C bin autom4te
    make -C lib
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    ln -s autoconf-2.57 "${DESTDIR}${PREFIX}/bin/autoconf"
    ln -s autoheader-2.57 "${DESTDIR}${PREFIX}/bin/autoheader"
    ln -s autom4te-2.57 "${DESTDIR}${PREFIX}/bin/autom4te"
    ln -s autoreconf-2.57 "${DESTDIR}${PREFIX}/bin/autoreconf"
}
