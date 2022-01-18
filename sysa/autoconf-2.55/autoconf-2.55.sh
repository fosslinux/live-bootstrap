# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=fc52f377a8169339d6d9de871670c6a85829b2d3fc57b7c5d2ed5b6e1dbc307d

src_prepare() {
    rm bin/autoconf.in
    rm Makefile.in */Makefile.in */*/Makefile.in aclocal.m4 configure
    rm doc/standards.info doc/autoconf.info

    autoreconf-2.54

    # Install autoconf data files into versioned directory
    for file in */*/Makefile.in */Makefile.in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' $file
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.55
}

src_compile() {
    # Workaround for racy make dependencies
    make -C bin autom4te
    make -C lib

    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"

    ln -sf "${PREFIX}/bin/autoconf-2.55" "${DESTDIR}${PREFIX}/bin/autoconf"
    ln -sf "${PREFIX}/bin/autoheader-2.55" "${DESTDIR}${PREFIX}/bin/autoheader"
    ln -sf "${PREFIX}/bin/autom4te-2.55" "${DESTDIR}${PREFIX}/bin/autom4te"
    ln -sf "${PREFIX}/bin/autoreconf-2.55" "${DESTDIR}${PREFIX}/bin/autoreconf"
}
