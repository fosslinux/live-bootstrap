# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm Makefile.in */Makefile.in */*/Makefile.in aclocal.m4 configure doc/standards.info
    aclocal-1.6
    cat config/m4.m4 >> aclocal.m4
    autoconf-2.53
    automake-1.6

    # Install autoconf data files into versioned directory
    for file in */*/Makefile.in */Makefile.in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' $file
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.53
}

src_compile() {
    make MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
