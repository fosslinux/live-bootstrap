# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm configure Makefile.in */Makefile.in */*/Makefile.in

    autoconf-2.52

    sed -i 's#$(datadir)/aclocal#$(datadir)/aclocal-1.5#' m4/Makefile.am
    aclocal-1.4
    automake-1.5

    sed -i 's#@datadir@/@PACKAGE@#@datadir@/@PACKAGE@-@VERSION@#' automake.in
    for file in Makefile.in */Makefile.in */*/Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' $file
    done
}

src_configure() {
    ./configure --prefix=/after --program-suffix=-1.5
}

src_install() {
    rm -rf "${PREFIX}/share/automake"
    default_src_install
}
