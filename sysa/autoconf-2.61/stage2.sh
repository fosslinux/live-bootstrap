# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    sed -i -e '/AC_PROG_GREP/d' -e '/AC_PROG_SED/d' configure.ac
    autoreconf-2.61 -f

    # Install autoconf data files into versioned directory
    for file in */*/Makefile.in */Makefile.in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' $file
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.61
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true
}
