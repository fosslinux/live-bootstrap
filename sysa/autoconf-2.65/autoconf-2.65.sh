# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    autoreconf-2.61 -f

    # Install autoconf data files into versioned directory
    for file in */*/Makefile.in */Makefile.in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' $file
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.65
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true

    ln -sf "${PREFIX}/bin/autoconf-2.65" "${PREFIX}/bin/autoconf"
    ln -sf "${PREFIX}/bin/autoheader-2.65" "${PREFIX}/bin/autoheader"
    ln -sf "${PREFIX}/bin/autom4te-2.65" "${PREFIX}/bin/autom4te"
    ln -sf "${PREFIX}/bin/autoreconf-2.65" "${PREFIX}/bin/autoreconf"
}
