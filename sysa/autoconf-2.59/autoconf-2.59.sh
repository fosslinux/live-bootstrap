# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    autoreconf-2.57 -f

    # Install autoconf data files into versioned directory
    for file in */*/Makefile.in */Makefile.in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' $file
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.59
}

src_install() {
    default_src_install

    ln -sf "${PREFIX}/bin/autoconf-2.59" "${PREFIX}/bin/autoconf"
    ln -sf "${PREFIX}/bin/autom4te-2.59" "${PREFIX}/bin/autom4te"
}
