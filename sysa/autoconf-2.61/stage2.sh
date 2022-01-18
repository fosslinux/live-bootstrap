# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=9b21657a7c62214a59cb4724c6b676e50ead136bef67f647cba072d061afbbf1

src_prepare() {
    rm doc/standards.info
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
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
