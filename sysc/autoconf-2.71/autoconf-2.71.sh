# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=d713ac1840e82250414ee4f7cd75c79ee44ecb4e8dfc659704eed3d75e7d348c

src_prepare() {
    rm doc/standards.info
    autoreconf-2.69 -fi

    # Install autoconf data files into versioned directory
    for file in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' $file
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.71
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"

    ln -sf "${PREFIX}/bin/autoconf-2.71" "${DESTDIR}${PREFIX}/bin/autoconf"
    ln -sf "${PREFIX}/bin/autoheader-2.71" "${DESTDIR}${PREFIX}/bin/autoheader"
    ln -sf "${PREFIX}/bin/autom4te-2.71" "${DESTDIR}${PREFIX}/bin/autom4te"
    ln -sf "${PREFIX}/bin/autoreconf-2.71" "${DESTDIR}${PREFIX}/bin/autoreconf"
}
