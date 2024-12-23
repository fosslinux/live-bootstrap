# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm doc/standards.info man/*.1
    AUTOMAKE=automake-1.11 ACLOCAL=aclocal-1.11 autoreconf-2.64 -f

    # Install autoconf data files into versioned directory
    for file in */*/Makefile.in */Makefile.in Makefile.in; do
        sed -i '/^pkgdatadir/s:$:-@VERSION@:' "$file"
    done
}

src_configure() {
    ./configure --prefix="${PREFIX}" --program-suffix=-2.69
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
    ln -s autoconf-2.69 "${DESTDIR}${PREFIX}/bin/autoconf"
    ln -s autoheader-2.69 "${DESTDIR}${PREFIX}/bin/autoheader"
    ln -s autom4te-2.69 "${DESTDIR}${PREFIX}/bin/autom4te"
    ln -s autoreconf-2.69 "${DESTDIR}${PREFIX}/bin/autoreconf"
}
