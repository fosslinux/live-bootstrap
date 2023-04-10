# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove bison and flex generated files
    rm demos/calc/calc.{c,h} demos/calc/calclex.c

    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi

    # Pre-built texinfo files
    find . -name '*.info*' -delete
}

src_configure() {
    ./configure \
	--prefix="${PREFIX}" \
	--libdir="${LIBDIR}" \
	--build=i386-unknown-linux-musl \
	--disable-shared
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
