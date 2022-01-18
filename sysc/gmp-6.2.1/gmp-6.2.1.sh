# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=cb28e5554c51291c456027e38f5563cc787c8e2f8656bb1f4dfcc085d0639016

src_prepare() {
    default

    # Remove bison and flex generated files
    rm demos/calc/calc.{c,h} demos/calc/calclex.c

    autoreconf -f -i

    # Pre-built texinfo files
    find . -name '*.info*' -delete
}

src_configure() {
    ./configure \
	--prefix="${PREFIX}" \
	--libdir="${PREFIX}/lib/musl" \
	--build=i386-unknown-linux-musl \
	--disable-shared
}

src_compile() {
    make MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
