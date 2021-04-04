# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default_src_prepare

    # Remove bison and flex generated files
    rm demos/calc/calc.{c,h} demos/calc/calclex.c

    autoreconf -f -i
}

src_configure() {
    ./configure \
	--prefix="${PREFIX}" \
	--build=i386-unknown-linux-gnu \
	--host=i386-unknown-linux-gnu \
	--target=i386-unknown-linux-gnu \
	--libdir="${PREFIX}/lib/musl"
}
