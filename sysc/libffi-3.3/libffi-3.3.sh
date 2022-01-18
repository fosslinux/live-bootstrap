# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=019bf569a1374e7fff7a106b11eb2c91dfd8da7f3a8b04e98d7e9403c27fc781

src_prepare() {
    find . -name '*.info*' -delete

    autoreconf-2.71 -fi
}

src_configure() {
    ./configure \
	--prefix="${PREFIX}" \
	--libdir="${PREFIX}/lib/musl" \
	--build=i386-unknown-linux-musl \
	--disable-shared \
	--with-gcc-arch=generic \
	--enable-pax_emutramp
}
