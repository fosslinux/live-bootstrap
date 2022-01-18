# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=7e6a1082a4785a7b58928865a40ed2c93940af54972a2dc30ff10185da97b491

src_prepare() {
    ./bootstrap
}

src_configure() {
    CC=tcc ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-shared \
        --host=i386-unknown-linux \
        --target=i386-unknown-linux \
        --build=i386-unknown-linux
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
