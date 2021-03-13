# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default_src_prepare

    AUTOCONF=autoconf-2.13 AUTOHEADER=autoheader-2.13 ACLOCAL=aclocal-1.4 AUTOMAKE=automake-1.4 ./bootstrap
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
