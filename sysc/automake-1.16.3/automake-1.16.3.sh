# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=708d854632c90e3706194a1a7555a3dc2fafb7ccae65872ad3420083f2781143

src_prepare() {
    ./bootstrap

    rm doc/automake-history.info doc/automake.info*
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true DESTDIR="${DESTDIR}"
}
