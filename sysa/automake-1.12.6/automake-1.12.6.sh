# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default_src_prepare

    autoreconf-2.69 -f
}

src_configure() {
    ./configure --prefix=/after
}

src_compile() {
    make MAKEINFO=true CC=tcc
}

src_install() {
    make install MAKEINFO=true
}
