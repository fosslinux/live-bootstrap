# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    sed -i 's/1.8a/1.8.5/; s/ filename-length-max=99//' configure.ac
    autoreconf-2.61 -f
}

src_configure() {
    ./configure --prefix=/after
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make install MAKEINFO=true
}
