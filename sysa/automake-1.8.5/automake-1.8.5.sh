# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=39e6fda316adc0c0f0dd59c7c2793d46583f292aca61e2f08c3bd6d0fb23c45c

src_prepare() {
    rm doc/automake.info*

    autoreconf-2.59 -f
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
