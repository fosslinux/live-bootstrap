# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=ce2252cf0e4e402248f06bff1f425829ba68a79a4e34efa2105234cfd0b76ca1

src_prepare() {
    rm doc/automake.info*

    autoreconf-2.61 -f
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
