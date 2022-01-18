# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=9c4e3e511aaaec495a23bac4e79442cda26430f9d63fd7c4392ab4949fb5815a

src_prepare() {
    rm doc/automake.info*

    sed -i 's/1.8a/1.8.5/; s/ filename-length-max=99//' configure.ac
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
