# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=3f6c27d6743f926c9b08cccd921c3d6179105c681dd5aa9ab362895c71c106c9

src_prepare() {
    autoreconf-2.69 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_compile() {
    make MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
