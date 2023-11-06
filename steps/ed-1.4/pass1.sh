# SPDX-FileCopyrightText: © 2023 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm doc/ed.info
}

src_configure() {
    ./configure --prefix="${PREFIX}" \
        --build=i386-unknown-linux-musl
}

src_install() {
    install -m 755 -D ed ${DESTDIR}/usr/bin/ed
}
