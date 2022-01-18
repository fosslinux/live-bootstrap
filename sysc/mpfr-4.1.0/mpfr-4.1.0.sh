# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=94409e5aa78c758b80fcb6bd2d8d094bd14f4a4c02bc3cf7163b8d1d7414e1bc

src_prepare() {
    default

    find . -name '*.info' -delete
    autoreconf-2.69 -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --disable-shared

    # Disable tuning as that might cause non-reproducible build
    mv mparam.h src
}

src_compile() {
    make MAKEINFO=true DESTDIR="${DESTDIR}"
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
