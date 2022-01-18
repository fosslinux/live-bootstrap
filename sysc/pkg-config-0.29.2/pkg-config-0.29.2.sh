# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=850f2663df30af439a5cc8b0c44ceb7f9ce0d2fecf2774f43fa9ab9b7b7be1c0

src_prepare() {
    autoreconf -fi
}

src_configure() {
    # We use internal glib because standalone glib library depends on
    # pkg-config and other software (python/meson) that we do not have.
    ./configure \
        --prefix="${PREFIX}" \
        --build=i386-unknown-linux-musl \
        --with-internal-glib
}
