# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    rm doc/libffi.{pdf,info}

    autoreconf-2.71 -fi
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --build="${TARGET}" \
        --disable-shared \
        --with-gcc-arch=generic \
        --enable-pax_emutramp
}
