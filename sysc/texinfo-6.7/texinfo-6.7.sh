# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=852e7be426aa5ae63b646fb6f5c8f95a884a143a04548e615289dacb5d0a9970

src_prepare() {
    find . -name '*.mo' -delete
    find . -name '*.gmo' -delete

    ../../import-gnulib.sh
    autoreconf -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
