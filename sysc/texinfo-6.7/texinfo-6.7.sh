# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    find . -name '*.mo' -delete
    find . -name '*.gmo' -delete

    ../../import-gnulib.sh
    autoreconf -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
