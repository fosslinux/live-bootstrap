# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

EXTRA_SRCS="gnulib-e017871.tar.gz"

src_prepare() {
    ../../import-gnulib.sh

    # bison
    rm lib/parse-datetime.c

    autoreconf -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
