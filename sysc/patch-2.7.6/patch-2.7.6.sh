# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=c18cdec4ba5292d4770f519d30fc30557eea8d424c83276aa6b180f8af1ea8a6

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
