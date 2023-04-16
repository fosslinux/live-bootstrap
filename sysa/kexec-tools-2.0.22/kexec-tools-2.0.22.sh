# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    AUTOMAKE=automake-1.10 ACLOCAL=aclocal-1.10 AUTOM4TE=autom4te-2.64 autoreconf-2.64 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}" \
        --sbindir="${PREFIX}/bin" \
        --build=i686-pc-linux-gnu \
        --host=i686-pc-linux-gnu \
        --target=i686-pc-linux-gnu
}
