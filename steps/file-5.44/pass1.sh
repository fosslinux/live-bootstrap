# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi
}

src_configure() {
    CFLAGS="-std=gnu99" \
    ./configure --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --disable-shared
}
