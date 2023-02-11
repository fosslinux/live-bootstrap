# SPDX-FileCopyrightText: 2023 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    AUTOMAKE=automake-1.11 ACLOCAL=aclocal-1.11 autoreconf-2.64 -fi
}

src_configure() {
    CFLAGS="-std=gnu99" \
    ./configure --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --disable-shared
}
