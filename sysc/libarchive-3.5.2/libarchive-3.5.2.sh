# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="http://libarchive.org/downloads/libarchive-3.5.2.tar.xz"

src_prepare() {
    default

    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}" --libdir="${PREFIX}/lib/musl" \
        --disable-shared
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
