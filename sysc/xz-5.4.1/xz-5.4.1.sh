# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    # Delete translation catalogs
    find . -name "*.gmo" -delete
    rm -rf po4a/man

    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 AUTOCONF=autoconf-2.69 AUTOM4TE=autom4te-2.69 autoreconf-2.69 -f
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --disable-shared \
        --disable-nls \
        --build=i386-unknown-linux-musl \
        --libdir="${LIBDIR}"
}
