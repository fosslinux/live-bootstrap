# SPDX-FileCopyrightText: © 2023 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    # Remove bison generated files
    rm bc/bc.c bc/bc.h
    rm bc/scan.c

    # Skip documentation
    sed -i 's/ doc//' Makefile.am
    
    # Rebuild configure script
    AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}" \
        --build=i386-unknown-linux-musl
}
