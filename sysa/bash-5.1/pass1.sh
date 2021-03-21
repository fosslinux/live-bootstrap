# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    # Remove bison generated files
    rm y.tab.c y.tab.h

    # Rebuild configure script
    rm configure
    autoconf-2.61
}

src_configure() {
    CC=tcc CPPFLAGS="-D HAVE_ALLOCA_H" \
        ./configure --prefix="${PREFIX}" \
        --without-bash-malloc \
        --disable-nls \
        --build=i386-unknown-linux-gnu \
        --enable-static-link
}

src_install() {
    install bash ${bindir}
}
