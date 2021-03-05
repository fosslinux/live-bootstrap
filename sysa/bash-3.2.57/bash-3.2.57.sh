# SPDX-FileCopyrightText: 2021 Paul Dersey <pdersey@gmail.com>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    # Remove bison generated files
    rm y.tab.c y.tab.h

    # Rebuild configure script
    # Ignore harmless error from autoconf for now
    rm configure
    autoconf-2.52 || true
}

src_configure() {
    CC=tcc CPPFLAGS="-D HAVE_ALLOCA_H" \
        ./configure --prefix=/after \
        --without-bash-malloc \
        --build=i386-unknown-linux
}

src_install() {
    install bash ${bindir}
}
