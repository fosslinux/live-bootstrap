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

    # Without this bash build can be non-deterministic when using
    # our old bash 2.05b which was built with Mes C library.
    sed -i 's/sleep 3/sleep 3; sync/' builtins/psize.sh
}

src_configure() {
    ./configure --prefix="${PREFIX}" \
        --without-bash-malloc \
        --disable-nls \
        --build=i386-unknown-linux-gnu \
        --enable-static-link
}

src_install() {
    # Do not install prebuilt .mo translation catalogs
    install bash "${DESTDIR}${PREFIX}/bin"
}
