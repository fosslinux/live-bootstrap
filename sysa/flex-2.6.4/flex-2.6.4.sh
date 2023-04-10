# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    mv Makefile src/
    touch src/config.h
    rm src/parse.c src/parse.h src/scan.c src/skel.c
}

src_compile() {
    cd src
    make -j1 PREFIX="${PREFIX}"
    cd ..
}

src_install() {
    cd src
    default
    cd ..

    # Remove yacc, we won't need it any longer
    rm "${PREFIX}/bin/yacc"
    rm /yaccpar
}
