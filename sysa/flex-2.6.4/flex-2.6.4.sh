# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=e4150bd14b0e3f2ab3afa918971c8589ed79be0fb1f2beb85f27989c873461fd

src_prepare() {
    default

    mv Makefile src/
    touch src/config.h
    rm src/parse.c src/parse.h src/scan.c src/skel.c
}

src_compile() {
    cd src
    default
    cd ..
}

src_install() {
    cd src
    default
    cd ..

    # Remove yacc, we won't need it any longer
    rm ${PREFIX}/bin/yacc
    rm /yaccpar
}
