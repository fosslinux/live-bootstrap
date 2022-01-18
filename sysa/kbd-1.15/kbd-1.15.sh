# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=001c9f84b3d7a5090fb2f4a0e109e49e80f9c59570e81870bb1debdcbb280f3b

src_prepare() {
    default

    # Remove flex/bison files
    rm src/loadkeys.c src/analyze.c

    # Fix musl incompat
    sed -i -e 's/u_char/unsigned char/g' \
        -e 's/u_short/unsigned short/g' src/dumpkeys.c src/loadkeys.y

    AUTOPOINT=true autoreconf -fi
}

src_configure() {
    ./configure --prefix=${PREFIX}
}

src_compile() {
    # Missing dependency in Makefile
    flex -o src/analyze.c src/analyze.l
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR=${DESTDIR} install
}
