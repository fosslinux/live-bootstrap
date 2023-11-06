# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    # Remove flex/bison files
    rm src/loadkeys.c src/analyze.c

    # Fix musl incompat
    sed -i -e 's/u_char/unsigned char/g' \
        -e 's/u_short/unsigned short/g' src/dumpkeys.c src/loadkeys.y

    AUTOPOINT=true AUTOMAKE=automake-1.10 ACLOCAL=aclocal-1.10 AUTOM4TE=autom4te-2.64 autoreconf-2.64 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_compile() {
    # Missing dependency in Makefile
    flex -o src/analyze.c src/analyze.l
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
