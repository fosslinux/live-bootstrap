# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=82625c1337c4c80e84c6e795852f60e4d01c429a667b544f29eb1cbf3d09e401

src_prepare() {
    default

    ../../import-gnulib.sh

    AUTOPOINT=true autoreconf-2.69 -fi

    # Remove pregenerated files
    rm src/parse-gram.c src/parse-gram.h src/scan-skel.c src/scan-gram.c

    # Remove pregenerated .info
    rm doc/bison.info
}

src_configure() {
    LEX=flex-2.5.33 ./configure \
        --prefix="${PREFIX}" \
        --program-suffix=-2.3 \
        --datarootdir="${PREFIX}/share/bison-2.3"
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
