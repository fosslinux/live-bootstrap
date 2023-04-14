# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    ../../import-gnulib.sh

    AUTOPOINT=true AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi

    # Remove pregenerated files
    rm src/parse-gram.c src/parse-gram.h src/scan-skel.c src/scan-gram.c

    # Remove pregenerated .info
    rm doc/bison.info
}

src_configure() {
    LEX=flex-2.5.33 ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --program-suffix=-2.3 \
        --datarootdir="${PREFIX}/share/bison-2.3"
}

src_compile() {
    make -j1 MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
