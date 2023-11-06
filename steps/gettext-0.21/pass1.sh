# SPDX-FileCopyrightText: 2021-22 fosslinux <fosslinux@aussies.space>
# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    find . -name '*.info*' -delete
    find . -name '*.gmo' -delete

    # bison
    rm gettext-runtime/intl/plural.c gettext-tools/src/{po-gram-gen,cldr-plural}.{c,h}
    GNULIB_SRCDIR=$(realpath ../gnulib-7daa86f) ./autogen.sh

    # Precompiled Java class
    rm gettext-tools/gnulib-lib/javaversion.class
    touch gettext-tools/gnulib-lib/javaversion.class
}

src_configure() {
    ./configure --prefix="${PREFIX}" --libdir="${LIBDIR}" --enable-static --disable-shared --disable-java
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true CFLAGS="-I${PWD}/libtextstyle/lib"
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
