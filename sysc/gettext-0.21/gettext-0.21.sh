# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=93d9f6a2532b36bd4cc2165756ea52c82addcdb9b3685dfb9e138f89dbdb3bb6

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
    ./configure --prefix="${PREFIX}" --enable-static --disable-shared --disable-java
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
