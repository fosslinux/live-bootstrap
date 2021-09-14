# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    find . -name '*.info*' -delete
    find . -name '*.gmo' -delete

    # bison
    rm gettext-runtime/intl/plural.c gettext-tools/src/{po-gram-gen,cldr-plural}.{c,h}
    GNULIB_SRCDIR=$(realpath ../gnulib-7daa86f) ./autogen.sh
}

src_configure() {
    ./configure --prefix="${PREFIX}" --enable-static --disable-shared
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
