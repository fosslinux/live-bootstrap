# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    find gnulib-tests -not -name "Makefile.am" -delete
    rm doc/grep.info po/*.gmo

    # cursed autotools order
    # autopoint and libtoolize have to be run *before* gnulib
    # and automake, etc after
    find . \( -name 'Makefile.in' -o -name 'configure' \) -delete
    autopoint -f
    libtoolize -fi

    . ../../import-gnulib.sh

    AUTOPOINT=true LIBTOOLIZE=true autoreconf-2.71 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
