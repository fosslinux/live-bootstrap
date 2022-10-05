# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="https://mirrors.kernel.org/gnu/grep/grep-3.7.tar.xz
 http://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-8f4538a5.tar.gz"

src_prepare() {
    rm configure
    find . -name 'Makefile.in' -delete

    ../../import-gnulib.sh

    autoreconf-2.71 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_install() {
    default
}
