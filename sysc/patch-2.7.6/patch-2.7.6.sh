# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="https://mirrors.kernel.org/gnu/patch/patch-2.7.6.tar.xz
 https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-e017871.tar.gz"

src_prepare() {
    ../../import-gnulib.sh

    # bison
    rm lib/parse-datetime.c

    autoreconf -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
