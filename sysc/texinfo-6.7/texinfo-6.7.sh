# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="https://mirrors.kernel.org/gnu/texinfo/texinfo-6.7.tar.xz
 https://git.savannah.gnu.org/cgit/gnulib.git/snapshot/gnulib-b81ec69.tar.gz"

src_prepare() {
    find . -name '*.mo' -delete
    find . -name '*.gmo' -delete

    ../../import-gnulib.sh
    autoreconf -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
