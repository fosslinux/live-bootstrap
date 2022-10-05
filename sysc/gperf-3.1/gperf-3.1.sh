# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    find . -name '*.info*' -delete

    for d in doc tests lib src; do
        cd $d
        ln -s ../aclocal.m4 aclocal.m4
        autoreconf-2.71 -fi
        cd ..
    done
    autoreconf-2.71 -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
