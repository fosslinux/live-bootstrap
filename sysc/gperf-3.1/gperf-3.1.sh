# SPDX-FileCopyrightText: 2021-2022 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="https://mirrors.kernel.org/gnu/gperf/gperf-3.1.tar.gz"

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
    LDFLAGS="-static" ./configure --prefix="${PREFIX}"
}
