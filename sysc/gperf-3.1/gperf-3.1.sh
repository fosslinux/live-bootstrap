# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=507f79f3c07b212154053caae665ee7ed3a53bc420987ec381cf637339fef7de

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
