# SPDX-FileCopyrightText: 2024 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
    autoreconf-2.71 -fi

    . ../../import-gnulib.sh
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
