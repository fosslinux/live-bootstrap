# SPDX-FileCopyrightText: 2024 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    autoreconf-2.71 -fi
    rm man/*.1
    rm doc/*.info

    # gperf
    rm lib/iconv_open*.h

    # Don't use in tree help2man
    rm man/help2man
    ln -s "${PREFIX}/bin/help2man" man/help2man

    . ../../import-gnulib.sh
}

src_configure() {
    ./configure --prefix="${PREFIX}"
}
