# SPDX-FileCopyrightText: 2024 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    . ../../import-gnulib.sh

    rm doc/*.1 doc/*.info* po/*.gmo

    rm lib/iconv_open*.h

    rm bootstrap
    autoreconf-2.71 -fi
}

src_configure() {
    LDFLAGS="-latomic" \
        ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}"
}
