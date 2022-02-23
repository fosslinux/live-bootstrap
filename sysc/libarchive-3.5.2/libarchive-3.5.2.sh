# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    autoreconf -fi
}

src_configure() {
    ./configure --prefix="${PREFIX}" --libdir="${PREFIX}/lib/musl" \
        --disable-shared
}

src_compile() {
    make MAKEINFO=true
}

src_install() {
    make MAKEINFO=true DESTDIR="${DESTDIR}" install
}
