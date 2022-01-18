# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=66778085e702e5ec3842285818c3427c3baffd9f03a4b1b15278f8acccc05917

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
