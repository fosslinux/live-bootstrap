# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=cabd674d18f3c3bb3abc3e933c9de68ed80a452da52e0b9503b70f8ca83cc4e3

src_configure() {
    PKG_CONFIG_PATH="${PREFIX}/lib/musl/pkgconfig" \
        ./configure --prefix="${PREFIX}" \
        --libdir="${PREFIX}/lib/musl" \
        --pkgconfigdir="${PREFIX}/lib/musl/pkgconfig" \
        --enable-rpath \
        --enable-static
    echo "CFLAGS += -Wno-error" >> config.mk
}

src_install() {
    default

    rm "${DESTDIR}${PREFIX}/lib/musl/libxbps.so"*
}
