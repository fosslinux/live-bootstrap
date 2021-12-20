# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

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

    rm "${PREFIX}/lib/musl/libxbps.so"*
}
