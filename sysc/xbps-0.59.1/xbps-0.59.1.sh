# SPDX-FileCopyrightText: 2021 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

urls="https://github.com/void-linux/xbps/archive/refs/tags/0.59.1.tar.gz"

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
    rm -r "${DESTDIR}${PREFIX}/share/bash-completion/completions"
}
