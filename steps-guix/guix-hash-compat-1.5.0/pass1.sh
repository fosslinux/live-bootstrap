# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    :
}

src_compile() {
    gcc \
        -O2 \
        -std=c99 \
        -Wall \
        -Wextra \
        -Werror \
        -o guix-hash-compat \
        guix-hash-compat.c
}

src_install() {
    install -D -m 0755 guix-hash-compat "${DESTDIR}/usr/bin/guix-hash-compat"
}