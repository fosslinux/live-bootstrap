# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    :
}

src_compile() {
    gcc -O2 -std=c99 -Wall -Wextra -Werror \
        -o /tmp/loopback-bind-127001 \
        loopback-bind-127001.c

    /tmp/loopback-bind-127001
}

src_install() {
    install -D -m 0644 /dev/null "${DESTDIR}/usr/share/loopback-bind-127001.done"
}
