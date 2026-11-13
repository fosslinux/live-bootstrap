# SPDX-License-Identifier: GPL-3.0-or-later

src_unpack() {
    mkdir -p boost-unordered-patch
}

src_configure() {
    :
}

src_compile() {
    :
}

src_install() {
    install -D -m 0644 /dev/null "${DESTDIR}/usr/share/boost-unordered-patch.done"
}
