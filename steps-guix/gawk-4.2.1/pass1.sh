# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/gawk-4.2.1"

src_prepare() {
    default
}

src_configure() {
    CFLAGS="-O2" LDFLAGS="-static" ./configure \
        --prefix="${SEED_PREFIX}" \
        --disable-nls \
        --disable-shared
}

src_compile() {
    default_src_compile
}

src_install() {
    make DESTDIR="${DESTDIR}" install
}
