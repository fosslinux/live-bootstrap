# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/bzip2-1.0.6"

src_configure() {
    :
}

src_compile() {
    make "${MAKEJOBS}" CFLAGS="-O2" LDFLAGS="-static"
}

src_install() {
    make PREFIX="${SEED_PREFIX}" DESTDIR="${DESTDIR}" install
}
