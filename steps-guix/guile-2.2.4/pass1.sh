# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/guile-2.2.4"

src_prepare() {
    default
}

src_configure() {
    ./configure \
        --prefix="${SEED_PREFIX}" \
        --disable-shared \
        --enable-static
}

src_compile() {
    default_src_compile
}

src_install() {
    local stage
    stage="${DESTDIR}${SEED_PREFIX}"

    make DESTDIR="${DESTDIR}" install
    seed_make_repro_tar_xz "${stage}" "${DISTFILES}/guile-static-stripped-2.2.4-i686-linux.tar.xz"
}
