# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/xz-5.2.4"

src_prepare() {
    default

    # Mirror Guix bootstrap static strategy: force libtool-built xz executable
    # to link with -all-static.
    sed -i 's/^xz_LDADD =/xz_LDADD = -all-static /' src/xz/Makefile.in
}

src_configure() {
    ./configure \
        --prefix="${SEED_PREFIX}" \
        --disable-shared \
        --enable-static \
        --disable-nls
}

src_compile() {
    default_src_compile
}

src_install() {
    make DESTDIR="${DESTDIR}" install
}
