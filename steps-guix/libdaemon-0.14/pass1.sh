# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    # TODO: The legacy build system does not recognize this musl triplet.
    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG_PATH="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --includedir="${PREFIX}/include" \
        --enable-static \
        --enable-shared \
        --disable-examples
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}
