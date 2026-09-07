# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    CFLAGS="${CFLAGS:-} -std=gnu89" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --includedir="${PREFIX}/include" \
        --enable-static \
        --enable-shared
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}
