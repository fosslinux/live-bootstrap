# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}"
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}
