# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    ./bootstrap \
        --prefix="${PREFIX}" \
        --parallel="${JOBS}" \
        --no-qt-gui \
        -- \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_USE_OPENSSL=ON \
        -DCMAKE_INSTALL_LIBDIR="${LIBDIR}"
}

src_compile() {
    make "${MAKEJOBS}"
}

src_install() {
    make install DESTDIR="${DESTDIR}"
}
