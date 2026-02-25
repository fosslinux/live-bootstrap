# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    mkdir -p build
    cd build

    cmake .. \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX="${PREFIX}" \
        -DCMAKE_INSTALL_LIBDIR="${LIBDIR}" \
        -DBUILD_SHARED_LIBS=OFF \
        -DBUILD_TESTS=OFF \
        -DBUILD_CLI=OFF \
        -DBUILD_EXAMPLES=OFF \
        -DUSE_SSH=OFF \
        -DUSE_HTTPS=OpenSSL \
        -DUSE_HTTP_PARSER=builtin \
        -DREGEX_BACKEND=regcomp \
        -DUSE_BUNDLED_ZLIB=OFF \
        -DUSE_THREADS=ON
}

src_compile() {
    make "${MAKEJOBS}"
}

src_install() {
    make install DESTDIR="${DESTDIR}"
}
