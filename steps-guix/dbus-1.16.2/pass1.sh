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
        -DCMAKE_INSTALL_INCLUDEDIR="${PREFIX}/include" \
        -DBUILD_SHARED_LIBS=ON \
        -DDBUS_BUILD_TESTS=OFF \
        -DDBUS_ENABLE_XML_DOCS=OFF \
        -DDBUS_ENABLE_DOXYGEN_DOCS=OFF \
        -DDBUS_BUILD_X11=OFF \
        -DDBUS_WITH_GLIB=OFF \
        -DENABLE_SYSTEMD=OFF \
        -DENABLE_USER_SESSION=OFF
}

src_compile() {
    make "${MAKEJOBS}"
}

src_install() {
    make install DESTDIR="${DESTDIR}"
}
