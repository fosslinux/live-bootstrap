# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    local cmake_libdir
    cmake_libdir="${LIBDIR#${PREFIX}/}"
    if [ "${cmake_libdir}" = "${LIBDIR}" ]; then
        cmake_libdir="${LIBDIR#/}"
    fi

    mkdir -p build
    cd build

    cmake .. \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX="${PREFIX}" \
        -DCMAKE_INSTALL_LIBDIR="${cmake_libdir}" \
        -DCMAKE_INSTALL_INCLUDEDIR="include" \
        -DCMAKE_PREFIX_PATH="${PREFIX}" \
        -DCMAKE_INCLUDE_PATH="${PREFIX}/include" \
        -DCMAKE_LIBRARY_PATH="${LIBDIR}:${PREFIX}/lib" \
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
