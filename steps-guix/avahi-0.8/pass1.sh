# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    local host_triplet
    host_triplet="$(gcc -dumpmachine)"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG_PATH="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --includedir="${PREFIX}/include" \
        --host="${host_triplet}" \
        --build="${host_triplet}" \
        --enable-static \
        --enable-shared \
        --disable-gtk \
        --disable-gtk3 \
        --disable-qt3 \
        --disable-qt4 \
        --disable-qt5 \
        --disable-python \
        --disable-python-dbus \
        --disable-pygobject \
        --disable-mono \
        --disable-monodoc \
        --disable-autoipd \
        --disable-doxygen-doc \
        --disable-manpages \
        --disable-xmltoman \
        --disable-tests
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}
