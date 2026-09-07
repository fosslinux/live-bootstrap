# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    local host_triplet pkg_config_path
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    DBUS_CFLAGS="$(pkg-config --cflags dbus-1)" \
    DBUS_LIBS="$(pkg-config --libs dbus-1)" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --includedir="${PREFIX}/include" \
        --host="${host_triplet}" \
        --build="${host_triplet}" \
        --with-distro=lfs \
        --disable-stack-protector \
        --enable-gdbm \
        --disable-dbm \
        --enable-static \
        --enable-shared \
        --disable-glib \
        --disable-gobject \
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
