# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    local host_triplet pkg_config_path
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    LD_LIBRARY_PATH="${LIBDIR}:${LD_LIBRARY_PATH}" \
    LIBS="$(pkg-config --static --libs guile-3.0)" \
    GUILE_LIBS="$(pkg-config --static --libs guile-3.0)" \
    GNUTLS_LIBS="$(pkg-config --static --libs gnutls)" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --host="${host_triplet}" \
        --build="${host_triplet}" \
        --enable-static \
        --disable-shared
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}
