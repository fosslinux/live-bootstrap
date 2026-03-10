# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    local host_triplet pkg_config_path guile_cflags guile_static_libs
    local avahi_cflags avahi_static_libs
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    guile_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags guile-3.0)"
    guile_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs guile-3.0)"
    avahi_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags avahi-client)"
    avahi_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs avahi-client)"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG="/usr/bin/pkg-config" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    LD_LIBRARY_PATH="${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}" \
    GUILE_CFLAGS="${guile_cflags}" \
    GUILE_LDFLAGS="${guile_static_libs}" \
    AVAHI_CFLAGS="${avahi_cflags}" \
    AVAHI_LIBS="${avahi_static_libs}" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --host="${host_triplet}" \
        --build="${host_triplet}" \
        --enable-static \
        --disable-shared \
        '--with-guilemoduledir=$(datarootdir)/guile/site/$(GUILE_EFFECTIVE_VERSION)'
}

src_compile() {
    local pkg_config_path guile_cflags guile_static_libs avahi_cflags avahi_static_libs
    local gnutls_static_libs guile_gnutls_static_lib
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    guile_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags guile-3.0)"
    guile_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs guile-3.0)"
    avahi_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags avahi-client)"
    avahi_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs avahi-client)"
    gnutls_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs gnutls)"
    guile_gnutls_static_lib="${LIBDIR}/libguile-gnutls-static.a"

    CPPFLAGS="${guile_cflags} ${avahi_cflags} ${CPPFLAGS:-}" \
    GUILE_CFLAGS="${guile_cflags}" \
    GUILE_LDFLAGS="${guile_static_libs}" \
    AVAHI_CFLAGS="${avahi_cflags}" \
    AVAHI_LIBS="${avahi_static_libs}" \
    GNUTLS_LIBS="${gnutls_static_libs}" \
    GUILE_GNUTLS_STATIC_LIB="${guile_gnutls_static_lib}" \
    default_src_compile
}

src_install() {
    local pkg_config_path guile_cflags guile_static_libs avahi_cflags avahi_static_libs
    local gnutls_static_libs guile_gnutls_static_lib
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    guile_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags guile-3.0)"
    guile_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs guile-3.0)"
    avahi_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags avahi-client)"
    avahi_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs avahi-client)"
    gnutls_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs gnutls)"
    guile_gnutls_static_lib="${LIBDIR}/libguile-gnutls-static.a"

    CPPFLAGS="${guile_cflags} ${avahi_cflags} ${CPPFLAGS:-}" \
    GUILE_CFLAGS="${guile_cflags}" \
    GUILE_LDFLAGS="${guile_static_libs}" \
    AVAHI_CFLAGS="${avahi_cflags}" \
    AVAHI_LIBS="${avahi_static_libs}" \
    GNUTLS_LIBS="${gnutls_static_libs}" \
    GUILE_GNUTLS_STATIC_LIB="${guile_gnutls_static_lib}" \
    default_src_install
}

src_postprocess() {
    default_src_postprocess
}
