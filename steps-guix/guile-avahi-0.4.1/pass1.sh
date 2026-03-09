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
    local guile_site_path guile_core_site guile_site_ccache guile_core_ccache

    default_src_postprocess

    if find "${DESTDIR}" -type f \( -name '*.so' -o -name '*.so.*' \) | grep -q .; then
        echo "guile-avahi: shared objects are forbidden in static profile." >&2
        false
    fi

    guile_site_path="${DESTDIR}${PREFIX}/share/guile/site/3.0"
    guile_core_site="${PREFIX}/share/guile/3.0"
    guile_site_ccache="${DESTDIR}${LIBDIR}/guile/3.0/site-ccache"
    guile_core_ccache="${LIBDIR}/guile/3.0/ccache"

    PATH="${DESTDIR}${PREFIX}/bin:${PREFIX}/bin:/usr/bin:/bin" \
    GUILE_LOAD_PATH="${guile_site_path}:${guile_core_site}" \
    GUILE_LOAD_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}" \
    GUILE_SYSTEM_PATH="${guile_site_path}:${guile_core_site}" \
    GUILE_SYSTEM_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}" \
    "${DESTDIR}${PREFIX}/bin/guile" -c '
      (use-modules (avahi) (avahi client) (gnutls))
      (unless (session? (make-session connection-end/client))
        (error "gnutls session init failed"))
      (display "avahi+gnutls-modules-ok\n")'
}
