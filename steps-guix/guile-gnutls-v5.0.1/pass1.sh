# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    local host_triplet pkg_config_path guile_cflags guile_libs gnutls_libs
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    guile_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags guile-3.0)"
    guile_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --libs guile-3.0)"
    gnutls_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --libs gnutls)"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG="/usr/bin/pkg-config" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    LD_LIBRARY_PATH="${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}" \
    GUILE_CFLAGS="${guile_cflags}" \
    GUILE_LIBS="${guile_libs}" \
    GNUTLS_LIBS="${gnutls_libs}" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --host="${host_triplet}" \
        --build="${host_triplet}" \
        --disable-static \
        --enable-shared \
        '--with-guile-site-dir=$(datarootdir)/guile/site/$(GUILE_EFFECTIVE_VERSION)' \
        '--with-guile-site-ccache-dir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache' \
        '--with-guile-extension-dir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/extensions'
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}
