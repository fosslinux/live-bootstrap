# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    local host_triplet pkg_config_path libgit2_cflags libgit2_libs
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    libgit2_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags libgit2)"
    libgit2_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --libs libgit2)"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG="/usr/bin/pkg-config" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    CPPFLAGS="${libgit2_cflags} ${CPPFLAGS:-}" \
    LIBS="${libgit2_libs}" \
    LIBGIT2_CFLAGS="${libgit2_cflags}" \
    LIBGIT2_LIBS="${libgit2_libs}" \
    libgit2_CFLAGS="${libgit2_cflags}" \
    libgit2_LIBS="${libgit2_libs}" \
    LD_LIBRARY_PATH="${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        '--with-guile-site-dir=$(datarootdir)/guile/site/$(GUILE_EFFECTIVE_VERSION)' \
        '--with-guile-site-ccache-dir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache' \
        '--with-guile-extension-dir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/extensions' \
        --host="${host_triplet}" \
        --build="${host_triplet}"
}

src_compile() {
    local pkg_config_path libgit2_cflags libgit2_libs

    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    libgit2_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags libgit2)"
    libgit2_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --libs libgit2)"

    CPPFLAGS="${libgit2_cflags} ${CPPFLAGS:-}" \
    LIBS="${libgit2_libs}" \
    LIBGIT2_CFLAGS="${libgit2_cflags}" \
    LIBGIT2_LIBS="${libgit2_libs}" \
    libgit2_CFLAGS="${libgit2_cflags}" \
    libgit2_LIBS="${libgit2_libs}" \
    default_src_compile
}

src_install() {
    default_src_install
}
