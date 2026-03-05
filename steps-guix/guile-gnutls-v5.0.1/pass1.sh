# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    local host_triplet pkg_config_path guile_cflags guile_static_libs gnutls_static_libs gnutls_link_flags
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"
    guile_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --cflags guile-3.0)"
    guile_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs guile-3.0)"
    gnutls_static_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        /usr/bin/pkg-config --static --libs gnutls)"
    gnutls_link_flags="-Wl,-Bstatic ${gnutls_static_libs} -Wl,-Bdynamic"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG="/usr/bin/pkg-config" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    LD_LIBRARY_PATH="${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}" \
    GUILE_CFLAGS="${guile_cflags}" \
    GUILE_LIBS="${guile_static_libs}" \
    GNUTLS_LIBS="${gnutls_link_flags}" \
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

src_postprocess() {
    local module_path

    default_src_postprocess

    module_path="$(find "${DESTDIR}${LIBDIR}/guile" -type f -name 'guile-gnutls-v-2.so' | head -n1)"
    if [ -z "${module_path}" ] || [ ! -f "${module_path}" ]; then
        echo "guile-gnutls: extension module not found after install." >&2
        false
    fi

    if command -v readelf >/dev/null 2>&1; then
        if readelf -d "${module_path}" | grep -q 'NEEDED.*libguile'; then
            echo "guile-gnutls: extension must not link libguile directly." >&2
            false
        fi
        if readelf -d "${module_path}" | grep -q 'NEEDED.*libgnutls'; then
            echo "guile-gnutls: extension must link gnutls stack statically." >&2
            false
        fi
    fi
}
