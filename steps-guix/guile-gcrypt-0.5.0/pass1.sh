# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    local host_triplet pkg_config_path
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig:${PREFIX}/share/pkgconfig"

    mkdir -p build
    cd build

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG="/usr/bin/pkg-config" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    LD_LIBRARY_PATH="${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}" \
    ../configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --host="${host_triplet}" \
        --build="${host_triplet}" \
        --with-libgcrypt-prefix="${PREFIX}" \
        --with-libgcrypt-libdir="${LIBDIR}"
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}

src_postprocess() {
    local guile_site_path guile_site_ccache guile_core_ccache

    default_src_postprocess

    guile_site_path="${DESTDIR}${PREFIX}/share/guile/site/3.0:${PREFIX}/share/guile/site/3.0:${PREFIX}/share/guile/3.0"
    guile_site_ccache="${DESTDIR}${LIBDIR}/guile/3.0/site-ccache:${LIBDIR}/guile/3.0/site-ccache"
    guile_core_ccache="${LIBDIR}/guile/3.0/ccache"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    LD_LIBRARY_PATH="${DESTDIR}${LIBDIR}:${DESTDIR}${PREFIX}/lib:${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}" \
    GUILE_LOAD_PATH="${guile_site_path}" \
    GUILE_LOAD_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}" \
    GUILE_SYSTEM_PATH="${guile_site_path}" \
    GUILE_SYSTEM_COMPILED_PATH="${guile_site_ccache}:${guile_core_ccache}" \
    "${DESTDIR}${PREFIX}/bin/guile" -c "(use-modules (gcrypt hash)) (unless (equal? (hash-algorithm sha256) (lookup-hash-algorithm 'sha256)) (error \"guile-gcrypt sha256 lookup mismatch\")) (display \"gcrypt-module-ok\n\")"
}
