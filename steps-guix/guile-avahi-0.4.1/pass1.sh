# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    local host_triplet pkg_config_path
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}:${PREFIX}/lib/pkgconfig" \
    PKG_CONFIG_PATH="${pkg_config_path}:${PREFIX}/lib/pkgconfig" \
    LD_LIBRARY_PATH="${LIBDIR}:${LD_LIBRARY_PATH}" \
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
    make "${MAKEJOBS}" -f Makefile PREFIX="${PREFIX}" \
        CROSS_COMPILING_VARIABLE="AVAHI_GUILE_CROSS_COMPILING=yes"
}

src_install() {
    make -f Makefile install PREFIX="${PREFIX}" DESTDIR="${DESTDIR}" \
        CROSS_COMPILING_VARIABLE="AVAHI_GUILE_CROSS_COMPILING=yes"
}
