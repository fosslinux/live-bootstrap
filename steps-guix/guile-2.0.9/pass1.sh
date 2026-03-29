# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/guile-2.0.9"

src_prepare() {
    default

    AUTOPOINT=true autoreconf -fi

    # Match Guix's static bootstrap Guile approach: force the final `guile`
    # executable to be linked through libtool with -all-static.
    sed -i \
        -e 's|^guile_LDADD =.*$|guile_LDADD = libguile-@GUILE_EFFECTIVE_VERSION@.la -ldl|' \
        -e 's|^guile_LDFLAGS =.*$|guile_LDFLAGS = -all-static|' \
        libguile/Makefile.in

}

src_configure() {
    local pkg_config_path libffi_cflags libffi_libs
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig"
    libffi_cflags="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        pkg-config --cflags libffi)"
    libffi_libs="$(PKG_CONFIG_LIBDIR="${pkg_config_path}" PKG_CONFIG_PATH="${pkg_config_path}" \
        pkg-config --static --libs libffi)"

    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    CFLAGS="${CFLAGS:-} -std=gnu89" \
    LIBFFI_CFLAGS="${libffi_cflags}" \
    LIBFFI_LIBS="${libffi_libs}" \
    LDFLAGS="-ldl" \
    ./configure \
        --prefix="${SEED_PREFIX}" \
        --disable-shared \
        --enable-static
}

src_compile() {
    default_src_compile
}

src_install() {
    local stage
    stage="${DESTDIR}${SEED_PREFIX}"

    make DESTDIR="${DESTDIR}" install
    seed_make_repro_tar_xz "${stage}" "${DISTFILES}/guile-static-stripped-2.0.9-i686-linux.tar.xz"
}
