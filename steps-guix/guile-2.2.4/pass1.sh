# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/guile-2.2.4"

src_prepare() {
    default
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
    LIBFFI_CFLAGS="${libffi_cflags}" \
    LIBFFI_LIBS="${libffi_libs}" \
    LDFLAGS="-static" \
    ./configure \
        --prefix="${SEED_PREFIX}" \
        --disable-shared \
        --enable-static
}

src_compile() {
    make "${MAKEJOBS}" -f Makefile PREFIX="${PREFIX}" LDFLAGS="-static"
}

src_install() {
    local stage
    stage="${DESTDIR}${SEED_PREFIX}"

    make DESTDIR="${DESTDIR}" LDFLAGS="-static" install
    seed_make_repro_tar_xz "${stage}" "${DISTFILES}/guile-static-stripped-2.2.4-i686-linux.tar.xz"
}
