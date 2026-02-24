# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
    autoreconf -fi
}

src_configure() {
    local host_triplet pkg_config_path
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig"

    mkdir -p build
    cd build

    test -x "${PREFIX}/bin/libgcrypt-config"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    LIBGCRYPT_CONFIG="${PREFIX}/bin/libgcrypt-config" \
    PKG_CONFIG_LIBDIR="${pkg_config_path}" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    LD_LIBRARY_PATH="${LIBDIR}:${LD_LIBRARY_PATH}" \
    ../configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --host="${host_triplet}" \
        --build="${host_triplet}" \
        --with-libgcrypt-prefix="${PREFIX}"
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}
