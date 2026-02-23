# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_configure() {
    local host_triplet pkg_config_path libgcrypt_prefix libgcrypt_libdir
    host_triplet="$(gcc -dumpmachine)"
    pkg_config_path="${LIBDIR}/pkgconfig:${PREFIX}/lib/pkgconfig"
    libgcrypt_prefix="$(PKG_CONFIG_PATH="${pkg_config_path}" pkg-config --variable=prefix libgcrypt)"
    libgcrypt_libdir="$(PKG_CONFIG_PATH="${pkg_config_path}" pkg-config --variable=libdir libgcrypt)"

    PATH="${PREFIX}/bin:/usr/bin:/bin" \
    PKG_CONFIG_PATH="${pkg_config_path}" \
    LD_LIBRARY_PATH="${libgcrypt_libdir}:${LIBDIR}:${PREFIX}/lib:${LD_LIBRARY_PATH}" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --host="${host_triplet}" \
        --build="${host_triplet}" \
        --with-libgcrypt-prefix="${libgcrypt_prefix}"
}

src_compile() {
    default_src_compile
}

src_install() {
    default_src_install
}
