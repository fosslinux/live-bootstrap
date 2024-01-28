src_configure() {
    PKG_CONFIG_PATH="${LIBDIR}/pkgconfig/" \
    ./configure \
        --prefix="${PREFIX}" \
        --libdir="${LIBDIR}" \
        --sysconfdir=/etc
}
