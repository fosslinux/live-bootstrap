# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/tar-1.30"

src_configure() {
    FORCE_UNSAFE_CONFIGURE=1 CFLAGS="-O2" LDFLAGS="-static" ./configure \
        --prefix="${SEED_PREFIX}" \
        --disable-nls \
        gl_cv_func_getcwd_path_max="no, but it is partly working"
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make DESTDIR="${DESTDIR}" MAKEINFO=true install
}
