# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/coreutils-8.30"

src_configure() {
    FORCE_UNSAFE_CONFIGURE=1 ./configure \
        --prefix="${SEED_PREFIX}" \
        --disable-nls \
        --disable-silent-rules \
        --enable-no-install-program=stdbuf,libstdbuf.so \
        CFLAGS="-Os -g0" \
        LDFLAGS="-static -pthread" \
        gl_cv_func_getcwd_path_max="no, but it is partly working" \
        gl_cv_prog_perl="no"
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true GPERF=true
}

src_install() {
    make DESTDIR="${DESTDIR}" MAKEINFO=true install
}
