# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/patch-2.7.6"

src_configure() {
    CFLAGS="-O2" LDFLAGS="-static" ./configure \
        --prefix="${SEED_PREFIX}" \
        gl_cv_func_working_mktime=yes
}

src_compile() {
    make "${MAKEJOBS}" MAKEINFO=true
}

src_install() {
    make DESTDIR="${DESTDIR}" MAKEINFO=true install
}
