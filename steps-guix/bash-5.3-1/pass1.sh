# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/bash-5.3-1"

src_prepare() {
    default
}

src_configure() {
    ./configure \
        --prefix="${SEED_PREFIX}" \
        --without-bash-malloc \
        --disable-nls \
        --enable-static-link \
        --build="${TARGET}" \
        bash_cv_dev_stdin=absent \
        bash_cv_dev_fd=whacky
}

src_compile() {
    make -j1
}

src_install() {
    # Keep seed output for bootstrap artifact generation.
    install -D -m 0755 bash "${DESTDIR}${SEED_PREFIX}/bin/bash"
    # same as steps/bash
    install -D -m 0755 bash "${DESTDIR}/usr/bin/bash"
}
