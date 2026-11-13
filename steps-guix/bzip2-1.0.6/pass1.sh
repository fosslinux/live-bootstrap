# SPDX-License-Identifier: GPL-3.0-or-later

SEED_PREFIX="/bootstrap-seeds/bzip2-1.0.6"
SYSTEM_PREFIX="/usr"

src_configure() {
    :
}

src_compile() {
    make "${MAKEJOBS}" CFLAGS="-O2" LDFLAGS="-static"
}

src_install() {
    # Upstream bzip2 Makefile does not support DESTDIR, so PREFIX must include it.
    make CFLAGS="-O2" LDFLAGS="-static" PREFIX="${DESTDIR}${SYSTEM_PREFIX}" install
    make CFLAGS="-O2" LDFLAGS="-static" PREFIX="${DESTDIR}${SEED_PREFIX}" install

    # Fail early if either installed binary is not executable in this environment.
    "${DESTDIR}${SYSTEM_PREFIX}/bin/bzip2" --help >/dev/null
    "${DESTDIR}${SEED_PREFIX}/bin/bzip2" --help >/dev/null
}
