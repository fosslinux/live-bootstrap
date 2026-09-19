# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default
}

src_compile() {
    make "${MAKEJOBS}" -f unix/Makefile generic_gcc
}

src_install() {
    make -f unix/Makefile install \
        BINDIR="${DESTDIR}${PREFIX}/bin" \
        MANDIR="${DESTDIR}${PREFIX}/share/man/man1"
}
