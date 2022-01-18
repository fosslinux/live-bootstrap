# SPDX-FileCopyrightText: 2021 Andrius Štikonas <andrius@stikonas.eu>
#
# SPDX-License-Identifier: GPL-3.0-or-later

checksum=18203eec735fc553b24a3b4c404f614a596a54d4aba95d98d9b6d5bce2f5c7e8

src_prepare() {
    default

    . ../../import-gnulib.sh

    # We don't have autopoint from gettext yet
    AUTOPOINT=true autoreconf -fi

    # Remove bison pregenerated file
    rm gnu/parse-datetime.c
}

src_configure() {
    FORCE_UNSAFE_CONFIGURE=1 ./configure \
        --prefix="${PREFIX}" \
        --disable-nls
}

src_compile() {
    make PREFIX="${PREFIX}" MAKEINFO="true"
}

src_install() {
    make install PREFIX="${PREFIX}" MAKEINFO="true" DESTDIR="${DESTDIR}"
    # Manually install tar (cannot replace tar while running)
    cp "${DESTDIR}${PREFIX}/bin/tar" "${PREFIX}/bin/tar"
    rm "${DESTDIR}${PREFIX}/bin/tar"
}
