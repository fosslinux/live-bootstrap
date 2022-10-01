# SPDX-FileCopyrightText: 2022 Andrius Štikonas <andrius@stikonas.eu>
# SPDX-FileCopyrightText: 2022 fosslinux <fosslinux@aussies.space>
#
# SPDX-License-Identifier: GPL-3.0-or-later

src_prepare() {
    default

    . ../../import-gnulib.sh

    # We don't have autopoint from gettext yet
    AUTOPOINT=true AUTOMAKE=automake-1.15 ACLOCAL=aclocal-1.15 autoreconf-2.69 -fi

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
